      *((program: SOCK17.cl2))
000001 IDENTIFICATION DIVISION.
000002 PROGRAM-ID. SOCK17.
000003 AUTHOR. Cowtown.
000004 DATE-COMPILED.
000005*SECURITY.   *****************************************************
000006*            *                                                   *
000007*            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
000008*            *                                                   *
000009*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000010*            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
000011*            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
000012*            *                                                   *
000013*            *****************************************************
000014
000015******************************************************************
000016*REMARKS.                                                        *
000017*        Reads the pending issue record passed to it.            *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 111020 CR2020061000002   PEMA  New Program
000027******************************************************************
000028 ENVIRONMENT DIVISION.
000029
000030 DATA DIVISION.
000031
000032 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000033 77  FILLER  PIC X(32) VALUE '********************************'.
000034 77  FILLER  PIC X(32) VALUE '   SOCK17   WORKING STORAGE     '.
000035 77  FILLER  PIC X(32) VALUE '********************************'.
000036
000037 77 ws-send-msg-size           pic s9(8) comp value 19200.
000038 77 ws-recv-msg-size           pic s9(8) comp value 19200.
000039 77 ws-recv-buf                pic x(4096).
000040 77 ws-send-buf                pic x(19200) VALUE SPACES.
000041 77 ws-recv-total              pic s9(8) comp value 0.
000042 77 ws-recv-left               pic s9(8) comp value 0.
000043 77 ws-seq-num                 pic s9(8) comp value 0.
000044 77 ws-flags                   pic s9(8) comp value 0.
000045 77  WS-EOF-SW                   PIC X VALUE SPACES.
000046     88  END-OF-ERPNDB                VALUE 'Y'.
000047 77  WS-ERALPH-SW                PIC X VALUE SPACES.
000048     88  END-OF-ERALPH                VALUE 'Y'.
000049 77  WS-ERPNDB5-SW               PIC X VALUE SPACES.
000050     88  END-OF-ERPNDB5               VALUE 'Y'.
000051 77  ERPNDB-RECS-IN              PIC 9(9) VALUE ZEROS.
000052 77  ERPNDB-RECS-OUT             PIC 9(9) VALUE ZEROS.
000053 77  WS-COMP-CD                  PIC X  VALUE LOW-VALUES.
000054 77  WS-COMP-ID                  PIC XXX VALUE 'CID'.
000055 77  SUB1                        PIC S9(5) VALUE +0 COMP-3.
000056 77  WS-PREV-PNDB-KEY            PIC X(23) VALUE LOW-VALUES.
000057 77  c1                          pic s999 value +0 comp-3.
000058 77  a1                          pic s999 value +0 comp-3.
000059 77  p1                          pic s999 value +0 comp-3.
000060 77  n1                          pic s999 value +0 comp-3.
000061 77  t1                          pic s999 value +0 comp-3.
000062 77  ws-match-sw                 pic x value spaces.
000063     88  no-matching-alpha           value 'N'.
000064 77  ws-save-issue-key           pic x(36)  value spaces.
000065 77  ws-last-name                pic x(15)  value spaces.
000066 77  ws-elcert-last-name         pic x(15)  value spaces.
000067 77  ws-first-three              pic xxx  value spaces.
000068 77  ws-age                      pic 999 value zeros.
000069 77  ws-cov-sw                   pic x value spaces.
000070     88  processing-primary         value 'P'.
000071     88  processing-secondary        value 'S'.
000072 77  ws-erpndb2-startbr-sw       pic x  value spaces.
000073     88  erpndb2-startbr           value 'Y'.
000074 77  ws-eralph2-startbr-sw       pic x  value spaces.
000075     88  eralph2-startbr           value 'Y'.
000076 77  ws-pend-match               pic x value ' '.
000077     88  pend-match                 value 'Y'.
000078 77  ws-elmstr5-startbr-sw       pic x  value spaces.
000079     88  elmstr5-startbr           value 'Y'.
000080 77  save-bin-date               pic xx value low-values.
000081 77  ws-cert-note-generic-key-len pic s9(4) comp value +34.
000082 77  note-count                  pic s999 comp-3 value +0.
000083 77  ws-build-note-sw            pic x value ' '.
000084     88  finished-with-notes      value 'Y'.
000085 77  ws-ercnot-sw                pic x  value spaces.
000086     88  ercnot-startbr            value 'Y'.
000087 77  ws-current-bin-dt           pic xx value low-values.
000088 77  ws-work-age                 pic s999 comp-3 value zeros.
000089 77  ws-cm-pri-curr-age          pic s999 comp-3 value +0.
000090 77  ws-cm-cob-curr-age          pic s999 comp-3 value +0.
000091 77  ws-p5-pri-curr-age          pic s999 comp-3 value +0.
000092 77  ws-p5-cob-curr-age          pic s999 comp-3 value +0.
000093 77  ws-alph-curr-age            pic s999 comp-3 value +0.
000094
000095 01  cert-note-records-holder.
000096     05  cert-note-record occurs 300.
000097         10  filler              pic x(48).
000098         10  cnr-rest            pic x(102).
000099
000100******************************************************************
000101
000102 01  ws-hold-elcert              pic x(450) value spaces.
000103 01  ws-return-stuff.
000104     05  ws-eralph-table occurs 25.
000105         10  ws-cov-type         pic x.
000106         10  ws-tbl-last-name    pic x(15).
000107         10  ws-first-name       pic x(10).
000108         10  ws-eff-dt           pic x(10).
000109         10  ws-eralph-cert      pic x(11).
000110         10  ws-eralph-age       pic 99.
000111         10  ws-eralph-prm-sec   pic x.
000112         10  ws-eralph-lf-cd     pic xx.
000113         10  ws-eralph-lf-remamt pic 9(9).99.
000114         10  ws-eralph-lf-remterm pic 999.
000115         10  ws-lf-exp-dt        pic x(10).
000116         10  ws-eralph-ah-cd     pic xx.
000117         10  ws-eralph-ah-amt    pic 9(7).99.
000118         10  ws-eralph-ah-remamt pic 9(9).99.
000119         10  ws-eralph-ah-remterm pic 999.
000120         10  ws-ah-exp-dt        pic x(10).
000121         10  ws-pend             pic x.
000122         10  ws-claim            pic x.
000123         10  ws-delimiter        pic x.
000124
000125 01  ws-alpha-table.
000126     05  ws-eralph-print-table occurs 25.
000127         10  pt-eralph-cert      pic x(11).
000128         10  pt-eralph-lf-cd     pic xx.
000129         10  pt-eralph-lf-remamt pic -zzz,zz9,999.99.
000130         10  pt-eralph-ah-cd     pic xx.
000131         10  pt-eralph-ah-amt    pic -z,zzz,z99.99.
000132         10  pt-eralph-ah-remamt pic -zzz,zzz,999.99.
000133
000134 01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
000135     88  RESP-NORMAL                    VALUE +0.
000136     88  resp-file-notfnd               value +12.
000137     88  RESP-NOTFND                    VALUE +13.
000138     88  resp-duprec                    value +14.
000139     88  resp-dupkey                    value +15.
000140     88  resp-invreq                    value +16.
000141     88  RESP-NOTOPEN                   VALUE +19.
000142     88  RESP-ENDFILE                   VALUE +20.
000143     88  resp-lengtherr                 value +22.
000144
000145 01  ws-alpha-last-name          pic x(15) value spaces.
000146 01  ws-p5-last-name             pic x(15) value spaces.
000147 01  ws-p5-jnt-last-name         pic x(15) value spaces.
000148 01  ws-name-in                  pic x(15) value spaces.
000149 01  ws-name-out                 pic x(15) value spaces.
000150 01  ws-age-from-pndb            pic 99.
000151 01  ws-lf-tot-benefit           pic s9(9)v99 comp-3 value +0.
000152 01  ws-ah-tot-benefit           pic s9(9)v99 comp-3 value +0.
000153 01  pt-lf-tot-benefit           pic -zzz,zzz,999.99.
000154 01  pt-ah-tot-benefit           pic -zzz,zzz,999.99.
000155 01  WS-ERPNDB-REC-HOLD          PIC X(585) VALUE SPACES.
000156 01  WS-MISC.
000157     05  PGM-SUB                 PIC S9(4)   VALUE +515.
000158     05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
000159     05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
000160     05  WS-ZERO                 PIC S9      VALUE ZERO.
000161     05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.
000162     05  ERPNDB-FILE-STATUS      PIC XX    VALUE ZEROS.
000163     05  ERPNDB5-FILE-STATUS     PIC XX    VALUE ZEROS.
000164     05  ERALPH-FILE-STATUS      PIC XX    VALUE ZEROS.
000165     05  ELMSTR5-FILE-STATUS     PIC XX    VALUE ZEROS.
000166     05  WS-DATE                 PIC 9(11) VALUE ZEROS.
000167     05  lf-joint-ind            pic x   value spaces.
000168     05  ah-joint-ind            pic x   value spaces.
000169     05  ws-lf-ben-code          pic xx.
000170     05  ws-ah-ben-code          pic xx.
000171     05  ws-lf-calc-cd           pic x   value spaces.
000172     05  ws-lf-coverage-type     pic x   value spaces.
000173     05  ws-lf-earnings-calc     pic x   value spaces.
000174     05  ws-lf-kind              pic x   value spaces.
000175     05  ws-lf-ben-descrip       pic x(10) value spaces.
000176     05  ws-ah-calc-cd           pic x   value spaces.
000177     05  ws-ah-coverage-type     pic x   value spaces.
000178     05  ws-ah-earnings-calc     pic x   value spaces.
000179     05  ws-ah-kind              pic x   value spaces.
000180     05  ws-ah-ben-descrip       pic x(10) value spaces.
000181
000182 01  ws-elcntl-key.
000183     05  ws-elcntl-company-id    pic xxx.
000184     05  ws-elcntl-rec-type      pic x.
000185         88  ws-elcntl-lf-ben-cd    value '4'.
000186         88  ws-elcntl-ah-ben-cd    value '5'.
000187     05  filler                  pic xx.
000188     05  ws-elcntl-hi-ben-cd     pic xx.
000189     05  ws-elcntl-seq-no        pic s9(4) comp.
000190
000191 01  ws-erpndb-key.
000192     05  ws-erpndb-company-cd    pic x.
000193     05  ws-erpndb-batch         pic x(6).
000194     05  ws-erpndb-seq-no        pic s9(4) comp.
000195     05  ws-erpndb-chg-seq-no    pic s9(4) comp.
000196
000197 01  ws-elcert2-orig-key        pic x(18).
000198 01  ws-elcert-orig-key         pic x(33).
000199 01  ws-elcert-key.
000200     05  ws-elcert-company-cd   pic x.
000201     05  ws-elcert-carrier      pic x.
000202     05  ws-elcert-grouping     pic x(6).
000203     05  ws-elcert-state        pic xx.
000204     05  ws-elcert-account      pic x(10).
000205     05  ws-elcert-eff-dt       pic xx.
000206     05  ws-elcert-cert-no      pic x(11).
000207
000208 01  ws-elcert2-key.
000209     05  ws-elcert2-company-cd  pic x.
000210     05  ws-elcert2-last-name   pic x(15).
000211     05  ws-elcert2-initials    pic xx.
000212
000213 01  WS-CZ-KEY.
000214     05  WS-CZ-COMPANY-CD        PIC X.
000215     05  WS-CZ-CARRIER           PIC X.
000216     05  WS-CZ-GROUP             PIC X(6).
000217     05  WS-CZ-STATE             PIC XX.
000218     05  WS-CZ-ACCOUNT           PIC X(10).
000219     05  WS-CZ-EFF-DT            PIC XX.
000220     05  WS-CZ-CERT-NO           PIC X(11).
000221     05  WS-CZ-REC-TYPE          PIC X.
000222     05  ws-cz-note-seq          pic s9(4) comp.
000223
000224 01  ws-erpndb2-key.
000225     05  ws-erpndb2-company-cd   pic x.
000226     05  ws-erpndb2-carrier      pic x.
000227     05  ws-erpndb2-grouping     pic x(6).
000228     05  ws-erpndb2-state        pic xx.
000229     05  ws-erpndb2-account      pic x(10).
000230     05  ws-erpndb2-eff-dt       pic xx.
000231     05  ws-erpndb2-cert-no      pic x(11).
000232     05  ws-erpndb2-seq-no       pic s9(4) comp.
000233     05  ws-erpndb2-rec-type     pic x.
000234
000235 01  ws-erpndb5-key.
000236     05  ws-erpndb5-company-cd   pic x.
000237     05  ws-erpndb5-carrier      pic x.
000238     05  ws-erpndb5-grouping     pic x(6).
000239     05  ws-erpndb5-state        pic xx.
000240     05  ws-erpndb5-account      pic x(10).
000241     05  ws-erpndb5-eff-dt       pic xx.
000242     05  ws-erpndb5-cert-no      pic x(11).
000243     05  ws-erpndb5-seq-no       pic s9(4) comp.
000244     05  ws-erpndb5-rec-type     pic x.
000245
000246 01  ws-elmstr5-key.
000247     05  ws-elmstr5-company-cd  pic x.
000248     05  ws-elmstr5-cert-no     pic x(11).
000249
000250 01  ws-eralph-aix-key.
000251     05  ws-eralph-aix-company-cd pic x.
000252     05  ws-eralph-aix-account   pic x(10).
000253     05  ws-eralph-aix-lname     pic x(15).
000254     05  ws-eralph-aix-1st-three pic xxx.
000255     05  filler                  pic x(8).
000256
000257 01  ws-passed-key-area.
000258     05  ws-passed-pend-key.
000259         10  ws-pass-company-cd  pic x.
000260         10  ws-pass-carrier     pic x.
000261         10  ws-pass-grouping    pic x(6).
000262         10  ws-pass-state       pic xx.
000263         10  ws-pass-account     pic x(10).
000264         10  ws-pass-eff-dt      pic xx.
000265         10  ws-pass-cert-no     pic x(11).
000266         10  ws-pass-seq-no      pic s9(4) comp.
000267         10  ws-pass-rec-type    pic x.
000268
000269     05  ws-return-area.
000270         10  ws-exceed-limit-yn  pic x.
000271         10  ws-has-claim-yn     pic x.
000272     05  filler                  pic x(62).
000273
000274*                                copy ELCCALC.
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
000275*                                copy ELCCNTL.
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
000276*                                copy ELCCERT.
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
000277*                                copy ELCMSTR.
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
000278*                                copy ERCALPH.
      *>>((file: ERCALPH))
000001******************************************************************
000002*                                                                *
000003*                            ERCALPH.                            *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*   FILE DESCRIPTION = ALPHA FILE                                *
000007*                                                                *
000008*   FILE TYPE = VSAM,KSDS                                        *
000009*   RECORD SIZE = 215  RECFORM = FIXED                           *
000010*                                                                *
000011*   BASE CLUSTER = ERALPH                         RKP=2,LEN=34   *
000012*       ALTERNATE PATH1 = ERALPH2 (FULL CONTROL)  RKP=36,LEN=37  *
000013*                                                                *
000014*   LOG = YES                                                    *
000015*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000016******************************************************************
000017*                   C H A N G E   L O G
000018*
000019* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000020*-----------------------------------------------------------------
000021*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000022* EFFECTIVE    NUMBER
000023*-----------------------------------------------------------------
000024* 102004    2015022600002  PEMA  NEW FILE AND COPYBOOK
000025******************************************************************
000026
000027 01  ALPHA-FILE-REC.
000028     12  AF-RECORD-ID                      PIC XX.
000029         88  VALID-AF-ID                      VALUE 'AF'.
000030     12  AF-CONTROL-PRIMARY.
000031         16  AF-COMPANY-CD                 PIC X.
000032         16  AF-CARRIER                    PIC X.
000033         16  AF-GROUPING.
000034             20  AF-GRP-PREFIX             PIC XXX.
000035             20  AF-GRP-PRIME              PIC XXX.
000036         16  AF-STATE                      PIC XX.
000037         16  AF-ACCOUNT.
000038             20  AF-ACCT-PREFIX            PIC X(4).
000039             20  AF-ACCT-PRIME             PIC X(6).
000040         16  AF-DT                         PIC XX.
000041         16  AF-CERT-NO.
000042             20  AF-CERT.
000043                 24  AF-CERT-PREFIX        PIC XXX.
000044                 24  AF-CERT-PRIME         PIC X(7).
000045             20  AF-CERT-SUFFIX            PIC X.
000046         16  AF-ALPHA-TYPE-CODE            PIC X.
000047             88  AF-INSURED-ALPHA             VALUE 'I'.
000048             88  AF-JOINT-ALPHA               VALUE 'J'.
000049
000050     12  AF-CONTROL-BY-ACCT-NAME.
000051         16  AF-COMPANY-CD-A1              PIC X.
000052         16  AF-ACCOUNT-A1                 PIC X(10).
000053         16  AF-NAME.
000054             20  AF-LNAME                  PIC X(15).
000055             20  AF-FNAME.
000056                 24  AF-1ST-INIT-FNAME     PIC X.
000057                 24  FILLER                PIC X(9).
000058             20  AF-INIT                   PIC X.
000059
000060     12  AF-INSURED-INFO.
000061         16  AF-AGE                        PIC 99.
000062         16  AF-SEX                        PIC X.
000063
000064
000065         16  AF-LIFE-DATA.
000066             20  AF-LF-TYP                 PIC XX.
000067             20  AF-LF-TERM                PIC S999       COMP-3.
000068             20  AF-LF-REMTERM             PIC S999       COMP-3.
000069             20  AF-LF-AMT                 PIC S9(9)V99   COMP-3.
000070             20  AF-LF-REMAMT              PIC S9(9)V99   COMP-3.
000071             20  AF-LF-PRM                 PIC S9(7)V99   COMP-3.
000072             20  AF-LF-AMT-ALT             PIC S9(9)V99   COMP-3.
000073             20  AF-LF-REMAMT-ALT          PIC S9(9)V99   COMP-3.
000074             20  AF-LF-PRM-ALT             PIC S9(7)V99   COMP-3.
000075
000076         16  AF-AH-DATA.
000077             20  AF-AH-TYP                 PIC XX.
000078             20  AF-AH-TERM                PIC S999       COMP-3.
000079             20  AF-AH-REMTERM             PIC S999       COMP-3.
000080             20  AF-AH-AMT                 PIC S9(7)V99   COMP-3.
000081             20  AF-AH-REMAMT              PIC S9(7)V99   COMP-3.
000082             20  AF-AH-PRM                 PIC S9(7)V99   COMP-3.
000083
000084         16  AF-APR                        PIC S999V9(4)  COMP-3.
000085         16  AF-IND-GRP                    PIC X.
000086         16  AF-SOC-NO                     PIC X(11).
000087
000088         16  AF-LF-STATUS                  PIC X.
000089         16  AF-LF-PRE-PLST                PIC X.
000090         16  AF-LF-CNCL                    PIC XX.
000091         16  AF-DEATH                      PIC XX.
000092         16  AF-LF-EXIT                    PIC XX.
000093         16  AF-LF-EXPIRES                 PIC XX.
000094         16  AF-AH-STATUS                  PIC X.
000095         16  AF-AH-PRE-PLST                PIC X.
000096         16  AF-AH-CNCL                    PIC XX.
000097         16  AF-LUMP-SUM                   PIC XX.
000098         16  AF-AH-EXIT                    PIC XX.
000099         16  AF-AH-EXPIRES                 PIC XX.
000100         16  AF-ENTRY                      PIC XX.
000101         16  AF-ENTRY-STATUS               PIC X.
000102         16  FILLER                        PIC X(39).
000103******************************************************************
      *<<((file: ERCALPH))
000279*                                COPY ERCPNDB.
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
000282*                                COPY ERCPNDB
      *     REPLACING LEADING ==PB== BY ==P5==
      *     PENDING-BUSINESS BY P5-PENDING-BUSINESS.
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
000063 01  P5-PENDING-BUSINESS.
000064     12  P5-RECORD-ID                     PIC XX.
000065         88  VALID-PB-ID                        VALUE 'PB'.
000066
000067     12  P5-CONTROL-PRIMARY.
000068         16  P5-COMPANY-CD                PIC X.
000069         16  P5-ENTRY-BATCH               PIC X(6).
000070         16  P5-BATCH-SEQ-NO              PIC S9(4)     COMP.
000071         16  P5-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
000072
000073     12  P5-CONTROL-BY-ACCOUNT.
000074         16  P5-COMPANY-CD-A1             PIC X.
000075         16  P5-CARRIER                   PIC X.
000076         16  P5-GROUPING.
000077             20  P5-GROUPING-PREFIX       PIC XXX.
000078             20  P5-GROUPING-PRIME        PIC XXX.
000079         16  P5-STATE                     PIC XX.
000080         16  P5-ACCOUNT.
000081             20  P5-ACCOUNT-PREFIX        PIC X(4).
000082             20  P5-ACCOUNT-PRIME         PIC X(6).
000083         16  P5-CERT-EFF-DT               PIC XX.
000084         16  P5-CERT-NO.
000085             20  P5-CERT-PRIME            PIC X(10).
000086             20  P5-CERT-SFX              PIC X.
000087         16  P5-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
000088
000089         16  P5-RECORD-TYPE               PIC X.
000090             88  P5-MAILING-DATA                VALUE '0'.
000091             88  P5-ISSUE                       VALUE '1'.
000092             88  P5-CANCELLATION                VALUE '2'.
000093             88  P5-BATCH-TRAILER               VALUE '9'.
000094
000095     12  P5-CONTROL-BY-ORIG-BATCH.
000096         16  P5-ORIGINAL-COMPANY-CD       PIC X.
000097         16  P5-ORIGINAL-ENTRY-BATCH      PIC X(6).
000098         16  P5-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
000099         16  P5-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
000100
000101     12  P5-CONTROL-BY-CSR.
000102         16  P5-CSR-COMPANY-CD            PIC X.
000103         16  P5-CSR-ID                    PIC X(4).
000104         16  P5-CSR-ENTRY-BATCH           PIC X(6).
000105         16  P5-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
000106         16  P5-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
000107******************************************************************
000108*    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
000109******************************************************************
000110
000111     12  P5-LAST-MAINT-DT                 PIC XX.
000112     12  P5-LAST-MAINT-BY                 PIC X(4).
000113     12  P5-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
000114
000115     12  P5-RECORD-BODY                   PIC X(375).
000116
000117     12  P5-ISSUE-RECORD   REDEFINES P5-RECORD-BODY.
000118         16  P5-CERT-ORIGIN               PIC X.
000119             88  CLASIC-CREATED-CERT         VALUE '1'.
000120         16  P5-I-NAME.
000121             20  P5-I-INSURED-LAST-NAME   PIC X(15).
000122             20  P5-I-INSURED-FIRST-NAME.
000123                 24  P5-I-INSURED-1ST-INIT PIC X.
000124                 24  FILLER                PIC X(9).
000125             20  P5-I-INSURED-MIDDLE-INIT PIC X.
000126         16  P5-I-AGE                     PIC S99   COMP-3.
000127         16  P5-I-JOINT-AGE               PIC S99   COMP-3.
000128         16  P5-I-BIRTHDAY                PIC XX.
000129         16  P5-I-INSURED-SEX             PIC X.
000130             88  P5-SEX-MALE     VALUE 'M'.
000131             88  P5-SEX-FEMALE   VALUE 'F'.
000132
000133         16  P5-I-LF-TERM                 PIC S999   COMP-3.
000134         16  P5-I-AH-TERM                 PIC S999   COMP-3.
000135         16  P5-I-LOAN-TERM               PIC S999   COMP-3.
000136         16  P5-I-PAY-FREQUENCY           PIC S99    COMP-3.
000137         16  P5-I-SKIP-CODE               PIC X.
000138             88  P5-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
000139             88  P5-SKIP-JULY              VALUE '1'.
000140             88  P5-SKIP-AUGUST            VALUE '2'.
000141             88  P5-SKIP-SEPTEMBER         VALUE '3'.
000142             88  P5-SKIP-JULY-AUG          VALUE '4'.
000143             88  P5-SKIP-AUG-SEPT          VALUE '5'.
000144             88  P5-SKIP-JULY-AUG-SEPT     VALUE '6'.
000145             88  P5-SKIP-JUNE-JULY-AUG     VALUE '7'.
000146             88  P5-SKIP-JUNE              VALUE '8'.
000147             88  P5-SKIP-JUNE-JULY         VALUE '9'.
000148             88  P5-SKIP-AUG-SEPT-OCT      VALUE 'A'.
000149             88  P5-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
000150         16  P5-I-TERM-TYPE               PIC X.
000151             88  P5-PAID-MONTHLY           VALUE ' ' 'M'.
000152             88  P5-PAID-WEEKLY            VALUE 'W'.
000153             88  P5-PAID-SEMI-MONTHLY      VALUE 'S'.
000154             88  P5-PAID-BI-WEEKLY         VALUE 'B'.
000155             88  P5-PAID-13-YEARLY         VALUE 'T'.
000156         16  P5-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
000157         16  P5-I-POLICY-FORM-NO          PIC X(12).
000158         16  P5-I-DATA-ENTRY-SW           PIC X.
000159             88  P5-EFF-DT-PROCESSING      VALUE '1' ' '.
000160             88  P5-EXT-DAYS-PROCESSING    VALUE '2'.
000161             88  P5-EXPIRE-DT-PROCESSING   VALUE '3'.
000162             88  P5-1ST-PMT-DT-PROCESSING  VALUE '4'.
000163         16  P5-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
000164         16  P5-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
000165*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
000166         16  P5-I-AH-CLP                  PIC S9(5)V99 COMP-3.
000167         16  P5-I-LETTER-REQD             PIC X.
000168
000169         16  P5-I-LIFE-BENEFIT-CD         PIC XX.
000170             88  P5-VALID-LIFE               VALUE '01' THRU '89'.
000171             88  P5-INVALID-LIFE             VALUE '  ' '00'
000172                                                   '90' THRU '99'.
000173         16  P5-I-LF-BENEFIT-CD   REDEFINES P5-I-LIFE-BENEFIT-CD
000174                                          PIC XX.
000175         16  P5-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
000176         16  P5-I-AMOUNT-FINANCED REDEFINES
000177                  P5-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
000178         16  P5-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
000179         16  P5-I-UNPAID-CASH-PRICE REDEFINES
000180                  P5-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
000181         16  P5-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
000182         16  P5-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
000183         16  P5-I-CLP-AMOUNT REDEFINES
000184                  P5-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
000185         16  P5-I-LF-CALC-FLAG            PIC X.
000186             88 P5-COMP-LF-PREM               VALUE '?'.
000187         16  P5-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
000188         16  P5-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
000189         16  P5-I-LF-RATE                 PIC S99V9(5)   COMP-3.
000190         16  P5-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
000191         16  P5-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
000192         16  P5-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
000193         16  P5-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
000194         16  P5-I-LF-ABBR                 PIC XXX.
000195         16  P5-I-LF-INPUT-CD             PIC XX.
000196
000197         16  P5-I-AH-BENEFIT-CD           PIC XX.
000198             88  P5-VALID-AH                 VALUE '01' THRU '89'.
000199             88  P5-INVALID-AH               VALUE '  ' '00'
000200                                                   '90' THRU '99'.
000201         16  P5-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
000202         16  P5-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
000203         16  P5-I-AH-CALC-FLAG            PIC X.
000204             88 P5-COMP-AH-PREM                  VALUE '?'.
000205         16  P5-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
000206         16  P5-I-AH-RATE                 PIC S99V9(5)   COMP-3.
000207         16  P5-I-CANCEL-FEE              PIC S9(3)V99   COMP-3.
000208         16  P5-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
000209         16  P5-I-AH-RATE-TRM             PIC S999       COMP-3.
000210         16  P5-I-AH-ABBR                 PIC XXX.
000211         16  P5-I-AH-INPUT-CD             PIC XXX.
000212
000213         16  P5-I-SPECIAL-REIN-CODE       PIC X.
000214         16  P5-I-REIN-TABLE              PIC XXX.
000215         16  P5-I-BUSINESS-TYPE           PIC 99.
000216         16  P5-I-INDV-GRP-CD             PIC X.
000217         16  P5-I-MORT-CODE.
000218             20  P5-I-TABLE               PIC X.
000219             20  P5-I-INTEREST            PIC XX.
000220             20  P5-I-MORT-TYP            PIC X.
000221         16  P5-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
000222         16  P5-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
000223         16  P5-I-LF-CLP                  PIC S9(5)V99   COMP-3.
000224         16  P5-I-INDV-GRP-OVRD           PIC X.
000225         16  P5-I-RATE-CLASS-OVRD         PIC XX.
000226         16  P5-I-SIG-SW                  PIC X.
000227             88  P5-POLICY-SIGNED             VALUE 'Y'.
000228         16  P5-I-RATE-CLASS              PIC XX.
000229         16  P5-I-RATE-DEVIATION-LF       PIC XXX.
000230         16  P5-I-RATE-DEVIATION-AH       PIC XXX.
000231         16  P5-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
000232         16  P5-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
000233         16  P5-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
000234         16  P5-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
000235         16  P5-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
000236         16  P5-I-BENEFIT-TYPE            PIC XXX.
000237         16  P5-I-OB-FLAG                 PIC X.
000238             88  P5-I-OB                      VALUE 'B'.
000239             88  P5-I-SUMMARY                 VALUE 'Z'.
000240         16  P5-I-ENTRY-STATUS            PIC X.
000241             88  P5-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
000242                                              'M' '5' '9' '2'.
000243             88  P5-I-NORMAL-ENTRY            VALUE '1'.
000244             88  P5-I-POLICY-PENDING          VALUE '2'.
000245             88  P5-I-CONVERSION-ENTRY        VALUE '4'.
000246             88  P5-I-POLICY-IS-REISSUE       VALUE '5'.
000247             88  P5-I-POLICY-IS-CASH          VALUE 'C'.
000248             88  P5-I-POLICY-IS-MONTHLY       VALUE 'M'.
000249             88  P5-I-REIN-ONLY               VALUE '9'.
000250             88  P5-I-POLICY-IS-DECLINED      VALUE 'D'.
000251             88  P5-I-POLICY-IS-VOIDED        VALUE 'V'.
000252             88  P5-I-PREM-ACCTNG-ONLY        VALUE 'P'.
000253             88  P5-I-UNDERWRITE-POLICY       VALUE 'U'.
000254         16  P5-I-INT-CODE                PIC X.
000255             88  P5-ADD-ON-INTEREST           VALUE 'A'.
000256             88  P5-SIMPLE-INTEREST           VALUE 'S'.
000257         16  P5-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
000258         16  P5-I-SOC-SEC-NO              PIC X(11).
000259         16  P5-I-MEMBER-NO               PIC X(12).
000260         16  P5-I-CURR-SEQ                PIC S9(4)       COMP.
000261*        16  PB-I-LOAN-OFFICER            PIC XXX.
000262         16  P5-I-OLD-LOF                 PIC XXX.
000263         16  P5-I-LF-EXPIRE-DT            PIC XX.
000264         16  P5-I-AH-EXPIRE-DT            PIC XX.
000265         16  P5-I-EXTENTION-DAYS          PIC S999        COMP-3.
000266         16  P5-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
000267         16  P5-I-LIFE-INDICATOR          PIC X.
000268             88  P5-I-JOINT-COVERAGE         VALUE 'J'.
000269         16  P5-I-LIVES                   PIC S9(7)       COMP-3.
000270         16  P5-I-DDF-IU-RATE-UP REDEFINES P5-I-LIVES
000271                                          PIC S9(5)V99    COMP-3.
000272         16  P5-I-MAIL-ADDRS-SW           PIC X.
000273             88 P5-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
000274             88 P5-I-MAIL-ADDRS-PRESENT      VALUE '1'.
000275         16  P5-I-1ST-PMT-DT              PIC XX.
000276         16  P5-I-JOINT-INSURED.
000277             20 P5-I-JOINT-LAST-NAME      PIC X(15).
000278             20 P5-I-JOINT-FIRST-NAME.
000279                24  P5-I-JOINT-FIRST-INIT PIC X.
000280                24  FILLER                PIC X(9).
000281             20 P5-I-JOINT-MIDDLE-INIT    PIC X.
000282*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
000283         16  P5-I-BENEFICIARY-NAME.
000284             20  P5-I-BANK-NUMBER         PIC X(10).
000285             20  FILLER                   PIC X(15).
000286         16  P5-I-LAST-ADD-ON-DT          PIC XX.
000287         16  P5-I-REFERENCE               PIC X(12).
000288         16  FILLER REDEFINES P5-I-REFERENCE.
000289             20  P5-I-TOT-FEES            PIC S9(7)V99 COMP-3.
000290             20  P5-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
000291             20  P5-I-CLP-STATE           PIC XX.
000292         16  P5-I-UNDERWRITING-STATUS     PIC X.
000293             88  P5-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
000294             88  P5-I-POLICY-DECLINED         VALUE 'D'.
000295             88  P5-I-NEEDS-UNDERWRITING      VALUE 'U'.
000296         16  P5-I-STATE-TAX               PIC S9(7)V99 COMP-3.
000297         16  P5-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
000298         16  P5-I-RESIDENT-STATE          PIC XX.
000299         16  P5-I-RATE-CODE               PIC X(4).
000300         16  P5-I-NUM-BILLED              PIC S9(7)    COMP-3.
000301         16  P5-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
000302         16  P5-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
000303         16  P5-I-BANK-FEE                PIC S999V99  COMP-3.
000304         16  P5-I-BANK-NOCHRGB            PIC 99.
000305         16  P5-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
000306         16  P5-I-JOINT-BIRTHDAY          PIC XX.
000307
000308     12  P5-CANCEL-RECORD   REDEFINES P5-RECORD-BODY.
000309         16  P5-C-LF-CANCEL-VOID-SW       PIC X.
000310             88  P5-C-LF-CANCEL-VOIDED        VALUE '1'.
000311         16  P5-C-CANCEL-ORIGIN           PIC X.
000312             88  P5-C-CLAIM-CREATED-CANCEL   VALUE '1'.
000313         16  P5-C-LF-CANCEL-DT            PIC XX.
000314         16  P5-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
000315         16  P5-C-LF-CALC-REQ             PIC X.
000316             88 P5-COMP-LF-CANCEL            VALUE '?'.
000317         16  P5-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
000318         16  P5-C-LF-REM-TERM             PIC S9(3)       COMP-3.
000319         16  P5-C-AH-CANCEL-VOID-SW       PIC X.
000320             88  P5-C-AH-CANCEL-VOIDED        VALUE '1'.
000321         16  P5-C-AH-CANCEL-DT            PIC XX.
000322         16  P5-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
000323         16  P5-C-AH-CALC-REQ             PIC X.
000324             88 P5-COMP-AH-CANCEL            VALUE '?'.
000325         16  P5-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
000326         16  P5-C-AH-REM-TERM             PIC S9(3)       COMP-3.
000327         16  P5-C-LAST-NAME               PIC X(15).
000328         16  P5-C-REFUND-SW               PIC X.
000329             88  P5-C-REFUND-CREATED          VALUE 'Y'.
000330             88  P5-C-REFUND-REQUESTED        VALUE 'R'.
000331         16  P5-C-LIVES                   PIC S9(3)       COMP-3.
000332         16  P5-C-PAYEE-CODE              PIC X(6).
000333         16  P5-C-LF-REFUND-OVERRIDE      PIC X.
000334         16  P5-C-AH-REFUND-OVERRIDE      PIC X.
000335         16  P5-C-LF-COMM-CHARGEBACK      PIC X.
000336         16  P5-C-AH-COMM-CHARGEBACK      PIC X.
000337         16  P5-C-REFERENCE               PIC X(12).
000338         16  P5-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
000339         16  P5-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
000340         16  P5-C-POST-CARD-IND           PIC X.
000341         16  P5-C-CANCEL-REASON           PIC X.
000342         16  P5-C-REF-INTERFACE-SW        PIC X.
000343         16  P5-C-LF-RFND-CLP             PIC S9(5)V99 COMP-3.
000344         16  P5-C-AH-RFND-CLP             PIC S9(5)V99 COMP-3.
000345         16  FILLER                       PIC X(01).
000346*        16  FILLER                       PIC X(18).
000347         16  P5-C-POLICY-FORM-NO          PIC X(12).
000348*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
000349         16  P5-C-INT-ON-REFS             PIC S9(7)V99   COMP-3.
000350         16  P5-CANCELED-CERT-DATA.
000351             20  P5-CI-INSURED-NAME.
000352                 24  P5-CI-LAST-NAME      PIC X(15).
000353                 24  P5-CI-INITIALS       PIC XX.
000354             20  P5-CI-INSURED-AGE        PIC S99         COMP-3.
000355             20  P5-CI-INSURED-SEX        PIC X.
000356             20  P5-CI-LF-TERM            PIC S999        COMP-3.
000357             20  P5-CI-LF-BENEFIT-CD      PIC XX.
000358             20  P5-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
000359             20  P5-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
000360             20  P5-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
000361             20  P5-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
000362             20  P5-CI-AH-TERM            PIC S999        COMP-3.
000363             20  P5-CI-AH-BENEFIT-CD      PIC XX.
000364             20  P5-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
000365             20  P5-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
000366             20  P5-CI-RATE-CLASS         PIC XX.
000367             20  P5-CI-RATE-DEV-LF        PIC XXX.
000368             20  P5-CI-RATE-DEV-AH        PIC XXX.
000369             20  P5-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
000370             20  P5-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
000371             20  P5-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
000372             20  P5-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
000373             20  P5-CI-LF-ABBR            PIC X(3).
000374             20  P5-CI-AH-ABBR            PIC X(3).
000375             20  P5-CI-OB-FLAG            PIC X.
000376                 88  P5-CI-OB                VALUE 'B'.
000377             20  P5-CI-LF-POLICY-STATUS   PIC X.
000378                 88  P5-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
000379                                           'M' '4' '5' '9' '2'.
000380                 88  P5-CI-LF-NORMAL-ENTRY           VALUE '1'.
000381                 88  P5-CI-LF-POLICY-PENDING         VALUE '2'.
000382                 88  P5-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
000383                 88  P5-CI-LF-CONVERSION-ENTRY       VALUE '4'.
000384                 88  P5-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
000385                 88  P5-CI-LF-POLICY-IS-CASH         VALUE 'C'.
000386                 88  P5-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
000387                 88  P5-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
000388                 88  P5-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
000389                 88  P5-CI-LF-CANCEL-APPLIED         VALUE '8'.
000390                 88  P5-CI-LF-REIN-ONLY              VALUE '9'.
000391                 88  P5-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
000392                 88  P5-CI-LF-POLICY-IS-VOID         VALUE 'V'.
000393             20  P5-CI-AH-POLICY-STATUS   PIC X.
000394                 88  P5-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
000395                                           'M' '4' '5' '9' '2'.
000396                 88  P5-CI-AH-NORMAL-ENTRY           VALUE '1'.
000397                 88  P5-CI-AH-POLICY-PENDING         VALUE '2'.
000398                 88  P5-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
000399                 88  P5-CI-AH-CONVERSION-ENTRY       VALUE '4'.
000400                 88  P5-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
000401                 88  P5-CI-AH-POLICY-IS-CASH         VALUE 'C'.
000402                 88  P5-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
000403                 88  P5-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
000404                 88  P5-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
000405                 88  P5-CI-AH-CANCEL-APPLIED         VALUE '8'.
000406                 88  P5-CI-AH-REIN-ONLY              VALUE '9'.
000407                 88  P5-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
000408                 88  P5-CI-AH-POLICY-IS-VOID         VALUE 'V'.
000409             20  P5-CI-PAY-FREQUENCY      PIC 99.
000410             20  P5-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
000411             20  P5-CI-SOC-SEC-NO         PIC X(11).
000412             20  P5-CI-MEMBER-NO          PIC X(12).
000413             20  P5-CI-INT-CODE           PIC X.
000414                 88  P5-CI-ADD-ON                  VALUE 'A'.
000415                 88  P5-CI-SIMPLE                  VALUE 'S'.
000416             20  P5-CI-LOAN-TERM          PIC S999        COMP-3.
000417             20  P5-CI-LOAN-1ST-PMT-DT    PIC X(2).
000418             20  P5-CI-COMP-EXCP-SW       PIC X.
000419                 88  P5-CI-NO-COMP-EXCP            VALUE ' '.
000420                 88  P5-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
000421             20  P5-CI-ENTRY-STATUS       PIC X.
000422             20  P5-CI-CURR-SEQ           PIC S9(4)       COMP.
000423             20  P5-CI-AH-PAID-THRU-DT    PIC XX.
000424             20  P5-CI-AH-SETTLEMENT-DT   PIC XX.
000425             20  P5-CI-DEATH-DT           PIC XX.
000426             20  P5-CI-LF-PRIOR-CANCEL-DT PIC XX.
000427             20  P5-CI-AH-PRIOR-CANCEL-DT PIC XX.
000428             20  P5-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
000429             20  P5-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
000430             20  P5-CI-CREDIT-INTERFACE-SW-1 PIC X.
000431             20  P5-CI-CREDIT-INTERFACE-SW-2 PIC X.
000432             20  P5-CI-ENTRY-DT              PIC XX.
000433             20  P5-CI-ENTRY-BATCH           PIC X(6).
000434             20  P5-CI-LF-EXPIRE-DT          PIC XX.
000435             20  P5-CI-AH-EXPIRE-DT          PIC XX.
000436             20  P5-CI-EXTENTION-DAYS        PIC S999     COMP-3.
000437             20  P5-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
000438             20  P5-CI-OLD-LOF               PIC XXX.
000439*            20  PB-CI-LOAN-OFFICER          PIC XXX.
000440             20  P5-CI-LIVES                 PIC S9(3)    COMP-3.
000441             20  P5-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
000442             20  P5-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
000443             20  P5-CI-INDV-GRP-CD           PIC X.
000444             20  P5-CI-BENEFICIARY-NAME.
000445                 24  P5-CI-BANK-NUMBER       PIC X(10).
000446                 24  FILLER                  PIC X(15).
000447             20  P5-CI-NOTE-SW               PIC X.
000448             20  P5-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
000449             20  P5-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
000450             20  P5-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
000451             20  P5-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
000452             20  P5-CI-LOAN-OFFICER          PIC X(5).
000453             20  P5-CI-BOW-LOAN-NUMBER       PIC X(14).
000454             20  P5-CI-FIRST-NAME            PIC X(10).
000455             20  P5-CI-DDF-IU-RATE-UP        PIC S9(5)V99 COMP-3.
000456
000457         16  FILLER                       PIC X(13).
000458*032306  16  FILLER                       PIC X(27).
000459*        16  FILLER                       PIC X(46).
000460
000461     12  P5-MAIL-RECORD    REDEFINES P5-RECORD-BODY.
000462         16  FILLER                       PIC X(10).
000463         16  P5-M-INSURED-LAST-NAME       PIC X(15).
000464         16  P5-M-INSURED-FIRST-NAME      PIC X(10).
000465         16  P5-M-INSURED-MID-INIT        PIC X.
000466         16  P5-M-INSURED-AGE             PIC 99.
000467         16  P5-M-INSURED-BIRTHDAY        PIC XX.
000468         16  P5-M-INSURED-SEX             PIC X.
000469         16  P5-M-INSURED-SOC-SEC-NO      PIC X(11).
000470         16  P5-M-INSURED-ADDRESS-1       PIC X(30).
000471         16  P5-M-INSURED-ADDRESS-2       PIC X(30).
000472         16  P5-M-INSURED-CITY-STATE.
000473             20  P5-M-INSURED-CITY        PIC X(28).
000474             20  P5-M-INSURED-STATE       PIC XX.
000475         16  P5-M-INSURED-ZIP-CODE.
000476             20  P5-M-INSURED-ZIP-PRIME.
000477                 24  P5-M-INSURED-ZIP-1   PIC X.
000478                     88  P5-M-CANADIAN-POST-CODE
000479                                             VALUE 'A' THRU 'Z'.
000480                 24  FILLER               PIC X(4).
000481             20  P5-M-INSURED-ZIP-PLUS4   PIC X(4).
000482         16  P5-M-INSURED-CANADIAN-ZIP  REDEFINES
000483                                        P5-M-INSURED-ZIP-CODE.
000484             20  PM-M-INS-CAN-POST1       PIC XXX.
000485             20  PM-M-INS-CAN-POST2       PIC XXX.
000486             20  FILLER                   PIC XXX.
000487         16  P5-M-INSURED-PHONE-NO        PIC 9(10).
000488         16  P5-M-JOINT-BIRTHDAY          PIC XX.
000489         16  P5-M-CRED-BENE-NAME          PIC X(30).
000490         16  P5-M-CRED-BENE-ADDR1         PIC X(30).
000491         16  P5-M-CRED-BENE-ADDR2         PIC X(30).
000492         16  P5-M-CRED-BENE-CITYST.
000493             20  P5-M-CRED-BENE-CITY      PIC X(28).
000494             20  P5-M-CRED-BENE-STATE     PIC XX.
000495
000496         16  FILLER                       PIC X(92).
000497
000498     12  P5-BATCH-RECORD   REDEFINES P5-RECORD-BODY.
000499         16  FILLER                       PIC X(10).
000500         16  P5-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000501         16  P5-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000502         16  P5-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000503         16  P5-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000504         16  P5-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000505         16  P5-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000506         16  P5-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000507         16  P5-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000508         16  P5-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000509         16  P5-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
000510         16  P5-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
000511         16  P5-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
000512         16  P5-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
000513         16  P5-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
000514         16  P5-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
000515         16  P5-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
000516         16  P5-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
000517         16  P5-ACCOUNT-NAME              PIC X(30).
000518         16  P5-PREM-REF-RPT-FLAG         PIC X.
000519         16  P5-REFERENCE                 PIC X(12).
000520         16  P5-B-RECEIVED-DT             PIC XX.
000521         16  FILLER                       PIC X(234).
000522
000523     12  P5-RECORD-STATUS.
000524         16  P5-CREDIT-SELECT-DT          PIC XX.
000525         16  P5-CREDIT-ACCEPT-DT          PIC XX.
000526         16  P5-BILLED-DT                 PIC XX.
000527         16  P5-BILLING-STATUS            PIC X.
000528             88  P5-ENTRY-REVERSED            VALUE 'R'.
000529             88  P5-EL860-INTERNAL-ERROR      VALUE 'E'.
000530             88  P5-EL860-INTERNAL-PROCESS    VALUE 'P'.
000531         16  P5-RECORD-BILL               PIC X.
000532             88  P5-RECORD-ON-HOLD            VALUE 'H'.
000533             88  P5-RECORD-RETURNED           VALUE 'R'.
000534             88  P5-RECORD-ENDORSED           VALUE 'E'.
000535             88  P5-OVERRIDE-LIFE             VALUE 'L'.
000536             88  P5-OVERRIDE-AH               VALUE 'A'.
000537             88  P5-OVERRIDE-BOTH             VALUE 'B'.
000538         16  P5-BATCH-ENTRY               PIC X.
000539             88  P5-POLICY-IS-DECLINED        VALUE 'D'.
000540             88  P5-REIN-ONLY-CERT            VALUE 'R'.
000541             88  P5-REISSUED-CERT             VALUE 'E'.
000542             88  P5-CASH-CERT                 VALUE 'C'.
000543             88  P5-MONTHLY-CERT              VALUE 'M'.
000544             88  P5-PREM-ACCTNG-ONLY          VALUE 'P'.
000545             88  P5-NEEDS-UNDERWRITING        VALUE 'U'.
000546             88  P5-POLICY-IS-VOIDED          VALUE 'V'.
000547         16  P5-FORCE-CODE                PIC X.
000548             88  P5-FORCE-OFF                 VALUE ' ' '0'.
000549             88  P5-ISSUE-FORCE               VALUE 'A' 'O'.
000550             88  P5-CANCEL-FORCE              VALUE '8'.
000551             88  P5-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
000552             88  P5-ALL-CANCEL-FORCED         VALUE '8'.
000553             88  P5-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
000554             88  P5-CANCEL-DATE-FORCED        VALUE 'D'.
000555             88  P5-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
000556             88  P5-ISSUE-DATE-FORCED         VALUE 'D'.
000557             88  P5-EXCEEDED-LIMIT-FORCED     VALUE 'L'.
000558             88  P5-OVERCHARGE-FORCE          VALUE 'O'.
000559         16  P5-FATAL-FLAG                PIC X.
000560             88  P5-FATAL-ERRORS              VALUE 'X'.
000561         16  P5-FORCE-ER-CD               PIC X.
000562             88  P5-FORCE-ERRORS              VALUE 'F'.
000563             88  P5-UNFORCED-ERRORS           VALUE 'X'.
000564         16  P5-WARN-ER-CD                PIC X.
000565             88  P5-WARNING-ERRORS            VALUE 'W'.
000566         16  FILLER                       PIC X.
000567         16  P5-OUT-BAL-CD                PIC X.
000568             88  P5-OUT-OF-BAL                VALUE 'O'.
000569         16  P5-LIFE-OVERRIDE-L1          PIC X.
000570         16  P5-AH-OVERRIDE-L1            PIC X.
000571         16  P5-INPUT-DT                  PIC XX.
000572         16  P5-INPUT-BY                  PIC X(4).
000573         16  P5-CHG-COUNT                 PIC 9(3)        COMP-3.
000574         16  P5-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
000575         16  P5-TOLERANCE-REJECT-SW       PIC X.
000576         16  P5-LF-EARNING-METHOD         PIC X.
000577         16  P5-AH-EARNING-METHOD         PIC X.
000578         16  P5-LF-TERM-CALC-METHOD       PIC X.
000579         16  P5-AH-TERM-CALC-METHOD       PIC X.
000580         16  P5-REIN-CD                   PIC XXX.
000581         16  P5-LF-REFUND-TYPE            PIC X.
000582         16  P5-AH-REFUND-TYPE            PIC X.
000583         16  P5-ACCT-EFF-DT               PIC XX.
000584         16  P5-ACCT-EXP-DT               PIC XX.
000585         16  P5-COMPANY-ID                PIC X(3).
000586         16  P5-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
000587         16  P5-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
000588         16  P5-SV-CARRIER                PIC X.
000589         16  P5-SV-GROUPING               PIC X(6).
000590         16  P5-SV-STATE                  PIC XX.
000591         16  P5-CONFIRMATION-REPT-DT      PIC XX.
000592         16  P5-GA-BILLING-INFO.
000593             20  P5-GA-BILL-DT OCCURS 5 TIMES
000594                                          PIC XX.
000595         16  P5-SV-REMIT-TO  REDEFINES
000596             P5-GA-BILLING-INFO           PIC X(10).
000597         16  P5-NO-OF-ERRORS              PIC S9(3) COMP-3.
000598         16  P5-I-LOAN-OFFICER            PIC X(5).
000599         16  P5-I-VIN                     PIC X(17).
000600
000601         16  FILLER                       PIC X(04).
000602         16  IMNET-BYPASS-SW              PIC X.
000603
000604******************************************************************
000605*                COMMON EDIT ERRORS                              *
000606******************************************************************
000607
000608     12  P5-COMMON-ERRORS.
000609         16  P5-COMMON-ERROR    OCCURS 10 TIMES
000610                                           PIC S9(4)     COMP.
000611
000612******************************************************************
      *<<((file: ERCPNDB))
000283*                                COPY ELCDATE.
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
000284*                                COPY ELCDTECX.
      *>>((file: ELCDTECX))
000001******************************************************************
000002*                                                                *
000003*                            ELCDTECX.                           *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.015                          *
000006*                                                                *
000007*      WORKING STORAGE COPY CODE FOR APPLICATIONS CALLING        *
000008*      THE DATE CARD DISK FILE. ( READ ROUTINE = ELCDTERX,       *
000009*      AND DISK FILE FD = ELCDTEFX)                              *
000010*                                                                *
000011******************************************************************
000012******************************************************************
000013*                   C H A N G E   L O G
000014*
000015* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000016*-----------------------------------------------------------------
000017*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000018* EFFECTIVE    NUMBER
000019*-----------------------------------------------------------------
000020* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
000021*                                  BENEFIT CODES FROM 300 TO 900
000022* 090803    2003080800002  PEMA  ADD CATEGORY FOR SUPER GAP
000023* 051304    2003080800002  SMVA  ADD REFUND METHOD
000024* 071714    2013100100002  PEMA  FIX CRIT PERIOD EDITS
000025* 052218  CR2014061700002  PEMA  INCREASE MORT TABLES TABLE
000026******************************************************************
000027 01  DATE-CARD.
000028     12  DATX-COD                    PIC  X(4).
000029     12  RUN-DATE                    PIC  9(11)  COMP-3.
000030     12  ALPH-DATE                   PIC  X(18).
000031     12  EP-DT                       PIC  9(11)  COMP-3.
000032     12  PEND-ACT-FILE               PIC  XX.
000033     12  EP-SW                       PIC  X.
000034         88  NO-EP-EXTRACTS          VALUE SPACE.
000035     12  TAPE-BATCHES                PIC  X.
000036         88  TP-PREL-BAL             VALUE '1'.
000037     12  BIN-RUN-DATE                PIC  XX.
000038     12  RUN-CENTURY.
000039         16 RUN-CENTURY-N            PIC  99.
000040     12  FILLER                      PIC  X.
000041     12  CLAS-MORT-OVRD              PIC  X(4).
000042     12  FILLER                      PIC  XXX.
000043     12  COMPANY-NAME                PIC  X(30).
000044
000045 01  FILLER                          PIC  X(7)
000046                                          VALUE 'OPTIONS'.
000047 01  DATE-CARD-OPTIONS.
000048     12  DATE-OPTIONS-ID             PIC  X(4).
000049     12  DTE-MIN-PREM                PIC  99V99.
000050     12  DTE-LANGUAGE-IND            PIC  X.
000051         88  DTE-LANGUAGE-IS-FR           VALUE 'F'.
000052         88  DTE-LANGUAGE-IS-ENG          VALUE 'E'.
000053     12  DTE-MIN-AGE                 PIC  99.
000054     12  DTE-DEFAULT-AGE             PIC  99.
000055     12  FILLER                      PIC  X.
000056     12  DTE-MAX-TERM                PIC  999.
000057     12  DTE-COMP-VG                 PIC  X.
000058     12  DTE-REM-TRM                 PIC  X.
000059     12  DTE-PRM-CK                  PIC S99V99.
000060     12  DTE-PRORATA                 PIC  X.
000061     12  DTE-CLM-REJ                 PIC  X.
000062     12  DTE-REF-REJ                 PIC  X.
000063     12  DTE-COM-TBL-USED            PIC  X.
000064     12  FILLER                      PIC  X.
000065     12  DTE-QTR-CO                  PIC  X.
000066     12  DTE-REF-CK                  PIC S99V99.
000067     12  DTE-CLM-CK                  PIC S99V99.
000068     12  DTE-CONV-DT                 PIC 9(11) COMP-3.
000069     12  DTE-DEFAULT-SEX             PIC  X.
000070     12  DTE-EPL-FORMAT              PIC  X.
000071     12  FILLER                      PIC  X.
000072     12  DTE-JT-AGE                  PIC  X.
000073     12  DTE-KEY-BIRTH               PIC  X.
000074     12  DTE-REINSURANCE             PIC  X.
000075     12  DTE-CLAIM-SORT              PIC  X.
000076     12  DTE-WRT-OFF                 PIC  S99V99.
000077     12  DTE-R78                     PIC  X.
000078     12  DTE-DLY-BILL                PIC  X.
000079     12  DTE-ALT-MORT-CODE           PIC  X(4).
000080     12  DTE-CLAIM-PAID-THRU-TO      PIC  X.
000081     12  DTE-COMPENSATION-ACCESS     PIC  X.
000082     12  DTE-MORTG-ACCESS-CNTL       PIC  X.
000083     12  DTE-MP-ALT-MORT-CODE        PIC  X(4).
000084     12  DTE-MORTALITY-AGE-CALC-METHOD
000085                                     PIC  X.
000086         88  DTE-USE-TABLE-ASSIGNED-METHOD VALUE '1'
000087                                                 ' '.
000088         88  DTE-USE-ALL-AGE-LAST          VALUE '2'.
000089         88  DTE-USE-ALL-AGE-NEAR          VALUE '3'.
000090     12  DTE-RESERVE-OPTION-SWITCH   PIC  X.
000091         88  DTE-OPT-RESERVE-METHOD-AUTH       VALUE 'Y'.
000092         88  DTE-OPT-RESERVE-METHOD-UNAUTH     VALUE 'N' ' '.
000093     12  DTE-REM-TRM-CALC-OPTION     PIC  X.
000094     12  DTE-EXPERIENCE-RETENTION-AGE
000095                                     PIC  9.
000096     12  DTE-SYSTEM.
000097         16  DTE-SYS-A-CREDIT        PIC  X.
000098         16  DTE-SYS-B-PEND-CLAIM    PIC  X.
000099         16  DTE-SYS-C-CONFIRMATIONS PIC  X.
000100         16  DTE-SYS-D-DEMAND-BILL   PIC  X.
000101         16  DTE-SYS-E-CLASIC-CLAIMS PIC  X.
000102         16  DTE-SYS-F-CLASIC-CREDIT PIC  X.
000103         16  DTE-SYS-G-AR-USED       PIC  X.
000104         16  DTE-SYS-H               PIC  X.
000105         16  DTE-SYS-I               PIC  X.
000106         16  DTE-SYS-J               PIC  X.
000107         16  DTE-SYS-K               PIC  X.
000108         16  DTE-SYS-L               PIC  X.
000109         16  DTE-SYS-M               PIC  X.
000110         16  DTE-SYS-N               PIC  X.
000111         16  DTE-SYS-O               PIC  X.
000112         16  DTE-SYS-P               PIC  X.
000113         16  DTE-SYS-Q               PIC  X.
000114         16  DTE-SYS-R               PIC  X.
000115         16  DTE-SYS-S               PIC  X.
000116         16  DTE-SYS-T               PIC  X.
000117         16  DTE-SYS-U               PIC  X.
000118         16  DTE-SYS-V               PIC  X.
000119         16  DTE-SYS-W               PIC  X.
000120         16  DTE-SYS-X               PIC  X.
000121         16  DTE-SYS-Y               PIC  X.
000122         16  DTE-SYS-Z               PIC  X.
000123     12  FILLER                      REDEFINES DTE-SYSTEM.
000124         16  DTE-SYS-CODE            OCCURS 26 TIMES
000125                                     PIC  X.
000126     12  DTE-CLIENT                  PIC  XXX.
000127
000128 01  CLASIC-SYSTEM-CODES.
000129     12  DTE-COLC-ID                 PIC  X(4).
000130     12  DTE-CLASIC-COMPANY-CD       PIC  X.
000131     12  DTE-CLASIC-COMPANY-NUMBER   PIC  999.
000132*    12  DTE-CLASIC-CLAIM-ACCESS     PIC  X.
000133     12  FILLER                      PIC  X.
000134     12  CLASIC-REIN-MAINT           PIC  XX.
000135     12  CLASIC-COMP-MAINT           PIC  XX.
000136     12  CLASIC-ACCT-MAINT           PIC  XX.
000137     12  CLASIC-CTBL-MAINT           PIC  XX.
000138     12  CLASIC-RATE-MAINT           PIC  XX.
000139     12  CLASIC-CREDIT-EOM-DT        PIC  XX.
000140     12  CLASIC-CLAIMS-EOM-DT        PIC  XX.
000141
000142     12  LIFE-OVERRIDE-L1            PIC  X.
000143     12  LIFE-OVERRIDE-L2            PIC  XX.
000144     12  LIFE-OVERRIDE-L6            PIC  X(6).
000145     12  LIFE-OVERRIDE-L12           PIC  X(12).
000146
000147     12  AH-OVERRIDE-L1              PIC  X.
000148     12  AH-OVERRIDE-L2              PIC  XX.
000149     12  AH-OVERRIDE-L6              PIC  X(6).
000150     12  AH-OVERRIDE-L12             PIC  X(12).
000151
000152     12  CLAS-REPORT-CD1-CAPTION     PIC  X(10).
000153     12  CLAS-REPORT-CD2-CAPTION     PIC  X(10).
000154
000155     12  CLASIC-MORTG-EOM-DT         PIC  XX.
000156     12  CLASIC-AR-EOM-DT            PIC  XX.
000157
000158     12  FILLER                      PIC  X(11)      VALUE SPACE.
000159
000160 01  DATE-CARD-FACTORS.
000161     12  DATE-FACTOR-ID              PIC  X(4).
000162     12  FAC-1                       PIC S999V9(5).
000163     12  FAC-2                       PIC S999V9(5).
000164     12  FAC-3                       PIC S999V9(5).
000165     12  FAC-4                       PIC S999V9(5).
000166     12  FAC-5                       PIC S999V9(5).
000167     12  FAC-6                       PIC S999V9(5).
000168
000169 01  FILLER                          PIC  X(12)
000170                                          VALUE 'COMPANY NAME'.
000171 01  COMPANY-NAME-TABLE.
000172     12  C-N-TBL                     OCCURS 6 TIMES.
000173         16  CNT-ID                  PIC  X.
000174         16  CNT-NAME                PIC  X(30).
000175
000176 01  FILLER                          PIC  X(11)
000177                                          VALUE 'STATE NAMES'.
000178 01  STATE-NAMES.
000179     12  STATE-NAME-FLD              OCCURS 75 TIMES.
000180         16  STATE-SUB               PIC  XX.
000181         16  STATE-PIC1.
000182             20  STATE-ABBR          PIC  XX.
000183             20  FILLER              PIC  XXX.
000184             20  STATE-PIC           PIC  X(20).
000185             20  STATE-CALL-EARN     PIC  X.
000186             20  STATE-CALL-BREAK    PIC  X.
000187             20  STATE-PRIM-FAC-DEV  PIC  XXX.
000188
000189 01  FILLER                          PIC  X(13)
000190                                          VALUE 'CARRIER NAMES'.
000191 01  CARRIER-NAMES.
000192     12  CARRIER-NAME-FLD            OCCURS 25 TIMES.
000193         16  CARRIER-SUB             PIC  X.
000194         16  CARRIER-DOM-ST          PIC  XX.
000195         16  CARRIER-PIC             PIC  X(30).
000196         16  CARRIER-UEP-PCT         PIC  S9V9(4) COMP-3.
000197         16  CARRIER-R78-PCT         PIC  S9V9(4) COMP-3.
000198         16  CARRIER-PRO-PCT         PIC  S9V9(4) COMP-3.
000199*        16  FILLER                  PIC  X.
000200         16  CARRIER-SEC-PAY         PIC  X.
000201             88  SEC-PAY-CARRIER          VALUE 'Y'.
000202
000203 01  FILLER                          PIC  X(9)
000204                                          VALUE 'INS TYPES'.
000205 01  CLAS-INS-TYPES.
000206*    12 CLAS-ALL-TYPES               OCCURS 300 TIMES.
000207     12 CLAS-ALL-TYPES               OCCURS 900 TIMES.
000208         16  CLAS-I-BEN              PIC  XX.
000209         16  CLAS-I-AB3.
000210             20  FILLER              PIC  X.
000211             20  CLAS-I-AB2.
000212                 24  FILLER          PIC  X.
000213                 24  CLAS-I-AB1      PIC  X.
000214         16  CLAS-I-AB3-AH REDEFINES CLAS-I-AB3.
000215             20  CLAS-EXCLUSION      PIC  XX.
000216             20  FILLER              PIC  X.
000217         16  CLAS-I-AB10.
000218             20  FILLER              PIC  X(9).
000219             20  CLAS-I-REIN-YN      PIC  X.
000220         16  CLAS-I-COMMENT          PIC  X(10).
000221         16  CLAS-I-JOINT            PIC  X.
000222         16  CLAS-I-RL-AH            PIC  X.
000223         16  CLAS-I-CALC-TYPE.
000224             20  CLAS-I-BAL          PIC  X.
000225         16  CLAS-I-EP               PIC  X.
000226         16  CLAS-CO-BEN-I-G-CD      PIC  X.
000227         16  CLAS-CO-REM-TERM-CALC   PIC  X.
000228         16  CLAS-I-BEN-CATEGORY     PIC  X.
000229         16  CLAS-I-REFUND-METHOD    PIC  X.
000230         16  CLAS-I-MAX-BENEFITS     PIC 99.
000231         16  FILLER                  PIC  XXX.
000232
000233 01  FILLER                          PIC  X(16)
000234                                     VALUE 'MORTALITY TABLES'.
000235 01  CLAS-MORTALITY-CODES.
000236     12  CLAS-MORT-FLD               OCCURS 130 TIMES
000237                                     INDEXED BY CLAS-MORT-NDX.
000238         16  CLAS-MORT-CODE          PIC  X(4).
000239         16  CLAS-MORT-J-CODE        PIC  X.
000240         16  CLAS-MORT-J-FACT        PIC S9V9(4).
000241         16  CLAS-MORT-DESC.
000242             20  CLAS-RESERVE-ADJ    PIC  999.
000243             20  FILLER              PIC  XX.
000244             20  CLAS-YEAR           PIC  X(4).
000245             20  FILLER              PIC  X.
000246             20  CLAS-TABLE-TYPE     PIC  XXX.
000247             20  FILLER              PIC  X.
000248             20  CLAS-AGE-METHOD     PIC  XX.
000249             20  FILLER              PIC  X.
000250             20  CLAS-INTEREST       PIC  99.99.
000251             20  FILLER              PIC  X(5).
000252
000253 01  FILLER                          PIC  X(15)
000254                                     VALUE 'BUSINESS TABLES'.
000255 01  CLAS-BUSINESS-CLASSES.
000256     12  CLAS-BUSC-FLD               OCCURS 50 TIMES.
000257         16  CLAS-BUSC-CODE          PIC  99.
000258         16  CLAS-BUSC-GROUP         PIC  X.
000259         16  CLAS-BUSC-DESC          PIC  X(19).
000260         16  CLAS-BUSC-EXCL          PIC  X.
000261         16  FILLER                  PIC  X(4).
000262
000263 01  FILLER                          PIC  X(7)
000264                                     VALUE 'INDEXES'.
000265 01  CLAS-INDEX-TBL.
000266     12  CLAX-ID                     PIC  X(4).
000267     12  CLAS-STARTC                 PIC S9(4) COMP.
000268     12  CLAS-MAXC                   PIC S9(4) COMP.
000269     12  CLAS-STARTL                 PIC S9(4) COMP.
000270     12  CLAS-MAXL                   PIC S9(4) COMP.
000271     12  CLAS-STARTA                 PIC S9(4) COMP.
000272     12  CLAS-MAXA                   PIC S9(4) COMP.
000273     12  CLAS-STARTM                 PIC S9(4) COMP.
000274     12  CLAS-MAXM                   PIC S9(4) COMP.
000275     12  CLAS-STARTB                 PIC S9(4) COMP.
000276     12  CLAS-MAXB                   PIC S9(4) COMP.
000277     12  CLAS-STARTS                 PIC S9(4) COMP.
000278     12  CLAS-MAXS                   PIC S9(4) COMP.
000279     12  CLAS-STARTE                 PIC S9(4) COMP.
000280     12  CLAS-MAXE                   PIC S9(4) COMP.
000281     12  CLAS-STARTCN                PIC S9(4) COMP.
000282     12  CLAS-MAXCN                  PIC S9(4) COMP.
000283
000284 01  CLAS-TYPE-MISC.
000285     12  CLAS-INDEXC                 PIC S9(4) COMP.
000286     12  CLAS-INDEXL                 PIC S9(4) COMP.
000287     12  CLAS-INDEXA                 PIC S9(4) COMP.
000288     12  CLAS-INDEXM                 PIC S9(4) COMP.
000289     12  CLAS-INDEXB                 PIC S9(4) COMP.
000290     12  CLAS-INDEXS                 PIC S9(4) COMP.
000291     12  CLAS-INDEXE                 PIC S9(4) COMP.
000292     12  CLAS-INDEX                  PIC S9(4) COMP.
000293     12  CLAS-INDEXEN                PIC S9(4) COMP.
000294     12  CLAS-INDEXCN                PIC S9(4) COMP.
000295     12  CLAS-INDEXON                PIC S9(4) COMP.
000296
000297 01  FILLER                          PIC  X(13)
000298                                     VALUE 'MISCELLANEOUS'.
000299 01  CLAS-MISC.
000300     12  DTE-FICH                    PIC  X.
000301         88  FICH-NO                 VALUE SPACE.
000302         88  FICH-ONLY               VALUE '1'.
000303         88  FICH-BOTH               VALUE '2'.
000304     12  FICH-OPEN                   PIC  X.
000305     12  REPT-OPEN                   PIC  X.
000306     12  DTE-PGM-OPT                 PIC  9.
000307     12  DTE-PRT-OPT                 PIC  X.
000308     12  DTE-FMT-OPT                 PIC  9.
000309     12  DTE-PRC-OPT                 PIC  9.
000310     12  DTE-TOT-OPT                 PIC  9.
000311     12  CLAS-LOOK                   PIC  XX.
000312     12  DTE-TOT-LINES               PIC S9(8)      COMP.
000313     12  STATE-L                     PIC  XX.
000314     12  CLAS-CN                     PIC  99.
000315     12  CLAS-CO                     PIC  99.
000316     12  DTE-VSAM-FLAGS                              VALUE ZERO.
000317         16  DTE-F-1                 PIC  X.
000318         16  DTE-F-2                 PIC  X.
000319     12  DTE-ABEND-WORK.
000320         16  DTE-ABEND-CD-1          PIC  XX         VALUE SPACES.
000321         16  DTE-ABEND-CD-2          PIC  XX         VALUE SPACES.
000322
000323 01  FILLER                          PIC  X(12)
000324                                     VALUE 'WORKING DATE'.
000325 01  WS-DATE-AND-TIME.
000326     12  WS-ACCEPT-DATE.
000327         16  WS-AD-YY                PIC  99.
000328         16  WS-AD-MM                PIC  99.
000329         16  WS-AD-DD                PIC  99.
000330     12  WS-CURRENT-DATE.
000331         16  WS-CD-MM                PIC  99.
000332         16  FILLER                  PIC  X          VALUE '/'.
000333         16  WS-CD-DD                PIC  99.
000334         16  FILLER                  PIC  X          VALUE '/'.
000335         16  WS-CD-YY                PIC  99.
000336     12  WS-TIME-OF-DAY.
000337         16  WS-TIME                 PIC  9(6).
000338         16  WS-HUN-SEC              PIC  99.
000339
000340 01  FILLER                          PIC  X(12)
000341                                     VALUE 'CARRIER CLMS'.
000342 01  CARRIER-OPT-CLAIM-DATA.
000343     12  CARRIER-FLDS                OCCURS 25 TIMES.
000344         16  CARRIER-IBNR-SWITCH     PIC  X.
000345         16  CARRIER-IBNR-PERCENT    PIC S9V9(4) COMP-3.
000346         16  CARRIER-CIDA-DISCOUNT   PIC S9V9(4) COMP-3.
000347
000348 01  FILLER                          PIC  X(10)
000349                                     VALUE 'STATE TLRS'.
000350 01  STATE-TARGET-LOSS-RATIOS.
000351     12  STATE-TLR-FLD               OCCURS 75 TIMES.
000352         16  STATE-TARGET-LOSS-RATIO PIC S9V9(4) COMP-3.
000353         16  STATE-CALC-INTEREST     PIC S9V9(4) COMP-3.
000354
000355 01  FILLER                          PIC  X(13)
000356                                     VALUE 'BUSINESS TLRS'.
000357 01  BUSINESS-TRGT-LOSS-RATIO-MODS.
000358     12  BUS-TLRM-FLD                OCCURS 99 TIMES.
000359         16  BUS-TRGT-LOSS-RATIO-MOD PIC S9V9(4) COMP-3.
000360
000361 01  FILLER                          PIC  X(13)
000362                                     VALUE 'MISC CLM DATA'.
000363 01  MISC-OPT-CLM-RSV-DATA.
000364     12  COMPANY-CALC-INTEREST       PIC S9V9(4) COMP-3.
000365     12  COMPANY-CIDA-DISCOUNT       PIC S9V9(4) COMP-3.
000366     12  COMPANY-CRDB-TABLE-SELECTION
000367                                     PIC  X.
000368     12  COMPANY-IBNR-AH-FACTOR      PIC S9V9(4) COMP-3.
000369     12  COMPANY-IBNR-LAG-MONTH      PIC S999    COMP-3.
000370     12  COMPANY-IBNR-LIFE-FACTOR    PIC S9V9(4) COMP-3.
000371     12  COMPANY-OPTION-START-DATE   PIC  XX.
000372     12  INDEXBS                     PIC S9(4)   COMP.
000373     12  INDEXCA                     PIC S9(4)   COMP.
000374     12  INDEXST                     PIC S9(4)   COMP.
000375     12  FILLER                      PIC  X(75).
000376******************************************************************
000377*01  MISC-CARR-PCT-DATA.
000378*    12  DD-CARR1-UEP-PCT            PIC S9V9(4) COMP-3.
000379*    12  DD-CARR1-R78-PCT            PIC S9V9(4) COMP-3.
000380*    12  DD-CARR1-PRO-PCT            PIC S9V9(4) COMP-3.
000381*    12  DD-CARR2-UEP-PCT            PIC S9V9(4) COMP-3.
000382*    12  DD-CARR2-R78-PCT            PIC S9V9(4) COMP-3.
000383*    12  DD-CARR2-PRO-PCT            PIC S9V9(4) COMP-3.
000384******************************************************************
      *<<((file: ELCDTECX))
000285*                                COPY ELCDTEVR.
      *>>((file: ELCDTEVR))
000001*****************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCDTEVR.                           *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.001                          *
000007*                                                                *
000008*   WORKING STORAGE FOR DATE VARIABLES CREATED                   *
000009*   FOR THE YEAR 2000 DATE MODIFICATION                          *
000010*                                                                *
000011******************************************************************
000012 01  WS-DATE-CARD-DATE.
000013     05  WS-RUN-DATE.
000014         10  FILLER                  PIC  9(03).
000015         10  RUN-CCYY                PIC  9(04).
000016         10  RUN-CCYR  REDEFINES  RUN-CCYY.
000017             15  RUN-CC              PIC  99.
000018             15  RUN-YR              PIC  99.
000019         10  RUN-MO                  PIC  99.
000020         10  RUN-DA                  PIC  99.
000021     05  WS-RUN-DATE-N REDEFINES
000022            WS-RUN-DATE              PIC 9(11).
000023     05  WS-EP-DT.
000024         10  FILLER                  PIC  9(03).
000025         10  EP-CCYY                 PIC  9(04).
000026         10  EP-CCYR  REDEFINES  EP-CCYY.
000027             15  EP-CC               PIC  99.
000028             15  EP-YR               PIC  99.
000029         10  EP-MO                   PIC  99.
000030         10  EP-DA                   PIC  99.
000031     05  WS-EP-DT-N REDEFINES
000032            WS-EP-DT                 PIC 9(11).
000033     05  WS-DTE-CONV-DT.
000034         10  FILLER                  PIC  9(03).
000035         10  DTE-CONV-CCYY           PIC  9(04).
000036         10  DTE-CONV-CCYR  REDEFINES  DTE-CONV-CCYY.
000037             15  DTE-CONV-CC         PIC  99.
000038             15  DTE-CONV-YR         PIC  99.
000039         10  DTE-CONV-MO             PIC  99.
000040         10  DTE-CONV-DA             PIC  99.
000041     05  WS-DTE-CONV-DT-N REDEFINES
000042            WS-DTE-CONV-DT           PIC  9(11).
000043 01  WS-BALANCE-FORWARD-DATE.
000044     05  WS-BF-LAST-ACTIVITY-DATE.
000045         10  FILLER                            PIC 999.
000046         10  BF-ACT-CCYY                       PIC 9(04).
000047         10  BF-ACT-CCYR  REDEFINES  BF-ACT-CCYY.
000048             15  BF-ACT-CC                     PIC 99.
000049             15  BF-ACT-YEAR                   PIC 99.
000050         10  BF-ACT-MONTH                      PIC 99.
000051         10  BF-ACT-DAY                        PIC 99.
000052     05  WS-BF-LAST-ACTIVITY-DATE-N REDEFINES
000053            WS-BF-LAST-ACTIVITY-DATE           PIC 9(11).
000054     05  WS-BF-CURRENT-LAST-STMT-DT.
000055         10  FILLER                            PIC 999.
000056         10  BF-CURRENT-LAST-STMT-CEN          PIC 99.
000057         10  BF-CURRENT-LAST-STMT-YEAR         PIC 99.
000058         10  BF-CURRENT-LAST-STMT-MONTH        PIC 99.
000059         10  BF-CURRENT-LAST-STMT-DAY          PIC 99.
000060     05  WS-BF-CURRENT-LAST-STMT-DT-N REDEFINES
000061            WS-BF-CURRENT-LAST-STMT-DT        PIC 9(11).
000062 01  WS-ADD-CONF-TRANSACTIONS-DTE.
000063     05  WS-AC-CERT-EFF-DATE.
000064         10  FILLER                            PIC 999.
000065         10  AC-CERT-CCYY                      PIC 9(04).
000066         10  AC-CERT-CCYR  REDEFINES  AC-CERT-CCYY.
000067             15  AC-CERT-CC                    PIC 99.
000068             15  AC-CERT-YR                    PIC 99.
000069         10  AC-CERT-MO                        PIC 99.
000070         10  AC-CERT-DA                        PIC 99.
000071     05  WS-AC-CERT-EFF-DATE-N REDEFINES
000072            WS-AC-CERT-EFF-DATE                PIC 9(11).
000073     05  WS-AC-LIFE-CANCEL-DATE.
000074         10  FILLER                            PIC 999.
000075         10  AC-LIFE-CANCEL-CCYY               PIC 9(04).
000076         10  AC-LIFE-CANCEL-CCYR REDEFINES AC-LIFE-CANCEL-CCYY.
000077             15  AC-LIFE-CANCEL-CC             PIC 99.
000078             15  AC-LIFE-CANCEL-YR             PIC 99.
000079         10  AC-LIFE-CANCEL-MO                 PIC 99.
000080         10  AC-LIFE-CANCEL-DA                 PIC 99.
000081     05  WS-AC-LIFE-CANCEL-DATE-N REDEFINES
000082            WS-AC-LIFE-CANCEL-DATE             PIC 9(11).
000083     05  WS-AC-AH-CANCEL-DATE.
000084         10  FILLER                            PIC 999.
000085         10  AC-AH-CANCEL-CCYY                 PIC 9(04).
000086         10  AC-AH-CANCEL-CCYR  REDEFINES  AC-AH-CANCEL-CCYY.
000087             15  AC-AH-CANCEL-CC               PIC 99.
000088             15  AC-AH-CANCEL-YR               PIC 99.
000089         10  AC-AH-CANCEL-MO                   PIC 99.
000090         10  AC-AH-CANCEL-DA                   PIC 99.
000091     05  WS-AC-AH-CANCEL-DATE-N REDEFINES
000092            WS-AC-AH-CANCEL-DATE               PIC 9(11).
000093 01  WS-RATE-RECORD.
000094         05  WS-RT-EXPIRY-DATE.
000095             10  FILLER                        PIC 999.
000096             10  RT-EXP-CCYY                   PIC 9(04).
000097             10  RT-EXP-CCYR  REDEFINES  RT-EXP-CCYY.
000098                 15  RT-EXP-CC                 PIC 99.
000099                 15  RT-EXP-YR                 PIC 99.
000100             10  RT-EXP-MO                     PIC 99.
000101             10  RT-EXP-DA                     PIC 99.
000102         05  WS-RT-EXPIRY-DATE-N REDEFINES
000103                WS-RT-EXPIRY-DATE              PIC 9(11).
      *<<((file: ELCDTEVR))
000286*                                COPY ERCCNOT.
      *>>((file: ERCCNOT))
000001******************************************************************
000002*                                                                *
000003*                            ERCCNOT                             *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.003                          *
000006*                                                                *
000007*        FILE DESCRIPTION = CERTIFICATE NOTES                    *
000008*                                                                *
000009*        FILE TYPE= VSAM,KSDS                                    *
000010*        RECORD SIZE = 150    RECFORM = FIXED                    *
000011*                                                                *
000012*        BASE CLUSTER = ERCNOT        RKP=2,LEN=36               *
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
000025* 091509  CR2008100900003  AJRA  NEW FILE FOR CERT NOTES.
000026******************************************************************
000027
000028 01  CERT-NOTE-FILE.
000029     12  CZ-RECORD-ID                PIC  XX.
000030         88  VALID-CZ-ID                  VALUE 'CZ'.
000031
000032     12  CZ-CONTROL-PRIMARY.
000033         16  CZ-COMPANY-CD           PIC X.
000034         16  CZ-CARRIER              PIC X.
000035         16  CZ-GROUPING.
000036             20 CZ-GROUPING-PREFIX   PIC XXX.
000037             20 CZ-GROUPING-PRIME    PIC XXX.
000038         16  CZ-STATE                PIC XX.
000039         16  CZ-ACCOUNT.
000040             20 CZ-ACCOUNT-PREFIX    PIC X(4).
000041             20 CZ-ACCOUNT-PRIME     PIC X(6).
000042         16  CZ-CERT-EFF-DT          PIC XX.
000043         16  CZ-CERT-NO.
000044             20  CZ-CERT-PRIME       PIC X(10).
000045             20  CZ-CERT-SFX         PIC X.
000046         16  CZ-RECORD-TYPE          PIC X.
000047             88  CERT-NOTE           VALUE '1'.
000048             88  CLAIM-CERT-NOTE     VALUE '2'.
000049         16  CZ-NOTE-SEQUENCE        PIC S9(4)     COMP.
000050
000051     12  CZ-LAST-MAINT-DT            PIC XX.
000052     12  CZ-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
000053     12  CZ-LAST-MAINT-USER          PIC X(4).
000054
000055     12  CZ-NOTE-INFORMATION.
000056         16  CZ-NOTE                 PIC X(63).
000057         16  FILLER                  PIC X(39).
000058******************************************************************
      *<<((file: ERCCNOT))
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
000288 01  DFHCOMMAREA.
000289   05 GIVE-TAKE-SOCKET         PIC 9(8) COMP.
000290   05 LSTN-NAME                PIC X(8).
000291   05 LSTN-SUBNAME             PIC X(8).
000292****   client-in-data must be 36 characters.  ****
000293   05 CLIENT-IN-DATA.
000294      15  client-comp-id       pic xxx.
000295      15  client-carrier       pic x.
000296      15  client-state         pic xx.
000297      15  client-account       pic x(10).
000298      15  client-eff-dt        pic 9(08).
000299      15  client-cert-no       pic x(11).
000300      15  filler               pic x.
000301   05 SOCKADDR-IN-PARM.
000302     15 SIN-FAMILY             PIC 9(4) COMP.
000303     15 SIN-PORT               PIC 9(4) COMP.
000304     15 SIN-ADDRESS            PIC 9(8) COMP.
000305     15 SIN-ZERO               PIC X(8).
000306
000307
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'SOCK17' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000308 VCOBOL-DUMMY-PROCEDURE.
000309
000310* when calling a C function the function returns its value
000311* in the system variable return code.
000312*
000313     perform 0000-INITIALIZE     thru 0000-exit
000314
000315     if resp-normal
000316        PERFORM 0050-PROCESS-INPUT
000317                                 THRU 0050-EXIT
000318     else
000319        display ' certificate   not found ' client-carrier ' '
000320        client-state ' ' client-account ' ' client-cert-no
000321        move ' issue rec not found '
000322                                 to ws-return-stuff
000323     end-if
000324
000325     perform 0200-send-buffer    thru 0200-exit
000326     perform 0300-close-socket   thru 0300-exit
000327     
      * exec cics return end-exec.
      *    MOVE '.(                    ''   #00005230' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303035323330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000328
000329     
      * GOBACK

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK17' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK
000330     .
000331 0000-INITIALIZE.
000332
000333     
      * exec cics
000334*       asktime
000335*    end-exec
      *    MOVE '0"                    "   #00005236' TO DFHEIV0
           MOVE X'302220202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303035323336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000336
000337     MOVE EIBDATE                TO DC-JULIAN-YYDDD
000338     MOVE '5'                    TO DC-OPTION-CODE
000339     perform 9700-date-convert   thru 9700-exit
000340
000341     IF DATE-CONVERSION-ERROR
000342        display ' error converting eibdate '
000343        MOVE LOW-VALUES          TO save-bin-date
000344     ELSE
000345        MOVE DC-BIN-DATE-1       TO save-bin-date
000346                                    ws-current-bin-dt
000347     end-if
000348
000349     move client-comp-id         to ws-comp-id
000350     evaluate true
000351        when ws-comp-id = 'AHL'
000352           MOVE X'06'            TO ws-comp-cd
000353        when ws-comp-id = 'DCC'
000354           move X'05'            to ws-comp-cd
000355        when other
000356           move X'04'            to ws-comp-cd
000357     end-evaluate
000358     move spaces                 to ws-alpha-table
000359
000360     PERFORM 0040-READ-ELCERT    THRU 0040-EXIT
000361     if resp-normal
000362        perform 0045-get-ben-codes
000363                                 thru 0045-exit
000364     end-if
000365
000366     .
000367 0000-EXIT.
000368     EXIT.
000369
000370 0040-READ-ELCERT.
000371
000372     move ws-comp-cd             to ws-elcert-company-cd
000373     move client-carrier         to ws-elcert-carrier
000374     move '000000'               to ws-elcert-grouping
000375     move client-state           to ws-elcert-state
000376     move client-account         to ws-elcert-account
000377     move client-eff-dt          to dc-greg-date-cymd
000378     move 'L'                    to dc-option-code
000379     perform 9700-DATE-CONVERT   thru 9700-exit
000380     if no-conversion-error
000381        move dc-bin-date-1       to ws-elcert-eff-dt
000382     end-if
000383     move client-cert-no         to ws-elcert-cert-no
000384
000385     
      * exec cics read
000386*       dataset     ('ELCERT')
000387*       into        (certificate-master)
000388*       ridfld      (ws-elcert-key)
000389*       resp        (ws-response)
000390*    end-exec
           MOVE LENGTH OF
            certificate-master
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005288' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303035323838' TO DFHEIV0
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
000391
000392     .
000393 0040-EXIT.
000394     EXIT.
000395
000396 0045-get-ben-codes.
000397
000398     if cm-lf-benefit-cd not = '00' and spaces
000399        move cm-lf-benefit-cd    to ws-lf-ben-code
000400        perform 0400-get-lf-code thru 0400-exit
000401        if c1 < +9
000402           move cf-joint-indicator (c1)
000403                                 to lf-joint-ind
000404        end-if
000405     end-if
000406     if cm-ah-benefit-cd not = '00' and spaces
000407        move cm-ah-benefit-cd    to ws-ah-ben-code
000408        perform 0410-get-ah-code thru 0410-exit
000409        if c1 < +9
000410           move cf-joint-indicator (c1)
000411                                 to ah-joint-ind
000412        end-if
000413     end-if
000414
000415     .
000416 0045-exit.
000417     exit.
000418
000419 0050-PROCESS-INPUT.
000420
000421     move cm-control-primary     to ws-elcert-orig-key
000422     perform 0100-check-cancel   thru 0100-exit
000423
000424     if ((cm-lf-benefit-cd <> spaces and zeros)
000425        or (cm-ah-benefit-cd <> spaces and zeros))
000426        and (cm-entry-status <> '5' and '9' and 'D' and 'V')
000427        display ' must not be cancelled '
000428        move +0                  to a1
000429        move spaces              to ws-alpha-table
000430                                    ws-return-stuff
000431        move 'P'                 to ws-cov-sw
000432        move cm-insured-last-name
000433                                 to ws-name-in
000434        perform 0650-set-last-name
000435                                 thru 0650-exit
000436        move ws-name-out         to ws-last-name
000437*       display ' ws last name ' ws-last-name
000438        move cm-insured-first-name (1:3)
000439                                 to ws-first-three
000440*       display ' ws 1st 3 ' ws-first-three
000441        perform 0700-calc-cur-cm-ages
000442                                 thru 0700-exit
000443        move ws-cm-pri-curr-age  to ws-age
000444*       display ' pri age ' ws-age
000445        perform 0150-process-continue
000446                                 thru 0150-exit
000447        if ((lf-joint-ind = 'J')
000448           or (ah-joint-ind = 'J'))
000449                   and
000450              (cm-jt-last-name not = spaces)
000451           move ws-hold-elcert   to certificate-master
000452           move ' '              to WS-ERPNDB5-SW
000453           move 'S'              to ws-cov-sw
000454           move cm-jt-last-name  to ws-name-in
000455           perform 0650-set-last-name
000456                                 thru 0650-exit
000457           move ws-name-out      to ws-last-name
000458           move cm-jt-first-name (1:3)
000459                                 to ws-first-three
000460           move ws-cm-cob-curr-age
000461                                 to ws-age
000462*          display ' COB age ' ws-age
000463           perform 0150-process-continue
000464                                 thru 0150-exit
000465        end-if
000466     else
000467        display ' certificate cancelled ' client-carrier ' '
000468        client-state ' ' client-account ' ' client-cert-no
000469        move ' certificate cancelled '
000470                                 to ws-return-stuff
000471
000472     end-if
000473
000474     .
000475 0050-EXIT.
000476     EXIT.
000477
000478 0100-check-cancel.
000479
000480*** check to see if the certificate   is cancelled
000481
000482     move certificate-master     to ws-hold-elcert
000483     if cm-lf-benefit-cd <> '00' and '  '
000484        if cm-lf-cancel-dt <> low-values
000485           move '00'             to cm-lf-benefit-cd
000486        end-if
000487     end-if
000488     if cm-ah-benefit-cd <> '00' and '  '
000489        if cm-ah-cancel-dt <> low-values
000490           move '00'             to cm-ah-benefit-cd
000491        end-if
000492     end-if
000493
000494     .
000495 0100-exit.
000496     exit.
000497
000498 0150-process-continue.
000499
000500****  First, move elcert  record to first occurance of table
000501
000502     move ' '                    to ws-match-sw
000503
000504     move low-values             to ws-eralph-aix-key
000505     move cm-company-cd          TO ws-eralph-aix-company-cd
000506     MOVE cm-ACCOUNT             TO ws-eralph-aix-account
000507     MOVE ws-last-name           to ws-eralph-aix-lname
000508     move ws-first-three         to ws-eralph-aix-1st-three
000509
000510     add +1                      to a1
000511     move ws-cov-sw              to ws-cov-type (a1)
000512     if cm-lf-policy-pending or
000513        cm-ah-policy-pending
000514        move 'Y'                 to ws-pend (a1)
000515     end-if
000516     move ws-age                 to ws-eralph-age (a1)
000517     if processing-primary
000518        move cm-insured-first-name
000519                                 to ws-first-name (a1)
000520     else
000521        move cm-jt-first-name    to ws-first-name (a1)
000522     end-if
000523     move ws-last-name           to ws-tbl-last-name (a1)
000524     move cm-cert-eff-dt         to dc-bin-date-1
000525     move ' '                    to dc-option-code
000526     perform 9700-date-convert   thru 9700-exit
000527     if no-conversion-error
000528        move dc-greg-date-a-edit to ws-eff-dt (a1)
000529     end-if
000530     move cm-cert-no             to ws-eralph-cert (a1)
000531
000532     move cm-lf-benefit-cd       to ws-eralph-lf-cd (a1)
000533
000534     if ((ws-cov-sw = 'P')
000535              or
000536        ((ws-cov-sw = 'S')
000537         and (lf-joint-ind = 'J')))
000538         and (cm-lf-benefit-cd <> '00' and '  ')
000539        perform 0500-get-lf-rem  thru 0500-exit
000540        move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
000541        move ws-eralph-lf-remamt (a1)
000542                                 to ws-lf-tot-benefit
000543        move cp-remaining-term-2 to ws-eralph-lf-remterm (a1)
000544     else
000545        move zeros               to ws-eralph-lf-remamt (a1)
000546                                    ws-eralph-lf-remterm (a1)
000547     end-if
000548
000549     if cm-lf-loan-expire-dt = low-values or spaces
000550        move spaces              to ws-lf-exp-dt (a1)
000551     else
000552        move cm-lf-loan-expire-dt to dc-bin-date-1
000553        move ' '                 to dc-option-code
000554        perform 9700-date-convert thru 9700-exit
000555        if no-conversion-error
000556           move dc-greg-date-a-edit
000557                                 to ws-lf-exp-dt (a1)
000558        end-if
000559     end-if
000560
000561     move cm-ah-benefit-cd       to ws-eralph-ah-cd (a1)
000562
000563     if ((ws-cov-sw = 'P')
000564              or
000565        ((ws-cov-sw = 'S')
000566         and (ah-joint-ind = 'J')))
000567         and (cm-ah-benefit-cd <> '00' and '  ')
000568        perform 0510-get-ah-rem  thru 0510-exit
000569        move cp-remaining-amt    to ws-eralph-ah-remamt (a1)
000570        move cp-remaining-term-2 to ws-eralph-ah-remterm (a1)
000571        move cm-ah-benefit-amt   to ws-eralph-ah-amt (a1)
000572                                    ws-ah-tot-benefit
000573     else
000574        move zeros               to ws-eralph-ah-amt (a1)
000575                                    ws-eralph-ah-remamt (a1)
000576                                    ws-eralph-ah-remterm (a1)
000577     end-if
000578
000579     if cm-ah-loan-expire-dt = low-values or spaces
000580        move spaces              to ws-ah-exp-dt (a1)
000581     else
000582        move cm-ah-loan-expire-dt to dc-bin-date-1
000583        move ' '                 to dc-option-code
000584        perform 9700-date-convert thru 9700-exit
000585        if no-conversion-error
000586           move dc-greg-date-a-edit
000587                                 to ws-ah-exp-dt (a1)
000588        end-if
000589     end-if
000590
000591     move ';'                    to ws-delimiter (a1)
000592
000593     perform 0170-ERpndb5        thru 0170-exit
000594
000595     if erpndb2-startbr
000596        
      * exec cics endbr
000597*          dataset     ('ELCERT')
000598*       end-exec
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005499' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035343939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000599     end-if
000600
000601     compute p1 = a1 + 1
000602
000603     move spaces                 to ws-eralph2-startbr-sw
000604                                    ws-eralph-sw
000605
000606     
      * exec cics startbr
000607*       dataset     ('ERALPH2')
000608*       ridfld      (ws-eralph-aix-key)
000609*       gteq
000610*       resp        (ws-response)
000611*    end-exec
           MOVE 'ERALPH2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005509' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303035353039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-eralph-aix-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000612
000613     if (resp-endfile)
000614        or (resp-notfnd)
000615*       display ' no matching eralph record '
000616        set no-matching-alpha to true
000617        go to 0150-exit
000618     else
000619        if not resp-normal
000620           display ' error-eralph-start ' ws-response
000621           set no-matching-alpha to true
000622           go to 0150-exit
000623        end-if
000624     end-if
000625
000626     set eralph2-startbr to true
000627     perform 0155-read-eralph    thru 0155-exit
000628     if (not resp-normal)
000629        and (not resp-dupkey)
000630        display ' Error-eralph-1stread ' ws-response
000631        go to 0150-exit
000632     end-if
000633
000634     perform 0160-accum-eralph   thru 0160-exit until
000635        (af-company-cd-a1  not = ws-comp-cd)
000636        or (af-account-a1  not = client-account)
000637        or (ws-alpha-last-name not = ws-last-name)
000638        or (af-fname (1:3) not = ws-first-three)
000639        or (end-of-eralph)
000640        or ((not resp-normal) and (not resp-dupkey))
000641
000642     if eralph2-startbr
000643        
      * exec cics endbr
000644*          dataset     ('ERALPH2')
000645*       end-exec
           MOVE 'ERALPH2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005546' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035353436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000646     end-if
000647
000648     if a1 = +1
000649        perform 0166-check-for-ah-claim
000650                                 thru 0166-exit
000651     end-if
000652
000653     if a1 > +1
000654        perform varying t1 from +1 by +1 until t1 > +20
000655           move ws-eralph-cert (t1)
000656                                 to pt-eralph-cert (t1)
000657           move ws-eralph-lf-cd (t1)
000658                                 to pt-eralph-lf-cd (t1)
000659           move ws-eralph-lf-remamt (t1)
000660                                 to pt-eralph-lf-remamt (t1)
000661           move ws-eralph-ah-cd (t1)
000662                                 to pt-eralph-ah-cd (t1)
000663           move ws-eralph-ah-amt (t1)
000664                                 to pt-eralph-ah-amt (t1)
000665           move ws-eralph-ah-remamt (t1)
000666                                 to pt-eralph-ah-remamt (t1)
000667*          display ws-cov-type (t1) ' '
000668*             pt-eralph-cert      (t1) '   '
000669*             pt-eralph-lf-cd     (t1) '   '
000670*             pt-eralph-lf-remamt (t1) '    A&H   '
000671*             pt-eralph-ah-cd     (t1) '   '
000672*             pt-eralph-ah-amt    (t1) '   '
000673*             pt-eralph-ah-remamt (t1) '   '
000674        end-perform
000675     end-if
000676
000677     .
000678 0150-exit.
000679     exit.
000680
000681 0155-READ-ERALPH.
000682
000683     
      * exec cics readnext
000684*       dataset      ('ERALPH2')
000685*       ridfld       (ws-eralph-aix-key)
000686*       into         (alpha-file-rec)
000687*       resp         (ws-response)
000688*    end-exec
           MOVE LENGTH OF
            alpha-file-rec
             TO DFHEIV12
           MOVE 'ERALPH2' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005586' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303035353836' TO DFHEIV0
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
000689
000690     if (not resp-normal)
000691        and (not resp-dupkey)
000692        display 'setting end-of-eralph to true '
000693        set end-of-eralph to true
000694        go to 0155-exit
000695     end-if
000696
000697     if (ws-comp-cd = ws-eralph-aix-company-cd)
000698        and (client-account = ws-eralph-aix-account)
000699        continue
000700     else
000701        set end-of-eralph to true
000702        go to 0155-exit
000703     end-if
000704
000705     move af-lname               to ws-name-in
000706     perform 0650-set-last-name  thru 0650-exit
000707     move ws-name-out            to ws-alpha-last-name
000708
000709
000710     evaluate true
000711        when (ws-last-name = ws-alpha-last-name)
000712           and (ws-first-three not = ws-eralph-aix-1st-three)
000713           go to 0155-read-eralph
000714        when (ws-last-name not = ws-alpha-last-name)
000715           set end-of-eralph to true
000716        when other
000717           continue
000718     end-evaluate
000719
000720     .
000721 0155-EXIT.
000722     EXIT.
000723
000724 0160-accum-eralph.
000725
000726***** calc the current age on the alpha record here
000727***** then compare it to the pb ages, if there not within
000728***** a few years then go to 0160-continue.
000729
000730*** check to see if there is a pending cancel for the alpha rec **
000731*** if not, then accumulate alpha's in table.
000732
000733     if ws-elcert-orig-key = af-control-primary(1:33)
000734        go to 0160-continue
000735     end-if
000736
000737     move af-control-primary     to ws-elcert-key
000738     perform 0600-get-elcert     thru 0600-exit
000739
000740     if resp-normal
000741        if cm-lf-benefit-cd not = '00' and '  '
000742           if cm-lf-cancel-dt <> low-values
000743              move '00'             to af-lf-typ
000744           end-if
000745        end-if
000746        if cm-ah-benefit-cd not = '00' and '  '
000747           if cm-ah-cancel-dt <> low-values
000748              move '00'             to af-ah-typ
000749           end-if
000750        end-if
000751     end-if
000752
000753     if (af-lf-typ = '00' or spaces)
000754        and (af-ah-typ = '00' or spaces)
000755        go to 0160-continue
000756     end-if
000757
000758     perform 0720-calc-cur-alph-age
000759                                 thru 0720-exit
000760
000761     if (ws-age - ws-alph-curr-age < 4
000762        and >= 0)
000763                      or
000764        (ws-alph-curr-age - ws-age < 4
000765        and >= 0)
000766        add +1 to a1
000767     else
000768        go to 0160-continue
000769     end-if
000770
000771     move zeros to ws-eralph-lf-remamt (a1)
000772                   ws-eralph-lf-remterm (a1)
000773                   ws-eralph-ah-amt (a1)
000774                   ws-eralph-ah-remamt (a1)
000775                   ws-eralph-ah-remterm (a1)
000776
000777     move ' '                    to ws-pend (a1)
000778     move af-fname               to ws-first-name (a1)
000779     move ws-alph-curr-age       to ws-eralph-age (a1)
000780     move ws-last-name           to ws-tbl-last-name (a1)
000781     if af-alpha-type-code = 'I'
000782        move 'P'                 to ws-eralph-prm-sec (a1)
000783     else
000784        move 'C'                 to ws-eralph-prm-sec (a1)
000785     end-if
000786     move af-dt                  to dc-bin-date-1
000787     move ' '                    to dc-option-code
000788     perform 9700-date-convert   thru 9700-exit
000789     if no-conversion-error
000790        move dc-greg-date-a-edit to ws-eff-dt (a1)
000791     end-if
000792
000793     if af-lf-typ not = '00' and '  '
000794        move ws-cov-sw           to ws-cov-type (a1)
000795        move af-cert-no          to ws-eralph-cert (a1)
000796        move af-lf-typ           to ws-eralph-lf-cd (a1)
000797        perform 0500-get-lf-rem  thru 0500-exit
000798        move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
000799        compute ws-lf-tot-benefit =
000800           ws-lf-tot-benefit + cp-remaining-amt
000801        move cp-remaining-term-3 to ws-eralph-lf-remterm (a1)
000802
000803        compute ws-lf-tot-benefit =
000804           ws-lf-tot-benefit + af-lf-remamt + af-lf-remamt-alt
000805        move af-lf-expires       to dc-bin-date-1
000806        move ' '                 to dc-option-code
000807        perform 9700-date-convert thru 9700-exit
000808        if no-conversion-error
000809           move dc-greg-date-a-edit
000810                                 to ws-lf-exp-dt (a1)
000811        end-if
000812        move ';'                 to ws-delimiter (a1)
000813     end-if
000814
000815     if af-ah-typ not = '00' and '  '
000816        move ws-cov-sw           to ws-cov-type (a1)
000817        move af-cert-no          to ws-eralph-cert (a1)
000818        move af-ah-typ           to ws-eralph-ah-cd (a1)
000819        if af-ah-status not = '8'
000820           move af-ah-amt        to ws-eralph-ah-amt (a1)
000821           perform 0510-get-ah-rem
000822                                 thru 0510-exit
000823           move cp-remaining-amt to ws-eralph-ah-remamt (a1)
000824           compute ws-ah-tot-benefit =
000825              ws-ah-tot-benefit + af-ah-amt
000826           move cp-remaining-term-3 to ws-eralph-ah-remterm (a1)
000827           move af-ah-expires    to dc-bin-date-1
000828           move ' '              to dc-option-code
000829           perform 9700-date-convert
000830                                 thru 9700-exit
000831           if no-conversion-error
000832              move dc-greg-date-a-edit
000833                                 to ws-ah-exp-dt (a1)
000834           end-if
000835        end-if
000836        move ';'                 to ws-delimiter (a1)
000837        perform 0165-check-for-ah-claim
000838                                 thru 0165-exit
000839     end-if
000840
000841     .
000842 0160-continue.
000843
000844     perform 0155-read-eralph    thru 0155-exit
000845
000846     .
000847 0160-exit.
000848     exit.
000849
000850 0165-check-for-ah-claim.
000851
000852     move ws-comp-cd             to ws-elmstr5-company-cd
000853     move af-cert-no             to ws-elmstr5-cert-no
000854     move spaces                 to ws-elmstr5-startbr-sw
000855     
      * exec cics startbr
000856*       dataset         ('ELMSTR5')
000857*       ridfld          (ws-elmstr5-key)
000858*       gteq
000859*       resp            (ws-response)
000860*    end-exec
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005758' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303035373538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-elmstr5-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000861
000862     if resp-normal
000863        set elmstr5-startbr to true
000864     else
000865        if (resp-notfnd)
000866           or (resp-endfile)
000867           go to 0165-exit
000868        else
000869           display 'error-elmstr5-startbr '
000870           ws-response ' ' af-cert-no
000871        end-if
000872     end-if
000873
000874     .
000875 0165-read-next.
000876
000877     
      * exec cics readnext
000878*       dataset     ('ELMSTR5')
000879*       ridfld      (ws-elmstr5-key)
000880*       into        (claim-master)
000881*       resp        (ws-response)
000882*    end-exec
           MOVE LENGTH OF
            claim-master
             TO DFHEIV12
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005780' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303035373830' TO DFHEIV0
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
000883
000884     if resp-normal or resp-dupkey
000885        continue
000886     else
000887        display ' error-elmstr5-readnext ' ws-response ' '
000888           af-cert-no
000889        go to 0165-endbr
000890     end-if
000891
000892     if (cl-company-cd-a4 not = af-company-cd)
000893        or (cl-cert-no-a4 not = af-cert-no)
000894        go to 0165-endbr
000895     end-if
000896
000897     if (cl-cert-carrier = af-carrier)
000898        and (cl-cert-grouping = af-grouping)
000899        and (cl-cert-state    = af-state)
000900        and (cl-cert-account  = af-account)
000901        and (cl-cert-eff-dt   = af-dt)
000902        and (cl-cert-no-a4    = af-cert-no)
000903*       move cl-incurred-dt      to dc-bin-date-1
000904*       move pb-cert-eff-dt      to dc-bin-date-2
000905*       move '1'                 to dc-option-code
000906*       perform 9700-date-convert thru 9700-exit
000907*       if (no-conversion-error)
000908*          and (dc-elapsed-months > 11)
000909*          display ' Found A & H claim ' cl-cert-no-a4 ' '
000910*             cl-claim-status ' ' cl-total-paid-amt
000911           move 'Y'              to ws-claim (a1)
000912*       end-if
000913     end-if
000914
000915     go to 0165-read-next
000916
000917     .
000918 0165-endbr.
000919
000920     if elmstr5-startbr
000921        
      * exec cics endbr
000922*          dataset     ('ELMSTR5')
000923*       end-exec
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005824' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035383234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000924     end-if
000925
000926     .
000927 0165-exit.
000928     exit.
000929
000930 0166-check-for-ah-claim.
000931
000932     move ws-elcert-orig-key to ws-elcert-key
000933
000934     move ws-comp-cd             to ws-elmstr5-company-cd
000935     move ws-elcert-cert-no      to ws-elmstr5-cert-no
000936     move spaces                 to ws-elmstr5-startbr-sw
000937     
      * exec cics startbr
000938*       dataset         ('ELMSTR5')
000939*       ridfld          (ws-elmstr5-key)
000940*       gteq
000941*       resp            (ws-response)
000942*    end-exec
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005840' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303035383430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-elmstr5-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000943
000944     if resp-normal
000945        set elmstr5-startbr to true
000946     else
000947        if (resp-notfnd)
000948           or (resp-endfile)
000949           go to 0166-exit
000950        else
000951           display 'error-elmstr5-startbr '
000952           ws-response ' ' ws-elcert-cert-no
000953        end-if
000954     end-if
000955
000956     .
000957 0166-read-next.
000958
000959     
      * exec cics readnext
000960*       dataset     ('ELMSTR5')
000961*       ridfld      (ws-elmstr5-key)
000962*       into        (claim-master)
000963*       resp        (ws-response)
000964*    end-exec
           MOVE LENGTH OF
            claim-master
             TO DFHEIV12
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005862' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303035383632' TO DFHEIV0
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
000965
000966     if resp-normal or resp-dupkey
000967        continue
000968     else
000969        display ' error-elmstr5-readnext ' ws-response ' '
000970           cm-cert-no
000971        go to 0166-endbr
000972     end-if
000973
000974     if (cl-company-cd-a4 not = ws-elcert-company-cd)
000975        or (cl-cert-no-a4 not = ws-elcert-cert-no)
000976        go to 0166-endbr
000977     end-if
000978
000979     if (cl-cert-carrier = ws-elcert-carrier)
000980        and (cl-cert-grouping = ws-elcert-grouping)
000981        and (cl-cert-state    = ws-elcert-state)
000982        and (cl-cert-account  = ws-elcert-account)
000983        and (cl-cert-eff-dt   = ws-elcert-eff-dt)
000984        and (cl-cert-no-a4    = ws-elcert-cert-no)
000985*       move cl-incurred-dt      to dc-bin-date-1
000986*       move pb-cert-eff-dt      to dc-bin-date-2
000987*       move '1'                 to dc-option-code
000988*       perform 9700-date-convert thru 9700-exit
000989*       if (no-conversion-error)
000990*          and (dc-elapsed-months > 11)
000991*          display ' Found A & H claim ' cl-cert-no-a4 ' '
000992*             cl-claim-status ' ' cl-total-paid-amt
000993           move 'Y'              to ws-claim (a1)
000994*       end-if
000995     end-if
000996
000997     go to 0166-read-next
000998
000999     .
001000 0166-endbr.
001001
001002     if elmstr5-startbr
001003        
      * exec cics endbr
001004*          dataset     ('ELMSTR5')
001005*       end-exec
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005906' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035393036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001006     end-if
001007
001008     .
001009 0166-exit.
001010     exit.
001011
001012 0170-erpndb5.
001013
001014***  this routine reads all the elcert  records that
001015***  match the acct#, last name & 1st init
001016***  and if it's not cancelled add to the accum table
001017
001018     move spaces                 to ws-erpndb2-startbr-sw
001019     move low-values             to ws-elcert-eff-dt
001020                                    ws-elcert-cert-no
001021
001022     
      * exec cics startbr
001023*       dataset      ('ELCERT')
001024*       ridfld       (ws-elcert-key)
001025*       gteq
001026*       resp         (ws-response)
001027*    end-exec
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00005925' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303035393235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-elcert-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001028
001029     if not resp-normal
001030        display ' error-erpndb5-start ' ws-response ' '
001031           ws-elcert-key (2:19)
001032        go to 0170-exit
001033     end-if
001034     set erpndb2-startbr to true
001035     perform 0175-read-erpndb5   thru 0175-exit
001036     if not resp-normal
001037        display ' Error-erpndb5-1stread '
001038           ws-passed-pend-key (2:19) ' '
001039           ws-passed-pend-key (22:11)
001040        go to 0170-exit
001041     end-if
001042     perform 0180-accum-erpndb5  thru 0180-exit until
001043        (ws-elcert-key(1:20) <> ws-elcert-orig-key(1:20))
001044        or (end-of-erpndb5)
001045        or (not resp-normal)
001046*    end-perform
001047
001048     .
001049 0170-exit.
001050     exit.
001051
001052 0175-READ-ERPNDB5.
001053
001054     
      * exec cics readnext
001055*       dataset    ('ELCERT')
001056*       ridfld     (ws-elcert-key)
001057*       into       (certificate-master)
001058*       resp       (ws-response)
001059*    end-exec
           MOVE LENGTH OF
            certificate-master
             TO DFHEIV12
           MOVE 'ELCERT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00005957' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303035393537' TO DFHEIV0
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
001060
001061     if (resp-endfile)
001062        or (resp-notfnd)
001063        or (ws-elcert-key(1:20) <> ws-elcert-orig-key(1:20))
001064*       or (ws-last-name <> ws-elcert-last-name)
001065*       display ' end of file on read next '
001066        set end-of-erpndb5       to true
001067     else
001068        if not resp-normal
001069           display ' error-erpndb5-readnext ' ws-response
001070              ' ' ws-erpndb5-key (2:19)
001071           set end-of-erpndb5    to true
001072        end-if
001073     end-if
001074
001075     if not end-of-erpndb5
001076*       display ' not end of erpndb5 '
001077        if ws-cov-sw = 'P'
001078           move cm-insured-last-name
001079                                 to ws-name-in
001080           perform 0650-set-last-name
001081                                 thru 0650-exit
001082           move ws-name-out      to ws-elcert-last-name
001083        else
001084           move cm-jt-last-name  to ws-name-in
001085           perform 0650-set-last-name
001086                                 thru 0650-exit
001087           move ws-name-out      to ws-elcert-last-name
001088        end-if
001089     end-if
001090
001091     .
001092 0175-EXIT.
001093     EXIT.
001094
001095 0180-accum-erpndb5.
001096*    display ' made it to 0180- '
001097     if ws-elcert-key = ws-elcert-orig-key
001098        go to 0180-read  *> I found myself
001099     end-if
001100
001101     if ws-last-name <> ws-elcert-last-name
001102        go to 0180-read
001103     end-if
001104
001105     if cm-lf-policy-pending or
001106        cm-ah-policy-pending
001107        continue
001108     else
001109        go to 0180-read  *> Will be on elaph
001110     end-if
001111
001112     if ((ws-cov-sw = 'P')
001113        and (cm-insured-first-name(1:3) = ws-first-three))
001114                      or
001115        ((ws-cov-sw = 'S')
001116        and (cm-jt-first-name(1:3) = ws-first-three))
001117        continue
001118     else
001119        go to 0180-read
001120     end-if
001121
001122     move spaces                 to ws-p5-last-name
001123                                    ws-p5-jnt-last-name
001124     move zeros                  to ws-p5-pri-curr-age
001125                                    ws-p5-cob-curr-age
001126
001127     move ' '                    to ws-pend-match
001128
001129     if cm-lf-benefit-cd not = '00' and spaces
001130        move cm-lf-benefit-cd    to ws-lf-ben-code
001131        perform 0400-get-lf-code thru 0400-exit
001132     end-if
001133     if cm-ah-benefit-cd not = '00' and spaces
001134        move cm-ah-benefit-cd    to ws-ah-ben-code
001135        perform 0410-get-ah-code thru 0410-exit
001136     end-if
001137
001138     if (cm-lf-benefit-cd <> '00' and spaces)
001139        and (cm-lf-cancel-dt <> low-values)
001140        move '00'                to cm-lf-benefit-cd
001141     end-if
001142     if (cm-ah-benefit-cd <> '00' and spaces)
001143        and (cm-ah-cancel-dt <> low-values)
001144        move '00'                to cm-ah-benefit-cd
001145     end-if
001146     if ((cm-lf-benefit-cd = '00' or spaces)
001147        and (cm-ah-benefit-cd = '00' or spaces))
001148                 or
001149        (cm-entry-status = '5' or '9' or
001150                          'D' or 'V')
001151        go to 0180-read   *>  Must be cancelled or something
001152     end-if
001153
001154     move cm-insured-last-name to ws-name-in
001155     perform 0650-set-last-name  thru 0650-exit
001156     move ws-name-out            to ws-p5-last-name
001157     move cm-jt-last-name   to ws-name-in
001158     perform 0650-set-last-name  thru 0650-exit
001159     move ws-name-out            to ws-p5-jnt-last-name
001160
001161     perform 0710-calc-cur-p5-ages
001162                              thru 0710-exit
001163
001164     if ws-cov-sw = 'P'
001165        if (ws-age - ws-p5-pri-curr-age < 4 and > -4)
001166           add +1 to a1
001167           set pend-match to true
001168           move 'Y'           to ws-pend (a1)
001169           move 'P'           to ws-eralph-prm-sec (a1)
001170           move ws-p5-pri-curr-age
001171                              to ws-eralph-age (a1)
001172           move cm-insured-first-name
001173                              to ws-first-name (a1)
001174           perform 0185-build-common
001175                              thru 0185-exit
001176        end-if
001177     end-if
001178
001179     if ws-cov-sw = 'S'
001180        if (ws-age - ws-p5-cob-curr-age < 4 and > -4)
001181           add +1 to a1
001182           set pend-match to true
001183           move 'Y'           to ws-pend (a1)
001184           move 'C'           to ws-eralph-prm-sec (a1)
001185           move ws-p5-cob-curr-age
001186                              to ws-eralph-age (a1)
001187           move cm-jt-first-name
001188                              to ws-first-name (a1)
001189           perform 0185-build-common
001190                              thru 0185-exit
001191        end-if
001192     end-if
001193
001194     .
001195 0180-read.
001196
001197     perform 0175-read-erpndb5   thru 0175-exit
001198
001199     .
001200 0180-exit.
001201     exit.
001202
001203 0185-build-common.
001204
001205     move ws-cov-sw              to ws-cov-type (a1)
001206     move ws-last-name           to ws-tbl-last-name (a1)
001207     move cm-cert-eff-dt         to dc-bin-date-1
001208     move ' '                    to dc-option-code
001209     perform 9700-date-convert   thru 9700-exit
001210     if no-conversion-error
001211        move dc-greg-date-a-edit to ws-eff-dt (a1)
001212     end-if
001213     move cm-cert-no             to ws-eralph-cert  (a1)
001214
001215     move cm-lf-benefit-cd     to ws-eralph-lf-cd (a1)
001216
001217     if ((ws-cov-sw = 'P')
001218                or
001219        ((ws-cov-sw = 'S')
001220         and (lf-joint-ind = 'J')))
001221         and (cm-lf-benefit-cd <> '00' and '  ')
001222        perform 0500-get-lf-rem  thru 0500-exit
001223        move cp-remaining-amt    to ws-eralph-lf-remamt (a1)
001224        compute ws-lf-tot-benefit =
001225           ws-lf-tot-benefit + cp-remaining-amt
001226        move cp-remaining-term-3 to ws-eralph-lf-remterm (a1)
001227     else
001228        move zeros               to ws-eralph-lf-remamt (a1)
001229                                    ws-eralph-lf-remterm (a1)
001230     end-if
001231
001232     if cm-lf-loan-expire-dt = low-values or spaces
001233        move spaces              to ws-lf-exp-dt (a1)
001234     else
001235        move cm-lf-loan-expire-dt   to dc-bin-date-1
001236        move ' '                 to dc-option-code
001237        perform 9700-date-convert thru 9700-exit
001238        if no-conversion-error
001239           move dc-greg-date-a-edit
001240                                 to ws-lf-exp-dt (a1)
001241        end-if
001242     end-if
001243
001244     move cm-ah-benefit-cd     to ws-eralph-ah-cd (a1)
001245     if ((ws-cov-sw = 'P')
001246                or
001247        ((ws-cov-sw = 'S')
001248         and (ah-joint-ind = 'J')))
001249         and (cm-ah-benefit-cd <> '00' and '  ')
001250           perform 0510-get-ah-rem
001251                                 thru 0510-exit
001252           move cp-remaining-amt to ws-eralph-ah-remamt (a1)
001253           compute ws-ah-tot-benefit =
001254              ws-ah-tot-benefit + cm-ah-benefit-amt
001255           move cp-remaining-term-3 to ws-eralph-ah-remterm (a1)
001256        move cm-ah-benefit-amt to ws-eralph-ah-amt (a1)
001257     else
001258        move zeros               to ws-eralph-ah-amt (a1)
001259                                    ws-eralph-ah-remamt (a1)
001260                                    ws-eralph-ah-remterm (a1)
001261     end-if
001262     if cm-ah-loan-expire-dt = low-values or spaces
001263        move spaces              to ws-ah-exp-dt (a1)
001264     else
001265        move cm-ah-loan-expire-dt   to dc-bin-date-1
001266        move ' '                 to dc-option-code
001267        perform 9700-date-convert thru 9700-exit
001268        if no-conversion-error
001269           move dc-greg-date-a-edit
001270                                 to ws-ah-exp-dt (a1)
001271        end-if
001272     end-if
001273
001274     move ';'                    to ws-delimiter (a1)
001275
001276     .
001277 0185-exit.
001278     exit.
001279
001280 0200-send-buffer.
001281
001282     move ws-return-stuff        to ws-send-buf
001283*    display 'SOCK06:About to send      '
001284*    display 'SOCK06:sequence number  =', ws-seq-num.
001285*    display 'SOCK06:send buffer      =', ws-send-buf(1:80).
001286
001287     call "send" using by value GIVE-TAKE-SOCKET,
001288         by reference ws-send-buf,
001289         by value ws-send-msg-size,
001290         by value ws-flags.
001291
001292     if return-code <= zero
001293        display 'SOCK06:send error ',
001294        go to 0200-socket-error
001295     end-if
001296     go to 0200-exit
001297
001298     .
001299 0200-socket-error.
001300
001301     if ws-seq-num <> 0
001302        display "SOCK06:did not complete"
001303     end-if
001304
001305     .
001306 0200-exit.
001307     exit.
001308
001309 0300-close-socket.
001310
001311*    display 'SOCK06:closing socket'.
001312*    call "close" using by value GIVE-TAKE-SOCKET .
001313*    display 'SOCK06:done'
001314
001315     .
001316 0300-exit.
001317     exit.
001318
001319 0400-get-lf-code.
001320
001321     move ws-comp-id             to ws-elcntl-key
001322     set ws-elcntl-lf-ben-cd     to true
001323     move ws-lf-ben-code         to ws-elcntl-hi-ben-cd
001324     move zeros                  to ws-elcntl-seq-no
001325
001326     
      * exec cics read
001327*       dataset     ('ELCNTL')
001328*       into        (control-file)
001329*       ridfld      (ws-elcntl-key)
001330*       gteq
001331*       resp        (ws-response)
001332*    end-exec
           MOVE LENGTH OF
            control-file
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00006229' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303036323239' TO DFHEIV0
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
001333
001334     if resp-normal
001335        perform varying c1 from +1 by +1 until
001336           (c1 > +8)
001337           or (cf-benefit-code (c1) = ws-lf-ben-code)
001338        end-perform
001339
001340        if c1 < +9
001341           MOVE CF-BENEFIT-ALPHA (c1)
001342                                 TO WS-lf-KIND
001343           MOVE CF-SPECIAL-CALC-CD (c1)
001344                                 TO WS-lf-CALC-CD
001345           MOVE CF-BENEFIT-DESCRIP (c1)
001346                                 TO WS-lf-BEN-DESCRIP
001347           MOVE CF-LF-COVERAGE-TYPE (c1)
001348                                 TO WS-LF-COVERAGE-TYPE
001349           MOVE CF-CO-EARNINGS-CALC (c1)
001350                                 TO WS-lf-EARNINGS-CALC
001351        end-if
001352     end-if
001353
001354     .
001355 0400-exit.
001356     exit.
001357
001358 0410-get-ah-code.
001359
001360     move ws-comp-id             to ws-elcntl-key
001361     set ws-elcntl-ah-ben-cd     to true
001362     move ws-ah-ben-code         to ws-elcntl-hi-ben-cd
001363     move zeros                  to ws-elcntl-seq-no
001364
001365     
      * exec cics read
001366*       dataset     ('ELCNTL')
001367*       into        (control-file)
001368*       ridfld      (ws-elcntl-key)
001369*       gteq
001370*       resp        (ws-response)
001371*    end-exec
           MOVE LENGTH OF
            control-file
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00006268' TO DFHEIV0
           MOVE X'2622494C2020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303036323638' TO DFHEIV0
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
001372
001373     if resp-normal
001374        perform varying c1 from +1 by +1 until
001375           (c1 > +8)
001376           or (cf-benefit-code (c1) = ws-ah-ben-code)
001377        end-perform
001378        if c1 < +9
001379           MOVE CF-BENEFIT-ALPHA (c1)
001380                                 TO WS-ah-KIND
001381           MOVE CF-SPECIAL-CALC-CD (c1)
001382                                 TO WS-ah-CALC-CD
001383           MOVE CF-BENEFIT-DESCRIP (c1)
001384                                 TO WS-ah-BEN-DESCRIP
001385           MOVE CF-LF-COVERAGE-TYPE (c1)
001386                                 TO WS-ah-COVERAGE-TYPE
001387           MOVE CF-CO-EARNINGS-CALC (c1)
001388                                 TO WS-ah-EARNINGS-CALC
001389        end-if
001390     .
001391 0410-exit.
001392     exit.
001393
001394 0500-get-lf-rem.
001395
001396     MOVE '2'                    TO CP-PROCESS-TYPE
001397     MOVE WS-LF-COVERAGE-TYPE    TO CP-BENEFIT-TYPE
001398     MOVE WS-lf-EARNINGS-CALC    TO CP-EARNING-METHOD
001399                                    CP-RATING-METHOD
001400     MOVE WS-lf-CALC-CD          TO CP-SPECIAL-CALC-CD
001401     MOVE cm-cert-eff-dt         TO CP-CERT-EFF-DT
001402     MOVE cm-loan-1st-pmt-dt     TO CP-FIRST-PAY-DATE
001403     MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT
001404     IF cm-lf-orig-term = 0
001405        MOVE 1                   TO CP-ORIGINAL-TERM
001406     ELSE
001407        MOVE cm-lf-orig-term     TO CP-ORIGINAL-TERM
001408     end-if
001409     MOVE cm-loan-term           TO CP-LOAN-TERM
001410     MOVE '1'                    TO CP-REM-TRM-CALC-OPTION
001411     MOVE '4'                    TO CP-REM-TERM-METHOD
001412     MOVE ws-comp-id             TO CP-COMPANY-ID
001413     MOVE ws-comp-cd             TO CP-COMPANY-CD
001414
001415     PERFORM 9800-LINK-REM-TERM  thru 9800-exit
001416
001417     MOVE cm-lf-benefit-amt      TO CP-ORIGINAL-BENEFIT
001418                                    CP-RATING-BENEFIT-AMT
001419     MOVE cm-lf-premium-amt      TO CP-ORIGINAL-PREMIUM
001420     MOVE cm-lf-alt-benefit-amt  TO CP-ALTERNATE-BENEFIT
001421     MOVE cm-lf-alt-premium-amt  TO CP-ALTERNATE-PREMIUM
001422     MOVE cm-loan-apr            TO CP-LOAN-APR
001423     MOVE cm-pay-frequency       TO CP-PAY-FREQUENCY
001424
001425     MOVE cm-rate-class          TO CP-CLASS-CODE
001426     MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
001427     perform 9500-link-rem-amt   thru 9500-exit
001428
001429     .
001430 0500-exit.
001431     exit.
001432
001433 0510-get-ah-rem.
001434
001435     MOVE '2'                    TO CP-PROCESS-TYPE
001436     MOVE WS-ah-COVERAGE-TYPE    TO CP-BENEFIT-TYPE
001437     MOVE WS-ah-EARNINGS-CALC    TO CP-EARNING-METHOD
001438                                    CP-RATING-METHOD
001439     MOVE WS-ah-CALC-CD          TO CP-SPECIAL-CALC-CD
001440     MOVE cm-cert-eff-dt         TO CP-CERT-EFF-DT
001441     MOVE cm-loan-1st-pmt-dt     TO CP-FIRST-PAY-DATE
001442     MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT
001443     MOVE cm-ah-orig-term        TO CP-ORIGINAL-TERM
001444     MOVE cm-loan-term           TO CP-LOAN-TERM
001445     MOVE '1'                    TO CP-REM-TRM-CALC-OPTION
001446     MOVE '4'                    TO CP-REM-TERM-METHOD
001447     MOVE ws-comp-id             TO CP-COMPANY-ID
001448     MOVE ws-comp-cd             TO CP-COMPANY-CD
001449
001450     PERFORM 9800-LINK-REM-TERM  thru 9800-exit
001451
001452     MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM
001453     compute cp-remaining-amt =
001454        cm-ah-benefit-amt * cp-remaining-term-3
001455*    perform 9500-link-rem-amt   thru 9500-exit
001456
001457     .
001458 0510-exit.
001459     exit.
001460
001461 0600-get-elcert.
001462
001463     
      * exec cics read
001464*       dataset   ('ELCERT')
001465*       ridfld    (ws-elcert-key)
001466*       into      (certificate-master)
001467*       resp      (ws-response)
001468*    end-exec
           MOVE LENGTH OF
            certificate-master
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00006366' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303036333636' TO DFHEIV0
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
001469
001470     if not resp-normal
001471        display ' bad read on elcert ' ws-response
001472        go to 0600-exit
001473     end-if
001474
001475     if cm-lf-benefit-cd not = '00' and spaces
001476        move cm-lf-benefit-cd    to ws-lf-ben-code
001477        perform 0400-get-lf-code thru 0400-exit
001478     end-if
001479     if cm-ah-benefit-cd not = '00' and spaces
001480        move cm-ah-benefit-cd    to ws-ah-ben-code
001481        perform 0410-get-ah-code thru 0410-exit
001482     end-if
001483
001484     .
001485 0600-exit.
001486     exit.
001487
001488 0650-set-last-name.
001489
001490     move ws-name-in             to ws-name-out
001491     perform varying n1 from +13 by -1 until n1 < +3
001492        if (ws-name-in (n1:3) = ' SR' or ' JR' or ' II' or
001493           ' IV' or ' VI' or ' I ' or ' V ')
001494           or (ws-name-in (n1:4) = ' III')
001495           or (ws-name-in (n1:5) = ' IIII')
001496           or (ws-name-in (14:2) = ' I')
001497           or (ws-name-in (14:2) = ' V')
001498           move ws-name-in (1:n1 - 1)
001499                                 to ws-name-out
001500           move +3               to n1
001501        end-if
001502     end-perform
001503
001504     .
001505 0650-exit.
001506     exit.
001507
001508 0700-calc-cur-cm-ages.
001509
001510     move zeros                  to ws-cm-pri-curr-age
001511                                    ws-cm-cob-curr-age
001512     if (cm-insured-issue-age not numeric)
001513               or
001514        (cm-insured-issue-age = zeros)
001515        move 42                  to cm-insured-issue-age
001516     end-if
001517
001518     move cm-insured-issue-age   to ws-cm-pri-curr-age
001519
001520     move cm-cert-eff-dt         to dc-bin-date-1
001521     move ws-current-bin-dt      to dc-bin-date-2
001522     move '1'                    to dc-option-code
001523     perform 9700-DATE-CONVERT   thru 9700-exit
001524     if no-conversion-error
001525        compute ws-work-age =
001526           cm-insured-issue-age + (dc-elapsed-months / 12)
001527        move ws-work-age         to ws-cm-pri-curr-age
001528     end-if
001529
001530     .
001531 0700-joint-stuff.
001532
001533     if cm-jt-last-name = spaces
001534        and cm-jt-first-name = spaces
001535        go to 0700-exit
001536     end-if
001537
001538     if (cm-insured-joint-age not numeric)
001539        move zeros               to cm-insured-joint-age
001540     end-if
001541
001542     move cm-insured-joint-age   to ws-cm-cob-curr-age
001543
001544     move cm-cert-eff-dt         to dc-bin-date-1
001545     move ws-current-bin-dt      to dc-bin-date-2
001546     move '1'                    to dc-option-code
001547     perform 9700-DATE-CONVERT   thru 9700-exit
001548     if no-conversion-error
001549        compute ws-work-age =
001550           cm-insured-joint-age + (dc-elapsed-months / 12)
001551        move ws-work-age         to ws-cm-cob-curr-age
001552     end-if
001553
001554     .
001555 0700-exit.
001556     exit.
001557
001558 0710-calc-cur-p5-ages.
001559
001560     move zeros                  to ws-p5-pri-curr-age
001561                                    ws-p5-cob-curr-age
001562
001563     if (cm-insured-issue-age not numeric)
001564               or
001565        (cm-insured-issue-age = zeros)
001566        move 42                  to cm-insured-issue-age
001567     end-if
001568
001569     move cm-insured-issue-age   to ws-p5-pri-curr-age
001570
001571*    if p5-i-birthday <> low-values
001572*       move p5-i-birthday       to dc-bin-date-1
001573*       move ws-current-bin-dt   to dc-bin-date-2
001574*       move '1'                 to dc-option-code
001575*       perform 9700-DATE-CONVERT
001576*                                thru 9700-exit
001577*       if no-conversion-error
001578*          compute ws-work-age = dc-elapsed-months / 12
001579*          move ws-work-age      to ws-p5-pri-curr-age
001580*          go to 0710-joint-stuff
001581*       end-if
001582*    end-if
001583
001584     move cm-cert-eff-dt         to dc-bin-date-1
001585     move ws-current-bin-dt      to dc-bin-date-2
001586     move '1'                    to dc-option-code
001587     perform 9700-DATE-CONVERT   thru 9700-exit
001588     if no-conversion-error
001589        compute ws-work-age =
001590           cm-insured-issue-age + (dc-elapsed-months / 12)
001591        move ws-work-age         to ws-p5-pri-curr-age
001592     end-if
001593
001594     .
001595 0710-joint-stuff.
001596
001597     if cm-jt-last-name = spaces
001598        and cm-jt-first-name = spaces
001599        go to 0710-exit
001600     end-if
001601
001602     if (cm-insured-joint-age not numeric)
001603        move zeros               to cm-insured-joint-age
001604     end-if
001605
001606     move cm-insured-joint-age   to ws-p5-cob-curr-age
001607
001608*    if p5-i-joint-birthday <> low-values
001609*       move p5-i-joint-birthday to dc-bin-date-1
001610*       move ws-current-bin-dt   to dc-bin-date-2
001611*       move '1'                 to dc-option-code
001612*       perform 9700-DATE-CONVERT
001613*                                thru 9700-exit
001614*       if no-conversion-error
001615*          compute ws-work-age = dc-elapsed-months / 12
001616*          move ws-work-age      to ws-p5-cob-curr-age
001617*          go to 0710-exit
001618*       end-if
001619*    end-if
001620
001621     move cm-cert-eff-dt         to dc-bin-date-1
001622     move ws-current-bin-dt      to dc-bin-date-2
001623     move '1'                    to dc-option-code
001624     perform 9700-DATE-CONVERT   thru 9700-exit
001625     if no-conversion-error
001626        compute ws-work-age =
001627           cm-insured-joint-age + (dc-elapsed-months / 12)
001628        move ws-work-age         to ws-p5-cob-curr-age
001629     end-if
001630
001631     .
001632 0710-exit.
001633     exit.
001634
001635 0720-calc-cur-alph-age.
001636
001637     move zeros                  to ws-alph-curr-age
001638
001639     if (af-age not numeric)
001640               or
001641        (af-age = zeros)
001642        move 42                  to af-age
001643     end-if
001644
001645     move af-age                 to ws-alph-curr-age
001646
001647     move af-dt                  to dc-bin-date-1
001648     move ws-current-bin-dt      to dc-bin-date-2
001649     move '1'                    to dc-option-code
001650     perform 9700-DATE-CONVERT   thru 9700-exit
001651     if no-conversion-error
001652        compute ws-work-age = af-age + (dc-elapsed-months / 12)
001653        move ws-work-age         to ws-alph-curr-age
001654     end-if
001655
001656     .
001657 0720-exit.
001658     exit.
001659
001660 9500-LINK-REM-AMT.
001661
001662     
      * EXEC CICS LINK
001663*        PROGRAM   ('ELRAMT')
001664*        COMMAREA  (CALCULATION-PASS-AREA)
001665*        LENGTH    (CP-COMM-LENGTH)
001666*    END-EXEC
           MOVE 'ELRAMT' TO DFHEIV1
      *    MOVE '."C                   (   #00006565' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036353635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001667
001668     .
001669 9500-EXIT.
001670     EXIT.
001671
001672 9700-DATE-CONVERT.
001673
001674     
      * EXEC CICS LINK
001675*         PROGRAM  ('ELDATCV')
001676*         COMMAREA (DATE-CONVERSION-DATA)
001677*         LENGTH   (DC-COMM-LENGTH)
001678*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00006577' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036353737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001679
001680 9700-EXIT.
001681      EXIT.
001682 9800-LINK-REM-TERM.
001683
001684     
      * EXEC CICS LINK
001685*        PROGRAM   ('ELRTRM')
001686*        COMMAREA  (CALCULATION-PASS-AREA)
001687*        LENGTH    (CP-COMM-LENGTH)
001688*    END-EXEC
           MOVE 'ELRTRM' TO DFHEIV1
      *    MOVE '."C                   (   #00006587' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303036353837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001689
001690     .
001691 9800-EXIT.
001692     EXIT.
001693
001694 ABEND-PGM.
001695*                                COPY ELCABEND.
      *>>((file: ELCABEND))
000001*****************************************************************
000002*                                                               *
000003*                            ELCABEND.                          *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD 2.002
000006*                                                               *
000007*                THIS SECTION DISPLAYS THE NECESSARY MESSAGES   *
000008*            AND THEN ABENDS.                                   *
000009*                                                               *
000010*  NO  CID  MODS  IN  COPYBOOK  ELCABEND                        *
000011*                                                               *
000012*****************************************************************
000013*APS-010.
000014     DISPLAY WS-ABEND-MESSAGE.
000015     DISPLAY WS-ABEND-MESSAGE UPON CONSOLE.
000016
000017     IF WS-ABEND-FILE-STATUS NOT = ZERO
000018         DISPLAY 'FILE STATUS = ' WS-ABEND-FILE-STATUS
000019         DISPLAY 'FILE STATUS = ' WS-ABEND-FILE-STATUS
000020                                 UPON CONSOLE.
000021
000022     IF WS-RETURN-CODE NOT = ZERO
000023         DISPLAY 'RETURN CODE = '  WS-RETURN-CODE
000024         DISPLAY 'RETURN CODE = '  WS-RETURN-CODE
000025                                 UPON CONSOLE.
000026
000027     DISPLAY 'PROGRAM WILL NOW ABEND **************'
000028     DISPLAY 'PROGRAM WILL NOW ABEND **************'
000029                                 UPON CONSOLE.
000030
000031     DIVIDE WS-ZERO BY WS-ZERO GIVING WS-ZERO.
000032     CALL 'ABORTME'.
000033
000034 APS-EXIT.
000035     EXIT.
      *<<((file: ELCABEND))

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
