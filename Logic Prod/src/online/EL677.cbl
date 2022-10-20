      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL677.
00008 *
00009 *AUTHOR.     CSO
00010 *            OMAHA, NEBRASKA.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *                                                                *
00020 *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF CSO             *
00022 *            *                                                   *
00023 *            *****************************************************
00024
00025 *REMARKS.    TRANSACTION - EXF3 - CHECK MAINTENANCE
111513******************************************************************
111513*                   C H A N G E   L O G
111513*
111513* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111513*-----------------------------------------------------------------
111513*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111513* EFFECTIVE    NUMBER
111513*-----------------------------------------------------------------
111513* 111513  CR2013053000001  PEMA  DAILY CHECK REQUEST CHANGES
021714* 021714  CR2014021700001  PEMA  ADD TEST DB FOR OTHER THAN cid1p
030414* 030414  IR2014030400001  PEMA  CHG PYAJ VOID TRANS TO R AND +
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
091615* 091615  CR2015082000001  PEMA  ADD TOTAL ENDT PROCESSING
020317* 020317  IR2016110900001  PEMA  FIXED MISC PROBLEMS
060717* 060717  CR2017032900002  PEMA  CHANGE TEST DB
111418* 111418  CR2018103100001  PEMA  ADD REISSUE CAPABILITY
040919* 040919  CR2019040900001  PEMA  FIX RETURN TO ON REISSUES
111219* 111219  CR2019110700001  PEMA  ALLOW REVERSAL OF VOID ON SAME DA
030921* 030921  CR2019012500003  PEMA  Connect to sdv-db01.cso.local
011822* 011822  CR2019012500003  PEMA  Convert to SQLSERVER 2016
070622* 070622  CR2020061200002  TANA  Add payee code
111513******************************************************************
00026  ENVIRONMENT DIVISION.
00027
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL677 WORKING STORAGE     *'.
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'.
       77  s1                          pic s999 comp-3 value +0.
       77  ws-tot-lf-prem              pic s9(7)v99 comp-3 value +0.
       77  ws-tot-iss-prem             pic s9(7)v99 comp-3 value +0.
       77  ws-tot-iss-comm             pic s9(7)v99 comp-3 value +0.
       77  ws-tot-ref-comm             pic s9(7)v99 comp-3 value +0.
111219 77  ws-pyaj-browse-sw           pic x value spaces.
111219     88  pyaj-browse-started        value 'Y'.
       77  ws-browse-sw                pic x value spaces.
           88  i-say-when                 value 'Y'.
       77  ws-delete-sw                pic x value ' '.
           88  row-deleted                 value 'Y'.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  ws-match-sw                 pic x  value ' '.
           88  found-a-match             value 'Y'.
       77  ws-commit-sw                pic x value ' '.
           88  tbl-commited                value 'Y'.
021714 01  P pointer.
021714 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
021714 01  var-ptr pointer.
021714 01  env-var-len                 pic 9(4)  binary.
021714
021714 01  WS-KIXSYS.
021714     05  WS-KIX-FIL1             PIC X(10).
021714     05  WS-KIX-APPS             PIC X(10).
021714     05  WS-KIX-ENV              PIC X(10).
021714     05  WS-KIX-MYENV            PIC X(10).
021714     05  WS-KIX-SYS              PIC X(10).
       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC
       01  ws-paid-bank-work-area.
           05  ws-pb-compid            pic x(9).
           05  ws-pb-check-no          pic x(10).
           05  ws-pb-bad-check-no      pic x(10).
021714     05  ws-check-amount         pic x(10).
       01  Paid-Bank-Info.
           05  pb-check-no             pic 9(10).
           05  pb-tran-type            pic x.
           05  pb-bank-acct-desc       pic x(50).
           05  pb-amount               pic x(12).
           05  pb-paid-date            pic x(25).
       01  ws-key-stuff.
           05  ws-compid               pic xxx.
           05  ws-carrier              pic x.
           05  ws-grouping             pic x(6).
           05  ws-state                pic xx.
           05  ws-account              pic x(10).
           05  ws-eff-date             pic x(10).
           05  ws-certificate          pic x(10).
           05  ws-cert-sfx             pic x.
           05  ws-seq-no               pic 999.
           05  ws-type                 pic x.
091615     05  ws-check-sub-type       pic x.
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is passed nulls from sql. The indicator will be -1        ***
      ***  if the value on sql is nulls and +0 if the value is       ***
      ***  something other than nulls. Here is an example on how     ***
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
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
       01  indicator-vaiables-for-nulls.
           05  nu-app-status           pic s9(4) comp value +0.
           05  nu-app-by               pic s9(4) comp value +0.
           05  nu-app-date             pic s9(4) comp value +0.
           05  nu-app-batch            pic s9(4) comp value +0.
       01  daily-check-request-rec.
           05  db-compid               pic xxx.
           05  db-carrier              pic x.
           05  db-grouping             pic x(6).
           05  db-state                pic xx.
           05  db-account              pic x(10).
           05  db-effdate              pic x(10).
           05  db-certificate          pic x(10).
           05  db-cert-sfx             pic x.
           05  db-seq-no               pic 999.
           05  db-type                 pic x.
           05  db-amount-n             pic 9(7).99.
           05  db-amount redefines db-amount-n
                                       pic x(10).
           05  db-checkno              pic x(15).
           05  db-checkdate            pic x(10).
           05  db-checkstatus          pic 9(5).
           05  db-releasebatch         pic 9(5).
           05  db-releasedt            pic x(10).
           05  db-releaseby            pic x(4).
           05  db-payeename1           pic x(30).
           05  db-payeename2           pic x(30).
           05  db-payeeaddr1           pic x(30).
           05  db-payeeaddr2           pic x(30).
           05  db-payeecity            pic x(30).
           05  db-payeest              pic xx.
           05  db-payeezip             pic x(10).
           05  db-fincar               pic x.
           05  db-fingrp               pic x(6).
           05  db-finresp              pic x(10).
           05  db-finacct              pic x(10).
           05  db-preparer             pic x(4).
           05  db-app-status           pic x(9).
           05  dp-app-status-n redefines db-app-status
                                       pic 9(9).
           05  db-app-by               pic x(20).
           05  db-app-date             pic x(30).
           05  db-app-batch            pic x(10).
           05  db-return-to            pic x(30).
           05  db-insured-name         pic x(30).
091615     05  db-check-sub-type       pic x.
070622     05  db-payeecode            pic x(10).
       EXEC SQL
          END DECLARE SECTION
       END-EXEC
00035 *                            COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
00036 *                            COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
       01  ws-dummy-erchek             pic x(600).
       01  filler.
021714     05  WS-CHECK-AMT-TMP        PIC Z(7).99.
021714     05  WS-CHECK-AMT-TMPX REDEFINES
021714         WS-CHECK-AMT-TMP        PIC X(10).
       01  ws-compare-erchek-key          pic x(33) value low-values.
       01  ws-eracct-sw                   pic x value ' '.
           88  eracct-found                 value 'Y'.
       01  ws-eracct-start-sw             pic x value ' '.
           88  eracct-start                 value 'Y'.
00040  01  WS-MISC-AREA.
00041      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00042      05  SAVE-CURRENT-DATE-MDY       PIC X(6)    VALUE SPACES.
00043      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
00044
       01  ws-user-defined.
           05  ws-user-reason         pic xxx.
           05  ws-user-cert-no        pic x(11).
           05  ws-user-name           pic x(28).
       01  ws-hold-eracct-record       pic x(2000)  value spaces.
00045  01  STANDARD-AREAS.
           12  ws-connect-sw               pic x  value ' '.
               88  connected-to-db             value 'Y'.
00046      12  SC-ITEM                     PIC S9(4) COMP VALUE +1.
00047      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00048      12  EL677A                      PIC X(8)    VALUE 'EL677A'.
00049      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL677S'.
00050      12  SCREEN-NUMBER               PIC X(4)    VALUE '677A'.
00051      12  TRANS-ID                    PIC X(4)    VALUE 'EXF3'.
00052      12  EL6311-TRANS-ID             PIC X(4)    VALUE 'EXB1'.
091615     12  EL6318-TRANS-ID             PIC X(4)    VALUE 'EXBE'.
00054      12  THIS-PGM                    PIC X(8)    VALUE 'EL677'.
00055      12  PGM-NAME                    PIC X(8)   VALUE SPACES.
00056      12  TIME-IN                     PIC S9(7)  VALUE ZEROS.
00057      12  TIME-OUT-R  REDEFINES TIME-IN.
00058          16  FILLER                  PIC X.
00059          16  TIME-OUT                PIC 99V99.
00060          16  FILLER                  PIC XX.
00061      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00062      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00063      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.
00064      12  XCTL-1273                   PIC X(8)    VALUE 'EL1273'.
00065      12  XCTL-114                    PIC X(8)    VALUE 'EL114'.
00066      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00067      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00068      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00069      12  ELCNTL-FILE-ID              PIC X(8)    VALUE 'ELCNTL'.
00071      12  ERACCT-FILE-ID              PIC X(8)    VALUE 'ERACCT'.
00072      12  ERCOMP-FILE-ID              PIC X(8)    VALUE 'ERCOMP'.
00073      12  ERCHEK-FILE-ID              PIC X(8)    VALUE 'ERCHEK'.
00075      12  ERPNDB-FILE-ID              PIC X(8)    VALUE 'ERPNDB'.
00076      12  ERPNDB-ALT-FILE-ID          PIC X(8)    VALUE 'ERPNDB2'.
00077      12  ELCERT-FILE-ID              PIC X(8)    VALUE 'ELCERT'.
00078      12  ERMAIL-FILE-ID              PIC X(8)    VALUE 'ERMAIL'.
00081      12  ERPYAJ-FILE-ID              PIC X(8)    VALUE 'ERPYAJ'.
00082      12  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)   VALUE +0    COMP.
00084      12  RETURNED-FROM               PIC X(8)    VALUE SPACES.
00085      12  QID.
00086          16  QID-TERM                PIC X(4)    VALUE SPACES.
00087          16  FILLER                  PIC X(4)    VALUE '677A'.
00088
00089  01  WORK-AREAS.
00090      12  WS-AM-WORK                  PIC S9(8)V99 VALUE +0.
00091      12  WS-AM-WK REDEFINES WS-AM-WORK.
00092          16  WS-AM-NUM               PIC S9(7)V99.
00093          16  WS-AM-SIGN              PIC X.
00094      12  WS-CHECK-WORK.
00095          16  FILLER                  PIC X        VALUE SPACE.
00096          16  WS-CHECK-NO             PIC X(6)     VALUE SPACES.
00097      12  WS-CHK-PRINT-DT             PIC XX       VALUE LOW-VALUE.
00098      12  WS-SV-AMOUNT-PAID           PIC S9(7)V99 VALUE +0.
00099      12  WS-AMT                      PIC S9(7)V99 VALUE +0.
00103      12  WS-REFUND-AMOUNT            PIC S9(7)V99 VALUE +0.
00107      12  WS-DEEDIT-FIELD             PIC S9(9)V99 VALUE +0.
00108      12  WS-DT-DEEDIT-FIELD          PIC X(10)   VALUE ZERO.
00109      12  WS-DEEDIT-FIELD-DATE        REDEFINES
00110          WS-DT-DEEDIT-FIELD.
00111          16  FILLER                  PIC X(4).
00112          16  WS-DEEDIT-FIELD-DATE-OUT PIC X(6).
CIDMOD     12  WS-DEEDIT-FIELD-A           PIC X(15)   VALUE ZERO.
CIDMOD     12  WS-DEEDIT-FIELD-V0          REDEFINES
CIDMOD         WS-DEEDIT-FIELD-A           PIC S9(15).
00113
00114      12  WS-SUB                      PIC S9(4)   VALUE ZERO  COMP.
00117      12  WS-CERT-FOUND-SW            PIC X       VALUE 'N'.
00118          88 WS-CERT-FOUND                        VALUE 'Y'.
00119          88 WS-CERT-NOT-FOUND                    VALUE 'N'.
00120      12  WS-PNDB-FOUND-SW            PIC X       VALUE 'N'.
00121          88 WS-PNDB-FOUND                        VALUE 'Y'.
00122          88 WS-PNDB-NOT-FOUND                    VALUE 'N'.
00123      12  WS-PROCESS-CERT-SW          PIC X       VALUE SPACE.
00124          88  WS-PROCESS-CERT                     VALUE 'Y'.
00125      12  WS-PROCESS-BENEFICIARY-SW   PIC X       VALUE SPACE.
00126          88  WS-PROCESS-BENEFICIARY              VALUE 'Y'.
00163      12  WS-TMS-ENTRY-COMMENT.
00164          16  WS-TMS-PY-CERT          PIC X(10)   VALUE SPACES.
00165          16  FILLER                  PIC XX      VALUE SPACES.
00166          16  WS-TMS-PY-PAYEE         PIC X(18)   VALUE SPACES.
00167      12  WS-ZIP-CODE.
00168          16  WS-ZIP-PRIME.
00169              20  WS-ZIP-PRI-1ST      PIC X.
00170                  88  WS-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00171              20  FILLER              PIC X(4).
00172          16  WS-ZIP-PLUS4            PIC X(4).
00173      12  WS-CANADIAN-POST-CODE1  REDEFINES  WS-ZIP-CODE.
00174          16  WS-CAN-POST1-1          PIC XXX.
00175          16  WS-CAN-POST1-2.
00176              20  WS-CAN-POST-4TH     PIC X.
00177              20  FILLER              PIC XX.
00178          16  FILLER                  PIC XXX.
00179      12  WS-CANADIAN-POST-CODE2  REDEFINES  WS-ZIP-CODE.
00180          16  WS-CAN-POST2-1          PIC XXX.
00181          16  FILLER                  PIC X.
00182          16  WS-CAN-POST2-2          PIC XXX.
00183          16  FILLER                  PIC XX.
00184
00185 *                                    COPY ELCNWA.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCNWA.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *                                                               *
00007 *            M O V E   N A M E   W O R K   A R E A.             *
00008 *                                                               *
00009 *****************************************************************.
00010
00011  01  WS-NAME-WORK-AREA.
00012      05  WS-INSURED-LAST-NAME        PIC X(15).
00013      05  WS-INSURED-1ST-NAME         PIC X(12).
00014      05  WS-INSURED-MID-INIT         PIC X.
00015
00016      05  WS-NAME-WORK.
00017          10  WS-NW                   PIC X
00018              OCCURS 30 TIMES INDEXED BY NWA-INDEX.
00019
00020      05  WS-NAME-WORK2.
00021          10  WS-NW2                  PIC X
00022              OCCURS 20 TIMES INDEXED BY NWA-INDEX2 NWA-INDEX3
00023                                         NWA-INDEX0.
00024
00025      05  WS-NAME-SW                  PIC S9          VALUE ZERO
00026                                      COMP-3.
00027
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
00188  01  ACCESS-KEYS.
00194      12  ELCNTL-KEY.
00195          16 ELCNTL-COMP-ID           PIC XXX     VALUE SPACES.
00196          16 ELCNTL-REC-TYPE          PIC X       VALUE SPACES.
00197          16 ELCNTL-ACCESS.
00198              20 ELCNTL-STATE         PIC XX      VALUE SPACES.
00199              20  FILLER              PIC X       VALUE SPACES.
00200              20 ELCNTL-CARRIER       PIC X       VALUE SPACES.
00201          16 ELCNTL-SEQ               PIC S9(4)   VALUE +0    COMP.
00202
00205      12  ERACCT-KEY.
00206          16  ERACCT-CO               PIC X       VALUE SPACES.
00207          16  ERACCT-CARRIER          PIC X       VALUE SPACES.
00208          16  ERACCT-GROUPING         PIC X(6)    VALUE SPACES.
00209          16  ERACCT-STATE            PIC XX      VALUE SPACES.
00210          16  ERACCT-ACCOUNT          PIC X(10)   VALUE SPACES.
00211          16  ERACCT-EXP-DATE         PIC XX      VALUE SPACES.
00212          16  ERACCT-EXP-DATE-FILLER  PIC X(4)    VALUE SPACES.
00213
00214      12  SAVE-ERACCT-KEY.
00215          16  SV-ACCT-CO              PIC X       VALUE SPACES.
00216          16  SV-ACCT-CARRIER         PIC X       VALUE SPACES.
00217          16  SV-ACCT-GROUPING        PIC X(6)    VALUE SPACES.
00218          16  SV-ACCT-STATE           PIC XX      VALUE SPACES.
00219          16  SV-ACCT-ACCOUNT         PIC X(10)   VALUE SPACES.
00220          16  SV-ACCT-EXP-DATE        PIC XX      VALUE SPACES.
00221          16  SV-ACCT-EXP-DATE-FILLER PIC X(4)    VALUE SPACES.
00222
00225      12  ERCOMP-KEY.
00226          16  ERCOMP-COMP-CD          PIC X       VALUE SPACE.
00227          16  ERCOMP-CARRIER          PIC X       VALUE SPACES.
00228          16  ERCOMP-GROUPING         PIC X(6)    VALUE SPACES.
00229          16  ERCOMP-FIN-RESP         PIC X(10)   VALUE SPACES.
00230          16  ERCOMP-ACCOUNT          PIC X(10)   VALUE SPACES.
00231          16  ERCOMP-RECORD-TYPE      PIC X       VALUE SPACES.
00232
00235      12  ELCERT-KEY.
00236          16 ELCERT-COMPANY-CD        PIC X       VALUE SPACES.
00237          16 ELCERT-CARRIER           PIC X       VALUE SPACES.
00238          16 ELCERT-GROUPING          PIC X(6)    VALUE SPACES.
00239          16 ELCERT-STATE             PIC XX      VALUE SPACES.
00240          16 ELCERT-ACCOUNT           PIC X(10)   VALUE SPACES.
00241          16 ELCERT-CERT-EFF-DT       PIC XX      VALUE SPACES.
00242          16 ELCERT-CERT-PRIME        PIC X(10)   VALUE SPACES.
00243          16 ELCERT-CERT-SFX          PIC X       VALUE SPACES.
00244
00247      12  ERPNDB-PRIMARY-KEY.
00248          16 ERPNDB-COMPANY-CD        PIC X       VALUE SPACES.
00249          16 ERPNDB-ENTRY-BATCH       PIC X(6)    VALUE SPACES.
00250          16 ERPNDB-BATCH-SEQ-NO      PIC S9(4)   VALUE +0    COMP.
00251          16 ERPNDB-BATCH-CHG-SEQ-NO  PIC S9(4)   VALUE +0    COMP.
00252
00253      12  ERPNDB-ALT-KEY.
00254          16 ERPNDB-ALT-COMPANY-CD    PIC X       VALUE SPACES.
00255          16 ERPNDB-ALT-CARRIER       PIC X       VALUE SPACES.
00256          16 ERPNDB-ALT-GROUPING      PIC X(6)    VALUE SPACES.
00257          16 ERPNDB-ALT-STATE         PIC XX      VALUE SPACES.
00258          16 ERPNDB-ALT-ACCOUNT       PIC X(10)   VALUE SPACES.
00259          16 ERPNDB-ALT-CERT-EFF-DT   PIC XX      VALUE SPACES.
00260          16 ERPNDB-ALT-CERT-PRIME    PIC X(10)   VALUE SPACES.
00261          16 ERPNDB-ALT-CERT-SFX      PIC X       VALUE SPACES.
00262          16 ERPNDB-ALT-CH-SEQ-NO     PIC S9(4)   VALUE +0    COMP.
00263          16 ERPNDB-ALT-RECORD-TYPE   PIC X       VALUE SPACE.
00264
00275      12  ERCHEK-KEY.
00276          16  CHEK-COMPANY-CD         PIC X       VALUE SPACE.
00277          16  CHEK-CARRIER            PIC X       VALUE SPACE.
00278          16  CHEK-GROUPING           PIC X(6)    VALUE SPACES.
00279          16  CHEK-STATE              PIC XX      VALUE SPACES.
00280          16  CHEK-ACCOUNT            PIC X(10)   VALUE SPACES.
00281          16  CHEK-EFF-DT             PIC XX      VALUE SPACES.
00282          16  CHEK-CERT-NO            PIC X(10)   VALUE SPACES.
00283          16  CHEK-SUF-NO             PIC X       VALUE SPACES.
00284          16  CHEK-RECORD-SEQ         PIC S9(4)   VALUE ZEROS COMP.
00285
00286      12  ERCHEK-RECORD-LENGTH        PIC S9(4)   VALUE +600  COMP.
00301      12  ERPYAJ-KEY.
00302          16 PYAJ-COMPANY-CD          PIC X       VALUE LOW-VALUES.
00303          16 PYAJ-CARRIER             PIC X       VALUE SPACE.
00304          16 PYAJ-GROUPING            PIC X(06)   VALUE SPACES.
00305          16 PYAJ-FIN-RESP            PIC X(10)   VALUE SPACES.
00306          16 PYAJ-ACCOUNT             PIC X(10)   VALUE SPACES.
00307          16 PYAJ-FILE-SEQ-NO         PIC S9(8)   VALUE +0  COMP.
00308          16 PYAJ-RECORD-TYPE         PIC X       VALUE SPACE.
00309
00313  01  ERROR-NUMBERS.
00314      12  ER-0000                 PIC X(4)    VALUE '0000'.
00315      12  ER-0004                 PIC X(4)    VALUE '0004'.
00317      12  ER-0013                 PIC X(4)    VALUE '0013'.
00318      12  ER-0022                 PIC X(4)    VALUE '0022'.
00319      12  ER-0023                 PIC X(4)    VALUE '0023'.
00321      12  ER-0029                 PIC X(4)    VALUE '0029'.
00322      12  ER-0070                 PIC X(4)    VALUE '0070'.
00323      12  ER-0194                 PIC X(4)    VALUE '0194'.
00324      12  ER-0195                 PIC X(4)    VALUE '0195'.
00325      12  ER-0196                 PIC X(4)    VALUE '0196'.
00326      12  ER-0197                 PIC X(4)    VALUE '0197'.
00327      12  ER-0203                 PIC X(4)    VALUE '0203'.
00328      12  ER-0215                 PIC X(4)    VALUE '0215'.
00329      12  ER-0216                 PIC X(4)    VALUE '0216'.
00330      12  ER-0433                 PIC X(4)    VALUE '0433'.
00332      12  ER-1159                 PIC X(4)    VALUE '1159'.
00333      12  ER-1162                 PIC X(4)    VALUE '1162'.
00334      12  ER-2056                 PIC X(4)    VALUE '2056'.
00335      12  ER-2208                 PIC X(4)    VALUE '2208'.
00336      12  ER-2209                 PIC X(4)    VALUE '2209'.
00337      12  ER-2210                 PIC X(4)    VALUE '2210'.
00338      12  ER-2230                 PIC X(4)    VALUE '2230'.
00339      12  ER-2237                 PIC X(4)    VALUE '2237'.
00340      12  ER-2238                 PIC X(4)    VALUE '2238'.
00342      12  ER-2394                 PIC X(4)    VALUE '2394'.
00345      12  ER-2583                 PIC X(4)    VALUE '2583'.
00346      12  ER-2600                 PIC X(4)    VALUE '2600'.
00347      12  ER-2726                 PIC X(4)    VALUE '2726'.
00350      12  ER-2907                 PIC X(4)    VALUE '2907'.
00351      12  ER-2908                 PIC X(4)    VALUE '2908'.
00353      12  ER-3044                 PIC X(4)    VALUE '3044'.
00354      12  ER-3046                 PIC X(4)    VALUE '3046'.
111418     12  er-3274                 pic x(4)    value '3274'.
           12  er-3450                 pic x(4)    value '3450'.
           12  er-3451                 pic x(4)    value '3451'.
           12  er-3452                 pic x(4)    value '3452'.
           12  er-3453                 pic x(4)    value '3453'.
           12  er-3454                 pic x(4)    value '3454'.
           12  er-3455                 pic x(4)    value '3455'.
091615     12  er-3459                 pic x(4)    value '3459'.
111418     12  er-3460                 pic x(4)    value '3460'.
111219     12  er-3461                 pic x(4)    value '3461'.
111219     12  er-3462                 pic x(4)    value '3462'.
111219     12  er-3463                 pic x(4)    value '3463'.
           12  er-9999                 pic x(4)    value '9999'.
00360 *                                COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
00362 *                                COPY ELCDATE.
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
00364 *                                COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
00366 *                                COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00368 *                                COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
00369      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00370          16  PI-EOF-SW               PIC X.
00371              88  PI-FILE-EOF                     VALUE 'Y'.
00372          16  PI-PREV-MAINT           PIC X.
00373          16  PI-WORK-SEQ-NO          PIC S9(9)   COMP-3.
00374          16  PI-ERCHEK-KEY.
00375              20  PI-CHEK-COMP-CD     PIC X.
00376              20  PI-CHEK-CARRIER     PIC X.
00377              20  PI-CHEK-GROUPING    PIC X(6).
00378              20  PI-CHEK-STATE       PIC XX.
00379              20  PI-CHEK-ACCOUNT     PIC X(10).
00380              20  PI-CHEK-EFF-DT      PIC XX.
00381              20  PI-CHEK-CERT-NO     PIC X(10).
00382              20  PI-CHEK-SUF-NO      PIC X.
00383              20  PI-CHEK-SEQUENCE    PIC S9(4)  COMP.
00384          16  PI-ACCOUNT-ADDRESS.
00385              20  PI-AM-NAME          PIC X(30).
00386              20  PI-AM-ADDRS         PIC X(30).
00387              20  PI-AM-CITY-st.
                       24  pi-am-city      PIC X(28).
                       24  pi-am-st        pic xx.
00388              20  PI-AM-ZIP-CODE.
00389                  24  PI-AM-ZIP-PRIME PIC X(5).
00390                  24  PI-AM-ZIP-PLUS4 PIC X(4).
00391          16  PI-PROCESS-CERT-SW          PIC X.
00392              88  PI-PROCESS-CERT                     VALUE 'Y'.
00393          16  PI-PROCESS-BENEFICIARY-SW   PIC X.
00394              88  PI-PROCESS-BENEFICIARY              VALUE 'Y'.
00395          16  PI-PAYTO1                   PIC X(30).
00396          16  PI-REFERENCE                PIC X(12).
00397          16  PI-CANC-DT                  PIC XX.
00398          16  PI-LF-REFUND                PIC S9(7)V99  COMP-3.
00399          16  PI-AH-REFUND                PIC S9(7)V99  COMP-3.
00400          16  PI-INSURED-NAME             PIC X(28).
00401          16  PI-PFKEY                    PIC XXX.
00402              88  PI-TO-EL1273-FROM-EL677        VALUE 'PF3'.
00403              88  PI-TO-EL677-FROM-EL1273        VALUE 'PF8'.
00404          16  PI-AMOUNT                   PIC S9(9)V99  COMP-3.
00405          16  PI-TYPE                     PIC X.
               16  pi-prev-paid                pic s9(7)v99 comp-3.
               16  pi-refund-on-pending-rec    pic s9(7)v99 comp-3.
               16  pi-ue-comm                  pic s9(7)v99 comp-3.
               16  pi-chek-rec-cnt             pic s999 comp-3.
               16  pi-am-csr                   pic x(4).
               16  pi-return-to                pic x(30).
               16  pi-prev-ded-comm            pic x.
               16  pi-table-name               pic x(30).
030414         16  pi-check-cashed             pic x.
091615         16  pi-endt-prm-diff            pic s9(7)v99 comp-3.
091615         16  pi-endt-com-diff            pic s9(5)v99 comp-3.
091615         16  pi-check-type               pic x.
091615             88  pi-corr-check              value 'C'.
091615             88  pi-ref-check               value 'R'.
091615         16  pi-check-cut                pic x.
103116         16  pi-prev-paid-this-month     pic s9(7)v99 comp-3.
               16  pi-previous-deduct-comm     pic x.
111418         16  pi-void-reissue-pass        pic x.
111418             88  pi-void-complete           value '1'.
111418             88  pi-address-selected        value '2'.
111418         16  pi-void-reissue-amt         pic s9(7)v99 comp-3.
111418         16  FILLER                      PIC X(304). *> wass 311
00409 *                                    COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  '�'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00410  01  FILLER    REDEFINES DFHAID.
00411      12  FILLER                      PIC X(8).
00412      12  PF-VALUES                   PIC X       OCCURS 24.
00413      12  FILLER                      PIC X(3).
00415 *                                    COPY EL677S.
       01  EL677AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  DMAINT1L PIC S9(0004) COMP.
           05  DMAINT1F PIC  X(0001).
           05  FILLER REDEFINES DMAINT1F.
               10  DMAINT1A PIC  X(0001).
           05  DMAINT1I PIC  X(0009).
      *    -------------------------------
           05  DMAINT2L PIC S9(0004) COMP.
           05  DMAINT2F PIC  X(0001).
           05  FILLER REDEFINES DMAINT2F.
               10  DMAINT2A PIC  X(0001).
           05  DMAINT2I PIC  X(0029).
      *    -------------------------------
           05  CARRIERL PIC S9(0004) COMP.
           05  CARRIERF PIC  X(0001).
           05  FILLER REDEFINES CARRIERF.
               10  CARRIERA PIC  X(0001).
           05  CARRIERI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  EFFDTL PIC S9(0004) COMP.
           05  EFFDTF PIC  X(0001).
           05  FILLER REDEFINES EFFDTF.
               10  EFFDTA PIC  X(0001).
           05  EFFDTI PIC  X(0008).
      *    -------------------------------
           05  CERTNOL PIC S9(0004) COMP.
           05  CERTNOF PIC  X(0001).
           05  FILLER REDEFINES CERTNOF.
               10  CERTNOA PIC  X(0001).
           05  CERTNOI PIC  X(0010).
      *    -------------------------------
           05  SFXL PIC S9(0004) COMP.
           05  SFXF PIC  X(0001).
           05  FILLER REDEFINES SFXF.
               10  SFXA PIC  X(0001).
           05  SFXI PIC  X(0001).
      *    -------------------------------
           05  SEQL PIC S9(0004) COMP.
           05  SEQF PIC  X(0001).
           05  FILLER REDEFINES SEQF.
               10  SEQA PIC  X(0001).
           05  SEQI PIC  99.
      *    -------------------------------
           05  PAYTO1L PIC S9(0004) COMP.
           05  PAYTO1F PIC  X(0001).
           05  FILLER REDEFINES PAYTO1F.
               10  PAYTO1A PIC  X(0001).
           05  PAYTO1I PIC  X(0030).
      *    -------------------------------
           05  CREATEDL PIC S9(0004) COMP.
           05  CREATEDF PIC  X(0001).
           05  FILLER REDEFINES CREATEDF.
               10  CREATEDA PIC  X(0001).
           05  CREATEDI PIC  X(0008).
      *    -------------------------------
           05  CBYL PIC S9(0004) COMP.
           05  CBYF PIC  X(0001).
           05  FILLER REDEFINES CBYF.
               10  CBYA PIC  X(0001).
           05  CBYI PIC  X(0004).
      *    -------------------------------
           05  PAYTO2L PIC S9(0004) COMP.
           05  PAYTO2F PIC  X(0001).
           05  FILLER REDEFINES PAYTO2F.
               10  PAYTO2A PIC  X(0001).
           05  PAYTO2I PIC  X(0030).
      *    -------------------------------
           05  PAYEEL PIC S9(0004) COMP.
           05  PAYEEF PIC  X(0001).
           05  FILLER REDEFINES PAYEEF.
               10  PAYEEA PIC  X(0001).
           05  PAYEEI PIC  X(0006).
      *    -------------------------------
           05  APVSTATL PIC S9(0004) COMP.
           05  APVSTATF PIC  X(0001).
           05  FILLER REDEFINES APVSTATF.
               10  APVSTATA PIC  X(0001).
           05  APVSTATI PIC  X(0009).
      *    -------------------------------
           05  PAYAD1L PIC S9(0004) COMP.
           05  PAYAD1F PIC  X(0001).
           05  FILLER REDEFINES PAYAD1F.
               10  PAYAD1A PIC  X(0001).
           05  PAYAD1I PIC  X(0030).
      *    -------------------------------
           05  APVDTL PIC S9(0004) COMP.
           05  APVDTF PIC  X(0001).
           05  FILLER REDEFINES APVDTF.
               10  APVDTA PIC  X(0001).
           05  APVDTI PIC  X(0008).
      *    -------------------------------
           05  APVBYL PIC S9(0004) COMP.
           05  APVBYF PIC  X(0001).
           05  FILLER REDEFINES APVBYF.
               10  APVBYA PIC  X(0001).
           05  APVBYI PIC  X(0004).
      *    -------------------------------
           05  PAYAD2L PIC S9(0004) COMP.
           05  PAYAD2F PIC  X(0001).
           05  FILLER REDEFINES PAYAD2F.
               10  PAYAD2A PIC  X(0001).
           05  PAYAD2I PIC  X(0030).
      *    -------------------------------
           05  VOIDEDL PIC S9(0004) COMP.
           05  VOIDEDF PIC  X(0001).
           05  FILLER REDEFINES VOIDEDF.
               10  VOIDEDA PIC  X(0001).
           05  VOIDEDI PIC  X(0008).
      *    -------------------------------
           05  VBYL PIC S9(0004) COMP.
           05  VBYF PIC  X(0001).
           05  FILLER REDEFINES VBYF.
               10  VBYA PIC  X(0001).
           05  VBYI PIC  X(0004).
      *    -------------------------------
           05  PAYCTYL PIC S9(0004) COMP.
           05  PAYCTYF PIC  X(0001).
           05  FILLER REDEFINES PAYCTYF.
               10  PAYCTYA PIC  X(0001).
           05  PAYCTYI PIC  X(0028).
      *    -------------------------------
           05  PAYSTL PIC S9(0004) COMP.
           05  PAYSTF PIC  X(0001).
           05  FILLER REDEFINES PAYSTF.
               10  PAYSTA PIC  X(0001).
           05  PAYSTI PIC  X(0002).
      *    -------------------------------
           05  PRINTEDL PIC S9(0004) COMP.
           05  PRINTEDF PIC  X(0001).
           05  FILLER REDEFINES PRINTEDF.
               10  PRINTEDA PIC  X(0001).
           05  PRINTEDI PIC  X(0008).
      *    -------------------------------
           05  PTOZIPL PIC S9(0004) COMP.
           05  PTOZIPF PIC  X(0001).
           05  FILLER REDEFINES PTOZIPF.
               10  PTOZIPA PIC  X(0001).
           05  PTOZIPI PIC  X(0009).
      *    -------------------------------
           05  DEDCYNL PIC S9(0004) COMP.
           05  DEDCYNF PIC  X(0001).
           05  FILLER REDEFINES DEDCYNF.
               10  DEDCYNA PIC  X(0001).
           05  DEDCYNI PIC  X(0001).
      *    -------------------------------
           05  CASHEDL PIC S9(0004) COMP.
           05  CASHEDF PIC  X(0001).
           05  FILLER REDEFINES CASHEDF.
               10  CASHEDA PIC  X(0001).
           05  CASHEDI PIC  X(0008).
      *    -------------------------------
           05  RETTOL PIC S9(0004) COMP.
           05  RETTOF PIC  X(0001).
           05  FILLER REDEFINES RETTOF.
               10  RETTOA PIC  X(0001).
           05  RETTOI PIC  X(0030).
      *    -------------------------------
           05  DPREML PIC S9(0004) COMP.
           05  DPREMF PIC  X(0001).
           05  FILLER REDEFINES DPREMF.
               10  DPREMA PIC  X(0001).
           05  DPREMI PIC  X(0005).
      *    -------------------------------
           05  PREML PIC S9(0004) COMP.
           05  PREMF PIC  X(0001).
           05  FILLER REDEFINES PREMF.
               10  PREMA PIC  X(0001).
           05  PREMI PIC  X(0008).
      *    -------------------------------
           05  DCOMML PIC S9(0004) COMP.
           05  DCOMMF PIC  X(0001).
           05  FILLER REDEFINES DCOMMF.
               10  DCOMMA PIC  X(0001).
           05  DCOMMI PIC  X(0005).
      *    -------------------------------
           05  ISSCOMML PIC S9(0004) COMP.
           05  ISSCOMMF PIC  X(0001).
           05  FILLER REDEFINES ISSCOMMF.
               10  ISSCOMMA PIC  X(0001).
           05  ISSCOMMI PIC  X(0008).
      *    -------------------------------
           05  DREFL PIC S9(0004) COMP.
           05  DREFF PIC  X(0001).
           05  FILLER REDEFINES DREFF.
               10  DREFA PIC  X(0001).
           05  DREFI PIC  X(0004).
      *    -------------------------------
           05  REFL PIC S9(0004) COMP.
           05  REFF PIC  X(0001).
           05  FILLER REDEFINES REFF.
               10  REFA PIC  X(0001).
           05  REFI PIC  X(0008).
      *    -------------------------------
           05  DUECOMML PIC S9(0004) COMP.
           05  DUECOMMF PIC  X(0001).
           05  FILLER REDEFINES DUECOMMF.
               10  DUECOMMA PIC  X(0001).
           05  DUECOMMI PIC  X(0007).
      *    -------------------------------
           05  UECOMML PIC S9(0004) COMP.
           05  UECOMMF PIC  X(0001).
           05  FILLER REDEFINES UECOMMF.
               10  UECOMMA PIC  X(0001).
           05  UECOMMI PIC  X(0008).
      *    -------------------------------
           05  DPREPDL PIC S9(0004) COMP.
           05  DPREPDF PIC  X(0001).
           05  FILLER REDEFINES DPREPDF.
               10  DPREPDA PIC  X(0001).
           05  DPREPDI PIC  X(0006).
      *    -------------------------------
           05  PREPDL PIC S9(0004) COMP.
           05  PREPDF PIC  X(0001).
           05  FILLER REDEFINES PREPDF.
               10  PREPDA PIC  X(0001).
           05  PREPDI PIC  X(0008).
      *    -------------------------------
           05  AMOUNTL PIC S9(0004) COMP.
           05  AMOUNTF PIC  X(0001).
           05  FILLER REDEFINES AMOUNTF.
               10  AMOUNTA PIC  X(0001).
           05  AMOUNTI PIC  S9(9)V99.
      *    -------------------------------
           05  CHECKL PIC S9(0004) COMP.
           05  CHECKF PIC  X(0001).
           05  FILLER REDEFINES CHECKF.
               10  CHECKA PIC  X(0001).
           05  CHECKI PIC  X(0007).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0001).
      *    -------------------------------
           05  REASONL PIC S9(0004) COMP.
           05  REASONF PIC  X(0001).
           05  FILLER REDEFINES REASONF.
               10  REASONA PIC  X(0001).
           05  REASONI PIC  X(0025).
      *    -------------------------------
           05  VREASONL PIC S9(0004) COMP.
           05  VREASONF PIC  X(0001).
           05  FILLER REDEFINES VREASONF.
               10  VREASONA PIC  X(0001).
           05  VREASONI PIC  X(0024).
      *    -------------------------------
           05  STUBL PIC S9(0004) COMP.
           05  STUBF PIC  X(0001).
           05  FILLER REDEFINES STUBF.
               10  STUBA PIC  X(0001).
           05  STUBI PIC  X(0030).
      *    -------------------------------
           05  TEXT1L PIC S9(0004) COMP.
           05  TEXT1F PIC  X(0001).
           05  FILLER REDEFINES TEXT1F.
               10  TEXT1A PIC  X(0001).
           05  TEXT1I PIC  X(0050).
      *    -------------------------------
           05  TEXT2L PIC S9(0004) COMP.
           05  TEXT2F PIC  X(0001).
           05  FILLER REDEFINES TEXT2F.
               10  TEXT2A PIC  X(0001).
           05  TEXT2I PIC  X(0050).
      *    -------------------------------
           05  TEXT3L PIC S9(0004) COMP.
           05  TEXT3F PIC  X(0001).
           05  FILLER REDEFINES TEXT3F.
               10  TEXT3A PIC  X(0001).
           05  TEXT3I PIC  X(0040).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0079).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
      *    -------------------------------
           05  PF3L PIC S9(0004) COMP.
           05  PF3F PIC  X(0001).
           05  FILLER REDEFINES PF3F.
               10  PF3A PIC  X(0001).
           05  PF3I PIC  X(0016).
      *    -------------------------------
           05  PF57L PIC S9(0004) COMP.
           05  PF57F PIC  X(0001).
           05  FILLER REDEFINES PF57F.
               10  PF57A PIC  X(0001).
           05  PF57I PIC  X(0030).
      *    -------------------------------
           05  PF4L PIC S9(0004) COMP.
           05  PF4F PIC  X(0001).
           05  FILLER REDEFINES PF4F.
               10  PF4A PIC  X(0001).
           05  PF4I PIC  X(0018).
      *    -------------------------------
           05  PF6L PIC S9(0004) COMP.
           05  PF6F PIC  X(0001).
           05  FILLER REDEFINES PF6F.
               10  PF6A PIC  X(0001).
           05  PF6I PIC  X(0013).
       01  EL677AO REDEFINES EL677AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DMAINT1O PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DMAINT2O PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEQO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYTO1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREATEDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYTO2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYEEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APVSTATO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYAD1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APVDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APVBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYAD2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOIDEDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYCTYO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINTEDO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PTOZIPO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEDCYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CASHEDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETTOO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DPREMO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PREMO PIC  Z(5).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DCOMMO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ISSCOMMO PIC  Z(5).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DREFO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFO PIC  Z(5).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DUECOMMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UECOMMO PIC  Z(5).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DPREPDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PREPDO PIC  Z(5).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMOUNTO PIC  Z(7).ZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHECKO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REASONO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VREASONO PIC  X(0024).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STUBO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT1O PIC  X(0050).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT2O PIC  X(0050).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT3O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF3O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF57O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF4O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF6O PIC  X(0013).
      *    -------------------------------
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
00418  01  DFHCOMMAREA.
091615     05  filler                  PIC X(1024).
021714 01  var  pic x(30).
00421 *                                COPY ELCCNTL.
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
00423 *                                COPY ERCCHEK.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCHEK                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK RECORDS                             *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 600    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERCHEK             RKP=2,LEN=35          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
021414*                   C H A N G E   L O G
021414*
021414* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
021414*-----------------------------------------------------------------
021414*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
021414* EFFECTIVE    NUMBER
021414*-----------------------------------------------------------------
021414* 021414    2003053000001  PEMA  changes for auto chk request
021414******************************************************************
00018  01  CHECK-RECORDS.
00019      12  CH-RECORD-ID                      PIC XX.
00020          88  VALID-CH-ID                      VALUE 'CH'.
00021
00022      12  CH-CONTROL-PRIMARY.
00023          16  CH-COMPANY-CD                 PIC X.
00024          16  CH-CARRIER                    PIC X.
00025          16  CH-GROUPING                   PIC X(6).
00026          16  CH-STATE                      PIC XX.
00027          16  CH-ACCOUNT                    PIC X(10).
00028          16  CH-CERT-EFF-DT                PIC XX.
00029          16  CH-CERT-NO.
00030              20  CH-CERT-PRIME             PIC X(10).
00031              20  CH-CERT-SFX               PIC X.
00032          16  CH-SEQUENCE-NO                PIC S9(4)     COMP.
00033
00034      12  CH-RECORDED-DT                    PIC XX.
00035      12  CH-RECORDED-BY                    PIC X(4).
00036      12  CH-LAST-MAINT-HHMMSS              PIC S9(6)     COMP-3.
00037
00038      12  CH-AMOUNT-PAID                    PIC S9(7)V99  COMP-3.
00039      12  CH-CHECK-NO                       PIC X(7).
00040      12  CH-REASON-FOR-CHECK               PIC X(25).
00041      12  CH-CHECK-WRITTEN-DT               PIC XX.
00042      12  FILLER                            PIC X.
00044
00045      12  CH-PAYEE-INFO.
00046          16  CH-PAYEE-NAME-1               PIC X(30).
00047          16  CH-PAYEE-NAME-2               PIC X(30).
00048          16  CH-PAYEE-ADDRESS-1            PIC X(30).
00049          16  CH-PAYEE-ADDRESS-2            PIC X(30).
00050          16  CH-PAYEE-CITY-ST.
021414             20  CH-PAYEE-CITY             PIC X(28).
021414             20  CH-PAYEE-STATE            PIC XX.
00051          16  CH-PAYEE-ZIP-CODE.
00052              20  CH-PAYEE-ZIP.
00053                  24  CH-ZIP-PRI-1ST        PIC X.
00054                      88  CH-CANADIAN-POST-CODE
00055                                            VALUES 'A' THRU 'Z'.
00056                  24  FILLER                PIC X(4).
00057              20  CH-PAYEE-ZIP-EXT          PIC X(4).
00058          16  CH-CANADIAN-POSTAL-CODE REDEFINES CH-PAYEE-ZIP-CODE.
00059              20  CH-CAN-POSTAL-1           PIC XXX.
00060              20  CH-CAN-POSTAL-2           PIC XXX.
00061              20  FILLER                    PIC XXX.
00062
00063      12  CH-CHECK-STUB-TEXT.
00064          16  CH-STUB-LINE-1                PIC X(30).
00065          16  CH-TEXT-LINE-1                PIC X(50).
00066          16  CH-TEXT-LINE-2                PIC X(50).
00067          16  CH-TEXT-LINE-3                PIC X(40).
021414     12  CH-RETURN-TO                      PIC X(30).
00070
00071      12  CH-COMPENSATION-CONTROL.
00072          16  CH-COMP-CARRIER               PIC X.
00073          16  CH-COMP-GROUPING              PIC X(6).
00074          16  CH-COMP-FIN-RESP              PIC X(10).
00075          16  CH-COMP-ACCOUNT               PIC X(10).
00076
00077      12  CH-CREDIT-SELECT-DT               PIC XX.
00078      12  CH-CREDIT-ACCEPT-DT               PIC XX.
00079      12  CH-PAYEE-CODE                     PIC X(6).
00080
00081      12  CH-VOID-DATA.
00082          20  CH-VOID-DT                    PIC XX.
00083          20  CH-VOID-BY                    PIC X(4).
00084          20  CH-VOID-REASON                PIC X(25).
00085
021414     12  CH-APPROVAL-DATA.
021414         20  CH-APPROVAL-DT                PIC XX.
021414         20  CH-APPROVAL-STATUS            PIC X.
021414             88  CH-IN-LIMBO                  VALUE ' '.
021414             88  CH-APPROV-PENDING            VALUE 'P' '2'.
021414             88  CH-APPROVED                  VALUE 'A'.
021414             88  CH-DENIED                    VALUE 'D'.
021414         20  CH-APPROVED-BY                PIC XXXX.
00086      12  CH-CHECK-QUE-CONTROL              PIC S9(8)     COMP.
00087              88  PAYMENT-NOT-QUEUED           VALUE ZERO.
00088      12  CH-CHECK-QUE-SEQUENCE             PIC S9(4)     COMP.
00089
021414     12  ch-released-dt                    pic xx.
021414     12  ch-check-cashed-dt                pic xx.
           12  FILLER                            PIC X.
00090 *    12  CH-CHECK-REFERENCE                PIC X(12).
00091      12  CH-CHECK-ORIGIN-SW                PIC X.
00092              88  CH-REFUND-CHECK              VALUE 'R'.
00093              88  CH-MAINT-CHECK               VALUE 'M'.
00094
00095      12  CH-CANC-DT                        PIC XX.
00096      12  CH-LF-REFUND                      PIC S9(7)V99  COMP-3.
00097      12  CH-AH-REFUND                      PIC S9(7)V99  COMP-3.
00098
00099      12  CH-INSURED-NAME                   PIC X(28).
00100
021414     12  ch-released-by                    pic x(4).
021414     12  ch-csr                            pic x(4).
021414     12  ch-deduct-commission              pic x.
021414         88  ch-deduct-comm                  value 'Y'.
021414         88  ch-do-not-deduct-comm           value 'N'.
00103
           12  FILLER                            PIC X(11).
021414*    12  CH-LETTER-TABLE.
021414*        16  CH-LETTERS OCCURS 3 TIMES
021414*                       INDEXED BY CH-LT-NDX
021414*                                          PIC X(04).
00108
00109      12  FILLER                            PIC X(07).
00110
00427 *                                COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030211* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
101711* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
021916* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
030211     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
030211         16  FILLER                        PIC X(10).
030211         16  AM-VG-KEY3.
030211             20  AM-VG3-ACCOUNT            PIC X(10).
030211             20  AM-VG3-EXP-DT             PIC XX.
030211         16  FILLER                        PIC X(4).
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
031811         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
021916         88  AM-ACCOUNT-DROPPED               VALUE '6'.
021916         88  AM-ACCOUNT-LAPSED                VALUE '7'.
021916         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
021916         88  AM-ACCOUNT-PENDING               VALUE '9'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
           12  AM-DCC-UEF-STATE                  PIC XX.
           12  FILLER                            PIC XXX.
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
00429 *                                COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
071712* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
071712     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
071712         16  CO-OV120                      PIC S9(7)V99   COMP-3.
071712         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
00431 *                                COPY ELCCERT.
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
00433 *                                COPY ERCPNDB.
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
00435 *                                COPY ERCPNDM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDM                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING MAILING DATA                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERPNDM                 RKP=2,LEN=11      *
00013 *   ALTERNATE PATH    = NOT USED                                 *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
080406******************************************************************
080406*                   C H A N G E   L O G
080406*
080406* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080406*-----------------------------------------------------------------
080406*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080406* EFFECTIVE    NUMBER
080406*-----------------------------------------------------------------
080406* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
071108* 071108  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
100217* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
00017 ******************************************************************
00018
00019  01  PENDING-MAILING-DATA.
00020      12  PM-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'PM'.
00022
00023      12  PM-CONTROL-PRIMARY.
00024          16  PM-COMPANY-CD                 PIC X.
00025          16  PM-ENTRY-BATCH                PIC X(6).
00026          16  PM-BATCH-SEQ-NO               PIC S9(4)     COMP.
00027          16  PM-BATCH-CHG-SEQ-NO           PIC S9(4)     COMP.
00028
00029      12  FILLER                            PIC X(14).
00030
00031      12  PM-ACCESS-CONTROL.
00032          16  PM-SOURCE-SYSTEM              PIC XX.
00033              88  PM-FROM-CREDIT                VALUE 'CR'.
00034              88  PM-FROM-VSI                   VALUE 'VS'.
00035              88  PM-FROM-WARRANTY              VALUE 'WA'.
00036              88  PM-FROM-OTHER                 VALUE 'OT'.
00037          16  PM-RECORD-ADD-DT              PIC XX.
00038          16  PM-RECORD-ADDED-BY            PIC XXXX.
00039          16  PM-LAST-MAINT-DT              PIC XX.
00040          16  PM-LAST-MAINT-BY              PIC XXXX.
00041          16  PM-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00042
00043      12  PM-PROFILE-INFO.
00044          16  PM-QUALIFY-CODE-1             PIC XX.
00045          16  PM-QUALIFY-CODE-2             PIC XX.
00046          16  PM-QUALIFY-CODE-3             PIC XX.
00047          16  PM-QUALIFY-CODE-4             PIC XX.
00048          16  PM-QUALIFY-CODE-5             PIC XX.
00049
00050          16  PM-INSURED-LAST-NAME          PIC X(15).
00051          16  PM-INSURED-FIRST-NAME         PIC X(10).
00052          16  PM-INSURED-MIDDLE-INIT        PIC X.
00053          16  PM-INSURED-ISSUE-AGE          PIC 99.
00054          16  PM-INSURED-BIRTH-DT           PIC XX.
00055          16  PM-INSURED-SEX                PIC X.
00056              88  PM-SEX-MALE                   VALUE 'M'.
00057              88  PM-SEX-FEMALE                 VALUE 'F'.
00058          16  PM-INSURED-SOC-SEC-NO         PIC X(11).
00059
080406         16  PM-ADDRESS-CORRECTED          PIC X.
081108         16  PM-JOINT-BIRTH-DT             PIC XX.
00060 *        16  FILLER                        PIC X(12).
00061
00062          16  PM-ADDRESS-LINE-1             PIC X(30).
00063          16  PM-ADDRESS-LINE-2             PIC X(30).
00064          16  PM-CITY-STATE.
                   20  PM-CITY                   PIC X(28).
                   20  PM-STATE                  PIC XX.
00065          16  PM-ZIP.
00066              20  PM-ZIP-CODE.
00067                  24  PM-ZIP-1              PIC X.
00068                      88  PM-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00069                  24  FILLER                PIC X(4).
00070              20  PM-ZIP-PLUS4              PIC X(4).
00071          16  PM-CANADIAN-ZIP  REDEFINES  PM-ZIP.
00072              20  PM-CAN-POST1              PIC XXX.
00073              20  PM-CAN-POST2              PIC XXX.
00074              20  FILLER                    PIC XXX.
00075
00076          16  PM-PHONE-NO                   PIC 9(11)       COMP-3.
100217         16  pm-city-st-zip-verified       pic x.
100217         16  FILLER                        PIC XX.
00079
           12  PM-CRED-BENE-INFO.
CIDMOD         16  PM-CRED-BENE-NAME             PIC X(25).
CIDMOD         16  PM-CRED-BENE-ADDR             PIC X(30).
071108         16  PM-CRED-BENE-ADDR2            PIC X(30).
CIDMOD         16  PM-CRED-BENE-CTYST.
                   20  PM-CRED-BENE-CITY         PIC X(28).
                   20  PM-CRED-BENE-STATE        PIC XX.
CIDMOD         16  PM-CRED-BENE-ZIP.
CIDMOD             20  PM-CB-ZIP-CODE.
CIDMOD                 24  PM-CB-ZIP-1           PIC X.
CIDMOD                     88  PM-CB-CANADIAN-POST-CODE
                                        VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                PIC X(4).
CIDMOD             20  PM-CB-ZIP-PLUS4           PIC X(4).
CIDMOD         16  PM-CB-CANADIAN-ZIP  REDEFINES  PM-CRED-BENE-ZIP.
CIDMOD             20  PM-CB-CAN-POST1           PIC XXX.
CIDMOD             20  PM-CB-CAN-POST2           PIC XXX.
CIDMOD             20  FILLER                    PIC XXX.
080406     12  PM-POST-CARD-MAIL-DATA.
080406         16  PM-MAIL-DATA OCCURS 7.
080406             20  PM-MAIL-TYPE              PIC X.
080406                 88  PM-12MO-MAILING           VALUE '1'.
080406                 88  PM-EXP-MAILING            VALUE '2'.
080406             20  PM-MAIL-STATUS            PIC X.
080406                 88  PM-MAIL-ST-MAILED         VALUE '1'.
080406                 88  PM-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  PM-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  PM-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC X(12).
080406*    12  FILLER                            PIC X(30).
00075
00081 ******************************************************************
00437 *                                COPY ERCMAIL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCMAIL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
00013 *   ALTERNATE PATH    = NOT USED                                 *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
080406******************************************************************
080406*                   C H A N G E   L O G
080406*
080406* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080406*-----------------------------------------------------------------
080406*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080406* EFFECTIVE    NUMBER
080406*-----------------------------------------------------------------
080406* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
111108* 111108                   PEMA  ADD CRED BENE ADDR2
00017 ******************************************************************
00018
00019  01  MAILING-DATA.
00020      12  MA-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'MA'.
00022
00023      12  MA-CONTROL-PRIMARY.
00024          16  MA-COMPANY-CD                 PIC X.
00025          16  MA-CARRIER                    PIC X.
00026          16  MA-GROUPING.
00027              20  MA-GROUPING-PREFIX        PIC XXX.
00028              20  MA-GROUPING-PRIME         PIC XXX.
00029          16  MA-STATE                      PIC XX.
00030          16  MA-ACCOUNT.
00031              20  MA-ACCOUNT-PREFIX         PIC X(4).
00032              20  MA-ACCOUNT-PRIME          PIC X(6).
00033          16  MA-CERT-EFF-DT                PIC XX.
00034          16  MA-CERT-NO.
00035              20  MA-CERT-PRIME             PIC X(10).
00036              20  MA-CERT-SFX               PIC X.
00037
00038      12  FILLER                            PIC XX.
00039
00040      12  MA-ACCESS-CONTROL.
00041          16  MA-SOURCE-SYSTEM              PIC XX.
00042              88  MA-FROM-CREDIT                VALUE 'CR'.
00043              88  MA-FROM-VSI                   VALUE 'VS'.
00044              88  MA-FROM-WARRANTY              VALUE 'WA'.
00045              88  MA-FROM-OTHER                 VALUE 'OT'.
00046          16  MA-RECORD-ADD-DT              PIC XX.
00047          16  MA-RECORD-ADDED-BY            PIC XXXX.
00048          16  MA-LAST-MAINT-DT              PIC XX.
00049          16  MA-LAST-MAINT-BY              PIC XXXX.
00050          16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00051
00052      12  MA-PROFILE-INFO.
00053          16  MA-QUALIFY-CODE-1             PIC XX.
00054          16  MA-QUALIFY-CODE-2             PIC XX.
00055          16  MA-QUALIFY-CODE-3             PIC XX.
00056          16  MA-QUALIFY-CODE-4             PIC XX.
00057          16  MA-QUALIFY-CODE-5             PIC XX.
00058
00059          16  MA-INSURED-LAST-NAME          PIC X(15).
00060          16  MA-INSURED-FIRST-NAME         PIC X(10).
00061          16  MA-INSURED-MIDDLE-INIT        PIC X.
00062          16  MA-INSURED-ISSUE-AGE          PIC 99.
00063          16  MA-INSURED-BIRTH-DT           PIC XX.
00064          16  MA-INSURED-SEX                PIC X.
00065              88  MA-SEX-MALE                   VALUE 'M'.
00066              88  MA-SEX-FEMALE                 VALUE 'F'.
00067          16  MA-INSURED-SOC-SEC-NO         PIC X(11).
00068
080406         16  MA-ADDRESS-CORRECTED          PIC X.
081108         16  MA-JOINT-BIRTH-DT             PIC XX.
00069 *        16  FILLER                        PIC X(12).
00070
00071          16  MA-ADDRESS-LINE-1             PIC X(30).
00072          16  MA-ADDRESS-LINE-2             PIC X(30).
00073          16  MA-CITY-STATE.
                   20  MA-CITY                   PIC X(28).
                   20  MA-ADDR-STATE             PIC XX.
00074          16  MA-ZIP.
00075              20  MA-ZIP-CODE.
00076                  24  MA-ZIP-CODE-1ST       PIC X(1).
00077                      88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00078                  24  FILLER                PIC X(4).
00079              20  MA-ZIP-PLUS4              PIC X(4).
00080          16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
00081              20  MA-CAN-POSTAL-CODE-1      PIC X(3).
00082              20  MA-CAN-POSTAL-CODE-2      PIC X(3).
00083              20  FILLER                    PIC X(3).
00084
00085          16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
00086
               16  FILLER                        PIC XXX.
00087 *        16  FILLER                        PIC X(10).
00088
           12  MA-CRED-BENE-INFO.
CIDMOD         16  MA-CRED-BENE-NAME                 PIC X(25).
CIDMOD         16  MA-CRED-BENE-ADDR                 PIC X(30).
               16  MA-CRED-BENE-ADDR2                PIC X(30).
CIDMOD         16  MA-CRED-BENE-CTYST.
                   20  MA-CRED-BENE-CITY             PIC X(28).
                   20  MA-CRED-BENE-STATE            PIC XX.
CIDMOD         16  MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-ZIP-CODE.
CIDMOD                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
CIDMOD                     88  MA-CB-CANADIAN-POST-CODE
                                                 VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                    PIC X(4).
CIDMOD             20  MA-CB-ZIP-PLUS4               PIC X(4).
CIDMOD         16  MA-CB-CANADIAN-POSTAL-CODE
                                  REDEFINES MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
CIDMOD             20  FILLER                        PIC X(3).
080406     12  MA-POST-CARD-MAIL-DATA.
080406         16  MA-MAIL-DATA OCCURS 7.
080406             20  MA-MAIL-TYPE              PIC X.
080406                 88  MA-12MO-MAILING           VALUE '1'.
080406                 88  MA-EXP-MAILING            VALUE '2'.
080406             20  MA-MAIL-STATUS            PIC X.
080406                 88  MA-MAIL-ST-MAILED         VALUE '1'.
080406                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  MA-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC XX.
080406*    12  FILLER                            PIC X(30).
00090 ******************************************************************
00441 *                                COPY ERCPYAJ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCPYAJ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.015                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING PAYMENT AND ADJUSTMENTS           *
00008 *                                                                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 200  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERPYAJ                         RKP=2,LEN=33   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
042303******************************************************************
042303*                   C H A N G E   L O G
042303*
042303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
042303*-----------------------------------------------------------------
042303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
042303* EFFECTIVE    NUMBER
042303*-----------------------------------------------------------------
042303* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
060205* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
042303******************************************************************
00019
00020  01  PENDING-PAY-ADJ.
00021      12  PY-RECORD-ID                     PIC XX.
00022          88  VALID-PY-ID                        VALUE 'PY'.
00023
00024      12  PY-CONTROL-PRIMARY.
00025          16  PY-COMPANY-CD                PIC X.
00026          16  PY-CARRIER                   PIC X.
00027          16  PY-GROUPING                  PIC X(6).
00028          16  PY-FIN-RESP                  PIC X(10).
00029          16  PY-ACCOUNT                   PIC X(10).
00030          16  PY-PRODUCER REDEFINES PY-ACCOUNT
00031                                           PIC X(10).
00032          16  PY-FILE-SEQ-NO               PIC S9(8)     COMP.
00033          16  PY-RECORD-TYPE               PIC X.
00034              88  PY-REMIT-RECEIVED            VALUE 'R'.
00035              88  PY-DEPOSIT                   VALUE 'D'.
00036              88  PY-CHARGE-TO-AGENT           VALUE 'C'.
00037              88  PY-ADJ-REM-RECEIVED          VALUE 'S'.
00038              88  PY-ADJ-DEPOSIT               VALUE 'T'.
00039              88  PY-ADJ-CHG-TO-AGT            VALUE 'U'.
00040              88  PY-ADD-TO-YTD-COMP           VALUE 'X'.
00041              88  PY-SUBTRACT-YTD-COMP         VALUE 'Y'.
00042              88  PY-ADD-TO-BALANCE            VALUE 'Z'.
00043              88  PY-FICA-ENTRY                VALUE 'F'.
00044              88  PY-REMIT-IND-GROUPING        VALUE 'G'.
00045              88  PY-POLICY-FEE                VALUE 'W'.
042303             88  PY-DUE-PREM-ADJ              VALUE 'P'.
00046
00047      12  PY-PYMT-TYPE                     PIC X.
00048              88  PY-NEW-BUS-PYMT              VALUE 'B'.
00049              88  PY-REINS-PYMT                VALUE 'R'.
00050              88  PY-EXP-PYMT                  VALUE 'E'.
00051
00052      12  PY-BIL-INV                       PIC X(6).
00053      12  PY-REF-NO                        PIC X(12).
00054
00055      12  PY-LAST-MAINT-DT                 PIC XX.
00056      12  PY-LAST-MAINT-BY                 PIC X(4).
00057      12  PY-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00058
00059      12  PY-PYADJ-RECORD.
00060          16  PY-ENTRY-AMT                 PIC S9(7)V99  COMP-3.
00061          16  PY-ENTRY-COMMENT             PIC X(30).
CIDMOD         16  PY-GL-DATA      REDEFINES PY-ENTRY-COMMENT.
CIDMOD             20  PY-GL-ACCOUNT            PIC X(10).
CIDMOD             20  PY-GL-STATE              PIC X(02).
CIDMOD             20  PY-GL-CANC-SW            PIC X(01).
CIDMOD                 88  PY-GL-CANC-SW-ON     VALUE 'Y'.
CIDMOD                 88  PY-GL-CANC-SW-OFF    VALUE 'N'.
CIDMOD             20  PY-GL-COMMENT            PIC X(10).
CIDMOD             20  FILLER      REDEFINES PY-GL-COMMENT.
CIDMOD                 24  PY-GL-CHECK-NO       PIC 9(06).
CIDMOD                 24  FILLER               PIC X(04).
CIDMOD             20  FILLER                   PIC X(07).
00074          16  PY-SAVE-ACCOUNT              PIC X(10).
00075          16  PY-SAVE-TYPE                 PIC X(01).
00076
00077          16  PY-LETTERS.
00078              20  PY-LETTER OCCURS 3 TIMES
00079                            INDEXED BY PY-LET-NDX
00080                                           PIC X(04).
00081
060205         16  PY-ERCOMP-TYPE               PIC X.
060205             88  PY-ACCOUNT-TYPE              VALUE 'A'.
060205             88  PY-GA-TYPE                   VALUE 'G'.
060205             88  PY-BANK-TYPE                 VALUE 'B'.
060205         16  FILLER                       PIC X(05).
00083
00084      12  PY-RECORD-STATUS.
00085          16  PY-CREDIT-SELECT-DT          PIC XX.
00086          16  PY-CREDIT-ACCEPT-DT          PIC XX.
00087          16  PY-BILLED-DATE               PIC XX.
00088          16  PY-REPORTED-DT               PIC XX.
00089          16  PY-PMT-APPLIED               PIC X.
00090              88  PY-ACCOUNT-PMT               VALUE 'A'.
00091              88  PY-GA-PMT                    VALUE 'G'.
00092              88  PY-OVWRITE-PMT               VALUE 'O'.
00093              88  PY-NON-AR-PMT                VALUE 'N'.
00094          16  FILLER                       PIC X(5).
00095          16  PY-INPUT-DT                  PIC XX.
00096          16  PY-CHECK-NUMBER              PIC X(6).
00097          16  PY-VOID-SW                   PIC X.
00098              88  PY-CHECK-VOIDED              VALUE 'V'.
00099          16  PY-CHECK-ORIGIN-SW           PIC X.
00100              88  PY-BILLING-CHECK             VALUE 'B'.
00101              88  PY-REFUND-CHECK              VALUE 'R'.
00102              88  PY-GA-CHECK                  VALUE 'G'.
00103              88  PY-CHECK-WRITTEN             VALUE 'W'.
00104              88  PY-CHECK-REVERSAL            VALUE 'V'.
00105          16  PY-CHECK-WRITTEN-DT          PIC XX.
00106          16  PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.
00107          16  PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.
00108          16  PY-BILL-FLAG                 PIC X.
00109              88  PY-BILLED                    VALUE 'B'.
00110          16  PY-AR-FLAG                   PIC X.
00111              88  PY-AR-CYCLE                  VALUE 'C'.
00112              88  PY-AR-MONTH-END              VALUE 'M'.
00113          16  PY-AR-DATE                   PIC XX.
00114
00115      12  PY-GL-CODES.
00116          16  PY-GL-DB                     PIC X(14).
00117          16  PY-GL-CR                     PIC X(14).
00118          16  PY-GL-FLAG                   PIC X.
00119          16  PY-GL-DATE                   PIC XX.
00120
00121      12  PY-CANCEL-FEE-FLAG               PIC X(2).
00122      12  FILLER                           PIC X(3).
00123 ******************************************************************
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA VAR CONTROL-FILE
                                CHECK-RECORDS ACCOUNT-MASTER
                                COMPENSATION-MASTER
                                CERTIFICATE-MASTER PENDING-BUSINESS
                                PENDING-MAILING-DATA MAILING-DATA
                                PENDING-PAY-ADJ.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL677' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00447
00448      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00449      MOVE '5'                    TO  DC-OPTION-CODE.
00450      PERFORM 8500-DATE-CONVERT.
00451      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00452      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00453      MOVE DC-GREG-DATE-1-MDY     TO  SAVE-CURRENT-DATE-MDY.
00454
00455      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00456
00457      IF PI-AMOUNT NOT NUMERIC
00458          MOVE ZEROS              TO  PI-AMOUNT.
00459
00460      IF EIBCALEN = 0
00461          GO TO 8800-UNAUTHORIZED-ACCESS.
021714     set P to address of KIXSYS
021714     CALL "getenv" using by value P returning var-ptr
021714     if var-ptr = null then
021714        display ' kixsys not set '
021714     else
021714        set address of var to var-ptr
021714        move 0 to env-var-len
021714        inspect var tallying env-var-len
021714          for characters before X'00'
021714        unstring var (1:env-var-len) delimited by '/'
021714           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
021714              WS-KIX-SYS
021714        end-unstring
021714     end-if
00463      MOVE 2                      TO  EMI-NUMBER-OF-LINES.
00464      MOVE 2                      TO  EMI-SWITCH2.
00465      MOVE EIBTRMID               TO  QID-TERM.
00466
00467      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00468          MOVE PI-CALLING-PROGRAM TO  RETURNED-FROM.
00469
00470      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00471          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00472              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00473              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00474              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00475              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00476              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00477              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00478              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00479              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00480          ELSE
00481              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00482              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00483              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00484              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00485              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00486              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00487              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00488              MOVE SPACES               TO  PI-SAVED-PROGRAM-6.
00489
00490      MOVE LOW-VALUES             TO  EL677AI.
00491
091615     if eibtrnid not = trans-id
091615        evaluate true
091615           when pi-return-to-program = 'EL6315'
091615              move pi-company-cd to pi-chek-comp-cd
091615              move pi-carrier    to pi-chek-carrier
091615              move pi-grouping   to pi-chek-grouping
091615              move pi-state      to pi-chek-state
091615              move pi-account    to pi-chek-account
091615              move pi-cert-eff-dt to pi-chek-eff-dt
091615              move pi-cert-prime to pi-chek-cert-no
091615              move pi-cert-sfx   to pi-chek-suf-no
091615              move +1            to pi-chek-sequence
091615              move 'C'           to pi-check-type *> Correction
091615              move 'S'           to mainti
091615              move +1            to maintl
091615              move al-uanon      to mainta
091615              move dfhenter      to eibaid
091615              go to 5000-browse-file
091615           when pi-return-to-program = 'EL6318'
091615              move 'C'           to pi-check-type *> Correction
091615           when other
091615              move 'R'           to pi-check-type *> Refund
091615        end-evaluate
091615     end-if
00492      IF EIBTRNID NOT = TRANS-ID
               move pi-processor-id    to pi-return-to
091615         move +1                 to dedcynl
               move 'N'                to dedcyno
               move al-uanon           to dedcyna
091615         move dfhpf7             to eibaid
111418         move ' '                to pi-void-reissue-pass
111418         move zeros              to pi-void-reissue-amt
091615         perform 6800-browse-previous-chek-recs *> totals up prev
                                       thru 6800-exit
091615         perform 5600-get-erpndb thru 5600-exit *> calc refund on
                                                      *> pending record
00493          IF PI-TO-EL677-FROM-EL1273
00494              MOVE 'S'            TO MAINTI
00495              MOVE 1              TO MAINTL
00496              MOVE AL-UABON       TO MAINTA
00497              MOVE DFHENTER       TO EIBAID
00498              GO TO 5000-BROWSE-FILE.
00499
00500      IF EIBTRNID NOT = TRANS-ID
00501         IF RETURNED-FROM = XCTL-1273 OR XCTL-114
00502            PERFORM 7200-RECOVER-TEMP-STORAGE THRU 7200-EXIT
00503            PERFORM 1500-VERIFY-PENDING-BUS-REC THRU 1590-EXIT
00504            IF PI-PROCESS-BENEFICIARY
00505               GO TO 6600-BUILD-BENEFICIARY-SCREEN
00506            ELSE
00507               GO TO 6300-BUILD-CERT-SCREEN
                 end-if
00508         ELSE
091615           IF EIBTRNID NOT = EL6311-TRANS-ID and el6318-trans-id
00510               GO TO 8100-SEND-INITIAL-MAP
00511            ELSE
00512               PERFORM 1500-VERIFY-PENDING-BUS-REC THRU 1590-EXIT
00513               IF PI-PROCESS-BENEFICIARY
00514                  GO TO 6600-BUILD-BENEFICIARY-SCREEN
00515               ELSE
00516                  GO TO 6300-BUILD-CERT-SCREEN
                    end-if
                 end-if
              end-if
            end-if
00517
00518      MOVE PI-COMPANY-CD          TO  CHEK-COMPANY-CD
00519                                      PI-CHEK-COMP-CD.
00520
00521      
      * EXEC CICS HANDLE CONDITION
00522 *        PGMIDERR  (9600-PGMID-ERROR)
00523 *        ERROR     (9990-ABEND)
00524 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00005844' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035383434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00525
00526      IF EIBAID = DFHCLEAR
00527          GO TO 9400-CLEAR.
00528
00529      IF PI-PROCESSOR-ID = 'LGXX'
00530          GO TO 0200-RECEIVE.
00531
00532      
      * EXEC CICS READQ TS
00533 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00534 *        INTO   (SECURITY-CONTROL)
00535 *        LENGTH (SC-COMM-LENGTH)
00536 *        ITEM   (SC-ITEM)
00537 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00005855' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00538
00539      MOVE SC-CREDIT-DISPLAY (17)  TO PI-DISPLAY-CAP.
00540      MOVE SC-CREDIT-UPDATE  (17)  TO PI-MODIFY-CAP.
00541
00542      IF NOT DISPLAY-CAP
00543          MOVE 'READ'          TO SM-READ
00544          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00545          MOVE ER-0070         TO  EMI-ERROR
00546          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00547          GO TO 8100-SEND-INITIAL-MAP.
00548
00551  0200-RECEIVE.
00552      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00553          MOVE ER-1159            TO  EMI-ERROR
00554          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00555          MOVE -1                 TO  PFENTERL
00556          GO TO 8200-SEND-DATAONLY.
00557
00558      
      * EXEC CICS HANDLE CONDITION
00559 *        MAPFAIL (8100-SEND-INITIAL-MAP)
00560 *    END-EXEC.
      *    MOVE '"$?                   ! # #00005879' TO DFHEIV0
           MOVE X'22243F202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035383739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00561
00562      
      * EXEC CICS RECEIVE
00563 *        MAP      (EL677A)
00564 *        MAPSET   (MAPSET-NAME)
00565 *        INTO     (EL677AI)
00566 *    END-EXEC.
           MOVE LENGTH OF
            EL677AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005883' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL677A, 
                 EL677AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00567
00568      IF PFENTERL GREATER ZERO
00569          IF EIBAID NOT = DFHENTER
00570              MOVE ER-0004        TO  EMI-ERROR
00571              MOVE AL-UNBOF       TO  PFENTERA
00572              MOVE -1             TO  PFENTERL
00573              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00574              GO TO 8200-SEND-DATAONLY
00575          ELSE
00576              IF (PFENTERI NUMERIC) AND
00577                 (PFENTERI GREATER 0 AND LESS 25)
00578                  MOVE PF-VALUES (PFENTERI)   TO  EIBAID
00579              ELSE
00580                  MOVE ER-0029    TO  EMI-ERROR
00581                  MOVE AL-UNBOF   TO  PFENTERA
00582                  MOVE -1         TO  PFENTERL
00583                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00584                  GO TO 8200-SEND-DATAONLY.
00587  0300-CHECK-PFKEYS.
00588      IF EIBAID = DFHPF23
00589          GO TO 8810-PF23.
00590
00591      IF EIBAID = DFHPF24
00592          GO TO 9200-RETURN-MAIN-MENU.
00593
00594      IF EIBAID = DFHPF12
00595          GO TO 9500-PF12.
00596
00597      IF EIBAID = DFHPF1 OR DFHPF2
00598          GO TO 5000-BROWSE-FILE.
00599
           if PI-TO-EL677-FROM-EL1273
              and (eibaid not = dfhenter)
111418        and (pi-void-reissue-pass not = '1' and '2')
              go to 0320-input-error
           end-if
00600      IF EIBAID = DFHPF3
00601          PERFORM 7000-SET-PI-AREA         THRU 7090-EXIT
00602          MOVE 'PF3'              TO  PI-PFKEY
00603          PERFORM 7100-CREATE-TEMP-STORAGE THRU 7100-EXIT
00604          IF PI-RETURN-TO-PROGRAM IS EQUAL TO 'EL1273  '
00605              GO TO 9400-CLEAR
00606          ELSE
00607              MOVE XCTL-1273      TO  PGM-NAME
00608              GO TO 9300-XCTL.
00609
00610      IF EIBAID = DFHPF4
00611          IF NOT FORCE-CAP
00612              MOVE ER-0433        TO EMI-ERROR
00613              MOVE -1             TO PFENTERL
00614              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00615              GO TO 8200-SEND-DATAONLY.
00616
00617      IF EIBAID = DFHENTER OR DFHPF1 OR DFHPF2 OR DFHPF4 OR
00618                              DFHPF5 OR DFHPF6 or dfhpf7
00619          GO TO 1000-EDIT-DATA.
00620
00621 *    IF EIBAID = DFHPF7
00622 *        PERFORM 7000-SET-PI-AREA         THRU 7090-EXIT
00623 *        PERFORM 7100-CREATE-TEMP-STORAGE THRU 7100-EXIT
00624 *        MOVE XCTL-114           TO  PGM-NAME
00625 *        GO TO 9300-XCTL.
00626
00627  0320-INPUT-ERROR.
00628      MOVE ER-0029                TO  EMI-ERROR.
00629      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00630      MOVE -1                     TO  PFENTERL.
00631      GO TO 8200-SEND-DATAONLY.
00632      EJECT
00633
00634  1000-EDIT-DATA.
111219     IF MAINTI = 'S' OR 'C' OR 'A' OR 'V' OR 'D' or 'R' or 'X'
00636          MOVE AL-UANON           TO MAINTA
111418         IF MAINTI = 'C' OR 'V' OR 'R'
111418             IF PI-PREV-MAINT = 'S'  OR  'C' or 'R'
00639                  NEXT SENTENCE
00640              ELSE
00641                  MOVE ER-2056    TO  EMI-ERROR
00642                  MOVE -1         TO  MAINTL
00643                  MOVE AL-UABON   TO  MAINTA
00644                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00645                  GO TO 1100-EDIT-COMPLETE
00646          ELSE
00647              NEXT SENTENCE
00648      ELSE
00649          IF EIBAID = DFHPF5 OR DFHPF6 or dfhpf7
00650             NEXT SENTENCE
00651          ELSE
00652             MOVE ER-0023            TO  EMI-ERROR
00653             MOVE -1                 TO  MAINTL
00654             MOVE AL-UABON           TO  MAINTA
00655             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00656
00657      IF MODIFY-CAP
00658          NEXT SENTENCE
00659        ELSE
00660          IF MAINTI NOT = 'S'
00661             MOVE 'UPDATE'       TO SM-READ
00662             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00663             MOVE ER-0070        TO EMI-ERROR
00664             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00665             GO TO 8100-SEND-INITIAL-MAP.
00666
           if (pi-return-to-program = 'EL1273')
111219        and (MAINTI not = 'S' and 'V' and 'R' and 'D' and 'X')
              move 'S'                 to mainti
              move +1                  to maintl
              move al-uanon            to mainta
           end-if
00667      IF CARRIERI NOT = LOW-VALUES
00668          MOVE AL-UANON           TO  CARRIERA
00669          PERFORM 1200-VERIFY-CARRIER-ID THRU 1200-EXIT
00670          MOVE CARRIERI           TO  PI-CHEK-CARRIER
00671                                      PI-CARRIER
00672      ELSE
00673          MOVE -1                 TO  CARRIERL
00674          MOVE AL-UABON           TO  CARRIERA
00675          MOVE ER-0194            TO  EMI-ERROR
00676          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00677
00678      IF GROUPI NOT = LOW-VALUES
00679          MOVE AL-UANON           TO  GROUPA
00680          MOVE GROUPI             TO  PI-CHEK-GROUPING
00681                                      PI-GROUPING
00682      ELSE
00683          MOVE -1                 TO  GROUPL
00684          MOVE AL-UABON           TO  GROUPA
00685          MOVE ER-0195            TO  EMI-ERROR
00686          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00687
00688      IF STATEI NOT = LOW-VALUES
00689          MOVE AL-UANON           TO  STATEA
00690          PERFORM 1300-VERIFY-STATE-ID THRU 1390-EXIT
00691          MOVE STATEI             TO  PI-CHEK-STATE
00692                                      PI-STATE
00693      ELSE
00694          MOVE -1                 TO  STATEL
00695          MOVE AL-UABON           TO  STATEA
00696          MOVE ER-0196            TO  EMI-ERROR
00697          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00698
00699      IF EFFDTL GREATER ZERO
CIDMOD         MOVE EFFDTI             TO  WS-DT-DEEDIT-FIELD
CIDMOD         PERFORM 8700-DEEDIT
CIDMOD         IF WS-DEEDIT-FIELD-DATE-OUT IS NUMERIC
CIDMOD             MOVE WS-DEEDIT-FIELD-DATE-OUT TO  EFFDTO
CIDMOD             INSPECT EFFDTI CONVERTING SPACES TO '/'
CIDMOD             MOVE WS-DEEDIT-FIELD-DATE-OUT TO  DC-GREG-DATE-1-MDY
00707              MOVE '4'                TO  DC-OPTION-CODE
00708              PERFORM 8500-DATE-CONVERT
00709              IF DC-ERROR-CODE NOT = SPACES
00710                  MOVE ER-0215        TO  EMI-ERROR
00711                  PERFORM 9900-ERROR-FORMAT
00712                  MOVE -1             TO  EFFDTL
00713                  MOVE AL-UABON       TO  EFFDTA
00714                ELSE
00715                  MOVE AL-UANON       TO  EFFDTA
00716                  MOVE DC-BIN-DATE-1  TO  PI-CHEK-EFF-DT
00717                                          PI-CERT-EFF-DT
00718            ELSE
00719              MOVE ER-0215        TO  EMI-ERROR
00720              PERFORM 9900-ERROR-FORMAT
00721              MOVE -1             TO  EFFDTL
00722              MOVE AL-UABON       TO  EFFDTA
00723        ELSE
00724          MOVE ER-0216            TO  EMI-ERROR
00725          PERFORM 9900-ERROR-FORMAT
00726          MOVE -1                 TO  EFFDTL
00727          MOVE AL-UABOF           TO  EFFDTA.
00728
00729      IF ACCTI NOT = LOW-VALUES
00730          MOVE AL-UANON           TO  ACCTA
00731          PERFORM 1400-VERIFY-ACCOUNT THRU 1490-EXIT
00732          MOVE ACCTI              TO  PI-CHEK-ACCOUNT
00733                                      PI-ACCOUNT
00734      ELSE
00735          MOVE -1                 TO  ACCTL
00736          MOVE AL-UABON           TO  ACCTA
00737          MOVE ER-0197            TO  EMI-ERROR
00738          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00739
00740      IF CERTNOI NOT = LOW-VALUES
00741          MOVE AL-UANON           TO  CERTNOA
00742          MOVE CERTNOI            TO  PI-CHEK-CERT-NO
00743                                      PI-CERT-PRIME
00744      ELSE
00745          MOVE -1                 TO  CERTNOL
00746          MOVE AL-UABON           TO  CERTNOA
00747          MOVE ER-0203            TO  EMI-ERROR
00748          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00749
00750      IF SFXI  = LOW-VALUES
00751          MOVE SPACE              TO  SFXI.
00752
00753      MOVE AL-UANON               TO  SFXA.
00754
00755      MOVE SFXI                   TO  PI-CHEK-SUF-NO
00756                                      PI-CERT-SFX.
00757      IF SEQI = LOW-VALUES
00758          MOVE +1                 TO  SEQI.
00759
00760      MOVE AL-UNNON               TO  SEQA.
00761      MOVE SEQI                   TO  PI-CHEK-SEQUENCE.
00762
00763      IF PAYTO1L > 0
00764          MOVE PAYTO1I            TO  PI-PAYTO1
00765      ELSE
00766          MOVE SPACES             TO  PI-PAYTO1.
00767
00768      IF AMOUNTL > 0
00769          
      * EXEC CICS BIF DEEDIT
00770 *            FIELD   (AMOUNTI)
00771 *            LENGTH  (11)
00772 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006098' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AMOUNTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00773          MOVE AMOUNTI            TO  PI-AMOUNT
00774      ELSE
00775          MOVE ZEROS              TO  PI-AMOUNT.
00776
00777      IF TYPEL > 0
00778          MOVE TYPEI              TO  PI-TYPE
00779      ELSE
00780          MOVE SPACE              TO  PI-TYPE.
00781
00782 *    IF REFL > 0
00783 *        MOVE REFI               TO  PI-REFERENCE
00784 *    ELSE
00785 *        MOVE SPACES             TO  PI-REFERENCE.
           if rettol not = zeros
              move rettoi              to pi-return-to
00753         MOVE AL-UANON            TO  RETTOA
           end-if
           if dedcynl not = zeros
              if dedcyni = 'Y' OR 'N'
                 move al-uanon         to dedcyna
                 continue
              else
                 move er-3451          to emi-error
                 move -1               to dedcynl
                 move al-uabon         to dedcyna
                 perform 9900-error-format
                                       thru 9900-exit
              end-if
           end-if
00786
111418     if mainti = 'V' or 'R'
              if ((vreasonl > zeros)
                 and (vreasoni (1:2) not = spaces))
111418             or (pi-void-reissue-pass = '1' or '2')
                 continue
              else
                 move er-3454          to emi-error
                 move -1               to vreasonl
                 move al-uabon         to vreasona
                 perform 9900-error-format
                                       thru 9900-exit
              end-if
           end-if
091615     if (mainti = 'A')
091615        and (pi-prev-paid > zeros)
091615        and (pi-check-type = 'C')
091615        move er-3459             to emi-error
091615        move -1                  to maintl
091615        move al-uabon            to mainta
091615        perform 9900-error-format
091615                                 thru 9900-exit
091615     end-if
           if mainti = 'A'
              continue
           else
00815         IF MAINTI = 'S'
                 or pi-prev-maint = 'S'
00816            GO TO 1100-EDIT-COMPLETE
              end-if
           end-if
111418     if (pi-void-reissue-pass = '1')
111418        and (mainti = 'R')
111418        if emi-error = zeros
111418           if eibaid = dfhpf5 or dfhpf6 or dfhpf7
111418              go to 7500-reissue-pass-1
111418           else
111418              if eibaid = dfhenter
111418                 move er-3460    to emi-error
111418                 move -1         to maintl
111418                 move al-uabon   to mainta
111418                 perform 9900-error-format
111418                                 thru 9900-exit
111418              end-if
111418           end-if
111418        end-if
111418     end-if
111418
111418     if (pi-void-reissue-pass = '2')
111418        and (mainti = 'R')
111418        if emi-error = zeros
111418           if eibaid = dfhpf5 or dfhpf6 or dfhpf7
111418              go to 7500-reissue-pass-1
111418           else
111418              if eibaid = dfhenter
111418                 go to 7510-reissue-pass-2
111418              end-if
111418           end-if
111418        end-if
111418     end-if
      *    if (pi-void-reissue-pass = '1')
      *       and (mainti = 'R')
      *       if emi-error = zeros
      *          if eibaid = dfhpf5 or dfhpf6 or dfhpf7
      *             go to 7500-reissue-pass-1
      *          else
      *             if eibaid = dfhenter
      *                go to 7510-reissue-pass-2
      *                move '2'        to pi-void-reissue-pass
      *             end-if
      *          end-if
      *       end-if
      *    end-if
00787      IF EIBAID = DFHPF5
00788         IF EMI-ERROR = ZEROS
00789           GO TO 6500-BUILD-ACCOUNT-SCREEN
00790         ELSE
00791           GO TO 1100-EDIT-COMPLETE.
00792
00793      IF EIBAID = DFHPF6
00794         IF EMI-ERROR = ZEROS
00795            GO TO 6300-BUILD-CERT-SCREEN
00796          ELSE
00797            GO TO 1100-EDIT-COMPLETE.
00798
00793      IF EIBAID = DFHPF7
00794         IF EMI-ERROR = ZEROS
00795            GO TO 6600-build-beneficiary-screen
00796          ELSE
00797            GO TO 1100-EDIT-COMPLETE.
020317     if mainti = 'A'
020317        if dedcynl not = zeros
020317           if dedcyni <> pi-previous-deduct-comm
020317              perform 6700-build-common
020317                                 thru 6700-exit
020317              MOVE WS-REFUND-AMOUNT
020317                                 TO AMOUNTO
020317              MOVE AL-UANON      TO PAYTO1A
020317                                    PAYTO2A
020317                                    PAYAD1A
020317                                    PAYCTYA
020317                                    paysta
020317                                    PTOZIPA
020317              GO TO 8100-send-initial-map
020317           end-if
020317        end-if
020317     end-if
      *    if mainti = 'V'
      *       if (vreasonl > zeros)
      *          and (vreasoni (1:2) not = spaces)
      *          continue
      *       else
      *          move er-3454          to emi-error
      *          move -1               to vreasonl
      *          move al-uabon         to vreasona
      *          perform 9900-error-format
      *                                thru 9900-exit
      *       end-if
      *    end-if
00799      IF MAINTI = 'A'
              if (payto1i not = spaces and low-values)
                 and (payad1i not = spaces and low-values)
                 and (payctyi not = spaces and low-values)
                 and (paysti not = spaces and low-values)
                 and (ptozipi not = spaces and low-values)
                 continue
              else
00806            MOVE -1               TO PAYTO1L
00807            MOVE ER-2907          TO EMI-ERROR
00808            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              end-if
           end-if
00809
00810      IF MAINTI = 'A'
00811          IF EIBAID NOT = DFHPF4
00812              MOVE DFHENTER       TO  EIBAID
00813              PERFORM 6100-VERIFY-CERTIFICATE THRU 6190-EXIT.
00814
111219     IF MAINTI = 'S' OR 'V' or 'R' or 'D' or 'X'
00816          GO TO 1100-EDIT-COMPLETE.
00817
00831      IF PRINTEDL GREATER THAN +0
CIDMOD         MOVE PRINTEDI           TO  WS-DEEDIT-FIELD-A
CIDMOD         PERFORM 8700-DEEDIT
CIDMOD         IF WS-DEEDIT-FIELD-V0 IS NUMERIC
CIDMOD             MOVE WS-DEEDIT-FIELD-V0 TO  PRINTEDO
CIDMOD             INSPECT PRINTEDI CONVERTING SPACES TO '/'
CIDMOD             MOVE WS-DEEDIT-FIELD-V0 TO  DC-GREG-DATE-1-MDY
00839              MOVE '4'                TO  DC-OPTION-CODE
00840              PERFORM 8500-DATE-CONVERT
00841              IF DC-ERROR-CODE NOT = SPACES
00842                  MOVE ER-3046        TO  EMI-ERROR
00843                  PERFORM 9900-ERROR-FORMAT
00844                  MOVE -1             TO  PRINTEDL
00845                  MOVE AL-UABON       TO  PRINTEDA
00846                  MOVE LOW-VALUES     TO  WS-CHK-PRINT-DT
00847              ELSE
00848                  MOVE AL-UANON       TO  PRINTEDA
00849                  MOVE DC-BIN-DATE-1  TO  WS-CHK-PRINT-DT
00850          ELSE
00851              MOVE ER-3046        TO  EMI-ERROR
00852              PERFORM 9900-ERROR-FORMAT
00853              MOVE -1             TO  PRINTEDL
00854              MOVE AL-UABON       TO  PRINTEDA
00855              MOVE LOW-VALUES     TO  WS-CHK-PRINT-DT.
00856
00864      IF AMOUNTL = ZEROS
00865          GO TO 1050-CK-AMT.
00866
00867 *    EXEC CICS BIF DEEDIT
00868 *        FIELD   (AMOUNTI)
00869 *        LENGTH  (11)
00870 *    END-EXEC.
00871
00872      IF AMOUNTI NOT NUMERIC     OR
00873         AMOUNTI LESS THAN ZERO  OR
00874        (AMOUNTI = ZEROS  AND
00875                (PI-COMPANY-ID NOT = 'LAP' AND 'RMC'))
00876           MOVE ER-1159            TO  EMI-ERROR
00877           MOVE -1                 TO  AMOUNTL
00878           MOVE AL-UNBON           TO  AMOUNTA
00879           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00880           GO TO 1100-EDIT-COMPLETE.
00881
00882      MOVE AMOUNTI                TO  WS-AM-WORK.
00883
00884      IF WS-AM-SIGN = SPACES
00885          MOVE WS-AM-NUM          TO  WS-AMT
00886          ADD 0                   TO  WS-AMT
00887        ELSE
00888          MOVE AMOUNTI            TO  WS-AMT
00889          ADD 0                   TO  WS-AMT.
00890
020317     IF MAINTI = 'A'
020317        IF EIBAID = DFHENTER
020317           if (ws-amt + pi-prev-paid) >
020317              (cm-lf-premium-amt + cm-lf-alt-premium-amt +
020317                 cm-ah-premium-amt)
020317              move zeros to ws-amt
020317           end-if
020317        end-if
020317     end-if
00891       MOVE WS-AMT                TO  AMOUNTO.
00892       MOVE AL-UNNON              TO  AMOUNTA.
00893
00894  1050-CK-AMT.
00895      IF AMOUNTL = ZEROS AND
00896         MAINTI  = 'A'
00897          MOVE ER-1159            TO  EMI-ERROR
00898          MOVE -1                 TO  AMOUNTL
00899          MOVE AL-UNBON           TO  AMOUNTA
00900          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00901
091615*    MOVE AL-UANON              TO  TYPEA.
020317     if (amountl <> zeros)
020317        if (ws-amt <= zeros)
020317           and (pi-check-type <> 'C')
020317           MOVE ER-9999          TO EMI-ERROR
020317           MOVE -1               TO AMOUNTL
020317           MOVE AL-UNBON         TO AMOUNTA
020317           PERFORM 9900-ERROR-FORMAT
020317                                 THRU 9900-EXIT
020317        end-if
020317     end-if
00904      IF TYPEL GREATER THAN ZEROS
091615         IF TYPEI = 'R' OR 'M' OR 'E' or 'C'
00906              NEXT SENTENCE
00907          ELSE
00908              MOVE ER-3044    TO  EMI-ERROR
00909              MOVE -1         TO  TYPEL
091615             MOVE AL-PABON   TO  TYPEA
00911              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00912      ELSE
00913          IF MAINTI  = 'A'
00914              MOVE ER-3044    TO  EMI-ERROR
00915              MOVE -1         TO  TYPEL
091615             MOVE AL-PABON   TO  TYPEA
00917              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00918
01000
01001  1100-EDIT-COMPLETE.
01002
01003      IF EMI-FATAL-CTR GREATER THAN +0  OR
01004        (EMI-FORCABLE-CTR GREATER THAN +0  AND
01005         EIBAID NOT = DFHPF4)
01006          GO TO 8200-SEND-DATAONLY.
01007
01008      MOVE MAINTI                 TO  PI-PREV-MAINT.
01009
01010      IF MAINTI = 'A'
01011          GO TO 2000-ADD-RECORD.
01012
01013 *    IF MAINTI = 'C' OR 'V'
111219     IF MAINTI = 'V' or 'R' or 'X'
01014          GO TO 3000-CHANGE-RECORD.
01015
01016      IF MAINTI = 'S' or 'C'
01017          GO TO 5000-BROWSE-FILE.
01018
           if mainti = 'D'
              go to 3400-delete-record
           end-if
           .
01021  1200-VERIFY-CARRIER-ID.
01022      MOVE SPACES                 TO ELCNTL-KEY.
01023      MOVE PI-COMPANY-ID          TO ELCNTL-COMP-ID.
01024      MOVE '6'                    TO ELCNTL-REC-TYPE.
01025      MOVE CARRIERI               TO ELCNTL-CARRIER.
01026      MOVE +0                     TO ELCNTL-SEQ.
01027
01028      
      * EXEC CICS HANDLE CONDITION
01029 *        NOTFND   (1290-NO-CARRIER)
01030 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00006401' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303036343031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01031
01032      
      * EXEC CICS READ
01033 *        DATASET   (ELCNTL-FILE-ID)
01034 *        SET       (ADDRESS OF CONTROL-FILE)
01035 *        RIDFLD    (ELCNTL-KEY)
01036 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006405' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343035' TO DFHEIV0(25:11)
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
           
01037
01038      GO TO 1200-EXIT.
01039
01040  1290-NO-CARRIER.
01041      MOVE ER-2208                TO EMI-ERROR.
01042      MOVE -1                     TO CARRIERL.
01043      MOVE AL-UABON               TO CARRIERA.
01044      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01045
01046  1200-EXIT.
01047       EXIT.
01048      EJECT
01078  1300-VERIFY-STATE-ID.
01079      MOVE SPACES                 TO ELCNTL-KEY.
01080      MOVE PI-COMPANY-ID          TO ELCNTL-COMP-ID.
01081      MOVE '3'                    TO ELCNTL-REC-TYPE.
01082      MOVE STATEI                 TO ELCNTL-STATE.
01083      MOVE +0                     TO ELCNTL-SEQ.
01084
01085      
      * EXEC CICS HANDLE CONDITION
01086 *        NOTFND   (1380-NO-STATE)
01087 *    END-EXEC.
      *    MOVE '"$I                   ! % #00006429' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303036343239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01088
01089      
      * EXEC CICS READ
01090 *        DATASET   (ELCNTL-FILE-ID)
01091 *        SET       (ADDRESS OF CONTROL-FILE)
01092 *        RIDFLD    (ELCNTL-KEY)
01093 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006433' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343333' TO DFHEIV0(25:11)
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
           
01094
01095      GO TO 1390-EXIT.
01096
01097  1380-NO-STATE.
01098      MOVE ER-2209                TO EMI-ERROR.
01099      MOVE -1                     TO STATEL.
01100      MOVE AL-UABON               TO STATEA.
01101      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01102
01103  1390-EXIT.
01104       EXIT.
01105      EJECT
01106
01107  1400-VERIFY-ACCOUNT.
01108      IF EIBAID = DFHPF5
01109          GO TO 1410-BUILD-ACCOUNT-KEY.
01110
01111      IF MAINTL GREATER +0
01112          NEXT SENTENCE
01113        ELSE
01114          GO TO 1490-EXIT.
01115
01116 *    IF MAINTI = 'A' OR 'C' OR 'V'
111418     IF MAINTI = 'A' OR 'V' or 'R'
01117          NEXT SENTENCE
01118        ELSE
01119          GO TO 1490-EXIT.
01120
01121  1410-BUILD-ACCOUNT-KEY.
01122      MOVE CARRIERI               TO ERACCT-CARRIER.
01123      MOVE GROUPI                 TO ERACCT-GROUPING
01124      MOVE STATEI                 TO ERACCT-STATE.
01125      MOVE ACCTI                  TO ERACCT-ACCOUNT.
01126      MOVE PI-COMPANY-CD          TO ERACCT-CO.
01127
01128      MOVE '2'                    TO DC-OPTION-CODE.
01129      MOVE EFFDTI                 TO DC-GREG-DATE-1-EDIT.
01130
01131      PERFORM 8500-DATE-CONVERT.
01132      IF DATE-CONVERSION-ERROR
01133         GO TO 1480-ACCOUNT-INVALID.
01134
01135      MOVE DC-BIN-DATE-1          TO ERACCT-EXP-DATE.
          .
       1420-get-eracct.
01137      MOVE ERACCT-KEY             TO SAVE-ERACCT-KEY.
01138
01139      
      * EXEC CICS HANDLE CONDITION
01140 *        NOTFND   (1480-ACCOUNT-INVALID)
01141 *        ENDFILE  (1480-ACCOUNT-INVALID)
01142 *    END-EXEC.
      *    MOVE '"$I''                  ! & #00006485' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303036343835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01143
01144      
      * EXEC CICS STARTBR
01145 *        DATASET   (ERACCT-FILE-ID)
01146 *        RIDFLD    (ERACCT-KEY)
01147 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006490' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01148
           set eracct-start to true
           .
01149  1420-READNEXT-ACCOUNT-MASTER.
01150      
      * EXEC CICS READNEXT
01151 *        DATASET   (ERACCT-FILE-ID)
01152 *        SET       (ADDRESS OF ACCOUNT-MASTER)
01153 *        RIDFLD    (ERACCT-KEY)
01154 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006498' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343938' TO DFHEIV0(25:11)
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
           
01155
01156 *    IF PI-COMPANY-CD    NOT = AM-COMPANY-CD   OR
01157 *       SV-ACCT-GROUPING NOT = AM-GROUPING     OR
01158 *       SV-ACCT-CARRIER  NOT = AM-CARRIER      OR
01159 *       SV-ACCT-STATE    NOT = AM-STATE        OR
01160 *       SV-ACCT-ACCOUNT  NOT = AM-ACCOUNT
01161 *         GO TO 1480-ACCOUNT-INVALID.
01162 *
01163 *    IF SV-ACCT-EXP-DATE LESS AM-EFFECTIVE-DT
01164 *        GO TO 1480-ACCOUNT-INVALID.
01165 *
01166 *    IF SV-ACCT-EXP-DATE NOT LESS AM-EXPIRATION-DT
01167 *        GO TO 1420-READNEXT-ACCOUNT-MASTER.
01168
           evaluate true
              when (save-eracct-key (1:20) not =
                 am-control-primary (1:20))
                 and (not eracct-found)
                 go to 1480-account-invalid
              when (save-eracct-key (1:20) not =
                 am-control-primary (1:20))
                 and (eracct-found)
                 move ws-hold-eracct-record
                                       to account-master
              when (sv-acct-exp-date < am-effective-dt)
                 and (not eracct-found)
                 go to 1480-account-invalid
              when sv-acct-exp-date >= am-expiration-dt
                 go to 1420-readnext-account-master
              when am-expiration-dt = high-values
                 continue
              when other
                 set eracct-found to true
                 move account-master   to ws-hold-eracct-record
                 go to 1420-readnext-account-master
           end-evaluate
           if eracct-start
              
      * exec cics endbr
      *          dataset  (eracct-file-id)
      *       end-exec
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006540' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 eracct-file-id, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
      *    if am-expiration-dt = high-values
      *       continue
      *    else
      *       move account-master      to ws-hold-eracct-record
      *       go to 1420-readnext-account-master
      *    end-if
01169      MOVE AM-CARRIER             TO PI-CR-CARRIER.
01170      MOVE AM-GROUPING            TO PI-CR-GROUPING.
01171      MOVE AM-STATE               TO PI-CR-STATE.
01172      MOVE AM-AGT (AM-REMIT-TO)   TO PI-CR-FIN-RESP.
01173
01174      MOVE AM-NAME                TO PI-AM-NAME.
01175      MOVE AM-ADDRS               TO PI-AM-ADDRS.
01176      MOVE AM-CITY                TO PI-AM-CITY-st
01177      MOVE AM-ZIP                 TO PI-AM-ZIP-CODE.
           move am-csr-code            to pi-am-csr
01178
01179      MOVE +0                     TO WS-SUB.
01180
01181  1430-FIND-ACC-AGT.
01182      ADD +1                      TO WS-SUB.
01183
01184      IF  WS-SUB GREATER +10
01185          GO TO 1480-ACCOUNT-INVALID.
01186
052814     IF (AM-COM-TYP (WS-SUB) = 'C' OR 'D' OR 'F')
01188          MOVE AM-AGT (WS-SUB)    TO  PI-CR-ACCOUNT
01189        ELSE
01190          GO TO 1430-FIND-ACC-AGT.
01191
01192      MOVE AL-UANON               TO CARRIERA
01193                                     GROUPA
01194                                     STATEA.
01195
01196      PERFORM 6000-VERIFY-COMP-MASTER THRU 6090-EXIT.
01197
01198      GO TO 1490-EXIT.
01199
01200  1480-ACCOUNT-INVALID.
01201      MOVE -1                     TO CARRIERL
01202      MOVE AL-UANON               TO CARRIERA
01203                                     GROUPA
01204                                     STATEA
01205                                     ACCTA.
01206      MOVE ER-2210                TO EMI-ERROR.
01207      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01208      GO TO 8200-SEND-DATAONLY.
01209
01210  1490-EXIT.
01211       EXIT.
01212
01213      EJECT
01214  1500-VERIFY-PENDING-BUS-REC.
01219      MOVE 'N'                    TO WS-PNDB-FOUND-SW.
091615     if pi-corr-check
091615        MOVE 'Y'                 TO PI-PROCESS-BENEFICIARY-SW
091615        go to 1590-exit
091615     end-if
01215      
      * EXEC CICS HANDLE CONDITION
01216 *        NOTFND   (1590-EXIT)
01217 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00006602' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303036363032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01218
01220
01221      MOVE PI-COMPANY-CD          TO ERPNDB-ALT-COMPANY-CD.
01222      MOVE PI-CARRIER             TO ERPNDB-ALT-CARRIER.
01223      MOVE PI-GROUPING            TO ERPNDB-ALT-GROUPING.
01224      MOVE PI-STATE               TO ERPNDB-ALT-STATE.
01225      MOVE PI-ACCOUNT             TO ERPNDB-ALT-ACCOUNT.
01226      MOVE PI-CERT-PRIME          TO ERPNDB-ALT-CERT-PRIME.
01227      MOVE PI-CERT-SFX            TO ERPNDB-ALT-CERT-SFX.
01228      MOVE PI-CERT-EFF-DT         TO ERPNDB-ALT-CERT-EFF-DT.
01229      MOVE ZEROS                  TO ERPNDB-ALT-CH-SEQ-NO.
01230      MOVE '2'                    TO ERPNDB-ALT-RECORD-TYPE.
01231
01232      IF ST-ACCNT-CNTL  OR
01233         ACCNT-CNTL
01234           MOVE SPACES             TO  ERPNDB-ALT-CARRIER.
01235
01236      IF ST-ACCNT-CNTL      OR
01237         CARR-ST-ACCNT-CNTL OR
01238         ACCNT-CNTL         OR
01239         CARR-ACCNT-CNTL
01240           MOVE SPACES             TO  ERPNDB-ALT-GROUPING.
01241
01242      IF ACCNT-CNTL OR
01243         CARR-ACCNT-CNTL
01244           MOVE SPACES             TO  ERPNDB-ALT-STATE.
01245
01246      
      * EXEC CICS READ
01247 *         DATASET   (ERPNDB-ALT-FILE-ID)
01248 *         SET       (ADDRESS OF PENDING-BUSINESS)
01249 *         RIDFLD    (ERPNDB-ALT-KEY)
01250 *     END-EXEC.
      *    MOVE '&"S        E          (   #00006632' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363332' TO DFHEIV0(25:11)
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
           
01251
01252      MOVE 'Y'                    TO WS-PNDB-FOUND-SW.
01253
01254      IF PB-C-PAYEE-CODE GREATER SPACES
01255         MOVE 'Y'                 TO PI-PROCESS-BENEFICIARY-SW.
01256
01257      MOVE PB-C-REFERENCE         TO PI-REFERENCE.
01258 *    MOVE PB-CI-INSURED-NAME     TO PI-INSURED-NAME.
01259
01260      MOVE PB-C-LF-CANCEL-AMT     TO  PI-LF-REFUND.
01261      MOVE PB-C-AH-CANCEL-AMT     TO  PI-AH-REFUND.
01262
01263      IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES
01264          MOVE PB-C-LF-CANCEL-DT  TO  PI-CANC-DT
01265      ELSE
01266          MOVE PB-C-AH-CANCEL-DT  TO  PI-CANC-DT.
01267
01268 *    IF MAINTI = 'A' OR 'C'
01269 *      IF PB-C-REFUND-CREATED
01270 *          MOVE -1               TO  MAINTL
01271 *          MOVE ER-3445          TO  EMI-ERROR
01272 *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01273
01274  1590-EXIT.
01275       EXIT.
01276
01277      EJECT
01278  2000-ADD-RECORD.
01279      
      * EXEC CICS GETMAIN
01280 *        SET      (ADDRESS OF CHECK-RECORDS)
01281 *        LENGTH   (600)
01282 *        INITIMG  (GETMAIN-SPACE)
01283 *    END-EXEC.
           MOVE 600
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00006665' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01284
01285      MOVE 'CH'                   TO CH-RECORD-ID.
01286      MOVE PI-COMPANY-CD          TO CH-COMPANY-CD.
01287      MOVE CARRIERI               TO CH-CARRIER.
01288      MOVE GROUPI                 TO CH-GROUPING.
01289      MOVE STATEI                 TO CH-STATE.
01290      MOVE ACCTI                  TO CH-ACCOUNT.
01291
01292      MOVE EFFDTI                 TO DC-GREG-DATE-1-EDIT.
01293      MOVE '2'                    TO DC-OPTION-CODE.
01294      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
01295      MOVE DC-BIN-DATE-1          TO CH-CERT-EFF-DT.
01296
01297      MOVE CERTNOI                TO CH-CERT-PRIME.
01298      MOVE SFXI                   TO CH-CERT-SFX.
01299
01300      MOVE SEQI                   TO CH-SEQUENCE-NO.
01301
01302      MOVE PI-PROCESSOR-ID        TO CH-RECORDED-BY
                                          ch-released-by
01303      MOVE EIBTIME                TO CH-LAST-MAINT-HHMMSS.
01304
01305      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
01306      MOVE '5'                    TO DC-OPTION-CODE.
01307      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
01308      MOVE DC-BIN-DATE-1          TO CH-RECORDED-DT
                                          ch-released-dt
01309
           move 'P'                    to ch-approval-status
01311
01312      MOVE LOW-VALUES             TO CH-CHECK-WRITTEN-DT
01313                                     CH-VOID-DT
01314                                     CH-CREDIT-ACCEPT-DT
01315                                     CH-CANC-DT
                                          ch-approval-dt
                                          ch-check-cashed-dt
01316      MOVE ZEROS                  TO CH-LF-REFUND
01317                                     CH-AH-REFUND
01318 *                                   CH-DEDUCT-WITHHELD
01319 *                                   CH-ADDITIONAL-CHARGE.
01320
01321      PERFORM 4000-ADD-CHANGE THRU 4000-EXIT.
01322
           if pi-prev-paid not numeric
              move zeros               to pi-prev-paid
           end-if
103116     if pi-prev-paid-this-month not numeric
103116        move zeros               to pi-prev-paid-this-month
103116     end-if
01354      IF EMI-FATAL-CTR GREATER THAN +0  OR
01355        (EMI-FORCABLE-CTR GREATER THAN +0  AND
01356         EIBAID NOT = DFHPF4)
01357          GO TO 8200-SEND-DATAONLY.
01358
           add ch-amount-paid to pi-prev-paid
01359      MOVE PI-CR-MONTH-END-DT     TO CH-CREDIT-SELECT-DT.
01360      MOVE PI-CR-CARRIER          TO CH-COMP-CARRIER.
01361      MOVE PI-CR-GROUPING         TO CH-COMP-GROUPING.
01362      MOVE PI-CR-FIN-RESP         TO CH-COMP-FIN-RESP.
01363      MOVE PI-CR-ACCOUNT          TO CH-COMP-ACCOUNT.
           move pi-am-csr              to ch-csr
           .
01366  2100-WRITE-RECORD.
           
      * exec cics read
      *       dataset    (erchek-file-id)
      *       into       (ws-dummy-erchek)
      *       ridfld     (ch-control-primary)
091615*       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            ws-dummy-erchek
             TO DFHEIV11
      *    MOVE '&"IL       E          (  N#00006733' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036373333' TO DFHEIV0(25:11)
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
           if RESP-NOTFND
              continue
           else
              add +1 to ch-sequence-no
              add 1 to seqi
              go to 2100-write-record
           end-if
           PERFORM 2700-WRITE-SQL      THRU 2700-EXIT
01367      
      * EXEC CICS HANDLE CONDITION
01368 *        DUPREC (2200-DUPREC)
01369 *    END-EXEC.
      *    MOVE '"$%                   ! ( #00006747' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303036373437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01370
01371      MOVE CH-CONTROL-PRIMARY     TO PI-ERCHEK-KEY.
01372      MOVE ZEROS                  TO CH-CHECK-QUE-CONTROL
01373                                     CH-CHECK-QUE-SEQUENCE
01374
01375      
      * EXEC CICS WRITE
01376 *        DATASET  (ERCHEK-FILE-ID)
01377 *        FROM     (CHECK-RECORDS)
01378 *        RIDFLD   (CH-CONTROL-PRIMARY)
091615*        resp     (ws-response)
01379 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-RECORDS
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00006755' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303036373535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 CHECK-RECORDS, 
                 DFHEIV11, 
                 CH-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
091615     if resp-normal and pi-check-type = 'C'
091615        move 'Y'                 to pi-check-cut
091615     end-if
01381      COMPUTE WS-JOURNAL-RECORD-LENGTH =
01382              ERCHEK-RECORD-LENGTH + 23.
01383
091615     if pi-check-type not = 'C'
              PERFORM 6200-UPDATE-PENDING-BUS-REC
                                       THRU 6200-EXIT
              if resp-normal
                 MOVE 'Y'              TO PB-C-REFUND-SW
                 add +1 to pi-chek-rec-cnt
                 perform 6210-rewrite-pndb
                                       thru 6210-exit
      *          PERFORM 2700-WRITE-SQL THRU 2700-EXIT
              end-if
091615     end-if
01388      IF EMI-NO-ERRORS
01389          MOVE ER-0000            TO EMI-ERROR
01390      ELSE
01391          IF EMI-FORCABLE-CTR GREATER THAN +0  AND
01392             EIBAID = DFHPF4
01393              MOVE ER-2600        TO EMI-ERROR.
01394
01395      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01396
01397      MOVE LOW-VALUES             TO EL677AI.
01398
01399      MOVE PI-CHEK-CARRIER        TO CARRIERO.
01400      MOVE PI-CHEK-GROUPING       TO GROUPO.
01401      MOVE PI-CHEK-STATE          TO STATEO.
01402      MOVE PI-CHEK-ACCOUNT        TO ACCTO.
01403
01404      MOVE PI-CHEK-EFF-DT         TO DC-BIN-DATE-1.
01405      MOVE SPACE                  TO DC-OPTION-CODE.
01406      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
01407      MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI.
01408
01409      MOVE PI-CHEK-CERT-NO        TO CERTNOO.
01410      MOVE PI-CHEK-SUF-NO         TO SFXO.
01411      MOVE PI-CHEK-SEQUENCE       TO SEQO.
01412
01413      MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA
01414                                     ACCTA     EFFDTA  CERTNOA
01415                                     SFXA.
01416      MOVE AL-UNNON               TO SEQA.
           go to 5000-browse-file
01417      GO TO 8100-SEND-INITIAL-MAP.
01418
01419  2200-DUPREC.
01420      ADD +1 TO CH-SEQUENCE-NO.
01421      MOVE CH-SEQUENCE-NO         TO SEQO
01422                                     PI-CHEK-SEQUENCE
01423                                     CHEK-RECORD-SEQ.
01424      GO TO 2100-WRITE-RECORD.
       2700-WRITE-SQL.
           perform 2710-build-rec      thru 2710-exit
           perform 2720-insert-row     thru 2720-exit
           perform 4300-FINISH-UP-DB   thru 4300-exit
           .
       2700-EXIT.
           EXIT.
       2710-BUILD-REC.
           move spaces                 to daily-check-request-rec
           move pi-company-id          to db-compid
           move ch-carrier             to db-carrier
                                          db-fincar
           move ch-grouping            to db-grouping
                                          db-fingrp
           move ch-state               to db-state
           move ch-account             to db-account
           move ch-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              move dc-greg-date-a-edit to db-effdate
           else
              move spaces              to db-effdate
           end-if
           move ch-cert-prime          to db-certificate
           move ch-cert-sfx            to db-cert-sfx
           move ch-sequence-no         to db-seq-no
           move '1'                    to db-type
091615                                    db-check-sub-type
091615     if pi-check-type = 'C'
091615        move '2'                 to db-check-sub-type
091615     end-if
           move ch-amount-paid         to db-amount-n
           move zeros                  to db-checkstatus
           move ch-check-que-control   to db-releasebatch
           move save-bin-date          to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              move dc-greg-date-a-edit to db-releasedt
           else
              move spaces              to db-releasedt
           end-if
           move pi-processor-id        to db-releaseby
           move ch-payee-name-1        to db-payeename1
           move ch-payee-name-2        to db-payeename2
           move ch-payee-address-1     to db-payeeaddr1
           move ch-payee-address-2     to db-payeeaddr2
           move ch-payee-city          to db-payeecity
           move ch-payee-state         to db-payeest
           move ch-payee-zip-code      to db-payeezip
           move ch-comp-fin-resp       to db-finresp
           move ch-comp-account        to db-finacct
           move ch-recorded-by         to db-preparer
           move ch-return-to           to db-return-to
           move pi-table-name          to db-insured-name
070622     MOVE CH-PAYEE-CODE          TO db-payeecode
           .
       2710-EXIT.
           EXIT.
       2720-INSERT-ROW.
           if not connected-to-db
              perform 4100-CONNECT-TO-DB
                                       thru 4100-exit
           end-if
           EXEC SQL
              INSERT into ChkApp_Check (
                 Company,
                 CertCarrier,
                 CertGroup,
                 CertState,
                 CertAccount,
                 CertEffDate,
                 CertNumber,
                 CertNumberSuf,
                 CheckSeqNbr,
                 CheckType,
                 CheckAmount,
                 ReleaseBatchNbr,
                 ReleaseDate,
                 ReleasedBy,
                 PayeeName1,
                 PayeeName2,
                 PayeeAddress1,
                 PayeeAddress2,
                 PayeeCity,
                 PayeeState,
                 PayeeZip,
                 CompCarrier,
                 CompGroup,
                 CompFinResp,
                 CompAccount,
                 Preparer,
                 ReturnTo,
091615           InsuredName,
091615           CheckSubType,
070622           PayeeCode)
               VALUES (
                 :DB-CompId,
                 :DB-Carrier,
                 :DB-Grouping,
                 :DB-State,
                 :DB-Account,
                 :DB-EffDate,
                 :DB-Certificate,
                 :db-cert-sfx,
                 :DB-Seq-No,
                 :DB-Type,
                 :db-amount,
                 :db-releasebatch,
                 :db-releasedt,
                 :db-releaseby,
                 :db-payeename1,
                 :db-payeename2,
                 :db-payeeaddr1,
                 :db-payeeaddr2,
                 :db-payeecity,
                 :db-payeest,
                 :db-payeezip,
                 :db-fincar,
                 :db-fingrp,
                 :db-finresp,
                 :db-finacct,
                 :db-preparer,
                 :db-return-to,
091615           :db-insured-name,
091615           :db-check-sub-type,
070622           :db-payeecode)
           END-EXEC
           display ' about to insert '
           display ' db-compid     ' db-compid
      *    display ' db-carrier    ' db-carrier
      *    display ' db-group      ' db-grouping
      *    display ' db state      ' db-state
      *    display ' accounbt      ' db-account
      *    display ' effdt         ' db-effdate
           display ' cert          ' db-certificate
           display ' payee code ' CH-PAYEE-CODE
      *    display ' suffix        ' db-cert-sfx
      *    display ' seq no        ' db-seq-no
      *    display ' type          ' db-type
           if sqlcode = -2601
              add +1                   to ch-sequence-no
              move ch-sequence-no      to DB-Seq-No
              go to 2720-insert-row
           end-if
           if sqlcode not = 0
              display "Error: cannot insert row "
              display ' sql return code ' sqlcode
              move sqlcode to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              move sqlcode to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              string ws-dis-sql-code ' ' sqlerrmc (1:50)
                 into EMI-MESSAGE-AREA (1)
              end-string
              display ' msga1 ' emi-message-area (1)
              perform 4300-FINISH-UP-DB thru 4300-exit
              go to 8200-send-dataonly
      *       goback
           end-if
           .
       2720-EXIT.
           EXIT.
01666  3000-CHANGE-RECORD.
01667      
      * EXEC CICS HANDLE CONDITION
01668 *        NOTFND (3900-RECORD-NOTFND)
01669 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00006983' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303036393833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
111219     if mainti <> 'X'
111219        go to 3000-continue
111219     end-if
111219
111219     move pi-erchek-key          to erchek-key
111219
111219     
      * exec cics startbr
111219*        dataset     ('ERCHEK')
111219*        ridfld      (erchek-key)
111219*        resp        (ws-response)
111219*    end-exec
           MOVE 'ERCHEK' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006992' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303036393932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erchek-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
111219
111219     if not resp-normal
111219        display ' bad startbr ' ws-response
111219        go to 3000-continue
111219     end-if
111219
111219     .
111219 3000-readnext.
111219
111219     
      * exec cics readnext
111219*       dataset    ('ERCHEK')
111219*       ridfld  (erchek-key)
111219*       set     (address of check-records)
111219*       resp    (ws-response)
111219*    end-exec
           MOVE 'ERCHEK' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00007006' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303037303036' TO DFHEIV0(25:11)
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
111219
111219     if not resp-normal
111219        display ' bad readnext ' ws-response
111219        go to 3000-endbr
111219     end-if
111219
111219     if ch-company-cd = pi-company-cd and
111219        ch-carrier    = pi-chek-carrier and
111219        ch-state      = pi-chek-state and
111219        ch-account    = pi-chek-account and
111219        ch-cert-eff-dt = pi-chek-eff-dt and
111219        ch-cert-prime = pi-chek-cert-no and
111219        ch-cert-sfx   = pi-chek-suf-no
111219        continue
111219     else
111219        go to 3000-endbr
111219     end-if
111219
111219     if ch-amount-paid = pi-void-reissue-amt and
111219        ch-void-dt = low-values and
111219        ch-recorded-dt = save-bin-date
111219        move er-3463             to emi-error
111219        move -1                  to maintl
111219        move al-uabon            to mainta
111219        perform 9900-error-format
111219                                 thru 9900-exit
111219        go to 8200-send-dataonly
111219     else
111219        go to 3000-readnext
111219     end-if
111219
111219     .
111219 3000-endbr.
111219
111219     
      * exec cics endbr
111219*       dataset   ('ERCHEK')
111219*    end-exec
           MOVE 'ERCHEK' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007046' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
111219
111219     .
111219 3000-continue.
01670
01671      
      * EXEC CICS READ
01672 *        DATASET  (ERCHEK-FILE-ID)
01673 *        SET      (ADDRESS OF CHECK-RECORDS)
01674 *        RIDFLD   (PI-ERCHEK-KEY)
01675 *        UPDATE
01676 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007053' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303533' TO DFHEIV0(25:11)
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
           
01677
01678      IF CH-LF-REFUND NOT NUMERIC
01679          MOVE ZEROS              TO CH-LF-REFUND.
01680      IF CH-AH-REFUND NOT NUMERIC
01681          MOVE ZEROS              TO CH-AH-REFUND.
01682 *    IF CH-DEDUCT-WITHHELD NOT NUMERIC
01683 *        MOVE ZEROS              TO CH-DEDUCT-WITHHELD.
01684 *    IF CH-ADDITIONAL-CHARGE NOT NUMERIC
01685 *        MOVE ZEROS              TO CH-ADDITIONAL-CHARGE.
01686
01687      IF VREASONL NOT = ZEROS
01688          MOVE VREASONI           TO  CH-VOID-REASON.
01689
111418     IF MAINTI = 'V' or 'R'
01691         GO TO 3050-void-processing
           end-if
           .
111219 3010-VOID-REVERSAL.  *>  Mainti of X only
111219
111219     if ch-void-dt = low-values
111219        move er-3462             to emi-error
111219        move -1                  to maintl
111219        move al-uabon            to mainta
111219        
      * exec cics unlock
111219*          dataset    (ERCHEK-FILE-ID)
111219*       end-exec
      *    MOVE '&*                    #   #00007082' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
111219        perform 9900-error-format
111219                                 thru 9900-exit
111219        go to 8200-send-dataonly
111219     end-if
111219
111219     if ch-void-dt <> SAVE-BIN-DATE
111219        move er-3461             to emi-error
111219        move -1                  to maintl
111219        move al-uabon            to mainta
111219        
      * exec cics unlock
111219*          dataset    (ERCHEK-FILE-ID)
111219*       end-exec
      *    MOVE '&*                    #   #00007094' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
111219        perform 9900-error-format
111219                                 thru 9900-exit
111219        go to 8200-send-dataonly
111219     end-if
111219
111219     move low-values             to ch-void-dt
111219     move spaces                 to ch-void-by
111219                                    ch-void-reason
111219
111219     go to 3100-rewrite-record
01693      PERFORM 6100-VERIFY-CERTIFICATE THRU 6190-EXIT.
01694
01695      PERFORM 4000-ADD-CHANGE THRU 4000-EXIT.
01696
01705      IF EMI-FATAL-CTR GREATER THAN +0  OR
01706        (EMI-FORCABLE-CTR GREATER THAN +0  AND
01707         EIBAID NOT = DFHPF4)
01708          GO TO 8200-SEND-DATAONLY.
           go to 3100-rewrite-record
           .
       3050-void-processing.
           perform 4100-CONNECT-TO-DB  thru 4100-exit
           if sqlcode = 0
              perform 4200-get-tbl-row thru 4200-exit
              if sqlcode = 0
                 if nu-app-date = -1
                    move -1            to maintl
                    move er-3453       to emi-error
                    move al-uabon      to mainta
                    perform 9900-error-format
                                       thru 9900-exit
                    perform 4300-FINISH-UP-DB thru 4300-exit
                    go to 8200-send-dataonly
                 end-if
              end-if
              if connected-to-db
                 perform 4300-FINISH-UP-DB thru 4300-exit
              end-if
           end-if
           if (ch-check-written-dt = low-values)
              or (ch-check-no = spaces)
              move -1                  to maintl
              move er-3455             to emi-error
              move al-uabon            to mainta
              perform 9900-error-format
                                       thru 9900-exit
              perform 4300-FINISH-UP-DB thru 4300-exit
              go to 8200-send-dataonly
030414     end-if
           .
01712  3100-REWRITE-RECORD.
01713      MOVE EIBTIME                TO CH-LAST-MAINT-HHMMSS.
01714
01715      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
01716      MOVE '5'                    TO DC-OPTION-CODE.
01717      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
01718
01719      IF MAINTI = 'C'
01720        MOVE PI-PROCESSOR-ID      TO CH-RECORDED-BY
           end-if
111418     IF MAINTI = 'V' or 'R'
030414        IF (CH-VOID-DT = LOW-VALUES)
030414           and (pi-check-cashed = spaces)
01726            MOVE PI-PROCESSOR-ID  TO CH-VOID-BY
01727            MOVE DC-BIN-DATE-1    TO CH-VOID-DT
01729            PERFORM 3200-PAY-ADJS-REVERSAL      THRU 3290-EXIT
                 if pi-chek-rec-cnt = +1
01730               PERFORM 6200-UPDATE-PENDING-BUS-REC
                                       THRU 6200-EXIT
                    if resp-normal
                       MOVE ' '        TO PB-C-REFUND-SW
                       perform 6210-rewrite-pndb
                                       thru 6210-exit
                    end-if
                 end-if
01731         ELSE
01732            MOVE -1               TO MAINTL
01733            MOVE ER-2583          TO EMI-ERROR
01734            MOVE AL-UABON         TO MAINTA
01735            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01736            GO TO 8200-SEND-DATAONLY
              end-if
           end-if
01737
01738      MOVE CH-AMOUNT-PAID         TO WS-SV-AMOUNT-PAID.
01739
01740      
      * EXEC CICS REWRITE
01741 *        DATASET  (ERCHEK-FILE-ID)
01742 *        FROM     (CHECK-RECORDS)
01743 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007183' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 CHECK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01744
01745      IF (PI-COMPANY-ID EQUAL 'LGX' OR 'TMS') AND
01746         (MAINTI EQUAL 'C')          AND
01747         (PI-PAYTO1 NOT EQUAL PAYTO1I)
01748             PERFORM 3300-UPDATE-ERPYAJ THRU 3300-EXIT.
111219     if mainti = 'X'
111219        perform 3320-remove-rev-erpyaj-rec
111219                                 thru 3320-exit
111219     end-if
01750      IF EMI-NO-ERRORS
01751          MOVE ER-0000            TO EMI-ERROR
01752      ELSE
01753          IF EMI-FORCABLE-CTR GREATER THAN +0  AND
01754             EIBAID = DFHPF4
01755              MOVE ER-2600        TO EMI-ERROR.
111418     if mainti = 'R'
111418        if pi-void-reissue-pass = ' '
111418           move '1'              to pi-void-reissue-pass
111418        MOVE ER-3274             TO EMI-ERROR
111418        end-if
111418     end-if
01757      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01758
01759      MOVE LOW-VALUES             TO EL677AI.
01760
01761      MOVE PI-CHEK-CARRIER        TO CARRIERO.
01762      MOVE PI-CHEK-GROUPING       TO GROUPO.
01763      MOVE PI-CHEK-STATE          TO STATEO.
01764      MOVE PI-CHEK-ACCOUNT        TO ACCTO.
01765
01766      MOVE PI-CHEK-EFF-DT         TO DC-BIN-DATE-1.
01767      MOVE SPACE                  TO DC-OPTION-CODE.
01768      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
01769      MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI.
01770
01771      MOVE PI-CHEK-CERT-NO        TO CERTNOO.
01772      MOVE PI-CHEK-SUF-NO         TO SFXO.
01773      MOVE PI-CHEK-SEQUENCE       TO SEQO.
01774
01775      MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA
01776                                      ACCTA   EFFDTA
01777                                     CERTNOA   SFXA.
01778
01779      MOVE AL-UNNON               TO SEQA.
           go to 5000-browse-file
01780 *    GO TO 8100-SEND-INITIAL-MAP.
01781
           .
01783  3200-PAY-ADJS-REVERSAL.
01784
01785      IF CH-AMOUNT-PAID NOT = ZEROS
01786          PERFORM 3250-BUILD-ERPYAJ-REVERSAL THRU 3259-BUILD-EXIT
01787          PERFORM 3280-WRITE-ERPYAJ-REVERSAL THRU 3289-WRITE-EXIT.
01788
01790      GO TO 3290-EXIT
           .
01827  3250-BUILD-ERPYAJ-REVERSAL.
01828
01829      
      * EXEC CICS GETMAIN
01830 *        SET     (ADDRESS OF PENDING-PAY-ADJ)
01831 *        LENGTH  (200)
01832 *        INITIMG (GETMAIN-SPACE)
01833 *    END-EXEC.
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007245' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01834
01835      MOVE 'PY'                   TO PY-RECORD-ID.
01836      MOVE PI-COMPANY-CD          TO PY-COMPANY-CD.
01837
01838      MOVE CH-COMP-CARRIER        TO PY-CARRIER.
01839      IF PI-ZERO-CARRIER  OR
01840         PI-ZERO-CAR-GROUP
01841          MOVE ZERO               TO PY-CARRIER.
01842
01843      MOVE CH-COMP-GROUPING       TO PY-GROUPING.
01844      IF PI-ZERO-GROUPING  OR
01845         PI-ZERO-CAR-GROUP
01846          MOVE ZERO               TO PY-GROUPING.
01847
01848      MOVE CH-COMP-FIN-RESP       TO PY-FIN-RESP.
01849      MOVE CH-COMP-ACCOUNT        TO PY-ACCOUNT.
030414     MOVE 'R'                    TO PY-RECORD-TYPE.
01851      MOVE EIBTIME                TO PY-FILE-SEQ-NO.
           move '1825011300'           to py-gl-account
           move 'VOID REFCK'           TO py-gl-comment
030414     move ch-amount-paid         to py-entry-amt
030414*    COMPUTE PY-ENTRY-AMT = CH-AMOUNT-PAID * -1.
01873
01874      MOVE CH-CHECK-NO            TO WS-CHECK-WORK
01875      MOVE WS-CHECK-NO            TO PY-CHECK-NUMBER.
01876
01877      MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY.
01878      MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS.
01879      MOVE SAVE-BIN-DATE          TO PY-LAST-MAINT-DT
01880                                     PY-INPUT-DT.
01881 *    MOVE ZEROS                  TO PY-CHECK-QUE-CONTROL
01882 *                                   PY-CHECK-QUE-SEQUENCE.
01883      MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT
01884                                     PY-BILLED-DATE
01885                                     PY-REPORTED-DT
01886                                     PY-CHECK-WRITTEN-DT
01887                                     PY-AR-DATE
01888                                     PY-GL-DATE.
01889      MOVE PI-CR-MONTH-END-DT     TO PY-CREDIT-SELECT-DT.
01890      MOVE CH-CHECK-ORIGIN-SW     TO PY-CHECK-ORIGIN-SW.
030414*    MOVE 'V'                    TO PY-VOID-SW.
01892 *    MOVE CH-CHECK-REFERENCE     TO PY-REF-NO.
01893
01894      IF PI-AR-PROCESSING
01895          MOVE 'B'                TO PY-PYMT-TYPE
01896          MOVE 'A'                TO PY-PMT-APPLIED.
01897
01898  3259-BUILD-EXIT.
01899      EXIT.
01900
01901  3280-WRITE-ERPYAJ-REVERSAL.
01902
01903      
      * EXEC CICS HANDLE CONDITION
01904 *        DUPREC (3287-DUPREC)
01905 *    END-EXEC.
      *    MOVE '"$%                   ! * #00007302' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303037333032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01906
01907      
      * EXEC CICS WRITE
01908 *        DATASET (ERPYAJ-FILE-ID)
01909 *        FROM    (PENDING-PAY-ADJ)
01910 *        RIDFLD  (PY-CONTROL-PRIMARY)
01911 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007306' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01912
01913      GO TO 3289-WRITE-EXIT.
01914
01915  3287-DUPREC.
01916      ADD +1 TO PY-FILE-SEQ-NO.
01917
01918      GO TO 3280-WRITE-ERPYAJ-REVERSAL.
01919
01920  3289-WRITE-EXIT.
01921      EXIT.
01922
01923  3290-EXIT.
01924       EXIT.
01925
01926      EJECT
01927  3300-UPDATE-ERPYAJ.
01928
01929      
      * EXEC CICS GETMAIN
01930 *        SET     (ADDRESS OF PENDING-PAY-ADJ)
01931 *        LENGTH  (200)
01932 *        INITIMG (GETMAIN-SPACE)
01933 *    END-EXEC.
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007328' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01934
01935      MOVE PI-COMPANY-CD          TO PYAJ-COMPANY-CD.
01936      MOVE PI-CHEK-CARRIER        TO PYAJ-CARRIER.
01937      MOVE PI-CHEK-GROUPING       TO PYAJ-GROUPING.
01938      MOVE PI-CR-FIN-RESP         TO PYAJ-FIN-RESP.
01939      MOVE PI-CR-ACCOUNT          TO PYAJ-ACCOUNT.
01940      MOVE 'C'                    TO PYAJ-RECORD-TYPE.
01941      MOVE ZEROS                  TO PYAJ-FILE-SEQ-NO.
01942
01943      
      * EXEC CICS HANDLE CONDITION
111219*        NOTFND   (3300-EXIT)
111219*        ENDFILE  (3300-EXIT)
01946 *    END-EXEC.
      *    MOVE '"$I''                  ! + #00007342' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303037333432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01947
111219 3300-READNEXT-ERPYAJ.
01949
01950      
      * EXEC CICS READ
01951 *        DATASET  (ERPYAJ-FILE-ID)
01952 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
01953 *        RIDFLD   (ERPYAJ-KEY)
01954 *        GTEQ
01955 *    END-EXEC.
      *    MOVE '&"S        G          (   #00007349' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333439' TO DFHEIV0(25:11)
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
           
01956
01957      MOVE PY-CONTROL-PRIMARY TO ERPYAJ-KEY.
01958
01959      IF (PI-COMPANY-CD    NOT = PY-COMPANY-CD) OR
01960         (PI-CHEK-CARRIER  NOT = PY-CARRIER)    OR
01961         (PI-CHEK-GROUPING NOT = PY-GROUPING)   OR
01962         (PI-CR-FIN-RESP   NOT = PY-FIN-RESP)   OR
01963         (PI-CR-ACCOUNT    NOT = PY-ACCOUNT)    OR
01964         (PY-RECORD-TYPE   NOT = 'C')
111219          GO TO 3300-EXIT.
01966
01967      IF WS-SV-AMOUNT-PAID NOT EQUAL PY-ENTRY-AMT
01968          ADD +1 TO PYAJ-FILE-SEQ-NO
111219         GO TO 3300-READNEXT-ERPYAJ.
01970
01971      
      * EXEC CICS READ UPDATE
01972 *        DATASET   (ERPYAJ-FILE-ID)
01973 *        RIDFLD    (ERPYAJ-KEY)
01974 *        SET       (ADDRESS OF PENDING-PAY-ADJ)
01975 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007370' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333730' TO DFHEIV0(25:11)
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
           
01976
01977      MOVE PAYTO1I                TO WS-TMS-PY-PAYEE.
01978      MOVE CERTNOI                TO WS-TMS-PY-CERT.
01979
01980      MOVE WS-TMS-ENTRY-COMMENT TO PY-ENTRY-COMMENT.
01981
01982      MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY.
01983      MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS.
01984      MOVE SAVE-BIN-DATE          TO PY-LAST-MAINT-DT
01985
01986      
      * EXEC CICS REWRITE
01987 *        DATASET  (ERPYAJ-FILE-ID)
01988 *        FROM     (PENDING-PAY-ADJ)
01989 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007385' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01990
111219 3300-EXIT.
01992       EXIT.
01993
111219 3320-remove-rev-erpyaj-rec.
111219
111219     
      * EXEC CICS GETMAIN
111219*       SET     (ADDRESS OF PENDING-PAY-ADJ)
111219*       LENGTH  (200)
111219*       INITIMG (GETMAIN-SPACE)
111219*    END-EXEC
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007395' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
111219
111219     MOVE PI-COMPANY-CD          TO erpyaj-key
111219     MOVE ch-comp-CARRIER        TO PYAJ-CARRIER
111219     MOVE ch-comp-grouping       TO PYAJ-GROUPING
111219     MOVE ch-comp-fin-resp       TO PYAJ-FIN-RESP
111219     MOVE ch-comp-account        TO PYAJ-ACCOUNT
111219     MOVE ZEROS                  TO PYAJ-FILE-SEQ-NO
111219
111219     MOVE CH-COMP-FIN-RESP       TO PY-FIN-RESP.
111219     MOVE CH-COMP-ACCOUNT        TO PY-ACCOUNT.
111219     
      * exec cics startbr
111219*       dataset    ('ERPYAJ')
111219*       ridfld     (erpyaj-key)
111219*       resp       (ws-response)
111219*    end-exec
           MOVE 'ERPYAJ' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00007410' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303037343130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erpyaj-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
111219
111219     if not resp-normal
111219        display ' no erpyaj found ' pyaj-carrier ' '
111219        pyaj-fin-resp ' ' pyaj-account
111219        go to 3320-exit
111219     end-if
111219
111219     set pyaj-browse-started to true
111219
111219     .
111219 3320-readnext.
111219
111219     
      * exec cics readnext
111219*       dataset     ('ERPYAJ')
111219*       set         (address of pending-pay-adj)
111219*       ridfld      (erpyaj-key)
111219*       resp        (ws-response)
111219*    end-exec
           MOVE 'ERPYAJ' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00007427' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303037343237' TO DFHEIV0(25:11)
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
111219
111219     if not resp-normal
111219        display ' error 3320-readnext ' ws-response
111219        go to 3320-exit
111219     end-if
111219
111219     if ch-comp-carrier = py-carrier and
111219        ch-comp-fin-resp  = py-fin-resp and
111219        ch-comp-account = py-account
111219        if py-record-type = 'R' and
111219           ch-amount-paid = py-entry-amt and
111219           py-last-maint-dt = save-bin-date and
111219           py-gl-comment = 'VOID REFCK' and
111219           py-check-number = ch-check-no(2:6) and
111219           py-gl-account = '1825011300'
111219           
      * exec cics delete
111219*             dataset     ('ERPYAJ')
111219*             ridfld      (erpyaj-key)
111219*             resp        (ws-response)
111219*          end-exec
           MOVE 'ERPYAJ' TO DFHEIV1
      *    MOVE '&(  R                 &  N#00007448' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303037343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 erpyaj-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
111219           if not resp-normal
111219              display ' bad delete ' ws-response ' ' pyaj-account
111219           end-if
111219        else
111219           go to 3320-readnext
111219        end-if
111219     end-if
111219
111219     .
111219 3320-end-browse.
111219
111219     if pyaj-browse-started
111219        
      * exec cics endbr
111219*           dataset    ('ERPYAJ')
111219*       end-exec
           MOVE 'ERPYAJ' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007465' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
111219     end-if
111219     .
111219 3320-exit.
111219     exit.
       3400-delete-record.
           
      * EXEC CICS READ
      *        DATASET  (ERCHEK-FILE-ID)
      *        SET      (ADDRESS OF CHECK-RECORDS)
      *        RIDFLD   (PI-ERCHEK-KEY)
      *        UPDATE
      *        resp     (ws-response)
      *    END-EXEC
      *    MOVE '&"S        EU         (  N#00007473' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037343733' TO DFHEIV0(25:11)
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
           if (ch-in-limbo or ch-approv-pending)
              and (ch-void-dt = low-values)
              and (ch-check-written-dt = low-values)
              and (ch-credit-accept-dt = low-values)
              perform 3500-check-tbl   thru 3500-exit
              if (row-deleted and tbl-commited)
      *          or (emi-error = er-3450)
                 
      * exec cics delete
      *             dataset   (erchek-file-id)
      *          end-exec
      *    MOVE '&(                    &   #00007487' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 erchek-file-id, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 move spaces to pi-check-cut
              end-if
           else
              move -1               to maintl
              move er-3452          to emi-error
              move al-uabon         to mainta
              perform 9900-error-format
                                       thru 9900-exit
              go to 8200-send-dataonly
           end-if
           if pi-chek-rec-cnt = +1
              PERFORM 6200-UPDATE-PENDING-BUS-REC
                                       THRU 6200-EXIT
              if resp-normal
                 MOVE ' '              TO PB-C-REFUND-SW
                 perform 6210-rewrite-pndb
                                       thru 6210-exit
              end-if
           end-if
           MOVE ER-0000            TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE LOW-VALUES             TO EL677AI.
           MOVE PI-CHEK-CARRIER        TO CARRIERO.
           MOVE PI-CHEK-GROUPING       TO GROUPO.
           MOVE PI-CHEK-STATE          TO STATEO.
           MOVE PI-CHEK-ACCOUNT        TO ACCTO.
           MOVE PI-CHEK-EFF-DT         TO DC-BIN-DATE-1.
           MOVE SPACE                  TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
           MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI.
           MOVE PI-CHEK-CERT-NO        TO CERTNOO.
           MOVE PI-CHEK-SUF-NO         TO SFXO.
           MOVE PI-CHEK-SEQUENCE       TO SEQO.
           MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA
                                          ACCTA   EFFDTA
                                          CERTNOA   SFXA.
           MOVE AL-UNNON               TO SEQA.
           GO TO 8100-SEND-INITIAL-MAP
           .
       3400-exit.
           exit.
       3500-check-tbl.
           perform 4100-CONNECT-TO-DB  thru 4100-exit
           if sqlcode = 0
              perform 4200-get-tbl-row thru 4200-exit
           end-if
            if sqlcode = 0
               if (nu-app-date = -1)
                  or (db-app-status = '2')
                  perform 4250-delete-row thru 4250-exit
               else
                 MOVE ER-3452            TO  EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 MOVE -1                 TO  PFENTERL
                 GO TO 8200-SEND-DATAONLY
              end-if
            else
00553          MOVE ER-3450            TO  EMI-ERROR
00554          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00555          MOVE -1                 TO  PFENTERL
00556 *        GO TO 8200-SEND-DATAONLY
            end-if
           if connected-to-db
              perform 4300-FINISH-UP-DB thru 4300-exit
           end-if
           .
       3500-exit.
           exit.
02046  3900-RECORD-NOTFND.
02047      MOVE ER-2908                TO EMI-ERROR.
02048      MOVE -1                     TO CARRIERL
02049      MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA
02050                                     ACCTA     EFFDTA  CERTNOA
02051                                     SFXA      SEQA.
02052      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02053      GO TO 8200-SEND-DATAONLY.
02054      EJECT
02055  4000-ADD-CHANGE.
02056      IF PAYTO1L NOT = ZEROS
02060         MOVE PAYTO1I             TO CH-PAYEE-NAME-1
           END-IF
02061
02062      IF PAYTO2L NOT = ZEROS
02063          MOVE PAYTO2I            TO CH-PAYEE-NAME-2.
02064
02065      IF PAYAD1L NOT = ZEROS
02066          MOVE PAYAD1I            TO CH-PAYEE-ADDRESS-1.
02067
02068      IF PAYAD2L NOT = ZEROS
02069          MOVE PAYAD2I            TO CH-PAYEE-ADDRESS-2.
02070
02071      IF PAYctyL NOT = ZEROS
02072          MOVE payctyi            TO CH-PAYEE-CITY.
02073
02071      IF PAYstL NOT = ZEROS
02072          MOVE paysti            TO CH-PAYEE-state.
02073
02074      IF PAYEEI GREATER SPACES
02075         MOVE PAYEEI              TO CH-PAYEE-CODE.
02076
02077      IF PTOZIPL  =  ZEROS
02078          MOVE ZEROS              TO CH-PAYEE-ZIP-CODE
02079          GO TO 4000-CK-REST.
02080
02081      MOVE PTOZIPI                TO WS-ZIP-CODE.
02082
02083      IF NOT WS-CANADIAN-POST-CODE
02084          MOVE WS-ZIP-PRIME       TO CH-PAYEE-ZIP
02085          MOVE WS-ZIP-PLUS4       TO CH-PAYEE-ZIP-EXT
02086          GO TO 4000-CK-REST.
02087
02088      MOVE SPACES                 TO CH-CANADIAN-POSTAL-CODE.
02089      IF WS-CAN-POST-4TH = SPACE OR '-'
02090          MOVE WS-CAN-POST2-1     TO CH-CAN-POSTAL-1
02091          MOVE WS-CAN-POST2-2     TO CH-CAN-POSTAL-2
02092      ELSE
02093          MOVE WS-CAN-POST1-1     TO CH-CAN-POSTAL-1
02094          MOVE WS-CAN-POST1-2     TO CH-CAN-POSTAL-2.
02095
02096  4000-CK-REST.
02097
           IF RETTOL NOT = ZEROS
              MOVE RETTOI TO CH-RETURN-TO.
           if dedcynl not = zeros
              move dedcyni             to ch-deduct-commission
           end-if
      *    if dedcynl not = zeros
      *       if (dedcyni = 'Y')
      *          or (pi-prev-ded-comm = 'Y')
      *          compute ws-amt =
      *             pi-refund-on-pending-rec -
      *                pi-prev-paid - pi-ue-comm
      *       else
      *          if dedcyni = 'N'
      *             compute ws-amt =
      *                pi-refund-on-pending-rec -
      *                   pi-prev-paid-this-month
      *          end-if
      *       end-if
      *    end-if
091615     if pi-check-type = 'C'
091615        compute ws-amt =
091615           pi-endt-prm-diff - pi-prev-paid
091615        if (dedcyni = 'Y')
091615           or (pi-prev-ded-comm = 'Y')
091615           compute ws-amt =
091615              ws-amt - pi-ue-comm
091615        end-if
091615        if ws-amt <= zeros
091615           move pi-endt-prm-diff to ws-amt
091615        end-if
091615     end-if
02105      IF AMOUNTL NOT = ZEROS
02106          MOVE WS-AMT             TO CH-AMOUNT-PAID
02107          MOVE CH-AMOUNT-PAID     TO AMOUNTO.
02108
02109      IF CHECKL NOT = ZEROS
02110          MOVE CHECKI             TO CH-CHECK-NO.
02111
02112      IF REASONL NOT = ZEROS
02113          MOVE REASONI            TO CH-REASON-FOR-CHECK.
02114
02115 *    IF DEDAMTL NOT = ZEROS
02116 *        MOVE WS-DED             TO CH-DEDUCT-WITHHELD
02117 *                                   DEDAMTO.
02118
02119 *    IF ADDLCHGL NOT = ZEROS
02120 *        MOVE WS-ADDL            TO CH-ADDITIONAL-CHARGE
02121 *                                   ADDLCHGO.
02122
02123      IF STUBL NOT = ZEROS
02124          MOVE STUBI              TO CH-STUB-LINE-1.
02125
02126      IF TEXT1L NOT = ZEROS
02127          MOVE TEXT1I             TO CH-TEXT-LINE-1.
02128
02129      IF TEXT2L NOT = ZEROS
02130          MOVE TEXT2I             TO CH-TEXT-LINE-2.
02131
02132      IF TEXT3L NOT = ZEROS
02133          MOVE TEXT3I             TO CH-TEXT-LINE-3.
02134
02135      IF TYPEL NOT = ZEROS
02136          MOVE TYPEI              TO CH-CHECK-ORIGIN-SW.
02137
02138 *    MOVE LETTER1I               TO CH-LETTERS (1).
02139 *    MOVE LETTER2I               TO CH-LETTERS (2).
02140 *    MOVE LETTER3I               TO CH-LETTERS (3).
02141
02142 *    MOVE SPACES                 TO CH-CHECK-REFERENCE.
02143      IF CH-CHECK-ORIGIN-SW = 'R'
02144          PERFORM 1500-VERIFY-PENDING-BUS-REC THRU 1590-EXIT
02145 *        MOVE PI-REFERENCE           TO CH-CHECK-REFERENCE
02146          MOVE PI-CANC-DT             TO CH-CANC-DT
02147          MOVE PI-LF-REFUND           TO CH-LF-REFUND
02148          MOVE PI-AH-REFUND           TO CH-AH-REFUND
091615     end-if
           MOVE PI-INSURED-NAME        TO CH-INSURED-NAME.
02151 *    IF PI-AR-PROCESSING AND
02152 *       REFL NOT = ZERO
02153 *        MOVE REFI                   TO CH-CHECK-REFERENCE.
02154
02155  4000-EXIT.
02156       EXIT.
       4100-CONNECT-TO-DB.
030921     move 'HOVTSTDB01_ChkApprv'  to svr
030921     move 'appuser'              to usr
030921     move 'appuser@cso'          to pass
021714
021714     if ws-kix-myenv = 'cid1p'
030921        move 'SDVDB01_ChkApprv'  to svr
030921        move 'appuser'           to usr
030921        move 'appuser@cso'       to pass
021714     end-if
           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string
           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
           if sqlcode not = 0
              display "Error: cannot connect "
              display sqlcode
              display sqlerrmc
           end-if
           set connected-to-db to true
           .
       4100-EXIT.
           EXIT.
       4200-get-tbl-row.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  I'm only expecting one row so no cursor is declared       ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
           move pi-company-id          to ws-compid
           move ch-carrier             to ws-carrier
           move ch-grouping            to ws-grouping
           move ch-state               to ws-state
           move ch-account             to ws-account
           move ch-cert-eff-dt         to dc-bin-date-1
           move ' '                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
              move dc-greg-date-a-edit to ws-eff-date
           end-if
           move ch-cert-prime          to ws-certificate
           move ch-cert-sfx            to ws-cert-sfx
           move ch-sequence-no         to ws-seq-no
           move '1'                    to ws-type
091615                                    ws-check-sub-type
091615     if pi-check-type = 'C'
091615        move '2'                 to ws-check-sub-type
091615     end-if
           exec sql
              SELECT
                 ApprovalStatus,
                 MaintainedBy,
                 MaintainedDate,
                 ApprovalBatch
              INTO
                 :db-app-status :nu-app-status,
                 :db-app-by :nu-app-by,
                 :db-app-date :nu-app-date,
                 :db-app-batch :nu-app-batch
              FROM
                 ChkApp_Check
              WHERE
                 Company           = :ws-compid
                 and CertCarrier   = :ws-carrier
                 and CertGroup     = :ws-grouping
                 and CertState     = :ws-state
                 and CertAccount   = :ws-account
                 and CertEffDate   = :ws-eff-date
                 and CertNumber    = :ws-certificate
                 and CertNumberSuf = :ws-cert-sfx
                 and CheckSeqNbr   = :ws-seq-no
                 and CheckType     = :ws-type
091615           and CheckSubType  = :ws-check-sub-type
           end-exec
           if sqlcode not = 0
              display "Error: cannot read row "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 4200-exit
           end-if
pemtst*    display ' status ' db-app-status
pemtst*    display ' by     ' db-app-by
pemtst*    display ' date   ' db-app-date
pemtst*    display ' batch  ' db-app-batch
pemtst*    if nu-app-date = -1
pemtst*       display ' approval date is low-values '
pemtst*    else
pemtst*       display ' approval date is NOT low values '
pemtst*    end-if
           .
       4200-exit.
           exit.
       4250-delete-row.
           EXEC SQL
              DELETE
                 from ChkApp_Check
              WHERE
                 Company           = :ws-compid
                 and CertCarrier   = :ws-carrier
                 and CertGroup     = :ws-grouping
                 and CertState     = :ws-state
                 and CertAccount   = :ws-account
                 and CertEffDate   = :ws-eff-date
                 and CertNumber    = :ws-certificate
                 and CertNumberSuf = :ws-cert-sfx
                 and CheckSeqNbr   = :ws-seq-no
                 and CheckType     = :ws-type
091615           and CheckSubType  = :ws-check-sub-type
           END-EXEC
           if sqlcode not = 0
              display "Error: cannot delete row  "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 4250-exit
           end-if
           set row-deleted         to true
           EXEC SQL
               commit transaction
           END-EXEC
           if sqlcode not = 0
              display "Error: commit "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 4250-exit
           end-if
           set tbl-commited to true
           .
       4250-exit.
           exit.
       4300-FINISH-UP-DB.
           EXEC SQL
               commit work release
           END-EXEC
           if sqlcode not = 0
              display "Error: commit release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if
           .
       4300-EXIT.
           EXIT.
02160  5000-BROWSE-FILE.
02161      
      * EXEC CICS HANDLE CONDITION
02162 *        NOTFND   (5560-END-OF-FILE)
02163 *        ENDFILE  (5560-END-OF-FILE)
02164 *    END-EXEC.
      *    MOVE '"$I''                  ! , #00007840' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303037383430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02165
02166      MOVE PI-ERCHEK-KEY          TO ERCHEK-KEY.
02167
02168      IF SEQI = LOW-VALUES
02169          MOVE +1                 TO CHEK-RECORD-SEQ.
02170
02171      IF EIBAID = DFHPF2
02172          GO TO 5100-BROWSE-BKWD.
02173
02174  5010-READ-LOOP.
02175      IF EIBAID = DFHPF1
02176          ADD +1                  TO CHEK-RECORD-SEQ.
02177
02178      
      * EXEC CICS READ
02179 *        DATASET  (ERCHEK-FILE-ID)
02180 *        SET      (ADDRESS OF CHECK-RECORDS)
02181 *        RIDFLD   (ERCHEK-KEY)
02182 *        GTEQ
02183 *    END-EXEC.
      *    MOVE '&"S        G          (   #00007857' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383537' TO DFHEIV0(25:11)
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
           
02184
02185      IF CH-COMPANY-CD NOT = PI-COMPANY-CD
02186          IF EIBAID = DFHENTER
02187              GO TO 5550-NO-RECORD
02188          ELSE
02189              GO TO 5560-END-OF-FILE.
02190
02192         IF PI-CHEK-CARRIER    = CH-CARRIER     AND
02193            PI-CHEK-GROUPING   = CH-GROUPING    AND
02194            PI-CHEK-STATE      = CH-STATE       AND
02195            PI-CHEK-ACCOUNT    = CH-ACCOUNT     AND
02196            PI-CHEK-EFF-DT     = CH-CERT-EFF-DT AND
02197            PI-CHEK-CERT-NO    = CH-CERT-PRIME  AND
02198            PI-CHEK-SUF-NO     = CH-CERT-SFX
02199               GO TO 5500-FORMAT-SCREEN
02200             ELSE
02201               IF EIBAID = DFHPF1
02202                   GO TO 5560-END-OF-FILE
02203                 ELSE
02204                   GO TO 5550-NO-RECORD.
02205
02206      IF EIBAID = DFHENTER
02207         IF CHEK-CARRIER    = CH-CARRIER     AND
02208            CHEK-GROUPING   = CH-GROUPING    AND
02209            CHEK-STATE      = CH-STATE       AND
02210            CHEK-ACCOUNT    = CH-ACCOUNT     AND
02211            CHEK-EFF-DT     = CH-CERT-EFF-DT AND
02212            CHEK-CERT-NO    = CH-CERT-PRIME  AND
02213            CHEK-SUF-NO     = CH-CERT-SFX    AND
02214            CHEK-RECORD-SEQ = CH-SEQUENCE-NO
02215               GO TO 5500-FORMAT-SCREEN
02216         ELSE
02217               GO TO 5550-NO-RECORD.
02218
02219      GO TO 5500-FORMAT-SCREEN.
02220
02221      EJECT
02222
02223  5100-BROWSE-BKWD.
02224      
      * EXEC CICS STARTBR
02225 *        DATASET  (ERCHEK-FILE-ID)
02226 *        RIDFLD   (ERCHEK-KEY)
02227 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007902' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 ERCHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02228
02229      
      * EXEC CICS READPREV
02230 *        DATASET  (ERCHEK-FILE-ID)
02231 *        SET      (ADDRESS OF CHECK-RECORDS)
02232 *        RIDFLD   (ERCHEK-KEY)
02233 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00007907' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393037' TO DFHEIV0(25:11)
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
           
02234
02235  5110-READ-LOOP.
02236      IF PI-FILE-EOF
02237          MOVE SPACE              TO PI-EOF-SW
02238      ELSE
02239          
      * EXEC CICS READPREV
02240 *            DATASET  (ERCHEK-FILE-ID)
02241 *            SET      (ADDRESS OF CHECK-RECORDS)
02242 *            RIDFLD   (ERCHEK-KEY)
02243 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00007917' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393137' TO DFHEIV0(25:11)
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
           
02244
02245      IF CH-COMPANY-CD NOT = PI-COMPANY-CD
02246          GO TO 5560-END-OF-FILE.
02247
02249         IF PI-CHEK-CARRIER    = CH-CARRIER     AND
02250            PI-CHEK-GROUPING   = CH-GROUPING    AND
02251            PI-CHEK-STATE      = CH-STATE       AND
02252            PI-CHEK-ACCOUNT    = CH-ACCOUNT     AND
02253            PI-CHEK-EFF-DT     = CH-CERT-EFF-DT AND
02254            PI-CHEK-CERT-NO    = CH-CERT-PRIME  AND
02255            PI-CHEK-SUF-NO     = CH-CERT-SFX
02256               GO TO 5500-FORMAT-SCREEN
02257             ELSE
02258               GO TO 5560-END-OF-FILE.
02259
02260      GO TO 5500-FORMAT-SCREEN.
02261
02262      EJECT
02263 *                          COPY ELCNPD.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCNPD                             *
00004 *                            VMOD=2.001                         *
00005 *****************************************************************
00006
00007  5200-MOVE-NAME.
00008 *                THE FOLLOWING ROUTINE MOVES THE INSURRED'S     *
00009 *            NAME TO A WORK AREA WITH NO EMBEDDED               *
00010 *            BLANKS.                                            *
00011 *                                                               *
00012 *                  FIELD               VALUE                    *
00013 *                                                               *
00014 *                LAST NAME (CL15)      SMITH                    *
00015 *                1ST NAME  (CL12)      JOHN                     *
00016 *                MID NAME  (CL1)       A                        *
00017 *                                                               *
00018 *                AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30)  *
00019 *         *                                                     *
00020 *         *              JOHN A. SMITH                          *
00021 *         *                                                     *
00022 *         *      TO USE THIS ROUTINE YOU ALSO NEED A WORKING    *
00023 *         *  STORAGE COPYBOOK:                                  *
00024 *         *                                                     *
00025 *         *      01  WS-NAME-WORK-AREA COPY ELCNWA.             *
00026 *         *******************************************************.
00027
00028      MOVE SPACES                 TO  WS-NAME-WORK
00029                                      WS-NAME-WORK2.
00030      MOVE ZERO                   TO  WS-NAME-SW.
00031      SET NWA-INDEX TO +1.
00032
00033      IF WS-INSURED-1ST-NAME = SPACES  AND
00034         WS-INSURED-MID-INIT = SPACES
00035          MOVE WS-INSURED-LAST-NAME TO WS-NAME-WORK
00036          GO TO 5200-EXIT.
00037
00038      MOVE WS-INSURED-1ST-NAME    TO  WS-NAME-WORK2.
00039      PERFORM 5300-MOVE-NAME THRU 5390-EXIT.
00040
00041      SET NWA-INDEX UP BY +1
00042      IF WS-INSURED-MID-INIT NOT = SPACES
00043         MOVE WS-INSURED-MID-INIT   TO  WS-NW (NWA-INDEX)
00044         SET NWA-INDEX UP BY +1
00045         MOVE '.'                   TO  WS-NW (NWA-INDEX)
00046         SET NWA-INDEX UP BY +2.
00047
00048      MOVE WS-INSURED-LAST-NAME  TO  WS-NAME-WORK2.
00049      PERFORM 5300-MOVE-NAME THRU 5390-EXIT.
00050
00051
00052  5200-EXIT.
00053      EXIT.
00054
00055      EJECT
00056  5300-MOVE-NAME SECTION.
00057      IF WS-NAME-SW GREATER THAN +1
00058          GO TO 5390-EXIT.
00059
00060      IF WS-NAME-WORK2 = SPACES
00061          GO TO 5390-EXIT.
00062
00063      SET NWA-INDEX2 TO +1.
00064      SET NWA-INDEX3 TO +2.
00065
00066  5310-MOVE-NAME.
00067      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
00068
00069      IF NWA-INDEX LESS THAN +30
00070         SET NWA-INDEX UP BY +1
00071      ELSE
00072         ADD +2  TO  WS-NAME-SW
00073         GO TO 5390-EXIT.
00074
00075      IF NWA-INDEX2 LESS THAN +20
00076          SET NWA-INDEX2 UP BY +1
00077          SET NWA-INDEX3 UP BY +1.
00078
00079      IF WS-NW2 (NWA-INDEX2) = SPACES AND
00080         WS-NW2 (NWA-INDEX3) = SPACES
00081         GO TO 5390-EXIT.
00082
00083      GO TO 5310-MOVE-NAME.
00084
00085  5390-EXIT.
00086      EXIT.
00087
00088      EJECT
02264      EJECT
02265  5500-FORMAT-SCREEN.
02266      MOVE LOW-VALUES             TO  EL677AI.
111418     if pi-void-reissue-pass = '1'
111418        move 'R'                 to mainti
111418                                    pi-prev-maint
111418     else
111418        MOVE 'S'                 TO MAINTI
111418                                    PI-PREV-MAINT
111418     end-if
02268      MOVE +1                     TO  MAINTL.
02269      MOVE CH-CONTROL-PRIMARY     TO  PI-ERCHEK-KEY.
02270      MOVE CH-CARRIER             TO  CARRIERO.
02271      MOVE CH-GROUPING            TO  GROUPO.
02272      MOVE CH-STATE               TO  STATEO.
02273      MOVE CH-ACCOUNT             TO  ACCTO.
02274
02275      MOVE CH-CERT-EFF-DT         TO  DC-BIN-DATE-1.
02276      MOVE SPACE                  TO  DC-OPTION-CODE.
02277      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
02278      MOVE DC-GREG-DATE-1-EDIT    TO  EFFDTI.
02279
02280      MOVE CH-CERT-PRIME          TO  CERTNOO.
02281      MOVE CH-CERT-SFX            TO  SFXO.
02282      MOVE CH-SEQUENCE-NO         TO  SEQO.
02283
02284      MOVE AL-UANON               TO  CARRIERA
02285                                      GROUPA
02286                                      STATEA
02287                                      ACCTA
02288                                      EFFDTA
02289                                      CERTNOA
02290                                      SFXA.
02291      MOVE CH-PAYEE-NAME-1         TO PI-PAYTO1.
02292
           if ch-approval-dt not = low-values
              move ch-approval-dt      to dc-bin-date-1
              move ' '                 to dc-option-code
              perform 8500-date-convert thru 8500-exit
              move dc-greg-date-1-edit to apvdto
           end-if
           evaluate ch-approval-status
              when '2'
                 move 'PENDING '       TO APVSTATO
              when 'P'
                 move 'PENDING '       TO APVSTATO
              when 'A'
                 move 'APPROVED'       to apvstato
              when 'D'
                 move 'DENIED'         to apvstato
           end-evaluate
           move ch-approved-by         to apvbyo
02293 *    IF PI-COMPANY-ID = 'TMS'
02294 *        MOVE CH-PAYEE-NAME-2    TO  PAYTO1O
02295 *        MOVE CH-LIENHOLDER-NAME TO  PAYTO1AO
02296 *    ELSE
02297          MOVE CH-PAYEE-NAME-1    TO  PAYTO1O
02298          MOVE CH-PAYEE-NAME-2    TO  PAYTO2O.
02299
02300      MOVE CH-PAYEE-ADDRESS-1     TO  PAYAD1O.
02301      MOVE CH-PAYEE-ADDRESS-2     TO  PAYAD2O.
           move ch-payee-city          to payctyo
           move ch-payee-state         to paysto
02303      MOVE CH-PAYEE-CODE          TO  PAYEEO.
02304
02305      IF CH-PAYEE-ZIP-CODE NOT = ZEROS
02306          MOVE CH-PAYEE-ZIP-CODE  TO  PTOZIPO
02307          MOVE AL-UANON           TO  PTOZIPA.
02308
           MOVE CH-RETURN-TO           TO  RETTOO
      *    if ch-check-cashed-dt not = spaces and low-values
      *       move ch-check-cashed-dt  to dc-bin-date-1
02310 *       MOVE SPACE               TO  DC-OPTION-CODE
02311 *       PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
02312 *       MOVE DC-GREG-DATE-1-EDIT TO  CASHEDO
      *    end-if
02309      MOVE CH-RECORDED-DT         TO  DC-BIN-DATE-1.
02310      MOVE SPACE                  TO  DC-OPTION-CODE.
02311      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
02312      MOVE DC-GREG-DATE-1-EDIT    TO  CREATEDO.
02313
02314      MOVE CH-RECORDED-BY         TO  CBYO.
02315
02321      IF CH-VOID-DT = SPACES OR LOW-VALUES OR ZEROS
02322          NEXT SENTENCE
02323      ELSE
02324          MOVE CH-VOID-DT             TO  DC-BIN-DATE-1
02325          MOVE SPACE                  TO  DC-OPTION-CODE
02326          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
02327          MOVE DC-GREG-DATE-1-EDIT    TO  VOIDEDO
02328          MOVE CH-VOID-BY             TO  VBYO.
02329
02330      IF CH-CHECK-WRITTEN-DT = SPACES OR LOW-VALUES OR ZEROS
02331          NEXT SENTENCE
02332      ELSE
02333          MOVE CH-CHECK-WRITTEN-DT    TO  DC-BIN-DATE-1
02334          MOVE SPACE                  TO  DC-OPTION-CODE
02335          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
02336          MOVE DC-GREG-DATE-1-EDIT    TO  PRINTEDI.
02337
02338      MOVE CH-AMOUNT-PAID         TO  AMOUNTO
111418                                     pi-void-reissue-amt
02339      MOVE AL-SANOF               TO  AMOUNTA.
02340      MOVE CH-CHECK-ORIGIN-SW     TO  TYPEI.
02341      MOVE AL-SANOF               TO  TYPEA.
02342      MOVE CH-CHECK-NO            TO  CHECKO.
           move ch-deduct-commission   to dedcyno
02343      MOVE CH-REASON-FOR-CHECK    TO  REASONO.
02344      MOVE CH-VOID-REASON         TO  VREASONO.
02345
02350      MOVE CH-STUB-LINE-1         TO  STUBO.
02351      MOVE CH-TEXT-LINE-1         TO  TEXT1O.
02352      MOVE CH-TEXT-LINE-2         TO  TEXT2O.
02353      MOVE CH-TEXT-LINE-3         TO  TEXT3O.
02374      IF CH-LF-REFUND NOT NUMERIC
02375          MOVE ZEROS              TO CH-LF-REFUND.
02376      IF CH-AH-REFUND NOT NUMERIC
02377          MOVE ZEROS              TO CH-AH-REFUND.
           if (ch-check-no not = spaces and low-values)
pemtst        and (ch-check-written-dt not = low-values)
              perform 5800-fetch-check-cashed-dt
                                       thru 5800-exit
           end-if
02388      IF CH-CHECK-WRITTEN-DT GREATER LOW-VALUES OR
02389         CH-VOID-DT          GREATER LOW-VALUES
02390         MOVE AL-SANON            TO  CHECKA    REASONA  VREASONA
02391                                      STUBA     TEXT1A   TEXT2A
02392                                      TEXT3A    PAYTO1A  SFXA
02393                                      PAYTO2A   PAYAD1A  PAYctyA
                                           paysta    RETTOA   PAYAD2A
02394                                      PTOZIPA
02396                                      PRINTEDA
02397      ELSE
02398         MOVE AL-UANON            TO  CARRIERA  GROUPA   STATEA
02399                                      ACCTA     EFFDTA   CERTNOA
02400                                      CHECKA    REASONA  VREASONA
02401                                      STUBA     TEXT1A   TEXT2A
02402                                      TEXT3A    PAYTO1A  SFXA
02403                                      PAYTO2A   PAYAD1A  PAYctyA
                                           paysta    RETTOA   PAYAD2A
02405                                      PRINTEDA.
02406
02407      MOVE AL-UNNON               TO SEQA.
           perform 5600-get-erpndb     thru 5600-exit
           perform 5700-get-elcert     thru 5700-exit
091615*    move pi-prev-paid           to prepdo
           move al-sadof               to drefa
091615*    if WS-PNDB-FOUND
091615*       compute refo = pb-c-lf-cancel-amt + pb-c-ah-cancel-amt
091615*    end-if
           if WS-CERT-FOUND
              compute ws-tot-lf-prem = cm-lf-premium-amt +
                 cm-lf-alt-premium-amt
              compute ws-tot-iss-prem  =
                 cm-ah-premium-amt + ws-tot-lf-prem
              compute ws-tot-iss-comm =
                 (ws-tot-lf-prem * cm-life-comm-pct) +
                 (cm-ah-premium-amt * cm-ah-comm-pct)
              compute ws-tot-ref-comm =
                 (pi-lf-refund * cm-life-comm-pct) +
                 (pi-ah-refund * cm-ah-comm-pct)
              move ws-tot-iss-prem     to premo
              move ws-tot-iss-comm     to isscommo
091615*       move ws-tot-ref-comm     to uecommo
           end-if
           if PI-TO-EL677-FROM-EL1273
              move zeros               to premo
                                          isscommo
                                          uecommo
                                          prepdo
                                          refo
           end-if
           GO TO 8100-SEND-INITIAL-MAP
           .
02413  5550-NO-RECORD.
02414      MOVE ER-1162                TO  EMI-ERROR.
02415      MOVE -1                     TO  CARRIERL.
02416      MOVE AL-UABON               TO  CARRIERA  GROUPA  STATEA
02417                                      ACCTA     EFFDTA  CERTNOA
02418                                      SFXA      SEQA.
02419      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02420
02421      IF (NOT PI-TO-EL677-FROM-EL1273)
091615        and (pi-return-to-program not = 'EL6315')
02422          GO TO 8200-SEND-DATAONLY.
02423
02424      MOVE CHEK-CARRIER           TO  CARRIERO.
02425      MOVE CHEK-GROUPING          TO  GROUPO.
02426      MOVE CHEK-STATE             TO  STATEO.
02427      MOVE CHEK-ACCOUNT           TO  ACCTO.
02428
02429      MOVE CHEK-EFF-DT            TO  DC-BIN-DATE-1.
02430      MOVE SPACE                  TO  DC-OPTION-CODE.
02431      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
02432      MOVE DC-GREG-DATE-1-EDIT    TO  EFFDTI.
02433
02434      MOVE CHEK-CERT-NO           TO  CERTNOO.
02435      MOVE CHEK-SUF-NO            TO  SFXO.
02436      GO TO 8100-SEND-INITIAL-MAP.
02437
02438  5560-END-OF-FILE.
02439      IF EIBAID = DFHPF1
02440          MOVE 'Y'                TO  PI-EOF-SW
02441          MOVE ER-2237            TO  EMI-ERROR
02442      ELSE
02443      IF EIBAID = DFHENTER
02444         GO TO 5550-NO-RECORD
02445      ELSE
02446 *        MOVE SPACES             TO  PI-ERCHEK-KEY
02447          MOVE ER-2238            TO  EMI-ERROR.
02448
02449      MOVE -1                     TO  MAINTL.
02450      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02451      GO TO 8200-SEND-DATAONLY.
02452      EJECT
       5600-get-erpndb.
           MOVE 'N'                    TO WS-PNDB-FOUND-SW.
           MOVE PI-COMPANY-CD          TO ERPNDB-ALT-COMPANY-CD.
           MOVE PI-CARRIER             TO ERPNDB-ALT-CARRIER.
           MOVE PI-GROUPING            TO ERPNDB-ALT-GROUPING.
           MOVE PI-STATE               TO ERPNDB-ALT-STATE.
           MOVE PI-ACCOUNT             TO ERPNDB-ALT-ACCOUNT.
           MOVE PI-CERT-PRIME          TO ERPNDB-ALT-CERT-PRIME.
           MOVE PI-CERT-SFX            TO ERPNDB-ALT-CERT-SFX.
           MOVE PI-CERT-EFF-DT         TO ERPNDB-ALT-CERT-EFF-DT.
           MOVE ZEROS                  TO ERPNDB-ALT-CH-SEQ-NO.
           MOVE '2'                    TO ERPNDB-ALT-RECORD-TYPE.
           IF ST-ACCNT-CNTL  OR
              ACCNT-CNTL
                MOVE SPACES             TO  ERPNDB-ALT-CARRIER.
           IF ST-ACCNT-CNTL      OR
              CARR-ST-ACCNT-CNTL OR
              ACCNT-CNTL         OR
              CARR-ACCNT-CNTL
                MOVE SPACES             TO  ERPNDB-ALT-GROUPING.
           IF ACCNT-CNTL OR
              CARR-ACCNT-CNTL
                MOVE SPACES             TO  ERPNDB-ALT-STATE.
           
      * EXEC CICS READ
      *         DATASET   (ERPNDB-ALT-FILE-ID)
      *         SET       (ADDRESS OF PENDING-BUSINESS)
      *         RIDFLD    (ERPNDB-ALT-KEY)
      *         resp      (ws-response)
      *    END-EXEC
      *    MOVE '&"S        E          (  N#00008268' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038323638' TO DFHEIV0(25:11)
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
           if resp-normal
              MOVE 'Y'                 TO WS-PNDB-FOUND-SW
              compute pi-refund-on-pending-rec =
                 pb-c-lf-cancel-amt + pb-c-ah-cancel-amt
           end-if
           .
       5600-exit.
           exit.
       5700-get-elcert.
           MOVE PI-COMPANY-CD          TO  ELCERT-COMPANY-CD.
           MOVE PI-CARRIER             TO  ELCERT-CARRIER.
           MOVE PI-GROUPING            TO  ELCERT-GROUPING.
           MOVE PI-STATE               TO  ELCERT-STATE.
           MOVE PI-ACCOUNT             TO  ELCERT-ACCOUNT.
           MOVE PI-CERT-PRIME          TO  ELCERT-CERT-PRIME.
           MOVE PI-CERT-SFX            TO  ELCERT-CERT-SFX.
           MOVE PI-CERT-EFF-DT         TO  ELCERT-CERT-EFF-DT.
           MOVE 'N'                    TO  WS-CERT-FOUND-SW.
           
      * EXEC CICS READ
      *         DATASET   (ELCERT-FILE-ID)
      *         SET       (ADDRESS OF CERTIFICATE-MASTER)
      *         RIDFLD    (ELCERT-KEY)
      *         resp      (ws-response)
      *    END-EXEC
      *    MOVE '&"S        E          (  N#00008292' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038323932' TO DFHEIV0(25:11)
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
           if resp-normal
              MOVE 'Y'                 TO WS-CERT-FOUND-SW
           end-if
           .
       5700-exit.
           exit.
       5800-fetch-check-cashed-dt.
           move ' '                    to ws-connect-sw
                                          ws-match-sw
030414                                    pi-check-cashed
011822     move 'HOVTSTDB01_PdBnkInfo' to svr
011822     move 'appuser'              to usr
011822     move 'appuser@cso'          to pass
011822
011822     if ws-kix-myenv = 'cid1p'
011822        move 'SDVDB01_PdBnkInfo' to svr
011822     end-if
           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string
           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
           if sqlcode not = 0
              display "Error: cannot connect to pdbnkinfo "
              display sqlcode
              display sqlerrmc
              go to 5800-exit
           end-if
           set connected-to-db to true
           if pi-company-id = 'CID'
              move 'CSO - AP%'         to ws-pb-compid
           else
              move 'AHL - AP%'         to ws-pb-compid
           end-if
           move '000'                  to ws-pb-check-no (1:3)
           move ch-check-no            to ws-pb-check-no (4:7)
           move spaces                 to ws-pb-bad-check-no
           perform varying s1 from +1 by +1 until
              (s1 > +10)
              or (ws-pb-check-no (s1:1) not = '0')
           end-perform
           if s1 < +11
              move ws-pb-check-no (s1:11 - s1)
                                  to ws-pb-bad-check-no (1: 11 - s1)
           end-if
      *    display ' compid **' ws-pb-compid '**'
      *    display ' ckno  **' ws-pb-check-no '**'
021714     MOVE ch-AMOUNT-PAID      TO WS-CHECK-AMT-TMP
021714     MOVE WS-CHECK-AMT-TMPX   TO WS-CHECK-AMOUNT
021714     MOVE SPACES              TO pb-paid-date
021714
021714     EXEC SQL
021714        CALL pbi_GetCashDate_by_TransactionNbr
021714           @compid          = :ws-pb-compid,
021714           @checkno         = :ws-pb-check-no,
021714           @badcheckno      = :ws-pb-bad-check-no,
021714           @checkamount     = :ws-check-amount,
111219           @checkcasheddate = :pb-paid-date :nu-app-date OUT
021714     END-EXEC
           if sqlcode not = 0
              display "Error: cannot run stor proc  "
              move sqlcode            to ws-sql-code
              move ws-sql-code to ws-dis-sql-code
              display ' dis sql code ' ws-dis-sql-code
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 5800-disconnect
           end-if
111219     if nu-app-date = -1 *> no check cashed date
111219        go to 5800-disconnect
111219     end-if
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  The following code works just as well as the above        ***
      ***  stored procedure and just as fast.                        ***
      ***  Originally, I had declared a cursor and fetched through   ***
      ***  the record set to find the correct check and that REALLY  ***
      ***  slowed the response down to ~ 3/4 second.                 ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
      *     EXEC SQL
      *        SELECT
      *           PaidDate
      *        INTO
      *           :pb-paid-date
      *        FROM
      *           dbo.cso_pbi_TransactionLookup
      *        WHERE
      *           (BankAcctDescr like :ws-pb-compid)
      *           and (Amount = :ws-check-amount)
      *           and ((transactionNbr = :ws-pb-check-no)
      *                  or
      *               (transactionnbr = :ws-pb-bad-check-no))
      **          and (CAST(TransactionNbr AS INT) =
      **             :ws-pb-check-no-num)
      **          and (right('00000' + rtrim(TransactionNbr),10)
      **             = :ws-pb-check-no)
      **          and (TransactionNbr  = :ws-pb-check-no)
      *     END-EXEC
pemtst*    display ' chkno  ' pb-check-no
pemtst*    display ' desc   ' pb-bank-acct-desc
pemtst*    display ' trn typ' pb-tran-type
pemtst*    display ' amt   *' pb-amount '**'
pemtst*    display ' pd dte ' pb-paid-date
           string pb-paid-date (1:4)
                  pb-paid-date (6:2)
                  pb-paid-date (9:2)
              delimited by size into dc-greg-date-cymd-r
           end-string
           move 'L'                    to dc-option-code
           perform 8500-date-convert   thru 8500-exit
           if no-conversion-error
      *       move dc-bin-date-1       to ws-bin-cashed-dt
              move dc-greg-date-1-edit to cashedo
030414        move 'Y' to pi-check-cashed
           else
              display ' error cvtdte cash dt ' pb-paid-date ' '
                 dc-error-code
           end-if
           .
       5800-disconnect.
           EXEC SQL
              DISCONNECT ALL
           END-EXEC
           if sqlcode not = 0
              display "Error: cannot disconnect pdbnk "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if
           .
       5800-exit.
           exit.
02454  6000-VERIFY-COMP-MASTER.
02455      
      * EXEC CICS HANDLE CONDITION
02456 *        NOTFND   (6070-NO-COMP-MSTR)
02457 *    END-EXEC.
      *    MOVE '"$I                   ! - #00008434' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303038343334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02458
02459      MOVE PI-COMPANY-CD          TO  ERCOMP-COMP-CD.
02460
02461      IF NOT PI-ZERO-CARRIER AND
02462         NOT PI-ZERO-CAR-GROUP
02463          MOVE PI-CR-CARRIER      TO  ERCOMP-CARRIER
02464      ELSE
02465          MOVE ZEROS              TO  ERCOMP-CARRIER.
02466
02467      IF NOT PI-ZERO-GROUPING  AND
02468         NOT PI-ZERO-CAR-GROUP
02469          MOVE PI-CR-GROUPING     TO  ERCOMP-GROUPING
02470      ELSE
02471          MOVE ZEROS              TO  ERCOMP-GROUPING.
02472
02473      MOVE PI-CR-FIN-RESP         TO  ERCOMP-FIN-RESP.
02474      MOVE PI-CR-ACCOUNT          TO  ERCOMP-ACCOUNT.
02475      MOVE 'A'                    TO  ERCOMP-RECORD-TYPE.
02476
02477      
      * EXEC CICS READ
02478 *         DATASET   (ERCOMP-FILE-ID)
02479 *         SET       (ADDRESS OF COMPENSATION-MASTER)
02480 *         RIDFLD    (ERCOMP-KEY)
02481 *     END-EXEC.
      *    MOVE '&"S        E          (   #00008456' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343536' TO DFHEIV0(25:11)
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
           
02482
02483      GO TO 6090-EXIT.
02484
02485  6070-NO-COMP-MSTR.
02486      MOVE -1                     TO  CARRIERL.
02487      MOVE ER-2230                TO  EMI-ERROR.
02488      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02489      GO TO 8200-SEND-DATAONLY.
02490
02491  6090-EXIT.
02492       EXIT.
02493      EJECT
02494  6100-VERIFY-CERTIFICATE.
02495      
      * EXEC CICS HANDLE CONDITION
02496 *        NOTFND   (6170-NO-CERTIFICATE)
02497 *    END-EXEC.
      *    MOVE '"$I                   ! . #00008474' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303038343734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02498
02499      MOVE PI-COMPANY-CD          TO  ELCERT-COMPANY-CD.
02500      MOVE PI-CARRIER             TO  ELCERT-CARRIER.
02501      MOVE PI-GROUPING            TO  ELCERT-GROUPING.
02502      MOVE PI-STATE               TO  ELCERT-STATE.
02503      MOVE PI-ACCOUNT             TO  ELCERT-ACCOUNT.
02504      MOVE PI-CERT-PRIME          TO  ELCERT-CERT-PRIME.
02505      MOVE PI-CERT-SFX            TO  ELCERT-CERT-SFX.
02506      MOVE PI-CERT-EFF-DT         TO  ELCERT-CERT-EFF-DT.
02507
02508      MOVE 'N'                    TO  WS-CERT-FOUND-SW.
02509
02510      MOVE SPACES                 TO  PI-REFERENCE
02511 *                                    PI-INSURED-NAME.
02512      MOVE LOW-VALUES             TO  PI-CANC-DT.
02513 *    MOVE ZEROS                  TO  PI-LF-REFUND
02514 *                                    PI-AH-REFUND.
02515
02516      
      * EXEC CICS READ
02517 *         DATASET   (ELCERT-FILE-ID)
02518 *         SET       (ADDRESS OF CERTIFICATE-MASTER)
02519 *         RIDFLD    (ELCERT-KEY)
02520 *     END-EXEC.
      *    MOVE '&"S        E          (   #00008495' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343935' TO DFHEIV0(25:11)
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
           
02521
02522      MOVE 'Y'                    TO  WS-CERT-FOUND-SW.
02523
02524 *    MOVE CM-LF-ITD-CANCEL-AMT   TO  PI-LF-REFUND.
02525 *    MOVE CM-AH-ITD-CANCEL-AMT   TO  PI-AH-REFUND.
02526      IF CM-LF-CANCEL-DT NOT = LOW-VALUES
02527          MOVE CM-LF-CANCEL-DT    TO  PI-CANC-DT
02528      ELSE
02529          IF CM-AH-CANCEL-DT NOT = LOW-VALUES
02530              MOVE CM-AH-CANCEL-DT TO PI-CANC-DT.
02531 *    MOVE CM-INSURED-LAST-NAME   TO  PI-INSURED-NAME.
02532
02533      GO TO 6190-EXIT.
02534
02535  6170-NO-CERTIFICATE.
02536      MOVE -1                     TO  CARRIERL.
02537      MOVE ER-2726                TO  EMI-ERROR.
02538      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02539
02540  6190-EXIT.
02541       EXIT.
02542
02543      EJECT
02544  6200-UPDATE-PENDING-BUS-REC.
02549      MOVE PI-COMPANY-CD          TO  ERPNDB-ALT-COMPANY-CD.
02550      MOVE PI-CARRIER             TO  ERPNDB-ALT-CARRIER.
02551      MOVE PI-GROUPING            TO  ERPNDB-ALT-GROUPING.
02552      MOVE PI-STATE               TO  ERPNDB-ALT-STATE.
02553      MOVE PI-ACCOUNT             TO  ERPNDB-ALT-ACCOUNT.
02554      MOVE PI-CERT-PRIME          TO  ERPNDB-ALT-CERT-PRIME.
02555      MOVE PI-CERT-SFX            TO  ERPNDB-ALT-CERT-SFX.
02556      MOVE PI-CERT-EFF-DT         TO  ERPNDB-ALT-CERT-EFF-DT.
02557      MOVE ZEROS                  TO  ERPNDB-ALT-CH-SEQ-NO.
02558      MOVE '2'                    TO  ERPNDB-ALT-RECORD-TYPE.
02559
02560      IF ST-ACCNT-CNTL  OR
02561         ACCNT-CNTL
02562           MOVE SPACES             TO  ERPNDB-ALT-CARRIER.
02563
02564      IF ST-ACCNT-CNTL      OR
02565         CARR-ST-ACCNT-CNTL OR
02566         ACCNT-CNTL         OR
02567         CARR-ACCNT-CNTL
02568           MOVE SPACES             TO  ERPNDB-ALT-GROUPING.
02569
02570      IF ACCNT-CNTL OR
02571         CARR-ACCNT-CNTL
02572           MOVE SPACES             TO  ERPNDB-ALT-STATE.
02573
02574      
      * EXEC CICS READ
02575 *         DATASET   (ERPNDB-ALT-FILE-ID)
02576 *         SET       (ADDRESS OF PENDING-BUSINESS)
02577 *         RIDFLD    (ERPNDB-ALT-KEY)
      *         resp      (ws-response)
02578 *     END-EXEC.
      *    MOVE '&"S        E          (  N#00008549' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038353439' TO DFHEIV0(25:11)
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
           
02579
           if not resp-normal
              go to 6200-exit
           end-if
02580      MOVE PB-CONTROL-PRIMARY     TO  ERPNDB-PRIMARY-KEY.
02581
02582      
      * EXEC CICS READ
02583 *         DATASET   (ERPNDB-FILE-ID)
02584 *         SET       (ADDRESS OF PENDING-BUSINESS)
02585 *         RIDFLD    (ERPNDB-PRIMARY-KEY)
02586 *         UPDATE
      *         resp      (ws-response)
02587 *     END-EXEC.
      *    MOVE '&"S        EU         (  N#00008561' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038353631' TO DFHEIV0(25:11)
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
           
02600
02601  6200-EXIT.
02602       EXIT.
       6210-rewrite-pndb.
02596      
      * EXEC CICS REWRITE
02597 *        DATASET  (ERPNDB-FILE-ID)
02598 *        FROM     (PENDING-BUSINESS)
02599 *    END-EXEC
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008572' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       6210-exit.
           exit.
02605  6300-BUILD-CERT-SCREEN.
      *    move 'N'                    to dedcyni
      *                                   pi-prev-ded-comm
           MOVE LOW-VALUES             TO  EL677AI.
           perform 6700-build-common   thru 6700-exit
           if ws-refund-amount = zeros
              move pi-company-cd       to pi-chek-comp-cd
              move pi-carrier          to pi-chek-carrier
              move pi-grouping         to pi-chek-grouping
              move pi-state            to pi-chek-state
              move pi-account          to pi-chek-account
              move pi-cert-eff-dt      to pi-chek-eff-dt
              move pi-cert-prime       to pi-chek-cert-no
              move pi-cert-sfx         to pi-chek-suf-no
              move +1                  to pi-chek-sequence
00494              MOVE 'S'            TO MAINTI
00495              MOVE 1              TO MAINTL
00496              MOVE AL-UABON       TO MAINTA
00497              MOVE DFHENTER       TO EIBAID
              go to 5000-browse-file
           end-if
02643      IF WS-CERT-NOT-FOUND
02644          GO TO 8100-SEND-INITIAL-MAP.
02645
02646 *    IF WS-PNDB-FOUND
02647 *        COMPUTE WS-REFUND-AMOUNT = PB-C-LF-CANCEL-AMT +
02648 *                                   PB-C-AH-CANCEL-AMT
02649 *        MOVE 'R'                TO  TYPEO
02650 *        IF PI-AR-PROCESSING
02651 *            MOVE AL-UANON       TO  REFA
02652 *            MOVE PB-C-REFERENCE TO  REFO
02653 *        END-IF
02654 *    ELSE
02655 *        COMPUTE WS-REFUND-AMOUNT = CM-LF-ITD-CANCEL-AMT +
02656 *                                   CM-AH-ITD-CANCEL-AMT.
02658      MOVE WS-REFUND-AMOUNT        TO AMOUNTO
091615     MOVE pi-check-type           TO TYPEO
02659
02673      IF PI-COMPANY-ID = 'TMS'
02674          MOVE WS-NAME-WORK         TO PAYTO1I
02675                                       PI-PAYTO1
02676          MOVE AL-UANON             TO PAYTO1A
02677      ELSE
02678          MOVE WS-NAME-WORK         TO PAYTO1I
02679                                       PI-PAYTO1
02680          MOVE AL-UANON             TO PAYTO1A.
02681
02682 *    MOVE CM-BENEFICIARY           TO PAYTO1AI.
02683 *    MOVE AL-UANON                 TO PAYTO1AA.
           move 'N'                    to dedcyni
           move al-uanon               to dedcyna
           move 'INS'                  to payeeo
           move al-panon               to payeea
02685      IF PI-MAIL-YES
02686          PERFORM 6400-MAILING-ADDRESS THRU 6490-EXIT.
02687
02688      GO TO 8100-SEND-INITIAL-MAP.
02689
02690      EJECT
02691
02692  6400-MAILING-ADDRESS.
02693      
      * EXEC CICS HANDLE CONDITION
02694 *        NOTFND   (6470-NO-ADDRESS)
02695 *    END-EXEC.
      *    MOVE '"$I                   ! / #00008640' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303038363430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02696
02697      
      * EXEC CICS READ
02698 *        DATASET   (ERMAIL-FILE-ID)
02699 *        SET       (ADDRESS OF MAILING-DATA)
02700 *        RIDFLD    (ELCERT-KEY)
02701 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008644' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363434' TO DFHEIV0(25:11)
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
           
02702
02703      MOVE MA-ADDRESS-LINE-1      TO PAYAD1I.
02704      MOVE MA-ADDRESS-LINE-2      TO PAYAD2I.
           move ma-city                to payctyi
           move ma-addr-state          to paysti
02706      MOVE MA-ZIP                 TO PTOZIPI.
02707      MOVE AL-UANON               TO PAYAD1A
02708                                     PAYAD2A
02709                                     PAYctyA
                                          paysta
02710                                     PTOZIPA.
02711
02712      GO TO 6490-EXIT.
02713
02714  6470-NO-ADDRESS.
02715      MOVE -1                     TO  PAYAD1L.
02716      MOVE ER-2394                TO  EMI-ERROR.
02717      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02718
02719  6490-EXIT.
02720       EXIT.
02721
02722      EJECT
02723
02724  6500-BUILD-ACCOUNT-SCREEN.
           MOVE LOW-VALUES             TO  EL677AI.
           perform 6700-build-common   thru 6700-exit
02658      MOVE WS-REFUND-AMOUNT       TO AMOUNTO
02756      MOVE PI-AM-NAME             TO PAYTO1I
           MOVE SPACES                 TO PAYTO2I
02757      MOVE PI-AM-ADDRS            TO PAYAD1I.
02758      MOVE PI-AM-CITY             TO PAYCTYI
           move pi-am-st               to paysti
02759      MOVE PI-AM-ZIP-CODE         TO PTOZIPI
           move 'ACCT'                 to payeeo
           move al-panon               to payeea
02761      MOVE AL-UANON               TO PAYTO1A
02762                                     PAYTO2A
02763                                     PAYAD1A
                                          PAYCTYA
                                          paysta
02764                                     PTOZIPA
02770      GO TO 8100-SEND-INITIAL-MAP
           .
02774  6600-BUILD-BENEFICIARY-SCREEN.
           MOVE LOW-VALUES             TO  EL677AI.
           perform 6700-build-common   thru 6700-exit
02643      IF WS-CERT-NOT-FOUND
02644          GO TO 8100-SEND-INITIAL-MAP.
02645
02646 *    IF WS-PNDB-FOUND
02647 *        COMPUTE WS-REFUND-AMOUNT = PB-C-LF-CANCEL-AMT +
02648 *                                   PB-C-AH-CANCEL-AMT
02649 *        MOVE 'R'                TO  TYPEO
02650 *        IF PI-AR-PROCESSING
02651 *            MOVE AL-UANON       TO  REFA
02652 *            MOVE PB-C-REFERENCE TO  REFO
02653 *        END-IF
02654 *    ELSE
02655 *        COMPUTE WS-REFUND-AMOUNT = CM-LF-ITD-CANCEL-AMT +
02656 *                                   CM-AH-ITD-CANCEL-AMT.
02657
02658      MOVE WS-REFUND-AMOUNT        TO  AMOUNTO.
091615     MOVE pi-check-type           TO  TYPEO.
02659
02673      IF PI-COMPANY-ID = 'TMS'
02674          MOVE WS-NAME-WORK         TO PAYTO1I
02675                                       PI-PAYTO1
02676          MOVE AL-UANON             TO PAYTO1A
02677      ELSE
02678          MOVE WS-NAME-WORK         TO PAYTO1I
02679                                       PI-PAYTO1
02680          MOVE AL-UANON             TO PAYTO1A.
02681
02682 *    MOVE CM-BENEFICIARY           TO PAYTO1AI.
02683 *    MOVE AL-UANON                 TO PAYTO1AA.
02684
           move 'N'                    to dedcyni
           move al-uanon               to dedcyna
           move 'BENE'                 to payeeo
           move al-panon               to payeea
02685      IF PI-MAIL-YES
02686          PERFORM 6620-MAILING-ADDRESS THRU 6690-EXIT.
02687
02688      GO TO 8100-SEND-INITIAL-MAP.
02689
02690      EJECT
02691
02692  6620-MAILING-ADDRESS.
02693      
      * EXEC CICS HANDLE CONDITION
02694 *        NOTFND   (6670-NO-beneficiary)
02695 *    END-EXEC.
      *    MOVE '"$I                   ! 0 #00008738' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303038373338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02696
02697      
      * EXEC CICS READ
02698 *        DATASET   (ERMAIL-FILE-ID)
02699 *        SET       (ADDRESS OF MAILING-DATA)
02700 *        RIDFLD    (ELCERT-KEY)
02701 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008742' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373432' TO DFHEIV0(25:11)
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
           
02702
           move spaces                 to payto2i
           move ma-cred-bene-name      to payto1i
02703      MOVE MA-cred-bene-addr      TO PAYad1I.
02704      MOVE MA-cred-bene-addr2     TO PAYad2I.
           move ma-cred-bene-city      to payctyi
           move ma-cred-bene-state     to paysti
02706      MOVE MA-cred-bene-zip       TO PTOZIPI.
02707      MOVE AL-UANON               TO payto1a
                                          PAYTO2A
02708                                     PAYad1A
                                          payad2a
02709                                     PAYctyA
                                          paysta
02710                                     PTOZIPA.
02711
02712      GO TO 6690-EXIT.
02713
02835  6670-NO-BENEFICIARY.
02715      MOVE -1                     TO  PAYTO1L.
02716      MOVE ER-2394                TO  EMI-ERROR.
02717      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02718
02719  6690-EXIT.
02720       EXIT.
       6700-build-common.
           if not eracct-found
              move pi-company-cd       to eracct-co
              move pi-carrier          to eracct-carrier
              move pi-grouping         to eracct-grouping
              move pi-state            to eracct-state
              move pi-account          to eracct-account
              move pi-cert-eff-dt      to eracct-exp-date
              perform 1420-get-eracct  thru 1490-exit
           end-if
      *    if WS-PNDB-NOT-FOUND
      *       perform 5600-get-erpndb thru 5600-exit
      *    end-if
      *    MOVE LOW-VALUES             TO  EL677AI.
           MOVE 'A'                    TO  MAINTI.
           MOVE AL-UANON               TO  MAINTA.
           MOVE +1                     TO  MAINTL.
           MOVE PI-CARRIER             TO  CARRIERO.
           MOVE PI-GROUPING            TO  GROUPO.
           MOVE PI-STATE               TO  STATEO.
           MOVE PI-ACCOUNT             TO  ACCTO.
           MOVE PI-CERT-PRIME          TO  CERTNOO.
           MOVE PI-CERT-SFX            TO  SFXO.
           MOVE PI-AMOUNT              TO  AMOUNTO.
091615*    MOVE PI-TYPE                TO  TYPEO.
091615     move pi-check-type          to  typeo
           move pi-return-to           to  rettoo
      *    MOVE PI-REFERENCE           TO  REFO.
           MOVE PI-PAYTO1              TO  PAYTO1O.
           MOVE AL-UANON               TO  CARRIERA
                                           GROUPA
                                           STATEA
                                           ACCTA
                                           CERTNOA
                                           SFXA
                                           PAYTO1A
                                           PAYTO2A
      *                                    AMOUNTA
091615*                                    TYPEA
                                           rettoa
                                           payeea.
091615     move al-panon               to typea
           move al-panon               to amounta
           MOVE PI-CERT-EFF-DT         TO  DC-BIN-DATE-1.
           MOVE ' '                    TO  DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERT.
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-1-EDIT TO  EFFDTI
               MOVE AL-UANON            TO  EFFDTA
               MOVE 'Y'                 TO  PI-PROCESS-CERT-SW
               PERFORM 6100-VERIFY-CERTIFICATE THRU 6190-EXIT
           ELSE
               GO TO 8200-SEND-DATAONLY.
           IF CM-INSURED-FIRST-NAME GREATER SPACES
               MOVE CM-INSURED-FIRST-NAME  TO  WS-INSURED-1ST-NAME
               MOVE CM-INSURED-INITIAL2    TO  WS-INSURED-MID-INIT
           ELSE
               MOVE CM-INSURED-INITIAL1    TO  WS-INSURED-1ST-NAME
               MOVE CM-INSURED-INITIAL2    TO  WS-INSURED-MID-INIT.
           MOVE CM-INSURED-LAST-NAME       TO  WS-INSURED-LAST-NAME.
           PERFORM 5200-MOVE-NAME THRU 5200-EXIT.
           move spaces to pi-insured-name
           string cm-insured-last-name ' '
                  cm-insured-first-name delimited by '  '
              into pi-insured-name
           end-string
           move spaces                 to pi-table-name
           string cm-insured-last-name ', '
                  cm-insured-first-name ' '
                  cm-insured-initial2
              delimited by '  '
              into
                 pi-table-name
           end-string
      *    MOVE WS-NAME-WORK           TO PI-INSURED-NAME.
           if eibaid = dfhpf5
              move 'Y'                 to dedcyni
              move al-uanon            to dedcyna
           end-if
          compute ws-tot-lf-prem = cm-lf-premium-amt +
             cm-lf-alt-premium-amt
          compute ws-tot-iss-prem  =
             cm-ah-premium-amt + ws-tot-lf-prem
          compute ws-tot-iss-comm =
             (ws-tot-lf-prem * cm-life-comm-pct) +
             (cm-ah-premium-amt * cm-ah-comm-pct)
          compute ws-tot-ref-comm =
             (pi-lf-refund * cm-life-comm-pct) +
             (pi-ah-refund * cm-ah-comm-pct)
           move ws-tot-iss-prem        to premo
           move ws-tot-iss-comm        to isscommo
091615     move al-sadof               to drefa
091615                                    refa
091615*    compute refo = pi-lf-refund + pi-ah-refund
           move pi-prev-paid            to prepdo
           move ws-tot-ref-comm         to uecommo
                                           pi-ue-comm
      ***  compute ws-refund-amount = pi-refund-on-pending-rec -
      ***     pi-prev-paid
101216     compute ws-refund-amount =
103116        pi-refund-on-pending-rec - pi-prev-paid-this-month
           if (pi-refund-on-pending-rec + pi-prev-paid) >
              ws-tot-iss-prem
              move zeros               to ws-refund-amount
           end-if
           if ((dedcyni = 'Y')
              or (pi-prev-ded-comm = 'Y'))
              and (ws-refund-amount >= ws-tot-ref-comm)
              compute ws-refund-amount = ws-refund-amount -
                 ws-tot-ref-comm
           end-if
091615     if pi-check-type = 'C'
091615*       move pi-endt-prm-diff    to refo
091615        move pi-endt-com-diff    to uecommo
091615                                    pi-ue-comm
091615        compute ws-refund-amount =
091615           pi-endt-prm-diff - pi-prev-paid
091615        if (dedcyni = 'Y')
091615           or (pi-prev-ded-comm = 'Y')
091615           compute ws-refund-amount = ws-refund-amount -
091615              pi-endt-com-diff
091615        end-if
091615        if ws-refund-amount <= zeros
091615           move pi-endt-prm-diff to ws-refund-amount
091615        end-if
091615     end-if
           if not WS-CERT-NOT-FOUND
              string
                 pi-cert-prime         ' : '
                 cm-insured-last-name  ', '
                 cm-insured-first-name ' : '
                 pi-account            ' : '
                 pi-am-csr delimited by '  ' into text1o
              end-string
              move al-uanon            to text1a
              MOVE 'REF'               TO WS-USER-REASON
091615        if pi-check-type = 'C'
091615           move 'COR'            to ws-user-reason
091615        end-if
              MOVE pi-cert-no          TO WS-USER-CERT-NO
              inspect ws-user-cert-no replacing leading zeros
                 by spaces
              move pi-insured-name     to ws-user-name
              perform varying s1 from +1 by +1 until
                 (s1 > +11)
                 or (ws-user-cert-no (s1:1)) <> ' '
              end-perform
              string ws-user-reason  ' ' delimited by size
                     ws-user-cert-no (s1:11 - s1 + 1) ' '
                        delimited by size
                     cm-insured-last-name ' '
                     cm-insured-first-name delimited by '  '
                 into stubo
              end-string
              move al-uanon            to stuba
           end-if
           .
       6700-exit.
           exit.
       6800-browse-previous-chek-recs.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      **** this should be the first time through, we need to gather  ***
      **** all the erchek records for this pending record, add the   ***
      **** amounts and make sure this check request will not exceed  ***
      **** the collected premium                                     ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
           move zeros                  to pi-prev-paid
103116                                    pi-prev-paid-this-month
                                          ws-browse-sw
                                          pi-chek-rec-cnt
           move spaces                 to pi-prev-ded-comm
           move pi-company-cd          to chek-company-cd
           move pi-carrier             to chek-carrier
           move pi-grouping            to chek-grouping
           move pi-state               to chek-state
           move pi-account             to chek-account
           move pi-cert-prime          to chek-cert-no
           move pi-cert-sfx            to chek-suf-no
           move pi-cert-eff-dt         to chek-eff-dt
           move +0                     to chek-record-seq
           move erchek-key             to ws-compare-erchek-key
           
      * EXEC CICS STARTBR
      *       DATASET  (ERCHEK-FILE-ID)
      *       RIDFLD   (ERCHEK-KEY)
      *       resp     (ws-response)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00008955' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303038393535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 ERCHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              go to 6800-exit
           end-if
           perform until i-say-when
              
      * EXEC CICS READNEXT
      *          DATASET   (ERCHEK-FILE-ID)
      *          SET       (ADDRESS OF CHECK-RECORDS)
      *          RIDFLD    (ERCHEK-KEY)
      *          resp      (ws-response)
      *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00008964' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303038393634' TO DFHEIV0(25:11)
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
              if resp-normal
                 and erchek-key (1:33) = ws-compare-erchek-key
                 if (ch-void-dt = low-values)
                    and (not ch-denied)
091615              if pi-check-type = 'R'
091615                 and ch-check-origin-sw = 'C'
091615                 continue
091615              else
091615                 compute pi-prev-paid = pi-prev-paid +
091615                    ch-amount-paid
091615                 add +1 to pi-chek-rec-cnt
091615                 move ch-deduct-commission
091615                                 to pi-prev-ded-comm
103116                 if ch-credit-select-dt = pi-cr-month-end-dt
103116                    compute pi-prev-paid-this-month =
103116                       pi-prev-paid-this-month + ch-amount-paid
                       end-if
091615              end-if
                 end-if
              else
                 set i-say-when to true
              end-if
           end-perform
           .
       6800-exit.
           exit.
02846  7000-SET-PI-AREA.
02847      MOVE CARRIERI               TO  PI-CARRIER
02848                                      PI-CHEK-CARRIER.
02849      MOVE GROUPI                 TO  PI-GROUPING
02850                                      PI-CHEK-GROUPING.
02851      MOVE STATEI                 TO  PI-STATE
02852                                      PI-CHEK-STATE.
02853      MOVE ACCTI                  TO  PI-ACCOUNT
02854                                      PI-CHEK-ACCOUNT.
02855      MOVE CERTNOI                TO  PI-CERT-PRIME
02856                                      PI-CHEK-CERT-NO.
02857      IF SFXI NOT = LOW-VALUES
02858          MOVE SFXI               TO  PI-CERT-SFX
02859                                      PI-CHEK-SUF-NO
02860      ELSE
02861          MOVE SPACE              TO  PI-CERT-SFX
02862                                      PI-CHEK-SUF-NO.
02863
02864      MOVE EFFDTI                 TO  DC-GREG-DATE-1-EDIT.
02865      MOVE '2'                    TO  DC-OPTION-CODE.
02866      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
02867      IF DATE-CONVERSION-ERROR
02868          MOVE ER-0215            TO  EMI-ERROR
02869          MOVE -1                 TO  EFFDTL
02870          MOVE AL-UNBON           TO  EFFDTA
02871          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02872          GO TO 8200-SEND-DATAONLY
02873      ELSE
02874          MOVE DC-BIN-DATE-1      TO  PI-CERT-EFF-DT
02875                                      PI-CHEK-EFF-DT.
02876
02877  7090-EXIT.
02878       EXIT.
02879      EJECT
02880  7100-CREATE-TEMP-STORAGE.
02881      PERFORM 7300-DELETE-TEMP-STORAGE THRU 7300-EXIT.
02882
02883      
      * EXEC CICS WRITEQ TS
02884 *        QUEUE   (QID)
02885 *        FROM    (PROGRAM-INTERFACE-BLOCK)
02886 *        LENGTH  (PI-COMM-LENGTH)
02887 *    END-EXEC.
      *    MOVE '*"     L              ''   #00009033' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02888
02889  7100-EXIT.
02890       EXIT.
02891
02892  7200-RECOVER-TEMP-STORAGE.
02893      
      * EXEC CICS READQ TS
02894 *        QUEUE   (QID)
02895 *        INTO    (PROGRAM-INTERFACE-BLOCK)
02896 *        LENGTH  (PI-COMM-LENGTH)
02897 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00009043' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02898
02899      PERFORM 7300-DELETE-TEMP-STORAGE THRU 7300-EXIT.
02900
02901  7200-EXIT.
02902       EXIT.
02903
02904  7300-DELETE-TEMP-STORAGE.
02905      
      * EXEC CICS HANDLE CONDITION
02906 *        QIDERR  (7300-EXIT)
02907 *    END-EXEC.
      *    MOVE '"$N                   ! 1 #00009055' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303039303535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02908
02909      
      * EXEC CICS DELETEQ TS
02910 *        QUEUE  (QID)
02911 *    END-EXEC.
      *    MOVE '*&                    #   #00009059' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02912
02913  7300-EXIT.
02914       EXIT.
02915      EJECT
111418 7500-reissue-pass-1.
111418
111418     move '2'                    to pi-void-reissue-pass
111418
111418     move pi-void-reissue-amt    to amounto
111418
111418     if eibaid = dfhpf5
111418        MOVE PI-AM-NAME          TO PAYTO1I
111418        MOVE SPACES              TO PAYTO2I
111418        MOVE PI-AM-ADDRS         TO PAYAD1I
111418        MOVE PI-AM-CITY          TO PAYCTYI
111418        move pi-am-st            to paysti
111418        MOVE PI-AM-ZIP-CODE      TO PTOZIPI
111418        move 'ACCT'              to payeeo
111418        move al-panon            to payeea
111418
111418        MOVE AL-UANON            TO PAYTO1A
111418                                    PAYTO2A
111418                                    PAYAD1A
111418                                    PAYCTYA
111418                                    paysta
111418                                    PTOZIPA
111418                                    payeea
111418        GO TO 8100-SEND-INITIAL-MAP
111418     end-if
111418
111418     perform 5700-get-elcert     thru 5700-exit
111418     move spaces                 to pi-table-name
111418     string cm-insured-last-name ', '
111418            cm-insured-first-name ' '
111418            cm-insured-initial2
111418        delimited by '  '
111418        into
111418           pi-table-name
111418     end-string
111418
111418     IF CM-INSURED-FIRST-NAME GREATER SPACES
111418         MOVE CM-INSURED-FIRST-NAME  TO  WS-INSURED-1ST-NAME
111418         MOVE CM-INSURED-INITIAL2    TO  WS-INSURED-MID-INIT
111418     ELSE
111418         MOVE CM-INSURED-INITIAL1    TO  WS-INSURED-1ST-NAME
111418         MOVE CM-INSURED-INITIAL2    TO  WS-INSURED-MID-INIT.
111418
111418     MOVE CM-INSURED-LAST-NAME       TO  WS-INSURED-LAST-NAME.
111418
111418     PERFORM 5200-MOVE-NAME THRU 5200-EXIT.
111418
111418     if eibaid = dfhpf7
111418        
      * EXEC CICS READ
111418*           DATASET   (ERMAIL-FILE-ID)
111418*           SET       (ADDRESS OF MAILING-DATA)
111418*           RIDFLD    (ELCERT-KEY)
111418*           resp      (ws-response)
111418*       END-EXEC
      *    MOVE '&"S        E          (  N#00009114' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039313134' TO DFHEIV0(25:11)
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
111418        if resp-normal
111418           move spaces             to payto2i
111418           move ma-cred-bene-name  to payto1i
111418           MOVE MA-cred-bene-addr  TO PAYad1I
111418           MOVE MA-cred-bene-addr2 TO PAYad2I
111418           move ma-cred-bene-city  to payctyi
111418           move ma-cred-bene-state to paysti
111418           MOVE MA-cred-bene-zip   TO PTOZIPI
111418           move 'BENE'              to payeeo
111418           MOVE AL-UANON           TO payto1a
111418                                      PAYTO2A
111418                                      PAYad1A
111418                                      payad2a
111418                                      PAYctyA
111418                                      paysta
111418                                      PTOZIPA
111418                                      payeea
111418        end-if
111418     end-if
111418
111418     if eibaid = dfhpf6
111418        
      * EXEC CICS READ
111418*           DATASET   (ERMAIL-FILE-ID)
111418*           SET       (ADDRESS OF MAILING-DATA)
111418*           RIDFLD    (ELCERT-KEY)
111418*           resp      (ws-response)
111418*       END-EXEC
      *    MOVE '&"S        E          (  N#00009141' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039313431' TO DFHEIV0(25:11)
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
111418        if resp-normal
111418           move spaces            to payto2i
111418           move ws-name-work      to payto1i
111418           MOVE MA-ADDRESS-LINE-1 TO PAYAD1I
111418           MOVE MA-ADDRESS-LINE-2 TO PAYAD2I
111418           move ma-city           to payctyi
111418           move ma-addr-state     to paysti
111418           move 'INS'             to payeeo
111418           MOVE MA-ZIP            TO PTOZIPI
111418           MOVE AL-UANON          TO PAYAD1A
111418                                     PAYAD2A
111418                                     PAYctyA
111418                                     paysta
111418                                     PTOZIPA
111418                                     payto1a
111418                                     payeea
111418        end-if
111418     end-if
111418
111418     GO TO 8100-SEND-INITIAL-MAP
111418
111418     .
111418 7500-exit.
111418     exit.
111418
111418 7510-reissue-pass-2.
111418
111418     
      * EXEC CICS READ
111418*        DATASET  (ERCHEK-FILE-ID)
111418*        SET      (ADDRESS OF CHECK-RECORDS)
111418*        RIDFLD   (PI-ERCHEK-KEY)
111418*        resp     (ws-response)
111418*    END-EXEC
      *    MOVE '&"S        E          (  N#00009174' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039313734' TO DFHEIV0(25:11)
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
111418
111418     if not resp-normal
111418        go to 7510-exit
111418     end-if
111418
111418     if ch-void-dt not = save-bin-date
111418        display ' something went wrong here '
111418        go to 7510-exit
111418     end-if
111418
111418     MOVE EIBTIME                TO CH-LAST-MAINT-HHMMSS
111418     MOVE save-bin-date          TO CH-RECORDED-DT
111418                                    ch-released-dt
111418     move pi-processor-id        to ch-recorded-by
111418                                    ch-released-by
040919                                    ch-return-to
111418     move spaces                 to ch-check-no
111418                                    ch-void-by
111418                                    ch-void-reason
111418                                    ch-approved-by
111418
111418
111418     move 'P'                    to ch-approval-status
111418
111418     MOVE LOW-VALUES             TO CH-CHECK-WRITTEN-DT
111418                                    CH-VOID-DT
111418                                    CH-CREDIT-ACCEPT-DT
111418                                    ch-approval-dt
111418                                    ch-check-cashed-dt
111418     add +1 to ch-sequence-no
111418     if ch-check-origin-sw = 'R'
111418        move 'R'                 to pi-check-type
111418     else
111418        move 'C'                 to pi-check-type
111418     end-if
111418
111418     .
111418 7510-WRITE-RECORD.
111418
111418     
      * exec cics read
111418*       dataset    (erchek-file-id)
111418*       into       (ws-dummy-erchek)
111418*       ridfld     (ch-control-primary)
111418*       resp       (ws-response)
111418*    end-exec
           MOVE LENGTH OF
            ws-dummy-erchek
             TO DFHEIV11
      *    MOVE '&"IL       E          (  N#00009219' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039323139' TO DFHEIV0(25:11)
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
111418     if RESP-NOTFND
111418        continue
111418     else
111418        add +1                   to ch-sequence-no
111418        add 1                    to seqi
111418        go to 7510-write-record
111418     end-if
111418
111418     MOVE CH-CONTROL-PRIMARY     TO PI-ERCHEK-KEY.
111418     MOVE ZEROS                  TO CH-CHECK-QUE-CONTROL
111418                                    CH-CHECK-QUE-SEQUENCE
111418
111418     IF PAYTO1L NOT = ZEROS
111418        MOVE PAYTO1I             TO CH-PAYEE-NAME-1
111418     END-IF
111418     IF PAYTO2L NOT = ZEROS
111418        MOVE PAYTO2I             TO CH-PAYEE-NAME-2
111418     end-if
111418     IF PAYAD1L NOT = ZEROS
111418        MOVE PAYAD1I             TO CH-PAYEE-ADDRESS-1
111418     end-if
111418     IF PAYAD2L NOT = ZEROS
111418        MOVE PAYAD2I             TO CH-PAYEE-ADDRESS-2
111418     end-if
111418     IF PAYctyL NOT = ZEROS
111418        MOVE payctyi             TO CH-PAYEE-CITY
111418     end-if
111418     IF PAYstL NOT = ZEROS
111418        MOVE paysti              TO CH-PAYEE-state
111418     end-if
111418     IF PAYEEI GREATER SPACES
111418        mOVE PAYEEI               TO CH-PAYEE-CODE
111418     end-if
111418     IF PTOZIPL  =  ZEROS
111418        MOVE ZEROS               TO CH-PAYEE-ZIP-CODE
111418     else
111418        MOVE PTOZIPI             TO WS-ZIP-CODE
111418        IF NOT WS-CANADIAN-POST-CODE
111418           MOVE WS-ZIP-PRIME     TO CH-PAYEE-ZIP
111418           MOVE WS-ZIP-PLUS4     TO CH-PAYEE-ZIP-EXT
111418        end-if
111418     end-if
111418
111418     PERFORM 2700-WRITE-SQL      THRU 2700-EXIT
111418
111418     
      * EXEC CICS WRITE
111418*        DATASET  (ERCHEK-FILE-ID)
111418*        FROM     (CHECK-RECORDS)
111418*        RIDFLD   (CH-CONTROL-PRIMARY)
111418*        resp     (ws-response)
111418*    END-EXEC.
           MOVE LENGTH OF
            CHECK-RECORDS
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00009270' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303039323730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCHEK-FILE-ID, 
                 CHECK-RECORDS, 
                 DFHEIV11, 
                 CH-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
111418     COMPUTE WS-JOURNAL-RECORD-LENGTH =
111418             ERCHEK-RECORD-LENGTH + 23.
111418
111418     IF EMI-NO-ERRORS
111418        MOVE ER-0000             TO EMI-ERROR
111418     ELSE
111418        IF EMI-FORCABLE-CTR GREATER THAN +0  AND
111418           EIBAID = DFHPF4
111418           MOVE ER-2600          TO EMI-ERROR
111418        end-if
111418     end-if
111418
111418     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
111418
111418     MOVE LOW-VALUES             TO EL677AI.
111418
111418     MOVE PI-CHEK-CARRIER        TO CARRIERO.
111418     MOVE PI-CHEK-GROUPING       TO GROUPO.
111418     MOVE PI-CHEK-STATE          TO STATEO.
111418     MOVE PI-CHEK-ACCOUNT        TO ACCTO.
111418
111418     MOVE PI-CHEK-EFF-DT         TO DC-BIN-DATE-1.
111418     MOVE SPACE                  TO DC-OPTION-CODE.
111418     PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
111418     MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI.
111418
111418     MOVE PI-CHEK-CERT-NO        TO CERTNOO.
111418     MOVE PI-CHEK-SUF-NO         TO SFXO.
111418     MOVE PI-CHEK-SEQUENCE       TO SEQO.
111418
111418     MOVE AL-UANON               TO CARRIERA  GROUPA  STATEA
111418                                    ACCTA     EFFDTA  CERTNOA
111418                                    SFXA.
111418     MOVE AL-UNNON               TO SEQA
111418     move ' ' to pi-void-reissue-pass
111418     go to 5000-browse-file
111418
111418     .
111418 7510-exit.
111418     exit.
02926  8100-SEND-INITIAL-MAP.
02927      MOVE SAVE-DATE              TO  DATEO.
02928      MOVE EIBTIME                TO  TIME-IN.
02929      MOVE TIME-OUT               TO  TIMEO.
02930      MOVE -1                     TO  MAINTL
           move al-uanon               to mainta
           move dedcyni                to pi-previous-deduct-comm
           if (PI-TO-EL677-FROM-EL1273)
              and (pi-void-reissue-pass = ' ')
111418        move al-sadof            to pf3a
111418                                    pf4a
111418                                    pf57a
111418                                    pf6a
                                          dprema
                                          dcomma
                                          drefa
                                          duecomma
                                          dprepda
                                          prema
                                          isscomma
                                          refa
                                          uecomma
                                          prepda
              move ', VOID(V)'         to dmaint1o
111418        move 'VOID & REISSUE(R), DELETE(D)'
111418                                 TO dmaint2o
           end-if
111418     if (pi-to-el677-from-el1273)
111418        and (pi-void-reissue-pass = '1')
111418        move al-sadof            to pf3a
111418                                    pf4a
111418                                    dprema
111418                                    dcomma
111418                                    drefa
111418                                    duecomma
111418                                    dprepda
111418                                    prema
111418                                    isscomma
111418                                    refa
111418                                    uecomma
111418                                    prepda
111418        move ', VOID(V)'         to dmaint1o
111418        move 'VOID AND REISSUE(R)'
111418                                 TO dmaint2o
111418     end-if
02931      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
02932      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
02933
02943      
      * EXEC CICS SEND
02944 *        MAP      (EL677A)
02945 *        MAPSET   (MAPSET-NAME)
02946 *        FROM     (EL677AO)
02947 *        ERASE
02948 *        CURSOR
02949 *    END-EXEC.
           MOVE LENGTH OF
            EL677AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009364' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333634' TO DFHEIV0(25:11)
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
           
02950
02951      GO TO 9100-RETURN-TRAN.
02952      EJECT
02953  8200-SEND-DATAONLY.
           if connected-to-db
              perform 4300-FINISH-UP-DB thru 4300-exit
           end-if
02954      MOVE SAVE-DATE              TO  DATEO.
02955      MOVE EIBTIME                TO  TIME-IN.
02956      MOVE TIME-OUT               TO  TIMEO.
02957      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
02958      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
02959
02966      
      * EXEC CICS SEND
02967 *        MAP     (EL677A)
02968 *        MAPSET  (MAPSET-NAME)
02969 *        FROM    (EL677AO)
02970 *        DATAONLY
02971 *        CURSOR
02972 *    END-EXEC.
           MOVE LENGTH OF
            EL677AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00009384' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333834' TO DFHEIV0(25:11)
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
           
02973
02974      GO TO 9100-RETURN-TRAN.
02975      EJECT
02976  8300-SEND-TEXT.
02977      
      * EXEC CICS SEND TEXT
02978 *        FROM     (LOGOFF-TEXT)
02979 *        LENGTH   (LOGOFF-LENGTH)
02980 *        ERASE
02981 *        FREEKB
02982 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009395' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333935' TO DFHEIV0(25:11)
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
           
02983
02984      
      * EXEC CICS RETURN
02985 *    END-EXEC.
      *    MOVE '.(                    ''   #00009402' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02986
02987  8500-DATE-CONVERT.
02988      MOVE LINK-ELDATCV           TO  PGM-NAME.
02989
02990      
      * EXEC CICS LINK
02991 *        PROGRAM    (PGM-NAME)
02992 *        COMMAREA   (DATE-CONVERSION-DATA)
02993 *        LENGTH     (DC-COMM-LENGTH)
02994 *    END-EXEC.
      *    MOVE '."C                   (   #00009408' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02995
02996  8500-EXIT.
02997       EXIT.
02998
CIDMOD 8600-DEEDIT.
CIDMOD     
      * EXEC CICS BIF DEEDIT
CIDMOD*        FIELD   (WS-DEEDIT-FIELD)
CIDMOD*        LENGTH  (10)
CIDMOD*    END-EXEC.
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009418' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD 8600-EXIT.
CIDMOD      EXIT.
CIDMOD
02999  8700-DEEDIT.
03000      
      * EXEC CICS BIF DEEDIT
03001 *        FIELD   (WS-DT-DEEDIT-FIELD)
03002 *        LENGTH  (10)
03003 *    END-EXEC.
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009427' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DT-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03004
03005  8700-EXIT.
03006       EXIT.
03007
03008  8800-UNAUTHORIZED-ACCESS.
03009      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
03010      GO TO 8300-SEND-TEXT.
03011
03012  8810-PF23.
03013      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
03014      MOVE XCTL-005               TO  PGM-NAME.
03015      GO TO 9300-XCTL.
03016
03017
03018  9100-RETURN-TRAN.
03019      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
03020      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
03021      
      * EXEC CICS RETURN
03022 *        TRANSID  (TRANS-ID)
03023 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
03024 *        LENGTH   (PI-COMM-LENGTH)
03025 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00009448' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03026
03027      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL677' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
03028
03029  9200-RETURN-MAIN-MENU.
03030      MOVE XCTL-626               TO  PGM-NAME.
03031      GO TO 9300-XCTL.
03032
03033  9300-XCTL.
03034      
      * EXEC CICS XCTL
03035 *        PROGRAM    (PGM-NAME)
03036 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
03037 *        LENGTH     (PI-COMM-LENGTH)
03038 *    END-EXEC.
      *    MOVE '.$C                   %   #00009461' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03039
03040  9400-CLEAR.
03041      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
03042      MOVE SPACES                 TO  PI-PFKEY.
03043      GO TO 9300-XCTL.
03044
03045  9500-PF12.
03046      MOVE XCTL-010               TO  PGM-NAME.
03047      GO TO 9300-XCTL.
03048
03049  9600-PGMID-ERROR.
03050      
      * EXEC CICS HANDLE CONDITION
03051 *        PGMIDERR    (8300-SEND-TEXT)
03052 *    END-EXEC.
      *    MOVE '"$L                   ! 2 #00009477' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303039343737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03053
03054      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
03055      MOVE ' '                    TO  PI-ENTRY-CD-1.
03056      MOVE XCTL-005               TO  PGM-NAME.
03057      MOVE PGM-NAME               TO  LOGOFF-PGM.
03058      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
03059
03060      GO TO 9300-XCTL.
03061
03062  9900-ERROR-FORMAT.
03063      IF NOT EMI-ERRORS-COMPLETE
03064          MOVE LINK-001           TO  PGM-NAME
03065          
      * EXEC CICS LINK
03066 *            PROGRAM    (PGM-NAME)
03067 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
03068 *            LENGTH     (EMI-COMM-LENGTH)
03069 *        END-EXEC.
      *    MOVE '."C                   (   #00009492' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03070
03071  9900-EXIT.
03072       EXIT.
03073
03074  9990-ABEND.
03075      MOVE LINK-004               TO  PGM-NAME.
03076      MOVE DFHEIBLK               TO  EMI-LINE1.
03077      
      * EXEC CICS LINK
03078 *        PROGRAM   (PGM-NAME)
03079 *        COMMAREA  (EMI-LINE1)
03080 *        LENGTH    (72)
03081 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00009504' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03082
03083      GO TO 8200-SEND-DATAONLY.
03084
03085  9995-SECURITY-VIOLATION.
03086 *                            COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00009530' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
03087
03088  9995-EXIT.
03089      EXIT.
03090

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
