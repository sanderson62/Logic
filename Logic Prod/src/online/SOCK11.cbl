      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK11.
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
080818******************************************************************
080818*REMARKS.                                                        *
080818*  This program gets kicked off by C# APP SpecAcctHandling via   *
080818*     Transaction SO11. Pulls all table rows from table          *
080818*     RefSpecHand in the Logic DB into an array. Next reads the  *
080818*     account master (ERACCT) one record at a time, compares to  *
080818*     all entries in array and updates the ERACCT accordingly.   *
080818******************************************************************
080818*                   C H A N G E   L O G
080818*
080818* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080818*-----------------------------------------------------------------
080818*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080818* EFFECTIVE    NUMBER
080818*-----------------------------------------------------------------
080818* 080818   2018040600002   PEMA  New Program
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
080818******************************************************************
       ENVIRONMENT DIVISION.
       data division.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   SOCK11   WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
      *
      * program buffers
      *
       77 ws-send-msg-size             pic s9(8) comp value 256.
       77 ws-recv-msg-size             pic s9(8) comp value 256.
       77 ws-recv-buf                  pic x(256).
       77 ws-send-buf                  pic x(256) VALUE SPACES.
       77 ws-recv-total                pic s9(8) comp value 0.
       77 ws-recv-left                 pic s9(8) comp value 0.
       77 ws-seq-num                   pic s9(8) comp value 0.
       77 ws-flags                     pic s9(8) comp value 0.
       77 WS-COMP-CD                   PIC X  VALUE LOW-VALUES.
       77  ws-comp-id                  pic xxx value spaces.
       77 X1                           PIC S999 COMP-3 VALUE +0.
       77 S1                           PIC S999 COMP-3 VALUE +0.
       77 S2                           PIC S999 COMP-3 VALUE +0.
       77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
       77  WS-ERACCT-SW                PIC X VALUE ' '.
           88  END-OF-ERACCT                 VALUE 'Y'.
       77  ws-socket-sw                pic x value ' '.
           88  end-of-socket              value 'Y'.
       77  ws-browse-sw                pic x value ' '.
           88  browse-started            value 'Y'.
       77  ws-connect-sw               pic x value ' '.
           88  connected-to-db           value 'Y'.
       77  ws-bin-current-dt           pic xx value low-values.
       77  WS-MATCH-SW                 PIC X value ' '.
           88  found-match               value 'Y'.
       77  ws-acct-reads               pic 9(7) value zeros.
       77  ws-acnt-reads               pic 9(7) value zeros.
       77  ws-acnt-updates             pic 9(7) value zeros.
       77  ws-acnt-deletes             pic 9(7) value zeros.
       77  ws-batch-job-sw             pic x value spaces.
           88  batch-job                  value 'Y'.
       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
       01  ws-save-report-code-1       pic x(10) value spaces.
       01  ws-save-report-code-2       pic x(10) value spaces.
       01  ws-save-report-code-3       pic x(10) value spaces.
       01  ws-save-user-select-2       pic x(10) value spaces.
       01  ws-save-user-select-5       pic x(10) value spaces.
       01  ws-save-eracct-key.
           05  ws-save-comp-cd         pic x.
           05  ws-save-carrier         pic x.
           05  f                       pic x(6).
           05  ws-save-state           pic xx.
           05  ws-save-account         pic x(10).
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).
       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC
       01  ws-error-message            pic x(50) value spaces.
       01  ws-status-date              pic x(10).
       01  sqlcmd                      pic x(1024).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
       01  ws-display-response         pic s9(9) value zeros.
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
       01  ws-bit                      pic x.
       01  ws-bit-comp redefines ws-bit pic s9(4) comp.
       01  indicator-vaiables-for-nulls.
           05  nu-report-code1         pic s9(4) comp value +0.
           05  nu-report-code2         pic s9(4) comp value +0.
           05  nu-report-code3         pic s9(4) comp value +0.
           05  nu-user-select2         pic s9(4) comp value +0.
           05  nu-user-select5         pic s9(4) comp value +0.
           05  nu-carrier              pic s9(4) comp value +0.
           05  nu-state                pic s9(4) comp value +0.
           05  nu-account              pic s9(4) comp value +0.
           05  nu-status               pic s9(4) comp value +0.
       01  table-row.
           05  tr-report-code1         pic x(10).
           05  tr-report-code2         pic x(10).
           05  tr-report-code3         pic x(10).
           05  tr-user-select2         pic x(10).
           05  tr-user-select5         pic x(10).
           05  tr-carrier              pic x.
           05  tr-state                pic xx.
           05  tr-account              pic x(10).
           05  tr-status               pic x.
       EXEC SQL
          END DECLARE SECTION
       END-EXEC
       01  a1                          pic s9(5) comp-3 value +0.
       01  ma1                         pic s9(5) comp-3 value +0.
       01  sql-table.
           05  sql-table-array occurs 3000.
               10  a1-report-code1     pic x(10).
               10  a1-report-code2     pic x(10).
               10  a1-report-code3     pic x(10).
               10  a1-user-select2     pic x(10).
               10  a1-user-select5     pic x(10).
               10  a1-carrier          pic x.
               10  a1-state            pic xx.
               10  a1-account          pic x(10).
               10  a1-status           pic x.
       01  soc-client-in-data          pic x(50).
       01  WS-AM-KEY.
           05  WS-AM-COMPANY-CD        PIC X.
           05  WS-AM-CARRIER           PIC X.
           05  WS-AM-GROUP             PIC X(6).
           05  WS-AM-STATE             PIC XX.
           05  WS-AM-ACCOUNT           PIC X(10).
           05  WS-AM-EXP-DT            PIC XX.
           05  FILLER                  PIC X(4).
       01  WS-NT-KEY.
           05  WS-NT-COMPANY-CD        PIC X.
           05  WS-NT-CARRIER           PIC X.
           05  WS-NT-GROUP             PIC X(6).
           05  WS-NT-STATE             PIC XX.
           05  WS-NT-ACCOUNT           PIC X(10).
           05  WS-NT-rec-type          PIC X.
           05  ws-nt-seq-no            pic s9(4) comp.
       01  ws-return-string.
           05  ws-return-error-no      pic x(4).
           05  ws-sc1                  pic x.
           05  ws-return-error-mess    pic x(45).
           05  ws-sc2                  pic x.
           05  ws-return-accts-read    pic zzzzzz9.
           05  ws-sc3                  pic x.
           05  ws-return-acnts-read    pic zzzzzz9.
           05  ws-sc4                  pic x.
           05  ws-return-acnts-updated pic zzzzzz9.
           05  ws-sc5                  pic x.
           05  ws-return-acnts-deletes pic zzzzzz9.
       01  WS-CID-NO                   PIC X(8).
       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  resp-duprec                  value +14.
           88  resp-dupkey                  value +15.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.
      *                                 COPY ERCACCT.
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
      *                                 copy ERCACNT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACNT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = NOTE FILE FOR RECORDING OF ACCOUNT NOTES  *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 120   RECFORM = FIXED                          *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERACNT             RKP=2,LEN=23          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
110706*                   C H A N G E   L O G
110706*
110706* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
110706*-----------------------------------------------------------------
110706*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
110706* EFFECTIVE    NUMBER
110706*-----------------------------------------------------------------
110706* 110706  CR2006071700004  PEMA  ADD BRANCH LOCATIONS
110706*           AND SHIPPING ADDRESS TO ACCOUNT NOTES FILE
110706******************************************************************
00019  01  NOTE-FILE.
00020      12  NT-FILE-ID                  PIC XX.
00021          88  VALID-NOTE-ID              VALUE 'NT'.
00022
00023      12  NT-CONTROL-PRIMARY.
00024          16  NT-COMPANY-CD           PIC X.
00027          16  NT-ACCT-NOTE-KEY.
00028              18  NT-CARRIER              PIC X.
00029              18  NT-GROUPING             PIC X(06).
00030              18  NT-STATE                PIC XX.
00031              18  NT-ACCOUNT              PIC X(10).
00025          16  NT-RECORD-TYPE          PIC X.
00026               88  ACCT-NOTE          VALUE '1'.
110706              88  ACCT-BRANCH-LOC    VALUE '2'.
110706              88  ACCT-SHIPPING-ADDR VALUE '3'.
00032          16  NT-LINE-SEQUENCE        PIC S9(4)     COMP.
00033
00034      12  NT-LAST-MAINT-DT            PIC XX.
00035      12  NT-LAST-MAINT-BY            PIC X(4).
00036      12  NT-LAST-MAINT-HHMMSS        PIC S9(7) COMP-3.
00037
110706*  ALL NOTE LINES ARE RECORD TYPE '1' WITH ALMOST UNLIMITED
110706*     SEQUENCE NUMBERS
110706     12  NT-NOTE-INFORMATION.
110706         16  NT-NOTE-LINE            PIC X(60).
00040          16  FILLER                  PIC X(25).
110706*  BOTH BRANCH LOCATION LINES ARE RECORD TYPE '2' SEQ 1 AND 2
110706     12  NT-LOCATION-INFORMATION REDEFINES
110706                         NT-NOTE-INFORMATION.
110706         16  NT-BRANCH-LOC-LINE      PIC X(60).
110706         16  FILLER                  PIC X(25).
052918* Account special indicator is record type '2', sequence 3
052918     12  filler REDEFINES NT-NOTE-INFORMATION.
052918         16  nt-account-special      PIC X.
052918         16  FILLER                  PIC X(84).
110706*  ALL SHIPPING ADDRESS LINES ARE RECORD TYPE '3'AND
      *     SEQUENCE NUMBER 1 IS NAME LINE 1
      *     SEQUENCE NUMBER 2 IS NAME LINE 2
      *     SEQUENCE NUMBER 3 IS ADDR LINE 1
      *     SEQUENCE NUMBER 4 IS ADDR LINE 2
      *     SEQUENCE NUMBER 5 IS ADDR LINE 3
      *     SEQUENCE NUMBER 6 IS CITY, ST AND ZIP
110706     12  NT-SHIPPING-INFORMATION REDEFINES
110706                         NT-NOTE-INFORMATION.
               16  NT-SHIPPING-LINE        PIC X(60).
110706         16  NT-SHIP-STATE           PIC XX.
110706         16  NT-SHIP-ZIP             PIC X(10).
110706         16  FILLER                  PIC X(13).
00041 *****************************************************************
      *                                 COPY ELCFUNDT.
00001 *****************************************************************
00002 *                                                               *
00003 *                            ELCFUNDT.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE *
00005 *                            VMOD=2.001                         *
00006 *                                                               *
00007 *           COPYBOOK FOR THE FUNCTION DATE FORMAT               *
00008 *                                                               *
00009 *****************************************************************
00010
00011
00012  01  FUNCTION-DATE.
00013      05  WS-FN-DATE                PIC 9(8)    VALUE ZEROS.
00014      05  WS-FN-CYMD  REDEFINES  WS-FN-DATE.
00015          10  WS-FN-CCYR            PIC 9(4).
00016          10  WS-FN-CCYY  REDEFINES  WS-FN-CCYR.
00017              15  WS-FN-CC          PIC 99.
00018              15  WS-FN-YR          PIC 99.
00019          10  WS-FN-MO              PIC 99.
00020          10  WS-FN-DA              PIC 99.
00021      05  WS-FN-HOURS               PIC 99      VALUE ZEROS.
00022      05  WS-FN-MINUTES             PIC 99      VALUE ZEROS.
00023      05  WS-FN-SECONDS             PIC 99      VALUE ZEROS.
00024      05  WS-FN-HUNDSECS            PIC 99      VALUE ZEROS.
00025      05  WS-FN-GMT-IND             PIC X       VALUE SPACES.
00026          88  WS-BEHIND-GMT                     VALUE '-'.
00027          88  WS-AFTER-GMT                      VALUE '+'.
00028          88  WS-NO-GMT                         VALUE ZERO.
00029      05  WS-FN-GMT-HOURS           PIC 99      VALUE ZEROS.
00030      05  WS-FN-GMT-MINUTES         PIC 99      VALUE ZEROS.
      *                                 COPY ELCDATE.
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
            15  CLIENT-KICK-OFF      PIC X(8).
            15  CLIENT-ID            PIC XXX.
            15  FILLER               PIC X(25).
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).
       01  var  pic x(30).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA VAR.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'SOCK11' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           display 'SOCK11:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK11:socket number    =', GIVE-TAKE-SOCKET.
           display 'SOCK11:socket name      =', lstn-name ' '
              lstn-subname
           perform 0000-init           thru 0000-exit
           perform 0010-init-contact   thru 0010-exit
      *    perform 0020-receive        thru 0020-exit
      *    if end-of-socket
      *       go to 0300-close-socket
      *    end-if
           perform 0030-build-array-from-sql
                                       thru 0030-exit
           perform 0040-process-eracct thru 0040-exit
      *    display ' acct reads   ' ws-acct-reads
      *    display ' acnt reads   ' ws-acnt-reads
      *    display ' acnt updates ' ws-acnt-updates
      *    display ' acnt deletes ' ws-acnt-deletes
           perform 0070-format-buffer  thru 0070-exit
           perform 0200-send-buffer    thru 0200-exit
           go to 0300-close-socket
           .
       0000-init.
           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           move ws-fn-cymd             to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-current-dt
           else
              display ' error current dt invalid ' dc-error-code
           end-if
      *          ws-fn-hours ':' ws-fn-minutes ':' ws-fn-seconds
           move spaces                 to sql-table
                                          ws-return-string
           move ';'                    to ws-sc1
                                          ws-sc2
                                          ws-sc3
                                          ws-sc4
                                          ws-sc5
           move zeros                  to ws-return-accts-read
                                          ws-return-acnts-read
                                          ws-return-acnts-updated
                                          ws-return-acnts-deletes
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
           .
       0000-exit.
           exit.
       0010-init-contact.
           if client-kick-off = 'SOCKET11'
              continue
           else
              set batch-job to true
      *       move '9999'              to ws-return-error-no
      *                                to ws-return-error-mess
      *       PERFORM 0200-SEND-BUFFER thru 0200-exit
      *       display ' Unknown origin ' client-in-data
      *       go to 0300-close-socket
           end-if
           MOVE X'04'                  TO WS-COMP-CD
           MOVE 'CID'                  TO WS-COMP-ID
      *    move 'SOCKET11READY'        to ws-send-buf
      *    move +25                    to ws-send-msg-size
      *
      *    display 'SOCK11:sequence number  =', ws-seq-num.
      *    display 'SOCK11:send buffer      =', ws-send-buf(1:25)
      *
      *    call "send" using by value GIVE-TAKE-SOCKET,
      *        by reference ws-send-buf,
      *        by value ws-send-msg-size,
      *        by value ws-flags
      *
      *    if return-code <= zero
      *       display 'SOCK11:send error ' return-code
      *       perform 0250-socket-error thru 0250-exit
      *       go to 0300-close-socket
      *    end-if
           .
       0010-exit.
           exit.
       0020-receive.
           call "recv" using by value GIVE-TAKE-SOCKET
               by reference ws-recv-buf
               by value ws-recv-msg-size
               by value ws-flags.
           if return-code < zero
              display 'SOCK11:recv error ' return-code
              perform 0250-socket-error thru 0250-exit
              set end-of-socket to true
              go to 0010-exit
           end-if
           if return-code = zero
              display 'SOCK11:client disconnected',
              perform 0250-socket-error thru 0250-exit
              set end-of-socket to true
              go to 0010-exit
           end-if
      *    display 'SOCK11:return code      = ', return-code
      *    display 'SOCK11:receive buffer   = ', ws-recv-buf(1:51)
           move +50                    to ws-send-msg-size
           move ws-recv-buf (1:50)     to soc-client-in-data
           if ws-recv-buf (1:4) = 'DONE' or 'done' or 'Done'
              set end-of-socket to true
           end-if
           .
       0020-exit.
           exit.
       0030-build-array-from-sql.
           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if
           EXEC SQL
              DECLARE
                 getSpecInstr cursor for
              SELECT
                 ReportCode1,
                 ReportCode2,
                 ReportCode3,
                 UserSelect2,
                 UserSelect5,
                 Carrier,
                 State,
                 Account,
                 Status
              FROM
                 RefSpecHand
           end-exec
           if sqlcode not = 0
              display "Error: cannot declare cursor "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move sqlcode             to ws-display-response
              move '9999'              to ws-return-error-no
              move 'Could not declare cursor'
                                       to ws-return-error-mess
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              go to 0300-close-socket
           end-if
           EXEC SQL
              open getSpecInstr
           END-EXEC
           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move '9999'              to ws-return-error-no
              move 'Could not Open cursor'
                                       to ws-return-error-mess
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              go to 0300-close-socket
           end-if
           perform until sqlcode not = 0
              EXEC SQL
                 fetch getSpecInstr into
                    :tr-report-code1  :nu-report-code1,
                    :tr-report-code2  :nu-report-code2,
                    :tr-report-code3  :nu-report-code3,
                    :tr-user-select2  :nu-user-select2,
                    :tr-user-select5  :nu-user-select5,
                    :tr-carrier       :nu-carrier,
                    :tr-state         :nu-state,
                    :tr-account       :nu-account,
                    :tr-status        :nu-status
              END-EXEC
      *       display ' rpt cd1 ' tr-report-code1 ' ' nu-report-code1
      *       display ' rpt cd2 ' tr-report-code2 ' ' nu-report-code2
      *       display ' rpt cd3 ' tr-report-code3 ' ' nu-report-code3
      *       display ' account ' tr-account ' ' nu-account
              if sqlcode = 0
                 add +1 to a1
                 if nu-report-code1 = +0
                    move tr-report-code1 to a1-report-code1(a1)
                 end-if
                 if nu-report-code2 = +0
                    move tr-report-code2 to a1-report-code2(a1)
                 end-if
                 if nu-report-code3 = +0
                    move tr-report-code3 to a1-report-code3(a1)
                 end-if
                 if nu-user-select2 = +0
                    move tr-user-select2 to a1-user-select2(a1)
                 end-if
                 if nu-user-select5 = +0
                    move tr-user-select5 to a1-user-select5(a1)
                 end-if
                 if nu-carrier = +0
                    move tr-carrier      to a1-carrier(a1)
                 end-if
                 if nu-state = +0
                    move tr-state        to a1-state(a1)
                 end-if
                 if nu-account = +0
                    move tr-account      to a1-account(a1)
                 end-if
                 if nu-status = +0
                    move tr-status       to a1-status(a1)
                 end-if
              else
                 if sqlcode not = 0 and 100
                    display "Error: cannot fetch row " a1
                    display ' sql return code ' sqlcode
                    display ' sql err mess    ' sqlerrmc
                    move '9999'        to ws-return-error-no
                    move 'Could not fetch rows'
                                       to ws-return-error-mess
                    PERFORM 0200-SEND-BUFFER thru 0200-exit
                    go to 0300-close-socket
                 end-if
              end-if
           end-perform
           EXEC SQL
               close getSpecInstr
           END-EXEC
           if sqlcode not = 0
              display "Error: cannot close cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if
           move a1                     to ma1
           if connected-to-db
              EXEC SQL
                  disconnect all
              END-EXEC
              move ' '                 to ws-connect-sw
           end-if
      *    perform varying a1 from +1 by +1 until a1 > ma1
      *       display ' rpt cd1 ' a1-report-code1(a1)
      *       display ' rpt cd2 ' a1-report-code2(a1)
      *       display ' rpt cd3 ' a1-report-code3(a1)
      *       display '     us2 ' a1-user-select2(a1)
      *       display '     us5 ' a1-user-select5(a1)
      *       display '   carr  ' a1-carrier(a1)
      *       display '  state  ' a1-state(a1)
      *       display ' account ' a1-account(a1)
      *       display ' status  ' a1-status(a1)
      *    end-perform
           .
       0030-exit.
           exit.
       0040-process-eracct.
           move X'04'                  to ws-am-key
           
      * exec cics startbr
      *       dataset       ('ERACCT')
      *       ridfld        (ws-am-key)
      *       resp          (ws-response)
      *       gteq
      *    end-exec
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00001407' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303031343037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ws-am-key, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if not resp-normal
              display 'error-eracct-startbr ' ws-response
              go to 0040-exit
           end-if
           
      * exec cics readnext
      *       dataset       ('ERACCT')
      *       ridfld        (ws-am-key)
      *       into          (account-master)
      *       resp          (ws-response)
      *    end-exec
           MOVE LENGTH OF
            account-master
             TO DFHEIV12
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001417' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031343137' TO DFHEIV0(25:11)
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
           if resp-normal
              move am-control-primary(1:20)
                                       to ws-save-eracct-key
              perform until end-of-eracct
                 perform 0050-check-table
                                       thru 0050-exit
      *          display ' key b4 readnext ' ws-am-key (2:19)
                 
      * exec cics readnext
      *             dataset    ('ERACCT')
      *             ridfld     (ws-am-key)
      *             into       (account-master)
      *             resp       (ws-response)
      *          end-exec
           MOVE LENGTH OF
            account-master
             TO DFHEIV12
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00001430' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303031343330' TO DFHEIV0(25:11)
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
                 if (not resp-normal)
                    or (am-company-cd not = X'04')
                    set end-of-eracct to true
                 else
                    add 1 to ws-acct-reads
                 end-if
      *          if ws-acct-reads > +400
      *             set end-of-eracct to true
      *          end-if
              end-perform
              perform 0050-check-table thru 0050-exit
           end-if
           .
       0040-exit.
           exit.
       0050-check-table.
           if am-control-primary(1:20) = ws-save-eracct-key
              move am-report-code-1    to ws-save-report-code-1
              move am-report-code-2    to ws-save-report-code-2
              move am-report-code-3    to ws-save-report-code-3
              move am-user-select-2    to ws-save-user-select-2
              move am-user-select-5    to ws-save-user-select-5
              go to 0050-exit
           end-if
           move ' '                    to ws-match-sw
           perform varying a1 from +1 by +1 until
              (a1 > ma1)
              or (found-match)
      **      display ' array values ' sql-table-array(a1) ' ' a1
              if ((a1-report-code1(a1) = spaces)
                 or (a1-report-code1(a1) = ws-save-report-code-1))
                            and
                 ((a1-report-code2(a1) = spaces)
                 or (a1-report-code2(a1) = ws-save-report-code-2))
                            and
                 ((a1-report-code3(a1) = spaces)
                 or (a1-report-code3(a1) = ws-save-report-code-3))
                            and
                 ((a1-user-select2(a1) = spaces)
                 or (a1-user-select2(a1) = ws-save-user-select-2))
                            and
                 ((a1-user-select5(a1) = spaces)
                 or (a1-user-select5(a1) = ws-save-user-select-5))
                            and
                 ((a1-carrier(a1) = spaces)
                 or (a1-carrier(a1) = ws-save-carrier))
                            and
                 ((a1-state(a1) = spaces)
                 or (a1-state(a1) = ws-save-state))
                            and
                 ((a1-account(a1) = spaces)
                 or (a1-account(a1) = ws-save-account))
                 perform 0060-update-eracnt
                                       thru 0060-exit
                 set found-match to true
      *          move high-values      to ws-am-exp-dt
              end-if
           end-perform
           if not found-match  *> may have been a delete
              move ws-save-eracct-key  to ws-nt-key
              move '2'                 to ws-nt-rec-type
              move +3                  to ws-nt-seq-no
              perform 0064-read-eracnt-update
                                       thru 0064-exit
              if resp-normal  *>  found a note
                 if nt-account-special <> 'Y'
                    perform 0061-unlock-eracnt
                                       thru 0061-exit
                 else  *> need to remove setting
                    move ' '           to nt-account-special
                    perform 0063-rewrite-eracnt
                                       thru 0063-exit
                    if not resp-normal
                       display 'error-rewrite-eracnt-D ' ws-response
                          ' ' am-account
                    else
                       add 1 to ws-acnt-deletes
      *                   am-account
                    end-if
                 end-if
              end-if
           end-if
           move am-control-primary(1:20)
                                       to ws-save-eracct-key
           move am-report-code-1       to ws-save-report-code-1
           move am-report-code-2       to ws-save-report-code-2
           move am-report-code-3       to ws-save-report-code-3
           move am-user-select-2       to ws-save-user-select-2
           move am-user-select-5       to ws-save-user-select-5
      *    move ' '                    to ws-match-sw
      *
      *    perform varying a1 from +1 by +1 until
      *       (a1 > ma1)
      *       or (found-match)
      **      display ' array values ' sql-table-array(a1) ' ' a1
      *
      *       if ((a1-report-code1(a1) = spaces)
      *          or (a1-report-code1(a1) = am-report-code-1))
      *                     and
      *          ((a1-report-code2(a1) = spaces)
      *          or (a1-report-code2(a1) = am-report-code-2))
      *                     and
      *          ((a1-report-code3(a1) = spaces)
      *          or (a1-report-code3(a1) = am-report-code-3))
      *                     and
      *          ((a1-user-select2(a1) = spaces)
      *          or (a1-user-select2(a1) = am-user-select-2))
      *                     and
      *          ((a1-user-select5(a1) = spaces)
      *          or (a1-user-select5(a1) = am-user-select-5))
      *                     and
      *          ((a1-carrier(a1) = spaces)
      *          or (a1-carrier(a1) = am-carrier))
      *                     and
      *          ((a1-state(a1) = spaces)
      *          or (a1-state(a1) = am-state))
      *                     and
      *          ((a1-account(a1) = spaces)
      *          or (a1-account(a1) = am-account))
      *          perform 0060-update-eracnt
      *                                thru 0060-exit
      *          set found-match to true
      *          move high-values      to ws-am-exp-dt
      *       end-if
      *    end-perform
      *
      *    if not found-match  *> may have been a delete
      *       move ws-am-key(1:20)     to ws-nt-key
      *       move '2'                 to ws-nt-rec-type
      *       move +3                  to ws-nt-seq-no
      *       perform 0064-read-eracnt-update
      *                                thru 0064-exit
      *       if resp-normal  *>  found a note
      *          if nt-account-special <> 'Y'
      *             perform 0061-unlock-eracnt
      *                                thru 0061-exit
      *          else  *> need to remove setting
      *             move ' '           to nt-account-special
      *             perform 0063-rewrite-eracnt
      *                                thru 0063-exit
      *             if not resp-normal
      *                display 'error-rewrite-eracnt-D ' ws-response
      *                   ' ' am-account
      *             else
      *                add 1 to ws-acnt-deletes
      *                   am-account
      *             end-if
      *          end-if
      *       end-if
      *    end-if
           .
       0050-exit.
           exit.
       0060-update-eracnt.
           if sql-table-array(a1) = spaces
              display ' bypass 0060, all spaces '
              go to 0060-exit
           end-if
      *       ws-save-account
      *    move ws-am-key(1:20)        to ws-nt-key
           move ws-save-eracct-key     to ws-nt-key
           move '2'                    to ws-nt-rec-type
           move +3                     to ws-nt-seq-no
           perform 0064-read-eracnt-update
                                       thru 0064-exit
           if resp-normal     *>  Found a note
      *       display ' found note ' ws-save-state ' ' ws-save-account
              add 1 to ws-acnt-reads
              if a1-status(a1) = 'A'  *> special instr active
                 if nt-account-special = 'Y' *> note already set to Y
                                             *> so go on about ur bus
                    perform 0061-unlock-eracnt
                                       thru 0061-exit
                 else  *> note not active so set it to active
                    move 'Y'           to nt-account-special
                    perform 0063-rewrite-eracnt
                                       thru 0063-exit
                    if not resp-normal
                       display 'error-rewrite-eracnt-A ' ws-response
                          ' ' ws-save-account
                    else
                       add 1 to ws-acnt-updates
      *                   ws-save-account ' ' sql-table-array(a1)
                    end-if
                 end-if
              else  *> special instr inactive
                 if nt-account-special <> 'Y' *> note inactive
                                              *> so go on about ur bus
                    perform 0061-unlock-eracnt
                                       thru 0061-exit
                 else *> note active so set it to inactive
                    move ' '           to nt-account-special
                    perform 0063-rewrite-eracnt
                                       thru 0063-exit
                    if not resp-normal
                       display 'error-rewrite-eracnt-I ' ws-response
                          ' ' ws-save-account
                    else
                       add 1 to ws-acnt-updates
      *                   ws-save-account ' ' sql-table-array(a1)
                    end-if
                 end-if
              end-if
           else
              if resp-notfnd  *> no note record
                 if a1-status(a1) = 'A'  *> spec instr is active
                    move 'NT'             to note-file
                    move ws-save-eracct-key
                                       to nt-control-primary
                    move '2'           to nt-record-type
                    move +3            to nt-line-sequence
                    move 'Y'           to nt-account-special
                    perform 0062-write-eracnt
                                       thru 0062-exit
                    if not resp-normal
                       display 'error-write-eracnt ' ws-response
                    else
                       add 1 to ws-acnt-updates
      *                   ws-save-account ' ' sql-table-array(a1)
                    end-if
                 end-if
              end-if
           end-if
           .
       0060-exit.
           exit.
       0061-unlock-eracnt.
           
      * exec cics unlock
      *       dataset   ('ERACNT')
      *    end-exec
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&*                    #   #00001669' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0061-exit.
           exit.
       0062-write-eracnt.
           
      * exec cics write
      *       dataset ('ERACNT')
      *       from    (note-file)
      *       ridfld  (nt-control-primary)
      *       resp    (ws-response)
      *    end-exec
           MOVE LENGTH OF
            note-file
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00001676' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 note-file, 
                 DFHEIV11, 
                 nt-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0062-exit.
           exit.
       0063-rewrite-eracnt.
           
      * exec cics rewrite
      *       dataset    ('ERACNT')
      *       from       (note-file)
      *       resp       (ws-response)
      *    end-exec
           MOVE LENGTH OF
            note-file
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&& L                  %  N#00001686' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 note-file, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0063-exit.
           exit.
       0064-read-eracnt-update.
           
      * exec cics read
      *       update
      *       dataset  ('ERACNT')
      *       RIDFLD   (WS-NT-KEY)
      *       into     (note-file)
      *       resp     (ws-response)
      *    end-exec
           MOVE LENGTH OF
            note-file
             TO DFHEIV11
           MOVE 'ERACNT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00001695' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031363935' TO DFHEIV0(25:11)
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
           .
       0064-exit.
           exit.
       0070-format-buffer.
           move '0000'                 to ws-return-error-no
           move 'Logic Updated completed successful '
                                       to ws-return-error-mess
           move ws-acct-reads          to ws-return-accts-read
           move ws-acnt-reads          to ws-return-acnts-read
           move ws-acnt-updates        to ws-return-acnts-updated
           move ws-acnt-deletes        to ws-return-acnts-deletes
           .
       0070-exit.
           exit.
       0200-send-buffer.
           if batch-job
              go to 0200-exit
           end-if
           move ws-return-string       to ws-send-buf
           display 'sock11:About to send      ' ws-send-buf
           display 'sock11:sequence number  =', ws-seq-num.
           display ' msg size ' ws-send-msg-size
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.
           if return-code <= zero
              display 'sock11:send error ',
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if
           .
       0200-exit.
           exit.
       0250-socket-error.
           display "sock11:did not complete"
           display 'sock11:transaction data =', CLIENT-IN-DATA '**'
           display 'sock11:socket number    =', GIVE-TAKE-SOCKET.
           display 'sock11:socket name      =', lstn-name ' '
              lstn-subname
           display ' return code = ' return-code
           .
       0250-exit.
           exit.
       0300-close-socket.
      *    call "close" using by value GIVE-TAKE-SOCKET .
           display 'sock11:done'
           
      * exec cics return end-exec.
      *    MOVE '.(                    ''   #00001749' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * goback.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK11' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback.
           .
       0300-exit.
           exit.
       6000-CONNECT-TO-DB.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
063022     MOVE 'TEST_Logic'           TO SVR
063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass
063022
063022     if ws-kix-myenv = 'cid1p'
063022        MOVE 'PROD_Logic'        TO SVR
063022     end-if
           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string
      *    display ' About to connect to ' svr ' ' usr-pass
           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
           if sqlcode not = 0
              display "Error: cannot connect to " svr
              display sqlcode
              display sqlerrmc
           else
      *       display ' Successful Connect ' sqlcode
              set connected-to-db to true
           end-if
           .
       6000-EXIT.
           EXIT.
       9700-DATE-LINK.
           
      * EXEC CICS LINK
      *         PROGRAM  ('ELDATCV')
      *         COMMAREA (DATE-CONVERSION-DATA)
      *         LENGTH   (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00001787' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373837' TO DFHEIV0(25:11)
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
