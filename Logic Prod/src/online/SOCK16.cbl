      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCK16.
       AUTHOR.     Paul.
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
      *       Receives a call from the GA withholding app and          *
      *   will update the ercomp table with the latest values based on *
      *   the key(s) passed to this program.                           *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 020119 CR2020060800001   PEMA  New Program
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
      *
      * program buffers
      *
       77 ws-send-msg-size             pic s9(8) comp value 4096.
       77 ws-recv-msg-size             pic s9(8) comp value 4096.
       77 ws-recv-buf                  pic x(4096).
       77 ws-send-buf                  pic x(4096) VALUE SPACES.
       77 ws-recv-total                pic s9(8) comp value 0.
       77 ws-recv-left                 pic s9(8) comp value 0.
       77 ws-seq-num                   pic s9(8) comp value 0.
       77 ws-flags                     pic s9(8) comp value 0.
       77 WS-COMP-CD                   PIC X  VALUE LOW-VALUES.
       77  ws-comp-id                  pic xxx value spaces.
       77 WS-SAVE-ACCOUNT              PIC X(10)  VALUE SPACES.
       77 WS-BIN-ORIG-EFF-DT           PIC XX  VALUE LOW-VALUES.
       77 WS-ORIG-EFF-DT               PIC X(10)  VALUE SPACES.
       77 WS-EFF-DATE                  PIC X(10)  VALUE SPACES.
       77 WS-EXP-DATE                  PIC X(10)  VALUE SPACES.
       77 S1                           PIC S999 COMP-3 VALUE +0.
       77 S2                           PIC S999 COMP-3 VALUE +0.
       77 WS-BUILD-SW                  PIC X.
          88  TIME-TO-BUILD               VALUE 'Y'.
       77 WS-SAVE-ERACCT               PIC X(2000).
       77 WS-DIS-RESP                  PIC 9(05) VALUE ZEROS.
       77 WS-PERFORM-SW                PIC X VALUE SPACES.
          88  GET-RATES                    VALUE 'R'.
          88  GET-ACT-ACCTS                VALUE 'A'.
       77  ws-bin-current-dt           pic xx value low-values.
       77 ws-bin-eff-dt                pic xx  value low-values.
       77 ws-bin-1st-pay-dt            pic xx  value low-values.
       77 WS-DISP-AMT                  PIC Z,ZZZ,Z99.99.
       77 ws-disp-rate                 pic z9.99999.
       77  WS-ERACCT-SW                PIC X VALUE ' '.
           88  END-OF-ERACCT                 VALUE 'Y'.
       77  WS-ERCTBL-SW                PIC X VALUE ' '.
           88  END-OF-ERCTBL                 VALUE 'Y'.
       77  WS-STATUS                   PIC X.
       77  ws-connect-sw               pic x value ' '.
           88  connected-to-db           value 'Y'.
       77  ws-socket-sw                pic x value ' '.
           88  end-of-socket              value 'Y'.
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
       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC
       01  sqlcmd                      pic x(1024).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
       01  ws-display-response         pic s9(9) value zeros.
       01  indicator-vaiables-for-nulls.
           05  nu-last-maint-dt        pic s9(4) comp value +0.
           05  nu-rolodex-print-dt     pic s9(4) comp value +0.
           05  nu-last-eom-stmt-dt     pic s9(4) comp value +0.
           05  nu-last-activity-date   pic s9(4) comp value +0.
           05  nu-last-stmt-dt         pic s9(4) comp value +0.
           05  nu-current-last-stmt-dt pic s9(4) comp value +0.
           05  nu-ga-effective-dt      pic s9(4) comp value +0.
           05  nu-ga-termination-dt    pic s9(4) comp value +0.
           05  nu-first-written-dt     pic s9(4) comp value +0.
       01  comp-record.
           12  cr-carrier                            pic x.
           12  cr-grouping                           pic x(6).
           12  cr-resp-no                            pic x(10).
           12  cr-account                            pic x(10).
           12  cr-type                               pic x.
           12  CR-LAST-MAINT-DT                      PIC x(10).
           12  CR-LAST-MAINT-HHMMSS                  PIC x(8).
           12  cr-last-maint-hhmmss-n redefines
               CR-LAST-MAINT-HHMMSS                  PIC -9(7).
           12  CR-LAST-MAINT-USER                    PIC X(4).
           12  CR-STMT-TYPE                          PIC XXX.
           12  CR-COMP-TYPE                          PIC X.
           12  CR-STMT-OWNER                         PIC X(4).
           12  CR-BALANCE-CONTROL                    PIC X.
           12  CR-INTERNAL-CONTROL-1                 PIC X.
           12  CR-INTERNAL-CONTROL-2                 PIC X.
           12  cr-ga-withold-pct                     pic x(7).
           12  CR-GA-WITHOLD-PCT-n redefines
               cr-ga-withold-pct                     PIC -9.9999.
           12  CR-GA-DIRECT-DEP                      PIC X.
           12  CR-ACCT-NAME                          PIC X(30).
           12  CR-MAIL-NAME                          PIC X(30).
           12  CR-ADDR-1                             PIC X(30).
           12  CR-ADDR-2                             PIC X(30).
           12  CR-ADDR-3                             pic x(29).
           12  CR-CSO-1099                           PIC X.
           12  CR-ZIP                                pic x(9).
           12  CR-SOC-SEC                            PIC X(13).
           12  CR-TELEPHONE                          pic x(10).
           12  CR-ROLODEX-PRINT-DT                   PIC X(10).
           12  CR-AR-BAL-LEVEL                       PIC X.
           12  CR-AR-NORMAL-PRINT                    PIC X.
           12  CR-AR-SUMMARY-CODE                    PIC X(6).
           12  CR-AR-REPORTING                       PIC X.
           12  CR-AR-PULL-CHECK                      PIC X.
           12  CR-AR-BALANCE-PRINT                   PIC X.
           12  CR-AR-LAST-RUN-CODE                   PIC X.
           12  CR-LAST-EOM-STMT-DT                   PIC X(10).
           12  CR-USER-CODE                          PIC X.
           12  CR-REPORT-GROUP-ID                    PIC X(12).
           12  CR-LAST-ACTIVITY-DATE                 pic x(10).
           12  CR-LAST-STMT-DT                       pic x(10).
           12  CR-BAL-FWD                            PIC -9(7).99.
           12  CR-CUR-COM                            PIC -9(7).99.
           12  CR-CUR-CHG                            PIC -9(7).99.
           12  CR-CUR-PMT                            PIC -9(7).99.
           12  CR-END-BAL                            PIC -9(7).99.
           12  CR-CUR                                PIC -9(7).99.
           12  CR-OV30                               PIC -9(7).99.
           12  CR-OV60                               PIC -9(7).99.
           12  CR-OV90                               PIC -9(7).99.
           12  CR-YTD-COM                            PIC -9(7).99.
           12  CR-YTD-OV                             PIC -9(7).99.
           12  CR-CUR-OVR-UNDR                       PIC -9(7).99.
           12  CR-YTD-OVR-UNDR                       PIC -9(7).99.
           12  CR-CUR-FICA                           PIC -9(7).99.
           12  CR-YTD-FICA                           PIC -9(7).99.
           12  CR-LF-CLM-AMT                         PIC -9(9).99.
           12  CR-AH-CLM-AMT                         PIC -9(9).99.
           12  CR-CURRENT-LAST-STMT-DT               pic x(10).
           12  CR-CURRENT-BAL-FWD                    PIC -9(7).99.
           12  CR-CURRENT-CUR-COM                    PIC -9(7).99.
           12  CR-CURRENT-CUR-CHG                    PIC -9(7).99.
           12  CR-CURRENT-CUR-PMT                    PIC -9(7).99.
           12  CR-CURRENT-END-BAL                    PIC -9(7).99.
           12  CR-CURRENT-CUR                        PIC -9(7).99.
           12  CR-CURRENT-OV30                       PIC -9(7).99.
           12  CR-CURRENT-OV60                       PIC -9(7).99.
           12  CR-CURRENT-OV90                       PIC -9(7).99.
           12  CR-CURRENT-YTD-COM                    PIC -9(7).99.
           12  CR-CURRENT-YTD-OV                     PIC -9(7).99.
           12  CR-YTD-PAID-COM                       PIC -9(7).99.
           12  CR-YTD-PAID-OV                        PIC -9(7).99.
           12  CR-CURRENT-MONTH-ACTIVITY             PIC X.
           12  CR-DELINQUENT-LETTER-CODE             PIC X.
           12  CR-CSR-CODE                           PIC X(4).
           12  CR-GA-EFFECTIVE-DT                    PIC X(10).
           12  CR-GA-TERMINATION-DT                  PIC X(10).
           12  CR-GA-STATUS-CODE                     PIC X.
           12  CR-GA-COMMENT-1                       PIC X(40).
           12  CR-GA-COMMENT-2                       PIC X(40).
           12  CR-GA-COMMENT-3                       PIC X(40).
           12  CR-GA-COMMENT-4                       PIC X(40).
           12  CR-RPTCD2                             PIC X(10).
           12  CR-OV120                              PIC -9(7).99.
           12  CR-CURRENT-OV120                      PIC -9(7).99.
           12  CR-TYPE-AGENT                         PIC X(01).
           12  CR-FAXNO                              pic x(10).
           12  CR-MD-GL-ACCT                         PIC X(10).
           12  CR-MD-DIV                             PIC XX.
           12  CR-MD-CENTER                          PIC X(4).
           12  cr-md-amt                             pic x(9).
           12  CR-MD-AMT-n redefines
               cr-md-amt                             PIC -9(5).99.
           12  CR-CREATE-AP-CHECK                    PIC X.
           12  CR-DELIVER-CK-TO-MEL                  PIC X.
           12  CR-ACH-STATUS                         PIC X.
           12  CR-BILL-SW                            PIC X.
           12  CR-CONTROL-NAME                       PIC X(30).
           12  CR-MAX-BANK-FEE-LEASE                 PIC -9(5).99.
           12  CR-MAX-BANK-FEE                       PIC -9(5).99.
           12  CR-CLP-STATE                          PIC XX.
           12  CR-FIRST-WRITTEN-DT                   PIC X(10).
           12  CR-SPP-REFUND-EDIT                    PIC X.
       EXEC SQL
          END DECLARE SECTION
       END-EXEC
       01  kt-sub                      pic s9(5) comp-3 value +0.
       01  kt-max                      pic s9(5) comp-3 value +0.
       01  WS-KEY-TABLE.
           05  key-table occurs 10.
               10  kt-carrier          pic x.
               10  kt-group            pic x(6).
               10  kt-fin-resp         pic x(10).
               10  kt-account          pic x(10).
               10  kt-type             pic x.
               10  kt-delim            pic x.
       01  ws-work-date.
           05  ws-work-ccyy            pic x(4).
           05  ws-work-mm              pic xx.
           05  ws-work-dd              pic xx.
       01  ws-work-date-num redefines ws-work-date
                                       pic 9(8).
       01  ws-work-time                pic 9(7).
       01  filler redefines ws-work-time.
           05  filler                  pic x.
           05  ws-hh                   pic 99.
           05  ws-mm                   pic 99.
           05  ws-ss                   pic 99.
       01  WS-FIN-RESP                 PIC X(10).
       01  WS-CO-DATA.
           05  WS-CO-NUM               PIC X(10).
           05  WS-CO-PRIMARY-CONTACT   PIC X(30).
           05  WS-CO-NAME              PIC X(30).
           05  WS-CO-MAIL-NAME         PIC X(30).
           05  WS-CO-ADDR1             PIC X(30).
           05  WS-CO-ADDR2             PIC X(30).
           05  WS-CO-ADDR3             PIC X(30).
           05  WS-CO-ZIP               PIC X(9).
           05  WS-CO-PHONE             PIC X(10).
       01  WS-CO-KEY.
           05  WS-CO-COMPANY-CD        PIC X.
           05  WS-CO-CARRIER           PIC X.
           05  WS-CO-GROUP             PIC X(6).
           05  WS-CO-FIN-RESP          PIC X(10).
           05  WS-CO-ACCOUNT           PIC X(10).
           05  WS-CO-TYPE              PIC X.
       01  WS-CID-NO                   PIC X(8).
       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.
      *                                 COPY ERCCOMP.
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
         05 CLIENT-IN-DATA.
            15  CLIENT-KICK-OFF      PIC X(8).
            15  CLIENT-ID            PIC XXX.
            15  client-cntr          pic 999.
            15  FILLER               PIC X(22).
         05 SOCKADDR-IN-PARM.
           15 SIN-FAMILY             PIC 9(4) COMP.
           15 SIN-PORT               PIC 9(4) COMP.
           15 SIN-ADDRESS            PIC 9(8) COMP.
           15 SIN-ZERO               PIC X(8).
       01  var  pic x(30).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA VAR.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'SOCK16' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           display 'SOCK16:transaction data = ', CLIENT-IN-DATA.
           display 'SOCK16:socket number    =', GIVE-TAKE-SOCKET.
           display ' client kick off **'client-kick-off '**'
           display ' client in data **' client-in-data '**'
           perform 0000-init           thru 0000-exit
           perform 0010-init-contact   thru 0010-exit
           perform 0100-process-socket thru 0100-exit until
              end-of-socket
           display ' after 0100 '
           move '0000;Success '        to ws-return-string
           PERFORM 0200-SEND-BUFFER    thru 0200-exit
           go to 0300-close-socket
           .
       0000-init.
           display ' made it to 0000-init '
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
           display ' client kick off **'client-kick-off '**'
           .
       0000-exit.
           exit.
       0010-init-contact.
           display ' made it to 0010-init '
           display ' client kick off **'client-kick-off '**'
           if client-kick-off = 'SOCKET16'
              continue
           else
              move '9999;Unknown origin, who are you? '
                                       to ws-return-string
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              display ' Unknown origin ' client-in-data
              go to 0300-close-socket
           end-if
           evaluate client-id
              when 'VPP'
                 MOVE X'07'            TO WS-COMP-CD
                 MOVE 'VPP'            TO WS-COMP-ID
              when 'AHL'
                 MOVE X'06'            TO WS-COMP-CD
                 MOVE 'AHL'            TO WS-COMP-ID
              when 'DCC'
                 MOVE X'05'            TO WS-COMP-CD
                 MOVE 'DCC'            TO WS-COMP-ID
              when 'CID'
                 MOVE X'04'            TO WS-COMP-CD
                 MOVE 'CID'            TO WS-COMP-ID
              when other
                 move '0113;Invalid company id ' to ws-return-string
                 PERFORM 0200-SEND-BUFFER thru 0200-exit
                 display ' Invalid company id ' client-id
                 go to 0300-close-socket
           END-evaluate
           if client-cntr numeric
              move client-cntr         to kt-max
           else
              move '0117;Invalid Counter ' to ws-return-string
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              display ' Invalid Counter  ' client-cntr
              go to 0300-close-socket
           end-if
           move 'SOCKET16READY'        to ws-send-buf
           move +25                    to ws-send-msg-size
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags
           if return-code <= zero
              display 'SOCK16:send error ' return-code
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if
           .
       0010-exit.
           exit.
       0100-process-socket.
           display ' made it to 0100-process '
           if kt-sub = +0
              perform 0110-receive     thru 0110-exit
           end-if
           compute kt-sub = kt-sub + +1
           if kt-sub > kt-max
              set end-of-socket to true
           end-if
           if end-of-socket
              go to 0100-exit
           end-if
           move ws-comp-cd             to ws-co-key
           move key-table(kt-sub)(1:28) to ws-co-key (2:28)
           perform 0400-get-ercomp     thru 0400-exit
           .
       0100-exit.
           exit.
       0110-receive.
           display ' made it to 0110-receive '
100917     move spaces                 to ws-recv-buf
           call "recv" using by value GIVE-TAKE-SOCKET
               by reference ws-recv-buf
               by value ws-recv-msg-size
               by value ws-flags.
           display ' ret code ' return-code
           if return-code < zero
              display 'SOCK16:recv error ' return-code
              perform 0250-socket-error thru 0250-exit
              set end-of-socket to true
              go to 0110-exit
           end-if
           if return-code = zero
              display 'SOCK16:client disconnected',
              perform 0250-socket-error thru 0250-exit
              set end-of-socket to true
              go to 0110-exit
           end-if
           display ' recv buf ' ws-recv-buf (1:50)
           move ws-recv-buf (1:290)     to ws-key-table
           if ws-recv-buf (1:4) = 'DONE' or 'done' or 'Done'
              set end-of-socket to true
           end-if
           .
       0110-exit.
           exit.
       0200-send-buffer.
           move ws-return-string       to ws-send-buf
           call "send" using by value GIVE-TAKE-SOCKET,
               by reference ws-send-buf,
               by value ws-send-msg-size,
               by value ws-flags.
           if return-code <= zero
              display 'SOCK16:send error ',
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if
           .
       0200-exit.
           exit.
       0250-socket-error.
           display "SOCK16:did not complete"
           display 'SOCK16:transaction data =', CLIENT-IN-DATA '**'
           display 'SOCK16:socket number    =', GIVE-TAKE-SOCKET.
           display ' return code = ' return-code
           .
       0250-exit.
           exit.
       0300-close-socket.
           if connected-to-db
              EXEC SQL
                  commit work release
              END-EXEC
              if sqlcode not = 0
                 move ' Failed to Commit DB '
                                       to ws-return-string
                 display "Error: commit release "
                 display ' sql return code ' sqlcode
                 display ' sql err mess    ' sqlerrmc
              end-if
           end-if
           if connected-to-db
              EXEC SQL
                  disconnect all
              END-EXEC
              move ' '                 to ws-connect-sw
           end-if
           display ' about to return cics '
           
      * exec cics return end-exec
      *    MOVE '.(                    ''   #00001119' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * goback

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK16' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback
           .
       0300-exit.
           exit.
       0400-GET-ERCOMP.
           display ' made it to 0400-get  '
      *    move ws-comp-cd to ws-co-company-cd
      *    move kt-carrier(1) to ws-co-carrier
      *    move kt-group(1) to ws-co-group
      *    move kt-fin-resp(1) to ws-co-fin-resp
      *    move kt-account(1) to ws-co-account
      *    move kt-type(1) to ws-co-type
           display ' ws co key ' ws-co-key (2:28)
           IF WS-CO-ACCOUNT = SPACES OR ZEROS OR LOW-VALUES
              MOVE LOW-VALUES          TO WS-CO-ACCOUNT
              MOVE 'G'                 TO WS-CO-TYPE
           END-IF
           
      * EXEC CICS READ
      *         INTO    (COMPENSATION-MASTER)
      *         DATASET ('ERCOMP')
      *         RIDFLD  (WS-CO-KEY)
      *         RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00001137' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031313337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 WS-CO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           display ' read response ' ws-response
           if not resp-normal
              go to 0400-exit
           end-if
           move co-carrier             to cr-carrier
           move co-grouping            to cr-grouping
           move co-resp-no             to cr-resp-no
           move co-account             to cr-account
           move co-type                to cr-type
           move co-last-maint-hhmmss   to ws-work-time
           string
              ws-hh   ':'
              ws-mm   ':'
              ws-ss delimited by size into cr-last-maint-hhmmss
           end-string
      *    move co-last-maint-hhmmss   to cr-last-maint-hhmmss-n
           move co-last-maint-user     to cr-last-maint-user
           if co-last-maint-dt <> spaces and low-values
              move co-last-maint-dt    to dc-bin-date-1
              set bin-to-greg to true *> move ' ' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-last-maint-dt
                 move +0               to nu-last-maint-dt
              else
                 display ' error maint dt invalid ' dc-error-code
                 move -1               to nu-last-maint-dt
              end-if
           else
              move spaces              to cr-last-maint-dt
              move -1                  to nu-last-maint-dt
           end-if
           move co-stmt-type           to cr-stmt-type
           move co-comp-type           to cr-comp-type
           move co-stmt-owner          to cr-stmt-owner
           move co-balance-control     to cr-balance-control
           move co-internal-control-1  to cr-internal-control-1
           move CO-INTERNAL-CONTROL-1  to cr-INTERNAL-CONTROL-1
           move CO-INTERNAL-CONTROL-2  to cr-INTERNAL-CONTROL-2
           move CO-GA-WITHOLD-PCT      to cr-GA-WITHOLD-PCT-n
           move CO-GA-DIRECT-DEP       to cr-GA-DIRECT-DEP
           move CO-ACCT-NAME           to cr-ACCT-NAME
           move CO-MAIL-NAME           to cr-MAIL-NAME
           move CO-ADDR-1              to cr-ADDR-1
           move CO-ADDR-2              to cr-ADDR-2
           move CO-ADDR-3              to cr-ADDR-3
           move CO-CSO-1099            to cr-CSO-1099
           move CO-ZIP                 to cr-ZIP
           move CO-SOC-SEC             to cr-SOC-SEC
           move CO-TELEPHONE           to cr-TELEPHONE
           if co-roladex-print-dt <> spaces and low-values
              move co-roladex-print-dt to dc-bin-date-1
              set bin-to-greg to true *> move ' ' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-rolodex-print-dt
                 move +0               to nu-rolodex-print-dt
              else
                 display ' error rolodex print dt invalid '
                    dc-error-code
                 move -1               to nu-rolodex-print-dt
              end-if
           else
              move spaces              to cr-rolodex-print-dt
              move -1                  to nu-rolodex-print-dt
           end-if
           move CO-AR-BAL-LEVEL        to cr-AR-BAL-LEVEL
           move CO-AR-NORMAL-PRINT     to cr-AR-NORMAL-PRINT
           move CO-AR-SUMMARY-CODE     to cr-AR-SUMMARY-CODE
           move CO-AR-REPORTING        to cr-AR-REPORTING
           move CO-AR-PULL-CHECK       to cr-AR-PULL-CHECK
           move CO-AR-BALANCE-PRINT    to cr-AR-BALANCE-PRINT
           move CO-AR-LAST-RUN-CODE    to cr-AR-LAST-RUN-CODE
           if co-LAST-EOM-STMT-DT <> spaces and low-values
              move co-LAST-EOM-STMT-DT to dc-bin-date-1
              set bin-to-greg to true *> move ' ' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-LAST-EOM-STMT-DT
                 move +0               to nu-LAST-EOM-STMT-DT
              else
                 display ' error last eom stmt dt invalid '
                    dc-error-code
                 move -1               to nu-LAST-EOM-STMT-DT
              end-if
           else
              move spaces              to cr-LAST-EOM-STMT-DT
              move -1                  to nu-LAST-EOM-STMT-DT
           end-if
           move CO-USER-CODE           to cr-USER-CODE
           move CO-REPORT-GROUP-ID     to cr-REPORT-GROUP-ID
           if co-LAST-ACTIVITY-DATE <> spaces and low-values
              move co-LAST-ACTIVITY-DATE
                                       to dc-greg-date-1-ymd-r
              set ymd-greg-to-bin to true *> move '3' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-LAST-ACTIVITY-DATE
                 move +0               to nu-LAST-ACTIVITY-DATE
              else
                 display ' error last activity dt invalid '
                    dc-error-code
                 move -1               to nu-LAST-ACTIVITY-DATE
              end-if
           else
              move spaces              to cr-LAST-ACTIVITY-DATE
              move -1                  to nu-LAST-ACTIVITY-DATE
           end-if
           if co-LAST-STMT-DT <> spaces and low-values
              move co-LAST-STMT-DT     to dc-greg-date-1-ymd-r
              set ymd-greg-to-bin to true *> move '3' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-LAST-STMT-DT
                 move +0               to nu-LAST-STMT-DT
              else
                 display ' error last stmt dt invalid '
                    dc-error-code
                 move -1               to nu-LAST-STMT-DT
              end-if
           else
              move spaces              to cr-LAST-STMT-DT
              move -1                  to nu-LAST-STMT-DT
           end-if
           move CO-BAL-FWD             to cr-BAL-FWD
           move CO-CUR-COM             to cr-CUR-COM
           move CO-CUR-CHG             to cr-CUR-CHG
           move CO-CUR-PMT             to cr-CUR-PMT
           move CO-END-BAL             to cr-END-BAL
           move CO-CUR                 to cr-CUR
           move CO-OV30                to cr-OV30
           move CO-OV60                to cr-OV60
           move CO-OV90                to cr-OV90
           move CO-YTD-COM             to cr-YTD-COM
           move CO-YTD-OV              to cr-YTD-OV
           move CO-CUR-OVR-UNDR        to cr-CUR-OVR-UNDR
           move CO-YTD-OVR-UNDR        to cr-YTD-OVR-UNDR
           move CO-CUR-FICA            to cr-CUR-FICA
           move CO-YTD-FICA            to cr-YTD-FICA
           move CO-LF-CLM-AMT          to cr-LF-CLM-AMT
           move CO-AH-CLM-AMT          to cr-AH-CLM-AMT
           if co-current-LAST-STMT-DT <> spaces and low-values
              move co-current-LAST-STMT-DT
                                       to dc-greg-date-1-ymd-r
              set ymd-greg-to-bin to true *> move '3' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-current-LAST-STMT-DT
                 move +0               to nu-current-LAST-STMT-DT
              else
                 display ' error current last stmt dt invalid '
                    dc-error-code
                 move -1               to nu-current-LAST-STMT-DT
              end-if
           else
              move spaces              to cr-current-LAST-STMT-DT
              move -1                  to nu-current-LAST-STMT-DT
           end-if
           move CO-CURRENT-BAL-FWD     to cr-CURRENT-BAL-FWD
           move CO-CURRENT-CUR-COM     to cr-CURRENT-CUR-COM
           move CO-CURRENT-CUR-CHG     to cr-CURRENT-CUR-CHG
           move CO-CURRENT-CUR-PMT     to cr-CURRENT-CUR-PMT
           move CO-CURRENT-END-BAL     to cr-CURRENT-END-BAL
           move CO-CURRENT-CUR         to cr-CURRENT-CUR
           move CO-CURRENT-OV30        to cr-CURRENT-OV30
           move CO-CURRENT-OV60        to cr-CURRENT-OV60
           move CO-CURRENT-OV90        to cr-CURRENT-OV90
           move CO-CURRENT-YTD-COM     to cr-CURRENT-YTD-COM
           move CO-CURRENT-YTD-OV      to cr-CURRENT-YTD-OV
           move CO-YTD-PAID-COM        to cr-YTD-PAID-COM
           move CO-YTD-PAID-OV         to cr-YTD-PAID-OV
           move CO-CURRENT-MONTH-ACTIVITY
                                       to cr-CURRENT-MONTH-ACTIVIty
           move CO-DELINQUENT-LETTER-CODE
                                       to cr-DELINQUENT-LETTER-COde
           move CO-CSR-CODE            to cr-CSR-CODE
           if co-GA-EFFECTIVE-DT <> spaces and low-values
              move co-GA-EFFECTIVE-DT to dc-bin-date-1
              set bin-to-greg to true *> move ' ' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-GA-EFFECTIVE-DT
                 move +0               to nu-GA-EFFECTIVE-DT
              else
                 display ' error GA EFF dt invalid '
                    dc-error-code
                 move -1               to nu-GA-EFFECTIVE-DT
              end-if
           else
              move spaces              to cr-GA-EFFECTIVE-DT
              move -1                  to nu-GA-EFFECTIVE-DT
           end-if
           if co-GA-termination-DT <> spaces and low-values
              move co-GA-TERMINATION-DT to dc-bin-date-1
              set bin-to-greg to true *> move ' ' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-GA-TERMINATION-DT
                 move +0               to nu-GA-TERMINATION-DT
              else
                 display ' error GA termination dt invalid '
                    dc-error-code
                 move -1               to nu-GA-TERMINATION-DT
              end-if
           else
              move spaces              to cr-GA-TERMINATION-DT
              move -1                  to nu-GA-TERMINATION-DT
           end-if
           move CO-GA-STATUS-CODE      to cr-GA-STATUS-CODE
           move CO-GA-COMMENT-1        to cr-GA-COMMENT-1
           move CO-GA-COMMENT-2        to cr-GA-COMMENT-2
           move CO-GA-COMMENT-3        to cr-GA-COMMENT-3
           move CO-GA-COMMENT-4        to cr-GA-COMMENT-4
           move CO-RPTCD2              to cr-RPTCD2
           move CO-OV120               to cr-OV120
           move CO-CURRENT-OV120       to cr-CURRENT-OV120
           move CO-TYPE-AGENT          to cr-TYPE-AGENT
           move CO-FAXNO               to cr-FAXNO
           move CO-MD-GL-ACCT          to cr-MD-GL-ACCT
           move CO-MD-DIV              to cr-MD-DIV
           move CO-MD-CENTER           to cr-MD-CENTER
           move CO-MD-AMT              to cr-MD-AMT-n
           move CO-CREATE-AP-CHECK     to cr-CREATE-AP-CHECK
           move CO-DELIVER-CK-TO-MEL   to cr-DELIVER-CK-TO-MEL
           move CO-ACH-STATUS          to cr-ACH-STATUS
           display ' co bill sw ' co-bill-sw
           move CO-BILL-SW             to cr-BILL-SW
           move CO-CONTROL-NAME        to cr-CONTROL-NAME
           move CO-MAX-BANK-FEE-LEASE  to cr-MAX-BANK-FEE-LEASE
           move CO-MAX-BANK-FEE        to cr-MAX-BANK-FEE
           move CO-CLP-STATE           to cr-CLP-STATE
           if co-FIRST-WRITTEN-DT <> spaces and low-values
              move co-FIRST-WRITTEN-DT to dc-bin-date-1
              set bin-to-greg to true *> move ' ' to dc-option
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-greg-date-a-edit
                                       to cr-FIRST-WRITTEN-DT
                 move +0               to nu-FIRST-WRITTEN-DT
              else
                 display ' error 1st written dt invalid '
                    dc-error-code
                 move -1               to nu-FIRST-WRITTEN-DT
              end-if
           else
              move spaces              to cr-FIRST-WRITTEN-DT
              move -1                  to nu-FIRST-WRITTEN-DT
           end-if
           move CO-SPP-REFUND-EDIT     to cr-SPP-REFUND-EDIT
           perform 0500-update-table   thru 0500-exit
           .
       0400-EXIT.
           EXIT.
       0500-update-table.
           display ' made it to 0500-update '
           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if
           display ' bill sw ' cr-bill-sw
           display ' key *' cr-carrier '*' cr-grouping '*'
              cr-resp-no '*' cr-account '*' cr-type '*'
           exec sql
              update
                 ERCOMP
              set
                 ex_stmt_type = :cr-stmt-type,
                 ex_comp_spp = :cr-comp-type,
                 ex_balance_control = :cr-balance-control,
                 ex_bill_sw = :cr-bill-sw,
                 ex_ga_withhold_pct = :cr-ga-withold-pct,
                 ex_ga_direct_deposit = :cr-ga-direct-dep,
                 ex_create_ap_check = :cr-create-ap-check,
                 ex_deliver_to_mel = :cr-deliver-ck-to-mel,
                 ex_md_gl_acct = :cr-md-gl-acct,
                 ex_md_div = :cr-md-div,
                 ex_md_center = :cr-md-center,
                 ex_md_amount = :cr-md-amt,
                 ex_report_group_id = :cr-report-group-id,
                 ex_comment1 = :cr-ga-comment-1,
                 ex_comment2 = :cr-ga-comment-2,
                 ex_comment3 = :cr-ga-comment-3,
                 ex_comment4 = :cr-ga-comment-4,
                 ex_status = :cr-ga-status-code,
                 ex_effect = :cr-GA-EFFECTIVE-DT :nu-GA-EFFECTIVE-DT,
                 ex_expire = :CR-GA-TERMINATION-DT
                             :nu-GA-TERMINATION-DT,
                 ex_last_maint_dt = :cr-last-maint-dt
                             :nu-last-maint-dt,
                 ex_last_maint_user = :cr-last-maint-user,
                 ex_last_maint_time = :cr-last-maint-hhmmss,
                 ex_stmt_owner = :cr-stmt-owner
              where
                 ex_carrier  = :cr-carrier and
                 ex_grouping = :cr-grouping and
                 ex_resp_no  = :cr-resp-no and
                 ex_account  = :cr-account and
                 ex_type     = :cr-type
           end-exec
           if sqlcode not = 0
              display "Error: did not update table "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move sqlcode             to ws-display-response
              move '9999;Did not update ' to ws-return-string
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           end-if
           display ' sql code upd ' sqlcode
           .
       0500-exit.
           exit.
       6000-CONNECT-TO-DB.
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
           MOVE 'HOVTSTDB01_Logic'
                                       TO SVR
           move 'appuser'              to usr
           move 'appuser@cso'          to pass
           if ws-kix-myenv = 'cid1p'
              MOVE 'SDVDB01_Logic'
                                       TO SVR
              move 'appuser'           to usr
              move 'appuser@cso'       to pass
           end-if
           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string
           display ' About to connect to ' svr ' ' usr-pass
           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
           if sqlcode not = 0
              display "Error: cannot connect to " svr
              display sqlcode
              display sqlerrmc
              move '9999;Could not connect to DB ' to ws-return-string
              PERFORM 0200-SEND-BUFFER thru 0200-exit
              perform 0250-socket-error thru 0250-exit
              go to 0300-close-socket
           else
              display ' Successful Connect ' sqlcode
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
      *    MOVE '."C                   (   #00001503' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353033' TO DFHEIV0(25:11)
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
           MOVE 'SOCK16' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'SOCK16' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
