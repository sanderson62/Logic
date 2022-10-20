00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1275.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 11/21/95 10:11:33.
00007 *                            VMOD=2.032.
00008 *
00008 *
00009 *AUTHOR.    LOGIC, INC.
00010 *           DALLAS, TEXAS.
00011
00024 *REMARKS.  TRANSACTION - EXX5
00025
00026 *        CERT COVERAGES DISPLAY PROGRAM.
00027
00028 *    SCREENS     - EL127E - CERTIFICATE PROFILE
00029
00030 *    ENTERED BY  - EL1273 - CERTIFICATE UPDATE
00031 *                - EL1274 - CERTIFICATE PROFILE
00032
00033 *    EXIT TO     - CALLING PROGRAM
00034 *                - EL132  - CLAIM LOOK-UP
00035
00036 *    INPUT FILE  - ELCERT - CERTIFICATE INFORCE FILE
00037 *                  ELCNTL - CONTROL FILE
00038
00039 *    NARRATIVE   - PROGRAM USES THE KEY TO THE CERTIFICATE MASTER
00040 *                  PASSED IN THE COMMAREA TO DISPLAY THE
00041 *                  COVERAGES ASSOCIATED WITH A CERTIFICATE AND
00042 *                  RETURNS WITH THE TRANSACTION OF THE CALLING
00043 *                  PROGRAM.
101201******************************************************************
101201*                   C H A N G E   L O G
101201*
101201* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101201*-----------------------------------------------------------------
101201*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101201* EFFECTIVE    NUMBER
101201*-----------------------------------------------------------------
101201* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
122002* 122002    2002111500006  PEMA  ADD MONTHLY PROCESSING
101509* 101509    2008100900003  AJRA  CALL NEW CERT NOTE SCREEN
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
010816* 010816  IR2015092900001  PEMA  USE CLP STATE WHERE NEEDED
092118* 092118  CR2018073000001  PEMA  Add lf & ah refund method
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
101201******************************************************************
00044
00045      EJECT
00046  ENVIRONMENT DIVISION.
00047
00048  DATA DIVISION.
00049
00050  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00051
00052  77  FILLER  PIC X(32)  VALUE '********************************'.
00053  77  FILLER  PIC X(32)  VALUE '*   EL1275 WORKING STORAGE     *'.
00054  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.032 *********'.
       77  P1                          PIC S999 COMP-3 VALUE +0.
080322 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
080322 77  WS-EDIT-AGE                 PIC S999       COMP-3 VALUE ZERO.
00055
00056  01  WS-DATE-AREA.
00057      05  SAVE-DATE                   PIC X(8)     VALUE SPACES.
00058      05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.
00059      05  WS-VALUATION-DT             PIC X(2)    VALUE LOW-VALUES.
092118 01  ws-elcrtt-sw                pic x  value spaces.
092118     88  good-hit-on-trlr          value 'Y'.
092118     88  crtt-not-found            value 'N'.
092118     88  crtt-not-read             value ' '.
       01  WS-RESPONSE                 PIC S9(8)       COMP.
           88  RESP-NORMAL                 VALUE +00.
           88  RESP-NOTFND                 VALUE +13.
           88  RESP-DUPKEY                 VALUE +15.
           88  RESP-NOTOPEN                VALUE +19.
           88  RESP-ENDFILE                VALUE +20.
00061  01  FILLER                          COMP-3.
00062      05  WS-ERROR-COUNT              PIC S9(3)    VALUE ZERO.
00063      05  WS-TIME-WORK                PIC S9(7)    VALUE ZERO.
00064      05  WS-TIME                     REDEFINES
00065          WS-TIME-WORK                PIC S9(3)V9(4).
00066      05  WS-NOT-FOUND                PIC S9       VALUE ZERO.
00067          88  BENEFIT-FOUND                        VALUE +1.
00068      05  ONE-MON-EARNED              PIC S9(7)V99 VALUE +0.
00069      05  WS-CALC-REFUND              PIC S9(7)V99 VALUE +0.
00070      05  WS-TERM                     PIC S9(4)    VALUE +0.
00071      05  WS-REMAINING-AMT            PIC S9(9)V99 VALUE +0.
           05  WS-PDEF-RECORD-SW       PIC X           VALUE ' '.
               88  PDEF-FOUND                          VALUE 'Y'.
00072
       01  ERPDEF-KEY-SAVE             PIC X(18).
       01  ERPDEF-KEY.
           12  ERPDEF-COMPANY-CD       PIC X.
           12  ERPDEF-STATE            PIC XX.
           12  ERPDEF-PROD-CD          PIC XXX.
           12  F                       PIC X(7).
           12  ERPDEF-BEN-TYPE         PIC X.
           12  ERPDEF-BEN-CODE         PIC XX.
           12  ERPDEF-EXP-DT           PIC XX.
00073  01  FILLER.
           05  WS-BROWSE-STARTED-SW    PIC  X(01)      VALUE SPACE.
               88  BROWSE-STARTED                      VALUE 'Y'.
           05  WS-ACCT-RECORD-SW       PIC  X(01)      VALUE SPACE.
               88  ACCT-FOUND                          VALUE 'Y'.
00074      05  WS-CALC-CD                  PIC X.
00075
00076      05  WS-BENEFIT-TYPE             PIC X        VALUE SPACE.
00077      05  WS-BENEFIT-NO               PIC XX       VALUE ZERO.
00078      05  WS-BENEFIT-DESCRIP          PIC X(10)    VALUE SPACES.
00079      05  WS-COMMENT                  PIC X(10)    VALUE SPACES.
00080      05  WS-KIND                     PIC X(3)     VALUE SPACES.
00081      05  WS-LF-COVERAGE-TYPE         PIC X        VALUE SPACE.
00082      05  WS-BENEFIT-CODE             PIC X(02)    VALUE SPACES.
00083      05  WS-EARNINGS-CALC            PIC X(01)    VALUE SPACE.
00084      05  WS-OVRD-EARNINGS-CALC       PIC X(01)    VALUE SPACE.
00085      05  WS-STATE-ABBREV             PIC X(02)    VALUE SPACES.
00086      05  WS-CLAIM-SW                 PIC X        VALUE SPACE.
00087          88  CHECK-AH-CLAIM          VALUE 'A'.
00088          88  CHECK-LF-CLAIM          VALUE 'L'.
00089
00090      05  WS-BROWSE-SW                PIC X        VALUE SPACE.
00091      05  WS-FREE-LOOK                PIC X        VALUE SPACE.
00092
00093      05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL1275S'.
00094      05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL127E'.
00095      05  WS-MAP-NUMBER               PIC X(4)     VALUE '127E'.
00096
00097      05  WS-PROGRAM-ID               PIC X(8)     VALUE 'EL1275'.
00098
00099      05  QID.
00100          10  QID-TERM                PIC X(04)    VALUE SPACES.
00101          10  FILLER                  PIC X(04)    VALUE '127E'.
00102
00103      05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXX5'.
00104      05  EL001                       PIC X(8)     VALUE 'EL001'.
00105      05  EL004                       PIC X(8)     VALUE 'EL004'.
00106      05  EL005                       PIC X(8)     VALUE 'EL005'.
00107      05  EL010                       PIC X(8)     VALUE 'EL010'.
00108      05  ELDATCV                     PIC X(8)     VALUE 'ELDATCV'.
00109      05  ELRTRM                      PIC X(8)     VALUE 'ELRTRM'.
00110      05  ELRAMT                      PIC X(8)     VALUE 'ELRAMT'.
00111      05  ELRFND                      PIC X(8)     VALUE 'ELRFND'.
00112
00113      05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'.
00114      05  WS-CERTIFICATE-MASTER-DSID  PIC X(8)     VALUE 'ELCERT'.
00115      05  WS-CLAIM-MASTER-DSID        PIC X(8)     VALUE 'ELMSTR5'.
00116
00117      05  DEEDIT-FIELD                PIC X(15).
00118      05  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
00119
00120      05  WS-ACCOUNT.
00121          10  FILLER                  PIC X(4).
00122          10  WS-ACCT                 PIC X(6).
00123
00124      05  WS-INDEX                    PIC S9(4)    VALUE ZERO
00125                                      COMP
00126                                      SYNC.
00127
00128      EJECT
00129      05  ER-0008                     PIC X(4)    VALUE '0008'.
00130      05  ER-0029                     PIC X(4)    VALUE '0029'.
00131      05  ER-0142                     PIC X(4)    VALUE '0142'.
00132      05  ER-7048                     PIC X(4)    VALUE '7048'.
00133      05  ER-7237                     PIC X(4)    VALUE '7237'.
00134
00135      05  WS-CONTROL-FILE-KEY.
00136          10  WS-CFK-COMPANY-ID       PIC X(3)     VALUE SPACES.
00137          10  WS-CFK-RECORD-TYPE      PIC X        VALUE ZERO.
00138 *          88  STATE-MASTER                       VALUE '3'.
00139 *          88  LF-BENEFIT-MASTER                  VALUE '4'.
00140 *          88  AH-BENEFIT-MASTER                  VALUE '5'.
00141 *          88  CARRIER-MASTER                     VALUE '6'.
00142          10  WS-CFK-ACCESS.
00143              15  WS-CFK-STATE        PIC XX       VALUE SPACES.
00144              15  WS-CFK-BENEFIT-NO                VALUE SPACES.
00145                  20  FILLER          PIC X.
00146                  20  WS-CFK-CARRIER  PIC X.
00147          10  WS-CFK-SEQUENCE-NO      PIC S9(4)    VALUE ZERO COMP.
00148
00149      05  WS-CERTIFICATE-KEY.
00150          10  WS-CK-COMPANY-CD        PIC X.
00151          10  WS-CK-CARRIER           PIC X.
00152          10  WS-CK-GROUPING          PIC X(6).
00153          10  WS-CK-STATE             PIC XX.
00154          10  WS-CK-ACCOUNT           PIC X(10).
00155          10  WS-CK-CERT-EFF-DT       PIC XX.
00156          10  WS-CK-CERT-NO.
00157              15  WS-CK-CERT-PRIME    PIC X(10).
00158              15  WS-CK-CERT-SFX      PIC X.
00159
00160      05  WS-CLAIM-KEY.
00161          10  WS-CL-COMPANY-CD        PIC X.
00162          10  WS-CL-CERT-NO.
00163              15  WS-CL-CERT-PRIME    PIC X(10).
00164              15  WS-CL-CERT-SFX      PIC X.
092118     05  WS-ELCRTT-KEY.
092118         10  WS-CS-COMPANY-CD        PIC X.
092118         10  WS-CS-CARRIER           PIC X.
092118         10  WS-CS-GROUPING          PIC X(6).
092118         10  WS-CS-STATE             PIC XX.
092118         10  WS-CS-ACCOUNT           PIC X(10).
092118         10  WS-CS-CERT-EFF-DT       PIC XX.
092118         10  WS-CS-CERT-NO.
092118             15  WS-CS-CERT-PRIME    PIC X(10).
092118             15  WS-CS-CERT-SFX      PIC X.
092118         10  WS-CS-REC-TYPE          PIC X.
           05  WS-ACCT-KEY.
               10  WS-AK-COMPANY-CD        PIC X.
               10  WS-AK-CARRIER           PIC X.
               10  WS-AK-GROUPING          PIC X(6).
               10  WS-AK-STATE             PIC XX.
               10  WS-AK-ACCOUNT           PIC X(10).
               10  WS-AK-ACCT-EXP-DT       PIC XX.
      *                                COPY ERCACCT.
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
092118*                                COPY ELCCRTT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCRTT.                            *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE TRAILERS                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 552  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCRTT                         RKP=2,LEN=34   *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
111204******************************************************************
111204*                   C H A N G E   L O G
111204*
111204* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111204*-----------------------------------------------------------------
111204*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111204* EFFECTIVE    NUMBER
111204*-----------------------------------------------------------------
111204* 111204                   PEMA  NEW FILE TO SPLIT BANK COMM
040109* 040109  2009031600001    AJRA  ADD NEW TRAILER TYPE AND REDEFINE
012010* 012010  2009061500002    AJRA  ADD FLAG FOR REFUND WITH OPEN CLA
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
022715* 022715  CR2015010800003  PEMA  AGENT SIGNATURE
020816* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
012918* 012918  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
091318* 091318  CR2018073000001  PEMA  ADD Refund methods
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
111204******************************************************************
00021
00022  01  CERTIFICATE-TRAILERS.
00023      12  CS-RECORD-ID                      PIC XX.
00024          88  VALID-CS-ID                      VALUE 'CS'.
00025
00026      12  CS-CONTROL-PRIMARY.
00027          16  CS-COMPANY-CD                 PIC X.
00028          16  CS-CARRIER                    PIC X.
00029          16  CS-GROUPING                   PIC X(6).
00032          16  CS-STATE                      PIC XX.
00033          16  CS-ACCOUNT                    PIC X(10).
00036          16  CS-CERT-EFF-DT                PIC XX.
00037          16  CS-CERT-NO.
00038              20  CS-CERT-PRIME             PIC X(10).
00039              20  CS-CERT-SFX               PIC X.
               16  CS-TRAILER-TYPE               PIC X.
                   88  COMM-TRLR           VALUE 'A'.
061013             88  CLAIM-HISTORY-TRLR  VALUE 'B'.
040109             88  CERT-DATA-TRLR      VALUE 'C'.
00040
040109     12  CS-DATA-AREA                      PIC X(516).
040109
040109     12  CS-BANK-COMMISSIONS REDEFINES CS-DATA-AREA.
040109         16  CS-BANK-COMMISSION-AREA.
040109             20  CS-BANK-COMMS       OCCURS 10.
040109                 24  CS-AGT                PIC X(10).
040109                 24  CS-COM-TYP            PIC X.
040109                 24  CS-SPP-FEES           PIC S9(5)V99   COMP-3.
040109                 24  CS-RECALC-LV-INDIC    PIC X.
040109                 24  FILLER                PIC X(10).
040109         16  FILLER                        PIC X(256).
040109
061013     12  CS-CLAIM-HISTORY-TRAILER REDEFINES CS-DATA-AREA.
061013****  TO CALC NO OF BENEFITS PAID = (CS-DAYS-PAID / 30)
               16  CS-MB-CLAIM-DATA OCCURS 24.
                   20  CS-CLAIM-NO               PIC X(7).
                   20  CS-CLAIM-TYPE             PIC X.
                       88  CS-AH-CLM               VALUE 'A'.
                       88  CS-IU-CLM               VALUE 'I'.
                       88  CS-GP-CLM               VALUE 'G'.
                       88  CS-LF-CLM               VALUE 'L'.
                       88  CS-PR-CLM               VALUE 'P'.
052614                 88  CS-FL-CLM               VALUE 'F'.
100518                 88  CS-OT-CLM               VALUE 'O'.
080322                 88  CS-BR-CLM               VALUE 'B'.
080322                 88  CS-HS-CLM               VALUE 'H'.
                   20  CS-INSURED-TYPE           PIC X.
                       88  CS-PRIM-INSURED          VALUE 'P'.
                       88  CS-CO-BORROWER           VALUE 'C'.
                   20  CS-BENEFIT-PERIOD         PIC 99.
                   20  CS-DAYS-PAID              PIC S9(5) COMP-3.
                   20  CS-TOTAL-PAID             PIC S9(7)V99 COMP-3.
                   20  CS-REMAINING-BENS         PIC S999 COMP-3.
               16  FILLER                        PIC X(12).
040109     12  CS-CERT-DATA REDEFINES CS-DATA-AREA.
040109         16  CS-VIN-NUMBER                 PIC X(17).
012010         16  CS-REFUND-CLAIM-FLAG          PIC X(01).
121712         16  CS-INS-AGE-DEFAULT-FLAG       PIC X(01).
121712         16  CS-JNT-AGE-DEFAULT-FLAG       PIC X(01).
022715         16  cs-agent-name.
022715             20  cs-agent-fname            pic x(20).
022715             20  cs-agent-mi               pic x.
022715             20  cs-agent-lname            pic x(25).
022715         16  cs-license-no                 pic x(15).
022715         16  cs-npn-number                 pic x(10).
022715         16  cs-agent-edit-status          pic x.
022715             88  cs-ae-refer-to-manager      value 'M'.
022715             88  cs-ae-cover-sheet           value 'C'.
022715             88  cs-ae-sig-form              value 'S'.
022715             88  cs-ae-verified              value 'V'.
022715             88  cs-unidentified-signature   value 'U'.
022715             88  cs-cert-returned            value 'R'.
022715             88  cs-accept-no-commission     value 'N'.
020816         16  cs-year                       pic 9999.
020816         16  cs-make                       pic x(20).
020816         16  cs-model                      pic x(20).
020816         16  cs-future                     pic x(20).
020816         16  cs-vehicle-odometer           pic s9(7) comp-3.
012918         16  cs-claim-verification-status  pic x.
012918             88  cs-clm-ver-eligible         value 'A'.
012918             88  cs-clm-ver-partial-elig     value 'B'.
012918             88  cs-clm-ver-not-eligible     value 'C'.
012918             88  cs-clm-ver-not-elig-opn-clm value 'D'.
012918             88  cs-clm-ver-not-part-elig-rw value 'E'.
012918             88  cs-clm-ver-ND-CERT          value 'F'.
012918             88  cs-clm-ver-spec-other       value 'G'.
012918             88  cs-clam-ver-pratial-corrected
012918                                             value 'H'.
012918             88  cs-clm-ver-no-matches       value 'I'.
012918             88  cs-clm-ver-not-elig-corrected
012918                                             value 'J'.
012918             88  cs-clm-ver-needs-review     value 'R'.
012918             88  cs-clm-ver-sent-to-claims   value 'W'.
091318         16  CS-LF-REFUND-METHOD           PIC X.
091318         16  CS-AH-REFUND-METHOD           PIC X.
020816         16  FILLER                        PIC X(353). *> was 420
121712*        16  FILLER                        PIC X(496).
00167 *                                    COPY ELCINTF.
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
00168      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00169          16  FILLER              PIC X(314).
00170          16  PI-1ST-TIME-SW      PIC X.
00171          16  PI-BENEFIT-IND      PIC X.
00172          16  PI-PREV-BENEFIT     PIC X.
00173          16  PI-PEND-SW          PIC X.
00174          16  PI-CLAIM-SW         PIC X.
00175          16  FILLER              PIC X(321).
00176
00178 *                                    COPY ELCDATE.
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
00180 *                                    COPY EL1275S.
       01  EL127EI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  EDATEL PIC S9(0004) COMP.
           05  EDATEF PIC  X(0001).
           05  FILLER REDEFINES EDATEF.
               10  EDATEA PIC  X(0001).
           05  EDATEI PIC  X(0008).
      *    -------------------------------
           05  ETIMEL PIC S9(0004) COMP.
           05  ETIMEF PIC  X(0001).
           05  FILLER REDEFINES ETIMEF.
               10  ETIMEA PIC  X(0001).
           05  ETIMEI PIC  X(0005).
      *    -------------------------------
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  EMEMCAPL PIC S9(0004) COMP.
           05  EMEMCAPF PIC  X(0001).
           05  FILLER REDEFINES EMEMCAPF.
               10  EMEMCAPA PIC  X(0001).
           05  EMEMCAPI PIC  X(0010).
      *    -------------------------------
           05  EASRISKL PIC S9(0004) COMP.
           05  EASRISKF PIC  X(0001).
           05  FILLER REDEFINES EASRISKF.
               10  EASRISKA PIC  X(0001).
           05  EASRISKI PIC  X(0001).
      *    -------------------------------
           05  ECERTNOL PIC S9(0004) COMP.
           05  ECERTNOF PIC  X(0001).
           05  FILLER REDEFINES ECERTNOF.
               10  ECERTNOA PIC  X(0001).
           05  ECERTNOI PIC  X(0010).
      *    -------------------------------
           05  ECRTSFXL PIC S9(0004) COMP.
           05  ECRTSFXF PIC  X(0001).
           05  FILLER REDEFINES ECRTSFXF.
               10  ECRTSFXA PIC  X(0001).
           05  ECRTSFXI PIC  X(0001).
      *    -------------------------------
           05  EACCTNOL PIC S9(0004) COMP.
           05  EACCTNOF PIC  X(0001).
           05  FILLER REDEFINES EACCTNOF.
               10  EACCTNOA PIC  X(0001).
           05  EACCTNOI PIC  X(0010).
      *    -------------------------------
           05  ESTATEL PIC S9(0004) COMP.
           05  ESTATEF PIC  X(0001).
           05  FILLER REDEFINES ESTATEF.
               10  ESTATEA PIC  X(0001).
           05  ESTATEI PIC  X(0002).
      *    -------------------------------
           05  ECARIERL PIC S9(0004) COMP.
           05  ECARIERF PIC  X(0001).
           05  FILLER REDEFINES ECARIERF.
               10  ECARIERA PIC  X(0001).
           05  ECARIERI PIC  X(0001).
      *    -------------------------------
           05  EGROUPL PIC S9(0004) COMP.
           05  EGROUPF PIC  X(0001).
           05  FILLER REDEFINES EGROUPF.
               10  EGROUPA PIC  X(0001).
           05  EGROUPI PIC  X(0006).
      *    -------------------------------
           05  EEFFDTL PIC S9(0004) COMP.
           05  EEFFDTF PIC  X(0001).
           05  FILLER REDEFINES EEFFDTF.
               10  EEFFDTA PIC  X(0001).
           05  EEFFDTI PIC  X(0008).
      *    -------------------------------
           05  EMEMNOL PIC S9(0004) COMP.
           05  EMEMNOF PIC  X(0001).
           05  FILLER REDEFINES EMEMNOF.
               10  EMEMNOA PIC  X(0001).
           05  EMEMNOI PIC  X(0012).
      *    -------------------------------
           05  ECLPSTL PIC S9(0004) COMP.
           05  ECLPSTF PIC  X(0001).
           05  FILLER REDEFINES ECLPSTF.
               10  ECLPSTA PIC  X(0001).
           05  ECLPSTI PIC  X(0002).
      *    -------------------------------
           05  ECLPL PIC S9(0004) COMP.
           05  ECLPF PIC  X(0001).
           05  FILLER REDEFINES ECLPF.
               10  ECLPA PIC  X(0001).
           05  ECLPI PIC  X(0009).
      *    -------------------------------
           05  EACLPL PIC S9(0004) COMP.
           05  EACLPF PIC  X(0001).
           05  FILLER REDEFINES EACLPF.
               10  EACLPA PIC  X(0001).
           05  EACLPI PIC  X(0009).
      *    -------------------------------
           05  EKINDL PIC S9(0004) COMP.
           05  EKINDF PIC  X(0001).
           05  FILLER REDEFINES EKINDF.
               10  EKINDA PIC  X(0001).
           05  EKINDI PIC  X(0012).
      *    -------------------------------
           05  ECODEL PIC S9(0004) COMP.
           05  ECODEF PIC  X(0001).
           05  FILLER REDEFINES ECODEF.
               10  ECODEA PIC  X(0001).
           05  ECODEI PIC  X(0002).
      *    -------------------------------
           05  EDESCL PIC S9(0004) COMP.
           05  EDESCF PIC  X(0001).
           05  FILLER REDEFINES EDESCF.
               10  EDESCA PIC  X(0001).
           05  EDESCI PIC  X(0010).
      *    -------------------------------
           05  ETERML PIC S9(0004) COMP.
           05  ETERMF PIC  X(0001).
           05  FILLER REDEFINES ETERMF.
               10  ETERMA PIC  X(0001).
           05  ETERMI PIC  X(0003).
      *    -------------------------------
           05  ERTERML PIC S9(0004) COMP.
           05  ERTERMF PIC  X(0001).
           05  FILLER REDEFINES ERTERMF.
               10  ERTERMA PIC  X(0001).
           05  ERTERMI PIC  X(0003).
      *    -------------------------------
           05  ECOMENTL PIC S9(0004) COMP.
           05  ECOMENTF PIC  X(0001).
           05  FILLER REDEFINES ECOMENTF.
               10  ECOMENTA PIC  X(0001).
           05  ECOMENTI PIC  X(0010).
      *    -------------------------------
           05  ETRMDAYL PIC S9(0004) COMP.
           05  ETRMDAYF PIC  X(0001).
           05  FILLER REDEFINES ETRMDAYF.
               10  ETRMDAYA PIC  X(0001).
           05  ETRMDAYI PIC  X(0005).
      *    -------------------------------
           05  EEXTDAYL PIC S9(0004) COMP.
           05  EEXTDAYF PIC  X(0001).
           05  FILLER REDEFINES EEXTDAYF.
               10  EEXTDAYA PIC  X(0001).
           05  EEXTDAYI PIC  X(0005).
      *    -------------------------------
           05  EENTSTL PIC S9(0004) COMP.
           05  EENTSTF PIC  X(0001).
           05  FILLER REDEFINES EENTSTF.
               10  EENTSTA PIC  X(0001).
           05  EENTSTI PIC  X(0010).
      *    -------------------------------
           05  EPREML PIC S9(0004) COMP.
           05  EPREMF PIC  X(0001).
           05  FILLER REDEFINES EPREMF.
               10  EPREMA PIC  X(0001).
           05  EPREMI PIC  X(0011).
      *    -------------------------------
           05  EALTPRML PIC S9(0004) COMP.
           05  EALTPRMF PIC  X(0001).
           05  FILLER REDEFINES EALTPRMF.
               10  EALTPRMA PIC  X(0001).
           05  EALTPRMI PIC  X(0011).
      *    -------------------------------
           05  ECURSTL PIC S9(0004) COMP.
           05  ECURSTF PIC  X(0001).
           05  FILLER REDEFINES ECURSTF.
               10  ECURSTA PIC  X(0001).
           05  ECURSTI PIC  X(0010).
      *    -------------------------------
           05  EBENEL PIC S9(0004) COMP.
           05  EBENEF PIC  X(0001).
           05  FILLER REDEFINES EBENEF.
               10  EBENEA PIC  X(0001).
           05  EBENEI PIC  X(0013).
      *    -------------------------------
           05  EALTBENL PIC S9(0004) COMP.
           05  EALTBENF PIC  X(0001).
           05  FILLER REDEFINES EALTBENF.
               10  EALTBENA PIC  X(0001).
           05  EALTBENI PIC  X(0012).
      *    -------------------------------
           05  EREMBENL PIC S9(0004) COMP.
           05  EREMBENF PIC  X(0001).
           05  FILLER REDEFINES EREMBENF.
               10  EREMBENA PIC  X(0001).
           05  EREMBENI PIC  X(0013).
      *    -------------------------------
           05  EEXPDTL PIC S9(0004) COMP.
           05  EEXPDTF PIC  X(0001).
           05  FILLER REDEFINES EEXPDTF.
               10  EEXPDTA PIC  X(0001).
           05  EEXPDTI PIC  X(0008).
      *    -------------------------------
           05  EREINSPL PIC S9(0004) COMP.
           05  EREINSPF PIC  X(0001).
           05  FILLER REDEFINES EREINSPF.
               10  EREINSPA PIC  X(0001).
           05  EREINSPI PIC  X(0012).
      *    -------------------------------
           05  ECRITPDL PIC S9(0004) COMP.
           05  ECRITPDF PIC  X(0001).
           05  FILLER REDEFINES ECRITPDF.
               10  ECRITPDA PIC  X(0001).
           05  ECRITPDI PIC  X(0003).
      *    -------------------------------
           05  EUEDTL PIC S9(0004) COMP.
           05  EUEDTF PIC  X(0001).
           05  FILLER REDEFINES EUEDTF.
               10  EUEDTA PIC  X(0001).
           05  EUEDTI PIC  X(0008).
      *    -------------------------------
           05  EUEPREML PIC S9(0004) COMP.
           05  EUEPREMF PIC  X(0001).
           05  FILLER REDEFINES EUEPREMF.
               10  EUEPREMA PIC  X(0001).
           05  EUEPREMI PIC  X(0012).
      *    -------------------------------
           05  EOMEARNL PIC S9(0004) COMP.
           05  EOMEARNF PIC  X(0001).
           05  FILLER REDEFINES EOMEARNF.
               10  EOMEARNA PIC  X(0001).
           05  EOMEARNI PIC  X(0013).
      *    -------------------------------
           05  EITDREFL PIC S9(0004) COMP.
           05  EITDREFF PIC  X(0001).
           05  FILLER REDEFINES EITDREFF.
               10  EITDREFA PIC  X(0001).
           05  EITDREFI PIC  X(0011).
      *    -------------------------------
           05  EITDPMTL PIC S9(0004) COMP.
           05  EITDPMTF PIC  X(0001).
           05  FILLER REDEFINES EITDPMTF.
               10  EITDPMTA PIC  X(0001).
           05  EITDPMTI PIC  X(0014).
      *    -------------------------------
           05  EACCPCTL PIC S9(0004) COMP.
           05  EACCPCTF PIC  X(0001).
           05  FILLER REDEFINES EACCPCTF.
               10  EACCPCTA PIC  X(0001).
           05  EACCPCTI PIC  X(0006).
      *    -------------------------------
           05  ECANDTL PIC S9(0004) COMP.
           05  ECANDTF PIC  X(0001).
           05  FILLER REDEFINES ECANDTF.
               10  ECANDTA PIC  X(0001).
           05  ECANDTI PIC  X(0008).
      *    -------------------------------
           05  EPTHRHDL PIC S9(0004) COMP.
           05  EPTHRHDF PIC  X(0001).
           05  FILLER REDEFINES EPTHRHDF.
               10  EPTHRHDA PIC  X(0001).
           05  EPTHRHDI PIC  X(0011).
      *    -------------------------------
           05  EPDTHRUL PIC S9(0004) COMP.
           05  EPDTHRUF PIC  X(0001).
           05  FILLER REDEFINES EPDTHRUF.
               10  EPDTHRUA PIC  X(0001).
           05  EPDTHRUI PIC  X(0008).
      *    -------------------------------
           05  EREITBLL PIC S9(0004) COMP.
           05  EREITBLF PIC  X(0001).
           05  FILLER REDEFINES EREITBLF.
               10  EREITBLA PIC  X(0001).
           05  EREITBLI PIC  X(0003).
      *    -------------------------------
           05  EEXITDTL PIC S9(0004) COMP.
           05  EEXITDTF PIC  X(0001).
           05  FILLER REDEFINES EEXITDTF.
               10  EEXITDTA PIC  X(0001).
           05  EEXITDTI PIC  X(0008).
      *    -------------------------------
           05  EEXDATEL PIC S9(0004) COMP.
           05  EEXDATEF PIC  X(0001).
           05  FILLER REDEFINES EEXDATEF.
               10  EEXDATEA PIC  X(0001).
           05  EEXDATEI PIC  X(0008).
      *    -------------------------------
           05  ESPECL PIC S9(0004) COMP.
           05  ESPECF PIC  X(0001).
           05  FILLER REDEFINES ESPECF.
               10  ESPECA PIC  X(0001).
           05  ESPECI PIC  X(0001).
      *    -------------------------------
           05  EEXBTCHL PIC S9(0004) COMP.
           05  EEXBTCHF PIC  X(0001).
           05  FILLER REDEFINES EEXBTCHF.
               10  EEXBTCHA PIC  X(0001).
           05  EEXBTCHI PIC  X(0008).
      *    -------------------------------
           05  EEXSTATL PIC S9(0004) COMP.
           05  EEXSTATF PIC  X(0001).
           05  FILLER REDEFINES EEXSTATF.
               10  EEXSTATA PIC  X(0001).
           05  EEXSTATI PIC  X(0008).
      *    -------------------------------
           05  EDVCDL PIC S9(0004) COMP.
           05  EDVCDF PIC  X(0001).
           05  FILLER REDEFINES EDVCDF.
               10  EDVCDA PIC  X(0001).
           05  EDVCDI PIC  X(0003).
      *    -------------------------------
           05  EEXITSTL PIC S9(0004) COMP.
           05  EEXITSTF PIC  X(0001).
           05  FILLER REDEFINES EEXITSTF.
               10  EEXITSTA PIC  X(0001).
           05  EEXITSTI PIC  X(0008).
      *    -------------------------------
           05  ECLSTATL PIC S9(0004) COMP.
           05  ECLSTATF PIC  X(0001).
           05  FILLER REDEFINES ECLSTATF.
               10  ECLSTATA PIC  X(0001).
           05  ECLSTATI PIC  X(0008).
      *    -------------------------------
           05  EPRRTL PIC S9(0004) COMP.
           05  EPRRTF PIC  X(0001).
           05  FILLER REDEFINES EPRRTF.
               10  EPRRTA PIC  X(0001).
           05  EPRRTI PIC  X(0008).
      *    -------------------------------
           05  REFMETHL PIC S9(0004) COMP.
           05  REFMETHF PIC  X(0001).
           05  FILLER REDEFINES REFMETHF.
               10  REFMETHA PIC  X(0001).
           05  REFMETHI PIC  X(0008).
      *    -------------------------------
           05  EALPRRTL PIC S9(0004) COMP.
           05  EALPRRTF PIC  X(0001).
           05  FILLER REDEFINES EALPRRTF.
               10  EALPRRTA PIC  X(0001).
           05  EALPRRTI PIC  X(0008).
      *    -------------------------------
           05  ECEDHDL PIC S9(0004) COMP.
           05  ECEDHDF PIC  X(0001).
           05  FILLER REDEFINES ECEDHDF.
               10  ECEDHDA PIC  X(0001).
           05  ECEDHDI PIC  X(0013).
      *    -------------------------------
           05  ECEDBENL PIC S9(0004) COMP.
           05  ECEDBENF PIC  X(0001).
           05  FILLER REDEFINES ECEDBENF.
               10  ECEDBENA PIC  X(0001).
           05  ECEDBENI PIC  X(0011).
      *    -------------------------------
           05  EERMSG1L PIC S9(0004) COMP.
           05  EERMSG1F PIC  X(0001).
           05  FILLER REDEFINES EERMSG1F.
               10  EERMSG1A PIC  X(0001).
           05  EERMSG1I PIC  X(0079).
      *    -------------------------------
           05  EERMSG2L PIC S9(0004) COMP.
           05  EERMSG2F PIC  X(0001).
           05  FILLER REDEFINES EERMSG2F.
               10  EERMSG2A PIC  X(0001).
           05  EERMSG2I PIC  X(0079).
      *    -------------------------------
           05  EPFKEYL PIC S9(0004) COMP.
           05  EPFKEYF PIC  X(0001).
           05  FILLER REDEFINES EPFKEYF.
               10  EPFKEYA PIC  X(0001).
           05  EPFKEYI PIC  99.
      *    -------------------------------
           05  EPFKEY7L PIC S9(0004) COMP.
           05  EPFKEY7F PIC  X(0001).
           05  FILLER REDEFINES EPFKEY7F.
               10  EPFKEY7A PIC  X(0001).
           05  EPFKEY7I PIC  X(0015).
      *    -------------------------------
           05  EPFKEY6L PIC S9(0004) COMP.
           05  EPFKEY6F PIC  X(0001).
           05  FILLER REDEFINES EPFKEY6F.
               10  EPFKEY6A PIC  X(0001).
           05  EPFKEY6I PIC  X(0017).
       01  EL127EO REDEFINES EL127EI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ETIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMEMCAPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EASRISKO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECRTSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EACCTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ESTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EEFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMEMNOO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECLPSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECLPO PIC  ZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EACLPO PIC  ZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EKINDO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECODEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EDESCO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ETERMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERTERMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECOMENTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ETRMDAYO PIC  ZZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EEXTDAYO PIC  ZZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EENTSTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EPREMO PIC  ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EALTPRMO PIC  ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECURSTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EBENEO PIC  ZZ,ZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EALTBENO PIC  ZZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EREMBENO PIC  ZZ,ZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EEXPDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EREINSPO PIC  Z,ZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECRITPDO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EUEDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EUEPREMO PIC  ZZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOMEARNO PIC  ZZ,ZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EITDREFO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EITDPMTO PIC  ZZZ,ZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EACCPCTO PIC  .99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECANDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EPTHRHDO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EPDTHRUO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EREITBLO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EEXITDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EEXDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ESPECO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EEXBTCHO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EEXSTATO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EDVCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EEXITSTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECLSTATO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EPRRTO PIC  Z9.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFMETHO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EALPRRTO PIC  Z9.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECEDHDO PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECEDBENO PIC  ZZZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EERMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EERMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EPFKEYO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EPFKEY7O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EPFKEY6O PIC  X(0017).
      *    -------------------------------
00182 *                                    COPY ELCCALC.
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
00185 *                                    COPY ELCEMIB.
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
00188 *                                    COPY ELCLOGOF.
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
00191 *                                    COPY ELCATTR.
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
00194 *                                    COPY ELCAID.
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
00195
00196  01  FILLER REDEFINES DFHAID.
00197      05  FILLER                      PIC X(8).
00198      05  PF-VALUES                   PIC X
00199          OCCURS 24 TIMES.
00200
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
00202
00203  01  DFHCOMMAREA                     PIC X(1024).
00204
00205 *01 PARMLIST   COMP SYNC.
00206 *    05  FILLER                      PIC S9(8).
00207 *    05  ELCERT-POINTER              PIC S9(8).
00208 *    05  ELCNTL-POINTER              PIC S9(8).
00209 *    05  ELMSTR-POINTER              PIC S9(8).
00210
00211      EJECT
00212 *                                    COPY ELCCERT.
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
00213      EJECT
00214 *                                    COPY ELCCNTL.
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
00215      EJECT
00216 *                                    COPY ELCMSTR.
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
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
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
022122*            88  hospital-claim         value 'H'.
022122*            88  bereavement-claim      value 'B'.
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
      *                                COPY ERCPDEF.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCPDEF.                            *
      *                                                                *
      *    FILE DESCRIPTION = PRODUCT DEFINITION MASTER                *
      *                                                                *
      *    FILE TYPE = VSAM,KSDS                                       *
      *    RECORD SIZE = 1319 RECFORM = FIXED                          *
      *                                                                *
      *    BASE CLUSTER = ERPDEF                      RKP=02,LEN=18    *
      *                                                                *
      *    LOG = YES                                                   *
      *    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
051414*                   C H A N G E   L O G
051414*
051414* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
051414*-----------------------------------------------------------------
051414*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
051414* EFFECTIVE    NUMBER
051414*-----------------------------------------------------------------
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100314* 100314  CR2014061900001  PEMA  ADD PCT OF BENEFIT
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  TANA  Add B and H claim types
      ******************************************************************
       01  PRODUCT-MASTER.
          12  PD-RECORD-ID                 PIC X(02).
              88  VALID-PD-ID                  VALUE 'PD'.
          12  PD-CONTROL-PRIMARY.
              16  PD-COMPANY-CD            PIC X.
              16  PD-STATE                 PIC XX.
              16  PD-PRODUCT-CD            PIC XXX.
              16  PD-FILLER                PIC X(7).
              16  PD-BEN-TYPE              PIC X.
              16  PD-BEN-CODE              PIC XX.
              16  PD-PROD-EXP-DT           PIC XX.
          12  FILLER                       PIC X(6).
          12  PD-PRODUCT-DATA OCCURS 11.
              16  PD-PROD-CODE             PIC X.
                  88  PD-PROD-LIFE           VALUE 'L'.
                  88  PD-PROD-PROP           VALUE 'P'.
                  88  PD-PROD-AH             VALUE 'A'.
                  88  PD-PROD-IU             VALUE 'I'.
                  88  PD-PROD-GAP            VALUE 'G'.
052614            88  PD-PROD-FAML           VALUE 'F'.
100518            88  PD-PROD-OTH            VALUE 'O'.
022122            88  PD-PROD-BRV            VALUE 'B'.
022122            88  PD-PROD-HOSP           VALUE 'H'.
              16  PD-MAX-ATT-AGE           PIC S999        COMP-3.
022122        16  PD-WAIT-PERIOD.
                  20  pd-wait-days         pic 99.
022122            20  PD-RET-ELIM          PIC X.
022122        16  FILLER                   PIC X.
021222*       16  PD-MIN-ISSUE-AGE         PIC S999        COMP-3.
021222*       16  PD-MAX-ISSUE-AGE         PIC S999        COMP-3.
              16  PD-MAX-TERM              PIC S999        COMP-3.
              16  PD-MAX-AMT               PIC S9(07)      COMP-3.
              16  FILLER                   PIC X.
              16  PD-PRE-EXIST-EXCL-TYPE   PIC 99.
              16  PD-EXCLUSION-PERIOD-DAYS PIC S999        COMP-3.
              16  PD-COVERAGE-ENDS-MOS     PIC S999        COMP-3.
              16  PD-ACCIDENT-ONLY-MOS     PIC S999        COMP-3.
              16  PD-CRIT-PERIOD           PIC S999        COMP-3.
              16  PD-REC-CRIT-PERIOD       PIC 99.
              16  PD-REC-CP-ALPHA  REDEFINES PD-REC-CRIT-PERIOD.
                  20  PD-RECURRING-YN      PIC X.
                  20  FILLER               PIC X.
              16  PD-RTW-MOS               PIC 99.
051414        16  PD-MAX-EXTENSION         PIC 99.
100314        16  pd-ben-pct               pic sv999 comp-3.
100314*       16  FILLER                   PIC XX.
          12  PD-1ST-YR-ADMIN-ALLOW        PIC S9(3)V99    COMP-3.
          12  PD-TERM-LIMITS OCCURS 15.
              16  PD-LOW-TERM              PIC S999        COMP-3.
              16  PD-HI-TERM               PIC S999        COMP-3.
      *  THE LOAN AMT LIMITS CORRESPOND TO THE TERM LIMITS ABOVE
          12  PD-LOAN-AMT-LIMITS OCCURS 15.
              16  PD-LOW-AMT               PIC S9(5)       COMP-3.
              16  PD-HI-AMT                PIC S9(7)       COMP-3.
          12  PD-EARN-FACTORS.
              16  FILLER OCCURS 15.
                  20  FILLER OCCURS 15.
                      24  PD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
          12  PD-PRODUCT-DESC              PIC X(80).
          12  PD-TRUNCATED                 PIC X.
          12  FILLER                       PIC X(7).
          12  PD-MAINT-INFORMATION.
              16  PD-LAST-MAINT-DT         PIC X(02).
              16  PD-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
              16  PD-LAST-MAINT-BY         PIC X(04).
00217
00218      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                CERTIFICATE-MASTER CONTROL-FILE
                                CLAIM-MASTER PRODUCT-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1275' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00220
00221      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00222      MOVE '5'                    TO  DC-OPTION-CODE.
00223      PERFORM 8500-DATE-CONVERSION.
00224      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00225      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00226
00227 *    NOTE *******************************************************
00228 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00229 *         *  FROM ANOTHER MODULE.                               *
00230 *         *******************************************************.
00231
00232      IF EIBCALEN NOT GREATER THAN ZERO
00233          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00234          GO TO 8300-SEND-TEXT.
00235
00236      
      * EXEC CICS HANDLE CONDITION
00237 *        ERROR  (9990-ERROR)
00238 *        QIDERR (0001-FIRST-TIME)
00239 *    END-EXEC.
      *    MOVE '"$.N                  ! " #00005061' TO DFHEIV0
           MOVE X'22242E4E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035303631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00240
00241      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00242      MOVE EIBTRMID               TO  QID-TERM.
00243
00244      
      * EXEC CICS READQ TS
00245 *        QUEUE    (QID)
00246 *        INTO     (PROGRAM-INTERFACE-BLOCK)
00247 *        LENGTH   (PI-COMM-LENGTH)
00248 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00005069' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00249
00250      
      * EXEC CICS DELETEQ TS
00251 *        QUEUE    (QID)
00252 *    END-EXEC.
      *    MOVE '*&                    #   #00005075' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00253
00254  0001-FIRST-TIME.
00255
00256      IF PI-1ST-TIME-SW = '1'
00257          MOVE '2'                TO  PI-1ST-TIME-SW
00258          GO TO 0100-RECEIVE.
00259
00260      IF PI-1ST-TIME-SW = '2'
00261          GO TO 0100-RECEIVE.
00262
00263      MOVE '1'                    TO  PI-1ST-TIME-SW.
00264      GO TO 1000-DISPLAY-CERTIFICATE.
00265
00266      EJECT
00267  0100-RECEIVE.
00268      IF EIBAID = DFHCLEAR
00269          GO TO 9400-CLEAR.
00270
00271      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00272          MOVE ER-0008            TO  EMI-ERROR
00273          PERFORM 9900-ERROR-FORMAT
00274          MOVE -1                 TO  EPFKEYL
00275          GO TO 8200-SEND-DATAONLY.
00276
00277      
      * EXEC CICS RECEIVE
00278 *        MAPSET (WS-MAPSET-NAME)
00279 *        MAP    (WS-MAP-NAME)
00280 *        INTO   (EL127EI)
00281 *    END-EXEC.
           MOVE LENGTH OF
            EL127EI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005102' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL127EI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00282
00283      IF EPFKEYL = 0
00284          GO TO 0200-CHECK-PFKEYS.
00285
00286      IF EIBAID NOT = DFHENTER
00287          MOVE ER-0008            TO  EMI-ERROR
00288          MOVE -1                 TO  EPFKEYL
00289          GO TO 0300-INPUT-ERROR.
00290
00291      IF (EPFKEYI IS NUMERIC) AND
00292          (EPFKEYI IS GREATER THAN 0 AND LESS THAN 25)
00293              MOVE PF-VALUES (EPFKEYI)    TO  EIBAID
00294      ELSE
00295          MOVE ER-0029                    TO  EMI-ERROR
00296          GO TO 0300-INPUT-ERROR.
00297
00298  0200-CHECK-PFKEYS.
00299
00300      IF EIBAID = DFHPF3
00301         MOVE 'EL1274'            TO  WS-PROGRAM-ID
00302         GO TO 9300-XCTL.
00303
00304      IF EIBAID = DFHPF4
00305          MOVE '1'                TO  PI-1ST-TIME-SW
00306          MOVE 'EL1273'           TO  WS-PROGRAM-ID
00307          GO TO 9300-XCTL.
00308
00309      IF EIBAID = DFHPF5
00310          IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
00311              MOVE ER-0029        TO  EMI-ERROR
00312              MOVE -1             TO  EPFKEYL
00313              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00314              GO TO 8200-SEND-DATAONLY
00315          ELSE
00316              IF PI-COMPANY-ID = 'DMD'
00317                  MOVE 'EL401DMD' TO  WS-PROGRAM-ID
00318                ELSE
101509*00319                  MOVE 'EL1276'   TO  WS-PROGRAM-ID
101509                 MOVE 'EL1279'   TO  WS-PROGRAM-ID
00320              END-IF
00321              GO TO 9300-XCTL.
00322
00323      IF PI-MAIL-YES
00324          IF EIBAID = DFHPF6
00325              IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
00326                  MOVE ER-0029    TO  EMI-ERROR
00327                  MOVE -1         TO  EPFKEYL
00328                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00329                  GO TO 8200-SEND-DATAONLY
00330              ELSE
00331                  MOVE 'EL1277'   TO  WS-PROGRAM-ID
00332                  GO TO 9300-XCTL.
00333
00334      IF EIBAID IS EQUAL TO DFHPF7
00335          IF PI-CLAIM-SW IS EQUAL TO 'Y'
00336                     AND
00337             CREDIT-SESSION
00338              MOVE ' '                    TO  PI-1ST-TIME-SW
00339              
      * EXEC CICS WRITEQ TS
00340 *                QUEUE    (QID)
00341 *                FROM     (PROGRAM-INTERFACE-BLOCK)
00342 *                LENGTH   (PI-COMM-LENGTH)
00343 *            END-EXEC
      *    MOVE '*"     L              ''   #00005165' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00344              MOVE WS-PROGRAM-ID          TO  PI-CALLING-PROGRAM
00345              MOVE 'EL132   '             TO  WS-PROGRAM-ID
00346              GO TO 9300-XCTL
00347          ELSE
00348              MOVE ER-0029                TO  EMI-ERROR
00349              MOVE -1                     TO  EPFKEYL
00350              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00351              GO TO 8200-SEND-DATAONLY.
00352
00353      IF EIBAID = DFHPF12
00354          MOVE 'EL010   '         TO  WS-PROGRAM-ID
00355          GO TO 9300-XCTL.
00356
00357      IF EIBAID = DFHPF23
00358          GO TO 9000-RETURN-CICS.
00359
00360      IF EIBAID = DFHPF24
00361          MOVE 'EL126   '         TO  WS-PROGRAM-ID
00362          GO TO 9300-XCTL.
00363
00364      IF EIBAID = DFHPF1 OR DFHPF2
00365          GO TO 1000-DISPLAY-CERTIFICATE.
00366
00367      IF EIBAID = DFHENTER
00368          GO TO 0400-EDIT.
00369
00370      MOVE ER-0029                TO  EMI-ERROR.
00371
00372  0300-INPUT-ERROR.
00373
00374      PERFORM 9900-ERROR-FORMAT.
00375      MOVE AL-UNBON               TO  EPFKEYA.
00376      MOVE -1                     TO  EPFKEYL.
00377      GO TO 8200-SEND-DATAONLY.
00378
00379  0400-EDIT.
00380      IF EUEDTL IS GREATER THAN 0
00381          MOVE EUEDTI                 TO  DEEDIT-FIELD
00382          PERFORM 7500-DEEDIT THRU 7500-EXIT
00383          IF DEEDIT-FIELD-V0 IS GREATER THAN 0
00384              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
00385              MOVE '4'                TO  DC-OPTION-CODE
00386              PERFORM 8500-DATE-CONVERSION
00387              IF NO-CONVERSION-ERROR
00388                  MOVE DC-BIN-DATE-1  TO  WS-VALUATION-DT
00389                  GO TO 1000-DISPLAY-CERTIFICATE.
00390
00391      MOVE PI-CR-MONTH-END-DT         TO  WS-VALUATION-DT.
00392      EJECT
00393  1000-DISPLAY-CERTIFICATE.
00394
00395 ******************************************************************
00396 *                                                                *
00397 *               READ THE REQUESTED CERT RECORD                   *
00398 *                                                                *
00399 ******************************************************************
00400
00401      MOVE PI-COMPANY-CD          TO  WS-CK-COMPANY-CD.
00402      MOVE PI-CARRIER             TO  WS-CK-CARRIER.
00403      MOVE PI-GROUPING            TO  WS-CK-GROUPING.
00404      MOVE PI-STATE               TO  WS-CK-STATE.
00405      MOVE PI-ACCOUNT             TO  WS-CK-ACCOUNT.
00406      MOVE PI-CERT-NO             TO  WS-CK-CERT-NO.
00407      MOVE PI-CERT-EFF-DT         TO  WS-CK-CERT-EFF-DT.
00408
00409      
      * EXEC CICS HANDLE CONDITION
00410 *        NOTFND (8880-NOT-FOUND)
00411 *    END-EXEC.
      *    MOVE '"$I                   ! # #00005235' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035323335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00412
00413      
      * EXEC CICS READ
00414 *        DATASET (WS-CERTIFICATE-MASTER-DSID)
00415 *        RIDFLD  (WS-CERTIFICATE-KEY)
00416 *        SET     (ADDRESS OF CERTIFICATE-MASTER)
00417 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005239' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CERTIFICATE-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CERTIFICATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00418
00419      EJECT
00420  2000-BUILD-OUTPUT-MAP.
00421
00422 ******************************************************************
00423 *                                                                *
00424 *              BUILD THE OUTPUT SCREEN TO BE DISPLAYED           *
00425 *                                                                *
00426 ******************************************************************
00427
00428      MOVE LOW-VALUES             TO  EL127EO.
00429      MOVE CM-CERT-PRIME          TO  ECERTNOO.
00430      MOVE CM-CERT-SFX            TO  ECRTSFXO.
00431      MOVE CM-ACCOUNT             TO  EACCTNOO
00432                                      WS-ACCOUNT.
00433      MOVE CM-STATE               TO  ESTATEO.
00434      MOVE CM-CARRIER             TO  ECARIERO.
00435      MOVE CM-GROUPING            TO  EGROUPO.
00436
00437      IF CM-MEMB-STATE   = CM-STATE OR
00438         CM-MEMB-ACCOUNT = WS-ACCT
00439          MOVE SPACES             TO  EMEMNOO
00440      ELSE
00441          MOVE CM-MEMBER-NO       TO  EMEMNOO.
00442
00443      IF CM-CERT-EFF-DT NOT = LOW-VALUES
00444          MOVE SPACES             TO  DC-OPTION-CODE
00445          MOVE CM-CERT-EFF-DT     TO  DC-BIN-DATE-1
00446          PERFORM 8500-DATE-CONVERSION
00447          MOVE DC-GREG-DATE-1-EDIT TO EEFFDTO.
00448
00449      MOVE CM-REIN-TABLE          TO  EREITBLO.
00450      MOVE CM-SPECIAL-REIN-CODE   TO  ESPECO.
00451
00452      IF PI-MAIL-YES
00453          MOVE AL-SANON           TO  EPFKEY6A
00454      ELSE
00455          MOVE AL-SADOF           TO  EPFKEY6A.
00456
00457      EJECT
00458 ******************************************************************
00459 *                                                                *
00460 *              PI-BENEFIT-INDICATOR VALUES:                      *
00461 *                   1 = LIFE COVERAGE ONLY                       *
00462 *                   2 = A&H  COVERAGE ONLY                       *
00463 *                   3 = TWO  COVERAGES PRESENT                   *
00464 *                                                                *
00465 ******************************************************************
00466      IF CM-LF-BENEFIT-CD NOT = '00'  AND
00467         CM-AH-BENEFIT-CD NOT = '00'
00468              MOVE '3'            TO  PI-BENEFIT-IND
00469          ELSE
00470              IF CM-LF-BENEFIT-CD NOT = '00'
00471                  MOVE '1'        TO  PI-BENEFIT-IND
00472              ELSE
00473                  IF CM-AH-BENEFIT-CD NOT = '00'
00474                      MOVE '2'    TO  PI-BENEFIT-IND
00475                  ELSE
00476                      MOVE '0'    TO  PI-BENEFIT-IND.
00477
00478      IF PI-BENEFIT-IND = '3' AND
00479          PI-1ST-TIME-SW = '1'
00480              GO TO 2100-LF-BENEFIT.
00481
00482      IF EIBAID = DFHPF1
00483          IF PI-BENEFIT-IND = '3'
00484              IF PI-PREV-BENEFIT = '1'
00485                  GO TO 2200-AH-BENEFIT.
00486
00487      IF EIBAID = DFHPF1
00488          IF PI-BENEFIT-IND = '3'
00489              IF PI-PREV-BENEFIT = '2'
00490                  MOVE ER-7237        TO  EMI-ERROR
00491                  MOVE -1             TO  EPFKEYL
00492                  PERFORM 9900-ERROR-FORMAT
00493                  GO TO 8200-SEND-DATAONLY.
00494
00495      IF EIBAID = DFHPF2
00496          IF PI-BENEFIT-IND = '3'
00497              IF PI-PREV-BENEFIT = '2'
00498                  GO TO 2100-LF-BENEFIT.
00499
00500      IF EIBAID = DFHPF2
00501          IF PI-BENEFIT-IND = '3'
00502              IF PI-PREV-BENEFIT = '1'
00503                  MOVE ER-7237        TO  EMI-ERROR
00504                  MOVE -1             TO  EPFKEYL
00505                  PERFORM 9900-ERROR-FORMAT
00506                  GO TO 8200-SEND-DATAONLY.
00507
00508      IF PI-BENEFIT-IND = '1'
00509          MOVE ER-7048                TO  EMI-ERROR
00510          PERFORM 9900-ERROR-FORMAT
00511          GO TO 2100-LF-BENEFIT.
00512
00513      IF PI-BENEFIT-IND = '2'
00514          MOVE ER-7048                TO  EMI-ERROR
00515          PERFORM 9900-ERROR-FORMAT
00516          GO TO 2200-AH-BENEFIT.
00517
00518      IF EIBAID = DFHENTER
00519          IF PI-PREV-BENEFIT IS EQUAL TO '1'
00520              GO TO 2100-LF-BENEFIT
00521          ELSE
00522              GO TO 2200-AH-BENEFIT.
00523
00524      GO TO 8200-SEND-DATAONLY.
00525
00526      EJECT
00527  2100-LF-BENEFIT.
00528      MOVE '1'                    TO  PI-PREV-BENEFIT.
00529
00530      MOVE '4'                    TO  WS-CFK-RECORD-TYPE.
00531      MOVE CM-LF-BENEFIT-CD       TO  WS-BENEFIT-NO.
00532      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
00533      PERFORM 3000-LOCATE-BENEFIT.
00534
00535      IF WS-CALC-CD IS EQUAL TO 'Z'
00536          MOVE AL-UNNOF           TO  ETERMA.
00537
00538      MOVE '2'                    TO CP-PROCESS-TYPE.
00539      MOVE WS-LF-COVERAGE-TYPE    TO  CP-BENEFIT-TYPE.
00540      MOVE WS-EARNINGS-CALC       TO  CP-EARNING-METHOD
00541                                      CP-RATING-METHOD.
00542      MOVE WS-CALC-CD             TO  CP-SPECIAL-CALC-CD.
00543      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
00544      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
00545      MOVE SAVE-BIN-DATE          TO  CP-VALUATION-DT.
00546      IF CM-LF-ORIG-TERM IS EQUAL TO 0
00547          MOVE 1                  TO  CP-ORIGINAL-TERM
00548      ELSE
00549          MOVE CM-LF-ORIG-TERM    TO  CP-ORIGINAL-TERM.
00550      MOVE CM-LOAN-TERM           TO  CP-LOAN-TERM.
00551      MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
00552      MOVE '4'                    TO  CP-REM-TERM-METHOD.
00553      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
00554      MOVE PI-COMPANY-CD          TO  CP-COMPANY-CD.
00555
00556 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
00557      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00558      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
00559      MOVE SPACES                 TO  WS-CFK-ACCESS.
00560      MOVE CM-STATE               TO  WS-CFK-STATE.
00561      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
00562      MOVE 'Y'                    TO  WS-FREE-LOOK.
00563
00564      PERFORM 3500-READ-CNTL THRU 3500-EXIT.
00565
00566      MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.
00567
00568      PERFORM 9800-LINK-REM-TERM.
00569
00570      MOVE PI-LIFE-OVERRIDE-L12   TO  EKINDO.
00571      MOVE CM-LF-BENEFIT-CD       TO  ECODEO.
00572      MOVE WS-BENEFIT-DESCRIP     TO  EDESCO.
00573      MOVE CM-LF-ORIG-TERM        TO  ETERMO.
00574      MOVE CP-REMAINING-TERM-3    TO  ERTERMO.
00575      IF CP-REMAINING-TERM-3 GREATER THAN CM-LF-ORIG-TERM
00576         MOVE CM-LF-ORIG-TERM     TO  ERTERMO.
00577      MOVE CM-LF-TERM-IN-DAYS     TO  ETRMDAYO.
00578      MOVE WS-COMMENT             TO  ECOMENTO.
00579      MOVE CM-LF-CRITICAL-PERIOD  TO  ECRITPDO.
00580      MOVE CM-LF-PREMIUM-AMT      TO  EPREMO.
           IF CP-EARNING-METHOD = 'B'
00581         MOVE CM-LF-ALT-PREMIUM-AMT  TO  EALTPRMO
           END-IF
           IF CM-LF-CLP NOT NUMERIC
              MOVE ZEROS               TO CM-LF-CLP
           END-IF
           MOVE CM-CLP-STATE           TO  ECLPSTO
           MOVE CM-LF-CLP              TO  ECLPO
      *    MOVE CM-ADDL-CLP            TO  EACLPO
00582      MOVE CM-LF-NSP-PREMIUM-AMT  TO  EREINSPO.
00583      MOVE CM-LF-BENEFIT-AMT      TO  EBENEO.
           IF CP-EARNING-METHOD = 'B'
00584         MOVE CM-LF-ALT-BENEFIT-AMT  TO  EALTBENO
           END-IF
00585      MOVE CM-LF-ITD-CANCEL-AMT   TO  EITDREFO.
00586      MOVE CM-LF-EXIT-BATCH       TO  EEXBTCHO.
00587      MOVE CM-LF-ITD-DEATH-AMT    TO  EITDPMTO.
00588      IF CM-LF-PREMIUM-RATE NUMERIC
00589          MOVE CM-LF-PREMIUM-RATE     TO  EPRRTO
00590      ELSE
00591          MOVE ZEROS                  TO  EPRRTO.
00592      IF CM-LF-ALT-PREMIUM-RATE NUMERIC
00593          MOVE CM-LF-ALT-PREMIUM-RATE TO  EALPRRTO
00594      ELSE
00595          MOVE ZEROS                  TO  EALPRRTO.
00596      MOVE CM-LF-DEV-CODE         TO  EDVCDO.
00597      MOVE CM-LIFE-COMM-PCT       TO  EACCPCTO.
00598      MOVE CM-REIN-TABLE          TO  EREITBLO.
00599      MOVE SPACES                 TO  EPDTHRUO.
00600
00601      MOVE SPACES                 TO  ECLSTATO.
00602      IF CM-CLAIM-ATTACHED-COUNT GREATER THAN +0
00603          MOVE '*'                TO  EASRISKO
00604          MOVE 'L'                TO WS-CLAIM-SW
00605          PERFORM 7600-CHECK-FOR-CLAIM
00606      ELSE
00607          MOVE ' '                TO  PI-CLAIM-SW
00608                                      EASRISKO.
00609
00610      IF CM-LF-LOAN-EXPIRE-DT IS NOT = LOW-VALUES AND SPACES
00611          MOVE SPACES                 TO  DC-OPTION-CODE
00612          MOVE CM-LF-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
00613          PERFORM 8500-DATE-CONVERSION
00614          MOVE DC-GREG-DATE-1-EDIT    TO  EEXPDTO.
00615
00616 ******************************************************************
00617 *               DISPLAY ENTRY STATUS CODES                       *
00618 ******************************************************************
00619
00620      MOVE SPACES                 TO EENTSTO.
00621
00622      IF CM-ENTRY-STATUS = '1'
00623          MOVE 'NORM'             TO  EENTSTO.
00624      IF CM-ENTRY-STATUS = '2'
00625          MOVE 'PEND'             TO  EENTSTO
00626          MOVE 'P'                TO  PI-PEND-SW.
00627      IF CM-ENTRY-STATUS = '4'
00628          MOVE 'CONV'             TO  EENTSTO.
00629      IF CM-ENTRY-STATUS = '5'
00630          MOVE 'REIS'             TO  EENTSTO.
122002     IF CM-ENTRY-STATUS = 'M'
122002         MOVE 'MONTHLY'          TO  EENTSTO.
00631      IF CM-ENTRY-STATUS = '9'
00632          MOVE 'REIN'             TO  EENTSTO.
00633
00634      IF CM-ENTRY-STATUS = 'D'
00635          MOVE 'DECL'             TO  EENTSTO.
00636
00637      IF CM-ENTRY-STATUS = 'V'
00638          MOVE 'VOID'             TO  EENTSTO.
00639
00640      IF CM-ENTRY-STATUS = 'U'
00641          MOVE 'UNDERWRT'         TO  EENTSTO.
00642
00643 ******************************************************************
00644 *                                                                *
00645 *               DISPLAY CURRENT STATUS CODES                     *
00646 *                                                                *
00647 ******************************************************************
00648
00649      MOVE SPACES                 TO ECURSTO.
00650
00651      IF CM-LF-CURRENT-STATUS = '1' OR '4'
00652          IF CP-REMAINING-TERM-3 = ZEROS
00653              MOVE 'EXPIRED'      TO  ECURSTO
00654          ELSE
00655              MOVE 'ACTIVE'       TO  ECURSTO.
00656
00657      IF CM-LF-CURRENT-STATUS = '2'
00658          MOVE 'PEND'             TO  ECURSTO
00659          MOVE 'P'                TO  PI-PEND-SW.
00660
00661      IF CM-LF-CURRENT-STATUS = '3'
00662          MOVE 'RESTORE'          TO  ECURSTO.
00663
00664      IF CM-LF-CURRENT-STATUS = '5'
00665          MOVE 'REISSUE'          TO  ECURSTO.
122002     IF CM-LF-CURRENT-STATUS = 'M'
122002         MOVE 'MONTHLY'          TO  ECURSTO.
00666
00667      IF CM-LF-CURRENT-STATUS = '6'
100518         MOVE 'LMP BEN'          TO  ECURSTO.
00669
00670      IF CM-LF-CURRENT-STATUS = '7'
00671          MOVE 'DEATH'            TO  ECURSTO.
00672
00673      IF CM-LF-CURRENT-STATUS = '8'
00674          MOVE 'CANCEL'           TO  ECURSTO.
00675
00676      IF CM-LF-CURRENT-STATUS = '9'
00677          MOVE 'RE-ONLY'          TO  ECURSTO.
00678
00679      IF CM-LF-CURRENT-STATUS = 'D'
00680          MOVE 'DECLINE'          TO  ECURSTO.
00681
00682      IF CM-LF-CURRENT-STATUS = 'V'
00683          MOVE 'VOID'             TO  ECURSTO.
00684
00685 ******************************************************************
00686 *                                                                *
00687 *            CALCULATE REMAINING BENEFIT AMOUNT                  *
00688 *                                                                *
00689 ******************************************************************
00690
00691      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00692      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
00693      MOVE SPACES                 TO  WS-CFK-ACCESS.
00694      MOVE CM-STATE               TO  WS-CFK-STATE.
00695      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
00696
00697      PERFORM 3500-READ-CNTL THRU 3500-EXIT.
00698
00699      MOVE CF-STATE-ABBREVIATION  TO  WS-STATE-ABBREV
00700                                      CP-STATE-STD-ABBRV.
00701
00702      MOVE CF-ST-FREE-LOOK-PERIOD TO  CP-FREE-LOOK.
00703
00704      MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
00705                                      CP-RATING-BENEFIT-AMT.
00706      MOVE CM-LF-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
00707      MOVE CM-LF-ALT-BENEFIT-AMT  TO  CP-ALTERNATE-BENEFIT.
00708      MOVE CM-LF-ALT-PREMIUM-AMT  TO  CP-ALTERNATE-PREMIUM.
00709      MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
00710      MOVE CM-PAY-FREQUENCY       TO  CP-PAY-FREQUENCY.
00711      MOVE CM-LOAN-TERM           TO  CP-LOAN-TERM.
00712
CIDMOD     MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE
00713      MOVE CP-REMAINING-TERM-3    TO  CP-REMAINING-TERM.
00714
00715      IF CM-LF-CURRENT-STATUS = '6' OR '7' OR '8'
00716          MOVE ZEROS                  TO  EREMBENO
00717      ELSE
              PERFORM 2500-GET-ERACCT  THRU 2500-EXIT
              MOVE ZEROS               TO CP-R-MAX-TOT-BEN
010816        IF (PI-COMPANY-ID = 'DCC' OR 'CAP')
                 AND (ACCT-FOUND)
                 AND (AM-DCC-PRODUCT-CODE <> SPACES)
                 MOVE ' '             TO WS-PDEF-RECORD-SW
                 PERFORM 2600-GET-MAX-BENEFIT
                                       THRU 2600-EXIT
080322           PERFORM 9800-LINK-REM-TERM
                 IF PDEF-FOUND
080322              IF CM-INSURED-JOINT-AGE LESS CM-INSURED-ISSUE-AGE
080322                  MOVE CM-INSURED-JOINT-AGE TO WS-EDIT-AGE
080322              ELSE
080322                  MOVE CM-INSURED-ISSUE-AGE TO WS-EDIT-AGE
080322              END-IF
080322              COMPUTE WS-ATT-AGE = WS-EDIT-AGE
080322                 + ((CM-LF-ORIG-TERM - CP-REMAINING-TERM-3) / 12)
                    display ' att age ' ws-att-age
                    PERFORM VARYING P1 FROM +1 BY +1 UNTIL
080322                 (P1 > +11)
                       OR ((PD-PROD-CODE (P1) = 'L' or 'O')
080322                   AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
                    END-PERFORM
080322              IF P1 < +12
                       MOVE PD-MAX-AMT (P1) TO CP-R-MAX-TOT-BEN
                       display ' max 1 ' cp-r-max-tot-ben
                    END-IF
                 END-IF
              END-IF
00718         PERFORM 9700-LINK-REM-AMT
              display ' max , rem ' cp-r-max-tot-ben ' '
                  cp-remaining-amt
00719         MOVE CP-REMAINING-AMT       TO  EREMBENO
           END-IF
00720
00721      IF PI-LIFE-OVERRIDE-L1 IS EQUAL TO 'P' OR
00722         WS-LF-COVERAGE-TYPE IS EQUAL TO 'P'
00723          COMPUTE WS-REMAINING-AMT = CM-LF-BENEFIT-AMT -
00724                                     CM-LF-ITD-DEATH-AMT
00725          MOVE WS-REMAINING-AMT       TO  EREMBENO.
00726
00727 ******************************************************************
00728 *                                                                *
00729 *            CALCULATE UNEARNED PREMIUM AMOUNT                   *
00730 *                      ONE MONTH EARNED PREMIUM AMOUNT           *
00731 *                                                                *
00732 ******************************************************************
00733
00734      MOVE '2'                    TO  CP-PROCESS-TYPE.
00735      MOVE CM-STATE               TO  CP-STATE.
00736      MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE.
00737      MOVE CM-LF-DEV-CODE         TO  CP-DEVIATION-CODE.
00738      MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
00739
00740      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00741      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
00742      MOVE SPACES                 TO  WS-CFK-ACCESS.
00743      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
00744      PERFORM 3500-READ-CNTL THRU 3500-EXIT.
00745      MOVE CF-LIFE-OVERRIDE-L1    TO  CP-LIFE-OVERRIDE-CODE.
00746      MOVE CF-CR-R78-METHOD       TO  CP-R78-OPTION.
00747      MOVE CF-CR-REM-TERM-CALC    TO  CP-REM-TERM-METHOD.
00748      MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
00749
00750      IF CP-STATE-STD-ABBRV = 'OR'
00751          COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
00752                                          CM-LF-ALT-BENEFIT-AMT.
00753
00754      MOVE WS-BENEFIT-CODE        TO  CP-BENEFIT-CD.
00755      IF WS-CALC-CD IS EQUAL TO 'T'
00756          MOVE WS-OVRD-EARNINGS-CALC  TO  CP-EARNING-METHOD.
00757
00758      IF WS-CALC-CD IS EQUAL TO 'D'
00759          MOVE CM-LF-TERM-IN-DAYS          TO  CP-TERM-OR-EXT-DAYS
00760      ELSE
00761          IF CM-PMT-EXTENSION-DAYS IS NUMERIC
00762              MOVE CM-PMT-EXTENSION-DAYS   TO  CP-TERM-OR-EXT-DAYS
00763          ELSE
00764              MOVE ZEROS                   TO  CP-TERM-OR-EXT-DAYS.
00765
00766      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
00767
00768      IF WS-VALUATION-DT IS EQUAL TO LOW-VALUES
00769          MOVE PI-CR-MONTH-END-DT TO  CP-VALUATION-DT
00770                                      WS-VALUATION-DT
00771      ELSE
00772          MOVE WS-VALUATION-DT    TO  CP-VALUATION-DT.
00773
00774      PERFORM 9800-LINK-REM-TERM.
00775      MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM.
00776      PERFORM 9700-LINK-REM-AMT.
00777      MOVE CP-REMAINING-AMT       TO  CP-REMAINING-BENEFIT.
00778
00779      MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM.
00780      PERFORM 9850-LINK-REFUND-RTN THRU 9850-EXIT.
00781      MOVE CP-CALC-REFUND         TO  EUEPREMO
00782                                      WS-CALC-REFUND.
00783      ADD +1                      TO  CP-REMAINING-TERM.
00784      IF CP-REMAINING-TERM IS EQUAL TO CM-LF-ORIG-TERM
00785          COMPUTE ONE-MON-EARNED = CM-LF-PREMIUM-AMT -
00786                                                 CP-CALC-REFUND
00787      ELSE
00788          PERFORM 9850-LINK-REFUND-RTN THRU 9850-EXIT
00789          COMPUTE ONE-MON-EARNED = WS-CALC-REFUND -
00790                                                 CP-CALC-REFUND.
00791
00792      MOVE ONE-MON-EARNED         TO  EOMEARNO.
00793
00794      IF WS-VALUATION-DT IS NOT = LOW-VALUES AND SPACES
00795          MOVE SPACES                 TO  DC-OPTION-CODE
00796          MOVE WS-VALUATION-DT        TO  DC-BIN-DATE-1
00797          PERFORM 8500-DATE-CONVERSION
00798          MOVE AL-UANON               TO  EUEDTA
00799          MOVE DC-GREG-DATE-1-EDIT    TO  EUEDTO.
00800
00801 ******************************************************************
00802 *                                                                *
00803 *                     DISPLAY CANCEL DATE                        *
00804 *                             CANCEL EXIT DATE                   *
00805 *                             CANCEL EXIT STATUS                 *
00806 *                                                                *
00807 ******************************************************************
00808
00809      MOVE SPACES                       TO  EEXITDTO.
00810
00811      IF CM-LF-CANCEL-EXIT-DT NOT = LOW-VALUES AND SPACES
00812          MOVE SPACES                   TO  DC-OPTION-CODE
00813          MOVE CM-LF-CANCEL-EXIT-DT     TO  DC-BIN-DATE-1
00814          PERFORM 8500-DATE-CONVERSION
00815          IF NOT DATE-CONVERSION-ERROR
00816              MOVE DC-GREG-DATE-1-EDIT  TO  EEXITDTO
00817          ELSE
00818              MOVE SPACES               TO  EEXITDTO.
00819
00820      MOVE SPACES                       TO  ECANDTO.
00821
00822      IF CM-LF-CANCEL-DT IS NOT = LOW-VALUES AND SPACES
00823          MOVE SPACES                   TO  DC-OPTION-CODE
00824          MOVE CM-LF-CANCEL-DT          TO  DC-BIN-DATE-1
00825          PERFORM 8500-DATE-CONVERSION
00826          IF NOT DATE-CONVERSION-ERROR
00827              MOVE DC-GREG-DATE-1-EDIT  TO  ECANDTO
00828          ELSE
00829              MOVE SPACES               TO  ECANDTO.
00830
00831      IF CM-LF-STATUS-AT-CANCEL = ' '
00832          MOVE SPACES             TO  EEXITSTO.
00833      IF CM-LF-STATUS-AT-CANCEL EQUAL '1' OR '2' OR '4' OR '9'
00834          MOVE 'ACTIVE'           TO  EEXITSTO.
00835      IF CM-LF-STATUS-AT-CANCEL = '6'
00836          MOVE 'PREV SETTLE'      TO  EEXITSTO.
00837      IF CM-LF-STATUS-AT-CANCEL = '7'
00838          MOVE 'PREV DTH'         TO  EEXITSTO.
00839      IF CM-LF-STATUS-AT-CANCEL = '8'
00840          MOVE 'PREV CANCEL'      TO  EEXITSTO.
00841
00842 ******************************************************************
00843 *                                                                *
00844 *                     DISPLAY CLAIM EXIT DATE                    *
00845 *                             CLAIM EXIT STATUS                  *
00846 *                                                                *
00847 ******************************************************************
00848
00849      MOVE SPACES                             TO  EEXDATEO.
00850
00851      IF CM-LF-DEATH-EXIT-DT IS NOT = LOW-VALUES AND
00852          (CM-LF-CURRENT-STATUS = '7' OR '8')
00853              MOVE SPACES                     TO  DC-OPTION-CODE
00854              MOVE CM-LF-DEATH-EXIT-DT        TO  DC-BIN-DATE-1
00855              PERFORM 8500-DATE-CONVERSION
00856              IF NOT DATE-CONVERSION-ERROR
00857                  MOVE DC-GREG-DATE-1-EDIT    TO  EEXDATEO
00858              ELSE
00859                  MOVE SPACES                 TO  EEXDATEO.
00860
00861      MOVE SPACES                 TO  EEXSTATO.
00862
00863      IF CM-LF-STATUS-AT-DEATH = ' '
00864          MOVE SPACES             TO  EEXSTATO.
00865      IF CM-LF-STATUS-AT-DEATH = '1' OR '2' OR '4' OR '9'
00866          MOVE 'ACTIVE'           TO  EEXSTATO.
00867      IF CM-LF-STATUS-AT-DEATH = '6'
00868          MOVE 'PREV SETTLE'      TO  EEXSTATO.
00869      IF CM-LF-STATUS-AT-DEATH = '7'
00870          MOVE 'PREV DTH'         TO  EEXSTATO.
00871      IF CM-LF-STATUS-AT-DEATH = '8'
00872          MOVE 'PREV CAN'         TO  EEXSTATO.
092118     move cm-control-primary     to ws-elcrtt-key
092118     move 'C'                    to ws-cs-rec-type
092118     
      * exec cics read
092118*       dataset   ('ELCRTT')
092118*       ridfld    (ws-elcrtt-key)
092118*       into      (certificate-trailers)
092118*       resp      (ws-response)
092118*    end-exec
           MOVE LENGTH OF
            certificate-trailers
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00005748' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303035373438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-trailers, 
                 DFHEIV11, 
                 ws-elcrtt-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
092118     if resp-normal
092118        set good-hit-on-trlr to true
092118         evaluate true
092118            when cs-lf-refund-method = '1'
092118               move 'R78'        to refmetho
092118            when cs-lf-refund-method = '2'
092118               move 'PRO'        to refmetho
092118            when cs-lf-refund-method = '3'
092118               move 'CALIF'      to refmetho
092118            when cs-lf-refund-method = '4'
092118               move 'IRR/FARM'   to refmetho
092118            when cs-lf-refund-method = '5'
092118               move 'NET'        to refmetho
092118            when cs-lf-refund-method = '6'
092118               move 'ANTIC'      to refmetho
092118            when cs-lf-refund-method = '8'
092118               move 'MEAN'       to refmetho
092118            when cs-lf-refund-method = '9'
092118               move 'SUMDI'      to refmetho
092118            when cs-lf-refund-method = 'G'
092118               move 'GAPNR'      to refmetho
092118            when cs-lf-refund-method = 'S'
092118               move 'GAPA'       to refmetho
092118            when cs-lf-refund-method = 'D'
092118               move 'DDF'        to refmetho
092118            when cs-lf-refund-method = 'I'
092118               move 'DDFIU'      to refmetho
092118            when cs-lf-refund-method = 'R'
092118               move 'REPO'       to refmetho
092118            when other
092118               move cs-lf-refund-method
092118                                 to refmetho
092118         end-evaluate
092118     else
092118        set crtt-not-found to true
092118     end-if
00873
00874 ******************************************************************
00875 *                                                                *
00876 *                   DISPLAY EXTENSION DAYS                       *
00877 *                                                                *
00878 ******************************************************************
00879
00880      IF CM-PMT-EXTENSION-DAYS NUMERIC
00881          MOVE CM-PMT-EXTENSION-DAYS  TO  EEXTDAYO
00882      ELSE
00883          MOVE ZEROS                  TO  EEXTDAYO.
00884
00885 ******************************************************************
00886 *                                                                *
00887 *                   DISPLAY CEDED BENEFIT                        *
00888 *                                                                *
00889 ******************************************************************
00890
121802*    IF PI-COMPANY-ID EQUAL 'CVL' OR 'LGX'
121802*        IF CM-LF-CEDED-BENEFIT NUMERIC
121802*            MOVE CM-LF-CEDED-BENEFIT
121802*                                TO  ECEDBENO
121802*        ELSE
121802*            MOVE ZEROS          TO  ECEDBENO
121802*    ELSE
00898          MOVE AL-SADOF           TO  ECEDBENA, ECEDHDA.
00899
00900      GO TO 8100-SEND-INITIAL-MAP.
00901      EJECT
00902  2200-AH-BENEFIT.
00903
00904      MOVE '2'                    TO  PI-PREV-BENEFIT.
00905
00906      MOVE '5'                    TO  WS-CFK-RECORD-TYPE.
00907      MOVE CM-AH-BENEFIT-CD       TO  WS-BENEFIT-NO.
00908      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
00909      PERFORM 3000-LOCATE-BENEFIT.
00910
00911      IF WS-CALC-CD IS EQUAL TO 'Z'
00912          MOVE AL-UNNOF           TO  ETERMA.
00913
00914      MOVE '2'                    TO  CP-PROCESS-TYPE.
00915      MOVE 'A'                    TO  CP-BENEFIT-TYPE.
00916      MOVE WS-EARNINGS-CALC       TO  CP-EARNING-METHOD
00917                                      CP-RATING-METHOD.
00918      MOVE WS-CALC-CD             TO  CP-SPECIAL-CALC-CD.
00919      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
00920      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
00921      MOVE SAVE-BIN-DATE          TO  CP-VALUATION-DT.
00922      IF CM-AH-ORIG-TERM IS EQUAL TO 0
00923          MOVE 1                  TO  CP-ORIGINAL-TERM
00924      ELSE
00925          MOVE CM-AH-ORIG-TERM    TO  CP-ORIGINAL-TERM.
00926      MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
00927      MOVE '4'                    TO  CP-REM-TERM-METHOD.
00928      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
00929      MOVE PI-COMPANY-CD          TO  CP-COMPANY-CD.
00930
00931 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
00932      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00933      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
00934      MOVE SPACES                 TO  WS-CFK-ACCESS.
00935      MOVE CM-STATE               TO  WS-CFK-STATE.
00936      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
00937      MOVE 'Y'                    TO  WS-FREE-LOOK.
00938
00939      PERFORM 3500-READ-CNTL THRU 3500-EXIT.
00940
00941      MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.
00942
00943      PERFORM 9800-LINK-REM-TERM.
00944
00945      MOVE PI-AH-OVERRIDE-L12     TO  EKINDO.
00946      MOVE CM-AH-BENEFIT-CD       TO  ECODEO.
00947      MOVE WS-BENEFIT-DESCRIP     TO  EDESCO.
00948      MOVE CM-AH-ORIG-TERM        TO  ETERMO.
00949      MOVE CP-REMAINING-TERM-1    TO  ERTERMO.
00950      MOVE WS-COMMENT             TO  ECOMENTO.
00951      MOVE CM-AH-CRITICAL-PERIOD  TO  ECRITPDO.
00952      MOVE CM-AH-PREMIUM-AMT      TO  EPREMO.
00953      MOVE CM-AH-NSP-PREMIUM-AMT  TO  EREINSPO.
           IF CM-AH-CLP NOT NUMERIC
              MOVE ZEROS               TO CM-AH-CLP
           END-IF
           IF CM-ADDL-CLP NOT NUMERIC
              MOVE ZEROS               TO CM-ADDL-CLP
           END-IF
           MOVE CM-CLP-STATE           TO  ECLPSTO
           MOVE CM-AH-CLP              TO  ECLPO
           MOVE CM-ADDL-CLP            TO  EACLPO
00954      MOVE CM-AH-BENEFIT-AMT      TO  EBENEO.
00955      MOVE CM-AH-ITD-CANCEL-AMT   TO  EITDREFO.
00956      MOVE CM-AH-EXIT-BATCH       TO  EEXBTCHO.
00957      MOVE CM-AH-ITD-AH-PMT       TO  EITDPMTO.
00958      IF CM-AH-PREMIUM-RATE NUMERIC
00959          MOVE CM-AH-PREMIUM-RATE TO  EPRRTO
00960      ELSE
00961          MOVE ZEROS              TO  EPRRTO.
00962      MOVE CM-AH-DEV-CODE         TO  EDVCDO.
00963      MOVE CM-AH-COMM-PCT         TO  EACCPCTO.
00964
00965      MOVE SPACES                 TO  ECLSTATO.
00966      IF CM-CLAIM-ATTACHED-COUNT GREATER THAN +0
00967          MOVE '*'                TO  EASRISKO
00968          MOVE 'A'                TO  WS-CLAIM-SW
00969          PERFORM 7600-CHECK-FOR-CLAIM
00970      ELSE
00971          MOVE ' '                TO  PI-CLAIM-SW
00972                                      EASRISKO.
00973
00974      IF CM-AH-LOAN-EXPIRE-DT IS NOT = LOW-VALUES AND SPACES
00975          MOVE SPACES                 TO  DC-OPTION-CODE
00976          MOVE CM-AH-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
00977          PERFORM 8500-DATE-CONVERSION
00978          MOVE DC-GREG-DATE-1-EDIT    TO  EEXPDTO.
00979
00980      IF CM-AH-PAID-THRU-DT IS NOT = LOW-VALUES AND SPACES
00981         IF NOT PI-USES-PAID-TO
00982            MOVE ' '                    TO  DC-OPTION-CODE
00983            MOVE CM-AH-PAID-THRU-DT     TO  DC-BIN-DATE-1
00984            PERFORM 8500-DATE-CONVERSION
00985            MOVE DC-GREG-DATE-1-EDIT    TO  EPDTHRUO
00986         ELSE
00987            MOVE '6'                    TO  DC-OPTION-CODE
00988            MOVE CM-AH-PAID-THRU-DT     TO  DC-BIN-DATE-1
00989            MOVE +1                     TO  DC-ELAPSED-DAYS
00990            MOVE +0                     TO  DC-ELAPSED-MONTHS
00991            PERFORM 8500-DATE-CONVERSION
00992            MOVE DC-GREG-DATE-1-EDIT    TO  EPDTHRUO.
00993
00994 ******************************************************************
00995 *                                                                *
00996 *               DISPLAY ENTRY STATUS CODES                       *
00997 *                                                                *
00998 ******************************************************************
00999
01000      MOVE SPACES                 TO  EENTSTO.
01001
01002      IF CM-ENTRY-STATUS = '1'
01003          MOVE 'NORM'             TO  EENTSTO.
01004      IF CM-ENTRY-STATUS = '2'
01005          MOVE 'PEND'             TO  EENTSTO
01006          MOVE 'P'                TO  PI-PEND-SW.
01007      IF CM-ENTRY-STATUS = '4'
01008          MOVE 'CONV'             TO  EENTSTO.
01009      IF CM-ENTRY-STATUS = '5'
01010          MOVE 'REIS'             TO  EENTSTO.
122002     IF CM-ENTRY-STATUS = 'M'
122002         MOVE 'MONTHLY'          TO  EENTSTO.
01011      IF CM-ENTRY-STATUS = '9'
01012          MOVE 'REIN'             TO  EENTSTO.
01013
01014      IF CM-ENTRY-STATUS = 'D'
01015          MOVE 'DECL'             TO  EENTSTO.
01016
01017      IF CM-ENTRY-STATUS = 'V'
01018          MOVE 'VOID'             TO  EENTSTO.
01019
01020      IF CM-ENTRY-STATUS = 'U'
01021          MOVE 'UNDERWRT'         TO  EENTSTO.
01022
01023 ******************************************************************
01024 *                                                                *
01025 *               DISPLAY CURRENT STATUS CODES                     *
01026 *                                                                *
01027 ******************************************************************
01028
01029      MOVE SPACES                 TO  ECURSTO.
01030
01031      IF CM-AH-CURRENT-STATUS = '1' OR '4'
01032          IF CP-REMAINING-TERM-3 = ZEROS
01033              MOVE 'EXPIRED'      TO  ECURSTO
01034          ELSE
01035              MOVE 'ACTIVE'       TO  ECURSTO.
01036
01037      IF CM-AH-CURRENT-STATUS = '2'
01038          MOVE 'PEND'             TO  ECURSTO
01039          MOVE 'P'                TO  PI-PEND-SW.
01040
01041      IF CM-AH-CURRENT-STATUS = '3'
01042          MOVE 'RESTORE'          TO  ECURSTO.
01043
01044      IF CM-AH-CURRENT-STATUS = '5'
01045          MOVE 'REISSUE'          TO  ECURSTO.
122002     IF CM-AH-CURRENT-STATUS = 'M'
122002         MOVE 'MONTHLY'          TO  ECURSTO.
01046
01047      IF CM-AH-CURRENT-STATUS = '6'
01048          MOVE 'LMP DIS'          TO  ECURSTO.
01049
01050      IF CM-AH-CURRENT-STATUS = '7'
01051          MOVE 'DEATH'            TO  ECURSTO.
01052
01053      IF CM-AH-CURRENT-STATUS = '8'
01054          MOVE 'CANCEL'           TO  ECURSTO.
01055
01056      IF CM-AH-CURRENT-STATUS = '9'
01057          MOVE 'RE-ONLY'          TO  ECURSTO.
01058
01059      IF CM-AH-CURRENT-STATUS = 'D'
01060          MOVE 'DECLINE'          TO  ECURSTO.
01061
01062      IF CM-AH-CURRENT-STATUS = 'V'
01063          MOVE 'VOID'             TO  ECURSTO.
01064
01065 ******************************************************************
01066 *                                                                *
01067 *            CALCULATE REMAINING BENEFIT AMOUNT                  *
01068 *                                                                *
01069 ******************************************************************
01070
01071      IF CM-AH-CURRENT-STATUS = '6' OR '7' OR '8'
01072          MOVE ZEROS              TO  EREMBENO
01073      ELSE
              PERFORM 2500-GET-ERACCT  THRU 2500-EXIT
              MOVE ZEROS               TO CP-R-MAX-MON-BEN
010816        IF (PI-COMPANY-ID = 'DCC' OR 'CAP')
                 AND (ACCT-FOUND)
                 AND (AM-DCC-PRODUCT-CODE = 'DDF')
                 MOVE ' '             TO WS-PDEF-RECORD-SW
                 PERFORM 2600-GET-MAX-BENEFIT
                                       THRU 2600-EXIT
                 IF PDEF-FOUND
080322              IF CM-INSURED-JOINT-AGE LESS CM-INSURED-ISSUE-AGE
080322                  MOVE CM-INSURED-JOINT-AGE TO WS-EDIT-AGE
080322              ELSE
080322                  MOVE CM-INSURED-ISSUE-AGE TO WS-EDIT-AGE
080322              END-IF
080322              COMPUTE WS-ATT-AGE = WS-EDIT-AGE
080322                 + ((CM-LF-ORIG-TERM - CP-REMAINING-TERM-3) / 12)
                    PERFORM VARYING P1 FROM +1 BY +1 UNTIL
080322                 (P1 > +11)
                       OR (PD-PROD-CODE (P1) = 'A'
080322                   AND PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
                    END-PERFORM
080322              IF P1 < +12
                       MOVE PD-MAX-AMT (P1) TO CP-R-MAX-MON-BEN
                    END-IF
                 END-IF
              END-IF
              IF CP-R-MAX-MON-BEN = ZEROS
                 MULTIPLY CM-AH-BENEFIT-AMT BY CP-REMAINING-TERM-3
01075                                     GIVING EREMBENO
              ELSE
                 MULTIPLY CP-R-MAX-MON-BEN BY CP-REMAINING-TERM-3
01075                                     GIVING EREMBENO
              END-IF
           END-IF
01076
01077 ******************************************************************
01078 *                                                                *
01079 *            CALCULATE UNEARNED PREMIUM AMOUNT                   *
01080 *                      ONE MONTH EARNED PREMIUM AMOUNT           *
01081 *                                                                *
01082 ******************************************************************
01083
01084      MOVE '2'                    TO  CP-PROCESS-TYPE.
01085      MOVE CM-STATE               TO  CP-STATE.
01086      MOVE CM-RATE-CLASS          TO  CP-CLASS-CODE.
01087      MOVE CM-AH-DEV-CODE         TO  CP-DEVIATION-CODE.
01088      MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
01089      MOVE CM-AH-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
01090                                      CP-RATING-BENEFIT-AMT.
01091      MOVE CM-AH-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
01092      MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
01093      MOVE CM-LOAN-TERM           TO  CP-LOAN-TERM.
01094
01095      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01096      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.
01097      MOVE SPACES                 TO  WS-CFK-ACCESS.
01098      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
01099      PERFORM 3500-READ-CNTL THRU 3500-EXIT.
01100      MOVE CF-AH-OVERRIDE-L1      TO  CP-AH-OVERRIDE-CODE.
01101      MOVE CF-CR-R78-METHOD       TO  CP-R78-OPTION.
01102      MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.
01103      MOVE CF-CR-REM-TERM-CALC    TO  CP-REM-TERM-METHOD.
01104
01105      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.
01106      MOVE PI-AH-OVERRIDE-L1      TO  WS-BENEFIT-TYPE.
01107      MOVE CM-AH-BENEFIT-CD       TO  WS-BENEFIT-NO.
01108      MOVE SPACES                 TO  WS-CFK-ACCESS.
01109      MOVE CM-STATE               TO  WS-CFK-STATE.
01110      MOVE +0                     TO  WS-CFK-SEQUENCE-NO.
01111      PERFORM 3500-READ-CNTL THRU 3500-EXIT.
01112      MOVE CF-STATE-ABBREVIATION  TO  CP-STATE-STD-ABBRV.
01113      MOVE CF-ST-FREE-LOOK-PERIOD TO  CP-FREE-LOOK.
01114
01115      IF CP-STATE-STD-ABBRV = 'OR'
01116          COMPUTE CP-RATING-BENEFIT-AMT = CM-AH-BENEFIT-AMT *
01117                                          CM-AH-ORIG-TERM.
01118
01119 *    IF CF-TRUNCATED-LIFE (WS-INDEX)
01120 *        MOVE CM-PAY-FREQUENCY   TO  CP-PAY-FREQUENCY
01121 *        MOVE CF-CO-OVRD-EARNINGS-CALC (WS-INDEX) TO
01122 *                                           CP-EARNING-METHOD.
01123
01124      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
01125
01126      IF WS-VALUATION-DT IS EQUAL TO LOW-VALUES
01127          MOVE PI-CR-MONTH-END-DT TO  CP-VALUATION-DT
01128                                      WS-VALUATION-DT
01129      ELSE
01130          MOVE WS-VALUATION-DT    TO  CP-VALUATION-DT.
01131
01132      PERFORM 9800-LINK-REM-TERM.
01133      MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM.
01134      PERFORM 9850-LINK-REFUND-RTN THRU 9850-EXIT.
01135      MOVE CP-CALC-REFUND         TO  EUEPREMO
01136                                      WS-CALC-REFUND.
01137      ADD +1                      TO  CP-REMAINING-TERM.
01138      IF CP-REMAINING-TERM IS EQUAL TO CM-AH-ORIG-TERM
01139          COMPUTE ONE-MON-EARNED = CM-AH-PREMIUM-AMT -
01140                                                 CP-CALC-REFUND
01141      ELSE
01142          PERFORM 9850-LINK-REFUND-RTN THRU 9850-EXIT
01143          COMPUTE ONE-MON-EARNED = WS-CALC-REFUND -
01144                                                 CP-CALC-REFUND.
01145
01146      MOVE ONE-MON-EARNED         TO  EOMEARNO.
01147
01148      IF WS-VALUATION-DT IS NOT = LOW-VALUES AND SPACES
01149          MOVE SPACES                 TO  DC-OPTION-CODE
01150          MOVE WS-VALUATION-DT        TO  DC-BIN-DATE-1
01151          PERFORM 8500-DATE-CONVERSION
01152          MOVE AL-UANON               TO  EUEDTA
01153          MOVE DC-GREG-DATE-1-EDIT    TO  EUEDTO.
01154
01155 ******************************************************************
01156 *                                                                *
01157 *                     DISPLAY CANCEL DATE                        *
01158 *                             CANCEL EXIT DATE                   *
01159 *                             CANCEL EXIT STATUS                 *
01160 *                                                                *
01161 ******************************************************************
01162
01163      MOVE SPACES                       TO  EEXITDTO.
01164
01165      IF CM-AH-CANCEL-EXIT-DT IS NOT = LOW-VALUES AND SPACES
01166          MOVE SPACES                   TO  DC-OPTION-CODE
01167          MOVE CM-AH-CANCEL-EXIT-DT     TO  DC-BIN-DATE-1
01168          PERFORM 8500-DATE-CONVERSION
01169          IF NOT DATE-CONVERSION-ERROR
01170              MOVE DC-GREG-DATE-1-EDIT  TO  EEXITDTO
01171          ELSE
01172              MOVE SPACES               TO  EEXITDTO.
01173
01174      MOVE SPACES                       TO  ECANDTO.
01175
01176      IF CM-AH-CANCEL-DT IS NOT = LOW-VALUES AND SPACES
01177          MOVE SPACES                   TO  DC-OPTION-CODE
01178          MOVE CM-AH-CANCEL-DT          TO  DC-BIN-DATE-1
01179          PERFORM 8500-DATE-CONVERSION
01180          IF NOT DATE-CONVERSION-ERROR
01181              MOVE DC-GREG-DATE-1-EDIT  TO  ECANDTO
01182          ELSE
01183              MOVE SPACES               TO  ECANDTO.
01184
01185      MOVE SPACES                       TO  EEXITSTO.
01186
01187      IF CM-AH-STATUS-AT-CANCEL = ' '
01188          MOVE SPACES             TO  EEXITSTO.
01189      IF CM-AH-STATUS-AT-CANCEL EQUAL '1' OR '2' OR '4' OR '9'
01190          MOVE 'ACTIVE'           TO  EEXITSTO.
01191      IF CM-AH-STATUS-AT-CANCEL = '6'
01192          MOVE 'PREV SETTLE'      TO  EEXITSTO.
01193      IF CM-AH-STATUS-AT-CANCEL = '7'
01194          MOVE 'PREV DTH'         TO  EEXITSTO.
01195      IF CM-AH-STATUS-AT-CANCEL = '8'
01196          MOVE 'PREV CANCEL'      TO  EEXITSTO.
01197
01198 ******************************************************************
01199 *                                                                *
01200 *                 DISPLAY CLAIM SETTLEMENT EXIT DATE             *
01201 *                         CLAIM SETTLEMENT STATUS                *
01202 *                                                                *
01203 ******************************************************************
01204
01205      MOVE SPACES                            TO  EEXDATEO.
01206
01207      IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES AND SPACE
01208         MOVE SPACES                    TO  DC-OPTION-CODE
01209         MOVE CM-AH-SETTLEMENT-EXIT-DT  TO  DC-BIN-DATE-1
01210         PERFORM 8500-DATE-CONVERSION
01211         IF NOT DATE-CONVERSION-ERROR
01212            MOVE DC-GREG-DATE-1-EDIT    TO  EEXDATEO
01213         ELSE
01214            MOVE SPACES                 TO  EEXDATEO.
01215
01216      MOVE SPACES                 TO  EEXSTATO.
01217
01218      IF CM-AH-STATUS-AT-SETTLEMENT = ' '
01219          MOVE SPACES             TO  EEXSTATO.
01220      IF CM-AH-STATUS-AT-SETTLEMENT EQUAL '1' OR '2' OR '4' OR '9'
01221          MOVE 'ACTIVE'           TO  EEXSTATO.
01222      IF CM-AH-STATUS-AT-SETTLEMENT = '6'
01223          MOVE 'PREV SETTLE'      TO  EEXSTATO.
01224      IF CM-AH-STATUS-AT-SETTLEMENT = '7'
01225          MOVE 'PREV DTH'         TO  EEXSTATO.
01226      IF CM-AH-STATUS-AT-SETTLEMENT = '8'
01227          MOVE 'PREV CAN'         TO  EEXSTATO.
092118     if crtt-not-read
092118        move cm-control-primary  to ws-elcrtt-key
092118        move 'C'                 to ws-cs-rec-type
092118        
      * exec cics read
092118*          dataset   ('ELCRTT')
092118*          ridfld    (ws-elcrtt-key)
092118*          into      (certificate-trailers)
092118*          resp      (ws-response)
092118*       end-exec
           MOVE LENGTH OF
            certificate-trailers
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00006193' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036313933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-trailers, 
                 DFHEIV11, 
                 ws-elcrtt-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
092118        if resp-normal
092118           set good-hit-on-trlr  to true
092118        else
092118           set crtt-not-found    to true
092118        end-if
092118     end-if
092118     if good-hit-on-trlr
092118        evaluate true
092118           when cs-ah-refund-method = '1'
092118              move 'R78'         to refmetho
092118           when cs-ah-refund-method = '2'
092118              move 'PRO'         to refmetho
092118           when cs-ah-refund-method = '3'
092118              move 'CALIF'       to refmetho
092118           when cs-ah-refund-method = '4'
092118              move 'FARM'        to refmetho
092118           when cs-ah-refund-method = '5'
092118              move 'NET'         to refmetho
092118           when cs-ah-refund-method = '6'
092118              move 'ANTIC'       to refmetho
092118           when cs-ah-refund-method = '8'
092118              move 'MEAN'        to refmetho
092118           when cs-ah-refund-method = '9'
092118              move 'SUMDI'       to refmetho
092118           when cs-ah-refund-method = 'G'
092118              move 'GAPNR'       to refmetho
092118           when cs-ah-refund-method = 'S'
092118              move 'GAPA'        to refmetho
092118           when cs-ah-refund-method = 'D'
092118              move 'DDF'         to refmetho
092118           when cs-ah-refund-method = 'I'
092118              move 'DDFIU'       to refmetho
092118           when cs-ah-refund-method = 'R'
092118              move 'REPO'        to refmetho
092118           when other
092118              move cs-ah-refund-method
092118                                 to refmetho
092118        end-evaluate
092118     end-if
01228
01229 ******************************************************************
01230 *                                                                *
01231 *                   DISPLAY EXTENSION DAYS                       *
01232 *                                                                *
01233 ******************************************************************
01234
01235      IF CM-PMT-EXTENSION-DAYS NUMERIC
01236          MOVE CM-PMT-EXTENSION-DAYS  TO  EEXTDAYO
01237      ELSE
01238          MOVE ZEROS                  TO  EEXTDAYO.
01239
01240 ******************************************************************
01241 *                                                                *
01242 *                   DISPLAY CEDED BENEFIT                        *
01243 *                                                                *
01244 ******************************************************************
01245
121802*    IF PI-COMPANY-ID EQUAL 'CVL' OR 'LGX'
121802*        IF CM-AH-CEDED-BENEFIT NUMERIC
121802*            MOVE CM-AH-CEDED-BENEFIT
121802*                                TO  ECEDBENO
121802*             ELSE
121802*            MOVE ZEROS          TO  ECEDBENO
121802*    ELSE
01253          MOVE AL-SADOF           TO  ECEDBENA, ECEDHDA.
01254
01255      GO TO 8100-SEND-INITIAL-MAP.
01256
01257      EJECT
       2500-GET-ERACCT.
           MOVE CM-CONTROL-PRIMARY (1:22)
                                       TO WS-ACCT-KEY
           MOVE ' '                    TO WS-ACCT-RECORD-SW
           
      * EXEC CICS STARTBR
      *        DATASET  ('ERACCT')
      *        RIDFLD   (WS-ACCT-KEY)
      *        GTEQ
      *        RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006272' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303036323732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              GO TO 2500-EXIT
           END-IF
           .
       2500-READ-ERACCT-NEXT.
           
      * EXEC CICS READNEXT
      *        DATASET  ('ERACCT')
      *        INTO     (ACCOUNT-MASTER)
      *        RIDFLD   (WS-ACCT-KEY)
      *        RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00006283' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303036323833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 WS-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              GO TO 2500-EXIT
           END-IF
           IF AM-CONTROL-PRIMARY (1:20) = CM-CONTROL-PRIMARY (1:20)
              IF (CM-CERT-EFF-DT < AM-EXPIRATION-DT)
                 AND (CM-CERT-EFF-DT >= AM-EFFECTIVE-DT)
                 SET ACCT-FOUND        TO TRUE
              ELSE
                 GO TO 2500-READ-ERACCT-NEXT
           END-IF
           .
       2500-EXIT.
           EXIT.
       2600-GET-MAX-BENEFIT.
           MOVE ' '                    TO WS-PDEF-RECORD-SW
           MOVE PI-COMPANY-CD          TO ERPDEF-KEY
           MOVE CM-STATE               TO ERPDEF-STATE
010816     if cm-clp-state not = cm-state and spaces and zeros
010816        move cm-clp-state        to erpdef-state
010816     end-if
           MOVE AM-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
           MOVE 'A'                    TO ERPDEF-BEN-TYPE
           MOVE CM-AH-BENEFIT-CD       TO ERPDEF-BEN-CODE
           MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
           MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
           
      * EXEC CICS STARTBR
      *        DATASET  ('ERPDEF')
      *        RIDFLD   (ERPDEF-KEY)
      *        GTEQ
      *        RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006314' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303036333134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              GO TO 2600-EXIT
           END-IF
           .
       2600-READNEXT.
           
      * EXEC CICS READNEXT
      *       DATASET  ('ERPDEF')
      *       SET      (ADDRESS OF PRODUCT-MASTER)
      *       RIDFLD   (ERPDEF-KEY)
      *       RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00006325' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303036333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              GO TO 2600-ENDBR
           END-IF
           IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
              IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
                 MOVE 'Y'              TO WS-PDEF-RECORD-SW
              ELSE
                 GO TO 2600-READNEXT
              END-IF
           ELSE
              GO TO 2600-ENDBR
           END-IF
           .
       2600-ENDBR.
           
      * EXEC CICS ENDBR
      *       DATASET  ('ERPDEF')
      *    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006345' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       2600-EXIT.
           EXIT.
01258  3000-LOCATE-BENEFIT         SECTION.
01259
01260      
      * EXEC CICS HANDLE CONDITION
01261 *            NOTFND  (3000-EXIT)
01262 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00006353' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303036333533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01263
01264      MOVE SPACES                 TO  WS-KIND
01265                                      WS-CFK-ACCESS.
01266      MOVE ZERO                   TO  WS-NOT-FOUND.
01267      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01268      MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT-NO.
01269
01270      
      * EXEC CICS READ
01271 *        DATASET  (WS-CONTROL-FILE-DSID)
01272 *        RIDFLD   (WS-CONTROL-FILE-KEY)
01273 *        SET      (ADDRESS OF CONTROL-FILE)
01274 *        GTEQ
01275 *    END-EXEC.
      *    MOVE '&"S        G          (   #00006363' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01276
01277      IF WS-CFK-COMPANY-ID NOT = CF-COMPANY-ID OR
01278          WS-CFK-RECORD-TYPE NOT = CF-RECORD-TYPE
01279              GO TO 3000-EXIT.
01280
01281      MOVE +1                     TO  WS-INDEX.
01282
01283  3000-LOOKUP-BENEFIT.
01284
01285      IF WS-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)
01286          MOVE CF-BENEFIT-ALPHA (WS-INDEX)   TO  WS-KIND
01287          MOVE CF-SPECIAL-CALC-CD (WS-INDEX) TO  WS-CALC-CD
01288          MOVE CF-BENEFIT-DESCRIP (WS-INDEX) TO  WS-BENEFIT-DESCRIP
01289          MOVE CF-BENEFIT-COMMENT (WS-INDEX) TO  WS-COMMENT
01290          MOVE CF-BENEFIT-CODE    (WS-INDEX) TO  WS-BENEFIT-CODE
01291          MOVE CF-LF-COVERAGE-TYPE (WS-INDEX)         TO
01292                                              WS-LF-COVERAGE-TYPE
01293          MOVE CF-CO-EARNINGS-CALC (WS-INDEX)         TO
01294                                              WS-EARNINGS-CALC
01295          MOVE CF-CO-OVRD-EARNINGS-CALC (WS-INDEX)    TO
01296                                              WS-OVRD-EARNINGS-CALC
01297          MOVE +1                            TO  WS-NOT-FOUND
01298          GO TO 3000-EXIT.
01299
01300      IF CF-BENEFIT-CODE (WS-INDEX) NOT LESS CF-HI-BEN-IN-REC
01301          GO TO 3000-EXIT.
01302
01303      IF WS-INDEX LESS THAN +8
01304          ADD +1                  TO  WS-INDEX
01305          GO TO 3000-LOOKUP-BENEFIT.
01306
01307  3000-EXIT.
01308      EXIT.
01309
01310  3500-READ-CNTL                  SECTION.
01311
01312      
      * EXEC CICS HANDLE CONDITION
01313 *            NOTFND  (3500-NOTFND)
01314 *    END-EXEC.
      *    MOVE '"$I                   ! % #00006405' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303036343035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01315
01316      
      * EXEC CICS READ
01317 *        DATASET  (WS-CONTROL-FILE-DSID)
01318 *        RIDFLD   (WS-CONTROL-FILE-KEY)
01319 *        SET      (ADDRESS OF CONTROL-FILE)
01320 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006409' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01321
01322      IF WS-FREE-LOOK = 'Y'
01323          MOVE SPACE              TO WS-FREE-LOOK
01324          GO TO 3500-EXIT.
01325
01326      IF WS-CFK-RECORD-TYPE IS EQUAL TO '3'
01327          NEXT SENTENCE
01328      ELSE
01329          GO TO 3500-EXIT.
01330
01331      MOVE +1                     TO  WS-INDEX.
01332
01333  3500-FIND-ST-CALC-METHOD.
01334
01335      IF WS-BENEFIT-TYPE = CF-ST-BENEFIT-KIND (WS-INDEX)  AND
01336         WS-BENEFIT-NO = CF-ST-BENEFIT-CD (WS-INDEX)
01337          MOVE CF-ST-REM-TERM-CALC (WS-INDEX)    TO
01338                                      CP-REM-TERM-METHOD
01339          GO TO 3500-EXIT.
01340
01341      IF WS-INDEX LESS +50
01342          ADD +1                  TO  WS-INDEX
01343          GO TO 3500-FIND-ST-CALC-METHOD.
01344
01345  3500-NOTFND.
01346
01347      IF WS-FREE-LOOK = 'Y'
01348          MOVE SPACE              TO WS-FREE-LOOK
01349          MOVE ZERO               TO CF-ST-FREE-LOOK-PERIOD.
01350
01351  3500-EXIT.
01352      EXIT.
01353
01354      EJECT
01355  7500-DEEDIT.
01356      
      * EXEC CICS BIF
01357 *         DEEDIT
01358 *         FIELD  (DEEDIT-FIELD)
01359 *         LENGTH (15)
01360 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006449' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01361
01362  7500-EXIT.
01363      EXIT.
01364
01365  7600-CHECK-FOR-CLAIM            SECTION.
01366
01367      MOVE WS-CK-COMPANY-CD  TO  WS-CL-COMPANY-CD.
01368      MOVE WS-CK-CERT-NO     TO  WS-CL-CERT-NO.
01369
01370      
      * EXEC CICS HANDLE CONDITION
01371 *        NOTFND (7600-CONTINUE)
01372 *        ENDFILE (7600-CONTINUE)
01373 *    END-EXEC.
      *    MOVE '"$I''                  ! & #00006463' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303036343633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01374
01375      
      * EXEC CICS IGNORE CONDITION
01376 *        DUPKEY
01377 *    END-EXEC.
      *    MOVE '"*$                   !   #00006468' TO DFHEIV0
           MOVE X'222A24202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01378
01379      
      * EXEC CICS STARTBR
01380 *        DATASET   (WS-CLAIM-MASTER-DSID)
01381 *        RIDFLD    (WS-CLAIM-KEY)
01382 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006472' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01383
01384      MOVE 'Y'                    TO  WS-BROWSE-SW.
01385
01386  7600-NEXT-CLAIM.
01387
01388      
      * EXEC CICS READNEXT
01389 *        DATASET   (WS-CLAIM-MASTER-DSID)
01390 *        RIDFLD    (WS-CLAIM-KEY)
01391 *        SET       (ADDRESS OF CLAIM-MASTER)
01392 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006481' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01393
01394      IF WS-CK-COMPANY-CD NOT = WS-CL-COMPANY-CD  OR
01395         WS-CK-CERT-NO    NOT = WS-CL-CERT-NO
01396            GO TO 7600-CONTINUE.
01397
01398      IF CM-CARRIER     NOT = CL-CARRIER       OR
01399         CM-GROUPING    NOT = CL-CERT-GROUPING OR
01400         CM-STATE       NOT = CL-CERT-STATE    OR
01401         CM-ACCOUNT     NOT = CL-CERT-ACCOUNT  OR
01402         CM-CERT-EFF-DT NOT = CL-CERT-EFF-DT
01403            GO TO 7600-NEXT-CLAIM.
01404
01405      MOVE 'Y'                    TO  PI-CLAIM-SW.
01406
01407      IF CHECK-AH-CLAIM
121802        IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
052614         OR 'F'
080322         OR 'B' OR 'H'
01409            IF CLAIM-IS-OPEN
01410               MOVE 'OPEN'   TO ECLSTATO
01411            ELSE
01412               MOVE 'CLOSED' TO ECLSTATO
121802           END-IF
01413         ELSE
01414            GO TO 7600-NEXT-CLAIM
121802        END-IF
121802     END-IF
01415
01416      IF CHECK-LF-CLAIM
100518         IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
01418              IF CLAIM-IS-OPEN
01419                  MOVE 'OPEN'   TO ECLSTATO
01420              ELSE
01421                  MOVE 'CLOSED' TO ECLSTATO
01422          ELSE
01423              GO TO 7600-NEXT-CLAIM.
01424
01425  7600-CONTINUE.
01426
01427      IF WS-BROWSE-SW IS EQUAL TO 'Y'
01428          
      * EXEC CICS ENDBR
01429 *            DATASET   (WS-CLAIM-MASTER-DSID)
01430 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006526' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CLAIM-MASTER-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01431      ELSE
01432          MOVE 'NO CLAIM'         TO  ECLSTATO.
01433
01434  7600-EXIT.
01435      EXIT.
01436
01437      EJECT
01438  8100-SEND-INITIAL-MAP           SECTION.
01439
01440      MOVE SAVE-DATE              TO  EDATEO.
01441      MOVE EIBTIME                TO  WS-TIME-WORK.
01442      MOVE WS-TIME                TO  ETIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01443      MOVE EMI-MESSAGE-AREA (1)   TO  EERMSG1O.
01444      MOVE PI-MEMBER-CAPTION      TO  EMEMCAPO.
01445      MOVE -1                     TO  EPFKEYL.
01446
01447      IF PI-USES-PAID-TO
01448         MOVE 'PAID  TO  :'  TO EPTHRHDO.
01449
01450      IF CREDIT-SESSION
01451             AND
01452         PI-CLAIM-SW IS EQUAL TO 'Y'
01453          MOVE AL-SANON           TO  EPFKEY7A
01454      ELSE
01455          MOVE AL-SADOF           TO  EPFKEY7A.
01456
01457      
      * EXEC CICS SEND
01458 *        FROM   (EL127EI)
01459 *        MAPSET (WS-MAPSET-NAME)
01460 *        MAP    (WS-MAP-NAME)
01461 *        CURSOR
01462 *        ERASE
01463 *    END-EXEC.
           MOVE LENGTH OF
            EL127EI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006557' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL127EI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01464
01465      GO TO 9100-RETURN-TRAN.
01466
01467  8100-EXIT.
01468      EXIT.
01469
01470      EJECT
01471  8200-SEND-DATAONLY SECTION.
01472
01473      IF PI-1ST-TIME-SW = '1'
01474          GO TO 8100-SEND-INITIAL-MAP.
01475
01476      MOVE SAVE-DATE              TO  EDATEO.
01477      MOVE EIBTIME                TO  WS-TIME-WORK.
01478      MOVE WS-TIME                TO  ETIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01479      MOVE EMI-MESSAGE-AREA (1)   TO  EERMSG1O.
01480      MOVE PI-MEMBER-CAPTION      TO  EMEMCAPO.
01481      MOVE -1                     TO  EPFKEYL.
01482
01483      IF PI-USES-PAID-TO
01484         MOVE 'PAID  TO  :'  TO EPTHRHDO.
01485
01486      IF CREDIT-SESSION
01487             AND
01488         PI-CLAIM-SW IS EQUAL TO 'Y'
01489          MOVE AL-SANON           TO  EPFKEY7A
01490      ELSE
01491          MOVE AL-SADOF           TO  EPFKEY7A.
01492
01493      
      * EXEC CICS SEND DATAONLY
01494 *        FROM   (EL127EI)
01495 *        MAPSET (WS-MAPSET-NAME)
01496 *        MAP    (WS-MAP-NAME)
01497 *        CURSOR
01498 *    END-EXEC.
           MOVE LENGTH OF
            EL127EI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00006595' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL127EI, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01499
01500      GO TO 9100-RETURN-TRAN.
01501
01502  8200-EXIT.
01503      EXIT.
01504
01505      EJECT
01506  8300-SEND-TEXT SECTION.
01507
01508      
      * EXEC CICS SEND TEXT
01509 *        FROM   (LOGOFF-TEXT)
01510 *        LENGTH (LOGOFF-LENGTH)
01511 *        ERASE
01512 *        FREEKB
01513 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00006610' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363130' TO DFHEIV0(25:11)
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
           
01514
01515      
      * EXEC CICS RETURN
01516 *    END-EXEC.
      *    MOVE '.(                    ''   #00006617' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01517
01518  8300-EXIT.
01519      EXIT.
01520
01521      EJECT
01522  8500-DATE-CONVERSION SECTION.
01523
01524      
      * EXEC CICS LINK
01525 *        PROGRAM  (ELDATCV)
01526 *        COMMAREA (DATE-CONVERSION-DATA)
01527 *        LENGTH   (DC-COMM-LENGTH)
01528 *    END-EXEC.
      *    MOVE '."C                   (   #00006626' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01529
01530  8500-EXIT.
01531      EXIT.
01532
01533  8880-NOT-FOUND SECTION.
01534
01535      MOVE -1                     TO  EPFKEYL.
01536      MOVE ER-0142                TO  EMI-ERROR.
01537      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01538      GO TO 8100-SEND-INITIAL-MAP.
01539
01540  8880-EXIT.
01541      EXIT.
01542
01543  9000-RETURN-CICS SECTION.
01544
01545      MOVE EL005                  TO  WS-PROGRAM-ID.
01546      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01547      PERFORM 9300-XCTL.
01548
01549  9000-EXIT.
01550      EXIT.
01551
01552  9100-RETURN-TRAN SECTION.
01553
01554      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01555      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01556
01557      
      * EXEC CICS RETURN
01558 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01559 *        LENGTH   (PI-COMM-LENGTH)
01560 *        TRANSID  (WS-TRANS-ID)
01561 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00006659' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01562
01563  9100-EXIT.
01564      EXIT.
01565
01566  9300-XCTL SECTION.
01567
01568      MOVE DFHENTER               TO  EIBAID.
01569
01570      
      * EXEC CICS XCTL
01571 *        PROGRAM  (WS-PROGRAM-ID)
01572 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01573 *        LENGTH   (PI-COMM-LENGTH)
01574 *    END-EXEC.
      *    MOVE '.$C                   %   #00006672' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PROGRAM-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01575
01576  9300-EXIT.
01577      EXIT.
01578
01579      EJECT
01580  9400-CLEAR SECTION.
01581
01582      MOVE ' '                    TO PI-1ST-TIME-SW.
01583      MOVE PI-RETURN-TO-PROGRAM   TO  WS-PROGRAM-ID.
01584      GO TO 9300-XCTL.
01585
01586  9400-EXIT.
01587      EXIT.
01588
01589  9600-PGMIDERR SECTION.
01590
01591      
      * EXEC CICS HANDLE CONDITION
01592 *        PGMIDERR (8300-SEND-TEXT)
01593 *    END-EXEC.
      *    MOVE '"$L                   ! '' #00006693' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303036363933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01594
01595      MOVE WS-PROGRAM-ID          TO  PI-CALLING-PROGRAM.
01596
01597      MOVE EL005                  TO  WS-PROGRAM-ID
01598                                      LOGOFF-PGM.
01599      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01600      MOVE SPACES                 TO  PI-ENTRY-CD-1.
01601      GO TO 9300-XCTL.
01602
01603  9600-EXIT.
01604      EXIT.
01605
01606  9700-LINK-REM-AMT               SECTION.
01607
01608      
      * EXEC CICS LINK
01609 *        PROGRAM   (ELRAMT)
01610 *        COMMAREA  (CALCULATION-PASS-AREA)
01611 *        LENGTH    (CP-COMM-LENGTH)
01612 *    END-EXEC.
      *    MOVE '."C                   (   #00006710' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRAMT, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01613
01614  9700-EXIT.
01615      EXIT.
01616
01617  9800-LINK-REM-TERM              SECTION.
01618
01619      
      * EXEC CICS LINK
01620 *        PROGRAM   (ELRTRM)
01621 *        COMMAREA  (CALCULATION-PASS-AREA)
01622 *        LENGTH    (CP-COMM-LENGTH)
01623 *    END-EXEC.
      *    MOVE '."C                   (   #00006721' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRTRM, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01624
01625  9800-EXIT.
01626      EXIT.
01627
01628  9850-LINK-REFUND-RTN            SECTION.
01629
01630      
      * EXEC CICS LINK
01631 *        PROGRAM   (ELRFND)
01632 *        COMMAREA  (CALCULATION-PASS-AREA)
01633 *        LENGTH    (CP-COMM-LENGTH)
01634 *    END-EXEC.
      *    MOVE '."C                   (   #00006732' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRFND, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01635
01636      IF CP-RETURN-CODE NOT EQUAL ZEROS
01637          MOVE ZEROS TO CP-CALC-REFUND.
01638
01639  9850-EXIT.
01640      EXIT.
01641
01642
01643      EJECT
01644  9900-ERROR-FORMAT SECTION.
01645
01646      ADD +1                      TO  WS-ERROR-COUNT.
01647
01648      IF EMI-ERRORS-COMPLETE
01649          MOVE ZERO               TO  EMI-ERROR
01650          GO TO 9900-EXIT.
01651
01652      
      * EXEC CICS LINK
01653 *        PROGRAM  (EL001)
01654 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01655 *        LENGTH   (EMI-COMM-LENGTH)
01656 *    END-EXEC.
      *    MOVE '."C                   (   #00006754' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL001, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01657
01658      MOVE ZERO                   TO  EMI-ERROR.
01659
01660  9900-EXIT.
01661      EXIT.
01662
01663  9990-ERROR SECTION.
01664
01665      MOVE DFHEIBLK               TO  EMI-LINE1.
01666
01667      
      * EXEC CICS LINK
01668 *        PROGRAM  (EL004)
01669 *        COMMAREA (EMI-LINE1)
01670 *        LENGTH   (72)
01671 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00006769' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL004, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01672
01673      GO TO 8100-SEND-INITIAL-MAP.
01674
01675  9990-EXIT.
01676      EXIT.
01677
01678  9999-LAST-PARAGRAPH SECTION.
01679
01680      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1275' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1275' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ERROR,
                     0001-FIRST-TIME
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3500-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7600-CONTINUE,
                     7600-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1275' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
