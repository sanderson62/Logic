00001  IDENTIFICATION DIVISION.                                         03/20/90
00002                                                                   ELCTLCNV
00003  PROGRAM-ID.                 ELCTLCNV.                               LV003
00004 *                            VMOD=2.003.                             CL**3
00005                                                                   ELCTLCNV
00006  AUTHOR.        LOGIC, INC.                                       ELCTLCNV
00007                 DALLAS, TEXAS.                                    ELCTLCNV
00008                                                                   ELCTLCNV
00009  DATE-COMPILED.                                                   ELCTLCNV
00010                                                                   ELCTLCNV
00011  SECURITY.   *****************************************************ELCTLCNV
00012              *                                                   *ELCTLCNV
00012              *                                                   *ELCTLCNV
00013              *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ELCTLCNV
00014              *                                                   *ELCTLCNV
00015              *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ELCTLCNV
00016              *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ELCTLCNV
00017              *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ELCTLCNV
00018              *                                                   *ELCTLCNV
00019              *****************************************************ELCTLCNV
00020                                                                   ELCTLCNV
00021  REMARKS.                                                         ELCTLCNV
00022          CONVERT OLD USER RECORDS FOR ENHANCEMENT OF              ELCTLCNV
00023          INTERNAL SECURITY PURPOSES.                              ELCTLCNV
00024                                                                   ELCTLCNV
00025  EJECT                                                            ELCTLCNV
00026  ENVIRONMENT DIVISION.                                            ELCTLCNV
00027  CONFIGURATION SECTION.                                           ELCTLCNV
00028  INPUT-OUTPUT SECTION.                                            ELCTLCNV
00029  FILE-CONTROL.                                                    ELCTLCNV
00030                                                                   ELCTLCNV
00031      SELECT CONTROL-FILE-IN  ASSIGN TO INPUT-S-ELCNTLI.           ELCTLCNV
00032                                                                   ELCTLCNV
00033      SELECT CONTROL-FILE-OUT ASSIGN TO OUTPUT-S-ELCNTLO.          ELCTLCNV
00034                                                                   ELCTLCNV
00035  EJECT                                                            ELCTLCNV
00036  DATA DIVISION.                                                   ELCTLCNV
00037  FILE SECTION.                                                    ELCTLCNV
00038                                                                   ELCTLCNV
00039      EJECT                                                        ELCTLCNV
00040  FD  CONTROL-FILE-IN                                              ELCTLCNV
00041      LABEL RECORDS STANDARD                                          CL**2
00042      BLOCK CONTAINS 0 RECORDS
00043      RECORD CONTAINS 504 CHARACTERS.                                 CL**3
00044                                                                   ELCTLCNV
00045 ******************************************************************ELCTLCNV
00046 *                                                                *ELCTLCNV
00047 *                            ELCCNTL.                            *ELCTLCNV
00048 *                            VMOD=2.007                          *ELCTLCNV
00049 *                                                                *ELCTLCNV
00050 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *ELCTLCNV
00051 *                                                                *ELCTLCNV
00052 *   FILE TYPE = VSAM,KSDS                                        *ELCTLCNV
00053 *   RECORD SIZE = 504  RECFORM = FIXED                           *   CL**3
00054 *                                                                *ELCTLCNV
00055 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *ELCTLCNV
00056 *       ALTERNATE INDEX = NONE                                   *ELCTLCNV
00057 *                                                                *ELCTLCNV
00058 *   LOG = YES                                                    *ELCTLCNV
00059 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *ELCTLCNV
00060 ******************************************************************ELCTLCNV
00061  01  CONTROL-IN.                                                     CL**2
00062      12  CF-RECORD-ID                       PIC XX.               ELCTLCNV
00063          88  VALID-CF-ID                        VALUE 'CF'.       ELCTLCNV
00064                                                                   ELCTLCNV
00065      12  CF-CONTROL-PRIMARY.                                      ELCTLCNV
00066          16  CF-COMPANY-ID                  PIC XXX.              ELCTLCNV
00067          16  CF-RECORD-TYPE                 PIC X.                ELCTLCNV
00068              88  CF-COMPANY-MASTER              VALUE '1'.        ELCTLCNV
00069              88  CF-PROCESSOR-MASTER            VALUE '2'.        ELCTLCNV
00070              88  CF-STATE-MASTER                VALUE '3'.        ELCTLCNV
00071              88  CF-LF-BENEFIT-MASTER           VALUE '4'.        ELCTLCNV
00072              88  CF-AH-BENEFIT-MASTER           VALUE '5'.        ELCTLCNV
00073              88  CF-CARRIER-MASTER              VALUE '6'.        ELCTLCNV
00074              88  CF-MORTALITY-MASTER            VALUE '7'.        ELCTLCNV
00075              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.        ELCTLCNV
00076              88  CF-TERMINAL-MASTER             VALUE '9'.        ELCTLCNV
00077              88  CF-AH-EDIT-MASTER              VALUE 'A'.        ELCTLCNV
00078              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.        ELCTLCNV
00079              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.        ELCTLCNV
00080              88  CF-REMINDERS-MASTER            VALUE 'R'.        ELCTLCNV
00081          16  CF-ACCESS-CD-GENL              PIC X(4).             ELCTLCNV
00082          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL. ELCTLCNV
00083              20  CF-PROCESSOR               PIC X(4).             ELCTLCNV
00084          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.    ELCTLCNV
00085              20  CF-STATE-CODE              PIC XX.               ELCTLCNV
00086              20  FILLER                     PIC XX.               ELCTLCNV
00087          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.  ELCTLCNV
00088              20  FILLER                     PIC XX.               ELCTLCNV
00089              20  CF-HI-BEN-IN-REC           PIC XX.               ELCTLCNV
00090          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.  ELCTLCNV
00091              20  FILLER                     PIC XXX.              ELCTLCNV
00092              20  CF-CARRIER-CNTL            PIC X.                ELCTLCNV
00093          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.  ELCTLCNV
00094              20  FILLER                     PIC XX.               ELCTLCNV
00095              20  CF-HI-TYPE-IN-REC          PIC 99.               ELCTLCNV
00096          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.  ELCTLCNV
00097              20  FILLER                     PIC X.                ELCTLCNV
00098              20  CF-CUSTOM-REPORT-NO        PIC 999.              ELCTLCNV
00099          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.     ELCTLCNV
00100                                                                   ELCTLCNV
00101      12  CF-LAST-MAINT-DT                   PIC XX.               ELCTLCNV
00102      12  CF-LAST-MAINT-BY                   PIC X(4).             ELCTLCNV
00103      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.   ELCTLCNV
00104                                                                   ELCTLCNV
00105      12  CF-RECORD-BODY                     PIC X(482).           ELCTLCNV
00106                                                                   ELCTLCNV
00107 ****************************************************************  ELCTLCNV
00108 *             COMPANY MASTER RECORD                            *  ELCTLCNV
00109 ****************************************************************  ELCTLCNV
00110                                                                   ELCTLCNV
00111      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.        ELCTLCNV
00112          16  CF-COMPANY-ADDRESS.                                  ELCTLCNV
00113              20  CF-CL-MAIL-TO-NAME         PIC X(30).            ELCTLCNV
00114              20  CF-CL-IN-CARE-OF           PIC X(30).            ELCTLCNV
00115              20  CF-CL-ADDR-LINE-1          PIC X(30).            ELCTLCNV
00116              20  CF-CL-ADDR-LINE-2          PIC X(30).            ELCTLCNV
00117              20  CF-CL-CITY-STATE           PIC X(30).            ELCTLCNV
00118              20  CF-CL-ZIP-CODE             PIC 9(9)    COMP-3.   ELCTLCNV
00119              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.   ELCTLCNV
00120          16  CF-COMPANY-CD                  PIC X.                ELCTLCNV
00121          16  CF-COMPANY-PASSWORD            PIC X(8).             ELCTLCNV
00122          16  CF-SECURITY-OPTION             PIC X.                ELCTLCNV
00123              88  ALL-SECURITY                   VALUE '1'.        ELCTLCNV
00124              88  COMPANY-VERIFY                 VALUE '2'.        ELCTLCNV
00125              88  PROCESSOR-VERIFY               VALUE '3'.        ELCTLCNV
00126              88  NO-SECURITY                    VALUE '4'.        ELCTLCNV
00127              88  ALL-BUT-TERM                   VALUE '5'.        ELCTLCNV
00128          16  CF-CARRIER-CONTROL-LEVEL       PIC X.                ELCTLCNV
00129              88  USE-ACTUAL-CARRIER             VALUE SPACE.      ELCTLCNV
00130          16  CF-LGX-INTERFACE-CNTL          PIC X.                ELCTLCNV
00131              88  LGX-TIME-SHR-COMPANY           VALUE '1'.        ELCTLCNV
00132          16  CF-INFORCE-LOCATION            PIC X.                ELCTLCNV
00133              88  CERTS-ARE-ONLINE               VALUE '1'.        ELCTLCNV
00134              88  CERTS-ARE-OFFLINE              VALUE '2'.        ELCTLCNV
00135              88  NO-CERTS-AVAILABLE             VALUE '3'.        ELCTLCNV
00136          16  CF-CLAIM-ACCESS-CONTROL        PIC X.                ELCTLCNV
00137              88  CF-CLAIM-NO-UNIQUE             VALUE '1'.        ELCTLCNV
00138              88  CF-CARRIER-CLAIM-CNTL          VALUE '2'.        ELCTLCNV
00139          16  CF-CERT-ACCESS-CONTROL         PIC X.                ELCTLCNV
00140              88  CF-ST-ACCNT-CNTL               VALUE ' '.        ELCTLCNV
00141              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.        ELCTLCNV
00142              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.        ELCTLCNV
00143              88  CF-ACCNT-CNTL                  VALUE '3'.        ELCTLCNV
00144              88  CF-CARR-ACCNT-CNTL             VALUE '4'.        ELCTLCNV
00145                                                                   ELCTLCNV
00146          16  CF-FORMS-PRINTER-ID            PIC X(4).             ELCTLCNV
00147          16  CF-CHECK-PRINTER-ID            PIC X(4).             ELCTLCNV
00148                                                                   ELCTLCNV
00149          16  CF-LGX-CREDIT-USER             PIC X.                ELCTLCNV
00150              88  CO-IS-NOT-USER                 VALUE 'N'.           CL**2
00151              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.           CL**2
00152                                                                   ELCTLCNV
00153          16 CF-CREDIT-CALC-CODES.                                 ELCTLCNV
00154              20  CF-CR-REM-TERM-CALC PIC X.                       ELCTLCNV
00155                88  CR-EARN-AFTER-15TH           VALUE '1'.        ELCTLCNV
00156                88  CR-EARN-ON-HALF-MO           VALUE '2'.        ELCTLCNV
00157                88  CR-EARN-ON-1ST-DAY           VALUE '3'.        ELCTLCNV
00158                88  CR-EARN-ON-FULL-MO           VALUE '4'.        ELCTLCNV
00159                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.        ELCTLCNV
00160              20  CF-CR-R78-METHOD           PIC X.                ELCTLCNV
00161                88  USE-TERM-PLUS-ONE            VALUE SPACE.      ELCTLCNV
00162                88  DONT-USE-PLUS-ONE            VALUE '1'.        ELCTLCNV
00163                                                                   ELCTLCNV
00164          16  CF-CLAIM-CONTROL-COUNTS.                             ELCTLCNV
00165              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.     ELCTLCNV
00166                  88  CO-CLM-COUNT-RESET         VALUE +99999.     ELCTLCNV
00167                                                                   ELCTLCNV
00168              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.     ELCTLCNV
00169                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.    ELCTLCNV
00170                                                                   ELCTLCNV
00171              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.     ELCTLCNV
00172                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.   ELCTLCNV
00173                                                                   ELCTLCNV
00174              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.     ELCTLCNV
00175                  88  CO-QUE-COUNT-RESET         VALUE +9999999.   ELCTLCNV
00176                                                                   ELCTLCNV
00177          16  CF-CURRENT-MONTH-END           PIC XX.               ELCTLCNV
00178                                                                   ELCTLCNV
00179          16  CF-CO-CALC-QUOTE-TOLERANCE.                          ELCTLCNV
00180              20  CF-CO-TOL-CLAIM            PIC S9(3)V99  COMP-3. ELCTLCNV
00181              20  CF-CO-TOL-PREM             PIC S9(3)V99  COMP-3. ELCTLCNV
00182              20  CF-CO-TOL-REFUND           PIC S9(3)V99  COMP-3. ELCTLCNV
00183              20  CF-CO-CLAIM-REJECT-SW      PIC X.                ELCTLCNV
00184                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.      ELCTLCNV
00185                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.        ELCTLCNV
00186              20  CF-CO-PREM-REJECT-SW       PIC X.                ELCTLCNV
00187                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.      ELCTLCNV
00188                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.        ELCTLCNV
00189              20  CF-CO-REF-REJECT-SW        PIC X.                ELCTLCNV
00190                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.      ELCTLCNV
00191                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.        ELCTLCNV
00192                                                                   ELCTLCNV
00193          16  CF-CO-REPORTING-DT             PIC XX.               ELCTLCNV
00194          16  CF-CO-REPORTING-MONTH-DT       PIC XX.               ELCTLCNV
00195          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.                ELCTLCNV
00196            88  CF-CO-NOT-MONTH-END              VALUE SPACES.     ELCTLCNV
00197            88  CF-CO-MONTH-END                  VALUE '1'.        ELCTLCNV
00198                                                                   ELCTLCNV
00199          16  CF-LGX-CLAIM-USER              PIC X.                ELCTLCNV
00200              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.           CL**2
00201              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.           CL**2
00202                                                                   ELCTLCNV
00203          16  CF-CREDIT-EDIT-CONTROLS.                             ELCTLCNV
00204              20  CF-MIN-PREMIUM             PIC S9(3)V99  COMP-3. ELCTLCNV
00205              20  CF-MIN-AGE                 PIC 99.               ELCTLCNV
00206              20  CF-DEFAULT-AGE             PIC 99.               ELCTLCNV
00207              20  CF-MIN-TERM                PIC S9(3)     COMP-3. ELCTLCNV
00208              20  CF-MAX-TERM                PIC S9(3)     COMP-3. ELCTLCNV
00209              20  CF-DEFAULT-SEX             PIC X.                ELCTLCNV
00210              20  CF-JOINT-AGE-INPUT         PIC X.                ELCTLCNV
00211                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.        ELCTLCNV
00212              20  CF-BIRTH-DATE-INPUT        PIC X.                ELCTLCNV
00213                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.        ELCTLCNV
00214              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.                ELCTLCNV
00215                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.        ELCTLCNV
00216                  88  CF-ZERO-CARRIER            VALUE '1'.        ELCTLCNV
00217                  88  CF-ZERO-GROUPING           VALUE '2'.        ELCTLCNV
00218                  88  CF-ZERO-CAR-GROUP          VALUE '3'.        ELCTLCNV
00219              20  CF-EDIT-SW                 PIC X.                ELCTLCNV
00220                  88  CF-START-EDIT-TONIGHT      VALUE '1'.        ELCTLCNV
00221              20  CF-EDIT-RESTART-BATCH      PIC X(6).             ELCTLCNV
00222              20  CF-CR-PR-METHOD            PIC X.                ELCTLCNV
00223                88  USE-NORMAL-PR-METHOD         VALUE SPACE.      ELCTLCNV
00224                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.        ELCTLCNV
00225              20  FILLER                     PIC X.                ELCTLCNV
00226                                                                   ELCTLCNV
00227          16  CF-CREDIT-MISC-CONTROLS.                             ELCTLCNV
00228              20  CF-REIN-TABLE-SW           PIC X.                ELCTLCNV
00229                  88 REIN-TABLES-ARE-USED        VALUE '1'.        ELCTLCNV
00230              20  CF-COMP-TABLE-SW           PIC X.                ELCTLCNV
00231                  88 COMP-TABLES-ARE-USED        VALUE '1'.        ELCTLCNV
00232              20  FILLER                     PIC X.                ELCTLCNV
00233              20  CF-CONVERSION-DT           PIC XX.               ELCTLCNV
00234              20  CF-COMP-WRITE-OFF-AMT      PIC S9(3)V99  COMP-3. ELCTLCNV
00235              20  CF-RUN-FREQUENCY-SW        PIC X.                ELCTLCNV
00236                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.      ELCTLCNV
00237                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.        ELCTLCNV
00238                                                                   ELCTLCNV
00239              20  CF-CR-CHECK-NO-CONTROL.                          ELCTLCNV
00240                  24  CF-CR-CHECK-NO-METHOD  PIC X.                ELCTLCNV
00241                      88  CR-CHECK-NO-MANUAL       VALUE '1'.      ELCTLCNV
00242                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.      ELCTLCNV
00243                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.      ELCTLCNV
00244                  24  CF-CR-CHECK-COUNTER    PIC S9(8)   COMP.     ELCTLCNV
00245                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.  ELCTLCNV
00246                                                                   ELCTLCNV
00247                  24  CF-CR-CHECK-COUNT       REDEFINES            ELCTLCNV
00248                      CF-CR-CHECK-COUNTER    PIC X(4).             ELCTLCNV
00249                                                                   ELCTLCNV
00250                  24  CF-CR-CHECK-QUE-COUNTER PIC S9(8)  COMP.     ELCTLCNV
00251                      88  CR-QUE-COUNT-RESET      VALUE +9999999.  ELCTLCNV
00252                                                                   ELCTLCNV
00253                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES            ELCTLCNV
00254                      CF-CR-CHECK-QUE-COUNTER PIC X(4).            ELCTLCNV
00255                  24  CF-MAIL-PROCESSING      PIC X.               ELCTLCNV
00256                      88  MAIL-PROCESSING     VALUE 'Y'.           ELCTLCNV
00257                                                                   ELCTLCNV
00258          16  CF-MISC-SYSTEM-CONTROL.                              ELCTLCNV
00259              20  CF-SYSTEM-C                 PIC X.               ELCTLCNV
00260                  88  CONFIRMATION-SYS-USED       VALUE '1'.       ELCTLCNV
00261              20  CF-SYSTEM-D                 PIC X.               ELCTLCNV
00262                  88  DAILY-BILL-SYS-USED         VALUE '1'.       ELCTLCNV
00263              20  CF-SOC-SEC-NO-SW            PIC X.               ELCTLCNV
00264                  88  SOC-SEC-NO-USED             VALUE '1'.       ELCTLCNV
00265              20  CF-MEMBER-NO-SW             PIC X.               ELCTLCNV
00266                  88  MEMBER-NO-USED              VALUE '1'.       ELCTLCNV
00267              20  CF-TAX-ID-NUMBER            PIC X(11).           ELCTLCNV
00268              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.      ELCTLCNV
00269              20  CF-PAYMENT-APPROVAL-SW      PIC X.               ELCTLCNV
00270                  88  CF-PMT-APPROVAL-USED        VALUE 'Y'.       ELCTLCNV
00271              20  CF-SYSTEM-E                 PIC X.               ELCTLCNV
00272                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.       ELCTLCNV
00273                                                                   ELCTLCNV
00274          16  CF-LGX-LIFE-USER               PIC X.                ELCTLCNV
00275              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.           CL**2
00276              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.           CL**2
00277                                                                   ELCTLCNV
00278          16  CF-CR-MONTH-END-DT             PIC XX.               ELCTLCNV
00279                                                                   ELCTLCNV
00280          16  CF-FILE-MAINT-DATES.                                 ELCTLCNV
00281              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.     ELCTLCNV
00282                  88  CF-LAST-BATCH-RESET        VALUE +999999.    ELCTLCNV
00283              20  CF-LAST-BATCH       REDEFINES                    ELCTLCNV
00284                  CF-LAST-BATCH-NO               PIC X(4).         ELCTLCNV
00285              20  CF-RATES-FILE-MAINT-DT         PIC XX.           ELCTLCNV
00286              20  CF-RATES-FILE-CREATE-DT        PIC XX.           ELCTLCNV
00287              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.           ELCTLCNV
00288              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.           ELCTLCNV
00289              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.           ELCTLCNV
00290              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.           ELCTLCNV
00291              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.           ELCTLCNV
00292              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.           ELCTLCNV
00293              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.           ELCTLCNV
00294              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.           ELCTLCNV
00295                                                                   ELCTLCNV
00296                                                                   ELCTLCNV
00297          16  CF-NEXT-COMPANY-ID             PIC X(3).             ELCTLCNV
00298          16  CF-LGX-GL-USER                 PIC X.                ELCTLCNV
00299              88  CO-IS-NOT-GL-USER              VALUE 'N'.           CL**2
00300              88  CO-HAS-CLAS-IC-GL              VALUE 'Y'.           CL**2
00301                                                                   ELCTLCNV
00302          16  CF-ALT-MORT-CODE               PIC X(4).             ELCTLCNV
00303          16  CF-MEMBER-CAPTION              PIC X(10).            ELCTLCNV
00304                                                                   ELCTLCNV
00305          16  CF-LIFE-ACCESS-CONTROL         PIC X.                ELCTLCNV
00306              88  CF-LIFE-ST-ACCNT-CNTL               VALUE ' '.   ELCTLCNV
00307              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL      VALUE '1'.   ELCTLCNV
00308              88  CF-LIFE-CARR-ST-ACCNT-CNTL          VALUE '2'.   ELCTLCNV
00309              88  CF-LIFE-ACCNT-CNTL                  VALUE '3'.   ELCTLCNV
00310              88  CF-LIFE-CARR-ACCNT-CNTL             VALUE '4'.   ELCTLCNV
00311                                                                   ELCTLCNV
00312          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.       ELCTLCNV
00313                                                                   ELCTLCNV
00314          16  CF-LIFE-OVERRIDE-L1            PIC X.                ELCTLCNV
00315          16  CF-LIFE-OVERRIDE-L2            PIC XX.               ELCTLCNV
00316          16  CF-LIFE-OVERRIDE-L6            PIC X(6).             ELCTLCNV
00317          16  CF-LIFE-OVERRIDE-L12           PIC X(12).            ELCTLCNV
00318                                                                   ELCTLCNV
00319          16  CF-AH-OVERRIDE-L1              PIC X.                ELCTLCNV
00320          16  CF-AH-OVERRIDE-L2              PIC XX.               ELCTLCNV
00321          16  CF-AH-OVERRIDE-L6              PIC X(6).             ELCTLCNV
00322          16  CF-AH-OVERRIDE-L12             PIC X(12).            ELCTLCNV
00323                                                                   ELCTLCNV
00324          16  CF-REPORT-CD1-CAPTION          PIC X(10).            ELCTLCNV
00325          16  CF-REPORT-CD2-CAPTION          PIC X(10).            ELCTLCNV
00326                                                                   ELCTLCNV
00327          16  CF-CLAIM-CUTOFF-DATE           PIC X(02).            ELCTLCNV
00328                                                                   ELCTLCNV
00329          16  CF-AR-LAST-INVOICE-NO          PIC S9(8) COMP.       ELCTLCNV
00330              88  CF-LAST-INVOICE-RESET          VALUE +999999.    ELCTLCNV
00331          16  CF-AR-LAST-INVOICE  REDEFINES                        ELCTLCNV
00332              CF-AR-LAST-INVOICE-NO          PIC X(4).             ELCTLCNV
00333                                                                   ELCTLCNV
00334          16  CF-MAX-NUM-PMTS-CHECK          PIC 9(02).            ELCTLCNV
00335          16  FILLER                         PIC X(77).            ELCTLCNV
00336                                                                   ELCTLCNV
00337                                                                   ELCTLCNV
00338 ****************************************************************  ELCTLCNV
00339 *             PROCESSOR/USER RECORD                            *  ELCTLCNV
00340 ****************************************************************  ELCTLCNV
00341                                                                   ELCTLCNV
00342      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.      ELCTLCNV
00343          16  CF-PROCESSOR-NAME              PIC X(30).            ELCTLCNV
00344          16  CF-PROCESSOR-TITLE             PIC X(26).            ELCTLCNV
00345          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.                ELCTLCNV
00346                  88  MESSAGE-YES                VALUE 'Y'.        ELCTLCNV
00347                  88  MESSAGE-NO                 VALUE ' ' 'N'.    ELCTLCNV
00348          16  CF-PROCESSOR-CREDIT.                                 ELCTLCNV
00349              20  CF-CREDIT-CONTROLS         PIC XX.               ELCTLCNV
00350              20  CF-CREDIT-FORCE            PIC X.                ELCTLCNV
00351          16  CF-CREDIT-CODES.                                     ELCTLCNV
00352              20  CF-CREDIT-AUTHORIZATION    PIC XX  OCCURS 40.    ELCTLCNV
00353          16  CF-PROCESSOR-CLAIMS.                                 ELCTLCNV
00354              20  CF-CLAIMS-CONTROLS         PIC XX.               ELCTLCNV
00355              20  CF-CLAIMS-FORCE            PIC X.                ELCTLCNV
00356          16  CF-CLAIMS-CODES.                                     ELCTLCNV
00357              20  CF-CLAIMS-AUTHORIZATION    PIC XX  OCCURS 30.    ELCTLCNV
00358                                                                   ELCTLCNV
00359          16  CF-CURRENT-TERM-ON             PIC X(4).             ELCTLCNV
00360                                                                   ELCTLCNV
00361          16  CF-PROCESSOR-LIMITS-CLAIMS.                          ELCTLCNV
00362              20  CF-PROC-CALC-AMT-TOL       PIC S9(3)V99  COMP-3. ELCTLCNV
00363              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3. ELCTLCNV
00364              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3. ELCTLCNV
00365              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3. ELCTLCNV
00366              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3. ELCTLCNV
00367              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3. ELCTLCNV
00368              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3. ELCTLCNV
00369                                                                   ELCTLCNV
00370          16  CF-PROCESSOR-PASSWORD          PIC X(11).            ELCTLCNV
00371                                                                   ELCTLCNV
00372          16  CF-PROCESSOR-SYS-ACCESS        PIC X.                ELCTLCNV
00373              88  ACCESS-TO-ALL-SYSTEMS          VALUE ' '.        ELCTLCNV
00374              88  ACCESS-TO-CLAIM-ONLY           VALUE '1'.        ELCTLCNV
00375              88  ACCESS-TO-CREDIT-ONLY          VALUE '2'.        ELCTLCNV
00376              88  ACCESS-TO-GL-ONLY              VALUE '3'.        ELCTLCNV
00377              88  ACCESS-TO-CLAIM-CREDIT         VALUE '4'.        ELCTLCNV
00378              88  ACCESS-TO-CLAIM-GL             VALUE '5'.        ELCTLCNV
00379              88  ACCESS-TO-CREDIT-GL            VALUE '6'.        ELCTLCNV
00380                                                                   ELCTLCNV
00381          16  CF-PROCESSOR-CARRIER           PIC X.                ELCTLCNV
00382              88  NO-CARRIER-SECURITY            VALUE ' '.        ELCTLCNV
00383          16  CF-PROCESSOR-ACCOUNT           PIC X(10).            ELCTLCNV
00384              88  NO-ACCOUNT-SECURITY            VALUE SPACES.     ELCTLCNV
00385                                                                   ELCTLCNV
00386          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.                ELCTLCNV
00387              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.        ELCTLCNV
00388          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.                ELCTLCNV
00389              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.        ELCTLCNV
00390          16  FILLER                         PIC X(226).           ELCTLCNV
00391                                                                   ELCTLCNV
00392 ****************************************************************  ELCTLCNV
00393 *             PROCESSOR/REMINDERS RECORD                       *  ELCTLCNV
00394 ****************************************************************  ELCTLCNV
00395                                                                   ELCTLCNV
00396      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.    ELCTLCNV
00397          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.              ELCTLCNV
00398              20  CF-START-REMIND-DT         PIC XX.               ELCTLCNV
00399              20  CF-END-REMIND-DT           PIC XX.               ELCTLCNV
00400              20  CF-REMINDER-TEXT           PIC X(50).            ELCTLCNV
00401          16  FILLER                         PIC X(50).            ELCTLCNV
00402                                                                   ELCTLCNV
00403                                                                   ELCTLCNV
00404 ****************************************************************  ELCTLCNV
00405 *             STATE MASTER RECORD                              *  ELCTLCNV
00406 ****************************************************************  ELCTLCNV
00407                                                                   ELCTLCNV
00408      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.          ELCTLCNV
00409          16  CF-STATE-ABBREVIATION          PIC XX.               ELCTLCNV
00410          16  CF-STATE-NAME                  PIC X(28).            ELCTLCNV
00411          16  CF-ST-CALC-QUOTE-TOLERANCE.                          ELCTLCNV
00412              20  CF-ST-TOL-CLAIM            PIC S9(3)V99  COMP-3. ELCTLCNV
00413              20  CF-ST-TOL-PREM             PIC S9(3)V99  COMP-3. ELCTLCNV
00414              20  CF-ST-TOL-REFUND           PIC S9(3)V99  COMP-3. ELCTLCNV
00415              20  CF-ST-CLAIM-REJECT-SW      PIC X.                ELCTLCNV
00416                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.      ELCTLCNV
00417                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.        ELCTLCNV
00418              20  CF-ST-PREM-REJECT-SW       PIC X.                ELCTLCNV
00419                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.      ELCTLCNV
00420                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.        ELCTLCNV
00421              20  CF-ST-REF-REJECT-SW        PIC X.                ELCTLCNV
00422                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.      ELCTLCNV
00423                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.        ELCTLCNV
00424          16  CF-ST-LF-EXP-PCT               PIC S9(3)V9(4) COMP-3.ELCTLCNV
00425          16  CF-ST-AH-EXP-PCT               PIC S9(3)V9(4) COMP-3.ELCTLCNV
00426          16  CF-ST-REFUND-RULES.                                  ELCTLCNV
00427              20  CF-ST-REFUND-MIN           PIC S9(3)V99   COMP-3.ELCTLCNV
00428              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.               ELCTLCNV
00429              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.               ELCTLCNV
00430          16  CF-ST-FST-PMT-EXTENSION.                             ELCTLCNV
00431              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.              ELCTLCNV
00432              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.                ELCTLCNV
00433          16  CF-ST-STATE-CALL.                                    ELCTLCNV
00434              20  CF-ST-CALL-UNEARNED        PIC X.                ELCTLCNV
00435              20  CF-ST-CALL-RPT-CNTL        PIC X.                ELCTLCNV
00436              20  CF-ST-CALL-RATE-DEV        PIC XXX.              ELCTLCNV
00437          16  FILLER                         PIC X(16).            ELCTLCNV
00438          16  CF-STATE-BENEFIT-CNTL  OCCURS 20 TIMES.              ELCTLCNV
00439              20  CF-ST-BENEFIT-CD           PIC XX.               ELCTLCNV
00440              20  CF-ST-BENEFIT-KIND         PIC X.                ELCTLCNV
00441                  88  CF-ST-LIFE-KIND            VALUE 'L'.        ELCTLCNV
00442                  88  CF-ST-AH-KIND              VALUE 'A'.        ELCTLCNV
00443              20  FILLER                     PIC X(12).            ELCTLCNV
00444              20  CF-ST-REM-TERM-CALC        PIC X.                ELCTLCNV
00445                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.      ELCTLCNV
00446                  88  ST-EARN-AFTER-15TH         VALUE '1'.        ELCTLCNV
00447                  88  ST-EARN-ON-HALF-MO         VALUE '2'.        ELCTLCNV
00448                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.        ELCTLCNV
00449                  88  ST-EARN-ON-FULL-MO         VALUE '4'.        ELCTLCNV
00450                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.        ELCTLCNV
00451                                                                   ELCTLCNV
00452              20  CF-ST-REFUND-CALC          PIC X.                ELCTLCNV
00453                  88  ST-REFUND-NOT-USED         VALUE SPACE.      ELCTLCNV
00454                  88  ST-REFD-BY-R78             VALUE '1'.        ELCTLCNV
00455                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.        ELCTLCNV
00456                  88  ST-REFD-AS-CALIF           VALUE '3'.        ELCTLCNV
00457                  88  ST-REFD-AS-TEXAS           VALUE '4'.        ELCTLCNV
00458                  88  ST-REFD-IS-NET-PAY         VALUE '5'.        ELCTLCNV
00459                  88  ST-REFD-ANTICIPATION       VALUE '6'.        ELCTLCNV
00460                  88  ST-REFD-UTAH               VALUE '7'.        ELCTLCNV
00461                  88  ST-REFD-REG-BALLOON        VALUE 'B'.        ELCTLCNV
00462                                                                   ELCTLCNV
00463              20  CF-ST-EARNING-CALC         PIC X.                ELCTLCNV
00464                  88  ST-EARNING-NOT-USED        VALUE SPACE.      ELCTLCNV
00465                  88  ST-EARN-BY-R78             VALUE '1'.        ELCTLCNV
00466                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.        ELCTLCNV
00467                  88  ST-EARN-AS-CALIF           VALUE '3'.        ELCTLCNV
00468                  88  ST-EARN-AS-TEXAS           VALUE '4'.        ELCTLCNV
00469                  88  ST-EARN-IS-NET-PAY         VALUE '5'.        ELCTLCNV
00470                  88  ST-EARN-ANTICIPATION       VALUE '6'.        ELCTLCNV
00471                  88  ST-EARN-MEAN               VALUE '8'.        ELCTLCNV
00472                  88  ST-EARN-REG-BALLOON        VALUE 'B'.        ELCTLCNV
00473                                                                   ELCTLCNV
00474              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.                ELCTLCNV
00475                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.      ELCTLCNV
00476                  88  ST-OVRD-BY-R78             VALUE '1'.        ELCTLCNV
00477                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.        ELCTLCNV
00478                  88  ST-OVRD-AS-CALIF           VALUE '3'.        ELCTLCNV
00479                  88  ST-OVRD-AS-TEXAS           VALUE '4'.        ELCTLCNV
00480                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.        ELCTLCNV
00481                  88  ST-OVRD-ANTICIPATION       VALUE '6'.        ELCTLCNV
00482                  88  ST-OVRD-MEAN               VALUE '8'.        ELCTLCNV
00483                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.        ELCTLCNV
00484              20  FILLER                     PIC X.                ELCTLCNV
00485                                                                   ELCTLCNV
00486                                                                   ELCTLCNV
00487 ****************************************************************  ELCTLCNV
00488 *             BENEFIT MASTER RECORD                            *  ELCTLCNV
00489 ****************************************************************  ELCTLCNV
00490                                                                   ELCTLCNV
00491      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.        ELCTLCNV
00492          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.                 ELCTLCNV
00493              20  CF-BENEFIT-CODE            PIC XX.               ELCTLCNV
00494              20  CF-BENEFIT-NUMERIC  REDEFINES                    ELCTLCNV
00495                  CF-BENEFIT-CODE            PIC XX.               ELCTLCNV
00496              20  CF-BENEFIT-ALPHA           PIC X(3).             ELCTLCNV
00497              20  CF-BENEFIT-DESCRIP         PIC X(10).            ELCTLCNV
00498              20  CF-BENEFIT-COMMENT         PIC X(10).            ELCTLCNV
00499                                                                   ELCTLCNV
00500              20  CF-LF-COVERAGE-TYPE        PIC X.                ELCTLCNV
00501                  88  CF-REDUCING                VALUE 'R'.        ELCTLCNV
00502                  88  CF-LEVEL                   VALUE 'L' 'P'.       CL**2
00503                                                                   ELCTLCNV
00504              20  CF-SPECIAL-CALC-CD         PIC X.                ELCTLCNV
00505                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.        ELCTLCNV
00506                  88  CF-NP-0-MO-INT             VALUE 'A'.        ELCTLCNV
00507                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.        ELCTLCNV
00508                  88  CF-CRITICAL-PERIOD         VALUE 'C'.        ELCTLCNV
00509                  88  CF-TERM-IN-DAYS            VALUE 'D'.        ELCTLCNV
00510                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.        ELCTLCNV
00511                  88  CF-FARM-PLAN               VALUE 'F'.        ELCTLCNV
00512                  88  CF-RATE-AS-STANDARD        VALUE 'G'.        ELCTLCNV
00513                  88  CF-2-MTH-INTEREST          VALUE 'I'.        ELCTLCNV
00514                  88  CF-3-MTH-INTEREST          VALUE 'J'.        ELCTLCNV
00515                  88  CF-4-MTH-INTEREST          VALUE 'K'.        ELCTLCNV
00516                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.        ELCTLCNV
00517                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.        ELCTLCNV
00518                  88  CF-PRUDENTIAL              VALUE 'P'.        ELCTLCNV
00519                  88  CF-OUTSTANDING-BAL         VALUE 'O'.        ELCTLCNV
00520                  88  CF-TRUNCATED-LIFE          VALUE 'T'.        ELCTLCNV
00521                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.        ELCTLCNV
00522                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.        ELCTLCNV
00523                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.        ELCTLCNV
00524                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.        ELCTLCNV
00525                                                                   ELCTLCNV
00526              20  CF-JOINT-INDICATOR         PIC X.                ELCTLCNV
00527                  88  CF-JOINT-COVERAGE          VALUE 'J'.        ELCTLCNV
00528                                                                   ELCTLCNV
00529              20  FILLER                     PIC X(12).            ELCTLCNV
00530              20  CF-LOAN-TYPE               PIC X(8).             ELCTLCNV
00531                                                                   ELCTLCNV
00532              20  CF-CO-REM-TERM-CALC        PIC X.                ELCTLCNV
00533                  88  CO-EARN-AFTER-15TH         VALUE '1'.        ELCTLCNV
00534                  88  CO-EARN-ON-HALF-MO         VALUE '2'.        ELCTLCNV
00535                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.        ELCTLCNV
00536                  88  CO-EARN-ON-FULL-MO         VALUE '4'.        ELCTLCNV
00537                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.        ELCTLCNV
00538                                                                   ELCTLCNV
00539              20  CF-CO-EARNINGS-CALC        PIC X.                ELCTLCNV
00540                  88  CO-EARN-BY-R78             VALUE '1'.        ELCTLCNV
00541                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.        ELCTLCNV
00542                  88  CO-EARN-AS-CALIF           VALUE '3'.        ELCTLCNV
00543                  88  CO-EARN-AS-TEXAS           VALUE '4'.        ELCTLCNV
00544                  88  CO-EARN-IS-NET-PAY         VALUE '5'.        ELCTLCNV
00545                  88  CO-EARN-ANTICIPATION       VALUE '6'.        ELCTLCNV
00546                  88  CO-EARN-AS-MEAN            VALUE '8'.        ELCTLCNV
00547                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.        ELCTLCNV
00548                                                                   ELCTLCNV
00549              20  CF-CO-REFUND-CALC          PIC X.                ELCTLCNV
00550                  88  CO-REFUND-NOT-USED         VALUE SPACE.      ELCTLCNV
00551                  88  CO-REFD-BY-R78             VALUE '1'.        ELCTLCNV
00552                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.        ELCTLCNV
00553                  88  CO-REFD-AS-CALIF           VALUE '3'.        ELCTLCNV
00554                  88  CO-REFD-AS-TEXAS           VALUE '4'.        ELCTLCNV
00555                  88  CO-REFD-IS-NET-PAY         VALUE '5'.        ELCTLCNV
00556                  88  CO-REFD-ANTICIPATION       VALUE '6'.        ELCTLCNV
00557                  88  CO-REFD-MEAN               VALUE '8'.        ELCTLCNV
00558                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.        ELCTLCNV
00559                                                                   ELCTLCNV
00560              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.                ELCTLCNV
00561                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.      ELCTLCNV
00562                  88  CO-OVRD-BY-R78             VALUE '1'.        ELCTLCNV
00563                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.        ELCTLCNV
00564                  88  CO-OVRD-AS-CALIF           VALUE '3'.        ELCTLCNV
00565                  88  CO-OVRD-AS-TEXAS           VALUE '4'.        ELCTLCNV
00566                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.        ELCTLCNV
00567                  88  CO-OVRD-ANTICIPATION       VALUE '6'.        ELCTLCNV
00568                  88  CO-OVRD-MEAN               VALUE '8'.        ELCTLCNV
00569                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.        ELCTLCNV
00570                                                                   ELCTLCNV
00571              20  CF-CO-BEN-I-G-CD           PIC X.                ELCTLCNV
00572                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.      ELCTLCNV
00573                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.        ELCTLCNV
00574                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.        ELCTLCNV
00575                                                                   ELCTLCNV
00576          16  FILLER                         PIC X(58).            ELCTLCNV
00577                                                                   ELCTLCNV
00578                                                                   ELCTLCNV
00579 ****************************************************************  ELCTLCNV
00580 *             CARRIER MASTER RECORD                            *  ELCTLCNV
00581 ****************************************************************  ELCTLCNV
00582                                                                   ELCTLCNV
00583      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.        ELCTLCNV
00584          16  CF-ADDRESS-DATA.                                     ELCTLCNV
00585              20  CF-MAIL-TO-NAME            PIC X(30).            ELCTLCNV
00586              20  CF-IN-CARE-OF              PIC X(30).            ELCTLCNV
00587              20  CF-ADDRESS-LINE-1          PIC X(30).            ELCTLCNV
00588              20  CF-ADDRESS-LINE-2          PIC X(30).            ELCTLCNV
00589              20  CF-CITY-STATE              PIC X(30).            ELCTLCNV
00590              20  CF-ZIP-CODE                PIC 9(9)      COMP-3. ELCTLCNV
00591              20  CF-PHONE-NO                PIC 9(11)     COMP-3. ELCTLCNV
00592          16  CF-CLAIM-NO-CONTROL.                                 ELCTLCNV
00593              20  CF-CLAIM-NO-METHOD         PIC X.                ELCTLCNV
00594                  88  CLAIM-NO-MANUAL            VALUE '1'.        ELCTLCNV
00595                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.        ELCTLCNV
00596                  88  CLAIM-NO-SEQ               VALUE '3'.        ELCTLCNV
00597              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.     ELCTLCNV
00598                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.   ELCTLCNV
00599                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.     ELCTLCNV
00600          16  CF-CHECK-NO-CONTROL.                                 ELCTLCNV
00601              20  CF-CHECK-NO-METHOD         PIC X.                ELCTLCNV
00602                  88  CHECK-NO-MANUAL            VALUE '1'.        ELCTLCNV
00603                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.        ELCTLCNV
00604                  88  CHECK-NO-CARR-SEQ          VALUE '3'.        ELCTLCNV
00605                  88  CHECK-NO-AT-PRINT          VALUE '4'.        ELCTLCNV
00606              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.     ELCTLCNV
00607                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.    ELCTLCNV
00608          16  CF-DOMICILE-STATE              PIC X(2).             ELCTLCNV
00609          16  CF-EXPENSE-CONTROLS.                                 ELCTLCNV
00610              20  CF-EXPENSE-METHOD          PIC X.                ELCTLCNV
00611                  88  EXPENSE-CALC-MANUAL        VALUE '1'.        ELCTLCNV
00612                  88  DOLLARS-PER-PMT            VALUE '2'.        ELCTLCNV
00613                  88  PERCENT-OF-PAYMENT         VALUE '3'.        ELCTLCNV
00614                  88  DOLLARS-PER-MONTH          VALUE '4'.        ELCTLCNV
00615              20  CF-EXPENSE-PERCENT         PIC S9(3)V99  COMP-3. ELCTLCNV
00616              20  CF-EXPENSE-DOLLAR          PIC S9(3)V99  COMP-3. ELCTLCNV
00617          16  CF-CORRESPONDENCE-CONTROL.                           ELCTLCNV
00618              20  CF-LETTER-RESEND-OPT       PIC X.                ELCTLCNV
00619                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.      ELCTLCNV
00620                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.        ELCTLCNV
00621 *        20  FILLER                         PIC X(4).             ELCTLCNV
00622              20  FILLER                     PIC X(4).             ELCTLCNV
00623          16  CF-RESERVE-CONTROLS.                                 ELCTLCNV
00624              20  CF-MANUAL-SW               PIC X.                ELCTLCNV
00625                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.        ELCTLCNV
00626              20  CF-FUTURE-SW               PIC X.                ELCTLCNV
00627                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.        ELCTLCNV
00628              20  CF-PTC-SW                  PIC X.                ELCTLCNV
00629                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.        ELCTLCNV
00630              20  CF-IBNR-SW                 PIC X.                ELCTLCNV
00631                  88  CF-IBNR-RESERVES-USED      VALUE '1'.        ELCTLCNV
00632              20  CF-PTC-LF-SW               PIC X.                ELCTLCNV
00633                  88  CF-LF-PTC-USED             VALUE '1'.        ELCTLCNV
00634              20  CF-CDT-ACCESS-METHOD       PIC X.                ELCTLCNV
00635                  88  CF-CDT-ROUND-NEAR          VALUE '1'.        ELCTLCNV
00636                  88  CF-CDT-ROUND-HIGH          VALUE '2'.        ELCTLCNV
00637                  88  CF-CDT-INTERPOLATED        VALUE '3'.        ELCTLCNV
00638              20  CF-PERCENT-OF-CDT          PIC S9(3)V99  COMP-3. ELCTLCNV
00639          16  CF-CLAIM-CALC-METHOD           PIC X.                ELCTLCNV
00640              88  360-PLUS-MONTHS                VALUE '1'.        ELCTLCNV
00641              88  365-PLUS-MONTHS                VALUE '2'.        ELCTLCNV
00642              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.        ELCTLCNV
00643              88  360-DAILY                      VALUE '4'.        ELCTLCNV
00644              88  365-DAILY                      VALUE '5'.        ELCTLCNV
00645          16  FILLER                         PIC X(12).            ELCTLCNV
00646          16  CF-LIMIT-AMOUNTS.                                    ELCTLCNV
00647              20  CF-CALC-AMT-TOL            PIC S9(3)V99  COMP-3. ELCTLCNV
00648              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3. ELCTLCNV
00649              20  CF-MAX-REG-DAYS            PIC S999      COMP-3. ELCTLCNV
00650              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3. ELCTLCNV
00651              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3. ELCTLCNV
00652              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3. ELCTLCNV
00653          16  FILLER                         PIC X(12).            ELCTLCNV
00654          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3. ELCTLCNV
00655          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3. ELCTLCNV
00656          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3. ELCTLCNV
00657          16  FILLER                         PIC X(237).           ELCTLCNV
00658                                                                   ELCTLCNV
00659                                                                   ELCTLCNV
00660 ****************************************************************  ELCTLCNV
00661 *             MORTALITY MASTER RECORD                          *  ELCTLCNV
00662 ****************************************************************  ELCTLCNV
00663                                                                   ELCTLCNV
00664      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.       ELCTLCNV
00665          16  CF-TABLE-DESCRIP   OCCURS  12  TIMES.                ELCTLCNV
00666              20  CF-MORT-TABLE-CODE         PIC X(4).             ELCTLCNV
00667              20  CF-JOINT-MORT-FACTOR       PIC S99V9(4).         ELCTLCNV
00668              20  CF-TABLE-COMMENTS          PIC X(26).            ELCTLCNV
00669              20  CF-JOINT-CODE              PIC X.                ELCTLCNV
00670                                                                   ELCTLCNV
00671          16  FILLER                         PIC X(38).            ELCTLCNV
00672                                                                   ELCTLCNV
00673                                                                   ELCTLCNV
00674 ****************************************************************  ELCTLCNV
00675 *             BUSSINESS TYPE MASTER RECORD                     *  ELCTLCNV
00676 ****************************************************************  ELCTLCNV
00677                                                                   ELCTLCNV
00678      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.   ELCTLCNV
00679 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20                        ELCTLCNV
00680 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80       ELCTLCNV
00681 * AND RECORD 05 IS TYPES 81-99                                    ELCTLCNV
00682          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.            ELCTLCNV
00683              20  CF-BUSINESS-TITLE          PIC X(24).            ELCTLCNV
00684          16  FILLER                         PIC XX.               ELCTLCNV
00685                                                                   ELCTLCNV
00686                                                                   ELCTLCNV
00687                                                                   ELCTLCNV
00688 ****************************************************************  ELCTLCNV
00689 *             TERMINAL MASTER RECORD                           *  ELCTLCNV
00690 ****************************************************************  ELCTLCNV
00691                                                                   ELCTLCNV
00692      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.       ELCTLCNV
00693                                                                   ELCTLCNV
00694          16  CF-COMPANY-TERMINALS.                                ELCTLCNV
00695              20  CF-TERMINAL-ID  OCCURS 120 TIMES                 ELCTLCNV
00696                                   PIC X(4).                       ELCTLCNV
00697          16  FILLER               PIC XX.                         ELCTLCNV
00698                                                                   ELCTLCNV
00699                                                                   ELCTLCNV
00700 ****************************************************************  ELCTLCNV
00701 *             LIFE EDIT MASTER RECORD                          *  ELCTLCNV
00702 ****************************************************************  ELCTLCNV
00703                                                                   ELCTLCNV
00704      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.       ELCTLCNV
00705          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.            ELCTLCNV
00706              20  CF-LIFE-CODE-IN            PIC XX.               ELCTLCNV
00707              20  CF-LIFE-CODE-OUT           PIC XX.               ELCTLCNV
00708          16  FILLER                         PIC XX.               ELCTLCNV
00709                                                                   ELCTLCNV
00710                                                                   ELCTLCNV
00711                                                                   ELCTLCNV
00712 ****************************************************************  ELCTLCNV
00713 *             AH EDIT MASTER RECORD                            *  ELCTLCNV
00714 ****************************************************************  ELCTLCNV
00715                                                                   ELCTLCNV
00716      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.         ELCTLCNV
00717          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.              ELCTLCNV
00718              20  CF-AH-CODE-IN              PIC XXX.              ELCTLCNV
00719              20  CF-AH-CODE-OUT             PIC XX.               ELCTLCNV
00720          16  FILLER                         PIC XX.               ELCTLCNV
00721                                                                   ELCTLCNV
00722                                                                   ELCTLCNV
00723                                                                   ELCTLCNV
00724 ****************************************************************  ELCTLCNV
00725 *             REPORT CUSTOMIZATION RECORD                      *  ELCTLCNV
00726 ****************************************************************  ELCTLCNV
00727                                                                   ELCTLCNV
00728      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.         ELCTLCNV
00729          16  CF-REPORT-REC-STATUS           PIC X.                ELCTLCNV
00730              88  CF-REPORT-TO-BE-CUSTOM         VALUE 'A'.        ELCTLCNV
00731              88  CF-IGNORE-CUSTOM-OPTION        VALUE 'I'.        ELCTLCNV
00732                                                                   ELCTLCNV
00733          16  FILLER                         PIC XX.               ELCTLCNV
00734                                                                   ELCTLCNV
00735          16  CF-CARRIER-CNTL-OPT.                                 ELCTLCNV
00736              20  CF-CARRIER-OPT-SEQ         PIC 9.                ELCTLCNV
00737                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.   ELCTLCNV
00738                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.          ELCTLCNV
00739              20  CF-CARRIER-SELECT OCCURS 3 TIMES                 ELCTLCNV
00740                                             PIC X.                ELCTLCNV
00741          16  CF-GROUP-CNTL-OPT.                                   ELCTLCNV
00742              20  CF-GROUP-OPT-SEQ           PIC 9.                ELCTLCNV
00743                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.   ELCTLCNV
00744                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.          ELCTLCNV
00745              20  CF-GROUP-SELECT OCCURS 3 TIMES                   ELCTLCNV
00746                                             PIC X(6).             ELCTLCNV
00747          16  CF-STATE-CNTL-OPT.                                   ELCTLCNV
00748              20  CF-STATE-OPT-SEQ           PIC 9.                ELCTLCNV
00749                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.   ELCTLCNV
00750                  88  CF-STATE-OPT-NOT-USED      VALUE 0.          ELCTLCNV
00751              20  CF-STATE-SELECT OCCURS 3 TIMES                   ELCTLCNV
00752                                             PIC XX.               ELCTLCNV
00753          16  CF-ACCOUNT-CNTL-OPT.                                 ELCTLCNV
00754              20  CF-ACCOUNT-OPT-SEQ         PIC 9.                ELCTLCNV
00755                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.   ELCTLCNV
00756                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.          ELCTLCNV
00757              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES                 ELCTLCNV
00758                                             PIC X(10).            ELCTLCNV
00759          16  CF-BUS-TYP-CNTL-OPT.                                 ELCTLCNV
00760              20  CF-BUS-TYP-OPT-SEQ         PIC 9.                ELCTLCNV
00761                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.   ELCTLCNV
00762                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.          ELCTLCNV
00763              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES                 ELCTLCNV
00764                                             PIC XX.               ELCTLCNV
00765          16  CF-LF-TYP-CNTL-OPT.                                  ELCTLCNV
00766              20  CF-LF-TYP-OPT-SEQ          PIC 9.                ELCTLCNV
00767                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.   ELCTLCNV
00768                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.          ELCTLCNV
00769              20  CF-BUS-LF-SELECT OCCURS 3 TIMES                  ELCTLCNV
00770                                             PIC XX.               ELCTLCNV
00771          16  CF-AH-TYP-CNTL-OPT.                                  ELCTLCNV
00772              20  CF-AH-TYP-OPT-SEQ          PIC 9.                ELCTLCNV
00773                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.   ELCTLCNV
00774                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.          ELCTLCNV
00775              20  CF-BUS-AH-SELECT OCCURS 3 TIMES                  ELCTLCNV
00776                                             PIC XX.               ELCTLCNV
00777          16  CF-REPTCD1-CNTL-OPT.                                 ELCTLCNV
00778              20  CF-REPTCD1-OPT-SEQ         PIC 9.                ELCTLCNV
00779                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.   ELCTLCNV
00780                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.          ELCTLCNV
00781              20  CF-REPTCD1-SELECT OCCURS 3 TIMES                 ELCTLCNV
00782                                             PIC X(10).            ELCTLCNV
00783          16  CF-REPTCD2-CNTL-OPT.                                 ELCTLCNV
00784              20  CF-REPTCD2-OPT-SEQ         PIC 9.                ELCTLCNV
00785                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.   ELCTLCNV
00786                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.          ELCTLCNV
00787              20  CF-REPTCD2-SELECT OCCURS 3 TIMES                 ELCTLCNV
00788                                             PIC X(10).            ELCTLCNV
00789          16  CF-USER1-CNTL-OPT.                                   ELCTLCNV
00790              20  CF-USER1-OPT-SEQ           PIC 9.                ELCTLCNV
00791                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.   ELCTLCNV
00792                  88  CF-USER1-OPT-NOT-USED      VALUE 0.          ELCTLCNV
00793              20  CF-USER1-SELECT OCCURS 3 TIMES                   ELCTLCNV
00794                                             PIC X(10).            ELCTLCNV
00795          16  CF-USER2-CNTL-OPT.                                   ELCTLCNV
00796              20  CF-USER2-OPT-SEQ           PIC 9.                ELCTLCNV
00797                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.   ELCTLCNV
00798                  88  CF-USER2-OPT-NOT-USED      VALUE 0.          ELCTLCNV
00799              20  CF-USER2-SELECT OCCURS 3 TIMES                   ELCTLCNV
00800                                             PIC X(10).            ELCTLCNV
00801          16  CF-USER3-CNTL-OPT.                                   ELCTLCNV
00802              20  CF-USER3-OPT-SEQ           PIC 9.                ELCTLCNV
00803                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.   ELCTLCNV
00804                  88  CF-USER3-OPT-NOT-USED      VALUE 0.          ELCTLCNV
00805              20  CF-USER3-SELECT OCCURS 3 TIMES                   ELCTLCNV
00806                                             PIC X(10).            ELCTLCNV
00807          16  CF-USER4-CNTL-OPT.                                   ELCTLCNV
00808              20  CF-USER4-OPT-SEQ           PIC 9.                ELCTLCNV
00809                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.   ELCTLCNV
00810                  88  CF-USER4-OPT-NOT-USED      VALUE 0.          ELCTLCNV
00811              20  CF-USER4-SELECT OCCURS 3 TIMES                   ELCTLCNV
00812                                             PIC X(10).            ELCTLCNV
00813          16  CF-USER5-CNTL-OPT.                                   ELCTLCNV
00814              20  CF-USER5-OPT-SEQ           PIC 9.                ELCTLCNV
00815                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.   ELCTLCNV
00816                  88  CF-USER5-OPT-NOT-USED      VALUE 0.          ELCTLCNV
00817              20  CF-USER5-SELECT OCCURS 3 TIMES                   ELCTLCNV
00818                                             PIC X(10).            ELCTLCNV
00819                                                                   ELCTLCNV
00820          16  FILLER                         PIC X(93).            ELCTLCNV
00821                                                                   ELCTLCNV
00822          16  CF-LOSS-RATIO-SELECT.                                ELCTLCNV
00823              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.  ELCTLCNV
00824              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.  ELCTLCNV
00825          16  CF-ENTRY-DATE-SELECT.                                ELCTLCNV
00826              20  CF-SEL-LO-ENTRY-DATE       PIC XX.               ELCTLCNV
00827              20  CF-SEL-HI-ENTRY-DATE       PIC XX.               ELCTLCNV
00828          16  CF-EFFECTIVE-DATE-SELECT.                            ELCTLCNV
00829              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.               ELCTLCNV
00830              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.               ELCTLCNV
00831                                                                   ELCTLCNV
00832          16  FILLER                         PIC X(73).            ELCTLCNV
00833                                                                   ELCTLCNV
00834 ******************************************************************ELCTLCNV
00835                                                                   ELCTLCNV
00836      EJECT                                                        ELCTLCNV
00837  FD  CONTROL-FILE-OUT                                             ELCTLCNV
00838      LABEL RECORDS STANDARD                                          CL**2
00839      BLOCK CONTAINS 0 RECORDS
00840      RECORD CONTAINS 750 CHARACTERS.                                 CL**3
00841                              COPY ELCCNTL.                           CL**3
00842  EJECT                                                            ELCTLCNV
00843  WORKING-STORAGE SECTION.                                         ELCTLCNV
00844  77  FILLER  PIC X(32)  VALUE '********************************'. ELCTLCNV
00845  77  FILLER  PIC X(32)  VALUE '*   ELCTLCNV WORKING-STORAGE    '. ELCTLCNV
00846  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.003 ************'.    CL**3
00847  77  CNTL-IN-CNT             PIC S9(7)   COMP-3  VALUE +0.        ELCTLCNV
00848  77  CNTL-OUT-CNT            PIC S9(7)   COMP-3  VALUE +0.        ELCTLCNV
00849  77  PROCESSOR-CNT           PIC S9(7)   COMP-3  VALUE +0.        ELCTLCNV
00850  77  SLOT                    PIC S9(4)   COMP    VALUE +0.        ELCTLCNV
00851  77  MAXSLOT                 PIC S9(4)   COMP    VALUE +44.       ELCTLCNV
00852  77  SYS                     PIC S9(4)   COMP    VALUE +0.        ELCTLCNV
00853  77  MAXSYS                  PIC S9(4)   COMP    VALUE +4.        ELCTLCNV
00854  77  ONE                     PIC S9(4)   COMP    VALUE +1.        ELCTLCNV
00855  77  TWO                     PIC S9(4)   COMP    VALUE +2.        ELCTLCNV
00856                                                                   ELCTLCNV
00857  01  WS.                                                          ELCTLCNV
00858      12  WS-RETURN-CODE        PIC S9(4)   COMP   VALUE +0.       ELCTLCNV
00859      12  WS-ABEND-MESSAGE      PIC X(80)          VALUE SPACES.   ELCTLCNV
00860      12  WS-ABEND-FILE-STATUS  PIC XX             VALUE ZEROS.    ELCTLCNV
00861      12  WS-ZERO               PIC S9      COMP-3 VALUE +0.       ELCTLCNV
00862                                                                   ELCTLCNV
00863      12  WS-ABEND-CODE         PIC 9(4).                          ELCTLCNV
00864      12  ABEND-CODE  REDEFINES  WS-ABEND-CODE.                    ELCTLCNV
00865          16  ABEND-CODE-1    PIC XX.                              ELCTLCNV
00866          16  ABEND-CODE-2    PIC XX.                              ELCTLCNV
00867                                                                   ELCTLCNV
00868      EJECT                                                        ELCTLCNV
00869  PROCEDURE DIVISION.                                              ELCTLCNV
00870                                                                   ELCTLCNV
00871 ******************************************************************ELCTLCNV
00872 ***           O P E N   F I L E S   R O U T I N E              ***ELCTLCNV
00873 ******************************************************************ELCTLCNV
00874                                                                   ELCTLCNV
00875  0080-OPEN-FILES.                                                 ELCTLCNV
00876                                                                   ELCTLCNV
00877      OPEN INPUT  CONTROL-FILE-IN                                  ELCTLCNV
00878           OUTPUT CONTROL-FILE-OUT.                                ELCTLCNV
00879                                                                   ELCTLCNV
00880 ******************************************************************ELCTLCNV
00881 ***       R E A D   C E R T   M A S T E R   R O U T I N E      ***ELCTLCNV
00882 ******************************************************************ELCTLCNV
00883                                                                   ELCTLCNV
00884  0200-CNTL-MASTER-READ-ROUTINE.                                   ELCTLCNV
00885                                                                   ELCTLCNV
00886      READ CONTROL-FILE-IN                                         ELCTLCNV
00887          AT END GO TO 1000-EXIT.                                  ELCTLCNV
00888                                                                   ELCTLCNV
00889      ADD +1                      TO CNTL-IN-CNT.                  ELCTLCNV
00890                                                                   ELCTLCNV
00891      IF CF-RECORD-TYPE OF CONTROL-IN NOT = '2'                       CL**2
00892          GO TO 0200-CNTL-MASTER-READ-ROUTINE.                     ELCTLCNV
00893                                                                   ELCTLCNV
00894      ADD +1                      TO PROCESSOR-CNT.                ELCTLCNV
00895                                                                   ELCTLCNV
00896      MOVE SPACES                 TO CONTROL-FILE.                    CL**2
00897                                                                   ELCTLCNV
00898      MOVE 'CF' TO CF-RECORD-ID   OF CONTROL-FILE.                    CL**2
00899                                                                   ELCTLCNV
00900      MOVE CF-CONTROL-PRIMARY     OF CONTROL-IN   TO                  CL**2
00901           CF-CONTROL-PRIMARY     OF CONTROL-FILE.                    CL**2
00902                                                                   ELCTLCNV
00903      MOVE CF-LAST-MAINT-DT       OF CONTROL-IN   TO                  CL**2
00904           CF-LAST-MAINT-DT       OF CONTROL-FILE.                    CL**2
00905                                                                   ELCTLCNV
00906      MOVE CF-LAST-MAINT-BY       OF CONTROL-IN   TO                  CL**2
00907           CF-LAST-MAINT-BY       OF CONTROL-FILE.                    CL**2
00908                                                                   ELCTLCNV
00909      IF CF-LAST-MAINT-HHMMSS OF CONTROL-IN NOT NUMERIC               CL**2
00910          MOVE ZEROS TO CF-LAST-MAINT-HHMMSS OF CONTROL-FILE.      ELCTLCNV
00911                                                                   ELCTLCNV
00912      MOVE CF-LAST-MAINT-HHMMSS   OF CONTROL-IN   TO                  CL**2
00913           CF-LAST-MAINT-HHMMSS   OF CONTROL-FILE.                    CL**2
00914                                                                   ELCTLCNV
00915      MOVE CF-PROCESSOR-NAME      OF CONTROL-IN   TO                  CL**2
00916           CF-PROCESSOR-NAME      OF CONTROL-FILE.                    CL**2
00917                                                                   ELCTLCNV
00918      MOVE CF-PROCESSOR-PASSWORD  OF CONTROL-IN   TO                  CL**2
00919           CF-PROCESSOR-PASSWORD  OF CONTROL-FILE.                    CL**2
00920                                                                   ELCTLCNV
00921      MOVE CF-PROCESSOR-TITLE     OF CONTROL-IN   TO                  CL**2
00922           CF-PROCESSOR-TITLE     OF CONTROL-FILE.                    CL**2
00923                                                                   ELCTLCNV
00924      MOVE CF-MESSAGE-AT-LOGON-CAP OF CONTROL-IN  TO                  CL**2
00925           CF-MESSAGE-AT-LOGON-CAP OF CONTROL-FILE.                   CL**2
00926                                                                   ELCTLCNV
00927      IF CF-PROCESSOR-SYS-ACCESS = SPACES                             CL**2
00928          MOVE 'YYYY'              TO CF-PROC-SYS-ACCESS-ALL.      ELCTLCNV
00929                                                                   ELCTLCNV
00930      IF CF-PROCESSOR-SYS-ACCESS = '1'                                CL**2
00931          MOVE 'NYNN'              TO CF-PROC-SYS-ACCESS-ALL.      ELCTLCNV
00932                                                                   ELCTLCNV
00933      IF CF-PROCESSOR-SYS-ACCESS = '2'                                CL**2
00934          MOVE 'YNNN'              TO CF-PROC-SYS-ACCESS-ALL.      ELCTLCNV
00935                                                                   ELCTLCNV
00936      IF CF-PROCESSOR-SYS-ACCESS = '3'                                CL**2
00937          MOVE 'NNNY'              TO CF-PROC-SYS-ACCESS-ALL.      ELCTLCNV
00938                                                                   ELCTLCNV
00939      IF CF-PROCESSOR-SYS-ACCESS = '4'                                CL**2
00940          MOVE 'YYNN'              TO CF-PROC-SYS-ACCESS-ALL.      ELCTLCNV
00941                                                                   ELCTLCNV
00942      IF CF-PROCESSOR-SYS-ACCESS = '5'                                CL**2
00943          MOVE 'NYNY'              TO CF-PROC-SYS-ACCESS-ALL.      ELCTLCNV
00944                                                                   ELCTLCNV
00945      IF CF-PROCESSOR-SYS-ACCESS = '6'                                CL**2
00946          MOVE 'YNNY'              TO CF-PROC-SYS-ACCESS-ALL.      ELCTLCNV
00947                                                                   ELCTLCNV
00948      MOVE ONE                    TO SYS                           ELCTLCNV
00949                                     SLOT.                         ELCTLCNV
00950                                                                   ELCTLCNV
00951  0300-INITIALIZE-SYSTEMS.                                         ELCTLCNV
00952      MOVE 'NN'                   TO                               ELCTLCNV
00953                                  CF-ADMINISTRATION-CONTROLS(SYS). ELCTLCNV
00954      MOVE 'N'                    TO CF-APPLICATION-FORCE(SYS).    ELCTLCNV
00955                                                                   ELCTLCNV
00956  0400-APP-INITIALIZE.                                             ELCTLCNV
00957      MOVE 'NN'                   TO CF-APP-SWITCHES(SYS SLOT).    ELCTLCNV
00958                                                                   ELCTLCNV
00959      IF SLOT LESS MAXSLOT                                            CL**2
00960          ADD ONE                 TO SLOT                          ELCTLCNV
00961          GO TO 0400-APP-INITIALIZE.                               ELCTLCNV
00962                                                                   ELCTLCNV
00963      IF SYS LESS MAXSYS                                              CL**2
00964          ADD ONE                 TO SYS                           ELCTLCNV
00965          MOVE ONE                TO SLOT                          ELCTLCNV
00966          GO TO 0300-INITIALIZE-SYSTEMS.                           ELCTLCNV
00967                                                                   ELCTLCNV
00968      MOVE CF-CURRENT-TERM-ON     OF CONTROL-IN          TO           CL**2
00969           CF-CURRENT-TERM-ON     OF CONTROL-FILE.                    CL**2
00970                                                                   ELCTLCNV
00971      MOVE CF-PROCESSOR-LIMITS-CLAIMS OF CONTROL-IN      TO           CL**2
00972           CF-PROCESSOR-LIMITS-CLAIMS OF CONTROL-FILE.                CL**2
00973                                                                   ELCTLCNV
00974      MOVE CF-PROCESSOR-CARRIER   OF CONTROL-IN          TO           CL**2
00975           CF-PROCESSOR-CARRIER   OF CONTROL-FILE.                    CL**2
00976                                                                   ELCTLCNV
00977      MOVE CF-PROCESSOR-ACCOUNT   OF CONTROL-IN          TO           CL**2
00978           CF-PROCESSOR-ACCOUNT   OF CONTROL-FILE.                    CL**2
00979                                                                   ELCTLCNV
00980      MOVE CF-PROCESSOR-LIFE-ACCESS OF CONTROL-IN        TO           CL**2
00981           CF-PROCESSOR-LIFE-ACCESS OF CONTROL-FILE.                  CL**2
00982                                                                   ELCTLCNV
00983      MOVE CF-PROCESSOR-USER-ALMIGHTY OF CONTROL-IN      TO           CL**2
00984           CF-PROCESSOR-USER-ALMIGHTY OF CONTROL-FILE.                CL**2
00985                                                                   ELCTLCNV
00986      MOVE ONE                    TO SYS.                          ELCTLCNV
00987      MOVE CF-CREDIT-CONTROLS     TO                               ELCTLCNV
00988                                  CF-ADMINISTRATION-CONTROLS(SYS). ELCTLCNV
00989      MOVE CF-CREDIT-FORCE        TO CF-APPLICATION-FORCE(SYS).    ELCTLCNV
00990                                                                   ELCTLCNV
00991      PERFORM 2000-UPDATE-CREDIT-APP THRU 2000-EXIT                ELCTLCNV
00992          VARYING SLOT FROM +1 BY +1                               ELCTLCNV
00993              UNTIL SLOT GREATER THAN +40.                         ELCTLCNV
00994                                                                   ELCTLCNV
00995      MOVE TWO                    TO SYS.                          ELCTLCNV
00996      MOVE CF-CLAIMS-CONTROLS     TO                               ELCTLCNV
00997                                  CF-ADMINISTRATION-CONTROLS(SYS). ELCTLCNV
00998      MOVE CF-CLAIMS-FORCE        TO CF-APPLICATION-FORCE(SYS).    ELCTLCNV
00999                                                                   ELCTLCNV
01000      PERFORM 3000-UPDATE-CLAIMS-APP THRU 3000-EXIT                ELCTLCNV
01001          VARYING SLOT FROM +1 BY +1                               ELCTLCNV
01002              UNTIL SLOT GREATER THAN +30.                         ELCTLCNV
01003                                                                   ELCTLCNV
01004  0500-WRITE-CONVERTED-RCD.                                        ELCTLCNV
01005      ADD +1                      TO CNTL-OUT-CNT.                 ELCTLCNV
01006                                                                   ELCTLCNV
01007      WRITE CONTROL-FILE.                                             CL**2
01008                                                                   ELCTLCNV
01009      GO TO 0200-CNTL-MASTER-READ-ROUTINE.                         ELCTLCNV
01010                                                                   ELCTLCNV
01011  1000-EXIT.                                                       ELCTLCNV
01012      EXIT.                                                        ELCTLCNV
01013                                                                   ELCTLCNV
01014      EJECT                                                        ELCTLCNV
01015  2000-UPDATE-CREDIT-APP.                                          ELCTLCNV
01016      IF CF-CREDIT-AUTHORIZATION(SLOT) = 'YY' OR 'YN'                 CL**2
01017          MOVE CF-CREDIT-AUTHORIZATION(SLOT)                       ELCTLCNV
01018                                  TO CF-APP-SWITCHES(SYS SLOT).    ELCTLCNV
01019                                                                   ELCTLCNV
01020  2000-EXIT.                                                       ELCTLCNV
01021      EXIT.                                                        ELCTLCNV
01022                                                                   ELCTLCNV
01023  3000-UPDATE-CLAIMS-APP.                                          ELCTLCNV
01024      IF CF-CLAIMS-AUTHORIZATION(SLOT) = 'YY' OR 'YN'                 CL**2
01025          MOVE CF-CLAIMS-AUTHORIZATION(SLOT)                       ELCTLCNV
01026                                  TO CF-APP-SWITCHES(SYS SLOT).    ELCTLCNV
01027                                                                   ELCTLCNV
01028  3000-EXIT.                                                       ELCTLCNV
01029      EXIT.                                                        ELCTLCNV
01030                                                                   ELCTLCNV
01031      EJECT                                                        ELCTLCNV
01032 ******************************************************************ELCTLCNV
01033 ***          E N D   O F   J O B   P R O C E S S I N G         ***ELCTLCNV
01034 ******************************************************************ELCTLCNV
01035                                                                   ELCTLCNV
01036  9910-EOJ-1.                                                      ELCTLCNV
01037                                                                   ELCTLCNV
01038  9990-FINAL-CLOSE.                                                ELCTLCNV
01039      DISPLAY '****   CONTROL RCDS READ = ' CNTL-IN-CNT.           ELCTLCNV
01040      DISPLAY '****PROCESSOR RCDS CNVRT = ' PROCESSOR-CNT.         ELCTLCNV
01041      DISPLAY '****CONTROL RCDS WRITTEN = ' CNTL-OUT-CNT.          ELCTLCNV
01042                                                                   ELCTLCNV
01043      CLOSE CONTROL-FILE-IN                                        ELCTLCNV
01044            CONTROL-FILE-OUT.                                      ELCTLCNV
01045                                                                   ELCTLCNV
01046      GO TO 9999-END-THE-JOB.                                      ELCTLCNV
01047                                                                   ELCTLCNV
01048      EJECT                                                        ELCTLCNV
01049  ABEND-PGM.                                                       ELCTLCNV
01050                                  COPY ELCABEND.                   ELCTLCNV
01051                                                                      CL**2
01052  9999-END-THE-JOB.                                                ELCTLCNV
01053      GOBACK.                                                      ELCTLCNV
