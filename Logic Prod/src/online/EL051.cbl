110921$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL051 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/13/95 13:23:45.
00007 *                            VMOD=2.013
00008 *
00008 *
00009 *AUTHOR.     LOGIC INC.
00010 *            DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022
00023 *REMARKS.    TRANSACTION - EXSE AND EXEB
00024 *         THIS PROGRAM IS STARTED  FROM EL630 AND CICS.  IT'S
00025 *         FUNCTION IS TO FEED PENDING BUSINESS RECORDS TO
00026 *         THE EDIT PROGRAM.
00027
110921******************************************************************
110921*                   C H A N G E   L O G
110921*
110921* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
110921*-----------------------------------------------------------------
110921*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
110921* EFFECTIVE    NUMBER
110921*-----------------------------------------------------------------
110921* 110921  CR2021051200001  PEMA  Onbase Workflow project
110921******************************************************************
00029  ENVIRONMENT DIVISION.
00030  DATA DIVISION.
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32) VALUE '********************************'.
00033  77  FILLER  PIC X(32) VALUE '*     EL051  WORKING-STORAGE   *'.
00034  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.013 *********'.
00035
00036  77  ELEN                        PIC S9(4)  COMP    VALUE +16.
110921 77  ws-sql-code                 pic s9(7) value zeros.
110921 77  ws-dis-sql-code             pic -9999999 value zeros.
110921
110921 01  P pointer.
110921 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
110921 01  var-ptr pointer.
110921 01  env-var-len                 pic 9(4)  binary.
110921
110921 01  WS-KIXSYS.
110921     05  WS-KIX-FIL1             PIC X(10).
110921     05  WS-KIX-APPS             PIC X(10).
110921     05  WS-KIX-ENV              PIC X(10).
110921     05  WS-KIX-MYENV            PIC X(10).
110921     05  WS-KIX-SYS              PIC X(10).
110921
110921 EXEC SQL
110921    INCLUDE SQLDA
110921 END-EXEC
110921
110921 EXEC SQL
110921    INCLUDE SQLCA
110921 END-EXEC
110921
110921 EXEC SQL
110921    BEGIN DECLARE SECTION
110921 END-EXEC
110921
110921 01  svr                         pic x(32).
110921 01  usr                         pic x(32).
110921 01  pass                        pic x(32).
110921 01  usr-pass                    pic x(64).
110921 01  ws-disp-code                pic s9(11).
110921
110921***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
110921***                                                            ***
110921***  These indicators are used to determine if a variable      ***
110921***  is passed nulls from sql. The indicator will be -1        ***
110921***  if the value on sql is nulls and +0 if the value is       ***
110921***  something other than nulls. Here is an example on how     ***
110921***  to use the indicator variables.                           ***
110921***                                                            ***
110921***     EXEC SQL                                               ***
110921***        fetch checkapp into                                 ***
110921***           :db-app-status :nu-app-status,                   ***
110921***           :db-app-by     :nu-app-by,                       ***
110921***           :db-app-date   :nu-app-date,                     ***
110921***           :db-app-batch  :nu-app-batch                     ***
110921***     END-EXEC                                               ***
110921***                                                            ***
110921***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
110921
110921 01  indicator-vaiables-for-nulls.
110921     05  nu-claim-no             pic s9(4) comp value +0.
110921     05  nu-app-by               pic s9(4) comp value +0.
110921     05  nu-exp-date             pic s9(4) comp value +0.
110921     05  nu-app-batch            pic s9(4) comp value +0.
110921
110921 EXEC SQL
110921    END DECLARE SECTION
110921 END-EXEC
00038  01  WORK-AREAS.
110921     12  ws-connect-sw               pic x  value ' '.
110921         88  connected-to-db             value 'Y'.
00039      12  ER-2617                 PIC 9(4)    VALUE 2617.
00040      12  WS-CSR-ID               PIC X(4)    VALUE SPACES.
00041      12  WS-PREV-BATCH           PIC X(6)    VALUE SPACES.
00042      12  LIT-2600                PIC 9(4)    VALUE 2600.
00043      12  LIT-2625                PIC 9(4)    VALUE 2625.
00044      12  LIT-2725                PIC 9(4)    VALUE 2725.
00045      12  LIT-2800                PIC 9(4)    VALUE 2800.
00046      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
00047      12  LINK-001                PIC X(8)    VALUE 'EL001'.
00048      12  ELREPT-FILE-ID          PIC X(8)    VALUE 'ELREPT'.
00049      12  ERPNDB-FILE-ID          PIC X(8)    VALUE 'ERPNDB'.
00050      12  ERPNDC-FILE-ID          PIC X(8)    VALUE 'ERPNDC'.
00051      12  ELCNTL-FILE-ID          PIC X(8)    VALUE 'ELCNTL'.
00052      12  CERT-ID                 PIC X(8)    VALUE 'ELCERT'.
00053      12  PNDB-EDIT-PGM           PIC X(8)    VALUE 'EL050'.
00054      12  PNDC-EDIT-PGM           PIC X(8)    VALUE 'EL053'.
00055      12  PGM-NAME                PIC X(8)    VALUE SPACES.
00056      12  THIS-PGM                PIC X(8)    VALUE 'EL051'.
00057      12  ERPNDB-LENGTH           PIC S9(4)   COMP VALUE +585.
00058      12  ERPNDC-LENGTH           PIC S9(4)   COMP VALUE +500.
00059      12  JOURNAL-LENGTH          PIC S9(4)   COMP VALUE +0.
00060      12  TRANS-DATA-LENGTH       PIC S9(4)   COMP VALUE +100.
00061      12  REC-COUNT               PIC S9(6)   COMP-3 VALUE +0.
00062      12  WS-WORK                 PIC S9(3)   COMP-3 VALUE +0.
00063      12  WS-REM                  PIC S9(6)   COMP-3 VALUE +0.
00064      12  PRT-CNT                 PIC 9       VALUE 4.
00065      12  WS-LINE-NUMBER          PIC 9(5)    COMP-3 VALUE ZERO.
00066      12  WS-PAGE                 PIC 9(5)    COMP-3 VALUE ZERO.
00067      12  WS-CURRENT-DATE         PIC X(8).
00068      12  SUB                     PIC 999     COMP-3 VALUE ZEROS.
00069      12  SUB1                    PIC 999     COMP-3 VALUE ZEROS.
00070      12  WS-SYNC-CNTR            PIC 999     COMP-3 VALUE ZEROS.
00071      12  WS-TIME-WORK            PIC S9(11)  COMP-3 VALUE ZERO.
00072      12  WS-START-TIME           PIC S9(11)  COMP-3 VALUE ZERO.
00073      12  WS-STOP-TIME            PIC S9(11)  COMP-3 VALUE ZERO.
00074      12  WS-LAST-TIME            PIC S9(11)  COMP-3 VALUE ZERO.
00075      12  WS-TIME                 PIC 9(7).
00076      12  FILLER    REDEFINES WS-TIME.
00077          16  FILLER              PIC X.
00078          16  WS-TIME-6           PIC X(6).
00079      12  FILLER    REDEFINES WS-TIME.
00080          16  FILLER              PIC X.
00081          16  WS-HOURS            PIC 99.
00082          16  WS-MINUTES          PIC 99.
00083          16  WS-SECONDS          PIC 99.
00084      12  ABEND-AREA              PIC X(72).
00085      12  ERPNDB-FILE-SW          PIC X     VALUE SPACE.
00086          88  ERPNDB-EOF      VALUE 'Y'.
00087          88  ERPNDB-EOF-COMP VALUE 'X'.
00088      12  ERPNDC-FILE-SW          PIC X     VALUE SPACE.
00089          88  ERPNDC-EOF      VALUE 'Y'.
00090          88  ERPNDC-EOF-COMP VALUE 'X'.
00091      12  DATA-EDITED-SW          PIC X     VALUE SPACE.
00092          88  DATA-EDITED     VALUE 'Y'.
00093      12  EDIT-PROCESS-SW         PIC X     VALUE SPACE.
00094          88  PROCESS-ENTIRE-FILE     VALUE 'F'.
00095          88  PROCESS-COMPANY         VALUE 'C'.
00096          88  PROCESS-BATCH           VALUE 'B'.
00097          88  BATCH-PROCESS-COMPLETE  VALUE 'Y'.
00098      12  FIRST-TIME-SW           PIC X     VALUE 'Y'.
00099          88  FIRST-TIME              VALUE 'Y'.
00100      12  WS-ERR-CODE.
00101          16  FILLER              PIC 99.
00102          16  WS-ERROR-SUB        PIC 99.
00103      12  PNDC-MSG PIC X(22)   VALUE '1PENDING CLAIMS EDIT  '.
00104      12  PRINT-CONTROL.
00105          16  SINGLE-SPACE        PIC X     VALUE SPACE.
00106          16  DOUBLE-SPACE        PIC X     VALUE ZERO.
00107          16  TRIPLE-SPACE        PIC X     VALUE '-'.
00108          16  SUPPRESS-SPACE      PIC X     VALUE '+'.
00109          16  TOP-OF-PAGE         PIC X     VALUE '1'.
00110      12  WS-COMPUTED-TOTALS      COMP-3.
00111          16  WS-LF-ISS-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.
00112          16  WS-LF-ISS-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.
00113          16  WS-AH-ISS-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.
00114          16  WS-AH-ISS-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.
00115          16  WS-LF-CAN-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.
00116          16  WS-LF-CAN-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.
00117          16  WS-AH-CAN-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.
00118          16  WS-AH-CAN-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.
00119          16  WS-ISSUE-CNT        PIC 9(5)      VALUE ZEROS COMP-3.
00120          16  WS-CANCEL-CNT       PIC 9(5)      VALUE ZEROS COMP-3.
00121      12  WS-LGX-CLAIM-USER       PIC X         VALUE SPACES.
00122          88  CO-HAS-CLAIMS                     VALUE 'Y'.
00123      12  TYPE-OF-EDIT            PIC X       VALUE SPACES.
00124          88  REEDIT-OR-RESTART-OR-FULL       VALUE 'X'.
00125
00126      12  WS-FILES-OPEN-SW        PIC X       VALUE SPACES.
00127          88  FILES-NOT-OPEN                  VALUE 'N'.
00128
00129      12  WS-DELAY-INTERVAL       PIC S9(7)     VALUE +2   COMP-3.
00130      12  PNDB-REC-COUNT          PIC S9(4)     VALUE +0.
00131
00132      12  WS-BATCH-NO.
00133          16  WS-BATCH-PREFIX         PIC XXX         VALUE SPACES.
00134          16  FILLER                  PIC XXX         VALUE SPACES.
00135
       01  ws-misc-work                pic x(1024) value spaces.
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
       01  WS-RESPONSE2                PIC S9(8) COMP VALUE +0.
00136  01  TEMPORARY-STORAGE-AREA.
00137      12  TS-QUEUE-ID.
00138          16  FILLER                  PIC XX          VALUE 'SE'.
00139          16  TS-QUEUE-COMPANY-ID     PIC X(3)        VALUE SPACES.
00140          16  FILLER                  PIC X(3)        VALUE SPACES.
00141
00142      12  TS-ITEM     COMP            PIC S9(4)       VALUE ZERO.
00143
00144      12  TS-RECORD.
00145          16  TS-COMPANY-CD           PIC X.
00146          16  TS-BATCH-NO             PIC X(6).
00147          16  TS-COMPANY-ID           PIC X(3).
00148          16  TS-RESTART-BATCH-NO     PIC X(6).
00149
00150      12  TS-RECORD-LENGTH  COMP      PIC S9(4) VALUE +16.
00151
00152      EJECT
00153  01  TRANS-DATA-MSG.
00154      12  TD-EDIT-TYPE                PIC X(7) VALUE 'BATCH'.
00155      12  TD-MSG      PIC X(20)  VALUE 'EDIT HAS COMPLETED  '.
00156      12  TD-REC-CNT              PIC ZZZ,ZZ9.
00157      12  FILLER                  PIC X(14) VALUE '  RECORDS FOR '.
00158      12  TD-COMPANY-ID           PIC X(3)  VALUE SPACES.
00159      12  FILLER      PIC X(20) VALUE ' LAST BATCH NO. WAS '.
00160      12  TD-LAST-BATCH           PIC X(6)  VALUE SPACES.
00161      12  FILLER      PIC X(09) VALUE ' TASK NO'.
00162      12  TD-TASK-NO              PIC 9(7)  VALUE ZERO.
00163      12  FILLER                      PIC X(14)       VALUE SPACES.
00164
00165  01  TRANS-DATA-MSG2.
00166      12  FILLER                      PIC X(10)    VALUE SPACES.
00167      12  FILLER      PIC X(9) VALUE ' STARTED '.
00168      12  TD-START-TIME.
00169          16  TD-START-HOURS          PIC 99 VALUE ZERO.
00170          16  FILLER                  PIC X VALUE '.'.
00171          16  TD-START-MINUTES        PIC 99 VALUE ZERO.
00172          16  FILLER                  PIC X VALUE '.'.
00173          16  TD-START-SECONDS        PIC 99 VALUE ZERO.
00174      12  FILLER      PIC X(9) VALUE ' ELAPSED '.
00175      12  TD-ELAPSED-TIME.
00176          16  TD-ELAPSED-HOURS        PIC 99 VALUE ZERO.
00177          16  FILLER                  PIC X VALUE '.'.
00178          16  TD-ELAPSED-MINUTES      PIC 99 VALUE ZERO.
00179          16  FILLER                  PIC X VALUE '.'.
00180          16  TD-ELAPSED-SECONDS      PIC 99 VALUE ZERO.
00181      12  FILLER      PIC X(10) VALUE ' LAST MSG '.
00182      12  TD-ELAPSED2-TIME.
00183          16  TD-ELAPSED2-HOURS       PIC 99 VALUE ZERO.
00184          16  FILLER                  PIC X VALUE '.'.
00185          16  TD-ELAPSED2-MINUTES     PIC 99 VALUE ZERO.
00186          16  FILLER                  PIC X VALUE '.'.
00187          16  TD-ELAPSED2-SECONDS     PIC 99 VALUE ZERO.
00188      12  FILLER  PIC X(38)       VALUE SPACES.
00189
00190      EJECT
00191  01  ACCESS-KEYS.
00192      12  ELCNTL-KEY.
00193          16  ELCNTL-COMPANY-ID   PIC XXX   VALUE SPACES.
00194          16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.
00195          16  ELCNTL-FILLER       PIC X(3)  VALUE SPACES.
00196          16  ELCNTL-CARRIER      PIC X     VALUE SPACES.
00197          16  ELCNTL-SEQ-NO       PIC S9(4) COMP  VALUE ZEROS.
00198      12  ERPNDB-KEY.
00199          16  ERPNDB-COMPANY-CD   PIC X     VALUE SPACES.
00200          16  ERPNDB-BATCH        PIC X(6)  VALUE SPACES.
00201          16  ERPNDB-SEQ-NO       PIC S9(4) COMP  VALUE ZEROS.
00202          16  ERPNDB-SEQ-XX REDEFINES ERPNDB-SEQ-NO PIC XX.
00203          16  ERPNDB-CHG-SEQ-NO   PIC S9(4) COMP  VALUE ZEROS.
00204      12  ERPNDC-KEY.
00205          16  ERPNDC-COMPANY-CD   PIC X     VALUE SPACES.
00206          16  ERPNDC-CARRIER      PIC X     VALUE SPACES.
00207          16  ERPNDC-GROUPING     PIC X(6)  VALUE SPACES.
00208          16  ERPNDC-STATE        PIC XX    VALUE SPACES.
00209          16  ERPNDC-ACCOUNT      PIC X(10) VALUE SPACES.
00210          16  ERPNDC-CERT-EFF-DT  PIC XX    VALUE SPACES.
00211          16  ERPNDC-CERT-NO      PIC X(11) VALUE SPACES.
00212          16  ERPNDC-CLAIM-NO     PIC X(7)  VALUE SPACES.
00213          16  ERPNDC-CHECK-NO     PIC X(7)  VALUE SPACES.
00214          16  ERPNDC-REC-TYPE     PIC X     VALUE SPACES.
00215          16  ERPNDC-SEQ-NO       PIC S9(4) COMP  VALUE ZEROS.
00216      12  CERT-KEY.
00217          16  CERT-COMPANY-CD             PIC X.
00218          16  CERT-CARRIER                PIC X.
00219          16  CERT-GROUPING               PIC X(6).
00220          16  CERT-STATE                  PIC XX.
00221          16  CERT-ACCOUNT                PIC X(10).
00222          16  CERT-CERT-EFF-DT            PIC XX.
00223          16  CERT-CERT-NO.
00224              20  CERT-CERT-PRIME         PIC X(10).
00225              20  CERT-CERT-SFX           PIC X.
00226
00227      EJECT
00228 *    COPY ELCREPT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCREPT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = REPORT STORAGE                            *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 146   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELREPT                 RKP=2,LEN=11      *
00013 *       ALTERNATE PATH  = NOT USED                               *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  REPORT-SAVE-FILE.
00019      12  RF-RECORD-ID                PIC XX.
00020          88  VALID-RF-ID                VALUE 'RF'.
00021
00022      12  RF-CONTROL-PRIMARY.
00023          16  RF-COMPANY-CD           PIC X.
00024          16  RF-RECORD-TYPE          PIC X.
00025              88  REPORT-DETAIL-RECORD   VALUE '1'.
00026              88  REPORT-TRAILER-RECORD  VALUE '2'.
00027          16  RF-REPORT-ID.
00028              20  RF-SYSTEM-CODE      PIC XX.
00029                  88  CLAS-IC-ONLINE     VALUE 'EL'.
00030                  88  CLAS-SCS-BATCH     VALUE 'EC'.
00031              20  RF-PROGRAM-SEQUENCE PIC 999.
00032          16  RF-LINE-NUMBER          PIC S9(8)       COMP.
00033      12  RF-REPORT-LINE-133.
00034          16  RF-CTL-CHAR-133         PIC X.
00035          16  RF-DATA-133             PIC X(132).
00036          16  RF-DATA-FIRST  REDEFINES RF-DATA-133.
00037              20  RF-DATA-2-81        PIC X(80).
00038              20  FILLER              PIC X(52).
00039          16  RF-DATA-LAST   REDEFINES RF-DATA-133.
00040              20  FILLER              PIC X(53).
00041              20  RF-DATA-55-133      PIC X(79).
00042      12  RF-TRAILER-RECORD  REDEFINES RF-REPORT-LINE-133.
00043          16  FILLER                  PIC X.
00044          16  RF-CURRENT-DATE         PIC X(8).
00045          16  RF-PRINT-HH-MM-SS       PIC X(6).
00046          16  FILLER                  PIC X(115).
00047          16  RF-COMPANY-ID           PIC XXX.
00048 ******************************************************************
00229      EJECT
00230
00231  01  PRT-LINE.
00232      12  PRT-MSG  PIC X(22)   VALUE '1PENDING BUSINESS EDIT'.
00233      12  FILLER   PIC X(11)   VALUE ' COMPLETED-'.
00234      12  PRT-CO   PIC X(31)   VALUE SPACES.
00235      12  PRT-DATE PIC X(8)    VALUE SPACES.
00236      12  FILLER   PIC X       VALUE SPACES.
00237      12  PRT-TIME PIC X(6)    VALUE SPACES.
00238
00239  01  START-LINE.
00240      12  START-MSG  PIC X(22)  VALUE '1PENDING BUSINESS EDIT'.
00241      12  FILLER     PIC X(11)  VALUE ' STARTED - '.
00242      12  START-CO   PIC X(31)  VALUE SPACES.
00243      12  START-DATE PIC X(8)   VALUE SPACES.
00244      12  FILLER     PIC X      VALUE SPACES.
00245      12  START-TIME PIC X(6)   VALUE SPACES.
00246
00247  01  TIME-UNFORMATTED.
00248      12  UN-HOURS                PIC XX.
00249      12  UN-MINUTES              PIC XX.
00250      12  FILLER                  PIC XX.
00251
00252  01  TIME-FORMATTED.
00253      12  FOR-HOURS               PIC XX.
00254      12  FILLER                  PIC X       VALUE ':'.
00255      12  FOR-MINUTES             PIC XX.
00256      EJECT
00257 *    COPY ELCDATE.
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
00258      EJECT
00259 *    COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00260  01  FILLER    REDEFINES DFHAID.
00261      12  FILLER              PIC X(8).
00262      12  PF-VALUES           PIC X       OCCURS 2.
00263      EJECT
00264 *    COPY ELCEMIB.
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
00265      EJECT
00266 *    COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
           12  jp-date                     pic s9(5) comp-3.
           12  jp-time                     pic s9(7) comp-3.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
00267                                 PIC X(585).
00268      EJECT
00269  01  BATCH-TO-PROCESS            VALUE SPACES.
00270      05  EDIT-COMPANY-CD         PIC X.
00271      05  EDIT-BATCH              PIC X(6).
00272      05  EDIT-COMPANY-ID         PIC XXX.
00273      05  EDIT-RESTART-BATCH      PIC X(6).
00274      EJECT
00275  01  EDIT-WORK-AREAS.
00276      05  ED-AH-BEN-ADDR          PIC S9(8)  COMP VALUE +0.
00277      05  ED-AH-BEN-ADDR          PIC S9(8)  COMP VALUE +0.
00278      05  ED-AH-BEN-ADDR          PIC S9(8)  COMP VALUE +0.
00279      05  ED-AH-BEN-ADDR          PIC S9(8)  COMP VALUE +0.
00280      EJECT
00281 *    COPY ERCPNDB REPLACING
00282 *    PENDING-BUSINESS       BY PNDB-EDIT-PASS-AREA.
00283 *                               COPY ERCPNDB.
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
00284  01  PNDB-EDIT-PASS-AREA.
00285      12  WK-PENDING-BUSINESS          PIC X(585) VALUE SPACES.
00286 * COPYBOOK FOR ADDITIONAL DFHCOMMAREA WK-WORK-AREA.
00287 *    COPY ELC50W1 REPLACING WK-CANCEL-EXIT-DT
00288 *                     BY  WK-CURRENT-DATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                             ELC50W1                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                          *
00006 *                                                                *
00007 *       THIS COPYBOOK IS USED BY EL050, EL051, AND EL517.        *
00008 *                                                                *
00009 *                                                                *
00010 ******************************************************************
00011
00012      12  WK-WORK-AREA.
00013          16  WK-CNTL-RECORD-FOUND-SW  PIC X   VALUE ' '.
00014          16  WK-LAST-CARRIER          PIC X   VALUE ' '.
00015          16  WK-CR-REM-TERM-CALC      PIC X   VALUE ' '.
00016          16  WK-CR-R78-METHOD         PIC X   VALUE ' '.
00017          16  WK-CO-MAX-CAP          PIC S9(3)V99  COMP-3 VALUE +0.
00018          16  WK-CO-TOL-CLAIM        PIC S9(3)V99  COMP-3 VALUE +0.
00019          16  WK-CO-TOL-PREM         PIC S9(3)V99  COMP-3 VALUE +0.
00020          16  WK-CO-TOL-REFUND       PIC S9(3)V99  COMP-3 VALUE +0.
00021          16  WK-CO-TOL-PREM-PCT     PIC S9V9(4)   COMP-3 VALUE +0.
00022          16  WK-CO-TOL-REFUND-PCT   PIC S9V9(4)   COMP-3 VALUE +0.
00023          16  WK-CO-PREM-REJECT-SW     PIC X   VALUE ' '.
00024          16  WK-CO-REF-REJECT-SW      PIC X   VALUE ' '.
00025          16  WK-BIRTH-DATE-INPUT      PIC X   VALUE ' '.
00026          16  WK-JOINT-AGE-INPUT       PIC X   VALUE ' '.
00027          16  WK-CURRENT-MONTH-END     PIC XX     VALUE LOW-VALUE.
00028          16  WK-CREDIT-EDIT-CONTROLS.
00029              20  WK-MIN-PREMIUM       PIC S9(3)V99  COMP-3.
00030              20  WK-MIN-AGE           PIC 99  VALUE 00.
00031              20  WK-DEFAULT-AGE       PIC 99  VALUE 00.
00032              20  WK-MIN-TERM          PIC S9(3) COMP-3 VALUE +0.
00033              20  WK-MAX-TERM          PIC S9(3) COMP-3 VALUE +0.
00034              20  WK-DEFAULT-SEX       PIC X  VALUE ' '.
00035          16  WK-CURRENT-DATE        PIC XX VALUE LOW-VALUE.
00036          16  WK-SAVE-REIN-DATA.
00037              20  WK-REIN-TABLE        PIC X(3) VALUE SPACES.
00038              20  WK-REIN-ST-AH        PIC X(2) VALUE SPACES.
00039              20  WK-REIN-ST-LF        PIC X(2) VALUE SPACES.
00040          16  WK-ENTRY-SW              PIC X    VALUE ' '.
00041              88  ENTRY-FROM-EL6311       VALUE '6'.
00042          16  WK-REM-TRM-CALC-OPTION   PIC X    VALUE ' '.
00043          16  WK-DEFAULT-APR           PIC S9(3)V9(4) COMP-3.
00044      12  WK-RECORD-ADDRESSES.
00045          16  WK-ACCT-ADDR             PIC S9(8) COMP VALUE ZEROS.
00046          16  WK-LIFE-EDIT-ADDR        PIC S9(8) COMP VALUE ZEROS.
00047          16  WK-AH-EDIT-ADDR          PIC S9(8) COMP VALUE ZEROS.
00048          16  WK-LIFE-BEN-ADDR         PIC S9(8) COMP VALUE ZEROS.
00049          16  WK-AH-BEN-ADDR           PIC S9(8) COMP VALUE ZEROS.
00050          16  WK-STATE-ADDR            PIC S9(8) COMP VALUE ZEROS.
00051          16  WK-PLAN-ADDR             PIC S9(8) COMP VALUE ZEROS.
00052          16  WK-FORM-ADDR             PIC S9(8) COMP VALUE ZEROS.
00053      12  WK-OVER-SHORT-AREA.
00054          16  WK-REFUND-OVS-AMT        PIC S999V99 COMP-3 VALUE +0.
00055          16  WK-REFUND-OVS-PCT        PIC S9V9(4) COMP-3 VALUE +0.
               16  WK-CSR-SESSION-SW        PIC X  VALUE ' '.
                   88  WK-CSR-EDIT-SESSION       VALUE 'Y'.
00056          16  FILLER                   PIC X(5) VALUE SPACES.
00289      12  EDIT-CRITERIA-WORK-AREA      PIC X(352) VALUE SPACES.
00290      EJECT
00291 *    COPY ERCPNDC REPLACING
00292 *    PENDING-CLAIMS          BY PNDC-EDIT-PASS-AREA.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDC                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING CLAIM TRANSACTIONS                *
00008 *                      PAYMENTS, RESERVES, EXPENSES              *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 500  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERPNDC                         RKP=2,LEN=50   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
00019
00020  01  PNDC-EDIT-PASS-AREA.
00021      12  PC-RECORD-ID                     PIC XX.
00022          88  VALID-PC-ID                      VALUE 'PC'.
00023
00024      12  PC-CONTROL-PRIMARY.
00025          16  PC-COMPANY-CD                PIC X.
00026          16  PC-CARRIER                   PIC X.
00027          16  PC-GROUPING.
00028              20  PC-GROUPING-PREFIX       PIC XXX.
00029              20  PC-GROUPING-PRIME        PIC XXX.
00030          16  PC-STATE                     PIC XX.
00031          16  PC-ACCOUNT.
00032              20  PC-ACCOUNT-PREFIX        PIC X(4).
00033              20  PC-ACCOUNT-PRIME         PIC X(6).
00034          16  PC-CERT-EFF-DT               PIC XX.
00035          16  PC-CERT-NO.
00036              20  PC-CERT-PRIME            PIC X(10).
00037              20  PC-CERT-SFX              PIC X.
00038          16  PC-CLAIM-NO                  PIC X(7).
00039          16  PC-CHECK-NO                  PIC X(7).
00040
00041          16  PC-RECORD-TYPE               PIC X.
00042              88  PC-CLAIMS                    VALUE '1'.
00043              88  PC-RESERVES                  VALUE '2'.
00044          16  PC-RECORD-SEQUENCE           PIC S9(4)     COMP.
00045
00046      12  PC-LAST-MAINT-DT                 PIC XX.
00047      12  PC-LAST-MAINT-BY                 PIC X(4).
00048      12  PC-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00049
00050      12  PC-CLAIM-RECORD.
00051          16  PC-CLAIM-TYPE                PIC X.
00052              88  PC-LF-CLAIM                  VALUE '1'.
00053              88  PC-AH-CLAIM                  VALUE '2'.
00054              88  PC-OB-LF-CLAIM               VALUE '3'.
00055              88  PC-OB-AH-CLAIM               VALUE '4'.
00056          16  PC-PAYMENT-DT                PIC XX.
00057          16  PC-PAID-THRU-DT              PIC XX.
00058          16  PC-REPORTED-DT               PIC XX.
00059          16  PC-INCURRED-DT               PIC XX.
00060          16  PC-NO-OF-DAYS-PAID           PIC S9(3)     COMP-3.
00061          16  PC-CLAIM-PAYMENT             PIC S9(7)V99  COMP-3.
00062          16  PC-AGE-AT-CLAIM              PIC 99.
00063          16  FILLER                       PIC XX.
00064          16  PC-PAYMENT-TYPE              PIC X.
00065              88  PC-PARTIAL-PAYMENT           VALUE '1'.
00066              88  PC-FINAL-PAYMENT             VALUE '2'.
00067              88  PC-LUMP-SUM-PAYMENT          VALUE '3'.
00068              88  PC-ADDITIONAL-PAYMENT        VALUE '4'.
00069              88  PC-CHARGEBLE-EXPENSE         VALUE '5'.
00070              88  PC-NON-CHARGEBLE-EXPENSE     VALUE '6'.
00071              88  PC-VOIDED-PAYMENT            VALUE '9'.
00072
00073          16  PC-FUTURE-RESERVE-AMT        PIC S9(7)V99  COMP-3.
00074          16  PC-IBNR-RESERVE-AMT          PIC S9(7)V99  COMP-3.
00075          16  PC-PTC-RESERVE-AMT           PIC S9(7)V99  COMP-3.
00076          16  PC-MANUAL-RESERVE-AMT        PIC S9(7)V99  COMP-3.
00077
00078          16  PC-SV-CARRIER                PIC X.
00079          16  PC-SV-GROUPING               PIC X(6).
00080          16  PC-SV-STATE                  PIC XX.
00081
00082          16  PC-VOID-SW                   PIC X.
00083              88  PC-PUT-CERT-INFORCE          VALUE '1'.
00084
00085          16  PC-CAUSE-CODE                PIC X(6).
00086          16  FILLER                       PIC X(48).
00087
00088          16  PC-CLAIMED-CERT-DATA.
00089              20  PC-CC-INSURED-NAME.
00090                  24  PC-CC-LAST-NAME      PIC X(15).
00091                  24  PC-CC-INITIALS       PIC XX.
00092              20  PC-CC-INSURED-AGE        PIC S99.
00093              20  PC-CC-INSURED-SEX        PIC X.
00094              20  PC-CC-ORIG-TERM          PIC S999        COMP-3.
00095              20  PC-CC-LF-BENEFIT-CD      PIC XX.
00096              20  PC-CC-LIFE-BENEFIT-AMT   PIC S9(9)V99    COMP-3.
00097              20  PC-CC-ALT-LF-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00098              20  PC-CC-LIFE-PREMIUM       PIC S9(7)V99    COMP-3.
00099              20  PC-CC-AH-BENEFIT-CD      PIC XX.
00100              20  PC-CC-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00101              20  PC-CC-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00102              20  PC-CC-RATE-CLASS         PIC XX.
00103              20  PC-CC-RATE-DEV           PIC XXX.
00104              20  PC-CC-OB-FLAG            PIC X.
00105                  88  PC-CC-OB                VALUE 'B'.
00106              20  PC-CC-AH-POLICY-STATUS   PIC X.
00107                  88  PC-CCA-POLICY-IS-ACTIVE        VALUE '1' '3'
00108                                                '4' '5' '9' '2'.
00109                  88  PC-CCA-NORMAL-ENTRY            VALUE '1'.
00110                  88  PC-CCA-POLICY-PENDING          VALUE '2'.
00111                  88  PC-CCA-POLICY-IS-RESTORE       VALUE '3'.
00112                  88  PC-CCA-CONVERSION-ENTRY        VALUE '4'.
00113                  88  PC-CCA-POLICY-IS-REISSUE       VALUE '5'.
00114                  88  PC-CCA-LUMP-SUM-DISAB          VALUE '6'.
00115                  88  PC-CCA-DEATH-CLAIM-APPLIED     VALUE '7'.
00116                  88  PC-CCA-CANCEL-APPLIED          VALUE '8'.
00117                  88  PC-CCA-REIN-ONLY               VALUE '9'.
00118              20  PC-CC-LF-POLICY-STATUS   PIC X.
00119                  88  PC-CCL-POLICY-IS-ACTIVE        VALUE '1' '3'
00120                                                '4' '5' '9' '2'.
00121                  88  PC-CCL-NORMAL-ENTRY            VALUE '1'.
00122                  88  PC-CCL-POLICY-PENDING          VALUE '2'.
00123                  88  PC-CCL-POLICY-IS-RESTORE       VALUE '3'.
00124                  88  PC-CCL-CONVERSION-ENTRY        VALUE '4'.
00125                  88  PC-CCL-POLICY-IS-REISSUE       VALUE '5'.
00126                  88  PC-CCL-LUMP-SUM-DISAB          VALUE '6'.
00127                  88  PC-CCL-DEATH-CLAIM-APPLIED     VALUE '7'.
00128                  88  PC-CCL-CANCEL-APPLIED          VALUE '8'.
00129                  88  PC-CCL-REIN-ONLY               VALUE '9'.
00130              20  PC-CC-PAY-FREQUENCY      PIC 99.
00131              20  PC-CC-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00132              20  PC-CC-SOC-SEC-NO         PIC X(11).
00133              20  PC-CC-MEMBER-NO          PIC X(12).
00134              20  PC-CC-INT-CODE           PIC X.
00135                  88  PC-CC-ADD-ON                  VALUE 'A'.
00136                  88  PC-CC-SIMPLE                  VALUE 'S'.
00137              20  PC-CC-CAPPED-TERM        PIC 999.
00138              20  PC-CC-PRIOR-LUMP-PMT     PIC S9(7)V99  COMP-3.
00139              20  PC-CC-PRIOR-DEATH-AMT    PIC S9(9)V99  COMP-3.
00140              20  PC-CC-CANCEL-DT          PIC XX.
00141              20  PC-CC-DEATH-DT           PIC XX.
00142              20  PC-CC-SETTLEMENT-DT      PIC XX.
00143              20  PC-CC-PRIOR-STATUS       PIC X.
00144              20  PC-CC-CERT-ENTRY-STATUS  PIC X.
00145          16  PC-TRLR-SEQ-NO               PIC S9(4)     COMP.
070105         16  PC-CC-CLP-STATE              PIC XX.
070105         16  FILLER                       PIC X(14).
00147          16  PC-REMAINING-BENEFIT         PIC S9(9)V99  COMP-3.
00148          16  PC-REMAINING-TERM            PIC S9(3)     COMP-3.
00149          16  FILLER                       PIC X(34).
00150
00151      12  PC-RECORD-STATUS.
00152          16  PC-CREDIT-SELECT-DT          PIC XX.
00153          16  PC-CREDIT-ACCEPT-DT          PIC XX.
00154          16  FILLER                       PIC XX.
00155          16  PC-FATAL-FLAG                PIC X.
00156              88  PC-FATAL-ERRORS             VALUE 'X'.
00157          16  PC-FORCE-CODE                PIC X.
00158              88  PC-FORCE-OFF                VALUE ' ' '0'.
00159              88  PC-CLAIM-FORCE              VALUE '6' '7'
00160                                                      '8'.
00161          16  PC-FORCE-ER-CD               PIC X.
00162              88  PC-FORCE-ERRORS             VALUE 'F'.
00163              88  PC-UNFORCED-ERRORS          VALUE 'X'.
00164          16  PC-WARN-ER-CD                PIC X.
00165              88  PC-WARNING-ERRORS           VALUE 'W'.
00166          16  PC-LF-OVERRIDE-L1            PIC X.
00167          16  PC-AH-OVERRIDE-L1            PIC X.
00168          16  FILLER                       PIC X(17).
00169          16  PC-CERT-UPDATE-SW            PIC X.
00170              88  PC-CERT-DATA-CAPTURED       VALUE '1'.
00171          16  PC-COMPANY-ID                PIC XXX.
00172          16  PC-INPUT-DT                  PIC XX.
00173
00174      12  PC-ERROR-FLAGS.
00175          16  PC-STANDARD-ERRORS.
00176              20  PC-STD-ERROR-FLAGS   OCCURS 25 TIMES PIC X.
00177          16  PC-TRANSACTION-ERRORS.
00178              20  PC-TRN-ERROR-FLAGS   OCCURS 75 TIMES PIC X.
00179
00180      12  PC-ERR-FLAGS-R REDEFINES  PC-ERROR-FLAGS.
00181          16  PC-ERR-FLAG              OCCURS 100 TIMES PIC X.
00182
00183      12  FILLER                           PIC X(25).
00184
00185 ******************************************************************
00293      12  WK-PC-WORK-AREA.
00294          16  WK-PC-CNTL-RECORD-FOUND-SW  PIC X.
00295          16  WK-PC-LAST-CARRIER          PIC X.
00296          16  WK-PC-CERT-ACCESS-CNTL      PIC X.
00297          16  WK-PC-CO-CLAIM-REJECT-SW    PIC X.
00298          16  WK-PC-CLAIM-SYSTEM-SW       PIC X.
00299          16  WK-PC-CO-TOL-CLAIM          PIC S9(3)V99  COMP-3.
00300          16  WK-PC-RESERVE-CONTROLS      PIC X(4).
00301          16  WK-PC-CREDIT-EDIT-CONTROLS  PIC X(12).
00302      12  WK-PC-RECORD-ADDRESSES.
00303          16  WK-PC-ACCT-ADDR             PIC S9(8)     COMP.
00304          16  WK-PC-STATE-ADDR            PIC S9(8)     COMP.
00305      12  WK-MISC.
00306          16  WK-PC-REM-TRM-CALC-OPTION   PIC X.
00307          16  FILLER                      PIC X(20).
00308      EJECT
00322 *    COPY ELCCNTL.
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
00323      EJECT
00324 *    COPY ELCCERT.
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
00325      EJECT
110921 01  work-flow-pass-area.
110921     05  pa-rec-type             pic x(4).
110921     05  pa-company-id           pic xxx.
110921     05  pa-rest-of-record       pic x(600).
00309  01  ERROR-SEVERITY-CODES.
00310      12  STD-ERR-SEVERITY            OCCURS 25 TIMES
00311                                      INDEXED BY SINDEX PIC X.
00312      12  TRN-ERR-SEVERITY            OCCURS 150 TIMES
00313                                      INDEXED BY TINDEX PIC X.
00314      EJECT
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
00316  01  DFHCOMMAREA                     PIC X(1024).
110921 01  var  pic x(30).
00317 *01 PARM-LIST .
00318 *    02  FILLER              PIC S9(8)   COMP.
00319 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.
00320 *    02  CERT-POINTER        PIC S9(8)   COMP.
00321      EJECT
00326
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA VAR.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL051' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           display ' Entering Program EL051 '
00328      
      * EXEC CICS HANDLE CONDITION
00329 *        ERROR    (8300-ABEND)
00330 *        PGMIDERR (9900-ERROR-FORMAT)
00331 *        NOTFND   (0210-PROCESS-FILE)
00332 *        ENDDATA  (0210-PROCESS-FILE)
00333 *    END-EXEC.
      *    MOVE '"$.LI&                ! " #00003626' TO DFHEIV0
           MOVE X'22242E4C4926202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033363236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS RETRIEVE
      *        INTO   (BATCH-TO-PROCESS)
      *        LENGTH (ELEN)
      *        resp   (ws-response)
      *        resp2  (ws-response2)
      *    END-EXEC
      *    MOVE '0*I L                 &  N#00003632' TO DFHEIV0
           MOVE X'302A49204C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033363332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BATCH-TO-PROCESS, 
                 ELEN, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           MOVE EIBRESP2 TO ws-response2
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           display ' retrieve response ' ws-response ' ' ws-response2
           display ' Pass area **' batch-to-process '**'
           if not resp-normal
              display ' not normal, returning '
              go to 9999-return-cics
           end-if
110921     set P to address of KIXSYS
110921     CALL "getenv" using by value P returning var-ptr
110921     if var-ptr = null then
110921        display ' kixsys not set '
110921     else
110921        set address of var to var-ptr
110921        move 0 to env-var-len
110921        inspect var tallying env-var-len
110921          for characters before X'00'
110921        unstring var (1:env-var-len) delimited by '/'
110921           into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
110921              WS-KIX-SYS
110921        end-unstring
110921     end-if
110921
110921     display ' ENVIRONMENT ' ws-kix-myenv
00339      MOVE BATCH-TO-PROCESS       TO  TS-RECORD.
00340
00341      MOVE EDIT-COMPANY-ID        TO  TS-QUEUE-COMPANY-ID.
00342
00343      IF TS-RESTART-BATCH-NO NOT NUMERIC
00344          MOVE LOW-VALUES         TO  TS-RESTART-BATCH-NO.
00345
00346      
      * EXEC CICS WRITEQ TS
00347 *        QUEUE  (TS-QUEUE-ID)
00348 *        ITEM   (TS-ITEM)
00349 *        FROM   (TS-RECORD)
00350 *        LENGTH (TS-RECORD-LENGTH)
00351 *    END-EXEC.
      *    MOVE '*" I   L              ''   #00003667' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-QUEUE-ID, 
                 TS-RECORD, 
                 TS-RECORD-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00352
00353      
      * EXEC CICS SYNCPOINT
00354 *    END-EXEC.
      *    MOVE '6"                    !   #00003674' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00355
00356      IF EDIT-BATCH = SPACES
00357          MOVE 'C'                TO EDIT-PROCESS-SW
00358        ELSE
00359          MOVE 'B'                TO EDIT-PROCESS-SW.
00360
00361      GO TO 1000-PROCESS-FILE.
00362
00363  0210-PROCESS-FILE.
00364      MOVE 'F'                    TO EDIT-PROCESS-SW.
00365
00366      EJECT
00367  1000-PROCESS-FILE.
           display ' EIBTRNID = ' EIBTRNID
00368      IF EIBTRNID = 'EXEB' OR 'XXEB'
00369          MOVE 'BATCH'          TO  TD-EDIT-TYPE
00370          
      * EXEC CICS ENQ
00371 *            RESOURCE (EIBTRNID)
00372 *            LENGTH   (4)
00373 *        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '2$L                   $   #00003692' TO DFHEIV0
           MOVE X'32244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EIBTRNID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00374       ELSE
00375        
      * EXEC CICS ENQ
00376 *           RESOURCE   (EDIT-COMPANY-CD)
00377 *           LENGTH     (1)
00378 *      END-EXEC
           MOVE 1
             TO DFHEIV11
      *    MOVE '2$L                   $   #00003697' TO DFHEIV0
           MOVE X'32244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EDIT-COMPANY-CD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00379        MOVE 'SYSTEM '            TO  TD-EDIT-TYPE.
00380
00381      MOVE REC-COUNT              TO  TD-REC-CNT.
00382      MOVE EDIT-COMPANY-ID        TO  TD-COMPANY-ID.
00383      MOVE EDIT-BATCH             TO  TD-LAST-BATCH.
00384      MOVE EIBTASKN               TO  TD-TASK-NO.
00385
00386      
      * EXEC CICS ASKTIME
00387 *    END-EXEC.
      *    MOVE '0"                    "   #00003708' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00388
00389      MOVE EIBTIME                TO  WS-TIME.
00390      COMPUTE WS-START-TIME = (WS-HOURS * 3600) +
00391                              (WS-MINUTES * 60) + WS-SECONDS.
00392
00393      MOVE WS-HOURS               TO  TD-START-HOURS.
00394      MOVE WS-MINUTES             TO  TD-START-MINUTES.
00395      MOVE WS-SECONDS             TO  TD-START-SECONDS.
00396
00397      
      * EXEC CICS WRITEQ TD
00398 *        QUEUE  ('CSMT')
00399 *        FROM   (TRANS-DATA-MSG)
00400 *        LENGTH (TRANS-DATA-LENGTH)
00401 *    END-EXEC.
           MOVE 'CSMT' TO DFHEIV5
      *    MOVE '(" L   L              &   #00003719' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRANS-DATA-MSG, 
                 TRANS-DATA-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00402
00403      
      * EXEC CICS WRITEQ TD
00404 *        QUEUE  ('CSMT')
00405 *        FROM   (TRANS-DATA-MSG2)
00406 *        LENGTH (TRANS-DATA-LENGTH)
00407 *    END-EXEC.
           MOVE 'CSMT' TO DFHEIV5
      *    MOVE '(" L   L              &   #00003725' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRANS-DATA-MSG2, 
                 TRANS-DATA-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00408
00409      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00410      MOVE '5'                    TO DC-OPTION-CODE.
00411      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00412      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DATE.
00413      MOVE DC-BIN-DATE-1          TO WK-CURRENT-DATE.
00414      MOVE WS-CURRENT-DATE        TO PRT-DATE
00415                                     START-DATE.
00416      MOVE ZEROS                  TO WK-ACCT-ADDR
00417                                     WK-LIFE-EDIT-ADDR
00418                                     WK-AH-EDIT-ADDR
00419                                     WK-LIFE-BEN-ADDR
00420                                     WK-AH-BEN-ADDR
00421                                     WK-STATE-ADDR
00422                                     WK-PC-ACCT-ADDR
00423                                     WK-PC-STATE-ADDR
00424                                     WK-PLAN-ADDR
00425                                     WK-FORM-ADDR.
00426
00427      MOVE SPACE                  TO WK-CNTL-RECORD-FOUND-SW
00428                                     WK-PC-CNTL-RECORD-FOUND-SW
00429                                     ERROR-SEVERITY-CODES.
00430
00431      IF PROCESS-ENTIRE-FILE
00432          MOVE LOW-VALUES         TO ERPNDB-KEY
00433                                     ERPNDC-KEY
00434          MOVE ZEROS              TO ERPNDB-SEQ-NO
00435                                     ERPNDC-SEQ-NO
00436      ELSE
00437          MOVE EDIT-COMPANY-CD    TO ERPNDB-COMPANY-CD
00438                                     ERPNDC-COMPANY-CD
00439          IF EDIT-RESTART-BATCH = SPACES OR 'REEDIT'
00440             MOVE EDIT-BATCH      TO ERPNDB-BATCH
00441          ELSE
00442             MOVE EDIT-RESTART-BATCH TO ERPNDB-BATCH.
00443
00444      IF EDIT-BATCH         = SPACES  OR
00445         EDIT-RESTART-BATCH = 'REEDIT'
00446         MOVE 'X'                 TO TYPE-OF-EDIT
00447         PERFORM 6000-WRITE-EL051-MESSAGE THRU 6999-EXIT.
00448
00449  1100-PROCESS-PNDB-LOOP.
00450      MOVE ERPNDB-BATCH           TO WS-PREV-BATCH.
00451      PERFORM 3200-READ-NEXT-RECORD THRU 3290-EXIT.
00452
00453      IF ERPNDB-EOF
00454        OR ERPNDB-EOF-COMP
00455          GO TO 1200-WRITE-REPORT.
00456
00457      IF NOT PB-BATCH-TRAILER
00458          ADD 1                  TO WS-SYNC-CNTR
00459                                    REC-COUNT
00460          PERFORM 2000-CALL-PNDB-EDIT-PROGRAM THRU  2090-EXIT.
110921     if pb-issue
110921        perform 1800-work-flow   thru 1800-exit
110921     end-if
00461
00462      PERFORM 4000-SET-PNDB-ERROR-FLAGS THRU 4900-EXIT.
00463
00464      IF WS-SYNC-CNTR = 32
00465         MOVE ZEROS               TO WS-SYNC-CNTR
00466         
      * EXEC CICS SYNCPOINT
00467 *       END-EXEC.
      *    MOVE '6"                    !   #00003791' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00468
00469      IF PROCESS-COMPANY AND NOT PB-BATCH-TRAILER
00470          DIVIDE REC-COUNT BY 500 GIVING WS-WORK REMAINDER WS-REM
00471          IF WS-REM = ZEROS
00472             MOVE REC-COUNT           TO TD-REC-CNT
00473             MOVE EDIT-COMPANY-ID TO TD-COMPANY-ID
00474             MOVE ERPNDB-BATCH TO TD-LAST-BATCH
00475                                  TS-RESTART-BATCH-NO
00476             PERFORM 1500-PRINT-MESSAGE THRU 1599-EXIT
00477             ADD +1 TO WS-LINE-NUMBER
00478             MOVE ERPNDB-COMPANY-CD  TO RF-COMPANY-CD
00479             MOVE '1'             TO RF-RECORD-TYPE
00480             MOVE 'EL051'         TO RF-REPORT-ID
00481             MOVE TRANS-DATA-MSG  TO RF-DATA-133
00482             PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT
00483
00484             PERFORM 5950-UPDATE-TYPE-2 THRU 5959-EXIT
00485             
      * EXEC CICS WRITEQ TS REWRITE
00486 *                QUEUE  (TS-QUEUE-ID)
00487 *                ITEM   (TS-ITEM)
00488 *                FROM   (TS-RECORD)
00489 *                LENGTH (TS-RECORD-LENGTH)
00490 *           END-EXEC.
      *    MOVE '*" IR  L              ''   #00003810' TO DFHEIV0
           MOVE X'2A2220495220204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-QUEUE-ID, 
                 TS-RECORD, 
                 TS-RECORD-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00491
00492      IF BATCH-PROCESS-COMPLETE
00493          MOVE 'EDIT TERMINATED'  TO TD-MSG
00494          PERFORM 1500-PRINT-MESSAGE THRU 1599-EXIT
00495          MOVE HIGH-VALUES        TO  TS-RESTART-BATCH-NO
00496          
      * EXEC CICS WRITEQ TS REWRITE
00497 *            QUEUE  (TS-QUEUE-ID)
00498 *            ITEM   (TS-ITEM)
00499 *            FROM   (TS-RECORD)
00500 *            LENGTH (TS-RECORD-LENGTH)
00501 *        END-EXEC
      *    MOVE '*" IR  L              ''   #00003821' TO DFHEIV0
           MOVE X'2A2220495220204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-QUEUE-ID, 
                 TS-RECORD, 
                 TS-RECORD-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00502          GO TO 9999-RETURN-CICS.
00503
00504      GO TO 1100-PROCESS-PNDB-LOOP.
00505
00506      EJECT
00507  1200-WRITE-REPORT.
00508      MOVE REC-COUNT              TO TD-REC-CNT.
00509      MOVE EDIT-COMPANY-ID        TO TD-COMPANY-ID.
00510      MOVE 'EDIT TERMINATED'      TO TD-MSG.
00511      MOVE WS-PREV-BATCH          TO TD-LAST-BATCH.
00512
00513      PERFORM 1500-PRINT-MESSAGE THRU 1599-EXIT.
00514
00515      MOVE HIGH-VALUES            TO  TS-RESTART-BATCH-NO.
00516
00517      
      * EXEC CICS WRITEQ TS REWRITE
00518 *        QUEUE  (TS-QUEUE-ID)
00519 *        ITEM   (TS-ITEM)
00520 *        FROM   (TS-RECORD)
00521 *        LENGTH (TS-RECORD-LENGTH)
00522 *    END-EXEC.
      *    MOVE '*" IR  L              ''   #00003842' TO DFHEIV0
           MOVE X'2A2220495220204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-QUEUE-ID, 
                 TS-RECORD, 
                 TS-RECORD-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00523
00524      IF NOT REEDIT-OR-RESTART-OR-FULL
00525         GO TO 1260-NO-REPORT.
00526
00527      IF EDIT-PROCESS-SW = 'B' OR 'Y'
00528          GO TO 1260-NO-REPORT.
00529
00530      ADD +1 TO WS-LINE-NUMBER.
00531      MOVE ERPNDB-COMPANY-CD      TO RF-COMPANY-CD.
00532      MOVE '1'                    TO RF-RECORD-TYPE.
00533      MOVE 'EL051'                TO RF-REPORT-ID.
00534      MOVE TRANS-DATA-MSG         TO RF-DATA-133.
00535      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.
00536
00537      IF DATA-EDITED
00538          MOVE SPACE              TO DATA-EDITED-SW
00539      ELSE
00540          GO TO 1260-NO-REPORT.
00541
00542  1250-CONTINUE.
00543
00544      MOVE EDIT-COMPANY-CD        TO RF-COMPANY-CD.
00545      MOVE 'RF'                   TO RF-RECORD-ID.
00546      MOVE '2'                    TO RF-RECORD-TYPE.
00547      MOVE 'EL051'                TO RF-REPORT-ID.
00548
00549      
      * EXEC CICS DELETE
00550 *        DATASET (ELREPT-FILE-ID)
00551 *        RIDFLD  (RF-CONTROL-PRIMARY)
00552 *        KEYLENGTH (7)
00553 *        GENERIC
00554 *    END-EXEC.
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00003874' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00555
00556      MOVE SPACES                 TO RF-TRAILER-RECORD.
00557      MOVE WS-CURRENT-DATE        TO RF-CURRENT-DATE
00558      MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.
00559      MOVE '2'                    TO RF-RECORD-TYPE.
00560      MOVE ERPNDB-COMPANY-CD      TO RF-COMPANY-CD.
00561      MOVE 'EL051'                TO RF-REPORT-ID.
00562      MOVE WS-LINE-NUMBER         TO RF-LINE-NUMBER.
00563      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.
00564
00565  1260-NO-REPORT.
00566      IF ERPNDB-EOF
00567          MOVE 'Y'                TO FIRST-TIME-SW
00568          MOVE PNDC-MSG           TO PRT-MSG
00569          GO TO 1300-START-CLAIMS-EDIT.
00570
00571      PERFORM 3000-READ-CONTROL-FILE THRU 3090-EXIT.
00572      MOVE CF-CL-MAIL-TO-NAME     TO PRT-CO.
00573      MOVE LOW-VALUES             TO ERPNDB-KEY.
00574      MOVE PB-COMPANY-CD          TO ERPNDB-COMPANY-CD.
00575      MOVE ZEROS                  TO ERPNDB-SEQ-NO.
00576      MOVE SPACE                  TO ERPNDB-FILE-SW
00577                                     WK-CNTL-RECORD-FOUND-SW.
00578      GO TO 1100-PROCESS-PNDB-LOOP.
00579      EJECT
00580  1300-START-CLAIMS-EDIT.
00581
00582      PERFORM 3000-READ-CONTROL-FILE THRU 3090-EXIT.
00583
00584      IF CO-HAS-CLAIMS
00585         GO TO 9999-RETURN-CICS.
00586
00587      PERFORM 7000-WRITE-EL053-MESSAGE THRU 7999-EXIT.
00588
00589  1300-PROCESS-PNDC-LOOP.
00590      PERFORM 3300-READ-NEXT-RECORD THRU 3390-EXIT.
00591
00592      IF ERPNDC-EOF  OR
00593         ERPNDC-EOF-COMP
00594          GO TO 1400-WRITE-REPORT.
00595
00596      PERFORM 2100-CALL-PNDC-EDIT-PROGRAM THRU  2190-EXIT.
00597      PERFORM 5000-SET-PNDC-ERROR-FLAGS THRU 5900-EXIT.
00598
00599      GO TO 1300-PROCESS-PNDC-LOOP.
00600
00601      EJECT
00602
00603  1400-WRITE-REPORT.
00604      IF DATA-EDITED
00605          MOVE SPACE              TO DATA-EDITED-SW
00606      ELSE
00607          GO TO 1460-NO-REPORT.
00608
00609      IF EDIT-PROCESS-SW = 'B' OR 'Y'
00610          GO TO 1460-NO-REPORT.
00611
00612  1450-CONTINUE.
00613      MOVE EDIT-COMPANY-CD        TO RF-COMPANY-CD.
00614      MOVE 'EL053'                TO RF-REPORT-ID.
00615      MOVE +1                     TO RF-LINE-NUMBER.
00616      MOVE '2'                    TO RF-RECORD-TYPE.
00617      
      * EXEC CICS READ
00618 *         DATASET  ('ELREPT')
00619 *         INTO     (REPORT-SAVE-FILE)
00620 *         RIDFLD   (RF-CONTROL-PRIMARY)
00621 *         UPDATE
00622 *    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&"IL       EU         (   #00003942' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00623
00624      MOVE WS-CURRENT-DATE        TO RF-CURRENT-DATE.
00625
00626      
      * EXEC CICS REWRITE
00627 *         DATASET  ('ELREPT')
00628 *         FROM     (REPORT-SAVE-FILE)
00629 *    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&& L                  %   #00003951' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00630
00631  1460-NO-REPORT.
00632      IF ERPNDC-EOF
00633          GO TO 9999-RETURN-CICS.
00634
00635      PERFORM 3000-READ-CONTROL-FILE THRU 3090-EXIT.
00636      MOVE CF-CL-MAIL-TO-NAME     TO PRT-CO.
00637      MOVE LOW-VALUES             TO ERPNDC-KEY.
00638      MOVE PC-COMPANY-CD          TO ERPNDC-COMPANY-CD.
00639      MOVE ZEROS                  TO ERPNDC-SEQ-NO.
00640      MOVE SPACE                  TO ERPNDC-FILE-SW
00641                                     WK-PC-CNTL-RECORD-FOUND-SW.
00642      GO TO 1300-PROCESS-PNDC-LOOP.
00643      EJECT
00644  1500-PRINT-MESSAGE.
00645      MOVE REC-COUNT              TO  TD-REC-CNT.
00646
00386      
      * EXEC CICS ASKTIME
00387 *    END-EXEC.
      *    MOVE '0"                    "   #00003972' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00388
00647      MOVE EIBTIME                TO  WS-TIME.
00649
00650      COMPUTE WS-STOP-TIME = (WS-HOURS * 3600) +
00651                                 (WS-MINUTES * 60) + WS-SECONDS.
00652
00648      MOVE WS-STOP-TIME           TO  WS-LAST-TIME.
00653      SUBTRACT WS-START-TIME FROM WS-STOP-TIME
00654                                  GIVING WS-TIME-WORK.
00655      DIVIDE WS-TIME-WORK BY +3600 GIVING TD-ELAPSED-HOURS
00656                                  REMAINDER WS-TIME-WORK.
00657      DIVIDE WS-TIME-WORK BY +60 GIVING TD-ELAPSED-MINUTES
00658                                  REMAINDER TD-ELAPSED-SECONDS.
00659
00660      SUBTRACT WS-LAST-TIME FROM WS-STOP-TIME
00661                                  GIVING WS-TIME-WORK.
00662      DIVIDE WS-TIME-WORK BY +3600 GIVING TD-ELAPSED2-HOURS
00663                                  REMAINDER WS-TIME-WORK.
00664      DIVIDE WS-TIME-WORK BY +60 GIVING TD-ELAPSED2-MINUTES
00665                                  REMAINDER TD-ELAPSED2-SECONDS.
00386      
      * EXEC CICS ASKTIME
00387 *    END-EXEC.
      *    MOVE '0"                    "   #00003994' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00388
00389      MOVE EIBTIME                TO  WS-TIME.
00390      COMPUTE WS-START-TIME = (WS-HOURS * 3600) +
00391                              (WS-MINUTES * 60) + WS-SECONDS.
00392
00393      MOVE WS-HOURS               TO  TD-START-HOURS.
00394      MOVE WS-MINUTES             TO  TD-START-MINUTES.
00395      MOVE WS-SECONDS             TO  TD-START-SECONDS.
110921     if connected-to-db
110921        exec sql
110921           set connection 'WFCERT'
110921        end-exec
110921
110921        EXEC SQL
110921            commit work release
110921        END-EXEC
110921        if sqlcode not = 0
110921           display "Error: commit release "
110921           display ' sql return code ' sqlcode
110921           display ' sql err mess    ' sqlerrmc
110921        end-if
110921
110921        EXEC SQL
110921           DISCONNECT 'WFCERT'
110921        END-EXEC
110921
110921        if sqlcode not = 0
110921           display "Error: cannot disconnect pdbnk "
110921           display ' sql return code ' sqlcode
110921           display ' sql err mess    ' sqlerrmc
110921        end-if
110921     end-if
00667      
      * EXEC CICS WRITEQ TD
00668 *         QUEUE     ('CSMT')
00669 *         FROM      (TRANS-DATA-MSG)
00670 *         LENGTH    (TRANS-DATA-LENGTH)
00671 *    END-EXEC.
           MOVE 'CSMT' TO DFHEIV5
      *    MOVE '(" L   L              &   #00004028' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRANS-DATA-MSG, 
                 TRANS-DATA-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00672
00673      
      * EXEC CICS WRITEQ TD
00674 *         QUEUE     ('CSMT')
00675 *         FROM      (TRANS-DATA-MSG2)
00676 *         LENGTH    (TRANS-DATA-LENGTH)
00677 *    END-EXEC.
           MOVE 'CSMT' TO DFHEIV5
      *    MOVE '(" L   L              &   #00004034' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRANS-DATA-MSG2, 
                 TRANS-DATA-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00678
00679  1599-EXIT.
00680      EXIT.
110921 1800-work-flow.
110921
110921     if not connected-to-db
110921        move 'HOV-TSTDB01_Workflow'
110921                                 to svr
110921        move 'appuser'           to usr
110921        move 'appuser@cso'       to pass
110921
110921        if ws-kix-myenv = 'cid1p'
110921           move 'SDVDB01_Workflow'
110921                                 to svr
110921           move 'appuser'        to usr
110921           move 'appuser@cso'    to pass
110921        end-if
110921
110921        string
110921            usr delimited space
110921            "." delimited size
110921            pass delimited space into usr-pass
110921        end-string
110921
110921        EXEC SQL
110921           CONNECT
110921              TO :svr AS 'WFCERT'
110921              USER :usr-pass
110921        END-EXEC
110921
110921        if sqlcode not = 0
110921           display "Error: cannot connect "
110921           display sqlcode
110921           display sqlerrmc
110921           
      * goback

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL051' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           goback
110921        end-if
110921        set connected-to-db to true
110921     end-if
110921
110921     move 'CRTS'                 to pa-rec-type
110921     move edit-company-id        to pa-company-id
110921     move pending-business       to pa-rest-of-record
110921
110921     
      * EXEC CICS LINK
110921*        PROGRAM  ('WF001')
110921*        COMMAREA (work-flow-pass-area)
110921*        LENGTH   (604)
110921*    END-EXEC
           MOVE 'WF001' TO DFHEIV1
           MOVE 604
             TO DFHEIV11
      *    MOVE '."C                   (   #00004082' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 work-flow-pass-area, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
110921
110921     .
110921 1800-exit.
110921     exit.
00683  2000-CALL-PNDB-EDIT-PROGRAM.
00684
00685      
      * EXEC CICS  HANDLE CONDITION
00686 *           NOTFND   (2080-WRITE-REPORT)
00687 *    END-EXEC.
      *    MOVE '"$I                   ! # #00004093' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034303933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00688
00689      MOVE PENDING-BUSINESS TO WK-PENDING-BUSINESS.
00690      
      * EXEC CICS LINK
00691 *        PROGRAM    (PNDB-EDIT-PGM)
00692 *        COMMAREA   (PNDB-EDIT-PASS-AREA)
00693 *        LENGTH     (1036)
00694 *    END-EXEC.
           MOVE 1036
             TO DFHEIV11
      *    MOVE '."C                   (   #00004098' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PNDB-EDIT-PGM, 
                 PNDB-EDIT-PASS-AREA, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00695
00696 ***********   IF VALID RECORD HAS NOT BEEN PASSED BACK TO THIS
00697 ***********   PROGRAM, THEN AN ABEND HAS OCCURRED.
00698 ***********   IF ERROR 2617 WAS SENT BACK THEN A FILE IS NOT OPEN
00699 ***********   AND THE EDIT SHOULD NOT CONTINUE.
00700
00701      MOVE WK-PENDING-BUSINESS TO PENDING-BUSINESS.
00702
00703         IF ER-2617               = PB-COMMON-ERROR (1)
00704                                 OR PB-COMMON-ERROR (2)
00705                                 OR PB-COMMON-ERROR (3)
00706                                 OR PB-COMMON-ERROR (4)
00707                                 OR PB-COMMON-ERROR (5)
00708                                 OR PB-COMMON-ERROR (6)
00709                                 OR PB-COMMON-ERROR (7)
00710                                 OR PB-COMMON-ERROR (8)
00711                                 OR PB-COMMON-ERROR (9)
00712                                 OR PB-COMMON-ERROR (10)
00713
00714         GO TO 9999-RETURN-CICS.
00715
00716      IF NOT VALID-PB-ID
00717         MOVE PNDB-EDIT-PASS-AREA TO RF-DATA-133
00718         MOVE '1'                 TO RF-CTL-CHAR-133
00719         MOVE EDIT-COMPANY-CD     TO RF-COMPANY-CD
00720         MOVE '1'                 TO RF-RECORD-TYPE
00721         MOVE 'EL051'             TO RF-REPORT-ID
00722         ADD +1                   TO WS-LINE-NUMBER
00723         PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT
00724         MOVE '2'                 TO RF-RECORD-TYPE
00725         MOVE +1                  TO RF-LINE-NUMBER
00726         MOVE 'ABEND'             TO RF-CURRENT-DATE
00727         
      * EXEC CICS READ
00728 *            DATASET  ('ELREPT')
00729 *            INTO     (REPORT-SAVE-FILE)
00730 *            RIDFLD   (RF-CONTROL-PRIMARY)
00731 *            UPDATE
00732 *       END-EXEC
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&"IL       EU         (   #00004135' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00733         MOVE 'ABEND'             TO RF-CURRENT-DATE
00734         
      * EXEC CICS REWRITE
00735 *            DATASET  ('ELREPT')
00736 *            FROM     (REPORT-SAVE-FILE)
00737 *       END-EXEC
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&& L                  %   #00004142' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00738         GO TO 9999-RETURN-CICS.
00739
00740      GO TO 2090-EXIT.
00741
00742  2080-WRITE-REPORT.
00743
00744      
      * EXEC CICS WRITE
00745 *         DATASET  ('ELREPT')
00746 *         RIDFLD   (RF-CONTROL-PRIMARY)
00747 *         FROM     (REPORT-SAVE-FILE)
00748 *    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00004152' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00749
00750      GO TO 9999-RETURN-CICS.
00751
00752  2090-EXIT.
00753      EXIT.
00754
00755      EJECT
00756
00757  2100-CALL-PNDC-EDIT-PROGRAM.
00758      
      * EXEC CICS LINK
00759 *        PROGRAM    (PNDC-EDIT-PGM)
00760 *        COMMAREA   (PNDC-EDIT-PASS-AREA)
00761 *        LENGTH     (553)
00762 *    END-EXEC.
           MOVE 553
             TO DFHEIV11
      *    MOVE '."C                   (   #00004166' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PNDC-EDIT-PGM, 
                 PNDC-EDIT-PASS-AREA, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00763
00764      IF NOT VALID-PC-ID
00765         MOVE PNDC-EDIT-PASS-AREA TO RF-DATA-133
00766         MOVE '1'                 TO RF-CTL-CHAR-133
00767         MOVE EDIT-COMPANY-CD     TO RF-COMPANY-CD
00768         MOVE '1'                 TO RF-RECORD-TYPE
00769         MOVE 'EL053'             TO RF-REPORT-ID
00770         ADD +1 TO RF-LINE-NUMBER
00771         PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT
00772         MOVE '2'                 TO RF-RECORD-TYPE
00773         MOVE +1                  TO RF-LINE-NUMBER
00774         
      * EXEC CICS READ
00775 *            DATASET  ('ELREPT')
00776 *            INTO     (REPORT-SAVE-FILE)
00777 *            RIDFLD   (RF-CONTROL-PRIMARY)
00778 *            UPDATE
00779 *       END-EXEC
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&"IL       EU         (   #00004182' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00780         MOVE 'ABEND'   TO RF-CURRENT-DATE
00781         
      * EXEC CICS REWRITE
00782 *            DATASET  ('ELREPT')
00783 *            FROM     (REPORT-SAVE-FILE)
00784 *       END-EXEC
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
           MOVE 'ELREPT' TO DFHEIV1
      *    MOVE '&& L                  %   #00004189' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00785         GO TO 9999-RETURN-CICS.
00786
00787  2190-EXIT.
00788      EXIT.
00789      EJECT
00790  3000-READ-CONTROL-FILE.
00791      
      * EXEC CICS HANDLE CONDITION
00792 *        NOTFND (9999-RETURN-CICS)
00793 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00004199' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034313939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00794
00795      MOVE SPACES                 TO ELCNTL-KEY.
00796      MOVE '1'                    TO ELCNTL-REC-TYPE.
00797      MOVE EDIT-COMPANY-ID        TO ELCNTL-COMPANY-ID.
00798
00799      MOVE +0                     TO ELCNTL-SEQ-NO.
00800
00801      
      * EXEC CICS READ
00802 *        DATASET (ELCNTL-FILE-ID)
      *        into (control-file)
00803 *        SET (ADDRESS OF CONTROL-FILE)
00804 *        RIDFLD (ELCNTL-KEY)
00805 *    END-EXEC.
           MOVE LENGTH OF
            control-file
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00004209' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 control-file, 
                 DFHEIV11, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00806
00807      MOVE CF-LGX-CLAIM-USER      TO WS-LGX-CLAIM-USER.
00808  3090-EXIT.
00809      EXIT.
00810      EJECT
00811  3200-READ-NEXT-RECORD.
00812      IF ERPNDB-SEQ-NO = 9999
00813         MOVE HIGH-VALUES         TO ERPNDB-SEQ-XX
00814        ELSE
00815         ADD +1                   TO ERPNDB-SEQ-NO.
00816
00817      
      * EXEC CICS HANDLE CONDITION
00818 *        NOTFND (3210-END-OF-FILE)
00819 *    END-EXEC.
      *    MOVE '"$I                   ! % #00004226' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034323236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00820
00821      
      * EXEC CICS READ
00822 *        INTO    (PENDING-BUSINESS)
00823 *        INTO    (PNDB-EDIT-PASS-AREA)
00824 *        DATASET (ERPNDB-FILE-ID)
00825 *        RIDFLD  (ERPNDB-KEY)
00826 *        GTEQ
00827 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&"IL       G          (   #00004230' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00828
00829      IF PB-COMPANY-CD NOT = ERPNDB-COMPANY-CD
00830          IF PROCESS-COMPANY
00831              MOVE 'Y'            TO ERPNDB-FILE-SW
00832              GO TO 3290-EXIT
00833          ELSE
00834              MOVE 'X'            TO ERPNDB-FILE-SW
00835              GO TO 3290-EXIT.
00836
00837      ADD +1                      TO PNDB-REC-COUNT.
00838      IF PNDB-REC-COUNT IS GREATER THAN +30
00839          MOVE +0                 TO PNDB-REC-COUNT
00840 *        EXEC CICS DELAY
00841 *            INTERVAL  (WS-DELAY-INTERVAL)
00842 *        END-EXEC
           end-if
00843
00844      IF FIRST-TIME
00845          MOVE PB-CONTROL-PRIMARY TO ERPNDB-KEY
00846          MOVE SPACE              TO FIRST-TIME-SW.
00847
00848      MOVE PB-CONTROL-PRIMARY     TO ERPNDB-KEY.
00849
00850      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.
00851
00852      IF  WS-BATCH-PREFIX = '#CL'
00853          MOVE 9999               TO ERPNDB-SEQ-NO
00854          GO TO 3200-READ-NEXT-RECORD.
00855
00856      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES
00857         IF EDIT-RESTART-BATCH NOT = 'REEDIT'
00858            GO TO 3200-READ-NEXT-RECORD.
00859
00860      IF PB-BILLED-DT NOT EQUAL LOW-VALUES
00861         GO TO 3200-READ-NEXT-RECORD.
00862
00863      MOVE 'Y'                    TO DATA-EDITED-SW.
00864      
      * EXEC CICS READ
00865 *        DATASET (ERPNDB-FILE-ID)
00866 *        INTO    (PENDING-BUSINESS)
00867 *        INTO    (PNDB-EDIT-PASS-AREA)
00868 *        RIDFLD  (ERPNDB-KEY)
00869 *        UPDATE
00870 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00004274' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00871
00872      GO TO 3290-EXIT.
00873
00874  3210-END-OF-FILE.
00875      MOVE 'Y'                    TO ERPNDB-FILE-SW.
00876
00877  3290-EXIT.
00878      EXIT.
00879      EJECT
00880  3300-READ-NEXT-RECORD.
00881      ADD +1                      TO ERPNDC-SEQ-NO.
00882      
      * EXEC CICS HANDLE CONDITION
00883 *        NOTFND (3310-END-OF-FILE)
00884 *    END-EXEC.
      *    MOVE '"$I                   ! & #00004292' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034323932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00885
00886      
      * EXEC CICS READ
00887 *        INTO    (PNDC-EDIT-PASS-AREA)
00888 *        DATASET (ERPNDC-FILE-ID)
00889 *        RIDFLD  (ERPNDC-KEY)
00890 *        GTEQ
00891 *    END-EXEC.
           MOVE LENGTH OF
            PNDC-EDIT-PASS-AREA
             TO DFHEIV11
      *    MOVE '&"IL       G          (   #00004296' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 PNDC-EDIT-PASS-AREA, 
                 DFHEIV11, 
                 ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00892
00893      IF FIRST-TIME
00894          MOVE PC-CONTROL-PRIMARY TO ERPNDC-KEY
00895          MOVE SPACE              TO FIRST-TIME-SW.
00896
00897      IF PC-COMPANY-CD NOT = ERPNDC-COMPANY-CD
00898          IF PROCESS-COMPANY
00899              MOVE 'Y'            TO ERPNDC-FILE-SW
00900              GO TO 3390-EXIT
00901          ELSE
00902              MOVE 'X'            TO ERPNDC-FILE-SW
00903              GO TO 3390-EXIT.
00904
00905      ADD +1                      TO PNDB-REC-COUNT.
00906      IF PNDB-REC-COUNT IS GREATER THAN +30
00907          MOVE +0                 TO PNDB-REC-COUNT
00908          
      * EXEC CICS DELAY
00909 *            INTERVAL  (WS-DELAY-INTERVAL)
00910 *        END-EXEC.
      *    MOVE '0$I                   &   #00004318' TO DFHEIV0
           MOVE X'302449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DELAY-INTERVAL, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00911
00912      MOVE PC-CONTROL-PRIMARY     TO ERPNDC-KEY.
00913
00914      IF PC-CREDIT-ACCEPT-DT NOT = LOW-VALUES
00915          GO TO 3300-READ-NEXT-RECORD.
00916
00917      MOVE 'Y'                    TO DATA-EDITED-SW.
00918      
      * EXEC CICS READ
00919 *        DATASET (ERPNDC-FILE-ID)
00920 *        INTO    (PNDC-EDIT-PASS-AREA)
00921 *        RIDFLD  (ERPNDC-KEY)
00922 *        UPDATE
00923 *    END-EXEC.
           MOVE LENGTH OF
            PNDC-EDIT-PASS-AREA
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00004328' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 PNDC-EDIT-PASS-AREA, 
                 DFHEIV11, 
                 ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00924
00925      GO TO 3390-EXIT.
00926
00927  3310-END-OF-FILE.
00928      MOVE 'Y'                    TO ERPNDC-FILE-SW.
00929
00930  3390-EXIT.
00931      EXIT.
00932      EJECT
00933  3400-WRITE-REPORT-RECORD.
00934
00935      IF FILES-NOT-OPEN
00936         NEXT SENTENCE
00937      ELSE
00938        IF NOT REEDIT-OR-RESTART-OR-FULL
00939           GO TO 3490-EXIT.
00940
00941      MOVE 'RF'                   TO RF-RECORD-ID.
00942      MOVE WS-LINE-NUMBER         TO RF-LINE-NUMBER.
00943
00944      
      * EXEC CICS WRITE
00945 *        DATASET (ELREPT-FILE-ID)
00946 *        FROM    (REPORT-SAVE-FILE)
00947 *        RIDFLD  (RF-CONTROL-PRIMARY)
00948 *    END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004354' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00949
00950  3490-EXIT.
00951       EXIT.
00952      EJECT
00953  3500-INCREMENT-ERROR-COUNTERS.
00954      IF EMI-SEVERITY-SAVE = 'W'
00955          ADD 1                   TO EMI-WARNING-CTR
00956      ELSE
00957          IF EMI-SEVERITY-SAVE = 'F'
00958              ADD 1               TO EMI-FORCABLE-CTR
00959          ELSE
00960              IF EMI-SEVERITY-SAVE = 'X'
00961                  ADD 1           TO EMI-FATAL-CTR.
00962
00963  3500-EXIT.
00964      EXIT.
00965      EJECT
00966  4000-SET-PNDB-ERROR-FLAGS.
00967
00968      IF PB-COMMON-ERRORS = LOW-VALUES OR
00969         PB-BATCH-TRAILER
00970         GO TO 4500-REWRITE-RECORD.
00971
00972  4010-FORMAT-ERRORS.
00973
00974      MOVE +0                     TO SUB1.
00975
00976  4100-ERROR-LOOP.
00977
00978      ADD +1                      TO SUB1.
00979
00980      IF SUB1 GREATER THAN PB-NO-OF-ERRORS
00981         GO TO 4200-SET-ERROR-FLAGS.
00982
00983      MOVE PB-COMMON-ERROR (SUB1) TO EMI-ERROR.
00984      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00985
00986      GO TO 4100-ERROR-LOOP.
00987
00988
00989  4200-SET-ERROR-FLAGS.
00990
00991      IF EMI-FATAL-CTR NOT = ZEROS
00992         MOVE 'X'                 TO PB-FATAL-FLAG
00993         GO TO 4300-SET-ERROR-FLAGS.
00994
00995      IF EMI-FORCABLE-CTR NOT = ZEROS
00996         IF PB-ISSUE
00997            IF PB-ISSUE-FORCE
00998               MOVE 'F'           TO PB-FORCE-ER-CD
00999              ELSE
01000               MOVE 'X'           TO PB-FORCE-ER-CD
01001           ELSE
01002            IF PB-CANCEL-FORCE
01003               MOVE 'F'           TO PB-FORCE-ER-CD
01004              ELSE
01005               MOVE 'X'           TO PB-FORCE-ER-CD.
01006
01007  4300-SET-ERROR-FLAGS.
01008
01009      IF EMI-WARNING-CTR NOT = ZEROS
01010         MOVE 'W'                 TO PB-WARN-ER-CD.
01011
01012      IF PB-UNFORCED-ERRORS OR
01013         PB-FATAL-ERRORS    OR
01014         PB-RECORD-ON-HOLD  OR
01015         PB-RECORD-RETURNED OR
01016         PB-CANCELLATION
01017           NEXT SENTENCE
01018         ELSE
01019           GO TO 4500-REWRITE-RECORD.
01020
01021
01022      
      * EXEC CICS  HANDLE CONDITION
01023 *          NOTFND    (4500-REWRITE-RECORD)
01024 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00004432' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034343332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01025
01026      MOVE PB-CONTROL-BY-ACCOUNT  TO CERT-KEY.
01027      MOVE PB-SV-CARRIER          TO CERT-CARRIER.
01028      MOVE PB-SV-GROUPING         TO CERT-GROUPING.
01029      MOVE PB-SV-STATE            TO CERT-STATE.
01030      
      * EXEC CICS READ
      *        into    (certificate-master)
01031 *        SET     (ADDRESS OF CERTIFICATE-MASTER)
01032 *        DATASET (CERT-ID)
01033 *        RIDFLD  (CERT-KEY)
01034 *        UPDATE
01035 *    END-EXEC.
           MOVE LENGTH OF
            certificate-master
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00004440' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-ID, 
                 certificate-master, 
                 DFHEIV11, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01036
01037      IF CERT-ADDED-BATCH OR CERT-PURGED-OFFLINE
01038         GO TO 4490-REWRITE-CERT-MASTER.
01039
01040      IF PB-ISSUE
01041         IF  PB-RECORD-RETURNED
01042             MOVE '4'             TO CM-CREDIT-INTERFACE-SW-1
01043         ELSE
01044              MOVE '2'            TO CM-CREDIT-INTERFACE-SW-1.
01045
01046      IF PB-CANCELLATION
01047         IF  PB-RECORD-RETURNED
01048             MOVE '7'             TO CM-CREDIT-INTERFACE-SW-2
01049         ELSE
01050             IF PB-C-LF-CANCEL-VOIDED OR PB-C-AH-CANCEL-VOIDED
01051                MOVE '6'          TO CM-CREDIT-INTERFACE-SW-2
01052             ELSE
01053                MOVE '4'          TO CM-CREDIT-INTERFACE-SW-2.
01054
01055  4490-REWRITE-CERT-MASTER.
01056      
      * EXEC CICS REWRITE
01057 *         DATASET    (CERT-ID)
01058 *         FROM       (CERTIFICATE-MASTER)
01059 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004467' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01060
01061  4500-REWRITE-RECORD.
01062      IF NOT PB-BATCH-TRAILER
01063         MOVE PB-CSR-ID               TO WS-CSR-ID
01064          IF PB-ISSUE
01065              ADD PB-I-LF-PREM-CALC   TO WS-LF-ISS-COMPUTED
01066              ADD PB-I-LF-ALT-PREM-CALC   TO WS-LF-ISS-COMPUTED
01067              ADD PB-I-LF-PREMIUM-AMT TO WS-LF-ISS-ENTERED
01068              ADD PB-I-LF-ALT-PREMIUM-AMT TO WS-LF-ISS-ENTERED
01069              ADD PB-I-AH-PREM-CALC   TO WS-AH-ISS-COMPUTED
01070              ADD PB-I-AH-PREMIUM-AMT TO WS-AH-ISS-ENTERED
01071              ADD 1                   TO WS-ISSUE-CNT
01072          ELSE
01073              ADD PB-C-LF-REF-CALC    TO WS-LF-CAN-COMPUTED
01074              ADD PB-C-LF-CANCEL-AMT  TO WS-LF-CAN-ENTERED
01075              ADD PB-C-AH-REF-CALC    TO WS-AH-CAN-COMPUTED
01076              ADD PB-C-AH-CANCEL-AMT  TO WS-AH-CAN-ENTERED
01077              ADD 1                   TO WS-CANCEL-CNT
01078      ELSE
01079          MOVE WS-CSR-ID              TO PB-CSR-ID
01080          MOVE WS-LF-ISS-COMPUTED     TO PB-B-LF-ISS-PRM-COMPUTED
01081          MOVE WS-LF-ISS-ENTERED      TO PB-B-LF-ISS-PRM-ENTERED
01082          MOVE WS-AH-ISS-COMPUTED     TO PB-B-AH-ISS-PRM-COMPUTED
01083          MOVE WS-AH-ISS-ENTERED      TO PB-B-AH-ISS-PRM-ENTERED
01084          MOVE WS-LF-CAN-COMPUTED     TO PB-B-LF-CAN-PRM-COMPUTED
01085          MOVE WS-LF-CAN-ENTERED      TO PB-B-LF-CAN-PRM-ENTERED
01086          MOVE WS-AH-CAN-COMPUTED     TO PB-B-AH-CAN-PRM-COMPUTED
01087          MOVE WS-AH-CAN-ENTERED      TO PB-B-AH-CAN-PRM-ENTERED
01088          MOVE WS-ISSUE-CNT           TO PB-B-ISSUE-CNT-ENTERED
01089          MOVE WS-CANCEL-CNT          TO PB-B-CANCEL-CNT-ENTERED
01090          MOVE ZEROS                  TO WS-LF-ISS-COMPUTED
01091                                         WS-LF-ISS-ENTERED
01092                                         WS-AH-ISS-COMPUTED
01093                                         WS-AH-ISS-ENTERED
01094                                         WS-LF-CAN-COMPUTED
01095                                         WS-LF-CAN-ENTERED
01096                                         WS-AH-CAN-COMPUTED
01097                                         WS-AH-CAN-ENTERED
01098                                         WS-ISSUE-CNT
01099                                         WS-CANCEL-CNT
01100          MOVE SPACE                  TO PB-FATAL-FLAG
01101          IF PROCESS-BATCH
01102              MOVE 'Y'            TO EDIT-PROCESS-SW.
01103
01104      IF PB-BATCH-TRAILER
01105          IF PB-B-LF-ISS-PRM-REMITTED =
01106             PB-B-LF-ISS-PRM-ENTERED   AND
01107             PB-B-LF-CAN-PRM-REMITTED =
01108             PB-B-LF-CAN-PRM-ENTERED   AND
01109             PB-B-AH-ISS-PRM-REMITTED =
01110             PB-B-AH-ISS-PRM-ENTERED   AND
01111             PB-B-AH-CAN-PRM-REMITTED =
01112             PB-B-AH-CAN-PRM-ENTERED   AND
01113             PB-B-ISSUE-CNT-REMITTED  =
01114             PB-B-ISSUE-CNT-ENTERED    AND
01115             PB-B-CANCEL-CNT-REMITTED =
01116             PB-B-CANCEL-CNT-ENTERED
01117              MOVE SPACE          TO PB-OUT-BAL-CD
01118      ELSE
01119          MOVE 'O'                TO PB-OUT-BAL-CD.
01120
01121      MOVE 'C'                    TO JP-RECORD-TYPE.
01122      MOVE ERPNDB-FILE-ID         TO JP-FILE-ID.
01123 *    MOVE PNDB-EDIT-PASS-AREA    TO JP-RECORD-AREA.
01124      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
01125      COMPUTE JOURNAL-LENGTH = ERPNDB-LENGTH + 23.
01126
01127      
      * EXEC CICS REWRITE
01128 *        DATASET (ERPNDB-FILE-ID)
01129 *        FROM    (PNDB-EDIT-PASS-AREA)
01130 *        FROM    (PENDING-BUSINESS)
01131 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004538' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01132
01133      PERFORM 8400-LOG-JOURNAL-RECORD.
01134      MOVE ZEROS                  TO EMI-WARNING-CTR
01135                                     EMI-FORCABLE-CTR
01136                                     EMI-FATAL-CTR.
01137
01138  4900-EXIT.
01139      EXIT.
01140      EJECT
01141  5000-SET-PNDC-ERROR-FLAGS.
01142      IF PC-ERROR-FLAGS = SPACES
01143          GO TO 5500-REWRITE-RECORD.
01144
01145      MOVE LIT-2800               TO WS-ERR-CODE.
01146      MOVE 1  TO SUB.
01147
01148  5100-ERR-LOOP.
01149      IF PC-ERR-FLAG (SUB) NOT = SPACES
01150          MOVE SUB                TO WS-ERROR-SUB
01151          MOVE WS-ERR-CODE        TO EMI-ERROR
01152          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01153          PERFORM 3500-INCREMENT-ERROR-COUNTERS THRU 3500-EXIT.
01154
01155      IF EMI-FATAL-CTR NOT = ZEROS
01156          GO TO 5200-SET-ERROR-FLAGS.
01157
01158      ADD 1   TO SUB.
01159      IF SUB LESS THAN 101
01160          GO TO 5100-ERR-LOOP.
01161
01162  5200-SET-ERROR-FLAGS.
01163      IF EMI-FATAL-CTR NOT = ZEROS
01164          MOVE 'X'                TO PC-FATAL-FLAG.
01165
01166      IF EMI-FORCABLE-CTR NOT = ZEROS
01167          IF PC-CLAIM-FORCE
01168              MOVE 'F'            TO PC-FORCE-ER-CD
01169          ELSE
01170              MOVE 'X'            TO PC-FORCE-ER-CD.
01171
01172      IF EMI-WARNING-CTR NOT = ZEROS
01173          MOVE 'W'                TO PC-WARN-ER-CD.
01174
01175  5500-REWRITE-RECORD.
01176      MOVE 'C'                    TO JP-RECORD-TYPE.
01177      MOVE ERPNDC-FILE-ID         TO JP-FILE-ID.
01178      MOVE PNDC-EDIT-PASS-AREA    TO JP-RECORD-AREA.
01179      COMPUTE JOURNAL-LENGTH = ERPNDC-LENGTH + 23.
01180      
      * EXEC CICS REWRITE
01181 *        DATASET (ERPNDC-FILE-ID)
01182 *        FROM    (PNDC-EDIT-PASS-AREA)
01183 *    END-EXEC.
           MOVE LENGTH OF
            PNDC-EDIT-PASS-AREA
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004591' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 PNDC-EDIT-PASS-AREA, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01184
01185      PERFORM 8400-LOG-JOURNAL-RECORD.
01186
01187      MOVE ZEROS                  TO EMI-WARNING-CTR
01188                                     EMI-FORCABLE-CTR
01189                                     EMI-FATAL-CTR.
01190
01191  5900-EXIT.
01192      EXIT.
01193      EJECT
01194  5950-UPDATE-TYPE-2.
01195      IF NOT REEDIT-OR-RESTART-OR-FULL
01196         GO TO 5959-EXIT.
01197
01198      
      * EXEC CICS  HANDLE CONDITION
01199 *           NOTFND   (5955-CONTINUE)
01200 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00004609' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034363039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01201
01202      MOVE ERPNDB-COMPANY-CD TO RF-COMPANY-CD.
01203      MOVE 'RF'                   TO RF-RECORD-ID.
01204      MOVE '2'                    TO RF-RECORD-TYPE.
01205      MOVE 'EL051'                TO RF-REPORT-ID.
01206      
      * EXEC CICS DELETE
01207 *        DATASET (ELREPT-FILE-ID)
01208 *        RIDFLD  (RF-CONTROL-PRIMARY)
01209 *        KEYLENGTH (7)
01210 *        GENERIC
01211 *    END-EXEC.
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00004617' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01212
01213  5955-CONTINUE.
01214      MOVE EIBTIME                TO WS-TIME.
01215      MOVE SPACES                 TO RF-TRAILER-RECORD.
01216      MOVE 'STARTED'              TO RF-CURRENT-DATE.
01217      MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.
01218      MOVE '2'                    TO RF-RECORD-TYPE.
01219      MOVE ERPNDB-COMPANY-CD      TO RF-COMPANY-CD.
01220      MOVE 'EL051'                TO RF-REPORT-ID.
01221      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.
01222
01223  5959-EXIT.
01224      EXIT.
01225      EJECT
01226  6000-WRITE-EL051-MESSAGE.
01227      
      * EXEC CICS  HANDLE CONDITION
01228 *           NOTFND   (6010-DELETE-TYPE-2)
01229 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00004638' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303034363338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01230
01231      MOVE ERPNDB-COMPANY-CD TO RF-COMPANY-CD.
01232      MOVE 'RF'                   TO RF-RECORD-ID.
01233      MOVE '1'                    TO RF-RECORD-TYPE.
01234      MOVE 'EL051'                TO RF-REPORT-ID.
01235      
      * EXEC CICS DELETE
01236 *        DATASET (ELREPT-FILE-ID)
01237 *        RIDFLD  (RF-CONTROL-PRIMARY)
01238 *        KEYLENGTH (7)
01239 *        GENERIC
01240 *    END-EXEC.
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00004646' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01241
01242  6010-DELETE-TYPE-2.
01243      
      * EXEC CICS  HANDLE CONDITION
01244 *           NOTFND   (6020-CONTINUE)
01245 *    END-EXEC.
      *    MOVE '"$I                   ! * #00004654' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303034363534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01246
01247      MOVE ERPNDB-COMPANY-CD TO RF-COMPANY-CD.
01248      MOVE 'RF'                   TO RF-RECORD-ID.
01249      MOVE '2'                    TO RF-RECORD-TYPE.
01250      MOVE 'EL051'                TO RF-REPORT-ID.
01251      
      * EXEC CICS DELETE
01252 *        DATASET (ELREPT-FILE-ID)
01253 *        RIDFLD  (RF-CONTROL-PRIMARY)
01254 *        KEYLENGTH (7)
01255 *        GENERIC
01256 *    END-EXEC.
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00004662' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01257
01258  6020-CONTINUE.
01259      MOVE EIBTIME                TO WS-TIME.
01260      MOVE SPACES                 TO RF-TRAILER-RECORD.
01261      MOVE 'STARTED'              TO RF-CURRENT-DATE.
01262      MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.
01263      MOVE '2'                    TO RF-RECORD-TYPE.
01264      MOVE ERPNDB-COMPANY-CD      TO RF-COMPANY-CD.
01265      MOVE 'EL051'                TO RF-REPORT-ID.
01266      MOVE +1                     TO WS-LINE-NUMBER.
01267      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.
01268
01269  6999-EXIT.
01270      EXIT.
01271      EJECT
01272  7000-WRITE-EL053-MESSAGE.
01273      
      * EXEC CICS  HANDLE CONDITION
01274 *           NOTFND   (7010-DELETE-TYPE-2)
01275 *    END-EXEC.
      *    MOVE '"$I                   ! + #00004684' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303034363834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01276
01277      MOVE ERPNDC-COMPANY-CD TO RF-COMPANY-CD.
01278      MOVE 'RF'                   TO RF-RECORD-ID.
01279      MOVE '1'                    TO RF-RECORD-TYPE.
01280      MOVE 'EL053'                TO RF-REPORT-ID.
01281      
      * EXEC CICS DELETE
01282 *        DATASET (ELREPT-FILE-ID)
01283 *        RIDFLD  (RF-CONTROL-PRIMARY)
01284 *        KEYLENGTH (7)
01285 *        GENERIC
01286 *    END-EXEC.
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00004692' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01287
01288  7010-DELETE-TYPE-2.
01289      
      * EXEC CICS  HANDLE CONDITION
01290 *           NOTFND   (7020-CONTINUE)
01291 *    END-EXEC.
      *    MOVE '"$I                   ! , #00004700' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303034373030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01292
01293      MOVE ERPNDC-COMPANY-CD TO RF-COMPANY-CD.
01294      MOVE 'RF'                   TO RF-RECORD-ID.
01295      MOVE '2'                    TO RF-RECORD-TYPE.
01296      MOVE 'EL053'                TO RF-REPORT-ID.
01297      
      * EXEC CICS DELETE
01298 *        DATASET (ELREPT-FILE-ID)
01299 *        RIDFLD  (RF-CONTROL-PRIMARY)
01300 *        KEYLENGTH (7)
01301 *        GENERIC
01302 *    END-EXEC.
           MOVE 7
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00004708' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELREPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01303
01304  7020-CONTINUE.
01305      MOVE EIBTIME                TO WS-TIME.
01306      MOVE EDIT-COMPANY-CD        TO RF-COMPANY-CD.
01307      MOVE SPACES                 TO RF-TRAILER-RECORD.
01308      MOVE WS-CURRENT-DATE        TO RF-CURRENT-DATE.
01309      MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.
01310      MOVE '2'                    TO RF-RECORD-TYPE.
01311      MOVE 'EL053'                TO RF-REPORT-ID.
01312      MOVE +1                     TO WS-LINE-NUMBER.
01313      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.
01314
01315  7999-EXIT.
01316      EXIT.
01317      EJECT
01318  8300-ABEND.
01319      MOVE DFHEIBLK TO EMI-LINE1.
01320      
      * EXEC CICS LINK
01321 *        PROGRAM   ('EL004')
01322 *        COMMAREA  (EMI-LINE1)
01323 *        LENGTH    (72)
01324 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00004731' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01325
01326      GO TO 9999-RETURN-CICS.
01327
01328  8400-LOG-JOURNAL-RECORD.
01329      MOVE 'EDIT'                 TO JP-USER-ID.
01330      MOVE THIS-PGM               TO JP-PROGRAM-ID.
01331 *    EXEC CICS JOURNAL
01332 *        JFILEID     (1)
01333 *        JTYPEID     ('EL')
01334 *        FROM        (JOURNAL-RECORD)
01335 *        LENGTH      (JOURNAL-LENGTH)
01336 *        END-EXEC.
01337
01338  8500-DATE-CONVERT.
01339      MOVE LINK-ELDATCV           TO PGM-NAME.
01340      
      * EXEC CICS LINK
01341 *        PROGRAM    (PGM-NAME)
01342 *        COMMAREA   (DATE-CONVERSION-DATA)
01343 *        LENGTH     (DC-COMM-LENGTH)
01344 *    END-EXEC.
      *    MOVE '."C                   (   #00004751' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01345  8500-EXIT.
01346      EXIT.
01347
01348      EJECT
01349
01350  9900-ERROR-FORMAT.
01351      MOVE EDIT-COMPANY-ID        TO EMI-CLIENT-ID.
01352      MOVE LINK-001               TO PGM-NAME
01353      
      * EXEC CICS LINK
01354 *        PROGRAM    (PGM-NAME)
01355 *        COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01356 *        LENGTH     (EMI-COMM-LENGTH)
01357 *    END-EXEC.
      *    MOVE '."C                   (   #00004764' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01358  9900-EXIT.
01359      EXIT.
01360
01361  9999-RETURN-CICS.
01362      
      * EXEC CICS DEQ
01363 *         RESOURCE   (EDIT-COMPANY-CD)
01364 *         LENGTH     (1)
01365 *    END-EXEC.
           MOVE 1
             TO DFHEIV11
      *    MOVE '2&L                   $   #00004773' TO DFHEIV0
           MOVE X'32264C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EDIT-COMPANY-CD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01366
01367      
      * EXEC CICS  RETURN
01368 *    END-EXEC.
      *    MOVE '.(                    ''   #00004778' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01369
01370  9999-EXIT.
01371       EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL051' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8300-ABEND,
                     9900-ERROR-FORMAT,
                     0210-PROCESS-FILE,
                     0210-PROCESS-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 2080-WRITE-REPORT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 9999-RETURN-CICS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3210-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3310-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 4500-REWRITE-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 5955-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 6010-DELETE-TYPE-2
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 6020-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7010-DELETE-TYPE-2
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 7020-CONTINUE
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL051' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
