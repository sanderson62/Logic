110921$SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
00001  IDENTIFICATION DIVISION.                                         02/21/97
00002                                                                   EL051
00003  PROGRAM-ID.                 EL051 .                                 LV013
00004 *              PROGRAM CONVERTED BY                                  CL*11
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*11
00006 *              CONVERSION DATE 03/13/95 13:23:45.                    CL*11
00007 *                            VMOD=2.013                              CL*13
00008 *                                                                 EL051
00008 *                                                                 EL051
00009 *AUTHOR.     LOGIC INC.                                              CL*11
00010 *            DALLAS, TEXAS.                                          CL*11
00011                                                                   EL051
00012 *DATE-COMPILED.                                                      CL*11
00013 *SECURITY.   *****************************************************   CL*11
00014 *            *                                                   *   CL*11
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*11
00016 *            *                                                   *   CL*11
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*11
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*11
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*11
00020 *            *                                                   *   CL*11
00021 *            *****************************************************   CL*11
00022                                                                   EL051
00023 *REMARKS.    TRANSACTION - EXSE AND EXEB                             CL**4
00024 *         THIS PROGRAM IS STARTED  FROM EL630 AND CICS.  IT'S        CL**4
00025 *         FUNCTION IS TO FEED PENDING BUSINESS RECORDS TO            CL**4
00026 *         THE EDIT PROGRAM.                                          CL**4
00027                                                                   EL051
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
00029  ENVIRONMENT DIVISION.                                            EL051
00030  DATA DIVISION.                                                   EL051
00031  WORKING-STORAGE SECTION.                                         EL051
00032  77  FILLER  PIC X(32) VALUE '********************************'.  EL051
00033  77  FILLER  PIC X(32) VALUE '*     EL051  WORKING-STORAGE   *'.  EL051
00034  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.013 *********'.     CL*13
00035                                                                   EL051
00036  77  ELEN                        PIC S9(4)  COMP    VALUE +16.    EL051
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

00038  01  WORK-AREAS.                                                  EL051
110921     12  ws-connect-sw               pic x  value ' '.
110921         88  connected-to-db             value 'Y'.
00039      12  ER-2617                 PIC 9(4)    VALUE 2617.             CL**8
00040      12  WS-CSR-ID               PIC X(4)    VALUE SPACES.           CL**8
00041      12  WS-PREV-BATCH           PIC X(6)    VALUE SPACES.        EL051
00042      12  LIT-2600                PIC 9(4)    VALUE 2600.          EL051
00043      12  LIT-2625                PIC 9(4)    VALUE 2625.          EL051
00044      12  LIT-2725                PIC 9(4)    VALUE 2725.          EL051
00045      12  LIT-2800                PIC 9(4)    VALUE 2800.          EL051
00046      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL051
00047      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL051
00048      12  ELREPT-FILE-ID          PIC X(8)    VALUE 'ELREPT'.      EL051
00049      12  ERPNDB-FILE-ID          PIC X(8)    VALUE 'ERPNDB'.      EL051
00050      12  ERPNDC-FILE-ID          PIC X(8)    VALUE 'ERPNDC'.      EL051
00051      12  ELCNTL-FILE-ID          PIC X(8)    VALUE 'ELCNTL'.      EL051
00052      12  CERT-ID                 PIC X(8)    VALUE 'ELCERT'.      EL051
00053      12  PNDB-EDIT-PGM           PIC X(8)    VALUE 'EL050'.       EL051
00054      12  PNDC-EDIT-PGM           PIC X(8)    VALUE 'EL053'.       EL051
00055      12  PGM-NAME                PIC X(8)    VALUE SPACES.        EL051
00056      12  THIS-PGM                PIC X(8)    VALUE 'EL051'.       EL051
00057      12  ERPNDB-LENGTH           PIC S9(4)   COMP VALUE +585.     EL051
00058      12  ERPNDC-LENGTH           PIC S9(4)   COMP VALUE +500.     EL051
00059      12  JOURNAL-LENGTH          PIC S9(4)   COMP VALUE +0.       EL051
00060      12  TRANS-DATA-LENGTH       PIC S9(4)   COMP VALUE +100.     EL051
00061      12  REC-COUNT               PIC S9(6)   COMP-3 VALUE +0.     EL051
00062      12  WS-WORK                 PIC S9(3)   COMP-3 VALUE +0.     EL051
00063      12  WS-REM                  PIC S9(6)   COMP-3 VALUE +0.     EL051
00064      12  PRT-CNT                 PIC 9       VALUE 4.             EL051
00065      12  WS-LINE-NUMBER          PIC 9(5)    COMP-3 VALUE ZERO.   EL051
00066      12  WS-PAGE                 PIC 9(5)    COMP-3 VALUE ZERO.   EL051
00067      12  WS-CURRENT-DATE         PIC X(8).                        EL051
00068      12  SUB                     PIC 999     COMP-3 VALUE ZEROS.  EL051
00069      12  SUB1                    PIC 999     COMP-3 VALUE ZEROS.     CL**8
00070      12  WS-SYNC-CNTR            PIC 999     COMP-3 VALUE ZEROS.  EL051
00071      12  WS-TIME-WORK            PIC S9(11)  COMP-3 VALUE ZERO.   EL051
00072      12  WS-START-TIME           PIC S9(11)  COMP-3 VALUE ZERO.   EL051
00073      12  WS-STOP-TIME            PIC S9(11)  COMP-3 VALUE ZERO.   EL051
00074      12  WS-LAST-TIME            PIC S9(11)  COMP-3 VALUE ZERO.   EL051
00075      12  WS-TIME                 PIC 9(7).                        EL051
00076      12  FILLER    REDEFINES WS-TIME.                             EL051
00077          16  FILLER              PIC X.                           EL051
00078          16  WS-TIME-6           PIC X(6).                        EL051
00079      12  FILLER    REDEFINES WS-TIME.                             EL051
00080          16  FILLER              PIC X.                           EL051
00081          16  WS-HOURS            PIC 99.                          EL051
00082          16  WS-MINUTES          PIC 99.                          EL051
00083          16  WS-SECONDS          PIC 99.                          EL051
00084      12  ABEND-AREA              PIC X(72).                       EL051
00085      12  ERPNDB-FILE-SW          PIC X     VALUE SPACE.           EL051
00086          88  ERPNDB-EOF      VALUE 'Y'.                           EL051
00087          88  ERPNDB-EOF-COMP VALUE 'X'.                           EL051
00088      12  ERPNDC-FILE-SW          PIC X     VALUE SPACE.           EL051
00089          88  ERPNDC-EOF      VALUE 'Y'.                           EL051
00090          88  ERPNDC-EOF-COMP VALUE 'X'.                           EL051
00091      12  DATA-EDITED-SW          PIC X     VALUE SPACE.           EL051
00092          88  DATA-EDITED     VALUE 'Y'.                           EL051
00093      12  EDIT-PROCESS-SW         PIC X     VALUE SPACE.           EL051
00094          88  PROCESS-ENTIRE-FILE     VALUE 'F'.                   EL051
00095          88  PROCESS-COMPANY         VALUE 'C'.                   EL051
00096          88  PROCESS-BATCH           VALUE 'B'.                   EL051
00097          88  BATCH-PROCESS-COMPLETE  VALUE 'Y'.                   EL051
00098      12  FIRST-TIME-SW           PIC X     VALUE 'Y'.             EL051
00099          88  FIRST-TIME              VALUE 'Y'.                   EL051
00100      12  WS-ERR-CODE.                                             EL051
00101          16  FILLER              PIC 99.                          EL051
00102          16  WS-ERROR-SUB        PIC 99.                          EL051
00103      12  PNDC-MSG PIC X(22)   VALUE '1PENDING CLAIMS EDIT  '.     EL051
00104      12  PRINT-CONTROL.                                           EL051
00105          16  SINGLE-SPACE        PIC X     VALUE SPACE.           EL051
00106          16  DOUBLE-SPACE        PIC X     VALUE ZERO.            EL051
00107          16  TRIPLE-SPACE        PIC X     VALUE '-'.             EL051
00108          16  SUPPRESS-SPACE      PIC X     VALUE '+'.             EL051
00109          16  TOP-OF-PAGE         PIC X     VALUE '1'.             EL051
00110      12  WS-COMPUTED-TOTALS      COMP-3.                          EL051
00111          16  WS-LF-ISS-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.EL051
00112          16  WS-LF-ISS-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.EL051
00113          16  WS-AH-ISS-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.EL051
00114          16  WS-AH-ISS-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.EL051
00115          16  WS-LF-CAN-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.EL051
00116          16  WS-LF-CAN-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.EL051
00117          16  WS-AH-CAN-COMPUTED  PIC S9(9)V99  VALUE ZEROS COMP-3.EL051
00118          16  WS-AH-CAN-ENTERED   PIC S9(9)V99  VALUE ZEROS COMP-3.EL051
00119          16  WS-ISSUE-CNT        PIC 9(5)      VALUE ZEROS COMP-3.EL051
00120          16  WS-CANCEL-CNT       PIC 9(5)      VALUE ZEROS COMP-3.EL051
00121      12  WS-LGX-CLAIM-USER       PIC X         VALUE SPACES.      EL051
00122          88  CO-HAS-CLAIMS                     VALUE 'Y'.            CL**4
00123      12  TYPE-OF-EDIT            PIC X       VALUE SPACES.        EL051
00124          88  REEDIT-OR-RESTART-OR-FULL       VALUE 'X'.           EL051
00125                                                                      CL**8
00126      12  WS-FILES-OPEN-SW        PIC X       VALUE SPACES.           CL**8
00127          88  FILES-NOT-OPEN                  VALUE 'N'.              CL**8
00128                                                                      CL**2
00129      12  WS-DELAY-INTERVAL       PIC S9(7)     VALUE +2   COMP-3.    CL**2
00130      12  PNDB-REC-COUNT          PIC S9(4)     VALUE +0.             CL**2
00131                                                                   EL051
00132      12  WS-BATCH-NO.                                             EL051
00133          16  WS-BATCH-PREFIX         PIC XXX         VALUE SPACES.EL051
00134          16  FILLER                  PIC XXX         VALUE SPACES.EL051
00135                                                                   EL051
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

00136  01  TEMPORARY-STORAGE-AREA.                                      EL051
00137      12  TS-QUEUE-ID.                                             EL051
00138          16  FILLER                  PIC XX          VALUE 'SE'.  EL051
00139          16  TS-QUEUE-COMPANY-ID     PIC X(3)        VALUE SPACES.EL051
00140          16  FILLER                  PIC X(3)        VALUE SPACES.EL051
00141                                                                   EL051
00142      12  TS-ITEM     COMP            PIC S9(4)       VALUE ZERO.  EL051
00143                                                                   EL051
00144      12  TS-RECORD.                                               EL051
00145          16  TS-COMPANY-CD           PIC X.                       EL051
00146          16  TS-BATCH-NO             PIC X(6).                    EL051
00147          16  TS-COMPANY-ID           PIC X(3).                    EL051
00148          16  TS-RESTART-BATCH-NO     PIC X(6).                    EL051
00149                                                                   EL051
00150      12  TS-RECORD-LENGTH  COMP      PIC S9(4) VALUE +16.         EL051
00151                                                                   EL051
00152      EJECT                                                        EL051
00153  01  TRANS-DATA-MSG.                                              EL051
00154      12  TD-EDIT-TYPE                PIC X(7) VALUE 'BATCH'.      EL051
00155      12  TD-MSG      PIC X(20)  VALUE 'EDIT HAS COMPLETED  '.     EL051
00156      12  TD-REC-CNT              PIC ZZZ,ZZ9.                     EL051
00157      12  FILLER                  PIC X(14) VALUE '  RECORDS FOR '.EL051
00158      12  TD-COMPANY-ID           PIC X(3)  VALUE SPACES.          EL051
00159      12  FILLER      PIC X(20) VALUE ' LAST BATCH NO. WAS '.      EL051
00160      12  TD-LAST-BATCH           PIC X(6)  VALUE SPACES.          EL051
00161      12  FILLER      PIC X(09) VALUE ' TASK NO'.                  EL051
00162      12  TD-TASK-NO              PIC 9(7)  VALUE ZERO.            EL051
00163      12  FILLER                      PIC X(14)       VALUE SPACES.EL051
00164                                                                   EL051
00165  01  TRANS-DATA-MSG2.                                             EL051
00166      12  FILLER                      PIC X(10)    VALUE SPACES.   EL051
00167      12  FILLER      PIC X(9) VALUE ' STARTED '.                  EL051
00168      12  TD-START-TIME.                                           EL051
00169          16  TD-START-HOURS          PIC 99 VALUE ZERO.           EL051
00170          16  FILLER                  PIC X VALUE '.'.             EL051
00171          16  TD-START-MINUTES        PIC 99 VALUE ZERO.           EL051
00172          16  FILLER                  PIC X VALUE '.'.             EL051
00173          16  TD-START-SECONDS        PIC 99 VALUE ZERO.           EL051
00174      12  FILLER      PIC X(9) VALUE ' ELAPSED '.                  EL051
00175      12  TD-ELAPSED-TIME.                                         EL051
00176          16  TD-ELAPSED-HOURS        PIC 99 VALUE ZERO.           EL051
00177          16  FILLER                  PIC X VALUE '.'.             EL051
00178          16  TD-ELAPSED-MINUTES      PIC 99 VALUE ZERO.           EL051
00179          16  FILLER                  PIC X VALUE '.'.             EL051
00180          16  TD-ELAPSED-SECONDS      PIC 99 VALUE ZERO.           EL051
00181      12  FILLER      PIC X(10) VALUE ' LAST MSG '.                EL051
00182      12  TD-ELAPSED2-TIME.                                        EL051
00183          16  TD-ELAPSED2-HOURS       PIC 99 VALUE ZERO.           EL051
00184          16  FILLER                  PIC X VALUE '.'.             EL051
00185          16  TD-ELAPSED2-MINUTES     PIC 99 VALUE ZERO.           EL051
00186          16  FILLER                  PIC X VALUE '.'.             EL051
00187          16  TD-ELAPSED2-SECONDS     PIC 99 VALUE ZERO.           EL051
00188      12  FILLER  PIC X(38)       VALUE SPACES.                    EL051
00189                                                                   EL051
00190      EJECT                                                        EL051
00191  01  ACCESS-KEYS.                                                 EL051
00192      12  ELCNTL-KEY.                                              EL051
00193          16  ELCNTL-COMPANY-ID   PIC XXX   VALUE SPACES.          EL051
00194          16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.          EL051
00195          16  ELCNTL-FILLER       PIC X(3)  VALUE SPACES.          EL051
00196          16  ELCNTL-CARRIER      PIC X     VALUE SPACES.          EL051
00197          16  ELCNTL-SEQ-NO       PIC S9(4) COMP  VALUE ZEROS.     EL051
00198      12  ERPNDB-KEY.                                              EL051
00199          16  ERPNDB-COMPANY-CD   PIC X     VALUE SPACES.          EL051
00200          16  ERPNDB-BATCH        PIC X(6)  VALUE SPACES.          EL051
00201          16  ERPNDB-SEQ-NO       PIC S9(4) COMP  VALUE ZEROS.     EL051
00202          16  ERPNDB-SEQ-XX REDEFINES ERPNDB-SEQ-NO PIC XX.        EL051
00203          16  ERPNDB-CHG-SEQ-NO   PIC S9(4) COMP  VALUE ZEROS.     EL051
00204      12  ERPNDC-KEY.                                              EL051
00205          16  ERPNDC-COMPANY-CD   PIC X     VALUE SPACES.          EL051
00206          16  ERPNDC-CARRIER      PIC X     VALUE SPACES.          EL051
00207          16  ERPNDC-GROUPING     PIC X(6)  VALUE SPACES.          EL051
00208          16  ERPNDC-STATE        PIC XX    VALUE SPACES.          EL051
00209          16  ERPNDC-ACCOUNT      PIC X(10) VALUE SPACES.          EL051
00210          16  ERPNDC-CERT-EFF-DT  PIC XX    VALUE SPACES.          EL051
00211          16  ERPNDC-CERT-NO      PIC X(11) VALUE SPACES.          EL051
00212          16  ERPNDC-CLAIM-NO     PIC X(7)  VALUE SPACES.          EL051
00213          16  ERPNDC-CHECK-NO     PIC X(7)  VALUE SPACES.          EL051
00214          16  ERPNDC-REC-TYPE     PIC X     VALUE SPACES.          EL051
00215          16  ERPNDC-SEQ-NO       PIC S9(4) COMP  VALUE ZEROS.     EL051
00216      12  CERT-KEY.                                                EL051
00217          16  CERT-COMPANY-CD             PIC X.                   EL051
00218          16  CERT-CARRIER                PIC X.                   EL051
00219          16  CERT-GROUPING               PIC X(6).                EL051
00220          16  CERT-STATE                  PIC XX.                  EL051
00221          16  CERT-ACCOUNT                PIC X(10).               EL051
00222          16  CERT-CERT-EFF-DT            PIC XX.                  EL051
00223          16  CERT-CERT-NO.                                        EL051
00224              20  CERT-CERT-PRIME         PIC X(10).               EL051
00225              20  CERT-CERT-SFX           PIC X.                   EL051
00226                                                                   EL051
00227      EJECT                                                        EL051
00228      COPY ELCREPT.                                                   CL**8
00229      EJECT                                                        EL051
00230                                                                   EL051
00231  01  PRT-LINE.                                                    EL051
00232      12  PRT-MSG  PIC X(22)   VALUE '1PENDING BUSINESS EDIT'.     EL051
00233      12  FILLER   PIC X(11)   VALUE ' COMPLETED-'.                EL051
00234      12  PRT-CO   PIC X(31)   VALUE SPACES.                       EL051
00235      12  PRT-DATE PIC X(8)    VALUE SPACES.                       EL051
00236      12  FILLER   PIC X       VALUE SPACES.                       EL051
00237      12  PRT-TIME PIC X(6)    VALUE SPACES.                       EL051
00238                                                                   EL051
00239  01  START-LINE.                                                  EL051
00240      12  START-MSG  PIC X(22)  VALUE '1PENDING BUSINESS EDIT'.    EL051
00241      12  FILLER     PIC X(11)  VALUE ' STARTED - '.               EL051
00242      12  START-CO   PIC X(31)  VALUE SPACES.                      EL051
00243      12  START-DATE PIC X(8)   VALUE SPACES.                      EL051
00244      12  FILLER     PIC X      VALUE SPACES.                      EL051
00245      12  START-TIME PIC X(6)   VALUE SPACES.                      EL051
00246                                                                   EL051
00247  01  TIME-UNFORMATTED.                                            EL051
00248      12  UN-HOURS                PIC XX.                          EL051
00249      12  UN-MINUTES              PIC XX.                          EL051
00250      12  FILLER                  PIC XX.                          EL051
00251                                                                   EL051
00252  01  TIME-FORMATTED.                                              EL051
00253      12  FOR-HOURS               PIC XX.                          EL051
00254      12  FILLER                  PIC X       VALUE ':'.           EL051
00255      12  FOR-MINUTES             PIC XX.                          EL051
00256      EJECT                                                        EL051
00257      COPY ELCDATE.                                                   CL**8
00258      EJECT                                                        EL051
00259      COPY ELCAID.                                                    CL**8
00260  01  FILLER    REDEFINES DFHAID.                                  EL051
00261      12  FILLER              PIC X(8).                            EL051
00262      12  PF-VALUES           PIC X       OCCURS 2.                EL051
00263      EJECT                                                        EL051
00264      COPY ELCEMIB.                                                   CL**8
00265      EJECT                                                        EL051
00266      COPY ELCJPFX.                                                   CL**8
00267                                 PIC X(585).                       EL051
00268      EJECT                                                        EL051
00269  01  BATCH-TO-PROCESS            VALUE SPACES.                    EL051
00270      05  EDIT-COMPANY-CD         PIC X.                           EL051
00271      05  EDIT-BATCH              PIC X(6).                        EL051
00272      05  EDIT-COMPANY-ID         PIC XXX.                         EL051
00273      05  EDIT-RESTART-BATCH      PIC X(6).                        EL051
00274      EJECT                                                        EL051
00275  01  EDIT-WORK-AREAS.                                             EL051
00276      05  ED-AH-BEN-ADDR          PIC S9(8)  COMP VALUE +0.        EL051
00277      05  ED-AH-BEN-ADDR          PIC S9(8)  COMP VALUE +0.        EL051
00278      05  ED-AH-BEN-ADDR          PIC S9(8)  COMP VALUE +0.        EL051
00279      05  ED-AH-BEN-ADDR          PIC S9(8)  COMP VALUE +0.        EL051
00280      EJECT                                                        EL051
00281 *    COPY ERCPNDB REPLACING                                          CL*11
00282 *    PENDING-BUSINESS       BY PNDB-EDIT-PASS-AREA.                  CL*11
00283                                 COPY ERCPNDB.                        CL*11
00284  01  PNDB-EDIT-PASS-AREA.                                            CL*11
00285      12  WK-PENDING-BUSINESS          PIC X(585) VALUE SPACES.       CL*13
00286 * COPYBOOK FOR ADDITIONAL DFHCOMMAREA WK-WORK-AREA.                  CL*13
00287      COPY ELC50W1 REPLACING WK-CANCEL-EXIT-DT                        CL*13
00288                       BY  WK-CURRENT-DATE.                           CL*13
00289      12  EDIT-CRITERIA-WORK-AREA      PIC X(352) VALUE SPACES.       CL*13
00290      EJECT                                                        EL051
00291      COPY ERCPNDC REPLACING                                          CL*11
00292      PENDING-CLAIMS          BY PNDC-EDIT-PASS-AREA.              EL051
00293      12  WK-PC-WORK-AREA.                                         EL051
00294          16  WK-PC-CNTL-RECORD-FOUND-SW  PIC X.                   EL051
00295          16  WK-PC-LAST-CARRIER          PIC X.                   EL051
00296          16  WK-PC-CERT-ACCESS-CNTL      PIC X.                   EL051
00297          16  WK-PC-CO-CLAIM-REJECT-SW    PIC X.                   EL051
00298          16  WK-PC-CLAIM-SYSTEM-SW       PIC X.                   EL051
00299          16  WK-PC-CO-TOL-CLAIM          PIC S9(3)V99  COMP-3.    EL051
00300          16  WK-PC-RESERVE-CONTROLS      PIC X(4).                EL051
00301          16  WK-PC-CREDIT-EDIT-CONTROLS  PIC X(12).               EL051
00302      12  WK-PC-RECORD-ADDRESSES.                                  EL051
00303          16  WK-PC-ACCT-ADDR             PIC S9(8)     COMP.      EL051
00304          16  WK-PC-STATE-ADDR            PIC S9(8)     COMP.      EL051
00305      12  WK-MISC.                                                    CL**7
00306          16  WK-PC-REM-TRM-CALC-OPTION   PIC X.                      CL**7
00307          16  FILLER                      PIC X(20).                  CL**7
00308      EJECT                                                        EL051


00322      COPY ELCCNTL.                                                   CL**8
00323      EJECT                                                        EL051
00324      COPY ELCCERT.                                                   CL**8
00325      EJECT                                                        EL051

110921 01  work-flow-pass-area.
110921     05  pa-rec-type             pic x(4).
110921     05  pa-company-id           pic xxx.
110921     05  pa-rest-of-record       pic x(600).

00309  01  ERROR-SEVERITY-CODES.                                        EL051
00310      12  STD-ERR-SEVERITY            OCCURS 25 TIMES              EL051
00311                                      INDEXED BY SINDEX PIC X.     EL051
00312      12  TRN-ERR-SEVERITY            OCCURS 150 TIMES             EL051
00313                                      INDEXED BY TINDEX PIC X.     EL051
00314      EJECT                                                        EL051
00315  LINKAGE SECTION.                                                 EL051
00316  01  DFHCOMMAREA                     PIC X(1024).
110921 01  var  pic x(30).
00317 *01 PARM-LIST .                                                      CL*11
00318 *    02  FILLER              PIC S9(8)   COMP.                       CL*11
00319 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL*11
00320 *    02  CERT-POINTER        PIC S9(8)   COMP.                       CL*11
00321      EJECT                                                        EL051
00326                                                                   EL051
00327  PROCEDURE DIVISION.

           display ' Entering Program EL051 '

00328      EXEC CICS HANDLE CONDITION                                   EL051
00329          ERROR    (8300-ABEND)                                    EL051
00330          PGMIDERR (9900-ERROR-FORMAT)                                CL*11
00331          NOTFND   (0210-PROCESS-FILE)                             EL051
00332          ENDDATA  (0210-PROCESS-FILE)                                CL*11
00333      END-EXEC.                                                       CL*11

           EXEC CICS RETRIEVE
               INTO   (BATCH-TO-PROCESS)
               LENGTH (ELEN)
               resp   (ws-response)
               resp2  (ws-response2)
           END-EXEC

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
00339      MOVE BATCH-TO-PROCESS       TO  TS-RECORD.                   EL051
00340                                                                   EL051
00341      MOVE EDIT-COMPANY-ID        TO  TS-QUEUE-COMPANY-ID.         EL051
00342                                                                   EL051
00343      IF TS-RESTART-BATCH-NO NOT NUMERIC                           EL051
00344          MOVE LOW-VALUES         TO  TS-RESTART-BATCH-NO.         EL051
00345                                                                   EL051
00346      EXEC CICS WRITEQ TS                                          EL051
00347          QUEUE  (TS-QUEUE-ID)                                     EL051
00348          ITEM   (TS-ITEM)                                         EL051
00349          FROM   (TS-RECORD)                                       EL051
00350          LENGTH (TS-RECORD-LENGTH)                                   CL*11
00351      END-EXEC.                                                       CL*11
00352                                                                   EL051
00353      EXEC CICS SYNCPOINT                                          EL051
00354      END-EXEC.                                                       CL*11
00355                                                                   EL051
00356      IF EDIT-BATCH = SPACES                                       EL051
00357          MOVE 'C'                TO EDIT-PROCESS-SW               EL051
00358        ELSE                                                       EL051
00359          MOVE 'B'                TO EDIT-PROCESS-SW.              EL051
00360                                                                   EL051
00361      GO TO 1000-PROCESS-FILE.                                     EL051
00362                                                                   EL051
00363  0210-PROCESS-FILE.                                               EL051
00364      MOVE 'F'                    TO EDIT-PROCESS-SW.              EL051
00365                                                                   EL051
00366      EJECT                                                        EL051
00367  1000-PROCESS-FILE.                                               EL051

           display ' EIBTRNID = ' EIBTRNID

00368      IF EIBTRNID = 'EXEB' OR 'XXEB'                               EL051
00369          MOVE 'BATCH'          TO  TD-EDIT-TYPE                   EL051
00370          EXEC CICS ENQ                                            EL051
00371              RESOURCE (EIBTRNID)                                  EL051
00372              LENGTH   (4)                                            CL*11
00373          END-EXEC                                                    CL*11
00374       ELSE                                                        EL051
00375        EXEC CICS ENQ                                              EL051
00376             RESOURCE   (EDIT-COMPANY-CD)                          EL051
00377             LENGTH     (1)                                        EL051
00378        END-EXEC                                                      CL*11
00379        MOVE 'SYSTEM '            TO  TD-EDIT-TYPE.                   CL*11
00380                                                                   EL051
00381      MOVE REC-COUNT              TO  TD-REC-CNT.                  EL051
00382      MOVE EDIT-COMPANY-ID        TO  TD-COMPANY-ID.               EL051
00383      MOVE EDIT-BATCH             TO  TD-LAST-BATCH.               EL051
00384      MOVE EIBTASKN               TO  TD-TASK-NO.                  EL051
00385                                                                   EL051
00386      EXEC CICS ASKTIME                                            EL051
00387      END-EXEC.                                                       CL*11
00388                                                                   EL051
00389      MOVE EIBTIME                TO  WS-TIME.                     EL051
00390      COMPUTE WS-START-TIME = (WS-HOURS * 3600) +                  EL051
00391                              (WS-MINUTES * 60) + WS-SECONDS.      EL051
00392                                                                   EL051
00393      MOVE WS-HOURS               TO  TD-START-HOURS.              EL051
00394      MOVE WS-MINUTES             TO  TD-START-MINUTES.            EL051
00395      MOVE WS-SECONDS             TO  TD-START-SECONDS.            EL051
00396                                                                   EL051
00397      EXEC CICS WRITEQ TD                                          EL051
00398          QUEUE  ('CSMT')                                          EL051
00399          FROM   (TRANS-DATA-MSG)                                  EL051
00400          LENGTH (TRANS-DATA-LENGTH)                                  CL*11
00401      END-EXEC.                                                       CL*11
00402                                                                   EL051
00403      EXEC CICS WRITEQ TD                                          EL051
00404          QUEUE  ('CSMT')                                          EL051
00405          FROM   (TRANS-DATA-MSG2)                                 EL051
00406          LENGTH (TRANS-DATA-LENGTH)                                  CL*11
00407      END-EXEC.                                                       CL*11
00408                                                                   EL051
00409      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL051
00410      MOVE '5'                    TO DC-OPTION-CODE.               EL051
00411      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL051
00412      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DATE.              EL051
00413      MOVE DC-BIN-DATE-1          TO WK-CURRENT-DATE.              EL051
00414      MOVE WS-CURRENT-DATE        TO PRT-DATE                      EL051
00415                                     START-DATE.                   EL051
00416      MOVE ZEROS                  TO WK-ACCT-ADDR                  EL051
00417                                     WK-LIFE-EDIT-ADDR             EL051
00418                                     WK-AH-EDIT-ADDR               EL051
00419                                     WK-LIFE-BEN-ADDR              EL051
00420                                     WK-AH-BEN-ADDR                EL051
00421                                     WK-STATE-ADDR                 EL051
00422                                     WK-PC-ACCT-ADDR               EL051
00423                                     WK-PC-STATE-ADDR                 CL**9
00424                                     WK-PLAN-ADDR                     CL*12
00425                                     WK-FORM-ADDR.                    CL*13
00426                                                                      CL*12
00427      MOVE SPACE                  TO WK-CNTL-RECORD-FOUND-SW       EL051
00428                                     WK-PC-CNTL-RECORD-FOUND-SW    EL051
00429                                     ERROR-SEVERITY-CODES.         EL051
00430                                                                   EL051
00431      IF PROCESS-ENTIRE-FILE                                       EL051
00432          MOVE LOW-VALUES         TO ERPNDB-KEY                    EL051
00433                                     ERPNDC-KEY                    EL051
00434          MOVE ZEROS              TO ERPNDB-SEQ-NO                 EL051
00435                                     ERPNDC-SEQ-NO                 EL051
00436      ELSE                                                         EL051
00437          MOVE EDIT-COMPANY-CD    TO ERPNDB-COMPANY-CD             EL051
00438                                     ERPNDC-COMPANY-CD             EL051
00439          IF EDIT-RESTART-BATCH = SPACES OR 'REEDIT'               EL051
00440             MOVE EDIT-BATCH      TO ERPNDB-BATCH                  EL051
00441          ELSE                                                     EL051
00442             MOVE EDIT-RESTART-BATCH TO ERPNDB-BATCH.              EL051
00443                                                                   EL051
00444      IF EDIT-BATCH         = SPACES  OR                           EL051
00445         EDIT-RESTART-BATCH = 'REEDIT'                             EL051
00446         MOVE 'X'                 TO TYPE-OF-EDIT                  EL051
00447         PERFORM 6000-WRITE-EL051-MESSAGE THRU 6999-EXIT.          EL051
00448                                                                   EL051
00449  1100-PROCESS-PNDB-LOOP.                                          EL051
00450      MOVE ERPNDB-BATCH           TO WS-PREV-BATCH.                EL051
00451      PERFORM 3200-READ-NEXT-RECORD THRU 3290-EXIT.                EL051
00452                                                                   EL051
00453      IF ERPNDB-EOF                                                EL051
00454        OR ERPNDB-EOF-COMP                                         EL051
00455          GO TO 1200-WRITE-REPORT.                                 EL051
00456                                                                   EL051
00457      IF NOT PB-BATCH-TRAILER                                      EL051
00458          ADD 1                  TO WS-SYNC-CNTR                   EL051
00459                                    REC-COUNT                      EL051
00460          PERFORM 2000-CALL-PNDB-EDIT-PROGRAM THRU  2090-EXIT.     EL051

110921     if pb-issue
110921        perform 1800-work-flow   thru 1800-exit
110921     end-if
00461                                                                   EL051
00462      PERFORM 4000-SET-PNDB-ERROR-FLAGS THRU 4900-EXIT.            EL051
00463                                                                   EL051
00464      IF WS-SYNC-CNTR = 32                                         EL051
00465         MOVE ZEROS               TO WS-SYNC-CNTR                  EL051
00466         EXEC CICS SYNCPOINT                                       EL051
00467         END-EXEC.                                                    CL*11
00468                                                                   EL051
00469      IF PROCESS-COMPANY AND NOT PB-BATCH-TRAILER                  EL051
00470          DIVIDE REC-COUNT BY 500 GIVING WS-WORK REMAINDER WS-REM  EL051
00471          IF WS-REM = ZEROS                                        EL051
00472             MOVE REC-COUNT           TO TD-REC-CNT                EL051
00473             MOVE EDIT-COMPANY-ID TO TD-COMPANY-ID                 EL051
00474             MOVE ERPNDB-BATCH TO TD-LAST-BATCH                    EL051
00475                                  TS-RESTART-BATCH-NO              EL051
00476             PERFORM 1500-PRINT-MESSAGE THRU 1599-EXIT             EL051
00477             ADD +1 TO WS-LINE-NUMBER                              EL051
00478             MOVE ERPNDB-COMPANY-CD  TO RF-COMPANY-CD              EL051
00479             MOVE '1'             TO RF-RECORD-TYPE                EL051
00480             MOVE 'EL051'         TO RF-REPORT-ID                  EL051
00481             MOVE TRANS-DATA-MSG  TO RF-DATA-133                   EL051
00482             PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT          CL*11
00483                                                                      CL*11
00484             PERFORM 5950-UPDATE-TYPE-2 THRU 5959-EXIT             EL051
00485             EXEC CICS WRITEQ TS REWRITE                           EL051
00486                  QUEUE  (TS-QUEUE-ID)                             EL051
00487                  ITEM   (TS-ITEM)                                 EL051
00488                  FROM   (TS-RECORD)                               EL051
00489                  LENGTH (TS-RECORD-LENGTH)                           CL*11
00490             END-EXEC.                                                CL*11
00491                                                                   EL051
00492      IF BATCH-PROCESS-COMPLETE                                    EL051
00493          MOVE 'EDIT TERMINATED'  TO TD-MSG                        EL051
00494          PERFORM 1500-PRINT-MESSAGE THRU 1599-EXIT                EL051
00495          MOVE HIGH-VALUES        TO  TS-RESTART-BATCH-NO          EL051
00496          EXEC CICS WRITEQ TS REWRITE                              EL051
00497              QUEUE  (TS-QUEUE-ID)                                 EL051
00498              ITEM   (TS-ITEM)                                     EL051
00499              FROM   (TS-RECORD)                                   EL051
00500              LENGTH (TS-RECORD-LENGTH)                               CL*11
00501          END-EXEC                                                    CL*11
00502          GO TO 9999-RETURN-CICS.                                  EL051
00503                                                                   EL051
00504      GO TO 1100-PROCESS-PNDB-LOOP.                                EL051
00505                                                                   EL051
00506      EJECT                                                        EL051
00507  1200-WRITE-REPORT.                                               EL051

00508      MOVE REC-COUNT              TO TD-REC-CNT.                   EL051
00509      MOVE EDIT-COMPANY-ID        TO TD-COMPANY-ID.                EL051
00510      MOVE 'EDIT TERMINATED'      TO TD-MSG.                       EL051
00511      MOVE WS-PREV-BATCH          TO TD-LAST-BATCH.                EL051
00512                                                                   EL051
00513      PERFORM 1500-PRINT-MESSAGE THRU 1599-EXIT.                   EL051
00514                                                                   EL051
00515      MOVE HIGH-VALUES            TO  TS-RESTART-BATCH-NO.         EL051
00516                                                                   EL051
00517      EXEC CICS WRITEQ TS REWRITE                                  EL051
00518          QUEUE  (TS-QUEUE-ID)                                     EL051
00519          ITEM   (TS-ITEM)                                         EL051
00520          FROM   (TS-RECORD)                                       EL051
00521          LENGTH (TS-RECORD-LENGTH)                                   CL*11
00522      END-EXEC.                                                       CL*11
00523                                                                   EL051
00524      IF NOT REEDIT-OR-RESTART-OR-FULL                             EL051
00525         GO TO 1260-NO-REPORT.                                     EL051
00526                                                                   EL051
00527      IF EDIT-PROCESS-SW = 'B' OR 'Y'                              EL051
00528          GO TO 1260-NO-REPORT.                                    EL051
00529                                                                   EL051
00530      ADD +1 TO WS-LINE-NUMBER.                                    EL051
00531      MOVE ERPNDB-COMPANY-CD      TO RF-COMPANY-CD.                EL051
00532      MOVE '1'                    TO RF-RECORD-TYPE.               EL051
00533      MOVE 'EL051'                TO RF-REPORT-ID.                 EL051
00534      MOVE TRANS-DATA-MSG         TO RF-DATA-133.                  EL051
00535      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.                CL*11
00536                                                                   EL051
00537      IF DATA-EDITED                                               EL051
00538          MOVE SPACE              TO DATA-EDITED-SW                EL051
00539      ELSE                                                         EL051
00540          GO TO 1260-NO-REPORT.                                    EL051
00541                                                                   EL051
00542  1250-CONTINUE.                                                   EL051
00543                                                                      CL**8
00544      MOVE EDIT-COMPANY-CD        TO RF-COMPANY-CD.                EL051
00545      MOVE 'RF'                   TO RF-RECORD-ID.                 EL051
00546      MOVE '2'                    TO RF-RECORD-TYPE.               EL051
00547      MOVE 'EL051'                TO RF-REPORT-ID.                 EL051
00548                                                                      CL**8
00549      EXEC CICS DELETE                                             EL051
00550          DATASET (ELREPT-FILE-ID)                                 EL051
00551          RIDFLD  (RF-CONTROL-PRIMARY)                             EL051
00552          KEYLENGTH (7)                                            EL051
00553          GENERIC                                                  EL051
00554      END-EXEC.                                                       CL*11
00555                                                                   EL051
00556      MOVE SPACES                 TO RF-TRAILER-RECORD.            EL051
00557      MOVE WS-CURRENT-DATE        TO RF-CURRENT-DATE               EL051
00558      MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.            EL051
00559      MOVE '2'                    TO RF-RECORD-TYPE.               EL051
00560      MOVE ERPNDB-COMPANY-CD      TO RF-COMPANY-CD.                EL051
00561      MOVE 'EL051'                TO RF-REPORT-ID.                 EL051
00562      MOVE WS-LINE-NUMBER         TO RF-LINE-NUMBER.               EL051
00563      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             EL051
00564                                                                   EL051
00565  1260-NO-REPORT.                                                  EL051
00566      IF ERPNDB-EOF                                                EL051
00567          MOVE 'Y'                TO FIRST-TIME-SW                 EL051
00568          MOVE PNDC-MSG           TO PRT-MSG                       EL051
00569          GO TO 1300-START-CLAIMS-EDIT.                            EL051
00570                                                                   EL051
00571      PERFORM 3000-READ-CONTROL-FILE THRU 3090-EXIT.               EL051
00572      MOVE CF-CL-MAIL-TO-NAME     TO PRT-CO.                       EL051
00573      MOVE LOW-VALUES             TO ERPNDB-KEY.                   EL051
00574      MOVE PB-COMPANY-CD          TO ERPNDB-COMPANY-CD.            EL051
00575      MOVE ZEROS                  TO ERPNDB-SEQ-NO.                EL051
00576      MOVE SPACE                  TO ERPNDB-FILE-SW                EL051
00577                                     WK-CNTL-RECORD-FOUND-SW.      EL051
00578      GO TO 1100-PROCESS-PNDB-LOOP.                                EL051
00579      EJECT                                                        EL051
00580  1300-START-CLAIMS-EDIT.                                          EL051
00581                                                                      CL**5
00582      PERFORM 3000-READ-CONTROL-FILE THRU 3090-EXIT.                  CL**5
00583                                                                      CL**5
00584      IF CO-HAS-CLAIMS                                             EL051
00585         GO TO 9999-RETURN-CICS.                                   EL051
00586                                                                   EL051
00587      PERFORM 7000-WRITE-EL053-MESSAGE THRU 7999-EXIT.             EL051
00588                                                                   EL051
00589  1300-PROCESS-PNDC-LOOP.                                          EL051
00590      PERFORM 3300-READ-NEXT-RECORD THRU 3390-EXIT.                EL051
00591                                                                   EL051
00592      IF ERPNDC-EOF  OR                                            EL051
00593         ERPNDC-EOF-COMP                                           EL051
00594          GO TO 1400-WRITE-REPORT.                                 EL051
00595                                                                   EL051
00596      PERFORM 2100-CALL-PNDC-EDIT-PROGRAM THRU  2190-EXIT.         EL051
00597      PERFORM 5000-SET-PNDC-ERROR-FLAGS THRU 5900-EXIT.            EL051
00598                                                                   EL051
00599      GO TO 1300-PROCESS-PNDC-LOOP.                                EL051
00600                                                                   EL051
00601      EJECT                                                        EL051
00602                                                                   EL051
00603  1400-WRITE-REPORT.                                               EL051
00604      IF DATA-EDITED                                               EL051
00605          MOVE SPACE              TO DATA-EDITED-SW                EL051
00606      ELSE                                                         EL051
00607          GO TO 1460-NO-REPORT.                                    EL051
00608                                                                   EL051
00609      IF EDIT-PROCESS-SW = 'B' OR 'Y'                              EL051
00610          GO TO 1460-NO-REPORT.                                    EL051
00611                                                                   EL051
00612  1450-CONTINUE.                                                   EL051
00613      MOVE EDIT-COMPANY-CD        TO RF-COMPANY-CD.                EL051
00614      MOVE 'EL053'                TO RF-REPORT-ID.                 EL051
00615      MOVE +1                     TO RF-LINE-NUMBER.               EL051
00616      MOVE '2'                    TO RF-RECORD-TYPE.               EL051
00617      EXEC CICS READ                                               EL051
00618           DATASET  ('ELREPT')                                     EL051
00619           INTO     (REPORT-SAVE-FILE)                             EL051
00620           RIDFLD   (RF-CONTROL-PRIMARY)                           EL051
00621           UPDATE                                                  EL051
00622      END-EXEC.                                                       CL*11
00623                                                                   EL051
00624      MOVE WS-CURRENT-DATE        TO RF-CURRENT-DATE.              EL051
00625                                                                   EL051
00626      EXEC CICS REWRITE                                            EL051
00627           DATASET  ('ELREPT')                                     EL051
00628           FROM     (REPORT-SAVE-FILE)                             EL051
00629      END-EXEC.                                                       CL*11
00630                                                                   EL051
00631  1460-NO-REPORT.                                                  EL051
00632      IF ERPNDC-EOF                                                EL051
00633          GO TO 9999-RETURN-CICS.                                  EL051
00634                                                                   EL051
00635      PERFORM 3000-READ-CONTROL-FILE THRU 3090-EXIT.               EL051
00636      MOVE CF-CL-MAIL-TO-NAME     TO PRT-CO.                       EL051
00637      MOVE LOW-VALUES             TO ERPNDC-KEY.                   EL051
00638      MOVE PC-COMPANY-CD          TO ERPNDC-COMPANY-CD.            EL051
00639      MOVE ZEROS                  TO ERPNDC-SEQ-NO.                EL051
00640      MOVE SPACE                  TO ERPNDC-FILE-SW                EL051
00641                                     WK-PC-CNTL-RECORD-FOUND-SW.   EL051
00642      GO TO 1300-PROCESS-PNDC-LOOP.                                EL051
00643      EJECT                                                        EL051
00644  1500-PRINT-MESSAGE.                                              EL051
00645      MOVE REC-COUNT              TO  TD-REC-CNT.                  EL051
00646                                                                   EL051
00386      EXEC CICS ASKTIME                                            EL051
00387      END-EXEC.                                                       CL*11
00388                                                                   EL051
00647      MOVE EIBTIME                TO  WS-TIME.                     EL051
00649                                                                   EL051
00650      COMPUTE WS-STOP-TIME = (WS-HOURS * 3600) +                   EL051
00651                                 (WS-MINUTES * 60) + WS-SECONDS.   EL051
00652                                                                   EL051
00648      MOVE WS-STOP-TIME           TO  WS-LAST-TIME.                EL051
00653      SUBTRACT WS-START-TIME FROM WS-STOP-TIME                     EL051
00654                                  GIVING WS-TIME-WORK.             EL051
00655      DIVIDE WS-TIME-WORK BY +3600 GIVING TD-ELAPSED-HOURS         EL051
00656                                  REMAINDER WS-TIME-WORK.          EL051
00657      DIVIDE WS-TIME-WORK BY +60 GIVING TD-ELAPSED-MINUTES         EL051
00658                                  REMAINDER TD-ELAPSED-SECONDS.    EL051
00659                                                                   EL051
00660      SUBTRACT WS-LAST-TIME FROM WS-STOP-TIME                      EL051
00661                                  GIVING WS-TIME-WORK.             EL051
00662      DIVIDE WS-TIME-WORK BY +3600 GIVING TD-ELAPSED2-HOURS        EL051
00663                                  REMAINDER WS-TIME-WORK.          EL051
00664      DIVIDE WS-TIME-WORK BY +60 GIVING TD-ELAPSED2-MINUTES        EL051
00665                                  REMAINDER TD-ELAPSED2-SECONDS.   EL051

00386      EXEC CICS ASKTIME                                            EL051
00387      END-EXEC.                                                       CL*11
00388                                                                   EL051
00389      MOVE EIBTIME                TO  WS-TIME.                     EL051
00390      COMPUTE WS-START-TIME = (WS-HOURS * 3600) +                  EL051
00391                              (WS-MINUTES * 60) + WS-SECONDS.      EL051
00392                                                                   EL051
00393      MOVE WS-HOURS               TO  TD-START-HOURS.              EL051
00394      MOVE WS-MINUTES             TO  TD-START-MINUTES.            EL051
00395      MOVE WS-SECONDS             TO  TD-START-SECONDS.            EL051

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

00667      EXEC CICS WRITEQ TD                                          EL051
00668           QUEUE     ('CSMT')                                      EL051
00669           FROM      (TRANS-DATA-MSG)                              EL051
00670           LENGTH    (TRANS-DATA-LENGTH)                              CL*11
00671      END-EXEC.                                                       CL*11
00672                                                                   EL051
00673      EXEC CICS WRITEQ TD                                          EL051
00674           QUEUE     ('CSMT')                                      EL051
00675           FROM      (TRANS-DATA-MSG2)                             EL051
00676           LENGTH    (TRANS-DATA-LENGTH)                              CL*11
00677      END-EXEC.                                                       CL*11
00678                                                                   EL051
00679  1599-EXIT.                                                       EL051
00680      EXIT.                                                        EL051

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
110921           goback
110921        end-if
110921        set connected-to-db to true
110921     end-if
110921
110921     move 'CRTS'                 to pa-rec-type
110921     move edit-company-id        to pa-company-id
110921     move pending-business       to pa-rest-of-record
110921
110921     EXEC CICS LINK                                               
110921         PROGRAM  ('WF001')
110921         COMMAREA (work-flow-pass-area)
110921         LENGTH   (604)
110921     END-EXEC
110921
110921     .
110921 1800-exit.
110921     exit.

00683  2000-CALL-PNDB-EDIT-PROGRAM.                                     EL051
00684                                                                      CL**8
00685      EXEC CICS  HANDLE CONDITION                                     CL**8
00686             NOTFND   (2080-WRITE-REPORT)                             CL**8
00687      END-EXEC.                                                       CL**8
00688                                                                      CL**8
00689      MOVE PENDING-BUSINESS TO WK-PENDING-BUSINESS.                   CL*11
00690      EXEC CICS LINK                                               EL051
00691          PROGRAM    (PNDB-EDIT-PGM)                               EL051
00692          COMMAREA   (PNDB-EDIT-PASS-AREA)                         EL051
00693          LENGTH     (1036)                                           CL*13
00694      END-EXEC.                                                       CL*11
00695                                                                   EL051
00696 ***********   IF VALID RECORD HAS NOT BEEN PASSED BACK TO THIS    EL051
00697 ***********   PROGRAM, THEN AN ABEND HAS OCCURRED.                   CL*11
00698 ***********   IF ERROR 2617 WAS SENT BACK THEN A FILE IS NOT OPEN    CL**8
00699 ***********   AND THE EDIT SHOULD NOT CONTINUE.                   EL051
00700                                                                      CL*11
00701      MOVE WK-PENDING-BUSINESS TO PENDING-BUSINESS.                   CL*11
00702                                                                   EL051
00703         IF ER-2617               = PB-COMMON-ERROR (1)               CL**8
00704                                 OR PB-COMMON-ERROR (2)               CL**8
00705                                 OR PB-COMMON-ERROR (3)               CL**8
00706                                 OR PB-COMMON-ERROR (4)               CL**8
00707                                 OR PB-COMMON-ERROR (5)               CL**8
00708                                 OR PB-COMMON-ERROR (6)               CL**8
00709                                 OR PB-COMMON-ERROR (7)               CL**8
00710                                 OR PB-COMMON-ERROR (8)               CL**8
00711                                 OR PB-COMMON-ERROR (9)               CL**8
00712                                 OR PB-COMMON-ERROR (10)              CL**8
00713                                                                      CL**8
00714         GO TO 9999-RETURN-CICS.                                   EL051
00715                                                                   EL051
00716      IF NOT VALID-PB-ID                                           EL051
00717         MOVE PNDB-EDIT-PASS-AREA TO RF-DATA-133                   EL051
00718         MOVE '1'                 TO RF-CTL-CHAR-133               EL051
00719         MOVE EDIT-COMPANY-CD     TO RF-COMPANY-CD                 EL051
00720         MOVE '1'                 TO RF-RECORD-TYPE                EL051
00721         MOVE 'EL051'             TO RF-REPORT-ID                  EL051
00722         ADD +1                   TO WS-LINE-NUMBER                EL051
00723         PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT           EL051
00724         MOVE '2'                 TO RF-RECORD-TYPE                EL051
00725         MOVE +1                  TO RF-LINE-NUMBER                EL051
00726         MOVE 'ABEND'             TO RF-CURRENT-DATE                  CL**8
00727         EXEC CICS READ                                            EL051
00728              DATASET  ('ELREPT')                                  EL051
00729              INTO     (REPORT-SAVE-FILE)                          EL051
00730              RIDFLD   (RF-CONTROL-PRIMARY)                        EL051
00731              UPDATE                                               EL051
00732         END-EXEC                                                     CL*11
00733         MOVE 'ABEND'             TO RF-CURRENT-DATE               EL051
00734         EXEC CICS REWRITE                                         EL051
00735              DATASET  ('ELREPT')                                  EL051
00736              FROM     (REPORT-SAVE-FILE)                          EL051
00737         END-EXEC                                                     CL*11
00738         GO TO 9999-RETURN-CICS.                                   EL051
00739                                                                      CL**8
00740      GO TO 2090-EXIT.                                                CL**8
00741                                                                      CL**8
00742  2080-WRITE-REPORT.                                                  CL**8
00743                                                                      CL**8
00744      EXEC CICS WRITE                                                 CL**8
00745           DATASET  ('ELREPT')                                        CL**8
00746           RIDFLD   (RF-CONTROL-PRIMARY)                              CL**8
00747           FROM     (REPORT-SAVE-FILE)                                CL**8
00748      END-EXEC.                                                       CL*11
00749                                                                      CL**8
00750      GO TO 9999-RETURN-CICS.                                         CL**8
00751                                                                   EL051
00752  2090-EXIT.                                                       EL051
00753      EXIT.                                                        EL051
00754                                                                   EL051
00755      EJECT                                                        EL051
00756                                                                   EL051
00757  2100-CALL-PNDC-EDIT-PROGRAM.                                     EL051
00758      EXEC CICS LINK                                               EL051
00759          PROGRAM    (PNDC-EDIT-PGM)                               EL051
00760          COMMAREA   (PNDC-EDIT-PASS-AREA)                         EL051
00761          LENGTH     (553)                                            CL**7
00762      END-EXEC.                                                       CL*11
00763                                                                   EL051
00764      IF NOT VALID-PC-ID                                           EL051
00765         MOVE PNDC-EDIT-PASS-AREA TO RF-DATA-133                   EL051
00766         MOVE '1'                 TO RF-CTL-CHAR-133               EL051
00767         MOVE EDIT-COMPANY-CD     TO RF-COMPANY-CD                 EL051
00768         MOVE '1'                 TO RF-RECORD-TYPE                EL051
00769         MOVE 'EL053'             TO RF-REPORT-ID                  EL051
00770         ADD +1 TO RF-LINE-NUMBER                                  EL051
00771         PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT           EL051
00772         MOVE '2'                 TO RF-RECORD-TYPE                EL051
00773         MOVE +1                  TO RF-LINE-NUMBER                EL051
00774         EXEC CICS READ                                            EL051
00775              DATASET  ('ELREPT')                                  EL051
00776              INTO     (REPORT-SAVE-FILE)                          EL051
00777              RIDFLD   (RF-CONTROL-PRIMARY)                        EL051
00778              UPDATE                                               EL051
00779         END-EXEC                                                     CL*11
00780         MOVE 'ABEND'   TO RF-CURRENT-DATE                         EL051
00781         EXEC CICS REWRITE                                         EL051
00782              DATASET  ('ELREPT')                                  EL051
00783              FROM     (REPORT-SAVE-FILE)                          EL051
00784         END-EXEC                                                     CL*11
00785         GO TO 9999-RETURN-CICS.                                   EL051
00786                                                                   EL051
00787  2190-EXIT.                                                       EL051
00788      EXIT.                                                        EL051
00789      EJECT                                                        EL051
00790  3000-READ-CONTROL-FILE.                                          EL051
00791      EXEC CICS HANDLE CONDITION                                   EL051
00792          NOTFND (9999-RETURN-CICS)                                EL051
00793      END-EXEC.                                                       CL*11
00794                                                                   EL051
00795      MOVE SPACES                 TO ELCNTL-KEY.                   EL051
00796      MOVE '1'                    TO ELCNTL-REC-TYPE.              EL051
00797      MOVE EDIT-COMPANY-ID        TO ELCNTL-COMPANY-ID.            EL051
00798                                                                   EL051
00799      MOVE +0                     TO ELCNTL-SEQ-NO.                EL051
00800                                                                   EL051
00801      EXEC CICS READ                                               EL051
00802          DATASET (ELCNTL-FILE-ID)                                 EL051
               into (control-file)
00803 *        SET (ADDRESS OF CONTROL-FILE)                               CL*11
00804          RIDFLD (ELCNTL-KEY)                                      EL051
00805      END-EXEC.                                                       CL*11
00806                                                                   EL051
00807      MOVE CF-LGX-CLAIM-USER      TO WS-LGX-CLAIM-USER.            EL051
00808  3090-EXIT.                                                       EL051
00809      EXIT.                                                        EL051
00810      EJECT                                                        EL051
00811  3200-READ-NEXT-RECORD.                                           EL051
00812      IF ERPNDB-SEQ-NO = 9999                                      EL051
00813         MOVE HIGH-VALUES         TO ERPNDB-SEQ-XX                 EL051
00814        ELSE                                                       EL051
00815         ADD +1                   TO ERPNDB-SEQ-NO.                EL051
00816                                                                   EL051
00817      EXEC CICS HANDLE CONDITION                                   EL051
00818          NOTFND (3210-END-OF-FILE)                                EL051
00819      END-EXEC.                                                       CL*11
00820                                                                   EL051
00821      EXEC CICS READ                                               EL051
00822          INTO    (PENDING-BUSINESS)                                  CL*11
00823 *        INTO    (PNDB-EDIT-PASS-AREA)                               CL*11
00824          DATASET (ERPNDB-FILE-ID)                                 EL051
00825          RIDFLD  (ERPNDB-KEY)                                     EL051
00826          GTEQ                                                     EL051
00827      END-EXEC.                                                       CL*11
00828                                                                   EL051
00829      IF PB-COMPANY-CD NOT = ERPNDB-COMPANY-CD                     EL051
00830          IF PROCESS-COMPANY                                       EL051
00831              MOVE 'Y'            TO ERPNDB-FILE-SW                EL051
00832              GO TO 3290-EXIT                                      EL051
00833          ELSE                                                     EL051
00834              MOVE 'X'            TO ERPNDB-FILE-SW                EL051
00835              GO TO 3290-EXIT.                                     EL051
00836                                                                   EL051
00837      ADD +1                      TO PNDB-REC-COUNT.                  CL**2
00838      IF PNDB-REC-COUNT IS GREATER THAN +30                           CL**2
00839          MOVE +0                 TO PNDB-REC-COUNT                   CL**2
00840 *        EXEC CICS DELAY                                             CL**2
00841 *            INTERVAL  (WS-DELAY-INTERVAL)                           CL**2
00842 *        END-EXEC
           end-if
00843                                                                      CL**2
00844      IF FIRST-TIME                                                EL051
00845          MOVE PB-CONTROL-PRIMARY TO ERPNDB-KEY                    EL051
00846          MOVE SPACE              TO FIRST-TIME-SW.                EL051
00847                                                                   EL051
00848      MOVE PB-CONTROL-PRIMARY     TO ERPNDB-KEY.                   EL051
00849                                                                   EL051
00850      MOVE PB-ENTRY-BATCH         TO WS-BATCH-NO.                  EL051
00851                                                                   EL051
00852      IF  WS-BATCH-PREFIX = '#CL'                                  EL051
00853          MOVE 9999               TO ERPNDB-SEQ-NO                 EL051
00854          GO TO 3200-READ-NEXT-RECORD.                             EL051
00855                                                                   EL051
00856      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL051
00857         IF EDIT-RESTART-BATCH NOT = 'REEDIT'                      EL051
00858            GO TO 3200-READ-NEXT-RECORD.                           EL051
00859                                                                      CL**3
00860      IF PB-BILLED-DT NOT EQUAL LOW-VALUES                            CL**3
00861         GO TO 3200-READ-NEXT-RECORD.                                 CL**3
00862                                                                   EL051
00863      MOVE 'Y'                    TO DATA-EDITED-SW.               EL051
00864      EXEC CICS READ                                               EL051
00865          DATASET (ERPNDB-FILE-ID)                                 EL051
00866          INTO    (PENDING-BUSINESS)                                  CL*11
00867 *        INTO    (PNDB-EDIT-PASS-AREA)                               CL*11
00868          RIDFLD  (ERPNDB-KEY)                                     EL051
00869          UPDATE                                                   EL051
00870      END-EXEC.                                                       CL*11
00871                                                                   EL051
00872      GO TO 3290-EXIT.                                             EL051
00873                                                                   EL051
00874  3210-END-OF-FILE.                                                EL051
00875      MOVE 'Y'                    TO ERPNDB-FILE-SW.               EL051
00876                                                                   EL051
00877  3290-EXIT.                                                       EL051
00878      EXIT.                                                        EL051
00879      EJECT                                                        EL051
00880  3300-READ-NEXT-RECORD.                                           EL051
00881      ADD +1                      TO ERPNDC-SEQ-NO.                EL051
00882      EXEC CICS HANDLE CONDITION                                   EL051
00883          NOTFND (3310-END-OF-FILE)                                EL051
00884      END-EXEC.                                                       CL*11
00885                                                                   EL051
00886      EXEC CICS READ                                               EL051
00887          INTO    (PNDC-EDIT-PASS-AREA)                            EL051
00888          DATASET (ERPNDC-FILE-ID)                                 EL051
00889          RIDFLD  (ERPNDC-KEY)                                     EL051
00890          GTEQ                                                     EL051
00891      END-EXEC.                                                       CL*11
00892                                                                   EL051
00893      IF FIRST-TIME                                                EL051
00894          MOVE PC-CONTROL-PRIMARY TO ERPNDC-KEY                    EL051
00895          MOVE SPACE              TO FIRST-TIME-SW.                EL051
00896                                                                   EL051
00897      IF PC-COMPANY-CD NOT = ERPNDC-COMPANY-CD                     EL051
00898          IF PROCESS-COMPANY                                       EL051
00899              MOVE 'Y'            TO ERPNDC-FILE-SW                EL051
00900              GO TO 3390-EXIT                                      EL051
00901          ELSE                                                     EL051
00902              MOVE 'X'            TO ERPNDC-FILE-SW                EL051
00903              GO TO 3390-EXIT.                                     EL051
00904                                                                      CL**2
00905      ADD +1                      TO PNDB-REC-COUNT.                  CL**2
00906      IF PNDB-REC-COUNT IS GREATER THAN +30                           CL**2
00907          MOVE +0                 TO PNDB-REC-COUNT                   CL**2
00908          EXEC CICS DELAY                                             CL**2
00909              INTERVAL  (WS-DELAY-INTERVAL)                           CL**2
00910          END-EXEC.                                                   CL**2
00911                                                                   EL051
00912      MOVE PC-CONTROL-PRIMARY     TO ERPNDC-KEY.                   EL051
00913                                                                   EL051
00914      IF PC-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL051
00915          GO TO 3300-READ-NEXT-RECORD.                             EL051
00916                                                                   EL051
00917      MOVE 'Y'                    TO DATA-EDITED-SW.               EL051
00918      EXEC CICS READ                                               EL051
00919          DATASET (ERPNDC-FILE-ID)                                 EL051
00920          INTO    (PNDC-EDIT-PASS-AREA)                            EL051
00921          RIDFLD  (ERPNDC-KEY)                                     EL051
00922          UPDATE                                                   EL051
00923      END-EXEC.                                                       CL*11
00924                                                                   EL051
00925      GO TO 3390-EXIT.                                             EL051
00926                                                                   EL051
00927  3310-END-OF-FILE.                                                EL051
00928      MOVE 'Y'                    TO ERPNDC-FILE-SW.               EL051
00929                                                                   EL051
00930  3390-EXIT.                                                       EL051
00931      EXIT.                                                        EL051
00932      EJECT                                                        EL051
00933  3400-WRITE-REPORT-RECORD.                                        EL051
00934                                                                      CL**8
00935      IF FILES-NOT-OPEN                                               CL**8
00936         NEXT SENTENCE                                                CL**8
00937      ELSE                                                            CL**8
00938        IF NOT REEDIT-OR-RESTART-OR-FULL                              CL**8
00939           GO TO 3490-EXIT.                                           CL**8
00940                                                                   EL051
00941      MOVE 'RF'                   TO RF-RECORD-ID.                 EL051
00942      MOVE WS-LINE-NUMBER         TO RF-LINE-NUMBER.               EL051
00943                                                                   EL051
00944      EXEC CICS WRITE                                              EL051
00945          DATASET (ELREPT-FILE-ID)                                 EL051
00946          FROM    (REPORT-SAVE-FILE)                               EL051
00947          RIDFLD  (RF-CONTROL-PRIMARY)                             EL051
00948      END-EXEC.                                                       CL*11
00949                                                                   EL051
00950  3490-EXIT.                                                       EL051
00951       EXIT.                                                       EL051
00952      EJECT                                                        EL051
00953  3500-INCREMENT-ERROR-COUNTERS.                                   EL051
00954      IF EMI-SEVERITY-SAVE = 'W'                                   EL051
00955          ADD 1                   TO EMI-WARNING-CTR               EL051
00956      ELSE                                                         EL051
00957          IF EMI-SEVERITY-SAVE = 'F'                               EL051
00958              ADD 1               TO EMI-FORCABLE-CTR              EL051
00959          ELSE                                                     EL051
00960              IF EMI-SEVERITY-SAVE = 'X'                           EL051
00961                  ADD 1           TO EMI-FATAL-CTR.                EL051
00962                                                                   EL051
00963  3500-EXIT.                                                       EL051
00964      EXIT.                                                        EL051
00965      EJECT                                                        EL051
00966  4000-SET-PNDB-ERROR-FLAGS.                                       EL051
00967                                                                      CL**8
00968      IF PB-COMMON-ERRORS = LOW-VALUES OR                             CL**8
00969         PB-BATCH-TRAILER                                          EL051
00970         GO TO 4500-REWRITE-RECORD.                                EL051
00971                                                                   EL051
00972  4010-FORMAT-ERRORS.                                                 CL**8
00973                                                                   EL051
00974      MOVE +0                     TO SUB1.                            CL**8
00975                                                                   EL051
00976  4100-ERROR-LOOP.                                                    CL**8
00977                                                                      CL**8
00978      ADD +1                      TO SUB1.                            CL**8
00979                                                                      CL**8
00980      IF SUB1 GREATER THAN PB-NO-OF-ERRORS                            CL**8
00981         GO TO 4200-SET-ERROR-FLAGS.                                  CL**8
00982                                                                      CL**8
00983      MOVE PB-COMMON-ERROR (SUB1) TO EMI-ERROR.                       CL**8
00984      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**8
00985                                                                      CL**8
00986      GO TO 4100-ERROR-LOOP.                                          CL**8
00987                                                                      CL**8
00988                                                                      CL**8
00989  4200-SET-ERROR-FLAGS.                                               CL**8
00990                                                                   EL051
00991      IF EMI-FATAL-CTR NOT = ZEROS                                 EL051
00992         MOVE 'X'                 TO PB-FATAL-FLAG                    CL**8
00993         GO TO 4300-SET-ERROR-FLAGS.                                  CL**8
00994                                                                   EL051
00995      IF EMI-FORCABLE-CTR NOT = ZEROS                              EL051
00996         IF PB-ISSUE                                                  CL**8
00997            IF PB-ISSUE-FORCE                                         CL**8
00998               MOVE 'F'           TO PB-FORCE-ER-CD                   CL**8
00999              ELSE                                                    CL**8
01000               MOVE 'X'           TO PB-FORCE-ER-CD                   CL**8
01001           ELSE                                                       CL**8
01002            IF PB-CANCEL-FORCE                                        CL**8
01003               MOVE 'F'           TO PB-FORCE-ER-CD                   CL**8
01004              ELSE                                                    CL**8
01005               MOVE 'X'           TO PB-FORCE-ER-CD.                  CL**8
01006                                                                      CL**8
01007  4300-SET-ERROR-FLAGS.                                               CL**8
01008                                                                   EL051
01009      IF EMI-WARNING-CTR NOT = ZEROS                               EL051
01010         MOVE 'W'                 TO PB-WARN-ER-CD.                EL051
01011                                                                   EL051
01012      IF PB-UNFORCED-ERRORS OR                                        CL**8
01013         PB-FATAL-ERRORS    OR                                        CL**8
01014         PB-RECORD-ON-HOLD  OR                                        CL**8
01015         PB-RECORD-RETURNED OR                                        CL**8
01016         PB-CANCELLATION                                              CL**8
01017           NEXT SENTENCE                                              CL**8
01018         ELSE                                                         CL**8
01019           GO TO 4500-REWRITE-RECORD.                                 CL**8
01020                                                                      CL**8
01021                                                                   EL051
01022      EXEC CICS  HANDLE CONDITION                                  EL051
01023            NOTFND    (4500-REWRITE-RECORD)                        EL051
01024      END-EXEC.                                                    EL051
01025                                                                   EL051
01026      MOVE PB-CONTROL-BY-ACCOUNT  TO CERT-KEY.                     EL051
01027      MOVE PB-SV-CARRIER          TO CERT-CARRIER.                 EL051
01028      MOVE PB-SV-GROUPING         TO CERT-GROUPING.                EL051
01029      MOVE PB-SV-STATE            TO CERT-STATE.                   EL051
01030      EXEC CICS READ                                               EL051
               into    (certificate-master)
01031 *        SET     (ADDRESS OF CERTIFICATE-MASTER)                     CL*11
01032          DATASET (CERT-ID)                                        EL051
01033          RIDFLD  (CERT-KEY)                                       EL051
01034          UPDATE                                                   EL051
01035      END-EXEC.                                                       CL*11
01036                                                                   EL051
01037      IF CERT-ADDED-BATCH OR CERT-PURGED-OFFLINE                   EL051
01038         GO TO 4490-REWRITE-CERT-MASTER.                           EL051
01039                                                                   EL051
01040      IF PB-ISSUE                                                  EL051
01041         IF  PB-RECORD-RETURNED                                    EL051
01042             MOVE '4'             TO CM-CREDIT-INTERFACE-SW-1      EL051
01043         ELSE                                                      EL051
01044              MOVE '2'            TO CM-CREDIT-INTERFACE-SW-1.     EL051
01045                                                                   EL051
01046      IF PB-CANCELLATION                                           EL051
01047         IF  PB-RECORD-RETURNED                                    EL051
01048             MOVE '7'             TO CM-CREDIT-INTERFACE-SW-2      EL051
01049         ELSE                                                      EL051
01050             IF PB-C-LF-CANCEL-VOIDED OR PB-C-AH-CANCEL-VOIDED     EL051
01051                MOVE '6'          TO CM-CREDIT-INTERFACE-SW-2      EL051
01052             ELSE                                                  EL051
01053                MOVE '4'          TO CM-CREDIT-INTERFACE-SW-2.     EL051
01054                                                                   EL051
01055  4490-REWRITE-CERT-MASTER.                                        EL051
01056      EXEC CICS REWRITE                                            EL051
01057           DATASET    (CERT-ID)                                    EL051
01058           FROM       (CERTIFICATE-MASTER)                         EL051
01059      END-EXEC.                                                       CL*11
01060                                                                   EL051
01061  4500-REWRITE-RECORD.                                             EL051
01062      IF NOT PB-BATCH-TRAILER                                      EL051
01063         MOVE PB-CSR-ID               TO WS-CSR-ID                    CL**8
01064          IF PB-ISSUE                                              EL051
01065              ADD PB-I-LF-PREM-CALC   TO WS-LF-ISS-COMPUTED        EL051
01066              ADD PB-I-LF-ALT-PREM-CALC   TO WS-LF-ISS-COMPUTED    EL051
01067              ADD PB-I-LF-PREMIUM-AMT TO WS-LF-ISS-ENTERED         EL051
01068              ADD PB-I-LF-ALT-PREMIUM-AMT TO WS-LF-ISS-ENTERED     EL051
01069              ADD PB-I-AH-PREM-CALC   TO WS-AH-ISS-COMPUTED        EL051
01070              ADD PB-I-AH-PREMIUM-AMT TO WS-AH-ISS-ENTERED         EL051
01071              ADD 1                   TO WS-ISSUE-CNT              EL051
01072          ELSE                                                     EL051
01073              ADD PB-C-LF-REF-CALC    TO WS-LF-CAN-COMPUTED        EL051
01074              ADD PB-C-LF-CANCEL-AMT  TO WS-LF-CAN-ENTERED         EL051
01075              ADD PB-C-AH-REF-CALC    TO WS-AH-CAN-COMPUTED        EL051
01076              ADD PB-C-AH-CANCEL-AMT  TO WS-AH-CAN-ENTERED         EL051
01077              ADD 1                   TO WS-CANCEL-CNT             EL051
01078      ELSE                                                         EL051
01079          MOVE WS-CSR-ID              TO PB-CSR-ID                    CL**8
01080          MOVE WS-LF-ISS-COMPUTED     TO PB-B-LF-ISS-PRM-COMPUTED  EL051
01081          MOVE WS-LF-ISS-ENTERED      TO PB-B-LF-ISS-PRM-ENTERED   EL051
01082          MOVE WS-AH-ISS-COMPUTED     TO PB-B-AH-ISS-PRM-COMPUTED  EL051
01083          MOVE WS-AH-ISS-ENTERED      TO PB-B-AH-ISS-PRM-ENTERED   EL051
01084          MOVE WS-LF-CAN-COMPUTED     TO PB-B-LF-CAN-PRM-COMPUTED  EL051
01085          MOVE WS-LF-CAN-ENTERED      TO PB-B-LF-CAN-PRM-ENTERED   EL051
01086          MOVE WS-AH-CAN-COMPUTED     TO PB-B-AH-CAN-PRM-COMPUTED  EL051
01087          MOVE WS-AH-CAN-ENTERED      TO PB-B-AH-CAN-PRM-ENTERED   EL051
01088          MOVE WS-ISSUE-CNT           TO PB-B-ISSUE-CNT-ENTERED    EL051
01089          MOVE WS-CANCEL-CNT          TO PB-B-CANCEL-CNT-ENTERED   EL051
01090          MOVE ZEROS                  TO WS-LF-ISS-COMPUTED        EL051
01091                                         WS-LF-ISS-ENTERED         EL051
01092                                         WS-AH-ISS-COMPUTED        EL051
01093                                         WS-AH-ISS-ENTERED         EL051
01094                                         WS-LF-CAN-COMPUTED        EL051
01095                                         WS-LF-CAN-ENTERED         EL051
01096                                         WS-AH-CAN-COMPUTED        EL051
01097                                         WS-AH-CAN-ENTERED         EL051
01098                                         WS-ISSUE-CNT              EL051
01099                                         WS-CANCEL-CNT             EL051
01100          MOVE SPACE                  TO PB-FATAL-FLAG             EL051
01101          IF PROCESS-BATCH                                         EL051
01102              MOVE 'Y'            TO EDIT-PROCESS-SW.              EL051
01103                                                                   EL051
01104      IF PB-BATCH-TRAILER                                          EL051
01105          IF PB-B-LF-ISS-PRM-REMITTED =                            EL051
01106             PB-B-LF-ISS-PRM-ENTERED   AND                         EL051
01107             PB-B-LF-CAN-PRM-REMITTED =                            EL051
01108             PB-B-LF-CAN-PRM-ENTERED   AND                         EL051
01109             PB-B-AH-ISS-PRM-REMITTED =                            EL051
01110             PB-B-AH-ISS-PRM-ENTERED   AND                         EL051
01111             PB-B-AH-CAN-PRM-REMITTED =                            EL051
01112             PB-B-AH-CAN-PRM-ENTERED   AND                         EL051
01113             PB-B-ISSUE-CNT-REMITTED  =                            EL051
01114             PB-B-ISSUE-CNT-ENTERED    AND                         EL051
01115             PB-B-CANCEL-CNT-REMITTED =                            EL051
01116             PB-B-CANCEL-CNT-ENTERED                               EL051
01117              MOVE SPACE          TO PB-OUT-BAL-CD                 EL051
01118      ELSE                                                         EL051
01119          MOVE 'O'                TO PB-OUT-BAL-CD.                EL051
01120                                                                   EL051
01121      MOVE 'C'                    TO JP-RECORD-TYPE.               EL051
01122      MOVE ERPNDB-FILE-ID         TO JP-FILE-ID.                   EL051
01123 *    MOVE PNDB-EDIT-PASS-AREA    TO JP-RECORD-AREA.                  CL*11
01124      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.                  CL*11
01125      COMPUTE JOURNAL-LENGTH = ERPNDB-LENGTH + 23.                 EL051
01126                                                                   EL051
01127      EXEC CICS REWRITE                                            EL051
01128          DATASET (ERPNDB-FILE-ID)                                 EL051
01129 *        FROM    (PNDB-EDIT-PASS-AREA)                               CL*11
01130          FROM    (PENDING-BUSINESS)                                  CL*11
01131      END-EXEC.                                                       CL*11
01132                                                                      CL*11
01133      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL051
01134      MOVE ZEROS                  TO EMI-WARNING-CTR               EL051
01135                                     EMI-FORCABLE-CTR              EL051
01136                                     EMI-FATAL-CTR.                EL051
01137                                                                   EL051
01138  4900-EXIT.                                                       EL051
01139      EXIT.                                                        EL051
01140      EJECT                                                        EL051
01141  5000-SET-PNDC-ERROR-FLAGS.                                       EL051
01142      IF PC-ERROR-FLAGS = SPACES                                   EL051
01143          GO TO 5500-REWRITE-RECORD.                               EL051
01144                                                                   EL051
01145      MOVE LIT-2800               TO WS-ERR-CODE.                  EL051
01146      MOVE 1  TO SUB.                                              EL051
01147                                                                   EL051
01148  5100-ERR-LOOP.                                                   EL051
01149      IF PC-ERR-FLAG (SUB) NOT = SPACES                            EL051
01150          MOVE SUB                TO WS-ERROR-SUB                  EL051
01151          MOVE WS-ERR-CODE        TO EMI-ERROR                     EL051
01152          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL051
01153          PERFORM 3500-INCREMENT-ERROR-COUNTERS THRU 3500-EXIT.    EL051
01154                                                                   EL051
01155      IF EMI-FATAL-CTR NOT = ZEROS                                 EL051
01156          GO TO 5200-SET-ERROR-FLAGS.                              EL051
01157                                                                   EL051
01158      ADD 1   TO SUB.                                              EL051
01159      IF SUB LESS THAN 101                                         EL051
01160          GO TO 5100-ERR-LOOP.                                     EL051
01161                                                                   EL051
01162  5200-SET-ERROR-FLAGS.                                            EL051
01163      IF EMI-FATAL-CTR NOT = ZEROS                                 EL051
01164          MOVE 'X'                TO PC-FATAL-FLAG.                EL051
01165                                                                   EL051
01166      IF EMI-FORCABLE-CTR NOT = ZEROS                              EL051
01167          IF PC-CLAIM-FORCE                                        EL051
01168              MOVE 'F'            TO PC-FORCE-ER-CD                EL051
01169          ELSE                                                     EL051
01170              MOVE 'X'            TO PC-FORCE-ER-CD.               EL051
01171                                                                   EL051
01172      IF EMI-WARNING-CTR NOT = ZEROS                               EL051
01173          MOVE 'W'                TO PC-WARN-ER-CD.                EL051
01174                                                                   EL051
01175  5500-REWRITE-RECORD.                                             EL051
01176      MOVE 'C'                    TO JP-RECORD-TYPE.               EL051
01177      MOVE ERPNDC-FILE-ID         TO JP-FILE-ID.                   EL051
01178      MOVE PNDC-EDIT-PASS-AREA    TO JP-RECORD-AREA.               EL051
01179      COMPUTE JOURNAL-LENGTH = ERPNDC-LENGTH + 23.                 EL051
01180      EXEC CICS REWRITE                                            EL051
01181          DATASET (ERPNDC-FILE-ID)                                 EL051
01182          FROM    (PNDC-EDIT-PASS-AREA)                            EL051
01183      END-EXEC.                                                       CL*11
01184                                                                   EL051
01185      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL051
01186                                                                   EL051
01187      MOVE ZEROS                  TO EMI-WARNING-CTR               EL051
01188                                     EMI-FORCABLE-CTR              EL051
01189                                     EMI-FATAL-CTR.                EL051
01190                                                                   EL051
01191  5900-EXIT.                                                       EL051
01192      EXIT.                                                        EL051
01193      EJECT                                                        EL051
01194  5950-UPDATE-TYPE-2.                                              EL051
01195      IF NOT REEDIT-OR-RESTART-OR-FULL                             EL051
01196         GO TO 5959-EXIT.                                          EL051
01197                                                                   EL051
01198      EXEC CICS  HANDLE CONDITION                                  EL051
01199             NOTFND   (5955-CONTINUE)                              EL051
01200      END-EXEC.                                                    EL051
01201                                                                   EL051
01202      MOVE ERPNDB-COMPANY-CD TO RF-COMPANY-CD.                     EL051
01203      MOVE 'RF'                   TO RF-RECORD-ID.                 EL051
01204      MOVE '2'                    TO RF-RECORD-TYPE.               EL051
01205      MOVE 'EL051'                TO RF-REPORT-ID.                 EL051
01206      EXEC CICS DELETE                                             EL051
01207          DATASET (ELREPT-FILE-ID)                                 EL051
01208          RIDFLD  (RF-CONTROL-PRIMARY)                             EL051
01209          KEYLENGTH (7)                                            EL051
01210          GENERIC                                                  EL051
01211      END-EXEC.                                                       CL*11
01212                                                                   EL051
01213  5955-CONTINUE.                                                   EL051
01214      MOVE EIBTIME                TO WS-TIME.                      EL051
01215      MOVE SPACES                 TO RF-TRAILER-RECORD.            EL051
01216      MOVE 'STARTED'              TO RF-CURRENT-DATE.              EL051
01217      MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.            EL051
01218      MOVE '2'                    TO RF-RECORD-TYPE.               EL051
01219      MOVE ERPNDB-COMPANY-CD      TO RF-COMPANY-CD.                EL051
01220      MOVE 'EL051'                TO RF-REPORT-ID.                 EL051
01221      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             EL051
01222                                                                   EL051
01223  5959-EXIT.                                                       EL051
01224      EXIT.                                                        EL051
01225      EJECT                                                        EL051
01226  6000-WRITE-EL051-MESSAGE.                                        EL051
01227      EXEC CICS  HANDLE CONDITION                                  EL051
01228             NOTFND   (6010-DELETE-TYPE-2)                         EL051
01229      END-EXEC.                                                    EL051
01230                                                                   EL051
01231      MOVE ERPNDB-COMPANY-CD TO RF-COMPANY-CD.                     EL051
01232      MOVE 'RF'                   TO RF-RECORD-ID.                 EL051
01233      MOVE '1'                    TO RF-RECORD-TYPE.               EL051
01234      MOVE 'EL051'                TO RF-REPORT-ID.                 EL051
01235      EXEC CICS DELETE                                             EL051
01236          DATASET (ELREPT-FILE-ID)                                 EL051
01237          RIDFLD  (RF-CONTROL-PRIMARY)                             EL051
01238          KEYLENGTH (7)                                            EL051
01239          GENERIC                                                  EL051
01240      END-EXEC.                                                       CL*11
01241                                                                   EL051
01242  6010-DELETE-TYPE-2.                                              EL051
01243      EXEC CICS  HANDLE CONDITION                                  EL051
01244             NOTFND   (6020-CONTINUE)                              EL051
01245      END-EXEC.                                                    EL051
01246                                                                   EL051
01247      MOVE ERPNDB-COMPANY-CD TO RF-COMPANY-CD.                     EL051
01248      MOVE 'RF'                   TO RF-RECORD-ID.                 EL051
01249      MOVE '2'                    TO RF-RECORD-TYPE.               EL051
01250      MOVE 'EL051'                TO RF-REPORT-ID.                 EL051
01251      EXEC CICS DELETE                                             EL051
01252          DATASET (ELREPT-FILE-ID)                                 EL051
01253          RIDFLD  (RF-CONTROL-PRIMARY)                             EL051
01254          KEYLENGTH (7)                                            EL051
01255          GENERIC                                                  EL051
01256      END-EXEC.                                                       CL*11
01257                                                                   EL051
01258  6020-CONTINUE.                                                   EL051
01259      MOVE EIBTIME                TO WS-TIME.                      EL051
01260      MOVE SPACES                 TO RF-TRAILER-RECORD.            EL051
01261      MOVE 'STARTED'              TO RF-CURRENT-DATE.              EL051
01262      MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.            EL051
01263      MOVE '2'                    TO RF-RECORD-TYPE.               EL051
01264      MOVE ERPNDB-COMPANY-CD      TO RF-COMPANY-CD.                EL051
01265      MOVE 'EL051'                TO RF-REPORT-ID.                 EL051
01266      MOVE +1                     TO WS-LINE-NUMBER.               EL051
01267      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             EL051
01268                                                                   EL051
01269  6999-EXIT.                                                       EL051
01270      EXIT.                                                        EL051
01271      EJECT                                                        EL051
01272  7000-WRITE-EL053-MESSAGE.                                        EL051
01273      EXEC CICS  HANDLE CONDITION                                  EL051
01274             NOTFND   (7010-DELETE-TYPE-2)                         EL051
01275      END-EXEC.                                                    EL051
01276                                                                   EL051
01277      MOVE ERPNDC-COMPANY-CD TO RF-COMPANY-CD.                     EL051
01278      MOVE 'RF'                   TO RF-RECORD-ID.                 EL051
01279      MOVE '1'                    TO RF-RECORD-TYPE.               EL051
01280      MOVE 'EL053'                TO RF-REPORT-ID.                 EL051
01281      EXEC CICS DELETE                                             EL051
01282          DATASET (ELREPT-FILE-ID)                                 EL051
01283          RIDFLD  (RF-CONTROL-PRIMARY)                             EL051
01284          KEYLENGTH (7)                                            EL051
01285          GENERIC                                                  EL051
01286      END-EXEC.                                                       CL*11
01287                                                                   EL051
01288  7010-DELETE-TYPE-2.                                              EL051
01289      EXEC CICS  HANDLE CONDITION                                  EL051
01290             NOTFND   (7020-CONTINUE)                              EL051
01291      END-EXEC.                                                    EL051
01292                                                                   EL051
01293      MOVE ERPNDC-COMPANY-CD TO RF-COMPANY-CD.                     EL051
01294      MOVE 'RF'                   TO RF-RECORD-ID.                 EL051
01295      MOVE '2'                    TO RF-RECORD-TYPE.               EL051
01296      MOVE 'EL053'                TO RF-REPORT-ID.                 EL051
01297      EXEC CICS DELETE                                             EL051
01298          DATASET (ELREPT-FILE-ID)                                 EL051
01299          RIDFLD  (RF-CONTROL-PRIMARY)                             EL051
01300          KEYLENGTH (7)                                            EL051
01301          GENERIC                                                  EL051
01302      END-EXEC.                                                       CL*11
01303                                                                   EL051
01304  7020-CONTINUE.                                                   EL051
01305      MOVE EIBTIME                TO WS-TIME.                      EL051
01306      MOVE EDIT-COMPANY-CD        TO RF-COMPANY-CD.                EL051
01307      MOVE SPACES                 TO RF-TRAILER-RECORD.            EL051
01308      MOVE WS-CURRENT-DATE        TO RF-CURRENT-DATE.              EL051
01309      MOVE WS-TIME-6              TO RF-PRINT-HH-MM-SS.            EL051
01310      MOVE '2'                    TO RF-RECORD-TYPE.               EL051
01311      MOVE 'EL053'                TO RF-REPORT-ID.                 EL051
01312      MOVE +1                     TO WS-LINE-NUMBER.               EL051
01313      PERFORM 3400-WRITE-REPORT-RECORD THRU 3490-EXIT.             EL051
01314                                                                   EL051
01315  7999-EXIT.                                                       EL051
01316      EXIT.                                                        EL051
01317      EJECT                                                        EL051
01318  8300-ABEND.                                                      EL051
01319      MOVE DFHEIBLK TO EMI-LINE1.                                  EL051
01320      EXEC CICS LINK                                               EL051
01321          PROGRAM   ('EL004')                                      EL051
01322          COMMAREA  (EMI-LINE1)                                    EL051
01323          LENGTH    (72)                                           EL051
01324      END-EXEC.                                                    EL051
01325                                                                      CL*11
01326      GO TO 9999-RETURN-CICS.                                      EL051
01327                                                                   EL051
01328  8400-LOG-JOURNAL-RECORD.                                         EL051
01329      MOVE 'EDIT'                 TO JP-USER-ID.                   EL051
01330      MOVE THIS-PGM               TO JP-PROGRAM-ID.                EL051
01331 *    EXEC CICS JOURNAL                                            EL051
01332 *        JFILEID     (1)                                          EL051
01333 *        JTYPEID     ('EL')                                       EL051
01334 *        FROM        (JOURNAL-RECORD)                             EL051
01335 *        LENGTH      (JOURNAL-LENGTH)                             EL051
01336 *        END-EXEC.                                                EL051
01337                                                                   EL051
01338  8500-DATE-CONVERT.                                               EL051
01339      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL051
01340      EXEC CICS LINK                                               EL051
01341          PROGRAM    (PGM-NAME)                                    EL051
01342          COMMAREA   (DATE-CONVERSION-DATA)                        EL051
01343          LENGTH     (DC-COMM-LENGTH)                              EL051
01344      END-EXEC.                                                       CL*11
01345  8500-EXIT.                                                       EL051
01346      EXIT.                                                        EL051
01347                                                                   EL051
01348      EJECT                                                        EL051
01349                                                                   EL051
01350  9900-ERROR-FORMAT.                                               EL051
01351      MOVE EDIT-COMPANY-ID        TO EMI-CLIENT-ID.                   CL*10
01352      MOVE LINK-001               TO PGM-NAME                      EL051
01353      EXEC CICS LINK                                               EL051
01354          PROGRAM    (PGM-NAME)                                    EL051
01355          COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)               EL051
01356          LENGTH     (EMI-COMM-LENGTH)                             EL051
01357      END-EXEC.                                                       CL*11
01358  9900-EXIT.                                                       EL051
01359      EXIT.                                                        EL051
01360                                                                   EL051
01361  9999-RETURN-CICS.                                                EL051
01362      EXEC CICS DEQ                                                EL051
01363           RESOURCE   (EDIT-COMPANY-CD)                            EL051
01364           LENGTH     (1)                                          EL051
01365      END-EXEC.                                                       CL*11
01366                                                                   EL051
01367      EXEC CICS  RETURN                                            EL051
01368      END-EXEC.                                                       CL*11
01369                                                                   EL051
01370  9999-EXIT.                                                       EL051
01371       EXIT.                                                       EL051
