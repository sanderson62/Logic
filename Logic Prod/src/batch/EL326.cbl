00001  IDENTIFICATION DIVISION.                                         08/21/97
00002                                                                   EL326
00003  PROGRAM-ID.                 EL326 .                                 LV001
00004 *              PROGRAM CONVERTED BY                               EL326
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL326
00006 *              CONVERSION DATE 06/22/95 15:51:40.                 EL326
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             EL326
00008 *                            VMOD=2.014.                          EL326
00009                                                                   EL326
00010 *AUTHOR.     LOGIC, INC.                                          EL326
00011 *            DALLAS, TEXAS.                                       EL326
00012                                                                   EL326
00012                                                                   EL326
00013 *DATE-COMPILED.                                                   EL326
00014                                                                   EL326
00015 *SECURITY.   *****************************************************EL326
00016 *            *                                                   *EL326
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL326
00018 *            *                                                   *EL326
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL326
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL326
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL326
00022 *            *                                                   *EL326
00023 *            *****************************************************EL326
00024                                                                   EL326
00025 *REMARKS.                                                         EL326
00026 *         THIS PROGRAM IS USED TO RESTORE RECORDS FROM            EL326
00027 *    THE HISTORY ARCHIVE FILE.                                    EL326
00028                                                                   EL326
00029  ENVIRONMENT DIVISION.                                            EL326
00030                                                                   EL326
00031  INPUT-OUTPUT SECTION.                                            EL326
00032                                                                   EL326
00033  FILE-CONTROL.                                                    EL326
00034                                                                   EL326
00035                                                                   EL326
00036      SELECT SORT-WORK  ASSIGN TO SYS001-UT-2314-S-SORTWK1.        EL326
00037                                                                   EL326
00038      SELECT HISTORY-INPUT-FILE                                    EL326
00039          ASSIGN TO SYS010-UT-2400-S-SYS010.                       EL326
00040                                                                   EL326
00041      SELECT ELCNTL                                                EL326
00042          ASSIGN TO SYS021-FBA1-ELCNTL                             EL326
00043          ORGANIZATION IS INDEXED                                  EL326
00044          ACCESS IS SEQUENTIAL                                     EL326
00045          RECORD KEY IS CF-CONTROL-PRIMARY                         EL326
00046          FILE STATUS IS ELCNTL-FILE-STATUS.                       EL326
00047                                                                   EL326
00048      SELECT ELACTQ                                                EL326
00049          ASSIGN TO SYS021-FBA1-ELACTQ                             EL326
00050          ORGANIZATION IS INDEXED                                  EL326
00051          ACCESS IS SEQUENTIAL                                     EL326
00052          RECORD KEY IS AQ-CONTROL-PRIMARY                         EL326
00053          FILE STATUS IS ELACTQ-FILE-STATUS.                       EL326
00054                                                                   EL326
00055      SELECT ELCERT                                                EL326
00056          ASSIGN TO SYS022-FBA1-ELCERT                             EL326
00057          ORGANIZATION IS INDEXED                                  EL326
00058          ACCESS IS DYNAMIC                                        EL326
00059          RECORD KEY IS CM-CONTROL-PRIMARY                         EL326
00060          FILE STATUS IS ELCERT-FILE-STATUS.                       EL326
00061                                                                   EL326
00062      SELECT ELMSTR                                                EL326
00063          ASSIGN TO SYS023-FBA1-ELMSTR                             EL326
00064          ORGANIZATION IS INDEXED                                  EL326
00065          ACCESS IS DYNAMIC                                        EL326
00066          RECORD KEY IS CL-CONTROL-PRIMARY                         EL326
00067          FILE STATUS IS ELMSTR-FILE-STATUS.                       EL326
00068                                                                   EL326
00069      SELECT ELTRLR                                                EL326
00070          ASSIGN TO SYS024-FBA1-ELTRLR                             EL326
00071          ORGANIZATION IS INDEXED                                  EL326
00072          ACCESS IS DYNAMIC                                        EL326
00073          RECORD KEY IS AT-CONTROL-PRIMARY                         EL326
00074          FILE STATUS IS ELTRLR-FILE-STATUS.                       EL326
00075                                                                   EL326
00076      SELECT ELARCH                                                EL326
00077          ASSIGN TO SYS025-FBA1-ELARCH                             EL326
00078          ORGANIZATION IS INDEXED                                  EL326
00079          ACCESS IS DYNAMIC                                        EL326
00080          RECORD KEY IS LA-CONTROL-PRIMARY                         EL326
00081          FILE STATUS IS ELARCH-FILE-STATUS.                       EL326
00082                                                                   EL326
00083      SELECT MPPLCY                                                EL326
00084          ASSIGN TO SYS026-FBA1-MPPLCY                             EL326
00085          ORGANIZATION IS INDEXED                                  EL326
00086          ACCESS IS DYNAMIC                                        EL326
00087          RECORD KEY IS PM-CONTROL-PRIMARY                         EL326
00088          FILE STATUS IS MPPLCY-FILE-STATUS.                       EL326
00089                                                                   EL326
00090      SELECT ELRETR                                                EL326
00091          ASSIGN TO SYS027-FBA1-ELRETR                             EL326
00092          ORGANIZATION IS INDEXED                                  EL326
00093          ACCESS IS DYNAMIC                                        EL326
00094          RECORD KEY IS RL-CONTROL-PRIMARY                         EL326
00095          FILE STATUS IS ELRETR-FILE-STATUS.                       EL326
00096                                                                   EL326
00097                                                                   EL326
00098      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL326
00099                                                                   EL326
00100      SELECT CONTROL-CARD-FILE                                     EL326
00101                              ASSIGN TO SYS006-UR-2540R-S-SYS006.  EL326
00102                                                                   EL326
00103      EJECT                                                        EL326
00104  DATA DIVISION.                                                   EL326
00105                                                                   EL326
00106  FILE SECTION.                                                    EL326
00107                                                                   EL326
00108  SD  SORT-WORK.                                                   EL326
00109                                                                   EL326
00110  01  SORT-REC.                                                    EL326
00111      05  FILLER                  PIC X(5).                        EL326
00112      05  SRT-COMPANY-ID          PIC XXX.                         EL326
00113      05  FILLER                  PIC X.                           EL326
00114      05  SRT-CARRIER             PIC X.                           EL326
00115      05  FILLER                  PIC X(70).                       EL326
00116      EJECT                                                        EL326
00117                                                                   EL326
00118  FD  HISTORY-INPUT-FILE                                           EL326
00119                                  COPY ELCHAF.                     EL326
00120      EJECT                                                        EL326
00121                                                                   EL326
00122  FD  ELCNTL.                                                      EL326
00123                                  COPY ELCCNTL.                    EL326
00124      EJECT                                                        EL326
00125                                                                   EL326
00126  FD  ELACTQ.                                                      EL326
00127                                  COPY ELCACTQ.                    EL326
00128      EJECT                                                        EL326
00129                                                                   EL326
00130  FD  ELCERT.                                                      EL326
00131                                  COPY ELCCERT.                    EL326
00132      EJECT                                                        EL326
00133                                                                   EL326
00134  FD  ELMSTR.                                                      EL326
00135                                  COPY ELCMSTR.                    EL326
00136      EJECT                                                        EL326
00137                                                                   EL326
00138  FD  ELTRLR.                                                      EL326
00139                                  COPY ELCTRLR.                    EL326
00140      EJECT                                                        EL326
00141                                                                   EL326
00142  FD  ELARCH.                                                      EL326
00143                                  COPY ELCARCH.                    EL326
00144      EJECT                                                        EL326
00145                                                                   EL326
00146  FD  MPPLCY.                                                      EL326
00147                                  COPY MPCPLCY.                    EL326
00148      EJECT                                                        EL326
00149                                                                   EL326
00150  FD  ELRETR.                                                      EL326
00151                                  COPY ELCRETR.                    EL326
00152      EJECT                                                        EL326
00153                                                                   EL326
00154  FD  PRNTR                       COPY ELCPRTFD.                   EL326
00155                                                                   EL326
00156      EJECT                                                        EL326
00157  FD  CONTROL-CARD-FILE                                            EL326
00158      BLOCK CONTAINS 0 RECORDS
00159      RECORDING MODE F.                                            EL326
00160                                                                   EL326
00161  01  CONTROL-CARD-RECORD.                                         EL326
00162                                                                   EL326
00163      05  CC-ID                    PIC X(5).                       EL326
00164        88  VALID-ID                                 VALUE 'EL326'.EL326
00165                                                                   EL326
00166      05  CC-COMPANY-ID            PIC XXX.                        EL326
00167      05  FILLER                   PIC X.                          EL326
00168      05  CC-CARRIER               PIC X.                          EL326
00169      05  FILLER                   PIC X(70).                      EL326
00170                                                                   EL326
00171 *    05  CC-CLAIM-NO              PIC X(7).                       EL326
00172 *    05  FILLER                   PIC X.                          EL326
00173 *    05  CC-CERT-NO               PIC X(11).                      EL326
00174 *    05  FILLER                   PIC X(53).                      EL326
00175                                                                   EL326
00176  WORKING-STORAGE SECTION.                                         EL326
00177                                                                   EL326
00178  77  FILLER  PIC X(32) VALUE '********************************'.  EL326
00179  77  FILLER  PIC X(32) VALUE '*    EL326 WORKING-STORAGE     *'.  EL326
00180  77  FILLER  PIC X(32) VALUE '******** VMOD=2.014 ************'.  EL326
00181                                                                   EL326
00182  01  FILLER                          COMP-3.                      EL326
00183      05  CL-TRAILERS-PROCESSED       PIC S9(9)       VALUE ZERO.  EL326
00184      05  CL-LETTERS-PROCESSED        PIC S9(9)       VALUE ZERO.  EL326
00185                                                                   EL326
00186      05  CO-CLAIMS-PROCESSED         PIC S9(9)       VALUE ZERO.  EL326
00187      05  CO-CLAIMS-RESET             PIC S9(9)       VALUE ZERO.  EL326
00188      05  CO-CERTS-WRITTEN            PIC S9(9)       VALUE ZERO.  EL326
00189      05  CO-CERTS-REWRITTEN          PIC S9(9)       VALUE ZERO.  EL326
00190      05  CO-POLICIES-PROCESSED       PIC S9(9)       VALUE ZERO.  EL326
00191      05  CO-TRAILERS-PROCESSED       PIC S9(9)       VALUE ZERO.  EL326
00192      05  CO-LETTERS-PROCESSED        PIC S9(9)       VALUE ZERO.  EL326
00193                                                                   EL326
00194      05  OV-CLAIMS-PROCESSED         PIC S9(9)       VALUE ZERO.  EL326
00195      05  OV-CLAIMS-RESET             PIC S9(9)       VALUE ZERO.  EL326
00196      05  OV-CERTS-WRITTEN            PIC S9(9)       VALUE ZERO.  EL326
00197      05  OV-CERTS-REWRITTEN          PIC S9(9)       VALUE ZERO.  EL326
00198      05  OV-POLICIES-PROCESSED       PIC S9(9)       VALUE ZERO.  EL326
00199      05  OV-TRAILERS-PROCESSED       PIC S9(9)       VALUE ZERO.  EL326
00200      05  OV-LETTERS-PROCESSED        PIC S9(9)       VALUE ZERO.  EL326
00201                                                                   EL326
00202  01  SWITCHES.                                                    EL326
00203      05  FIRST-TIME-SW               PIC X           VALUE 'Y'.   EL326
00204          88  FIRST-TIME                              VALUE 'Y'.   EL326
00205      05  WS-ELACTQ-EOF               PIC X           VALUE 'N'.   EL326
00206          88  ELACTQ-EOF                              VALUE 'Y'.   EL326
00207      05  WS-HISTORY-EOF              PIC X           VALUE 'N'.   EL326
00208          88  HISTORY-EOF                             VALUE 'Y'.   EL326
00209      05  WS-PULL-CARRIER-SW          PIC X           VALUE 'N'.   EL326
00210          88  PULL-CARRIER                            VALUE 'Y'.   EL326
00211      05  WS-DELETE-ACTQ-SW           PIC X           VALUE 'N'.   EL326
00212          88  DELETE-ACTQ                             VALUE 'Y'.   EL326
00213      05  WS-REWRITE-ACTQ-SW          PIC X           VALUE 'N'.   EL326
00214          88  REWRITE-ACTQ                            VALUE 'Y'.   EL326
00215      05  CO-CLAIMS-RESTORED-SW       PIC X           VALUE 'N'.   EL326
00216          88  CLAIMS-WERE-RESTORED                    VALUE 'Y'.   EL326
00217      05  WS-RETURN-SW                PIC X           VALUE 'N'.   EL326
00218          88  RETURN-RECORD                           VALUE 'Y'.   EL326
00219      05  WS-READ-RETURN-SW           PIC X           VALUE 'N'.   EL326
00220          88  READ-RETURN-RECORD                      VALUE 'Y'.   EL326
00221      05  WS-SORT-EOF-SW              PIC X           VALUE 'N'.   EL326
00222          88  SORT-AT-END                             VALUE 'Y'.   EL326
00223      05  WS-READ-ACTQ-SW             PIC X           VALUE 'N'.   EL326
00224          88  READ-ACTIVITY-FILE                      VALUE 'Y'.   EL326
00225      05  WS-HAVE-ACTIVITY-SW         PIC X           VALUE 'N'.   EL326
00226          88  HAVE-ACTIVITY                           VALUE 'Y'.   EL326
00227      05  WS-BYPASS-READ-SW           PIC X           VALUE 'N'.   EL326
00228          88  BYPASS-READ                             VALUE 'Y'.   EL326
00229                                                                   EL326
00230      05  WS-WORK                     PIC S9(9)       VALUE ZERO.  EL326
00231      05  WS-REMAINDER                PIC S9(9)       VALUE ZERO.  EL326
00232      05  WS-LINE-COUNT               PIC S9(3)       VALUE +90.   EL326
00233      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +60.   EL326
00234      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL326
00235      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL326
00236                                                                   EL326
00237  01  FILLER                          COMP SYNC.                   EL326
00238      05  WS-RETURN-CODE              PIC S9(4)       VALUE ZERO.  EL326
00239      05  PGM-SUB                     PIC S9(4)       VALUE +326.  EL326
00240                                                                   EL326
00241  01  FILLER.                                                      EL326
00242      05  ABEND-CODE         PIC XXXX             VALUE SPACE.     EL326
00243      05  ABEND-OPTION       PIC X.                                EL326
00244      05  OLC-REPORT-NAME    PIC X(5)             VALUE 'EL326'.   EL326
00245      05  X                  PIC X                VALUE SPACES.    EL326
00246                                                                   EL326
00247      05  WS-TIME-OF-DAY.                                          EL326
00248          10  WS-TIME        PIC 9(6).                             EL326
00249          10  WS-HUN-SEC     PIC 99.                               EL326
00250      05  WS-ACCEPT-DATE.                                          EL326
00251          10  WS-AD-YY       PIC 99.                               EL326
00252          10  WS-AD-MM       PIC 99.                               EL326
00253          10  WS-AD-DD       PIC 99.                               EL326
00254      05  WS-CURRENT-BIN-DT           PIC XX VALUE LOW-VALUES.     EL326
00255                                                                   EL326
00256      05  WS-DISPLAY-COUNT            PIC ZZZ,ZZZ,ZZ9-.            EL326
00257                                                                   EL326
00258      05  WS-PREV-COMPANY-ID          PIC XXX         VALUE SPACES.EL326
00259      05  WS-AQ-COMPANY-ID            PIC XXX         VALUE SPACES.EL326
00260                                                                   EL326
00261      05  WS-HIR-CLAIM-KEY.                                        EL326
00262          10  WS-HIR-CLAIM-CO-ID      PIC XXX.                     EL326
00263          10  WS-HIR-CLAIM-CCC        PIC X(19)                    EL326
00264              VALUE LOW-VALUES            JUSTIFIED RIGHT.         EL326
00265                                                                   EL326
00266          10  FILLER REDEFINES WS-HIR-CLAIM-CCC.                   EL326
00267              15  WS-HIR-CLAIM-CARR   PIC X(1).                    EL326
00268              15  WS-HIR-CLAIM-NO     PIC X(7).                    EL326
00269              15  WS-HIR-CERT-NO      PIC X(11).                   EL326
00270                                                                   EL326
00271      05  WS-CLAIM-KEY                VALUE LOW-VALUES.            EL326
00272          10  WS-CLAIM-COMPANY-ID     PIC XXX.                     EL326
00273          10  WS-CLAIM-CARRIER        PIC X.                       EL326
00274          10  WS-CLAIM-CLAIM-NO       PIC X(7).                    EL326
00275          10  WS-CLAIM-CERT-NO        PIC X(11).                   EL326
00276                                                                   EL326
00277      05  ELCERT-FILE-STATUS                          VALUE ZERO.  EL326
00278          10  ELCERT-FILE-STATUS1     PIC X.                       EL326
00279          10  ELCERT-FILE-STATUS2     PIC X.                       EL326
00280                                                                   EL326
00281      05  ELMSTR-FILE-STATUS                          VALUE ZERO.  EL326
00282          10  ELMSTR-FILE-STATUS1     PIC X.                       EL326
00283          10  ELMSTR-FILE-STATUS2     PIC X.                       EL326
00284                                                                   EL326
00285      05  ELCNTL-FILE-STATUS          PIC XX          VALUE ZERO.  EL326
00286      05  ELTRLR-FILE-STATUS          PIC XX          VALUE ZERO.  EL326
00287      05  ELARCH-FILE-STATUS          PIC XX          VALUE ZERO.  EL326
00288      05  MPPLCY-FILE-STATUS          PIC X(02)       VALUE ZEROS. EL326
00289      05  ELACTQ-FILE-STATUS          PIC X(02)       VALUE ZEROS. EL326
00290      05  ELRETR-FILE-STATUS          PIC X(02)       VALUE ZEROS. EL326
00291                                                                   EL326
00292      05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL326
00293      05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL326
00294                                                                   EL326
00295      05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.EL326
00296                                                                   EL326
00297      EJECT                                                        EL326
00298      05  WS-ERROR-MESSAGE-AREA.                                   EL326
00299          10  FILLER                  PIC X(30)       VALUE        EL326
00300              'CLAIM NOT ON FILE'.                                 EL326
00301          10  FILLER                  PIC X(30)       VALUE        EL326
00302              'CLAIM ALREADY ON FILE'.                             EL326
00303          10  FILLER                  PIC X(30)       VALUE        EL326
00304              'CLAIM SUCCESSFULLY RESTORED'.                       EL326
00305          10  FILLER                  PIC X(30)       VALUE        EL326
00306              'CLAIM ALREADY ON FILE BYPASSED'.                    EL326
00307          10  FILLER                  PIC X(30)       VALUE        EL326
00308              'PURGED CLAIM ON FILE RESET    '.                    EL326
00309          10  FILLER                  PIC X(30)       VALUE        EL326
00310              'CONTROL CARD DUP OR OUT OF SEQ'.                    EL326
00311                                                                   EL326
00312      05  WS-ERROR-MESSAGE            REDEFINES                    EL326
00313          WS-ERROR-MESSAGE-AREA       PIC X(30)                    EL326
00314          OCCURS 06 TIMES.                                         EL326
00315                                                                   EL326
00316      05  WS-ERROR-FLAG-AREA          PIC X(06)       VALUE SPACES.EL326
00317                                                                   EL326
00318      05  WS-ERROR-FLAG               REDEFINES                    EL326
00319          WS-ERROR-FLAG-AREA          PIC X                        EL326
00320          OCCURS 06 TIMES.                                         EL326
00321                                                                   EL326
00322      05  WS-ERROR-INDEX              PIC S9(4)       VALUE ZERO   EL326
00323                                      COMP                         EL326
00324                                      SYNC.                        EL326
00325                                                                   EL326
00326      05  WS-ERROR-INDEX-MAX          PIC S9(4)       VALUE +06    EL326
00327                                      COMP                         EL326
00328                                      SYNC.                        EL326
00329                                                                   EL326
00330      EJECT                                                        EL326
00331  01  WS-HEADING1.                                                 EL326
00332      05  FILLER                     PIC X(47) VALUE '1'.          EL326
00333      05  FILLER                     PIC X(32) VALUE               EL326
00334                            'RESTORE CLAIMS FROM HISTORY FILE'.    EL326
00335      05  FILLER                     PIC X(40) VALUE SPACES.       EL326
00336      05  FILLER                     PIC X(08) VALUE 'EL326'.      EL326
00337                                                                   EL326
00338                                                                   EL326
00339  01  WS-HEADING2.                                                 EL326
00340      05  FILLER                      PIC X       VALUE SPACE.     EL326
00341      05  WS-H2-CO-ID                 PIC XXX     VALUE SPACE.     EL326
00342      05  FILLER                      PIC X(44)   VALUE SPACE.     EL326
00343      05  WS-H2-CLIENT-NAME           PIC X(30)   VALUE SPACE.     EL326
00344      05  FILLER                      PIC X(41)   VALUE SPACES.    EL326
00345      05  WS-H2-DATE                  PIC X(8)    VALUE SPACES.    EL326
00346                                                                   EL326
00347  01  WS-HEADING3.                                                 EL326
00348      05  FILLER                      PIC X(53) VALUE SPACE.       EL326
00349      05  WS-H3-DATE                  PIC X(18).                   EL326
00350      05  FILLER                      PIC X(48) VALUE SPACES.      EL326
00351      05  FILLER                      PIC X(05) VALUE 'PAGE'.      EL326
00352      05  WS-H3-PAGE                  PIC ZZZ9.                    EL326
00353                                                                   EL326
00354  01  WS-HEADING4.                                                 EL326
00355      05  FILLER                      PIC X(56) VALUE              EL326
00356          '0     CARR  CLAIM   CERT NO    NAME'.                   EL326
00357      05  FILLER                      PIC X(77) VALUE              EL326
00358          '       TRAILERS   LETTERS   MESSAGE'.                   EL326
00359                                                                   EL326
00360      EJECT                                                        EL326
00361  01  WS-DETAIL-LINE1                 PIC X(133)      VALUE SPACES.EL326
00362                                                                   EL326
00363  01  FILLER                          REDEFINES                    EL326
00364      WS-DETAIL-LINE1.                                             EL326
00365      05  FILLER                      PIC X.                       EL326
00366      05  FILLER                      PIC X(6).                    EL326
00367      05  WS-D1-CARRIER               PIC X.                       EL326
00368      05  FILLER                      PIC XXX.                     EL326
00369      05  WS-D1-CLAIM-NO              PIC X(7).                    EL326
00370      05  FILLER                      PIC X.                       EL326
00371      05  WS-D1-CERT-NO               PIC X(11).                   EL326
00372      05  FILLER                      PIC X.                       EL326
00373      05  WS-D1-NAME                  PIC X(30).                   EL326
00374      05  FILLER                      PIC X.                       EL326
00375      05  WS-D1-TRAILERS              PIC Z,ZZZ,ZZZ.               EL326
00376      05  FILLER                      PIC X.                       EL326
00377      05  WS-D1-LETTERS               PIC Z,ZZZ,ZZZ.               EL326
00378      05  FILLER                      PIC X(4).                    EL326
00379      05  WS-D1-MESSAGE               PIC X(30).                   EL326
00380      05  FILLER                      PIC X.                       EL326
00381      05  WS-D1-FILE-STATUS           PIC XX.                      EL326
00382                                                                   EL326
00383  01  FILLER                          REDEFINES                    EL326
00384      WS-DETAIL-LINE1.                                             EL326
00385      05  FILLER                      PIC XX.                      EL326
00386      05  WS-D2-MESSAGE               PIC X(30).                   EL326
00387      05  FILLER                      PIC X(5).                    EL326
00388      05  WS-D2-COUNT                 PIC ZZZ,ZZZ,ZZ9-.            EL326
00389                                                                   EL326
00390  01  WS-DISPLAY3.                                                 EL326
00391      05  WS-D3-TIME                  PIC 99B99B99.                EL326
00392      05  FILLER                      PIC X           VALUE SPACES.EL326
00393      05  WS-D3-MESSAGE               PIC X(30).                   EL326
00394      05  WS-D3-COUNT                 PIC ZZZ,ZZZ,ZZ9-.            EL326
00395                                                                   EL326
00396      EJECT                                                        EL326
00397                                  COPY ELCNWA.                     EL326
00398                                                                   EL326
00399                                  COPY ELCDATE.                    EL326
00400                                                                   EL326
00401      EJECT                                                        EL326
00402  PROCEDURE DIVISION.                                              EL326
00403                                                                   EL326
00404  0000-BEGIN-PROCESS  SECTION.                                     EL326
00405                                                                   EL326
00406      SORT SORT-WORK ASCENDING KEY SRT-COMPANY-ID                  EL326
00407                                  SRT-CARRIER                      EL326
00408      INPUT  PROCEDURE 0000-CC-SORT  THRU  0099-EXIT               EL326
00409      OUTPUT PROCEDURE 0500-RESTORE-PROCESS  THRU  4999-EXIT.      EL326
00410                                                                   EL326
00411      IF SORT-RETURN GREATER THAN ZERO                             EL326
00412          MOVE SORT-RETURN      TO  WS-RETURN-CODE                 EL326
00413          MOVE 'SORT-FAILED - ABORT'                               EL326
00414                                TO  WS-ABEND-MESSAGE               EL326
00415          PERFORM ABEND-PGM.                                       EL326
00416                                                                   EL326
00417      PERFORM CLOSE-FILES.                                         EL326
00418                                                                   EL326
00419      GOBACK.                                                      EL326
00420                                                                   EL326
00421  0000-EXIT.                                                       EL326
00422      EXIT.                                                        EL326
00423                                                                   EL326
00424      EJECT                                                        EL326
00425  0000-CC-SORT  SECTION.                                           EL326
00426                                                                   EL326
00427      OPEN INPUT CONTROL-CARD-FILE.                                EL326
00428                                                                   EL326
00429  0000-CC-READ.                                                    EL326
00430                                                                   EL326
00431      READ CONTROL-CARD-FILE                                       EL326
00432          AT END                                                   EL326
00433              CLOSE CONTROL-CARD-FILE                              EL326
00434              GO TO 0099-EXIT.                                     EL326
00435                                                                   EL326
00436      MOVE CONTROL-CARD-RECORD    TO  SORT-REC.                    EL326
00437                                                                   EL326
00438      RELEASE SORT-REC.                                            EL326
00439                                                                   EL326
00440      GO TO 0000-CC-READ.                                          EL326
00441                                                                   EL326
00442  0099-EXIT.                                                       EL326
00443      EXIT.                                                        EL326
00444                                                                   EL326
00445      EJECT                                                        EL326
00446  0500-RESTORE-PROCESS SECTION.                                    EL326
00447                                                                   EL326
00448      OPEN INPUT  ELCNTL                                           EL326
00449           I-O    ELACTQ                                           EL326
00450           OUTPUT PRNTR.                                           EL326
00451                                                                   EL326
00452      IF ELCNTL-FILE-STATUS  EQUAL '00' OR '97'                    EL326
00453          NEXT SENTENCE                                            EL326
00454      ELSE                                                         EL326
00455          MOVE 'ERROR OCCURED OPEN - ELCNTL'                       EL326
00456                                  TO  WS-ABEND-MESSAGE             EL326
00457          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
00458          GO TO ABEND-PGM.                                         EL326
00459                                                                   EL326
00460      IF ELACTQ-FILE-STATUS  EQUAL '00' OR '97'                    EL326
00461          NEXT SENTENCE                                            EL326
00462      ELSE                                                         EL326
00463          MOVE 'ERROR OCCURED OPEN - ELACTQ'                       EL326
00464                                  TO  WS-ABEND-MESSAGE             EL326
00465          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
00466          GO TO ABEND-PGM.                                         EL326
00467                                                                   EL326
00468      PERFORM OPEN-FILES.                                          EL326
00469                                                                   EL326
00470      MOVE SPACES                 TO  CF-CONTROL-PRIMARY.          EL326
00471                                                                   EL326
00472      START ELCNTL KEY NOT LESS THAN CF-CONTROL-PRIMARY.           EL326
00473                                                                   EL326
00474      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL326
00475          NEXT SENTENCE                                            EL326
00476      ELSE                                                         EL326
00477          MOVE 'ERROR OCCURED START - ELCNTL'                      EL326
00478                                  TO  WS-ABEND-MESSAGE             EL326
00479          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
00480          GO TO ABEND-PGM.                                         EL326
00481                                                                   EL326
00482  0500-READ-NEXT-ELCNTL.                                           EL326
00483                                                                   EL326
00484      READ ELCNTL NEXT RECORD.                                     EL326
00485                                                                   EL326
00486      IF ELCNTL-FILE-STATUS  EQUAL '10' OR '23'                    EL326
00487          PERFORM  1900-COMPANY-TOTALS  THRU  1900-EXIT            EL326
00488          GO TO 1950-OVERALL-TOTALS.                               EL326
00489                                                                   EL326
00490      IF ELCNTL-FILE-STATUS  NOT = '00'                            EL326
00491          MOVE 'ERROR OCCURED READNEXT - ELCNTL'                   EL326
00492                                  TO  WS-ABEND-MESSAGE             EL326
00493          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
00494          GO TO ABEND-PGM.                                         EL326
00495                                                                   EL326
00496      IF NOT CF-COMPANY-MASTER                                     EL326
00497          GO TO 0500-READ-NEXT-ELCNTL.                             EL326
00498                                                                   EL326
00499      DISPLAY 'BEGIN PROCESSING ' CF-COMPANY-ID UPON CONSOLE.      EL326
00500                                                                   EL326
00501      PERFORM 3000-ACTQ-START.                                     EL326
00502                                                                   EL326
00503      IF FIRST-TIME                                                EL326
00504          MOVE 'N'                TO  FIRST-TIME-SW                EL326
00505          PERFORM 2000-RETURN-CARR-PULL.                           EL326
00506                                                                   EL326
00507  0600-SELECT-RESTORE.                                             EL326
00508                                                                   EL326
00509      IF NOT RETURN-RECORD AND                                     EL326
00510         NOT HAVE-ACTIVITY                                         EL326
00511          GO TO 0500-READ-NEXT-ELCNTL.                             EL326
00512                                                                   EL326
00513      IF NOT HAVE-ACTIVITY AND                                     EL326
00514         CF-COMPANY-ID LESS THAN SRT-COMPANY-ID                    EL326
00515          GO TO 0500-READ-NEXT-ELCNTL.                             EL326
00516                                                                   EL326
00517      IF SRT-COMPANY-ID = WS-AQ-COMPANY-ID AND                     EL326
00518         SRT-CARRIER    = AQ-CARRIER                               EL326
00519          PERFORM 3100-ACTQ-READ UNTIL                             EL326
00520                  SRT-COMPANY-ID NOT EQUAL WS-AQ-COMPANY-ID.       EL326
00521                                                                   EL326
00522      IF SRT-COMPANY-ID NOT GREATER THAN WS-AQ-COMPANY-ID AND      EL326
00523         SRT-CARRIER    NOT GREATER THAN AQ-CARRIER                EL326
00524          MOVE SRT-COMPANY-ID     TO  WS-CLAIM-COMPANY-ID          EL326
00525          MOVE SRT-CARRIER        TO  WS-CLAIM-CARRIER             EL326
00526          MOVE SPACES             TO  WS-CLAIM-CLAIM-NO            EL326
00527                                      WS-CLAIM-CERT-NO             EL326
00528          MOVE 'Y'                TO  WS-READ-RETURN-SW            EL326
00529          MOVE 'Y'                TO  WS-PULL-CARRIER-SW           EL326
00530      ELSE                                                         EL326
00531          MOVE WS-AQ-COMPANY-ID   TO  WS-CLAIM-COMPANY-ID          EL326
00532          MOVE AQ-CARRIER         TO  WS-CLAIM-CARRIER             EL326
00533          MOVE AQ-CLAIM-NO        TO  WS-CLAIM-CLAIM-NO            EL326
00534          MOVE AQ-CERT-NO         TO  WS-CLAIM-CERT-NO             EL326
00535          MOVE 'N'                TO  WS-PULL-CARRIER-SW           EL326
00536          MOVE 'Y'                TO  WS-READ-ACTQ-SW.             EL326
00537                                                                   EL326
00538      IF WS-PREV-COMPANY-ID = SPACES                               EL326
00539          MOVE WS-CLAIM-COMPANY-ID   TO  WS-PREV-COMPANY-ID.       EL326
00540                                                                   EL326
00541      IF WS-CLAIM-COMPANY-ID NOT = WS-PREV-COMPANY-ID              EL326
00542          PERFORM 1900-COMPANY-TOTALS  THRU  1900-EXIT             EL326
00543          MOVE WS-CLAIM-COMPANY-ID   TO  WS-PREV-COMPANY-ID.       EL326
00544                                                                   EL326
00545      PERFORM 1000-PROCESS-HISTORY  THRU  1999-EXIT.               EL326
00546                                                                   EL326
00547      IF READ-RETURN-RECORD                                        EL326
00548          PERFORM 2000-RETURN-CARR-PULL                            EL326
00549          MOVE 'N'                TO  WS-READ-RETURN-SW            EL326
00550      ELSE                                                         EL326
00551          MOVE 'N'                TO  WS-READ-ACTQ-SW              EL326
00552          PERFORM 3100-ACTQ-READ.                                  EL326
00553                                                                   EL326
00554      GO TO 0600-SELECT-RESTORE.                                   EL326
00555                                                                   EL326
00556  EJECT                                                            EL326
00557  1000-PROCESS-HISTORY.                                            EL326
00558      IF HISTORY-EOF                                               EL326
00559          IF NOT CLAIMS-WERE-RESTORED                              EL326
00560              MOVE '*'            TO  WS-ERROR-FLAG (01)           EL326
00561              PERFORM 1800-STATUS-PRINT  THRU  1899-EXIT           EL326
00562              GO TO 1999-EXIT.                                     EL326
00563                                                                   EL326
00564      IF BYPASS-READ                                               EL326
00565          MOVE 'N'                TO  WS-BYPASS-READ-SW            EL326
00566      ELSE                                                         EL326
00567          READ HISTORY-INPUT-FILE                                  EL326
00568              AT END                                               EL326
00569                  MOVE 'Y'        TO  WS-HISTORY-EOF               EL326
00570                  MOVE HIGH-VALUES                                 EL326
00571                                  TO  HIR-COMPANY-ID               EL326
00572                                      HIR-CLAIM-KEY.               EL326
00573                                                                   EL326
00574      IF HISTORY-EOF                                               EL326
00575          IF NOT CLAIMS-WERE-RESTORED                              EL326
00576              MOVE '*'            TO  WS-ERROR-FLAG (01)           EL326
00577              PERFORM 1800-STATUS-PRINT  THRU  1899-EXIT           EL326
00578              GO TO 1999-EXIT.                                     EL326
00579                                                                   EL326
00580      MOVE HIR-CLAIM-KEY          TO  WS-HIR-CLAIM-CCC.            EL326
00581      MOVE HIR-COMPANY-ID         TO  WS-HIR-CLAIM-CO-ID.          EL326
00582                                                                   EL326
00583      IF PULL-CARRIER                                              EL326
00584          MOVE SPACES             TO WS-HIR-CLAIM-NO               EL326
00585                                     WS-HIR-CERT-NO.               EL326
00586                                                                   EL326
00587  1030-MAIN-LOGIC.                                                 EL326
00588                                                                   EL326
00589      IF WS-HIR-CLAIM-KEY LESS THAN WS-CLAIM-KEY                   EL326
00590          GO TO 1000-PROCESS-HISTORY.                              EL326
00591                                                                   EL326
00592      IF WS-HIR-CLAIM-KEY GREATER THAN WS-CLAIM-KEY                EL326
00593          IF NOT CLAIMS-WERE-RESTORED                              EL326
00594              MOVE '*'            TO  WS-ERROR-FLAG (01)           EL326
00595           END-IF                                                  EL326
00596           PERFORM 1800-STATUS-PRINT  THRU  1899-EXIT              EL326
00597           MOVE 'Y'               TO  WS-BYPASS-READ-SW            EL326
00598           GO TO 1999-EXIT.                                        EL326
00599                                                                   EL326
00600      IF HIR-RECORD-ID NOT EQUAL 'CL'                              EL326
00601          GO TO 1100-MAIN-LOGIC.                                   EL326
00602                                                                   EL326
00603      MOVE HIR-CLAIM-RECORD       TO  CLAIM-MASTER.                EL326
00604                                                                   EL326
00605      MOVE '0'                    TO  WS-DETAIL-LINE1.             EL326
00606      MOVE CL-CARRIER             TO  WS-D1-CARRIER.               EL326
00607      MOVE CL-CLAIM-NO            TO  WS-D1-CLAIM-NO.              EL326
00608      MOVE CL-CERT-NO             TO  WS-D1-CERT-NO.               EL326
00609      PERFORM 5000-MOVE-NAME.                                      EL326
00610      MOVE WS-NAME-WORK           TO  WS-D1-NAME.                  EL326
00611                                                                   EL326
00612      MOVE WS-CURRENT-BIN-DT      TO  CL-LAST-MAINT-DT.            EL326
00613      MOVE LOW-VALUES             TO  CL-PURGED-DT.                EL326
00614      ACCEPT WS-TIME-OF-DAY FROM  TIME.                            EL326
00615      MOVE WS-TIME                TO  CL-LAST-MAINT-HHMMSS.        EL326
00616      MOVE '4'                    TO  CL-LAST-MAINT-TYPE.          EL326
00617                                                                   EL326
00618      WRITE CLAIM-MASTER.                                          EL326
00619                                                                   EL326
00620      EVALUATE TRUE                                                EL326
00621         WHEN ELMSTR-FILE-STATUS = ZERO OR '02'                    EL326
00622            PERFORM 6000-VERIFY-NO-RETRIEVE                        EL326
00623         WHEN ELMSTR-FILE-STATUS = '22'                            EL326
00624            MOVE '*'               TO  WS-ERROR-FLAG (04)          EL326
00625            MOVE 'Y'               TO  CO-CLAIMS-RESTORED-SW       EL326
00626            PERFORM 1050-REWRITE-ELMSTR-REC THRU 1050-EXIT         EL326
00627            PERFORM 6000-VERIFY-NO-RETRIEVE                        EL326
00628            GO TO 1000-PROCESS-HISTORY                             EL326
00629         WHEN OTHER                                                EL326
00630            MOVE 'ERROR OCCURED WRITE - ELMSTR'                    EL326
00631                                        TO  WS-ABEND-MESSAGE       EL326
00632            MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS       EL326
00633            GO TO ABEND-PGM                                        EL326
00634      END-EVALUATE.                                                EL326
00635                                                                   EL326
00636      MOVE 'Y'                    TO  CO-CLAIMS-RESTORED-SW.       EL326
00637                                                                   EL326
00638      MOVE '*'                    TO  WS-ERROR-FLAG (3).           EL326
00639                                                                   EL326
00640      ADD +1  TO  CO-CLAIMS-PROCESSED.                             EL326
00641                                                                   EL326
00642  1040-MAIN-LOGIC.                                                 EL326
00643                                                                   EL326
00644      GO TO 1000-PROCESS-HISTORY.                                  EL326
00645                                                                   EL326
00646  1050-REWRITE-ELMSTR-REC.                                         EL326
00647                                                                   EL326
00648      READ ELMSTR.                                                 EL326
00649                                                                   EL326
00650      IF ELMSTR-FILE-STATUS NOT EQUAL '00'                         EL326
00651         MOVE 'ERROR OCCURED READ - ELMSTR'  TO  WS-ABEND-MESSAGE  EL326
00652         MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS          EL326
00653         GO TO ABEND-PGM.                                          EL326
00654                                                                   EL326
00655      IF CL-PURGED-DT NOT EQUAL LOW-VALUES                         EL326
00656         MOVE LOW-VALUES TO CL-PURGED-DT                           EL326
00657         REWRITE CLAIM-MASTER                                      EL326
00658         IF ELMSTR-FILE-STATUS NOT EQUAL '00'                      EL326
00659            MOVE 'ERROR OCCURED REWRITE - ELMSTR'                  EL326
00660                                 TO  WS-ABEND-MESSAGE              EL326
00661            MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS       EL326
00662            GO TO ABEND-PGM                                        EL326
00663         ELSE                                                      EL326
00664            ADD +1  TO  CO-CLAIMS-RESET                            EL326
00665            MOVE ' '             TO  WS-ERROR-FLAG (04)            EL326
00666            MOVE '*'             TO  WS-ERROR-FLAG (05).           EL326
00667                                                                   EL326
00668  1050-EXIT.                                                       EL326
00669      EXIT.                                                        EL326
00670      EJECT                                                        EL326
00671                                                                   EL326
00672  1100-MAIN-LOGIC.                                                 EL326
00673      IF NOT CLAIMS-WERE-RESTORED                                  EL326
00674         DISPLAY 'LOGIC ERROR PROCESS SW = 0 ' UPON CONSOLE        EL326
00675         GO TO 1000-PROCESS-HISTORY.                               EL326
00676                                                                   EL326
00677      IF HIR-RECORD-ID NOT EQUAL 'AT'                              EL326
00678          GO TO 1200-MAIN-LOGIC.                                   EL326
00679                                                                   EL326
00680      IF NOT PULL-CARRIER                                          EL326
00681          IF AQ-PENDING-CLAIM-RESTORE NOT = 'C' AND 'B'            EL326
00682              GO TO 1200-MAIN-LOGIC.                               EL326
00683                                                                   EL326
00684      MOVE HIR-ACTIVITY-TRAILER-RECORD  TO  ACTIVITY-TRAILERS.     EL326
00685                                                                   EL326
00686      IF AT-TRAILER-TYPE EQUAL '2'                                 EL326
00687          MOVE +99999999          TO  AT-CHECK-QUE-CONTROL         EL326
00688          MOVE ZERO               TO  AT-CHECK-QUE-SEQUENCE.       EL326
00689                                                                   EL326
00690      WRITE ACTIVITY-TRAILERS.                                     EL326
00691                                                                   EL326
00692      IF ELTRLR-FILE-STATUS EQUAL '22'                             EL326
00693         GO TO 1190-MAIN-LOGIC.                                    EL326
00694                                                                   EL326
00695      IF ELTRLR-FILE-STATUS NOT EQUAL ZERO                         EL326
00696          MOVE 'ERROR OCCURED WRITE - ELTRLR' TO WS-ABEND-MESSAGE  EL326
00697          MOVE ELTRLR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
00698          GO TO ABEND-PGM.                                         EL326
00699                                                                   EL326
00700      ADD +1  TO  CL-TRAILERS-PROCESSED.                           EL326
00701                                                                   EL326
00702  1190-MAIN-LOGIC.                                                 EL326
00703                                                                   EL326
00704      GO TO 1000-PROCESS-HISTORY.                                  EL326
00705                                                                   EL326
00706      EJECT                                                        EL326
00707  1200-MAIN-LOGIC.                                                 EL326
00708                                                                   EL326
00709      IF HIR-RECORD-ID NOT EQUAL 'LA'                              EL326
00710         GO TO 1300-MAIN-LOGIC.                                    EL326
00711                                                                   EL326
00712      IF NOT PULL-CARRIER                                          EL326
00713          IF AQ-PENDING-CLAIM-RESTORE NOT = 'L' AND 'B'            EL326
00714              GO TO 1300-MAIN-LOGIC.                               EL326
00715                                                                   EL326
00716      MOVE HIR-LETTER-ARCHIVE-RECORD  TO  LETTER-ARCHIVE.          EL326
00717                                                                   EL326
00718      WRITE LETTER-ARCHIVE.                                        EL326
00719                                                                   EL326
00720      IF ELARCH-FILE-STATUS EQUAL '22'                             EL326
00721         GO TO 1290-MAIN-LOGIC.                                    EL326
00722                                                                   EL326
00723      IF ELARCH-FILE-STATUS NOT EQUAL ZERO                         EL326
00724          MOVE 'ERROR OCCURED WRITE - ELARCH' TO WS-ABEND-MESSAGE  EL326
00725          MOVE ELARCH-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
00726          GO TO ABEND-PGM.                                         EL326
00727                                                                   EL326
00728      ADD +1  TO  CL-LETTERS-PROCESSED.                            EL326
00729                                                                   EL326
00730  1290-MAIN-LOGIC.                                                 EL326
00731                                                                   EL326
00732      GO TO 1000-PROCESS-HISTORY.                                  EL326
00733                                                                   EL326
00734      EJECT                                                        EL326
00735  1300-MAIN-LOGIC.                                                 EL326
00736      IF HIR-RECORD-ID NOT EQUAL 'CM'                              EL326
00737          GO TO 1400-MAIN-LOGIC.                                   EL326
00738                                                                   EL326
00739      IF ELMSTR-FILE-STATUS EQUAL '22'                             EL326
00740         GO TO 1000-PROCESS-HISTORY.                               EL326
00741                                                                   EL326
00742      MOVE HIR-CERTIFICATE-RECORD  TO  CERTIFICATE-MASTER.         EL326
00743                                                                   EL326
00744      READ ELCERT.                                                 EL326
00745                                                                   EL326
00746      IF ELCERT-FILE-STATUS NOT EQUAL '23'                         EL326
00747          GO TO 1320-MAIN-LOGIC.                                   EL326
00748                                                                   EL326
00749      MOVE HIR-CERTIFICATE-RECORD  TO  CERTIFICATE-MASTER.         EL326
00750                                                                   EL326
00751      MOVE +1                     TO  CM-CLAIM-ATTACHED-COUNT.     EL326
00752      INSPECT CM-CLAIM-INTERFACE-SW CONVERTING ' 12' TO '112'.     EL326
00753                                                                   EL326
00754      WRITE CERTIFICATE-MASTER.                                    EL326
00755                                                                   EL326
00756      IF ELCERT-FILE-STATUS1 NOT EQUAL ZERO                        EL326
00757          MOVE 'ERROR OCCURED WRITE - ELCERT' TO WS-ABEND-MESSAGE  EL326
00758          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
00759          GO TO ABEND-PGM.                                         EL326
00760                                                                   EL326
00761      ADD +1  TO  CO-CERTS-WRITTEN.                                EL326
00762      GO TO 1000-PROCESS-HISTORY.                                  EL326
00763                                                                   EL326
00764  1320-MAIN-LOGIC.                                                 EL326
00765      ADD +1  TO  CM-CLAIM-ATTACHED-COUNT.                         EL326
00766      INSPECT CM-CLAIM-INTERFACE-SW CONVERTING ' 12' TO '112'.     EL326
00767                                                                   EL326
00768      REWRITE CERTIFICATE-MASTER.                                  EL326
00769                                                                   EL326
00770      IF ELCERT-FILE-STATUS1 NOT EQUAL ZERO                        EL326
00771          MOVE 'ERROR OCCURED REWRITE - ELCERT' TO WS-ABEND-MESSAGEEL326
00772          MOVE ELCERT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
00773          GO TO ABEND-PGM.                                         EL326
00774                                                                   EL326
00775      ADD +1  TO  CO-CERTS-REWRITTEN.                              EL326
00776                                                                   EL326
00777      GO TO 1000-PROCESS-HISTORY.                                  EL326
00778                                                                   EL326
00779      EJECT                                                        EL326
00780  1400-MAIN-LOGIC.                                                 EL326
00781      IF HIR-RECORD-ID NOT EQUAL 'PM'                              EL326
00782          GO TO 1000-PROCESS-HISTORY.                              EL326
00783                                                                   EL326
00784      IF ELMSTR-FILE-STATUS EQUAL '22'                             EL326
00785         GO TO 1000-PROCESS-HISTORY.                               EL326
00786                                                                   EL326
PEMTST*    MOVE HIR-POLICY-RECORD      TO  POLICY-MASTER.               EL326
00788                                                                   EL326
00789      READ MPPLCY.                                                 EL326
00790                                                                   EL326
00791      IF MPPLCY-FILE-STATUS NOT EQUAL '23'                         EL326
00792          GO TO 1420-MAIN-LOGIC.                                   EL326
00793                                                                   EL326
PEMTST*    MOVE HIR-POLICY-RECORD      TO  POLICY-MASTER.               EL326
00795                                                                   EL326
00796      MOVE +1                     TO  PM-CLAIM-ATTACH-CNT.         EL326
00797      INSPECT PM-CLAIM-INTERFACE-SW CONVERTING ' 12' TO '112'.     EL326
00798                                                                   EL326
00799      WRITE POLICY-MASTER.                                         EL326
00800                                                                   EL326
00801      IF MPPLCY-FILE-STATUS NOT EQUAL ZERO                         EL326
00802          MOVE 'ERROR OCCURED WRITE - MPPLCY' TO WS-ABEND-MESSAGE  EL326
00803          MOVE MPPLCY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
00804          GO TO ABEND-PGM.                                         EL326
00805                                                                   EL326
00806      ADD +1                      TO  CO-POLICIES-PROCESSED.       EL326
00807      GO TO 1000-PROCESS-HISTORY.                                  EL326
00808                                                                   EL326
00809  1420-MAIN-LOGIC.                                                 EL326
00810      ADD +1                      TO  PM-CLAIM-ATTACH-CNT.         EL326
00811      INSPECT PM-CLAIM-INTERFACE-SW CONVERTING ' 12' TO '112'.     EL326
00812                                                                   EL326
00813      REWRITE POLICY-MASTER.                                       EL326
00814                                                                   EL326
00815      IF MPPLCY-FILE-STATUS NOT EQUAL ZERO                         EL326
00816          MOVE 'ERROR OCCURED REWRITE - MPPLCY' TO WS-ABEND-MESSAGEEL326
00817          MOVE MPPLCY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
00818          GO TO ABEND-PGM.                                         EL326
00819                                                                   EL326
00820      ADD +1                      TO  CO-POLICIES-PROCESSED.       EL326
00821                                                                   EL326
00822      GO TO 1000-PROCESS-HISTORY.                                  EL326
00823                                                                   EL326
00824  1999-EXIT.                                                       EL326
00825      EXIT.                                                        EL326
00826      EJECT                                                        EL326
00827  1800-STATUS-PRINT.                                               EL326
00828 *    NOTE ******************************************************* EL326
00829 *         *      THIS SECTION CONTROLS THE PRINTING OF THE      * EL326
00830 *         *  TRANSACTIONS AND ANY ASSOCIATED ERROR MESSAGES.    * EL326
00831 *         *******************************************************.EL326
00832                                                                   EL326
00833      MOVE +1                     TO  WS-ERROR-INDEX.              EL326
00834                                                                   EL326
00835  1820-MAIN-LOGIC.                                                 EL326
00836                                                                   EL326
00837      IF PULL-CARRIER                                              EL326
00838          MOVE SPACES             TO  WS-DETAIL-LINE1              EL326
00839          MOVE WS-CLAIM-CARRIER   TO  WS-D1-CARRIER                EL326
00840          MOVE CL-LETTERS-PROCESSED                                EL326
00841                                  TO  WS-D1-LETTERS                EL326
00842          MOVE CL-TRAILERS-PROCESSED                               EL326
00843                                  TO  WS-D1-TRAILERS               EL326
00844          MOVE WS-DETAIL-LINE1    TO  PRT                          EL326
00845          PERFORM WRITE-A-LINE                                     EL326
00846          MOVE SPACES             TO  WS-DETAIL-LINE1              EL326
00847          GO TO 1830-MAIN-LOGIC.                                   EL326
00848                                                                   EL326
00849      IF WS-ERROR-FLAG-AREA EQUAL SPACES                           EL326
00850          GO TO 1830-MAIN-LOGIC.                                   EL326
00851                                                                   EL326
00852      IF WS-ERROR-FLAG (WS-ERROR-INDEX) NOT EQUAL SPACES           EL326
00853          MOVE WS-ERROR-MESSAGE (WS-ERROR-INDEX)                   EL326
00854                                  TO  WS-D1-MESSAGE                EL326
00855          MOVE WS-CLAIM-CARRIER   TO  WS-D1-CARRIER                EL326
00856          MOVE WS-CLAIM-CLAIM-NO  TO  WS-D1-CLAIM-NO               EL326
00857          MOVE WS-CLAIM-CERT-NO   TO  WS-D1-CERT-NO                EL326
00858          MOVE CL-LETTERS-PROCESSED                                EL326
00859                                  TO  WS-D1-LETTERS                EL326
00860          MOVE CL-TRAILERS-PROCESSED                               EL326
00861                                  TO  WS-D1-TRAILERS               EL326
00862          MOVE WS-DETAIL-LINE1    TO  PRT                          EL326
00863          PERFORM WRITE-A-LINE                                     EL326
00864          MOVE SPACES             TO  WS-DETAIL-LINE1.             EL326
00865                                                                   EL326
00866      IF WS-ERROR-INDEX LESS THAN WS-ERROR-INDEX-MAX               EL326
00867          ADD +1  TO  WS-ERROR-INDEX                               EL326
00868          GO TO 1820-MAIN-LOGIC.                                   EL326
00869                                                                   EL326
00870  1830-MAIN-LOGIC.                                                 EL326
00871      IF WS-DETAIL-LINE1 NOT EQUAL SPACES                          EL326
00872          MOVE WS-DETAIL-LINE1    TO  PRT                          EL326
00873          PERFORM WRITE-A-LINE.                                    EL326
00874                                                                   EL326
00875      MOVE SPACES                 TO  WS-ERROR-FLAG-AREA.          EL326
00876                                                                   EL326
00877      MOVE 'N'                    TO  CO-CLAIMS-RESTORED-SW.       EL326
00878      ADD CL-TRAILERS-PROCESSED TO CO-TRAILERS-PROCESSED.          EL326
00879      ADD CL-LETTERS-PROCESSED  TO CO-LETTERS-PROCESSED.           EL326
00880      MOVE ZEROS                  TO  CL-TRAILERS-PROCESSED        EL326
00881                                      CL-LETTERS-PROCESSED.        EL326
00882                                                                   EL326
00883  1899-EXIT.                                                       EL326
00884      EXIT.                                                        EL326
00885                                                                   EL326
00886      EJECT                                                        EL326
00887  1900-COMPANY-TOTALS.                                             EL326
00888                                                                   EL326
00889      IF WS-PREV-COMPANY-ID = SPACES                               EL326
00890          GO TO 1900-EXIT.                                         EL326
00891                                                                   EL326
00892      MOVE '0'                    TO  WS-DETAIL-LINE1.             EL326
00893                                                                   EL326
00894      MOVE CO-CLAIMS-PROCESSED    TO  WS-D2-COUNT.                 EL326
00895      MOVE 'CLAIMS ADDED'         TO  WS-D2-MESSAGE.               EL326
00896      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL326
00897      PERFORM WRITE-A-LINE.                                        EL326
00898                                                                   EL326
00899      MOVE CO-CLAIMS-RESET        TO  WS-D2-COUNT.                 EL326
00900      MOVE 'PURGED CLAIMS RESET'  TO  WS-D2-MESSAGE.               EL326
00901      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL326
00902      PERFORM WRITE-A-LINE.                                        EL326
00903                                                                   EL326
00904      MOVE CO-TRAILERS-PROCESSED  TO  WS-D2-COUNT.                 EL326
00905      MOVE 'TRAILERS ADDED'       TO  WS-D2-MESSAGE.               EL326
00906      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL326
00907      PERFORM WRITE-A-LINE.                                        EL326
00908                                                                   EL326
00909      MOVE CO-LETTERS-PROCESSED   TO  WS-D2-COUNT.                 EL326
00910      MOVE 'LETTERS ADDED'        TO  WS-D2-MESSAGE.               EL326
00911      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL326
00912      PERFORM WRITE-A-LINE.                                        EL326
00913                                                                   EL326
00914      MOVE CO-CERTS-WRITTEN       TO  WS-D2-COUNT.                 EL326
00915      MOVE 'CERTS WRITTEN             '                            EL326
00916                                  TO  WS-D2-MESSAGE.               EL326
00917      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL326
00918      PERFORM WRITE-A-LINE.                                        EL326
00919                                                                   EL326
00920      MOVE CO-CERTS-REWRITTEN     TO  WS-D2-COUNT.                 EL326
00921      MOVE 'CERTS REWRITTEN           '                            EL326
00922                                  TO  WS-D2-MESSAGE.               EL326
00923      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL326
00924      PERFORM WRITE-A-LINE.                                        EL326
00925                                                                   EL326
00926      ADD CO-CLAIMS-PROCESSED    TO  OV-CLAIMS-PROCESSED,          EL326
00927      ADD CO-CLAIMS-RESET        TO  OV-CLAIMS-RESET.              EL326
00928      ADD CO-TRAILERS-PROCESSED  TO  OV-TRAILERS-PROCESSED.        EL326
00929      ADD CO-LETTERS-PROCESSED   TO  OV-LETTERS-PROCESSED.         EL326
00930      ADD CO-CERTS-WRITTEN       TO  OV-CERTS-WRITTEN.             EL326
00931      ADD CO-CERTS-REWRITTEN     TO  OV-CERTS-REWRITTEN.           EL326
00932                                                                   EL326
00933      MOVE ZEROS                TO   CO-CLAIMS-PROCESSED           EL326
00934                                     CO-CLAIMS-RESET               EL326
00935                                     CO-TRAILERS-PROCESSED         EL326
00936                                     CO-LETTERS-PROCESSED          EL326
00937                                     CO-CERTS-WRITTEN              EL326
00938                                     CO-CERTS-REWRITTEN.           EL326
00939      MOVE SPACES                TO  WS-DETAIL-LINE1.              EL326
00940      MOVE +90                   TO  WS-LINE-COUNT.                EL326
00941  1900-EXIT.                                                       EL326
00942       EXIT.                                                       EL326
00943      EJECT                                                        EL326
00944  1950-OVERALL-TOTALS.                                             EL326
00945      MOVE '0'                    TO  WS-DETAIL-LINE1.             EL326
00946      MOVE SPACES                 TO  WS-HEADING4.                 EL326
00947      MOVE +99                    TO  WS-LINE-COUNT.               EL326
00948      MOVE '        OVERALL TOTALS         '                       EL326
00949                                  TO  WS-H2-CLIENT-NAME.           EL326
00950      MOVE SPACES                 TO  WS-H2-CO-ID.                 EL326
00951                                                                   EL326
00952      MOVE OV-CLAIMS-PROCESSED    TO  WS-DISPLAY-COUNT             EL326
00953                                      WS-D2-COUNT.                 EL326
00954      MOVE 'CLAIMS ADDED'         TO  WS-D2-MESSAGE.               EL326
00955      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL326
00956      PERFORM WRITE-A-LINE.                                        EL326
00957                                                                   EL326
00958      DISPLAY 'CLAIMS ADDED    '  WS-DISPLAY-COUNT                 EL326
00959                                  UPON CONSOLE.                    EL326
00960                                                                   EL326
00961      MOVE OV-CLAIMS-RESET        TO  WS-DISPLAY-COUNT             EL326
00962                                      WS-D2-COUNT.                 EL326
00963      MOVE 'PURGED CLAIMS RESET'  TO  WS-D2-MESSAGE.               EL326
00964      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL326
00965      PERFORM WRITE-A-LINE.                                        EL326
00966                                                                   EL326
00967      DISPLAY 'PURGED CLAIMS RESET    '  WS-DISPLAY-COUNT          EL326
00968                                         UPON CONSOLE.             EL326
00969                                                                   EL326
00970      MOVE OV-TRAILERS-PROCESSED  TO  WS-DISPLAY-COUNT             EL326
00971                                      WS-D2-COUNT.                 EL326
00972      MOVE 'TRAILERS ADDED'       TO  WS-D2-MESSAGE.               EL326
00973      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL326
00974      PERFORM WRITE-A-LINE.                                        EL326
00975                                                                   EL326
00976      DISPLAY 'TRAILERS ADDED  '  WS-DISPLAY-COUNT                 EL326
00977                                  UPON CONSOLE.                    EL326
00978                                                                   EL326
00979      MOVE OV-LETTERS-PROCESSED   TO  WS-DISPLAY-COUNT             EL326
00980                                      WS-D2-COUNT.                 EL326
00981      MOVE 'LETTERS ADDED'        TO  WS-D2-MESSAGE.               EL326
00982      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL326
00983      PERFORM WRITE-A-LINE.                                        EL326
00984                                                                   EL326
00985      DISPLAY 'LETTERS ADDED   '  WS-DISPLAY-COUNT                 EL326
00986                                  UPON CONSOLE.                    EL326
00987                                                                   EL326
00988      MOVE OV-CERTS-WRITTEN       TO  WS-DISPLAY-COUNT             EL326
00989                                      WS-D2-COUNT.                 EL326
00990      MOVE 'CERTS WRITTEN             '                            EL326
00991                                  TO  WS-D2-MESSAGE.               EL326
00992      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL326
00993      PERFORM WRITE-A-LINE.                                        EL326
00994                                                                   EL326
00995      MOVE OV-CERTS-REWRITTEN     TO  WS-DISPLAY-COUNT             EL326
00996                                      WS-D2-COUNT.                 EL326
00997      MOVE 'CERTS WRITTEN             '                            EL326
00998                                  TO  WS-D2-MESSAGE.               EL326
00999      MOVE WS-DETAIL-LINE1        TO  PRT.                         EL326
01000      PERFORM WRITE-A-LINE.                                        EL326
01001                                                                   EL326
01002      DISPLAY 'CERTS REWRITTEN '  WS-DISPLAY-COUNT                 EL326
01003                                  UPON CONSOLE.                    EL326
01004                                                                   EL326
01005      GO TO 4999-EXIT.                                             EL326
01006                                                                   EL326
01007      EJECT                                                        EL326
01008                                                                   EL326
01009  2000-RETURN-CARR-PULL  SECTION.                                  EL326
01010                                                                   EL326
01011      RETURN SORT-WORK                                             EL326
01012          AT END                                                   EL326
01013              MOVE 'N'            TO  WS-RETURN-SW                 EL326
01014              MOVE HIGH-VALUES    TO  SORT-REC                     EL326
01015              MOVE 'Y'            TO  WS-SORT-EOF-SW               EL326
01016              GO TO 2099-EXIT.                                     EL326
01017                                                                   EL326
01018      MOVE 'Y'                    TO  WS-RETURN-SW.                EL326
01019                                                                   EL326
01020  2099-EXIT.                                                       EL326
01021      EXIT.                                                        EL326
01022                                                                   EL326
01023      EJECT                                                        EL326
01024  3000-ACTQ-START  SECTION.                                        EL326
01025                                                                   EL326
01026      IF DELETE-ACTQ                                               EL326
01027          PERFORM 3200-DELETE-ELACTQ  THRU  3290-EXIT.             EL326
01028                                                                   EL326
01029      IF REWRITE-ACTQ                                              EL326
01030          PERFORM 3300-REWRITE-ELACTQ  THRU  3390-EXIT.            EL326
01031                                                                   EL326
01032      MOVE 'N'                    TO  WS-HAVE-ACTIVITY-SW.         EL326
01033                                                                   EL326
01034      MOVE SPACES                 TO  AQ-CONTROL-PRIMARY.          EL326
01035      MOVE CF-COMPANY-CD          TO  AQ-CONTROL-PRIMARY.          EL326
01036                                                                   EL326
01037      START ELACTQ KEY NOT LESS THAN AQ-CONTROL-PRIMARY.           EL326
01038                                                                   EL326
01039      IF ELACTQ-FILE-STATUS = '10'                                 EL326
01040          MOVE 'Y'                TO  WS-ELACTQ-EOF                EL326
01041          GO TO 3099-EXIT.                                         EL326
01042                                                                   EL326
01043      IF ELACTQ-FILE-STATUS = '23'                                 EL326
01044          GO TO 3099-EXIT.                                         EL326
01045                                                                   EL326
01046      IF ELACTQ-FILE-STATUS  NOT = '00'                            EL326
01047          MOVE 'ERROR OCCURED START - ELACTQ'                      EL326
01048                                  TO  WS-ABEND-MESSAGE             EL326
01049          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
01050          GO TO ABEND-PGM.                                         EL326
01051                                                                   EL326
01052       PERFORM 3100-ACTQ-READ.                                     EL326
01053                                                                   EL326
01054  3099-EXIT.                                                       EL326
01055      EXIT.                                                        EL326
01056                                                                   EL326
01057      EJECT                                                        EL326
01058  3100-ACTQ-READ  SECTION.                                         EL326
01059                                                                   EL326
01060      IF DELETE-ACTQ                                               EL326
01061          PERFORM 3200-DELETE-ELACTQ  THRU  3290-EXIT.             EL326
01062                                                                   EL326
01063      IF REWRITE-ACTQ                                              EL326
01064          PERFORM 3300-REWRITE-ELACTQ  THRU  3390-EXIT.            EL326
01065                                                                   EL326
01066      READ ELACTQ NEXT RECORD.                                     EL326
01067                                                                   EL326
01068      IF ELACTQ-FILE-STATUS = '10' OR                              EL326
01069         AQ-CONTROL-PRIMARY = '99999999999999999999'               EL326
01070          MOVE 'Y'                TO  WS-ELACTQ-EOF                EL326
01071          MOVE 'N'                TO  WS-HAVE-ACTIVITY-SW          EL326
01072          MOVE HIGH-VALUES        TO  AQ-CONTROL-PRIMARY           EL326
01073                                      WS-AQ-COMPANY-ID             EL326
01074          GO TO 3199-EXIT.                                         EL326
01075                                                                   EL326
01076      IF AQ-COMPANY-CD NOT = CF-COMPANY-CD                         EL326
01077          MOVE HIGH-VALUES        TO  AQ-CONTROL-PRIMARY           EL326
01078                                      WS-AQ-COMPANY-ID             EL326
01079          MOVE 'N'                TO  WS-HAVE-ACTIVITY-SW          EL326
01080          GO TO 3199-EXIT.                                         EL326
01081                                                                   EL326
01082      IF ELACTQ-FILE-STATUS  NOT = '00'                            EL326
01083          MOVE 'ERROR OCCURED READNEXT - ELACTQ'                   EL326
01084                                  TO  WS-ABEND-MESSAGE             EL326
01085          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
01086          GO TO ABEND-PGM.                                         EL326
01087                                                                   EL326
01088      IF AQ-PENDING-CLAIM-RESTORE NOT = 'C' AND 'L' AND 'B'        EL326
01089          GO TO 3100-ACTQ-READ.                                    EL326
01090                                                                   EL326
01091      IF PENDING-PAYMENTS   OR                                     EL326
01092         PENDING-FULL-PRINT OR                                     EL326
01093         PENDING-PART-PRINT OR                                     EL326
01094         PENDING-LETTERS                                           EL326
01095          MOVE 'Y'                TO  WS-REWRITE-ACTQ-SW           EL326
01096      ELSE                                                         EL326
01097          MOVE 'Y'                TO  WS-DELETE-ACTQ-SW.           EL326
01098                                                                   EL326
01099      MOVE 'Y'                    TO  WS-HAVE-ACTIVITY-SW.         EL326
01100      MOVE CF-COMPANY-ID          TO  WS-AQ-COMPANY-ID.            EL326
01101                                                                   EL326
01102  3199-EXIT.                                                       EL326
01103      EXIT.                                                        EL326
01104                                                                   EL326
01105      EJECT                                                        EL326
01106  3200-DELETE-ELACTQ  SECTION.                                     EL326
01107                                                                   EL326
01108      DELETE ELACTQ.                                               EL326
01109                                                                   EL326
01110      IF ELACTQ-FILE-STATUS  NOT = '00'                            EL326
01111          MOVE 'ERROR OCCURED DELETE - ELACTQ'                     EL326
01112                                  TO  WS-ABEND-MESSAGE             EL326
01113          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
01114          DISPLAY AQ-CONTROL-PRIMARY                               EL326
01115          GO TO ABEND-PGM.                                         EL326
01116                                                                   EL326
01117      MOVE 'N'                    TO  WS-DELETE-ACTQ-SW.           EL326
01118                                                                   EL326
01119  3290-EXIT.                                                       EL326
01120      EXIT.                                                        EL326
01121                                                                   EL326
01122  3300-REWRITE-ELACTQ  SECTION.                                    EL326
01123                                                                   EL326
01124      MOVE SPACE                  TO  AQ-PENDING-CLAIM-RESTORE.    EL326
01125                                                                   EL326
01126      REWRITE ACTIVITY-QUE.                                        EL326
01127                                                                   EL326
01128      IF ELACTQ-FILE-STATUS  NOT = '00'                            EL326
01129          MOVE 'ERROR OCCURED REWRITE - ELACTQ'                    EL326
01130                                  TO  WS-ABEND-MESSAGE             EL326
01131          MOVE ELACTQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
01132          GO TO ABEND-PGM.                                         EL326
01133                                                                   EL326
01134      MOVE 'N'                    TO  WS-REWRITE-ACTQ-SW.          EL326
01135                                                                   EL326
01136  3390-EXIT.                                                       EL326
01137      EXIT.                                                        EL326
01138                                                                   EL326
01139  4999-EXIT.                                                       EL326
01140      EXIT.                                                        EL326
01141                                                                   EL326
01142      EJECT                                                        EL326
01143  5000-MOVE-NAME SECTION. COPY ELCMNS.                             EL326
01144                                                                   EL326
01145      EJECT                                                        EL326
01146                                                                   EL326
01147  6000-VERIFY-NO-RETRIEVE.                                         EL326
01148                                                                   EL326
01149      MOVE CLAIM-MASTER TO RETRIEVE-MASTER.                        EL326
01150                                                                   EL326
01151      MOVE 'RL'         TO RL-RECORD-ID.                           EL326
01152                                                                   EL326
01153      DELETE ELRETR RECORD.                                        EL326
01154                                                                   EL326
01155      IF ELRETR-FILE-STATUS > '23'                                 EL326
01156          MOVE 'ERROR OCCURED DURING DELETE OF ELRETR '            EL326
01157                                         TO  WS-ABEND-MESSAGE      EL326
01158          MOVE ELMSTR-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL326
01159          GO TO ABEND-PGM                                          EL326
01160      END-IF.                                                      EL326
01161                                                                   EL326
01162  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL326
01163                                                                   EL326
01164  WRITE-A-LINE SECTION.                                            EL326
01165                                                                   EL326
01166  WAL-010.                                                         EL326
01167      IF WS-LINE-COUNT GREATER WS-LINE-COUNT-MAX                   EL326
01168          PERFORM WRITE-HEADINGS.                                  EL326
01169                                                                   EL326
01170      PERFORM WRITE-PRINTER.                                       EL326
01171                                                                   EL326
01172  WAL-EXIT.                                                        EL326
01173      EXIT.                                                        EL326
01174  WRITE-HEADINGS SECTION.                                          EL326
01175  WHS-010.                                                         EL326
01176      IF WS-LINE-COUNT = +99                                       EL326
01177          NEXT SENTENCE                                            EL326
01178      ELSE                                                         EL326
01179          MOVE CF-CL-MAIL-TO-NAME TO  WS-H2-CLIENT-NAME            EL326
01180          MOVE CF-COMPANY-ID      TO  WS-H2-CO-ID.                 EL326
01181                                                                   EL326
01182      ADD +1  TO  WS-PAGE.                                         EL326
01183      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL326
01184      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL326
01185      MOVE ZERO                   TO  WS-LINE-COUNT.               EL326
01186                                                                   EL326
01187      MOVE WS-HEADING1            TO  PRT.                         EL326
01188      MOVE '1'                    TO  X.                           EL326
01189      PERFORM WRITE-PRINTER.                                       EL326
01190                                                                   EL326
01191      MOVE WS-HEADING2            TO  PRT.                         EL326
01192      MOVE ' '                    TO  X.                           EL326
01193      PERFORM WRITE-PRINTER.                                       EL326
01194                                                                   EL326
01195      MOVE WS-HEADING3            TO  PRT.                         EL326
01196      MOVE ' '                    TO  X.                           EL326
01197      PERFORM WRITE-PRINTER.                                       EL326
01198                                                                   EL326
01199      MOVE WS-HEADING4            TO  PRT.                         EL326
01200      MOVE ' '                    TO  X.                           EL326
01201      PERFORM WRITE-PRINTER.                                       EL326
01202                                                                   EL326
01203      MOVE +7                     TO  WS-LINE-COUNT.               EL326
01204                                                                   EL326
01205  WHS-020.                                                         EL326
01206      MOVE WS-SAVE-PRINT-RECORD   TO  PRT.                         EL326
01207      MOVE 1                      TO  P-CTL.                       EL326
01208                                                                   EL326
01209  WHS-EXIT.                                                        EL326
01210      EXIT.                                                        EL326
01211                                                                   EL326
01212  WRITE-PRINTER      SECTION.                                      EL326
01213                                                                   EL326
01214      IF P-CTL = ' '                                               EL326
01215          WRITE PRT AFTER ADVANCING 1 LINE                         EL326
01216          ADD +1 TO WS-LINE-COUNT                                  EL326
01217      ELSE                                                         EL326
01218          IF P-CTL = '0'                                           EL326
01219              WRITE PRT AFTER ADVANCING 2 LINES                    EL326
01220              ADD +2 TO WS-LINE-COUNT                              EL326
01221          ELSE                                                     EL326
01222              IF P-CTL = '-'                                       EL326
01223                  WRITE PRT AFTER ADVANCING 3 LINES                EL326
01224                  ADD +3 TO WS-LINE-COUNT                          EL326
01225              ELSE                                                 EL326
01226                  WRITE PRT AFTER ADVANCING PAGE                   EL326
01227                  MOVE ZEROS      TO  WS-LINE-COUNT.               EL326
01228                                                                   EL326
01229  7600-EXIT.                                                       EL326
01230       EXIT.                                                       EL326
01231  EJECT                                                            EL326
01232                                                                   EL326
01233  OPEN-FILES SECTION.                                              EL326
01234                                                                   EL326
01235      OPEN INPUT HISTORY-INPUT-FILE                                EL326
01236           I-O   ELCERT                                            EL326
01237                 ELMSTR                                            EL326
01238                 ELTRLR                                            EL326
01239                 ELARCH                                            EL326
01240                 ELRETR                                            EL326
01241                 MPPLCY.                                           EL326
01242                                                                   EL326
01243      IF ELARCH-FILE-STATUS  EQUAL '00' OR '97'                    EL326
01244          NEXT SENTENCE                                            EL326
01245          ELSE                                                     EL326
01246          MOVE 'ERROR OCCURED OPEN - ELARCH'  TO  WS-ABEND-MESSAGE EL326
01247          MOVE ELARCH-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL326
01248          GO TO ABEND-PGM.                                         EL326
01249                                                                   EL326
01250      IF ELCERT-FILE-STATUS EQUAL '00' OR '97'                     EL326
01251          NEXT SENTENCE                                            EL326
01252          ELSE                                                     EL326
01253          MOVE 'ERROR OCCURED OPEN - ELCERT'  TO  WS-ABEND-MESSAGE EL326
01254          MOVE ELCERT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL326
01255          GO TO ABEND-PGM.                                         EL326
01256                                                                   EL326
01257      IF ELMSTR-FILE-STATUS  EQUAL '00' OR '97'                    EL326
01258          NEXT SENTENCE                                            EL326
01259          ELSE                                                     EL326
01260          MOVE 'ERROR OCCURED OPEN - ELMSTR'  TO  WS-ABEND-MESSAGE EL326
01261          MOVE ELMSTR-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL326
01262          GO TO ABEND-PGM.                                         EL326
01263                                                                   EL326
01264      IF ELTRLR-FILE-STATUS EQUAL '00' OR '97'                     EL326
01265          NEXT SENTENCE                                            EL326
01266          ELSE                                                     EL326
01267          MOVE 'ERROR OCCURED OPEN - ELTRLR'  TO  WS-ABEND-MESSAGE EL326
01268          MOVE ELTRLR-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL326
01269          GO TO ABEND-PGM.                                         EL326
01270                                                                   EL326
111102     IF MPPLCY-FILE-STATUS EQUAL '00' OR '97' OR '9%' OR '9+'     EL326
01272          NEXT SENTENCE                                            EL326
01273      ELSE                                                         EL326
01274          MOVE 'ERROR OCCURED OPEN - MPPLCY'                       EL326
01275                                   TO  WS-ABEND-MESSAGE            EL326
01276          MOVE MPPLCY-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL326
01277          GO TO ABEND-PGM.                                         EL326
01278                                                                   EL326
01279      IF ELRETR-FILE-STATUS EQUAL '00' OR '97'                     EL326
01280         NEXT SENTENCE                                             EL326
01281      ELSE                                                         EL326
01282         MOVE 'ERROR OCCURED OPEN - ELRETR'                        EL326
01283                                   TO  WS-ABEND-MESSAGE            EL326
01284         MOVE ELRETR-FILE-STATUS  TO  WS-ABEND-FILE-STATUS         EL326
01285         GO TO ABEND-PGM.                                          EL326
01286                                                                   EL326
01287      ACCEPT WS-ACCEPT-DATE FROM  DATE.                            EL326
01288      MOVE WS-ACCEPT-DATE         TO  DC-GREG-DATE-1-YMD.          EL326
01289      MOVE '3'                    TO  DC-OPTION-CODE.              EL326
01290      PERFORM 8500-DATE-CONVERSION.                                EL326
01291      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.           EL326
01292      MOVE DC-GREG-DATE-1-EDIT    TO  WS-H2-DATE.                  EL326
01293      MOVE DC-GREG-DATE-1-ALPHA   TO  WS-H3-DATE.                  EL326
01294                                                                   EL326
01295  OFS-EXIT.                                                        EL326
01296      EXIT.                                                        EL326
01297                                                                   EL326
01298      EJECT                                                        EL326
01299  CLOSE-FILES SECTION.                                             EL326
01300                                                                   EL326
01301      CLOSE HISTORY-INPUT-FILE                                     EL326
01302            ELCERT                                                 EL326
01303            ELMSTR                                                 EL326
01304            ELTRLR                                                 EL326
01305            ELARCH                                                 EL326
01306            MPPLCY                                                 EL326
01307            ELRETR                                                 EL326
01308            PRNTR.                                                 EL326
01309                                                                   EL326
01310      IF ELARCH-FILE-STATUS NOT EQUAL '00'                         EL326
01311          MOVE 'ERROR OCCURED CLOSE - ELARCH'  TO  WS-ABEND-MESSAGEEL326
01312          MOVE ELARCH-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL326
01313          GO TO ABEND-PGM.                                         EL326
01314                                                                   EL326
01315      IF ELCERT-FILE-STATUS NOT EQUAL '00'                         EL326
01316          MOVE 'ERROR OCCURED CLOSE - ELCERT'  TO  WS-ABEND-MESSAGEEL326
01317          MOVE ELCERT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL326
01318          GO TO ABEND-PGM.                                         EL326
01319                                                                   EL326
01320      IF ELMSTR-FILE-STATUS NOT EQUAL '00'                         EL326
01321          MOVE 'ERROR OCCURED CLOSE - ELMSTR'  TO  WS-ABEND-MESSAGEEL326
01322          MOVE ELMSTR-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL326
01323          GO TO ABEND-PGM.                                         EL326
01324                                                                   EL326
01325      IF ELTRLR-FILE-STATUS NOT EQUAL '00'                         EL326
01326          MOVE 'ERROR OCCURED CLOSE - ELTRLR'  TO  WS-ABEND-MESSAGEEL326
01327          MOVE ELTRLR-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL326
01328          GO TO ABEND-PGM.                                         EL326
01329                                                                   EL326
111102*    IF MPPLCY-FILE-STATUS NOT EQUAL '00'                         EL326
111102*        MOVE 'ERROR OCCURED CLOSE - MPPLCY'                      EL326
111102*                                    TO  WS-ABEND-MESSAGE         EL326
111102*        MOVE MPPLCY-FILE-STATUS     TO  WS-ABEND-FILE-STATUS     EL326
111102*        GO TO ABEND-PGM.                                         EL326
01335                                                                   EL326
01336      IF ELRETR-FILE-STATUS NOT EQUAL '00'                         EL326
01337          MOVE 'ERROR OCCURED CLOSE - ELRETR'                      EL326
01338                                      TO  WS-ABEND-MESSAGE         EL326
01339          MOVE ELRETR-FILE-STATUS     TO  WS-ABEND-FILE-STATUS     EL326
01340          GO TO ABEND-PGM.                                         EL326
01341                                                                   EL326
01342  CFS-EXIT.                                                        EL326
01343      EXIT.                                                        EL326
01344                                                                   EL326
01345  ABEND-PGM SECTION. COPY ELCABEND.                                EL326
01346                                                                   EL326
