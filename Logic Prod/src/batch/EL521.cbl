00001  IDENTIFICATION DIVISION.                                         08/19/98
00002                                                                   EL521
00003  PROGRAM-ID.                 EL521 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL521
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL521
00006 *              CONVERSION DATE 02/28/96 15:46:24.                 EL521
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL521
00008 *                            VMOD=2.008.                          EL521
00009                                                                   EL521
00010 *AUTHOR.     LOGIC, INC.                                          EL521
00011 *            DALLAS, TEXAS.                                       EL521
00012                                                                   EL521
00013 *DATE-COMPILED.                                                   EL521
00014                                                                   EL521
00015 *SECURITY.   *****************************************************EL521
00016 *            *                                                   *EL521
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL521
00018 *            *                                                   *EL521
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL521
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL521
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL521
00022 *            *                                                   *EL521
00023 *            *****************************************************EL521
00024                                                                   EL521
00025 *REMARKS.                                                         EL521
00026 *        THIS PROGRAM IS RUN AT MONTH END TO PULL THE CLAS-IC     EL521
00027 *    CREDIT FILES OFF TO AN EXTRACT INTERFACE TAPE FOR INPUT      EL521
00028 *    INTO THE MONTHLY CYCLE.                                      EL521
00029                                                                   EL521
00030 *    INPUT FILES  - ELCNTL - CONTROL FILE                         EL521
00031 *                   ERPNDB - PENDING BUSINESS FILE                EL521
00032 *                   ERPNDC - PENDING CLAIMS FILE                  EL521
00033 *                   ERCRTC - PENDING CERT CHANGES FILE            EL521
00034 *                   ERPYAJ - PAYMENTS AND ADJUSTMENTS FILE        EL521
00035 *                   ERREPY - PENDING RETRO/EPEC PMTS & ADJUSTMENTSEL521
00036 *                   ERRQST - REQUEST FILE                         EL521
00037                                                                   EL521
00038 *    OUTPUT FILES - EREXTR - CREDIT EXTRACT INTERFACE             EL521
00039                                                                   EL521
00040 *        EACH COMPANY'S OPTIONS FOR PROGRAM EL521 ARE READ DURING EL521
00041 *    THE EXECUTION OF THIS PROGRAM.  ONLY OPTIONS WITH A 'NONE'   EL521
00042 *    FREQUENCY WILL BE RECOGNIZED.                                EL521
00043                                                                   EL521
00044 *        THE INTERFACE EXTRACT, EREXTR, IS WRITTEN FOR ALL        EL521
00045 *    COMPANIES HAVING AN ONLINE CONTROL FILE.  AN INTERNAL        EL521
00046 *    SORT IS USED TO SEQUENCE THIS FILE AFTER CREATION.  THE      EL521
00047 *    SELECTION CRITERIA USED FOR EACH OF THE EXTRACT TYPE IS      EL521
00048 *    AS FOLLOWS -                                                 EL521
00049                                                                   EL521
00050 *    - EXTRACT - A     RECORD - A     PENDING BUSINESS            EL521
00051 *          EXTRACT FOR ALL ISSUES AND CANCELS                     EL521
00052                                                                   EL521
00053 *    - EXTRACT - A     RECORD - B     PENDING CLAIMS              EL521
00054 *          ALL CLAIMS RECORDS FOR CLIENTS NOT USING THE           EL521
00055 *          ONLINE CLAS-IC CLAIMS SYSTEM.                          EL521
00056                                                                   EL521
00057 *    - EXTRACT - A     RECORD - C     CERT CHANGES                EL521
00058 *          ALL PENDING CHANGES THE CERTIFICATE FILE.              EL521
00059                                                                   EL521
00060 *    - EXTRACT - A     RECORD - D     PAYMENTS AND ADJUSTMENTS    EL521
00061 *          ALL PAYMENTS AND ADJUSTMENTS.                          EL521
00062                                                                   EL521
00063 *    - EXTRACT - A     RECORD - E     ERREPY PMTS & ADJS          EL521
00064 *          ALL PENDING RETRO/EPEC PAYMENTS AND ADJUSTMENTS        EL521
00065                                                                   EL521
00066 *    - EXTRACT - A     RECORD - F     ERRQST REQUEST FILE         EL521
00067 *          ALL PENDING A/R REQUEST RECORDS.                       EL521
00068                                                                   EL521
031102******************************************************************
031102*                   C H A N G E   L O G
031102*
031102* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031102*-----------------------------------------------------------------
031102*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031102* EFFECTIVE    NUMBER
031102*-----------------------------------------------------------------
061402* 061802                   PEMA  ADD PROCESS FOR PEOPLES
080702* 080702    2002061800008  PEMA  ADD PROCESS FOR SUNFLOWER
061206* 061206  CR2006050500001  PEMA  ADD FIRST PREMIER BANK
022616* 022616  CR2016021100002  PEMA  MID MONTH BILLING FOR MACHENS
031102******************************************************************
00069      EJECT                                                        EL521
00070  ENVIRONMENT DIVISION.                                            EL521
00071  CONFIGURATION SECTION.                                           EL521
00072  SPECIAL-NAMES.                                                   EL521
00073      C02 IS LCP-CH2                                               EL521
00074      C03 IS LCP-CH3                                               EL521
00075      C04 IS LCP-CH4                                               EL521
00076      C05 IS LCP-CH5                                               EL521
00077      C06 IS LCP-CH6                                               EL521
00078      C07 IS LCP-CH7                                               EL521
00079      C08 IS LCP-CH8                                               EL521
00080      C09 IS LCP-CH9                                               EL521
00081      C10 IS LCP-CH10                                              EL521
00082      C11 IS LCP-CH11                                              EL521
00083      C12 IS LCP-CH12                                              EL521
00084      S01 IS LCP-P01                                               EL521
00085      S02 IS LCP-P02.                                              EL521
00086                                                                   EL521
00087  INPUT-OUTPUT SECTION.                                            EL521
00088                                                                   EL521
00089  FILE-CONTROL.                                                    EL521
00090                                                                   EL521
00091      SELECT EXTRACT-INTERFACE-FILE                                EL521
00092                              ASSIGN TO SYS010-UT-2400-S-SYS010.   EL521
00093                                                                   EL521
00094      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL521
00095                                                                   EL521
00096      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL521
00097                                                                   EL521
00098      SELECT ELCNTL           ASSIGN TO SYS021-FBA1-ELCNTL         EL521
00099                              ORGANIZATION IS INDEXED              EL521
00100                              ACCESS IS DYNAMIC                    EL521
00101                              RECORD KEY IS CF-CONTROL-PRIMARY     EL521
00102                              FILE STATUS IS ELCNTL-FILE-STATUS.   EL521
00103                                                                   EL521
00104      SELECT ERPNDB           ASSIGN TO SYS022-FBA1-ERPNDB         EL521
00105                              ORGANIZATION IS INDEXED              EL521
00106                              ACCESS IS DYNAMIC                    EL521
00107                              RECORD KEY IS PB-CONTROL-PRIMARY     EL521
00108                              FILE STATUS IS ERPNDB-FILE-STATUS.   EL521
00109                                                                   EL521
00110      SELECT ERPNDC           ASSIGN TO SYS023-FBA1-ERPNDC         EL521
00111                              ORGANIZATION IS INDEXED              EL521
00112                              ACCESS IS DYNAMIC                    EL521
00113                              RECORD KEY IS PC-CONTROL-PRIMARY     EL521
00114                              FILE STATUS IS ERPNDC-FILE-STATUS.   EL521
00115                                                                   EL521
00116      SELECT ERCRTC           ASSIGN TO SYS024-FBA1-ERCRTC         EL521
00117                              ORGANIZATION IS INDEXED              EL521
00118                              ACCESS IS DYNAMIC                    EL521
00119                              RECORD KEY IS CC-CONTROL-PRIMARY     EL521
00120                              FILE STATUS IS ERCRTC-FILE-STATUS.   EL521
00121                                                                   EL521
00122      SELECT ERPYAJ           ASSIGN TO SYS025-FBA1-ERPYAJ         EL521
00123                              ORGANIZATION IS INDEXED              EL521
00124                              ACCESS IS DYNAMIC                    EL521
00125                              RECORD KEY IS PY-CONTROL-PRIMARY     EL521
00126                              FILE STATUS IS ERPYAJ-FILE-STATUS.   EL521
00127                                                                   EL521
00128      SELECT ERREPY           ASSIGN TO SYS027-FBA1-ERREPY         EL521
00129                              ORGANIZATION IS INDEXED              EL521
00130                              ACCESS IS DYNAMIC                    EL521
00131                              RECORD KEY IS RP-CONTROL-PRIMARY     EL521
00132                              FILE STATUS IS ERREPY-FILE-STATUS.   EL521
00133                                                                   EL521
00134      SELECT ERRQST           ASSIGN TO ERRQST                     EL521
00135                              ORGANIZATION IS INDEXED              EL521
00136                              ACCESS IS DYNAMIC                    EL521
00137                              RECORD KEY IS RQ-CONTROL-PRIMARY     EL521
00138                              FILE STATUS IS ERRQST-FILE-STATUS.   EL521
00139                                                                   EL521
00140      EJECT                                                        EL521
00141  DATA DIVISION.                                                   EL521
00142                                                                   EL521
00143  FILE SECTION.                                                    EL521
00144                                                                   EL521
00145  FD  ELCNTL.                                                      EL521
00146                                                                   EL521
00147                                      COPY ELCCNTL.                EL521
00148                                                                   EL521
00149      EJECT                                                        EL521
00150  FD  ERPNDB.                                                      EL521
00151                                                                   EL521
00152                                      COPY ERCPNDB.                EL521
00153                                                                   EL521
00154      EJECT                                                        EL521
00155  FD  ERPNDC.                                                      EL521
00156                                                                   EL521
00157                                      COPY ERCPNDC.                EL521
00158                                                                   EL521
00159      EJECT                                                        EL521
00160  FD  ERCRTC.                                                      EL521
00161                                                                   EL521
00162                                      COPY ERCCRTC.                EL521
00163                                                                   EL521
00164      EJECT                                                        EL521
00165  FD  ERPYAJ.                                                      EL521
00166                                                                   EL521
00167                                      COPY ERCPYAJ.                EL521
00168                                                                   EL521
00169      EJECT                                                        EL521
00170  FD  ERREPY.                                                      EL521
00171                                                                   EL521
00172                                      COPY ERCREPY.                EL521
00173                                                                   EL521
00174      EJECT                                                        EL521
00175  FD  ERRQST.                                                      EL521
00176                                                                   EL521
00177                                      COPY ERCRQST.                EL521
00178                                                                   EL521
00179      EJECT                                                        EL521
00180  FD  EXTRACT-INTERFACE-FILE          COPY ERCEXTFD.               EL521
00181                                                                   EL521
00182                                      COPY ERCEXTR.                EL521
00183                                                                   EL521
00184      EJECT                                                        EL521
00185  FD  PRNTR                           COPY ELCPRTFD.               EL521
00186                                                                   EL521
00187  FD  DISK-DATE                       COPY ELCDTEFD.               EL521
00188                                                                   EL521
00189      EJECT                                                        EL521
00190                                                                   EL521
00191  WORKING-STORAGE SECTION.                                         EL521
00192  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL521
00193  77  LCP-ASA                       PIC X.                         EL521
00194                                                                   EL521
00195  77  FILLER  PIC X(32)   VALUE '********************************'.EL521
00196  77  FILLER  PIC X(32)   VALUE '*     EL521  WORKING STORAGE   *'.EL521
00197  77  FILLER  PIC X(32)   VALUE '*********** VM 2.008 ***********'.EL521
00198                                                                   EL521
00199  01  FILLER                      COMP-3.                          EL521
00200      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL521
00201      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +60.   EL521
00202      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL521
00203      05  WS-REPORT-SW                PIC S9          VALUE +1.    EL521
00204      05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  EL521
00205      05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  EL521
00206      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL521
00207      05  WS-CURRENT-TIME             PIC S9(7)       VALUE ZERO.  EL521
00208                                                                   EL521
00209  01  WS-FIX-DATE-01.                                              EL521
00210      05  WS-FIX-DATE-05              PIC S9(7) COMP  VALUE +32542.EL521
00211      05  WS-FIX-DATE-REDEF   REDEFINES WS-FIX-DATE-05.            EL521
00212          10  FILLER                  PIC XX.                      EL521
00213          10  WS-FIX-DATE             PIC XX.                      EL521
00214                                                                   EL521
00215  01  WS-TOTAL-TABLE.                                              EL521
00216      05  WS-TOTAL-1.                                              EL521
00217          10  WS-EXTRACT-AA-DESC      PIC X(40)       VALUE        EL521
00218              ' PENDING BUSINESS EXTRACTS  - '.                    EL521
00219          10  WS-EXTRACT-AA-COUNT     PIC S9(7)       VALUE ZERO.  EL521
00220                                                                   EL521
00221      05  WS-TOTAL-1A.                                             EL521
00222          10  WS-EXT-ISSUE-DESC       PIC X(40)       VALUE        EL521
00223              ' PENDING BUSINESS ISSUES    - '.                    EL521
00224          10  WS-EXT-ISSUE-COUNT      PIC S9(7)       VALUE ZERO.  EL521
00225                                                                   EL521
00226      05  WS-TOTAL-1B.                                             EL521
00227          10  WS-EXT-CANCEL-DESC      PIC X(40)       VALUE        EL521
00228              ' PENDING BUSINESS CANCELS   - '.                    EL521
00229          10  WS-EXT-CANCEL-COUNT     PIC S9(7)       VALUE ZERO.  EL521
00230                                                                   EL521
00231      05  WS-TOTAL-1C.                                             EL521
00232          10  WS-EXT-BATCH-DESC       PIC X(40)       VALUE        EL521
00233              ' PENDING BUS. BATCH HDRS    - '.                    EL521
00234          10  WS-EXT-BATCH-COUNT      PIC S9(7)       VALUE ZERO.  EL521
00235                                                                   EL521
00236      05  WS-TOTAL-2.                                              EL521
00237          10  WS-EXTRACT-AB-DESC      PIC X(40)       VALUE        EL521
00238              ' PENDING CLAIMS EXTRACTS    - '.                    EL521
00239          10  WS-EXTRACT-AB-COUNT     PIC S9(7)       VALUE ZERO.  EL521
00240                                                                   EL521
00241      05  WS-TOTAL-2A.                                             EL521
00242          10  WS-EXT-CLAIMS-DESC      PIC X(40)       VALUE        EL521
00243              ' PENDING CLAIMS PAYMENTS    - '.                    EL521
00244          10  WS-EXT-CLAIMS-COUNT     PIC S9(7)       VALUE ZERO.  EL521
00245                                                                   EL521
00246      05  WS-TOTAL-2B.                                             EL521
00247          10  WS-EXT-RESERV-DESC      PIC X(40)       VALUE        EL521
00248              ' PENDING CLAIMS RESERVES    - '.                    EL521
00249          10  WS-EXT-RESERV-COUNT     PIC S9(7)       VALUE ZERO.  EL521
00250                                                                   EL521
00251      05  WS-TOTAL-3.                                              EL521
00252          10  WS-EXTRACT-AC-DESC      PIC X(40)       VALUE        EL521
00253              ' CERT CHANGE EXTRACTS       - '.                    EL521
00254          10  WS-EXTRACT-AC-COUNT     PIC S9(7)       VALUE ZERO.  EL521
00255                                                                   EL521
00256      05  WS-TOTAL-4.                                              EL521
00257          10  WS-EXTRACT-AD-DESC      PIC X(40)       VALUE        EL521
00258              ' PAYMENT/ADJ EXTRACTS       - '.                    EL521
00259          10  WS-EXTRACT-AD-COUNT     PIC S9(7)       VALUE ZERO.  EL521
00260                                                                   EL521
00261      05  WS-TOTAL-5.                                              EL521
00262          10  WS-EXTRACT-AE-DESC      PIC X(40)       VALUE        EL521
00263              ' PENDING RETRO REIN EXTRACTS- '.                    EL521
00264          10  WS-EXTRACT-AE-COUNT     PIC S9(7)       VALUE ZERO.  EL521
00265                                                                   EL521
00266      05  WS-TOTAL-6.                                              EL521
00267          10  WS-EXTRACT-AF-DESC      PIC X(40)       VALUE        EL521
00268              ' REQUEST FILE EXTRACTS      - '.                    EL521
00269          10  WS-EXTRACT-AF-COUNT     PIC S9(7)       VALUE ZERO.  EL521
00270                                                                   EL521
00271      05  WS-TOTAL-7.                                              EL521
00272          10  WS-EXTRACT-TOT-DESC     PIC X(40)       VALUE        EL521
00273              ' *** TOTAL EXTRACTS ***     - '.                    EL521
00274          10  WS-EXTRACT-TOT-COUNT    PIC S9(7)       VALUE ZERO.  EL521
00275                                                                   EL521
00276  01  WS-TOT-TABLE REDEFINES WS-TOTAL-TABLE.                       EL521
00277      05  WS-TOTALS OCCURS 12 TIMES                                EL521
00278                    INDEXED BY TOT-INDX.                           EL521
00279          10  WS-TOT-DESC             PIC X(40).                   EL521
00280          10  WS-TOT-COUNT            PIC S9(7).                   EL521
00281                                                                   EL521
00282  01  FILLER                          COMP SYNC.                   EL521
00283      05  PGM-SUB                     PIC S9(4)       VALUE +521.  EL521
00284      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  EL521
00285      05  WS-LENGTH                   REDEFINES                    EL521
00286          WS-INDEX                    PIC S9(4).                   EL521
00287                                                                   EL521
00288  01  FILLER.                                                      EL521
00289      05  WS-DISPLAY-TIME             PIC 99B99B99.                EL521
00290      05  WS-DEL-BIN1                 PIC 9999-.                   EL521
00291                                                                   EL521
00292      05  X                           PIC X.                       EL521
00293      05  ABEND-CODE                  PIC X(4).                    EL521
00294      05  ABEND-OPTION                PIC X.                       EL521
00295      05  OLC-REPORT-NAME             PIC X(8) VALUE 'EL521'.      EL521
00296                                                                   EL521
00297      05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.EL521
00298                                                                   EL521
00299      05  WS-LAST-COMPANY-CD          PIC X VALUE LOW-VALUES.      EL521
00300      05  WS-LAST-CARRIER             PIC X VALUE LOW-VALUES.      EL521
00301                                                                   EL521
00302      05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL521
00303                                                                   EL521
00304      05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL521
00305      05  ELCNTL-FILE-STATUS          PIC XX          VALUE ZERO.  EL521
00306      05  ERPNDB-FILE-STATUS          PIC XX          VALUE ZERO.  EL521
00307      05  ERPNDC-FILE-STATUS          PIC XX          VALUE ZERO.  EL521
00308      05  ERCRTC-FILE-STATUS          PIC XX          VALUE ZERO.  EL521
00309      05  ERPYAJ-FILE-STATUS          PIC XX          VALUE ZERO.  EL521
00310      05  ERREPY-FILE-STATUS          PIC XX          VALUE ZERO.  EL521
00311      05  ERRQST-FILE-STATUS          PIC XX          VALUE ZERO.  EL521
00312                                                                   EL521
00313      05  WS-FILE-ERROR-MESSAGE.                                   EL521
00314          10  FILLER                  PIC X(24)       VALUE        EL521
00315              'ERROR OCCURED OPENING - '.                          EL521
00316          10  WS-FEM-FILE-NAME        PIC X(8).                    EL521
00317                                                                   EL521
00318      05  WS-THIS-DATE                PIC XX      VALUE LOW-VALUES.EL521
00319      05  WS-RUN-DATE-BIN             PIC XX      VALUE LOW-VALUES.EL521
00320      05  WS-SAVE-LAST-DT             PIC XX      VALUE LOW-VALUES.EL521
00321      05  WS-PULL-DATE                PIC XX      VALUE LOW-VALUES.EL521
00322      05  WS-MONTH-END-DATE           PIC XX      VALUE LOW-VALUES.EL521
00323                                                                   EL521
00324      05  WS-COMPANY-ID               PIC X(3).                    EL521
00325      05  WS-COMPANY-CD               PIC X.                       EL521
00326                                                                   EL521
00327      05  WS-COMPANY-NAME.                                         EL521
00328          10  WS-CN-CHAR              PIC X                        EL521
00329              OCCURS 30 TIMES         INDEXED BY CN1.              EL521
00330                                                                   EL521
00331      05  WS-COMPANY-NAME2.                                        EL521
00332          10  WS-CN2-CHAR             PIC X                        EL521
00333              OCCURS 30 TIMES         INDEXED BY CN2.              EL521
00334                                                                   EL521
00335      05  WS-BIN-DATE-WORK-X.                                      EL521
00336          10  WS-BIN-DATE-WORK        PIC S9(4)                    EL521
00337                                      COMP.                        EL521
00338                                                                   EL521
00339      EJECT                                                        EL521
00340  01  WS-HEADING1.                                                 EL521
00341      05  FILLER                      PIC X(52)       VALUE '1'.   EL521
00342      05  WS-H1-TITLE                 PIC X(72)       VALUE        EL521
00343          'EXTRACT INTERFACE FILE TOTALS'.                         EL521
00344      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE '  EL521'.    EL521
00345                                                                   EL521
00346  01  WS-HEADING2.                                                 EL521
00347      05  FILLER                      PIC X           VALUE SPACES.EL521
00348      05  WS-H2-COMPANY-ID            PIC XXX         VALUE 'XXX'. EL521
00349      05  FILLER                      PIC X(47)       VALUE SPACES.EL521
00350      05  WS-H2-CLIENT-NAME           PIC X(30)       VALUE SPACES.EL521
00351      05  FILLER                      PIC X(44)       VALUE SPACES.EL521
00352      05  WS-H2-DATE                  PIC X(08)       VALUE SPACES.EL521
00353                                                                   EL521
00354  01  WS-HEADING3.                                                 EL521
00355      05  FILLER                      PIC X(58)       VALUE SPACES.EL521
00356      05  WS-H3-DATE                  PIC X(53)       VALUE SPACES.EL521
00357      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL521
00358      05  WS-H3-PAGE                  PIC ZZ,ZZ9      VALUE ZERO.  EL521
00359      05  FILLER                      PIC X(11)       VALUE SPACES.EL521
00360                                                                   EL521
00361  01  WS-HEADING4.                                                 EL521
00362      05  FILLER                      PIC X(80)      VALUE         EL521
00363          '0           FILE                      COUNTS '.         EL521
00364                                                                   EL521
00365  01  WS-DETAIL1.                                                  EL521
00366      05  FILLER                      PIC X.                       EL521
00367      05  WS-D2-DESC                  PIC X(30).                   EL521
00368      05  WS-D2-COUNT                 PIC Z,ZZZ,ZZZ,ZZ9-.          EL521
00369      05  FILLER                      PIC X(35).                   EL521
00370                                                                   EL521
00371      EJECT                                                        EL521
00372                                      COPY ELCDATE.                EL521
00373                                                                   EL521
00374                                      COPY ELCDTECX.               EL521
00375                                                                   EL521
00376                                      COPY ELCDTEVR.               EL521
00377                                                                   EL521
00378      EJECT                                                        EL521
00379  PROCEDURE DIVISION.                                              EL521
00380                                                                   EL521
00381  0000-LOAD-DATE-CARD.                COPY ELCDTERX SUPPRESS.      EL521
00382                                                                   EL521
00383  0000-MAIN-LOGIC SECTION.                                         EL521
00384                                                                   EL521
00385      PERFORM OPEN-FILES.                                          EL521
00386                                                                   EL521
00387      PERFORM 1000-PROCESS-EXTRACT.                                EL521
00388                                                                   EL521
00389      CLOSE PRNTR  EXTRACT-INTERFACE-FILE.                         EL521
00390                                                                   EL521
00391      MOVE ZEROS  TO RETURN-CODE.
00391      GOBACK.                                                      EL521
00392                                                                   EL521
00393      EJECT                                                        EL521
00394  1000-PROCESS-EXTRACT SECTION.                                    EL521
00395      MOVE BIN-RUN-DATE           TO  WS-PULL-DATE.                EL521
00396                                                                   EL521
00397      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          EL521
00398      MOVE DTE-CLIENT             TO  CF-COMPANY-ID.               EL521
00399      MOVE '1'                    TO  CF-RECORD-TYPE.              EL521
00400      MOVE SPACES                 TO  CF-ACCESS-CD-GENL.           EL521
00401      MOVE +0                     TO  CF-SEQUENCE-NO.              EL521
00402                                                                   EL521
00403      READ ELCNTL.                                                 EL521
00404                                                                   EL521
00405      IF ELCNTL-FILE-STATUS NOT = '00'                             EL521
00406          MOVE 'ERROR OCCURED READ INITIAL - ELCNTL'               EL521
00407                                  TO  WS-ABEND-MESSAGE             EL521
00408          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00409          GO TO ABEND-PGM.                                         EL521
00410                                                                   EL521
00411      MOVE CF-CL-MAIL-TO-NAME     TO  WS-COMPANY-NAME              EL521
00412                                      WS-H2-CLIENT-NAME.           EL521
00413      MOVE CF-COMPANY-ID          TO  WS-COMPANY-ID                EL521
00414                                      WS-H2-COMPANY-ID.            EL521
00415      MOVE CF-COMPANY-CD          TO  WS-COMPANY-CD.               EL521
00416      MOVE LOW-VALUES             TO  WS-LAST-CARRIER.             EL521
00417                                                                   EL521
00418      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL521
00419                                                                   EL521
00420      MOVE WS-TIME                TO  WS-DISPLAY-TIME.             EL521
00421      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'.            EL521
00422      DISPLAY 'BEGIN PROCESSING ' WS-H2-CLIENT-NAME  ' AT '        EL521
00423              WS-DISPLAY-TIME UPON CONSOLE.                        EL521
00424                                                                   EL521
00425      MOVE SPACES                 TO  WS-COMPANY-NAME2.            EL521
00426      SET CN1 TO +30.                                              EL521
00427                                                                   EL521
00428  1020-SIP.                                                        EL521
00429      IF WS-CN-CHAR (CN1) = SPACES                                 EL521
00430          IF CN1 GREATER THAN +1                                   EL521
00431              SET CN1 DOWN BY +1                                   EL521
00432              GO TO 1020-SIP                                       EL521
00433            ELSE                                                   EL521
00434              GO TO 1100-SIP.                                      EL521
00435                                                                   EL521
00436      SET WS-LENGTH TO CN1.                                        EL521
00437                                                                   EL521
00438      SUBTRACT WS-LENGTH FROM +30 GIVING WS-LENGTH.                EL521
00439      DIVIDE +2 INTO WS-LENGTH ROUNDED.                            EL521
00440                                                                   EL521
00441      IF WS-LENGTH NOT GREATER THAN ZERO                           EL521
00442          GO TO 1100-SIP.                                          EL521
00443                                                                   EL521
00444      SET CN2 TO CN1.                                              EL521
00445      SET CN2 UP BY WS-LENGTH.                                     EL521
00446                                                                   EL521
00447  1030-SIP.                                                        EL521
00448      MOVE WS-CN-CHAR (CN1) TO WS-CN2-CHAR (CN2).                  EL521
00449                                                                   EL521
00450      IF CN1 GREATER THAN +1                                       EL521
00451          SET CN1                                                  EL521
00452              CN2 DOWN BY +1                                       EL521
00453          GO TO 1030-SIP.                                          EL521
00454                                                                   EL521
00455      MOVE WS-COMPANY-NAME2       TO  WS-COMPANY-NAME.             EL521
00456                                                                   EL521
00457      EJECT                                                        EL521
00458                                                                   EL521
00459  1100-SIP.                                                        EL521
00460 *    NOTE ******************************************************* EL521
00461 *         *      POSITION THE PENDING BUSINESS FILE AT THE BE-  * EL521
00462 *         *  GINNING OF THE COMPANY TO LOAD AND RELEASE ALL     * EL521
00463 *         *  ISSUES, CANCELS, AND BATCH RECORDS TO THE EXTRACT  * EL521
00464 *         *  SORT.                                              * EL521
00465 *         *******************************************************.EL521
00466                                                                   EL521
00467      OPEN INPUT ERPNDB.                                           EL521
00468                                                                   EL521
00469      IF ERPNDB-FILE-STATUS  = '00' OR '97'                        EL521
00470          NEXT SENTENCE                                            EL521
00471        ELSE                                                       EL521
00472          MOVE 'ERPNDB  '         TO  WS-FEM-FILE-NAME             EL521
00473          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
00474          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00475          GO TO ABEND-PGM.                                         EL521
00476                                                                   EL521
00477      MOVE LOW-VALUES             TO  PB-CONTROL-PRIMARY.          EL521
00478      MOVE WS-COMPANY-CD          TO  PB-COMPANY-CD.               EL521
00479                                                                   EL521
00480      START ERPNDB                                                 EL521
00481          KEY IS GREATER THAN PB-CONTROL-PRIMARY.                  EL521
00482                                                                   EL521
pemuni     IF ERPNDB-FILE-STATUS = '23' or '10'                         EL521
00484          DISPLAY 'EL521 NO RECORDS FOUND - ERPNDB  FOR CO - '     EL521
00485              WS-COMPANY-ID UPON CONSOLE                           EL521
00486          GO TO 1190-SIP.                                          EL521
00487                                                                   EL521
00488      IF ERPNDB-FILE-STATUS NOT = '00'                             EL521
00489          MOVE 'ERROR OCCURED START - ERPNDB'                      EL521
00490                                  TO  WS-ABEND-MESSAGE             EL521
00491          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00492          GO TO ABEND-PGM.                                         EL521
00493                                                                   EL521
00494      EJECT                                                        EL521
00495                                                                   EL521
00496  1150-SIP.                                                        EL521
00497      READ ERPNDB NEXT RECORD.                                     EL521
00498                                                                   EL521
00499      IF ERPNDB-FILE-STATUS = '10'                                 EL521
00500          GO TO 1190-SIP.                                          EL521
00501                                                                   EL521
00502      IF ERPNDB-FILE-STATUS NOT = '00'                             EL521
00503          MOVE 'ERROR OCCURED READNEXT - ERPNDB'                   EL521
00504                                  TO  WS-ABEND-MESSAGE             EL521
00505          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00506          GO TO ABEND-PGM.                                         EL521
00507                                                                   EL521
00508      IF PB-COMPANY-CD NOT = WS-COMPANY-CD                         EL521
00509          GO TO 1190-SIP.                                          EL521
00510                                                                   EL521
00511      IF (DTE-CLIENT = 'UCL') AND                                  EL521
00512         (PB-RECORD-ON-HOLD)  AND                                  EL521
00513         (NOT PB-CANCELLATION)                                     EL521
00514          NEXT SENTENCE                                            EL521
00515       ELSE                                                        EL521
00516      IF PB-CREDIT-SELECT-DT LESS THAN WS-PULL-DATE OR             EL521
00517         PB-CREDIT-SELECT-DT  = WS-PULL-DATE                       EL521
00518          IF PB-CREDIT-ACCEPT-DT = LOW-VALUES OR                   EL521
00519             PB-CREDIT-ACCEPT-DT = WS-PULL-DATE                    EL521
00520              NEXT SENTENCE                                        EL521
00521          ELSE                                                     EL521
00522              GO TO 1150-SIP                                       EL521
00523      ELSE                                                         EL521
00524          GO TO 1150-SIP.                                          EL521
00525                                                                   EL521
CIDMOD*************************************************************
CIDMOD* THIS LOGIC WAS ADDED TO ACCOMMODATE THE EXECUTION OF A
CIDMOD* "MID MONTH" BILLING (EL562) FOR COMMERCIAL FEDERAL AND PEOPLES
CIDMOD*************************************************************
CIDMOD     IF DTE-CLIENT = 'CID'
              evaluate company-name
                 when '      COMMERCIAL FEDERAL'
                    IF PB-ENTRY-BATCH (1:2) = 'CF'
                       CONTINUE
                    ELSE
                       GO TO 1150-SIP
                    END-IF
                 when '        PEOPLES TRUST'
                    IF (PB-ENTRY-BATCH (1:2) = 'PT')
                       OR (PB-ENTRY-BATCH (5:2) = 'PT')
                       CONTINUE
                    ELSE
                       GO TO 1150-SIP
                    END-IF
                 when '        SUNFLOWER BANK'
                    IF PB-ENTRY-BATCH (1:2) = 'SB'
                       CONTINUE
                    ELSE
                       GO TO 1150-SIP
                    END-IF
                 when '      FIRST PREMIER BANK'
                    IF PB-ENTRY-BATCH (1:2) = 'FP'
                       CONTINUE
                    ELSE
                       GO TO 1150-SIP
                    END-IF
022616           when '        MACHENS CONLEY  '
022616              IF pb-ACCOUNT = '0000311700' OR '0000312400' OR
022616                 '0000314000' OR '0000314100' OR '0000314200' OR
022616                 '0000314400' OR '0000314600' OR '0000314700' OR
022616                 '0000314800' OR '0000457900' OR '0000652900' OR
022616                 '0000763300' OR '0000763600' OR '0000763800' OR
022616                 '0000836500'
022616                 CONTINUE
022616              ELSE
022616                 GO TO 1150-SIP
022616              END-IF
              END-EVALUATE
           end-if

CIDMOD*************************************************************
00526      MOVE SPACES TO EXTRACT-INTERFACE-RECORD.                     EL521
00527                                                                   EL521
00528      IF PB-I-INDV-GRP-CD = 'I'                                    EL521
00529         MOVE '1' TO PB-I-INDV-GRP-CD                              EL521
00530      ELSE                                                         EL521
00531      IF PB-I-INDV-GRP-CD = 'G'                                    EL521
00532         MOVE '2' TO PB-I-INDV-GRP-CD.                             EL521
00533                                                                   EL521
00534      MOVE 'EX'                   TO  EX-RECORD-ID.                EL521
00535      MOVE '1'                    TO  EX-POSITIONING-CODE.         EL521
00536      MOVE 'A'                    TO  EX-EXTRACT-CODE.             EL521
00537      MOVE CF-COMPANY-CD          TO  EX-COMPANY-CD.               EL521
00538      MOVE CF-COMPANY-ID          TO  EX-COMPANY-ID.               EL521
00539      MOVE 'A'                    TO  EX-RECORD-TYPE.              EL521
00540      MOVE PB-CARRIER             TO  EX-SA-CARRIER.               EL521
00541      MOVE PB-GROUPING            TO  EX-SA-GROUPING.              EL521
00542      MOVE PB-STATE               TO  EX-SA-STATE.                 EL521
00543      MOVE PB-ACCOUNT             TO  EX-SA-ACCOUNT.               EL521
00544      MOVE PB-CERT-EFF-DT         TO  EX-SA-CERT-EFF-DT.           EL521
00545      MOVE PB-CERT-NO             TO  EX-SA-CERT-NO.               EL521
00546      MOVE PB-RECORD-TYPE         TO  EX-SA-RECORD-TYPE.           EL521
00547      MOVE PENDING-BUSINESS       TO  EX-DATA-AREAS.               EL521
00548                                                                   EL521
00549      WRITE EXTRACT-INTERFACE-RECORD.                              EL521
00550                                                                   EL521
00551      ADD +1                      TO  WS-EXTRACT-AA-COUNT          EL521
00552                                      WS-EXTRACT-TOT-COUNT.        EL521
00553                                                                   EL521
00554      IF PB-ISSUE                                                  EL521
00555          ADD +1  TO  WS-EXT-ISSUE-COUNT.                          EL521
00556                                                                   EL521
00557      IF PB-CANCELLATION                                           EL521
00558          ADD +1  TO  WS-EXT-CANCEL-COUNT.                         EL521
00559                                                                   EL521
00560      IF PB-BATCH-TRAILER                                          EL521
00561          ADD +1  TO  WS-EXT-BATCH-COUNT.                          EL521
00562                                                                   EL521
00563      GO TO 1150-SIP.                                              EL521
00564                                                                   EL521
00565  1190-SIP.                                                        EL521
00566      CLOSE ERPNDB.                                                EL521
00567                                                                   EL521
00568      IF ERPNDB-FILE-STATUS NOT = '00'                             EL521
00569          MOVE 'ERPNDB  '         TO  WS-FEM-FILE-NAME             EL521
00570          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
00571          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00572          GO TO ABEND-PGM.                                         EL521
00573                                                                   EL521
00574      EJECT                                                        EL521
00575                                                                   EL521
00576  1200-SIP.                                                        EL521
00577 *    NOTE ******************************************************* EL521
00578 *         *      POSITION THE PENDING CLAIMS FILE AT THE BE-    * EL521
00579 *         *  GINNING OF THE COMPANY TO LOAD AND RELEASE ALL     * EL521
00580 *         *  PENDING CLAIM RECORDS TO THE EXTRACT SORT.         * EL521
00581 *         *******************************************************.EL521
00582                                                                   EL521
00583      OPEN INPUT ERPNDC.                                           EL521
00584                                                                   EL521
pemtst     IF ERPNDC-FILE-STATUS = '9%' OR '9+'
PEMTST        GO TO 1300-SIP
PEMTST     END-IF
00585      IF ERPNDC-FILE-STATUS  = '00' OR '97'                        EL521
00586          NEXT SENTENCE                                            EL521
00587        ELSE                                                       EL521
00588          MOVE 'ERPNDC  '         TO  WS-FEM-FILE-NAME             EL521
00589          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
00590          MOVE ERPNDC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00591          GO TO ABEND-PGM.                                         EL521
00592                                                                   EL521
00593      MOVE LOW-VALUES             TO  PC-CONTROL-PRIMARY           EL521
00594      MOVE WS-COMPANY-CD          TO  PC-COMPANY-CD                EL521
00595                                                                   EL521
00596      START ERPNDC                                                 EL521
00597          KEY IS GREATER THAN PC-CONTROL-PRIMARY.                  EL521
00598                                                                   EL521
pemuni     IF ERPNDC-FILE-STATUS = '23' or '10'                         EL521
00600          DISPLAY 'EL521 NO RECORDS FOUND - ERPNDC  FOR CO - '     EL521
00601              WS-COMPANY-ID UPON CONSOLE                           EL521
00602          GO TO 1290-SIP.                                          EL521
00603                                                                   EL521
00604      IF ERPNDC-FILE-STATUS NOT = '00'                             EL521
00605          MOVE 'ERROR OCCURED START - ERPNDC'                      EL521
00606                                  TO  WS-ABEND-MESSAGE             EL521
00607          MOVE ERPNDC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00608          GO TO ABEND-PGM.                                         EL521
00609                                                                   EL521
00610      EJECT                                                        EL521
00611                                                                   EL521
00612  1250-SIP.                                                        EL521
00613      READ ERPNDC NEXT RECORD.                                     EL521
00614                                                                   EL521
00615      IF ERPNDC-FILE-STATUS = '10'                                 EL521
00616          GO TO 1290-SIP.                                          EL521
00617                                                                   EL521
00618      IF ERPNDC-FILE-STATUS NOT = '00'                             EL521
00619          MOVE 'ERROR OCCURED READNEXT - ERPNDC'                   EL521
00620                                  TO  WS-ABEND-MESSAGE             EL521
00621          MOVE ERPNDC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00622          GO TO ABEND-PGM.                                         EL521
00623                                                                   EL521
00624      IF PC-COMPANY-CD NOT = WS-COMPANY-CD                         EL521
00625          GO TO 1290-SIP.                                          EL521
00626                                                                   EL521
00627      IF CO-HAS-CLAS-IC-CLAIM                                      EL521
00628          NEXT SENTENCE                                            EL521
00629      ELSE                                                         EL521
00630          IF PC-CREDIT-SELECT-DT LESS THAN WS-PULL-DATE OR         EL521
00631             PC-CREDIT-SELECT-DT  = WS-PULL-DATE                   EL521
00632               IF PC-CREDIT-ACCEPT-DT = LOW-VALUES OR              EL521
00633                  PC-CREDIT-ACCEPT-DT = WS-PULL-DATE               EL521
00634                   NEXT SENTENCE                                   EL521
00635               ELSE                                                EL521
00636                   GO TO 1250-SIP                                  EL521
00637          ELSE                                                     EL521
00638              GO TO 1250-SIP.                                      EL521
00639                                                                   EL521
00640      MOVE SPACES TO EXTRACT-INTERFACE-RECORD.                     EL521
00641                                                                   EL521
00642      MOVE 'EX'                   TO  EX-RECORD-ID.                EL521
00643      MOVE '1'                    TO  EX-POSITIONING-CODE.         EL521
00644      MOVE 'A'                    TO  EX-EXTRACT-CODE.             EL521
00645      MOVE CF-COMPANY-CD          TO  EX-COMPANY-CD.               EL521
00646      MOVE CF-COMPANY-ID          TO  EX-COMPANY-ID.               EL521
00647      MOVE 'B'                    TO  EX-RECORD-TYPE.              EL521
00648      MOVE PC-CARRIER             TO  EX-SA-CARRIER.               EL521
00649      MOVE PC-GROUPING            TO  EX-SA-GROUPING.              EL521
00650      MOVE PC-STATE               TO  EX-SA-STATE.                 EL521
00651      MOVE PC-ACCOUNT             TO  EX-SA-ACCOUNT.               EL521
00652      MOVE PC-CERT-EFF-DT         TO  EX-SA-CERT-EFF-DT.           EL521
00653      MOVE PC-CERT-NO             TO  EX-SA-CERT-NO.               EL521
00654      MOVE PC-RECORD-TYPE         TO  EX-SA-RECORD-TYPE.           EL521
00655      MOVE PENDING-CLAIMS         TO  EX-DATA-AREAS.               EL521
00656                                                                   EL521
00657      WRITE EXTRACT-INTERFACE-RECORD                               EL521
00658                                                                   EL521
00659      ADD +1                      TO  WS-EXTRACT-AB-COUNT          EL521
00660                                      WS-EXTRACT-TOT-COUNT.        EL521
00661                                                                   EL521
00662      IF PC-CLAIMS                                                 EL521
00663          ADD +1  TO  WS-EXT-CLAIMS-COUNT.                         EL521
00664                                                                   EL521
00665      IF PC-RESERVES                                               EL521
00666          ADD +1  TO  WS-EXT-RESERV-COUNT.                         EL521
00667                                                                   EL521
00668      GO TO 1250-SIP.                                              EL521
00669                                                                   EL521
00670  1290-SIP.                                                        EL521
00671      CLOSE ERPNDC.                                                EL521
00672                                                                   EL521
00673      IF ERPNDC-FILE-STATUS NOT = '00'                             EL521
00674          MOVE 'ERPNDC  '         TO  WS-FEM-FILE-NAME             EL521
00675          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
00676          MOVE ERPNDC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00677          GO TO ABEND-PGM.                                         EL521
00678                                                                   EL521
00679      EJECT                                                        EL521
00680                                                                   EL521
00681  1300-SIP.                                                        EL521
00682 *    NOTE ******************************************************* EL521
00683 *         *      POSITION THE CERTIFICATE CHANGE FILE AT THE    * EL521
00684 *         *  BEGINNING OF THE COMPANY TO LOAD AND RELEASE ALL   * EL521
00685 *         *  CHANGES TO CERTIFICATES FROM THE ONLINE CERT FILE. * EL521
00686 *         *******************************************************.EL521
00687                                                                   EL521
00688      OPEN INPUT ERCRTC.                                           EL521
00689                                                                   EL521
PEMTST     IF ERCRTC-FILE-STATUS = '9%' OR '9+'
PEMTST        GO TO 1400-SIP
PEMTST     END-IF
00690      IF ERCRTC-FILE-STATUS  = '00' OR '97'                        EL521
00691          NEXT SENTENCE                                            EL521
00692        ELSE                                                       EL521
00693          MOVE 'ERCRTC  '         TO  WS-FEM-FILE-NAME             EL521
00694          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
00695          MOVE ERCRTC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00696          GO TO ABEND-PGM.                                         EL521
00697                                                                   EL521
00698      MOVE LOW-VALUES             TO  CC-CONTROL-PRIMARY           EL521
00699      MOVE WS-COMPANY-CD          TO  CC-COMPANY-CD                EL521
00700                                                                   EL521
00701      START ERCRTC                                                 EL521
00702          KEY IS GREATER THAN CC-CONTROL-PRIMARY.                  EL521
00703                                                                   EL521
pemuni     IF ERCRTC-FILE-STATUS = '23' or '10'                         EL521
00705          DISPLAY 'EL521 NO RECORDS FOUND - ERCRTC  FOR CO - '     EL521
00706              WS-COMPANY-ID UPON CONSOLE                           EL521
00707          GO TO 1390-SIP.                                          EL521
00708                                                                   EL521
00709      IF ERCRTC-FILE-STATUS NOT = '00'                             EL521
00710          MOVE 'ERROR OCCURED START - ERCRTC'                      EL521
00711                                  TO  WS-ABEND-MESSAGE             EL521
00712          MOVE ERCRTC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00713          GO TO ABEND-PGM.                                         EL521
00714                                                                   EL521
00715      EJECT                                                        EL521
00716                                                                   EL521
00717  1350-SIP.                                                        EL521
00718      READ ERCRTC NEXT RECORD.                                     EL521
00719                                                                   EL521
00720      IF ERCRTC-FILE-STATUS = '10'                                 EL521
00721          GO TO 1390-SIP.                                          EL521
00722                                                                   EL521
00723      IF ERCRTC-FILE-STATUS NOT = '00'                             EL521
00724          MOVE 'ERROR OCCURED READNEXT - ERCRTC'                   EL521
00725                                  TO  WS-ABEND-MESSAGE             EL521
00726          MOVE ERCRTC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00727          GO TO ABEND-PGM.                                         EL521
00728                                                                   EL521
00729      IF CC-COMPANY-CD NOT = WS-COMPANY-CD                         EL521
00730          GO TO 1390-SIP.                                          EL521
00731                                                                   EL521
00732      IF CC-CREDIT-SELECT-DT LESS THAN WS-PULL-DATE  OR            EL521
00733         CC-CREDIT-SELECT-DT  = WS-PULL-DATE                       EL521
00734          IF CC-CREDIT-ACCEPT-DT = LOW-VALUES OR                   EL521
00735             CC-CREDIT-ACCEPT-DT = WS-PULL-DATE                    EL521
00736              NEXT SENTENCE                                        EL521
00737          ELSE                                                     EL521
00738              GO TO 1350-SIP                                       EL521
00739      ELSE                                                         EL521
00740          GO TO 1350-SIP.                                          EL521
00741                                                                   EL521
00742      MOVE SPACES TO EXTRACT-INTERFACE-RECORD.                     EL521
00743                                                                   EL521
00744      MOVE 'EX'                   TO  EX-RECORD-ID.                EL521
00745      MOVE '1'                    TO  EX-POSITIONING-CODE.         EL521
00746      MOVE 'A'                    TO  EX-EXTRACT-CODE.             EL521
00747      MOVE CF-COMPANY-CD          TO  EX-COMPANY-CD.               EL521
00748      MOVE CF-COMPANY-ID          TO  EX-COMPANY-ID.               EL521
00749      MOVE 'C'                    TO  EX-RECORD-TYPE.              EL521
00750      MOVE CC-CARRIER             TO  EX-SA-CARRIER.               EL521
00751      MOVE CC-GROUPING            TO  EX-SA-GROUPING.              EL521
00752      MOVE CC-STATE               TO  EX-SA-STATE.                 EL521
00753      MOVE CC-ACCOUNT             TO  EX-SA-ACCOUNT.               EL521
00754      MOVE CC-CERT-EFF-DT         TO  EX-SA-CERT-EFF-DT.           EL521
00755      MOVE CC-CERT-NO             TO  EX-SA-CERT-NO.               EL521
00756      MOVE '1'                    TO  EX-SA-RECORD-TYPE.           EL521
00757      MOVE PENDING-MAINT-TO-CERT-FILE                              EL521
00758                                  TO  EX-DATA-AREAS                EL521
00759                                                                   EL521
00760      WRITE EXTRACT-INTERFACE-RECORD                               EL521
00761                                                                   EL521
00762      ADD +1                      TO  WS-EXTRACT-AC-COUNT          EL521
00763                                      WS-EXTRACT-TOT-COUNT.        EL521
00764                                                                   EL521
00765      GO TO 1350-SIP.                                              EL521
00766                                                                   EL521
00767  1390-SIP.                                                        EL521
00768      CLOSE ERCRTC.                                                EL521
00769                                                                   EL521
00770      IF ERCRTC-FILE-STATUS NOT = ZEROS                            EL521
00771          MOVE 'ERCRTC  '         TO  WS-FEM-FILE-NAME             EL521
00772          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
00773          MOVE ERCRTC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00774          GO TO ABEND-PGM.                                         EL521
00775                                                                   EL521
00776      EJECT                                                        EL521
00777                                                                   EL521
00778  1400-SIP.                                                        EL521
00779 *    NOTE ******************************************************* EL521
00780 *         *      POSITION THE PAYMENTS AND ADJUSTMENST FILE AT  * EL521
00781 *         *  THE BEGINNING OF THE COMPANY TO LOAD AND RELEASE   * EL521
00782 *         *  ALL PAYMENTS AND ADJUSTMENTS TO THE EXTRACT SORT.  * EL521
00783 *         *******************************************************.EL521
00784                                                                   EL521
00785      OPEN INPUT ERPYAJ.                                           EL521
00786                                                                   EL521
00787      IF ERPYAJ-FILE-STATUS  = '00' OR '97'                        EL521
00788          NEXT SENTENCE                                            EL521
00789        ELSE                                                       EL521
00790          MOVE 'ERPYAJ  '         TO  WS-FEM-FILE-NAME             EL521
00791          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
00792          MOVE ERPYAJ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00793          GO TO ABEND-PGM.                                         EL521
00794                                                                   EL521
00795      MOVE LOW-VALUES             TO  PY-CONTROL-PRIMARY.          EL521
00796      MOVE WS-COMPANY-CD          TO  PY-COMPANY-CD.               EL521
00797                                                                   EL521
00798      START ERPYAJ                                                 EL521
00799          KEY IS GREATER THAN PY-CONTROL-PRIMARY.                  EL521
00800                                                                   EL521
pemuni     IF ERPYAJ-FILE-STATUS = '23' or '10'                         EL521
00802          DISPLAY 'EL521 NO RECORDS FOUND - ERPYAJ  FOR CO - '     EL521
00803              WS-COMPANY-ID UPON CONSOLE                           EL521
00804          GO TO 1490-SIP.                                          EL521
00805                                                                   EL521
00806      IF ERPYAJ-FILE-STATUS NOT = ZERO                             EL521
00807          MOVE 'ERROR OCCURED START - ERPYAJ'                      EL521
00808                                  TO  WS-ABEND-MESSAGE             EL521
00809          MOVE ERPYAJ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00810          GO TO ABEND-PGM.                                         EL521
00811                                                                   EL521
00812      EJECT                                                        EL521
00813                                                                   EL521
00814  1450-SIP.                                                        EL521
00815      READ ERPYAJ NEXT RECORD.                                     EL521
00816                                                                   EL521
00817      IF ERPYAJ-FILE-STATUS = '10'                                 EL521
00818          GO TO 1490-SIP.                                          EL521
00819                                                                   EL521
00820      IF ERPYAJ-FILE-STATUS NOT = ZERO                             EL521
00821          MOVE 'ERROR OCCURED READNEXT - ERPYAJ'                   EL521
00822                                  TO  WS-ABEND-MESSAGE             EL521
00823          MOVE ERPYAJ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00824          GO TO ABEND-PGM.                                         EL521
00825                                                                   EL521
00826      IF PY-COMPANY-CD NOT = WS-COMPANY-CD                         EL521
00827          GO TO 1490-SIP.                                          EL521
00828                                                                   EL521
00829      IF PY-CREDIT-SELECT-DT LESS THAN WS-PULL-DATE  OR            EL521
00830         PY-CREDIT-SELECT-DT  = WS-PULL-DATE                       EL521
00831          IF PY-CREDIT-ACCEPT-DT = LOW-VALUES OR                   EL521
00832             PY-CREDIT-ACCEPT-DT = WS-PULL-DATE                    EL521
00833              NEXT SENTENCE                                        EL521
00834          ELSE                                                     EL521
00835              GO TO 1450-SIP                                       EL521
00836      ELSE                                                         EL521
00837          GO TO 1450-SIP.                                          EL521
00838                                                                   EL521
CIDMOD*************************************************************
CIDMOD* THIS LOGIC WAS ADDED TO ACCOMMODATE THE EXECUTION OF A
CIDMOD* "MID MONTH" BILLING (EL562) FOR COMMERCIAL FEDERAL
CIDMOD*************************************************************
CIDMOD     IF DTE-CLIENT = 'CID'
CIDMOD        IF COMPANY-NAME = '      COMMERCIAL FEDERAL'
CIDMOD           IF PY-FIN-RESP = '0000713100'
CIDMOD              CONTINUE
CIDMOD           ELSE
CIDMOD              GO TO 1450-SIP
CIDMOD           END-IF
061802        ELSE
061802           IF COMPANY-NAME = '        PEOPLES TRUST'
061802              IF PY-FIN-RESP = '0000845800'
061802                 CONTINUE
061802              ELSE
061802                 GO TO 1450-SIP
061802              END-IF
080702           ELSE
080702              IF COMPANY-NAME = '        SUNFLOWER BANK'
080702                 IF PY-FIN-RESP = '0000681700'
080702                    CONTINUE
080702                 ELSE
080702                    GO TO 1450-SIP
080702                 END-IF
061206              ELSE
061206                 IF COMPANY-NAME = '      FIRST PREMIER BANK'
061206                    IF PY-FIN-RESP = '0000713100'
061206                       CONTINUE
061206                    ELSE
061206                       GO TO 1450-SIP
061206                    END-IF
061206                 END-IF
080702              END-IF
061802           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD*************************************************************
00839      MOVE SPACES TO EXTRACT-INTERFACE-RECORD.                     EL521
00840                                                                   EL521
00841      MOVE 'EX'                   TO  EX-RECORD-ID.                EL521
00842      MOVE '1'                    TO  EX-POSITIONING-CODE.         EL521
00843      MOVE 'A'                    TO  EX-EXTRACT-CODE.             EL521
00844      MOVE CF-COMPANY-CD          TO  EX-COMPANY-CD.               EL521
00845      MOVE CF-COMPANY-ID          TO  EX-COMPANY-ID.               EL521
00846      MOVE 'D'                    TO  EX-RECORD-TYPE.              EL521
00847      MOVE PY-CARRIER             TO  EX-SB-CARRIER.               EL521
00848      MOVE PY-GROUPING            TO  EX-SB-GROUPING.              EL521
00849      MOVE PY-FIN-RESP            TO  EX-SB-FIN-RESP.              EL521
00850      MOVE PY-ACCOUNT             TO  EX-SB-ACCOUNT.               EL521
00851      MOVE PY-FILE-SEQ-NO         TO  EX-SB-FILE-SEQ-NO.           EL521
00852      MOVE PY-RECORD-TYPE         TO  EX-SB-RECORD-TYPE.           EL521
00853      MOVE PENDING-PAY-ADJ        TO  EX-DATA-AREAS.               EL521
00854                                                                   EL521
00855      WRITE EXTRACT-INTERFACE-RECORD.                              EL521
00856                                                                   EL521
00857      ADD +1                      TO  WS-EXTRACT-AD-COUNT          EL521
00858                                      WS-EXTRACT-TOT-COUNT.        EL521
00859                                                                   EL521
00860      GO TO 1450-SIP.                                              EL521
00861                                                                   EL521
00862  1490-SIP.                                                        EL521
00863      CLOSE ERPYAJ.                                                EL521
00864                                                                   EL521
00865      IF ERPYAJ-FILE-STATUS NOT = ZEROS                            EL521
00866          MOVE 'ERPYAJ  '         TO  WS-FEM-FILE-NAME             EL521
00867          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
00868          MOVE ERPYAJ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00869          GO TO ABEND-PGM.                                         EL521
00870                                                                   EL521
00871      EJECT                                                        EL521
00872                                                                   EL521
00873  1500-SIP.                                                        EL521
00874 *    NOTE ******************************************************* EL521
00875 *         *      POSITION THE PAYMENTS AND ADJUSTMENTS FILE AT  * EL521
00876 *         *  THE BEGINNING OF THE COMPANY TO LOAD AND RELEASE   * EL521
00877 *         *  ALL PAYMENTS AND ADJUSTMENTS TO THE EXTRACT SORT.  * EL521
00878 *         *******************************************************.EL521
00879                                                                   EL521
00880      OPEN INPUT ERREPY.                                           EL521
00881                                                                   EL521
PEMTST     IF ERREPY-FILE-STATUS = '9%' OR '9+'
PEMTST        GO TO 1600-SIP
PEMTST     END-IF
00882      IF ERREPY-FILE-STATUS  = '00' OR '97'                        EL521
00883          NEXT SENTENCE                                            EL521
00884        ELSE                                                       EL521
00885          MOVE 'ERREPY  '         TO  WS-FEM-FILE-NAME             EL521
00886          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
00887          MOVE ERREPY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00888          GO TO ABEND-PGM.                                         EL521
00889                                                                   EL521
00890      MOVE LOW-VALUES             TO  RP-CONTROL-PRIMARY.          EL521
00891      MOVE WS-COMPANY-CD          TO  RP-COMPANY-CD.               EL521
00892                                                                   EL521
00893      START ERREPY                                                 EL521
00894          KEY IS GREATER THAN RP-CONTROL-PRIMARY.                  EL521
00895                                                                   EL521
pemuni     IF ERREPY-FILE-STATUS = '23' or '10'                         EL521
00897          DISPLAY 'EL521 NO RECORDS FOUND - ERREPY  FOR CO - '     EL521
00898              WS-COMPANY-ID UPON CONSOLE                           EL521
00899          GO TO 1590-SIP.                                          EL521
00900                                                                   EL521
00901      IF ERREPY-FILE-STATUS NOT = ZERO                             EL521
00902          MOVE 'ERROR OCCURED START - ERREPY'                      EL521
00903                                  TO  WS-ABEND-MESSAGE             EL521
00904          MOVE ERREPY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00905          GO TO ABEND-PGM.                                         EL521
00906                                                                   EL521
00907      EJECT                                                        EL521
00908                                                                   EL521
00909  1550-SIP.                                                        EL521
00910      READ ERREPY NEXT RECORD.                                     EL521
00911                                                                   EL521
00912      IF ERREPY-FILE-STATUS = '10'                                 EL521
00913          GO TO 1590-SIP.                                          EL521
00914                                                                   EL521
00915      IF ERREPY-FILE-STATUS NOT = ZERO                             EL521
00916          MOVE 'ERROR OCCURED READNEXT - ERREPY'                   EL521
00917                                  TO  WS-ABEND-MESSAGE             EL521
00918          MOVE ERREPY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00919          GO TO ABEND-PGM.                                         EL521
00920                                                                   EL521
00921      IF RP-COMPANY-CD NOT = WS-COMPANY-CD                         EL521
00922          GO TO 1590-SIP.                                          EL521
00923                                                                   EL521
00924      IF RP-CREDIT-SELECT-DT LESS THAN WS-PULL-DATE  OR            EL521
00925         RP-CREDIT-SELECT-DT  = WS-PULL-DATE                       EL521
00926          IF RP-CREDIT-ACCEPT-DT = LOW-VALUES OR                   EL521
00927             RP-CREDIT-ACCEPT-DT = WS-PULL-DATE                    EL521
00928              NEXT SENTENCE                                        EL521
00929          ELSE                                                     EL521
00930              GO TO 1550-SIP                                       EL521
00931      ELSE                                                         EL521
00932          GO TO 1550-SIP.                                          EL521
00933                                                                   EL521
00934      MOVE SPACES TO EXTRACT-INTERFACE-RECORD.                     EL521
00935                                                                   EL521
00936      MOVE 'EX'                   TO  EX-RECORD-ID.                EL521
00937      MOVE '1'                    TO  EX-POSITIONING-CODE.         EL521
00938      MOVE 'A'                    TO  EX-EXTRACT-CODE.             EL521
00939      MOVE CF-COMPANY-CD          TO  EX-COMPANY-CD.               EL521
00940      MOVE CF-COMPANY-ID          TO  EX-COMPANY-ID.               EL521
00941      MOVE 'E'                    TO  EX-RECORD-TYPE.              EL521
00942      MOVE RP-CARRIER             TO  EX-SD-CARRIER.               EL521
00943      MOVE RP-GROUPING            TO  EX-SD-GROUPING.              EL521
00944      MOVE RP-STATE               TO  EX-SD-STATE.                 EL521
00945      MOVE RP-ACCOUNT             TO  EX-SD-ACCOUNT.               EL521
00946      MOVE RP-FILE-SEQ-NO         TO  EX-SD-FILE-SEQ-NO.           EL521
00947      MOVE RP-RECORD-TYPE         TO  EX-SD-RECORD-TYPE.           EL521
00948                                                                   EL521
00949      MOVE PENDING-RETRO-REIN-ADJUSTMENTS                          EL521
00950                                  TO  EX-DATA-AREAS.               EL521
00951                                                                   EL521
00952      WRITE EXTRACT-INTERFACE-RECORD                               EL521
00953                                                                   EL521
00954      ADD +1                      TO  WS-EXTRACT-AE-COUNT          EL521
00955                                      WS-EXTRACT-TOT-COUNT.        EL521
00956                                                                   EL521
00957      GO TO 1550-SIP.                                              EL521
00958                                                                   EL521
00959  1590-SIP.                                                        EL521
00960      CLOSE ERREPY.                                                EL521
00961                                                                   EL521
00962      IF ERREPY-FILE-STATUS NOT = ZEROS                            EL521
00963          MOVE 'ERREPY  '         TO  WS-FEM-FILE-NAME             EL521
00964          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
00965          MOVE ERREPY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00966          GO TO ABEND-PGM.                                         EL521
00967                                                                   EL521
00968      EJECT                                                        EL521
00969                                                                   EL521
00970  1600-SIP.                                                        EL521
00971 *    NOTE ******************************************************* EL521
00972 *         *      POSITION THE REQUEST FILE AT THE BEGINNING OF  * EL521
00973 *         *  THE COMPANY TO LOAD AND RELEASE ALL REQUEST RECORDS* EL521
00974 *         *  TO THE EXTRACT SORT.                               * EL521
00975 *         *******************************************************.EL521
00976                                                                   EL521
00977      OPEN INPUT ERRQST.                                           EL521
00978                                                                   EL521
pemtst     IF ERRQST-FILE-STATUS = '9%' OR '9+'
PEMTST        GO TO 1700-SIP
PEMTST     END-IF
00979      IF ERRQST-FILE-STATUS  = '00' OR '97'                        EL521
00980          NEXT SENTENCE                                            EL521
00981        ELSE                                                       EL521
00982          MOVE 'ERRQST  '         TO  WS-FEM-FILE-NAME             EL521
00983          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
00984          MOVE ERRQST-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
00985          GO TO ABEND-PGM.                                         EL521
00986                                                                   EL521
00987      MOVE LOW-VALUES             TO  RQ-CONTROL-PRIMARY.          EL521
00988      MOVE WS-COMPANY-CD          TO  RQ-COMPANY-CD.               EL521
00989                                                                   EL521
00990      START ERRQST                                                 EL521
00991          KEY IS GREATER THAN RQ-CONTROL-PRIMARY.                  EL521
00992                                                                   EL521
pemuni     IF ERRQST-FILE-STATUS = '23' or '10'                         EL521
00994          DISPLAY 'EL521 NO RECORDS FOUND - ERRQST  FOR CO - '     EL521
00995              WS-COMPANY-ID UPON CONSOLE                           EL521
00996          GO TO 1690-SIP.                                          EL521
00997                                                                   EL521
00998      IF ERRQST-FILE-STATUS NOT = ZERO                             EL521
00999          MOVE 'ERROR OCCURED START - ERRQST'                      EL521
01000                                  TO  WS-ABEND-MESSAGE             EL521
01001          MOVE ERRQST-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
01002          GO TO ABEND-PGM.                                         EL521
01003                                                                   EL521
01004      EJECT                                                        EL521
01005                                                                   EL521
01006  1650-SIP.                                                        EL521
01007      READ ERRQST NEXT RECORD.                                     EL521
01008                                                                   EL521
01009      IF ERRQST-FILE-STATUS = '10'                                 EL521
01010          GO TO 1690-SIP.                                          EL521
01011                                                                   EL521
01012      IF ERRQST-FILE-STATUS NOT = ZERO                             EL521
01013          MOVE 'ERROR OCCURED READNEXT - ERRQST'                   EL521
01014                                  TO  WS-ABEND-MESSAGE             EL521
01015          MOVE ERRQST-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
01016          GO TO ABEND-PGM.                                         EL521
01017                                                                   EL521
01018      IF RQ-COMPANY-CD NOT = WS-COMPANY-CD                         EL521
01019          GO TO 1690-SIP.                                          EL521
01020                                                                   EL521
01021      IF RQ-CREDIT-SELECT-DT LESS THAN WS-PULL-DATE  OR            EL521
01022         RQ-CREDIT-SELECT-DT  = WS-PULL-DATE                       EL521
01023          IF RQ-CREDIT-ACCEPT-DT = LOW-VALUES OR                   EL521
01024             RQ-CREDIT-ACCEPT-DT = WS-PULL-DATE                    EL521
01025              NEXT SENTENCE                                        EL521
01026          ELSE                                                     EL521
01027              GO TO 1650-SIP                                       EL521
01028      ELSE                                                         EL521
01029          GO TO 1650-SIP.                                          EL521
01030                                                                   EL521
01031      MOVE SPACES TO EXTRACT-INTERFACE-RECORD.                     EL521
01032                                                                   EL521
01033      MOVE 'EX'                   TO  EX-RECORD-ID.                EL521
01034      MOVE '1'                    TO  EX-POSITIONING-CODE.         EL521
01035      MOVE 'A'                    TO  EX-EXTRACT-CODE.             EL521
01036      MOVE CF-COMPANY-CD          TO  EX-COMPANY-CD.               EL521
01037      MOVE CF-COMPANY-ID          TO  EX-COMPANY-ID.               EL521
01038      MOVE 'F'                    TO  EX-RECORD-TYPE.              EL521
01039      MOVE RQ-ENTRY-BATCH         TO  EX-SF-ENTRY-BATCH.           EL521
01040      MOVE AR-REQUEST-RECORD      TO  EX-DATA-AREAS.               EL521
01041                                                                   EL521
01042      WRITE EXTRACT-INTERFACE-RECORD                               EL521
01043                                                                   EL521
01044      ADD +1                      TO  WS-EXTRACT-AF-COUNT          EL521
01045                                      WS-EXTRACT-TOT-COUNT.        EL521
01046                                                                   EL521
01047      GO TO 1650-SIP.                                              EL521
01048                                                                   EL521
01049  1690-SIP.                                                        EL521
01050      CLOSE ERRQST.                                                EL521
01051                                                                   EL521
01052 *    IF ERRQST-FILE-STATUS NOT = ZEROS                            EL521
01053 *        MOVE 'ERRQST  '         TO  WS-FEM-FILE-NAME             EL521
01054 *        MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
01055 *        MOVE ERRQST-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
01056 *        GO TO ABEND-PGM.                                         EL521
01057                                                                   EL521
01058      EJECT                                                        EL521
01059                                                                   EL521
01060  1700-SIP.                                                        EL521
01061      ADD WS-LINE-COUNT-MAX       TO  WS-LINE-COUNT.               EL521
01062      SET TOT-INDX TO +1.                                          EL521
01063      SET TOT-INDX DOWN BY 1.                                      EL521
01064                                                                   EL521
01065  1720-SIP.                                                        EL521
01066      SET TOT-INDX UP BY +1.                                       EL521
01067                                                                   EL521
01068      IF TOT-INDX GREATER +12                                      EL521
01069          GO TO 1900-SIP.                                          EL521
01070                                                                   EL521
01071      MOVE SPACES                 TO  WS-DETAIL1.                  EL521
01072                                                                   EL521
01073      MOVE WS-TOT-DESC (TOT-INDX) TO  WS-D2-DESC.                  EL521
01074      MOVE WS-TOT-COUNT (TOT-INDX) TO WS-D2-COUNT.                 EL521
01075                                                                   EL521
01076      MOVE WS-DETAIL1             TO  PRT.                         EL521
01077      PERFORM WRITE-A-LINE                                         EL521
01078                                                                   EL521
01079      GO TO 1720-SIP.                                              EL521
01080                                                                   EL521
01081  1900-SIP.                                                        EL521
01082      CLOSE ELCNTL.                                                EL521
01083                                                                   EL521
01084      MOVE 'ERROR OCCURED CLOSING -'  TO  WS-FILE-ERROR-MESSAGE    EL521
01085                                                                   EL521
01086      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL521
01087          MOVE 'ELCNTL  '         TO  WS-FEM-FILE-NAME             EL521
01088          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
01089          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
01090          GO TO ABEND-PGM.                                         EL521
01091                                                                   EL521
01092  1999-EXIT.                                                       EL521
01093      EXIT.                                                        EL521
01094      EJECT                                                        EL521
01095                                                                   EL521
01096  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL521
01097                                                                   EL521
01098      EJECT                                                        EL521
01099                                                                   EL521
01100  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL521
01101                                                                   EL521
01102      EJECT                                                        EL521
01103  WRITE-HEADINGS SECTION.                                          EL521
01104 ***************************************************************** EL521
01105 *                                                               * EL521
01106 *                            ELCWHS1.                           * EL521
01107 *                            VMOD=2.001                         * EL521
01108 *                                                               * EL521
01109 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL521
01110 *****************************************************************.EL521
01111  WHS-010.                                                         EL521
01112      IF  WS-H2-DATE EQUAL SPACES                                  EL521
01113          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL521
01114          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL521
01115          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL521
01116                                                                   EL521
01117      ADD +1  TO  WS-PAGE.                                         EL521
01118      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL521
01119      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL521
01120      MOVE ZERO                   TO  WS-LINE-COUNT.               EL521
01121                                                                   EL521
01122      MOVE WS-HEADING1            TO  PRT.                         EL521
01123      MOVE '1'                    TO  X.                           EL521
01124      PERFORM WRITE-PRINTER.                                       EL521
01125                                                                   EL521
01126      MOVE WS-HEADING2            TO  PRT.                         EL521
01127      MOVE ' '                    TO  X.                           EL521
01128      PERFORM WRITE-PRINTER.                                       EL521
01129                                                                   EL521
01130      MOVE WS-HEADING3            TO  PRT.                         EL521
01131      MOVE ' '                    TO  X.                           EL521
01132      PERFORM WRITE-PRINTER.                                       EL521
01133                                                                   EL521
01134      MOVE WS-HEADING4            TO  PRT.                         EL521
01135      MOVE ' '                    TO  X.                           EL521
01136      PERFORM WRITE-PRINTER.                                       EL521
01137                                                                   EL521
01138                                                                   EL521
01139  WHS-020. COPY ELCWHS2.                                           EL521
01140                                                                   EL521
01141      EJECT                                                        EL521
01142  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL521
01143                                                                   EL521
01144      MOVE P-CTL TO LCP-ASA                                        EL521
01145      PERFORM LCP-WRITE-POS-PRT                                    EL521
01146          THRU LCP-WRITE-END-PRT.                                  EL521
01147                                                                   EL521
01148  WPS-EXIT.                                                        EL521
01149                                                                   EL521
01150      EXIT.                                                        EL521
01151                                                                   EL521
01152      EJECT                                                        EL521
01153  OPEN-FILES SECTION.                                              EL521
01154                                                                   EL521
01155  OFS-010.                                                         EL521
01156      OPEN INPUT ELCNTL                                            EL521
01157           OUTPUT PRNTR                                            EL521
01158                  EXTRACT-INTERFACE-FILE.                          EL521
01159                                                                   EL521
01160      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL521
01161          NEXT SENTENCE                                            EL521
01162        ELSE                                                       EL521
01163          MOVE 'ELCNTL  '         TO  WS-FEM-FILE-NAME             EL521
01164          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL521
01165          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL521
01166          GO TO ABEND-PGM.                                         EL521
01167                                                                   EL521
01168  OFS-EXIT.                                                        EL521
01169      EXIT.                                                        EL521
01170      EJECT                                                        EL521
01171                                                                   EL521
01172  ABEND-PGM SECTION. COPY ELCABEND SUPPRESS.                       EL521
01173 /                                                                 EL521
01174  LCP-WRITE-POS-PRT SECTION.                                       EL521
01175      IF LCP-ASA = '+'                                             EL521
01176          WRITE PRT AFTER 0 LINE                                   EL521
01177      ELSE                                                         EL521
01178      IF LCP-ASA = ' '                                             EL521
01179          WRITE PRT AFTER ADVANCING 1 LINE                         EL521
01180      ELSE                                                         EL521
01181      IF LCP-ASA = '0'                                             EL521
01182          WRITE PRT AFTER ADVANCING 2 LINE                         EL521
01183      ELSE                                                         EL521
01184      IF LCP-ASA = '-'                                             EL521
01185          WRITE PRT AFTER ADVANCING 3 LINE                         EL521
01186      ELSE                                                         EL521
01187      IF LCP-ASA = '1'                                             EL521
01188          WRITE PRT AFTER ADVANCING PAGE                           EL521
01189      ELSE                                                         EL521
01190      IF LCP-ASA = '2'                                             EL521
01191          WRITE PRT AFTER ADVANCING LCP-CH2                        EL521
01192      ELSE                                                         EL521
01193      IF LCP-ASA = '3'                                             EL521
01194          WRITE PRT AFTER ADVANCING LCP-CH3                        EL521
01195      ELSE                                                         EL521
01196      IF LCP-ASA = '4'                                             EL521
01197          WRITE PRT AFTER ADVANCING LCP-CH4                        EL521
01198      ELSE                                                         EL521
01199      IF LCP-ASA = '5'                                             EL521
01200          WRITE PRT AFTER ADVANCING LCP-CH5                        EL521
01201      ELSE                                                         EL521
01202      IF LCP-ASA = '6'                                             EL521
01203          WRITE PRT AFTER ADVANCING LCP-CH6                        EL521
01204      ELSE                                                         EL521
01205      IF LCP-ASA = '7'                                             EL521
01206          WRITE PRT AFTER ADVANCING LCP-CH7                        EL521
01207      ELSE                                                         EL521
01208      IF LCP-ASA = '8'                                             EL521
01209          WRITE PRT AFTER ADVANCING LCP-CH8                        EL521
01210      ELSE                                                         EL521
01211      IF LCP-ASA = '9'                                             EL521
01212          WRITE PRT AFTER ADVANCING LCP-CH9                        EL521
01213      ELSE                                                         EL521
01214      IF LCP-ASA = 'A'                                             EL521
01215          WRITE PRT AFTER ADVANCING LCP-CH10                       EL521
01216      ELSE                                                         EL521
01217      IF LCP-ASA = 'B'                                             EL521
01218          WRITE PRT AFTER ADVANCING LCP-CH11                       EL521
01219      ELSE                                                         EL521
01220      IF LCP-ASA = 'C'                                             EL521
01221          WRITE PRT AFTER ADVANCING LCP-CH12                       EL521
01222      ELSE                                                         EL521
01223      IF LCP-ASA = 'V'                                             EL521
01224          WRITE PRT AFTER ADVANCING LCP-P01                        EL521
01225      ELSE                                                         EL521
01226      IF LCP-ASA = 'W'                                             EL521
01227          WRITE PRT AFTER ADVANCING LCP-P02                        EL521
01228      ELSE                                                         EL521
01229      DISPLAY 'ASA CODE ERROR'.                                    EL521
01230  LCP-WRITE-END-PRT.                                               EL521
01231      EXIT.                                                        EL521
01232                                                                   EL521
