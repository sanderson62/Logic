00001  IDENTIFICATION DIVISION.                                         10/11/97
00002                                                                   EL535
00003  PROGRAM-ID.                 EL535 .                                 LV002
00004 *              PROGRAM CONVERTED BY                               EL535
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL535
00006 *              CONVERSION DATE 04/10/96 10:19:03.                 EL535
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL535
00008 *                            VMOD=2.008.                          EL535
00009                                                                   EL535
00010 *AUTHOR.     LOGIC, INC.                                          EL535
00011 *            DALLAS, TEXAS.                                       EL535
00012                                                                   EL535
00013 *DATE-COMPILED.                                                   EL535
00014                                                                   EL535
00015 *SECURITY.   *****************************************************EL535
00016 *            *                                                   *EL535
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL535
00018 *            *                                                   *EL535
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL535
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL535
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL535
00022 *            *                                                   *EL535
00023 *            *****************************************************EL535
00024                                                                   EL535
00025 *REMARKS.                                                         EL535
00026 *        THIS PROGRAM IS ONLY RUN TO RESTORE THE CREDIT FILES     EL535
00027 *    UNLOADED FROM EL521 (SXXXCRED) IF THE NEED EXISTS.           EL535
00028                                                                   EL535
00029                                                                   EL535
00030 *    INPUT FILES  - ELCNTL - CONTROL FILE                         EL535
00031 *                 - EREXTR - CREDIT EXTRACT INTERFACE             EL535
00032 *                 - ELDATE - DATE CARD FILE                       EL535
00033                                                                   EL535
00034 *    OUTPUT FILES - ERPNDB - PENDING BUSINESS FILE                EL535
00035 *                 - ERPNDC - PENDING CLAIMS FILE                  EL535
00036 *                 - ERPYAJ - PAYMENTS AND ADJUSTMENTS FILE        EL535
00037 *                 - ERCRTC - CERTIFICATE CHANGES FILE             EL535
00038 *                 - ERREPY - PENDING RETRO/REIN ADJUSTMENTS       EL535
00039 *                 - ERRQST - REQUEST FILE                         EL535
00040                                                                   EL535
00041      EJECT                                                        EL535
00042  ENVIRONMENT DIVISION.                                            EL535
00043  CONFIGURATION SECTION.                                           EL535
00044  SPECIAL-NAMES.                                                   EL535
00045      C02 IS LCP-CH2                                               EL535
00046      C03 IS LCP-CH3                                               EL535
00047      C04 IS LCP-CH4                                               EL535
00048      C05 IS LCP-CH5                                               EL535
00049      C06 IS LCP-CH6                                               EL535
00050      C07 IS LCP-CH7                                               EL535
00051      C08 IS LCP-CH8                                               EL535
00052      C09 IS LCP-CH9                                               EL535
00053      C10 IS LCP-CH10                                              EL535
00054      C11 IS LCP-CH11                                              EL535
00055      C12 IS LCP-CH12                                              EL535
00056      S01 IS LCP-P01                                               EL535
00057      S02 IS LCP-P02.                                              EL535
00058                                                                   EL535
00059  INPUT-OUTPUT SECTION.                                            EL535
00060                                                                   EL535
00061  FILE-CONTROL.                                                    EL535
00062                                                                   EL535
00063      SELECT EXTRACT-INTERFACE-FILE                                EL535
00064                              ASSIGN TO SYS010-UT-2400-S-SYS010.   EL535
00065                                                                   EL535
00066      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL535
00067                                                                   EL535
00068      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL535
00069                                                                   EL535
00070      SELECT ELCNTL           ASSIGN TO SYS021-FBA1-ELCNTL         EL535
00071                              ORGANIZATION IS INDEXED              EL535
00072                              ACCESS IS DYNAMIC                    EL535
00073                              RECORD KEY IS CF-CONTROL-PRIMARY     EL535
00074                              FILE STATUS IS ELCNTL-FILE-STATUS.   EL535
00075                                                                   EL535
00076      SELECT ERPNDB           ASSIGN TO SYS022-FBA1-ERPNDB         EL535
00077                              ORGANIZATION IS INDEXED              EL535
00078                              ACCESS IS DYNAMIC                    EL535
00079                              RECORD KEY IS PB-CONTROL-PRIMARY     EL535
00080                              FILE STATUS IS ERPNDB-FILE-STATUS.   EL535
00081                                                                   EL535
00082      SELECT ERPNDC           ASSIGN TO SYS023-FBA1-ERPNDC         EL535
00083                              ORGANIZATION IS INDEXED              EL535
00084                              ACCESS IS DYNAMIC                    EL535
00085                              RECORD KEY IS PC-CONTROL-PRIMARY     EL535
00086                              FILE STATUS IS ERPNDC-FILE-STATUS.   EL535
00087                                                                   EL535
00088      SELECT ERPYAJ           ASSIGN TO SYS024-FBA1-ERPYAJ         EL535
00089                              ORGANIZATION IS INDEXED              EL535
00090                              ACCESS IS DYNAMIC                    EL535
00091                              RECORD KEY IS PY-CONTROL-PRIMARY     EL535
00092                              FILE STATUS IS ERPYAJ-FILE-STATUS.   EL535
00093                                                                   EL535
00094      SELECT ERCRTC          ASSIGN TO SYS025-FBA1-ERCRTC          EL535
00095                              ORGANIZATION IS INDEXED              EL535
00096                              ACCESS IS DYNAMIC                    EL535
00097                              RECORD KEY IS CC-CONTROL-PRIMARY     EL535
00098                              FILE STATUS IS ERCRTC-FILE-STATUS.   EL535
00099                                                                   EL535
00100      SELECT ELREPT           ASSIGN TO SYS026-FBA1-ELREPT         EL535
00101                              ORGANIZATION IS INDEXED              EL535
00102                              ACCESS IS DYNAMIC                    EL535
00103                              RECORD KEY IS RF-CONTROL-PRIMARY     EL535
00104                              FILE STATUS IS DTE-VSAM-FLAGS.       EL535
00105                                                                   EL535
00106      SELECT ERREPY           ASSIGN TO SYS027-FBA1-ERREPY         EL535
00107                              ORGANIZATION IS INDEXED              EL535
00108                              ACCESS IS DYNAMIC                    EL535
00109                              RECORD KEY IS RP-CONTROL-PRIMARY     EL535
00110                              FILE STATUS IS ERREPY-FILE-STATUS.   EL535
00111                                                                   EL535
00112      SELECT ERRQST           ASSIGN TO SYS027-FBA1-ERRQST         EL535
00113                              ORGANIZATION IS INDEXED              EL535
00114                              ACCESS IS DYNAMIC                    EL535
00115                              RECORD KEY IS RQ-CONTROL-PRIMARY     EL535
00116                              FILE STATUS IS ERRQST-FILE-STATUS.   EL535
00117                                                                   EL535
00118      SELECT SORT-FILE        ASSIGN TO SYS001-FBA1-S-SORTWK1.     EL535
00119                                                                   EL535
00120      EJECT                                                        EL535
00121  DATA DIVISION.                                                   EL535
00122                                                                   EL535
00123  FILE SECTION.                                                    EL535
00124                                                                   EL535
00125  FD  DISK-DATE               COPY ELCDTEFD.                       EL535
00126                                                                   EL535
00127  FD  ELREPT                  COPY ELCRPTFD.                       EL535
00128                                                                   EL535
00129                              COPY ELCREPT.                        EL535
00130                                                                   EL535
00131  FD  ELCNTL.                                                      EL535
00132                                                                   EL535
00133                                      COPY ELCCNTL.                EL535
00134                                                                   EL535
00135      EJECT                                                        EL535
00136  FD  ERPNDB.                                                      EL535
00137                                                                   EL535
00138                                      COPY ERCPNDB.                EL535
00139                                                                   EL535
00140      EJECT                                                        EL535
00141  FD  ERPNDC.                                                      EL535
00142                                                                   EL535
00143                                      COPY ERCPNDC.                EL535
00144                                                                   EL535
00145      EJECT                                                        EL535
00146  FD  ERPYAJ.                                                      EL535
00147                                                                   EL535
00148                                      COPY ERCPYAJ.                EL535
00149                                                                   EL535
00150      EJECT                                                        EL535
00151  FD  ERCRTC.                                                      EL535
00152                                                                   EL535
00153                                      COPY ERCCRTC.                EL535
00154                                                                   EL535
00155      EJECT                                                        EL535
00156  FD  ERREPY.                                                      EL535
00157                                                                   EL535
00158                                      COPY ERCREPY.                EL535
00159                                                                   EL535
00160      EJECT                                                        EL535
00161  FD  ERRQST.                                                      EL535
00162                                                                   EL535
00163                                      COPY ERCRQST.                EL535
00164                                                                   EL535
00165      EJECT                                                        EL535
00166  FD  EXTRACT-INTERFACE-FILE          COPY ERCEXTFD.               EL535
00167                                                                   EL535
00168                                      COPY ERCEXTR.                EL535
00169                                                                   EL535
00170  FD  PRNTR                           COPY ELCPRTFD.               EL535
00171                                                                   EL535
00172  SD  SORT-FILE.                                                   EL535
00173                                                                   EL535
00174  01  SORT-RECORD.                                                 EL535
00175      05  FILLER                       PIC XX.                     EL535
00176      05  SR-CONTROL-PRIMARY.                                      EL535
00177          10  SR-COMPANY-CD            PIC X.                      EL535
00178          10  SR-ENTRY-BATCH           PIC X(6).                   EL535
00179          10  SR-BATCH-SEQ-NO          PIC S9(4)         COMP.     EL535
00180          10  SR-BATCH-CHG-SEQ-NO      PIC S9(4)         COMP.     EL535
00181      05  FILLER                       PIC X(572).                 EL535
00182                                                                   EL535
00183                                                                   EL535
00184      EJECT                                                        EL535
00185  WORKING-STORAGE SECTION.                                         EL535
00186  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL535
00187  01  LCP-TIME-OF-DAY-68            PIC 9(6).                      EL535
00188  01  LCP-TIME-OF-DAY-74.                                          EL535
00189      05  LCP-TIME-74               PIC 9(6).                      EL535
00190      05  FILLER                    PIC 9(2).                      EL535
00191  77  LCP-ASA                       PIC X.                         EL535
00192                                                                   EL535
00193  77  FILLER  PIC X(32)   VALUE '********************************'.EL535
00194  77  FILLER  PIC X(32)   VALUE '*     EL535  WORKING STORAGE   *'.EL535
00195  77  FILLER  PIC X(32)   VALUE '*********** VM 2.008 ***********'.EL535
00196                                                                   EL535
00197  01  FILLER                          COMP-3.                      EL535
00198      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL535
00199      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +60.   EL535
00200      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL535
00201      05  WS-REPORT-SW                PIC S9          VALUE +1.    EL535
00202      05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  EL535
00203      05  WS-MONTHS-WORK              PIC S9(3)       VALUE ZERO.  EL535
00204      05  WS-MONTH-END                PIC S9          VALUE ZERO.  EL535
00205          88  THIS-IS-NOT-MONTH-END                   VALUE ZERO.  EL535
00206          88  THIS-IS-MONTH-END                       VALUE +1.    EL535
00207      05  WS-CURRENT-TIME             PIC S9(7)       VALUE ZERO.  EL535
00208      05  WS-RECORDS-RELEASED         PIC S9(7)       VALUE ZERO.  EL535
00209      05  WS-RECORDS-RETURNED         PIC S9(7)       VALUE ZERO.  EL535
00210                                                                   EL535
00211  01  FILLER                          COMP SYNC.                   EL535
00212      05  PGM-SUB                     PIC S9(4)       VALUE +535.  EL535
00213      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  EL535
00214      05  WS-LENGTH                   REDEFINES                    EL535
00215          WS-INDEX                    PIC S9(4).                   EL535
00216                                                                   EL535
00217  01  WS-TOTAL-TABLE.                                              EL535
00218      05  WS-TOTAL-1.                                              EL535
00219          10  WS-ERPNDB-DESC          PIC X(36)       VALUE        EL535
00220              ' PENDING BUSINESS RECORDS RESTORED- '.              EL535
00221          10  WS-ERPNDB-COUNT         PIC S9(7)       VALUE ZERO.  EL535
00222      05  WS-TOTAL-2.                                              EL535
00223          10  WS-ERPNDC-DESC          PIC X(36)       VALUE        EL535
00224              ' PENDING CLAIMS RECORDS RESTORED  - '.              EL535
00225          10  WS-ERPNDC-COUNT         PIC S9(7)       VALUE ZERO.  EL535
00226      05  WS-TOTAL-3.                                              EL535
00227          10  WS-ERCRTC-DESC          PIC X(36)       VALUE        EL535
00228              ' CERT CHANGE RECORDS RESTORED     - '.              EL535
00229          10  WS-ERCRTC-COUNT         PIC S9(7)       VALUE ZERO.  EL535
00230      05  WS-TOTAL-4.                                              EL535
00231          10  WS-ERPYAJ-DESC          PIC X(36)       VALUE        EL535
00232              ' PAYMENT/ADJ RECORDS RESTORED     - '.              EL535
00233          10  WS-ERPYAJ-COUNT         PIC S9(7)       VALUE ZERO.  EL535
00234      05  WS-TOTAL-5.                                              EL535
00235          10  WS-ERREPY-DESC          PIC X(36)       VALUE        EL535
00236              ' RETRO/EPEC  RECORDS RESTORED     - '.              EL535
00237          10  WS-ERREPY-COUNT         PIC S9(7)       VALUE ZERO.  EL535
00238      05  WS-TOTAL-6.                                              EL535
00239          10  WS-ERRQST-DESC          PIC X(36)       VALUE        EL535
00240              ' REQUEST RECORDS RESTORED         - '.              EL535
00241          10  WS-ERRQST-COUNT         PIC S9(7)       VALUE ZERO.  EL535
00242      05  WS-TOTAL-7.                                              EL535
00243          10  WS-TOTAL-DESC           PIC X(36)       VALUE        EL535
00244              ' ** TOTAL RECORDS RESTORED **     - '.              EL535
00245          10  WS-TOTAL-COUNT          PIC S9(7)       VALUE ZERO.  EL535
00246                                                                   EL535
00247  01  WS-TOT-TABLE REDEFINES WS-TOTAL-TABLE.                       EL535
00248      05  WS-TOTALS OCCURS 7 TIMES                                 EL535
00249                    INDEXED BY TOT-INDX.                           EL535
00250          10  WS-TOT-DESC             PIC X(36).                   EL535
00251          10  WS-TOT-COUNT            PIC S9(7).                   EL535
00252                                                                   EL535
00253  01  FILLER.                                                      EL535
00254      05  WS-DISPLAY-TIME             PIC 99B99B99.                EL535
00255      05  WS-DEL-BIN1                 PIC 9999-.                   EL535
00256                                                                   EL535
00257      05  X                           PIC X.                       EL535
00258      05  OLC-REPORT-NAME             PIC X(8) VALUE 'EL535'.      EL535
00259      05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.EL535
00260      05  WS-LAST-COMPANY-CD          PIC X      VALUE LOW-VALUES. EL535
00261      05  WS-LAST-CARRIER             PIC X      VALUE LOW-VALUES. EL535
00262      05  WS-LAST-BATCH               PIC X(6).                    EL535
00263      05  WS-DISPLAY-AMOUNT           PIC Z,ZZZ,ZZ9.99-.           EL535
00264      05  WS-DISPLAY-COUNT            PIC Z,ZZZ,ZZ9-.              EL535
00265      05  WS-PROCESSOR-ID             PIC X(4)        VALUE SPACES.EL535
00266      05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL535
00267      05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL535
00268      05  WS-RETURN-CODE              PIC S9(4)       COMP.        EL535
00269      05  WS-ZERO                     PIC S9          COMP-3       EL535
00270                                                      VALUE ZERO.  EL535
00271                                                                   EL535
00272      05  ELCNTL-FILE-STATUS          PIC XX          VALUE ZERO.  EL535
00273      05  ERPNDB-FILE-STATUS          PIC XX          VALUE ZERO.  EL535
00274      05  ERPNDC-FILE-STATUS          PIC XX          VALUE ZERO.  EL535
00275      05  ERCRTC-FILE-STATUS          PIC XX          VALUE ZERO.  EL535
00276      05  ERPYAJ-FILE-STATUS          PIC XX          VALUE ZERO.  EL535
00277      05  ERREPY-FILE-STATUS          PIC XX          VALUE ZERO.  EL535
00278      05  ERRQST-FILE-STATUS          PIC XX          VALUE ZERO.  EL535
00279                                                                   EL535
00280      05  WS-FILE-ERROR-MESSAGE.                                   EL535
00281          10  FILLER                  PIC X(24)       VALUE        EL535
00282              'ERROR OCCURED OPENING - '.                          EL535
00283          10  WS-FEM-FILE-NAME        PIC X(8).                    EL535
00284                                                                   EL535
00285      05  WS-BATCH-MISSING-SW         PIC XX VALUE LOW-VALUES.     EL535
00286          88  BATCH-MISSING                      VALUE HIGH-VALUES.EL535
00287                                                                   EL535
00288      05  WS-LAST-CERT-NO             PIC X(11).                   EL535
00289                                                                   EL535
00290      05  WS-SAVE-TIME                PIC 9(6).                    EL535
00291      05  WS-POST-DATE                PIC XX      VALUE LOW-VALUES.EL535
00292      05  WS-RUN-DT                   PIC XX      VALUE LOW-VALUES.   CL**2
00293      05  WS-SAVE-LAST-DT             PIC XX      VALUE LOW-VALUES.EL535
00294      05  WS-SAVE-DATE                PIC XX      VALUE LOW-VALUES.EL535
00295      05  WS-WORK-DATE1               PIC 9(6).                    EL535
00296      05  WS-WORK-DATE1-X             REDEFINES                    EL535
00297          WS-WORK-DATE1.                                           EL535
00298          10  WS-WORK-MM1             PIC 99.                      EL535
00299          10  WS-WORK-DD1             PIC 99.                      EL535
00300          10  WS-WORK-YY1             PIC 99.                      EL535
00301                                                                   EL535
00302      05  WS-WORK-DATE2               PIC 9(6).                    EL535
00303      05  WS-WORK-DATE2-X             REDEFINES                    EL535
00304          WS-WORK-DATE2.                                           EL535
00305          10  WS-WORK-MM2             PIC 99.                      EL535
00306          10  WS-WORK-DD2             PIC 99.                      EL535
00307          10  WS-WORK-YY2             PIC 99.                      EL535
00308                                                                   EL535
00309      05  WS-MONTH-END-DATE           PIC XX      VALUE LOW-VALUES.EL535
00310                                                                   EL535
00311      05  NEW-PB-CERT.                                             EL535
00312          10  NEW-PB-CERT-EFF-DT      PIC XX.                      EL535
00313          10  NEW-PB-CERT-NO          PIC X(11).                   EL535
00314                                                                   EL535
00315      05  HELD-PB-CERT.                                            EL535
00316          10  HELD-PB-CERT-EFF-DT     PIC XX        VALUE SPACES.  EL535
00317          10  HELD-PB-CERT-NO         PIC X(11) VALUE SPACES.      EL535
00318                                                                   EL535
00319      05  WS-COMPANY-ID               PIC X(3).                    EL535
00320      05  WS-COMPANY-CD               PIC X.                       EL535
00321                                                                   EL535
00322      05  WS-COMPANY-NAME.                                         EL535
00323          10  WS-CN-CHAR              PIC X                        EL535
00324              OCCURS 30 TIMES         INDEXED BY CN1.              EL535
00325                                                                   EL535
00326      05  WS-COMPANY-NAME2.                                        EL535
00327          10  WS-CN2-CHAR             PIC X                        EL535
00328              OCCURS 30 TIMES         INDEXED BY CN2.              EL535
00329      05  WS-INITIALS.                                             EL535
00330          10  WS-INITIAL1             PIC X.                       EL535
00331          10  WS-INITIAL2             PIC X.                       EL535
00332                                                                   EL535
00333      05  WS-PHONETIC-WORK-AREA.                                   EL535
00334          10  WS-PWA-PHONETIC-NAME    PIC X(4).                    EL535
00335          10  WS-PWA-NAME             PIC X(16).                   EL535
00336          10  WS-PWA-LANGUAGE         PIC X.                       EL535
00337                                                                   EL535
00338      05  WS-BIN-DATE-WORK-X.                                      EL535
00339          10  WS-BIN-DATE-WORK        PIC S9(4)                    EL535
00340                                      COMP.                        EL535
00341                                                                   EL535
00342      EJECT                                                        EL535
00343  01  WS-HEADING1.                                                 EL535
00344      05  FILLER                      PIC X(49)       VALUE '1'.   EL535
00345      05  WS-H1-TITLE                 PIC X(71)       VALUE        EL535
00346          '   ** RESTORE CREDIT FILES ** '.                        EL535
00347      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL535'.      EL535
00348                                                                   EL535
00349  01  WS-HEADING2.                                                 EL535
00350      05  FILLER                      PIC X           VALUE SPACES.EL535
00351      05  WS-H2-COMPANY-ID            PIC XXX         VALUE 'XXX'. EL535
00352      05  FILLER                      PIC X(42)       VALUE SPACES.EL535
00353      05  WS-H2-CLIENT-NAME           PIC X(30)       VALUE SPACES.EL535
00354      05  FILLER                      PIC X(38)       VALUE SPACES.EL535
00355      05  WS-H2-DATE                  PIC X(08)       VALUE SPACES.EL535
00356                                                                   EL535
00357  01  WS-HEADING3.                                                 EL535
00358      05  FILLER                      PIC X(54)       VALUE SPACES.EL535
00359      05  WS-H3-DATE                  PIC X(18)       VALUE SPACES.EL535
00360      05  FILLER                      PIC X(45)       VALUE SPACES.EL535
00361      05  FILLER                      PIC X(4)        VALUE 'PAGE'.EL535
00362      05  WS-H3-PAGE                  PIC ZZ,ZZ9      VALUE ZERO.  EL535
00363                                                                   EL535
00364  01  WS-HEADING4.                                                 EL535
00365      05  FILLER                      PIC X(133)      VALUE        EL535
00366          '0           FILE                           COUNTS '.    EL535
00367                                                                   EL535
00368  01  WS-DETAIL1.                                                  EL535
00369      05  FILLER                      PIC X.                       EL535
00370      05  WS-D1-DESC                  PIC X(36).                   EL535
00371      05  WS-D1-COUNT                 PIC Z,ZZZ,ZZZ,ZZ9-.          EL535
00372      05  FILLER                      PIC X(82).                   EL535
00373                                                                   EL535
00374  01  WS-DETAIL1A                     REDEFINES                    EL535
00375      WS-DETAIL1.                                                  EL535
00376      05  WS-KEY-DATA                 PIC X(133).                  EL535
00377                                                                   EL535
00378      05  WS-PB-KEY-DATA              REDEFINES                    EL535
00379          WS-KEY-DATA.                                             EL535
00380          10  FILLER                  PIC XX.                      EL535
00381          10  WS-PB-ENTRY-BATCH       PIC X(6).                    EL535
00382          10  FILLER                  PIC XX.                      EL535
00383          10  WS-PB-BATCH-SEQ-NO      PIC 9(4).                    EL535
00384          10  FILLER                  PIC XX.                      EL535
00385          10  WS-PB-BATCH-CHG-SEQ-NO  PIC 9(4).                    EL535
00386          10  FILLER                  PIC XX.                      EL535
00387          10  WS-PB-RECORD-TYPE       PIC X.                       EL535
00388          10  FILLER                  PIC XX.                      EL535
00389          10  WS-PB-MESSAGE           PIC X(45).                   EL535
00390          10  FILLER                  PIC X(63).                   EL535
00391                                                                   EL535
00392      05  WS-PC-KEY-DATA              REDEFINES                    EL535
00393          WS-KEY-DATA.                                             EL535
00394          10  FILLER                  PIC XX.                      EL535
00395          10  WS-PC-CARRIER           PIC X.                       EL535
00396          10  FILLER                  PIC XX.                      EL535
00397          10  WS-PC-GROUPING          PIC X(6).                    EL535
00398          10  FILLER                  PIC XX.                      EL535
00399          10  WS-PC-STATE             PIC XX.                      EL535
00400          10  FILLER                  PIC XX.                      EL535
00401          10  WS-PC-ACCOUNT           PIC X(10).                   EL535
00402          10  FILLER                  PIC XX.                      EL535
00403          10  WS-PC-CERT-EFF-DT       PIC X(8).                    EL535
00404          10  FILLER                  PIC XX.                      EL535
00405          10  WS-PC-CERT-NO           PIC X(11).                   EL535
00406          10  FILLER                  PIC XX.                      EL535
00407          10  WS-PC-CLAIM-NO          PIC X(7).                    EL535
00408          10  FILLER                  PIC XX.                      EL535
00409          10  WS-PC-CHECK-NO          PIC X(7).                    EL535
00410          10  FILLER                  PIC XX.                      EL535
00411          10  WS-PC-RECORD-TYPE       PIC X.                       EL535
00412          10  FILLER                  PIC XX.                      EL535
00413          10  WS-PC-RECORD-SEQUENCE   PIC 9(4).                    EL535
00414          10  FILLER                  PIC XX.                      EL535
00415          10  WS-PC-MESSAGE           PIC X(45).                   EL535
00416          10  FILLER                  PIC X(9).                    EL535
00417                                                                   EL535
00418      05  WS-CC-KEY-DATA              REDEFINES                    EL535
00419          WS-KEY-DATA.                                             EL535
00420          10  FILLER                  PIC XX.                      EL535
00421          10  WS-CC-CARRIER           PIC X.                       EL535
00422          10  FILLER                  PIC XX.                      EL535
00423          10  WS-CC-GROUPING          PIC X(6).                    EL535
00424          10  FILLER                  PIC XX.                      EL535
00425          10  WS-CC-STATE             PIC XX.                      EL535
00426          10  FILLER                  PIC XX.                      EL535
00427          10  WS-CC-ACCOUNT           PIC X(10).                   EL535
00428          10  FILLER                  PIC XX.                      EL535
00429          10  WS-CC-CERT-EFF-DT       PIC X(8).                    EL535
00430          10  FILLER                  PIC XX.                      EL535
00431          10  WS-CC-CERT-NO           PIC X(11).                   EL535
00432          10  FILLER                  PIC XX.                      EL535
00433          10  WS-CC-FILE-SEQ-NO       PIC 9(8).                    EL535
00434          10  FILLER                  PIC XX.                      EL535
00435          10  WS-CC-MESSAGE           PIC X(45).                   EL535
00436          10  FILLER                  PIC X(26).                   EL535
00437                                                                   EL535
00438      05  WS-PY-KEY-DATA              REDEFINES                    EL535
00439          WS-KEY-DATA.                                             EL535
00440          10  FILLER                  PIC XX.                      EL535
00441          10  WS-PY-CARRIER           PIC X.                       EL535
00442          10  FILLER                  PIC XX.                      EL535
00443          10  WS-PY-GROUPING          PIC X(6).                    EL535
00444          10  FILLER                  PIC XX.                      EL535
00445          10  WS-PY-FIN-RESP          PIC X(6).                    EL535
00446          10  FILLER                  PIC XX.                      EL535
00447          10  WS-PY-ACCOUNT           PIC X(10).                   EL535
00448          10  FILLER                  PIC XX.                      EL535
00449          10  WS-PY-FILE-SEQ-NO       PIC 9(8).                    EL535
00450          10  FILLER                  PIC XX.                      EL535
00451          10  WS-PY-RECORD-TYPE       PIC X.                       EL535
00452          10  FILLER                  PIC XX.                      EL535
00453          10  WS-PY-MESSAGE           PIC X(45).                   EL535
00454          10  FILLER                  PIC X(42).                   EL535
00455                                                                   EL535
00456      05  WS-RP-KEY-DATA              REDEFINES                    EL535
00457          WS-KEY-DATA.                                             EL535
00458          10  FILLER                  PIC XX.                      EL535
00459          10  WS-RP-CARRIER           PIC X.                       EL535
00460          10  FILLER                  PIC XX.                      EL535
00461          10  WS-RP-GROUPING          PIC X(6).                    EL535
00462          10  FILLER                  PIC XX.                      EL535
00463          10  WS-RP-STATE             PIC X(2).                    EL535
00464          10  FILLER                  PIC XX.                      EL535
00465          10  WS-RP-ACCOUNT           PIC X(10).                   EL535
00466          10  FILLER                  PIC XX.                      EL535
00467          10  WS-RP-FILE-SEQ-NO       PIC 9(8).                    EL535
00468          10  FILLER                  PIC XX.                      EL535
00469          10  WS-RP-RECORD-TYPE       PIC X.                       EL535
00470          10  FILLER                  PIC XX.                      EL535
00471          10  WS-RP-MESSAGE           PIC X(45).                   EL535
00472          10  FILLER                  PIC X(46).                   EL535
00473                                                                   EL535
00474      05  WS-RQ-KEY-DATA              REDEFINES                    EL535
00475          WS-KEY-DATA.                                             EL535
00476          10  FILLER                  PIC XX.                      EL535
00477          10  WS-RQ-ENTRY-BATCH       PIC X(6).                    EL535
00478          10  FILLER                  PIC X(125).                  EL535
00479                                                                   EL535
00480      EJECT                                                        EL535
00481                                      COPY ELCDATE.                EL535
00482                                                                   EL535
00483      EJECT                                                        EL535
00484                                      COPY ELCDTECX.               EL535
00485                                                                   EL535
00486                                      COPY ELCDTEVR.                  CL**2
00487                                                                      CL**2
00488      EJECT                                                        EL535
00489  PROCEDURE DIVISION.                                              EL535
00490                                                                   EL535
00491  0000-LOAD-DATE-CARD.        COPY ELCDTERX SUPPRESS.              EL535
00492                                                                   EL535
00493  0000-MAIN-LOGIC SECTION.                                         EL535
00494                                                                   EL535
00495      PERFORM OPEN-FILES.                                          EL535
00496                                                                   EL535
00497      PERFORM GET-DATE.                                            EL535
00498                                                                   EL535
00499      SORT SORT-FILE                                               EL535
00500          ON ASCENDING KEY SR-CONTROL-PRIMARY                      EL535
00501              INPUT  PROCEDURE 1000-SORT-INPUT-PROCEDURE           EL535
00502              OUTPUT PROCEDURE 2000-SORT-OUTPUT-PROCEDURE.         EL535
00503                                                                   EL535
00504      IF SORT-RETURN GREATER THAN ZERO                             EL535
00505          MOVE 'SORT FAILED'      TO  WS-ABEND-MESSAGE             EL535
00506          MOVE SORT-RETURN        TO  WS-RETURN-CODE               EL535
00507          GO TO ABEND-PGM.                                         EL535
00508                                                                   EL535
00509      PERFORM CLOSE-FILES.                                         EL535
00510                                                                   EL535
00511      GOBACK.                                                      EL535
00512                                                                   EL535
00513      EJECT                                                        EL535
00514                                                                   EL535
00515  1000-SORT-INPUT-PROCEDURE SECTION.                               EL535
00516      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          EL535
00517      MOVE DTE-CLIENT             TO  CF-COMPANY-ID                EL535
00518                                      WS-PROCESSOR-ID.             EL535
00519      MOVE '1'                    TO  CF-RECORD-TYPE.              EL535
00520      MOVE SPACES                 TO  CF-ACCESS-CD-GENL.           EL535
00521      MOVE +0                     TO  CF-SEQUENCE-NO.              EL535
00522                                                                   EL535
00523      READ ELCNTL.                                                 EL535
00524                                                                   EL535
00525      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL535
00526          NEXT SENTENCE                                            EL535
00527        ELSE                                                       EL535
00528          MOVE 'ERROR OCCURED READ INITIAL - ELCNTL'               EL535
00529                                  TO  WS-ABEND-MESSAGE             EL535
00530          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
00531          GO TO ABEND-PGM.                                         EL535
00532                                                                   EL535
00533      MOVE CF-CL-MAIL-TO-NAME     TO  WS-COMPANY-NAME              EL535
00534                                      WS-H2-CLIENT-NAME.           EL535
00535      MOVE CF-COMPANY-ID          TO  WS-COMPANY-ID                EL535
00536                                      WS-H2-COMPANY-ID.            EL535
00537      MOVE CF-COMPANY-CD          TO  WS-COMPANY-CD.               EL535
00538      MOVE LOW-VALUES             TO  WS-LAST-CARRIER.             EL535
00539      ACCEPT LCP-TIME-OF-DAY-74 FROM TIME                          EL535
00540      MOVE LCP-TIME-74 TO LCP-TIME-OF-DAY-68                       EL535
00541                                                                   EL535
00542      MOVE  LCP-TIME-OF-DAY-68 TO WS-DISPLAY-TIME.                 EL535
00543      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'.            EL535
00544      DISPLAY 'BEGIN PROCESSING ' WS-H2-CLIENT-NAME  ' AT '        EL535
00545              WS-DISPLAY-TIME UPON CONSOLE.                        EL535
00546                                                                   EL535
00547      MOVE SPACES                 TO  WS-COMPANY-NAME2.            EL535
00548      SET CN1 TO +30.                                              EL535
00549                                                                   EL535
00550  1020-SIP.                                                        EL535
00551      IF WS-CN-CHAR (CN1) = SPACES                                 EL535
00552          IF CN1 GREATER THAN +1                                   EL535
00553              SET CN1 DOWN BY +1                                   EL535
00554              GO TO 1020-SIP                                       EL535
00555            ELSE                                                   EL535
00556              GO TO 1100-SIP.                                      EL535
00557                                                                   EL535
00558      SET WS-LENGTH TO CN1.                                        EL535
00559                                                                   EL535
00560      SUBTRACT WS-LENGTH FROM +30 GIVING WS-LENGTH.                EL535
00561      DIVIDE +2 INTO WS-LENGTH ROUNDED.                            EL535
00562                                                                   EL535
00563      IF WS-LENGTH NOT GREATER THAN ZERO                           EL535
00564          GO TO 1100-SIP.                                          EL535
00565                                                                   EL535
00566      SET CN2 TO CN1.                                              EL535
00567      SET CN2 UP BY WS-LENGTH.                                     EL535
00568                                                                   EL535
00569  1030-SIP.                                                        EL535
00570      MOVE WS-CN-CHAR (CN1) TO WS-CN2-CHAR (CN2).                  EL535
00571                                                                   EL535
00572      IF CN1 GREATER THAN +1                                       EL535
00573          SET CN1                                                  EL535
00574              CN2 DOWN BY +1                                       EL535
00575          GO TO 1030-SIP.                                          EL535
00576                                                                   EL535
00577      MOVE WS-COMPANY-NAME2       TO  WS-COMPANY-NAME.             EL535
00578                                                                   EL535
00579                                                                   EL535
00580      EJECT                                                        EL535
00581                                                                   EL535
00582  1100-SIP.                                                        EL535
00583 *    NOTE ******************************************************* EL535
00584 *         *      POSITION THE EXTRACT INTERFACE FILE AT THE     * EL535
00585 *         *  BEGINNING OF THE COMPANY IN ORDER TO UPDATE THE    * EL535
00586 *         *  CREDIT ACCEPT DATES ON THE ERPNDB, ERPNDC, ERPYAJ  * EL535
00587 *         *  FILES.                                             * EL535
00588 *         *******************************************************.EL535
00589                                                                   EL535
00590      READ EXTRACT-INTERFACE-FILE                                  EL535
00591          AT END                                                   EL535
00592              CLOSE EXTRACT-INTERFACE-FILE                         EL535
00593              GO TO 1999-EXIT.                                     EL535
00594                                                                   EL535
00595      IF EX-COMPANY-CD LESS THAN WS-COMPANY-CD                     EL535
00596          GO TO 1100-SIP                                           EL535
00597      ELSE                                                         EL535
00598          IF EX-COMPANY-CD GREATER THAN WS-COMPANY-CD              EL535
00599              GO TO 1999-EXIT.                                     EL535
00600                                                                   EL535
00601      IF EX-EXTRACT-CODE NOT = 'A'                                 EL535
00602          GO TO 1999-EXIT.                                         EL535
00603                                                                   EL535
00604      IF EX-RECORD-TYPE = 'A'                                      EL535
00605          MOVE EX-DATA-AREAS TO SORT-RECORD                        EL535
00606          RELEASE SORT-RECORD                                      EL535
00607          ADD +1 TO WS-RECORDS-RELEASED                            EL535
00608      ELSE                                                         EL535
00609          IF EX-RECORD-TYPE = 'B'                                  EL535
00610              IF CO-HAS-CLAS-IC-CLAIM                              EL535
00611                  NEXT SENTENCE                                    EL535
00612              ELSE                                                 EL535
00613                  PERFORM 4200-PROCESS-ERPNDC                      EL535
00614          ELSE                                                     EL535
00615              IF EX-RECORD-TYPE = 'C'                              EL535
00616                  PERFORM 4300-PROCESS-ERCRTC                      EL535
00617              ELSE                                                 EL535
00618                  IF EX-RECORD-TYPE = 'D'                          EL535
00619                      PERFORM 4400-PROCESS-ERPYAJ                  EL535
00620                  ELSE                                             EL535
00621                      IF EX-RECORD-TYPE = 'E'                      EL535
00622                          PERFORM 4500-PROCESS-ERREPY              EL535
00623                  ELSE                                             EL535
00624                      IF EX-RECORD-TYPE = 'F'                      EL535
00625                          PERFORM 4600-PROCESS-ERRQST.             EL535
00626                                                                   EL535
00627      GO TO 1100-SIP.                                              EL535
00628                                                                   EL535
00629  1999-EXIT.                                                       EL535
00630      EXIT.                                                        EL535
00631      EJECT                                                        EL535
00632                                                                   EL535
00633  2000-SORT-OUTPUT-PROCEDURE SECTION.                              EL535
00634      IF WS-RECORDS-RELEASED GREATER ZERO                          EL535
00635          NEXT SENTENCE                                            EL535
00636      ELSE                                                         EL535
00637          MOVE 'EL535 NO RECORDS ON FILE - JOB ABORTED '           EL535
00638                                  TO WS-ABEND-MESSAGE              EL535
00639          MOVE ZEROS              TO WS-ABEND-FILE-STATUS          EL535
00640          GO TO ABEND-PGM.                                         EL535
00641                                                                   EL535
00642  2100-SOP.                                                        EL535
00643      MOVE SPACES                 TO PENDING-BUSINESS.             EL535
00644                                                                   EL535
00645      RETURN SORT-FILE INTO PENDING-BUSINESS                       EL535
00646          AT END                                                   EL535
00647              GO TO 2900-SOP.                                      EL535
00648                                                                   EL535
00649      MOVE WS-SAVE-DATE           TO PB-LAST-MAINT-DT.             EL535
00650      MOVE WS-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             EL535
00651      MOVE WS-SAVE-TIME           TO PB-LAST-MAINT-HHMMSS.         EL535
00652                                                                   EL535
00653      WRITE PENDING-BUSINESS.                                      EL535
00654                                                                   EL535
00655      IF ERPNDB-FILE-STATUS = '22'                                 EL535
00656          MOVE SPACES             TO  WS-DETAIL1                   EL535
00657          MOVE PB-ENTRY-BATCH     TO  WS-PB-ENTRY-BATCH            EL535
00658          MOVE PB-BATCH-SEQ-NO    TO  WS-PB-BATCH-SEQ-NO           EL535
00659          MOVE PB-BATCH-CHG-SEQ-NO TO WS-PB-BATCH-CHG-SEQ-NO       EL535
00660          MOVE PB-RECORD-TYPE     TO  WS-PB-RECORD-TYPE            EL535
00661          MOVE 'DUPLICATE ERPNDB RECORD FOUND '                    EL535
00662                                  TO  WS-PB-MESSAGE                EL535
00663          MOVE WS-DETAIL1A        TO  PRT                          EL535
00664          PERFORM WRITE-A-LINE                                     EL535
00665          GO TO 2100-SOP.                                          EL535
00666                                                                   EL535
00667      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL535
00668          MOVE 'ERROR OCCURED WRITE - ERPNDB'                      EL535
00669                                  TO  WS-ABEND-MESSAGE             EL535
00670          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
00671          GO TO ABEND-PGM.                                         EL535
00672                                                                   EL535
00673      ADD +1 TO WS-ERPNDB-COUNT.                                   EL535
00674                                                                   EL535
00675      GO TO 2100-SOP.                                              EL535
00676                                                                   EL535
00677  2900-SOP.                                                        EL535
00678      MOVE SPACES                 TO  WS-H2-COMPANY-ID.            EL535
00679      MOVE '       FINAL TOTALS'  TO  WS-H2-CLIENT-NAME.           EL535
00680      ADD WS-LINE-COUNT-MAX       TO  WS-LINE-COUNT.               EL535
00681      SET TOT-INDX TO +1.                                          EL535
00682      SET TOT-INDX DOWN BY +1.                                     EL535
00683                                                                   EL535
00684  2920-SOP.                                                        EL535
00685      SET TOT-INDX UP BY +1.                                       EL535
00686                                                                   EL535
00687      IF TOT-INDX GREATER +7                                       EL535
00688          GO TO 2999-EXIT.                                         EL535
00689                                                                   EL535
00690      MOVE SPACES                 TO  WS-DETAIL1.                  EL535
00691                                                                   EL535
00692      MOVE WS-TOT-DESC (TOT-INDX) TO  WS-D1-DESC.                  EL535
00693      MOVE WS-TOT-COUNT (TOT-INDX) TO WS-D1-COUNT.                 EL535
00694                                                                   EL535
00695      MOVE WS-DETAIL1             TO  PRT.                         EL535
00696      PERFORM WRITE-A-LINE                                         EL535
00697                                                                   EL535
00698      GO TO 2920-SOP.                                              EL535
00699                                                                   EL535
00700  2999-EXIT.                                                       EL535
00701      EXIT.                                                        EL535
00702      EJECT                                                        EL535
00703                                                                   EL535
00704  4200-PROCESS-ERPNDC SECTION.                                     EL535
00705      MOVE EX-DATA-AREAS   TO  PENDING-CLAIMS.                     EL535
00706                                                                   EL535
00707      MOVE WS-SAVE-DATE           TO PC-LAST-MAINT-DT.             EL535
00708      MOVE WS-PROCESSOR-ID        TO PC-LAST-MAINT-BY.             EL535
00709      MOVE WS-SAVE-TIME           TO PC-LAST-MAINT-HHMMSS.         EL535
00710                                                                   EL535
00711      WRITE PENDING-CLAIMS.                                        EL535
00712                                                                   EL535
00713      IF ERPNDC-FILE-STATUS = '22'                                 EL535
00714          MOVE SPACES             TO  WS-DETAIL1                   EL535
00715          MOVE PC-CARRIER         TO  WS-PC-CARRIER                EL535
00716          MOVE PC-GROUPING        TO  WS-PC-GROUPING               EL535
00717          MOVE PC-STATE           TO  WS-PC-STATE                  EL535
00718          MOVE PC-ACCOUNT         TO  WS-PC-ACCOUNT                EL535
00719          MOVE PC-CERT-EFF-DT     TO  DC-BIN-DATE-1                EL535
00720          MOVE SPACE              TO  DC-OPTION-CODE               EL535
00721          PERFORM 8500-DATE-CONVERSION                             EL535
00722          MOVE DC-GREG-DATE-1-MDY TO  WS-PC-CERT-EFF-DT            EL535
00723          MOVE PC-CERT-NO         TO  WS-PC-CERT-NO                EL535
00724          MOVE PC-CLAIM-NO        TO  WS-PC-CLAIM-NO               EL535
00725          MOVE PC-CHECK-NO        TO  WS-PC-CHECK-NO               EL535
00726          MOVE PC-RECORD-TYPE     TO  WS-PC-RECORD-TYPE            EL535
00727          MOVE PC-RECORD-SEQUENCE TO  WS-PC-RECORD-SEQUENCE        EL535
00728          MOVE 'DUPLICATE ERPNDC RECORD FOUND   '                  EL535
00729                                  TO  WS-PC-MESSAGE                EL535
00730          MOVE WS-DETAIL1A        TO  PRT                          EL535
00731          PERFORM WRITE-A-LINE                                     EL535
00732          GO TO 4200-EXIT.                                         EL535
00733                                                                   EL535
00734      IF ERPNDC-FILE-STATUS NOT = ZERO                             EL535
00735          MOVE 'ERROR OCCURED READ - ERPNDC'                       EL535
00736                                  TO  WS-ABEND-MESSAGE             EL535
00737          MOVE ERPNDC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
00738          GO TO ABEND-PGM.                                         EL535
00739                                                                   EL535
00740      ADD +1 TO WS-ERPNDC-COUNT.                                   EL535
00741                                                                   EL535
00742  4200-EXIT.                                                       EL535
00743      EXIT.                                                        EL535
00744      EJECT                                                        EL535
00745                                                                   EL535
00746  4300-PROCESS-ERCRTC SECTION.                                     EL535
00747      MOVE EX-DATA-AREAS   TO  PENDING-MAINT-TO-CERT-FILE.         EL535
00748                                                                   EL535
00749      MOVE WS-SAVE-DATE           TO CC-LAST-MAINT-DT.             EL535
00750      MOVE WS-PROCESSOR-ID        TO CC-LAST-MAINT-BY.             EL535
00751      MOVE WS-SAVE-TIME           TO CC-LAST-MAINT-HHMMSS.         EL535
00752                                                                   EL535
00753      WRITE PENDING-MAINT-TO-CERT-FILE.                            EL535
00754                                                                   EL535
00755      IF ERCRTC-FILE-STATUS = '22'                                 EL535
00756          MOVE SPACES             TO  WS-DETAIL1                   EL535
00757          MOVE CC-CARRIER         TO  WS-CC-CARRIER                EL535
00758          MOVE CC-GROUPING        TO  WS-CC-GROUPING               EL535
00759          MOVE CC-STATE           TO  WS-CC-STATE                  EL535
00760          MOVE CC-ACCOUNT         TO  WS-CC-ACCOUNT                EL535
00761          MOVE CC-CERT-EFF-DT     TO  DC-BIN-DATE-1                EL535
00762          MOVE SPACE              TO  DC-OPTION-CODE               EL535
00763          PERFORM 8500-DATE-CONVERSION                             EL535
00764          MOVE DC-GREG-DATE-1-MDY TO  WS-CC-CERT-EFF-DT            EL535
00765          MOVE CC-CERT-NO         TO  WS-CC-CERT-NO                EL535
00766          MOVE CC-FILE-SEQ-NO     TO  WS-CC-FILE-SEQ-NO            EL535
00767          MOVE 'DUPLICATE ERCRTC RECORD FOUND   '                  EL535
00768                                  TO  WS-CC-MESSAGE                EL535
00769          MOVE WS-DETAIL1A        TO  PRT                          EL535
00770          PERFORM WRITE-A-LINE                                     EL535
00771          GO TO 4300-EXIT.                                         EL535
00772                                                                   EL535
00773      IF ERCRTC-FILE-STATUS NOT = ZERO                             EL535
00774          MOVE 'ERROR OCCURED READ - ERCRTC'                       EL535
00775                                  TO  WS-ABEND-MESSAGE             EL535
00776          MOVE ERCRTC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
00777          GO TO ABEND-PGM.                                         EL535
00778                                                                   EL535
00779      ADD +1 TO WS-ERCRTC-COUNT.                                   EL535
00780                                                                   EL535
00781  4300-EXIT.                                                       EL535
00782      EXIT.                                                        EL535
00783      EJECT                                                        EL535
00784                                                                   EL535
00785  4400-PROCESS-ERPYAJ SECTION.                                     EL535
00786      MOVE SPACES                 TO PENDING-PAY-ADJ.              EL535
00787      MOVE EX-DATA-AREAS          TO PENDING-PAY-ADJ.              EL535
00788                                                                   EL535
00789      MOVE WS-SAVE-DATE           TO PY-LAST-MAINT-DT.             EL535
00790      MOVE WS-PROCESSOR-ID        TO PY-LAST-MAINT-BY.             EL535
00791      MOVE WS-SAVE-TIME           TO PY-LAST-MAINT-HHMMSS.         EL535
00792                                                                   EL535
00793      WRITE PENDING-PAY-ADJ.                                       EL535
00794                                                                   EL535
00795      IF ERPYAJ-FILE-STATUS = '22'                                 EL535
00796          MOVE SPACES             TO  WS-DETAIL1                   EL535
00797          MOVE PY-CARRIER         TO  WS-PY-CARRIER                EL535
00798          MOVE PY-GROUPING        TO  WS-PY-GROUPING               EL535
00799          MOVE PY-FIN-RESP        TO  WS-PY-FIN-RESP               EL535
00800          MOVE PY-ACCOUNT         TO  WS-PY-ACCOUNT                EL535
00801          MOVE PY-FILE-SEQ-NO     TO  WS-PY-FILE-SEQ-NO            EL535
00802          MOVE PY-RECORD-TYPE     TO  WS-PY-RECORD-TYPE            EL535
00803          MOVE 'DUPLICATE ERPYAJ RECORD FOUND '                    EL535
00804                                  TO  WS-PY-MESSAGE                EL535
00805          MOVE WS-DETAIL1A        TO  PRT                          EL535
00806          PERFORM WRITE-A-LINE                                     EL535
00807          GO TO 4400-EXIT.                                         EL535
00808                                                                   EL535
00809      IF ERPYAJ-FILE-STATUS NOT = ZERO                             EL535
00810          MOVE 'ERROR OCCURED READ - ERPYAJ'                       EL535
00811                                  TO  WS-ABEND-MESSAGE             EL535
00812          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL535
00813          GO TO ABEND-PGM.                                         EL535
00814                                                                   EL535
00815      ADD +1 TO WS-ERPYAJ-COUNT.                                   EL535
00816                                                                   EL535
00817  4400-EXIT.                                                       EL535
00818      EXIT.                                                        EL535
00819      EJECT                                                        EL535
00820                                                                   EL535
00821  4500-PROCESS-ERREPY SECTION.                                     EL535
00822      MOVE EX-DATA-AREAS         TO PENDING-RETRO-REIN-ADJUSTMENTS.EL535
00823                                                                   EL535
00824      MOVE WS-SAVE-DATE           TO RP-LAST-MAINT-DT.             EL535
00825      MOVE WS-PROCESSOR-ID        TO RP-LAST-MAINT-BY.             EL535
00826      MOVE WS-SAVE-TIME           TO RP-LAST-MAINT-HHMMSS.         EL535
00827                                                                   EL535
00828      WRITE PENDING-RETRO-REIN-ADJUSTMENTS.                        EL535
00829                                                                   EL535
00830      IF ERREPY-FILE-STATUS = '22'                                 EL535
00831          MOVE SPACES             TO  WS-DETAIL1                   EL535
00832          MOVE RP-CARRIER         TO  WS-RP-CARRIER                EL535
00833          MOVE RP-GROUPING        TO  WS-RP-GROUPING               EL535
00834          MOVE RP-STATE           TO  WS-RP-STATE                  EL535
00835          MOVE RP-ACCOUNT         TO  WS-RP-ACCOUNT                EL535
00836          MOVE RP-FILE-SEQ-NO     TO  WS-RP-FILE-SEQ-NO            EL535
00837          MOVE RP-RECORD-TYPE     TO  WS-RP-RECORD-TYPE            EL535
00838          MOVE 'DUPLICATE ERREPY RECORD FOUND '                    EL535
00839                                  TO  WS-RP-MESSAGE                EL535
00840          MOVE WS-DETAIL1A        TO  PRT                          EL535
00841          PERFORM WRITE-A-LINE                                     EL535
00842          GO TO 4500-EXIT.                                         EL535
00843                                                                   EL535
00844      IF ERREPY-FILE-STATUS NOT = ZERO                             EL535
00845          MOVE 'ERROR OCCURED READ - EEREPY'                       EL535
00846                                  TO  WS-ABEND-MESSAGE             EL535
00847          MOVE ERREPY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
00848          GO TO ABEND-PGM.                                         EL535
00849                                                                   EL535
00850      ADD +1 TO WS-ERREPY-COUNT.                                   EL535
00851                                                                   EL535
00852  4500-EXIT.                                                       EL535
00853      EXIT.                                                        EL535
00854      EJECT                                                        EL535
00855                                                                   EL535
00856  4600-PROCESS-ERRQST SECTION.                                     EL535
00857      MOVE EX-DATA-AREAS         TO AR-REQUEST-RECORD.             EL535
00858                                                                   EL535
00859      MOVE WS-PROCESSOR-ID        TO RQ-PROCESSOR-ID.              EL535
00860                                                                   EL535
00861      WRITE AR-REQUEST-RECORD.                                     EL535
00862                                                                   EL535
00863      IF ERRQST-FILE-STATUS = '22'                                 EL535
00864          MOVE SPACES             TO  WS-DETAIL1                   EL535
00865          MOVE RQ-ENTRY-BATCH     TO  WS-RQ-ENTRY-BATCH            EL535
00866          MOVE 'DUPLICATE ERRQST RECORD FOUND '                    EL535
00867                                  TO  WS-RP-MESSAGE                EL535
00868          MOVE WS-DETAIL1A        TO  PRT                          EL535
00869          PERFORM WRITE-A-LINE                                     EL535
00870          GO TO 4600-EXIT.                                         EL535
00871                                                                   EL535
00872      IF ERRQST-FILE-STATUS NOT = ZERO                             EL535
00873          MOVE 'ERROR OCCURED READ - ERRQST'                       EL535
00874                                  TO  WS-ABEND-MESSAGE             EL535
00875          MOVE ERRQST-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
00876          GO TO ABEND-PGM.                                         EL535
00877                                                                   EL535
00878      ADD +1 TO WS-ERRQST-COUNT.                                   EL535
00879                                                                   EL535
00880  4600-EXIT.                                                       EL535
00881      EXIT.                                                        EL535
00882      EJECT                                                        EL535
00883                                                                   EL535
00884  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL535
00885                                                                   EL535
00886      EJECT                                                        EL535
00887                                                                   EL535
00888  GET-DATE SECTION.                                                EL535
00889                                                                   EL535
00890  GDS-010.                                                         EL535
00891      MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.         EL535
00892      MOVE '2'                    TO  DC-OPTION-CODE.              EL535
00893      PERFORM 8500-DATE-CONVERSION.                                EL535
00894      MOVE DC-BIN-DATE-1          TO  WS-SAVE-DATE.                EL535
00895                                                                   EL535
00896      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL535
00897                                                                   EL535
00898      MOVE WS-TIME                TO  WS-SAVE-TIME.                EL535
00899                                                                   EL535
00900  GDS-EXIT.                                                        EL535
00901      EXIT.                                                        EL535
00902                                                                   EL535
00903  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL535
00904                                                                   EL535
00905      EJECT                                                        EL535
00906  WRITE-HEADINGS SECTION.                                          EL535
00907 ***************************************************************** EL535
00908 *                            ELCWHS1.                           * EL535
00909 *                            VMOD=2.001                         * EL535
00910 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL535
00911 *****************************************************************.EL535
00912  WHS-010.                                                         EL535
00913      IF  WS-H2-DATE EQUAL SPACES                                  EL535
00914          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL535
00915          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL535
00916          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL535
00917                                                                   EL535
00918      ADD +1  TO  WS-PAGE.                                         EL535
00919      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL535
00920      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL535
00921      MOVE ZERO                   TO  WS-LINE-COUNT.               EL535
00922                                                                   EL535
00923      MOVE WS-HEADING1            TO  PRT.                         EL535
00924      MOVE '1'                    TO  X.                           EL535
00925      PERFORM WRITE-PRINTER.                                       EL535
00926                                                                   EL535
00927      MOVE WS-HEADING2            TO  PRT.                         EL535
00928      MOVE ' '                    TO  X.                           EL535
00929      PERFORM WRITE-PRINTER.                                       EL535
00930                                                                   EL535
00931      MOVE WS-HEADING3            TO  PRT.                         EL535
00932      MOVE ' '                    TO  X.                           EL535
00933      PERFORM WRITE-PRINTER.                                       EL535
00934                                                                   EL535
00935      MOVE WS-HEADING4            TO  PRT.                         EL535
00936      MOVE ' '                    TO  X.                           EL535
00937      PERFORM WRITE-PRINTER.                                       EL535
00938                                                                   EL535
00939                                                                   EL535
00940       MOVE +10                   TO  WS-LINE-COUNT.               EL535
00941                                                                   EL535
00942  WHS-020. COPY ELCWHS2.                                           EL535
00943                                                                   EL535
00944      EJECT                                                        EL535
00945  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL535
00946                                                                   EL535
00947      MOVE P-CTL TO LCP-ASA                                        EL535
00948      PERFORM LCP-WRITE-POS-PRT                                    EL535
00949          THRU LCP-WRITE-END-PRT.                                  EL535
00950                                                                   EL535
00951  WPS-EXIT.                                                        EL535
00952                                                                   EL535
00953      EXIT.                                                        EL535
00954                                                                   EL535
00955      EJECT                                                        EL535
00956  OPEN-FILES SECTION.                                              EL535
00957                                                                   EL535
00958  OFS-010.                                                         EL535
00959      OPEN INPUT  ELCNTL                                           EL535
00960                  EXTRACT-INTERFACE-FILE                           EL535
00961           I-O    ERPNDB                                           EL535
00962                  ERPNDC                                           EL535
00963                  ERCRTC                                           EL535
00964                  ERPYAJ                                           EL535
00965                  ERREPY                                           EL535
00966                  ERRQST                                           EL535
00967           OUTPUT PRNTR.                                           EL535
00968                                                                   EL535
00969      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL535
00970          NEXT SENTENCE                                            EL535
00971        ELSE                                                       EL535
00972          MOVE 'ELCNTL  '         TO  WS-FEM-FILE-NAME             EL535
00973          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
00974          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
00975          GO TO ABEND-PGM.                                         EL535
00976                                                                   EL535
00977      IF ERPNDB-FILE-STATUS  = '00' OR '97'                        EL535
00978          NEXT SENTENCE                                            EL535
00979        ELSE                                                       EL535
00980          MOVE 'ERPNDB  '         TO  WS-FEM-FILE-NAME             EL535
00981          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
00982          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
00983          GO TO ABEND-PGM.                                         EL535
00984                                                                   EL535
00985      IF ERPNDC-FILE-STATUS  = '00' OR '97'                        EL535
00986          NEXT SENTENCE                                            EL535
00987        ELSE                                                       EL535
00988          MOVE 'ERPNDC  '         TO  WS-FEM-FILE-NAME             EL535
00989          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
00990          MOVE ERPNDC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
00991          GO TO ABEND-PGM.                                         EL535
00992                                                                   EL535
00993      IF ERCRTC-FILE-STATUS  = '00' OR '97'                        EL535
00994          NEXT SENTENCE                                            EL535
00995        ELSE                                                       EL535
00996          MOVE 'ERCRTC  '         TO  WS-FEM-FILE-NAME             EL535
00997          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
00998          MOVE ERCRTC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
00999          GO TO ABEND-PGM.                                         EL535
01000                                                                   EL535
01001      IF ERPYAJ-FILE-STATUS  = '00' OR '97'                        EL535
01002          NEXT SENTENCE                                            EL535
01003        ELSE                                                       EL535
01004          MOVE 'ERPYAJ   '        TO  WS-FEM-FILE-NAME             EL535
01005          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
01006          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL535
01007          GO TO ABEND-PGM.                                         EL535
01008                                                                   EL535
01009      IF ERREPY-FILE-STATUS  = '00' OR '97'                        EL535
01010          NEXT SENTENCE                                            EL535
01011        ELSE                                                       EL535
01012          MOVE 'ERREPY  '         TO  WS-FEM-FILE-NAME             EL535
01013          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
01014          MOVE ERREPY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
01015          GO TO ABEND-PGM.                                         EL535
01016                                                                   EL535
01017      IF ERRQST-FILE-STATUS  = '00' OR '97'                        EL535
01018          NEXT SENTENCE                                            EL535
01019        ELSE                                                       EL535
01020          MOVE 'ERRQST  '         TO  WS-FEM-FILE-NAME             EL535
01021          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
01022          MOVE ERRQST-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
01023          GO TO ABEND-PGM.                                         EL535
01024                                                                   EL535
01025  OFS-EXIT.                                                        EL535
01026                                                                   EL535
01027      EXIT.                                                        EL535
01028      EJECT                                                        EL535
01029  CLOSE-FILES SECTION.                                             EL535
01030                                                                   EL535
01031  CFS-010.                                                         EL535
01032      CLOSE  ELCNTL                                                EL535
01033             ERPNDB                                                EL535
01034             ERPNDC                                                EL535
01035             ERCRTC                                                EL535
01036             ERPYAJ                                                EL535
01037             ERREPY                                                EL535
01038             ERRQST                                                EL535
01039             PRNTR.                                                EL535
01040                                                                   EL535
01041      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL535
01042          MOVE 'ELCNTL  '         TO  WS-FEM-FILE-NAME             EL535
01043          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
01044          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
01045          GO TO ABEND-PGM.                                         EL535
01046                                                                   EL535
01047      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL535
01048          MOVE 'ERPNDB  '         TO  WS-FEM-FILE-NAME             EL535
01049          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
01050          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
01051          GO TO ABEND-PGM.                                         EL535
01052                                                                   EL535
01053      IF ERPNDC-FILE-STATUS NOT = ZERO                             EL535
01054          MOVE 'ERPNDC  '         TO  WS-FEM-FILE-NAME             EL535
01055          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
01056          MOVE ERPNDC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
01057          GO TO ABEND-PGM.                                         EL535
01058                                                                   EL535
01059      IF ERCRTC-FILE-STATUS NOT = ZERO                             EL535
01060          MOVE 'ERCRTC  '         TO  WS-FEM-FILE-NAME             EL535
01061          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
01062          MOVE ERCRTC-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
01063          GO TO ABEND-PGM.                                         EL535
01064                                                                   EL535
01065      IF ERPYAJ-FILE-STATUS NOT = ZERO                             EL535
01066          MOVE 'ERPYAJ   '        TO  WS-FEM-FILE-NAME             EL535
01067          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
01068          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL535
01069          GO TO ABEND-PGM.                                         EL535
01070                                                                   EL535
01071      IF ERREPY-FILE-STATUS NOT = ZERO                             EL535
01072          MOVE 'ERREPY  '         TO  WS-FEM-FILE-NAME             EL535
01073          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
01074          MOVE ERREPY-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
01075          GO TO ABEND-PGM.                                         EL535
01076                                                                   EL535
01077      IF ERRQST-FILE-STATUS NOT = ZERO                             EL535
01078          MOVE 'ERRQST  '         TO  WS-FEM-FILE-NAME             EL535
01079          MOVE WS-FILE-ERROR-MESSAGE TO  WS-ABEND-MESSAGE          EL535
01080          MOVE ERRQST-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL535
01081          GO TO ABEND-PGM.                                         EL535
01082                                                                   EL535
01083  CFS-EXIT.                                                        EL535
01084      EXIT.                                                        EL535
01085                                                                   EL535
01086                                                                   EL535
01087  ABEND-PGM SECTION. COPY ELCABEND SUPPRESS.                       EL535
01088 /                                                                 EL535
01089  LCP-WRITE-POS-PRT SECTION.                                       EL535
01090      IF LCP-ASA = '+'                                             EL535
01091          WRITE PRT AFTER 0 LINE                                   EL535
01092      ELSE                                                         EL535
01093      IF LCP-ASA = ' '                                             EL535
01094          WRITE PRT AFTER ADVANCING 1 LINE                         EL535
01095      ELSE                                                         EL535
01096      IF LCP-ASA = '0'                                             EL535
01097          WRITE PRT AFTER ADVANCING 2 LINE                         EL535
01098      ELSE                                                         EL535
01099      IF LCP-ASA = '-'                                             EL535
01100          WRITE PRT AFTER ADVANCING 3 LINE                         EL535
01101      ELSE                                                         EL535
01102      IF LCP-ASA = '1'                                             EL535
01103          WRITE PRT AFTER ADVANCING PAGE                           EL535
01104      ELSE                                                         EL535
01105      IF LCP-ASA = '2'                                             EL535
01106          WRITE PRT AFTER ADVANCING LCP-CH2                        EL535
01107      ELSE                                                         EL535
01108      IF LCP-ASA = '3'                                             EL535
01109          WRITE PRT AFTER ADVANCING LCP-CH3                        EL535
01110      ELSE                                                         EL535
01111      IF LCP-ASA = '4'                                             EL535
01112          WRITE PRT AFTER ADVANCING LCP-CH4                        EL535
01113      ELSE                                                         EL535
01114      IF LCP-ASA = '5'                                             EL535
01115          WRITE PRT AFTER ADVANCING LCP-CH5                        EL535
01116      ELSE                                                         EL535
01117      IF LCP-ASA = '6'                                             EL535
01118          WRITE PRT AFTER ADVANCING LCP-CH6                        EL535
01119      ELSE                                                         EL535
01120      IF LCP-ASA = '7'                                             EL535
01121          WRITE PRT AFTER ADVANCING LCP-CH7                        EL535
01122      ELSE                                                         EL535
01123      IF LCP-ASA = '8'                                             EL535
01124          WRITE PRT AFTER ADVANCING LCP-CH8                        EL535
01125      ELSE                                                         EL535
01126      IF LCP-ASA = '9'                                             EL535
01127          WRITE PRT AFTER ADVANCING LCP-CH9                        EL535
01128      ELSE                                                         EL535
01129      IF LCP-ASA = 'A'                                             EL535
01130          WRITE PRT AFTER ADVANCING LCP-CH10                       EL535
01131      ELSE                                                         EL535
01132      IF LCP-ASA = 'B'                                             EL535
01133          WRITE PRT AFTER ADVANCING LCP-CH11                       EL535
01134      ELSE                                                         EL535
01135      IF LCP-ASA = 'C'                                             EL535
01136          WRITE PRT AFTER ADVANCING LCP-CH12                       EL535
01137      ELSE                                                         EL535
01138      IF LCP-ASA = 'V'                                             EL535
01139          WRITE PRT AFTER ADVANCING LCP-P01                        EL535
01140      ELSE                                                         EL535
01141      IF LCP-ASA = 'W'                                             EL535
01142          WRITE PRT AFTER ADVANCING LCP-P02                        EL535
01143      ELSE                                                         EL535
01144      DISPLAY 'ASA CODE ERROR'.                                    EL535
01145  LCP-WRITE-END-PRT.                                               EL535
01146      EXIT.                                                        EL535
01147                                                                   EL535
