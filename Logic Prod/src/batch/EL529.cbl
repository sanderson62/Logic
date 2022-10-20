00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL529
00003  PROGRAM-ID.                 EL529 .                                 LV002
00004 *              PROGRAM CONVERTED BY                               EL529
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL529
00006 *              CONVERSION DATE 04/10/96 10:11:56.                 EL529
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL529
00008 *                            VMOD=2.007.                          EL529
00009 *AUTHOR.     LOGIC, INC.                                          EL529
00010 *            DALLAS, TEXAS.                                       EL529
00011                                                                   EL529
00012 *DATE-COMPILED.                                                   EL529
00013                                                                   EL529
00014 *SECURITY.   *****************************************************EL529
00015 *            *                                                   *EL529
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL529
00017 *            *                                                   *EL529
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL529
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL529
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL529
00021 *            *                                                   *EL529
00022 *            *****************************************************EL529
00023                                                                   EL529
00024 *REMARKS.                                                         EL529
00025 *        THIS PROGRAM PRINTS A REPORT SHOWING ALL BATCHES WHICH   EL529
00026 *    CONTAIN ERRORS WITHIN THE PENDING BUSINESS FILE.  CONTROL    EL529
00027 *    BREAKS ARE TAKEN AT THE BATCH AND ACCOUNT LEVEL.             EL529
00028                                                                   EL529
00029                                                                   EL529
00030      EJECT                                                        EL529
00031  ENVIRONMENT DIVISION.                                            EL529
00032  CONFIGURATION SECTION.                                           EL529
00033  SPECIAL-NAMES.                                                   EL529
00034      C02 IS LCP-CH2                                               EL529
00035      C03 IS LCP-CH3                                               EL529
00036      C04 IS LCP-CH4                                               EL529
00037      C05 IS LCP-CH5                                               EL529
00038      C06 IS LCP-CH6                                               EL529
00039      C07 IS LCP-CH7                                               EL529
00040      C08 IS LCP-CH8                                               EL529
00041      C09 IS LCP-CH9                                               EL529
00042      C10 IS LCP-CH10                                              EL529
00043      C11 IS LCP-CH11                                              EL529
00044      C12 IS LCP-CH12                                              EL529
00045      S01 IS LCP-P01                                               EL529
00046      S02 IS LCP-P02.                                              EL529
00047                                                                   EL529
00048  INPUT-OUTPUT SECTION.                                            EL529
00049                                                                   EL529
00050  FILE-CONTROL.                                                    EL529
00051                                                                   EL529
00052      SELECT SORT-FILE        ASSIGN TO SYS001-FBA1-S-SORTWK1.     EL529
00053                                                                   EL529
00054      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL529
00055                                                                   EL529
00056      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL529
00057                                                                   EL529
00058      SELECT ERPNDB           ASSIGN TO SYS022-FBA1-ERPNDB         EL529
00059                              ORGANIZATION IS INDEXED              EL529
00060                              ACCESS IS DYNAMIC                    EL529
00061                              RECORD KEY IS PB-CONTROL-PRIMARY     EL529
00062                              FILE STATUS IS ERPNDB-FILE-STATUS.   EL529
00063                                                                   EL529
00064      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL529
00065                                                                   EL529
00066      SELECT ELCNTL           ASSIGN TO SYS022-FBA1-ELCNTL         EL529
00067                              ORGANIZATION IS INDEXED              EL529
00068                              ACCESS IS DYNAMIC                    EL529
00069                              RECORD KEY IS CF-CONTROL-PRIMARY     EL529
00070                              FILE STATUS IS ELCNTL-FILE-STATUS.   EL529
00071                                                                   EL529
00072      SELECT ERACCT           ASSIGN TO SYS021-FBA1-ERACCT2        EL529
00073                              ORGANIZATION IS INDEXED              EL529
00074                              ACCESS IS DYNAMIC                    EL529
00075                              RECORD KEY IS AM-CONTROL-BY-VAR-GRP  EL529
00076                              FILE STATUS IS ERACCT-FILE-STATUS.   EL529
00077                                                                   EL529
00078      SELECT ELPGMS           ASSIGN TO SYS023-FBA1-ELPGMS         EL529
00079                              ORGANIZATION IS INDEXED              EL529
00080                              ACCESS IS DYNAMIC                    EL529
00081                              RECORD KEY IS PS-CONTROL-PRIMARY     EL529
00082                              FILE STATUS IS ELPGMS-FILE-STATUS.   EL529
00083                                                                   EL529
00084      SELECT ELREPT           ASSIGN TO SYS024-FBA1-ELREPT         EL529
00085                              ORGANIZATION IS INDEXED              EL529
00086                              ACCESS IS DYNAMIC                    EL529
00087                              RECORD KEY IS RF-CONTROL-PRIMARY     EL529
00088                              FILE STATUS IS ELREPT-FILE-STATUS.   EL529
00089                                                                   EL529
00090      EJECT                                                        EL529
00091  DATA DIVISION.                                                   EL529
00092                                                                   EL529
00093  FILE SECTION.                                                    EL529
00094                                                                   EL529
00095                                                                   EL529
00096      EJECT                                                        EL529
00097  FD  DISK-DATE                   COPY ELCDTEFD.                   EL529
00098                                                                   EL529
00099  FD  PRNTR                       COPY ELCPRTFD.                   EL529
00100                                                                   EL529
00101  FD  FICH                        COPY ELCFCHFD.                   EL529
00102                                                                   EL529
00103  FD  ERPNDB.                                                      EL529
00104                                                                   EL529
00105                                  COPY ERCPNDB.                    EL529
00106                                                                   EL529
00107  FD  ELCNTL.                                                      EL529
00108                                                                   EL529
00109                                  COPY ELCCNTL.                    EL529
00110                                                                   EL529
00111      EJECT                                                        EL529
00112  FD  ERACCT.                                                      EL529
00113                                                                   EL529
00114                                  COPY ERCACCT.                    EL529
00115                                                                   EL529
00116      EJECT                                                        EL529
00117  FD  ELPGMS.                                                      EL529
00118                                                                   EL529
00119                                  COPY ELCPGMS.                    EL529
00120                                                                   EL529
00121      EJECT                                                        EL529
00122  FD  ELREPT                      COPY ELCRPTFD.                   EL529
00123                                                                   EL529
00124                                  COPY ELCREPT.                    EL529
00125                                                                   EL529
00126  SD  SORT-FILE.                                                   EL529
00127                                                                   EL529
00128                                  COPY ERCEXTR.                    EL529
00129      EJECT                                                        EL529
00130  WORKING-STORAGE SECTION.                                         EL529
00131  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL529
00132  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   EL529
00133  01  LCP-TIME-OF-DAY-68            PIC 9(6).                      EL529
00134  01  LCP-TIME-OF-DAY-74.                                          EL529
00135      05  LCP-TIME-74               PIC 9(6).                      EL529
00136      05  FILLER                    PIC 9(2).                      EL529
00137  01  LCP-CURRENT-DATE-68.                                         EL529
00138      05  LCP-MONTH                 PIC X(2).                      EL529
00139      05  FILLER                    PIC X VALUE '/'.               EL529
00140      05  LCP-DAY1                  PIC X(2).                      EL529
00141      05  FILLER                    PIC X VALUE '/'.               EL529
00142      05  LCP-YEAR                  PIC X(2).                      EL529
00143  01  LCP-DATE-NEW-74.                                             EL529
00144      05  LCP-YEAR                  PIC X(2).                      EL529
00145      05  LCP-MONTH                 PIC X(2).                      EL529
00146      05  LCP-DAY1                  PIC X(2).                      EL529
00147  77  LCP-ASA                       PIC X.                         EL529
00148                                                                   EL529
00149  77  FILLER  PIC X(32)   VALUE '********************************'.EL529
00150  77  FILLER  PIC X(32)   VALUE '*     EL529  WORKING STORAGE   *'.EL529
00151  77  FILLER  PIC X(32)   VALUE '********** V/M 2.007 ***********'.EL529
00152                                                                   EL529
00153  01  WS-ELREPT-SWITCHES.                                          EL529
00154      05  WS-LINE-NUMBER              PIC S9(8)   VALUE +0  COMP.  EL529
00155      05  WS-START-SW                 PIC S9      VALUE +0.        EL529
00156      05  WS-ELREPT-STATUS-SW         PIC S9      VALUE +0.        EL529
00157          88  ELREPT-NOT-OPEN                     VALUE +0.        EL529
00158          88  ELREPT-OPEN                         VALUE +1.        EL529
00159      05  WS-FICHE-STATUS-SW          PIC S9      VALUE +0.        EL529
00160          88  FICHE-NOT-OPEN                      VALUE +0.        EL529
00161          88  FICHE-OPEN                          VALUE +1.        EL529
00162      05  WS-PRNTR-STATUS-SW          PIC S9      VALUE +0.        EL529
00163          88  PRNTR-NOT-OPEN                      VALUE +0.        EL529
00164          88  PRNTR-OPEN                          VALUE +1.        EL529
00165      05  WS-COMPANY-ID               PIC X(3).                    EL529
00166      05  WS-COMPANY-CD               PIC X.                       EL529
00167      05  WS-ALPHA-DATE               PIC X(20)   VALUE SPACES.    EL529
00168      05  WS-REPORT-ID                PIC X(5)    VALUE 'EL529'.   EL529
00169      05  WS-PROGRAM-OPTIONS.                                      EL529
00170          10  WS-FREQUENCY-OPTION     PIC X(4).                    EL529
00171          10  WS-PRINT-OPTION         PIC X.                       EL529
00172          10  WS-FORMAT-OPTION        PIC X.                       EL529
00173          10  WS-PROCESS-OPTION       PIC X.                       EL529
00174          10  WS-TOTAL-OPTION         PIC X.                       EL529
00175                                                                   EL529
00176  01  FILLER                          COMP-3.                      EL529
00177      05  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       EL529
00178      05  WS-LINE-COUNT-MAX           PIC S9(3)   VALUE +60.       EL529
00179      05  WS-PAGE                     PIC S9(5)   VALUE +0.        EL529
00180      05  WS-REPORT-SW                PIC S9      VALUE +0.        EL529
00181      05  WS-HEADING-SW               PIC S9      VALUE +0.        EL529
00182      05  WS-PRINT-SW                 PIC S9      VALUE +0.        EL529
00183      05  WS-RECORD-COUNT             PIC S9(9)   VALUE +0.        EL529
00184      05  WS-RETURN-CODE              PIC S9(3)   VALUE +0.        EL529
00185      05  WS-ZERO                     PIC S9      VALUE +0.        EL529
00186      05  WS-NO-RECORDS-RELEASED      PIC S9(5)   VALUE +0.        EL529
00187      05  WS-EOF-SW                   PIC S9      VALUE +0.        EL529
00188          88  EXTRACT-EOF                         VALUE +1.        EL529
00189      05  WS-NO-BATCHES               PIC S999    VALUE +0.        EL529
00190          88  NEW-ACCOUNT                         VALUE +0.        EL529
00191          88  ONLY-ONE-BATCH                      VALUE +1.        EL529
00192      05  WS-INCURRED-AGE             PIC S9(3)   VALUE +0.        EL529
00193      05  WS-YEAR                     REDEFINES                    EL529
00194          WS-INCURRED-AGE             PIC S9(3).                   EL529
00195                                                                   EL529
00196      05  WS-AMOUNT                   PIC S9(9)V99 VALUE +0.       EL529
00197                                                                   EL529
00198      EJECT                                                        EL529
00199  01  FILLER                          COMP SYNC.                   EL529
00200      05  PGM-SUB                     PIC S9(4)   VALUE +529.      EL529
00201      05  WS-INDEX                    PIC S9(4)   VALUE ZERO.      EL529
00202      05  WS-LENGTH                   REDEFINES                    EL529
00203          WS-INDEX                    PIC S9(4).                   EL529
00204                                                                   EL529
00205  01  FILLER.                                                      EL529
00206      05  WS-DISPLAY-TIME             PIC 99B99B99.                EL529
00207      05  ABEND-CODE                  PIC X(4).                    EL529
00208      05  ABEND-OPTION                PIC X.                       EL529
00209      05  OLC-REPORT-NAME             PIC X(5) VALUE 'EL529'.      EL529
00210      05  X                           PIC X       VALUE SPACE.     EL529
00211                                                                   EL529
00212      05  WS-SAVE-PRINT-RECORD        PIC X(133)  VALUE SPACES.    EL529
00213      05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL529
00214      05  WS-LAST-CARRIER             PIC X       VALUE SPACE.     EL529
00215                                                                   EL529
00216      05  WS-SAVE-KEY.                                             EL529
00217          10  WS-SAVE-CO-CD           PIC X.                       EL529
00218          10  WS-SAVE-ACCOUNT-KEY.                                 EL529
00219              15  WS-SAVE-CARRIER     PIC X.                       EL529
00220              15  WS-SAVE-GROUPING    PIC X(6).                    EL529
00221              15  WS-SAVE-STATE       PIC XX.                      EL529
00222              15  WS-SAVE-ACCOUNT     PIC X(10).                   EL529
00223          10  FILLER                  PIC X(16).                   EL529
00224                                                                   EL529
00225      05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL529
00226      05  ERPNDB-FILE-STATUS          PIC XX      VALUE ZERO.      EL529
00227      05  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      EL529
00228      05  ERACCT-FILE-STATUS          PIC XX      VALUE ZERO.      EL529
00229      05  ELREPT-FILE-STATUS          PIC XX      VALUE ZERO.      EL529
00230      05  ELPGMS-FILE-STATUS          PIC XX      VALUE ZERO.      EL529
00231                                                                   EL529
00232      05  WS-FILE-ERROR-MESSAGE.                                   EL529
00233          10  FILLER                  PIC X(24)   VALUE            EL529
00234              'ERROR OCCURED OPENING - '.                          EL529
00235          10  WS-FEM-FILE-NAME        PIC X(8).                    EL529
00236                                                                   EL529
00237      05  WS-COMPANY-NAME.                                         EL529
00238          10  WS-CN-CHAR              PIC X                        EL529
00239              OCCURS 30 TIMES         INDEXED BY CN1.              EL529
00240                                                                   EL529
00241      05  WS-COMPANY-NAME2.                                        EL529
00242          10  WS-CN2-CHAR             PIC X                        EL529
00243              OCCURS 30 TIMES         INDEXED BY CN2.              EL529
00244      05  WS-INITIALS.                                             EL529
00245          10  WS-INITIAL1             PIC X.                       EL529
00246          10  WS-INITIAL2             PIC X.                       EL529
00247                                                                   EL529
00248      05  WS-BIN-DATE-WORK-X.                                      EL529
00249          10  WS-BIN-DATE-WORK        PIC S9(4)                    EL529
00250                                      COMP.                        EL529
00251                                                                   EL529
00252      05  WS-DATE-WORK.                                            EL529
00253          10  WS-DW-MONTH             PIC 99.                      EL529
00254          10  FILLER                  PIC X.                       EL529
00255          10  WS-DW-DAY               PIC 99.                      EL529
00256          10  FILLER                  PIC X.                       EL529
00257          10  WS-DW-YEAR              PIC 99.                      EL529
00258                                                                   EL529
00259      05  WS-DATE-PAID.                                            EL529
00260          10  WS-MONTH-PAID           PIC 99.                      EL529
00261          10  FILLER                  PIC X.                       EL529
00262          10  WS-DAY-PAID             PIC 99.                      EL529
00263          10  FILLER                  PIC X.                       EL529
00264          10  WS-YEAR-PAID            PIC 99.                      EL529
00265                                                                   EL529
00266      05  WS-BATCH-TOTALS         PIC X(94)           VALUE ZEROS. EL529
00267                                                                   EL529
00268      05  WS-ISS-CAN-TOTALS       REDEFINES                        EL529
00269          WS-BATCH-TOTALS.                                         EL529
00270          10  WS-ISS-TOTAL-CNT    PIC S9(5).                       EL529
00271          10  WS-ISS-AVAIL-CNT    PIC S9(5).                       EL529
00272          10  WS-ISS-FATAL-CNT    PIC S9(5).                       EL529
00273          10  WS-ISS-FORCE-CNT    PIC S9(5).                       EL529
00274          10  WS-ISS-HOLD-CNT     PIC S9(5).                       EL529
00275          10  WS-ISS-GOOD-PREM    PIC S9(9)V99.                    EL529
00276          10  WS-ISS-BAD-PREM     PIC S9(9)V99.                    EL529
00277          10  WS-CAN-TOTAL-CNT    PIC S9(5).                       EL529
00278          10  WS-CAN-AVAIL-CNT    PIC S9(5).                       EL529
00279          10  WS-CAN-FATAL-CNT    PIC S9(5).                       EL529
00280          10  WS-CAN-FORCE-CNT    PIC S9(5).                       EL529
00281          10  WS-CAN-HOLD-CNT     PIC S9(5).                       EL529
00282          10  WS-CAN-GOOD-PREM    PIC S9(9)V99.                    EL529
00283          10  WS-CAN-BAD-PREM     PIC S9(9)V99.                    EL529
00284                                                                   EL529
00285      05  WS-BATCH-ISS-CAN-TOTALS.                                 EL529
00286          10  WS-TOT-TOTAL-CNT    PIC S9(5).                       EL529
00287          10  WS-TOT-AVAIL-CNT    PIC S9(5).                       EL529
00288          10  WS-TOT-FATAL-CNT    PIC S9(5).                       EL529
00289          10  WS-TOT-FORCE-CNT    PIC S9(5).                       EL529
00290          10  WS-TOT-HOLD-CNT     PIC S9(5).                       EL529
00291          10  WS-TOT-GOOD-PREM    PIC S9(9)V99.                    EL529
00292          10  WS-TOT-BAD-PREM     PIC S9(9)V99.                    EL529
00293                                                                   EL529
00294      05  WS-ACCOUNT-TOTALS.                                       EL529
00295          10  WS-ACC-ISS-TOTAL-CNT    PIC S9(5).                   EL529
00296          10  WS-ACC-ISS-AVAIL-CNT    PIC S9(5).                   EL529
00297          10  WS-ACC-ISS-FATAL-CNT    PIC S9(5).                   EL529
00298          10  WS-ACC-ISS-FORCE-CNT    PIC S9(5).                   EL529
00299          10  WS-ACC-ISS-HOLD-CNT     PIC S9(5).                   EL529
00300          10  WS-ACC-ISS-GOOD-PREM    PIC S9(9)V99.                EL529
00301          10  WS-ACC-ISS-BAD-PREM     PIC S9(9)V99.                EL529
00302          10  WS-ACC-CAN-TOTAL-CNT    PIC S9(5).                   EL529
00303          10  WS-ACC-CAN-AVAIL-CNT    PIC S9(5).                   EL529
00304          10  WS-ACC-CAN-FATAL-CNT    PIC S9(5).                   EL529
00305          10  WS-ACC-CAN-FORCE-CNT    PIC S9(5).                   EL529
00306          10  WS-ACC-CAN-HOLD-CNT     PIC S9(5).                   EL529
00307          10  WS-ACC-CAN-GOOD-PREM    PIC S9(9)V99.                EL529
00308          10  WS-ACC-CAN-BAD-PREM     PIC S9(9)V99.                EL529
00309          10  WS-ACC-TOT-TOTAL-CNT    PIC S9(5).                   EL529
00310          10  WS-ACC-TOT-AVAIL-CNT    PIC S9(5).                   EL529
00311          10  WS-ACC-TOT-FATAL-CNT    PIC S9(5).                   EL529
00312          10  WS-ACC-TOT-FORCE-CNT    PIC S9(5).                   EL529
00313          10  WS-ACC-TOT-HOLD-CNT     PIC S9(5).                   EL529
00314          10  WS-ACC-TOT-GOOD-PREM    PIC S9(9)V99.                EL529
00315          10  WS-ACC-TOT-BAD-PREM     PIC S9(9)V99.                EL529
00316                                                                   EL529
00317      05  WS-FINAL-TOTALS.                                         EL529
00318          10  WS-FIN-ISS-TOTAL-CNT    PIC S9(6).                   EL529
00319          10  WS-FIN-ISS-AVAIL-CNT    PIC S9(5).                   EL529
00320          10  WS-FIN-ISS-FATAL-CNT    PIC S9(6).                   EL529
00321          10  WS-FIN-ISS-FORCE-CNT    PIC S9(5).                   EL529
00322          10  WS-FIN-ISS-HOLD-CNT     PIC S9(5).                   EL529
00323          10  WS-FIN-ISS-GOOD-PREM    PIC S9(9)V99.                EL529
00324          10  WS-FIN-ISS-BAD-PREM     PIC S9(9)V99.                EL529
00325          10  WS-FIN-CAN-TOTAL-CNT    PIC S9(6).                   EL529
00326          10  WS-FIN-CAN-AVAIL-CNT    PIC S9(5).                   EL529
00327          10  WS-FIN-CAN-FATAL-CNT    PIC S9(6).                   EL529
00328          10  WS-FIN-CAN-FORCE-CNT    PIC S9(5).                   EL529
00329          10  WS-FIN-CAN-HOLD-CNT     PIC S9(5).                   EL529
00330          10  WS-FIN-CAN-GOOD-PREM    PIC S9(9)V99.                EL529
00331          10  WS-FIN-CAN-BAD-PREM     PIC S9(9)V99.                EL529
00332          10  WS-FIN-TOT-TOTAL-CNT    PIC S9(6).                   EL529
00333          10  WS-FIN-TOT-AVAIL-CNT    PIC S9(5).                   EL529
00334          10  WS-FIN-TOT-FATAL-CNT    PIC S9(6).                   EL529
00335          10  WS-FIN-TOT-FORCE-CNT    PIC S9(5).                   EL529
00336          10  WS-FIN-TOT-HOLD-CNT     PIC S9(5).                   EL529
00337          10  WS-FIN-TOT-GOOD-PREM    PIC S9(9)V99.                EL529
00338          10  WS-FIN-TOT-BAD-PREM     PIC S9(9)V99.                EL529
00339                                                                   EL529
00340  01  WS-HEADING1.                                                 EL529
00341      05  FILLER                      PIC X(20)   VALUE '1'.       EL529
00342      05  WS-H1-TITLE                 PIC X(51)   VALUE            EL529
00343          'PENDING BUSINESS SUMMARY - BATCH ERRORS  '.             EL529
00344      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL529'.      EL529
00345                                                                   EL529
00346  01  WS-HEADING2.                                                 EL529
00347      05  FILLER                      PIC X(25) VALUE SPACES.      EL529
00348      05  WS-H2-CLIENT-NAME           PIC X(30) VALUE SPACES.      EL529
00349      05  FILLER                      PIC X(16) VALUE SPACES.      EL529
00350      05  WS-H2-DATE                  PIC X(08) VALUE SPACES.      EL529
00351                                                                   EL529
00352  01  WS-HEADING3.                                                 EL529
00353      05  FILLER                      PIC X(31) VALUE SPACES.      EL529
00354      05  WS-H3-DATE                  PIC X(18).                   EL529
00355      05  FILLER                      PIC X(10) VALUE SPACES.      EL529
00356      05  FILLER                      PIC X(05) VALUE 'PAGE'.      EL529
00357      05  WS-H3-PAGE                  PIC ZZZ9.                    EL529
00358                                                                   EL529
00359  01  WS-HEADING4.                                                 EL529
00360      05  FILLER                      PIC X(80) VALUE              EL529
00361          '0CARRIER  GROUPING  ST   ACCOUNT       ACCOUNT NAME'.   EL529
00362                                                                   EL529
00363  01  WS-HEADING5.                                                 EL529
00364      05  FILLER                      PIC X(4)  VALUE  SPACES.     EL529
00365      05  H5-CARRIER                  PIC X     VALUE  SPACES.     EL529
00366      05  FILLER                      PIC X(6)  VALUE  SPACES.     EL529
00367      05  H5-GROUPING                 PIC X(6)  VALUE  SPACES.     EL529
00368      05  FILLER                      PIC X(3)  VALUE  SPACES.     EL529
00369      05  H5-STATE                    PIC X(2)  VALUE  SPACES.     EL529
00370      05  FILLER                      PIC X(2)  VALUE  SPACES.     EL529
00371      05  H5-ACCOUNT                  PIC X(10) VALUE  SPACES.     EL529
00372      05  FILLER                      PIC X(2)  VALUE  SPACES.     EL529
00373      05  H5-ACCOUNT-NAME             PIC X(30) VALUE  SPACES.     EL529
00374      05  FILLER                      PIC X(14) VALUE  SPACES.     EL529
00375                                                                   EL529
00376  01  WS-HEADING6.                                                 EL529
00377      05  FILLER                      PIC X(40)  VALUE             EL529
00378          '0BATCH            TOTAL   FATAL FORCBL O'.              EL529
00379      05  FILLER                      PIC X(40) VALUE              EL529
00380          'N HOLD AVAIL       GOOD        BAD      '.              EL529
00381                                                                   EL529
00382  01  WS-HEADING7.                                                 EL529
00383      05  FILLER                      PIC X(40) VALUE              EL529
00384          ' NUMBER           COUNT   COUNT ERRORS  '.              EL529
00385      05  FILLER                      PIC X(40)  VALUE             EL529
00386          'RCDS             PREMIUM     PREMIUM    '.              EL529
00387                                                                   EL529
00388  01  WS-HEADING8.                                                 EL529
00389      05  FILLER                      PIC X(80) VALUE              EL529
00390          '0**ACCOUNT TOTALS**'.                                   EL529
00391                                                                   EL529
00392  01  WS-HEADING9.                                                 EL529
00393      05  FILLER                      PIC X(80) VALUE              EL529
00394          '0***FINAL TOTALS***'.                                   EL529
00395                                                                   EL529
00396  01  WS-SAVE-HEADING5                PIC X(46)  VALUE SPACES.     EL529
00397                                                                   EL529
00398      EJECT                                                        EL529
00399                                                                   EL529
00400  01  WS-DETAIL1.                                                  EL529
00401      05  FILLER                      PIC X.                       EL529
00402      05  WS-D1-BATCH-NO              PIC X(6).                    EL529
00403      05  FILLER                      PIC X.                       EL529
00404      05  WS-D1-DESC                  PIC X(7).                    EL529
00405      05  FILLER                      PIC X.                       EL529
00406      05  WS-D1-TOTAL-COUNT           PIC ZZZ,ZZ9-.                EL529
00407      05  WS-D1-FATAL-COUNT           PIC ZZZ,ZZ9-.                EL529
00408      05  WS-D1-FORCE-COUNT           PIC ZZ,ZZ9-.                 EL529
00409      05  WS-D1-HOLD-COUNT            PIC ZZ,ZZ9-.                 EL529
00410      05  WS-D1-AVAIL-COUNT           PIC ZZ,ZZ9-.                 EL529
00411      05  WS-D1-GOOD-PREMIUM          PIC Z,ZZZ,ZZZ.99-.           EL529
00412      05  WS-D1-BAD-PREMIUM           PIC Z,ZZZ,ZZZ.99-.           EL529
00413                                                                   EL529
00414      EJECT                                                        EL529
00415                COPY ELCDATE.                                         CL**2
00416                                                                   EL529
00417                COPY ELCDTECX.                                     EL529
00418                                                                   EL529
00419                COPY ELCDTEVR.                                     EL529
00420 *                                                                 EL529
00421 *                                                                 EL529
00422 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      EL529
00430      EJECT                                                        EL529
00431  PROCEDURE DIVISION.                                              EL529
00432                                                                   EL529
00433  0000-DATE-CARD-READ SECTION. COPY ELCDTERX.                      EL529
00434                                                                   EL529
00435  0000-MAIN-LOGIC SECTION.                                         EL529
00436      PERFORM OPEN-FILES.                                          EL529
00437                                                                   EL529
00438      SORT SORT-FILE                                               EL529
00439          ON ASCENDING KEY EX-SORT-KEY-AREAS                       EL529
00440          INPUT  PROCEDURE IS 1000-SORT-INPUT-PROCEDURE            EL529
00441          OUTPUT PROCEDURE IS 1900-PROCESS-EXTRACT.                EL529
00442                                                                   EL529
00443      IF SORT-RETURN GREATER THAN ZERO                             EL529
00444          MOVE 'SORT FAILED'      TO WS-ABEND-MESSAGE              EL529
00445          MOVE SORT-RETURN        TO WS-RETURN-CODE                EL529
00446          GO TO ABEND-PGM.                                         EL529
00447                                                                   EL529
00448      PERFORM CLOSE-FILES.                                         EL529
00449                                                                   EL529
00450      GOBACK.                                                      EL529
00451                                                                   EL529
00452  0000-EXIT.                                                       EL529
00453      EXIT.                                                        EL529
00454      EJECT                                                        EL529
00455  1000-SORT-INPUT-PROCEDURE SECTION.                               EL529
00456      ACCEPT WS-ACCEPT-DATE FROM DATE.                             EL529
00457      MOVE WS-AD-YY               TO WS-CD-YY.                     EL529
00458      MOVE WS-AD-MM               TO WS-CD-MM.                     EL529
00459      MOVE WS-AD-DD               TO WS-CD-DD.                     EL529
00460      MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT.          EL529
00461      MOVE '2'                    TO DC-OPTION-CODE.               EL529
00462      PERFORM 8500-DATE-CONVERSION.                                EL529
00463      MOVE DC-GREG-DATE-1-ALPHA   TO WS-ALPHA-DATE.                EL529
00464                                                                   EL529
00465  1010-SIP.                                                        EL529
00466      MOVE LOW-VALUES             TO CF-CONTROL-PRIMARY.           EL529
00467      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL529
00468      MOVE '1'                    TO CF-RECORD-TYPE.               EL529
00469      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL529
00470      MOVE +0                     TO CF-SEQUENCE-NO.               EL529
00471                                                                   EL529
00472      READ  ELCNTL.                                                EL529
00473                                                                   EL529
00474      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL529
00475          MOVE 'ERROR OCCURED CLOSE - ELCNTL'                      EL529
00476                                  TO  WS-ABEND-MESSAGE             EL529
00477          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
00478          GO TO ABEND-PGM.                                         EL529
00479                                                                   EL529
00480                                                                   EL529
00481      IF CO-HAS-CLAS-IC-CREDIT                                     EL529
00482          NEXT SENTENCE                                            EL529
00483      ELSE                                                         EL529
00484          DISPLAY 'NOT A CREDIT USER' ' ' CF-COMPANY-ID            EL529
00485          GO TO 1099-EXIT.                                         EL529
00486                                                                   EL529
00487      MOVE CF-CL-MAIL-TO-NAME     TO  WS-COMPANY-NAME              EL529
00488                                      WS-H2-CLIENT-NAME.           EL529
00489      MOVE CF-COMPANY-ID          TO  WS-COMPANY-ID.               EL529
00490      MOVE CF-COMPANY-CD          TO  WS-COMPANY-CD.               EL529
00491      MOVE LOW-VALUES             TO  WS-LAST-CARRIER.             EL529
00492                                                                   EL529
00493      ACCEPT WS-TIME-OF-DAY       FROM TIME.                       EL529
00494      MOVE WS-TIME                TO  WS-DISPLAY-TIME.             EL529
00495      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'.            EL529
00496      DISPLAY 'BEGIN PROCESSING ' WS-H2-CLIENT-NAME ' AT '         EL529
00497              WS-DISPLAY-TIME UPON CONSOLE.                        EL529
00498                                                                   EL529
00499      MOVE SPACES                 TO  WS-COMPANY-NAME2.            EL529
00500      SET CN1 TO +30.                                              EL529
00501                                                                   EL529
00502  1020-SIP.                                                        EL529
00503      IF WS-CN-CHAR (CN1) = SPACES                                 EL529
00504          IF CN1 GREATER THAN +1                                   EL529
00505              SET CN1 DOWN BY +1                                   EL529
00506              GO TO 1020-SIP                                       EL529
00507            ELSE                                                   EL529
00508              GO TO 1040-SIP.                                      EL529
00509                                                                   EL529
00510      SET WS-LENGTH TO CN1.                                        EL529
00511                                                                   EL529
00512      SUBTRACT WS-LENGTH FROM +30 GIVING WS-LENGTH.                EL529
00513      DIVIDE +2 INTO WS-LENGTH ROUNDED.                            EL529
00514                                                                   EL529
00515      IF WS-LENGTH NOT GREATER THAN ZERO                           EL529
00516          GO TO 1040-SIP.                                          EL529
00517                                                                   EL529
00518      SET CN2 TO CN1.                                              EL529
00519      SET CN2 UP BY WS-LENGTH.                                     EL529
00520                                                                   EL529
00521  1030-SIP.                                                        EL529
00522      MOVE WS-CN-CHAR (CN1) TO WS-CN2-CHAR (CN2).                  EL529
00523                                                                   EL529
00524      IF CN1 GREATER THAN +1                                       EL529
00525          SET CN1                                                  EL529
00526              CN2 DOWN BY +1                                       EL529
00527          GO TO 1030-SIP.                                          EL529
00528                                                                   EL529
00529      MOVE WS-COMPANY-NAME2       TO  WS-COMPANY-NAME.             EL529
00530                                                                   EL529
00531      EJECT                                                        EL529
00532                                                                   EL529
00533  1040-SIP.                                                        EL529
00534 *    NOTE ******************************************************* EL529
00535 *         *      POSITION THE PENDING BUSINESS FILE AT THE BE-  * EL529
00536 *         *  GINNING OF THE COMPANY TO LOAD AND RELEASE ALL     * EL529
00537 *         *  ISSUES, CANCELS, AND BATCH RECORDS TO THE EXTRACT  * EL529
00538 *         *  SORT.                                              * EL529
00539 *         *******************************************************.EL529
00540                                                                   EL529
00541      MOVE LOW-VALUES             TO  PB-CONTROL-PRIMARY           EL529
00542      MOVE WS-COMPANY-CD          TO  PB-COMPANY-CD                EL529
00543      MOVE ZEROS                  TO  WS-ISS-CAN-TOTALS.           EL529
00544                                                                   EL529
00545      START ERPNDB                                                 EL529
00546          KEY IS GREATER THAN PB-CONTROL-PRIMARY.                  EL529
00547                                                                   EL529
00548      IF ERPNDB-FILE-STATUS = '23'                                 EL529
00549          DISPLAY 'EL520 NO RECORDS FOUND - ERPNDB  FOR CO - '     EL529
00550              WS-COMPANY-ID UPON CONSOLE                           EL529
00551          GO TO 1099-CLOSE-PENDING-BUS-FILE.                       EL529
00552                                                                   EL529
00553      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL529
00554          MOVE 'ERROR OCCURED START - ERPNDB'                      EL529
00555                                  TO  WS-ABEND-MESSAGE             EL529
00556          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
00557          GO TO ABEND-PGM.                                         EL529
00558                                                                   EL529
00559      EJECT                                                        EL529
00560                                                                   EL529
00561  1050-SIP.                                                        EL529
00562      READ ERPNDB NEXT RECORD.                                     EL529
00563                                                                   EL529
00564      IF ERPNDB-FILE-STATUS = '10'                                 EL529
00565          GO TO 1099-CLOSE-PENDING-BUS-FILE.                       EL529
00566                                                                   EL529
00567      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL529
00568          MOVE 'ERROR OCCURED READNEXT - ERPNDB'                   EL529
00569                                  TO  WS-ABEND-MESSAGE             EL529
00570          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
00571          GO TO ABEND-PGM.                                         EL529
00572                                                                   EL529
00573      IF PB-COMPANY-CD NOT EQUAL  TO WS-COMPANY-CD                 EL529
00574          GO TO 1099-CLOSE-PENDING-BUS-FILE.                       EL529
00575                                                                   EL529
00576      IF PB-CREDIT-ACCEPT-DT NOT EQUAL LOW-VALUES                  EL529
00577          GO TO 1050-SIP.                                          EL529
00578                                                                   EL529
00579      IF PB-ALT-CHG-SEQ-NO  NOT EQUAL ZERO                         EL529
00580          GO TO 1050-SIP.                                          EL529
00581                                                                   EL529
00582      IF PB-ISSUE                                                  EL529
00583         ADD +1                           TO WS-ISS-TOTAL-CNT      EL529
00584         IF PB-FATAL-ERRORS                                        EL529
00585            ADD +1                        TO WS-ISS-FATAL-CNT      EL529
00586            ADD PB-I-LF-PREMIUM-AMT       TO WS-ISS-BAD-PREM       EL529
00587            ADD PB-I-AH-PREMIUM-AMT       TO WS-ISS-BAD-PREM       EL529
00588        ELSE                                                       EL529
00589            IF PB-UNFORCED-ERRORS                                  EL529
00590               ADD +1                     TO WS-ISS-FORCE-CNT      EL529
00591               ADD PB-I-LF-PREMIUM-AMT    TO WS-ISS-BAD-PREM       EL529
00592               ADD PB-I-AH-PREMIUM-AMT    TO WS-ISS-BAD-PREM       EL529
00593           ELSE                                                    EL529
00594               IF PB-RECORD-ON-HOLD                                EL529
00595                  ADD +1                  TO WS-ISS-HOLD-CNT       EL529
00596                  ADD PB-I-LF-PREMIUM-AMT TO WS-ISS-BAD-PREM       EL529
00597                  ADD PB-I-AH-PREMIUM-AMT TO WS-ISS-BAD-PREM       EL529
00598              ELSE                                                 EL529
00599                  ADD +1                  TO WS-ISS-AVAIL-CNT      EL529
00600                  ADD PB-I-LF-PREMIUM-AMT TO WS-ISS-GOOD-PREM      EL529
00601                  ADD PB-I-AH-PREMIUM-AMT TO WS-ISS-GOOD-PREM.     EL529
00602                                                                   EL529
00603      IF PB-CANCELLATION                                           EL529
00604         ADD +1                           TO WS-CAN-TOTAL-CNT      EL529
00605         IF PB-FATAL-ERRORS                                        EL529
00606            ADD +1                        TO WS-CAN-FATAL-CNT      EL529
00607            ADD PB-C-LF-CANCEL-AMT        TO WS-CAN-BAD-PREM       EL529
00608            ADD PB-C-AH-CANCEL-AMT        TO WS-CAN-BAD-PREM       EL529
00609        ELSE                                                       EL529
00610            IF PB-UNFORCED-ERRORS                                  EL529
00611               ADD +1                     TO WS-CAN-FORCE-CNT      EL529
00612               ADD PB-C-LF-CANCEL-AMT     TO WS-CAN-BAD-PREM       EL529
00613               ADD PB-C-AH-CANCEL-AMT     TO WS-CAN-BAD-PREM       EL529
00614           ELSE                                                    EL529
00615               IF PB-RECORD-ON-HOLD                                EL529
00616                  ADD +1                  TO WS-CAN-HOLD-CNT       EL529
00617                  ADD PB-C-LF-CANCEL-AMT  TO WS-CAN-BAD-PREM       EL529
00618                  ADD PB-C-AH-CANCEL-AMT  TO WS-CAN-BAD-PREM       EL529
00619              ELSE                                                 EL529
00620                  ADD +1                  TO WS-CAN-AVAIL-CNT      EL529
00621                  ADD PB-C-LF-CANCEL-AMT  TO WS-CAN-GOOD-PREM      EL529
00622                  ADD PB-C-AH-CANCEL-AMT  TO WS-CAN-GOOD-PREM.     EL529
00623                                                                   EL529
00624                                                                   EL529
00625      IF PB-BATCH-TRAILER                                          EL529
00626              GO TO 1060-SIP.                                      EL529
00627                                                                   EL529
00628      GO TO 1050-SIP.                                              EL529
00629                                                                   EL529
00630      EJECT                                                        EL529
00631                                                                   EL529
00632  1060-SIP.                                                        EL529
00633      IF WS-ISS-TOTAL-CNT = WS-ISS-AVAIL-CNT  AND                  EL529
00634         WS-CAN-TOTAL-CNT = WS-CAN-AVAIL-CNT                       EL529
00635          MOVE ZEROS              TO  WS-ISS-CAN-TOTALS            EL529
00636          GO TO 1050-SIP.                                          EL529
00637                                                                   EL529
00638      MOVE SPACES                 TO  EXTRACT-INTERFACE-RECORD.    EL529
00639      MOVE 'B'                    TO  EX-EXTRACT-CODE.             EL529
00640      MOVE 'EX'                   TO  EX-RECORD-ID.                EL529
00641      MOVE '2'                    TO  EX-POSITIONING-CODE.         EL529
00642      MOVE 'B'                    TO  EX-RECORD-TYPE.              EL529
00643      MOVE CF-COMPANY-CD          TO  EX-COMPANY-CD.               EL529
00644      MOVE CF-COMPANY-ID          TO  EX-COMPANY-ID.               EL529
00645      MOVE PB-CARRIER             TO  EX-SE-CARRIER.               EL529
00646      MOVE PB-GROUPING            TO  EX-SE-GROUPING.              EL529
00647      MOVE PB-STATE               TO  EX-SE-STATE.                 EL529
00648      MOVE PB-ACCOUNT             TO  EX-SE-ACCOUNT.               EL529
00649      MOVE PB-ENTRY-BATCH         TO  EX-SE-BATCH-NO.              EL529
00650      MOVE PB-RECORD-TYPE         TO  EX-SE-RECORD-TYPE.           EL529
00651      MOVE WS-BATCH-TOTALS        TO  EX-DATA-AREAS.               EL529
00652      RELEASE EXTRACT-INTERFACE-RECORD.                            EL529
00653      ADD +1                      TO WS-RECORD-COUNT.              EL529
00654      MOVE ZEROS                  TO WS-ISS-CAN-TOTALS.            EL529
00655                                                                   EL529
00656      GO TO 1050-SIP.                                              EL529
00657                                                                   EL529
00658  1099-CLOSE-PENDING-BUS-FILE.                                     EL529
00659      CLOSE ERPNDB.                                                EL529
00660                                                                   EL529
00661      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL529
00662          MOVE 'ERROR OCCURED CLOSE - ERPNDB'                      EL529
00663                                  TO  WS-ABEND-MESSAGE             EL529
00664          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
00665          GO TO ABEND-PGM.                                         EL529
00666                                                                   EL529
00667      MOVE SPACES                 TO  WS-COMPANY-NAME              EL529
00668                                      WS-H2-CLIENT-NAME            EL529
00669                                      WS-COMPANY-ID                EL529
00670                                      WS-COMPANY-CD                EL529
00671                                      WS-LAST-CARRIER.             EL529
00672                                                                   EL529
00673  1099-EXIT.                                                       EL529
00674      EJECT                                                        EL529
00675  1900-PROCESS-EXTRACT SECTION.                                    EL529
00676                                                                   EL529
00677  1901-READ-NEXT.                                                  EL529
00678      RETURN SORT-FILE                                             EL529
00679          AT END                                                   EL529
00680              GO TO 1990-EOJ.                                      EL529
00681                                                                   EL529
00682      IF EX-POSITIONING-CODE LESS THAN '2'                         EL529
00683          GO TO 1901-READ-NEXT                                     EL529
00684      ELSE                                                         EL529
00685          IF EX-POSITIONING-CODE GREATER THAN '2'                  EL529
00686              GO TO 1990-EOJ.                                      EL529
00687                                                                   EL529
00688      IF EX-EXTRACT-CODE NOT = 'B'                                 EL529
00689          GO TO 1990-EOJ.                                          EL529
00690                                                                   EL529
00691      IF EX-RECORD-TYPE NOT = 'B'                                  EL529
00692          GO TO 1901-READ-NEXT.                                    EL529
00693                                                                   EL529
00694      IF EX-COMPANY-CD = WS-COMPANY-CD                             EL529
00695          NEXT SENTENCE                                            EL529
00696      ELSE                                                         EL529
00697          PERFORM 3500-FINAL-TOTALS                                EL529
00698          PERFORM 7000-CHECK-PRINT-OPTIONS.                        EL529
00699                                                                   EL529
00700      IF WS-FREQUENCY-OPTION NOT = 'NONE' OR                       EL529
00701         WS-PROCESS-OPTION = 'X'                                   EL529
00702          GO TO 1901-READ-NEXT.                                    EL529
00703                                                                   EL529
00704      PERFORM 2000-PRINT.                                          EL529
00705                                                                   EL529
00706      GO TO 1901-READ-NEXT.                                        EL529
00707                                                                   EL529
00708  1990-EOJ.                                                        EL529
00709      MOVE +1 TO WS-EOF-SW                                         EL529
00710      PERFORM 3500-FINAL-TOTALS.                                   EL529
00711                                                                   EL529
00712  1999-EXIT.                                                       EL529
00713      EXIT.                                                        EL529
00714      EJECT                                                        EL529
00715                                                                   EL529
00716  2000-PRINT SECTION.                                              EL529
00717      IF EX-SE-CARRIER  NOT = WS-SAVE-CARRIER  OR                  EL529
00718         EX-SE-GROUPING NOT = WS-SAVE-GROUPING OR                  EL529
00719         EX-SE-STATE    NOT = WS-SAVE-STATE    OR                  EL529
00720         EX-SE-ACCOUNT  NOT = WS-SAVE-ACCOUNT                      EL529
00721          PERFORM 3000-ACCOUNT-BREAK.                              EL529
00722                                                                   EL529
00723      ADD +1                      TO  WS-NO-BATCHES.               EL529
00724      MOVE EX-DATA-AREAS          TO  WS-BATCH-TOTALS.             EL529
00725                                                                   EL529
00726      ADD WS-ISS-TOTAL-CNT        TO  WS-TOT-TOTAL-CNT             EL529
00727                                      WS-ACC-ISS-TOTAL-CNT         EL529
00728                                      WS-ACC-TOT-TOTAL-CNT         EL529
00729                                      WS-FIN-ISS-TOTAL-CNT         EL529
00730                                      WS-FIN-TOT-TOTAL-CNT.        EL529
00731      ADD WS-ISS-FATAL-CNT        TO  WS-TOT-FATAL-CNT             EL529
00732                                      WS-ACC-ISS-FATAL-CNT         EL529
00733                                      WS-ACC-TOT-FATAL-CNT         EL529
00734                                      WS-FIN-ISS-FATAL-CNT         EL529
00735                                      WS-FIN-TOT-FATAL-CNT.        EL529
00736      ADD WS-ISS-FORCE-CNT        TO  WS-TOT-FORCE-CNT             EL529
00737                                      WS-ACC-ISS-FORCE-CNT         EL529
00738                                      WS-ACC-TOT-FORCE-CNT         EL529
00739                                      WS-FIN-ISS-FORCE-CNT         EL529
00740                                      WS-FIN-TOT-FORCE-CNT.        EL529
00741      ADD WS-ISS-HOLD-CNT         TO  WS-TOT-HOLD-CNT              EL529
00742                                      WS-ACC-ISS-HOLD-CNT          EL529
00743                                      WS-ACC-TOT-HOLD-CNT          EL529
00744                                      WS-FIN-ISS-HOLD-CNT          EL529
00745                                      WS-FIN-TOT-HOLD-CNT.         EL529
00746      ADD WS-ISS-AVAIL-CNT        TO  WS-TOT-AVAIL-CNT             EL529
00747                                      WS-ACC-ISS-AVAIL-CNT         EL529
00748                                      WS-ACC-TOT-AVAIL-CNT         EL529
00749                                      WS-FIN-ISS-AVAIL-CNT         EL529
00750                                      WS-FIN-TOT-AVAIL-CNT.        EL529
00751      ADD WS-ISS-GOOD-PREM        TO  WS-TOT-GOOD-PREM             EL529
00752                                      WS-ACC-ISS-GOOD-PREM         EL529
00753                                      WS-ACC-TOT-GOOD-PREM         EL529
00754                                      WS-FIN-ISS-GOOD-PREM         EL529
00755                                      WS-FIN-TOT-GOOD-PREM.        EL529
00756      ADD WS-ISS-BAD-PREM         TO  WS-TOT-BAD-PREM              EL529
00757                                      WS-ACC-ISS-BAD-PREM          EL529
00758                                      WS-ACC-TOT-BAD-PREM          EL529
00759                                      WS-FIN-ISS-BAD-PREM          EL529
00760                                      WS-FIN-TOT-BAD-PREM.         EL529
00761      ADD WS-CAN-TOTAL-CNT        TO  WS-TOT-TOTAL-CNT             EL529
00762                                      WS-ACC-CAN-TOTAL-CNT         EL529
00763                                      WS-ACC-TOT-TOTAL-CNT         EL529
00764                                      WS-FIN-CAN-TOTAL-CNT         EL529
00765                                      WS-FIN-TOT-TOTAL-CNT.        EL529
00766      ADD WS-CAN-FATAL-CNT        TO  WS-TOT-FATAL-CNT             EL529
00767                                      WS-ACC-CAN-FATAL-CNT         EL529
00768                                      WS-ACC-TOT-FATAL-CNT         EL529
00769                                      WS-FIN-CAN-FATAL-CNT         EL529
00770                                      WS-FIN-TOT-FATAL-CNT.        EL529
00771      ADD WS-CAN-FORCE-CNT        TO  WS-TOT-FORCE-CNT             EL529
00772                                      WS-ACC-CAN-FORCE-CNT         EL529
00773                                      WS-ACC-TOT-FORCE-CNT         EL529
00774                                      WS-FIN-CAN-FORCE-CNT         EL529
00775                                      WS-FIN-TOT-FORCE-CNT.        EL529
00776      ADD WS-CAN-HOLD-CNT         TO  WS-TOT-HOLD-CNT              EL529
00777                                      WS-ACC-CAN-HOLD-CNT          EL529
00778                                      WS-ACC-TOT-HOLD-CNT          EL529
00779                                      WS-FIN-CAN-HOLD-CNT          EL529
00780                                      WS-FIN-TOT-HOLD-CNT.         EL529
00781      ADD WS-CAN-AVAIL-CNT        TO  WS-TOT-AVAIL-CNT             EL529
00782                                      WS-ACC-CAN-AVAIL-CNT         EL529
00783                                      WS-ACC-TOT-AVAIL-CNT         EL529
00784                                      WS-FIN-CAN-AVAIL-CNT         EL529
00785                                      WS-FIN-TOT-AVAIL-CNT.        EL529
00786      ADD WS-CAN-GOOD-PREM        TO  WS-TOT-GOOD-PREM             EL529
00787                                      WS-ACC-CAN-GOOD-PREM         EL529
00788                                      WS-ACC-TOT-GOOD-PREM         EL529
00789                                      WS-FIN-CAN-GOOD-PREM         EL529
00790                                      WS-FIN-TOT-GOOD-PREM.        EL529
00791      ADD WS-CAN-BAD-PREM         TO  WS-TOT-BAD-PREM              EL529
00792                                      WS-ACC-CAN-BAD-PREM          EL529
00793                                      WS-ACC-TOT-BAD-PREM          EL529
00794                                      WS-FIN-CAN-BAD-PREM          EL529
00795                                      WS-FIN-TOT-BAD-PREM.         EL529
00796  2010-PRINT-DETAIL.                                               EL529
00797      MOVE SPACES                 TO  WS-DETAIL1.                  EL529
00798                                                                   EL529
00799      MOVE EX-SE-BATCH-NO         TO  WS-D1-BATCH-NO.              EL529
00800      MOVE WS-ISS-TOTAL-CNT       TO  WS-D1-TOTAL-COUNT.           EL529
00801      MOVE WS-ISS-FATAL-CNT       TO  WS-D1-FATAL-COUNT.           EL529
00802      MOVE WS-ISS-FORCE-CNT       TO  WS-D1-FORCE-COUNT.           EL529
00803      MOVE WS-ISS-HOLD-CNT        TO  WS-D1-HOLD-COUNT.            EL529
00804      MOVE WS-ISS-AVAIL-CNT       TO  WS-D1-AVAIL-COUNT.           EL529
00805      MOVE WS-ISS-GOOD-PREM       TO  WS-D1-GOOD-PREMIUM.          EL529
00806      MOVE WS-ISS-BAD-PREM        TO  WS-D1-BAD-PREMIUM.           EL529
00807      MOVE 'ISSUES '              TO  WS-D1-DESC.                  EL529
00808                                                                   EL529
00809      MOVE WS-DETAIL1             TO  PRT.                         EL529
00810      MOVE ZERO                   TO  P-CTL.                       EL529
00811      PERFORM WRITE-A-LINE.                                        EL529
00812                                                                   EL529
00813      MOVE SPACES                 TO  WS-DETAIL1.                  EL529
00814                                                                   EL529
00815      MOVE SPACES                 TO  WS-D1-BATCH-NO.              EL529
00816      MOVE WS-CAN-TOTAL-CNT       TO  WS-D1-TOTAL-COUNT.           EL529
00817      MOVE WS-CAN-FATAL-CNT       TO  WS-D1-FATAL-COUNT.           EL529
00818      MOVE WS-CAN-FORCE-CNT       TO  WS-D1-FORCE-COUNT.           EL529
00819      MOVE WS-CAN-HOLD-CNT        TO  WS-D1-HOLD-COUNT.            EL529
00820      MOVE WS-CAN-AVAIL-CNT       TO  WS-D1-AVAIL-COUNT.           EL529
00821      MOVE WS-CAN-GOOD-PREM       TO  WS-D1-GOOD-PREMIUM.          EL529
00822      MOVE WS-CAN-BAD-PREM        TO  WS-D1-BAD-PREMIUM.           EL529
00823      MOVE 'CANCELS'              TO  WS-D1-DESC.                  EL529
00824                                                                   EL529
00825      MOVE WS-DETAIL1             TO  PRT.                         EL529
00826      PERFORM WRITE-A-LINE.                                        EL529
00827                                                                   EL529
00828      MOVE SPACES                 TO  WS-DETAIL1.                  EL529
00829                                                                   EL529
00830      MOVE SPACES                 TO  WS-D1-BATCH-NO.              EL529
00831      MOVE WS-TOT-TOTAL-CNT       TO  WS-D1-TOTAL-COUNT.           EL529
00832      MOVE WS-TOT-FATAL-CNT       TO  WS-D1-FATAL-COUNT.           EL529
00833      MOVE WS-TOT-FORCE-CNT       TO  WS-D1-FORCE-COUNT.           EL529
00834      MOVE WS-TOT-HOLD-CNT        TO  WS-D1-HOLD-COUNT.            EL529
00835      MOVE WS-TOT-AVAIL-CNT       TO  WS-D1-AVAIL-COUNT.           EL529
00836      MOVE WS-TOT-GOOD-PREM       TO  WS-D1-GOOD-PREMIUM.          EL529
00837      MOVE WS-TOT-BAD-PREM        TO  WS-D1-BAD-PREMIUM.           EL529
00838      MOVE 'TOTAL  '              TO  WS-D1-DESC.                  EL529
00839                                                                   EL529
00840      MOVE WS-DETAIL1             TO  PRT.                         EL529
00841      PERFORM WRITE-A-LINE.                                        EL529
00842                                                                   EL529
00843      MOVE ZEROS                  TO  WS-BATCH-TOTALS              EL529
00844                                      WS-BATCH-ISS-CAN-TOTALS.     EL529
00845                                                                   EL529
00846  2000-EXIT.                                                       EL529
00847      EXIT.                                                        EL529
00848      EJECT                                                        EL529
00849                                                                   EL529
00850  3000-ACCOUNT-BREAK SECTION.                                      EL529
00851      IF ONLY-ONE-BATCH OR                                         EL529
00852         NEW-ACCOUNT                                               EL529
00853          GO TO 3020-ZERO-TOTALS.                                  EL529
00854                                                                   EL529
00855      MOVE WS-HEADING8            TO  PRT.                         EL529
00856      MOVE ZERO                   TO  P-CTL.                       EL529
00857      PERFORM WRITE-A-LINE.                                        EL529
00858                                                                   EL529
00859  3010-PRINT-DETAIL.                                               EL529
00860      MOVE SPACES                 TO  WS-DETAIL1.                  EL529
00861                                                                   EL529
00862      MOVE SPACES                 TO  WS-D1-BATCH-NO.              EL529
00863      MOVE WS-ACC-ISS-TOTAL-CNT   TO  WS-D1-TOTAL-COUNT.           EL529
00864      MOVE WS-ACC-ISS-FATAL-CNT   TO  WS-D1-FATAL-COUNT.           EL529
00865      MOVE WS-ACC-ISS-FORCE-CNT   TO  WS-D1-FORCE-COUNT.           EL529
00866      MOVE WS-ACC-ISS-HOLD-CNT    TO  WS-D1-HOLD-COUNT.            EL529
00867      MOVE WS-ACC-ISS-AVAIL-CNT   TO  WS-D1-AVAIL-COUNT.           EL529
00868      MOVE WS-ACC-ISS-GOOD-PREM   TO  WS-D1-GOOD-PREMIUM.          EL529
00869      MOVE WS-ACC-ISS-BAD-PREM    TO  WS-D1-BAD-PREMIUM.           EL529
00870      MOVE 'ISSUES '              TO  WS-D1-DESC.                  EL529
00871                                                                   EL529
00872      MOVE WS-DETAIL1             TO  PRT.                         EL529
00873      PERFORM WRITE-A-LINE.                                        EL529
00874                                                                   EL529
00875      MOVE SPACES                 TO  WS-DETAIL1.                  EL529
00876                                                                   EL529
00877      MOVE SPACES                 TO  WS-D1-BATCH-NO.              EL529
00878      MOVE WS-ACC-CAN-TOTAL-CNT   TO  WS-D1-TOTAL-COUNT.           EL529
00879      MOVE WS-ACC-CAN-FATAL-CNT   TO  WS-D1-FATAL-COUNT.           EL529
00880      MOVE WS-ACC-CAN-FORCE-CNT   TO  WS-D1-FORCE-COUNT.           EL529
00881      MOVE WS-ACC-CAN-HOLD-CNT    TO  WS-D1-HOLD-COUNT.            EL529
00882      MOVE WS-ACC-CAN-AVAIL-CNT   TO  WS-D1-AVAIL-COUNT.           EL529
00883      MOVE WS-ACC-CAN-GOOD-PREM   TO  WS-D1-GOOD-PREMIUM.          EL529
00884      MOVE WS-ACC-CAN-BAD-PREM    TO  WS-D1-BAD-PREMIUM.           EL529
00885      MOVE 'CANCELS'              TO  WS-D1-DESC.                  EL529
00886                                                                   EL529
00887      MOVE WS-DETAIL1             TO  PRT.                         EL529
00888      PERFORM WRITE-A-LINE.                                        EL529
00889                                                                   EL529
00890      MOVE SPACES                 TO  WS-DETAIL1.                  EL529
00891                                                                   EL529
00892      MOVE SPACES                 TO  WS-D1-BATCH-NO.              EL529
00893      MOVE WS-ACC-TOT-TOTAL-CNT   TO  WS-D1-TOTAL-COUNT.           EL529
00894      MOVE WS-ACC-TOT-FATAL-CNT   TO  WS-D1-FATAL-COUNT.           EL529
00895      MOVE WS-ACC-TOT-FORCE-CNT   TO  WS-D1-FORCE-COUNT.           EL529
00896      MOVE WS-ACC-TOT-HOLD-CNT    TO  WS-D1-HOLD-COUNT.            EL529
00897      MOVE WS-ACC-TOT-AVAIL-CNT   TO  WS-D1-AVAIL-COUNT.           EL529
00898      MOVE WS-ACC-TOT-GOOD-PREM   TO  WS-D1-GOOD-PREMIUM.          EL529
00899      MOVE WS-ACC-TOT-BAD-PREM    TO  WS-D1-BAD-PREMIUM.           EL529
00900      MOVE 'TOTAL  '              TO  WS-D1-DESC.                  EL529
00901                                                                   EL529
00902      MOVE WS-DETAIL1             TO  PRT.                         EL529
00903      PERFORM WRITE-A-LINE.                                        EL529
00904                                                                   EL529
00905  3020-ZERO-TOTALS.                                                EL529
00906      MOVE ZEROS                  TO WS-ISS-CAN-TOTALS             EL529
00907                                     WS-BATCH-ISS-CAN-TOTALS       EL529
00908                                     WS-ACCOUNT-TOTALS.            EL529
00909                                                                   EL529
00910      IF EXTRACT-EOF                                               EL529
00911          GO TO 3000-EXIT.                                         EL529
00912                                                                   EL529
00913      PERFORM 6300-PROCESS-ACCOUNT.                                EL529
00914                                                                   EL529
00915      ADD WS-LINE-COUNT-MAX       TO WS-LINE-COUNT.                EL529
00916      MOVE +0                     TO WS-NO-BATCHES.                EL529
00917      MOVE EX-VARIABLE-ASCENDING-KEYS                              EL529
00918                                  TO WS-SAVE-ACCOUNT-KEY.          EL529
00919                                                                   EL529
00920  3000-EXIT.                                                       EL529
00921      EXIT.                                                        EL529
00922      EJECT                                                        EL529
00923  3500-FINAL-TOTALS SECTION.                                       EL529
00924      IF LCP-ONCTR-01 =  0                                         EL529
00925          ADD 1 TO LCP-ONCTR-01                                    EL529
00926          GO TO 3520-ZERO-TOTALS.                                  EL529
00927                                                                   EL529
00928      IF WS-FREQUENCY-OPTION NOT = 'NONE' OR                       EL529
00929         WS-PROCESS-OPTION = 'X'                                   EL529
00930          GO TO 3500-EXIT.                                         EL529
00931                                                                   EL529
00932      PERFORM 3000-ACCOUNT-BREAK.                                  EL529
00933                                                                   EL529
00934      MOVE WS-HEADING5            TO  WS-SAVE-HEADING5.            EL529
00935      MOVE SPACES                 TO  WS-HEADING5.                 EL529
00936      MOVE WS-HEADING9            TO  PRT.                         EL529
00937      MOVE ZERO                   TO  P-CTL.                       EL529
00938      PERFORM WRITE-A-LINE.                                        EL529
00939      MOVE WS-SAVE-HEADING5       TO  WS-HEADING5.                 EL529
00940                                                                   EL529
00941  3510-PRINT-DETAIL.                                               EL529
00942      MOVE SPACES                 TO  WS-DETAIL1.                  EL529
00943                                                                   EL529
00944      MOVE WS-FIN-ISS-TOTAL-CNT   TO  WS-D1-TOTAL-COUNT.           EL529
00945      MOVE WS-FIN-ISS-FATAL-CNT   TO  WS-D1-FATAL-COUNT.           EL529
00946      MOVE WS-FIN-ISS-FORCE-CNT   TO  WS-D1-FORCE-COUNT.           EL529
00947      MOVE WS-FIN-ISS-HOLD-CNT    TO  WS-D1-HOLD-COUNT.            EL529
00948      MOVE WS-FIN-ISS-AVAIL-CNT   TO  WS-D1-AVAIL-COUNT.           EL529
00949      MOVE WS-FIN-ISS-GOOD-PREM   TO  WS-D1-GOOD-PREMIUM.          EL529
00950      MOVE WS-FIN-ISS-BAD-PREM    TO  WS-D1-BAD-PREMIUM.           EL529
00951      MOVE 'ISSUES '              TO  WS-D1-DESC.                  EL529
00952                                                                   EL529
00953      MOVE WS-DETAIL1             TO  PRT.                         EL529
00954      PERFORM WRITE-A-LINE.                                        EL529
00955                                                                   EL529
00956      MOVE SPACES                 TO  WS-DETAIL1.                  EL529
00957                                                                   EL529
00958      MOVE WS-FIN-CAN-TOTAL-CNT   TO  WS-D1-TOTAL-COUNT.           EL529
00959      MOVE WS-FIN-CAN-FATAL-CNT   TO  WS-D1-FATAL-COUNT.           EL529
00960      MOVE WS-FIN-CAN-FORCE-CNT   TO  WS-D1-FORCE-COUNT.           EL529
00961      MOVE WS-FIN-CAN-HOLD-CNT    TO  WS-D1-HOLD-COUNT.            EL529
00962      MOVE WS-FIN-CAN-AVAIL-CNT   TO  WS-D1-AVAIL-COUNT.           EL529
00963      MOVE WS-FIN-CAN-GOOD-PREM   TO  WS-D1-GOOD-PREMIUM.          EL529
00964      MOVE WS-FIN-CAN-BAD-PREM    TO  WS-D1-BAD-PREMIUM.           EL529
00965      MOVE 'CANCELS'              TO  WS-D1-DESC.                  EL529
00966                                                                   EL529
00967      MOVE WS-DETAIL1             TO  PRT.                         EL529
00968      PERFORM WRITE-A-LINE.                                        EL529
00969                                                                   EL529
00970      MOVE SPACES                 TO  WS-DETAIL1.                  EL529
00971                                                                   EL529
00972      MOVE WS-FIN-TOT-TOTAL-CNT   TO  WS-D1-TOTAL-COUNT.           EL529
00973      MOVE WS-FIN-TOT-FATAL-CNT   TO  WS-D1-FATAL-COUNT.           EL529
00974      MOVE WS-FIN-TOT-FORCE-CNT   TO  WS-D1-FORCE-COUNT.           EL529
00975      MOVE WS-FIN-TOT-HOLD-CNT    TO  WS-D1-HOLD-COUNT.            EL529
00976      MOVE WS-FIN-TOT-AVAIL-CNT   TO  WS-D1-AVAIL-COUNT.           EL529
00977      MOVE WS-FIN-TOT-GOOD-PREM   TO  WS-D1-GOOD-PREMIUM.          EL529
00978      MOVE WS-FIN-TOT-BAD-PREM    TO  WS-D1-BAD-PREMIUM.           EL529
00979      MOVE 'TOTAL  '              TO  WS-D1-DESC.                  EL529
00980                                                                   EL529
00981      MOVE WS-DETAIL1             TO  PRT.                         EL529
00982      PERFORM WRITE-A-LINE.                                        EL529
00983                                                                   EL529
00984      PERFORM 4000-BUILD-TRAILER.                                  EL529
00985                                                                   EL529
00986  3520-ZERO-TOTALS.                                                EL529
00987      MOVE ZEROS                  TO WS-FINAL-TOTALS               EL529
00988                                     WS-ISS-CAN-TOTALS             EL529
00989                                     WS-BATCH-ISS-CAN-TOTALS       EL529
00990                                     WS-ACCOUNT-TOTALS.            EL529
00991                                                                   EL529
00992      ADD WS-LINE-COUNT-MAX       TO WS-LINE-COUNT.                EL529
00993                                                                   EL529
00994  3500-EXIT.                                                       EL529
00995      EXIT.                                                        EL529
00996      EJECT                                                        EL529
00997                                                                   EL529
00998  4000-BUILD-TRAILER SECTION.                                      EL529
00999      IF WS-PRINT-OPTION = ('S' OR 'T')                            EL529
01000          AND WS-LINE-NUMBER GREATER THAN ZERO                     EL529
01001              NEXT SENTENCE                                        EL529
01002          ELSE                                                     EL529
01003              GO TO 4000-EXIT.                                     EL529
01004                                                                   EL529
01005       MOVE WS-COMPANY-CD         TO  RF-COMPANY-CD.               EL529
01006       MOVE '2'                   TO  RF-RECORD-TYPE.              EL529
01007       MOVE 'EL529'               TO  RF-RECORD-ID.                EL529
01008       ADD +1                     TO  WS-LINE-NUMBER.              EL529
01009       MOVE WS-LINE-NUMBER        TO  RF-LINE-NUMBER.              EL529
01010       MOVE SPACES                TO  RF-TRAILER-RECORD.           EL529
01011       ACCEPT LCP-TIME-OF-DAY-74 FROM TIME                         EL529
01012       MOVE LCP-TIME-74 TO LCP-TIME-OF-DAY-68                      EL529
01013       MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.              EL529
01014       ACCEPT  LCP-DATE-NEW-74 FROM DATE                           EL529
01015       MOVE CORRESPONDING LCP-DATE-NEW-74 TO LCP-CURRENT-DATE-68   EL529
01016       MOVE  LCP-CURRENT-DATE-68 TO RF-CURRENT-DATE.               EL529
01017                                                                   EL529
01018       WRITE REPORT-SAVE-FILE.                                     EL529
01019                                                                   EL529
01020       IF ELREPT-FILE-STATUS NOT = ZERO                            EL529
01021           MOVE 'ERROR OCCURED WRITE ELREPT TRAILER RECORD'        EL529
01022                                  TO WS-ABEND-MESSAGE              EL529
01023           MOVE ELREPT-FILE-STATUS                                 EL529
01024                                  TO WS-ABEND-FILE-STATUS          EL529
01025           GO TO ABEND-PGM.                                        EL529
01026                                                                   EL529
01027  4000-EXIT.                                                       EL529
01028      EXIT.                                                        EL529
01029      EJECT                                                        EL529
01030                                                                   EL529
01031  6300-PROCESS-ACCOUNT SECTION.                                    EL529
01032      MOVE SPACES                 TO  AM-CONTROL-BY-VAR-GRP.       EL529
01033      MOVE CF-COMPANY-CD          TO  AM-COMPANY-CD-A1.            EL529
01034      MOVE EX-SE-ACCOUNT          TO  AM-VG-ACCOUNT.               EL529
01035                                                                   EL529
01036      IF CF-ST-ACCNT-CNTL                                          EL529
01037          MOVE EX-SE-STATE        TO  AM-VG-STATE.                 EL529
01038                                                                   EL529
01039      IF CF-CARR-ACCNT-CNTL                                        EL529
01040          MOVE EX-SE-CARRIER      TO  AM-VG-CARRIER.               EL529
01041                                                                   EL529
01042      IF CF-CARR-ST-ACCNT-CNTL                                     EL529
01043          MOVE EX-SE-CARRIER      TO  AM-VG-CARRIER                EL529
01044          MOVE EX-SE-STATE        TO  AM-VG-STATE.                 EL529
01045                                                                   EL529
01046      IF CF-CARR-GROUP-ST-ACCNT-CNTL                               EL529
01047          MOVE EX-SE-CARRIER      TO  AM-VG-CARRIER                EL529
01048          MOVE EX-SE-GROUPING     TO  AM-VG-GROUPING               EL529
01049          MOVE EX-SE-STATE        TO  AM-VG-STATE.                 EL529
01050                                                                   EL529
01051      START ERACCT                                                 EL529
01052          KEY IS GREATER THAN AM-CONTROL-BY-VAR-GRP.               EL529
01053                                                                   EL529
01054      IF ERACCT-FILE-STATUS NOT = ZERO                             EL529
01055          GO TO 6310-INVALID-ACCOUNT.                              EL529
01056                                                                   EL529
01057      READ ERACCT NEXT RECORD.                                     EL529
01058                                                                   EL529
01059      IF ERACCT-FILE-STATUS NOT = ZERO                             EL529
01060          DISPLAY '*** EL529 ERACCT INVALID READ NEXT'             EL529
01061              UPON CONSOLE                                         EL529
01062          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01063          GO TO ABEND-PGM.                                         EL529
01064                                                                   EL529
01065      IF CF-ACCNT-CNTL                                             EL529
01066          IF EX-SE-ACCOUNT = AM-VG-ACCOUNT                         EL529
01067              GO TO 6320-VALID-ACCOUNT                             EL529
01068          ELSE                                                     EL529
01069              GO TO 6310-INVALID-ACCOUNT.                          EL529
01070                                                                   EL529
01071      IF CF-ST-ACCNT-CNTL                                          EL529
01072          IF EX-SE-ACCOUNT = AM-VG-ACCOUNT AND                     EL529
01073             EX-SE-STATE   = AM-VG-STATE                           EL529
01074              GO TO 6320-VALID-ACCOUNT                             EL529
01075          ELSE                                                     EL529
01076              GO TO 6310-INVALID-ACCOUNT.                          EL529
01077                                                                   EL529
01078      IF CF-CARR-ACCNT-CNTL                                        EL529
01079          IF EX-SE-ACCOUNT = AM-VG-ACCOUNT AND                     EL529
01080             EX-SE-CARRIER = AM-VG-CARRIER                         EL529
01081              GO TO 6320-VALID-ACCOUNT                             EL529
01082          ELSE                                                     EL529
01083              GO TO 6310-INVALID-ACCOUNT.                          EL529
01084                                                                   EL529
01085      IF CF-CARR-ST-ACCNT-CNTL                                     EL529
01086          IF EX-SE-ACCOUNT = AM-VG-ACCOUNT AND                     EL529
01087             EX-SE-STATE   = AM-VG-STATE   AND                     EL529
01088             EX-SE-CARRIER = AM-VG-CARRIER                         EL529
01089              GO TO 6320-VALID-ACCOUNT                             EL529
01090          ELSE                                                     EL529
01091              GO TO 6310-INVALID-ACCOUNT.                          EL529
01092                                                                   EL529
01093      IF CF-CARR-GROUP-ST-ACCNT-CNTL                               EL529
01094          IF EX-SE-ACCOUNT   = AM-VG-ACCOUNT AND                   EL529
01095             EX-SE-STATE     = AM-VG-STATE   AND                   EL529
01096             EX-SE-CARRIER   = AM-VG-CARRIER AND                   EL529
01097             EX-SE-GROUPING  = AM-VG-GROUPING                      EL529
01098              GO TO 6320-VALID-ACCOUNT.                            EL529
01099                                                                   EL529
01100  6310-INVALID-ACCOUNT.                                            EL529
01101      MOVE '**INVALID ACCOUNT**'  TO  H5-ACCOUNT-NAME              EL529
01102      MOVE EX-SE-CARRIER          TO  H5-CARRIER.                  EL529
01103      MOVE EX-SE-GROUPING         TO  H5-GROUPING.                 EL529
01104      MOVE EX-SE-STATE            TO  H5-STATE.                    EL529
01105      MOVE EX-SE-ACCOUNT          TO  H5-ACCOUNT.                  EL529
01106      GO TO 6300-EXIT.                                             EL529
01107                                                                   EL529
01108  6320-VALID-ACCOUNT.                                              EL529
01109      MOVE AM-NAME                TO  H5-ACCOUNT-NAME.             EL529
01110      MOVE EX-SE-CARRIER          TO  H5-CARRIER.                  EL529
01111      MOVE EX-SE-GROUPING         TO  H5-GROUPING.                 EL529
01112      MOVE EX-SE-STATE            TO  H5-STATE.                    EL529
01113      MOVE AM-ACCOUNT             TO  H5-ACCOUNT.                  EL529
01114                                                                   EL529
01115  6300-EXIT.                                                       EL529
01116      EXIT.                                                        EL529
01117      EJECT                                                        EL529
01118                                                                   EL529
01119  7000-CHECK-PRINT-OPTIONS SECTION.                                EL529
01120 *                                                                 EL529
01121 *                                                                 EL529
01122 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      EL529
01127                                                                   EL529
01128      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          EL529
01129      MOVE EX-COMPANY-ID          TO  CF-COMPANY-ID.               EL529
01130      MOVE '1'                    TO  CF-RECORD-TYPE.              EL529
01131      MOVE SPACES                 TO  CF-ACCESS-CD-GENL.           EL529
01132      MOVE +0                     TO  CF-SEQUENCE-NO.              EL529
01133                                                                   EL529
01134      READ ELCNTL.                                                 EL529
01135                                                                   EL529
01136      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL529
01137          MOVE 'ERROR OCCURED READ INITIAL - ELCNTL'               EL529
01138                                  TO  WS-ABEND-MESSAGE             EL529
01139          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01140          GO TO ABEND-PGM.                                         EL529
01141                                                                   EL529
01142      MOVE CF-CL-MAIL-TO-NAME     TO  WS-COMPANY-NAME              EL529
01143                                      WS-H2-CLIENT-NAME.           EL529
01144      MOVE CF-COMPANY-ID          TO  WS-COMPANY-ID.               EL529
01145      MOVE CF-COMPANY-CD          TO  WS-COMPANY-CD.               EL529
01146      MOVE LOW-VALUES             TO  WS-LAST-CARRIER.             EL529
01147      ACCEPT LCP-TIME-OF-DAY-74 FROM TIME                          EL529
01148      MOVE LCP-TIME-74 TO LCP-TIME-OF-DAY-68                       EL529
01149                                                                   EL529
01150      MOVE  LCP-TIME-OF-DAY-68 TO WS-DISPLAY-TIME.                 EL529
01151      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'.            EL529
01152      DISPLAY 'BEGIN PROCESSING ' WS-H2-CLIENT-NAME ' AT '         EL529
01153              WS-DISPLAY-TIME UPON CONSOLE.                        EL529
01154                                                                   EL529
01155      MOVE SPACES                 TO  WS-COMPANY-NAME2.            EL529
01156      SET CN1 TO +30.                                              EL529
01157                                                                   EL529
01158  7020-PROCESS.                                                    EL529
01159      IF WS-CN-CHAR (CN1) = SPACES                                 EL529
01160          IF CN1 GREATER THAN +1                                   EL529
01161              SET CN1 DOWN BY +1                                   EL529
01162              GO TO 7020-PROCESS                                   EL529
01163          ELSE                                                     EL529
01164              GO TO 7040-PROCESS.                                  EL529
01165                                                                   EL529
01166      SET WS-LENGTH TO CN1.                                        EL529
01167                                                                   EL529
01168      SUBTRACT WS-LENGTH FROM +30 GIVING WS-LENGTH.                EL529
01169      DIVIDE +2 INTO WS-LENGTH ROUNDED.                            EL529
01170                                                                   EL529
01171      IF WS-LENGTH NOT GREATER THAN ZERO                           EL529
01172          GO TO 7040-PROCESS.                                      EL529
01173                                                                   EL529
01174      SET CN2 TO CN1.                                              EL529
01175      SET CN2 UP BY WS-LENGTH.                                     EL529
01176                                                                   EL529
01177  7030-PROCESS.                                                    EL529
01178      MOVE WS-CN-CHAR (CN1) TO WS-CN2-CHAR (CN2).                  EL529
01179                                                                   EL529
01180      IF CN1 GREATER THAN +1                                       EL529
01181          SET CN1                                                  EL529
01182              CN2 DOWN BY +1                                       EL529
01183          GO TO 7030-PROCESS.                                      EL529
01184                                                                   EL529
01185      MOVE WS-COMPANY-NAME2       TO  WS-COMPANY-NAME.             EL529
01186                                                                   EL529
01187                                                                   EL529
01188  7040-PROCESS.                                                    EL529
01189      MOVE WS-COMPANY-CD          TO  PS-COMPANY-CD.               EL529
01190      MOVE WS-REPORT-ID           TO  PS-PROGRAM-NUMBER.           EL529
01191                                                                   EL529
01192      READ ELPGMS.                                                 EL529
01193                                                                   EL529
01194      IF ELPGMS-FILE-STATUS = '23'                                 EL529
01195          MOVE SPACES             TO  WS-PROGRAM-OPTIONS           EL529
01196          GO TO 7099-EXIT.                                         EL529
01197                                                                   EL529
01198      IF ELPGMS-FILE-STATUS NOT = ZERO                             EL529
01199          MOVE 'ERROR OCCURED READ - ELPGMS'                       EL529
01200                                  TO  WS-ABEND-MESSAGE             EL529
01201          MOVE ELPGMS-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01202          GO TO ABEND-PGM.                                         EL529
01203                                                                   EL529
01204      MOVE PS-PROGRAM-OPTIONS (1) TO  WS-PROGRAM-OPTIONS.          EL529
01205                                                                   EL529
01206      IF WS-FREQUENCY-OPTION NOT = 'NONE' OR                       EL529
01207         WS-PROCESS-OPTION = 'X'                                   EL529
01208          GO TO 7099-EXIT.                                         EL529
01209                                                                   EL529
01210  7050-PROCESS.                                                    EL529
01211      IF PRNTR-NOT-OPEN                                            EL529
01212          IF WS-PRINT-OPTION = ('P' OR 'B' OR 'T')                 EL529
01213              OPEN OUTPUT PRNTR                                    EL529
01214              MOVE +1             TO  WS-PRNTR-STATUS-SW           EL529
01215              MOVE +0             TO  WS-PAGE                      EL529
01216          ELSE                                                     EL529
01217              NEXT SENTENCE                                        EL529
01218      ELSE                                                         EL529
01219          CLOSE PRNTR                                              EL529
01220          MOVE +0                 TO  WS-PRNTR-STATUS-SW           EL529
01221          GO TO 7050-PROCESS.                                      EL529
01222                                                                   EL529
01223      IF WS-PRINT-OPTION = ('F' OR 'B')                            EL529
01224        AND FICHE-NOT-OPEN                                         EL529
01225          OPEN OUTPUT FICH                                         EL529
01226          MOVE +1                 TO  WS-FICHE-STATUS-SW.          EL529
01227                                                                   EL529
01228      IF WS-PRINT-OPTION = 'S' OR 'T'                              EL529
01229          NEXT SENTENCE                                            EL529
01230      ELSE                                                         EL529
01231          GO TO 7099-EXIT.                                         EL529
01232                                                                   EL529
01233      MOVE ZERO                   TO  WS-LINE-NUMBER.              EL529
01234                                                                   EL529
01235      IF ELREPT-NOT-OPEN                                           EL529
01236          OPEN I-O ELREPT                                          EL529
01237          MOVE +1                 TO  WS-ELREPT-STATUS-SW          EL529
01238          IF ELREPT-FILE-STATUS  = '00' OR '97'                    EL529
01239             NEXT SENTENCE                                         EL529
01240          ELSE                                                     EL529
01241              MOVE 'ELREPT  '     TO  WS-FEM-FILE-NAME             EL529
01242              MOVE WS-FILE-ERROR-MESSAGE  TO  WS-ABEND-MESSAGE     EL529
01243              MOVE ELREPT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS    EL529
01244              GO TO ABEND-PGM.                                     EL529
01245                                                                   EL529
01246      MOVE ZERO                   TO  WS-START-SW.                 EL529
01247      MOVE WS-COMPANY-CD          TO  RF-COMPANY-CD.               EL529
01248      MOVE '1'                    TO  RF-RECORD-TYPE.              EL529
01249      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL529
01250      MOVE ZERO                   TO  RF-LINE-NUMBER.              EL529
01251                                                                   EL529
01252  7060-PRINT-REPORT.                                               EL529
01253      START ELREPT                                                 EL529
01254          KEY IS NOT LESS THAN RF-CONTROL-PRIMARY.                 EL529
01255                                                                   EL529
01256      IF ELREPT-FILE-STATUS = '10' OR '23'                         EL529
01257          GO TO 7090-PRINT-REPORT.                                 EL529
01258                                                                   EL529
01259      IF ELREPT-FILE-STATUS NOT = ZERO                             EL529
01260          MOVE 'ERROR OCCURED START ELREPT'  TO  WS-ABEND-MESSAGE  EL529
01261          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01262          GO TO ABEND-PGM.                                         EL529
01263                                                                   EL529
01264  7070-PRINT-REPORT.                                               EL529
01265      READ ELREPT NEXT.                                            EL529
01266                                                                   EL529
01267      IF ELREPT-FILE-STATUS = '10'                                 EL529
01268          GO TO 7090-PRINT-REPORT.                                 EL529
01269                                                                   EL529
01270      IF ELREPT-FILE-STATUS NOT = ZERO                             EL529
01271          MOVE 'ERROR OCCURED READNEXT ELREPT' TO WS-ABEND-MESSAGE EL529
01272          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01273          GO TO ABEND-PGM.                                         EL529
01274                                                                   EL529
01275      IF RF-COMPANY-CD NOT = WS-COMPANY-CD                         EL529
01276          GO TO 7090-PRINT-REPORT.                                 EL529
01277                                                                   EL529
01278      IF WS-START-SW = ZERO                                        EL529
01279          IF RF-RECORD-TYPE = '1'                                  EL529
01280              NEXT SENTENCE                                        EL529
01281          ELSE                                                     EL529
01282              GO TO 7090-PRINT-REPORT                              EL529
01283      ELSE                                                         EL529
01284          IF RF-RECORD-TYPE = '2'                                  EL529
01285              NEXT SENTENCE                                        EL529
01286          ELSE                                                     EL529
01287              GO TO 7090-PRINT-REPORT.                             EL529
01288                                                                   EL529
01289      IF RF-REPORT-ID NOT = WS-REPORT-ID                           EL529
01290          GO TO 7090-PRINT-REPORT.                                 EL529
01291                                                                   EL529
01292      DELETE ELREPT RECORD.                                        EL529
01293                                                                   EL529
01294      IF ELREPT-FILE-STATUS NOT = ZERO                             EL529
01295          MOVE 'ERROR OCCURED DELETE ELREPT' TO WS-ABEND-MESSAGE   EL529
01296          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01297          GO TO ABEND-PGM.                                         EL529
01298                                                                   EL529
01299      GO TO 7070-PRINT-REPORT.                                     EL529
01300                                                                   EL529
01301  7090-PRINT-REPORT.                                               EL529
01302      IF WS-START-SW = ZERO                                        EL529
01303          MOVE WS-COMPANY-CD      TO  RF-COMPANY-CD                EL529
01304          MOVE '2'                TO  RF-RECORD-TYPE               EL529
01305          MOVE WS-REPORT-ID       TO  RF-REPORT-ID                 EL529
01306          MOVE +0                 TO  RF-LINE-NUMBER               EL529
01307          MOVE +1                 TO  WS-START-SW                  EL529
01308          GO TO 7060-PRINT-REPORT.                                 EL529
01309                                                                   EL529
01310      MOVE WS-COMPANY-CD          TO  RF-COMPANY-CD.               EL529
01311      MOVE '1'                    TO  RF-RECORD-TYPE.              EL529
01312      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL529
01313      MOVE +0                     TO  RF-LINE-NUMBER.              EL529
01314                                                                   EL529
01315      MOVE SPACES                 TO  RF-REPORT-LINE-133.          EL529
01316                                                                   EL529
01317  7099-EXIT.                                                       EL529
01318      EXIT.                                                        EL529
01319      EJECT                                                        EL529
01320                                                                   EL529
01321  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL529
01322                                                                   EL529
01323  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL529
01324                                                                   EL529
01325  WRITE-HEADINGS SECTION.                                          EL529
01326 ***************************************************************** EL529
01327 *                            ELCWHS1.                           * EL529
01328 *                            VMOD=2.001                         * EL529
01329 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL529
01330 *****************************************************************.EL529
01331  WHS-010.                                                         EL529
01332      IF  WS-H2-DATE EQUAL SPACES                                  EL529
01333          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL529
01334          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL529
01335          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL529
01336                                                                   EL529
01337      ADD +1  TO  WS-PAGE.                                         EL529
01338      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL529
01339      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL529
01340      MOVE ZERO                   TO  WS-LINE-COUNT.               EL529
01341                                                                   EL529
01342      MOVE WS-HEADING1            TO  PRT.                         EL529
01343      MOVE '1'                    TO  X.                           EL529
01344      PERFORM WRITE-PRINTER.                                       EL529
01345                                                                   EL529
01346      MOVE WS-HEADING2            TO  PRT.                         EL529
01347      MOVE ' '                    TO  X.                           EL529
01348      PERFORM WRITE-PRINTER.                                       EL529
01349                                                                   EL529
01350      MOVE WS-HEADING3            TO  PRT.                         EL529
01351      MOVE ' '                    TO  X.                           EL529
01352      PERFORM WRITE-PRINTER.                                       EL529
01353                                                                   EL529
01354      MOVE WS-HEADING4            TO  PRT.                         EL529
01355      MOVE ' '                    TO  X.                           EL529
01356      PERFORM WRITE-PRINTER.                                       EL529
01357                                                                   EL529
01358                                                                   EL529
01359      MOVE WS-HEADING5            TO  PRT.                         EL529
01360      PERFORM WRITE-PRINTER.                                       EL529
01361                                                                   EL529
01362      MOVE WS-HEADING6            TO  PRT.                         EL529
01363      PERFORM WRITE-PRINTER.                                       EL529
01364                                                                   EL529
01365      MOVE WS-HEADING7            TO  PRT.                         EL529
01366      PERFORM WRITE-PRINTER.                                       EL529
01367                                                                   EL529
01368      MOVE +10                    TO  WS-LINE-COUNT.               EL529
01369                                                                   EL529
01370                                                                   EL529
01371  WHS-020.                    COPY ELCWHS2.                        EL529
01372                                                                   EL529
01373  WRITE-PRINTER SECTION.                                           EL529
01374                                                                   EL529
01375  WPS-010.                                                         EL529
01376      IF WS-PRINT-OPTION = ('S' OR 'T')                            EL529
01377          NEXT SENTENCE                                            EL529
01378      ELSE                                                         EL529
01379          GO TO WPS-020.                                           EL529
01380                                                                   EL529
01381      MOVE WS-COMPANY-CD          TO  RF-COMPANY-CD.               EL529
01382      MOVE '1'                    TO  RF-RECORD-TYPE.              EL529
01383      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL529
01384      ADD +1                      TO  WS-LINE-NUMBER.              EL529
01385      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              EL529
01386      MOVE PRT                    TO  RF-REPORT-LINE-133.          EL529
01387                                                                   EL529
01388      WRITE REPORT-SAVE-FILE.                                      EL529
01389                                                                   EL529
01390      IF ELREPT-FILE-STATUS NOT = ZERO                             EL529
01391          MOVE 'ERROR OCCURED WRITE ELREPT'  TO  WS-ABEND-MESSAGE  EL529
01392          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01393          GO TO ABEND-PGM.                                         EL529
01394                                                                   EL529
01395  WPS-020.                                                         EL529
01396      IF WS-PRINT-OPTION = 'F' OR 'B'                              EL529
01397          WRITE FICH-REC FROM PRT.                                 EL529
01398                                                                   EL529
01399      IF WS-PRINT-OPTION = 'P' OR 'B' OR 'T'                       EL529
01400          MOVE P-CTL TO LCP-ASA                                    EL529
01401          PERFORM LCP-WRITE-POS-PRT                                EL529
01402              THRU LCP-WRITE-END-PRT.                              EL529
01403                                                                   EL529
01404  WPS-EXIT.                                                        EL529
01405      EXIT.                                                        EL529
01406      EJECT                                                        EL529
01407                                                                   EL529
01408  OPEN-FILES SECTION.                                              EL529
01409                                                                   EL529
01410  OFS-010.                                                         EL529
01411      OPEN INPUT ERACCT                                            EL529
01412                 ELCNTL                                            EL529
01413                 ELPGMS                                            EL529
01414                 ERPNDB.                                           EL529
01415                                                                   EL529
01416      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL529
01417          NEXT SENTENCE                                            EL529
01418        ELSE                                                       EL529
01419          MOVE 'ERROR OCCURED OPEN - ELCNTL'                       EL529
01420                                  TO  WS-ABEND-MESSAGE             EL529
01421          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01422          GO TO ABEND-PGM.                                         EL529
01423                                                                   EL529
01424      IF ERPNDB-FILE-STATUS  = '00' OR '97'                        EL529
01425          NEXT SENTENCE                                            EL529
01426        ELSE                                                       EL529
01427          MOVE 'ERROR OCCURED OPEN - ERPNDB'                       EL529
01428                                  TO  WS-ABEND-MESSAGE             EL529
01429          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01430          GO TO ABEND-PGM.                                         EL529
01431                                                                   EL529
01432      IF ERACCT-FILE-STATUS  = '00' OR '97'                        EL529
01433          NEXT SENTENCE                                            EL529
01434        ELSE                                                       EL529
01435          MOVE 'ERROR OCCURED OPEN - ERACCT'                       EL529
01436                                  TO  WS-ABEND-MESSAGE             EL529
01437          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01438          GO TO ABEND-PGM.                                         EL529
01439                                                                   EL529
01440      IF ELPGMS-FILE-STATUS  = '00' OR '97'                        EL529
01441          NEXT SENTENCE                                            EL529
01442        ELSE                                                       EL529
01443          MOVE 'ERROR OCCURED CLOSE - ELPGMS'                      EL529
01444                                  TO  WS-ABEND-MESSAGE             EL529
01445          MOVE ELPGMS-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01446          GO TO ABEND-PGM.                                         EL529
01447                                                                   EL529
01448  OFS-EXIT.                                                        EL529
01449      EXIT.                                                        EL529
01450                                                                   EL529
01451  CLOSE-FILES SECTION.                                             EL529
01452                                                                   EL529
01453  CFS-010.                                                         EL529
01454      IF FICHE-OPEN                                                EL529
01455          CLOSE FICH.                                              EL529
01456                                                                   EL529
01457      IF PRNTR-OPEN                                                EL529
01458          CLOSE PRNTR.                                             EL529
01459                                                                   EL529
01460      IF ELREPT-OPEN                                               EL529
01461          CLOSE ELREPT                                             EL529
01462          IF ELREPT-FILE-STATUS NOT = ZERO                         EL529
01463              MOVE 'ERROR OCCURED CLOSE - ELREPT'                  EL529
01464                                      TO  WS-ABEND-MESSAGE         EL529
01465              MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS     EL529
01466              GO TO ABEND-PGM.                                     EL529
01467                                                                   EL529
01468      CLOSE ELCNTL                                                 EL529
01469            ERACCT                                                 EL529
01470            ELPGMS.                                                EL529
01471                                                                   EL529
01472      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL529
01473          MOVE 'ERROR OCCURED CLOSE - ELCNTL'                      EL529
01474                                  TO  WS-ABEND-MESSAGE             EL529
01475          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01476          GO TO ABEND-PGM.                                         EL529
01477                                                                   EL529
01478                                                                   EL529
01479      IF ERACCT-FILE-STATUS NOT = ZERO                             EL529
01480          MOVE 'ERROR OCCURED CLOSE - ERACCT'                      EL529
01481                                  TO  WS-ABEND-MESSAGE             EL529
01482          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01483          GO TO ABEND-PGM.                                         EL529
01484                                                                   EL529
01485      IF ELPGMS-FILE-STATUS NOT = ZERO                             EL529
01486          MOVE 'ERROR OCCURED CLOSE - ELPGMS'                      EL529
01487                                  TO  WS-ABEND-MESSAGE             EL529
01488          MOVE ELPGMS-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL529
01489          GO TO ABEND-PGM.                                         EL529
01490                                                                   EL529
01491  CFS-EXIT.                                                        EL529
01492      EXIT.                                                        EL529
01493                                                                   EL529
01494  ABEND-PGM SECTION. COPY ELCABEND.                                EL529
01495 /                                                                 EL529
01496  LCP-WRITE-POS-PRT SECTION.                                       EL529
01497      IF LCP-ASA = '+'                                             EL529
01498          WRITE PRT AFTER 0 LINE                                   EL529
01499      ELSE                                                         EL529
01500      IF LCP-ASA = ' '                                             EL529
01501          WRITE PRT AFTER ADVANCING 1 LINE                         EL529
01502      ELSE                                                         EL529
01503      IF LCP-ASA = '0'                                             EL529
01504          WRITE PRT AFTER ADVANCING 2 LINE                         EL529
01505      ELSE                                                         EL529
01506      IF LCP-ASA = '-'                                             EL529
01507          WRITE PRT AFTER ADVANCING 3 LINE                         EL529
01508      ELSE                                                         EL529
01509      IF LCP-ASA = '1'                                             EL529
01510          WRITE PRT AFTER ADVANCING PAGE                           EL529
01511      ELSE                                                         EL529
01512      IF LCP-ASA = '2'                                             EL529
01513          WRITE PRT AFTER ADVANCING LCP-CH2                        EL529
01514      ELSE                                                         EL529
01515      IF LCP-ASA = '3'                                             EL529
01516          WRITE PRT AFTER ADVANCING LCP-CH3                        EL529
01517      ELSE                                                         EL529
01518      IF LCP-ASA = '4'                                             EL529
01519          WRITE PRT AFTER ADVANCING LCP-CH4                        EL529
01520      ELSE                                                         EL529
01521      IF LCP-ASA = '5'                                             EL529
01522          WRITE PRT AFTER ADVANCING LCP-CH5                        EL529
01523      ELSE                                                         EL529
01524      IF LCP-ASA = '6'                                             EL529
01525          WRITE PRT AFTER ADVANCING LCP-CH6                        EL529
01526      ELSE                                                         EL529
01527      IF LCP-ASA = '7'                                             EL529
01528          WRITE PRT AFTER ADVANCING LCP-CH7                        EL529
01529      ELSE                                                         EL529
01530      IF LCP-ASA = '8'                                             EL529
01531          WRITE PRT AFTER ADVANCING LCP-CH8                        EL529
01532      ELSE                                                         EL529
01533      IF LCP-ASA = '9'                                             EL529
01534          WRITE PRT AFTER ADVANCING LCP-CH9                        EL529
01535      ELSE                                                         EL529
01536      IF LCP-ASA = 'A'                                             EL529
01537          WRITE PRT AFTER ADVANCING LCP-CH10                       EL529
01538      ELSE                                                         EL529
01539      IF LCP-ASA = 'B'                                             EL529
01540          WRITE PRT AFTER ADVANCING LCP-CH11                       EL529
01541      ELSE                                                         EL529
01542      IF LCP-ASA = 'C'                                             EL529
01543          WRITE PRT AFTER ADVANCING LCP-CH12                       EL529
01544      ELSE                                                         EL529
01545      IF LCP-ASA = 'V'                                             EL529
01546          WRITE PRT AFTER ADVANCING LCP-P01                        EL529
01547      ELSE                                                         EL529
01548      IF LCP-ASA = 'W'                                             EL529
01549          WRITE PRT AFTER ADVANCING LCP-P02                        EL529
01550      ELSE                                                         EL529
01551      DISPLAY 'ASA CODE ERROR'.                                    EL529
01552  LCP-WRITE-END-PRT.                                               EL529
01553      EXIT.                                                        EL529
01554                                                                   EL529
