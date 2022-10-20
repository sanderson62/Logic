00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL526
00003  PROGRAM-ID.                 EL526 .                                 LV002
00004 *              PROGRAM CONVERTED BY                               EL526
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL526
00006 *              CONVERSION DATE 04/10/96 10:07:31.                 EL526
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL526
00008 *                            VMOD=2.013.                          EL526
00009                                                                   EL526
00010 *AUTHOR.     LOGIC, INC.                                          EL526
00011 *            DALLAS, TEXAS.                                       EL526
00012                                                                   EL526
00013 *DATE-COMPILED.                                                   EL526
00014                                                                   EL526
00015 *SECURITY.   *****************************************************EL526
00016 *            *                                                   *EL526
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL526
00018 *            *                                                   *EL526
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL526
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL526
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL526
00022 *            *                                                   *EL526
00023 *            *****************************************************EL526
00024                                                                   EL526
00025 *REMARKS.                                                         EL526
00026 *        THIS PROGRAM PRINTS A REGISTER OF ALL CHECKS ISSUED      EL526
00027 *    IN THE REPORTED MONTH.   BOTH ONLINE CREATED AND OFFLINE     EL526
00028 *    CHECKS ARE REPORTED.                                         EL526
00029 *                                                                 EL526
00030 *            PROGRAM                                              EL526
00031 *             OPTION    DESCRIPTION                               EL526
00032 *                                                                 EL526
00033 *               1       MONTHLY CHECK REGISTER                    EL526
00034 *               2       DAILY CHECK REGISTER                      EL526
00035 *               3       YEAR-TO-DATE CHECK REGISTER               EL526
00036 *               4       INCEPTION-TO-DATE CHECK REGISTER          EL526
00037                                                                   EL526
00038                                                                   EL526
00039      EJECT                                                        EL526
00040  ENVIRONMENT DIVISION.                                            EL526
00041                                                                   EL526
00042  INPUT-OUTPUT SECTION.                                            EL526
00043                                                                   EL526
00044  FILE-CONTROL.                                                    EL526
00045                                                                   EL526
00046      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL526
00047                                                                   EL526
00048      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL526
00049                                                                   EL526
00050      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL526
00051                                                                   EL526
00052      SELECT ABC-INTERFACE-FILE ASSIGN TO SYS021-UT-FBA1-S-SYS021. EL526
00053                                                                   EL526
00054      SELECT ELCNTL           ASSIGN TO SYS022-FBA1-ELCNTL         EL526
00055                              ORGANIZATION IS INDEXED              EL526
00056                              ACCESS IS DYNAMIC                    EL526
00057                              RECORD KEY IS CF-CONTROL-PRIMARY     EL526
00058                              FILE STATUS IS ELCNTL-FILE-STATUS.   EL526
00059                                                                   EL526
00060      SELECT ERCHKQ           ASSIGN TO SYS018-FBA1-ERCHKQ         EL526
00061                              ORGANIZATION IS INDEXED              EL526
00062                              ACCESS IS DYNAMIC                    EL526
00063                              RECORD KEY IS CQ-CONTROL-PRIMARY     EL526
00064                              FILE STATUS IS ERCHKQ-FILE-STATUS.   EL526
00065                                                                   EL526
00066      SELECT ERCHEK           ASSIGN TO SYS025-FBA1-ERCHEK         EL526
00067                              ORGANIZATION IS INDEXED              EL526
00068                              ACCESS IS DYNAMIC                    EL526
00069                              RECORD KEY IS CH-CONTROL-PRIMARY     EL526
00070                              FILE STATUS IS ERCHEK-FILE-STATUS.   EL526
00071                                                                   EL526
00072      SELECT ERCOMP           ASSIGN TO SYS023-FBA1-ERCOMP         EL526
00073                              ORGANIZATION IS INDEXED              EL526
00074                              ACCESS IS DYNAMIC                    EL526
00075                              RECORD KEY IS CO-CONTROL-PRIMARY     EL526
00076                              FILE STATUS IS ERCOMP-FILE-STATUS.   EL526
00077                                                                   EL526
00078      SELECT ELREPT           ASSIGN TO SYS024-FBA1-ELREPT         EL526
00079                              ORGANIZATION IS INDEXED              EL526
00080                              ACCESS IS DYNAMIC                    EL526
00081                              RECORD KEY IS RF-CONTROL-PRIMARY     EL526
00082                              FILE STATUS IS DTE-VSAM-FLAGS.       EL526
00083                                                                   EL526
00084      SELECT SORT-FILE        ASSIGN TO SYS001-FBA1-S-SORTWK1.     EL526
00085                                                                   EL526
00086      EJECT                                                        EL526
00087  DATA DIVISION.                                                   EL526
00088                                                                   EL526
00089  FILE SECTION.                                                    EL526
00090                                                                   EL526
00091  FD  ABC-INTERFACE-FILE                                           EL526
00092      BLOCK CONTAINS 0 RECORDS
00093      RECORDING MODE F.                                            EL526
00094                                                                   EL526
00095  01  ABC-INTERFACE-RECORD.                                        EL526
00096      05  ABC-CONSTANT                PIC X(11).                   EL526
00097      05  ABC-DATE                    PIC X(6).                    EL526
00098      05  ABC-ACCOUNT                 PIC X(7).                    EL526
00099 *        88  LIFE BENEFITS                        VALUE '5071000'.EL526
00100 *        88  A&H BENEFITS                         VALUE '5072000'.EL526
00101 *        88  CASH                                 VALUE '1266000'.EL526
00102                                                                   EL526
00103      05  FILLER                      PIC X(21).                   EL526
00104      05  ABC-DESC                    PIC X(20).                   EL526
00105      05  ABC-AMOUNT                  PIC 9(8)V99.                 EL526
00106      05  ABC-DEBIT-CREDIT            PIC X.                       EL526
00107 *      88  DEBIT                                     VALUE 'D'.   EL526
00108 *      88  CREDIT                                    VALUE 'C'.   EL526
00109                                                                   EL526
00110      05  FILLER                      PIC X(4).                    EL526
00111                                                                   EL526
00112      EJECT                                                        EL526
00113  FD  DISK-DATE               COPY ELCDTEFD.                       EL526
00114                                                                   EL526
00115  FD  PRNTR                   COPY ELCPRTFD.                       EL526
00116                                                                   EL526
00117  FD  FICH                    COPY ELCFCHFD.                       EL526
00118                                                                   EL526
00119  FD  ELCNTL.                                                      EL526
00120                                                                   EL526
00121                                  COPY ELCCNTL.                    EL526
00122                                                                   EL526
00123      EJECT                                                        EL526
00124  FD  ERCOMP.                                                      EL526
00125                                                                   EL526
00126                                  COPY ERCCOMP.                    EL526
00127                                                                   EL526
00128      EJECT                                                        EL526
00129  FD  ERCHKQ.                                                      EL526
00130                                                                   EL526
00131                                  COPY ERCCHKQ.                    EL526
00132                                                                   EL526
00133      EJECT                                                        EL526
00134  FD  ERCHEK.                                                      EL526
00135                                                                   EL526
00136                                  COPY ERCCHEK.                    EL526
00137                                                                   EL526
00138      EJECT                                                        EL526
00139                                                                   EL526
00140  FD  ELREPT                      COPY ELCRPTFD.                   EL526
00141                                                                   EL526
00142                                  COPY ELCREPT.                    EL526
00143                                                                   EL526
00144      EJECT                                                        EL526
00145  SD  SORT-FILE.                                                   EL526
00146                                                                   EL526
00147  01  SORT-RECORD.                                                 EL526
00148      05  SR-CONTROL-PRIMARY.                                      EL526
00149          10  SR-CARRIER               PIC X.                      EL526
00150          10  SR-CONTROL-NUMBER        PIC S9(8)     COMP.         EL526
00151          10  SR-CHECK-NO              PIC X(7).                   EL526
00152      05  SR-CK-AMOUNT                 PIC S9(7)V99.               EL526
00153      05  SR-CK-WRITTEN-DT             PIC XX.                     EL526
00154      05  SR-CK-PAYMENT-TYPE           PIC X.                      EL526
00155          88  BILLING-CREDIT                      VALUE '1'.       EL526
00156          88  REFUND-PAYMENT                      VALUE '2'.       EL526
00157          88  CHECK-MAINT-PMT                     VALUE '3'.       EL526
00158      05  SR-CK-PAYEE-NAME             PIC X(30).                  EL526
00159      05  SR-CK-GROUP                  PIC X(6).                   EL526
00160      05  SR-CK-FIN-RESP               PIC X(10).                  EL526
00161                                                                   EL526
00162      05  SR-CK-STATE-X                REDEFINES                   EL526
00163          SR-CK-FIN-RESP.                                          EL526
00164          10  FILLER                   PIC X(6).                   EL526
00165          10  SR-CK-STATE              PIC XX.                     EL526
00166          10  FILLER                   PIC XX.                     EL526
00167                                                                   EL526
00168      05  SR-CK-ACCOUNT                PIC X(10).                  EL526
00169      05  SR-CK-ENTRY-TYPE             PIC X.                      EL526
00170          88  CHECK-IS-MANUAL                     VALUE 'M'.       EL526
00171          88  CHECK-IS-VOIDED                     VALUE 'V'.       EL526
00172      05  SR-CK-CERT-NUMBER            PIC X(11).                  EL526
00173                                                                   EL526
00174      EJECT                                                        EL526
00175  WORKING-STORAGE SECTION.                                         EL526
00176  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL526
00177  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   EL526
00178                                                                   EL526
00179  77  FILLER  PIC X(32)   VALUE '********************************'.EL526
00180  77  FILLER  PIC X(32)   VALUE '*     EL526  WORKING STORAGE   *'.EL526
00181  77  FILLER  PIC X(32)   VALUE '******** VMOD=2.013 ************'.EL526
00182                                                                   EL526
00183  01  FILLER                          COMP-3.                      EL526
00184      05  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       EL526
00185      05  WS-LINE-COUNT-MAX           PIC S9(3)   VALUE +60.       EL526
00186      05  WS-PAGE                     PIC S9(5)   VALUE ZERO.      EL526
00187      05  WS-RECORD-COUNT             PIC S9(9)   VALUE ZERO.      EL526
00188      05  WS-RETURN-CODE              PIC S9(3)   VALUE ZERO.      EL526
00189      05  WS-ZERO                     PIC S9      VALUE ZERO.      EL526
00190      05  WS-NO-RECORDS-RELEASED      PIC S9(5)   VALUE ZERO.      EL526
00191                                                                   EL526
00192      05  WS-INCURRED-AGE             PIC S9(3)   VALUE ZERO.      EL526
00193      05  WS-YEAR                     REDEFINES                    EL526
00194          WS-INCURRED-AGE             PIC S9(3).                   EL526
00195                                                                   EL526
00196      05  WS-AMOUNT                   PIC S9(7)V99 VALUE ZERO.     EL526
00197                                                                   EL526
00198      EJECT                                                        EL526
00199  01  FILLER                          COMP SYNC.                   EL526
00200      05  PGM-SUB                     PIC S9(4)   VALUE +526.      EL526
00201      05  WS-INDEX                    PIC S9(4)   VALUE ZERO.      EL526
00202      05  WS-LENGTH                   REDEFINES                    EL526
00203          WS-INDEX                    PIC S9(4).                   EL526
00204                                                                   EL526
00205  01  FILLER.                                                      EL526
00206      05  WS-DISPLAY-TIME             PIC 99B99B99.                EL526
00207      05  ABEND-CODE                  PIC X(4).                    EL526
00208      05  ABEND-OPTION                PIC X.                       EL526
00209      05  OLC-REPORT-NAME             PIC X(5) VALUE 'EL526'.      EL526
00210      05  X                           PIC X       VALUE SPACE.     EL526
00211                                                                   EL526
00212      05  WS-CLIENT                   PIC X(3)    VALUE SPACES.    EL526
00213          88  CLIENT-POS                          VALUE 'POS'.     EL526
00214          88  CLIENT-WSL                          VALUE 'WSL'.     EL526
00215          88  CLIENT-CIM                          VALUE 'CIM'.     EL526
00216                                                                   EL526
00217      05  WS-SAVE-PRINT-RECORD        PIC X(133)  VALUE SPACES.    EL526
00218                                                                   EL526
00219      05  WS-LAST-CONTROL             PIC S9(8)   VALUE +0  COMP.  EL526
00220      05  WS-LAST-CARRIER             PIC X       VALUE SPACES.    EL526
00221                                                                   EL526
00222      05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL526
00223                                                                   EL526
00224      05  WS-LAST-MONTH               PIC 99      VALUE ZERO.      EL526
00225      05  WS-LAST-MONTH-X             REDEFINES                    EL526
00226          WS-LAST-MONTH               PIC XX.                      EL526
00227                                                                   EL526
00228      05  WS-MONTH                    PIC XX      VALUE ZERO.      EL526
00229      05  WS-BIN-RUN-DATE             PIC XX      VALUE LOW-VALUES.EL526
00230                                                                   EL526
00231      05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL526
00232      05  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      EL526
00233      05  ERCOMP-FILE-STATUS          PIC XX      VALUE ZERO.      EL526
00234      05  ERCHKQ-FILE-STATUS          PIC XX      VALUE ZERO.      EL526
00235      05  ERCHEK-FILE-STATUS          PIC XX      VALUE ZERO.      EL526
00236                                                                   EL526
00237      05  WS-FILE-ERROR-MESSAGE.                                   EL526
00238          10  FILLER                  PIC X(24)   VALUE            EL526
00239              'ERROR OCCURED OPENING - '.                          EL526
00240          10  WS-FEM-FILE-NAME        PIC X(8).                    EL526
00241                                                                   EL526
00242      05  WS-COMPANY-ID               PIC X(3).                    EL526
00243      05  WS-COMPANY-CD               PIC X.                       EL526
00244                                                                   EL526
00245      05  WS-COMPANY-NAME.                                         EL526
00246          10  WS-CN-CHAR              PIC X                        EL526
00247              OCCURS 30 TIMES         INDEXED BY CN1.              EL526
00248                                                                   EL526
00249      05  WS-COMPANY-NAME2.                                        EL526
00250          10  WS-CN2-CHAR             PIC X                        EL526
00251              OCCURS 30 TIMES         INDEXED BY CN2.              EL526
00252      05  WS-INITIALS.                                             EL526
00253          10  WS-INITIAL1             PIC X.                       EL526
00254          10  WS-INITIAL2             PIC X.                       EL526
00255                                                                   EL526
00256      05  WS-PHONETIC-WORK-AREA.                                   EL526
00257          10  WS-PWA-PHONETIC-NAME    PIC X(4).                    EL526
00258          10  WS-PWA-NAME             PIC X(16).                   EL526
00259          10  WS-PWA-LANGUAGE         PIC X.                       EL526
00260                                                                   EL526
00261      05  WS-BIN-DATE-WORK-X.                                      EL526
00262          10  WS-BIN-DATE-WORK        PIC S9(4)                    EL526
00263                                      COMP.                        EL526
00264                                                                   EL526
00265      05  WS-DATE-WORK.                                            EL526
00266          10  WS-DW-MONTH             PIC 99.                      EL526
00267          10  FILLER                  PIC X.                       EL526
00268          10  WS-DW-DAY               PIC 99.                      EL526
00269          10  FILLER                  PIC X.                       EL526
00270          10  WS-DW-YEAR              PIC 99.                      EL526
00271                                                                   EL526
00272      05  WS-DATE-PAID.                                            EL526
00273          10  WS-MONTH-PAID           PIC 99.                      EL526
00274          10  FILLER                  PIC X.                       EL526
00275          10  WS-DAY-PAID             PIC 99.                      EL526
00276          10  FILLER                  PIC X.                       EL526
00277          10  WS-YEAR-PAID            PIC 99.                      EL526
00278      EJECT                                                        EL526
00279  01  WS-3800-CARD.                                                EL526
00280      12  3800-LITERAL            PIC X(6)    VALUE '0LG00 '.      EL526
00281      12  3800-DATE.                                               EL526
00282          16  3800-MONTH          PIC 99.                          EL526
00283          16  3800-DAY            PIC 99.                          EL526
00284          16  3800-YEAR           PIC 99.                          EL526
00285      12  3800-VOUCHER-ID         PIC X(5)    VALUE '*****'.       EL526
00286      12  3800-JULIAN-DATE.                                        EL526
00287          16  3800-JULIAN-YEAR    PIC 99.                          EL526
00288          16  3800-JULIAN-DAY     PIC S999.                        EL526
00289      12  3800-GENERAL-LEDGER     PIC X(7).                        EL526
00290      12  3800-U-LITERAL          PIC X       VALUE 'U'.           EL526
00291      12  3800-AMOUNT             PIC 9(7)V99.                     EL526
00292      12  3800-DEBIT-CREDIT       PIC X.                           EL526
00293      12  3800-CHECK-NUMBER       PIC 9(6).                        EL526
00294      12  3800-DESCRIPTION.                                        EL526
00295          16  3800-ACCOUNT        PIC X(6).                        EL526
00296          16  3800-CERT-NO        PIC X(6)    JUSTIFIED RIGHT.     EL526
00297          16  3800-DESC-L-H       PIC X.                           EL526
00298          16  3800-DESC-C-U-P     PIC X       VALUE 'C'.           EL526
00299          16  FILLER              PIC X(4)    VALUE SPACES.        EL526
00300          16  3800-SYSTEM         PIC X       VALUE 'C'.           EL526
00301      12  FILLER                  PIC X(5)    VALUE SPACES.        EL526
00302      12  3800-STATE              PIC XX      VALUE SPACES.        EL526
00303      12  3800-COMPANY            PIC XXX.                         EL526
00304      12  FILLER                  PIC X       VALUE SPACES.        EL526
00305      12  3800-MISC-MONTH-YEAR    PIC X(4)    VALUE '0000'.        EL526
00306                                                                   EL526
00307      EJECT                                                        EL526
00308  01  TOTALS-WORK-AREA.                                            EL526
00309    02  TOTAL1.                                                    EL526
00310      05  WS-T1-DESC                      PIC X(30)   VALUE        EL526
00311          'REFUNDS'.                                               EL526
00312      05  FILLER                          COMP-3.                  EL526
00313          10  WS-T1-REFUND-CNT            PIC S9(7)     VALUE +0.  EL526
00314          10  WS-T1-REFUND-AMT            PIC S9(9)V99  VALUE +0.  EL526
00315          10  WS-T1-CAR-REFUND-CNT        PIC S9(7)     VALUE +0.  EL526
00316          10  WS-T1-CAR-REFUND-AMT        PIC S9(9)V99  VALUE +0.  EL526
00317          10  WS-T1-TOT-REFUND-CNT        PIC S9(7)     VALUE +0.  EL526
00318          10  WS-T1-TOT-REFUND-AMT        PIC S9(9)V99  VALUE +0.  EL526
00319                                                                   EL526
00320    02  TOTAL2.                                                    EL526
00321      05  WS-T2-DESC                      PIC X(30)   VALUE        EL526
00322          'VOIDED REFUNDS'.                                        EL526
00323      05  FILLER                          COMP-3.                  EL526
00324          10  WS-T2-REFUND-VOID-CNT       PIC S9(7)     VALUE +0.  EL526
00325          10  WS-T2-REFUND-VOID-AMT       PIC S9(9)V99  VALUE +0.  EL526
00326          10  WS-T2-CAR-REFUND-VOID-CNT   PIC S9(7)     VALUE +0.  EL526
00327          10  WS-T2-CAR-REFUND-VOID-AMT   PIC S9(9)V99  VALUE +0.  EL526
00328          10  WS-T2-TOT-REFUND-VOID-CNT   PIC S9(7)     VALUE +0.  EL526
00329          10  WS-T2-TOT-REFUND-VOID-AMT   PIC S9(9)V99  VALUE +0.  EL526
00330                                                                   EL526
00331    02  TOTAL3.                                                    EL526
00332      05  WS-T3-DESC                      PIC X(30)   VALUE        EL526
00333          'NET REFUNDS'.                                           EL526
00334      05  FILLER                          COMP-3.                  EL526
00335          10  WS-T3-NET-REFUND-CNT        PIC S9(7)     VALUE +0.  EL526
00336          10  WS-T3-NET-REFUND-AMT        PIC S9(9)V99  VALUE +0.  EL526
00337          10  WS-T3-CAR-NET-REFUND-CNT    PIC S9(7)     VALUE +0.  EL526
00338          10  WS-T3-CAR-NET-REFUND-AMT    PIC S9(9)V99  VALUE +0.  EL526
00339          10  WS-T3-TOT-NET-REFUND-CNT    PIC S9(7)     VALUE +0.  EL526
00340          10  WS-T3-TOT-NET-REFUND-AMT    PIC S9(9)V99  VALUE +0.  EL526
00341                                                                   EL526
00342    02  TOTAL4.                                                    EL526
00343      05  WS-T4-DESC                      PIC X(30)   VALUE        EL526
00344          'PAYMENTS'.                                              EL526
00345      05  FILLER                          COMP-3.                  EL526
00346          10  WS-T4-PMTS-CNT              PIC S9(7)     VALUE +0.  EL526
00347          10  WS-T4-PMTS-AMT              PIC S9(9)V99  VALUE +0.  EL526
00348          10  WS-T4-CAR-PMTS-CNT          PIC S9(7)     VALUE +0.  EL526
00349          10  WS-T4-CAR-PMTS-AMT          PIC S9(9)V99  VALUE +0.  EL526
00350          10  WS-T4-TOT-PMTS-CNT          PIC S9(7)     VALUE +0.  EL526
00351          10  WS-T4-TOT-PMTS-AMT          PIC S9(9)V99  VALUE +0.  EL526
00352                                                                   EL526
00353    02  TOTAL5.                                                    EL526
00354      05  WS-T5-DESC                      PIC X(30)   VALUE        EL526
00355          'VOIDED PAYMENTS'.                                       EL526
00356      05  FILLER                          COMP-3.                  EL526
00357          10  WS-T5-PMTS-VOID-CNT         PIC S9(7)     VALUE +0.  EL526
00358          10  WS-T5-PMTS-VOID-AMT         PIC S9(9)V99  VALUE +0.  EL526
00359          10  WS-T5-CAR-PMTS-VOID-CNT     PIC S9(7)     VALUE +0.  EL526
00360          10  WS-T5-CAR-PMTS-VOID-AMT     PIC S9(9)V99  VALUE +0.  EL526
00361          10  WS-T5-TOT-PMTS-VOID-CNT     PIC S9(7)     VALUE +0.  EL526
00362          10  WS-T5-TOT-PMTS-VOID-AMT     PIC S9(9)V99  VALUE +0.  EL526
00363                                                                   EL526
00364    02  TOTAL6.                                                    EL526
00365      05  WS-T6-DESC                      PIC X(30)   VALUE        EL526
00366          'NET PAYMENTS'.                                          EL526
00367      05  FILLER                          COMP-3.                  EL526
00368          10  WS-T6-NET-PMTS-CNT          PIC S9(7)     VALUE +0.  EL526
00369          10  WS-T6-NET-PMTS-AMT          PIC S9(9)V99  VALUE +0.  EL526
00370          10  WS-T6-CAR-NET-PMTS-CNT      PIC S9(7)     VALUE +0.  EL526
00371          10  WS-T6-CAR-NET-PMTS-AMT      PIC S9(9)V99  VALUE +0.  EL526
00372          10  WS-T6-TOT-NET-PMTS-CNT      PIC S9(7)     VALUE +0.  EL526
00373          10  WS-T6-TOT-NET-PMTS-AMT      PIC S9(9)V99  VALUE +0.  EL526
00374                                                                   EL526
00375    02  TOTAL7.                                                    EL526
00376      05  WS-T7-DESC                      PIC X(30)   VALUE        EL526
00377          'MAINT PAYMENTS'.                                        EL526
00378      05  FILLER                          COMP-3.                  EL526
00379          10  WS-T7-MAINT-CNT             PIC S9(7)     VALUE +0.  EL526
00380          10  WS-T7-MAINT-AMT             PIC S9(9)V99  VALUE +0.  EL526
00381          10  WS-T7-CAR-MAINT-CNT         PIC S9(7)     VALUE +0.  EL526
00382          10  WS-T7-CAR-MAINT-AMT         PIC S9(9)V99  VALUE +0.  EL526
00383          10  WS-T7-TOT-MAINT-CNT         PIC S9(7)     VALUE +0.  EL526
00384          10  WS-T7-TOT-MAINT-AMT         PIC S9(9)V99  VALUE +0.  EL526
00385                                                                   EL526
00386    02  TOTAL8.                                                    EL526
00387      05  WS-T8-DESC                      PIC X(30)   VALUE        EL526
00388          'VOIDED MAINT PAYMENTS'.                                 EL526
00389      05  FILLER                          COMP-3.                  EL526
00390          10  WS-T8-MAINT-VOID-CNT        PIC S9(7)     VALUE +0.  EL526
00391          10  WS-T8-MAINT-VOID-AMT        PIC S9(9)V99  VALUE +0.  EL526
00392          10  WS-T8-CAR-MAINT-VOID-CNT    PIC S9(7)     VALUE +0.  EL526
00393          10  WS-T8-CAR-MAINT-VOID-AMT    PIC S9(9)V99  VALUE +0.  EL526
00394          10  WS-T8-TOT-MAINT-VOID-CNT    PIC S9(7)     VALUE +0.  EL526
00395          10  WS-T8-TOT-MAINT-VOID-AMT    PIC S9(9)V99  VALUE +0.  EL526
00396                                                                   EL526
00397    02  TOTAL9.                                                    EL526
00398      05  WS-T9-DESC                      PIC X(30)   VALUE        EL526
00399          'NET MAINT PAYMENTS'.                                    EL526
00400      05  FILLER                          COMP-3.                  EL526
00401          10  WS-T9-NET-MAINT-CNT         PIC S9(7)     VALUE +0.  EL526
00402          10  WS-T9-NET-MAINT-AMT         PIC S9(9)V99  VALUE +0.  EL526
00403          10  WS-T9-CAR-NET-MAINT-CNT     PIC S9(7)     VALUE +0.  EL526
00404          10  WS-T9-CAR-NET-MAINT-AMT     PIC S9(9)V99  VALUE +0.  EL526
00405          10  WS-T9-TOT-NET-MAINT-CNT     PIC S9(7)     VALUE +0.  EL526
00406          10  WS-T9-TOT-NET-MAINT-AMT     PIC S9(9)V99  VALUE +0.  EL526
00407                                                                   EL526
00408                                                                   EL526
00409  01  WS-TOTALS-WORK-AREA             REDEFINES                    EL526
00410      TOTALS-WORK-AREA.                                            EL526
00411                                                                   EL526
00412    02  TOTAL-WORK    OCCURS 9 TIMES                               EL526
00413                      INDEXED BY TOT-INDX.                         EL526
00414      05  TOTAL-DESC                  PIC X(30).                   EL526
00415      05  CONTROL-CNT                 PIC S9(7)     COMP-3.        EL526
00416      05  CONTROL-AMT                 PIC S9(9)V99  COMP-3.        EL526
00417      05  CARRIER-CNT                 PIC S9(7)     COMP-3.        EL526
00418      05  CARRIER-AMT                 PIC S9(9)V99  COMP-3.        EL526
00419      05  TOTAL-CNT                   PIC S9(7)     COMP-3.        EL526
00420      05  TOTAL-AMT                   PIC S9(9)V99  COMP-3.        EL526
00421                                                                   EL526
00422  01  TOT-INDX-MAX                    PIC S9(4)   VALUE +9         EL526
00423                                      COMP SYNC.                   EL526
00424      EJECT                                                        EL526
00425  01  WS-HEADING1.                                                 EL526
00426      05  FILLER                      PIC X(49)   VALUE '1'.       EL526
00427      05  WS-H1-TITLE                 PIC X(71)   VALUE            EL526
00428          'MONTHLY CHECK REGISTER'.                                EL526
00429      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL526  '.    EL526
00430                                                                   EL526
00431  01  WS-HEADING2.                                                 EL526
00432      05  FILLER                      PIC X(12)   VALUE            EL526
00433          '  CARRIER -'.                                           EL526
00434      05  WS-H2-CARRIER               PIC X       VALUE SPACES.    EL526
00435      05  FILLER                      PIC X(32)   VALUE SPACES.    EL526
00436      05  WS-H2-CLIENT-NAME           PIC X(75)   VALUE SPACES.    EL526
00437      05  WS-H2-DATE                  PIC X(8)    VALUE SPACES.    EL526
00438                                                                   EL526
00439  01  WS-HEADING3.                                                 EL526
00440      05  FILLER                      PIC XX      VALUE SPACES.    EL526
00441      05  FILLER                      PIC X(30)   VALUE SPACES.    EL526
00442      05  FILLER                      PIC X(21)   VALUE SPACES.    EL526
00443      05  WS-H3-DATE                  PIC X(58)   VALUE SPACES.    EL526
00444      05  FILLER                      PIC X(5)    VALUE 'PAGE'.    EL526
00445      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL526
00446      05  FILLER                      PIC X(11)   VALUE SPACES.    EL526
00447                                                                   EL526
00448  01  WS-HEADING4.                                                 EL526
00449      05  FILLER                      PIC X(30)   VALUE            EL526
00450          '-  CHECK      AMOUNT     DATE '.                        EL526
00451      05  FILLER                      PIC X(49)   VALUE            EL526
00452          '   CONTROL   PAY TYPE         PAYEE NAME '.             EL526
00453      05  FILLER                      PIC X(62)   VALUE            EL526
00454          '   ******************  KEY DATA  ******************'.   EL526
00455                                                                   EL526
00456  01  WS-HEADING5.                                                 EL526
00457      05  FILLER                      PIC X(82)   VALUE            EL526
00458          '  NUMBER                 PAID     GROUP'.               EL526
00459      05  FILLER                      PIC X(50)   VALUE            EL526
00460          'CAR GROUP   FRE/STATE     ACCOUNT    CERT NO.'.         EL526
00461                                                                   EL526
00462  01  WS-HEADING6.                                                 EL526
00463      05  FILLER                      PIC X(13)   VALUE            EL526
00464          '0*** CONTROL '.                                         EL526
00465      05  WS-HD6-CONTROL-NUMBER       PIC 9(8).                    EL526
00466      05  FILLER                      PIC X(112)  VALUE            EL526
00467          ' TOTALS ***    COUNT       AMOUNT'.                     EL526
00468                                                                   EL526
00469  01  WS-HEADING7.                                                 EL526
00470      05  FILLER                      PIC X(133)  VALUE            EL526
00471          '0 * * * * CARRIER TOTALS * * * *    COUNT       AMOUNT'.EL526
00472                                                                   EL526
00473  01  WS-HEADING8.                                                 EL526
00474      05  FILLER                      PIC X(133)  VALUE            EL526
00475          '- * * * * FINAL TOTALS * * * * *    COUNT       AMOUNT'.EL526
00476                                                                   EL526
00477  01  WS-HEADING9.                                                 EL526
00478      05  FILLER                      PIC X(133)  VALUE            EL526
00479           '                                             CURRENT MOEL526
00480 -         'NTH'.                                                  EL526
00481                                                                   EL526
00482  01  WS-HEADING10.                                                EL526
00483      05  FILLER                      PIC X(133)  VALUE            EL526
00484           '                                         COUNT         EL526
00485 -         ' AMOUNT'.                                              EL526
00486                                                                   EL526
00487      EJECT                                                        EL526
00488  01  WS-DETAIL1.                                                  EL526
00489                                                                   EL526
00490      05  FILLER                      PIC X.                       EL526
00491      05  WS-D1-CHECK-NUMBER          PIC X(7).                    EL526
00492      05  FILLER                      PIC X.                       EL526
00493      05  WS-D1-AMOUNT                PIC Z,ZZZ,ZZ9.99-.           EL526
00494      05  FILLER                      PIC X.                       EL526
00495      05  WS-D1-DATE-PAID.                                         EL526
00496          10  WS-D1-MONTH-PAID        PIC 9(02).                   EL526
00497          10  FILLER                  PIC X.                       EL526
00498          10  WS-D1-DAY-PAID          PIC 9(02).                   EL526
00499          10  FILLER                  PIC X.                       EL526
00500          10  WS-D1-YEAR-PAID         PIC 9(02).                   EL526
00501      05  FILLER                      PIC X.                       EL526
00502      05  WS-D1-CTL-GROUP             PIC 9(8).                    EL526
00503      05  FILLER                      PIC X.                       EL526
00504      05  WS-D1-PMT-TYPE              PIC X(15).                   EL526
00505      05  FILLER                      PIC X.                       EL526
00506      05  WS-D1-PAYEE-NAME            PIC X(25).                   EL526
00507      05  FILLER                      PIC X.                       EL526
00508      05  WS-D1-CARRIER               PIC X.                       EL526
00509      05  FILLER                      PIC XX.                      EL526
00510      05  WS-D1-GROUP                 PIC X(6).                    EL526
00511      05  FILLER                      PIC X.                       EL526
00512      05  WS-D1-FIN-RESP              PIC X(10).                   EL526
00513                                                                   EL526
00514      05  FILLER                      REDEFINES                    EL526
00515          WS-D1-FIN-RESP.                                          EL526
00516          10  FILLER                  PIC X(6).                    EL526
00517          10  WS-D1-STATE             PIC XX.                      EL526
00518          10  FILLER                  PIC XX.                      EL526
00519                                                                   EL526
00520      05  FILLER                      PIC X(3).                    EL526
00521      05  WS-D1-ACCOUNT               PIC X(10).                   EL526
00522      05  FILLER                      PIC XX.                      EL526
00523      05  WS-D1-CERT-NUMBER           PIC X(11).                   EL526
00524      05  FILLER                      PIC X(4).                    EL526
00525                                                                   EL526
00526      EJECT                                                        EL526
00527  01  WS-DETAIL2                      REDEFINES                    EL526
00528      WS-DETAIL1.                                                  EL526
00529                                                                   EL526
00530      05  FILLER                      PIC X(9).                    EL526
00531      05  WS-D2-MESSAGE               PIC X(15).                   EL526
00532      05  FILLER                      PIC X(108).                  EL526
00533                                                                   EL526
00534      EJECT                                                        EL526
00535  01  WS-TOTAL-LINE1                  REDEFINES                    EL526
00536      WS-DETAIL1.                                                  EL526
00537                                                                   EL526
00538      05  WS-T1-CTL                   PIC X.                       EL526
00539      05  WS-T1-DESCRIPTION           PIC X(30).                   EL526
00540      05  FILLER                      PIC X(1).                    EL526
00541      05  WS-T1-COUNT                 PIC Z,ZZZ,ZZZ.               EL526
00542      05  FILLER                      PIC XX.                      EL526
00543      05  WS-T1-AMOUNT                PIC ZZZ,ZZZ,ZZZ.99-.         EL526
00544      05  FILLER                      PIC X(73).                   EL526
00545                                                                   EL526
00546                           COPY ELCDATE.                              CL**2
00547                                                                   EL526
00548                           COPY ELCDTECX.                          EL526
00549                                                                   EL526
00550                           COPY ELCDTEVR.                          EL526
00551                                                                   EL526
00552      EJECT                                                        EL526
00553  PROCEDURE DIVISION.                                              EL526
00554                                                                   EL526
00555  0000-DATE-CARD-READ SECTION. COPY ELCDTERX.                      EL526
00556                                                                   EL526
00557  1000-MAIN-LOGIC SECTION.                                         EL526
00558      PERFORM OPEN-FILES.                                          EL526
00559                                                                   EL526
00560      SORT SORT-FILE                                               EL526
00561          ON ASCENDING KEY SR-CONTROL-PRIMARY                      EL526
00562              INPUT  PROCEDURE 2000-SORT-INPUT-PROCEDURE           EL526
00563              OUTPUT PROCEDURE 3000-SORT-OUTPUT-PROCEDURE.         EL526
00564                                                                   EL526
00565      IF  SORT-RETURN GREATER THAN ZERO                            EL526
00566          MOVE   'SORT FAILED'  TO  WS-ABEND-MESSAGE               EL526
00567          MOVE    SORT-RETURN   TO  WS-RETURN-CODE                 EL526
00568          GO TO ABEND-PGM.                                         EL526
00569                                                                   EL526
00570      PERFORM CLOSE-FILES.                                         EL526
00571                                                                   EL526
00572      GOBACK.                                                      EL526
00573                                                                   EL526
00574  1099-EXIT.                                                       EL526
00575      EXIT.                                                        EL526
00576                                                                   EL526
00577      EJECT                                                        EL526
00578                                                                   EL526
00579  2000-SORT-INPUT-PROCEDURE SECTION.                               EL526
00580                                                                   EL526
00581      IF DTE-PRC-OPT   = '2'                                       EL526
00582          MOVE '  DAILY CHECK REGISTER' TO WS-H1-TITLE.            EL526
00583      IF DTE-PRC-OPT   = '3'                                       EL526
00584          MOVE 'YEAR-TO-DATE CHECK REGISTER' TO WS-H1-TITLE.       EL526
00585      IF DTE-PRC-OPT   = '4'                                       EL526
00586          MOVE 'INCEPTION-DATE CHECK REGISTER' TO WS-H1-TITLE.     EL526
00587                                                                   EL526
00588      MOVE    BIN-RUN-DATE        TO  WS-BIN-RUN-DATE.             EL526
00589                                                                   EL526
00590      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          EL526
00591      MOVE DTE-CLIENT             TO  CF-COMPANY-ID.               EL526
00592      MOVE '1'                    TO  CF-RECORD-TYPE.              EL526
00593      MOVE SPACES                 TO  CF-ACCESS-CD-GENL.           EL526
00594      MOVE +0                     TO  CF-SEQUENCE-NO.              EL526
00595                                                                   EL526
00596      READ ELCNTL.                                                 EL526
00597                                                                   EL526
00598      IF  ELCNTL-FILE-STATUS NOT = ZERO                            EL526
00599          MOVE    'ERROR OCCURED READ INITIAL - ELCNTL'            EL526
00600                                     TO  WS-ABEND-MESSAGE          EL526
00601          MOVE    ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS      EL526
00602          GO TO ABEND-PGM.                                         EL526
00603                                                                   EL526
00604      MOVE      CF-CL-MAIL-TO-NAME     TO  WS-COMPANY-NAME         EL526
00605                                           WS-H2-CLIENT-NAME.      EL526
00606      MOVE      CF-COMPANY-ID          TO  WS-COMPANY-ID.          EL526
00607      MOVE      CF-COMPANY-CD          TO  WS-COMPANY-CD.          EL526
00608      MOVE      LOW-VALUES             TO  WS-LAST-CARRIER.        EL526
00609                                                                   EL526
00610      ACCEPT WS-TIME-OF-DAY            FROM  TIME.                 EL526
00611                                                                   EL526
00612      MOVE      WS-TIME                TO  WS-DISPLAY-TIME.        EL526
00613      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'.            EL526
00614      DISPLAY   'BEGIN PROCESSING ' WS-H2-CLIENT-NAME ' AT '       EL526
00615                WS-DISPLAY-TIME UPON CONSOLE.                      EL526
00616                                                                   EL526
00617      MOVE SPACES                 TO  WS-COMPANY-NAME2.            EL526
00618      SET CN1 TO +30.                                              EL526
00619                                                                   EL526
00620  2020-SIP.                                                        EL526
00621      IF  WS-CN-CHAR (CN1) = SPACES                                EL526
00622          IF  CN1 GREATER THAN +1                                  EL526
00623              SET CN1 DOWN BY +1                                   EL526
00624              GO TO 2020-SIP                                       EL526
00625          ELSE                                                     EL526
00626              GO TO 2100-SIP.                                      EL526
00627                                                                   EL526
00628      SET WS-LENGTH TO CN1.                                        EL526
00629                                                                   EL526
00630      SUBTRACT WS-LENGTH FROM +30 GIVING WS-LENGTH.                EL526
00631      DIVIDE   +2 INTO WS-LENGTH ROUNDED.                          EL526
00632                                                                   EL526
00633      IF  WS-LENGTH NOT GREATER THAN ZERO                          EL526
00634          GO TO 2100-SIP.                                          EL526
00635                                                                   EL526
00636      SET CN2 TO CN1.                                              EL526
00637      SET CN2 UP BY WS-LENGTH.                                     EL526
00638                                                                   EL526
00639  2030-SIP.                                                        EL526
00640      MOVE WS-CN-CHAR (CN1)       TO WS-CN2-CHAR (CN2).            EL526
00641                                                                   EL526
00642      IF CN1 GREATER THAN +1                                       EL526
00643          SET CN1                                                  EL526
00644              CN2 DOWN BY +1                                       EL526
00645          GO TO 2030-SIP.                                          EL526
00646                                                                   EL526
00647      MOVE WS-COMPANY-NAME2       TO  WS-COMPANY-NAME.             EL526
00648                                                                   EL526
00649      EJECT                                                        EL526
00650                                                                   EL526
00651  2100-SIP.                                                        EL526
00652      MOVE LOW-VALUES             TO  CQ-CONTROL-PRIMARY.          EL526
00653      MOVE CF-COMPANY-CD          TO  CQ-COMPANY-CD.               EL526
00654                                                                   EL526
00655      START ERCHKQ                                                 EL526
00656          KEY IS GREATER THAN CQ-CONTROL-PRIMARY.                  EL526
00657                                                                   EL526
00658      IF ERCHKQ-FILE-STATUS NOT = ZERO                             EL526
00659          MOVE    'ERROR OCCURED START - ERCHKQ'                   EL526
00660                                     TO  WS-ABEND-MESSAGE          EL526
00661          MOVE    ERCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS      EL526
00662          GO TO ABEND-PGM.                                         EL526
00663                                                                   EL526
00664      EJECT                                                        EL526
00665                                                                   EL526
00666  2150-SIP.                                                        EL526
00667      READ ERCHKQ NEXT RECORD.                                     EL526
00668                                                                   EL526
00669      IF  ERCHKQ-FILE-STATUS = '10'                                EL526
00670          GO TO 2190-EXIT.                                         EL526
00671                                                                   EL526
00672      IF  ERCHKQ-FILE-STATUS NOT = ZERO                            EL526
00673          MOVE    'ERROR OCCURED READNEXT - ERCHKQ'                EL526
00674                                     TO  WS-ABEND-MESSAGE          EL526
00675          MOVE    ERCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS      EL526
00676          GO TO ABEND-PGM.                                         EL526
00677                                                                   EL526
00678      IF CF-COMPANY-CD NOT = CQ-COMPANY-CD                         EL526
00679          GO TO 2190-EXIT.                                         EL526
00680                                                                   EL526
00681      IF CHECK-ON-QUE  OR                                          EL526
00682         MANUAL-CHECK  OR                                          EL526
00683         VOIDED-CHECK                                              EL526
00684          NEXT SENTENCE                                            EL526
00685      ELSE                                                         EL526
00686          GO TO 2150-SIP.                                          EL526
00687                                                                   EL526
00688      IF CQ-TIMES-PRINTED = ZEROS                                  EL526
00689          GO TO 2150-SIP.                                          EL526
00690                                                                   EL526
00691      IF CQ-CHECK-WRITTEN-DT GREATER THAN WS-BIN-RUN-DATE          EL526
00692          GO TO 2150-SIP.                                          EL526
00693                                                                   EL526
00694      IF CQ-CHECK-VOIDED-DT GREATER THAN WS-BIN-RUN-DATE           EL526
00695          MOVE LOW-VALUES            TO CQ-CHECK-VOIDED-DT         EL526
00696          MOVE 'Q'                   TO CQ-ENTRY-TYPE              EL526
00697          MOVE SPACE                 TO CQ-VOID-INDICATOR.         EL526
00698                                                                   EL526
00699      IF VOIDED-CHECK  AND                                         EL526
00700         CQ-CHECK-VOIDED-DT GREATER THAN CQ-CHECK-WRITTEN-DT       EL526
00701          MOVE CQ-CHECK-VOIDED-DT    TO  DC-BIN-DATE-1             EL526
00702      ELSE                                                         EL526
00703          MOVE CQ-CHECK-WRITTEN-DT   TO  DC-BIN-DATE-1.            EL526
00704      MOVE    SPACES                 TO  DC-OPTION-CODE.           EL526
00705      PERFORM 8500-DATE-CONVERSION.                                EL526
00706      MOVE    DC-GREG-DATE-1-EDIT    TO  WS-DATE-WORK.             EL526
00707                                                                   EL526
00708      IF  DTE-PRC-OPT   = '4'                                      EL526
00709          GO TO 2160-SIP.                                          EL526
00710                                                                   EL526
00711      IF  DTE-PRC-OPT   = '3'                                      EL526
00712        AND WS-DW-YEAR  = RUN-YR                                   EL526
00713          GO TO 2160-SIP.                                          EL526
00714                                                                   EL526
00715      IF  DTE-PRC-OPT   = '2'                                      EL526
00716        AND WS-DW-MONTH = RUN-MO                                   EL526
00717        AND WS-DW-DAY   = RUN-DA                                   EL526
00718        AND WS-DW-YEAR  = RUN-YR                                   EL526
00719          GO TO 2160-SIP.                                          EL526
00720                                                                   EL526
00721      IF  DTE-PRC-OPT   = '1'                                      EL526
00722       AND WS-DW-MONTH = RUN-MO                                    EL526
00723       AND WS-DW-YEAR = RUN-YR                                     EL526
00724          GO TO 2160-SIP.                                          EL526
00725                                                                   EL526
00726      GO TO 2150-SIP.                                              EL526
00727                                                                   EL526
00728  2160-SIP.                                                        EL526
00729                                                                   EL526
00730      IF NOT CQ-BILLING-CREDIT                                     EL526
00731          GO TO 2170-SIP.                                          EL526
00732                                                                   EL526
00733      MOVE SPACES                 TO  SORT-RECORD.                 EL526
00734      MOVE LOW-VALUES             TO  CO-CONTROL-PRIMARY.          EL526
00735      MOVE CF-COMPANY-CD          TO  CO-COMPANY-CD.               EL526
00736      MOVE CQ-PYAJ-CARRIER        TO  CO-CARRIER                   EL526
00737                                      SR-CARRIER.                  EL526
00738      MOVE CQ-PYAJ-GROUPING       TO  CO-GROUPING                  EL526
00739                                      SR-CK-GROUP.                 EL526
00740      MOVE CQ-PYAJ-FIN-RESP       TO  CO-RESP-NO                   EL526
00741                                      SR-CK-FIN-RESP.              EL526
00742      MOVE CQ-PYAJ-ACCOUNT        TO  CO-ACCOUNT                   EL526
00743                                      SR-CK-ACCOUNT.               EL526
00744                                                                   EL526
00745      IF CQ-PYAJ-ACCOUNT = LOW-VALUES                              EL526
00746          MOVE 'G'                TO  CO-TYPE                      EL526
00747      ELSE                                                         EL526
00748          MOVE 'A'                TO  CO-TYPE.                     EL526
00749                                                                   EL526
00750      IF CF-ZERO-CARRIER  OR                                       EL526
00751         CF-ZERO-CAR-GROUP                                         EL526
00752          MOVE ZERO               TO CO-CARRIER.                   EL526
00753                                                                   EL526
00754      IF CF-ZERO-GROUPING  OR                                      EL526
00755         CF-ZERO-CAR-GROUP                                         EL526
00756          MOVE ZEROS              TO CO-GROUPING.                  EL526
00757                                                                   EL526
00758      READ ERCOMP.                                                 EL526
00759                                                                   EL526
00760      IF ERCOMP-FILE-STATUS = '23'                                 EL526
00761          MOVE SPACES             TO  SR-CK-PAYEE-NAME             EL526
00762          GO TO 2180-SIP.                                          EL526
00763                                                                   EL526
00764      IF ERCOMP-FILE-STATUS NOT = ZERO                             EL526
00765          MOVE    'ERROR OCCURED READNEXT - ERCOMP'                EL526
00766                                     TO  WS-ABEND-MESSAGE          EL526
00767          MOVE    ERCOMP-FILE-STATUS TO  WS-ABEND-FILE-STATUS      EL526
00768          GO TO ABEND-PGM.                                         EL526
00769                                                                   EL526
00770      IF  CO-MAIL-NAME = SPACES OR LOW-VALUES                      EL526
00771          MOVE CO-ACCT-NAME      TO  SR-CK-PAYEE-NAME              EL526
00772      ELSE                                                         EL526
00773          MOVE CO-MAIL-NAME      TO  SR-CK-PAYEE-NAME.             EL526
00774                                                                   EL526
00775      GO TO 2180-SIP.                                              EL526
00776                                                                   EL526
00777  2170-SIP.                                                        EL526
00778      MOVE SPACES                 TO  SORT-RECORD.                 EL526
00779      MOVE LOW-VALUES             TO  CH-CONTROL-PRIMARY.          EL526
00780      MOVE CF-COMPANY-CD          TO  CH-COMPANY-CD.               EL526
00781      MOVE CQ-CHEK-CARRIER        TO  CH-CARRIER                   EL526
00782                                      SR-CARRIER.                  EL526
00783      MOVE CQ-CHEK-GROUPING       TO  CH-GROUPING                  EL526
00784                                      SR-CK-GROUP.                 EL526
00785      MOVE CQ-CHEK-STATE          TO  CH-STATE                     EL526
00786                                      SR-CK-STATE.                 EL526
00787      MOVE CQ-CHEK-ACCOUNT        TO  CH-ACCOUNT                   EL526
00788                                      SR-CK-ACCOUNT.               EL526
00789      MOVE CQ-CHEK-CERT-EFF-DT    TO  CH-CERT-EFF-DT.              EL526
00790      MOVE CQ-CHEK-CERT-NO        TO  CH-CERT-NO                   EL526
00791                                      SR-CK-CERT-NUMBER.           EL526
00792      MOVE CQ-CHEK-SEQ-NO         TO  CH-SEQUENCE-NO.              EL526
00793                                                                   EL526
00794      READ ERCHEK.                                                 EL526
00795                                                                   EL526
00796      IF ERCHEK-FILE-STATUS = '23'                                 EL526
00797          MOVE SPACES             TO  SR-CK-PAYEE-NAME             EL526
00798          GO TO 2180-SIP.                                          EL526
00799                                                                   EL526
00800      IF ERCHEK-FILE-STATUS NOT = ZERO                             EL526
00801          MOVE    'ERROR OCCURED READNEXT - ERCHEK'                EL526
00802                                     TO  WS-ABEND-MESSAGE          EL526
00803          MOVE    ERCHEK-FILE-STATUS TO  WS-ABEND-FILE-STATUS      EL526
00804          GO TO ABEND-PGM.                                         EL526
00805                                                                   EL526
00806      IF CH-PAYEE-NAME-1 = SPACES OR LOW-VALUES                    EL526
00807          MOVE CH-PAYEE-NAME-2    TO  SR-CK-PAYEE-NAME             EL526
00808      ELSE                                                         EL526
00809          MOVE CH-PAYEE-NAME-1    TO  SR-CK-PAYEE-NAME.            EL526
00810                                                                   EL526
00811  2180-SIP.                                                        EL526
00812      MOVE CQ-CONTROL-NUMBER      TO  SR-CONTROL-NUMBER.           EL526
00813      MOVE CQ-CHECK-NUMBER        TO  SR-CHECK-NO.                 EL526
00814      MOVE CQ-CHECK-AMOUNT        TO  SR-CK-AMOUNT.                EL526
00815      MOVE CQ-PAYMENT-TYPE        TO  SR-CK-PAYMENT-TYPE.          EL526
00816      MOVE CQ-ENTRY-TYPE          TO  SR-CK-ENTRY-TYPE.            EL526
00817      MOVE CQ-CHECK-WRITTEN-DT    TO  SR-CK-WRITTEN-DT.            EL526
00818                                                                   EL526
00819      RELEASE SORT-RECORD.                                         EL526
00820                                                                   EL526
00821      ADD +1                      TO  WS-NO-RECORDS-RELEASED.      EL526
00822                                                                   EL526
00823      IF NOT VOIDED-CHECK                                          EL526
00824          GO TO 2150-SIP.                                          EL526
00825                                                                   EL526
00826      MOVE CQ-CHECK-WRITTEN-DT    TO  DC-BIN-DATE-1.               EL526
00827      MOVE SPACES                 TO  DC-OPTION-CODE.              EL526
00828      PERFORM 8500-DATE-CONVERSION.                                EL526
00829      MOVE DC-GREG-DATE-1-EDIT    TO  WS-DATE-WORK.                EL526
00830                                                                   EL526
00831      IF  DTE-PRC-OPT   = '1'                                      EL526
00832        IF WS-DW-MONTH = RUN-MO  AND                               EL526
00833           WS-DW-YEAR = RUN-YR                                     EL526
00834            NEXT SENTENCE                                          EL526
00835        ELSE                                                       EL526
00836            GO TO 2150-SIP.                                        EL526
00837                                                                   EL526
00838      IF  DTE-PRC-OPT   = '2'                                      EL526
00839        IF WS-DW-MONTH = RUN-MO  AND                               EL526
00840           WS-DW-DAY   = RUN-DA  AND                               EL526
00841           WS-DW-YEAR  = RUN-YR                                    EL526
00842            NEXT SENTENCE                                          EL526
00843        ELSE                                                       EL526
00844            GO TO 2150-SIP.                                        EL526
00845                                                                   EL526
00846      IF  DTE-PRC-OPT   = '3'                                      EL526
00847        IF WS-DW-YEAR  = RUN-YR                                    EL526
00848            NEXT SENTENCE                                          EL526
00849        ELSE                                                       EL526
00850            GO TO 2150-SIP.                                        EL526
00851                                                                   EL526
00852      MOVE 'Q'                    TO SR-CK-ENTRY-TYPE.             EL526
00853                                                                   EL526
00854      RELEASE SORT-RECORD.                                         EL526
00855                                                                   EL526
00856      ADD +1                      TO  WS-NO-RECORDS-RELEASED.      EL526
00857                                                                   EL526
00858      GO TO 2150-SIP.                                              EL526
00859                                                                   EL526
00860  2190-EXIT.                                                       EL526
00861      EXIT.                                                        EL526
00862      EJECT                                                        EL526
00863  3000-SORT-OUTPUT-PROCEDURE SECTION.                              EL526
00864                                                                   EL526
00865      IF WS-NO-RECORDS-RELEASED = ZEROS                            EL526
00866          DISPLAY '*** EL526  NO CHECKS ON FILE FOR - '            EL526
00867              RUN-MO '-' RUN-YR                                    EL526
00868          GO TO 3190-EXIT.                                         EL526
00869                                                                   EL526
00870  3100-SOP.                                                        EL526
00871      RETURN SORT-FILE                                             EL526
00872          AT END                                                   EL526
00873              GO TO 3180-SOP.                                      EL526
00874                                                                   EL526
00875      ADD +1                      TO  WS-RECORD-COUNT.             EL526
00876                                                                   EL526
00877      IF LCP-ONCTR-01 =  0                                         EL526
00878          ADD 1 TO LCP-ONCTR-01                                    EL526
00879          MOVE SR-CARRIER         TO WS-LAST-CARRIER               EL526
00880                                     WS-H2-CARRIER                 EL526
00881          MOVE SR-CONTROL-NUMBER  TO WS-LAST-CONTROL.              EL526
00882                                                                   EL526
00883      IF  SR-CARRIER NOT = WS-LAST-CARRIER                         EL526
00884          PERFORM 5000-CONTROL-TOTALS                              EL526
00885          MOVE    SR-CONTROL-NUMBER  TO  WS-LAST-CONTROL           EL526
00886          ADD     WS-LINE-COUNT-MAX  TO  WS-LINE-COUNT             EL526
00887          PERFORM 5100-CARRIER-TOTALS                              EL526
00888          MOVE    SR-CARRIER         TO  WS-LAST-CARRIER           EL526
00889                                         WS-H2-CARRIER             EL526
00890          ADD     WS-LINE-COUNT-MAX  TO  WS-LINE-COUNT.            EL526
00891                                                                   EL526
00892      IF  SR-CONTROL-NUMBER NOT = WS-LAST-CONTROL                  EL526
00893          PERFORM 5000-CONTROL-TOTALS                              EL526
00894          MOVE    SR-CONTROL-NUMBER  TO  WS-LAST-CONTROL           EL526
00895          ADD     WS-LINE-COUNT-MAX  TO  WS-LINE-COUNT.            EL526
00896                                                                   EL526
00897  3200-SOP.                                                        EL526
00898      MOVE ' '                    TO  WS-DETAIL1.                  EL526
00899                                                                   EL526
00900      MOVE SR-CHECK-NO            TO  WS-D1-CHECK-NUMBER.          EL526
00901      MOVE SR-CK-AMOUNT           TO  WS-D1-AMOUNT.                EL526
00902      MOVE SR-CONTROL-NUMBER      TO  WS-D1-CTL-GROUP.             EL526
00903      MOVE SR-CK-PAYEE-NAME       TO  WS-D1-PAYEE-NAME.            EL526
00904      MOVE SR-CARRIER             TO  WS-D1-CARRIER.               EL526
00905      MOVE SR-CK-GROUP            TO  WS-D1-GROUP.                 EL526
00906      MOVE SR-CK-FIN-RESP         TO  WS-D1-FIN-RESP.              EL526
00907      MOVE SR-CK-ACCOUNT          TO  WS-D1-ACCOUNT.               EL526
00908      MOVE SR-CK-CERT-NUMBER      TO  WS-D1-CERT-NUMBER.           EL526
00909                                                                   EL526
00910      IF  BILLING-CREDIT                                           EL526
00911          MOVE 'BILLING-CREDIT'   TO  WS-D1-PMT-TYPE               EL526
00912          IF  CHECK-IS-VOIDED                                      EL526
00913              ADD +1              TO  WS-T5-PMTS-VOID-CNT          EL526
00914                                      WS-T5-CAR-PMTS-VOID-CNT      EL526
00915                                      WS-T5-TOT-PMTS-VOID-CNT      EL526
00916              ADD SR-CK-AMOUNT    TO  WS-T5-PMTS-VOID-AMT          EL526
00917                                      WS-T5-CAR-PMTS-VOID-AMT      EL526
00918                                      WS-T5-TOT-PMTS-VOID-AMT      EL526
00919          ELSE                                                     EL526
00920              ADD +1              TO  WS-T4-PMTS-CNT               EL526
00921                                      WS-T4-CAR-PMTS-CNT           EL526
00922                                      WS-T4-TOT-PMTS-CNT           EL526
00923              ADD SR-CK-AMOUNT    TO  WS-T4-PMTS-AMT               EL526
00924                                      WS-T4-CAR-PMTS-AMT           EL526
00925                                      WS-T4-TOT-PMTS-AMT.          EL526
00926                                                                   EL526
00927      IF  REFUND-PAYMENT                                           EL526
00928          MOVE 'REFUND-PAYMENT'   TO  WS-D1-PMT-TYPE               EL526
00929          IF  CHECK-IS-VOIDED                                      EL526
00930              ADD +1              TO  WS-T2-REFUND-VOID-CNT        EL526
00931                                      WS-T2-CAR-REFUND-VOID-CNT    EL526
00932                                      WS-T2-TOT-REFUND-VOID-CNT    EL526
00933              ADD SR-CK-AMOUNT    TO  WS-T2-REFUND-VOID-AMT        EL526
00934                                      WS-T2-CAR-REFUND-VOID-AMT    EL526
00935                                      WS-T2-TOT-REFUND-VOID-AMT    EL526
00936          ELSE                                                     EL526
00937              ADD +1              TO  WS-T1-REFUND-CNT             EL526
00938                                      WS-T1-CAR-REFUND-CNT         EL526
00939                                      WS-T1-TOT-REFUND-CNT         EL526
00940              ADD SR-CK-AMOUNT    TO  WS-T1-REFUND-AMT             EL526
00941                                      WS-T1-CAR-REFUND-AMT         EL526
00942                                      WS-T1-TOT-REFUND-AMT.        EL526
00943                                                                   EL526
00944      IF  CHECK-MAINT-PMT                                          EL526
00945          MOVE 'CHECK-MAINT-PMT'  TO  WS-D1-PMT-TYPE               EL526
00946          IF  CHECK-IS-VOIDED                                      EL526
00947              ADD +1              TO  WS-T8-MAINT-VOID-CNT         EL526
00948                                      WS-T8-CAR-MAINT-VOID-CNT     EL526
00949                                      WS-T8-TOT-MAINT-VOID-CNT     EL526
00950              ADD SR-CK-AMOUNT    TO  WS-T8-MAINT-VOID-AMT         EL526
00951                                      WS-T8-CAR-MAINT-VOID-AMT     EL526
00952                                      WS-T8-TOT-MAINT-VOID-AMT     EL526
00953          ELSE                                                     EL526
00954              ADD +1              TO  WS-T7-MAINT-CNT              EL526
00955                                      WS-T7-CAR-MAINT-CNT          EL526
00956                                      WS-T7-TOT-MAINT-CNT          EL526
00957              ADD SR-CK-AMOUNT    TO  WS-T7-MAINT-AMT              EL526
00958                                      WS-T7-CAR-MAINT-AMT          EL526
00959                                      WS-T7-TOT-MAINT-AMT.         EL526
00960                                                                   EL526
00961      IF CHECK-IS-MANUAL                                           EL526
00962          IF BILLING-CREDIT                                        EL526
00963              MOVE 'MANUAL BILL-CRD'     TO WS-D1-PMT-TYPE         EL526
00964          ELSE                                                     EL526
00965              IF REFUND-PAYMENT                                    EL526
00966                  MOVE 'MANUAL RFND-PMT' TO WS-D1-PMT-TYPE         EL526
00967              ELSE                                                 EL526
00968                  MOVE 'MANUAL CHK-MAIN' TO WS-D1-PMT-TYPE.        EL526
00969                                                                   EL526
00970      IF CHECK-IS-VOIDED                                           EL526
00971          IF BILLING-CREDIT                                        EL526
00972              MOVE '*VOID* BILL-CRD'     TO WS-D1-PMT-TYPE         EL526
00973          ELSE                                                     EL526
00974              IF REFUND-PAYMENT                                    EL526
00975                  MOVE '*VOID* RFND-PMT' TO WS-D1-PMT-TYPE         EL526
00976              ELSE                                                 EL526
00977                  MOVE '*VOID* CHK-MAIN' TO WS-D1-PMT-TYPE.        EL526
00978                                                                   EL526
00979      IF  SR-CK-WRITTEN-DT NOT = LOW-VALUES AND SPACES             EL526
00980          MOVE    SR-CK-WRITTEN-DT    TO  DC-BIN-DATE-1            EL526
00981          MOVE    SPACES              TO  DC-OPTION-CODE           EL526
00982          PERFORM 8500-DATE-CONVERSION                             EL526
00983          MOVE    DC-GREG-DATE-1-EDIT TO  WS-D1-DATE-PAID          EL526
00984                                          WS-DATE-PAID.            EL526
00985                                                                   EL526
00986      MOVE    WS-DETAIL1              TO  PRT.                     EL526
00987      PERFORM WRITE-A-LINE.                                        EL526
00988                                                                   EL526
00989      MOVE SPACES                 TO  WS-DETAIL1.                  EL526
00990                                                                   EL526
00991      MOVE    WS-DETAIL1          TO  PRT.                         EL526
00992      PERFORM WRITE-A-LINE.                                        EL526
00993                                                                   EL526
00994      IF CLIENT-WSL                                                EL526
00995          PERFORM 4000-G-L-PROCESSING.                             EL526
00996                                                                   EL526
00997      IF CLIENT-CIM                                                EL526
00998          PERFORM 4100-G-L-PROCESSING.                             EL526
00999                                                                   EL526
01000      GO TO 3100-SOP.                                              EL526
01001                                                                   EL526
01002  3180-SOP.                                                        EL526
01003      PERFORM 5000-CONTROL-TOTALS.                                 EL526
01004      ADD     WS-LINE-COUNT-MAX  TO  WS-LINE-COUNT.                EL526
01005      PERFORM 5100-CARRIER-TOTALS.                                 EL526
01006      PERFORM 5200-FINAL-TOTALS.                                   EL526
01007                                                                   EL526
01008  3190-EXIT.                                                       EL526
01009      EXIT.                                                        EL526
01010      EJECT                                                        EL526
01011                                                                   EL526
01012  4000-G-L-PROCESSING SECTION.                                     EL526
01013      IF  SR-CARRIER = 'C'                                         EL526
01014          MOVE 'ELS' TO 3800-COMPANY                               EL526
01015      ELSE                                                         EL526
01016          IF  SR-CARRIER = 'E'                                     EL526
01017              MOVE 'ENL' TO 3800-COMPANY                           EL526
01018          ELSE                                                     EL526
01019              IF  SR-CARRIER = 'I'                                 EL526
01020                  MOVE 'WCO' TO 3800-COMPANY                       EL526
01021              ELSE                                                 EL526
01022                  IF  SR-CARRIER = 'W'  OR '2'                     EL526
01023                      MOVE 'WCO' TO 3800-COMPANY                   EL526
01024                  ELSE                                             EL526
01025                      IF  SR-CARRIER = 'T'  OR '1'                 EL526
01026                          MOVE 'WTX' TO 3800-COMPANY               EL526
01027                      ELSE                                         EL526
01028                          IF  SR-CARRIER = 'U'  OR 'R' OR '3'      EL526
01029                              MOVE 'UFL' TO 3800-COMPANY           EL526
01030                          ELSE                                     EL526
01031                              IF  SR-CARRIER = 'F'                 EL526
01032                                  MOVE 'FLA' TO 3800-COMPANY.      EL526
01033                                                                   EL526
01034      MOVE SR-CHECK-NO            TO  3800-CHECK-NUMBER.           EL526
01035      MOVE SR-CK-ACCOUNT          TO  3800-ACCOUNT.                EL526
01036      MOVE SPACES                 TO  3800-CERT-NO                 EL526
01037                                      3800-DESC-L-H                EL526
01038                                      3800-STATE.                  EL526
01039      IF CHECK-MAINT-PMT                                           EL526
01040          MOVE SR-CK-STATE        TO  3800-STATE.                  EL526
01041                                                                   EL526
01042      MOVE SR-CK-AMOUNT           TO  3800-AMOUNT.                 EL526
01043                                                                   EL526
01044      IF  SR-CK-AMOUNT LESS THAN ZERO  OR                          EL526
01045          CHECK-IS-VOIDED                                          EL526
01046          MOVE 'C'                TO  3800-DEBIT-CREDIT            EL526
01047      ELSE                                                         EL526
01048          MOVE 'D'                TO  3800-DEBIT-CREDIT.           EL526
01049                                                                   EL526
01050      IF  SR-CK-GROUP = '000003'                                   EL526
01051          IF  SR-CARRIER = 'W' OR '2' OR 'T' OR '1'                EL526
01052              MOVE '2310940'  TO  3800-GENERAL-LEDGER              EL526
01053          ELSE                                                     EL526
01054              NEXT SENTENCE                                        EL526
01055      ELSE                                                         EL526
01056          IF  SR-CARRIER = 'U' OR 'R' OR 'C' OR 'W' OR 'T' OR      EL526
01057                               'I' OR '3' OR 'E'                   EL526
01058              MOVE '2310920'  TO  3800-GENERAL-LEDGER              EL526
01059          ELSE                                                     EL526
01060              IF  SR-CARRIER = 'F'                                 EL526
01061                  MOVE '2212890'  TO  3800-GENERAL-LEDGER          EL526
01062              ELSE                                                 EL526
01063                  GO TO 4000-EXIT.                                 EL526
01064                                                                   EL526
01065      WRITE ABC-INTERFACE-RECORD FROM WS-3800-CARD.                EL526
01066                                                                   EL526
01067      IF  SR-CK-AMOUNT LESS THAN ZERO  OR                          EL526
01068          CHECK-IS-VOIDED                                          EL526
01069          MOVE 'D'                TO  3800-DEBIT-CREDIT            EL526
01070      ELSE                                                         EL526
01071          MOVE 'C'                TO  3800-DEBIT-CREDIT.           EL526
01072                                                                   EL526
01073      IF  SR-CARRIER = 'W' OR 'I' OR '1' OR '2'                    EL526
01074          MOVE '1361008' TO  3800-GENERAL-LEDGER                   EL526
01075      ELSE                                                         EL526
01076          IF  SR-CARRIER = 'U' OR 'R' OR '3'                       EL526
01077              MOVE '1361400'  TO  3800-GENERAL-LEDGER              EL526
01078          ELSE                                                     EL526
01079              IF  SR-CARRIER = 'F'                                 EL526
01080                  MOVE '1070955' TO  3800-GENERAL-LEDGER           EL526
01081              ELSE                                                 EL526
01082                  IF  SR-CARRIER = 'C'                             EL526
01083                      MOVE '2081029' TO 3800-GENERAL-LEDGER        EL526
01084                  ELSE                                             EL526
01085                      IF  SR-CARRIER = 'T' OR 'E' OR '1'           EL526
01086                          MOVE '16390  ' TO 3800-GENERAL-LEDGER.   EL526
01087                                                                   EL526
01088      WRITE ABC-INTERFACE-RECORD FROM WS-3800-CARD.                EL526
01089                                                                   EL526
01090      IF SR-CARRIER = 'U' OR 'R' OR 'F' OR 'C' OR                  EL526
01091                             '3' OR 'W' OR 'I'                     EL526
01092          GO TO 4000-EXIT.                                         EL526
01093                                                                   EL526
01094      IF  SR-CK-GROUP = '000003'                                   EL526
01095          IF  SR-CARRIER = 'E'                                     EL526
01096              GO TO 4000-EXIT.                                     EL526
01097                                                                   EL526
01098      MOVE 'WCO'                  TO  3800-COMPANY.                EL526
01099                                                                   EL526
01100      MOVE '16390'                TO  3800-GENERAL-LEDGER.         EL526
01101                                                                   EL526
01102      IF  SR-CK-AMOUNT LESS THAN ZERO  OR                          EL526
01103          CHECK-IS-VOIDED                                          EL526
01104          MOVE 'C'                TO  3800-DEBIT-CREDIT            EL526
01105      ELSE                                                         EL526
01106          MOVE 'D'                TO  3800-DEBIT-CREDIT.           EL526
01107                                                                   EL526
01108      WRITE ABC-INTERFACE-RECORD FROM WS-3800-CARD.                EL526
01109                                                                   EL526
01110      MOVE '1361008'              TO  3800-GENERAL-LEDGER.         EL526
01111                                                                   EL526
01112      IF SR-CK-AMOUNT LESS THAN ZERO  OR                           EL526
01113         CHECK-IS-VOIDED                                           EL526
01114          MOVE 'D'                TO  3800-DEBIT-CREDIT            EL526
01115      ELSE                                                         EL526
01116          MOVE 'C'                TO  3800-DEBIT-CREDIT.           EL526
01117                                                                   EL526
01118      WRITE ABC-INTERFACE-RECORD FROM WS-3800-CARD.                EL526
01119                                                                   EL526
01120  4000-EXIT.                                                       EL526
01121      EXIT.                                                        EL526
01122      EJECT                                                        EL526
01123                                                                   EL526
01124  4100-G-L-PROCESSING SECTION.                                     EL526
01125      IF SR-CARRIER = 'W'                                          EL526
01126          MOVE 'WCO'              TO  3800-COMPANY.                EL526
01127                                                                   EL526
01128      MOVE SR-CHECK-NO            TO  3800-CHECK-NUMBER.           EL526
01129      MOVE SR-CK-ACCOUNT          TO  3800-ACCOUNT.                EL526
01130      MOVE SPACES                 TO  3800-CERT-NO                 EL526
01131                                      3800-DESC-L-H                EL526
01132                                      3800-STATE.                  EL526
01133      IF CHECK-MAINT-PMT                                           EL526
01134          MOVE SR-CK-STATE        TO  3800-STATE.                  EL526
01135                                                                   EL526
01136      MOVE SR-CK-AMOUNT           TO  3800-AMOUNT.                 EL526
01137                                                                   EL526
01138      IF SR-CK-AMOUNT LESS THAN ZERO  OR                           EL526
01139         CHECK-IS-VOIDED                                           EL526
01140          MOVE 'C'                TO  3800-DEBIT-CREDIT            EL526
01141      ELSE                                                         EL526
01142          MOVE 'D'                TO  3800-DEBIT-CREDIT.           EL526
01143                                                                   EL526
01144      IF SR-CARRIER = 'W'                                          EL526
01145          MOVE '2310920'          TO  3800-GENERAL-LEDGER          EL526
01146      ELSE                                                         EL526
01147          GO TO 4100-EXIT.                                         EL526
01148                                                                   EL526
01149      WRITE ABC-INTERFACE-RECORD FROM WS-3800-CARD.                EL526
01150                                                                   EL526
01151      IF SR-CK-AMOUNT LESS THAN ZERO  OR                           EL526
01152         CHECK-IS-VOIDED                                           EL526
01153          MOVE 'D'                TO  3800-DEBIT-CREDIT            EL526
01154      ELSE                                                         EL526
01155          MOVE 'C'                TO  3800-DEBIT-CREDIT.           EL526
01156                                                                   EL526
01157      MOVE '1361008'              TO  3800-GENERAL-LEDGER.         EL526
01158                                                                   EL526
01159      WRITE ABC-INTERFACE-RECORD FROM WS-3800-CARD.                EL526
01160                                                                   EL526
01161  4100-EXIT.                                                       EL526
01162      EXIT.                                                        EL526
01163      EJECT                                                        EL526
01164                                                                   EL526
01165  5000-CONTROL-TOTALS SECTION.                                     EL526
01166                                                                   EL526
01167      ADD WS-T1-REFUND-AMT        TO  WS-T3-NET-REFUND-AMT         EL526
01168                                      WS-T3-CAR-NET-REFUND-AMT     EL526
01169                                      WS-T3-TOT-NET-REFUND-AMT.    EL526
01170      SUBTRACT WS-T2-REFUND-VOID-AMT                               EL526
01171                                FROM  WS-T3-NET-REFUND-AMT         EL526
01172                                      WS-T3-CAR-NET-REFUND-AMT     EL526
01173                                      WS-T3-TOT-NET-REFUND-AMT.    EL526
01174                                                                   EL526
01175      ADD WS-T4-PMTS-AMT          TO  WS-T6-NET-PMTS-AMT           EL526
01176                                      WS-T6-CAR-NET-PMTS-AMT       EL526
01177                                      WS-T6-TOT-NET-PMTS-AMT.      EL526
01178      SUBTRACT WS-T5-PMTS-VOID-AMT                                 EL526
01179                                FROM  WS-T6-NET-PMTS-AMT           EL526
01180                                      WS-T6-CAR-NET-PMTS-AMT       EL526
01181                                      WS-T6-TOT-NET-PMTS-AMT.      EL526
01182                                                                   EL526
01183      ADD WS-T7-MAINT-AMT         TO  WS-T9-NET-MAINT-AMT          EL526
01184                                      WS-T9-CAR-NET-MAINT-AMT      EL526
01185                                      WS-T9-TOT-NET-MAINT-AMT.     EL526
01186      SUBTRACT WS-T8-MAINT-VOID-AMT                                EL526
01187                                FROM  WS-T9-NET-MAINT-AMT          EL526
01188                                      WS-T9-CAR-NET-MAINT-AMT      EL526
01189                                      WS-T9-TOT-NET-MAINT-AMT.     EL526
01190                                                                   EL526
01191      IF WS-LINE-COUNT GREATER +43                                 EL526
01192          ADD WS-LINE-COUNT-MAX   TO  WS-LINE-COUNT.               EL526
01193                                                                   EL526
01194      MOVE WS-LAST-CONTROL        TO WS-HD6-CONTROL-NUMBER.        EL526
01195      MOVE WS-HEADING6            TO  PRT.                         EL526
01196      PERFORM WRITE-A-LINE.                                        EL526
01197                                                                   EL526
01198      SET TOT-INDX TO +1.                                          EL526
01199                                                                   EL526
01200  5010-CONT.                                                       EL526
01201      IF TOT-INDX GREATER TOT-INDX-MAX                             EL526
01202          GO TO 5000-EXIT.                                         EL526
01203                                                                   EL526
01204      MOVE TOTAL-DESC (TOT-INDX)  TO  WS-T1-DESCRIPTION.           EL526
01205      MOVE CONTROL-CNT (TOT-INDX) TO  WS-T1-COUNT.                 EL526
01206      MOVE CONTROL-AMT (TOT-INDX) TO  WS-T1-AMOUNT.                EL526
01207                                                                   EL526
01208      MOVE ZEROS                  TO  CONTROL-CNT (TOT-INDX)       EL526
01209                                      CONTROL-AMT (TOT-INDX).      EL526
01210                                                                   EL526
01211      IF TOT-INDX = +1 OR +4 OR +7                                 EL526
01212          MOVE '0'                TO  WS-T1-CTL                    EL526
01213      ELSE                                                         EL526
01214          MOVE SPACE              TO  WS-T1-CTL.                   EL526
01215                                                                   EL526
01216      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL526
01217      PERFORM WRITE-A-LINE.                                        EL526
01218                                                                   EL526
01219      SET TOT-INDX UP BY +1.                                       EL526
01220                                                                   EL526
01221      GO TO 5010-CONT.                                             EL526
01222                                                                   EL526
01223  5000-EXIT.                                                       EL526
01224      EXIT.                                                        EL526
01225      EJECT                                                        EL526
01226                                                                   EL526
01227  5100-CARRIER-TOTALS SECTION.                                     EL526
01228                                                                   EL526
01229      IF WS-LINE-COUNT GREATER +43                                 EL526
01230          ADD WS-LINE-COUNT-MAX   TO  WS-LINE-COUNT.               EL526
01231                                                                   EL526
01232      MOVE WS-HEADING7            TO  PRT.                         EL526
01233      PERFORM WRITE-A-LINE.                                        EL526
01234                                                                   EL526
01235      SET TOT-INDX TO +1.                                          EL526
01236                                                                   EL526
01237  5110-CONT.                                                       EL526
01238      IF TOT-INDX GREATER TOT-INDX-MAX                             EL526
01239          GO TO 5100-EXIT.                                         EL526
01240                                                                   EL526
01241      MOVE TOTAL-DESC (TOT-INDX)  TO  WS-T1-DESCRIPTION.           EL526
01242      MOVE CARRIER-CNT (TOT-INDX) TO  WS-T1-COUNT.                 EL526
01243      MOVE CARRIER-AMT (TOT-INDX) TO  WS-T1-AMOUNT.                EL526
01244                                                                   EL526
01245      MOVE ZEROS                  TO  CARRIER-CNT (TOT-INDX)       EL526
01246                                      CARRIER-AMT (TOT-INDX).      EL526
01247                                                                   EL526
01248      IF TOT-INDX = +1 OR +4 OR +7                                 EL526
01249          MOVE '0'                TO  WS-T1-CTL                    EL526
01250      ELSE                                                         EL526
01251          MOVE SPACE              TO  WS-T1-CTL.                   EL526
01252                                                                   EL526
01253      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL526
01254      PERFORM WRITE-A-LINE.                                        EL526
01255                                                                   EL526
01256      SET TOT-INDX UP BY +1.                                       EL526
01257                                                                   EL526
01258      GO TO 5110-CONT.                                             EL526
01259                                                                   EL526
01260  5100-EXIT.                                                       EL526
01261      EXIT.                                                        EL526
01262      EJECT                                                        EL526
01263                                                                   EL526
01264  5200-FINAL-TOTALS SECTION.                                       EL526
01265                                                                   EL526
01266      IF WS-LINE-COUNT GREATER +43                                 EL526
01267          ADD WS-LINE-COUNT-MAX   TO  WS-LINE-COUNT.               EL526
01268                                                                   EL526
01269      MOVE WS-HEADING8            TO  PRT.                         EL526
01270      PERFORM WRITE-A-LINE.                                        EL526
01271                                                                   EL526
01272      SET TOT-INDX TO +1.                                          EL526
01273                                                                   EL526
01274  5210-CONT.                                                       EL526
01275                                                                   EL526
01276      MOVE TOTAL-DESC (TOT-INDX)  TO  WS-T1-DESCRIPTION.           EL526
01277      MOVE TOTAL-CNT (TOT-INDX)   TO  WS-T1-COUNT.                 EL526
01278      MOVE TOTAL-AMT (TOT-INDX)   TO  WS-T1-AMOUNT.                EL526
01279                                                                   EL526
01280      IF TOT-INDX = +1 OR +4 OR +7                                 EL526
01281          MOVE '0'                TO  WS-T1-CTL                    EL526
01282      ELSE                                                         EL526
01283          MOVE SPACE              TO  WS-T1-CTL.                   EL526
01284                                                                   EL526
01285      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL526
01286      PERFORM WRITE-A-LINE.                                        EL526
01287                                                                   EL526
01288      SET TOT-INDX UP BY +1.                                       EL526
01289                                                                   EL526
01290      IF TOT-INDX NOT GREATER TOT-INDX-MAX                         EL526
01291          GO TO 5210-CONT.                                         EL526
01292                                                                   EL526
01293      MOVE '-'                    TO  WS-T1-CTL.                   EL526
01294      MOVE '******  CHECK TOTAL ' TO  WS-T1-DESCRIPTION.           EL526
01295      COMPUTE WS-T1-COUNT =                                        EL526
01296                  TOTAL-CNT (1) + TOTAL-CNT (4) + TOTAL-CNT (7).   EL526
01297      MOVE TOTAL-AMT (TOT-INDX)   TO  WS-T1-AMOUNT.                EL526
01298      COMPUTE WS-T1-AMOUNT =                                       EL526
01299                  TOTAL-AMT (1) + TOTAL-AMT (4) + TOTAL-AMT (7).   EL526
01300                                                                   EL526
01301      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL526
01302      PERFORM WRITE-A-LINE.                                        EL526
01303                                                                   EL526
01304      MOVE SPACE                  TO  WS-T1-CTL.                   EL526
01305      MOVE '******  VOID TOTAL  ' TO  WS-T1-DESCRIPTION.           EL526
01306      COMPUTE WS-T1-COUNT =                                        EL526
01307                  TOTAL-CNT (2) + TOTAL-CNT (5) + TOTAL-CNT (8).   EL526
01308      MOVE TOTAL-AMT (TOT-INDX)   TO  WS-T1-AMOUNT.                EL526
01309      COMPUTE WS-T1-AMOUNT =                                       EL526
01310                  TOTAL-AMT (2) + TOTAL-AMT (5) + TOTAL-AMT (8).   EL526
01311                                                                   EL526
01312      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL526
01313      PERFORM WRITE-A-LINE.                                        EL526
01314                                                                   EL526
01315      MOVE SPACE                  TO  WS-T1-CTL.                   EL526
01316      MOVE '******  NET TOTAL   ' TO  WS-T1-DESCRIPTION.           EL526
01317      COMPUTE WS-T1-COUNT =                                        EL526
01318                  TOTAL-CNT (3) + TOTAL-CNT (6) + TOTAL-CNT (9).   EL526
01319      MOVE TOTAL-AMT (TOT-INDX)   TO  WS-T1-AMOUNT.                EL526
01320      COMPUTE WS-T1-AMOUNT =                                       EL526
01321                  TOTAL-AMT (3) + TOTAL-AMT (6) + TOTAL-AMT (9).   EL526
01322                                                                   EL526
01323      MOVE WS-TOTAL-LINE1         TO  PRT.                         EL526
01324      PERFORM WRITE-A-LINE.                                        EL526
01325                                                                   EL526
01326  5200-EXIT.                                                       EL526
01327      EXIT.                                                        EL526
01328      EJECT                                                        EL526
01329                                                                   EL526
01330  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL526
01331                                                                   EL526
01332  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL526
01333                                                                   EL526
01334  WRITE-HEADINGS SECTION.                                          EL526
01335 ***************************************************************** EL526
01336 *                            ELCWHS1.                           * EL526
01337 *                            VMOD=2.001                         * EL526
01338 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL526
01339 *****************************************************************.EL526
01340  WHS-010.                                                         EL526
01341      IF  WS-H2-DATE EQUAL SPACES                                  EL526
01342          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL526
01343          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL526
01344          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL526
01345                                                                   EL526
01346      ADD +1  TO  WS-PAGE.                                         EL526
01347      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL526
01348      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL526
01349      MOVE ZERO                   TO  WS-LINE-COUNT.               EL526
01350                                                                   EL526
01351      MOVE WS-HEADING1            TO  PRT.                         EL526
01352      MOVE '1'                    TO  X.                           EL526
01353      PERFORM WRITE-PRINTER.                                       EL526
01354                                                                   EL526
01355      MOVE WS-HEADING2            TO  PRT.                         EL526
01356      MOVE ' '                    TO  X.                           EL526
01357      PERFORM WRITE-PRINTER.                                       EL526
01358                                                                   EL526
01359      MOVE WS-HEADING3            TO  PRT.                         EL526
01360      MOVE ' '                    TO  X.                           EL526
01361      PERFORM WRITE-PRINTER.                                       EL526
01362                                                                   EL526
01363      MOVE WS-HEADING4            TO  PRT.                         EL526
01364      MOVE ' '                    TO  X.                           EL526
01365      PERFORM WRITE-PRINTER.                                       EL526
01366                                                                   EL526
01367                                                                   EL526
01368      MOVE    WS-HEADING5           TO  PRT.                       EL526
01369      PERFORM WRITE-PRINTER.                                       EL526
01370                                                                   EL526
01371      MOVE    +10                   TO  WS-LINE-COUNT.             EL526
01372                                                                   EL526
01373  WHS-020. COPY ELCWHS2.                                           EL526
01374                                                                   EL526
01375  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL526
01376                                                                   EL526
01377  WPS-020. COPY ELCPRT2X.                                          EL526
01378                                                                   EL526
01379  OPEN-FILES SECTION.                                              EL526
01380  OFS-010.                                                         EL526
01381      OPEN INPUT ERCHKQ                                            EL526
01382                 ERCHEK                                            EL526
01383                 ERCOMP                                            EL526
01384                 ELCNTL                                            EL526
01385          OUTPUT PRNTR                                             EL526
01386                 ABC-INTERFACE-FILE.                               EL526
01387                                                                   EL526
01388      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL526
01389          NEXT SENTENCE                                            EL526
01390        ELSE                                                       EL526
01391          MOVE 'ERROR OCCURED OPEN - ELCNTL'                       EL526
01392                                  TO  WS-ABEND-MESSAGE             EL526
01393          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL526
01394          GO TO ABEND-PGM.                                         EL526
01395                                                                   EL526
01396      IF ERCOMP-FILE-STATUS  = '00' OR '97'                        EL526
01397          NEXT SENTENCE                                            EL526
01398        ELSE                                                       EL526
01399          MOVE 'ERROR OCCURED OPEN - ERCOMP'                       EL526
01400                                  TO  WS-ABEND-MESSAGE             EL526
01401          MOVE ERCOMP-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL526
01402          GO TO ABEND-PGM.                                         EL526
01403                                                                   EL526
01404      IF ERCHKQ-FILE-STATUS  = '00' OR '97'                        EL526
01405          NEXT SENTENCE                                            EL526
01406        ELSE                                                       EL526
01407          MOVE 'ERROR OCCURED OPEN - ERCHKQ'                       EL526
01408                                  TO  WS-ABEND-MESSAGE             EL526
01409          MOVE ERCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL526
01410          GO TO ABEND-PGM.                                         EL526
01411                                                                   EL526
01412      IF ERCHEK-FILE-STATUS  = '00' OR '97'                        EL526
01413          NEXT SENTENCE                                            EL526
01414        ELSE                                                       EL526
01415          MOVE 'ERROR OCCURED OPEN - ERCHEK'                       EL526
01416                                  TO  WS-ABEND-MESSAGE             EL526
01417          MOVE ERCHEK-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL526
01418          GO TO ABEND-PGM.                                         EL526
01419                                                                   EL526
01420                                                                   EL526
01421      SET TOT-INDX TO +1.                                          EL526
01422                                                                   EL526
01423  OFS-020.                                                         EL526
01424      MOVE ZERO  TO  CARRIER-CNT (TOT-INDX)                        EL526
01425                     CARRIER-AMT (TOT-INDX)                        EL526
01426                     TOTAL-CNT (TOT-INDX)                          EL526
01427                     TOTAL-AMT (TOT-INDX).                         EL526
01428                                                                   EL526
01429      IF TOT-INDX LESS THAN TOT-INDX-MAX                           EL526
01430          SET TOT-INDX UP BY +1                                    EL526
01431          GO TO OFS-020.                                           EL526
01432                                                                   EL526
01433      MOVE RUN-DATE               TO  DC-GREG-DATE-1-MDY           EL526
01434                                      3800-DATE.                   EL526
01435      MOVE '4'                    TO  DC-OPTION-CODE.              EL526
01436      PERFORM 8500-DATE-CONVERSION.                                EL526
01437      MOVE DC-JULIAN-DATE         TO  3800-JULIAN-DATE.            EL526
01438                                                                   EL526
01439      MOVE DTE-CLIENT             TO  WS-CLIENT.                   EL526
01440                                                                   EL526
01441      SET TOT-INDX TO +1.                                          EL526
01442                                                                   EL526
01443  OFS-EXIT.                                                        EL526
01444      EXIT.                                                        EL526
01445                                                                   EL526
01446                                                                   EL526
01447  CLOSE-FILES SECTION.                                             EL526
01448  CFS-010. COPY ELCPRTCX.                                          EL526
01449                                                                   EL526
01450      CLOSE ELCNTL                                                 EL526
01451            ERCHEK                                                 EL526
01452            ERCHKQ                                                 EL526
01453            ERCOMP                                                 EL526
01454            ABC-INTERFACE-FILE                                     EL526
01455            PRNTR.                                                 EL526
01456                                                                   EL526
01457      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL526
01458          MOVE 'ERROR OCCURED CLOSE - ELCNTL'                      EL526
01459                                  TO  WS-ABEND-MESSAGE             EL526
01460          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL526
01461          GO TO ABEND-PGM.                                         EL526
01462                                                                   EL526
01463      IF ERCOMP-FILE-STATUS NOT = ZERO                             EL526
01464          MOVE 'ERROR OCCURED CLOSE - ERCOMP'                      EL526
01465                                  TO  WS-ABEND-MESSAGE             EL526
01466          MOVE ERCOMP-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL526
01467          GO TO ABEND-PGM.                                         EL526
01468                                                                   EL526
01469      IF ERCHKQ-FILE-STATUS NOT = ZERO                             EL526
01470          MOVE 'ERROR OCCURED CLOSE - ERCHKQ'                      EL526
01471                                  TO  WS-ABEND-MESSAGE             EL526
01472          MOVE ERCHKQ-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL526
01473          GO TO ABEND-PGM.                                         EL526
01474                                                                   EL526
01475      IF ERCHEK-FILE-STATUS NOT = ZERO                             EL526
01476          MOVE 'ERROR OCCURED CLOSE - ERCHEK'                      EL526
01477                                  TO  WS-ABEND-MESSAGE             EL526
01478          MOVE ERCHEK-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL526
01479          GO TO ABEND-PGM.                                         EL526
01480                                                                   EL526
01481  CFS-EXIT.                                                        EL526
01482      EXIT.                                                        EL526
01483                                                                   EL526
01484  ABEND-PGM SECTION. COPY ELCABEND.                                EL526
01485                                                                   EL526
