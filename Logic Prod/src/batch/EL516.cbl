00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL516
00003  PROGRAM-ID.                 EL516 .                                 LV009
00004 *              PROGRAM CONVERTED BY                               EL516
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL516
00006 *              CONVERSION DATE 02/23/96 10:51:33.                 EL516
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL516
00008 *                            VMOD=2.009                           EL516
00009                                                                   EL516
00010 *AUTHOR.        LOGIC,INC.                                        EL516
00011 *               DALLAS, TEXAS.                                    EL516
00012                                                                   EL516
00013 *DATE-COMPILED.                                                   EL516
00014                                                                   EL516
00015 *SECURITY.   *****************************************************EL516
00016 *            *                                                   *EL516
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL516
00018 *            *                                                   *EL516
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL516
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL516
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL516
00022 *            *                                                   *EL516
00023 *            *****************************************************EL516
00024                                                                   EL516
00025 *REMARKS.                                                         EL516
00026 *        THIS PROGRAM READS AN 80 BYTE TAPE OR CARD FILE AND      EL516
00027 *        FORMATS IT INTO CLAIM PAYMENT AND CLAIM RESERVE          EL516
00028 *        RECORDS FOR THE LOGIC CLAS-IC CREDIT SYSTEM.             EL516
00029                                                                   EL516
00030  ENVIRONMENT DIVISION.                                            EL516
00031  INPUT-OUTPUT SECTION.                                            EL516
00032  FILE-CONTROL.                                                    EL516
00033                                                                   EL516
00034      SELECT SORT-WORK        ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  EL516
00035      SELECT CLAIM-EXTRACT-FILE                                    EL516
00036                              ASSIGN TO SYS006-UR-2540R-SYS006.    EL516
00037      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL516
00038      SELECT CLAIM-EXTRACT-TAPE                                    EL516
00039                              ASSIGN TO SYS010-UT-2400-S-SYS010.   EL516
00040      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL516
00041      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL516
00042      SELECT ERPNDC           ASSIGN TO SYS021-FBA1-ERPNDC         EL516
00043                              ORGANIZATION IS INDEXED              EL516
00044                              ACCESS IS DYNAMIC                    EL516
00045                              RECORD KEY IS PC-CONTROL-PRIMARY     EL516
00046                              FILE STATUS IS ERPNDC-FILE-STATUS.   EL516
00047      SELECT ELREPT           ASSIGN TO SYS023-FBA1-ELREPT         EL516
00048                              ORGANIZATION IS INDEXED              EL516
00049                              ACCESS IS DYNAMIC                    EL516
00050                              RECORD KEY IS RF-CONTROL-PRIMARY     EL516
00051                              FILE STATUS IS DTE-VSAM-FLAGS.       EL516
00052  EJECT                                                            EL516
00053  DATA DIVISION.                                                   EL516
00054  FILE SECTION.                                                    EL516
00055                                                                   EL516
00056  SD  SORT-WORK.                                                   EL516
00057                                                                   EL516
00058  01  SORT-RECORD.                                                 EL516
00059      12  SRT-CARRIER         PIC  X.                              EL516
00060      12  SRT-GROUPING        PIC  X(6).                           EL516
00061      12  SRT-STATE           PIC  XX.                             EL516
00062      12  SRT-ACCOUNT         PIC  X(10).                          EL516
00063      12  FILLER              PIC  X(6).                           EL516
00064      12  SRT-CERT            PIC  X(11).                          EL516
00065      12  SRT-CLAIM           PIC  X(7).                           EL516
00066      12  SRT-CHECK           PIC  X(7).                           EL516
00067      12  SRT-CODE            PIC  X.                              EL516
00068      12  FILLER              PIC  X(28).                          EL516
00069      12  SRT-RECORD-SEQ      PIC  X.                              EL516
00070                                                                   EL516
00071  FD  CLAIM-EXTRACT-FILE                                           EL516
00072      RECORDING MODE F.                                            EL516
00073                                                                   EL516
00074  01  CLAIM-EXTRACT-RECORD    PIC  X(80).                          EL516
00075                                                                   EL516
00076  FD  PRNTR                                                        EL516
00077                              COPY ELCPRTFD.                       EL516
00078  EJECT                                                            EL516
00079  FD  CLAIM-EXTRACT-TAPE                                           EL516
00080      BLOCK CONTAINS 0 RECORDS
00081      RECORDING MODE F.                                            EL516
00082                                                                   EL516
00083  01  CLAIM-INPUT-RECORD      PIC  X(80).                          EL516
00084                                                                   EL516
00085  FD  DISK-DATE                                                    EL516
00086                              COPY ELCDTEFD.                       EL516
00087  EJECT                                                            EL516
00088  FD  FICH                                                         EL516
00089                              COPY ELCFCHFD.                       EL516
00090  EJECT                                                            EL516
00091  FD  ERPNDC.                                                      EL516
00092                                                                   EL516
00093                              COPY ERCPNDC.                        EL516
00094  EJECT                                                            EL516
00095  FD  ELREPT                                                       EL516
00096                              COPY ELCRPTFD.                       EL516
00097                                                                   EL516
00098                              COPY ELCREPT.                        EL516
00099  EJECT                                                            EL516
00100  WORKING-STORAGE SECTION.                                         EL516
00101  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL516
00102  77  FILLER  PIC  X(32) VALUE '********************************'. EL516
00103  77  FILLER  PIC  X(32) VALUE '*     EL516  WORKING STORAGE   *'. EL516
00104  77  FILLER  PIC  X(32) VALUE '************VMOD=2.009**********'. EL516
00105                                                                   EL516
00106  77  FIRST-TIME-SW           PIC  X.                              EL516
00107      88  FIRST-TIME                              VALUE 'Y'.       EL516
00108  77  THIS-MANY               PIC  9              VALUE ZEROS.     EL516
00109  77  TAPE-NO                 PIC  9              VALUE 1.         EL516
00110  77  SAVE-PAYMENT-DATE       PIC  XX             VALUE SPACES.    EL516
00111  77  SAVE-CLAIM-PAYMENT      PIC S9(7)V99 COMP-3 VALUE +0.        EL516
00112  77  SAVE-PENDING-CLAIMS     PIC  X(500)         VALUE SPACES.    EL516
00113                                                                   EL516
00114  01  WS-CHECK-NO.                                                 EL516
00115       12  WS-CHECK-5         PIC  X(7).                           EL516
00116                                                                   EL516
00117  01  FILLER              COMP-3.                                  EL516
00118      12  WS-RETURN-CODE          PIC S9(4)       VALUE +0.        EL516
00119      12  WS-ZERO                 PIC S9(4)       VALUE +0.        EL516
00120      12  E-SUB                   PIC S99         VALUE +0.        EL516
00121      12  MAX-SUB                 PIC S99         VALUE +0.        EL516
00122      12  WS-RECORDS-DROPPED      PIC S9(4)       VALUE +0.        EL516
00123      12  WS-LINE-COUNT           PIC S9(3)       VALUE +99.       EL516
00124      12  WS-LINE-COUNT-MAX       PIC S9(3)       VALUE +56.       EL516
00125      12  WS-PAGE                 PIC S9(5)       VALUE ZERO.      EL516
00126      12  WS-RECORD-COUNT         PIC S9(9)       VALUE ZERO.      EL516
00127      12  WS-TRANS-OUTPUT-COUNT   PIC S9(7)       VALUE ZERO.      EL516
00128      12  WS-GRND-TOTALS.                                          EL516
00129          16  WS-GRND-CNT         PIC S9(5)        VALUE ZERO.     EL516
00130          16  WS-GRND-LIFE        PIC S9(7)V99 VALUE ZERO.         EL516
00131          16  WS-GRND-DISAB       PIC S9(7)V99 VALUE ZERO.         EL516
00132          16  WS-RESV-CNT         PIC S9(5)        VALUE ZERO.     EL516
00133          16  WS-RESV-LIFE        PIC S9(7)V99 VALUE ZERO.         EL516
00134          16  WS-RESV-DISAB       PIC S9(7)V99 VALUE ZERO.         EL516
00135                                                                   EL516
00136  01  FILLER              COMP    SYNC.                            EL516
00137      12  PGM-SUB             PIC S9(4)           VALUE +312.      EL516
00138      12  WS-INDEX            PIC S9(4)           VALUE ZERO.      EL516
00139                                                                   EL516
00140  01  FILLER.                                                      EL516
00141      12  PRINT-REASON            PIC  X(50)  OCCURS  15  TIMES.   EL516
00142      12  WS-SAVE-PRINT-RECORD    PIC  X(133).                     EL516
00143      12  ABEND-CODE              PIC  X(4).                       EL516
00144      12  ABEND-OPTION            PIC  X.                          EL516
00145      12  OLC-REPORT-NAME         PIC  X(5)       VALUE 'EL516'.   EL516
00146      12  CLAIM-EOF-SW            PIC  X          VALUE SPACE.     EL516
00147          88  CLAIM-EOF                           VALUE 'Y'.       EL516
00148      12  X                       PIC  X          VALUE SPACES.    EL516
00149      12  WS-ABEND-MESSAGE        PIC  X(80)      VALUE SPACES.    EL516
00150      12  WS-ABEND-FILE-STATUS    PIC  XX         VALUE ZERO.      EL516
00151      12  WS-RUN-DT               PIC  XX.                            CL**2
00152      12  WS-FILE-ERROR-MESSAGE.                                   EL516
00153          16  FILLER              PIC  X(24)      VALUE            EL516
00154                  'ERROR OCCURED OPENING - '.                      EL516
00155          16  WS-FEM-FILE-NAME    PIC  X(8).                       EL516
00156      12  ERPNDC-FILE-STATUS      PIC  XX         VALUE ZERO.      EL516
00157      12  WS-SAV-INC-DATE-R.                                          CL**6
00158          16  FILLER              PIC 999.                            CL**6
00159          16  SAV-INC-CC-R        PIC 99.                             CL**6
00160          16  SAV-INC-YR-R        PIC 99.                             CL**6
00161          16  SAV-INC-MO-R        PIC 99.                             CL**6
00162          16  SAV-INC-DA-R        PIC 99.                             CL**6
00163  EJECT                                                            EL516
00164  01  WS-HEADING1.                                                 EL516
00165      12  FILLER              PIC  X(43)          VALUE '1'.       EL516
00166      12  WS-H1-TITLE         PIC  X(82)          VALUE            EL516
00167              'EXTRACTED CLAIM PAYMENTS AND RESERVES '.            EL516
00168      12  WS-H1-REPORT-NUMBER PIC  X(9)           VALUE '  EL516'. EL516
00169                                                                   EL516
00170  01  WS-HEADING2.                                                 EL516
00171      12  FILLER              PIC  X(46)          VALUE SPACES.    EL516
00172      12  WS-H2-CLIENT-NAME   PIC  X(79)          VALUE SPACES.    EL516
00173      12  WS-H2-DATE          PIC  X(8).                           EL516
00174                                                                   EL516
00175  01  WS-HEADING3.                                                 EL516
00176      12  FILLER              PIC  X(53)          VALUE SPACES.    EL516
00177      12  WS-H3-DATE          PIC  X(58)          VALUE SPACES.    EL516
00178      12  FILLER              PIC  X(5)           VALUE 'PAGE'.    EL516
00179      12  WS-H3-PAGE          PIC ZZ,ZZ9.                          EL516
00180      12  FILLER              PIC  X(11)          VALUE SPACES.    EL516
00181                                                                   EL516
00182  01  WS-HEADING4.                                                 EL516
00183      12  FILLER              PIC  X(45)          VALUE            EL516
00184              '0 CLAIM                 CERT      ACCOUNT    '.     EL516
00185      12  FILLER              PIC  X(44)          VALUE            EL516
00186              '  CLAIM   PAID     CHECK                    '.      EL516
00187      12  FILLER              PIC  X(44)          VALUE            EL516
00188              '    EFFECTIVE                               '.      EL516
00189                                                                   EL516
00190  01  WS-HEADING6.                                                 EL516
00191      12  FILLER              PIC  X(45)          VALUE            EL516
00192              '  NUMBER CAR  GROUP    NUMBER     NUMBER   ST'.     EL516
00193      12  FILLER              PIC  X(44)          VALUE            EL516
00194              '  TYPE    DATE    NUMBER     AMOUNT      TYP'.      EL516
00195      12  FILLER              PIC  X(44)          VALUE            EL516
00196              'E     DATE               COMMENT            '.      EL516
00197  EJECT                                                            EL516
00198  01  WS-DETAIL1.                                                  EL516
00199      12  WS-D1-CARRIAGE-CONTROL  PIC  X.                          EL516
00200      12  WS-D1-CLAIM-NO          PIC  X(7).                       EL516
00201      12  FILLER                  PIC  XX.                         EL516
00202      12  WS-D1-CARRIER           PIC  X.                          EL516
00203      12  FILLER                  PIC  XX.                         EL516
00204      12  WS-D1-GROUPING          PIC  X(6).                       EL516
00205      12  FILLER                  PIC  X.                          EL516
00206      12  WS-D1-CERT-NO           PIC  X(11).                      EL516
00207      12  FILLER                  PIC  X.                          EL516
00208      12  WS-D1-ACCOUNT           PIC  X(10).                      EL516
00209      12  FILLER                  PIC  X.                          EL516
00210      12  WS-D1-STATE             PIC  XX.                         EL516
00211      12  FILLER                  PIC  XX.                         EL516
00212      12  WS-D1-CLAIM-TYPE        PIC  X(4).                       EL516
00213      12  FILLER                  PIC  XX.                         EL516
00214      12  WS-D1-DATE-PAID.                                         EL516
00215          16  WS-D1-MONTH-PAID    PIC  XX.                         EL516
00216          16  SLASH1              PIC  X          VALUE '/'.       EL516
00217          16  WS-D1-DAY-PAID      PIC  XX.                         EL516
00218          16  SLASH2              PIC  X          VALUE '/'.       EL516
00219          16  WS-D1-YEAR-PAID     PIC  XX.                         EL516
00220      12  FILLER                  PIC  X.                          EL516
00221      12  WS-D1-CHECK-NUMBER      PIC  X(7).                       EL516
00222      12  FILLER                  PIC  X.                          EL516
00223      12  WS-D1-AMOUNT            PIC Z,ZZZ,ZZ9.99-.               EL516
00224      12  WS-X-AMOUNT  REDEFINES                                   EL516
00225          WS-D1-AMOUNT            PIC  X(13).                      EL516
00226      12  FILLER                  PIC  X.                          EL516
00227      12  WS-D1-TYPE              PIC  X(7).                       EL516
00228      12  FILLER                  PIC  XX.                         EL516
00229      12  WS-D1-DATE-EFF.                                          EL516
00230          16  WS-D1-MONTH-EFF     PIC  XX.                         EL516
00231          16  SLASH3              PIC  X          VALUE '/'.       EL516
00232          16  WS-D1-DAY-EFF       PIC  XX.                         EL516
00233          16  SLASH4              PIC  X          VALUE '/'.       EL516
00234          16  WS-D1-YEAR-EFF      PIC  XX.                         EL516
00235      12  FILLER                  PIC  XX.                         EL516
00236      12  PRINT-COMMENT           PIC  X(30).                      EL516
00237  EJECT                                                            EL516
00238  01  WS-DETAIL2.                                                  EL516
00239      12  FILLER              PIC  X              VALUE SPACES.    EL516
00240      12  WS-D2-ERROR         PIC  X(99)          VALUE SPACES.    EL516
00241      12  WS-D2-PRINT-REASON  PIC  X(32).                          EL516
00242                                                                   EL516
00243  01  WS-TOTAL-LINE1.                                              EL516
00244      12  FILLER                  PIC  X(15).                      EL516
00245      12  WS-T1-DESCRIPTION       PIC  X(20).                      EL516
00246      12  FILLER                  PIC  X(5).                       EL516
00247      12  WS-T1-BREAK             PIC  X(6)       VALUE 'TOTAL '.  EL516
00248      12  FILLER                  PIC  X(5).                       EL516
00249      12  WS-T1-COUNT             PIC Z,ZZZ,ZZ9-.                  EL516
00250      12  FILLER                  PIC  X(5).                       EL516
00251      12  WS-T1-AMOUNT-LIFE       PIC Z,ZZZ,ZZ9.99-.               EL516
00252      12  FILLER                  PIC  X(5).                       EL516
00253      12  WS-T1-AMOUNT-DISAB      PIC Z,ZZZ,ZZ9.99-.               EL516
00254      12  FILLER                  PIC  X(10).                      EL516
00255  EJECT                                                            EL516
00256                              COPY ERCPNDCI.                       EL516
00257  EJECT                                                            EL516
00258  01  SAVE-CLAIMS-IN.                                              EL516
00259      12  SAV-CARRIER             PIC X.                           EL516
00260      12  SAV-GROUPING            PIC X(6).                        EL516
00261      12  SAV-STATE               PIC XX.                          EL516
00262      12  SAV-ACCOUNT-NO          PIC X(10).                       EL516
00263      12  SAV-EFF-DATE  COMP-3.                                    EL516
00264          16  FILLER              PIC 999.                         EL516
00265          16  SAV-EFF-CC          PIC 99.                          EL516
00266          16  SAV-EFF-YR          PIC 99.                          EL516
00267          16  SAV-EFF-MO          PIC 99.                          EL516
00268          16  SAV-EFF-DA          PIC 99.                          EL516
00269      12  SAV-CERT-NO.                                             EL516
00270          16  SAV-CERT-PREFIX     PIC X(10).                       EL516
00271          16  SAV-CERT-SUFFIX     PIC X.                           EL516
00272      12  SAV-CLAIM-NO            PIC X(7).                        EL516
00273      12  SAV-CLAIM-REC-SEQ-1.                                     EL516
00274          16  SAV-CHECK-NO        PIC X(7).                        EL516
00275          16  SAV-CODE            PIC X.                           EL516
00276              88  SAV-VALID-CODE                VALUE '1' THRU '4'.EL516
00277              88  SAV-LIFE                      VALUE '1'.         EL516
00278              88  SAV-DISAB                     VALUE '2'.         EL516
00279              88  SAV-OB-LIFE                   VALUE '3'.         EL516
00280              88  SAV-OB-DISAB                  VALUE '4'.         EL516
00281          16  SAV-TYPE-PMT        PIC X.                           EL516
00282              88  SAV-VALID-TYPE-PMT VALUE '1' THRU '6' '9'        EL516
00283                  'P' 'F' 'L' 'A' 'C' 'N' 'V'.                     EL516
00284              88  SAV-PARTIAL                   VALUE '1' 'P'.     EL516
00285              88  SAV-FINAL                     VALUE '2' 'F'.     EL516
00286              88  SAV-LUMP-SUM                  VALUE '3' 'L'.     EL516
00287              88  SAV-ADDITIONAL                VALUE '4' 'A'.     EL516
00288              88  SAV-CHARGEABLE                VALUE '5' 'C'.     EL516
00289              88  SAV-NON-CHARGEABLE            VALUE '6' 'N'.     EL516
00290              88  SAV-VOIDED                    VALUE '9' 'V'.     EL516
00291          16  SAV-PAID-AMT            PIC S9(9)V99.                EL516
00292          16  SAV-AMT REDEFINES SAV-PAID-AMT                       EL516
00293                                      PIC X(11).                   EL516
00294          16  SAV-DAYS-DISAB          PIC S9(3).                   EL516
00295          16  SAV-DAYS REDEFINES SAV-DAYS-DISAB                    EL516
00296                                      PIC X(3).                    EL516
00297          16  SAV-AGE-DTH.                                         EL516
00298              20  SAV-AGE             PIC 99 .                     EL516
00299          16  SAV-CAUSE               PIC XX.                      EL516
00300          16  FILLER                  PIC X(7).                    EL516
00301          16  SAV-FORCE-CD            PIC X.                       EL516
00302          16  SAV-RECORD-TYPE         PIC X.                       EL516
00303              88  SAV-CLAIM-PAYMENT       VALUE '4'.               EL516
00304              88  SAV-RESERVE             VALUE '5'.               EL516
00305          16  SAV-RECORD-SEQUENCE     PIC X.                       EL516
00306                                                                   EL516
00307      12  RESERVE-OVERLAY-AREA  REDEFINES  SAV-CLAIM-REC-SEQ-1.    EL516
00308          16  SAV-RESERVE-CODE        PIC X.                       EL516
00309              88  SAV-VALID-RESERVE             VALUE '1' THRU '4'.EL516
00310              88  SAV-LIFE-R                    VALUE '1'.         EL516
00311              88  SAV-DISAB-R                   VALUE '2'.         EL516
00312              88  SAV-OB-LIFE-R                 VALUE '3'.         EL516
00313              88  SAV-OB-DISAB-R                VALUE '4'.         EL516
00314          16  SAV-INC-DATE-R          PIC 9(11) COMP-3.               CL**2
00315          16  SAV-IBNR                PIC S9(7)V99.                EL516
00316          16  SAV-IBNR-X   REDEFINES SAV-IBNR                      EL516
00317                                      PIC X(9).                    EL516
00318          16  SAV-PTC                 PIC S9(7)V99.                EL516
00319          16  SAV-PTC-X    REDEFINES SAV-PTC                       EL516
00320                                      PIC X(9).                    EL516
00321          16  SAV-FUTURE              PIC S9(7)V99.                EL516
00322          16  SAV-FUTURE-X REDEFINES SAV-FUTURE                    EL516
00323                                      PIC X(9).                    EL516
00324          16  SAV-FORCE-CD-R          PIC X.                       EL516
00325          16  FILLER                  PIC X(2).                    EL516
00326                                                                   EL516
00327      12  SAV-CLAIM-REC-SEQ-2.                                     EL516
00328          16  FILLER                  PIC X(51).                   EL516
00329          16  SAV-INC-DATE  COMP-3.                                EL516
00330              20  FILLER              PIC 999.                     EL516
00331              20  SAV-INC-CC          PIC 99.                      EL516
00332              20  SAV-INC-YR          PIC 99.                      EL516
00333              20  SAV-INC-MO          PIC 99.                      EL516
00334              20  SAV-INC-DA          PIC 99.                      EL516
00335          16  SAV-RPT-DATE.                                        EL516
00336              20  SAV-RPT-MO          PIC 99.                      EL516
00337              20  SAV-RPT-DA          PIC 99.                      EL516
00338              20  SAV-RPT-YR          PIC 99.                      EL516
00339          16  SAV-PAID-DATE  COMP-3.                               EL516
00340              20  FILLER              PIC 999.                     EL516
00341              20  SAV-PAID-CC         PIC 99.                      EL516
00342              20  SAV-PAID-YR         PIC 99.                      EL516
00343              20  SAV-PAID-MO         PIC 99.                      EL516
00344              20  SAV-PAID-DA         PIC 99.                      EL516
00345          16  SAV-PD-THRU-DATE  COMP-3.                            EL516
00346              20  FILLER              PIC 999.                     EL516
00347              20  SAV-PD-THRU-CC      PIC 99.                      EL516
00348              20  SAV-PD-THRU-YR      PIC 99.                      EL516
00349              20  SAV-PD-THRU-MO      PIC 99.                      EL516
00350              20  SAV-PD-THRU-DA      PIC 99.                      EL516
00351          16  FILLER                  PIC X(5).                    EL516
00352  EJECT                                                            EL516
00353                              COPY ELCDATE.                           CL**9
00354                                                                   EL516
00355                              COPY ELCDTECX.                          CL**3
00356                                                                      CL**4
00357                              COPY ELCDTEVR.                          CL**4
00358  EJECT                                                            EL516
00359  PROCEDURE DIVISION.                                              EL516
00360                                                                   EL516
00361  0000-DATE-CARD-READ SECTION.                                     EL516
00362                              COPY ELCDTERX.                          CL**3
00363                                                                   EL516
00364      IF DTE-SYS-F-CLASIC-CREDIT  NOT = 'Y'                        EL516
00365          DISPLAY 'EL516 - COMPANY NOT ONLINE CREDIT USER *'       EL516
00366              DTE-CLIENT '*'  UPON  CONSOLE                        EL516
00367          GO TO 0210-CLOSE-FILES.                                  EL516
00368                                                                   EL516
00369  0100-OPEN-FILES.                                                 EL516
00370      OPEN I-O     ERPNDC                                          EL516
00371           OUTPUT  PRNTR.                                          EL516
00372                                                                   EL516
00373      IF ERPNDC-FILE-STATUS  = '00' OR '97'                        EL516
00374          NEXT SENTENCE                                            EL516
00375      ELSE                                                         EL516
00376          MOVE  '*** EL516  ERPNDC OPEN ERROR - JOB WILL ABEND'    EL516
00377                                   TO  WS-ABEND-MESSAGE            EL516
00378          MOVE ERPNDC-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL516
00379          GO TO ABEND-PGM.                                         EL516
00380                                                                   EL516
00381      MOVE SPACES                 TO  FIRST-TIME-SW.               EL516
00382      MOVE LOW-VALUES             TO  SAVE-CLAIMS-IN.              EL516
00383                                                                   EL516
00384  0110-CONVERT-RUN-DATE.                                           EL516
00385                                                                   EL516
00386      MOVE BIN-RUN-DATE           TO  WS-RUN-DT.                      CL**2
00387                                                                   EL516
00388      IF TAPE-BATCHES  NUMERIC                                     EL516
00389          MOVE TAPE-BATCHES       TO  THIS-MANY                    EL516
00390          PERFORM 0300-SORT-INPUT-TAPE  THIS-MANY  TIMES           EL516
00391          GO TO 0210-CLOSE-FILES.                                  EL516
00392  EJECT                                                            EL516
00393  0200-SORT-INPUT-FILE.                                            EL516
00394      MOVE 'Y'                    TO  FIRST-TIME-SW.               EL516
00395                                                                   EL516
00396      SORT SORT-WORK  ON ASCENDING KEY  SRT-CARRIER                EL516
00397                                        SRT-GROUPING               EL516
00398                                        SRT-STATE                  EL516
00399                                        SRT-ACCOUNT                EL516
00400                                        SRT-CERT                   EL516
00401                                        SRT-CLAIM                  EL516
00402                                        SRT-CHECK                  EL516
00403                                        SRT-RECORD-SEQ             EL516
00404          USING  CLAIM-EXTRACT-FILE                                EL516
00405              OUTPUT PROCEDURE 1000-UPDATE-PENDING-CLAIMS.         EL516
00406                                                                   EL516
00407      IF SORT-RETURN  NOT = ZERO                                   EL516
00408          MOVE '0101'             TO  ABEND-CODE                   EL516
00409          GO TO ABEND-PGM.                                         EL516
00410                                                                   EL516
00411  0210-CLOSE-FILES.                                                EL516
00412      IF FIRST-TIME                                                EL516
00413          MOVE '-  *** NO VALID RECORDS ON INPUT FILE ***'         EL516
00414                                  TO  PRT                          EL516
00415          PERFORM WRITE-A-LINE.                                    EL516
00416                                                                   EL516
00417      MOVE 'RF'                   TO  RF-RECORD-ID.                EL516
00418      MOVE DTE-CLASIC-COMPANY-CD  TO  RF-COMPANY-CD.               EL516
00419      MOVE OLC-REPORT-NAME        TO  RF-REPORT-ID.                EL516
00420                                                                   EL516
00421  0250-CLOSE-ELREPT.                                               EL516
00422                              COPY ELCPRTCX.                       EL516
00423                                                                   EL516
00424      CLOSE ERPNDC                                                 EL516
00425            PRNTR.                                                 EL516
00426                                                                   EL516
00427      IF ERPNDC-FILE-STATUS  NOT = ZEROS                           EL516
00428          MOVE  '*** EL516  ERPNDC CLOSE ERROR - JOB WILL ABEND'   EL516
00429                                   TO  WS-ABEND-MESSAGE            EL516
00430          MOVE ERPNDC-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL516
00431          GO TO ABEND-PGM.                                         EL516
00432                                                                   EL516
00433      GOBACK.                                                      EL516
00434  EJECT                                                            EL516
00435  0300-SORT-INPUT-TAPE.                                            EL516
00436      DISPLAY 'MOUNT TAPE NUMBER..... '  TAPE-NO  UPON  CONSOLE.   EL516
00437                                                                   EL516
00438      MOVE 'Y'                    TO  FIRST-TIME-SW.               EL516
00439                                                                   EL516
00440      SORT SORT-WORK  ON ASCENDING KEY  SRT-CARRIER                EL516
00441                                        SRT-GROUPING               EL516
00442                                        SRT-STATE                  EL516
00443                                        SRT-ACCOUNT                EL516
00444                                        SRT-CERT                   EL516
00445                                        SRT-CLAIM                  EL516
00446                                        SRT-CHECK                  EL516
00447                                        SRT-RECORD-SEQ             EL516
00448          USING  CLAIM-EXTRACT-TAPE                                EL516
00449          OUTPUT PROCEDURE 1000-UPDATE-PENDING-CLAIMS.             EL516
00450                                                                   EL516
00451      IF SORT-RETURN  NOT =  ZERO                                  EL516
00452          MOVE '0101'             TO  ABEND-CODE                   EL516
00453          GO TO ABEND-PGM.                                         EL516
00454                                                                   EL516
00455      ADD 1                       TO  TAPE-NO.                     EL516
00456                                                                   EL516
00457  0300-END.                                                        EL516
00458  EJECT                                                            EL516
00459  1000-UPDATE-PENDING-CLAIMS  SECTION.                             EL516
00460                                                                   EL516
00461  1010-RETURN-SORT-WORK.                                           EL516
00462      RETURN SORT-WORK  INTO  PENDING-CLAIMS-IN  AT END            EL516
00463          PERFORM 5000-GRND-TOTALS  THRU  5999-GRND-EXIT           EL516
00464          GO TO 6999-SORT-FINISH.                                  EL516
00465                                                                   EL516
00466      IF PCI-RESERVE                                               EL516
00467          MOVE PENDING-CLAIMS-IN  TO SAVE-CLAIMS-IN                EL516
00468          GO TO 1020-BEGIN-EDIT.                                   EL516
00469                                                                   EL516
00470      IF SAVE-CLAIMS-IN  = LOW-VALUES                              EL516
00471          IF PCI-RECORD-SEQUENCE  = '1'                            EL516
00472              MOVE PENDING-CLAIMS-IN                               EL516
00473                                  TO  SAVE-CLAIMS-IN               EL516
00474              GO TO 1010-RETURN-SORT-WORK                          EL516
00475          ELSE                                                     EL516
00476              MOVE PENDING-CLAIMS-IN                               EL516
00477                                  TO  WS-D2-ERROR                  EL516
00478              MOVE 'FIRST RECORD NOT SEQ 1'                        EL516
00479                                  TO  WS-D2-PRINT-REASON           EL516
00480              MOVE WS-DETAIL2     TO  PRT                          EL516
00481              MOVE ' '            TO  P-CTL                           CL**8
00482              PERFORM WRITE-A-LINE                                 EL516
00483              MOVE SPACES         TO  WS-DETAIL2                   EL516
00484              GO TO 1010-RETURN-SORT-WORK.                         EL516
00485                                                                   EL516
00486      IF PCI-CARRIER     = SAV-CARRIER    AND                      EL516
00487         PCI-GROUPING    = SAV-GROUPING   AND                      EL516
00488         PCI-STATE       = SAV-STATE      AND                      EL516
00489         PCI-ACCOUNT-NO  = SAV-ACCOUNT-NO AND                      EL516
00490         PCI-CERT-NO     = SAV-CERT-NO    AND                      EL516
00491         PCI-CHECK-NO    = SAV-CHECK-NO   AND                      EL516
00492         PCI-CLAIM-NO    = SAV-CLAIM-NO                            EL516
00493          IF PCI-RECORD-SEQUENCE  = '2'                            EL516
00494              MOVE PENDING-CLAIMS-IN                               EL516
00495                                  TO  SAV-CLAIM-REC-SEQ-2          EL516
00496              GO TO 1020-BEGIN-EDIT.                               EL516
00497                                                                   EL516
00498      MOVE PENDING-CLAIMS-IN      TO  SAVE-CLAIMS-IN.              EL516
00499                                                                   EL516
00500      GO TO 1010-RETURN-SORT-WORK.                                 EL516
00501                                                                   EL516
00502  1020-BEGIN-EDIT.                                                 EL516
00503      INSPECT SAV-ACCOUNT-NO REPLACING ALL ' ' BY '0'.             EL516
00504      INSPECT SAV-CERT-PREFIX REPLACING ALL ' ' BY '0'.            EL516
00505                                                                   EL516
00506      IF SAV-RESERVE                                               EL516
00507          INSPECT SAV-IBNR-X REPLACING ALL ' ' BY '0'              EL516
00508          INSPECT SAV-PTC-X REPLACING ALL ' ' BY '0'               EL516
00509          INSPECT SAV-FUTURE-X REPLACING ALL ' ' BY '0'            EL516
00510      ELSE                                                         EL516
00511          INSPECT SAV-AMT REPLACING ALL ' ' BY '0'                 EL516
00512          INSPECT SAV-DAYS REPLACING ALL ' ' BY '0'                EL516
00513          INSPECT SAV-EFF-DATE REPLACING ALL ' ' BY '0'               CL**6
00514          INSPECT SAV-INC-DATE REPLACING ALL ' ' BY '0'            EL516
00515          INSPECT SAV-RPT-DATE REPLACING ALL ' ' BY '0'            EL516
00516          INSPECT SAV-PAID-DATE REPLACING ALL ' ' BY '0'           EL516
00517          INSPECT SAV-PD-THRU-DATE REPLACING ALL ' ' BY '0'        EL516
00518          INSPECT SAV-AGE-DTH REPLACING ALL ' ' BY '0'.            EL516
00519                                                                   EL516
00520      IF FIRST-TIME                                                EL516
00521          MOVE 'N'                TO  FIRST-TIME-SW                EL516
00522          MOVE +99                TO  WS-LINE-COUNT                EL516
00523          MOVE +0                 TO  WS-PAGE.                     EL516
00524                                                                   EL516
00525                                                                   EL516
00526  1030-BUILD-EDIT-RECORD.                                          EL516
00527      PERFORM 1035-SPACE-OUT                                       EL516
00528          VARYING  E-SUB  FROM  +1  BY  +1                         EL516
00529              UNTIL  E-SUB  IS GREATER THAN  +15.                  EL516
00530                                                                   EL516
00531      GO TO 1038-NEXT-SENTENCE.                                    EL516
00532                                                                   EL516
00533  1035-SPACE-OUT.                                                  EL516
00534      MOVE SPACES                 TO  PRINT-REASON (E-SUB).        EL516
00535                                                                   EL516
00536  1038-NEXT-SENTENCE.                                              EL516
00537      MOVE +0                     TO  WS-RECORDS-DROPPED           EL516
00538                                      E-SUB                        EL516
00539                                      MAX-SUB.                     EL516
00540      MOVE SPACES                 TO  PENDING-CLAIMS               EL516
00541                                      WS-DETAIL1.                  EL516
00542      MOVE '/'                    TO  SLASH1  SLASH2               EL516
00543                                      SLASH3  SLASH4.              EL516
00544      MOVE 'PC'                   TO  PC-RECORD-ID.                EL516
00545      MOVE DTE-CLASIC-COMPANY-CD  TO  PC-COMPANY-CD.               EL516
00546      MOVE '1'                    TO  PC-RECORD-TYPE.              EL516
00547      MOVE SAV-CARRIER            TO  PC-SV-CARRIER                EL516
00548                                      WS-D1-CARRIER.               EL516
00549      MOVE SAV-GROUPING           TO  PC-SV-GROUPING               EL516
00550                                      WS-D1-GROUPING.              EL516
00551      MOVE SAV-STATE              TO  PC-SV-STATE                  EL516
00552                                      WS-D1-STATE.                 EL516
00553      MOVE SAV-ACCOUNT-NO         TO  PC-ACCOUNT                   EL516
00554                                      WS-D1-ACCOUNT.               EL516
00555      MOVE SAV-CERT-NO            TO  PC-CERT-NO                   EL516
00556                                      WS-D1-CERT-NO.               EL516
00557      MOVE '4'                    TO  DC-OPTION-CODE.              EL516
00558      MOVE SAV-EFF-CC             TO  DC-ALPHA-CEN-N.              EL516
00559      MOVE SAV-EFF-YR             TO  DC-MDY-YEAR.                 EL516
00560      MOVE SAV-EFF-DA             TO  DC-MDY-DAY.                  EL516
00561      MOVE SAV-EFF-MO             TO  DC-MDY-MONTH.                EL516
00562                                                                   EL516
00563      PERFORM 8500-DATE-CONVERSION.                                EL516
00564                                                                   EL516
00565      IF NO-CONVERSION-ERROR                                       EL516
00566          MOVE DC-BIN-DATE-1      TO  PC-CERT-EFF-DT               EL516
00567      ELSE                                                         EL516
00568          ADD +1                  TO  E-SUB                        EL516
00569          MOVE 'DROPPED'          TO  PRINT-REASON (E-SUB)         EL516
00570          ADD +1                  TO  WS-RECORDS-DROPPED           EL516
00571          MOVE LOW-VALUES         TO  PC-CERT-EFF-DT               EL516
00572          ADD +1                  TO  E-SUB                        EL516
00573          MOVE 'INVALID EFFECTIVE DATE'                            EL516
00574                                  TO  PRINT-REASON (E-SUB).        EL516
00575                                                                   EL516
00576      MOVE SAV-EFF-MO             TO  WS-D1-MONTH-EFF.             EL516
00577      MOVE SAV-EFF-DA             TO  WS-D1-DAY-EFF.               EL516
00578      MOVE SAV-EFF-YR             TO  WS-D1-YEAR-EFF.              EL516
00579                                                                   EL516
00580      IF SAV-CLAIM-NO  NOT = SPACES                                EL516
00581        AND  ZEROS  AND  LOW-VALUES                                EL516
00582          MOVE SAV-CLAIM-NO       TO  PC-CLAIM-NO                  EL516
00583      ELSE                                                         EL516
00584          MOVE SPACES             TO  PC-CLAIM-NO                  EL516
00585          ADD +1                  TO  E-SUB                        EL516
00586          MOVE 'INVALID CLAIM NUMBER'                              EL516
00587                                  TO  PRINT-REASON (E-SUB).        EL516
00588                                                                   EL516
00589      MOVE SAV-CLAIM-NO           TO  WS-D1-CLAIM-NO.              EL516
00590                                                                   EL516
00591      IF SAV-CLAIM-PAYMENT                                         EL516
00592          MOVE SAV-CODE               TO  PC-CLAIM-TYPE            EL516
00593          IF SAV-LIFE                                              EL516
00594              MOVE '  LF'             TO  WS-D1-CLAIM-TYPE         EL516
00595          ELSE                                                     EL516
00596              IF SAV-DISAB                                         EL516
00597                  MOVE '  AH'         TO  WS-D1-CLAIM-TYPE         EL516
00598              ELSE                                                 EL516
00599                  IF SAV-OB-LIFE                                   EL516
00600                      MOVE 'OBLF'     TO  WS-D1-CLAIM-TYPE         EL516
00601                  ELSE                                             EL516
00602                      IF SAV-OB-DISAB                              EL516
00603                          MOVE 'OBAH'    TO  WS-D1-CLAIM-TYPE      EL516
00604                      ELSE                                         EL516
00605                          MOVE SAV-CODE  TO  WS-D1-CLAIM-TYPE      EL516
00606                          ADD +1         TO  E-SUB                 EL516
00607                          MOVE 'CLAIM CODE NOT VALID'              EL516
00608                                         TO  PRINT-REASON (E-SUB). EL516
00609                                                                   EL516
00610      IF SAV-RESERVE                                               EL516
00611          MOVE SAV-RESERVE-CODE       TO  PC-CLAIM-TYPE            EL516
00612          IF SAV-LIFE-R                                            EL516
00613              MOVE '  LF'             TO  WS-D1-CLAIM-TYPE         EL516
00614          ELSE                                                     EL516
00615              IF SAV-DISAB-R                                       EL516
00616                  MOVE '  AH'         TO  WS-D1-CLAIM-TYPE         EL516
00617              ELSE                                                 EL516
00618                  IF SAV-OB-LIFE-R                                 EL516
00619                      MOVE 'OBLF'     TO  WS-D1-CLAIM-TYPE         EL516
00620                  ELSE                                             EL516
00621                      IF SAV-OB-DISAB-R                            EL516
00622                          MOVE 'OBAH'    TO  WS-D1-CLAIM-TYPE      EL516
00623                      ELSE                                         EL516
00624                          MOVE SAV-RESERVE-CODE                    EL516
00625                                         TO  WS-D1-CLAIM-TYPE      EL516
00626                          ADD +1         TO  E-SUB                 EL516
00627                          MOVE 'CLAIM CODE NOT VALID'              EL516
00628                                         TO  PRINT-REASON (E-SUB). EL516
00629                                                                   EL516
00630      IF SAV-RESERVE                                               EL516
00631          MOVE SAV-INC-DATE-R     TO  WS-SAV-INC-DATE-R               CL**7
00632          MOVE '2'                TO  PC-RECORD-TYPE               EL516
00633          MOVE SAV-FUTURE         TO  PC-FUTURE-RESERVE-AMT        EL516
00634          MOVE SAV-IBNR           TO  PC-IBNR-RESERVE-AMT          EL516
00635          MOVE SAV-PTC            TO  PC-PTC-RESERVE-AMT           EL516
00636          COMPUTE WS-D1-AMOUNT = SAV-FUTURE + SAV-IBNR + SAV-PTC   EL516
00637          MOVE 'RESERVE'          TO  WS-D1-TYPE                   EL516
00638          MOVE SPACE              TO  SLASH1  SLASH2               EL516
00639          MOVE '4'                TO  DC-OPTION-CODE               EL516
00640          MOVE SAV-INC-CC-R       TO  DC-ALPHA-CEN-N               EL516
00641          MOVE SAV-INC-YR-R       TO  DC-MDY-YEAR                  EL516
00642          MOVE SAV-INC-MO-R       TO  DC-MDY-MONTH                 EL516
00643          MOVE SAV-INC-DA-R       TO  DC-MDY-DAY                   EL516
00644          PERFORM 8500-DATE-CONVERSION                             EL516
00645          IF NO-CONVERSION-ERROR                                   EL516
00646              MOVE DC-BIN-DATE-1  TO  PC-INCURRED-DT               EL516
00647              GO TO 1050-BUILD-KEY                                 EL516
00648          ELSE                                                     EL516
00649              MOVE LOW-VALUES     TO  PC-INCURRED-DT               EL516
00650              ADD +1              TO  E-SUB                        EL516
00651              MOVE 'INVALID INCURRED DATE'                         EL516
00652                                  TO  PRINT-REASON (E-SUB)         EL516
00653              GO TO 1050-BUILD-KEY.                                EL516
00654                                                                   EL516
00655      MOVE '4'                    TO  DC-OPTION-CODE.              EL516
00656      MOVE SAV-INC-YR             TO  DC-MDY-YEAR.                 EL516
00657      MOVE SAV-INC-MO             TO  DC-MDY-MONTH.                EL516
00658      MOVE SAV-INC-DA             TO  DC-MDY-DAY.                  EL516
00659      MOVE SAV-INC-CC             TO  DC-ALPHA-CEN-N.              EL516
00660                                                                   EL516
00661      PERFORM 8500-DATE-CONVERSION.                                EL516
00662                                                                   EL516
00663      IF NO-CONVERSION-ERROR                                       EL516
00664          MOVE DC-BIN-DATE-1      TO  PC-INCURRED-DT               EL516
00665      ELSE                                                         EL516
00666          MOVE LOW-VALUES         TO  PC-INCURRED-DT               EL516
00667          ADD +1                  TO  E-SUB                        EL516
00668          MOVE 'INVALID INCURRED DATE'                             EL516
00669                                  TO  PRINT-REASON (E-SUB).        EL516
00670                                                                   EL516
00671      MOVE '4'                    TO  DC-OPTION-CODE.              EL516
00672      MOVE SAV-RPT-DATE           TO  DC-GREG-DATE-1-MDY.          EL516
00673                                                                   EL516
00674      PERFORM 8500-DATE-CONVERSION.                                EL516
00675                                                                   EL516
00676      IF NO-CONVERSION-ERROR                                       EL516
00677          MOVE DC-BIN-DATE-1      TO  PC-REPORTED-DT               EL516
00678      ELSE                                                         EL516
00679          MOVE LOW-VALUES         TO  PC-REPORTED-DT               EL516
00680          ADD +1                  TO  E-SUB                        EL516
00681          MOVE 'INVALID REPORTED DATE'                             EL516
00682                                  TO  PRINT-REASON (E-SUB).        EL516
00683                                                                   EL516
00684      MOVE '4'                    TO  DC-OPTION-CODE.              EL516
00685      MOVE SAV-PAID-YR            TO  DC-MDY-YEAR.                 EL516
00686      MOVE SAV-PAID-CC            TO  DC-ALPHA-CEN-N.              EL516
00687      MOVE SAV-PAID-MO            TO  DC-MDY-MONTH.                EL516
00688      MOVE SAV-PAID-DA            TO  DC-MDY-DAY.                  EL516
00689                                                                   EL516
00690      PERFORM 8500-DATE-CONVERSION.                                EL516
00691                                                                   EL516
00692      IF NO-CONVERSION-ERROR                                       EL516
00693          MOVE DC-BIN-DATE-1      TO  PC-PAYMENT-DT                EL516
00694      ELSE                                                         EL516
00695          MOVE LOW-VALUES         TO  PC-PAYMENT-DT                EL516
00696          ADD +1                  TO  E-SUB                        EL516
00697          MOVE 'INVALID PAYMENT DATE'                              EL516
00698                                  TO  PRINT-REASON (E-SUB).        EL516
00699                                                                   EL516
00700      MOVE SAV-PAID-MO            TO  WS-D1-MONTH-PAID.            EL516
00701      MOVE SAV-PAID-DA            TO  WS-D1-DAY-PAID.              EL516
00702      MOVE SAV-PAID-YR            TO  WS-D1-YEAR-PAID.             EL516
00703                                                                   EL516
00704      IF SAV-LIFE                                                  EL516
00705        OR  SAV-OB-LIFE                                            EL516
00706         MOVE LOW-VALUES          TO  PC-PAID-THRU-DT              EL516
00707      ELSE                                                         EL516
00708         MOVE '4'                 TO  DC-OPTION-CODE               EL516
00709         MOVE SAV-PD-THRU-YR      TO  DC-MDY-YEAR                  EL516
00710         MOVE SAV-PD-THRU-MO      TO  DC-MDY-MONTH                 EL516
00711         MOVE SAV-PD-THRU-DA      TO  DC-MDY-DAY                   EL516
00712         MOVE SAV-PD-THRU-CC      TO  DC-ALPHA-CEN-N               EL516
00713         PERFORM 8500-DATE-CONVERSION                              EL516
00714         IF NO-CONVERSION-ERROR                                    EL516
00715            MOVE DC-BIN-DATE-1    TO  PC-PAID-THRU-DT              EL516
00716            IF DTE-CLAIM-PAID-THRU-TO EQUAL '1'                    EL516
00717               MOVE -1     TO DC-ELAPSED-DAYS                      EL516
00718               MOVE +0     TO DC-ELAPSED-MONTHS                    EL516
00719               MOVE '6'    TO DC-OPTION-CODE                       EL516
00720               PERFORM 8500-DATE-CONVERSION                        EL516
00721               IF NO-CONVERSION-ERROR                              EL516
00722                  MOVE DC-BIN-DATE-2 TO PC-PAID-THRU-DT            EL516
00723               ELSE                                                EL516
00724                  MOVE LOW-VALUES       TO  PC-PAID-THRU-DT        EL516
00725                  ADD +1                TO  E-SUB                  EL516
00726                  MOVE 'INVALID PAID  TO  DATE'                    EL516
00727                                  TO  PRINT-REASON (E-SUB)         EL516
00728            ELSE                                                   EL516
00729               NEXT SENTENCE                                       EL516
00730         ELSE                                                      EL516
00731            MOVE LOW-VALUES       TO  PC-PAID-THRU-DT              EL516
00732            ADD +1                TO  E-SUB                        EL516
00733            MOVE 'INVALID PAID THRU/TO DATE'                       EL516
00734                                  TO  PRINT-REASON (E-SUB).        EL516
00735                                                                   EL516
00736      IF SAV-PAID-AMT NUMERIC                                      EL516
00737          MOVE SAV-PAID-AMT       TO  PC-CLAIM-PAYMENT             EL516
00738                                      WS-D1-AMOUNT                 EL516
00739      ELSE                                                         EL516
00740          MOVE +0                 TO  PC-CLAIM-PAYMENT             EL516
00741          MOVE SAV-AMT            TO  WS-X-AMOUNT                  EL516
00742          ADD +1                  TO  E-SUB                        EL516
00743          MOVE 'INVALID PAYMENT AMT '                              EL516
00744                                  TO  PRINT-REASON (E-SUB).        EL516
00745                                                                   EL516
00746      IF SAV-CHECK-NO    NOT = SPACES                              EL516
00747        AND  ZEROS  AND  LOW-VALUES                                EL516
00748          MOVE SAV-CHECK-NO       TO  WS-CHECK-NO                  EL516
00749          MOVE WS-CHECK-NO        TO  PC-CHECK-NO                  EL516
00750      ELSE                                                         EL516
00751          MOVE SPACES             TO  PC-CHECK-NO                  EL516
00752          ADD +1                  TO  E-SUB                        EL516
00753          MOVE 'INVALID CHECK NUMBER'                              EL516
00754                                  TO  PRINT-REASON (E-SUB).        EL516
00755                                                                   EL516
00756      MOVE SAV-CHECK-NO           TO  WS-D1-CHECK-NUMBER.          EL516
00757                                                                   EL516
00758      IF DTE-CLIENT  = 'MIC'  OR  'MCC'                            EL516
00759          MOVE SAV-CAUSE           TO  PC-CAUSE-CODE               EL516
00760          MOVE +0                  TO  PC-NO-OF-DAYS-PAID          EL516
00761      ELSE                                                         EL516
00762          IF SAV-DAYS-DISAB NUMERIC                                EL516
00763              MOVE SAV-DAYS-DISAB  TO  PC-NO-OF-DAYS-PAID          EL516
00764          ELSE                                                     EL516
00765              MOVE +0              TO  PC-NO-OF-DAYS-PAID.         EL516
00766                                                                   EL516
00767      IF SAV-AGE-DTH NUMERIC                                       EL516
00768          MOVE SAV-AGE-DTH        TO  PC-AGE-AT-CLAIM              EL516
00769      ELSE                                                         EL516
00770          MOVE ZEROS              TO  PC-AGE-AT-CLAIM.             EL516
00771                                                                   EL516
00772      IF SAV-VALID-TYPE-PMT                                        EL516
00773          INSPECT SAV-TYPE-PMT CONVERTING 'PFLACNV' TO '1234569'   EL516
00774          MOVE SAV-TYPE-PMT       TO  PC-PAYMENT-TYPE              EL516
00775      ELSE                                                         EL516
00776          MOVE SPACES             TO  PC-PAYMENT-TYPE              EL516
00777          ADD +1                  TO  E-SUB                        EL516
00778          MOVE 'INVALID PAYMENT TYPE '                             EL516
00779                                  TO  PRINT-REASON (E-SUB).        EL516
00780                                                                   EL516
00781      IF SAV-PARTIAL                                               EL516
00782         MOVE 'PARTIAL'                       TO  WS-D1-TYPE       EL516
00783      ELSE                                                         EL516
00784         IF SAV-FINAL                                              EL516
00785            MOVE 'FINAL'                      TO  WS-D1-TYPE       EL516
00786         ELSE                                                      EL516
00787            IF SAV-LUMP-SUM                                        EL516
00788               MOVE 'LUMP'                    TO  WS-D1-TYPE       EL516
00789            ELSE                                                   EL516
00790               IF SAV-ADDITIONAL                                   EL516
00791                  MOVE 'ADDL'                 TO  WS-D1-TYPE       EL516
00792               ELSE                                                EL516
00793                  IF SAV-CHARGEABLE                                EL516
00794                     MOVE 'CHG EXP'           TO  WS-D1-TYPE       EL516
00795                  ELSE                                             EL516
00796                     IF SAV-NON-CHARGEABLE                         EL516
00797                        MOVE 'NON CHG'        TO  WS-D1-TYPE       EL516
00798                     ELSE                                          EL516
00799                        IF SAV-VOIDED                              EL516
00800                           MOVE 'VOIDED '     TO  WS-D1-TYPE       EL516
00801                        ELSE                                       EL516
00802                           MOVE SAV-TYPE-PMT  TO  WS-D1-TYPE.      EL516
00803                                                                   EL516
00804  1050-BUILD-KEY.                                                  EL516
00805      IF DTE-COMP-VG  = '1'                                        EL516
00806          MOVE SAV-CARRIER                TO  PC-CARRIER           EL516
00807          MOVE SAV-GROUPING               TO  PC-GROUPING          EL516
00808          MOVE SAV-STATE                  TO  PC-STATE             EL516
00809      ELSE                                                         EL516
00810          IF DTE-COMP-VG  = '2'                                    EL516
00811              MOVE SAV-CARRIER            TO  PC-CARRIER           EL516
00812              MOVE SPACES                 TO  PC-GROUPING          EL516
00813              MOVE SAV-STATE              TO  PC-STATE             EL516
00814          ELSE                                                     EL516
00815              IF DTE-COMP-VG  = '3'                                EL516
00816                  MOVE SPACES             TO  PC-CARRIER           EL516
00817                  MOVE SPACES             TO  PC-GROUPING          EL516
00818                  MOVE SPACES             TO  PC-STATE             EL516
00819              ELSE                                                 EL516
00820                  IF DTE-COMP-VG  = '4'                            EL516
00821                      MOVE SAV-CARRIER    TO  PC-CARRIER           EL516
00822                      MOVE SPACES         TO  PC-GROUPING          EL516
00823                      MOVE SPACES         TO  PC-STATE             EL516
00824                  ELSE                                             EL516
00825                      IF DTE-COMP-VG  = ' '                        EL516
00826                          MOVE SAV-STATE  TO  PC-STATE             EL516
00827                          MOVE SPACES     TO  PC-GROUPING          EL516
00828                          MOVE SPACES     TO  PC-CARRIER.          EL516
00829                                                                   EL516
00830      IF SAV-RESERVE                                               EL516
00831          MOVE SAV-FORCE-CD-R         TO  PC-FORCE-CODE            EL516
00832      ELSE                                                         EL516
00833          MOVE SAV-FORCE-CD           TO  PC-FORCE-CODE.           EL516
00834                                                                   EL516
00835      IF WS-RECORDS-DROPPED  IS GREATER THAN  +0                   EL516
00836          MOVE +0                 TO  WS-RECORDS-DROPPED           EL516
00837          GO TO 1060-PRINT-REPORT.                                 EL516
00838                                                                   EL516
00839                                                                   EL516
00840  1040-FILL-REST-OF-RECORD.                                        EL516
00841      MOVE +0                     TO  PC-RECORD-SEQUENCE.          EL516
00842      MOVE WS-RUN-DT              TO  PC-LAST-MAINT-DT                CL**2
00843                                      PC-INPUT-DT.                 EL516
00844      MOVE CLASIC-CREDIT-EOM-DT   TO  PC-CREDIT-SELECT-DT.         EL516
00845      MOVE DTE-CLIENT             TO  PC-LAST-MAINT-BY             EL516
00846                                      PC-COMPANY-ID.               EL516
00847                                                                   EL516
00848      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL516
00849                                                                   EL516
00850      MOVE WS-TIME                TO  PC-LAST-MAINT-HHMMSS.        EL516
00851                                                                   EL516
00852      IF PC-RESERVES                                               EL516
00853          MOVE ZERO               TO  PC-CLAIM-PAYMENT             EL516
00854          MOVE LOW-VALUE          TO  PC-PAYMENT-DT                EL516
00855          MOVE SPACES             TO  PC-CHECK-NO.                 EL516
00856                                                                   EL516
00857      IF PC-CLAIMS                                                 EL516
00858          MOVE ZEROS              TO  PC-FUTURE-RESERVE-AMT        EL516
00859                                      PC-IBNR-RESERVE-AMT          EL516
00860                                      PC-PTC-RESERVE-AMT.          EL516
00861                                                                   EL516
00862      MOVE ZERO                   TO  PC-CC-INSURED-AGE            EL516
00863                                      PC-MANUAL-RESERVE-AMT        EL516
00864                                      PC-CC-ORIG-TERM              EL516
00865                                      PC-CC-LF-BENEFIT-CD          EL516
00866                                      PC-CC-LIFE-BENEFIT-AMT       EL516
00867                                      PC-CC-ALT-LF-BENEFIT-AMT     EL516
00868                                      PC-CC-LIFE-PREMIUM           EL516
00869                                      PC-CC-AH-BENEFIT-CD          EL516
00870                                      PC-CC-AH-BENEFIT-AMT         EL516
00871                                      PC-CC-AH-PREMIUM-AMT         EL516
00872                                      PC-CC-PAY-FREQUENCY          EL516
00873                                      PC-CC-LOAN-APR               EL516
00874                                      PC-CC-CAPPED-TERM            EL516
00875                                      PC-CC-PRIOR-LUMP-PMT         EL516
00876                                      PC-CC-PRIOR-DEATH-AMT        EL516
00877                                      PC-REMAINING-BENEFIT         EL516
00878                                      PC-REMAINING-TERM.           EL516
00879      MOVE LOW-VALUES             TO  PC-CC-CANCEL-DT              EL516
00880                                      PC-CC-DEATH-DT               EL516
00881                                      PC-CC-SETTLEMENT-DT          EL516
00882                                      PC-CREDIT-ACCEPT-DT.         EL516
00883      MOVE SPACES                 TO  PC-CAUSE-CODE                EL516
00884                                      PC-VOID-SW                   EL516
00885                                      PC-CC-INSURED-NAME           EL516
00886                                      PC-CC-INSURED-SEX            EL516
00887                                      PC-CC-RATE-CLASS             EL516
00888                                      PC-CC-RATE-DEV               EL516
00889                                      PC-CC-OB-FLAG.               EL516
00890      MOVE 'X'                    TO  PC-FATAL-FLAG.               EL516
00891  EJECT                                                            EL516
00892  1050-WRITE-PENDING-CLAIMS.                                       EL516
00893      WRITE PENDING-CLAIMS.                                        EL516
00894                                                                   EL516
00895      ADD +1                      TO  WS-TRANS-OUTPUT-COUNT.       EL516
00896                                                                   EL516
00897      IF ERPNDC-FILE-STATUS  = '22'                                EL516
00898          GO TO 1055-CHECK-IF-DUPLICATE.                           EL516
00899                                                                   EL516
00900      IF ERPNDC-FILE-STATUS  NOT =  ZEROS                          EL516
00901          MOVE  '*** EL516  ERPNDC WRITE ERROR - JOB WILL ABEND'   EL516
00902                                   TO  WS-ABEND-MESSAGE            EL516
00903          MOVE ERPNDC-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL516
00904          GO TO ABEND-PGM.                                         EL516
00905                                                                   EL516
00906      GO TO 1060-PRINT-REPORT.                                     EL516
00907                                                                   EL516
00908                                                                   EL516
00909  1055-CHECK-IF-DUPLICATE.                                         EL516
00910      MOVE PC-PAYMENT-DT          TO  SAVE-PAYMENT-DATE.           EL516
00911      MOVE PC-CLAIM-PAYMENT       TO  SAVE-CLAIM-PAYMENT.          EL516
00912      MOVE PENDING-CLAIMS         TO  SAVE-PENDING-CLAIMS.         EL516
00913                                                                   EL516
00914      READ ERPNDC.                                                 EL516
00915                                                                   EL516
00916      IF PC-PAYMENT-DT     = SAVE-PAYMENT-DATE   AND               EL516
00917         PC-CLAIM-PAYMENT  = SAVE-CLAIM-PAYMENT                    EL516
00918          ADD +1                  TO  E-SUB                        EL516
00919          MOVE 'DROPPED'          TO  PRINT-REASON (E-SUB)         EL516
00920          ADD +1                  TO  WS-RECORDS-DROPPED           EL516
00921                                      E-SUB                        EL516
00922          MOVE 'DUPLICATE CLAIM ON FILE '                          EL516
00923                                  TO  PRINT-REASON (E-SUB)         EL516
00924      ELSE                                                         EL516
00925          MOVE SAVE-PENDING-CLAIMS  TO  PENDING-CLAIMS             EL516
00926          ADD +1                    TO  PC-RECORD-SEQUENCE         EL516
00927          GO TO 1050-WRITE-PENDING-CLAIMS.                         EL516
00928  EJECT                                                            EL516
00929  1060-PRINT-REPORT.                                               EL516
00930      MOVE E-SUB                  TO  MAX-SUB.                     EL516
00931      MOVE +1                     TO  E-SUB.                       EL516
00932      MOVE PRINT-REASON (E-SUB)   TO  PRINT-COMMENT.               EL516
00933      MOVE WS-DETAIL1             TO  PRT.                         EL516
00934      MOVE ' '                    TO  P-CTL.                          CL**8
00935                                                                   EL516
00936      PERFORM WRITE-A-LINE.                                        EL516
00937                                                                   EL516
00938      ADD +1                      TO  E-SUB.                       EL516
00939                                                                   EL516
00940  1070-PRINT-REASON.                                               EL516
00941      IF MAX-SUB  = +0                                             EL516
00942          GO TO 1080-ADD-TO-TOTALS.                                EL516
00943                                                                   EL516
00944      MOVE PRINT-REASON (E-SUB)   TO  WS-D2-PRINT-REASON.          EL516
00945      MOVE WS-DETAIL2             TO  PRT.                         EL516
00946      MOVE ' '                    TO  P-CTL.                          CL**8
00947                                                                   EL516
00948      PERFORM WRITE-A-LINE.                                        EL516
00949                                                                   EL516
00950      ADD +1                      TO  E-SUB.                       EL516
00951                                                                   EL516
00952      IF E-SUB NOT GREATER MAX-SUB                                 EL516
00953          GO TO 1070-PRINT-REASON.                                 EL516
00954  EJECT                                                            EL516
00955  1080-ADD-TO-TOTALS.                                              EL516
00956      IF PC-CLAIMS                                                 EL516
00957          IF WS-RECORDS-DROPPED  = +0                              EL516
00958              ADD +1                TO  WS-GRND-CNT                EL516
00959              IF PC-LF-CLAIM                                       EL516
00960                OR  PC-OB-LF-CLAIM                                 EL516
00961                  ADD SAV-PAID-AMT  TO  WS-GRND-LIFE               EL516
00962              ELSE                                                 EL516
00963                  ADD SAV-PAID-AMT  TO  WS-GRND-DISAB.             EL516
00964                                                                   EL516
00965      IF PC-RESERVES                                               EL516
00966          IF WS-RECORDS-DROPPED  = +0                              EL516
00967              ADD +1                         TO  WS-RESV-CNT       EL516
00968              IF PC-LF-CLAIM                                       EL516
00969                OR  PC-OB-LF-CLAIM                                 EL516
00970                  ADD PC-FUTURE-RESERVE-AMT  TO  WS-RESV-LIFE      EL516
00971                  ADD PC-PTC-RESERVE-AMT     TO  WS-RESV-LIFE      EL516
00972                  ADD PC-IBNR-RESERVE-AMT    TO  WS-RESV-LIFE      EL516
00973              ELSE                                                 EL516
00974                  ADD PC-FUTURE-RESERVE-AMT  TO  WS-RESV-DISAB     EL516
00975                  ADD PC-PTC-RESERVE-AMT     TO  WS-RESV-DISAB     EL516
00976                  ADD PC-IBNR-RESERVE-AMT    TO  WS-RESV-DISAB.    EL516
00977                                                                   EL516
00978      MOVE LOW-VALUES               TO SAVE-CLAIMS-IN.             EL516
00979                                                                   EL516
00980      GO TO 1010-RETURN-SORT-WORK.                                 EL516
00981                                                                   EL516
00982  1999-EXIT.                                                       EL516
00983       EXIT.                                                       EL516
00984  EJECT                                                            EL516
00985  5000-GRND-TOTALS.                                                EL516
00986      MOVE SPACES                  TO  WS-TOTAL-LINE1.             EL516
00987      MOVE 'CLAIM PAYMENT TOTALS'  TO  WS-T1-DESCRIPTION.          EL516
00988      MOVE WS-GRND-CNT             TO  WS-T1-COUNT.                EL516
00989      MOVE WS-GRND-LIFE            TO  WS-T1-AMOUNT-LIFE.          EL516
00990      MOVE WS-GRND-DISAB           TO  WS-T1-AMOUNT-DISAB.         EL516
00991      MOVE WS-TOTAL-LINE1          TO  PRT.                        EL516
00992      MOVE '-'                     TO  P-CTL.                         CL**8
00993                                                                   EL516
00994      PERFORM WRITE-A-LINE.                                        EL516
00995                                                                   EL516
00996      MOVE SPACES                  TO  WS-TOTAL-LINE1.             EL516
00997      MOVE 'CLAIM RESERVE TOTALS'  TO  WS-T1-DESCRIPTION.          EL516
00998      MOVE WS-RESV-CNT             TO  WS-T1-COUNT.                EL516
00999      MOVE WS-RESV-LIFE            TO  WS-T1-AMOUNT-LIFE.          EL516
01000      MOVE WS-RESV-DISAB           TO  WS-T1-AMOUNT-DISAB.         EL516
01001      MOVE WS-TOTAL-LINE1          TO  PRT.                        EL516
01002      MOVE '-'                     TO  P-CTL.                         CL**8
01003                                                                   EL516
01004      PERFORM WRITE-A-LINE.                                        EL516
01005                                                                   EL516
01006      MOVE +0                     TO  WS-GRND-CNT                  EL516
01007                                      WS-GRND-LIFE                 EL516
01008                                      WS-GRND-DISAB.               EL516
01009                                                                   EL516
01010  5999-GRND-EXIT.                                                  EL516
01011      EXIT.                                                        EL516
01012                                                                   EL516
01013  6999-SORT-FINISH.                                                EL516
01014      EXIT.                                                        EL516
01015  EJECT                                                            EL516
01016  8500-DATE-CONVERSION SECTION.   COPY ELCDCS.                     EL516
01017  EJECT                                                            EL516
01018  WRITE-A-LINE SECTION.       COPY ELCWAL.                         EL516
01019  EJECT                                                            EL516
01020  WRITE-HEADINGS SECTION.                                          EL516
01021 ***************************************************************** EL516
01022 *                                                               * EL516
01023 *                            ELCWHS1.                           * EL516
01024 *                            VMOD=2.001                         * EL516
01025 *                                                               * EL516
01026 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL516
01027 *****************************************************************.EL516
01028  WHS-010.                                                         EL516
01029      IF  WS-H2-DATE EQUAL SPACES                                  EL516
01030          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL516
01031          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL516
01032          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL516
01033                                                                   EL516
01034      ADD +1  TO  WS-PAGE.                                         EL516
01035      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL516
01036      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL516
01037      MOVE ZERO                   TO  WS-LINE-COUNT.               EL516
01038                                                                   EL516
01039      MOVE WS-HEADING1            TO  PRT.                         EL516
01040      MOVE '1'                    TO  X.                           EL516
01041      PERFORM WRITE-PRINTER.                                       EL516
01042                                                                   EL516
01043      MOVE WS-HEADING2            TO  PRT.                         EL516
01044      MOVE ' '                    TO  X.                           EL516
01045      PERFORM WRITE-PRINTER.                                       EL516
01046                                                                   EL516
01047      MOVE WS-HEADING3            TO  PRT.                         EL516
01048      MOVE ' '                    TO  X.                           EL516
01049      PERFORM WRITE-PRINTER.                                       EL516
01050                                                                   EL516
01051      MOVE WS-HEADING4            TO  PRT.                         EL516
01052      MOVE ' '                    TO  X.                           EL516
01053      PERFORM WRITE-PRINTER.                                       EL516
01054                                                                   EL516
01055      MOVE WS-HEADING6            TO  PRT.                         EL516
01056                                                                   EL516
01057      PERFORM WRITE-PRINTER.                                       EL516
01058                                                                   EL516
01059      MOVE +7                     TO  WS-LINE-COUNT.               EL516
01060                                                                   EL516
01061  WHS-020.                    COPY ELCWHS2.                        EL516
01062                                                                   EL516
01063  EJECT                                                            EL516
01064  WRITE-PRINTER SECTION.      COPY ELCWPS.                         EL516
01065                                                                   EL516
01066  WPS-020.                    COPY ELCPRT2X.                       EL516
01067                                                                   EL516
01068  EJECT                                                            EL516
01069  ABEND-PGM SECTION.                                               EL516
01070                              COPY ELCABEND.                       EL516
01071                                                                   EL516
