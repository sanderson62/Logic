00001  IDENTIFICATION DIVISION.                                         08/19/98
00002                                                                   EL522
00003  PROGRAM-ID.                 EL522 .                                 LV005
00004 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          EL522
00005 *                            VMOD=2.013.                             CL**3
00006 *                                                                 EL522
00007 *AUTHOR.        LOGIC, INC.                                       EL522
00008 *               DALLAS, TEXAS.                                    EL522
00009                                                                   EL522
00010 *DATE-COMPILED.                                                   EL522
00011                                                                   EL522
00012 *SECURITY.   *****************************************************EL522
00013 *            *                                                   *EL522
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL522
00015 *            *                                                   *EL522
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL522
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL522
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *EL522
00019 *            *                                                   *EL522
00020 *            *****************************************************EL522
00021                                                                   EL522
00022 *REMARKS.                                                         EL522
00023 *        THIS PROGRAM RUNS AT UPDATE TIME AND SELECTS PENDING     EL522
00024 *        BUSINESS RECORDS, PENDING CLAIMS RECORDS (FOR CLIENTS    EL522
00025 *        NOT ON THE CLASIC CLAIMS SYSTEM) AND CERTIFICATE CHANGE  EL522
00026 *        RECORDS FOR INPUT INTO THE LOGIC CLAS CREDIT SYSTEM -    EL522
00027 *        ECS LEVEL 6 PRGRAM ECS010, PAYMENTS & ADJUSTMENTS FOR    EL522
00028 *        ECS061 AND RETRO PAYMENTS FOR ECS041.                    EL522
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
100703* 100703                   PEMA  ADD SUPER GAP PROCESSING
062104* 062104    2004050700001  SMVA  ADD NEW FILE TO AUTOMATE ME BALANCING
122002******************************************************************
00029  EJECT                                                            EL522
00030  ENVIRONMENT DIVISION.                                            EL522
00031  INPUT-OUTPUT SECTION.                                            EL522
00032  FILE-CONTROL.                                                    EL522
00033                                                                   EL522
00034      SELECT PRNTR            ASSIGN TO SYS008.
00035                                                                   EL522
00036      SELECT EXTRACT-INTERFACE-FILE                                EL522
00037                              ASSIGN TO SYS010.
00038                                                                   EL522
00039      SELECT EXTRACT-DATA-FILE                                     EL522
00040                              ASSIGN TO SYS003.
00041                                                                   EL522
062104     SELECT ME50-EL522-BALANCE
062104                             ASSIGN TO SYS011
062104                             ORGANIZATION IS LINE SEQUENTIAL.
00042      SELECT PAYMENTS-AND-ADJUSTMENTS                              EL522
00043                              ASSIGN TO SYS012.
00044                                                                   EL522
00045      SELECT RETRO-PAYMENTS                                        EL522
00046                              ASSIGN TO SYS013.
00047                                                                   EL522
00048      SELECT ELREPT           ASSIGN TO SYS018-FBA1-ELREPT         EL522
00049                              ORGANIZATION IS INDEXED              EL522
00050                              ACCESS IS DYNAMIC                    EL522
00051                              RECORD KEY IS RF-CONTROL-PRIMARY     EL522
00052                              FILE STATUS IS DTE-VSAM-FLAGS.       EL522
00053                                                                   EL522
00054      SELECT DISK-DATE        ASSIGN TO SYS019.
00055                                                                   EL522
00056      SELECT FICH             ASSIGN TO SYS020.
00057                                                                   EL522
00058      SELECT ERMEBL           ASSIGN SYS024-FBA1-ERMEBL            EL522
00059                              ORGANIZATION INDEXED                 EL522
00060                              ACCESS DYNAMIC                       EL522
00061                              RECORD KEY ME-CONTROL-PRIMARY        EL522
00062                              FILE STATUS ERMEBL-FILE-STATUS.      EL522
00063                                                                   EL522
00064      SELECT SORT-WORK-FILE   ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  EL522
00065                                                                   EL522
00066  EJECT                                                            EL522
00067  DATA DIVISION.                                                   EL522
00068  FILE SECTION.                                                    EL522
00069                                                                   EL522
00070  FD  PRNTR                                                        EL522
00071                                  COPY ELCPRTFD.                   EL522
00072  EJECT                                                            EL522
00073  FD  EXTRACT-INTERFACE-FILE                                       EL522
00074                                  COPY ERCEXTFD.                   EL522
00075                                                                   EL522
00076  01  EXTRACT-INTERFACE-FILE-RECORD   PIC X(629).                  EL522
00077                                                                   EL522
00078  FD  EXTRACT-DATA-FILE                                            EL522
00079      BLOCK CONTAINS 0 RECORDS
00080      RECORDING MODE F.                                            EL522
00081  01  EXTRACT-DATA-FILE-RECORD    PIC X(588).                      EL522

062104 FD  ME50-EL522-BALANCE
062104     RECORDING MODE IS F
062104     BLOCK CONTAINS 0 RECORDS.
062104 01  ME50-EL522-BALANCE-REC      PIC X(95).
00082                                                                   EL522
00083  FD  PAYMENTS-AND-ADJUSTMENTS                                     EL522
00084      BLOCK CONTAINS 0 RECORDS
00085      RECORDING MODE F.                                            EL522
00086  01  PMT-AND-ADJ                 PIC X(80).                       EL522
00087                                                                   EL522
00088  EJECT                                                            EL522
00089  FD  RETRO-PAYMENTS                                               EL522
00090      BLOCK CONTAINS 0 RECORDS
00091      RECORDING MODE F.                                            EL522
00092  01  RETRO-PMTS                  PIC X(200).                      EL522
00093                                                                   EL522
00094  EJECT                                                            EL522
00095  FD  ELREPT.                                                      EL522
00096                                  COPY ELCREPT.                    EL522
00097                                                                   EL522
00098  EJECT                                                            EL522
00099  FD  DISK-DATE                                                    EL522
00100                                  COPY ELCDTEFD.                   EL522
00101                                                                   EL522
00102  EJECT                                                            EL522
00103  FD  FICH                                                         EL522
00104                                  COPY ELCFCHFD.                   EL522
00105                                                                   EL522
00106  EJECT                                                            EL522
00107                                                                   EL522
00108  FD  ERMEBL.                                                      EL522
00109                                  COPY ERCMEBL.                    EL522
00110                                                                   EL522
00111      EJECT                                                        EL522
00112  SD  SORT-WORK-FILE.                                              EL522
00113                                                                   EL522
00114  01  SORT-RECORD.                                                 EL522
00115      12  SORT-KEY.                                                EL522
00116          16  S-CARRIER           PIC X.                           EL522
00117          16  S-GROUPING          PIC X(6).                        EL522
00118          16  S-STATE             PIC XX.                          EL522
00119          16  S-ACCOUNT           PIC X(10).                       EL522
00120          16  S-CERT-EFF-DT       PIC XX.                          EL522
00121          16  S-CERT-NO           PIC X(11).                       EL522
00122          16  S-ED-RECORD-TYPE    PIC X.                           EL522
00123          16  S-RECORD-TYPE       PIC X.                           EL522
00124          16  S-PAYMENT-CODE      PIC X.                           EL522
00125      12  S-EXTRACT-DATA-RECORD   PIC X(588).                      EL522
00126                                                                   EL522
00127      EJECT                                                        EL522
00128  WORKING-STORAGE SECTION.                                         EL522
00129  77  FILLER  PIC X(32)   VALUE '********************************'.EL522
00130  77  FILLER  PIC X(32)   VALUE '*     EL522  WORKING STORAGE   *'.EL522
00131  77  FILLER  PIC X(32)   VALUE '********* VMOD=2.013 ***********'.   CL**3
00132                                                                   EL522
00133  01  SELECT-DATE-TRAP-AREA.                                       EL522
00134      12  HOLD-BATCH              PIC X(6).                        EL522
00135      12  HOLD-DATE               PIC XX.                          EL522
00136      12  HOLD-BATCH-ERROR-SW     PIC X           VALUE 'N'.       EL522
00137          88  ERROR-IN-BATCH                      VALUE 'Y'.       EL522
00138                                                                   EL522
00139  01  MONTH-END-DATA.                                              EL522
00140      12  ME-START-DATE.                                           EL522
00141          16  ME-START-MO         PIC 99.                          EL522
00142          16  FILLER              PIC X.                           EL522
00143          16  ME-START-DA         PIC 99.                          EL522
00144          16  FILLER              PIC X.                           EL522
00145          16  ME-START-YR         PIC 99.                          EL522
00146      12  ME-CNDS-DATE            PIC 9(6).                        EL522
00147      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   EL522
00148          16  ME-CNDS-MO          PIC 99.                          EL522
00149          16  ME-CNDS-DA          PIC 99.                          EL522
00150          16  ME-CNDS-YR          PIC 99.                          EL522
00151      12  ME-START-TIME           PIC 9(6).                        EL522
00152      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 EL522
00153          88  ME-DO-UPDATE        VALUE 'Y'.                       EL522
00154          88  ME-NO-UPDATE        VALUE 'N'.                       EL522
00155      12  ERMEBL-FILE-STATUS      PIC XX.                          EL522
00156      12  MONTH-END-MOYR          PIC 9999 COMP.                   EL522
00157      12  NEW-ME-REC-SW           PIC 9 VALUE ZERO.                EL522

062104 01  WS-BAL50-DESCRIPTION          PIC X(50)  VALUE
062104     'EL522 PB should be +/-15 of EL525 PB recs updated '.

062104 01  WS-ME50-TEMP-BAL-AMT        PIC S9(9)       VALUE ZERO.      EL522
062104 01  WS-ME50-PEND-BUS-RECS       PIC S9(9)       VALUE ZERO.      EL522
062104 01  WS-ME50-BATCH-TRAILERS      PIC S9(9)       VALUE ZERO.      EL522
062104 01  WS-ME50-VALID-ISSUES        PIC S9(9)       VALUE ZERO.      EL522
062104 01  WS-ME50-VALID-CANCELS       PIC S9(9)       VALUE ZERO.      EL522

062104 01  WS-ME50-BALANCE-REC.
062104     12  WS-ME50-BAL-JOB           PIC X(11)  VALUE SPACES.
062104     12  WS-ME50-BAL-DELIM1        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-STEP          PIC X(08)  VALUE 'EL522   '.
062104     12  WS-ME50-BAL-DELIM2        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-AMT-LOW       PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME50-BAL-DELIM3        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-AMT-HIGH      PIC ZZZ,ZZZ,ZZ9.
062104     12  WS-ME50-BAL-DELIM4        PIC X(01)  VALUE ';'.
062104     12  WS-ME50-BAL-DESCRIP       PIC X(50)  VALUE SPACES.
00158  EJECT                                                            EL522
00159                                      COPY ERCEXTR.                EL522
00160  EJECT                                                            EL522
00161                                      COPY ERCEXTD.                EL522
00162  EJECT                                                            EL522
00163                                      COPY ERCPNDB.                EL522
00164  EJECT                                                            EL522
00165                                      COPY ERCPNDC.                EL522
00166  EJECT                                                            EL522
00167                                      COPY ERCCRTC.                EL522
00168  EJECT                                                            EL522
00169                                      COPY ERCPYAJ.                EL522
00170  EJECT                                                            EL522
00171                                      COPY ERCREPY.                EL522
00172  EJECT                                                            EL522
00173  01  NEW-PB-CERT.                                                 EL522
00174      12  NEW-PB-CERT-EFF-DT      PIC XX.                          EL522
00175      12  NEW-PB-CERT-NO          PIC X(11).                       EL522
00176                                                                   EL522
00177  01  HELD-PB-CERT.                                                EL522
00178      12  HELD-PB-CERT-EFF-DT     PIC XX          VALUE SPACES.    EL522
00179      12  HELD-PB-CERT-NO         PIC X(11)       VALUE SPACES.    EL522
00180                                                                   EL522
00181  01  WORK-CLAIM-CONTROL.                                          EL522
00182      12  WORK-P-CLAIM-TYPE       PIC 9.                           EL522
00183      12  WORK-P-CHECK-NUMBER     PIC X(5).                        EL522
00184                                                                   EL522
00185  01  TEMP-P-CHECK-NUMBER.                                         EL522
00186      12  FILLER                  PIC XX.                          EL522
00187      12  TEMP-P-CK-NO            PIC X(5).                        EL522
00188                                                                   EL522
00189  01  FILLER              COMP-3.                                  EL522
00190      12  WS-LINE-COUNT           PIC S9(3)       VALUE +99.       EL522
00191      12  WS-LINE-COUNT-MAX       PIC S9(3)       VALUE +56.       EL522
00192      12  WS-PAGE                 PIC S9(5)       VALUE ZERO.      EL522
00193      12  WS-REPORT-SW            PIC S9          VALUE +1.        EL522
00194      12  WS-EXTRACTS-READ        PIC S9(9)       VALUE ZERO.      EL522
00195      12  WS-PEND-BUS-RECORDS     PIC S9(9)       VALUE ZERO.      EL522
00196      12  WS-BATCH-TRAILERS       PIC S9(9)       VALUE ZERO.      EL522
00197      12  WS-GOOD-ISSUES          PIC S9(9)       VALUE ZERO.      EL522
00198      12  WS-GOOD-CANCELS         PIC S9(9)       VALUE ZERO.      EL522
00199      12  WS-BAD-ISSUES           PIC S9(9)       VALUE ZERO.      EL522
00200      12  WS-BAD-CANCELS          PIC S9(9)       VALUE ZERO.      EL522
00201      12  WS-RETURNED-ISSUES      PIC S9(9)       VALUE ZERO.      EL522
00202      12  WS-RETURNED-CANCELS     PIC S9(9)       VALUE ZERO.      EL522
00203      12  WS-CLAIMS-RECORDS       PIC S9(9)       VALUE ZERO.      EL522
00204      12  WS-CLAIMS-PAMTS         PIC S9(9)       VALUE ZERO.      EL522
00205      12  WS-CLAIMS-EXPENSE       PIC S9(9)       VALUE ZERO.      EL522
00206      12  WS-CLAIMS-RESERVE       PIC S9(9)       VALUE ZERO.      EL522
00207      12  WS-CLAIMS-PMT-BAD       PIC S9(9)       VALUE ZERO.      EL522
00208      12  WS-CLAIMS-EXP-BAD       PIC S9(9)       VALUE ZERO.      EL522
00209      12  WS-CLAIMS-RSV-BAD       PIC S9(9)       VALUE ZERO.      EL522
00210      12  WS-CHANGE-COUNT         PIC S9(9)       VALUE ZERO.      EL522
00211      12  WS-TRANS-OUTPUT-COUNT   PIC S9(9)       VALUE ZERO.      EL522
00212      12  WS-PAYMTS-COUNT         PIC S9(9)       VALUE ZERO.      EL522
00213      12  WS-PYMTS-OUTPUT-COUNT   PIC S9(9)       VALUE ZERO.      EL522
00214      12  WS-RETROS-COUNT         PIC S9(9)       VALUE ZERO.      EL522
00215      12  WS-RETRO-OUTPUT-COUNT   PIC S9(9)       VALUE ZERO.      EL522
00216      12  WS-RETURN-CODE          PIC S9(3)       VALUE ZERO.      EL522
00217      12  WS-ZERO                 PIC S9          VALUE ZERO.      EL522
00218                                                                   EL522
00219  01  FINAL-TOTALS-PRINT-TITLES.                                   EL522
00220      12  FTP-TITLE-1             PIC X(55)           VALUE        EL522
00221      'EXTRACT FILE RECORDS READ..............................'.   EL522
00222      12  FTP-TITLE-2             PIC X(55)           VALUE        EL522
00223      'PENDING BUSINESS EXTRACTS READ.........................'.   EL522
00224      12  FTP-TITLE-2A            PIC X(55)           VALUE        EL522
00225      'PENDING BUSINESS BATCH TRAILERS........................'.   EL522
00226      12  FTP-TITLE-2B            PIC X(55)           VALUE        EL522
00227      'PENDING BUSINESS VALID ISSUES..........................'.   EL522
00228      12  FTP-TITLE-2C            PIC X(55)           VALUE        EL522
00229      'PENDING BUSINESS VALID CANCELS.........................'.   EL522
00230      12  FTP-TITLE-2D            PIC X(55)           VALUE        EL522
00231      'PENDING BUSINESS ISSUES IN ERROR.......................'.   EL522
00232      12  FTP-TITLE-2E            PIC X(55)           VALUE        EL522
00233      'PENDING BUSINESS CANCELS IN ERROR......................'.   EL522
00234      12  FTP-TITLE-2F            PIC X(55)           VALUE        EL522
00235      'PENDING BUSINESS ISSUES RETURNED TO ACCOUNT............'.   EL522
00236      12  FTP-TITLE-2G            PIC X(55)           VALUE        EL522
00237      'PENDING BUSINESS CANCELS RETURNED TO ACCOUNT...........'.   EL522
00238      12  FTP-TITLE-3             PIC X(55)           VALUE        EL522
00239      'PENDING CLAIMS EXTRACTS READ...........................'.   EL522
00240      12  FTP-TITLE-3A            PIC X(55)           VALUE        EL522
00241      'PENDING CLAIMS VALID PAYMENTS..........................'.   EL522
00242      12  FTP-TITLE-3B            PIC X(55)           VALUE        EL522
00243      'PENDING CLAIMS VALID EXPENSE PAYMENTS..................'.   EL522
00244      12  FTP-TITLE-3C            PIC X(55)           VALUE        EL522
00245      'PENDING CLAIMS VALID RESERVES..........................'.   EL522
00246      12  FTP-TITLE-3D            PIC X(55)           VALUE        EL522
00247      'PENDING CLAIMS PAYMENTS IN ERROR.......................'.   EL522
00248      12  FTP-TITLE-3E            PIC X(55)           VALUE        EL522
00249      'PENDING CLAIMS EXPENSE PAYMENTS IN ERROR...............'.   EL522
00250      12  FTP-TITLE-3F            PIC X(55)           VALUE        EL522
00251      'PENDING CLAIMS RESERVES IN ERROR.......................'.   EL522
00252      12  FTP-TITLE-4             PIC X(55)           VALUE        EL522
00253      'CERTIFICATE CHANGE EXTRACTS READ.......................'.   EL522
00254      12  FTP-TITLE-5             PIC X(55)           VALUE        EL522
00255      'PAYMENT AND ADJUSTMENT EXTRACTS READ...................'.   EL522
00256      12  FTP-TITLE-6             PIC X(55)           VALUE        EL522
00257      'RETRO/REINSURANCE ADJUSTMENT EXTRACTS READ.............'.   EL522
00258      12  FTP-TITLE-7             PIC X(55)           VALUE        EL522
00259      'PAYMENT AND ADJUSTMENT EXTRACTS PASSED TO ECS061.......'.   EL522
00260      12  FTP-TITLE-8             PIC X(55)           VALUE        EL522
00261      'RETRO/REINSURANCE ADJUSTMENT EXTRACTS PASSED TO ECS041.'.   EL522
00262      12  FTP-TITLE-9             PIC X(55)           VALUE        EL522
00263      'PENDING BUSINESS EXTRACTS PASSED TO ECS010.............'.   EL522
00264                                                                   EL522
00265  01  FINAL-TOTALS-PRINT-LINE.                                     EL522
00266      12  FILLER              PIC X(5)            VALUE SPACES.    EL522
00267      12  FTP-TITL            PIC X(55).                           EL522
00268      12  FILLER              PIC X(4)            VALUE SPACES.    EL522
00269      12  FTP-TOTL-1          PIC ZZZZ,ZZZ,ZZZ-   BLANK WHEN ZERO. EL522
00270      12  FILLER              PIC XXX             VALUE SPACES.    EL522
00271      12  FTP-TOTL-2          PIC ZZZZ,ZZZ,ZZZ-   BLANK WHEN ZERO. EL522
00272      12  FILLER              PIC XXX             VALUE SPACES.    EL522
00273      12  FTP-TOTL-3          PIC ZZZZ,ZZZ,ZZZ-   BLANK WHEN ZERO. EL522
00274      12  FILLER              PIC XXX             VALUE SPACES.    EL522
00275      12  FTP-TOTL-4          PIC ZZZZ,ZZZ,ZZZ-   BLANK WHEN ZERO. EL522
00276                                                                   EL522
00277  01  FILLER              COMP   SYNC.                             EL522
00278      12  PGM-SUB             PIC S9(4)           VALUE +522.      EL522
00279      12  WS-INDEX            PIC S9(4)           VALUE ZERO.      EL522
00280      12  SUB1                PIC S9(4)           VALUE ZERO.      EL522
00281                                                                   EL522
00282  01  FILLER.                                                      EL522
00283      12  ABEND-CODE              PIC X(4).                        EL522
00284      12  ABEND-OPTION            PIC X.                           EL522
00285      12  OLC-REPORT-NAME         PIC X(5)        VALUE 'EL522'.   EL522
00286      12  X                       PIC X           VALUE SPACE.     EL522
00287      12  WS-SAVE-PRINT-RECORD    PIC X(133)      VALUE SPACES.    EL522
00288      12  WS-DAYS-DISAB           PIC 9(3)        VALUE ZERO.      EL522
00289      12  WS-LAST-CARRIER         PIC X           VALUE SPACES.    EL522
00290      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    EL522
00291      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      EL522
00292      12  WS-RUN-DATE-BIN         PIC XX.                          EL522
00293      12  WS-FILE-ERROR-MESSAGE.                                   EL522
00294          16  FILLER              PIC X(24)       VALUE            EL522
00295                  'ERROR OCCURED OPENING - '.                      EL522
00296          16  WS-FEM-FILE-NAME    PIC X(8).                        EL522
00297  EJECT                                                            EL522
00298  01  PYMT-REC.                                                    EL522
00299      12  PYMT-CONTROL.                                            EL522
00300          16  PYMT-CODE           PIC XXX.                         EL522
00301              88  CLA-CARD                        VALUE 'CLA'.     EL522
00302              88  CLT-CARD                        VALUE 'CLT'.     EL522
00303          16  PYMT-CARR           PIC X.                           EL522
00304          16  PYMT-GROUP          PIC X(6).                        EL522
00305          16  PYMT-RESP           PIC X(10).                       EL522
00306          16  PYMT-ACCT           PIC X(10).                       EL522
00307      12  FILLER                  PIC X(8).                        EL522
00308      12  PYMT-DESC               PIC X(30).                       EL522
00309      12  PYMT-TYPE               PIC X.                           EL522
00310      12  PYMT-AMT                PIC S9(7)V99.                    EL522
00311      12  FILLER                  PIC X.                           EL522
00312      12  PYMT-BILL-FLAG          PIC X.                           EL522
00313          88  PYMT-BILLED                         VALUE 'B'.       EL522
00314      12  PYMT-AR-FLAG            PIC X.                           EL522
00315          88  PYMT-CYCLE-BILL                     VALUE 'C'.       EL522
00316                                                                   EL522
00317  EJECT                                                            EL522
00318  01  TOTAL-ACCUMS        COMP-3.                                  EL522
00319      12  TOT-LIFE-PREM           PIC S9(9)V99     VALUE +0.       EL522
00320      12  TOT-AH-PREM             PIC S9(9)V99     VALUE +0.       EL522
00321      12  TOT-LIFE-REFUND         PIC S9(9)V99     VALUE +0.       EL522
00322      12  TOT-AH-REFUND           PIC S9(9)V99     VALUE +0.       EL522
00323      12  TOT-DEATH               PIC S9(9)V99     VALUE +0.       EL522
00324      12  TOT-DIS                 PIC S9(9)V99     VALUE +0.       EL522
00325      12  TOT-OB                  PIC S9(9)V99     VALUE +0.       EL522
00326      12  TOT-OBDIS               PIC S9(9)V99     VALUE +0.       EL522
00327      12  TOT-DEATH-EXP           PIC S9(9)V99     VALUE +0.       EL522
00328      12  TOT-DIS-EXP             PIC S9(9)V99     VALUE +0.       EL522
00329      12  TOT-OB-EXP              PIC S9(9)V99     VALUE +0.       EL522
00330      12  TOT-OBDIS-EXP           PIC S9(9)V99     VALUE +0.       EL522
00331      12  TOT-LFRSV               PIC S9(9)V99     VALUE +0.       EL522
00332      12  TOT-AHRSV               PIC S9(9)V99     VALUE +0.       EL522
00333      12  TOT-C-REMIT-RECEIVED    PIC S9(9)V99     VALUE +0.       EL522
00334      12  TOT-C-DEPOSIT           PIC S9(9)V99     VALUE +0.       EL522
00335      12  TOT-C-CHARGE-TO-AGENT   PIC S9(9)V99     VALUE +0.       EL522
00336      12  TOT-C-ADJ-REM-RECEIVED  PIC S9(9)V99     VALUE +0.       EL522
00337      12  TOT-C-ADJ-DEPOSIT       PIC S9(9)V99     VALUE +0.       EL522
00338      12  TOT-C-ADJ-CHG-TO-AGENT  PIC S9(9)V99     VALUE +0.       EL522
00339      12  TOT-C-ADD-TO-YTD-COMP   PIC S9(9)V99     VALUE +0.       EL522
00340      12  TOT-C-SUB-YTD-COMP      PIC S9(9)V99     VALUE +0.       EL522
00341      12  TOT-C-ADD-TO-BALANCE    PIC S9(9)V99     VALUE +0.       EL522
00342      12  TOT-C-FICA-ENTRY        PIC S9(9)V99     VALUE +0.       EL522
00343      12  TOT-B-REMIT-RECEIVED    PIC S9(9)V99     VALUE +0.       EL522
00344      12  TOT-B-DEPOSIT           PIC S9(9)V99     VALUE +0.       EL522
00345      12  TOT-B-CHARGE-TO-AGENT   PIC S9(9)V99     VALUE +0.       EL522
00346      12  TOT-B-ADJ-REM-RECEIVED  PIC S9(9)V99     VALUE +0.       EL522
00347      12  TOT-B-ADJ-DEPOSIT       PIC S9(9)V99     VALUE +0.       EL522
00348      12  TOT-B-ADJ-CHG-TO-AGENT  PIC S9(9)V99     VALUE +0.       EL522
00349      12  TOT-B-ADD-TO-YTD-COMP   PIC S9(9)V99     VALUE +0.       EL522
00350      12  TOT-B-SUB-YTD-COMP      PIC S9(9)V99     VALUE +0.       EL522
00351      12  TOT-B-ADD-TO-BALANCE    PIC S9(9)V99     VALUE +0.       EL522
00352      12  TOT-B-FICA-ENTRY        PIC S9(9)V99     VALUE +0.       EL522
00353      12  TOT-T-REMIT-RECEIVED    PIC S9(9)V99     VALUE +0.       EL522
00354      12  TOT-T-DEPOSIT           PIC S9(9)V99     VALUE +0.       EL522
00355      12  TOT-T-CHARGE-TO-AGENT   PIC S9(9)V99     VALUE +0.       EL522
00356      12  TOT-T-ADJ-REM-RECEIVED  PIC S9(9)V99     VALUE +0.       EL522
00357      12  TOT-T-ADJ-DEPOSIT       PIC S9(9)V99     VALUE +0.       EL522
00358      12  TOT-T-ADJ-CHG-TO-AGENT  PIC S9(9)V99     VALUE +0.       EL522
00359      12  TOT-T-ADD-TO-YTD-COMP   PIC S9(9)V99     VALUE +0.       EL522
00360      12  TOT-T-SUB-YTD-COMP      PIC S9(9)V99     VALUE +0.       EL522
00361      12  TOT-T-ADD-TO-BALANCE    PIC S9(9)V99     VALUE +0.       EL522
00362      12  TOT-T-FICA-ENTRY        PIC S9(9)V99     VALUE +0.       EL522
00363      12  TOT-LIFE-PREM-W         PIC S9(9)V99     VALUE +0.       EL522
00364      12  TOT-AH-PREM-W           PIC S9(9)V99     VALUE +0.       EL522
00365      12  TOT-LIFE-REFUND-W       PIC S9(9)V99     VALUE +0.       EL522
00366      12  TOT-AH-REFUND-W         PIC S9(9)V99     VALUE +0.       EL522
00367      12  TOT-DEATH-W             PIC S9(9)V99     VALUE +0.       EL522
00368      12  TOT-DIS-W               PIC S9(9)V99     VALUE +0.       EL522
00369      12  TOT-OB-W                PIC S9(9)V99     VALUE +0.       EL522
00370      12  TOT-OBDIS-W             PIC S9(9)V99     VALUE +0.       EL522
00371      12  TOT-DEATH-EXP-W         PIC S9(9)V99     VALUE +0.       EL522
00372      12  TOT-DIS-EXP-W           PIC S9(9)V99     VALUE +0.       EL522
00373      12  TOT-OB-EXP-W            PIC S9(9)V99     VALUE +0.       EL522
00374      12  TOT-OBDIS-EXP-W         PIC S9(9)V99     VALUE +0.       EL522
00375      12  TOT-LFRSV-W             PIC S9(9)V99     VALUE +0.       EL522
00376      12  TOT-AHRSV-W             PIC S9(9)V99     VALUE +0.       EL522
00377      12  TOT-LIFE-PREM-R         PIC S9(9)V99     VALUE +0.       EL522
00378      12  TOT-AH-PREM-R           PIC S9(9)V99     VALUE +0.       EL522
00379      12  TOT-LIFE-REFUND-R       PIC S9(9)V99     VALUE +0.       EL522
00380      12  TOT-AH-REFUND-R         PIC S9(9)V99     VALUE +0.       EL522
00381      12  TOT-LIFE-MORTALITY-AMT  PIC S9(9)V99     VALUE +0.       EL522
00382      12  TOT-LF-AMT-INFORCE      PIC S9(9)V99     VALUE +0.       EL522
00383      12  TOT-AH-AMT-INFORCE      PIC S9(9)V99     VALUE +0.       EL522
00384      12  TOT-FUTURE-RESERVE      PIC S9(9)V99     VALUE +0.       EL522
00385      12  TOT-PTC-RESERVE         PIC S9(9)V99     VALUE +0.       EL522
00386      12  TOT-IBNR-RESERVE        PIC S9(9)V99     VALUE +0.       EL522
00387      12  TOT-CLAIM-ADJ-AMT       PIC S9(9)V99     VALUE +0.       EL522
00388      12  TOT-EXPENSES            PIC S9(9)V99     VALUE +0.       EL522
00389      12  TOT-PAYMENTS            PIC S9(9)V99     VALUE +0.       EL522
00390      12  TOT-OTHER-COMM          PIC S9(9)V99     VALUE +0.       EL522
00391      12  TOT-REIN-PREM-ADJ       PIC S9(9)V99     VALUE +0.       EL522
00392      12  WS-NET-TOTAL            PIC S9(9)V99     VALUE +0.       EL522
00393  EJECT                                                            EL522
00394  01  WS-HEADING1.                                                 EL522
00395      12  FILLER                  PIC X(41)       VALUE '1'.       EL522
00396      12  WS-H1-TITLE             PIC X(79)       VALUE            EL522
00397             'PENDING ACTIVITY TO UPDATE CERTIFICATE MASTER FILE'. EL522
00398      12  WS-H1-REPORT-NUMBER     PIC X(9)        VALUE 'EL522'.   EL522
00399                                                                   EL522
00400  01  WS-HEADING2.                                                 EL522
00401      12  FILLER                  PIC X(46)       VALUE SPACES.    EL522
00402      12  WS-H2-CLIENT-NAME       PIC X(74)       VALUE SPACES.    EL522
00403      12  WS-H2-DATE              PIC X(8).                        EL522
00404                                                                   EL522
00405  01  WS-HEADING3.                                                 EL522
00406      12  FILLER              PIC X(53)           VALUE SPACES.    EL522
00407      12  WS-H3-DATE          PIC X(67)           VALUE SPACES.    EL522
00408      12  FILLER              PIC X(5)            VALUE 'PAGE'.    EL522
00409      12  WS-H3-PAGE          PIC ZZ,ZZ9.                          EL522
00410      12  FILLER              PIC X(11)           VALUE SPACES.    EL522
00411                                                                   EL522
00412  01  WS-HEADING4.                                                 EL522
00413      12  WS-H4-CTL           PIC X               VALUE '0'.       EL522
00414      12  WS-H4-TITLE         PIC X(43).                           EL522
00415      12  FILLER              PIC X(89)           VALUE SPACES.    EL522
00416                                                                   EL522
00417  01  WS-H4-TITLES.                                                EL522
00418      12  WS-H4-TITLE-1       PIC X(43)           VALUE            EL522
00419              'RELEASED TO MONTH END UPDATE               '.       EL522
00420      12  WS-H4-TITLE-2       PIC X(43)           VALUE            EL522
00421              'RETURNED TO PENDING BUSINESS FILE          '.       EL522
00422      12  WS-H4-TITLE-3       PIC X(43)           VALUE            EL522
00423              'RELEASED TO PAYMENTS AND ADJUSTMENTS       '.       EL522
00424      12  WS-H4-TITLE-4       PIC X(43)           VALUE            EL522
00425              'RELEASED TO RETRO PMTS AND REIN ADJUSTMENTS'.       EL522
00426      12  WS-H4-TITLE-5       PIC X(43)           VALUE            EL522
00427              'RETURNED TO ACCOUNT                        '.       EL522
00428                                                                   EL522
00429  01  WS-HEADING5.                                                 EL522
00430      12  FILLER              PIC X(133).                          EL522
00431                                                                   EL522
00432  01  WS-HEADING5-A.                                               EL522
00433      12  FILLER              PIC X     VALUE '0'.                 EL522
00434      12  FILLER              PIC XXX   VALUE SPACE.               EL522
00435      12  HDG5A-OVRD-1        PIC X(6).                            EL522
00436      12  FILLER              PIC X(10) VALUE ' ISSUE    '.        EL522
00437      12  HDG5A-OVRD-2        PIC X(6).                            EL522
00438      12  FILLER              PIC X(09) VALUE ' ISSUE   '.         EL522
00439      12  HDG5A-OVRD-3        PIC X(6).                            EL522
00440      12  FILLER              PIC X(10) VALUE ' REFUND   '.        EL522
00441      12  HDG5A-OVRD-4        PIC X(6).                            EL522
00442      12  FILLER              PIC X(10) VALUE ' REFUND   '.        EL522
00443      12  HDG5A-OVRD-5        PIC X(6).                            EL522
00444      12  FILLER              PIC X(10) VALUE ' CLAIM    '.        EL522
00445      12  HDG5A-OVRD-6        PIC X(6).                            EL522
00446      12  FILLER              PIC X(13) VALUE ' CLAIM    OB '.     EL522
00447      12  HDG5A-OVRD-7        PIC X(6).                            EL522
00448      12  FILLER              PIC X(10) VALUE ' CLM   OB '.        EL522
00449      12  HDG5A-OVRD-8        PIC X(6).                            EL522
00450      12  FILLER              PIC X(09) VALUE ' CLM     '.         EL522
00451                                                                   EL522
00452  01  WS-HEADING5-B.                                               EL522
00453      12  FILLER              PIC X     VALUE '0'.                 EL522
00454      12  FILLER              PIC X(34) VALUE SPACES.              EL522
00455      12  HDG5B-OVRD-3        PIC X(6).                            EL522
00456      12  FILLER              PIC X(10) VALUE ' RESERVE  '.        EL522
00457      12  HDG5B-OVRD-4        PIC X(6).                            EL522
00458      12  FILLER              PIC X(10) VALUE ' RESERVE  '.        EL522
00459      12  HDG5B-OVRD-5        PIC X(6).                            EL522
00460      12  FILLER              PIC X(10) VALUE ' EXPENSE  '.        EL522
00461      12  HDG5B-OVRD-6        PIC X(6).                            EL522
00462      12  FILLER              PIC X(13) VALUE ' EXPENSE  OB-'.     EL522
00463      12  HDG5B-OVRD-7        PIC X(2).                            EL522
00464      12  FILLER              PIC X(14) VALUE ' EXPENSE   OB-'.    EL522
00465      12  HDG5B-OVRD-8        PIC X(2).                            EL522
00466      12  FILLER              PIC X(13) VALUE ' EXPENSE     '.     EL522
00467                                                                   EL522
00468  01  WS-HEADING5-C.                                               EL522
00469      12  FILLER              PIC X               VALUE '0'.       EL522
00470      12  FILLER              PIC X(44)           VALUE            EL522
00471              '                         PREVIOUSLY BILLED  '.      EL522
00472      12  FILLER              PIC X(44)           VALUE            EL522
00473              '                            PULLED AT MONTH '.      EL522
00474      12  FILLER              PIC X(44)           VALUE            EL522
00475              'END                 MONTHLY TOTALS          '.      EL522
00476                                                                   EL522
00477  01  WS-HEADING5-C1.                                              EL522
00478      12  FILLER              PIC X               VALUE '0'.       EL522
00479      12  FILLER              PIC X(44)           VALUE            EL522
00480              '                                      ENTERE'.      EL522
00481      12  FILLER              PIC X(44)           VALUE            EL522
00482              'D           NET             ENTERED         '.      EL522
00483      12  FILLER              PIC X(44)           VALUE            EL522
00484              '   NET            ENTERED            NET    '.      EL522
00485                                                                   EL522
00486  01  WS-HEADING5-D1.                                              EL522
00487      12  FILLER              PIC X               VALUE '0'.       EL522
00488      12  FILLER              PIC X(33) VALUE SPACES.              EL522
00489      12  FILLER              PIC X(17) VALUE 'MORTALITY RESRV  '. EL522
00490      12  HDG5D1-OVRD-4       PIC X(6).                            EL522
00491      12  FILLER              PIC X(10) VALUE ' INFORCE  '.        EL522
00492      12  HDG5D1-OVRD-5       PIC X(6).                            EL522
00493      12  FILLER              PIC X(10) VALUE ' INFORCE  '.        EL522
00494      12  FILLER              PIC X(50) VALUE SPACES.              EL522
00495                                                                   EL522
00496  01  WS-HEADING5-D2.                                              EL522
00497      12  FILLER              PIC X               VALUE '0'.       EL522
00498      12  FILLER              PIC X(44)           VALUE            EL522
00499              '                                 FUTURE RESE'.      EL522
00500      12  FILLER              PIC X(44)           VALUE            EL522
00501              'RVE  PAY TO CUR RES  IBNR RESERVE   CLAIMS A'.      EL522
00502      12  FILLER              PIC X(44)           VALUE            EL522
00503              'DJUST.                                      '.      EL522
00504                                                                   EL522
00505  01  WS-HEADING5-D3.                                              EL522
00506      12  FILLER              PIC X               VALUE '0'.       EL522
00507      12  FILLER              PIC X(44)           VALUE            EL522
00508              '                                 RETRO EXPEN'.      EL522
00509      12  FILLER              PIC X(44)           VALUE            EL522
00510              'SES  RETRO PAYMENTS  RETRO OTH COMM  REIN. A'.      EL522
00511      12  FILLER              PIC X(44)           VALUE            EL522
00512              'DJUST.                                      '.      EL522
00513                                                                   EL522
00514  01  WS-HEADING5-R.                                               EL522
00515      12  FILLER              PIC X               VALUE '0'.       EL522
00516      12  FILLER              PIC XXX   VALUE SPACE.               EL522
00517      12  HDG5R-OVRD-1        PIC X(6).                            EL522
00518      12  FILLER              PIC X(10) VALUE ' ISSUE    '.        EL522
00519      12  HDG5R-OVRD-2        PIC X(6).                            EL522
00520      12  FILLER              PIC X(09) VALUE ' ISSUE   '.         EL522
00521      12  HDG5R-OVRD-3        PIC X(6).                            EL522
00522      12  FILLER              PIC X(10) VALUE ' REFUND   '.        EL522
00523      12  HDG5R-OVRD-4        PIC X(6).                            EL522
00524      12  FILLER              PIC X(76) VALUE ' REFUND   '.        EL522
00525                                                                   EL522
00526  01  WS-DETAIL1.                                                  EL522
00527      12  FILLER              PIC X               VALUE SPACE.     EL522
00528      12  WS-D-AMT1           PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00529      12  FILLER              PIC X               VALUE SPACE.     EL522
00530      12  WS-D-AMT2           PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00531      12  FILLER              PIC X               VALUE SPACE.     EL522
00532      12  WS-D-AMT3           PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00533      12  FILLER              PIC X               VALUE SPACE.     EL522
00534      12  WS-D-AMT4           PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00535      12  FILLER              PIC X               VALUE SPACE.     EL522
00536      12  WS-D-AMT5           PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00537      12  FILLER              PIC X               VALUE SPACE.     EL522
00538      12  WS-D-AMT6           PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00539      12  FILLER              PIC X               VALUE SPACE.     EL522
00540      12  WS-D-AMT7           PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00541      12  FILLER              PIC X               VALUE SPACE.     EL522
00542      12  WS-D-AMT8           PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00543      12  FILLER              PIC X(4)            VALUE SPACE.     EL522
00544                                                                   EL522
00545  01  WS-DETAIL2.                                                  EL522
00546      12  FILLER              PIC X(33)           VALUE SPACE.     EL522
00547      12  WS-D2-AMT3          PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00548      12  FILLER              PIC X               VALUE SPACE.     EL522
00549      12  WS-D2-AMT4          PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00550      12  FILLER              PIC X               VALUE SPACE.     EL522
00551      12  WS-D2-AMT5          PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00552      12  FILLER              PIC X               VALUE SPACE.     EL522
00553      12  WS-D2-AMT6          PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00554      12  FILLER              PIC X               VALUE SPACE.     EL522
00555      12  WS-D2-AMT7          PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00556      12  FILLER              PIC X               VALUE SPACE.     EL522
00557      12  WS-D2-AMT8          PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00558      12  FILLER              PIC X(4)            VALUE SPACE.     EL522
00559                                                                   EL522
00560  01  WS-DETAIL3.                                                  EL522
00561      12  FILLER              PIC X               VALUE SPACE.     EL522
00562      12  WS-D3-DESC          PIC X(32)           VALUE SPACE.     EL522
00563      12  WS-D3-AMT1          PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00564      12  FILLER              PIC X               VALUE SPACE.     EL522
00565      12  WS-D3-AMT2          PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00566      12  FILLER              PIC XXX             VALUE SPACE.     EL522
00567      12  WS-D3-AMT3          PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00568      12  FILLER              PIC X               VALUE SPACE.     EL522
00569      12  WS-D3-AMT4          PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00570      12  FILLER              PIC XXX             VALUE SPACE.     EL522
00571      12  WS-D3-AMT5          PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00572      12  FILLER              PIC XX              VALUE SPACE.     EL522
00573      12  WS-D3-AMT6          PIC ZZZ,ZZZ,ZZZ.99-.                 EL522
00574  EJECT                                                            EL522
00575                                      COPY ELCDTECX.               EL522
00576  EJECT                                                            EL522
00577                                      COPY ELCDTEVR.               EL522
00578  EJECT                                                            EL522
00579                                      COPY ELCDATE.                   CL**4
00580  EJECT                                                            EL522
00581  PROCEDURE DIVISION.                                              EL522
00582                                                                   EL522
00583  0000-DATE-CARD-READ SECTION.    COPY ELCDTERX.                   EL522
00584                                                                   EL522
00585      MOVE +99                    TO  WS-LINE-COUNT.               EL522
00586      MOVE +56                    TO  WS-LINE-COUNT-MAX.           EL522
00587                                                                   EL522
00588      MOVE WS-CURRENT-DATE        TO  WS-H2-DATE                   EL522
00589      MOVE COMPANY-NAME           TO  WS-H2-CLIENT-NAME            EL522
00590      MOVE ALPH-DATE              TO  WS-H3-DATE.                  EL522
00591                                                                   EL522
00592      OPEN I-O ERMEBL.                                             EL522
00593                                                                   EL522
00594      IF ERMEBL-FILE-STATUS  NOT = '00'  AND  '97'                 EL522
00595          MOVE 'N'                TO ME-UPDATE-FLAG.               EL522
00596                                                                   EL522
00597      MOVE WS-TIME                TO ME-START-TIME.                EL522
00598      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                EL522
00599      MOVE ME-START-MO            TO ME-CNDS-MO.                   EL522
00600      MOVE ME-START-DA            TO ME-CNDS-DA.                   EL522
00601      MOVE ME-START-YR            TO ME-CNDS-YR.                   EL522
00602                                                                   EL522
00603  0800-READ-ERMEBL.                                                EL522
00604      MOVE DTE-CLIENT             TO ME-COMPANY.                   EL522
00605                                                                   EL522
00606                                                                   EL522
00607      COMPUTE MONTH-END-MOYR = RUN-CCYY * 12 + RUN-MO.             EL522
00608      MOVE MONTH-END-MOYR         TO ME-MOYR.                      EL522
00609                                                                   EL522
00610      IF ME-DO-UPDATE                                              EL522
00611          READ ERMEBL INVALID KEY                                  EL522
00612          MOVE 'N'                TO ME-UPDATE-FLAG                EL522
00613          CLOSE ERMEBL.                                            EL522
00614                                                                   EL522
00615      MOVE LOW-VALUES  TO  HOLD-BATCH  HOLD-DATE.                  EL522
00616      MOVE 'N'         TO  HOLD-BATCH-ERROR-SW.                    EL522
00617                                                                   EL522
00618  0900-MOVE-HEADING-OVERRIDES.                                     EL522
00619      MOVE LIFE-OVERRIDE-L6       TO HDG5A-OVRD-1                  EL522
00620                                     HDG5A-OVRD-3                  EL522
00621                                     HDG5A-OVRD-5                  EL522
00622                                     HDG5A-OVRD-7                  EL522
00623                                     HDG5B-OVRD-3                  EL522
00624                                     HDG5B-OVRD-5                  EL522
00625                                     HDG5D1-OVRD-4.                EL522
00626                                                                   EL522
00627      MOVE   AH-OVERRIDE-L6       TO HDG5A-OVRD-2                  EL522
00628                                     HDG5A-OVRD-4                  EL522
00629                                     HDG5A-OVRD-6                  EL522
00630                                     HDG5A-OVRD-8                  EL522
00631                                     HDG5B-OVRD-4                  EL522
00632                                     HDG5B-OVRD-6                  EL522
00633                                     HDG5D1-OVRD-5.                EL522
00634                                                                   EL522
00635      MOVE LIFE-OVERRIDE-L2       TO HDG5B-OVRD-7.                 EL522
00636      MOVE   AH-OVERRIDE-L2       TO HDG5B-OVRD-8.                 EL522

062104     IF DTE-CLIENT = 'CID'
062104         MOVE 'CILGM15'          TO  WS-ME50-BAL-JOB
062104     ELSE
062104         MOVE 'CIDCLGM15'        TO  WS-ME50-BAL-JOB
062104     END-IF.

00637  EJECT                                                            EL522
00638  1000-MAIN-LOGIC SECTION.                                         EL522
00639      PERFORM 8900-OPEN-FILES THRU 8999-OFS-XIT.                   EL522
00640                                                                   EL522
00641      SORT SORT-WORK-FILE  ON ASCENDING KEY  SORT-KEY              EL522
00642          INPUT PROCEDURE 1100-MAIN-LOGIC                          EL522
00643          OUTPUT PROCEDURE 8000-WRITE-ED-FILE.                     EL522
00644                                                                   EL522
00645      IF SORT-RETURN NOT = ZERO AND 4                              EL522
00646          MOVE 'SORT FAILED'      TO  WS-ABEND-MESSAGE             EL522
00647          MOVE SORT-RETURN        TO  WS-RETURN-CODE               EL522
00648          GO TO ABEND-PGM.                                         EL522
00649                                                                   EL522
00650      PERFORM 9000-CLOSE-FILES THRU 9099-CFS-XIT.                  EL522
00651                                                                   EL522
00652      IF ME-DO-UPDATE                                              EL522
00653          MOVE ME-START-TIME      TO ME-522-START                  EL522
00654          MOVE ME-CNDS-DATE       TO ME-522-RUN-DT                 EL522
00655          ADD TOT-DEATH    TOT-DEATH-EXP                           EL522
00656              TOT-DEATH-W  TOT-DEATH-EXP-W                         EL522
00657              TOT-OB       TOT-OB-EXP                              EL522
00658              TOT-OB-W     TOT-OB-EXP-W                            EL522
00659                 GIVING ME-522-ALL-CLM-L                           EL522
00660          ADD TOT-DIS      TOT-DIS-EXP                             EL522
00661              TOT-DIS-W    TOT-DIS-EXP-W                           EL522
00662              TOT-OBDIS    TOT-OBDIS-EXP                           EL522
00663              TOT-OBDIS-W  TOT-OBDIS-EXP-W                         EL522
00664                 GIVING ME-522-ALL-CLM-AH                          EL522
00665          ACCEPT WS-TIME-OF-DAY   FROM  TIME                       EL522
00666          MOVE WS-TIME            TO ME-522-END                    EL522
00667          ADD 1                   TO ME-522-RUN-CT.                EL522
00668                                                                   EL522
00669      IF ME-DO-UPDATE                                              EL522
00670          REWRITE MONTH-END-BALANCES.                              EL522
00671                                                                   EL522
00672      IF ME-DO-UPDATE                                              EL522
00673          CLOSE ERMEBL                                             EL522
00674          DISPLAY 'MONTH-END BALANCES POSTED'                      EL522
00675      ELSE                                                         EL522
00676          DISPLAY 'MONTH-END BALANCES NOT POSTED'.                 EL522
00677                                                                   EL522
00678      MOVE ZEROS  TO RETURN-CODE.
00678      GOBACK.                                                      EL522
00679                                                                   EL522
00680  1100-MAIN-LOGIC SECTION.                                         EL522
00681      READ EXTRACT-INTERFACE-FILE INTO EXTRACT-INTERFACE-RECORD    EL522
00682          AT END                                                   EL522
00683              GO TO 1700-END-PROCESSING.                           EL522
00684                                                                   EL522
00685      ADD +1  TO  WS-EXTRACTS-READ.                                EL522
00686                                                                   EL522
00687      IF EX-EXTRACT-CODE GREATER THAN 'A'                          EL522
00688          GO TO 1700-END-PROCESSING.                               EL522
00689                                                                   EL522
00690      IF EX-RECORD-TYPE GREATER THAN 'E'                           EL522
00691          GO TO 1700-END-PROCESSING.                               EL522
00692                                                                   EL522
00693      IF EX-COMPANY-CD LESS THAN DTE-CLASIC-COMPANY-CD             EL522
00694          GO TO 1100-MAIN-LOGIC.                                   EL522
00695                                                                   EL522
00696      IF EX-COMPANY-CD GREATER THAN DTE-CLASIC-COMPANY-CD          EL522
00697          GO TO 1700-END-PROCESSING.                               EL522
00698  EJECT                                                            EL522
00699  1200-PROCESS-PEND-BUS.                                           EL522
00700      IF EX-RECORD-TYPE NOT = 'A'                                  EL522
00701          GO TO 1300-PROCESS-PEND-CLAIMS.                          EL522
00702                                                                   EL522
00703      ADD +1   TO   WS-PEND-BUS-RECORDS.                           EL522
00704                                                                   EL522
00705      MOVE EX-DATA-AREAS   TO  PENDING-BUSINESS.                   EL522
00706                                                                   EL522
00707      IF PB-CREDIT-ACCEPT-DT = WS-RUN-DATE-BIN OR                     CL**2
00708         PB-CREDIT-ACCEPT-DT = LOW-VALUES                          EL522
00709          NEXT SENTENCE                                            EL522
00710      ELSE                                                         EL522
00711          GO TO 1100-MAIN-LOGIC.                                   EL522
00712                                                                   EL522
00713      IF PB-BATCH-TRAILER                                          EL522
00714          ADD +1   TO   WS-BATCH-TRAILERS                          EL522
00715          GO TO 1100-MAIN-LOGIC.                                   EL522
00716                                                                   EL522
00717      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS                             EL522
00718          GO TO 1100-MAIN-LOGIC.                                   EL522
00719                                                                   EL522
00720      MOVE PB-CERT-EFF-DT  TO  NEW-PB-CERT-EFF-DT.                 EL522
00721      MOVE PB-CERT-NO      TO  NEW-PB-CERT-NO.                     EL522
00722                                                                   EL522
00723      IF NEW-PB-CERT NOT = HELD-PB-CERT                            EL522
00724          MOVE SPACES  TO  HELD-PB-CERT.                           EL522
00725                                                                   EL522
00726      IF PB-ISSUE                                                  EL522
00727          IF PB-RECORD-ON-HOLD                                     EL522
00728              MOVE NEW-PB-CERT  TO  HELD-PB-CERT                   EL522
00729          ELSE                                                     EL522
00730              MOVE SPACES       TO  HELD-PB-CERT.                  EL522
00731                                                                   EL522
00732      IF PB-ISSUE                                                  EL522
00733          IF PB-I-INDV-GRP-OVRD = 'G'                              EL522
00734              MOVE '2'          TO  PB-I-INDV-GRP-OVRD             EL522
00735          ELSE                                                     EL522
00736              IF PB-I-INDV-GRP-OVRD = 'I'                          EL522
00737                  MOVE '1'      TO  PB-I-INDV-GRP-OVRD.            EL522
00738                                                                   EL522
00739      IF PB-ISSUE                                                  EL522
00740          IF PB-I-INDV-GRP-OVRD = '1' OR '2'                       EL522
00741              NEXT SENTENCE                                        EL522
00742          ELSE                                                     EL522
00743              MOVE SPACE        TO  PB-I-INDV-GRP-OVRD.            EL522
00744                                                                   EL522
00745      IF PB-RECORD-ON-HOLD                                         EL522
00746          GO TO 1240-WRITE-WRAP.                                   EL522
00747                                                                   EL522
00748      IF PB-RECORD-RETURNED                                        EL522
00749          GO TO 1260-WRITE-RETURNED.                               EL522
00750                                                                   EL522
00751      IF PB-FATAL-ERRORS                                           EL522
00752          GO TO 1240-WRITE-WRAP.                                   EL522
00753                                                                   EL522
00754      IF PB-UNFORCED-ERRORS                                        EL522
00755          GO TO 1240-WRITE-WRAP.                                   EL522
00756                                                                   EL522
00757      IF HELD-PB-CERT NOT = SPACES                                 EL522
00758          IF HELD-PB-CERT = NEW-PB-CERT                            EL522
00759              GO TO 1240-WRITE-WRAP.                               EL522
00760                                                                   EL522
00761      IF PB-CANCELLATION                                           EL522
00762          GO TO 1210-BUILD-CANCELLATION.                           EL522
00763                                                                   EL522
00764      ADD +1  TO  WS-GOOD-ISSUES.                                  EL522
00765                                                                   EL522
00766      IF PB-OVERRIDE-LIFE OR PB-OVERRIDE-BOTH                      EL522
00767          MOVE PB-I-LF-PREM-CALC  TO PB-I-LF-PREMIUM-AMT           EL522
00768          MOVE PB-I-LF-ALT-PREM-CALC                               EL522
00769                                  TO PB-I-LF-ALT-PREMIUM-AMT.      EL522
00770                                                                   EL522
00771      IF PB-OVERRIDE-AH OR PB-OVERRIDE-BOTH                        EL522
00772          MOVE PB-I-AH-PREM-CALC  TO PB-I-AH-PREMIUM-AMT.          EL522
00773                                                                   EL522
PEMFIX     IF PB-ISSUE
PEMFIX        IF PB-CERT-NO = '00 7558417 '
PEMFIX           MOVE +300.68          TO PB-I-LF-PREMIUM-AMT
PEMFIX           DISPLAY ' FOUND AND FIXED  ' PB-CERT-NO
PEMFIX        END-IF
PEMFIX     END-IF

00774      IF PB-REIN-ONLY-CERT                                         EL522
00775        OR PB-REISSUED-CERT                                        EL522
122002       OR PB-MONTHLY-CERT
00776        OR CLASIC-CREATED-CERT                                     EL522
00777        OR PB-POLICY-IS-VOIDED                                     EL522
00778        OR PB-POLICY-IS-DECLINED                                   EL522
100703       CONTINUE
00780      ELSE                                                         EL522
100703        IF PB-I-LF-BENEFIT-CD NOT = '  ' AND '00'
00781            ADD PB-I-LF-ALT-PREMIUM-AMT
00782                                  TO TOT-LIFE-PREM
00783            ADD PB-I-LF-PREMIUM-AMT TO  TOT-LIFE-PREM
100703        END-IF
00784         ADD PB-I-AH-PREMIUM-AMT TO  TOT-AH-PREM
100703     END-IF
00785                                                                   EL522
00786      MOVE PB-SV-CARRIER          TO S-CARRIER                     EL522
00787                                     PB-CARRIER.                   EL522
00788      MOVE PB-SV-GROUPING         TO S-GROUPING                    EL522
00789                                     PB-GROUPING.                  EL522
00790      MOVE PB-SV-STATE            TO S-STATE                       EL522
00791                                     PB-STATE.                     EL522
00792      MOVE PB-ACCOUNT             TO S-ACCOUNT.                    EL522
00793      MOVE PB-CERT-EFF-DT         TO S-CERT-EFF-DT.                EL522
00794      MOVE PB-CERT-NO             TO S-CERT-NO.                    EL522
00795      MOVE PENDING-BUSINESS       TO EX-DATA-AREAS.                EL522
00796      MOVE EX-RECORD-TYPE         TO S-ED-RECORD-TYPE.             EL522
00797      MOVE '0'                    TO S-RECORD-TYPE.                EL522
00798      MOVE SPACE                  TO S-PAYMENT-CODE.               EL522
00799                                                                   EL522
00800      PERFORM 1900-RELEASE-EXTRACT-DATA THRU 1999-XIT.             EL522
00801                                                                   EL522
00802      GO TO 1100-MAIN-LOGIC.                                       EL522
00803  EJECT                                                            EL522
00804  1210-BUILD-CANCELLATION.                                         EL522
00805      ADD +1  TO  WS-GOOD-CANCELS.                                 EL522
00806                                                                   EL522
00807      IF PB-CI-LF-REIN-ONLY OR PB-CI-ENTRY-STATUS = '9'            EL522
00808        OR PB-CI-LF-POLICY-IS-DECLINED                             EL522
00809        OR PB-CI-LF-POLICY-IS-VOID                                 EL522
00810          GO TO 1220-CONTINUE.                                     EL522
00811                                                                   EL522
00812      IF PB-OVERRIDE-LIFE OR PB-OVERRIDE-BOTH                      EL522
00813          MOVE PB-C-LF-REF-CALC   TO  PB-C-LF-CANCEL-AMT.          EL522
00814                                                                   EL522
00815      ADD PB-C-LF-CANCEL-AMT  TO  TOT-LIFE-REFUND.                 EL522
00816                                                                   EL522
00817  1220-CONTINUE.                                                   EL522
00818      IF PB-CI-AH-REIN-ONLY OR PB-CI-ENTRY-STATUS = '9'            EL522
00819        OR PB-CI-LF-POLICY-IS-DECLINED                             EL522
00820        OR PB-CI-LF-POLICY-IS-VOID                                 EL522
00821          GO TO 1230-CONTINUE.                                     EL522
00822                                                                   EL522
00823      IF PB-OVERRIDE-AH OR PB-OVERRIDE-BOTH                        EL522
00824          MOVE PB-C-AH-REF-CALC   TO  PB-C-AH-CANCEL-AMT.          EL522
00825                                                                   EL522
00826      ADD PB-C-AH-CANCEL-AMT  TO  TOT-AH-REFUND.                   EL522
00827                                                                   EL522
00828  1230-CONTINUE.                                                   EL522
00829      MOVE PB-SV-CARRIER          TO S-CARRIER                     EL522
00830                                     PB-CARRIER.                   EL522
00831      MOVE PB-SV-GROUPING         TO S-GROUPING                    EL522
00832                                     PB-GROUPING.                  EL522
00833      MOVE PB-SV-STATE            TO S-STATE                       EL522
00834                                     PB-STATE.                     EL522
00835      MOVE PB-ACCOUNT             TO S-ACCOUNT.                    EL522
00836      MOVE PB-CERT-EFF-DT         TO S-CERT-EFF-DT.                EL522
00837      MOVE PB-CERT-NO             TO S-CERT-NO.                    EL522
00838      MOVE PENDING-BUSINESS       TO EX-DATA-AREAS.                EL522
00839      MOVE EX-RECORD-TYPE         TO S-ED-RECORD-TYPE.             EL522
00840      MOVE PB-RECORD-TYPE         TO S-RECORD-TYPE.                EL522
00841      MOVE SPACE                  TO S-PAYMENT-CODE.               EL522
00842                                                                   EL522
00843      PERFORM 1900-RELEASE-EXTRACT-DATA THRU 1999-XIT.             EL522
00844                                                                   EL522
00845      GO TO 1100-MAIN-LOGIC.                                       EL522
00846                                                                   EL522
00847  1240-WRITE-WRAP.                                                 EL522
00848      IF PB-ISSUE                                                  EL522
00849          ADD +1   TO   WS-BAD-ISSUES                              EL522
00850      ELSE                                                         EL522
00851          ADD +1   TO   WS-BAD-CANCELS.                            EL522
00852                                                                   EL522
00853      IF PB-ISSUE AND PB-REIN-ONLY-CERT                            EL522
00854          GO TO 1100-MAIN-LOGIC.                                   EL522
00855                                                                   EL522
00856      IF (PB-ISSUE)
122002        AND (PB-REISSUED-CERT OR PB-MONTHLY-CERT)
00857          GO TO 1100-MAIN-LOGIC.                                   EL522
00858                                                                   EL522
00859      IF PB-POLICY-IS-DECLINED OR PB-POLICY-IS-VOIDED              EL522
00860          GO TO 1100-MAIN-LOGIC.                                   EL522
00861                                                                   EL522
00862      IF PB-CANCELLATION AND (PB-CI-ENTRY-STATUS = '9')            EL522
00863          GO TO 1100-MAIN-LOGIC.                                   EL522
00864                                                                   EL522
00865      IF PB-CANCELLATION AND (PB-CI-ENTRY-STATUS = 'D'             EL522
00866          OR PB-CI-ENTRY-STATUS = 'V')                             EL522
00867          GO TO 1100-MAIN-LOGIC.                                   EL522
00868                                                                   EL522
00869      IF PB-CANCELLATION                                           EL522
00870          GO TO 1250-WRAP-CANCEL.                                  EL522
00871                                                                   EL522
00872      IF PB-OVERRIDE-LIFE                                          EL522
00873        OR PB-OVERRIDE-BOTH                                        EL522
00874          MOVE PB-I-LF-PREM-CALC  TO  PB-I-LF-PREMIUM-AMT          EL522
00875          MOVE PB-I-LF-ALT-PREM-CALC                               EL522
00876                                  TO  PB-I-LF-ALT-PREMIUM-AMT.     EL522
00877                                                                   EL522
00878      IF PB-OVERRIDE-AH                                            EL522
00879        OR PB-OVERRIDE-BOTH                                        EL522
00880          MOVE PB-I-AH-PREM-CALC  TO  PB-I-AH-PREMIUM-AMT.         EL522
00881                                                                   EL522
00882      IF PB-I-LF-PREMIUM-AMT NUMERIC                               EL522
00883          ADD PB-I-LF-PREMIUM-AMT  TO  TOT-LIFE-PREM-W.            EL522
00884                                                                   EL522
00885      IF PB-I-LF-ALT-PREMIUM-AMT NUMERIC                           EL522
100703        IF PB-I-LF-BENEFIT-CD NOT = '00' AND '  '
00886            ADD PB-I-LF-ALT-PREMIUM-AMT
00887                                  TO TOT-LIFE-PREM-W
100703        END-IF
100703     END-IF
00888                                                                   EL522
00889      IF PB-I-AH-PREMIUM-AMT NUMERIC                               EL522
00890          ADD PB-I-AH-PREMIUM-AMT  TO  TOT-AH-PREM-W.              EL522
00891                                                                   EL522
00892      GO TO 1100-MAIN-LOGIC.                                       EL522
00893                                                                   EL522
00894  1250-WRAP-CANCEL.                                                EL522
00895      IF PB-OVERRIDE-LIFE                                          EL522
00896        OR PB-OVERRIDE-BOTH                                        EL522
00897          MOVE PB-C-LF-REF-CALC   TO  PB-C-LF-CANCEL-AMT.          EL522
00898                                                                   EL522
00899      IF PB-OVERRIDE-AH                                            EL522
00900        OR PB-OVERRIDE-BOTH                                        EL522
00901          MOVE PB-C-AH-REF-CALC   TO  PB-C-AH-CANCEL-AMT.          EL522
00902                                                                   EL522
00903      IF PB-C-LF-CANCEL-AMT NUMERIC                                EL522
00904          ADD PB-C-LF-CANCEL-AMT  TO  TOT-LIFE-REFUND-W.           EL522
00905                                                                   EL522
00906      IF PB-C-AH-CANCEL-AMT NUMERIC                                EL522
00907          ADD PB-C-AH-CANCEL-AMT  TO  TOT-AH-REFUND-W.             EL522
00908                                                                   EL522
00909      GO TO 1100-MAIN-LOGIC.                                       EL522
00910                                                                   EL522
00911  1260-WRITE-RETURNED.                                             EL522
00912      IF PB-ISSUE                                                  EL522
00913          ADD +1   TO   WS-RETURNED-ISSUES                         EL522
00914      ELSE                                                         EL522
00915          ADD +1   TO   WS-RETURNED-CANCELS.                       EL522
00916                                                                   EL522
00917      IF PB-ISSUE AND PB-REIN-ONLY-CERT                            EL522
00918          GO TO 1100-MAIN-LOGIC.                                   EL522
00919                                                                   EL522
122002     IF (PB-ISSUE)
122002        AND (PB-REISSUED-CERT OR PB-MONTHLY-CERT)
00921          GO TO 1100-MAIN-LOGIC.                                   EL522
00922                                                                   EL522
00923      IF PB-POLICY-IS-DECLINED OR PB-POLICY-IS-VOIDED              EL522
00924          GO TO 1100-MAIN-LOGIC.                                   EL522
00925                                                                   EL522
00926      IF PB-CANCELLATION AND (PB-CI-ENTRY-STATUS = '9')            EL522
00927          GO TO 1100-MAIN-LOGIC.                                   EL522
00928                                                                   EL522
00929      IF PB-CANCELLATION AND (PB-CI-ENTRY-STATUS = 'D'             EL522
00930          OR PB-CI-ENTRY-STATUS = 'V')                             EL522
00931          GO TO 1100-MAIN-LOGIC.                                   EL522
00932                                                                   EL522
00933      IF PB-CANCELLATION                                           EL522
00934          GO TO 1270-RETURNED-CANCEL.                              EL522
00935                                                                   EL522
00936      IF PB-OVERRIDE-LIFE                                          EL522
00937        OR PB-OVERRIDE-BOTH                                        EL522
00938          MOVE PB-I-LF-PREM-CALC  TO  PB-I-LF-PREMIUM-AMT          EL522
00939          MOVE PB-I-LF-ALT-PREM-CALC                               EL522
00940                                  TO  PB-I-LF-ALT-PREMIUM-AMT.     EL522
00941                                                                   EL522
00942      IF PB-OVERRIDE-AH                                            EL522
00943        OR PB-OVERRIDE-BOTH                                        EL522
00944          MOVE PB-I-AH-PREM-CALC  TO  PB-I-AH-PREMIUM-AMT.         EL522
00945                                                                   EL522
00946      IF PB-I-LF-PREMIUM-AMT NUMERIC                               EL522
00947          ADD PB-I-LF-PREMIUM-AMT  TO  TOT-LIFE-PREM-R.            EL522
00948                                                                   EL522
00949      IF PB-I-LF-ALT-PREMIUM-AMT NUMERIC
100703        IF PB-I-LF-BENEFIT-CD NOT = '  ' AND '00'
00950            ADD PB-I-LF-ALT-PREMIUM-AMT
00951                                  TO TOT-LIFE-PREM-R
100703        END-IF
100703     END-IF
00952                                                                   EL522
00953      IF PB-I-AH-PREMIUM-AMT NUMERIC                               EL522
00954          ADD PB-I-AH-PREMIUM-AMT  TO  TOT-AH-PREM-R.              EL522
00955                                                                   EL522
00956      GO TO 1100-MAIN-LOGIC.                                       EL522
00957                                                                   EL522
00958  1270-RETURNED-CANCEL.                                            EL522
00959      IF PB-OVERRIDE-LIFE                                          EL522
00960        OR PB-OVERRIDE-BOTH                                        EL522
00961          MOVE PB-C-LF-REF-CALC   TO  PB-C-LF-CANCEL-AMT.          EL522
00962                                                                   EL522
00963      IF PB-OVERRIDE-AH                                            EL522
00964        OR PB-OVERRIDE-BOTH                                        EL522
00965          MOVE PB-C-AH-REF-CALC   TO  PB-C-AH-CANCEL-AMT.          EL522
00966                                                                   EL522
00967      IF PB-C-LF-CANCEL-AMT NUMERIC                                EL522
00968          ADD PB-C-LF-CANCEL-AMT  TO  TOT-LIFE-REFUND-R.           EL522
00969                                                                   EL522
00970      IF PB-C-AH-CANCEL-AMT NUMERIC                                EL522
00971          ADD PB-C-AH-CANCEL-AMT  TO  TOT-AH-REFUND-R.             EL522
00972                                                                   EL522
00973      GO TO 1100-MAIN-LOGIC.                                       EL522
00974  EJECT                                                            EL522
00975  1300-PROCESS-PEND-CLAIMS.                                        EL522
00976      IF EX-RECORD-TYPE NOT = 'B'                                  EL522
00977          GO TO 1400-PROCESS-CERT-MAINT.                           EL522
00978                                                                   EL522
00979      ADD +1   TO   WS-CLAIMS-RECORDS.                             EL522
00980                                                                   EL522
00981      MOVE EX-DATA-AREAS  TO  PENDING-CLAIMS.                      EL522
00982                                                                   EL522
00983      IF PC-PAYMENT-DT GREATER THAN WS-RUN-DATE-BIN                EL522
00984          GO TO 1100-MAIN-LOGIC.                                   EL522
00985                                                                   EL522
00986      MOVE PC-CERT-EFF-DT  TO  NEW-PB-CERT-EFF-DT.                 EL522
00987      MOVE PC-CERT-NO      TO  NEW-PB-CERT-NO.                     EL522
00988                                                                   EL522
00989      IF NEW-PB-CERT NOT = HELD-PB-CERT                            EL522
00990          MOVE SPACES  TO  HELD-PB-CERT.                           EL522
00991                                                                   EL522
00992      IF PC-FATAL-ERRORS                                           EL522
00993          GO TO 1375-WRITE-WRAP.                                   EL522
00994                                                                   EL522
00995      IF PC-UNFORCED-ERRORS                                        EL522
00996          GO TO 1375-WRITE-WRAP.                                   EL522
00997                                                                   EL522
00998      IF HELD-PB-CERT NOT = SPACES                                 EL522
00999          IF HELD-PB-CERT = NEW-PB-CERT                            EL522
01000              GO TO 1375-WRITE-WRAP.                               EL522
01001                                                                   EL522
01002      IF PC-RESERVES                                               EL522
01003          GO TO 1320-PROCESS-RESERVES.                             EL522
01004                                                                   EL522
01005      IF PC-CHARGEBLE-EXPENSE OR                                   EL522
01006         PC-NON-CHARGEBLE-EXPENSE                                  EL522
01007          GO TO 1310-PROCESS-EXPENSE.                              EL522
01008                                                                   EL522
01009      IF (PC-LF-CLAIM OR PC-OB-LF-CLAIM)                           EL522
01010          IF (PC-CCL-REIN-ONLY OR                                  EL522
01011              PC-CC-CERT-ENTRY-STATUS = '9')                       EL522
01012              GO TO 1305-CONTINUE                                  EL522
01013          ELSE                                                     EL522
01014              NEXT SENTENCE                                        EL522
01015      ELSE                                                         EL522
01016          GO TO 1305-CONTINUE.                                     EL522
01017                                                                   EL522
01018      ADD +1   TO   WS-CLAIMS-PAMTS.                               EL522
01019                                                                   EL522
01020      IF PC-LF-CLAIM                                               EL522
01021          ADD PC-CLAIM-PAYMENT    TO  TOT-DEATH.                   EL522
01022                                                                   EL522
01023      IF PC-OB-LF-CLAIM                                            EL522
01024          ADD PC-CLAIM-PAYMENT    TO  TOT-OB.                      EL522
01025                                                                   EL522
01026  1305-CONTINUE.                                                   EL522
01027      IF (PC-AH-CLAIM OR PC-OB-AH-CLAIM)                           EL522
01028          IF (PC-CCA-REIN-ONLY OR                                  EL522
01029              PC-CC-CERT-ENTRY-STATUS = '9')                       EL522
01030              GO TO 1330-CONTINUE                                  EL522
01031          ELSE                                                     EL522
01032              NEXT SENTENCE                                        EL522
01033      ELSE                                                         EL522
01034          GO TO 1330-CONTINUE.                                     EL522
01035                                                                   EL522
01036      ADD +1   TO   WS-CLAIMS-PAMTS.                               EL522
01037                                                                   EL522
01038      IF PC-AH-CLAIM                                               EL522
01039          ADD PC-CLAIM-PAYMENT    TO  TOT-DIS.                     EL522
01040                                                                   EL522
01041      IF PC-OB-AH-CLAIM                                            EL522
01042          ADD PC-CLAIM-PAYMENT    TO  TOT-OBDIS.                   EL522
01043                                                                   EL522
01044      GO TO 1330-CONTINUE.                                         EL522
01045  EJECT                                                            EL522
01046  1310-PROCESS-EXPENSE.                                            EL522
01047      IF (PC-LF-CLAIM OR PC-OB-LF-CLAIM)                           EL522
01048          IF (PC-CCL-REIN-ONLY OR                                  EL522
01049              PC-CC-CERT-ENTRY-STATUS = '9')                       EL522
01050              GO TO 1315-CONTINUE                                  EL522
01051          ELSE                                                     EL522
01052              NEXT SENTENCE                                        EL522
01053      ELSE                                                         EL522
01054          GO TO 1315-CONTINUE.                                     EL522
01055                                                                   EL522
01056      ADD +1   TO   WS-CLAIMS-EXPENSE.                             EL522
01057                                                                   EL522
01058      IF PC-LF-CLAIM                                               EL522
01059          ADD PC-CLAIM-PAYMENT    TO  TOT-DEATH-EXP.               EL522
01060                                                                   EL522
01061      IF PC-OB-LF-CLAIM                                            EL522
01062          ADD PC-CLAIM-PAYMENT    TO  TOT-OB-EXP.                  EL522
01063                                                                   EL522
01064  1315-CONTINUE.                                                   EL522
01065      IF (PC-AH-CLAIM OR PC-OB-AH-CLAIM)                           EL522
01066          IF (PC-CCA-REIN-ONLY OR                                  EL522
01067              PC-CC-CERT-ENTRY-STATUS = '9')                       EL522
01068              GO TO 1330-CONTINUE                                  EL522
01069          ELSE                                                     EL522
01070              NEXT SENTENCE                                        EL522
01071      ELSE                                                         EL522
01072          GO TO 1330-CONTINUE.                                     EL522
01073                                                                   EL522
01074      ADD +1   TO   WS-CLAIMS-EXPENSE.                             EL522
01075                                                                   EL522
01076      IF PC-AH-CLAIM                                               EL522
01077          ADD PC-CLAIM-PAYMENT    TO  TOT-DIS-EXP.                 EL522
01078                                                                   EL522
01079      IF PC-OB-AH-CLAIM                                            EL522
01080          ADD PC-CLAIM-PAYMENT    TO  TOT-OBDIS-EXP.               EL522
01081                                                                   EL522
01082      GO TO 1330-CONTINUE.                                         EL522
01083                                                                   EL522
01084  1320-PROCESS-RESERVES.                                           EL522
01085      IF (PC-LF-CLAIM OR PC-OB-LF-CLAIM)                           EL522
01086          IF (PC-CCL-REIN-ONLY OR                                  EL522
01087              PC-CC-CERT-ENTRY-STATUS = '9')                       EL522
01088              GO TO 1325-CONTINUE                                  EL522
01089          ELSE                                                     EL522
01090              NEXT SENTENCE                                        EL522
01091      ELSE                                                         EL522
01092          GO TO 1325-CONTINUE.                                     EL522
01093                                                                   EL522
01094      ADD +1   TO   WS-CLAIMS-RESERVE.                             EL522
01095                                                                   EL522
01096      ADD PC-IBNR-RESERVE-AMT     TO  TOT-LFRSV.                   EL522
01097      ADD PC-PTC-RESERVE-AMT      TO  TOT-LFRSV.                   EL522
01098      ADD PC-FUTURE-RESERVE-AMT   TO  TOT-LFRSV.                   EL522
01099      ADD PC-MANUAL-RESERVE-AMT   TO  TOT-LFRSV.                   EL522
01100                                                                   EL522
01101  1325-CONTINUE.                                                   EL522
01102      IF (PC-AH-CLAIM OR PC-OB-AH-CLAIM)                           EL522
01103          IF (PC-CCA-REIN-ONLY OR                                  EL522
01104              PC-CC-CERT-ENTRY-STATUS = '9')                       EL522
01105              GO TO 1330-CONTINUE                                  EL522
01106          ELSE                                                     EL522
01107              NEXT SENTENCE                                        EL522
01108      ELSE                                                         EL522
01109          GO TO 1330-CONTINUE.                                     EL522
01110                                                                   EL522
01111      ADD +1   TO   WS-CLAIMS-RESERVE.                             EL522
01112                                                                   EL522
01113      ADD PC-IBNR-RESERVE-AMT     TO  TOT-AHRSV.                   EL522
01114      ADD PC-PTC-RESERVE-AMT      TO  TOT-AHRSV.                   EL522
01115      ADD PC-FUTURE-RESERVE-AMT   TO  TOT-AHRSV.                   EL522
01116      ADD PC-MANUAL-RESERVE-AMT   TO  TOT-AHRSV.                   EL522
01117                                                                   EL522
01118  1330-CONTINUE.                                                   EL522
01119      MOVE PC-SV-CARRIER          TO S-CARRIER                     EL522
01120                                     PC-CARRIER.                   EL522
01121      MOVE PC-SV-GROUPING         TO S-GROUPING                    EL522
01122                                     PC-GROUPING.                  EL522
01123      MOVE PC-SV-STATE            TO S-STATE                       EL522
01124                                     PC-STATE.                     EL522
01125      MOVE PC-ACCOUNT             TO S-ACCOUNT.                    EL522
01126      MOVE PC-CERT-EFF-DT         TO S-CERT-EFF-DT.                EL522
01127      MOVE PC-CERT-NO             TO S-CERT-NO.                    EL522
01128      MOVE PENDING-CLAIMS         TO EX-DATA-AREAS.                EL522
01129      MOVE EX-RECORD-TYPE         TO S-ED-RECORD-TYPE.             EL522
01130      MOVE PC-RECORD-TYPE         TO S-RECORD-TYPE.                EL522
01131      MOVE SPACE                  TO S-PAYMENT-CODE.               EL522
01132                                                                   EL522
01133      IF PC-CLAIMS                                                 EL522
01134          MOVE PC-PAYMENT-TYPE    TO S-PAYMENT-CODE                EL522
01135          IF PC-VOIDED-PAYMENT                                     EL522
01136              MOVE 'V'            TO S-PAYMENT-CODE.               EL522
01137                                                                   EL522
01138      PERFORM 1900-RELEASE-EXTRACT-DATA THRU 1999-XIT.             EL522
01139                                                                   EL522
01140      GO TO 1100-MAIN-LOGIC.                                       EL522
01141  EJECT                                                            EL522
01142  1375-WRITE-WRAP.                                                 EL522
01143      IF PC-CHARGEBLE-EXPENSE OR                                   EL522
01144         PC-NON-CHARGEBLE-EXPENSE                                  EL522
01145          GO TO 1380-WRAP-EXPENSE.                                 EL522
01146                                                                   EL522
01147      IF PC-RESERVES                                               EL522
01148          GO TO 1390-WRAP-RESERVES.                                EL522
01149                                                                   EL522
01150      ADD +1   TO   WS-CLAIMS-PMT-BAD.                             EL522
01151                                                                   EL522
01152      IF PC-LF-CLAIM AND PC-CLAIM-PAYMENT NUMERIC                  EL522
01153          ADD PC-CLAIM-PAYMENT  TO  TOT-DEATH-W.                   EL522
01154                                                                   EL522
01155      IF PC-AH-CLAIM AND PC-CLAIM-PAYMENT NUMERIC                  EL522
01156          ADD PC-CLAIM-PAYMENT  TO  TOT-DIS-W.                     EL522
01157                                                                   EL522
01158      IF PC-OB-LF-CLAIM AND PC-CLAIM-PAYMENT NUMERIC               EL522
01159          ADD PC-CLAIM-PAYMENT  TO  TOT-OB-W.                      EL522
01160                                                                   EL522
01161      IF PC-OB-AH-CLAIM AND PC-CLAIM-PAYMENT NUMERIC               EL522
01162          ADD PC-CLAIM-PAYMENT  TO  TOT-OBDIS-W.                   EL522
01163                                                                   EL522
01164      GO TO 1100-MAIN-LOGIC.                                       EL522
01165                                                                   EL522
01166  1380-WRAP-EXPENSE.                                               EL522
01167      ADD +1   TO   WS-CLAIMS-EXP-BAD.                             EL522
01168                                                                   EL522
01169      IF PC-LF-CLAIM AND PC-CLAIM-PAYMENT NUMERIC                  EL522
01170          ADD PC-CLAIM-PAYMENT  TO  TOT-DEATH-EXP-W.               EL522
01171                                                                   EL522
01172      IF PC-AH-CLAIM AND PC-CLAIM-PAYMENT NUMERIC                  EL522
01173          ADD PC-CLAIM-PAYMENT  TO  TOT-DIS-EXP-W.                 EL522
01174                                                                   EL522
01175      IF PC-OB-LF-CLAIM AND PC-CLAIM-PAYMENT NUMERIC               EL522
01176          ADD PC-CLAIM-PAYMENT  TO  TOT-OB-EXP-W.                  EL522
01177                                                                   EL522
01178      IF PC-OB-AH-CLAIM AND PC-CLAIM-PAYMENT NUMERIC               EL522
01179          ADD PC-CLAIM-PAYMENT  TO  TOT-OBDIS-EXP-W.               EL522
01180                                                                   EL522
01181      GO TO 1100-MAIN-LOGIC.                                       EL522
01182                                                                   EL522
01183  1390-WRAP-RESERVES.                                              EL522
01184      ADD +1   TO   WS-CLAIMS-RSV-BAD.                             EL522
01185                                                                   EL522
01186      IF PC-FUTURE-RESERVE-AMT NUMERIC                             EL522
01187          IF PC-LF-CLAIM OR PC-OB-LF-CLAIM                         EL522
01188              ADD PC-FUTURE-RESERVE-AMT  TO  TOT-LFRSV-W           EL522
01189          ELSE                                                     EL522
01190              ADD PC-FUTURE-RESERVE-AMT  TO  TOT-AHRSV-W.          EL522
01191                                                                   EL522
01192      IF PC-IBNR-RESERVE-AMT NUMERIC                               EL522
01193          IF PC-LF-CLAIM OR PC-OB-LF-CLAIM                         EL522
01194              ADD PC-IBNR-RESERVE-AMT  TO  TOT-LFRSV-W             EL522
01195          ELSE                                                     EL522
01196              ADD PC-IBNR-RESERVE-AMT  TO  TOT-AHRSV-W.            EL522
01197                                                                   EL522
01198      IF PC-PTC-RESERVE-AMT NUMERIC                                EL522
01199          IF PC-LF-CLAIM OR PC-OB-LF-CLAIM                         EL522
01200              ADD PC-PTC-RESERVE-AMT  TO  TOT-LFRSV-W              EL522
01201          ELSE                                                     EL522
01202              ADD PC-PTC-RESERVE-AMT  TO  TOT-AHRSV-W.             EL522
01203                                                                   EL522
01204      IF PC-MANUAL-RESERVE-AMT NUMERIC                             EL522
01205          IF PC-LF-CLAIM OR PC-OB-LF-CLAIM                         EL522
01206              ADD PC-MANUAL-RESERVE-AMT  TO  TOT-LFRSV-W           EL522
01207          ELSE                                                     EL522
01208              ADD PC-MANUAL-RESERVE-AMT  TO  TOT-AHRSV-W.          EL522
01209                                                                   EL522
01210      GO TO 1100-MAIN-LOGIC.                                       EL522
01211  EJECT                                                            EL522
01212  1400-PROCESS-CERT-MAINT.                                         EL522
01213      IF EX-RECORD-TYPE NOT = 'C'                                  EL522
01214          GO TO 1500-PROCESS-PMTS-AND-ADJ.                         EL522
01215                                                                   EL522
01216      MOVE EX-DATA-AREAS  TO  PENDING-MAINT-TO-CERT-FILE.          EL522
01217                                                                   EL522
01218      IF CC-CREDIT-ACCEPT-DT = WS-RUN-DATE-BIN OR                  EL522
01219         CC-CREDIT-ACCEPT-DT = LOW-VALUES                          EL522
01220          NEXT SENTENCE                                            EL522
01221      ELSE                                                         EL522
01222          GO TO 1100-MAIN-LOGIC.                                   EL522
01223                                                                   EL522
01224      ADD +1  TO  WS-CHANGE-COUNT.                                 EL522
01225                                                                   EL522
01226      MOVE CC-CARRIER             TO S-CARRIER.                    EL522
01227      MOVE CC-STATE               TO S-STATE.                      EL522
01228      MOVE CC-GROUPING            TO S-GROUPING.                   EL522
01229      MOVE CC-ACCOUNT             TO S-ACCOUNT.                    EL522
01230      MOVE CC-CERT-EFF-DT         TO S-CERT-EFF-DT.                EL522
01231      MOVE CC-CERT-NO             TO S-CERT-NO.                    EL522
01232      MOVE 'A'                    TO S-ED-RECORD-TYPE.             EL522
01233      MOVE '1'                    TO S-RECORD-TYPE.                EL522
01234      MOVE SPACE                  TO S-PAYMENT-CODE.               EL522
01235                                                                   EL522
01236      PERFORM 1900-RELEASE-EXTRACT-DATA THRU 1999-XIT.             EL522
01237                                                                   EL522
01238      GO TO 1100-MAIN-LOGIC.                                       EL522
01239  EJECT                                                            EL522
01240  1500-PROCESS-PMTS-AND-ADJ.                                       EL522
01241      IF EX-RECORD-TYPE NOT = 'D'                                  EL522
01242          GO TO 1600-PROCESS-RETRO-PAYMENTS.                       EL522
01243                                                                   EL522
01244      MOVE EX-DATA-AREAS  TO  PENDING-PAY-ADJ.                     EL522
01245                                                                   EL522
01246      IF PY-CREDIT-ACCEPT-DT = WS-RUN-DATE-BIN OR                  EL522
01247         PY-CREDIT-ACCEPT-DT = LOW-VALUES                          EL522
01248          NEXT SENTENCE                                            EL522
01249      ELSE                                                         EL522
01250          GO TO 1100-MAIN-LOGIC.                                   EL522
01251                                                                   EL522
01252      ADD +1  TO  WS-PAYMTS-COUNT.                                 EL522
01253                                                                   EL522
01254      IF PY-BILLED-DATE = LOW-VALUES OR SPACES                     EL522
01255          MOVE ' '           TO  PY-BILL-FLAG                      EL522
01256      ELSE                                                         EL522
01257          MOVE 'B'           TO  PY-BILL-FLAG.                     EL522
01258                                                                   EL522
01259      MOVE SPACES            TO  PYMT-REC.                         EL522
01260      MOVE 'CLA'             TO  PYMT-CODE.                        EL522
01261      MOVE PY-CARRIER        TO  PYMT-CARR.                        EL522
01262      MOVE PY-GROUPING       TO  PYMT-GROUP.                       EL522
01263      MOVE PY-FIN-RESP       TO  PYMT-RESP.                        EL522
01264      MOVE PY-ACCOUNT        TO  PYMT-ACCT.                        EL522
01265      MOVE PY-ENTRY-COMMENT  TO  PYMT-DESC.                        EL522
01266      MOVE PY-RECORD-TYPE    TO  PYMT-TYPE.                        EL522
01267      MOVE PY-ENTRY-AMT      TO  PYMT-AMT.                         EL522
01268      MOVE PY-BILL-FLAG      TO  PYMT-BILL-FLAG.                   EL522
01269      MOVE PY-AR-FLAG        TO  PYMT-AR-FLAG.                     EL522
01270                                                                   EL522
01271      WRITE PMT-AND-ADJ FROM PYMT-REC.                             EL522
01272                                                                   EL522
01273      ADD +1  TO  WS-PYMTS-OUTPUT-COUNT.                           EL522
01274                                                                   EL522
01275      IF PY-REMIT-RECEIVED                                         EL522
01276          IF PY-BILLED OR PY-AR-CYCLE                              EL522
01277              ADD PYMT-AMT TO TOT-B-REMIT-RECEIVED                 EL522
01278          ELSE                                                     EL522
01279              ADD PYMT-AMT TO TOT-C-REMIT-RECEIVED.                EL522
01280                                                                   EL522
01281      IF PY-ADJ-REM-RECEIVED                                       EL522
01282          IF PY-BILLED OR PY-AR-CYCLE                              EL522
01283              ADD PYMT-AMT TO TOT-B-ADJ-REM-RECEIVED               EL522
01284          ELSE                                                     EL522
01285              ADD PYMT-AMT TO TOT-C-ADJ-REM-RECEIVED.              EL522
01286                                                                   EL522
01287      IF PY-DEPOSIT                                                EL522
01288          IF PY-BILLED OR PY-AR-CYCLE                              EL522
01289              ADD PYMT-AMT TO TOT-B-DEPOSIT                        EL522
01290          ELSE                                                     EL522
01291              ADD PYMT-AMT TO TOT-C-DEPOSIT.                       EL522
01292                                                                   EL522
01293      IF PY-ADJ-DEPOSIT                                            EL522
01294          IF PY-BILLED OR PY-AR-CYCLE                              EL522
01295              ADD PYMT-AMT TO TOT-B-ADJ-DEPOSIT                    EL522
01296          ELSE                                                     EL522
01297              ADD PYMT-AMT TO TOT-C-ADJ-DEPOSIT.                   EL522
01298                                                                   EL522
01299      IF PY-CHARGE-TO-AGENT                                        EL522
01300          IF PY-BILLED OR PY-AR-CYCLE                              EL522
01301              ADD PYMT-AMT TO TOT-B-CHARGE-TO-AGENT                EL522
01302          ELSE                                                     EL522
01303              ADD PYMT-AMT TO TOT-C-CHARGE-TO-AGENT.               EL522
01304                                                                   EL522
01305      IF PY-ADJ-CHG-TO-AGT                                         EL522
01306          IF PY-BILLED OR PY-AR-CYCLE                              EL522
01307              ADD PYMT-AMT TO TOT-B-ADJ-CHG-TO-AGENT               EL522
01308          ELSE                                                     EL522
01309              ADD PYMT-AMT TO TOT-C-ADJ-CHG-TO-AGENT.              EL522
01310                                                                   EL522
01311      IF PY-ADD-TO-BALANCE                                         EL522
01312          IF PY-BILLED OR PY-AR-CYCLE                              EL522
01313              ADD PYMT-AMT TO TOT-B-ADD-TO-BALANCE                 EL522
01314          ELSE                                                     EL522
01315              ADD PYMT-AMT TO TOT-C-ADD-TO-BALANCE.                EL522
01316                                                                   EL522
01317      IF PY-ADD-TO-YTD-COMP                                        EL522
01318          IF PY-BILLED OR PY-AR-CYCLE                              EL522
01319              ADD PYMT-AMT TO TOT-B-ADD-TO-YTD-COMP                EL522
01320          ELSE                                                     EL522
01321              ADD PYMT-AMT TO TOT-C-ADD-TO-YTD-COMP.               EL522
01322                                                                   EL522
01323      IF PY-SUBTRACT-YTD-COMP                                      EL522
01324          IF PY-BILLED OR PY-AR-CYCLE                              EL522
01325              ADD PYMT-AMT TO TOT-B-SUB-YTD-COMP                   EL522
01326          ELSE                                                     EL522
01327              ADD PYMT-AMT TO TOT-C-SUB-YTD-COMP.                  EL522
01328                                                                   EL522
01329      IF PY-FICA-ENTRY                                             EL522
01330          IF PY-BILLED OR PY-AR-CYCLE                              EL522
01331              ADD PYMT-AMT TO TOT-B-FICA-ENTRY                     EL522
01332          ELSE                                                     EL522
01333              ADD PYMT-AMT TO TOT-C-FICA-ENTRY.                    EL522
01334                                                                   EL522
01335      GO TO 1100-MAIN-LOGIC.                                       EL522
01336  EJECT                                                            EL522
01337  1600-PROCESS-RETRO-PAYMENTS.                                     EL522
01338      MOVE EX-DATA-AREAS  TO  PENDING-RETRO-REIN-ADJUSTMENTS.      EL522
01339                                                                   EL522
01340      IF RP-CREDIT-ACCEPT-DT = WS-RUN-DATE-BIN  OR                 EL522
01341         RP-CREDIT-ACCEPT-DT = LOW-VALUES                          EL522
01342          NEXT SENTENCE                                            EL522
01343      ELSE                                                         EL522
01344          GO TO 1100-MAIN-LOGIC.                                   EL522
01345                                                                   EL522
01346      ADD +1  TO  WS-RETROS-COUNT.                                 EL522
01347                                                                   EL522
01348      WRITE RETRO-PMTS FROM PENDING-RETRO-REIN-ADJUSTMENTS.        EL522
01349                                                                   EL522
01350      IF RP-BENEFIT-TYPE = LIFE-OVERRIDE-L1                        EL522
01351          ADD RP-LIFE-MORTALITY-AMT  TO  TOT-LIFE-MORTALITY-AMT    EL522
01352          ADD RP-INS-AMT-INFORCE     TO  TOT-LF-AMT-INFORCE.       EL522
01353                                                                   EL522
01354      IF RP-BENEFIT-TYPE = AH-OVERRIDE-L1                          EL522
01355          ADD RP-INS-AMT-INFORCE     TO  TOT-AH-AMT-INFORCE.       EL522
01356                                                                   EL522
01357      ADD RP-FUTURE-RESERVE       TO  TOT-FUTURE-RESERVE.          EL522
01358      ADD RP-PTC-RESERVE          TO  TOT-PTC-RESERVE.             EL522
01359      ADD RP-IBNR-RESERVE         TO  TOT-IBNR-RESERVE.            EL522
01360      ADD RP-CLAIM-ADJ-AMT        TO  TOT-CLAIM-ADJ-AMT.           EL522
01361                                                                   EL522
01362      ADD RP-EXPENSES             TO  TOT-EXPENSES.                EL522
01363      ADD RP-PAYMENTS             TO  TOT-PAYMENTS.                EL522
01364      ADD RP-OTHER-COMM           TO  TOT-OTHER-COMM.              EL522
01365      ADD RP-REIN-PREM-ADJ        TO  TOT-REIN-PREM-ADJ.           EL522
01366                                                                   EL522
01367      ADD +1  TO  WS-RETRO-OUTPUT-COUNT.                           EL522
01368                                                                   EL522
01369      GO TO 1100-MAIN-LOGIC.                                       EL522
01370  EJECT                                                            EL522
01371  1700-END-PROCESSING.                                             EL522
01372      MOVE WS-H4-TITLE-1    TO  WS-H4-TITLE.                       EL522
01373      MOVE WS-HEADING5-A    TO  WS-HEADING5.                       EL522
01374                                                                   EL522
01375      PERFORM WRITE-HEADINGS.                                      EL522
01376                                                                   EL522
01377      MOVE TOT-LIFE-PREM    TO  WS-D-AMT1.                         EL522
01378      MOVE TOT-AH-PREM      TO  WS-D-AMT2.                         EL522
01379      MOVE TOT-LIFE-REFUND  TO  WS-D-AMT3.                         EL522
01380      MOVE TOT-AH-REFUND    TO  WS-D-AMT4.                         EL522
01381      MOVE TOT-DEATH        TO  WS-D-AMT5.                         EL522
01382      MOVE TOT-DIS          TO  WS-D-AMT6.                         EL522
01383      MOVE TOT-OB           TO  WS-D-AMT7.                         EL522
01384      MOVE TOT-OBDIS        TO  WS-D-AMT8.                         EL522
01385                                                                   EL522
01386      IF ME-DO-UPDATE                                              EL522
01387          MOVE TOT-LIFE-PREM      TO ME-522-PREM-L                 EL522
01388          MOVE TOT-AH-PREM        TO ME-522-PREM-AH                EL522
01389          MOVE TOT-LIFE-REFUND    TO ME-522-REF-L                  EL522
01390          MOVE TOT-AH-REFUND      TO ME-522-REF-AH                 EL522
01391          ADD TOT-DEATH  TOT-OB  TOT-DEATH-EXP  TOT-OB-EXP            CL**3
01392                  GIVING ME-522-PROC-CLM-L                         EL522
01393          ADD TOT-DIS  TOT-OBDIS  TOT-DIS-EXP  TOT-OBDIS-EXP          CL**3
01394                  GIVING ME-522-PROC-CLM-AH.                       EL522
01395                                                                   EL522
01396      MOVE '0'              TO  P-CTL.                             EL522
01397      MOVE WS-DETAIL1       TO  P-DATA.                            EL522
01398                                                                   EL522
01399      PERFORM WRITE-A-LINE.                                        EL522
01400                                                                   EL522
01401      MOVE WS-HEADING5-B    TO  PRT.                               EL522
01402                                                                   EL522
01403      PERFORM WRITE-PRINTER.                                       EL522
01404                                                                   EL522
01405      MOVE TOT-LFRSV        TO  WS-D2-AMT3.                        EL522
01406      MOVE TOT-AHRSV        TO  WS-D2-AMT4.                        EL522
01407      MOVE TOT-DEATH-EXP    TO  WS-D2-AMT5.                        EL522
01408      MOVE TOT-DIS-EXP      TO  WS-D2-AMT6.                        EL522
01409      MOVE TOT-OB-EXP       TO  WS-D2-AMT7.                        EL522
01410      MOVE TOT-OBDIS-EXP    TO  WS-D2-AMT8.                        EL522
01411                                                                   EL522
01412      IF ME-DO-UPDATE                                              EL522
01413          MOVE TOT-LFRSV          TO ME-522-PROC-RSV-L             EL522
01414          MOVE TOT-AHRSV          TO ME-522-PROC-RSV-AH.              CL**3
01415                                                                   EL522
01416      MOVE '0'              TO  P-CTL.                             EL522
01417      MOVE WS-DETAIL2       TO  P-DATA.                            EL522
01418                                                                   EL522
01419      PERFORM WRITE-A-LINE.                                        EL522
01420                                                                   EL522
01421      MOVE '-'                TO  WS-H4-CTL.                       EL522
01422      MOVE WS-H4-TITLE-2      TO  WS-H4-TITLE.                     EL522
01423      MOVE WS-HEADING5-A      TO  WS-HEADING5.                     EL522
01424                                                                   EL522
01425      PERFORM WRITE-HEADINGS.                                      EL522
01426                                                                   EL522
01427      MOVE TOT-LIFE-PREM-W    TO  WS-D-AMT1.                       EL522
01428      MOVE TOT-AH-PREM-W      TO  WS-D-AMT2.                       EL522
01429      MOVE TOT-LIFE-REFUND-W  TO  WS-D-AMT3.                       EL522
01430      MOVE TOT-AH-REFUND-W    TO  WS-D-AMT4.                       EL522
01431      MOVE TOT-DEATH-W        TO  WS-D-AMT5.                       EL522
01432      MOVE TOT-DIS-W          TO  WS-D-AMT6.                       EL522
01433      MOVE TOT-OB-W           TO  WS-D-AMT7.                       EL522
01434      MOVE TOT-OBDIS-W        TO  WS-D-AMT8.                       EL522
01435                                                                   EL522
01436      MOVE '0'                TO  P-CTL.                           EL522
01437      MOVE WS-DETAIL1         TO  P-DATA.                          EL522
01438                                                                   EL522
01439      PERFORM WRITE-A-LINE.                                        EL522
01440                                                                   EL522
01441      MOVE WS-HEADING5-B    TO  PRT.                               EL522
01442                                                                   EL522
01443      PERFORM WRITE-PRINTER.                                       EL522
01444                                                                   EL522
01445      MOVE TOT-LFRSV-W      TO  WS-D2-AMT3.                        EL522
01446      MOVE TOT-AHRSV-W      TO  WS-D2-AMT4.                        EL522
01447      MOVE TOT-DEATH-EXP-W  TO  WS-D2-AMT5.                        EL522
01448      MOVE TOT-DIS-EXP-W    TO  WS-D2-AMT6.                        EL522
01449      MOVE TOT-OB-EXP-W     TO  WS-D2-AMT7.                        EL522
01450      MOVE TOT-OBDIS-EXP-W  TO  WS-D2-AMT8.                        EL522
01451                                                                   EL522
01452      IF ME-DO-UPDATE                                              EL522
01453          ADD TOT-LFRSV  TOT-LFRSV-W                               EL522
01454                 GIVING ME-522-ALL-RSV-L                           EL522
01455          ADD TOT-AHRSV  TOT-AHRSV-W                               EL522
01456                 GIVING ME-522-ALL-RSV-AH                          EL522
01457          ADD TOT-DEATH-W    TOT-DEATH                             EL522
01458              TOT-DEATH-EXP  TOT-DEATH-EXP-W                       EL522
01459              TOT-OB         TOT-OB-W                                 CL**3
01460              TOT-OB-EXP     TOT-OB-EXP-W                             CL**3
01461                 GIVING ME-522-ALL-CLM-L                              CL**3
01462          ADD TOT-DIS-W      TOT-DIS                                  CL**3
01463              TOT-DIS-EXP    TOT-DIS-EXP-W                            CL**3
01464              TOT-OBDIS      TOT-OBDIS-W                              CL**3
01465              TOT-OBDIS-EXP  TOT-OBDIS-EXP-W                          CL**3
01466                 GIVING ME-522-ALL-CLM-AH.                            CL**3
01467                                                                   EL522
01468      MOVE '0'              TO  P-CTL.                             EL522
01469      MOVE WS-DETAIL2       TO  P-DATA.                            EL522
01470                                                                   EL522
01471      PERFORM WRITE-A-LINE.                                        EL522
01472                                                                   EL522
01473      MOVE '-'                TO  WS-H4-CTL.                       EL522
01474      MOVE WS-H4-TITLE-5      TO  WS-H4-TITLE.                     EL522
01475      MOVE WS-HEADING5-R      TO  WS-HEADING5.                     EL522
01476                                                                   EL522
01477      PERFORM WRITE-HEADINGS.                                      EL522
01478                                                                   EL522
01479      MOVE SPACES             TO  WS-DETAIL1.                      EL522
01480      MOVE TOT-LIFE-PREM-R    TO  WS-D-AMT1.                       EL522
01481      MOVE TOT-AH-PREM-R      TO  WS-D-AMT2.                       EL522
01482      MOVE TOT-LIFE-REFUND-R  TO  WS-D-AMT3.                       EL522
01483      MOVE TOT-AH-REFUND-R    TO  WS-D-AMT4.                       EL522
01484                                                                   EL522
01485      MOVE '0'                TO  P-CTL.                           EL522
01486      MOVE WS-DETAIL1         TO  P-DATA.                          EL522
01487                                                                   EL522
01488      PERFORM WRITE-A-LINE.                                        EL522
01489                                                                   EL522
01490      MOVE '-'                TO  WS-H4-CTL.                       EL522
01491      MOVE WS-H4-TITLE-3      TO  WS-H4-TITLE.                     EL522
01492      MOVE WS-HEADING5-C      TO  WS-HEADING5.                     EL522
01493                                                                   EL522
01494      PERFORM WRITE-HEADINGS.                                      EL522
01495                                                                   EL522
01496      MOVE WS-HEADING5-C1     TO  PRT.                             EL522
01497      PERFORM WRITE-PRINTER.                                       EL522
01498                                                                   EL522
01499      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01500      MOVE '*** REMITTANCE AND DEPOSITS  ***'                      EL522
01501                                  TO  WS-D3-DESC.                  EL522
01502      MOVE WS-DETAIL3             TO  PRT.                         EL522
01503      PERFORM WRITE-PRINTER.                                       EL522
01504                                                                   EL522
01505      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01506      MOVE 'R - REMITTANCE RECEIVED         '                      EL522
01507                                  TO  WS-D3-DESC.                  EL522
01508      MOVE TOT-C-REMIT-RECEIVED   TO  WS-D3-AMT1.                  EL522
01509      MOVE TOT-B-REMIT-RECEIVED   TO  WS-D3-AMT3.                  EL522
01510      COMPUTE TOT-T-REMIT-RECEIVED =                               EL522
01511              TOT-C-REMIT-RECEIVED + TOT-B-REMIT-RECEIVED.         EL522
01512      MOVE TOT-T-REMIT-RECEIVED   TO  WS-D3-AMT5.                  EL522
01513                                                                   EL522
01514      MOVE WS-DETAIL3             TO  PRT.                         EL522
01515      PERFORM WRITE-PRINTER.                                       EL522
01516                                                                   EL522
01517      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01518      MOVE 'S - ADJUST REMITTANCE RECEIVED  '                      EL522
01519                                  TO  WS-D3-DESC.                  EL522
01520      MOVE TOT-C-ADJ-REM-RECEIVED TO  WS-D3-AMT1.                  EL522
01521      MOVE TOT-B-ADJ-REM-RECEIVED TO  WS-D3-AMT3.                  EL522
01522      COMPUTE TOT-T-ADJ-REM-RECEIVED =                             EL522
01523              TOT-C-ADJ-REM-RECEIVED + TOT-B-ADJ-REM-RECEIVED.     EL522
01524      MOVE TOT-T-ADJ-REM-RECEIVED TO  WS-D3-AMT5.                  EL522
01525                                                                   EL522
01526      MOVE WS-DETAIL3             TO  PRT.                         EL522
01527      PERFORM WRITE-PRINTER.                                       EL522
01528                                                                   EL522
01529      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01530      MOVE '   TOTAL REMITTANCE             '                      EL522
01531                                  TO  WS-D3-DESC.                  EL522
01532      COMPUTE WS-NET-TOTAL =                                       EL522
01533              TOT-C-REMIT-RECEIVED + TOT-C-ADJ-REM-RECEIVED.       EL522
01534      MOVE WS-NET-TOTAL           TO  WS-D3-AMT2.                  EL522
01535      COMPUTE WS-NET-TOTAL =                                       EL522
01536              TOT-B-REMIT-RECEIVED + TOT-B-ADJ-REM-RECEIVED.       EL522
01537      MOVE WS-NET-TOTAL           TO  WS-D3-AMT4.                  EL522
01538      COMPUTE WS-NET-TOTAL =                                       EL522
01539              TOT-T-REMIT-RECEIVED + TOT-T-ADJ-REM-RECEIVED.       EL522
01540      MOVE WS-NET-TOTAL           TO  WS-D3-AMT6.                  EL522
01541                                                                   EL522
01542      MOVE WS-DETAIL3             TO  PRT.                         EL522
01543      PERFORM WRITE-PRINTER.                                       EL522
01544                                                                   EL522
01545      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01546      MOVE WS-DETAIL3             TO  PRT.                         EL522
01547      PERFORM WRITE-PRINTER.                                       EL522
01548                                                                   EL522
01549      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01550      MOVE 'D - DEPOSITS                    '                      EL522
01551                                  TO  WS-D3-DESC.                  EL522
01552      MOVE TOT-C-DEPOSIT          TO  WS-D3-AMT1.                  EL522
01553      MOVE TOT-B-DEPOSIT          TO  WS-D3-AMT3.                  EL522
01554      COMPUTE TOT-T-DEPOSIT =                                      EL522
01555              TOT-C-DEPOSIT + TOT-B-DEPOSIT.                       EL522
01556      MOVE TOT-T-DEPOSIT          TO  WS-D3-AMT5.                  EL522
01557                                                                   EL522
01558      MOVE WS-DETAIL3             TO  PRT.                         EL522
01559      PERFORM WRITE-PRINTER.                                       EL522
01560                                                                   EL522
01561      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01562      MOVE 'T - ADJUST DEPOSITS             '                      EL522
01563                                  TO  WS-D3-DESC.                  EL522
01564      MOVE TOT-C-ADJ-DEPOSIT      TO  WS-D3-AMT1.                  EL522
01565      MOVE TOT-B-ADJ-DEPOSIT      TO  WS-D3-AMT3.                  EL522
01566      COMPUTE TOT-T-ADJ-DEPOSIT =                                  EL522
01567              TOT-C-ADJ-DEPOSIT + TOT-B-ADJ-DEPOSIT.               EL522
01568      MOVE TOT-T-ADJ-DEPOSIT      TO  WS-D3-AMT5.                  EL522
01569                                                                   EL522
01570      MOVE WS-DETAIL3             TO  PRT.                         EL522
01571      PERFORM WRITE-PRINTER.                                       EL522
01572                                                                   EL522
01573      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01574      MOVE '   TOTAL DEPOSITS               '                      EL522
01575                                  TO  WS-D3-DESC.                  EL522
01576      COMPUTE WS-NET-TOTAL =                                       EL522
01577              TOT-C-DEPOSIT + TOT-C-ADJ-DEPOSIT.                   EL522
01578      MOVE WS-NET-TOTAL           TO  WS-D3-AMT2.                  EL522
01579      COMPUTE WS-NET-TOTAL =                                       EL522
01580              TOT-B-DEPOSIT + TOT-B-ADJ-DEPOSIT.                   EL522
01581      MOVE WS-NET-TOTAL           TO  WS-D3-AMT4.                  EL522
01582      COMPUTE WS-NET-TOTAL =                                       EL522
01583              TOT-T-DEPOSIT + TOT-T-ADJ-DEPOSIT.                   EL522
01584      MOVE WS-NET-TOTAL           TO  WS-D3-AMT6.                  EL522
01585                                                                   EL522
01586      MOVE WS-DETAIL3             TO  PRT.                         EL522
01587      PERFORM WRITE-PRINTER.                                       EL522
01588                                                                   EL522
01589      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01590      MOVE '0'                    TO  WS-DETAIL3.                  EL522
01591      MOVE '    TOTAL REMITTANCE AND DEPOSIT'                      EL522
01592                                  TO  WS-D3-DESC.                  EL522
01593      COMPUTE WS-NET-TOTAL =                                       EL522
01594              TOT-C-REMIT-RECEIVED + TOT-C-ADJ-REM-RECEIVED        EL522
01595              + TOT-C-DEPOSIT + TOT-C-ADJ-DEPOSIT.                 EL522
01596      MOVE WS-NET-TOTAL           TO  WS-D3-AMT2.                  EL522
01597      COMPUTE WS-NET-TOTAL =                                       EL522
01598              TOT-B-REMIT-RECEIVED + TOT-B-ADJ-REM-RECEIVED        EL522
01599              + TOT-B-DEPOSIT + TOT-B-ADJ-DEPOSIT.                 EL522
01600      MOVE WS-NET-TOTAL           TO  WS-D3-AMT4.                  EL522
01601      COMPUTE WS-NET-TOTAL =                                       EL522
01602              TOT-T-REMIT-RECEIVED + TOT-T-ADJ-REM-RECEIVED        EL522
01603              + TOT-T-DEPOSIT + TOT-T-ADJ-DEPOSIT.                 EL522
01604      MOVE WS-NET-TOTAL           TO  WS-D3-AMT6.                  EL522
01605                                                                   EL522
01606      MOVE WS-DETAIL3             TO  PRT.                         EL522
01607      PERFORM WRITE-PRINTER.                                       EL522
01608                                                                   EL522
01609      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01610      MOVE WS-DETAIL3             TO  PRT.                         EL522
01611      PERFORM WRITE-PRINTER.                                       EL522
01612                                                                   EL522
01613      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01614      MOVE '** CHECKS - CHARGES TO AGENTS **'                      EL522
01615                                  TO  WS-D3-DESC.                  EL522
01616      MOVE WS-DETAIL3             TO  PRT.                         EL522
01617      PERFORM WRITE-PRINTER.                                       EL522
01618                                                                   EL522
01619      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01620      MOVE 'C - CHARGE TO AGENT             '                      EL522
01621                                  TO  WS-D3-DESC.                  EL522
01622      MOVE TOT-C-CHARGE-TO-AGENT  TO  WS-D3-AMT1.                  EL522
01623      MOVE TOT-B-CHARGE-TO-AGENT  TO  WS-D3-AMT3.                  EL522
01624      COMPUTE TOT-T-CHARGE-TO-AGENT =                              EL522
01625              TOT-C-CHARGE-TO-AGENT + TOT-B-CHARGE-TO-AGENT.       EL522
01626      MOVE TOT-T-CHARGE-TO-AGENT  TO  WS-D3-AMT5.                  EL522
01627                                                                   EL522
01628      MOVE WS-DETAIL3             TO  PRT.                         EL522
01629      PERFORM WRITE-PRINTER.                                       EL522
01630                                                                   EL522
01631      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01632      MOVE 'U - ADJUST CHARGE TO AGENT      '                      EL522
01633                                  TO  WS-D3-DESC.                  EL522
01634      MOVE TOT-C-ADJ-CHG-TO-AGENT TO  WS-D3-AMT1.                  EL522
01635      MOVE TOT-B-ADJ-CHG-TO-AGENT TO  WS-D3-AMT3.                  EL522
01636      COMPUTE TOT-T-ADJ-CHG-TO-AGENT =                             EL522
01637              TOT-C-ADJ-CHG-TO-AGENT + TOT-B-ADJ-CHG-TO-AGENT.     EL522
01638      MOVE TOT-T-ADJ-CHG-TO-AGENT TO  WS-D3-AMT5.                  EL522
01639                                                                   EL522
01640      MOVE WS-DETAIL3             TO  PRT.                         EL522
01641      PERFORM WRITE-PRINTER.                                       EL522
01642                                                                   EL522
01643      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01644      MOVE '   TOTAL CHARGE TO AGENT        '                      EL522
01645                                  TO  WS-D3-DESC.                  EL522
01646      COMPUTE WS-NET-TOTAL =                                       EL522
01647              TOT-C-CHARGE-TO-AGENT + TOT-C-ADJ-CHG-TO-AGENT.      EL522
01648      MOVE WS-NET-TOTAL           TO  WS-D3-AMT2.                  EL522
01649      COMPUTE WS-NET-TOTAL =                                       EL522
01650              TOT-B-CHARGE-TO-AGENT + TOT-B-ADJ-CHG-TO-AGENT.      EL522
01651      MOVE WS-NET-TOTAL           TO  WS-D3-AMT4.                  EL522
01652      COMPUTE WS-NET-TOTAL =                                       EL522
01653              TOT-T-CHARGE-TO-AGENT + TOT-T-ADJ-CHG-TO-AGENT.      EL522
01654      MOVE WS-NET-TOTAL           TO  WS-D3-AMT6.                  EL522
01655                                                                   EL522
01656      MOVE WS-DETAIL3             TO  PRT.                         EL522
01657      PERFORM WRITE-PRINTER.                                       EL522
01658                                                                   EL522
01659      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01660      MOVE WS-DETAIL3             TO  PRT.                         EL522
01661      PERFORM WRITE-PRINTER.                                       EL522
01662                                                                   EL522
01663      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01664      MOVE '0'                    TO  WS-DETAIL3.                  EL522
01665      MOVE '    TOTAL CASH TRANSACTIONS     '                      EL522
01666                                  TO  WS-D3-DESC.                  EL522
01667      COMPUTE WS-NET-TOTAL =                                       EL522
01668              TOT-C-REMIT-RECEIVED + TOT-C-ADJ-REM-RECEIVED        EL522
01669              + TOT-C-DEPOSIT + TOT-C-ADJ-DEPOSIT                  EL522
01670              - TOT-C-CHARGE-TO-AGENT - TOT-C-ADJ-CHG-TO-AGENT.    EL522
01671      MOVE WS-NET-TOTAL           TO  WS-D3-AMT2.                  EL522
01672      COMPUTE WS-NET-TOTAL =                                       EL522
01673              TOT-B-REMIT-RECEIVED + TOT-B-ADJ-REM-RECEIVED        EL522
01674              + TOT-B-DEPOSIT + TOT-B-ADJ-DEPOSIT                  EL522
01675              - TOT-B-CHARGE-TO-AGENT - TOT-B-ADJ-CHG-TO-AGENT.    EL522
01676      MOVE WS-NET-TOTAL           TO  WS-D3-AMT4.                  EL522
01677      COMPUTE WS-NET-TOTAL =                                       EL522
01678              TOT-T-REMIT-RECEIVED + TOT-T-ADJ-REM-RECEIVED        EL522
01679              + TOT-T-DEPOSIT + TOT-T-ADJ-DEPOSIT                  EL522
01680              - TOT-T-CHARGE-TO-AGENT - TOT-T-ADJ-CHG-TO-AGENT.    EL522
01681      MOVE WS-NET-TOTAL           TO  WS-D3-AMT6.                  EL522
01682                                                                   EL522
01683      MOVE WS-DETAIL3             TO  PRT.                         EL522
01684      PERFORM WRITE-PRINTER.                                       EL522
01685                                                                   EL522
01686      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01687      MOVE '** CHANGES TO AGENT BALANCES ***'                      EL522
01688                                  TO  WS-D3-DESC.                  EL522
01689      MOVE WS-DETAIL3             TO  PRT.                         EL522
01690      PERFORM WRITE-PRINTER.                                       EL522
01691                                                                   EL522
01692      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01693      MOVE 'Z - ADD TO BALANCE              '                      EL522
01694                                  TO  WS-D3-DESC.                  EL522
01695      MOVE TOT-C-ADD-TO-BALANCE   TO  WS-D3-AMT1.                  EL522
01696      MOVE TOT-B-ADD-TO-BALANCE   TO  WS-D3-AMT3.                  EL522
01697      COMPUTE TOT-T-ADD-TO-BALANCE =                               EL522
01698              TOT-C-ADD-TO-BALANCE + TOT-B-ADD-TO-BALANCE.         EL522
01699      MOVE TOT-T-ADD-TO-BALANCE   TO  WS-D3-AMT5.                  EL522
01700                                                                   EL522
01701      MOVE WS-DETAIL3             TO  PRT.                         EL522
01702      PERFORM WRITE-PRINTER.                                       EL522
01703                                                                   EL522
01704      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01705      MOVE WS-DETAIL3             TO  PRT.                         EL522
01706      PERFORM WRITE-PRINTER.                                       EL522
01707                                                                   EL522
01708      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01709      MOVE '0'                    TO  WS-DETAIL3.                  EL522
01710      MOVE '  NET ADJUSTMENT TO RECEIVABLE  '                      EL522
01711                                  TO  WS-D3-DESC.                  EL522
01712      COMPUTE WS-NET-TOTAL =                                       EL522
01713              TOT-C-REMIT-RECEIVED + TOT-C-ADJ-REM-RECEIVED        EL522
01714              + TOT-C-DEPOSIT + TOT-C-ADJ-DEPOSIT                  EL522
01715              - TOT-C-CHARGE-TO-AGENT - TOT-C-ADJ-CHG-TO-AGENT     EL522
01716              + TOT-C-ADD-TO-BALANCE.                              EL522
01717      MOVE WS-NET-TOTAL           TO  WS-D3-AMT2.                  EL522
01718      COMPUTE WS-NET-TOTAL =                                       EL522
01719              TOT-B-REMIT-RECEIVED + TOT-B-ADJ-REM-RECEIVED        EL522
01720              + TOT-B-DEPOSIT + TOT-B-ADJ-DEPOSIT                  EL522
01721              - TOT-B-CHARGE-TO-AGENT - TOT-B-ADJ-CHG-TO-AGENT     EL522
01722              + TOT-B-ADD-TO-BALANCE.                              EL522
01723      MOVE WS-NET-TOTAL           TO  WS-D3-AMT4.                  EL522
01724      COMPUTE WS-NET-TOTAL =                                       EL522
01725              TOT-T-REMIT-RECEIVED + TOT-T-ADJ-REM-RECEIVED        EL522
01726              + TOT-T-DEPOSIT + TOT-T-ADJ-DEPOSIT                  EL522
01727              - TOT-T-CHARGE-TO-AGENT - TOT-T-ADJ-CHG-TO-AGENT     EL522
01728              + TOT-T-ADD-TO-BALANCE.                              EL522
01729      MOVE WS-NET-TOTAL           TO  WS-D3-AMT6.                  EL522
01730                                                                   EL522
01731      MOVE WS-DETAIL3             TO  PRT.                         EL522
01732      PERFORM WRITE-PRINTER.                                       EL522
01733                                                                   EL522
01734      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01735      MOVE '0'                    TO  WS-DETAIL3.                  EL522
01736      MOVE '* ADJUSTMENTS TO COMPENSATIONS *'                      EL522
01737                                  TO  WS-D3-DESC.                  EL522
01738      MOVE WS-DETAIL3             TO  PRT.                         EL522
01739      PERFORM WRITE-PRINTER.                                       EL522
01740                                                                   EL522
01741      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01742      MOVE 'X - ADD TO YTD COMP             '                      EL522
01743                                  TO  WS-D3-DESC.                  EL522
01744      MOVE TOT-C-ADD-TO-YTD-COMP  TO  WS-D3-AMT1.                  EL522
01745      MOVE TOT-B-ADD-TO-YTD-COMP  TO  WS-D3-AMT3.                  EL522
01746      COMPUTE TOT-T-ADD-TO-YTD-COMP =                              EL522
01747              TOT-C-ADD-TO-YTD-COMP + TOT-B-ADD-TO-YTD-COMP.       EL522
01748      MOVE TOT-T-ADD-TO-YTD-COMP  TO  WS-D3-AMT5.                  EL522
01749                                                                   EL522
01750      MOVE WS-DETAIL3             TO  PRT.                         EL522
01751      PERFORM WRITE-PRINTER.                                       EL522
01752                                                                   EL522
01753      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01754      MOVE 'Y - SUBTRACT FROM YTD COMP      '                      EL522
01755                                  TO  WS-D3-DESC.                  EL522
01756      MOVE TOT-C-SUB-YTD-COMP     TO  WS-D3-AMT1.                  EL522
01757      MOVE TOT-B-SUB-YTD-COMP     TO  WS-D3-AMT3.                  EL522
01758      COMPUTE TOT-T-SUB-YTD-COMP =                                 EL522
01759              TOT-C-SUB-YTD-COMP + TOT-B-SUB-YTD-COMP.             EL522
01760      MOVE TOT-T-SUB-YTD-COMP     TO  WS-D3-AMT5.                  EL522
01761                                                                   EL522
01762      MOVE WS-DETAIL3             TO  PRT.                         EL522
01763      PERFORM WRITE-PRINTER.                                       EL522
01764                                                                   EL522
01765      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01766      MOVE '    NET CHANGE TO YTD COMP      '                      EL522
01767                                  TO  WS-D3-DESC.                  EL522
01768      COMPUTE WS-NET-TOTAL =                                       EL522
01769              TOT-C-ADD-TO-YTD-COMP - TOT-C-SUB-YTD-COMP.          EL522
01770      MOVE WS-NET-TOTAL           TO  WS-D3-AMT2.                  EL522
01771      COMPUTE WS-NET-TOTAL =                                       EL522
01772              TOT-B-ADD-TO-YTD-COMP - TOT-B-SUB-YTD-COMP.          EL522
01773      MOVE WS-NET-TOTAL           TO  WS-D3-AMT2.                  EL522
01774      COMPUTE WS-NET-TOTAL =                                       EL522
01775              TOT-T-ADD-TO-YTD-COMP - TOT-T-SUB-YTD-COMP.          EL522
01776      MOVE WS-NET-TOTAL           TO  WS-D3-AMT6.                  EL522
01777                                                                   EL522
01778      MOVE WS-DETAIL3             TO  PRT.                         EL522
01779      PERFORM WRITE-PRINTER.                                       EL522
01780                                                                   EL522
01781      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01782      MOVE WS-DETAIL3             TO  PRT.                         EL522
01783      PERFORM WRITE-PRINTER.                                       EL522
01784                                                                   EL522
01785      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01786      MOVE '**** MISCELLANEOUS ENTRIES  ****'                      EL522
01787                                  TO  WS-D3-DESC.                  EL522
01788      MOVE WS-DETAIL3             TO  PRT.                         EL522
01789      PERFORM WRITE-PRINTER.                                       EL522
01790                                                                   EL522
01791      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01792      MOVE 'F - FICA                        '                      EL522
01793                                  TO  WS-D3-DESC.                  EL522
01794      MOVE TOT-C-FICA-ENTRY       TO  WS-D3-AMT1.                  EL522
01795      MOVE TOT-B-FICA-ENTRY       TO  WS-D3-AMT3.                  EL522
01796      COMPUTE TOT-T-FICA-ENTRY =                                   EL522
01797              TOT-C-FICA-ENTRY + TOT-B-FICA-ENTRY.                 EL522
01798      MOVE TOT-T-FICA-ENTRY       TO  WS-D3-AMT5.                  EL522
01799                                                                   EL522
01800      MOVE WS-DETAIL3             TO  PRT.                         EL522
01801      PERFORM WRITE-PRINTER.                                       EL522
01802                                                                   EL522
01803      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01804      MOVE WS-DETAIL3             TO  PRT.                         EL522
01805      PERFORM WRITE-PRINTER.                                       EL522
01806                                                                   EL522
01807      MOVE SPACES                 TO  WS-DETAIL3.                  EL522
01808                                                                   EL522
01809                                                                   EL522
01810 * MODIFIED 02/22/95 TO MATCH THE CALCULATION FROM                 EL522
01811 * ECS061 FOR BALANCING PURPOSES.                                  EL522
01812                                                                   EL522
01813      IF ME-DO-UPDATE                                              EL522
01814          COMPUTE ME-522-PY-ADJ =                                  EL522
01815                   (TOT-C-REMIT-RECEIVED + TOT-C-ADJ-REM-RECEIVED) EL522
01816                 + (TOT-C-DEPOSIT + TOT-C-ADJ-DEPOSIT)             EL522
01817                 - (TOT-C-CHARGE-TO-AGENT + TOT-C-ADJ-CHG-TO-AGENT)EL522
01818                 +  TOT-C-ADD-TO-BALANCE.                          EL522
01819                                                                   EL522
01820      MOVE '-'                TO  WS-H4-CTL.                       EL522
01821      MOVE WS-H4-TITLE-4      TO  WS-H4-TITLE.                     EL522
01822      MOVE WS-HEADING5-D1     TO  WS-HEADING5.                     EL522
01823                                                                   EL522
01824      PERFORM WRITE-HEADINGS.                                      EL522
01825                                                                   EL522
01826      MOVE TOT-LIFE-MORTALITY-AMT  TO  WS-D2-AMT3.                 EL522
01827      MOVE TOT-LF-AMT-INFORCE      TO  WS-D2-AMT4.                 EL522
01828      MOVE TOT-AH-AMT-INFORCE      TO  WS-D2-AMT5.                 EL522
01829                                                                   EL522
01830      MOVE '0'              TO  P-CTL.                             EL522
01831      MOVE WS-DETAIL2       TO  P-DATA.                            EL522
01832                                                                   EL522
01833      PERFORM WRITE-A-LINE.                                        EL522
01834                                                                   EL522
01835      MOVE WS-HEADING5-D2     TO  PRT.                             EL522
01836                                                                   EL522
01837      PERFORM WRITE-PRINTER.                                       EL522
01838                                                                   EL522
01839      MOVE TOT-FUTURE-RESERVE      TO  WS-D2-AMT3.                 EL522
01840      MOVE TOT-PTC-RESERVE         TO  WS-D2-AMT4.                 EL522
01841      MOVE TOT-IBNR-RESERVE        TO  WS-D2-AMT5.                 EL522
01842      MOVE TOT-CLAIM-ADJ-AMT       TO  WS-D2-AMT6.                 EL522
01843                                                                   EL522
01844      MOVE '0'              TO  P-CTL.                             EL522
01845      MOVE WS-DETAIL2       TO  P-DATA.                            EL522
01846                                                                   EL522
01847      PERFORM WRITE-A-LINE.                                        EL522
01848                                                                   EL522
01849      MOVE WS-HEADING5-D3     TO  PRT.                             EL522
01850                                                                   EL522
01851      PERFORM WRITE-PRINTER.                                       EL522
01852                                                                   EL522
01853      MOVE TOT-EXPENSES            TO  WS-D2-AMT3.                 EL522
01854      MOVE TOT-PAYMENTS            TO  WS-D2-AMT4.                 EL522
01855      MOVE TOT-OTHER-COMM          TO  WS-D2-AMT5.                 EL522
01856      MOVE TOT-REIN-PREM-ADJ       TO  WS-D2-AMT6.                 EL522
01857                                                                   EL522
01858      ADD TOT-EXPENSES    TOT-PAYMENTS                             EL522
01859          TOT-OTHER-COMM  TOT-REIN-PREM-ADJ                        EL522
01860             GIVING ME-522-RETROS.                                 EL522
01861                                                                   EL522
01862      MOVE '0'              TO  P-CTL.                             EL522
01863      MOVE WS-DETAIL2       TO  P-DATA.                            EL522
01864                                                                   EL522
01865      PERFORM WRITE-A-LINE.                                        EL522
01866                                                                   EL522
01867      MOVE SPACES           TO  WS-HEADING4.                       EL522
01868      MOVE SPACES           TO  WS-HEADING5.                       EL522
01869                                                                   EL522
01870      PERFORM WRITE-HEADINGS.                                      EL522
01871                                                                   EL522
01872      GO TO 2099-XIT.                                              EL522
01873                                                                   EL522
01874  1900-RELEASE-EXTRACT-DATA.                                       EL522
01875      MOVE 'ED'                   TO ED-RECORD-ID.                 EL522
01876      MOVE EX-RECORD-TYPE         TO ED-RECORD-TYPE.               EL522
01877      MOVE EX-DATA-AREAS          TO ED-DATA-AREAS.                EL522
01878      MOVE EXTRACT-DATA-RECORD    TO S-EXTRACT-DATA-RECORD.        EL522
01879                                                                   EL522
01880      RELEASE SORT-RECORD.                                         EL522
01881                                                                   EL522
01882      ADD +1  TO  WS-TRANS-OUTPUT-COUNT.                           EL522
01883                                                                   EL522
01884  1999-XIT.                                                        EL522
01885      EXIT.                                                        EL522
01886                                                                   EL522
01887  2099-XIT.                                                        EL522
01888      EXIT.                                                        EL522
01889  EJECT                                                            EL522
01890  8000-WRITE-ED-FILE   SECTION.                                    EL522
01891                                                                   EL522
01892      RETURN SORT-WORK-FILE                                        EL522
01893          AT END GO TO 8099-XIT.                                   EL522
01894                                                                   EL522
01895      MOVE S-EXTRACT-DATA-RECORD  TO EXTRACT-DATA-RECORD.          EL522
01896                                                                   EL522
01897      WRITE EXTRACT-DATA-FILE-RECORD FROM EXTRACT-DATA-RECORD.     EL522
01898      GO TO 8000-WRITE-ED-FILE.                                    EL522
01899                                                                   EL522
01900  8099-XIT.                                                        EL522
01901      EXIT.                                                        EL522
01902  EJECT                                                            EL522
01903  8500-DATE-CONVERSION SECTION.   COPY ELCDCS.                     EL522
01904  EJECT                                                            EL522
01905  WRITE-A-LINE SECTION.           COPY ELCWAL.                     EL522
01906  EJECT                                                            EL522
01907  WRITE-HEADINGS SECTION.                                          EL522
01908  WHS-010.                                                         EL522
01909                                                                   EL522
01910      ADD +1  TO  WS-PAGE.                                         EL522
01911      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL522
01912      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL522
01913                                                                   EL522
01914      MOVE WS-HEADING1            TO  PRT.                         EL522
01915      MOVE '1'                    TO  X.                           EL522
01916      PERFORM WRITE-PRINTER.                                       EL522
01917                                                                   EL522
01918      MOVE WS-HEADING2            TO  PRT.                         EL522
01919      MOVE ' '                    TO  X.                           EL522
01920      PERFORM WRITE-PRINTER.                                       EL522
01921                                                                   EL522
01922      MOVE WS-HEADING3            TO  PRT.                         EL522
01923      MOVE ' '                    TO  X.                           EL522
01924      PERFORM WRITE-PRINTER.                                       EL522
01925                                                                   EL522
01926      MOVE WS-HEADING4            TO  PRT.                         EL522
01927      MOVE ' '                    TO  X.                           EL522
01928      PERFORM WRITE-PRINTER.                                       EL522
01929                                                                   EL522
01930      MOVE WS-HEADING5            TO  PRT.                         EL522
01931      PERFORM WRITE-PRINTER.                                       EL522
01932                                                                   EL522
01933      MOVE +6                     TO  WS-LINE-COUNT.               EL522
01934                                                                   EL522
01935  WHS-020.                        COPY ELCWHS2.                    EL522
01936  EJECT                                                            EL522
01937  WRITE-PRINTER SECTION.          COPY ELCWPS.                     EL522
01938                                                                   EL522
01939  WPS-020.                        COPY ELCPRT2X.                   EL522
01940  EJECT                                                            EL522
01941  8900-OPEN-FILES SECTION.                                         EL522
01942                                                                   EL522
01943  8910-OFS-010.                                                    EL522
01944      OPEN INPUT  EXTRACT-INTERFACE-FILE                           EL522
01945           OUTPUT EXTRACT-DATA-FILE                                EL522
01946                  PAYMENTS-AND-ADJUSTMENTS                         EL522
01947                  RETRO-PAYMENTS                                   EL522
062104                 ME50-EL522-BALANCE 
01948                  PRNTR.                                           EL522
01949                                                                   EL522
01950      MOVE BIN-RUN-DATE           TO  WS-RUN-DATE-BIN.             EL522
01951                                                                   EL522
01952  8999-OFS-XIT.                                                    EL522
01953      EXIT.                                                        EL522
01954  EJECT                                                            EL522
01955  9000-CLOSE-FILES SECTION.                                        EL522
01956      MOVE SPACES                   TO FINAL-TOTALS-PRINT-LINE.    EL522
01957                                                                   EL522
01958      MOVE FTP-TITLE-1              TO  FTP-TITL.                  EL522
01959      MOVE WS-EXTRACTS-READ         TO  FTP-TOTL-1.                EL522
01960      MOVE '0'                      TO  P-CTL.                     EL522
01961      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
01962                                                                   EL522
01963      PERFORM WRITE-A-LINE.                                        EL522
01964                                                                   EL522
01965      MOVE FTP-TITLE-2              TO  FTP-TITL.                  EL522
01966      MOVE WS-PEND-BUS-RECORDS      TO  FTP-TOTL-1.                EL522
01967      MOVE '0'                      TO  P-CTL.                     EL522
01968      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
01969                                                                   EL522
01970      PERFORM WRITE-A-LINE.                                        EL522
01971                                                                   EL522
01972      MOVE FTP-TITLE-2A             TO  FTP-TITL.                  EL522
01973      MOVE WS-BATCH-TRAILERS        TO  FTP-TOTL-1.                EL522
062104     MOVE WS-BATCH-TRAILERS        TO  WS-ME50-BATCH-TRAILERS.
01974      MOVE '0'                      TO  P-CTL.                     EL522
01975      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
01976                                                                   EL522
01977      PERFORM WRITE-A-LINE.                                        EL522
01978                                                                   EL522
01979      MOVE FTP-TITLE-2B             TO  FTP-TITL.                  EL522
01980      MOVE WS-GOOD-ISSUES           TO  FTP-TOTL-1.                EL522
062104     MOVE WS-GOOD-ISSUES           TO  WS-ME50-VALID-ISSUES.
01981      MOVE '0'                      TO  P-CTL.                     EL522
01982      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
01983                                                                   EL522
01984      PERFORM WRITE-A-LINE.                                        EL522
01985                                                                   EL522
01986      MOVE FTP-TITLE-2C             TO  FTP-TITL.                  EL522
01987      MOVE WS-GOOD-CANCELS          TO  FTP-TOTL-1.                EL522
062104     MOVE WS-GOOD-CANCELS          TO  WS-ME50-VALID-CANCELS.
01988      MOVE '0'                      TO  P-CTL.                     EL522
01989      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
01990                                                                   EL522
01991      PERFORM WRITE-A-LINE.                                        EL522
01992                                                                   EL522
062104     ADD WS-ME50-BATCH-TRAILERS 
062104         WS-ME50-VALID-ISSUES
062104         WS-ME50-VALID-CANCELS
062104         GIVING WS-ME50-PEND-BUS-RECS.

062104     SUBTRACT +15 FROM WS-ME50-PEND-BUS-RECS 
062104         GIVING WS-ME50-TEMP-BAL-AMT.
062104     IF WS-ME50-TEMP-BAL-AMT < +0
062104         MOVE +0                   TO WS-ME50-TEMP-BAL-AMT 
062104     END-IF.
062104     MOVE WS-ME50-TEMP-BAL-AMT     TO WS-ME50-BAL-AMT-LOW.

062104     ADD +15 TO WS-ME50-PEND-BUS-RECS 
062104         GIVING WS-ME50-TEMP-BAL-AMT.
062104     MOVE WS-ME50-TEMP-BAL-AMT     TO WS-ME50-BAL-AMT-HIGH.

062104     MOVE WS-BAL50-DESCRIPTION   TO WS-ME50-BAL-DESCRIP.
062104     WRITE ME50-EL522-BALANCE-REC FROM WS-ME50-BALANCE-REC.

01993      MOVE FTP-TITLE-2D             TO  FTP-TITL.                  EL522
01994      MOVE WS-BAD-ISSUES            TO  FTP-TOTL-1.                EL522
01995      MOVE '0'                      TO  P-CTL.                     EL522
01996      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
01997                                                                   EL522
01998      PERFORM WRITE-A-LINE.                                        EL522
01999                                                                   EL522
02000      MOVE FTP-TITLE-2E             TO  FTP-TITL.                  EL522
02001      MOVE WS-BAD-CANCELS           TO  FTP-TOTL-1.                EL522
02002      MOVE '0'                      TO  P-CTL.                     EL522
02003      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02004                                                                   EL522
02005      PERFORM WRITE-A-LINE.                                        EL522
02006                                                                   EL522
02007      MOVE FTP-TITLE-2F           TO  FTP-TITL.                    EL522
02008      MOVE WS-RETURNED-ISSUES     TO  FTP-TOTL-1.                  EL522
02009      MOVE '0'                    TO  P-CTL.                       EL522
02010      MOVE FINAL-TOTALS-PRINT-LINE                                 EL522
02011                                  TO  P-DATA.                      EL522
02012                                                                   EL522
02013      PERFORM WRITE-A-LINE.                                        EL522
02014                                                                   EL522
02015      MOVE FTP-TITLE-2G           TO  FTP-TITL.                    EL522
02016      MOVE WS-RETURNED-CANCELS    TO  FTP-TOTL-1.                  EL522
02017      MOVE '0'                    TO  P-CTL.                       EL522
02018      MOVE FINAL-TOTALS-PRINT-LINE                                 EL522
02019                                  TO  P-DATA.                      EL522
02020                                                                   EL522
02021      PERFORM WRITE-A-LINE.                                        EL522
02022                                                                   EL522
02023      MOVE FTP-TITLE-3              TO  FTP-TITL.                  EL522
02024      MOVE WS-CLAIMS-RECORDS        TO  FTP-TOTL-1.                EL522
02025      MOVE '0'                      TO  P-CTL.                     EL522
02026      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02027                                                                   EL522
02028      PERFORM WRITE-A-LINE.                                        EL522
02029                                                                   EL522
02030      MOVE FTP-TITLE-3A             TO  FTP-TITL.                  EL522
02031      MOVE WS-CLAIMS-PAMTS          TO  FTP-TOTL-1.                EL522
02032      MOVE '0'                      TO  P-CTL.                     EL522
02033      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02034                                                                   EL522
02035      PERFORM WRITE-A-LINE.                                        EL522
02036                                                                   EL522
02037      MOVE FTP-TITLE-3B             TO  FTP-TITL.                  EL522
02038      MOVE WS-CLAIMS-EXPENSE        TO  FTP-TOTL-1.                EL522
02039      MOVE '0'                      TO  P-CTL.                     EL522
02040      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02041                                                                   EL522
02042      PERFORM WRITE-A-LINE.                                        EL522
02043                                                                   EL522
02044      MOVE FTP-TITLE-3C             TO  FTP-TITL.                  EL522
02045      MOVE WS-CLAIMS-RESERVE        TO  FTP-TOTL-1.                EL522
02046      MOVE '0'                      TO  P-CTL.                     EL522
02047      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02048                                                                   EL522
02049      PERFORM WRITE-A-LINE.                                        EL522
02050                                                                   EL522
02051      MOVE FTP-TITLE-3D             TO  FTP-TITL.                  EL522
02052      MOVE WS-CLAIMS-PMT-BAD        TO  FTP-TOTL-1.                EL522
02053      MOVE '0'                      TO  P-CTL.                     EL522
02054      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02055                                                                   EL522
02056      PERFORM WRITE-A-LINE.                                        EL522
02057                                                                   EL522
02058      MOVE FTP-TITLE-3E             TO  FTP-TITL.                  EL522
02059      MOVE WS-CLAIMS-EXP-BAD        TO  FTP-TOTL-1.                EL522
02060      MOVE '0'                      TO  P-CTL.                     EL522
02061      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02062                                                                   EL522
02063      PERFORM WRITE-A-LINE.                                        EL522
02064                                                                   EL522
02065      MOVE FTP-TITLE-3F             TO  FTP-TITL.                  EL522
02066      MOVE WS-CLAIMS-RSV-BAD        TO  FTP-TOTL-1.                EL522
02067      MOVE '0'                      TO  P-CTL.                     EL522
02068      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02069                                                                   EL522
02070      PERFORM WRITE-A-LINE.                                        EL522
02071                                                                   EL522
02072      MOVE FTP-TITLE-4              TO  FTP-TITL.                  EL522
02073      MOVE WS-CHANGE-COUNT          TO  FTP-TOTL-1.                EL522
02074      MOVE '0'                      TO  P-CTL.                     EL522
02075      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02076                                                                   EL522
02077      PERFORM WRITE-A-LINE.                                        EL522
02078                                                                   EL522
02079      MOVE FTP-TITLE-9              TO  FTP-TITL.                  EL522
02080      MOVE WS-TRANS-OUTPUT-COUNT    TO  FTP-TOTL-1.                EL522
02081      MOVE '0'                      TO  P-CTL.                     EL522
02082      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02083                                                                   EL522
02084      PERFORM WRITE-A-LINE.                                        EL522
02085                                                                   EL522
02086      MOVE FTP-TITLE-5              TO  FTP-TITL.                  EL522
02087      MOVE WS-PAYMTS-COUNT          TO  FTP-TOTL-1.                EL522
02088      MOVE '0'                      TO  P-CTL.                     EL522
02089      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02090                                                                   EL522
02091      PERFORM WRITE-A-LINE.                                        EL522
02092                                                                   EL522
02093      MOVE FTP-TITLE-7              TO  FTP-TITL.                  EL522
02094      MOVE WS-PYMTS-OUTPUT-COUNT    TO  FTP-TOTL-1.                EL522
02095      MOVE '0'                      TO  P-CTL.                     EL522
02096      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02097                                                                   EL522
02098      PERFORM WRITE-A-LINE.                                        EL522
02099                                                                   EL522
02100      MOVE FTP-TITLE-6              TO  FTP-TITL.                  EL522
02101      MOVE WS-RETROS-COUNT          TO  FTP-TOTL-1.                EL522
02102      MOVE '0'                      TO  P-CTL.                     EL522
02103      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02104                                                                   EL522
02105      PERFORM WRITE-A-LINE.                                        EL522
02106                                                                   EL522
02107      MOVE FTP-TITLE-8              TO  FTP-TITL.                  EL522
02108      MOVE WS-RETRO-OUTPUT-COUNT    TO  FTP-TOTL-1.                EL522
02109      MOVE '0'                      TO  P-CTL.                     EL522
02110      MOVE FINAL-TOTALS-PRINT-LINE  TO  P-DATA.                    EL522
02111                                                                   EL522
02112      PERFORM WRITE-A-LINE.                                        EL522
02113                                                                   EL522
02114  9010-CFS-010.                   COPY ELCPRTCX.                   EL522
02115                                                                   EL522
02116      CLOSE EXTRACT-INTERFACE-FILE                                 EL522
02117            EXTRACT-DATA-FILE                                      EL522
02118            RETRO-PAYMENTS                                         EL522
02119            PAYMENTS-AND-ADJUSTMENTS                               EL522
062104           ME50-EL522-BALANCE
02120            PRNTR.                                                 EL522
02121                                                                   EL522
02122  9099-CFS-XIT.                                                    EL522
02123      EXIT.                                                        EL522
02124  EJECT                                                            EL522
02125  ABEND-PGM SECTION.              COPY ELCABEND.                   EL522
