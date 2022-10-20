00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL528
00003  PROGRAM-ID.                 EL528 .                                 LV002
00004 *              PROGRAM CONVERTED BY                               EL528
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL528
00006 *              CONVERSION DATE 04/10/96 10:10:29.                 EL528
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL528
00008 *                            VMOD=2.007.                          EL528
00009 *AUTHOR.     LOGIC, INC.                                          EL528
00010 *            DALLAS, TEXAS.                                       EL528
00011                                                                   EL528
00012 *DATE-COMPILED.                                                   EL528
00013                                                                   EL528
00014                                                                   EL528
00015 *SECURITY.   *****************************************************EL528
00016 *            *                                                   *EL528
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL528
00018 *            *                                                   *EL528
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL528
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL528
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL528
00022 *            *                                                   *EL528
00023 *            *****************************************************EL528
00024                                                                   EL528
00025 *REMARKS.                                                         EL528
00026                                                                   EL528
00027 *        THIS PROGRAM PRINTS A REPORT SHOWING ALL OUT OF BALANCE  EL528
00028 *    BATCHES WITH CONTROL BREAKS AT THE ACCOUNT LEVEL.            EL528
00029                                                                   EL528
00030      EJECT                                                        EL528
00031  ENVIRONMENT DIVISION.                                            EL528
00032  CONFIGURATION SECTION.                                           EL528
00033  SPECIAL-NAMES.                                                   EL528
00034      C02 IS LCP-CH2                                               EL528
00035      C03 IS LCP-CH3                                               EL528
00036      C04 IS LCP-CH4                                               EL528
00037      C05 IS LCP-CH5                                               EL528
00038      C06 IS LCP-CH6                                               EL528
00039      C07 IS LCP-CH7                                               EL528
00040      C08 IS LCP-CH8                                               EL528
00041      C09 IS LCP-CH9                                               EL528
00042      C10 IS LCP-CH10                                              EL528
00043      C11 IS LCP-CH11                                              EL528
00044      C12 IS LCP-CH12                                              EL528
00045      S01 IS LCP-P01                                               EL528
00046      S02 IS LCP-P02.                                              EL528
00047                                                                   EL528
00048  INPUT-OUTPUT SECTION.                                            EL528
00049                                                                   EL528
00050  FILE-CONTROL.                                                    EL528
00051                                                                   EL528
00052                                                                   EL528
00053      SELECT SORT-FILE  ASSIGN TO SYS001-FBA1-S-SORTWK1.           EL528
00054                                                                   EL528
00055      SELECT PRNTR      ASSIGN TO SYS008-UR-1403-S-SYS008.         EL528
00056                                                                   EL528
00057      SELECT DISK-DATE  ASSIGN TO SYS019-FBA1-S-SYS019.            EL528
00058                                                                   EL528
00059      SELECT ERPNDB ASSIGN TO SYS022-FBA1-ERPNDB                   EL528
00060                    ORGANIZATION     IS INDEXED                    EL528
00061                    ACCESS           IS DYNAMIC                    EL528
00062                    RECORD KEY       IS PB-CONTROL-PRIMARY         EL528
00063                    FILE STATUS      IS ERPNDB-FILE-STATUS.        EL528
00064                                                                   EL528
00065      SELECT FICH   ASSIGN TO SYS020-UT-2400-S-SYS020.             EL528
00066                                                                   EL528
00067      SELECT ELCNTL ASSIGN TO SYS022-FBA1-ELCNTL                   EL528
00068                    ORGANIZATION     IS INDEXED                    EL528
00069                    ACCESS           IS DYNAMIC                    EL528
00070                    RECORD KEY       IS CF-CONTROL-PRIMARY         EL528
00071                    FILE STATUS      IS ELCNTL-FILE-STATUS.        EL528
00072                                                                   EL528
00073      SELECT ERACCT ASSIGN TO SYS021-FBA1-ERACCT2                  EL528
00074                    ORGANIZATION     IS INDEXED                    EL528
00075                    ACCESS           IS DYNAMIC                    EL528
00076                    RECORD KEY       IS AM-CONTROL-BY-VAR-GRP      EL528
00077                    FILE STATUS      IS ERACCT-FILE-STATUS.        EL528
00078                                                                   EL528
00079      SELECT ELREPT ASSIGN TO SYS024-FBA1-ELREPT                   EL528
00080                    ORGANIZATION     IS INDEXED                    EL528
00081                    ACCESS           IS DYNAMIC                    EL528
00082                    RECORD KEY       IS RF-CONTROL-PRIMARY         EL528
00083                    FILE STATUS      IS ELREPT-FILE-STATUS.        EL528
00084                                                                   EL528
00085      SELECT ELPGMS ASSIGN TO SYS025-FBA1-ELPGMS                   EL528
00086                    ORGANIZATION     IS INDEXED                    EL528
00087                    ACCESS           IS DYNAMIC                    EL528
00088                    RECORD KEY       IS PS-CONTROL-PRIMARY         EL528
00089                    FILE STATUS      IS ELPGMS-FILE-STATUS.        EL528
00090                                                                   EL528
00091      EJECT                                                        EL528
00092  DATA DIVISION.                                                   EL528
00093                                                                   EL528
00094  FILE SECTION.                                                    EL528
00095                                                                   EL528
00096  FD  PRNTR                       COPY ELCPRTFD.                   EL528
00097                                                                   EL528
00098  FD  DISK-DATE                   COPY ELCDTEFD.                   EL528
00099                                                                   EL528
00100  FD  ERPNDB.                                                      EL528
00101                                                                   EL528
00102                                  COPY ERCPNDB.                    EL528
00103                                                                   EL528
00104      EJECT                                                        EL528
00105                                                                   EL528
00106  FD  FICH                        COPY ELCFCHFD.                   EL528
00107                                                                   EL528
00108  FD  ELCNTL.                                                      EL528
00109                                                                   EL528
00110                                  COPY ELCCNTL.                    EL528
00111                                                                   EL528
00112      EJECT                                                        EL528
00113  FD  ERACCT.                                                      EL528
00114                                                                   EL528
00115                                  COPY ERCACCT.                    EL528
00116                                                                   EL528
00117      EJECT                                                        EL528
00118  FD  ELPGMS.                                                      EL528
00119                                                                   EL528
00120                                  COPY ELCPGMS.                    EL528
00121                                                                   EL528
00122      EJECT                                                        EL528
00123  FD  ELREPT                      COPY ELCRPTFD.                   EL528
00124                                                                   EL528
00125                                  COPY ELCREPT.                    EL528
00126                                                                   EL528
00127  SD  SORT-FILE.                                                   EL528
00128                                                                   EL528
00129                                  COPY ERCEXTR.                    EL528
00130                                                                   EL528
00131      EJECT                                                        EL528
00132  WORKING-STORAGE SECTION.                                         EL528
00133  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL528
00134  01  LCP-CURRENT-DATE-68.                                         EL528
00135      05  LCP-MONTH                 PIC X(2).                      EL528
00136      05  FILLER                    PIC X VALUE '/'.               EL528
00137      05  LCP-DAY1                  PIC X(2).                      EL528
00138      05  FILLER                    PIC X VALUE '/'.               EL528
00139      05  LCP-YEAR                  PIC X(2).                      EL528
00140  01  LCP-DATE-NEW-74.                                             EL528
00141      05  LCP-YEAR                  PIC X(2).                      EL528
00142      05  LCP-MONTH                 PIC X(2).                      EL528
00143      05  LCP-DAY1                  PIC X(2).                      EL528
00144  77  LCP-ASA                       PIC X.                         EL528
00145                                                                   EL528
00146  77  FILLER  PIC X(32)   VALUE '********************************'.EL528
00147  77  FILLER  PIC X(32)   VALUE '*     EL528  WORKING STORAGE   *'.EL528
00148  77  FILLER  PIC X(32)   VALUE '********** V/M 2.007 ***********'.EL528
00149                                                                   EL528
00150  01  WS-ELREPT-SWITCHES.                                          EL528
00151      05  WS-LINE-NUMBER              PIC S9(8)   VALUE +0  COMP.  EL528
00152      05  WS-START-SW                 PIC S9      VALUE +0.        EL528
00153      05  WS-ELREPT-STATUS-SW         PIC S9      VALUE +0.        EL528
00154          88  ELREPT-NOT-OPEN                     VALUE +0.        EL528
00155          88  ELREPT-OPEN                         VALUE +1.        EL528
00156      05  WS-FICHE-STATUS-SW          PIC S9      VALUE +0.        EL528
00157          88  FICHE-NOT-OPEN                      VALUE +0.        EL528
00158          88  FICHE-OPEN                          VALUE +1.        EL528
00159      05  WS-PRNTR-STATUS-SW          PIC S9      VALUE +0.        EL528
00160          88  PRNTR-NOT-OPEN                      VALUE +0.        EL528
00161          88  PRNTR-OPEN                          VALUE +1.        EL528
00162      05  WS-FIRST-TIME-SW            PIC S9      VALUE +0.        EL528
00163          88  FIRST-TIME                          VALUE +0.        EL528
00164          88  NOT-FIRST-TIME                      VALUE +1.        EL528
00165      05  WS-FIRST-TIME-OPTION        PIC S9      VALUE +0.        EL528
00166          88  FIRST-TIME-OPTION                   VALUE +0.        EL528
00167          88  NOT-FIRST-TIME-OPT                  VALUE +1.        EL528
00168      05  WS-COMPANY-ID               PIC X(3).                    EL528
00169      05  WS-COMPANY-CD               PIC X.                       EL528
00170      05  WS-ALPHA-DATE               PIC X(20)   VALUE SPACES.    EL528
00171      05  WS-REPORT-ID                PIC X(5)    VALUE 'EL528'.   EL528
00172      05  WS-PROGRAM-OPTIONS.                                      EL528
00173          10  WS-FREQUENCY-OPTION     PIC X(4).                    EL528
00174          10  WS-PRINT-OPTION         PIC X.                       EL528
00175          10  WS-FORMAT-OPTION        PIC X.                       EL528
00176          10  WS-PROCESS-OPTION       PIC X.                       EL528
00177          10  WS-TOTAL-OPTION         PIC X.                       EL528
00178                                                                   EL528
00179  01  WS-BATCH-TOTALS                 PIC X(82)   VALUE ZEROS.     EL528
00180                                                                   EL528
00181  01  WS-ISS-CAN-TOTALS               REDEFINES                    EL528
00182                                   WS-BATCH-TOTALS.                EL528
00183      05 WS-ISS-TOTAL-CNT             PIC S9(5).                   EL528
00184      05 WS-ISS-AVAIL-CNT             PIC S9(5).                   EL528
00185      05 WS-ISS-FATAL-CNT             PIC S9(5).                   EL528
00186      05 WS-ISS-FORCE-CNT             PIC S9(5).                   EL528
00187      05 WS-ISS-HOLD-CNT              PIC S9(5).                   EL528
00188      05 WS-ISS-GOOD-PREM             PIC S9(6)V99.                EL528
00189      05 WS-ISS-BAD-PREM              PIC S9(6)V99.                EL528
00190      05 WS-CAN-TOTAL-CNT             PIC S9(5).                   EL528
00191      05 WS-CAN-AVAIL-CNT             PIC S9(5).                   EL528
00192      05 WS-CAN-FATAL-CNT             PIC S9(5).                   EL528
00193      05 WS-CAN-FORCE-CNT             PIC S9(5).                   EL528
00194      05 WS-CAN-HOLD-CNT              PIC S9(5).                   EL528
00195      05 WS-CAN-GOOD-PREM             PIC S9(6)V99.                EL528
00196      05 WS-CAN-BAD-PREM              PIC S9(6)V99.                EL528
00197                                                                   EL528
00198                                                                   EL528
00199  01  FILLER                          COMP-3.                      EL528
00200      05  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       EL528
00201      05  WS-LINE-COUNT-MAX           PIC S9(3)   VALUE +58.       EL528
00202      05  WS-PAGE                     PIC S9(5)   VALUE +0.        EL528
00203      05  WS-REPORT-SW                PIC S9      VALUE +0.        EL528
00204      05  WS-HEADING-SW               PIC S9      VALUE +0.        EL528
00205      05  WS-PRINT-SW                 PIC S9      VALUE +0.        EL528
00206      05  WS-RECORD-COUNT             PIC S9(9)   VALUE +0.        EL528
00207      05  WS-RETURN-CODE              PIC S9(3)   VALUE +0.        EL528
00208      05  WS-ZERO                     PIC S9      VALUE +0.        EL528
00209      05  WS-NO-RECORDS-RELEASED      PIC S9(5)   VALUE +0.        EL528
00210      05  WS-EOF-SW                   PIC S9      VALUE +0.        EL528
00211          88  EXTRACT-EOF                         VALUE +1.        EL528
00212      05  WS-NO-BATCHES               PIC S999    VALUE +0.        EL528
00213          88  NEW-ACCOUNT                         VALUE +0.        EL528
00214          88  ONLY-ONE-BATCH                      VALUE +1.        EL528
00215      05  WS-INCURRED-AGE             PIC S9(3)   VALUE +0.        EL528
00216      05  WS-YEAR                     REDEFINES                    EL528
00217          WS-INCURRED-AGE             PIC S9(3).                   EL528
00218                                                                   EL528
00219      05  WS-AMOUNT                   PIC S9(9)V99 VALUE +0.       EL528
00220                                                                   EL528
00221      EJECT                                                        EL528
00222  01  FILLER                          COMP SYNC.                   EL528
00223      05  PGM-SUB                     PIC S9(4)   VALUE +528.      EL528
00224      05  WS-INDEX                    PIC S9(4)   VALUE ZERO.      EL528
00225      05  WS-LENGTH                   REDEFINES                    EL528
00226          WS-INDEX                    PIC S9(4).                   EL528
00227                                                                   EL528
00228  01  FILLER.                                                      EL528
00229      05  WS-DISPLAY-TIME             PIC 99B99B99.                EL528
00230      05  ABEND-CODE                  PIC X(4).                    EL528
00231      05  ABEND-OPTION                PIC X.                       EL528
00232      05  X                           PIC X       VALUE SPACE.     EL528
00233                                                                   EL528
00234      05  WS-SAVE-PRINT-RECORD        PIC X(133)  VALUE SPACES.    EL528
00235      05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL528
00236      05  WS-LAST-CARRIER             PIC X       VALUE SPACE.     EL528
00237                                                                   EL528
00238      05  WS-SAVE-KEY.                                             EL528
00239          10  WS-SAVE-CO-CD           PIC X.                       EL528
00240          10  WS-SAVE-ACCOUNT-KEY.                                 EL528
00241              15  WS-SAVE-CARRIER     PIC X.                       EL528
00242              15  WS-SAVE-GROUPING    PIC X(6).                    EL528
00243              15  WS-SAVE-STATE       PIC XX.                      EL528
00244              15  WS-SAVE-ACCOUNT     PIC X(10).                   EL528
00245          10  FILLER                  PIC X(16).                   EL528
00246                                                                   EL528
00247      05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL528
00248      05  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      EL528
00249      05  ERPNDB-FILE-STATUS          PIC XX      VALUE ZERO.      EL528
00250      05  ERACCT-FILE-STATUS          PIC XX      VALUE ZERO.      EL528
00251      05  ELPGMS-FILE-STATUS          PIC XX      VALUE ZERO.      EL528
00252      05  ELREPT-FILE-STATUS          PIC XX      VALUE ZERO.      EL528
00253                                                                   EL528
00254      05  WS-FILE-ERROR-MESSAGE.                                   EL528
00255          10  FILLER                  PIC X(24)   VALUE            EL528
00256              'ERROR OCCURED OPENING - '.                          EL528
00257          10  WS-FEM-FILE-NAME        PIC X(8).                    EL528
00258                                                                   EL528
00259      05  WS-COMPANY-NAME.                                         EL528
00260          10  WS-CN-CHAR              PIC X                        EL528
00261              OCCURS 30 TIMES         INDEXED BY CN1.              EL528
00262                                                                   EL528
00263      05  WS-COMPANY-NAME2.                                        EL528
00264          10  WS-CN2-CHAR             PIC X                        EL528
00265              OCCURS 30 TIMES         INDEXED BY CN2.              EL528
00266      05  WS-INITIALS.                                             EL528
00267          10  WS-INITIAL1             PIC X.                       EL528
00268          10  WS-INITIAL2             PIC X.                       EL528
00269                                                                   EL528
00270      05  WS-BIN-DATE-WORK-X.                                      EL528
00271          10  WS-BIN-DATE-WORK        PIC S9(4)                    EL528
00272                                      COMP.                        EL528
00273      05  WS-DATE-WORK.                                            EL528
00274          10  WS-DW-MONTH             PIC 99.                      EL528
00275          10  FILLER                  PIC X.                       EL528
00276          10  WS-DW-DAY               PIC 99.                      EL528
00277          10  FILLER                  PIC X.                       EL528
00278          10  WS-DW-YEAR              PIC 99.                      EL528
00279                                                                   EL528
00280      05  WS-DATE-PAID.                                            EL528
00281          10  WS-MONTH-PAID           PIC 99.                      EL528
00282          10  FILLER                  PIC X.                       EL528
00283          10  WS-DAY-PAID             PIC 99.                      EL528
00284          10  FILLER                  PIC X.                       EL528
00285          10  WS-YEAR-PAID            PIC 99.                      EL528
00286                                                                   EL528
00287  01  WS-TOTAL-LINES-AREA.                                         EL528
00288                                                                   EL528
00289    02  WS-TOTAL-1.                                                EL528
00290      05  FILLER                      PIC X(18) VALUE              EL528
00291          'REMITTED'.                                              EL528
00292      05  WS-BT-CNTL-ISS-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00293      05  WS-BT-CNTL-CAN-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00294      05  WS-BT-CNTL-LF-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00295      05  WS-BT-CNTL-LF-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00296      05  WS-BT-CNTL-AH-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00297      05  WS-BT-CNTL-AH-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00298                                                                   EL528
00299    02  WS-TOTAL-2.                                                EL528
00300      05  FILLER                      PIC X(18) VALUE              EL528
00301          'ENTERED'.                                               EL528
00302      05  WS-BT-ENTR-ISS-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00303      05  WS-BT-ENTR-CAN-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00304      05  WS-BT-ENTR-LF-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00305      05  WS-BT-ENTR-LF-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00306      05  WS-BT-ENTR-AH-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00307      05  WS-BT-ENTR-AH-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00308                                                                   EL528
00309    02  WS-TOTAL-3.                                                EL528
00310      05  FILLER                      PIC X(18) VALUE              EL528
00311          'DIFFERENCE'.                                            EL528
00312      05  WS-BT-DIFF-ISS-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00313      05  WS-BT-DIFF-CAN-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00314      05  WS-BT-DIFF-LF-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00315      05  WS-BT-DIFF-LF-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00316      05  WS-BT-DIFF-AH-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00317      05  WS-BT-DIFF-AH-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00318                                                                   EL528
00319    02  WS-TOTAL-4.                                                EL528
00320      05  FILLER                      PIC X(18) VALUE              EL528
00321          'REMITTED'.                                              EL528
00322      05  WS-AT-CNTL-ISS-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00323      05  WS-AT-CNTL-CAN-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00324      05  WS-AT-CNTL-LF-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00325      05  WS-AT-CNTL-LF-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00326      05  WS-AT-CNTL-AH-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00327      05  WS-AT-CNTL-AH-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00328                                                                   EL528
00329    02  WS-TOTAL-5.                                                EL528
00330      05  FILLER                      PIC X(18) VALUE              EL528
00331          'ENTERED'.                                               EL528
00332      05  WS-AT-ENTR-ISS-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00333      05  WS-AT-ENTR-CAN-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00334      05  WS-AT-ENTR-LF-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00335      05  WS-AT-ENTR-LF-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00336      05  WS-AT-ENTR-AH-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00337      05  WS-AT-ENTR-AH-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00338                                                                   EL528
00339    02  WS-TOTAL-6.                                                EL528
00340      05  FILLER                      PIC X(18) VALUE              EL528
00341          'DIFFERENCE'.                                            EL528
00342      05  WS-AT-DIFF-ISS-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00343      05  WS-AT-DIFF-CAN-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00344      05  WS-AT-DIFF-LF-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00345      05  WS-AT-DIFF-LF-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00346      05  WS-AT-DIFF-AH-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00347      05  WS-AT-DIFF-AH-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00348                                                                   EL528
00349    02  WS-TOTAL-7.                                                EL528
00350      05  FILLER                      PIC X(18) VALUE              EL528
00351          'REMITTED'.                                              EL528
00352      05  WS-FT-CNTL-ISS-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00353      05  WS-FT-CNTL-CAN-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00354      05  WS-FT-CNTL-LF-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00355      05  WS-FT-CNTL-LF-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00356      05  WS-FT-CNTL-AH-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00357      05  WS-FT-CNTL-AH-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00358                                                                   EL528
00359    02  WS-TOTAL-8.                                                EL528
00360      05  FILLER                      PIC X(18) VALUE              EL528
00361          'ENTERED'.                                               EL528
00362      05  WS-FT-ENTR-ISS-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00363      05  WS-FT-ENTR-CAN-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00364      05  WS-FT-ENTR-LF-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00365      05  WS-FT-ENTR-LF-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00366      05  WS-FT-ENTR-AH-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00367      05  WS-FT-ENTR-AH-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00368                                                                   EL528
00369    02  WS-TOTAL-9.                                                EL528
00370      05  FILLER                      PIC X(18) VALUE              EL528
00371          'DIFFERENCE'.                                            EL528
00372      05  WS-FT-DIFF-ISS-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00373      05  WS-FT-DIFF-CAN-CNT          PIC S9(5)    COMP-3 VALUE +0.EL528
00374      05  WS-FT-DIFF-LF-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00375      05  WS-FT-DIFF-LF-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00376      05  WS-FT-DIFF-AH-PRM           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00377      05  WS-FT-DIFF-AH-REF           PIC S9(9)V99 COMP-3 VALUE +0.EL528
00378                                                                   EL528
00379  01  FILLER                          REDEFINES                    EL528
00380      WS-TOTAL-LINES-AREA.                                         EL528
00381      05  FILLER                      OCCURS 9 TIMES               EL528
00382          INDEXED BY TOT-INDX  TOT-INDX1  TOT-INDX2.               EL528
00383          10  WS-TOTAL-DESC            PIC X(18).                  EL528
00384          10  WS-TOTAL-ISS-CNT         PIC S9(5)     COMP-3.       EL528
00385          10  WS-TOTAL-CAN-CNT         PIC S9(5)     COMP-3.       EL528
00386          10  WS-TOTAL-LF-PRM          PIC S9(9)V99  COMP-3.       EL528
00387          10  WS-TOTAL-LF-REF          PIC S9(9)V99  COMP-3.       EL528
00388          10  WS-TOTAL-AH-PRM          PIC S9(9)V99  COMP-3.       EL528
00389          10  WS-TOTAL-AH-REF          PIC S9(9)V99  COMP-3.       EL528
00390                                                                   EL528
00391  01  TOT-INDX-MAX                     PIC S9(4)   VALUE +3        EL528
00392                                       COMP.                       EL528
00393  01  TOT-INDX1-MAX                    PIC S9(4)   VALUE +6        EL528
00394                                       COMP.                       EL528
00395  01  TOT-INDX2-MAX                    PIC S9(4)   VALUE +9        EL528
00396                                       COMP.                       EL528
00397                                                                   EL528
00398      EJECT                                                        EL528
00399  01  WS-HEADING1.                                                 EL528
00400      05  FILLER                      PIC X(20)   VALUE '1'.       EL528
00401      05  WS-H1-TITLE                 PIC X(51)   VALUE            EL528
00402          'PENDING BUSINESS SUMMARY - OUT OF BALANCE'.             EL528
00403      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL528'.      EL528
00404                                                                   EL528
00405  01  WS-HEADING2.                                                 EL528
00406      05  FILLER                      PIC X(25) VALUE SPACES.      EL528
00407      05  WS-H2-CLIENT-NAME           PIC X(30) VALUE SPACES.      EL528
00408      05  FILLER                      PIC X(16) VALUE SPACES.      EL528
00409      05  WS-H2-DATE                  PIC X(8)  VALUE SPACES.      EL528
00410                                                                   EL528
00411  01  WS-HEADING3.                                                 EL528
00412      05  FILLER                      PIC X(31) VALUE SPACES.      EL528
00413      05  WS-H3-DATE                  PIC X(18).                   EL528
00414      05  FILLER                      PIC X(22) VALUE SPACES.      EL528
00415      05  FILLER                      PIC X(5) VALUE 'PAGE'.       EL528
00416      05  WS-H3-PAGE                  PIC ZZZ9.                    EL528
00417                                                                   EL528
00418  01  WS-HEADING4.                                                 EL528
00419      05  FILLER                      PIC X(80) VALUE              EL528
00420          '0CARRIER  GROUPING  ST   ACCOUNT       ACCOUNT NAME'.   EL528
00421                                                                   EL528
00422  01  WS-HEADING5.                                                 EL528
00423      05  FILLER                      PIC X(4)  VALUE  SPACES.     EL528
00424      05  H5-CARRIER                  PIC X     VALUE  SPACES.     EL528
00425      05  FILLER                      PIC X(6)  VALUE  SPACES.     EL528
00426      05  H5-GROUPING                 PIC X(6)  VALUE  SPACES.     EL528
00427      05  FILLER                      PIC X(3)  VALUE  SPACES.     EL528
00428      05  H5-STATE                    PIC X(2)  VALUE  SPACES.     EL528
00429      05  FILLER                      PIC X(2)  VALUE  SPACES.     EL528
00430      05  H5-ACCOUNT                  PIC X(10) VALUE  SPACES.     EL528
00431      05  FILLER                      PIC X(2)  VALUE  SPACES.     EL528
00432      05  H5-ACCOUNT-NAME             PIC X(30) VALUE  SPACES.     EL528
00433      05  FILLER                      PIC X(14) VALUE  SPACES.     EL528
00434                                                                   EL528
00435                                                                   EL528
00436  01  WS-HEADING6.                                                 EL528
00437      05  FILLER                      PIC X(31)  VALUE             EL528
00438          '0   BATCH    ISSUE   CANCEL    '.                       EL528
00439      05  HDG6-OVRD-1                 PIC X(6).                    EL528
00440      05  FILLER                      PIC X(7) VALUE   SPACES.     EL528
00441      05  HDG6-OVRD-2                 PIC X(6).                    EL528
00442      05  FILLER                      PIC X(7) VALUE   SPACES.     EL528
00443      05  HDG6-OVRD-3                 PIC X(6).                    EL528
00444      05  FILLER                      PIC X(7) VALUE   SPACES.     EL528
00445      05  HDG6-OVRD-4                 PIC X(6).                    EL528
00446      05  FILLER                      PIC X(4) VALUE   SPACES.     EL528
00447                                                                   EL528
00448  01  WS-HEADING7.                                                 EL528
00449      05  FILLER                      PIC X(40) VALUE              EL528
00450          '    NUMBER   COUNT   COUNT    PREMIUM'.                 EL528
00451      05  FILLER                      PIC X(40)  VALUE             EL528
00452          '    REFUND      PREMIUM       REFUND'.                  EL528
00453                                                                   EL528
00454  01  WS-HEADING8.                                                 EL528
00455      05  FILLER                      PIC X(132) VALUE             EL528
00456          '0**ACCOUNT TOTALS**'.                                   EL528
00457                                                                   EL528
00458  01  WS-HEADING9.                                                 EL528
00459      05  FILLER                      PIC X(132) VALUE             EL528
00460          '0***FINAL TOTALS***'.                                   EL528
00461                                                                   EL528
00462  01  WS-SAVE-HEADING5                PIC X(80)  VALUE SPACES.     EL528
00463                                                                   EL528
00464      EJECT                                                        EL528
00465                                                                   EL528
00466  01  WS-DETAIL1.                                                  EL528
00467      05  FILLER                      PIC X.                       EL528
00468      05  WS-D1-DESC                  PIC X(10).                   EL528
00469      05  FILLER                      PIC X.                       EL528
00470      05  WS-D1-ISSUE-COUNT           PIC ZZ,ZZ9-.                 EL528
00471      05  FILLER                      PIC X.                       EL528
00472      05  WS-D1-CANCEL-COUNT          PIC ZZ,ZZ9-.                 EL528
00473      05  FILLER                      PIC X.                       EL528
00474      05  WS-D1-LIFE-PREMIUM          PIC ZZZZ,ZZZ.99-.            EL528
00475      05  FILLER                      PIC X.                       EL528
00476      05  WS-D1-LIFE-REFUND           PIC ZZZZ,ZZZ.99-.            EL528
00477      05  FILLER                      PIC X.                       EL528
00478      05  WS-D1-AH-PREMIUM            PIC ZZZZ,ZZZ.99-.            EL528
00479      05  FILLER                      PIC X.                       EL528
00480      05  WS-D1-AH-REFUND             PIC ZZZZ,ZZZ.99-.            EL528
00481      05  FILLER                      PIC X.                       EL528
00482                                                                   EL528
00483  01  WS-DETAIL2                      REDEFINES                    EL528
00484      WS-DETAIL1.                                                  EL528
00485      05  WS-D2-CTL                   PIC X.                       EL528
00486      05  FILLER                      PIC X(3).                    EL528
00487      05  WS-D2-BATCH-NO              PIC X(6).                    EL528
00488      05  FILLER                      PIC X(70).                   EL528
00489                                                                   EL528
00490      EJECT                                                        EL528
00491                                      COPY ELCDATE.                   CL**2
00492                                                                   EL528
00493                                      COPY ELCDTECX.               EL528
00494                                                                   EL528
00495                                      COPY ELCDTEVR.               EL528
00496 *                                                                 EL528
00497 *                                                                 EL528
00498 * LEAVE THE ABOVE 2 LINES IN TO PREVENT LOSING THE LINE ABOVE     EL528
00506      EJECT                                                        EL528
00507  PROCEDURE DIVISION.                                              EL528
00508  0000-LOAD-DATE-CARD.                                             EL528
00509                                  COPY ELCDTERX.                   EL528
00510  0000-MAIN-LOGIC SECTION.                                         EL528
00511                                                                   EL528
00512      PERFORM OPEN-FILES.                                          EL528
00513                                                                   EL528
00514      SORT SORT-FILE                                               EL528
00515          ON ASCENDING KEY EX-SORT-KEY-AREAS                       EL528
00516             INPUT  PROCEDURE IS 1000-SORT-INPUT-PROCEDURE         EL528
00517             OUTPUT PROCEDURE IS 1900-PROCESS-EXTRACT.             EL528
00518                                                                   EL528
00519      IF SORT-RETURN GREATER THAN ZERO                             EL528
00520          MOVE 'SORT FAILED'      TO WS-ABEND-MESSAGE              EL528
00521          MOVE SORT-RETURN        TO WS-RETURN-CODE                EL528
00522          GO TO ABEND-PGM.                                         EL528
00523                                                                   EL528
00524      PERFORM CLOSE-FILES.                                         EL528
00525                                                                   EL528
00526      GOBACK.                                                      EL528
00527                                                                   EL528
00528  0000-EXIT.                                                       EL528
00529      EXIT.                                                        EL528
00530      EJECT                                                        EL528
00531  1000-SORT-INPUT-PROCEDURE SECTION.                               EL528
00532      ACCEPT WS-ACCEPT-DATE FROM DATE.                             EL528
00533      MOVE WS-AD-YY               TO WS-CD-YY.                     EL528
00534      MOVE WS-AD-MM               TO WS-CD-MM.                     EL528
00535      MOVE WS-AD-DD               TO WS-CD-DD.                     EL528
00536      MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT.          EL528
00537      MOVE '2'                    TO DC-OPTION-CODE.               EL528
00538      PERFORM 8500-DATE-CONVERSION.                                EL528
00539      MOVE DC-GREG-DATE-1-ALPHA   TO ALPH-DATE.                    EL528
00540                                                                   EL528
00541  1010-SIP.                                                        EL528
00542      MOVE LOW-VALUES             TO CF-CONTROL-PRIMARY.           EL528
00543      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL528
00544      MOVE '1'                    TO CF-RECORD-TYPE.               EL528
00545      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL528
00546      MOVE +0                     TO CF-SEQUENCE-NO.               EL528
00547                                                                   EL528
00548      READ ELCNTL.                                                 EL528
00549                                                                   EL528
00550                                                                   EL528
00551      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL528
00552          MOVE 'ERROR OCCURED CLOSE - ELCNTL'                      EL528
00553                                  TO  WS-ABEND-MESSAGE             EL528
00554          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
00555          GO TO ABEND-PGM.                                         EL528
00556                                                                   EL528
00557                                                                   EL528
00558      MOVE CF-CL-MAIL-TO-NAME     TO  WS-COMPANY-NAME              EL528
00559                                      WS-H2-CLIENT-NAME.           EL528
00560      MOVE CF-COMPANY-ID          TO  WS-COMPANY-ID.               EL528
00561      MOVE CF-COMPANY-CD          TO  WS-COMPANY-CD.               EL528
00562      MOVE LOW-VALUES             TO  WS-LAST-CARRIER.             EL528
00563                                                                   EL528
00564      ACCEPT WS-TIME-OF-DAY       FROM TIME.                       EL528
00565      MOVE WS-TIME                TO  WS-DISPLAY-TIME.             EL528
00566      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'.            EL528
00567      DISPLAY 'BEGIN PROCESSING ' WS-H2-CLIENT-NAME ' AT '         EL528
00568              WS-DISPLAY-TIME UPON CONSOLE.                        EL528
00569                                                                   EL528
00570      MOVE SPACES                 TO  WS-COMPANY-NAME2.            EL528
00571      SET CN1 TO +30.                                              EL528
00572                                                                   EL528
00573  1020-SIP.                                                        EL528
00574      IF WS-CN-CHAR (CN1) = SPACES                                 EL528
00575          IF CN1 GREATER THAN +1                                   EL528
00576              SET CN1 DOWN BY +1                                   EL528
00577              GO TO 1020-SIP                                       EL528
00578            ELSE                                                   EL528
00579              GO TO 1040-SIP.                                      EL528
00580                                                                   EL528
00581      SET WS-LENGTH TO CN1.                                        EL528
00582                                                                   EL528
00583      SUBTRACT WS-LENGTH FROM +30 GIVING WS-LENGTH.                EL528
00584      DIVIDE +2 INTO WS-LENGTH ROUNDED.                            EL528
00585                                                                   EL528
00586      IF WS-LENGTH NOT GREATER THAN ZERO                           EL528
00587          GO TO 1040-SIP.                                          EL528
00588                                                                   EL528
00589      SET CN2 TO CN1.                                              EL528
00590      SET CN2 UP BY WS-LENGTH.                                     EL528
00591                                                                   EL528
00592  1030-SIP.                                                        EL528
00593      MOVE WS-CN-CHAR (CN1) TO WS-CN2-CHAR (CN2).                  EL528
00594                                                                   EL528
00595      IF CN1 GREATER THAN +1                                       EL528
00596          SET CN1                                                  EL528
00597              CN2 DOWN BY +1                                       EL528
00598          GO TO 1030-SIP.                                          EL528
00599                                                                   EL528
00600      MOVE WS-COMPANY-NAME2       TO  WS-COMPANY-NAME.             EL528
00601                                                                   EL528
00602      EJECT                                                        EL528
00603                                                                   EL528
00604  1040-SIP.                                                        EL528
00605 *    NOTE ******************************************************* EL528
00606 *         *      POSITION THE PENDING BUSINESS FILE AT THE BE-  * EL528
00607 *         *  GINNING OF THE COMPANY TO LOAD AND RELEASE ALL     * EL528
00608 *         *  ISSUES, CANCELS, AND BATCH RECORDS TO THE EXTRACT  * EL528
00609 *         *  SORT.                                              * EL528
00610 *         *******************************************************.EL528
00611                                                                   EL528
00612      MOVE LOW-VALUES             TO  PB-CONTROL-PRIMARY           EL528
00613      MOVE WS-COMPANY-CD          TO  PB-COMPANY-CD                EL528
00614      MOVE ZEROS                  TO  WS-ISS-CAN-TOTALS.           EL528
00615                                                                   EL528
00616      START ERPNDB                                                 EL528
00617          KEY IS GREATER THAN PB-CONTROL-PRIMARY.                  EL528
00618                                                                   EL528
00619      IF ERPNDB-FILE-STATUS = '23'                                 EL528
00620          DISPLAY 'EL528 NO RECORDS FOUND - ERPNDB  FOR CO - '     EL528
00621              WS-COMPANY-ID UPON CONSOLE                           EL528
00622          GO TO 1099-CLOSE-PENDING-BUS-FILE.                       EL528
00623                                                                   EL528
00624      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL528
00625          MOVE 'ERROR OCCURED START - ERPNDB'                      EL528
00626                                  TO  WS-ABEND-MESSAGE             EL528
00627          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
00628          GO TO ABEND-PGM.                                         EL528
00629                                                                   EL528
00630      EJECT                                                        EL528
00631                                                                   EL528
00632  1050-SIP.                                                        EL528
00633      READ ERPNDB NEXT RECORD.                                     EL528
00634                                                                   EL528
00635      IF ERPNDB-FILE-STATUS = '10'                                 EL528
00636          GO TO 1099-CLOSE-PENDING-BUS-FILE.                       EL528
00637                                                                   EL528
00638      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL528
00639          MOVE 'ERROR OCCURED READNEXT - ERPNDB'                   EL528
00640                                  TO  WS-ABEND-MESSAGE             EL528
00641          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
00642          GO TO ABEND-PGM.                                         EL528
00643                                                                   EL528
00644      IF PB-COMPANY-CD NOT EQUAL  TO WS-COMPANY-CD                 EL528
00645          GO TO 1099-CLOSE-PENDING-BUS-FILE.                       EL528
00646                                                                   EL528
00647                                                                   EL528
00648      IF PB-CREDIT-ACCEPT-DT NOT EQUAL LOW-VALUES                  EL528
00649          GO TO 1050-SIP.                                          EL528
00650                                                                   EL528
00651      IF PB-BATCH-TRAILER                                          EL528
00652          IF PB-OUT-OF-BAL                                         EL528
00653              GO TO 1060-SIP.                                      EL528
00654                                                                   EL528
00655      GO TO 1050-SIP.                                              EL528
00656                                                                   EL528
00657      EJECT                                                        EL528
00658                                                                   EL528
00659  1060-SIP.                                                        EL528
00660      MOVE SPACES                 TO  EXTRACT-INTERFACE-RECORD.    EL528
00661      MOVE 'B'                    TO  EX-EXTRACT-CODE.             EL528
00662      MOVE 'EX'                   TO  EX-RECORD-ID.                EL528
00663      MOVE '2'                    TO  EX-POSITIONING-CODE.         EL528
00664      MOVE 'A'                    TO  EX-RECORD-TYPE.              EL528
00665      MOVE CF-COMPANY-CD          TO  EX-COMPANY-CD.               EL528
00666      MOVE CF-COMPANY-ID          TO  EX-COMPANY-ID.               EL528
00667      MOVE PB-CARRIER             TO  EX-SE-CARRIER.               EL528
00668      MOVE PB-GROUPING            TO  EX-SE-GROUPING.              EL528
00669      MOVE PB-STATE               TO  EX-SE-STATE.                 EL528
00670      MOVE PB-ACCOUNT             TO  EX-SE-ACCOUNT.               EL528
00671      MOVE PB-ENTRY-BATCH         TO  EX-SE-BATCH-NO.              EL528
00672      MOVE PB-RECORD-TYPE         TO  EX-SE-RECORD-TYPE.           EL528
00673      MOVE PENDING-BUSINESS       TO  EX-DATA-AREAS.               EL528
00674                                                                   EL528
00675      RELEASE EXTRACT-INTERFACE-RECORD.                            EL528
00676      ADD +1                      TO WS-RECORD-COUNT.              EL528
00677                                                                   EL528
00678      GO TO 1050-SIP.                                              EL528
00679                                                                   EL528
00680  1099-CLOSE-PENDING-BUS-FILE.                                     EL528
00681      CLOSE ERPNDB.                                                EL528
00682                                                                   EL528
00683      IF ERPNDB-FILE-STATUS NOT = ZERO                             EL528
00684          MOVE 'ERROR OCCURED CLOSE - ERPNDB'                      EL528
00685                                  TO  WS-ABEND-MESSAGE             EL528
00686          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
00687          GO TO ABEND-PGM.                                         EL528
00688                                                                   EL528
00689      MOVE SPACES                 TO  WS-COMPANY-NAME              EL528
00690                                      WS-H2-CLIENT-NAME            EL528
00691                                      WS-COMPANY-ID                EL528
00692                                      WS-COMPANY-CD                EL528
00693                                      WS-LAST-CARRIER.             EL528
00694                                                                   EL528
00695  1099-EXIT.                                                       EL528
00696      EXIT.                                                        EL528
00697      EJECT                                                        EL528
00698                                                                   EL528
00699                                                                   EL528
00700  1900-PROCESS-EXTRACT SECTION.                                    EL528
00701      RETURN SORT-FILE                                             EL528
00702          AT END                                                   EL528
00703              MOVE +1 TO WS-EOF-SW                                 EL528
00704              PERFORM 3500-FINAL-TOTALS                            EL528
00705              GO TO 1999-EXIT.                                     EL528
00706                                                                   EL528
00707      IF EX-POSITIONING-CODE LESS THAN '2'                         EL528
00708          GO TO 1900-PROCESS-EXTRACT                               EL528
00709      ELSE                                                         EL528
00710          IF EX-POSITIONING-CODE GREATER THAN '2'                  EL528
00711              GO TO 1999-EXIT.                                     EL528
00712                                                                   EL528
00713      IF EX-EXTRACT-CODE  NOT = 'B'                                EL528
00714          GO TO 1999-EXIT.                                         EL528
00715                                                                   EL528
00716      IF EX-RECORD-TYPE NOT = 'A'                                  EL528
00717          GO TO 1900-PROCESS-EXTRACT.                              EL528
00718                                                                   EL528
00719      MOVE EX-DATA-AREAS          TO PENDING-BUSINESS.             EL528
00720                                                                   EL528
00721      IF EX-COMPANY-CD = WS-COMPANY-CD                             EL528
00722          NEXT SENTENCE                                            EL528
00723      ELSE                                                         EL528
00724          PERFORM 3500-FINAL-TOTALS                                EL528
00725          PERFORM 6000-GET-OPTIONS.                                EL528
00726                                                                   EL528
00727      IF WS-FREQUENCY-OPTION NOT = 'NONE' OR                       EL528
00728         WS-PROCESS-OPTION = 'X'                                   EL528
00729          GO TO 1900-PROCESS-EXTRACT.                              EL528
00730                                                                   EL528
00731      PERFORM 2000-PRINT.                                          EL528
00732                                                                   EL528
00733      GO TO 1900-PROCESS-EXTRACT.                                  EL528
00734                                                                   EL528
00735  1999-EXIT.                                                       EL528
00736      EXIT.                                                        EL528
00737      EJECT                                                        EL528
00738  2000-PRINT SECTION.                                              EL528
00739      IF PB-CARRIER  NOT = WS-SAVE-CARRIER  OR                     EL528
00740         PB-GROUPING NOT = WS-SAVE-GROUPING OR                     EL528
00741         PB-STATE    NOT = WS-SAVE-STATE    OR                     EL528
00742         PB-ACCOUNT  NOT = WS-SAVE-ACCOUNT                         EL528
00743          PERFORM 3000-ACCOUNT-BREAK.                              EL528
00744                                                                   EL528
00745      IF NOT PB-BATCH-TRAILER                                      EL528
00746          MOVE 'ERPNDB O/B EXTRACT SEQUENCE ERROR  '               EL528
00747                                  TO  WS-ABEND-MESSAGE             EL528
00748          GO TO ABEND-PGM.                                         EL528
00749                                                                   EL528
00750      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL528
00751          GO TO 2000-EXIT.                                         EL528
00752                                                                   EL528
00753      ADD +1                      TO  WS-NO-BATCHES.               EL528
00754                                                                   EL528
00755      MOVE PB-B-LF-ISS-PRM-ENTERED  TO WS-BT-ENTR-LF-PRM.          EL528
00756      MOVE PB-B-AH-ISS-PRM-ENTERED  TO WS-BT-ENTR-AH-PRM.          EL528
00757      MOVE PB-B-LF-CAN-PRM-ENTERED  TO WS-BT-ENTR-LF-REF.          EL528
00758      MOVE PB-B-AH-CAN-PRM-ENTERED  TO WS-BT-ENTR-AH-REF.          EL528
00759      MOVE PB-B-ISSUE-CNT-ENTERED   TO WS-BT-ENTR-ISS-CNT.         EL528
00760      MOVE PB-B-CANCEL-CNT-ENTERED  TO WS-BT-ENTR-CAN-CNT.         EL528
00761      MOVE PB-B-LF-ISS-PRM-REMITTED TO WS-BT-CNTL-LF-PRM.          EL528
00762      MOVE PB-B-AH-ISS-PRM-REMITTED TO WS-BT-CNTL-AH-PRM.          EL528
00763      MOVE PB-B-LF-CAN-PRM-REMITTED TO WS-BT-CNTL-LF-REF.          EL528
00764      MOVE PB-B-AH-CAN-PRM-REMITTED TO WS-BT-CNTL-AH-REF.          EL528
00765      MOVE PB-B-ISSUE-CNT-REMITTED  TO WS-BT-CNTL-ISS-CNT.         EL528
00766      MOVE PB-B-CANCEL-CNT-REMITTED TO WS-BT-CNTL-CAN-CNT.         EL528
00767                                                                   EL528
00768      SUBTRACT WS-BT-ENTR-LF-PRM  FROM  WS-BT-CNTL-LF-PRM          EL528
00769                                 GIVING WS-BT-DIFF-LF-PRM.         EL528
00770      SUBTRACT WS-BT-ENTR-LF-REF  FROM  WS-BT-CNTL-LF-REF          EL528
00771                                 GIVING WS-BT-DIFF-LF-REF.         EL528
00772      SUBTRACT WS-BT-ENTR-AH-PRM  FROM  WS-BT-CNTL-AH-PRM          EL528
00773                                 GIVING WS-BT-DIFF-AH-PRM.         EL528
00774      SUBTRACT WS-BT-ENTR-AH-REF  FROM  WS-BT-CNTL-AH-REF          EL528
00775                                 GIVING WS-BT-DIFF-AH-REF.         EL528
00776      SUBTRACT WS-BT-ENTR-ISS-CNT FROM  WS-BT-CNTL-ISS-CNT         EL528
00777                                 GIVING WS-BT-DIFF-ISS-CNT.        EL528
00778      SUBTRACT WS-BT-ENTR-CAN-CNT FROM  WS-BT-CNTL-CAN-CNT         EL528
00779                                 GIVING WS-BT-DIFF-CAN-CNT.        EL528
00780                                                                   EL528
00781      MOVE SPACES                 TO  WS-DETAIL2.                  EL528
00782      MOVE PB-ENTRY-BATCH         TO  WS-D2-BATCH-NO.              EL528
00783      MOVE WS-DETAIL2             TO  PRT.                         EL528
00784      MOVE ZERO                   TO  P-CTL.                       EL528
00785      PERFORM WRITE-A-LINE.                                        EL528
00786                                                                   EL528
00787      SET TOT-INDX  TO +1.                                         EL528
00788      SET TOT-INDX1 TO +4.                                         EL528
00789      SET TOT-INDX2 TO +7.                                         EL528
00790                                                                   EL528
00791  2010-PRINT-DETAIL.                                               EL528
00792      IF TOT-INDX GREATER TOT-INDX-MAX                             EL528
00793          SET TOT-INDX TO +1                                       EL528
00794          GO TO 2020-ZERO-TOTALS.                                  EL528
00795                                                                   EL528
00796      MOVE SPACES                 TO  WS-DETAIL1.                  EL528
00797                                                                   EL528
00798      MOVE WS-TOTAL-DESC (TOT-INDX)                                EL528
00799                                  TO  WS-D1-DESC.                  EL528
00800      MOVE WS-TOTAL-ISS-CNT (TOT-INDX)                             EL528
00801                                  TO  WS-D1-ISSUE-COUNT.           EL528
00802      MOVE WS-TOTAL-CAN-CNT (TOT-INDX)                             EL528
00803                                  TO  WS-D1-CANCEL-COUNT.          EL528
00804      MOVE WS-TOTAL-LF-PRM (TOT-INDX)                              EL528
00805                                  TO  WS-D1-LIFE-PREMIUM.          EL528
00806      MOVE WS-TOTAL-LF-REF(TOT-INDX)                               EL528
00807                                  TO  WS-D1-LIFE-REFUND.           EL528
00808      MOVE WS-TOTAL-AH-PRM (TOT-INDX)                              EL528
00809                                  TO  WS-D1-AH-PREMIUM.            EL528
00810      MOVE WS-TOTAL-AH-REF (TOT-INDX)                              EL528
00811                                  TO  WS-D1-AH-REFUND.             EL528
00812                                                                   EL528
00813      ADD WS-TOTAL-ISS-CNT (TOT-INDX)                              EL528
00814                                  TO  WS-TOTAL-ISS-CNT (TOT-INDX1) EL528
00815                                      WS-TOTAL-ISS-CNT (TOT-INDX2).EL528
00816      ADD WS-TOTAL-CAN-CNT (TOT-INDX)                              EL528
00817                                  TO  WS-TOTAL-CAN-CNT (TOT-INDX1) EL528
00818                                      WS-TOTAL-CAN-CNT (TOT-INDX2).EL528
00819      ADD WS-TOTAL-LF-PRM (TOT-INDX)                               EL528
00820                                  TO  WS-TOTAL-LF-PRM (TOT-INDX1)  EL528
00821                                      WS-TOTAL-LF-PRM (TOT-INDX2). EL528
00822      ADD WS-TOTAL-LF-REF (TOT-INDX)                               EL528
00823                                  TO  WS-TOTAL-LF-REF (TOT-INDX1)  EL528
00824                                      WS-TOTAL-LF-REF (TOT-INDX2). EL528
00825      ADD WS-TOTAL-AH-PRM (TOT-INDX)                               EL528
00826                                  TO  WS-TOTAL-AH-PRM (TOT-INDX1)  EL528
00827                                      WS-TOTAL-AH-PRM (TOT-INDX2). EL528
00828      ADD WS-TOTAL-AH-REF (TOT-INDX)                               EL528
00829                                  TO  WS-TOTAL-AH-REF (TOT-INDX1)  EL528
00830                                      WS-TOTAL-AH-REF (TOT-INDX2). EL528
00831                                                                   EL528
00832      MOVE WS-DETAIL1             TO  PRT.                         EL528
00833                                                                   EL528
00834      IF TOT-INDX = +1                                             EL528
00835          MOVE ZERO               TO  P-CTL                        EL528
00836      ELSE                                                         EL528
00837          MOVE 1              TO  P-CTL.                           EL528
00838                                                                   EL528
00839      PERFORM WRITE-A-LINE.                                        EL528
00840                                                                   EL528
00841      SET TOT-INDX  UP BY +1.                                      EL528
00842      SET TOT-INDX1 UP BY +1.                                      EL528
00843      SET TOT-INDX2 UP BY +1.                                      EL528
00844                                                                   EL528
00845      GO TO 2010-PRINT-DETAIL.                                     EL528
00846                                                                   EL528
00847  2020-ZERO-TOTALS.                                                EL528
00848      MOVE ZEROS                  TO WS-TOTAL-ISS-CNT (TOT-INDX)   EL528
00849                                     WS-TOTAL-CAN-CNT (TOT-INDX)   EL528
00850                                     WS-TOTAL-LF-PRM (TOT-INDX)    EL528
00851                                     WS-TOTAL-LF-REF (TOT-INDX)    EL528
00852                                     WS-TOTAL-AH-PRM (TOT-INDX)    EL528
00853                                     WS-TOTAL-AH-REF (TOT-INDX).   EL528
00854      SET TOT-INDX UP BY +1.                                       EL528
00855                                                                   EL528
00856      IF TOT-INDX GREATER TOT-INDX-MAX                             EL528
00857          SET TOT-INDX TO +1                                       EL528
00858      ELSE                                                         EL528
00859          GO TO 2020-ZERO-TOTALS.                                  EL528
00860                                                                   EL528
00861  2000-EXIT.                                                       EL528
00862      EXIT.                                                        EL528
00863      EJECT                                                        EL528
00864                                                                   EL528
00865  3000-ACCOUNT-BREAK SECTION.                                      EL528
00866      IF ONLY-ONE-BATCH OR                                         EL528
00867         NEW-ACCOUNT                                               EL528
00868          SET TOT-INDX1 TO +4                                      EL528
00869          GO TO 3020-ZERO-TOTALS.                                  EL528
00870                                                                   EL528
00871      MOVE WS-HEADING8            TO  PRT.                         EL528
00872      MOVE ZERO                   TO  P-CTL.                       EL528
00873      PERFORM WRITE-A-LINE.                                        EL528
00874                                                                   EL528
00875      SUBTRACT WS-AT-ENTR-LF-PRM  FROM  WS-AT-CNTL-LF-PRM          EL528
00876                                 GIVING WS-AT-DIFF-LF-PRM.         EL528
00877      SUBTRACT WS-AT-ENTR-LF-REF  FROM  WS-AT-CNTL-LF-REF          EL528
00878                                 GIVING WS-AT-DIFF-LF-REF.         EL528
00879      SUBTRACT WS-AT-ENTR-AH-PRM  FROM  WS-AT-CNTL-AH-PRM          EL528
00880                                 GIVING WS-AT-DIFF-AH-PRM.         EL528
00881      SUBTRACT WS-AT-ENTR-AH-REF  FROM  WS-AT-CNTL-AH-REF          EL528
00882                                 GIVING WS-AT-DIFF-AH-REF.         EL528
00883      SUBTRACT WS-AT-ENTR-ISS-CNT FROM  WS-AT-CNTL-ISS-CNT         EL528
00884                                 GIVING WS-AT-DIFF-ISS-CNT.        EL528
00885      SUBTRACT WS-AT-ENTR-CAN-CNT FROM  WS-AT-CNTL-CAN-CNT         EL528
00886                                 GIVING WS-AT-DIFF-CAN-CNT.        EL528
00887      SET TOT-INDX1 TO +4.                                         EL528
00888                                                                   EL528
00889  3010-CONT.                                                       EL528
00890      IF TOT-INDX1 GREATER TOT-INDX1-MAX                           EL528
00891          SET TOT-INDX1 TO +4                                      EL528
00892          GO TO 3020-ZERO-TOTALS.                                  EL528
00893                                                                   EL528
00894      MOVE SPACES                 TO  WS-DETAIL1.                  EL528
00895                                                                   EL528
00896      MOVE WS-TOTAL-DESC (TOT-INDX1)                               EL528
00897                                  TO  WS-D1-DESC.                  EL528
00898      MOVE WS-TOTAL-ISS-CNT (TOT-INDX1)                            EL528
00899                                  TO  WS-D1-ISSUE-COUNT.           EL528
00900      MOVE WS-TOTAL-CAN-CNT (TOT-INDX1)                            EL528
00901                                  TO  WS-D1-CANCEL-COUNT.          EL528
00902      MOVE WS-TOTAL-LF-PRM (TOT-INDX1)                             EL528
00903                                  TO  WS-D1-LIFE-PREMIUM.          EL528
00904      MOVE WS-TOTAL-LF-REF(TOT-INDX1)                              EL528
00905                                  TO  WS-D1-LIFE-REFUND.           EL528
00906      MOVE WS-TOTAL-AH-PRM (TOT-INDX1)                             EL528
00907                                  TO  WS-D1-AH-PREMIUM.            EL528
00908      MOVE WS-TOTAL-AH-REF (TOT-INDX1)                             EL528
00909                                  TO  WS-D1-AH-REFUND.             EL528
00910                                                                   EL528
00911      MOVE WS-DETAIL1             TO  PRT.                         EL528
00912      PERFORM WRITE-A-LINE.                                        EL528
00913                                                                   EL528
00914      SET TOT-INDX1 UP BY +1.                                      EL528
00915                                                                   EL528
00916      GO TO 3010-CONT.                                             EL528
00917                                                                   EL528
00918  3020-ZERO-TOTALS.                                                EL528
00919      MOVE ZEROS                  TO WS-TOTAL-ISS-CNT (TOT-INDX1)  EL528
00920                                     WS-TOTAL-CAN-CNT (TOT-INDX1)  EL528
00921                                     WS-TOTAL-LF-PRM (TOT-INDX1)   EL528
00922                                     WS-TOTAL-LF-REF (TOT-INDX1)   EL528
00923                                     WS-TOTAL-AH-PRM (TOT-INDX1)   EL528
00924                                     WS-TOTAL-AH-REF (TOT-INDX1).  EL528
00925      SET TOT-INDX1 UP BY +1.                                      EL528
00926                                                                   EL528
00927      IF TOT-INDX1 GREATER TOT-INDX1-MAX                           EL528
00928          SET TOT-INDX1 TO +4                                      EL528
00929      ELSE                                                         EL528
00930          GO TO 3020-ZERO-TOTALS.                                  EL528
00931                                                                   EL528
00932      IF EXTRACT-EOF                                               EL528
00933          GO TO 3000-EXIT.                                         EL528
00934                                                                   EL528
00935      PERFORM 4000-PROCESS-ACCOUNT.                                EL528
00936                                                                   EL528
00937      ADD WS-LINE-COUNT-MAX       TO WS-LINE-COUNT.                EL528
00938      MOVE +0                     TO WS-NO-BATCHES.                EL528
00939      MOVE PB-CONTROL-BY-ACCOUNT  TO WS-SAVE-KEY.                  EL528
00940                                                                   EL528
00941  3000-EXIT.                                                       EL528
00942      EXIT.                                                        EL528
00943      EJECT                                                        EL528
00944                                                                   EL528
00945  3500-FINAL-TOTALS SECTION.                                       EL528
00946      IF FIRST-TIME                                                EL528
00947           SET TOT-INDX2          TO +1                            EL528
00948           MOVE +1                TO WS-FIRST-TIME-SW              EL528
00949           GO TO 3520-ZERO-TOTALS.                                 EL528
00950                                                                   EL528
00951      IF WS-FREQUENCY-OPTION NOT = 'NONE' OR                       EL528
00952         WS-PROCESS-OPTION = 'X'                                   EL528
00953          GO TO 3500-EXIT.                                         EL528
00954                                                                   EL528
00955      PERFORM 3000-ACCOUNT-BREAK.                                  EL528
00956                                                                   EL528
00957      MOVE WS-HEADING5            TO  WS-SAVE-HEADING5.            EL528
00958      MOVE SPACES                 TO  WS-HEADING5.                 EL528
00959      ADD WS-LINE-COUNT-MAX       TO  WS-LINE-COUNT.               EL528
00960      MOVE WS-HEADING9            TO  PRT.                         EL528
00961      MOVE ZERO                   TO  P-CTL.                       EL528
00962      PERFORM WRITE-A-LINE.                                        EL528
00963      MOVE WS-SAVE-HEADING5       TO  WS-HEADING5.                 EL528
00964                                                                   EL528
00965      SUBTRACT WS-FT-ENTR-LF-PRM  FROM  WS-FT-CNTL-LF-PRM          EL528
00966                                 GIVING WS-FT-DIFF-LF-PRM.         EL528
00967      SUBTRACT WS-FT-ENTR-LF-REF  FROM  WS-FT-CNTL-LF-REF          EL528
00968                                 GIVING WS-FT-DIFF-LF-REF.         EL528
00969      SUBTRACT WS-FT-ENTR-AH-PRM  FROM  WS-FT-CNTL-AH-PRM          EL528
00970                                 GIVING WS-FT-DIFF-AH-PRM.         EL528
00971      SUBTRACT WS-FT-ENTR-AH-REF  FROM  WS-FT-CNTL-AH-REF          EL528
00972                                 GIVING WS-FT-DIFF-AH-REF.         EL528
00973      SUBTRACT WS-FT-ENTR-ISS-CNT FROM  WS-FT-CNTL-ISS-CNT         EL528
00974                                 GIVING WS-FT-DIFF-ISS-CNT.        EL528
00975      SUBTRACT WS-FT-ENTR-CAN-CNT FROM  WS-FT-CNTL-CAN-CNT         EL528
00976                                 GIVING WS-FT-DIFF-CAN-CNT.        EL528
00977      SET TOT-INDX2 TO +7.                                         EL528
00978                                                                   EL528
00979  3510-CONT.                                                       EL528
00980      IF TOT-INDX2 GREATER TOT-INDX2-MAX                           EL528
00981          SET TOT-INDX2 TO +1                                      EL528
00982          GO TO 3520-ZERO-TOTALS.                                  EL528
00983                                                                   EL528
00984      MOVE SPACES                 TO  WS-DETAIL1.                  EL528
00985                                                                   EL528
00986      MOVE WS-TOTAL-DESC (TOT-INDX2)                               EL528
00987                                  TO  WS-D1-DESC.                  EL528
00988      MOVE WS-TOTAL-ISS-CNT (TOT-INDX2)                            EL528
00989                                  TO  WS-D1-ISSUE-COUNT.           EL528
00990      MOVE WS-TOTAL-CAN-CNT (TOT-INDX2)                            EL528
00991                                  TO  WS-D1-CANCEL-COUNT.          EL528
00992      MOVE WS-TOTAL-LF-PRM (TOT-INDX2)                             EL528
00993                                  TO  WS-D1-LIFE-PREMIUM.          EL528
00994      MOVE WS-TOTAL-LF-REF(TOT-INDX2)                              EL528
00995                                  TO  WS-D1-LIFE-REFUND.           EL528
00996      MOVE WS-TOTAL-AH-PRM (TOT-INDX2)                             EL528
00997                                  TO  WS-D1-AH-PREMIUM.            EL528
00998      MOVE WS-TOTAL-AH-REF (TOT-INDX2)                             EL528
00999                                  TO  WS-D1-AH-REFUND.             EL528
01000                                                                   EL528
01001      MOVE WS-DETAIL1             TO  PRT.                         EL528
01002      PERFORM WRITE-A-LINE.                                        EL528
01003                                                                   EL528
01004      SET TOT-INDX2 UP BY +1.                                      EL528
01005                                                                   EL528
01006      GO TO 3510-CONT.                                             EL528
01007                                                                   EL528
01008  3515-CONT.                                                       EL528
01009      PERFORM 5000-BUILD-TRAILER.                                  EL528
01010                                                                   EL528
01011  3520-ZERO-TOTALS.                                                EL528
01012      MOVE ZEROS                  TO WS-TOTAL-ISS-CNT (TOT-INDX2)  EL528
01013                                     WS-TOTAL-CAN-CNT (TOT-INDX2)  EL528
01014                                     WS-TOTAL-LF-PRM (TOT-INDX2)   EL528
01015                                     WS-TOTAL-LF-REF (TOT-INDX2)   EL528
01016                                     WS-TOTAL-AH-PRM (TOT-INDX2)   EL528
01017                                     WS-TOTAL-AH-REF (TOT-INDX2).  EL528
01018      SET TOT-INDX2 UP BY +1.                                      EL528
01019                                                                   EL528
01020      IF TOT-INDX2 GREATER TOT-INDX2-MAX                           EL528
01021          ADD WS-LINE-COUNT-MAX   TO WS-LINE-COUNT                 EL528
01022          SET TOT-INDX2 TO +7                                      EL528
01023      ELSE                                                         EL528
01024          GO TO 3520-ZERO-TOTALS.                                  EL528
01025                                                                   EL528
01026                                                                   EL528
01027  3500-EXIT.                                                       EL528
01028      EXIT.                                                        EL528
01029      EJECT                                                        EL528
01030                                                                   EL528
01031  4000-PROCESS-ACCOUNT SECTION.                                    EL528
01032      MOVE SPACES                 TO  AM-CONTROL-BY-VAR-GRP.       EL528
01033      MOVE CF-COMPANY-CD          TO  AM-COMPANY-CD-A1.            EL528
01034      MOVE PB-ACCOUNT             TO  AM-VG-ACCOUNT.               EL528
01035                                                                   EL528
01036      IF CF-ST-ACCNT-CNTL                                          EL528
01037          MOVE PB-STATE           TO  AM-VG-STATE.                 EL528
01038                                                                   EL528
01039      IF CF-CARR-ACCNT-CNTL                                        EL528
01040          MOVE PB-CARRIER         TO  AM-VG-CARRIER.               EL528
01041                                                                   EL528
01042      IF CF-CARR-ST-ACCNT-CNTL                                     EL528
01043          MOVE PB-CARRIER         TO  AM-VG-CARRIER                EL528
01044          MOVE PB-STATE           TO  AM-VG-STATE.                 EL528
01045                                                                   EL528
01046      IF CF-CARR-GROUP-ST-ACCNT-CNTL                               EL528
01047          MOVE PB-CARRIER         TO  AM-VG-CARRIER                EL528
01048          MOVE PB-GROUPING        TO  AM-VG-GROUPING               EL528
01049          MOVE PB-STATE           TO  AM-VG-STATE.                 EL528
01050                                                                   EL528
01051      START ERACCT                                                 EL528
01052          KEY IS GREATER THAN AM-CONTROL-BY-VAR-GRP.               EL528
01053                                                                   EL528
01054      IF ERACCT-FILE-STATUS NOT = ZERO                             EL528
01055          GO TO 4010-INVALID-ACCOUNT.                              EL528
01056                                                                   EL528
01057      READ ERACCT NEXT RECORD.                                     EL528
01058                                                                   EL528
01059      IF ERACCT-FILE-STATUS NOT = ZERO                             EL528
01060          DISPLAY '*** EL528 ERACCT INVALID READ NEXT'             EL528
01061              UPON CONSOLE                                         EL528
01062          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01063          GO TO ABEND-PGM.                                         EL528
01064                                                                   EL528
01065      IF CF-ACCNT-CNTL                                             EL528
01066          IF PB-ACCOUNT = AM-VG-ACCOUNT                            EL528
01067              GO TO 4020-VALID-ACCOUNT                             EL528
01068          ELSE                                                     EL528
01069              GO TO 4010-INVALID-ACCOUNT.                          EL528
01070                                                                   EL528
01071      IF CF-ST-ACCNT-CNTL                                          EL528
01072          IF PB-ACCOUNT = AM-VG-ACCOUNT AND                        EL528
01073             PB-STATE   = AM-VG-STATE                              EL528
01074              GO TO 4020-VALID-ACCOUNT                             EL528
01075          ELSE                                                     EL528
01076              GO TO 4010-INVALID-ACCOUNT.                          EL528
01077                                                                   EL528
01078      IF CF-CARR-ACCNT-CNTL                                        EL528
01079          IF PB-ACCOUNT = AM-VG-ACCOUNT AND                        EL528
01080             PB-CARRIER = AM-VG-CARRIER                            EL528
01081              GO TO 4020-VALID-ACCOUNT                             EL528
01082          ELSE                                                     EL528
01083              GO TO 4010-INVALID-ACCOUNT.                          EL528
01084                                                                   EL528
01085      IF CF-CARR-ST-ACCNT-CNTL                                     EL528
01086          IF PB-ACCOUNT  = AM-VG-ACCOUNT AND                       EL528
01087             PB-STATE    = AM-VG-STATE   AND                       EL528
01088             PB-CARRIER  = AM-VG-CARRIER                           EL528
01089              GO TO 4020-VALID-ACCOUNT                             EL528
01090          ELSE                                                     EL528
01091              GO TO 4010-INVALID-ACCOUNT.                          EL528
01092                                                                   EL528
01093      IF CF-CARR-GROUP-ST-ACCNT-CNTL                               EL528
01094          IF PB-ACCOUNT   = AM-VG-ACCOUNT AND                      EL528
01095             PB-STATE     = AM-VG-STATE   AND                      EL528
01096             PB-CARRIER   = AM-VG-CARRIER AND                      EL528
01097             PB-GROUPING  = AM-VG-GROUPING                         EL528
01098              GO TO 4020-VALID-ACCOUNT.                            EL528
01099                                                                   EL528
01100  4010-INVALID-ACCOUNT.                                            EL528
01101      MOVE '**INVALID ACCOUNT**'  TO  H5-ACCOUNT-NAME              EL528
01102      MOVE PB-CARRIER             TO  H5-CARRIER.                  EL528
01103      MOVE PB-GROUPING            TO  H5-GROUPING.                 EL528
01104      MOVE PB-STATE               TO  H5-STATE.                    EL528
01105      MOVE PB-ACCOUNT             TO  H5-ACCOUNT.                  EL528
01106      GO TO 4000-EXIT.                                             EL528
01107                                                                   EL528
01108  4020-VALID-ACCOUNT.                                              EL528
01109      MOVE AM-NAME                TO  H5-ACCOUNT-NAME.             EL528
01110      MOVE AM-CARRIER             TO  H5-CARRIER.                  EL528
01111      MOVE AM-GROUPING            TO  H5-GROUPING.                 EL528
01112      MOVE AM-STATE               TO  H5-STATE.                    EL528
01113      MOVE AM-ACCOUNT             TO  H5-ACCOUNT.                  EL528
01114                                                                   EL528
01115  4000-EXIT.                                                       EL528
01116      EXIT.                                                        EL528
01117      EJECT                                                        EL528
01118  5000-BUILD-TRAILER.                                              EL528
01119      IF WS-PRINT-OPTION = ('S' OR 'T')                            EL528
01120          AND WS-LINE-NUMBER GREATER THAN ZERO                     EL528
01121              NEXT SENTENCE                                        EL528
01122          ELSE                                                     EL528
01123              GO TO 5000-EXIT.                                     EL528
01124                                                                   EL528
01125      MOVE WS-COMPANY-CD          TO  RF-COMPANY-CD.               EL528
01126      MOVE '2'                    TO  RF-RECORD-TYPE.              EL528
01127      MOVE 'EL528'                TO  RF-RECORD-ID.                EL528
01128      ADD +1                      TO  WS-LINE-NUMBER.              EL528
01129      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              EL528
01130      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL528
01131      MOVE WS-TIME                TO  RF-PRINT-HH-MM-SS.           EL528
01132      ACCEPT  LCP-DATE-NEW-74 FROM DATE                            EL528
01133      MOVE CORRESPONDING LCP-DATE-NEW-74 TO LCP-CURRENT-DATE-68    EL528
01134      MOVE  LCP-CURRENT-DATE-68 TO RF-CURRENT-DATE.                EL528
01135                                                                   EL528
01136      WRITE REPORT-SAVE-FILE.                                      EL528
01137                                                                   EL528
01138      IF ELREPT-FILE-STATUS NOT = ZERO                             EL528
01139          MOVE 'ERROR OCCURED WRITE ELREPT TRAILER RECORD'         EL528
01140                                  TO WS-ABEND-MESSAGE              EL528
01141          MOVE ELREPT-FILE-STATUS                                  EL528
01142                                  TO WS-ABEND-FILE-STATUS.         EL528
01143                                                                   EL528
01144  5000-EXIT.                                                       EL528
01145      EXIT.                                                        EL528
01146      EJECT                                                        EL528
01147                                                                   EL528
01148  6000-GET-OPTIONS SECTION.                                        EL528
01149 *                                                                 EL528
01150 *                                                                 EL528
01151 * LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINE ABOVE      EL528
01156                                                                   EL528
01157      IF FIRST-TIME-OPTION                                         EL528
01158          MOVE +1                 TO WS-FIRST-TIME-OPTION          EL528
01159          GO TO 6010-PROCESS.                                      EL528
01160                                                                   EL528
01161      IF WS-PRINT-OPTION = ('S' OR 'T')                            EL528
01162          AND WS-LINE-NUMBER GREATER THAN ZERO                     EL528
01163              NEXT SENTENCE                                        EL528
01164          ELSE                                                     EL528
01165              GO TO 6010-PROCESS.                                  EL528
01166                                                                   EL528
01167      MOVE WS-COMPANY-CD          TO  RF-COMPANY-CD.               EL528
01168      MOVE '2'                    TO  RF-RECORD-TYPE.              EL528
01169      MOVE 'EL528'                TO  RF-RECORD-ID.                EL528
01170      ADD +1                      TO  WS-LINE-NUMBER.              EL528
01171      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              EL528
01172      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL528
01173      MOVE WS-TIME                TO  RF-PRINT-HH-MM-SS.           EL528
01174      ACCEPT  LCP-DATE-NEW-74 FROM DATE                            EL528
01175      MOVE CORRESPONDING LCP-DATE-NEW-74 TO LCP-CURRENT-DATE-68    EL528
01176      MOVE  LCP-CURRENT-DATE-68 TO RF-CURRENT-DATE.                EL528
01177                                                                   EL528
01178      WRITE REPORT-SAVE-FILE.                                      EL528
01179                                                                   EL528
01180      IF ELREPT-FILE-STATUS NOT = ZERO                             EL528
01181          MOVE 'ERROR OCCURED WRITE ELREPT TRAILER RECORD'         EL528
01182                                  TO WS-ABEND-MESSAGE              EL528
01183          MOVE ELREPT-FILE-STATUS                                  EL528
01184                                  TO WS-ABEND-FILE-STATUS          EL528
01185          GO TO ABEND-PGM.                                         EL528
01186                                                                   EL528
01187  6010-PROCESS.                                                    EL528
01188      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          EL528
01189      MOVE EX-COMPANY-ID          TO  CF-COMPANY-ID.               EL528
01190      MOVE '1'                    TO  CF-RECORD-TYPE.              EL528
01191      MOVE SPACES                 TO  CF-ACCESS-CD-GENL.           EL528
01192      MOVE +0                     TO  CF-SEQUENCE-NO.              EL528
01193                                                                   EL528
01194      READ ELCNTL.                                                 EL528
01195                                                                   EL528
01196      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL528
01197          MOVE 'ERROR OCCURED READ INITIAL - ELCNTL'               EL528
01198                                  TO  WS-ABEND-MESSAGE             EL528
01199          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01200          GO TO ABEND-PGM.                                         EL528
01201                                                                   EL528
01202      IF NOT CF-COMPANY-MASTER                                     EL528
01203          MOVE 'COMPANY MASTER RECORD MISSING - ELCNTL'            EL528
01204                                  TO  WS-ABEND-MESSAGE             EL528
01205          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01206          GO TO ABEND-PGM.                                         EL528
01207                                                                   EL528
01208      MOVE CF-CL-MAIL-TO-NAME     TO  WS-COMPANY-NAME              EL528
01209                                      WS-H2-CLIENT-NAME.           EL528
01210      MOVE CF-COMPANY-ID          TO  WS-COMPANY-ID.               EL528
01211      MOVE CF-COMPANY-CD          TO  WS-COMPANY-CD.               EL528
01212      MOVE LOW-VALUES             TO  WS-LAST-CARRIER.             EL528
01213      MOVE CF-LIFE-OVERRIDE-L6    TO  HDG6-OVRD-1.                 EL528
01214      MOVE CF-LIFE-OVERRIDE-L6    TO  HDG6-OVRD-2.                 EL528
01215      MOVE CF-AH-OVERRIDE-L6      TO  HDG6-OVRD-3.                 EL528
01216      MOVE CF-AH-OVERRIDE-L6      TO  HDG6-OVRD-4.                 EL528
01217                                                                   EL528
01218      MOVE CF-CR-MONTH-END-DT     TO  DC-BIN-DATE-1.               EL528
01219      MOVE ' '                    TO  DC-OPTION-CODE.              EL528
01220      PERFORM 8500-DATE-CONVERSION.                                EL528
01221      MOVE DC-GREG-DATE-1-ALPHA   TO  ALPH-DATE.                   EL528
01222                                                                   EL528
01223      ACCEPT WS-TIME-OF-DAY       FROM TIME.                       EL528
01224      MOVE WS-TIME                TO  WS-DISPLAY-TIME.             EL528
01225      INSPECT WS-DISPLAY-TIME CONVERTING SPACES TO '.'.            EL528
01226      DISPLAY 'BEGIN PROCESSING ' WS-H2-CLIENT-NAME ' AT '         EL528
01227              WS-DISPLAY-TIME UPON CONSOLE.                        EL528
01228                                                                   EL528
01229      MOVE SPACES                 TO  WS-COMPANY-NAME2.            EL528
01230      SET CN1 TO +30.                                              EL528
01231                                                                   EL528
01232  6020-PROCESS.                                                    EL528
01233      IF WS-CN-CHAR (CN1) = SPACES                                 EL528
01234          IF CN1 GREATER THAN +1                                   EL528
01235              SET CN1 DOWN BY +1                                   EL528
01236              GO TO 6020-PROCESS                                   EL528
01237          ELSE                                                     EL528
01238              GO TO 6040-PROCESS.                                  EL528
01239                                                                   EL528
01240      SET WS-LENGTH TO CN1.                                        EL528
01241                                                                   EL528
01242      SUBTRACT WS-LENGTH FROM +30 GIVING WS-LENGTH.                EL528
01243      DIVIDE +2 INTO WS-LENGTH ROUNDED.                            EL528
01244                                                                   EL528
01245      IF WS-LENGTH NOT GREATER THAN ZERO                           EL528
01246          GO TO 6040-PROCESS.                                      EL528
01247                                                                   EL528
01248      SET CN2 TO CN1.                                              EL528
01249      SET CN2 UP BY WS-LENGTH.                                     EL528
01250                                                                   EL528
01251  6030-PROCESS.                                                    EL528
01252      MOVE WS-CN-CHAR (CN1) TO WS-CN2-CHAR (CN2).                  EL528
01253                                                                   EL528
01254      IF CN1 GREATER THAN +1                                       EL528
01255          SET CN1                                                  EL528
01256              CN2 DOWN BY +1                                       EL528
01257          GO TO 6030-PROCESS.                                      EL528
01258                                                                   EL528
01259      MOVE WS-COMPANY-NAME2       TO  WS-COMPANY-NAME.             EL528
01260                                                                   EL528
01261  6040-PROCESS.                                                    EL528
01262      MOVE WS-COMPANY-CD          TO  PS-COMPANY-CD.               EL528
01263      MOVE WS-REPORT-ID           TO  PS-PROGRAM-NUMBER.           EL528
01264                                                                   EL528
01265      READ ELPGMS.                                                 EL528
01266                                                                   EL528
01267      IF ELPGMS-FILE-STATUS = '23'                                 EL528
01268          MOVE SPACES             TO  WS-PROGRAM-OPTIONS           EL528
01269          GO TO 6099-EXIT.                                         EL528
01270                                                                   EL528
01271      IF ELPGMS-FILE-STATUS NOT = ZERO                             EL528
01272          MOVE 'ERROR OCCURED READ - ELPGMS'                       EL528
01273                                  TO  WS-ABEND-MESSAGE             EL528
01274          MOVE ELPGMS-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01275          GO TO ABEND-PGM.                                         EL528
01276                                                                   EL528
01277      MOVE PS-PROGRAM-OPTIONS (1) TO  WS-PROGRAM-OPTIONS.          EL528
01278                                                                   EL528
01279      IF WS-FREQUENCY-OPTION = 'NONE'                              EL528
01280          NEXT SENTENCE                                            EL528
01281      ELSE                                                         EL528
01282          GO TO 6099-EXIT.                                         EL528
01283                                                                   EL528
01284      IF WS-PROCESS-OPTION = 'X'                                   EL528
01285          GO TO 6099-EXIT.                                         EL528
01286                                                                   EL528
01287  6050-PROCESS.                                                    EL528
01288      IF PRNTR-NOT-OPEN                                            EL528
01289          IF WS-PRINT-OPTION = ('P' OR 'B' OR 'T')                 EL528
01290              OPEN OUTPUT PRNTR                                    EL528
01291              MOVE +1             TO  WS-PRNTR-STATUS-SW           EL528
01292              MOVE +0             TO  WS-PAGE                      EL528
01293          ELSE                                                     EL528
01294              NEXT SENTENCE                                        EL528
01295      ELSE                                                         EL528
01296          CLOSE PRNTR                                              EL528
01297          MOVE +0                 TO  WS-PRNTR-STATUS-SW           EL528
01298          GO TO 6050-PROCESS.                                      EL528
01299                                                                   EL528
01300      IF WS-PRINT-OPTION = ('F' OR 'B')                            EL528
01301        AND FICHE-NOT-OPEN                                         EL528
01302          OPEN OUTPUT FICH                                         EL528
01303          MOVE +0                 TO  WS-LINE-COUNT                EL528
01304                                      WS-PAGE                      EL528
01305          MOVE +1                 TO  WS-FICHE-STATUS-SW.          EL528
01306                                                                   EL528
01307      IF WS-PRINT-OPTION = ('S' OR 'T')                            EL528
01308          NEXT SENTENCE                                            EL528
01309      ELSE                                                         EL528
01310          GO TO 6099-EXIT.                                         EL528
01311                                                                   EL528
01312      MOVE ZERO                   TO  WS-LINE-NUMBER.              EL528
01313                                                                   EL528
01314      IF ELREPT-NOT-OPEN                                           EL528
01315          OPEN I-O ELREPT                                          EL528
01316          MOVE +1                 TO  WS-ELREPT-STATUS-SW          EL528
01317          IF ELREPT-FILE-STATUS  = '00' OR '97'                    EL528
01318             NEXT SENTENCE                                         EL528
01319             ELSE                                                  EL528
01320              MOVE 'ELREPT  '     TO  WS-FEM-FILE-NAME             EL528
01321              MOVE WS-FILE-ERROR-MESSAGE  TO  WS-ABEND-MESSAGE     EL528
01322              MOVE ELREPT-FILE-STATUS  TO  WS-ABEND-FILE-STATUS    EL528
01323              GO TO ABEND-PGM.                                     EL528
01324                                                                   EL528
01325      MOVE ZERO                   TO  WS-START-SW.                 EL528
01326      MOVE WS-COMPANY-CD          TO  RF-COMPANY-CD.               EL528
01327      MOVE '1'                    TO  RF-RECORD-TYPE.              EL528
01328      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL528
01329      MOVE ZERO                   TO  RF-LINE-NUMBER.              EL528
01330                                                                   EL528
01331  6060-PRINT-REPORT.                                               EL528
01332      START ELREPT                                                 EL528
01333          KEY IS NOT LESS THAN RF-CONTROL-PRIMARY.                 EL528
01334                                                                   EL528
01335      IF ELREPT-FILE-STATUS = '10' OR '23'                         EL528
01336          GO TO 6090-PRINT-REPORT.                                 EL528
01337                                                                   EL528
01338      IF ELREPT-FILE-STATUS NOT = ZERO                             EL528
01339          MOVE 'ERROR OCCURED START ELREPT'  TO  WS-ABEND-MESSAGE  EL528
01340          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01341          GO TO ABEND-PGM.                                         EL528
01342                                                                   EL528
01343  6070-PRINT-REPORT.                                               EL528
01344      READ ELREPT NEXT.                                            EL528
01345                                                                   EL528
01346      IF ELREPT-FILE-STATUS = '10'                                 EL528
01347          GO TO 6090-PRINT-REPORT.                                 EL528
01348                                                                   EL528
01349      IF ELREPT-FILE-STATUS NOT = ZERO                             EL528
01350          MOVE 'ERROR OCCURED READNEXT ELREPT' TO WS-ABEND-MESSAGE EL528
01351          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01352          GO TO ABEND-PGM.                                         EL528
01353                                                                   EL528
01354      IF RF-COMPANY-CD NOT = WS-COMPANY-CD                         EL528
01355          GO TO 6090-PRINT-REPORT.                                 EL528
01356                                                                   EL528
01357      IF WS-START-SW = ZERO                                        EL528
01358          IF RF-RECORD-TYPE = '1'                                  EL528
01359              NEXT SENTENCE                                        EL528
01360          ELSE                                                     EL528
01361              GO TO 6090-PRINT-REPORT                              EL528
01362      ELSE                                                         EL528
01363          IF RF-RECORD-TYPE = '2'                                  EL528
01364              NEXT SENTENCE                                        EL528
01365          ELSE                                                     EL528
01366              GO TO 6090-PRINT-REPORT.                             EL528
01367                                                                   EL528
01368      IF RF-REPORT-ID NOT = WS-REPORT-ID                           EL528
01369          GO TO 6090-PRINT-REPORT.                                 EL528
01370                                                                   EL528
01371      DELETE ELREPT RECORD.                                        EL528
01372                                                                   EL528
01373      IF ELREPT-FILE-STATUS NOT = ZERO                             EL528
01374          MOVE 'ERROR OCCURED DELETE ELREPT' TO WS-ABEND-MESSAGE   EL528
01375          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01376          GO TO ABEND-PGM.                                         EL528
01377                                                                   EL528
01378      GO TO 6070-PRINT-REPORT.                                     EL528
01379                                                                   EL528
01380  6090-PRINT-REPORT.                                               EL528
01381      IF WS-START-SW = ZERO                                        EL528
01382          MOVE WS-COMPANY-CD      TO  RF-COMPANY-CD                EL528
01383          MOVE '2'                TO  RF-RECORD-TYPE               EL528
01384          MOVE WS-REPORT-ID       TO  RF-REPORT-ID                 EL528
01385          MOVE +0                 TO  RF-LINE-NUMBER               EL528
01386          MOVE +1                 TO  WS-START-SW                  EL528
01387          GO TO 6060-PRINT-REPORT.                                 EL528
01388                                                                   EL528
01389      MOVE WS-COMPANY-CD          TO  RF-COMPANY-CD.               EL528
01390      MOVE '1'                    TO  RF-RECORD-TYPE.              EL528
01391      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL528
01392      MOVE +0                     TO  RF-LINE-NUMBER.              EL528
01393                                                                   EL528
01394      MOVE SPACES                 TO  RF-REPORT-LINE-133.          EL528
01395                                                                   EL528
01396  6099-EXIT.                                                       EL528
01397      EXIT.                                                        EL528
01398      EJECT                                                        EL528
01399                                                                   EL528
01400  8500-DATE-CONVERSION SECTION.   COPY ELCDCS.                     EL528
01401                                                                   EL528
01402  WRITE-A-LINE SECTION.           COPY ELCWAL.                     EL528
01403                                                                   EL528
01404  WRITE-HEADINGS SECTION.                                          EL528
01405 ***************************************************************** EL528
01406 *                            ELCWHS1.                           * EL528
01407 *                            VMOD=2.001                         * EL528
01408 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL528
01409 *****************************************************************.EL528
01410  WHS-010.                                                         EL528
01411      IF  WS-H2-DATE EQUAL SPACES                                  EL528
01412          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL528
01413          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL528
01414          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL528
01415                                                                   EL528
01416      ADD +1  TO  WS-PAGE.                                         EL528
01417      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL528
01418      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL528
01419      MOVE ZERO                   TO  WS-LINE-COUNT.               EL528
01420                                                                   EL528
01421      MOVE WS-HEADING1            TO  PRT.                         EL528
01422      MOVE '1'                    TO  X.                           EL528
01423      PERFORM WRITE-PRINTER.                                       EL528
01424                                                                   EL528
01425      MOVE WS-HEADING2            TO  PRT.                         EL528
01426      MOVE ' '                    TO  X.                           EL528
01427      PERFORM WRITE-PRINTER.                                       EL528
01428                                                                   EL528
01429      MOVE WS-HEADING3            TO  PRT.                         EL528
01430      MOVE ' '                    TO  X.                           EL528
01431      PERFORM WRITE-PRINTER.                                       EL528
01432                                                                   EL528
01433      MOVE WS-HEADING4            TO  PRT.                         EL528
01434      MOVE ' '                    TO  X.                           EL528
01435      PERFORM WRITE-PRINTER.                                       EL528
01436                                                                   EL528
01437                                                                   EL528
01438      MOVE WS-HEADING5            TO  PRT.                         EL528
01439      PERFORM WRITE-PRINTER.                                       EL528
01440                                                                   EL528
01441      MOVE WS-HEADING6            TO  PRT.                         EL528
01442      PERFORM WRITE-PRINTER.                                       EL528
01443                                                                   EL528
01444      MOVE WS-HEADING7            TO  PRT.                         EL528
01445      PERFORM WRITE-PRINTER.                                       EL528
01446                                                                   EL528
01447      MOVE +10                    TO  WS-LINE-COUNT.               EL528
01448                                                                   EL528
01449                                                                   EL528
01450  WHS-020.                        COPY ELCWHS2.                    EL528
01451                                                                   EL528
01452      EJECT                                                        EL528
01453                                                                   EL528
01454  WRITE-PRINTER SECTION.                                           EL528
01455                                                                   EL528
01456  WPS-010.                                                         EL528
01457      IF WS-PRINT-OPTION = ('S' OR 'T')                            EL528
01458          NEXT SENTENCE                                            EL528
01459      ELSE                                                         EL528
01460          GO TO WPS-020.                                           EL528
01461                                                                   EL528
01462      MOVE WS-COMPANY-CD          TO  RF-COMPANY-CD.               EL528
01463      MOVE '1'                    TO  RF-RECORD-TYPE.              EL528
01464      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                EL528
01465      ADD +1                      TO  WS-LINE-NUMBER.              EL528
01466      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              EL528
01467      MOVE PRT                    TO  RF-REPORT-LINE-133.          EL528
01468                                                                   EL528
01469      WRITE REPORT-SAVE-FILE.                                      EL528
01470                                                                   EL528
01471      IF ELREPT-FILE-STATUS NOT = ZERO                             EL528
01472          MOVE 'ERROR OCCURED WRITE ELREPT'  TO  WS-ABEND-MESSAGE  EL528
01473          MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01474          GO TO ABEND-PGM.                                         EL528
01475                                                                   EL528
01476  WPS-020.                                                         EL528
01477      IF WS-PRINT-OPTION = 'F' OR 'B'                              EL528
01478          WRITE FICH-REC FROM PRT.                                 EL528
01479                                                                   EL528
01480      IF WS-PRINT-OPTION = 'P' OR 'B' OR 'T'                       EL528
01481          MOVE P-CTL TO LCP-ASA                                    EL528
01482          PERFORM LCP-WRITE-POS-PRT                                EL528
01483              THRU LCP-WRITE-END-PRT.                              EL528
01484                                                                   EL528
01485  WPS-EXIT.                                                        EL528
01486      EXIT.                                                        EL528
01487      EJECT                                                        EL528
01488                                                                   EL528
01489  OPEN-FILES SECTION.                                              EL528
01490                                                                   EL528
01491  OFS-010.                                                         EL528
01492      OPEN INPUT ERPNDB                                            EL528
01493                 ERACCT                                            EL528
01494                 ELCNTL                                            EL528
01495                 ELPGMS.                                           EL528
01496                                                                   EL528
01497      IF ERPNDB-FILE-STATUS  = '00' OR '97'                        EL528
01498          NEXT SENTENCE                                            EL528
01499        ELSE                                                       EL528
01500          MOVE 'ERROR OCCURED OPEN - ERPNDB'                       EL528
01501                                  TO  WS-ABEND-MESSAGE             EL528
01502          MOVE ERPNDB-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01503          GO TO ABEND-PGM.                                         EL528
01504                                                                   EL528
01505      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL528
01506          NEXT SENTENCE                                            EL528
01507        ELSE                                                       EL528
01508          MOVE 'ERROR OCCURED OPEN - ELCNTL'                       EL528
01509                                  TO  WS-ABEND-MESSAGE             EL528
01510          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01511          GO TO ABEND-PGM.                                         EL528
01512                                                                   EL528
01513      IF ERACCT-FILE-STATUS  = '00' OR '97'                        EL528
01514          NEXT SENTENCE                                            EL528
01515        ELSE                                                       EL528
01516          MOVE 'ERROR OCCURED OPEN - ERACCT'                       EL528
01517                                  TO  WS-ABEND-MESSAGE             EL528
01518          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01519          GO TO ABEND-PGM.                                         EL528
01520                                                                   EL528
01521      IF ELPGMS-FILE-STATUS  = '00' OR '97'                        EL528
01522          NEXT SENTENCE                                            EL528
01523        ELSE                                                       EL528
01524          MOVE 'ERROR OCCURED OPEN - ELPGMS'                       EL528
01525                                  TO  WS-ABEND-MESSAGE             EL528
01526          MOVE ELPGMS-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01527          GO TO ABEND-PGM.                                         EL528
01528                                                                   EL528
01529      SET TOT-INDX TO +1.                                          EL528
01530                                                                   EL528
01531                                                                   EL528
01532  OFS-EXIT.                                                        EL528
01533                                                                   EL528
01534      EXIT.                                                        EL528
01535                                                                   EL528
01536  CLOSE-FILES SECTION.                                             EL528
01537                                                                   EL528
01538  CFS-010.                                                         EL528
01539      IF FICHE-OPEN                                                EL528
01540          CLOSE FICH.                                              EL528
01541                                                                   EL528
01542      IF PRNTR-OPEN                                                EL528
01543          CLOSE PRNTR.                                             EL528
01544                                                                   EL528
01545      IF ELREPT-OPEN                                               EL528
01546          CLOSE ELREPT                                             EL528
01547          IF ELREPT-FILE-STATUS NOT = ZERO                         EL528
01548              MOVE 'ERROR OCCURED CLOSE - ELREPT'                  EL528
01549                                      TO  WS-ABEND-MESSAGE         EL528
01550              MOVE ELREPT-FILE-STATUS TO  WS-ABEND-FILE-STATUS     EL528
01551              GO TO ABEND-PGM.                                     EL528
01552                                                                   EL528
01553      CLOSE ELCNTL                                                 EL528
01554            ERACCT                                                 EL528
01555            ELPGMS.                                                EL528
01556                                                                   EL528
01557      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL528
01558          MOVE 'ERROR OCCURED CLOSE - ELCNTL'                      EL528
01559                                  TO  WS-ABEND-MESSAGE             EL528
01560          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01561          GO TO ABEND-PGM.                                         EL528
01562                                                                   EL528
01563      IF ERACCT-FILE-STATUS NOT = ZERO                             EL528
01564          MOVE 'ERROR OCCURED CLOSE - ERACCT'                      EL528
01565                                  TO  WS-ABEND-MESSAGE             EL528
01566          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01567          GO TO ABEND-PGM.                                         EL528
01568                                                                   EL528
01569      IF ELPGMS-FILE-STATUS NOT = ZERO                             EL528
01570          MOVE 'ERROR OCCURED CLOSE - ELPGMS'                      EL528
01571                                  TO  WS-ABEND-MESSAGE             EL528
01572          MOVE ELPGMS-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL528
01573          GO TO ABEND-PGM.                                         EL528
01574                                                                   EL528
01575  CFS-EXIT.                                                        EL528
01576      EXIT.                                                        EL528
01577                                                                   EL528
01578  ABEND-PGM SECTION. COPY ELCABEND.                                EL528
01579 /                                                                 EL528
01580  LCP-WRITE-POS-PRT SECTION.                                       EL528
01581      IF LCP-ASA = '+'                                             EL528
01582          WRITE PRT AFTER 0 LINE                                   EL528
01583      ELSE                                                         EL528
01584      IF LCP-ASA = ' '                                             EL528
01585          WRITE PRT AFTER ADVANCING 1 LINE                         EL528
01586      ELSE                                                         EL528
01587      IF LCP-ASA = '0'                                             EL528
01588          WRITE PRT AFTER ADVANCING 2 LINE                         EL528
01589      ELSE                                                         EL528
01590      IF LCP-ASA = '-'                                             EL528
01591          WRITE PRT AFTER ADVANCING 3 LINE                         EL528
01592      ELSE                                                         EL528
01593      IF LCP-ASA = '1'                                             EL528
01594          WRITE PRT AFTER ADVANCING PAGE                           EL528
01595      ELSE                                                         EL528
01596      IF LCP-ASA = '2'                                             EL528
01597          WRITE PRT AFTER ADVANCING LCP-CH2                        EL528
01598      ELSE                                                         EL528
01599      IF LCP-ASA = '3'                                             EL528
01600          WRITE PRT AFTER ADVANCING LCP-CH3                        EL528
01601      ELSE                                                         EL528
01602      IF LCP-ASA = '4'                                             EL528
01603          WRITE PRT AFTER ADVANCING LCP-CH4                        EL528
01604      ELSE                                                         EL528
01605      IF LCP-ASA = '5'                                             EL528
01606          WRITE PRT AFTER ADVANCING LCP-CH5                        EL528
01607      ELSE                                                         EL528
01608      IF LCP-ASA = '6'                                             EL528
01609          WRITE PRT AFTER ADVANCING LCP-CH6                        EL528
01610      ELSE                                                         EL528
01611      IF LCP-ASA = '7'                                             EL528
01612          WRITE PRT AFTER ADVANCING LCP-CH7                        EL528
01613      ELSE                                                         EL528
01614      IF LCP-ASA = '8'                                             EL528
01615          WRITE PRT AFTER ADVANCING LCP-CH8                        EL528
01616      ELSE                                                         EL528
01617      IF LCP-ASA = '9'                                             EL528
01618          WRITE PRT AFTER ADVANCING LCP-CH9                        EL528
01619      ELSE                                                         EL528
01620      IF LCP-ASA = 'A'                                             EL528
01621          WRITE PRT AFTER ADVANCING LCP-CH10                       EL528
01622      ELSE                                                         EL528
01623      IF LCP-ASA = 'B'                                             EL528
01624          WRITE PRT AFTER ADVANCING LCP-CH11                       EL528
01625      ELSE                                                         EL528
01626      IF LCP-ASA = 'C'                                             EL528
01627          WRITE PRT AFTER ADVANCING LCP-CH12                       EL528
01628      ELSE                                                         EL528
01629      IF LCP-ASA = 'V'                                             EL528
01630          WRITE PRT AFTER ADVANCING LCP-P01                        EL528
01631      ELSE                                                         EL528
01632      IF LCP-ASA = 'W'                                             EL528
01633          WRITE PRT AFTER ADVANCING LCP-P02                        EL528
01634      ELSE                                                         EL528
01635      DISPLAY 'ASA CODE ERROR'.                                    EL528
01636  LCP-WRITE-END-PRT.                                               EL528
01637      EXIT.                                                        EL528
01638                                                                   EL528
