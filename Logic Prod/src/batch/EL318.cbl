00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL318
00003  PROGRAM-ID.                 EL318 .                                 LV008
00004 *              PROGRAM CONVERTED BY                               EL318
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL318
00006 *              CONVERSION DATE 02/13/96 14:53:56.                 EL318
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL318
00008 *                            VMOD=2.015                           EL318
00009                                                                   EL318
00009                                                                   EL318
00010 *AUTHOR.     LOGIC INC.                                           EL318
00011 *            DALLAS, TEXAS.                                       EL318
00012                                                                   EL318
00013 *DATE-COMPILED.                                                   EL318
00014                                                                   EL318
00015 *SECURITY.   *****************************************************EL318
00016 *            *                                                   *EL318
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL318
00018 *            *                                                   *EL318
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL318
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL318
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL318
00022 *            *                                                   *EL318
00023 *            *****************************************************EL318
00024                                                                   EL318
00025 *REMARKS.                                                         EL318
00026 *       GENERAL FUNCTION IS TO PRODUCE THE CLAIM REGISTER REPORT- EL318
00027 *       CLAIM MASTER FILE IS SEQUENTIALLY PROCESSED, AND A        EL318
00028 *       REPORT OF ALL CLAIMS IN THE FILE IS PRINTED.              EL318
00029 *       AT END OF JOB, A SUMMARY PAGE IS PRODUCED SHOWING         EL318
00030 *       THE FILE STATISTICS  AND PROCESSOR STATISTICS.            EL318
00031 *                                                                 EL318
00032 *       INPUT FILES-             CLAIM HISTORY FILE               EL318
00033 *                                DATE CARD FILE                   EL318
00034 *                                                                 EL318
00035 *       OUTPUT-                  CLAIM REGISTER REPORT            EL318
00036 *                                (FICH FILE)                      EL318
00037 *                                                                 EL318
00038 *                                                                 EL318
00039 ****************  I  M  P  O  R  T  A  N  T  *********************EL318
00040 *                                                                 EL318
00041 *       ANY CHANGES MADE TO THIS PROGRAM NEED TO BE DUPLICATED    EL318
00042 *       IN PROGRAM ELGX318!.                                      EL318
00043 *                                                                 EL318
00044 ****************  I  M  P  O  R  T  A  N  T  *********************EL318
00045                                                                   EL318
070207******************************************************************
070207*                   C H A N G E   L O G
070207*
070207* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
070207*-----------------------------------------------------------------
070207*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
070207* EFFECTIVE    NUMBER
070207*-----------------------------------------------------------------
070107* 070107  IR2007070200002  PEMA  INCREASE SIZE OF PROCESSOR TABLE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
070207******************************************************************

00047  ENVIRONMENT DIVISION.                                            EL318
00048                                                                   EL318
00049  INPUT-OUTPUT SECTION.                                            EL318
00050                                                                   EL318
00051  FILE-CONTROL.                                                    EL318
00052                                                                   EL318
00053      SELECT CLAIM-HIST  ASSIGN TO SYS010-UT-2400-S-SYS010.        EL318
00054                                                                   EL318
00055      SELECT PRNTR       ASSIGN TO SYS008-UR-1403-S-SYS008.        EL318
00056                                                                   EL318
00057      SELECT FICH        ASSIGN TO SYS020-UT-2400-S-SYS020.        EL318
00058                                                                   EL318
00059      SELECT DISK-DATE   ASSIGN TO SYS019-FBA1-S-SYS019.           EL318
00060                                                                   EL318
00061      SELECT SORT-FILE   ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.       EL318
00062                                                                   EL318
00063      SELECT MPPLAN      ASSIGN TO SYS021-FBA1-MPPLAN              EL318
00064                         ORGANIZATION IS INDEXED                   EL318
00065                         ACCESS IS DYNAMIC                         EL318
00066                         RECORD KEY IS PP-CONTROL-PRIMARY          EL318
00067                         FILE STATUS IS MPPLAN-FILE-STATUS.        EL318
00068                                                                   EL318
00069      SELECT ELREPT      ASSIGN TO SYS010-FBA1-ELREPT              EL318
00070              ORGANIZATION IS INDEXED                              EL318
00071              ACCESS IS DYNAMIC                                    EL318
00072              RECORD KEY IS RF-CONTROL-PRIMARY                     EL318
00073              FILE STATUS IS DTE-VSAM-FLAGS.                       EL318
00074                                                                   EL318
00075  DATA DIVISION.                                                   EL318
00076                                                                   EL318
00077  FILE SECTION.                                                    EL318
00078                                                                   EL318
00079  FD  CLAIM-HIST             COPY ELCHAF.                          EL318
00080                                                                   EL318
00081  FD  PRNTR                  COPY ELCPRTFD.                        EL318
00082                                                                   EL318
00083  FD  DISK-DATE              COPY ELCDTEFD.                        EL318
00084                                                                   EL318
00085  FD  FICH                   COPY ELCFCHFD.                        EL318
00086                                                                   EL318
00087  FD  MPPLAN.                                                      EL318
00088                                                                   EL318
00089                             COPY MPCPLAN.                         EL318
00090                                                                   EL318
00091  FD  ELREPT                 COPY ELCRPTFD.                        EL318
00092                                                                   EL318
00093                             COPY ELCREPT.                         EL318
00094                                                                   EL318
00095  SD  SORT-FILE.                                                   EL318
00096  01  SORT-RECORD.                                                 EL318
00097      05  SR-CONTROL.                                              EL318
00098          10  SR-NAME             PIC X(15).                       EL318
00099          10  SR-CARRIER          PIC X.                           EL318
00100          10  SR-CLAIM-NO         PIC X(7).                        EL318
00101          10  SR-CERT-NO.                                          EL318
00102              15  SR-CERT-PRIME   PIC X(10).                       EL318
00103              15  SR-CERT-SFX     PIC X.                           EL318
00104                                                                   EL318
00105      05  SR-REST-OF-RECORD       PIC X(350).                      EL318
00106                                                                   EL318
00107      EJECT                                                        EL318
00108  WORKING-STORAGE SECTION.                                         EL318
00109  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL318
00110                                                                   EL318
00111  77  FILLER  PIC X(32) VALUE '********************************'.  EL318
00112  77  FILLER  PIC X(32) VALUE '      EL318 WORKING-STORAGE     '.  EL318
00113  77  FILLER  PIC X(32) VALUE '******** VMOD=2.015 ************'.  EL318
00114                                                                   EL318
00115  77  WS-CL-ESTABLISH-DT      PIC XX    VALUE SPACES.              EL318
00116  77  WS-AT-RECORDED-DT       PIC XX    VALUE SPACES.              EL318
00117  77  WS-RUN-DT               PIC XX    VALUE SPACES.              EL318
00118                                                                   EL318
00119  77  RANGE-SW                PIC X                   VALUE SPACES.EL318
00120      88  CLAIM-WITHIN-RANGE                          VALUE 'X'.   EL318
00121      88  CLAIM-NOT-WITHIN-RANGE                      VALUE ' '.   EL318
00122  77  WS-INC-MY               PIC S9(5)        COMP-3 VALUE +0.    EL318
00123  77  WS-PDTH-MY              PIC S9(5)        COMP-3 VALUE +0.    EL318
00124  77  WS-EFF-MY               PIC S9(5)        COMP-3 VALUE +0.    EL318
00125  77  WS-REM-TERM             PIC S9(5)        COMP-3 VALUE +0.    EL318
00126  77  WS-CLAIM-DURA           PIC S9(5)        COMP-3 VALUE +0.    EL318
00127  77  WS-RATIO                PIC S9V9(7)      COMP-3 VALUE +0.    EL318
00128  77  WS-TOT-REM-TERM         PIC S9(11)       COMP-3 VALUE +0.    EL318
00129  77  WS-TOT-CLAIM-DURA       PIC S9(11)       COMP-3 VALUE +0.    EL318
00130  77  WS-TOT-FACTOR           PIC S9(3)V9(4)   COMP-3 VALUE +0.    EL318
00131  77  WS-AH-CLAIM-COUNT       PIC S9(11)       COMP-3 VALUE +0.    EL318
00132  77  WS-AVG-CLAIM-DURA       PIC S9(5)V99     COMP-3 VALUE +0.    EL318
00133                                                                   EL318
00134  01  FILLER                          COMP-3.                      EL318
00135      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL318
00136      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL318
00137      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +65.   EL318
00138      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL318
00139      05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  EL318
00140      05  WS-TOT-RECORDS-RELEASED     PIC S9(7)       VALUE ZERO.  EL318
00141      05  WS-TOT-PURGED-CLAIMS        PIC S9(7)       VALUE ZERO.  EL318
00142                                                                   EL318
00143      EJECT                                                        EL318
00144  01  FILLER.                                                      EL318
00145      05  WS-ARCHIVE-NO               PIC S9(8)   COMP VALUE ZERO. EL318
00146      05  WS-ABEND-MESSAGE            PIC  X(80)       VALUE SPACE.EL318
00147      05  WS-ABEND-FILE-STATUS        PIC  XX          VALUE ZERO. EL318
00148      05  MPPLAN-FILE-STATUS          PIC  X(02)  VALUE '00'.      EL318
00149      05  WS-SAVE-PRINT-RECORD        PIC  X(133)      VALUE SPACE.EL318
00150      05  X                           PIC  X           VALUE SPACE.EL318
00151      05  WS-PROCESSOR                PIC  X(4)   VALUE LOW-VALUES.EL318
00152      05  PGM-SUB                     PIC S9(4) COMP VALUE +318.   EL318
00153      05  ABEND-CODE                  PIC  X(4)        VALUE SPACE.EL318
00154      05  ABEND-OPTION                PIC  X           VALUE SPACE.EL318
00155      05  WS-BYPASS-SW                PIC  X.                      EL318
00156          88  BYPASS-THIS-CLAIM    VALUE 'Y'.                      EL318
00157      05  OLC-REPORT-NAME             PIC  X(5)      VALUE 'EL318'.EL318
00158      05  WS-EST-DT.                                               EL318
00159          10  WS-EST-MO               PIC  99.                     EL318
00160          10  FILLER                  PIC  XX.                     EL318
00161          10  WS-EST-YR               PIC  99.                     EL318
00162      05  WS-WORK-DATE.                                            EL318
00163          10  WORK-CY                 PIC  9(04)       VALUE ZEROS.EL318
00164          10  WORK-CYR REDEFINES WORK-CY.                          EL318
00165              15  WORK-C              PIC  99.                     EL318
00166              15  WORK-Y              PIC  99.                     EL318
00167          10  WORK-M                  PIC  99          VALUE ZEROS.EL318
00168          10  WORK-D                  PIC  99          VALUE ZEROS.EL318
00169      05  B-WORK-DATE.                                             EL318
00170          10  B-WORK-Y                PIC  XX          VALUE SPACE.EL318
00171          10  B-WORK-M                PIC  XX          VALUE SPACE.EL318
00172          10  B-WORK-D                PIC  XX          VALUE SPACE.EL318
00173                                                                   EL318
00174      05  WS-CLO-DT.                                               EL318
00175          10  WS-CLO-MO               PIC  99.                     EL318
00176          10  FILLER                  PIC  XX.                     EL318
00177          10  WS-CLO-YR               PIC  99.                     EL318
00178                                                                   EL318
00179      05  WS-PMT-DT.                                               EL318
00180          10  WS-PMT-MO               PIC  99.                     EL318
00181          10  FILLER                  PIC  XX.                     EL318
00182          10  WS-PMT-YR               PIC  99.                     EL318
00183                                                                   EL318
00184      05  WS-LAST-MO                  PIC  99.                     EL318
LGC190     05  WS-LAST-YR                  PIC  S99.                    EL318
00185 **   05  WS-LAST-YR                  PIC  99.                     EL318
00186                                                                   EL318
00187      05  WS-PROCESSOR-TABLE.                                      EL318
070107        07  FILLER                   OCCURS 150 TIMES             EL318
00189                                      INDEXED BY PROC-INDEX.       EL318
00190          10  P-TBL-PROC-NO           PIC  X(4).                   EL318
00191          10  P-TBL-OPEN-CLAIMS-CNTR  PIC S9(5) COMP-3.            EL318
00192          10  P-TBL-EST-THIS-MO-CNTR  PIC S9(5) COMP-3.            EL318
00193          10  P-TBL-PMT-THIS-MO-CNTR  PIC S9(5) COMP-3.            EL318
00194          10  P-TBL-PMT-FORCED-CNTR   PIC S9(5) COMP-3.            EL318
00195          10  P-TBL-CLO-THIS-MO-CNTR  PIC S9(5) COMP-3.            EL318
00196          10  P-TBL-OTHER-ACT-CNTR    PIC S9(5) COMP-3.            EL318
00197                                                                   EL318
070107     05  PROCESSOR-TABLE-MAX         PIC S9(4) COMP VALUE +150.   EL318
00199                                                                   EL318
00200                                  COPY ELCCERT.                    EL318
00201     EJECT                                                         EL318
00202                                  COPY MPCPLCY.                    EL318
00203                                                                   EL318
00204     EJECT                                                         EL318
00205                                                                   EL318
00206                                  COPY ELCARCH.                    EL318
00207                                                                   EL318
00208     EJECT                                                         EL318
00209                                  COPY ELCMSTR.                    EL318
00210                                                                   EL318
00211     EJECT                                                         EL318
00212                                  COPY ELCTRLR.                    EL318
00213                                                                   EL318
00214      EJECT                                                        EL318
00215  01  WS-HEADING1.                                                 EL318
00216      05  FILLER             PIC X(48) VALUE '1'.                  EL318
00217      05  FILLER             PIC X(25) VALUE                       EL318
00218          'REGISTER OF CLAIMS ONLINE'.                             EL318
00219      05  FILLER             PIC X(47) VALUE SPACES.               EL318
00220      05  FILLER             PIC X(8)  VALUE 'EL318A'.             EL318
00221                                                                   EL318
00222  01  WS-HEADING1A.                                                EL318
00223      05  FILLER             PIC X(48) VALUE '1'.                  EL318
00224      05  FILLER             PIC X(25) VALUE                       EL318
00225          'REGISTER OF PURGED CLAIMS'.                             EL318
00226      05  FILLER             PIC X(47) VALUE SPACES.               EL318
00227      05  FILLER             PIC X(8)  VALUE 'EL318B'.             EL318
00228                                                                   EL318
00229  01  WS-HEADING2.                                                 EL318
00230      05  FILLER             PIC X(45) VALUE SPACE.                EL318
00231      05  WS-H2-CLIENT-NAME  PIC X(30) VALUE SPACES.               EL318
00232      05  FILLER             PIC X(45) VALUE SPACES.               EL318
00233      05  WS-H2-DATE         PIC X(8)  VALUE SPACES.               EL318
00234                                                                   EL318
00235  01  WS-HEADING3.                                                 EL318
00236      05  FILLER             PIC X(52) VALUE SPACE.                EL318
00237      05  WS-H3-DATE         PIC X(18).                            EL318
00238      05  FILLER             PIC X(50) VALUE SPACES.               EL318
00239      05  FILLER             PIC X(4)  VALUE 'PAGE'.               EL318
00240      05  FILLER             PIC X     VALUE SPACE.                EL318
00241      05  WS-H3-PAGE         PIC ZZZZ9.                            EL318
00242                                                                   EL318
00243  01  WS-HEADING4.                                                 EL318
00244      05  FILLER             PIC X(29) VALUE  '0'.                 EL318
00245      05  FILLER             PIC X(50) VALUE                       EL318
00246          '-------- C E R T I F I C A T E   D A T A ---------'.    EL318
00247      05  FILLER             PIC X VALUE SPACES.                   EL318
00248      05  FILLER             PIC X(19) VALUE                       EL318
00249          '-------------------'.                                   EL318
00250      05  FILLER             PIC X(21) VALUE                       EL318
00251          ' C L A I M   D A T A '.                                 EL318
00252      05  FILLER             PIC X(19) VALUE                       EL318
00253          '-------------------'.                                   EL318
00254                                                                   EL318
00255  01  WS-HEADING4A.                                                EL318
00256      05  FILLER             PIC X(16) VALUE   '0'.                EL318
00257      05  FILLER             PIC X(117) VALUE SPACES.              EL318
00258                                                                   EL318
00259  01  WS-HEADING5.                                                 EL318
00260      05  FILLER             PIC X(24) VALUE                       EL318
00261          '0      CLAIM        CERT'.                              EL318
00262      05  FILLER             PIC X(41)  VALUE SPACES.              EL318
00263      05  FILLER             PIC X(13)  VALUE                      EL318
00264          'BEN  ORIGINAL'.                                         EL318
00265      05  FILLER             PIC X(22)  VALUE SPACES.              EL318
00266      05  FILLER             PIC X(33) VALUE                       EL318
00267          'PAID-   PMTS   TOTAL'.                                  EL318
00268                                                                   EL318
00269  01  WS-HEADING5A.                                                EL318
00270      05  FILLER             PIC X(23)  VALUE                      EL318
00271          '0      CLAIM       CERT'.                               EL318
00272      05  FILLER             PIC X(65)  VALUE SPACES.              EL318
00273      05  FILLER             PIC X(45)  VALUE                      EL318
00274          'LAST MAINT       MICROFILM'.                            EL318
00275                                                                   EL318
00276  01  WS-HEADING6.                                                 EL318
00277      05  FILLER             PIC X(40)  VALUE                      EL318
00278          ' CAR  NUMBER TYP   NUMBER     ACCOUNT'.                 EL318
00279      05  FILLER             PIC X(40)  VALUE                      EL318
00280          'ST  GROUP EFFECTIVE TERM CD   BENEFIT'.                 EL318
00281      05  FILLER             PIC X(20)  VALUE                      EL318
00282          'INCURRED REPORTED  '.                                   EL318
00283      05  WS-HEAD6-THRU-TO   PIC X(04)  VALUE 'THRU'.              EL318
00284      05  FILLER             PIC X(29)  VALUE                      EL318
00285          '    MADE  PAYMENTS STATUS'.                             EL318
00286                                                                   EL318
00287  01  WS-HEADING6A.                                                EL318
00288      05  FILLER             PIC X(52)  VALUE                      EL318
00289          ' CAR  NUMBER TYP  NUMBER       ACCOUNT    ST  GROUP'.   EL318
00290      05  FILLER             PIC X(43)  VALUE                      EL318
00291          '             NAME                      DATE'.           EL318
00292      05  FILLER             PIC X(35)  VALUE                      EL318
00293          '           NUMBER'.                                     EL318
00294                                                                   EL318
00295  01  WS-HEADING4B.                                                EL318
00296      05  FILLER             PIC X(64) VALUE                       EL318
00297          '-* * *  F I L E   S T A T I S T I C S  * * *'.          EL318
00298      05  FILLER             PIC X(46) VALUE                       EL318
00299       'OPEN    ESTABLISHED PAYMENTS MADE   FORCED'.               EL318
00300      05  FILLER             PIC X(20) VALUE                       EL318
00301       ' CLOSED      OTHER'.                                       EL318
00302                                                                   EL318
00303  01  WS-HEADING5B.                                                EL318
00304      05  FILLER             PIC X(51) VALUE SPACES.               EL318
00305      05  FILLER             PIC X(45) VALUE                       EL318
00306          'PROCESSOR   CLAIMS   THIS MONTH   THIS MONTH'.          EL318
00307      05  FILLER             PIC X(11) VALUE                       EL318
00308          ' THIS MONTH'.                                           EL318
00309      05  FILLER             PIC X(23) VALUE                       EL318
00310          '  THIS MONTH   ACTIVITY'.                               EL318
00311                                                                   EL318
00312  01  WS-DETAIL1.                                                  EL318
00313      05  FILLER             PIC XX       VALUE  SPACE.            EL318
00314      05  P-CARR             PIC X.                                EL318
00315      05  FILLER             PIC XX       VALUE  SPACES.           EL318
00316      05  P-CLAIM-NO         PIC X(7).                             EL318
00317      05  FILLER             PIC XX       VALUE  SPACES.           EL318
00318      05  P-TYPE             PIC X.                                EL318
00319      05  FILLER             PIC XX       VALUE  SPACES.           EL318
00320      05  P-CERT-NO          PIC X(11).                            EL318
00321      05  FILLER             PIC X        VALUE  SPACES.           EL318
00322      05  P-CERT-ACCOUNT     PIC X(10).                            EL318
00323      05  FILLER             PIC X        VALUE  SPACES.           EL318
00324      05  P-CERT-STATE       PIC XX.                               EL318
00325      05  FILLER             PIC X        VALUE  SPACES.           EL318
00326      05  P-CERT-GROUPING    PIC X(6).                             EL318
00327      05  FILLER             PIC X        VALUE  SPACES.           EL318
00328      05  P-CERT-EFF-DT      PIC X(8).                             EL318
00329      05  FILLER             PIC XX       VALUE  SPACES.           EL318
00330      05  P-CM-ORIG-TERM     PIC ZZ9.                              EL318
00331      05  FILLER             PIC XX       VALUE  SPACES.           EL318
00332      05  P-CM-BENEFIT-CD    PIC X(3).                             EL318
00333      05  FILLER             PIC X        VALUE  SPACES.           EL318
00334      05  P-CM-BENEFIT-AMT   PIC ZZZ,ZZZ.99.                       EL318
00335      05  FILLER             PIC X        VALUE  SPACES.           EL318
00336      05  P-INCURRED-DT      PIC X(8).                             EL318
00337      05  FILLER             PIC X        VALUE SPACES.            EL318
00338      05  P-REPORTED-DT      PIC X(8).                             EL318
00339      05  FILLER             PIC X        VALUE  SPACES.           EL318
00340      05  P-PAID-THRU-DT     PIC X(8).                             EL318
00341      05  FILLER             PIC X        VALUE  SPACES.           EL318
00342      05  P-NO-OF-PMTS-MADE  PIC ZZZ9.                             EL318
00343      05  FILLER             PIC X        VALUE  SPACES.           EL318
00344      05  P-TOTAL-PAID-AMT   PIC ZZZ,ZZZ.99.                       EL318
00345      05  FILLER             PIC X        VALUE  SPACES.           EL318
00346      05  P-STATUS           PIC X(6)     VALUE  SPACES.           EL318
00347      05  FILLER             PIC X(3)     VALUE  SPACES.           EL318
00348                                                                   EL318
00349  01  WS-TOTAL-LINE1         REDEFINES                             EL318
00350      WS-DETAIL1.                                                  EL318
00351      05  FILLER                  PIC X.                           EL318
00352      05  WS-T1-DESC              PIC X(35).                       EL318
00353      05  FILLER                  PIC X.                           EL318
00354      05  WS-T1-COUNT             PIC Z,ZZZ,ZZ9-.                  EL318
00355      05  FILLER                  PIC X(4).                        EL318
00356      05  WS-T1-PROCESSOR         PIC X(4).                        EL318
00357      05  FILLER                  PIC X(3).                        EL318
00358      05  WS-T1-CLAIMS-CNTR       PIC Z,ZZZ,ZZ9-.                  EL318
00359      05  FILLER                  PIC XX.                          EL318
00360      05  WS-T1-ESTABLISHED       PIC Z,ZZZ,ZZ9-.                  EL318
00361      05  FILLER                  PIC XX.                          EL318
00362      05  WS-T1-PAYMENTS-MADE     PIC Z,ZZZ,ZZ9-.                  EL318
00363      05  FILLER                  PIC XX.                          EL318
00364      05  WS-T1-FORCED            PIC Z,ZZZ,ZZ9-.                  EL318
00365      05  FILLER                  PIC X(5).                        EL318
00366      05  WS-T1-CLOSED            PIC Z,ZZZ,ZZ9-.                  EL318
00367      05  FILLER                  PIC XX.                          EL318
00368      05  WS-T1-OTHER             PIC Z,ZZZ,ZZ9-.                  EL318
00369                                                                   EL318
00370  01  WS-DETAIL1A            REDEFINES                             EL318
00371      WS-DETAIL1.                                                  EL318
00372      05  FILLER             PIC XX.                               EL318
00373      05  PR-CARR            PIC X.                                EL318
00374      05  FILLER             PIC XX.                               EL318
00375      05  PR-CLAIM-NO        PIC X(7).                             EL318
00376      05  FILLER             PIC XX.                               EL318
00377      05  PR-TYPE            PIC X.                                EL318
00378      05  FILLER             PIC XX.                               EL318
00379      05  PR-CERT-NO         PIC X(11).                            EL318
00380      05  FILLER             PIC XX.                               EL318
00381      05  PR-CERT-ACCOUNT    PIC X(10).                            EL318
00382      05  FILLER             PIC XX.                               EL318
00383      05  PR-CERT-STATE      PIC XX.                               EL318
00384      05  FILLER             PIC XX.                               EL318
00385      05  PR-CERT-GROUPING   PIC X(6).                             EL318
00386      05  FILLER             PIC XX.                               EL318
00387      05  PR-LAST-NAME       PIC X(15).                            EL318
00388      05  FILLER             PIC X.                                EL318
00389      05  PR-FIRST-NAME      PIC X(12).                            EL318
00390      05  FILLER             PIC X.                                EL318
00391      05  PR-MID-INIT        PIC X.                                EL318
00392      05  FILLER             PIC X(5).                             EL318
00393      05  PR-MAINT-DATE      PIC X(8).                             EL318
00394      05  FILLER             PIC X(8).                             EL318
00395      05  PR-MICROFILM-NO    PIC X(10).                            EL318
00396                                                                   EL318
00397  01  WS-TOTAL-1A            REDEFINES                             EL318
00398      WS-DETAIL1.                                                  EL318
00399      05  WS-T1-CC                PIC X.                           EL318
00400      05  PR-DESC-PURGED-CLAIMS   PIC X(29).                       EL318
00401      05  FILLER                  PIC X.                           EL318
00402      05  PR-TOT-PURGED-CLAIMS    PIC Z,ZZZ,ZZ9-.                  EL318
00403      05  FILLER                  PIC X(14).                       EL318
00404                                                                   EL318
00405      EJECT                                                        EL318
00406  01  WS-TOTAL-LINES-AREA.                                         EL318
00407      05  FILLER.                                                  EL318
00408          10  FILLER                    PIC  X(25)        VALUE       CL**2
00409              ' CLAIMS ADDED TO FILE IN '  .                       EL318
00410          10  P-CURR-CC-1               PIC  XX.                   EL318
00411          10  P-CURR-YR-1               PIC  XX.                   EL318
00412          10  FILLER                    PIC  X(6)         VALUE    EL318
00413              '    - '.                                            EL318
00414          10  WS-CL-ADDED-CURR-YR-CNTR  PIC S9(7) COMP-3 VALUE +0. EL318
00415                                                                   EL318
00416      05  FILLER.                                                  EL318
00417          10  FILLER                    PIC  X(25)        VALUE       CL**3
00418              ' CLAIMS ADDED TO FILE IN '.                            CL**4
00419          10  P-LAST-CC-1               PIC  99.                      CL**4
00420          10  P-LAST-YR-1               PIC  XX.                      CL**3
00421          10  FILLER                    PIC  X(6)         VALUE    EL318
00422              '    - '.                                            EL318
00423          10  WS-CL-ADDED-LAST-YR-CNTR  PIC S9(7) COMP-3 VALUE +0. EL318
00424                                                                   EL318
00425      05  FILLER.                                                  EL318
00426          10  FILLER                    PIC  X(25)        VALUE    EL318
00427              ' CLAIMS ADDED TO FILE IN '.                         EL318
00428          10  P-CURR-MO                 PIC  XX.                   EL318
00429          10  FILLER                    PIC  X            VALUE       CL**3
00430              '/'.                                                    CL**3
00431          10  P-CURR-CC-2               PIC  99.                      CL**4
00432          10  P-CURR-YR-2               PIC  XX.                      CL**3
00433          10  FILLER                    PIC  X(3)         VALUE    EL318
00434              ' - '.                                               EL318
00435          10  WS-CL-ADDED-CURR-MO-CNTR  PIC S9(7) COMP-3 VALUE +0. EL318
00436                                                                   EL318
00437      05  FILLER.                                                  EL318
00438          10  FILLER                    PIC  X(25)        VALUE    EL318
00439              ' CLAIMS ADDED TO FILE IN '.                         EL318
00440          10  P-LAST-MO                 PIC  XX.                   EL318
00441          10  FILLER                    PIC  X            VALUE       CL**3
00442              '/'.                                                    CL**3
00443          10  P-CURR-CC-3               PIC  99.                      CL**4
00444          10  P-CURR-YR-3               PIC  XX.                      CL**3
00445          10  FILLER                    PIC  X(3)         VALUE    EL318
00446              ' - '.                                               EL318
00447          10  WS-CL-ADDED-LAST-MO-CNTR  PIC S9(7) COMP-3 VALUE +0. EL318
00448                                                                   EL318
00449      05  FILLER.                                                  EL318
00450          10  FILLER                    PIC  X(29)        VALUE    EL318
00451              ' TOTAL OPEN CLAIMS IN FILE - '.                     EL318
00452          10  WS-TL5-AH2-OVERRIDE       PIC  XX        VALUE SPACE.EL318
00453          10  FILLER                    PIC  X(4)      VALUE       EL318
00454              '  - '.                                              EL318
00455          10  WS-CL-OPEN-CL-AH-CNTR     PIC S9(7) COMP-3 VALUE +0. EL318
00456                                                                   EL318
00457      05  FILLER.                                                  EL318
00458          10  FILLER                    PIC  X(29)        VALUE    EL318
00459              ' TOTAL OPEN CLAIMS IN FILE - '.                     EL318
00460          10  WS-TL6-LF2-OVERRIDE       PIC  XX        VALUE SPACE.EL318
00461          10  FILLER                    PIC  X(4)         VALUE    EL318
00462              '  - '.                                              EL318
00463          10  WS-CL-OPEN-CL-LF-CNTR     PIC S9(7) COMP-3 VALUE +0. EL318
00464                                                                   EL318
00465      05  FILLER.                                                  EL318
00466          10  FILLER                    PIC  X(31)        VALUE    EL318
00467              ' TOTAL CLOSED CLAIMS IN FILE - '.                   EL318
00468          10  WS-TL7-AH2-OVERRIDE       PIC  XX        VALUE SPACE.EL318
00469          10  FILLER                    PIC  XX           VALUE    EL318
00470              '- '.                                                EL318
00471          10  WS-CL-CLOSED-CL-AH-CNTR   PIC S9(7) COMP-3 VALUE +0. EL318
00472                                                                   EL318
00473      05  FILLER.                                                  EL318
00474          10  FILLER                    PIC  X(31)        VALUE    EL318
00475              ' TOTAL CLOSED CLAIMS IN FILE - '.                   EL318
00476          10  WS-TL8-LF2-OVERRIDE       PIC  XX        VALUE SPACE.EL318
00477          10  FILLER                    PIC  XX           VALUE    EL318
00478              '- '.                                                EL318
00479          10  WS-CL-CLOSED-CL-LF-CNTR   PIC S9(7) COMP-3 VALUE +0. EL318
00480                                                                   EL318
00481      05  FILLER.                                                  EL318
00482          10  FILLER                    PIC  X         VALUE SPACE.EL318
00483          10  WS-TL9-AH2-OVERRIDE       PIC  XX        VALUE SPACE.EL318
00484          10  FILLER                    PIC  X(32)        VALUE    EL318
00485              '   CLAIMS                     - '.                  EL318
00486          10  WS-CL-AH-TOTAL-MSTR-CNTR  PIC S9(7) COMP-3 VALUE +0. EL318
00487                                                                   EL318
00488      05  FILLER.                                                  EL318
00489          10  FILLER                    PIC  X         VALUE SPACE.EL318
00490          10  WS-TL10-LF2-OVERRIDE      PIC  XX        VALUE SPACE.EL318
00491          10  FILLER                    PIC  X(32)        VALUE    EL318
00492              '   CLAIMS                     - '.                  EL318
00493          10  WS-CL-LF-TOTAL-MSTR-CNTR  PIC S9(7) COMP-3 VALUE +0. EL318
00494                                                                   EL318
00495      05  FILLER.                                                  EL318
00496          10  FILLER                    PIC  X(35)        VALUE    EL318
00497              ' TOTAL CLAIMS                    - '.               EL318
00498          10  WS-CL-TOTAL-MSTR-CNTR     PIC S9(7) COMP-3 VALUE +0. EL318
00499                                                                   EL318
00500      05  FILLER.                                                  EL318
00501          10  FILLER                    PIC  X(35)        VALUE    EL318
00502              ' ACTIVITY TRAILER COUNTS'.                          EL318
00503          10  FILLER                    PIC S9(7) COMP-3 VALUE +0. EL318
00504                                                                   EL318
00505      05  FILLER.                                                  EL318
00506          10  FILLER                    PIC  X(20)     VALUE SPACE.EL318
00507          10  FILLER                    PIC  X(15)        VALUE    EL318
00508              'RESERVES     - '.                                   EL318
00509          10  WS-CL-RESERVE-TRLR-CNTR   PIC S9(7) COMP-3 VALUE +0. EL318
00510                                                                   EL318
00511      05  FILLER.                                                  EL318
00512          10  FILLER                    PIC  X(20)     VALUE SPACE.EL318
00513          10  FILLER                    PIC  X(15)        VALUE    EL318
00514              'PAYMENTS     - '.                                   EL318
00515          10  WS-CL-PMT-TRLR-CNTR       PIC S9(7) COMP-3 VALUE +0. EL318
00516                                                                   EL318
00517      05  FILLER.                                                  EL318
00518          10  FILLER                    PIC  X(20)    VALUE SPACE. EL318
00519          10  FILLER                    PIC  X(15)        VALUE    EL318
00520              'AUTO PAYMENT - '.                                   EL318
00521          10  WS-CL-AUP-TRLR-CNTR       PIC S9(7) COMP-3 VALUE +0. EL318
00522                                                                   EL318
00523      05  FILLER.                                                  EL318
00524          10  FILLER                    PIC  X(20)     VALUE SPACE.EL318
00525          10  FILLER                    PIC  X(15)        VALUE    EL318
00526              'LETTERS      - '.                                   EL318
00527          10  WS-CL-COR-TRLR-CNTR       PIC S9(7) COMP-3 VALUE +0. EL318
00528                                                                   EL318
00529      05  FILLER.                                                  EL318
00530          10  FILLER                    PIC  X(20)     VALUE SPACE.EL318
00531          10  FILLER                    PIC  X(15)        VALUE    EL318
00532              'ADDRESSES    - '.                                   EL318
00533          10  WS-CL-ADD-TRLR-CNTR       PIC S9(7) COMP-3 VALUE +0. EL318
00534                                                                   EL318
00535      05  FILLER.                                                  EL318
00536          10  FILLER                    PIC  X(20)     VALUE SPACE.EL318
00537          10  FILLER                    PIC  X(15)        VALUE    EL318
00538              'NOTES        - '.                                   EL318
00539          10  WS-CL-NOT-TRLR-CNTR       PIC S9(7) COMP-3 VALUE +0. EL318
00540                                                                   EL318
00541      05  FILLER.                                                  EL318
00542          10  FILLER                    PIC  X(20)     VALUE SPACE.EL318
00543          10  FILLER                    PIC  X(15)        VALUE    EL318
00544              'REMINDERS    - '.                                   EL318
00545          10  WS-CL-REM-TRLR-CNTR       PIC S9(7) COMP-3 VALUE +0. EL318
00546                                                                   EL318
00547      05  FILLER.                                                  EL318
00548          10  FILLER                    PIC  X(20)     VALUE SPACE.EL318
00549          10  FILLER                    PIC  X(15)        VALUE    EL318
00550              'DENIAL       - '.                                   EL318
00551          10  WS-CL-DEN-TRLR-CNTR       PIC S9(7) COMP-3 VALUE +0. EL318
00552                                                                   EL318
00553      05  FILLER.                                                  EL318
00554          10  FILLER                    PIC  X(20)     VALUE SPACE.EL318
00555          10  FILLER                    PIC  X(15)        VALUE    EL318
00556              'INCURRED CHG - '.                                   EL318
00557          10  WS-CL-CHG-TRLR-CNTR       PIC S9(7) COMP-3 VALUE +0. EL318
00558                                                                   EL318
00559      05  FILLER.                                                  EL318
00560          10  FILLER                    PIC  X(20)     VALUE SPACE.EL318
00561          10  FILLER                    PIC  X(15)        VALUE    EL318
00562              'FORMS        - '.                                   EL318
00563          10  WS-CL-FORM-TRLR-CNTR      PIC S9(7) COMP-3 VALUE +0. EL318
00564                                                                   EL318
00565      05  FILLER.                                                  EL318
00566          10  FILLER                    PIC  X(20)     VALUE SPACE.EL318
00567          10  FILLER                    PIC  X(15)        VALUE    EL318
00568              'TOTAL        - '.                                   EL318
00569          10  WS-CL-TOTAL-TRLR-CNTR     PIC S9(7) COMP-3 VALUE +0. EL318
00570                                                                   EL318
00571      05  FILLER.                                                  EL318
00572          10  FILLER                    PIC  X(35)        VALUE    EL318
00573              'CERTIFICATES  CREATED BY CLAIMS  - '.               EL318
00574          10  WS-CERT-CREAT-BY-CL-CNTR  PIC S9(7) COMP-3 VALUE +0. EL318
00575                                                                   EL318
00576      05  FILLER.                                                  EL318
00577          10  FILLER                    PIC  X(35)        VALUE    EL318
00578              'CERTIFICATES ALREADY ONLINE      - '.               EL318
00579          10  WS-CERT-ONLINE-CNTR       PIC S9(7) COMP-3 VALUE +0. EL318
00580                                                                   EL318
00581      05  FILLER.                                                  EL318
00582          10  FILLER                    PIC  X(35) VALUE           EL318
00583              'LETTERS STORED IN ARCHIVE        - '.               EL318
00584          10  WS-LET-IN-ARCH-CNTR       PIC S9(7) COMP-3 VALUE +0. EL318
00585                                                                   EL318
00586      05  FILLER.                                                  EL318
00587          10  FILLER                    PIC  X(35)        VALUE    EL318
00588              'RECORDS STORED IN ARCHIVE        - '.               EL318
00589          10  WS-REC-IN-ARCH-CNTR       PIC S9(7) COMP-3 VALUE +0. EL318
00590                                                                   EL318
00591      05  FILLER.                                                  EL318
00592          10  FILLER                    PIC  X(35)        VALUE    EL318
00593              ' '.                                                 EL318
00594          10  FILLER                    PIC S9(7) COMP-3 VALUE +0. EL318
00595                                                                   EL318
00596      05  FILLER.                                                  EL318
00597          10  FILLER                    PIC  X(35)        VALUE    EL318
00598              ' '.                                                 EL318
00599          10  FILLER                    PIC S9(7) COMP-3 VALUE +0. EL318
00600                                                                   EL318
00601      05  FILLER.                                                  EL318
00602          10  FILLER                    PIC  X(35)        VALUE    EL318
00603              ' AVERAGE DAYS BEFORE PROCESSED'.                    EL318
00604          10  WS-AVERAGE-DAYS           PIC S9(7) COMP-3 VALUE +0. EL318
00605                                                                   EL318
00606      05  FILLER.                                                  EL318
00607          10  FILLER                    PIC  X(9)      VALUE       EL318
00608              ' AVERAGE '.                                         EL318
00609          10  WS-TL31-AH2-OVERRIDE      PIC  XX        VALUE SPACE.EL318
00610          10  FILLER                    PIC  X(24)        VALUE    EL318
00611              ' CLAIM DURATION         '.                          EL318
00612          10  WS-AVERAGE-DURATION       PIC S9(7) COMP-3 VALUE +0. EL318
00613                                                                   EL318
00614      05  FILLER.                                                  EL318
00615          10  FILLER                    PIC  X(28)        VALUE    EL318
00616              ' AVERAGE NUMBER OF PAYMENTS '.                      EL318
00617          10  WS-TL32-AH2-OVERRIDE      PIC  XX        VALUE SPACE.EL318
00618          10  FILLER                    PIC  X(5)      VALUE SPACE.EL318
00619          10  WS-AVERAGE-PAY-AH         PIC S9(7) COMP-3 VALUE +0. EL318
00620                                                                   EL318
00621      05  FILLER.                                                  EL318
00622          10  FILLER                    PIC  X(28)        VALUE    EL318
00623              ' AVERAGE NUMBER OF PAYMENTS '.                      EL318
00624          10  WS-TL33-LF2-OVERRIDE      PIC  XX        VALUE SPACE.EL318
00625          10  FILLER                    PIC  X(5)      VALUE SPACE.EL318
00626          10  WS-AVERAGE-PAY-LF         PIC S9(7) COMP-3 VALUE +0. EL318
00627                                                                   EL318
00628  01  FILLER                          REDEFINES                    EL318
00629      WS-TOTAL-LINES-AREA.                                         EL318
00630                                                                   EL318
00631      05  FILLER                  OCCURS 33 TIMES                  EL318
00632          INDEXED BY TOTAL-INDEX.                                  EL318
00633                                                                   EL318
00634          10  WS-TOTAL-DESCRIPTION      PIC  X(35).                EL318
00635          10  WS-TOTAL-COUNT           PIC S9(7) COMP-3.           EL318
00636                                                                   EL318
00637  01  TOTAL-INDEX-MAX                 PIC S9(4) COMP  VALUE +33.   EL318
00638                                                                   EL318
00639                             COPY ELCDATE.                            CL**8
00640                                                                   EL318
00641                              COPY ELCDTECX.                       EL318
00642                                                                   EL318
00643                              COPY ELCDTEVR.                       EL318
00644                                                                   EL318
00645      EJECT                                                        EL318
00646  PROCEDURE DIVISION.                                              EL318
00647                                                                   EL318
00648  0000-LOAD-DATE-CARD.       COPY ELCDTERX.                        EL318
00649                                                                   EL318
00650  0000-INIT-LF-AH-HEADINGS.                                        EL318
00651                                                                   EL318
00652      MOVE SPACE             TO  WS-BYPASS-SW.                     EL318
00653                                                                   EL318
00654      MOVE LIFE-OVERRIDE-L2  TO  WS-TL6-LF2-OVERRIDE               EL318
00655                                 WS-TL8-LF2-OVERRIDE               EL318
00656                                 WS-TL10-LF2-OVERRIDE              EL318
00657                                 WS-TL33-LF2-OVERRIDE.             EL318
00658                                                                   EL318
00659      MOVE AH-OVERRIDE-L2    TO  WS-TL5-AH2-OVERRIDE               EL318
00660                                 WS-TL7-AH2-OVERRIDE               EL318
00661                                 WS-TL9-AH2-OVERRIDE               EL318
00662                                 WS-TL31-AH2-OVERRIDE              EL318
00663                                 WS-TL32-AH2-OVERRIDE.             EL318
00664                                                                   EL318
00665  0000-MAIN-LOGIC SECTION.                                         EL318
00666                                                                   EL318
00667      PERFORM OPEN-FILES.                                          EL318
00668                                                                   EL318
00669      IF DTE-CLAIM-PAID-THRU-TO EQUAL '1'                          EL318
00670         MOVE ' TO ' TO WS-HEAD6-THRU-TO.                          EL318
00671                                                                   EL318
00672      SORT SORT-FILE                                               EL318
00673          ON ASCENDING KEY SR-CONTROL                              EL318
00674              INPUT PROCEDURE  IS 0100-PROCESS-HISTORY-FILE        EL318
00675              OUTPUT PROCEDURE IS 1000-PRINT-PURGED-REPORT.        EL318
00676                                                                   EL318
00677      GOBACK.                                                      EL318
00678                                                                   EL318
00679      EJECT                                                        EL318
00680  0100-PROCESS-HISTORY-FILE SECTION.                               EL318
00681                                                                   EL318
00682  0100-READ-HISTORY.                                               EL318
00683      READ  CLAIM-HIST                                             EL318
00684        AT END                                                     EL318
00685           GO TO 0900-CLOSE-HIST.                                  EL318
00686                                                                   EL318
00687      IF DTE-CLIENT LESS THAN HIR-COMPANY-ID                       EL318
00688           GO TO 0900-CLOSE-HIST.                                  EL318
00689                                                                   EL318
00690      IF DTE-CLIENT GREATER THAN HIR-COMPANY-ID                    EL318
00691           GO TO 0100-READ-HISTORY.                                EL318
00692                                                                   EL318
00693      IF  HIR-RECORD-ID EQUAL 'CL'                                 EL318
00694          NEXT SENTENCE                                            EL318
00695      ELSE                                                         EL318
00696          GO TO 0300-PROCESS-CERTIFICATE.                          EL318
00697                                                                   EL318
00698      MOVE SPACE             TO  WS-BYPASS-SW.                     EL318
00699      MOVE HIR-CLAIM-RECORD TO  CLAIM-MASTER.                      EL318
00700                                                                   EL318
00701      IF RECORD-HAS-BEEN-PURGED                                    EL318
00702          PERFORM 4000-RELEASE-SORT-RECORD.                        EL318
00703                                                                   EL318
00704      IF DTE-CLIENT EQUAL 'FIA'                                    EL318
00705          IF CL-CERT-EFF-DT GREATER THAN CL-INCURRED-DT            EL318
00706              MOVE 'Y'               TO  WS-BYPASS-SW              EL318
00707              DISPLAY 'INVALID RECORD ' CL-CONTROL-PRIMARY         EL318
00708                  ' CLAIM TYPE ' CL-CLAIM-TYPE ' STATUS '          EL318
00709                  CL-CLAIM-STATUS                                  EL318
00710          ELSE                                                     EL318
00711              MOVE SPACE             TO  WS-BYPASS-SW.             EL318
00712                                                                   EL318
00713      PERFORM 0270-ACCUMULATE-CLAIM THRU 0270-EXIT.                EL318
00714                                                                   EL318
00715      MOVE CL-FILE-ESTABLISH-DT TO WS-CL-ESTABLISH-DT.             EL318
00716      MOVE SPACES               TO WS-AT-RECORDED-DT.              EL318
00717                                                                   EL318
00718  0100-FIRST-TIME.                                                 EL318
00719                                                                   EL318
00720      IF  CL-CLAIM-TYPE = AH-OVERRIDE-L1                           EL318
00721          ADD +1 TO WS-CL-AH-TOTAL-MSTR-CNTR                       EL318
00722      ELSE                                                         EL318
00723          ADD +1 TO WS-CL-LF-TOTAL-MSTR-CNTR.                      EL318
00724                                                                   EL318
00725      ADD  +1               TO WS-CL-TOTAL-MSTR-CNTR.              EL318
00726                                                                   EL318
00727  0100-CONTINUE.                                                   EL318
00728      MOVE CL-CARRIER       TO P-CARR.                             EL318
00729      MOVE CL-CLAIM-TYPE    TO P-TYPE.                             EL318
00730      MOVE CL-CLAIM-NO      TO P-CLAIM-NO.                         EL318
00731                                                                   EL318
00732      IF  CL-INCURRED-DT = LOW-VALUES                              EL318
00733          MOVE    SPACES                TO P-INCURRED-DT           EL318
00734      ELSE                                                         EL318
00735          MOVE    CL-INCURRED-DT        TO DC-BIN-DATE-1           EL318
00736          MOVE    SPACES                TO DC-OPTION-CODE          EL318
00737          PERFORM 8500-DATE-CONVERSION                             EL318
00738          MOVE    DC-GREG-DATE-1-EDIT   TO P-INCURRED-DT.          EL318
00739                                                                   EL318
00740      IF CL-REPORTED-DT = LOW-VALUES                               EL318
00741          MOVE    SPACES              TO P-REPORTED-DT             EL318
00742      ELSE                                                         EL318
00743          MOVE    CL-REPORTED-DT      TO DC-BIN-DATE-1             EL318
00744          MOVE    SPACES              TO DC-OPTION-CODE            EL318
00745          PERFORM 8500-DATE-CONVERSION                             EL318
00746          MOVE    DC-GREG-DATE-1-EDIT TO P-REPORTED-DT.            EL318
00747                                                                   EL318
00748      IF CL-PAID-THRU-DT EQUAL LOW-VALUES                          EL318
00749         MOVE SPACES              TO P-PAID-THRU-DT                EL318
00750      ELSE                                                         EL318
00751         IF DTE-CLAIM-PAID-THRU-TO EQUAL ' '                       EL318
00752            MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1              EL318
00753            MOVE SPACES              TO DC-OPTION-CODE             EL318
00754            PERFORM 8500-DATE-CONVERSION                           EL318
00755            MOVE DC-GREG-DATE-1-EDIT TO P-PAID-THRU-DT             EL318
00756         ELSE                                                      EL318
00757            MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1              EL318
00758            MOVE '6'                 TO DC-OPTION-CODE             EL318
00759            MOVE +1                  TO DC-ELAPSED-DAYS            EL318
00760            MOVE +0                  TO DC-ELAPSED-MONTHS          EL318
00761            PERFORM 8500-DATE-CONVERSION                           EL318
00762            MOVE DC-GREG-DATE-1-EDIT TO P-PAID-THRU-DT.            EL318
00763                                                                   EL318
00764      MOVE CL-NO-OF-PMTS-MADE           TO P-NO-OF-PMTS-MADE.      EL318
00765      MOVE CL-TOTAL-PAID-AMT            TO P-TOTAL-PAID-AMT.       EL318
00766      MOVE SPACES                       TO P-STATUS.               EL318
00767                                                                   EL318
00768      IF RECORD-HAS-BEEN-PURGED                                    EL318
00769         OR BYPASS-THIS-CLAIM                                      EL318
00770          NEXT SENTENCE                                            EL318
00771      ELSE                                                         EL318
00772         IF ((CLAIM-IS-OPEN) AND                                   EL318
00773            (CL-PURGED-DT EQUAL LOW-VALUES OR SPACES))             EL318
00774             MOVE 'OPEN  '    TO P-STATUS                          EL318
00775             IF  CL-CLAIM-TYPE = AH-OVERRIDE-L1                    EL318
00776                 ADD +1   TO WS-CL-OPEN-CL-AH-CNTR                 EL318
00777             ELSE                                                  EL318
00778                 ADD +1   TO WS-CL-OPEN-CL-LF-CNTR                 EL318
00779         ELSE                                                      EL318
00780             IF CLAIM-IS-OPEN                                      EL318
00781                 DISPLAY CL-CONTROL-PRIMARY ' OPEN REATTACHMENT'.  EL318
00782                                                                   EL318
00783      IF  CLAIM-IS-CLOSED                                          EL318
00784          MOVE 'CLOSED'    TO P-STATUS                             EL318
00785          IF  CL-CLAIM-TYPE = AH-OVERRIDE-L1                       EL318
00786              ADD +1   TO WS-CL-CLOSED-CL-AH-CNTR                  EL318
00787          ELSE                                                     EL318
00788              ADD +1   TO WS-CL-CLOSED-CL-LF-CNTR.                 EL318
00789                                                                   EL318
00790      MOVE CL-CERT-NO                   TO P-CERT-NO.              EL318
00791      MOVE CL-CERT-ACCOUNT              TO P-CERT-ACCOUNT.         EL318
00792      MOVE CL-CERT-STATE                TO P-CERT-STATE.           EL318
00793      MOVE CL-CERT-GROUPING             TO P-CERT-GROUPING.        EL318
00794                                                                   EL318
00795      IF  CL-CERT-EFF-DT = LOW-VALUES                              EL318
00796          MOVE    SPACES               TO P-CERT-EFF-DT            EL318
00797      ELSE                                                         EL318
00798          MOVE    CL-CERT-EFF-DT       TO DC-BIN-DATE-1            EL318
00799          MOVE    SPACES               TO DC-OPTION-CODE           EL318
00800          PERFORM 8500-DATE-CONVERSION                             EL318
00801          MOVE    DC-GREG-DATE-1-EDIT  TO P-CERT-EFF-DT.           EL318
00802                                                                   EL318
00803      IF  CL-FILE-ESTABLISH-DT = LOW-VALUES                        EL318
00804          MOVE    ZEROS                TO WS-EST-DT                EL318
00805      ELSE                                                         EL318
00806          MOVE    CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1            EL318
00807          MOVE    SPACES               TO DC-OPTION-CODE           EL318
00808          PERFORM 8500-DATE-CONVERSION                             EL318
00809          MOVE    DC-GREG-DATE-1-MDY   TO WS-EST-DT.               EL318
00810                                                                   EL318
00811      IF  WS-EST-YR  =      RUN-YR                                 EL318
00812          ADD +1  TO WS-CL-ADDED-CURR-YR-CNTR                      EL318
00813          IF  WS-EST-MO  = RUN-MO                                  EL318
00814              ADD +1  TO WS-CL-ADDED-CURR-MO-CNTR                  EL318
00815                         P-TBL-EST-THIS-MO-CNTR (PROC-INDEX)       EL318
00816          ELSE                                                     EL318
00817              IF  WS-EST-MO  = WS-LAST-MO                          EL318
00818                  ADD +1  TO WS-CL-ADDED-LAST-MO-CNTR.             EL318
00819                                                                   EL318
00820      IF  WS-EST-YR  = WS-LAST-YR                                  EL318
00821          ADD +1  TO WS-CL-ADDED-LAST-YR-CNTR                      EL318
00822          IF  WS-EST-MO  = 12                                      EL318
00823              IF RUN-MO = 01                                       EL318
00824                  ADD +1  TO WS-CL-ADDED-LAST-MO-CNTR.             EL318
00825                                                                   EL318
00826      MOVE    CL-PROCESSOR-ID  TO  WS-PROCESSOR.                   EL318
00827      PERFORM 2100-PROCESSOR-LOOKUP.                               EL318
00828                                                                   EL318
00829      IF BYPASS-THIS-CLAIM                                         EL318
00830          NEXT SENTENCE                                            EL318
00831      ELSE                                                         EL318
00832         IF ((CLAIM-IS-OPEN) AND                                   EL318
00833         (CL-PURGED-DT EQUAL LOW-VALUES OR SPACES))                EL318
00834              ADD +1  TO P-TBL-OPEN-CLAIMS-CNTR (PROC-INDEX).      EL318
00835                                                                   EL318
00836      MOVE    CL-LAST-CLOSE-DT     TO DC-BIN-DATE-1.               EL318
00837      MOVE    SPACES               TO DC-OPTION-CODE.              EL318
00838      PERFORM 8500-DATE-CONVERSION.                                EL318
00839      MOVE    DC-GREG-DATE-1-MDY   TO WS-CLO-DT.                   EL318
00840                                                                   EL318
00841      IF    CLAIM-IS-CLOSED                                        EL318
00842        AND WS-CLO-YR = RUN-YR                                     EL318
00843        AND WS-CLO-MO = RUN-MO                                     EL318
00844            ADD +1  TO  P-TBL-CLO-THIS-MO-CNTR (PROC-INDEX).       EL318
00845                                                                   EL318
00846      GO TO 0100-PROCESS-HISTORY-FILE.                             EL318
00847                                                                   EL318
00848      EJECT                                                        EL318
00849  0270-ACCUMULATE-CLAIM.                                           EL318
00850      MOVE    CL-INCURRED-DT     TO DC-BIN-DATE-1.                 EL318
00851      MOVE    SPACE              TO DC-OPTION-CODE.                EL318
00852      PERFORM 8500-DATE-CONVERSION.                                EL318
00853 *    MOVE    DC-GREG-DATE-1-YMD TO WS-WORK-DATE.                     CL**5
00854      MOVE    DC-GREG-DATE-CYMD  TO WS-WORK-DATE.                     CL**5
00855 *    MOVE    DC-ALPHA-CEN-N    TO WORK-C.                            CL**5
00856      COMPUTE WS-INC-MY = (WORK-CY * 12) + WORK-M.                 EL318
00857                                                                   EL318
00858      IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                            EL318
00859         IF  CL-TOTAL-PAID-AMT GREATER THAN +0                     EL318
00860             IF  CLAIM-IS-CLOSED                                   EL318
00861                 IF  NO-CONVERSION-ERROR                           EL318
00862                     MOVE    CL-PAID-THRU-DT TO DC-BIN-DATE-1      EL318
00863                     MOVE    SPACE           TO DC-OPTION-CODE     EL318
00864                     PERFORM 8500-DATE-CONVERSION                  EL318
00865                     IF  NO-CONVERSION-ERROR                       EL318
00866 *                       MOVE    DC-GREG-DATE-1-YMD TO WS-WORK-DATE   CL**5
00867                         MOVE    DC-GREG-DATE-CYMD  TO WS-WORK-DATE   CL**5
00868 *                       MOVE    DC-ALPHA-CEN-N     TO WORK-C         CL**5
00869                         COMPUTE WS-PDTH-MY =                      EL318
00870                                 (WORK-CY * 12) + WORK-M           EL318
00871                         COMPUTE WS-CLAIM-DURA =                   EL318
00872                                 (WS-PDTH-MY - WS-INC-MY)          EL318
00873                         COMPUTE WS-TOT-CLAIM-DURA =               EL318
00874                                (WS-TOT-CLAIM-DURA + WS-CLAIM-DURA)EL318
00875                         ADD     1 TO WS-AH-CLAIM-COUNT.           EL318
00876  0270-EXIT.                                                       EL318
00877      EXIT.                                                        EL318
00878                                                                   EL318
00879  0280-ACCUMULATE-CERT.                                            EL318
00880      MOVE    CM-CERT-EFF-DT     TO DC-BIN-DATE-1.                 EL318
00881      MOVE    SPACE              TO DC-OPTION-CODE.                EL318
00882      PERFORM 8500-DATE-CONVERSION.                                EL318
00883 *    MOVE    DC-GREG-DATE-1-YMD TO WS-WORK-DATE.                     CL**5
00884      MOVE    DC-GREG-DATE-CYMD  TO WS-WORK-DATE.                     CL**5
00885 *    MOVE    DC-ALPHA-CEN-N     TO WORK-C.                           CL**5
00886      COMPUTE WS-EFF-MY = (WORK-CY * 12) + WORK-M.                 EL318
00887                                                                   EL318
00888      IF  CL-CLAIM-TYPE = AH-OVERRIDE-L1                           EL318
00889          IF  CL-TOTAL-PAID-AMT GREATER THAN +0                    EL318
00890              IF  CLAIM-IS-CLOSED                                  EL318
00891                  COMPUTE WS-REM-TERM =                            EL318
00892                        (CM-AH-ORIG-TERM - (WS-INC-MY - WS-EFF-MY))EL318
00893                  COMPUTE WS-TOT-REM-TERM =                        EL318
00894                         (WS-TOT-REM-TERM + WS-REM-TERM).          EL318
00895  0280-EXIT.                                                       EL318
00896      EXIT.                                                        EL318
00897                                                                   EL318
00898  0290-ACCUMULATE-POLICY.                                          EL318
00899                                                                   EL318
00900      MOVE PM-POLICY-EFF-DT       TO  DC-BIN-DATE-1.               EL318
00901      MOVE ' '                    TO  DC-OPTION-CODE.              EL318
00902      PERFORM 8500-DATE-CONVERSION.                                EL318
00903 *    MOVE DC-GREG-DATE-1-YMD     TO  WS-WORK-DATE.                   CL**5
00904      MOVE DC-GREG-DATE-CYMD      TO  WS-WORK-DATE.                   CL**5
00905 *    MOVE DC-ALPHA-CEN-N         TO WORK-C.                          CL**5
00906      COMPUTE WS-EFF-MY = (WORK-CY * 12) + WORK-M.                 EL318
00907                                                                   EL318
00908      IF CL-CLAIM-TYPE IS EQUAL TO AH-OVERRIDE-L1                  EL318
00909          IF CL-TOTAL-PAID-AMT IS GREATER THAN +0                  EL318
00910              IF CLAIM-IS-CLOSED                                   EL318
00911                  COMPUTE WS-REM-TERM =                            EL318
00912                      (PM-LOAN-TERM - (WS-INC-MY - WS-EFF-MY))     EL318
00913                  COMPUTE WS-TOT-REM-TERM =                        EL318
00914                      (WS-TOT-REM-TERM + WS-REM-TERM).             EL318
00915                                                                   EL318
00916  0290-EXIT.                                                       EL318
00917      EXIT.                                                        EL318
00918      EJECT                                                        EL318
00919  0300-PROCESS-CERTIFICATE.                                        EL318
00920      IF (HIR-RECORD-ID NOT = 'CM' AND 'PM')                       EL318
00921          GO TO 0400-PROCESS-TRAILERS.                             EL318
00922                                                                   EL318
00923      IF HIR-RECORD-ID IS EQUAL TO 'PM'                            EL318
00924          GO TO 0350-PROCESS-POLICY-RECORD.                        EL318
00925                                                                   EL318
00926      MOVE    HIR-CERTIFICATE-RECORD  TO  CERTIFICATE-MASTER.      EL318
00927      PERFORM 0280-ACCUMULATE-CERT   THRU 0280-EXIT.               EL318
00928      MOVE    SPACES                  TO  P-CM-BENEFIT-CD          EL318
00929                                          CLAS-LOOK.               EL318
00930      MOVE    ZEROS                   TO  P-CM-ORIG-TERM           EL318
00931                                          P-CM-BENEFIT-AMT.        EL318
00932                                                                   EL318
100518     IF  CL-CLAIM-TYPE = LIFE-OVERRIDE-L1 OR 'O'                  EL318
00934          IF  CM-LF-BENEFIT-CD = ZEROS                             EL318
00935              NEXT SENTENCE                                        EL318
00936          ELSE                                                     EL318
00937              MOVE    CM-LF-BENEFIT-CD    TO CLAS-LOOK             EL318
00938              MOVE    CM-LF-ORIG-TERM     TO P-CM-ORIG-TERM        EL318
00939              MOVE    CM-LF-BENEFIT-AMT   TO P-CM-BENEFIT-AMT      EL318
00940              PERFORM 3000-GET-LF-BENEFIT-CD                       EL318
00941                      VARYING CLAS-INDEXL FROM CLAS-STARTL         EL318
00942                      BY +1  UNTIL  CLAS-INDEXL GREATER CLAS-MAXL  EL318
00943      ELSE                                                         EL318
00944          IF  CM-AH-BENEFIT-CD = ZEROS                             EL318
00945              NEXT    SENTENCE                                     EL318
00946          ELSE                                                     EL318
00947              MOVE    CM-AH-BENEFIT-CD    TO CLAS-LOOK             EL318
00948              MOVE    CM-AH-ORIG-TERM     TO P-CM-ORIG-TERM        EL318
00949              MOVE    CM-AH-BENEFIT-AMT   TO P-CM-BENEFIT-AMT      EL318
00950              PERFORM 3100-GET-AH-BENEFIT-CD                       EL318
00951                      VARYING CLAS-INDEXA FROM CLAS-STARTA         EL318
00952                      BY +1  UNTIL  CLAS-INDEXA GREATER CLAS-MAXA. EL318
00953                                                                   EL318
00954      IF CERT-WAS-CREATED-FOR-CLAIM                                EL318
00955          ADD +1   TO WS-CERT-CREAT-BY-CL-CNTR                     EL318
00956      ELSE                                                         EL318
00957          ADD +1   TO WS-CERT-ONLINE-CNTR.                         EL318
00958                                                                   EL318
00959      MOVE    WS-DETAIL1   TO  PRT.                                EL318
00960      PERFORM WRITE-A-LINE.                                        EL318
00961      GO                   TO 0100-PROCESS-HISTORY-FILE.           EL318
00962                                                                   EL318
00963      EJECT                                                        EL318
00964  0350-PROCESS-POLICY-RECORD.                                      EL318
00965                                                                   EL318
pemuni*    MOVE HIR-POLICY-RECORD      TO  POLICY-MASTER.               EL318
00967      PERFORM 0290-ACCUMULATE-POLICY THRU 0290-EXIT.               EL318
00968      MOVE SPACES                 TO  P-CM-BENEFIT-CD.             EL318
00969      MOVE ZEROS                  TO  P-CM-ORIG-TERM               EL318
00970                                      P-CM-BENEFIT-AMT.            EL318
00971                                                                   EL318
00972      IF PM-INS-PLAN-CD IS NOT EQUAL TO ZEROS                      EL318
100518         IF CL-CLAIM-TYPE IS EQUAL TO LIFE-OVERRIDE-L1 OR 'O'     EL318
00974              PERFORM 3500-READ-EMPLAN THRU 3500-EXIT              EL318
00975              MOVE PM-LOAN-TERM          TO  P-CM-ORIG-TERM        EL318
00976              MOVE PM-INS-TOTAL-BENEFIT  TO  P-CM-BENEFIT-AMT      EL318
00977          ELSE                                                     EL318
00978              PERFORM 3500-READ-EMPLAN THRU 3500-EXIT              EL318
00979              MOVE PM-LOAN-TERM          TO  P-CM-ORIG-TERM        EL318
00980              MOVE PM-INS-MONTH-BENEFIT  TO  P-CM-BENEFIT-AMT.     EL318
00981                                                                   EL318
00982      ADD +1                      TO  WS-CERT-ONLINE-CNTR.         EL318
00983                                                                   EL318
00984      MOVE WS-DETAIL1             TO  PRT.                         EL318
00985      PERFORM WRITE-A-LINE.                                        EL318
00986      GO TO 0100-PROCESS-HISTORY-FILE.                             EL318
00987                                                                   EL318
00988      EJECT                                                        EL318
00989  0400-PROCESS-TRAILERS.                                           EL318
00990      IF HIR-RECORD-ID NOT = 'AT'                                  EL318
00991          GO TO 0500-PROCESS-ARCHIVE.                              EL318
00992                                                                   EL318
00993      ADD +1  TO  WS-CL-TOTAL-TRLR-CNTR.                           EL318
00994      MOVE    HIR-ACTIVITY-TRAILER-RECORD TO  ACTIVITY-TRAILERS.   EL318
00995      MOVE    AT-RECORDED-DT              TO  DC-BIN-DATE-1.       EL318
00996      MOVE    SPACES                      TO  DC-OPTION-CODE.      EL318
00997      PERFORM 8500-DATE-CONVERSION.                                EL318
00998      MOVE    DC-GREG-DATE-1-MDY          TO  WS-PMT-DT.           EL318
00999                                                                   EL318
01000      IF  RESERVE-EXPENSE-TR                                       EL318
01001          ADD +1 TO WS-CL-RESERVE-TRLR-CNTR                        EL318
01002          GO     TO 0100-PROCESS-HISTORY-FILE.                     EL318
01003                                                                   EL318
01004      IF WS-AT-RECORDED-DT NOT EQUAL SPACES                        EL318
01005          GO TO 0400-PROCESS-CONTINUE.                             EL318
01006                                                                   EL318
01007      MOVE AT-RECORDED-DT          TO WS-AT-RECORDED-DT.           EL318
01008                                                                   EL318
01009      MOVE WS-CL-ESTABLISH-DT      TO DC-BIN-DATE-1.               EL318
01010      MOVE WS-AT-RECORDED-DT       TO DC-BIN-DATE-2.               EL318
01011      MOVE '1'                     TO DC-OPTION-CODE.              EL318
01012      PERFORM 8500-DATE-CONVERSION.                                EL318
01013      IF NO-CONVERSION-ERROR                                       EL318
01014         ADD DC-ELAPSED-DAYS       TO WS-AVERAGE-DAYS.             EL318
01015                                                                   EL318
01016  0400-PROCESS-CONTINUE.                                           EL318
01017      MOVE AT-RECORDED-DT  TO  WS-AT-RECORDED-DT.                  EL318
01018                                                                   EL318
01019      IF  CORRESPONDENCE-TR                                        EL318
01020          ADD +1  TO WS-CL-COR-TRLR-CNTR                           EL318
01021          GO      TO 0100-PROCESS-HISTORY-FILE.                    EL318
01022                                                                   EL318
01023      IF NOT PAYMENT-TR                                            EL318
01024          GO TO 0450-NOT-PAYMENT.                                  EL318
01025                                                                   EL318
01026      ADD +1                  TO WS-CL-PMT-TRLR-CNTR.              EL318
01027                                                                   EL318
01028      IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                            EL318
01029          ADD +1              TO WS-AVERAGE-PAY-AH                 EL318
01030       ELSE                                                        EL318
01031          ADD +1              TO WS-AVERAGE-PAY-LF.                EL318
01032                                                                   EL318
01033      MOVE    AT-RECORDED-BY  TO  WS-PROCESSOR.                    EL318
01034      PERFORM 2100-PROCESSOR-LOOKUP.                               EL318
01035                                                                   EL318
01036      IF WS-PMT-YR NOT = RUN-YR  OR                                EL318
01037         WS-PMT-MO NOT = RUN-MO                                    EL318
01038           GO TO  0440-NOT-THIS-MONTH.                             EL318
01039                                                                   EL318
01040      ADD +1  TO  P-TBL-PMT-THIS-MO-CNTR (PROC-INDEX).             EL318
01041                                                                   EL318
01042      IF PAYMENT-WAS-FORCED                                        EL318
01043          ADD +1  TO  P-TBL-PMT-FORCED-CNTR  (PROC-INDEX).         EL318
01044                                                                   EL318
01045  0440-NOT-THIS-MONTH.                                             EL318
01046      GO TO 0100-PROCESS-HISTORY-FILE.                             EL318
01047                                                                   EL318
01048  0450-NOT-PAYMENT.                                                EL318
01049      IF WS-PMT-YR = RUN-YR  AND                                   EL318
01050         WS-PMT-MO = RUN-MO                                        EL318
01051            ADD +1  TO P-TBL-OTHER-ACT-CNTR (PROC-INDEX).          EL318
01052                                                                   EL318
01053      IF  AUTO-PROMPT-TR                                           EL318
01054          ADD +1  TO WS-CL-REM-TRLR-CNTR                           EL318
01055          GO TO 0100-PROCESS-HISTORY-FILE.                         EL318
01056                                                                   EL318
01057      IF GENERAL-INFO-TR                                           EL318
01058          ADD +1  TO WS-CL-NOT-TRLR-CNTR                           EL318
01059          GO TO 0100-PROCESS-HISTORY-FILE.                         EL318
01060                                                                   EL318
01061      IF INCURRED-CHG-TR                                           EL318
01062          ADD +1  TO WS-CL-CHG-TRLR-CNTR                           EL318
01063          GO TO 0100-PROCESS-HISTORY-FILE.                         EL318
01064                                                                   EL318
01065      IF ADDRESS-TR                                                EL318
01066          ADD +1  TO WS-CL-ADD-TRLR-CNTR                           EL318
01067          GO TO 0100-PROCESS-HISTORY-FILE.                         EL318
01068                                                                   EL318
01069      IF AUTO-PAY-TR                                               EL318
01070          ADD +1  TO WS-CL-AUP-TRLR-CNTR                           EL318
01071          GO TO 0100-PROCESS-HISTORY-FILE.                         EL318
01072                                                                   EL318
01073      IF DENIAL-TR                                                 EL318
01074          ADD +1  TO WS-CL-DEN-TRLR-CNTR                           EL318
01075          GO TO 0100-PROCESS-HISTORY-FILE.                         EL318
01076                                                                   EL318
01077      IF FORM-CONTROL-TR                                           EL318
01078          ADD +1  TO WS-CL-FORM-TRLR-CNTR.                         EL318
01079                                                                   EL318
01080      GO TO 0100-PROCESS-HISTORY-FILE.                             EL318
01081      EJECT                                                        EL318
01082  0500-PROCESS-ARCHIVE.                                            EL318
01083      IF HIR-RECORD-ID NOT = 'LA'                                  EL318
01084          GO TO 0700-PROCESS-HISTORY-FILE.                         EL318
01085                                                                   EL318
01086      MOVE HIR-LETTER-ARCHIVE-RECORD TO  LETTER-ARCHIVE.           EL318
01087                                                                   EL318
01088      IF LA-ARCHIVE-NO NOT = WS-ARCHIVE-NO                         EL318
01089         ADD  +1  TO WS-LET-IN-ARCH-CNTR.                          EL318
01090                                                                   EL318
01091      MOVE LA-ARCHIVE-NO TO WS-ARCHIVE-NO.                         EL318
01092      ADD  +1            TO WS-REC-IN-ARCH-CNTR.                   EL318
01093      GO TO 0100-PROCESS-HISTORY-FILE.                             EL318
01094                                                                   EL318
01095  0700-PROCESS-HISTORY-FILE.                                       EL318
01096      MOVE 'INVALID RECORD TYPE ON HISTORY FILE'                   EL318
01097                                  TO  WS-ABEND-MESSAGE.            EL318
01098      GO TO ABEND-PGM.                                             EL318
01099                                                                   EL318
01100  0900-CLOSE-HIST.                                                 EL318
01101      PERFORM CLOSE-FILES.                                         EL318
01102                                                                   EL318
01103      MOVE SPACES       TO WS-HEADING1                             EL318
01104                           WS-HEADING5                             EL318
01105                           WS-HEADING4                             EL318
01106                           WS-HEADING6.                            EL318
01107      MOVE WS-HEADING1A TO WS-HEADING1.                            EL318
01108      MOVE WS-HEADING4A TO WS-HEADING4.                            EL318
01109      MOVE WS-HEADING5A TO WS-HEADING5.                            EL318
01110      MOVE WS-HEADING6A TO WS-HEADING6.                            EL318
01111      MOVE +0           TO WS-PAGE.                                EL318
01112      MOVE +99          TO WS-LINE-COUNT.                          EL318
01113                                                                   EL318
01114  0900-EXIT.                                                       EL318
01115      EXIT.                                                        EL318
01116      EJECT                                                        EL318
01117                                                                   EL318
01118  1000-PRINT-PURGED-REPORT SECTION.                                EL318
01119                                                                   EL318
01120      IF  WS-TOT-RECORDS-RELEASED GREATER ZERO                     EL318
01121          NEXT     SENTENCE                                        EL318
01122      ELSE                                                         EL318
01123          DISPLAY 'EL318  NO PURGED CLAIMS ON HISTORY'             EL318
01124          GO      TO 1900-PRINT-PURGED-TOTALS.                     EL318
01125                                                                   EL318
01126      RETURN SORT-FILE                                             EL318
01127          AT END                                                   EL318
01128              GO TO 1900-PRINT-PURGED-TOTALS.                      EL318
01129                                                                   EL318
01130      ADD     +1                           TO WS-TOT-PURGED-CLAIMS.EL318
01131      MOVE    SR-REST-OF-RECORD            TO CLAIM-MASTER.        EL318
01132      MOVE    SPACES                       TO WS-DETAIL1A.         EL318
01133      MOVE    CL-CARRIER                   TO PR-CARR.             EL318
01134      MOVE    CL-CLAIM-TYPE                TO PR-TYPE.             EL318
01135      MOVE    CL-CLAIM-NO                  TO PR-CLAIM-NO.         EL318
01136      MOVE    CL-CERT-NO                   TO PR-CERT-NO.          EL318
01137      MOVE    CL-CERT-ACCOUNT              TO PR-CERT-ACCOUNT.     EL318
01138      MOVE    CL-CERT-STATE                TO PR-CERT-STATE.       EL318
01139      MOVE    CL-CERT-GROUPING             TO PR-CERT-GROUPING.    EL318
01140      MOVE    CL-INSURED-LAST-NAME         TO PR-LAST-NAME.        EL318
01141      MOVE    CL-INSURED-1ST-NAME          TO PR-FIRST-NAME.       EL318
01142      MOVE    CL-INSURED-MID-INIT          TO PR-MID-INIT.         EL318
01143      MOVE    CL-MICROFILM-NO              TO PR-MICROFILM-NO.     EL318
01144      MOVE    CL-LAST-MAINT-DT             TO DC-BIN-DATE-1.       EL318
01145      MOVE    SPACE                        TO DC-OPTION-CODE.      EL318
01146      PERFORM 8500-DATE-CONVERSION.                                EL318
01147      MOVE    DC-GREG-DATE-1-EDIT          TO PR-MAINT-DATE.       EL318
01148      MOVE    WS-DETAIL1A                  TO  PRT.                EL318
01149      PERFORM WRITE-A-LINE.                                        EL318
01150      GO TO 1000-PRINT-PURGED-REPORT.                              EL318
01151                                                                   EL318
01152  1900-PRINT-PURGED-TOTALS.                                        EL318
01153      MOVE    SPACES               TO  WS-TOTAL-1A.                EL318
01154      MOVE    '-'                  TO  WS-T1-CC.                   EL318
01155      MOVE    WS-TOT-PURGED-CLAIMS TO PR-TOT-PURGED-CLAIMS.        EL318
01156      MOVE    '***  TOTAL PURGED CLAIMS  ***    -' TO              EL318
01157              PR-DESC-PURGED-CLAIMS.                               EL318
01158      MOVE    WS-TOTAL-1A          TO  PRT.                        EL318
01159      PERFORM WRITE-A-LINE.                                        EL318
01160      PERFORM CFS-050 THRU CFS-050-EXIT.                           EL318
01161                                                                   EL318
01162  1900-EXIT.                                                       EL318
01163      EXIT.                                                        EL318
01164      EJECT                                                        EL318
01165                                                                   EL318
01166  2100-PROCESSOR-LOOKUP SECTION.                                   EL318
01167                                                                   EL318
01168      IF WS-PROCESSOR  = P-TBL-PROC-NO (PROC-INDEX)                EL318
01169          GO TO 2100-EXIT.                                         EL318
01170                                                                   EL318
01171      SET PROC-INDEX TO +1.                                        EL318
01172                                                                   EL318
01173  2100-LOOKUP.                                                     EL318
01174      IF WS-PROCESSOR  = P-TBL-PROC-NO (PROC-INDEX)                EL318
01175          GO TO 2100-EXIT.                                         EL318
01176                                                                   EL318
01177      IF P-TBL-PROC-NO (PROC-INDEX) = LOW-VALUES                   EL318
01178          MOVE  WS-PROCESSOR TO P-TBL-PROC-NO (PROC-INDEX)         EL318
01179          GO                 TO 2100-EXIT.                         EL318
01180                                                                   EL318
01181      IF PROC-INDEX LESS THAN PROCESSOR-TABLE-MAX                  EL318
01182          SET PROC-INDEX UP BY +1                                  EL318
01183          GO  TO 2100-LOOKUP.                                      EL318
01184                                                                   EL318
01185      MOVE   'PROCESSOR TABLE LIMIT EXCEDED' TO  WS-ABEND-MESSAGE.
           DISPLAY ' PROCESSOR ' WS-PROCESSOR
01186      GO TO ABEND-PGM.                                             EL318
01187                                                                   EL318
01188  2100-EXIT.                                                       EL318
01189       EXIT.                                                       EL318
01190      EJECT                                                        EL318
01191                                                                   EL318
01192  3000-GET-LF-BENEFIT-CD SECTION.                                  EL318
01193                                                                   EL318
01194      IF CLAS-LOOK = CLAS-I-BEN (CLAS-INDEXL)                      EL318
01195          MOVE CLAS-I-AB3 (CLAS-INDEXL) TO P-CM-BENEFIT-CD         EL318
01196          MOVE CLAS-MAXL                TO CLAS-INDEXL.            EL318
01197                                                                   EL318
01198  3000-EXIT.                                                       EL318
01199      EXIT.                                                        EL318
01200                                                                   EL318
01201  3100-GET-AH-BENEFIT-CD SECTION.                                  EL318
01202                                                                   EL318
01203      IF CLAS-LOOK = CLAS-I-BEN (CLAS-INDEXA)                      EL318
01204          MOVE CLAS-I-AB3 (CLAS-INDEXA) TO P-CM-BENEFIT-CD         EL318
01205          MOVE CLAS-MAXA                TO CLAS-INDEXA.            EL318
01206                                                                   EL318
01207  3100-EXIT.                                                       EL318
01208      EXIT.                                                        EL318
01209     EJECT                                                         EL318
01210  3500-READ-EMPLAN SECTION.                                        EL318
01211                                                                   EL318
01212      MOVE PM-COMPANY-CD              TO  PP-COMPANY-CD.           EL318
01213      MOVE PM-CARRIER                 TO  PP-CARRIER.              EL318
01214      MOVE PM-GROUPING                TO  PP-GROUPING.             EL318
01215      MOVE PM-STATE                   TO  PP-STATE.                EL318
01216      MOVE PM-PRODUCER                TO  PP-PRODUCER.             EL318
01217      MOVE PM-INS-PLAN-CD             TO  PP-PLAN-CODE.            EL318
01218      MOVE PM-INS-PLAN-REVISION       TO  PP-PLAN-REVISION.        EL318
01219                                                                   EL318
01220      READ MPPLAN.                                                 EL318
01221                                                                   EL318
01222      IF MPPLAN-FILE-STATUS IS EQUAL TO '23'                       EL318
01223          MOVE SPACES                 TO  P-CM-BENEFIT-CD          EL318
01224          GO TO 3500-EXIT.                                         EL318
01225                                                                   EL318
01226      IF MPPLAN-FILE-STATUS IS NOT EQUAL TO '00'                   EL318
01227          MOVE 'ERROR OCCURED READ - MPPLAN'                       EL318
01228                                      TO  WS-ABEND-MESSAGE         EL318
01229          MOVE MPPLAN-FILE-STATUS     TO  WS-ABEND-FILE-STATUS     EL318
01230          GO TO ABEND-PGM.                                         EL318
01231                                                                   EL318
01232      MOVE PP-PLAN-ABBREV             TO  P-CM-BENEFIT-CD.         EL318
01233                                                                   EL318
01234  3500-EXIT.                                                       EL318
01235      EXIT.                                                        EL318
01236     EJECT                                                         EL318
01237  4000-RELEASE-SORT-RECORD SECTION.                                EL318
01238                                                                   EL318
01239      MOVE    CL-INSURED-LAST-NAME   TO  SR-NAME.                  EL318
01240      MOVE    CL-CLAIM-NO            TO  SR-CLAIM-NO.              EL318
01241      MOVE    CL-CARRIER             TO  SR-CARRIER.               EL318
01242      MOVE    CL-CERT-NO             TO  SR-CERT-NO.               EL318
01243      MOVE    HIR-CLAIM-RECORD       TO  SR-REST-OF-RECORD.        EL318
01244      RELEASE SORT-RECORD.                                         EL318
01245      ADD     +1                     TO WS-TOT-RECORDS-RELEASED.   EL318
01246                                                                   EL318
01247  4000-EXIT.                                                       EL318
01248      EXIT.                                                        EL318
01249      EJECT                                                        EL318
01250                                                                   EL318
01251  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL318
01252                                                                   EL318
01253  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL318
01254                                                                   EL318
01255  WRITE-HEADINGS SECTION.                                          EL318
01256  WHS-010.                                                         EL318
01257      IF  WS-H2-DATE EQUAL SPACES                                  EL318
01258          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL318
01259          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL318
01260          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL318
01261                                                                   EL318
01262      ADD +1  TO  WS-PAGE.                                         EL318
01263      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL318
01264      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL318
01265      MOVE ZERO                   TO  WS-LINE-COUNT.               EL318
01266                                                                   EL318
01267      MOVE WS-HEADING1            TO  PRT.                         EL318
01268      MOVE '1'                    TO  X.                           EL318
01269      PERFORM WRITE-PRINTER.                                       EL318
01270                                                                   EL318
01271      MOVE WS-HEADING2            TO  PRT.                         EL318
01272      MOVE ' '                    TO  X.                           EL318
01273      PERFORM WRITE-PRINTER.                                       EL318
01274                                                                   EL318
01275      MOVE WS-HEADING3            TO  PRT.                         EL318
01276      MOVE ' '                    TO  X.                           EL318
01277      PERFORM WRITE-PRINTER.                                       EL318
01278                                                                   EL318
01279      MOVE WS-HEADING4            TO  PRT.                         EL318
01280      MOVE ' '                    TO  X.                           EL318
01281      PERFORM WRITE-PRINTER.                                       EL318
01282                                                                   EL318
01283      MOVE    WS-HEADING5            TO  PRT.                      EL318
01284      PERFORM WRITE-PRINTER.                                       EL318
01285      MOVE    WS-HEADING6            TO  PRT.                      EL318
01286      PERFORM WRITE-PRINTER.                                       EL318
01287      MOVE    +8                     TO  WS-LINE-COUNT.            EL318
01288                                                                   EL318
01289  WHS-020. COPY ELCWHS2 SUPPRESS.                                  EL318
01290                                                                   EL318
01291  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL318
01292                                                                   EL318
01293  WPS-020.                                                         EL318
01294                                                                   EL318
01295      IF DTE-FICH NOT = SPACE AND                                  EL318
01296          FICH-OPEN   = SPACE                                      EL318
01297          MOVE 'X' TO FICH-OPEN                                    EL318
01298          OPEN OUTPUT FICH.                                        EL318
01299                                                                   EL318
01300      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL318
01301          IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)      EL318
01302              OPEN I-O ELREPT                                      EL318
01303              IF DTE-F-1 NOT = ZERO AND                            EL318
01304                 DTE-VSAM-FLAGS NOT = '97'                         EL318
01305                  MOVE DTE-VSAM-FLAGS  TO  WS-ABEND-FILE-STATUS    EL318
01306                  MOVE 'ERROR OCCURED OPEN - ELREPT'               EL318
01307                                  TO  WS-ABEND-MESSAGE             EL318
01308                  GO TO ABEND-PGM                                  EL318
01309              ELSE                                                 EL318
01310                  MOVE '1'                   TO REPT-OPEN          EL318
01311                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL318
01312                  MOVE '1'                   TO RF-RECORD-TYPE     EL318
01313                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL318
01314                  MOVE ZERO                  TO RF-LINE-NUMBER     EL318
01315                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL318
01316                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL318
01317                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL318
01318                  MOVE '2'                   TO RF-RECORD-TYPE     EL318
01319                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL318
01320                  MOVE ZERO                  TO RF-LINE-NUMBER     EL318
01321                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL318
01322                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL318
01323                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL318
01324                  MOVE '1'                   TO RF-RECORD-TYPE     EL318
01325                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL318
01326                  MOVE SPACES                TO RF-REPORT-LINE-133.EL318
01327                                                                   EL318
01328      IF DTE-ABEND-CD-1 = '81' AND                                 EL318
01329         DTE-PRT-OPT    = 'S'                                      EL318
01330          MOVE +0302  TO WS-RETURN-CODE                            EL318
01331          GO TO ABEND-PGM.                                         EL318
01332                                                                   EL318
01333      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL318
01334          MOVE X      TO RF-CTL-CHAR-133                           EL318
01335          MOVE P-DATA TO RF-DATA-133                               EL318
01336              IF DTE-ABEND-CD-1 = SPACES                           EL318
01337                  ADD +1 TO DTE-TOT-LINES                          EL318
01338                  MOVE DTE-TOT-LINES TO RF-LINE-NUMBER             EL318
01339                  WRITE REPORT-SAVE-FILE                           EL318
01340                      INVALID KEY                                  EL318
01341                          MOVE '88' TO DTE-ABEND-CD-1              EL318
01342                          CLOSE ELREPT                             EL318
01343                          MOVE SPACE TO REPT-OPEN.                 EL318
01344                                                                   EL318
01345      IF DTE-FICH NOT = SPACE                                      EL318
01346          MOVE X TO P-CTL                                          EL318
01347          WRITE FICH-REC FROM PRT.                                 EL318
01348                                                                   EL318
01349      IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'                           EL318
01350          MOVE X TO P-CTL                                          EL318
01351          WRITE PRT.                                               EL318
01352                                                                   EL318
01353      GO TO DTE-PRINT-EXIT.                                        EL318
01354                                                                   EL318
01355  DTE-REPORT-DELETE.                                               EL318
01356      IF DTE-F-1 NOT = ZERO                                        EL318
01357          MOVE ZERO TO DTE-VSAM-FLAGS                              EL318
01358          GO TO DTE-DELETE-EXIT.                                   EL318
01359                                                                   EL318
01360      READ ELREPT   NEXT RECORD                                    EL318
01361            AT END   GO TO DTE-DELETE-EXIT.                        EL318
01362                                                                   EL318
01363      IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND                EL318
01364         OLC-REPORT-NAME       = RF-REPORT-ID                      EL318
01365          DELETE ELREPT RECORD                                     EL318
01366          GO TO DTE-REPORT-DELETE.                                 EL318
01367                                                                   EL318
01368  DTE-DELETE-EXIT.                                                 EL318
01369      EXIT.                                                        EL318
01370                                                                   EL318
01371  DTE-PRINT-EXIT.                                                  EL318
01372      EXIT.                                                        EL318
01373 ******************************************************************EL318
01374                                                                   EL318
01375      EJECT                                                        EL318
01376  OPEN-FILES SECTION.                                              EL318
01377                                                                   EL318
01378  OFS-010.                                                         EL318
01379      OPEN INPUT CLAIM-HIST                                        EL318
unix  *               MPPLAN                                            EL318
01381           OUTPUT PRNTR.                                           EL318
01382                                                                   EL318
01383      IF (MPPLAN-FILE-STATUS IS EQUAL TO '00' OR '97')             EL318
01384          NEXT SENTENCE                                            EL318
01385      ELSE                                                         EL318
01386          MOVE 'ERROR OCCURED OPEN - MPPLAN'                       EL318
01387                                      TO  WS-ABEND-MESSAGE         EL318
01388          MOVE MPPLAN-FILE-STATUS     TO  WS-ABEND-FILE-STATUS     EL318
01389          GO TO ABEND-PGM.                                         EL318
01390                                                                   EL318
01391      MOVE RUN-MO      TO P-CURR-MO.                               EL318
01392      MOVE RUN-CC      TO P-CURR-CC-1                                 CL**3
01393                          P-CURR-CC-2                                 CL**4
01394                          P-CURR-CC-3.                                CL**4
01395      MOVE RUN-YR      TO P-CURR-YR-1                              EL318
01396                          P-CURR-YR-2                              EL318
01397                          P-CURR-YR-3.                             EL318
01398                                                                   EL318
01399      SUBTRACT 1            FROM RUN-YR GIVING WS-LAST-YR.            CL**6
01400      MOVE RUN-CC      TO P-LAST-CC-1                                 CL**3
01401                                                                      CL**3
01402      IF WS-LAST-YR  <  0                                          EL318
01403         ADD +100  TO  WS-LAST-YR                                  EL318
01404         SUBTRACT 1 FROM P-LAST-CC-1.                                 CL**7
01405                                                                      CL**3
01406      MOVE     WS-LAST-YR    TO  P-LAST-YR-1.                      EL318
01407                                                                   EL318
01408      IF P-CURR-MO = '01'                                          EL318
01409         MOVE  WS-LAST-YR    TO  P-CURR-YR-3                          CL**3
01410         SUBTRACT 1 FROM P-CURR-CC-3.                                 CL**3
01411                                                                   EL318
01412      MOVE BIN-RUN-DATE      TO  WS-RUN-DT.                        EL318
01413                                                                   EL318
01414      IF RUN-MO =   1                                              EL318
01415          MOVE     '12' TO WS-LAST-MO                              EL318
01416        ELSE                                                       EL318
01417          SUBTRACT +1 FROM RUN-MO  GIVING WS-LAST-MO.              EL318
01418                                                                   EL318
01419      MOVE WS-LAST-MO   TO  P-LAST-MO.                             EL318
01420      MOVE LOW-VALUES   TO  WS-PROCESSOR-TABLE.                    EL318
01421      SET  PROC-INDEX   TO  +1.                                    EL318
01422                                                                   EL318
01423  OFS-020.                                                         EL318
01424      MOVE ZERO    TO  P-TBL-OPEN-CLAIMS-CNTR   (PROC-INDEX)       EL318
01425                       P-TBL-EST-THIS-MO-CNTR   (PROC-INDEX)       EL318
01426                       P-TBL-PMT-THIS-MO-CNTR   (PROC-INDEX)       EL318
01427                       P-TBL-PMT-FORCED-CNTR    (PROC-INDEX)       EL318
01428                       P-TBL-CLO-THIS-MO-CNTR   (PROC-INDEX)       EL318
01429                       P-TBL-OTHER-ACT-CNTR     (PROC-INDEX).      EL318
01430                                                                   EL318
01431      IF PROC-INDEX LESS THAN PROCESSOR-TABLE-MAX                  EL318
01432          SET PROC-INDEX UP BY +1                                  EL318
01433          GO  TO OFS-020.                                          EL318
01434                                                                   EL318
01435  OFS-EXIT.                                                        EL318
01436      EXIT.                                                        EL318
01437                                                                   EL318
01438      EJECT                                                        EL318
01439  CLOSE-FILES SECTION.                                             EL318
01440                                                                   EL318
01441  CFS-010.                                                         EL318
01442      SET  PROC-INDEX                                              EL318
01443           TOTAL-INDEX TO +1.                                      EL318
01444      MOVE SPACES                 TO  WS-DETAIL1.                  EL318
01445      MOVE +99                    TO  WS-LINE-COUNT.               EL318
01446      MOVE WS-HEADING4B           TO  WS-HEADING4.                 EL318
01447      MOVE WS-HEADING5B           TO  WS-HEADING5.                 EL318
01448      MOVE SPACES                 TO  WS-HEADING6.                 EL318
01449                                                                   EL318
01450      IF WS-TOT-REM-TERM GREATER THAN +0                           EL318
01451         COMPUTE WS-TOT-FACTOR =                                   EL318
01452                 (WS-TOT-CLAIM-DURA / WS-TOT-REM-TERM).            EL318
01453                                                                   EL318
01454      IF WS-AH-CLAIM-COUNT GREATER THAN +0                         EL318
01455         COMPUTE WS-AVG-CLAIM-DURA =                               EL318
01456                 (WS-TOT-CLAIM-DURA / WS-AH-CLAIM-COUNT).          EL318
01457                                                                   EL318
01458      MOVE WS-AVG-CLAIM-DURA  TO WS-AVERAGE-DURATION.              EL318
01459                                                                   EL318
01460      IF DTE-CLIENT EQUAL 'FIA'                                    EL318
01461         COMPUTE WS-CL-AH-TOTAL-MSTR-CNTR =                        EL318
01462            (WS-CL-OPEN-CL-AH-CNTR + WS-CL-CLOSED-CL-AH-CNTR )     EL318
01463         COMPUTE WS-CL-TOTAL-MSTR-CNTR =                           EL318
01464           (WS-CL-AH-TOTAL-MSTR-CNTR + WS-CL-LF-TOTAL-MSTR-CNTR).  EL318
01465                                                                   EL318
01466      IF WS-CL-TOTAL-MSTR-CNTR = ZERO                              EL318
01467         GO TO CFS-020.                                            EL318
01468                                                                   EL318
01469      COMPUTE WS-AVERAGE-DAYS =                                    EL318
01470              WS-AVERAGE-DAYS / WS-CL-TOTAL-MSTR-CNTR.             EL318
01471                                                                   EL318
01472      COMPUTE WS-AVERAGE-PAY-AH =                                  EL318
01473              WS-AVERAGE-PAY-AH / WS-CL-TOTAL-MSTR-CNTR.           EL318
01474                                                                   EL318
01475      COMPUTE WS-AVERAGE-PAY-LF =                                  EL318
01476              WS-AVERAGE-PAY-LF / WS-CL-TOTAL-MSTR-CNTR.           EL318
01477                                                                   EL318
01478  CFS-020.                                                         EL318
01479      MOVE SPACES                 TO  WS-TOTAL-LINE1.              EL318
01480                                                                   EL318
01481      IF  TOTAL-INDEX NOT GREATER THAN TOTAL-INDEX-MAX             EL318
01482          MOVE WS-TOTAL-DESCRIPTION (TOTAL-INDEX)  TO  WS-T1-DESC  EL318
01483          IF  TOTAL-INDEX NOT = +12 AND +28 AND +29                EL318
01484              MOVE WS-TOTAL-COUNT (TOTAL-INDEX) TO  WS-T1-COUNT.   EL318
01485                                                                   EL318
01486  CFS-021-NEXT-PROC.                                               EL318
01487      IF  PROC-INDEX NOT GREATER THAN PROCESSOR-TABLE-MAX          EL318
01488          IF P-TBL-PROC-NO (PROC-INDEX) NOT = LOW-VALUES           EL318
01489             IF (P-TBL-OPEN-CLAIMS-CNTR (PROC-INDEX) NOT = ZEROS OREL318
01490                 P-TBL-EST-THIS-MO-CNTR (PROC-INDEX) NOT = ZEROS OREL318
01491                 P-TBL-PMT-FORCED-CNTR  (PROC-INDEX) NOT = ZEROS OREL318
01492                 P-TBL-PMT-THIS-MO-CNTR (PROC-INDEX) NOT = ZEROS OREL318
01493                 P-TBL-CLO-THIS-MO-CNTR (PROC-INDEX) NOT = ZEROS OREL318
01494                 P-TBL-OTHER-ACT-CNTR   (PROC-INDEX) NOT = ZEROS)  EL318
01495                 MOVE P-TBL-PROC-NO (PROC-INDEX) TO WS-T1-PROCESSOREL318
01496                 MOVE P-TBL-OPEN-CLAIMS-CNTR (PROC-INDEX)          EL318
01497                                     TO  WS-T1-CLAIMS-CNTR         EL318
01498                 MOVE P-TBL-EST-THIS-MO-CNTR (PROC-INDEX)          EL318
01499                                     TO  WS-T1-ESTABLISHED         EL318
01500                 MOVE P-TBL-PMT-FORCED-CNTR  (PROC-INDEX)          EL318
01501                                     TO  WS-T1-FORCED              EL318
01502                 MOVE P-TBL-PMT-THIS-MO-CNTR (PROC-INDEX)          EL318
01503                                     TO  WS-T1-PAYMENTS-MADE       EL318
01504                 MOVE P-TBL-CLO-THIS-MO-CNTR (PROC-INDEX)          EL318
01505                                     TO  WS-T1-CLOSED              EL318
01506                 MOVE P-TBL-OTHER-ACT-CNTR   (PROC-INDEX)          EL318
01507                                     TO  WS-T1-OTHER               EL318
01508              ELSE                                                 EL318
01509                 SET PROC-INDEX UP BY 1                            EL318
01510                 GO TO CFS-021-NEXT-PROC                           EL318
01511          ELSE                                                     EL318
01512              SET PROC-INDEX TO PROCESSOR-TABLE-MAX.               EL318
01513                                                                   EL318
01514      IF   PROC-INDEX NOT GREATER THAN PROCESSOR-TABLE-MAX         EL318
01515        OR TOTAL-INDEX NOT GREATER THAN TOTAL-INDEX-MAX            EL318
01516           MOVE WS-TOTAL-LINE1 TO  PRT                             EL318
01517           PERFORM WRITE-A-LINE.                                   EL318
01518                                                                   EL318
01519      IF    PROC-INDEX NOT LESS THAN PROCESSOR-TABLE-MAX           EL318
01520        AND TOTAL-INDEX NOT LESS THAN TOTAL-INDEX-MAX              EL318
01521            CLOSE CLAIM-HIST                                       EL318
01522            GO TO CFS-CLOSE-EMPLAN.                                EL318
01523                                                                   EL318
01524      SET PROC-INDEX                                               EL318
01525          TOTAL-INDEX UP BY +1.                                    EL318
01526                                                                   EL318
01527      GO TO CFS-020.                                               EL318
01528                                                                   EL318
01529  CFS-050. COPY ELCPRTCX SUPPRESS.                                 EL318
01530                                                                   EL318
01531      CLOSE PRNTR.                                                 EL318
01532                                                                   EL318
01533  CFS-050-EXIT.                                                    EL318
01534      EXIT.                                                        EL318
01535                                                                   EL318
01536  CFS-CLOSE-EMPLAN.                                                EL318
01537                                                                   EL318
unix  *    CLOSE MPPLAN.                                                EL318
01539                                                                   EL318
01540      IF MPPLAN-FILE-STATUS IS EQUAL TO '00'                       EL318
01541          NEXT SENTENCE                                            EL318
01542      ELSE                                                         EL318
01543          MOVE 'ERROR OCCURED CLOSE - EMPLAN'                      EL318
01544                                      TO  WS-ABEND-MESSAGE         EL318
01545          MOVE MPPLAN-FILE-STATUS     TO  WS-ABEND-FILE-STATUS     EL318
01546          GO TO ABEND-PGM.                                         EL318
01547                                                                   EL318
01548  CFS-EXIT.                                                        EL318
01549      EXIT.                                                        EL318
01550                                                                   EL318
01551  ABEND-PGM SECTION. COPY ELCABEND.                                EL318
01552                                                                   EL318
