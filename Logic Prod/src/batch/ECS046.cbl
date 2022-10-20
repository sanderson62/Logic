00001  IDENTIFICATION DIVISION.                                         03/09/98
00002                                                                   ECS046
00003  PROGRAM-ID.                ECS046.                                  LV009
00004 *              PROGRAM CONVERTED BY                               ECS046
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS046
00006 *              CONVERSION DATE 02/20/96 19:03:05.                 ECS046
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS046
00008 *                           VMOD=2.014.                           ECS046
00009                                                                   ECS046
00010 *AUTHOR.        LOGIC, INC.                                       ECS046
00011 *               DALLAS, TEXAS.                                    ECS046
00012                                                                   ECS046
00013 *DATE-COMPILED.                                                   ECS046
00014                                                                   ECS046
00015 *SECURITY.   *****************************************************ECS046
00016 *            *                                                   *ECS046
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS046
00018 *            *                                                   *ECS046
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS046
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS046
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS046
00022 *            *                                                   *ECS046
00023 *            *****************************************************ECS046
00024                                                                   ECS046
00025 *REMARKS.                                                         ECS046
00026 *            THIS PROGRAM WILL READ THE EP-EC FILE AND PRINT THE  ECS046
00027 *            ACCOUNT PROFIT SUMMARY REPORT.                       ECS046
00028  EJECT                                                            ECS046
052814******************************************************************
052814*                   C H A N G E   L O G
052814*
052814* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
052814*-----------------------------------------------------------------
052814*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
052814* EFFECTIVE    NUMBER
052814*-----------------------------------------------------------------
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
052814******************************************************************
00029  ENVIRONMENT DIVISION.                                            ECS046
00030  INPUT-OUTPUT SECTION.                                            ECS046
00031  FILE-CONTROL.                                                    ECS046
00032                                                                   ECS046
00033      SELECT  SORT-WORK-A     ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.  ECS046
00034      SELECT  SORT-WORK-B     ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.  ECS046
00035      SELECT  WORK-FILE       ASSIGN TO SYS011-UT-FBA1-S-SYS011.   ECS046
00036      SELECT  PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS046
00037      SELECT  EPEC-FILE       ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS046
00038      SELECT  ACC-MSTR        ASSIGN TO SYS015-FBA1-SYS015         ECS046
00039                              ACCESS        SEQUENTIAL             ECS046
00040                              ORGANIZATION  INDEXED                ECS046
00041                              FILE STATUS   ERACCT-FILE-STATUS     ECS046
00042                              RECORD KEY    AM-CONTROL-PRIMARY.    ECS046
00043      SELECT  COMP-FILE       ASSIGN TO SYS014-UT-FBA1-ERCOMP      ECS046
00044                              ORGANIZATION IS INDEXED              ECS046
00045                              ACCESS IS RANDOM                     ECS046
00046                              RECORD KEY IS CO-CONTROL-PRIMARY     ECS046
00047                              FILE STATUS IS ERCOMP-FILE-STATUS.   ECS046
00048      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS046
00049      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS046
00050  EJECT                                                            ECS046
00051  DATA DIVISION.                                                   ECS046
00052  FILE SECTION.                                                    ECS046
00053                                                                   ECS046
00054 *    THE FOLLOWING SORT WORK FILE IS FOR SORTING REPORT-A RECORDS ECS046
00055 *    SEE SWA-REC IN WORKING STORAGE FOR LAYOUT AND SORT CONTROL   ECS046
00056  SD  SORT-WORK-A.                                                    CL**9
00057                                                                   ECS046
00058  01  SWA-REC-OUT.                                                 ECS046
00059      12  SWA-CONTROL-OUT     PIC X(31).                           ECS046
00060      12  FILLER              PIC X(60).                           ECS046
00061      12  SWA-AMTS   COMP-3 OCCURS 2.                              ECS046
00062          16 SW-NP            PIC S9(9)V99.                        ECS046
00063          16 SW-EP            PIC S9(9)V99.                        ECS046
00064          16 SW-CI            PIC S9(9)V99.                        ECS046
00065          16 SW-EC            PIC S9(9)V99.                        ECS046
00066          16 SW-RT            PIC S9(9)V99.                        ECS046
00067          16 SW-PT            PIC S9(9)V99.                        ECS046
00068      12  SWA-SAVE-GROUPING   PIC X(6).                            ECS046
00069                                                                   ECS046
00070 *    THE FOLLOWING SORT WORK FILE IS FOR SORTING REPORT-B RECORDS ECS046
00071 *    SEE SWB-REC IN WORKING STORAGE FOR LAYOUT AND SORT CONTROL   ECS046
00072  SD  SORT-WORK-B.                                                    CL**9
00073                                                                   ECS046
00074  01  SWB-REC-OUT.                                                 ECS046
00075      12  SWB-CONTROL-OUT     PIC S9(9)V99  COMP-3.                ECS046
00076      12  FILLER              PIC X(80).                           ECS046
00077                                                                   ECS046
00078 *    THE FOLLOWING WORK FILE IS USED TO STORE REPORT-B RECORDS    ECS046
00079 *    UNTIL THEY CAN BE SORTED PRIOR TO BUILDING THE B REPORT.     ECS046
00080 *    THESE RECORDS ARE CREATED IN THE PRINT-REPORT-A SECTION.     ECS046
00081 *    SEE PARAGRAPH 3300- IN THE PROCEDURE DIVISION                ECS046
00082  FD  WORK-FILE                                                    ECS046
00083      RECORDING MODE F.                                               CL**9
00084                                                                   ECS046
00085  01  WORK-REC                PIC X(86).                           ECS046
00086                                                                   ECS046
00087  FD  PRNTR                                                        ECS046
00088                              COPY ELCPRTFD.                       ECS046
00089  EJECT                                                            ECS046
00090  FD  EPEC-FILE                                                    ECS046
00091                              COPY ECSEPCFD.                       ECS046
00092                                                                   ECS046
00093                              COPY ECSEPC01.                       ECS046
00094  EJECT                                                            ECS046
00095  FD  ACC-MSTR.                                                       CL**9
00096                                                                   ECS046
00097                              COPY ERCACCT.                        ECS046
00098  EJECT                                                            ECS046
00099  FD  COMP-FILE.                                                      CL**9
00100                                                                   ECS046
00101                              COPY ERCCOMP.                        ECS046
00102  EJECT                                                            ECS046
00103  FD  DISK-DATE                                                    ECS046
00104                              COPY ELCDTEFD.                       ECS046
00105                                                                   ECS046
00106  FD  FICH                                                         ECS046
00107                              COPY ELCFCHFD.                       ECS046
00108                                                                   ECS046
00109  EJECT                                                            ECS046
00110  WORKING-STORAGE SECTION.                                         ECS046
00111  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS046
00112  77  FILLER  PIC X(32) VALUE '********************************'.  ECS046
00113  77  FILLER  PIC X(32) VALUE '     ECS046 WORKING STORAGE     '.  ECS046
00114  77  FILLER  PIC X(32) VALUE '***********VMOD=2.014 **********'.  ECS046
00115                                                                   ECS046
00116  77  NO-OF-RECORDS-RELEASED    PIC S9(9)      COMP-3  VALUE +0.   ECS046
00117  77  SA                        PIC S999       COMP.               ECS046
00118  77  SB                        PIC S999       COMP.               ECS046
00119  77  SC                        PIC S999       COMP.               ECS046
00120  77  SE                        PIC S999       COMP.               ECS046
00121  77  CL                        PIC S999       COMP.               ECS046
00122  77  PAGE-CNT                  PIC S9(5)      COMP-3 VALUE +0.    ECS046
00123  77  LINE-CNT                  PIC S999       COMP-3 VALUE +0.    ECS046
00124  77  PGM-SUB                   PIC S999       COMP-3 VALUE +46.   ECS046
00125  77  X                         PIC X.                             ECS046
00126  77  WS-BREAK-SW               PIC X       VALUE SPACES.          ECS046
00127  77  WS-EP-CODE                PIC X.                             ECS046
00128  77  WS-FIN-RESP-AGT           PIC X(10).                         ECS046
00129  77  SAVE-STATE                PIC XX.                            ECS046
00130  77  SAVE-LF-BEN               PIC XX.                            ECS046
00131  77  SAVE-EP-CODE              PIC X.                             ECS046
00132                                                                   ECS046
00133  01  WS-ABEND-STORAGE.                                            ECS046
00134      12  WS-RETURN-CODE        PIC S9(4)  VALUE ZERO COMP.        ECS046
00135      12  WS-ABEND-MESSAGE      PIC X(80)  VALUE SPACES.           ECS046
00136      12  WS-ABEND-FILE-STATUS  PIC XX     VALUE ZERO.             ECS046
00137      12  WS-ZERO               PIC S9     VALUE ZERO COMP-3.      ECS046
00138                                                                   ECS046
00139      12  ERACCT-FILE-STATUS    PIC XX     VALUE ZERO.             ECS046
00140      12  ERCOMP-FILE-STATUS    PIC XX     VALUE ZERO.             ECS046
00141                                                                   ECS046
00142  01  WS-ACTIVE-PARM.                                              ECS046
00143      12  WS-ACTIVE-INDIC       PIC X(6).                          ECS046
00144      12  FILLER                PIC X(74).                         ECS046
00145                                                                   ECS046
00146  01  HOLD-COMP-KEY.                                               ECS046
00147      12  HOLD-COMPANY-CD       PIC X.                             ECS046
00148      12  HOLD-CARRIER          PIC X.                             ECS046
00149      12  HOLD-GROUPING         PIC X(6).                          ECS046
00150      12  HOLD-GA-NUMBER        PIC X(10).                         ECS046
00151      12  HOLD-ACCT             PIC X(10).                         ECS046
00152      12  HOLD-RECORD-TYPE      PIC X.                             ECS046
00153                                                                   ECS046
00154  01  MISC-FIELDS.                                                 ECS046
00155      12  HOLD-GA-NAME          PIC X(30)  VALUE SPACES.           ECS046
00156      12  HOLD-ACTIVE-DESC      PIC X(15)  VALUE                   ECS046
00157              'ACTIVE ACCOUNTS'.                                   ECS046
00158      12  HOLD-ALL-DESC         PIC X(15)  VALUE                   ECS046
00159              'ALL ACCOUNTS   '.                                   ECS046
00160      EJECT                                                        ECS046
00161 *    THE FOLLOWING RECORD CONTAINS REPORT DATA FOR THE A REPORT.  ECS046
00162 *    NOT ALL THE INDIVIDUAL FIELDS IN THE CONTROL AREA ARE USED   ECS046
00163 *    DEPENDING ON THE TOTAL-OPTION FOR THE PROGRAM (SEE ECS046    ECS046
00164 *    TOTAL-OPTIONS IN THE PROGRAM OPTIONS FILE).  THOSE FIELDS    ECS046
00165 *    THAT AREN'T USED ARE PRIMED WITH BLANKS ON ALL RECORDS.      ECS046
00166                                                                   ECS046
00167  01  SWA-REC.                                                     ECS046
00168      12  SWA-CONTROL.                                             ECS046
00169          16  SWA-CONTROL-1.                                       ECS046
00170              20  SWA-BUS-TYPE        PIC XX.                      ECS046
00171              20  SWA-CARR            PIC X.                       ECS046
00172              20  SWA-GROUP           PIC X(6).                    ECS046
00173              20  SWA-GA-OR-RPT-CD    PIC X(10).                   ECS046
00174              20  SWA-STATE           PIC XX.                      ECS046
00175          16  SWA-CONTROL-R REDEFINES SWA-CONTROL-1.               ECS046
00176              20  SWA-RPT-CD-1        PIC X(10).                   ECS046
00177              20  SWA-RPT-CD-2        PIC X(11).                   ECS046
00178          16  SWA-ACCT                PIC X(10).                   ECS046
00179      12  SWA-NAME                    PIC X(30).                   ECS046
00180      12  SWA-CITY                    PIC X(30).                   ECS046
00181      12  SWA-AMOUNTS COMP-3 OCCURS 2.                             ECS046
00182 *  OCCURRENCE 1 = LIFE, 2 = A&H                                   ECS046
00183          16 SWA-NET-PREM             PIC S9(9)V99.                ECS046
00184          16 SWA-EARN-PREM            PIC S9(9)V99.                ECS046
00185          16 SWA-CLMS-INCUR           PIC S9(9)V99.                ECS046
00186          16 SWA-EARN-COMP            PIC S9(9)V99.                ECS046
00187          16 SWA-RETENTION            PIC S9(9)V99.                ECS046
00188          16 SWA-PREM-TAX             PIC S9(9)V99.                ECS046
00189      12  SWA-SAVE-GROUP              PIC X(6).                    ECS046
00190                                                                   ECS046
00191 *    THE FOLLOWING RECORD CONTAINS REPORT DATA FOR THE B REPORT.  ECS046
00192 *    NOT ALL THE INDIVIDUAL FIELDS IN THE RECORD FOLLOWING THE    ECS046
00193 *    AMOUNT FIELDS HAVE DATA, DEPENDING ON THE TOTAL OPTIONS FOR  ECS046
00194 *    THIS PROGRAM.  THE SORT CONTROL FIELD (SWB-SORT-AMT) CONTAINSECS046
00195 *    THE AMOUNT THAT DETERMINES THE SEQUENCE OF THE B REPORT (SEE ECS046
00196 *    THE FORMAT-OPTION IN THE PROGRAM OPTIONS FILE).              ECS046
00197                                                                   ECS046
00198  01  SWB-REC.                                                     ECS046
00199      12  SWB-CONTROL.                                             ECS046
00200          16  SWB-SORT-AMT           PIC S9(9)V99  COMP-3.         ECS046
00201      12  SWB-LOSS-RATIO             PIC S9(4)V99  COMP-3.         ECS046
00202      12  SWB-NET-PREM               PIC S9(9)V99  COMP-3.         ECS046
00203      12  SWB-TOT-CHARGES            PIC S9(3)V99  COMP-3.         ECS046
00204      12  SWB-PREM-PROFIT            PIC S9(8)V99  COMP-3.         ECS046
00205      12  SWB-BUS-TYPE               PIC XX.                       ECS046
00206      12  SWB-CARR                   PIC X.                        ECS046
00207      12  SWB-GROUP                  PIC X(6).                     ECS046
00208      12  SWB-STATE                  PIC XX.                       ECS046
00209      12  SWB-GA-OR-RPT-CD           PIC X(10).                    ECS046
00210      12  SWB-ACCT                   PIC X(10).                    ECS046
00211      12  SWB-NAME                   PIC X(30).                    ECS046
00212                                                                   ECS046
00213      EJECT                                                        ECS046
00214  01  WS-ACCT-CONTROL.                                             ECS046
00215      12  WS-MSTR-CNTRL              PIC X(25) VALUE LOW-VALUES.   ECS046
00216      12  WS-EFFECT                  PIC 9(11) COMP-3 VALUE 0.        CL**8
00217                                                                   ECS046
00218  01  WS-SAVE-CONTROL.                                             ECS046
00219      12  WS-SAVE-CNTRL-1.                                         ECS046
00220          16 WS-SAVE-CARR            PIC X.                        ECS046
00221          16 WS-SAVE-GROUP           PIC X(6).                     ECS046
00222          16 WS-SAVE-STATE           PIC XX.                       ECS046
00223          16 WS-SAVE-ACCT            PIC X(10).                    ECS046
00224      12  WS-SAVE-EXP-DATE           PIC 9(11)  COMP-3.               CL**2
00225      12  WS-SAVE-EFF-DATE           PIC 9(11)  COMP-3.               CL**2
00226                                                                   ECS046
00227  01  SAVE-SWA-CONTROL.                                            ECS046
00228      12  SAVE-SWA-CONTROL-1.                                      ECS046
00229          16  SAVE-SWA-BUS-TYPE      PIC XX.                       ECS046
00230          16  SAVE-SWA-CARR          PIC X.                        ECS046
00231          16  SAVE-SWA-GROUP         PIC X(6).                     ECS046
00232          16  SAVE-SWA-GA-OR-RPT-CD  PIC X(10).                    ECS046
00233          16  SAVE-SWA-STATE         PIC XX.                       ECS046
00234       12 SAVE-SWA-CONTROL-R REDEFINES SAVE-SWA-CONTROL-1.         ECS046
00235          16  SAVE-SWA-RPT-CD-1      PIC X(10).                    ECS046
00236          16  SAVE-SWA-RPT-CD-2      PIC X(11).                    ECS046
00237       12  SAVE-SWA-ACCT             PIC X(10).                    ECS046
00238                                                                   ECS046
00239  01  MISC-AMTS    COMP-3.                                         ECS046
00240      12 WS-RETENTION-PCT            PIC S9V9(5).                  ECS046
00241      12 WS-TAX-RATE                 PIC S9V9(4).                  ECS046
00242      12 WS-DA-EARN-PREM             PIC S9(9)V99.                 ECS046
00243      12 WS-LF-SURPLUS               PIC S9(9)V99.                 ECS046
00244      12 WS-LF-PREM-PROFIT           PIC S9(9)V99.                 ECS046
00245      12 WS-LF-UW-INCOME             PIC S9(9)V99.                 ECS046
00246      12 WS-AH-SURPLUS               PIC S9(9)V99.                 ECS046
00247      12 WS-AH-PREM-PROFIT           PIC S9(9)V99.                 ECS046
00248      12 WS-AH-UW-INCOME             PIC S9(9)V99.                 ECS046
00249      12 WS-LOSS-RATIO               PIC S9(4)V9(4).               ECS046
00250      12 WS-COMP-RATIO               PIC S9(3)V9(4).               ECS046
00251      12 WS-TOT-CHARGES              PIC S9(3)V9(4).               ECS046
00252      12 WS-SEQUENCE                 PIC S9(5)     VALUE +0.       ECS046
00253                                                                   ECS046
00254  01  BEG-DATE                PIC 9(11).                              CL**2
00255  01  BEG-DATE-R REDEFINES BEG-DATE.                                  CL**2
00256      12 FILLER               PIC 999.                             ECS046
00257      12  BD-CCYY             PIC 9(04).                           ECS046
00258      12  BD-CCYR REDEFINES BD-CCYY.                               ECS046
00259          16  BD-CC           PIC 99.                              ECS046
00260          16  BD-YR           PIC 99.                              ECS046
00261      12  BD-MO               PIC 99.                              ECS046
00262      12  BD-DA               PIC 99.                              ECS046
00263                                                                   ECS046
00264  01  RUN-DT                  PIC 9(11).                              CL**2
00265  01  RUN-DT-R REDEFINES RUN-DT.                                      CL**2
00266      12  FILLER              PIC 999.                             ECS046
00267      12  RD-CCYY             PIC 9(04).                           ECS046
00268      12  RD-CCYR REDEFINES RD-CCYY.                               ECS046
00269          16  RD-CC           PIC 99.                              ECS046
00270          16  RD-YR           PIC 99.                              ECS046
00271      12  RD-MO               PIC 99.                              ECS046
00272      12  RD-DA               PIC 99.                              ECS046
00273                                                                   ECS046
00274  01  WS-SAVE-EXP-DATE-R      PIC 9(11).                              CL**3
00275  01  WS-SAVE-EXP-DATE-R-N REDEFINES WS-SAVE-EXP-DATE-R.              CL**5
00276      12  FILLER                 PIC 999.                          ECS046
00277      12  WS-SAVE-EXP-CCYY       PIC 9(04).                        ECS046
00278      12  WS-SAVE-EXP-CCYR REDEFINES WS-SAVE-EXP-CCYY.             ECS046
00279          16  WS-SAVE-EXP-CC     PIC 99.                           ECS046
00280          16  WS-SAVE-EXP-YR     PIC 99.                           ECS046
00281      12  WS-SAVE-EXP-MO         PIC 99.                           ECS046
00282      12  WS-SAVE-EXP-DA         PIC 99.                           ECS046
00283                                                                   ECS046
00284  01  WS-SAVE-EFF-DATE-R      PIC 9(11).                              CL**3
00285  01  WS-SAVE-EFF-DATE-R-N  REDEFINES WS-SAVE-EFF-DATE-R.             CL**3
00286      12  FILLER              PIC 999.                             ECS046
00287      12  WS-SAVE-EFF-CCYY    PIC 9(04).                           ECS046
00288      12  WS-SAVE-EFF-CCYR REDEFINES WS-SAVE-EFF-CCYY.             ECS046
00289          16  WS-SAVE-EFF-CC  PIC 99.                              ECS046
00290          16  WS-SAVE-EFF-YR  PIC 99.                              ECS046
00291      12  WS-SAVE-EFF-MO      PIC 99.                              ECS046
00292      12  WS-SAVE-EFF-DA      PIC 99.                              ECS046
00293                                                                   ECS046
00294      EJECT                                                        ECS046
00295 *                                                                 ECS046
00296 * THE DETAIL AND AGENTS ACCUMULATORS ARE USED AS FOLLOWS          ECS046
00297 *                                                                 ECS046
00298 *    OCCURRENCE 1 = LIFE BEGINNING DATA                           ECS046
00299 *    OCCURRENCE 2 = A&H BEGINNING DATA                            ECS046
00300 *    OCCURRENCE 3 = LIFE CURRENT DATA                             ECS046
00301 *    OCCURRENCE 4 = A&H CURRENT DATA                              ECS046
00302 *                                                                 ECS046
00303  01  DETAIL-ACCUMULATORS     COMP-3.                              ECS046
00304      12  DETAIL-ACCUMS   OCCURS 4.                                ECS046
00305          16  DA-NET-PREM            PIC S9(9)V99.                 ECS046
00306          16  DA-EARN-PREM           PIC S9(9)V99.                 ECS046
00307          16  DA-CLAIM-AMT           PIC S9(9)V99.                 ECS046
00308          16  DA-LOSS-RSRV           PIC S9(9)V99.                 ECS046
00309          16  DA-EARN-COMP           PIC S9(9)V99.                 ECS046
00310          16  DA-RETENTION           PIC S9(9)V99.                 ECS046
00311          16  DA-PREM-TAX            PIC S9(9)V99.                 ECS046
00312 *                                                                 ECS046
00313 * THE BREAK ACCUMULATORS ARE USED AS FOLLOWS                      ECS046
00314 *                                                                 ECS046
00315 *    A SET CONSISTS OF 2 ACCUMULATORS FOR EACH ITEM.              ECS046
00316 *        OCCURRENCE 1 = LIFE TOTAL PERIOD                         ECS046
00317 *        OCCURRENCE 2 = A&H TOTAL PERIOD                          ECS046
00318 *                                                                 ECS046
00319 *    THERE ARE 6 SETS                                             ECS046
00320 *        SET 1 = LEVEL 1 (STATE TOTALS)                           ECS046
00321 *        SET 2 = LEVEL 2 (REPORT CODE OR G.A. TOTALS)             ECS046
00322 *        SET 3 = LEVEL 3 (GROUP TOTALS)                           ECS046
00323 *        SET 4 = LEVEL 4 (CARRIER TOTALS)                         ECS046
00324 *        SET 5 = LEVEL 5 (BUSINESS TYPE TOTALS)                   ECS046
00325 *        SET 6 = LEVEL 6 (FINAL TOTALS)                           ECS046
00326 *                                                                 ECS046
00327  01  BREAK-ACCUMULATORS      COMP-3.                              ECS046
00328      12  BREAK-ACCUMS    OCCURS 2.                                ECS046
00329          16  BREAK-ACCUMS-SL    OCCURS 6.                         ECS046
00330              20  BA-NET-PREM     PIC S9(9)V99.                    ECS046
00331              20  BA-EARN-PREM    PIC S9(9)V99.                    ECS046
00332              20  BA-CLMS-INCUR   PIC S9(9)V99.                    ECS046
00333              20  BA-EARN-COMP    PIC S9(9)V99.                    ECS046
00334              20  BA-RETENTION    PIC S9(9)V99.                    ECS046
00335              20  BA-PREM-PROFIT  PIC S9(9)V99.                    ECS046
00336              20  BA-PREM-TAX     PIC S9(9)V99.                    ECS046
00337                                                                   ECS046
00338      EJECT                                                        ECS046
00339  01  HEAD-1.                                                      ECS046
00340      12  FILLER          PIC X(52)   VALUE SPACES.                ECS046
00341      12  FILLER          PIC X(22)   VALUE                        ECS046
00342                                      'ACCOUNT PROFIT SUMMARY'.    ECS046
00343      12  FILLER          PIC X(4)    VALUE SPACES.                ECS046
00344      12  HD-RPT-TYPE     PIC X(25)   VALUE SPACES.                ECS046
00345      12  FILLER          PIC X       VALUE SPACES.                ECS046
00346      12  HD-STATUS-DESC  PIC X(15)   VALUE SPACES.                ECS046
00347 *    12  FILLER          PIC X(5)    VALUE SPACES.                ECS046
00348      12  FILLER          PIC X(6)    VALUE 'ECS046'.              ECS046
00349      12  H1-REPORT-SUF   PIC X       VALUE SPACES.                ECS046
00350                                                                   ECS046
00351  01  HEAD-2.                                                      ECS046
00352      12  FILLER          PIC X(48)   VALUE SPACES.                ECS046
00353      12  HD-COMP         PIC X(30)   VALUE SPACES.                ECS046
00354      12  FILLER          PIC X(41)   VALUE SPACES.                ECS046
00355      12  HD-RUN-DATE     PIC X(8)    VALUE SPACES.                ECS046
00356                                                                   ECS046
00357  01  HEAD-3.                                                      ECS046
00358      12  FILLER          PIC X(53)   VALUE SPACES.                ECS046
00359      12  HD-ALPHA-DATE   PIC X(18)   VALUE SPACES.                ECS046
00360      12  FILLER          PIC X(48)   VALUE SPACES.                ECS046
00361      12  FILLER          PIC X(5)    VALUE 'PAGE'.                ECS046
00362      12  HD-PAGE-NO      PIC ZZZ9.                                ECS046
00363                                                                   ECS046
00364  01  HEAD-CARR.                                                   ECS046
00365      12  FILLER          PIC X(13)   VALUE 'CARRIER    -'.        ECS046
00366      12  HC-CARRIER      PIC X       VALUE SPACES.                ECS046
00367      12  FILLER          PIC XX      VALUE SPACES.                ECS046
00368      12  HC-CARRIER-NAME PIC X(30)   VALUE SPACES.                ECS046
00369                                                                   ECS046
00370  01  HEAD-BUS-TYPE.                                               ECS046
00371      12  FILLER          PIC X(13)   VALUE 'BUS. TYPE  -'.        ECS046
00372      12  HB-BUS-TYPE     PIC XX      VALUE SPACES.                ECS046
00373      12  FILLER          PIC X       VALUE SPACES.                ECS046
00374      12  HB-BUS-DESC     PIC X(24)   VALUE SPACES.                ECS046
00375                                                                   ECS046
00376  01  HEAD-RPT-CODE.                                               ECS046
00377      12  HR-CAPTION      PIC X(10)   VALUE SPACES.                ECS046
00378      12  FILLER          PIC XXX     VALUE ' -'.                  ECS046
00379      12  HR-RPT-CODE     PIC X(10)   VALUE SPACES.                ECS046
00380                                                                   ECS046
00381  01  HEAD-RPT2-CODE.                                              ECS046
00382      12  HR2-CAPTION     PIC X(10)   VALUE SPACES.                ECS046
00383      12  FILLER          PIC XXX     VALUE ' -'.                  ECS046
00384      12  HR2-RPT-CODE1   PIC X(10)   VALUE SPACES.                ECS046
00385      12  FILLER          PIC XXX     VALUE ' -'.                  ECS046
00386      12  HR2-RPT-CODE2   PIC X(10)   VALUE SPACES.                ECS046
00387                                                                   ECS046
00388  01  HEAD-GROUP.                                                  ECS046
00389      12  FILLER          PIC X(13)   VALUE 'GROUP      -'.        ECS046
00390      12  HG-GROUP        PIC X(6)    VALUE SPACES.                ECS046
00391                                                                   ECS046
00392      EJECT                                                        ECS046
00393  01  HEAD-G-A.                                                    ECS046
00394      12  FILLER          PIC X(13)   VALUE 'GENL AGENT -'.        ECS046
00395      12  HGA-AGENT       PIC X(10)   VALUE SPACES.                ECS046
00396      12  FILLER          PIC X(5)    VALUE SPACES.                ECS046
00397      12  HGA-GA-NAME     PIC X(30)   VALUE SPACES.                ECS046
00398                                                                   ECS046
00399  01  HEAD-STATE.                                                  ECS046
00400      12  FILLER          PIC X(13)   VALUE 'STATE      -'.        ECS046
00401      12  HS-STATE        PIC XX      VALUE SPACES.                ECS046
00402                                                                   ECS046
00403  01  HEAD-ACCT.                                                   ECS046
00404      12  FILLER          PIC X(13)   VALUE 'ACCOUNT    -'.        ECS046
00405      12  HA-ACCOUNT      PIC X(10)   VALUE SPACES.                ECS046
00406      12  FILLER          PIC XX      VALUE SPACES.                ECS046
00407      12  HA-ACCT-NAME    PIC X(30)   VALUE SPACES.                ECS046
00408      12  FILLER          PIC XX      VALUE SPACES.                ECS046
00409      12  HA-ACCT-CITY    PIC X(30)   VALUE SPACES.                ECS046
00410                                                                   ECS046
00411  01  HD-BRK-TYPE.                                                 ECS046
00412      12  HD-RPT-CAPTION  PIC X(10)   VALUE SPACES.                ECS046
00413      12  FILLER          PIC X(7)    VALUE ' TOTALS'.             ECS046
00414                                                                   ECS046
00415  01  HEAD-4A.                                                     ECS046
00416      12  FILLER          PIC X(50)   VALUE                        ECS046
00417          '          NET       EARNED     CLAIMS       EARNED'.    ECS046
00418      12  HEAD-4A-FIL1    PIC X(27)   VALUE SPACES.                ECS046
00419      12  FILLER          PIC X(12)   VALUE                        ECS046
00420          'PREMIUM     '.                                          ECS046
00421      12  HEAD-4A-FIL2    PIC X(8)    VALUE 'PREMIUM '.            ECS046
00422      12  FILLER          PIC X(33)   VALUE                        ECS046
00423          'UNDERWRITE  LOSS   COMP    TOTAL '.                     ECS046
00424                                                                   ECS046
00425  01  HEAD-4A-DESC.                                                ECS046
00426      12  FIL1-DESC       PIC X(27)   VALUE                        ECS046
00427              '                RETRO      '.                       ECS046
00428      12  FIL2-DESC       PIC X(8)    VALUE                        ECS046
00429              'MINIMUM '.                                          ECS046
00430                                                                   ECS046
00431  01  HEAD-5A.                                                     ECS046
00432      12  FILLER          PIC X(50)   VALUE                        ECS046
00433          '        PREMIUM     PREMIUM   INCURRED       COMP'.     ECS046
00434      12  FILLER          PIC X(27)   VALUE                        ECS046
00435          '    SURPLUS   RETENTION'.                               ECS046
00436      12  FILLER          PIC X(12)   VALUE                        ECS046
00437          'PROFIT      '.                                          ECS046
00438      12  HEAD-5A-FILL1   PIC X(6)    VALUE  ' TAXES'.             ECS046
00439      12  FILLER          PIC X(35)   VALUE                        ECS046
00440          '    INCOME    RATIO  RATIO  CHARGES'.                   ECS046
00441                                                                   ECS046
00442  01  HEAD-5A-DESC.                                                ECS046
00443      12  FILLER          PIC X(6)    VALUE                        ECS046
00444          'MARGIN'.                                                ECS046
00445                                                                   ECS046
00446  01  HEAD-4B.                                                     ECS046
00447      12  FILLER          PIC X(50)   VALUE                        ECS046
00448          '        LOSS          NET      TOTAL      PREMIUM'.     ECS046
00449      12  FILLER          PIC X(12).                               ECS046
00450      12  H4B-BUS         PIC XXX.                                 ECS046
00451      12  FILLER          PIC X(10).                               ECS046
00452      12  H4B-GENERAL     PIC X(7).                                ECS046
00453      12  FILLER          PIC X(50).                               ECS046
00454                                                                   ECS046
00455  01  HEAD-5B.                                                     ECS046
00456      12  FILLER          PIC X(50)   VALUE                        ECS046
00457          ' SEQ    RATIO       PREMIUM   CHARGES     PROFIT'.      ECS046
00458      12  FILLER          PIC X(9)    VALUE SPACES.                ECS046
00459      12  H5B-CTL-HDA     PIC X(15).                               ECS046
00460      12  H5B-CTL-HDB     PIC X(16).                               ECS046
00461      12  FILLER          PIC X(44)   VALUE                        ECS046
00462          ' ACCOUNT     ACCOUNT NAME'.                             ECS046
00463                                                                   ECS046
00464      EJECT                                                        ECS046
00465  01  DTL-NO-ACCT.                                                 ECS046
00466      12  DNA-CARR        PIC X       VALUE SPACES.                ECS046
00467      12  FILLER          PIC X       VALUE SPACES.                ECS046
00468      12  DNA-GROUP       PIC X(6)    VALUE SPACES.                ECS046
00469      12  FILLER          PIC X       VALUE SPACES.                ECS046
00470      12  DNA-ST          PIC XX      VALUE SPACES.                ECS046
00471      12  FILLER          PIC X       VALUE SPACES.                ECS046
00472      12  DNA-ACCT        PIC X(10)   VALUE SPACES.                ECS046
00473      12  FILLER          PIC X       VALUE SPACES.                ECS046
00474      12  DNA-FMO         PIC XX      VALUE SPACES.                ECS046
00475      12  DNA-FS1         PIC X       VALUE '-'.                   ECS046
00476      12  DNA-FDA         PIC XX      VALUE SPACES.                ECS046
00477      12  DNA-FS2         PIC X       VALUE '-'.                   ECS046
00478      12  DNA-FYR         PIC XX      VALUE SPACES.                ECS046
00479      12  FILLER          PIC X       VALUE SPACES.                ECS046
00480      12  DNA-TMO         PIC XX      VALUE SPACES.                ECS046
00481      12  DNA-TS1         PIC X       VALUE '-'.                   ECS046
00482      12  DNA-TDA         PIC XX      VALUE SPACES.                ECS046
00483      12  DNA-TS2         PIC X       VALUE '-'.                   ECS046
00484      12  DNA-TYR         PIC XX      VALUE SPACES.                ECS046
00485      12  FILLER          PIC X(4)    VALUE SPACES.                ECS046
00486      12  DNA-MESSAGE     PIC X(40)   VALUE SPACES.                ECS046
00487                                                                   ECS046
00488  01  DTL-LINE-A.                                                  ECS046
00489      12  DLA-BEN-DESC    PIC XXX.                                 ECS046
00490      12  FILLER          PIC X           VALUE SPACES.            ECS046
00491      12  DLA-NET-PREM    PIC ZZZ,ZZZ,ZZZ-   BLANK WHEN ZERO.      ECS046
00492      12  DLA-EARN-PREM   PIC ZZZ,ZZZ,ZZZ-   BLANK WHEN ZERO.      ECS046
00493      12  DLA-CLMS-INCUR  PIC ZZZ,ZZZ,ZZZ-   BLANK WHEN ZERO.      ECS046
00494      12  DLA-EARN-COMP   PIC ZZZ,ZZZ,ZZZ-   BLANK WHEN ZERO.      ECS046
00495      12  DLA-SURPLUS     PIC  ZZ,ZZZ,ZZZ-   BLANK WHEN ZERO.      ECS046
00496      12  DLA-RETENTION   PIC ZZZ,ZZZ,ZZZ-   BLANK WHEN ZERO.      ECS046
00497      12  DLA-PREM-PROFIT PIC  ZZ,ZZZ,ZZZ-   BLANK WHEN ZERO.      ECS046
00498      12  DLA-PREM-TAX    PIC  ZZ,ZZZ,ZZZ-   BLANK WHEN ZERO.      ECS046
00499      12  DLA-UW-INCOME   PIC  ZZ,ZZZ,ZZZ-   BLANK WHEN ZERO.      ECS046
00500      12  DLA-LOSS-RATIO  PIC ZZZ9.99-.                            ECS046
00501      12  DLA-LOSS-RATIO-RD REDEFINES DLA-LOSS-RATIO  PIC X(8).    ECS046
00502      12  DLA-COMP-RATIO  PIC  Z9.99-.                             ECS046
00503      12  FILLER          PIC X           VALUE SPACES.            ECS046
00504      12  DLA-TOT-CHARGES PIC ZZ9.99-.                             ECS046
00505      12  DLA-TOT-CHARGES-RD REDEFINES DLA-TOT-CHARGES  PIC X(7).  ECS046
00506      12  FILLER          PIC XXX         VALUE SPACES.            ECS046
00507      EJECT                                                        ECS046
00508  01  DTL-LINE-B.                                                  ECS046
00509      12  DLB-SEQ-NO      PIC ZZZZ9.                               ECS046
00510      12  FILLER          PIC X           VALUE SPACES.            ECS046
00511      12  DLB-LOSS-RATIO  PIC ZZZ9.99-.                            ECS046
00512      12  FILLER          PIC XX          VALUE SPACES.            ECS046
00513      12  DLB-NET-PREM    PIC ZZZ,ZZZ,ZZZ-   BLANK WHEN ZERO.      ECS046
00514      12  FILLER          PIC XX          VALUE SPACES.            ECS046
00515      12  DLB-TOT-CHARGES PIC ZZ9.99-.                             ECS046
00516      12  FILLER          PIC X           VALUE SPACES.            ECS046
00517      12  DLB-PREM-PROFIT PIC ZZZ,ZZZ,ZZZ-   BLANK WHEN ZERO.      ECS046
00518      12  DLB-DESC.                                                ECS046
00519          16  FILLER          PIC X(13).                           ECS046
00520          16  DLB-BUS-TYPE    PIC XX.                              ECS046
00521          16  FILLER          PIC X(4).                            ECS046
00522          16  DLB-CARRIER     PIC X.                               ECS046
00523          16  FILLER          PIC X(4).                            ECS046
00524          16  DLB-GROUP       PIC X(6).                            ECS046
00525          16  FILLER          PIC X(4).                            ECS046
00526          16  DLB-STATE       PIC XX.                              ECS046
00527          16  FILLER          PIC X(4).                            ECS046
00528      12  DLB-DESC2  REDEFINES  DLB-DESC.                          ECS046
00529          16  FILLER          PIC X(10).                           ECS046
00530          16  DLB2-CARRIER    PIC X.                               ECS046
00531          16  FILLER          PIC X(4).                            ECS046
00532          16  DLB2-GROUP      PIC X(6).                            ECS046
00533          16  FILLER          PIC XXX.                             ECS046
00534          16  DLB2-GA-OR-RPT-CD PIC X(10).                         ECS046
00535          16  FILLER          PIC X(6).                            ECS046
00536      12  DLB-ACCOUNT         PIC X(10)       VALUE SPACES.        ECS046
00537      12  FILLER              PIC XX          VALUE SPACES.        ECS046
00538      12  DLB-ACCT-NAME       PIC X(30)       VALUE SPACES.        ECS046
00539                                                                   ECS046
00540                              COPY ELCDTECX.                       ECS046
00541                                                                   ECS046
00542                              COPY ELCDTEVR.                       ECS046
00543                                                                   ECS046
00544                              COPY ELCEPCVR.                       ECS046
00545                                                                   ECS046
00546  EJECT                                                            ECS046
00547  PROCEDURE DIVISION.                                              ECS046
00548                                                                   ECS046
00549  0100-START.                                                      ECS046
00550                              COPY ELCDTERX.                       ECS046
00551                                                                   ECS046
00552      MOVE WS-CURRENT-DATE        TO HD-RUN-DATE.                  ECS046
00553      MOVE ALPH-DATE              TO HD-ALPHA-DATE.                ECS046
00554      MOVE COMPANY-NAME           TO HD-COMP.                      ECS046
00555      MOVE RUN-DATE               TO BEG-DATE                      ECS046
00556                                     RUN-DT.                       ECS046
00557  0200-BUILD-SELECT-DATES.                                         ECS046
00558      IF DTE-PGM-OPT = '1'                                         ECS046
00559 *                           SET BEGIN DATE FOR YTD REPORT         ECS046
00560          SUBTRACT 1 FROM BD-CCYY                                  ECS046
00561          MOVE 12                 TO BD-MO                         ECS046
00562          MOVE 31                 TO BD-DA.                        ECS046
00563                                                                   ECS046
00564      IF DTE-PGM-OPT = '2'                                         ECS046
00565 *                           SET BEGIN DATE FOR 12 MO. REPORT      ECS046
00566          SUBTRACT 1 FROM BD-CCYY.                                 ECS046
00567                                                                   ECS046
00568      IF DTE-PGM-OPT = '3'                                         ECS046
00569 *                           SET BEGIN DATE FOR ITD REPORT         ECS046
00570          MOVE ZEROS              TO BEG-DATE.                     ECS046
00571                                                                   ECS046
00572      IF DTE-PGM-OPT = 4                                           ECS046
00573 *                         SET BEGIN DATE FOR CURRENT MONTH REPORT ECS046
00574          MOVE 31                 TO BD-DA                         ECS046
00575          IF BD-MO = 01                                            ECS046
00576              SUBTRACT 1 FROM BD-CCYY                              ECS046
00577              MOVE 12             TO BD-MO                         ECS046
00578            ELSE                                                   ECS046
00579              SUBTRACT 1 FROM BD-MO.                               ECS046
00580                                                                   ECS046
00581  0500-SORT-CONTROL.                                               ECS046
00582      SORT SORT-WORK-A ON ASCENDING                                ECS046
00583                          SWA-CONTROL-OUT                          ECS046
00584          INPUT PROCEDURE   1000-BUILD-EXTRACTS                    ECS046
00585          OUTPUT PROCEDURE  3000-PRINT-REPORT-A.                   ECS046
00586                                                                   ECS046
00587      IF SORT-RETURN NOT = ZERO AND 4                              ECS046
00588          MOVE SORT-RETURN                  TO WS-RETURN-CODE      ECS046
00589          MOVE 'UNSUCCESSFUL SORT - SORT 1' TO WS-ABEND-MESSAGE    ECS046
00590          GO TO ABEND-PGM.                                         ECS046
00591                                                                   ECS046
00592      IF DTE-FMT-OPT NOT = 1                                       ECS046
00593          SORT SORT-WORK-B                                         ECS046
00594              DESCENDING        SWB-CONTROL-OUT                    ECS046
00595              USING WORK-FILE                                      ECS046
00596              OUTPUT PROCEDURE  5000-PRINT-REPORT-B                ECS046
00597          IF SORT-RETURN NOT = ZERO AND 4                          ECS046
00598              MOVE SORT-RETURN        TO WS-RETURN-CODE            ECS046
00599              MOVE 'UNSUCCESSFUL SORT - SORT 2'                    ECS046
00600                                      TO WS-ABEND-MESSAGE          ECS046
00601              GO TO ABEND-PGM.                                     ECS046
00602                                                                   ECS046
00603      GOBACK.                                                      ECS046
00604                                                                   ECS046
00605      EJECT                                                        ECS046
00606  1000-BUILD-EXTRACTS SECTION.                                     ECS046
00607                                                                   ECS046
00608  1100-BEGIN-PROCESSING.                                           ECS046
00609      OPEN INPUT ACC-MSTR                                          ECS046
00610                 EPEC-FILE                                         ECS046
00611          OUTPUT PRNTR.                                            ECS046
00612                                                                   ECS046
00613      IF ERACCT-FILE-STATUS   = '00' OR '97'                       ECS046
00614          NEXT SENTENCE                                            ECS046
00615      ELSE                                                         ECS046
00616          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS046
00617          MOVE 'OPEN ERROR - ERACCT' TO WS-ABEND-MESSAGE           ECS046
00618          GO TO ABEND-PGM.                                         ECS046
00619                                                                   ECS046
00620      IF DTE-CLIENT  =  'MON'                                      ECS046
00621          MOVE FIL1-DESC          TO  HEAD-4A-FIL1                 ECS046
00622          MOVE FIL2-DESC          TO  HEAD-4A-FIL2                 ECS046
00623          MOVE HEAD-5A-DESC       TO  HEAD-5A-FILL1.               ECS046
00624                                                                   ECS046
00625      PERFORM 2400-ZERO-DETAIL-ACCUMULATORS                        ECS046
00626          VARYING SA FROM 1 BY 1 UNTIL SA GREATER 4.               ECS046
00627                                                                   ECS046
00628      PERFORM 2100-READ-EXTRACT THRU 2140-EXIT.                    ECS046
00629      PERFORM 2200-READ-ACCT    THRU 2240-EXIT.                    ECS046
00630                                                                   ECS046
00631      MOVE AM-COMPANY-CD          TO  HOLD-COMPANY-CD.             ECS046
00632      MOVE EP-CONTROL             TO WS-SAVE-CONTROL.              ECS046
00633      GO TO 1800-CHECK-ACCT-MSTR.                                  ECS046
00634                                                                   ECS046
00635  1200-CHECK-CONTROL.                                              ECS046
00636                                                                   ECS046
00637 * ALL PURGE RECORDS FROM THE EPEC FILE ARE INCLUDED.              ECS046
00638 * THOSE WITH A DATE GREATER THAN THE PERIOD BEGIN DATE            ECS046
00639 * ARE ADDED TO THE PERIOD ENDING DATE TOTALS.                     ECS046
00640 * THOSE WITH A DATE LESS THAN OR EQUAL TO THE PERIOD ENDING DATE  ECS046
00641 * ARE ADDED TO THE PERIOD BEGINNING AND ENDING DATE TOTALS.       ECS046
00642                                                                   ECS046
00643      IF EP-PURGE = 'P'                                            ECS046
00644          IF WS-EP-RUN-DTE GREATER BEG-DATE                        ECS046
00645              MOVE 'Q'            TO EP-PURGE                      ECS046
00646              GO TO 1250-TEST-ACCUM                                ECS046
00647          ELSE                                                     ECS046
00648              GO TO 1250-TEST-ACCUM.                               ECS046
00649                                                                   ECS046
00650      IF EP-RUN-YR = BD-YR AND                                     ECS046
00651         EP-RUN-MO = BD-MO                                         ECS046
00652          MOVE '1'                TO EP-PURGE                      ECS046
00653          GO TO 1250-TEST-ACCUM.                                   ECS046
00654                                                                   ECS046
00655      MOVE '2'                    TO EP-PURGE.                     ECS046
00656                                                                   ECS046
00657      EJECT                                                        ECS046
00658  1250-TEST-ACCUM.                                                 ECS046
00659      IF EP-RECORD-ID = 'EP'                                       ECS046
00660          IF EP-PURGE = ('1' OR 'P')                               ECS046
00661              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    ECS046
00662                  MOVE +1         TO SA                            ECS046
00663                  PERFORM 1300-EP-ADD THRU 1350-EXIT               ECS046
00664              ELSE                                                 ECS046
00665                  MOVE +2         TO SA                            ECS046
00666                  PERFORM 1300-EP-ADD THRU 1350-EXIT.              ECS046
00667                                                                   ECS046
00668      IF EP-RECORD-ID = 'EP'                                       ECS046
00669          IF EP-PURGE = ('2' OR 'P' OR 'Q')                        ECS046
00670              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    ECS046
00671                  MOVE +3         TO SA                            ECS046
00672                  PERFORM 1300-EP-ADD THRU 1350-EXIT               ECS046
00673              ELSE                                                 ECS046
00674                  MOVE +4         TO SA                            ECS046
00675                  PERFORM 1300-EP-ADD THRU 1350-EXIT.              ECS046
00676                                                                   ECS046
00677      IF EP-RECORD-ID = 'EC'                                       ECS046
00678          IF EP-PURGE = ('1' OR 'P')                               ECS046
00679              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    ECS046
00680                  MOVE +1         TO SA                            ECS046
00681                  PERFORM 1400-EC-ADD THRU 1440-EXIT               ECS046
00682              ELSE                                                 ECS046
00683                  MOVE +2         TO SA                            ECS046
00684                  PERFORM 1400-EC-ADD THRU 1440-EXIT.              ECS046
00685                                                                   ECS046
00686      IF EP-RECORD-ID = 'EC'                                       ECS046
00687          IF EP-PURGE = ('2' OR 'P' OR 'Q')                        ECS046
00688              IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                    ECS046
00689                  MOVE +3         TO SA                            ECS046
00690                  PERFORM 1400-EC-ADD THRU 1440-EXIT               ECS046
00691              ELSE                                                 ECS046
00692                  MOVE +4         TO SA                            ECS046
00693                  PERFORM 1400-EC-ADD THRU 1440-EXIT.              ECS046
00694                                                                   ECS046
00695      GO TO 1500-GET-NEXT-EXTRACT.                                 ECS046
00696                                                                   ECS046
00697      EJECT                                                        ECS046
00698  1300-EP-ADD.                                                     ECS046
00699                                                                   ECS046
00700 * TOTALS FOR EARNED PREMIUM RECORDS ARE ACCUMULATED IN A          ECS046
00701 * TOTAL TABLE FOR THE GIVEN ACCOUNT                               ECS046
00702                                                                   ECS046
00703      ADD EP-ISS-PRM              TO DA-NET-PREM (SA).             ECS046
00704      SUBTRACT EP-CNC-PRM         FROM DA-NET-PREM (SA).           ECS046
00705      ADD EP-CLM-AMT              TO DA-CLAIM-AMT (SA).            ECS046
00706      ADD EP-CLM-DU               TO DA-LOSS-RSRV (SA).            ECS046
00707      ADD EP-CLM-IBNR             TO DA-LOSS-RSRV (SA).            ECS046
00708      ADD EP-LOSS-RESV            TO DA-LOSS-RSRV (SA).            ECS046
00709      ADD EP-CLAIM-ADJ            TO DA-CLAIM-AMT (SA).            ECS046
00710      MOVE ZERO                   TO WS-DA-EARN-PREM.              ECS046
00711                                                                   ECS046
00712      IF WS-EP-CODE = 'P'                                          ECS046
00713          MOVE EP-PRM-PR          TO WS-DA-EARN-PREM               ECS046
00714      ELSE                                                         ECS046
00715      IF WS-EP-CODE = 'M'                                          ECS046
00716          COMPUTE WS-DA-EARN-PREM ROUNDED =                        ECS046
00717                 (EP-PRM-PR + EP-PRM-78) * .5                      ECS046
00718      ELSE                                                         ECS046
00719      IF WS-EP-CODE = '1'                                          ECS046
00720          COMPUTE WS-DA-EARN-PREM ROUNDED =                        ECS046
00721               (EP-PRM-PR * +.6667) + (EP-PRM-78 * +.3333)         ECS046
00722      ELSE                                                         ECS046
00723      IF WS-EP-CODE = '2'                                          ECS046
00724          COMPUTE WS-DA-EARN-PREM ROUNDED =                        ECS046
00725               (EP-PRM-PR * +.80) + (EP-PRM-78 * +.20)             ECS046
00726      ELSE                                                         ECS046
00727          MOVE EP-PRM-78          TO WS-DA-EARN-PREM.              ECS046
00728                                                                   ECS046
00729      COMPUTE DA-EARN-PREM (SA) ROUNDED = DA-EARN-PREM (SA) +      ECS046
00730                                          WS-DA-EARN-PREM.         ECS046
00731                                                                   ECS046
00732      COMPUTE DA-RETENTION (SA) ROUNDED = DA-RETENTION (SA) +      ECS046
00733                        (WS-RETENTION-PCT * WS-DA-EARN-PREM).      ECS046
00734                                                                   ECS046
00735      COMPUTE DA-PREM-TAX (SA) ROUNDED = DA-PREM-TAX (SA) +        ECS046
00736                           (WS-TAX-RATE * WS-DA-EARN-PREM).        ECS046
00737                                                                   ECS046
00738  1350-EXIT.                                                       ECS046
00739       EXIT.                                                       ECS046
00740                                                                   ECS046
00741      EJECT                                                        ECS046
00742  1400-EC-ADD.                                                     ECS046
00743                                                                   ECS046
00744 * TOTALS FOR EARNED COMMISSION RECORDS ARE ACCUMULATED IN A       ECS046
00745 * TOTAL TABLE FOR THE GIVEN ACCOUNT                               ECS046
00746                                                                   ECS046
00747      PERFORM 1420-ACCT-EC-ADD THRU 1430-EXIT                      ECS046
00748          VARYING CL FROM 1 BY 1 UNTIL CL GREATER 5.               ECS046
00749                                                                   ECS046
00750      GO TO 1440-EXIT.                                             ECS046
00751                                                                   ECS046
00752  1420-ACCT-EC-ADD.                                                ECS046
00753      IF EC-AGT-TYPE (CL) = 'C' OR 'D' OR 'O' OR 'P'               ECS046
00754          NEXT SENTENCE                                            ECS046
00755       ELSE                                                        ECS046
00756           GO TO 1430-EXIT.                                        ECS046
00757                                                                   ECS046
00758      IF WS-EP-CODE = 'P'                                          ECS046
00759          COMPUTE DA-EARN-COMP (SA) ROUNDED = DA-EARN-COMP (SA) +  ECS046
00760                                              EC-COMM-PR (CL)      ECS046
00761      ELSE                                                         ECS046
00762      IF WS-EP-CODE = 'M'                                          ECS046
00763          COMPUTE DA-EARN-COMP (SA) ROUNDED = DA-EARN-COMP (SA) +  ECS046
00764                                              ((EC-COMM-PR (CL) +  ECS046
00765                                            EC-COMM-78 (CL)) * .5) ECS046
00766      ELSE                                                         ECS046
00767      IF WS-EP-CODE = '1'                                          ECS046
00768          COMPUTE DA-EARN-COMP (SA) ROUNDED = DA-EARN-COMP (SA) +  ECS046
00769                                    ((EC-COMM-PR (CL) * +.6667) +  ECS046
00770                                     (EC-COMM-78 (CL) * +.3333))   ECS046
00771      ELSE                                                         ECS046
00772      IF WS-EP-CODE = '2'                                          ECS046
00773          COMPUTE DA-EARN-COMP (SA) ROUNDED = DA-EARN-COMP (SA) +  ECS046
00774                                      ((EC-COMM-PR (CL) * +.80) +  ECS046
00775                                       (EC-COMM-78 (CL) * +.20))   ECS046
00776      ELSE                                                         ECS046
00777          COMPUTE DA-EARN-COMP (SA) ROUNDED = DA-EARN-COMP (SA) +  ECS046
00778                                              EC-COMM-78 (CL).     ECS046
00779                                                                   ECS046
00780  1430-EXIT.                                                       ECS046
00781       EXIT.                                                       ECS046
00782                                                                   ECS046
00783  1440-EXIT.                                                       ECS046
00784       EXIT.                                                       ECS046
00785                                                                   ECS046
00786      EJECT                                                        ECS046
00787  1500-GET-NEXT-EXTRACT.                                           ECS046
00788      PERFORM 2100-READ-EXTRACT THRU 2140-EXIT.                    ECS046
00789                                                                   ECS046
00790      IF EP-CONTROL = WS-SAVE-CONTROL                              ECS046
00791          GO TO 1200-CHECK-CONTROL.                                ECS046
00792                                                                   ECS046
00793 * CONTROL BREAKS OCCUR AT THE ACCOUNT LEVEL, FOR TOT-OPT 1 AND 2  ECS046
00794 * ALLOW DATE RANGE LEVEL BREAK ( TOT-OPT 3 THRU 6)                ECS046
00795                                                                   ECS046
00796      IF DTE-TOT-OPT = 1  OR  2                                    ECS046
00797          IF EP-CNTRL-1 = WS-SAVE-CNTRL-1                          ECS046
00798              MOVE EP-CONTROL         TO WS-SAVE-CONTROL           ECS046
00799              GO TO 1200-CHECK-CONTROL.                            ECS046
00800                                                                   ECS046
00801      MOVE AM-ACCOUNT             TO SWA-ACCT.                     ECS046
00802      MOVE AM-NAME                TO SWA-NAME.                     ECS046
00803      MOVE AM-CITY                TO SWA-CITY.                     ECS046
00804                                                                   ECS046
00805 * DEPENDING ON THE TOTAL OPTION, SORT CONTROL FIELDS ARE PRIMED   ECS046
00806                                                                   ECS046
00807      IF DTE-TOT-OPT = 1                                           ECS046
00808          MOVE SPACES             TO SWA-BUS-TYPE                  ECS046
00809          MOVE AM-CARRIER         TO SWA-CARR                      ECS046
00810          MOVE AM-GROUPING        TO SWA-GROUP                     ECS046
00811          MOVE AM-STATE           TO SWA-STATE                     ECS046
00812          MOVE SPACES             TO SWA-GA-OR-RPT-CD.             ECS046
00813                                                                   ECS046
00814      IF DTE-TOT-OPT = 2                                           ECS046
00815          MOVE AM-GPCD            TO SWA-BUS-TYPE                  ECS046
00816          MOVE AM-CARRIER         TO SWA-CARR                      ECS046
00817          MOVE AM-GROUPING        TO SWA-GROUP                     ECS046
00818          MOVE AM-STATE           TO SWA-STATE                     ECS046
00819          MOVE SPACES             TO SWA-GA-OR-RPT-CD.             ECS046
00820                                                                   ECS046
00821 * IF A GIVEN ACCOUNT DOESN'T HAVE ALTERNATE REPORT CODES,         ECS046
00822 * THAT ACCOUNT DOESN'T APPEAR ON THE REPORT FOR TOTAL OPTIONS     ECS046
00823 * 3 AND 4.                                                        ECS046
00824                                                                   ECS046
00825      IF DTE-TOT-OPT = 3                                           ECS046
00826          IF AM-REPORT-CODE-1 = SPACES OR LOW-VALUES               ECS046
00827              GO TO 1600-PERFORM-ZERO-ACCUM                        ECS046
00828          ELSE                                                     ECS046
00829              MOVE SPACES           TO SWA-BUS-TYPE                ECS046
00830                                       SWA-CARR                    ECS046
00831                                       SWA-GROUP                   ECS046
00832                                       SWA-STATE                   ECS046
00833              MOVE AM-REPORT-CODE-1 TO SWA-GA-OR-RPT-CD.           ECS046
00834                                                                   ECS046
00835      IF DTE-TOT-OPT = 4                                           ECS046
00836          IF AM-REPORT-CODE-2 = SPACES OR LOW-VALUES               ECS046
00837              GO TO 1600-PERFORM-ZERO-ACCUM                        ECS046
00838          ELSE                                                     ECS046
00839              MOVE SPACES           TO SWA-BUS-TYPE                ECS046
00840              MOVE AM-CARRIER       TO SWA-CARR                    ECS046
00841              MOVE AM-GROUPING      TO SWA-GROUP                   ECS046
00842              MOVE SPACES           TO SWA-STATE                   ECS046
00843              MOVE AM-REPORT-CODE-2 TO SWA-GA-OR-RPT-CD.           ECS046
00844                                                                   ECS046
00845      IF DTE-TOT-OPT = 6                                           ECS046
00846          IF AM-REPORT-CODE-1 = SPACES OR LOW-VALUES               ECS046
00847              GO TO 1600-PERFORM-ZERO-ACCUM                        ECS046
00848          ELSE                                                     ECS046
00849              MOVE AM-REPORT-CODE-1 TO SWA-RPT-CD-1                ECS046
00850              MOVE AM-REPORT-CODE-2 TO SWA-RPT-CD-2.               ECS046
00851                                                                   ECS046
00852 * IF A GIVEN ACCOUNT DOESN'T HAVE A FINANCIAL RESPONSIBLE G-A,    ECS046
00853 * THAT ACCOUNT DOESN'T APPEAR ON THE REPORT FOR TOTAL OPTION 5.   ECS046
00854                                                                   ECS046
00855      IF DTE-TOT-OPT = 5                                           ECS046
00856          IF WS-FIN-RESP-AGT = SPACES  OR  ZEROS                   ECS046
00857              GO TO 1600-PERFORM-ZERO-ACCUM                        ECS046
00858          ELSE                                                     ECS046
00859              MOVE SPACES           TO SWA-BUS-TYPE                ECS046
00860              MOVE AM-CARRIER       TO SWA-CARR                    ECS046
00861              MOVE AM-GROUPING      TO SWA-SAVE-GROUP              ECS046
00862              MOVE SPACES           TO SWA-GROUP                   ECS046
00863                                       SWA-STATE                   ECS046
00864              MOVE WS-FIN-RESP-AGT  TO SWA-GA-OR-RPT-CD.           ECS046
00865                                                                   ECS046
00866      COMPUTE SWA-NET-PREM (1) = DA-NET-PREM (3) - DA-NET-PREM (1).ECS046
00867      COMPUTE SWA-EARN-PREM (1) =                                  ECS046
00868                              DA-EARN-PREM (3) - DA-EARN-PREM (1). ECS046
00869      COMPUTE SWA-CLMS-INCUR (1) =                                 ECS046
00870                           (DA-CLAIM-AMT (3) - DA-CLAIM-AMT (1)) + ECS046
00871                           (DA-LOSS-RSRV (3) - DA-LOSS-RSRV (1)).  ECS046
00872      COMPUTE SWA-EARN-COMP (1) =                                  ECS046
00873                              DA-EARN-COMP (3) - DA-EARN-COMP (1). ECS046
00874      COMPUTE SWA-RETENTION (1) =                                  ECS046
00875                              DA-RETENTION (3) - DA-RETENTION (1). ECS046
00876      COMPUTE SWA-PREM-TAX (1) = DA-PREM-TAX (3) - DA-PREM-TAX (1).ECS046
00877                                                                   ECS046
00878      COMPUTE SWA-NET-PREM (2) = DA-NET-PREM (4) - DA-NET-PREM (2).ECS046
00879      COMPUTE SWA-EARN-PREM (2) =                                  ECS046
00880                              DA-EARN-PREM (4) - DA-EARN-PREM (2). ECS046
00881      COMPUTE SWA-CLMS-INCUR (2) =                                 ECS046
00882                           (DA-CLAIM-AMT (4) - DA-CLAIM-AMT (2)) + ECS046
00883                           (DA-LOSS-RSRV (4) - DA-LOSS-RSRV (2)).  ECS046
00884      COMPUTE SWA-EARN-COMP (2) =                                  ECS046
00885                              DA-EARN-COMP (4) - DA-EARN-COMP (2). ECS046
00886      COMPUTE SWA-RETENTION (2) =                                  ECS046
00887                              DA-RETENTION (4) - DA-RETENTION (2). ECS046
00888      COMPUTE SWA-PREM-TAX (2) = DA-PREM-TAX (4) - DA-PREM-TAX (2).ECS046
00889                                                                   ECS046
00890      RELEASE SWA-REC-OUT FROM SWA-REC.                            ECS046
00891                                                                   ECS046
00892      ADD +1 TO NO-OF-RECORDS-RELEASED.                            ECS046
00893                                                                   ECS046
00894  1600-PERFORM-ZERO-ACCUM.                                         ECS046
00895      PERFORM 2400-ZERO-DETAIL-ACCUMULATORS                        ECS046
00896          VARYING SA FROM 1 BY 1 UNTIL SA GREATER 4.               ECS046
00897                                                                   ECS046
00898      EJECT                                                        ECS046
00899  1700-PROCESS-NEXT-EPEC.                                          ECS046
00900      IF EP-CONTROL = HIGH-VALUES                                  ECS046
00901          GO TO 1990-EXIT.                                         ECS046
00902                                                                   ECS046
00903      MOVE EP-CONTROL             TO WS-SAVE-CONTROL.              ECS046
00904                                                                   ECS046
00905  1800-CHECK-ACCT-MSTR.                                            ECS046
00906      IF WS-ACCT-CONTROL LESS WS-SAVE-CONTROL                      ECS046
00907          PERFORM 2200-READ-ACCT THRU 2240-EXIT                    ECS046
00908          GO TO 1800-CHECK-ACCT-MSTR.                              ECS046
00909                                                                   ECS046
00910  1820-NO-ACCT-MSTR.                                               ECS046
00911      IF WS-ACCT-CONTROL GREATER WS-SAVE-CONTROL                   ECS046
00912          PERFORM 2300-NO-ACCT-MSG                                 ECS046
00913          PERFORM 2100-READ-EXTRACT THRU 2140-EXIT                 ECS046
00914            GO TO 1700-PROCESS-NEXT-EPEC.                          ECS046
00915                                                                   ECS046
00916  1840-ACCT-MSTR-MATCHED.                                          ECS046
00917                                                                   ECS046
00918      IF DTE-CLIENT  =  'MON'                                      ECS046
00919          IF WS-ACTIVE-INDIC  =  'ACTIVE'                          ECS046
00920              IF AM-STATUS  =  '1'                                 ECS046
00921                  PERFORM 2100-READ-EXTRACT THRU 2140-EXIT         ECS046
00922                  GO TO 1700-PROCESS-NEXT-EPEC.                    ECS046
00923                                                                   ECS046
00924      IF DTE-CLIENT = 'CSO'                                        ECS046
00925          IF AM-STATUS NOT = 'A'  AND  '0'                         ECS046
00926              PERFORM 2100-READ-EXTRACT THRU 2140-EXIT             ECS046
00927              GO TO 1700-PROCESS-NEXT-EPEC.                        ECS046
00928                                                                   ECS046
00929  1950-SAVE-ACCT-DATA.                                             ECS046
00930                                                                   ECS046
00931      MOVE SPACES                 TO WS-FIN-RESP-AGT.              ECS046
00932                                                                   ECS046
00933 * RETENTION AND TAX PERCENTS ARE FOUND IN THE ACCOUNT MASTER      ECS046
00934                                                                   ECS046
00935      IF AM-LF-RET NOT NUMERIC                                     ECS046
00936          MOVE ZERO               TO AM-LF-RET.                    ECS046
00937                                                                   ECS046
00938      IF AM-AH-RET NOT NUMERIC                                     ECS046
00939          MOVE ZERO               TO AM-AH-RET.                    ECS046
00940                                                                   ECS046
00941      IF AM-REI-LF-TAX NOT NUMERIC                                 ECS046
00942          MOVE ZERO               TO AM-REI-LF-TAX.                ECS046
00943                                                                   ECS046
00944      IF AM-REI-AH-TAX NOT NUMERIC                                 ECS046
00945          MOVE ZERO               TO AM-REI-AH-TAX.                ECS046
00946                                                                   ECS046
00947      IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                            ECS046
00948          MOVE AM-LF-RET          TO WS-RETENTION-PCT              ECS046
00949          MOVE AM-REI-LF-TAX      TO WS-TAX-RATE                   ECS046
00950      ELSE                                                         ECS046
00951          MOVE AM-AH-RET          TO WS-RETENTION-PCT              ECS046
00952          MOVE AM-REI-AH-TAX      TO WS-TAX-RATE.                  ECS046
00953                                                                   ECS046
00954      MOVE +0                     TO CL.                           ECS046
00955                                                                   ECS046
00956      EJECT                                                        ECS046
00957  1970-PROCESS-COMM-STRUCT.                                        ECS046
00958                                                                   ECS046
00959 * THIS ROUTINE LOCATES G-A'S IN THE COMMISSION STRUCTURE OF THE   ECS046
00960 * ACCOUNT MASTER AND SUBTRACTS THE G-A'S COMMISSION PERCENT FROM  ECS046
00961 * THE ACCOUNT RETENTION, IF NECESSARY.  THIS ROUTINE ALSO LOCATES ECS046
00962 * THE FINANCIALLY RESPONSIBLE G-A FOR THIS ACCOUNT (THE FIRST     ECS046
00963 * G-A IN THE COMMISSION STRUCTURE).  THE FINANCIALLY RESPONSIBLE  ECS046
00964 * G-A IS USED FOR TOTAL OPTION 5.                                 ECS046
00965                                                                   ECS046
00966      IF DTE-CLIENT  =  'MON'                                      ECS046
00967          NEXT SENTENCE                                            ECS046
00968      ELSE                                                         ECS046
00969          GO TO 1975-BYPASS-MON-CODE.                              ECS046
00970                                                                   ECS046
00971      MOVE +9                     TO  CL.                          ECS046
00972                                                                   ECS046
00973      IF AM-RETRO-LV-INDIC (CL)  =  'Y'                            ECS046
00974          IF AM-J-COM (CL)  NOT NUMERIC                            ECS046
00975              MOVE ZERO           TO  AM-J-COM (CL).               ECS046
00976                                                                   ECS046
00977      IF AM-L-COM (CL)  NOT NUMERIC                                ECS046
00978          MOVE ZERO               TO  AM-L-COM (CL).               ECS046
00979                                                                   ECS046
00980      IF AM-A-COM (CL)  NOT NUMERIC                                ECS046
00981          MOVE ZERO               TO  AM-A-COM (CL).               ECS046
00982                                                                   ECS046
00983      IF EP-RCD-TYPE  =  LIFE-OVERRIDE-L1                          ECS046
00984          IF CLAS-I-JOINT (CLAS-INDEXL)  =  'J'                    ECS046
00985              SUBTRACT AM-J-COM (CL)  FROM  WS-RETENTION-PCT       ECS046
00986          ELSE                                                     ECS046
00987              SUBTRACT AM-L-COM (CL)  FROM  WS-RETENTION-PCT       ECS046
00988      ELSE                                                         ECS046
00989          SUBTRACT AM-A-COM (CL)  FROM  WS-RETENTION-PCT.          ECS046
00990                                                                   ECS046
00991      IF AM-RET-Y-N  =  'N'  OR  ' '                               ECS046
00992          MOVE ZEROS              TO  WS-RETENTION-PCT.            ECS046
00993                                                                   ECS046
00994      ADD +.06                    TO  WS-TAX-RATE.                 ECS046
00995                                                                   ECS046
00996      GO TO 1980-EXIT.                                             ECS046
00997                                                                   ECS046
00998  1975-BYPASS-MON-CODE.                                            ECS046
00999      ADD +1 TO CL.                                                ECS046
01000                                                                   ECS046
01001      IF CL GREATER +10                                            ECS046
01002          GO TO 1980-EXIT.                                         ECS046
01003                                                                   ECS046
052814     IF AM-COM-TYP (CL) = 'O' OR 'P' or 'S'
01005          NEXT SENTENCE                                            ECS046
01006      ELSE                                                         ECS046
01007          GO TO 1970-PROCESS-COMM-STRUCT.                          ECS046
01008                                                                   ECS046
01009      IF AM-J-COM (CL) NOT NUMERIC                                 ECS046
01010          MOVE ZERO               TO AM-J-COM (CL).                ECS046
01011                                                                   ECS046
01012      IF AM-L-COM (CL) NOT NUMERIC                                 ECS046
01013          MOVE ZERO               TO AM-L-COM (CL).                ECS046
01014                                                                   ECS046
01015      IF AM-A-COM (CL) NOT NUMERIC                                 ECS046
01016          MOVE ZERO               TO AM-A-COM (CL).                ECS046
01017                                                                   ECS046
01018      IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                            ECS046
01019          IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                      ECS046
01020              SUBTRACT AM-J-COM (CL) FROM WS-RETENTION-PCT         ECS046
01021          ELSE                                                     ECS046
01022              SUBTRACT AM-L-COM (CL) FROM WS-RETENTION-PCT         ECS046
01023      ELSE                                                         ECS046
01024          SUBTRACT AM-A-COM (CL) FROM WS-RETENTION-PCT.            ECS046
01025                                                                   ECS046
01026      GO TO 1970-PROCESS-COMM-STRUCT.                              ECS046
01027                                                                   ECS046
01028  1980-EXIT.                                                       ECS046
01029      MOVE SPACES                 TO WS-FIN-RESP-AGT.              ECS046
01030                                                                   ECS046
01031      IF DTE-CLIENT = 'MON'                                        ECS046
01032          MOVE AM-AGT (9)         TO WS-FIN-RESP-AGT               ECS046
01033      ELSE                                                         ECS046
052814         IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' or 'S'
01035              MOVE AM-AGT (AM-REMIT-TO)  TO WS-FIN-RESP-AGT.       ECS046
01036                                                                   ECS046
01037      IF WS-RETENTION-PCT LESS ZERO                                ECS046
01038          MOVE ZERO               TO WS-RETENTION-PCT.             ECS046
01039                                                                   ECS046
01040      IF DTE-CLIENT = 'CSO'                                        ECS046
01041          MOVE ZERO               TO WS-RETENTION-PCT.             ECS046
01042                                                                   ECS046
01043      GO TO 1200-CHECK-CONTROL.                                    ECS046
01044                                                                   ECS046
01045  1990-EXIT.                                                       ECS046
01046      CLOSE ACC-MSTR                                               ECS046
01047            EPEC-FILE.                                             ECS046
01048                                                                   ECS046
01049  EJECT                                                            ECS046
01050  2000-PERFORMED-INPUT-ROUTINES SECTION.                           ECS046
01051                                                                   ECS046
01052  2100-READ-EXTRACT.                                               ECS046
01053                                                                   ECS046
01054      READ EPEC-FILE AT END                                        ECS046
01055          MOVE HIGH-VALUES        TO EP-CONTROL                    ECS046
01056          GO TO 2140-EXIT.                                         ECS046
01057                                                                   ECS046
01058      IF EP-RECORD-ID NOT = 'EP' AND 'EC'                          ECS046
01059          GO TO 2100-READ-EXTRACT.                                 ECS046
01060                                                                   ECS046
01061      COPY ELCEPCM1.                                               ECS046
01062                                                                   ECS046
01063 * REINSURANCE RECORDS IN THE EPEC FILE ARE BYPASSED.              ECS046
01064      IF EP-REIN = 'R'                                             ECS046
01065          GO TO 2100-READ-EXTRACT.                                 ECS046
01066                                                                   ECS046
01067      IF WS-EP-RUN-DTE GREATER RUN-DT                              ECS046
01068          GO TO 2100-READ-EXTRACT.                                 ECS046
01069                                                                   ECS046
01070      IF EP-PURGE = 'P'  OR                                        ECS046
01071        (EP-RUN-YR = RD-YR AND EP-RUN-MO = RD-MO)  OR              ECS046
01072        (EP-RUN-YR = BD-YR AND EP-RUN-MO = BD-MO)                  ECS046
01073         NEXT SENTENCE                                             ECS046
01074      ELSE                                                         ECS046
01075          GO TO 2100-READ-EXTRACT.                                 ECS046
01076                                                                   ECS046
01077      IF EP-STATE EQUAL SAVE-STATE                                 ECS046
01078         GO TO 2105-BENE-SEARCH.                                   ECS046
01079                                                                   ECS046
01080      MOVE SPACES                 TO SAVE-LF-BEN.                  ECS046
01081      MOVE CLAS-STARTS            TO CLAS-INDEXS.                  ECS046
01082                                                                   ECS046
01083  2110-STATE-LOOK-UP.                                              ECS046
01084                                                                   ECS046
01085 * THE GIVEN STATE THAT THE ACCOUNT IS LOCATED IN IS FOUND         ECS046
01086 * IN THE STATE TABLE IN THE DATE CARD WORKING STORAGE AREA        ECS046
01087 * TO DETERMINE IF THE STATE CODE IS FOR WYOMING.  THIS            ECS046
01088 * IS DONE BECAUSE ALL WYOMING ACCOUNTS MUST USE THE PRO-RATA      ECS046
01089 * METHOD FOR DETERMINING EARNED PREMIUM.                          ECS046
01090                                                                   ECS046
01091      IF EP-STATE = STATE-SUB (CLAS-INDEXS)                        ECS046
01092         MOVE EP-STATE    TO SAVE-STATE                            ECS046
01093      ELSE                                                         ECS046
01094          ADD +1 TO CLAS-INDEXS                                    ECS046
01095          IF CLAS-INDEXS NOT GREATER CLAS-MAXS                     ECS046
01096              GO TO 2110-STATE-LOOK-UP                             ECS046
01097          ELSE                                                     ECS046
01098              DISPLAY ' INVALID STATE CODE -' AM-STATE             ECS046
01099              MOVE    ' INVALID STATE CODE -' TO WS-ABEND-MESSAGE  ECS046
01100              MOVE 0402           TO WS-RETURN-CODE                ECS046
01101              GO TO ABEND-PGM.                                     ECS046
01102                                                                   ECS046
01103  2105-BENE-SEARCH.                                                ECS046
01104                                                                   ECS046
01105      IF EP-RCD-TYPE = AH-OVERRIDE-L1                              ECS046
01106          GO TO 2120-EP-EARN-AH.                                   ECS046
01107                                                                   ECS046
01108      IF EP-BEN-CODE EQUAL SAVE-LF-BEN                             ECS046
01109          MOVE SAVE-EP-CODE       TO WS-EP-CODE                    ECS046
01110          GO TO 2140-EXIT.                                         ECS046
01111                                                                   ECS046
01112      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  ECS046
01113                                                                   ECS046
01114  2110-FIND-BENE.                                                  ECS046
01115                                                                   ECS046
01116      IF CLAS-INDEXL GREATER CLAS-MAXL OR                          ECS046
01117         CLAS-INDEXL = ZERO                                        ECS046
01118          DISPLAY ' INVALID LIFE BENEFIT TYPE -' EP-BEN-CODE       ECS046
01119          MOVE   ' INVALID LIFE BENEFIT TYPE -' TO WS-ABEND-MESSAGEECS046
01120          MOVE 0401               TO WS-RETURN-CODE                ECS046
01121          GO TO ABEND-PGM.                                         ECS046
01122                                                                   ECS046
01123      IF EP-BEN-CODE NOT = CLAS-I-BEN (CLAS-INDEXL)                ECS046
01124          ADD +1 TO CLAS-INDEXL                                    ECS046
01125          GO TO 2110-FIND-BENE.                                    ECS046
01126                                                                   ECS046
01127      MOVE EP-BEN-CODE             TO SAVE-LF-BEN.                 ECS046
01128      MOVE CLAS-I-EP (CLAS-INDEXL) TO WS-EP-CODE                   ECS046
01129                                      SAVE-EP-CODE.                ECS046
01130                                                                   ECS046
01131      IF STATE-ABBR (CLAS-INDEXS) = 'WY'                           ECS046
01132          MOVE 'P'                 TO WS-EP-CODE                   ECS046
01133                                      SAVE-EP-CODE.                ECS046
01134                                                                   ECS046
01135      IF DTE-CLIENT  =  'MON'                                      ECS046
01136          IF WS-EP-CODE  NOT =  'P'                                ECS046
01137              MOVE 'R'            TO  WS-EP-CODE.                  ECS046
01138                                                                   ECS046
01139      GO TO 2140-EXIT.                                             ECS046
01140                                                                   ECS046
01141  2120-EP-EARN-AH.                                                 ECS046
01142                                                                   ECS046
01143 * THIS REPORT DEFAULTS TO THE MEAN EARNING METHOD FOR A & H.      ECS046
01144                                                                   ECS046
01145      MOVE SPACES                 TO  SAVE-LF-BEN.                 ECS046
01146                                                                   ECS046
01147      IF DTE-CLIENT  =  'MON'                                      ECS046
01148          MOVE 'R'                TO  WS-EP-CODE                   ECS046
01149      ELSE                                                         ECS046
01150          MOVE 'M'                TO  WS-EP-CODE.                  ECS046
01151                                                                   ECS046
01152  2140-EXIT.                                                       ECS046
01153      EXIT.                                                        ECS046
01154                                                                   ECS046
01155  2200-READ-ACCT.                                                  ECS046
01156      READ ACC-MSTR AT END                                         ECS046
01157              MOVE ZEROS          TO AM-MSTR-CNTRL                    CL**9
01158                                     AM-EFFECT-DT.                 ECS046
01159                                                                   ECS046
01160      MOVE AM-MSTR-CNTRL          TO WS-MSTR-CNTRL.                ECS046
01161      MOVE AM-EFFECT-DT           TO WS-EFFECT.                    ECS046
01162                                                                   ECS046
01163  2240-EXIT.                                                       ECS046
01164       EXIT.                                                       ECS046
01165                                                                   ECS046
01166      EJECT                                                        ECS046
01167  2300-NO-ACCT-MSG.                                                ECS046
01168      IF DTE-CLIENT  =  'MON'                                      ECS046
01169          IF WS-ACTIVE-INDIC  =  'ACTIVE'                          ECS046
01170              MOVE HOLD-ACTIVE-DESC                                ECS046
01171                                  TO  HD-STATUS-DESC               ECS046
01172          ELSE                                                     ECS046
01173              MOVE HOLD-ALL-DESC  TO  HD-STATUS-DESC               ECS046
01174      ELSE                                                         ECS046
01175          MOVE SPACES             TO  HD-STATUS-DESC.              ECS046
01176                                                                   ECS046
01177      IF LINE-CNT GREATER +55                                      ECS046
01178          MOVE ZERO               TO LINE-CNT                      ECS046
01179          MOVE '1'                TO X                             ECS046
01180          MOVE HEAD-1             TO P-DATA                        ECS046
01181          PERFORM 4600-PRINT-ROUTINE                               ECS046
01182          MOVE ' '                TO X                             ECS046
01183          MOVE HEAD-2             TO P-DATA                        ECS046
01184          PERFORM 4600-PRINT-ROUTINE                               ECS046
01185          MOVE ' '                TO X                             ECS046
01186          MOVE HEAD-3             TO P-DATA                        ECS046
01187          PERFORM 4600-PRINT-ROUTINE.                              ECS046
01188                                                                   ECS046
01189      MOVE WS-SAVE-CARR           TO DNA-CARR.                     ECS046
01190      MOVE WS-SAVE-GROUP          TO DNA-GROUP.                    ECS046
01191      MOVE WS-SAVE-STATE          TO DNA-ST.                       ECS046
01192      MOVE WS-SAVE-ACCT           TO DNA-ACCT.                     ECS046
01193      MOVE WS-SAVE-EXP-DATE       TO WS-SAVE-EXP-DATE-R.           ECS046
01194      MOVE WS-SAVE-EXP-YR         TO DNA-TYR.                      ECS046
01195      MOVE WS-SAVE-EXP-MO         TO DNA-TMO.                      ECS046
01196      MOVE WS-SAVE-EXP-DA         TO DNA-TDA.                      ECS046
01197      MOVE WS-SAVE-EFF-DATE       TO WS-SAVE-EFF-DATE-R.           ECS046
01198      MOVE WS-SAVE-EFF-YR         TO DNA-FYR.                      ECS046
01199      MOVE WS-SAVE-EFF-MO         TO DNA-FMO.                      ECS046
01200      MOVE WS-SAVE-EFF-DA         TO DNA-FDA.                      ECS046
01201      MOVE ' NO MATCHING ACCOUNT MASTER' TO DNA-MESSAGE.           ECS046
01202      MOVE ' '                    TO X.                            ECS046
01203      MOVE DTL-NO-ACCT            TO P-DATA.                       ECS046
01204      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01205                                                                   ECS046
01206  2400-ZERO-DETAIL-ACCUMULATORS.                                   ECS046
01207      MOVE +0                     TO DA-NET-PREM  (SA)             ECS046
01208                                     DA-EARN-PREM (SA)             ECS046
01209                                     DA-CLAIM-AMT (SA)             ECS046
01210                                     DA-LOSS-RSRV (SA)             ECS046
01211                                     DA-EARN-COMP (SA)             ECS046
01212                                     DA-RETENTION (SA)             ECS046
01213                                     DA-PREM-TAX  (SA).            ECS046
01214                                                                   ECS046
01215      EJECT                                                        ECS046
01216  3000-PRINT-REPORT-A   SECTION.                                   ECS046
01217                                                                   ECS046
01218      OPEN INPUT  COMP-FILE                                        ECS046
01219           OUTPUT WORK-FILE.                                       ECS046
01220                                                                   ECS046
01221      IF NO-OF-RECORDS-RELEASED = +0                               ECS046
01222          GO TO 3990-PRINT-RPT-A-EXIT.                             ECS046
01223                                                                   ECS046
01224      MOVE ZERO                   TO NO-OF-RECORDS-RELEASED.       ECS046
01225                                                                   ECS046
01226      MOVE +1                     TO SB.                           ECS046
01227      PERFORM 4400-ZERO-BREAK-ACCUMULATORS                         ECS046
01228          VARYING SE FROM 1 BY 1 UNTIL SE GREATER 6.               ECS046
01229                                                                   ECS046
01230      MOVE +2                     TO SB.                           ECS046
01231      PERFORM 4400-ZERO-BREAK-ACCUMULATORS                         ECS046
01232          VARYING SE FROM 1 BY 1 UNTIL SE GREATER 6.               ECS046
01233                                                                   ECS046
01234      PERFORM 3100-RETURN-SORT-WORK-A.                             ECS046
01235                                                                   ECS046
01236      MOVE SWA-REC-OUT            TO SWA-REC.                      ECS046
01237      MOVE SWA-CONTROL            TO SAVE-SWA-CONTROL.             ECS046
01238                                                                   ECS046
01239      PERFORM 3500-GET-GA-NAME THRU 3500-EXIT.                     ECS046
01240                                                                   ECS046
01241      IF DTE-PGM-OPT = 1                                           ECS046
01242          MOVE '- YEAR TO DATE'   TO HD-RPT-TYPE.                  ECS046
01243                                                                   ECS046
01244      IF DTE-PGM-OPT = 2                                           ECS046
01245          MOVE '- LAST 12 MONTHS' TO HD-RPT-TYPE.                  ECS046
01246                                                                   ECS046
01247      IF DTE-PGM-OPT = 3                                           ECS046
01248          MOVE '- INCEPTION TO DATE' TO HD-RPT-TYPE.               ECS046
01249                                                                   ECS046
01250      IF DTE-PGM-OPT = 4                                           ECS046
01251          MOVE '- CURRENT MONTH'  TO HD-RPT-TYPE.                  ECS046
01252                                                                   ECS046
01253      IF DTE-TOT-OPT = 3                                           ECS046
01254          MOVE CLAS-REPORT-CD1-CAPTION TO HD-RPT-CAPTION.          ECS046
01255                                                                   ECS046
01256      IF DTE-TOT-OPT = 4                                           ECS046
01257          MOVE CLAS-REPORT-CD2-CAPTION TO HD-RPT-CAPTION.          ECS046
01258                                                                   ECS046
01259      MOVE 'A'                    TO H1-REPORT-SUF.                ECS046
01260      PERFORM 4500-PRINT-HEADINGS THRU 4580-DTL-HDRS.              ECS046
01261                                                                   ECS046
01262                                                                   ECS046
01263      EJECT                                                        ECS046
01264  3100-RETURN-SORT-WORK-A.                                         ECS046
01265      RETURN SORT-WORK-A                                           ECS046
01266                 AT END  MOVE HIGH-VALUES   TO SWA-CONTROL-OUT.    ECS046
01267                                                                   ECS046
01268  3150-CHECK-FOR-CTL-BREAK.                                        ECS046
01269                                                                   ECS046
01270      IF SWA-CONTROL-OUT = SWA-CONTROL                             ECS046
01271        COMPUTE SWA-NET-PREM (1) = SWA-NET-PREM (1) + SW-NP (1)    ECS046
01272        COMPUTE SWA-EARN-PREM (1) = SWA-EARN-PREM (1) + SW-EP (1)  ECS046
01273        COMPUTE SWA-CLMS-INCUR (1) = SWA-CLMS-INCUR (1) + SW-CI (1)ECS046
01274        COMPUTE SWA-EARN-COMP (1) = SWA-EARN-COMP (1) + SW-EC (1)  ECS046
01275        COMPUTE SWA-RETENTION (1) = SWA-RETENTION (1) + SW-RT (1)  ECS046
01276        COMPUTE SWA-PREM-TAX (1) = SWA-PREM-TAX (1) + SW-PT (1)    ECS046
01277        COMPUTE SWA-NET-PREM (2) = SWA-NET-PREM (2) + SW-NP (2)    ECS046
01278        COMPUTE SWA-EARN-PREM (2) = SWA-EARN-PREM (2) + SW-EP (2)  ECS046
01279        COMPUTE SWA-CLMS-INCUR (2) = SWA-CLMS-INCUR (2) + SW-CI (2)ECS046
01280        COMPUTE SWA-EARN-COMP (2) = SWA-EARN-COMP (2) + SW-EC (2)  ECS046
01281        COMPUTE SWA-RETENTION (2) = SWA-RETENTION (2) + SW-RT (2)  ECS046
01282        COMPUTE SWA-PREM-TAX (2) = SWA-PREM-TAX (2) + SW-PT (2)    ECS046
01283            GO TO 3100-RETURN-SORT-WORK-A.                         ECS046
01284                                                                   ECS046
01285      IF SWA-CONTROL-1 NOT = SAVE-SWA-CONTROL-1                    ECS046
01286          PERFORM 4200-SWA-CONTROL-BREAK                           ECS046
01287          PERFORM 3500-GET-GA-NAME THRU 3500-EXIT.                 ECS046
01288                                                                   ECS046
01289      MOVE SWA-CONTROL            TO SAVE-SWA-CONTROL.             ECS046
01290                                                                   ECS046
01291      EJECT                                                        ECS046
01292  3200-BUILD-DTL-LINE-A.                                           ECS046
01293      IF WS-BREAK-SW = 'Y'                                         ECS046
01294          MOVE ZERO               TO LINE-CNT                      ECS046
01295          MOVE 'N'                TO WS-BREAK-SW                   ECS046
01296          PERFORM 4510-BUS-TYPE-HDR THRU 4580-DTL-HDRS.            ECS046
01297                                                                   ECS046
01298      IF LINE-CNT GREATER +48                                      ECS046
01299          MOVE ZERO               TO LINE-CNT                      ECS046
01300          PERFORM 4500-PRINT-HEADINGS THRU 4580-DTL-HDRS.          ECS046
01301                                                                   ECS046
01302      MOVE ' '                    TO X.                            ECS046
01303      MOVE ALL '-'                TO P-DATA.                       ECS046
01304      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01305                                                                   ECS046
01306      MOVE SWA-ACCT               TO HA-ACCOUNT.                   ECS046
01307      MOVE SWA-NAME               TO HA-ACCT-NAME.                 ECS046
01308      MOVE SWA-CITY               TO HA-ACCT-CITY.                 ECS046
01309      MOVE ' '                    TO X.                            ECS046
01310      MOVE HEAD-ACCT              TO P-DATA.                       ECS046
01311      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01312                                                                   ECS046
01313      MOVE LIFE-OVERRIDE-L2       TO DLA-BEN-DESC.                 ECS046
01314                                                                   ECS046
01315      COMPUTE  DLA-NET-PREM ROUNDED   = SWA-NET-PREM   (1).        ECS046
01316      COMPUTE  DLA-EARN-PREM ROUNDED  = SWA-EARN-PREM  (1).        ECS046
01317      COMPUTE  DLA-CLMS-INCUR ROUNDED = SWA-CLMS-INCUR (1).        ECS046
01318      COMPUTE  DLA-EARN-COMP ROUNDED  = SWA-EARN-COMP  (1).        ECS046
01319      COMPUTE WS-LF-SURPLUS ROUNDED   = SWA-EARN-PREM  (1) -       ECS046
01320                   (SWA-CLMS-INCUR (1) + SWA-EARN-COMP (1)).       ECS046
01321      COMPUTE  DLA-SURPLUS ROUNDED    = WS-LF-SURPLUS.             ECS046
01322      COMPUTE  DLA-RETENTION ROUNDED  = SWA-RETENTION  (1).        ECS046
01323                                                                   ECS046
01324      IF SWA-RETENTION (1) = ZERO     OR                           ECS046
01325         SWA-RETENTION (1) GREATER WS-LF-SURPLUS                   ECS046
01326          MOVE WS-LF-SURPLUS      TO WS-LF-PREM-PROFIT             ECS046
01327      ELSE                                                         ECS046
01328          MOVE SWA-RETENTION (1)  TO WS-LF-PREM-PROFIT.            ECS046
01329                                                                   ECS046
01330      COMPUTE  DLA-PREM-PROFIT ROUNDED = WS-LF-PREM-PROFIT.        ECS046
01331      COMPUTE  DLA-PREM-TAX ROUNDED    = SWA-PREM-TAX (1).         ECS046
01332      COMPUTE WS-LF-UW-INCOME ROUNDED  =                           ECS046
01333                 WS-LF-PREM-PROFIT - SWA-PREM-TAX (1).             ECS046
01334      COMPUTE  DLA-UW-INCOME ROUNDED   = WS-LF-UW-INCOME.          ECS046
01335      EJECT                                                        ECS046
01336      IF SWA-EARN-PREM (1) = ZERO                                  ECS046
01337          MOVE '*******'          TO DLA-LOSS-RATIO-RD             ECS046
01338                                     DLA-TOT-CHARGES-RD            ECS046
01339          MOVE ZERO               TO DLA-COMP-RATIO                ECS046
01340      ELSE                                                         ECS046
01341          COMPUTE WS-LOSS-RATIO ROUNDED   =                        ECS046
01342                    (SWA-CLMS-INCUR (1) / SWA-EARN-PREM (1)) * +100ECS046
01343          COMPUTE DLA-LOSS-RATIO ROUNDED  = WS-LOSS-RATIO          ECS046
01344          COMPUTE WS-COMP-RATIO ROUNDED   =                        ECS046
01345                     (SWA-EARN-COMP (1) / SWA-EARN-PREM (1)) * +100ECS046
01346          COMPUTE DLA-COMP-RATIO ROUNDED  = WS-COMP-RATIO          ECS046
01347          COMPUTE WS-TOT-CHARGES ROUNDED  =                        ECS046
01348               (+1 - (WS-LF-UW-INCOME / SWA-EARN-PREM (1))) * +100 ECS046
01349          COMPUTE DLA-TOT-CHARGES ROUNDED = WS-TOT-CHARGES.        ECS046
01350                                                                   ECS046
01351      MOVE '0'                    TO X.                            ECS046
01352      MOVE DTL-LINE-A             TO P-DATA.                       ECS046
01353      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01354      MOVE SPACES                 TO DTL-LINE-A   P-DATA.          ECS046
01355                                                                   ECS046
01356      EJECT                                                        ECS046
01357      MOVE AH-OVERRIDE-L2         TO DLA-BEN-DESC.                 ECS046
01358      COMPUTE  DLA-NET-PREM ROUNDED   = SWA-NET-PREM   (2).        ECS046
01359      COMPUTE  DLA-EARN-PREM ROUNDED  = SWA-EARN-PREM  (2).        ECS046
01360      COMPUTE  DLA-CLMS-INCUR ROUNDED = SWA-CLMS-INCUR (2).        ECS046
01361      COMPUTE  DLA-EARN-COMP ROUNDED  = SWA-EARN-COMP  (2).        ECS046
01362      COMPUTE WS-AH-SURPLUS ROUNDED   = SWA-EARN-PREM  (2) -       ECS046
01363                   (SWA-CLMS-INCUR (2) + SWA-EARN-COMP (2)).       ECS046
01364      COMPUTE  DLA-SURPLUS ROUNDED    = WS-AH-SURPLUS.             ECS046
01365      COMPUTE  DLA-RETENTION ROUNDED  = SWA-RETENTION  (2).        ECS046
01366                                                                   ECS046
01367      IF SWA-RETENTION (2) = ZERO     OR                           ECS046
01368         SWA-RETENTION (2) GREATER WS-AH-SURPLUS                   ECS046
01369          MOVE WS-AH-SURPLUS      TO WS-AH-PREM-PROFIT             ECS046
01370      ELSE                                                         ECS046
01371          MOVE SWA-RETENTION (2)  TO WS-AH-PREM-PROFIT.            ECS046
01372                                                                   ECS046
01373      COMPUTE  DLA-PREM-PROFIT ROUNDED = WS-AH-PREM-PROFIT.        ECS046
01374      COMPUTE  DLA-PREM-TAX ROUNDED    = SWA-PREM-TAX (2).         ECS046
01375      COMPUTE WS-AH-UW-INCOME =                                    ECS046
01376                 WS-AH-PREM-PROFIT - SWA-PREM-TAX (2).             ECS046
01377      COMPUTE  DLA-UW-INCOME ROUNDED   = WS-AH-UW-INCOME.          ECS046
01378                                                                   ECS046
01379      IF SWA-EARN-PREM (2) = ZERO                                  ECS046
01380          MOVE ZERO               TO DLA-LOSS-RATIO                ECS046
01381                                     DLA-COMP-RATIO                ECS046
01382                                     DLA-TOT-CHARGES               ECS046
01383      ELSE                                                         ECS046
01384          COMPUTE WS-LOSS-RATIO ROUNDED =                          ECS046
01385                    (SWA-CLMS-INCUR (2) / SWA-EARN-PREM (2)) * +100ECS046
01386          COMPUTE DLA-LOSS-RATIO ROUNDED = WS-LOSS-RATIO           ECS046
01387          COMPUTE WS-COMP-RATIO ROUNDED =                          ECS046
01388                     (SWA-EARN-COMP (2) / SWA-EARN-PREM (2)) * +100ECS046
01389          COMPUTE DLA-COMP-RATIO ROUNDED = WS-COMP-RATIO           ECS046
01390          COMPUTE WS-TOT-CHARGES ROUNDED =                         ECS046
01391               (+1 - (WS-AH-UW-INCOME / SWA-EARN-PREM (2))) * +100 ECS046
01392          COMPUTE DLA-TOT-CHARGES ROUNDED = WS-TOT-CHARGES.        ECS046
01393                                                                   ECS046
01394      MOVE ' '                    TO X.                            ECS046
01395      MOVE DTL-LINE-A             TO P-DATA.                       ECS046
01396      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01397      MOVE SPACES                 TO DTL-LINE-A   P-DATA.          ECS046
01398                                                                   ECS046
01399      EJECT                                                        ECS046
01400      MOVE 'TOT'                  TO DLA-BEN-DESC.                 ECS046
01401      COMPUTE DLA-NET-PREM ROUNDED =                               ECS046
01402                 SWA-NET-PREM (1) + SWA-NET-PREM (2).              ECS046
01403      COMPUTE DLA-EARN-PREM ROUNDED =                              ECS046
01404                 SWA-EARN-PREM (1) + SWA-EARN-PREM (2).            ECS046
01405      COMPUTE DLA-CLMS-INCUR ROUNDED =                             ECS046
01406                 SWA-CLMS-INCUR (1) + SWA-CLMS-INCUR (2).          ECS046
01407      COMPUTE DLA-EARN-COMP ROUNDED =                              ECS046
01408                 SWA-EARN-COMP (1) + SWA-EARN-COMP (2).            ECS046
01409      COMPUTE DLA-SURPLUS ROUNDED = WS-AH-SURPLUS + WS-LF-SURPLUS. ECS046
01410      COMPUTE DLA-RETENTION ROUNDED =                              ECS046
01411                 SWA-RETENTION (1) + SWA-RETENTION (2).            ECS046
01412      COMPUTE DLA-PREM-PROFIT ROUNDED =                            ECS046
01413                 WS-LF-PREM-PROFIT + WS-AH-PREM-PROFIT.            ECS046
01414      COMPUTE DLA-PREM-TAX ROUNDED =                               ECS046
01415                 SWA-PREM-TAX (1) + SWA-PREM-TAX (2).              ECS046
01416      COMPUTE DLA-UW-INCOME ROUNDED =                              ECS046
01417                 WS-LF-UW-INCOME + WS-AH-UW-INCOME.                ECS046
01418                                                                   ECS046
01419      IF SWA-EARN-PREM (1) = ZERO AND SWA-EARN-PREM (2) = ZERO     ECS046
01420          MOVE ZERO               TO DLA-LOSS-RATIO                ECS046
01421                                     DLA-COMP-RATIO                ECS046
01422                                     DLA-TOT-CHARGES               ECS046
01423      ELSE                                                         ECS046
01424          COMPUTE WS-LOSS-RATIO ROUNDED =                          ECS046
01425                 ((SWA-CLMS-INCUR (1) + SWA-CLMS-INCUR (2)) /      ECS046
01426                  (SWA-EARN-PREM (1) + SWA-EARN-PREM (2))) * +100  ECS046
01427          COMPUTE WS-COMP-RATIO ROUNDED =                          ECS046
01428                 ((SWA-EARN-COMP (1) + SWA-EARN-COMP (2)) /        ECS046
01429                  (SWA-EARN-PREM (1) + SWA-EARN-PREM (2))) * +100  ECS046
01430          COMPUTE WS-TOT-CHARGES ROUNDED =                         ECS046
01431            (+1 - (WS-LF-UW-INCOME + WS-AH-UW-INCOME) /            ECS046
01432                  (SWA-EARN-PREM (1) + SWA-EARN-PREM (2))) * +100  ECS046
01433          COMPUTE DLA-LOSS-RATIO ROUNDED  = WS-LOSS-RATIO          ECS046
01434          COMPUTE DLA-COMP-RATIO ROUNDED  = WS-COMP-RATIO          ECS046
01435          COMPUTE DLA-TOT-CHARGES ROUNDED = WS-TOT-CHARGES.        ECS046
01436                                                                   ECS046
01437      MOVE DTL-LINE-A             TO P-DATA.                       ECS046
01438      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01439      MOVE SPACES                 TO DTL-LINE-A   P-DATA.          ECS046
01440                                                                   ECS046
01441      EJECT                                                        ECS046
01442  3300-BUILD-SORT-WORK-B.                                          ECS046
01443      IF DTE-FMT-OPT = 1                                           ECS046
01444          GO TO 3400-ACCUMULATE-BREAK-TOTALS.                      ECS046
01445                                                                   ECS046
01446      IF SWA-EARN-PREM (1) = ZERO AND                              ECS046
01447         SWA-EARN-PREM (2) = ZERO                                  ECS046
01448          MOVE ZERO               TO SWB-LOSS-RATIO                ECS046
01449                                     SWB-TOT-CHARGES               ECS046
01450      ELSE                                                         ECS046
01451          COMPUTE WS-LOSS-RATIO ROUNDED =                          ECS046
01452                 ((SWA-CLMS-INCUR (1) + SWA-CLMS-INCUR (2)) /      ECS046
01453                  (SWA-EARN-PREM (1) + SWA-EARN-PREM (2))) * +100  ECS046
01454          COMPUTE SWB-LOSS-RATIO ROUNDED = WS-LOSS-RATIO           ECS046
01455          COMPUTE WS-TOT-CHARGES ROUNDED =                         ECS046
01456                  (+1 - (WS-LF-UW-INCOME + WS-AH-UW-INCOME) /      ECS046
01457                  (SWA-EARN-PREM (1) + SWA-EARN-PREM (2))) * +100  ECS046
01458          COMPUTE SWB-TOT-CHARGES ROUNDED = WS-TOT-CHARGES.        ECS046
01459                                                                   ECS046
01460      COMPUTE SWB-NET-PREM ROUNDED =                               ECS046
01461                 SWA-NET-PREM (1) + SWA-NET-PREM (2).              ECS046
01462      COMPUTE SWB-PREM-PROFIT ROUNDED =                            ECS046
01463                 WS-LF-PREM-PROFIT + WS-AH-PREM-PROFIT.            ECS046
01464                                                                   ECS046
01465      IF DTE-FMT-OPT = 2                                           ECS046
01466          MOVE SWB-LOSS-RATIO     TO SWB-SORT-AMT.                 ECS046
01467                                                                   ECS046
01468      IF DTE-FMT-OPT = 3                                           ECS046
01469          MOVE SWB-TOT-CHARGES    TO SWB-SORT-AMT.                 ECS046
01470                                                                   ECS046
01471      IF DTE-FMT-OPT = 4                                           ECS046
01472          MOVE SWB-NET-PREM       TO SWB-SORT-AMT.                 ECS046
01473                                                                   ECS046
01474      IF DTE-FMT-OPT = 5                                           ECS046
01475          MOVE SWB-PREM-PROFIT    TO SWB-SORT-AMT.                 ECS046
01476                                                                   ECS046
01477      MOVE SWA-BUS-TYPE           TO SWB-BUS-TYPE.                 ECS046
01478      MOVE SWA-CARR               TO SWB-CARR.                     ECS046
01479      MOVE SWA-GROUP              TO SWB-GROUP.                    ECS046
01480      MOVE SWA-STATE              TO SWB-STATE.                    ECS046
01481      MOVE SWA-GA-OR-RPT-CD       TO SWB-GA-OR-RPT-CD.             ECS046
01482      MOVE SWA-ACCT               TO SWB-ACCT.                     ECS046
01483      MOVE SWA-NAME               TO SWB-NAME.                     ECS046
01484                                                                   ECS046
01485      WRITE WORK-REC    FROM SWB-REC.                              ECS046
01486      ADD +1 TO NO-OF-RECORDS-RELEASED.                            ECS046
01487                                                                   ECS046
01488      EJECT                                                        ECS046
01489  3400-ACCUMULATE-BREAK-TOTALS.                                    ECS046
01490      PERFORM 4100-ACCUMULATE                                      ECS046
01491          VARYING SC FROM 1 BY 1 UNTIL SC GREATER 6.               ECS046
01492                                                                   ECS046
01493      MOVE SWA-REC-OUT            TO SWA-REC.                      ECS046
01494                                                                   ECS046
01495      IF SWA-CONTROL = HIGH-VALUES                                 ECS046
01496          PERFORM 4200-SWA-CONTROL-BREAK                           ECS046
01497          GO TO 3990-PRINT-RPT-A-EXIT.                             ECS046
01498                                                                   ECS046
01499      GO TO 3100-RETURN-SORT-WORK-A.                               ECS046
01500                                                                   ECS046
01501  3500-GET-GA-NAME.                                                ECS046
01502      MOVE SWA-CARR               TO  HOLD-CARRIER.                ECS046
01503      MOVE SWA-SAVE-GROUP         TO  HOLD-GROUPING.               ECS046
01504      MOVE SWA-GA-OR-RPT-CD       TO  HOLD-GA-NUMBER.              ECS046
01505      MOVE LOW-VALUES             TO  HOLD-ACCT.                   ECS046
01506      MOVE SWA-GA-OR-RPT-CD       TO  HOLD-ACCT.                   ECS046
01507      MOVE 'G'                    TO  HOLD-RECORD-TYPE.            ECS046
01508      MOVE HOLD-COMP-KEY          TO  CO-CONTROL-PRIMARY.          ECS046
01509                                                                   ECS046
01510      READ COMP-FILE.                                              ECS046
01511                                                                   ECS046
01512      IF ERCOMP-FILE-STATUS  =  '00'                               ECS046
01513          NEXT SENTENCE                                            ECS046
01514      ELSE                                                         ECS046
01515          MOVE 'NOT FOUND'        TO  CO-MAIL-NAME  CO-ACCT-NAME.  ECS046
01516                                                                   ECS046
01517      IF DTE-CLIENT = 'MON'                                        ECS046
01518          MOVE CO-MAIL-NAME       TO  HOLD-GA-NAME                 ECS046
01519      ELSE                                                         ECS046
01520          MOVE CO-ACCT-NAME       TO  HOLD-GA-NAME.                ECS046
01521                                                                   ECS046
01522  3500-EXIT.                                                       ECS046
01523      EXIT.                                                        ECS046
01524                                                                   ECS046
01525  3990-PRINT-RPT-A-EXIT.                                           ECS046
01526      CLOSE COMP-FILE  WORK-FILE.                                  ECS046
01527                                                                   ECS046
01528      EJECT                                                        ECS046
01529  4000-PERFORMED-RPT-A-ROUTINES SECTION.                           ECS046
01530                                                                   ECS046
01531  4100-ACCUMULATE.                                                 ECS046
01532      ADD SWA-NET-PREM   (1)      TO BA-NET-PREM    (1, SC).       ECS046
01533      ADD SWA-EARN-PREM  (1)      TO BA-EARN-PREM   (1, SC).       ECS046
01534      ADD SWA-CLMS-INCUR (1)      TO BA-CLMS-INCUR  (1, SC).       ECS046
01535      ADD SWA-EARN-COMP  (1)      TO BA-EARN-COMP   (1, SC).       ECS046
01536      ADD SWA-RETENTION  (1)      TO BA-RETENTION   (1, SC).       ECS046
01537      ADD SWA-PREM-TAX   (1)      TO BA-PREM-TAX    (1, SC).       ECS046
01538      ADD WS-LF-PREM-PROFIT       TO BA-PREM-PROFIT (1, SC)        ECS046
01539      ADD SWA-NET-PREM   (2)      TO BA-NET-PREM    (2, SC).       ECS046
01540      ADD SWA-EARN-PREM  (2)      TO BA-EARN-PREM   (2, SC).       ECS046
01541      ADD SWA-CLMS-INCUR (2)      TO BA-CLMS-INCUR  (2, SC).       ECS046
01542      ADD SWA-EARN-COMP  (2)      TO BA-EARN-COMP   (2, SC).       ECS046
01543      ADD SWA-RETENTION  (2)      TO BA-RETENTION   (2, SC).       ECS046
01544      ADD SWA-PREM-TAX   (2)      TO BA-PREM-TAX    (2, SC).       ECS046
01545      ADD WS-AH-PREM-PROFIT       TO BA-PREM-PROFIT (2, SC).       ECS046
01546                                                                   ECS046
01547  4200-SWA-CONTROL-BREAK.                                          ECS046
01548      MOVE ZERO                   TO LINE-CNT.                     ECS046
01549      PERFORM 4500-PRINT-HEADINGS.                                 ECS046
01550                                                                   ECS046
01551      IF DTE-TOT-OPT = 1                                           ECS046
01552          PERFORM 4210-STATE-BREAK                                 ECS046
01553          PERFORM 4240-GROUP-BREAK                                 ECS046
01554          PERFORM 4250-CARRIER-BREAK.                              ECS046
01555                                                                   ECS046
01556      IF DTE-TOT-OPT = 2                                           ECS046
01557          PERFORM 4210-STATE-BREAK                                 ECS046
01558          PERFORM 4240-GROUP-BREAK                                 ECS046
01559          PERFORM 4250-CARRIER-BREAK                               ECS046
01560          PERFORM 4260-BUS-TYPE-BREAK.                             ECS046
01561                                                                   ECS046
01562      IF DTE-TOT-OPT = 3                                           ECS046
01563          PERFORM 4220-RPT-CODE-BREAK.                             ECS046
01564                                                                   ECS046
01565      IF DTE-TOT-OPT = 4                                           ECS046
01566          PERFORM 4220-RPT-CODE-BREAK                              ECS046
01567          PERFORM 4240-GROUP-BREAK                                 ECS046
01568          PERFORM 4250-CARRIER-BREAK.                              ECS046
01569                                                                   ECS046
01570      IF DTE-TOT-OPT = 5                                           ECS046
01571          PERFORM 4230-GNL-AGT-BREAK                               ECS046
01572          PERFORM 4250-CARRIER-BREAK.                              ECS046
01573                                                                   ECS046
01574      IF DTE-TOT-OPT = 6                                           ECS046
01575          PERFORM 4225-RPT2-CODE-BREAK.                            ECS046
01576                                                                   ECS046
01577      IF SWA-CONTROL = HIGH-VALUES                                 ECS046
01578          MOVE 'F'              TO WS-BREAK-SW                     ECS046
01579          PERFORM 4270-FINAL-TOT-BREAK.                            ECS046
01580                                                                   ECS046
01581      EJECT                                                        ECS046
01582  4210-STATE-BREAK.                                                ECS046
01583      IF SWA-STATE    = SAVE-SWA-STATE AND                         ECS046
01584         SWA-GROUP    = SAVE-SWA-GROUP AND                         ECS046
01585         SWA-CARR     = SAVE-SWA-CARR  AND                         ECS046
01586         SWA-BUS-TYPE = SAVE-SWA-BUS-TYPE                          ECS046
01587          NEXT SENTENCE                                            ECS046
01588      ELSE                                                         ECS046
01589          PERFORM 4510-BUS-TYPE-HDR THRU 4540-STATE-HDR            ECS046
01590          MOVE '0'                TO X                             ECS046
01591          MOVE 'STATE TOTALS'     TO P-DATA                        ECS046
01592          PERFORM 4600-PRINT-ROUTINE                               ECS046
01593          PERFORM 4580-DTL-HDRS                                    ECS046
01594          MOVE +1                 TO SC                            ECS046
01595          PERFORM 4280-PRINT-TOTAL-LINES.                          ECS046
01596                                                                   ECS046
01597  4220-RPT-CODE-BREAK.                                             ECS046
01598      IF SWA-GA-OR-RPT-CD = SAVE-SWA-GA-OR-RPT-CD AND              ECS046
01599         SWA-GROUP        = SAVE-SWA-GROUP        AND              ECS046
01600         SWA-CARR         = SAVE-SWA-CARR                          ECS046
01601          NEXT SENTENCE                                            ECS046
01602      ELSE                                                         ECS046
01603          PERFORM 4520-CARR-HDR THRU 4570-G-A-HDR                  ECS046
01604          MOVE '0'                TO X                             ECS046
01605          MOVE HD-BRK-TYPE        TO P-DATA                        ECS046
01606          PERFORM 4600-PRINT-ROUTINE                               ECS046
01607          PERFORM 4580-DTL-HDRS                                    ECS046
01608          MOVE +2                 TO SC                            ECS046
01609          PERFORM 4280-PRINT-TOTAL-LINES.                          ECS046
01610                                                                   ECS046
01611  4225-RPT2-CODE-BREAK.                                            ECS046
01612      IF SWA-RPT-CD-1 = SAVE-SWA-RPT-CD-1 AND                      ECS046
01613         SWA-RPT-CD-2 = SAVE-SWA-RPT-CD-2                          ECS046
01614          NEXT SENTENCE                                            ECS046
01615      ELSE                                                         ECS046
01616          PERFORM 4575-RPT-CD1-2-HDR                               ECS046
01617          MOVE '0'                TO X                             ECS046
01618          MOVE HD-BRK-TYPE        TO P-DATA                        ECS046
01619          PERFORM 4600-PRINT-ROUTINE                               ECS046
01620          PERFORM 4580-DTL-HDRS                                    ECS046
01621          MOVE +2                 TO SC                            ECS046
01622          PERFORM 4280-PRINT-TOTAL-LINES.                          ECS046
01623                                                                   ECS046
01624  4230-GNL-AGT-BREAK.                                              ECS046
01625      IF SWA-GA-OR-RPT-CD = SAVE-SWA-GA-OR-RPT-CD AND              ECS046
01626         SWA-CARR         = SAVE-SWA-CARR                          ECS046
01627          NEXT SENTENCE                                            ECS046
01628      ELSE                                                         ECS046
01629          PERFORM 4520-CARR-HDR                                    ECS046
01630          PERFORM 4570-G-A-HDR                                     ECS046
01631          MOVE '0'                TO X                             ECS046
01632          MOVE 'GENERAL AGENT TOTALS' TO P-DATA                    ECS046
01633          PERFORM 4600-PRINT-ROUTINE                               ECS046
01634          PERFORM 4580-DTL-HDRS                                    ECS046
01635          MOVE +2                 TO SC                            ECS046
01636          PERFORM 4280-PRINT-TOTAL-LINES.                          ECS046
01637                                                                   ECS046
01638      EJECT                                                        ECS046
01639  4240-GROUP-BREAK.                                                ECS046
01640      IF SWA-GROUP    = SAVE-SWA-GROUP AND                         ECS046
01641         SWA-CARR     = SAVE-SWA-CARR  AND                         ECS046
01642         SWA-BUS-TYPE = SAVE-SWA-BUS-TYPE                          ECS046
01643          NEXT SENTENCE                                            ECS046
01644      ELSE                                                         ECS046
01645          PERFORM 4510-BUS-TYPE-HDR THRU 4530-GROUP-HDR            ECS046
01646          MOVE '0'                TO X                             ECS046
01647          MOVE 'GROUP TOTALS'     TO P-DATA                        ECS046
01648          PERFORM 4600-PRINT-ROUTINE                               ECS046
01649          PERFORM 4580-DTL-HDRS                                    ECS046
01650          MOVE +3                 TO SC                            ECS046
01651          PERFORM 4280-PRINT-TOTAL-LINES.                          ECS046
01652                                                                   ECS046
01653  4250-CARRIER-BREAK.                                              ECS046
01654      IF SWA-CARR     = SAVE-SWA-CARR AND                          ECS046
01655         SWA-BUS-TYPE = SAVE-SWA-BUS-TYPE                          ECS046
01656          NEXT SENTENCE                                            ECS046
01657      ELSE                                                         ECS046
01658          PERFORM 4510-BUS-TYPE-HDR THRU 4520-CARR-HDR             ECS046
01659          MOVE '0'                TO X                             ECS046
01660          MOVE 'CARRIER TOTALS'   TO P-DATA                        ECS046
01661          PERFORM 4600-PRINT-ROUTINE                               ECS046
01662          PERFORM 4580-DTL-HDRS                                    ECS046
01663          MOVE +4                 TO SC                            ECS046
01664          PERFORM 4280-PRINT-TOTAL-LINES.                          ECS046
01665                                                                   ECS046
01666  4260-BUS-TYPE-BREAK.                                             ECS046
01667      IF SWA-BUS-TYPE = SAVE-SWA-BUS-TYPE                          ECS046
01668          NEXT SENTENCE                                            ECS046
01669      ELSE                                                         ECS046
01670          PERFORM 4510-BUS-TYPE-HDR                                ECS046
01671          MOVE '0'                TO X                             ECS046
01672          MOVE 'BUSINESS TYPE TOTALS'   TO P-DATA                  ECS046
01673          PERFORM 4600-PRINT-ROUTINE                               ECS046
01674          PERFORM 4580-DTL-HDRS                                    ECS046
01675          MOVE +5                 TO SC                            ECS046
01676          PERFORM 4280-PRINT-TOTAL-LINES.                          ECS046
01677                                                                   ECS046
01678  4270-FINAL-TOT-BREAK.                                            ECS046
01679      MOVE '0'                    TO X.                            ECS046
01680      MOVE 'FINAL TOTALS'         TO P-DATA.                       ECS046
01681      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01682      PERFORM 4580-DTL-HDRS.                                       ECS046
01683      MOVE +6                     TO SC.                           ECS046
01684      PERFORM 4280-PRINT-TOTAL-LINES.                              ECS046
01685                                                                   ECS046
01686      EJECT                                                        ECS046
01687  4280-PRINT-TOTAL-LINES.                                          ECS046
01688      MOVE LIFE-OVERRIDE-L2       TO DLA-BEN-DESC.                 ECS046
01689      COMPUTE DLA-NET-PREM ROUNDED   = BA-NET-PREM (1, SC).        ECS046
01690      COMPUTE DLA-EARN-PREM ROUNDED  = BA-EARN-PREM (1, SC).       ECS046
01691      COMPUTE DLA-CLMS-INCUR ROUNDED = BA-CLMS-INCUR (1, SC).      ECS046
01692      COMPUTE DLA-EARN-COMP ROUNDED  = BA-EARN-COMP (1, SC).       ECS046
01693      COMPUTE WS-LF-SURPLUS = BA-EARN-PREM (1, SC) -               ECS046
01694                   (BA-CLMS-INCUR (1, SC) + BA-EARN-COMP (1, SC)). ECS046
01695      COMPUTE DLA-SURPLUS ROUNDED    = WS-LF-SURPLUS.              ECS046
01696      COMPUTE DLA-RETENTION ROUNDED  = BA-RETENTION (1, SC).       ECS046
01697      COMPUTE DLA-PREM-PROFIT ROUNDED = BA-PREM-PROFIT (1, SC).    ECS046
01698      COMPUTE DLA-PREM-TAX ROUNDED    = BA-PREM-TAX (1, SC).       ECS046
01699      COMPUTE WS-LF-UW-INCOME =                                    ECS046
01700                 BA-PREM-PROFIT (1, SC) - BA-PREM-TAX (1, SC).     ECS046
01701      COMPUTE DLA-UW-INCOME ROUNDED = WS-LF-UW-INCOME.             ECS046
01702                                                                   ECS046
01703      IF BA-EARN-PREM (1, SC) = ZERO                               ECS046
01704          MOVE ZERO               TO DLA-LOSS-RATIO                ECS046
01705                                     DLA-COMP-RATIO                ECS046
01706                                     DLA-TOT-CHARGES               ECS046
01707      ELSE                                                         ECS046
01708          COMPUTE WS-LOSS-RATIO ROUNDED =                          ECS046
01709           (BA-CLMS-INCUR (1, SC) / BA-EARN-PREM (1, SC)) * +100   ECS046
01710          COMPUTE DLA-LOSS-RATIO ROUNDED = WS-LOSS-RATIO           ECS046
01711          COMPUTE WS-COMP-RATIO ROUNDED =                          ECS046
01712           (BA-EARN-COMP (1, SC) / BA-EARN-PREM (1, SC)) * +100    ECS046
01713          COMPUTE DLA-COMP-RATIO ROUNDED = WS-COMP-RATIO           ECS046
01714          COMPUTE WS-TOT-CHARGES ROUNDED =                         ECS046
01715           (+1 - (WS-LF-UW-INCOME / BA-EARN-PREM (1, SC))) * +100  ECS046
01716          COMPUTE DLA-TOT-CHARGES ROUNDED = WS-TOT-CHARGES.        ECS046
01717                                                                   ECS046
01718      MOVE '0'                    TO X.                            ECS046
01719      MOVE DTL-LINE-A             TO P-DATA.                       ECS046
01720      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01721      MOVE SPACES                 TO DTL-LINE-A   P-DATA.          ECS046
01722                                                                   ECS046
01723      EJECT                                                        ECS046
01724      MOVE   AH-OVERRIDE-L2       TO DLA-BEN-DESC.                 ECS046
01725      COMPUTE DLA-NET-PREM ROUNDED    = BA-NET-PREM   (2, SC).     ECS046
01726      COMPUTE DLA-EARN-PREM ROUNDED   = BA-EARN-PREM  (2, SC).     ECS046
01727      COMPUTE DLA-CLMS-INCUR ROUNDED  = BA-CLMS-INCUR (2, SC).     ECS046
01728      COMPUTE DLA-EARN-COMP ROUNDED   = BA-EARN-COMP  (2, SC).     ECS046
01729      COMPUTE WS-AH-SURPLUS = BA-EARN-PREM  (2, SC) -              ECS046
01730                             (BA-CLMS-INCUR (2, SC) +              ECS046
01731                              BA-EARN-COMP  (2, SC)).              ECS046
01732      COMPUTE DLA-SURPLUS ROUNDED     = WS-AH-SURPLUS.             ECS046
01733      COMPUTE DLA-RETENTION ROUNDED   = BA-RETENTION   (2, SC).    ECS046
01734      COMPUTE DLA-PREM-PROFIT ROUNDED = BA-PREM-PROFIT (2, SC).    ECS046
01735      COMPUTE DLA-PREM-TAX ROUNDED    = BA-PREM-TAX    (2, SC).    ECS046
01736      COMPUTE WS-AH-UW-INCOME = BA-PREM-PROFIT (2, SC) -           ECS046
01737                                BA-PREM-TAX    (2, SC).            ECS046
01738      COMPUTE DLA-UW-INCOME ROUNDED   = WS-AH-UW-INCOME.           ECS046
01739                                                                   ECS046
01740      IF BA-EARN-PREM (2, SC) = ZERO                               ECS046
01741          MOVE ZERO               TO DLA-LOSS-RATIO                ECS046
01742                                     DLA-COMP-RATIO                ECS046
01743                                     DLA-TOT-CHARGES               ECS046
01744      ELSE                                                         ECS046
01745          COMPUTE WS-LOSS-RATIO ROUNDED =                          ECS046
01746           (BA-CLMS-INCUR (2, SC) / BA-EARN-PREM (2, SC)) * +100   ECS046
01747          COMPUTE DLA-LOSS-RATIO ROUNDED = WS-LOSS-RATIO           ECS046
01748          COMPUTE WS-COMP-RATIO ROUNDED =                          ECS046
01749           (BA-EARN-COMP (2, SC) / BA-EARN-PREM (2, SC)) * +100    ECS046
01750          COMPUTE DLA-COMP-RATIO ROUNDED = WS-COMP-RATIO           ECS046
01751          COMPUTE WS-TOT-CHARGES ROUNDED =                         ECS046
01752           (+1 - (WS-AH-UW-INCOME / BA-EARN-PREM (2, SC))) * +100  ECS046
01753          COMPUTE DLA-TOT-CHARGES ROUNDED = WS-TOT-CHARGES.        ECS046
01754                                                                   ECS046
01755      MOVE ' '                    TO X.                            ECS046
01756      MOVE DTL-LINE-A             TO P-DATA.                       ECS046
01757      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01758      MOVE SPACES                 TO DTL-LINE-A   P-DATA.          ECS046
01759                                                                   ECS046
01760      EJECT                                                        ECS046
01761      MOVE 'TOT'                  TO DLA-BEN-DESC.                 ECS046
01762      COMPUTE DLA-NET-PREM ROUNDED =                               ECS046
01763                 BA-NET-PREM (1, SC) + BA-NET-PREM (2, SC).        ECS046
01764      COMPUTE DLA-EARN-PREM ROUNDED =                              ECS046
01765                 BA-EARN-PREM (1, SC) + BA-EARN-PREM (2, SC).      ECS046
01766      COMPUTE DLA-CLMS-INCUR ROUNDED =                             ECS046
01767                 BA-CLMS-INCUR (1, SC) + BA-CLMS-INCUR (2, SC).    ECS046
01768      COMPUTE DLA-EARN-COMP ROUNDED =                              ECS046
01769                 BA-EARN-COMP (1, SC) + BA-EARN-COMP (2, SC).      ECS046
01770      COMPUTE DLA-SURPLUS ROUNDED = WS-AH-SURPLUS + WS-LF-SURPLUS. ECS046
01771      COMPUTE DLA-RETENTION ROUNDED =                              ECS046
01772                 BA-RETENTION (1, SC) + BA-RETENTION (2, SC).      ECS046
01773      COMPUTE DLA-PREM-PROFIT ROUNDED =                            ECS046
01774                 BA-PREM-PROFIT (1, SC) + BA-PREM-PROFIT (2, SC).  ECS046
01775      COMPUTE DLA-PREM-TAX ROUNDED =                               ECS046
01776                 BA-PREM-TAX (1, SC) + BA-PREM-TAX (2, SC).        ECS046
01777      COMPUTE DLA-UW-INCOME ROUNDED =                              ECS046
01778                 WS-LF-UW-INCOME + WS-AH-UW-INCOME.                ECS046
01779                                                                   ECS046
01780      IF BA-EARN-PREM (1, SC) = ZERO     AND                       ECS046
01781         BA-EARN-PREM (2, SC) = ZERO                               ECS046
01782          MOVE ZERO               TO DLA-LOSS-RATIO                ECS046
01783                                     DLA-COMP-RATIO                ECS046
01784                                     DLA-TOT-CHARGES               ECS046
01785      ELSE                                                         ECS046
01786          COMPUTE WS-LOSS-RATIO ROUNDED =                          ECS046
01787           ((BA-CLMS-INCUR (1, SC) + BA-CLMS-INCUR (2, SC)) /      ECS046
01788            (BA-EARN-PREM (1, SC) + BA-EARN-PREM (2, SC))) * +100  ECS046
01789          COMPUTE WS-COMP-RATIO ROUNDED =                          ECS046
01790           ((BA-EARN-COMP (1, SC) + BA-EARN-COMP (2, SC)) /        ECS046
01791            (BA-EARN-PREM (1, SC) + BA-EARN-PREM (2, SC))) * +100  ECS046
01792          COMPUTE WS-TOT-CHARGES ROUNDED =                         ECS046
01793            (+1 - (WS-LF-UW-INCOME + WS-AH-UW-INCOME) /            ECS046
01794            (BA-EARN-PREM (1, SC) + BA-EARN-PREM (2, SC))) * +100  ECS046
01795          COMPUTE DLA-LOSS-RATIO ROUNDED  = WS-LOSS-RATIO          ECS046
01796          COMPUTE DLA-COMP-RATIO ROUNDED  = WS-COMP-RATIO          ECS046
01797          COMPUTE DLA-TOT-CHARGES ROUNDED = WS-TOT-CHARGES.        ECS046
01798                                                                   ECS046
01799      MOVE ' '                    TO X.                            ECS046
01800      MOVE DTL-LINE-A             TO P-DATA.                       ECS046
01801      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01802      MOVE SPACES                 TO DTL-LINE-A   P-DATA.          ECS046
01803                                                                   ECS046
01804      MOVE +1                     TO SB.                           ECS046
01805      PERFORM 4400-ZERO-BREAK-ACCUMULATORS                         ECS046
01806          VARYING SE FROM 1 BY 1 UNTIL SE GREATER SC.              ECS046
01807                                                                   ECS046
01808      MOVE +2                     TO SB.                           ECS046
01809      PERFORM 4400-ZERO-BREAK-ACCUMULATORS                         ECS046
01810          VARYING SE FROM 1 BY 1 UNTIL SE GREATER SC.              ECS046
01811                                                                   ECS046
01812      MOVE ZERO                   TO LINE-CNT.                     ECS046
01813                                                                   ECS046
01814      IF WS-BREAK-SW NOT = 'F'                                     ECS046
01815          PERFORM 4500-PRINT-HEADINGS.                             ECS046
01816                                                                   ECS046
01817      MOVE 'Y'                    TO WS-BREAK-SW.                  ECS046
01818                                                                   ECS046
01819      EJECT                                                        ECS046
01820  4400-ZERO-BREAK-ACCUMULATORS.                                    ECS046
01821      MOVE +0                     TO BA-NET-PREM      (SB, SE)     ECS046
01822                                     BA-EARN-PREM     (SB, SE)     ECS046
01823                                     BA-CLMS-INCUR    (SB, SE)     ECS046
01824                                     BA-EARN-COMP     (SB, SE)     ECS046
01825                                     BA-RETENTION     (SB, SE)     ECS046
01826                                     BA-PREM-PROFIT   (SB, SE)     ECS046
01827                                     BA-PREM-TAX      (SB, SE).    ECS046
01828                                                                   ECS046
01829  4500-PRINT-HEADINGS.                                             ECS046
01830      IF DTE-CLIENT  =  'MON'                                      ECS046
01831          IF WS-ACTIVE-INDIC  =  'ACTIVE'                          ECS046
01832              MOVE HOLD-ACTIVE-DESC                                ECS046
01833                                  TO  HD-STATUS-DESC               ECS046
01834          ELSE                                                     ECS046
01835              MOVE HOLD-ALL-DESC  TO  HD-STATUS-DESC               ECS046
01836      ELSE                                                         ECS046
01837          MOVE SPACES             TO  HD-STATUS-DESC.              ECS046
01838                                                                   ECS046
01839      MOVE '1'                    TO X.                            ECS046
01840      MOVE HEAD-1                 TO P-DATA.                       ECS046
01841      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01842      MOVE ' '                    TO X.                            ECS046
01843      MOVE HEAD-2                 TO P-DATA.                       ECS046
01844      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01845      ADD +1                      TO PAGE-CNT.                     ECS046
01846      MOVE PAGE-CNT               TO HD-PAGE-NO.                   ECS046
01847      MOVE HEAD-3                 TO P-DATA.                       ECS046
01848      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01849                                                                   ECS046
01850  4510-BUS-TYPE-HDR.                                               ECS046
01851      IF DTE-TOT-OPT = 2                                           ECS046
01852          MOVE SAVE-SWA-BUS-TYPE  TO HB-BUS-TYPE                   ECS046
01853          PERFORM 4595-GET-BUS-DESC THRU 4595-EXIT                 ECS046
01854          MOVE HEAD-BUS-TYPE      TO P-DATA                        ECS046
01855          PERFORM 4600-PRINT-ROUTINE                               ECS046
01856          MOVE SPACES             TO P-DATA.                       ECS046
01857                                                                   ECS046
01858  4520-CARR-HDR.                                                   ECS046
01859      IF DTE-TOT-OPT = 1 OR 2 OR 4 OR 5                            ECS046
01860          PERFORM 4590-GET-CARRIER-NAME THRU 4590-EXIT             ECS046
01861          MOVE HEAD-CARR          TO P-DATA                        ECS046
01862          PERFORM 4600-PRINT-ROUTINE                               ECS046
01863          MOVE SPACES             TO P-DATA.                       ECS046
01864                                                                   ECS046
01865  4530-GROUP-HDR.                                                  ECS046
01866      IF DTE-TOT-OPT = 1 OR 2 OR 4                                 ECS046
01867          MOVE SAVE-SWA-GROUP     TO HG-GROUP                      ECS046
01868          MOVE HEAD-GROUP         TO P-DATA                        ECS046
01869          PERFORM 4600-PRINT-ROUTINE                               ECS046
01870          MOVE SPACES             TO P-DATA.                       ECS046
01871                                                                   ECS046
01872      EJECT                                                        ECS046
01873  4540-STATE-HDR.                                                  ECS046
01874      IF DTE-TOT-OPT = 1 OR 2                                      ECS046
01875          MOVE SAVE-SWA-STATE     TO HS-STATE                      ECS046
01876          MOVE HEAD-STATE         TO P-DATA                        ECS046
01877          PERFORM 4600-PRINT-ROUTINE                               ECS046
01878          MOVE SPACES             TO P-DATA.                       ECS046
01879                                                                   ECS046
01880  4550-RPT-CD1-HDR.                                                ECS046
01881      IF DTE-TOT-OPT = 3                                           ECS046
01882          MOVE CLAS-REPORT-CD1-CAPTION TO HR-CAPTION               ECS046
01883          MOVE SAVE-SWA-GA-OR-RPT-CD   TO HR-RPT-CODE              ECS046
01884          MOVE HEAD-RPT-CODE           TO P-DATA                   ECS046
01885          PERFORM 4600-PRINT-ROUTINE                               ECS046
01886          MOVE SPACES                  TO P-DATA.                  ECS046
01887                                                                   ECS046
01888  4560-RPT-CD2-HDR.                                                ECS046
01889      IF DTE-TOT-OPT = 4                                           ECS046
01890          MOVE CLAS-REPORT-CD2-CAPTION TO HR-CAPTION               ECS046
01891          MOVE SAVE-SWA-GA-OR-RPT-CD   TO HR-RPT-CODE              ECS046
01892          MOVE HEAD-RPT-CODE           TO P-DATA                   ECS046
01893          PERFORM 4600-PRINT-ROUTINE                               ECS046
01894          MOVE SPACES                  TO P-DATA.                  ECS046
01895                                                                   ECS046
01896  4570-G-A-HDR.                                                    ECS046
01897      IF DTE-TOT-OPT = 5                                           ECS046
01898          MOVE SAVE-SWA-GA-OR-RPT-CD TO HGA-AGENT                  ECS046
01899          MOVE HOLD-GA-NAME          TO  HGA-GA-NAME               ECS046
01900          MOVE HEAD-G-A              TO P-DATA                     ECS046
01901          PERFORM 4600-PRINT-ROUTINE                               ECS046
01902          MOVE SPACES                TO P-DATA.                    ECS046
01903                                                                   ECS046
01904  4575-RPT-CD1-2-HDR.                                              ECS046
01905      IF DTE-TOT-OPT = 6                                           ECS046
01906          MOVE SAVE-SWA-RPT-CD-1       TO HR2-RPT-CODE1            ECS046
01907          MOVE SAVE-SWA-RPT-CD-2       TO HR2-RPT-CODE2            ECS046
01908          MOVE HEAD-RPT2-CODE          TO P-DATA                   ECS046
01909          PERFORM 4600-PRINT-ROUTINE                               ECS046
01910          MOVE SPACES                  TO P-DATA.                  ECS046
01911                                                                   ECS046
01912  4580-DTL-HDRS.                                                   ECS046
01913      MOVE '0'                    TO X.                            ECS046
01914      MOVE HEAD-4A                TO P-DATA.                       ECS046
01915      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01916      MOVE ' '                    TO X.                            ECS046
01917      MOVE HEAD-5A                TO P-DATA.                       ECS046
01918      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
01919                                                                   ECS046
01920      EJECT                                                        ECS046
01921  4590-GET-CARRIER-NAME.                                           ECS046
01922      IF SAVE-SWA-CARR = HC-CARRIER                                ECS046
01923          GO TO 4590-EXIT.                                         ECS046
01924                                                                   ECS046
01925      MOVE SAVE-SWA-CARR          TO HC-CARRIER.                   ECS046
01926      PERFORM 4591-SRCH VARYING CLAS-INDEXCN FROM 1 BY 1           ECS046
01927          UNTIL CLAS-INDEXCN GREATER CLAS-MAXCN OR                 ECS046
01928                SAVE-SWA-CARR = CARRIER-SUB (CLAS-INDEXCN).        ECS046
01929                                                                   ECS046
01930      IF CLAS-INDEXCN GREATER CLAS-MAXCN                           ECS046
01931          MOVE SPACE                      TO HC-CARRIER-NAME       ECS046
01932      ELSE                                                         ECS046
01933          MOVE CARRIER-PIC (CLAS-INDEXCN) TO HC-CARRIER-NAME.      ECS046
01934                                                                   ECS046
01935  4590-EXIT.                                                       ECS046
01936       EXIT.                                                       ECS046
01937                                                                   ECS046
01938  4591-SRCH.                                                       ECS046
01939       EXIT.                                                       ECS046
01940                                                                   ECS046
01941  4595-GET-BUS-DESC.                                               ECS046
01942      IF SAVE-SWA-BUS-TYPE = HB-BUS-TYPE                           ECS046
01943          GO TO 4595-EXIT.                                         ECS046
01944                                                                   ECS046
01945      MOVE SAVE-SWA-BUS-TYPE      TO HB-BUS-TYPE.                  ECS046
01946      PERFORM 4596-SRCH VARYING CLAS-INDEXB FROM 1 BY 1            ECS046
01947          UNTIL CLAS-INDEXB GREATER CLAS-MAXB OR                   ECS046
01948                SAVE-SWA-BUS-TYPE = CLAS-BUSC-CODE (CLAS-INDEXB).  ECS046
01949                                                                   ECS046
01950      IF CLAS-INDEXB GREATER CLAS-MAXB                             ECS046
01951          MOVE SPACE                        TO HB-BUS-DESC         ECS046
01952      ELSE                                                         ECS046
01953          MOVE CLAS-BUSC-DESC (CLAS-INDEXB) TO HB-BUS-DESC.        ECS046
01954                                                                   ECS046
01955  4595-EXIT.                                                       ECS046
01956       EXIT.                                                       ECS046
01957                                                                   ECS046
01958  4596-SRCH.                                                       ECS046
01959       EXIT.                                                       ECS046
01960                                                                   ECS046
01961      EJECT                                                        ECS046
01962  4600-PRINT-ROUTINE.                                              ECS046
01963                              COPY ELCPRT2.                        ECS046
01964                                                                   ECS046
01965      IF X = '0'                                                   ECS046
01966          ADD +2                  TO LINE-CNT                      ECS046
01967      ELSE                                                         ECS046
01968          ADD +1                  TO LINE-CNT.                     ECS046
01969                                                                   ECS046
01970      MOVE SPACES                 TO P-DATA.                       ECS046
01971                                                                   ECS046
01972      EJECT                                                        ECS046
01973  5000-PRINT-REPORT-B   SECTION.                                   ECS046
01974                                                                   ECS046
01975      IF NO-OF-RECORDS-RELEASED = +0                               ECS046
01976          GO TO 5990-PRINT-RPT-B-EXIT.                             ECS046
01977                                                                   ECS046
01978      MOVE ZERO                   TO PAGE-CNT.                     ECS046
01979                                                                   ECS046
01980      IF DTE-FMT-OPT = 2                                           ECS046
01981          MOVE '- LOSS RATIO SEQUENCE'    TO HD-RPT-TYPE.          ECS046
01982                                                                   ECS046
01983      IF DTE-FMT-OPT = 3                                           ECS046
01984          MOVE '- TOTAL CHARGES SEQUENCE' TO HD-RPT-TYPE.          ECS046
01985                                                                   ECS046
01986      IF DTE-FMT-OPT = 4                                           ECS046
01987          MOVE '- WRITTEN SEQUENCE'       TO HD-RPT-TYPE.          ECS046
01988                                                                   ECS046
01989      IF DTE-FMT-OPT = 5                                           ECS046
01990          MOVE '- PROFIT SEQUENCE'        TO HD-RPT-TYPE.          ECS046
01991                                                                   ECS046
01992      MOVE 'B'                            TO H1-REPORT-SUF.        ECS046
01993                                                                   ECS046
01994      PERFORM 6500-PRINT-HEADINGS.                                 ECS046
01995                                                                   ECS046
01996  5100-RETURN-SORT-WORK-B.                                         ECS046
01997      RETURN SORT-WORK-B  INTO SWB-REC  AT END                     ECS046
01998          GO TO 5990-PRINT-RPT-B-EXIT.                             ECS046
01999                                                                   ECS046
02000      EJECT                                                        ECS046
02001  5200-BUILD-DTL-LINE-B.                                           ECS046
02002      IF LINE-CNT GREATER +54                                      ECS046
02003          MOVE ZERO               TO LINE-CNT                      ECS046
02004          PERFORM 6500-PRINT-HEADINGS.                             ECS046
02005                                                                   ECS046
02006      ADD +1                      TO WS-SEQUENCE.                  ECS046
02007      MOVE WS-SEQUENCE            TO DLB-SEQ-NO.                   ECS046
02008      MOVE SWB-LOSS-RATIO         TO DLB-LOSS-RATIO.               ECS046
02009      COMPUTE DLB-NET-PREM ROUNDED = SWB-NET-PREM.                 ECS046
02010      MOVE SWB-TOT-CHARGES        TO DLB-TOT-CHARGES.              ECS046
02011      COMPUTE DLB-PREM-PROFIT ROUNDED = SWB-PREM-PROFIT.           ECS046
02012                                                                   ECS046
02013      MOVE SWB-ACCT               TO DLB-ACCOUNT.                  ECS046
02014      MOVE SWB-NAME               TO DLB-ACCT-NAME.                ECS046
02015                                                                   ECS046
02016      IF DTE-TOT-OPT = 1                                           ECS046
02017          MOVE SWB-CARR           TO DLB-CARRIER                   ECS046
02018          MOVE SWB-GROUP          TO DLB-GROUP                     ECS046
02019          MOVE SWB-STATE          TO DLB-STATE.                    ECS046
02020                                                                   ECS046
02021      IF DTE-TOT-OPT = 2                                           ECS046
02022          MOVE SWB-BUS-TYPE       TO DLB-BUS-TYPE                  ECS046
02023          MOVE SWB-CARR           TO DLB-CARRIER                   ECS046
02024          MOVE SWB-GROUP          TO DLB-GROUP                     ECS046
02025          MOVE SWB-STATE          TO DLB-STATE.                    ECS046
02026                                                                   ECS046
02027      IF DTE-TOT-OPT = 3                                           ECS046
02028          MOVE SWB-GA-OR-RPT-CD   TO DLB2-GA-OR-RPT-CD.            ECS046
02029                                                                   ECS046
02030      IF DTE-TOT-OPT = 4                                           ECS046
02031          MOVE SWB-CARR           TO DLB2-CARRIER                  ECS046
02032          MOVE SWB-GROUP          TO DLB2-GROUP                    ECS046
02033          MOVE SWB-GA-OR-RPT-CD   TO DLB2-GA-OR-RPT-CD.            ECS046
02034                                                                   ECS046
02035      IF DTE-TOT-OPT = 5                                           ECS046
02036          MOVE SWB-CARR           TO DLB2-CARRIER                  ECS046
02037          MOVE SWB-GA-OR-RPT-CD   TO DLB2-GA-OR-RPT-CD.            ECS046
02038                                                                   ECS046
02039      MOVE ' '                    TO X.                            ECS046
02040      MOVE DTL-LINE-B             TO P-DATA.                       ECS046
02041      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
02042      MOVE SPACES                 TO DTL-LINE-B   P-DATA.          ECS046
02043                                                                   ECS046
02044      GO TO 5100-RETURN-SORT-WORK-B.                               ECS046
02045                                                                   ECS046
02046  5990-PRINT-RPT-B-EXIT.                                           ECS046
02047      CLOSE PRNTR.                                                 ECS046
02048                                                                   ECS046
02049      EJECT                                                        ECS046
02050  6000-PERFORMED-RPT-B-ROUTINES   SECTION.                         ECS046
02051                                                                   ECS046
02052  6500-PRINT-HEADINGS.                                             ECS046
02053      IF DTE-CLIENT  =  'MON'                                      ECS046
02054          IF WS-ACTIVE-INDIC  =  'ACTIVE'                          ECS046
02055              MOVE HOLD-ACTIVE-DESC                                ECS046
02056                                  TO  HD-STATUS-DESC               ECS046
02057          ELSE                                                     ECS046
02058              MOVE HOLD-ALL-DESC  TO  HD-STATUS-DESC               ECS046
02059      ELSE                                                         ECS046
02060          MOVE SPACES             TO  HD-STATUS-DESC.              ECS046
02061                                                                   ECS046
02062      MOVE '1'                    TO X.                            ECS046
02063      MOVE HEAD-1                 TO P-DATA.                       ECS046
02064      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
02065      MOVE ' '                    TO X.                            ECS046
02066      MOVE HEAD-2                 TO P-DATA.                       ECS046
02067      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
02068      ADD +1                      TO PAGE-CNT.                     ECS046
02069      MOVE PAGE-CNT               TO HD-PAGE-NO.                   ECS046
02070      MOVE HEAD-3                 TO P-DATA.                       ECS046
02071      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
02072      MOVE SPACES                 TO H4B-BUS                       ECS046
02073                                     H4B-GENERAL.                  ECS046
02074                                                                   ECS046
02075      IF DTE-TOT-OPT = 1                                           ECS046
02076          MOVE '         CARR'    TO H5B-CTL-HDA                   ECS046
02077          MOVE 'GROUP    STATE'   TO H5B-CTL-HDB.                  ECS046
02078                                                                   ECS046
02079      IF DTE-TOT-OPT = 2                                           ECS046
02080          MOVE 'BUS'              TO H4B-BUS                       ECS046
02081          MOVE '   TYPE  CARR'    TO H5B-CTL-HDA                   ECS046
02082          MOVE 'GROUP    STATE'   TO H5B-CTL-HDB.                  ECS046
02083                                                                   ECS046
02084      IF DTE-TOT-OPT = 3                                           ECS046
02085          MOVE SPACES                  TO H5B-CTL-HDA              ECS046
02086          MOVE CLAS-REPORT-CD1-CAPTION TO H5B-CTL-HDB.             ECS046
02087                                                                   ECS046
02088      IF DTE-TOT-OPT = 4                                           ECS046
02089          MOVE 'CARR  GROUP'           TO H5B-CTL-HDA              ECS046
02090          MOVE CLAS-REPORT-CD2-CAPTION TO H5B-CTL-HDB.             ECS046
02091                                                                   ECS046
02092      IF DTE-TOT-OPT = 5                                           ECS046
02093          MOVE 'GENERAL'          TO H4B-GENERAL                   ECS046
02094          MOVE 'CARR'             TO H5B-CTL-HDA                   ECS046
02095          MOVE '  AGENT'          TO H5B-CTL-HDB.                  ECS046
02096                                                                   ECS046
02097      MOVE '0'                    TO X.                            ECS046
02098      MOVE HEAD-4B                TO P-DATA.                       ECS046
02099      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
02100      MOVE ' '                    TO X.                            ECS046
02101      MOVE HEAD-5B                TO P-DATA.                       ECS046
02102      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
02103      MOVE ' '                    TO X.                            ECS046
02104      MOVE SPACES                 TO P-DATA.                       ECS046
02105      PERFORM 4600-PRINT-ROUTINE.                                  ECS046
02106                                                                   ECS046
02107      EJECT                                                        ECS046
02108  9000-JOB-CLEANUP  SECTION.                                       ECS046
02109                                                                   ECS046
02110  9100-E-O-J.                                                      ECS046
02111      CLOSE PRNTR.                                                 ECS046
02112                                                                   ECS046
02113  9200-CLOSE-FICH.                                                 ECS046
02114                              COPY ELCPRTC.                        ECS046
02115                                                                   ECS046
02116  ABEND-PGM SECTION.                                               ECS046
02117                              COPY ELCABEND.                       ECS046
