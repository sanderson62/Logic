00001  IDENTIFICATION DIVISION.                                         04/09/98
00002                                                                   ECS032
00003  PROGRAM-ID.                ECS032.                                  LV008
00004 *              PROGRAM CONVERTED BY                               ECS032
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS032
00006 *              CONVERSION DATE 11/28/95 11:08:15.                 ECS032
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS032
00008 *                            VMOD=2.018.                          ECS032
00009                                                                   ECS032
00010 *DATE-COMPILED.                                                   ECS032
00011                                                                   ECS032
00012 *SECURITY.   *****************************************************ECS032
00013 *            *                                                   *ECS032
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS032
00015 *            *                                                   *ECS032
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS032
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS032
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS032
00019 *            *                                                   *ECS032
00020 *            *****************************************************ECS032
00021                                                                   ECS032
00022 *REMARKS.                                                         ECS032
00023 *        CLAIMS RESERVE REPORT.                                   ECS032
100703******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
100703******************************************************************
00024                                                                   ECS032
00025  ENVIRONMENT DIVISION.                                            ECS032
00026  CONFIGURATION SECTION.                                           ECS032
00027                                                                      CL**6
00028  INPUT-OUTPUT SECTION.                                            ECS032
00029  FILE-CONTROL.                                                    ECS032
00030                                                                   ECS032
00031      SELECT SORT-FILE    ASSIGN TO SYS001-UT-3380-S-SORTWK1.      ECS032
00032      SELECT PRNTR        ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS032
00033      SELECT EXTRACT      ASSIGN TO SYS018-UT-2400-S-SYS018.       ECS032
00034      SELECT DISK-DATE    ASSIGN TO SYS019-UT-3380-S-SYS019.       ECS032
00035      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS032
00036      SELECT ERMEBL       ASSIGN TO SYS024-UT-3380-ERMEBL          ECS032
00037                          ORGANIZATION INDEXED                     ECS032
00038                          ACCESS DYNAMIC                           ECS032
00039                          RECORD KEY ME-CONTROL-PRIMARY            ECS032
00040                          FILE STATUS ERMEBL-FILE-STATUS.          ECS032
00041  EJECT                                                            ECS032
00042  DATA DIVISION.                                                   ECS032
00043  FILE SECTION.                                                    ECS032
00044                                                                   ECS032
00045  SD  SORT-FILE.                                                   ECS032
00046                                                                   ECS032
00047  01  SORT-RECORD.                                                 ECS032
00048      12  FILLER              PIC  X(4).                           ECS032
00049      12  SORT-KEY3           PIC  X(36).                          ECS032
00050      12  SORT-KEY1           PIC  X(1).                           ECS032
00051      12  SORT-KEY2           PIC  X(10).                          ECS032
00052      12  FILLER              PIC  X(186).                         ECS032
00053      12  SORT-REIN-COMP      PIC  X(6).                           ECS032
00054      12  FILLER              PIC  X(267).                         ECS032
00055                                                                   ECS032
00056  FD  PRNTR                                                        ECS032
00057                              COPY ELCPRTFD.                       ECS032
00058                                                                   ECS032
00059  FD  EXTRACT                                                      ECS032
00060      BLOCK CONTAINS 0 RECORDS
00061      RECORDING MODE IS F.                                         ECS032
00062                                                                   ECS032
00063  01  EXTRACT-RECORD          PIC  X(510).                         ECS032
00064  EJECT                                                            ECS032
00065  FD  DISK-DATE                                                    ECS032
00066                              COPY ELCDTEFD.                       ECS032
00067  EJECT                                                            ECS032
00068  FD  FICH                                                         ECS032
00069                              COPY ELCFCHFD.                       ECS032
00070  EJECT                                                            ECS032
00071  FD  ERMEBL.                                                      ECS032
00072                                                                   ECS032
00073      COPY ERCMEBL.                                                ECS032
00074  EJECT                                                            ECS032
00075  WORKING-STORAGE SECTION.                                         ECS032
00076  77  FILLER  PIC  X(32) VALUE '********************************'. ECS032
00077  77  FILLER  PIC  X(32) VALUE '     ECS032 WORKING STORAGE     '. ECS032
00078  77  FILLER  PIC  X(32) VALUE '********* VMOD=2.018 ***********'. ECS032
00079                                                                   ECS032
00080  77  PGM-SUB                 PIC S9(3)   COMP    VALUE +032.      ECS032
00081  77  SUB1                    PIC  9(4)   COMP    VALUE ZEROS.     ECS032
00082  77  SUB2                    PIC  9(4)   COMP    VALUE ZEROS.     ECS032
00083  77  SUB3                    PIC S9(5)   COMP-3  VALUE ZEROS.        CL**5
00084  77  SUB4                    PIC  9(4)   COMP    VALUE ZEROS.     ECS032
00085  77  X                       PIC  X              VALUE SPACE.     ECS032
00086  77  DIRECT-SW               PIC  X              VALUE SPACE.     ECS032
00087  77  FIRST-READ-SW           PIC  X              VALUE SPACE.     ECS032
00088      88  FIRST-READ             VALUE SPACE.                      ECS032
00089  77  SORT-SW                 PIC  X              VALUE SPACE.     ECS032
00090      88  NO-RECORDS-SORTED      VALUE SPACE.                      ECS032
00091  77  FST                     PIC  9              VALUE ZERO.      ECS032
00092  77  LINE-CNT                PIC  9(3)           VALUE 99.        ECS032
00093  77  PAGE-SIZE               PIC  9(3)           VALUE 56.        ECS032
00094  77  PGE-CNT                 PIC  9(4)           VALUE ZEROS.     ECS032
00095  77  RECD-CNT                PIC  9(6)           VALUE ZEROS.     ECS032
00096  77  W-LINES-USED            PIC  9(3)           VALUE ZEROS.     ECS032
00097  77  PRINT-HOLD              PIC  X(133).                         ECS032
00098                                                                   ECS032
00099  01  MONTH-END-DATA.                                              ECS032
00100      12  ME-START-DATE.                                           ECS032
00101          16  ME-START-MO     PIC  99.                             ECS032
00102          16  FILLER          PIC  X.                              ECS032
00103          16  ME-START-DA     PIC  99.                             ECS032
00104          16  FILLER          PIC  X.                              ECS032
00105          16  ME-START-YR     PIC  99.                             ECS032
00106      12  ME-CNDS-DATE        PIC  9(6).                           ECS032
00107      12  ME-CNDS-DATE-R  REDEFINES  ME-CNDS-DATE.                 ECS032
00108          16  ME-CNDS-MO      PIC  99.                             ECS032
00109          16  ME-CNDS-DA      PIC  99.                             ECS032
00110          16  ME-CNDS-YR      PIC  99.                             ECS032
00111      12  ME-START-TIME       PIC  9(6).                           ECS032
00112      12  ME-UPDATE-FLAG      PIC  X              VALUE 'Y'.       ECS032
00113          88  ME-DO-UPDATE                        VALUE 'Y'.       ECS032
00114          88  ME-NO-UPDATE                        VALUE 'N'.       ECS032
00115      12  ERMEBL-FILE-STATUS  PIC  XX.                             ECS032
00116      12  MONTH-END-MOYR      PIC S9(5)   COMP-3.                  ECS032
070714     12  hld-032-resv-l      pic s9(7)v99 comp-3 value +0.
070714     12  hld-032-resv-ah     pic s9(7)v99 comp-3 value +0.
00117  EJECT                                                            ECS032
00118  01  TOTAL-COUNTERS.                                              ECS032
00119      12  COUNTERS        COMP-3      OCCURS  6  TIMES.            ECS032
00120          16  LIFE-IBNR       PIC S9(9)V99.                        ECS032
00121          16  LIFE-PAYCUR     PIC S9(9)V99.                        ECS032
00122          16  LIFE-FUTURE     PIC S9(9)V99.                        ECS032
00123          16  DISB-IBNR       PIC S9(9)V99.                        ECS032
00124          16  DISB-PAYCUR     PIC S9(9)V99.                        ECS032
00125          16  DISB-FUTURE     PIC S9(9)V99.                        ECS032
00126          16  INCURRED-YEAR-COUNTERS  COMP-3   OCCURS  11  TIMES.  ECS032
00127              20 IBNR-RSRV    PIC S9(9)V99.                        ECS032
00128              20 PAYCUR-RSRV  PIC S9(9)V99.                        ECS032
00129              20 FUTURE-RSRV  PIC S9(9)V99.                        ECS032
00130                                                                   ECS032

070714***  THESE ACCUMS ARE FOR THE UEP IBNR AND ONLY USED
070714***  TO DEDUCT FROM THE ERMEBL TOTALS.
070714 01  W-WORKING-TOTALS.
070714     16  W-TOT-LIFE-IBNR         PIC S9(9)V99 COMP-3 VALUE +0.
070714     16  W-TOT-DISB-IBNR         PIC S9(9)V99 COMP-3 VALUE +0.

00131  01  W-WORKING-TOTALS.                                            ECS032
00132      16  W-ACCT-LIFE-IBNR        PIC S9(9)V99 COMP-3 VALUE +0.    ECS032
00133      16  W-ACCT-DISB-IBNR        PIC S9(9)V99 COMP-3 VALUE +0.    ECS032
00134                                                                   ECS032
00135  01  TOTAL-LEVEL-NAMES.                                           ECS032
00136      12  FILLER              PIC  X(25)          VALUE            ECS032
00137              'ACCOUNT TOTAL            '.                         ECS032
00138      12  FILLER              PIC  X(25)          VALUE            ECS032
00139              'STATE TOTAL              '.                         ECS032
00140      12  FILLER              PIC  X(25)          VALUE            ECS032
00141              'GROUPING TOTAL           '.                         ECS032
00142      12  FILLER              PIC  X(25)          VALUE            ECS032
00143              'CARRIER X TOTAL          '.                         ECS032
00144      12  FILLER              PIC  X(25)          VALUE            ECS032
00145              'OVERALL TOTAL            '.                         ECS032
00146      12  FILLER              PIC  X(25)          VALUE            ECS032
00147              'OVERALL TOTAL            '.                         ECS032
00148                                                                   ECS032
00149  01  TOTAL-DESCRIPTION REDEFINES TOTAL-LEVEL-NAMES.               ECS032
00150      12  TOTAL-ID            PIC  X(25)      OCCURS  6  TIMES.    ECS032
00151  01  FILLER REDEFINES TOTAL-LEVEL-NAMES.                          ECS032
00152      12  FILLER                              OCCURS  6  TIMES.    ECS032
00153          16  FILLER          PIC  X(08).                          ECS032
00154          16  TOTAL-ID-CARRIER                                     ECS032
00155                              PIC  XX.                             ECS032
00156          16  TOTAL-ID-TITLE  PIC  X(15).                          ECS032
00157  EJECT                                                            ECS032
00158  01  HEAD-1.                                                      ECS032
00159      12  FILLER              PIC  X(51)          VALUE SPACES.    ECS032
00160      12  FILLER              PIC  X(21)          VALUE            ECS032
00161              'CLAIM RESERVES REPORT'.                             ECS032
00162      12  FILLER              PIC  X(47)          VALUE SPACES.    ECS032
00163      12  FILLER              PIC  X(6)           VALUE 'ECS032'.  ECS032
00164      12  HD1-SUFFIX          PIC  X              VALUE 'A'.       ECS032
00165                                                                   ECS032
00166  01  HEAD-2.                                                      ECS032
00167      12  FILLER              PIC  X(47)          VALUE SPACES.    ECS032
00168      12  CO-NAME             PIC  X(30).                          ECS032
00169      12  FILLER              PIC  X(42)          VALUE SPACES.    ECS032
00170      12  HD-DATE             PIC  X(8).                           ECS032
00171                                                                   ECS032
00172  01  HEAD-3.                                                      ECS032
00173      12  FILLER              PIC  X(53)          VALUE SPACES.    ECS032
00174      12  HD-ALF-DTE          PIC  X(18).                          ECS032
00175      12  FILLER              PIC  X(48)          VALUE SPACES.    ECS032
00176      12  FILLER              PIC  X(5)           VALUE 'PAGE '.   ECS032
00177      12  HD-PAGE             PIC ZZ,ZZZ.                          ECS032
00178                                                                   ECS032
00179  01  HEAD-4.                                                      ECS032
00180      12  HD4-RPT-REIN-HDG    PIC  X(20)          VALUE            ECS032
00181              'REPORT CODE 1: '.                                   ECS032
00182      12  HD4-RPT-REIN-CD     PIC  X(10)          VALUE SPACES.    ECS032
00183                                                                   ECS032
00184  01  DETL-HEAD1.                                                  ECS032
00185      12  FILLER              PIC  X(44)          VALUE            ECS032
00186              'CAR GROUP ST  ACCOUNT   CERTIFICATE EFF DATE'.      ECS032
00187      12  FILLER              PIC  X(42)          VALUE            ECS032
00188              '    INSURED         CLAIM        RESERVE  '.        ECS032
00189      12  FILLER              PIC  X(34)          VALUE            ECS032
00190              'PAID TO CURR       FUTURE'.                         ECS032
00191      12  DETL-HD1-IBNR-SPACE PIC  X(04)          VALUE 'IBNR'.    ECS032
00192                                                                   ECS032
00193  01  DETL-HEAD2.                                                  ECS032
00194      12  FILLER              PIC  X(44)          VALUE SPACES.    ECS032
00195      12  FILLER              PIC  X(44)          VALUE            ECS032
00196              '                    NUMBER         TYPE     '.      ECS032
00197      12  FILLER              PIC  X(31)          VALUE            ECS032
00198              'RESERVE          RESERVE'.                          ECS032
00199      12  DETL-HD2-IBNR-SPACE PIC  X(07)          VALUE 'RESERVE'. ECS032
00200                                                                   ECS032
00201  EJECT                                                            ECS032
00202  01  DETL-LINE.                                                   ECS032
00203      12  FILLER              PIC  X              VALUE SPACE.     ECS032
00204      12  DETL-CAR            PIC  X.                              ECS032
00205      12  FILLER              PIC  X              VALUE SPACE.     ECS032
00206      12  DETL-CO             PIC  X(6).                           ECS032
00207      12  FILLER              PIC  X              VALUE SPACE.     ECS032
00208      12  DETL-ST             PIC  X(2).                           ECS032
00209      12  FILLER              PIC  X              VALUE SPACE.     ECS032
00210      12  DETL-ACCT           PIC  X(10).                          ECS032
00211      12  FILLER              PIC  X              VALUE SPACE.     ECS032
00212      12  DETL-CERT           PIC  X(11).                          ECS032
00213      12  FILLER              PIC  X              VALUE SPACE.     ECS032
00214      12  DETL-EFF.                                                ECS032
00215          16  DETL-EFF-MO     PIC  99.                             ECS032
00216          16  DETL-DASH-1     PIC  X.                              ECS032
00217          16  DETL-EFF-DAY    PIC  99.                             ECS032
00218          16  DETL-DASH-2     PIC  X.                              ECS032
00219          16  DETL-EFF-YR     PIC  99.                             ECS032
00220      12  FILLER              PIC  XX             VALUE SPACES.    ECS032
00221      12  DETL-AC-FINIT       PIC  X.                              ECS032
00222      12  DETL-AC-MINIT       PIC  X.                              ECS032
00223      12  FILLER              PIC  X              VALUE SPACES.    ECS032
00224      12  DETL-AC-NAM         PIC  X(15).                          ECS032
00225      12  DETL-CLM            PIC  X(14).                          ECS032
00226      12  DETL-TYPE           PIC  X(7).                           ECS032
00227      12  DETL-PDCUR          PIC ZZ,ZZZ,ZZZ.99-.                  ECS032
00228      12  DETL-FUT            PIC ZZ,ZZZ,ZZZ.99-.                  ECS032
00229      12  DETL-IBNR           PIC ZZ,ZZZ,ZZZ.99-.                  ECS032
00230      12  DETL-IBNR-SPACE REDEFINES DETL-IBNR                      ECS032
00231                              PIC  X(14).                          ECS032
00232                                                                   ECS032
00233  01  TOTAL-LINE-P  REDEFINES  DETL-LINE.                          ECS032
00234      12  FILLER              PIC  X(10).                          ECS032
00235      12  TOTAL-NAME          PIC  X(30).                          ECS032
00236      12  TOTAL-DESC          PIC  X(38).                          ECS032
00237      12  FILLER              PIC  X(49).                          ECS032
00238                                                                   ECS032
00239                                                                   ECS032
00240  01  RUN-YEAR.                                                    ECS032
00241      12  FILLER              PIC X                 VALUE ' '.        CL**5
00242      12  PROCESS-CCYR        PIC X(4).                               CL**5
00243                                                                   ECS032
00244  01  CONTROL-HOLD.                                                ECS032
00245      12  RPT-1-REIN-CTL      PIC  X(10)            VALUE SPACES.  ECS032
00246      12  CAR-CTL             PIC  X                VALUE SPACES.  ECS032
00247      12  CO-CTL              PIC  X(6)             VALUE SPACES.  ECS032
00248      12  ST-CTL              PIC  XX               VALUE SPACES.  ECS032
00249      12  ACCT-CTL            PIC  X(10)            VALUE SPACES.  ECS032
00250                                                                   ECS032
00251  01  WS-ABEND.                                                    ECS032
00252      12  WS-RETURN-CODE          PIC  X(4)       VALUE SPACES.    ECS032
00253      12  WS-ABEND-MESSAGE        PIC  X(80)      VALUE SPACES.    ECS032
00254      12  WS-ABEND-FILE-STATUS    PIC  XX         VALUE SPACES.    ECS032
00255      12  WS-ZERO                 PIC S9          VALUE ZERO.      ECS032
00256  EJECT                                                            ECS032
00257      COPY ECSEXT01.                                               ECS032
00258                                                                   ECS032
00259      COPY ELCEXTVR.                                               ECS032
00260                                                                   ECS032
00261      COPY ELCDTECX.                                               ECS032
00262                                                                   ECS032
00263      COPY ELCDTEVR.                                               ECS032
00264                                                                   ECS032
00265  EJECT                                                            ECS032
00266  PROCEDURE DIVISION.                                              ECS032
00267                                                                   ECS032
00275  0000-SETUP-DATES.                                                ECS032
00276                              COPY ELCDTERX.                       ECS032
00277                                                                   ECS032
00278      MOVE WS-TIME                TO  ME-START-TIME.               ECS032
00279      MOVE WS-CURRENT-DATE        TO  ME-START-DATE.               ECS032
00280      MOVE ME-START-MO            TO  ME-CNDS-MO.                  ECS032
00281      MOVE ME-START-DA            TO  ME-CNDS-DA.                  ECS032
00282      MOVE ME-START-YR            TO  ME-CNDS-YR.                  ECS032
00283                                                                   ECS032
00295      MOVE RUN-CCYR               TO  PROCESS-CCYR.                   CL**5
00296                                                                   ECS032
00297      MOVE WS-CURRENT-DATE        TO  HD-DATE.                     ECS032
00298      MOVE COMPANY-NAME           TO  CO-NAME.                     ECS032
00299      MOVE ALPH-DATE              TO  HD-ALF-DTE.                  ECS032
00300                                                                   ECS032
00301      PERFORM 0400-INITIALIZATION-ROUTINE THRU 0499-EXIT.          ECS032
00302                                                                   ECS032
00303  EJECT                                                            ECS032
00304  0100-OPEN-FILES.                                                 ECS032
00305                                                                   ECS032
00306      OPEN INPUT   EXTRACT                                         ECS032
00307           OUTPUT  PRNTR.                                          ECS032
00308                                                                   ECS032
00309  0150-SORT-PROCEDURE.                                             ECS032
00310                                                                   ECS032
00311      SORT SORT-FILE  ASCENDING KEY  SORT-KEY1                     ECS032
00312                                     SORT-KEY2                     ECS032
00313                                     SORT-KEY3                     ECS032
00314          INPUT PROCEDURE 0200-READ-EXTRACT  THRU  0299-EXIT       ECS032
00315          OUTPUT PROCEDURE 0300-READ-REINS  THRU  0399-EXIT.       ECS032
00316                                                                   ECS032
00317      IF (SORT-RETURN  NOT = ZEROS)
100703        AND (NOT NO-RECORDS-SORTED)
00318          MOVE 'SORT RETURN CODE NON ZERO'                         ECS032
00319                                  TO  WS-ABEND-MESSAGE             ECS032
00320          MOVE '0101'             TO  WS-RETURN-CODE               ECS032
00321          PERFORM ABEND-PGM.                                       ECS032
00322                                                                   ECS032
00323      CLOSE PRNTR.                                                 ECS032
00324                                                                   ECS032
00325  0190-CLOSE-FICH.                                                 ECS032
00326                              COPY ELCPRTC.                        ECS032
00327                                                                   ECS032
070714     OPEN I-O ERMEBL.                                             ECS032
070714                                                                  ECS032
070714     IF ERMEBL-FILE-STATUS  = '00' OR '97'                        ECS032
070714         NEXT SENTENCE                                            ECS032
070714       ELSE                                                       ECS032
070714         MOVE 'N'                TO  ME-UPDATE-FLAG.              ECS032
070714     MOVE DTE-CLIENT             TO  ME-COMPANY.                  ECS032
070714                                                                  ECS032
070714     COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.              CL**4
070714                                                                  ECS032
070714     MOVE MONTH-END-MOYR         TO  ME-MOYR.                     ECS032
070714                                                                  ECS032
070714     IF ME-DO-UPDATE                                              ECS032
070714         READ ERMEBL  INVALID KEY                                 ECS032
070714             MOVE 'N'            TO  ME-UPDATE-FLAG               ECS032
070714             CLOSE ERMEBL.                                        ECS032
070714                                                                  ECS032
070714
070714     IF ME-DO-UPDATE                                              ECS032
070714         move hld-032-resv-l     to me-032-resv-l
070714         move hld-032-resv-ah    to me-032-resv-ah
070714         MOVE ME-CNDS-DATE       TO  ME-032-RUN-DT                ECS032
070714         ADD 1                   TO  ME-032-RUN-CT                ECS032
070714         REWRITE MONTH-END-BALANCES                               ECS032
070714         CLOSE ERMEBL.                                            ECS032
00336                                                                   ECS032
00337      GOBACK.                                                      ECS032
00338                                                                   ECS032
00339  EJECT                                                            ECS032
00340  0200-READ-EXTRACT  SECTION.                                      ECS032
00341                                                                   ECS032
00342      READ EXTRACT  INTO  DETAIL-EXTRACT                           ECS032
00343                  AT END PERFORM 0600-TOTAL-LEVEL-1  THRU          ECS032
00344                                 0640-TOTAL-LEVEL-5                ECS032
00345                         CLOSE EXTRACT                             ECS032
00346                         GO TO 0299-EXIT.                          ECS032
00347                                                                   ECS032
00348      IF DE-RECORD-ID  NOT = 'DE'                                  ECS032
00349          GO TO 0200-READ-EXTRACT.                                 ECS032
00350                                                                   ECS032
00351      IF NOT  DE-RESERVE                                           ECS032
00352          GO TO 0200-READ-EXTRACT.                                 ECS032
00353                                                                   ECS032
00354      COPY ELCEXTM1.                                               ECS032
00355                                                                   ECS032
00356      IF DE-RP-CCYY = RUN-CCYY  AND                                   CL**7
00357         DE-RP-MO = RUN-MO                                         ECS032
00358          NEXT SENTENCE                                            ECS032
00359      ELSE                                                         ECS032
00360          GO TO 0200-READ-EXTRACT.                                 ECS032
00361                                                                   ECS032
00362      IF DE-REIN  = 'R'                                            ECS032
pemuni*      IF DTE-PGM-OPT NOT = 1
pemuni       IF DTE-prc-OPT NOT = 1
00364            GO TO 0200-READ-EXTRACT                                ECS032
00365        ELSE                                                       ECS032
00366            MOVE 'R'              TO  DE-TRANS                     ECS032
00367            MOVE DE-REI-COMP      TO  DE-REPORT-CODE-1             ECS032
00368            GO TO 0215-RELEASE-TO-SORT.                            ECS032
00369                                                                   ECS032
00370      IF FIRST-READ                                                ECS032
00371          MOVE '*'                TO  FIRST-READ-SW                ECS032
00372          GO TO 0210-PRINT-DETAIL.                                 ECS032
00373                                                                   ECS032
00374      IF  DE-CARRIER  NOT = CAR-CTL                                ECS032
00375          PERFORM 0600-TOTAL-LEVEL-1  THRU  0630-TOTAL-LEVEL-4     ECS032
00376          MOVE 99                 TO LINE-CNT                      ECS032
00377          GO TO 0210-PRINT-DETAIL.                                 ECS032
00378                                                                   ECS032
00379      IF  DE-GROUPING  NOT = CO-CTL                                ECS032
00380          PERFORM 0600-TOTAL-LEVEL-1  THRU  0620-TOTAL-LEVEL-3     ECS032
00381          GO TO 0210-PRINT-DETAIL.                                 ECS032
00382                                                                   ECS032
00383      IF  DE-STATE  NOT =  ST-CTL                                  ECS032
00384          PERFORM 0600-TOTAL-LEVEL-1  THRU  0610-TOTAL-LEVEL-2     ECS032
00385          GO TO 0210-PRINT-DETAIL.                                 ECS032
00386                                                                   ECS032
00387      IF  DE-ACCOUNT  NOT = ACCT-CTL                               ECS032
00388          PERFORM 0600-TOTAL-LEVEL-1 THRU 0605-CONTINUE-LEVEL-1.   ECS032
00389                                                                   ECS032
00390  0210-PRINT-DETAIL.                                               ECS032
00391                                                                   ECS032
00392      MOVE DE-CARRIER             TO  CAR-CTL.                     ECS032
00393      MOVE DE-GROUPING            TO  CO-CTL.                      ECS032
00394      MOVE DE-STATE               TO  ST-CTL.                      ECS032
00395      MOVE DE-ACCOUNT             TO  ACCT-CTL.                    ECS032
00396                                                                   ECS032
00397      PERFORM 0500-PRINT-DETAIL THRU 0500-EXIT.                    ECS032
00398                                                                   ECS032
00399      IF DTE-CLIENT = 'NCL'                                        ECS032
00400        IF DE-NCL-POOL-CODE = 'SIN'                                ECS032
00401            MOVE DE-ACCOUNT       TO  DE-REPORT-CODE-1             ECS032
00402        ELSE                                                       ECS032
00403            MOVE DE-NCL-POOL-CODE TO  DE-REPORT-CODE-1.            ECS032
00404                                                                   ECS032
00405      IF DTE-TOT-OPT = 2                                           ECS032
00406        IF DE-REPORT-CODE-1 NOT = SPACES                           ECS032
00407            MOVE 'B'              TO  DE-TRANS                     ECS032
00408            GO TO 0215-RELEASE-TO-SORT.                            ECS032
00409                                                                   ECS032
00410      GO TO 0200-READ-EXTRACT.                                     ECS032
00411                                                                   ECS032
00412  0215-RELEASE-TO-SORT.                                            ECS032
00413                                                                   ECS032
00414 *****COPY ELCEXTM2.                                                  CL**4
00415                                                                   ECS032
00416      RELEASE SORT-RECORD FROM DETAIL-EXTRACT.                     ECS032
00417                                                                   ECS032
00418      MOVE '*'                    TO  SORT-SW.                     ECS032
00419                                                                   ECS032
00420      GO TO 0200-READ-EXTRACT.                                     ECS032
00421                                                                   ECS032
00422                                                                   ECS032
00423  0299-EXIT.                                                       ECS032
00424      EXIT.                                                        ECS032
00425  EJECT                                                            ECS032
00426  0300-READ-REINS  SECTION.                                        ECS032
00427                                                                   ECS032
00428      IF NO-RECORDS-SORTED                                         ECS032
00429          GO TO 0399-EXIT.                                         ECS032
00430                                                                   ECS032
00431      PERFORM 0400-INITIALIZATION-ROUTINE THRU 0499-EXIT.          ECS032
00432                                                                   ECS032
00433      PERFORM 0310-RETURN-REINS.                                   ECS032
00434                                                                   ECS032
00435      MOVE DE-TRANS               TO  HD1-SUFFIX.                  ECS032
00436      IF DE-TRANS = 'B'                                            ECS032
00437          MOVE CLAS-REPORT-CD1-CAPTION                             ECS032
00438                                  TO  HD4-RPT-REIN-HDG             ECS032
00439                                      TOTAL-ID (5)                 ECS032
00440          MOVE ' TOTAL'           TO  TOTAL-ID-TITLE (5)           ECS032
00441      ELSE                                                         ECS032
00442          MOVE 'REINSURANCE COMPANY '                              ECS032
00443                                  TO  HD4-RPT-REIN-HDG             ECS032
00444          MOVE 'REINSURANCE COMPANY TOTAL'                         ECS032
00445                                  TO  TOTAL-ID (5).                ECS032
00446                                                                   ECS032
00447      GO TO 0390-PRINT-DETAIL-LINE.                                ECS032
00448                                                                   ECS032
00449  0310-RETURN-REINS.                                               ECS032
00450                                                                   ECS032
00451      RETURN SORT-FILE  INTO  DETAIL-EXTRACT                       ECS032
00452                      AT END PERFORM 0600-TOTAL-LEVEL-1  THRU      ECS032
00453                                     0650-TOTAL-LEVEL-6            ECS032
00454                             GO TO 0399-EXIT.                      ECS032
00455                                                                      CL**8
00456      MOVE DE-EFF                     TO  WS-DE-EFF-N.                CL**8
00457      MOVE DE-RSV-INCUR               TO  WS-DE-RSV-INCUR-N.          CL**8
00458                                                                      CL**8
00459                                                                   ECS032
00460  0330-CHECK-CONTROL.                                              ECS032
00461                                                                   ECS032
00462      IF DE-TRANS  NOT = HD1-SUFFIX                                ECS032
00463          PERFORM 0600-TOTAL-LEVEL-1  THRU  0650-TOTAL-LEVEL-6     ECS032
00464          PERFORM 0400-INITIALIZATION-ROUTINE THRU 0499-EXIT       ECS032
00465          MOVE DE-TRANS           TO  HD1-SUFFIX                   ECS032
00466          MOVE 'REINSURANCE COMPANY '                              ECS032
00467                                  TO  HD4-RPT-REIN-HDG             ECS032
00468          MOVE 'REINSURANCE COMPANY TOTAL'                         ECS032
00469                                  TO  TOTAL-ID (5)                 ECS032
00470          GO TO 0390-PRINT-DETAIL-LINE.                            ECS032
00471                                                                   ECS032
00472      IF DE-REPORT-CODE-1  NOT = RPT-1-REIN-CTL                    ECS032
00473          PERFORM 0600-TOTAL-LEVEL-1  THRU  0640-TOTAL-LEVEL-5     ECS032
00474          MOVE 99                 TO LINE-CNT                      ECS032
00475          GO TO 0390-PRINT-DETAIL-LINE.                            ECS032
00476                                                                   ECS032
00477      IF  DE-CARRIER  NOT = CAR-CTL                                ECS032
00478          PERFORM 0600-TOTAL-LEVEL-1  THRU  0630-TOTAL-LEVEL-4     ECS032
00479          MOVE 99                 TO LINE-CNT                      ECS032
00480          GO TO 0390-PRINT-DETAIL-LINE.                            ECS032
00481                                                                   ECS032
00482      IF  DE-GROUPING  NOT = CO-CTL                                ECS032
00483          PERFORM 0600-TOTAL-LEVEL-1  THRU  0620-TOTAL-LEVEL-3     ECS032
00484          GO TO 0390-PRINT-DETAIL-LINE.                            ECS032
00485                                                                   ECS032
00486      IF  DE-STATE  NOT = ST-CTL                                   ECS032
00487          PERFORM 0600-TOTAL-LEVEL-1  THRU  0610-TOTAL-LEVEL-2     ECS032
00488          GO TO 0390-PRINT-DETAIL-LINE.                            ECS032
00489                                                                   ECS032
00490      IF  DE-ACCOUNT  NOT = ACCT-CTL                               ECS032
00491          PERFORM 0600-TOTAL-LEVEL-1 THRU 0605-CONTINUE-LEVEL-1.   ECS032
00492                                                                   ECS032
00493  0390-PRINT-DETAIL-LINE.                                          ECS032
00494                                                                   ECS032
00495      MOVE DE-REPORT-CODE-1       TO  RPT-1-REIN-CTL.              ECS032
00496      MOVE DE-CARRIER             TO  CAR-CTL.                     ECS032
00497      MOVE DE-GROUPING            TO  CO-CTL.                      ECS032
00498      MOVE DE-STATE               TO  ST-CTL.                      ECS032
00499      MOVE DE-ACCOUNT             TO  ACCT-CTL.                    ECS032
00500                                                                   ECS032
00501      PERFORM 0500-PRINT-DETAIL THRU 0500-EXIT.                    ECS032
00502                                                                   ECS032
00503      GO TO 0310-RETURN-REINS.                                     ECS032
00504                                                                   ECS032
00505  0399-EXIT.                                                       ECS032
00506      EXIT.                                                        ECS032
00507  EJECT                                                            ECS032
00508  PERFORMED-PROCEDURES  SECTION.                                   ECS032
00509                                                                   ECS032
00510  0400-INITIALIZATION-ROUTINE.                                     ECS032
00511                                                                   ECS032
00512      MOVE ZEROS                  TO  PGE-CNT.                     ECS032
00513      MOVE 99                     TO  LINE-CNT.                    ECS032
00514                                                                   ECS032
00515      PERFORM 0450-ZERO-TOTALS THRU 0459-EXIT                      ECS032
00516          VARYING  SUB1  FROM  1  BY  1                            ECS032
00517            UNTIL  SUB1  IS GREATER THAN  6.                       ECS032
00518                                                                   ECS032
00519      MOVE ZEROS                  TO  W-ACCT-LIFE-IBNR             ECS032
00520                                      W-ACCT-DISB-IBNR.            ECS032
00521                                                                   ECS032
00522      GO TO 0499-EXIT.                                             ECS032
00523                                                                   ECS032
00524  0450-ZERO-TOTALS.                                                ECS032
00525                                                                   ECS032
00526      MOVE ZEROS                  TO  LIFE-IBNR (SUB1)             ECS032
00527                                      LIFE-PAYCUR (SUB1)           ECS032
00528                                      LIFE-FUTURE (SUB1)           ECS032
00529                                      DISB-IBNR (SUB1)             ECS032
00530                                      DISB-PAYCUR (SUB1)           ECS032
00531                                      DISB-FUTURE (SUB1).          ECS032
00532                                                                   ECS032
00533      PERFORM 0470-ZERO-YEARS THRU 0479-EXIT                       ECS032
00534          VARYING  SUB3  FROM  1  BY  1                            ECS032
00535            UNTIL  SUB3  IS GREATER THAN  11.                      ECS032
00536                                                                   ECS032
00537  0459-EXIT.                                                       ECS032
00538      EXIT.                                                        ECS032
00539                                                                   ECS032
00540  0470-ZERO-YEARS.                                                 ECS032
00541                                                                   ECS032
00542      MOVE ZEROS                  TO  IBNR-RSRV (SUB1 SUB3)        ECS032
00543                                      PAYCUR-RSRV (SUB1 SUB3)      ECS032
00544                                      FUTURE-RSRV (SUB1 SUB3).     ECS032
00545                                                                   ECS032
00546  0479-EXIT.                                                       ECS032
00547      EXIT.                                                        ECS032
00548                                                                   ECS032
00549  0499-EXIT.                                                       ECS032
00550      EXIT.                                                        ECS032
00551                                                                   ECS032
00552  EJECT                                                            ECS032
00553  0500-PRINT-DETAIL.                                               ECS032
00554                                                                   ECS032
00555      MOVE 1                      TO  SUB1.                        ECS032
00556                                                                   ECS032
00557      MOVE DE-CARRIER             TO  DETL-CAR.                    ECS032
00558      MOVE DE-GROUPING            TO  DETL-CO.                     ECS032
00559      MOVE DE-STATE               TO  DETL-ST.                     ECS032
00560      MOVE DE-ACCOUNT             TO  DETL-ACCT.                   ECS032
00561      MOVE DE-CERT                TO  DETL-CERT.                   ECS032
00562                                                                   ECS032
00563      MOVE DE-EF-YR               TO  DETL-EFF-YR.                 ECS032
00564      MOVE '-'                    TO  DETL-DASH-1.                 ECS032
00565      MOVE DE-EF-MO               TO  DETL-EFF-MO.                 ECS032
00566      MOVE '-'                    TO  DETL-DASH-2.                 ECS032
00567      MOVE DE-EF-DA               TO  DETL-EFF-DAY.                ECS032
00568                                                                   ECS032
00569      MOVE DE-1ST-INIT-FNAME      TO  DETL-AC-FINIT.               ECS032
00570      MOVE DE-INIT                TO  DETL-AC-MINIT.               ECS032
00571      MOVE DE-LNAME               TO  DETL-AC-NAM.                 ECS032
00572      MOVE DE-CLMNO               TO  DETL-CLM.                    ECS032
00573                                                                   ECS032
00574      IF DE-LIFE-RSV                                               ECS032
00575          MOVE LIFE-OVERRIDE-L6   TO  DETL-TYPE                    ECS032
00576      ELSE                                                         ECS032
00577          MOVE AH-OVERRIDE-L6     TO  DETL-TYPE.                   ECS032
00578                                                                   ECS032
00579      IF DE-REIN  = 'R'                                            ECS032
00580          MOVE DE-REI-IBNR        TO  DETL-IBNR                    ECS032
00581          MOVE DE-REI-PAYCUR      TO  DETL-PDCUR                   ECS032
00582          MOVE DE-REI-FUTRSV      TO  DETL-FUT                     ECS032
00583      ELSE                                                         ECS032
00584          MOVE DE-IBNR            TO  DETL-IBNR                    ECS032
00585          MOVE DE-PAYCUR          TO  DETL-PDCUR                   ECS032
00586          MOVE DE-FUTRSV          TO  DETL-FUT.                    ECS032
00587                                                                   ECS032
00588      IF DE-REIN  = 'R'                                            ECS032
00589          IF DE-LIFE-RSV                                           ECS032
00590              ADD DE-REI-IBNR     TO  LIFE-IBNR (SUB1)             ECS032
00591              ADD DE-REI-PAYCUR   TO  LIFE-PAYCUR (SUB1)           ECS032
00592              ADD DE-REI-FUTRSV   TO  LIFE-FUTURE (SUB1)           ECS032
00593          ELSE                                                     ECS032
00594              ADD DE-REI-IBNR     TO  DISB-IBNR (SUB1)             ECS032
00595              ADD DE-REI-PAYCUR   TO  DISB-PAYCUR (SUB1)           ECS032
00596              ADD DE-REI-FUTRSV   TO  DISB-FUTURE (SUB1).          ECS032

070714     if (de-rein not = 'R')
070714        and (de-cert-no = 'ACTBENIBNR')
070714        if de-life-rsv
070714           add de-ibnr           to w-tot-life-ibnr
070714        else
070714           add de-ibnr           to w-tot-disb-ibnr
070714        end-if
070714     end-if

00598      IF DE-REIN  NOT = 'R'                                        ECS032
00599          IF DE-LIFE-RSV                                           ECS032
00600              ADD DE-IBNR         TO  LIFE-IBNR (SUB1)             ECS032
00601              ADD DE-PAYCUR       TO  LIFE-PAYCUR (SUB1)           ECS032
00602              ADD DE-FUTRSV       TO  LIFE-FUTURE (SUB1)           ECS032
00603          ELSE                                                     ECS032
00604              ADD DE-IBNR         TO  DISB-IBNR (SUB1)             ECS032
00605              ADD DE-PAYCUR       TO  DISB-PAYCUR (SUB1)           ECS032
00606              ADD DE-FUTRSV       TO  DISB-FUTURE (SUB1).          ECS032
00607                                                                   ECS032
00608      COMPUTE SUB3 = (RUN-CCYY - DE-RSV-INCUR-CCYY) + 1.              CL**5
00609                                                                      CL**2
00610      IF SUB3 LESS THAN 01                                            CL**5
00611          MOVE 01                 TO  SUB3.                           CL**5
00612                                                                      CL**5
00613      IF SUB3 GREATER THAN 11                                         CL**5
00614          MOVE 11                 TO  SUB3.                        ECS032
00615                                                                   ECS032
00616      IF DE-REIN  = 'R'                                            ECS032
00617          ADD DE-REI-IBNR         TO  IBNR-RSRV (SUB1 SUB3)        ECS032
00618          ADD DE-REI-PAYCUR       TO  PAYCUR-RSRV (SUB1 SUB3)      ECS032
00619          ADD DE-REI-FUTRSV       TO  FUTURE-RSRV (SUB1 SUB3)      ECS032
00620      ELSE                                                         ECS032
00621          ADD DE-IBNR             TO  IBNR-RSRV (SUB1 SUB3)        ECS032
00622          ADD DE-PAYCUR           TO  PAYCUR-RSRV (SUB1 SUB3)      ECS032
00623          ADD DE-FUTRSV           TO  FUTURE-RSRV (SUB1 SUB3).     ECS032
00624                                                                   ECS032
00625      IF DTE-OPT-RESERVE-METHOD-AUTH                               ECS032
00626          IF DE-CERT-NO = 'ACTBENIBNR'                             ECS032
00627              GO TO 0500-ACCUM-ACCT-IBNR.                          ECS032
00628                                                                   ECS032
00629      MOVE DETL-LINE              TO  P-DATA.                      ECS032
00630      ADD 1                       TO  W-LINES-USED.                ECS032
00631      PERFORM 0900-WRITE-LINE  THRU  0999-EXIT.                    ECS032
00632                                                                   ECS032
00633      GO TO 0500-EXIT.                                             ECS032
00634                                                                   ECS032
00635  0500-ACCUM-ACCT-IBNR.                                            ECS032
00636                                                                   ECS032
00637      IF DE-REIN  = 'R'                                            ECS032
00638          IF DE-LIFE-RSV                                           ECS032
00639              ADD DE-REI-IBNR     TO  W-ACCT-LIFE-IBNR             ECS032
00640          ELSE                                                     ECS032
00641              ADD DE-REI-IBNR     TO  W-ACCT-DISB-IBNR             ECS032
00642      ELSE                                                         ECS032
00643          IF DE-LIFE-RSV                                           ECS032
00644              ADD DE-IBNR         TO  W-ACCT-LIFE-IBNR             ECS032
00645          ELSE                                                     ECS032
00646              ADD DE-IBNR         TO  W-ACCT-DISB-IBNR.            ECS032
00647                                                                   ECS032
00648  0500-EXIT.                                                       ECS032
00649      EXIT.                                                        ECS032
00650  EJECT                                                            ECS032
00651  0600-TOTAL-LEVEL-1.                                              ECS032
00652                                                                   ECS032
00653      IF DTE-OPT-RESERVE-METHOD-UNAUTH                             ECS032
00654          GO TO 0605-CONTINUE-LEVEL-1.                             ECS032
00655                                                                   ECS032
00656      MOVE CAR-CTL                TO  DETL-CAR.                    ECS032
00657      MOVE CO-CTL                 TO  DETL-CO.                     ECS032
00658      MOVE ST-CTL                 TO  DETL-ST.                     ECS032
00659      MOVE ACCT-CTL               TO  DETL-ACCT.                   ECS032
00660      MOVE 'ACTBENIBNR'           TO  DETL-CERT.                   ECS032
00661      MOVE RUN-YR                 TO  DETL-EFF-YR.                 ECS032
00662      MOVE '-'                    TO  DETL-DASH-1.                 ECS032
00663      MOVE RUN-MO                 TO  DETL-EFF-MO.                 ECS032
00664      MOVE '-'                    TO  DETL-DASH-1.                 ECS032
00665      MOVE RUN-DA                 TO  DETL-EFF-DAY.                ECS032
00666      MOVE SPACES                 TO  DETL-AC-FINIT                ECS032
00667                                      DETL-AC-MINIT                ECS032
00668                                      DETL-AC-NAM.                 ECS032
00669      MOVE 'IBNRRSV'              TO  DETL-CLM.                    ECS032
00670      MOVE ZEROS                  TO  DETL-PDCUR                   ECS032
00671                                      DETL-FUT.                    ECS032
00672                                                                   ECS032
00673      IF W-ACCT-LIFE-IBNR NOT = ZEROS                              ECS032
00674          MOVE LIFE-OVERRIDE-L6   TO  DETL-TYPE                    ECS032
00675          MOVE W-ACCT-LIFE-IBNR   TO  DETL-IBNR                    ECS032
00676          MOVE DETL-LINE          TO  P-DATA                       ECS032
00677          ADD 1                   TO  W-LINES-USED                 ECS032
00678          PERFORM 0900-WRITE-LINE  THRU  0999-EXIT.                ECS032
00679                                                                   ECS032
00680      IF W-ACCT-DISB-IBNR NOT = ZEROS                              ECS032
00681          MOVE AH-OVERRIDE-L6     TO  DETL-TYPE.                   ECS032
00682          MOVE W-ACCT-DISB-IBNR   TO  DETL-IBNR                    ECS032
00683          MOVE DETL-LINE              TO  P-DATA                   ECS032
00684          ADD 1                       TO  W-LINES-USED             ECS032
00685          PERFORM 0900-WRITE-LINE  THRU  0999-EXIT.                ECS032
00686                                                                   ECS032
00687  0605-CONTINUE-LEVEL-1.                                           ECS032
00688                                                                   ECS032
00689      MOVE 1                      TO  SUB1                         ECS032
00690                                      W-LINES-USED.                ECS032
00691      MOVE 2                      TO  SUB2.                        ECS032
00692                                                                   ECS032
00693      PERFORM 0700-PRINT-TOTAL-LINE THRU 0799-EXIT.                ECS032
00694                                                                   ECS032
00695      PERFORM 0800-ROLL-TOTALS THRU 0899-EXIT.                     ECS032
00696                                                                   ECS032
00697      MOVE ZEROS                  TO  W-ACCT-LIFE-IBNR             ECS032
00698                                      W-ACCT-DISB-IBNR.            ECS032
00699                                                                   ECS032
00700  0610-TOTAL-LEVEL-2.                                              ECS032
00701                                                                   ECS032
00702      ADD 1                       TO  SUB1 SUB2.                   ECS032
00703                                                                   ECS032
00704      PERFORM 0700-PRINT-TOTAL-LINE THRU 0799-EXIT.                ECS032
00705                                                                   ECS032
00706      PERFORM 0800-ROLL-TOTALS THRU 0899-EXIT.                     ECS032
00707                                                                   ECS032
00708  0620-TOTAL-LEVEL-3.                                              ECS032
00709                                                                   ECS032
00710      ADD 1                       TO  SUB1 SUB2.                   ECS032
00711                                                                   ECS032
00712      PERFORM 0700-PRINT-TOTAL-LINE THRU 0799-EXIT.                ECS032
00713                                                                   ECS032
00714      PERFORM 0800-ROLL-TOTALS THRU 0899-EXIT.                     ECS032
00715                                                                   ECS032
00716  0630-TOTAL-LEVEL-4.                                              ECS032
00717                                                                   ECS032
00718      ADD 1                       TO  SUB1 SUB2.                   ECS032
00719      MOVE 99                     TO  LINE-CNT.                    ECS032
00720      MOVE CAR-CTL                TO  TOTAL-ID-CARRIER (SUB1).     ECS032
00721                                                                   ECS032
00722      PERFORM 0700-PRINT-TOTAL-LINE THRU 0799-EXIT.                ECS032
00723                                                                   ECS032
00724      PERFORM 0800-ROLL-TOTALS THRU 0899-EXIT.                     ECS032
00725                                                                   ECS032
00726  0640-TOTAL-LEVEL-5.                                              ECS032
00727                                                                   ECS032
00728      ADD 1                       TO  SUB1 SUB2.                   ECS032
00729      MOVE 99                     TO  LINE-CNT.                    ECS032
00730                                                                   ECS032
00731      PERFORM 0700-PRINT-TOTAL-LINE THRU 0799-EXIT.                ECS032
00732                                                                   ECS032
00733      PERFORM 0800-ROLL-TOTALS THRU 0899-EXIT.                     ECS032
00734                                                                   ECS032
00735  0650-TOTAL-LEVEL-6.                                              ECS032
00736                                                                   ECS032
00737      ADD 1                       TO  SUB1 SUB2.                   ECS032
00738      MOVE 99                     TO  LINE-CNT.                    ECS032
00739      MOVE ' OVERALL'             TO  RPT-1-REIN-CTL.              ECS032
00740                                                                   ECS032
00741      PERFORM 0700-PRINT-TOTAL-LINE THRU 0799-EXIT.                ECS032
00742                                                                   ECS032
00743  0699-TOTAL-EXIT.                                                 ECS032
00744      EXIT.                                                        ECS032
00745                                                                   ECS032
00746  EJECT                                                            ECS032
00747  0700-PRINT-TOTAL-LINE.                                           ECS032
00748                                                                   ECS032
00749      IF SUB1 = 1  OR  2  OR  3                                    ECS032
00750          GO TO 0790-PRINT-TOTALS.                                 ECS032
00751                                                                   ECS032
00752      MOVE SPACES                 TO  TOTAL-LINE-P.                ECS032
00753      MOVE '   RESERVE SUMMARY BY INCURRED YEAR'                   ECS032
00754                                  TO  TOTAL-DESC.                  ECS032
00755      MOVE RUN-YEAR               TO  DETL-TYPE.                   ECS032
00756      MOVE 1                      TO  SUB3.                        ECS032
00757      PERFORM 0780-PRINT-YEARS.                                    ECS032
00758                                                                   ECS032
00759      MOVE SPACES                 TO  TOTAL-DESC.                  ECS032
00760      MOVE ' (-1)  '              TO  DETL-TYPE.                   ECS032
00761      MOVE 2                      TO  SUB3.                        ECS032
00762      PERFORM 0780-PRINT-YEARS.                                    ECS032
00763                                                                   ECS032
00764      MOVE ' (-2)  '              TO  DETL-TYPE.                   ECS032
00765      MOVE 3                      TO  SUB3.                        ECS032
00766      PERFORM 0780-PRINT-YEARS.                                    ECS032
00767                                                                   ECS032
00768      MOVE ' (-3)  '              TO  DETL-TYPE.                   ECS032
00769      MOVE 4                      TO  SUB3.                        ECS032
00770      PERFORM 0780-PRINT-YEARS.                                    ECS032
00771                                                                   ECS032
00772      MOVE ' (-4)  '              TO  DETL-TYPE.                   ECS032
00773      MOVE 5                      TO  SUB3.                        ECS032
00774      PERFORM 0780-PRINT-YEARS.                                    ECS032
00775                                                                   ECS032
00776      MOVE ' (-5)  '              TO  DETL-TYPE.                   ECS032
00777      MOVE 6                      TO  SUB3.                        ECS032
00778      PERFORM 0780-PRINT-YEARS.                                    ECS032
00779                                                                   ECS032
00780      MOVE ' (-6)  '              TO  DETL-TYPE.                   ECS032
00781      MOVE 7                      TO  SUB3.                        ECS032
00782      PERFORM 0780-PRINT-YEARS.                                    ECS032
00783                                                                   ECS032
00784      MOVE ' (-7)  '              TO  DETL-TYPE.                   ECS032
00785      MOVE 8                      TO  SUB3.                        ECS032
00786      PERFORM 0780-PRINT-YEARS.                                    ECS032
00787                                                                   ECS032
00788      MOVE ' (-8)  '              TO  DETL-TYPE.                   ECS032
00789      MOVE 9                      TO  SUB3.                        ECS032
00790      PERFORM 0780-PRINT-YEARS.                                    ECS032
00791                                                                   ECS032
00792      MOVE ' (-9)  '              TO  DETL-TYPE.                   ECS032
00793      MOVE 10                     TO  SUB3.                        ECS032
00794      PERFORM 0780-PRINT-YEARS.                                    ECS032
00795                                                                   ECS032
00796      MOVE ' PRIOR '              TO  DETL-TYPE.                   ECS032
00797      MOVE 11                     TO  SUB3.                        ECS032
00798      PERFORM 0780-PRINT-YEARS.                                    ECS032
00799      MOVE 1                      TO  W-LINES-USED.                ECS032
00800                                                                   ECS032
00801      GO TO 0790-PRINT-TOTALS.                                     ECS032
00802                                                                   ECS032
00803                                                                   ECS032
00804  0780-PRINT-YEARS.                                                ECS032
00805                                                                   ECS032
00806      MOVE IBNR-RSRV (SUB1 SUB3)   TO  DETL-IBNR.                  ECS032
00807      MOVE PAYCUR-RSRV (SUB1 SUB3) TO  DETL-PDCUR.                 ECS032
00808      MOVE FUTURE-RSRV (SUB1 SUB3) TO  DETL-FUT.                   ECS032
00809                                                                   ECS032
00810      MOVE DETL-LINE              TO  P-DATA.                      ECS032
00811      ADD 1                       TO  W-LINES-USED.                ECS032
00812      PERFORM 0900-WRITE-LINE  THRU  0999-EXIT.                    ECS032
00813                                                                   ECS032
00814                                                                   ECS032
00815  0790-PRINT-TOTALS.                                               ECS032
00816                                                                   ECS032
00817      MOVE SPACES                 TO  TOTAL-LINE-P.                ECS032
00818      MOVE TOTAL-ID (SUB1)        TO  TOTAL-NAME.                  ECS032
00819      MOVE LIFE-OVERRIDE-L6       TO  DETL-TYPE.                   ECS032
00820                                                                   ECS032
00821      MOVE LIFE-IBNR (SUB1)       TO  DETL-IBNR.                   ECS032
00822      MOVE LIFE-PAYCUR (SUB1)     TO  DETL-PDCUR.                  ECS032
00823      MOVE LIFE-FUTURE (SUB1)     TO  DETL-FUT.                    ECS032
00824                                                                   ECS032
00825      IF ME-DO-UPDATE
00826         AND (SUB1 = 5)
00828         AND (FST = ZERO)
00830          ADD LIFE-IBNR (5)  LIFE-PAYCUR (5)  LIFE-FUTURE (5)
070714             GIVING  hld-032-RESV-L
070714     end-if

00833      MOVE DETL-LINE              TO  P-DATA.                      ECS032
00834      ADD 1                       TO  W-LINES-USED.                ECS032
00835      PERFORM 0900-WRITE-LINE  THRU  0999-EXIT.                    ECS032
00836                                                                   ECS032
00837      MOVE SPACES                 TO  TOTAL-LINE-P.                ECS032
00838      MOVE AH-OVERRIDE-L6         TO  DETL-TYPE.                   ECS032
00839                                                                   ECS032
00840      MOVE DISB-IBNR (SUB1)       TO  DETL-IBNR.                   ECS032
00841      MOVE DISB-PAYCUR (SUB1)     TO  DETL-PDCUR.                  ECS032
00842      MOVE DISB-FUTURE (SUB1)     TO  DETL-FUT.                    ECS032
00843                                                                   ECS032
00844      IF ME-DO-UPDATE
00845         AND (SUB1 = 5)
00847         AND (FST = ZERO)
00849          ADD DISB-IBNR (5)  DISB-PAYCUR (5)  DISB-FUTURE (5)
070714             GIVING  hld-032-RESV-AH
070714     end-if
00851                                                                   ECS032
00852      MOVE DETL-LINE              TO  P-DATA.                      ECS032
00853      ADD 1                       TO  W-LINES-USED.                ECS032
00854      PERFORM 0900-WRITE-LINE  THRU  0999-EXIT.                    ECS032
00855                                                                   ECS032
00856      ADD 1                       TO  W-LINES-USED.                ECS032
00857      MOVE SPACES                 TO  TOTAL-LINE-P.                ECS032
00858                                                                   ECS032
00859      IF SUB1  = 5                                                 ECS032
00860          MOVE 1                  TO  FST.                         ECS032
00861                                                                   ECS032
00862  0799-EXIT.                                                       ECS032
00863      EXIT.                                                        ECS032
00864                                                                   ECS032
00865  EJECT                                                            ECS032
00866  0800-ROLL-TOTALS.                                                ECS032
00867                                                                   ECS032
00868      ADD LIFE-IBNR (SUB1)        TO  LIFE-IBNR (SUB2).            ECS032
00869      ADD LIFE-PAYCUR (SUB1)      TO  LIFE-PAYCUR (SUB2).          ECS032
00870      ADD LIFE-FUTURE (SUB1)      TO  LIFE-FUTURE (SUB2).          ECS032
00871      ADD DISB-IBNR (SUB1)        TO  DISB-IBNR (SUB2).            ECS032
00872      ADD DISB-PAYCUR (SUB1)      TO  DISB-PAYCUR (SUB2).          ECS032
00873      ADD DISB-FUTURE (SUB1)      TO  DISB-FUTURE (SUB2).          ECS032
00874                                                                   ECS032
00875      PERFORM 0880-ROLL-YEARS THRU 0889-EXIT                       ECS032
00876          VARYING  SUB3  FROM  1  BY  1                            ECS032
00877            UNTIL  SUB3  IS GREATER THAN  11.                      ECS032
00878                                                                   ECS032
00879      PERFORM 0450-ZERO-TOTALS THRU 0459-EXIT.                     ECS032
00880                                                                   ECS032
00881      GO TO 0899-EXIT.                                             ECS032
00882                                                                   ECS032
00883  0880-ROLL-YEARS.                                                 ECS032
00884                                                                   ECS032
00885      ADD IBNR-RSRV (SUB1 SUB3)   TO  IBNR-RSRV (SUB2 SUB3).       ECS032
00886      ADD PAYCUR-RSRV (SUB1 SUB3) TO  PAYCUR-RSRV (SUB2 SUB3).     ECS032
00887      ADD FUTURE-RSRV (SUB1 SUB3) TO  FUTURE-RSRV (SUB2 SUB3).     ECS032
00888                                                                   ECS032
00889  0889-EXIT.                                                       ECS032
00890      EXIT.                                                        ECS032
00891                                                                   ECS032
00892  0899-EXIT.                                                       ECS032
00893      EXIT.                                                        ECS032
00894                                                                   ECS032
00895                                                                      CL**3
00896  0900-WRITE-LINE.                                                 ECS032
00897                                                                   ECS032
00898      ADD W-LINES-USED            TO  LINE-CNT.                    ECS032
00899                                                                   ECS032
00900      IF  LINE-CNT GREATER THAN PAGE-SIZE                          ECS032
00901          MOVE P-DATA             TO  PRINT-HOLD                   ECS032
00902          PERFORM 1000-PRINT-HEADINGS THRU 1099-EXIT               ECS032
00903          MOVE PRINT-HOLD         TO  P-DATA.                      ECS032
00904                                                                   ECS032
00905      IF  W-LINES-USED = 1                                         ECS032
00906          MOVE SPACE              TO  X                            ECS032
00907      ELSE                                                         ECS032
00908          IF  W-LINES-USED = 2                                     ECS032
00909              MOVE '0'            TO  X                            ECS032
00910          ELSE                                                     ECS032
00911              MOVE '-'            TO  X.                           ECS032
00912                                                                   ECS032
00913      PERFORM 1100-PRINT-LINE THRU 1100-EXIT.                      ECS032
00914                                                                   ECS032
00915      MOVE 0                      TO  W-LINES-USED.                ECS032
00916                                                                   ECS032
00917  0999-EXIT.                                                       ECS032
00918      EXIT.                                                        ECS032
00919  EJECT                                                            ECS032
00920  1000-PRINT-HEADINGS.                                             ECS032
00921                                                                   ECS032
00922      MOVE '1'                    TO  X.                           ECS032
00923      ADD 1                       TO  PGE-CNT.                     ECS032
00924      MOVE PGE-CNT                TO  HD-PAGE.                     ECS032
00925      MOVE HEAD-1                 TO  P-DATA.                      ECS032
00926      PERFORM 1100-PRINT-LINE  THRU  1100-EXIT.                    ECS032
00927                                                                   ECS032
00928      MOVE HEAD-2                 TO  P-DATA.                      ECS032
00929      MOVE SPACE                  TO  X.                           ECS032
00930      PERFORM 1100-PRINT-LINE  THRU  1100-EXIT.                    ECS032
00931                                                                   ECS032
00932      MOVE HEAD-3                 TO  P-DATA.                      ECS032
00933      PERFORM 1100-PRINT-LINE  THRU  1100-EXIT.                    ECS032
00934                                                                   ECS032
00935      MOVE ZEROS                  TO  X.                           ECS032
00936                                                                   ECS032
00937      IF  HD1-SUFFIX = 'B'  OR  'R'                                ECS032
00938          MOVE RPT-1-REIN-CTL     TO  HD4-RPT-REIN-CD              ECS032
00939          MOVE HEAD-4             TO  P-DATA                       ECS032
00940          PERFORM 1100-PRINT-LINE  THRU  1100-EXIT.                ECS032
00941                                                                   ECS032
00942      MOVE DETL-HEAD1             TO  P-DATA.                      ECS032
00943      PERFORM 1100-PRINT-LINE  THRU  1100-EXIT.                    ECS032
00944                                                                   ECS032
00945      MOVE SPACE                  TO  X.                           ECS032
00946      MOVE DETL-HEAD2             TO  P-DATA.                      ECS032
00947      PERFORM 1100-PRINT-LINE  THRU  1100-EXIT.                    ECS032
00948                                                                   ECS032
00949      IF  HD1-SUFFIX = 'B'  OR  'R'                                ECS032
00950          MOVE 8                  TO  LINE-CNT                     ECS032
00951      ELSE                                                         ECS032
00952          MOVE 6                  TO  LINE-CNT.                    ECS032
00953                                                                   ECS032
00954      MOVE 2                      TO  W-LINES-USED.                ECS032
00955                                                                   ECS032
00956  1099-EXIT.                                                       ECS032
00957      EXIT.                                                        ECS032
00958                                                                   ECS032
00959  EJECT                                                            ECS032
00960  1100-PRINT-LINE.                                                 ECS032
00961      COPY ELCPRT2.                                                ECS032
00962                                                                   ECS032
00963  1100-EXIT.                                                       ECS032
00964      EXIT.                                                        ECS032
00965                                                                   ECS032
00966  ABEND-PGM SECTION.                                               ECS032
00967                            COPY ELCABEND.                         ECS032
00968                                                                      CL**6
