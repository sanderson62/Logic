00001  IDENTIFICATION DIVISION.                                         03/05/98
00002                                                                   ECS020
00003  PROGRAM-ID.                 ECS020.                                 LV029
00004 *              PROGRAM CONVERTED BY                               ECS020
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS020
00006 *              CONVERSION DATE 11/28/95 10:59:57.                 ECS020
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS020
00008 *                            VMOD=2.038.                          ECS020
00009                                                                   ECS020
00010 *AUTHOR.        LOGIC, INC.                                       ECS020
00011 *               DALLAS, TEXAS.                                    ECS020
00012                                                                   ECS020
00013 *DATE-COMPILED.                                                   ECS020
00014                                                                   ECS020
00015 *SECURITY.   *****************************************************ECS020
00016 *            *                                                   *ECS020
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS020
00018 *            *                                                   *ECS020
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS020
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS020
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS020
00022 *            *                                                   *ECS020
00023 *            *****************************************************ECS020
00024                                                                   ECS020
00025 *REMARKS.                                                         ECS020
00026 *        THIS PROGRAM WILL PRINT THE EARNED PREMIUM               ECS020
00027 *        AND LOSS REPORT, AND CREATE A 'LOSS RATIO'               ECS020
00028 *        EXTRACT FILE TO BE LOADED ON-LINE.                       ECS020
00029                                                                   ECS020
00030  ENVIRONMENT DIVISION.                                            ECS020
00031  CONFIGURATION SECTION.                                           ECS020
00032  SPECIAL-NAMES.                                                   ECS020
00033      C02 IS LCP-CH2                                               ECS020
00034      C03 IS LCP-CH3                                               ECS020
00035      C04 IS LCP-CH4                                               ECS020
00036      C05 IS LCP-CH5                                               ECS020
00037      C06 IS LCP-CH6                                               ECS020
00038      C07 IS LCP-CH7                                               ECS020
00039      C08 IS LCP-CH8                                               ECS020
00040      C09 IS LCP-CH9                                               ECS020
00041      C10 IS LCP-CH10                                              ECS020
00042      C11 IS LCP-CH11                                              ECS020
00043      C12 IS LCP-CH12                                              ECS020
00044      S01 IS LCP-P01                                               ECS020
00045      S02 IS LCP-P02.                                              ECS020
00046  INPUT-OUTPUT SECTION.                                            ECS020
00047  FILE-CONTROL.                                                    ECS020
00048                                                                   ECS020
00049      SELECT SORT-WORK        ASSIGN TO SYS001-UT-3380-S-SORTWK1.  ECS020
00050      SELECT PRINTER          ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS020
00051      SELECT EXTRACTS         ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS020
00052      SELECT LOSS-RATIOS      ASSIGN TO SYS013-UT-2400-S-SYS013.   ECS020
pemuni     SELECT RTBL-FILE        ASSIGN TO ERRTBLT                    ECS020
00054                              ACCESS IS SEQUENTIAL                 ECS020
00055                              ORGANIZATION IS INDEXED              ECS020
00056                              FILE STATUS IS REIN-FILE-STATUS      ECS020
00057                              RECORD KEY IS RE-CONTROL-PRIMARY.    ECS020
pemuni     SELECT ACCT-MASTER      ASSIGN TO ERACCTT                    ECS020
00059                              ACCESS IS DYNAMIC                    ECS020
00060                              ORGANIZATION IS INDEXED              ECS020
00061                              FILE STATUS IS AM-FILE-STATUS        ECS020
00062                              RECORD KEY IS AM-CONTROL-PRIMARY.    ECS020
00063                                                                   ECS020
00064      SELECT ELCNTL           ASSIGN TO SYS009-FBA1-ELCNTL         ECS020
00065                              ACCESS       DYNAMIC                 ECS020
00066                              ORGANIZATION INDEXED                 ECS020
00067                              FILE STATUS  ELCNTL-FILE-STATUS      ECS020
00068                              RECORD KEY   CF-CONTROL-PRIMARY.     ECS020
00069                                                                   ECS020
00070      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   ECS020
00071      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS020
00072  EJECT                                                            ECS020
00073  DATA DIVISION.                                                   ECS020
00074  FILE SECTION.                                                    ECS020
00075                                                                   ECS020
00076  SD  SORT-WORK.                                                   ECS020
00077                                                                   ECS020
00078  01  SORT-RECORD.                                                 ECS020
00079      12  SW-CONTROL.                                              ECS020
00080          16  SW-RPT-CD-1         PIC X(10).                       ECS020
00081          16  SW-CARRIER          PIC X.                           ECS020
00082          16  SW-GROUPING         PIC X(6).                        ECS020
00083          16  SW-RPT-CD-2         PIC X(10).                       ECS020
00084          16  SW-STATE            PIC XX.                          ECS020
00085          16  SW-ACCOUNT          PIC X(10).                       ECS020
00086          16  SW-EXP-DATE         PIC 9(11)  COMP-3.                  CL*11
00087          16  SW-EFF-DATE         PIC 9(11)  COMP-3.                  CL*11
00088      12  SW-EP-RECORD.                                            ECS020
00089          16  FILLER              PIC XXX.                         ECS020
00090          16  SW-REIN             PIC X.                           ECS020
00091          16  FILLER              PIC X(31).                       ECS020
00092          16  SW-REI-CO.                                           ECS020
00093              20  SW-REINCO       PIC XXX.                         ECS020
00094              20  SW-REINCO-SUB   PIC XXX.                         ECS020
00095          16  SW-RCD-TYPE         PIC X.                           ECS020
00096          16  SW-BEN-CODE         PIC XX.                          ECS020
00097          16  FILLER              PIC X(274).                      ECS020
00098          16  SW-PURGE            PIC X.                           ECS020
00099          16  SW-RUN-DTE          PIC 9(11)  COMP-3.                  CL*11
00100      12  SW-ACCT-FLAG            PIC X.                           ECS020
JJPMOD     12  SW-REIN-NAME            PIC X(30).
00101                                                                   ECS020
00102  FD  PRINTER                                                      ECS020
00103                                 COPY ELCPRTFD.                    ECS020
00104  EJECT                                                            ECS020
00105  FD  EXTRACTS                                                     ECS020
00106      BLOCK CONTAINS 0 RECORDS
00107      RECORDING MODE F.                                            ECS020
00108                                                                   ECS020
00109  01  EPR-RECORD.                                                  ECS020
00110      12  EPR-REC-ID                PIC XX.                        ECS020
00111      12  EPR-COMPANY-CD            PIC X.                         ECS020
00112      12  EPR-REIN                  PIC X.                         ECS020
00113      12  EPR-CONTROL.                                             ECS020
00114          16  EPR-CARRIER           PIC X.                         ECS020
00115          16  EPR-GROUPING.                                        ECS020
00116              20  EPR-GROUP-PREFIX  PIC XXX.                       ECS020
00117              20  EPR-GROUP-PRIME   PIC XXX.                       ECS020
00118          16  EPR-STATE             PIC XX.                        ECS020
00119          16  EPR-ACCOUNT.                                         ECS020
00120              20  EPR-ACCT-PREFIX   PIC X(4).                      ECS020
00121              20  EPR-ACCT-PRIME    PIC X(6).                      ECS020
00122          16  EPR-DATES.                                           ECS020
00123              20  EPR-EXP-DTE       PIC 9(11)  COMP-3.                CL**2
00124              20  EPR-EFF-DTE       PIC 9(11)  COMP-3.                CL**2
00125      12  EPR-REI-CO                PIC X(6).                      ECS020
00126      12  EPR-RCD-TYPE              PIC X.                         ECS020
00127      12  EPR-BEN-CODE              PIC XX.                        ECS020
00128      12  EPR-SPEC-REIN             PIC X.                         ECS020
00129      12  FILLER                    PIC X(273).                    ECS020
00130      12  EPR-PURGE                 PIC X.                         ECS020
00131      12  EPR-RUN-DATE              PIC 9(11)  COMP-3.                CL**7
00132                                                                   ECS020
00133  FD  LOSS-RATIOS                                                  ECS020
00134      BLOCK CONTAINS 0 RECORDS
00135      RECORDING MODE F.                                            ECS020
00136                                                                   ECS020
00137  01  LOSS-RATIO-EXTRACT            PIC X(525).                    ECS020
00138                                                                   ECS020
00139  EJECT                                                            ECS020
00140  FD  RTBL-FILE.                                                   ECS020
00141                                                                   ECS020
00142                                    COPY ERCREIN.                  ECS020
00143  EJECT                                                            ECS020
00144  FD  ACCT-MASTER.                                                 ECS020
00145                                                                   ECS020
00146                                    COPY ERCACCT.                  ECS020
00147                                                                   ECS020
00148  EJECT                                                            ECS020
00149  FD  ELCNTL.                                                      ECS020
00150                                                                   ECS020
00151                                    COPY ELCCNTL.                  ECS020
00152                                                                   ECS020
00153  FD  DISK-DATE                                                    ECS020
00154                                    COPY ELCDTEFD.                 ECS020
00155  EJECT                                                            ECS020
00156  FD  FICH                                                         ECS020
00157                                    COPY ELCFCHFD.                 ECS020
00158  EJECT                                                            ECS020
00159  WORKING-STORAGE SECTION.                                         ECS020
00160  77  LCP-ASA                       PIC X.                         ECS020
00161  77  FILLER  PIC X(32) VALUE '********************************'.  ECS020
00162  77  FILLER  PIC X(32) VALUE '     ECS020 WORKING STORAGE     '.  ECS020
00163  77  FILLER  PIC X(32) VALUE '******** VMOD=2.038 ************'.  ECS020
00164                                                                   ECS020
00165  77  W-ZEROS                 PIC S9(4) COMP VALUE +0.             ECS020
00166  77  VALID-RTBL              PIC 9     VALUE 0.                   ECS020
00167  77  VALID-EXT               PIC 9     VALUE 0.                   ECS020
00168  77  EXT-EOF                 PIC 9     VALUE 0.                   ECS020
00169  77  FIRST-SW                PIC 9     VALUE 0.                   ECS020
00170  77  SECND-PASS-SW           PIC 9     VALUE 0.                   ECS020
00171  77  L1                      PIC X     VALUE SPACES.              ECS020
00172  77  EP-COMP                 PIC X(7)  VALUE SPACES.              ECS020
00173  77  EP-KEY                  PIC X(26) VALUE SPACES.              ECS020
CIDMOD                                                                  ECS020
CIDMOD 01  SPACE-LINE              PIC X(132)     VALUE SPACES.         ECS020
00174                                                                   ECS020
00175  01  AM-FILE-STATUS          PIC XX.                              ECS020
00176                                                                   ECS020
00177  01  ELCNTL-FILE-STATUS.                                          ECS020
00178      12  CNTL-FILE-STATUS-1  PIC X.                               ECS020
00179      12  CNTL-FILE-STATUS-2  PIC X.                               ECS020
00180                                                                   ECS020
00181  01  REIN-FILE-STATUS.                                            ECS020
00182      12  REIN-FILE-STATUS-1  PIC X.                               ECS020
00183      12  REIN-FILE-STATUS-2  PIC X.                               ECS020
00184                                                                   ECS020
00185  01  WORK-ABEND-CODE.                                             ECS020
00186      12  WAC-1               PIC X.                               ECS020
00187      12  WAC-2               PIC X.                               ECS020
00188      12  WAC-3-4.                                                 ECS020
00189          16  WAC-3           PIC X.                               ECS020
00190          16  WAC-4           PIC X.                               ECS020
00191                                                                   ECS020
00192  01  MISC.                                                        ECS020
unix       12  WS-SAVE-SORT-REC    PIC X(407) VALUE SPACES.
00193      12  WS-CUSTOMIZATION-FLAG PIC X   VALUE SPACE.               ECS020
00194          88  CUSTOMIZATION-FOUND       VALUE SPACE.               ECS020
00195      12  X1                  PIC S999    COMP.                    ECS020
00196      12  X2                  PIC S999    COMP.                    ECS020
00197      12  X3                  PIC S999    COMP.                    ECS020
00198      12  X4                  PIC S999    COMP.                    ECS020
00199      12  X5                  PIC S999    COMP.                    ECS020
00200      12  X                   PIC X.                               ECS020
00201      12  ZERO-FLAG           PIC X               VALUE SPACE.     ECS020
00202          88  ALL-AMOUNTS-ZERO   VALUE ' '.                        ECS020
00203      12  W-TOTAL-SW          PIC X(01)           VALUE SPACE.     ECS020
00204          88  W-TOTALS-BEING-PROCESSED VALUE 'Y'.                  ECS020
00205      12  WS-RETURN-CODE      PIC X(4)            VALUE ZEROS.     ECS020
00206      12  ABEND-OPTION        PIC X               VALUE 'Y'.       ECS020
00207      12  WS-ABEND-MESSAGE    PIC X(80)           VALUE SPACES.    ECS020
00208      12  WS-ABEND-FILE-STATUS PIC XX             VALUE SPACES.    ECS020
00209      12  WS-ZERO             PIC S9              VALUE ZERO.      ECS020
00210      12  PAGER               PIC S9(5)   COMP-3  VALUE ZERO.      ECS020
00211      12  LINER               PIC S999    COMP-3  VALUE +99.       ECS020
00212      12  PGM-SUB             PIC S999    COMP-3  VALUE +020.      ECS020
00213      12  DATE-PRT-CTR        PIC S9(5)   COMP-3  VALUE ZERO.      ECS020
00214      12  WS-Y-LOSS           PIC S99V999 COMP-3.                  ECS020
00215      12  WS-I-LOSS           PIC S99V999 COMP-3.                  ECS020
00216      12  WS-FAC              PIC S999V9(5)       COMP-3.          ECS020
00217      12  WS-PRO              PIC S9V9999         COMP-3.          ECS020
00218      12  WS-R78              PIC S9V9999         COMP-3.          ECS020
00219      12  LF-PRO              PIC S9V9999         COMP-3.          ECS020
00220      12  LF-R78              PIC S9V9999         COMP-3.          ECS020
00221      12  AH-PRO              PIC S9V9999         COMP-3.          ECS020
00222      12  AH-R78              PIC S9V9999         COMP-3.          ECS020
00223      12  TT-PRM-78           PIC S9(9)V99        COMP-3.          ECS020
00224      12  TT-PRM-PR           PIC S9(9)V99        COMP-3.          ECS020
00225      12  TT-PRM-ST           PIC S9(9)V99        COMP-3.          ECS020
00226      12  LOSS-RATIO-SW       PIC X               VALUE SPACE.     ECS020
00227          88  LOSS-RATIO-TOO-LARGE      VALUE 'Y'.                 ECS020
00228      12  BENEFIT-PRT-SW      PIC X               VALUE SPACE.     ECS020
00229          88  PRINTING-1ST-BENEFIT      VALUE 'X'.                 ECS020
00230      12  G-A-PRT-SW          PIC X               VALUE SPACE.     ECS020
00231          88  PRINTING-G-A-HDR          VALUE SPACE.               ECS020
00232      12  REIN-PRT-SW         PIC X               VALUE SPACE.     ECS020
00233          88  PRINTING-REIN-TOTALS      VALUE 'X'.                 ECS020
00234      12  WS-PRT-FLAGS.                                            ECS020
00235          16  WS-REIN-PRT-FLG PIC X.                               ECS020
00236          16  WS-GROS-PRT-FLG PIC X.                               ECS020
00237      12  WS-RUN-DTE.                                              ECS020
00238          16  WS-RUN-CC       PIC 99.                                 CL**3
00239          16  WS-RUN-YR       PIC 99.                                 CL**3
00240          16  WS-RUN-MO       PIC 99.                              ECS020
00241          16  WS-RUN-DA       PIC 99.                              ECS020
00242      12  WS-RUN-DTE-N REDEFINES                                      CL**6
00243             WS-RUN-DTE       PIC 9(8).                               CL**6
00244      12  WS-DATE.                                                 ECS020
00245          16  WS-MO           PIC 99.                              ECS020
00246          16  FILLER          PIC X               VALUE '-'.       ECS020
00247          16  WS-DA           PIC 99.                              ECS020
00248          16  FILLER          PIC X               VALUE '-'.       ECS020
00249          16  WS-YR           PIC 99.                              ECS020
00250      12  WS-EPR-RUN-DATE     PIC 9(11).                              CL*15
00251      12  WS-EPR-RUN-DATE-N REDEFINES WS-EPR-RUN-DATE.                CL*15
00252          16  FILLER                PIC 999.                          CL**4
00253          16  EPR-RUN-YR-MO.                                          CL**3
00254              20  EP-RUN-CC         PIC 99.                           CL*20
00255              20  EPR-RUN-YR        PIC 99.                           CL**4
00256              20  FILLER            PIC 99.                           CL**4
00257          16  FILLER                PIC 99.                           CL**4
00258                                                                   ECS020
00259      12  WS-REIN-COMP-TBL.                                        ECS020
00260          16  WS-REIN-COMP-TBL-X OCCURS 1500 TIMES.                ECS020
00261              20  WS-RCT-COMP PIC X(6).                            ECS020
00262              20  WS-RCT-NAME PIC X(30).                           ECS020
00263              20  WS-RCT-CEDE PIC X(30).                           ECS020
00264              20  WS-LF-IBNR  PIC SV999   COMP-3.                  ECS020
00265              20  WS-AH-IBNR  PIC SV999   COMP-3.                  ECS020
00266              20  WS-LF-PRO   PIC S9V9999 COMP-3.                  ECS020
00267              20  WS-LF-R78   PIC S9V9999 COMP-3.                  ECS020
00268              20  WS-AH-PRO   PIC S9V9999 COMP-3.                  ECS020
00269              20  WS-AH-R78   PIC S9V9999 COMP-3.                  ECS020
00270                                                                   ECS020
00271      12  WS-RCT-X            PIC S9(4)   COMP    VALUE +0.        ECS020
00272      12  WS-RCT-MAX          PIC S9(4)   COMP    VALUE +1500.     ECS020
00273      12  WS-LAST-REIN-CO     PIC X(6)            VALUE SPACES.    ECS020
00274      12  WS-ACCT-OV-SW       PIC X               VALUE SPACE.     ECS020
00275      12  W-TOTAL-LINE.                                            ECS020
00276          16  W-TOTAL-WORK-AREA.                                   ECS020
00277              20  W-TW-1-TITLE                                     ECS020
00278                              PIC X(09).                           ECS020
00279              20  W-TW-1-CODE PIC X(11).                           ECS020
00280              20  W-TW-1-LP   PIC X(01).                           ECS020
00281              20  W-TW-1-NAME PIC X(30).                           ECS020
00282              20  W-TW-1-RP   PIC X(01).                           ECS020
00283              20  FILLER      PIC X(01).                           ECS020
00284              20  W-TW-2-TITLE                                     ECS020
00285                              PIC X(14).                           ECS020
00286              20  W-TW-2-CODE PIC X(11).                           ECS020
00287              20  W-TW-2-LP   PIC X(01).                           ECS020
00288              20  W-TW-2-NAME PIC X(30).                           ECS020
00289              20  W-TW-2-RP   PIC X(01).                           ECS020
00290              20  FILLER      PIC X(01).                           ECS020
00291              20  W-TW-3-TITLE                                     ECS020
00292                              PIC X(14).                           ECS020
00293              20  W-TW-3-CODE PIC X(11).                           ECS020
00294              20  W-TW-3-LP   PIC X(01).                           ECS020
00295              20  W-TW-3-NAME PIC X(30).                           ECS020
00296              20  W-TW-3-RP   PIC X(01).                           ECS020
00297              20  FILLER      PIC X(01).                           ECS020
00298              20  W-TW-4-TITLE                                     ECS020
00299                              PIC X(14).                           ECS020
00300              20  W-TW-4-CODE PIC X(11).                           ECS020
00301              20  W-TW-4-LP   PIC X(01).                           ECS020
00302              20  W-TW-4-NAME PIC X(30).                           ECS020
00303              20  W-TW-4-RP   PIC X(01).                           ECS020
00304              20  FILLER      PIC X(01).                           ECS020
00305              20  W-TW-5-TITLE                                     ECS020
00306                              PIC X(14).                           ECS020
00307              20  W-TW-5-CODE PIC X(11).                           ECS020
00308              20  W-TW-5-LP   PIC X(01).                           ECS020
00309              20  W-TW-5-NAME PIC X(30).                           ECS020
00310              20  W-TW-5-RP   PIC X(01).                           ECS020
00311              20  FILLER      PIC X(01).                           ECS020
00312          16  FILLER REDEFINES W-TOTAL-WORK-AREA.                  ECS020
00313              20  W-TW-CHAR   OCCURS 285 TIMES                     ECS020
00314                              INDEXED BY W-TW-NDX                  ECS020
00315                              PIC X(01).                           ECS020
CIDMOD                                                                  ECS020
CIDMOD 01  WORK-SV-RPT-CD.                                              ECS020
CIDMOD     12  FILLER              PIC X  VALUE SPACE.                  ECS020
CIDMOD     12  WK-SV-RPT-CD        PIC X(10).                           ECS020
CIDMOD                                                                  ECS020
00316  EJECT                                                            ECS020
00317  01  FILLER PIC X(32) VALUE '******* ACCUMULATORS ***********'.   ECS020
00318  01  DATE-TOTS               PIC X(20400).                        ECS020
00319  01  ACCT-TOTS               PIC X(20400).                        ECS020
00320  01  ST-TOTS                 PIC X(20400).                        ECS020
00321  01  RPT2-TOTS               PIC X(20400).                        ECS020
00322  01  COMP-TOTS               PIC X(20400).                        ECS020
00323  01  CARR-TOTS               PIC X(20400).                        ECS020
00324  01  REIN-TOTS               PIC X(20400).                        ECS020
00325  01  RPT1-TOTS               PIC X(20400).                        ECS020
00326  01  FINL-TOTS               PIC X(20400).                        ECS020
00327  01  WORK-TOT-0              PIC X(20400).                        ECS020
00328                                                                   ECS020
00329  01  TOTAL-WORK-AREAS.                                            ECS020
00330      12  WORK-TOT-1.                                              ECS020
00331          16  WT1             OCCURS 300.                          ECS020
00332              20  WT1-KEY.                                         ECS020
00333                  24  WT1-REIN    PIC X.                           ECS020
00334                  24  WT1-LF-AH   PIC X.                           ECS020
00335                  24  WT1-BEN-TYP PIC XX.                          ECS020
00336              20  WT1-ACCUMS  PIC X(64).                           ECS020
00337                                                                   ECS020
00338      12  WORK-TOT-2.                                              ECS020
00339          16  WT2             OCCURS 300.                          ECS020
00340              20  WT2-KEY.                                         ECS020
00341                  24  WT2-REIN    PIC X.                           ECS020
00342                  24  WT2-LF-AH   PIC X.                           ECS020
00343                  24  WT2-BEN-TYP PIC XX.                          ECS020
00344              20  WT2-ACCUMS  PIC X(64).                           ECS020
00345                                                                   ECS020
00346      12  WORK-ACCUMS-0       PIC X(64).                           ECS020
00347                                                                   ECS020
00348      12  WORK-ACCUMS-1       COMP-3.                              ECS020
00349          16  WA1-Y-CERT      PIC S9(7)       VALUE ZERO.          ECS020
00350          16  WA1-Y-NET       PIC S9(11)V99   VALUE ZERO.          ECS020
00351          16  WA1-Y-EARN      PIC S9(11)V99   VALUE ZERO.          ECS020
00352          16  WA1-Y-PAID      PIC S9(11)V99   VALUE ZERO.          ECS020
00353          16  WA1-Y-RESV      PIC S9(11)V99   VALUE ZERO.          ECS020
00354          16  WA1-I-CERT      PIC S9(7)       VALUE ZERO.          ECS020
00355          16  WA1-I-NET       PIC S9(11)V99   VALUE ZERO.          ECS020
00356          16  WA1-I-EARN      PIC S9(11)V99   VALUE ZERO.          ECS020
00357          16  WA1-I-PAID      PIC S9(11)V99   VALUE ZERO.          ECS020
00358          16  WA1-I-RESV      PIC S9(11)V99   VALUE ZERO.          ECS020
00359          16  WRK-RESV        PIC S9(11)V99   VALUE ZERO.          ECS020
00360                                                                   ECS020
00361      12  WORK-ACCUMS-2       COMP-3.                              ECS020
00362          16  WA2-Y-CERT      PIC S9(7).                           ECS020
00363          16  WA2-Y-NET       PIC S9(11)V99.                       ECS020
00364          16  WA2-Y-EARN      PIC S9(11)V99.                       ECS020
00365          16  WA2-Y-PAID      PIC S9(11)V99.                       ECS020
00366          16  WA2-Y-RESV      PIC S9(11)V99.                       ECS020
00367          16  WA2-I-CERT      PIC S9(7).                           ECS020
00368          16  WA2-I-NET       PIC S9(11)V99.                       ECS020
00369          16  WA2-I-EARN      PIC S9(11)V99.                       ECS020
00370          16  WA2-I-PAID      PIC S9(11)V99.                       ECS020
00371          16  WA2-I-RESV      PIC S9(11)V99.                       ECS020
00372                                                                   ECS020
00373      12  TABLE-LIMIT         PIC S999    COMP-3  VALUE +300.      ECS020
00374      12  BEN-TOTALS          PIC X(64).                           ECS020
00375      12  TYP-TOTALS          PIC X(64).                           ECS020
00376      12  SAVE-PR-REIN        PIC X.                               ECS020
00377      12  SAVE-PR-LF-AH       PIC X.                               ECS020
00378                                                                   ECS020
00379      12  KEY-LOOK.                                                ECS020
00380          16  KL-REIN         PIC X.                               ECS020
00381          16  KL-LF-AH        PIC X.                               ECS020
00382          16  KL-BEN-TYP      PIC XX.                              ECS020
00383                                                                   ECS020
00384      12  PR-BEN-CTR          PIC S999    COMP-3  VALUE ZERO.      ECS020
00385      12  PR-TYP-CTR          PIC S999    COMP-3  VALUE ZERO.      ECS020
00386                                                                   ECS020
00387  01  SAVE-CONTROL.                                                ECS020
00388      12  SV-REIN             PIC X.                               ECS020
00389      12  SV-CONTROL.                                              ECS020
00390          16  SV-CONTROL-A.                                        ECS020
00391              20  SV-RPT-CD-1     PIC X(10).                       ECS020
00392              20  SV-CARRIER      PIC X.                           ECS020
00393              20  SV-GROUPING     PIC X(6).                        ECS020
00394              20  SV-RPT-CD-2     PIC X(10).                       ECS020
00395              20  SV-STATE        PIC XX.                          ECS020
00396              20  SV-ACCOUNT      PIC X(10).                       ECS020
00397          16  SV-EXP-DTE          PIC 9(11) COMP-3.                   CL*18
00398          16  SV-EFF-DTE          PIC 9(11) COMP-3.                   CL*18
00399      12  SV-REI-CO.                                               ECS020
00400          16  SV-REINCO       PIC XXX.                             ECS020
00401          16  SV-REINCO-SUB   PIC XXX.                             ECS020
00402      12  SV-ACCT-FLAG        PIC X.                               ECS020
00403                                                                   ECS020
00404  01  SV-NAME                 PIC X(30).                           ECS020
00405  01  SV-CITY                 PIC X(30).                           ECS020
00406  01  SV-REIN-NAME            PIC X(30).                           ECS020
JJPMOD 01  SV-X-REIN-NAME          PIC X(30).                           ECS020
JJPMOD 01  SV-X-REIN-COMP          PIC X(03).                           ECS020
00407                                                                   ECS020
00408  EJECT                                                            ECS020
00409                              COPY ERCLOSS.                        ECS020
00410                                                                   ECS020
00411  01  LOSS-RATIO-INITIALIZED  PIC X(525).                          ECS020
00412                                                                   ECS020
00413  EJECT                                                            ECS020
00414                              COPY ECSEPC01.                       ECS020
00415  EJECT                                                            ECS020
00416                              COPY ELCEPCVR.                       ECS020
00417  EJECT                                                            ECS020
00418                              COPY ELCDTECX.                       ECS020
00419  EJECT                                                            ECS020
00420                              COPY ELCDTEVR.                       ECS020
00421                                                                   ECS020
00422  01  DATE-EDIT.                                                   ECS020
00423      12  DE-MO               PIC 99.                              ECS020
00424      12  FILLER              PIC X           VALUE '/'.           ECS020
00425      12  DE-DA               PIC 99.                              ECS020
00426      12  FILLER              PIC X           VALUE '/'.           ECS020
00427      12  DE-YR               PIC 99.                              ECS020
00428  01  PRV-DEC.                                                     ECS020
00429      12  PRV-CCYY            PIC 9(04)       VALUE 1999.             CL*17
00430      12  PRV-CCYR REDEFINES PRV-CCYY.                                CL*17
00431          16  PRV-CC          PIC 99.                                 CL*17
00432          16  PRV-YR          PIC 99.                                 CL*17
00433      12  PRV-MO              PIC 99          VALUE 12.            ECS020
00434  01  WS-SV-EFF-DTE           PIC 9(11).                              CL*11
00435  01  WS-SV-EFF-DTE-R REDEFINES WS-SV-EFF-DTE.                        CL*11
00436      12  FILLER          PIC 999.                                    CL*12
00437      12  SV-EFF-CC       PIC 99.                                     CL*12
00438      12  SV-EFF-YR       PIC 99.                                     CL*11
00439      12  SV-EFF-MO       PIC 99.                                     CL*11
00440      12  SV-EFF-DA       PIC 99.                                     CL*11
00441  EJECT                                                            ECS020
00442  01  PRINT-LINES.                                                 ECS020
00443                                                                   ECS020
00444      12  LOSS-RATIO-MSG.                                          ECS020
00445          16  FILLER              PIC X(01)       VALUE '-'.       ECS020
00446          16  FILLER              PIC X(48)       VALUE            ECS020
00447          '* THIS LOSS RATIO FIELD CONTAINS A VALUE TOO LAR'.      ECS020
00448          16  FILLER              PIC X(54)       VALUE            ECS020
00449          'GE OR TOO SMALL TO BE SHOWN.'.                          ECS020
00450      12  HDR-1.                                                   ECS020
00451          16  FILLER          PIC X(48)       VALUE SPACES.        ECS020
00452          16  FILLER          PIC X(72)       VALUE                ECS020
00453             'EARNED PREMIUM AND LOSS REPORT'.                     ECS020
00454          16  FILLER          PIC X(6)        VALUE 'ECS020'.      ECS020
00455          16  H1-SUFFIX       PIC X           VALUE 'A'.           ECS020
00456      12  HDR-2.                                                   ECS020
00457          16  FILLER          PIC XX          VALUE SPACES.        ECS020
00458          16  H2-CARRIER      PIC X(10)       VALUE SPACES.        ECS020
00459          16  FILLER          PIC X           VALUE SPACES.        ECS020
00460          16  H2-CARR         PIC X           VALUE SPACES.        ECS020
00461          16  FILLER          PIC X(33)       VALUE SPACES.        ECS020
00462          16  H2-COMP         PIC X(30).                           ECS020
00463          16  FILLER          PIC X(43)       VALUE SPACES.        ECS020
00464          16  H2-DATE         PIC X(8).                            ECS020
00465      12  HDR-3.                                                   ECS020
00466          16  FILLER          PIC XX          VALUE SPACES.        ECS020
00467          16  H3-REIN         PIC X(13)       VALUE SPACES.        ECS020
00468          16  FILLER          PIC X           VALUE SPACES.        ECS020
00469          16  H3-REIN-COMP    PIC XXX         VALUE SPACES.        ECS020
00470          16  FILLER          PIC X           VALUE SPACES.        ECS020
00471          16  H3-REIN-NAME    PIC X(30)       VALUE SPACES.        ECS020
00472          16  FILLER          PIC X(4)        VALUE SPACES.        ECS020
00473          16  H3-DATE         PIC X(18).                           ECS020
00474          16  FILLER          PIC X(48)       VALUE SPACES.        ECS020
00475          16  FILLER          PIC X(5)        VALUE 'PAGE '.       ECS020
00476          16  H3-PAGE         PIC ZZ,ZZ9.                          ECS020
00477      12  HDR-3B.                                                  ECS020
00478          16  FILLER          PIC XX          VALUE SPACES.        ECS020
00479          16  H3B-RPT1-DESC   PIC X(13)     VALUE 'REPORT CODE 1'. ECS020
00480          16  FILLER          PIC XXX         VALUE ' - '.         ECS020
00481          16  H3B-RPT-CODE-1  PIC X(10)       VALUE SPACES.        ECS020
00482      12  HDR-3C.                                                  ECS020
00483          16  FILLER          PIC XX          VALUE SPACES.        ECS020
00484          16  H3C-RPT2-DESC   PIC X(13)     VALUE 'REPORT CODE 2'. ECS020
00485          16  FILLER          PIC XXX         VALUE ' - '.         ECS020
00486          16  H3C-RPT-CODE-2  PIC X(10)       VALUE SPACES.        ECS020
00487      12  HDR-3G.                                                  ECS020
00488          16  FILLER          PIC XX          VALUE SPACES.        ECS020
00489          16  H3G-G-A         PIC X(13)     VALUE 'GENERAL AGENT'. ECS020
00490          16  FILLER          PIC X           VALUE SPACES.        ECS020
00491          16  H3G-G-A-NUMBER  PIC X(10)       VALUE SPACES.        ECS020
00492          16  FILLER          PIC XX          VALUE SPACES.        ECS020
00493          16  H3G-G-A-NAME    PIC X(30)       VALUE SPACES.        ECS020
00494      12  HDR-3R.                                                  ECS020
00495          16  FILLER          PIC XX          VALUE SPACES.        ECS020
00496          16  H3R-REIN        PIC X(13)       VALUE SPACES.        ECS020
00497          16  FILLER          PIC XX          VALUE '( '.          ECS020
00498          16  H3R-REIN-COMP   PIC XXX         VALUE SPACES.        ECS020
00499          16  FILLER          PIC XXX         VALUE ' ) '.         ECS020
00500          16  H3R-REIN-NAME   PIC X(30)       VALUE SPACES.        ECS020
00501      12  HDR-4A.                                                  ECS020
00502          16  H4A-SUMMARY     PIC X(20)       VALUE SPACES.        ECS020
00503          16  H4A-CARRIER     PIC X(7)        VALUE SPACES.        ECS020
00504          16  H4A-CARR        PIC X           VALUE SPACES.        ECS020
00505          16  FILLER          PIC X           VALUE SPACES.        ECS020
00506          16  H4A-GROUP       PIC X(5)        VALUE SPACES.        ECS020
00507          16  H4A-GRP         PIC X(6)        VALUE SPACES.        ECS020
00508          16  H4A-STATE       PIC X(5)        VALUE SPACES.        ECS020
00509          16  H4A-ST          PIC XX          VALUE SPACES.        ECS020
00510          16  H4A-ACCOUNT     PIC X(10)       VALUE SPACES.        ECS020
00511          16  H4A-ACCT        PIC X(10)       VALUE SPACES.        ECS020
00512          16  FILLER          PIC X           VALUE SPACES.        ECS020
00513          16  H4A-AM-NAME     PIC X(31)       VALUE SPACES.        ECS020
00514          16  FILLER          PIC X           VALUE SPACES.        ECS020
00515          16  H4A-CTY-ST      PIC X(15)       VALUE SPACES.        ECS020
00516          16  FILLER          PIC X           VALUE SPACES.        ECS020
00517          16  H4A-EFFECTIVE   PIC X(8)        VALUE SPACES.        ECS020
00518          16  H4A-EFF-DTE     PIC X(8)        VALUE SPACES.        ECS020
00519      12  HDR-4A-SUB-DESC.                                         ECS020
00520          16  FILLER          PIC X(11)    VALUE '  REIN SUB '.    ECS020
00521          16  H4A-SUB         PIC XXX.                             ECS020
00522          16  FILLER          PIC X           VALUE SPACES.        ECS020
00523      12  HDR-4-TOTAL.                                             ECS020
00524          16  FILLER          PIC X(12)                            ECS020
00525              VALUE ' TOTALS FOR '.                                ECS020
00526          16  H4T-LINE.                                            ECS020
00527              20  H4T-CHAR OCCURS 121 TIMES                        ECS020
00528                           INDEXED BY H4T-NDX                      ECS020
00529                              PIC X(01).                           ECS020
00530      12  HDR-5A.                                                  ECS020
00531          16  FILLER          PIC X(45)       VALUE                ECS020
00532             '            ** - - - - - - - - - - YEAR-TO-DA'.      ECS020
00533          16  FILLER          PIC X(44)       VALUE                ECS020
00534             'TE - - - - - - - - - - ** ** - - - - - - - -'.       ECS020
00535          16  FILLER          PIC X(44)       VALUE                ECS020
00536             ' -  INCEPTION-TO-DATE - - - - - - - - - - **'.       ECS020
00537      12  HDR-5B.                                                  ECS020
00538          16  FILLER          PIC X(45)       VALUE                ECS020
00539             '            ** - - - - - - - - - - YEAR-TO-DA'.      ECS020
00540          16  FILLER          PIC X(44)       VALUE                ECS020
00541             'TE - - - - - - - - - - ** ** - - - - - - - -'.       ECS020
00542          16  FILLER          PIC X(44)       VALUE                ECS020
00543             ' -  INCEPTION-TO-DATE - - - - - - - - - - **'.       ECS020
00544      12  HDR-5C.                                                  ECS020
00545          16  FILLER          PIC X(45)       VALUE                ECS020
00546             '            ** - - - - - - - - - -  YEAR-TO-D'.      ECS020
00547          16  FILLER          PIC X(44)       VALUE                ECS020
00548             'ATE  - - - - - - - - - - ** ** - - - - - - -'.       ECS020
00549          16  FILLER          PIC X(44)       VALUE                ECS020
00550             ' - -  INCEPTION-TO-DATE - - - - - - - - - **'.       ECS020
00551      12  HDR-5D.                                                  ECS020
00552          16  FILLER          PIC X(45)       VALUE                ECS020
00553             '            ** - - - - - - - - YEAR-TO-DATE -'.      ECS020
00554          16  FILLER          PIC X(44)       VALUE                ECS020
00555             ' - - - - - - -  ** ** - - - - - - - - INCEPT'.       ECS020
00556          16  FILLER          PIC X(44)       VALUE                ECS020
00557             'ION-TO-DATE - - - - - - - - **              '.       ECS020
00558      12  HDR-6A.                                                  ECS020
00559          16  FILLER          PIC X(45)       VALUE                ECS020
00560             '            NUMBER    WRITTEN     EARNED     '.      ECS020
00561          16  FILLER          PIC X(44)       VALUE                ECS020
00562             'CLAIMS     CLAIM    LOSS  NUMBER     WRITTEN'.       ECS020
00563          16  FILLER          PIC X(44)       VALUE                ECS020
00564             '      EARNED      CLAIMS      CLAIM    LOSS '.       ECS020
00565      12  HDR-6B.                                                  ECS020
00566          16  FILLER          PIC X(45)       VALUE                ECS020
00567             '               WRITTEN        EARNED       CL'.      ECS020
00568          16  FILLER          PIC X(44)       VALUE                ECS020
00569             'AIMS      CLAIM     LOSS      WRITTEN       '.       ECS020
00570          16  FILLER          PIC X(44)       VALUE                ECS020
00571             '  EARNED        CLAIMS       CLAIM    LOSS  '.       ECS020
00572      12  HDR-6C.                                                  ECS020
00573          16  FILLER          PIC X(45)       VALUE                ECS020
00574             '             NUMBER       WRITTEN         EAR'.      ECS020
00575          16  FILLER          PIC X(44)       VALUE                ECS020
00576             'NED         CLAIMS    LOSS   NUMBER       WR'.       ECS020
00577          16  FILLER          PIC X(44)       VALUE                ECS020
00578             'ITTEN         EARNED         CLAIMS    LOSS '.       ECS020
00579      12  HDR-6D.                                                  ECS020
00580          16  FILLER          PIC X(45)       VALUE                ECS020
00581             '                WRITTEN      EARNED      CLAI'.      ECS020
00582          16  FILLER          PIC X(44)       VALUE                ECS020
00583             'MS      CLAIM  LOSS     WRITTEN       EARNED'.       ECS020
00584          16  FILLER          PIC X(44)       VALUE                ECS020
00585             '       CLAIMS       CLAIM  LOSS    UNEARNED '.       ECS020
00586      12  HDR-7A.                                                  ECS020
00587          16  FILLER          PIC X(45)       VALUE                ECS020
00588             '             CERTS    PREMIUM    PREMIUM     '.      ECS020
00589          16  FILLER          PIC X(44)       VALUE                ECS020
00590             ' PAID     RESERVE  RATIO             PREMIUM'.       ECS020
00591          16  FILLER          PIC X(44)       VALUE                ECS020
00592             '     PREMIUM       PAID      RESERVE  RATIO '.       ECS020
00593      12  HDR-7B.                                                  ECS020
00594          16  FILLER          PIC X(45)       VALUE                ECS020
00595             '               PREMIUM       PREMIUM        P'.      ECS020
00596          16  FILLER          PIC X(44)       VALUE                ECS020
00597             'AID      RESERVE   RATIO      PREMIUM       '.       ECS020
00598          16  FILLER          PIC X(44)       VALUE                ECS020
00599             ' PREMIUM         PAID       RESERVE   RATIO '.       ECS020
00600      12  HDR-7C.                                                  ECS020
00601          16  FILLER          PIC X(45)       VALUE                ECS020
00602             '              CERTS       PREMIUM        PREM'.      ECS020
00603          16  FILLER          PIC X(44)       VALUE                ECS020
00604             'IUM          PAID    RATIO    CERTS       PR'.       ECS020
00605          16  FILLER          PIC X(44)       VALUE                ECS020
00606             'EMIUM        PREMIUM          PAID    RATIO '.       ECS020
00607      12  HDR-7D.                                                  ECS020
00608          16  FILLER          PIC X(45)       VALUE                ECS020
00609             '                PREMIUM     PREMIUM       PAI'.      ECS020
00610          16  FILLER          PIC X(44)       VALUE                ECS020
00611             'D      RESERVE RATIO    PREMIUM      PREMIUM'.       ECS020
00612          16  FILLER          PIC X(44)       VALUE                ECS020
00613             '        PAID       RESERVE RATIO    PREMIUM '.       ECS020
00614      12  HDR-8.                                                   ECS020
00615          16  H8-SW           PIC X           VALUE SPACES.        ECS020
00616          16  H8-DESC         PIC X(13).                           ECS020
00617                                                                   ECS020
00618      12  DTL-1A.                                                  ECS020
00619          16  FILLER          PIC X           VALUE SPACES.        ECS020
00620          16  D1-DESC.                                             ECS020
00621              20  D1-DESC1    PIC X(6).                            ECS020
00622              20  D1-DESC2    PIC X(4).                            ECS020
00623          16  D1A-Y-CERTS     PIC ZZZZ,ZZ9-.                       ECS020
00624          16  D1A-Y-NET       PIC ZZ,ZZZ,ZZ9-.                     ECS020
00625          16  D1A-Y-EARNED    PIC ZZ,ZZZ,ZZ9-.                     ECS020
00626          16  D1A-Y-PAID      PIC ZZ,ZZZ,ZZ9-.                     ECS020
00627          16  D1A-Y-RESERVE   PIC ZZ,ZZZ,ZZ9-.                     ECS020
00628          16  D1A-Y-LOSS-OV.                                       ECS020
00629              20  D1A-Y-LOSS  PIC ZZ9.9-.                          ECS020
00630          16  D1A-I-CERTS     PIC ZZZZ,ZZ9-.                       ECS020
00631          16  D1A-I-NET       PIC ZZZ,ZZZ,ZZ9-.                    ECS020
00632          16  D1A-I-EARNED    PIC ZZZ,ZZZ,ZZ9-.                    ECS020
00633          16  D1A-I-PAID      PIC ZZZ,ZZZ,ZZ9-.                    ECS020
00634          16  D1A-I-RESERVE   PIC ZZZ,ZZZ,ZZ9-.                    ECS020
00635          16  D1A-I-LOSS-OV.                                       ECS020
00636              20  D1A-I-LOSS  PIC ZZ9.9-.                          ECS020
00637      12  DTL-1B              REDEFINES DTL-1A.                    ECS020
00638          16  FILLER          PIC X(11).                           ECS020
00639          16  D1B-Y-NET       PIC ZZ,ZZZ,ZZZ.99-.                  ECS020
00640          16  D1B-Y-EARNED    PIC ZZ,ZZZ,ZZZ.99-.                  ECS020
00641          16  D1B-Y-PAID      PIC Z(5),ZZZ.99-.                    ECS020
00642          16  D1B-Y-RESERVE   PIC Z(4),ZZZ.99-.                    ECS020
00643          16  D1B-Y-LOSS-OV.                                       ECS020
00644              20  D1B-Y-LOSS  PIC ZZ9.9-.                          ECS020
00645          16  D1B-I-NET       PIC ZZZ,ZZZ,ZZZ.99-.                 ECS020
00646          16  D1B-I-EARNED    PIC ZZZ,ZZZ,ZZZ.99-.                 ECS020
00647          16  D1B-I-PAID      PIC Z(6),ZZZ.99-.                    ECS020
00648          16  D1B-I-RESERVE   PIC Z(5),ZZZ.99-.                    ECS020
00649          16  D1B-I-LOSS-OV.                                       ECS020
00650              20  D1B-I-LOSS  PIC ZZ9.9-.                          ECS020
00651      12  DTL-1C              REDEFINES DTL-1B.                    ECS020
00652          16  FILLER          PIC X(11).                           ECS020
00653          16  D1C-Y-CERTS     PIC Z,ZZZ,ZZ9-.                      ECS020
00654          16  D1C-Y-NET       PIC ZZZ,ZZZ,ZZZ.99-.                 ECS020
00655          16  D1C-Y-EARNED    PIC ZZZ,ZZZ,ZZZ.99-.                 ECS020
00656          16  D1C-Y-PAID      PIC ZZZ,ZZZ,ZZZ.99-.                 ECS020
00657          16  D1C-Y-LOSS-OV.                                       ECS020
00658              20  D1C-Y-LOSS  PIC ZZ9.9-.                          ECS020
00659          16  D1C-I-CERTS     PIC Z,ZZZ,ZZ9-.                      ECS020
00660          16  D1C-I-NET       PIC ZZZ,ZZZ,ZZZ.99-.                 ECS020
00661          16  D1C-I-EARNED    PIC ZZZ,ZZZ,ZZZ.99-.                 ECS020
00662          16  D1C-I-PAID      PIC ZZZ,ZZZ,ZZZ.99-.                 ECS020
00663          16  D1C-I-LOSS-OV.                                       ECS020
00664              20  D1C-I-LOSS  PIC ZZ9.9-.                          ECS020
00665      12  DTL-1D              REDEFINES DTL-1C.                    ECS020
00666          16  FILLER          PIC X(12).                           ECS020
00667          16  D1D-Y-NET       PIC ZZZ,ZZZ,ZZ9-.                    ECS020
00668          16  D1D-Y-EARNED    PIC ZZZ,ZZZ,ZZ9-.                    ECS020
00669          16  D1D-Y-PAID      PIC ZZZ,ZZZ,ZZ9-.                    ECS020
00670          16  D1D-Y-RESERVE   PIC ZZZ,ZZZ,ZZ9-.                    ECS020
00671          16  D1D-Y-LOSS-OV.                                       ECS020
00672              20  D1D-Y-LOSS  PIC ZZ9-.                            ECS020
00673          16  D1D-I-NET       PIC ZZZZ,ZZZ,ZZ9-.                   ECS020
00674          16  D1D-I-EARNED    PIC ZZZZ,ZZZ,ZZ9-.                   ECS020
00675          16  D1D-I-PAID      PIC ZZZZ,ZZZ,ZZ9-.                   ECS020
00676          16  D1D-I-RESERVE   PIC ZZZZ,ZZZ,ZZ9-.                   ECS020
00677          16  D1D-I-LOSS-OV.                                       ECS020
00678              20  D1D-I-LOSS  PIC ZZ9-.                            ECS020
00679          16  D1D-UNEARNED    PIC ZZZZ,ZZZ,ZZ9-.                   ECS020
00680  EJECT                                                            ECS020
00681  PROCEDURE DIVISION.                                              ECS020
00682                                                                   ECS020
00683  0000-READ-DATE.                                                  ECS020
00684                              COPY ELCDTERX.                          CL*19
00685                                                                   ECS020
00686  0010-INITIALIZATION.                                             ECS020
00687      MOVE SPACES          TO DTL-1A.                              ECS020
00688      MOVE WS-CURRENT-DATE TO H2-DATE.                             ECS020
00689      MOVE COMPANY-NAME    TO H2-COMP.                             ECS020
00690      MOVE ALPH-DATE       TO H3-DATE.                             ECS020
00691                                                                   ECS020
00692      IF CLAS-REPORT-CD1-CAPTION NOT = SPACES                      ECS020
00693          MOVE CLAS-REPORT-CD1-CAPTION TO H3B-RPT1-DESC.           ECS020
00694      IF CLAS-REPORT-CD2-CAPTION NOT = SPACES                      ECS020
00695          MOVE CLAS-REPORT-CD2-CAPTION TO H3C-RPT2-DESC.           ECS020
00696                                                                   ECS020
00697      IF DTE-PGM-OPT = 6                                           ECS020
00698          MOVE 2   TO DTE-PGM-OPT                                  ECS020
00699          MOVE 'X' TO WS-ACCT-OV-SW.                               ECS020
00700                                                                   ECS020
00701      IF DTE-FMT-OPT = '2'                                         ECS020
00702          MOVE HDR-5B TO HDR-5A                                    ECS020
00703          MOVE HDR-6B TO HDR-6A                                    ECS020
00704          MOVE HDR-7B TO HDR-7A.                                   ECS020
00705                                                                   ECS020
00706      IF DTE-FMT-OPT = '3'                                         ECS020
00707          MOVE HDR-5C TO HDR-5A                                    ECS020
00708          MOVE HDR-6C TO HDR-6A                                    ECS020
00709          MOVE HDR-7C TO HDR-7A.                                   ECS020
00710                                                                   ECS020
00711      IF DTE-FMT-OPT = '4'                                         ECS020
00712          MOVE HDR-5D TO HDR-5A                                    ECS020
00713          MOVE HDR-6D TO HDR-6A                                    ECS020
00714          MOVE HDR-7D TO HDR-7A.                                   ECS020
00715                                                                   ECS020
00716      MOVE LIFE-OVERRIDE-L1            TO L1.                      ECS020
00717                                                                   ECS020
00718      SUBTRACT 1 FROM RUN-CCYY GIVING PRV-CCYY.                       CL*16
00719                                                                   ECS020
00720      MOVE DTE-CONV-DT           TO WS-RUN-DTE-N.                     CL*29
00721                                                                   ECS020
00722      IF WS-RUN-YR = RUN-YR                                        ECS020
00723          MOVE WS-RUN-CC TO PRV-CC                                    CL*16
00724          MOVE WS-RUN-YR TO PRV-YR                                    CL*16
00725          MOVE WS-RUN-MO TO PRV-MO.                                ECS020
00726                                                                   ECS020
00727      MOVE RUN-DATE              TO WS-RUN-DTE-N.                     CL*29
00728                                                                   ECS020
00729      MOVE SPACES                TO LOSS-RATIO-MASTER.             ECS020
00730      MOVE 'LR'                  TO LR-RECORD-ID.                  ECS020
00731      MOVE LOW-VALUES            TO LR-CONTROL.                    ECS020
00732      MOVE DTE-CLASIC-COMPANY-CD TO LR-COMPANY-CD.                 ECS020
00733      MOVE WS-RUN-DTE-N          TO LR-RUN-DATE-N.                    CL*29
00734                                                                   ECS020
00735      MOVE ZEROS           TO  LR-AGT-NO (1 1)   LR-SNG-PCT (1 1)  ECS020
00736                               LR-AGT-NO (1 2)   LR-SNG-PCT (1 2)  ECS020
00737                               LR-AGT-NO (1 3)   LR-SNG-PCT (1 3)  ECS020
00738                               LR-JNT-PCT (1 1)  LR-A-H-PCT (1 1)  ECS020
00739                               LR-JNT-PCT (1 2)  LR-A-H-PCT (1 2)  ECS020
00740                               LR-JNT-PCT (1 3)  LR-A-H-PCT (1 3). ECS020
00741      MOVE LR-ACCT-RANGES (1)    TO LR-ACCT-RANGES (2).            ECS020
00742                                                                   ECS020
00743      MOVE ZEROS             TO LR-YTD-NET (1)    LR-ITD-NET (1)   ECS020
00744                                LR-YTD-EARN (1)   LR-ITD-EARN (1)  ECS020
00745                                LR-YTD-PAID (1)   LR-ITD-PAID (1)  ECS020
00746                                LR-YTD-RESV (1)   LR-ITD-RESV (1)  ECS020
00747                                LR-YTD-INCUR (1)  LR-ITD-INCUR (1) ECS020
00748                                LR-YTD-RATIO (1)  LR-ITD-RATIO (1).ECS020
00749      MOVE LR-TOTALS (1)         TO LR-TOTALS (2)   LR-TOTALS (3). ECS020
00750                                                                   ECS020
00751      MOVE LOSS-RATIO-MASTER     TO LOSS-RATIO-INITIALIZED.        ECS020
00752                                                                   ECS020
00753      IF DTE-CLIENT = 'FFL'                                        ECS020
00754          MOVE +.005 TO FAC-1                                      ECS020
00755          MOVE ZEROS TO FAC-2.                                     ECS020
00756                                                                   ECS020
00757      IF DTE-CLIENT = 'FIM'                                        ECS020
00758          MOVE +.015 TO FAC-1                                      ECS020
00759          MOVE +.025 TO FAC-2.                                     ECS020
00760                                                                   ECS020
00761      MOVE WORK-ACCUMS-1 TO WORK-ACCUMS-0.                         ECS020
00762      MOVE WORK-ACCUMS-0 TO WT1-ACCUMS (1).                        ECS020
00763      MOVE SPACES        TO WT1-KEY (1).                           ECS020
00764      MOVE +2            TO X1.                                    ECS020
00765                                                                   ECS020
00766      EJECT                                                        ECS020
00767  0050-OPEN-CONTROL-FILE.                                          ECS020
00768      OPEN INPUT ELCNTL.                                           ECS020
00769                                                                   ECS020
00770      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        ECS020
00771          NEXT SENTENCE                                            ECS020
00772        ELSE                                                       ECS020
00773          MOVE '**** ELCNTL OPEN ERROR ****'                       ECS020
00774                                  TO WS-ABEND-MESSAGE              ECS020
00775          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS020
00776          GO TO ABEND-PGM.                                         ECS020
00777                                                                   ECS020
00778  0055-READ-REPORT-RECORD.                                         ECS020
00779      MOVE SPACES                 TO CF-CONTROL-PRIMARY.           ECS020
00780      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                ECS020
00781      MOVE 'C'                    TO CF-RECORD-TYPE.               ECS020
00782      MOVE PGM-SUB                TO CF-CUSTOM-REPORT-NO.          ECS020
00783      MOVE +0                     TO CF-SEQUENCE-NO.               ECS020
00784                                                                   ECS020
00785      READ ELCNTL.                                                 ECS020
00786                                                                   ECS020
00787      IF ELCNTL-FILE-STATUS NOT = '00'                             ECS020
00788          MOVE 'X'                TO WS-CUSTOMIZATION-FLAG.        ECS020
00789                                                                   ECS020
00790      EJECT                                                        ECS020
00791  0085-INIT-WORK-ZERO.                                             ECS020
00792      MOVE WT1 (1) TO WT1 (X1).                                    ECS020
00793      ADD +1 TO X1.                                                ECS020
00794                                                                   ECS020
00795      IF X1 NOT GREATER THAN TABLE-LIMIT                           ECS020
00796          GO TO 0085-INIT-WORK-ZERO.                               ECS020
00797                                                                   ECS020
00798      MOVE WORK-TOT-1                TO WORK-TOT-0.                ECS020
00799                                                                   ECS020
00800      OPEN INPUT RTBL-FILE.                                        ECS020
00801                                                                   ECS020
00802      IF REIN-FILE-STATUS = '35'                                   ECS020
00803          GO TO 0100-SET-SORT-SEQUENCE.                            ECS020
00804                                                                   ECS020
00805      IF REIN-FILE-STATUS  = '00' OR '97'                          ECS020
00806          NEXT SENTENCE                                            ECS020
00807        ELSE                                                       ECS020
00808          MOVE '2'                  TO WAC-1                       ECS020
00809          MOVE '1'                  TO WAC-2                       ECS020
00810          MOVE REIN-FILE-STATUS     TO WAC-3-4                     ECS020
00811          MOVE WORK-ABEND-CODE      TO WS-RETURN-CODE              ECS020
00812          GO TO ABEND-PGM.                                         ECS020
00813                                                                   ECS020
00814      MOVE HIGH-VALUES TO WS-REIN-COMP-TBL.                        ECS020
00815                                                                   ECS020
00816  EJECT                                                            ECS020
00817  0090-READ-RTBL-FILE.                                             ECS020
00818      READ RTBL-FILE.                                              ECS020
00819                                                                   ECS020
00820      IF REIN-FILE-STATUS = '10'                                   ECS020
00821          CLOSE RTBL-FILE                                          ECS020
00822          IF REIN-FILE-STATUS = '00'                               ECS020
00823              GO TO 0100-SET-SORT-SEQUENCE                         ECS020
00824          ELSE                                                     ECS020
00825              MOVE '2'              TO WAC-1                       ECS020
00826              MOVE '2'              TO WAC-2                       ECS020
00827              MOVE REIN-FILE-STATUS TO WAC-3-4                     ECS020
00828              MOVE WORK-ABEND-CODE  TO WS-RETURN-CODE              ECS020
00829              GO TO ABEND-PGM.                                     ECS020
00830                                                                   ECS020
00831      IF REIN-FILE-STATUS NOT = '00'                               ECS020
00832          MOVE '2'               TO WAC-1                          ECS020
00833          MOVE '4'               TO WAC-2                          ECS020
00834          MOVE REIN-FILE-STATUS  TO WAC-3-4                        ECS020
00835          MOVE WORK-ABEND-CODE   TO WS-RETURN-CODE                 ECS020
00836          GO TO ABEND-PGM.                                         ECS020
00837                                                                   ECS020
00838      IF NOT RE-COMPANY-RECORD                                     ECS020
00839          GO TO 0090-READ-RTBL-FILE.                               ECS020
00840                                                                   ECS020
00841      ADD +1 TO WS-RCT-X.                                          ECS020
00842                                                                   ECS020
00843      IF WS-RCT-X GREATER THAN WS-RCT-MAX                          ECS020
00844          MOVE '0201'            TO WS-RETURN-CODE                 ECS020
00845          GO TO ABEND-PGM.                                         ECS020
00846                                                                   ECS020
00847      MOVE RE-COMPANY            TO WS-RCT-COMP (WS-RCT-X).        ECS020
00848      MOVE RE-NAME               TO WS-RCT-NAME (WS-RCT-X).        ECS020
00849      MOVE RE-CEDE-NAME          TO WS-RCT-CEDE (WS-RCT-X).        ECS020
00850                                                                   ECS020
00851      IF RE-LF-IBNR-PCT NOT NUMERIC  MOVE ZEROS TO RE-LF-IBNR-PCT. ECS020
00852      IF RE-AH-IBNR-PCT NOT NUMERIC  MOVE ZEROS TO RE-AH-IBNR-PCT. ECS020
00853      IF RE-LF-PR-PCT   NOT NUMERIC  MOVE ZEROS TO RE-LF-PR-PCT.   ECS020
00854      IF RE-LF-78-PCT   NOT NUMERIC  MOVE ZEROS TO RE-LF-78-PCT.   ECS020
00855      IF RE-AH-PR-PCT   NOT NUMERIC  MOVE ZEROS TO RE-AH-PR-PCT.   ECS020
00856      IF RE-AH-78-PCT   NOT NUMERIC  MOVE ZEROS TO RE-AH-78-PCT.   ECS020
00857                                                                   ECS020
00858      MOVE RE-LF-IBNR-PCT        TO WS-LF-IBNR (WS-RCT-X).         ECS020
00859      MOVE RE-AH-IBNR-PCT        TO WS-AH-IBNR (WS-RCT-X).         ECS020
00860                                                                   ECS020
00861      MOVE RE-LF-PR-PCT          TO WS-LF-PRO (WS-RCT-X).          ECS020
00862      MOVE RE-LF-78-PCT          TO WS-LF-R78 (WS-RCT-X).          ECS020
00863      MOVE RE-AH-PR-PCT          TO WS-AH-PRO (WS-RCT-X).          ECS020
00864      MOVE RE-AH-78-PCT          TO WS-AH-R78 (WS-RCT-X).          ECS020
00865                                                                   ECS020
00866      GO TO 0090-READ-RTBL-FILE.                                   ECS020
00867  EJECT                                                            ECS020
00868  0100-SET-SORT-SEQUENCE.                                          ECS020
00869                                                                   ECS020
00870          SORT SORT-WORK ON ASCENDING  SW-REIN                     ECS020
JJPMOD                                      SW-REIN-NAME                ECS020
00871                                       SW-REINCO                   ECS020
00872                                       SW-CONTROL                  ECS020
00873                                       SW-REINCO-SUB               ECS020
00874                                       SW-RCD-TYPE                 ECS020
00875                                       SW-BEN-CODE                 ECS020
00876                                       SW-RUN-DTE                  ECS020
00877                            DESCENDING SW-PURGE                    ECS020
00878          INPUT PROCEDURE  0200-SORT-INPUT THRU 0299-EXIT          ECS020
00879          OUTPUT PROCEDURE 0300-PRINT-EPL  THRU 2999-EXIT.         ECS020
00880                                                                   ECS020
00881      IF SORT-RETURN NOT = ZEROS                                   ECS020
00882          MOVE '0101' TO WS-RETURN-CODE                            ECS020
00883          GO TO ABEND-PGM.                                         ECS020
00884                                                                   ECS020
00885      GO TO 9999-END-OF-JOB.                                       ECS020
00886  EJECT                                                            ECS020
00887  0200-SORT-INPUT SECTION.                                         ECS020
00888                                                                   ECS020
00889      OPEN INPUT EXTRACTS  ACCT-MASTER.                               CL*28
00890                                                                      CL*23
00891      IF AM-FILE-STATUS  = '00' OR '97'                            ECS020
00892          NEXT SENTENCE                                            ECS020
00893        ELSE                                                       ECS020
00894          MOVE '1'                  TO WAC-1                       ECS020
00895          MOVE '1'                  TO WAC-2                       ECS020
00896          MOVE AM-FILE-STATUS       TO WAC-3-4                     ECS020
00897          MOVE WORK-ABEND-CODE      TO WS-RETURN-CODE              ECS020
00898          GO TO ABEND-PGM.                                         ECS020
00899                                                                   ECS020
00900  0210-READ-EXTRACTS.                                              ECS020
00901      READ EXTRACTS                                                ECS020
00902                AT END CLOSE EXTRACTS                              ECS020
unix                        DISPLAY 'EXTRACTS AT END '
00903                       GO TO 0299-EXIT.                            ECS020
00904                                                                   ECS020
unix  *     IF (EPR-CARRIER = '9')
unix  *        AND (EPR-STATE = 'WY')
unix  *        CLOSE EXTRACTS
unix  *        DISPLAY ' PAST 6 AND NE '
unix  *        GO TO 0299-EXIT
unix  *     END-IF
unix       .
00905                                                                   ECS020
00906  0230-SELECT-EXTRACTS.                                            ECS020
00907                                                                   ECS020
00908      IF EPR-REC-ID NOT = 'EP'                                     ECS020
00909          GO TO 0210-READ-EXTRACTS.                                ECS020
00910                                                                   ECS020
00911      MOVE EPR-RUN-DATE TO WS-EPR-RUN-DATE.                           CL*21
00912                                                                      CL**3
00913      IF EPR-PURGE     NOT = 'P'          AND                         CL*29
00914         EPR-RUN-DATE  NOT = WS-RUN-DTE-N AND                         CL**9
00915         EPR-RUN-YR-MO NOT = PRV-DEC                               ECS020
00916           GO TO 0210-READ-EXTRACTS.                               ECS020
00917                                                                   ECS020
00918      IF EPR-PURGE = 'P'                                           ECS020
00919          AND EPR-RUN-DATE GREATER THAN WS-RUN-DTE-N                  CL**9
00920              GO TO 0210-READ-EXTRACTS.                            ECS020
00921                                                                   ECS020
00922      MOVE SPACE                            TO SW-ACCT-FLAG.       ECS020
00923      IF CUSTOMIZATION-FOUND                                       ECS020
00924          IF CF-ACTIVE-ACCOUNTS                                    ECS020
00925              MOVE DTE-CLASIC-COMPANY-CD    TO AM-COMPANY-CD       ECS020
00926              MOVE EPR-CONTROL              TO AM-MSTR-CNTRL       ECS020
00927              PERFORM 2700-READ-AM-MSTR THRU 2700-EXIT             ECS020
00928              IF NOT AM-ACCOUNT-ACTIVE                             ECS020
00929                  MOVE 'X'                  TO SW-ACCT-FLAG.       ECS020
00930                                                                   ECS020
00931  0250-RELEASE-EXTRACTS.                                           ECS020
00932      IF EPR-PURGE NOT = 'P'                                       ECS020
00933          IF EPR-RUN-DATE = WS-RUN-DTE-N                              CL**9
00934              MOVE 'A' TO EPR-PURGE                                ECS020
00935          ELSE                                                     ECS020
00936              IF EPR-RUN-YR-MO = PRV-DEC                           ECS020
00937                  MOVE 'S' TO EPR-PURGE.                           ECS020
00938                                                                   ECS020
00939      IF EPR-PURGE = 'P'                                           ECS020
00940          AND EPR-RUN-YR-MO GREATER THAN PRV-DEC                   ECS020
00941              MOVE 'Q' TO EPR-PURGE.                               ECS020
00942                                                                   ECS020
00943      IF EPR-RCD-TYPE = AH-OVERRIDE-L1                             ECS020
00944          MOVE 'Z'                  TO EPR-RCD-TYPE.               ECS020
00945                                                                   ECS020
00946      MOVE LOW-VALUES               TO SW-CONTROL.                 ECS020
JJPMOD     MOVE LOW-VALUES               TO SW-REIN-NAME.               ECS020
JJPMOD     MOVE LOW-VALUES               TO SV-X-REIN-COMP.             ECS020
00947      MOVE EPR-CARRIER              TO SW-CARRIER.                 ECS020
00948      MOVE EPR-GROUPING             TO SW-GROUPING.                ECS020
00949      MOVE EPR-STATE                TO SW-STATE.                   ECS020
00950      MOVE EPR-ACCOUNT              TO SW-ACCOUNT.                 ECS020
00951      MOVE EPR-EXP-DTE              TO SW-EXP-DATE.                ECS020
00952      MOVE EPR-EFF-DTE              TO SW-EFF-DATE.                ECS020
00953                                                                   ECS020
00954      MOVE EPR-RECORD               TO SW-EP-RECORD.               ECS020
00955                                                                   ECS020
00956      IF SW-REIN NOT = SPACE                                       ECS020
00957          MOVE 'R'                  TO SW-REIN                     ECS020
unix           MOVE SORT-RECORD          TO WS-SAVE-SORT-REC
00958          RELEASE SORT-RECORD                                      ECS020
unix           MOVE WS-SAVE-SORT-REC     TO SORT-RECORD
JJPMOD         MOVE 'X'                  TO SW-REIN                     ECS020
JJPMOD         PERFORM 2250-FIND-REIN-NAME THRU 2270-EXIT               ECS020
unix           MOVE SORT-RECORD          TO WS-SAVE-SORT-REC
JJPMOD         RELEASE SORT-RECORD                                      ECS020
unix           MOVE WS-SAVE-SORT-REC     TO SORT-RECORD
00959          GO TO 0210-READ-EXTRACTS.                                ECS020
00960                                                                   ECS020
00961      MOVE 'A'                      TO SW-REIN.                    ECS020
00962      MOVE LOW-VALUES               TO SW-REI-CO.                  ECS020
JJPMOD     MOVE LOW-VALUES               TO SW-REIN-NAME.               ECS020
unix       MOVE SORT-RECORD              TO WS-SAVE-SORT-REC
00963      RELEASE SORT-RECORD.                                         ECS020
unix       MOVE WS-SAVE-SORT-REC         TO SORT-RECORD
00964                                                                   ECS020
00965      MOVE DTE-CLASIC-COMPANY-CD    TO AM-COMPANY-CD.              ECS020
00966      MOVE EPR-CONTROL              TO AM-MSTR-CNTRL.              ECS020
00967      PERFORM 2700-READ-AM-MSTR THRU 2700-EXIT.                    ECS020
00968                                                                   ECS020
00969      IF DTE-TOT-OPT = '5' OR '6' OR '7' OR '8'                    ECS020
00970          GO TO 0280-RELEASE-RPTCD-EXTRACTS.                       ECS020
00971                                                                   ECS020
00972      MOVE 'G'                      TO SW-REIN.                    ECS020
00973      MOVE +1 TO X1.                                               ECS020
00974                                                                   ECS020
00975  0270-RELEASE-GA-EXTRACTS.                                        ECS020
00976                                                                   ECS020
00977      IF AM-COM-TYP (X1) = 'O'  OR  'P'                            ECS020
00978          MOVE AM-AGT (X1)          TO SW-RPT-CD-2                 ECS020
unix           MOVE SORT-RECORD          TO WS-SAVE-SORT-REC
00979          RELEASE SORT-RECORD                                      ECS020
unix           MOVE WS-SAVE-SORT-REC     TO SORT-RECORD
unix       END-IF
00980                                                                   ECS020
00981      ADD +1 TO X1.                                                ECS020
00982      IF X1 LESS THAN +11                                          ECS020
00983          GO TO 0270-RELEASE-GA-EXTRACTS.                          ECS020
00984                                                                   ECS020
00985  0280-RELEASE-RPTCD-EXTRACTS.                                     ECS020
00986                                                                   ECS020
00987      IF DTE-TOT-OPT = '2' OR '4' OR '6' OR '8'                    ECS020
00988          IF AM-REPORT-CODE-1 NOT = SPACES                         ECS020
00989              MOVE 'B'              TO SW-REIN                     ECS020
00990              MOVE AM-REPORT-CODE-1 TO SW-RPT-CD-1                 ECS020
00991              MOVE LOW-VALUES       TO SW-RPT-CD-2                 ECS020
unix               MOVE SORT-RECORD      TO WS-SAVE-SORT-REC
00992              RELEASE SORT-RECORD                                  ECS020
unix               MOVE WS-SAVE-SORT-REC TO SORT-RECORD
unix           END-IF
           END-IF
00993                                                                   ECS020
00994                                                                   ECS020
00995      IF DTE-TOT-OPT = '3' OR '4' OR '7' OR '8'                    ECS020
00996          IF AM-REPORT-CODE-2 NOT = SPACES                         ECS020
00997              MOVE 'C'              TO SW-REIN                     ECS020
00998              MOVE LOW-VALUES       TO SW-RPT-CD-1                 ECS020
00999              MOVE AM-REPORT-CODE-2 TO SW-RPT-CD-2                 ECS020
unix               MOVE SORT-RECORD      TO WS-SAVE-SORT-REC
01000              RELEASE SORT-RECORD                                  ECS020
unix               MOVE WS-SAVE-SORT-REC TO SORT-RECORD
               END-IF
           END-IF
01001                                                                   ECS020
01002                                                                   ECS020
01003      GO TO 0210-READ-EXTRACTS.                                    ECS020
01004                                                                   ECS020
01005  0299-EXIT.                                                       ECS020
01006      EXIT.                                                        ECS020
01007                                                                   ECS020
01008  EJECT                                                            ECS020
01009  0300-PRINT-EPL SECTION.                                          ECS020
01010                                                                   ECS020
unix       DISPLAY ' 0300 PRINT-EPL'
01011      OPEN OUTPUT PRINTER                                          ECS020
01012                  LOSS-RATIOS.                                     ECS020
01013                                                                   ECS020
01014      PERFORM 0390-INITIALIZE-ACCUMS THRU 0390-EXIT.               ECS020
01015                                                                   ECS020
01016      PERFORM 0383-RETURN-EXTRACTS.                                ECS020
01017                                                                   ECS020
           DISPLAY 'SW-CONTROL= ' SW-CONTROL
01018      IF SW-CONTROL = HIGH-VALUES                                  ECS020
01019          DISPLAY ' NO EXTRACT RECORDS'                            ECS020
01020          MOVE '0701' TO WS-RETURN-CODE                            ECS020
01021          GO TO ABEND-PGM.                                         ECS020
01022                                                                   ECS020
01023      MOVE SW-REIN                   TO SV-REIN                    ECS020
01024                                        H1-SUFFIX.                 ECS020
01025      MOVE SW-CONTROL                TO SV-CONTROL.                ECS020
01026      MOVE SW-REI-CO                 TO SV-REI-CO.                 ECS020
01027      MOVE SW-ACCT-FLAG              TO SV-ACCT-FLAG.              ECS020
JJPMOD     MOVE SW-REIN-NAME              TO SV-X-REIN-NAME.            ECS020
01028                                                                   ECS020
01029      GO TO 0330-CHECK-EOF.                                        ECS020
01030                                                                   ECS020
01031  0320-CHECK-CONTROL.                                              ECS020
01032      IF SW-REIN NOT = SV-REIN                                     ECS020
01033          PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    ECS020
01034          PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    ECS020
01035          PERFORM 1500-STATE-PRT THRU 1549-EXIT                    ECS020
01036          PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                    ECS020
01037          PERFORM 1600-COMP-PRT  THRU 1699-EXIT                    ECS020
01038          PERFORM 1700-CARR-PRT  THRU 1749-EXIT                    ECS020
01039          PERFORM 1750-REIN-PRT  THRU 1759-EXIT                    ECS020
01040          PERFORM 1770-RPT1-PRT  THRU 1799-EXIT                    ECS020
01041          PERFORM 1800-FINAL-PRT THRU 1899-EXIT                    ECS020
01042          PERFORM 0390-INITIALIZE-ACCUMS THRU 0390-EXIT            ECS020
01043          MOVE SW-REIN TO H1-SUFFIX                                ECS020
01044          MOVE +0  TO PAGER                                        ECS020
01045          MOVE +99 TO LINER                                        ECS020
CIDMOD                                                                  ECS020
CIDMOD**    SKIP 1 ADDITIONAL PAGE BETWEEN THE ECS020A, ECS020B         ECS020
CIDMOD**    AND ECS020C REPORTS.                                        ECS020
CIDMOD                                                                  ECS020
CIDMOD         IF SW-REIN = 'B' OR 'C'                                  ECS020
CIDMOD            WRITE PRT FROM SPACE-LINE AFTER ADVANCING PAGE        ECS020
CIDMOD         END-IF                                                   ECS020
CIDMOD         GO TO 0330-CHECK-EOF                                     ECS020
CIDMOD     END-IF.                                                      ECS020
01047                                                                   ECS020
01048      IF SW-RPT-CD-1 NOT = SV-RPT-CD-1                             ECS020
01049          PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    ECS020
01050          PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    ECS020
01051          PERFORM 1500-STATE-PRT THRU 1549-EXIT                    ECS020
01052          PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                    ECS020
01053          PERFORM 1600-COMP-PRT  THRU 1699-EXIT                    ECS020
01054          PERFORM 1700-CARR-PRT  THRU 1749-EXIT                    ECS020
01055          PERFORM 1750-REIN-PRT  THRU 1759-EXIT                    ECS020
01056          PERFORM 1770-RPT1-PRT  THRU 1799-EXIT                    ECS020
01057          GO TO 0330-CHECK-EOF.                                    ECS020
01058                                                                   ECS020
JJPMOD     IF SW-REIN  =  'X'                                           ECS020
JJPMOD         IF SW-REIN-NAME NOT = SV-X-REIN-NAME                     ECS020
JJPMOD             PERFORM 1300-DATE-PRT  THRU 1399-EXIT                ECS020
JJPMOD             PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                ECS020
JJPMOD             PERFORM 1500-STATE-PRT THRU 1549-EXIT                ECS020
JJPMOD             PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                ECS020
JJPMOD             PERFORM 1600-COMP-PRT  THRU 1699-EXIT                ECS020
JJPMOD             PERFORM 1700-CARR-PRT  THRU 1749-EXIT                ECS020
JJPMOD             PERFORM 1750-REIN-PRT  THRU 1759-EXIT                ECS020
JJPMOD             GO TO 0330-CHECK-EOF                                 ECS020
JJPMOD         END-IF                                                   ECS020
JJPMOD     ELSE                                                         ECS020
JJPMOD         IF SW-REINCO NOT = SV-REINCO                             ECS020
JJPMOD             PERFORM 1300-DATE-PRT  THRU 1399-EXIT                ECS020
JJPMOD             PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                ECS020
JJPMOD             PERFORM 1500-STATE-PRT THRU 1549-EXIT                ECS020
JJPMOD             PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                ECS020
JJPMOD             PERFORM 1600-COMP-PRT  THRU 1699-EXIT                ECS020
JJPMOD             PERFORM 1700-CARR-PRT  THRU 1749-EXIT                ECS020
JJPMOD             PERFORM 1750-REIN-PRT  THRU 1759-EXIT                ECS020
JJPMOD             GO TO 0330-CHECK-EOF                                 ECS020
JJPMOD         END-IF                                                   ECS020
JJPMOD     END-IF.                                                      ECS020
JJPMOD                                                                  ECS020
01059 **   IF SW-REINCO NOT = SV-REINCO                                 ECS020
01060 **       PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    ECS020
01061 **       PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    ECS020
01062 **       PERFORM 1500-STATE-PRT THRU 1549-EXIT                    ECS020
01063 **       PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                    ECS020
01064 **       PERFORM 1600-COMP-PRT  THRU 1699-EXIT                    ECS020
01065 **       PERFORM 1700-CARR-PRT  THRU 1749-EXIT                    ECS020
01066 **       PERFORM 1750-REIN-PRT  THRU 1759-EXIT                    ECS020
01067 **       GO TO 0330-CHECK-EOF.                                    ECS020
01068                                                                   ECS020
01069      IF SW-CARRIER NOT = SV-CARRIER                               ECS020
01070          PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    ECS020
01071          PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    ECS020
01072          PERFORM 1500-STATE-PRT THRU 1549-EXIT                    ECS020
01073          PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                    ECS020
01074          PERFORM 1600-COMP-PRT  THRU 1699-EXIT                    ECS020
01075          PERFORM 1700-CARR-PRT  THRU 1749-EXIT                    ECS020
01076          GO TO 0330-CHECK-EOF.                                    ECS020
01077                                                                   ECS020
01078      IF SW-GROUPING NOT = SV-GROUPING                             ECS020
01079          PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    ECS020
01080          PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    ECS020
01081          PERFORM 1500-STATE-PRT THRU 1549-EXIT                    ECS020
01082          PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                    ECS020
01083          PERFORM 1600-COMP-PRT  THRU 1699-EXIT                    ECS020
01084          GO TO 0330-CHECK-EOF.                                    ECS020
01085                                                                   ECS020
01086      IF SW-RPT-CD-2 NOT = SV-RPT-CD-2                             ECS020
01087          PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    ECS020
01088          PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    ECS020
01089          PERFORM 1500-STATE-PRT THRU 1549-EXIT                    ECS020
01090          PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                    ECS020
01091          GO TO 0330-CHECK-EOF.                                    ECS020
01092                                                                   ECS020
01093      IF SW-STATE NOT = SV-STATE                                   ECS020
01094          PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    ECS020
01095          PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    ECS020
01096          PERFORM 1500-STATE-PRT THRU 1549-EXIT                    ECS020
01097          GO TO 0330-CHECK-EOF.                                    ECS020
01098                                                                   ECS020
01099      IF SW-ACCOUNT NOT = SV-ACCOUNT                               ECS020
01100          PERFORM 1300-DATE-PRT THRU 1399-EXIT                     ECS020
01101          PERFORM 1400-ACCT-PRT THRU 1499-EXIT                     ECS020
01102          GO TO 0330-CHECK-EOF.                                    ECS020
01103                                                                   ECS020
01104      IF SW-EXP-DATE   NOT = SV-EXP-DTE  OR                        ECS020
01105         SW-EFF-DATE   NOT = SV-EFF-DTE  OR                        ECS020
01106         SW-REINCO-SUB NOT = SV-REINCO-SUB                         ECS020
01107            PERFORM 1300-DATE-PRT THRU 1399-EXIT.                  ECS020
01108                                                                   ECS020
01109  0330-CHECK-EOF.                                                  ECS020
01110      IF SW-CONTROL = HIGH-VALUES                                  ECS020
01111          GO TO 2999-EXIT.                                         ECS020
01112                                                                   ECS020
01113      MOVE SW-EP-RECORD                TO EP-RECORD.               ECS020
01114                                                                   ECS020
01115      COPY ELCEPCM1.                                               ECS020
01116                                                                   ECS020
01117      IF EP-REIN NOT = 'R'                                         ECS020
01118          MOVE SPACES TO H3-REIN                                   ECS020
01119                         H3-REIN-COMP                              ECS020
01120                         H3-REIN-NAME.                             ECS020
01121                                                                   ECS020
01122      MOVE DATE-TOTS   TO WORK-TOT-1.                              ECS020
01123      MOVE EP-REIN     TO KL-REIN.                                 ECS020
01124      MOVE EP-RCD-TYPE TO KL-LF-AH.                                ECS020
01125                                                                   ECS020
unix       IF EP-BEN-CODE NOT = ZERO AND SPACES AND LOW-VALUES
                  AND HIGH-VALUES                                       ECS020
01127          MOVE EP-BEN-CODE TO KL-BEN-TYP                           ECS020
01128      ELSE                                                         ECS020
01129          DISPLAY 'ZERO BENEFIT TYPE INVALID ON EPEC FILE '        ECS020
01130                        EP-CONTROL                                 ECS020
01131          MOVE '0301' TO WS-RETURN-CODE                            ECS020
01132          GO TO ABEND-PGM.                                         ECS020
01133                                                                   ECS020
01134      MOVE +1 TO X1.                                               ECS020
01135                                                                   ECS020
01136  0340-FIND-WT1-KEY.                                               ECS020
01137      IF WT1-KEY (X1) = SPACES   OR                                ECS020
01138         KEY-LOOK     = WT1-KEY (X1)                               ECS020
01139              MOVE WT1-ACCUMS (X1) TO WORK-ACCUMS-1                ECS020
01140              MOVE KEY-LOOK        TO WT1-KEY (X1)                 ECS020
01141      ELSE                                                         ECS020
01142          ADD +1 TO X1                                             ECS020
01143          IF X1 NOT GREATER THAN TABLE-LIMIT                       ECS020
01144              GO TO 0340-FIND-WT1-KEY                              ECS020
01145          ELSE                                                     ECS020
01146              DISPLAY 'DETAIL ACCUMULATOR TABLE FULL '             ECS020
01147                        EP-CONTROL                                 ECS020
01148              MOVE '0202' TO WS-RETURN-CODE                        ECS020
01149              GO TO ABEND-PGM.                                     ECS020
01150                                                                   ECS020
01151      PERFORM 1200-FIND-BEN-CODE THRU 1299-EXIT.                   ECS020
01152                                                                   ECS020
01153      MOVE CLAS-STARTS TO CLAS-INDEXS.                             ECS020
01154                                                                   ECS020
01155  0350-STATE-LOOKUP.                                               ECS020
01156      IF EP-STATE NOT = STATE-SUB (CLAS-INDEXS)                    ECS020
01157          IF CLAS-INDEXS NOT = CLAS-MAXS                           ECS020
01158              ADD +1 TO CLAS-INDEXS                                ECS020
01159              GO TO 0350-STATE-LOOKUP.                             ECS020
01160                                                                   ECS020
01161      IF DTE-CLIENT = 'FIM'  AND                                   ECS020
01162         EP-REIN    = 'R'    AND                                   ECS020
01163         EP-REI-CO NOT = WS-LAST-REIN-CO                           ECS020
01164          MOVE +1 TO WS-RCT-X                                      ECS020
01165      ELSE                                                         ECS020
01166          GO TO 0380-ACCUMULATE-TOTALS.                            ECS020
01167                                                                   ECS020
01168  0360-FIND-REIN-IBNR-PCT.                                         ECS020
01169      IF WS-RCT-X GREATER THAN WS-RCT-MAX OR                       ECS020
01170         WS-RCT-COMP (WS-RCT-X) = HIGH-VALUES                      ECS020
01171          MOVE ZEROS TO FAC-3  FAC-4                               ECS020
01172          GO TO 0380-ACCUMULATE-TOTALS.                            ECS020
01173                                                                   ECS020
01174      IF EP-REI-CO NOT = WS-RCT-COMP (WS-RCT-X)                    ECS020
01175          ADD +1 TO WS-RCT-X                                       ECS020
01176          GO TO 0360-FIND-REIN-IBNR-PCT.                           ECS020
01177                                                                   ECS020
01178      MOVE WS-LF-IBNR (WS-RCT-X) TO FAC-3.                         ECS020
01179      MOVE WS-AH-IBNR (WS-RCT-X) TO FAC-4.                         ECS020
01180                                                                   ECS020
01181      MOVE WS-LF-PRO (WS-RCT-X)  TO LF-PRO.                        ECS020
01182      MOVE WS-LF-R78 (WS-RCT-X)  TO LF-R78.                        ECS020
01183      MOVE WS-AH-PRO (WS-RCT-X)  TO AH-PRO.                        ECS020
01184      MOVE WS-AH-R78 (WS-RCT-X)  TO AH-R78.                        ECS020
01185                                                                   ECS020
01186      MOVE EP-REI-CO TO WS-LAST-REIN-CO.                           ECS020
01187                                                                   ECS020
01188  0380-ACCUMULATE-TOTALS.                                          ECS020
01189                                                                   ECS020
01190      IF EP-PRM-78-ADJ NOT NUMERIC                                 ECS020
01191          MOVE ZEROS             TO EP-PRM-78-ADJ.                 ECS020
01192      IF EP-PRM-PR-ADJ NOT NUMERIC                                 ECS020
01193          MOVE ZEROS             TO EP-PRM-PR-ADJ.                 ECS020
01194      IF EP-PRM-ST-ADJ NOT NUMERIC                                 ECS020
01195          MOVE ZEROS             TO EP-PRM-ST-ADJ.                 ECS020
01196                                                                   ECS020
01197      COMPUTE TT-PRM-78 = EP-PRM-78 + EP-PRM-78-ADJ.               ECS020
01198      COMPUTE TT-PRM-PR = EP-PRM-PR + EP-PRM-PR-ADJ.               ECS020
01199      COMPUTE TT-PRM-ST = EP-PRM-ST + EP-PRM-ST-ADJ.               ECS020
01200                                                                   ECS020
01201      IF EP-PURGE = 'P'                                            ECS020
01202          PERFORM 1900-ITD-ADD THRU 1999-EXIT.                     ECS020
01203                                                                   ECS020
01204      IF EP-PURGE = ('A' OR 'Q')                                   ECS020
01205          PERFORM 1900-ITD-ADD THRU 1999-EXIT                      ECS020
01206          PERFORM 2000-YTD-ADD THRU 2099-EXIT.                     ECS020
01207                                                                   ECS020
01208      IF EP-PURGE = 'S'                                            ECS020
01209          PERFORM 2100-YTD-SUB THRU 2199-EXIT.                     ECS020
01210                                                                   ECS020
01211      MOVE WORK-ACCUMS-1 TO WT1-ACCUMS (X1).                       ECS020
01212      MOVE WORK-TOT-1    TO DATE-TOTS.                             ECS020
01213                                                                   ECS020
01214      MOVE SW-REIN                      TO SV-REIN.                ECS020
01215      MOVE SW-CONTROL                   TO SV-CONTROL.             ECS020
01216      MOVE SW-REI-CO                    TO SV-REI-CO.              ECS020
01217      MOVE SW-ACCT-FLAG                 TO SV-ACCT-FLAG.           ECS020
JJPMOD     MOVE SW-REIN-NAME                 TO SV-X-REIN-NAME.         ECS020
01218                                                                   ECS020
01219  0383-RETURN-EXTRACTS.                                            ECS020
01220      RETURN SORT-WORK AT END                                      ECS020
unix           DISPLAY ' RETURN AT END '
01221          MOVE HIGH-VALUES TO SORT-RECORD                          ECS020
           END-RETURN
      *     IF SW-RCD-TYPE = LOW-VALUES OR SPACES OR
      *           ZEROS
      *        DISPLAY 'BAD RCD TYPE ' SW-CONTROL
      *        GO TO 0383-RETURN-EXTRACTS
      *     END-IF
           IF SW-BEN-CODE = LOW-VALUES OR HIGH-VALUES OR SPACES
                OR ZEROS
           IF SW-BEN-CODE = SPACES
              DISPLAY 'SW-BEN-CODE IS SPACES'
           END-IF
           IF SW-BEN-CODE = HIGH-VALUES
              DISPLAY 'SW-BEN-CODE IS HV'
           END-IF
           IF SW-BEN-CODE = LOW-VALUES
              DISPLAY 'SW-BEN-CODE IS LV'
           END-IF
              DISPLAY 'BAD BEN CODE ' SW-CONTROL
           END-IF
01222      .                                                            ECS020
01223  0389-RETURN-TO-CHECK.                                            ECS020
01224      GO TO 0320-CHECK-CONTROL.                                    ECS020
01225                                                                   ECS020
01226  0390-INITIALIZE-ACCUMS.                                          ECS020
01227      MOVE WORK-TOT-0                TO DATE-TOTS                  ECS020
01228                                        ACCT-TOTS                  ECS020
01229                                        ST-TOTS                    ECS020
01230                                        RPT2-TOTS                  ECS020
01231                                        COMP-TOTS                  ECS020
01232                                        CARR-TOTS                  ECS020
01233                                        REIN-TOTS                  ECS020
01234                                        RPT1-TOTS                  ECS020
01235                                        FINL-TOTS                  ECS020
01236                                        WORK-TOT-1                 ECS020
01237                                        WORK-TOT-2.                ECS020
01238                                                                   ECS020
01239      MOVE WORK-ACCUMS-0             TO WORK-ACCUMS-1              ECS020
01240                                        WORK-ACCUMS-2              ECS020
01241                                        BEN-TOTALS                 ECS020
01242                                        TYP-TOTALS.                ECS020
01243      MOVE ZEROS                     TO WRK-RESV.                  ECS020
01244                                                                   ECS020
01245  0390-EXIT.                                                       ECS020
01246      EXIT.                                                        ECS020
01247  EJECT                                                            ECS020
01248  0400-PRINT-RTN.                                                  ECS020
01249      MOVE LOSS-RATIO-INITIALIZED   TO LOSS-RATIO-MASTER.          ECS020
01250                                                                   ECS020
01251      MOVE +1            TO X1.                                    ECS020
01252      MOVE 'X'           TO BENEFIT-PRT-SW.                        ECS020
01253      MOVE WT1-LF-AH (1) TO SAVE-PR-LF-AH.                         ECS020
01254                                                                   ECS020
01255  0410-PR-LOOP.                                                    ECS020
01256      IF WT1-KEY (X1) = SPACES                                     ECS020
01257          PERFORM 1000-PR-BEN-TOT THRU 1099-EXIT                   ECS020
01258          PERFORM 1100-PR-TYP-TOT THRU 1199-EXIT                   ECS020
01259          GO TO 0499-EXIT.                                         ECS020
01260                                                                   ECS020
01261      IF WT1-LF-AH (X1) NOT = SAVE-PR-LF-AH                        ECS020
01262          PERFORM 1000-PR-BEN-TOT THRU 1099-EXIT.                  ECS020
01263                                                                   ECS020
01264      MOVE WT1-LF-AH  (X1) TO SAVE-PR-LF-AH.                       ECS020
01265      MOVE WT1-ACCUMS (X1) TO WORK-ACCUMS-1.                       ECS020
01266                                                                   ECS020
01267      PERFORM 0900-CHECK-ZEROS THRU 0999-EXIT.                     ECS020
01268                                                                   ECS020
01269      IF SV-ACCT-FLAG = 'X'                                        ECS020
01270          IF SV-REIN = 'R'                                         ECS020
01271 **           PERFORM 2200-FIND-REIN-NAME THRU 2299-EXIT           ECS020
JJPMOD             PERFORM 2200-FIND-REIN-NAME THRU 2230-EXIT           ECS020
01272              GO TO 0420-SKIP-PRINT                                ECS020
01273            ELSE                                                   ECS020
01274              GO TO 0420-SKIP-PRINT.                               ECS020
01275                                                                   ECS020
01276      IF ALL-AMOUNTS-ZERO                                          ECS020
01277          GO TO 0420-SKIP-PRINT.                                   ECS020
01278                                                                   ECS020
01279      IF  LINER GREATER THAN +57                                   ECS020
01280          MOVE 'X'                TO BENEFIT-PRT-SW                ECS020
01281                                                                   ECS020
01282          IF  W-TOTALS-BEING-PROCESSED                             ECS020
01283              PERFORM 2350-HDR-RTN-T THRU 2350-EXIT                ECS020
01284              PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT      ECS020
01285                                                                   ECS020
01286          ELSE                                                     ECS020
01287              PERFORM 2300-HDR-RTN-A THRU 2300-EXIT.               ECS020
01288                                                                   ECS020
01289      IF  PRINTING-1ST-BENEFIT                                     ECS020
01290          MOVE SPACE              TO BENEFIT-PRT-SW                ECS020
01291                                                                   ECS020
01292          IF  W-TOTALS-BEING-PROCESSED                             ECS020
01293              PERFORM 2420-HDR-RTN-T THRU 2420-EXIT                ECS020
01294                                                                   ECS020
01295          ELSE                                                     ECS020
01296              PERFORM 2400-HDR-RTN-B THRU 2400-EXIT.               ECS020
01297                                                                   ECS020
01298      ADD +1 TO PR-BEN-CTR                                         ECS020
01299                PR-TYP-CTR.                                        ECS020
01300                                                                   ECS020
01301      PERFORM 1200-FIND-BEN-CODE THRU 1299-EXIT.                   ECS020
01302                                                                   ECS020
01303      IF WT1-LF-AH (X1) = L1                                       ECS020
01304          MOVE CLAS-I-AB10 (CLAS-INDEXL) TO D1-DESC                ECS020
01305      ELSE                                                         ECS020
01306          MOVE CLAS-I-AB10 (CLAS-INDEXA) TO D1-DESC.               ECS020
01307                                                                   ECS020
01308      PERFORM 0600-SET-UP-DTL-PRT THRU 0699-EXIT.                  ECS020
01309                                                                   ECS020
01310      MOVE ' ' TO X.                                               ECS020
01311      ADD +1 TO LINER.                                             ECS020
01312                                                                   ECS020
01313      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
01314                                                                   ECS020
01315  0420-SKIP-PRINT.                                                 ECS020
01316      MOVE BEN-TOTALS TO WORK-ACCUMS-2.                            ECS020
01317                                                                   ECS020
01318      PERFORM 0500-ADD-RTN THRU 0599-EXIT.                         ECS020
01319                                                                   ECS020
01320      MOVE WORK-ACCUMS-2 TO BEN-TOTALS.                            ECS020
01321      MOVE TYP-TOTALS TO WORK-ACCUMS-2.                            ECS020
01322                                                                   ECS020
01323      PERFORM 0500-ADD-RTN THRU 0599-EXIT.                         ECS020
01324                                                                   ECS020
01325      MOVE WORK-ACCUMS-2 TO TYP-TOTALS.                            ECS020
01326      ADD +1 TO X1.                                                ECS020
01327                                                                   ECS020
01328      GO TO 0410-PR-LOOP.                                          ECS020
01329                                                                   ECS020
01330  0499-EXIT.                                                       ECS020
01331      EXIT.                                                        ECS020
01332                                                                   ECS020
01333  0500-ADD-RTN.                                                    ECS020
01334      ADD WA1-Y-CERT TO WA2-Y-CERT.                                ECS020
01335      ADD WA1-Y-NET  TO WA2-Y-NET.                                 ECS020
01336      ADD WA1-Y-EARN TO WA2-Y-EARN.                                ECS020
01337      ADD WA1-Y-PAID TO WA2-Y-PAID.                                ECS020
01338      ADD WA1-Y-RESV TO WA2-Y-RESV.                                ECS020
01339      ADD WA1-I-CERT TO WA2-I-CERT.                                ECS020
01340      ADD WA1-I-NET  TO WA2-I-NET.                                 ECS020
01341      ADD WA1-I-EARN TO WA2-I-EARN.                                ECS020
01342      ADD WA1-I-PAID TO WA2-I-PAID.                                ECS020
01343      ADD WA1-I-RESV TO WA2-I-RESV.                                ECS020
01344                                                                   ECS020
01345  0599-EXIT.                                                       ECS020
01346      EXIT.                                                        ECS020
01347  EJECT                                                            ECS020
01348  0600-SET-UP-DTL-PRT.                                             ECS020
01349      IF WA1-Y-EARN NOT = ZERO                                     ECS020
01350          IF DTE-CLIENT = 'FMK' OR 'FLI' OR 'FLU'                  ECS020
01351              COMPUTE WS-Y-LOSS ROUNDED = WA1-Y-PAID / WA1-Y-EARN  ECS020
01352          ELSE                                                     ECS020
01353              COMPUTE WS-Y-LOSS ROUNDED =                          ECS020
01354                       (WA1-Y-PAID + WA1-Y-RESV) / WA1-Y-EARN      ECS020
01355      ELSE                                                         ECS020
01356          MOVE ZEROS TO WS-Y-LOSS.                                 ECS020
01357                                                                   ECS020
01358      IF WA1-I-EARN NOT = ZERO                                     ECS020
01359          IF DTE-CLIENT = 'FMK' OR 'FLI' OR 'FLU'                  ECS020
01360              COMPUTE WS-I-LOSS ROUNDED = WA1-I-PAID / WA1-I-EARN  ECS020
01361          ELSE                                                     ECS020
01362              COMPUTE WS-I-LOSS ROUNDED =                          ECS020
01363                       (WA1-I-PAID + WA1-I-RESV) / WA1-I-EARN      ECS020
01364      ELSE                                                         ECS020
01365          MOVE ZEROS TO WS-I-LOSS.                                 ECS020
01366                                                                   ECS020
01367      IF DTE-FMT-OPT = '2'                                         ECS020
01368          GO TO 0620-PRT-OPTION-2.                                 ECS020
01369                                                                   ECS020
01370      IF DTE-FMT-OPT = '3'                                         ECS020
01371          GO TO 0630-PRT-OPTION-3.                                 ECS020
01372                                                                   ECS020
01373      IF DTE-FMT-OPT = '4'                                         ECS020
01374          GO TO 0640-PRT-OPTION-4.                                 ECS020
01375                                                                   ECS020
01376  0610-PRT-OPTION-1.                                               ECS020
01377      MOVE WA1-Y-CERT TO D1A-Y-CERTS.                              ECS020
01378      MOVE WA1-Y-NET  TO D1A-Y-NET.                                ECS020
01379      MOVE WA1-Y-EARN TO D1A-Y-EARNED.                             ECS020
01380      MOVE WA1-Y-PAID TO D1A-Y-PAID.                               ECS020
01381      MOVE WA1-Y-RESV TO D1A-Y-RESERVE.                            ECS020
01382                                                                   ECS020
01383      IF WS-Y-LOSS GREATER +9.999 OR LESS -9.999                   ECS020
01384          MOVE '******' TO D1A-Y-LOSS-OV                           ECS020
01385          MOVE 'Y'      TO LOSS-RATIO-SW                           ECS020
01386      ELSE                                                         ECS020
01387          MULTIPLY WS-Y-LOSS BY +100 GIVING D1A-Y-LOSS.            ECS020
01388                                                                   ECS020
01389      MOVE WA1-I-CERT TO D1A-I-CERTS.                              ECS020
01390      MOVE WA1-I-NET  TO D1A-I-NET.                                ECS020
01391      MOVE WA1-I-EARN TO D1A-I-EARNED.                             ECS020
01392      MOVE WA1-I-PAID TO D1A-I-PAID.                               ECS020
01393      MOVE WA1-I-RESV TO D1A-I-RESERVE.                            ECS020
01394                                                                   ECS020
01395      IF WS-I-LOSS GREATER +9.999 OR LESS -9.999                   ECS020
01396          MOVE '******' TO D1A-I-LOSS-OV                           ECS020
01397          MOVE 'Y'      TO LOSS-RATIO-SW                           ECS020
01398      ELSE                                                         ECS020
01399          MULTIPLY WS-I-LOSS BY +100 GIVING D1A-I-LOSS.            ECS020
01400                                                                   ECS020
01401      MOVE DTL-1A TO PRT.                                          ECS020
01402                                                                   ECS020
01403      GO TO 0699-EXIT.                                             ECS020
01404                                                                   ECS020
01405  0620-PRT-OPTION-2.                                               ECS020
01406      MOVE WA1-Y-NET  TO D1B-Y-NET.                                ECS020
01407      MOVE WA1-Y-EARN TO D1B-Y-EARNED.                             ECS020
01408      MOVE WA1-Y-PAID TO D1B-Y-PAID.                               ECS020
01409      MOVE WA1-Y-RESV TO D1B-Y-RESERVE.                            ECS020
01410                                                                   ECS020
01411      IF WS-Y-LOSS GREATER +9.999 OR LESS -9.999                   ECS020
01412          MOVE '******' TO D1B-Y-LOSS-OV                           ECS020
01413          MOVE 'Y'      TO LOSS-RATIO-SW                           ECS020
01414      ELSE                                                         ECS020
01415          MULTIPLY WS-Y-LOSS BY +100 GIVING D1B-Y-LOSS.            ECS020
01416                                                                   ECS020
01417      MOVE WA1-I-NET  TO D1B-I-NET.                                ECS020
01418      MOVE WA1-I-EARN TO D1B-I-EARNED.                             ECS020
01419      MOVE WA1-I-PAID TO D1B-I-PAID.                               ECS020
01420      MOVE WA1-I-RESV TO D1B-I-RESERVE.                            ECS020
01421                                                                   ECS020
01422      IF WS-I-LOSS GREATER +9.999 OR LESS -9.999                   ECS020
01423          MOVE '******' TO D1B-I-LOSS-OV                           ECS020
01424          MOVE 'Y'      TO LOSS-RATIO-SW                           ECS020
01425      ELSE                                                         ECS020
01426          MULTIPLY WS-I-LOSS BY +100 GIVING D1B-I-LOSS.            ECS020
01427                                                                   ECS020
01428      MOVE DTL-1A TO PRT.                                          ECS020
01429                                                                   ECS020
01430      GO TO 0699-EXIT.                                             ECS020
01431                                                                   ECS020
01432  0630-PRT-OPTION-3.                                               ECS020
01433      MOVE WA1-Y-CERT TO D1C-Y-CERTS.                              ECS020
01434      MOVE WA1-Y-NET  TO D1C-Y-NET.                                ECS020
01435      MOVE WA1-Y-EARN TO D1C-Y-EARNED.                             ECS020
01436      MOVE WA1-Y-PAID TO D1C-Y-PAID.                               ECS020
01437                                                                   ECS020
01438      IF WS-Y-LOSS GREATER +9.999 OR LESS -9.999                   ECS020
01439          MOVE '******' TO D1C-Y-LOSS-OV                           ECS020
01440          MOVE 'Y'      TO LOSS-RATIO-SW                           ECS020
01441      ELSE                                                         ECS020
01442          MULTIPLY WS-Y-LOSS BY +100 GIVING D1C-Y-LOSS.            ECS020
01443                                                                   ECS020
01444      MOVE WA1-I-CERT TO D1C-I-CERTS.                              ECS020
01445      MOVE WA1-I-NET  TO D1C-I-NET.                                ECS020
01446      MOVE WA1-I-EARN TO D1C-I-EARNED.                             ECS020
01447      MOVE WA1-I-PAID TO D1C-I-PAID.                               ECS020
01448                                                                   ECS020
01449      IF WS-I-LOSS GREATER +9.999 OR LESS -9.999                   ECS020
01450          MOVE '******' TO D1C-I-LOSS-OV                           ECS020
01451          MOVE 'Y'      TO LOSS-RATIO-SW                           ECS020
01452      ELSE                                                         ECS020
01453          MULTIPLY WS-I-LOSS BY +100 GIVING D1C-I-LOSS.            ECS020
01454                                                                   ECS020
01455      MOVE DTL-1A TO PRT.                                          ECS020
01456                                                                   ECS020
01457      GO TO 0699-EXIT.                                             ECS020
01458                                                                   ECS020
01459  0640-PRT-OPTION-4.                                               ECS020
01460      MOVE WA1-Y-NET  TO D1D-Y-NET.                                ECS020
01461      MOVE WA1-Y-EARN TO D1D-Y-EARNED.                             ECS020
01462      MOVE WA1-Y-PAID TO D1D-Y-PAID.                               ECS020
01463      MOVE WA1-Y-RESV TO D1D-Y-RESERVE.                            ECS020
01464                                                                   ECS020
01465      IF WS-Y-LOSS GREATER +9.999 OR LESS -9.999                   ECS020
01466          MOVE '****' TO D1D-Y-LOSS-OV                             ECS020
01467      ELSE                                                         ECS020
01468          MULTIPLY WS-Y-LOSS BY +100 GIVING D1D-Y-LOSS.            ECS020
01469                                                                   ECS020
01470      MOVE WA1-I-NET  TO D1D-I-NET.                                ECS020
01471      MOVE WA1-I-EARN TO D1D-I-EARNED.                             ECS020
01472      MOVE WA1-I-PAID TO D1D-I-PAID.                               ECS020
01473      MOVE WA1-I-RESV TO D1D-I-RESERVE.                            ECS020
01474                                                                   ECS020
01475      IF WS-I-LOSS GREATER +9.999 OR LESS -9.999                   ECS020
01476          MOVE '****' TO D1D-I-LOSS-OV                             ECS020
01477      ELSE                                                         ECS020
01478          MULTIPLY WS-I-LOSS BY +100 GIVING D1D-I-LOSS.            ECS020
01479                                                                   ECS020
01480      SUBTRACT WA1-I-EARN FROM WA1-I-NET GIVING D1D-UNEARNED.      ECS020
01481                                                                   ECS020
01482      MOVE DTL-1A TO PRT.                                          ECS020
01483                                                                   ECS020
01484  0699-EXIT.                                                       ECS020
01485      EXIT.                                                        ECS020
01486  EJECT                                                            ECS020
01487  0700-ACCUM-RTN.                                                  ECS020
01488      MOVE +1     TO X1.                                           ECS020
01489                                                                   ECS020
01490  0710-ACCUM-RTN-LOOP-1.                                           ECS020
01491      IF WT1-KEY (X1) = SPACES                                     ECS020
01492          GO TO 0799-EXIT.                                         ECS020
01493                                                                   ECS020
01494      MOVE +1 TO X2.                                               ECS020
01495                                                                   ECS020
01496  0720-ACCUM-RTN-LOOP-2.                                           ECS020
01497      IF WT1-KEY (X1) = WT2-KEY (X2)  OR                           ECS020
01498         WT2-KEY (X2) = SPACES                                     ECS020
01499              MOVE WT1-ACCUMS (X1) TO WORK-ACCUMS-1                ECS020
01500              MOVE WT2-ACCUMS (X2) TO WORK-ACCUMS-2                ECS020
01501              PERFORM 0500-ADD-RTN THRU 0599-EXIT                  ECS020
01502              MOVE WORK-ACCUMS-2   TO WT2-ACCUMS (X2)              ECS020
01503              MOVE WT1-KEY (X1)    TO WT2-KEY (X2)                 ECS020
01504              ADD +1 TO X1                                         ECS020
01505              GO TO 0710-ACCUM-RTN-LOOP-1.                         ECS020
01506                                                                   ECS020
01507      IF WT1-KEY (X1) LESS THAN WT2-KEY (X2)                       ECS020
01508          PERFORM 0800-SLIDE-ACCUMS THRU 0899-EXIT                 ECS020
01509          GO TO 0720-ACCUM-RTN-LOOP-2.                             ECS020
01510                                                                   ECS020
01511      ADD +1 TO X2.                                                ECS020
01512                                                                   ECS020
01513      IF X2 NOT GREATER THAN TABLE-LIMIT                           ECS020
01514          GO TO 0720-ACCUM-RTN-LOOP-2                              ECS020
01515      ELSE                                                         ECS020
01516          DISPLAY 'TOTAL ACCUMULATOR TABLE FULL '                  ECS020
01517                        SAVE-CONTROL                               ECS020
01518          MOVE '0203' TO WS-RETURN-CODE                            ECS020
01519          GO TO ABEND-PGM.                                         ECS020
01520                                                                   ECS020
01521  0799-EXIT.                                                       ECS020
01522      EXIT.                                                        ECS020
01523  EJECT                                                            ECS020
01524  0800-SLIDE-ACCUMS.                                               ECS020
01525      IF WT2-KEY (TABLE-LIMIT) NOT = SPACES                        ECS020
01526          DISPLAY 'TOTAL ACCUMULATOR TABLE FULL '                  ECS020
01527                        SAVE-CONTROL                               ECS020
01528          MOVE '0203' TO WS-RETURN-CODE                            ECS020
01529          GO TO ABEND-PGM.                                         ECS020
01530                                                                   ECS020
01531      MOVE TABLE-LIMIT TO X3.                                      ECS020
01532                                                                   ECS020
01533  0810-SLIDE-ACCUMS-LOOP-1.                                        ECS020
01534      IF WT2-KEY (X3) = SPACES                                     ECS020
01535          SUBTRACT +1 FROM X3                                      ECS020
01536          GO TO 0810-SLIDE-ACCUMS-LOOP-1.                          ECS020
01537                                                                   ECS020
01538      ADD X3 +1 GIVING X4.                                         ECS020
01539                                                                   ECS020
01540  0820-SLIDE-ACCUMS-LOOP-2.                                        ECS020
01541      MOVE WT2 (X3) TO WT2 (X4).                                   ECS020
01542                                                                   ECS020
01543      IF X3 GREATER THAN X2                                        ECS020
01544          SUBTRACT +1 FROM X3                                      ECS020
01545          SUBTRACT +1 FROM X4                                      ECS020
01546          GO TO 0820-SLIDE-ACCUMS-LOOP-2.                          ECS020
01547                                                                   ECS020
01548      MOVE SPACES        TO WT2-KEY (X2).                          ECS020
01549      MOVE WORK-ACCUMS-0 TO WT2-ACCUMS (X2).                       ECS020
01550                                                                   ECS020
01551  0899-EXIT.                                                       ECS020
01552      EXIT.                                                        ECS020
01553                                                                   ECS020
01554  0900-CHECK-ZEROS.                                                ECS020
01555      MOVE ' ' TO ZERO-FLAG.                                       ECS020
01556                                                                   ECS020
01557      IF WA1-Y-NET  NOT = ZERO  OR                                 ECS020
01558         WA1-Y-EARN NOT = ZERO  OR                                 ECS020
01559         WA1-Y-PAID NOT = ZERO  OR                                 ECS020
01560         WA1-I-NET  NOT = ZERO  OR                                 ECS020
01561         WA1-I-EARN NOT = ZERO  OR                                 ECS020
01562         WA1-I-PAID NOT = ZERO                                     ECS020
01563              MOVE 'X' TO ZERO-FLAG                                ECS020
01564              GO TO 0999-EXIT.                                     ECS020
01565                                                                   ECS020
01566      IF DTE-FMT-OPT = '1' OR '3'                                  ECS020
01567          IF WA1-Y-CERT NOT = ZERO  OR                             ECS020
01568             WA1-I-CERT NOT = ZERO                                 ECS020
01569                  MOVE 'X' TO ZERO-FLAG                            ECS020
01570                  GO TO 0999-EXIT.                                 ECS020
01571                                                                   ECS020
01572      IF DTE-FMT-OPT = '1' OR '2' OR '4'                           ECS020
01573          IF WA1-Y-RESV NOT = ZERO  OR                             ECS020
01574             WA1-I-RESV NOT = ZERO                                 ECS020
01575                  MOVE 'X' TO ZERO-FLAG                            ECS020
01576                  GO TO 0999-EXIT.                                 ECS020
01577                                                                   ECS020
01578  0999-EXIT.                                                       ECS020
01579      EXIT.                                                        ECS020
01580  EJECT                                                            ECS020
01581  1000-PR-BEN-TOT.                                                 ECS020
01582      MOVE '  TOT'              TO D1-DESC1.                       ECS020
01583                                                                   ECS020
01584      IF SAVE-PR-LF-AH = L1                                        ECS020
01585          MOVE +2               TO X5                              ECS020
01586          MOVE LIFE-OVERRIDE-L2 TO D1-DESC2                        ECS020
01587      ELSE                                                         ECS020
01588          MOVE +3               TO X5                              ECS020
01589          MOVE AH-OVERRIDE-L2   TO D1-DESC2.                       ECS020
01590                                                                   ECS020
01591      MOVE BEN-TOTALS           TO WORK-ACCUMS-1.                  ECS020
01592      PERFORM 0600-SET-UP-DTL-PRT THRU 0699-EXIT.                  ECS020
01593                                                                   ECS020
01594      IF PR-BEN-CTR GREATER THAN +1                                ECS020
01595          MOVE ' ' TO X                                            ECS020
01596          PERFORM 2500-PRT-RTN THRU 2599-EXIT.                     ECS020
01597                                                                   ECS020
01598      MOVE WA1-Y-NET     TO LR-YTD-NET (X5).                       ECS020
01599      MOVE WA1-Y-EARN    TO LR-YTD-EARN (X5).                      ECS020
01600      MOVE WA1-Y-PAID    TO LR-YTD-PAID (X5).                      ECS020
01601      MOVE WA1-Y-RESV    TO LR-YTD-RESV (X5).                      ECS020
01602      COMPUTE LR-YTD-INCUR (X5) = WA1-Y-PAID + WA1-Y-RESV.         ECS020
01603                                                                   ECS020
01604      MOVE WA1-I-NET     TO LR-ITD-NET (X5).                       ECS020
01605      MOVE WA1-I-EARN    TO LR-ITD-EARN (X5).                      ECS020
01606      MOVE WA1-I-PAID    TO LR-ITD-PAID (X5).                      ECS020
01607      MOVE WA1-I-RESV    TO LR-ITD-RESV (X5).                      ECS020
01608      COMPUTE LR-ITD-INCUR (X5) = WA1-I-PAID + WA1-I-RESV.         ECS020
01609                                                                   ECS020
01610      MOVE ZERO          TO PR-BEN-CTR.                            ECS020
01611      MOVE WORK-ACCUMS-0 TO BEN-TOTALS.                            ECS020
01612      ADD +1 TO LINER.                                             ECS020
01613                                                                   ECS020
01614  1099-EXIT.                                                       ECS020
01615      EXIT.                                                        ECS020
01616                                                                   ECS020
01617  1100-PR-TYP-TOT.                                                 ECS020
01618      MOVE '    TOTAL' TO D1-DESC.                                 ECS020
01619                                                                   ECS020
01620      MOVE +1                   TO X5.                             ECS020
01621                                                                   ECS020
01622      MOVE TYP-TOTALS           TO WORK-ACCUMS-1.                  ECS020
01623      PERFORM 0600-SET-UP-DTL-PRT THRU 0699-EXIT.                  ECS020
01624                                                                   ECS020
01625      IF PR-TYP-CTR GREATER THAN +1                                ECS020
01626          MOVE ' ' TO X                                            ECS020
01627          PERFORM 2500-PRT-RTN THRU 2599-EXIT.                     ECS020
01628                                                                   ECS020
01629      MOVE WA1-Y-NET     TO LR-YTD-NET (X5).                       ECS020
01630      MOVE WA1-Y-EARN    TO LR-YTD-EARN (X5).                      ECS020
01631      MOVE WA1-Y-PAID    TO LR-YTD-PAID (X5).                      ECS020
01632      MOVE WA1-Y-RESV    TO LR-YTD-RESV (X5).                      ECS020
01633      COMPUTE LR-YTD-INCUR (X5) = WA1-Y-PAID + WA1-Y-RESV.         ECS020
01634                                                                   ECS020
01635      MOVE WA1-I-NET     TO LR-ITD-NET (X5).                       ECS020
01636      MOVE WA1-I-EARN    TO LR-ITD-EARN (X5).                      ECS020
01637      MOVE WA1-I-PAID    TO LR-ITD-PAID (X5).                      ECS020
01638      MOVE WA1-I-RESV    TO LR-ITD-RESV (X5).                      ECS020
01639      COMPUTE LR-ITD-INCUR (X5) = WA1-I-PAID + WA1-I-RESV.         ECS020
01640                                                                   ECS020
01641      MOVE ZERO          TO PR-TYP-CTR.                            ECS020
01642      MOVE WORK-ACCUMS-0 TO TYP-TOTALS.                            ECS020
01643      ADD +1 TO LINER.                                             ECS020
01644                                                                   ECS020
01645  1199-EXIT.                                                       ECS020
01646      EXIT.                                                        ECS020
01647                                                                   ECS020
01648  1200-FIND-BEN-CODE.                                              ECS020
01649      MOVE CLAS-STARTL TO CLAS-INDEXL.                             ECS020
01650                                                                   ECS020
01651      IF WT1-LF-AH (X1) = 'Z' OR 'A'                               ECS020
01652          GO TO 1230-SET-FIND-AH-TYPE.                             ECS020
01653                                                                   ECS020
01654      IF CLAS-STARTL = ZEROS                                       ECS020
01655          GO TO 1220-LIFE-NOT-TABLED.                              ECS020
01656                                                                   ECS020
01657  1210-FIND-LF-TYPE.                                               ECS020
01658      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        ECS020
01659          GO TO 1220-LIFE-NOT-TABLED.                              ECS020
01660                                                                   ECS020
01661      IF WT1-BEN-TYP (X1) = CLAS-I-BEN (CLAS-INDEXL)               ECS020
01662          GO TO 1299-EXIT.                                         ECS020
01663                                                                   ECS020
01664      ADD +1 TO CLAS-INDEXL.                                       ECS020
01665                                                                   ECS020
01666      GO TO 1210-FIND-LF-TYPE.                                     ECS020
01667                                                                   ECS020
01668  1220-LIFE-NOT-TABLED.                                            ECS020
unix       DISPLAY 'MAXL= ' CLAS-MAXL
unix       DISPLAY 'X1= ' X1
unix       DISPLAY 'INDEXL= ' CLAS-INDEXL
unix       DISPLAY 'CONTROL= ' SW-CONTROL
unix       DISPLAY 'ECONTROL= ' EP-CONTROL
01669      DISPLAY 'LIFE/AH REC CODE'                                   ECS020
01670                        WT1-LF-AH (X1).                            ECS020
01671      DISPLAY 'LIFE BENEFIT TYPE NOT IN TABLE - ('                 ECS020
01672                        WT1-BEN-TYP (X1) ')'.                      ECS020
01673                                                                   ECS020
01674      MOVE '0401' TO WS-RETURN-CODE.                               ECS020
01675                                                                   ECS020
01676      GO TO ABEND-PGM.                                             ECS020
01677                                                                   ECS020
01678  1230-SET-FIND-AH-TYPE.                                           ECS020
01679      MOVE CLAS-STARTA TO CLAS-INDEXA.                             ECS020
01680                                                                   ECS020
01681      IF CLAS-STARTA = ZEROS                                       ECS020
01682          GO TO 1250-AH-NOT-TABLED.                                ECS020
01683                                                                   ECS020
01684  1240-FIND-AH-TYPE.                                               ECS020
01685      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        ECS020
01686          GO TO 1250-AH-NOT-TABLED.                                ECS020
01687                                                                   ECS020
01688      IF WT1-BEN-TYP (X1) = CLAS-I-BEN (CLAS-INDEXA)               ECS020
01689          GO TO 1299-EXIT.                                         ECS020
01690                                                                   ECS020
01691      ADD +1 TO CLAS-INDEXA.                                       ECS020
01692                                                                   ECS020
01693      GO TO 1240-FIND-AH-TYPE.                                     ECS020
01694                                                                   ECS020
01695  1250-AH-NOT-TABLED.                                              ECS020
01696      DISPLAY 'LIFE/AH REC CODE'                                   ECS020
01697                        WT1-LF-AH (X1).                            ECS020
01698      DISPLAY 'A&H BENEFIT TYPE NOT IN TABLE - ('                  ECS020
01699                        WT1-BEN-TYP (X1) ')'.                      ECS020
01700                                                                   ECS020
01701      MOVE '0402' TO WS-RETURN-CODE.                               ECS020
01702                                                                   ECS020
01703      GO TO ABEND-PGM.                                             ECS020
01704                                                                   ECS020
01705  1299-EXIT.                                                       ECS020
01706      EXIT.                                                        ECS020
01707  EJECT                                                            ECS020
01708  1300-DATE-PRT.                                                   ECS020
01709      MOVE SPACES TO HDR-4A.                                       ECS020
01710                                                                   ECS020
01711      MOVE DATE-TOTS TO WORK-TOT-1.                                ECS020
01712      MOVE ACCT-TOTS TO WORK-TOT-2.                                ECS020
01713                                                                   ECS020
01714      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       ECS020
01715                                                                   ECS020
01716      MOVE WORK-TOT-0 TO DATE-TOTS.                                ECS020
01717      MOVE WORK-TOT-2 TO ACCT-TOTS.                                ECS020
01718                                                                   ECS020
01719      IF DTE-PGM-OPT = '1'  OR  '2'                                ECS020
CIDMOD*        IF EP-CNTRL-1 NOT = AM-CONTROL-A                         ECS020
CIDMOD         IF EP-CONTROL NOT = AM-MSTR-CNTRL                        ECS020
01721              MOVE DTE-CLASIC-COMPANY-CD    TO AM-COMPANY-CD       ECS020
01722              MOVE EP-CONTROL               TO AM-MSTR-CNTRL       ECS020
01723              PERFORM 2700-READ-AM-MSTR THRU 2700-EXIT             ECS020
01724              MOVE AM-NAME                  TO SV-NAME             ECS020
01725              MOVE AM-CITY                  TO SV-CITY.            ECS020
01726                                                                   ECS020
01727      IF (SV-REIN = 'B'  OR  'C'  OR  'G')  OR                     ECS020
01728         DTE-PGM-OPT NOT = 1                                       ECS020
01729          GO TO 1399-EXIT.                                         ECS020
01730                                                                   ECS020
JJPMOD     IF SV-REIN = 'X'                                             ECS020
JJPMOD         GO TO 1399-EXIT                                          ECS020
JJPMOD     END-IF.                                                      ECS020
JJPMOD                                                                  ECS020
01731      MOVE ' CARR'                     TO H4A-CARRIER.             ECS020
01732      MOVE ' GRP '                     TO H4A-GROUP.               ECS020
01733      MOVE '  ST'                      TO H4A-STATE.               ECS020
01734      MOVE '    ACCT  '                TO H4A-ACCOUNT.             ECS020
01735      MOVE 'EFF DT'                    TO H4A-EFFECTIVE.           ECS020
01736                                                                   ECS020
01737      MOVE EP-CARRIER                  TO H4A-CARR.                ECS020
01738      MOVE EP-GROUPING                 TO H4A-GRP.                 ECS020
01739      MOVE EP-STATE                    TO H4A-ST.                  ECS020
01740      MOVE EP-ACCOUNT                  TO H4A-ACCT.                ECS020
01741      MOVE SV-NAME                     TO H4A-AM-NAME.             ECS020
01742      MOVE SV-CITY                     TO H4A-CTY-ST.              ECS020
01743      MOVE EP-EFF-MO                   TO WS-MO.                   ECS020
01744      MOVE EP-EFF-DA                   TO WS-DA.                   ECS020
01745      MOVE EP-EFF-YR                   TO WS-YR.                   ECS020
01746      MOVE WS-DATE                     TO H4A-EFF-DTE.             ECS020
01747                                                                   ECS020
01748      IF SV-REIN = 'R'                                             ECS020
01749          MOVE SV-REINCO-SUB      TO H4A-SUB                       ECS020
01750          MOVE HDR-4A-SUB-DESC    TO H4A-CTY-ST.                   ECS020
01751                                                                   ECS020
01752      IF DATE-PRT-CTR = ZERO                                       ECS020
01753          IF EP-CARRIER  NOT = SW-CARRIER  OR                      ECS020
01754             EP-GROUPING NOT = SW-GROUPING OR                      ECS020
01755             EP-STATE    NOT = SW-STATE    OR                      ECS020
01756             EP-ACCOUNT  NOT = SW-ACCOUNT                          ECS020
01757              MOVE '  ****** SUMMARY FOR' TO H4A-SUMMARY           ECS020
01758              MOVE SV-CITY        TO H4A-CTY-ST                    ECS020
01759              MOVE SPACES         TO H4A-EFFECTIVE                 ECS020
01760                                     H4A-EFF-DTE.                  ECS020
01761                                                                   ECS020
01762      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       ECS020
01763                                                                   ECS020
01764      ADD +1 TO DATE-PRT-CTR.                                      ECS020
01765                                                                   ECS020
01766  1399-EXIT.                                                       ECS020
01767      EXIT.                                                        ECS020
01768                                                                   ECS020
01769  1400-ACCT-PRT.                                                   ECS020
01770      MOVE ACCT-TOTS TO WORK-TOT-1.                                ECS020
01771      MOVE ST-TOTS   TO WORK-TOT-2.                                ECS020
01772                                                                   ECS020
01773      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       ECS020
01774                                                                   ECS020
01775      MOVE WORK-TOT-0 TO ACCT-TOTS.                                ECS020
01776      MOVE WORK-TOT-2 TO ST-TOTS.                                  ECS020
01777                                                                   ECS020
JJPMOD     IF SV-REIN = 'X'                                             ECS020
JJPMOD         GO TO 1499-EXIT                                          ECS020
JJPMOD     END-IF.                                                      ECS020
JJPMOD                                                                  ECS020
01778      IF DTE-PGM-OPT = 2  OR                                       ECS020
01779         (SV-REIN  = 'B'  OR  'C'  OR  'G')                        ECS020
01780          MOVE +2 TO DATE-PRT-CTR                                  ECS020
01781      ELSE                                                         ECS020
01782          IF DTE-PGM-OPT NOT = 1                                   ECS020
01783              GO TO 1499-EXIT.                                     ECS020
01784                                                                   ECS020
01785      IF WS-ACCT-OV-SW = 'X'                                       ECS020
01786          MOVE +99  TO LINER.                                      ECS020
01787                                                                   ECS020
01788      IF DATE-PRT-CTR GREATER THAN +1                              ECS020
CIDMOD         IF  LINER GREATER THAN +55                               ECS020
CIDMOD             MOVE 'X'                TO BENEFIT-PRT-SW            ECS020
CIDMOD             PERFORM 2300-HDR-RTN-A THRU 2300-EXIT                ECS020
CIDMOD         END-IF
01789          MOVE SPACES TO HDR-4A                                    ECS020
01790          MOVE '  ****** SUMMARY FOR' TO H4A-SUMMARY               ECS020
01791          MOVE ' CARR'                TO H4A-CARRIER               ECS020
01792          MOVE ' GRP '                TO H4A-GROUP                 ECS020
01793          MOVE '  ST'                 TO H4A-STATE                 ECS020
01794          MOVE '    ACCT  '           TO H4A-ACCOUNT               ECS020
01795          MOVE SV-CARRIER             TO H4A-CARR                  ECS020
01796          MOVE SV-GROUPING            TO H4A-GRP                   ECS020
01797          MOVE SV-STATE               TO H4A-ST                    ECS020
01798          MOVE SV-ACCOUNT             TO H4A-ACCT                  ECS020
01799          MOVE SV-NAME                TO H4A-AM-NAME               ECS020
01800          MOVE SV-CITY                TO H4A-CTY-ST                ECS020
01801          PERFORM 0400-PRINT-RTN THRU 0499-EXIT                    ECS020
CIDMOD     END-IF.                                                      ECS020
01802                                                                   ECS020
01803      MOVE +0 TO DATE-PRT-CTR.                                     ECS020
01804                                                                   ECS020
01805      MOVE SV-REIN              TO LR-RCD-TYPE.                    ECS020
01806      MOVE SV-REINCO            TO LR-REIN-CO                      ECS020
01807      MOVE SV-RPT-CD-1          TO LR-RPT-CD-1.                    ECS020
01808      MOVE SV-CARRIER           TO LR-CARRIER.                     ECS020
01809      MOVE SV-GROUPING          TO LR-GROUPING.                    ECS020
01810      MOVE SV-RPT-CD-2          TO LR-GA-RPT-CD-2.                 ECS020
01811      MOVE SV-STATE             TO LR-STATE.                       ECS020
01812      MOVE SV-ACCOUNT           TO LR-ACCOUNT.                     ECS020
01813 *    MOVE SV-REINCO-SUB        TO LR-REIN-SUB.                    ECS020
01814                                                                   ECS020
01815      MOVE SV-NAME              TO LR-ACCT-NAME.                   ECS020
01816      MOVE SV-REIN-NAME         TO LR-REIN-NAME.                   ECS020
01817                                                                   ECS020
01818      PERFORM 2800-WRITE-LOSS-RATIO-MASTER THRU 2899-EXIT.         ECS020
01819                                                                   ECS020
01820  1499-EXIT.                                                       ECS020
01821      EXIT.                                                        ECS020
01822                                                                   ECS020
01823  1500-STATE-PRT.                                                  ECS020
01824                                                                   ECS020
01825      MOVE ST-TOTS   TO WORK-TOT-1.                                ECS020
01826      MOVE RPT2-TOTS TO WORK-TOT-2.                                ECS020
01827                                                                   ECS020
01828      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       ECS020
01829                                                                   ECS020
01830      MOVE WORK-TOT-0 TO ST-TOTS.                                  ECS020
01831      MOVE WORK-TOT-2 TO RPT2-TOTS.                                ECS020
01832                                                                   ECS020
01833      IF SV-REIN = 'B'  OR  'C'  OR  'G'                           ECS020
01834          GO TO 1549-EXIT                                          ECS020
01835      ELSE                                                         ECS020
01836          IF DTE-PGM-OPT NOT = 1 AND 2 AND 3                       ECS020
01837              GO TO 1549-EXIT.                                     ECS020
01838                                                                   ECS020
JJPMOD     IF SV-REIN = 'X'                                             ECS020
JJPMOD         GO TO 1549-EXIT                                          ECS020
JJPMOD     END-IF.                                                      ECS020
JJPMOD                                                                  ECS020
01839      IF CLAS-STARTS = ZERO                                        ECS020
01840          MOVE SPACES TO HDR-4A                                    ECS020
01841          GO TO 1520-GET-ST-DESC-1.                                ECS020
01842                                                                   ECS020
01843      MOVE CLAS-STARTS TO CLAS-INDEXS.                             ECS020
01844                                                                   ECS020
01845  1510-GET-ST-DESC.                                                ECS020
01846      IF SV-STATE NOT = STATE-SUB (CLAS-INDEXS)                    ECS020
01847          IF CLAS-INDEXS NOT = CLAS-MAXS                           ECS020
01848              ADD +1 TO CLAS-INDEXS                                ECS020
01849              GO TO 1510-GET-ST-DESC.                              ECS020
01850                                                                   ECS020
01851      MOVE SPACES                 TO W-TOTAL-WORK-AREA.            ECS020
01852                                                                   ECS020
01853      MOVE STATE-PIC (CLAS-INDEXS) TO W-TW-1-NAME.                 ECS020
01854                                                                   ECS020
01855  1520-GET-ST-DESC-1.                                              ECS020
01856      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       ECS020
01857                                                                   ECS020
01858      MOVE 'IN CARRIER'           TO W-TW-3-TITLE.                 ECS020
01859      MOVE 'IN GROUP'             TO W-TW-2-TITLE.                 ECS020
01860      MOVE 'STATE'                TO W-TW-1-TITLE.                 ECS020
01861      MOVE SV-CARRIER             TO W-TW-3-CODE.                  ECS020
01862      MOVE SV-GROUPING            TO W-TW-2-CODE.                  ECS020
01863      MOVE SV-STATE               TO W-TW-1-CODE.                  ECS020
01864      MOVE ')'                    TO W-TW-1-RP.                    ECS020
01865      MOVE '('                    TO W-TW-1-LP.                    ECS020
01866                                                                   ECS020
01867      IF  SV-REIN = 'R'                                            ECS020
01868 **       PERFORM 2200-FIND-REIN-NAME THRU 2299-EXIT               ECS020
JJPMOD         PERFORM 2200-FIND-REIN-NAME THRU 2230-EXIT               ECS020
01869          MOVE 'IN REIN COMP'     TO W-TW-4-TITLE                  ECS020
01870          MOVE SV-REIN-NAME       TO W-TW-4-NAME                   ECS020
01871          MOVE SV-REINCO          TO W-TW-4-CODE                   ECS020
01872          MOVE ')'                TO W-TW-4-RP                     ECS020
01873          MOVE '('                TO W-TW-4-LP.                    ECS020
01874 *                                                                 ECS020
01875 *    ELSE                                                         ECS020
01876 *        IF  SV-REIN = 'C'                                        ECS020
01877 *            MOVE 'IN RPT CODE2' TO W-TW-4-TITLE                  ECS020
01878 *            MOVE SV-RPT-CD-2    TO W-TW-4-CODE                   ECS020
01879 *                                                                 ECS020
01880 *        ELSE                                                     ECS020
01881 *            IF  SV-REIN = 'G'                                    ECS020
01882 *                MOVE 'FOR GEN AGENT'                             ECS020
01883 *                                TO W-TW-4-TITLE                  ECS020
01884 *                MOVE SV-RPT-CD-2                                 ECS020
01885 *                                TO W-TW-4-CODE                   ECS020
01886 *                                                                 ECS020
01887 *            ELSE                                                 ECS020
01888 *                IF  SV-REIN = 'B'                                ECS020
01889 *                    MOVE 'IN RPT CODE1'                          ECS020
01890 *                                TO W-TW-4-TITLE                  ECS020
01891 *                    MOVE SV-RPT-CD-1                             ECS020
01892 *                                TO W-TW-4-CODE.                  ECS020
01893                                                                   ECS020
01894      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   ECS020
01895                                                                   ECS020
01896      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             ECS020
01897                                                                   ECS020
01898      MOVE 'Y'                    TO W-TOTAL-SW.                   ECS020
01899      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       ECS020
01900      MOVE +99                    TO LINER.                        ECS020
01901      MOVE SPACE                  TO W-TOTAL-SW.                   ECS020
01902                                                                   ECS020
01903      IF SV-REIN = 'A'                                             ECS020
01904          MOVE 'S'                TO LR-RCD-TYPE                   ECS020
01905          MOVE SV-STATE           TO LR-STATE                      ECS020
01906          MOVE STATE-PIC (CLAS-INDEXS)                             ECS020
01907                                  TO LR-ACCT-NAME                  ECS020
01908          PERFORM 2800-WRITE-LOSS-RATIO-MASTER THRU 2899-EXIT.     ECS020
01909                                                                   ECS020
01910  1549-EXIT.                                                       ECS020
01911      EXIT.                                                        ECS020
01912                                                                   ECS020
01913  1550-RPT2-PRT.                                                   ECS020
01914      MOVE RPT2-TOTS TO WORK-TOT-1.                                ECS020
01915      MOVE COMP-TOTS TO WORK-TOT-2.                                ECS020
01916                                                                   ECS020
01917      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       ECS020
01918                                                                   ECS020
01919      MOVE WORK-TOT-0 TO RPT2-TOTS.                                ECS020
01920      MOVE WORK-TOT-2 TO COMP-TOTS.                                ECS020
01921                                                                   ECS020
01922      IF SV-REIN = 'C'  OR  'G'                                    ECS020
01923          NEXT SENTENCE                                            ECS020
01924      ELSE                                                         ECS020
01925          GO TO 1599-EXIT.                                         ECS020
01926                                                                   ECS020
01927      MOVE SPACES TO W-TOTAL-WORK-AREA.                            ECS020
01928      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       ECS020
01929                                                                   ECS020
01930      MOVE 'IN CARRIER'           TO W-TW-3-TITLE.                 ECS020
01931 *    MOVE 'IN CARRIER'           TO W-TW-2-TITLE.                 ECS020
01932      MOVE 'IN GROUP'             TO W-TW-2-TITLE.                 ECS020
01933 *    MOVE 'GROUP'                TO W-TW-1-TITLE.                 ECS020
01934      MOVE 'AGENT'                TO W-TW-1-TITLE.                 ECS020
01935 *    MOVE SV-CARRIER             TO W-TW-2-CODE.                  ECS020
01936 *    MOVE SV-GROUPING            TO W-TW-1-CODE.                  ECS020
01937      MOVE SV-CARRIER             TO W-TW-3-CODE.                  ECS020
01938      MOVE SV-GROUPING            TO W-TW-2-CODE.                  ECS020
01939      MOVE SV-RPT-CD-2            TO W-TW-1-CODE.                  ECS020
01940                                                                   ECS020
01941      IF  SV-REIN = 'C'                                            ECS020
01942          MOVE 'IN RPT CODE2'     TO W-TW-4-TITLE                  ECS020
01943          MOVE SV-RPT-CD-2        TO W-TW-4-CODE                   ECS020
01944                                                                   ECS020
01945      ELSE                                                         ECS020
01946          IF  SV-REIN = 'G'                                        ECS020
01947              MOVE 'FOR GEN AGENT'                                 ECS020
01948                                  TO W-TW-4-TITLE                  ECS020
01949              MOVE SV-RPT-CD-2    TO W-TW-4-CODE.                  ECS020
01950                                                                   ECS020
01951      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   ECS020
01952                                                                   ECS020
01953      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             ECS020
01954                                                                   ECS020
01955      MOVE 'Y'                    TO W-TOTAL-SW.                   ECS020
01956      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       ECS020
01957      MOVE SPACE                  TO W-TOTAL-SW.                   ECS020
01958                                                                   ECS020
01959      IF DTE-CLIENT = 'AN1' OR 'ANT' OR 'DDB' OR 'BWS' OR          ECS020
01960                      'GSL' OR 'NIS' OR 'TFS' OR 'UW1'             ECS020
01961          MOVE ' '              TO PRT                             ECS020
01962          MOVE '1'              TO X                               ECS020
01963          PERFORM 2500-PRT-RTN THRU 2599-EXIT.                     ECS020
01964                                                                   ECS020
01965      MOVE +99                  TO LINER.                          ECS020
01966                                                                   ECS020
01967      MOVE SV-REIN              TO LR-RCD-TYPE.                    ECS020
01968      MOVE SV-CARRIER           TO LR-CARRIER.                     ECS020
01969      MOVE SV-GROUPING          TO LR-GROUPING.                    ECS020
01970      MOVE SV-RPT-CD-2          TO LR-GA-RPT-CD-2.                 ECS020
01971                                                                   ECS020
01972      PERFORM 2800-WRITE-LOSS-RATIO-MASTER THRU 2899-EXIT.         ECS020
01973                                                                   ECS020
CIDMOD     IF H1-SUFFIX = 'G' OR 'R'                                    ECS020
CIDMOD         GO TO 1599-EXIT.                                         ECS020
CIDMOD                                                                  ECS020
CIDMOD* SKIP 1 ADDITIONAL PAGE TO GET NEW ACCT TO PRINT ON SEP. PAGE.   ECS020
CIDMOD* PRINT IS SENT TO AGENTS. ONLY PAPER RPT NEEDS EXTRA SKIP.       ECS020
CIDMOD                                                                  ECS020
CIDMOD     WRITE PRT FROM SPACE-LINE AFTER ADVANCING PAGE.              ECS020
CIDMOD                                                                  ECS020
01974  1599-EXIT.                                                       ECS020
01975      EXIT.                                                        ECS020
01976                                                                   ECS020
01977  1600-COMP-PRT.                                                   ECS020
01978      MOVE COMP-TOTS TO WORK-TOT-1.                                ECS020
01979      MOVE CARR-TOTS TO WORK-TOT-2.                                ECS020
01980                                                                   ECS020
01981      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       ECS020
01982                                                                   ECS020
01983      MOVE WORK-TOT-0 TO COMP-TOTS.                                ECS020
01984      MOVE WORK-TOT-2 TO CARR-TOTS.                                ECS020
01985                                                                   ECS020
JJPMOD     IF SV-REIN = 'X'                                             ECS020
JJPMOD         GO TO 1699-EXIT                                          ECS020
JJPMOD     END-IF.                                                      ECS020
JJPMOD                                                                  ECS020
01986      IF SV-REIN = 'C'  OR  'G'                                    ECS020
01987          NEXT SENTENCE                                            ECS020
01988      ELSE                                                         ECS020
01989          IF SV-REIN = 'B'  OR                                     ECS020
01990            (DTE-PGM-OPT NOT = 1 AND 2 AND 3 AND 4                 ECS020
01991                           AND 5 AND 6 AND 7 AND 8)                ECS020
01992              GO TO 1699-EXIT.                                     ECS020
01993                                                                   ECS020
01994      MOVE 'X'                    TO G-A-PRT-SW.                   ECS020
01995      MOVE SPACES                 TO W-TOTAL-WORK-AREA.            ECS020
01996      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       ECS020
01997                                                                   ECS020
01998      MOVE 'IN CARRIER'           TO W-TW-2-TITLE.                 ECS020
01999      MOVE 'GROUP'                TO W-TW-1-TITLE.                 ECS020
02000      MOVE SV-CARRIER             TO W-TW-2-CODE.                  ECS020
02001      MOVE SV-GROUPING            TO W-TW-1-CODE.                  ECS020
02002                                                                   ECS020
02003      IF  SV-REIN = 'R'                                            ECS020
02004 **       PERFORM 2200-FIND-REIN-NAME THRU 2299-EXIT               ECS020
JJPMOD         PERFORM 2200-FIND-REIN-NAME THRU 2230-EXIT               ECS020
02005          MOVE 'IN REIN COMP'     TO W-TW-4-TITLE                  ECS020
02006          MOVE SV-REIN-NAME       TO W-TW-4-NAME                   ECS020
02007          MOVE SV-REINCO          TO W-TW-4-CODE                   ECS020
02008          MOVE ')'                TO W-TW-4-RP                     ECS020
02009          MOVE '('                TO W-TW-4-LP                     ECS020
02010                                                                   ECS020
02011      ELSE                                                         ECS020
02012          IF  SV-REIN = 'C'                                        ECS020
02013              MOVE 'IN RPT CODE2' TO W-TW-4-TITLE                  ECS020
02014              MOVE SV-RPT-CD-2    TO W-TW-4-CODE                   ECS020
02015                                                                   ECS020
02016          ELSE                                                     ECS020
02017              IF  SV-REIN = 'G'                                    ECS020
02018                  MOVE 'FOR GEN AGENT'                             ECS020
02019                                  TO W-TW-4-TITLE                  ECS020
02020                  MOVE SV-RPT-CD-2                                 ECS020
02021                                  TO W-TW-4-CODE                   ECS020
02022                                                                   ECS020
02023              ELSE                                                 ECS020
02024                  IF  SV-REIN = 'B'                                ECS020
02025                      MOVE 'IN RPT CODE1'                          ECS020
02026                                  TO W-TW-4-TITLE                  ECS020
02027                      MOVE SV-RPT-CD-1                             ECS020
02028                                  TO W-TW-4-CODE.                  ECS020
02029                                                                   ECS020
02030      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   ECS020
02031                                                                   ECS020
02032      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             ECS020
02033                                                                   ECS020
02034      MOVE 'Y'                    TO W-TOTAL-SW.                   ECS020
02035      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       ECS020
02036      MOVE +99                    TO LINER.                        ECS020
02037      MOVE SPACE                  TO W-TOTAL-SW.                   ECS020
02038                                                                   ECS020
02039      MOVE SPACE                  TO G-A-PRT-SW.                   ECS020
02040                                                                   ECS020
02041  1699-EXIT.                                                       ECS020
02042      EXIT.                                                        ECS020
02043                                                                   ECS020
02044  1700-CARR-PRT.                                                   ECS020
02045      MOVE CARR-TOTS TO WORK-TOT-1.                                ECS020
02046      MOVE REIN-TOTS TO WORK-TOT-2.                                ECS020
02047                                                                   ECS020
02048      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       ECS020
02049                                                                   ECS020
02050      MOVE WORK-TOT-0 TO CARR-TOTS.                                ECS020
02051      MOVE WORK-TOT-2 TO REIN-TOTS.                                ECS020
02052                                                                   ECS020
JJPMOD     IF SV-REIN = 'X'                                             ECS020
JJPMOD         GO TO 1749-EXIT                                          ECS020
JJPMOD     END-IF.                                                      ECS020
JJPMOD                                                                  ECS020
02053      IF SV-REIN = 'C'  OR  'G'                                    ECS020
02054          NEXT SENTENCE                                            ECS020
02055      ELSE                                                         ECS020
02056          IF SV-REIN = 'B'  OR                                     ECS020
02057            (DTE-PGM-OPT NOT = 1 AND 2 AND 3 AND 4 AND 5)          ECS020
02058              GO TO 1749-EXIT.                                     ECS020
02059                                                                   ECS020
02060      MOVE 'X'                    TO G-A-PRT-SW.                   ECS020
02061      MOVE SPACES                 TO W-TOTAL-WORK-AREA.            ECS020
02062      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       ECS020
02063                                                                   ECS020
02064      MOVE 'CARRIER'              TO W-TW-1-TITLE.                 ECS020
02065      MOVE SV-CARRIER             TO W-TW-1-CODE.                  ECS020
02066                                                                   ECS020
02067      IF  SV-REIN = 'R'                                            ECS020
02068 **       PERFORM 2200-FIND-REIN-NAME THRU 2299-EXIT               ECS020
JJPMOD         PERFORM 2200-FIND-REIN-NAME THRU 2230-EXIT               ECS020
02069          MOVE 'IN REIN COMP'     TO W-TW-4-TITLE                  ECS020
02070          MOVE SV-REIN-NAME       TO W-TW-4-NAME                   ECS020
02071          MOVE SV-REINCO          TO W-TW-4-CODE                   ECS020
02072          MOVE ')'                TO W-TW-4-RP                     ECS020
02073          MOVE '('                TO W-TW-4-LP                     ECS020
02074                                                                   ECS020
02075      ELSE                                                         ECS020
02076          IF  SV-REIN = 'C'                                        ECS020
02077              MOVE 'IN RPT CODE2' TO W-TW-4-TITLE                  ECS020
02078              MOVE SV-RPT-CD-2    TO W-TW-4-CODE                   ECS020
02079                                                                   ECS020
02080          ELSE                                                     ECS020
02081              IF  SV-REIN = 'G'                                    ECS020
02082                  MOVE 'FOR GEN AGENT'                             ECS020
02083                                  TO W-TW-4-TITLE                  ECS020
02084                  MOVE SV-RPT-CD-2                                 ECS020
02085                                  TO W-TW-4-CODE                   ECS020
02086                                                                   ECS020
02087              ELSE                                                 ECS020
02088                  IF  SV-REIN = 'B'                                ECS020
02089                      MOVE 'IN RPT CODE1'                          ECS020
02090                                  TO W-TW-4-TITLE                  ECS020
02091                      MOVE SV-RPT-CD-1                             ECS020
02092                                  TO W-TW-4-CODE.                  ECS020
02093                                                                   ECS020
02094      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   ECS020
02095                                                                   ECS020
02096      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             ECS020
02097                                                                   ECS020
02098      MOVE 'Y'                    TO W-TOTAL-SW.                   ECS020
02099      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       ECS020
02100      MOVE +99                    TO LINER.                        ECS020
02101      MOVE SPACE                  TO W-TOTAL-SW.                   ECS020
02102                                                                   ECS020
02103      MOVE SPACE                  TO G-A-PRT-SW.                   ECS020
02104                                                                   ECS020
02105  1749-EXIT.                                                       ECS020
02106      EXIT.                                                        ECS020
02107                                                                   ECS020
02108  1750-REIN-PRT.                                                   ECS020
02109      MOVE REIN-TOTS TO WORK-TOT-1.                                ECS020
02110      MOVE RPT1-TOTS TO WORK-TOT-2.                                ECS020
02111                                                                   ECS020
02112      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       ECS020
02113                                                                   ECS020
02114      MOVE WORK-TOT-0 TO REIN-TOTS.                                ECS020
02115      MOVE WORK-TOT-2 TO RPT1-TOTS.                                ECS020
02116                                                                   ECS020
02117      IF SV-REIN = 'A'  OR  'B'  OR  'C'  OR  'G'                  ECS020
02118          GO TO 1759-EXIT                                          ECS020
02119      ELSE                                                         ECS020
02120          IF DTE-PGM-OPT NOT = 1 AND 2 AND 3 AND 4 AND 5           ECS020
02121              GO TO 1759-EXIT.                                     ECS020
02122                                                                   ECS020
02123      MOVE 'X'                TO REIN-PRT-SW.                      ECS020
02124      MOVE SPACES                 TO W-TOTAL-WORK-AREA.            ECS020
02125      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       ECS020
02126 *                                                                 ECS020
02127 *    MOVE 'REINSURANCE'          TO W-TW-1-NAME.                  ECS020
02128 *                                                                 ECS020
02129      IF  SV-REIN = 'R'                                            ECS020
02130 **       PERFORM 2200-FIND-REIN-NAME THRU 2299-EXIT               ECS020
JJPMOD         PERFORM 2200-FIND-REIN-NAME THRU 2230-EXIT               ECS020
02131          MOVE 'IN REIN COMP'     TO W-TW-4-TITLE                  ECS020
02132          MOVE SV-REIN-NAME       TO W-TW-4-NAME                   ECS020
02133          MOVE SV-REINCO          TO W-TW-4-CODE                   ECS020
02134          MOVE ')'                TO W-TW-4-RP                     ECS020
02135          MOVE '('                TO W-TW-4-LP.                    ECS020
02136                                                                   ECS020
JJPMOD     IF  SV-REIN = 'X'                                            ECS020
JJPMOD         MOVE 'IN REIN COMP'     TO W-TW-4-TITLE                  ECS020
JJPMOD         MOVE SV-X-REIN-NAME     TO W-TW-4-NAME                   ECS020
JJPMOD         MOVE SPACES             TO W-TW-4-CODE                   ECS020
JJPMOD         MOVE ')'                TO W-TW-4-RP                     ECS020
JJPMOD         MOVE '('                TO W-TW-4-LP.                    ECS020
JJPMOD                                                                  ECS020
02137      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   ECS020
02138                                                                   ECS020
02139      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             ECS020
02140                                                                   ECS020
02141      MOVE 'Y'                    TO W-TOTAL-SW.                   ECS020
02142      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       ECS020
02143      MOVE SPACE                  TO W-TOTAL-SW.                   ECS020
02144      MOVE +99                TO LINER.                            ECS020
02145                                                                   ECS020
02146      MOVE SPACE              TO REIN-PRT-SW.                      ECS020
02147                                                                   ECS020
JJPMOD     IF SV-REIN = 'X'                                             ECS020
JJPMOD         GO TO 1759-EXIT                                          ECS020
JJPMOD     END-IF.                                                      ECS020
JJPMOD                                                                  ECS020
02148      MOVE SV-REIN            TO LR-RCD-TYPE.                      ECS020
02149      MOVE SV-REINCO          TO LR-REIN-CO.                       ECS020
02150      MOVE SV-REIN-NAME       TO LR-REIN-NAME.                     ECS020
02151                                                                   ECS020
02152      PERFORM 2800-WRITE-LOSS-RATIO-MASTER THRU 2899-EXIT.         ECS020
02153                                                                   ECS020
02154  1759-EXIT.                                                       ECS020
02155      EXIT.                                                        ECS020
02156                                                                   ECS020
02157  1770-RPT1-PRT.                                                   ECS020
02158      MOVE RPT1-TOTS TO WORK-TOT-1.                                ECS020
02159      MOVE FINL-TOTS TO WORK-TOT-2.                                ECS020
02160                                                                   ECS020
02161      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       ECS020
02162                                                                   ECS020
02163      MOVE WORK-TOT-0 TO RPT1-TOTS.                                ECS020
02164      MOVE WORK-TOT-2 TO FINL-TOTS.                                ECS020
02165                                                                   ECS020
02166      IF SV-REIN NOT = 'B'                                         ECS020
02167          GO TO 1799-EXIT.                                         ECS020
02168                                                                   ECS020
02169      MOVE SPACES                 TO W-TOTAL-WORK-AREA.            ECS020
02170      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       ECS020
02171                                                                   ECS020
02172      MOVE 'RPT CODE1'            TO W-TW-1-TITLE                  ECS020
02173 **   MOVE SV-RPT-CD-1            TO W-TW-1-CODE.                  ECS020
CIDMOD     MOVE SV-RPT-CD-1            TO WK-SV-RPT-CD.                 ECS020
CIDMOD     MOVE WORK-SV-RPT-CD         TO W-TW-1-CODE.                  ECS020
02174                                                                   ECS020
02175      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   ECS020
02176                                                                   ECS020
02177      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             ECS020
02178                                                                   ECS020
02179      MOVE SPACES                   TO HDR-4A.                     ECS020
02180      MOVE '  ****** SUMMARY    '   TO H4A-SUMMARY.                ECS020
02181                                                                   ECS020
02182      MOVE 'Y'                    TO W-TOTAL-SW.                   ECS020
02183      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       ECS020
02184      MOVE +99                TO LINER.                            ECS020
02185      MOVE SPACE                  TO W-TOTAL-SW.                   ECS020
02186                                                                   ECS020
02187      MOVE SV-REIN              TO LR-RCD-TYPE.                    ECS020
02188      MOVE SV-RPT-CD-1          TO LR-RPT-CD-1.                    ECS020
02189                                                                   ECS020
02190      PERFORM 2800-WRITE-LOSS-RATIO-MASTER THRU 2899-EXIT.         ECS020
CIDMOD                                                                  ECS020
CIDMOD* SKIP 1 ADDITIONAL PAGE TO GET NEW ACCT TO PRINT ON SEP. PAGE.   ECS020
CIDMOD* PRINT IS SENT TO AGENTS. ONLY PAPER RPT NEEDS EXTRA SKIP.       ECS020
CIDMOD                                                                  ECS020
CIDMOD     WRITE PRT FROM SPACE-LINE AFTER ADVANCING PAGE.              ECS020
CIDMOD                                                                  ECS020
02191                                                                   ECS020
02192  1799-EXIT.                                                       ECS020
02193      EXIT.                                                        ECS020
02194                                                                   ECS020
02195  1800-FINAL-PRT.                                                  ECS020
02196      MOVE FINL-TOTS  TO WORK-TOT-1.                               ECS020
02197      MOVE WORK-TOT-0 TO FINL-TOTS.                                ECS020
02198                                                                   ECS020
JJPMOD     IF SV-REIN = 'X'                                             ECS020
JJPMOD         GO TO 1899-EXIT                                          ECS020
JJPMOD     END-IF.                                                      ECS020
JJPMOD                                                                  ECS020
02199      MOVE SPACE TO SV-REIN                                        ECS020
02200                    SV-ACCT-FLAG.                                  ECS020
02201                                                                   ECS020
02202      MOVE SPACES                 TO W-TOTAL-WORK-AREA.            ECS020
02203      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       ECS020
02204                                                                   ECS020
02205      MOVE 'REPORT'               TO W-TW-1-NAME                   ECS020
02206                                                                   ECS020
02207      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   ECS020
02208                                                                   ECS020
02209      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             ECS020
02210      MOVE '0'             TO X.                                   ECS020
02211                                                                   ECS020
02212      MOVE 'Y'                    TO W-TOTAL-SW.                   ECS020
02213      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       ECS020
02214      MOVE SPACE                  TO W-TOTAL-SW.                   ECS020
02215                                                                   ECS020
02216  1899-EXIT.                                                       ECS020
02217      EXIT.                                                        ECS020
02218  EJECT                                                            ECS020
02219  1900-ITD-ADD.                                                    ECS020
02220      COMPUTE WA1-I-CERT =                                         ECS020
02221          WA1-I-CERT + EP-ISS-CNT - EP-CNC-CNT.                    ECS020
02222                                                                   ECS020
02223      COMPUTE WA1-I-NET =                                          ECS020
02224          WA1-I-NET + EP-ISS-PRM - EP-CNC-PRM.                     ECS020
02225                                                                   ECS020
02226      ADD EP-CLM-AMT TO WA1-I-PAID.                                ECS020
02227                                                                   ECS020
02228      ADD EP-CLM-DU TO WA1-I-RESV.                                 ECS020
02229      IF EP-CLM-PV NUMERIC                                         ECS020
02230          ADD EP-CLM-PV TO WA1-I-RESV.                             ECS020
02231      ADD EP-LOSS-RESV TO WA1-I-RESV.                              ECS020
02232                                                                   ECS020
02233      IF DTE-CLIENT = 'FIM' OR                                     ECS020
02234        (DTE-CLIENT = 'FFL' AND EP-RCD-TYPE = L1)                  ECS020
02235          NEXT SENTENCE                                            ECS020
02236      ELSE                                                         ECS020
02237          ADD EP-CLM-IBNR TO WA1-I-RESV.                           ECS020
02238                                                                   ECS020
02239      IF EP-RCD-TYPE = L1                                          ECS020
02240          MOVE FAC-1 TO WS-FAC                                     ECS020
02241      ELSE                                                         ECS020
02242          MOVE FAC-2 TO WS-FAC.                                    ECS020
02243                                                                   ECS020
02244      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    ECS020
02245          IF EP-RCD-TYPE = L1                                      ECS020
02246              MOVE FAC-3          TO WS-FAC                        ECS020
02247              MOVE LF-PRO         TO WS-PRO                        ECS020
02248              MOVE LF-R78         TO WS-R78                        ECS020
02249          ELSE                                                     ECS020
02250              MOVE FAC-4          TO WS-FAC                        ECS020
02251              MOVE AH-PRO         TO WS-PRO                        ECS020
02252              MOVE AH-R78         TO WS-R78.                       ECS020
02253                                                                   ECS020
02254      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    ECS020
02255          IF WS-PRO = ZERO  AND  WS-R78 = ZERO                     ECS020
02256              IF (EP-RCD-TYPE = L1 AND                             ECS020
02257                  CLAS-I-EP (CLAS-INDEXL) = 'P') OR                ECS020
02258                 (EP-RCD-TYPE = 'Z' AND                            ECS020
02259                  CLAS-I-EP (CLAS-INDEXA) = 'P') OR                ECS020
02260                 (STATE-ABBR (CLAS-INDEXS) = 'WY')                 ECS020
02261                  MOVE +1.00      TO WS-PRO                        ECS020
02262              ELSE                                                 ECS020
02263                  MOVE +1.00      TO WS-R78.                       ECS020
02264                                                                   ECS020
02265      IF EP-RCD-TYPE =  L1  AND                                    ECS020
02266         CLAS-I-EP (CLAS-INDEXL) = 'U'                             ECS020
02267          MOVE 'R' TO CLAS-I-EP (CLAS-INDEXL).                     ECS020
02268                                                                   ECS020
02269      IF (DTE-CLIENT = 'FIA'  AND                                  ECS020
02270          EP-CARRIER = ('A' OR 'B' OR 'C' OR 'O')  AND             ECS020
02271          EP-RCD-TYPE = 'Z')                                       ECS020
02272                    OR                                             ECS020
02273         (DTE-CLIENT = ('AN1' OR 'DDB' OR 'UW1' OR 'TFS' OR        ECS020
02274                       'NIS' OR 'BWS')  AND                        ECS020
02275          EP-RCD-TYPE = 'Z')                                       ECS020
02276              COMPUTE WA1-I-NET = WA1-I-NET -                      ECS020
02277                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           ECS020
02278              COMPUTE WA1-I-EARN = WA1-I-EARN +                    ECS020
02279                  ((EP-PRM-PR + EP-PRM-78) / +2)                   ECS020
02280              COMPUTE WA1-I-RESV = WA1-I-RESV +                    ECS020
02281                  (((EP-ISS-PRM - EP-CNC-PRM) -                    ECS020
02282                  ((TT-PRM-PR + TT-PRM-78) / +2))                  ECS020
02283                  * WS-FAC)                                        ECS020
02284               GO TO 1999-EXIT.                                    ECS020
02285                                                                   ECS020
02286      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    ECS020
02287          COMPUTE WA1-I-NET = WA1-I-NET -                          ECS020
02288               (EP-PRM-PR-ADJ * WS-PRO) - (EP-PRM-78-ADJ * WS-R78) ECS020
02289          COMPUTE WA1-I-EARN = WA1-I-EARN +                        ECS020
02290               (EP-PRM-PR * WS-PRO) + (EP-PRM-78 * WS-R78)         ECS020
02291          COMPUTE WA1-I-RESV = WA1-I-RESV +                        ECS020
02292              (((EP-ISS-PRM - EP-CNC-PRM) -                        ECS020
02293              (TT-PRM-PR * WS-PRO) - (TT-PRM-78 * WS-R78))         ECS020
02294              * WS-FAC)                                            ECS020
02295            GO TO 1999-EXIT.                                       ECS020
02296                                                                   ECS020
02297      IF DTE-CLIENT = 'GIC'                                        ECS020
02298          IF EP-RCD-TYPE = 'Z' AND                                 ECS020
02299             CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                    ECS020
02300              COMPUTE WA1-I-NET = WA1-I-NET -                      ECS020
02301                   (EP-PRM-PR-ADJ * +.80) - (EP-PRM-78-ADJ * +.20) ECS020
02302              COMPUTE WA1-I-EARN = WA1-I-EARN +                    ECS020
02303                   (EP-PRM-PR * +.80) + (EP-PRM-78 * +.20)         ECS020
02304              COMPUTE WA1-I-RESV = WA1-I-RESV +                    ECS020
02305                  (((EP-ISS-PRM - EP-CNC-PRM) -                    ECS020
02306                  (TT-PRM-PR * +.80) - (TT-PRM-78 * +.20))         ECS020
02307                  * WS-FAC)                                        ECS020
02308                GO TO 1999-EXIT.                                   ECS020
02309                                                                   ECS020
02310      IF DTE-CLIENT = 'FIA'                                        ECS020
02311          IF EP-RCD-TYPE = L1                                      ECS020
02312            AND EP-REIN NOT = 'R'                                  ECS020
02313              COMPUTE WA1-I-NET = WA1-I-NET - EP-PRM-78-ADJ        ECS020
02314              COMPUTE WA1-I-EARN = WA1-I-EARN + EP-PRM-78          ECS020
02315              COMPUTE WA1-I-RESV = WA1-I-RESV +                    ECS020
02316                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         ECS020
02317                  * WS-FAC)                                        ECS020
02318              GO TO 1999-EXIT.                                     ECS020
02319                                                                   ECS020
02320      IF (EP-RCD-TYPE = L1  AND CLAS-I-EP (CLAS-INDEXL) = '1') OR  ECS020
02321         (EP-RCD-TYPE = 'Z' AND CLAS-I-EP (CLAS-INDEXA) = '1') OR  ECS020
02322         (EP-RCD-TYPE = 'Z' AND DTE-CLIENT = 'ITY'                 ECS020
02323                              AND (EP-CARRIER = '5' OR '7'))       ECS020
02324                  COMPUTE WA1-I-NET = WA1-I-NET -                  ECS020
02325                       (EP-PRM-PR-ADJ * +.6667) -                  ECS020
02326                       (EP-PRM-78-ADJ * +.3333)                    ECS020
02327                  COMPUTE WA1-I-EARN = WA1-I-EARN +                ECS020
02328                       (EP-PRM-PR * +.6667) + (EP-PRM-78 * +.3333) ECS020
02329                  COMPUTE WA1-I-RESV = WA1-I-RESV +                ECS020
02330                      (((EP-ISS-PRM - EP-CNC-PRM) -                ECS020
02331                      (TT-PRM-PR * +.6667) - (TT-PRM-78 * +.3333)) ECS020
02332                      * WS-FAC)                                    ECS020
02333                  GO TO 1999-EXIT.                                 ECS020
02334                                                                   ECS020
02335      IF (EP-RCD-TYPE = L1  AND CLAS-I-EP (CLAS-INDEXL) = 'P') OR  ECS020
02336         (EP-RCD-TYPE = 'Z' AND CLAS-I-EP (CLAS-INDEXA) = 'P') OR  ECS020
02337         (STATE-ABBR (CLAS-INDEXS) = 'WY') OR                      ECS020
02338         (DTE-CLIENT = 'SLC')  OR                                  ECS020
02339         (EP-RCD-TYPE = 'Z' AND DTE-CLIENT = 'ITY'                 ECS020
02340                            AND EP-CARRIER = '2')                  ECS020
02341              COMPUTE WA1-I-NET = WA1-I-NET - EP-PRM-PR-ADJ        ECS020
02342              COMPUTE WA1-I-EARN = WA1-I-EARN + EP-PRM-PR          ECS020
02343              COMPUTE WA1-I-RESV = WA1-I-RESV +                    ECS020
02344                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-PR)         ECS020
02345                  * WS-FAC)                                        ECS020
02346              GO TO 1999-EXIT.                                     ECS020
02347                                                                   ECS020
02348      IF EP-RCD-TYPE = L1                                          ECS020
02349          AND CLAS-I-EP (CLAS-INDEXL) = 'M'                        ECS020
02350               COMPUTE WA1-I-NET = WA1-I-NET -                     ECS020
02351                   ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)          ECS020
02352               COMPUTE WA1-I-EARN = WA1-I-EARN +                   ECS020
02353                   ((EP-PRM-PR + EP-PRM-78) / +2)                  ECS020
02354               COMPUTE WA1-I-RESV = WA1-I-RESV +                   ECS020
02355                   (((EP-ISS-PRM - EP-CNC-PRM) -                   ECS020
02356                   ((TT-PRM-PR + TT-PRM-78) / +2))                 ECS020
02357                   * WS-FAC).                                      ECS020
02358                                                                   ECS020
02359      IF EP-RCD-TYPE = L1                                          ECS020
02360          AND (CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'T' OR 'N' OR 'A') ECS020
02361              COMPUTE WA1-I-NET = WA1-I-NET - EP-PRM-78-ADJ        ECS020
02362              COMPUTE WA1-I-EARN = WA1-I-EARN + EP-PRM-78          ECS020
02363              COMPUTE WA1-I-RESV = WA1-I-RESV +                    ECS020
02364                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         ECS020
02365                  * WS-FAC).                                       ECS020
02366                                                                   ECS020
02367      IF EP-RCD-TYPE = L1                                          ECS020
02368          AND CLAS-I-EP (CLAS-INDEXL) = 'B'                        ECS020
02369              COMPUTE WA1-I-NET = WA1-I-NET - EP-PRM-ST-ADJ        ECS020
02370              COMPUTE WA1-I-EARN = WA1-I-EARN + EP-PRM-ST          ECS020
02371              COMPUTE WA1-I-RESV = WA1-I-RESV +                    ECS020
02372                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-ST)         ECS020
02373                  * WS-FAC).                                       ECS020
02374                                                                   ECS020
02375      IF EP-RCD-TYPE = 'Z'                                         ECS020
02376          AND CLAS-I-EP (CLAS-INDEXA) = 'M'                        ECS020
02377               COMPUTE WA1-I-NET = WA1-I-NET -                     ECS020
02378                   ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)          ECS020
02379               COMPUTE WA1-I-EARN = WA1-I-EARN +                   ECS020
02380                   ((EP-PRM-PR + EP-PRM-78) / +2)                  ECS020
02381               COMPUTE WA1-I-RESV = WA1-I-RESV +                   ECS020
02382                   (((EP-ISS-PRM - EP-CNC-PRM) -                   ECS020
02383                   ((TT-PRM-PR + TT-PRM-78) / +2))                 ECS020
02384                   * WS-FAC).                                      ECS020
02385                                                                   ECS020
02386      IF (EP-RCD-TYPE = 'Z'                                        ECS020
02387                  AND (CLAS-I-EP (CLAS-INDEXA) = 'R' OR 'A'))      ECS020
02388           IF (DTE-CLIENT = 'POS' AND EP-CARRIER = '2')  OR        ECS020
02389              (DTE-CLIENT = 'FIM')                                 ECS020
02390               COMPUTE WA1-I-NET = WA1-I-NET -                     ECS020
02391                   ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)          ECS020
02392               COMPUTE WA1-I-EARN = WA1-I-EARN +                   ECS020
02393                   ((EP-PRM-PR + EP-PRM-78) / +2)                  ECS020
02394               COMPUTE WA1-I-RESV = WA1-I-RESV +                   ECS020
02395                   (((EP-ISS-PRM - EP-CNC-PRM) -                   ECS020
02396                   ((TT-PRM-PR + TT-PRM-78) / +2))                 ECS020
02397                   * WS-FAC)                                       ECS020
02398           ELSE                                                    ECS020
02399              COMPUTE WA1-I-NET = WA1-I-NET - EP-PRM-78-ADJ        ECS020
02400              COMPUTE WA1-I-EARN = WA1-I-EARN + EP-PRM-78          ECS020
02401              COMPUTE WA1-I-RESV = WA1-I-RESV +                    ECS020
02402                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         ECS020
02403                  * WS-FAC).                                       ECS020
02404                                                                   ECS020
02405  1999-EXIT.                                                       ECS020
02406      EXIT.                                                        ECS020
02407  EJECT                                                            ECS020
02408  2000-YTD-ADD.                                                    ECS020
02409      COMPUTE WA1-Y-CERT =                                         ECS020
02410          WA1-Y-CERT + EP-ISS-CNT - EP-CNC-CNT.                    ECS020
02411                                                                   ECS020
02412      COMPUTE WA1-Y-NET =                                          ECS020
02413          WA1-Y-NET + EP-ISS-PRM - EP-CNC-PRM.                     ECS020
02414                                                                   ECS020
02415      ADD EP-CLM-AMT TO WA1-Y-PAID.                                ECS020
02416                                                                   ECS020
02417      ADD EP-CLM-DU TO WA1-Y-RESV.                                 ECS020
02418      IF EP-CLM-PV NUMERIC                                         ECS020
02419          ADD EP-CLM-PV TO WA1-Y-RESV.                             ECS020
02420      ADD EP-LOSS-RESV TO WA1-Y-RESV.                              ECS020
02421                                                                   ECS020
02422      IF DTE-CLIENT = 'FIM'  OR                                    ECS020
02423         (DTE-CLIENT = 'FFL'  AND  EP-RCD-TYPE = L1)               ECS020
02424          NEXT SENTENCE                                            ECS020
02425      ELSE                                                         ECS020
02426          ADD EP-CLM-IBNR TO WA1-Y-RESV.                           ECS020
02427                                                                   ECS020
02428      IF EP-RCD-TYPE = L1                                          ECS020
02429          MOVE FAC-1 TO WS-FAC                                     ECS020
02430      ELSE                                                         ECS020
02431          MOVE FAC-2 TO WS-FAC.                                    ECS020
02432                                                                   ECS020
02433      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    ECS020
02434          IF EP-RCD-TYPE = L1                                      ECS020
02435              MOVE FAC-3          TO WS-FAC                        ECS020
02436              MOVE LF-PRO         TO WS-PRO                        ECS020
02437              MOVE LF-R78         TO WS-R78                        ECS020
02438          ELSE                                                     ECS020
02439              MOVE FAC-4          TO WS-FAC                        ECS020
02440              MOVE AH-PRO         TO WS-PRO                        ECS020
02441              MOVE AH-R78         TO WS-R78.                       ECS020
02442                                                                   ECS020
02443      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    ECS020
02444          IF WS-PRO = ZERO  AND  WS-R78 = ZERO                     ECS020
02445              IF (EP-RCD-TYPE = L1 AND                             ECS020
02446                  CLAS-I-EP (CLAS-INDEXL) = 'P') OR                ECS020
02447                 (EP-RCD-TYPE = 'Z' AND                            ECS020
02448                  CLAS-I-EP (CLAS-INDEXA) = 'P') OR                ECS020
02449                 (STATE-ABBR (CLAS-INDEXS) = 'WY')                 ECS020
02450                  MOVE +1.00      TO WS-PRO                        ECS020
02451              ELSE                                                 ECS020
02452                  MOVE +1.00      TO WS-R78.                       ECS020
02453                                                                   ECS020
02454      IF EP-RCD-TYPE = L1  AND                                     ECS020
02455         CLAS-I-EP (CLAS-INDEXL) = 'U'                             ECS020
02456          MOVE 'R' TO CLAS-I-EP (CLAS-INDEXL).                     ECS020
02457                                                                   ECS020
02458      IF (DTE-CLIENT = 'FIA'  AND                                  ECS020
02459          EP-CARRIER = ('A' OR 'B' OR 'C' OR 'O')  AND             ECS020
02460          EP-RCD-TYPE = 'Z')                                       ECS020
02461                    OR                                             ECS020
02462         (DTE-CLIENT = ('AN1' OR 'DDB' OR 'UW1' OR 'TFS' OR        ECS020
02463                       'NIS' OR 'BWS')  AND                        ECS020
02464          EP-RCD-TYPE = 'Z')                                       ECS020
02465              COMPUTE WA1-Y-NET = WA1-Y-NET -                      ECS020
02466                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           ECS020
02467              COMPUTE WA1-Y-EARN = WA1-Y-EARN +                    ECS020
02468                  ((EP-PRM-PR + EP-PRM-78) / +2)                   ECS020
02469              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    ECS020
02470                  (((EP-ISS-PRM - EP-CNC-PRM) -                    ECS020
02471                  ((TT-PRM-PR + TT-PRM-78) / +2))                  ECS020
02472                  * WS-FAC)                                        ECS020
02473                GO TO 2099-EXIT.                                   ECS020
02474                                                                   ECS020
02475      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    ECS020
02476          COMPUTE WA1-Y-NET = WA1-Y-NET -                          ECS020
02477               (EP-PRM-PR-ADJ * WS-PRO) - (EP-PRM-78-ADJ * WS-R78) ECS020
02478          COMPUTE WA1-Y-EARN = WA1-Y-EARN +                        ECS020
02479               (EP-PRM-PR * WS-PRO) + (EP-PRM-78 * WS-R78)         ECS020
02480          COMPUTE WA1-Y-RESV = WA1-Y-RESV +                        ECS020
02481              (((EP-ISS-PRM - EP-CNC-PRM) -                        ECS020
02482              (TT-PRM-PR * WS-PRO) - (TT-PRM-78 * WS-R78))         ECS020
02483              * WS-FAC)                                            ECS020
02484            GO TO 2099-EXIT.                                       ECS020
02485                                                                   ECS020
02486      IF DTE-CLIENT = 'GIC'                                        ECS020
02487          IF EP-RCD-TYPE = 'Z' AND                                 ECS020
02488             CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                    ECS020
02489              COMPUTE WA1-Y-NET = WA1-Y-NET -                      ECS020
02490                   (EP-PRM-PR-ADJ * +.80) - (EP-PRM-78-ADJ * +.20) ECS020
02491              COMPUTE WA1-Y-EARN = WA1-Y-EARN +                    ECS020
02492                   (EP-PRM-PR * +.80) + (EP-PRM-78 * +.20)         ECS020
02493              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    ECS020
02494                  (((EP-ISS-PRM - EP-CNC-PRM) -                    ECS020
02495                  (TT-PRM-PR * +.80) - (TT-PRM-78 * +.20))         ECS020
02496                  * WS-FAC)                                        ECS020
02497                GO TO 2099-EXIT.                                   ECS020
02498                                                                   ECS020
02499      IF DTE-CLIENT = 'FIA'                                        ECS020
02500          IF EP-RCD-TYPE = L1                                      ECS020
02501            AND EP-REIN NOT = 'R'                                  ECS020
02502              COMPUTE WA1-Y-NET = WA1-Y-NET - EP-PRM-78-ADJ        ECS020
02503              COMPUTE WA1-Y-EARN = WA1-Y-EARN + EP-PRM-78          ECS020
02504              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    ECS020
02505                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         ECS020
02506                  * WS-FAC)                                        ECS020
02507              GO TO 2099-EXIT.                                     ECS020
02508                                                                   ECS020
02509      IF (EP-RCD-TYPE =  L1 AND CLAS-I-EP (CLAS-INDEXL) = '1') OR  ECS020
02510         (EP-RCD-TYPE = 'Z' AND CLAS-I-EP (CLAS-INDEXA) = '1') OR  ECS020
02511         (EP-RCD-TYPE = 'Z' AND DTE-CLIENT = 'ITY'                 ECS020
02512                              AND (EP-CARRIER = '5' OR '7'))       ECS020
02513                  COMPUTE WA1-Y-NET = WA1-Y-NET -                  ECS020
02514                       (EP-PRM-PR-ADJ * +.6667) -                  ECS020
02515                       (EP-PRM-78-ADJ * +.3333)                    ECS020
02516                  COMPUTE WA1-Y-EARN = WA1-Y-EARN +                ECS020
02517                       (EP-PRM-PR * +.6667) + (EP-PRM-78 * +.3333) ECS020
02518                  COMPUTE WA1-Y-RESV = WA1-Y-RESV +                ECS020
02519                      (((EP-ISS-PRM - EP-CNC-PRM) -                ECS020
02520                      (TT-PRM-PR * +.6667) - (TT-PRM-78 * +.3333)) ECS020
02521                      * WS-FAC)                                    ECS020
02522                  GO TO 2099-EXIT.                                 ECS020
02523                                                                   ECS020
02524      IF (EP-RCD-TYPE =  L1 AND CLAS-I-EP (CLAS-INDEXL) = 'P') OR  ECS020
02525         (EP-RCD-TYPE = 'Z' AND CLAS-I-EP (CLAS-INDEXA) = 'P') OR  ECS020
02526         (STATE-ABBR (CLAS-INDEXS) = 'WY') OR                      ECS020
02527         (DTE-CLIENT = 'SLC')  OR                                  ECS020
02528         (EP-RCD-TYPE = 'Z' AND DTE-CLIENT = 'ITY'                 ECS020
02529                              AND EP-CARRIER = '2')                ECS020
02530              COMPUTE WA1-Y-NET = WA1-Y-NET - EP-PRM-PR-ADJ        ECS020
02531              COMPUTE WA1-Y-EARN = WA1-Y-EARN + EP-PRM-PR          ECS020
02532              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    ECS020
02533                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-PR)         ECS020
02534                  * WS-FAC)                                        ECS020
02535             GO TO 2099-EXIT.                                      ECS020
02536                                                                   ECS020
02537      IF EP-RCD-TYPE = L1                                          ECS020
02538          AND CLAS-I-EP (CLAS-INDEXL) = 'M'                        ECS020
02539              COMPUTE WA1-Y-NET = WA1-Y-NET -                      ECS020
02540                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           ECS020
02541              COMPUTE WA1-Y-EARN = WA1-Y-EARN +                    ECS020
02542                  ((EP-PRM-PR + EP-PRM-78) / +2)                   ECS020
02543              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    ECS020
02544                  (((EP-ISS-PRM - EP-CNC-PRM) -                    ECS020
02545                  ((TT-PRM-PR + TT-PRM-78) / +2))                  ECS020
02546                  * WS-FAC).                                       ECS020
02547                                                                   ECS020
02548      IF EP-RCD-TYPE = L1                                          ECS020
02549          AND (CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'T' OR 'N' OR 'A') ECS020
02550              COMPUTE WA1-Y-NET = WA1-Y-NET - EP-PRM-78-ADJ        ECS020
02551              COMPUTE WA1-Y-EARN = WA1-Y-EARN + EP-PRM-78          ECS020
02552              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    ECS020
02553                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         ECS020
02554                  * WS-FAC).                                       ECS020
02555                                                                   ECS020
02556      IF EP-RCD-TYPE = L1                                          ECS020
02557          AND CLAS-I-EP (CLAS-INDEXL) = 'B'                        ECS020
02558              COMPUTE WA1-Y-NET = WA1-Y-NET - EP-PRM-ST-ADJ        ECS020
02559              COMPUTE WA1-Y-EARN = WA1-Y-EARN + EP-PRM-ST          ECS020
02560              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    ECS020
02561                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-ST)         ECS020
02562                  * WS-FAC).                                       ECS020
02563                                                                   ECS020
02564      IF EP-RCD-TYPE = 'Z'                                         ECS020
02565          AND CLAS-I-EP (CLAS-INDEXA) = 'M'                        ECS020
02566              COMPUTE WA1-Y-NET = WA1-Y-NET -                      ECS020
02567                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           ECS020
02568              COMPUTE WA1-Y-EARN = WA1-Y-EARN +                    ECS020
02569                  ((EP-PRM-PR + EP-PRM-78) / +2)                   ECS020
02570              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    ECS020
02571                  (((EP-ISS-PRM - EP-CNC-PRM) -                    ECS020
02572                  ((TT-PRM-PR + TT-PRM-78) / +2))                  ECS020
02573                  * WS-FAC).                                       ECS020
02574                                                                   ECS020
02575      IF (EP-RCD-TYPE = 'Z'                                        ECS020
02576                  AND (CLAS-I-EP (CLAS-INDEXA) = 'R' OR 'A'))      ECS020
02577          IF (DTE-CLIENT = 'POS' AND EP-CARRIER = '2')  OR         ECS020
02578             (DTE-CLIENT = 'FIM')                                  ECS020
02579              COMPUTE WA1-Y-NET = WA1-Y-NET -                      ECS020
02580                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           ECS020
02581              COMPUTE WA1-Y-EARN = WA1-Y-EARN +                    ECS020
02582                  ((EP-PRM-PR + EP-PRM-78) / +2)                   ECS020
02583              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    ECS020
02584                  (((EP-ISS-PRM - EP-CNC-PRM) -                    ECS020
02585                  ((TT-PRM-PR + TT-PRM-78) / +2))                  ECS020
02586                  * WS-FAC)                                        ECS020
02587          ELSE                                                     ECS020
02588              COMPUTE WA1-Y-NET = WA1-Y-NET - EP-PRM-78-ADJ        ECS020
02589              COMPUTE WA1-Y-EARN = WA1-Y-EARN + EP-PRM-78          ECS020
02590              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    ECS020
02591                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         ECS020
02592                  * WS-FAC).                                       ECS020
02593                                                                   ECS020
02594  2099-EXIT.                                                       ECS020
02595      EXIT.                                                        ECS020
02596  EJECT                                                            ECS020
02597  2100-YTD-SUB.                                                    ECS020
02598      COMPUTE WA1-Y-CERT =                                         ECS020
02599          WA1-Y-CERT - EP-ISS-CNT + EP-CNC-CNT.                    ECS020
02600                                                                   ECS020
02601      COMPUTE WA1-Y-NET =                                          ECS020
02602          WA1-Y-NET - EP-ISS-PRM + EP-CNC-PRM.                     ECS020
02603                                                                   ECS020
02604      SUBTRACT EP-CLM-AMT FROM WA1-Y-PAID.                         ECS020
02605                                                                   ECS020
02606      SUBTRACT EP-CLM-DU FROM WA1-Y-RESV.                          ECS020
02607      IF EP-CLM-PV NUMERIC                                         ECS020
02608          SUBTRACT EP-CLM-PV FROM WA1-Y-RESV.                      ECS020
02609      SUBTRACT EP-LOSS-RESV FROM WA1-Y-RESV.                       ECS020
02610                                                                   ECS020
02611      IF DTE-CLIENT = 'FIM'  OR                                    ECS020
02612         (DTE-CLIENT = 'FFL'  AND  EP-RCD-TYPE = L1)               ECS020
02613          NEXT SENTENCE                                            ECS020
02614      ELSE                                                         ECS020
02615          SUBTRACT EP-CLM-IBNR FROM WA1-Y-RESV.                    ECS020
02616                                                                   ECS020
02617      IF EP-RCD-TYPE = L1                                          ECS020
02618          MOVE FAC-1 TO WS-FAC                                     ECS020
02619      ELSE                                                         ECS020
02620          MOVE FAC-2 TO WS-FAC.                                    ECS020
02621                                                                   ECS020
02622      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    ECS020
02623          IF EP-RCD-TYPE = L1                                      ECS020
02624              MOVE FAC-3          TO WS-FAC                        ECS020
02625              MOVE LF-PRO         TO WS-PRO                        ECS020
02626              MOVE LF-R78         TO WS-R78                        ECS020
02627          ELSE                                                     ECS020
02628              MOVE FAC-4          TO WS-FAC                        ECS020
02629              MOVE AH-PRO         TO WS-PRO                        ECS020
02630              MOVE AH-R78         TO WS-R78.                       ECS020
02631                                                                   ECS020
02632      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    ECS020
02633          IF WS-PRO = ZERO  AND  WS-R78 = ZERO                     ECS020
02634              IF (EP-RCD-TYPE = L1 AND                             ECS020
02635                  CLAS-I-EP (CLAS-INDEXL) = 'P') OR                ECS020
02636                 (EP-RCD-TYPE = 'Z' AND                            ECS020
02637                  CLAS-I-EP (CLAS-INDEXA) = 'P') OR                ECS020
02638                 (STATE-ABBR (CLAS-INDEXS) = 'WY')                 ECS020
02639                  MOVE +1.00      TO WS-PRO                        ECS020
02640              ELSE                                                 ECS020
02641                  MOVE +1.00      TO WS-R78.                       ECS020
02642                                                                   ECS020
02643      IF EP-RCD-TYPE = L1  AND                                     ECS020
02644         CLAS-I-EP (CLAS-INDEXL) = 'U'                             ECS020
02645          MOVE 'R' TO CLAS-I-EP (CLAS-INDEXL).                     ECS020
02646                                                                   ECS020
02647      IF (DTE-CLIENT = 'FIA'  AND                                  ECS020
02648          EP-CARRIER = ('A' OR 'B' OR 'C' OR 'O')  AND             ECS020
02649          EP-RCD-TYPE = 'Z')                                       ECS020
02650                    OR                                             ECS020
02651         (DTE-CLIENT = ('AN1' OR 'DDB' OR 'UW1' OR 'TFS' OR        ECS020
02652                       'NIS' OR 'BWS')  AND                        ECS020
02653          EP-RCD-TYPE = 'Z')                                       ECS020
02654              COMPUTE WA1-Y-NET = WA1-Y-NET +                      ECS020
02655                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           ECS020
02656              COMPUTE WA1-Y-EARN = WA1-Y-EARN -                    ECS020
02657                  ((EP-PRM-PR + EP-PRM-78) / +2)                   ECS020
02658              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    ECS020
02659                  (((EP-ISS-PRM - EP-CNC-PRM) -                    ECS020
02660                  ((TT-PRM-PR + TT-PRM-78) / +2))                  ECS020
02661                  * WS-FAC)                                        ECS020
02662                GO TO 2199-EXIT.                                   ECS020
02663                                                                   ECS020
02664      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    ECS020
02665          COMPUTE WA1-Y-NET = WA1-Y-NET +                          ECS020
02666               (EP-PRM-PR-ADJ * WS-PRO) + (EP-PRM-78-ADJ * WS-R78) ECS020
02667          COMPUTE WA1-Y-EARN = WA1-Y-EARN -                        ECS020
02668               (EP-PRM-PR * WS-PRO) - (EP-PRM-78 * WS-R78)         ECS020
02669          COMPUTE WA1-Y-RESV = WA1-Y-RESV -                        ECS020
02670              (((EP-ISS-PRM - EP-CNC-PRM) -                        ECS020
02671              (TT-PRM-PR * WS-PRO) - (TT-PRM-78 * WS-R78))         ECS020
02672              * WS-FAC)                                            ECS020
02673         GO TO 2199-EXIT.                                          ECS020
02674                                                                   ECS020
02675      IF DTE-CLIENT = 'GIC'                                        ECS020
02676          IF EP-RCD-TYPE = 'Z' AND                                 ECS020
02677             CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                    ECS020
02678              COMPUTE WA1-Y-NET = WA1-Y-NET +                      ECS020
02679                   (EP-PRM-PR-ADJ * +.80) + (EP-PRM-78-ADJ * +.20) ECS020
02680              COMPUTE WA1-Y-EARN = WA1-Y-EARN -                    ECS020
02681                   (EP-PRM-PR * +.80) - (EP-PRM-78 * +.20)         ECS020
02682              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    ECS020
02683                  (((EP-ISS-PRM - EP-CNC-PRM) -                    ECS020
02684                  (TT-PRM-PR * +.80) - (TT-PRM-78 * +.20))         ECS020
02685                  * WS-FAC)                                        ECS020
02686             GO TO 2199-EXIT.                                      ECS020
02687                                                                   ECS020
02688      IF DTE-CLIENT = 'FIA'                                        ECS020
02689          IF EP-RCD-TYPE = L1                                      ECS020
02690            AND EP-REIN NOT = 'R'                                  ECS020
02691              COMPUTE WA1-Y-NET = WA1-Y-NET + EP-PRM-78-ADJ        ECS020
02692              COMPUTE WA1-Y-EARN = WA1-Y-EARN - EP-PRM-78          ECS020
02693              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    ECS020
02694                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         ECS020
02695                  * WS-FAC)                                        ECS020
02696              GO TO 2199-EXIT.                                     ECS020
02697                                                                   ECS020
02698      IF (EP-RCD-TYPE = L1 AND CLAS-I-EP (CLAS-INDEXL) = '1') OR   ECS020
02699         (EP-RCD-TYPE = 'Z' AND CLAS-I-EP (CLAS-INDEXA) = '1') OR  ECS020
02700         (EP-RCD-TYPE = 'Z' AND DTE-CLIENT = 'ITY'                 ECS020
02701                              AND (EP-CARRIER = '5' OR '7'))       ECS020
02702                  COMPUTE WA1-Y-NET = WA1-Y-NET +                  ECS020
02703                       (EP-PRM-PR-ADJ * +.6667) +                  ECS020
02704                       (EP-PRM-78-ADJ * +.3333)                    ECS020
02705                  COMPUTE WA1-Y-EARN = WA1-Y-EARN -                ECS020
02706                       (EP-PRM-PR * +.6667) - (EP-PRM-78 * +.3333) ECS020
02707                  COMPUTE WA1-Y-RESV = WA1-Y-RESV -                ECS020
02708                      (((EP-ISS-PRM - EP-CNC-PRM) -                ECS020
02709                      (TT-PRM-PR * +.6667) - (TT-PRM-78 * +.3333)) ECS020
02710                      * WS-FAC)                                    ECS020
02711                 GO TO 2199-EXIT.                                  ECS020
02712                                                                   ECS020
02713      IF (EP-RCD-TYPE = L1 AND CLAS-I-EP (CLAS-INDEXL) = 'P') OR   ECS020
02714         (EP-RCD-TYPE = 'Z' AND CLAS-I-EP (CLAS-INDEXA) = 'P') OR  ECS020
02715         (STATE-ABBR (CLAS-INDEXS) = 'WY') OR                      ECS020
02716         (DTE-CLIENT = 'SLC')  OR                                  ECS020
02717         (EP-RCD-TYPE = 'Z' AND DTE-CLIENT = 'ITY'                 ECS020
02718                              AND EP-CARRIER = '2')                ECS020
02719              COMPUTE WA1-Y-NET = WA1-Y-NET + EP-PRM-PR-ADJ        ECS020
02720              COMPUTE WA1-Y-EARN = WA1-Y-EARN - EP-PRM-PR          ECS020
02721              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    ECS020
02722                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-PR)         ECS020
02723                  * WS-FAC)                                        ECS020
02724                   GO TO 2199-EXIT.                                ECS020
02725                                                                   ECS020
02726      IF EP-RCD-TYPE = L1                                          ECS020
02727         AND CLAS-I-EP (CLAS-INDEXL) = 'M'                         ECS020
02728              COMPUTE WA1-Y-NET = WA1-Y-NET +                      ECS020
02729                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           ECS020
02730              COMPUTE WA1-Y-EARN = WA1-Y-EARN -                    ECS020
02731                  ((EP-PRM-PR + EP-PRM-78) / +2)                   ECS020
02732              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    ECS020
02733                  (((EP-ISS-PRM - EP-CNC-PRM) -                    ECS020
02734                  ((TT-PRM-PR + TT-PRM-78) / +2))                  ECS020
02735                  * WS-FAC).                                       ECS020
02736                                                                   ECS020
02737      IF EP-RCD-TYPE = L1                                          ECS020
02738         AND (CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'T' OR 'N' OR 'A')  ECS020
02739              COMPUTE WA1-Y-NET = WA1-Y-NET + EP-PRM-78-ADJ        ECS020
02740              COMPUTE WA1-Y-EARN = WA1-Y-EARN - EP-PRM-78          ECS020
02741              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    ECS020
02742                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         ECS020
02743                  * WS-FAC).                                       ECS020
02744                                                                   ECS020
02745      IF EP-RCD-TYPE = L1                                          ECS020
02746         AND CLAS-I-EP (CLAS-INDEXL) = 'B'                         ECS020
02747              COMPUTE WA1-Y-NET = WA1-Y-NET + EP-PRM-ST-ADJ        ECS020
02748              COMPUTE WA1-Y-EARN = WA1-Y-EARN - EP-PRM-ST          ECS020
02749              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    ECS020
02750                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-ST)         ECS020
02751                  * WS-FAC).                                       ECS020
02752                                                                   ECS020
02753      IF EP-RCD-TYPE = 'Z'                                         ECS020
02754         AND CLAS-I-EP (CLAS-INDEXA) = 'M'                         ECS020
02755              COMPUTE WA1-Y-NET = WA1-Y-NET +                      ECS020
02756                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           ECS020
02757              COMPUTE WA1-Y-EARN = WA1-Y-EARN -                    ECS020
02758                  ((EP-PRM-PR + EP-PRM-78) / +2)                   ECS020
02759              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    ECS020
02760                  (((EP-ISS-PRM - EP-CNC-PRM) -                    ECS020
02761                  ((TT-PRM-PR + TT-PRM-78) / +2))                  ECS020
02762                  * WS-FAC).                                       ECS020
02763                                                                   ECS020
02764      IF (EP-RCD-TYPE = 'Z'                                        ECS020
02765                  AND (CLAS-I-EP (CLAS-INDEXA) = 'R' OR 'A'))      ECS020
02766          IF (DTE-CLIENT = 'POS' AND EP-CARRIER = '2')  OR         ECS020
02767             (DTE-CLIENT = 'FIM')                                  ECS020
02768              COMPUTE WA1-Y-NET = WA1-Y-NET +                      ECS020
02769                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           ECS020
02770              COMPUTE WA1-Y-EARN = WA1-Y-EARN -                    ECS020
02771                  ((EP-PRM-PR + EP-PRM-78) / +2)                   ECS020
02772              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    ECS020
02773                  (((EP-ISS-PRM - EP-CNC-PRM) -                    ECS020
02774                  ((TT-PRM-PR + TT-PRM-78) / +2))                  ECS020
02775                  * WS-FAC)                                        ECS020
02776          ELSE                                                     ECS020
02777              COMPUTE WA1-Y-NET = WA1-Y-NET + EP-PRM-78-ADJ        ECS020
02778              COMPUTE WA1-Y-EARN = WA1-Y-EARN - EP-PRM-78          ECS020
02779              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    ECS020
02780                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         ECS020
02781                  * WS-FAC).                                       ECS020
02782                                                                   ECS020
02783  2199-EXIT.                                                       ECS020
02784      EXIT.                                                        ECS020
02785  EJECT                                                            ECS020
02786  2200-FIND-REIN-NAME.                                             ECS020
02787 *    IF PRINTING-REIN-TOTALS                                      ECS020
02788 *        MOVE SPACES              TO H2-CARRIER  H2-CARR          ECS020
02789 *    ELSE                                                         ECS020
02790 *        MOVE 'CARRIER   '        TO H2-CARRIER.                  ECS020
02791 *        MOVE SV-CARRIER          TO H2-CARR.                     ECS020
02792                                                                   ECS020
02793      MOVE 'REINSURED BY' TO H3R-REIN.                             ECS020
02794 *    MOVE 'CEDED FROM'   TO H3R-REIN.                             ECS020
02795                                                                   ECS020
02796      IF SV-REINCO = H3R-REIN-COMP                                 ECS020
02797 **       GO TO 2299-EXIT.                                         ECS020
JJPMOD         GO TO 2230-EXIT.                                         ECS020
02798                                                                   ECS020
02799      MOVE +0        TO WS-RCT-X.                                  ECS020
02800      MOVE SV-REINCO TO H3R-REIN-COMP.                             ECS020
02801                                                                   ECS020
02802  2210-FIND-REIN-NAME-LOOP.                                        ECS020
02803      ADD +1 TO WS-RCT-X.                                          ECS020
02804                                                                   ECS020
02805      IF WS-RCT-X GREATER THAN WS-RCT-MAX OR                       ECS020
02806         WS-RCT-COMP (WS-RCT-X) = HIGH-VALUES                      ECS020
02807              MOVE 'NAME UNKNOWN' TO H3R-REIN-NAME                 ECS020
02808                                     SV-REIN-NAME                  ECS020
02809 **           GO TO 2299-EXIT.                                     ECS020
JJPMOD             GO TO 2230-EXIT.                                     ECS020
02810                                                                   ECS020
02811      IF SV-REI-CO = WS-RCT-COMP (WS-RCT-X)                        ECS020
02812          MOVE WS-RCT-NAME (WS-RCT-X) TO H3R-REIN-NAME             ECS020
02813                                         SV-REIN-NAME              ECS020
02814 *        MOVE WS-RCT-CEDE (WS-RCT-X) TO H3R-REIN-NAME             ECS020
02815 **       GO TO 2299-EXIT.                                         ECS020
JJPMOD         GO TO 2230-EXIT.                                         ECS020
02816                                                                   ECS020
02817      GO TO 2210-FIND-REIN-NAME-LOOP.                              ECS020
02818                                                                   ECS020
02819 *2299-EXIT.                                                       ECS020
JJPMOD 2230-EXIT.                                                       ECS020
02820      EXIT.                                                        ECS020
JJPMOD                                                                  ECS020
JJPMOD 2250-FIND-REIN-NAME.                                             ECS020
JJPMOD                                                                  ECS020
JJPMOD     IF SW-REINCO = SV-X-REIN-COMP                                ECS020
JJPMOD         MOVE SV-X-REIN-NAME TO SW-REIN-NAME                      ECS020
JJPMOD         GO TO 2270-EXIT.                                         ECS020
JJPMOD                                                                  ECS020
JJPMOD     MOVE +0        TO WS-RCT-X.                                  ECS020
JJPMOD     MOVE SW-REINCO TO SV-X-REIN-COMP.                            ECS020
JJPMOD                                                                  ECS020
JJPMOD 2260-FIND-REIN-NAME-LOOP.                                        ECS020
JJPMOD     ADD +1 TO WS-RCT-X.                                          ECS020
JJPMOD                                                                  ECS020
JJPMOD     IF WS-RCT-X GREATER THAN WS-RCT-MAX OR                       ECS020
JJPMOD        WS-RCT-COMP (WS-RCT-X) = HIGH-VALUES                      ECS020
JJPMOD             MOVE 'NAME UNKNOWN' TO SW-REIN-NAME                  ECS020
JJPMOD                                    SV-X-REIN-NAME                ECS020
JJPMOD             GO TO 2270-EXIT.                                     ECS020
JJPMOD                                                                  ECS020
JJPMOD     IF SW-REI-CO = WS-RCT-COMP (WS-RCT-X)                        ECS020
JJPMOD         MOVE WS-RCT-NAME (WS-RCT-X) TO SW-REIN-NAME              ECS020
JJPMOD                                        SV-X-REIN-NAME            ECS020
JJPMOD         IF SW-REIN-NAME = SPACES OR LOW-VALUES
JJPMOD             MOVE 'NAME UNKNOWN' TO SW-REIN-NAME                  ECS020
JJPMOD                                    SV-X-REIN-NAME                ECS020
JJPMOD         END-IF
JJPMOD         GO TO 2270-EXIT                                          ECS020
JJPMOD     END-IF.                                                      ECS020
JJPMOD                                                                  ECS020
JJPMOD     GO TO 2260-FIND-REIN-NAME-LOOP.                              ECS020
JJPMOD                                                                  ECS020
JJPMOD 2270-EXIT.                                                       ECS020
JJPMOD     EXIT.                                                        ECS020
02821  EJECT                                                            ECS020
02822  2300-HDR-RTN-A.                                                  ECS020
02823                                                                   ECS020
02824      IF  LOSS-RATIO-TOO-LARGE                                     ECS020
02825          MOVE LOSS-RATIO-MSG     TO PRT                           ECS020
02826          MOVE '-'                TO X                             ECS020
02827          MOVE SPACES             TO LOSS-RATIO-SW                 ECS020
02828          MOVE +3                 TO LINER                         ECS020
02829          PERFORM 2500-PRT-RTN THRU 2599-EXIT.                     ECS020
02830                                                                   ECS020
02831      ADD +1 TO PAGER.                                             ECS020
02832      MOVE PAGER TO H3-PAGE.                                       ECS020
02833      MOVE HDR-1 TO PRT.                                           ECS020
02834      MOVE '1'   TO X.                                             ECS020
02835                                                                   ECS020
02836      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02837                                                                   ECS020
02838      MOVE HDR-2 TO PRT.                                           ECS020
02839      MOVE ' '   TO X.                                             ECS020
02840                                                                   ECS020
02841      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02842                                                                   ECS020
02843      MOVE HDR-3 TO PRT.                                           ECS020
02844      MOVE ' '   TO X.                                             ECS020
02845                                                                   ECS020
02846      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02847                                                                   ECS020
02848      MOVE +3 TO LINER.                                            ECS020
02849                                                                   ECS020
02850      IF SV-REIN = 'B'                                             ECS020
02851          MOVE SV-RPT-CD-1      TO H3B-RPT-CODE-1                  ECS020
02852          MOVE HDR-3B TO PRT                                       ECS020
02853          MOVE ' '    TO X                                         ECS020
02854          PERFORM 2500-PRT-RTN THRU 2599-EXIT                      ECS020
02855          ADD +1 TO LINER.                                         ECS020
02856                                                                   ECS020
02857      IF SV-REIN = 'C'                                             ECS020
02858          MOVE SV-RPT-CD-2      TO H3C-RPT-CODE-2                  ECS020
02859          MOVE HDR-3C TO PRT                                       ECS020
02860          MOVE ' '    TO X                                         ECS020
02861          PERFORM 2500-PRT-RTN THRU 2599-EXIT                      ECS020
02862          ADD +1 TO LINER.                                         ECS020
02863                                                                   ECS020
02864      IF SV-REIN = 'G'                                             ECS020
02865          IF PRINTING-G-A-HDR                                      ECS020
02866              MOVE SV-RPT-CD-2      TO H3G-G-A-NUMBER              ECS020
02867              MOVE HDR-3G TO PRT                                   ECS020
02868              MOVE ' '    TO X                                     ECS020
02869              PERFORM 2500-PRT-RTN THRU 2599-EXIT                  ECS020
02870              ADD +1 TO LINER.                                     ECS020
02871                                                                   ECS020
02872      IF SV-REIN = 'R'                                             ECS020
02873 **       PERFORM 2200-FIND-REIN-NAME THRU 2299-EXIT               ECS020
JJPMOD         PERFORM 2200-FIND-REIN-NAME THRU 2230-EXIT               ECS020
02874          MOVE HDR-3R TO PRT                                       ECS020
02875          MOVE ' '    TO X                                         ECS020
02876          PERFORM 2500-PRT-RTN THRU 2599-EXIT                      ECS020
02877          ADD +1 TO LINER.                                         ECS020
02878                                                                   ECS020
02879      MOVE HDR-5A TO PRT.                                          ECS020
02880      MOVE '0'    TO X.                                            ECS020
02881      ADD +2 TO LINER.                                             ECS020
02882                                                                   ECS020
02883      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02884                                                                   ECS020
02885      MOVE HDR-6A TO PRT.                                          ECS020
02886      MOVE ' '    TO X.                                            ECS020
02887      ADD +1 TO LINER.                                             ECS020
02888                                                                   ECS020
02889      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02890                                                                   ECS020
02891      MOVE HDR-7A TO PRT.                                          ECS020
02892      MOVE ' '    TO X.                                            ECS020
02893      ADD +1 TO LINER.                                             ECS020
02894                                                                   ECS020
02895      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02896                                                                   ECS020
02897  2300-EXIT.                                                       ECS020
02898      EXIT.                                                        ECS020
02899  EJECT                                                            ECS020
02900  2350-HDR-RTN-T.                                                  ECS020
02901                                                                   ECS020
02902      IF  LOSS-RATIO-TOO-LARGE                                     ECS020
02903          MOVE LOSS-RATIO-MSG TO PRT                               ECS020
02904          MOVE '-'            TO X                                 ECS020
02905          MOVE SPACES         TO LOSS-RATIO-SW                     ECS020
02906          PERFORM 2500-PRT-RTN THRU 2599-EXIT.                     ECS020
02907                                                                   ECS020
02908      ADD +1 TO PAGER.                                             ECS020
02909      MOVE PAGER TO H3-PAGE.                                       ECS020
02910      MOVE HDR-1 TO PRT.                                           ECS020
02911      MOVE '1'   TO X.                                             ECS020
02912                                                                   ECS020
02913      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02914                                                                   ECS020
02915      MOVE HDR-2 TO PRT.                                           ECS020
02916      MOVE ' '   TO X.                                             ECS020
02917                                                                   ECS020
02918      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02919                                                                   ECS020
02920      MOVE HDR-3 TO PRT.                                           ECS020
02921      MOVE ' '   TO X.                                             ECS020
02922                                                                   ECS020
02923      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02924                                                                   ECS020
02925      MOVE +3 TO LINER.                                            ECS020
02926                                                                   ECS020
02927  2350-EXIT.                                                       ECS020
02928      EXIT.                                                        ECS020
02929                                                                   ECS020
02930  2370-WRITE-DETAIL-HEADER.                                        ECS020
02931                                                                   ECS020
02932      MOVE HDR-4-TOTAL            TO PRT.                          ECS020
02933      MOVE '0'                    TO X.                            ECS020
02934      ADD +2                      TO LINER.                        ECS020
02935                                                                   ECS020
02936      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02937                                                                   ECS020
02938      MOVE HDR-5A TO PRT.                                          ECS020
02939      MOVE '0'    TO X.                                            ECS020
02940      ADD +2 TO LINER.                                             ECS020
02941                                                                   ECS020
02942      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02943                                                                   ECS020
02944      MOVE HDR-6A TO PRT.                                          ECS020
02945      MOVE ' '    TO X.                                            ECS020
02946      ADD +1 TO LINER.                                             ECS020
02947                                                                   ECS020
02948      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02949                                                                   ECS020
02950      MOVE HDR-7A TO PRT.                                          ECS020
02951      MOVE ' '    TO X.                                            ECS020
02952      ADD +1 TO LINER.                                             ECS020
02953                                                                   ECS020
02954      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02955                                                                   ECS020
02956  2370-EXIT.                                                       ECS020
02957      EXIT.                                                        ECS020
02958                                                                   ECS020
02959  2400-HDR-RTN-B.                                                  ECS020
02960                                                                   ECS020
02961      MOVE HDR-4A TO PRT.                                          ECS020
02962      MOVE '-'    TO X.                                            ECS020
02963      ADD +3      TO LINER.                                        ECS020
02964                                                                   ECS020
02965      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02966                                                                   ECS020
02967 **   IF H1-SUFFIX = 'R'                                           ECS020
JJPMOD     IF H1-SUFFIX = 'R' OR 'X'                                    ECS020
02968          MOVE ' REINSURANCE' TO H8-DESC                           ECS020
02969      ELSE                                                         ECS020
02970          MOVE ' NEW BUSINESS' TO H8-DESC.                         ECS020
02971                                                                   ECS020
02972      MOVE ' '    TO X.                                            ECS020
02973      MOVE HDR-8  TO PRT.                                          ECS020
02974                                                                   ECS020
02975      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02976                                                                   ECS020
02977      ADD +1 TO LINER.                                             ECS020
02978                                                                   ECS020
02979  2400-EXIT.                                                       ECS020
02980      EXIT.                                                        ECS020
02981                                                                   ECS020
02982  2420-HDR-RTN-T.                                                  ECS020
02983                                                                   ECS020
02984 **   IF H1-SUFFIX = 'R'                                           ECS020
JJPMOD     IF H1-SUFFIX = 'R' OR 'X'                                    ECS020
02985          MOVE ' REINSURANCE' TO H8-DESC                           ECS020
02986      ELSE                                                         ECS020
02987          MOVE ' NEW BUSINESS' TO H8-DESC.                         ECS020
02988                                                                   ECS020
02989      MOVE ' '    TO X.                                            ECS020
02990      MOVE HDR-8  TO PRT.                                          ECS020
02991                                                                   ECS020
02992      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         ECS020
02993                                                                   ECS020
02994      ADD +1 TO LINER.                                             ECS020
02995                                                                   ECS020
02996  2420-EXIT.                                                       ECS020
02997      EXIT.                                                        ECS020
02998                                                                   ECS020
02999  2500-PRT-RTN.                                                    ECS020
CIDMOD                             COPY PRTN020.                        ECS020
CIDMOD*                            COPY ELCPRT2.                        ECS020
03001                                                                   ECS020
03002  2599-EXIT.            EXIT.                                      ECS020
03003                                  EJECT                            ECS020
03004  2600-REMOVE-SPACES.                                              ECS020
03005                                                                   ECS020
03006      MOVE SPACES                 TO H4T-LINE.                     ECS020
03007      SET H4T-NDX                 TO W-ZEROS.                      ECS020
03008                                                                   ECS020
03009      PERFORM 2620-TRANSFER-DATA THRU 2620-EXIT                    ECS020
03010              VARYING                                              ECS020
03011          W-TW-NDX FROM 1 BY 1                                     ECS020
03012              UNTIL                                                ECS020
03013          W-TW-NDX GREATER THAN +284.                              ECS020
03014                                                                   ECS020
03015  2600-EXIT.                                                       ECS020
03016      EXIT.                                                        ECS020
03017                                                                   ECS020
03018  2620-TRANSFER-DATA.                                              ECS020
03019                                                                   ECS020
03020      IF  W-TW-CHAR (W-TW-NDX) EQUAL SPACES                        ECS020
03021              AND                                                  ECS020
03022          H4T-CHAR (H4T-NDX) EQUAL SPACES                          ECS020
03023          NEXT SENTENCE                                            ECS020
03024                                                                   ECS020
03025      ELSE                                                         ECS020
03026          IF  W-TW-CHAR (W-TW-NDX) EQUAL ')'                       ECS020
03027                  AND                                              ECS020
03028              H4T-CHAR (H4T-NDX) EQUAL SPACES                      ECS020
03029              MOVE W-TW-CHAR (W-TW-NDX)                            ECS020
03030                                  TO H4T-CHAR (H4T-NDX)            ECS020
03031                                                                   ECS020
03032          ELSE                                                     ECS020
03033              SET H4T-NDX UP BY +1                                 ECS020
03034              MOVE W-TW-CHAR (W-TW-NDX)                            ECS020
03035                                  TO H4T-CHAR (H4T-NDX).           ECS020
03036                                                                   ECS020
03037  2620-EXIT.                                                       ECS020
03038      EXIT.                                                        ECS020
03039                                  EJECT                            ECS020
03040  2700-READ-AM-MSTR.                                               ECS020
03041      READ ACCT-MASTER.                                            ECS020
03042                                                                   ECS020
03043      IF AM-FILE-STATUS NOT = '00'                                 ECS020
03044          MOVE '1'                  TO WAC-1                       ECS020
03045          MOVE '4'                  TO WAC-2                       ECS020
03046          MOVE AM-FILE-STATUS       TO WAC-3-4                     ECS020
03047          MOVE WORK-ABEND-CODE      TO WS-RETURN-CODE              ECS020
unix  *        DISPLAY 'AM-KEY - ' AM-CONTROL-PRIMARY                   ECS020
unix  *        DISPLAY 'RET-CD - ' WS-RETURN-CODE                       ECS020
CIDMOD         GO TO 2700-EXIT.                                         ECS020
CIDMOD*        GO TO ABEND-PGM.                                         ECS020
03050                                                                   ECS020
03051  2700-EXIT.                                                       ECS020
03052      EXIT.                                                        ECS020
03053                                                                   ECS020
03054  2800-WRITE-LOSS-RATIO-MASTER.                                    ECS020
03055                                                                   ECS020
03056      WRITE LOSS-RATIO-EXTRACT  FROM  LOSS-RATIO-MASTER.           ECS020
03057                                                                   ECS020
03058  2899-EXIT.                                                       ECS020
03059      EXIT.                                                        ECS020
03060                                                                   ECS020
03061  2999-EXIT.                                                       ECS020
03062      EXIT.                                                        ECS020
03063                                                                   ECS020
03064  ABEND-PGM SECTION.                                               ECS020
03065                           COPY ELCABEND.                             CL*19
03066                                                                   ECS020
03067  9999-END-OF-JOB.                                                 ECS020
03068      CLOSE ACCT-MASTER                                            ECS020
03069            ELCNTL                                                 ECS020
03070            LOSS-RATIOS                                            ECS020
03071            PRINTER.                                                  CL*28
03072                                                                   ECS020
03073      IF AM-FILE-STATUS NOT = '00'                                 ECS020
03074          MOVE '1'                  TO WAC-1                       ECS020
03075          MOVE '2'                  TO WAC-2                       ECS020
03076          MOVE AM-FILE-STATUS       TO WAC-3-4                     ECS020
03077          MOVE WORK-ABEND-CODE      TO WS-RETURN-CODE              ECS020
03078          GO TO ABEND-PGM.                                         ECS020
03079                                                                   ECS020
03080      IF ELCNTL-FILE-STATUS NOT = '00'                             ECS020
03081          MOVE '**** ELCNTL CLOSE ERROR ****'                      ECS020
03082                                  TO WS-ABEND-MESSAGE              ECS020
03083          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS020
03084          GO TO ABEND-PGM.                                         ECS020
03085                                                                   ECS020
03086                                COPY ELCPRTC.                      ECS020
03087      GOBACK.                                                      ECS020
03088 /                                                                 ECS020
03089  LCP-WRITE-POS-PRT SECTION.                                       ECS020
03090      IF LCP-ASA = '+'                                             ECS020
03091          WRITE PRT AFTER 0 LINE                                   ECS020
03092      ELSE                                                         ECS020
03093      IF LCP-ASA = ' '                                             ECS020
03094          WRITE PRT AFTER ADVANCING 1 LINE                         ECS020
03095      ELSE                                                         ECS020
03096      IF LCP-ASA = '0'                                             ECS020
03097          WRITE PRT AFTER ADVANCING 2 LINE                         ECS020
03098      ELSE                                                         ECS020
03099      IF LCP-ASA = '-'                                             ECS020
03100          WRITE PRT AFTER ADVANCING 3 LINE                         ECS020
03101      ELSE                                                         ECS020
03102      IF LCP-ASA = '1'                                             ECS020
03103          WRITE PRT AFTER ADVANCING PAGE                           ECS020
03104      ELSE                                                         ECS020
03105      IF LCP-ASA = '2'                                             ECS020
03106          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS020
03107      ELSE                                                         ECS020
03108      IF LCP-ASA = '3'                                             ECS020
03109          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS020
03110      ELSE                                                         ECS020
03111      IF LCP-ASA = '4'                                             ECS020
03112          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS020
03113      ELSE                                                         ECS020
03114      IF LCP-ASA = '5'                                             ECS020
03115          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS020
03116      ELSE                                                         ECS020
03117      IF LCP-ASA = '6'                                             ECS020
03118          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS020
03119      ELSE                                                         ECS020
03120      IF LCP-ASA = '7'                                             ECS020
03121          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS020
03122      ELSE                                                         ECS020
03123      IF LCP-ASA = '8'                                             ECS020
03124          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS020
03125      ELSE                                                         ECS020
03126      IF LCP-ASA = '9'                                             ECS020
03127          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS020
03128      ELSE                                                         ECS020
03129      IF LCP-ASA = 'A'                                             ECS020
03130          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS020
03131      ELSE                                                         ECS020
03132      IF LCP-ASA = 'B'                                             ECS020
03133          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS020
03134      ELSE                                                         ECS020
03135      IF LCP-ASA = 'C'                                             ECS020
03136          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS020
03137      ELSE                                                         ECS020
03138      IF LCP-ASA = 'V'                                             ECS020
03139          WRITE PRT AFTER ADVANCING LCP-P01                        ECS020
03140      ELSE                                                         ECS020
03141      IF LCP-ASA = 'W'                                             ECS020
03142          WRITE PRT AFTER ADVANCING LCP-P02                        ECS020
03143      ELSE                                                         ECS020
03144      DISPLAY 'ASA CODE ERROR'.                                    ECS020
03145  LCP-WRITE-END-PRT.                                               ECS020
03146      EXIT.                                                        ECS020
