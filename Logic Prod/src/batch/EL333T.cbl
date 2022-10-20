00001  IDENTIFICATION DIVISION.                                         09/21/98
00002                                                                   EL333
00003  PROGRAM-ID.                 EL333 .                                 LV026
00004 *              PROGRAM CONVERTED BY                               EL333
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL333
00006 *              CONVERSION DATE 05/08/95 14:12:26.                 EL333
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL333
00008 *                            VMOD=2.013                              CL*26
00009                                                                   EL333
00010 *AUTHOR.     LOGIC INC.                                           EL333
00011 *            DALLAS, TEXAS.                                       EL333
00012                                                                   EL333
00013 *DATE-COMPILED.                                                   EL333
00014                                                                   EL333
00015 *SECURITY.   *****************************************************EL333
00016 *            *                                                   *EL333
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL333
00018 *            *                                                   *EL333
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL333
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL333
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL333
00022 *            *                                                   *EL333
00023 *            *****************************************************EL333
00024                                                                   EL333
00025 *REMARKS.                                                         EL333
00026 *       GENERAL FUNCTION IS TO PRODUCE A LIFE AND A&H CLAIMS      EL333
00027 *       PROFILE REPORT.                                           EL333
00028 *       CLAIM MASTER FILE IS SEQUENTIALLY PROCESSED, PRODUCING A  EL333
00029 *       SUMMARY OF STATISTICS .                                   EL333
00030 *       THE REPORT IS SUMMARIZED DEPENDING UPON OPTION SELECTED.  EL333
00031                                                                   EL333
00032 *       OPTIONS AVAILABLE.                                        EL333
00033 *                          1).  BY CARRIER                        EL333
00034 *                          2).  BY STATE                          EL333
00035 *                          3).  BY ACCOUNT                        EL333
00036 *                          4).  BY BENEFIT                        EL333
00037 *                          5).  BY USER                           EL333
00038 *                          6).  BY BENEFICIARY                    EL333
00039 *                                                                 EL333
00040 ******************************************************************EL333
00041 *   **** NOTE ****                                               *EL333
00042 *                                                                *EL333
00043 *      RIGHT HAND COLUMN IS ALWAYS INCEPTION-TO-DATE.            *EL333
00044 *      LEFT HAND COLUMN IS YEAR-TO-DATE, UNLESS OVERRIDDEN BY    *EL333
00045 *      PLACING BEGINNING DATE IN "RUN-DATE" AND ENDING DATE      *EL333
00046 *      IN "EP-DATE".                                             *EL333
00047 ******************************************************************EL333
00048                                                                   EL333
00049 *       INPUT FILES-             CLAIM HISTORY FILE               EL333
00050 *                                DATE CARD FILE                   EL333
00051                                                                   EL333
00052 *       WORK FILES-              SORT WORK FILES                  EL333
00053                                                                   EL333
00054 *       OUTPUT-                  PROFILE ANALYSIS REPORT          EL333
00055 *                                (FICH FILE)                      EL333
00056                                                                   EL333
00057      EJECT                                                        EL333
00058  ENVIRONMENT DIVISION.                                            EL333
00059  CONFIGURATION SECTION.                                           EL333
00060  INPUT-OUTPUT SECTION.                                            EL333
00061                                                                   EL333
00062  FILE-CONTROL.                                                    EL333
00063                                                                   EL333
00064      SELECT CLAIM-HIST  ASSIGN TO SYS010-UT-2400-S-SYS010.        EL333
00065                                                                   EL333
00066      SELECT PRNTR       ASSIGN TO SYS008-UR-1403-S-SYS008.        EL333
00067                                                                   EL333
00068      SELECT DISK-DATE   ASSIGN TO SYS019-FBA1-S-SYS019.           EL333
00069                                                                   EL333
00070      SELECT SORT-FILE   ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.       EL333
00071                                                                   EL333
00072      SELECT ELREPT      ASSIGN TO SYS012-FBA1-ELREPT              EL333
00073                         ORGANIZATION IS INDEXED                   EL333
00074                         ACCESS IS DYNAMIC                         EL333
00075                         RECORD KEY IS RF-CONTROL-PRIMARY          EL333
00076                         FILE STATUS IS DTE-VSAM-FLAGS.            EL333
00077                                                                   EL333
00078      SELECT FICH        ASSIGN TO SYS020-UT-2400-S-SYS020.        EL333
00079                                                                   EL333
00080  DATA DIVISION.                                                   EL333
00081                                                                   EL333
00082  FILE SECTION.                                                    EL333
00083                                                                   EL333
00084  FD  CLAIM-HIST             COPY ELCHAF.                          EL333
00085                                                                   EL333
00086  FD  PRNTR                                                        EL333
00087      RECORDING MODE IS F.                                         EL333
00088  01  PRT.                                                         EL333
00089      12  P-CTL               PIC  X.                              EL333
00090      12  P-DATA              PIC  X(132).                         EL333
00091                                                                   EL333
00092  FD  DISK-DATE              COPY ELCDTEFD.                        EL333
00093                                  EJECT                            EL333
00094  SD  SORT-FILE.                                                   EL333
00095  01  SORT-RECORD.                                                 EL333
00096      05  SR-CONTROL.                                              EL333
00097 *    SR-KEY WILL CONSIST OF                                       EL333
00098 *                CARRIER  OR                                      EL333
00099 *                STATE    OR                                      EL333
00100 *                ACCOUNT  OR                                      EL333
00101 *                BENEFIT  OR                                      EL333
00102 *                USER , DEPENDING UPON PROGRAM OPTION SELECTED.   EL333
00103          10  SR-KEY              PIC X(10).                       EL333
00104          10  SR-CLAIM-KEY        PIC X(20).                       EL333
00105 *   SR-REC-TYPE WILL CONSIST OF                                   EL333
00106 *                '1' FOR ELMSTR RECORD                            EL333
00107 *                '2' FOR ELCERT OR MPPLCY RECORD                  EL333
00108 *                '3' FOR ELTRLR RECORD                            EL333
00109                                                                   EL333
00110          10  SR-REC-TYPE         PIC X.                           EL333
00111          10  SR-SEQ-NO           PIC S9(4)  COMP.                 EL333
00112                                                                   EL333
00113      05  SR-REST-OF-RECORD.                                       EL333
00114          10  SR-RECORD-ID        PIC X(02).                       EL333
00115          10  FILLER              PIC X(1198).                     EL333
00116                                                                   EL333
00117      EJECT                                                        EL333
00118  FD  ELREPT                                                       EL333
00119                          COPY ELCRPTFD.                           EL333
00120                                                                   EL333
00121                          COPY ELCREPT.                            EL333
00122                                                                   EL333
00123  FD  FICH                                                         EL333
00124                          COPY ELCFCHFD.                           EL333
00125                                                                   EL333
00126      EJECT                                                        EL333
00127  WORKING-STORAGE SECTION.                                         EL333
00128                                                                   EL333
00129  77  FILLER  PIC X(32) VALUE '********************************'.  EL333
00130  77  FILLER  PIC X(32) VALUE '      EL333 WORKING-STORAGE     '.  EL333
00131  77  FILLER  PIC X(32) VALUE '******* VMOD=2.013 *************'.     CL*26
00132                                                                   EL333
00133  77  ESTABLISH-SW            PIC X       VALUE SPACES.            EL333
00134      88  ESTABLISHED-WITHIN-PERIOD       VALUE 'W'.               EL333
00135      88  ESTABLISHED-AFTER-PERIOD        VALUE 'A'.               EL333
00136      88  ESTABLISHED-BEFORE-PERIOD       VALUE 'B'.               EL333
00137                                                                   EL333
00138  77  CRITERIA-SW             PIC X       VALUE SPACES.            EL333
00139      88  CLAIM-WITHIN-SPECIFIED-RANGE    VALUE 'Y'.               EL333
00140                                                                   EL333
00141  77  RELEASE-SW              PIC X       VALUE 'Y'.                  CL*25
00142      88  RELEASE-TO-SORT                 VALUE 'Y'.                  CL**8
00143                                                                      CL**8
00144  77  EOF-SW                      PIC X       VALUE SPACES.        EL333
00145      88  END-OF-FILE                         VALUE 'Y'.           EL333
00146  77  FIRST-RECORD-SW             PIC X       VALUE SPACES.        EL333
00147      88  FIRST-RECORD                        VALUE SPACES.        EL333
00148  77  DISPLAY-CNT                 PIC ZZZZZ9.                      EL333
00149  77  DISPLAY-AMT                 PIC ZZ,ZZZ,ZZZ.99-.              EL333
00150  77  DETAIL-SUB                  PIC S9(3)   COMP-3 VALUE +0.     EL333
00151  77  HOLD-ODD-DAYS-OVER          PIC SV9(3) COMP-3 VALUE ZEROS.   EL333
00152  77  LAS-ST                      PIC XX     VALUE SPACES.         EL333
00153  77  LAS-CLM-TYPE                PIC X      VALUE SPACES.         EL333
00154  77  WS-FUTURE-CLAIMS            PIC 9(7)   VALUE ZEROS.          EL333
00155  77  IN-RECS                     PIC 9(7)   VALUE ZEROS.          EL333
00156  77  SEL-RECS                    PIC 9(7)   VALUE ZEROS.          EL333
00157  77  REL-CL-RECS                 PIC 9(7)   VALUE ZEROS.          EL333
00158  77  REL-CR-RECS                 PIC 9(7)   VALUE ZEROS.          EL333
00159  77  REL-PM-RECS                 PIC 9(7)   VALUE ZEROS.          EL333
00160  77  REL-TR-RECS                 PIC 9(7)   VALUE ZEROS.          EL333
00161  77  RET-RECS                    PIC 9(7)   VALUE ZEROS.          EL333
00162  77  RET-CL-RECS                 PIC 9(7)   VALUE ZEROS.          EL333
00163  77  RET-CR-RECS                 PIC 9(7)   VALUE ZEROS.          EL333
00164  77  RET-PM-RECS                 PIC 9(7)   VALUE ZEROS.          EL333
00165  77  RET-TR-RECS                 PIC 9(7)   VALUE ZEROS.          EL333
00166                                                                   EL333
00167  77  BLANK-RECORD                PIC X(800) VALUE SPACES.         EL333
00168      EJECT                                                        EL333
00169 ******************************************************************EL333
00170 *   ***** NOTE *****                                             *EL333
00171 *      IF ANY FIELDS ARE ADDED TO REPORT TABLE, FOUR STEPS MUST  *EL333
00172 *      BE TAKEN IN ADDITION TO TABLE ADDITIONS.                  *EL333
00173 *         1).  ADD THE SAME AMOUNT OF FIELDS TO INITIALIZE-TABLE *EL333
00174 *         2).  INCREASE FIELD 'BLANK-RECORD' BY THE SAME AMOUNT  *EL333
00175 *                  OF BYTES                                      *EL333
00176 *         3).  ADD THE SAME FIELDS TO GRAND-TOTAL-RECORD.        *EL333
00177 *         4).  ADD THE FIELD DESCRIPTION(S) TO                   *EL333
00178 *                  PRINT-DESCRIPTION-TABLE.                      *   CL**5
00179 ******************************************************************EL333
00180                                                                   EL333
00181  01  REPORT-TABLE-RECORD.                                         EL333
00182      05  REST-OF-RECORD OCCURS 4 TIMES INDEXED BY REPORT-INDEX.   EL333
00183 *  FIRST  OCCURRENCE  LIFE RECORD FOR PERIOD                      EL333
00184 *  SECOND OCCURRENCE  A&H  RECORD FOR PERIOD                      EL333
00185 *  THIRD  OCCURRENCE  LIFE RECORD SINCE INCEPTION                 EL333
00186 *  FOURTH OCCURRENCE  A&H  RECORD SINCE INCEPTION                 EL333
00187 *     DURATIONS                                                   EL333
00188          10  TB-CNT-INC-RPT        PIC S9(7)      COMP-3.         EL333
00189          10  TB-CNT-RPT-EST        PIC S9(7)      COMP-3.         EL333
00190          10  TB-CNT-RPT-FRST-PMT   PIC S9(7)      COMP-3.         EL333
00191          10  TB-CNT-RPT-FRST-ACT   PIC S9(7)      COMP-3.         EL333
00192          10  TB-CNT-INC-LST-PMT    PIC S9(7)      COMP-3.         EL333
00193          10  TB-CNT-EFF-INC        PIC S9(7)      COMP-3.         EL333
00194          10  TB-CNT-LETR-RESP      PIC S9(7)      COMP-3.         EL333
00195          10  TB-CNT-AUTO-BEG-END   PIC S9(7)      COMP-3.         EL333
00196          10  TB-INC-RPT            PIC S9(7)V99   COMP-3.         EL333
00197          10  TB-RPT-EST            PIC S9(7)V99   COMP-3.         EL333
00198          10  TB-RPT-FRST-PMT       PIC S9(7)V99   COMP-3.         EL333
00199          10  TB-RPT-FRST-ACT       PIC S9(7)V99   COMP-3.         EL333
00200          10  TB-INC-LST-PMT        PIC S9(7)V99   COMP-3.         EL333
00201          10  TB-EFF-INC            PIC S9(7)V99   COMP-3.         EL333
00202          10  TB-LETR-RESP          PIC S9(7)V99   COMP-3.         EL333
00203          10  TB-AUTO-BEG-END       PIC S9(7)V99   COMP-3.         EL333
00204 *     VOLUMES                                                     EL333
00205          10  TB-EXST-OPEN          PIC S9(7)      COMP-3.         EL333
00206          10  TB-NOW-OPEN           PIC S9(7)      COMP-3.         EL333
00207          10  TB-REOPENED           PIC S9(7)      COMP-3.         EL333
00208          10  TB-REMAIN-REOPEN      PIC S9(7)      COMP-3.         EL333
00209          10  TB-NEW-CLAIMS         PIC S9(7)      COMP-3.         EL333
00210          10  TB-NEW-CLOSED         PIC S9(7)      COMP-3.         EL333
00211          10  TB-CLO-CLAIMS         PIC S9(7)      COMP-3.         EL333
00212          10  TB-AUTO-CLOSE         PIC S9(7)      COMP-3.         EL333
00213          10  TB-PAID-CLOSE         PIC S9(7)      COMP-3.         EL333
00214          10  TB-UNPAID-CLOSE       PIC S9(7)      COMP-3.         EL333
00215          10  TB-BENEFITS-CHANGE    PIC S9(7)      COMP-3.         EL333
00216          10  TB-SETUP-ERROR        PIC S9(7)      COMP-3.         EL333
00217          10  TB-DEN-CLAIMS         PIC S9(7)      COMP-3.         EL333
00218          10  TB-OPEN-NO-COV        PIC S9(7)      COMP-3.         EL333
00219          10  TB-FRST-PMTS          PIC S9(7)      COMP-3.         EL333
00220          10  TB-CONT-PMTS          PIC S9(7)      COMP-3.         EL333
00221          10  TB-AUTO-PMTS          PIC S9(7)      COMP-3.         EL333
00222          10  TB-FINAL-PMTS         PIC S9(7)      COMP-3.         EL333
00223          10  TB-LETRS-SENT         PIC S9(7)      COMP-3.         EL333
00224          10  TB-ACT-REC            PIC S9(7)      COMP-3.         EL333
00225 *     AVERAGES                                                    EL333
00226          10  TB-CNT-ESTATE-PMTS    PIC S9(7)      COMP-3.         EL333
00227          10  TB-CNT-BENE-PMTS      PIC S9(7)      COMP-3.         EL333
00228          10  TB-CNT-CLAIM-PMTS     PIC S9(7)      COMP-3.         EL333
00229          10  TB-CNT-BENEFIT        PIC S9(7)      COMP-3.         EL333
00230          10  TB-ESTATE-PMTS        PIC S9(7)      COMP-3.         EL333
00231          10  TB-BENE-PMTS          PIC S9(7)      COMP-3.         EL333
00232          10  TB-CLAIM-PMTS         PIC S9(9)      COMP-3.         EL333
00233          10  TB-BENEFIT            PIC S9(9)V99   COMP-3.         EL333
00234          10  TB-TERM               PIC S9(7)      COMP-3.         EL333
00235          10  TB-AGE                PIC S9(7)      COMP-3.         EL333
00236          10  TB-DAYS-PMT           PIC S9(7)      COMP-3.         EL333
00237                                                                   EL333
00238  01  GRAND-TOTAL-RECORD.                                          EL333
00239      05  GRAND-RECORD OCCURS 4 TIMES INDEXED BY GRAND-INDEX.      EL333
00240 *  FIRST  OCCURRENCE  LIFE RECORD FOR PERIOD      GRAND TOTALS    EL333
00241 *  SECOND OCCURRENCE  A&H  RECORD FOR PERIOD      GRAND TOTALS    EL333
00242 *  THIRD  OCCURRENCE  LIFE RECORD SINCE INCEPTION GRAND TOTALS    EL333
00243 *  FOURTH OCCURRENCE  A&H  RECORD SINCE INCEPTION GRAND-TOTALS    EL333
00244 *     DURATIONS                                                   EL333
00245          10  GR-CNT-INC-RPT        PIC S9(7)      COMP-3.         EL333
00246          10  GR-CNT-RPT-EST        PIC S9(7)      COMP-3.         EL333
00247          10  GR-CNT-RPT-FRST-PMT   PIC S9(7)      COMP-3.         EL333
00248          10  GR-CNT-RPT-FRST-ACT   PIC S9(7)      COMP-3.         EL333
00249          10  GR-CNT-INC-LST-PMT    PIC S9(7)      COMP-3.         EL333
00250          10  GR-CNT-EFF-INC        PIC S9(7)      COMP-3.         EL333
00251          10  GR-CNT-LETR-RESP      PIC S9(7)      COMP-3.         EL333
00252          10  GR-CNT-AUTO-BEG-END   PIC S9(7)      COMP-3.         EL333
00253          10  GR-INC-RPT            PIC S9(7)V99   COMP-3.         EL333
00254          10  GR-RPT-EST            PIC S9(7)V99   COMP-3.         EL333
00255          10  GR-RPT-FRST-PMT       PIC S9(7)V99   COMP-3.         EL333
00256          10  GR-RPT-FRST-ACT       PIC S9(7)V99   COMP-3.         EL333
00257          10  GR-INC-LST-PMT        PIC S9(7)V99   COMP-3.         EL333
00258          10  GR-EFF-INC            PIC S9(7)V99   COMP-3.         EL333
00259          10  GR-LETR-RESP          PIC S9(7)V99   COMP-3.         EL333
00260          10  GR-AUTO-BEG-END       PIC S9(7)V99   COMP-3.         EL333
00261 *     VOLUMES                                                     EL333
00262          10  GR-EXST-OPEN          PIC S9(7)      COMP-3.         EL333
00263          10  GR-NOW-OPEN           PIC S9(7)      COMP-3.         EL333
00264          10  GR-REOPENED           PIC S9(7)      COMP-3.         EL333
00265          10  GR-REMAIN-REOPEN      PIC S9(7)      COMP-3.         EL333
00266          10  GR-NEW-CLAIMS         PIC S9(7)      COMP-3.         EL333
00267          10  GR-NEW-CLOSED         PIC S9(7)      COMP-3.         EL333
00268          10  GR-CLO-CLAIMS         PIC S9(7)      COMP-3.         EL333
00269          10  GR-AUTO-CLOSE         PIC S9(7)      COMP-3.         EL333
00270          10  GR-PAID-CLOSE         PIC S9(7)      COMP-3.         EL333
00271          10  GR-UNPAID-CLOSE       PIC S9(7)      COMP-3.         EL333
00272          10  GR-BENEFITS-CHANGE    PIC S9(7)      COMP-3.         EL333
00273          10  GR-SETUP-ERROR        PIC S9(7)      COMP-3.         EL333
00274          10  GR-DEN-CLAIMS         PIC S9(7)      COMP-3.         EL333
00275          10  GR-OPEN-NO-COV        PIC S9(7)      COMP-3.         EL333
00276          10  GR-FRST-PMTS          PIC S9(7)      COMP-3.         EL333
00277          10  GR-CONT-PMTS          PIC S9(7)      COMP-3.         EL333
00278          10  GR-AUTO-PMTS          PIC S9(7)      COMP-3.         EL333
00279          10  GR-FINAL-PMTS         PIC S9(7)      COMP-3.         EL333
00280          10  GR-LETRS-SENT         PIC S9(7)      COMP-3.         EL333
00281          10  GR-ACT-REC            PIC S9(7)      COMP-3.         EL333
00282 *     AVERAGES                                                    EL333
00283          10  GR-CNT-ESTATE-PMTS    PIC S9(7)      COMP-3.         EL333
00284          10  GR-CNT-BENE-PMTS      PIC S9(7)      COMP-3.         EL333
00285          10  GR-CNT-CLAIM-PMTS     PIC S9(7)      COMP-3.         EL333
00286          10  GR-CNT-BENEFIT        PIC S9(7)      COMP-3.         EL333
00287          10  GR-ESTATE-PMTS        PIC S9(7)      COMP-3.         EL333
00288          10  GR-BENE-PMTS          PIC S9(7)      COMP-3.         EL333
00289          10  GR-CLAIM-PMTS         PIC S9(9)      COMP-3.         EL333
00290          10  GR-BENEFIT            PIC S9(9)V99   COMP-3.         EL333
00291          10  GR-TERM               PIC S9(7)      COMP-3.         EL333
00292          10  GR-AGE                PIC S9(7)      COMP-3.         EL333
00293          10  GR-DAYS-PMT           PIC S9(7)      COMP-3.         EL333
00294                                                                   EL333
00295  01  INITIALIZE-TABLE.                                            EL333
00296      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00297      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00298      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00299      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00300      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00301      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00302      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00303      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00304      05  FILLER                PIC S9(7)V99   COMP-3 VALUE +0.    EL333
00305      05  FILLER                PIC S9(7)V99   COMP-3 VALUE +0.    EL333
00306      05  FILLER                PIC S9(7)V99   COMP-3 VALUE +0.    EL333
00307      05  FILLER                PIC S9(7)V99   COMP-3 VALUE +0.    EL333
00308      05  FILLER                PIC S9(7)V99   COMP-3 VALUE +0.    EL333
00309      05  FILLER                PIC S9(7)V99   COMP-3 VALUE +0.    EL333
00310      05  FILLER                PIC S9(7)V99   COMP-3 VALUE +0.    EL333
00311      05  FILLER                PIC S9(7)V99   COMP-3 VALUE +0.    EL333
00312      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00313      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00314      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00315      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00316      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00317      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00318      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00319      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00320      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00321      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00322      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00323      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00324      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00325      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00326      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00327      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00328      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00329      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00330      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00331      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00332      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00333      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00334      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00335      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00336      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00337      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00338      05  FILLER                PIC S9(9)   COMP-3 VALUE +0.       EL333
00339      05  FILLER                PIC S9(9)V99   COMP-3 VALUE +0.    EL333
00340      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00341      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00342      05  FILLER                PIC S9(7)   COMP-3 VALUE +0.       EL333
00343                                                                   EL333
00344  01  FILLER                          COMP-3.                      EL333
00345      05  WS-CURRENT-PMT-CNTR         PIC S9(5)       VALUE +0.    EL333
00346      05  WS-ITD-PMT-CNTR             PIC S9(5)       VALUE +0.    EL333
00347      05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  EL333
00348      05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   EL333
00349      05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +75.   EL333
00350      05  WS-ZERO                     PIC S9          VALUE ZERO.  EL333
00351      05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  EL333
00352      05  WS-TOT-RECORDS-RELEASED     PIC S9(7)       VALUE ZERO.  EL333
00353                                                                   EL333
00354      EJECT                                                        EL333
00355  01  FILLER.                                                      EL333
00356      05  X                       PIC X      VALUE SPACES.         EL333
00357      05  LAST-SR-KEY             PIC X(10)  VALUE SPACES.         EL333
00358      05  LAST-CONTROL-PRIMARY    PIC X(20)  VALUE SPACES.         EL333
00359      05  HOLD-CONTROL.                                            EL333
00360          10  HOLD-KEY.                                            EL333
00361              15  FILLER          PIC X(9).                        EL333
00362              15  HOLD-LF-AH      PIC X.                           EL333
00363          10  HOLD-REC-TYPE       PIC X      VALUE SPACES.         EL333
00364      05  LAST-RECORD-TYPE        PIC XX     VALUE SPACES.         EL333
00365      05  LAST-REPORTED-DATE      PIC XX     VALUE LOW-VALUES.     EL333
00366      05  HOLD-FIRST-ACTION-DATE  PIC XX.                          EL333
00367      05  HOLD-FIRST-PMT-DATE     PIC XX.                          EL333
00368      05  BEGIN-BIN-DATE          PIC XX.                          EL333
00369      05  END-BIN-DATE            PIC XX.                          EL333
00370      05  PERIOD-BEGIN-BIN-DATE   PIC XX.                             CL*12
00371      05  BEGIN-YTD-BIN-DATE      PIC XX.                             CL*12
00372      05  END-YTD-BIN-DATE        PIC XX.                             CL*12
00373      05  PERIOD-BEGIN-DATE.                                          CL*12
00374          10  PERIOD-BEGIN-MO     PIC 9(2).                           CL*12
00375          10  PERIOD-BEGIN-DA     PIC 9(2).                           CL*12
00376          10  PERIOD-BEGIN-YR     PIC 9(2).                           CL*12
00377      05  PERIOD-END-DATE.                                            CL*12
00378          10  PERIOD-END-MO       PIC 9(2).                           CL*12
00379          10  PERIOD-END-DA       PIC 9(2).                           CL*12
00380          10  PERIOD-END-YR       PIC 9(2).                           CL*12
00381 *    05  YTD-FROM-DATE.                                              CL*14
00382 *        10  YTD-FROM-MO         PIC 9(2).                           CL*14
00383 *        10  YTD-FROM-DA         PIC 9(2).                           CL*14
00384 *        10  YTD-FROM-YR         PIC 9(2).                           CL*14
00385      05  YTD-THRU-DATE.                                              CL*12
00386          10  YTD-THRU-MO         PIC 9(2).                           CL*12
00387          10  YTD-THRU-DA         PIC 9(2).                           CL*12
00388          10  YTD-THRU-YR         PIC 9(2).                           CL*12
00389                                                                      CL*12
00390      05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL333
00391      05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL333
00392                                                                   EL333
00393      05  WS-SAVE-PRINT-RECORD        PIC X(133) VALUE SPACES.     EL333
00394                                                                   EL333
00395      05  PGM-SUB           PIC S9(4) COMP  VALUE +333.            EL333
00396      05  ABEND-CODE        PIC XXXX  VALUE SPACES.                EL333
00397      05  ABEND-OPTION      PIC X     VALUE SPACES.                EL333
00398      05  OLC-REPORT-NAME   PIC X(5)  VALUE 'EL333'.               EL333
00399                                                                   EL333
00400      05  WORK-MONTH-DAY          PIC 9(7)V99    VALUE ZEROS.      EL333
00401      05  RED-WORK-MONTH-DAY REDEFINES WORK-MONTH-DAY.             EL333
00402         10  WORK-MONTH           PIC 9(7).                        EL333
00403         10  WORK-DAY             PIC V9(2).                       EL333
00404                                                                   EL333
00405      05  DENIED-STATUS           PIC X(01)      VALUE SPACES.     EL333
00406                                                                   EL333
00407      EJECT                                                        EL333
00408                                  COPY ELCCERT.                    EL333
00409                                                                   EL333
00410      EJECT                                                        EL333
00411                                  COPY MPCPLCY.                    EL333
00412                                                                   EL333
00413      EJECT                                                        EL333
00414                                  COPY ELCARCH.                    EL333
00415                                                                   EL333
00416      EJECT                                                        EL333
00417                                  COPY ELCMSTR.                    EL333
00418                                                                   EL333
00419      EJECT                                                        EL333
00420                                  COPY ELCTRLR.                    EL333
00421                                                                   EL333
00422      EJECT                                                        EL333
00423  01  PRINT-DESCRIPTION-TABLE.                                     EL333
00424      05  PRINT-TABLE.                                             EL333
00425          10  FILLER              PIC X(30)  VALUE                 EL333
00426              ' INCURRED TO REPORTED'.                             EL333
00427          10  FILLER              PIC X(30)  VALUE                 EL333
00428              ' REPORTED TO ESTABLISHED'.                          EL333
00429          10  FILLER              PIC X(30)  VALUE                 EL333
00430              ' REPORTED TO FIRST ACTION'.                         EL333
00431          10  FILLER              PIC X(30)  VALUE                 EL333
00432              ' REPORTED TO FIRST PAYMENT'.                        EL333
00433          10  FILLER              PIC X(30)  VALUE                 EL333
00434              ' INCURRED TO PAID THRU DATE'.                       EL333
00435          10  FILLER              PIC X(30)  VALUE                 EL333
00436              ' EFFECTIVE TO INCURRED'.                            EL333
00437          10  FILLER              PIC X(30)  VALUE                 EL333
00438              ' LETTERS SENT TO RECEIVED'.                         EL333
00439          10  FILLER              PIC X(30)  VALUE                 EL333
00440              ' AUTO PAY START TO END'.                            EL333
00441          10  FILLER.                                              EL333
00442              15  FILLER          PIC X(12)  VALUE ' OPEN AS OF'.  EL333
00443              15  BEGIN-DT        PIC X(8)   VALUE SPACES.         EL333
00444              15  FILLER          PIC X(10)  VALUE SPACES.         EL333
00445          10  FILLER.                                              EL333
00446              15  FILLER          PIC X(12)  VALUE ' OPEN AS OF'.  EL333
00447              15  END-DT          PIC X(8)   VALUE SPACES.         EL333
00448              15  FILLER          PIC X(10)  VALUE SPACES.         EL333
00449          10  FILLER              PIC X(30)  VALUE                 EL333
00450              ' REOPENED CLAIMS'.                                  EL333
00451          10  FILLER              PIC X(30)  VALUE                 EL333
00452              '    REMAINED OPEN'.                                 EL333
00453          10  FILLER              PIC X(30)  VALUE                 EL333
00454              ' NEW CLAIMS'.                                       EL333
00455          10  FILLER              PIC X(30)  VALUE                 EL333
00456              '    NEW/CLOSED'.                                    EL333
00457          10  FILLER              PIC X(30)  VALUE                 EL333
00458              ' CLOSED CLAIMS'.                                    EL333
00459          10  FILLER              PIC X(30)  VALUE                 EL333
00460              '    AUTO CLOSINGS'.                                 EL333
00461          10  FILLER              PIC X(30)  VALUE                 EL333
00462              '    CLOSED AND PAID'.                               EL333
00463          10  FILLER              PIC X(30)  VALUE                 EL333
00464              '    CLOSED AND UNPAID'.                             EL333
00465          10  FILLER              PIC X(30)  VALUE                 EL333
00466              ' DENIED CLAIMS'.                                    EL333
00467          10  FILLER              PIC X(30)  VALUE                 EL333
00468              '    BENEFIT CHANGE'.                                EL333
00469          10  FILLER              PIC X(30)  VALUE                 EL333
00470              '    CLOSED DUE TO SET-UP ERROR'.                    EL333
00471          10  FILLER              PIC X(30)  VALUE                 EL333
00472              ' OPEN WITH NO COVERAGE'.                            EL333
00473          10  FILLER              PIC X(30)  VALUE                 EL333
00474              ' FIRST PAYMENTS'.                                   EL333
00475          10  FILLER              PIC X(30)  VALUE                 EL333
00476              ' CONTINUING PAYMENTS'.                              EL333
00477          10  FILLER              PIC X(30)  VALUE                 EL333
00478              ' FINAL PAYMENTS'.                                   EL333
00479          10  FILLER              PIC X(30)  VALUE                 EL333
00480              ' AUTO PAYMENTS'.                                    EL333
00481          10  FILLER              PIC X(30)  VALUE                 EL333
00482              ' LETTERS SENT'.                                     EL333
00483          10  FILLER              PIC X(30)  VALUE                 EL333
00484              ' ACTIVITIES RECORDED'.                              EL333
00485          10  FILLER              PIC X(30)  VALUE                 EL333
00486              ' PAYMENTS TO BENEFICIARY'.                          EL333
00487          10  FILLER              PIC X(30)  VALUE                 EL333
00488              ' CLAIM PAYMENT'.                                    EL333
00489          10  FILLER              PIC X(30)  VALUE                 EL333
00490              ' ORIGINAL BENEFIT'.                                 EL333
00491          10  FILLER              PIC X(30)  VALUE                 EL333
00492              ' TERM'.                                             EL333
00493          10  FILLER              PIC X(30)  VALUE                 EL333
00494              ' AGE'.                                              EL333
00495          10  FILLER              PIC X(30)  VALUE                 EL333
00496              ' DAYS PER PAYMENT'.                                 EL333
00497          10  FILLER              PIC X(30)  VALUE                 EL333
00498              ' PAYMENTS PER CLAIM'.                               EL333
00499          10  FILLER              PIC X(30)  VALUE                 EL333
00500              ' TOTAL PAID'.                                       EL333
00501          10  FILLER              PIC X(30)  VALUE                 EL333
00502              ' BENEFICIARY PAYMENTS'.                             EL333
00503          10  FILLER              PIC X(30)  VALUE                 EL333
00504              ' ESTATE PAYMENTS'.                                  EL333
00505      05  PRINT-TABLER REDEFINES PRINT-TABLE.                      EL333
00506          10  PRINT-DESCRIPTION   PIC X(30)  OCCURS 38 TIMES       EL333
00507                                  INDEXED BY PRINT-INDEX.          EL333
00508                                                                   EL333
00509  01  TOTAL-VALUE-MESSAGES.                                        EL333
00510      05  TVM-LIFE-MSG1.                                           EL333
00511          10  FILLER              PIC X(8)   VALUE                 EL333
00512              'INVALID'.                                           EL333
00513          10  TVM-LIFE-L2-MSG1    PIC XX     VALUE  SPACES.        EL333
00514          10  FILLER              PIC X(10)  VALUE                 EL333
00515              ' BENEFIT'.                                          EL333
00516      05  TVM-LIFE-MSG2.                                           EL333
00517          10  FILLER              PIC X(3)   VALUE                 EL333
00518              'NO '.                                               EL333
00519          10  TVM-LIFE-L2-MSG2    PIC XX     VALUE  SPACES.        EL333
00520          10  FILLER              PIC X(15)  VALUE                 EL333
00521              ' COVERAGE'.                                         EL333
00522      05  TVM-AH-MSG1.                                             EL333
00523          10  FILLER              PIC X(8)   VALUE                 EL333
00524              'INVALID'.                                           EL333
00525          10  TVM-AH-L2-MSG1      PIC XX     VALUE  SPACES.        EL333
00526          10  FILLER              PIC X(10)  VALUE                 EL333
00527              ' BENEFIT'.                                          EL333
00528      05  TVM-AH-MSG2.                                             EL333
00529          10  FILLER              PIC X(3)   VALUE                 EL333
00530              'NO '.                                               EL333
00531          10  TVM-AH-L2-MSG2      PIC XX     VALUE  SPACES.        EL333
00532          10  FILLER              PIC X(15)  VALUE                 EL333
00533              ' COVERAGE'.                                         EL333
00534                                                                   EL333
00535 ***      COMMON HEADING LINES     ********************************EL333
00536  01  WS-HEADING1.                                                 EL333
00537      05  FILLER             PIC X(51) VALUE '1'.                  EL333
00538      05  FILLER             PIC X(29) VALUE                       EL333
00539          'CLAIM PROFILE ANALYSIS REPORT'.                         EL333
00540      05  FILLER             PIC X(39) VALUE SPACES.               EL333
00541      05  WS-H1-RPT          PIC X(08) VALUE 'EL333'.                 CL*12
00542                                                                   EL333
00543  01  WS-HEADING2.                                                 EL333
00544      05  FILLER             PIC X(51) VALUE SPACES.               EL333
00545      05  WS-H2-CLIENT-NAME  PIC X(30) VALUE SPACES.               EL333
00546      05  FILLER             PIC X(38) VALUE SPACES.               EL333
00547      05  WS-H2-DATE         PIC X(8).                             EL333
00548                                                                   EL333
00549  01  WS-HEADING3.                                                 EL333
00550      05  FILLER             PIC X(57) VALUE SPACES.               EL333
00551      05  WS-H3-DATE         PIC X(18).                            EL333
00552      05  FILLER             PIC X(44) VALUE SPACES.               EL333
00553      05  FILLER             PIC X(4) VALUE 'PAGE'.                EL333
00554      05  WS-H3-PAGE         PIC ZZZ9.                             EL333
00555                                                                   EL333
00556  01  WS-HEADING4.                                                 EL333
00557      05  FILLER                  PIC X(3) VALUE SPACES.           EL333
00558      05  TOTAL-DESCRIPTION       PIC X(22).                       EL333
00559      05  TOTAL-DESC              PIC X(6).                        EL333
00560      05  FILLER                  PIC X(5)   VALUE SPACES.         EL333
00561      05  TOTAL-VALUE             PIC X(20)  VALUE SPACES.         EL333
00562                                                                   EL333
00563  01  WS-HEADING5            PIC X(133).                              CL*12
00564                                                                      CL*12
00565  01  WS-HEADING5A.                                                   CL*12
00566      05  FILLER             PIC X(25) VALUE  '0'.                 EL333
00567      05  FILLER             PIC X(10) VALUE                       EL333
00568          '-------   '.                                            EL333
00569      05  FILLER                  PIC X(5)   VALUE 'FROM '.        EL333
00570      05  BEGIN-RPT-A-DATE.                                           CL*16
00571          10  BEGIN-RPT-A-MO      PIC XX.                             CL*12
00572          10  FILLER              PIC X      VALUE '/'.            EL333
00573          10  BEGIN-RPT-A-DA      PIC XX.                             CL*12
00574          10  FILLER              PIC X      VALUE '/'.            EL333
00575          10  BEGIN-RPT-A-YR      PIC XX.                             CL*12
00576      05  FILLER                  PIC X(6)   VALUE ' THRU '.       EL333
00577      05  END-RPT-A-DATE.                                             CL*12
00578          10  END-RPT-A-MO        PIC XX.                             CL*12
00579          10  FILLER              PIC X      VALUE '/'.            EL333
00580          10  END-RPT-A-DA        PIC XX.                             CL*12
00581          10  FILLER              PIC X      VALUE '/'.            EL333
00582          10  END-RPT-A-YR        PIC XX.                             CL*12
00583      05  FILLER             PIC X(10) VALUE                       EL333
00584          '  ------  '.                                            EL333
00585      05  FILLER             PIC X(59) VALUE                       EL333
00586          ' --------  S I N C E    I N C E P T I O N  ---------'.  EL333
00587                                                                   EL333
00588  01  WS-HEADING5B.                                                   CL*12
00589      05  FILLER             PIC X(25) VALUE  '0'.                    CL*12
00590      05  FILLER             PIC X(06) VALUE                          CL*12
00591          '----  '.                                                   CL*12
00592      05  FILLER                  PIC X(5)   VALUE 'FROM '.           CL*12
00593      05  BEGIN-RPT-B-DATE.                                           CL*17
00594          10  BEGIN-RPT-B-MO      PIC XX.                             CL*12
00595          10  FILLER              PIC X      VALUE '/'.               CL*12
00596          10  BEGIN-RPT-B-DA      PIC XX.                             CL*12
00597          10  FILLER              PIC X      VALUE '/'.               CL*12
00598          10  BEGIN-RPT-B-YR      PIC XX.                             CL*12
00599      05  FILLER                  PIC X(6)   VALUE ' THRU '.          CL*12
00600      05  END-RPT-B-DATE.                                             CL*12
00601          10  END-RPT-B-MO        PIC XX.                             CL*12
00602          10  FILLER              PIC X      VALUE '/'.               CL*12
00603          10  END-RPT-B-DA        PIC XX.                             CL*12
00604          10  FILLER              PIC X      VALUE '/'.               CL*12
00605          10  END-RPT-B-YR        PIC XX.                             CL*12
00606      05  FILLER             PIC X(06) VALUE                          CL*12
00607          '  ----'.                                                   CL*12
00608      05  FILLER             PIC X(02) VALUE SPACES.                  CL*12
00609      05  FILLER             PIC X(12) VALUE                          CL*12
00610          ' ------ YTD '.                                             CL*12
00611      05  YTD-FROM-RPT-B-DATE.                                        CL*12
00612          10  FROM-RPT-B-MO       PIC XX.                             CL*17
00613          10  FILLER              PIC X      VALUE '/'.               CL*12
00614          10  FROM-RPT-B-DA       PIC XX.                             CL*17
00615          10  FILLER              PIC X      VALUE '/'.               CL*12
00616          10  FROM-RPT-B-YR       PIC XX.                             CL*17
00617      05  FILLER             PIC X(6) VALUE ' THRU '.                 CL*12
00618      05  YTD-THRU-RPT-B-DATE.                                        CL*12
00619          10  THRU-RPT-B-MO       PIC XX.                             CL*17
00620          10  FILLER              PIC X      VALUE '/'.               CL*12
00621          10  THRU-RPT-B-DA       PIC XX.                             CL*17
00622          10  FILLER              PIC X      VALUE '/'.               CL*12
00623          10  THRU-RPT-B-YR       PIC XX.                             CL*17
00624      05  FILLER             PIC X(21) VALUE                          CL*12
00625          ' ------    '.                                              CL*12
00626                                                                      CL*12
00627  01  WS-HEADING6.                                                 EL333
00628      05  FILLER             PIC X(40)  VALUE SPACES.              EL333
00629      05  WS-H6-LF1          PIC X(6)   VALUE SPACES.              EL333
00630      05  FILLER             PIC X(15)  VALUE SPACES.              EL333
00631      05  WS-H6-AH1          PIC X(6)   VALUE SPACES.              EL333
00632      05  FILLER             PIC X(15)  VALUE SPACES.              EL333
00633      05  WS-H6-LF2          PIC X(6)   VALUE SPACES.              EL333
00634      05  FILLER             PIC X(16)  VALUE SPACES.              EL333
00635      05  WS-H6-AH2          PIC X(6)   VALUE SPACES.              EL333
00636                                                                   EL333
00637  01  WS-SUB-HEADING1.                                             EL333
00638      05  FILLER                  PIC XX     VALUE SPACES.         EL333
00639      05  FILLER                  PIC X(19)  VALUE                 EL333
00640          '**** DURATIONS ****'.                                   EL333
00641      05  FILLER                  PIC X(16)  VALUE SPACES.         EL333
00642      05  FILLER                  PIC X(11)  VALUE 'MONTHS DAYS'.  EL333
00643      05  FILLER                  PIC X(10)  VALUE SPACES.         EL333
00644      05  FILLER                  PIC X(11)  VALUE 'MONTHS DAYS'.  EL333
00645      05  FILLER                  PIC X(10)  VALUE SPACES.         EL333
00646      05  FILLER                  PIC X(11)  VALUE 'MONTHS DAYS'.  EL333
00647      05  FILLER                  PIC X(10)  VALUE SPACES.         EL333
00648      05  FILLER                  PIC X(11)  VALUE 'MONTHS DAYS'.  EL333
00649                                                                   EL333
00650  01  WS-SUB-HEADING2.                                             EL333
00651      05  FILLER             PIC XX   VALUE SPACE.                 EL333
00652      05  FILLER                  PIC X(16) VALUE                  EL333
00653          '**** COUNTS ****'.                                      EL333
00654                                                                   EL333
00655  01  WS-SUB-HEADING3.                                             EL333
00656      05  FILLER             PIC XX   VALUE SPACE.                 EL333
00657      05  FILLER                  PIC X(18) VALUE                  EL333
00658          '**** AVERAGES ****'.                                    EL333
00659                                                                   EL333
00660  01  WS-DETAIL1.                                                  EL333
00661      05  FILLER             PIC XX   VALUE SPACES.                EL333
00662      05  P-DESCRIPTION      PIC X(30).                            EL333
00663      05  FILLER             PIC XX   VALUE SPACES.                EL333
00664      05  P-AMOUNT-X.                                              EL333
00665          10  P-AMT-EX OCCURS 4 TIMES.                             EL333
00666              15  P-AMT-X                PIC ZZZZZZ99B99.          EL333
00667              15  FILLER                 PIC X(10).                EL333
00668      05  P-AMOUNT-Z REDEFINES P-AMOUNT-X.                         EL333
00669          10  P-AMT-ZEE OCCURS 4 TIMES.                            EL333
00670              15  P-AMT-Z                PIC ZZZ,ZZZ,Z99.          EL333
00671              15  FILLER                 PIC X(10).                EL333
00672      05  P-AMOUNT-9 REDEFINES P-AMOUNT-X.                         EL333
00673          10  P-AMT-NINE OCCURS 4 TIMES.                           EL333
00674              15  P-AMT-9                PIC ZZZZ,ZZZ.99.          EL333
00675              15  FILLER                 PIC X(10).                EL333
00676                                                                   EL333
00677      EJECT                                                        EL333
00678                             COPY ELCDATE.                            CL**6
00679                                                                   EL333
00680                             COPY ELCDTECX.                        EL333
00681                                                                   EL333
00682                             COPY ELCDTEVR.                        EL333
00683                                                                   EL333
00684      EJECT                                                        EL333
00685  PROCEDURE DIVISION.                                              EL333
00686                                                                   EL333
00687  0000-LOAD-DATE-CARD.       COPY ELCDTERX.                        EL333
00688                                                                   EL333
00689  0005-INITILIZE-RPT-HEADINGS.                                     EL333
00690      MOVE LIFE-OVERRIDE-L6 TO WS-H6-LF1                           EL333
00691                               WS-H6-LF2.                          EL333
00692                                                                   EL333
00693      MOVE AH-OVERRIDE-L6   TO WS-H6-AH1                           EL333
00694                               WS-H6-AH2.                          EL333
00695                                                                   EL333
00696      MOVE LIFE-OVERRIDE-L2 TO TVM-LIFE-L2-MSG1                    EL333
00697                               TVM-LIFE-L2-MSG2.                   EL333
00698                                                                   EL333
00699      MOVE AH-OVERRIDE-L2   TO TVM-AH-L2-MSG1                      EL333
00700                               TVM-AH-L2-MSG2.                     EL333
00701                                                                   EL333
00702      IF  DTE-CLIENT = 'DMD'                                       EL333
00703          MOVE ' ESTABLISHED TO FIRST ACTION'                      EL333
00704                                  TO PRINT-DESCRIPTION (6).        EL333
00705                                                                   EL333
00706  0010-START-PROGRAM-ROUTINE.                                      EL333
00707      IF DTE-SYS-E-CLASIC-CLAIMS NOT = 'Y'                         EL333
00708         DISPLAY ' CLIENT ' DTE-CLIENT ' IS NOT ON LINE CLAIM USER'EL333
00709         UPON CONSOLE                                              EL333
00710         DISPLAY ' PROCESSING TERMINATED ' UPON CONSOLE            EL333
00711         GO TO 0150-STOP-RUN.                                      EL333
00712                                                                   EL333
00713      OPEN INPUT CLAIM-HIST                                        EL333
00714           OUTPUT PRNTR.                                           EL333
00715                                                                   EL333
00716      IF  DTE-PGM-OPT LESS 1 OR GREATER 6                          EL333
00717         MOVE 1 TO DTE-PGM-OPT.                                    EL333
00718                                                                   EL333
00719      IF DTE-TOT-OPT NOT = '2'                                        CL*12
00720          IF RUN-DATE = EP-DT                                         CL*12
00721             MOVE 12              TO RUN-MO                           CL*12
00722             MOVE 31              TO RUN-DA                           CL*12
00723             SUBTRACT 1 FROM RUN-CCYY                                 CL*12
00724             MOVE WS-RUN-DATE-N   TO RUN-DATE                         CL*12
00725          ELSE                                                        CL*12
00726              MOVE    EP-YR       TO  DC-MDY-YEAR                     CL*12
00727              MOVE    EP-MO       TO  DC-MDY-MONTH                    CL*12
00728              MOVE    EP-DA       TO  DC-MDY-DAY                      CL*12
00729              MOVE    EP-CC       TO  DC-ALPHA-CEN-N                  CL*12
00730              MOVE    '4'                   TO  DC-OPTION-CODE        CL*12
00731              PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT             CL*12
00732              MOVE    DC-GREG-DATE-1-ALPHA                            CL*12
00733                                  TO  ALPH-DATE                       CL*18
00734          END-IF                                                      CL*12
00735          MOVE    EP-MO           TO  END-RPT-A-MO                    CL*13
00736          MOVE    EP-DA           TO  END-RPT-A-DA                    CL*13
00737          MOVE    EP-YR           TO  END-RPT-A-YR                    CL*13
00738          MOVE    END-RPT-A-DATE  TO  END-DT                          CL*18
00739          MOVE    WS-RUN-DATE-N(4:8)                                  CL*12
00740                                  TO  DC-GREG-DATE-CYMD               CL*13
TSTMOD*        MOVE    'H'             TO  DC-OPTION-CODE                  CL*13
TSTMOD         MOVE    'L'             TO  DC-OPTION-CODE                  CL*13
00742          PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT                 CL*13
00743          MOVE    DC-BIN-DATE-1   TO  BEGIN-BIN-DATE                  CL*13
pemuni         move +0                 to  dc-elapsed-months
00744          MOVE    +1              TO  DC-ELAPSED-DAYS                 CL*13
00745          MOVE    '6'             TO  DC-OPTION-CODE                  CL*13
00746          PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT                 CL*13
00747          MOVE    DC-GREG-DATE-1-EDIT                                 CL*13
00748                                  TO  BEGIN-RPT-A-DATE                CL*13
00749                                      BEGIN-DT                        CL*13
00750          MOVE    +0              TO  DC-ELAPSED-DAYS                 CL*13
00751          MOVE    EP-YR           TO  DC-MDY-YEAR                     CL*13
00752          MOVE    EP-MO           TO  DC-MDY-MONTH                    CL*13
00753          MOVE    EP-DA           TO  DC-MDY-DAY                      CL*13
00754          MOVE    EP-CC           TO  DC-ALPHA-CEN-N                  CL*13
00755          MOVE    '4'             TO  DC-OPTION-CODE                  CL*13
00756          PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT                 CL*13
00757          MOVE    DC-BIN-DATE-1   TO  END-BIN-DATE                    CL*13
00758          MOVE WS-HEADING5A       TO  WS-HEADING5                     CL*13
00759      ELSE                                                            CL*13
00760          MOVE 'N'                TO  RELEASE-SW                      CL*25
00761          MOVE EP-YR              TO  FROM-RPT-B-YR                   CL*25
00762          MOVE 01                 TO  FROM-RPT-B-MO                   CL*13
00763                                      DC-MDY-MONTH                    CL*23
00764          MOVE 01                 TO  FROM-RPT-B-DA                   CL*13
00765                                      DC-MDY-DAY                      CL*23
00766          MOVE EP-YR              TO  DC-MDY-YEAR                     CL*22
00767          MOVE    '4'             TO  DC-OPTION-CODE                  CL*13
00768          PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT                 CL*13
00769          MOVE DC-BIN-DATE-1      TO  BEGIN-YTD-BIN-DATE              CL*13
00770          MOVE EP-MO              TO  THRU-RPT-B-MO                   CL*14
00771                                      END-RPT-B-MO                    CL*13
00772                                      DC-MDY-MONTH                    CL*24
00773          MOVE EP-DA              TO  THRU-RPT-B-DA                   CL*14
00774                                      END-RPT-B-DA                    CL*13
00775                                      DC-MDY-DAY                      CL*24
00776          MOVE EP-YR              TO  THRU-RPT-B-YR                   CL*14
00777                                      END-RPT-B-YR                    CL*13
00778          MOVE EP-YR              TO  DC-YMD-YEAR                     CL*19
00779          MOVE EP-MO              TO  DC-YMD-MONTH                    CL*19
00780          MOVE EP-DA              TO  DC-YMD-DAY                      CL*19
00781          MOVE    '4'             TO  DC-OPTION-CODE                  CL*13
00782          PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT                 CL*13
00783          MOVE    DC-GREG-DATE-1-ALPHA                                CL*13
00784                                  TO  ALPH-DATE                       CL*13
00785          MOVE DC-BIN-DATE-1      TO  END-YTD-BIN-DATE                CL*13
00786                                      END-BIN-DATE                    CL*13
00787          MOVE DC-GREG-DATE-1-EDIT                                    CL*13
00788                                  TO  END-DT                          CL*13
00789          IF RUN-DATE = EP-DT                                         CL*13
00790              MOVE RUN-YR         TO  DC-MDY-YEAR                     CL*21
00791              MOVE RUN-MO         TO  DC-MDY-MONTH                    CL*21
00792              MOVE RUN-DA         TO  DC-MDY-DAY                      CL*21
00793              MOVE '4'            TO  DC-OPTION-CODE                  CL*13
00794              PERFORM  8500-DATE-CONVERSION  THRU  8590-EXIT          CL*13
pemuni             move +0             to  dc-elapsed-days
00795              MOVE -1             TO  DC-ELAPSED-MONTHS               CL*13
00796              MOVE '6'            TO  DC-OPTION-CODE                  CL*13
00797              PERFORM  8500-DATE-CONVERSION  THRU  8590-EXIT          CL*13
00798              MOVE ZEROS          TO  DC-ELAPSED-MONTHS               CL*13
00799              MOVE DC-BIN-DATE-2  TO  BEGIN-BIN-DATE                  CL*13
00800              MOVE DC-BIN-DATE-2  TO  DC-BIN-DATE-1                   CL*13
00801              MOVE +1             TO  DC-ELAPSED-DAYS                 CL*13
pemuni             move +0             to  dc-elapsed-months
00802              MOVE '6'            TO  DC-OPTION-CODE                  CL*13
00803              PERFORM  8500-DATE-CONVERSION  THRU  8590-EXIT          CL*13
00804              MOVE DC-BIN-DATE-1  TO  PERIOD-BEGIN-BIN-DATE           CL*13
00805              MOVE DC-GREG-DATE-1-EDIT                                CL*13
00806                                  TO  BEGIN-DT                        CL*13
00807              MOVE DC-GREG-DATE-1-MDY                                 CL*13
00808                                  TO  PERIOD-BEGIN-DATE               CL*13
00809              MOVE PERIOD-BEGIN-MO                                    CL*13
00810                                  TO  BEGIN-RPT-B-MO                  CL*13
00811              MOVE PERIOD-BEGIN-DA                                    CL*13
00812                                  TO  BEGIN-RPT-B-DA                  CL*13
00813              MOVE PERIOD-BEGIN-YR                                    CL*13
00814                                  TO  BEGIN-RPT-B-YR                  CL*13
00815          ELSE                                                        CL*13
00816              MOVE RUN-DATE       TO  DC-GREG-DATE-1-MDY              CL*13
00817              MOVE '4'            TO  DC-OPTION-CODE                  CL*13
00818              PERFORM  8500-DATE-CONVERSION  THRU  8590-EXIT          CL*13
00819              MOVE DC-GREG-DATE-1-EDIT                                CL*13
00820                                  TO  BEGIN-DT                        CL*13
00821              MOVE DC-GREG-DATE-1-MDY                                 CL*13
00822                                  TO  PERIOD-BEGIN-DATE               CL*13
00823              MOVE PERIOD-BEGIN-YR                                    CL*13
00824                                  TO  BEGIN-RPT-B-YR                  CL*13
00825              MOVE PERIOD-BEGIN-MO                                    CL*13
00826                                  TO  BEGIN-RPT-B-MO                  CL*13
00827              MOVE PERIOD-BEGIN-DA                                    CL*13
00828                                  TO  BEGIN-RPT-B-DA                  CL*13
00829              MOVE -1             TO  DC-ELAPSED-DAYS                 CL*13
pemuni             move +0             to  dc-elapsed-months
00830              MOVE '6'            TO  DC-OPTION-CODE                  CL*13
00831              PERFORM  8500-DATE-CONVERSION  THRU  8590-EXIT          CL*13
00832              MOVE DC-BIN-DATE-1  TO  BEGIN-BIN-DATE                  CL*13
00833         END-IF                                                       CL*13
00834             MOVE 'EL333A'        TO  WS-H1-RPT                       CL*13
00835             MOVE WS-HEADING5B    TO  WS-HEADING5.                    CL*13
00836                                                                      CL*13
00837                                                                   EL333
00838      IF  DTE-PGM-OPT = 1                                          EL333
00839          MOVE 'TOTALS FOR CARRIER' TO TOTAL-DESCRIPTION           EL333
00840      ELSE                                                         EL333
00841          IF  DTE-PGM-OPT = 2                                      EL333
00842              MOVE 'TOTALS FOR STATE ' TO TOTAL-DESCRIPTION        EL333
00843          ELSE                                                     EL333
00844              IF  DTE-PGM-OPT = 3                                  EL333
00845                  MOVE 'TOTALS FOR ACCOUNT' TO TOTAL-DESCRIPTION   EL333
00846              ELSE                                                 EL333
00847                  IF  DTE-PGM-OPT = 4                              EL333
00848                      MOVE 'TOTALS FOR BENEFIT'                    EL333
00849                          TO TOTAL-DESCRIPTION                     EL333
00850                  ELSE                                             EL333
00851                      IF  DTE-PGM-OPT = 5                          EL333
00852                          IF  DTE-CLIENT = 'DMD'                   EL333
00853                              MOVE 'TOTALS FOR ESTAB. BY'          EL333
00854                                  TO TOTAL-DESCRIPTION             EL333
00855                          ELSE                                     EL333
00856                              MOVE 'TOTALS FOR PROCESSOR'          EL333
00857                                  TO TOTAL-DESCRIPTION             EL333
00858                      ELSE                                         EL333
00859                          IF  DTE-PGM-OPT = 6                      EL333
00860                                  AND                              EL333
00861                              DTE-CLIENT = 'DMD'                   EL333
00862                              MOVE 'TOTALS FOR BENEFICIARY'        EL333
00863                                  TO TOTAL-DESCRIPTION.            EL333
00864                                                                   EL333
00865      SET REPORT-INDEX TO +1.                                      EL333
00866      SET GRAND-INDEX  TO +1.                                      EL333
00867                                                                   EL333
00868  0080-ZERO-TABLE.                                                 EL333
00869      IF REPORT-INDEX GREATER THAN +4                              EL333
00870         MOVE REPORT-TABLE-RECORD TO BLANK-RECORD                  EL333
00871         GO TO 0100-SORTING-PARAMETERS.                            EL333
00872                                                                   EL333
00873      MOVE INITIALIZE-TABLE TO REST-OF-RECORD (REPORT-INDEX)       EL333
00874                               GRAND-RECORD (GRAND-INDEX).         EL333
00875                                                                   EL333
00876      SET REPORT-INDEX UP BY +1.                                   EL333
00877      SET GRAND-INDEX  UP BY +1.                                   EL333
00878                                                                   EL333
00879      GO TO 0080-ZERO-TABLE.                                       EL333
00880                                                                   EL333
00881  0100-SORTING-PARAMETERS.                                         EL333
00882      MOVE SPACES TO ESTABLISH-SW                                  EL333
00883                     CRITERIA-SW.                                  EL333
00884      MOVE ZEROS  TO WS-FUTURE-CLAIMS.                             EL333
00885                                                                   EL333
00886      SORT SORT-FILE                                               EL333
00887          ON ASCENDING KEY SR-CONTROL                              EL333
00888              INPUT PROCEDURE  0200-PROCESS-HISTORY-FILE           EL333
00889              OUTPUT PROCEDURE 1000-OUTPUT-ROUTINE THRU 7999-EXIT. EL333
00890                                                                   EL333
00891      DISPLAY ' '.                                                 EL333
00892      DISPLAY ' '.                                                 EL333
00893      DISPLAY '    FUTURE CLAIMS           .... ' WS-FUTURE-CLAIMS.EL333
00894      DISPLAY '    RECORDS IN              .... ' IN-RECS.         EL333
00895      DISPLAY '    SELECTED RECORDS        .... ' SEL-RECS.        EL333
00896      DISPLAY '    RELEASED CLAIM RECORDS  .... ' REL-CL-RECS.     EL333
00897      DISPLAY '    RELEASED CERT RECORDS   .... ' REL-CR-RECS.     EL333
00898      DISPLAY '    RELEASED POLICY RECORDS .... ' REL-PM-RECS.     EL333
00899      DISPLAY '    RELEASED TRLR RECORDS   .... ' REL-TR-RECS.     EL333
00900      DISPLAY '    RETURNED RECORDS        .... ' RET-RECS.        EL333
00901      DISPLAY '    RETURNED CLAIM RECORDS  .... ' RET-CL-RECS.     EL333
00902      DISPLAY '    RETURNED CERT RECORDS   .... ' RET-CR-RECS.     EL333
00903      DISPLAY '    RETURNED POLICY RECORDS .... ' RET-PM-RECS.     EL333
00904      DISPLAY '    RETURNED TRLR RECORDS   .... ' RET-TR-RECS.     EL333
00905                                                                   EL333
00906      IF SORT-RETURN NOT = ZEROS                                   EL333
00907          MOVE '0101' TO ABEND-CODE                                EL333
00908          GO TO ABEND-PGM.                                         EL333
00909                                                                   EL333
00910  0145-CLOSE-FILES.                                                EL333
00911                        COPY ELCPRTCX.                             EL333
00912                                                                   EL333
00913      CLOSE PRNTR.                                                 EL333
00914                                                                   EL333
00915  0150-STOP-RUN.                                                   EL333
00916      MOVE ZEROS  TO RETURN-CODE.
00916      GOBACK.                                                      EL333
00917      EJECT                                                        EL333
00918  0200-PROCESS-HISTORY-FILE SECTION.                               EL333
00919                                                                   EL333
00920  0250-READ-HISTORY.                                               EL333
00921      READ  CLAIM-HIST                                             EL333
00922        AT END                                                     EL333
00923           GO TO 0500-CLOSE-HIST.                                  EL333
00924                                                                   EL333
00925      ADD 1 TO IN-RECS.                                            EL333
00926                                                                   EL333
00927      IF DTE-CLIENT LESS THAN HIR-COMPANY-ID                       EL333
00928           GO TO 0500-CLOSE-HIST.                                  EL333
00929                                                                   EL333
00930      IF DTE-CLIENT GREATER THAN HIR-COMPANY-ID                    EL333
00931           GO TO 0250-READ-HISTORY.                                EL333
00932                                                                   EL333
00933      ADD 1 TO SEL-RECS.                                           EL333
00934                                                                   EL333
00935      IF (HIR-RECORD-ID NOT = 'CL' AND 'CM' AND 'PM')              EL333
00936          GO TO 0300-PROCESS-CLTRLR.                               EL333
00937                                                                   EL333
00938      IF HIR-RECORD-ID = 'CL'                                      EL333
00939         PERFORM 6100-REVIEW-CLAIM THRU 6100-EXIT                  EL333
00940         GO TO 0250-READ-HISTORY.                                  EL333
00941                                                                   EL333
00942      IF HIR-RECORD-ID = 'CM'                                      EL333
00943         IF LAST-RECORD-TYPE = 'CL'                                EL333
00944            MOVE HIR-CERTIFICATE-RECORD TO CERTIFICATE-MASTER      EL333
00945            MOVE 'CM'                   TO LAST-RECORD-TYPE        EL333
00946         ELSE                                                      EL333
00947            DISPLAY ' HISTORY FILE OUT OF SEQUENCE ' UPON CONSOLE  EL333
00948            DISPLAY ' HISTORY FILE OUT OF SEQUENCE '               EL333
00949            GO TO ABEND-PGM.                                       EL333
00950                                                                   EL333
00951      IF HIR-RECORD-ID = 'PM'                                      EL333
00952         IF LAST-RECORD-TYPE = 'CL'                                EL333
pemuni*          MOVE HIR-POLICY-RECORD      TO POLICY-MASTER           EL333
00954            MOVE 'PM'                   TO LAST-RECORD-TYPE        EL333
00955         ELSE                                                      EL333
00956            DISPLAY ' HISTORY FILE OUT OF SEQUENCE ' UPON CONSOLE  EL333
00957            DISPLAY ' HISTORY FILE OUT OF SEQUENCE '               EL333
00958            GO TO ABEND-PGM.                                       EL333
00959                                                                   EL333
00960      IF NOT RELEASE-TO-SORT                                          CL*14
00961          GO TO 0250-READ-HISTORY.                                    CL*14
00962                                                                      CL*14
00963  0250-PROCESS-SORT-RECORD.                                        EL333
00964                                                                   EL333
00965      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                     EL333
00966          GO TO 0260-PROCESS-CONV-SORT-RECORD.                     EL333
00967                                                                   EL333
00968      IF  DTE-PGM-OPT = 1                                          EL333
00969          MOVE CM-CARRIER TO HOLD-KEY                              EL333
00970      ELSE                                                         EL333
00971          IF  DTE-PGM-OPT = 2                                      EL333
00972              MOVE CM-STATE TO HOLD-KEY                            EL333
00973          ELSE                                                     EL333
00974              IF  DTE-PGM-OPT = 3                                  EL333
00975                  MOVE CM-ACCOUNT TO HOLD-KEY                      EL333
00976              ELSE                                                 EL333
00977                  IF  DTE-PGM-OPT = 6                              EL333
00978                      MOVE CL-BENEFICIARY TO HOLD-KEY              EL333
00979                  ELSE                                             EL333
00980                      IF  DTE-PGM-OPT = 5                          EL333
00981                          IF  DTE-CLIENT = 'DMD'                   EL333
00982                              MOVE CL-FILE-ESTABLISHED-BY          EL333
00983                                  TO HOLD-KEY                      EL333
00984                          ELSE                                     EL333
00985                              MOVE CL-PROCESSOR-ID TO HOLD-KEY     EL333
00986                      ELSE                                         EL333
00987                          IF  DTE-PGM-OPT = 4                      EL333
00988                             IF  CL-CLAIM-TYPE = AH-OVERRIDE-L1    EL333
00989                                 MOVE CM-AH-BENEFIT-CD TO HOLD-KEY EL333
00990                                 MOVE 'A' TO HOLD-LF-AH            EL333
00991                             ELSE                                  EL333
00992                                 MOVE CM-LF-BENEFIT-CD TO HOLD-KEY EL333
00993                                 MOVE 'L' TO HOLD-LF-AH.           EL333
00994                                                                   EL333
00995      MOVE    '1'                TO SR-REC-TYPE.                   EL333
00996      MOVE    HOLD-KEY           TO SR-KEY.                        EL333
00997      MOVE    CLAIM-MASTER       TO SR-REST-OF-RECORD.             EL333
00998      MOVE    CL-CONTROL-PRIMARY TO SR-CLAIM-KEY.                  EL333
00999      MOVE    +0                 TO SR-SEQ-NO                      EL333
01000                                                                   EL333
01001      RELEASE SORT-RECORD.                                         EL333
01002                                                                   EL333
01003      ADD     1                  TO REL-CL-RECS.                   EL333
01004      MOVE    '2'                TO SR-REC-TYPE.                   EL333
01005      MOVE    HOLD-KEY           TO SR-KEY.                        EL333
01006      MOVE    CERTIFICATE-MASTER TO SR-REST-OF-RECORD.             EL333
01007      MOVE    CL-CONTROL-PRIMARY TO SR-CLAIM-KEY.                  EL333
01008      MOVE    +0                 TO SR-SEQ-NO                      EL333
01009                                                                   EL333
01010      RELEASE SORT-RECORD.                                         EL333
01011                                                                   EL333
01012      ADD     1                  TO REL-CR-RECS.                   EL333
01013      GO TO 0250-READ-HISTORY.                                     EL333
01014                                                                   EL333
01015  0260-PROCESS-CONV-SORT-RECORD.                                   EL333
01016                                                                   EL333
01017      IF DTE-PGM-OPT IS EQUAL TO 1                                 EL333
01018          MOVE PM-CARRIER                         TO  HOLD-KEY     EL333
01019      ELSE                                                         EL333
01020          IF DTE-PGM-OPT IS EQUAL TO 2                             EL333
01021              MOVE PM-STATE                       TO  HOLD-KEY     EL333
01022          ELSE                                                     EL333
01023              IF DTE-PGM-OPT IS EQUAL TO 3                         EL333
01024                  MOVE PM-PRODUCER                TO  HOLD-KEY     EL333
01025              ELSE                                                 EL333
01026                  IF DTE-PGM-OPT IS EQUAL TO 6                     EL333
01027                      MOVE CL-BENEFICIARY         TO  HOLD-KEY     EL333
01028                  ELSE                                             EL333
01029                      IF DTE-PGM-OPT IS EQUAL TO 5                 EL333
01030                          IF  DTE-CLIENT = 'DMD'                   EL333
01031                              MOVE CL-FILE-ESTABLISHED-BY          EL333
01032                                  TO HOLD-KEY                      EL333
01033                          ELSE                                     EL333
01034                              MOVE CL-PROCESSOR-ID                 EL333
01035                                                  TO  HOLD-KEY     EL333
01036                      ELSE                                         EL333
01037                          IF DTE-PGM-OPT IS EQUAL TO 4             EL333
01038                             IF CL-CLAIM-TYPE EQUAL AH-OVERRIDE-L1 EL333
01039                                 MOVE PM-INS-PLAN-CD               EL333
01040                                                  TO  HOLD-KEY     EL333
01041                                 MOVE 'A'         TO  HOLD-LF-AH   EL333
01042                             ELSE                                  EL333
01043                                 MOVE PM-INS-PLAN-CD               EL333
01044                                                  TO  HOLD-KEY     EL333
01045                                 MOVE 'L'         TO  HOLD-LF-AH.  EL333
01046                                                                   EL333
01047      MOVE '1'                        TO  SR-REC-TYPE.             EL333
01048      MOVE HOLD-KEY                   TO  SR-KEY.                  EL333
01049      MOVE CLAIM-MASTER               TO  SR-REST-OF-RECORD.       EL333
01050      MOVE CL-CONTROL-PRIMARY         TO  SR-CLAIM-KEY.            EL333
01051      MOVE +0                         TO  SR-SEQ-NO.               EL333
01052                                                                   EL333
01053      RELEASE SORT-RECORD.                                         EL333
01054                                                                   EL333
01055      ADD 1                           TO  REL-CL-RECS.             EL333
01056      MOVE '2'                        TO  SR-REC-TYPE.             EL333
01057      MOVE HOLD-KEY                   TO  SR-KEY.                  EL333
01058      MOVE POLICY-MASTER              TO  SR-REST-OF-RECORD.       EL333
01059      MOVE CL-CONTROL-PRIMARY         TO  SR-CLAIM-KEY.            EL333
01060      MOVE +0                         TO  SR-SEQ-NO.               EL333
01061                                                                   EL333
01062      RELEASE SORT-RECORD.                                         EL333
01063                                                                   EL333
01064      ADD 1                           TO  REL-PM-RECS.             EL333
01065      GO TO 0250-READ-HISTORY.                                     EL333
01066                                                                   EL333
01067  0300-PROCESS-CLTRLR.                                             EL333
01068 ***************************************************************** EL333
01069 *********       BYPASS LETTER ARCHIVE TRAILERS          ********* EL333
01070 ***************************************************************** EL333
01071                                                                   EL333
01072      IF HIR-RECORD-ID NOT = 'AT'                                  EL333
01073         GO TO 0250-READ-HISTORY.                                  EL333
01074                                                                   EL333
01075      IF NOT RELEASE-TO-SORT                                          CL*14
01076          GO TO 0250-READ-HISTORY.                                    CL*14
01077                                                                      CL*14
01078      IF LAST-RECORD-TYPE = 'AT'                                   EL333
01079                             OR 'CM' OR 'PM'                       EL333
01080         MOVE HIR-ACTIVITY-TRAILER-RECORD TO ACTIVITY-TRAILERS     EL333
01081 ***************************************************************** EL333
01082 *********       BYPASS RESERVE/EXPENSE TRAILERS         ********* EL333
01083 ***************************************************************** EL333
01084         IF AT-TRAILER-TYPE = '1'                                  EL333
01085            GO TO 0250-READ-HISTORY                                EL333
01086         ELSE                                                      EL333
01087            MOVE '3'                         TO SR-REC-TYPE        EL333
01088            MOVE HOLD-KEY                    TO SR-KEY             EL333
01089            MOVE HIR-ACTIVITY-TRAILER-RECORD TO SR-REST-OF-RECORD  EL333
01090            MOVE CL-CONTROL-PRIMARY          TO SR-CLAIM-KEY       EL333
01091            MOVE AT-SEQUENCE-NO              TO SR-SEQ-NO          EL333
01092            RELEASE SORT-RECORD                                    EL333
01093            ADD 1 TO REL-TR-RECS                                   EL333
01094            GO TO 0250-READ-HISTORY.                               EL333
01095                                                                   EL333
01096      DISPLAY ' HISTORY FILE OUT OF SEQUENCE ' UPON CONSOLE        EL333
01097      DISPLAY ' HISTORY FILE OUT OF SEQUENCE '                     EL333
01098      GO TO ABEND-PGM.                                             EL333
01099                                                                   EL333
01100  0500-CLOSE-HIST.                                                 EL333
01101      CLOSE CLAIM-HIST.                                            EL333
01102      MOVE SPACES           TO LAST-CONTROL-PRIMARY.               EL333
01103                                                                   EL333
01104  0999-EXIT.                                                       EL333
01105      EXIT.                                                        EL333
01106      EJECT                                                        EL333
01107  1000-OUTPUT-ROUTINE SECTION.                                     EL333
01108                                                                   EL333
01109  1010-RETURN-FILE.                                                EL333
01110      RETURN SORT-FILE AT END                                      EL333
01111          MOVE HIGH-VALUES TO SR-CONTROL                           EL333
01112          GO TO 1013-CONTINUE.                                     EL333
01113                                                                   EL333
01114      ADD 1                  TO RET-RECS.                          EL333
01115                                                                   EL333
01116      IF SR-REC-TYPE NOT = '1'                                     EL333
01117         GO TO 1020-PROCESS-CLCERT.                                EL333
01118                                                                   EL333
01119      MOVE SR-REST-OF-RECORD TO CLAIM-MASTER                       EL333
01120      MOVE 'CL'              TO LAST-RECORD-TYPE.                  EL333
01121      ADD 1                  TO RET-CL-RECS.                       EL333
01122                                                                   EL333
01123      IF CL-CONTROL-PRIMARY   NOT = LAST-CONTROL-PRIMARY AND       EL333
01124         LAST-CONTROL-PRIMARY NOT = SPACES                         EL333
01125            NEXT SENTENCE                                          EL333
01126      ELSE                                                         EL333
01127         GO TO 1015-CHECK-FOR-BREAK.                               EL333
01128                                                                   EL333
01129  1013-CONTINUE.                                                   EL333
01130                                                                   EL333
01131      MOVE SPACES                    TO  DENIED-STATUS.            EL333
01132                                                                   EL333
01133      MOVE    LAST-REPORTED-DATE     TO  DC-BIN-DATE-1.            EL333
01134      MOVE    HOLD-FIRST-ACTION-DATE TO  DC-BIN-DATE-2.            EL333
01135      MOVE    '1'                    TO  DC-OPTION-CODE.           EL333
01136      PERFORM 8500-DATE-CONVERSION  THRU 8590-EXIT.                EL333
01137                                                                   EL333
01138      IF  NO-CONVERSION-ERROR                                      EL333
01139          ADD +1 TO TB-CNT-RPT-FRST-ACT (REPORT-INDEX)             EL333
01140          ADD DC-ELAPSED-MONTHS TO TB-RPT-FRST-ACT (REPORT-INDEX)  EL333
01141          IF  DC-ODD-DAYS-OVER GREATER THAN +0                     EL333
01142              COMPUTE HOLD-ODD-DAYS-OVER =                         EL333
01143                      (DC-ODD-DAYS-OVER / +30)                     EL333
01144              ADD HOLD-ODD-DAYS-OVER                               EL333
01145                  TO TB-RPT-FRST-ACT (REPORT-INDEX).               EL333
01146                                                                   EL333
01147      MOVE    LAST-REPORTED-DATE     TO  DC-BIN-DATE-1.            EL333
01148      MOVE    HOLD-FIRST-PMT-DATE    TO  DC-BIN-DATE-2.            EL333
01149      MOVE    '1'                    TO  DC-OPTION-CODE.           EL333
01150      PERFORM 8500-DATE-CONVERSION  THRU 8590-EXIT.                EL333
01151                                                                   EL333
01152      IF  NO-CONVERSION-ERROR                                      EL333
01153          ADD +1 TO TB-CNT-RPT-FRST-PMT (REPORT-INDEX)             EL333
01154          ADD DC-ELAPSED-MONTHS TO TB-RPT-FRST-PMT (REPORT-INDEX)  EL333
01155          IF  DC-ODD-DAYS-OVER GREATER THAN +0                     EL333
01156              COMPUTE HOLD-ODD-DAYS-OVER =                         EL333
01157                      (DC-ODD-DAYS-OVER / +30)                     EL333
01158              ADD HOLD-ODD-DAYS-OVER                               EL333
01159                  TO TB-RPT-FRST-PMT (REPORT-INDEX).               EL333
01160                                                                   EL333
01161      IF WS-CURRENT-PMT-CNTR = +1                                  EL333
01162          IF (HOLD-FIRST-PMT-DATE GREATER BEGIN-BIN-DATE AND       EL333
01163              HOLD-FIRST-PMT-DATE NOT GREATER END-BIN-DATE)        EL333
01164              IF ESTABLISHED-WITHIN-PERIOD                         EL333
01165                  ADD +1 TO TB-FRST-PMTS (REPORT-INDEX)            EL333
01166              ELSE                                                 EL333
01167                  IF CLAIM-WITHIN-SPECIFIED-RANGE                  EL333
01168                      ADD +1 TO TB-FRST-PMTS (REPORT-INDEX - 2)    EL333
01169                  ELSE                                             EL333
01170                      NEXT SENTENCE                                EL333
01171          ELSE                                                     EL333
01172              IF ESTABLISHED-WITHIN-PERIOD                         EL333
01173                  ADD WS-CURRENT-PMT-CNTR         TO               EL333
01174                                  TB-CONT-PMTS (REPORT-INDEX)      EL333
01175              ELSE                                                 EL333
01176                  ADD WS-CURRENT-PMT-CNTR     TO                   EL333
01177                                  TB-CONT-PMTS (REPORT-INDEX - 2). EL333
01178                                                                   EL333
01179      IF WS-CURRENT-PMT-CNTR GREATER THAN +1                       EL333
01180          IF (HOLD-FIRST-PMT-DATE GREATER BEGIN-BIN-DATE AND       EL333
01181              HOLD-FIRST-PMT-DATE NOT GREATER END-BIN-DATE)        EL333
01182              IF ESTABLISHED-WITHIN-PERIOD                         EL333
01183                  ADD +1 TO TB-FRST-PMTS (REPORT-INDEX)            EL333
01184                  SUBTRACT +1 FROM WS-CURRENT-PMT-CNTR             EL333
01185                  ADD WS-CURRENT-PMT-CNTR TO                       EL333
01186                      TB-CONT-PMTS (REPORT-INDEX)                  EL333
01187               ELSE                                                EL333
01188                   IF CLAIM-WITHIN-SPECIFIED-RANGE                 EL333
01189                       ADD +1 TO TB-FRST-PMTS (REPORT-INDEX - 2)   EL333
01190                       SUBTRACT +1 FROM WS-CURRENT-PMT-CNTR        EL333
01191                       ADD WS-CURRENT-PMT-CNTR TO                  EL333
01192                           TB-CONT-PMTS (REPORT-INDEX - 2)         EL333
01193                   ELSE                                            EL333
01194                       NEXT SENTENCE                               EL333
01195          ELSE                                                     EL333
01196              IF ESTABLISHED-WITHIN-PERIOD                         EL333
01197                  ADD WS-CURRENT-PMT-CNTR     TO                   EL333
01198                                  TB-CONT-PMTS (REPORT-INDEX)      EL333
01199              ELSE                                                 EL333
01200                  ADD WS-CURRENT-PMT-CNTR  TO                      EL333
01201                                  TB-CONT-PMTS (REPORT-INDEX - 2). EL333
01202                                                                   EL333
01203      IF WS-ITD-PMT-CNTR = +1                                      EL333
01204            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01205               ADD +1 TO TB-FRST-PMTS (REPORT-INDEX + 2)           EL333
01206            ELSE                                                   EL333
01207               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01208                   ADD +1 TO TB-FRST-PMTS (REPORT-INDEX).          EL333
01209                                                                   EL333
01210      IF WS-ITD-PMT-CNTR GREATER THAN +1                           EL333
01211            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01212               ADD +1 TO TB-FRST-PMTS (REPORT-INDEX + 2)           EL333
01213               SUBTRACT +1 FROM WS-ITD-PMT-CNTR                    EL333
01214               ADD WS-ITD-PMT-CNTR TO                              EL333
01215                   TB-CONT-PMTS (REPORT-INDEX + 2)                 EL333
01216            ELSE                                                   EL333
01217               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01218                   ADD +1 TO TB-FRST-PMTS (REPORT-INDEX)           EL333
01219                   SUBTRACT +1 FROM WS-ITD-PMT-CNTR                EL333
01220                   ADD WS-ITD-PMT-CNTR TO                          EL333
01221                       TB-CONT-PMTS (REPORT-INDEX).                EL333
01222                                                                   EL333
01223      MOVE +0 TO WS-CURRENT-PMT-CNTR                               EL333
01224                 WS-ITD-PMT-CNTR.                                  EL333
01225                                                                   EL333
01226  1015-CHECK-FOR-BREAK.                                            EL333
01227      MOVE LOW-VALUES TO HOLD-FIRST-ACTION-DATE                    EL333
01228                         HOLD-FIRST-PMT-DATE.                      EL333
01229                                                                   EL333
01230      IF  SR-KEY NOT = LAST-SR-KEY                                 EL333
01231              AND                                                  EL333
01232          NOT FIRST-RECORD                                         EL333
01233          PERFORM 7000-REPORT-BREAK-ROUTINE THRU 7099-EXIT         EL333
01234          MOVE BLANK-RECORD  TO REPORT-TABLE-RECORD.               EL333
01235                                                                   EL333
01236      MOVE 'Y'               TO FIRST-RECORD-SW.                   EL333
01237                                                                   EL333
01238      IF SR-CONTROL = HIGH-VALUES                                  EL333
01239         MOVE SPACES         TO TOTAL-VALUE                        EL333
01240                                TOTAL-DESC                         EL333
01241         MOVE 'GRAND TOTALS' TO TOTAL-DESCRIPTION                  EL333
01242         MOVE ZEROS          TO DTE-PGM-OPT                        EL333
01243         MOVE GRAND-TOTAL-RECORD TO REPORT-TABLE-RECORD            EL333
01244         PERFORM 7000-REPORT-BREAK-ROUTINE THRU 7099-EXIT          EL333
01245         MOVE +99 TO WS-LINE-COUNT                                 EL333
01246         MOVE SPACES TO WS-DETAIL1                                 EL333
01247                        TOTAL-VALUE                                EL333
01248                        TOTAL-DESC                                 EL333
01249                        TOTAL-DESCRIPTION                          EL333
01250         MOVE WS-DETAIL1 TO P-DATA                                 EL333
01251         MOVE ' ' TO X                                             EL333
01252 *       PERFORM WRITE-A-LINE                                      EL333
01253         GO TO 7999-EXIT.                                          EL333
01254                                                                   EL333
01255      MOVE SR-KEY             TO LAST-SR-KEY.                      EL333
01256      MOVE CL-CONTROL-PRIMARY TO LAST-CONTROL-PRIMARY.             EL333
01257      MOVE CL-REPORTED-DT     TO LAST-REPORTED-DATE.               EL333
01258      GO TO 1010-RETURN-FILE.                                      EL333
01259                                                                   EL333
01260  1020-PROCESS-CLCERT.                                             EL333
01261      IF SR-REC-TYPE NOT = '2'                                     EL333
01262         GO TO 2000-PROCESS-PAYMENT-CLTRLR.                        EL333
01263                                                                   EL333
01264      IF SR-RECORD-ID IS EQUAL TO 'PM'                             EL333
01265          ADD 1                   TO  RET-PM-RECS                  EL333
01266          MOVE SR-REST-OF-RECORD  TO  POLICY-MASTER                EL333
01267          MOVE 'PM'               TO  LAST-RECORD-TYPE             EL333
01268      ELSE                                                         EL333
01269          ADD 1                   TO  RET-CR-RECS                  EL333
01270          MOVE SR-REST-OF-RECORD  TO  CERTIFICATE-MASTER           EL333
01271          MOVE 'CM'               TO  LAST-RECORD-TYPE.            EL333
01272                                                                   EL333
01273      IF  CL-CLAIM-TYPE = LIFE-OVERRIDE-L1                         EL333
01274          MOVE LIFE-OVERRIDE-L1 TO LAS-CLM-TYPE                    EL333
01275          SET REPORT-INDEX TO +1                                   EL333
01276      ELSE                                                         EL333
01277          MOVE AH-OVERRIDE-L1   TO LAS-CLM-TYPE                    EL333
01278          SET REPORT-INDEX TO +2.                                  EL333
01279                                                                   EL333
01280      IF DTE-TOT-OPT NOT = '2'                                        CL*15
01281          IF CL-FILE-ESTABLISH-DT NOT GREATER THAN BEGIN-BIN-DATE     CL*15
01282             MOVE 'Y'             TO CRITERIA-SW                      CL*15
01283             MOVE 'B'             TO ESTABLISH-SW                     CL*15
01284          END-IF                                                      CL*15
01285          IF CL-FILE-ESTABLISH-DT GREATER THAN END-BIN-DATE           CL*15
01286              MOVE ' '            TO CRITERIA-SW                      CL*15
01287              MOVE 'A'            TO ESTABLISH-SW                     CL*15
01288              ADD 1               TO WS-FUTURE-CLAIMS                 CL*15
01289          END-IF                                                      CL*15
01290          IF (CL-FILE-ESTABLISH-DT GREATER BEGIN-BIN-DATE AND         CL*15
01291              CL-FILE-ESTABLISH-DT NOT GREATER END-BIN-DATE)          CL*15
01292              MOVE 'Y'            TO CRITERIA-SW                      CL*15
01293              MOVE 'W'            TO ESTABLISH-SW                     CL*15
01294          ELSE                                                        CL*15
01295              SET REPORT-INDEX UP BY +2                               CL*15
01296          END-IF                                                      CL*15
01297      ELSE                                                            CL*15
01298          IF CL-FILE-ESTABLISH-DT NOT GREATER THAN                    CL*15
01299                                  PERIOD-BEGIN-BIN-DATE               CL*15
01300                                                                      CL*15
01301                  MOVE 'B'      TO ESTABLISH-SW                       CL*15
01302                  MOVE 'Y'      TO CRITERIA-SW                        CL*15
01303          END-IF                                                      CL*15
01304          IF CL-FILE-ESTABLISH-DT GREATER THAN                        CL*15
01305                                  END-BIN-DATE                        CL*15
01306                                                                      CL*15
01307              MOVE 'A'          TO ESTABLISH-SW                       CL*15
01308              MOVE ' '          TO CRITERIA-SW                        CL*15
01309              ADD +1 TO TB-NEW-CLAIMS (REPORT-INDEX)                  CL*15
01310          END-IF                                                      CL*15
01311          IF (CL-FILE-ESTABLISH-DT GREATER THAN                       CL*15
01312                                   PERIOD-BEGIN-BIN-DATE AND          CL*15
01313              CL-FILE-ESTABLISH-DT NOT GREATER THAN END-BIN-DATE)     CL*15
01314              MOVE 'Y'          TO CRITERIA-SW                        CL*15
01315              MOVE 'W'          TO ESTABLISH-SW                       CL*15
01316          ELSE                                                        CL*15
01317              SET REPORT-INDEX UP BY +2.                              CL*15
01318                                                                      CL*15
01319      IF CLAIM-WITHIN-SPECIFIED-RANGE                              EL333
01320          ADD +1                TO TB-NEW-CLAIMS (REPORT-INDEX)    EL333
01321          IF CL-CLAIM-STATUS EQUAL 'C'                             EL333
01322              ADD +1            TO TB-NEW-CLOSED (REPORT-INDEX).   EL333
01323                                                                   EL333
01324      MOVE CL-INCURRED-DT TO DC-BIN-DATE-1                         EL333
01325      MOVE CL-REPORTED-DT TO DC-BIN-DATE-2                         EL333
01326      MOVE '1' TO DC-OPTION-CODE                                   EL333
01327      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 EL333
01328                                                                   EL333
01329      IF NO-CONVERSION-ERROR                                       EL333
01330         ADD +1 TO TB-CNT-INC-RPT (REPORT-INDEX)                   EL333
01331         ADD DC-ELAPSED-MONTHS TO TB-INC-RPT (REPORT-INDEX)        EL333
01332         IF DC-ODD-DAYS-OVER GREATER THAN +0                       EL333
01333            COMPUTE HOLD-ODD-DAYS-OVER =                           EL333
01334                    (DC-ODD-DAYS-OVER / +30)                       EL333
01335            ADD HOLD-ODD-DAYS-OVER TO TB-INC-RPT (REPORT-INDEX).   EL333
01336                                                                   EL333
01337      MOVE CL-INCURRED-DT      TO DC-BIN-DATE-1.                   EL333
01338      MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-2.                   EL333
01339      MOVE '1'                 TO DC-OPTION-CODE.                  EL333
01340      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 EL333
01341                                                                   EL333
01342      IF  NO-CONVERSION-ERROR                                      EL333
01343          ADD +1 TO TB-CNT-INC-LST-PMT (REPORT-INDEX)              EL333
01344          ADD DC-ELAPSED-MONTHS TO TB-INC-LST-PMT (REPORT-INDEX)   EL333
01345          IF  DC-ODD-DAYS-OVER GREATER THAN +0                     EL333
01346              COMPUTE HOLD-ODD-DAYS-OVER =                         EL333
01347                      (DC-ODD-DAYS-OVER / +30)                     EL333
01348              ADD HOLD-ODD-DAYS-OVER                               EL333
01349                  TO TB-INC-LST-PMT (REPORT-INDEX).                EL333
01350                                                                   EL333
01351      MOVE    CL-REPORTED-DT        TO  DC-BIN-DATE-1.             EL333
01352      MOVE    CL-FILE-ESTABLISH-DT  TO  DC-BIN-DATE-2.             EL333
01353      MOVE    '1'                   TO  DC-OPTION-CODE.            EL333
01354      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 EL333
01355                                                                   EL333
01356      IF NO-CONVERSION-ERROR                                       EL333
01357         ADD +1 TO TB-CNT-RPT-EST (REPORT-INDEX)                   EL333
01358         ADD DC-ELAPSED-MONTHS TO TB-RPT-EST (REPORT-INDEX)        EL333
01359         IF DC-ODD-DAYS-OVER GREATER THAN +0                       EL333
01360            COMPUTE HOLD-ODD-DAYS-OVER =                           EL333
01361                    (DC-ODD-DAYS-OVER / +30)                       EL333
01362            ADD HOLD-ODD-DAYS-OVER TO TB-RPT-EST (REPORT-INDEX).   EL333
01363                                                                   EL333
01364      IF  LAST-RECORD-TYPE IS EQUAL TO 'PM'                        EL333
01365          MOVE PM-POLICY-EFF-DT   TO DC-BIN-DATE-1                 EL333
01366      ELSE                                                         EL333
01367          IF  DTE-CLIENT = 'DMD'                                   EL333
01368              GO TO 1020-CONTINUE                                  EL333
01369          ELSE                                                     EL333
01370              MOVE CM-CERT-EFF-DT TO DC-BIN-DATE-1.                EL333
01371                                                                   EL333
01372      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-2.                EL333
01373      MOVE '1'                    TO DC-OPTION-CODE.               EL333
01374      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 EL333
01375                                                                   EL333
01376      IF NO-CONVERSION-ERROR                                       EL333
01377         ADD +1 TO TB-CNT-EFF-INC (REPORT-INDEX)                   EL333
01378         ADD DC-ELAPSED-MONTHS TO TB-EFF-INC (REPORT-INDEX)        EL333
01379         IF DC-ODD-DAYS-OVER GREATER THAN +0                       EL333
01380            COMPUTE HOLD-ODD-DAYS-OVER =                           EL333
01381                    (DC-ODD-DAYS-OVER / +30)                       EL333
01382            ADD HOLD-ODD-DAYS-OVER TO TB-EFF-INC (REPORT-INDEX).   EL333
01383                                                                   EL333
01384 * FILE FIXES DUE TO CLAIM HISTORY ARCHIVE LOGIC MODIFICATIONS.    EL333
01385                                                                   EL333
01386  1020-CONTINUE.                                                   EL333
01387                                                                   EL333
01388      IF  DTE-CLIENT = 'DMD'                                       EL333
01389              AND                                                  EL333
01390          CLAIM-IS-CLOSED                                          EL333
01391          IF  BENEFITS-CHANGED                                     EL333
01392              ADD +1 TO TB-BENEFITS-CHANGE (REPORT-INDEX)          EL333
01393          ELSE                                                     EL333
01394              IF  SETUP-ERRORS                                     EL333
01395                  ADD +1 TO TB-SETUP-ERROR (REPORT-INDEX).         EL333
01396                                                                   EL333
01397      IF CLAIM-IS-CLOSED                                           EL333
01398        IF CL-LAST-CLOSE-DT NOT = LOW-VALUES AND SPACES            EL333
01399          IF (CL-LAST-CLOSE-DT GREATER THAN BEGIN-BIN-DATE         EL333
01400            AND CL-LAST-CLOSE-DT NOT GREATER THAN END-BIN-DATE)    EL333
01401                IF  ESTABLISHED-WITHIN-PERIOD                      EL333
01402                    ADD +1 TO TB-CLO-CLAIMS (REPORT-INDEX)         EL333
01403                ELSE                                               EL333
01404                   IF CLAIM-WITHIN-SPECIFIED-RANGE                 EL333
01405                        ADD +1 TO TB-CLO-CLAIMS (REPORT-INDEX - 2) EL333
01406                   ELSE                                            EL333
01407                        NEXT SENTENCE                              EL333
01408          ELSE                                                     EL333
01409              IF  ESTABLISHED-WITHIN-PERIOD                        EL333
01410                  ADD +1 TO TB-CLO-CLAIMS (REPORT-INDEX + 2)       EL333
01411              ELSE                                                 EL333
01412                  IF CLAIM-WITHIN-SPECIFIED-RANGE                  EL333
01413                      ADD +1 TO TB-CLO-CLAIMS (REPORT-INDEX).      EL333
01414                                                                   EL333
01415      IF (CL-LAST-REOPEN-DT NOT = LOW-VALUES AND SPACES)           EL333
01416         IF (CL-LAST-REOPEN-DT GREATER THAN BEGIN-BIN-DATE  AND    EL333
01417             CL-LAST-REOPEN-DT NOT GREATER THAN END-BIN-DATE)      EL333
01418             IF ESTABLISHED-WITHIN-PERIOD                          EL333
01419                ADD +1 TO TB-REOPENED (REPORT-INDEX)               EL333
01420             ELSE                                                  EL333
01421                IF CLAIM-WITHIN-SPECIFIED-RANGE                    EL333
01422                    ADD +1 TO TB-REOPENED (REPORT-INDEX - 2)       EL333
01423                ELSE                                               EL333
01424                    NEXT SENTENCE                                  EL333
01425         ELSE                                                      EL333
01426             IF ESTABLISHED-WITHIN-PERIOD                          EL333
01427                ADD +1 TO TB-REOPENED (REPORT-INDEX + 2)           EL333
01428             ELSE                                                  EL333
01429                IF CLAIM-WITHIN-SPECIFIED-RANGE                    EL333
01430                    ADD +1 TO TB-REOPENED (REPORT-INDEX).          EL333
01431                                                                   EL333
01432      IF ((CL-LAST-REOPEN-DT NOT = LOW-VALUES AND SPACES)          EL333
01433         AND (CLAIM-IS-OPEN))                                      EL333
01434         IF (CL-LAST-REOPEN-DT GREATER THAN BEGIN-BIN-DATE  AND    EL333
01435             CL-LAST-REOPEN-DT NOT GREATER THAN END-BIN-DATE)      EL333
01436             IF ESTABLISHED-WITHIN-PERIOD                          EL333
01437                ADD +1 TO TB-REMAIN-REOPEN (REPORT-INDEX)          EL333
01438             ELSE                                                  EL333
01439                IF CLAIM-WITHIN-SPECIFIED-RANGE                    EL333
01440                    ADD +1 TO TB-REMAIN-REOPEN (REPORT-INDEX - 2)  EL333
01441                ELSE                                               EL333
01442                    NEXT SENTENCE                                  EL333
01443         ELSE                                                      EL333
01444             IF ESTABLISHED-WITHIN-PERIOD                          EL333
01445                ADD +1 TO TB-REMAIN-REOPEN (REPORT-INDEX + 2)      EL333
01446             ELSE                                                  EL333
01447                IF CLAIM-WITHIN-SPECIFIED-RANGE                    EL333
01448                    ADD +1 TO TB-REMAIN-REOPEN (REPORT-INDEX).     EL333
01449                                                                   EL333
01450      IF CLAIM-IS-OPEN                                             EL333
01451           IF ESTABLISHED-WITHIN-PERIOD                            EL333
01452              ADD +1   TO TB-NOW-OPEN (REPORT-INDEX)               EL333
01453           ELSE                                                    EL333
01454              IF CLAIM-WITHIN-SPECIFIED-RANGE                      EL333
01455                  ADD +1   TO TB-NOW-OPEN (REPORT-INDEX - 2).      EL333
01456                                                                   EL333
01457      IF CLAIM-IS-OPEN                                             EL333
01458          IF ESTABLISHED-BEFORE-PERIOD                             EL333
01459             PERFORM 6000-REVIEW-OPEN-CLAIM THRU 6000-EXIT.        EL333
01460                                                                   EL333
01461      IF CLAIM-IS-CLOSED                                           EL333
01462          IF  ESTABLISHED-BEFORE-PERIOD                            EL333
01463             IF ((CL-LAST-CLOSE-DT NOT LESS THAN                   EL333
01464                          BEGIN-BIN-DATE) AND                      EL333
01465                 (CL-LAST-CLOSE-DT NOT GREATER THAN END-BIN-DATE)) EL333
01466                  ADD +1 TO TB-EXST-OPEN (REPORT-INDEX - 2).       EL333
01467                                                                   EL333
01468      IF CL-LAST-CLOSE-REASON = '3'                                EL333
01469         IF (CL-LAST-CLOSE-DT GREATER THAN BEGIN-BIN-DATE  AND     EL333
01470             CL-LAST-CLOSE-DT NOT GREATER THAN END-BIN-DATE)       EL333
01471             IF ESTABLISHED-WITHIN-PERIOD                          EL333
01472                ADD +1 TO TB-AUTO-CLOSE (REPORT-INDEX)             EL333
01473             ELSE                                                  EL333
01474               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01475                   ADD +1 TO TB-AUTO-CLOSE (REPORT-INDEX - 2)      EL333
01476               ELSE                                                EL333
01477                   NEXT SENTENCE                                   EL333
01478         ELSE                                                      EL333
01479             IF ESTABLISHED-WITHIN-PERIOD                          EL333
01480                ADD +1 TO TB-AUTO-CLOSE (REPORT-INDEX + 2)         EL333
01481             ELSE                                                  EL333
01482               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01483                   ADD +1 TO TB-AUTO-CLOSE (REPORT-INDEX).         EL333

           IF (CLAIM-IS-CLOSED)
              AND (CL-TOTAL-PAID-AMT = +0)
              AND (CL-CERT-STATE = 'MO')
              AND (CL-CLAIM-TYPE = 'L')
              DISPLAY ' CLS & UPD  ' CL-CONTROL-PRIMARY (2:19)
                 '  ' CL-CLAIM-TYPE
              IF CLAIM-DENIED
                 DISPLAY ' DENIED TOO ' CL-CONTROL-PRIMARY (2:19)
                    '  ' CL-CLAIM-TYPE
              END-IF
           END-IF

01485      IF (CLAIM-IS-CLOSED)
01486         AND (CL-TOTAL-PAID-AMT = +0)
              AND (NOT CLAIM-DENIED)
01487         IF (CL-LAST-CLOSE-DT GREATER THAN BEGIN-BIN-DATE  AND
01488             CL-LAST-CLOSE-DT NOT GREATER THAN END-BIN-DATE)
01489             IF ESTABLISHED-WITHIN-PERIOD
01490                ADD +1 TO TB-UNPAID-CLOSE (REPORT-INDEX)
01491             ELSE
01492                IF CLAIM-WITHIN-SPECIFIED-RANGE
01493                   ADD +1 TO TB-UNPAID-CLOSE (REPORT-INDEX - 2)
                     END-IF
                  END-IF  
01496         ELSE
01497             IF ESTABLISHED-WITHIN-PERIOD
01498                ADD +1 TO TB-UNPAID-CLOSE (REPORT-INDEX + 2)
01499             ELSE
01500                IF CLAIM-WITHIN-SPECIFIED-RANGE
01501                   ADD +1 TO TB-UNPAID-CLOSE (REPORT-INDEX)
                     END-IF
                  END-IF
              END-IF
           END-IF

01503      IF (CLAIM-IS-CLOSED AND                                      EL333
01504         CL-TOTAL-PAID-AMT GREATER THAN +0)                        EL333
01505         IF (CL-LAST-CLOSE-DT GREATER THAN BEGIN-BIN-DATE  AND     EL333
01506             CL-LAST-CLOSE-DT NOT GREATER THAN END-BIN-DATE)       EL333
01507             IF ESTABLISHED-WITHIN-PERIOD                          EL333
01508                ADD +1 TO TB-PAID-CLOSE (REPORT-INDEX)             EL333
01509             ELSE                                                  EL333
01510                IF CLAIM-WITHIN-SPECIFIED-RANGE                    EL333
01511                    ADD +1 TO TB-PAID-CLOSE (REPORT-INDEX - 2)     EL333
01512                ELSE                                               EL333
01513                    NEXT SENTENCE                                  EL333
01514         ELSE                                                      EL333
01515             IF ESTABLISHED-WITHIN-PERIOD                          EL333
01516                ADD +1 TO TB-PAID-CLOSE (REPORT-INDEX + 2)         EL333
01517             ELSE                                                  EL333
01518                IF CLAIM-WITHIN-SPECIFIED-RANGE                    EL333
01519                    ADD +1 TO TB-PAID-CLOSE (REPORT-INDEX).        EL333
01520                                                                   EL333
01521      IF LAST-RECORD-TYPE IS EQUAL TO 'PM'                         EL333
01522          GO TO 1030-PROCESS-EMPLCY.                               EL333
01523                                                                   EL333
01524      IF (CL-CLAIM-TYPE = LIFE-OVERRIDE-L1 AND CLAIM-IS-OPEN)      EL333
01525          IF  CM-LF-BENEFIT-CD = ZEROS                             EL333
01526                ADD +1 TO TB-OPEN-NO-COV (REPORT-INDEX).           EL333
01527                                                                   EL333
01528      IF (CL-CLAIM-TYPE = AH-OVERRIDE-L1 AND CLAIM-IS-OPEN)        EL333
01529            IF  CM-AH-BENEFIT-CD = ZEROS                           EL333
01530                ADD +1 TO TB-OPEN-NO-COV (REPORT-INDEX).           EL333
01531                                                                   EL333
01532      IF  CL-CLAIM-TYPE = LIFE-OVERRIDE-L1                         EL333
01533        IF CLAIM-WITHIN-SPECIFIED-RANGE                            EL333
01534         ADD CM-LF-BENEFIT-AMT TO TB-BENEFIT (REPORT-INDEX)        EL333
01535         ADD CM-LF-ORIG-TERM   TO TB-TERM    (REPORT-INDEX).       EL333
01536                                                                   EL333
01537      IF  CL-CLAIM-TYPE = AH-OVERRIDE-L1                           EL333
01538        IF CLAIM-WITHIN-SPECIFIED-RANGE                            EL333
01539         ADD CM-AH-BENEFIT-AMT TO TB-BENEFIT (REPORT-INDEX)        EL333
01540         ADD CM-AH-ORIG-TERM   TO TB-TERM    (REPORT-INDEX).       EL333
01541                                                                   EL333
01542      IF CLAIM-WITHIN-SPECIFIED-RANGE                              EL333
01543         IF CM-INSURED-ISSUE-AGE GREATER THAN ZEROS                EL333
01544            ADD CM-INSURED-ISSUE-AGE TO TB-AGE (REPORT-INDEX)      EL333
01545         ELSE                                                      EL333
01546            ADD DTE-DEFAULT-AGE TO TB-AGE (REPORT-INDEX).          EL333
01547                                                                   EL333
01548      GO TO 1010-RETURN-FILE.                                      EL333
01549                                                                   EL333
01550      EJECT                                                        EL333
01551  1030-PROCESS-EMPLCY.                                             EL333
01552                                                                   EL333
01553      IF (CL-CLAIM-TYPE IS EQUAL TO LIFE-OVERRIDE-L1 OR            EL333
01554          CL-CLAIM-TYPE IS EQUAL TO AH-OVERRIDE-L1) AND            EL333
01555          CLAIM-IS-OPEN                                            EL333
01556          IF PM-INS-PLAN-CD IS EQUAL TO ZEROS                      EL333
01557              ADD +1              TO TB-OPEN-NO-COV (REPORT-INDEX).EL333
01558                                                                   EL333
01559      IF CL-CLAIM-TYPE IS EQUAL TO LIFE-OVERRIDE-L1                EL333
01560          IF CLAIM-WITHIN-SPECIFIED-RANGE                          EL333
01561              ADD PM-INS-TOTAL-BENEFIT    TO                       EL333
01562                                      TB-BENEFIT (REPORT-INDEX)    EL333
01563              ADD PM-LOAN-TERM            TO                       EL333
01564                                      TB-TERM    (REPORT-INDEX).   EL333
01565                                                                   EL333
01566      IF CL-CLAIM-TYPE IS EQUAL TO AH-OVERRIDE-L1                  EL333
01567          IF CLAIM-WITHIN-SPECIFIED-RANGE                          EL333
01568              ADD PM-INS-TOTAL-BENEFIT    TO                       EL333
01569                                      TB-BENEFIT (REPORT-INDEX)    EL333
01570              ADD PM-LOAN-TERM            TO                       EL333
01571                                      TB-TERM    (REPORT-INDEX).   EL333
01572                                                                   EL333
01573      IF CLAIM-WITHIN-SPECIFIED-RANGE                              EL333
01574          IF PM-INSURED-ISSUE-AGE IS GREATER THAN ZEROS            EL333
01575              ADD PM-INSURED-ISSUE-AGE   TO  TB-AGE (REPORT-INDEX) EL333
01576          ELSE                                                     EL333
01577              ADD DTE-DEFAULT-AGE        TO  TB-AGE (REPORT-INDEX).EL333
01578                                                                   EL333
01579      GO TO 1010-RETURN-FILE.                                      EL333
01580                                                                   EL333
01581      EJECT                                                        EL333
01582  2000-PROCESS-PAYMENT-CLTRLR.                                     EL333
01583      IF SR-REC-TYPE NOT = '3'                                     EL333
01584         GO TO 1010-RETURN-FILE.                                   EL333
01585                                                                   EL333
01586      IF DTE-TOT-OPT = '2'                                            CL*15
01587          IF NOT CLAIM-WITHIN-SPECIFIED-RANGE                         CL*15
01588              GO TO 1010-RETURN-FILE.                                 CL*15
01589                                                                      CL*15
01590      MOVE SR-REST-OF-RECORD TO ACTIVITY-TRAILERS.                 EL333
01591      ADD 1 TO RET-TR-RECS.                                        EL333
01592                                                                   EL333
01593      IF AT-RECORDED-DT GREATER THAN END-BIN-DATE                  EL333
01594         GO TO 1010-RETURN-FILE.                                   EL333
01595                                                                   EL333
01596      IF AT-TRAILER-TYPE NOT = '1' AND '2' AND '3' AND '8'         EL333
01597         IF (AT-RECORDED-DT GREATER THAN BEGIN-BIN-DATE AND        EL333
01598             AT-RECORDED-DT NOT GREATER THAN END-BIN-DATE)         EL333
01599            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01600               ADD +1 TO TB-ACT-REC (REPORT-INDEX)                 EL333
01601            ELSE                                                   EL333
01602               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01603                  ADD +1 TO TB-ACT-REC (REPORT-INDEX - 2)          EL333
01604               ELSE                                                EL333
01605                  NEXT SENTENCE                                    EL333
01606         ELSE                                                      EL333
01607            IF  ESTABLISHED-WITHIN-PERIOD                          EL333
01608                ADD +1 TO TB-ACT-REC (REPORT-INDEX + 2)            EL333
01609            ELSE                                                   EL333
01610               IF  CLAIM-WITHIN-SPECIFIED-RANGE                    EL333
01611                   ADD +1 TO TB-ACT-REC (REPORT-INDEX).            EL333
01612                                                                   EL333
01613      IF  AT-TRAILER-TYPE NOT = '1'                                EL333
01614          MOVE AT-RECORDED-DT     TO HOLD-FIRST-ACTION-DATE        EL333
01615          IF  DTE-CLIENT = 'DMD'                                   EL333
01616              MOVE CL-FILE-ESTABLISH-DT                            EL333
01617                                  TO DC-BIN-DATE-1                 EL333
01618              MOVE HOLD-FIRST-ACTION-DATE                          EL333
01619                                  TO  DC-BIN-DATE-2                EL333
01620              MOVE    '1'         TO  DC-OPTION-CODE               EL333
01621              PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT          EL333
01622              IF  NO-CONVERSION-ERROR                              EL333
01623                  ADD +1          TO TB-CNT-EFF-INC (REPORT-INDEX) EL333
01624                  ADD DC-ELAPSED-MONTHS                            EL333
01625                                  TO TB-EFF-INC (REPORT-INDEX)     EL333
01626                  IF  DC-ODD-DAYS-OVER GREATER THAN +0             EL333
01627                      COMPUTE HOLD-ODD-DAYS-OVER                   EL333
01628                          = (DC-ODD-DAYS-OVER / +30)               EL333
01629                      ADD HOLD-ODD-DAYS-OVER                       EL333
01630                                  TO TB-EFF-INC (REPORT-INDEX).    EL333
01631                                                                   EL333
01632      IF AT-TRAILER-TYPE NOT = '2'                                 EL333
01633         GO TO 3000-PROCESS-AUTO-PMTS.                             EL333
01634                                                                   EL333
01635      IF AT-VOID-DT NOT = LOW-VALUES                                  CL*15
01636          GO TO 1010-RETURN-FILE.                                     CL*15
01637                                                                      CL*15
01638      IF ((AT-PAYMENT-TYPE NOT = '2') AND                          EL333
01639         (AT-PAYMENT-ORIGIN NOT = '2'))                            EL333
01640         IF (AT-CHECK-WRITTEN-DT GREATER THAN BEGIN-BIN-DATE AND   EL333
01641             AT-CHECK-WRITTEN-DT NOT GREATER THAN END-BIN-DATE)    EL333
01642               ADD +1 TO WS-CURRENT-PMT-CNTR                       EL333
01643         ELSE                                                      EL333
01644               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01645                   ADD +1 TO WS-ITD-PMT-CNTR.                      EL333
01646                                                                   EL333
01647      MOVE AT-CHECK-WRITTEN-DT TO HOLD-FIRST-PMT-DATE.             EL333
01648                                                                   EL333
01649      IF ((AT-PAYMENT-TYPE = '2') AND                              EL333
01650         (AT-PAYMENT-ORIGIN NOT = '2'))                            EL333
01651         IF (AT-CHECK-WRITTEN-DT GREATER THAN BEGIN-BIN-DATE AND   EL333
01652             AT-CHECK-WRITTEN-DT NOT GREATER THAN END-BIN-DATE)    EL333
01653            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01654               ADD +1 TO TB-FINAL-PMTS (REPORT-INDEX)              EL333
01655            ELSE                                                   EL333
01656               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01657                  ADD +1 TO TB-FINAL-PMTS (REPORT-INDEX - 2)       EL333
01658               ELSE                                                EL333
01659                  NEXT SENTENCE                                    EL333
01660         ELSE                                                      EL333
01661            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01662               ADD +1 TO TB-FINAL-PMTS (REPORT-INDEX + 2)          EL333
01663            ELSE                                                   EL333
01664               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01665                   ADD +1 TO TB-FINAL-PMTS (REPORT-INDEX).         EL333
01666                                                                   EL333
01667      IF AT-PAYMENT-ORIGIN = '2'                                   EL333
01668         IF (AT-CHECK-WRITTEN-DT GREATER THAN BEGIN-BIN-DATE AND   EL333
01669             AT-CHECK-WRITTEN-DT NOT GREATER THAN END-BIN-DATE)    EL333
01670            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01671               ADD +1 TO TB-AUTO-PMTS (REPORT-INDEX)               EL333
01672            ELSE                                                   EL333
01673               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01674                   ADD +1 TO TB-AUTO-PMTS (REPORT-INDEX - 2)       EL333
01675               ELSE                                                EL333
01676                   NEXT SENTENCE                                   EL333
01677         ELSE                                                      EL333
01678            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01679               ADD +1 TO TB-AUTO-PMTS (REPORT-INDEX + 2)           EL333
01680            ELSE                                                   EL333
01681               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01682                   ADD +1 TO TB-AUTO-PMTS (REPORT-INDEX).          EL333
01683                                                                   EL333
01684      IF AT-PAYEE-TYPE = 'B'                                       EL333
01685         IF (AT-CHECK-WRITTEN-DT GREATER THAN BEGIN-BIN-DATE AND   EL333
01686             AT-CHECK-WRITTEN-DT NOT GREATER THAN END-BIN-DATE)    EL333
01687            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01688               ADD +1 TO TB-CNT-BENE-PMTS (REPORT-INDEX)           EL333
01689               ADD AT-AMOUNT-PAID                                  EL333
01690                               TO TB-BENE-PMTS (REPORT-INDEX)      EL333
01691            ELSE                                                   EL333
01692               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01693                  ADD +1 TO TB-CNT-BENE-PMTS (REPORT-INDEX - 2)    EL333
01694                  ADD AT-AMOUNT-PAID TO                            EL333
01695                      TB-BENE-PMTS (REPORT-INDEX - 2)              EL333
01696               ELSE                                                EL333
01697                  NEXT SENTENCE                                    EL333
01698         ELSE                                                      EL333
01699            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01700               ADD +1 TO TB-CNT-BENE-PMTS (REPORT-INDEX + 2)       EL333
01701               ADD AT-AMOUNT-PAID TO                               EL333
01702                   TB-BENE-PMTS (REPORT-INDEX + 2)                 EL333
01703            ELSE                                                   EL333
01704               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01705                  ADD +1 TO TB-CNT-BENE-PMTS (REPORT-INDEX)        EL333
01706                  ADD AT-AMOUNT-PAID                               EL333
01707                                TO TB-BENE-PMTS (REPORT-INDEX).    EL333
01708                                                                   EL333
01709      IF ((AT-AMOUNT-PAID GREATER THAN +0) AND                     EL333
01710         (AT-VOID-DT = LOW-VALUES))                                EL333
01711         IF (AT-CHECK-WRITTEN-DT GREATER THAN BEGIN-BIN-DATE AND   EL333
01712             AT-CHECK-WRITTEN-DT NOT GREATER THAN END-BIN-DATE)    EL333
01713            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01714               ADD +1 TO TB-CNT-CLAIM-PMTS (REPORT-INDEX)          EL333
01715               ADD AT-AMOUNT-PAID TO TB-CLAIM-PMTS (REPORT-INDEX)  EL333
01716               ADD AT-DAYS-IN-PERIOD TO                            EL333
01717                   TB-DAYS-PMT (REPORT-INDEX)                      EL333
01718            ELSE                                                   EL333
01719               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01720                  ADD +1 TO TB-CNT-CLAIM-PMTS (REPORT-INDEX - 2)   EL333
01721                  ADD AT-AMOUNT-PAID TO                            EL333
01722                      TB-CLAIM-PMTS (REPORT-INDEX - 2)             EL333
01723                  ADD AT-DAYS-IN-PERIOD TO                         EL333
01724                      TB-DAYS-PMT (REPORT-INDEX - 2)               EL333
01725               ELSE                                                EL333
01726                  NEXT SENTENCE                                    EL333
01727         ELSE                                                      EL333
01728            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01729               ADD +1 TO TB-CNT-CLAIM-PMTS (REPORT-INDEX + 2)      EL333
01730               ADD AT-AMOUNT-PAID TO                               EL333
01731                   TB-CLAIM-PMTS (REPORT-INDEX + 2)                EL333
01732               ADD AT-DAYS-IN-PERIOD TO                            EL333
01733                   TB-DAYS-PMT (REPORT-INDEX + 2)                  EL333
01734            ELSE                                                   EL333
01735               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01736                  ADD +1 TO TB-CNT-CLAIM-PMTS (REPORT-INDEX)       EL333
01737                  ADD AT-AMOUNT-PAID                               EL333
01738                              TO TB-CLAIM-PMTS (REPORT-INDEX)      EL333
01739                  ADD AT-DAYS-IN-PERIOD TO                         EL333
01740                      TB-DAYS-PMT (REPORT-INDEX).                  EL333
01741                                                                   EL333
01742      IF (AT-PAYEE-TYPE = 'I')                                     EL333
01743         IF (AT-CHECK-WRITTEN-DT GREATER THAN BEGIN-BIN-DATE AND   EL333
01744             AT-CHECK-WRITTEN-DT NOT GREATER THAN END-BIN-DATE)    EL333
01745            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01746               ADD +1 TO TB-CNT-ESTATE-PMTS (REPORT-INDEX)         EL333
01747               ADD AT-AMOUNT-PAID TO TB-ESTATE-PMTS (REPORT-INDEX) EL333
01748            ELSE                                                   EL333
01749               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01750                  ADD +1 TO TB-CNT-ESTATE-PMTS (REPORT-INDEX - 2)  EL333
01751                  ADD AT-AMOUNT-PAID TO                            EL333
01752                      TB-ESTATE-PMTS (REPORT-INDEX - 2)            EL333
01753               ELSE                                                EL333
01754                  NEXT SENTENCE                                    EL333
01755         ELSE                                                      EL333
01756            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01757               ADD +1 TO TB-CNT-ESTATE-PMTS (REPORT-INDEX + 2)     EL333
01758               ADD AT-AMOUNT-PAID TO                               EL333
01759                   TB-ESTATE-PMTS (REPORT-INDEX + 2)               EL333
01760            ELSE                                                   EL333
01761               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01762                  ADD +1 TO TB-CNT-ESTATE-PMTS (REPORT-INDEX)      EL333
01763                  ADD AT-AMOUNT-PAID                               EL333
01764                               TO TB-ESTATE-PMTS (REPORT-INDEX).   EL333
01765                                                                   EL333
01766      GO TO 1010-RETURN-FILE.                                      EL333
01767                                                                   EL333
01768  3000-PROCESS-AUTO-PMTS.                                          EL333
01769      IF AT-TRAILER-TYPE NOT = '3'                                 EL333
01770         GO TO 4000-PROCESS-CORRESPONDENCE.                        EL333
01771                                                                   EL333
01772      IF AT-RECORDED-DT GREATER THAN END-BIN-DATE                  EL333
01773          GO TO 1010-RETURN-FILE.                                  EL333
01774                                                                   EL333
01775      IF AT-VOID-DT NOT = LOW-VALUES                                  CL*15
01776          GO TO 1010-RETURN-FILE.                                     CL*15
01777                                                                      CL*15
01778      MOVE AT-SCHEDULE-START-DT     TO  DC-BIN-DATE-1.             EL333
01779      MOVE AT-SCHEDULE-END-DT       TO  DC-BIN-DATE-2.             EL333
01780      MOVE    '1'                   TO  DC-OPTION-CODE.            EL333
01781      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 EL333
01782                                                                   EL333
01783      IF NO-CONVERSION-ERROR                                       EL333
01784         ADD +1 TO TB-CNT-AUTO-BEG-END (REPORT-INDEX)              EL333
01785         ADD DC-ELAPSED-MONTHS TO TB-AUTO-BEG-END (REPORT-INDEX)   EL333
01786         IF DC-ODD-DAYS-OVER GREATER THAN +0                       EL333
01787            COMPUTE HOLD-ODD-DAYS-OVER =                           EL333
01788                    (DC-ODD-DAYS-OVER / +30)                       EL333
01789            ADD HOLD-ODD-DAYS-OVER TO                              EL333
01790                TB-AUTO-BEG-END (REPORT-INDEX).                    EL333
01791                                                                   EL333
01792      GO TO 1010-RETURN-FILE.                                      EL333
01793                                                                   EL333
01794  4000-PROCESS-CORRESPONDENCE.                                     EL333
01795      IF AT-TRAILER-TYPE NOT = '4'                                 EL333
01796         GO TO 5000-PROCESS-DENIAL.                                EL333
01797                                                                   EL333
01798      IF AT-RECORDED-DT GREATER THAN END-BIN-DATE                  EL333
01799          GO TO 1010-RETURN-FILE.                                  EL333
01800                                                                   EL333
01801      MOVE AT-LETTER-SENT-DT        TO  DC-BIN-DATE-1.             EL333
01802      MOVE AT-LETTER-ANSWERED-DT    TO  DC-BIN-DATE-2.             EL333
01803      MOVE '1'                      TO  DC-OPTION-CODE.            EL333
01804      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 EL333
01805                                                                   EL333
01806      IF NO-CONVERSION-ERROR                                       EL333
01807         ADD +1 TO TB-CNT-LETR-RESP (REPORT-INDEX)                 EL333
01808         ADD DC-ELAPSED-MONTHS TO TB-LETR-RESP (REPORT-INDEX)      EL333
01809         IF DC-ODD-DAYS-OVER GREATER THAN +0                       EL333
01810            COMPUTE HOLD-ODD-DAYS-OVER =                           EL333
01811                    (DC-ODD-DAYS-OVER / +30)                       EL333
01812            ADD HOLD-ODD-DAYS-OVER TO                              EL333
01813                TB-LETR-RESP (REPORT-INDEX).                       EL333
01814                                                                   EL333
01815      IF AT-LETTER-SENT-DT NOT = LOW-VALUES                        EL333
01816         IF (AT-LETTER-SENT-DT GREATER THAN BEGIN-BIN-DATE AND     EL333
01817             AT-LETTER-SENT-DT NOT GREATER THAN END-BIN-DATE)      EL333
01818            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01819               ADD +1 TO TB-LETRS-SENT (REPORT-INDEX)              EL333
01820            ELSE                                                   EL333
01821               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01822                   ADD +1 TO TB-LETRS-SENT (REPORT-INDEX - 2)      EL333
01823               ELSE                                                EL333
01824                   NEXT SENTENCE                                   EL333
01825         ELSE                                                      EL333
01826            IF ESTABLISHED-WITHIN-PERIOD                           EL333
01827               ADD +1 TO TB-LETRS-SENT (REPORT-INDEX + 2)          EL333
01828            ELSE                                                   EL333
01829               IF CLAIM-WITHIN-SPECIFIED-RANGE                     EL333
01830                   ADD +1 TO TB-LETRS-SENT (REPORT-INDEX).         EL333
01831                                                                   EL333
01832      GO TO 1010-RETURN-FILE.                                      EL333
01833                                                                   EL333
01834  5000-PROCESS-DENIAL.                                             EL333
01835      IF AT-TRAILER-TYPE NOT = '8'                                 EL333
01836         GO TO 1010-RETURN-FILE.                                   EL333
01837                                                                   EL333
01838      IF AT-RECORDED-DT GREATER THAN END-BIN-DATE                  EL333
01839          GO TO 1010-RETURN-FILE.                                  EL333
01840                                                                   EL333
01841      IF (CLAIM-DENIED)  AND                                       EL333
01842         (AT-RETRACTION-DT = LOW-VALUES  AND                       EL333
01843          AT-DENIAL-DT NOT = LOW-VALUES)                           EL333
01844          IF DENIED-STATUS IS EQUAL TO '1'                         EL333
01845              GO TO 1010-RETURN-FILE.                              EL333
01846                                                                   EL333
01847      MOVE '1'                    TO  DENIED-STATUS.               EL333
01848                                                                   EL333
01849      IF    CLAIM-DENIED                                           EL333
01850        AND AT-RETRACTION-DT = LOW-VALUES                          EL333
01851        AND AT-DENIAL-DT NOT = LOW-VALUES                          EL333
01852            IF   (AT-DENIAL-DT GREATER THAN BEGIN-BIN-DATE         EL333
01853              AND AT-DENIAL-DT NOT GREATER THAN END-BIN-DATE)      EL333
01854                  IF  ESTABLISHED-WITHIN-PERIOD                    EL333
01855                      ADD +1 TO TB-DEN-CLAIMS (REPORT-INDEX)       EL333
01856                  ELSE                                             EL333
01857                  IF CLAIM-WITHIN-SPECIFIED-RANGE                  EL333
01858                      ADD +1 TO TB-DEN-CLAIMS (REPORT-INDEX - 2)   EL333
01859                  ELSE                                             EL333
01860                      NEXT SENTENCE                                EL333
01861            ELSE                                                   EL333
01862                IF  ESTABLISHED-WITHIN-PERIOD                      EL333
01863                    ADD +1 TO TB-DEN-CLAIMS (REPORT-INDEX + 2)     EL333
01864                ELSE                                               EL333
01865                  IF CLAIM-WITHIN-SPECIFIED-RANGE                  EL333
01866                    ADD +1 TO TB-DEN-CLAIMS (REPORT-INDEX).        EL333
01867                                                                   EL333
01868      GO TO 1010-RETURN-FILE.                                      EL333
01869                                                                   EL333
01870  6000-REVIEW-OPEN-CLAIM.                                          EL333
01871                                                                   EL333
01872      IF CL-LAST-CLOSE-DT EQUAL LOW-VALUES                         EL333
01873         ADD +1   TO TB-EXST-OPEN (REPORT-INDEX - 2)               EL333
01874         GO TO 6000-EXIT.                                          EL333
01875                                                                   EL333
01876      IF (CL-LAST-REOPEN-DT NOT EQUAL LOW-VALUES AND SPACES)       EL333
01877           IF CL-LAST-REOPEN-DT NOT GREATER THAN BEGIN-BIN-DATE    EL333
01878             ADD +1 TO TB-EXST-OPEN (REPORT-INDEX - 2)             EL333
01879             GO TO 6000-EXIT.                                      EL333
01880                                                                   EL333
01881  6000-EXIT.                                                       EL333
01882       EXIT.                                                          CL**5
01883       EJECT                                                          CL*15
01884                                                                   EL333
01885  6100-REVIEW-CLAIM.                                               EL333
01886                                                                   EL333
01887      IF LAST-RECORD-TYPE = 'CL'                                   EL333
01888         DISPLAY '  MISSING CERT INFO ' CL-CONTROL-PRIMARY         EL333
01889         DISPLAY '  MISSING CERT INFO ' UPON CONSOLE               EL333
01890         DISPLAY '  BYPASSING CLAIM   '                            EL333
01891         DISPLAY '  BYPASSING CLAIM   ' UPON CONSOLE               EL333
01892         GO TO 6100-EXIT.                                          EL333
01893                                                                   EL333
01894      MOVE HIR-CLAIM-RECORD TO CLAIM-MASTER                        EL333
01895      MOVE 'CL'             TO LAST-RECORD-TYPE                    EL333
01896                                                                   EL333
01897      IF DTE-TOT-OPT = '2'                                            CL*15
01898          IF CL-FILE-ESTABLISH-DT NOT LESS THAN BEGIN-YTD-BIN-DATE    CL*15
01899                             AND  NOT GREATER THAN END-BIN-DATE       CL*15
01900              MOVE 'Y'           TO  RELEASE-SW                       CL*15
01901          ELSE                                                        CL*15
01902              MOVE 'N'           TO  RELEASE-SW                       CL*15
01903              GO TO 6100-EXIT.                                        CL*15
01904                                                                      CL*15
01905      IF (CLAIM-IS-CLOSED                                          EL333
01906          AND CL-LAST-CLOSE-DT EQUAL LOW-VALUES OR SPACES)         EL333
01907            IF CL-LAST-MAINT-DT  EQUAL LOW-VALUES OR SPACES        EL333
01908                MOVE CL-FILE-ESTABLISH-DT TO CL-LAST-CLOSE-DT      EL333
01909            ELSE                                                   EL333
01910                MOVE CL-LAST-MAINT-DT    TO CL-LAST-CLOSE-DT.      EL333
01911                                                                   EL333
01912      IF CLAIM-IS-CLOSED                                           EL333
01913         GO TO  6100-EXIT.                                         EL333
01914                                                                   EL333
01915      IF (CL-LAST-REOPEN-DT NOT EQUAL LOW-VALUES AND SPACES)       EL333
01916          IF  (CL-LAST-CLOSE-DT  EQUAL LOW-VALUES OR SPACES)       EL333
01917             MOVE LOW-VALUES     TO CL-LAST-REOPEN-DT.             EL333
01918                                                                   EL333
01919      IF RECORD-HAS-BEEN-PURGED                                    EL333
01920           MOVE 'C'               TO CL-CLAIM-STATUS               EL333
01921           MOVE CL-FILE-ESTABLISH-DT TO CL-LAST-CLOSE-DT           EL333
01922      ELSE                                                         EL333
01923           IF CL-PURGED-DT NOT EQUAL LOW-VALUES AND SPACES         EL333
01924              MOVE 'C'               TO CL-CLAIM-STATUS            EL333
01925              MOVE CL-PURGED-DT      TO CL-LAST-CLOSE-DT.          EL333
01926                                                                   EL333
01927  6100-EXIT.                                                       EL333
01928       EXIT.                                                          CL**5
01929       EJECT                                                          CL*15
01930                                                                      CL**5
01931  7000-REPORT-BREAK-ROUTINE.                                       EL333
01932      IF DTE-PGM-OPT = 1 OR 3 OR 5 OR 6                            EL333
01933         MOVE SPACES TO TOTAL-DESC                                 EL333
01934         MOVE LAST-SR-KEY TO TOTAL-VALUE                           EL333
01935      ELSE                                                         EL333
01936          IF DTE-PGM-OPT = 2                                       EL333
01937             MOVE LAST-SR-KEY TO LAS-ST                            EL333
01938                                 TOTAL-DESC                        EL333
01939             PERFORM 8000-STATE-LOOK-UP THRU 8099-EXIT             EL333
01940          ELSE                                                     EL333
01941              IF DTE-PGM-OPT = 4                                   EL333
01942                 MOVE LAST-SR-KEY TO CLAS-LOOK                     EL333
01943                                     TOTAL-DESC                    EL333
01944                 PERFORM 8100-BENEFIT-LOOK-UP THRU 8199-EXIT.      EL333
01945                                                                   EL333
01946      MOVE SPACES       TO WS-DETAIL1.                             EL333
01947      SET  REPORT-INDEX TO +1.                                     EL333
01948      SET  GRAND-INDEX  TO +1.                                     EL333
01949      SET  PRINT-INDEX  TO +1.                                     EL333
01950      MOVE +1           TO DETAIL-SUB.                             EL333
01951                                                                   EL333
01952      IF TOTAL-DESCRIPTION = 'GRAND TOTALS        '                EL333
01953         NEXT SENTENCE                                             EL333
01954      ELSE                                                         EL333
01955         SET GRAND-INDEX TO +1                                     EL333
01956         PERFORM 7200-ACCUMULATE-GRAND-TOTALS THRU 7299-EXIT       EL333
01957                 VARYING REPORT-INDEX FROM +1 BY +1                EL333
01958                 UNTIL REPORT-INDEX GREATER THAN +4.               EL333
01959                                                                   EL333
01960      SET REPORT-INDEX TO +1.                                      EL333
01961                                                                   EL333
01962      ADD CORRESPONDING REST-OF-RECORD (REPORT-INDEX) TO           EL333
01963          REST-OF-RECORD (REPORT-INDEX + 2).                       EL333
01964                                                                   EL333
01965  7000-CONT-REPORT-BREAK.                                          EL333
01966                                                                   EL333
01967      ADD CORRESPONDING REST-OF-RECORD (REPORT-INDEX + 1) TO       EL333
01968          REST-OF-RECORD (REPORT-INDEX + 3).                       EL333
01969                                                                   EL333
01970      MOVE +99 TO WS-LINE-COUNT.                                   EL333
01971      MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION.       EL333
01972                                                                   EL333
01973  7001-INC-RPT-LOOP.                                               EL333
01974      IF DETAIL-SUB GREATER THAN +4                                EL333
01975         MOVE WS-SUB-HEADING1 TO  PRT                              EL333
01976         MOVE '0' TO X                                             EL333
01977         PERFORM WRITE-A-LINE                                      EL333
01978         MOVE WS-DETAIL1 TO P-DATA                                 EL333
01979         MOVE ' ' TO X                                             EL333
01980         PERFORM WRITE-A-LINE                                      EL333
01981         MOVE SPACES TO WS-DETAIL1                                 EL333
01982         SET REPORT-INDEX TO +1                                    EL333
01983         SET PRINT-INDEX UP BY +1                                  EL333
01984         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
01985         MOVE +1 TO DETAIL-SUB                                     EL333
01986         GO TO 7002-RPT-EST.                                       EL333
01987                                                                   EL333
01988      MOVE ZEROS TO P-AMT-X (DETAIL-SUB).                          EL333
01989                                                                   EL333
01990      IF TB-CNT-INC-RPT (REPORT-INDEX) GREATER THAN +0             EL333
01991         DIVIDE TB-INC-RPT (REPORT-INDEX) BY                       EL333
01992                TB-CNT-INC-RPT (REPORT-INDEX)                      EL333
01993                GIVING WORK-MONTH-DAY                              EL333
01994         COMPUTE WORK-DAY ROUNDED = (WORK-DAY * .30)               EL333
01995         COMPUTE WORK-MONTH-DAY   = (WORK-MONTH-DAY * 100)         EL333
01996         MOVE WORK-MONTH TO P-AMT-X (DETAIL-SUB).                  EL333
01997                                                                   EL333
01998      SET REPORT-INDEX UP BY +1.                                   EL333
01999      ADD +1 TO DETAIL-SUB.                                        EL333
02000      GO TO 7001-INC-RPT-LOOP.                                     EL333
02001                                                                   EL333
02002  7002-RPT-EST.                                                    EL333
02003      IF DETAIL-SUB GREATER THAN +4                                EL333
02004         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02005         MOVE ' ' TO X                                             EL333
02006         PERFORM WRITE-A-LINE                                      EL333
02007         MOVE SPACES TO WS-DETAIL1                                 EL333
02008         SET REPORT-INDEX TO +1                                    EL333
02009         SET PRINT-INDEX UP BY +1                                  EL333
02010         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02011         MOVE +1 TO DETAIL-SUB                                     EL333
02012         GO TO 7003-RPT-FRST-ACT.                                  EL333
02013                                                                   EL333
02014      MOVE ZEROS TO P-AMT-X (DETAIL-SUB).                          EL333
02015                                                                   EL333
02016      IF TB-CNT-RPT-EST (REPORT-INDEX) GREATER THAN +0             EL333
02017         DIVIDE TB-RPT-EST (REPORT-INDEX) BY                       EL333
02018                TB-CNT-RPT-EST (REPORT-INDEX)                      EL333
02019                GIVING WORK-MONTH-DAY                              EL333
02020         COMPUTE WORK-DAY ROUNDED = (WORK-DAY * .30)               EL333
02021         COMPUTE WORK-MONTH-DAY   = (WORK-MONTH-DAY * 100)         EL333
02022         MOVE WORK-MONTH TO P-AMT-X (DETAIL-SUB).                  EL333
02023                                                                   EL333
02024      SET REPORT-INDEX UP BY +1.                                   EL333
02025      ADD +1 TO DETAIL-SUB.                                        EL333
02026      GO TO 7002-RPT-EST.                                          EL333
02027                                                                   EL333
02028  7003-RPT-FRST-ACT.                                               EL333
02029      IF DETAIL-SUB GREATER THAN +4                                EL333
02030         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02031         MOVE ' ' TO X                                             EL333
02032         PERFORM WRITE-A-LINE                                      EL333
02033         MOVE SPACES TO WS-DETAIL1                                 EL333
02034         SET REPORT-INDEX TO +1                                    EL333
02035         SET PRINT-INDEX UP BY +1                                  EL333
02036         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02037         MOVE +1 TO DETAIL-SUB                                     EL333
02038         GO TO 7004-RPT-FRST-PMT.                                  EL333
02039                                                                   EL333
02040      MOVE ZEROS TO P-AMT-X (DETAIL-SUB).                          EL333
02041                                                                   EL333
02042      IF TB-CNT-RPT-FRST-ACT (REPORT-INDEX) GREATER THAN +0        EL333
02043         DIVIDE TB-RPT-FRST-ACT (REPORT-INDEX) BY                  EL333
02044                TB-CNT-RPT-FRST-ACT (REPORT-INDEX)                 EL333
02045                GIVING WORK-MONTH-DAY                              EL333
02046         COMPUTE WORK-DAY ROUNDED = (WORK-DAY * .30)               EL333
02047         COMPUTE WORK-MONTH-DAY   = (WORK-MONTH-DAY * 100)         EL333
02048         MOVE WORK-MONTH TO P-AMT-X (DETAIL-SUB).                  EL333
02049                                                                   EL333
02050      SET REPORT-INDEX UP BY +1.                                   EL333
02051      ADD +1 TO DETAIL-SUB.                                        EL333
02052      GO TO 7003-RPT-FRST-ACT.                                     EL333
02053                                                                   EL333
02054  7004-RPT-FRST-PMT.                                               EL333
02055      IF DETAIL-SUB GREATER THAN +4                                EL333
02056         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02057         MOVE ' ' TO X                                             EL333
02058         PERFORM WRITE-A-LINE                                      EL333
02059         MOVE SPACES TO WS-DETAIL1                                 EL333
02060         SET REPORT-INDEX TO +1                                    EL333
02061         SET PRINT-INDEX UP BY +1                                  EL333
02062         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02063         MOVE +1 TO DETAIL-SUB                                     EL333
02064         GO TO 7005-INC-LAST-PMT.                                  EL333
02065                                                                   EL333
02066      MOVE ZEROS TO P-AMT-X (DETAIL-SUB).                          EL333
02067                                                                   EL333
02068      IF TB-CNT-RPT-FRST-PMT (REPORT-INDEX) GREATER THAN +0        EL333
02069         DIVIDE TB-RPT-FRST-PMT (REPORT-INDEX) BY                  EL333
02070                TB-CNT-RPT-FRST-PMT (REPORT-INDEX)                 EL333
02071                GIVING WORK-MONTH-DAY                              EL333
02072         COMPUTE WORK-DAY ROUNDED = (WORK-DAY * .30)               EL333
02073         COMPUTE WORK-MONTH-DAY = (WORK-MONTH-DAY * 100)           EL333
02074         MOVE WORK-MONTH TO P-AMT-X (DETAIL-SUB).                  EL333
02075                                                                   EL333
02076      SET REPORT-INDEX UP BY +1.                                   EL333
02077      ADD +1 TO DETAIL-SUB.                                        EL333
02078      GO TO 7004-RPT-FRST-PMT.                                     EL333
02079                                                                   EL333
02080  7005-INC-LAST-PMT.                                               EL333
02081      IF DETAIL-SUB GREATER THAN +4                                EL333
02082         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02083         MOVE ' ' TO X                                             EL333
02084         PERFORM WRITE-A-LINE                                      EL333
02085         MOVE SPACES TO WS-DETAIL1                                 EL333
02086         SET REPORT-INDEX TO +1                                    EL333
02087         SET PRINT-INDEX UP BY +1                                  EL333
02088         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02089         MOVE +1 TO DETAIL-SUB                                     EL333
02090         GO TO 7006-EFF-INC.                                       EL333
02091                                                                   EL333
02092      MOVE ZEROS TO P-AMT-X (DETAIL-SUB).                          EL333
02093                                                                   EL333
02094      IF TB-CNT-INC-LST-PMT (REPORT-INDEX) GREATER THAN +0         EL333
02095         DIVIDE TB-INC-LST-PMT (REPORT-INDEX) BY                   EL333
02096                TB-CNT-INC-LST-PMT (REPORT-INDEX)                  EL333
02097                GIVING WORK-MONTH-DAY                              EL333
02098         COMPUTE WORK-DAY ROUNDED = (WORK-DAY * .30)               EL333
02099         COMPUTE WORK-MONTH-DAY = (WORK-MONTH-DAY * 100)           EL333
02100         MOVE WORK-MONTH TO P-AMT-X (DETAIL-SUB).                  EL333
02101                                                                   EL333
02102      SET REPORT-INDEX UP BY +1.                                   EL333
02103      ADD +1 TO DETAIL-SUB.                                        EL333
02104      GO TO 7005-INC-LAST-PMT.                                     EL333
02105                                                                   EL333
02106  7006-EFF-INC.                                                    EL333
02107      IF DETAIL-SUB GREATER THAN +4                                EL333
02108         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02109         MOVE ' ' TO X                                             EL333
02110         PERFORM WRITE-A-LINE                                      EL333
02111         MOVE SPACES TO WS-DETAIL1                                 EL333
02112         SET REPORT-INDEX TO +1                                    EL333
02113         SET PRINT-INDEX UP BY +1                                  EL333
02114         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02115         MOVE +1 TO DETAIL-SUB                                     EL333
02116         GO TO 7007-LETR-RESP.                                     EL333
02117                                                                   EL333
02118      MOVE ZEROS TO P-AMT-X (DETAIL-SUB).                          EL333
02119                                                                   EL333
02120      IF TB-CNT-EFF-INC (REPORT-INDEX) GREATER THAN +0             EL333
02121         DIVIDE TB-EFF-INC (REPORT-INDEX) BY                       EL333
02122                TB-CNT-EFF-INC (REPORT-INDEX)                      EL333
02123                GIVING WORK-MONTH-DAY                              EL333
02124         COMPUTE WORK-DAY ROUNDED = (WORK-DAY * .30)               EL333
02125         COMPUTE WORK-MONTH-DAY = (WORK-MONTH-DAY * 100)           EL333
02126         MOVE WORK-MONTH TO P-AMT-X (DETAIL-SUB).                  EL333
02127                                                                   EL333
02128      SET REPORT-INDEX UP BY +1.                                   EL333
02129      ADD +1 TO DETAIL-SUB.                                        EL333
02130      GO TO 7006-EFF-INC.                                          EL333
02131                                                                   EL333
02132  7007-LETR-RESP.                                                  EL333
02133      IF DETAIL-SUB GREATER THAN +4                                EL333
02134         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02135         MOVE ' ' TO X                                             EL333
02136         PERFORM WRITE-A-LINE                                      EL333
02137         MOVE SPACES TO WS-DETAIL1                                 EL333
02138         SET REPORT-INDEX TO +1                                    EL333
02139         SET PRINT-INDEX UP BY +1                                  EL333
02140         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02141         MOVE +1 TO DETAIL-SUB                                     EL333
02142         GO TO 7008-AUTO-BEG-END.                                  EL333
02143                                                                   EL333
02144      MOVE ZEROS TO P-AMT-X (DETAIL-SUB).                          EL333
02145                                                                   EL333
02146      IF TB-CNT-LETR-RESP (REPORT-INDEX) GREATER THAN +0           EL333
02147         DIVIDE TB-LETR-RESP (REPORT-INDEX) BY                     EL333
02148                TB-CNT-LETR-RESP (REPORT-INDEX)                    EL333
02149                GIVING WORK-MONTH-DAY                              EL333
02150         COMPUTE WORK-DAY ROUNDED = (WORK-DAY * .30)               EL333
02151         COMPUTE WORK-MONTH-DAY = (WORK-MONTH-DAY * 100)           EL333
02152         MOVE WORK-MONTH TO P-AMT-X (DETAIL-SUB).                  EL333
02153                                                                   EL333
02154      SET REPORT-INDEX UP BY +1.                                   EL333
02155      ADD +1 TO DETAIL-SUB.                                        EL333
02156      GO TO 7007-LETR-RESP.                                        EL333
02157                                                                   EL333
02158  7008-AUTO-BEG-END.                                               EL333
02159      IF DETAIL-SUB GREATER THAN +4                                EL333
02160         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02161         MOVE ' ' TO X                                             EL333
02162         PERFORM WRITE-A-LINE                                      EL333
02163         MOVE SPACES TO WS-DETAIL1                                 EL333
02164         SET REPORT-INDEX TO +1                                    EL333
02165         SET PRINT-INDEX UP BY +1                                  EL333
02166         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02167         MOVE +1 TO DETAIL-SUB                                     EL333
02168         MOVE ZEROS TO TB-EXST-OPEN (3)                            EL333
02169                       TB-EXST-OPEN (4)                            EL333
02170         GO TO 7009-EXST-OPEN.                                     EL333
02171                                                                   EL333
02172      MOVE ZEROS TO P-AMT-X (DETAIL-SUB).                          EL333
02173                                                                   EL333
02174      IF TB-CNT-AUTO-BEG-END (REPORT-INDEX) GREATER THAN +0        EL333
02175         DIVIDE TB-AUTO-BEG-END (REPORT-INDEX) BY                  EL333
02176                TB-CNT-AUTO-BEG-END (REPORT-INDEX)                 EL333
02177                GIVING WORK-MONTH-DAY                              EL333
02178         COMPUTE WORK-DAY ROUNDED = (WORK-DAY * .30)               EL333
02179         COMPUTE WORK-MONTH-DAY = (WORK-MONTH-DAY * 100)           EL333
02180         MOVE WORK-MONTH TO P-AMT-X (DETAIL-SUB).                  EL333
02181                                                                   EL333
02182      SET REPORT-INDEX UP BY +1.                                   EL333
02183      ADD +1 TO DETAIL-SUB.                                        EL333
02184      GO TO 7008-AUTO-BEG-END.                                     EL333
02185                                                                   EL333
02186  7009-EXST-OPEN.                                                  EL333
02187 *    IF DETAIL-SUB GREATER THAN +4                                EL333
02188      IF DETAIL-SUB GREATER THAN +2                                EL333
02189         MOVE WS-SUB-HEADING2            TO  PRT                   EL333
02190         MOVE '0' TO X                                             EL333
02191         PERFORM WRITE-A-LINE                                      EL333
02192         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02193         MOVE ' ' TO X                                             EL333
02194         PERFORM WRITE-A-LINE                                      EL333
02195         MOVE SPACES TO WS-DETAIL1                                 EL333
02196         SET REPORT-INDEX TO +1                                    EL333
02197         SET PRINT-INDEX UP BY +1                                  EL333
02198         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02199         MOVE +1 TO DETAIL-SUB                                     EL333
02200         GO TO 7009-NOW-OPEN.                                      EL333
02201                                                                   EL333
02202      MOVE TB-EXST-OPEN (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).    EL333
02203                                                                   EL333
02204      SET REPORT-INDEX UP BY +1.                                   EL333
02205      ADD +1 TO DETAIL-SUB.                                        EL333
02206      GO TO 7009-EXST-OPEN.                                        EL333
02207                                                                   EL333
02208  7009-NOW-OPEN.                                                   EL333
02209 *    IF DETAIL-SUB GREATER THAN +4                                EL333
02210      IF DETAIL-SUB GREATER THAN +2                                EL333
02211         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02212         MOVE ' ' TO X                                             EL333
02213         PERFORM WRITE-A-LINE                                      EL333
02214         MOVE SPACES TO WS-DETAIL1                                 EL333
02215         SET REPORT-INDEX TO +1                                    EL333
02216         SET PRINT-INDEX UP BY +1                                  EL333
02217         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02218         MOVE +1 TO DETAIL-SUB                                     EL333
02219         GO TO 7009-REOPENED.                                      EL333
02220                                                                   EL333
02221      MOVE TB-NOW-OPEN (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).     EL333
02222                                                                   EL333
02223      SET REPORT-INDEX UP BY +1.                                   EL333
02224      ADD +1 TO DETAIL-SUB.                                        EL333
02225      GO TO 7009-NOW-OPEN.                                         EL333
02226                                                                   EL333
02227  7009-REOPENED.                                                   EL333
02228      IF DETAIL-SUB GREATER THAN +4                                EL333
02229         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02230         MOVE ' ' TO X                                             EL333
02231         PERFORM WRITE-A-LINE                                      EL333
02232         MOVE SPACES TO WS-DETAIL1                                 EL333
02233         SET REPORT-INDEX TO +1                                    EL333
02234         SET PRINT-INDEX UP BY +1                                  EL333
02235         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02236         MOVE +1 TO DETAIL-SUB                                     EL333
02237         GO TO 7009-REMAINED.                                      EL333
02238                                                                   EL333
02239      MOVE TB-REOPENED (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).     EL333
02240                                                                   EL333
02241      SET REPORT-INDEX UP BY +1.                                   EL333
02242      ADD +1 TO DETAIL-SUB.                                        EL333
02243      GO TO 7009-REOPENED.                                         EL333
02244                                                                   EL333
02245  7009-REMAINED.                                                   EL333
02246      IF DETAIL-SUB GREATER THAN +4                                EL333
02247         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02248         MOVE ' ' TO X                                             EL333
02249         PERFORM WRITE-A-LINE                                      EL333
02250         MOVE SPACES TO WS-DETAIL1                                 EL333
02251         SET REPORT-INDEX TO +1                                    EL333
02252         SET PRINT-INDEX UP BY +1                                  EL333
02253         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02254         MOVE +1 TO DETAIL-SUB                                     EL333
02255         GO TO 7010-NEW-CLAIMS.                                    EL333
02256                                                                   EL333
02257      MOVE TB-REMAIN-REOPEN (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).EL333
02258                                                                   EL333
02259      SET REPORT-INDEX UP BY +1.                                   EL333
02260      ADD +1 TO DETAIL-SUB.                                        EL333
02261      GO TO 7009-REMAINED.                                         EL333
02262                                                                   EL333
02263  7010-NEW-CLAIMS.                                                 EL333
02264      IF DETAIL-SUB GREATER THAN +4                                EL333
02265         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02266         MOVE ' ' TO X                                             EL333
02267         PERFORM WRITE-A-LINE                                      EL333
02268         MOVE SPACES TO WS-DETAIL1                                 EL333
02269         SET REPORT-INDEX TO +1                                    EL333
02270         SET PRINT-INDEX UP BY +1                                  EL333
02271         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02272         MOVE +1 TO DETAIL-SUB                                     EL333
02273         GO TO 7010-NEW-CLOSED.                                    EL333
02274                                                                   EL333
02275      MOVE TB-NEW-CLAIMS (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).   EL333
02276                                                                   EL333
02277      SET REPORT-INDEX UP BY +1.                                   EL333
02278      ADD +1 TO DETAIL-SUB.                                        EL333
02279      GO TO 7010-NEW-CLAIMS.                                       EL333
02280                                                                   EL333
02281  7010-NEW-CLOSED.                                                 EL333
02282      IF DETAIL-SUB GREATER THAN +4                                EL333
02283         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02284         MOVE ' ' TO X                                             EL333
02285         PERFORM WRITE-A-LINE                                      EL333
02286         MOVE SPACES TO WS-DETAIL1                                 EL333
02287         SET REPORT-INDEX TO +1                                    EL333
02288         SET PRINT-INDEX UP BY +1                                  EL333
02289         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02290         MOVE +1 TO DETAIL-SUB                                     EL333
02291         GO TO 7011-CLO-CLAIMS.                                    EL333
02292                                                                   EL333
02293      MOVE TB-NEW-CLOSED (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).   EL333
02294                                                                   EL333
02295      SET REPORT-INDEX UP BY +1.                                   EL333
02296      ADD +1 TO DETAIL-SUB.                                        EL333
02297      GO TO 7010-NEW-CLOSED.                                       EL333
02298                                                                   EL333
02299  7011-CLO-CLAIMS.                                                 EL333
02300      IF DETAIL-SUB GREATER THAN +4                                EL333
02301         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02302         MOVE ' ' TO X                                             EL333
02303         PERFORM WRITE-A-LINE                                      EL333
02304         MOVE SPACES TO WS-DETAIL1                                 EL333
02305         SET REPORT-INDEX TO +1                                    EL333
02306         SET PRINT-INDEX UP BY +1                                  EL333
02307         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02308         MOVE +1 TO DETAIL-SUB                                     EL333
02309         GO TO 7011-AUTO-CLOS.                                     EL333
02310                                                                   EL333
02311      MOVE TB-CLO-CLAIMS (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).   EL333
02312                                                                   EL333
02313      SET REPORT-INDEX UP BY +1.                                   EL333
02314      ADD +1 TO DETAIL-SUB.                                        EL333
02315      GO TO 7011-CLO-CLAIMS.                                       EL333
02316                                                                   EL333
02317  7011-AUTO-CLOS.                                                  EL333
02318      IF DETAIL-SUB GREATER THAN +4                                EL333
02319         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02320         MOVE ' ' TO X                                             EL333
02321         PERFORM WRITE-A-LINE                                      EL333
02322         MOVE SPACES TO WS-DETAIL1                                 EL333
02323         SET REPORT-INDEX TO +1                                    EL333
02324         SET PRINT-INDEX UP BY +1                                  EL333
02325         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02326         MOVE +1 TO DETAIL-SUB                                     EL333
02327         GO TO 7011-PAID-CLOS.                                     EL333
02328                                                                   EL333
02329      MOVE TB-AUTO-CLOSE (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).   EL333
02330                                                                   EL333
02331      SET REPORT-INDEX UP BY +1.                                   EL333
02332      ADD +1 TO DETAIL-SUB.                                        EL333
02333      GO TO 7011-AUTO-CLOS.                                        EL333
02334                                                                   EL333
02335  7011-PAID-CLOS.                                                  EL333
02336      IF DETAIL-SUB GREATER THAN +4                                EL333
02337         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02338         MOVE ' ' TO X                                             EL333
02339         PERFORM WRITE-A-LINE                                      EL333
02340         MOVE SPACES TO WS-DETAIL1                                 EL333
02341         SET REPORT-INDEX TO +1                                    EL333
02342         SET PRINT-INDEX UP BY +1                                  EL333
02343         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02344         MOVE +1 TO DETAIL-SUB                                     EL333
02345         GO TO 7011-UNPAID-CLOS.                                   EL333
02346                                                                   EL333
02347      MOVE TB-PAID-CLOSE (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).   EL333
02348                                                                   EL333
02349      SET REPORT-INDEX UP BY +1.                                   EL333
02350      ADD +1 TO DETAIL-SUB.                                        EL333
02351      GO TO 7011-PAID-CLOS.                                        EL333
02352                                                                   EL333
02353  7011-UNPAID-CLOS.                                                EL333
02354      IF DETAIL-SUB GREATER THAN +4                                EL333
02355         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02356         MOVE ' ' TO X                                             EL333
02357         PERFORM WRITE-A-LINE                                      EL333
02358         MOVE SPACES TO WS-DETAIL1                                 EL333
02359         SET REPORT-INDEX TO +1                                    EL333
02360         SET PRINT-INDEX UP BY +1                                  EL333
02361         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02362         MOVE +1 TO DETAIL-SUB                                     EL333
02363         GO TO 7011-B-BENEFITS-CHANGED.                            EL333
02364                                                                   EL333
02365      MOVE TB-UNPAID-CLOSE (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB). EL333
02366                                                                   EL333
02367      SET REPORT-INDEX UP BY +1.                                   EL333
02368      ADD +1 TO DETAIL-SUB.                                        EL333
02369      GO TO 7011-UNPAID-CLOS.                                      EL333
02370                                                                   EL333
02371  7011-B-BENEFITS-CHANGED.                                         EL333
02372      IF DTE-CLIENT NOT = 'DMD'                                    EL333
02373         SET PRINT-INDEX UP BY +2                                  EL333
02374         GO TO 7012-DEN-CLAIMS.                                    EL333
02375                                                                   EL333
02376      IF DETAIL-SUB GREATER THAN +4                                EL333
02377         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02378         MOVE ' ' TO X                                             EL333
02379         PERFORM WRITE-A-LINE                                      EL333
02380         MOVE SPACES TO WS-DETAIL1                                 EL333
02381         SET REPORT-INDEX TO +1                                    EL333
02382         SET PRINT-INDEX UP BY +1                                  EL333
02383         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02384         MOVE +1 TO DETAIL-SUB                                     EL333
02385         GO TO 7011-C-SETUP-ERRORS.                                EL333
02386                                                                   EL333
02387      MOVE TB-BENEFITS-CHANGE (REPORT-INDEX)                       EL333
02388                                  TO P-AMT-Z (DETAIL-SUB).         EL333
02389                                                                   EL333
02390      SET REPORT-INDEX UP BY +1.                                   EL333
02391      ADD +1 TO DETAIL-SUB.                                        EL333
02392      GO TO 7011-B-BENEFITS-CHANGED.                               EL333
02393                                                                   EL333
02394  7011-C-SETUP-ERRORS.                                             EL333
02395      IF DETAIL-SUB GREATER THAN +4                                EL333
02396         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02397         MOVE ' ' TO X                                             EL333
02398         PERFORM WRITE-A-LINE                                      EL333
02399         MOVE SPACES TO WS-DETAIL1                                 EL333
02400         SET REPORT-INDEX TO +1                                    EL333
02401         SET PRINT-INDEX UP BY +1                                  EL333
02402         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02403         MOVE +1 TO DETAIL-SUB                                     EL333
02404         GO TO 7012-DEN-CLAIMS.                                    EL333
02405                                                                   EL333
02406      MOVE TB-SETUP-ERROR (REPORT-INDEX)                           EL333
02407                                  TO P-AMT-Z (DETAIL-SUB).         EL333
02408                                                                   EL333
02409      SET REPORT-INDEX UP BY +1.                                   EL333
02410      ADD +1 TO DETAIL-SUB.                                        EL333
02411      GO TO 7011-C-SETUP-ERRORS.                                   EL333
02412                                                                   EL333
02413  7012-DEN-CLAIMS.                                                 EL333
02414      IF DETAIL-SUB GREATER THAN +4                                EL333
02415         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02416         MOVE ' ' TO X                                             EL333
02417         PERFORM WRITE-A-LINE                                      EL333
02418         MOVE SPACES TO WS-DETAIL1                                 EL333
02419         SET REPORT-INDEX TO +1                                    EL333
02420         SET PRINT-INDEX UP BY +1                                  EL333
02421         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02422         MOVE +1 TO DETAIL-SUB                                     EL333
02423         GO TO 7013-OPEN-NO-COV.                                   EL333
02424                                                                   EL333
02425      MOVE TB-DEN-CLAIMS (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).   EL333
02426                                                                   EL333
02427      SET REPORT-INDEX UP BY +1.                                   EL333
02428      ADD +1 TO DETAIL-SUB.                                        EL333
02429      GO TO 7012-DEN-CLAIMS.                                       EL333
02430                                                                   EL333
02431  7013-OPEN-NO-COV.                                                EL333
02432      IF DETAIL-SUB GREATER THAN +4                                EL333
02433         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02434         MOVE ' ' TO X                                             EL333
02435         PERFORM WRITE-A-LINE                                      EL333
02436         MOVE SPACES TO WS-DETAIL1                                 EL333
02437         SET REPORT-INDEX TO +1                                    EL333
02438         SET PRINT-INDEX UP BY +1                                  EL333
02439         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02440         MOVE +1 TO DETAIL-SUB                                     EL333
02441         GO TO 7014-FRST-PMTS.                                     EL333
02442                                                                   EL333
02443      MOVE TB-OPEN-NO-COV (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).  EL333
02444                                                                   EL333
02445      SET REPORT-INDEX UP BY +1.                                   EL333
02446      ADD +1 TO DETAIL-SUB.                                        EL333
02447      GO TO 7013-OPEN-NO-COV.                                      EL333
02448                                                                   EL333
02449  7014-FRST-PMTS.                                                  EL333
02450      IF DETAIL-SUB GREATER THAN +4                                EL333
02451         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02452         MOVE ' ' TO X                                             EL333
02453         PERFORM WRITE-A-LINE                                      EL333
02454         MOVE SPACES TO WS-DETAIL1                                 EL333
02455         SET REPORT-INDEX TO +1                                    EL333
02456         SET PRINT-INDEX UP BY +1                                  EL333
02457         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02458         MOVE +1 TO DETAIL-SUB                                     EL333
02459         GO TO 7015-CONT-PMTS.                                     EL333
02460                                                                   EL333
02461      MOVE TB-FRST-PMTS (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).    EL333
02462                                                                   EL333
02463      SET REPORT-INDEX UP BY +1.                                   EL333
02464      ADD +1 TO DETAIL-SUB.                                        EL333
02465      GO TO 7014-FRST-PMTS.                                        EL333
02466                                                                   EL333
02467  7015-CONT-PMTS.                                                  EL333
02468      IF DETAIL-SUB GREATER THAN +4                                EL333
02469         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02470         MOVE ' ' TO X                                             EL333
02471         PERFORM WRITE-A-LINE                                      EL333
02472         MOVE SPACES TO WS-DETAIL1                                 EL333
02473         SET REPORT-INDEX TO +1                                    EL333
02474         SET PRINT-INDEX UP BY +1                                  EL333
02475         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02476         MOVE +1 TO DETAIL-SUB                                     EL333
02477         GO TO 7017-FINAL-PMTS.                                    EL333
02478                                                                   EL333
02479      MOVE TB-CONT-PMTS (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).    EL333
02480                                                                   EL333
02481      SET REPORT-INDEX UP BY +1.                                   EL333
02482      ADD +1 TO DETAIL-SUB.                                        EL333
02483      GO TO 7015-CONT-PMTS.                                        EL333
02484                                                                   EL333
02485  7017-FINAL-PMTS.                                                 EL333
02486      IF DETAIL-SUB GREATER THAN +4                                EL333
02487         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02488         MOVE ' ' TO X                                             EL333
02489         PERFORM WRITE-A-LINE                                      EL333
02490         MOVE SPACES TO WS-DETAIL1                                 EL333
02491         SET REPORT-INDEX TO +1                                    EL333
02492         SET PRINT-INDEX UP BY +1                                  EL333
02493         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02494         MOVE +1 TO DETAIL-SUB                                     EL333
02495         GO TO 7018-AUTO-PMTS.                                     EL333
02496                                                                   EL333
02497      MOVE TB-FINAL-PMTS (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).   EL333
02498                                                                   EL333
02499      SET REPORT-INDEX UP BY +1.                                   EL333
02500      ADD +1 TO DETAIL-SUB.                                        EL333
02501      GO TO 7017-FINAL-PMTS.                                       EL333
02502                                                                   EL333
02503  7018-AUTO-PMTS.                                                  EL333
02504      IF DETAIL-SUB GREATER THAN +4                                EL333
02505         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02506         MOVE ' ' TO X                                             EL333
02507         PERFORM WRITE-A-LINE                                      EL333
02508         MOVE SPACES TO WS-DETAIL1                                 EL333
02509         SET REPORT-INDEX TO +1                                    EL333
02510         SET PRINT-INDEX UP BY +1                                  EL333
02511         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02512         MOVE +1 TO DETAIL-SUB                                     EL333
02513         GO TO 7019-LETRS-SENT.                                    EL333
02514                                                                   EL333
02515      MOVE TB-AUTO-PMTS (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).    EL333
02516                                                                   EL333
02517      SET REPORT-INDEX UP BY +1.                                   EL333
02518      ADD +1 TO DETAIL-SUB.                                        EL333
02519      GO TO 7018-AUTO-PMTS.                                        EL333
02520                                                                   EL333
02521  7019-LETRS-SENT.                                                 EL333
02522      IF DETAIL-SUB GREATER THAN +4                                EL333
02523         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02524         MOVE ' ' TO X                                             EL333
02525         PERFORM WRITE-A-LINE                                      EL333
02526         MOVE SPACES TO WS-DETAIL1                                 EL333
02527         SET REPORT-INDEX TO +1                                    EL333
02528         SET PRINT-INDEX UP BY +1                                  EL333
02529         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02530         MOVE +1 TO DETAIL-SUB                                     EL333
02531         GO TO 7020-ACT-REC.                                       EL333
02532                                                                   EL333
02533      MOVE TB-LETRS-SENT (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).   EL333
02534                                                                   EL333
02535      SET REPORT-INDEX UP BY +1.                                   EL333
02536      ADD +1 TO DETAIL-SUB.                                        EL333
02537      GO TO 7019-LETRS-SENT.                                       EL333
02538                                                                   EL333
02539  7020-ACT-REC.                                                    EL333
02540      IF DETAIL-SUB GREATER THAN +4                                EL333
02541         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02542         MOVE ' ' TO X                                             EL333
02543         PERFORM WRITE-A-LINE                                      EL333
02544         MOVE SPACES TO WS-DETAIL1                                 EL333
02545         SET REPORT-INDEX TO +1                                    EL333
02546         SET PRINT-INDEX UP BY +1                                  EL333
02547         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02548         MOVE +1 TO DETAIL-SUB                                     EL333
02549         GO TO 7021-BENE-PMTS.                                     EL333
02550                                                                   EL333
02551      MOVE TB-ACT-REC (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).      EL333
02552                                                                   EL333
02553      SET REPORT-INDEX UP BY +1.                                   EL333
02554      ADD +1 TO DETAIL-SUB.                                        EL333
02555      GO TO 7020-ACT-REC.                                          EL333
02556                                                                   EL333
02557  7021-BENE-PMTS.                                                  EL333
02558      IF DETAIL-SUB GREATER THAN +4                                EL333
02559         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02560         MOVE ' ' TO X                                             EL333
02561         PERFORM WRITE-A-LINE                                      EL333
02562         MOVE SPACES TO WS-DETAIL1                                 EL333
02563         SET REPORT-INDEX TO +1                                    EL333
02564         SET PRINT-INDEX UP BY +1                                  EL333
02565         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02566         MOVE +1 TO DETAIL-SUB                                     EL333
02567         GO TO 7022-AVG-CLM-PMT.                                   EL333
02568                                                                   EL333
02569      MOVE TB-CNT-BENE-PMTS (REPORT-INDEX) TO P-AMT-Z (DETAIL-SUB).EL333
02570                                                                   EL333
02571      SET REPORT-INDEX UP BY +1.                                   EL333
02572      ADD +1 TO DETAIL-SUB.                                        EL333
02573      GO TO 7021-BENE-PMTS.                                        EL333
02574                                                                   EL333
02575  7022-AVG-CLM-PMT.                                                EL333
02576      IF DETAIL-SUB GREATER THAN +4                                EL333
02577         MOVE WS-SUB-HEADING3            TO  PRT                   EL333
02578         MOVE '0' TO X                                             EL333
02579         PERFORM WRITE-A-LINE                                      EL333
02580         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02581         MOVE ' ' TO X                                             EL333
02582         PERFORM WRITE-A-LINE                                      EL333
02583         MOVE SPACES TO WS-DETAIL1                                 EL333
02584         SET REPORT-INDEX TO +1                                    EL333
02585         SET PRINT-INDEX UP BY +1                                  EL333
02586         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02587         MOVE +1 TO DETAIL-SUB                                     EL333
02588         GO TO 7023-AVG-BENEFIT.                                   EL333
02589                                                                   EL333
02590      MOVE ZEROS TO P-AMT-9 (DETAIL-SUB).                          EL333
02591                                                                   EL333
02592                                                                   EL333
02593      IF TB-CNT-CLAIM-PMTS (REPORT-INDEX) GREATER THAN +0          EL333
02594         DIVIDE TB-CLAIM-PMTS (REPORT-INDEX) BY                    EL333
02595                TB-CNT-CLAIM-PMTS (REPORT-INDEX)                   EL333
02596           GIVING P-AMT-9 (DETAIL-SUB).                            EL333
02597                                                                   EL333
02598      SET REPORT-INDEX UP BY +1.                                   EL333
02599      ADD +1 TO DETAIL-SUB.                                        EL333
02600      GO TO 7022-AVG-CLM-PMT.                                      EL333
02601                                                                   EL333
02602  7023-AVG-BENEFIT.                                                EL333
02603      IF DETAIL-SUB GREATER THAN +4                                EL333
02604         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02605         MOVE ' ' TO X                                             EL333
02606         PERFORM WRITE-A-LINE                                      EL333
02607         MOVE SPACES TO WS-DETAIL1                                 EL333
02608         SET REPORT-INDEX TO +1                                    EL333
02609         SET PRINT-INDEX UP BY +1                                  EL333
02610         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02611         MOVE +1 TO DETAIL-SUB                                     EL333
02612         GO TO 7024-AVG-TERM.                                      EL333
02613                                                                   EL333
02614      MOVE ZEROS TO P-AMT-9 (DETAIL-SUB).                          EL333
02615                                                                   EL333
02616      IF TB-NEW-CLAIMS (REPORT-INDEX) GREATER THAN +0              EL333
02617         DIVIDE TB-BENEFIT (REPORT-INDEX) BY                       EL333
02618                TB-NEW-CLAIMS (REPORT-INDEX)                       EL333
02619           GIVING P-AMT-9 (DETAIL-SUB).                            EL333
02620                                                                   EL333
02621      SET REPORT-INDEX UP BY +1.                                   EL333
02622      ADD +1 TO DETAIL-SUB.                                        EL333
02623      GO TO 7023-AVG-BENEFIT.                                      EL333
02624                                                                   EL333
02625  7024-AVG-TERM.                                                   EL333
02626      IF DETAIL-SUB GREATER THAN +4                                EL333
02627         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02628         MOVE ' ' TO X                                             EL333
02629         PERFORM WRITE-A-LINE                                      EL333
02630         MOVE SPACES TO WS-DETAIL1                                 EL333
02631         SET REPORT-INDEX TO +1                                    EL333
02632         SET PRINT-INDEX UP BY +1                                  EL333
02633         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02634         MOVE +1 TO DETAIL-SUB                                     EL333
02635         GO TO 7025-AVG-AGE.                                       EL333
02636                                                                   EL333
02637      MOVE ZEROS TO P-AMT-9 (DETAIL-SUB).                          EL333
02638                                                                   EL333
02639      IF TB-NEW-CLAIMS (REPORT-INDEX) GREATER THAN +0              EL333
02640         DIVIDE TB-TERM (REPORT-INDEX) BY                          EL333
02641                TB-NEW-CLAIMS (REPORT-INDEX)                       EL333
02642           GIVING P-AMT-9 (DETAIL-SUB).                            EL333
02643                                                                   EL333
02644      SET REPORT-INDEX UP BY +1.                                   EL333
02645      ADD +1 TO DETAIL-SUB.                                        EL333
02646      GO TO 7024-AVG-TERM.                                         EL333
02647                                                                   EL333
02648  7025-AVG-AGE.                                                    EL333
02649      IF DETAIL-SUB GREATER THAN +4                                EL333
02650         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02651         MOVE ' ' TO X                                             EL333
02652         PERFORM WRITE-A-LINE                                      EL333
02653         MOVE SPACES TO WS-DETAIL1                                 EL333
02654         SET REPORT-INDEX TO +1                                    EL333
02655         SET PRINT-INDEX UP BY +1                                  EL333
02656         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02657         MOVE +1 TO DETAIL-SUB                                     EL333
02658         GO TO 7026-AVG-DAYS-PMT.                                  EL333
02659                                                                   EL333
02660      MOVE ZEROS TO P-AMT-9 (DETAIL-SUB).                          EL333
02661                                                                   EL333
02662      IF TB-NEW-CLAIMS (REPORT-INDEX) GREATER THAN +0              EL333
02663         DIVIDE TB-AGE (REPORT-INDEX) BY                           EL333
02664                TB-NEW-CLAIMS (REPORT-INDEX)                       EL333
02665           GIVING P-AMT-9 (DETAIL-SUB).                            EL333
02666                                                                   EL333
02667      SET REPORT-INDEX UP BY +1.                                   EL333
02668      ADD +1 TO DETAIL-SUB.                                        EL333
02669      GO TO 7025-AVG-AGE.                                          EL333
02670                                                                   EL333
02671  7026-AVG-DAYS-PMT.                                               EL333
02672      IF DETAIL-SUB GREATER THAN +4                                EL333
02673         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02674         MOVE ' ' TO X                                             EL333
02675         PERFORM WRITE-A-LINE                                      EL333
02676         MOVE SPACES TO WS-DETAIL1                                 EL333
02677         SET REPORT-INDEX TO +1                                    EL333
02678         SET PRINT-INDEX UP BY +1                                  EL333
02679         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02680         MOVE +1 TO DETAIL-SUB                                     EL333
02681         GO TO 7027-AVG-PMTS-CLAIM.                                EL333
02682                                                                   EL333
02683      MOVE ZEROS TO P-AMT-9 (DETAIL-SUB).                          EL333
02684                                                                   EL333
02685      IF TB-CNT-CLAIM-PMTS (REPORT-INDEX) GREATER THAN +0          EL333
02686         DIVIDE TB-DAYS-PMT (REPORT-INDEX) BY                      EL333
02687                TB-CNT-CLAIM-PMTS (REPORT-INDEX)                   EL333
02688           GIVING P-AMT-9 (DETAIL-SUB).                            EL333
02689                                                                   EL333
02690      SET REPORT-INDEX UP BY +1.                                   EL333
02691      ADD +1 TO DETAIL-SUB.                                        EL333
02692      GO TO 7026-AVG-DAYS-PMT.                                     EL333
02693                                                                   EL333
02694  7027-AVG-PMTS-CLAIM.                                             EL333
02695      IF DETAIL-SUB GREATER THAN +4                                EL333
02696         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02697         MOVE ' ' TO X                                             EL333
02698         PERFORM WRITE-A-LINE                                      EL333
02699         MOVE SPACES TO WS-DETAIL1                                 EL333
02700         SET REPORT-INDEX TO +1                                    EL333
02701         SET PRINT-INDEX UP BY +1                                  EL333
02702         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02703         MOVE +1 TO DETAIL-SUB                                     EL333
02704         GO TO 7028-AVG-TOT-PAID.                                  EL333
02705                                                                   EL333
02706      MOVE ZEROS TO P-AMT-9 (DETAIL-SUB).                          EL333
02707                                                                   EL333
02708      IF TB-NEW-CLAIMS (REPORT-INDEX) GREATER THAN +0              EL333
02709         DIVIDE TB-CNT-CLAIM-PMTS (REPORT-INDEX) BY                EL333
02710                TB-NEW-CLAIMS (REPORT-INDEX)                       EL333
02711           GIVING P-AMT-9 (DETAIL-SUB).                            EL333
02712                                                                   EL333
02713      SET REPORT-INDEX UP BY +1.                                   EL333
02714      ADD +1 TO DETAIL-SUB.                                        EL333
02715      GO TO 7027-AVG-PMTS-CLAIM.                                   EL333
02716                                                                   EL333
02717  7028-AVG-TOT-PAID.                                               EL333
02718      IF DETAIL-SUB GREATER THAN +4                                EL333
02719         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02720         MOVE ' ' TO X                                             EL333
02721         PERFORM WRITE-A-LINE                                      EL333
02722         MOVE SPACES TO WS-DETAIL1                                 EL333
02723         SET REPORT-INDEX TO +1                                    EL333
02724         SET PRINT-INDEX UP BY +1                                  EL333
02725         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02726         MOVE +1 TO DETAIL-SUB                                     EL333
02727         GO TO 7029-AVG-BENE-PMT.                                  EL333
02728                                                                   EL333
02729      MOVE ZEROS TO P-AMT-9 (DETAIL-SUB).                          EL333
02730                                                                   EL333
02731      IF TB-NEW-CLAIMS (REPORT-INDEX) GREATER THAN +0              EL333
02732         DIVIDE TB-CLAIM-PMTS (REPORT-INDEX) BY                    EL333
02733                TB-NEW-CLAIMS (REPORT-INDEX)                       EL333
02734           GIVING P-AMT-9 (DETAIL-SUB).                            EL333
02735                                                                   EL333
02736      SET REPORT-INDEX UP BY +1.                                   EL333
02737      ADD +1 TO DETAIL-SUB.                                        EL333
02738      GO TO 7028-AVG-TOT-PAID.                                     EL333
02739                                                                   EL333
02740  7029-AVG-BENE-PMT.                                               EL333
02741      IF DETAIL-SUB GREATER THAN +4                                EL333
02742         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02743         MOVE ' ' TO X                                             EL333
02744         PERFORM WRITE-A-LINE                                      EL333
02745         MOVE SPACES TO WS-DETAIL1                                 EL333
02746         SET REPORT-INDEX TO +1                                    EL333
02747         SET PRINT-INDEX UP BY +1                                  EL333
02748         MOVE PRINT-DESCRIPTION (PRINT-INDEX) TO P-DESCRIPTION     EL333
02749         MOVE +1 TO DETAIL-SUB                                     EL333
02750         GO TO 7030-AVG-ESTATE-PMT.                                EL333
02751                                                                   EL333
02752      MOVE ZEROS TO P-AMT-9 (DETAIL-SUB).                          EL333
02753                                                                   EL333
02754      IF TB-CNT-BENE-PMTS (REPORT-INDEX) GREATER THAN +0           EL333
02755         DIVIDE TB-BENE-PMTS (REPORT-INDEX) BY                     EL333
02756                TB-CNT-BENE-PMTS (REPORT-INDEX)                    EL333
02757           GIVING P-AMT-9 (DETAIL-SUB).                            EL333
02758                                                                   EL333
02759      SET REPORT-INDEX UP BY +1.                                   EL333
02760      ADD +1 TO DETAIL-SUB.                                        EL333
02761      GO TO 7029-AVG-BENE-PMT.                                     EL333
02762                                                                   EL333
02763  7030-AVG-ESTATE-PMT.                                             EL333
02764      IF DETAIL-SUB GREATER THAN +4                                EL333
02765         MOVE WS-DETAIL1 TO P-DATA                                 EL333
02766         MOVE ' ' TO X                                             EL333
02767         PERFORM WRITE-A-LINE                                      EL333
02768         MOVE SPACES TO WS-DETAIL1                                 EL333
02769         SET REPORT-INDEX TO +1                                    EL333
02770         MOVE +1 TO DETAIL-SUB                                     EL333
02771         GO TO 7099-EXIT.                                          EL333
02772                                                                   EL333
02773      MOVE ZEROS TO P-AMT-9 (DETAIL-SUB).                          EL333
02774                                                                   EL333
02775      IF TB-CNT-ESTATE-PMTS (REPORT-INDEX) GREATER THAN +0         EL333
02776         DIVIDE TB-ESTATE-PMTS (REPORT-INDEX) BY                   EL333
02777                TB-CNT-ESTATE-PMTS (REPORT-INDEX)                  EL333
02778           GIVING P-AMT-9 (DETAIL-SUB).                            EL333
02779                                                                   EL333
02780      SET REPORT-INDEX UP BY +1.                                   EL333
02781      ADD +1 TO DETAIL-SUB.                                        EL333
02782      GO TO 7030-AVG-ESTATE-PMT.                                   EL333
02783                                                                   EL333
02784  7099-EXIT.                                                       EL333
02785      EXIT.                                                        EL333
02786      EJECT                                                        EL333
02787                                                                   EL333
02788  7200-ACCUMULATE-GRAND-TOTALS.                                    EL333
02789      ADD TB-CNT-INC-RPT (REPORT-INDEX) TO                         EL333
02790          GR-CNT-INC-RPT (GRAND-INDEX).                            EL333
02791      ADD TB-CNT-RPT-EST (REPORT-INDEX) TO                         EL333
02792          GR-CNT-RPT-EST (GRAND-INDEX).                            EL333
02793      ADD TB-CNT-RPT-FRST-PMT (REPORT-INDEX) TO                    EL333
02794          GR-CNT-RPT-FRST-PMT (GRAND-INDEX).                       EL333
02795      ADD TB-CNT-RPT-FRST-ACT (REPORT-INDEX) TO                    EL333
02796          GR-CNT-RPT-FRST-ACT (GRAND-INDEX).                       EL333
02797      ADD TB-CNT-INC-LST-PMT (REPORT-INDEX) TO                     EL333
02798          GR-CNT-INC-LST-PMT (GRAND-INDEX).                        EL333
02799      ADD TB-CNT-EFF-INC (REPORT-INDEX) TO                         EL333
02800          GR-CNT-EFF-INC (GRAND-INDEX).                            EL333
02801      ADD TB-CNT-LETR-RESP (REPORT-INDEX) TO                       EL333
02802          GR-CNT-LETR-RESP (GRAND-INDEX).                          EL333
02803      ADD TB-CNT-AUTO-BEG-END (REPORT-INDEX) TO                    EL333
02804          GR-CNT-AUTO-BEG-END (GRAND-INDEX).                       EL333
02805      ADD TB-INC-RPT (REPORT-INDEX) TO                             EL333
02806          GR-INC-RPT (GRAND-INDEX).                                EL333
02807      ADD TB-RPT-EST (REPORT-INDEX) TO                             EL333
02808          GR-RPT-EST (GRAND-INDEX).                                EL333
02809      ADD TB-RPT-FRST-PMT (REPORT-INDEX) TO                        EL333
02810          GR-RPT-FRST-PMT (GRAND-INDEX).                           EL333
02811      ADD TB-RPT-FRST-ACT (REPORT-INDEX) TO                        EL333
02812          GR-RPT-FRST-ACT (GRAND-INDEX).                           EL333
02813      ADD TB-INC-LST-PMT (REPORT-INDEX) TO                         EL333
02814          GR-INC-LST-PMT (GRAND-INDEX).                            EL333
02815      ADD TB-EFF-INC (REPORT-INDEX) TO                             EL333
02816          GR-EFF-INC (GRAND-INDEX).                                EL333
02817      ADD TB-LETR-RESP (REPORT-INDEX) TO                           EL333
02818          GR-LETR-RESP (GRAND-INDEX).                              EL333
02819      ADD TB-AUTO-BEG-END (REPORT-INDEX) TO                        EL333
02820          GR-AUTO-BEG-END (GRAND-INDEX).                           EL333
02821      ADD TB-EXST-OPEN (REPORT-INDEX) TO                           EL333
02822          GR-EXST-OPEN (GRAND-INDEX).                              EL333
02823      ADD TB-NOW-OPEN (REPORT-INDEX)  TO                           EL333
02824          GR-NOW-OPEN (GRAND-INDEX).                               EL333
02825      ADD TB-REOPENED (REPORT-INDEX) TO                            EL333
02826          GR-REOPENED (GRAND-INDEX).                               EL333
02827      ADD TB-REMAIN-REOPEN (REPORT-INDEX) TO                       EL333
02828          GR-REMAIN-REOPEN (GRAND-INDEX).                          EL333
02829      ADD TB-NEW-CLAIMS (REPORT-INDEX) TO                          EL333
02830          GR-NEW-CLAIMS (GRAND-INDEX).                             EL333
02831      ADD TB-NEW-CLOSED (REPORT-INDEX) TO                          EL333
02832          GR-NEW-CLOSED (GRAND-INDEX).                             EL333
02833      ADD TB-CLO-CLAIMS (REPORT-INDEX) TO                          EL333
02834          GR-CLO-CLAIMS (GRAND-INDEX).                             EL333
02835      ADD TB-PAID-CLOSE (REPORT-INDEX) TO                          EL333
02836          GR-PAID-CLOSE (GRAND-INDEX).                             EL333
02837      ADD TB-UNPAID-CLOSE (REPORT-INDEX) TO                        EL333
02838          GR-UNPAID-CLOSE (GRAND-INDEX).                           EL333
02839      IF  DTE-CLIENT EQUAL 'DMD'                                   EL333
02840          ADD TB-BENEFITS-CHANGE (REPORT-INDEX) TO                 EL333
02841              GR-BENEFITS-CHANGE (GRAND-INDEX)                     EL333
02842          ADD TB-SETUP-ERROR (REPORT-INDEX) TO                     EL333
02843              GR-SETUP-ERROR (GRAND-INDEX).                        EL333
02844      ADD TB-DEN-CLAIMS (REPORT-INDEX) TO                          EL333
02845          GR-DEN-CLAIMS (GRAND-INDEX).                             EL333
02846      ADD TB-OPEN-NO-COV (REPORT-INDEX) TO                         EL333
02847          GR-OPEN-NO-COV (GRAND-INDEX).                            EL333
02848      ADD TB-FRST-PMTS (REPORT-INDEX) TO                           EL333
02849          GR-FRST-PMTS (GRAND-INDEX).                              EL333
02850      ADD TB-CONT-PMTS (REPORT-INDEX) TO                           EL333
02851          GR-CONT-PMTS (GRAND-INDEX).                              EL333
02852      ADD TB-AUTO-PMTS (REPORT-INDEX) TO                           EL333
02853          GR-AUTO-PMTS (GRAND-INDEX).                              EL333
02854      ADD TB-FINAL-PMTS (REPORT-INDEX) TO                          EL333
02855          GR-FINAL-PMTS (GRAND-INDEX).                             EL333
02856      ADD TB-AUTO-CLOSE (REPORT-INDEX) TO                          EL333
02857          GR-AUTO-CLOSE (GRAND-INDEX).                             EL333
02858      ADD TB-LETRS-SENT (REPORT-INDEX) TO                          EL333
02859          GR-LETRS-SENT (GRAND-INDEX).                             EL333
02860      ADD TB-ACT-REC (REPORT-INDEX) TO                             EL333
02861          GR-ACT-REC (GRAND-INDEX).                                EL333
02862      ADD TB-CNT-ESTATE-PMTS (REPORT-INDEX) TO                     EL333
02863          GR-CNT-ESTATE-PMTS (GRAND-INDEX).                        EL333
02864      ADD TB-CNT-BENE-PMTS (REPORT-INDEX) TO                       EL333
02865          GR-CNT-BENE-PMTS (GRAND-INDEX).                          EL333
02866      ADD TB-CNT-CLAIM-PMTS (REPORT-INDEX) TO                      EL333
02867          GR-CNT-CLAIM-PMTS (GRAND-INDEX).                         EL333
02868      ADD TB-CNT-BENEFIT (REPORT-INDEX) TO                         EL333
02869          GR-CNT-BENEFIT (GRAND-INDEX).                            EL333
02870      ADD TB-ESTATE-PMTS (REPORT-INDEX) TO                         EL333
02871          GR-ESTATE-PMTS (GRAND-INDEX).                            EL333
02872      ADD TB-BENE-PMTS (REPORT-INDEX) TO                           EL333
02873          GR-BENE-PMTS (GRAND-INDEX).                              EL333
02874      ADD TB-CLAIM-PMTS (REPORT-INDEX) TO                          EL333
02875          GR-CLAIM-PMTS (GRAND-INDEX).                             EL333
02876      ADD TB-BENEFIT (REPORT-INDEX) TO                             EL333
02877          GR-BENEFIT (GRAND-INDEX).                                EL333
02878      ADD TB-TERM (REPORT-INDEX) TO                                EL333
02879          GR-TERM (GRAND-INDEX).                                   EL333
02880      ADD TB-AGE (REPORT-INDEX) TO                                 EL333
02881          GR-AGE (GRAND-INDEX).                                    EL333
02882      ADD TB-DAYS-PMT (REPORT-INDEX) TO                            EL333
02883          GR-DAYS-PMT (GRAND-INDEX).                               EL333
02884                                                                   EL333
02885      SET GRAND-INDEX UP BY +1.                                    EL333
02886                                                                   EL333
02887  7299-EXIT.                                                       EL333
02888       EXIT.                                                          CL**5
02889                                                                   EL333
02890  7999-EXIT.                                                       EL333
02891      EXIT.                                                        EL333
02892      EJECT                                                        EL333
02893                                                                   EL333
02894  8000-STATE-LOOK-UP.                                              EL333
02895      MOVE CLAS-STARTS TO CLAS-INDEXS.                             EL333
02896                                                                   EL333
02897  8010-FIND-STATE-LOOP.                                            EL333
02898      IF CLAS-INDEXS GREATER THAN CLAS-MAXS                        EL333
02899         MOVE 'INVALID STATE' TO TOTAL-VALUE                       EL333
02900         GO TO 8099-EXIT.                                          EL333
02901                                                                   EL333
02902      IF LAS-ST NOT = STATE-SUB (CLAS-INDEXS)                      EL333
02903         ADD +1 TO CLAS-INDEXS                                     EL333
02904         GO TO 8010-FIND-STATE-LOOP.                               EL333
02905                                                                   EL333
02906      MOVE STATE-PIC (CLAS-INDEXS) TO TOTAL-VALUE.                 EL333
02907                                                                   EL333
02908  8099-EXIT.                                                       EL333
02909      EXIT.                                                        EL333
02910                                                                   EL333
02911  8100-BENEFIT-LOOK-UP.                                            EL333
02912      IF LAS-CLM-TYPE = AH-OVERRIDE-L1                             EL333
02913         GO TO 8130-FIND-AH-TYPE.                                  EL333
02914                                                                   EL333
02915      MOVE CLAS-STARTL TO CLAS-INDEXL.                             EL333
02916                                                                   EL333
02917      IF CLAS-MAXL = ZEROS                                         EL333
02918         MOVE TVM-LIFE-MSG1 TO TOTAL-VALUE                         EL333
02919         GO TO 8199-EXIT.                                          EL333
02920                                                                   EL333
02921      IF CLAS-LOOK = '00'                                          EL333
02922         MOVE TVM-LIFE-MSG2 TO TOTAL-VALUE                         EL333
02923         GO TO 8199-EXIT.                                          EL333
02924                                                                   EL333
02925  8110-FIND-LF-TYPE-LOOP.                                          EL333
02926      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        EL333
02927         MOVE TVM-LIFE-MSG1 TO TOTAL-VALUE                         EL333
02928         GO TO 8199-EXIT.                                          EL333
02929                                                                   EL333
02930      IF CLAS-I-BEN (CLAS-INDEXL) NOT = CLAS-LOOK                  EL333
02931         ADD +1 TO CLAS-INDEXL                                     EL333
02932         GO TO 8110-FIND-LF-TYPE-LOOP.                             EL333
02933                                                                   EL333
02934      MOVE CLAS-I-AB10 (CLAS-INDEXL) TO TOTAL-VALUE.               EL333
02935      GO TO 8199-EXIT.                                             EL333
02936                                                                   EL333
02937  8130-FIND-AH-TYPE.                                               EL333
02938      MOVE CLAS-STARTA TO CLAS-INDEXA.                             EL333
02939                                                                   EL333
02940      IF CLAS-MAXA = ZEROS                                         EL333
02941         MOVE TVM-AH-MSG1 TO TOTAL-VALUE                           EL333
02942         GO TO 8199-EXIT.                                          EL333
02943                                                                   EL333
02944      IF CLAS-LOOK = '00'                                          EL333
02945         MOVE TVM-AH-MSG2 TO TOTAL-VALUE                           EL333
02946         GO TO 8199-EXIT.                                          EL333
02947                                                                   EL333
02948  8140-FIND-A-TYPE-LOOP.                                           EL333
02949      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        EL333
02950         MOVE TVM-AH-MSG1 TO TOTAL-VALUE                           EL333
02951         GO TO 8199-EXIT.                                          EL333
02952                                                                   EL333
02953      IF CLAS-I-BEN (CLAS-INDEXA) NOT = CLAS-LOOK                  EL333
02954         ADD +1 TO CLAS-INDEXA                                     EL333
02955         GO TO 8140-FIND-A-TYPE-LOOP.                              EL333
02956                                                                   EL333
02957      MOVE CLAS-I-AB10 (CLAS-INDEXA) TO TOTAL-VALUE.               EL333
02958                                                                   EL333
02959  8199-EXIT.                                                       EL333
02960      EXIT.                                                        EL333
02961      EJECT                                                        EL333
02962  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL333
02963                                                                   EL333
02964  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL333
02965                                                                   EL333
02966  WRITE-HEADINGS SECTION.                                          EL333
02967                                                                   EL333
02968      MOVE WS-CURRENT-DATE        TO WS-H2-DATE.                   EL333
02969      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME.            EL333
02970      MOVE ALPH-DATE              TO WS-H3-DATE.                   EL333
02971                                                                   EL333
02972      ADD +1                      TO WS-PAGE.                      EL333
02973      MOVE WS-PAGE                TO WS-H3-PAGE.                   EL333
02974      MOVE PRT                    TO WS-SAVE-PRINT-RECORD.         EL333
02975      MOVE ZERO                   TO WS-LINE-COUNT.                EL333
02976                                                                   EL333
02977      MOVE WS-HEADING1            TO PRT.                          EL333
02978      MOVE '1'                    TO X.                            EL333
02979      PERFORM WRITE-PRINTER.                                       EL333
02980                                                                   EL333
02981      MOVE WS-HEADING2            TO PRT.                          EL333
02982      MOVE ' '                    TO X.                            EL333
02983      PERFORM WRITE-PRINTER.                                       EL333
02984                                                                   EL333
02985      MOVE WS-HEADING3            TO PRT.                          EL333
02986      MOVE ' '                    TO X.                            EL333
02987      PERFORM WRITE-PRINTER.                                       EL333
02988                                                                   EL333
02989      MOVE WS-HEADING4            TO PRT.                          EL333
02990      MOVE ' '                    TO X.                            EL333
02991      PERFORM WRITE-PRINTER.                                       EL333
02992                                                                   EL333
02993      MOVE WS-HEADING5            TO  PRT.                         EL333
02994      MOVE '0'                    TO  X.                           EL333
02995      PERFORM WRITE-PRINTER.                                       EL333
02996      MOVE WS-HEADING6            TO  PRT.                         EL333
02997      MOVE ' '                    TO  X.                           EL333
02998      PERFORM WRITE-PRINTER.                                       EL333
02999      MOVE +9                     TO  WS-LINE-COUNT.               EL333
03000      MOVE '0'                    TO  X.                           EL333
03001                                                                   EL333
03002  WHS-020. COPY ELCWHS2.                                           EL333
03003                                                                   EL333
03004  WRITE-PRINTER SECTION.                                           EL333
03005                             COPY ELCPRT2X.                        EL333
03006                                  EJECT                            EL333
03007  ABEND-PGM SECTION. COPY ELCABEND.                                EL333
