00001  IDENTIFICATION DIVISION.                                         11/20/98
00002                                                                   ECS159
00003  PROGRAM-ID.                 ECS159.                                 LV008
00004 *              PROGRAM CONVERTED BY                                  CL**1
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**1
00006 *              CONVERSION DATE 10/17/97 08:56:37.                    CL**1
00007 *                            VMOD=2.016.                             CL**1
00008 *AUTHOR.     LOGIC, INC.                                             CL**1
00009 *            DALLAS, TEXAS.                                          CL**1
00010 *DATE-COMPILED.                                                      CL**1
00011 *SECURITY.   *****************************************************   CL**1
00012 *            *                                                   *   CL**1
00013 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**1
00014 *            *                                                   *   CL**1
00015 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**1
00016 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**1
00017 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**1
00018 *            *                                                   *   CL**1
00019 *            *****************************************************   CL**1
00020                                                                      CL**1
00021 *REMARKS. PROGRAM READS THE EXTRACTS FROM ECS158 AND                 CL**1
00022 *    ACCUMULATES TOTALS FOR PRINTING AT CHANGE IN CARRIER, STATE     CL**1
00023 *    RATE DEVIATION, AND ACCOUNT.                                    CL**1
00024                                                                      CL**1
00025 *    COMMISSION CALL REPORT.                                         CL**1
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 200 TO 450
092602******************************************************************
00026                                                                      CL**1
00027                                                                      CL**1
00028                                                                      CL**1
00029  ENVIRONMENT DIVISION.                                               CL**1
00030  CONFIGURATION SECTION.                                              CL**1
00031  SPECIAL-NAMES.                                                      CL**1
00032      C02 IS LCP-CH2                                                  CL**1
00033      C03 IS LCP-CH3                                                  CL**1
00034      C04 IS LCP-CH4                                                  CL**1
00035      C05 IS LCP-CH5                                                  CL**1
00036      C06 IS LCP-CH6                                                  CL**1
00037      C07 IS LCP-CH7                                                  CL**1
00038      C08 IS LCP-CH8                                                  CL**1
00039      C09 IS LCP-CH9                                                  CL**1
00040      C10 IS LCP-CH10                                                 CL**1
00041      C11 IS LCP-CH11                                                 CL**1
00042      C12 IS LCP-CH12                                                 CL**1
00043      S01 IS LCP-P01                                                  CL**1
00044      S02 IS LCP-P02.                                                 CL**1
00045  INPUT-OUTPUT SECTION.                                               CL**1
00046  FILE-CONTROL.                                                       CL**1
00047                                                                      CL**1
00048      SELECT EXTRACT         ASSIGN TO SYS011-UT-2400-S-SYS011.       CL**1
00049      SELECT CALL-PRT        ASSIGN TO SYS008-UR-1403-S-SYS008.       CL**1
00050      SELECT DISK-DATE       ASSIGN TO SYS019-UT-FBA1-S-SYS019.       CL**1
00051      SELECT FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.       CL**1
00052      SELECT SORT-FILE       ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.      CL**1
00053                                                                      CL**1
00054      EJECT                                                           CL**1
00055  DATA DIVISION.                                                      CL**1
00056  FILE SECTION.                                                       CL**1
00057  FD  EXTRACT                                                         CL**1
00058      BLOCK CONTAINS 0 RECORDS
00059      RECORDING MODE F.                                               CL**1
00060                                                                      CL**1
00061  01  XTRACT                      PIC X(666).                         CL**1
00062                                                                      CL**1
00063      EJECT                                                           CL**1
00064  FD  CALL-PRT                                                        CL**1
00065                              COPY ELCPRTFD.                          CL**1
00066                                                                      CL**1
00067      EJECT                                                           CL**1
00068  FD  FICH                                                            CL**1
00069                              COPY ECSFICH.                           CL**1
00070                                                                      CL**1
00071  SD  SORT-FILE.                                                      CL**1
00072                                                                      CL**1
00073  01  SORT-RECORD.                                                    CL**1
00074      12  SORT-CLAIM-KEY      PIC X(35).                              CL**1
00075      12  FILLER              PIC X(631).                             CL**1
00076      EJECT                                                           CL**1
00077  FD  DISK-DATE                                                       CL**1
00078                              COPY ELCDTEFD.                          CL**1
00079      EJECT                                                           CL**1
00080  WORKING-STORAGE SECTION.                                            CL**1
00081  77  FILLER  PIC X(32) VALUE '********************************'.     CL**1
00082  77  FILLER  PIC X(32) VALUE '     ECS159 WORKING STORAGE     '.     CL**1
00083  77  FILLER  PIC X(32) VALUE '*****VMOD=2.016*****************'.     CL**1
00084                                                                      CL**1
00085  77  LCP-ASA                       PIC X.                            CL**1
00086  77  OB-COV                        PIC X       VALUE SPACES.         CL**1
00087  77  REIN-PRINT-SW                 PIC X       VALUE SPACES.         CL**1
00088  77  SV-INDEXL                     PIC S9(4)    COMP.                CL**1
00089  77  SV-INDEXA                     PIC S9(4)    COMP.                CL**1
00090  77  WS-CM-DT                      PIC 9(06)    VALUE ZEROS.         CL**1
00091  77  SAVE-YR-GP                    PIC S9(04)   COMP VALUE +0.       CL**1
00092  77  SKIP-PRINT-SW                 PIC X(01)   VALUE SPACES.         CL**1
00093  77  WS-PROCESS-SW                 PIC X(01)   VALUE SPACES.         CL**1
00094      88  SINGLE-PERIOD-PROCESS                 VALUE 'S'.            CL**1
00095  77  BLANK-LINE                    PIC X(132)  VALUE SPACES.         CL**1
00096                                                                      CL**1
00097  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL**1
00098  01  LCP-CURRENT-DATE-68.                                            CL**1
00099      05  LCP-MONTH                 PIC XX.                           CL**1
00100      05  FILLER                    PIC X VALUE '/'.                  CL**1
00101      05  LCP-DAY1                  PIC XX.                           CL**1
00102      05  FILLER                    PIC X VALUE '/'.                  CL**1
00103      05  LCP-YEAR                  PIC XX.                           CL**1
00104  01  LCP-DATE-NEW-74.                                                CL**1
00105      05  LCP-YEAR                  PIC XX.                           CL**1
00106      05  LCP-MONTH                 PIC XX.                           CL**1
00107      05  LCP-DAY1                  PIC XX.                           CL**1
00108                                                                      CL**1
00109  01  WS-CALL-REPORTS.                                                CL**1
00110      12  WS-A-REPORT             PIC X(01) VALUE 'X'.                CL**1
00111      12  WS-B-REPORT             PIC X(01) VALUE SPACES.             CL**1
00112      12  WS-C-REPORT             PIC X(01) VALUE SPACES.             CL**1
00113      12  WS-D-REPORT             PIC X(01) VALUE SPACES.             CL**1
00114      12  WS-E-REPORT             PIC X(01) VALUE SPACES.             CL**1
00115      12  WS-F-REPORT             PIC X(01) VALUE SPACES.             CL**1
00116      12  WS-G-REPORT             PIC X(01) VALUE SPACES.             CL**1
00117      12  WS-H-REPORT             PIC X(01) VALUE SPACES.             CL**1
00118      12  WS-I-REPORT             PIC X(01) VALUE SPACES.             CL**1
00119      12  WS-J-REPORT             PIC X(01) VALUE SPACES.             CL**1
00120      12  WS-K-REPORT             PIC X(01) VALUE SPACES.             CL**1
00121                                                                      CL**1
CIDMOD 01  WS-REPORT-TITLE           PIC X(10)   VALUE SPACES.
00122  01  WS-ABEND-AREA.                                                  CL**1
00123      12  WS-ABEND-FILE-STATUS    PIC X(02).                          CL**1
00124      12  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.            CL**1
00125      12  WS-RETURN-CODE          PIC S9(04) VALUE +0 COMP.           CL**1
00126      12  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.         CL**1
00127      12  PGM-SUB                 PIC S999  COMP VALUE +159.          CL**1
00128                                                                      CL**1
00129  01  WORK-ABEND-CODE.                                                CL**1
00130      12  WAC-1                   PIC X.                              CL**1
00131      12  WAC-2                   PIC X.                              CL**1
00132      12  WAC-3-4.                                                    CL**1
00133          16  WAC-3               PIC X.                              CL**1
00134          16  WAC-4               PIC X.                              CL**1
00135                                                                      CL**1
00136  01  WORK-AREAS.                                                     CL**1
00137      12  W-BUS-TYPE              PIC X(02).                          CL**1
00138      12  R-BUS-TYPE REDEFINES W-BUS-TYPE  PIC 9(02).                 CL**1
00139      12  WORK-AGE            PIC 999.                                CL**1
00140      12  WORK-TERM           PIC 999.                                CL**1
00141      12  WORK-RATE           PIC S99V9(5) COMP-3.                    CL**1
00142      12  WK-C1               PIC S9(7) COMP-3.                       CL**1
00143      12  WS-WORK-DATE.                                               CL**7
00144          16  WS-WORK-CCYY    PIC 9(4).                               CL**1
00145          16  WS-WORK-CCYR  REDEFINES  WS-WORK-CCYY.                  CL**1
00146              20  WS-WORK-CC  PIC 99.                                 CL**1
00147              20  WS-WORK-YR  PIC 99.                                 CL**1
00148          16  WS-WORK-MM      PIC 99.                                 CL**1
00149          16  WS-WORK-DD      PIC 99.                                 CL**1
00150      12  WORK-BEG-RES        PIC S9(07)V99 COMP-3 VALUE +0.          CL**1
00151      12  WORK-END-RES        PIC S9(07)V99 COMP-3 VALUE +0.          CL**1
00152                                                                      CL**1
00153      EJECT                                                           CL**1
00154                                 COPY ECSEXTCL.                       CL**1
00155      EJECT                                                           CL**1
00156                                                                      CL**1
00157  01  AH-ACCUMS          COMP-3  SYNC.                                CL**1
092602*    12  AH-NO-TYPE       OCCURS 01.                                 CL**1
092602     12  AH-NO-TYPE       OCCURS 07.                                 CL**1
00159          16  AH-YR-TYPE      OCCURS 03.                              CL**1
092602             20  AH-A-TYPE      OCCURS 450.                          CL**1
00161                  24  AH-A-PREM           PIC S9(9)V99.               CL**1
00162                  24  AH-A-RST-PRM        PIC S9(9)V99.               CL**1
00163                  24  AH-A-CREF           PIC S9(9)V99.               CL**1
00164                  24  AH-A-PRESA          PIC S9(9)V99.               CL**1
00165                  24  AH-A-PRESB          PIC S9(9)V99.               CL**1
00166                  24  AH-A-CLAIM          PIC S9(9)V99.               CL**1
00167                  24  AH-A-OVR-COMM       PIC S9(9)V99.               CL**1
00168                  24  AH-A-AGT-COMM       PIC S9(9)V99.               CL**1
00169                  24  AH-A-B-IBNR         PIC S9(9)V99.               CL**1
00170                  24  AH-A-E-IBNR         PIC S9(9)V99.               CL**1
00171                  24  AH-A-B-LOSS         PIC S9(9)V99.               CL**1
00172                  24  AH-A-E-LOSS         PIC S9(9)V99.               CL**1
00173                  24  AH-A-RETRO-PMTS     PIC S9(9)V99.               CL**1
00174                  24  AH-A-CNT            PIC S9(9).                  CL**1
00175                  24  AH-A-CLM-CNT        PIC S9(9).                  CL**1
00176                  24  AH-A-PD-CNT         PIC S9(9).                  CL**1
00177                  24  AH-A-PRT-IND        PIC 9.                      CL**1
00178                                                                      CL**1
092602*01  AH-TABLE-1         COMP-3  SYNC.                                CL**1
00180 *    12  FILLER           OCCURS 01.                                 CL**1
00181 *        16  FILLER          OCCURS 03.                              CL**1
092602*            20  FILLER         OCCURS 450.                          CL**1
00183 *                24  FILLER              PIC S9(9)V99.               CL**1
00184 *                24  FILLER              PIC S9(9)V99.               CL**1
00185 *                24  FILLER              PIC S9(9)V99.               CL**1
00186 *                24  FILLER              PIC S9(9)V99.               CL**1
00187 *                24  FILLER              PIC S9(9)V99.               CL**1
00188 *                24  FILLER              PIC S9(9)V99.               CL**1
00189 *                24  FILLER              PIC S9(9)V99.               CL**1
00190 *                24  FILLER              PIC S9(9)V99.               CL**1
00191 *                24  FILLER              PIC S9(9)V99.               CL**1
00192 *                24  FILLER              PIC S9(9)V99.               CL**1
00193 *                24  FILLER              PIC S9(9)V99.               CL**1
00194 *                24  FILLER              PIC S9(9)V99.               CL**1
00195 *                24  FILLER              PIC S9(9)V99.               CL**1
00196 *                24  FILLER              PIC S9(9).                  CL**1
00197 *                24  FILLER              PIC S9(9).                  CL**1
00198 *                24  FILLER              PIC S9(9).                  CL**1
00199 *                24  FILLER              PIC 9.                      CL**1
00200 *                                                                    CL**1
00201 *01  AH-TABLE-2         COMP-3  SYNC.                                CL**1
00202 *    12  FILLER           OCCURS 01.                                 CL**1
00203 *        16  FILLER          OCCURS 03.                              CL**1
00204 *            20  FILLER         OCCURS 200.                          CL**1
00205 *                24  FILLER              PIC S9(9)V99.               CL**1
00206 *                24  FILLER              PIC S9(9)V99.               CL**1
00207 *                24  FILLER              PIC S9(9)V99.               CL**1
00208 *                24  FILLER              PIC S9(9)V99.               CL**1
00209 *                24  FILLER              PIC S9(9)V99.               CL**1
00210 *                24  FILLER              PIC S9(9)V99.               CL**1
00211 *                24  FILLER              PIC S9(9)V99.               CL**1
00212 *                24  FILLER              PIC S9(9)V99.               CL**1
00213 *                24  FILLER              PIC S9(9)V99.               CL**1
00214 *                24  FILLER              PIC S9(9)V99.               CL**1
00215 *                24  FILLER              PIC S9(9)V99.               CL**1
00216 *                24  FILLER              PIC S9(9)V99.               CL**1
00217 *                24  FILLER              PIC S9(9)V99.               CL**1
00218 *                24  FILLER              PIC S9(9).                  CL**1
00219 *                24  FILLER              PIC S9(9).                  CL**1
00220 *                24  FILLER              PIC S9(9).                  CL**1
00221 *                24  FILLER              PIC 9.                      CL**1
00222 *                                                                    CL**1
00223 *01  AH-TABLE-3         COMP-3  SYNC.                                CL**1
00224 *    12  FILLER           OCCURS 01.                                 CL**1
00225 *        16  FILLER          OCCURS 03.                              CL**1
00226 *            20  FILLER         OCCURS 200.                          CL**1
00227 *                24  FILLER              PIC S9(9)V99.               CL**1
00228 *                24  FILLER              PIC S9(9)V99.               CL**1
00229 *                24  FILLER              PIC S9(9)V99.               CL**1
00230 *                24  FILLER              PIC S9(9)V99.               CL**1
00231 *                24  FILLER              PIC S9(9)V99.               CL**1
00232 *                24  FILLER              PIC S9(9)V99.               CL**1
00233 *                24  FILLER              PIC S9(9)V99.               CL**1
00234 *                24  FILLER              PIC S9(9)V99.               CL**1
00235 *                24  FILLER              PIC S9(9)V99.               CL**1
00236 *                24  FILLER              PIC S9(9)V99.               CL**1
00237 *                24  FILLER              PIC S9(9)V99.               CL**1
00238 *                24  FILLER              PIC S9(9)V99.               CL**1
00239 *                24  FILLER              PIC S9(9)V99.               CL**1
00240 *                24  FILLER              PIC S9(9).                  CL**1
00241 *                24  FILLER              PIC S9(9).                  CL**1
00242 *                24  FILLER              PIC S9(9).                  CL**1
00243 *                24  FILLER              PIC 9.                      CL**1
00244                                                                      CL**1
00245 *01  AH-TABLE-4         COMP-3  SYNC.                                CL**1
00246 *    12  FILLER           OCCURS 01.                                 CL**1
00247 *        16  FILLER          OCCURS 03.                              CL**1
00248 *            20  FILLER         OCCURS 200.                          CL**1
00249 *                24  FILLER              PIC S9(9)V99.               CL**1
00250 *                24  FILLER              PIC S9(9)V99.               CL**1
00251 *                24  FILLER              PIC S9(9)V99.               CL**1
00252 *                24  FILLER              PIC S9(9)V99.               CL**1
00253 *                24  FILLER              PIC S9(9)V99.               CL**1
00254 *                24  FILLER              PIC S9(9)V99.               CL**1
00255 *                24  FILLER              PIC S9(9)V99.               CL**1
00256 *                24  FILLER              PIC S9(9)V99.               CL**1
00257 *                24  FILLER              PIC S9(9)V99.               CL**1
00258 *                24  FILLER              PIC S9(9)V99.               CL**1
00259 *                24  FILLER              PIC S9(9)V99.               CL**1
00260 *                24  FILLER              PIC S9(9)V99.               CL**1
00261 *                24  FILLER              PIC S9(9)V99.               CL**1
00262 *                24  FILLER              PIC S9(9).                  CL**1
00263 *                24  FILLER              PIC S9(9).                  CL**1
00264 *                24  FILLER              PIC S9(9).                  CL**1
00265 *                24  FILLER              PIC 9.                      CL**1
00266                                                                      CL**1
00267 *01  AH-TABLE-5         COMP-3  SYNC.                                CL**1
00268 *    12  FILLER           OCCURS 01.                                 CL**1
00269 *        16  FILLER          OCCURS 03.                              CL**1
00270 *            20  FILLER         OCCURS 200.                          CL**1
00271 *                24  FILLER              PIC S9(9)V99.               CL**1
00272 *                24  FILLER              PIC S9(9)V99.               CL**1
00273 *                24  FILLER              PIC S9(9)V99.               CL**1
00274 *                24  FILLER              PIC S9(9)V99.               CL**1
00275 *                24  FILLER              PIC S9(9)V99.               CL**1
00276 *                24  FILLER              PIC S9(9)V99.               CL**1
00277 *                24  FILLER              PIC S9(9)V99.               CL**1
00278 *                24  FILLER              PIC S9(9)V99.               CL**1
00279 *                24  FILLER              PIC S9(9)V99.               CL**1
00280 *                24  FILLER              PIC S9(9)V99.               CL**1
00281 *                24  FILLER              PIC S9(9)V99.               CL**1
00282 *                24  FILLER              PIC S9(9)V99.               CL**1
00283 *                24  FILLER              PIC S9(9)V99.               CL**1
00284 *                24  FILLER              PIC S9(9).                  CL**1
00285 *                24  FILLER              PIC S9(9).                  CL**1
00286 *                24  FILLER              PIC S9(9).                  CL**1
00287 *                24  FILLER              PIC 9.                      CL**1
00288 *                                                                    CL**1
00289 *01  AH-TABLE-6         COMP-3  SYNC.                                CL**1
00290 *    12  FILLER           OCCURS 01.                                 CL**1
00291 *        16  FILLER          OCCURS 03.                              CL**1
00292 *            20  FILLER         OCCURS 200.                          CL**1
00293 *                24  FILLER              PIC S9(9)V99.               CL**1
00294 *                24  FILLER              PIC S9(9)V99.               CL**1
00295 *                24  FILLER              PIC S9(9)V99.               CL**1
00296 *                24  FILLER              PIC S9(9)V99.               CL**1
00297 *                24  FILLER              PIC S9(9)V99.               CL**1
00298 *                24  FILLER              PIC S9(9)V99.               CL**1
00299 *                24  FILLER              PIC S9(9)V99.               CL**1
00300 *                24  FILLER              PIC S9(9)V99.               CL**1
00301 *                24  FILLER              PIC S9(9)V99.               CL**1
00302 *                24  FILLER              PIC S9(9)V99.               CL**1
00303 *                24  FILLER              PIC S9(9)V99.               CL**1
00304 *                24  FILLER              PIC S9(9)V99.               CL**1
00305 *                24  FILLER              PIC S9(9)V99.               CL**1
00306 *                24  FILLER              PIC S9(9).                  CL**1
00307 *                24  FILLER              PIC S9(9).                  CL**1
00308 *                24  FILLER              PIC S9(9).                  CL**1
092602*                24  FILLER              PIC 9.                      CL**1
00310                                                                      CL**1
00311  01  LF-ACCUMS          COMP-3  SYNC.                                CL**1
092602     12  LF-NO-TYPE       OCCURS 07.                                 CL**1
00313          16  LF-YR-TYPE      OCCURS 03.                              CL**1
092602             20  LF-A-TYPE      OCCURS 450.                          CL**1
00315                  24  LF-A-PREM           PIC S9(9)V99.               CL**1
00316                  24  LF-A-RST-PRM        PIC S9(9)V99.               CL**1
00317                  24  LF-A-CREF           PIC S9(9)V99.               CL**1
00318                  24  LF-A-PRESA          PIC S9(9)V99.               CL**1
00319                  24  LF-A-PRESB          PIC S9(9)V99.               CL**1
00320                  24  LF-A-CLAIM          PIC S9(9)V99.               CL**1
00321                  24  LF-A-OVR-COMM       PIC S9(9)V99.               CL**1
00322                  24  LF-A-AGT-COMM       PIC S9(9)V99.               CL**1
00323                  24  LF-A-B-IBNR         PIC S9(9)V99.               CL**1
00324                  24  LF-A-E-IBNR         PIC S9(9)V99.               CL**1
00325                  24  LF-A-B-LOSS         PIC S9(9)V99.               CL**1
00326                  24  LF-A-E-LOSS         PIC S9(9)V99.               CL**1
00327                  24  LF-A-RETRO-PMTS     PIC S9(9)V99.               CL**1
00328                  24  LF-A-CNT            PIC S9(9).                  CL**1
00329                  24  LF-A-CLM-CNT        PIC S9(9).                  CL**1
00330                  24  LF-A-PD-CNT         PIC S9(9).                  CL**1
00331                  24  LF-A-PRT-IND        PIC 9.                      CL**1
00332                                                                      CL**1
092602*01  LF-TABLE-1         COMP-3  SYNC.                                CL**1
00334 *    12  FILLER           OCCURS 01.                                 CL**1
00335 *        16  FILLER          OCCURS 03.                              CL**1
00336 *            20  FILLER         OCCURS 200.                          CL**1
00337 *                24  FILLER              PIC S9(9)V99.               CL**1
00338 *                24  FILLER              PIC S9(9)V99.               CL**1
00339 *                24  FILLER              PIC S9(9)V99.               CL**1
00340 *                24  FILLER              PIC S9(9)V99.               CL**1
00341 *                24  FILLER              PIC S9(9)V99.               CL**1
00342 *                24  FILLER              PIC S9(9)V99.               CL**1
00343 *                24  FILLER              PIC S9(9)V99.               CL**1
00344 *                24  FILLER              PIC S9(9)V99.               CL**1
00345 *                24  FILLER              PIC S9(9)V99.               CL**1
00346 *                24  FILLER              PIC S9(9)V99.               CL**1
00347 *                24  FILLER              PIC S9(9)V99.               CL**1
00348 *                24  FILLER              PIC S9(9)V99.               CL**1
00349 *                24  FILLER              PIC S9(9)V99.               CL**1
00350 *                24  FILLER              PIC S9(9).                  CL**1
00351 *                24  FILLER              PIC S9(9).                  CL**1
00352 *                24  FILLER              PIC S9(9).                  CL**1
00353 *                24  FILLER              PIC 9.                      CL**1
00354                                                                      CL**1
00355 *01  LF-TABLE-2         COMP-3  SYNC.                                CL**1
00356 *    12  FILLER           OCCURS 01.                                 CL**1
00357 *        16  FILLER          OCCURS 03.                              CL**1
00358 *            20  FILLER         OCCURS 200.                          CL**1
00359 *                24  FILLER              PIC S9(9)V99.               CL**1
00360 *                24  FILLER              PIC S9(9)V99.               CL**1
00361 *                24  FILLER              PIC S9(9)V99.               CL**1
00362 *                24  FILLER              PIC S9(9)V99.               CL**1
00363 *                24  FILLER              PIC S9(9)V99.               CL**1
00364 *                24  FILLER              PIC S9(9)V99.               CL**1
00365 *                24  FILLER              PIC S9(9)V99.               CL**1
00366 *                24  FILLER              PIC S9(9)V99.               CL**1
00367 *                24  FILLER              PIC S9(9)V99.               CL**1
00368 *                24  FILLER              PIC S9(9)V99.               CL**1
00369 *                24  FILLER              PIC S9(9)V99.               CL**1
00370 *                24  FILLER              PIC S9(9)V99.               CL**1
00371 *                24  FILLER              PIC S9(9)V99.               CL**1
00372 *                24  FILLER              PIC S9(9).                  CL**1
00373 *                24  FILLER              PIC S9(9).                  CL**1
00374 *                24  FILLER              PIC S9(9).                  CL**1
00375 *                24  FILLER              PIC 9.                      CL**1
00376                                                                      CL**1
00377 *01  LF-TABLE-3         COMP-3  SYNC.                                CL**1
00378 *    12  FILLER           OCCURS 01.                                 CL**1
00379 *        16  FILLER          OCCURS 03.                              CL**1
00380 *            20  FILLER         OCCURS 200.                          CL**1
00381 *                24  FILLER              PIC S9(9)V99.               CL**1
00382 *                24  FILLER              PIC S9(9)V99.               CL**1
00383 *                24  FILLER              PIC S9(9)V99.               CL**1
00384 *                24  FILLER              PIC S9(9)V99.               CL**1
00385 *                24  FILLER              PIC S9(9)V99.               CL**1
00386 *                24  FILLER              PIC S9(9)V99.               CL**1
00387 *                24  FILLER              PIC S9(9)V99.               CL**1
00388 *                24  FILLER              PIC S9(9)V99.               CL**1
00389 *                24  FILLER              PIC S9(9)V99.               CL**1
00390 *                24  FILLER              PIC S9(9)V99.               CL**1
00391 *                24  FILLER              PIC S9(9)V99.               CL**1
00392 *                24  FILLER              PIC S9(9)V99.               CL**1
00393 *                24  FILLER              PIC S9(9)V99.               CL**1
00394 *                24  FILLER              PIC S9(9).                  CL**1
00395 *                24  FILLER              PIC S9(9).                  CL**1
00396 *                24  FILLER              PIC S9(9).                  CL**1
00397 *                24  FILLER              PIC 9.                      CL**1
00398                                                                      CL**1
00399 *01  LF-TABLE-4         COMP-3  SYNC.                                CL**1
00400 *    12  FILLER           OCCURS 01.                                 CL**1
00401 *        16  FILLER          OCCURS 03.                              CL**1
00402 *            20  FILLER         OCCURS 200.                          CL**1
00403 *                24  FILLER              PIC S9(9)V99.               CL**1
00404 *                24  FILLER              PIC S9(9)V99.               CL**1
00405 *                24  FILLER              PIC S9(9)V99.               CL**1
00406 *                24  FILLER              PIC S9(9)V99.               CL**1
00407 *                24  FILLER              PIC S9(9)V99.               CL**1
00408 *                24  FILLER              PIC S9(9)V99.               CL**1
00409 *                24  FILLER              PIC S9(9)V99.               CL**1
00410 *                24  FILLER              PIC S9(9)V99.               CL**1
00411 *                24  FILLER              PIC S9(9)V99.               CL**1
00412 *                24  FILLER              PIC S9(9)V99.               CL**1
00413 *                24  FILLER              PIC S9(9)V99.               CL**1
00414 *                24  FILLER              PIC S9(9)V99.               CL**1
00415 *                24  FILLER              PIC S9(9)V99.               CL**1
00416 *                24  FILLER              PIC S9(9).                  CL**1
00417 *                24  FILLER              PIC S9(9).                  CL**1
00418 *                24  FILLER              PIC S9(9).                  CL**1
00419 *                24  FILLER              PIC 9.                      CL**1
00420                                                                      CL**1
00421 *01  LF-TABLE-5         COMP-3  SYNC.                                CL**1
00422 *    12  FILLER           OCCURS 01.                                 CL**1
00423 *        16  FILLER          OCCURS 03.                              CL**1
00424 *            20  FILLER         OCCURS 200.                          CL**1
00425 *                24  FILLER              PIC S9(9)V99.               CL**1
00426 *                24  FILLER              PIC S9(9)V99.               CL**1
00427 *                24  FILLER              PIC S9(9)V99.               CL**1
00428 *                24  FILLER              PIC S9(9)V99.               CL**1
00429 *                24  FILLER              PIC S9(9)V99.               CL**1
00430 *                24  FILLER              PIC S9(9)V99.               CL**1
00431 *                24  FILLER              PIC S9(9)V99.               CL**1
00432 *                24  FILLER              PIC S9(9)V99.               CL**1
00433 *                24  FILLER              PIC S9(9)V99.               CL**1
00434 *                24  FILLER              PIC S9(9)V99.               CL**1
00435 *                24  FILLER              PIC S9(9)V99.               CL**1
00436 *                24  FILLER              PIC S9(9)V99.               CL**1
00437 *                24  FILLER              PIC S9(9)V99.               CL**1
00438 *                24  FILLER              PIC S9(9).                  CL**1
00439 *                24  FILLER              PIC S9(9).                  CL**1
00440 *                24  FILLER              PIC S9(9).                  CL**1
00441 *                24  FILLER              PIC 9.                      CL**1
00442                                                                      CL**1
00443 *01  LF-TABLE-6         COMP-3  SYNC.                                CL**1
00444 *    12  FILLER           OCCURS 01.                                 CL**1
00445 *        16  FILLER          OCCURS 03.                              CL**1
00446 *            20  FILLER         OCCURS 200.                          CL**1
00447 *                24  FILLER              PIC S9(9)V99.               CL**1
00448 *                24  FILLER              PIC S9(9)V99.               CL**1
00449 *                24  FILLER              PIC S9(9)V99.               CL**1
00450 *                24  FILLER              PIC S9(9)V99.               CL**1
00451 *                24  FILLER              PIC S9(9)V99.               CL**1
00452 *                24  FILLER              PIC S9(9)V99.               CL**1
00453 *                24  FILLER              PIC S9(9)V99.               CL**1
00454 *                24  FILLER              PIC S9(9)V99.               CL**1
00455 *                24  FILLER              PIC S9(9)V99.               CL**1
00456 *                24  FILLER              PIC S9(9)V99.               CL**1
00457 *                24  FILLER              PIC S9(9)V99.               CL**1
00458 *                24  FILLER              PIC S9(9)V99.               CL**1
00459 *                24  FILLER              PIC S9(9)V99.               CL**1
00460 *                24  FILLER              PIC S9(9).                  CL**1
00461 *                24  FILLER              PIC S9(9).                  CL**1
00462 *                24  FILLER              PIC S9(9).                  CL**1
00463 *                24  FILLER              PIC 9.                      CL**1
00464                                                                      CL**1
00465      EJECT                                                           CL**1
00466  01  FILLER.                                                         CL**1
00467      12  TX-LF-CLM-ADJUSTMENTS OCCURS 2 TIMES.                       CL**1
00468          16  FILLER OCCURS 4 TIMES.                                  CL**1
092602             20  FILLER OCCURS 450 TIMES.                            CL**1
00470                  24  WS-TOTAL-LF-PMTS   PIC S9(09)V99 COMP-3.        CL**1
00471                  24  WS-TOTAL-E-LF-RESV PIC S9(09)V99 COMP-3.        CL**1
00472                  24  WS-TOTAL-B-LF-RESV PIC S9(09)V99 COMP-3.        CL**1
00473                                                                      CL**1
00474  01  FILLER.                                                         CL**1
00475      12  TX-AH-CLM-ADJUSTMENTS OCCURS 2 TIMES.                       CL**1
00476          16  FILLER OCCURS 4 TIMES.                                  CL**1
092602             20  FILLER OCCURS 450 TIMES.                            CL**1
00478                  24  WS-TOTAL-AH-PMTS   PIC S9(09)V99 COMP-3.        CL**1
00479                  24  WS-TOTAL-E-AH-RESV PIC S9(09)V99 COMP-3.        CL**1
00480                  24  WS-TOTAL-B-AH-RESV PIC S9(09)V99 COMP-3.        CL**1
00481                                                                      CL**1
00482  01  MISC-WORK-AREAS     SYNC.                                       CL**1
00483      12  PRIMA-REFUND            PIC S9(09)V99   COMP-3  VALUE +0.   CL**1
00484                                                                      CL**1
00485      12  YR                      PIC S9999       COMP VALUE +3.      CL**1
00486      12  SJ                      PIC S9999       COMP.               CL**1
00487      12  SO                      PIC S9999       COMP.               CL**1
00488      12  SOA                     PIC S9999       COMP.               CL**1
00489      12  CA                      PIC S9999       COMP.               CL**1
00490      12  CB                      PIC S9999       COMP.               CL**1
00491      12  CE                      PIC S9999       COMP.               CL**1
00492      12  CG                      PIC S9999       COMP.               CL**1
00493      12  SI                      PIC S9999       COMP.               CL**1
00494      12  TX                      PIC S9999       COMP.               CL**1
00495      12  Z                       PIC S9(9)V99    COMP-3  VALUE +0.   CL**1
00496      12  WORK-PREM               PIC S9(7)V99    COMP-3  VALUE +0.   CL**1
00497      12  HOLD-PREM               PIC S9(9)V99    COMP-3  VALUE +0.   CL**1
00498      12  NO-SIGN                 PIC 99.                             CL**1
00499      12  PRT-SET                 PIC S999        COMP-3  VALUE +0.   CL**1
00500      12  CHECK-PRT-SET           PIC X           VALUE SPACES.       CL**1
00501      12  ZERO-PRT-SET            PIC X           VALUE SPACES.       CL**1
00502      12  Z-L.                                                        CL**1
092602         16  Z-L-1               PIC X(42300).                       CL**1
092602         16  Z-L-2               PIC X(42300).                       CL**1
092602         16  Z-L-3               PIC X(42300).                       CL**1
092602*    12  Z-M                     PIC X(4200).                        CL**1
092602     12  Z-M                     PIC X(9450).                        CL**1
092602*    12  Z-C                     PIC X(14400).                       CL**1
092602     12  Z-C                     PIC X(32400).                       CL**1
00508      12  TEMP-BUS-HEAD.                                              CL**1
00509          16  FILLER                  PIC X(17)   VALUE               CL**1
00510              ' BUSINESS TYPE - '.                                    CL**1
00511          16  TEMP-BUS-TYPE           PIC X(02).                      CL**1
00512          16  FILLER                  PIC X(06)   VALUE SPACES.       CL**1
00513          16  TEMP-BUS-DESC           PIC X(25).                      CL**1
00514          16  FILLER                  PIC X(35)   VALUE SPACES.       CL**1
00515                                                                      CL**1
00516      12  LR-CALC-AREA            COMP-3.                             CL**1
00517          16  LR-CALC-ITEMS   OCCURS 04.                              CL**1
00518              20  LR-CALC-PCT    PIC S9(9)V9(9).                      CL**1
00519                                                                      CL**1
00520      12  L-MEAN-AREA             COMP-3.                             CL**1
00521          16  TYPE-MEAN       OCCURS 07.                              CL**1
00522              20  YR-MEAN        OCCURS 03.                           CL**1
092602                 24  LIFE-MEAN     OCCURS 450.                       CL**1
00524                      28  MEANS           PIC S9(11)V99.              CL**1
00525      12  LF-CLM-FACTOR           PIC S9(2)V9(16)  COMP-3.            CL**1
00526                                                                      CL**1
00527      12  AH-CLM-FACTOR           PIC S9(2)V9(16)  COMP-3.            CL**1
00528                                                                      CL**1
00529      12  A-AREA                  COMP-3.                             CL**1
00530          16  PREM                PIC S9(9)V99.                       CL**1
00531          16  PRIM-FAC-EARN-PRM   PIC S9(9)V99.                       CL**1
00532          16  CREF                PIC S9(9)V99.                       CL**1
00533          16  BEGIN-PRM-RES       PIC S9(9)V99.                       CL**1
00534          16  END-PRM-RES         PIC S9(9)V99.                       CL**1
00535          16  CLAIM-AMT           PIC S9(9)V99.                       CL**1
00536          16  OVR-COMM            PIC S9(9)V99.                       CL**1
00537          16  AGT-COMM            PIC S9(9)V99.                       CL**1
00538          16  B-IBNR              PIC S9(9)V99.                       CL**1
00539          16  E-IBNR              PIC S9(9)V99.                       CL**1
00540          16  B-LOSS              PIC S9(9)V99.                       CL**1
00541          16  E-LOSS              PIC S9(9)V99.                       CL**1
00542          16  RETRO-PMTS          PIC S9(09)V99.                      CL**1
00543          16  CNT                 PIC S9(9).                          CL**1
00544          16  CLM-CNT             PIC S9(9).                          CL**1
00545          16  PD-CNT              PIC S9(9).                          CL**1
00546          16  PRT-IND             PIC 9.                              CL**1
00547                                                                      CL**1
00548      12  PAGE-CT                 PIC S9(5)      COMP-3  VALUE +0.    CL**1
00549      12  X                       PIC X.                              CL**1
00550      12  DAT-LIM OCCURS 3        PIC S9(5)      COMP-3.              CL**1
00551      12  DAT-MAX OCCURS 3        PIC S9(5)      COMP-3.              CL**1
00552      12  DTE                     PIC S9(5)      COMP-3.              CL**1
00553      12  WRK-C                   PIC S9(7)V99   COMP-3  VALUE +0.    CL**1
00554                                                                      CL**1
00555  01  ADD-AREAS                   COMP-3.                             CL**1
00556      12  CA-ADDS.                                                    CL**1
00557          16 CA-PREM               PIC S9(9)V99.                      CL**1
00558          16 CA-RST-PRM            PIC S9(9)V99.                      CL**1
00559          16 CA-REF                PIC S9(9)V99.                      CL**1
00560          16 CA-RES-A              PIC S9(9)V99.                      CL**1
00561          16 CA-RES-B              PIC S9(9)V99.                      CL**1
00562          16 CA-CLAIM              PIC S9(9)V99.                      CL**1
00563          16 CA-OVR-COMM           PIC S9(9)V99.                      CL**1
00564          16 CA-AGT-COMM           PIC S9(9)V99.                      CL**1
00565          16 CA-B-IBNR             PIC S9(9)V99.                      CL**1
00566          16 CA-E-IBNR             PIC S9(9)V99.                      CL**1
00567          16 CA-B-LOSS             PIC S9(9)V99.                      CL**1
00568          16 CA-E-LOSS             PIC S9(9)V99.                      CL**1
00569          16 CA-RETRO-PMTS         PIC S9(09)V99.                     CL**1
00570          16 CA-COUNT              PIC S9(9).                         CL**1
00571          16 CA-CLM-CNT            PIC S9(9).                         CL**1
00572          16 CA-PD-CNT             PIC S9(9).                         CL**1
00573          16 CA-PRT-IND            PIC 9.                             CL**1
00574      12  CB-ADDS.                                                    CL**1
00575          16 CB-PREM               PIC S9(9)V99.                      CL**1
00576          16 CB-RST-PRM            PIC S9(9)V99.                      CL**1
00577          16 CB-REF                PIC S9(9)V99.                      CL**1
00578          16 CB-RES-A              PIC S9(9)V99.                      CL**1
00579          16 CB-RES-B              PIC S9(9)V99.                      CL**1
00580          16 CB-CLAIM              PIC S9(9)V99.                      CL**1
00581          16 CB-OVR-COMM           PIC S9(9)V99.                      CL**1
00582          16 CB-AGT-COMM           PIC S9(9)V99.                      CL**1
00583          16 CB-B-IBNR             PIC S9(9)V99.                      CL**1
00584          16 CB-E-IBNR             PIC S9(9)V99.                      CL**1
00585          16 CB-B-LOSS             PIC S9(9)V99.                      CL**1
00586          16 CB-E-LOSS             PIC S9(9)V99.                      CL**1
00587          16 CB-RETRO-PMTS         PIC S9(09)V99.                     CL**1
00588          16 CB-COUNT              PIC S9(9).                         CL**1
00589          16 CB-CLM-CNT            PIC S9(9).                         CL**1
00590          16 CB-PD-CNT             PIC S9(9).                         CL**1
00591          16 CB-PRT-IND            PIC 9.                             CL**1
00592                                                                      CL**1
00593  01  PRNT-SET-ACCUM  COMP-3.                                         CL**1
00594      12  X-TYPE      OCCURS 4.                                       CL**1
00595          16 X-PREM                PIC S9(9)V99.                      CL**1
00596          16 X-RST-PRM             PIC S9(9)V99.                      CL**1
00597          16 X-CREF                PIC S9(9)V99.                      CL**1
00598          16 X-PRESA               PIC S9(9)V99.                      CL**1
00599          16 X-PRESB               PIC S9(9)V99.                      CL**1
00600          16 X-CLAIM               PIC S9(9)V99.                      CL**1
00601          16 X-OVR-COMM            PIC S9(9)V99.                      CL**1
00602          16 X-AGT-COMM            PIC S9(9)V99.                      CL**1
00603          16 X-B-IBNR              PIC S9(9)V99.                      CL**1
00604          16 X-E-IBNR              PIC S9(9)V99.                      CL**1
00605          16 X-B-LOSS              PIC S9(9)V99.                      CL**1
00606          16 X-E-LOSS              PIC S9(9)V99.                      CL**1
00607          16 X-RETRO-PMTS          PIC S9(09)V99.                     CL**1
00608          16 X-CNT                 PIC S9(9).                         CL**1
00609          16 X-CLM-CNT             PIC S9(9).                         CL**1
00610          16 X-PD-CNT              PIC S9(9).                         CL**1
00611          16 X-PRT-IND             PIC 9.                             CL**1
00612                                                                      CL**1
00613  01  LIFE-AH-TABLE   COMP-3.                                         CL**1
00614      12  LIFE-OR-AH      OCCURS 2.                                   CL**1
00615          16  LA-X-TYPE      OCCURS 4.                                CL**1
00616              20 LA-X-PREM             PIC S9(9)V99.                  CL**1
00617              20 LA-X-RST-PRM          PIC S9(9)V99.                  CL**1
00618              20 LA-X-CREF             PIC S9(9)V99.                  CL**1
00619              20 LA-X-PRESA            PIC S9(9)V99.                  CL**1
00620              20 LA-X-PRESB            PIC S9(9)V99.                  CL**1
00621              20 LA-X-CLAIM            PIC S9(9)V99.                  CL**1
00622              20 LA-X-OVR-COMM         PIC S9(9)V99.                  CL**1
00623              20 LA-X-AGT-COMM         PIC S9(9)V99.                  CL**1
00624              20 LA-X-B-IBNR           PIC S9(9)V99.                  CL**1
00625              20 LA-X-E-IBNR           PIC S9(9)V99.                  CL**1
00626              20 LA-X-B-LOSS           PIC S9(9)V99.                  CL**1
00627              20 LA-X-E-LOSS           PIC S9(9)V99.                  CL**1
00628              20 LA-X-RETRO-PMTS       PIC S9(09)V99.                 CL**1
00629              20 LA-X-CNT              PIC S9(9).                     CL**1
00630              20 LA-X-CLM-CNT          PIC S9(9).                     CL**1
00631              20 LA-X-PD-CNT           PIC S9(9).                     CL**1
00632              20 LA-X-PRT-IND          PIC 9.                         CL**1
00633                                                                      CL**1
00634  01  SNGL-JNT-TABLE  COMP-3.                                         CL**1
00635      12  SNG-OR-JNT      OCCURS 2.                                   CL**1
00636          16  SP-OR-OB        OCCURS 3.                               CL**1
00637              20  SJ-X-YEARS     OCCURS 4.                            CL**1
00638                  24  SJ-X-PREM             PIC S9(9)V99.             CL**1
00639                  24  SJ-X-RST-PRM          PIC S9(9)V99.             CL**1
00640                  24  SJ-X-CREF             PIC S9(9)V99.             CL**1
00641                  24  SJ-X-PRESA            PIC S9(9)V99.             CL**1
00642                  24  SJ-X-PRESB            PIC S9(9)V99.             CL**1
00643                  24  SJ-X-CLAIM            PIC S9(9)V99.             CL**1
00644                  24  SJ-X-OVR-COMM         PIC S9(9)V99.             CL**1
00645                  24  SJ-X-AGT-COMM         PIC S9(9)V99.             CL**1
00646                  24  SJ-X-B-IBNR           PIC S9(9)V99.             CL**1
00647                  24  SJ-X-E-IBNR           PIC S9(9)V99.             CL**1
00648                  24  SJ-X-B-LOSS           PIC S9(9)V99.             CL**1
00649                  24  SJ-X-E-LOSS           PIC S9(9)V99.             CL**1
00650                  24  SJ-X-RETRO-PMTS       PIC S9(09)V99.            CL**1
00651                  24  SJ-X-CNT              PIC S9(9).                CL**1
00652                  24  SJ-X-CLM-CNT          PIC S9(9).                CL**1
00653                  24  SJ-X-PD-CNT           PIC S9(9).                CL**1
00654                  24  SJ-X-PRT-IND          PIC 9.                    CL**1
00655                                                                      CL**1
00656  01  AH-SGL-OB-TABLE   COMP-3.                                       CL**1
00657      12  AH-SP-OR-OB     OCCURS 2.                                   CL**1
00658          16  AH-YEARS       OCCURS 4.                                CL**1
00659              20 SOA-X-PREM            PIC S9(9)V99.                  CL**1
00660              20 SOA-X-RST-PRM         PIC S9(9)V99.                  CL**1
00661              20 SOA-X-CREF            PIC S9(9)V99.                  CL**1
00662              20 SOA-X-PRESA           PIC S9(9)V99.                  CL**1
00663              20 SOA-X-PRESB           PIC S9(9)V99.                  CL**1
00664              20 SOA-X-CLAIM           PIC S9(9)V99.                  CL**1
00665              20 SOA-X-OVR-COMM        PIC S9(9)V99.                  CL**1
00666              20 SOA-X-AGT-COMM        PIC S9(9)V99.                  CL**1
00667              20 SOA-X-B-IBNR          PIC S9(9)V99.                  CL**1
00668              20 SOA-X-E-IBNR          PIC S9(9)V99.                  CL**1
00669              20 SOA-X-B-LOSS          PIC S9(9)V99.                  CL**1
00670              20 SOA-X-E-LOSS          PIC S9(9)V99.                  CL**1
00671              20 SOA-X-RETRO-PMTS      PIC S9(09)V99.                 CL**1
00672              20 SOA-X-CNT             PIC S9(9).                     CL**1
00673              20 SOA-X-CLM-CNT         PIC S9(9).                     CL**1
00674              20 SOA-X-PD-CNT          PIC S9(9).                     CL**1
00675              20 SOA-X-PRT-IND         PIC 9.                         CL**1
00676                                                                      CL**1
00677  01  PRNT-SET-MEANS.                                                 CL**1
00678      12  X-MEANS     OCCURS 4    PIC S9(11)V99    COMP-3.            CL**1
00679                                                                      CL**1
00680  01  LIFE-MEANS.                                                     CL**1
00681      12  L-X-MEANS     OCCURS 4    PIC S9(11)V99    COMP-3.          CL**1
00682                                                                      CL**1
00683  01  SINGLE-MEANS.                                                   CL**1
00684      12  S-X-SP-OB     OCCURS 3.                                     CL**1
00685          16  S-X-MEANS     OCCURS 4    PIC S9(11)V99    COMP-3.      CL**1
00686                                                                      CL**1
00687  01  JOINT-MEANS.                                                    CL**1
00688      12  J-X-SP-OB     OCCURS 3.                                     CL**1
00689          16  J-X-MEANS     OCCURS 4    PIC S9(11)V99    COMP-3.      CL**1
00690                                                                      CL**1
00691      EJECT                                                           CL**1
00692  01  HEAD-A.                                                         CL**1
00693      12  FILLER      PIC X(31)   VALUE ' INSURANCE CALL'.            CL**1
CIDMOD     12  HEAD-A-OV   PIC X(5)    VALUE SPACES.
00694      12  FILLER      PIC X(24)   VALUE 'CREDIT LIFE AND CREDIT H'.   CL**1
00695      12  FILLER      PIC X(24)   VALUE 'EALTH AND ACCIDENT EXPER'.   CL**1
00696      12  FILLER      PIC X(05)   VALUE 'IENCE'.                      CL**1
00697      12  FILLER      PIC X(31)   VALUE SPACES.                       CL**1
00698      12  FILLER      PIC X(06)   VALUE 'ECS159'.                     CL**1
00699      12  HEAD-A-TYPE PIC X(01)   VALUE 'A'.                          CL**1
00700      12  FILLER      PIC X(05)   VALUE SPACES.                       CL**1
00701                                                                      CL**1
00702  01  HEAD-AA.                                                        CL**1
00703      12  FILLER      PIC X(47)   VALUE SPACES.                       CL**1
00704      12  HD-RPT-TYP  PIC X(30)   VALUE SPACES.                       CL**1
00705      12  FILLER      PIC X(55)   VALUE SPACES.                       CL**1
00706                                                                      CL**1
00707  01  HEAD-B.                                                         CL**1
00708      12  FILLER      PIC X(18)   VALUE SPACES.                       CL**1
00709      12  HDB-REIN    PIC X(13)   VALUE SPACES.                       CL**1
00710      12  FILLER      PIC X(16)   VALUE SPACES.                       CL**1
00711      12  COM-NAM     PIC X(30)   VALUE SPACES.                       CL**1
00712      12  FILLER      PIC X(43)   VALUE SPACES.                       CL**1
00713      12  HD-RUN-DT   PIC X(8)    VALUE SPACES.                       CL**1
00714      12  FILLER      PIC X(04)   VALUE SPACES.                       CL**1
00715                                                                      CL**1
00716  01  HEAD-C.                                                         CL**1
00717      12  FILLER      PIC X(53)   VALUE SPACES.                       CL**1
00718      12  HC-DATE     PIC X(18).                                      CL**1
00719      12  FILLER      PIC X(41)   VALUE SPACES.                       CL**1
00720      12  FILLER      PIC X(5)    VALUE 'PAGE '.                      CL**1
00721      12  HD-PAGE     PIC ZZ,ZZ9.                                     CL**1
00722                                                                      CL**1
00723  01  HEAD-CC.                                                        CL**1
00724      12  FILLER      PIC X(18)   VALUE ' ***** TOTALS FOR'.          CL**1
00725      12  HD-TYPE-TOT PIC X(33)   VALUE SPACES.                       CL**1
00726                                                                      CL**1
00727  01  HEAD-HH.                                                        CL**1
00728      12  FILLER                  PIC X(38) VALUE SPACES.             CL**1
00729      12  HH-YEARS  OCCURS 3.                                         CL**1
00730          16  FILLER              PIC X(03).                          CL**1
00731          16  HEAD-HH-MO          PIC 9(02).                          CL**1
00732          16  HEAD-HH-SLASH-1     PIC X(01).                          CL**1
00733          16  HEAD-HH-DA          PIC 9(02).                          CL**1
00734          16  HEAD-HH-SLASH-2     PIC X(01).                          CL**1
00735          16  HEAD-HH-YR          PIC 9(02).                          CL**1
00736          16  HEAD-HH-THRU        PIC X(05).                          CL**1
00737                                                                      CL**1
00738  01  HEAD-H.                                                         CL**1
00739      12  FILLER                  PIC X(08) VALUE SPACES.             CL**1
00740      12  HH-BENEFIT              PIC X(19).                          CL**1
00741      12  FILLER                  PIC X(11) VALUE SPACES.             CL**1
00742      12  HH-NAMES OCCURS 4.                                          CL**1
00743          16  FILLER              PIC X(03).                          CL**1
00744          16  HEAD-H-MO           PIC 9(02).                          CL**1
00745          16  HEAD-H-SLASH-1      PIC X(01).                          CL**1
00746          16  HEAD-H-DA           PIC 9(02).                          CL**1
00747          16  HEAD-H-SLASH-2      PIC X(01).                          CL**1
00748          16  HEAD-H-YR           PIC 9(02).                          CL**1
00749          16  FILLER              PIC X(05).                          CL**1
00750                                                                      CL**1
00751                                                                      CL**1
00752  01  HEAD-D-REIN.                                                    CL**1
00753      12  FILLER                  PIC X(24)   VALUE                   CL**1
00754          ' REINSURANCE COMPANY -  '.                                 CL**1
00755      12  HEAD-D-REIN-CO          PIC X(6)    VALUE SPACES.           CL**1
00756                                                                      CL**1
00757  01  HEAD-E-CARR.                                                    CL**1
00758      12  FILLER                  PIC X(11)   VALUE                   CL**1
00759          ' CARRIER - '.                                              CL**1
00760      12  HEADE-CARR              PIC X       VALUE SPACES.           CL**1
00761                                                                      CL**1
00762  01  HEAD-F-COMP.                                                    CL**1
00763      12  FILLER                  PIC X(11)   VALUE                   CL**1
00764          ' GROUPING- '.                                              CL**1
00765      12  HEADF-COMP              PIC X(6)    VALUE SPACES.           CL**1
00766                                                                      CL**1
00767  01  HEAD-FF-VAR.                                                    CL**1
00768      12  HEADFF-VAR-DESC         PIC X(25)   VALUE SPACES.           CL**1
00769      12  HEADFF-VAR              PIC X(25)   VALUE SPACES.           CL**1
00770      12  FILLER                  PIC X(5)    VALUE SPACES.           CL**1
00771      12  HEADFF-VARA             PIC X(30)   VALUE SPACES.           CL**1
00772                                                                      CL**1
00773  01  HEAD-G-STATE.                                                   CL**1
00774      12  FILLER                  PIC X(09)   VALUE                   CL**1
00775          ' STATE - '.                                                CL**1
00776      12  HEADG-STATE             PIC X(20)   VALUE SPACES.           CL**1
00777                                                                      CL**1
00778  01  HEAD-GG-VAR.                                                    CL**1
00779      12  HEADGG-VAR-DESC         PIC X(25)   VALUE SPACES.           CL**1
00780      12  HEADGG-VAR.                                                 CL**1
00781          16  HEADGG-VAR-ACCT     PIC X(10)   VALUE SPACES.           CL**1
00782          16  FILLER              PIC X(02)   VALUE SPACES.           CL**1
00783          16  HEADGG-VAR-SLASH    PIC X(01)   VALUE SPACES.           CL**1
00784          16  FILLER              PIC X(02)   VALUE SPACES.           CL**1
00785          16  HEADGG-VAR-DEV      PIC X(03)   VALUE SPACES.           CL**1
00786          16  FILLER              PIC X(07)   VALUE SPACES.           CL**1
00787      12  FILLER                  PIC X(5)    VALUE SPACES.           CL**1
00788      12  HEADGG-VARA             PIC X(50)   VALUE SPACES.           CL**1
00789                                                                      CL**1
00790  01  HEAD-HH-VAR.                                                    CL**1
00791      12  HEADHH-VAR-DESC.                                            CL**1
00792          16  FILLER              PIC X(01)   VALUE SPACES.           CL**1
00793          16  HEADHH-VAR-TERM     PIC X(07)   VALUE SPACES.           CL**1
00794          16  HEADHH-TERM-DESC    PIC X(15)   VALUE SPACES.           CL**1
00795          16  FILLER              PIC X(02)   VALUE SPACES.           CL**1
00796      12  HEADHH-VAR              PIC X(50)   VALUE SPACES.           CL**1
00797      12  FILLER                  PIC X(5)    VALUE SPACES.           CL**1
00798      12  HEADHH-VARA             PIC X(30)   VALUE SPACES.           CL**1
00799                                                                      CL**1
00800  01  HEAD-I.                                                         CL**1
00801      12  FILLER          PIC X             VALUE SPACES.             CL**1
00802      12  HI-A            PIC X(30).                                  CL**1
00803      12  HI-AA           PIC X(05)         VALUE SPACES.             CL**1
00804      12  AST1            PIC X             VALUE ' '.                CL**1
00805      12  HI-B            PIC ZZZ,ZZZ,ZZZ.ZZ-.                        CL**1
00806      12  HI-BB REDEFINES HI-B    PIC ZZ,ZZZ,ZZZ,ZZZ-.                CL**1
00807      12  AST2            PIC X             VALUE ' '.                CL**1
00808      12  HI-C            PIC ZZZ,ZZZ,ZZZ.ZZ-.                        CL**1
00809      12  HI-CC REDEFINES HI-C    PIC ZZ,ZZZ,ZZZ,ZZZ-.                CL**1
00810      12  AST3            PIC X             VALUE ' '.                CL**1
00811      12  HI-D            PIC ZZZ,ZZZ,ZZZ.ZZ-.                        CL**1
00812      12  HI-DD REDEFINES HI-D    PIC ZZ,ZZZ,ZZZ,ZZZ-.                CL**1
00813      12  AST4            PIC X             VALUE ' '.                CL**1
00814      12  HI-E            PIC ZZZ,ZZZ,ZZZ.ZZ-.                        CL**1
00815      12  HI-EE REDEFINES HI-E    PIC ZZ,ZZZ,ZZZ,ZZZ-.                CL**1
00816                                                                      CL**1
00817  01  HEAD-IA.                                                        CL**1
00818      12  HI-1.                                                       CL**1
00819          16  FILLER PIC X(21)  VALUE ' 1. PREMIUM WRITTEN  '.        CL**1
00820      12  HI-2       PIC X(27)  VALUE                                 CL**1
00821                                 ' 3. REFUNDS ON TERMINATIONS'.       CL**1
00822      12  HI-4.                                                       CL**1
00823          16  FILLER PIC X(20)  VALUE ' 2. PREMIUM RESERVE '.         CL**1
00824          16  FILLER PIC X(09)  VALUE 'BEGINNING'.                    CL**1
00825      12  HI-5.                                                       CL**1
00826          16 FILLER  PIC X(20)  VALUE ' 4. PREMIUM RESERVE '.         CL**1
00827          16 FILLER  PIC X(09)  VALUE 'ENDING   '.                    CL**1
00828      12  HI-6       PIC X(24)  VALUE ' 5. EARNED PREMIUM'.           CL**1
00829      12  HI-7       PIC X(24)  VALUE ' 6. CLAIMS PAID'.              CL**1
00830      12  HI-8       PIC X(24)  VALUE 'OVR COMM PAID  '.              CL**1
00831      12  HI-8A      PIC X(24)  VALUE 'AGENT COMM PAID'.              CL**1
00832      12  HI-9.                                                       CL**1
00833          16  FILLER PIC X(23)  VALUE 'MEAN INS. IN FORCE  '.         CL**1
00834      12  HI-10.                                                      CL**1
00835          16  FILLER PIC X(14)  VALUE 'CERTS INFORCE '.               CL**1
00836          16  FILLER PIC X(09)  VALUE 'ENDING   '.                    CL**1
00837      12  HI-11.                                                      CL**1
00838          16  FILLER PIC X(18)  VALUE ' 7. LOSS RESERVES '.           CL**1
00839          16  FILLER PIC X(09)  VALUE 'BEGINNING'.                    CL**1
00840      12  HI-13.                                                      CL**1
00841          16 FILLER  PIC X(18)  VALUE ' 8. LOSS RESERVES '.           CL**1
00842          16 FILLER  PIC X(09)  VALUE 'ENDING   '.                    CL**1
00843      12  HI-14      PIC X(26)  VALUE '13. LOSS RATIO PRIMA FACIE'.   CL**1
00844      12  HI-15      PIC X(24)  VALUE ' 9. INCURRED CLAIMS'.          CL**1
00845      12  HI-16      PIC X(24)  VALUE '12. LOSS RATIO     '.          CL**1
00846      12  HI-17   PIC X(29)  VALUE '10. DIVIDENDS AND EXPERIENCE '.   CL**1
00847      12  HI-18   PIC X(28)  VALUE '11. PRIMA FACIE EARN PREMIUM'.    CL**1
00848      12  HI-19   PIC X(29)  VALUE '14. NUMBER OF INCURRED CLAIMS'.   CL**1
00849      12  HI-20   PIC X(25)  VALUE '15. NUMBER OF PAID CLAIMS'.       CL**1
00850                                                                      CL**1
00851      EJECT                                                           CL**1
00852  01  LAST-CONTROL.                                                   CL**1
00853      12  LAS-RECORD-TYPE         PIC X(01)   VALUE SPACES.           CL**1
00854      12  LAS-REIN-CO             PIC X(06)   VALUE SPACES.           CL**1
00855      12  LAS-CARRIER             PIC X(01)   VALUE SPACES.           CL**1
00856      12  LAS-GROUPING            PIC X(06)   VALUE SPACES.           CL**1
00857      12  LAS-STATE               PIC X(02)   VALUE SPACES.           CL**1
00858      12  LAS-ACCOUNT             PIC X(10)   VALUE SPACES.           CL**1
00859      12  LAS-TERM-CD             PIC X(01)   VALUE SPACES.           CL**1
00860      12  FILLER                  PIC X(01)   VALUE SPACES.           CL**1
00861      12  LAS-CAL-TABLE           PIC X(02)   VALUE SPACES.           CL**1
00862      12  LAS-ACCT-TYPE           PIC X(02)   VALUE SPACES.           CL**1
00863      12  LAS-STATE-DEV           PIC X(03)   VALUE SPACES.           CL**1
00864                                                                      CL**1
00865  01  FILLER.                                                         CL**1
00866      12  LAS-ACT-NAME            PIC X(30)   VALUE SPACES.           CL**1
00867      12  LAS-STATE-NAME          PIC X(20)   VALUE SPACES.           CL**1
00868      EJECT                                                           CL**1
00869                                                                      CL**1
00870                                  COPY ELCDATE.                       CL**1
00871      EJECT                                                           CL**1
00872                                  COPY ELCDTECX.                      CL**1
00873                                                                      CL**1
00874      EJECT                                                           CL**1
00875                                  COPY ELCDTEVR.                      CL**1
00876                                                                      CL**1
00877      EJECT                                                           CL**1
00878  PROCEDURE DIVISION.                                                 CL**1
00879  0100-READ-DATE-CARD.                                                CL**1
CIDMOD
CIDMOD     ACCEPT      WS-REPORT-TITLE.
CIDMOD
CIDMOD     IF WS-REPORT-TITLE = 'NON CREDIT'
CIDMOD        MOVE ' NON '         TO HEAD-A-OV
CIDMOD     ELSE
CIDMOD        IF WS-REPORT-TITLE = 'CREDIT'
CIDMOD           MOVE 'ONLY '      TO HEAD-A-OV
CIDMOD        ELSE
CIDMOD           IF WS-REPORT-TITLE = 'CREDIT 120'
CIDMOD              MOVE ' 120 '   TO HEAD-A-OV
CIDMOD           ELSE
CIDMOD              MOVE SPACES    TO HEAD-A-OV
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
00880                                  COPY ELCDTERX.                      CL**1
00881                                                                      CL**1
00882      IF EP-SW EQUAL '1'                                              CL**1
00883          MOVE 'S'                TO WS-PROCESS-SW.                   CL**1
00884                                                                      CL**1
00885      MOVE LOW-VALUES             TO LAST-CONTROL                     CL**1
00886      MOVE WS-CURRENT-DATE        TO HD-RUN-DT.                       CL**1
00887      MOVE COMPANY-NAME           TO COM-NAM.                         CL**1
00888                                                                      CL**1
00889      IF EP-DT EQUAL RUN-DATE                                         CL**1
00890         MOVE ALPH-DATE           TO HC-DATE                          CL**1
00891         SUBTRACT 1 FROM RUN-CCYY                                     CL**1
00892      ELSE                                                            CL**1
00893         MOVE EP-DT               TO DC-GREG-DATE-CYMD                CL**5
00894         MOVE 'L'                 TO DC-OPTION-CODE                   CL**1
00895         CALL 'ELDATCX' USING DATE-CONVERSION-DATA                    CL**1
00896         MOVE DC-GREG-DATE-1-ALPHA                                    CL**1
00897                                  TO HC-DATE.                         CL**1
00898                                                                      CL**1
00899      MOVE SPACES TO HEAD-HH.                                         CL**1
00900                                                                      CL**1
00901      IF  SINGLE-PERIOD-PROCESS                                       CL**1
00902           MOVE RUN-YR       TO   HEAD-HH-YR (3)                      CL**1
00903           MOVE RUN-MO       TO   HEAD-HH-MO (3)                      CL**1
00904           MOVE RUN-DA       TO   HEAD-HH-DA (3)                      CL**1
00905           MOVE '/'          TO   HEAD-HH-SLASH-1 (3)                 CL**1
00906                                  HEAD-HH-SLASH-2 (3)                 CL**1
00907           MOVE ' THRU'      TO   HEAD-HH-THRU (3)                    CL**1
00908           MOVE SPACES       TO   HEAD-H                              CL**1
00909           MOVE EP-MO        TO   HEAD-H-MO (3)                       CL**1
00910           MOVE EP-DA        TO   HEAD-H-DA (3)                       CL**1
00911           MOVE EP-YR        TO   HEAD-H-YR (3)                       CL**1
00912           MOVE '/'          TO   HEAD-H-SLASH-1 (3)                  CL**1
00913                                  HEAD-H-SLASH-2 (3)                  CL**1
00914           GO TO 0080-OPEN-FILES.                                     CL**1
00915                                                                      CL**1
00916      MOVE RUN-MO       TO   HEAD-HH-MO (1)                           CL**1
00917                             HEAD-HH-MO (2)                           CL**1
00918                             HEAD-HH-MO (3).                          CL**1
00919                                                                      CL**1
00920      MOVE RUN-DA       TO   HEAD-HH-DA (1)                           CL**1
00921                             HEAD-HH-DA (2)                           CL**1
00922                             HEAD-HH-DA (3).                          CL**1
00923                                                                      CL**1
00924      MOVE RUN-YR       TO   HEAD-HH-YR (3).                          CL**1
00925                                                                      CL**1
PEMUNI*    COMPUTE HEAD-HH-YR (2) = (RUN-YR - 1).                          CL**2
PEMUNI     COMPUTE HEAD-HH-YR (2) = (RUN-CCYY - 1).                        CL**2
00927                                                                      CL**2
00928      IF HEAD-HH-YR (2) < 0                                           CL**2
00929          ADD 100 TO HEAD-HH-YR (2).                                  CL**2
00930                                                                      CL**2
PEMUNI*    COMPUTE HEAD-HH-YR (1) = (RUN-YR - 2).                          CL**2
PEMUNI     COMPUTE HEAD-HH-YR (1) = (RUN-CCYY - 2).                        CL**2
00932                                                                      CL**2
00933      IF HEAD-HH-YR (1) < 0                                           CL**2
00934          ADD 100 TO HEAD-HH-YR (1).                                  CL**2
00935                                                                      CL**2
00936                                                                      CL**1
00937      MOVE '/'          TO   HEAD-HH-SLASH-1 (1)                      CL**1
00938                             HEAD-HH-SLASH-1 (2)                      CL**1
00939                             HEAD-HH-SLASH-1 (3)                      CL**1
00940                             HEAD-HH-SLASH-2 (1)                      CL**1
00941                             HEAD-HH-SLASH-2 (2)                      CL**1
00942                             HEAD-HH-SLASH-2 (3).                     CL**1
00943                                                                      CL**1
00944      MOVE ' THRU'      TO   HEAD-HH-THRU (1)                         CL**1
00945                             HEAD-HH-THRU (2)                         CL**1
00946                             HEAD-HH-THRU (3).                        CL**1
00947                                                                      CL**1
00948      MOVE SPACES       TO   HEAD-H.                                  CL**1
00949                                                                      CL**1
00950      MOVE ' T O T A L ' TO  HH-NAMES (4).                            CL**1
00951                                                                      CL**1
00952      MOVE EP-MO        TO   HEAD-H-MO (1)                            CL**1
00953                             HEAD-H-MO (2)                            CL**1
00954                             HEAD-H-MO (3).                           CL**1
00955                                                                      CL**1
00956      MOVE EP-DA        TO   HEAD-H-DA (1)                            CL**1
00957                             HEAD-H-DA (2)                            CL**1
00958                             HEAD-H-DA (3).                           CL**1
00959                                                                      CL**1
00960      MOVE EP-YR        TO   HEAD-H-YR (3).                           CL**1
00961                                                                      CL**1
PEMUNI*    COMPUTE HEAD-H-YR (2) = (EP-YR - 1).                            CL**4
PEMUNI     COMPUTE HEAD-H-YR (2) = (EP-CCYY - 1).                          CL**4
00963                                                                      CL**4
00964      IF HEAD-HH-YR (2) < 0                                           CL**4
00965          ADD 100 TO HEAD-HH-YR (2).                                  CL**4
00966                                                                      CL**4
PEMUNI*    COMPUTE HEAD-H-YR (1) = (EP-YR - 2).                            CL**4
PEMUNI     COMPUTE HEAD-H-YR (1) = (EP-CCYY - 2).                          CL**4
00968                                                                      CL**4
00969      IF HEAD-HH-YR (1) < 0                                           CL**4
00970          ADD 100 TO HEAD-HH-YR (1).                                  CL**4
00971                                                                      CL**4
00972                                                                      CL**1
00973      MOVE '/'          TO   HEAD-H-SLASH-1 (1)                       CL**1
00974                             HEAD-H-SLASH-1 (2)                       CL**1
00975                             HEAD-H-SLASH-1 (3)                       CL**1
00976                             HEAD-H-SLASH-2 (1)                       CL**1
00977                             HEAD-H-SLASH-2 (2)                       CL**1
00978                             HEAD-H-SLASH-2 (3).                      CL**1
00979                                                                      CL**1
00980  0080-OPEN-FILES.                                                    CL**1
00981                                                                      CL**1
00982      OPEN OUTPUT CALL-PRT.                                           CL**1
00983                                                                      CL**1
00984  0100-BEGIN-PROGRAM.                                                 CL**1
00985                                                                      CL**1
00986      MOVE ZEROS TO PAGE-CT.                                          CL**1
00987                                                                      CL**1
00988      COMPUTE  DAT-LIM (1) = (RUN-CCYY * +12) + RUN-MO.               CL**4
00989      COMPUTE  DAT-LIM (2) = ((RUN-CCYY - +1) * +12) + RUN-MO.        CL**4
00990      COMPUTE  DAT-LIM (3) = ((RUN-CCYY - +2) * +12) + RUN-MO.        CL**4
00991                                                                      CL**1
00992      COMPUTE  DAT-MAX (1) = (EP-CCYY * +12) + EP-MO.                 CL**4
00993      COMPUTE  DAT-MAX (2) = ((EP-CCYY - +1) * +12) + EP-MO.          CL**4
00994      COMPUTE  DAT-MAX (3) = ((EP-CCYY - +2) * +12) + EP-MO.          CL**4
00995                                                                      CL**1
00996      PERFORM 0135-ZERO-CLM-ADJUSTMENTS VARYING                       CL**1
00997         YR FROM +1 BY +1 UNTIL YR GREATER THAN +4 AFTER              CL**1
092602        CB FROM +1 BY +1 UNTIL CB GREATER THAN +450.                 CL**1
00999                                                                      CL**1
01000      MOVE TX-LF-CLM-ADJUSTMENTS (1)                                  CL**4
01001                                  TO Z-C.                             CL**4
01002      MOVE TX-AH-CLM-ADJUSTMENTS (1)                                  CL**4
01003                                  TO Z-C.                             CL**4
01004      MOVE Z-C                    TO TX-LF-CLM-ADJUSTMENTS (2).       CL**4
01005      MOVE Z-C                    TO TX-AH-CLM-ADJUSTMENTS (2).       CL**4
01006                                                                      CL**1
01007      PERFORM 0130-ZERO-ACCUMS-NOW VARYING                            CL**1
092602        CA FROM +1 BY +1 UNTIL CA GREATER THAN +7 AFTER              CL**1
01009         YR FROM +1 BY +1 UNTIL YR GREATER THAN +3 AFTER              CL**1
092602        CB FROM +1 BY +1 UNTIL CB GREATER THAN +450.                 CL**1
01011                                                                      CL**1
01012      MOVE AH-ACCUMS              TO LF-ACCUMS                        CL**1
092602*                                   AH-TABLE-1                       CL**1
092602*                                   AH-TABLE-2                       CL**1
092602*                                   AH-TABLE-3                       CL**1
092602*                                   AH-TABLE-4                       CL**1
092602*                                   AH-TABLE-5                       CL**1
01018 *                                   AH-TABLE-6                       CL**1
01019 *                                   LF-TABLE-1                       CL**1
01020 *                                   LF-TABLE-2                       CL**1
01021 *                                   LF-TABLE-3                       CL**1
01022 *                                   LF-TABLE-4                       CL**1
01023 *                                   LF-TABLE-5                       CL**1
092602*                                   LF-TABLE-6.                      CL**1
01025                                                                      CL**1
01026      MOVE AH-A-TYPE (1 1 1)      TO A-AREA CA-ADDS CB-ADDS.          CL**1
01027      MOVE AH-NO-TYPE (1)         TO Z-L.                             CL**1
01028                                                                      CL**1
01029      PERFORM 0110-ZERO-MEANS VARYING                                 CL**1
01030         CA FROM +1 BY +1 UNTIL CA GREATER THAN +07 AFTER             CL**1
01031         YR FROM +1 BY +1 UNTIL YR GREATER THAN +3 AFTER              CL**1
092602        CB FROM +1 BY +1 UNTIL CB GREATER THAN +450.                 CL**1
01033                                                                      CL**1
01034      MOVE TYPE-MEAN (1)          TO Z-M.                             CL**1
01035                                                                      CL**1
01036      MOVE +0         TO LF-CLM-FACTOR                                CL**1
01037                         AH-CLM-FACTOR.                               CL**1
01038                                                                      CL**1
01039  0105-END-ZERO-OUT.                                                  CL**1
01040                                                                      CL**1
01041      GO TO 0140-FIND-REPORT-BREAKS.                                  CL**1
01042                                                                      CL**1
01043  0110-ZERO-MEANS.                                                    CL**1
01044                                                                      CL**1
01045      MOVE ZEROS TO MEANS (CA YR CB).                                 CL**1
01046                                                                      CL**1
01047  0130-ZERO-ACCUMS-NOW.                                               CL**1
01048                                                                      CL**1
01049      MOVE Z                      TO AH-A-PREM (CA YR CB).            CL**1
01050      MOVE Z                      TO AH-A-CREF (CA YR CB).            CL**1
01051      MOVE Z                      TO AH-A-PRESA (CA YR CB).           CL**1
01052      MOVE Z                      TO AH-A-PRESB (CA YR CB).           CL**1
01053      MOVE Z                      TO AH-A-CLAIM (CA YR CB).           CL**1
01054      MOVE Z                      TO AH-A-B-IBNR (CA YR CB).          CL**1
01055      MOVE Z                      TO AH-A-E-IBNR (CA YR CB)           CL**1
01056      MOVE Z                      TO AH-A-B-LOSS (CA YR CB).          CL**1
01057      MOVE Z                      TO AH-A-E-LOSS (CA YR CB)           CL**1
01058      MOVE Z                      TO AH-A-CNT (CA YR CB).             CL**1
01059      MOVE Z                      TO AH-A-CLM-CNT (CA YR CB).         CL**1
01060      MOVE Z                      TO AH-A-PD-CNT  (CA YR CB).         CL**1
01061      MOVE Z                      TO AH-A-RST-PRM (CA YR CB).         CL**1
01062      MOVE ZEROS                  TO AH-A-PRT-IND (CA YR CB).         CL**1
01063      MOVE Z                      TO AH-A-OVR-COMM (CA YR CB)         CL**1
01064      MOVE Z                      TO AH-A-AGT-COMM (CA YR CB)         CL**1
01065      MOVE Z                      TO AH-A-RETRO-PMTS (CA YR CB).      CL**1
01066                                                                      CL**1
01067  0135-ZERO-CLM-ADJUSTMENTS.                                          CL**1
01068                                                                      CL**1
01069      MOVE +0 TO WS-TOTAL-LF-PMTS (1 YR CB)                           CL**1
01070                 WS-TOTAL-AH-PMTS (1 YR CB)                           CL**1
01071                 WS-TOTAL-E-LF-RESV (1 YR CB)                         CL**1
01072                 WS-TOTAL-B-LF-RESV (1 YR CB)                         CL**1
01073                 WS-TOTAL-E-AH-RESV (1 YR CB)                         CL**1
01074                 WS-TOTAL-B-AH-RESV (1 YR CB).                        CL**1
01075                                                                      CL**1
01076      EJECT                                                           CL**1
01077  0140-FIND-REPORT-BREAKS.                                            CL**1
01078                                                                      CL**1
01079      MOVE SPACES TO WS-CALL-REPORTS.                                 CL**1
01080      MOVE 'X'    TO WS-A-REPORT.                                     CL**1
01081                                                                      CL**1
01082  0150-BUILD-STATE.                                                   CL**1
01083                                                                      CL**1
01084      MOVE CLAS-STARTS TO CLAS-INDEXS.                                CL**1
01085                                                                      CL**1
01086  0160-BUILD-STATE-LOOP.                                              CL**1
01087                                                                      CL**1
01088      IF CLAS-INDEXS GREATER THAN CLAS-MAXS                           CL**1
01089         GO TO 0170-SORT-PROCEDURE.                                   CL**1
01090                                                                      CL**1
           MOVE '4'           TO STATE-CALL-BREAK (CLAS-INDEXS)
01091      IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '1'                     CL**1
01092         MOVE 'X'        TO WS-A-REPORT                               CL**1
01093      ELSE                                                            CL**1
01094      IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '2'                     CL**1
01095         MOVE 'X'        TO WS-B-REPORT                               CL**1
01096      ELSE                                                            CL**1
01097      IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '3'                     CL**1
01098         MOVE 'X'        TO WS-C-REPORT                               CL**1
01099      ELSE                                                            CL**1
01100      IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '4'                     CL**1
01101         MOVE 'X'        TO WS-D-REPORT                               CL**1
01102      ELSE                                                            CL**1
01103      IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '5'                     CL**1
01104         MOVE 'X'        TO WS-E-REPORT                               CL**1
01105      ELSE                                                            CL**1
01106      IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '6'                     CL**1
01107         MOVE 'X'        TO WS-F-REPORT                               CL**1
01108      ELSE                                                            CL**1
01109      IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '7'                     CL**1
01110         MOVE 'X'        TO WS-G-REPORT                               CL**1
01111      ELSE                                                            CL**1
01112      IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '8'                     CL**1
01113         MOVE 'X'        TO WS-H-REPORT                               CL**1
01114      ELSE                                                            CL**1
01115      IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '9'                     CL**1
01116         MOVE 'X'        TO WS-I-REPORT                               CL**1
01117      ELSE                                                            CL**1
01118      IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL 'A'                     CL**1
01119         MOVE 'X'        TO WS-J-REPORT                               CL**1
01120      ELSE                                                            CL**1
01121      IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL 'B'                     CL**1
01122         MOVE 'X'        TO WS-K-REPORT.                              CL**1
01123                                                                      CL**1
01124      ADD +1 TO CLAS-INDEXS                                           CL**1
01125      GO TO 0160-BUILD-STATE-LOOP.                                    CL**1
01126                                                                      CL**1
01127  0170-SORT-PROCEDURE.                                                CL**1
01128                                                                      CL**1
01129      IF WS-CALL-REPORTS EQUAL SPACES                                 CL**1
01130         GO TO 0410-END-OF-JOB.                                       CL**1
01131                                                                      CL**1
01132      SORT SORT-FILE ON ASCENDING KEY SORT-CLAIM-KEY                  CL**1
01133           INPUT PROCEDURE 0180-GET-EXTRACT-RECORD THRU               CL**1
01134                           0230-END-INPUT-PROC                        CL**1
01135           OUTPUT PROCEDURE 0300-PRINT-REPORT THRU                    CL**1
01136                            0400-END-OUTPUT-PROC.                     CL**1
01137                                                                      CL**1
01138      IF SORT-RETURN NOT EQUAL ZEROS                                  CL**1
01139         MOVE +0101 TO WS-RETURN-CODE                                 CL**1
01140         MOVE ' ERROR ON SORT ' TO WS-ABEND-MESSAGE                   CL**1
01141         MOVE SPACES TO WS-ABEND-FILE-STATUS                          CL**1
01142         GO TO ABEND-PGM.                                             CL**1
01143                                                                      CL**1
01144      IF WS-A-REPORT EQUAL 'X'                                        CL**1
01145         MOVE SPACES TO WS-A-REPORT                                   CL**1
01146      ELSE                                                            CL**1
01147      IF WS-B-REPORT EQUAL 'X'                                        CL**1
01148         MOVE SPACES TO WS-B-REPORT                                   CL**1
01149      ELSE                                                            CL**1
01150      IF WS-C-REPORT EQUAL 'X'                                        CL**1
01151         MOVE SPACES TO WS-C-REPORT                                   CL**1
01152      ELSE                                                            CL**1
01153      IF WS-D-REPORT EQUAL 'X'                                        CL**1
01154         MOVE SPACES TO WS-D-REPORT                                   CL**1
01155      ELSE                                                            CL**1
01156      IF WS-E-REPORT EQUAL 'X'                                        CL**1
01157         MOVE SPACES TO WS-E-REPORT                                   CL**1
01158      ELSE                                                            CL**1
01159      IF WS-F-REPORT EQUAL 'X'                                        CL**1
01160         MOVE SPACES TO WS-F-REPORT                                   CL**1
01161      ELSE                                                            CL**1
01162      IF WS-G-REPORT EQUAL 'X'                                        CL**1
01163         MOVE SPACES TO WS-G-REPORT                                   CL**1
01164      ELSE                                                            CL**1
01165      IF WS-H-REPORT EQUAL 'X'                                        CL**1
01166         MOVE SPACES TO WS-H-REPORT                                   CL**1
01167      ELSE                                                            CL**1
01168      IF WS-I-REPORT EQUAL 'X'                                        CL**1
01169         MOVE SPACES TO WS-I-REPORT                                   CL**1
01170      ELSE                                                            CL**1
01171      IF WS-J-REPORT EQUAL 'X'                                        CL**1
01172         MOVE SPACES TO WS-J-REPORT                                   CL**1
01173      ELSE                                                            CL**1
01174      IF WS-K-REPORT EQUAL 'X'                                        CL**1
01175         MOVE SPACES TO WS-K-REPORT.                                  CL**1
01176                                                                      CL**1
01177      GO TO 0170-SORT-PROCEDURE.                                      CL**1
01178                                                                      CL**1
01179  0180-GET-EXTRACT-RECORD SECTION.                                    CL**1
01180                                                                      CL**1
01181      OPEN INPUT EXTRACT.                                             CL**1
01182      MOVE CLAS-STARTS TO CLAS-INDEXS.                                CL**1
01183                                                                      CL**1
01184  0190-READ-EXTRACT.                                                  CL**1
01185                                                                      CL**1
01186      READ EXTRACT INTO CALL-EXTRACT AT END                           CL**1
01187           GO TO 0230-END-INPUT-PROC.                                 CL**1
01188                                                                      CL**1
01189  0200-FIND-STATE.                                                    CL**1
01190                                                                      CL**1
01191      IF EX-STATE NOT EQUAL STATE-SUB (CLAS-INDEXS)                   CL**1
01192         MOVE CLAS-STARTS TO CLAS-INDEXS.                             CL**1
01193                                                                      CL**1
01194  0210-FIND-STATE-LOOP.                                               CL**1
01195                                                                      CL**1
01196      IF CLAS-INDEXS GREATER THAN CLAS-MAXS                           CL**1
01197         GO TO 0220-RELEASE-RECORD.                                   CL**1
01198                                                                      CL**1
01199      IF EX-STATE NOT EQUAL STATE-SUB (CLAS-INDEXS)                   CL**1
01200         ADD +1 TO CLAS-INDEXS                                        CL**1
01201         GO TO 0210-FIND-STATE-LOOP.                                  CL**1
01202                                                                      CL**1
01203      IF WS-A-REPORT EQUAL 'X'                                        CL**1
01204         MOVE 'A'                 TO SX-RECORD-TYPE                   CL**1
01205         GO TO 0220-RELEASE-RECORD                                    CL**1
01206      ELSE                                                            CL**1
01207      IF WS-B-REPORT EQUAL 'X'                                        CL**1
01208         IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '2'                  CL**1
01209            MOVE EX-GROUPING      TO SX-GROUPING                      CL**1
01210            MOVE 'B'              TO SX-RECORD-TYPE                   CL**1
01211            GO TO 0220-RELEASE-RECORD                                 CL**1
01212         ELSE                                                         CL**1
01213            NEXT SENTENCE                                             CL**1
01214      ELSE                                                            CL**1
01215      IF WS-C-REPORT EQUAL 'X'                                        CL**1
01216         IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '3'                  CL**1
01217            MOVE EX-ACCOUNT       TO SX-ACCOUNT                       CL**1
01218            MOVE 'C'              TO SX-RECORD-TYPE                   CL**1
01219            GO TO 0220-RELEASE-RECORD                                 CL**1
01220         ELSE                                                         CL**1
01221            NEXT SENTENCE                                             CL**1
01222      ELSE                                                            CL**1
01223      IF WS-D-REPORT EQUAL 'X'                                        CL**1
01224         IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '4'                  CL**1
01225            MOVE EX-TERM-CD       TO SX-TERM-CD                       CL**1
01226            MOVE 'D'              TO SX-RECORD-TYPE                   CL**1
01227            GO TO 0220-RELEASE-RECORD                                 CL**1
01228         ELSE                                                         CL**1
01229            NEXT SENTENCE                                             CL**1
01230      ELSE                                                            CL**1
01231      IF WS-E-REPORT EQUAL 'X'                                        CL**1
01232         IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '5'                  CL**1
01233            MOVE EX-ACCT-TYPE     TO SX-ACCT-TYPE                     CL**1
01234            MOVE EX-TERM-CD       TO SX-TERM-CD                       CL**1
01235            MOVE 'E'              TO SX-RECORD-TYPE                   CL**1
01236            GO TO 0220-RELEASE-RECORD                                 CL**1
01237         ELSE                                                         CL**1
01238            NEXT SENTENCE                                             CL**1
01239      ELSE                                                            CL**1
01240      IF WS-F-REPORT EQUAL 'X'                                        CL**1
01241         IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '6'                  CL**1
01242            MOVE EX-CAL-TABLE     TO SX-CAL-TABLE                     CL**1
01243            MOVE 'F'              TO SX-RECORD-TYPE                   CL**1
01244            GO TO 0220-RELEASE-RECORD                                 CL**1
01245         ELSE                                                         CL**1
01246            NEXT SENTENCE                                             CL**1
01247      ELSE                                                            CL**1
01248      IF WS-G-REPORT EQUAL 'X'                                        CL**1
01249         IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '7'                  CL**1
01250            MOVE EX-ACCT-TYPE     TO SX-ACCT-TYPE                     CL**1
01251            MOVE 'G'              TO SX-RECORD-TYPE                   CL**1
01252            GO TO 0220-RELEASE-RECORD                                 CL**1
01253         ELSE                                                         CL**1
01254            NEXT SENTENCE                                             CL**1
01255      ELSE                                                            CL**1
01256      IF WS-H-REPORT EQUAL 'X'                                        CL**1
01257         IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '8'                  CL**1
01258            MOVE EX-STATE-DEV     TO SX-STATE-DEV                     CL**1
01259            MOVE 'H'              TO SX-RECORD-TYPE                   CL**1
01260            GO TO 0220-RELEASE-RECORD                                 CL**1
01261         ELSE                                                         CL**1
01262            NEXT SENTENCE                                             CL**1
01263      ELSE                                                            CL**1
01264      IF WS-I-REPORT EQUAL 'X'                                        CL**1
01265         IF STATE-CALL-BREAK (CLAS-INDEXS) EQUAL '9'                  CL**1
01266            MOVE EX-ACCOUNT       TO SX-ACCOUNT                       CL**1
01267            MOVE EX-STATE-DEV     TO SX-STATE-DEV                     CL**1
01268            MOVE 'I'              TO SX-RECORD-TYPE                   CL**1
01269            GO TO 0220-RELEASE-RECORD                                 CL**1
01270         ELSE                                                         CL**1
01271            NEXT SENTENCE                                             CL**1
01272      ELSE                                                            CL**1
01273      IF WS-J-REPORT EQUAL 'X' AND                                    CL**1
01274         STATE-CALL-BREAK (CLAS-INDEXS) EQUAL 'A'                     CL**1
01275         MOVE EX-ACCT-TYPE        TO SX-ACCT-TYPE                     CL**1
01276         MOVE EX-STATE-DEV        TO SX-STATE-DEV                     CL**1
01277         MOVE 'J'                 TO SX-RECORD-TYPE                   CL**1
01278         GO TO 0220-RELEASE-RECORD                                    CL**1
01279      ELSE                                                            CL**1
01280      IF WS-K-REPORT EQUAL 'X' AND                                    CL**1
01281         STATE-CALL-BREAK (CLAS-INDEXS) EQUAL 'B'                     CL**1
01282         MOVE EX-ACCT-TYPE        TO SX-ACCT-TYPE                     CL**1
01283         MOVE SPACES              TO SX-STATE                         CL**1
01284         MOVE 'K'                 TO SX-RECORD-TYPE                   CL**1
01285         GO TO 0220-RELEASE-RECORD.                                   CL**1
01286                                                                      CL**1
01287      GO TO 0190-READ-EXTRACT.                                        CL**1
01288                                                                      CL**1
01289  0220-RELEASE-RECORD.                                                CL**1
01290                                                                      CL**1
01291      IF SX-RECORD-TYPE NOT EQUAL 'K'                                 CL**1
01292          MOVE EX-STATE   TO SX-STATE.                                CL**1
01293      MOVE EX-CARRIER TO SX-CARRIER.                                  CL**1
01294                                                                      CL**1
01295      IF EX-REIN NOT EQUAL SPACES                                     CL**1
01296         MOVE EX-REIN TO SX-REIN-CO.                                  CL**1
01297                                                                      CL**1
01298      MOVE CALL-EXTRACT     TO SORT-RECORD.                           CL**1
01299      RELEASE SORT-RECORD.                                            CL**1
01300                                                                      CL**1
01301      GO TO 0190-READ-EXTRACT.                                        CL**1
01302                                                                      CL**1
01303  0230-END-INPUT-PROC.                                                CL**1
01304      EXIT.                                                           CL**1
01305      EJECT                                                           CL**1
01306  0300-PRINT-REPORT SECTION.                                          CL**1
01307                                                                      CL**1
01308  0310-RETURN-SORT-RECORD.                                            CL**1
01309                                                                      CL**1
01310      RETURN SORT-FILE INTO CALL-EXTRACT AT END                       CL**1
01311          MOVE HIGH-VALUES TO SORT-KEY                                CL**1
01312          GO TO 0390-CLOSE-EXTRACT.                                   CL**1
01313                                                                      CL**1
01314      IF SORT-KEY NOT EQUAL LAST-CONTROL                              CL**1
01315         IF LAST-CONTROL NOT EQUAL LOW-VALUES                         CL**1
01316            PERFORM 0500-PRINT-BREAKS THRU 0610-BREAK-EXIT.           CL**1
01317                                                                      CL**1
01318      MOVE SORT-KEY        TO LAST-CONTROL.                           CL**1
01319      MOVE EX-NAME         TO LAS-ACT-NAME.                           CL**1
01320                                                                      CL**1
01321  0320-FIND-STATE-DESC.                                               CL**1
01322                                                                      CL**1
01323      MOVE CLAS-STARTS TO CLAS-INDEXS.                                CL**1
01324                                                                      CL**1
01325  0330-FIND-STATE-DESC-LOOP.                                          CL**1
01326                                                                      CL**1
01327      IF CLAS-INDEXS GREATER THAN CLAS-MAXS                           CL**1
01328         MOVE 'INVALID STATE' TO LAS-STATE-NAME                       CL**1
01329         GO TO 0340-FIND-AH-TYPE.                                     CL**1
01330                                                                      CL**1
01331      IF EX-STATE NOT EQUAL STATE-SUB (CLAS-INDEXS)                   CL**1
01332         ADD +1 TO CLAS-INDEXS                                        CL**1
01333         GO TO 0330-FIND-STATE-DESC-LOOP.                             CL**1
01334                                                                      CL**1
01335      MOVE STATE-PIC (CLAS-INDEXS) TO LAS-STATE-NAME.                 CL**1
01336                                                                      CL**1
01337  0340-FIND-AH-TYPE.                                                  CL**1
01338                                                                      CL**1
01339      IF EX-LIFE-AH NOT EQUAL 'A'                                     CL**1
01340         GO TO 0360-FIND-LF-TYPE.                                     CL**1
01341                                                                      CL**1
01342      IF EX-BEN-TYPE IS EQUAL TO SPACES OR '00'                       CL**1
01343         GO TO 0360-FIND-LF-TYPE.                                     CL**1
01344                                                                      CL**1
01345      MOVE CLAS-STARTA TO CLAS-INDEXA.                                CL**1
01346                                                                      CL**1
01347  0350-FIND-A-TYPE-LOOP.                                              CL**1
01348                                                                      CL**1
01349      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                           CL**1
01350         MOVE ' INVALID A/H  BENEFIT ' TO WS-ABEND-MESSAGE            CL**1
01351         MOVE EX-BEN-TYPE TO WS-ABEND-FILE-STATUS                     CL**1
01352         GO TO ABEND-PGM.                                             CL**1
01353                                                                      CL**1
01354      IF CLAS-I-BEN (CLAS-INDEXA) NOT = EX-BEN-TYPE                   CL**1
01355         ADD +1 TO CLAS-INDEXA                                        CL**1
01356         GO TO 0350-FIND-A-TYPE-LOOP.                                 CL**1
01357                                                                      CL**1
01358  0360-FIND-LF-TYPE.                                                  CL**1
01359                                                                      CL**1
01360      IF EX-LIFE-AH NOT EQUAL 'L'                                     CL**1
01361         GO TO 0380-BUILD-TOTALS.                                     CL**1
01362                                                                      CL**1
01363      IF EX-BEN-TYPE IS EQUAL TO SPACES OR '00'                       CL**1
01364         GO TO 0380-BUILD-TOTALS.                                     CL**1
01365                                                                      CL**1
01366      MOVE CLAS-STARTL TO CLAS-INDEXL.                                CL**1
01367                                                                      CL**1
01368  0370-FIND-L-TYPE-LOOP.                                              CL**1
01369                                                                      CL**1
01370      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                           CL**1
01371          MOVE ' INVALID LIFE BENEFIT ' TO WS-ABEND-MESSAGE           CL**1
01372          MOVE EX-BEN-TYPE TO WS-ABEND-FILE-STATUS                    CL**1
01373          GO TO ABEND-PGM.                                            CL**1
01374                                                                      CL**1
01375      IF CLAS-I-BEN (CLAS-INDEXL) NOT = EX-BEN-TYPE                   CL**1
01376         ADD +1 TO CLAS-INDEXL                                        CL**1
01377         GO TO 0370-FIND-L-TYPE-LOOP.                                 CL**1
01378                                                                      CL**1
01379  0380-BUILD-TOTALS.                                                  CL**1
01380                                                                      CL**1
01381      MOVE SX-RECORD-TYPE TO HEAD-A-TYPE.                             CL**1
01382                                                                      CL**1
01383      IF SX-RECORD-TYPE EQUAL 'A' OR 'B' OR 'K'                       CL**1
01384         MOVE 3 TO CA                                                 CL**1
01385      ELSE                                                            CL**1
01386      IF SX-RECORD-TYPE EQUAL 'E' OR 'I' OR 'J'                       CL**1
01387         MOVE 1 TO CA                                                 CL**1
01388      ELSE                                                            CL**1
01389         MOVE 2 TO CA.                                                CL**1
01390                                                                      CL**1
01391      IF EX-ENTRY-DATE NOT NUMERIC                                    CL**1
01392         MOVE ZEROS TO EX-ENTRY-DATE.                                 CL**1
01393                                                                      CL**1
01394      MOVE EX-ENTRY-DATE    TO WS-WORK-DATE.                          CL**7
01395                                                                      CL**1
01396      COMPUTE DTE = (WS-WORK-CCYY * +12) + WS-WORK-MM.                CL**4
01397                                                                      CL**1
01398      PERFORM 0420-ACCUMULATE-TOTALS THRU                             CL**1
01399              0470-ACCUMULATE-EXIT VARYING YR                         CL**1
01400         FROM +1 BY +1 UNTIL YR GREATER THAN +3.                      CL**1
01401                                                                      CL**1
01402      GO TO 0310-RETURN-SORT-RECORD.                                  CL**1
01403                                                                      CL**1
01404  0390-CLOSE-EXTRACT.                                                 CL**1
01405                                                                      CL**1
01406      PERFORM 0500-PRINT-BREAKS THRU 0610-BREAK-EXIT.                 CL**1
01407      CLOSE EXTRACT.                                                  CL**1
01408      MOVE LOW-VALUES TO LAST-CONTROL.                                CL**1
01409      PERFORM 0100-BEGIN-PROGRAM.                                     CL**1
01410                                                                      CL**1
01411  0400-END-OUTPUT-PROC.                                               CL**1
01412      EXIT.                                                           CL**1
01413      EJECT                                                           CL**1
01414  0410-END-OF-JOB.                                                    CL**1
01415                                                                      CL**1
01416      MOVE ZEROS  TO RETURN-CODE.
           CLOSE CALL-PRT FICH
01416      GOBACK.                                                         CL**1
01417                                                                      CL**1
01418  0420-ACCUMULATE-TOTALS.                                             CL**1
01419                                                                      CL**1
01420  0430-TRY-LIFE-TYPES.                                                CL**1
01421                                                                      CL**1
01422      IF EX-LIFE-AH NOT EQUAL 'L'                                     CL**1
01423         GO TO 0450-TRY-AH-TYPES.                                     CL**1
01424                                                                      CL**1
01425      IF SX-RECORD-TYPE EQUAL 'D' OR 'E'                              CL**1
01426         ADD EX-CLAIM-AMT (YR) TO                                     CL**1
01427                          WS-TOTAL-LF-PMTS (1 YR CLAS-INDEXL)         CL**1
01428         ADD EX-CLAIM-AMT (YR) TO                                     CL**1
01429                          WS-TOTAL-LF-PMTS (2 YR CLAS-INDEXL)         CL**1
01430         IF EX-RECORD-TYPE EQUAL '1'                                  CL**1
01431            ADD EX-E-LOSS (YR) TO                                     CL**1
01432                          WS-TOTAL-E-LF-RESV (1 YR CLAS-INDEXL)       CL**1
01433            ADD EX-E-LOSS (YR) TO                                     CL**1
01434                          WS-TOTAL-E-LF-RESV (2 YR CLAS-INDEXL)       CL**1
01435            ADD EX-B-LOSS (YR) TO                                     CL**1
01436                          WS-TOTAL-B-LF-RESV (1 YR CLAS-INDEXL)       CL**1
01437            ADD EX-B-LOSS (YR) TO                                     CL**1
01438                          WS-TOTAL-B-LF-RESV (2 YR CLAS-INDEXL).      CL**1
01439                                                                      CL**1
01440      IF EX-TX-RESV-ADJ-REC EQUAL 'X'                                 CL**1
01441         GO TO 0470-ACCUMULATE-EXIT.                                  CL**1
01442                                                                      CL**1
01443      MOVE LF-A-TYPE (CA YR CLAS-INDEXL) TO A-AREA.                   CL**1
01444                                                                      CL**1
01445      MOVE 1                      TO PRT-IND.                         CL**1
01446                                                                      CL**1
01447      MOVE +0 TO WORK-PREM.                                           CL**1
01448                                                                      CL**1
01449      IF (SINGLE-PERIOD-PROCESS) AND (YR GREATER THAN +1)             CL**1
01450          GO TO 0435-PROCESS-SINGLE-YEAR.                             CL**1
01451                                                                      CL**1
01452      IF (DTE  GREATER  DAT-LIM (YR))  AND                            CL**1
01453         (DTE NOT GREATER THAN DAT-MAX (YR))                          CL**1
01454          ADD  EX-PREM  TO  PREM                                      CL**1
01455          ADD  EX-PREM-ALT TO PREM                                    CL**1
01456          MOVE EX-PRIM-FAC-PREM  TO  WORK-PREM                        CL**1
01457          ADD  EX-PRIM-FAC-PREM-ALT TO WORK-PREM.                     CL**1
01458                                                                      CL**1
01459  0435-PROCESS-SINGLE-YEAR.                                           CL**1
01460                                                                      CL**1
01461      ADD EX-CNC-AMT (YR)         TO CREF.                            CL**1
01462      ADD EX-MEAN-INFORCE (YR)    TO MEANS (CA YR CLAS-INDEXL).       CL**1
01463      ADD EX-CLAIM-AMT (YR)       TO CLAIM-AMT.                       CL**1
01464      ADD EX-OVR-COMM (YR)        TO OVR-COMM.                        CL**1
01465      ADD EX-AGT-COMM (YR)        TO AGT-COMM.                        CL**1
01466      ADD EX-B-IBNR (YR)          TO B-IBNR.                          CL**1
01467      ADD EX-E-IBNR (YR)          TO E-IBNR.                          CL**1
01468      ADD EX-B-LOSS (YR)          TO B-LOSS.                          CL**1
01469      ADD EX-E-LOSS (YR)          TO E-LOSS.                          CL**1
01470      ADD EX-RETRO-PMTS (YR)      TO RETRO-PMTS.                      CL**1
01471      ADD EX-C-CNT (YR)           TO CNT.                             CL**1
01472      ADD EX-INC-CNT (YR)         TO CLM-CNT.                         CL**1
01473      ADD EX-PD-CNT (YR)          TO PD-CNT.                          CL**1
01474      MOVE EX-PREM                TO WRK-C                            CL**1
01475                                     HOLD-PREM.                       CL**1
01476                                                                      CL**1
01477      IF EX-RECORD-TYPE EQUAL '1'                                     CL**1
01478         GO TO 0440-PUT-BACK-LIFE.                                    CL**1
01479                                                                      CL**1
01480      IF (EX-PREM NOT GREATER THAN +0) AND                            CL**1
01481          (EX-CNC-AMT (YR) NOT GREATER THAN +0)                       CL**1
01482          GO TO 0440-PUT-BACK-LIFE.                                   CL**1
01483                                                                      CL**1
01484      ADD EX-BEG-ST-RES (YR)      TO BEGIN-PRM-RES.                   CL**1
01485      ADD EX-END-ST-RES (YR)      TO END-PRM-RES.                     CL**1
01486                                                                      CL**1
01487      IF EX-PREM NOT GREATER THAN +0                                  CL**1
01488          MOVE EX-CNC-AMT (YR) TO PRIMA-REFUND                        CL**1
01489      ELSE                                                            CL**1
01490          COMPUTE PRIMA-REFUND EQUAL                                  CL**1
01491          ((EX-PRIM-FAC-PREM / EX-PREM) * EX-CNC-AMT (YR)).           CL**1
01492                                                                      CL**1
01493      COMPUTE PRIM-FAC-EARN-PRM EQUAL PRIM-FAC-EARN-PRM +             CL**1
01494      ((WORK-PREM + EX-PRI-BEG-ST-RES (YR)) -                         CL**1
01495       EX-PRI-END-ST-RES (YR) - PRIMA-REFUND).                        CL**1
01496                                                                      CL**1
01497  0440-PUT-BACK-LIFE.                                                 CL**1
01498                                                                      CL**1
01499      MOVE A-AREA TO LF-A-TYPE (CA YR CLAS-INDEXL)                    CL**1
01500      GO TO 0470-ACCUMULATE-EXIT.                                     CL**1
01501                                                                      CL**1
01502      EJECT                                                           CL**1
01503  0450-TRY-AH-TYPES.                                                  CL**1
01504                                                                      CL**1
01505      IF EX-LIFE-AH NOT EQUAL 'A'                                     CL**1
01506         GO TO 0470-ACCUMULATE-EXIT.                                  CL**1
01507                                                                      CL**1
01508      SUBTRACT CLAS-MAXL FROM CLAS-INDEXA.                            CL**1
01509                                                                      CL**1
01510      IF SX-RECORD-TYPE EQUAL 'D' OR 'E'                              CL**1
01511         ADD EX-CLAIM-AMT (YR) TO                                     CL**1
01512                          WS-TOTAL-AH-PMTS (1 YR CLAS-INDEXA)         CL**1
01513         ADD EX-CLAIM-AMT (YR) TO                                     CL**1
01514                          WS-TOTAL-AH-PMTS (2 YR CLAS-INDEXA)         CL**1
01515         IF EX-RECORD-TYPE EQUAL '1'                                  CL**1
01516            ADD EX-E-LOSS (YR) TO                                     CL**1
01517                          WS-TOTAL-E-AH-RESV (1 YR CLAS-INDEXA)       CL**1
01518            ADD EX-E-LOSS (YR) TO                                     CL**1
01519                          WS-TOTAL-E-AH-RESV (2 YR CLAS-INDEXA)       CL**1
01520            ADD EX-E-IBNR (YR) TO                                     CL**1
01521                          WS-TOTAL-E-AH-RESV (1 YR CLAS-INDEXA)       CL**1
01522            ADD EX-E-IBNR (YR) TO                                     CL**1
01523                          WS-TOTAL-E-AH-RESV (2 YR CLAS-INDEXA)       CL**1
01524                                                                      CL**1
01525            ADD EX-B-LOSS (YR) TO                                     CL**1
01526                          WS-TOTAL-B-AH-RESV (1 YR CLAS-INDEXA)       CL**1
01527            ADD EX-B-LOSS (YR) TO                                     CL**1
01528                          WS-TOTAL-B-AH-RESV (2 YR CLAS-INDEXA)       CL**1
01529            ADD EX-B-IBNR (YR) TO                                     CL**1
01530                          WS-TOTAL-B-AH-RESV (1 YR CLAS-INDEXA)       CL**1
01531            ADD EX-B-IBNR (YR) TO                                     CL**1
01532                          WS-TOTAL-B-AH-RESV (2 YR CLAS-INDEXA).      CL**1
01533                                                                      CL**1
01534      IF EX-TX-RESV-ADJ-REC EQUAL 'X'                                 CL**1
01535         ADD CLAS-MAXL TO CLAS-INDEXA                                 CL**1
01536         GO TO 0470-ACCUMULATE-EXIT.                                  CL**1
01537                                                                      CL**1
01538      MOVE AH-A-TYPE (CA YR CLAS-INDEXA) TO A-AREA.                   CL**1
01539      ADD CLAS-MAXL TO CLAS-INDEXA.                                   CL**1
01540      ADD EX-C-CNT (YR) TO CNT.                                       CL**1
01541                                                                      CL**1
01542      MOVE 1 TO PRT-IND.                                              CL**1
01543                                                                      CL**1
01544      MOVE +0 TO WORK-PREM.                                           CL**1
01545                                                                      CL**1
01546      IF (SINGLE-PERIOD-PROCESS) AND (YR GREATER THAN +1)             CL**1
01547          GO TO 0455-PROCESS-SINGLE-YEAR.                             CL**1
01548                                                                      CL**1
01549      IF  (DTE  GREATER  DAT-LIM (YR))  AND                           CL**1
01550          (DTE NOT GREATER THAN DAT-MAX (YR))                         CL**1
01551          ADD EX-PREM      TO PREM                                    CL**1
01552          MOVE EX-PRIM-FAC-PREM  TO  WORK-PREM.                       CL**1
01553                                                                      CL**1
01554  0455-PROCESS-SINGLE-YEAR.                                           CL**1
01555                                                                      CL**1
01556      ADD EX-CNC-AMT (YR)         TO CREF.                            CL**1
01557      ADD EX-CLAIM-AMT (YR)       TO CLAIM-AMT.                       CL**1
01558      ADD EX-OVR-COMM (YR)         TO OVR-COMM.                       CL**1
01559      ADD EX-AGT-COMM (YR)         TO AGT-COMM.                       CL**1
01560      ADD EX-B-IBNR (YR)          TO B-LOSS.                          CL**1
01561      ADD EX-E-IBNR (YR)          TO E-LOSS.                          CL**1
01562      ADD EX-B-LOSS (YR)          TO B-LOSS.                          CL**1
01563      ADD EX-E-LOSS (YR)          TO E-LOSS.                          CL**1
01564      ADD EX-RETRO-PMTS (YR)      TO RETRO-PMTS.                      CL**1
01565      ADD EX-INC-CNT (YR)         TO CLM-CNT.                         CL**1
01566      ADD EX-PD-CNT (YR)          TO PD-CNT.                          CL**1
01567      MOVE EX-PREM                TO WRK-C                            CL**1
01568                                     HOLD-PREM.                       CL**1
01569                                                                      CL**1
01570      IF EX-RECORD-TYPE EQUAL '1'                                     CL**1
01571         GO TO 0460-PUT-BACK-AH.                                      CL**1
01572                                                                      CL**1
01573      IF EX-PREM NOT GREATER THAN +0                                  CL**1
01574         GO TO 0460-PUT-BACK-AH.                                      CL**1
01575                                                                      CL**1
           MOVE '1' TO STATE-CALL-EARN (CLAS-INDEXS)
01576      IF STATE-CALL-EARN (CLAS-INDEXS) EQUAL '4'                      CL**1
01577         ADD EX-BEG-MEAN-RES (YR) TO BEGIN-PRM-RES                    CL**1
01578         ADD EX-END-MEAN-RES (YR) TO END-PRM-RES                      CL**1
01579         MOVE EX-PRI-BEG-MEAN-RES (YR) TO WORK-BEG-RES                CL**1
01580         MOVE EX-PRI-END-MEAN-RES (YR) TO WORK-END-RES                CL**1
01581      ELSE                                                            CL**1
01582      IF STATE-CALL-EARN (CLAS-INDEXS) EQUAL '2'                      CL**1
01583         ADD EX-BEG-R78-RES (YR) TO BEGIN-PRM-RES                     CL**1
01584         ADD EX-END-R78-RES (YR) TO END-PRM-RES                       CL**1
01585         MOVE EX-PRI-BEG-R78-RES (YR) TO WORK-BEG-RES                 CL**1
01586         MOVE EX-PRI-END-R78-RES (YR) TO WORK-END-RES                 CL**1
01587      ELSE                                                            CL**1
01588      IF STATE-CALL-EARN (CLAS-INDEXS) EQUAL '3'                      CL**1
01589         ADD EX-BEG-PRO-RES (YR) TO BEGIN-PRM-RES                     CL**1
01590         ADD EX-END-PRO-RES (YR) TO END-PRM-RES                       CL**1
01591         MOVE EX-PRI-BEG-PRO-RES (YR) TO WORK-BEG-RES                 CL**1
01592         MOVE EX-PRI-END-PRO-RES (YR) TO WORK-END-RES                 CL**1
01593      ELSE                                                            CL**1
01594         ADD EX-BEG-ST-RES (YR) TO BEGIN-PRM-RES                      CL**1
01595         ADD EX-END-ST-RES (YR) TO END-PRM-RES                        CL**1
01596         MOVE EX-PRI-BEG-ST-RES (YR) TO WORK-BEG-RES                  CL**1
01597         MOVE EX-PRI-END-ST-RES (YR) TO WORK-END-RES.                 CL**1
01598                                                                      CL**1
01599      COMPUTE PRIMA-REFUND EQUAL                                      CL**1
01600      ((EX-PRIM-FAC-PREM / EX-PREM) * EX-CNC-AMT (YR))                CL**1
01601      COMPUTE PRIM-FAC-EARN-PRM EQUAL PRIM-FAC-EARN-PRM +             CL**1
01602      ((WORK-PREM + WORK-BEG-RES) -                                   CL**1
01603      WORK-END-RES - PRIMA-REFUND).                                   CL**1
01604                                                                      CL**1
01605  0460-PUT-BACK-AH.                                                   CL**1
01606                                                                      CL**1
01607      SUBTRACT CLAS-MAXL FROM CLAS-INDEXA.                            CL**1
01608      MOVE A-AREA TO AH-A-TYPE (CA YR CLAS-INDEXA)                    CL**1
01609      ADD CLAS-MAXL TO CLAS-INDEXA.                                   CL**1
01610                                                                      CL**1
01611  0470-ACCUMULATE-EXIT.                                               CL**1
01612      EXIT.                                                           CL**1
01613                                                                      CL**1
01614      EJECT                                                           CL**1
01615  0500-PRINT-BREAKS.                                                  CL**1
01616                                                                      CL**1
01617      MOVE LAS-REIN-CO    TO HEAD-D-REIN-CO                           CL**1
01618      MOVE LAS-CARRIER    TO HEADE-CARR                               CL**1
01619      MOVE LAS-GROUPING   TO HEADF-COMP                               CL**1
01620      MOVE LAS-STATE-NAME TO HEADG-STATE.                             CL**1
01621                                                                      CL**1
01622      IF LAS-RECORD-TYPE NOT EQUAL 'H' AND 'I' AND 'J'                CL**1
01623         GO TO 0505-BUS-TOTS.                                         CL**1
01624                                                                      CL**1
01625      MOVE 2 TO CA.                                                   CL**1
01626      MOVE 3 TO CB.                                                   CL**1
01627                                                                      CL**1
01628      IF LAS-RECORD-TYPE EQUAL 'I' OR 'J'                             CL**1
01629         MOVE 1 TO CA                                                 CL**1
01630         MOVE 2 TO CB.                                                CL**1
01631                                                                      CL**1
01632      IF LAS-RECORD-TYPE EQUAL 'I'                                    CL**1
01633          MOVE LAS-ACCOUNT                 TO HEADGG-VAR-ACCT         CL**1
01634          MOVE '/'                         TO HEADGG-VAR-SLASH        CL**1
01635          MOVE LAS-STATE-DEV               TO HEADGG-VAR-DEV          CL**1
01636          MOVE LAS-ACT-NAME                TO HEADGG-VARA             CL**1
01637          MOVE ' ACCOUNT / DEVIATION  -  ' TO HEADGG-VAR-DESC         CL**1
01638          MOVE 'ACCOUNT / DEVIATION'       TO HD-TYPE-TOT.            CL**1
01639                                                                      CL**1
01640      IF LAS-RECORD-TYPE EQUAL 'H' OR 'J'                             CL**1
01641          MOVE LAS-STATE-DEV               TO HEADGG-VAR              CL**1
01642          MOVE ' RATE DEVIATION    - '     TO HEADGG-VAR-DESC         CL**1
01643          MOVE 'DEVIATION'                 TO HD-TYPE-TOT             CL**1
01644          IF LAS-RECORD-TYPE EQUAL 'J'                                CL**1
01645              PERFORM 0700-FIND-BUSC-TYPE THRU 0750-EXIT              CL**1
01646              MOVE TEMP-BUS-HEAD           TO HEAD-HH-VAR.            CL**1
01647                                                                      CL**1
01648      PERFORM 0500-PRNT-RTN THRU 0530-E-PRNTS.                        CL**1
01649      MOVE Z-L TO AH-NO-TYPE (CA).                                    CL**1
01650      MOVE Z-L TO LF-NO-TYPE (CA).                                    CL**1
01651      MOVE Z-M TO TYPE-MEAN (CA).                                     CL**1
01652      MOVE SPACES                 TO HEADGG-VAR-DESC.                 CL**1
01653      MOVE SPACES                 TO HEADGG-VAR.                      CL**1
01654      MOVE SPACES                 TO HEADGG-VARA.                     CL**1
01655      MOVE SPACES                 TO HEAD-HH-VAR.                     CL**1
01656      MOVE SX-STATE-DEV           TO LAS-STATE-DEV.                   CL**1
01657      IF LAST-CONTROL EQUAL SORT-KEY                                  CL**1
01658         GO TO 0610-BREAK-EXIT.                                       CL**1
01659                                                                      CL**1
01660  0505-BUS-TOTS.                                                      CL**1
01661                                                                      CL**1
01662      IF LAS-RECORD-TYPE NOT EQUAL 'E' AND 'G' AND 'J' AND 'K'        CL**1
01663         GO TO 0510-TAB-TOTS.                                         CL**1
01664                                                                      CL**1
01665      MOVE 2 TO CA.                                                   CL**1
01666      MOVE 3 TO CB.                                                   CL**1
01667                                                                      CL**1
01668      IF LAS-RECORD-TYPE EQUAL 'E'                                    CL**1
01669          MOVE 1 TO CA                                                CL**1
01670          MOVE 2 TO CB.                                               CL**1
01671                                                                      CL**1
01672      IF LAS-RECORD-TYPE EQUAL 'K'                                    CL**1
01673          MOVE 3 TO CA                                                CL**1
01674          MOVE 4 TO CB                                                CL**1
01675          MOVE SPACES              TO HEADG-STATE                     CL**1
01676          MOVE SPACES              TO HEADF-COMP.                     CL**1
01677                                                                      CL**1
01678      MOVE '  BUSINESS TYPE  ' TO HD-TYPE-TOT.                        CL**1
01679      PERFORM 0700-FIND-BUSC-TYPE THRU 0750-EXIT.                     CL**1
01680                                                                      CL**1
01681      IF LAS-RECORD-TYPE EQUAL 'K'                                    CL**1
01682          MOVE TEMP-BUS-HEAD      TO HEAD-FF-VAR.                     CL**1
01683                                                                      CL**1
01684      IF LAS-RECORD-TYPE EQUAL 'G'                                    CL**1
01685          MOVE TEMP-BUS-HEAD      TO HEAD-GG-VAR.                     CL**1
01686                                                                      CL**1
01687      IF LAS-RECORD-TYPE EQUAL 'J'                                    CL**1
01688          PERFORM 0700-FIND-BUSC-TYPE THRU 0750-EXIT                  CL**1
01689          MOVE TEMP-BUS-HEAD           TO HEAD-HH-VAR.                CL**1
01690                                                                      CL**1
01691      IF LAS-RECORD-TYPE NOT EQUAL 'E'                                CL**1
01692          GO TO 0507-CONTINUE.                                        CL**1
01693                                                                      CL**1
01694      IF LAS-TERM-CD EQUAL '1' OR '2'                                 CL**1
01695         MOVE SPACES            TO HEADHH-VAR-DESC                    CL**1
01696         MOVE 'TERM - '         TO HEADHH-VAR-TERM                    CL**1
01697         MOVE TEMP-BUS-HEAD      TO HEADHH-VAR.                       CL**1
01698                                                                      CL**1
01699      IF LAS-TERM-CD EQUAL '1'                                        CL**1
01700         MOVE 'UNDER 60 MONTHS' TO HEADHH-TERM-DESC                   CL**1
01701      ELSE                                                            CL**1
01702         IF LAS-TERM-CD EQUAL '2'                                     CL**1
01703             MOVE 'OVER 60 MONTHS' TO HEADHH-TERM-DESC                CL**1
01704      ELSE                                                            CL**1
01705         MOVE TEMP-BUS-HEAD      TO HEAD-HH-VAR.                      CL**1
01706                                                                      CL**1
01707  0507-CONTINUE.                                                      CL**1
01708                                                                      CL**1
01709      MOVE +1 TO TX.                                                  CL**1
01710      PERFORM 0500-PRNT-RTN THRU 0530-E-PRNTS.                        CL**1
01711      MOVE Z-L TO AH-NO-TYPE (CA).                                    CL**1
01712      MOVE Z-L TO LF-NO-TYPE (CA).                                    CL**1
01713      MOVE Z-M TO TYPE-MEAN (CA).                                     CL**1
01714      MOVE SPACES                 TO HEADGG-VAR-DESC.                 CL**1
01715      MOVE SPACES                 TO HEADGG-VAR.                      CL**1
01716      MOVE SPACES                 TO HEAD-FF-VAR.                     CL**1
01717      MOVE SPACES                 TO HEAD-HH-VAR.                     CL**1
01718      MOVE SX-ACCT-TYPE           TO LAS-ACCT-TYPE.                   CL**1
01719                                                                      CL**1
01720      IF LAST-CONTROL EQUAL SORT-KEY                                  CL**1
01721         MOVE Z-C TO TX-LF-CLM-ADJUSTMENTS (1)                        CL**1
01722         MOVE Z-C TO TX-AH-CLM-ADJUSTMENTS (1)                        CL**1
01723         GO TO 0610-BREAK-EXIT.                                       CL**1
01724                                                                      CL**1
01725  0510-TAB-TOTS.                                                      CL**1
01726                                                                      CL**1
01727      IF LAS-RECORD-TYPE NOT EQUAL 'F'                                CL**1
01728         GO TO 0530-TERM-TOTS.                                        CL**1
01729                                                                      CL**1
01730      MOVE 2 TO CA, MOVE 3 TO CB.                                     CL**1
01731                                                                      CL**1
01732      MOVE LAS-CAL-TABLE          TO HEADGG-VAR.                      CL**1
01733      MOVE ' RATE CLASS CODE   - ' TO HEADGG-VAR-DESC.                CL**1
01734      MOVE '  CLASS  ' TO HD-TYPE-TOT.                                CL**1
01735      PERFORM 0500-PRNT-RTN THRU 0530-E-PRNTS.                        CL**1
01736      MOVE Z-L TO AH-NO-TYPE (CA).                                    CL**1
01737      MOVE Z-L TO LF-NO-TYPE (CA).                                    CL**1
01738      MOVE Z-M TO TYPE-MEAN (CA).                                     CL**1
01739      MOVE SPACES                 TO HEADGG-VAR-DESC.                 CL**1
01740      MOVE SPACES                 TO HEADGG-VAR.                      CL**1
01741      MOVE SX-CAL-TABLE           TO LAS-CAL-TABLE.                   CL**1
01742      IF LAST-CONTROL EQUAL SORT-KEY                                  CL**1
01743         GO TO 0610-BREAK-EXIT.                                       CL**1
01744                                                                      CL**1
01745  0530-TERM-TOTS.                                                     CL**1
01746                                                                      CL**1
01747      IF LAS-RECORD-TYPE NOT EQUAL 'D' AND 'E'                        CL**1
01748         GO TO 0550-ACCT-TOTS.                                        CL**1
01749                                                                      CL**1
01750      MOVE 2 TO CA.                                                   CL**1
01751      MOVE 3 TO CB.                                                   CL**1
01752                                                                      CL**1
01753      MOVE '  TERM      '  TO HD-TYPE-TOT.                            CL**1
01754                                                                      CL**1
01755      IF LAS-TERM-CD EQUAL '1' OR '2'                                 CL**1
01756         MOVE SPACES            TO HEADHH-VAR-DESC                    CL**1
01757         MOVE 'TERM - '         TO HEADHH-VAR-TERM                    CL**1
01758         MOVE SPACES             TO HEADHH-VAR.                       CL**1
01759                                                                      CL**1
01760      IF LAS-TERM-CD EQUAL '1'                                        CL**1
01761         MOVE 'UNDER 60 MONTHS' TO HEADHH-TERM-DESC                   CL**1
01762      ELSE                                                            CL**1
01763         IF LAS-TERM-CD EQUAL '2'                                     CL**1
01764             MOVE 'OVER 60 MONTHS' TO HEADHH-TERM-DESC.               CL**1
01765                                                                      CL**1
01766      MOVE +2 TO TX.                                                  CL**1
01767      PERFORM 0500-PRNT-RTN THRU 0530-E-PRNTS.                        CL**1
01768      MOVE Z-L TO AH-NO-TYPE (CA).                                    CL**1
01769      MOVE Z-L TO LF-NO-TYPE (CA).                                    CL**1
01770      MOVE Z-M TO TYPE-MEAN (CA).                                     CL**1
01771      MOVE SPACES                 TO HEADGG-VAR-DESC.                 CL**1
01772      MOVE SPACES                 TO HEADGG-VAR.                      CL**1
01773      MOVE SPACES                 TO HEADGG-VARA.                     CL**1
01774      MOVE SX-TERM-CD             TO LAS-TERM-CD.                     CL**1
01775                                                                      CL**1
01776      IF LAST-CONTROL EQUAL SORT-KEY                                  CL**1
01777         MOVE Z-C TO TX-LF-CLM-ADJUSTMENTS (1)                        CL**1
01778         MOVE Z-C TO TX-AH-CLM-ADJUSTMENTS (1)                        CL**1
01779         MOVE Z-C TO TX-LF-CLM-ADJUSTMENTS (2)                        CL**1
01780         MOVE Z-C TO TX-AH-CLM-ADJUSTMENTS (2)                        CL**1
01781         GO TO 0610-BREAK-EXIT.                                       CL**1
01782                                                                      CL**1
01783  0550-ACCT-TOTS.                                                     CL**1
01784                                                                      CL**1
01785      IF LAS-RECORD-TYPE NOT EQUAL 'C' AND 'I'                        CL**1
01786         GO TO 0560-ST-TOTS.                                          CL**1
01787                                                                      CL**1
01788      MOVE 2 TO CA.                                                   CL**1
01789      MOVE 3 TO CB.                                                   CL**1
01790                                                                      CL**1
01791      MOVE ' ACCOUNT NO.  ' TO HEADGG-VAR-DESC.                       CL**1
01792      MOVE ' ACCOUNT  '     TO HD-TYPE-TOT.                           CL**1
01793      MOVE LAS-ACT-NAME TO HEADGG-VARA.                               CL**1
01794      MOVE LAS-ACCOUNT TO HEADGG-VAR.                                 CL**1
01795                                                                      CL**1
01796      PERFORM 0500-PRNT-RTN THRU 0530-E-PRNTS.                        CL**1
01797      MOVE Z-L TO AH-NO-TYPE (CA).                                    CL**1
01798      MOVE Z-L TO LF-NO-TYPE (CA).                                    CL**1
01799      MOVE Z-M TO TYPE-MEAN (CA).                                     CL**1
01800      MOVE SPACES                 TO HEADGG-VAR-DESC.                 CL**1
01801      MOVE SPACES                 TO HEADGG-VAR.                      CL**1
01802      MOVE SPACES                 TO HEADGG-VARA.                     CL**1
01803      MOVE SX-ACCOUNT             TO LAS-ACCOUNT.                     CL**1
01804      IF LAST-CONTROL EQUAL SORT-KEY                                  CL**1
01805         GO TO 0610-BREAK-EXIT.                                       CL**1
01806                                                                      CL**1
01807  0560-ST-TOTS.                                                       CL**1
01808                                                                      CL**1
01809      IF LAS-RECORD-TYPE EQUAL 'K'                                    CL**1
01810          GO TO 0570-GROUP-BREAK.                                     CL**1
01811                                                                      CL**1
01812      MOVE 3 TO CA.                                                   CL**1
01813      MOVE 4 TO CB.                                                   CL**1
01814      MOVE '  STATE ' TO HD-TYPE-TOT.                                 CL**1
01815      MOVE SPACES                 TO HEADGG-VAR-DESC.                 CL**1
01816      MOVE SPACES                 TO HEADGG-VAR.                      CL**1
01817      MOVE SPACES                 TO HEAD-HH-VAR.                     CL**1
01818      PERFORM 0500-PRNT-RTN THRU 0530-E-PRNTS.                        CL**1
01819      MOVE Z-L TO AH-NO-TYPE (CA).                                    CL**1
01820      MOVE Z-L TO LF-NO-TYPE (CA).                                    CL**1
01821      MOVE Z-M TO TYPE-MEAN (CA).                                     CL**1
01822      MOVE SX-STATE               TO LAS-STATE.                       CL**1
01823      IF LAST-CONTROL EQUAL SORT-KEY                                  CL**1
01824         GO TO 0610-BREAK-EXIT.                                       CL**1
01825                                                                      CL**1
01826  0570-GROUP-BREAK.                                                   CL**1
01827                                                                      CL**1
01828      MOVE SPACES         TO HEADG-STATE.                             CL**1
01829      MOVE SPACES                 TO HEAD-HH-VAR.                     CL**1
01830                                                                      CL**1
01831      IF DTE-CLIENT NOT EQUAL 'IMS' AND 'SEN'                         CL**8
01832         IF LAS-RECORD-TYPE NOT EQUAL 'B'                             CL**8
01833            GO TO 0580-CARRIER-BREAK.                                 CL**8
01834                                                                      CL**8
01835      ADD +1 TO CA.                                                   CL**1
01836      ADD +1 TO CB.                                                   CL**1
01837      MOVE ' GROUPING'            TO HD-TYPE-TOT.                     CL**1
01838      MOVE SPACES                 TO HEADGG-VAR-DESC.                 CL**1
01839      MOVE LAS-GROUPING           TO HEADF-COMP.                      CL**1
01840      PERFORM 0500-PRNT-RTN THRU 0530-E-PRNTS.                        CL**1
01841      MOVE Z-L TO AH-NO-TYPE (CA).                                    CL**1
01842      MOVE Z-L TO LF-NO-TYPE (CA).                                    CL**1
01843      MOVE Z-M TO TYPE-MEAN (CA).                                     CL**1
01844      MOVE SPACES                 TO HEADGG-VAR-DESC.                 CL**1
01845      MOVE SPACES                 TO HEADGG-VAR.                      CL**1
01846                                                                      CL**1
01847      MOVE SX-GROUPING            TO LAS-GROUPING.                    CL**1
01848      IF LAST-CONTROL EQUAL SORT-KEY                                  CL**1
01849         GO TO 0610-BREAK-EXIT.                                       CL**1
01850                                                                      CL**1
01851  0580-CARRIER-BREAK.                                                 CL**1
01852                                                                      CL**1
01853      MOVE SPACES         TO HEADF-COMP.                              CL**1
01854      MOVE SPACES                 TO HEAD-HH-VAR.                     CL**1
01855      ADD +1 TO CA.                                                   CL**1
01856      ADD +1 TO CB.                                                   CL**1
01857      MOVE ' CARRIER' TO HD-TYPE-TOT.                                 CL**1
01858      MOVE SPACES                 TO HEADGG-VAR-DESC.                 CL**1
01859      MOVE LAS-CARRIER            TO HEADE-CARR.                      CL**1
01860      PERFORM 0500-PRNT-RTN THRU 0530-E-PRNTS.                        CL**1
01861      MOVE Z-L TO AH-NO-TYPE (CA).                                    CL**1
01862      MOVE Z-L TO LF-NO-TYPE (CA).                                    CL**1
01863      MOVE Z-M TO TYPE-MEAN (CA).                                     CL**1
01864      MOVE SPACES                 TO HEADGG-VAR-DESC.                 CL**1
01865      MOVE SPACES                 TO HEADGG-VAR.                      CL**1
01866      MOVE SX-CARRIER             TO LAS-CARRIER.                     CL**1
01867      IF LAST-CONTROL EQUAL SORT-KEY                                  CL**1
01868         GO TO 0610-BREAK-EXIT.                                       CL**1
01869                                                                      CL**1
01870  0590-REIN-TOTS.                                                     CL**1
01871                                                                      CL**1
01872      MOVE SPACES         TO HEADE-CARR.                              CL**1
01873      IF LAS-REIN-CO EQUAL LOW-VALUES                                 CL**1
01874         GO TO 0600-FINAL-TOTALS.                                     CL**1
01875                                                                      CL**1
01876      ADD +1 TO CA.                                                   CL**1
01877      ADD +1 TO CB.                                                   CL**1
01878      MOVE ' REINSURANCE COMPANY ' TO HD-TYPE-TOT.                    CL**1
01879      MOVE SPACES                 TO HEADGG-VAR-DESC.                 CL**1
01880      MOVE SPACES                 TO HEADGG-VAR.                      CL**1
01881      MOVE SPACES                 TO HEADGG-VARA.                     CL**1
01882      MOVE LAS-REIN-CO            TO HEAD-D-REIN-CO.                  CL**1
01883      PERFORM 0500-PRNT-RTN THRU 0530-E-PRNTS.                        CL**1
01884      MOVE Z-L TO AH-NO-TYPE (CA).                                    CL**1
01885      MOVE Z-L TO LF-NO-TYPE (CA).                                    CL**1
01886      MOVE Z-M TO TYPE-MEAN (CA).                                     CL**1
01887      MOVE SX-REIN-CO             TO LAS-REIN-CO.                     CL**1
01888                                                                      CL**1
01889      IF LAST-CONTROL EQUAL SORT-KEY                                  CL**1
01890         GO TO 0610-BREAK-EXIT.                                       CL**1
01891                                                                      CL**1
01892  0600-FINAL-TOTALS.                                                  CL**1
01893                                                                      CL**1
01894      MOVE SPACES         TO HEAD-D-REIN-CO.                          CL**1
01895      ADD +1 TO CA                                                    CL**1
01896      ADD +1 TO CB.                                                   CL**1
01897      MOVE SPACES                 TO HEAD-D-REIN-CO.                  CL**1
01898      MOVE SPACES                 TO HEADE-CARR.                      CL**1
01899      MOVE SPACES                 TO HEADF-COMP.                      CL**1
01900      MOVE SPACES                 TO HEADG-STATE.                     CL**1
01901      MOVE SPACES                 TO HEADGG-VAR.                      CL**1
01902                                                                      CL**1
01903      MOVE '  RUN    ' TO HD-TYPE-TOT.                                CL**1
01904      PERFORM 0500-PRNT-RTN THRU 0530-E-PRNTS.                        CL**1
01905      PERFORM 0100-BEGIN-PROGRAM.                                     CL**1
01906      MOVE SPACES TO PRT.                                             CL**1
01907      MOVE '1' TO X.                                                  CL**1
01908      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
01909                                                                      CL**1
01910  0610-BREAK-EXIT.                                                    CL**1
01911      EXIT.                                                           CL**1
01912                                                                      CL**1
01913      EJECT                                                           CL**1
01914  0500-PRNT-RTN.                                                      CL**1
01915                                                                      CL**1
01916      PERFORM 0501-ZERO-LFAH-SET VARYING                              CL**1
01917         YR FROM +1 BY +1 UNTIL YR GREATER THAN +4 AFTER              CL**1
01918         CE FROM +1 BY +1 UNTIL CE GREATER THAN +2.                   CL**1
01919                                                                      CL**1
01920      MOVE LIFE-AH-TABLE TO AH-SGL-OB-TABLE.                          CL**1
01921                                                                      CL**1
01922      PERFORM 0501-ZERO-S-J-SET VARYING                               CL**1
01923         YR FROM +1 BY +1 UNTIL YR GREATER THAN +4 AFTER              CL**1
01924         SI FROM +1 BY +1 UNTIL SI GREATER THAN +3 AFTER              CL**1
01925         CE FROM +1 BY +1 UNTIL CE GREATER THAN +2.                   CL**1
01926                                                                      CL**1
01927      GO TO 0502-ADD-ACCROSS.                                         CL**1
01928                                                                      CL**1
01929  0501-ZERO-LFAH-SET.                                                 CL**1
01930      MOVE ZEROS TO LA-X-PREM (CE YR)     LA-X-RST-PRM (CE YR)        CL**1
01931           LA-X-CREF (CE YR)         LA-X-PRESA (CE YR)               CL**1
01932           LA-X-PRESB (CE YR)        LA-X-CLAIM (CE YR)               CL**1
01933           LA-X-OVR-COMM (CE YR)     LA-X-AGT-COMM (CE YR)            CL**1
01934           LA-X-B-IBNR (CE YR)       LA-X-E-IBNR (CE YR)              CL**1
01935           LA-X-B-LOSS (CE YR)       LA-X-E-LOSS (CE YR)              CL**1
01936           LA-X-RETRO-PMTS (CE YR)   LA-X-CNT (CE YR)                 CL**1
01937           LA-X-CLM-CNT (CE YR)      LA-X-PD-CNT (CE YR)              CL**1
01938           LA-X-PRT-IND (CE YR)      L-X-MEANS (YR).                  CL**1
01939                                                                      CL**1
01940  0501-ZERO-S-J-SET.                                                  CL**1
01941      MOVE ZEROS TO SJ-X-PREM (CE SI YR) SJ-X-RST-PRM (CE SI YR)      CL**1
01942           SJ-X-CREF (CE SI YR)      SJ-X-PRESA (CE SI YR)            CL**1
01943           SJ-X-PRESB (CE SI YR)     SJ-X-CLAIM (CE SI YR)            CL**1
01944           SJ-X-OVR-COMM (CE SI YR)  SJ-X-AGT-COMM (CE SI YR)         CL**1
01945           SJ-X-B-IBNR (CE SI YR)    SJ-X-E-IBNR (CE SI YR)           CL**1
01946           SJ-X-B-LOSS (CE SI YR)    SJ-X-E-LOSS (CE SI YR)           CL**1
01947           SJ-X-RETRO-PMTS (CE SI YR) SJ-X-CNT (CE SI YR)             CL**1
01948           SJ-X-CLM-CNT (CE SI YR)    SJ-X-PD-CNT (CE SI YR)          CL**1
01949           SJ-X-PRT-IND (CE SI YR)     S-X-MEANS (SI YR)              CL**1
01950                                       J-X-MEANS (SI YR).             CL**1
01951                                                                      CL**1
01952  0502-ADD-ACCROSS.                                                   CL**1
01953                                                                      CL**1
01954      PERFORM 0790-ADD-ACROSS-RTN THRU 0800-ADD-ACROSS-EXIT           CL**1
01955          VARYING YR FROM 1 BY 1 UNTIL YR EQUAL +4 AFTER              CL**1
092602                 CE FROM 1 BY 1 UNTIL CE GREATER THAN +450.          CL**1
01957                                                                      CL**1
01958      PERFORM 0805-ADD-ACROSS-RTN THRU 0809-ADD-ACROSS-EXIT           CL**1
01959          VARYING YR FROM 1 BY 1 UNTIL YR EQUAL +4 AFTER              CL**1
092602                 CE FROM 1 BY 1 UNTIL CE GREATER THAN +450.          CL**1
01961                                                                      CL**1
01962      MOVE +1                     TO CG.                              CL**1
01963      MOVE ZERO                   TO PRT-SET.                         CL**1
01964      GO TO 0510-PRINT-L-LOOP.                                        CL**1
01965                                                                      CL**1
01966  0505-ZERO-PRNT-SET.                                                 CL**1
01967      MOVE ZEROS TO X-PREM (YR) X-RST-PRM (YR) X-CREF (YR)            CL**1
01968           X-PRESA (YR)      X-PRESB (YR)    X-CLAIM (YR)             CL**1
01969           X-OVR-COMM (YR)   X-AGT-COMM (YR) X-B-IBNR (YR)            CL**1
01970           X-E-IBNR (YR)     X-B-LOSS (YR)   X-E-LOSS (YR)            CL**1
01971           X-RETRO-PMTS (YR) X-CNT (YR)      X-CLM-CNT (YR)           CL**1
01972           X-PD-CNT (YR)     X-PRT-IND (YR)  X-MEANS (YR).            CL**1
01973                                                                      CL**1
01974  0510-PRINT-L-LOOP.                                                  CL**1
01975      MOVE SPACES TO HEAD-I.                                          CL**1
01976      PERFORM 0505-ZERO-PRNT-SET VARYING                              CL**1
01977         YR FROM +1 BY +1 UNTIL YR GREATER THAN +4.                   CL**1
01978                                                                      CL**1
01979      MOVE +1 TO YR.                                                  CL**1
01980                                                                      CL**1
01981      MOVE '*' TO SKIP-PRINT-SW.                                      CL**1
01982      PERFORM 0810-FILL-5L THRU 0830-E-FILL-5L.                       CL**1
01983                                                                      CL**1
01984      IF (PRT-SET = ZERO OR +2) AND SKIP-PRINT-SW EQUAL ' '           CL**1
01985          PERFORM 0540-PRINT-5-HEADS THRU 0540-E-PRINT-HEAD           CL**1
01986          IF ZERO-PRT-SET EQUAL 'Y'                                   CL**1
01987              MOVE ZERO TO PRT-SET.                                   CL**1
01988                                                                      CL**1
01989      IF SKIP-PRINT-SW EQUAL ' '                                      CL**1
01990          MOVE 'Y' TO CHECK-PRT-SET                                   CL**1
01991          ADD +1 TO PRT-SET                                           CL**1
01992          PERFORM 0550-PRINT-5 THRU 0580-E-PRINT-5.                   CL**1
01993                                                                      CL**1
01994      IF CG LESS THAN CLAS-MAXL                                       CL**1
01995         ADD +1 TO CG                                                 CL**1
01996         GO TO 0510-PRINT-L-LOOP.                                     CL**1
01997                                                                      CL**1
01998      MOVE +1                     TO CG.                              CL**1
01999                                                                      CL**1
02000      MOVE ZERO TO PRT-SET.                                           CL**1
02001  0520-PRINT-A-LOOP.                                                  CL**1
02002      MOVE SPACES TO HEAD-I.                                          CL**1
02003      PERFORM 0505-ZERO-PRNT-SET VARYING                              CL**1
02004         YR FROM +1 BY +1 UNTIL YR GREATER THAN +4.                   CL**1
02005                                                                      CL**1
02006      MOVE +1 TO YR.                                                  CL**1
02007                                                                      CL**1
02008      MOVE '*' TO SKIP-PRINT-SW.                                      CL**1
02009      PERFORM 0840-FILL-5A THRU 0860-E-FILL-5A.                       CL**1
02010                                                                      CL**1
02011      IF (PRT-SET = ZERO OR +2) AND SKIP-PRINT-SW EQUAL ' '           CL**1
02012          PERFORM 0540-PRINT-5-HEADS THRU 0540-E-PRINT-HEAD           CL**1
02013          IF ZERO-PRT-SET EQUAL 'Y'                                   CL**1
02014              MOVE ZERO TO PRT-SET.                                   CL**1
02015                                                                      CL**1
02016      IF SKIP-PRINT-SW EQUAL ' '                                      CL**1
02017          MOVE 'Y' TO CHECK-PRT-SET                                   CL**1
02018          ADD +1 TO PRT-SET                                           CL**1
02019          PERFORM 0550-PRINT-5 THRU 0580-E-PRINT-5.                   CL**1
02020                                                                      CL**1
092602*    IF CG LESS THAN +200                                            CL**1
092602     IF CG LESS THAN +450                                            CL**1
02022         ADD +1 TO CG                                                 CL**1
02023          GO TO 0520-PRINT-A-LOOP.                                    CL**1
02024                                                                      CL**1
02025  0523-PRINT-SNG-JNT-TOTALS.                                          CL**1
02026                                                                      CL**1
02027 *--------------------------------------------------------------*     CL**1
02028 *     PRINT SINGLE LIFE SINGLE PREMIUM TOTALS                  *     CL**1
02029 *--------------------------------------------------------------*     CL**1
02030                                                                      CL**1
02031      MOVE SP-OR-OB (1, 1)        TO PRNT-SET-ACCUM.                  CL**1
02032      MOVE S-X-SP-OB  (1)         TO PRNT-SET-MEANS.                  CL**1
02033      MOVE ' SINGLE LIFE TOTALS'  TO HEADHH-VAR-DESC.                 CL**1
02034      MOVE 'SINGLE PREM TOTALS'   TO HH-BENEFIT.                      CL**1
02035      PERFORM 0540-PRINT-5-HEADS THRU 0540-E-PRINT-HEAD.              CL**1
02036      PERFORM 0550-PRINT-5 THRU 0580-E-PRINT-5.                       CL**1
02037                                                                      CL**1
02038      MOVE ZEROS                  TO X-MEANS (1)   X-MEANS (2)        CL**1
02039                                     X-MEANS (3).                     CL**1
02040                                                                      CL**1
02041      MOVE SPACES                 TO HEADHH-VAR-DESC.                 CL**1
02042                                                                      CL**1
02043 *--------------------------------------------------------------*     CL**1
02044 *     PRINT SINGLE LIFE OUTSTANDING BAL TOTALS                 *     CL**1
02045 *--------------------------------------------------------------*     CL**1
02046                                                                      CL**1
02047      MOVE SP-OR-OB (1, 2)        TO PRNT-SET-ACCUM.                  CL**1
02048      MOVE S-X-SP-OB  (2)         TO PRNT-SET-MEANS.                  CL**1
02049      MOVE 'OUTSTAND BAL TOTALS' TO HH-BENEFIT.                       CL**1
02050      PERFORM 0550-PRINT-5 THRU 0580-E-PRINT-5.                       CL**1
02051                                                                      CL**1
02052      MOVE ZEROS                  TO X-MEANS (1)   X-MEANS (2)        CL**1
02053                                     X-MEANS (3).                     CL**1
02054                                                                      CL**1
02055 *--------------------------------------------------------------*     CL**1
02056 *     PRINT JOINT LIFE SINGLE PREMIUM TOTALS                   *     CL**1
02057 *--------------------------------------------------------------*     CL**1
02058                                                                      CL**1
02059      MOVE SP-OR-OB (2, 1)        TO PRNT-SET-ACCUM.                  CL**1
02060      MOVE J-X-SP-OB  (1)         TO PRNT-SET-MEANS.                  CL**1
02061      MOVE ' JOINT LIFE TOTALS'   TO HEADHH-VAR-DESC.                 CL**1
02062      MOVE 'SINGLE PREM TOTALS '  TO HH-BENEFIT.                      CL**1
02063      PERFORM 0540-PRINT-5-HEADS THRU 0540-E-PRINT-HEAD.              CL**1
02064      PERFORM 0550-PRINT-5 THRU 0580-E-PRINT-5.                       CL**1
02065                                                                      CL**1
02066      MOVE ZEROS                  TO X-MEANS (1)   X-MEANS (2)        CL**1
02067                                     X-MEANS (3).                     CL**1
02068                                                                      CL**1
02069      MOVE SPACES                 TO HEADHH-VAR-DESC.                 CL**1
02070                                                                      CL**1
02071 *--------------------------------------------------------------*     CL**1
02072 *     PRINT JOINT LIFE OUTSTANDING BAL TOTALS                  *     CL**1
02073 *--------------------------------------------------------------*     CL**1
02074                                                                      CL**1
02075      MOVE SP-OR-OB (2, 2)        TO PRNT-SET-ACCUM.                  CL**1
02076      MOVE J-X-SP-OB  (2)         TO PRNT-SET-MEANS.                  CL**1
02077      MOVE 'OUTSTAND BAL TOTALS'  TO HH-BENEFIT.                      CL**1
02078      PERFORM 0550-PRINT-5 THRU 0580-E-PRINT-5.                       CL**1
02079                                                                      CL**1
02080      MOVE ZEROS                  TO X-MEANS (1)   X-MEANS (2)        CL**1
02081                                     X-MEANS (3).                     CL**1
02082                                                                      CL**1
02083  0524-PRINT-AH-SP-OB-TOTALS.                                         CL**1
02084                                                                      CL**1
02085 *--------------------------------------------------------------*     CL**1
02086 *     PRINT A&H SINGLE PREMIUM TOTALS                          *     CL**1
02087 *--------------------------------------------------------------*     CL**1
02088                                                                      CL**1
02089      MOVE AH-SP-OR-OB (1)        TO PRNT-SET-ACCUM.                  CL**1
02090      MOVE ' A&H  TOTALS' TO HEADHH-VAR-DESC.                         CL**1
02091      MOVE 'SINGLE PREM TOTALS'  TO HH-BENEFIT.                       CL**1
02092      PERFORM 0540-PRINT-5-HEADS THRU 0540-E-PRINT-HEAD.              CL**1
02093      PERFORM 0550-PRINT-5 THRU 0580-E-PRINT-5.                       CL**1
02094                                                                      CL**1
02095      MOVE SPACES                 TO HEADHH-VAR-DESC.                 CL**1
02096                                                                      CL**1
02097 *--------------------------------------------------------------*     CL**1
02098 *     PRINT A&H OUTSTANDING BAL TOTALS                         *     CL**1
02099 *--------------------------------------------------------------*     CL**1
02100                                                                      CL**1
02101      MOVE AH-SP-OR-OB (2)        TO PRNT-SET-ACCUM.                  CL**1
02102      MOVE 'OUTSTAND BAL TOTALS'  TO HH-BENEFIT.                      CL**1
02103      PERFORM 0550-PRINT-5 THRU 0580-E-PRINT-5.                       CL**1
02104                                                                      CL**1
02105 *--------------------------------------------------------------*     CL**1
02106 *     PRINT COMBINED SINGLE LIFE TOTALS                        *     CL**1
02107 *--------------------------------------------------------------*     CL**1
02108                                                                      CL**1
02109      MOVE SP-OR-OB (1, 3)        TO PRNT-SET-ACCUM.                  CL**1
02110      MOVE S-X-SP-OB  (3)         TO PRNT-SET-MEANS.                  CL**1
02111      MOVE 'SINGLE LIFE TOTALS' TO HH-BENEFIT.                        CL**1
02112      PERFORM 0540-PRINT-5-HEADS THRU 0540-E-PRINT-HEAD.              CL**1
02113      PERFORM 0550-PRINT-5 THRU 0580-E-PRINT-5.                       CL**1
02114                                                                      CL**1
02115      MOVE ZEROS                  TO X-MEANS (1)   X-MEANS (2)        CL**1
02116                                     X-MEANS (3).                     CL**1
02117 *--------------------------------------------------------------*     CL**1
02118 *     PRINT COMBINED JOINT LIFE TOTALS                         *     CL**1
02119 *--------------------------------------------------------------*     CL**1
02120                                                                      CL**1
02121      MOVE SP-OR-OB (2, 3)        TO PRNT-SET-ACCUM.                  CL**1
02122      MOVE J-X-SP-OB  (3)         TO PRNT-SET-MEANS.                  CL**1
02123      MOVE ' JOINT LIFE TOTALS '  TO HH-BENEFIT.                      CL**1
02124      PERFORM 0550-PRINT-5 THRU 0580-E-PRINT-5.                       CL**1
02125                                                                      CL**1
02126      MOVE ZEROS                  TO X-MEANS (1)   X-MEANS (2)        CL**1
02127                                     X-MEANS (3).                     CL**1
02128                                                                      CL**1
02129  0525-PRINT-LF-AH-TOTALS.                                            CL**1
02130                                                                      CL**1
02131      MOVE LIFE-OR-AH (1)         TO PRNT-SET-ACCUM.                  CL**1
02132      MOVE LIFE-MEANS             TO PRNT-SET-MEANS.                  CL**1
02133      MOVE ' LIFE  TOTALS      '  TO HH-BENEFIT.                      CL**1
02134      PERFORM 0540-PRINT-5-HEADS THRU 0540-E-PRINT-HEAD.              CL**1
02135      PERFORM 0550-PRINT-5 THRU 0580-E-PRINT-5.                       CL**1
02136                                                                      CL**1
02137      MOVE ZEROS                  TO X-MEANS (1)   X-MEANS (2)        CL**1
02138                                     X-MEANS (3).                     CL**1
02139                                                                      CL**1
02140      MOVE LIFE-OR-AH (2)         TO PRNT-SET-ACCUM.                  CL**1
02141      MOVE '  A&H  TOTALS      '  TO HH-BENEFIT.                      CL**1
02142      PERFORM 0550-PRINT-5 THRU 0580-E-PRINT-5.                       CL**1
02143                                                                      CL**1
02144  0530-E-PRNTS.                                                       CL**1
02145      EXIT.                                                           CL**1
02146                                                                      CL**1
02147      EJECT                                                           CL**1
02148  0540-PRINT-5-HEADS.                                                 CL**1
02149                                                                      CL**1
02150      IF SINGLE-PERIOD-PROCESS                                        CL**1
02151         IF (X-MEANS (1) EQUAL ZEROS)      AND                        CL**1
02152            (X-PREM (1) EQUAL ZEROS)       AND                        CL**1
02153            (X-PRESA (1) EQUAL ZEROS)      AND                        CL**1
02154            (X-CREF (1) EQUAL ZEROS)       AND                        CL**1
02155            (X-PRESB (1) EQUAL ZEROS)      AND                        CL**1
02156            (X-RST-PRM (1) EQUAL ZEROS)    AND                        CL**1
02157            (X-CLAIM (1) EQUAL ZEROS)      AND                        CL**1
02158            (X-B-IBNR (1) EQUAL ZEROS)     AND                        CL**1
02159            (X-B-LOSS (1) EQUAL ZEROS)     AND                        CL**1
02160            (X-E-IBNR (1) EQUAL ZEROS)     AND                        CL**1
02161            (X-E-LOSS (1) EQUAL ZEROS)     AND                        CL**1
02162            (X-RETRO-PMTS (1) EQUAL ZEROS) AND                        CL**1
02163            (X-OVR-COMM (1) EQUAL ZEROS)   AND                        CL**1
02164            (X-AGT-COMM (1) EQUAL ZEROS)   AND                        CL**1
02165            (X-CLM-CNT (1)  EQUAL ZEROS)   AND                        CL**1
02166            (X-PD-CNT (1)  EQUAL ZEROS)   AND                         CL**1
02167            (X-CNT (1) EQUAL ZEROS)                                   CL**1
02168             MOVE 'N' TO ZERO-PRT-SET                                 CL**1
02169             GO TO 0540-E-PRINT-HEAD.                                 CL**1
02170                                                                      CL**1
02171      MOVE 'Y' TO ZERO-PRT-SET.                                       CL**1
02172                                                                      CL**1
02173      ADD 1 TO PAGE-CT.                                               CL**1
02174      MOVE PAGE-CT TO HD-PAGE.                                        CL**1
02175                                                                      CL**1
02176      IF HEAD-A-TYPE EQUAL 'A'                                        CL**1
02177         MOVE '           BY STATE           ' TO HD-RPT-TYP          CL**1
02178      ELSE                                                            CL**1
02179      IF HEAD-A-TYPE EQUAL 'B'                                        CL**1
02180         MOVE '     BY STATE / GROUPING      ' TO HD-RPT-TYP          CL**1
02181      ELSE                                                            CL**1
02182      IF HEAD-A-TYPE EQUAL 'C'                                        CL**1
02183         MOVE '          BY ACCOUNT          ' TO HD-RPT-TYP          CL**1
02184      ELSE                                                            CL**1
02185      IF HEAD-A-TYPE EQUAL 'D'                                        CL**1
02186         MOVE '           BY TERM            ' TO HD-RPT-TYP          CL**1
02187      ELSE                                                            CL**1
02188      IF HEAD-A-TYPE EQUAL 'E'                                        CL**1
02189         MOVE '   BY BUSINESS TYPE/TERM      ' TO HD-RPT-TYP          CL**1
02190      ELSE                                                            CL**1
02191      IF HEAD-A-TYPE EQUAL 'F'                                        CL**1
02192         MOVE '        BY RATEING CLASS      ' TO HD-RPT-TYP          CL**1
02193      ELSE                                                            CL**1
02194      IF HEAD-A-TYPE EQUAL 'G'                                        CL**1
02195         MOVE '        BY BUSINESS TYPE      ' TO HD-RPT-TYP          CL**1
02196      ELSE                                                            CL**1
02197      IF HEAD-A-TYPE EQUAL 'H'                                        CL**1
02198         MOVE '        BY RATE DEVIATION     ' TO HD-RPT-TYP          CL**1
02199      ELSE                                                            CL**1
02200      IF HEAD-A-TYPE EQUAL 'I'                                        CL**1
02201         MOVE '    BY ACCOUNT/DEVIATION      ' TO HD-RPT-TYP          CL**1
02202      ELSE                                                            CL**1
02203      IF HEAD-A-TYPE EQUAL 'J'                                        CL**1
02204         MOVE '  BY DEVIATION/BUSINESS TYPE  ' TO HD-RPT-TYP          CL**1
02205      ELSE                                                            CL**1
02206      IF HEAD-A-TYPE EQUAL 'K'                                        CL**1
02207         MOVE '  BY CARRIER/BUSINESS TYPE    ' TO HD-RPT-TYP.         CL**1
02208                                                                      CL**1
02209      IF LAS-REIN-CO NOT EQUAL LOW-VALUES AND SPACES AND ZEROS        CL**1
02210         MOVE '(REINSURANCE)'     TO HDB-REIN                         CL**1
02211      ELSE                                                            CL**1
02212         MOVE SPACES              TO HDB-REIN.                        CL**1
02213                                                                      CL**1
02214      MOVE HEAD-A TO PRT  MOVE '1' TO X  PERFORM 0620-PRXX THRU       CL**1
02215              0630-PR-XX.                                             CL**1
02216      MOVE HEAD-AA TO PRT  MOVE ' ' TO X  PERFORM 0620-PRXX THRU      CL**1
02217              0630-PR-XX.                                             CL**1
02218      MOVE HEAD-B TO PRT  MOVE ' ' TO X  PERFORM 0620-PRXX THRU       CL**1
02219              0630-PR-XX.                                             CL**1
02220      MOVE HEAD-C TO PRT  MOVE ' ' TO X  PERFORM 0620-PRXX THRU       CL**1
02221              0630-PR-XX.                                             CL**1
02222      MOVE HEAD-CC TO PRT MOVE ' ' TO X  PERFORM 0620-PRXX THRU       CL**1
02223              0630-PR-XX.                                             CL**1
02224                                                                      CL**1
02225      IF HEAD-D-REIN-CO NOT EQUAL SPACES AND LOW-VALUES               CL**1
02226         MOVE HEAD-D-REIN         TO PRT                              CL**1
02227         MOVE ' '                 TO X                                CL**1
02228         PERFORM 0620-PRXX THRU 0630-PR-XX                            CL**1
02229      ELSE                                                            CL**1
02230         MOVE BLANK-LINE          TO PRT                              CL**1
02231         MOVE ' '                 TO X                                CL**1
02232         PERFORM 0620-PRXX THRU 0630-PR-XX.                           CL**1
02233                                                                      CL**1
02234      IF HEADE-CARR NOT EQUAL SPACES AND LOW-VALUES                   CL**1
02235         MOVE HEAD-E-CARR         TO PRT                              CL**1
02236         MOVE ' '                 TO X                                CL**1
02237         PERFORM 0620-PRXX THRU 0630-PR-XX                            CL**1
02238      ELSE                                                            CL**1
02239         MOVE BLANK-LINE          TO PRT                              CL**1
02240         MOVE ' '                 TO X                                CL**1
02241         PERFORM 0620-PRXX THRU 0630-PR-XX.                           CL**1
02242                                                                      CL**1
02243      IF HEADF-COMP NOT EQUAL SPACES AND LOW-VALUES                   CL**1
02244         MOVE HEAD-F-COMP         TO PRT                              CL**1
02245         MOVE ' '                 TO X                                CL**1
02246         PERFORM 0620-PRXX THRU 0630-PR-XX                            CL**1
02247         ELSE                                                         CL**1
02248         IF HEADFF-VAR NOT EQUAL SPACES AND LOW-VALUES                CL**1
02249             MOVE HEAD-FF-VAR         TO PRT                          CL**1
02250             MOVE ' '                 TO X                            CL**1
02251             PERFORM 0620-PRXX THRU 0630-PR-XX                        CL**1
02252         ELSE                                                         CL**1
02253             MOVE BLANK-LINE          TO PRT                          CL**1
02254             MOVE ' '                 TO X                            CL**1
02255             PERFORM 0620-PRXX THRU 0630-PR-XX.                       CL**1
02256                                                                      CL**1
02257      IF HEADG-STATE NOT EQUAL SPACES AND LOW-VALUES                  CL**1
02258         MOVE HEAD-G-STATE        TO PRT                              CL**1
02259         MOVE ' '                 TO X                                CL**1
02260         PERFORM 0620-PRXX THRU 0630-PR-XX                            CL**1
02261      ELSE                                                            CL**1
02262         MOVE BLANK-LINE          TO PRT                              CL**1
02263         MOVE ' '                 TO X                                CL**1
02264         PERFORM 0620-PRXX THRU 0630-PR-XX.                           CL**1
02265                                                                      CL**1
02266      IF HEADGG-VAR NOT EQUAL SPACES AND LOW-VALUES                   CL**1
02267         MOVE HEAD-GG-VAR         TO PRT                              CL**1
02268         MOVE ' '                 TO X                                CL**1
02269         PERFORM 0620-PRXX THRU 0630-PR-XX                            CL**1
02270      ELSE                                                            CL**1
02271         MOVE BLANK-LINE          TO PRT                              CL**1
02272         MOVE ' '                 TO X                                CL**1
02273         PERFORM 0620-PRXX THRU 0630-PR-XX.                           CL**1
02274                                                                      CL**1
02275      IF HEAD-HH-VAR NOT EQUAL SPACES AND LOW-VALUES                  CL**1
02276         MOVE HEAD-HH-VAR         TO PRT                              CL**1
02277         MOVE ' '                 TO X                                CL**1
02278         PERFORM 0620-PRXX THRU 0630-PR-XX                            CL**1
02279      ELSE                                                            CL**1
02280         MOVE BLANK-LINE          TO PRT                              CL**1
02281         MOVE ' '                 TO X                                CL**1
02282         PERFORM 0620-PRXX THRU 0630-PR-XX.                           CL**1
02283                                                                      CL**1
02284  0540-E-PRINT-HEAD.                                                  CL**1
02285      EXIT.                                                           CL**1
02286                                                                      CL**1
02287      EJECT                                                           CL**1
02288  0550-PRINT-5.                                                       CL**1
02289                                                                      CL**1
02290      IF SINGLE-PERIOD-PROCESS                                        CL**1
02291         IF (X-MEANS (1) EQUAL ZEROS)      AND                        CL**1
02292            (X-PREM (1) EQUAL ZEROS)       AND                        CL**1
02293            (X-PRESA (1) EQUAL ZEROS)      AND                        CL**1
02294            (X-CREF (1) EQUAL ZEROS)       AND                        CL**1
02295            (X-PRESB (1) EQUAL ZEROS)      AND                        CL**1
02296            (X-RST-PRM (1) EQUAL ZEROS)    AND                        CL**1
02297            (X-CLAIM (1) EQUAL ZEROS)      AND                        CL**1
02298            (X-B-IBNR (1) EQUAL ZEROS)     AND                        CL**1
02299            (X-B-LOSS (1) EQUAL ZEROS)     AND                        CL**1
02300            (X-E-IBNR (1) EQUAL ZEROS)     AND                        CL**1
02301            (X-E-LOSS (1) EQUAL ZEROS)     AND                        CL**1
02302            (X-RETRO-PMTS (1) EQUAL ZEROS) AND                        CL**1
02303            (X-OVR-COMM (1) EQUAL ZEROS)   AND                        CL**1
02304            (X-AGT-COMM (1) EQUAL ZEROS)   AND                        CL**1
02305            (X-CLM-CNT (1) EQUAL ZEROS)    AND                        CL**1
02306            (X-PD-CNT (1) EQUAL ZEROS)    AND                         CL**1
02307            (X-CNT (1) EQUAL ZEROS)                                   CL**1
02308              IF CHECK-PRT-SET EQUAL 'Y'                              CL**1
02309                  SUBTRACT +1 FROM PRT-SET                            CL**1
02310                  MOVE 'N' TO CHECK-PRT-SET                           CL**1
02311                  GO TO 0580-E-PRINT-5                                CL**1
02312              ELSE                                                    CL**1
02313                  GO TO 0580-E-PRINT-5.                               CL**1
02314                                                                      CL**1
02315      MOVE 'N' TO CHECK-PRT-SET.                                      CL**1
02316                                                                      CL**1
02317      MOVE HEAD-HH TO PRT                                             CL**1
02318      MOVE '0' TO X                                                   CL**1
02319      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02320                                                                      CL**1
02321      MOVE HEAD-H TO PRT                                              CL**1
02322      MOVE ' ' TO X                                                   CL**1
02323      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02324                                                                      CL**1
02325      PERFORM 0940-PRINT-MEAN THRU 0950-MEAN-EXIT.                    CL**1
02326      MOVE HI-1 TO HI-A.                                              CL**1
02327                                                                      CL**1
02328  0560-SET-PREMS.                                                     CL**1
02329      MOVE X-PREM (3) TO HI-B.                                        CL**1
02330      MOVE X-PREM (2) TO HI-C.                                        CL**1
02331      MOVE X-PREM (1) TO HI-D.                                        CL**1
02332                                                                      CL**1
02333      MOVE ZEROS TO X-PREM (4).                                       CL**1
02334      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02335          COMPUTE X-PREM (4) EQUAL                                    CL**1
02336             (X-PREM (1) + X-PREM (2) + X-PREM (3)).                  CL**1
02337                                                                      CL**1
02338      MOVE X-PREM (4) TO HI-E.                                        CL**1
02339      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02340      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02341                                                                      CL**1
02342  0570-E-SET-PREM.                                                    CL**1
02343                                                                      CL**1
02344      MOVE HI-4 TO HI-A.                                              CL**1
02345      MOVE X-PRESA (3) TO HI-B.                                       CL**1
02346      MOVE X-PRESA (2) TO HI-C.                                       CL**1
02347      MOVE X-PRESA (1) TO HI-D.                                       CL**1
02348      MOVE ZEROS TO X-PRESA (4).                                      CL**1
02349      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02350          COMPUTE X-PRESA (4) EQUAL                                   CL**1
02351             (X-PRESA (1) + X-PRESA (2) + X-PRESA (3)).               CL**1
02352                                                                      CL**1
02353      MOVE X-PRESA (4) TO HI-E.                                       CL**1
02354      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02355      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02356                                                                      CL**1
02357      MOVE HI-2 TO HI-A.                                              CL**1
02358      MOVE X-CREF (3) TO HI-B.                                        CL**1
02359      MOVE X-CREF (2) TO HI-C.                                        CL**1
02360      MOVE X-CREF (1) TO HI-D.                                        CL**1
02361      MOVE ZEROS TO X-CREF (4).                                       CL**1
02362      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02363          COMPUTE X-CREF (4) EQUAL                                    CL**1
02364             (X-CREF (1) + X-CREF (2) + X-CREF (3)).                  CL**1
02365                                                                      CL**1
02366      MOVE X-CREF (4) TO HI-E.                                        CL**1
02367      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02368      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02369                                                                      CL**1
02370      MOVE HI-5 TO HI-A.                                              CL**1
02371      MOVE X-PRESB (3) TO HI-B.                                       CL**1
02372      MOVE X-PRESB (2) TO HI-C.                                       CL**1
02373      MOVE X-PRESB (1) TO HI-D.                                       CL**1
02374      MOVE ZEROS TO X-PRESB (4).                                      CL**1
02375      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02376          COMPUTE X-PRESB (4) EQUAL                                   CL**1
02377             (X-PRESB (1) + X-PRESB (2) + X-PRESB (3)).               CL**1
02378                                                                      CL**1
02379      MOVE X-PRESB (4) TO HI-E.                                       CL**1
02380      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02381      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02382                                                                      CL**1
02383      MOVE HI-6 TO HI-A.                                              CL**1
02384      ADD X-PRESA (1) TO X-PREM (1).                                  CL**1
02385      ADD X-PRESA (2) TO X-PREM (2).                                  CL**1
02386      ADD X-PRESA (3) TO X-PREM (3).                                  CL**1
02387      SUBTRACT X-PRESB (1) FROM X-PREM (1).                           CL**1
02388      SUBTRACT X-PRESB (2) FROM X-PREM (2).                           CL**1
02389      SUBTRACT X-PRESB (3) FROM X-PREM (3).                           CL**1
02390      SUBTRACT X-CREF (1) FROM X-PREM (1).                            CL**1
02391      SUBTRACT X-CREF (2) FROM X-PREM (2).                            CL**1
02392      SUBTRACT X-CREF (3) FROM X-PREM (3).                            CL**1
02393      PERFORM 0560-SET-PREMS.                                         CL**1
02394                                                                      CL**1
02395      IF X-RST-PRM (1) EQUAL +0                                       CL**1
02396         MOVE X-PREM (1)          TO X-RST-PRM (1).                   CL**1
02397      IF X-RST-PRM (2) EQUAL +0                                       CL**1
02398         MOVE X-PREM (2)          TO X-RST-PRM (2).                   CL**1
02399      IF X-RST-PRM (3) EQUAL +0                                       CL**1
02400         MOVE X-PREM (3)          TO X-RST-PRM (3).                   CL**1
02401                                                                      CL**1
02402      MOVE HI-7 TO HI-A.                                              CL**1
02403      MOVE X-CLAIM (1) TO HI-D.                                       CL**1
02404      MOVE ZEROS TO X-CLAIM (4).                                      CL**1
02405      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02406           MOVE X-CLAIM (3) TO HI-B                                   CL**1
02407           MOVE X-CLAIM (2) TO HI-C                                   CL**1
02408          COMPUTE X-CLAIM (4) EQUAL                                   CL**1
02409             (X-CLAIM (1) + X-CLAIM (2) + X-CLAIM (3)).               CL**1
02410                                                                      CL**1
02411      MOVE X-CLAIM (4) TO HI-E.                                       CL**1
02412      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02413      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02414                                                                      CL**1
02415      MOVE HI-11      TO HI-A.                                        CL**1
02416      ADD X-B-IBNR (1) TO X-B-LOSS (1).                               CL**1
02417      ADD X-B-IBNR (2) TO X-B-LOSS (2).                               CL**1
02418      ADD X-B-IBNR (3) TO X-B-LOSS (3).                               CL**1
02419                                                                      CL**1
02420      MOVE X-B-LOSS (1) TO HI-D.                                      CL**1
02421      MOVE ZEROS TO X-B-LOSS (4).                                     CL**1
02422      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02423          MOVE X-B-LOSS (3) TO HI-B                                   CL**1
02424          MOVE X-B-LOSS (2) TO HI-C                                   CL**1
02425          COMPUTE X-B-LOSS (4) EQUAL                                  CL**1
02426             (X-B-LOSS (1) + X-B-LOSS (2) + X-B-LOSS (3)).            CL**1
02427                                                                      CL**1
02428      MOVE X-B-LOSS (4) TO HI-E.                                      CL**1
02429      MOVE HEAD-I TO PRT  MOVE ' ' TO X                               CL**1
02430      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02431                                                                      CL**1
02432      MOVE HI-13      TO HI-A.                                        CL**1
02433      ADD X-E-IBNR (1) TO X-E-LOSS (1).                               CL**1
02434      ADD X-E-IBNR (2) TO X-E-LOSS (2).                               CL**1
02435      ADD X-E-IBNR (3) TO X-E-LOSS (3).                               CL**1
02436                                                                      CL**1
02437      MOVE X-E-LOSS (1) TO HI-D.                                      CL**1
02438      MOVE ZEROS TO X-E-LOSS (4).                                     CL**1
02439      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02440          MOVE X-E-LOSS (2) TO HI-C                                   CL**1
02441          MOVE X-E-LOSS (3) TO HI-B                                   CL**1
02442          COMPUTE X-E-LOSS (4) EQUAL                                  CL**1
02443             (X-E-LOSS (1) + X-E-LOSS (2) + X-E-LOSS (3)).            CL**1
02444                                                                      CL**1
02445      MOVE X-E-LOSS (4) TO HI-E.                                      CL**1
02446      MOVE HEAD-I TO PRT  MOVE ' ' TO X                               CL**1
02447      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02448                                                                      CL**1
02449                                                                      CL**1
02450      MOVE HI-15      TO HI-A.                                        CL**1
02451      ADD X-E-LOSS (1) TO X-CLAIM (1).                                CL**1
02452      ADD X-E-LOSS (2) TO X-CLAIM (2).                                CL**1
02453      ADD X-E-LOSS (3) TO X-CLAIM (3).                                CL**1
02454      ADD X-E-LOSS (4) TO X-CLAIM (4).                                CL**1
02455      SUBTRACT X-B-LOSS (1) FROM X-CLAIM (1).                         CL**1
02456      SUBTRACT X-B-LOSS (2) FROM X-CLAIM (2).                         CL**1
02457      SUBTRACT X-B-LOSS (3) FROM X-CLAIM (3).                         CL**1
02458      SUBTRACT X-B-LOSS (4) FROM X-CLAIM (4).                         CL**1
02459      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02460          MOVE X-CLAIM (2) TO HI-C                                    CL**1
02461          MOVE X-CLAIM (3) TO HI-B.                                   CL**1
02462      MOVE X-CLAIM (4) TO HI-E.                                       CL**1
02463      MOVE X-CLAIM (1) TO HI-D.                                       CL**1
02464      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02465      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02466                                                                      CL**1
02467      MOVE HI-17 TO HI-A.                                             CL**1
02468      MOVE X-RETRO-PMTS (3) TO HI-B.                                  CL**1
02469      MOVE X-RETRO-PMTS (2) TO HI-C.                                  CL**1
02470      MOVE X-RETRO-PMTS (1) TO HI-D.                                  CL**1
02471      MOVE ZEROS TO X-RETRO-PMTS (4).                                 CL**1
02472      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02473          COMPUTE X-RETRO-PMTS (4) EQUAL                              CL**1
02474         (X-RETRO-PMTS (1) + X-RETRO-PMTS (2) + X-RETRO-PMTS (3)).    CL**1
02475                                                                      CL**1
02476      MOVE X-RETRO-PMTS (4) TO HI-E.                                  CL**1
02477      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02478      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02479                                                                      CL**1
02480      MOVE HI-18      TO HI-A.                                        CL**1
02481      MOVE X-RST-PRM (3) TO HI-B.                                     CL**1
02482      MOVE X-RST-PRM (2) TO HI-C.                                     CL**1
02483      MOVE X-RST-PRM (1) TO HI-D.                                     CL**1
02484      MOVE ZEROS TO X-RST-PRM (4).                                    CL**1
02485      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02486          COMPUTE X-RST-PRM (4) EQUAL                                 CL**1
02487             (X-RST-PRM (1) + X-RST-PRM (2) + X-RST-PRM (3)).         CL**1
02488                                                                      CL**1
02489      MOVE X-RST-PRM (4) TO HI-E.                                     CL**1
02490      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02491      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02492                                                                      CL**1
02493      MOVE HI-16  TO  HI-A.                                           CL**1
02494      IF X-PREM (1) NOT = ZERO                                        CL**1
02495         COMPUTE LR-CALC-PCT (1) ROUNDED EQUAL                        CL**1
02496                                       (X-CLAIM (1) / X-PREM (1))     CL**1
02497        ELSE                                                          CL**1
02498            MOVE ZERO TO LR-CALC-PCT (1).                             CL**1
02499      IF X-PREM (2) NOT = ZERO                                        CL**1
02500         COMPUTE LR-CALC-PCT (2) ROUNDED EQUAL                        CL**1
02501                                       (X-CLAIM (2) / X-PREM (2))     CL**1
02502        ELSE                                                          CL**1
02503            MOVE ZERO TO LR-CALC-PCT (2).                             CL**1
02504      IF X-PREM (3) NOT = ZERO                                        CL**1
02505         COMPUTE LR-CALC-PCT (3) ROUNDED EQUAL                        CL**1
02506                                       (X-CLAIM (3) / X-PREM (3))     CL**1
02507        ELSE                                                          CL**1
02508            MOVE ZERO TO LR-CALC-PCT (3).                             CL**1
02509      IF X-PREM (4) NOT = ZERO                                        CL**1
02510         COMPUTE LR-CALC-PCT (4) ROUNDED EQUAL                        CL**1
02511                                       (X-CLAIM (4) / X-PREM (4))     CL**1
02512        ELSE                                                          CL**1
02513            MOVE ZERO TO LR-CALC-PCT (4).                             CL**1
02514                                                                      CL**1
02515      MULTIPLY LR-CALC-PCT (1) BY +100 GIVING X-PREM (1).             CL**1
02516      MULTIPLY LR-CALC-PCT (2) BY +100 GIVING X-PREM (2).             CL**1
02517      MULTIPLY LR-CALC-PCT (3) BY +100 GIVING X-PREM (3).             CL**1
02518      MULTIPLY LR-CALC-PCT (4) BY +100 GIVING X-PREM (4).             CL**1
02519                                                                      CL**1
02520      MOVE ' ' TO AST1, AST2, AST3, AST4.                             CL**1
02521                                                                      CL**1
02522      IF X-PREM (1) LESS THAN +.01                                    CL**1
02523         MOVE +0 TO X-PREM (1).                                       CL**1
02524      IF X-PREM (2) LESS THAN +.01                                    CL**1
02525         MOVE +0 TO X-PREM (2).                                       CL**1
02526      IF X-PREM (3) LESS THAN +.01                                    CL**1
02527         MOVE +0 TO X-PREM (3).                                       CL**1
02528      IF X-PREM (4) LESS THAN +.01                                    CL**1
02529         MOVE +0 TO X-PREM (4).                                       CL**1
02530                                                                      CL**1
02531      MOVE X-PREM (3) TO HI-B.                                        CL**1
02532      MOVE X-PREM (2) TO HI-C.                                        CL**1
02533      MOVE X-PREM (1) TO HI-D.                                        CL**1
02534      MOVE X-PREM (4) TO HI-E.                                        CL**1
02535      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02536      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02537                                                                      CL**1
02538      MOVE HI-14  TO  HI-A.                                           CL**1
02539      IF X-RST-PRM (1) NOT = ZERO                                     CL**1
02540         COMPUTE LR-CALC-PCT (1) ROUNDED EQUAL                        CL**1
02541                                    (X-CLAIM (1) / X-RST-PRM (1))     CL**1
02542        ELSE                                                          CL**1
02543            MOVE ZERO TO LR-CALC-PCT (1).                             CL**1
02544      IF X-RST-PRM (2) NOT = ZERO                                     CL**1
02545         COMPUTE LR-CALC-PCT (2) ROUNDED EQUAL                        CL**1
02546                                    (X-CLAIM (2) / X-RST-PRM (2))     CL**1
02547        ELSE                                                          CL**1
02548            MOVE ZERO TO LR-CALC-PCT (2).                             CL**1
02549      IF X-RST-PRM (3) NOT = ZERO                                     CL**1
02550         COMPUTE LR-CALC-PCT (3) ROUNDED EQUAL                        CL**1
02551                                    (X-CLAIM (3) / X-RST-PRM (3))     CL**1
02552        ELSE                                                          CL**1
02553            MOVE ZERO TO LR-CALC-PCT (3).                             CL**1
02554      IF X-RST-PRM (4) NOT = ZERO                                     CL**1
02555         COMPUTE LR-CALC-PCT (4) ROUNDED EQUAL                        CL**1
02556                                    (X-CLAIM (4) / X-RST-PRM (4))     CL**1
02557        ELSE                                                          CL**1
02558            MOVE ZERO TO LR-CALC-PCT (4).                             CL**1
02559                                                                      CL**1
02560      MULTIPLY LR-CALC-PCT (1) BY +100 GIVING X-RST-PRM (1).          CL**1
02561      MULTIPLY LR-CALC-PCT (2) BY +100 GIVING X-RST-PRM (2).          CL**1
02562      MULTIPLY LR-CALC-PCT (3) BY +100 GIVING X-RST-PRM (3).          CL**1
02563      MULTIPLY LR-CALC-PCT (4) BY +100 GIVING X-RST-PRM (4).          CL**1
02564                                                                      CL**1
02565      MOVE ' ' TO AST1, AST2, AST3, AST4.                             CL**1
02566                                                                      CL**1
02567      IF X-RST-PRM (1) LESS THAN +.01                                 CL**1
02568         MOVE +0 TO X-RST-PRM (1).                                    CL**1
02569      IF X-RST-PRM (2) LESS THAN +.01                                 CL**1
02570         MOVE +0 TO X-RST-PRM (2).                                    CL**1
02571      IF X-RST-PRM (3) LESS THAN +.01                                 CL**1
02572         MOVE +0 TO X-RST-PRM (3).                                    CL**1
02573      IF X-RST-PRM (4) LESS THAN +.01                                 CL**1
02574         MOVE +0 TO X-RST-PRM (4).                                    CL**1
02575                                                                      CL**1
02576      MOVE X-RST-PRM (3) TO HI-B.                                     CL**1
02577      MOVE X-RST-PRM (2) TO HI-C.                                     CL**1
02578      MOVE X-RST-PRM (1) TO HI-D.                                     CL**1
02579      MOVE X-RST-PRM (4) TO HI-E.                                     CL**1
02580      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02581      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02582                                                                      CL**1
02583      MOVE HI-19 TO HI-A.                                             CL**1
02584      MOVE X-CLM-CNT (3) TO HI-BB.                                    CL**1
02585      MOVE X-CLM-CNT (2) TO HI-CC.                                    CL**1
02586      MOVE X-CLM-CNT (1) TO HI-DD.                                    CL**1
02587      MOVE ZEROS TO X-CLM-CNT (4).                                    CL**1
02588      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02589          COMPUTE X-CLM-CNT (4) EQUAL                                 CL**1
02590             (X-CLM-CNT (1) + X-CLM-CNT (2) + X-CLM-CNT (3)).         CL**1
02591                                                                      CL**1
02592      MOVE X-CLM-CNT (4) TO HI-EE.                                    CL**1
02593      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02594      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02595                                                                      CL**1
02596      MOVE HI-20 TO HI-A.                                             CL**1
02597      MOVE X-PD-CNT (3) TO HI-BB.                                     CL**1
02598      MOVE X-PD-CNT (2) TO HI-CC.                                     CL**1
02599      MOVE X-PD-CNT (1) TO HI-DD.                                     CL**1
02600      MOVE ZEROS TO X-PD-CNT (4).                                     CL**1
02601      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02602          COMPUTE X-PD-CNT (4) EQUAL                                  CL**1
02603             (X-PD-CNT (1) + X-PD-CNT (2) + X-PD-CNT (3)).            CL**1
02604                                                                      CL**1
02605      MOVE X-PD-CNT (4) TO HI-EE.                                     CL**1
02606      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02607      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02608                                                                      CL**1
02609      MOVE HI-8 TO HI-A.                                              CL**1
02610      MOVE X-OVR-COMM (3) TO HI-B.                                    CL**1
02611      MOVE X-OVR-COMM (2) TO HI-C.                                    CL**1
02612      MOVE X-OVR-COMM (1) TO HI-D.                                    CL**1
02613      MOVE ZEROS TO X-OVR-COMM (4).                                   CL**1
02614      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02615          COMPUTE X-OVR-COMM (4) EQUAL                                CL**1
02616             (X-OVR-COMM (1) + X-OVR-COMM (2) + X-OVR-COMM (3)).      CL**1
02617                                                                      CL**1
02618      MOVE X-OVR-COMM (4) TO HI-E.                                    CL**1
02619      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02620      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02621                                                                      CL**1
02622      MOVE HI-8A TO HI-A.                                             CL**1
02623      MOVE X-AGT-COMM (3) TO HI-B.                                    CL**1
02624      MOVE X-AGT-COMM (2) TO HI-C.                                    CL**1
02625      MOVE X-AGT-COMM (1) TO HI-D.                                    CL**1
02626      MOVE ZEROS TO X-AGT-COMM (4).                                   CL**1
02627      IF NOT SINGLE-PERIOD-PROCESS                                    CL**1
02628          COMPUTE X-AGT-COMM (4) EQUAL                                CL**1
02629            (X-AGT-COMM (1) + X-AGT-COMM (2) + X-AGT-COMM (3)).       CL**1
02630                                                                      CL**1
02631      MOVE X-AGT-COMM (4) TO HI-E.                                    CL**1
02632      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02633      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02634                                                                      CL**1
02635      MOVE HI-10  TO  HI-A.                                           CL**1
02636      MOVE X-CNT (3)  TO  HI-BB.                                      CL**1
02637      MOVE X-CNT (2)  TO  HI-CC.                                      CL**1
02638      MOVE X-CNT (1)  TO  HI-DD.                                      CL**1
02639                                                                      CL**1
02640      IF SINGLE-PERIOD-PROCESS                                        CL**1
02641          MOVE ZEROS      TO  HI-EE                                   CL**1
02642      ELSE                                                            CL**1
02643          MOVE X-CNT (4)  TO  HI-EE.                                  CL**1
02644                                                                      CL**1
02645      MOVE ' ' TO AST1, AST2, AST3, AST4.                             CL**1
02646      MOVE HEAD-I TO PRT  MOVE ' ' TO X.                              CL**1
02647      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02648                                                                      CL**1
02649  0580-E-PRINT-5.                                                     CL**1
02650      EXIT.                                                           CL**1
02651                                                                      CL**1
02652      EJECT                                                           CL**1
02653  0620-PRXX.                                                          CL**1
02654                              COPY ELCPRT2.                           CL**1
02655  0630-PR-XX.                                                         CL**1
02656      EXIT.                                                           CL**1
02657                                                                      CL**1
02658  0650-CLOSE-FICH.                                                    CL**1
02659                              COPY ELCPRTC.                           CL**1
02660      MOVE ZEROS  TO RETURN-CODE.
02660      GOBACK.                                                         CL**1
02661                                                                      CL**1
02662  0660-PRT-RTN.                                                       CL**1
02663      PERFORM 0620-PRXX THRU 0630-PR-XX.                              CL**1
02664  0670-E-PRT-RTN.                                                     CL**1
02665      EXIT.                                                           CL**1
02666                                                                      CL**1
02667      EJECT                                                           CL**1
02668  0700-FIND-BUSC-TYPE.                                                CL**1
02669                                                                      CL**1
02670      MOVE CLAS-STARTB            TO CLAS-INDEXB.                     CL**1
02671      MOVE LAS-ACCT-TYPE          TO W-BUS-TYPE                       CL**1
02672                                     TEMP-BUS-TYPE.                   CL**1
02673      MOVE 'UNKNOWN '             TO TEMP-BUS-DESC.                   CL**1
02674                                                                      CL**1
02675      IF R-BUS-TYPE NOT NUMERIC                                       CL**1
02676         MOVE ZEROS TO R-BUS-TYPE.                                    CL**1
02677                                                                      CL**1
02678  0700-FIND-BUSC-LOOP.                                                CL**1
02679                                                                      CL**1
02680      IF CLAS-INDEXB GREATER THAN CLAS-MAXB                           CL**1
02681          GO TO 0750-EXIT.                                            CL**1
02682                                                                      CL**1
02683      IF CLAS-BUSC-CODE (CLAS-INDEXB) NOT EQUAL R-BUS-TYPE            CL**1
02684          ADD +1 TO CLAS-INDEXB                                       CL**1
02685          GO TO 0700-FIND-BUSC-LOOP.                                  CL**1
02686                                                                      CL**1
02687      MOVE CLAS-BUSC-DESC (CLAS-INDEXB) TO TEMP-BUS-DESC.             CL**1
02688                                                                      CL**1
02689  0750-EXIT.                                                          CL**1
02690      EXIT.                                                           CL**1
02691                                                                      CL**1
02692      EJECT                                                           CL**1
02693  0790-ADD-ACROSS-RTN.                                                CL**1
02694      MOVE AH-A-TYPE (CA YR CE)   TO CA-ADDS.                         CL**1
02695      MOVE AH-A-TYPE (CB YR CE)   TO CB-ADDS.                         CL**1
02696      ADD CA-PREM                 TO CB-PREM.                         CL**1
02697      ADD CA-RST-PRM              TO CB-RST-PRM.                      CL**1
02698      ADD CA-REF                  TO CB-REF.                          CL**1
02699      ADD CA-RES-A                TO CB-RES-A.                        CL**1
02700      ADD CA-RES-B                TO CB-RES-B.                        CL**1
02701      ADD CA-CLAIM                TO CB-CLAIM.                        CL**1
02702      ADD CA-OVR-COMM             TO CB-OVR-COMM.                     CL**1
02703      ADD CA-AGT-COMM             TO CB-AGT-COMM.                     CL**1
02704      ADD CA-B-IBNR               TO CB-B-IBNR.                       CL**1
02705      ADD CA-E-IBNR               TO CB-E-IBNR.                       CL**1
02706      ADD CA-B-LOSS               TO CB-B-LOSS.                       CL**1
02707      ADD CA-E-LOSS               TO CB-E-LOSS.                       CL**1
02708      ADD CA-RETRO-PMTS           TO CB-RETRO-PMTS.                   CL**1
02709      ADD CA-COUNT                TO CB-COUNT.                        CL**1
02710      ADD CA-CLM-CNT              TO CB-CLM-CNT.                      CL**1
02711      ADD CA-PD-CNT               TO CB-PD-CNT.                       CL**1
02712                                                                      CL**1
02713      IF CB-PRT-IND EQUAL ZERO                                        CL**1
02714          MOVE CA-PRT-IND         TO CB-PRT-IND.                      CL**1
02715                                                                      CL**1
02716      MOVE CA-ADDS                TO AH-A-TYPE (CA YR CE).            CL**1
02717      MOVE CB-ADDS                TO AH-A-TYPE (CB YR CE).            CL**1
02718                                                                      CL**1
02719  0800-ADD-ACROSS-EXIT.                                               CL**1
02720      EXIT.                                                           CL**1
02721                                                                      CL**1
02722  0805-ADD-ACROSS-RTN.                                                CL**1
02723      MOVE LF-A-TYPE (CA YR CE)   TO CA-ADDS.                         CL**1
02724      MOVE LF-A-TYPE (CB YR CE)   TO CB-ADDS.                         CL**1
02725      ADD CA-PREM                 TO CB-PREM.                         CL**1
02726      ADD CA-RST-PRM              TO CB-RST-PRM.                      CL**1
02727      ADD CA-REF                  TO CB-REF.                          CL**1
02728      ADD CA-RES-A                TO CB-RES-A.                        CL**1
02729      ADD CA-RES-B                TO CB-RES-B.                        CL**1
02730      ADD CA-CLAIM                TO CB-CLAIM.                        CL**1
02731      ADD CA-OVR-COMM             TO CB-OVR-COMM.                     CL**1
02732      ADD CA-AGT-COMM             TO CB-AGT-COMM.                     CL**1
02733      ADD CA-B-IBNR               TO CB-B-IBNR.                       CL**1
02734      ADD CA-E-IBNR               TO CB-E-IBNR.                       CL**1
02735      ADD CA-B-LOSS               TO CB-B-LOSS.                       CL**1
02736      ADD CA-E-LOSS               TO CB-E-LOSS.                       CL**1
02737      ADD CA-RETRO-PMTS           TO CB-RETRO-PMTS.                   CL**1
02738      ADD CA-COUNT                TO CB-COUNT.                        CL**1
02739      ADD CA-CLM-CNT              TO CB-CLM-CNT.                      CL**1
02740      ADD CA-PD-CNT               TO CB-PD-CNT.                       CL**1
02741                                                                      CL**1
02742      IF CB-PRT-IND EQUAL ZERO                                        CL**1
02743          MOVE CA-PRT-IND         TO CB-PRT-IND.                      CL**1
02744                                                                      CL**1
02745      MOVE CA-ADDS                TO LF-A-TYPE (CA YR CE).            CL**1
02746      MOVE CB-ADDS                TO LF-A-TYPE (CB YR CE).            CL**1
02747                                                                      CL**1
02748      ADD MEANS (CA YR CE)        TO MEANS (CB YR CE).                CL**1
02749                                                                      CL**1
02750  0809-ADD-ACROSS-EXIT.                                               CL**1
02751      EXIT.                                                           CL**1
02752      EJECT                                                           CL**1
02753  0810-FILL-5L.                                                       CL**1
02754                                                                      CL**1
02755      IF LF-A-PRT-IND (CA YR CG) EQUAL ZERO                           CL**1
02756         GO TO 0820-F-5L-BUMP.                                        CL**1
02757                                                                      CL**1
02758      MOVE LF-A-TYPE (CA YR CG)   TO X-TYPE (YR).                     CL**1
02759      MOVE MEANS (CA YR CG)       TO X-MEANS (YR).                    CL**1
02760      MOVE SPACES                 TO HH-BENEFIT.                      CL**1
02761      MOVE CLAS-I-AB10 (CG)       TO HH-BENEFIT.                      CL**1
02762                                                                      CL**1
02763      MOVE +0 TO LF-CLM-FACTOR.                                       CL**1
02764                                                                      CL**1
02765      IF LAS-RECORD-TYPE EQUAL 'D' OR 'E'                             CL**1
02766         IF WS-TOTAL-LF-PMTS (TX YR CG) NOT EQUAL +0                  CL**1
02767            COMPUTE LF-CLM-FACTOR ROUNDED EQUAL                       CL**1
02768                 (X-CLAIM (YR) / WS-TOTAL-LF-PMTS (TX YR CG)).        CL**1
02769                                                                      CL**1
02770      IF (LAS-RECORD-TYPE EQUAL 'D' OR 'E')                           CL**1
02771        AND                                                           CL**1
02772        (CA LESS THAN +3)                                             CL**1
02773         COMPUTE X-E-LOSS (YR) ROUNDED EQUAL                          CL**1
02774                 (WS-TOTAL-E-LF-RESV (TX YR CG) * LF-CLM-FACTOR)      CL**1
02775         IF YR EQUAL +3                                               CL**1
02776             COMPUTE X-B-LOSS (YR) ROUNDED EQUAL                      CL**1
02777                 (WS-TOTAL-B-LF-RESV (TX YR CG) * LF-CLM-FACTOR)      CL**1
02778             MOVE X-E-LOSS (YR) TO X-B-LOSS (2)                       CL**1
02779         ELSE                                                         CL**1
02780             IF YR EQUAL +2                                           CL**1
02781                 MOVE X-E-LOSS (YR) TO X-B-LOSS (1).                  CL**1
02782                                                                      CL**1
02783      IF CLAS-I-JOINT (CG) EQUAL 'J'                                  CL**1
02784          MOVE +2 TO SJ                                               CL**1
02785      ELSE                                                            CL**1
02786          MOVE +1 TO SJ.                                              CL**1
02787                                                                      CL**1
02788      IF CLAS-I-BAL (CG) EQUAL 'B'                                    CL**1
02789          MOVE +2 TO SO                                               CL**1
02790      ELSE                                                            CL**1
02791          MOVE +1 TO SO.                                              CL**1
02792                                                                      CL**1
02793      ADD X-PREM       (YR)       TO SJ-X-PREM       (SJ SO YR)       CL**1
02794                                     SJ-X-PREM       (SJ 3 YR).       CL**1
02795      ADD X-RST-PRM    (YR)       TO SJ-X-RST-PRM    (SJ SO YR)       CL**1
02796                                     SJ-X-RST-PRM    (SJ 3 YR).       CL**1
02797      ADD X-CREF       (YR)       TO SJ-X-CREF       (SJ SO YR)       CL**1
02798                                     SJ-X-CREF       (SJ 3 YR).       CL**1
02799      ADD X-PRESA      (YR)       TO SJ-X-PRESA      (SJ SO YR)       CL**1
02800                                     SJ-X-PRESA      (SJ 3 YR).       CL**1
02801      ADD X-PRESB      (YR)       TO SJ-X-PRESB      (SJ SO YR)       CL**1
02802                                     SJ-X-PRESB      (SJ 3 YR).       CL**1
02803      ADD X-CLAIM      (YR)       TO SJ-X-CLAIM      (SJ SO YR)       CL**1
02804                                     SJ-X-CLAIM      (SJ 3 YR).       CL**1
02805      ADD X-OVR-COMM   (YR)       TO SJ-X-OVR-COMM   (SJ SO YR)       CL**1
02806                                     SJ-X-OVR-COMM   (SJ 3 YR).       CL**1
02807      ADD X-AGT-COMM   (YR)       TO SJ-X-AGT-COMM   (SJ SO YR)       CL**1
02808                                     SJ-X-AGT-COMM   (SJ 3 YR).       CL**1
02809      ADD X-RETRO-PMTS (YR)       TO SJ-X-RETRO-PMTS (SJ SO YR)       CL**1
02810                                     SJ-X-RETRO-PMTS (SJ 3 YR).       CL**1
02811      ADD X-CNT        (YR)       TO SJ-X-CNT        (SJ SO YR)       CL**1
02812                                     SJ-X-CNT        (SJ 3 YR).       CL**1
02813      ADD X-CLM-CNT    (YR)       TO SJ-X-CLM-CNT    (SJ SO YR)       CL**1
02814                                     SJ-X-CLM-CNT    (SJ 3 YR).       CL**1
02815      ADD X-PD-CNT     (YR)       TO SJ-X-PD-CNT     (SJ SO YR)       CL**1
02816                                     SJ-X-PD-CNT     (SJ 3 YR).       CL**1
02817                                                                      CL**1
02818      IF SJ EQUAL +1                                                  CL**1
02819          ADD X-MEANS  (YR)       TO S-X-MEANS       (SO YR)          CL**1
02820                                     S-X-MEANS        (3 YR)          CL**1
02821      ELSE                                                            CL**1
02822          ADD X-MEANS  (YR)       TO J-X-MEANS       (SO YR)          CL**1
02823                                     J-X-MEANS        (3 YR).         CL**1
02824                                                                      CL**1
02825      ADD X-PREM       (YR)       TO LA-X-PREM       (1 YR).          CL**1
02826      ADD X-RST-PRM    (YR)       TO LA-X-RST-PRM    (1 YR).          CL**1
02827      ADD X-CREF       (YR)       TO LA-X-CREF       (1 YR).          CL**1
02828      ADD X-PRESA      (YR)       TO LA-X-PRESA      (1 YR).          CL**1
02829      ADD X-PRESB      (YR)       TO LA-X-PRESB      (1 YR).          CL**1
02830      ADD X-CLAIM      (YR)       TO LA-X-CLAIM      (1 YR).          CL**1
02831      ADD X-OVR-COMM   (YR)       TO LA-X-OVR-COMM   (1 YR).          CL**1
02832      ADD X-AGT-COMM   (YR)       TO LA-X-AGT-COMM   (1 YR).          CL**1
02833      ADD X-RETRO-PMTS (YR)       TO LA-X-RETRO-PMTS (1 YR).          CL**1
02834      ADD X-CNT        (YR)       TO LA-X-CNT        (1 YR).          CL**1
02835      ADD X-CLM-CNT    (YR)       TO LA-X-CLM-CNT    (1 YR).          CL**1
02836      ADD X-PD-CNT     (YR)       TO LA-X-PD-CNT     (1 YR).          CL**1
02837      ADD X-MEANS      (YR)       TO L-X-MEANS       (YR).            CL**1
02838                                                                      CL**1
02839      ADD X-E-LOSS (YR)           TO SJ-X-E-LOSS (SJ SO YR)           CL**1
02840                                     SJ-X-E-LOSS (SJ 3 YR).           CL**1
02841      ADD X-E-IBNR (YR)           TO SJ-X-E-IBNR (SJ SO YR)           CL**1
02842                                     SJ-X-E-IBNR (SJ 3 YR).           CL**1
02843                                                                      CL**1
02844      IF YR = +3                                                      CL**1
02845          ADD X-B-LOSS (YR)       TO SJ-X-B-LOSS (SJ SO 3)            CL**1
02846                                     SJ-X-B-LOSS (SJ 3 3)             CL**1
02847          ADD X-B-IBNR (YR)       TO SJ-X-B-IBNR (SJ SO 3)            CL**1
02848                                     SJ-X-B-IBNR (SJ 3 3)             CL**1
02849          ADD X-E-LOSS (YR)       TO SJ-X-B-LOSS (SJ SO 2)            CL**1
02850                                     SJ-X-B-LOSS (SJ 3 2)             CL**1
02851          ADD X-E-IBNR (YR)       TO SJ-X-B-IBNR (SJ SO 2)            CL**1
02852                                     SJ-X-B-IBNR (SJ 3 2)             CL**1
02853      ELSE                                                            CL**1
02854         IF YR = +2                                                   CL**1
02855             ADD X-E-LOSS (YR)    TO SJ-X-B-LOSS (SJ SO 1)            CL**1
02856                                     SJ-X-B-LOSS (SJ 3 1)             CL**1
02857             ADD X-E-IBNR (YR)    TO SJ-X-B-IBNR (SJ SO 1)            CL**1
02858                                     SJ-X-B-IBNR (SJ 3 1).            CL**1
02859                                                                      CL**1
02860      ADD X-E-LOSS (YR)           TO LA-X-E-LOSS (1 YR).              CL**1
02861      ADD X-E-IBNR (YR)           TO LA-X-E-IBNR (1 YR).              CL**1
02862                                                                      CL**1
02863      IF YR = +3                                                      CL**1
02864          ADD X-B-LOSS (YR)       TO LA-X-B-LOSS (1 3)                CL**1
02865          ADD X-B-IBNR (YR)       TO LA-X-B-IBNR (1 3)                CL**1
02866          ADD X-E-LOSS (YR)       TO LA-X-B-LOSS (1 2)                CL**1
02867          ADD X-E-IBNR (YR)       TO LA-X-B-IBNR (1 2)                CL**1
02868      ELSE                                                            CL**1
02869         IF YR = +2                                                   CL**1
02870             ADD X-E-LOSS (2)     TO LA-X-B-LOSS (1 1)                CL**1
02871             ADD X-E-IBNR (2)     TO LA-X-B-IBNR (1 1).               CL**1
02872                                                                      CL**1
02873      IF YR = +1  MOVE ' ' TO AST1.                                   CL**1
02874      IF YR = +2  MOVE ' ' TO AST2.                                   CL**1
02875      IF YR = +3 MOVE ' ' TO AST3.                                    CL**1
02876      MOVE ' ' TO SKIP-PRINT-SW.                                      CL**1
02877                                                                      CL**1
02878  0820-F-5L-BUMP.                                                     CL**1
02879      IF YR EQUAL +3                                                  CL**1
02880           GO TO 0830-E-FILL-5L.                                      CL**1
02881      ADD +1 TO YR.                                                   CL**1
02882      GO TO 0810-FILL-5L.                                             CL**1
02883  0830-E-FILL-5L.                                                     CL**1
02884      EXIT.                                                           CL**1
02885      EJECT                                                           CL**1
02886  0840-FILL-5A.                                                       CL**1
02887                                                                      CL**1
02888      IF AH-A-PRT-IND (CA YR CG) = ZEROS                              CL**1
02889         GO TO 0850-F-5A-BUMP.                                        CL**1
02890                                                                      CL**1
02891      MOVE AH-A-TYPE (CA YR CG) TO X-TYPE (YR).                       CL**1
02892      ADD CLAS-MAXL               TO CG.                              CL**1
02893      MOVE SPACES           TO HH-BENEFIT.                            CL**1
02894      MOVE CLAS-I-AB10 (CG) TO HH-BENEFIT.                            CL**1
02895                                                                      CL**1
02896      IF CLAS-I-BAL (CG) EQUAL  'B'                                   CL**1
02897          MOVE +2 TO SOA                                              CL**1
02898      ELSE                                                            CL**1
02899          MOVE +1 TO SOA.                                             CL**1
02900                                                                      CL**1
02901      SUBTRACT CLAS-MAXL          FROM CG.                            CL**1
02902                                                                      CL**1
02903      MOVE +0 TO AH-CLM-FACTOR.                                       CL**1
02904                                                                      CL**1
02905      IF LAS-RECORD-TYPE EQUAL 'D' OR 'E'                             CL**1
02906         IF WS-TOTAL-AH-PMTS (TX YR CG) NOT EQUAL +0                  CL**1
02907            COMPUTE AH-CLM-FACTOR ROUNDED EQUAL                       CL**1
02908                 (X-CLAIM (YR) / WS-TOTAL-AH-PMTS (TX YR CG)).        CL**1
02909                                                                      CL**1
02910      IF (LAS-RECORD-TYPE EQUAL 'D' OR 'E')                           CL**1
02911        AND                                                           CL**1
02912        (CA LESS THAN +3)                                             CL**1
02913          COMPUTE X-E-LOSS (YR) ROUNDED EQUAL                         CL**1
02914                 (WS-TOTAL-E-AH-RESV (TX YR CG) * AH-CLM-FACTOR)      CL**1
02915          IF YR EQUAL +3                                              CL**1
02916              COMPUTE X-B-LOSS (YR) ROUNDED EQUAL                     CL**1
02917                 (WS-TOTAL-B-AH-RESV (TX YR CG) * AH-CLM-FACTOR)      CL**1
02918              MOVE X-E-LOSS (YR) TO X-B-LOSS (2)                      CL**1
02919          ELSE                                                        CL**1
02920              IF YR EQUAL +2                                          CL**1
02921                  MOVE X-E-LOSS (YR) TO X-B-LOSS (1).                 CL**1
02922                                                                      CL**1
02923      ADD X-PREM       (YR)       TO LA-X-PREM       (2 YR).          CL**1
02924      ADD X-RST-PRM    (YR)       TO LA-X-RST-PRM    (2 YR).          CL**1
02925      ADD X-CREF       (YR)       TO LA-X-CREF       (2 YR).          CL**1
02926      ADD X-PRESA      (YR)       TO LA-X-PRESA      (2 YR).          CL**1
02927      ADD X-PRESB      (YR)       TO LA-X-PRESB      (2 YR).          CL**1
02928      ADD X-CLAIM      (YR)       TO LA-X-CLAIM      (2 YR).          CL**1
02929      ADD X-OVR-COMM   (YR)       TO LA-X-OVR-COMM   (2 YR).          CL**1
02930      ADD X-AGT-COMM   (YR)       TO LA-X-AGT-COMM   (2 YR).          CL**1
02931      ADD X-RETRO-PMTS (YR)       TO LA-X-RETRO-PMTS (2 YR).          CL**1
02932      ADD X-CNT        (YR)       TO LA-X-CNT        (2 YR).          CL**1
02933      ADD X-CLM-CNT    (YR)       TO LA-X-CLM-CNT    (2 YR).          CL**1
02934      ADD X-PD-CNT     (YR)       TO LA-X-PD-CNT     (2 YR).          CL**1
02935                                                                      CL**1
02936      ADD X-PREM       (YR)       TO SOA-X-PREM      (SOA YR).        CL**1
02937      ADD X-RST-PRM    (YR)       TO SOA-X-RST-PRM   (SOA YR).        CL**1
02938      ADD X-CREF       (YR)       TO SOA-X-CREF      (SOA YR).        CL**1
02939      ADD X-PRESA      (YR)       TO SOA-X-PRESA     (SOA YR).        CL**1
02940      ADD X-PRESB      (YR)       TO SOA-X-PRESB     (SOA YR).        CL**1
02941      ADD X-CLAIM      (YR)       TO SOA-X-CLAIM     (SOA YR).        CL**1
02942      ADD X-OVR-COMM   (YR)       TO SOA-X-OVR-COMM  (SOA YR).        CL**1
02943      ADD X-AGT-COMM   (YR)       TO SOA-X-AGT-COMM  (SOA YR).        CL**1
02944      ADD X-RETRO-PMTS (YR)       TO SOA-X-RETRO-PMTS (SOA YR).       CL**1
02945      ADD X-CNT        (YR)       TO SOA-X-CNT       (SOA YR).        CL**1
02946      ADD X-CLM-CNT    (YR)       TO SOA-X-CLM-CNT   (SOA YR).        CL**1
02947      ADD X-PD-CNT     (YR)       TO SOA-X-PD-CNT    (SOA YR).        CL**1
02948                                                                      CL**1
02949      ADD X-E-LOSS (YR)           TO LA-X-E-LOSS (2 YR).              CL**1
02950      ADD X-E-IBNR (YR)           TO LA-X-E-IBNR (2 YR).              CL**1
02951                                                                      CL**1
02952      ADD X-E-LOSS (YR)           TO SOA-X-E-LOSS (SOA YR).           CL**1
02953      ADD X-E-IBNR (YR)           TO SOA-X-E-IBNR (SOA YR).           CL**1
02954                                                                      CL**1
02955      IF YR = +3                                                      CL**1
02956          ADD X-B-LOSS (YR)       TO LA-X-B-LOSS (2 3)                CL**1
02957          ADD X-B-IBNR (YR)       TO LA-X-B-IBNR (2 3)                CL**1
02958          ADD X-E-LOSS (YR)       TO LA-X-B-LOSS (2 2)                CL**1
02959          ADD X-E-IBNR (YR)       TO LA-X-B-IBNR (2 2)                CL**1
02960      ELSE                                                            CL**1
02961         IF YR = +2                                                   CL**1
02962             ADD X-E-LOSS (YR)    TO LA-X-B-LOSS (2 1)                CL**1
02963             ADD X-E-IBNR (YR)    TO LA-X-B-IBNR (2 1).               CL**1
02964                                                                      CL**1
02965      IF YR = +3                                                      CL**1
02966          ADD X-B-LOSS (YR)       TO SOA-X-B-LOSS (SOA 3)             CL**1
02967          ADD X-B-IBNR (YR)       TO SOA-X-B-IBNR (SOA 3)             CL**1
02968          ADD X-E-LOSS (YR)       TO SOA-X-B-LOSS (SOA 2)             CL**1
02969          ADD X-E-IBNR (YR)       TO SOA-X-B-IBNR (SOA 2)             CL**1
02970      ELSE                                                            CL**1
02971         IF YR = +2                                                   CL**1
02972             ADD X-E-LOSS (YR)    TO SOA-X-B-LOSS (SOA 1)             CL**1
02973             ADD X-E-IBNR (YR)    TO SOA-X-B-IBNR (SOA 1).            CL**1
02974                                                                      CL**1
02975      IF YR = +1  MOVE ' ' TO AST1.                                   CL**1
02976      IF YR = +2  MOVE ' ' TO AST2.                                   CL**1
02977      IF YR = +3  MOVE ' ' TO AST3.                                   CL**1
02978      MOVE ' ' TO SKIP-PRINT-SW.                                      CL**1
02979                                                                      CL**1
02980  0850-F-5A-BUMP.                                                     CL**1
02981                                                                      CL**1
02982      IF (YR EQUAL +3)                                                CL**1
02983         GO TO 0860-E-FILL-5A.                                        CL**1
02984      ADD +1 TO YR.                                                   CL**1
02985                                                                      CL**1
02986      GO TO 0840-FILL-5A.                                             CL**1
02987  0860-E-FILL-5A.                                                     CL**1
02988      EXIT.                                                           CL**1
02989      EJECT                                                           CL**1
02990  0940-PRINT-MEAN.                                                    CL**1
02991                                                                      CL**1
02992 *    IF X-MEANS (1) EQUAL ZEROS AND                                  CL**1
02993 *       X-MEANS (2) EQUAL ZEROS AND                                  CL**1
02994 *       X-MEANS (3) EQUAL ZEROS                                      CL**1
02995 *       GO TO 0950-MEAN-EXIT.                                        CL**1
02996                                                                      CL**1
02997      MOVE X-MEANS (3) TO HI-BB.                                      CL**1
02998      MOVE X-MEANS (2) TO HI-CC.                                      CL**1
02999      MOVE X-MEANS (1) TO HI-DD.                                      CL**1
03000                                                                      CL**1
03001      MOVE HI-9 TO HI-A.                                              CL**1
03002      MOVE HEAD-I TO PRT  MOVE '0' TO X  PERFORM 0620-PRXX THRU       CL**1
03003              0630-PR-XX.                                             CL**1
03004  0950-MEAN-EXIT.                                                     CL**1
03005      EXIT.                                                           CL**1
03006                                                                      CL**1
03007  9000-DUM-END.                                                       CL**1
03008      MOVE ZEROS  TO RETURN-CODE.
03008      GOBACK.                                                         CL**1
03009                                                                      CL**1
03010  ABEND-PGM.                                                          CL**1
03011                        COPY ELCABEND.                                CL**1
03012 /                                                                    CL**1
03013  LCP-WRITE-POS-PRT SECTION.                                          CL**1
03014      IF LCP-ASA = '+'                                                CL**1
03015          WRITE PRT AFTER 0 LINE                                      CL**1
03016      ELSE                                                            CL**1
03017      IF LCP-ASA = ' '                                                CL**1
03018          WRITE PRT AFTER ADVANCING 1 LINE                            CL**1
03019      ELSE                                                            CL**1
03020      IF LCP-ASA = '0'                                                CL**1
03021          WRITE PRT AFTER ADVANCING 2 LINE                            CL**1
03022      ELSE                                                            CL**1
03023      IF LCP-ASA = '-'                                                CL**1
03024          WRITE PRT AFTER ADVANCING 3 LINE                            CL**1
03025      ELSE                                                            CL**1
03026      IF LCP-ASA = '1'                                                CL**1
03027          WRITE PRT AFTER ADVANCING PAGE                              CL**1
03028      ELSE                                                            CL**1
03029      IF LCP-ASA = '2'                                                CL**1
03030          WRITE PRT AFTER ADVANCING LCP-CH2                           CL**1
03031      ELSE                                                            CL**1
03032      IF LCP-ASA = '3'                                                CL**1
03033          WRITE PRT AFTER ADVANCING LCP-CH3                           CL**1
03034      ELSE                                                            CL**1
03035      IF LCP-ASA = '4'                                                CL**1
03036          WRITE PRT AFTER ADVANCING LCP-CH4                           CL**1
03037      ELSE                                                            CL**1
03038      IF LCP-ASA = '5'                                                CL**1
03039          WRITE PRT AFTER ADVANCING LCP-CH5                           CL**1
03040      ELSE                                                            CL**1
03041      IF LCP-ASA = '6'                                                CL**1
03042          WRITE PRT AFTER ADVANCING LCP-CH6                           CL**1
03043      ELSE                                                            CL**1
03044      IF LCP-ASA = '7'                                                CL**1
03045          WRITE PRT AFTER ADVANCING LCP-CH7                           CL**1
03046      ELSE                                                            CL**1
03047      IF LCP-ASA = '8'                                                CL**1
03048          WRITE PRT AFTER ADVANCING LCP-CH8                           CL**1
03049      ELSE                                                            CL**1
03050      IF LCP-ASA = '9'                                                CL**1
03051          WRITE PRT AFTER ADVANCING LCP-CH9                           CL**1
03052      ELSE                                                            CL**1
03053      IF LCP-ASA = 'A'                                                CL**1
03054          WRITE PRT AFTER ADVANCING LCP-CH10                          CL**1
03055      ELSE                                                            CL**1
03056      IF LCP-ASA = 'B'                                                CL**1
03057          WRITE PRT AFTER ADVANCING LCP-CH11                          CL**1
03058      ELSE                                                            CL**1
03059      IF LCP-ASA = 'C'                                                CL**1
03060          WRITE PRT AFTER ADVANCING LCP-CH12                          CL**1
03061      ELSE                                                            CL**1
03062      IF LCP-ASA = 'V'                                                CL**1
03063          WRITE PRT AFTER ADVANCING LCP-P01                           CL**1
03064      ELSE                                                            CL**1
03065      IF LCP-ASA = 'W'                                                CL**1
03066          WRITE PRT AFTER ADVANCING LCP-P02                           CL**1
03067      ELSE                                                            CL**1
03068      DISPLAY 'ASA CODE ERROR'.                                       CL**1
03069  LCP-WRITE-END-PRT.                                                  CL**1
03070      EXIT.                                                           CL**1
03071                                                                      CL**1
03072      EJECT                                                           CL**1
