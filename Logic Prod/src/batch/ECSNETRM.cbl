00001  IDENTIFICATION DIVISION.                                         04/14/98
00002                                                                   ECSNETRM
00003  PROGRAM-ID.                 ECSNETRM.                               LV002
00004 *              PROGRAM CONVERTED BY                               ECSNETRM
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECSNETRM
00006 *              CONVERSION DATE 02/08/96 12:31:54.                 ECSNETRM
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE                CL**2
00008 *                            VMOD=2.004                           ECSNETRM
CIDMOD*                                                                 ECSNETRM
CIDMOD*     CSO  MODS  IN  COPYBOOK  ECSNETRM                           ECSNETRM
00009                                                                   ECSNETRM
00010 *AUTHOR.     LOGIC, INC.                                          ECSNETRM
00011 *            DALLAS, TEXAS.                                       ECSNETRM
00012                                                                   ECSNETRM
00013 *DATE-COMPILED.                                                   ECSNETRM
00014                                                                   ECSNETRM
00015 *            *****************************************************ECSNETRM
00016 *            *                                                   *ECSNETRM
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECSNETRM
00018 *            *                                                   *ECSNETRM
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECSNETRM
00020 *                                                                *ECSNETRM
00021 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECSNETRM
00022 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECSNETRM
00023 *            *                                                   *ECSNETRM
00024 *            *****************************************************ECSNETRM
00025                                                                   ECSNETRM
00026 *REMARKS.                                                         ECSNETRM
00027 *        STANDARD NET PAY MODULE.                                 ECSNETRM
00028                                                                   ECSNETRM
00029 *        FIVE PARAMETERS ARE PASSED TO THIS MODULE AND A FACTOR   ECSNETRM
00030 *        IS RETURNED. PARAMETERS PASSED  - A.P.R. (S999V9999)     ECSNETRM
00031 *                                          ORIGINAL TERM (S999)   ECSNETRM
00032 *                                          REMAINING TERM (S999)  ECSNETRM
00033 *                                          NET PAY OPTION (X)     ECSNETRM
00034 *                                          CAPPED TERM (S999)     ECSNETRM
00035 *                     FACTOR RETURNED IS - FACTOR (S9(4)V9(9))    ECSNETRM
00036                                                                   ECSNETRM
00037 *        FACTOR RETURNED IS MULTIPLIED BY ORIG. FACE TO GET       ECSNETRM
00038 *        REMAINING FACE. IF ORIGINAL TERM = REMAINING TERM,       ECSNETRM
00039 *        FACTOR WOULD BE 1, THEREFORE MODULE ASSUMES RATING IS    ECSNETRM
00040 *        DESIRED AND FACTOR THAT IS RETURNED MAY BE MULTIPLIED    ECSNETRM
00041 *        BY THOUSANDS OF ORIGINAL FACE AND REGULAR PREMIUM PER    ECSNETRM
00042 *        $100 PER MONTH TO GET PREMIUM TO BE CHARGED.             ECSNETRM
00043                                                                   ECSNETRM
00044 *        OPTIONS - S = NET SIMPLE                                 ECSNETRM
00045 *              SPACE = NET PAY STANDARD  (1 MO. INTEREST)         ECSNETRM
00046 *                  N = NET PAY STANDARD  (1 MO. INTEREST)         ECSNETRM
00047 *                  A = ALTERNATE NET PAY  (0 MO. INTEREST)        ECSNETRM
00048 *                  I = ALTERNATE NET PAY  (2 MO. INTEREST)        ECSNETRM
00049 *                  T = TRUNCATED  (0 MO. INTEREST)                ECSNETRM
00050 *                  U = TRUNCATED  (1 MO. INTEREST)                ECSNETRM
00051 *                  V = TRUNCATED  (2 MO. INTEREST)                ECSNETRM
00052 *                  R = REFUNDS    (REGULAR OR TRUNCATED)          ECSNETRM
042904******************************************************************
042904*                   C H A N G E   L O G
042904*
042904* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
042904*-----------------------------------------------------------------
042904*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
042904* EFFECTIVE    NUMBER
042904*-----------------------------------------------------------------
042904* 042904    2003080800002  PEMA ADD ACTUARIAL EARNING METHOD
111413* 111413  IR2013111300001  PEMA REMOVE CODE FOR APR < 3.0
042904******************************************************************
00053  EJECT                                                            ECSNETRM
00054  ENVIRONMENT DIVISION.                                            ECSNETRM
00055                                                                   ECSNETRM
00056  DATA DIVISION.                                                   ECSNETRM
00057                                                                   ECSNETRM
00058  WORKING-STORAGE SECTION.                                         ECSNETRM
00059  77  FILLER  PIC X(32) VALUE '********************************'.  ECSNETRM
00060  77  FILLER  PIC X(32) VALUE '     ECSNETRM WORKING-STORAGE   '.  ECSNETRM
00061  77  FILLER  PIC X(32) VALUE '********* VMOD=2.004 ***********'.  ECSNETRM
00062                                                                   ECSNETRM
00063  01  COMP-3-WORK-AREA    COMP-3.                                  ECSNETRM
00064      12  V                   PIC SV9(9)          VALUE +.0.       ECSNETRM
00065      12  I                   PIC SV9(9)          VALUE +.0.       ECSNETRM
00066      12  RA                  PIC S9(6)V9(9)      VALUE +0.0.      ECSNETRM
00067      12  VX                  PIC S9V9(8)         VALUE +0.0.      ECSNETRM
00068      12  SV                  PIC S9V9(8)         VALUE +0.0.      ECSNETRM
00069      12  SX                  PIC S9V9(8)         VALUE +0.0.      ECSNETRM
00070      12  N2                  PIC S9(7)           VALUE +0.        ECSNETRM
00071      12  N3                  PIC S9(7)           VALUE +0.        ECSNETRM
00072      12  K-I                 PIC S9V9(8)         VALUE +0.0.      ECSNETRM
00073      12  FACTOR              PIC S9(4)V9(9)      VALUE +0.0.      ECSNETRM
00074      12  WK1                 PIC S9(4)V9(9)      VALUE +0.0.      ECSNETRM
00075      12  WK2                 PIC S9(4)V9(9)      VALUE +0.0.      ECSNETRM
00076      12  WK3                 PIC S9(7)V9(8)      VALUE +0.0.      ECSNETRM
00077      12  WK4                 PIC S9(7)V9(8)      VALUE +0.0.      ECSNETRM
00078      12  WK5                 PIC S9(7)V9(8)      VALUE +0.0.      ECSNETRM
00079      12  K1                  PIC S9              VALUE +1.        ECSNETRM
00080      12  K12                 PIC S999            VALUE +12.       ECSNETRM
00081      12  K100                PIC S999            VALUE +100.      ECSNETRM
00082      12  K1000               PIC S9(7)           VALUE +1000.     ECSNETRM
00083      12  ANNUAL-INT-RATE     PIC S9(3)V9(4).                      ECSNETRM
00084      12  ORIGINAL-TERM       PIC S999.                            ECSNETRM
00085      12  N  REDEFINES                                             ECSNETRM
00086          ORIGINAL-TERM       PIC S999.                            ECSNETRM
00087      12  REMAINING-TERM      PIC S999.                            ECSNETRM
00088      12  R  REDEFINES                                             ECSNETRM
00089          REMAINING-TERM      PIC S999.                            ECSNETRM
00090      12  CAPPED-TERM         PIC S999.                            ECSNETRM
00091      12  M  REDEFINES                                             ECSNETRM
00092          CAPPED-TERM         PIC S999.                            ECSNETRM
00093      12  EXPIRED-TERM        PIC S999.                            ECSNETRM
00094      12  E  REDEFINES                                             ECSNETRM
00095          EXPIRED-TERM        PIC S999.                            ECSNETRM
00096                                                                   ECSNETRM
00097  01  BINARY-WORK-AREA    COMP.                                    ECSNETRM
00098      12  X1                  PIC S999            VALUE +0.        ECSNETRM
00099      12  X2                  PIC S999            VALUE +0.        ECSNETRM
00100      12  MAX-X               PIC S9(5)           VALUE +0.        ECSNETRM
00101      12  B1                  PIC S9(5)           VALUE +1.        ECSNETRM
00102                                                                   ECSNETRM
00103  01  OPTION-SW               PIC X               VALUE 'X'.       ECSNETRM
00104      88  NPO-STD                             VALUE SPACE.         ECSNETRM
00105      88  NPO-ALT                             VALUE 'A'.           ECSNETRM
00106      88  NPO-SIMPLE                          VALUE 'S'.           ECSNETRM
00107      88  NPO-2MO                             VALUE 'I'.           ECSNETRM
00108      88  NPO-TRUNC                           VALUE 'T' 'U' 'V'.   ECSNETRM
00109      88  NPO-TRUNC-0                         VALUE 'T'.           ECSNETRM
00110      88  NPO-TRUNC-1                         VALUE 'U'.           ECSNETRM
00111      88  NPO-TRUNC-2                         VALUE 'V'.           ECSNETRM
00112      88  NPO-REFUND                          VALUE 'R'.           ECSNETRM
00113                                                                   ECSNETRM
00114  01  NP-PROCESS-SW           PIC X               VALUE '1'.       ECSNETRM
00115      88  NP-RATING                               VALUE '1'.       ECSNETRM
00116      88  NP-REFUND                               VALUE '2'.       ECSNETRM
00117      88  NP-REMAIN-AMT                           VALUE '3'.       ECSNETRM
00118                                                                   ECSNETRM
00119  01  TYPE-SW                 PIC X               VALUE 'N'.       ECSNETRM
00120      88  NET-STD                                 VALUE 'N'.       ECSNETRM
00121      88  NET-SMP                                 VALUE 'S'.       ECSNETRM
00122  EJECT                                                            ECSNETRM
00123  LINKAGE SECTION.                                                 ECSNETRM
00124                                                                   ECSNETRM
00125  01  N-P-APR                 PIC S9(3)V9(4)  COMP-3.              ECSNETRM
00126                                                                   ECSNETRM
00127  01  N-P-ORIG                PIC S999        COMP-3.              ECSNETRM
00128                                                                   ECSNETRM
00129  01  N-P-REM                 PIC S999        COMP-3.              ECSNETRM
00130                                                                   ECSNETRM
00131  01  N-P-OPT                 PIC X.                               ECSNETRM
00132                                                                   ECSNETRM
00133  01  N-P-CAP                 PIC S999        COMP-3.              ECSNETRM
00134                                                                   ECSNETRM
00135  01  N-P-FACTOR              PIC S9(4)V9(9)  COMP-3.              ECSNETRM
00136                                                                   ECSNETRM
00137  EJECT                                                            ECSNETRM
00138  PROCEDURE DIVISION                                               ECSNETRM
00139      USING N-P-APR  N-P-ORIG  N-P-REM  N-P-OPT  N-P-CAP           ECSNETRM
00140            N-P-FACTOR.                                            ECSNETRM
00141                                                                   ECSNETRM
00142  0000-MAIN-LINE.                                                  ECSNETRM
00143      MOVE N-P-APR     TO ANNUAL-INT-RATE.                         ECSNETRM
00144      MOVE N-P-ORIG    TO ORIGINAL-TERM                            ECSNETRM
00145                          CAPPED-TERM.                             ECSNETRM
00146      MOVE N-P-REM     TO REMAINING-TERM.                          ECSNETRM
00147      MOVE N-P-OPT     TO OPTION-SW.                               ECSNETRM
00148                                                                   ECSNETRM
00149      IF NPO-TRUNC                                                 ECSNETRM
00150          MOVE N-P-CAP TO CAPPED-TERM.                             ECSNETRM
00151                                                                   ECSNETRM
00152      IF (NPO-REFUND)
042904        OR (N-P-OPT = 'S')
00153         MOVE N-P-CAP TO CAPPED-TERM
042904     END-IF
00154                                                                   ECSNETRM
00155      MOVE +0 TO FACTOR.                                           ECSNETRM
00156                                                                   ECSNETRM
00157      IF ANNUAL-INT-RATE = ZERO                                    ECSNETRM
00158          GO TO 9999-EOJ.                                          ECSNETRM
00159                                                                   ECSNETRM
00160      IF ORIGINAL-TERM = ZERO                                      ECSNETRM
00161          GO TO 9999-EOJ.                                          ECSNETRM
00162                                                                   ECSNETRM
00163      IF REMAINING-TERM = ZERO                                     ECSNETRM
00164          GO TO 9999-EOJ.                                          ECSNETRM
00165                                                                   ECSNETRM
00166      IF REMAINING-TERM GREATER ORIGINAL-TERM                      ECSNETRM
00167          GO TO 9999-EOJ.                                          ECSNETRM
00168                                                                   ECSNETRM
00169      IF CAPPED-TERM = ZERO                                        ECSNETRM
00170          GO TO 9999-EOJ.                                          ECSNETRM
00171                                                                   ECSNETRM
00172      IF CAPPED-TERM GREATER ORIGINAL-TERM                         ECSNETRM
00173          GO TO 9999-EOJ.                                          ECSNETRM

111413*    IF ANNUAL-INT-RATE LESS +3                                   ECSNETRM
111413*        COMPUTE ANNUAL-INT-RATE = ANNUAL-INT-RATE * +10.         ECSNETRM
111413*                                                                 ECSNETRM
111413*    IF ANNUAL-INT-RATE LESS +3                                   ECSNETRM
111413*        COMPUTE ANNUAL-INT-RATE = ANNUAL-INT-RATE * +10.         ECSNETRM
111413*                                                                 ECSNETRM
111413*    IF ANNUAL-INT-RATE LESS +3                                   ECSNETRM
111413*        COMPUTE ANNUAL-INT-RATE = ANNUAL-INT-RATE * +10.         ECSNETRM

00184      IF (NPO-REFUND)
042904        OR (N-P-OPT = 'S')
00185          MOVE '2' TO NP-PROCESS-SW                                ECSNETRM
00186      ELSE                                                         ECSNETRM
00187          IF ORIGINAL-TERM = REMAINING-TERM                        ECSNETRM
00188              MOVE '1' TO NP-PROCESS-SW                            ECSNETRM
00189          ELSE                                                     ECSNETRM
00190              MOVE '3' TO NP-PROCESS-SW.                           ECSNETRM
00191                                                                   ECSNETRM
00192      IF NPO-SIMPLE                                                ECSNETRM
00193          MOVE 'S' TO TYPE-SW                                      ECSNETRM
00194      ELSE                                                         ECSNETRM
00195          MOVE 'N' TO TYPE-SW.                                     ECSNETRM
00196                                                                   ECSNETRM
00197      COMPUTE I ROUNDED = (ANNUAL-INT-RATE / K100) / K12.          ECSNETRM
00198                                                                   ECSNETRM
00199      COMPUTE V ROUNDED = K1 / (K1 + I).                           ECSNETRM
00200                                                                   ECSNETRM
00201      MOVE V     TO VX.                                            ECSNETRM
00202      MOVE V     TO SV.                                            ECSNETRM
00203                                                                   ECSNETRM
00204      MOVE +1    TO X1.                                            ECSNETRM
00205      MOVE +1    TO SX.                                            ECSNETRM
00206                                                                   ECSNETRM
00207      MOVE ORIGINAL-TERM TO MAX-X.                                 ECSNETRM
00208                                                                   ECSNETRM
00209      COMPUTE X2 = MAX-X - CAPPED-TERM.                            ECSNETRM
00210                                                                   ECSNETRM
00211      IF MAX-X = +1                                                ECSNETRM
00212          GO TO 1000-COMPUTE-REMAINING-FACTOR.                     ECSNETRM
00213                                                                   ECSNETRM
00214      COMPUTE EXPIRED-TERM = CAPPED-TERM - REMAINING-TERM.         ECSNETRM
00215                                                                   ECSNETRM
00216      IF CAPPED-TERM NOT = ORIGINAL-TERM                           ECSNETRM
00217          COMPUTE REMAINING-TERM = ORIGINAL-TERM - EXPIRED-TERM.   ECSNETRM
00218                                                                   ECSNETRM
00219  0500-VX-LOOP.                                                    ECSNETRM
00220      COMPUTE VX ROUNDED = VX * V.                                 ECSNETRM
00221                                                                   ECSNETRM
00222      ADD B1 TO X1.                                                ECSNETRM
00223                                                                   ECSNETRM
00224      IF X1 = REMAINING-TERM                                       ECSNETRM
00225          MOVE VX    TO SV.                                        ECSNETRM
00226                                                                   ECSNETRM
00227      IF X1 = X2                                                   ECSNETRM
00228          MOVE VX    TO SX.                                        ECSNETRM
00229                                                                   ECSNETRM
00230      IF X1 NOT = MAX-X                                            ECSNETRM
00231          GO TO 0500-VX-LOOP.                                      ECSNETRM
00232                                                                   ECSNETRM
00233  1000-COMPUTE-REMAINING-FACTOR.                                   ECSNETRM
00234      COMPUTE WK1 = K1 - VX.                                       ECSNETRM
00235      COMPUTE WK2 = K1 - SV.                                       ECSNETRM
00236      COMPUTE WK5 = K1 - SX.                                       ECSNETRM
00237                                                                   ECSNETRM
00238      IF NP-RATING                                                 ECSNETRM
00239          GO TO 2000-PREMIUM-RATE.                                 ECSNETRM
00240                                                                   ECSNETRM
00241      IF (NP-REFUND)
042904        OR (N-P-OPT = 'S')
00242          GO TO 1500-REFUND-CALC
042904     END-IF
00243                                                                   ECSNETRM
00244      IF NET-STD                                                   ECSNETRM
00245          COMPUTE WK3 ROUNDED = (WK2 * K1000) / WK1.               ECSNETRM
00246                                                                   ECSNETRM
00247      IF NET-SMP                                                   ECSNETRM
00248          COMPUTE WK3 ROUNDED = ((R + 1) / (N + 1)) * (R / N)      ECSNETRM
00249          COMPUTE WK3 ROUNDED = (1 - WK3) * ((I * N / WK1) - 1)    ECSNETRM
00250          COMPUTE WK3 ROUNDED = WK3 + 1 - ((N - R) * I / WK1)      ECSNETRM
00251          COMPUTE WK3 ROUNDED = WK3 * 1000.                        ECSNETRM
00252                                                                   ECSNETRM
00253      IF REMAINING-TERM LESS THAN X2                               ECSNETRM
00254          MOVE +0 TO WK3.                                          ECSNETRM
00255                                                                   ECSNETRM
00256      MOVE WK3 TO FACTOR.                                          ECSNETRM
00257                                                                   ECSNETRM
00258      GO TO 9999-EOJ.                                              ECSNETRM
00259                                                                   ECSNETRM
00260  1500-REFUND-CALC.                                                ECSNETRM
00261      IF REMAINING-TERM NOT LESS MAX-X                             ECSNETRM
00262          MOVE +1 TO FACTOR                                        ECSNETRM
00263          GO TO 9999-EOJ.                                          ECSNETRM
00264                                                                   ECSNETRM
00265      IF REMAINING-TERM LESS +1                                    ECSNETRM
00266          MOVE 0 TO FACTOR                                         ECSNETRM
00267          GO TO 9999-EOJ.                                          ECSNETRM
00268                                                                   ECSNETRM
00269      COMPUTE WK2 ROUNDED = WK2 / I.                               ECSNETRM
00270      COMPUTE WK5 ROUNDED = WK5 / I.                               ECSNETRM
00271      COMPUTE WK1 ROUNDED = WK1 / I.                               ECSNETRM
00272                                                                   ECSNETRM
042904*    DISPLAY ' USING TEST PROGRAM '
042904     IF N-P-OPT = 'S'
042904*       COMPUTE WK3 ROUNDED = WK2 / WK1
042904        COMPUTE WK3 ROUNDED = (R - WK2) / (M - WK1)
042904     ELSE
00273         COMPUTE WK3 ROUNDED =
00274           (N-P-REM - WK2 + WK5) / (CAPPED-TERM - WK1 + WK5)
042904     END-IF
00275                                                                   ECSNETRM
00276      MOVE WK3  TO FACTOR.                                         ECSNETRM
00277                                                                   ECSNETRM
00278      GO TO 9999-EOJ.                                              ECSNETRM
00279                                                                   ECSNETRM
00280  2000-PREMIUM-RATE.                                               ECSNETRM
00281 *    K-I IS ADJUSTMENT FACTOR FOR NO. MONTHS ADD'L. INTEREST      ECSNETRM
00282 *                                                                 ECSNETRM
00283 *      OPTION - N OR U OR SPACE  = 1 MO,  SO K-I = 1 + I          ECSNETRM
00284 *      OPTION - A OR T           = 0 MO,  SO K-I = 1              ECSNETRM
00285 *      OPTION - I OR V           = 2 MO,  SO K-I = 1 + 2I         ECSNETRM
00286 *                                                                 ECSNETRM
00287      COMPUTE K-I = K1 + I.                                        ECSNETRM
00288                                                                   ECSNETRM
00289      IF NPO-ALT OR NPO-TRUNC-0                                    ECSNETRM
00290          MOVE K1 TO K-I.                                          ECSNETRM
00291                                                                   ECSNETRM
00292      IF NPO-2MO OR NPO-TRUNC-2                                    ECSNETRM
00293          COMPUTE K-I = K1 + (2 * I).                              ECSNETRM
00294                                                                   ECSNETRM
00295      COMPUTE RA ROUNDED = 1 -                                     ECSNETRM
00296              ((X2 * (X2 + 1)) /                                   ECSNETRM
00297               (N *  (N  + 1))).                                   ECSNETRM
00298                                                                   ECSNETRM
00299      IF NET-STD                                                   ECSNETRM
00300          COMPUTE WK3 ROUNDED = ((I * M) + VX - SX) * 2 * N        ECSNETRM
00301          COMPUTE WK3 ROUNDED = WK3 / ((1 - VX) * M * I)           ECSNETRM
00302          COMPUTE WK3 ROUNDED = WK3 / ((2 * N) - M + 1)            ECSNETRM
00303          COMPUTE WK3 ROUNDED = WK3 * RA * K-I.                    ECSNETRM
00304                                                                   ECSNETRM
00305      IF NET-SMP                                                   ECSNETRM
00306          COMPUTE N2 = N * N                                       ECSNETRM
00307          COMPUTE N3 = N2 * N                                      ECSNETRM
00308          COMPUTE WK3 ROUNDED = 2 * N2 * WK1                       ECSNETRM
00309          COMPUTE WK3 ROUNDED = WK3 + (N3 * I) - (N2 * I)          ECSNETRM
00310          COMPUTE WK3 ROUNDED = WK3 + (4 * N * WK1)                ECSNETRM
00311          COMPUTE WK3 ROUNDED = WK3 * (1 + I) * 10                 ECSNETRM
00312          COMPUTE WK3 ROUNDED = WK3 / (36 * (N + 1) * WK1).        ECSNETRM
00313                                                                   ECSNETRM
00314      MOVE WK3 TO FACTOR.                                          ECSNETRM
00315                                                                   ECSNETRM
00316      GO TO 9999-EOJ.                                              ECSNETRM
00317                                                                   ECSNETRM
00318  9999-EOJ.                                                        ECSNETRM
00319      MOVE FACTOR TO N-P-FACTOR.                                   ECSNETRM
00320                                                                   ECSNETRM
00321      GOBACK.                                                      ECSNETRM
00322                                                                   ECSNETRM
00323  9999-DUMMY-END.                                                  ECSNETRM
00324      GOBACK.                                                      ECSNETRM
