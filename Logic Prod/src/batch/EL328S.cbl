00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL328
00003  PROGRAM-ID.                 EL328 .                                 LV003
00004 *              PROGRAM CONVERTED BY                               EL328
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL328
00006 *              CONVERSION DATE 02/14/96 13:43:07.                 EL328
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL328
00008 *                            VMOD=2.008                           EL328
00009                                                                   EL328
00009                                                                   EL328
00010 *AUTHOR.     LOGIC,INC.                                           EL328
00011 *            DALLAS, TEXAS.                                       EL328
00012                                                                   EL328
00013 *DATE-COMPILED.                                                   EL328
00014                                                                   EL328
00015 *SECURITY.   *****************************************************EL328
00016 *            *                                                   *EL328
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL328
00018 *            *                                                   *EL328
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL328
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL328
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL328
00022 *            *                                                   *EL328
00023 *            *****************************************************EL328
00024                                                                   EL328
00025 *REMARKS.                                                         EL328
00026 *    PRINTS REGISTER OF DENIED CLAIMS FROM THE                    EL328
00027 *    CLAIMS HISTORY FILE.                                         EL328
00028                                                                   EL328
00029  ENVIRONMENT DIVISION.                                            EL328
00030                                                                   EL328
00031  INPUT-OUTPUT SECTION.                                            EL328
00032                                                                   EL328
00033  FILE-CONTROL.                                                    EL328
00034      SELECT CLAIMS-HIST-IN   ASSIGN TO SYS010-UT-2400-S-SYS010.   EL328
00035      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL328
00036      SELECT PRINTER          ASSIGN TO SYS008-UR-1403-S-SYS008.   EL328
00037      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   EL328
00038      SELECT MPPLAN           ASSIGN TO SYS021-FBA1-MPPLAN         EL328
00039                              ORGANIZATION IS INDEXED              EL328
00040                              ACCESS IS DYNAMIC                    EL328
00041                              RECORD KEY IS PP-CONTROL-PRIMARY     EL328
00042                              FILE STATUS IS MPPLAN-FILE-STATUS.   EL328
00043                                                                   EL328
00044      SELECT ELREPT           ASSIGN TO SYS018-FBA1-ELREPT         EL328
00045                              ORGANIZATION IS INDEXED              EL328
00046                              ACCESS IS DYNAMIC                    EL328
00047                              RECORD KEY IS RF-CONTROL-PRIMARY     EL328
00048                              FILE STATUS IS DTE-VSAM-FLAGS.       EL328
00049                                                                   EL328
00050  DATA DIVISION.                                                   EL328
00051  FILE SECTION.                                                    EL328
00052                                                                   EL328
00053  FD  FICH                    COPY ELCFCHFD.                       EL328
00054                                                                   EL328
00055      EJECT                                                        EL328
00056  FD  PRINTER                 COPY ELCPRTFD.                       EL328
00057                                                                   EL328
00058      EJECT                                                        EL328
00059  FD  CLAIMS-HIST-IN          COPY ELCHAF.                         EL328
00060                                                                   EL328
00061      EJECT                                                        EL328
00062  FD  DISK-DATE               COPY ELCDTEFD.                       EL328
00063                                                                   EL328
00064      EJECT                                                        EL328
00065  FD  MPPLAN.                                                      EL328
00066                              COPY MPCPLAN.                        EL328
00067      EJECT                                                        EL328
00068  FD  ELREPT                  COPY ELCRPTFD.                       EL328
00069                              COPY ELCREPT.                        EL328
00070                                                                   EL328
00071  EJECT                                                            EL328
00072  WORKING-STORAGE SECTION.                                         EL328
00073  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL328
00074  77  FILLER  PIC X(32) VALUE '********************************'.  EL328
00075  77  FILLER  PIC X(32) VALUE '*    EL328  WORKING STORAGE    *'.  EL328
00076  77  FILLER  PIC X(32) VALUE '**** VMOD=2.008 ****************'.  EL328
00077                                                                   EL328
00078  77  CLM-SUB-L               PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00079  77  CLM-SUB-A               PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00080  77  CLM-CLO-L               PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00081  77  CLM-CLO-A               PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00082  77  CLM-DEN-L               PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00083  77  CLM-DEN-A               PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00084  77  CLM-PAY-L               PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00085  77  CLM-PAY-A               PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00086  77  CLM-PND-L               PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00087  77  CLM-PND-A               PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00088  77  PCT-DEN-L               PIC S9(3)V99   COMP-3 VALUE ZERO.    EL328
00089  77  PCT-DEN-A               PIC S9(3)V99   COMP-3 VALUE ZERO.    EL328
00090  77  TOT-EXP-L               PIC S9(9)V99   COMP-3 VALUE ZERO.    EL328
00091  77  TOT-EXP-A               PIC S9(9)V99   COMP-3 VALUE ZERO.    EL328
00092  77  TOT-AH-BEN              PIC S9(9)V99   COMP-3 VALUE ZERO.    EL328
00093  77  TOT-LF-BEN              PIC S9(9)V99   COMP-3 VALUE ZERO.    EL328
00094  77  DEN-BEN-L               PIC S9(9)V99   COMP-3 VALUE ZERO.    EL328
00095  77  DEN-BEN-A               PIC S9(9)V99   COMP-3 VALUE ZERO.    EL328
00096  77  PCT-BEN-L               PIC S9(3)V99   COMP-3 VALUE ZERO.    EL328
00097  77  PCT-BEN-A               PIC S9(3)V99   COMP-3 VALUE ZERO.    EL328
00098  77  LIN                     PIC S9(3)      COMP-3 VALUE ZERO.    EL328
00099  77  GRAND-CLM-SUB-L         PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00100  77  GRAND-CLM-SUB-A         PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00101  77  GRAND-CLM-CLO-L         PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00102  77  GRAND-CLM-CLO-A         PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00103  77  GRAND-CLM-DEN-L         PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00104  77  GRAND-CLM-DEN-A         PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00105  77  GRAND-CLM-PAY-L         PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00106  77  GRAND-CLM-PAY-A         PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00107  77  GRAND-CLM-PND-L         PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00108  77  GRAND-CLM-PND-A         PIC S9(7)      COMP-3 VALUE ZERO.    EL328
00109  77  GRAND-TOT-EXP-L         PIC S9(9)V99   COMP-3 VALUE ZERO.    EL328
00110  77  GRAND-TOT-EXP-A         PIC S9(9)V99   COMP-3 VALUE ZERO.    EL328
00111  77  GRAND-DEN-BEN-L         PIC S9(9)V99   COMP-3 VALUE ZERO.    EL328
00112  77  GRAND-DEN-BEN-A         PIC S9(9)V99   COMP-3 VALUE ZERO.    EL328
00113  77  RANGE-SW                PIC X                 VALUE SPACES.  EL328
00114      88  CLAIM-WITHIN-RANGE                        VALUE 'X'.     EL328
00115      88  CLAIM-NOT-WITHIN-RANGE                    VALUE ' '.     EL328
00116  77  WS-INC-MY               PIC S9(5)      COMP-3 VALUE +0.      EL328
00117  77  WS-PDTH-MY              PIC S9(5)      COMP-3 VALUE +0.      EL328
00118  77  WS-EFF-MY               PIC S9(5)      COMP-3 VALUE +0.      EL328
00119  77  WS-REM-TERM             PIC S9(5)      COMP-3 VALUE +0.      EL328
00120  77  WS-CLAIM-DURA           PIC S9(5)V99   COMP-3 VALUE +0.      EL328
00121  77  WS-RATIO                PIC S9V9(7)    COMP-3 VALUE +0.      EL328
00122  77  WS-TOT-REM-TERM         PIC S9(11)     COMP-3 VALUE +0.      EL328
00123  77  GR-TOT-REM-TERM         PIC S9(11)     COMP-3 VALUE +0.      EL328
00124  77  WS-TOT-CLAIM-DURA       PIC S9(11)V99  COMP-3 VALUE +0.      EL328
00125  77  GR-TOT-CLAIM-DURA       PIC S9(11)V99  COMP-3 VALUE +0.      EL328
00126  77  WS-TOT-FACTOR           PIC S9(3)V9(4) COMP-3 VALUE +0.      EL328
00127  77  WS-AH-CLAIM-COUNT       PIC S9(11)     COMP-3 VALUE +0.      EL328
00128  77  GR-AH-CLAIM-COUNT       PIC S9(11)     COMP-3 VALUE +0.      EL328
00129  77  WS-AH-ITD-CLAIM-COUNT   PIC S9(11)     COMP-3 VALUE +0.      EL328
00130  77  GR-AH-ITD-CLAIM-COUNT   PIC S9(11)     COMP-3 VALUE +0.      EL328
00131  77  WS-TOTAL-PAID-AMT       PIC S9(9)V99   COMP-3 VALUE +0.      EL328
00132  77  GR-TOTAL-PAID-AMT       PIC S9(9)V99   COMP-3 VALUE +0.      EL328
00133  77  DISPLAY-COUNT           PIC S9(5)      COMP-3 VALUE +0.      EL328
00134  77  DISPLAY-FIELD           PIC ZZZ,ZZZ,Z99       VALUE ZERO.    EL328
00135  77  HOLD-ODD-DAYS-OVER      PIC SV9(3)     COMP-3 VALUE ZEROS.   EL328
00136                                                                   EL328
00137      EJECT                                                        EL328
00138                             COPY ELCDATE.                            CL**3
00139  01  TEMP-STORAGE                    COMP-3.                      EL328
00140      12  TEMP-YR                     PIC 99.                      EL328
00141      12  TEMP-MO                     PIC 99.                      EL328
00142      12  SUB-A                       PIC S999        VALUE +0.    EL328
00143      12  SUB-B                       PIC S999        VALUE +0.    EL328
00144      12  SAVE-SUB-A                  PIC S999        VALUE +0.    EL328
00145      12  SAVE-SUB-B                  PIC S999        VALUE +0.    EL328
00146      12  REC-CT                      PIC S9(6)       VALUE +0.    EL328
00147      12  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  EL328
00148      12  WS-ZERO                     PIC S9          VALUE ZERO.  EL328
00149                                                                   EL328
00150  01  WS-AVG-CLAIM-DURA       PIC  9(5)V99            VALUE ZERO.  EL328
00151  01  WS-AVG-CLAIM-DURA-R REDEFINES WS-AVG-CLAIM-DURA.             EL328
00152      12  WS-AVG-CLAIM-MONTH  PIC 9(5).                            EL328
00153      12  WS-AVG-CLAIM-DAY    PIC V9(2).                           EL328
00154                                                                   EL328
00155  01  MISC.                                                        EL328
00156      12  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.EL328
00157      12  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  EL328
00158      12  ABEND-CODE                  PIC X(4)        VALUE SPACES.EL328
00159      12  ABEND-OPTION                PIC X           VALUE 'Y'.   EL328
00160      12  MPPLAN-FILE-STATUS          PIC X(02)   VALUE LOW-VALUES.EL328
00161      12  PGM-SUB                     PIC S999 COMP-3 VALUE +328.  EL328
00162      12  X                           PIC X       VALUE SPACE.     EL328
00163      12  PG-NO                       PIC S9(5)   VALUE +0.        EL328
00164      12  DENIED-STATUS               PIC X       VALUE SPACE.     EL328
00165           88  DENIED-2                           VALUE '2'.       EL328
00166      12  OLC-REPORT-NAME             PIC X(5)    VALUE 'EL328'.   EL328
00167      12  B-WORK-DATE.                                             EL328
00168          15  B-WORK-C                PIC XX      VALUE SPACES.    EL328
00169          15  B-WORK-Y                PIC XX      VALUE SPACES.    EL328
00170          15  B-WORK-M                PIC XX      VALUE SPACES.    EL328
00171          15  B-WORK-D                PIC XX      VALUE SPACES.    EL328
00172      12  BEGIN-DATE REDEFINES B-WORK-DATE                         EL328
00173                              PIC 9(8).                            EL328
00174                                                                   EL328
00175      12  E-WORK-DATE.                                             EL328
00176          15  E-WORK-C                PIC XX      VALUE SPACES.    EL328
00177          15  E-WORK-Y                PIC XX      VALUE SPACES.    EL328
00178          15  E-WORK-M                PIC XX      VALUE SPACES.    EL328
00179          15  E-WORK-D                PIC XX      VALUE SPACES.    EL328
00180      12  END-DATE REDEFINES E-WORK-DATE                           EL328
00181                                      PIC 9(8).                    EL328
00182      12  PLAY-DATE                   PIC 9(8)    VALUE ZEROS.     EL328
00183      12  WS-WORK-DATE.                                            EL328
00184          15  WORK-CY                 PIC 9(04)   VALUE ZEROS.     EL328
00185          15  WORK-CYR REDEFINES WORK-CY.                          EL328
00186              20  WORK-C              PIC 99.                      EL328
00187              20  WORK-Y              PIC 99.                      EL328
00188          15  WORK-M                  PIC 99      VALUE ZEROS.     EL328
00189          15  WORK-D                  PIC 99      VALUE ZEROS.     EL328
00190                                                                   EL328
00191      EJECT                                                        EL328
00192                                  COPY ELCCERT.                    EL328
00193      EJECT                                                        EL328
00194                                  COPY MPCPLCY.                    EL328
00195      EJECT                                                        EL328
00196                                  COPY ELCARCH.                    EL328
00197      EJECT                                                        EL328
00198                                  COPY ELCMSTR.                    EL328
00199      EJECT                                                        EL328
00200                                  COPY ELCTRLR.                    EL328
00201      EJECT                                                        EL328
00202                                                                   EL328
00203  01  HD-1.                                                        EL328
00204      12  FILLER          PIC X(48)  VALUE SPACES.                 EL328
00205      12  FILLER          PIC X(20)  VALUE ' REGISTER OF DENIED'.  EL328
00206      12  FILLER          PIC X(51)  VALUE ' CLAIMS'.              EL328
00207      12  FILLER          PIC X(8)   VALUE 'EL328'.                EL328
00208  01  HD-2.                                                        EL328
00209      12  FILER           PIC X(47)  VALUE SPACES.                 EL328
00210      12  HD-COMP         PIC X(30).                               EL328
00211      12  FILLER          PIC X(42)  VALUE SPACES.                 EL328
00212      12  HD-IPL          PIC X(8).                                EL328
00213  01  HD-3.                                                        EL328
00214      12  FILLER          PIC X(53)  VALUE SPACES.                 EL328
00215      12  HD-DATE         PIC X(18).                               EL328
00216      12  FILLER          PIC X(48)  VALUE SPACES.                 EL328
00217      12  FILLER          PIC X(5)   VALUE 'PAGE'.                 EL328
00218      12  HD-PG           PIC ZZ,ZZZ.                              EL328
00219  01  HD-4.                                                        EL328
00220      12  FILLER          PIC X(9)   VALUE ' CARRIER '.            EL328
00221      12  P-CARR          PIC X      VALUE 'Z'.                    EL328
00222      12  FILLER          PIC X(3)   VALUE ' - '.                  EL328
00223      12  P-CNAM          PIC X(30)  VALUE SPACES.                 EL328
00224  01  HD-5.                                                        EL328
00225      12  FILLER          PIC X(11)  VALUE 'FOR PERIOD '.          EL328
PEMTMP     12  FILLER          PIC X(8) VALUE '00/00/00'.

00231      12  FILLER          PIC X(6)  VALUE ' THRU '.                EL328
00232      12  PDT2-MM         PIC XX.                                  EL328
00233      12  FILLER          PIC X     VALUE '/'.                     EL328
00234      12  PDT2-DD         PIC XX.                                  EL328
00235      12  FILLER          PIC X     VALUE '/'.                     EL328
00236      12  PDT2-YY         PIC XX.                                  EL328
00237  01  HD-6.                                                        EL328
           12  FILLER PIC X(51) VALUE 
             '  ACCT NO  CLAIM NO  CERT NO   TYPE CLAIMANT NAME '.
      *        XXXXXXXXXX XXXXXXX XXXXXXXXXXX XX XXXXXXXXXX XXXXXX
00238 *    12  FILLER          PIC X(4)  VALUE SPACES.                  EL328
00239 *    12  FILLER          PIC X(21) VALUE 'CLAIM NO.  CERT NO.'.   EL328
00240 *    12  FILLER          PIC X(26) VALUE 'TYPE  CLAIMANT NAME '.  EL328
00241      12  FILLER          PIC X(20) VALUE 'INCURRED  REPORTED  '.  EL328
00242      12  FILLER          PIC X(19) VALUE 'AMT PAID    DENIAL '.   EL328
00243      12  FILLER          PIC X(16) VALUE 'CD  DENIAL DT   '.      EL328
00244      12  FILLER          PIC X(20) VALUE 'COVERAGE   REMAINING'.  EL328
00245      12  FILLER          PIC X(8)  VALUE ' BENEFIT'.              EL328
00246                                                                   EL328
00247  01  HD-TOTAL.                                                    EL328
00248      12  FILLER              PIC X(39)  VALUE SPACES.             EL328
00249      12  HDT-LF-OVERRIDE12   PIC X(12)  VALUE SPACES.             EL328
00250      12  FILLER              PIC X(14)  VALUE SPACES.             EL328
00251      12  HDT-AH-OVERRIDE12   PIC X(12)  VALUE SPACES.             EL328
00252      12  FILLER              PIC X(43)  VALUE SPACES.             EL328
00253                                                                   EL328
00254  01  DETAIL-LINE.                                                 EL328
00255      12  FILLER          PIC X.
           12  P-ACCT          PIC X(10).
           12  FILLER          PIC X.
00256      12  P-CLAIM         PIC X(7).                                EL328
00257      12  FILLER          PIC X.
00258      12  P-CERT          PIC X(11).                               EL328
00259      12  FILLER          PIC X.                                   EL328
00260      12  P-TYPE          PIC XX.                                  EL328
00261      12  FILLER          PIC X.
00262      12  P-LNAME         PIC X(10).                               EL328
00263      12  FILLER          PIC X.                                   EL328
00264      12  P-FNAME         PIC X(6).                                EL328
00265      12  FILLER          PIC X(3).                                EL328
00266      12  P-INCURRED-DT   PIC X(8).                                EL328
00267      12  FILLER          PIC XX.                                  EL328
00268      12  P-REPORTED-DT   PIC X(8).                                EL328
00269      12  P-AMT           PIC ZZZZ,ZZZ.99-.                        EL328
00270      12  FILLER          PIC X(5).                                EL328
00271      12  P-CODE          PIC X(4).                                EL328
00272      12  FILLER          PIC X(5).                                EL328
00273      12  P-DENIAL-DT     PIC X(8).                                EL328
00274      12  FILLER          PIC X(3).                                EL328
00275      12  P-COV           PIC X(10).                               EL328
00276      12  FILLER          PIC XX.                                  EL328
00277      12  P-BEN           PIC ZZZ,ZZZ,ZZZ.99-.                     EL328
00278                                                                   EL328
00279  01  COMMENT-LINE.                                                EL328
00280      12  FILLER          PIC X(50)  VALUE SPACES.                 EL328
00281      12  COMM-LINE       PIC X(50).                               EL328
00282                                                                   EL328
00283  01  FACTOR-LINE.                                                 EL328
00284      05  FILLER          PIC X(5)      VALUE SPACES.              EL328
00285      05  FACTOR-OUT      PIC ZZZ.ZZZZ  VALUE ZEROS.               EL328
00286      05  FILLER          PIC X(8)      VALUE ' IS THE '.          EL328
00287      05  FL-AH-OVERRIDE  PIC X(6)      VALUE SPACES.              EL328
00288      05  FILLER          PIC X(19)    VALUE ' PAID CLAIMS FACTOR'.EL328
00289      05  FILLER          PIC X(86)     VALUE SPACES.              EL328
00290                                                                   EL328
00291  01  AVERAGE-CLAIM-LINE.                                          EL328
00292      05  FILLER          PIC X(48)   VALUE '   THE AVERAGE DURATIOEL328
00293 -       'N OF A DISABILITY CLAIM IS'.                             EL328
00294      05  AVERAGE-CLAIM-OUT   PIC ZZZZZ.ZZ VALUE ZEROS.            EL328
00295      05  FILLER              PIC X(7)   VALUE ' MONTHS'.          EL328
00296      05  FILLER          PIC X(48)   VALUE ',  BASED UPON PAID THREL328
00297 -        'U DATE MINUS INCURRED DATE'.                            EL328
00298                                                                   EL328
00299  01  TOT-CLOSE-CLM-LINE.                                          EL328
00300      05  FILLER                    PIC X(3)    VALUE SPACES.      EL328
00301      05  FILLER                    PIC X(20)   VALUE              EL328
00302          'TOTAL NUMBER OF ITD '.                                  EL328
00303      05  TCCL-AH-OVERRIDE          PIC XX      VALUE SPACES.      EL328
00304      05  FILLER                    PIC X(23)   VALUE              EL328
00305         ' CLOSED CLAIMS         '.                                EL328
00306      05  WS-CLOS-CLM-OUT           PIC ZZZ,ZZZ,Z99   VALUE ZERO.  EL328
00307                                                                   EL328
00308  01  AVG-PAID-LINE.                                               EL328
00309      05  FILLER                    PIC X(33)   VALUE              EL328
00310          '   AVERAGE TOTAL PAID AMOUNT PER '.                     EL328
00311      05  APL-AH-OVERRIDE           PIC XX      VALUE SPACES.      EL328
00312      05  FILLER                    PIC X(19)   VALUE              EL328
00313         ' CLOSED CLAIM      '.                                    EL328
00314      05  WS-AVG-PAID-AMT           PIC ZZZ,ZZ9.99   VALUE ZEROS.  EL328
00315                                                                   EL328
00316  01  SUMMARY-LINE.                                                EL328
00317      12  SUMM-NAME                      PIC X(20).                EL328
00318      12  FILLER                         PIC X(10) VALUE SPACES.   EL328
00319      12  LIFE-COUNT                     PIC Z(14)9.               EL328
00320      12  LIFE-PCT  REDEFINES LIFE-COUNT PIC Z(12).99.             EL328
00321      12  LIFE-BEN  REDEFINES LIFE-COUNT PIC ZZZZ,ZZZ,ZZZ.99.      EL328
00322      12  FILLER                         PIC X(10) VALUE SPACES.   EL328
00323      12  AH-COUNT                       PIC Z(14)9.               EL328
00324      12  AH-PCT  REDEFINES AH-COUNT     PIC Z(12).99.             EL328
00325      12  AH-BEN  REDEFINES AH-COUNT     PIC ZZZZ,ZZZ,ZZZ.99.      EL328
00326      12  FILLER                         PIC X(62) VALUE SPACES.   EL328
00327                                                                   EL328
00328                         COPY ELCDTECX.                            EL328
00329                                                                   EL328
00330                         COPY ELCDTEVR.                            EL328
00331  EJECT                                                            EL328
00332  PROCEDURE DIVISION.                                              EL328
00333                                                                   EL328
00334  0100-INITALIZATION.                                              EL328
00335                                                                   EL328
00336  0110-READ-DATE-CARD.                                             EL328
00337      COPY ELCDTERX SUPPRESS.                                      EL328
00338                                                                   EL328
00339  0115-SET-DATES.                                                  EL328
00340                                                                   EL328
00358      MOVE '00'                       TO

00360                                          B-WORK-M                 EL328
00361                                          B-WORK-D.                EL328
00362      MOVE ZEROS                      TO
00363                                          B-WORK-Y.                EL328
00364      MOVE ZEROS                      TO  B-WORK-C.                EL328
00365                                                                   EL328
00366      MOVE RUN-MO                     TO  PDT2-MM                  EL328
00367                                          E-WORK-M.                EL328
00368      MOVE RUN-DA                     TO  PDT2-DD                  EL328
00369                                          E-WORK-D.                EL328
00370      MOVE RUN-YR                     TO  PDT2-YY                  EL328
00371                                          E-WORK-Y.                EL328
00372      MOVE RUN-CC                     TO  E-WORK-C.                EL328
00373                                                                   EL328
00374  0120-SET-HEADING.                                                EL328
00375                                                                   EL328
00376      MOVE COMPANY-NAME      TO HD-COMP.                           EL328
00377      MOVE WS-CURRENT-DATE   TO HD-IPL.                            EL328
00378      MOVE ALPH-DATE         TO HD-DATE.                           EL328
00379      MOVE LIFE-OVERRIDE-L12 TO HDT-LF-OVERRIDE12.                 EL328
00380      MOVE AH-OVERRIDE-L12   TO HDT-AH-OVERRIDE12.                 EL328
00381      MOVE AH-OVERRIDE-L6    TO FL-AH-OVERRIDE.                    EL328
00382      MOVE AH-OVERRIDE-L2    TO TCCL-AH-OVERRIDE                   EL328
00383                                APL-AH-OVERRIDE.                   EL328
00384                                                                   EL328
00385      OPEN INPUT CLAIMS-HIST-IN                                    EL328
00386                 MPPLAN                                            EL328
00387          OUTPUT PRINTER.                                          EL328
00388                                                                   EL328
00389      IF (MPPLAN-FILE-STATUS IS EQUAL TO '00' OR '97' OR '9%'      EL328
               OR '9+')
00390          NEXT SENTENCE                                            EL328
00391      ELSE                                                         EL328
00392          MOVE 'ERROR OCCURED OPEN - MPPLAN'                       EL328
00393                                  TO  WS-ABEND-MESSAGE             EL328
00394          MOVE MPPLAN-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL328
00395          GO TO ABEND-PGM.                                         EL328
00396                                                                   EL328
00397      EJECT                                                        EL328
00398  0130-READ-INPUT.                                                 EL328
00399      READ CLAIMS-HIST-IN  AT END                                  EL328
00400                           GO TO 0220-END-JOB.                     EL328
00401                                                                   EL328
00402      IF HIR-COMPANY-ID LESS DTE-CLIENT                            EL328
00403           GO TO 0130-READ-INPUT.                                  EL328
00404                                                                   EL328
00405      IF HIR-COMPANY-ID GREATER DTE-CLIENT                         EL328
00406           GO TO 0220-END-JOB.                                     EL328
00407                                                                   EL328
00408      IF HIR-RECORD-ID = 'CL'                                      EL328
00409         GO TO 0010-PROCESS-CLAIM.                                 EL328
00410                                                                   EL328
00411      IF HIR-RECORD-ID = 'CM'                                      EL328
00412          GO TO 0020-PROCESS-CERTIFICATE.                          EL328
00413                                                                   EL328
00414      IF HIR-RECORD-ID = 'PM'                                      EL328
00415          GO TO 0025-PROCESS-POLICY.                               EL328
00416                                                                   EL328
00417      IF HIR-RECORD-ID = 'AT'                                      EL328
00418          GO TO 0030-PROCESS-TRAILER.                              EL328
00419                                                                   EL328
00420      GO TO 0130-READ-INPUT.                                       EL328
00421                                                                   EL328
00422  0140-EMPTY-LOOP.                                                 EL328
00423       EXIT.                                                       EL328
00424                                                                   EL328
00425      EJECT                                                        EL328
00426  0010-PROCESS-CLAIM.                                              EL328
00427      MOVE    SPACES                 TO  DETAIL-LINE DENIED-STATUS.EL328
00428      MOVE    HIR-CLAIM-RECORD       TO  CLAIM-MASTER.             EL328
00429 *    PERFORM 0070-ACCUMULATE-CLAIM THRU 0070-EXIT.                EL328
00430                                                                   EL328
00431      IF  P-CARR = CL-CARRIER                                      EL328
00432          GO TO 0020-PROCESS-DENIAL.                               EL328
00433                                                                   EL328
00434      IF P-CARR NOT = 'Z'                                          EL328
00435          PERFORM 0230-PRT-SUMM THRU 0235-EXIT.                    EL328
00436                                                                   EL328
00437      MOVE CL-CARRIER      TO P-CARR.                              EL328
00438                                                                   EL328
00439      PERFORM 0140-EMPTY-LOOP                                      EL328
00440               VARYING CLAS-INDEXCN FROM 1 BY 1                    EL328
00441               UNTIL P-CARR = CARRIER-SUB (CLAS-INDEXCN)           EL328
00442            OR CLAS-INDEXCN GREATER CLAS-MAXCN.                    EL328
00443                                                                   EL328
00444      IF  P-CARR = CARRIER-SUB (CLAS-INDEXCN)                      EL328
00445          MOVE CARRIER-PIC (CLAS-INDEXCN) TO P-CNAM                EL328
00446      ELSE                                                         EL328
00447          MOVE SPACES                     TO P-CNAM.               EL328
00448                                                                   EL328
00449      PERFORM 0170-HEADING-SUB THRU 0180-EXIT.                     EL328
00450                                                                   EL328
00451  0020-PROCESS-DENIAL.                                             EL328
00452                                                                   EL328
00453      PERFORM 0070-ACCUMULATE-CLAIM THRU 0070-EXIT.                EL328
00454                                                                   EL328
00455      IF CL-INCURRED-DT = SPACES OR LOW-VALUES                     EL328
00456          MOVE SPACES  TO P-INCURRED-DT                            EL328
00457      ELSE                                                         EL328
00458          MOVE CL-INCURRED-DT           TO DC-BIN-DATE-1           EL328
00459          MOVE SPACES                   TO DC-OPTION-CODE          EL328
00460          PERFORM 0600-DATE-RTN         THRU 0600-EXIT             EL328
00461          MOVE DC-GREG-DATE-1-EDIT      TO P-INCURRED-DT.          EL328
00462                                                                   EL328
00463      IF CL-REPORTED-DT = SPACES OR LOW-VALUES                     EL328
00464          MOVE SPACES  TO P-REPORTED-DT                            EL328
00465      ELSE                                                         EL328
00466          MOVE    CL-REPORTED-DT        TO  DC-BIN-DATE-1          EL328
00467          MOVE    SPACES                TO  DC-OPTION-CODE         EL328
00468          PERFORM 0600-DATE-RTN        THRU 0600-EXIT              EL328
00469          MOVE    DC-GREG-DATE-1-EDIT   TO  P-REPORTED-DT.         EL328
00470                                                                   EL328
00471      MOVE CL-CLAIM-NO          TO P-CLAIM.                        EL328
           MOVE CL-CERT-ACCOUNT      TO P-ACCT
00472      MOVE CL-CERT-NO           TO P-CERT.                         EL328
00473      MOVE CL-INSURED-LAST-NAME TO P-LNAME.                        EL328
00474      MOVE CL-INSURED-1ST-NAME  TO P-FNAME.                        EL328
00475                                                                   EL328
00476      IF  CL-CLAIM-TYPE = AH-OVERRIDE-L1                           EL328
00477          MOVE AH-OVERRIDE-L2   TO P-TYPE                          EL328
00478      ELSE                                                         EL328
00479          MOVE LIFE-OVERRIDE-L2 TO P-TYPE.                         EL328
00480                                                                   EL328
00481      MOVE CL-TOTAL-PAID-AMT  TO P-AMT.                            EL328
00482      GO TO 0130-READ-INPUT.                                       EL328
00483                                                                   EL328
00484      EJECT                                                        EL328
00485  0020-PROCESS-CERTIFICATE.                                        EL328
00486      MOVE HIR-CERTIFICATE-RECORD  TO  CERTIFICATE-MASTER.         EL328
00487      PERFORM 0080-ACCUMULATE-CERT THRU 0080-EXIT.                 EL328
00488      GO TO 0130-READ-INPUT.                                       EL328
00489                                                                   EL328
00490  0025-PROCESS-POLICY.                                             EL328
00491                                                                   EL328
UNIX  *    MOVE HIR-POLICY-RECORD      TO  POLICY-MASTER.               EL328
00493      PERFORM 0090-ACCUMULATE-POLICY THRU 0090-EXIT.               EL328
00494      GO TO 0130-READ-INPUT.                                       EL328
00495                                                                   EL328
00496      EJECT                                                        EL328
00497  0030-PROCESS-TRAILER.                                            EL328
00498      MOVE HIR-ACTIVITY-TRAILER-RECORD TO  ACTIVITY-TRAILERS.      EL328
00499                                                                   EL328
00500      IF PAYMENT-TR                                                EL328
00501         PERFORM 0075-ACCUMULATE-TRAILER THRU 0075-EXIT.           EL328
00502                                                                   EL328
00503      IF NOT DENIAL-TR                                             EL328
00504          GO TO 0130-READ-INPUT.                                   EL328
00505                                                                   EL328
PEMTST*    IF CL-CERT-STATE NOT = 'OH'
           IF CL-CERT-ACCOUNT NOT = '0000600109'
              GO TO 0130-READ-INPUT
           END-IF
00506      IF CL-LAST-CLOSE-REASON IS NOT EQUAL TO '2'                  EL328
00507          GO TO 0130-READ-INPUT.                                   EL328
00508                                                                   EL328
00509      IF (AT-RETRACTION-DT IS NOT EQUAL TO LOW-VALUES AND SPACES)  EL328
00510          GO TO 0130-READ-INPUT.                                   EL328
00511                                                                   EL328
PEMTST*    IF (AT-DENIAL-DT = LOW-VALUES OR  SPACES)
00513 *        GO TO 0130-READ-INPUT.                                   EL328
00514                                                                   EL328
00515      MOVE AT-DENIAL-DT       TO  DC-BIN-DATE-1.                   EL328
00516      MOVE SPACE              TO  DC-OPTION-CODE.                  EL328
00517      PERFORM 0600-DATE-RTN THRU 0600-EXIT.                        EL328
00518      MOVE DC-GREG-DATE-1-YMD TO  PLAY-DATE.                          CL**2
00519      MOVE DC-ALPHA-CEN-N     TO  PLAY-DATE (1:2).                 EL328
00520                                                                   EL328
00521      IF PLAY-DATE NOT LESS    BEGIN-DATE  AND                     EL328
00522                   NOT GREATER END-DATE                            EL328
00523         NEXT SENTENCE                                             EL328
00524      ELSE                                                         EL328
00525         GO TO 0130-READ-INPUT.                                    EL328
00526                                                                   EL328
00527      MOVE AT-DENIAL-REASON-CODE TO P-CODE.                        EL328
00528                                                                   EL328
00529      IF AT-DENIAL-DT = SPACES OR LOW-VALUES                       EL328
00530          MOVE SPACES  TO P-DENIAL-DT                              EL328
00531      ELSE                                                         EL328
00532          MOVE AT-DENIAL-DT         TO  DC-BIN-DATE-1              EL328
00533          MOVE SPACES               TO  DC-OPTION-CODE             EL328
00534          PERFORM 0600-DATE-RTN    THRU 0600-EXIT                  EL328
00535          MOVE DC-GREG-DATE-1-EDIT  TO  P-DENIAL-DT.               EL328
00536                                                                   EL328
00537      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                     EL328
00538          GO TO 0030-PROCESS-CONV-TRAILER.                         EL328
00539                                                                   EL328
00540      IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                            EL328
00541         COMPUTE WS-REM-TERM =                                     EL328
00542                 (CM-AH-ORIG-TERM - (WS-INC-MY - WS-EFF-MY))       EL328
00543         COMPUTE TOT-AH-BEN =                                      EL328
00544                 (CM-AH-BENEFIT-AMT * WS-REM-TERM)                 EL328
00545         ADD  TOT-AH-BEN TO TOT-EXP-A                              EL328
00546         MOVE TOT-AH-BEN TO P-BEN                                  EL328
00547      ELSE                                                         EL328
00548         PERFORM 0060-FIND-LIFE-BENEFIT THRU 0060-EXIT             EL328
00549         ADD     TOT-LF-BEN              TO  TOT-EXP-L             EL328
00550         MOVE    TOT-LF-BEN              TO  P-BEN.                EL328
00551                                                                   EL328
00552      MOVE SPACE TO X.                                             EL328
00553                                                                   EL328
00554      IF LIN GREATER 60                                            EL328
00555           PERFORM 0170-HEADING-SUB THRU 0180-EXIT.                EL328
00556                                                                   EL328
00557      IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                            EL328
00558          PERFORM 0140-EMPTY-LOOP                                  EL328
00559                  VARYING CLAS-INDEXA FROM CLAS-STARTA BY 1        EL328
00560                  UNTIL CM-AH-BENEFIT-CD =                         EL328
00561                        CLAS-I-BEN (CLAS-INDEXA)                   EL328
00562                     OR CLAS-INDEXA = CLAS-MAXA                    EL328
00563          IF  CM-AH-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXA)          EL328
00564              MOVE CLAS-I-AB10 (CLAS-INDEXA) TO P-COV.             EL328
00565                                                                   EL328
00566      IF CL-CLAIM-TYPE = LIFE-OVERRIDE-L1                          EL328
00567          PERFORM 0140-EMPTY-LOOP                                  EL328
00568                  VARYING CLAS-INDEXL FROM CLAS-STARTL BY 1        EL328
00569                  UNTIL CM-LF-BENEFIT-CD =                         EL328
00570                        CLAS-I-BEN (CLAS-INDEXL)                   EL328
00571                     OR CLAS-INDEXL = CLAS-MAXL                    EL328
00572         IF  CM-LF-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXL)           EL328
00573             MOVE CLAS-I-AB10 (CLAS-INDEXL) TO P-COV.              EL328
00574                                                                   EL328
00575      IF  NOT DENIED-2                                             EL328
00576          MOVE    '2'         TO  DENIED-STATUS                    EL328
00577          MOVE    '-'         TO  X                                EL328
00578          MOVE    DETAIL-LINE TO  P-DATA                           EL328
00579          PERFORM 0190-PRINT THRU 0200-EXIT                        EL328
00580          ADD     +3 TO LIN                                        EL328
00581          IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                        EL328
00582              ADD TOT-AH-BEN TO DEN-BEN-A                          EL328
00583              ADD 1          TO CLM-DEN-A                          EL328
00584          ELSE                                                     EL328
00585              ADD CM-LF-BENEFIT-AMT TO DEN-BEN-L                   EL328
00586              ADD 1                 TO CLM-DEN-L.                  EL328
00587                                                                   EL328
00588      GO TO 0030-FINISH-PROCESS-TRAILER.                           EL328
00589                                                                   EL328
00590  0030-PROCESS-CONV-TRAILER.                                       EL328
00591                                                                   EL328
00592      IF CL-CLAIM-TYPE IS EQUAL TO AH-OVERRIDE-L1                  EL328
00593          COMPUTE WS-REM-TERM =                                    EL328
00594              (PM-LOAN-TERM - (WS-INC-MY - WS-EFF-MY))             EL328
00595          COMPUTE TOT-AH-BEN =                                     EL328
00596              (PM-INS-MONTH-BENEFIT * WS-REM-TERM)                 EL328
00597          ADD TOT-AH-BEN       TO  TOT-EXP-A                       EL328
00598          MOVE TOT-AH-BEN      TO  P-BEN                           EL328
00599      ELSE                                                         EL328
00600          COMPUTE WS-REM-TERM =                                    EL328
00601              (PM-LOAN-TERM - (WS-INC-MY - WS-EFF-MY))             EL328
00602          IF WS-REM-TERM IS GREATER THAN +0                        EL328
00603              COMPUTE WS-RATIO = (WS-REM-TERM / PM-LOAN-TERM)      EL328
00604              COMPUTE TOT-LF-BEN =                                 EL328
00605                  (WS-RATIO * PM-INS-TOTAL-BENEFIT)                EL328
00606              ADD TOT-LF-BEN   TO  TOT-EXP-L                       EL328
00607              MOVE TOT-LF-BEN  TO  P-BEN                           EL328
00608          ELSE                                                     EL328
00609              MOVE +0          TO  TOT-LF-BEN                      EL328
00610              ADD TOT-LF-BEN   TO  TOT-EXP-L                       EL328
00611              MOVE TOT-LF-BEN  TO  P-BEN.                          EL328
00612                                                                   EL328
00613      MOVE SPACE               TO  X.                              EL328
00614      IF LIN IS GREATER THAN +60                                   EL328
00615          PERFORM 0170-HEADING-SUB THRU 0180-EXIT.                 EL328
00616                                                                   EL328
00617      PERFORM 0100-READ-EMPLAN THRU 0100-EXIT.                     EL328
00618                                                                   EL328
00619      IF NOT DENIED-2                                              EL328
00620          MOVE '2'                         TO  DENIED-STATUS       EL328
00621          MOVE '-'                         TO  X                   EL328
00622          MOVE DETAIL-LINE                 TO  P-DATA              EL328
00623          PERFORM 0190-PRINT THRU 0200-EXIT                        EL328
00624          ADD +3                           TO  LIN                 EL328
00625          IF CL-CLAIM-TYPE IS EQUAL TO AH-OVERRIDE-L1              EL328
00626              ADD TOT-AH-BEN               TO  DEN-BEN-L           EL328
00627              ADD 1                        TO  CLM-DEN-A           EL328
00628          ELSE                                                     EL328
00629              ADD PM-INS-TOTAL-BENEFIT     TO  DEN-BEN-L           EL328
00630              ADD 1                        TO  CLM-DEN-L.          EL328
00631                                                                   EL328
00632  0030-FINISH-PROCESS-TRAILER.                                     EL328
00633                                                                   EL328
00634      MOVE    AT-DENIAL-INFO-1 TO  COMM-LINE.                      EL328
00635      MOVE    ' '              TO  X.                              EL328
00636      MOVE    COMMENT-LINE     TO  P-DATA.                         EL328
00637      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
00638      ADD     +1               TO  LIN.                            EL328
00639      MOVE    AT-DENIAL-INFO-2 TO COMM-LINE.                       EL328
00640      MOVE    COMMENT-LINE     TO P-DATA.                          EL328
           IF AT-DENIAL-INFO-2 NOT = SPACES AND LOW-VALUES
              PERFORM 0190-PRINT THRU 0200-EXIT
              ADD     +1               TO  LIN
           END-IF
           
00641      GO TO 0130-READ-INPUT.                                       EL328
00642                                                                   EL328
00643  EJECT                                                            EL328
00644  0060-FIND-LIFE-BENEFIT.                                          EL328
00645      PERFORM 0140-EMPTY-LOOP                                      EL328
00646              VARYING CLAS-INDEXL FROM CLAS-STARTL BY 1            EL328
00647              UNTIL   CM-LF-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXL)  EL328
00648                   OR CLAS-INDEXL = CLAS-MAXL.                     EL328
00649                                                                   EL328
00650      IF  CM-LF-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXL)              EL328
00651          IF  CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'              EL328
00652              MOVE CM-LF-BENEFIT-AMT TO TOT-LF-BEN                 EL328
00653              GO TO 0060-EXIT.                                     EL328
00654                                                                   EL328
00655      COMPUTE WS-REM-TERM =                                        EL328
00656              (CM-LF-ORIG-TERM - (WS-INC-MY - WS-EFF-MY)).         EL328
00657                                                                   EL328
00658      IF  WS-REM-TERM GREATER THAN +0                              EL328
00659          IF CM-LF-ORIG-TERM GREATER THAN +0                       EL328
00660             COMPUTE WS-RATIO = (WS-REM-TERM / CM-LF-ORIG-TERM)    EL328
00661             COMPUTE TOT-LF-BEN =                                  EL328
00662                     (WS-RATIO * CM-LF-BENEFIT-AMT)                EL328
00663          ELSE                                                     EL328
00664             MOVE +0 TO TOT-LF-BEN                                 EL328
00665      ELSE                                                         EL328
00666          MOVE +0 TO TOT-LF-BEN.                                   EL328
00667                                                                   EL328
00668  0060-EXIT.                                                       EL328
00669      EXIT.                                                        EL328
00670      EJECT                                                        EL328
00671  0070-ACCUMULATE-CLAIM.                                           EL328
00672                                                                   EL328
00673      MOVE ' '                TO  RANGE-SW.                        EL328
00674                                                                   EL328
00675      MOVE CL-FILE-ESTABLISH-DT TO  DC-BIN-DATE-1.                 EL328
00676      MOVE SPACE              TO  DC-OPTION-CODE.                  EL328
00677      PERFORM 0600-DATE-RTN THRU 0600-EXIT.                        EL328
00678      MOVE DC-GREG-DATE-1-YMD TO  PLAY-DATE.                          CL**2
00679      MOVE DC-ALPHA-CEN-N     TO  PLAY-DATE (1:2).                 EL328
00680                                                                   EL328
00681      IF PLAY-DATE NOT LESS    BEGIN-DATE  AND                     EL328
00682                   NOT GREATER END-DATE                            EL328
00683         MOVE 'X'             TO  RANGE-SW                         EL328
00684         IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                         EL328
00685            ADD +1 TO CLM-SUB-A                                    EL328
00686         ELSE                                                      EL328
00687            ADD +1 TO CLM-SUB-L.                                   EL328
00688                                                                   EL328
00689      IF PLAY-DATE NOT LESS    BEGIN-DATE  AND                     EL328
00690                   NOT GREATER END-DATE                            EL328
00691         IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                         EL328
00692            IF CLAIM-IS-OPEN  AND                                  EL328
00693               CL-TOTAL-PAID-AMT = +0                              EL328
00694                  ADD 1 TO CLM-PND-A                               EL328
00695            ELSE                                                   EL328
00696               NEXT SENTENCE                                       EL328
00697         ELSE                                                      EL328
00698            IF CLAIM-IS-OPEN  AND                                  EL328
00699               CL-TOTAL-PAID-AMT = +0                              EL328
00700               ADD 1 TO CLM-PND-L.                                 EL328
00701                                                                   EL328
00702      IF CLAIM-IS-CLOSED                                           EL328
00703          IF (CL-LAST-CLOSE-DT EQUAL LOW-VALUES OR SPACES)         EL328
00704              IF (CL-LAST-MAINT-DT EQUAL LOW-VALUES AND SPACES)    EL328
00705                  MOVE CL-FILE-ESTABLISH-DT   TO  CL-LAST-CLOSE-DT EL328
00706              ELSE                                                 EL328
00707                  MOVE CL-LAST-MAINT-DT       TO  CL-LAST-CLOSE-DT.EL328
00708                                                                   EL328
00709      MOVE CL-LAST-CLOSE-DT   TO  DC-BIN-DATE-1.                   EL328
00710      MOVE SPACE              TO  DC-OPTION-CODE.                  EL328
00711      PERFORM 0600-DATE-RTN THRU 0600-EXIT.                        EL328
00712      MOVE DC-GREG-DATE-1-YMD TO  PLAY-DATE.                       EL328
00713      MOVE DC-ALPHA-CEN-N     TO  PLAY-DATE (1:2).                 EL328
00714                                                                   EL328
00715      IF CLAIM-IS-CLOSED                                           EL328
00716         IF PLAY-DATE NOT LESS    BEGIN-DATE  AND                  EL328
00717                      NOT GREATER END-DATE                         EL328
00718            IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                      EL328
00719               ADD +1 TO CLM-CLO-A                                 EL328
00720            ELSE                                                   EL328
00721               ADD +1 TO CLM-CLO-L.                                EL328
00722                                                                   EL328
00723      MOVE    CL-INCURRED-DT      TO  DC-BIN-DATE-1.               EL328
00724      MOVE    SPACE               TO  DC-OPTION-CODE.              EL328
00725      PERFORM 0600-DATE-RTN      THRU 0600-EXIT.                   EL328
00726      MOVE    DC-GREG-DATE-1-YMD  TO  WS-WORK-DATE.                EL328
00727      MOVE    DC-ALPHA-CEN-N      TO  WORK-C.                      EL328
00728      COMPUTE WS-INC-MY = (WORK-CY * 12) + WORK-M.                 EL328
00729                                                                   EL328
00730      IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                            EL328
00731        IF CL-TOTAL-PAID-AMT GREATER THAN +0                       EL328
00732          IF CLAIM-IS-CLOSED                                       EL328
00733            ADD +1                        TO  WS-AH-ITD-CLAIM-COUNTEL328
00734            ADD CL-TOTAL-PAID-AMT         TO  WS-TOTAL-PAID-AMT    EL328
00735            IF CLAIM-WITHIN-RANGE                                  EL328
00736              MOVE CL-INCURRED-DT         TO  DC-BIN-DATE-1        EL328
00737              MOVE CL-PAID-THRU-DT        TO  DC-BIN-DATE-2        EL328
00738              MOVE '1'                    TO  DC-OPTION-CODE       EL328
00739              PERFORM 0600-DATE-RTN  THRU 0600-EXIT                EL328
00740              IF NO-CONVERSION-ERROR                               EL328
00741                ADD +1                    TO  WS-AH-CLAIM-COUNT    EL328
00742                MOVE DC-ELAPSED-MONTHS    TO  WS-CLAIM-DURA        EL328
00743                IF DC-ODD-DAYS-OVER IS GREATER THAN +0             EL328
00744                    COMPUTE HOLD-ODD-DAYS-OVER =                   EL328
00745                        (DC-ODD-DAYS-OVER / +30)                   EL328
00746                    ADD HOLD-ODD-DAYS-OVER    TO  WS-CLAIM-DURA    EL328
00747                    COMPUTE WS-TOT-CLAIM-DURA =                    EL328
00748                        (WS-TOT-CLAIM-DURA + WS-CLAIM-DURA)        EL328
00749                ELSE                                               EL328
00750                    COMPUTE WS-TOT-CLAIM-DURA =                    EL328
00751                        (WS-TOT-CLAIM-DURA + WS-CLAIM-DURA).       EL328
00752                                                                   EL328
00753  0070-EXIT.                                                       EL328
00754      EXIT.                                                        EL328
00755                                                                   EL328
00756      EJECT                                                        EL328
00757  0075-ACCUMULATE-TRAILER.                                         EL328
00758                                                                   EL328
00759      MOVE AT-CHECK-WRITTEN-DT    TO  DC-BIN-DATE-1.               EL328
00760      MOVE SPACE              TO  DC-OPTION-CODE.                  EL328
00761      PERFORM 0600-DATE-RTN THRU 0600-EXIT.                        EL328
00762      MOVE DC-GREG-DATE-1-YMD TO  PLAY-DATE.                       EL328
00763      MOVE DC-ALPHA-CEN-N     TO  PLAY-DATE (1:2).                 EL328
00764                                                                   EL328
00765      IF PLAY-DATE NOT LESS    BEGIN-DATE  AND                     EL328
00766                   NOT GREATER END-DATE                            EL328
00767         IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                         EL328
00768            ADD +1 TO CLM-PAY-A                                    EL328
00769         ELSE                                                      EL328
00770            ADD +1 TO CLM-PAY-L.                                   EL328
00771                                                                   EL328
00772  0075-EXIT.                                                       EL328
00773      EXIT.                                                        EL328
00774                                                                   EL328
00775      EJECT                                                        EL328
00776  0080-ACCUMULATE-CERT.                                            EL328
00777      MOVE    CM-CERT-EFF-DT     TO  DC-BIN-DATE-1.                EL328
00778      MOVE    SPACE              TO  DC-OPTION-CODE.               EL328
00779      PERFORM 0600-DATE-RTN     THRU 0600-EXIT.                    EL328
00780      MOVE    DC-GREG-DATE-1-YMD TO  WS-WORK-DATE.                 EL328
00781      MOVE    DC-ALPHA-CEN-N     TO  WORK-C.                       EL328
00782      COMPUTE WS-EFF-MY = (WORK-CY * 12) + WORK-M.                 EL328
00783                                                                   EL328
00784      IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                            EL328
00785          IF  CL-TOTAL-PAID-AMT GREATER THAN +0                    EL328
00786              IF  CLAIM-IS-CLOSED                                  EL328
00787                  COMPUTE WS-REM-TERM =                            EL328
00788                        (CM-AH-ORIG-TERM - (WS-INC-MY - WS-EFF-MY))EL328
00789                  COMPUTE WS-TOT-REM-TERM =                        EL328
00790                         (WS-TOT-REM-TERM + WS-REM-TERM).          EL328
00791  0080-EXIT.                                                       EL328
00792      EXIT.                                                        EL328
00793      EJECT                                                        EL328
00794  0090-ACCUMULATE-POLICY.                                          EL328
00795                                                                   EL328
00796      MOVE PM-POLICY-EFF-DT       TO  DC-BIN-DATE-1.               EL328
00797      MOVE ' '                    TO  DC-OPTION-CODE.              EL328
00798      PERFORM 0600-DATE-RTN THRU 0600-EXIT.                        EL328
00799      MOVE DC-GREG-DATE-1-YMD     TO  WS-WORK-DATE.                EL328
00800      MOVE DC-ALPHA-CEN-N         TO  WORK-C.                      EL328
00801      COMPUTE WS-EFF-MY = (WORK-CY * 12) + WORK-M.                 EL328
00802                                                                   EL328
00803      IF CL-CLAIM-TYPE IS EQUAL TO AH-OVERRIDE-L1                  EL328
00804          IF CL-TOTAL-PAID-AMT IS GREATER THAN +0                  EL328
00805              IF CLAIM-IS-CLOSED                                   EL328
00806                  COMPUTE WS-REM-TERM =                            EL328
00807                      (PM-LOAN-TERM - (WS-INC-MY - WS-EFF-MY))     EL328
00808                  COMPUTE WS-TOT-REM-TERM =                        EL328
00809                      (WS-TOT-REM-TERM + WS-REM-TERM).             EL328
00810                                                                   EL328
00811  0090-EXIT.                                                       EL328
00812      EXIT.                                                        EL328
00813      EJECT                                                        EL328
00814  0100-READ-EMPLAN.                                                EL328
00815                                                                   EL328
00816      MOVE PM-COMPANY-CD          TO  PP-COMPANY-CD.               EL328
00817      MOVE PM-CARRIER             TO  PP-CARRIER.                  EL328
00818      MOVE PM-GROUPING            TO  PP-GROUPING.                 EL328
00819      MOVE PM-STATE               TO  PP-STATE.                    EL328
00820      MOVE PM-PRODUCER            TO  PP-PRODUCER.                 EL328
00821      MOVE PM-INS-PLAN-CD         TO  PP-PLAN-CODE.                EL328
00822      MOVE PM-INS-PLAN-REVISION   TO  PP-PLAN-REVISION.            EL328
00823                                                                   EL328
00824      READ MPPLAN.                                                 EL328
00825                                                                   EL328
00826      IF MPPLAN-FILE-STATUS IS EQUAL TO '23'                       EL328
00827          MOVE SPACES             TO  P-COV                        EL328
00828          GO TO 0100-EXIT.                                         EL328
00829                                                                   EL328
00830      IF MPPLAN-FILE-STATUS IS NOT EQUAL TO '00'                   EL328
00831          MOVE 'ERROR OCCURED READ - MPPLAN'                       EL328
00832                                  TO  WS-ABEND-MESSAGE             EL328
00833          MOVE MPPLAN-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL328
00834          GO TO ABEND-PGM.                                         EL328
00835                                                                   EL328
00836      MOVE PP-PLAN-ABBREV         TO  P-COV.                       EL328
00837                                                                   EL328
00838  0100-EXIT.                                                       EL328
00839      EXIT.                                                        EL328
00840      EJECT                                                        EL328
00841  0170-HEADING-SUB.                                                EL328
00842      ADD     +1          TO  PG-NO.                               EL328
00843      MOVE    0           TO  LIN.                                 EL328
00844      MOVE    PG-NO       TO  HD-PG.                               EL328
00845      MOVE    HD-1        TO  P-DATA.                              EL328
00846      MOVE    '1'         TO  X.                                   EL328
00847      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
00848      MOVE    SPACE       TO  X.                                   EL328
00849      MOVE    HD-2        TO  P-DATA.                              EL328
00850      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
00851      MOVE    SPACE       TO  X.                                   EL328
00852      MOVE    HD-3        TO  P-DATA.                              EL328
00853      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
00854      MOVE    SPACE       TO  X.                                   EL328
00855      MOVE    HD-4        TO  P-DATA.                              EL328
00856      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
00857      MOVE    SPACE       TO  X.                                   EL328
00858      MOVE    HD-5        TO  P-DATA.                              EL328
00859      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
00860      MOVE    '0'         TO  X.                                   EL328
00861      MOVE    HD-6        TO  P-DATA.                              EL328
00862      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
00863      MOVE    SPACES      TO  PRT  X.                              EL328
00864      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
00865      MOVE    +12         TO  LIN.                                 EL328
00866                                                                   EL328
00867  0180-EXIT.                                                       EL328
00868      EXIT.                                                        EL328
00869                                                                   EL328
00870      EJECT                                                        EL328
00871  0190-PRINT.                                                      EL328
00872                                                                   EL328
00873      IF DTE-FICH NOT = SPACE AND                                  EL328
00874          FICH-OPEN   = SPACE                                      EL328
00875          MOVE 'X' TO FICH-OPEN                                    EL328
00876          OPEN OUTPUT FICH.                                        EL328
00877                                                                   EL328
00878      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL328
00879          IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)      EL328
00880              OPEN I-O ELREPT                                      EL328
00881              IF DTE-F-1 NOT = ZERO AND                            EL328
00882                 DTE-VSAM-FLAGS NOT = '97'                         EL328
00883                  MOVE DTE-VSAM-FLAGS  TO  WS-ABEND-FILE-STATUS    EL328
00884                  MOVE 'ERROR OCCURED OPEN - ELREPT'               EL328
00885                                  TO  WS-ABEND-MESSAGE             EL328
00886                  GO TO ABEND-PGM                                  EL328
00887              ELSE                                                 EL328
00888                  MOVE '1'                   TO REPT-OPEN          EL328
00889                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL328
00890                  MOVE '1'                   TO RF-RECORD-TYPE     EL328
00891                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL328
00892                  MOVE ZERO                  TO RF-LINE-NUMBER     EL328
00893                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL328
00894                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL328
00895                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL328
00896                  MOVE '2'                   TO RF-RECORD-TYPE     EL328
00897                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL328
00898                  MOVE ZERO                  TO RF-LINE-NUMBER     EL328
00899                  START ELREPT  KEY NOT LESS RF-CONTROL-PRIMARY    EL328
00900                  PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT   EL328
00901                  MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD      EL328
00902                  MOVE '1'                   TO RF-RECORD-TYPE     EL328
00903                  MOVE OLC-REPORT-NAME       TO RF-REPORT-ID       EL328
00904                  MOVE SPACES                TO RF-REPORT-LINE-133.EL328
00905                                                                   EL328
00906      IF DTE-ABEND-CD-1 = '81' AND                                 EL328
00907         DTE-PRT-OPT    = 'S'                                      EL328
00908          MOVE +0302  TO WS-RETURN-CODE                            EL328
00909          GO TO ABEND-PGM.                                         EL328
00910                                                                   EL328
00911      IF DTE-PRT-OPT = 'S' OR 'T'                                  EL328
00912          MOVE X      TO RF-CTL-CHAR-133                           EL328
00913          MOVE P-DATA TO RF-DATA-133                               EL328
00914              IF DTE-ABEND-CD-1 = SPACES                           EL328
00915                  ADD +1 TO DTE-TOT-LINES                          EL328
00916                  MOVE DTE-TOT-LINES TO RF-LINE-NUMBER             EL328
00917                  WRITE REPORT-SAVE-FILE                           EL328
00918                      INVALID KEY                                  EL328
00919                          MOVE '88' TO DTE-ABEND-CD-1              EL328
00920                          CLOSE ELREPT                             EL328
00921                          MOVE SPACE TO REPT-OPEN.                 EL328
00922                                                                   EL328
00923      IF DTE-FICH NOT = SPACE                                      EL328
00924          MOVE X TO P-CTL                                          EL328
00925          WRITE FICH-REC FROM PRT.                                 EL328
00926                                                                   EL328
00927      IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'                           EL328
00928          MOVE X TO P-CTL                                          EL328
00929          WRITE PRT.                                               EL328
00930                                                                   EL328
00931      GO TO DTE-PRINT-EXIT.                                        EL328
00932                                                                   EL328
00933  DTE-REPORT-DELETE.                                               EL328
00934      IF DTE-F-1 NOT = ZERO                                        EL328
00935          MOVE ZERO TO DTE-VSAM-FLAGS                              EL328
00936          GO TO DTE-DELETE-EXIT.                                   EL328
00937                                                                   EL328
00938      READ ELREPT   NEXT RECORD                                    EL328
00939            AT END   GO TO DTE-DELETE-EXIT.                        EL328
00940                                                                   EL328
00941      IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND                EL328
00942         OLC-REPORT-NAME       = RF-REPORT-ID                      EL328
00943          DELETE ELREPT RECORD                                     EL328
00944          GO TO DTE-REPORT-DELETE.                                 EL328
00945                                                                   EL328
00946  DTE-DELETE-EXIT.                                                 EL328
00947      EXIT.                                                        EL328
00948                                                                   EL328
00949  DTE-PRINT-EXIT.                                                  EL328
00950      EXIT.                                                        EL328
00951  0200-EXIT.                                                       EL328
00952      EXIT.                                                        EL328
00953  EJECT                                                            EL328
00954  0220-END-JOB.                                                    EL328
00955                                                                   EL328
00956      PERFORM 0230-PRT-SUMM THRU 0235-EXIT.                        EL328
00957      MOVE GRAND-CLM-SUB-L   TO  CLM-SUB-L.                        EL328
00958      MOVE GRAND-CLM-SUB-A   TO  CLM-SUB-A.                        EL328
00959      MOVE GRAND-CLM-CLO-L   TO  CLM-CLO-L.                        EL328
00960      MOVE GRAND-CLM-CLO-A   TO  CLM-CLO-A.                        EL328
00961      MOVE GRAND-CLM-DEN-L   TO  CLM-DEN-L.                        EL328
00962      MOVE GRAND-CLM-DEN-A   TO  CLM-DEN-A.                        EL328
00963      MOVE GRAND-CLM-PAY-L   TO  CLM-PAY-L.                        EL328
00964      MOVE GRAND-CLM-PAY-A   TO  CLM-PAY-A.                        EL328
00965      MOVE GRAND-CLM-PND-L   TO  CLM-PND-L.                        EL328
00966      MOVE GRAND-CLM-PND-A   TO  CLM-PND-A.                        EL328
00967      MOVE GRAND-TOT-EXP-L   TO  TOT-EXP-L.                        EL328
00968      MOVE GRAND-TOT-EXP-A   TO  TOT-EXP-A.                        EL328
00969      MOVE GRAND-DEN-BEN-L   TO  DEN-BEN-L.                        EL328
00970      MOVE GRAND-DEN-BEN-A   TO  DEN-BEN-A.                        EL328
00971      MOVE GR-TOT-CLAIM-DURA TO  WS-TOT-CLAIM-DURA.                EL328
00972      MOVE GR-TOT-REM-TERM   TO  WS-TOT-REM-TERM.                  EL328
00973      MOVE GR-AH-CLAIM-COUNT TO  WS-AH-CLAIM-COUNT.                EL328
00974      MOVE GR-AH-ITD-CLAIM-COUNT TO  WS-AH-ITD-CLAIM-COUNT.        EL328
00975      MOVE GR-TOTAL-PAID-AMT TO  WS-TOTAL-PAID-AMT.                EL328
00976      MOVE SPACES            TO HD-4.                              EL328
00977      MOVE '  GRAND TOTAL  ' TO HD-4.                              EL328
00978      PERFORM 0230-PRT-SUMM THRU 0235-EXIT.                        EL328
00979      PERFORM 8900-CLOSE-FILES.                                    EL328
00980      GOBACK.                                                      EL328
00981                                                                   EL328
00982  0230-PRT-SUMM.                                                   EL328
00983      MOVE ZEROS TO WS-TOT-FACTOR                                  EL328
00984                    WS-AVG-CLAIM-DURA                              EL328
00985                    WS-AVG-PAID-AMT.                               EL328
00986                                                                   EL328
00987      IF WS-TOT-REM-TERM GREATER THAN +0                           EL328
00988         COMPUTE WS-TOT-FACTOR =                                   EL328
00989                 (WS-TOT-CLAIM-DURA / WS-TOT-REM-TERM).            EL328
00990                                                                   EL328
00991      IF WS-AH-CLAIM-COUNT GREATER THAN +0                         EL328
00992         DIVIDE WS-TOT-CLAIM-DURA BY WS-AH-CLAIM-COUNT             EL328
00993             GIVING WS-AVG-CLAIM-DURA                              EL328
00994         COMPUTE WS-AVG-CLAIM-DAY ROUNDED =                        EL328
00995                                      (WS-AVG-CLAIM-DAY * .30).    EL328
00996                                                                   EL328
00997      IF WS-AH-ITD-CLAIM-COUNT GREATER THAN +0                     EL328
00998         DIVIDE WS-TOTAL-PAID-AMT BY WS-AH-ITD-CLAIM-COUNT         EL328
00999                GIVING WS-AVG-PAID-AMT.                            EL328
01000                                                                   EL328
01001      PERFORM 0170-HEADING-SUB THRU 0180-EXIT.                     EL328
01002      MOVE    SPACES            TO  P-DATA.                        EL328
01003      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
01004      MOVE    HD-TOTAL          TO  P-DATA.                        EL328
01005      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
01006      MOVE    SPACES            TO  P-DATA.                        EL328
01007      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
01008      MOVE    'CLAIMS SUBMITTED' TO SUMM-NAME.                     EL328
01009      MOVE    CLM-SUB-L         TO  LIFE-COUNT.                    EL328
01010      MOVE    CLM-SUB-A         TO  AH-COUNT.                      EL328
01011      MOVE    '0'               TO  X.                             EL328
01012      MOVE    SUMMARY-LINE      TO  P-DATA.                        EL328
01013      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
01014      MOVE    'CLAIMS CLOSED   ' TO SUMM-NAME.                     EL328
01015      MOVE    CLM-CLO-L         TO  LIFE-COUNT.                    EL328
01016      MOVE    CLM-CLO-A         TO  AH-COUNT.                      EL328
01017      MOVE    '0'               TO  X.                             EL328
01018      MOVE    SUMMARY-LINE      TO  P-DATA.                        EL328
01019      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
01020      MOVE    'CLAIMS DENIED'   TO  SUMM-NAME.                     EL328
01021      MOVE    CLM-DEN-L         TO  LIFE-COUNT.                    EL328
01022      MOVE    CLM-DEN-A         TO  AH-COUNT.                      EL328
01023      MOVE    '0'               TO  X.                             EL328
01024      MOVE    SUMMARY-LINE      TO  P-DATA.                        EL328
01025      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
01026      MOVE    'CLAIMS PAID'     TO  SUMM-NAME.                     EL328
01027      MOVE    CLM-PAY-L         TO  LIFE-COUNT.                    EL328
01028      MOVE    CLM-PAY-A         TO  AH-COUNT.                      EL328
01029      MOVE    '0'               TO  X.                             EL328
01030      MOVE    SUMMARY-LINE      TO  P-DATA.                        EL328
01031      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
01032      MOVE    'CLAIMS PENDING'  TO  SUMM-NAME.                     EL328
01033      MOVE    CLM-PND-L         TO  LIFE-COUNT.                    EL328
01034      MOVE    CLM-PND-A         TO  AH-COUNT.                      EL328
01035      MOVE    '0'               TO  X.                             EL328
01036      MOVE    SUMMARY-LINE      TO  P-DATA.                        EL328
01037      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
01038      MOVE    'PERCENT DENIED'  TO  SUMM-NAME.                     EL328
01039                                                                   EL328
01040      IF CLM-SUB-L NOT = 0                                         EL328
01041          COMPUTE LIFE-PCT = (CLM-DEN-L * 100) / CLM-SUB-L.        EL328
01042                                                                   EL328
01043      IF CLM-SUB-A NOT = 0                                         EL328
01044          COMPUTE AH-PCT = (CLM-DEN-A * 100) / CLM-SUB-A.          EL328
01045                                                                   EL328
01046      MOVE    '0'                   TO  X.                         EL328
01047      MOVE    SUMMARY-LINE          TO  P-DATA.                    EL328
01048      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
01049                                                                   EL328
01050      MOVE    WS-AVG-CLAIM-DURA  TO  AVERAGE-CLAIM-OUT.            EL328
01051      MOVE    AVERAGE-CLAIM-LINE TO  P-DATA.                       EL328
01052      MOVE    '-'                TO  X.                            EL328
01053      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
01054      MOVE    WS-AH-ITD-CLAIM-COUNT  TO  WS-CLOS-CLM-OUT.          EL328
01055      MOVE    TOT-CLOSE-CLM-LINE TO  P-DATA.                       EL328
01056      MOVE    '-'                TO  X.                            EL328
01057      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
01058      MOVE    AVG-PAID-LINE      TO  P-DATA.                       EL328
01059      MOVE    '-'                TO  X.                            EL328
01060      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
01061      ADD     CLM-SUB-L          TO    GRAND-CLM-SUB-L.            EL328
01062      ADD     CLM-SUB-A          TO    GRAND-CLM-SUB-A.            EL328
01063      ADD     CLM-CLO-L          TO    GRAND-CLM-CLO-L.            EL328
01064      ADD     CLM-CLO-A          TO    GRAND-CLM-CLO-A.            EL328
01065      ADD     CLM-DEN-L          TO    GRAND-CLM-DEN-L.            EL328
01066      ADD     CLM-DEN-A          TO    GRAND-CLM-DEN-A.            EL328
01067      ADD     CLM-PAY-L          TO    GRAND-CLM-PAY-L.            EL328
01068      ADD     CLM-PAY-A          TO    GRAND-CLM-PAY-A.            EL328
01069      ADD     CLM-PND-L          TO    GRAND-CLM-PND-L.            EL328
01070      ADD     CLM-PND-A          TO    GRAND-CLM-PND-A.            EL328
01071      ADD     TOT-EXP-L          TO    GRAND-TOT-EXP-L.            EL328
01072      ADD     TOT-EXP-A          TO    GRAND-TOT-EXP-A.            EL328
01073      ADD     DEN-BEN-L          TO    GRAND-DEN-BEN-L.            EL328
01074      ADD     DEN-BEN-A          TO    GRAND-DEN-BEN-A.            EL328
01075      ADD     WS-TOT-CLAIM-DURA  TO    GR-TOT-CLAIM-DURA.          EL328
01076      ADD     WS-TOT-REM-TERM    TO    GR-TOT-REM-TERM.            EL328
01077      ADD     WS-AH-CLAIM-COUNT  TO    GR-AH-CLAIM-COUNT.          EL328
01078      ADD     WS-AH-ITD-CLAIM-COUNT    TO  GR-AH-ITD-CLAIM-COUNT.  EL328
01079      ADD     WS-TOTAL-PAID-AMT  TO    GR-TOTAL-PAID-AMT.          EL328
01080                                                                   EL328
01081      MOVE ZEROS TO CLM-SUB-L   CLM-SUB-A   CLM-DEN-L   CLM-DEN-A  EL328
01082                    CLM-PAY-L   CLM-PAY-A   CLM-PND-L   CLM-PND-A  EL328
01083                    TOT-EXP-L   TOT-EXP-A   DEN-BEN-L   DEN-BEN-A  EL328
01084                    CLM-CLO-L   CLM-CLO-A   WS-TOT-CLAIM-DURA      EL328
01085                    WS-TOT-REM-TERM  WS-AH-CLAIM-COUNT             EL328
01086                    WS-AH-ITD-CLAIM-COUNT WS-TOTAL-PAID-AMT.       EL328
01087  0235-EXIT.                                                       EL328
01088       EXIT.                                                       EL328
01089                                                                   EL328
01090      EJECT                                                        EL328
01091  0600-DATE-RTN.                                                   EL328
01092      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL328
01093                                                                   EL328
01094      IF DC-ERROR-CODE NOT = SPACE                                 EL328
01095         MOVE ZEROS TO DC-CONVERSION-DATES.                        EL328
01096                                                                   EL328
01097  0600-EXIT.                                                       EL328
01098      EXIT.                                                        EL328
01099                                                                   EL328
01100      EJECT                                                        EL328
01101  8900-CLOSE-FILES SECTION.       COPY ELCPRTCX.                   EL328
01102                                                                   EL328
01103      CLOSE CLAIMS-HIST-IN  PRINTER                                EL328
01104            MPPLAN.                                                EL328
01105                                                                   EL328
unix       MOVE '00'   TO MPPLAN-FILE-STATUS
01106      IF MPPLAN-FILE-STATUS IS NOT EQUAL TO '00'                   EL328
01107          MOVE 'ERROR OCCURED CLOSE - MPPLAN'                      EL328
01108                                  TO  WS-ABEND-MESSAGE             EL328
01109          MOVE MPPLAN-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL328
01110          GO TO ABEND-PGM.                                         EL328
01111                                                                   EL328
01112  ABEND-PGM SECTION. COPY ELCABEND.                                EL328
01113                                                                   EL328
