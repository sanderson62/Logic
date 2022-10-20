       IDENTIFICATION DIVISION.
       PROGRAM-ID. EL328DX.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

00034      SELECT CLAIMS-HIST-IN   ASSIGN TO SYS010-UT-2400-S-SYS010.   EL328
00035      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL328
00036      SELECT PRINTER          ASSIGN TO SYS008-UR-1403-S-SYS008.   EL328
00037      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   EL328
           SELECT EXTR-OUT         ASSIGN TO SYS011
              ORGANIZATION IS LINE SEQUENTIAL.
00049                                                                   EL328
00050  DATA DIVISION.                                                   EL328
00051  FILE SECTION.                                                    EL328
00052                                                                   EL328
00053  FD  FICH                    COPY ELCFCHFD.                       EL328

00056  FD  PRINTER                 COPY ELCPRTFD.                       EL328
00057                                                                   EL328
00059  FD  CLAIMS-HIST-IN          COPY ELCHAF.                         EL328
00060                                                                   EL328
00062  FD  DISK-DATE               COPY ELCDTEFD.                       EL328

       FD  EXTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  DENIAL-RECORD-OUT           PIC X(360).

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
00119  77  WS-EARN-TERM            PIC S9(5)      COMP-3 VALUE +0.      EL328
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
00192                                  COPY ELCCERT.                    EL328

00196                                  COPY ELCARCH.                    EL328

00198                                  COPY ELCMSTR.                    EL328

00200                                  COPY ELCTRLR.                    EL328

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
00226      12  PDT1-MM         PIC XX.                                  EL328
00227      12  FILLER          PIC X     VALUE '/'.                     EL328
00228      12  PDT1-DD         PIC XX.                                  EL328
00229      12  FILLER          PIC X     VALUE '/'.                     EL328
00230      12  PDT1-YY         PIC XX.                                  EL328
00231      12  FILLER          PIC X(6)  VALUE ' THRU '.                EL328
00232      12  PDT2-MM         PIC XX.                                  EL328
00233      12  FILLER          PIC X     VALUE '/'.                     EL328
00234      12  PDT2-DD         PIC XX.                                  EL328
00235      12  FILLER          PIC X     VALUE '/'.                     EL328
00236      12  PDT2-YY         PIC XX.                                  EL328
00237  01  HD-6.                                                        EL328
00238      12  FILLER          PIC X(4)  VALUE SPACES.                  EL328
00239      12  FILLER          PIC X(21) VALUE 'CLAIM NO.  CERT NO.'.   EL328
00240      12  FILLER          PIC X(26) VALUE 'TYPE  CLAIMANT NAME '.  EL328
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
       01  DENIAL-EXTRACT-INIT         PIC X(360).
       01  DENIAL-EXTRACT.
           12  DEN-CARRIER             PIC X.
           12  DEN-TAB1                PIC X.
           12  DEN-CLAIM-NO            PIC X(7).
           12  DEN-TAB2                PIC X.
           12  DEN-CERT-NO             PIC X(11).
           12  DEN-TAB3                PIC X.
           12  DEN-ACCT-NO             PIC X(10).
           12  DEN-TAB4                PIC X.
           12  DEN-PRI-LNAME           PIC X(20).
           12  DEN-TAB5                PIC X.
           12  DEN-PRI-FNAME           PIC X(20).
           12  DEN-TAB6                PIC X.
           12  DEN-JNT-LNAME           PIC X(20).
           12  DEN-TAB7                PIC X.
           12  DEN-JNT-FNAME           PIC X(20).
           12  DEN-TAB8                PIC X.
           12  DEN-PLAN-CODE           PIC X(10).
           12  DEN-TAB9                PIC X.
           12  DEN-COV-TYPE            PIC X(10).
           12  DEN-TAB10               PIC X.
           12  DEN-EFF-DT              PIC X(10).
           12  DEN-TAB11               PIC X.
           12  DEN-CLM-STATUS          PIC X.
           12  DEN-TAB12               PIC X.
           12  DEN-INC-DT              PIC X(10).
           12  DEN-TAB13               PIC X.
           12  DEN-RPT-DT              PIC X(10).
           12  DEN-TAB14               PIC X.
           12  DEN-PROOF-DT            PIC X(10).
           12  DEN-TAB15               PIC X.
           12  DEN-DEN-DT              PIC X(10).
           12  DEN-TAB16               PIC X.
           12  DEN-DENIAL-LINE-1       PIC X(60).
           12  DEN-TAB17               PIC X.
           12  DEN-DENIAL-LINE-2       PIC X(60).
           12  DEN-TAB18               PIC X.
           12  DEN-BEN-AMT             PIC ------99999.99.
           12  DEN-TAB19               PIC X.
           12  DEN-DEN-AMT             PIC ------99999.99.
           12  DEN-TAB20               PIC X.
           12  DEN-EOR                 PIC X.

00254  01  DETAIL-LINE.                                                 EL328
00255      12  FILLER          PIC X(4).                                EL328
00256      12  P-CLAIM         PIC X(7).                                EL328
00257      12  FILLER          PIC XX.                                  EL328
00258      12  P-CERT          PIC X(11).                               EL328
00259      12  FILLER          PIC X.                                   EL328
00260      12  P-TYPE          PIC XX.                                  EL328
00261      12  FILLER          PIC X(4).                                EL328
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
00341      IF DTE-PRC-OPT IS EQUAL TO '2'                               EL328
00342          MOVE RUN-MO                 TO  PDT1-MM                  EL328
00343                                          B-WORK-M                 EL328
00344          MOVE RUN-DA                 TO  PDT1-DD                  EL328
00345                                          B-WORK-D                 EL328
00346          MOVE RUN-YR                 TO  PDT1-YY                  EL328
00347                                          B-WORK-Y                 EL328
00348          MOVE RUN-CC                 TO  B-WORK-C                 EL328
00349          MOVE EP-MO                  TO  PDT2-MM                  EL328
00350                                          E-WORK-M                 EL328
00351          MOVE EP-DA                  TO  PDT2-DD                  EL328
00352                                          E-WORK-D                 EL328
00353          MOVE EP-YR                  TO  PDT2-YY                  EL328
00354                                          E-WORK-Y                 EL328
00355          MOVE EP-CC                  TO  E-WORK-C                 EL328
00356          GO TO 0120-SET-HEADING.                                  EL328
00357                                                                   EL328
00358      MOVE '01'                       TO  PDT1-MM                  EL328
00359                                          PDT1-DD                  EL328
00360                                          B-WORK-M                 EL328
00361                                          B-WORK-D.                EL328
00362      MOVE RUN-YR                     TO  PDT1-YY                  EL328
00363                                          B-WORK-Y.                EL328
00364      MOVE RUN-CC                     TO  B-WORK-C.                EL328
00365                                                                   EL328
00366      MOVE RUN-MO                     TO  PDT2-MM                  EL328
00367                                          E-WORK-M.                EL328
00368      MOVE RUN-DA                     TO  PDT2-DD                  EL328
00369                                          E-WORK-D.                EL328
00370      MOVE RUN-YR                     TO  PDT2-YY                  EL328
00371                                          E-WORK-Y.                EL328
00372      MOVE RUN-CC                     TO  E-WORK-C.                EL328
00373                                                                   EL328
           MOVE SPACES                 TO DENIAL-EXTRACT
           MOVE 'E'                    TO DEN-EOR
           MOVE ZEROS                  TO DEN-DEN-AMT
                                          DEN-BEN-AMT
           MOVE ';'                    TO DEN-TAB1
                                          DEN-TAB2
                                          DEN-TAB3
                                          DEN-TAB4
                                          DEN-TAB5
                                          DEN-TAB6
                                          DEN-TAB7
                                          DEN-TAB8
                                          DEN-TAB9
                                          DEN-TAB10
                                          DEN-TAB11
                                          DEN-TAB12
                                          DEN-TAB13
                                          DEN-TAB14
                                          DEN-TAB15
                                          DEN-TAB16
                                          DEN-TAB17
                                          DEN-TAB18
                                          DEN-TAB19
                                          DEN-TAB20
           MOVE DENIAL-EXTRACT         TO DENIAL-EXTRACT-INIT

           .
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
00387          OUTPUT PRINTER EXTR-OUT
           .

00398  0130-READ-INPUT.                                                 EL328
00399      READ CLAIMS-HIST-IN  AT END                                  EL328
00400                           GO TO 0220-END-JOB.                     EL328
00401                                                                   EL328
00402      IF HIR-COMPANY-ID < DTE-CLIENT
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
00439      PERFORM VARYING CLAS-INDEXCN FROM 1 BY 1 UNTIL
00441         (P-CARR = CARRIER-SUB (CLAS-INDEXCN))
00442         OR (CLAS-INDEXCN > CLAS-MAXCN)
           END-PERFORM
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
00461          MOVE DC-GREG-DATE-1-EDIT      TO P-INCURRED-DT
               MOVE DC-GREG-DATE-A-EDIT      TO DEN-INC-DT
           END-IF
00462                                                                   EL328
00463      IF CL-REPORTED-DT = SPACES OR LOW-VALUES                     EL328
00464          MOVE SPACES  TO P-REPORTED-DT                            EL328
00465      ELSE                                                         EL328
00466          MOVE    CL-REPORTED-DT        TO  DC-BIN-DATE-1          EL328
00467          MOVE    SPACES                TO  DC-OPTION-CODE         EL328
00468          PERFORM 0600-DATE-RTN        THRU 0600-EXIT              EL328
00469          MOVE    DC-GREG-DATE-1-EDIT   TO  P-REPORTED-DT
               MOVE DC-GREG-DATE-A-EDIT      TO DEN-RPT-DT
           END-IF
00470                                                                   EL328
           MOVE CL-CARRIER           TO DEN-CARRIER
           MOVE CL-CLAIM-STATUS      TO DEN-CLM-STATUS
00471      MOVE CL-CLAIM-NO          TO P-CLAIM
                                        DEN-CLAIM-NO
00472      MOVE CL-CERT-NO           TO P-CERT
                                        DEN-CERT-NO
           MOVE CL-CERT-ACCOUNT      TO DEN-ACCT-NO
00473      MOVE CL-INSURED-LAST-NAME TO P-LNAME
                                        DEN-PRI-LNAME
00474      MOVE CL-INSURED-1ST-NAME  TO P-FNAME
                                        DEN-PRI-FNAME
00475                                                                   EL328
00476      IF  CL-CLAIM-TYPE = AH-OVERRIDE-L1                           EL328
00477          MOVE AH-OVERRIDE-L2   TO P-TYPE
                                        DEN-COV-TYPE
00478      ELSE                                                         EL328
00479          MOVE LIFE-OVERRIDE-L2 TO P-TYPE
                                        DEN-COV-TYPE
           END-IF
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
           DISPLAY ' HOW DID I GET HERE 0025-PROCESS-POLICY '
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
00506 *    IF CL-LAST-CLOSE-REASON IS NOT EQUAL TO '2'                  EL328
00507 *        GO TO 0130-READ-INPUT.                                   EL328
00508                                                                   EL328
00509      IF AT-RETRACTION-DT NOT = LOW-VALUES AND SPACES
00510         GO TO 0130-READ-INPUT
           END-IF
00511                                                                   EL328
00512      IF AT-DENIAL-DT = LOW-VALUES OR SPACES
00513         GO TO 0130-READ-INPUT
           END-IF
00514                                                                   EL328
           IF (CL-INCURRED-DT > X'9F5F'
              AND < X'A0E1')
              CONTINUE
           ELSE
00513         GO TO 0130-READ-INPUT
           END-IF
           
00515      MOVE AT-DENIAL-DT       TO  DC-BIN-DATE-1.                   EL328
00516      MOVE SPACE              TO  DC-OPTION-CODE.                  EL328
00517      PERFORM 0600-DATE-RTN THRU 0600-EXIT.                        EL328
00518      MOVE DC-GREG-DATE-1-YMD TO  PLAY-DATE.                          CL**2
           MOVE DC-GREG-DATE-A-EDIT TO DEN-DEN-DT
00519      MOVE DC-ALPHA-CEN-N     TO  PLAY-DATE (1:2).                 EL328

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

00463      IF AT-DENIAL-PROOF-DT = SPACES OR LOW-VALUES
00464         MOVE SPACES              TO DEN-PROOF-DT
00465      ELSE
00466         MOVE AT-DENIAL-PROOF-DT  TO DC-BIN-DATE-1
00467         MOVE SPACES              TO DC-OPTION-CODE
00468         PERFORM 0600-DATE-RTN    THRU 0600-EXIT
              MOVE DC-GREG-DATE-A-EDIT TO DEN-PROOF-DT
           END-IF

           MOVE AT-DENIAL-INFO-1       TO DEN-DENIAL-LINE-1
           MOVE AT-DENIAL-INFO-2       TO DEN-DENIAL-LINE-2

           MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE CL-INCURRED-DT         TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 0600-DATE-RTN       THRU 0600-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-ELAPSED-MONTHS   TO WS-EARN-TERM
           ELSE
              MOVE ZEROS               TO WS-EARN-TERM
           END-IF
           
00540      IF CL-CLAIM-TYPE = AH-OVERRIDE-L1
              COMPUTE WS-REM-TERM = CM-AH-ORIG-TERM - WS-EARN-TERM
00541 *       COMPUTE WS-REM-TERM =
00542 *          (CM-AH-ORIG-TERM - (WS-INC-MY - WS-EFF-MY))
00543         COMPUTE TOT-AH-BEN =
00544            (CM-AH-BENEFIT-AMT * WS-REM-TERM)
00545         ADD  TOT-AH-BEN          TO TOT-EXP-A
00546         MOVE TOT-AH-BEN          TO P-BEN
                                          DEN-DEN-AMT
              MOVE CM-AH-BENEFIT-AMT   TO DEN-BEN-AMT
00547      ELSE
00548         PERFORM 0060-FIND-LIFE-BENEFIT
                                       THRU 0060-EXIT
00549         ADD TOT-LF-BEN           TO TOT-EXP-L
00550         MOVE TOT-LF-BEN          TO P-BEN
                                          DEN-DEN-AMT
              MOVE CM-LF-BENEFIT-AMT   TO DEN-BEN-AMT
           END-IF

00552      MOVE SPACE                  TO X

00554      IF LIN GREATER 60                                            EL328
00555           PERFORM 0170-HEADING-SUB THRU 0180-EXIT.                EL328
00556                                                                   EL328
00557      IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                            EL328
00558         PERFORM VARYING CLAS-INDEXA FROM CLAS-STARTA BY 1 UNTIL
00560            (CM-AH-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXA))
00562            OR (CLAS-INDEXA = CLAS-MAXA)
              END-PERFORM
00563          IF  CM-AH-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXA)          EL328
00564              MOVE CLAS-I-AB10 (CLAS-INDEXA) TO P-COV
                                          DEN-PLAN-CODE
                   MOVE 'A&H'          TO DEN-COV-TYPE
               END-IF
           END-IF
00565                                                                   EL328
00566      IF CL-CLAIM-TYPE = LIFE-OVERRIDE-L1                          EL328
00567         PERFORM VARYING CLAS-INDEXL FROM CLAS-STARTL BY 1 UNTIL
00569            (CM-LF-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXL))
00571            OR (CLAS-INDEXL = CLAS-MAXL)
              END-PERFORM
00572         IF  CM-LF-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXL)           EL328
00573             MOVE CLAS-I-AB10 (CLAS-INDEXL) TO P-COV
                                          DEN-PLAN-CODE
                   MOVE 'LIFE'         TO DEN-COV-TYPE
               END-IF
           END-IF
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
           DISPLAY ' HOW DID I GET HERE 0030-PROCESS-CONV '

           .
00632  0030-FINISH-PROCESS-TRAILER.                                     EL328
00633                                                                   EL328
00634      MOVE    AT-DENIAL-INFO-1 TO  COMM-LINE.                      EL328
00635      MOVE    ' '              TO  X.                              EL328
00636      MOVE    COMMENT-LINE     TO  P-DATA.                         EL328
00637      PERFORM 0190-PRINT THRU 0200-EXIT.                           EL328
00638      ADD     +1               TO  LIN.                            EL328
00639      MOVE    AT-DENIAL-INFO-2 TO COMM-LINE.                       EL328
00640      MOVE    COMMENT-LINE     TO P-DATA.                          EL328
           WRITE DENIAL-RECORD-OUT     FROM DENIAL-EXTRACT
00641      GO TO 0130-READ-INPUT.                                       EL328
00642                                                                   EL328
00643  EJECT                                                            EL328
00644  0060-FIND-LIFE-BENEFIT.                                          EL328
00645      PERFORM VARYING CLAS-INDEXL FROM CLAS-STARTL BY 1 UNTIL
00647         (CM-LF-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXL))
00648         OR (CLAS-INDEXL = CLAS-MAXL)
           END-PERFORM
00649                                                                   EL328
00650      IF  CM-LF-BENEFIT-CD = CLAS-I-BEN (CLAS-INDEXL)              EL328
00651          IF  CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'              EL328
00652              MOVE CM-LF-BENEFIT-AMT TO TOT-LF-BEN                 EL328
00653              GO TO 0060-EXIT.                                     EL328
00654                                                                   EL328
00655 *    COMPUTE WS-REM-TERM =
00656 *       (CM-LF-ORIG-TERM - (WS-INC-MY - WS-EFF-MY))

00655      COMPUTE WS-REM-TERM =
00656         CM-LF-ORIG-TERM - WS-EARN-TERM

00658      IF WS-REM-TERM > +0
00659         IF CM-LF-ORIG-TERM > +0
00660            COMPUTE WS-RATIO = (WS-REM-TERM / CM-LF-ORIG-TERM)
00661            COMPUTE TOT-LF-BEN =
00662               (WS-RATIO * CM-LF-BENEFIT-AMT)
00663         ELSE
00664            MOVE +0               TO TOT-LF-BEN
              END-IF
00665      ELSE
00666         MOVE +0                  TO TOT-LF-BEN
           END-IF

           .
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
           IF CL-CLAIM-NO = '6629085' OR '6C31723' OR '6B31370'
                         OR '6C31834'
              DISPLAY ' INC DATE ' WS-INC-MY ' ' CL-CLAIM-NO
              DISPLAY ' GREG DATE ' DC-GREG-DATE-A-EDIT
           END-IF

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
           MOVE DC-GREG-DATE-A-EDIT      TO DEN-EFF-DT

           
00781      MOVE    DC-ALPHA-CEN-N     TO  WORK-C.                       EL328
00782      COMPUTE WS-EFF-MY = (WORK-CY * 12) + WORK-M.                 EL328

           IF CL-CLAIM-NO = '6629085' OR '6C31723' OR '6B31370'
                         OR '6C31834'
              DISPLAY ' EFF DATE ' WS-EFF-MY ' ' CL-CLAIM-NO
              DISPLAY ' GREG DATE ' DC-GREG-DATE-A-EDIT
           END-IF

00783                                                                   EL328
00784      IF CL-CLAIM-TYPE = AH-OVERRIDE-L1                            EL328
00785          IF  CL-TOTAL-PAID-AMT GREATER THAN +0                    EL328
00786              IF  CLAIM-IS-CLOSED                                  EL328
00787                  COMPUTE WS-REM-TERM =                            EL328
00788                        (CM-AH-ORIG-TERM - (WS-INC-MY - WS-EFF-MY))EL328
00789                  COMPUTE WS-TOT-REM-TERM =                        EL328
00790                         (WS-TOT-REM-TERM + WS-REM-TERM).          EL328
00473      MOVE CM-JT-LAST-NAME      TO P-LNAME
                                        DEN-JNT-LNAME
00474      MOVE CM-JT-FIRST-NAME     TO P-FNAME
                                        DEN-JNT-FNAME
           .
00791  0080-EXIT.                                                       EL328
00792      EXIT.                                                        EL328
00793      EJECT                                                        EL328
00794  0090-ACCUMULATE-POLICY.                                          EL328

           .
00811  0090-EXIT.                                                       EL328
00812      EXIT.                                                        EL328
00813      EJECT                                                        EL328
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
01101  8900-CLOSE-FILES SECTION.

           IF FICH-OPEN NOT = SPACE
              CLOSE FICH EXTR-OUT
           END-IF

01103      CLOSE CLAIMS-HIST-IN  PRINTER                                EL328

           .
01112  ABEND-PGM SECTION. COPY ELCABEND.                                EL328
01113                                                                   EL328
