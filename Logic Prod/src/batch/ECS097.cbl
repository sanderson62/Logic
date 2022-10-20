00001  IDENTIFICATION DIVISION.                                         10/03/97
00002                                                                   ECS097
00003  PROGRAM-ID.                 ECS097.                                 LV001
00004 *              PROGRAM CONVERTED BY                               ECS097
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS097
00006 *              CONVERSION DATE 02/09/96 09:36:31.                 ECS097
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS097
00008 *                            VMOD=2.006.                          ECS097
00009                                                                   ECS097
00010 *AUTHOR.        LOGIC, INC.                                       ECS097
00011 *               DALLAS, TEXAS.                                    ECS097
00012                                                                   ECS097
00013 *DATE-COMPILED.                                                   ECS097
00014                                                                   ECS097
00015 *SECURITY.   *****************************************************ECS097
00016 *            *                                                   *ECS097
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS097
00018 *            *                                                   *ECS097
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS097
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS097
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS097
00022 *            *                                                   *ECS097
00023 *            *****************************************************ECS097
00024                                                                   ECS097
00025 *REMARKS.                                                         ECS097
00026 *        PRINT RESERVE FACTOR TAPE.                               ECS097
00027                                                                   ECS097
00028  ENVIRONMENT DIVISION.                                            ECS097
00029  INPUT-OUTPUT SECTION.                                            ECS097
00030  FILE-CONTROL.                                                    ECS097
00031                                                                   ECS097
00032      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS097
00033      SELECT RESV-TP          ASSIGN TO SYS014-UT-2400-S-SYS014.   ECS097
00034      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS097
00035      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS097
00036  EJECT                                                            ECS097
00037  DATA DIVISION.                                                   ECS097
00038  FILE SECTION.                                                    ECS097
00039                                                                   ECS097
00040  FD  PRNTR                                                        ECS097
00041                              COPY ELCPRTFD.                       ECS097
00042  EJECT                                                            ECS097
00043  FD  RESV-TP                                                      ECS097
00044      BLOCK CONTAINS 0 RECORDS
00045      RECORDING MODE IS F.                                         ECS097
00046                                                                   ECS097
00047  01  RF-REC.                                                      ECS097
00048      12  RF-ID               PIC  XX.                             ECS097
00049      12  RF-CTL.                                                  ECS097
00050          16  RF-CODE.                                             ECS097
00051              20  RF-TBL      PIC  X.                              ECS097
00052              20  RF-INT      PIC  XX.                             ECS097
00053              20  RF-TYPE     PIC  X.                              ECS097
00054      12  RF-INT-ADJ          PIC S9V9(4)     COMP-3.              ECS097
00055      12  RF-RESV-ADJ         PIC S9V9(4)     COMP-3.              ECS097
00056      12  RF-JOINT-ADJ        PIC S9V9(4)     COMP-3.              ECS097
00057      12  RF-PLUS-MINUS       PIC  X.                              ECS097
00058      12  FILLER              PIC  X(24).                          ECS097
00059      12  CX-DX-FACTORS.                                           ECS097
00060          16  RF-CX-FACTOR        OCCURS  100  TIMES               ECS097
00061                              PIC S9(9)V99 COMP-3.                 ECS097
00062          16  RF-DX-FACTOR        OCCURS  100  TIMES               ECS097
00063                              PIC S9(9)V99 COMP-3.                 ECS097
00064  EJECT                                                            ECS097
00065  FD  DISK-DATE                                                    ECS097
00066                              COPY ELCDTEFD.                       ECS097
00067  EJECT                                                            ECS097
00068  FD  FICH                                                         ECS097
00069                              COPY ELCFCHFD.                       ECS097
00070  EJECT                                                            ECS097
00071  WORKING-STORAGE SECTION.                                         ECS097
00072  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS097
00073  01  LCP-CURRENT-DATE-68.                                         ECS097
00074      05  LCP-MONTH                 PIC XX.                        ECS097
00075      05  FILLER                    PIC X VALUE '/'.               ECS097
00076      05  LCP-DAY1                  PIC XX.                        ECS097
00077      05  FILLER                    PIC X VALUE '/'.               ECS097
00078      05  LCP-YEAR                  PIC XX.                        ECS097
00079  01  LCP-DATE-NEW-74.                                             ECS097
00080      05  LCP-YEAR                  PIC XX.                        ECS097
00081      05  LCP-MONTH                 PIC XX.                        ECS097
00082      05  LCP-DAY1                  PIC XX.                        ECS097
00083  77  FILLER  PIC  X(32) VALUE '********************************'. ECS097
00084  77  FILLER  PIC  X(32) VALUE '     ECS097 WORKING-STORAGE     '. ECS097
00085  77  FILLER  PIC  X(32) VALUE '***********VMOD=2.006***********'. ECS097
00086                                                                   ECS097
00087  01  REQUIRED-STORAGE.                                            ECS097
00088      12  WS-RETURN-CODE          PIC S9(4)              COMP.     ECS097
00089      12  WS-ABEND-MESSAGE        PIC  X(80).                      ECS097
00090      12  WS-ABEND-FILE-STATUS    PIC  XX     VALUE ZEROS.         ECS097
00091      12  WS-ZERO                 PIC S9      VALUE ZERO COMP-3.   ECS097
00092                                                                   ECS097
00093  01  MISC.                                                        ECS097
00094      12  X                   PIC  X.                              ECS097
00095      12  W-CTL.                                                   ECS097
00096          16  W-VAL           PIC  X(4).                           ECS097
00097          16  W-PG            PIC S9(5)  COMP-3.                   ECS097
00098          16  W-AGE           PIC  99.                             ECS097
00099          16  W-LT            PIC S999   COMP-3.                   ECS097
00100      12  WS-MORT-CODE.                                            ECS097
00101          16  FILLER          PIC  XXX.                            ECS097
00102          16  WS-MORT-TYPE    PIC  X.                              ECS097
00103      12  SV-DESC             PIC  X(26).                          ECS097
00104      12  ERR-SW              PIC  X              VALUE SPACE.     ECS097
00105  EJECT                                                            ECS097
00106  01  HDR-1.                                                       ECS097
00107      12  FILLER              PIC  X(50)          VALUE SPACE.     ECS097
00108      12  FILLER              PIC  X(25)          VALUE            ECS097
00109              'MORTALITY RESERVE FACTORS'.                         ECS097
00110      12  FILLER              PIC  X(45)          VALUE SPACE.     ECS097
00111      12  FILLER              PIC  X(8)           VALUE 'ECS097'.  ECS097
00112                                                                   ECS097
00113  01  HDR-2.                                                       ECS097
00114      12  FILLER              PIC  X(47)          VALUE SPACE.     ECS097
00115      12  H2-COMP             PIC  X(30).                          ECS097
00116      12  FILLER              PIC  X(43)          VALUE SPACE.     ECS097
00117      12  H2-DATE             PIC  X(8).                           ECS097
00118                                                                   ECS097
00119  01  HDR-3.                                                       ECS097
00120      12  FILLER              PIC  X(53)          VALUE SPACE.     ECS097
00121      12  H3-DATE             PIC  X(18).                          ECS097
00122      12  FILLER              PIC  X(49)          VALUE SPACE.     ECS097
00123      12  FILLER              PIC  X(5)           VALUE 'PAGE '.   ECS097
00124      12  H3-PAGE             PIC ZZ,ZZ9.                          ECS097
00125      12  FILLER              PIC  X(9)           VALUE SPACE.     ECS097
00126                                                                   ECS097
00127  01  HDR-4.                                                       ECS097
00128      12  FILLER              PIC  X.                              ECS097
00129      12  H4-MORT-CODE        PIC  X(4).                           ECS097
00130      12  H4-SLASH            PIC  X              VALUE '/'.       ECS097
00131      12  H4-MORT-CODE-RF     PIC  X(4).                           ECS097
00132      12  FILLER              PIC  X(5)           VALUE SPACE.     ECS097
00133      12  H4-MORT-DESC        PIC  X(26).                          ECS097
00134      12  FILLER              PIC  X(11)          VALUE SPACE.     ECS097
00135      12  H4-TYPE-MSG         PIC  X(30).                          ECS097
00136      12  FILLER              PIC  X(14)          VALUE SPACE.     ECS097
00137      12  H4-PER-MSG          PIC  X(30).                          ECS097
00138      12  FILLER              PIC  X(7)           VALUE SPACE.     ECS097
00139                                                                   ECS097
00140  01  HDR-4-10                PIC  X(30)          VALUE            ECS097
00141          '    COMPUTATION FUNCTIONS     '.                        ECS097
00142                                                                   ECS097
00143  01  DTL-1.                                                       ECS097
00144      12  FILLER              PIC  X(4).                           ECS097
00145      12  D1-AGE              PIC  99.                             ECS097
00146      12  FILLER              PIC  X(4).                           ECS097
00147      12  D1-CX               PIC ZZZ,ZZZ,ZZ9.99.                  ECS097
00148      12  FILLER              PIC  X(6).                           ECS097
00149      12  D1-DX               PIC ZZZ,ZZZ,ZZ9.99.                  ECS097
00150      12  FILLER              PIC  X(6).                           ECS097
00151                                                                   ECS097
00152  01  DTL-2.                                                       ECS097
00153      12  D2-COM              PIC  X(30).                          ECS097
00154      12  D2-AMT              PIC 9.9999-.                         ECS097
00155                                                                   ECS097
00156  01  DTL-3.                                                       ECS097
00157      12  D3-COM              PIC  X(30).                          ECS097
00158      12  D3-CODE             PIC  X.                              ECS097
00159                                                                   ECS097
00160  01  ERR-MESS-1.                                                  ECS097
00161      12  FILLER              PIC  X(30)          VALUE            ECS097
00162              ' INVALID MORTALITY TABLE'.                          ECS097
00163                                                                   ECS097
00164  01  ERR-MESS-2.                                                  ECS097
00165      12  FILLER              PIC  X(30)          VALUE            ECS097
00166              ' ****CHECK DATE FILE****'.                          ECS097
00167  EJECT                                                            ECS097
00168  01  BINARY-AREA         COMP.                                    ECS097
00169      12  X1                  PIC S999            VALUE +0.        ECS097
00170      12  X2                  PIC S999            VALUE +0.        ECS097
00171      12  X3                  PIC S999            VALUE +0.        ECS097
00172      12  X4                  PIC S999            VALUE +0.        ECS097
00173      12  X5                  PIC S999            VALUE +0.        ECS097
00174      12  X6                  PIC S999            VALUE +0.        ECS097
00175      12  B0                  PIC S999            VALUE +0.        ECS097
00176      12  B1                  PIC S999            VALUE +1.        ECS097
00177                                                                   ECS097
00178  01  COMP-3-AREA         COMP-3.                                  ECS097
00179      12  PG-NO               PIC S9(5)           VALUE +1.        ECS097
00180      12  K1                  PIC S9(5)           VALUE +1.        ECS097
00181      12  LT                  PIC S999            VALUE +1.        ECS097
00182      12  WT                  PIC S999            VALUE +1.        ECS097
00183      12  HT                  PIC S999            VALUE +0.        ECS097
00184      12  K9                  PIC S999            VALUE +9.        ECS097
00185      12  LST-PG              PIC S9(5)           VALUE +0.        ECS097
00186      12  LN-CT               PIC S999            VALUE +0.        ECS097
00187      12  LN-MX               PIC S999            VALUE +44.       ECS097
00188      12  K0                  PIC S999            VALUE +0.        ECS097
00189      12  PGM-SUB             PIC S999            VALUE +097.      ECS097
00190  EJECT                                                            ECS097
00191                                                                   ECS097
00192                              COPY ELCDTECX.                       ECS097
00193                                                                   ECS097
00194                              COPY ELCDTEVR.                       ECS097
00195  EJECT                                                            ECS097
00196  PROCEDURE DIVISION.                                              ECS097
00197                                                                   ECS097
00198  0100-SET-START.                                                  ECS097
00199                              COPY ELCDTERX.                       ECS097
00200  EJECT                                                            ECS097
00201  0220-PRINT-FACTORS SECTION.                                      ECS097
00202      OPEN INPUT  RESV-TP                                          ECS097
00203           OUTPUT PRNTR.                                           ECS097
00204                                                                   ECS097
00205      MOVE COMPANY-NAME           TO  H2-COMP.                     ECS097
00206      ACCEPT  LCP-DATE-NEW-74 FROM DATE                            ECS097
00207      MOVE CORRESPONDING LCP-DATE-NEW-74 TO LCP-CURRENT-DATE-68    ECS097
00208      MOVE  LCP-CURRENT-DATE-68 TO H2-DATE.                        ECS097
00209      MOVE ALPH-DATE              TO  H3-DATE.                     ECS097
00210      MOVE K0                     TO  LN-CT PG-NO.                 ECS097
00211  EJECT                                                            ECS097
00212  0240-GET-REC.                                                    ECS097
00213      READ RESV-TP  AT END                                         ECS097
00214          GO TO 0385-END-JOB.                                      ECS097
00215                                                                   ECS097
00216      MOVE +1                     TO  X1.                          ECS097
00217                                                                   ECS097
00218  0260-SET-NEW.                                                    ECS097
00219      MOVE RF-CODE                TO  W-CTL.                       ECS097
00220      MOVE CLAS-STARTM            TO  CLAS-INDEXM.                 ECS097
00221      MOVE RF-CODE                TO  WS-MORT-CODE.                ECS097
00222      MOVE '0'                    TO  WS-MORT-TYPE.                ECS097
00223                                                                   ECS097
00224  0270-FIND-MORT-LOOP.                                             ECS097
00225      IF CLAS-INDEXM  IS GREATER THAN  CLAS-MAXM                   ECS097
00226          MOVE 'UNKNOWN'          TO  SV-DESC                      ECS097
00227          MOVE 'X'                TO  ERR-SW                       ECS097
00228          GO TO 0280-PRINT.                                        ECS097
00229                                                                   ECS097
00230      IF WS-MORT-CODE  IS NOT EQUAL TO                             ECS097
00231          CLAS-MORT-CODE (CLAS-INDEXM)                             ECS097
00232              ADD +1             TO  CLAS-INDEXM                   ECS097
00233              GO TO 0270-FIND-MORT-LOOP.                           ECS097
00234                                                                   ECS097
00235      MOVE CLAS-MORT-DESC (CLAS-INDEXM)                            ECS097
00236                                 TO  SV-DESC.                      ECS097
00237                                                                   ECS097
00238  0280-PRINT.                                                      ECS097
00239      MOVE SPACES                 TO  HDR-4.                       ECS097
00240      MOVE SV-DESC                TO  H4-MORT-DESC.                ECS097
00241      MOVE WS-MORT-CODE           TO  H4-MORT-CODE.                ECS097
00242      MOVE '/'                    TO  H4-SLASH.                    ECS097
00243      MOVE RF-CODE                TO  H4-MORT-CODE-RF.             ECS097
00244      MOVE HDR-4-10               TO  H4-PER-MSG                   ECS097
00245      MOVE ' CX AND DX FACTORS '  TO  H4-TYPE-MSG.                 ECS097
00246                                                                   ECS097
00247  0290-PT-HDNG.                                                    ECS097
00248      ADD +1                      TO  PG-NO.                       ECS097
00249                                                                   ECS097
00250      MOVE PG-NO                  TO  H3-PAGE.                     ECS097
00251      MOVE HDR-1                  TO  PRT.                         ECS097
00252      MOVE '1'                    TO  X.                           ECS097
00253                                                                   ECS097
00254      PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT.                 ECS097
00255                                                                   ECS097
00256      MOVE HDR-2                  TO  PRT.                         ECS097
00257      MOVE ' '                    TO  X.                           ECS097
00258                                                                   ECS097
00259      PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT.                 ECS097
00260                                                                   ECS097
00261      MOVE HDR-3                  TO  PRT.                         ECS097
00262      MOVE ' '                    TO  X.                           ECS097
00263                                                                   ECS097
00264      PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT.                 ECS097
00265                                                                   ECS097
00266      MOVE HDR-4                  TO  PRT.                         ECS097
00267      MOVE '0'                    TO  X.                           ECS097
00268                                                                   ECS097
00269      PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT.                 ECS097
00270                                                                   ECS097
00271      MOVE '    .AGE.   ......CX......      ......DX......'        ECS097
00272                                  TO  PRT.                         ECS097
00273      MOVE ' '                    TO  X.                           ECS097
00274                                                                   ECS097
00275      PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT.                 ECS097
00276                                                                   ECS097
00277      MOVE SPACES                 TO  PRT.                         ECS097
00278      MOVE ' '                    TO  X.                           ECS097
00279                                                                   ECS097
00280      PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT.                 ECS097
00281                                                                   ECS097
00282      MOVE K0                     TO  LN-CT.                       ECS097
00283      MOVE SPACES                 TO  DTL-1.                       ECS097
00284                                                                   ECS097
00285  0310-MAIN-PRINT.                                                 ECS097
00286      IF LN-CT  IS GREATER THAN  44                                ECS097
00287          GO TO 0290-PT-HDNG.                                      ECS097
00288                                                                   ECS097
00289      MOVE RF-CX-FACTOR (X1)      TO  D1-CX.                       ECS097
00290      MOVE RF-DX-FACTOR (X1)      TO  D1-DX.                       ECS097
00291                                                                   ECS097
00292      COMPUTE X2 = X1 - +1.                                        ECS097
00293                                                                   ECS097
00294      MOVE X2                     TO  D1-AGE.                      ECS097
00295      MOVE DTL-1                  TO  PRT.                         ECS097
00296      MOVE ' '                    TO  X.                           ECS097
00297                                                                   ECS097
00298      PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT.                 ECS097
00299                                                                   ECS097
00300      ADD B1                      TO  X1.                          ECS097
00301                                                                   ECS097
00302      IF X1  IS GREATER THAN  +100                                 ECS097
00303          MOVE SPACES             TO  PRT                          ECS097
00304          MOVE ' '                TO  X                            ECS097
00305          PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT              ECS097
00306          MOVE '  INTEREST ADJUSTMENT FACTOR -'                    ECS097
00307                                  TO  D2-COM                       ECS097
00308          MOVE RF-INT-ADJ         TO  D2-AMT                       ECS097
00309          MOVE DTL-2              TO  PRT                          ECS097
00310          MOVE ' '                TO  X                            ECS097
00311          PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT              ECS097
00312          MOVE '  RESERVE TABLE ADJUSTMENT   -'                    ECS097
00313                                  TO  D2-COM                       ECS097
00314          MOVE RF-RESV-ADJ        TO  D2-AMT                       ECS097
00315          MOVE DTL-2              TO  PRT                          ECS097
00316          MOVE ' '                TO  X                            ECS097
00317          PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT              ECS097
00318          MOVE '  JOINT ADJUSTMENT FACTOR    -'                    ECS097
00319                                  TO  D2-COM                       ECS097
00320          MOVE RF-JOINT-ADJ       TO  D2-AMT                       ECS097
00321          MOVE DTL-2              TO  PRT                          ECS097
00322          MOVE ' '                TO  X                            ECS097
00323          PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT              ECS097
00324          MOVE '  PLUS OR MINUS FACTOR       -'                    ECS097
00325                                  TO  D3-COM                       ECS097
00326          MOVE RF-PLUS-MINUS      TO  D3-CODE                      ECS097
00327          MOVE DTL-3              TO  PRT                          ECS097
00328          MOVE ' '                TO  X                            ECS097
00329          PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT              ECS097
00330          GO TO 0240-GET-REC.                                      ECS097
00331                                                                   ECS097
00332      ADD K1                      TO  LN-CT.                       ECS097
00333                                                                   ECS097
00334      IF LN-CT  IS EQUAL TO   5  OR  10  OR  15  OR  20            ECS097
00335                         OR  25  OR  30  OR  35  OR  40            ECS097
00336          MOVE SPACES             TO  PRT                          ECS097
00337          PERFORM 0350-PRT-RTN THRU 0360-PRT-RTN-EXIT.             ECS097
00338                                                                   ECS097
00339      GO TO 0310-MAIN-PRINT.                                       ECS097
00340  EJECT                                                            ECS097
00341  0350-PRT-RTN.                                                    ECS097
00342                              COPY ELCPRT2.                        ECS097
00343                                                                   ECS097
00344  0360-PRT-RTN-EXIT.                                               ECS097
00345      EXIT.                                                        ECS097
00346  EJECT                                                            ECS097
00347  0385-END-JOB.                                                    ECS097
00348                              COPY ELCPRTC.                        ECS097
00349                                                                   ECS097
00350      CLOSE RESV-TP  PRNTR.                                        ECS097
00351                                                                   ECS097
00352  0390-END.                                                        ECS097
00353      GOBACK.                                                      ECS097
00354                                                                   ECS097
00355  ABEND-PGM SECTION.                                               ECS097
00356                              COPY ELCABEND.                       ECS097
