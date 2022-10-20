00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS037
00003  PROGRAM-ID.                ECS037.                                  LV002
00004 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             ECS037
00005 *                           VMOD=2.008                            ECS037
00006 *                                                                 ECS037
00007 *AUTHOR.     LOGIC, INC.                                          ECS037
00008 *            DALLAS, TEXAS.                                       ECS037
00009 *                                                                 ECS037
00010 *DATE-COMPILED.                                                   ECS037
00011                                                                   ECS037
00012 *SECURITY.   *****************************************************ECS037
00013 *            *                                                   *ECS037
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS037
00015 *            *                                                   *ECS037
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS037
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS037
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ECS037
00019 *            *                                                   *ECS037
00020 *            *****************************************************ECS037
00021                                                                   ECS037
00022 *REMARKS.                                                         ECS037
00023 *    THIS PROGRAM SHOULD BE RUN AFTER PERFORMING A RECALC OF      ECS037
00024 *        REINSURANCE SINCE CLAIMS HISTORY IS NOT UPDATED BY A     ECS037
00025 *        RECALC PROCEDURE. ALL REINSURED CLAIM PAYMENTS WILL      ECS037
00026 *        BE RECREATED USING THE NEW TABLES.                       ECS037
00027                                                                   ECS037
00028  ENVIRONMENT DIVISION.                                            ECS037
00029                                                                   ECS037
00030  INPUT-OUTPUT SECTION.                                            ECS037
00031  FILE-CONTROL.                                                    ECS037
00032                                                                   ECS037
00033      SELECT CLMS-HIST-IN    ASSIGN TO SYS010-UT-2400-S-SYS010.    ECS037
00034      SELECT CLMS-HIST-OUT   ASSIGN TO SYS011-UT-2400-S-SYS011.    ECS037
00035      SELECT CERT-FILE       ASSIGN TO SYS012-UT-2400-S-SYS012.    ECS037
00036      SELECT CLMS-WORK       ASSIGN TO SYS015-UT-3380-S-SYS015.    ECS037
00037      SELECT DISK-DATE       ASSIGN TO SYS019-UT-3380-S-SYS019.    ECS037
00038      SELECT SORT-FILE-1     ASSIGN TO SYS001-UT-3380-S-SORTWK1.   ECS037
00039      SELECT SORT-FILE-2     ASSIGN TO SYS001-UT-3380-S-SORTWK1.   ECS037
00040      SELECT ERRTBL-IN       ASSIGN TO SYS014-FBA1-ERRTBL          ECS037
00041                             ACCESS IS DYNAMIC                     ECS037
00042                             ORGANIZATION IS INDEXED               ECS037
00043                             FILE STATUS IS ERRTBL-FILE-STATUS     ECS037
00044                             RECORD KEY IS RE-CONTROL-PRIMARY.     ECS037
00045      SELECT PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.    ECS037
00046      SELECT FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.    ECS037
00047      EJECT                                                        ECS037
00048  DATA DIVISION.                                                   ECS037
00049  FILE SECTION.                                                    ECS037
00050                                                                   ECS037
00051  FD  CLMS-HIST-IN                                                 ECS037
00052      BLOCK CONTAINS 0 RECORDS
00053      RECORDING MODE IS F.                                         ECS037
00054                                                                   ECS037
00055  01  CLAIM-IN        PIC X(510).                                  ECS037
00056                                                                   ECS037
00057                                                                   ECS037
00058  FD  CLMS-HIST-OUT                                                ECS037
00059      BLOCK CONTAINS 0 RECORDS
00060      RECORDING MODE IS F.                                         ECS037
00061                                                                   ECS037
00062  01  CLAIM-OUT       PIC X(510).                                  ECS037
00063                                                                   ECS037
00064                                                                   ECS037
00065  FD  CLMS-WORK                                                    ECS037
00066      BLOCK CONTAINS 0 RECORDS
00067      RECORDING MODE IS F.                                         ECS037
00068                                                                   ECS037
00069  01  CLAIM-WORK      PIC X(510).                                  ECS037
00070                                                                   ECS037
00071      EJECT                                                        ECS037
00072  FD  CERT-FILE                                                    ECS037
00073                              COPY ECSCRIFD.                       ECS037
00074                              COPY ECSCRT01.                       ECS037
00075      EJECT                                                        ECS037
00076                                                                   ECS037
00077  FD  ERRTBL-IN.                                                   ECS037
00078                                                                   ECS037
00079                              COPY ERCREIN.                        ECS037
00080      EJECT                                                        ECS037
00081                                                                   ECS037
00082  FD  DISK-DATE                                                    ECS037
00083                              COPY ELCDTEFD.                       ECS037
00084      EJECT                                                        ECS037
00085                                                                   ECS037
00086  SD  SORT-FILE-1.                                                 ECS037
00087                                                                   ECS037
00088  01  SORT-REC-1.                                                  ECS037
00089      12  FILLER              PIC X(4).                            ECS037
00090      12  SORT-KEY-CNTRL      PIC X(36).                           ECS037
00091      12  FILLER              PIC X(319).                          ECS037
00092      12  SORT-PAID-DATE      PIC X(6).                            ECS037
00093      12  FILLER              PIC X(145).                          ECS037
00094      EJECT                                                        ECS037
00095                                                                   ECS037
00096  SD  SORT-FILE-2.                                                 ECS037
00097                                                                   ECS037
00098  01  SORT-REC.                                                    ECS037
00099      12  S-CLM-REC.                                               ECS037
00100          16 FILLER      PIC X(334).                               ECS037
00101          16 SR-DE-TYPE PIC X.                                     ECS037
00102             88 SR-DE-DTH         VALUE '1'.                       ECS037
00103             88 SR-DE-AH          VALUE '2'.                       ECS037
00104             88 SR-DE-OB-DTH      VALUE '3'.                       ECS037
00105             88 SR-DE-OB-AH       VALUE '4'.                       ECS037
00106          16 FILLER      PIC X(175).                               ECS037
00107      12  SR-CNTRL.                                                ECS037
00108          16 S-REIN      PIC X(6).                                 ECS037
00109          16 S-CARR      PIC X.                                    ECS037
00110          16 S-GROUP     PIC X(6).                                 ECS037
00111          16 S-ST        PIC XX.                                   ECS037
00112          16 S-ACCT      PIC X(10).                                ECS037
00113          16 S-CLAIM     PIC X(7).                                 ECS037
00114          16 S-CLM REDEFINES S-CLAIM.                              ECS037
00115             20  S1-CLM  PIC XX.                                   ECS037
00116             20  S2-CLM  PIC X(5).                                 ECS037
00117          16 S-PAY       PIC 9(11)   COMP-3.                       ECS037
00118                                                                   ECS037
00119      EJECT                                                        ECS037
00120  FD  PRNTR                                                        ECS037
00121                              COPY ELCPRTFD.                       ECS037
00122  01  DTL.                                                         ECS037
00123      12  FILLER      PIC X.                                       ECS037
00124      12  P-CARR      PIC X.                                       ECS037
00125      12  FILLER      PIC X.                                       ECS037
00126      12  P-GRP       PIC X(6).                                    ECS037
00127      12  FILLER      PIC X.                                       ECS037
00128      12  P-ST        PIC XX.                                      ECS037
00129      12  FILLER      PIC X.                                       ECS037
00130      12  P-ACCT      PIC X(10).                                   ECS037
00131      12  FILLER      PIC X.                                       ECS037
00132      12  P-CERT      PIC X(11).                                   ECS037
00133      12  FILLER      PIC XX.                                      ECS037
00134      12  P-EMO       PIC 99.                                      ECS037
00135      12  P-EMOD      PIC X.                                       ECS037
00136      12  P-EDA       PIC 99.                                      ECS037
00137      12  P-EDAD      PIC X.                                       ECS037
00138      12  P-EYR       PIC 99.                                      ECS037
00139      12  FILLER      PIC XX.                                      ECS037
00140      12  P-NAME      PIC X(15).                                   ECS037
00141      12  FILLER      PIC X.                                       ECS037
00142      12  P-TYPE.                                                  ECS037
00143          16  P-TYPE-CON       PIC X(5).                           ECS037
00144          16  P-TYPE-COMP      PIC X(6).                           ECS037
00145      12  FILLER      PIC XX.                                      ECS037
00146      12  P-IMO       PIC 99.                                      ECS037
00147      12  P-IMOD      PIC X.                                       ECS037
00148      12  P-IDA       PIC 99.                                      ECS037
00149      12  P-IDAD      PIC X.                                       ECS037
00150      12  P-IYR       PIC 99.                                      ECS037
00151      12  FILLER      PIC X.                                       ECS037
00152      12  P-PMO       PIC 99.                                      ECS037
00153      12  P-PMOD      PIC X.                                       ECS037
00154      12  P-PDA       PIC 99.                                      ECS037
00155      12  P-PDAD      PIC X.                                       ECS037
00156      12  P-PYR       PIC 99.                                      ECS037
00157      12  FILLER      PIC X.                                       ECS037
00158      12  P-CLMNO     PIC X(7).                                    ECS037
00159      12  FILLER      PIC X.                                       ECS037
00160      12  P-DTH       PIC ZZZZ,ZZZ.99-  BLANK WHEN ZERO.           ECS037
00161      12  P-DIS       PIC ZZZZ,ZZZ.99-  BLANK WHEN ZERO.           ECS037
00162      12  P-CHK-NO    PIC X(7).                                    ECS037
00163                                                                   ECS037
00164      EJECT                                                        ECS037
00165  FD  FICH                                                         ECS037
00166                              COPY ECSFICH.                        ECS037
00167      EJECT                                                        ECS037
00168  WORKING-STORAGE SECTION.                                         ECS037
00169  77  FILLER  PIC X(32) VALUE '********************************'.  ECS037
00170  77  FILLER  PIC X(32) VALUE '     ECS037 WORKING STORAGE     '.  ECS037
00171  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.008 *********'.  ECS037
00172                                                                   ECS037
00173  01  ERRTBL-FILE-STATUS.                                          ECS037
00174      12  ERRTBL-FILE-STATUS-1 PIC X.                              ECS037
00175      12  ERRTBL-FILE-STATUS-2 PIC X.                              ECS037
00176                                                                   ECS037
00177  01  WORK-ABEND-CODE.                                             ECS037
00178      12  WAC-1-2             PIC XX.                              ECS037
00179      12  WAC-3-4             PIC XX.                              ECS037
00180                                                                   ECS037
00181  01  SAVE-DE-REC               PIC X(510).                        ECS037
00182                                                                   ECS037
00183  01  WS.                                                          ECS037
00184      12  ABEND-CODE            PIC X(4) VALUE ZEROS.              ECS037
00185      12  FILLER  REDEFINES ABEND-CODE.                            ECS037
00186          16  ABEND-CODE-1      PIC XX.                            ECS037
00187          16  ABEND-CODE-2      PIC XX.                            ECS037
00188      12  ABEND-OPTION          PIC X VALUE 'Y'.                   ECS037
00189      12  WS-ABEND-MESSAGE      PIC X(80)     VALUE SPACES.        ECS037
00190      12  WS-ABEND-FILE-STATUS  PIC X(4)      VALUE ZERO.          ECS037
00191      12  WS-RETURN-CODE        PIC X(4)      VALUE ZERO.          ECS037
00192      12  WS-ZERO               PIC S999 COMP VALUE +0.            ECS037
00193      12  PGM-SUB               PIC S999 COMP VALUE +037.          ECS037
00194      12  X                     PIC X         VALUE SPACE.         ECS037
00195      12  ERRTBL-SW             PIC 9         VALUE 0.             ECS037
00196          88  ERRTBL-CLOSED                           VALUE 0.     ECS037
00197          88  ERRTBL-OPEN                             VALUE 1.     ECS037
00198      12  FIRST-TIME-SW         PIC 9         VALUE 0.             ECS037
00199          88  FIRST-TIME-THRU                         VALUE 0.     ECS037
00200      12  WS-LINE-COUNT         PIC S999      COMP-3  VALUE +099.  ECS037
00201      12  WS-PAGE-COUNT         PIC S9(5)     COMP-3  VALUE +001.  ECS037
00202      12  TOT-CLM-CNT           PIC S9(7)     COMP-3  VALUE +0.    ECS037
00203      12  TOT-REIN-CNT          PIC S9(7)     COMP-3  VALUE +0.    ECS037
00204      12  TOT-LIFE-AMT          PIC S9(9)V99  COMP-3  VALUE +0.    ECS037
00205      12  TOT-AH-AMT            PIC S9(9)V99  COMP-3  VALUE +0.    ECS037
00206      12  TOT-REIN-LIFE         PIC S9(9)V99  COMP-3  VALUE +0.    ECS037
00207      12  TOT-REIN-AH           PIC S9(9)V99  COMP-3  VALUE +0.    ECS037
00208                                                                   ECS037
00209      12  WS-CR-BIN-DATE        PIC XX.                            ECS037
00210      12  CLAIM-MONTHS          PIC S999      COMP-3.              ECS037
00211      12  VALUATION-DATE        PIC 9(11)     COMP-3.              ECS037
00212      12  VALUATION-DATE-R.                                        ECS037
00213          16  FILLER            PIC 999.                           ECS037
00214          16  VAL-CC            PIC 99.                            ECS037
00215          16  VAL-YR            PIC 99.                            ECS037
00216          16  VAL-MO            PIC 99.                            ECS037
00217          16  VAL-DA            PIC 99.                            ECS037
00218      12  LEVEL-NUM             PIC 999.                           ECS037
00219                                                                   ECS037
00220      12  EOM-RUN-DATE COMP-3.                                     ECS037
00221          16  FILLER            PIC 999.                           ECS037
00222          16  EOM-CC            PIC 99.                            ECS037
00223          16  EOM-YR            PIC 99.                            ECS037
00224          16  EOM-MO            PIC 99.                            ECS037
00225          16  EOM-DA            PIC 99.                            ECS037
00226                                                                   ECS037
00227      12  NDX                   PIC S9        COMP-3.              ECS037
00228      12 S-PAY-R.                                                  ECS037
00229         16  FILLER             PIC 999.                           ECS037
00230         16  S-PY-CCYY          PIC 9(04).                         ECS037
00231         16  S-PY-CCYR REDEFINES S-PY-CCYY.                        ECS037
00232             20  S-PY-CC        PIC 99.                            ECS037
00233             20  S-PY-YR        PIC 99.                            ECS037
00234         16  S-PY-MO            PIC 99.                            ECS037
00235         16  S-PY-DA            PIC 99.                            ECS037
00236                                                                   ECS037
00237  01  TABLE-OF-INCURRED-DATES.                                     ECS037
00238      12  INCURRED-TABLE-ENTRY  OCCURS  5  TIMES.                  ECS037
00239          16  RISK-AMT          PIC S9(9)V99  COMP-3.              ECS037
00240          16  PAID-AMT          PIC S9(9)V99  COMP-3.              ECS037
00241                                                                   ECS037
00242  01  HEAD-1.                                                      ECS037
00243      12  FILLER      PIC X(46) VALUE SPACES.                      ECS037
00244      12  FILLER      PIC X(18) VALUE 'CREATE HISTORY OF'.         ECS037
00245      12  FILLER      PIC X(16) VALUE 'REINSURED CLAIMS'.          ECS037
00246      12  FILLER      PIC X(39) VALUE SPACES.                      ECS037
00247      12  FILLER      PIC X(6)  VALUE 'ECS037'.                    ECS037
00248                                                                   ECS037
00249  01  HEAD-2.                                                      ECS037
00250      12  FILLER      PIC X(47) VALUE SPACES.                      ECS037
00251      12  HD-CLIENT   PIC X(30) VALUE SPACES.                      ECS037
00252      12  FILLER      PIC X(42) VALUE SPACES.                      ECS037
00253      12  HD-DATE     PIC X(8)  VALUE SPACES.                      ECS037
00254                                                                   ECS037
00255  01  HEAD-3.                                                      ECS037
00256      12  FILLER      PIC X(53) VALUE SPACES.                      ECS037
00257      12  HD-ALF-DTE  PIC X(18) VALUE SPACES.                      ECS037
00258      12  FILLER      PIC X(48) VALUE SPACES.                      ECS037
00259      12  FILLER      PIC X(5)  VALUE 'PAGE'.                      ECS037
00260      12  HD-PAGE     PIC ZZ,ZZZ.                                  ECS037
00261                                                                   ECS037
00262  01  HEAD-4.                                                      ECS037
00263      12  FILLER  PIC X(23) VALUE 'C GROUP  ST    ACCOUNT '.       ECS037
00264      12  FILLER  PIC X(12) VALUE '  CERT NO.  '.                  ECS037
00265      12  FILLER  PIC X(9)  VALUE 'EFFECTIVE'.                     ECS037
00266      12  FILLER  PIC X(24) VALUE  '    INSURED          ACT'.     ECS037
00267      12  FILLER  PIC X(25) VALUE 'ION    INCURRED   DATE   '.     ECS037
00268      12  FILLER  PIC X(25) VALUE '  CLAIM   ----AMOUNT OF C'.     ECS037
00269      12  FILLER  PIC X(14) VALUE 'LAIM---- CHECK'.                ECS037
00270                                                                   ECS037
00271  01  HEAD-5.                                                      ECS037
00272      12  FILLER  PIC X(8)  VALUE SPACES.                          ECS037
00273      12  FILLER  PIC X(25) VALUE SPACES.                          ECS037
00274      12  FILLER  PIC X(25) VALUE '    DATE'.                      ECS037
00275      12  FILLER  PIC X(25) VALUE '                   DATE  '.     ECS037
00276      12  FILLER  PIC X(22) VALUE '   PAID    NUMBER'.             ECS037
00277      12  H5-LF-OVRD PIC X(6) VALUE SPACES.                        ECS037
00278      12  FILLER  PIC X(7)  VALUE SPACES.                          ECS037
00279      12  H5-AH-OVRD PIC X(6) VALUE SPACES.                        ECS037
00280      12  FILLER  PIC X(8)  VALUE '  NUMBER'.                      ECS037
00281                                                                   ECS037
00282  01  G-T-HD.                                                      ECS037
00283      12  FILL    PIC X(91)   VALUE SPACE.                         ECS037
00284      12  GT-LOVR PIC X(6).                                        ECS037
00285      12  FILL    PIC X(12)   VALUE SPACE.                         ECS037
00286      12  GT-AOVR PIC X(6).                                        ECS037
00287      12  FILL    PIC X(17)   VALUE SPACE.                         ECS037
00288                                                                   ECS037
00289  01  G-T-LN.                                                      ECS037
00290      12  FILL    PIC X(8)    VALUE SPACE.                         ECS037
00291      12  FILLGT  PIC X(18)   VALUE 'GRAND TOTALS'.                ECS037
00292      12  FILL    PIC X(40)   VALUE SPACE.                         ECS037
00293      12  GT-CNT  PIC Z,ZZZ,ZZ9.                                   ECS037
00294      12  FILLNT  PIC X(12)   VALUE ' ORIGINALS'.                  ECS037
00295      12  GT-DTH  PIC $$$,$$$,$$$.99-.                             ECS037
00296      12  FILLER  PIC X(3)    VALUE SPACES.                        ECS037
00297      12  GT-DIS  PIC $$$,$$$,$$$.99-.                             ECS037
00298                                                                   ECS037
00299      EJECT                                                        ECS037
00300  01  KB-REIN.                                                     ECS037
00301      12  CLM-INCUR-REIN-DATE.                                     ECS037
00302          16  CIRD-CCYY       PIC 9(04).                           ECS037
00303          16  CIRD-CCYR REDEFINES                                  ECS037
00304              CIRD-CCYY.                                           ECS037
00305              20  CIRD-CC     PIC 99.                              ECS037
00306              20  CIRD-YR     PIC 99.                              ECS037
00307          16  CIRD-MO         PIC 99.                              ECS037
00308      12  DTO-CC              PIC 99.                              ECS037
00309      12  DTO-YR              PIC 99.                              ECS037
00310      12  DTO-MO              PIC 99.                              ECS037
00311      12  FLA-CLM-REIN-BASE   PIC S9(5)V99.                        ECS037
00312      12  ADJ-FLA-REIN-BASE   PIC S9(5)V99.                        ECS037
00313      12  REIN-RT-SW          PIC X       VALUE SPACE.             ECS037
00314      12  REIN-FLAG           PIC X       VALUE SPACE.             ECS037
00315                                                                   ECS037
00316                              COPY ECSRITAB.                       ECS037
00317                                                                   ECS037
00318                              COPY ELCPSEVR.                       ECS037
00319      EJECT                                                        ECS037
00320                              COPY ECSEXT01.                       ECS037
00321                                                                   ECS037
00322                              COPY ELCEXTVR.                       ECS037
00323                                                                   ECS037
00324                              COPY ELCCRTVR.                       ECS037
00325                                                                   ECS037
00326      EJECT                                                        ECS037
00327                              COPY ELCDATE.                           CL**2
00328                                                                   ECS037
00329                              COPY ELCDTECX.                       ECS037
00330                                                                   ECS037
00331                              COPY ELCDTEVR.                       ECS037
00332      EJECT                                                        ECS037
00333                                                                   ECS037
00334  PROCEDURE DIVISION.                                              ECS037
00335  0100-SET-START.                                                  ECS037
00336                              COPY ELCDTERX.                       ECS037
00337                                                                   ECS037
00338      MOVE RUN-DATE           TO EOM-RUN-DATE.                     ECS037
00339                                                                   ECS037
00340      MOVE LIFE-OVERRIDE-L6   TO H5-LF-OVRD  GT-LOVR.              ECS037
00341      MOVE   AH-OVERRIDE-L6   TO H5-AH-OVRD  GT-AOVR.              ECS037
00342                                                                   ECS037
00343  0105-INITIALIZE-REIN-DATA.                                       ECS037
00344                                                                   ECS037
00345      MOVE HIGH-VALUES        TO REIN-HOLD-AREAS.                  ECS037
00346      MOVE SPACES             TO REIN-LEVELS-END.                  ECS037
00347      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.              ECS037
00348                                                                   ECS037
00349  0110-DO-SORT-1           SECTION.                                ECS037
00350                                                                   ECS037
00351      OPEN INPUT CLMS-HIST-IN.                                     ECS037
00352                                                                   ECS037
00353      SORT SORT-FILE-1  ASCENDING KEY  SORT-KEY-CNTRL              ECS037
00354                                       SORT-PAID-DATE              ECS037
00355          INPUT PROCEDURE 0120-BYPASS-ALL-REIN                     ECS037
00356          GIVING CLMS-WORK.                                        ECS037
00357                                                                   ECS037
00358      IF SORT-RETURN NOT = ZEROS                                   ECS037
00359          MOVE '0101' TO ABEND-CODE                                ECS037
00360          MOVE '1ST INTERNAL SORT FAILED' TO WS-ABEND-MESSAGE      ECS037
00361          GO TO ABEND-PGM.                                         ECS037
00362                                                                   ECS037
00363      CLOSE   CLMS-HIST-IN.                                        ECS037
00364                                                                   ECS037
00365  0110-DO-SORT-2           SECTION.                                ECS037
00366                                                                   ECS037
00367      SORT SORT-FILE-2  ASCENDING KEY  SR-CNTRL                    ECS037
00368          INPUT PROCEDURE 0130-GEN-NEW-REIN THRU 0170-E-GET-CLAIMS ECS037
00369          OUTPUT PROCEDURE IS 0180-NEW-HIST-OUT  THRU 0180-EXIT.   ECS037
00370                                                                   ECS037
00371      IF SORT-RETURN NOT = ZEROS                                   ECS037
00372          MOVE '0101' TO ABEND-CODE                                ECS037
00373          MOVE '2ND INTERNAL SORT FAILED' TO WS-ABEND-MESSAGE      ECS037
00374          GO TO ABEND-PGM.                                         ECS037
00375                                                                   ECS037
00376  0115-E-NOW-SORT.                                                 ECS037
00377      GO TO 1285-CLOSE.                                            ECS037
00378      EJECT                                                        ECS037
00379                                                                   ECS037
00380  0120-BYPASS-ALL-REIN      SECTION.                               ECS037
00381                                                                   ECS037
00382      READ CLMS-HIST-IN INTO DETAIL-EXTRACT                        ECS037
00383             AT END  GO TO 0120-EXIT.                              ECS037
00384                                                                   ECS037
00385      IF DE-REIN NOT = SPACE                                       ECS037
00386         GO TO 0120-BYPASS-ALL-REIN.                               ECS037
00387                                                                   ECS037
00388      IF DE-CLAIM-AMT NOT NUMERIC  OR                              ECS037
00389         DE-CLAIM-AMT = ZEROS                                      ECS037
00390         GO TO 0120-BYPASS-ALL-REIN.                               ECS037
00391                                                                   ECS037
00392      COPY ELCEXTM1.                                               ECS037
00393                                                                   ECS037
00394      MOVE DE-CLAIM-EXTRACT    TO SORT-REC-1                       ECS037
00395      RELEASE SORT-REC-1                                           ECS037
00396                                                                   ECS037
00397      GO TO 0120-BYPASS-ALL-REIN.                                  ECS037
00398                                                                   ECS037
00399  0120-EXIT.                                                       ECS037
00400      EXIT.                                                        ECS037
00401     EJECT                                                         ECS037
00402                                                                   ECS037
00403  0130-GEN-NEW-REIN        SECTION.                                ECS037
00404  0135-OPEN-FILES.                                                 ECS037
00405                                                                   ECS037
00406      OPEN  INPUT CLMS-WORK                                        ECS037
00407                  CERT-FILE                                        ECS037
00408           OUTPUT CLMS-HIST-OUT                                    ECS037
00409                  PRNTR.                                           ECS037
00410                                                                   ECS037
00411  0160-R-INPUT.                                                    ECS037
00412                                                                   ECS037
00413      READ CLMS-WORK  INTO DETAIL-EXTRACT                          ECS037
00414        AT END  GO TO 0170-E-GET-CLAIMS.                           ECS037
00415                                                                   ECS037
00416      COPY ELCEXTM1.                                               ECS037
00417                                                                   ECS037
00418      MOVE DE-CLAIM-EXTRACT TO SORT-REC.                           ECS037
00419      MOVE DE-CARRIER       TO S-CARR.                             ECS037
00420      MOVE DE-GROUPING      TO S-GROUP.                            ECS037
00421      MOVE SPACES           TO S-REIN.                             ECS037
00422      MOVE DE-STATE         TO S-ST.                               ECS037
00423      MOVE DE-ACCOUNT       TO S-ACCT.                             ECS037
00424      MOVE DE-CNUM          TO S-CLAIM.                            ECS037
00425      MOVE DE-PAID-TO       TO S-PAY                               ECS037
00426                               S-PAY-R.                            ECS037
00427                                                                   ECS037
00428      RELEASE SORT-REC.                                            ECS037
00429                                                                   ECS037
00430      ADD +1 TO TOT-CLM-CNT.                                       ECS037
00431                                                                   ECS037
00432      IF WS-LINE-COUNT GREATER +60                                 ECS037
00433          PERFORM 0800-PRINT-HEADINGS THRU 0800-EXIT.              ECS037
00434                                                                   ECS037
00435      PERFORM 0400-PRINT-DETAIL     THRU 0400-EXIT.                ECS037
00436      PERFORM 0500-MATCH-TO-CERT    THRU 0500-EXIT.                ECS037
00437      PERFORM 0600-GEN-REIN-PORTION THRU 0699-EXIT.                ECS037
00438                                                                   ECS037
00439      GO TO 0160-R-INPUT.                                          ECS037
00440                                                                   ECS037
00441  0170-E-GET-CLAIMS.                                               ECS037
00442      EXIT.                                                        ECS037
00443                                                                   ECS037
00444      EJECT                                                        ECS037
00445  0180-NEW-HIST-OUT        SECTION.                                ECS037
00446                                                                   ECS037
00447      RETURN SORT-FILE-2                                           ECS037
00448         AT END   GO TO 0180-EXIT.                                 ECS037
00449                                                                   ECS037
00450      MOVE S-CLM-REC        TO CLAIM-OUT.                          ECS037
00451      WRITE CLAIM-OUT.                                             ECS037
00452                                                                   ECS037
00453      GO TO 0180-NEW-HIST-OUT.                                     ECS037
00454                                                                   ECS037
00455  0180-EXIT.                                                       ECS037
00456      EXIT.                                                        ECS037
00457      EJECT                                                        ECS037
00458                                                                   ECS037
00459  0400-PRINT-DETAIL         SECTION.                               ECS037
00460                                                                   ECS037
00461      MOVE SPACES           TO DTL.                                ECS037
00462      MOVE DE-STATE         TO P-ST.                               ECS037
00463      MOVE DE-CARRIER       TO P-CARR                              ECS037
00464      MOVE DE-GROUPING      TO P-GRP.                              ECS037
00465      MOVE DE-CERT          TO P-CERT.                             ECS037
00466      MOVE DE-ACCOUNT       TO P-ACCT.                             ECS037
00467      MOVE DE-EF-YR         TO P-EYR.                              ECS037
00468      MOVE DE-EF-MO         TO P-EMO.                              ECS037
00469      MOVE DE-EF-DA         TO P-EDA.                              ECS037
00470      MOVE '-'              TO P-EMOD  P-EDAD.                     ECS037
00471      MOVE DE-LNAME         TO P-NAME.                             ECS037
00472      MOVE 'ORIG-'          TO P-TYPE-CON.                         ECS037
00473                                                                   ECS037
00474      IF DE-DEATH                                                  ECS037
00475          MOVE LIFE-OVERRIDE-L6  TO P-TYPE-COMP                    ECS037
00476        ELSE                                                       ECS037
00477          MOVE   AH-OVERRIDE-L6  TO P-TYPE-COMP.                   ECS037
00478                                                                   ECS037
00479      MOVE DE-CNUM          TO P-CLMNO.                            ECS037
00480      MOVE DE-INCUR-MO      TO P-IMO.                              ECS037
00481      MOVE DE-INCUR-DA      TO P-IDA.                              ECS037
00482      MOVE DE-INCUR-YR      TO P-IYR.                              ECS037
00483      MOVE '-'              TO P-IMOD  P-IDAD.                     ECS037
00484      MOVE DE-PAY-MO        TO P-PMO.                              ECS037
00485      MOVE DE-PAY-DA        TO P-PDA.                              ECS037
00486      MOVE DE-PAY-YR        TO P-PYR.                              ECS037
00487      MOVE '-'              TO P-PMOD  P-PDAD.                     ECS037
00488      MOVE  DE-CHECK        TO P-CHK-NO.                           ECS037
00489                                                                   ECS037
00490      IF DE-DEATH                                                  ECS037
00491         MOVE DE-CLAIM-AMT  TO P-DTH                               ECS037
00492         MOVE ZERO          TO P-DIS                               ECS037
00493         ADD DE-CLAIM-AMT   TO TOT-LIFE-AMT                        ECS037
00494       ELSE                                                        ECS037
00495         MOVE DE-CLAIM-AMT  TO P-DIS                               ECS037
00496         MOVE ZERO          TO P-DTH                               ECS037
00497         ADD DE-CLAIM-AMT   TO TOT-AH-AMT.                         ECS037
00498                                                                   ECS037
00499      MOVE ' '              TO X.                                  ECS037
00500                                                                   ECS037
00501      PERFORM 1270-PRINT-RTN THRU 1270-EXIT.                       ECS037
00502                                                                   ECS037
00503      ADD +1 TO WS-LINE-COUNT.                                     ECS037
00504                                                                   ECS037
00505  0400-EXIT.                                                       ECS037
00506      EXIT.                                                        ECS037
00507      EJECT                                                        ECS037
00508                                                                   ECS037
00509                                                                   ECS037
00510  0500-MATCH-TO-CERT        SECTION.                               ECS037
00511                                                                   ECS037
00512      IF FIRST-TIME-THRU                                           ECS037
00513          MOVE LOW-VALUE TO CR-FULL-CONTROL                        ECS037
00514          MOVE 9               TO FIRST-TIME-SW.                   ECS037
00515                                                                   ECS037
00516      IF CR-FULL-CONTROL NOT LESS DE-CONTROL                       ECS037
00517         GO TO 0500-EXIT.                                          ECS037
00518                                                                   ECS037
00519      READ CERT-FILE                                               ECS037
00520         AT END MOVE HIGH-VALUE TO CR-FULL-CONTROL                 ECS037
00521                GO TO 0500-EXIT.                                   ECS037
00522                                                                   ECS037
00523      COPY ELCCRTM1.                                               ECS037
00524                                                                   ECS037
00525      MOVE SPACE                TO REIN-FLAG.                      ECS037
00526                                                                   ECS037
00527      COMPUTE CR-DTHAMT = CR-DTHAMT + CR-DTHEXP.                   ECS037
00528      COMPUTE CR-DISAMT = CR-DISAMT + CR-DISEXP.                   ECS037
00529                                                                   ECS037
00530      MOVE ZEROS                TO CR-DTHEXP                       ECS037
00531                                   CR-DISEXP.                      ECS037
00532                                                                   ECS037
00533      GO TO 0500-MATCH-TO-CERT.                                    ECS037
00534                                                                   ECS037
00535  0500-EXIT.                                                       ECS037
00536      EXIT.                                                        ECS037
00537      EJECT                                                        ECS037
00538                                                                   ECS037
00539  0600-GEN-REIN-PORTION     SECTION.                               ECS037
00540                                                                   ECS037
00541      IF DE-CONTROL NOT = CR-FULL-CONTROL                          ECS037
00542         MOVE SPACES           TO DTL                              ECS037
00543         MOVE 'NO = CERT'      TO P-TYPE                           ECS037
00544         PERFORM 1270-PRINT-RTN THRU 1270-EXIT                     ECS037
00545         GO TO 0699-EXIT.                                          ECS037
00546                                                                   ECS037
00547      COPY ELCEXTM2.                                               ECS037
00548                                                                   ECS037
00549      MOVE DETAIL-EXTRACT      TO SAVE-DE-REC.                     ECS037
00550                                                                   ECS037
00551      IF REIN-FLAG = 'X'                                           ECS037
00552          GO TO 0630-REINSURE-CLAIM.                               ECS037
00553                                                                   ECS037
00554      MOVE CLAS-STARTL         TO CLAS-INDEXL.                     ECS037
00555      MOVE CLAS-STARTA         TO CLAS-INDEXA.                     ECS037
00556                                                                   ECS037
00557  0610-FIND-LIFE-BENEFIT.                                          ECS037
00558                                                                   ECS037
00559      IF CR-LFTYP = ZERO                                           ECS037
00560          MOVE ZERO            TO CLAS-INDEXL                      ECS037
00561          GO TO 0615-FIND-AH-BENEFIT.                              ECS037
00562                                                                   ECS037
00563  0611-FIND-LIFE-LOOP.                                             ECS037
00564      IF CLAS-INDEXL GREATER CLAS-MAXL OR CLAS-STARTL = ZERO       ECS037
00565          DISPLAY 'LIFE BENEFIT ' CR-LFTYP ' NOT IN TABLE FOR: '   ECS037
00566             CR-FULL-CONTROL                                       ECS037
00567          MOVE 0401            TO WS-RETURN-CODE                   ECS037
00568          GO TO ABEND-PGM.                                         ECS037
00569                                                                   ECS037
00570      IF CR-LFTYP NOT = CLAS-I-BEN (CLAS-INDEXL)                   ECS037
00571          ADD +1 TO CLAS-INDEXL                                    ECS037
00572          GO TO 0611-FIND-LIFE-LOOP.                               ECS037
00573                                                                   ECS037
00574  0615-FIND-AH-BENEFIT.                                            ECS037
00575                                                                   ECS037
00576      IF CR-AHTYP = ZERO                                           ECS037
00577          MOVE ZERO TO CLAS-INDEXA                                 ECS037
00578          GO TO 0620-CALC-REINSURANCE.                             ECS037
00579                                                                   ECS037
00580  0616-FIND-AH-LOOP.                                               ECS037
00581      IF CLAS-INDEXA GREATER CLAS-MAXA OR CLAS-STARTA = ZEROS      ECS037
00582          DISPLAY 'A&H BENEFIT ' CR-AHTYP ' NOT IN TABLE FOR: '    ECS037
00583             CR-FULL-CONTROL                                       ECS037
00584          MOVE 0402 TO WS-RETURN-CODE                              ECS037
00585          GO TO ABEND-PGM.                                         ECS037
00586                                                                   ECS037
00587      IF CR-AHTYP NOT = CLAS-I-BEN (CLAS-INDEXA)                   ECS037
00588          ADD +1 TO CLAS-INDEXA                                    ECS037
00589          GO TO 0616-FIND-AH-LOOP.                                 ECS037
00590                                                                   ECS037
00591  0620-CALC-REINSURANCE.                                           ECS037
00592                                                                   ECS037
00593      PERFORM 0690-REINSURANCE-CALC THRU 0698-EXIT.                ECS037
00594                                                                   ECS037
00595      IF REIN-REM-SW (1) NOT = 'I'                                 ECS037
00596          GO TO 0630-REINSURE-CLAIM.                               ECS037
00597                                                                   ECS037
00598      MOVE ZEROS                    TO RISK-AMT (1)                ECS037
00599                                       PAID-AMT (1).               ECS037
00600      MOVE INCURRED-TABLE-ENTRY (1) TO INCURRED-TABLE-ENTRY (2)    ECS037
00601                                       INCURRED-TABLE-ENTRY (3)    ECS037
00602                                       INCURRED-TABLE-ENTRY (4)    ECS037
00603                                       INCURRED-TABLE-ENTRY (5).   ECS037
00604                                                                   ECS037
00605                                                                   ECS037
00606      MOVE CR-DISAB-INCURRED-DETAIL TO RW-AH-INCURRED-DETAIL.      ECS037
00607                                                                   ECS037
00608      MOVE ZEROS                    TO REIN-AH-CLM-MONTHS          ECS037
00609                                       REIN-AH-PRIOR-CLMS-PAID.    ECS037
00610                                                                   ECS037
00611                                                                   ECS037
00612  0630-REINSURE-CLAIM.                                             ECS037
00613                                                                   ECS037
00614      IF DE-DEATH                                                  ECS037
00615          IF CR-LFTYP = ZEROS                                      ECS037
00616              DISPLAY '***** NO LIFE COV FOR DEATH CLAIM *****'    ECS037
00617                  ' DEXTR CNTL: ' DE-CONTROL ' TYP: ' DE-LF-TYPE.  ECS037
00618      IF DE-DEATH                                                  ECS037
00619          COMPUTE CR-DTHAMT = CR-DTHAMT - DE-CLAIM-AMT             ECS037
00620          IF CR-DTHAMT LESS THAN ZERO                              ECS037
00621              DISPLAY '##### TOTAL DEATH EXCEEDED FOR: '           ECS037
00622                  CR-FULL-CONTROL ' LAST AMT: ' DE-CLAIM-AMT       ECS037
00623                  ' DTH AMT: ' CR-DTHAMT.                          ECS037
00624                                                                   ECS037
00625      IF DE-DISABILITY                                             ECS037
00626          IF CR-AHTYP = ZEROS                                      ECS037
00627              DISPLAY '***** NO  A&H COV FOR DISAB CLAIM *****'    ECS037
00628                  ' DEXTR CNTL: ' DE-CONTROL ' TYP: ' DE-AH-TYPE.  ECS037
00629      IF DE-DISABILITY                                             ECS037
00630          COMPUTE CR-DISAMT = CR-DISAMT - DE-CLAIM-AMT             ECS037
00631          IF CR-DISAMT LESS THAN ZERO                              ECS037
00632              DISPLAY '##### TOTAL DISAB EXCEEDED FOR: '           ECS037
00633                  CR-FULL-CONTROL ' LAST AMT: ' DE-CLAIM-AMT       ECS037
00634                  ' DIS AMT: ' CR-DISAMT.                          ECS037
00635                                                                   ECS037
00636      IF REIN-COMP (1) = SPACES                                    ECS037
00637          GO TO 0699-EXIT.                                         ECS037
00638                                                                   ECS037
00639      IF DE-DEATH                                                  ECS037
00640         MOVE DE-INCUR            TO CR-DTH-DT                     ECS037
00641         MOVE DE-CLAIM-AMT        TO RW-LFCLMWK                    ECS037
00642         MOVE ZERO                TO RW-AHCLMWK                    ECS037
00643       ELSE                                                        ECS037
00644         MOVE DE-INCUR            TO CR-DIS-DT                     ECS037
00645         MOVE DE-CLAIM-AMT        TO RW-AHCLMWK                    ECS037
00646         MOVE ZERO                TO RW-LFCLMWK.                   ECS037
00647                                                                   ECS037
00648      MOVE +0                     TO RW-LFCLM                      ECS037
00649                                     RW-AHCLM.                     ECS037
00650                                                                   ECS037
00651      IF RW-LFCLMWK = ZERO  AND                                    ECS037
00652         RW-AHCLMWK = ZERO                                         ECS037
00653              GO TO 0699-EXIT.                                     ECS037
00654                                                                   ECS037
00655      PERFORM REINSURE-CALC-CLAIM THRU REINSURE-CALC-CLAIM-X       ECS037
00656              VARYING SUB1 FROM +1 BY +1                           ECS037
00657              UNTIL REIN-COMP (SUB1) = SPACES.                     ECS037
00658                                                                   ECS037
00659      PERFORM 0670-REINS-EXTRACT THRU 0679-EXIT                    ECS037
00660              VARYING SUB1 FROM +1 BY +1                           ECS037
00661              UNTIL REIN-COMP (SUB1) = SPACES.                     ECS037
00662                                                                   ECS037
00663      GO TO 0699-EXIT.                                             ECS037
00664                                                                   ECS037
00665                                                                   ECS037
00666  0670-REINS-EXTRACT.                                              ECS037
00667                                                                   ECS037
00668      IF REIN-LF-AH-FLGS (SUB1) = SPACES                           ECS037
00669          GO TO 0679-EXIT.                                         ECS037
00670      IF REIN-REM-SW (SUB1) = 'Z'                                  ECS037
00671          GO TO 0679-EXIT.                                         ECS037
00672                                                                   ECS037
00673      MOVE REIN-WORK-FLDS (SUB1)   TO RWF-FIELDS.                  ECS037
00674      MOVE SAVE-DE-REC             TO DETAIL-EXTRACT.              ECS037
00675      COPY ELCEXTM1.                                               ECS037
00676      MOVE REIN-COMP (SUB1)        TO DE-REI-COMP.                 ECS037
00677      MOVE ZEROES                  TO DE-REI-LFAMT                 ECS037
00678                                      DE-REI-LFPRM                 ECS037
00679                                      DE-REI-AHAMT                 ECS037
00680                                      DE-REI-AHPRM                 ECS037
00681                                      DE-REI-CLAIM-AMT.            ECS037
00682                                                                   ECS037
00683      IF DE-DEATH  OR                                              ECS037
00684         REIN-REM-SW (SUB1) NOT = 'I'                              ECS037
00685          GO TO 0678-CONT-EXTRACT.                                 ECS037
00686                                                                   ECS037
00687      IF RWF-AHAMT NOT GREATER THAN +0                             ECS037
00688          GO TO 0679-EXIT.                                         ECS037
00689                                                                   ECS037
00690      IF DE-INCUR = CR-DIS-INCUR-DT (1)                            ECS037
00691          MOVE +1                  TO NDX                          ECS037
00692          MOVE REIN-AH-CLM-MTH-1   TO CLAIM-MONTHS                 ECS037
00693      ELSE                                                         ECS037
00694      IF DE-INCUR = CR-DIS-INCUR-DT (2)                            ECS037
00695          MOVE +2                  TO NDX                          ECS037
00696          MOVE REIN-AH-CLM-MTH-2   TO CLAIM-MONTHS                 ECS037
00697      ELSE                                                         ECS037
00698          IF DE-INCUR = CR-DIS-INCUR-DT (3)                        ECS037
00699              MOVE +3                  TO NDX                      ECS037
00700              MOVE REIN-AH-CLM-MTH-3   TO CLAIM-MONTHS             ECS037
00701          ELSE                                                     ECS037
00702              IF DE-INCUR = CR-DIS-INCUR-DT (4)                    ECS037
00703                  MOVE +4                  TO NDX                  ECS037
00704                  MOVE REIN-AH-CLM-MTH-4   TO CLAIM-MONTHS         ECS037
00705              ELSE                                                 ECS037
00706                  IF DE-INCUR = CR-DIS-INCUR-DT (5)                ECS037
00707                      MOVE +5                  TO NDX              ECS037
00708                      MOVE REIN-AH-CLM-MTH-5   TO CLAIM-MONTHS     ECS037
00709                  ELSE                                             ECS037
00710                      DISPLAY '***** NO DISAB PERIOD FOR INCUR: '  ECS037
00711                          DE-INCUR                                 ECS037
00712                      MOVE +1                  TO NDX              ECS037
00713                      MOVE REIN-AH-CLM-MTH-1   TO CLAIM-MONTHS.    ECS037
00714                                                                   ECS037
00715      COMPUTE RISK-AMT (NDX) = (RWF-AHAMT * CR-AH-TERM) -          ECS037
00716                               (RS-AH-BEN * CLAIM-MONTHS).         ECS037
00717                                                                   ECS037
00718      IF RISK-AMT (NDX) NOT GREATER THAN +0                        ECS037
00719          MOVE ZEROS               TO RISK-AMT (NDX).              ECS037
00720                                                                   ECS037
00721      IF RISK-AMT (NDX) = ZEROS                                    ECS037
00722          MOVE ZEROS               TO RWF-AHCLML                   ECS037
00723            GO TO 0679-EXIT.                                       ECS037
00724                                                                   ECS037
00725      IF (RWF-AHCLML + PAID-AMT (NDX)) GREATER THAN RISK-AMT (NDX) ECS037
00726        IF RISK-AMT (NDX) = PAID-AMT (NDX)                         ECS037
00727            MOVE ZEROS             TO RWF-AHCLML                   ECS037
00728        ELSE                                                       ECS037
00729            COMPUTE RWF-AHCLML = RISK-AMT (NDX) - PAID-AMT (NDX)   ECS037
00730            MOVE RISK-AMT (NDX)    TO PAID-AMT (NDX)               ECS037
00731            GO TO 0678-CONT-EXTRACT.                               ECS037
00732                                                                   ECS037
00733      ADD RWF-AHCLML               TO PAID-AMT (NDX).              ECS037
00734                                                                   ECS037
00735  0678-CONT-EXTRACT.                                               ECS037
00736                                                                   ECS037
00737      IF REIN-LF-FLG (SUB1) = 'X'                                  ECS037
00738          MOVE RWF-LFAMT           TO DE-REI-LFAMT                 ECS037
00739          MOVE RWF-LFPRM           TO DE-REI-LFPRM                 ECS037
00740          IF DE-DEATH                                              ECS037
00741               MOVE RWF-LFCLML     TO DE-REI-CLAIM-AMT.            ECS037
00742                                                                   ECS037
00743      IF REIN-AH-FLG (SUB1) = 'X'                                  ECS037
00744          MOVE RWF-AHAMT           TO DE-REI-AHAMT                 ECS037
00745          MOVE RWF-AHPRM           TO DE-REI-AHPRM                 ECS037
00746          IF DE-DISABILITY                                         ECS037
00747               MOVE RWF-AHCLML     TO DE-REI-CLAIM-AMT.            ECS037
00748                                                                   ECS037
00749      IF DE-REI-CLAIM-AMT = ZERO                                   ECS037
00750          GO TO 0679-EXIT.                                         ECS037
00751                                                                   ECS037
00752      MOVE 'R'                     TO DE-REIN.                     ECS037
00753      COPY ELCEXTM2.                                               ECS037
00754      MOVE DE-CLAIM-EXTRACT        TO SORT-REC.                    ECS037
00755      MOVE SPACES                  TO DTL.                         ECS037
00756      MOVE DE-CARRIER              TO S-CARR.                      ECS037
00757      MOVE DE-GROUPING             TO S-GROUP.                     ECS037
00758      MOVE ' RE- '                 TO P-TYPE-CON.                  ECS037
00759      MOVE REIN-COMP (SUB1)        TO S-REIN  P-TYPE-COMP.         ECS037
00760      MOVE DE-STATE                TO S-ST.                        ECS037
00761      MOVE DE-ACCOUNT              TO S-ACCT.                      ECS037
00762      MOVE DE-CNUM                 TO S-CLAIM.                     ECS037
00763      MOVE DE-PAID-TO              TO S-PAY                        ECS037
00764                                      S-PAY-R.                     ECS037
00765                                                                   ECS037
00766      RELEASE SORT-REC.                                            ECS037
00767                                                                   ECS037
00768      ADD +1                       TO TOT-REIN-CNT.                ECS037
00769                                                                   ECS037
00770      IF DE-DEATH                                                  ECS037
00771          MOVE  DE-REI-CLAIM-AMT   TO  P-DTH                       ECS037
00772          ADD  DE-REI-CLAIM-AMT    TO  TOT-REIN-LIFE               ECS037
00773        ELSE                                                       ECS037
00774          MOVE  DE-REI-CLAIM-AMT   TO  P-DIS                       ECS037
00775          ADD  DE-REI-CLAIM-AMT    TO  TOT-REIN-AH.                ECS037
00776                                                                   ECS037
00777      MOVE ' '                     TO X.                           ECS037
00778                                                                   ECS037
00779      PERFORM 1270-PRINT-RTN THRU 1270-EXIT.                       ECS037
00780                                                                   ECS037
00781      ADD +1 TO WS-LINE-COUNT.                                     ECS037
00782                                                                   ECS037
00783  0679-EXIT.                                                       ECS037
00784       EXIT.                                                       ECS037
00785                                                                   ECS037
00786  0690-REINSURANCE-CALC.                                           ECS037
00787                                                                   ECS037
00788      MOVE 'X'                   TO REIN-FLAG.                     ECS037
00789                                                                   ECS037
00790      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.              ECS037
00791                                                                   ECS037
00792      IF CR-REIN-TABLE = SPACES OR ZEROS                           ECS037
00793         GO TO 0699-EXIT.                                          ECS037
00794                                                                   ECS037
00795      MOVE CR-REIN-TABLE         TO REIN-SRCH.                     ECS037
00796      MOVE DTE-CLASIC-COMPANY-CD TO REIN-SRCH-COMP-CD.             ECS037
00797                                                                   ECS037
00798      PERFORM RR-READ-REIN THRU RR-READ-REIN-X.                    ECS037
00799                                                                   ECS037
00800      IF RE-REMAINING (1) NOT = 'I'                                ECS037
00801          GO TO REINSURE-ROUTINE-GET-CALC.                         ECS037
00802                                                                   ECS037
00803      MOVE ZEROS                    TO REIN-EARN-LF-TERM           ECS037
00804                                       REIN-EARN-AH-TERM           ECS037
00805                                       REIN-LF-CLM-MONTHS          ECS037
00806                                       REIN-AH-CLM-MTH-1           ECS037
00807                                       REIN-AH-CLM-MTH-2           ECS037
00808                                       REIN-AH-CLM-MTH-3           ECS037
00809                                       REIN-AH-CLM-MTH-4           ECS037
00810                                       REIN-AH-CLM-MTH-5.          ECS037
00811                                                                   ECS037
00812      MOVE CR-YR                    TO DC-YMD-YEAR.                ECS037
00813      MOVE CR-MO                    TO DC-YMD-MONTH.               ECS037
00814      MOVE CR-DA                    TO DC-YMD-DAY.                 ECS037
00815      MOVE CR-CC                    TO DC-ALPHA-CEN-N.             ECS037
00816      MOVE '3'                      TO DC-OPTION-CODE.             ECS037
00817      PERFORM 0700-DATE-CONVERSION-ROUTINE THRU 0799-EXIT.         ECS037
00818      MOVE DC-BIN-DATE-1            TO WS-CR-BIN-DATE.             ECS037
00819                                                                   ECS037
00820      IF CR-LFTYP = ZEROS  OR  SPACES                              ECS037
00821          GO TO 0693-CONTINUE.                                     ECS037
00822                                                                   ECS037
00823      MOVE EOM-RUN-DATE             TO VALUATION-DATE              ECS037
00824                                       VALUATION-DATE-R.           ECS037
00825                                                                   ECS037
00826      IF CR-LF-CANCEL-EXIT-DATE NOT = ZEROS                        ECS037
00827           IF CR-LF-CANCEL-EXIT-DATE  LESS THAN VALUATION-DATE     ECS037
00828              MOVE CR-LF-CANCEL-EXIT-DATE TO VALUATION-DATE        ECS037
00829                                             VALUATION-DATE-R.     ECS037
00830                                                                   ECS037
00831      IF CR-LF-CLAIM-EXIT-DATE NOT = ZEROS                         ECS037
00832           IF CR-LF-CLAIM-EXIT-DATE  LESS THAN VALUATION-DATE      ECS037
00833              MOVE CR-LF-CLAIM-EXIT-DATE  TO VALUATION-DATE        ECS037
00834                                             VALUATION-DATE-R.     ECS037
00835                                                                   ECS037
00836      MOVE VAL-CC                   TO DC-ALPHA-CEN-N.             ECS037
00837      MOVE VAL-YR                   TO DC-YMD-YEAR.                ECS037
00838      MOVE VAL-MO                   TO DC-YMD-MONTH.               ECS037
00839      MOVE VAL-DA                   TO DC-YMD-DAY.                 ECS037
00840      PERFORM 0695-CALC-ELAPSED-MONTHS THRU 0695-CALC-EXIT.        ECS037
00841      IF DC-ELAPSED-MONTHS GREATER THAN CR-LF-TERM                 ECS037
00842          MOVE CR-LF-TERM           TO REIN-EARN-LF-TERM           ECS037
00843      ELSE                                                         ECS037
00844          MOVE DC-ELAPSED-MONTHS    TO REIN-EARN-LF-TERM.          ECS037
00845                                                                   ECS037
00846      IF CR-DTH-DT NOT = ZEROS                                     ECS037
00847          MOVE CR-DTH-CC            TO DC-ALPHA-CEN-N              ECS037
00848          MOVE CR-DTH-YR            TO DC-YMD-YEAR                 ECS037
00849          MOVE CR-DTH-MO            TO DC-YMD-MONTH                ECS037
00850          MOVE CR-DTH-DA            TO DC-YMD-DAY                  ECS037
00851          PERFORM 0695-CALC-ELAPSED-MONTHS THRU 0695-CALC-EXIT     ECS037
00852          MOVE DC-ELAPSED-MONTHS    TO REIN-LF-CLM-MONTHS.         ECS037
00853                                                                   ECS037
00854  0693-CONTINUE.                                                   ECS037
00855                                                                   ECS037
00856      IF CR-AHTYP = ZEROS  OR  SPACES                              ECS037
00857          GO TO REINSURE-ROUTINE-GET-CALC.                         ECS037
00858                                                                   ECS037
00859      MOVE EOM-RUN-DATE             TO VALUATION-DATE              ECS037
00860                                       VALUATION-DATE-R.           ECS037
00861                                                                   ECS037
00862      IF CR-AH-CANCEL-EXIT-DATE NOT = ZEROS                        ECS037
00863           IF CR-AH-CANCEL-EXIT-DATE  LESS THAN VALUATION-DATE     ECS037
00864              MOVE CR-AH-CANCEL-EXIT-DATE TO VALUATION-DATE        ECS037
00865                                             VALUATION-DATE-R.     ECS037
00866                                                                   ECS037
00867      IF CR-AH-SETTLEMENT-EXIT-DATE NOT = ZEROS                    ECS037
00868           IF CR-AH-SETTLEMENT-EXIT-DATE  LESS THAN VALUATION-DATE ECS037
00869              MOVE CR-AH-SETTLEMENT-EXIT-DATE TO VALUATION-DATE    ECS037
00870                                                 VALUATION-DATE-R. ECS037
00871                                                                   ECS037
00872      MOVE VAL-CC                   TO DC-ALPHA-CEN-N.             ECS037
00873      MOVE VAL-YR                   TO DC-YMD-YEAR.                ECS037
00874      MOVE VAL-MO                   TO DC-YMD-MONTH.               ECS037
00875      MOVE VAL-DA                   TO DC-YMD-DAY.                 ECS037
00876      PERFORM 0695-CALC-ELAPSED-MONTHS THRU 0695-CALC-EXIT.        ECS037
00877      IF DC-ELAPSED-MONTHS GREATER THAN CR-AH-TERM                 ECS037
00878          MOVE CR-AH-TERM           TO REIN-EARN-AH-TERM           ECS037
00879      ELSE                                                         ECS037
00880          MOVE DC-ELAPSED-MONTHS    TO REIN-EARN-AH-TERM.          ECS037
00881                                                                   ECS037
00882      IF CR-DIS-INCUR-DT (1) NOT = ZEROS                           ECS037
00883          MOVE CR-DIS-INC-CC (1)    TO DC-ALPHA-CEN-N              ECS037
00884          MOVE CR-DIS-INC-YR (1)    TO DC-YMD-YEAR                 ECS037
00885          MOVE CR-DIS-INC-MO (1)    TO DC-YMD-MONTH                ECS037
00886          MOVE CR-DIS-INC-DA (1)    TO DC-YMD-DAY                  ECS037
00887          PERFORM 0695-CALC-ELAPSED-MONTHS THRU 0695-CALC-EXIT     ECS037
00888          MOVE DC-ELAPSED-MONTHS    TO REIN-AH-CLM-MTH-1.          ECS037
00889                                                                   ECS037
00890      IF CR-DIS-INCUR-DT (2) NOT = ZEROS                           ECS037
00891          MOVE CR-DIS-INC-CC (2)    TO DC-ALPHA-CEN-N              ECS037
00892          MOVE CR-DIS-INC-YR (2)    TO DC-YMD-YEAR                 ECS037
00893          MOVE CR-DIS-INC-MO (2)    TO DC-YMD-MONTH                ECS037
00894          MOVE CR-DIS-INC-DA (2)    TO DC-YMD-DAY                  ECS037
00895          PERFORM 0695-CALC-ELAPSED-MONTHS THRU 0695-CALC-EXIT     ECS037
00896          MOVE DC-ELAPSED-MONTHS    TO REIN-AH-CLM-MTH-2.          ECS037
00897                                                                   ECS037
00898      IF CR-DIS-INCUR-DT (3) NOT = ZEROS                           ECS037
00899          MOVE CR-DIS-INC-CC (3)    TO DC-ALPHA-CEN-N              ECS037
00900          MOVE CR-DIS-INC-YR (3)    TO DC-YMD-YEAR                 ECS037
00901          MOVE CR-DIS-INC-MO (3)    TO DC-YMD-MONTH                ECS037
00902          MOVE CR-DIS-INC-DA (3)    TO DC-YMD-DAY                  ECS037
00903          PERFORM 0695-CALC-ELAPSED-MONTHS THRU 0695-CALC-EXIT     ECS037
00904          MOVE DC-ELAPSED-MONTHS    TO REIN-AH-CLM-MTH-3.          ECS037
00905                                                                   ECS037
00906      IF CR-DIS-INCUR-DT (4) NOT = ZEROS                           ECS037
00907          MOVE CR-DIS-INC-CC (4)    TO DC-ALPHA-CEN-N              ECS037
00908          MOVE CR-DIS-INC-YR (4)    TO DC-YMD-YEAR                 ECS037
00909          MOVE CR-DIS-INC-MO (4)    TO DC-YMD-MONTH                ECS037
00910          MOVE CR-DIS-INC-DA (4)    TO DC-YMD-DAY                  ECS037
00911          PERFORM 0695-CALC-ELAPSED-MONTHS THRU 0695-CALC-EXIT     ECS037
00912          MOVE DC-ELAPSED-MONTHS    TO REIN-AH-CLM-MTH-4.          ECS037
00913                                                                   ECS037
00914      IF CR-DIS-INCUR-DT (5) NOT = ZEROS                           ECS037
00915          MOVE CR-DIS-INC-CC (5)    TO DC-ALPHA-CEN-N              ECS037
00916          MOVE CR-DIS-INC-YR (5)    TO DC-YMD-YEAR                 ECS037
00917          MOVE CR-DIS-INC-MO (5)    TO DC-YMD-MONTH                ECS037
00918          MOVE CR-DIS-INC-DA (5)    TO DC-YMD-DAY                  ECS037
00919          PERFORM 0695-CALC-ELAPSED-MONTHS THRU 0695-CALC-EXIT     ECS037
00920          MOVE DC-ELAPSED-MONTHS    TO REIN-AH-CLM-MTH-5.          ECS037
00921                                                                   ECS037
00922      GO TO REINSURE-ROUTINE-GET-CALC.                             ECS037
00923                                                                   ECS037
00924                                                                   ECS037
00925  0695-CALC-ELAPSED-MONTHS.                                        ECS037
00926                                                                   ECS037
00927      MOVE '3'                      TO DC-OPTION-CODE.             ECS037
00928      PERFORM 0700-DATE-CONVERSION-ROUTINE THRU 0799-EXIT.         ECS037
00929      MOVE DC-BIN-DATE-1            TO DC-BIN-DATE-2.              ECS037
00930      MOVE WS-CR-BIN-DATE           TO DC-BIN-DATE-1.              ECS037
00931      MOVE '1'                      TO DC-OPTION-CODE.             ECS037
00932      MOVE ' '                      TO DC-CENTURY-ADJUSTMENT.      ECS037
00933      MOVE ZEROS                    TO DC-ELAPSED-MONTHS           ECS037
00934                                       DC-ODD-DAYS-OVER            ECS037
00935                                       DC-ELAPSED-DAYS.            ECS037
00936      PERFORM 0700-DATE-CONVERSION-ROUTINE THRU 0799-EXIT.         ECS037
00937                                                                   ECS037
00938  0695-CALC-EXIT.                                                  ECS037
00939      EXIT.                                                        ECS037
00940                                                                   ECS037
00941  EJECT                                                            ECS037
00942  REINSURE-ROUTINE-GET-CALC.                                       ECS037
00943                              COPY ECSRTPFM.                       ECS037
00944                                                                   ECS037
00945                                                                   ECS037
00946      GO TO 0698-EXIT.                                             ECS037
00947                                                                   ECS037
00948  RR-READ-REIN.                                                    ECS037
00949                                                                   ECS037
00950      IF REIN-OPEN-SW = ' '                                        ECS037
00951          MOVE 'X'               TO REIN-OPEN-SW                   ECS037
00952          MOVE 'A'               TO REIN-SRCH-CODE                 ECS037
00953          OPEN INPUT ERRTBL-IN                                     ECS037
00954              IF ERRTBL-FILE-STATUS NOT = '00' AND '97'            ECS037
00955                  MOVE ERRTBL-FILE-STATUS TO WS-ABEND-FILE-STATUS  ECS037
00956                  MOVE 'UNABLE TO OPEN REIN TABLE FILE'            ECS037
00957                                 TO WS-ABEND-MESSAGE               ECS037
00958                  GO TO ABEND-PGM                                  ECS037
00959              ELSE                                                 ECS037
00960                  PERFORM REIN-CO-TABLE-BUILD THRU                 ECS037
00961                          REIN-BUILD-EXIT.                         ECS037
00962                                                                   ECS037
00963      IF REIN-SRCH NOT = SAVE-REIN-SRCH                            ECS037
00964          MOVE REIN-SRCH TO SAVE-REIN-SRCH                         ECS037
00965          MOVE REIN-SEARCH TO RE-CONTROL-PRIMARY                   ECS037
00966          READ ERRTBL-IN                                           ECS037
00967              IF ERRTBL-FILE-STATUS = '23'                         ECS037
00968                  DISPLAY 'INVALID REINSURANCE TABLE CODE - '      ECS037
00969                            REIN-SRCH ' ' CR-FULL-CONTROL          ECS037
00970                  MOVE ERRTBL-FILE-STATUS TO WS-ABEND-FILE-STATUS  ECS037
00971                  MOVE 'UNABLE TO FIND REIN TABLE '                ECS037
00972                                 TO WS-ABEND-MESSAGE               ECS037
00973                  GO TO ABEND-PGM                                  ECS037
00974             ELSE                                                  ECS037
00975                IF ERRTBL-FILE-STATUS NOT = '00'                   ECS037
00976                   MOVE ERRTBL-FILE-STATUS TO WS-ABEND-FILE-STATUS ECS037
00977                   MOVE 'UNABLE TO READ REIN TABLE FILE '          ECS037
00978                                TO WS-ABEND-MESSAGE                ECS037
00979                   GO TO ABEND-PGM.                                ECS037
00980                                                                   ECS037
00981  RR-READ-REIN-X.                                                  ECS037
00982      EXIT.                                                        ECS037
00983                                                                   ECS037
00984  0698-EXIT.                                                       ECS037
00985      EXIT.                                                        ECS037
00986                                                                   ECS037
00987  0699-EXIT.                                                       ECS037
00988      EXIT.                                                        ECS037
00989                                                                   ECS037
00990  EJECT                                                            ECS037
00991  0700-DATE-CONVERSION-ROUTINE.                                    ECS037
00992                                                                   ECS037
00993      COPY ELCDCS.                                                 ECS037
00994                                                                   ECS037
00995  0799-EXIT.                                                       ECS037
00996      EXIT.                                                        ECS037
00997                                                                   ECS037
00998                                                                   ECS037
00999  0800-PRINT-HEADINGS.                                             ECS037
01000                                                                   ECS037
01001      MOVE '1'                   TO X.                             ECS037
01002      MOVE HEAD-1                TO P-DATA.                        ECS037
01003      PERFORM 1270-PRINT-RTN THRU 1270-EXIT.                       ECS037
01004                                                                   ECS037
01005      MOVE ' '                   TO X.                             ECS037
01006      MOVE WS-CURRENT-DATE       TO HD-DATE.                       ECS037
01007      MOVE COMPANY-NAME          TO HD-CLIENT.                     ECS037
01008      MOVE HEAD-2                TO P-DATA.                        ECS037
01009      PERFORM 1270-PRINT-RTN THRU 1270-EXIT.                       ECS037
01010                                                                   ECS037
01011      MOVE WS-PAGE-COUNT         TO HD-PAGE.                       ECS037
01012      MOVE ALPH-DATE             TO HD-ALF-DTE.                    ECS037
01013      MOVE HEAD-3                TO P-DATA.                        ECS037
01014      PERFORM 1270-PRINT-RTN THRU 1270-EXIT.                       ECS037
01015                                                                   ECS037
01016      MOVE HEAD-4                TO P-DATA.                        ECS037
01017      PERFORM 1270-PRINT-RTN THRU 1270-EXIT.                       ECS037
01018                                                                   ECS037
01019      MOVE HEAD-5                TO P-DATA.                        ECS037
01020      PERFORM 1270-PRINT-RTN THRU 1270-EXIT.                       ECS037
01021                                                                   ECS037
01022      ADD +1                     TO WS-PAGE-COUNT.                 ECS037
01023      MOVE +5                    TO WS-LINE-COUNT.                 ECS037
01024                                                                   ECS037
01025   0800-EXIT.                                                      ECS037
01026      EXIT.                                                        ECS037
01027                                                                   ECS037
01028  1270-PRINT-RTN.                                                  ECS037
01029                              COPY ELCPRT2.                        ECS037
01030  1270-EXIT.                                                       ECS037
01031      EXIT.                                                        ECS037
01032                                                                   ECS037
01033  EJECT                                                            ECS037
01034  1285-CLOSE.                                                      ECS037
01035                                                                   ECS037
01036      IF WS-LINE-COUNT GREATER +55                                 ECS037
01037          PERFORM 0800-PRINT-HEADINGS THRU 0800-EXIT.              ECS037
01038                                                                   ECS037
01039      MOVE SPACES                TO DTL.                           ECS037
01040      MOVE G-T-HD                TO P-DATA                         ECS037
01041      MOVE '-'                   TO X.                             ECS037
01042      PERFORM 1270-PRINT-RTN THRU 1270-EXIT.                       ECS037
01043                                                                   ECS037
01044      MOVE SPACES                TO DTL.                           ECS037
01045      MOVE TOT-CLM-CNT           TO GT-CNT.                        ECS037
01046      MOVE TOT-LIFE-AMT          TO GT-DTH.                        ECS037
01047      MOVE TOT-AH-AMT            TO GT-DIS                         ECS037
01048      MOVE G-T-LN                TO P-DATA                         ECS037
01049      MOVE ' '                   TO X.                             ECS037
01050      PERFORM 1270-PRINT-RTN THRU 1270-EXIT.                       ECS037
01051                                                                   ECS037
01052      MOVE SPACES                TO FILLGT.                        ECS037
01053      MOVE ' REINSURED'          TO FILLNT.                        ECS037
01054      MOVE TOT-REIN-CNT          TO GT-CNT.                        ECS037
01055      MOVE TOT-REIN-LIFE         TO GT-DTH.                        ECS037
01056      MOVE TOT-REIN-AH           TO GT-DIS                         ECS037
01057      MOVE G-T-LN                TO P-DATA                         ECS037
01058      MOVE ' '                   TO X.                             ECS037
01059      PERFORM 1270-PRINT-RTN THRU 1270-EXIT.                       ECS037
01060                                                                   ECS037
01061  1290-CLOSE-FICH.                                                 ECS037
01062                              COPY ELCPRTC.                        ECS037
01063                                                                   ECS037
01064  1300-CLOSE-FILES.                                                ECS037
01065                                                                   ECS037
01066      IF REIN-OPEN-SW = 'X'                                        ECS037
01067          CLOSE ERRTBL-IN                                          ECS037
01068          IF ERRTBL-FILE-STATUS NOT = '00' AND '97'                ECS037
01069              MOVE ERRTBL-FILE-STATUS TO WS-ABEND-FILE-STATUS      ECS037
01070                  MOVE 'UNABLE TO CLOSE REIN TABLE FILE'           ECS037
01071                                 TO WS-ABEND-MESSAGE               ECS037
01072              GO TO ABEND-PGM.                                     ECS037
01073                                                                   ECS037
01074      CLOSE PRNTR CLMS-HIST-OUT CERT-FILE.                         ECS037
01075                                                                   ECS037
01076      GOBACK.                                                      ECS037
01077                                                                   ECS037
01078      EJECT                                                        ECS037
01079  REINSURANCE-ROUTINES.                                            ECS037
01080                              COPY ECSRIRTN.                       ECS037
01081                                                                   ECS037
01082                                                                   ECS037
01083  ABEND-PGM.                  COPY ELCABEND.                       ECS037
