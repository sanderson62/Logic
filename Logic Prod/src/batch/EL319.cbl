00001  IDENTIFICATION DIVISION.                                         03/09/98
00002                                                                   EL319
00003  PROGRAM-ID.                 EL319 .                                 LV002
00004 *              PROGRAM CONVERTED BY                               EL319
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL319
00006 *              CONVERSION DATE 01/24/96 15:04:24.                 EL319
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             EL319
00008 *                            VMOD=2.008.                          EL319
00009                                                                   EL319
00009                                                                   EL319
00010 *AUTHOR.        LOGIC, INC.                                       EL319
00011 *               DALLAS, TEXAS.                                    EL319
00012                                                                   EL319
00013 *DATE-COMPILED.                                                   EL319
00014                                                                   EL319
00015 *SECURITY.   *****************************************************EL319
00016 *            *                                                   *EL319
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL319
00018 *            *                                                   *EL319
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL319
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL319
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL319
00022 *            *                                                   *EL319
00023 *            *****************************************************EL319
00024 *                                                                 EL319
00025 *REMARKS.                                                         EL319
00026 *        THIS PROGRAM WILL REPORT ON THOSE CLAIMS THAT OCCURRED   EL319
00027 *        WITHIN 30, 60, AND 90 DAYS OF THE CERTIFICATES EFFECTIVE EL319
00028 *        DATE.                                                    EL319
00029                                                                   EL319
00030  ENVIRONMENT DIVISION.                                            EL319
00031  INPUT-OUTPUT SECTION.                                            EL319
00032  FILE-CONTROL.                                                    EL319
00033                                                                   EL319
00034      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   EL319
00035                                                                   EL319
00036      SELECT ELMSTR                                                EL319
00037          COPY ELCMSTRS                                            EL319
00038          REPLACING ==CL-CONTROL-PRIMARY==                         EL319
00039          BY ==CLAIMS-CONTROL==                                    EL319
00040                    ==ELMSTR-FILE-STATUS==                         EL319
00041          BY ==W-CLMS-FILE-STATUS==.                               EL319
00042                                                                   EL319
00043      SELECT ELTRLR           ASSIGN TO SYS024-FBA1-ELTRLR         EL319
00044                              ORGANIZATION IS INDEXED              EL319
00045                              ACCESS IS RANDOM                     EL319
00046                              RECORD KEY IS AT-CONTROL-PRIMARY     EL319
00047                              FILE STATUS IS W-TRLR-FILE-STATUS.   EL319
00048                                                                   EL319
00049      SELECT ERACCT                                                EL319
00050          COPY ERCACCTS                                            EL319
00051          REPLACING ==ERACCT-FILE-STATUS==                         EL319
00052          BY ==W-AM-FILE-STATUS==.                                 EL319
00053                                                                   EL319
00054      SELECT MPPROD           ASSIGN TO SYS026-FBA1-MPPROD         EL319
00055                              ORGANIZATION IS INDEXED              EL319
00056                              ACCESS IS DYNAMIC                    EL319
00057                              RECORD KEY IS PD-CONTROL-PRIMARY     EL319
00058                              FILE STATUS IS W-PD-FILE-STATUS.     EL319
00059                                                                   EL319
00060      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL319
00061                                                                   EL319
00062      SELECT PRINTER          ASSIGN TO SYS008-UR-1403-S-SYS008.   EL319
00063                                                                   EL319
00064      SELECT SORT-WORK        ASSIGN TO SYS001-UT-3380-S-SORTWK1.  EL319
00065                                  EJECT                            EL319
00066  DATA DIVISION.                                                   EL319
00067  FILE SECTION.                                                    EL319
00068  FD  DISK-DATE                                                    EL319
00069      COPY ELCDTEFD.                                               EL319
00070                                  EJECT                            EL319
00071  FD  ELMSTR.                                                      EL319
00072                                                                   EL319
00073  01  CLAIMS.                                                      EL319
00074      12  FILLER                      PIC  X(02).                  EL319
00075      12  CLAIMS-CONTROL              PIC  X(20).                  EL319
00076      12  FILLER                      PIC X(328).                  EL319
00077                                  EJECT                            EL319
00078  FD  ELTRLR.                                                      EL319
00079                                                                   EL319
00080      COPY ELCTRLR.                                                EL319
00081                                  EJECT                            EL319
00082  FD  ERACCT.                                                      EL319
00083                                                                   EL319
00084      COPY ERCACCT.                                                EL319
00085                                  EJECT                            EL319
00086  FD  MPPROD.                                                      EL319
00087                                                                   EL319
00088      COPY MPCPROD.                                                EL319
00089                                  EJECT                            EL319
00090  FD  FICH                                                         EL319
00091      COPY ELCFCHFD.                                               EL319
00092                                  EJECT                            EL319
00093  FD  PRINTER                                                      EL319
00094      COPY ELCPRTFD.                                               EL319
00095                                  EJECT                            EL319
00096  SD  SORT-WORK.                                                   EL319
00097                                                                   EL319
00098  01  SORT-RECORD.                                                 EL319
00099      12  SW-KEY.                                                  EL319
00100          16  SW-CONTROL.                                          EL319
00101              20  SW-CARRIER      PIC  X(01).                      EL319
00102              20  SW-GROUPING     PIC  X(06).                      EL319
00103              20  SW-STATE        PIC  X(02).                      EL319
00104              20  SW-ACCOUNT      PIC  X(10).                      EL319
00105          16  SW-REPORT-KEY.                                       EL319
00106              20  SW-DAYS-DIFF    PIC  9(02).                      EL319
00107              20  SW-CERT-NO      PIC  X(11).                      EL319
00108              20  SW-CLAIM-NO     PIC  X(12).                      EL319
00109      12  SW-DATA.                                                 EL319
00110          16  SW-QUALIFIED-SW     PIC  X(01).                      EL319
00111              88  SW-QUALIFIED          VALUE 'Y'.                 EL319
00112              88  SW-CERT-NOT-QUALIFIED VALUE 'N'.                 EL319
00113          16  SW-DAYS-SLOT        PIC  X(01).                      EL319
00114          16  SW-CLAIMS-RECORD    PIC X(350).                      EL319
00115                                                                   EL319
00116                                  EJECT                            EL319
00117  WORKING-STORAGE SECTION.                                         EL319
00118  77  FILLER  PIC  X(32) VALUE '********************************'. EL319
00119  77  FILLER  PIC  X(32) VALUE '     EL319 WORKING STORAGE     '.  EL319
00120  77  W-VERS  PIC  X(32) VALUE '********** VMOD=2.008 **********'. EL319
00121                                                                   EL319
00122  01  W-PROGRAM-WORK-AREAS.                                        EL319
00123      12  FILLER                  PIC  X(14)                       EL319
00124                                  VALUE 'WORK AREA COMP'.          EL319
00125      12  W-NDX                   PIC S9(04)       COMP.           EL319
00126      12  W-NDX2                  PIC S9(04)       COMP.           EL319
00127      12  W-NDX3                  PIC S9(04)       COMP.           EL319
00128                                                                   EL319
00129      12  FILLER                  PIC  X(16)                       EL319
00130                                  VALUE 'WORK AREA COMP-3'.        EL319
00131                                                                   EL319
00132      12  W-ACCT-COUNT            PIC  9(06)       COMP-3          EL319
00133                                             VALUE ZEROS.          EL319
00134      12  W-ACCT-ELIGIBLE         PIC  9(06)       COMP-3          EL319
00135                                             VALUE ZEROS.          EL319
00136      12  W-ACCT-30-DAY-AMOUNT    PIC S9(11)V9(02) COMP-3          EL319
00137                                             VALUE ZEROS.          EL319
00138      12  W-ACCT-30-DAY-COUNT     PIC  9(06)       COMP-3          EL319
00139                                             VALUE ZEROS.          EL319
00140      12  W-ACCT-60-DAY-AMOUNT    PIC S9(11)V9(02) COMP-3          EL319
00141                                             VALUE ZEROS.          EL319
00142      12  W-ACCT-60-DAY-COUNT     PIC  9(06)       COMP-3          EL319
00143                                             VALUE ZEROS.          EL319
00144      12  W-ACCT-90-DAY-AMOUNT    PIC S9(11)V9(02) COMP-3          EL319
00145                                             VALUE ZEROS.          EL319
00146      12  W-ACCT-90-DAY-COUNT     PIC  9(06)       COMP-3          EL319
00147                                             VALUE ZEROS.          EL319
00148      12  W-CAR-COUNT             PIC  9(06)       COMP-3          EL319
00149                                             VALUE ZEROS.          EL319
00150      12  W-CAR-ELIGIBLE          PIC  9(06)       COMP-3          EL319
00151                                             VALUE ZEROS.          EL319
00152      12  W-CAR-BAD-DATE          PIC  9(06)       COMP-3          EL319
00153                                             VALUE ZEROS.          EL319
00154      12  W-CAR-PRV-DATE          PIC  9(06)       COMP-3          EL319
00155                                             VALUE ZEROS.          EL319
00156      12  W-CAR-30-DAY-AMOUNT     PIC S9(11)V9(02) COMP-3          EL319
00157                                             VALUE ZEROS.          EL319
00158      12  W-CAR-30-DAY-COUNT      PIC  9(06)       COMP-3          EL319
00159                                             VALUE ZEROS.          EL319
00160      12  W-CAR-60-DAY-AMOUNT     PIC S9(11)V9(02) COMP-3          EL319
00161                                             VALUE ZEROS.          EL319
00162      12  W-CAR-60-DAY-COUNT      PIC  9(06)       COMP-3          EL319
00163                                             VALUE ZEROS.          EL319
00164      12  W-CAR-90-DAY-AMOUNT     PIC S9(11)V9(02) COMP-3          EL319
00165                                             VALUE ZEROS.          EL319
00166      12  W-CAR-90-DAY-COUNT      PIC  9(06)       COMP-3          EL319
00167                                             VALUE ZEROS.          EL319
00168      12  W-CLMS-RECORDS-BAD-DATE PIC  9(06)       COMP-3          EL319
00169                                             VALUE ZEROS.          EL319
00170      12  W-CLMS-RECORDS-ELIGIBLE PIC  9(06)       COMP-3          EL319
00171                                             VALUE ZEROS.          EL319
00172      12  W-CLMS-RECORDS-PRV-DATE PIC  9(06)       COMP-3          EL319
00173                                             VALUE ZEROS.          EL319
00174      12  W-CLMS-RECORDS-READ     PIC  9(06)       COMP-3          EL319
00175                                             VALUE ZEROS.          EL319
00176      12  W-CLMS-RECORDS-USED     PIC  9(06)       COMP-3          EL319
00177                                             VALUE ZEROS.          EL319
00178      12  W-FIN-COUNT             PIC  9(06)       COMP-3          EL319
00179                                             VALUE ZEROS.          EL319
00180      12  W-FIN-ELIGIBLE          PIC  9(06)       COMP-3          EL319
00181                                             VALUE ZEROS.          EL319
00182      12  W-FIN-30-DAY-AMOUNT     PIC S9(11)V9(02) COMP-3          EL319
00183                                             VALUE ZEROS.          EL319
00184      12  W-FIN-30-DAY-COUNT      PIC  9(06)       COMP-3          EL319
00185                                             VALUE ZEROS.          EL319
00186      12  W-FIN-60-DAY-AMOUNT     PIC S9(11)V9(02) COMP-3          EL319
00187                                             VALUE ZEROS.          EL319
00188      12  W-FIN-60-DAY-COUNT      PIC  9(06)       COMP-3          EL319
00189                                             VALUE ZEROS.          EL319
00190      12  W-FIN-90-DAY-AMOUNT     PIC S9(11)V9(02) COMP-3          EL319
00191                                             VALUE ZEROS.          EL319
00192      12  W-FIN-90-DAY-COUNT      PIC  9(06)       COMP-3          EL319
00193                                             VALUE ZEROS.          EL319
00194      12  W-LINES-USED            PIC S9(03)       COMP-3          EL319
00195                                             VALUE ZEROS.          EL319
00196      12  W-LINE-COUNT            PIC S9(03)       COMP-3          EL319
00197                                             VALUE +99.            EL319
00198      12  W-PAGE-COUNT            PIC S9(05)       COMP-3          EL319
00199                                             VALUE ZEROS.          EL319
00200      12  W-PERCENT               PIC S9(04)V9(05) COMP-3          EL319
00201                                             VALUE ZEROS.          EL319
00202      12  W-SLOT-AMOUNT           PIC S9(11)V9(02) COMP-3          EL319
00203                                             VALUE ZEROS.          EL319
00204      12  W-SLOT-COUNT            PIC  9(06)       COMP-3          EL319
00205                                             VALUE ZEROS.          EL319
00206                                                                   EL319
00207      12  FILLER                  PIC  X(15)                       EL319
00208                                  VALUE 'DISPLAY WK AREA'.         EL319
00209      12  W-DAYS-SLOT             PIC  X(01).                      EL319
00210                                                                   EL319
00211      12  W-ACCOUNT-NAME          PIC  X(30).                      EL319
00212      12  W-AM-FILE-STATUS        PIC  X(02).                      EL319
00213      12  W-CLMS-FILE-STATUS      PIC  X(02).                      EL319
00214      12  W-PD-FILE-STATUS        PIC  X(02).                      EL319
00215                                                                   EL319
00216      12  W-CONTROL.                                               EL319
00217          16  W-CARRIER           PIC  X(01).                      EL319
00218          16  W-GROUPING          PIC  X(06).                      EL319
00219          16  W-STATE             PIC  X(02).                      EL319
00220          16  W-ACCOUNT           PIC  X(10).                      EL319
00221      12  W-DIAGNOSIS             PIC  X(60).                      EL319
00222      12  W-END-DATE-BIN          PIC  X(02).                      EL319
00223      12  W-END-DATE.                                              EL319
00224          16  W-END-YR-MO.                                         EL319
00225              20  W-END-YR        PIC  9(02).                      EL319
00226              20  W-END-MO        PIC  9(02).                      EL319
00227          16  W-END-DA            PIC  9(02).                      EL319
00228                                                                   EL319
00229      12  W-NAME.                                                  EL319
00230          16  W-NAME-CHAR OCCURS 30 TIMES                          EL319
00231                                  PIC  X(01).                      EL319
00232                                                                   EL319
00233      12  W-RUN-DATE.                                              EL319
00234          16  W-RUN-YR            PIC  X(02).                      EL319
00235          16  W-RUN-MO            PIC  X(02).                      EL319
00236          16  W-RUN-DA            PIC  X(02).                      EL319
00237      12  W-START-BIN-DATE        PIC  X(02).                      EL319
00238      12  W-START-DATE.                                            EL319
00239          16  W-START-YR-MO.                                       EL319
00240              20  W-START-YR      PIC  9(02).                      EL319
00241              20  W-START-MO      PIC  9(02).                      EL319
00242          16  W-START-DA          PIC  9(02).                      EL319
LGC190     12  W-START-YR-WORK         PIC  S99.                        EL319
00243      12  W-TRLR-FILE-STATUS      PIC  X(02).                      EL319
00244      12  W-VERS-WORK-AREA.                                        EL319
00245          16  FILLER              PIC  X(16) VALUE SPACES.         EL319
00246          16  W-PGM-VERS-VALUE    PIC  X(05) VALUE SPACES.         EL319
00247          16  FILLER              PIC  X(11) VALUE SPACES.         EL319
00248      12  W-WORK-NAME.                                             EL319
00249          16  W-WN-CHAR OCCURS 15 TIMES                            EL319
00250                                  PIC  X(01).                      EL319
00251      12  W-X                     PIC  X(01).                      EL319
00252                                                                   EL319
00253      12  WS-CERT-EFF-DATE        PIC X(6).                        EL319
00254                                                                   EL319
00255      12  WS-ABEND-CODE.                                           EL319
00256          16  WS-AC-1-2.                                           EL319
00257              20  WS-AC-1         PIC  X(01).                      EL319
00258              20  WS-AC-2         PIC  X(01).                      EL319
00259          16  WS-AC-3-4.                                           EL319
00260              20  WS-AC-3         PIC  X(01).                      EL319
00261              20  WS-AC-4         PIC  X(01).                      EL319
00262      12  WS-ABEND-MESSAGE.                                        EL319
00263          16  FILLER              PIC  X(35)      VALUE SPACES.    EL319
00264          16  WS-ABEND-CO-ID      PIC  X(03)      VALUE SPACES.    EL319
00265          16  FILLER              PIC  X(42)      VALUE SPACES.    EL319
00266      12  WS-ABEND-FILE-STATUS    PIC  X(02)      VALUE SPACES.    EL319
00267      12  WS-RETURN-CODE          PIC S9(04)      VALUE ZEROS.     EL319
00268                                                                   EL319
00269  01  W-PROGRAM-SWITCHES.                                          EL319
00270      12  FILLER                  PIC  X(16)                       EL319
00271                                  VALUE 'PROGRAM SWITCHES'.        EL319
00272                                                                   EL319
00273      12  W-ACCOUNT-FOUND-SW      PIC  X(01)      VALUE SPACE.     EL319
00274          88  W-ACCOUNT-FOUND           VALUE 'Y'.                 EL319
00275          88  W-ACCOUNT-NOT-FOUND       VALUE ' '.                 EL319
00276      12  W-ACCT-NAME-PRINTED-SW  PIC  X(01)      VALUE SPACE.     EL319
00277          88  W-ACCT-NAME-PRINTED       VALUE 'Y'.                 EL319
00278          88  W-ACCT-NAME-NOT-PRINTED   VALUE SPACES.              EL319
00279      12  W-END-OF-ACCT-FILE-SW   PIC  X(01)      VALUE SPACE.     EL319
00280          88  W-END-OF-ACCT-FILE        VALUE 'Y'.                 EL319
00281      12  W-END-OF-CLMS-FILE-SW   PIC  X(01)      VALUE SPACE.     EL319
00282          88  W-END-OF-CLMS-FILE                  VALUE 'Y'.       EL319
00283      12  W-END-OF-SORT-DATA-SW   PIC  X(01)      VALUE SPACE.     EL319
00284          88  W-END-OF-SORT-DATA                  VALUE 'Y'.       EL319
00285                                                                   EL319
00286  01  W-PROGRAM-CONSTANTS.                                         EL319
00287      12  FILLER                  PIC  X(17)                       EL319
00288                                  VALUE 'PROGRAM CONSTANTS'.       EL319
00289      12  W-PGM-SUB               PIC S9(03) COMP-3 VALUE +319.    EL319
00290      12  W-ZEROS                 PIC S9(04) COMP   VALUE ZERO.    EL319
00291      12  WS-ZERO                 PIC S9(01)        VALUE ZERO.    EL319
00292                                                                   EL319
00293                                  EJECT                            EL319
00294  01  W-PRINT-LINES.                                               EL319
00295      12  FILLER                  PIC  X(11)                       EL319
00296                                  VALUE 'PRINT LINES'.             EL319
00297                                                                   EL319
00298      12  W-PRINT-LINE-HOLD       PIC X(133).                      EL319
00299                                                                   EL319
00300      12  W-DETAIL-LINE.                                           EL319
00301          16  FILLER              PIC  X(01)  VALUE SPACES.        EL319
00302          16  W-DT-CERT-NO        PIC  X(12).                      EL319
00303          16  W-DT-CLAIM-NO       PIC  X(09).                      EL319
00304          16  W-DT-DAYS-DIFF      PIC  Z9.                         EL319
00305          16  FILLER              PIC  X(02)  VALUE SPACES.        EL319
00306          16  W-DT-CLAIM-TYPE     PIC  X(03)  VALUE SPACES.        EL319
00307          16  W-DT-CLAIM-STATUS   PIC  X(12).                      EL319
00308          16  W-DT-TOTAL-PAID-AMT PIC  --,---,--9.99.              EL319
00309          16  FILLER              PIC  X(01)  VALUE SPACES.        EL319
00310          16  W-DT-INCURRED-DATE  PIC  X(09).                      EL319
00311          16  W-DT-INSURED-NAME   PIC  X(26).                      EL319
00312          16  W-DT-DIAGNOSIS      PIC  X(43).                      EL319
00313                                                                   EL319
00314      12  W-CONTROL-LINE.                                          EL319
00315          16  FILLER              PIC  X(01).                      EL319
00316          16  FILLER              PIC  X(09) VALUE                 EL319
00317              'ACCOUNT: '.                                         EL319
00318          16  W-CL-ACCOUNT        PIC  X(10).                      EL319
00319          16  FILLER              PIC  X(02) VALUE                 EL319
00320              ','.                                                 EL319
00321          16  W-CL-ACCOUNT-NAME   PIC  X(30).                      EL319
00322                                                                   EL319
00323      12  W-CONTROL-UNDERLINE.                                     EL319
00324          16  FILLER              PIC  X(21).                      EL319
00325          16  FILLER              PIC  X(30) VALUE                 EL319
00326              '______________________________'.                    EL319
00327                                                                   EL319
00328      12  W-HDR-1.                                                 EL319
00329          16  FILLER              PIC  X(01)  VALUE SPACES.        EL319
00330          16  W-HDR1-COMPANY-ID   PIC  X(56)  VALUE SPACES.        EL319
00331          16  FILLER              PIC  X(62)  VALUE                EL319
00332             'QUICK CLAIM REPORT'.                                 EL319
00333          16  W-HDR1-REPORT-NUMBER                                 EL319
00334                                  PIC  X(05)  VALUE SPACES.        EL319
00335          16  FILLER              PIC  X(02)  VALUE ' V'.          EL319
00336          16  W-HDR1-VERS-VALUE   PIC  X(05)  VALUE SPACES.        EL319
00337                                                                   EL319
00338      12  W-HDR-2.                                                 EL319
00339          16  FILLER              PIC  X(01)  VALUE ' '.           EL319
00340          16  FILLER              PIC  X(16)                       EL319
00341              VALUE 'PERIOD COVERED: '.                            EL319
00342          16  W-HDR2-START-DATE   PIC  X(09).                      EL319
00343          16  FILLER              PIC  X(06)                       EL319
00344              VALUE ' THRU '.                                      EL319
00345          16  W-HDR2-END-DATE.                                     EL319
00346              20  W-HDR2-ED-MM    PIC  X(02).                      EL319
00347              20  FILLER          PIC  X(01)  VALUE '/'.           EL319
00348              20  W-HDR2-ED-DD    PIC  X(02).                      EL319
00349              20  FILLER          PIC  X(01)  VALUE '/'.           EL319
00350              20  W-HDR2-ED-YY    PIC  X(02).                      EL319
00351          16  FILLER              PIC  X(11)  VALUE SPACES.        EL319
00352          16  W-HDR2-COMPANY-NAME PIC  X(68)                       EL319
00353              VALUE '         COMPANY NAME'.                       EL319
00354          16  W-HDR2-REPORT-DATE  PIC  X(12)  VALUE 'XX/XX/XX'.    EL319
00355                                                                   EL319
00356      12  W-HDR-3.                                                 EL319
00357          16  FILLER              PIC  X(57)  VALUE SPACES.        EL319
00358          16  W-HDR3-DATE         PIC  X(62)  VALUE SPACES.        EL319
00359          16  FILLER              PIC  X(05)  VALUE 'PAGE:'.       EL319
00360          16  W-HDR3-PAGE         PIC  ZZZZZ9.                     EL319
00361                                                                   EL319
00362      12  W-HDR-4.                                                 EL319
00363          16  FILLER              PIC  X(10) VALUE                 EL319
00364              ' CARRIER: '.                                        EL319
00365          16  W-HDR4-CARRIER      PIC  X(02).                      EL319
00366          16  W-HDR4-GRP-TITLE    PIC  X(10) VALUE                 EL319
00367              'GROUPING: '.                                        EL319
00368          16  W-HDR4-GROUPING     PIC  X(07).                      EL319
00369          16  W-HDR4-STATE-TITLE  PIC  X(07) VALUE                 EL319
00370              'STATE: '.                                           EL319
00371          16  W-HDR4-STATE        PIC  X(02).                      EL319
00372                                                                   EL319
00373      12  W-HDR-5.                                                 EL319
00374          16  FILLER              PIC  X(41)  VALUE                EL319
00375             ' CERTIFICATE CLAIM   DAYS    CLAIMS      '.          EL319
00376          16  FILLER              PIC  X(49)  VALUE                EL319
00377             '     TOTAL    INCURRED          INSURED          '.  EL319
00378          16  FILLER              PIC  X(43)  VALUE                EL319
00379             '                 DIAGNOSIS'.                         EL319
00380                                                                   EL319
00381      12  W-HDR-6.                                                 EL319
00382          16  FILLER              PIC  X(41)  VALUE                EL319
00383             '   NUMBER    NUMBER     TYPE STATUS      '.          EL319
00384          16  FILLER              PIC  X(49)  VALUE                EL319
00385             '     PAID       DATE             NAME'.              EL319
00386                                                                   EL319
00387      12  W-EXCEPTION-LINE.                                        EL319
00388          16  FILLER              PIC  X(01)  VALUE SPACES.        EL319
00389          16  W-EXCEPT-COUNT      PIC  ZZZ,ZZZ,ZZ9.                EL319
00390          16  W-EXCEPT-TITLE      PIC  X(50)  VALUE SPACES.        EL319
00391                                                                      CL**2
00392      12  W-TOTAL-TITLE-LINE.                                      EL319
00393          16  FILLER              PIC  X(01)  VALUE SPACES.        EL319
00394          16  W-TOTAL-TITLE       PIC  X(18)  VALUE                EL319
00395              'CARRIER TOTALS'.                                    EL319
00396                                                                   EL319
00397      12  W-TOTAL-DAYS-LINE.                                       EL319
00398          16  FILLER              PIC  X(07)  VALUE SPACES.        EL319
00399          16  W-TDL-DAYS-GRP      PIC  9(02).                      EL319
00400          16  FILLER              PIC  X(24)  VALUE                EL319
00401              ' DAYS OR LESS'.                                     EL319
00402          16  W-TDL-AMOUNT        PIC  ---,---,---,--9.99.         EL319
00403          16  FILLER              PIC  X(01)  VALUE SPACES.        EL319
00404          16  W-TDL-COUNT         PIC  ZZZ,ZZ9.                    EL319
00405          16  W-TDL-FIN-GRP       PIC  X(70)  VALUE SPACES.        EL319
00406          16  FILLER REDEFINES W-TDL-FIN-GRP.                      EL319
00407              20  W-TDL-OF        PIC  X(04).                      EL319
00408              20  W-TDL-ELIGIBLE  PIC  ZZZ,ZZ9.                    EL319
00409              20  W-TDL-OR        PIC  X(04).                      EL319
00410              20  W-TDL-PERCENT   PIC  ZZ9.                        EL319
00411              20  W-TDL-PERCENT-TITLE                              EL319
00412                                  PIC  X(52).                      EL319
00413                                                                   EL319
00414      12  W-TOTAL-YEAR-LINE.                                       EL319
00415          16  FILLER              PIC  X(01)  VALUE SPACES.        EL319
00416          16  W-TYL-TITLE         PIC  X(32)  VALUE SPACES.        EL319
00417          16  W-TYL-AMOUNT        PIC  ---,---,---,--9.99.         EL319
00418          16  FILLER              PIC  X(01)  VALUE SPACES.        EL319
00419          16  W-TYL-COUNT         PIC  ZZZ,ZZ9.                    EL319
00420          16  W-TYL-OF            PIC  X(04)  VALUE ' OF'.         EL319
00421          16  W-TYL-ELIGIBLE      PIC  ZZZ,ZZ9.                    EL319
00422          16  W-TYL-OR            PIC  X(04)  VALUE ' OR'.         EL319
00423          16  W-TYL-PERCENT       PIC  ZZ9.                        EL319
00424          16  W-TYL-PERCENT-TITLE PIC  X(58).                      EL319
00425                                                                   EL319
00426  01  W-TOTAL-YEAR-TITLES.                                         EL319
00427      12  W-TYT-ACCOUNT           PIC  X(32)  VALUE                EL319
00428              'ACCOUNT TOTAL FOR LAST 12 MONTHS'.                  EL319
00429      12  W-TYT-CARRIER           PIC  X(32)  VALUE                EL319
00430              'CARRIER TOTAL FOR LAST 12 MONTHS'.                  EL319
00431      12  W-TYT-COMPANY           PIC  X(32)  VALUE                EL319
00432              'COMPANY TOTAL FOR LAST 12 MONTHS'.                  EL319
00433                                  EJECT                            EL319
00434      COPY ELCDATE.                                                EL319
00435                                  EJECT                            EL319
00436      COPY ELCDTECX.                                               EL319
00437                                  EJECT                            EL319
00438      COPY ELCDTEVR.                                               EL319
00439                                  EJECT                            EL319
00440      COPY ELCMSTR.                                                EL319
00441                                  EJECT                            EL319
00442  PROCEDURE DIVISION.                                              EL319
00443                                                                   EL319
00444      DISPLAY ' '.                                                 EL319
00445      DISPLAY '****** THE FOLLOWING MESSAGES WERE CREATED BY '     EL319
00446          'EL319 ******'                                           EL319
00447      DISPLAY ' '.                                                 EL319
00448                                                                   EL319
00449  0000-CONTROL-DATA-PROCESS SECTION.                               EL319
00450      COPY ELCDTERX SUPPRESS                                       EL319
00451                    REPLACING == PGM-SUB == BY == W-PGM-SUB ==.    EL319
00452                                  EJECT                            EL319
00453  0100-START-PROGRAM.                                              EL319
00454                                                                   EL319
00455      PERFORM 1000-INITIALIZE THRU 1000-EXIT.                      EL319
00456                                                                   EL319
00457      SORT SORT-WORK                                               EL319
00458          ON ASCENDING  SW-CONTROL                                 EL319
00459                        SW-DAYS-DIFF                               EL319
00460                        SW-CERT-NO                                 EL319
00461                        SW-CLAIM-NO                                EL319
00462          INPUT PROCEDURE  2000-PROCESS-INPUT                      EL319
00463          OUTPUT PROCEDURE 3000-PRINT-DATA.                        EL319
00464                                                                   EL319
00465      IF  SORT-RETURN NOT = ZEROS                                  EL319
00466          MOVE '**** SORT RETURN ERROR ****'                       EL319
00467                                  TO WS-ABEND-MESSAGE              EL319
00468          MOVE SORT-RETURN        TO WS-RETURN-CODE                EL319
00469          GO TO ABEND-PGM.                                         EL319
00470                                                                   EL319
00471      GO TO 9999-END-OF-JOB.                                       EL319
00472                                                                   EL319
00473  0100-EXIT.                                                       EL319
00474      EXIT.                                                        EL319
00475                                  EJECT                            EL319
00476  1000-INITIALIZE SECTION.                                         EL319
00477                                                                   EL319
00478      OPEN INPUT ELMSTR.                                           EL319
00479                                                                   EL319
00480      IF  W-CLMS-FILE-STATUS = '00' OR '97'                        EL319
00481          NEXT SENTENCE                                            EL319
00482      ELSE                                                         EL319
00483          MOVE '**** OPEN CLAIMS FILE ERROR ****'                  EL319
00484                                  TO WS-ABEND-MESSAGE              EL319
00485          MOVE '1202'             TO WS-RETURN-CODE                EL319
00486          MOVE W-CLMS-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL319
00487          GO TO ABEND-PGM.                                         EL319
00488                                                                   EL319
00489      PERFORM 1100-GET-DATES THRU 1100-EXIT.                       EL319
00490                                                                   EL319
00491      MOVE WS-CURRENT-DATE        TO W-HDR2-REPORT-DATE.           EL319
00492      MOVE COMPANY-NAME           TO W-HDR2-COMPANY-NAME.          EL319
00493      MOVE ALPH-DATE              TO W-HDR3-DATE.                  EL319
00494      MOVE DTE-CLIENT             TO W-HDR1-COMPANY-ID             EL319
00495      MOVE W-VERS                 TO W-VERS-WORK-AREA.             EL319
00496      MOVE 'EL319'                TO W-HDR1-REPORT-NUMBER.         EL319
00497      MOVE W-PGM-VERS-VALUE       TO W-HDR1-VERS-VALUE.            EL319
00498      MOVE ZEROS                  TO SORT-RETURN.                  EL319
00499                                                                   EL319
00500  1000-EXIT.                                                       EL319
00501      EXIT.                                                        EL319
00502                                  EJECT                            EL319
00503  1100-GET-DATES.                                                  EL319
00504                                                                   EL319
00505      MOVE RUN-MO                 TO W-END-MO                      EL319
00506                                     W-HDR2-ED-MM.                 EL319
00507      MOVE RUN-DA                 TO W-END-DA                      EL319
00508                                     W-HDR2-ED-DD.                 EL319
00509      MOVE RUN-YR                 TO W-END-YR                      EL319
00510                                     W-HDR2-ED-YY.                 EL319
00511                                                                   EL319
00512 *****NOTE: END DATE ONLY NEEDED IF RUN DATE NOT END OF TARGET     EL319
00513 *****PERIOD.                                                      EL319
00514      MOVE W-END-DATE             TO W-START-DATE.                 EL319
00515                                                                   EL319
00516      IF  W-START-MO EQUAL 02 AND                                  EL319
00517          W-START-DA EQUAL 29                                      EL319
00518          SUBTRACT +1 FROM W-START-DA.                             EL319
00519                                                                   EL319
00520 **   SUBTRACT +1 FROM W-START-YR.                                 EL319
LGC190     MOVE W-START-YR             TO W-START-YR-WORK.
LGC190     SUBTRACT +1 FROM W-START-YR-WORK.
LGC190
LGC190     IF W-START-YR-WORK < 0                                       EL319
LGC190        ADD +100  TO W-START-YR-WORK.                                CL**2
00521                                                                   EL319
00522 **   IF W-START-YR < 0                                            EL319
00523 **      ADD +100  TO W-START-YR.                                     CL**2
LGC190     MOVE W-START-YR-WORK        TO W-START-YR.
00524                                                                   EL319
00525      MOVE W-START-DATE           TO DC-GREG-DATE-1-YMD.           EL319
00526      MOVE '3'                    TO DC-OPTION-CODE.               EL319
00527      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL319
00528                                                                   EL319
00529      IF  NO-CONVERSION-ERROR                                      EL319
00530          MOVE +1                 TO DC-ELAPSED-DAYS               EL319
00531          MOVE ZEROS              TO DC-ELAPSED-MONTHS             EL319
00532          MOVE '6'                TO DC-OPTION-CODE                EL319
00533          CALL 'ELDATCX' USING DATE-CONVERSION-DATA                EL319
00534          IF  NO-CONVERSION-ERROR                                  EL319
00535              MOVE DC-BIN-DATE-2  TO W-START-BIN-DATE              EL319
00536          ELSE                                                     EL319
00537              MOVE '**** START DATE INVALID ****'                  EL319
00538                                  TO WS-ABEND-MESSAGE              EL319
00539              MOVE '99'           TO WS-AC-1-2                     EL319
00540              MOVE '0'            TO WS-AC-3                       EL319
00541              MOVE DC-ERROR-CODE  TO WS-AC-4                       EL319
00542              MOVE WS-ABEND-CODE  TO WS-RETURN-CODE                EL319
00543              GO TO ABEND-PGM                                      EL319
00544      ELSE                                                         EL319
00545          MOVE '**** START DATE INVALID ****'                      EL319
00546                                  TO WS-ABEND-MESSAGE              EL319
00547          MOVE '99'               TO WS-AC-1-2                     EL319
00548          MOVE '1'                TO WS-AC-3                       EL319
00549          MOVE DC-ERROR-CODE      TO WS-AC-4                       EL319
00550          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                EL319
00551          GO TO ABEND-PGM.                                         EL319
00552                                                                   EL319
00553      MOVE W-START-BIN-DATE       TO DC-BIN-DATE-1.                EL319
00554      MOVE ' '                    TO DC-OPTION-CODE.               EL319
00555      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL319
00556                                                                   EL319
00557      IF  NO-CONVERSION-ERROR                                      EL319
00558          MOVE DC-GREG-DATE-1-EDIT                                 EL319
00559                                  TO W-HDR2-START-DATE             EL319
00560          MOVE DC-GREG-DATE-1-YMD TO W-START-DATE.                 EL319
00561                                                                   EL319
00562      MOVE W-END-DATE             TO DC-GREG-DATE-1-YMD.           EL319
00563      MOVE '3'                    TO DC-OPTION-CODE.               EL319
00564      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL319
00565                                                                   EL319
00566      IF  NO-CONVERSION-ERROR                                      EL319
00567          MOVE DC-BIN-DATE-1      TO W-END-DATE-BIN                EL319
00568      ELSE                                                         EL319
00569          MOVE '**** END DATE INVALID ****'                        EL319
00570                                  TO WS-ABEND-MESSAGE              EL319
00571          MOVE '99'               TO WS-AC-1-2                     EL319
00572          MOVE '0'                TO WS-AC-3                       EL319
00573          MOVE DC-ERROR-CODE      TO WS-AC-4                       EL319
00574          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                EL319
00575          GO TO ABEND-PGM.                                         EL319
00576                                                                   EL319
00577  1100-EXIT.                                                       EL319
00578      EXIT.                                                        EL319
00579                                  EJECT                            EL319
00580  2000-PROCESS-INPUT SECTION.                                      EL319
00581                                                                   EL319
00582      PERFORM 2100-PROCESS-CLAIMS-FILE THRU 2100-EXIT.             EL319
00583                                                                   EL319
00584      CLOSE ELMSTR.                                                EL319
00585                                                                   EL319
00586      IF  W-CLMS-FILE-STATUS NOT = '00'                            EL319
00587          MOVE '**** CLAIMS MASTER CLOSE ERROR - ELCLMS ****'      EL319
00588                                  TO WS-ABEND-MESSAGE              EL319
00589          MOVE '1204'             TO WS-RETURN-CODE                EL319
00590          MOVE W-CLMS-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL319
00591          GO TO ABEND-PGM.                                         EL319
00592                                                                   EL319
00593      GO TO 2999-EXIT.                                             EL319
00594                                  EJECT                            EL319
00595  2100-PROCESS-CLAIMS-FILE.                                        EL319
00596                                                                   EL319
00597      MOVE LOW-VALUES             TO CL-CONTROL-PRIMARY.           EL319
00598      MOVE DTE-CLASIC-COMPANY-CD  TO CL-COMPANY-CD.                EL319
00599      MOVE CL-CONTROL-PRIMARY     TO CLAIMS-CONTROL.               EL319
00600                                                                   EL319
00601      START ELMSTR                                                 EL319
00602          KEY IS NOT LESS THAN CLAIMS-CONTROL.                     EL319
00603 *        KEY IS NOT LESS THAN CL-CONTROL-PRIMARY.                 EL319
00604                                                                   EL319
00605      IF  W-CLMS-FILE-STATUS NOT = ZERO                            EL319
00606          MOVE 'ERROR OCCURED STARTING CLAIM MASTER - ELMSTR'      EL319
00607                                  TO WS-ABEND-MESSAGE              EL319
00608          MOVE W-CLMS-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL319
00609          PERFORM ABEND-PGM.                                       EL319
00610                                                                   EL319
00611      PERFORM 2200-PROCESS-CLAIMS THRU 2200-EXIT                   EL319
00612              UNTIL                                                EL319
00613          W-END-OF-CLMS-FILE.                                      EL319
00614                                                                   EL319
00615      IF  W-CLMS-RECORDS-READ EQUAL ZEROS                          EL319
00616          MOVE 'NO CLAIMS FOUND FOR THIS COMPANY - XXX'            EL319
00617                                  TO WS-ABEND-MESSAGE              EL319
00618          MOVE DTE-CLIENT         TO WS-ABEND-CO-ID                EL319
00619          MOVE '9801'             TO WS-ABEND-CODE                 EL319
00620          PERFORM ABEND-PGM.                                       EL319
00621                                                                   EL319
00622  2100-EXIT.                                                       EL319
00623      EXIT.                                                        EL319
00624                                  EJECT                            EL319
00625  2200-PROCESS-CLAIMS.                                             EL319
00626                                                                   EL319
00627      READ ELMSTR NEXT INTO CLAIM-MASTER.                          EL319
00628                                                                   EL319
00629      IF  W-CLMS-FILE-STATUS = '00'                                EL319
00630          NEXT SENTENCE                                            EL319
00631      ELSE                                                         EL319
00632          IF  W-CLMS-FILE-STATUS EQUAL '10'                        EL319
00633              MOVE 'Y'            TO W-END-OF-CLMS-FILE-SW         EL319
00634              GO TO 2200-EXIT                                      EL319
00635          ELSE                                                     EL319
00636              MOVE '**** ERROR READING CLAIMS FILE ******'         EL319
00637                                  TO WS-ABEND-MESSAGE              EL319
00638              MOVE '99'           TO WS-AC-1-2                     EL319
00639              MOVE W-CLMS-FILE-STATUS                              EL319
00640                                  TO WS-AC-3-4                     EL319
00641                                     WS-ABEND-FILE-STATUS          EL319
00642              MOVE WS-ABEND-CODE  TO WS-RETURN-CODE                EL319
00643              GO TO ABEND-PGM.                                     EL319
00644                                                                   EL319
00645      IF  CL-COMPANY-CD EQUAL DTE-CLASIC-COMPANY-CD                EL319
00646          ADD 1                   TO W-CLMS-RECORDS-READ           EL319
00647      ELSE                                                         EL319
00648          MOVE 'Y'                TO W-END-OF-CLMS-FILE-SW         EL319
00649          GO TO 2200-EXIT.                                         EL319
00650                                                                   EL319
00651      IF  CL-INCURRED-DT NOT GREATER THAN LOW-VALUES               EL319
00652              OR                                                   EL319
00653          CL-CERT-EFF-DT NOT GREATER THAN LOW-VALUES               EL319
00654          ADD +1 TO W-CLMS-RECORDS-BAD-DATE                        EL319
00655 *        MOVE 'N'                TO SW-QUALIFIED-SW               EL319
00656 *        PERFORM 2400-CREATE-SORT-RECORD THRU 2400-EXIT           EL319
00657          GO TO 2200-EXIT.                                         EL319
00658                                                                   EL319
00659      IF  CL-INCURRED-DT NOT GREATER THAN CL-CERT-EFF-DT           EL319
00660          ADD +1 TO W-CLMS-RECORDS-PRV-DATE                        EL319
00661 *        MOVE 'N'                TO SW-QUALIFIED-SW               EL319
00662 *        PERFORM 2400-CREATE-SORT-RECORD THRU 2400-EXIT           EL319
00663          GO TO 2200-EXIT.                                         EL319
00664                                                                   EL319
00665      IF  CL-INCURRED-DT LESS THAN W-START-BIN-DATE                EL319
00666 *        MOVE 'N'                TO SW-QUALIFIED-SW               EL319
00667 *        PERFORM 2400-CREATE-SORT-RECORD THRU 2400-EXIT           EL319
00668          GO TO 2200-EXIT.                                         EL319
00669                                                                   EL319
00670      MOVE CL-CERT-EFF-DT         TO DC-BIN-DATE-1.                EL319
00671      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-2.                EL319
00672      MOVE ZEROS                  TO DC-ELAPSED-DAYS               EL319
00673                                     DC-ELAPSED-MONTHS.            EL319
00674      MOVE '1'                    TO DC-OPTION-CODE.               EL319
00675      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL319
00676                                                                   EL319
00677      IF  DATE-CONVERSION-ERROR                                    EL319
00678          MOVE '**** EFF DATE VRS INCURRED DATE ERROR ****'        EL319
00679                                  TO WS-ABEND-MESSAGE              EL319
00680          MOVE '99'               TO WS-AC-1-2                     EL319
00681          MOVE '0'                TO WS-AC-3                       EL319
00682          MOVE DC-ERROR-CODE      TO WS-AC-4                       EL319
00683          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                EL319
00684          GO TO ABEND-PGM.                                         EL319
00685                                                                   EL319
00686      ADD +1                      TO W-CLMS-RECORDS-ELIGIBLE.      EL319
00687                                                                   EL319
00688      IF  DC-ELAPSED-DAYS LESS THAN +91                            EL319
00689          MOVE 'Y'                TO SW-QUALIFIED-SW               EL319
00690          PERFORM 2400-CREATE-SORT-RECORD THRU 2400-EXIT           EL319
00691      ELSE                                                         EL319
00692          MOVE 'N'                TO SW-QUALIFIED-SW               EL319
00693          PERFORM 2400-CREATE-SORT-RECORD THRU 2400-EXIT.          EL319
00694                                                                   EL319
00695  2200-EXIT.                                                       EL319
00696      EXIT.                                                        EL319
00697                                  EJECT                            EL319
00698  2400-CREATE-SORT-RECORD.                                         EL319
00699                                                                   EL319
00700      MOVE CL-CERT-KEY-DATA       TO SW-CONTROL.                   EL319
00701      MOVE CL-CERT-NO             TO SW-CERT-NO.                   EL319
00702      MOVE CL-CLAIM-NO            TO SW-CLAIM-NO.                  EL319
00703                                                                   EL319
00704      IF  SW-QUALIFIED                                             EL319
00705          MOVE CLAIM-MASTER       TO SW-CLAIMS-RECORD              EL319
00706          ADD +1                  TO W-CLMS-RECORDS-USED           EL319
00707          MOVE DC-ELAPSED-DAYS    TO SW-DAYS-DIFF                  EL319
00708          IF  DC-ELAPSED-DAYS GREATER THAN +60                     EL319
00709              MOVE '3'            TO SW-DAYS-SLOT                  EL319
00710          ELSE                                                     EL319
00711              IF  DC-ELAPSED-DAYS GREATER THAN +30                 EL319
00712                  MOVE '2'        TO SW-DAYS-SLOT                  EL319
00713              ELSE                                                 EL319
00714                  MOVE '1'        TO SW-DAYS-SLOT.                 EL319
00715                                                                   EL319
00716      RELEASE SORT-RECORD.                                         EL319
00717                                                                   EL319
00718  2400-EXIT.                                                       EL319
00719      EXIT.                                                        EL319
00720                                                                   EL319
00721  2999-EXIT.                                                       EL319
00722      EXIT.                                                        EL319
00723                                  EJECT                            EL319
00724  3000-PRINT-DATA SECTION.                                         EL319
00725                                                                   EL319
00726      OPEN INPUT  ELTRLR                                           EL319
00727                  ERACCT                                           EL319
00728                  MPPROD                                           EL319
00729           OUTPUT PRINTER.                                         EL319
00730                                                                   EL319
00731      IF  W-TRLR-FILE-STATUS = '00' OR '97'                        EL319
00732          NEXT SENTENCE                                            EL319
00733      ELSE                                                         EL319
00734          MOVE '**** OPEN TRAILER FILE ERROR ****'                 EL319
00735                                  TO WS-ABEND-MESSAGE              EL319
00736          MOVE '1203'             TO WS-RETURN-CODE                EL319
00737          MOVE W-TRLR-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL319
00738          GO TO ABEND-PGM.                                         EL319
00739                                                                   EL319
00740      IF  W-AM-FILE-STATUS = '00' OR '97'                          EL319
00741          NEXT SENTENCE                                            EL319
00742      ELSE                                                         EL319
00743          MOVE '**** OPEN ACCOUNT FILE ERROR ****'                 EL319
00744                                  TO WS-ABEND-MESSAGE              EL319
00745          MOVE '1201'             TO WS-RETURN-CODE                EL319
00746          MOVE W-AM-FILE-STATUS   TO WS-ABEND-FILE-STATUS          EL319
00747          GO TO ABEND-PGM.                                         EL319
00748                                                                   EL319
00749      IF (W-PD-FILE-STATUS IS EQUAL TO '00' OR '97')               EL319
00750          NEXT SENTENCE                                            EL319
00751      ELSE                                                         EL319
00752          MOVE '**** OPEN PRODUCER FILE ERROR ****'                EL319
00753                                  TO WS-ABEND-MESSAGE              EL319
00754          MOVE '1206'             TO WS-RETURN-CODE                EL319
00755          MOVE W-PD-FILE-STATUS   TO WS-ABEND-FILE-STATUS          EL319
00756          GO TO ABEND-PGM.                                         EL319
00757                                                                   EL319
00758      PERFORM 3900-RETURN-SORTED-DATA THRU 3900-EXIT.              EL319
00759                                                                   EL319
00760      IF  W-END-OF-SORT-DATA                                       EL319
00761          PERFORM 3820-HEADER THRU 3820-EXIT                       EL319
00762          MOVE ' NO SUITABLE CLAIM DATA FOUND'                     EL319
00763                                  TO PRT                           EL319
00764          MOVE +3                 TO W-LINES-USED                  EL319
00765          GO TO 3999-EXIT                                          EL319
00766      ELSE                                                         EL319
00767          MOVE SPACES             TO W-CONTROL                     EL319
00768                                     W-DAYS-SLOT.                  EL319
00769                                                                   EL319
00770      PERFORM 3100-PROCESS-RETURN-DATA THRU 3100-EXIT              EL319
00771              UNTIL                                                EL319
00772          W-END-OF-SORT-DATA.                                      EL319
00773                                                                   EL319
00774      PERFORM 3200-SLOT-CHANGE THRU 3200-EXIT.                     EL319
00775      PERFORM 3240-ACCOUNT-CHANGE THRU 3240-EXIT.                  EL319
00776      PERFORM 3260-CARRIER-CHANGE THRU 3260-EXIT.                  EL319
00777      PERFORM 3280-FINAL-TOTALS THRU 3280-EXIT.                    EL319
00778                                                                   EL319
00779      GO TO 3999-EXIT.                                             EL319
00780                                                                   EL319
00781  3000-EXIT.                                                       EL319
00782      EXIT.                                                        EL319
00783                                  EJECT                            EL319
00784  3100-PROCESS-RETURN-DATA.                                        EL319
00785                                                                   EL319
00786      IF  SW-CARRIER NOT EQUAL W-CARRIER                           EL319
00787          IF  W-CAR-COUNT GREATER THAN ZERO                        EL319
00788              PERFORM 3200-SLOT-CHANGE THRU 3200-EXIT              EL319
00789              PERFORM 3240-ACCOUNT-CHANGE THRU 3240-EXIT           EL319
00790              PERFORM 3260-CARRIER-CHANGE THRU 3260-EXIT           EL319
00791              MOVE SW-STATE       TO W-HDR4-STATE                  EL319
00792              MOVE SW-CARRIER     TO W-HDR4-CARRIER                EL319
00793              MOVE SW-GROUPING    TO W-HDR4-GROUPING               EL319
00794              MOVE SW-CONTROL     TO W-CONTROL                     EL319
00795              MOVE SW-DAYS-SLOT   TO W-DAYS-SLOT                   EL319
00796          ELSE                                                     EL319
00797              PERFORM 3208-REINITIALIZE-SLOT THRU 3208-EXIT        EL319
00798              PERFORM 3248-REINITIALIZE-ACCOUNT THRU 3248-EXIT     EL319
00799              PERFORM 3268-REINITIALIZE-CARRIER THRU 3268-EXIT     EL319
00800              MOVE SW-STATE       TO W-HDR4-STATE                  EL319
00801              MOVE SW-CARRIER     TO W-HDR4-CARRIER                EL319
00802              MOVE SW-GROUPING    TO W-HDR4-GROUPING               EL319
00803              MOVE SW-CONTROL     TO W-CONTROL                     EL319
00804              MOVE SW-DAYS-SLOT   TO W-DAYS-SLOT                   EL319
00805      ELSE                                                         EL319
00806          IF  SW-GROUPING NOT EQUAL W-GROUPING                     EL319
00807              IF  W-ACCT-COUNT GREATER THAN ZEROS                  EL319
00808                  PERFORM 3200-SLOT-CHANGE THRU 3200-EXIT          EL319
00809                  PERFORM 3240-ACCOUNT-CHANGE THRU 3240-EXIT       EL319
00810                  MOVE SW-STATE   TO W-HDR4-STATE                  EL319
00811                  MOVE SW-CARRIER TO W-HDR4-CARRIER                EL319
00812                  MOVE SW-GROUPING                                 EL319
00813                                  TO W-HDR4-GROUPING               EL319
00814                  MOVE SW-CONTROL TO W-CONTROL                     EL319
00815                  MOVE SW-DAYS-SLOT                                EL319
00816                                  TO W-DAYS-SLOT                   EL319
00817              ELSE                                                 EL319
00818                  PERFORM 3208-REINITIALIZE-SLOT THRU 3208-EXIT    EL319
00819                  PERFORM 3248-REINITIALIZE-ACCOUNT THRU 3248-EXIT EL319
00820                  MOVE SW-STATE   TO W-HDR4-STATE                  EL319
00821                  MOVE SW-CARRIER TO W-HDR4-CARRIER                EL319
00822                  MOVE SW-GROUPING                                 EL319
00823                                  TO W-HDR4-GROUPING               EL319
00824                  MOVE SW-CONTROL TO W-CONTROL                     EL319
00825                  MOVE SW-DAYS-SLOT                                EL319
00826                                  TO W-DAYS-SLOT                   EL319
00827          ELSE                                                     EL319
00828              IF  SW-STATE NOT EQUAL W-STATE                       EL319
00829                  IF  W-ACCT-COUNT GREATER THAN ZEROS              EL319
00830                      PERFORM 3200-SLOT-CHANGE THRU 3200-EXIT      EL319
00831                      PERFORM 3240-ACCOUNT-CHANGE THRU 3240-EXIT   EL319
00832                      MOVE SW-STATE                                EL319
00833                                  TO W-HDR4-STATE                  EL319
00834                      MOVE SW-CARRIER                              EL319
00835                                  TO W-HDR4-CARRIER                EL319
00836                      MOVE SW-GROUPING                             EL319
00837                                  TO W-HDR4-GROUPING               EL319
00838                      MOVE SW-CONTROL                              EL319
00839                                  TO W-CONTROL                     EL319
00840                      MOVE SW-DAYS-SLOT                            EL319
00841                                  TO W-DAYS-SLOT                   EL319
00842                  ELSE                                             EL319
00843                      PERFORM 3208-REINITIALIZE-SLOT               EL319
00844                          THRU 3208-EXIT                           EL319
00845                      PERFORM 3248-REINITIALIZE-ACCOUNT            EL319
00846                          THRU 3248-EXIT                           EL319
00847                      MOVE SW-STATE                                EL319
00848                                  TO W-HDR4-STATE                  EL319
00849                      MOVE SW-CARRIER                              EL319
00850                                  TO W-HDR4-CARRIER                EL319
00851                      MOVE SW-GROUPING                             EL319
00852                                  TO W-HDR4-GROUPING               EL319
00853                      MOVE SW-CONTROL                              EL319
00854                                  TO W-CONTROL                     EL319
00855                      MOVE SW-DAYS-SLOT                            EL319
00856                                  TO W-DAYS-SLOT                   EL319
00857              ELSE                                                 EL319
00858                  IF  SW-ACCOUNT NOT EQUAL W-ACCOUNT               EL319
00859                      IF  W-ACCT-COUNT GREATER THAN ZEROS          EL319
00860                          PERFORM 3200-SLOT-CHANGE THRU 3200-EXIT  EL319
00861                          PERFORM 3240-ACCOUNT-CHANGE              EL319
00862                              THRU 3240-EXIT                       EL319
00863                          MOVE SW-CONTROL                          EL319
00864                                  TO W-CONTROL                     EL319
00865                          MOVE SW-DAYS-SLOT                        EL319
00866                                  TO W-DAYS-SLOT                   EL319
00867                      ELSE                                         EL319
00868                          PERFORM 3208-REINITIALIZE-SLOT           EL319
00869                              THRU 3208-EXIT                       EL319
00870                          PERFORM 3248-REINITIALIZE-ACCOUNT        EL319
00871                              THRU 3248-EXIT                       EL319
00872                          MOVE SW-CONTROL                          EL319
00873                                  TO W-CONTROL                     EL319
00874                          MOVE SW-DAYS-SLOT                        EL319
00875                                  TO W-DAYS-SLOT                   EL319
00876                  ELSE                                             EL319
00877                      IF  SW-DAYS-SLOT NOT EQUAL W-DAYS-SLOT       EL319
00878                          IF  W-SLOT-COUNT GREATER THAN ZERO       EL319
00879                              PERFORM 3200-SLOT-CHANGE             EL319
00880                                  THRU 3200-EXIT                   EL319
00881                              MOVE SW-DAYS-SLOT                    EL319
00882                                  TO W-DAYS-SLOT                   EL319
00883                          ELSE                                     EL319
00884                              PERFORM 3208-REINITIALIZE-SLOT       EL319
00885                                  THRU 3208-EXIT                   EL319
00886                              MOVE SW-DAYS-SLOT                    EL319
00887                                  TO W-DAYS-SLOT.                  EL319
00888                                                                   EL319
00889      ADD +1                      TO W-ACCT-ELIGIBLE               EL319
00890                                     W-CAR-ELIGIBLE                EL319
00891                                     W-FIN-ELIGIBLE.               EL319
00892                                                                   EL319
00893      IF  SW-CERT-NOT-QUALIFIED                                    EL319
00894          PERFORM 3900-RETURN-SORTED-DATA THRU 3900-EXIT           EL319
00895          GO TO 3100-EXIT.                                         EL319
00896                                                                   EL319
00897      MOVE SW-CLAIMS-RECORD       TO CLAIM-MASTER.                 EL319
00898      PERFORM 3120-GET-DIAGNOSIS THRU 3120-EXIT.                   EL319
00899      PERFORM 3140-FORMAT-NAME THRU 3140-EXIT.                     EL319
00900      PERFORM 3160-GET-STATUS THRU 3160-EXIT.                      EL319
00901      PERFORM 3180-DETERMINE-INCURRED-DATE THRU 3180-EXIT.         EL319
00902                                                                   EL319
00903      MOVE SW-CERT-NO             TO W-DT-CERT-NO.                 EL319
00904      MOVE SW-CLAIM-NO            TO W-DT-CLAIM-NO.                EL319
00905      MOVE SW-DAYS-DIFF           TO W-DT-DAYS-DIFF.               EL319
00906      MOVE CL-CLAIM-TYPE          TO W-DT-CLAIM-TYPE.              EL319
00907      MOVE CL-TOTAL-PAID-AMT      TO W-DT-TOTAL-PAID-AMT.          EL319
00908                                                                   EL319
00909      ADD +1                      TO W-ACCT-COUNT                  EL319
00910                                     W-CAR-COUNT                   EL319
00911                                     W-FIN-COUNT                   EL319
00912                                     W-SLOT-COUNT.                 EL319
00913      ADD CL-TOTAL-PAID-AMT       TO W-SLOT-AMOUNT.                EL319
00914                                                                   EL319
00915      IF  W-ACCT-NAME-NOT-PRINTED                                  EL319
00916          IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                 EL319
00917              PERFORM 3350-GET-PRODUCER-NAME THRU 3350-EXIT        EL319
00918              PERFORM 3400-PRINT-ACCOUNT-LINE THRU 3400-EXIT       EL319
00919              MOVE 'Y'            TO W-ACCT-NAME-PRINTED-SW        EL319
00920          ELSE                                                     EL319
00921              PERFORM 3300-GET-ACCOUNT-NAME THRU 3300-EXIT         EL319
00922              PERFORM 3400-PRINT-ACCOUNT-LINE THRU 3400-EXIT       EL319
00923              MOVE 'Y'            TO W-ACCT-NAME-PRINTED-SW.       EL319
00924                                                                   EL319
00925      MOVE W-DETAIL-LINE          TO PRT.                          EL319
00926      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
00927      MOVE +1                     TO W-LINES-USED.                 EL319
00928                                                                   EL319
00929      PERFORM 3900-RETURN-SORTED-DATA THRU 3900-EXIT.              EL319
00930                                                                   EL319
00931  3100-EXIT.                                                       EL319
00932      EXIT.                                                        EL319
00933                                  EJECT                            EL319
00934  3120-GET-DIAGNOSIS.                                              EL319
00935                                                                   EL319
00936      MOVE CL-COMPANY-CD          TO AT-COMPANY-CD.                EL319
00937      MOVE CL-CERT-CARRIER        TO AT-CARRIER.                   EL319
00938      MOVE CL-CLAIM-NO            TO AT-CLAIM-NO.                  EL319
00939      MOVE CL-CERT-NO             TO AT-CERT-NO.                   EL319
00940      MOVE +90                    TO AT-SEQUENCE-NO.               EL319
00941                                                                   EL319
00942      READ ELTRLR.                                                 EL319
00943                                                                   EL319
00944      IF  W-TRLR-FILE-STATUS = '10'                                EL319
00945              OR                                                   EL319
00946          W-TRLR-FILE-STATUS = '23'                                EL319
00947          MOVE 'HAS NO DIAGNOSTIC TRAILER'                         EL319
00948                                  TO W-DIAGNOSIS                   EL319
00949          GO TO 3120-EXIT                                          EL319
00950      ELSE                                                         EL319
00951          IF  W-TRLR-FILE-STATUS NOT = ZERO                        EL319
00952              MOVE 'ERROR OCCURED READ TRAILER RECORD - ELTRLR'    EL319
00953                                  TO WS-ABEND-MESSAGE              EL319
00954              MOVE W-TRLR-FILE-STATUS                              EL319
00955                                  TO WS-ABEND-FILE-STATUS          EL319
00956              PERFORM ABEND-PGM.                                   EL319
00957                                                                   EL319
00958      MOVE AT-INFO-LINE-1         TO W-DT-DIAGNOSIS.               EL319
00959                                                                   EL319
00960  3120-EXIT.                                                       EL319
00961      EXIT.                                                        EL319
00962                                  EJECT                            EL319
00963  3140-FORMAT-NAME.                                                EL319
00964                                                                   EL319
00965       MOVE SPACES                TO W-NAME.                       EL319
00966       MOVE +1                    TO W-NDX2.                       EL319
00967                                                                   EL319
00968       MOVE CL-INSURED-LAST-NAME  TO W-WORK-NAME.                  EL319
00969       PERFORM 3142-MOVE-NAME THRU 3142-EXIT                       EL319
00970               VARYING                                             EL319
00971           W-NDX FROM 1 BY 1                                       EL319
00972               UNTIL                                               EL319
00973           W-NDX GREATER THAN +15.                                 EL319
00974                                                                   EL319
00975       IF  CL-INSURED-1ST-NAME GREATER THAN SPACES                 EL319
00976           MOVE ','               TO W-NAME-CHAR (W-NDX2)          EL319
00977           ADD +2                 TO W-NDX2                        EL319
00978           MOVE CL-INSURED-1ST-NAME                                EL319
00979                                  TO W-WORK-NAME                   EL319
00980           PERFORM 3142-MOVE-NAME THRU 3142-EXIT                   EL319
00981                   VARYING                                         EL319
00982               W-NDX FROM 1 BY 1                                   EL319
00983                   UNTIL                                           EL319
00984               W-NDX GREATER THAN +12                              EL319
00985           ADD +1                 TO W-NDX2                        EL319
00986           IF  CL-INSURED-MID-INIT GREATER THAN SPACES             EL319
00987               MOVE CL-INSURED-MID-INIT TO W-NAME-CHAR (W-NDX2)    EL319
00988               ADD +1             TO W-NDX2                        EL319
00989               MOVE '.'           TO W-NAME-CHAR (W-NDX2).         EL319
00990                                                                   EL319
00991      MOVE W-NAME                 TO W-DT-INSURED-NAME.            EL319
00992                                                                   EL319
00993  3140-EXIT.                                                       EL319
00994      EXIT.                                                        EL319
00995                                  EJECT                            EL319
00996  3142-MOVE-NAME.                                                  EL319
00997                                                                   EL319
00998      IF  W-WN-CHAR (W-NDX) GREATER THAN SPACES                    EL319
00999          MOVE W-WN-CHAR (W-NDX)  TO W-NAME-CHAR (W-NDX2)          EL319
01000          ADD +1                  TO W-NDX2                        EL319
01001      ELSE                                                         EL319
01002          COMPUTE W-NDX3 = W-NDX + 1                               EL319
01003          IF  W-WN-CHAR (W-NDX3) GREATER THAN SPACES               EL319
01004              ADD +16             TO W-NDX.                        EL319
01005                                                                   EL319
01006  3142-EXIT.                                                       EL319
01007      EXIT.                                                        EL319
01008                                  EJECT                            EL319
01009  3160-GET-STATUS.                                                 EL319
01010                                                                   EL319
01011      IF  CL-CLAIM-STATUS EQUAL 'O'                                EL319
01012          MOVE 'OPEN'             TO W-DT-CLAIM-STATUS             EL319
01013      ELSE                                                         EL319
01014          IF  CL-LAST-CLOSE-REASON EQUAL '1'                       EL319
01015              MOVE 'FINAL PAID'   TO W-DT-CLAIM-STATUS             EL319
01016          ELSE                                                     EL319
01017              IF  CL-LAST-CLOSE-REASON EQUAL '2'                   EL319
01018                  MOVE 'DENIED'   TO W-DT-CLAIM-STATUS             EL319
01019              ELSE                                                 EL319
01020                  IF  CL-LAST-CLOSE-REASON EQUAL '3'               EL319
01021                      MOVE 'AUTO CLOSED'                           EL319
01022                                  TO W-DT-CLAIM-STATUS             EL319
01023                  ELSE                                             EL319
01024                      MOVE 'MAN. CLOSED'                           EL319
01025                                  TO W-DT-CLAIM-STATUS.            EL319
01026                                                                   EL319
01027  3160-EXIT.                                                       EL319
01028      EXIT.                                                        EL319
01029                                  EJECT                            EL319
01030  3180-DETERMINE-INCURRED-DATE.                                    EL319
01031                                                                   EL319
01032      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.                EL319
01033      MOVE ' '                    TO DC-OPTION-CODE.               EL319
01034      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL319
01035                                                                   EL319
01036      IF  DATE-CONVERSION-ERROR                                    EL319
01037          MOVE '**** INCURRED DATE CONVERSION ERROR ****'          EL319
01038                                  TO WS-ABEND-MESSAGE              EL319
01039          MOVE '99'               TO WS-AC-1-2                     EL319
01040          MOVE '0'                TO WS-AC-3                       EL319
01041          MOVE DC-ERROR-CODE      TO WS-AC-4                       EL319
01042          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                EL319
01043          GO TO ABEND-PGM                                          EL319
01044      ELSE                                                         EL319
01045          MOVE DC-GREG-DATE-1-EDIT                                 EL319
01046                                  TO W-DT-INCURRED-DATE.           EL319
01047                                                                   EL319
01048  3180-EXIT.                                                       EL319
01049      EXIT.                                                        EL319
01050                                  EJECT                            EL319
01051  3200-SLOT-CHANGE.                                                EL319
01052                                                                   EL319
01053      IF  W-SLOT-COUNT NOT GREATER THAN ZEROS                      EL319
01054          PERFORM 3208-REINITIALIZE-SLOT THRU 3208-EXIT            EL319
01055          GO TO 3200-EXIT.                                         EL319
01056                                                                   EL319
01057      MOVE W-SLOT-COUNT           TO W-TDL-COUNT.                  EL319
01058      MOVE W-SLOT-AMOUNT          TO W-TDL-AMOUNT.                 EL319
01059                                                                   EL319
01060      IF  W-DAYS-SLOT EQUAL 1                                      EL319
01061          MOVE '30'               TO W-TDL-DAYS-GRP                EL319
01062          ADD W-SLOT-COUNT        TO W-ACCT-30-DAY-COUNT           EL319
01063                                     W-CAR-30-DAY-COUNT            EL319
01064                                     W-FIN-30-DAY-COUNT            EL319
01065          ADD W-SLOT-AMOUNT       TO W-ACCT-30-DAY-AMOUNT          EL319
01066                                     W-CAR-30-DAY-AMOUNT           EL319
01067                                     W-FIN-30-DAY-AMOUNT           EL319
01068      ELSE                                                         EL319
01069          IF  W-DAYS-SLOT EQUAL 2                                  EL319
01070              MOVE '60'           TO W-TDL-DAYS-GRP                EL319
01071              ADD W-SLOT-COUNT    TO W-ACCT-60-DAY-COUNT           EL319
01072                                     W-CAR-60-DAY-COUNT            EL319
01073                                     W-FIN-60-DAY-COUNT            EL319
01074              ADD W-SLOT-AMOUNT   TO W-ACCT-60-DAY-AMOUNT          EL319
01075                                     W-CAR-60-DAY-AMOUNT           EL319
01076                                     W-FIN-60-DAY-AMOUNT           EL319
01077          ELSE                                                     EL319
01078              IF  W-DAYS-SLOT EQUAL 3                              EL319
01079                  MOVE '90'       TO W-TDL-DAYS-GRP                EL319
01080                  ADD W-SLOT-COUNT                                 EL319
01081                                  TO W-ACCT-90-DAY-COUNT           EL319
01082                                     W-CAR-90-DAY-COUNT            EL319
01083                                     W-FIN-90-DAY-COUNT            EL319
01084                  ADD W-SLOT-AMOUNT                                EL319
01085                                  TO W-ACCT-90-DAY-AMOUNT          EL319
01086                                     W-CAR-90-DAY-AMOUNT           EL319
01087                                     W-FIN-90-DAY-AMOUNT.          EL319
01088                                                                   EL319
01089      MOVE SPACES                 TO W-TDL-FIN-GRP.                EL319
01090      MOVE W-TOTAL-DAYS-LINE      TO PRT.                          EL319
01091      MOVE +2                     TO W-LINES-USED.                 EL319
01092      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01093      PERFORM 3208-REINITIALIZE-SLOT THRU 3208-EXIT.               EL319
01094                                                                   EL319
01095  3200-EXIT.                                                       EL319
01096      EXIT.                                                        EL319
01097                                  EJECT                            EL319
01098  3208-REINITIALIZE-SLOT.                                          EL319
01099                                                                   EL319
01100      MOVE ZEROS                  TO W-SLOT-COUNT                  EL319
01101                                     W-SLOT-AMOUNT.                EL319
01102                                                                   EL319
01103  3208-EXIT.                                                       EL319
01104      EXIT.                                                        EL319
01105  3240-ACCOUNT-CHANGE.                                             EL319
01106                                                                   EL319
01107      MOVE W-ACCT-COUNT           TO W-TYL-COUNT.                  EL319
01108      MOVE W-ACCT-ELIGIBLE        TO W-TYL-ELIGIBLE.               EL319
01109                                                                   EL319
01110      COMPUTE W-TYL-AMOUNT                                         EL319
01111          = W-ACCT-30-DAY-AMOUNT                                   EL319
01112          + W-ACCT-60-DAY-AMOUNT                                   EL319
01113          + W-ACCT-90-DAY-AMOUNT.                                  EL319
01114                                                                   EL319
01115      COMPUTE W-PERCENT ROUNDED                                    EL319
01116          = W-ACCT-COUNT                                           EL319
01117          / W-ACCT-ELIGIBLE                                        EL319
01118          * 100.                                                   EL319
01119      COMPUTE W-TYL-PERCENT ROUNDED = W-PERCENT.                   EL319
01120      MOVE '% OF ELIGIBLE ACCOUNT CLAIMS'                          EL319
01121                                  TO W-TYL-PERCENT-TITLE.          EL319
01122                                                                   EL319
01123      MOVE W-TYT-ACCOUNT          TO W-TYL-TITLE.                  EL319
01124      MOVE +2                     TO W-LINES-USED.                 EL319
01125      MOVE W-TOTAL-YEAR-LINE      TO PRT.                          EL319
01126      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01127                                                                   EL319
01128      MOVE SPACES                 TO W-ACCT-NAME-PRINTED-SW.       EL319
01129                                                                   EL319
01130      PERFORM 3248-REINITIALIZE-ACCOUNT THRU 3248-EXIT.            EL319
01131                                                                   EL319
01132  3240-EXIT.                                                       EL319
01133      EXIT.                                                        EL319
01134                                  EJECT                            EL319
01135  3248-REINITIALIZE-ACCOUNT.                                       EL319
01136                                                                   EL319
01137      MOVE +0                     TO W-ACCT-COUNT                  EL319
01138                                     W-ACCT-ELIGIBLE               EL319
01139                                     W-ACCT-30-DAY-AMOUNT          EL319
01140                                     W-ACCT-30-DAY-COUNT           EL319
01141                                     W-ACCT-60-DAY-AMOUNT          EL319
01142                                     W-ACCT-60-DAY-COUNT           EL319
01143                                     W-ACCT-90-DAY-AMOUNT          EL319
01144                                     W-ACCT-90-DAY-COUNT.          EL319
01145                                                                   EL319
01146  3248-EXIT.                                                       EL319
01147      EXIT.                                                        EL319
01148                                  EJECT                            EL319
01149  3260-CARRIER-CHANGE.                                             EL319
01150                                                                   EL319
01151      MOVE +99                    TO W-LINE-COUNT.                 EL319
01152      MOVE SPACES                 TO W-HDR4-GRP-TITLE              EL319
01153                                     W-HDR4-GROUPING               EL319
01154                                     W-HDR4-STATE-TITLE            EL319
01155                                     W-HDR4-STATE.                 EL319
01156                                                                   EL319
01157      MOVE +3                     TO W-LINES-USED.                 EL319
01158      MOVE W-TOTAL-TITLE-LINE     TO PRT.                          EL319
01159      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01160                                                                   EL319
01161      MOVE '30'                   TO W-TDL-DAYS-GRP.               EL319
01162      MOVE W-CAR-30-DAY-COUNT     TO W-TDL-COUNT.                  EL319
01163      MOVE W-CAR-ELIGIBLE         TO W-TDL-ELIGIBLE.               EL319
01164      MOVE W-CAR-30-DAY-AMOUNT    TO W-TDL-AMOUNT.                 EL319
01165      COMPUTE W-PERCENT ROUNDED                                    EL319
01166          = W-CAR-30-DAY-COUNT                                     EL319
01167          / W-CAR-ELIGIBLE                                         EL319
01168          * 100.                                                   EL319
01169      COMPUTE W-TDL-PERCENT ROUNDED = W-PERCENT.                   EL319
01170      MOVE '% OF ELIGIBLE CARRIER CLAIMS'                          EL319
01171                                  TO W-TDL-PERCENT-TITLE.          EL319
01172      MOVE ' OF'                  TO W-TDL-OF.                     EL319
01173      MOVE ' OR'                  TO W-TDL-OR.                     EL319
01174      MOVE +1                     TO W-LINES-USED.                 EL319
01175      MOVE W-TOTAL-DAYS-LINE      TO PRT.                          EL319
01176      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01177                                                                   EL319
01178      MOVE '60'                   TO W-TDL-DAYS-GRP.               EL319
01179      MOVE W-CAR-60-DAY-COUNT     TO W-TDL-COUNT.                  EL319
01180      MOVE W-CAR-ELIGIBLE         TO W-TDL-ELIGIBLE.               EL319
01181      MOVE W-CAR-60-DAY-AMOUNT    TO W-TDL-AMOUNT.                 EL319
01182      COMPUTE W-PERCENT ROUNDED                                    EL319
01183          = W-CAR-60-DAY-COUNT                                     EL319
01184          / W-CAR-ELIGIBLE                                         EL319
01185          * 100.                                                   EL319
01186      COMPUTE W-TDL-PERCENT ROUNDED = W-PERCENT.                   EL319
01187      MOVE +1                     TO W-LINES-USED.                 EL319
01188      MOVE W-TOTAL-DAYS-LINE      TO PRT.                          EL319
01189      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01190                                                                   EL319
01191      MOVE '90'                   TO W-TDL-DAYS-GRP.               EL319
01192      MOVE W-CAR-90-DAY-COUNT     TO W-TDL-COUNT.                  EL319
01193      MOVE W-CAR-ELIGIBLE         TO W-TDL-ELIGIBLE.               EL319
01194      MOVE W-CAR-90-DAY-AMOUNT    TO W-TDL-AMOUNT.                 EL319
01195      COMPUTE W-PERCENT ROUNDED                                    EL319
01196          = W-CAR-90-DAY-COUNT                                     EL319
01197          / W-CAR-ELIGIBLE                                         EL319
01198          * 100.                                                   EL319
01199      COMPUTE W-TDL-PERCENT ROUNDED = W-PERCENT.                   EL319
01200      MOVE +1                     TO W-LINES-USED.                 EL319
01201      MOVE W-TOTAL-DAYS-LINE      TO PRT.                          EL319
01202      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01203                                                                   EL319
01204      MOVE W-CAR-COUNT            TO W-TYL-COUNT.                  EL319
01205      MOVE W-CAR-ELIGIBLE         TO W-TYL-ELIGIBLE.               EL319
01206                                                                   EL319
01207      COMPUTE W-TYL-AMOUNT                                         EL319
01208          = W-CAR-30-DAY-AMOUNT                                    EL319
01209          + W-CAR-60-DAY-AMOUNT                                    EL319
01210          + W-CAR-90-DAY-AMOUNT.                                   EL319
01211                                                                   EL319
01212      COMPUTE W-PERCENT ROUNDED                                    EL319
01213          = W-CAR-COUNT                                            EL319
01214          / W-CAR-ELIGIBLE                                         EL319
01215          * 100.                                                   EL319
01216                                                                   EL319
01217      COMPUTE W-TYL-PERCENT ROUNDED = W-PERCENT.                   EL319
01218      MOVE '% OF ELIGIBLE CARRIER CLAIMS'                          EL319
01219                                  TO W-TYL-PERCENT-TITLE.          EL319
01220      MOVE W-TYT-CARRIER          TO W-TYL-TITLE.                  EL319
01221      MOVE +1                     TO W-LINES-USED.                 EL319
01222      MOVE W-TOTAL-YEAR-LINE      TO PRT.                          EL319
01223      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01224                                                                   EL319
01225      PERFORM 3268-REINITIALIZE-CARRIER THRU 3268-EXIT.            EL319
01226      MOVE 'GROUPING: '           TO W-HDR4-GRP-TITLE.             EL319
01227      MOVE 'STATE: '              TO W-HDR4-STATE-TITLE.           EL319
01228                                                                   EL319
01229      MOVE +1                     TO W-LINES-USED.                 EL319
01230      MOVE +99                    TO W-LINE-COUNT.                 EL319
01231                                                                   EL319
01232  3260-EXIT.                                                       EL319
01233      EXIT.                                                        EL319
01234                                  EJECT                            EL319
01235  3268-REINITIALIZE-CARRIER.                                       EL319
01236                                                                   EL319
01237      MOVE +0                     TO W-CAR-COUNT                   EL319
01238                                     W-CAR-ELIGIBLE                EL319
01239                                     W-CAR-30-DAY-AMOUNT           EL319
01240                                     W-CAR-30-DAY-COUNT            EL319
01241                                     W-CAR-60-DAY-AMOUNT           EL319
01242                                     W-CAR-60-DAY-COUNT            EL319
01243                                     W-CAR-90-DAY-AMOUNT           EL319
01244                                     W-CAR-90-DAY-COUNT.           EL319
01245                                                                   EL319
01246  3268-EXIT.                                                       EL319
01247      EXIT.                                                        EL319
01248                                  EJECT                            EL319
01249  3280-FINAL-TOTALS.                                               EL319
01250                                                                   EL319
01251      MOVE SPACES                 TO W-HDR-4.                      EL319
01252                                                                   EL319
01253      MOVE +99                    TO W-LINE-COUNT.                 EL319
01254      MOVE +2                     TO W-LINES-USED.                 EL319
01255      MOVE ' COMPANY TOTAL'       TO PRT.                          EL319
01256      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01257                                                                   EL319
01258      MOVE '30'                   TO W-TDL-DAYS-GRP.               EL319
01259      MOVE W-FIN-30-DAY-COUNT     TO W-TDL-COUNT.                  EL319
01260      MOVE W-FIN-ELIGIBLE         TO W-TDL-ELIGIBLE.               EL319
01261      MOVE W-FIN-30-DAY-AMOUNT    TO W-TDL-AMOUNT.                 EL319
01262      COMPUTE W-PERCENT ROUNDED                                    EL319
01263          = W-FIN-30-DAY-COUNT                                     EL319
01264          / W-FIN-ELIGIBLE                                         EL319
01265          * 100.                                                   EL319
01266      COMPUTE W-TDL-PERCENT ROUNDED = W-PERCENT.                   EL319
01267      MOVE '% OF CLAIM RECORDS'   TO W-TDL-PERCENT-TITLE.          EL319
01268      MOVE +1                     TO W-LINES-USED.                 EL319
01269      MOVE W-TOTAL-DAYS-LINE      TO PRT.                          EL319
01270      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01271                                                                   EL319
01272      MOVE '60'                   TO W-TDL-DAYS-GRP.               EL319
01273      MOVE W-FIN-60-DAY-COUNT     TO W-TDL-COUNT.                  EL319
01274      MOVE W-FIN-ELIGIBLE         TO W-TDL-ELIGIBLE.               EL319
01275      MOVE W-FIN-60-DAY-AMOUNT    TO W-TDL-AMOUNT.                 EL319
01276      COMPUTE W-PERCENT ROUNDED                                    EL319
01277          = W-FIN-60-DAY-COUNT                                     EL319
01278          / W-FIN-ELIGIBLE                                         EL319
01279          * 100.                                                   EL319
01280      COMPUTE W-TDL-PERCENT ROUNDED = W-PERCENT.                   EL319
01281      MOVE +1                     TO W-LINES-USED.                 EL319
01282      MOVE W-TOTAL-DAYS-LINE      TO PRT.                          EL319
01283      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01284                                                                   EL319
01285      MOVE '90'                   TO W-TDL-DAYS-GRP.               EL319
01286      MOVE W-FIN-90-DAY-COUNT     TO W-TDL-COUNT.                  EL319
01287      MOVE W-FIN-ELIGIBLE         TO W-TDL-ELIGIBLE.               EL319
01288      MOVE W-FIN-90-DAY-AMOUNT    TO W-TDL-AMOUNT.                 EL319
01289      COMPUTE W-PERCENT ROUNDED                                    EL319
01290          = W-FIN-90-DAY-COUNT                                     EL319
01291          / W-FIN-ELIGIBLE                                         EL319
01292          * 100.                                                   EL319
01293      COMPUTE W-TDL-PERCENT ROUNDED = W-PERCENT.                   EL319
01294      MOVE +1                     TO W-LINES-USED.                 EL319
01295      MOVE W-TOTAL-DAYS-LINE      TO PRT.                          EL319
01296      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01297                                                                   EL319
01298      MOVE W-FIN-COUNT            TO W-TYL-COUNT.                  EL319
01299      MOVE W-FIN-ELIGIBLE         TO W-TYL-ELIGIBLE.               EL319
01300                                                                   EL319
01301      COMPUTE W-PERCENT ROUNDED                                    EL319
01302          = W-FIN-COUNT                                            EL319
01303          / W-FIN-ELIGIBLE                                         EL319
01304          * 100.                                                   EL319
01305      COMPUTE W-TYL-PERCENT ROUNDED = W-PERCENT.                   EL319
01306      MOVE '% OF CLAIM RECORDS'   TO W-TYL-PERCENT-TITLE.          EL319
01307                                                                   EL319
01308      COMPUTE W-TYL-AMOUNT                                         EL319
01309          = W-FIN-30-DAY-AMOUNT                                    EL319
01310          + W-FIN-60-DAY-AMOUNT                                    EL319
01311          + W-FIN-90-DAY-AMOUNT.                                   EL319
01312      MOVE W-TYT-COMPANY          TO W-TYL-TITLE.                  EL319
01313      MOVE +1                     TO W-LINES-USED.                 EL319
01314      MOVE W-TOTAL-YEAR-LINE      TO PRT.                          EL319
01315      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01316                                                                   EL319
01317      MOVE W-CLMS-RECORDS-READ    TO W-EXCEPT-COUNT.               EL319
01318      MOVE ' TOTAL CLAIMS READ.'  TO W-EXCEPT-TITLE.               EL319
01319      MOVE +3                     TO W-LINES-USED.                 EL319
01320      MOVE W-EXCEPTION-LINE       TO PRT.                          EL319
01321      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01322                                                                   EL319
01323      MOVE W-CLMS-RECORDS-USED    TO W-EXCEPT-COUNT.               EL319
01324      MOVE ' TOTAL CLAIMS USED.'  TO W-EXCEPT-TITLE.               EL319
01325      MOVE +1                     TO W-LINES-USED.                 EL319
01326      MOVE W-EXCEPTION-LINE       TO PRT.                          EL319
01327      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01328                                                                   EL319
01329      MOVE W-CLMS-RECORDS-BAD-DATE                                 EL319
01330                                  TO W-EXCEPT-COUNT.               EL319
01331      MOVE ' WITH BAD DATES.'     TO W-EXCEPT-TITLE.               EL319
01332      MOVE +1                     TO W-LINES-USED.                 EL319
01333      MOVE W-EXCEPTION-LINE       TO PRT.                          EL319
01334      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01335                                                                   EL319
01336      MOVE W-CLMS-RECORDS-PRV-DATE                                 EL319
01337                                  TO W-EXCEPT-COUNT.               EL319
01338      MOVE ' INCURRED BEFORE EFFECTIVE DATE.'                      EL319
01339                                  TO W-EXCEPT-TITLE.               EL319
01340      MOVE +1                     TO W-LINES-USED.                 EL319
01341      MOVE W-EXCEPTION-LINE       TO PRT.                          EL319
01342      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01343                                                                   EL319
01344  3280-EXIT.                                                       EL319
01345      EXIT.                                                        EL319
01346                                  EJECT                            EL319
01347  3300-GET-ACCOUNT-NAME.                                           EL319
01348                                                                   EL319
01349      MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD.                EL319
01350      MOVE SW-CARRIER             TO AM-CARRIER.                   EL319
01351      MOVE SW-GROUPING            TO AM-GROUPING.                  EL319
01352      MOVE SW-STATE               TO AM-STATE.                     EL319
01353      MOVE SW-ACCOUNT             TO AM-ACCOUNT.                   EL319
01354      MOVE LOW-VALUES             TO AM-EXPIRATION-DT.             EL319
01355                                                                   EL319
01356      START ERACCT                                                 EL319
01357          KEY IS GREATER THAN AM-CONTROL-PRIMARY.                  EL319
01358                                                                   EL319
01359      IF  W-AM-FILE-STATUS NOT = ZERO                              EL319
01360          MOVE 'ERROR OCCURED STARTING ACCOUNT MASTER - ERACCT'    EL319
01361                                  TO  WS-ABEND-MESSAGE             EL319
01362          MOVE W-AM-FILE-STATUS   TO  WS-ABEND-FILE-STATUS         EL319
01363          PERFORM ABEND-PGM.                                       EL319
01364                                                                   EL319
01365      MOVE SPACE                  TO W-ACCOUNT-FOUND-SW.           EL319
01366                                                                   EL319
01367  3300-READ-ERACCT-NEXT.                                           EL319
01368      READ ERACCT NEXT.                                            EL319
01369                                                                   EL319
01370      IF  W-AM-FILE-STATUS NOT = ZERO                              EL319
01371          MOVE 'ERROR OCCURED READ ACCOUNT MASTER - ERACCT'        EL319
01372                                  TO  WS-ABEND-MESSAGE             EL319
01373          MOVE W-AM-FILE-STATUS   TO  WS-ABEND-FILE-STATUS         EL319
01374          PERFORM ABEND-PGM.                                       EL319
01375                                                                   EL319
01376      IF AM-ACCOUNT NOT = W-ACCOUNT                                EL319
01377          IF W-ACCOUNT-FOUND                                       EL319
01378              MOVE CL-CERT-EFF-DT     TO DC-BIN-DATE-1             EL319
01379              MOVE ' '                TO DC-OPTION-CODE            EL319
01380              CALL 'ELDATCX' USING DATE-CONVERSION-DATA            EL319
01381              MOVE DC-GREG-DATE-1-MDY TO WS-CERT-EFF-DATE          EL319
01382              DISPLAY '** ACCOUNT DATE RANGE NOT FOUND FOR '       EL319
01383                                                 W-ACCOUNT         EL319
01384                      ' CL-CERT-EFF-DT = ' WS-CERT-EFF-DATE        EL319
01385              GO TO 3300-EXIT                                      EL319
01386            ELSE                                                   EL319
01387              DISPLAY '** ACCOUNT NOT FOUND FOR ' W-ACCOUNT        EL319
01388                      ' CL-CERT-EFF-DT = ' WS-CERT-EFF-DATE        EL319
01389              MOVE 'NO ACCOUNT RECORD FOUND - ERACCT'              EL319
01390                                      TO WS-ABEND-MESSAGE          EL319
01391              MOVE W-AM-FILE-STATUS   TO WS-ABEND-FILE-STATUS      EL319
01392              GO TO ABEND-PGM.                                     EL319
01393                                                                   EL319
01394      MOVE 'Y'                    TO W-ACCOUNT-FOUND-SW.           EL319
01395                                                                   EL319
01396      IF CL-CERT-EFF-DT  LESS   AM-EFFECTIVE-DT OR                 EL319
01397                       GREATER  AM-EXPIRATION-DT                   EL319
01398          GO TO 3300-READ-ERACCT-NEXT.                             EL319
01399                                                                   EL319
01400      MOVE AM-NAME                TO W-ACCOUNT-NAME.               EL319
01401                                                                   EL319
01402  3300-EXIT.                                                       EL319
01403      EXIT.                                                        EL319
01404                                  EJECT                            EL319
01405  3350-GET-PRODUCER-NAME.                                          EL319
01406                                                                   EL319
01407      MOVE DTE-CLASIC-COMPANY-CD  TO PD-COMPANY-CD.                EL319
01408      MOVE SW-CARRIER             TO PD-CARRIER.                   EL319
01409      MOVE SW-GROUPING            TO PD-GROUPING.                  EL319
01410      MOVE SW-STATE               TO PD-STATE.                     EL319
01411      MOVE SW-ACCOUNT             TO PD-PRODUCER.                  EL319
01412      MOVE LOW-VALUES             TO PD-EXPIRE-DATE.               EL319
01413                                                                   EL319
01414      START MPPROD                                                 EL319
01415          KEY IS GREATER THAN PD-CONTROL-PRIMARY.                  EL319
01416                                                                   EL319
01417      IF  W-PD-FILE-STATUS NOT = ZERO                              EL319
01418          MOVE 'ERROR OCCURED STARTING PRODUCER MASTER - MPPROD'   EL319
01419                                  TO  WS-ABEND-MESSAGE             EL319
01420          MOVE W-PD-FILE-STATUS   TO  WS-ABEND-FILE-STATUS         EL319
01421          PERFORM ABEND-PGM.                                       EL319
01422                                                                   EL319
01423      MOVE SPACE                  TO W-ACCOUNT-FOUND-SW.           EL319
01424                                                                   EL319
01425  3350-READ-MPPROD-NEXT.                                           EL319
01426      READ MPPROD NEXT.                                            EL319
01427                                                                   EL319
01428      IF  W-PD-FILE-STATUS NOT = ZERO                              EL319
01429          MOVE 'ERROR OCCURED READ PRODUCER MASTER - MPPROD'       EL319
01430                                  TO  WS-ABEND-MESSAGE             EL319
01431          MOVE W-PD-FILE-STATUS   TO  WS-ABEND-FILE-STATUS         EL319
01432          PERFORM ABEND-PGM.                                       EL319
01433                                                                   EL319
01434      IF PD-PRODUCER NOT = W-ACCOUNT                               EL319
01435          IF W-ACCOUNT-FOUND                                       EL319
01436              MOVE CL-CERT-EFF-DT     TO DC-BIN-DATE-1             EL319
01437              MOVE ' '                TO DC-OPTION-CODE            EL319
01438              CALL 'ELDATCX' USING DATE-CONVERSION-DATA            EL319
01439              MOVE DC-GREG-DATE-1-MDY TO WS-CERT-EFF-DATE          EL319
01440              DISPLAY '** PRODUCER DATE RANGE NOT FOUND FOR '      EL319
01441                                                 W-ACCOUNT         EL319
01442                      ' CL-CERT-EFF-DT = ' WS-CERT-EFF-DATE        EL319
01443              GO TO 3300-EXIT                                      EL319
01444            ELSE                                                   EL319
01445              DISPLAY '** PRODUCER NOT FOUND FOR ' W-ACCOUNT       EL319
01446                      ' CL-CERT-EFF-DT = ' WS-CERT-EFF-DATE        EL319
01447              MOVE 'NO PRODUCER RECORD FOUND - ERACCT'             EL319
01448                                      TO WS-ABEND-MESSAGE          EL319
01449              MOVE W-PD-FILE-STATUS   TO WS-ABEND-FILE-STATUS      EL319
01450              GO TO ABEND-PGM.                                     EL319
01451                                                                   EL319
01452      MOVE 'Y'                    TO W-ACCOUNT-FOUND-SW.           EL319
01453                                                                   EL319
01454      IF CL-CERT-EFF-DT  LESS   PD-EFFECT-DATE  OR                 EL319
01455                       GREATER  PD-EXPIRE-DATE                     EL319
01456          GO TO 3350-READ-MPPROD-NEXT.                             EL319
01457                                                                   EL319
01458      MOVE PD-NAME                TO W-ACCOUNT-NAME.               EL319
01459                                                                   EL319
01460  3350-EXIT.                                                       EL319
01461      EXIT.                                                        EL319
01462                                  EJECT                            EL319
01463  3400-PRINT-ACCOUNT-LINE.                                         EL319
01464                                                                   EL319
01465      MOVE W-ACCOUNT              TO W-CL-ACCOUNT.                 EL319
01466      MOVE W-ACCOUNT-NAME         TO W-CL-ACCOUNT-NAME.            EL319
01467      MOVE +3                     TO W-LINES-USED.                 EL319
01468      MOVE W-CONTROL-LINE         TO PRT.                          EL319
01469      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01470      MOVE +0                     TO W-LINES-USED.                 EL319
01471      MOVE W-CONTROL-UNDERLINE    TO PRT.                          EL319
01472      PERFORM 3800-PRINT-CONTROL THRU 3800-EXIT.                   EL319
01473      MOVE +1                     TO W-LINES-USED.                 EL319
01474                                                                   EL319
01475  3400-EXIT.                                                       EL319
01476      EXIT.                                                        EL319
01477                                  EJECT                            EL319
01478  3800-PRINT-CONTROL.                                              EL319
01479                                                                   EL319
01480      ADD W-LINES-USED            TO W-LINE-COUNT.                 EL319
01481                                                                   EL319
01482      IF  W-LINE-COUNT GREATER THAN +56                            EL319
01483          MOVE PRT                TO W-PRINT-LINE-HOLD             EL319
01484          PERFORM 3820-HEADER THRU 3820-EXIT                       EL319
01485          MOVE W-PRINT-LINE-HOLD  TO PRT.                          EL319
01486                                                                   EL319
01487      IF  W-LINES-USED EQUAL +0                                    EL319
01488          MOVE ' '                TO W-X                           EL319
01489      ELSE                                                         EL319
01490          IF  W-LINES-USED EQUAL +1                                EL319
01491              MOVE SPACES         TO W-X                           EL319
01492          ELSE                                                     EL319
01493              IF  W-LINES-USED EQUAL +2                            EL319
01494                  MOVE '0'        TO W-X                           EL319
01495              ELSE                                                 EL319
01496                  MOVE '-'        TO W-X.                          EL319
01497                                                                   EL319
01498      PERFORM 3840-PRT-RTN THRU 3840-EXIT.                         EL319
01499                                                                   EL319
01500  3800-EXIT.                                                       EL319
01501      EXIT.                                                        EL319
01502                                  EJECT                            EL319
01503  3820-HEADER.                                                     EL319
01504                                                                   EL319
01505      ADD +1                      TO W-PAGE-COUNT.                 EL319
01506      MOVE W-PAGE-COUNT           TO W-HDR3-PAGE.                  EL319
01507                                                                   EL319
01508      MOVE W-HDR-1                TO PRT.                          EL319
01509      MOVE '1'                    TO W-X                           EL319
01510      PERFORM 3840-PRT-RTN THRU 3840-EXIT.                         EL319
01511                                                                   EL319
01512      MOVE W-HDR-2                TO PRT.                          EL319
01513      MOVE ' '                    TO W-X                           EL319
01514      PERFORM 3840-PRT-RTN THRU 3840-EXIT.                         EL319
01515                                                                   EL319
01516      MOVE W-HDR-3                TO PRT.                          EL319
01517      MOVE ' '                    TO W-X                           EL319
01518      PERFORM 3840-PRT-RTN THRU 3840-EXIT.                         EL319
01519                                                                   EL319
01520      MOVE W-HDR-4                TO PRT.                          EL319
01521      MOVE '0'                    TO W-X                           EL319
01522      PERFORM 3840-PRT-RTN THRU 3840-EXIT.                         EL319
01523                                                                   EL319
01524      MOVE W-HDR-5                TO PRT.                          EL319
01525      MOVE '0'                    TO W-X.                          EL319
01526      PERFORM 3840-PRT-RTN THRU 3840-EXIT.                         EL319
01527                                                                   EL319
01528      MOVE W-HDR-6                TO PRT.                          EL319
01529      MOVE SPACES                 TO W-X.                          EL319
01530      PERFORM 3840-PRT-RTN THRU 3840-EXIT.                         EL319
01531                                                                   EL319
01532      MOVE +10                    TO W-LINE-COUNT.                 EL319
01533      MOVE +2                     TO W-LINES-USED.                 EL319
01534                                                                   EL319
01535  3820-EXIT.                                                       EL319
01536      EXIT.                                                        EL319
01537                                  EJECT                            EL319
01538  3840-PRT-RTN.                                                    EL319
01539                                                                   EL319
01540      COPY ELCPRT2 REPLACING == X == BY == W-X ==.                 EL319
01541                                                                   EL319
01542  3840-EXIT.                                                       EL319
01543      EXIT.                                                        EL319
01544                                  EJECT                            EL319
01545  3900-RETURN-SORTED-DATA.                                         EL319
01546                                                                   EL319
01547      RETURN SORT-WORK                                             EL319
01548          AT END                                                   EL319
01549              MOVE 'Y'            TO W-END-OF-SORT-DATA-SW.        EL319
01550                                                                   EL319
01551  3900-EXIT.                                                       EL319
01552      EXIT.                                                        EL319
01553                                                                   EL319
01554  3999-EXIT.                                                       EL319
01555      EXIT.                                                        EL319
01556                                  EJECT                            EL319
01557  8000-MISCELLANEOUS SECTION.                                      EL319
01558                                                                   EL319
01559  8000-DATE-CONVERT-ROUTINE.                                       EL319
01560                                                                   EL319
01561      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL319
01562                                                                   EL319
01563  8099-DATE-CONVERT-X.                                             EL319
01564      EXIT.                                                        EL319
01565                                  EJECT                            EL319
01566  ABEND-PGM SECTION.                                               EL319
01567      COPY ELCABEND SUPPRESS.                                      EL319
01568                                                                   EL319
01569  9999-END-OF-JOB.                                                 EL319
01570                                                                   EL319
01571      CLOSE ELTRLR                                                 EL319
01572            ERACCT                                                 EL319
01573            MPPROD                                                 EL319
01574            PRINTER.                                               EL319
01575                                                                   EL319
01576      IF  W-AM-FILE-STATUS NOT = '00'                              EL319
01577          MOVE '**** ERACCT CLOSE ERROR ****'                      EL319
01578                                  TO WS-ABEND-MESSAGE              EL319
01579          MOVE '9601'             TO WS-RETURN-CODE                EL319
01580          MOVE W-AM-FILE-STATUS   TO WS-ABEND-FILE-STATUS          EL319
01581          GO TO ABEND-PGM.                                         EL319
01582                                                                   EL319
01583      IF  W-PD-FILE-STATUS IS NOT EQUAL TO '00'                    EL319
01584          MOVE '**** MPPROD CLOSE ERROR ****'                      EL319
01585                                  TO WS-ABEND-MESSAGE              EL319
01586          MOVE '9606'             TO WS-RETURN-CODE                EL319
01587          MOVE W-PD-FILE-STATUS   TO WS-ABEND-FILE-STATUS          EL319
01588          GO TO ABEND-PGM.                                         EL319
01589                                                                   EL319
01590      IF  W-TRLR-FILE-STATUS NOT = '00'                            EL319
01591          MOVE '**** TRAILER MASTER CLOSE ERROR - ELTRLR ****'     EL319
01592                                  TO WS-ABEND-MESSAGE              EL319
01593          MOVE '1205'             TO WS-RETURN-CODE                EL319
01594          MOVE W-TRLR-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL319
01595          GO TO ABEND-PGM.                                         EL319
01596                                                                   EL319
01597      COPY ELCPRTC.                                                EL319
01598                                                                   EL319
01599      DISPLAY ' '.                                                 EL319
01600      DISPLAY '****** THE END OF MESSAGES CREATED BY '             EL319
01601          'EL319 ******'                                           EL319
01602      DISPLAY ' '.                                                 EL319
01603                                                                   EL319
01604      GOBACK.                                                      EL319
