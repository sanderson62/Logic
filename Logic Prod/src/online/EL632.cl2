00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL632
00003  PROGRAM-ID.                 EL632 .                                 LV008
00004 *              PROGRAM CONVERTED BY                                  CL**8
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**8
00006 *              CONVERSION DATE 02/10/96 11:56:51.                    CL**8
00007 *                            VMOD=2.008                              CL**8
00008 *                                                                 EL632
00009 *AUTHOR.        LOGIC,INC.                                           CL**8
00010 *               DALLAS, TEXAS.                                       CL**8
00011                                                                   EL632
00012 *DATE-COMPILED.                                                      CL**8
00013                                                                   EL632
00014 *SECURITY.   *****************************************************   CL**8
00015 *            *                                                   *   CL**8
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**8
00017 *            *                                                   *   CL**8
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**8
00019 *                                                                *   CL**8
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**8
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**8
00022 *            *                                                   *   CL**8
00023 *            *****************************************************   CL**8
00024                                                                   EL632
00025 *REMARKS.                                                            CL**2
00026 *        TRANSACTION - EXB4 - CLAIMS AND RESERVES                    CL**2
00027 *                             (CLAIM PAYMENTS).                      CL**2
00028                                                                   EL632
00029  ENVIRONMENT DIVISION.                                            EL632
00030  DATA DIVISION.                                                   EL632
00031  EJECT                                                            EL632
00032  WORKING-STORAGE SECTION.                                         EL632
00033  77  FILLER  PIC X(32)  VALUE '********************************'. EL632
00034  77  FILLER  PIC X(32)  VALUE '*    EL632 WORKING STORAGE     *'. EL632
00035  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.008 *********'.    CL**8
00036                                                                   EL632
00037                              COPY ELCSCTM.                           CL**7
00038                              COPY ELCSCRTY.                          CL**7
00039                                                                   EL632
00040     EJECT                                                         EL632
00041                                                                   EL632
00042  01  STANDARD-AREAS.                                              EL632
00043      12  SC-ITEM             PIC S9(4) COMP VALUE +1.             EL632
00044      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.         EL632
00045      12  EL632A              PIC  X(8)       VALUE 'EL632A'.      EL632
00046      12  EL632B              PIC  X(8)       VALUE 'EL632B'.      EL632
00047      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL632S'.      EL632
00048      12  SCREEN-NUMBER       PIC  X(4)       VALUE '632B'.        EL632
00049      12  TRANS-ID            PIC  X(4)       VALUE 'EXB4'.        EL632
00050      12  THIS-PGM            PIC  X(8)       VALUE 'EL632 '.      EL632
00051      12  PGM-NAME            PIC  X(8).                           EL632
00052      12  TIME-IN             PIC S9(7).                           EL632
00053      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL632
00054          16  FILLER          PIC  X.                              EL632
00055          16  TIME-OUT        PIC  99V99.                          EL632
00056          16  FILLER          PIC  XX.                             EL632
00057      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.       EL632
00058      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.       EL632
00059      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.       EL632
00060      12  XCTL-6321           PIC  X(8)       VALUE 'EL6321'.      EL632
00061      12  XCTL-127            PIC  X(8)       VALUE 'EL127'.       EL632
00062      12  XCTL-1273           PIC  X(8)       VALUE 'EL1273'.      EL632
00063      12  LINK-001            PIC  X(8)       VALUE 'EL001'.       EL632
00064      12  LINK-004            PIC  X(8)       VALUE 'EL004'.       EL632
00065      12  LINK-053            PIC  X(8)       VALUE 'EL053'.       EL632
00066      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.     EL632
00067      12  ELCNTL-FILE-ID      PIC  X(8)       VALUE 'ELCNTL'.      EL632
00068      12  ERACCT-FILE-ID      PIC  X(8)       VALUE 'ERACCT2'.     EL632
00069      12  ERPNDC-FILE-ID      PIC  X(8)       VALUE 'ERPNDC'.      EL632
00070      12  RETURNED-FROM       PIC  X(8)       VALUE SPACES.        EL632
00071      12  PNDC-EDIT-PASS-AREA-LEN                                  EL632
00072                              PIC S9(4)       VALUE +553 COMP.        CL**6
00073      12  SUB                 PIC  9(3)       COMP-3 VALUE ZEROS.     CL**8
00074      12  QID.                                                     EL632
00075          16  QID-TERM        PIC  X(4).                           EL632
00076          16  FILLER          PIC  X(4)       VALUE '632A'.        EL632
00077      12  WS-ERR-CODE.                                             EL632
00078          16  FILLER          PIC  99         VALUE ZEROS.            CL**8
00079          16  WS-ERROR-SUB    PIC  99         VALUE ZEROS.            CL**8
00080      12  WS-ERROR-CODES.                                          EL632
00081          16  ER-0000         PIC  X(4)       VALUE '0000'.        EL632
00082          16  ER-0004         PIC  X(4)       VALUE '0004'.        EL632
00083          16  ER-0008         PIC  X(4)       VALUE '0008'.        EL632
00084          16  ER-0023         PIC  X(4)       VALUE '0023'.        EL632
00085          16  ER-0029         PIC  X(4)       VALUE '0029'.        EL632
00086          16  ER-0070         PIC  X(4)       VALUE '0070'.        EL632
00087          16  ER-0194         PIC  X(4)       VALUE '0194'.        EL632
00088          16  ER-0195         PIC  X(4)       VALUE '0195'.        EL632
00089          16  ER-0196         PIC  X(4)       VALUE '0196'.        EL632
00090          16  ER-0197         PIC  X(4)       VALUE '0197'.        EL632
00091          16  ER-0203         PIC  X(4)       VALUE '0203'.        EL632
00092          16  ER-0209         PIC  X(4)       VALUE '0209'.        EL632
00093          16  ER-0587         PIC  X(4)       VALUE '0587'.        EL632
00094          16  ER-2167         PIC  X(4)       VALUE '2167'.        EL632
00095          16  ER-2208         PIC  X(4)       VALUE '2208'.        EL632
00096          16  ER-2209         PIC  X(4)       VALUE '2209'.        EL632
00097          16  ER-2210         PIC  X(4)       VALUE '2210'.        EL632
00098          16  ER-2220         PIC  X(4)       VALUE '2220'.        EL632
00099          16  ER-2223         PIC  X(4)       VALUE '2223'.        EL632
00100          16  ER-2226         PIC  X(4)       VALUE '2226'.        EL632
00101          16  ER-2237         PIC  X(4)       VALUE '2237'.        EL632
00102          16  ER-2238         PIC  X(4)       VALUE '2238'.        EL632
00103          16  ER-2243         PIC  X(4)       VALUE '2243'.        EL632
00104          16  ER-2370         PIC  X(4)       VALUE '2270'.        EL632
00105          16  ER-2371         PIC  X(4)       VALUE '2271'.        EL632
00106          16  ER-2445         PIC  X(4)       VALUE '2445'.        EL632
00107          16  ER-2446         PIC  X(4)       VALUE '2446'.        EL632
00108          16  ER-2447         PIC  X(4)       VALUE '2447'.        EL632
00109          16  ER-2448         PIC  X(4)       VALUE '2448'.        EL632
00110          16  ER-2451         PIC  X(4)       VALUE '2451'.        EL632
00111          16  ER-2452         PIC  X(4)       VALUE '2452'.        EL632
00112          16  ER-2454         PIC  X(4)       VALUE '2454'.        EL632
00113          16  ER-2455         PIC  X(4)       VALUE '2455'.        EL632
00114          16  ER-2457         PIC  X(4)       VALUE '2457'.        EL632
00115          16  ER-2458         PIC  X(4)       VALUE '2458'.        EL632
00116          16  ER-2460         PIC  X(4)       VALUE '2460'.        EL632
00117          16  ER-2461         PIC  X(4)       VALUE '2461'.        EL632
00118          16  ER-2462         PIC  X(4)       VALUE '2462'.        EL632
00119          16  ER-2463         PIC  X(4)       VALUE '2463'.        EL632
00120          16  ER-2464         PIC  X(4)       VALUE '2464'.        EL632
00121          16  ER-2465         PIC  X(4)       VALUE '2465'.        EL632
00122          16  ER-2467         PIC  X(4)       VALUE '2467'.        EL632
00123          16  ER-2468         PIC  X(4)       VALUE '2468'.        EL632
00124          16  ER-2469         PIC  X(4)       VALUE '2469'.        EL632
00125          16  ER-2470         PIC  X(4)       VALUE '2470'.        EL632
00126          16  ER-2563         PIC  X(4)       VALUE '2563'.        EL632
00127          16  ER-2574         PIC  X(4)       VALUE '2574'.        EL632
00128          16  ER-2600         PIC  X(4)       VALUE '2600'.        EL632
00129          16  ER-2761         PIC  X(4)       VALUE '2761'.        EL632
00130          16  ER-2800         PIC  X(4)       VALUE '2800'.        EL632
00131          16  ER-2815         PIC  X(4)       VALUE '2815'.           CL**4
00132          16  ER-2853         PIC  X(4)       VALUE '2853'.        EL632
00133  EJECT                                                            EL632
00134  01  WORK-AREAS.                                                  EL632
00135      12  EL632A-HEADING          PIC X(38) VALUE                     CL**3
00136          'PAID DT  PAID  TO    DAYS   AGE  CAUSE'.                   CL**3
00137      12  WS-ACLMTP1.                                              EL632
00138          16  FILLER              PIC  X(13)  VALUE                EL632
00139                  'CLAIM TYPES: '.                                 EL632
00140          16  WS-ACLM-TYP-1       PIC  X(6).                       EL632
00141          16  FILLER              PIC  X(5)   VALUE '(1), '.       EL632
00142          16  WS-ACLM-TYP-3       PIC  X(6).                       EL632
00143          16  FILLER              PIC  X(6)   VALUE '-OB(3)'.      EL632
00144      12  WS-ACLMTP2.                                              EL632
00145          16  WS-ACLM-TYP-2       PIC  X(6).                       EL632
00146          16  FILLER              PIC  X(5)   VALUE '(2), '.       EL632
00147          16  WS-ACLM-TYP-4       PIC  X(6).                       EL632
00148          16  FILLER              PIC  X(6)   VALUE '-OB(4)'.      EL632
00149      12  WS-EL632-EOM-DATE       PIC  XX     VALUE LOW-VALUES.    EL632
00150      12  WS-DATE-TEST.                                            EL632
00151          16  WS-DATE-MO          PIC  99     VALUE ZEROS.         EL632
00152          16  WS-DATE-DA          PIC  99     VALUE ZEROS.         EL632
00153          16  FILLER              PIC  99     VALUE ZEROS.         EL632
00154      12  WS-CURRENT-DT           PIC  X(8)   VALUE SPACES.        EL632
00155      12  WS-CURRENT-BIN-DT       PIC  XX     VALUE SPACES.        EL632
00156      12  WS-CARRIER-SW           PIC  X      VALUE SPACES.        EL632
00157      12  WS-SV-CARRIER           PIC  X      VALUE SPACES.        EL632
00158      12  WS-SV-GROUPING          PIC  X(6)   VALUE SPACES.        EL632
00159      12  WS-SV-STATE             PIC  XX     VALUE SPACES.        EL632
00160      12  WS-MAINT                PIC  X      VALUE SPACE.         EL632
00161      12  WS-CONVERTED-EFFDT      PIC  XX     VALUE SPACES.        EL632
00162      12  WS-CONVERTED-INCUR      PIC  XX     VALUE SPACES.        EL632
00163      12  WS-CONVERTED-REPORT     PIC  XX     VALUE SPACES.        EL632
00164      12  WS-CONVERTED-PAID       PIC  XX     VALUE SPACES.        EL632
00165      12  WS-CONVERTED-PTHRU      PIC  XX     VALUE SPACES.        EL632
00166      12  WS-VALID-CLAIM-TYPES    PIC  X      VALUE SPACE.         EL632
00167          88  VALID-CLAIM-TYPE                VALUE '1' '2' '3'    EL632
00168                                                    '4'.           EL632
00169      12  WS-VALID-PAY-TYPES      PIC  X      VALUE SPACE.         EL632
00170          88  VALID-PAY-TYPE                  VALUE '1' '2' '3'    EL632
00171                                                    '4' '5' '6'    EL632
00172                                                    '9'.           EL632
00173      12  WS-VALID-FORCE-CD       PIC  X      VALUE SPACE.         EL632
00174          88  VALID-FORCE-CD                  VALUE '6' '7' '8'.   EL632
CIDMOD     12  WS-DEEDIT-FIELD         PIC S9(9)V99.                         000
00175      12  WS-DEEDIT-FIELD1        PIC  X(11)  VALUE ZEROS.         EL632
00176      12  WS-DEEDIT-FIELDV0  REDEFINES  WS-DEEDIT-FIELD1              CL**8
00177                                  PIC  S9(11).                        CL**8
00178      12  WS-RESERVE-CONTROLS.                                     EL632
00179          16  WS-MANUAL-SW        PIC  X.                          EL632
00180              88  WS-MANUAL-RESERVES-USED     VALUE '1'.           EL632
00181          16  CF-FUTURE-SW        PIC  X.                          EL632
00182              88  WS-FUTURE-RESERVES-USED     VALUE '1'.           EL632
00183          16  CF-PTC-SW           PIC  X.                          EL632
00184              88  WS-PAY-TO-CURRENT-USED      VALUE '1'.           EL632
00185          16  CF-IBNR-SW          PIC  X.                          EL632
00186              88  WS-IBNR-RESERVES-USED       VALUE '1'.           EL632
00187  EJECT                                                            EL632
00188  01  ACCESS-KEYS.                                                 EL632
00189      12  ELCNTL-KEY.                                              EL632
00190          16  CNTL-COMP-ID        PIC  X(3)   VALUE SPACES.        EL632
00191          16  CNTL-REC-TYPE       PIC  X      VALUE SPACES.        EL632
00192          16  CNTL-ACCESS.                                         EL632
00193              20  CNTL-STATE      PIC  XX     VALUE SPACES.        EL632
00194              20  FILLER          PIC  X      VALUE SPACES.        EL632
00195              20  CNTL-CARRIER    PIC  X      VALUE SPACES.        EL632
00196          16  CNTL-SEQ            PIC S9(4)   VALUE +0   COMP.     EL632
00197      12  ERACCT-KEY.                                              EL632
00198          16  ERACCT-COMP-KEY.                                     EL632
00199              20  ACCT-CO         PIC  X      VALUE SPACES.        EL632
00200              20  ACCT-CARRIER    PIC  X      VALUE SPACES.        EL632
00201              20  ACCT-GROUPING   PIC  X(6)   VALUE SPACES.        EL632
00202              20  ACCT-STATE      PIC  XX     VALUE SPACES.        EL632
00203              20  ACCT-ACCOUNT    PIC  X(10)  VALUE SPACES.        EL632
00204          16  ACCT-EXP-DATE       PIC  X(6)   VALUE SPACES.        EL632
00205      12  ERACCT-SAVE-KEY         PIC  X(20)  VALUE SPACES.        EL632
00206      12  ERPNDC-KEY.                                              EL632
00207          16  PNDC-COMPANY-CD     PIC  X      VALUE SPACE.         EL632
00208          16  PNDC-CARRIER        PIC  X      VALUE SPACE.         EL632
00209          16  PNDC-GROUPING       PIC  X(6)   VALUE SPACES.        EL632
00210          16  PNDC-STATE          PIC  XX     VALUE SPACES.        EL632
00211          16  PNDC-ACCOUNT        PIC  X(10)  VALUE SPACES.        EL632
00212          16  PNDC-CERT-EFF-DT    PIC  XX     VALUE SPACES.        EL632
00213          16  PNDC-CERT-NO.                                        EL632
00214              20  PNDC-CERT-PRIME PIC  X(10)  VALUE SPACES.        EL632
00215              20  PNDC-CERT-SFX   PIC  X      VALUE SPACE.         EL632
00216          16  PNDC-CLAIM-NO       PIC  X(7)   VALUE SPACES.        EL632
00217          16  PNDC-CHECK-NO       PIC  X(7)   VALUE SPACES.        EL632
00218          16  PNDC-RECORD-TYPE    PIC  X      VALUE '1'.           EL632
00219          16  PNDC-RECORD-SEQ     PIC S9(4)   VALUE +0   COMP.     EL632
00220  EJECT                                                            EL632
00221                                      COPY ELCDATE.                   CL**7
00222  EJECT                                                            EL632
00223                                      COPY ELCLOGOF.                  CL**7
00224  EJECT                                                            EL632
00225                                      COPY ELCATTR.                   CL**7
00226  EJECT                                                            EL632
00227                                      COPY ELCEMIB.                   CL**7
00228  EJECT                                                            EL632
00229                                      COPY ELCINTF.                   CL**7
00230      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                 EL632
00231          16  PI-MAP-NAME                 PIC  X(8).               EL632
00232          16  PI-FIRST-TIME-SW            PIC  X.                  EL632
00233              88  PI-FIRST-TIME               VALUE SPACE.         EL632
00234          16  PI-EOF-SW                   PIC  X.                  EL632
00235              88  PI-FILE-EOF                 VALUE 'Y'.           EL632
00236          16  PI-ERPNDC-KEY.                                       EL632
00237              20  PI-PNDC-COMP-CD         PIC  X.                  EL632
00238              20  PI-PNDC-CARRIER         PIC  X.                  EL632
00239              20  PI-PNDC-GROUPING        PIC  X(6).               EL632
00240              20  PI-PNDC-STATE           PIC  XX.                 EL632
00241              20  PI-PNDC-ACCOUNT         PIC  X(10).              EL632
00242              20  PI-PNDC-CERT-EFF-DT     PIC  XX.                 EL632
00243              20  PI-PNDC-CERT-NO.                                 EL632
00244                  24  PI-PNDC-PRIME       PIC  X(10).              EL632
00245                  24  PI-PNDC-SFX         PIC  X.                  EL632
00246              20  PI-PNDC-CLAIM-NO        PIC  X(7).               EL632
00247              20  PI-PNDC-CHECK-NO        PIC  X(7).               EL632
00248              20  PI-PNDC-REC-TYP         PIC  X.                  EL632
00249              20  PI-RECORD-SEQUENCE      PIC S9(4)     COMP.      EL632
00250          16  PI-PREV-ERPNDC-KEY          PIC  X(50).              EL632
00251          16  PI-MAINT                    PIC  X.                  EL632
00252          16  FILLER                      PIC  X(529).                CL**8
00253  EJECT                                                            EL632
00254  01  PNDC-EDIT-PASS-AREA.                                         EL632
00255      12  PNDC-RECORD                     PIC  X(500).             EL632
00256      12  WK-PC-WORK-AREA.                                         EL632
00257          16  WK-PC-CNTL-RECORD-FOUND-SW  PIC  X.                  EL632
00258          16  WK-PC-LAST-CARRIER          PIC  X.                  EL632
00259          16  WK-PC-CERT-ACCESS-CNTL      PIC  X.                  EL632
00260          16  WK-PC-CO-CLAIM-REJECT-SW    PIC  X.                  EL632
00261          16  WK-PC-CLAIM-SYSTEM-SW       PIC  X.                  EL632
00262          16  WK-PC-CO-TOL-CLAIM          PIC S999V99 COMP-3.      EL632
00263          16  WK-PC-RESERVE-CONTROLS      PIC  X(4).               EL632
00264          16  WK-PC-CREDIT-EDIT-CONTROLS  PIC  X(12).              EL632
00265      12  WK-PC-RECORD-ADDRESSES.                                  EL632
00266          16  WK-PC-ACCT-ADDR             PIC S9(8)     COMP.      EL632
00267          16  WK-PC-STATE-ADDR            PIC S9(8)     COMP.      EL632
00268      12  WK-MISC.                                                    CL**6
00269          16  WK-PC-REM-TRM-CALC-OPTION   PIC X.                      CL**6
00270          16  FILLER                      PIC X(20).                  CL**6
00271                                                                      CL**6
00272  EJECT                                                            EL632
00273                              COPY ELCJPFX.                           CL**7
00274                              PIC  X(500).                         EL632
00275  EJECT                                                            EL632
00276                              COPY ELCAID.                            CL**7
00277                                                                   EL632
00278  01  FILLER    REDEFINES DFHAID.                                  EL632
00279      12  FILLER              PIC  X(8).                           EL632
00280      12  PF-VALUES           PIC  X      OCCURS 2 TIMES.          EL632
00281  EJECT                                                            EL632
00282                              COPY EL632S.                            CL**7
00283  EJECT                                                            EL632
00284  LINKAGE SECTION.                                                 EL632
00285  01  DFHCOMMAREA             PIC  X(1024).                        EL632
00286  EJECT                                                            EL632
00287                              COPY ELCCNTL.                           CL**7
00288  EJECT                                                            EL632
00289                              COPY ERCACCT.                           CL**7
00290  EJECT                                                            EL632
00291                              COPY ERCPNDC.                           CL**7
00292  EJECT                                                            EL632
00293  PROCEDURE DIVISION.                                              EL632
00294      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL632
00295                                                                   EL632
00296      IF EIBCALEN  = ZERO                                          EL632
00297          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL632
00298                                                                   EL632
00299      MOVE 3                      TO  EMI-NUMBER-OF-LINES.         EL632
00300      MOVE 2                      TO  EMI-SWITCH2.                 EL632
00301      MOVE EIBTRMID               TO  QID-TERM.                    EL632
00302      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL632
00303      MOVE '5'                    TO  DC-OPTION-CODE.              EL632
00304                                                                   EL632
00305      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                  EL632
00306                                                                   EL632
00307      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.           EL632
00308      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.               EL632
00309      MOVE PI-CALLING-PROGRAM     TO  RETURNED-FROM.               EL632
00310                                                                   EL632
00311      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL632
00312          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL632
00313              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL632
00314              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL632
00315              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL632
00316              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL632
00317              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL632
00318              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL632
00319              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL632
00320              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM    EL632
00321          ELSE                                                     EL632
00322              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL632
00323              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL632
00324              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL632
00325              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL632
00326              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL632
00327              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL632
00328              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL632
00329              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.   EL632
00330                                                                   EL632
00331      MOVE LOW-VALUES             TO  EL632AI.                     EL632
00332                                                                   EL632
00333      IF EIBTRNID NOT = TRANS-ID                                   EL632
00334          IF RETURNED-FROM = XCTL-6321                             EL632
00335              GO TO 5000-BROWSE-CLAIMS-FILE                        EL632
00336          ELSE                                                     EL632
00337              IF RETURNED-FROM = XCTL-127 OR XCTL-1273             EL632
00338                  PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0690-EXIT EL632
00339                  IF PI-CONTROL-IN-PROGRESS = SPACES               EL632
00340                      MOVE EL632A  TO  PI-MAP-NAME                 EL632
00341                      GO TO 8100-SEND-INITIAL-MAP                  EL632
00342                  ELSE                                             EL632
00343                      GO TO 5000-BROWSE-CLAIMS-FILE                EL632
00344              ELSE                                                 EL632
00345                  MOVE EL632A     TO  PI-MAP-NAME                  EL632
00346                  GO TO 8100-SEND-INITIAL-MAP.                     EL632
00347                                                                   EL632
00348      MOVE PI-COMPANY-CD          TO  PNDC-COMPANY-CD              EL632
00349                                      PI-PNDC-COMP-CD.             EL632
00350                                                                   EL632
00351      EXEC CICS HANDLE CONDITION                                   EL632
00352          PGMIDERR  (9600-PGMID-ERROR)                             EL632
00353          ERROR     (9990-ABEND)                                   EL632
00354      END-EXEC.                                                    EL632
00355                                                                   EL632
00356      IF EIBAID = DFHCLEAR                                         EL632
00357          GO TO 9400-CLEAR.                                        EL632
00358                                                                   EL632
00359      IF PI-PROCESSOR-ID = 'LGXX'                                  EL632
00360          GO TO 0200-RECEIVE.                                      EL632
00361                                                                   EL632
00362      EXEC CICS READQ TS                                           EL632
00363          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       EL632
00364          INTO   (SECURITY-CONTROL)                                EL632
00365          LENGTH (SC-COMM-LENGTH)                                  EL632
00366          ITEM   (SC-ITEM)                                         EL632
00367      END-EXEC.                                                    EL632
00368                                                                   EL632
00369      MOVE SC-CREDIT-DISPLAY (14)  TO PI-DISPLAY-CAP.              EL632
00370      MOVE SC-CREDIT-UPDATE  (14)  TO PI-MODIFY-CAP.               EL632
00371                                                                   EL632
00372      IF NOT DISPLAY-CAP                                           EL632
00373          MOVE 'READ'          TO SM-READ                          EL632
00374          PERFORM 9995-SECURITY-VIOLATION                          EL632
00375          MOVE ER-0070         TO  EMI-ERROR                       EL632
00376          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL632
00377          GO TO 8100-SEND-INITIAL-MAP.                             EL632
00378                                                                   EL632
00379  EJECT                                                            EL632
00380  0200-RECEIVE.                                                    EL632
00381      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL632
00382          MOVE ER-0008            TO  EMI-ERROR                    EL632
00383          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL632
00384          IF PI-MAP-NAME = EL632A                                  EL632
00385              MOVE -1             TO  APFNTERL                     EL632
00386              GO TO 8200-SEND-DATAONLY                             EL632
00387          ELSE                                                     EL632
00388              MOVE -1             TO  BPFNTERL                     EL632
00389              GO TO 8200-SEND-DATAONLY.                            EL632
00390                                                                   EL632
00391      EXEC CICS RECEIVE                                            EL632
00392          MAP     (PI-MAP-NAME)                                    EL632
00393          MAPSET  (MAPSET-NAME)                                    EL632
00394          INTO    (EL632AI)                                        EL632
00395      END-EXEC.                                                    EL632
00396                                                                   EL632
00397      IF PI-MAP-NAME = EL632A                                      EL632
00398          IF APFNTERL IS GREATER ZERO                              EL632
00399              IF EIBAID NOT = DFHENTER                             EL632
00400                  MOVE ER-0004    TO  EMI-ERROR                    EL632
00401                  MOVE AL-UNBOF   TO  APFNTERA                     EL632
00402                  MOVE -1         TO  APFNTERL                     EL632
00403                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL632
00404                  GO TO 8200-SEND-DATAONLY                         EL632
00405              ELSE                                                 EL632
00406                  IF APFNTERI NUMERIC   AND                        EL632
00407                     APFNTERI GREATER 0 AND                        EL632
00408                     APFNTERI LESS 25                              EL632
00409                      MOVE PF-VALUES (APFNTERI)  TO  EIBAID        EL632
00410                  ELSE                                             EL632
00411                      MOVE ER-0029   TO  EMI-ERROR                 EL632
00412                      MOVE AL-UNBOF  TO  APFNTERA                  EL632
00413                      MOVE -1        TO  APFNTERL                  EL632
00414                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT   EL632
00415                      GO TO 8200-SEND-DATAONLY                     EL632
00416          ELSE                                                     EL632
00417              NEXT SENTENCE                                        EL632
00418      ELSE                                                         EL632
00419          IF PI-MAP-NAME = EL632B                                  EL632
00420              IF BPFNTERL IS GREATER ZERO                          EL632
00421                  IF EIBAID NOT = DFHENTER                         EL632
00422                      MOVE ER-0004   TO  EMI-ERROR                 EL632
00423                      MOVE AL-UNBOF  TO  BPFNTERA                  EL632
00424                      MOVE -1        TO  BPFNTERL                  EL632
00425                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT   EL632
00426                      GO TO 8200-SEND-DATAONLY                     EL632
00427                  ELSE                                             EL632
00428                      IF BPFNTERI IS NUMERIC                       EL632
00429                        AND  BPFNTERI IS GREATER 0                 EL632
00430                        AND  BPFNTERI IS LESS 25                   EL632
00431                          MOVE PF-VALUES (BPFNTERI)  TO  EIBAID    EL632
00432                      ELSE                                         EL632
00433                          MOVE ER-0029   TO  EMI-ERROR             EL632
00434                          MOVE AL-UNBOF  TO  BPFNTERA              EL632
00435                          MOVE -1        TO  BPFNTERL              EL632
00436                          PERFORM 9900-ERROR-FORMAT                EL632
00437                              THRU  9900-EXIT                      EL632
00438                          GO TO 8200-SEND-DATAONLY.                EL632
00439                                                                   EL632
00440  0300-CHECK-PFKEYS.                                               EL632
00441      IF EIBAID = DFHPF23                                          EL632
00442          GO TO 8810-PF23.                                         EL632
00443                                                                   EL632
00444      IF EIBAID = DFHPF24                                          EL632
00445          GO TO 9200-RETURN-MAIN-MENU.                             EL632
00446                                                                   EL632
00447      IF EIBAID = DFHPF12                                          EL632
00448          GO TO 9500-PF12.                                         EL632
00449                                                                   EL632
00450      IF EIBAID = DFHPF1 OR DFHPF2 OR DFHPF9                       EL632
00451          MOVE SPACE              TO  PI-MAINT                     EL632
00452          GO TO 5000-BROWSE-CLAIMS-FILE.                           EL632
00453                                                                   EL632
00454      IF EIBAID = DFHPF3                                           EL632
00455          MOVE LOW-VALUES         TO  EL632AO                      EL632
00456          MOVE EL632A             TO  PI-MAP-NAME                  EL632
00457          GO TO 8100-SEND-INITIAL-MAP.                             EL632
00458                                                                   EL632
00459      IF EIBAID = DFHPF4                                           EL632
00460          MOVE LOW-VALUES         TO  EL632BO                      EL632
00461          MOVE EL632B             TO  PI-MAP-NAME                  EL632
00462          GO TO 8110-SEND-INITIAL-MAPB.                            EL632
00463                                                                   EL632
00464      IF EIBAID = DFHPF6                                           EL632
00465        AND PI-MAP-NAME = EL632A                                   EL632
00466          PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT        EL632
00467          MOVE XCTL-1273          TO  PGM-NAME                     EL632
00468          GO TO 9300-XCTL.                                         EL632
00469                                                                   EL632
00470      IF EIBAID = DFHPF7                                           EL632
00471        AND PI-MAP-NAME = EL632A                                   EL632
00472          PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT        EL632
00473          MOVE XCTL-127           TO  PGM-NAME                     EL632
00474          GO TO 9300-XCTL.                                         EL632
00475                                                                   EL632
00476      IF EIBAID = DFHPF10                                          EL632
00477          MOVE XCTL-6321          TO  PGM-NAME                     EL632
00478          GO TO 9300-XCTL.                                         EL632
00479                                                                   EL632
00480      IF EIBAID = DFHENTER  OR  DFHPF11                            EL632
00481          GO TO 1000-EDIT-DATA.                                    EL632
00482                                                                   EL632
00483      MOVE ER-0029                TO  EMI-ERROR.                   EL632
00484                                                                   EL632
00485  0320-INPUT-ERROR.                                                EL632
00486      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
00487                                                                   EL632
00488      IF PI-MAP-NAME = EL632A                                      EL632
00489          MOVE -1                 TO  APFNTERL                     EL632
00490      ELSE                                                         EL632
00491          MOVE -1                 TO  BPFNTERL.                    EL632
00492                                                                   EL632
00493      GO TO 8200-SEND-DATAONLY.                                    EL632
00494  EJECT                                                            EL632
00495  0500-CREATE-TEMP-STORAGE.                                        EL632
00496      EXEC CICS WRITEQ TS                                          EL632
00497          QUEUE   (QID)                                            EL632
00498          FROM    (PROGRAM-INTERFACE-BLOCK)                        EL632
00499          LENGTH  (PI-COMM-LENGTH)                                 EL632
00500      END-EXEC.                                                    EL632
00501                                                                   EL632
00502  0590-EXIT.                                                       EL632
00503       EXIT.                                                       EL632
00504                                                                   EL632
00505  0600-RECOVER-TEMP-STORAGE.                                       EL632
00506      EXEC CICS READQ TS                                           EL632
00507          QUEUE   (QID)                                            EL632
00508          INTO    (PROGRAM-INTERFACE-BLOCK)                        EL632
00509          LENGTH  (PI-COMM-LENGTH)                                 EL632
00510      END-EXEC.                                                    EL632
00511                                                                   EL632
00512      PERFORM 0800-DELETE-TS  THRU  0890-EXIT.                     EL632
00513                                                                   EL632
00514  0690-EXIT.                                                       EL632
00515       EXIT.                                                       EL632
00516                                                                   EL632
00517  0800-DELETE-TS.                                                  EL632
00518      EXEC CICS HANDLE CONDITION                                   EL632
00519          QIDERR  (0890-EXIT)                                      EL632
00520      END-EXEC.                                                    EL632
00521                                                                   EL632
00522      EXEC CICS DELETEQ TS                                         EL632
00523          QUEUE  (QID)                                             EL632
00524      END-EXEC.                                                    EL632
00525                                                                   EL632
00526  0890-EXIT.                                                       EL632
00527       EXIT.                                                       EL632
00528  EJECT                                                            EL632
00529  1000-EDIT-DATA.                                                  EL632
00530      IF MODIFY-CAP                                                EL632
00531          NEXT SENTENCE                                            EL632
00532        ELSE                                                       EL632
00533      IF AMAINTI NOT = 'S'                                         EL632
00534          MOVE 'UPDATE'       TO SM-READ                           EL632
00535          PERFORM 9995-SECURITY-VIOLATION                          EL632
00536          MOVE ER-0070        TO EMI-ERROR                         EL632
00537          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL632
00538          GO TO 8100-SEND-INITIAL-MAP.                             EL632
00539                                                                   EL632
00540      IF PI-MAP-NAME = EL632B                                      EL632
00541          GO TO 1100-EDIT-RESERVE-DATA.                            EL632
00542                                                                   EL632
00543      MOVE PI-ERPNDC-KEY          TO  PI-PREV-ERPNDC-KEY.          EL632
00544                                                                   EL632
00545      IF PI-HAS-CLAS-IC-CLAIM                                      EL632
00546        AND  AMAINTI NOT = 'S'                                     EL632
00547          MOVE ER-2243            TO  EMI-ERROR                    EL632
00548          MOVE -1                 TO  AMAINTL                      EL632
00549          MOVE AL-UABON           TO  AMAINTA                      EL632
00550          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL632
00551          GO TO 8200-SEND-DATAONLY.                                EL632
00552                                                                   EL632
00553      IF AMAINTI = 'S' OR 'C' OR 'A' OR 'D' OR 'K'                 EL632
00554          MOVE AMAINTI            TO  WS-MAINT                     EL632
00555      ELSE                                                         EL632
00556          MOVE ER-0023            TO  EMI-ERROR                    EL632
00557          MOVE -1                 TO  AMAINTL                      EL632
00558          MOVE AL-UABON           TO  AMAINTA                      EL632
00559          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL632
00560          GO TO 8200-SEND-DATAONLY.                                EL632
00561                                                                   EL632
00562      IF AMAINTI = 'K'                                             EL632
00563          IF PI-MAINT = 'S'  OR  'K'                               EL632
00564              NEXT SENTENCE                                        EL632
00565          ELSE                                                     EL632
00566              MOVE ER-2167        TO  EMI-ERROR                    EL632
00567              MOVE -1             TO  AMAINTL                      EL632
00568              MOVE AL-UABON       TO  AMAINTA                      EL632
00569              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL632
00570              GO TO 8200-SEND-DATAONLY.                            EL632
00571                                                                   EL632
00572      IF ACARIERL GREATER ZEROS                                    EL632
00573          MOVE AL-UANON           TO  ACARIERA                     EL632
00574          PERFORM 1200-VERIFY-CARRIER-ID  THRU  1200-EXIT          EL632
00575          MOVE ACARIERI           TO  PI-PNDC-CARRIER              EL632
00576      ELSE                                                         EL632
00577          IF NOT ST-ACCNT-CNTL  AND  NOT ACCNT-CNTL                EL632
00578              MOVE -1             TO  ACARIERL                     EL632
00579              MOVE AL-UABON       TO  ACARIERA                     EL632
00580              MOVE ER-0194        TO  EMI-ERROR                    EL632
00581              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00582                                                                   EL632
00583      IF PI-CARRIER-SECURITY GREATER SPACES                        EL632
00584          IF ACARIERL GREATER ZEROS                                EL632
00585              IF ACARIERI = PI-CARRIER-SECURITY                    EL632
00586                  NEXT SENTENCE                                    EL632
00587              ELSE                                                 EL632
00588                  MOVE -1         TO  ACARIERL                     EL632
00589                  MOVE AL-UABON   TO  ACARIERA                     EL632
00590                  MOVE ER-2370    TO  EMI-ERROR                    EL632
00591                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.      EL632
00592                                                                   EL632
00593      IF AGROUPL GREATER ZEROS                                     EL632
00594          MOVE AL-UANON           TO  AGROUPA                      EL632
00595          MOVE AGROUPI            TO  PI-PNDC-GROUPING             EL632
00596      ELSE                                                         EL632
00597          IF CARR-GROUP-ST-ACCNT-CNTL                              EL632
00598              MOVE -1             TO  AGROUPL                      EL632
00599              MOVE AL-UABON       TO  AGROUPA                      EL632
00600              MOVE ER-0195        TO  EMI-ERROR                    EL632
00601              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00602                                                                   EL632
00603      IF ASTATEL GREATER ZEROS                                     EL632
00604          MOVE AL-UANON           TO  ASTATEA                      EL632
00605          PERFORM 1300-VERIFY-STATE-ID  THRU  1300-EXIT            EL632
00606          MOVE ASTATEI            TO  PI-PNDC-STATE                EL632
00607      ELSE                                                         EL632
00608          IF NOT ACCNT-CNTL  AND  NOT CARR-ACCNT-CNTL              EL632
00609              MOVE -1             TO  ASTATEL                      EL632
00610              MOVE AL-UABON       TO  ASTATEA                      EL632
00611              MOVE ER-0196        TO  EMI-ERROR                    EL632
00612              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00613                                                                   EL632
00614      IF AACCTL GREATER ZEROS                                      EL632
00615          MOVE AL-UANON           TO  AACCTA                       EL632
00616          PERFORM 1400-VERIFY-ACCOUNT  THRU  1400-EXIT             EL632
00617          MOVE AACCTI             TO  PI-PNDC-ACCOUNT              EL632
00618      ELSE                                                         EL632
00619          MOVE -1                 TO  AACCTL                       EL632
00620          MOVE AL-UABON           TO  AACCTA                       EL632
00621          MOVE ER-0197            TO  EMI-ERROR                    EL632
00622          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL632
00623                                                                   EL632
00624      IF PI-ACCOUNT-SECURITY GREATER SPACES                        EL632
00625          IF AACCTL GREATER ZEROS                                  EL632
00626              IF AACCTI = PI-ACCOUNT-SECURITY                      EL632
00627                      NEXT SENTENCE                                EL632
00628                  ELSE                                             EL632
00629                      MOVE -1        TO  AACCTL                    EL632
00630                      MOVE AL-UABON  TO  AACCTA                    EL632
00631                      MOVE ER-2371   TO  EMI-ERROR                 EL632
00632                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.  EL632
00633                                                                   EL632
00634      IF ACERTL NOT = ZEROS                                        EL632
00635          MOVE AL-UANON           TO  ACERTA                       EL632
00636          MOVE ACERTI             TO  PI-PNDC-PRIME                EL632
00637      ELSE                                                         EL632
00638          MOVE ER-0203            TO  EMI-ERROR                    EL632
00639          MOVE -1                 TO  ACERTL                       EL632
00640          MOVE AL-UABON           TO  ACERTA                       EL632
00641          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL632
00642                                                                   EL632
00643      IF ACRTSFXL NOT = ZEROS                                      EL632
00644          MOVE ACRTSFXI           TO  PI-PNDC-SFX                  EL632
00645          MOVE AL-UANON           TO  ACRTSFXA                     EL632
00646      ELSE                                                         EL632
00647          MOVE SPACES             TO  PI-PNDC-SFX.                 EL632
00648                                                                   EL632
00649      IF AEFFDTEL NOT = ZEROS                                      EL632
00650          MOVE AL-UNNON           TO  AEFFDTEA                     EL632
00651          IF AEFFDTEI  IS NUMERIC                                  EL632
00652              MOVE 4              TO  DC-OPTION-CODE               EL632
00653              MOVE AEFFDTEI       TO  DC-GREG-DATE-1-MDY           EL632
00654              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT           EL632
00655              MOVE DC-BIN-DATE-1  TO  WS-CONVERTED-EFFDT           EL632
00656              IF NO-CONVERSION-ERROR                               EL632
00657                  MOVE WS-CONVERTED-EFFDT  TO  PI-PNDC-CERT-EFF-DT EL632
00658              ELSE                                                 EL632
00659                  MOVE -1         TO  AEFFDTEL                     EL632
00660                  MOVE ER-2226    TO  EMI-ERROR                    EL632
00661                  MOVE AL-UNBON   TO  AEFFDTEA                     EL632
00662                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL632
00663          ELSE                                                     EL632
00664              MOVE -1             TO  AEFFDTEL                     EL632
00665              MOVE ER-2223        TO  EMI-ERROR                    EL632
00666              MOVE AL-UNBON       TO  AEFFDTEA                     EL632
00667              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL632
00668      ELSE                                                         EL632
00669          MOVE -1                 TO  AEFFDTEL                     EL632
00670          MOVE ER-2220            TO  EMI-ERROR                    EL632
00671          MOVE AL-UNBON           TO  AEFFDTEA                     EL632
00672          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL632
00673                                                                   EL632
00674      IF ACLAIML NOT = ZEROS                                       EL632
00675          MOVE AL-UANON           TO  ACLAIMA                      EL632
00676          MOVE ACLAIMI            TO  PI-PNDC-CLAIM-NO             EL632
00677      ELSE                                                         EL632
00678          IF ACHECKL NOT = ZEROS                                   EL632
00679              MOVE ACHECKI        TO  ACLAIMI                      EL632
00680                                      PI-PNDC-CLAIM-NO             EL632
00681              MOVE AL-UANON       TO  ACLAIMA                      EL632
00682          ELSE                                                     EL632
00683              MOVE ER-0209        TO  EMI-ERROR                    EL632
00684              MOVE -1             TO  ACLAIML                      EL632
00685              MOVE AL-UABON       TO  ACLAIMA                      EL632
00686              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00687                                                                   EL632
00688      IF ACTYPEL NOT = ZEROS                                       EL632
00689          MOVE ACTYPEI            TO  WS-VALID-CLAIM-TYPES         EL632
00690          IF VALID-CLAIM-TYPE                                      EL632
00691              MOVE AL-UANON       TO  ACTYPEA                      EL632
00692          ELSE                                                     EL632
00693              MOVE ER-2445        TO  EMI-ERROR                    EL632
00694              MOVE -1             TO  ACTYPEL                      EL632
00695              MOVE AL-UABON       TO  ACTYPEA                      EL632
00696              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL632
00697      ELSE                                                         EL632
00698          IF AMAINTI = 'A'                                         EL632
00699              MOVE ER-2446        TO  EMI-ERROR                    EL632
00700              MOVE -1             TO  ACTYPEL                      EL632
00701              MOVE AL-UABON       TO  ACTYPEA                      EL632
00702              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00703                                                                   EL632
00704      IF ACHECKL NOT = ZEROS                                       EL632
00705          MOVE AL-UANON           TO  ACHECKA                      EL632
00706          MOVE ACHECKI            TO  PI-PNDC-CHECK-NO             EL632
00707      ELSE                                                         EL632
00708          MOVE ER-2447            TO  EMI-ERROR                    EL632
00709          MOVE -1                 TO  ACHECKL                      EL632
00710          MOVE AL-UABON           TO  ACHECKA                      EL632
00711          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL632
00712                                                                   EL632
00713      IF AMAINTI IS = 'S'  OR  'D'                                 EL632
00714          GO TO 1000-EDIT-COMPLETE.                                EL632
00715                                                                   EL632
00716      IF APAYMNTL NOT = ZEROS                                      EL632
00717          MOVE AL-UNNON           TO  APAYMNTA                     EL632
CIDMOD         MOVE APAYMNTI           TO  WS-DEEDIT-FIELD                   000
CIDMOD         PERFORM 8600-DEEDIT  THRU  8600-EXIT                          000
CIDMOD         MOVE WS-DEEDIT-FIELD    TO  APAYMNTI                          000
00722      ELSE                                                         EL632
00723          IF AMAINTI = 'A'                                         EL632
00724              MOVE ER-2448        TO  EMI-ERROR                    EL632
00725              MOVE -1             TO  APAYMNTL                     EL632
00726              MOVE AL-UNBON       TO  APAYMNTA                     EL632
00727              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00728                                                                   EL632
00729      IF APTYPEL NOT = ZEROS                                       EL632
00730          MOVE APTYPEI            TO  WS-VALID-PAY-TYPES           EL632
00731          IF VALID-PAY-TYPE                                        EL632
00732              MOVE AL-UANON       TO  APTYPEA                      EL632
00733          ELSE                                                     EL632
00734              MOVE ER-2853        TO  EMI-ERROR                    EL632
00735              MOVE -1             TO  APTYPEL                      EL632
00736              MOVE AL-UABON       TO  APTYPEA                      EL632
00737              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL632
00738      ELSE                                                         EL632
00739          IF AMAINTI = 'A'                                         EL632
00740              MOVE ER-2853        TO  EMI-ERROR                    EL632
00741              MOVE -1             TO  APTYPEL                      EL632
00742              MOVE AL-UABON       TO  APTYPEA                      EL632
00743              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00744                                                                   EL632
00745      IF AINCURL NOT = ZEROS                                       EL632
00746          MOVE AL-UNNON           TO  AINCURA                      EL632
00747          IF AINCURI  IS NUMERIC                                   EL632
00748              MOVE 4              TO  DC-OPTION-CODE               EL632
00749              MOVE AINCURI        TO  DC-GREG-DATE-1-MDY           EL632
00750              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT           EL632
00751              MOVE DC-BIN-DATE-1  TO  WS-CONVERTED-INCUR           EL632
00752              IF NO-CONVERSION-ERROR                               EL632
00753                  NEXT SENTENCE                                    EL632
00754              ELSE                                                 EL632
00755                  MOVE -1         TO  AINCURL                      EL632
00756                  MOVE ER-2452    TO  EMI-ERROR                    EL632
00757                  MOVE AL-UNBON   TO  AINCURA                      EL632
00758                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL632
00759          ELSE                                                     EL632
00760              MOVE -1             TO  AINCURL                      EL632
00761              MOVE ER-2451        TO  EMI-ERROR                    EL632
00762              MOVE AL-UNBON       TO  AINCURA                      EL632
00763              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00764                                                                   EL632
00765      IF AREPORTL NOT = ZEROS                                      EL632
00766          MOVE AL-UNNON           TO  AREPORTA                     EL632
00767          IF AREPORTI  IS NUMERIC                                  EL632
00768              MOVE 4              TO  DC-OPTION-CODE               EL632
00769              MOVE AREPORTI       TO  DC-GREG-DATE-1-MDY           EL632
00770              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT           EL632
00771              MOVE DC-BIN-DATE-1  TO  WS-CONVERTED-REPORT          EL632
00772              IF NO-CONVERSION-ERROR                               EL632
00773                  NEXT SENTENCE                                    EL632
00774              ELSE                                                 EL632
00775                  MOVE -1         TO  AREPORTL                     EL632
00776                  MOVE ER-2455    TO  EMI-ERROR                    EL632
00777                  MOVE AL-UNBON   TO  AREPORTA                     EL632
00778                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL632
00779          ELSE                                                     EL632
00780              MOVE -1             TO  AREPORTL                     EL632
00781              MOVE ER-2454        TO  EMI-ERROR                    EL632
00782              MOVE AL-UNBON       TO  AREPORTA                     EL632
00783              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00784                                                                   EL632
00785      IF APAIDL NOT = ZEROS                                        EL632
00786          MOVE AL-UNNON           TO  APAIDA                       EL632
00787          IF APAIDI  IS NUMERIC                                    EL632
00788              MOVE 4              TO  DC-OPTION-CODE               EL632
00789              MOVE APAIDI         TO  DC-GREG-DATE-1-MDY           EL632
00790              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT           EL632
00791              MOVE DC-BIN-DATE-1  TO  WS-CONVERTED-PAID            EL632
00792              IF NO-CONVERSION-ERROR                               EL632
00793                  NEXT SENTENCE                                    EL632
00794              ELSE                                                 EL632
00795                  MOVE -1         TO  APAIDL                       EL632
00796                  MOVE ER-2458    TO  EMI-ERROR                    EL632
00797                  MOVE AL-UNBON   TO  APAIDA                       EL632
00798                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL632
00799          ELSE                                                     EL632
00800              MOVE -1             TO  APAIDL                       EL632
00801              MOVE ER-2457        TO  EMI-ERROR                    EL632
00802              MOVE AL-UNBON       TO  APAIDA                       EL632
00803              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00804                                                                   EL632
00805      IF APTHRUL NOT EQUAL ZEROS                                      CL**3
00806         MOVE AL-UNNON           TO  APTHRUA                          CL**3
00807         IF APTHRUI  IS NUMERIC                                       CL**3
00808            MOVE '4'            TO  DC-OPTION-CODE                    CL**3
00809            MOVE APTHRUI        TO  DC-GREG-DATE-1-MDY                CL**3
00810            PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT                CL**3
00811            MOVE DC-BIN-DATE-1  TO  WS-CONVERTED-PTHRU                CL**3
00812            IF NO-CONVERSION-ERROR                                    CL**3
00813               IF PI-USES-PAID-TO                                     CL**3
00814                  MOVE '6'            TO  DC-OPTION-CODE              CL**3
00815                  MOVE APTHRUI        TO  DC-GREG-DATE-1-MDY          CL**3
00816                  MOVE -1             TO DC-ELAPSED-DAYS              CL**3
00817                  MOVE +0             TO DC-ELAPSED-MONTHS            CL**3
00818                  PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT          CL**3
00819                  MOVE DC-BIN-DATE-2  TO  WS-CONVERTED-PTHRU          CL**3
00820               ELSE                                                   CL**3
00821                  NEXT SENTENCE                                    EL632
00822            ELSE                                                      CL**3
00823               MOVE -1         TO  APTHRUL                            CL**3
00824               MOVE ER-2461    TO  EMI-ERROR                          CL**3
00825               MOVE AL-UNBON   TO  APTHRUA                            CL**3
00826               PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT             CL**3
00827         ELSE                                                         CL**3
00828            MOVE -1             TO  APTHRUL                           CL**3
00829            MOVE ER-2460        TO  EMI-ERROR                         CL**3
00830            MOVE AL-UNBON       TO  APTHRUA                           CL**3
00831            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.               CL**3
00832                                                                   EL632
00833      IF ADAYSL NOT = ZEROS                                        EL632
00834          IF ADAYSI  IS NUMERIC                                    EL632
00835              MOVE AL-UNNON       TO  ADAYSA                       EL632
00836          ELSE                                                     EL632
00837              MOVE ER-2463        TO  EMI-ERROR                    EL632
00838              MOVE -1             TO  ADAYSL                       EL632
00839              MOVE AL-UNBON       TO  ADAYSA                       EL632
00840              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL632
00841      ELSE                                                         EL632
00842          IF AMAINTI = 'A'                                         EL632
00843            AND (ACTYPEI = '2'  OR  '4')                           EL632
00844            AND (APTYPEI NOT = '5'  AND  '6')                      EL632
00845              MOVE ER-2462        TO  EMI-ERROR                    EL632
00846              MOVE -1             TO  ADAYSL                       EL632
00847              MOVE AL-UNBON       TO  ADAYSA                       EL632
00848              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00849                                                                   EL632
00850      IF AAGEL NOT = ZEROS                                         EL632
00851          IF AAGEI  IS NUMERIC                                     EL632
00852              MOVE AL-UNNON       TO  AAGEA                        EL632
00853          ELSE                                                     EL632
00854              MOVE ER-2464        TO  EMI-ERROR                    EL632
00855              MOVE -1             TO  AAGEL                        EL632
00856              MOVE AL-UNBON       TO  AAGEA                        EL632
00857              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00858                                                                   EL632
00859      IF ACAUSEL NOT = ZEROS                                       EL632
00860          MOVE AL-UANON           TO  ACAUSEA.                     EL632
00861                                                                   EL632
00862      IF AFORCEL NOT = ZEROS                                       EL632
00863          MOVE AL-UANON           TO  AFORCEA.                     EL632
00864                                                                   EL632
00865      IF AEOMDTL NOT = ZEROS                                       EL632
00866          MOVE AL-UNNON           TO  AEOMDTA                      EL632
00867          MOVE AEOMDTI            TO  WS-DEEDIT-FIELD1             EL632
00868          EXEC CICS BIF DEEDIT                                     EL632
00869              FIELD   (WS-DEEDIT-FIELD1)                           EL632
00870              LENGTH  (11)                                         EL632
00871          END-EXEC                                                 EL632
00872          MOVE WS-DEEDIT-FIELDV0   TO  AEOMDTO                     EL632
00873                                       DC-GREG-DATE-1-MDY          EL632
00874          INSPECT AEOMDTO CONVERTING SPACES TO '/'                    CL**8
00875          MOVE 4                   TO  DC-OPTION-CODE              EL632
00876          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT               EL632
00877          MOVE DC-GREG-DATE-1-MDY  TO  WS-DATE-TEST                EL632
00878          IF (DATE-CONVERSION-ERROR)                               EL632
00879            OR  (DC-DAYS-IN-MONTH NOT = WS-DATE-DA)                EL632
00880              MOVE -1              TO  AEOMDTL                     EL632
00881              MOVE ER-0587         TO  EMI-ERROR                   EL632
00882              MOVE AL-UNBON        TO  AEOMDTA                     EL632
00883              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL632
00884          ELSE                                                     EL632
00885              MOVE DC-BIN-DATE-1   TO  WS-EL632-EOM-DATE.          EL632
00886                                                                   EL632
00887  1000-EDIT-COMPLETE.                                              EL632
00888      MOVE AMAINTI                TO  PI-MAINT.                    EL632
00889                                                                   EL632
00890      IF PI-MAINT = 'C'                                            EL632
00891          IF PI-ERPNDC-KEY = PI-PREV-ERPNDC-KEY                    EL632
00892              NEXT SENTENCE                                        EL632
00893          ELSE                                                     EL632
00894              MOVE ER-2761          TO  EMI-ERROR                  EL632
00895              MOVE -1               TO  ACARIERL                   EL632
00896              MOVE AL-UABON         TO  ACARIERA                   EL632
00897                                        AGROUPA                    EL632
00898                                        ASTATEA                    EL632
00899                                        AACCTA                     EL632
00900                                        ACERTA                     EL632
00901                                        ACRTSFXA                   EL632
00902                                        AEFFDTEA                   EL632
00903                                        ACLAIMA                    EL632
00904                                        ACHECKA                    EL632
00905              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00906                                                                   EL632
00907      IF AMAINTI = 'S'                                             EL632
00908          MOVE ZEROS              TO  PI-RECORD-SEQUENCE           EL632
00909          MOVE '1'                TO  PI-PNDC-REC-TYP              EL632
00910          GO TO 5000-BROWSE-CLAIMS-FILE.                           EL632
00911                                                                   EL632
00912      IF EMI-ERROR NOT = ZEROS                                     EL632
00913          MOVE PI-PREV-ERPNDC-KEY  TO  PI-ERPNDC-KEY               EL632
00914          GO TO 8200-SEND-DATAONLY.                                EL632
00915                                                                   EL632
00916      IF AMAINTI = 'D'                                             EL632
00917          GO TO 4000-DELETE-RECORD.                                EL632
00918                                                                   EL632
00919      IF AMAINTI = 'A'                                             EL632
00920          GO TO 2000-ADD-RECORD.                                   EL632
00921                                                                   EL632
00922      IF AMAINTI = 'C'                                             EL632
00923          GO TO 3000-CHANGE-RECORD.                                EL632
00924                                                                   EL632
00925      IF AMAINTI = 'K'                                             EL632
00926          PERFORM 3500-REWRITE-RECORD  THRU  3900-EXIT             EL632
00927          GO TO 3000-CHANGE-RECORD.                                EL632
00928  EJECT                                                            EL632
00929  1100-EDIT-RESERVE-DATA.                                          EL632
00930      MOVE PI-ERPNDC-KEY          TO  PI-PREV-ERPNDC-KEY.          EL632
00931      MOVE SPACES                 TO  PI-PNDC-CHECK-NO.            EL632
00932                                                                   EL632
00933      IF PI-HAS-CLAS-IC-CLAIM                                      EL632
00934        AND  BMAINTI NOT = 'S'                                     EL632
00935          MOVE ER-2243            TO  EMI-ERROR                    EL632
00936          MOVE -1                 TO  BMAINTL                      EL632
00937          MOVE AL-UABON           TO  BMAINTA                      EL632
00938          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL632
00939          GO TO 1100-EDIT-COMPLETE.                                EL632
00940                                                                   EL632
00941      IF BMAINTI = 'S'  OR  'C'  OR  'A'                           EL632
00942        OR  'D'  OR  'K'                                           EL632
00943          MOVE BMAINTI            TO  WS-MAINT                     EL632
00944      ELSE                                                         EL632
00945          MOVE ER-0023            TO  EMI-ERROR                    EL632
00946          MOVE -1                 TO  BMAINTL                      EL632
00947          MOVE AL-UABON           TO  BMAINTA                      EL632
00948          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL632
00949          GO TO 1100-EDIT-COMPLETE.                                EL632
00950                                                                   EL632
00951      IF BMAINTI = 'K'                                             EL632
00952          IF PI-MAINT = 'S'  OR  'K'                               EL632
00953              NEXT SENTENCE                                        EL632
00954          ELSE                                                     EL632
00955              MOVE ER-2167        TO  EMI-ERROR                    EL632
00956              MOVE -1             TO  BMAINTL                      EL632
00957              MOVE AL-UABON       TO  BMAINTA                      EL632
00958              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL632
00959              GO TO 8200-SEND-DATAONLY.                            EL632
00960                                                                   EL632
00961      IF BCARIERL GREATER ZEROS                                    EL632
00962          MOVE AL-UANON           TO  BCARIERA                     EL632
00963          PERFORM 1200-VERIFY-CARRIER-ID  THRU  1200-EXIT          EL632
00964          MOVE BCARIERI           TO  PI-PNDC-CARRIER              EL632
00965      ELSE                                                         EL632
00966          IF NOT ST-ACCNT-CNTL  AND  NOT ACCNT-CNTL                EL632
00967              MOVE -1             TO  BCARIERL                     EL632
00968              MOVE AL-UABON       TO  BCARIERA                     EL632
00969              MOVE ER-0194        TO  EMI-ERROR                    EL632
00970              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00971                                                                   EL632
00972      IF BGROUPL GREATER ZEROS                                     EL632
00973          MOVE AL-UANON           TO  BGROUPA                      EL632
00974          MOVE BGROUPI            TO  PI-PNDC-GROUPING             EL632
00975      ELSE                                                         EL632
00976          IF CARR-GROUP-ST-ACCNT-CNTL                              EL632
00977              MOVE -1             TO  BGROUPL                      EL632
00978              MOVE AL-UABON       TO  BGROUPA                      EL632
00979              MOVE ER-0195        TO  EMI-ERROR                    EL632
00980              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00981                                                                   EL632
00982      IF BSTATEL GREATER ZEROS                                     EL632
00983          MOVE AL-UANON           TO  BSTATEA                      EL632
00984          PERFORM 1300-VERIFY-STATE-ID  THRU  1300-EXIT            EL632
00985          MOVE BSTATEI            TO  PI-PNDC-STATE                EL632
00986      ELSE                                                         EL632
00987          IF NOT ACCNT-CNTL  AND  NOT CARR-ACCNT-CNTL              EL632
00988              MOVE -1             TO  BSTATEL                      EL632
00989              MOVE AL-UABON       TO  BSTATEA                      EL632
00990              MOVE ER-0196        TO  EMI-ERROR                    EL632
00991              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
00992                                                                   EL632
00993      IF BMAINTI = 'S'  OR  'A'  OR  'C'                           EL632
00994          IF BACCTL GREATER ZEROS                                  EL632
00995              MOVE AL-UANON           TO  BACCTA                   EL632
00996              PERFORM 1400-VERIFY-ACCOUNT  THRU  1400-EXIT         EL632
00997              MOVE BACCTI             TO  PI-PNDC-ACCOUNT          EL632
00998          ELSE                                                     EL632
00999              MOVE -1                 TO  BACCTL                   EL632
01000              MOVE AL-UABON           TO  BACCTA                   EL632
01001              MOVE ER-0197            TO  EMI-ERROR                EL632
01002              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
01003                                                                   EL632
01004      IF PI-COMPANY-ID = 'RIG'                                     EL632
01005        AND  BMAINTI NOT = 'D'                                     EL632
01006          MOVE AM-CARRIER         TO  BCARIERI                     EL632
01007          PERFORM 1200-VERIFY-CARRIER-ID  THRU  1200-EXIT          EL632
01008          MOVE LOW-VALUES TO BCARIERI.                             EL632
01009                                                                   EL632
01010      IF BCERTL NOT = ZEROS                                        EL632
01011          MOVE AL-UANON           TO  BCERTA                       EL632
01012          MOVE BCERTI             TO  PI-PNDC-PRIME                EL632
01013      ELSE                                                         EL632
01014          MOVE ER-0203            TO  EMI-ERROR                    EL632
01015          MOVE -1                 TO  BCERTL                       EL632
01016          MOVE AL-UABON           TO  BCERTA                       EL632
01017          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL632
01018                                                                   EL632
01019      IF BCRTSFXL NOT = ZEROS                                      EL632
01020          MOVE BCRTSFXI           TO  PI-PNDC-SFX                  EL632
01021          MOVE AL-UANON           TO  BCRTSFXA.                    EL632
01022                                                                   EL632
01023      IF BEFFDTEL NOT = ZEROS                                      EL632
01024          MOVE AL-UNNON           TO  BEFFDTEA                     EL632
01025          IF BEFFDTEI  IS NUMERIC                                  EL632
01026              MOVE 4              TO  DC-OPTION-CODE               EL632
01027              MOVE BEFFDTEI       TO  DC-GREG-DATE-1-MDY           EL632
01028              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT           EL632
01029              MOVE DC-BIN-DATE-1  TO  WS-CONVERTED-EFFDT           EL632
01030              IF NO-CONVERSION-ERROR                               EL632
01031                  MOVE WS-CONVERTED-EFFDT                          EL632
01032                                  TO  PI-PNDC-CERT-EFF-DT          EL632
01033              ELSE                                                 EL632
01034                  MOVE -1         TO  BEFFDTEL                     EL632
01035                  MOVE ER-2226    TO  EMI-ERROR                    EL632
01036                  MOVE AL-UNBON   TO  BEFFDTEA                     EL632
01037                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL632
01038          ELSE                                                     EL632
01039              MOVE -1             TO  BEFFDTEL                     EL632
01040              MOVE ER-2223        TO  EMI-ERROR                    EL632
01041              MOVE AL-UNBON       TO  BEFFDTEA                     EL632
01042              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL632
01043      ELSE                                                         EL632
01044          MOVE -1                 TO  BEFFDTEL                     EL632
01045          MOVE ER-2220            TO  EMI-ERROR                    EL632
01046          MOVE AL-UNBON           TO  BEFFDTEA                     EL632
01047          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL632
01048                                                                   EL632
01049      IF BCLAIML NOT = ZEROS                                       EL632
01050          MOVE AL-UANON           TO  BCLAIMA                      EL632
01051          MOVE BCLAIMI            TO  PI-PNDC-CLAIM-NO             EL632
01052      ELSE                                                         EL632
01053          MOVE ER-0209            TO  EMI-ERROR                    EL632
01054          MOVE -1                 TO  BCLAIML                      EL632
01055          MOVE AL-UABON           TO  BCLAIMA                      EL632
01056          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL632
01057                                                                   EL632
01058      IF BCLMTYPL NOT = ZEROS                                      EL632
01059          MOVE BCLMTYPI           TO  WS-VALID-CLAIM-TYPES         EL632
01060          IF VALID-CLAIM-TYPE                                      EL632
01061              MOVE AL-UANON       TO  BCLMTYPA                     EL632
01062          ELSE                                                     EL632
01063              MOVE ER-2445        TO  EMI-ERROR                    EL632
01064              MOVE -1             TO  BCLMTYPL                     EL632
01065              MOVE AL-UABON       TO  BCLMTYPA                     EL632
01066              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL632
01067      ELSE                                                         EL632
01068          IF BMAINTI = 'A'                                         EL632
01069              MOVE ER-2446        TO  EMI-ERROR                    EL632
01070              MOVE -1             TO  BCLMTYPL                     EL632
01071              MOVE AL-UABON       TO  BCLMTYPA                     EL632
01072              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
01073                                                                   EL632
01074      IF BMAINTI = 'S'  OR  'D'                                    EL632
01075          GO TO 1100-EDIT-COMPLETE.                                EL632
01076                                                                   EL632
01077      IF BFUTUREL NOT =  ZEROS                                     EL632
01078          IF WS-FUTURE-RESERVES-USED                               EL632
CIDMOD             MOVE BFUTUREI         TO  WS-DEEDIT-FIELD                 000
CIDMOD             PERFORM 8600-DEEDIT  THRU  8600-EXIT                      000
CIDMOD             MOVE WS-DEEDIT-FIELD  TO  BFUTUREI                        000
01083              MOVE AL-UNNON         TO  BFUTUREA                   EL632
01084          ELSE                                                     EL632
01085              MOVE ER-2467          TO  EMI-ERROR                  EL632
01086              MOVE -1               TO  BFUTUREL                   EL632
01087              MOVE AL-UNBON         TO  BFUTUREA                   EL632
01088              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
01089                                                                   EL632
01090      IF BPAYCURL NOT = ZEROS                                      EL632
01091          IF WS-PAY-TO-CURRENT-USED                                EL632
CIDMOD             MOVE BPAYCURI         TO  WS-DEEDIT-FIELD                 000
CIDMOD             PERFORM 8600-DEEDIT  THRU  8600-EXIT                      000
CIDMOD             MOVE WS-DEEDIT-FIELD  TO  BPAYCURI                        000
01096              MOVE AL-UNNON         TO  BPAYCURA                   EL632
01097          ELSE                                                     EL632
01098              MOVE ER-2468          TO  EMI-ERROR                  EL632
01099              MOVE -1               TO  BPAYCURL                   EL632
01100              MOVE AL-UNBON         TO  BPAYCURA                   EL632
01101              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
01102                                                                   EL632
01103      IF BIBNRL NOT = ZEROS                                        EL632
01104          IF WS-IBNR-RESERVES-USED                                 EL632
CIDMOD             MOVE BIBNRI           TO  WS-DEEDIT-FIELD                 000
CIDMOD             PERFORM 8600-DEEDIT  THRU  8600-EXIT                      000
CIDMOD             MOVE WS-DEEDIT-FIELD  TO  BIBNRI                          000
01109              MOVE AL-UNNON         TO  BIBNRA                     EL632
01110          ELSE                                                     EL632
01111              MOVE ER-2469          TO  EMI-ERROR                  EL632
01112              MOVE -1               TO  BIBNRL                     EL632
01113              MOVE AL-UNBON         TO  BIBNRA                     EL632
01114              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
01115                                                                   EL632
01116      IF BMANUALL NOT = ZEROS                                      EL632
01117          IF WS-MANUAL-RESERVES-USED                               EL632
CIDMOD             MOVE BMANUALI         TO  WS-DEEDIT-FIELD                 000
CIDMOD             PERFORM 8600-DEEDIT  THRU  8600-EXIT                      000
CIDMOD             MOVE WS-DEEDIT-FIELD  TO  BMANUALI                        000
01122              MOVE AL-UNNON         TO  BMANUALA                   EL632
01123          ELSE                                                     EL632
01124              MOVE ER-2470          TO  EMI-ERROR                  EL632
01125              MOVE -1               TO  BMANUALL                   EL632
01126              MOVE AL-UNBON         TO  BMANUALA                   EL632
01127              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
01128                                                                   EL632
01129      IF BFORCEL NOT = ZEROS                                       EL632
01130          MOVE BFORCEI            TO  WS-VALID-FORCE-CD            EL632
01131          IF VALID-FORCE-CD                                        EL632
01132              MOVE AL-UANON       TO  BFORCEA                      EL632
01133          ELSE                                                     EL632
01134              MOVE ER-2815        TO  EMI-ERROR                       CL**4
01135              MOVE -1             TO  BFORCEL                      EL632
01136              MOVE AL-UABON       TO  BFORCEA                      EL632
01137              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL632
01138                                                                   EL632
01139      IF BEOMDTL NOT = ZEROS                                       EL632
01140          MOVE AL-UNNON           TO  BEOMDTA                      EL632
01141          MOVE BEOMDTI            TO  WS-DEEDIT-FIELD1             EL632
01142          EXEC CICS BIF DEEDIT                                     EL632
01143               FIELD   (WS-DEEDIT-FIELD1)                          EL632
01144               LENGTH  (11)                                        EL632
01145          END-EXEC                                                 EL632
01146          MOVE WS-DEEDIT-FIELDV0  TO  BEOMDTO                      EL632
01147                                      DC-GREG-DATE-1-MDY           EL632
01148          INSPECT BEOMDTO CONVERTING SPACES TO '/'                    CL**8
01149          MOVE 4                  TO  DC-OPTION-CODE               EL632
01150          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT               EL632
01151          MOVE DC-GREG-DATE-1-MDY  TO  WS-DATE-TEST                EL632
01152          IF (DATE-CONVERSION-ERROR)                               EL632
01153            OR (DC-DAYS-IN-MONTH NOT = WS-DATE-DA)                 EL632
01154              MOVE -1             TO  BEOMDTL                      EL632
01155              MOVE ER-0587        TO  EMI-ERROR                    EL632
01156              MOVE AL-UNBON       TO  BEOMDTA                      EL632
01157              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL632
01158          ELSE                                                     EL632
01159              MOVE DC-BIN-DATE-1  TO  WS-EL632-EOM-DATE.           EL632
01160                                                                   EL632
01161  1100-EDIT-COMPLETE.                                              EL632
01162      MOVE BMAINTI                TO  PI-MAINT.                    EL632
01163                                                                   EL632
01164      IF PI-MAINT = 'C'                                            EL632
01165          IF PI-ERPNDC-KEY = PI-PREV-ERPNDC-KEY                    EL632
01166              NEXT SENTENCE                                        EL632
01167          ELSE                                                     EL632
01168              MOVE ER-2761        TO  EMI-ERROR                    EL632
01169              MOVE -1             TO  BCARIERL                     EL632
01170              MOVE AL-UABON       TO  BCARIERA                     EL632
01171                                      BGROUPA                      EL632
01172                                      BSTATEA                      EL632
01173                                      BACCTA                       EL632
01174                                      BCERTA                       EL632
01175                                      BCRTSFXA                     EL632
01176                                      BEFFDTEA                     EL632
01177                                      BCLAIMA                      EL632
01178            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.            EL632
01179                                                                   EL632
01180      IF EMI-ERROR NOT = ZEROS                                     EL632
01181          MOVE PI-PREV-ERPNDC-KEY  TO  PI-ERPNDC-KEY               EL632
01182          GO TO 8200-SEND-DATAONLY.                                EL632
01183                                                                   EL632
01184      IF BMAINTI = 'A'                                             EL632
01185          GO TO 2000-ADD-RECORD.                                   EL632
01186                                                                   EL632
01187      IF BMAINTI = 'C'                                             EL632
01188          GO TO 3000-CHANGE-RECORD.                                EL632
01189                                                                   EL632
01190      IF BMAINTI = 'K'                                             EL632
01191          PERFORM 3500-REWRITE-RECORD  THRU  3900-EXIT             EL632
01192          GO TO 3000-CHANGE-RECORD.                                EL632
01193                                                                   EL632
01194      IF BMAINTI = 'D'                                             EL632
01195          GO TO 4000-DELETE-RECORD.                                EL632
01196                                                                   EL632
01197      IF AMAINTI = 'S'                                             EL632
01198          MOVE ZEROS              TO  PI-RECORD-SEQUENCE           EL632
01199          MOVE '2'                TO  PI-PNDC-REC-TYP              EL632
01200          MOVE SPACES             TO  PI-PNDC-CHECK-NO             EL632
01201          GO TO 5000-BROWSE-CLAIMS-FILE.                           EL632
01202  EJECT                                                            EL632
01203  1200-VERIFY-CARRIER-ID.                                          EL632
01204      MOVE SPACES                 TO  ELCNTL-KEY.                  EL632
01205      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL632
01206      MOVE '6'                    TO  CNTL-REC-TYPE.               EL632
01207                                                                   EL632
01208      IF WS-CARRIER-SW = 'Y'                                       EL632
01209          MOVE WS-SV-CARRIER      TO  CNTL-CARRIER                 EL632
01210      ELSE                                                         EL632
01211          IF PI-MAP-NAME = EL632A                                  EL632
01212              MOVE ACARIERI       TO  CNTL-CARRIER                 EL632
01213          ELSE                                                     EL632
01214              MOVE BCARIERI       TO  CNTL-CARRIER.                EL632
01215                                                                   EL632
01216      MOVE +0                     TO  CNTL-SEQ.                    EL632
01217                                                                   EL632
01218      EXEC CICS HANDLE CONDITION                                   EL632
01219          NOTFND  (1200-NO-CARRIER)                                EL632
01220      END-EXEC.                                                    EL632
01221                                                                   EL632
01222      EXEC CICS READ                                               EL632
01223          DATASET  (ELCNTL-FILE-ID)                                EL632
01224          SET      (ADDRESS OF CONTROL-FILE)                          CL**8
01225          RIDFLD   (ELCNTL-KEY)                                    EL632
01226      END-EXEC.                                                    EL632
01227                                                                   EL632
01228      MOVE CF-RESERVE-CONTROLS    TO  WS-RESERVE-CONTROLS.         EL632
01229                                                                   EL632
01230      GO TO 1200-EXIT.                                             EL632
01231                                                                   EL632
01232  1200-NO-CARRIER.                                                 EL632
01233      MOVE ER-2208                TO  EMI-ERROR.                   EL632
01234                                                                   EL632
01235      IF PI-MAP-NAME = EL632A                                      EL632
01236          MOVE -1                 TO  ACARIERL                     EL632
01237          MOVE AL-UABON           TO  ACARIERA                     EL632
01238      ELSE                                                         EL632
01239          MOVE -1                 TO  BCARIERL                     EL632
01240          MOVE AL-UABON           TO  BCARIERA.                    EL632
01241                                                                   EL632
01242      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
01243                                                                   EL632
01244  1200-EXIT.                                                       EL632
01245      EXIT.                                                        EL632
01246  EJECT                                                            EL632
01247  1300-VERIFY-STATE-ID.                                            EL632
01248      MOVE SPACES                 TO  ELCNTL-KEY.                  EL632
01249      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL632
01250      MOVE '3'                    TO  CNTL-REC-TYPE.               EL632
01251                                                                   EL632
01252      IF PI-MAP-NAME = EL632A                                      EL632
01253          MOVE ASTATEI            TO  CNTL-STATE                   EL632
01254      ELSE                                                         EL632
01255          MOVE BSTATEI            TO  CNTL-STATE.                  EL632
01256                                                                   EL632
01257      MOVE +0                     TO  CNTL-SEQ.                    EL632
01258                                                                   EL632
01259      EXEC CICS HANDLE CONDITION                                   EL632
01260          NOTFND  (1300-NO-STATE)                                  EL632
01261      END-EXEC.                                                    EL632
01262                                                                   EL632
01263      EXEC CICS READ                                               EL632
01264          DATASET  (ELCNTL-FILE-ID)                                EL632
01265          SET      (ADDRESS OF CONTROL-FILE)                          CL**8
01266          RIDFLD   (ELCNTL-KEY)                                    EL632
01267      END-EXEC.                                                    EL632
01268                                                                   EL632
01269      GO TO 1300-EXIT.                                             EL632
01270                                                                   EL632
01271  1300-NO-STATE.                                                   EL632
01272      MOVE ER-2209                TO  EMI-ERROR.                   EL632
01273                                                                   EL632
01274      IF PI-MAP-NAME = EL632A                                      EL632
01275          MOVE -1                 TO  ASTATEL                      EL632
01276          MOVE AL-UABON           TO  ASTATEA                      EL632
01277      ELSE                                                         EL632
01278          MOVE -1                 TO  BSTATEL                      EL632
01279          MOVE AL-UABON           TO  BSTATEA.                     EL632
01280                                                                   EL632
01281      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
01282                                                                   EL632
01283  1300-EXIT.                                                       EL632
01284      EXIT.                                                        EL632
01285  EJECT                                                            EL632
01286  1400-VERIFY-ACCOUNT.                                             EL632
01287      IF PI-MAP-NAME = EL632B                                      EL632
01288          GO TO 1400-MAPB-DATA.                                    EL632
01289                                                                   EL632
01290      IF AMAINTI = 'D'  OR  'S'                                    EL632
01291          GO TO 1400-EXIT.                                         EL632
01292                                                                   EL632
01293      IF ACARIERL GREATER ZEROS                                    EL632
01294          MOVE ACARIERI           TO  ACCT-CARRIER                 EL632
01295      ELSE                                                         EL632
01296          MOVE SPACES             TO  ACCT-CARRIER.                EL632
01297                                                                   EL632
01298      IF AGROUPL GREATER ZEROS                                     EL632
01299          MOVE AGROUPI            TO  ACCT-GROUPING                EL632
01300      ELSE                                                         EL632
01301          MOVE SPACES             TO  ACCT-GROUPING.               EL632
01302                                                                   EL632
01303      IF ASTATEL GREATER ZEROS                                     EL632
01304          MOVE ASTATEI            TO  ACCT-STATE                   EL632
01305      ELSE                                                         EL632
01306          MOVE SPACES             TO  ACCT-STATE.                  EL632
01307                                                                   EL632
01308      MOVE AACCTI                 TO  ACCT-ACCOUNT.                EL632
01309                                                                   EL632
01310      GO TO 1400-READ-ACCOUNT.                                     EL632
01311                                                                   EL632
01312  1400-MAPB-DATA.                                                  EL632
01313      IF BMAINTI = 'D'  OR  'S'                                    EL632
01314          GO TO 1400-EXIT.                                         EL632
01315                                                                   EL632
01316      IF BCARIERL GREATER ZEROS                                    EL632
01317          MOVE BCARIERI           TO  ACCT-CARRIER                 EL632
01318      ELSE                                                         EL632
01319          MOVE SPACES             TO  ACCT-CARRIER.                EL632
01320                                                                   EL632
01321      IF BGROUPL GREATER ZEROS                                     EL632
01322          MOVE BGROUPI            TO  ACCT-GROUPING                EL632
01323      ELSE                                                         EL632
01324          MOVE SPACES             TO  ACCT-GROUPING.               EL632
01325                                                                   EL632
01326      IF BSTATEL GREATER ZEROS                                     EL632
01327          MOVE BSTATEI            TO  ACCT-STATE                   EL632
01328      ELSE                                                         EL632
01329          MOVE SPACES             TO  ACCT-STATE.                  EL632
01330                                                                   EL632
01331      MOVE BACCTI                 TO  ACCT-ACCOUNT.                EL632
01332                                                                   EL632
01333  1400-READ-ACCOUNT.                                               EL632
01334      MOVE PI-COMPANY-CD          TO  ACCT-CO.                     EL632
01335      MOVE LOW-VALUES             TO  ACCT-EXP-DATE.               EL632
01336                                                                   EL632
01337      EXEC CICS HANDLE CONDITION                                   EL632
01338          NOTFND  (1400-ACCOUNT-INVALID)                           EL632
01339      END-EXEC.                                                    EL632
01340                                                                   EL632
01341      EXEC CICS READ                                               EL632
01342          GTEQ                                                     EL632
01343          DATASET  (ERACCT-FILE-ID)                                EL632
01344          SET      (ADDRESS OF ACCOUNT-MASTER)                        CL**8
01345          RIDFLD   (ERACCT-KEY)                                    EL632
01346      END-EXEC.                                                    EL632
01347                                                                   EL632
01348      MOVE AM-CONTROL-BY-VAR-GRP  TO  ERACCT-SAVE-KEY.             EL632
01349                                                                   EL632
01350      IF ERACCT-COMP-KEY NOT = ERACCT-SAVE-KEY                     EL632
01351          GO TO 1400-ACCOUNT-INVALID.                              EL632
01352                                                                   EL632
01353      MOVE AM-CARRIER             TO  WS-SV-CARRIER.               EL632
01354      MOVE AM-GROUPING            TO  WS-SV-GROUPING.              EL632
01355      MOVE AM-STATE               TO  WS-SV-STATE.                 EL632
01356                                                                   EL632
01357      IF ST-ACCNT-CNTL  OR  ACCNT-CNTL                             EL632
01358          MOVE 'Y'                TO  WS-CARRIER-SW                EL632
01359          PERFORM 1200-VERIFY-CARRIER-ID  THRU  1200-EXIT.         EL632
01360                                                                   EL632
01361      GO TO 1400-EXIT.                                             EL632
01362                                                                   EL632
01363  1400-ACCOUNT-INVALID.                                            EL632
01364      IF PI-MAP-NAME = EL632A                                      EL632
01365          IF CARR-GROUP-ST-ACCNT-CNTL                              EL632
01366              MOVE -1                     TO  ACARIERL             EL632
01367              MOVE AL-UABON               TO  ACARIERA             EL632
01368                                              AGROUPA              EL632
01369                                              ASTATEA              EL632
01370                                              AACCTA               EL632
01371          ELSE                                                     EL632
01372              IF ST-ACCNT-CNTL                                     EL632
01373                  MOVE -1                 TO  ASTATEL              EL632
01374                  MOVE AL-UABON           TO  ASTATEA              EL632
01375                                              AACCTA               EL632
01376              ELSE                                                 EL632
01377                  IF CARR-ST-ACCNT-CNTL                            EL632
01378                      MOVE -1             TO  ACARIERL             EL632
01379                      MOVE AL-UABON       TO  ACARIERA             EL632
01380                                              ASTATEA              EL632
01381                                              AACCTA               EL632
01382                  ELSE                                             EL632
01383                      IF ACCNT-CNTL                                EL632
01384                          MOVE -1         TO  AACCTL               EL632
01385                          MOVE AL-UABON   TO  AACCTA               EL632
01386                      ELSE                                         EL632
01387                          MOVE -1         TO  ACARIERL             EL632
01388                          MOVE AL-UABON   TO  ACARIERA             EL632
01389                                              AACCTA               EL632
01390      ELSE                                                         EL632
01391          IF CARR-GROUP-ST-ACCNT-CNTL                              EL632
01392              MOVE -1                     TO  BCARIERL             EL632
01393              MOVE AL-UABON               TO  BCARIERA             EL632
01394                                              BGROUPA              EL632
01395                                              BSTATEA              EL632
01396                                              BACCTA               EL632
01397          ELSE                                                     EL632
01398              IF ST-ACCNT-CNTL                                     EL632
01399                  MOVE -1                 TO  BSTATEL              EL632
01400                  MOVE AL-UABON           TO  BSTATEA              EL632
01401                                              BACCTA               EL632
01402              ELSE                                                 EL632
01403                  IF CARR-ST-ACCNT-CNTL                            EL632
01404                      MOVE -1             TO  BCARIERL             EL632
01405                      MOVE AL-UABON       TO  BCARIERA             EL632
01406                                              BSTATEA              EL632
01407                                              BACCTA               EL632
01408                  ELSE                                             EL632
01409                      IF ACCNT-CNTL                                EL632
01410                          MOVE -1         TO  BACCTL               EL632
01411                          MOVE AL-UABON   TO  BACCTA               EL632
01412                      ELSE                                         EL632
01413                          MOVE -1         TO  BCARIERL             EL632
01414                          MOVE AL-UABON   TO  BCARIERA             EL632
01415                                              BACCTA.              EL632
01416                                                                   EL632
01417      MOVE ER-2210                TO  EMI-ERROR.                   EL632
01418                                                                   EL632
01419      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
01420                                                                   EL632
01421  1400-EXIT.                                                       EL632
01422      EXIT.                                                        EL632
01423  EJECT                                                            EL632
01424  2000-ADD-RECORD.                                                 EL632
01425      EXEC CICS HANDLE CONDITION                                   EL632
01426          DUPREC  (2900-DUPLICATE-RECORD)                          EL632
01427      END-EXEC.                                                    EL632
01428                                                                   EL632
01429      EXEC CICS GETMAIN                                            EL632
01430          SET      (ADDRESS OF PENDING-CLAIMS)                        CL**8
01431          LENGTH   (500)                                           EL632
01432          INITIMG  (GETMAIN-SPACE)                                 EL632
01433      END-EXEC.                                                    EL632
01434                                                                   EL632
01435      MOVE PI-COMPANY-CD          TO  PC-COMPANY-CD.               EL632
01436      MOVE PI-COMPANY-ID          TO  PC-COMPANY-ID.               EL632
01437      MOVE 'PC'                   TO  PC-RECORD-ID.                EL632
01438      MOVE ZEROS                  TO  PC-RECORD-SEQUENCE.          EL632
01439                                                                   EL632
01440      IF PI-MAP-NAME = EL632B                                      EL632
01441          GO TO 2100-ADD-RESERVE-RECORD.                           EL632
01442                                                                   EL632
01443      MOVE '1'                    TO  PC-RECORD-TYPE.              EL632
01444                                                                   EL632
01445      IF ACARIERL NOT = ZEROS                                      EL632
01446          MOVE ACARIERI           TO  PC-CARRIER.                  EL632
01447                                                                   EL632
01448      IF AGROUPL NOT = ZEROS                                       EL632
01449          MOVE AGROUPI            TO  PC-GROUPING.                 EL632
01450                                                                   EL632
01451      IF ASTATEL NOT = ZEROS                                       EL632
01452          MOVE ASTATEI            TO  PC-STATE.                    EL632
01453                                                                   EL632
01454      IF AACCTL NOT = ZEROS                                        EL632
01455          MOVE AACCTI             TO  PC-ACCOUNT.                  EL632
01456                                                                   EL632
01457      MOVE WS-CONVERTED-EFFDT     TO  PC-CERT-EFF-DT.              EL632
01458      MOVE ACERTI                 TO  PC-CERT-PRIME.               EL632
01459                                                                   EL632
01460      IF ACRTSFXL NOT = ZEROS                                      EL632
01461          MOVE ACRTSFXI           TO  PC-CERT-SFX.                 EL632
01462                                                                   EL632
01463      MOVE ACLAIMI                TO  PC-CLAIM-NO.                 EL632
01464      MOVE ACHECKI                TO  PC-CHECK-NO.                 EL632
01465      MOVE ACTYPEI                TO  PC-CLAIM-TYPE.               EL632
01466      MOVE APAYMNTI               TO  PC-CLAIM-PAYMENT.            EL632
01467      MOVE APTYPEI                TO  PC-PAYMENT-TYPE.             EL632
01468                                                                   EL632
01469      IF AINCURL NOT = ZEROS                                       EL632
01470          MOVE WS-CONVERTED-INCUR  TO  PC-INCURRED-DT              EL632
01471      ELSE                                                         EL632
01472          MOVE LOW-VALUES          TO  PC-INCURRED-DT.             EL632
01473                                                                   EL632
01474      IF AREPORTL NOT = ZEROS                                      EL632
01475          MOVE WS-CONVERTED-REPORT     TO  PC-REPORTED-DT          EL632
01476      ELSE                                                         EL632
01477          IF AINCURL NOT = ZEROS                                   EL632
01478              MOVE WS-CONVERTED-INCUR  TO  PC-REPORTED-DT          EL632
01479          ELSE                                                     EL632
01480              MOVE LOW-VALUES          TO  PC-REPORTED-DT.         EL632
01481                                                                   EL632
01482      IF APAIDL NOT =  ZEROS                                       EL632
01483          MOVE WS-CONVERTED-PAID  TO  PC-PAYMENT-DT                EL632
01484      ELSE                                                         EL632
01485          MOVE LOW-VALUES         TO  PC-PAYMENT-DT.               EL632
01486                                                                   EL632
01487      IF APTHRUL NOT = ZEROS                                       EL632
01488          MOVE WS-CONVERTED-PTHRU  TO  PC-PAID-THRU-DT             EL632
01489      ELSE                                                         EL632
01490          MOVE LOW-VALUES          TO  PC-PAID-THRU-DT.            EL632
01491                                                                   EL632
01492      IF ADAYSL NOT = ZEROS                                        EL632
01493          MOVE ADAYSI             TO  PC-NO-OF-DAYS-PAID           EL632
01494      ELSE                                                         EL632
01495          MOVE ZEROS              TO  PC-NO-OF-DAYS-PAID.          EL632
01496                                                                   EL632
01497      IF AAGEL NOT = ZEROS                                         EL632
01498          MOVE AAGEI              TO  PC-AGE-AT-CLAIM              EL632
01499      ELSE                                                         EL632
01500          MOVE ZEROS              TO  PC-AGE-AT-CLAIM.             EL632
01501                                                                   EL632
01502      IF ACAUSEL NOT = ZEROS                                       EL632
01503          MOVE ACAUSEI            TO  PC-CAUSE-CODE.               EL632
01504                                                                   EL632
01505      IF AFORCEL NOT = ZEROS                                       EL632
01506          MOVE AFORCEI            TO  PC-FORCE-CODE.               EL632
01507                                                                   EL632
01508      MOVE ZEROS                  TO  PC-FUTURE-RESERVE-AMT        EL632
01509                                      PC-IBNR-RESERVE-AMT          EL632
01510                                      PC-PTC-RESERVE-AMT           EL632
01511                                      PC-MANUAL-RESERVE-AMT        EL632
01512                                      PC-REMAINING-BENEFIT         EL632
01513                                      PC-REMAINING-TERM.           EL632
01514                                                                   EL632
01515      GO TO 2200-WRITE-THE-RECORD.                                 EL632
01516                                                                   EL632
01517  2100-ADD-RESERVE-RECORD.                                         EL632
01518      MOVE '2'                    TO  PC-RECORD-TYPE.              EL632
01519                                                                   EL632
01520      IF BCARIERL NOT = ZEROS                                      EL632
01521          MOVE BCARIERI           TO  PC-CARRIER.                  EL632
01522                                                                   EL632
01523      IF BGROUPL NOT = ZEROS                                       EL632
01524          MOVE BGROUPI            TO  PC-GROUPING.                 EL632
01525                                                                   EL632
01526      IF BSTATEL NOT = ZEROS                                       EL632
01527          MOVE BSTATEI            TO  PC-STATE.                    EL632
01528                                                                   EL632
01529      IF BACCTL NOT = ZEROS                                        EL632
01530          MOVE BACCTI             TO  PC-ACCOUNT.                  EL632
01531                                                                   EL632
01532      MOVE WS-CONVERTED-EFFDT     TO  PC-CERT-EFF-DT.              EL632
01533      MOVE BCERTI                 TO  PC-CERT-PRIME.               EL632
01534                                                                   EL632
01535      IF BCRTSFXL NOT = ZEROS                                      EL632
01536          MOVE BCRTSFXI           TO  PC-CERT-SFX.                 EL632
01537                                                                   EL632
01538      MOVE BCLAIMI                TO  PC-CLAIM-NO.                 EL632
01539      MOVE BCLMTYPI               TO  PC-CLAIM-TYPE.               EL632
01540                                                                   EL632
01541      IF BFUTUREL NOT = ZEROS                                         CL**2
01542          MOVE BFUTUREI           TO  PC-FUTURE-RESERVE-AMT        EL632
01543      ELSE                                                         EL632
01544          MOVE ZEROS              TO  PC-FUTURE-RESERVE-AMT.       EL632
01545                                                                   EL632
01546      IF BPAYCURL NOT = ZEROS                                      EL632
01547          MOVE BPAYCURI           TO  PC-PTC-RESERVE-AMT           EL632
01548      ELSE                                                         EL632
01549          MOVE ZEROS              TO  PC-PTC-RESERVE-AMT.          EL632
01550                                                                   EL632
01551      IF BIBNRL NOT = ZEROS                                        EL632
01552          MOVE BIBNRI             TO  PC-IBNR-RESERVE-AMT          EL632
01553      ELSE                                                         EL632
01554          MOVE ZEROS              TO  PC-IBNR-RESERVE-AMT.         EL632
01555                                                                   EL632
01556      IF BMANUALL NOT = ZEROS                                      EL632
01557          MOVE BMANUALI           TO  PC-MANUAL-RESERVE-AMT        EL632
01558      ELSE                                                         EL632
01559          MOVE ZEROS              TO  PC-MANUAL-RESERVE-AMT.       EL632
01560                                                                   EL632
01561      IF BFORCEL NOT = ZEROS                                       EL632
01562          MOVE BFORCEI            TO  PC-FORCE-CODE.               EL632
01563                                                                   EL632
01564      MOVE ZEROS                  TO  PC-CLAIM-PAYMENT.            EL632
01565                                                                   EL632
01566  2200-WRITE-THE-RECORD.                                           EL632
01567      MOVE WS-SV-CARRIER          TO  PC-SV-CARRIER.               EL632
01568      MOVE WS-SV-GROUPING         TO  PC-SV-GROUPING.              EL632
01569      MOVE WS-SV-STATE            TO  PC-SV-STATE.                 EL632
01570      MOVE ZEROS                  TO  PC-CC-PRIOR-DEATH-AMT        EL632
01571                                      PC-CC-PRIOR-LUMP-PMT.        EL632
01572      MOVE PI-PROCESSOR-ID        TO  PC-LAST-MAINT-BY.            EL632
01573      MOVE EIBTIME                TO  PC-LAST-MAINT-HHMMSS.        EL632
01574      MOVE WS-CURRENT-BIN-DT      TO  PC-LAST-MAINT-DT             EL632
01575                                      PC-INPUT-DT.                 EL632
01576      MOVE LOW-VALUES             TO  PC-CREDIT-ACCEPT-DT.         EL632
01577      MOVE PI-CR-MONTH-END-DT     TO  PC-CREDIT-SELECT-DT.         EL632
01578                                                                   EL632
01579      IF WS-EL632-EOM-DATE NOT = SPACES AND ZEROS                  EL632
01580        AND  LOW-VALUES                                            EL632
01581          MOVE WS-EL632-EOM-DATE  TO  PC-CREDIT-SELECT-DT.         EL632
01582                                                                   EL632
01583      MOVE ERPNDC-FILE-ID         TO  JP-FILE-ID.                  EL632
01584      MOVE 'A'                    TO  JP-RECORD-TYPE               EL632
01585      MOVE PENDING-CLAIMS         TO  JP-RECORD-AREA.              EL632
01586      MOVE PC-CONTROL-PRIMARY     TO  PI-ERPNDC-KEY.               EL632
01587                                                                   EL632
01588      PERFORM 9800-LINK-CLAIMS-EDIT.                               EL632
01589                                                                   EL632
01590      MOVE LOW-VALUES             TO  EL632AI.                     EL632
01591                                                                   EL632
01592      PERFORM 7000-FORMAT-ERRORS  THRU  7090-EXIT.                 EL632
01593                                                                   EL632
01594      PERFORM 6000-FORMAT-RECORD  THRU  6900-EXIT.                 EL632
01595                                                                   EL632
01596      IF PC-CLAIMS                                                 EL632
01597          MOVE 'C'                TO  AMAINTI                      EL632
01598          MOVE AL-UANON           TO  AMAINTA                      EL632
01599          IF NOT PC-FATAL-ERRORS                                   EL632
01600            AND  (PC-FORCE-ER-CD = 'F'  OR  ' ')                   EL632
01601              MOVE -1             TO  AMAINTL                      EL632
01602      ELSE                                                         EL632
01603          MOVE 'C'                TO  BMAINTI                      EL632
01604          MOVE AL-UANON           TO  BMAINTA                      EL632
01605          IF NOT PC-FATAL-ERRORS                                   EL632
01606            AND  (PC-FORCE-ER-CD = 'F'  OR  ' ')                   EL632
01607              MOVE -1             TO  BMAINTL.                     EL632
01608                                                                   EL632
01609  2300-WRITE.                                                      EL632
01610      EXEC CICS WRITE                                              EL632
01611          DATASET  (ERPNDC-FILE-ID)                                EL632
01612          FROM     (PENDING-CLAIMS)                                EL632
01613          RIDFLD   (PC-CONTROL-PRIMARY)                            EL632
01614      END-EXEC.                                                    EL632
01615                                                                   EL632
01616      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL632
01617                                                                   EL632
01618      IF NOT EMI-NO-ERRORS                                         EL632
01619          GO TO 8200-SEND-DATAONLY.                                EL632
01620                                                                   EL632
01621      MOVE ER-0000                TO  EMI-ERROR                    EL632
01622                                                                   EL632
01623      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
01624                                                                   EL632
01625      GO TO 8100-SEND-INITIAL-MAP.                                 EL632
01626                                                                   EL632
01627  2900-DUPLICATE-RECORD.                                           EL632
01628      MOVE 1                      TO  EMI-SUB                      EL632
01629                                      EMI-SWITCH1                  EL632
01630                                      EMI-SWITCH-AREA-1            EL632
01631                                      EMI-SWITCH-AREA-2            EL632
01632      MOVE SPACES                 TO  EMI-ERROR-LINES.             EL632
01633                                                                   EL632
01634      IF PI-MAP-NAME = EL632A                                      EL632
01635          MOVE 'A'                TO  AMAINTI                      EL632
01636          MOVE 1                  TO  AMAINTL                      EL632
01637          MOVE ER-2563            TO  EMI-ERROR                    EL632
01638          MOVE -1                 TO  ACHECKL                      EL632
01639          MOVE AL-UABON           TO  AACCTA                       EL632
01640                                      ACERTA                       EL632
01641                                      ACRTSFXA                     EL632
01642                                      AEFFDTEA                     EL632
01643                                      ACLAIMA                      EL632
01644                                      ACHECKA                      EL632
01645      ELSE                                                         EL632
01646          MOVE 'A'                TO  BMAINTI                      EL632
01647          MOVE 1                  TO  BMAINTL                      EL632
01648          MOVE ER-2574            TO  EMI-ERROR                    EL632
01649          MOVE -1                 TO  BCARIERL                     EL632
01650          MOVE AL-UABON           TO  BACCTA                       EL632
01651                                      BCERTA                       EL632
01652                                      BCRTSFXA                     EL632
01653                                      BEFFDTEA                     EL632
01654                                      BCLAIMA.                     EL632
01655                                                                   EL632
01656      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL632
01657          IF PI-MAP-NAME = EL632A                                  EL632
01658              MOVE AL-UABON       TO  ACARIERA                     EL632
01659                                      AGROUPA                      EL632
01660                                      ASTATEA                      EL632
01661          ELSE                                                     EL632
01662              MOVE AL-UABON       TO  BCARIERA                     EL632
01663                                      BGROUPA                      EL632
01664                                      BSTATEA.                     EL632
01665                                                                   EL632
01666      IF CARR-ST-ACCNT-CNTL                                        EL632
01667          IF PI-MAP-NAME = EL632A                                  EL632
01668              MOVE AL-UABON       TO  ACARIERA                     EL632
01669                                      ASTATEA                      EL632
01670          ELSE                                                     EL632
01671              MOVE AL-UABON       TO  BCARIERA                     EL632
01672                                      BSTATEA.                     EL632
01673                                                                   EL632
01674      IF CARR-ACCNT-CNTL                                           EL632
01675          IF PI-MAP-NAME = EL632A                                  EL632
01676              MOVE AL-UABON       TO  ACARIERA                     EL632
01677          ELSE                                                     EL632
01678              MOVE AL-UABON       TO  BCARIERA.                    EL632
01679                                                                   EL632
01680      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
01681                                                                   EL632
01682      MOVE PI-PREV-ERPNDC-KEY     TO  PI-ERPNDC-KEY.               EL632
01683                                                                   EL632
01684      GO TO 8200-SEND-DATAONLY.                                    EL632
01685  EJECT                                                            EL632
01686  3000-CHANGE-RECORD.                                              EL632
01687      EXEC CICS HANDLE CONDITION                                   EL632
01688          NOTFND  (3400-RECORD-NOTFND)                             EL632
01689      END-EXEC.                                                    EL632
01690                                                                   EL632
01691      EXEC CICS READ                                               EL632
01692          DATASET  (ERPNDC-FILE-ID)                                EL632
01693          SET      (ADDRESS OF PENDING-CLAIMS)                        CL**8
01694          RIDFLD   (PI-ERPNDC-KEY)                                 EL632
01695          UPDATE                                                   EL632
01696      END-EXEC.                                                    EL632
01697                                                                   EL632
01698      MOVE ERPNDC-FILE-ID         TO  JP-FILE-ID.                  EL632
01699      MOVE 'B'                    TO  JP-RECORD-TYPE               EL632
01700      MOVE PENDING-CLAIMS         TO  JP-RECORD-AREA.              EL632
01701                                                                   EL632
01702      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL632
01703                                                                   EL632
01704      IF PI-MAP-NAME = EL632B                                      EL632
01705          GO TO 3100-CHANGE-RESERVE-DATA.                          EL632
01706                                                                   EL632
01707      IF ACTYPEL NOT = ZEROS                                       EL632
01708          MOVE ACTYPEI            TO  PC-CLAIM-TYPE.               EL632
01709                                                                   EL632
01710      IF APAYMNTL NOT = ZEROS                                      EL632
01711          MOVE APAYMNTI           TO  PC-CLAIM-PAYMENT.            EL632
01712                                                                   EL632
01713      IF APTYPEL NOT = ZEROS                                       EL632
01714          MOVE APTYPEI            TO  PC-PAYMENT-TYPE.             EL632
01715                                                                   EL632
01716      IF AINCURL NOT = ZEROS                                       EL632
01717          MOVE WS-CONVERTED-INCUR  TO  PC-INCURRED-DT.             EL632
01718                                                                   EL632
01719      IF AREPORTL NOT = ZEROS                                      EL632
01720          MOVE WS-CONVERTED-REPORT  TO  PC-REPORTED-DT.            EL632
01721                                                                   EL632
01722      IF APAIDL NOT = ZEROS                                        EL632
01723          MOVE WS-CONVERTED-PAID  TO  PC-PAYMENT-DT.               EL632
01724                                                                   EL632
01725      IF APTHRUL NOT = ZEROS                                       EL632
01726          MOVE WS-CONVERTED-PTHRU  TO  PC-PAID-THRU-DT.            EL632
01727                                                                   EL632
01728      IF ADAYSL NOT = ZEROS                                        EL632
01729          MOVE ADAYSI             TO  PC-NO-OF-DAYS-PAID.          EL632
01730                                                                   EL632
01731      IF AAGEL NOT = ZEROS                                         EL632
01732          MOVE AAGEI              TO  PC-AGE-AT-CLAIM.             EL632
01733                                                                   EL632
01734      IF ACAUSEL NOT = ZEROS                                       EL632
01735          MOVE ACAUSEI            TO  PC-CAUSE-CODE.               EL632
01736                                                                   EL632
01737      IF AFORCEL NOT = ZEROS                                       EL632
01738          MOVE AFORCEI            TO  PC-FORCE-CODE.               EL632
01739                                                                   EL632
01740      GO TO 3200-REWRITE-THE-RECORD.                               EL632
01741  EJECT                                                            EL632
01742  3100-CHANGE-RESERVE-DATA.                                        EL632
01743      IF BFUTUREL NOT = ZEROS                                      EL632
01744          MOVE BFUTUREI           TO  PC-FUTURE-RESERVE-AMT.       EL632
01745                                                                   EL632
01746      IF BPAYCURL NOT = ZEROS                                      EL632
01747          MOVE BPAYCURI           TO  PC-PTC-RESERVE-AMT.          EL632
01748                                                                   EL632
01749      IF BIBNRL NOT = ZEROS                                        EL632
01750          MOVE BIBNRI             TO  PC-IBNR-RESERVE-AMT.         EL632
01751                                                                   EL632
01752      IF BMANUALL NOT = ZEROS                                      EL632
01753          MOVE BMANUALI           TO  PC-MANUAL-RESERVE-AMT.       EL632
01754                                                                   EL632
01755      IF BFORCEL NOT = ZEROS                                       EL632
01756          MOVE BFORCEI            TO  PC-FORCE-CODE.               EL632
01757                                                                   EL632
01758  3200-REWRITE-THE-RECORD.                                         EL632
01759      IF WS-EL632-EOM-DATE NOT = SPACES AND ZEROS                  EL632
01760        AND  LOW-VALUES                                            EL632
01761          MOVE WS-EL632-EOM-DATE   TO  PC-CREDIT-SELECT-DT.        EL632
01762                                                                   EL632
01763      MOVE PI-PROCESSOR-ID        TO  PC-LAST-MAINT-BY.            EL632
01764      MOVE EIBTIME                TO  PC-LAST-MAINT-HHMMSS.        EL632
01765      MOVE WS-CURRENT-BIN-DT      TO  PC-LAST-MAINT-DT.            EL632
01766      MOVE ERPNDC-FILE-ID         TO  JP-FILE-ID.                  EL632
01767      MOVE 'C'                    TO  JP-RECORD-TYPE               EL632
01768      MOVE PENDING-CLAIMS         TO  JP-RECORD-AREA.              EL632
01769      MOVE PC-CONTROL-PRIMARY     TO  PI-ERPNDC-KEY.               EL632
01770                                                                   EL632
01771      PERFORM 9800-LINK-CLAIMS-EDIT.                               EL632
01772                                                                   EL632
01773      MOVE LOW-VALUES             TO  EL632AI.                     EL632
01774                                                                   EL632
01775      PERFORM 7000-FORMAT-ERRORS  THRU  7090-EXIT.                 EL632
01776                                                                   EL632
01777      PERFORM 6000-FORMAT-RECORD  THRU  6900-EXIT.                 EL632
01778                                                                   EL632
01779      IF PC-CLAIMS                                                 EL632
01780          MOVE 'C'                TO  AMAINTI                      EL632
01781                                      PI-MAINT                     EL632
01782          MOVE AL-UANON           TO  AMAINTA                      EL632
01783          IF NOT PC-FATAL-ERRORS                                   EL632
01784            AND  (PC-FORCE-ER-CD = 'F'  OR  ' ')                   EL632
01785              MOVE -1             TO  AMAINTL                      EL632
01786      ELSE                                                         EL632
01787          MOVE 'C'                TO  BMAINTI                      EL632
01788                                      PI-MAINT                     EL632
01789          MOVE AL-UANON           TO  BMAINTA                      EL632
01790          IF NOT PC-FATAL-ERRORS                                   EL632
01791            AND  (PC-FORCE-ER-CD = 'F'  OR  ' ')                   EL632
01792              MOVE -1             TO  BMAINTL.                     EL632
01793                                                                   EL632
01794      EXEC CICS REWRITE                                            EL632
01795          DATASET  (ERPNDC-FILE-ID)                                EL632
01796          FROM     (PENDING-CLAIMS)                                EL632
01797      END-EXEC.                                                    EL632
01798                                                                   EL632
01799      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL632
01800                                                                   EL632
01801      IF NOT EMI-NO-ERRORS                                         EL632
01802          IF PI-MAP-NAME = EL632A                                  EL632
01803              MOVE 'C'            TO  AMAINTI                      EL632
01804              GO TO 8200-SEND-DATAONLY                             EL632
01805          ELSE                                                     EL632
01806              MOVE 'C'            TO  BMAINTI                      EL632
01807              GO TO 8200-SEND-DATAONLY.                            EL632
01808                                                                   EL632
01809      MOVE ER-0000                TO  EMI-ERROR.                   EL632
01810                                                                   EL632
01811      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
01812                                                                   EL632
01813      MOVE PI-PREV-ERPNDC-KEY     TO  PI-ERPNDC-KEY.               EL632
01814                                                                   EL632
01815      GO TO 8100-SEND-INITIAL-MAP.                                 EL632
01816                                                                   EL632
01817  3400-RECORD-NOTFND.                                              EL632
01818      MOVE ER-2465                TO  EMI-ERROR.                   EL632
01819      MOVE PI-PREV-ERPNDC-KEY     TO  PI-ERPNDC-KEY.               EL632
01820                                                                   EL632
01821      IF PI-MAP-NAME = EL632A                                      EL632
01822          MOVE -1                 TO  ACARIERL                     EL632
01823      ELSE                                                         EL632
01824          MOVE -1                 TO  BCARIERL.                    EL632
01825                                                                   EL632
01826      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
01827                                                                   EL632
01828      GO TO 8200-SEND-DATAONLY.                                    EL632
01829  EJECT                                                            EL632
01830  3500-REWRITE-RECORD.                                             EL632
01831                                                                   EL632
01832      EXEC CICS HANDLE CONDITION                                   EL632
01833          NOTFND  (3600-DELETE-OLDREC)                             EL632
01834      END-EXEC.                                                    EL632
01835                                                                   EL632
01836      EXEC CICS READ                                               EL632
01837          DATASET  (ERPNDC-FILE-ID)                                EL632
01838          SET      (ADDRESS OF PENDING-CLAIMS)                        CL**8
01839          RIDFLD   (PI-ERPNDC-KEY)                                 EL632
01840      END-EXEC.                                                    EL632
01841                                                                   EL632
01842      GO TO 3800-DUPLICATE-RECORD.                                 EL632
01843                                                                   EL632
01844  3600-DELETE-OLDREC.                                              EL632
01845      EXEC CICS HANDLE CONDITION                                   EL632
01846          NOTFND  (3700-REBUILD-KEY)                               EL632
01847      END-EXEC.                                                    EL632
01848                                                                   EL632
01849      EXEC CICS READ                                               EL632
01850          DATASET  (ERPNDC-FILE-ID)                                EL632
01851          SET      (ADDRESS OF PENDING-CLAIMS)                        CL**8
01852          RIDFLD   (PI-PREV-ERPNDC-KEY)                            EL632
01853          UPDATE                                                   EL632
01854      END-EXEC.                                                    EL632
01855                                                                   EL632
01856      MOVE ERPNDC-FILE-ID         TO  JP-FILE-ID.                  EL632
01857      MOVE 'D'                    TO  JP-RECORD-TYPE.              EL632
01858      MOVE PENDING-CLAIMS         TO  JP-RECORD-AREA.              EL632
01859                                                                   EL632
01860      EXEC CICS DELETE                                             EL632
01861           DATASET  (ERPNDC-FILE-ID)                               EL632
01862      END-EXEC.                                                    EL632
01863                                                                   EL632
01864      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL632
01865                                                                   EL632
01866  3700-REBUILD-KEY.                                                EL632
01867      EXEC CICS GETMAIN                                            EL632
01868          SET      (ADDRESS OF PENDING-CLAIMS)                        CL**8
01869          LENGTH   (500)                                           EL632
01870          INITIMG  (GETMAIN-SPACE)                                 EL632
01871      END-EXEC.                                                    EL632
01872                                                                   EL632
01873      MOVE JP-RECORD-AREA         TO  PENDING-CLAIMS.              EL632
01874                                                                   EL632
01875      IF PI-MAP-NAME = EL632B                                      EL632
01876          GO TO 3750-REWRITE-RESERVE-REC.                          EL632
01877                                                                   EL632
01878      IF ACARIERL NOT = ZEROS                                      EL632
01879          MOVE ACARIERI           TO  PC-CARRIER.                  EL632
01880                                                                   EL632
01881      IF AGROUPL NOT = ZEROS                                       EL632
01882          MOVE AGROUPI            TO  PC-GROUPING.                 EL632
01883                                                                   EL632
01884      IF ASTATEL NOT = ZEROS                                       EL632
01885          MOVE ASTATEI            TO  PC-STATE.                    EL632
01886                                                                   EL632
01887      IF AACCTL NOT = ZEROS                                        EL632
01888          MOVE AACCTI             TO  PC-ACCOUNT.                  EL632
01889                                                                   EL632
01890      IF  AEFFDTEL NOT = ZEROS                                     EL632
01891          MOVE WS-CONVERTED-EFFDT  TO  PC-CERT-EFF-DT.             EL632
01892                                                                   EL632
01893      IF  ACERTL NOT = ZEROS                                       EL632
01894          MOVE ACERTI             TO  PC-CERT-PRIME.               EL632
01895                                                                   EL632
01896      IF ACRTSFXL NOT = ZEROS                                      EL632
01897          MOVE ACRTSFXI           TO  PC-CERT-SFX.                 EL632
01898                                                                   EL632
01899      MOVE ACLAIMI                TO  PC-CLAIM-NO.                 EL632
01900      MOVE ACHECKI                TO  PC-CHECK-NO.                 EL632
01901                                                                   EL632
01902      GO TO 3790-WRITE-NEW-RECORD.                                 EL632
01903                                                                   EL632
01904  3750-REWRITE-RESERVE-REC.                                        EL632
01905      IF BCARIERL NOT = ZEROS                                      EL632
01906          MOVE BCARIERI           TO  PC-CARRIER.                  EL632
01907                                                                   EL632
01908      IF BGROUPL NOT = ZEROS                                       EL632
01909          MOVE BGROUPI            TO  PC-GROUPING.                 EL632
01910                                                                   EL632
01911      IF BSTATEL NOT = ZEROS                                       EL632
01912          MOVE BSTATEI            TO  PC-STATE.                    EL632
01913                                                                   EL632
01914      IF BACCTL NOT = ZEROS                                        EL632
01915          MOVE BACCTI             TO  PC-ACCOUNT.                  EL632
01916                                                                   EL632
01917      IF BEFFDTEL NOT = ZEROS                                      EL632
01918          MOVE WS-CONVERTED-EFFDT  TO  PC-CERT-EFF-DT.             EL632
01919                                                                   EL632
01920      IF BCERTL NOT = ZEROS                                        EL632
01921          MOVE BCERTI             TO  PC-CERT-PRIME.               EL632
01922                                                                   EL632
01923      IF BCRTSFXL NOT = ZEROS                                      EL632
01924          MOVE BCRTSFXI           TO  PC-CERT-SFX.                 EL632
01925                                                                   EL632
01926      MOVE BCLAIMI                TO  PC-CLAIM-NO.                 EL632
01927                                                                   EL632
01928  3790-WRITE-NEW-RECORD.                                           EL632
01929      MOVE ERPNDC-FILE-ID         TO  JP-FILE-ID.                  EL632
01930      MOVE 'A'                    TO  JP-RECORD-TYPE.              EL632
01931      MOVE PENDING-CLAIMS         TO  JP-RECORD-AREA.              EL632
01932      MOVE PC-CONTROL-PRIMARY     TO  PI-ERPNDC-KEY.               EL632
01933                                                                   EL632
01934      EXEC CICS WRITE                                              EL632
01935          DATASET  (ERPNDC-FILE-ID)                                EL632
01936          FROM     (PENDING-CLAIMS)                                EL632
01937          RIDFLD   (PC-CONTROL-PRIMARY)                            EL632
01938      END-EXEC.                                                    EL632
01939                                                                   EL632
01940      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL632
01941                                                                   EL632
01942      GO TO 3900-EXIT.                                             EL632
01943                                                                   EL632
01944  3800-DUPLICATE-RECORD.                                           EL632
01945      MOVE ER-2563                TO  EMI-ERROR.                      CL**4
01946                                                                   EL632
01947      IF PI-MAP-NAME = EL632A                                      EL632
01948          MOVE AL-UABON           TO  ACARIERA                     EL632
01949                                      AGROUPA                      EL632
01950                                      ASTATEA                      EL632
01951                                      AACCTA                       EL632
01952                                      ACERTA                       EL632
01953                                      ACRTSFXA                     EL632
01954                                      AEFFDTEA                     EL632
01955                                      ACLAIMA                      EL632
01956                                      ACTYPEA                      EL632
01957                                      ACHECKA                      EL632
01958          MOVE -1                 TO  ACARIERL                     EL632
01959      ELSE                                                         EL632
01960          MOVE AL-UABON           TO  BCARIERA                     EL632
01961                                      BGROUPA                      EL632
01962                                      BSTATEA                      EL632
01963                                      BACCTA                       EL632
01964                                      BEFFDTEA                     EL632
01965                                      BCERTA                       EL632
01966                                      BCRTSFXA                     EL632
01967                                      BCLAIMA                      EL632
01968          MOVE -1                 TO  BCARIERL.                    EL632
01969                                                                   EL632
01970      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
01971                                                                   EL632
01972      GO TO 8200-SEND-DATAONLY.                                    EL632
01973                                                                   EL632
01974  3900-EXIT.                                                       EL632
01975       EXIT.                                                       EL632
01976  EJECT                                                            EL632
01977  4000-DELETE-RECORD.                                              EL632
01978      EXEC CICS HANDLE CONDITION                                   EL632
01979          NOTFND  (4900-RECORD-NOTFND)                             EL632
01980      END-EXEC.                                                    EL632
01981                                                                   EL632
01982      MOVE 'D'                    TO  JP-RECORD-TYPE.              EL632
01983      MOVE PENDING-CLAIMS         TO  JP-RECORD-AREA.              EL632
01984                                                                   EL632
01985      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL632
01986                                                                   EL632
01987      EXEC CICS DELETE                                             EL632
01988          DATASET  (ERPNDC-FILE-ID)                                EL632
01989          RIDFLD   (PI-ERPNDC-KEY)                                 EL632
01990      END-EXEC.                                                    EL632
01991                                                                   EL632
01992      MOVE LOW-VALUES             TO  EL632AO.                     EL632
01993      MOVE ER-0000                TO  EMI-ERROR                    EL632
01994                                                                   EL632
01995      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
01996                                                                   EL632
01997      GO TO 8100-SEND-INITIAL-MAP.                                 EL632
01998                                                                   EL632
01999  4900-RECORD-NOTFND.                                              EL632
02000      MOVE ER-2465                TO  EMI-ERROR.                   EL632
02001      MOVE PI-PREV-ERPNDC-KEY     TO  PI-ERPNDC-KEY.               EL632
02002                                                                   EL632
02003      IF PI-MAP-NAME = EL632A                                      EL632
02004          MOVE -1                 TO  ACARIERL                     EL632
02005      ELSE                                                         EL632
02006          MOVE -1                 TO  BCARIERL.                    EL632
02007                                                                   EL632
02008      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
02009                                                                   EL632
02010      GO TO 8200-SEND-DATAONLY.                                    EL632
02011  EJECT                                                            EL632
02012  5000-BROWSE-CLAIMS-FILE.                                         EL632
02013      EXEC CICS HANDLE CONDITION                                   EL632
02014          NOTFND   (5800-NO-RECORD)                                EL632
02015          ENDFILE  (5900-END-OF-FILE)                              EL632
02016      END-EXEC.                                                    EL632
02017                                                                   EL632
02018      MOVE PI-ERPNDC-KEY          TO  ERPNDC-KEY.                  EL632
02019                                                                   EL632
02020      IF EIBAID = DFHPF2                                           EL632
02021          GO TO 5100-BROWSE-BKWD.                                  EL632
02022                                                                   EL632
02023  5010-READ-LOOP.                                                  EL632
02024      IF EIBAID = DFHPF1  OR  DFHPF9                               EL632
02025          ADD +1                  TO  PNDC-RECORD-SEQ.             EL632
02026                                                                   EL632
02027      EXEC CICS READ                                               EL632
02028          DATASET  (ERPNDC-FILE-ID)                                EL632
02029          SET      (ADDRESS OF PENDING-CLAIMS)                        CL**8
02030          RIDFLD   (ERPNDC-KEY)                                    EL632
02031          GTEQ                                                     EL632
02032      END-EXEC.                                                    EL632
02033                                                                   EL632
02034      IF PC-COMPANY-CD NOT = PI-COMPANY-CD                         EL632
02035          GO TO 5900-END-OF-FILE.                                  EL632
02036                                                                   EL632
02037      IF EIBAID = DFHPF1  OR  DFHPF9                               EL632
02038          NEXT SENTENCE                                            EL632
02039      ELSE                                                         EL632
02040          GO TO 5020-PROCESS-CLAIMS-RECORD.                        EL632
02041                                                                   EL632
02042      IF PI-NO-CARRIER-SECURITY                                    EL632
02043          IF PI-NO-ACCOUNT-SECURITY                                EL632
02044              GO TO 5020-PROCESS-CLAIMS-RECORD.                    EL632
02045                                                                   EL632
02046      IF PI-CARRIER-SECURITY GREATER SPACES                        EL632
02047          IF PI-CARRIER-SECURITY = PC-CARRIER                      EL632
02048              NEXT SENTENCE                                        EL632
02049          ELSE                                                     EL632
02050              MOVE PC-CONTROL-PRIMARY  TO  ERPNDC-KEY              EL632
02051              GO TO 5010-READ-LOOP.                                EL632
02052                                                                   EL632
02053      IF PI-ACCOUNT-SECURITY GREATER SPACES                        EL632
02054          IF PI-ACCOUNT-SECURITY = PC-ACCOUNT                      EL632
02055              NEXT SENTENCE                                        EL632
02056          ELSE                                                     EL632
02057              MOVE PC-CONTROL-PRIMARY  TO  ERPNDC-KEY              EL632
02058              GO TO 5010-READ-LOOP.                                EL632
02059                                                                   EL632
02060  5020-PROCESS-CLAIMS-RECORD.                                      EL632
02061      IF PC-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL632
02062          IF WS-MAINT = 'S'                                           CL**8
02063              GO TO 5800-NO-RECORD                                 EL632
02064          ELSE                                                     EL632
02065              MOVE PC-CONTROL-PRIMARY  TO  ERPNDC-KEY              EL632
02066              GO TO 5010-READ-LOOP.                                EL632
02067                                                                   EL632
02068      IF WS-MAINT = 'S'                                            EL632
02069          IF PNDC-CARRIER     = PC-CARRIER      AND                EL632
02070             PNDC-GROUPING    = PC-GROUPING     AND                EL632
02071             PNDC-STATE       = PC-STATE        AND                EL632
02072             PNDC-ACCOUNT     = PC-ACCOUNT      AND                EL632
02073             PNDC-CERT-EFF-DT = PC-CERT-EFF-DT  AND                EL632
02074             PNDC-CERT-NO     = PC-CERT-NO      AND                EL632
02075             PNDC-CLAIM-NO    = PC-CLAIM-NO     AND                EL632
02076             PNDC-CHECK-NO    = PC-CHECK-NO     AND                EL632
02077             PNDC-RECORD-TYPE = PC-RECORD-TYPE                     EL632
02078              GO TO 5200-FORMAT-SCREEN                             EL632
02079          ELSE                                                     EL632
02080              GO TO 5800-NO-RECORD.                                EL632
02081                                                                   EL632
02082      IF EIBAID = DFHPF9                                           EL632
02083          IF PC-FATAL-ERRORS                                       EL632
02084            OR PC-UNFORCED-ERRORS                                  EL632
02085              NEXT SENTENCE                                        EL632
02086          ELSE                                                     EL632
02087              MOVE PC-CONTROL-PRIMARY  TO  ERPNDC-KEY              EL632
02088              GO TO 5010-READ-LOOP.                                EL632
02089                                                                   EL632
02090      GO TO 5200-FORMAT-SCREEN.                                    EL632
02091  EJECT                                                            EL632
02092  5100-BROWSE-BKWD.                                                EL632
02093      EXEC CICS STARTBR                                            EL632
02094          DATASET  (ERPNDC-FILE-ID)                                EL632
02095          RIDFLD   (ERPNDC-KEY)                                    EL632
02096      END-EXEC.                                                    EL632
02097                                                                   EL632
02098      EXEC CICS READPREV                                           EL632
02099          DATASET  (ERPNDC-FILE-ID)                                EL632
02100          SET      (ADDRESS OF PENDING-CLAIMS)                        CL**8
02101          RIDFLD   (ERPNDC-KEY)                                    EL632
02102      END-EXEC.                                                    EL632
02103                                                                   EL632
02104  5110-READ-LOOP.                                                  EL632
02105      IF PI-FILE-EOF                                               EL632
02106          MOVE SPACE              TO  PI-EOF-SW                    EL632
02107      ELSE                                                         EL632
02108          EXEC CICS READPREV                                       EL632
02109              DATASET  (ERPNDC-FILE-ID)                            EL632
02110              SET      (ADDRESS OF PENDING-CLAIMS)                    CL**8
02111              RIDFLD   (ERPNDC-KEY)                                EL632
02112          END-EXEC.                                                EL632
02113                                                                   EL632
02114      IF PC-COMPANY-CD NOT = PI-COMPANY-CD                         EL632
02115          GO TO 5900-END-OF-FILE.                                  EL632
02116                                                                   EL632
02117      IF PI-NO-CARRIER-SECURITY                                    EL632
02118          IF PI-NO-ACCOUNT-SECURITY                                EL632
02119              GO TO 5120-PROCESS-CLAIMS-RECORD.                    EL632
02120                                                                   EL632
02121      IF PI-CARRIER-SECURITY GREATER SPACES                        EL632
02122          IF PI-CARRIER-SECURITY = PC-CARRIER                      EL632
02123              NEXT SENTENCE                                        EL632
02124          ELSE                                                     EL632
02125              GO TO 5110-READ-LOOP.                                EL632
02126                                                                   EL632
02127      IF PI-ACCOUNT-SECURITY GREATER SPACES                        EL632
02128          IF PI-ACCOUNT-SECURITY = PC-ACCOUNT                      EL632
02129              NEXT SENTENCE                                        EL632
02130          ELSE                                                     EL632
02131              GO TO 5110-READ-LOOP.                                EL632
02132                                                                   EL632
02133  5120-PROCESS-CLAIMS-RECORD.                                      EL632
02134      IF PC-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL632
02135          GO TO 5110-READ-LOOP.                                    EL632
02136                                                                   EL632
02137  5200-FORMAT-SCREEN.                                              EL632
02138      MOVE PC-CONTROL-PRIMARY     TO  PI-ERPNDC-KEY.               EL632
02139      MOVE PC-SV-CARRIER          TO  PI-CARRIER.                  EL632
02140      MOVE PC-SV-GROUPING         TO  PI-GROUPING.                 EL632
02141      MOVE PC-SV-STATE            TO  PI-STATE.                    EL632
02142      MOVE PC-ACCOUNT             TO  PI-ACCOUNT.                  EL632
02143      MOVE PC-CERT-NO             TO  PI-CERT-NO.                  EL632
02144      MOVE PC-CERT-EFF-DT         TO  PI-CERT-EFF-DT.              EL632
02145      MOVE LOW-VALUES             TO  EL632AI.                     EL632
02146                                                                   EL632
02147      PERFORM 7000-FORMAT-ERRORS  THRU  7090-EXIT.                 EL632
02148                                                                   EL632
02149      PERFORM 6000-FORMAT-RECORD  THRU  6900-EXIT.                 EL632
02150                                                                   EL632
02151      IF PC-CLAIMS                                                 EL632
02152          IF NOT PI-HAS-CLAS-IC-CLAIM                              EL632
02153              MOVE 'C'            TO  AMAINTI                      EL632
02154              MOVE AL-UANON       TO  AMAINTA                      EL632
02155              IF NOT PC-FATAL-ERRORS                               EL632
02156                AND (PC-FORCE-ER-CD = 'F' OR  ' ')                 EL632
02157                  MOVE -1         TO  AMAINTL                      EL632
02158              ELSE                                                 EL632
02159                  NEXT SENTENCE                                    EL632
02160          ELSE                                                     EL632
02161              MOVE 'S'            TO  AMAINTI                      EL632
02162              MOVE AL-UANON       TO  AMAINTA                      EL632
02163              IF NOT PC-FATAL-ERRORS                               EL632
02164                AND (PC-FORCE-ER-CD = 'F' OR  ' ')                 EL632
02165                  MOVE -1         TO  AMAINTL                      EL632
02166              ELSE                                                 EL632
02167                  NEXT SENTENCE                                    EL632
02168      ELSE                                                         EL632
02169          IF NOT PI-HAS-CLAS-IC-CLAIM                              EL632
02170              MOVE 'C'            TO  BMAINTI                      EL632
02171              MOVE AL-UANON       TO  BMAINTA                      EL632
02172              IF NOT PC-FATAL-ERRORS                               EL632
02173                AND (PC-FORCE-ER-CD = 'F' OR  ' ')                 EL632
02174                  MOVE -1         TO  BMAINTL                      EL632
02175              ELSE                                                 EL632
02176                  NEXT SENTENCE                                    EL632
02177          ELSE                                                     EL632
02178              MOVE 'S'            TO  BMAINTI                      EL632
02179              MOVE AL-UANON       TO  BMAINTA                      EL632
02180              IF NOT PC-FATAL-ERRORS                               EL632
02181                AND (PC-FORCE-ER-CD = 'F' OR  ' ')                 EL632
02182                  MOVE -1         TO  BMAINTL.                     EL632
02183                                                                   EL632
02184      GO TO 8100-SEND-INITIAL-MAP.                                 EL632
02185                                                                   EL632
02186  5800-NO-RECORD.                                                  EL632
02187      IF EIBAID = DFHPF1 OR  DFHPF9                                EL632
02188          GO TO 5900-END-OF-FILE.                                  EL632
02189                                                                   EL632
02190      MOVE PI-PREV-ERPNDC-KEY     TO  PI-ERPNDC-KEY.               EL632
02191      MOVE ER-2465                TO  EMI-ERROR.                   EL632
02192      MOVE PI-PREV-ERPNDC-KEY     TO  PI-ERPNDC-KEY.               EL632
02193                                                                   EL632
02194      IF PI-MAP-NAME = EL632A                                      EL632
02195          MOVE -1                 TO  ACARIERL                     EL632
02196      ELSE                                                         EL632
02197          MOVE -1                 TO  BCARIERL.                    EL632
02198                                                                   EL632
02199      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
02200                                                                   EL632
02201      GO TO 8200-SEND-DATAONLY.                                    EL632
02202                                                                   EL632
02203  5900-END-OF-FILE.                                                EL632
02204      IF EIBAID = DFHPF1 OR  DFHPF9                                EL632
02205          MOVE 'Y'                TO  PI-EOF-SW                    EL632
02206          MOVE -1                 TO  AMAINTL                      EL632
02207          MOVE ER-2237            TO  EMI-ERROR                    EL632
02208      ELSE                                                         EL632
02209          MOVE SPACES             TO  PI-ERPNDC-KEY                EL632
02210          MOVE ER-2238            TO  EMI-ERROR.                   EL632
02211                                                                   EL632
02212      IF PI-MAP-NAME = EL632A                                      EL632
02213          MOVE -1                 TO  ACARIERL                     EL632
02214      ELSE                                                         EL632
02215          MOVE -1                 TO  BCARIERL.                    EL632
02216                                                                   EL632
02217      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL632
02218                                                                   EL632
02219      IF PI-MAP-NAME = EL632A                                      EL632
02220          IF EMI-ERROR = ER-2238 OR  ER-2237                       EL632
02221              GO TO 8100-SEND-INITIAL-MAP.                         EL632
02222                                                                   EL632
02223      IF PI-MAP-NAME = EL632B                                      EL632
02224          IF EMI-ERROR = ER-2238 OR  ER-2237                       EL632
02225              GO TO 8110-SEND-INITIAL-MAPB.                        EL632
02226                                                                   EL632
02227      GO TO 8200-SEND-DATAONLY.                                    EL632
02228  EJECT                                                            EL632
02229  6000-FORMAT-RECORD.                                              EL632
02230      IF PC-RESERVES                                               EL632
02231          GO TO 6500-FORMAT-RESERVE-DATA.                          EL632
02232                                                                   EL632
02233      MOVE EL632A                 TO  PI-MAP-NAME.                 EL632
02234      MOVE PC-ACCOUNT             TO  AACCTO.                      EL632
02235                                                                   EL632
02236      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL632
02237          MOVE PC-CARRIER               TO  ACARIERO               EL632
02238          MOVE PC-GROUPING              TO  AGROUPO                EL632
02239          MOVE PC-STATE                 TO  ASTATEO                EL632
02240      ELSE                                                         EL632
02241          IF CARR-ST-ACCNT-CNTL                                    EL632
02242              MOVE PC-CARRIER           TO  ACARIERO               EL632
02243              MOVE PC-STATE             TO  ASTATEO                EL632
02244          ELSE                                                     EL632
02245              IF CARR-ACCNT-CNTL                                   EL632
02246                  MOVE PC-CARRIER       TO  ACARIERO               EL632
02247              ELSE                                                 EL632
02248                  IF ST-ACCNT-CNTL                                 EL632
02249                      MOVE PC-STATE     TO  ASTATEO.               EL632
02250                                                                   EL632
02251      MOVE PC-CERT-EFF-DT         TO  DC-BIN-DATE-1.               EL632
02252      MOVE SPACE                  TO  DC-OPTION-CODE.              EL632
02253                                                                   EL632
02254      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                  EL632
02255                                                                   EL632
02256      MOVE DC-GREG-DATE-1-MDY     TO  AEFFDTEO.                    EL632
02257      MOVE PC-CERT-PRIME          TO  ACERTI.                      EL632
02258                                                                   EL632
02259      IF PC-CERT-SFX NOT = SPACES                                  EL632
02260          MOVE PC-CERT-SFX        TO  ACRTSFXO.                    EL632
02261                                                                   EL632
02262      MOVE PC-CLAIM-NO            TO  ACLAIMO.                     EL632
02263      MOVE PC-CHECK-NO            TO  ACHECKO.                     EL632
02264                                                                   EL632
02265      IF WS-MAINT = 'S'                                            EL632
02266          MOVE AL-UANON           TO  ACARIERA                     EL632
02267                                      AGROUPA                      EL632
02268                                      ASTATEA                      EL632
02269                                      AACCTA                       EL632
02270                                      AEFFDTEA                     EL632
02271                                      ACERTA                       EL632
02272                                      ACRTSFXA                     EL632
02273                                      ACLAIMA                      EL632
02274                                      ACHECKA                      EL632
02275      ELSE                                                         EL632
02276          MOVE AL-SANON           TO  ACARIERA                     EL632
02277                                      AGROUPA                      EL632
02278                                      ASTATEA                      EL632
02279                                      AACCTA                       EL632
02280                                      AEFFDTEA                     EL632
02281                                      ACERTA                       EL632
02282                                      ACRTSFXA                     EL632
02283                                      ACLAIMA                      EL632
02284                                      ACHECKA.                     EL632
02285                                                                   EL632
02286      MOVE PC-CLAIM-TYPE          TO  ACTYPEO.                     EL632
02287      MOVE PC-CLAIM-PAYMENT       TO  APAYMNTO.                    EL632
02288      MOVE PC-PAYMENT-TYPE        TO  APTYPEO.                     EL632
02289                                                                   EL632
02290      IF PC-INCURRED-DT NOT = LOW-VALUES                           EL632
02291          MOVE PC-INCURRED-DT      TO  DC-BIN-DATE-1               EL632
02292          MOVE SPACE               TO  DC-OPTION-CODE              EL632
02293          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT               EL632
02294          MOVE DC-GREG-DATE-1-MDY  TO  AINCURO.                    EL632
02295                                                                   EL632
02296      IF PC-REPORTED-DT NOT = LOW-VALUES                           EL632
02297          MOVE PC-REPORTED-DT      TO  DC-BIN-DATE-1               EL632
02298          MOVE SPACE               TO  DC-OPTION-CODE              EL632
02299          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT               EL632
02300          MOVE DC-GREG-DATE-1-MDY  TO  AREPORTO.                   EL632
02301                                                                   EL632
02302      IF PC-PAYMENT-DT NOT = LOW-VALUES                            EL632
02303          MOVE PC-PAYMENT-DT       TO  DC-BIN-DATE-1               EL632
02304          MOVE SPACE               TO  DC-OPTION-CODE              EL632
02305          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT               EL632
02306          MOVE DC-GREG-DATE-1-MDY  TO  APAIDO.                     EL632
02307                                                                   EL632
02308      IF PC-PAID-THRU-DT NOT = LOW-VALUES                          EL632
02309         IF NOT PI-USES-PAID-TO                                       CL**3
02310            MOVE PC-PAID-THRU-DT     TO  DC-BIN-DATE-1                CL**3
02311            MOVE SPACE               TO  DC-OPTION-CODE               CL**3
02312            PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT                CL**3
02313            MOVE DC-GREG-DATE-1-MDY  TO  APTHRUO                      CL**3
02314         ELSE                                                         CL**3
02315            MOVE PC-PAID-THRU-DT     TO  DC-BIN-DATE-1                CL**3
02316            MOVE '6'                 TO  DC-OPTION-CODE               CL**3
02317            MOVE +1                  TO  DC-ELAPSED-DAYS              CL**3
02318            MOVE +0                  TO  DC-ELAPSED-MONTHS            CL**3
02319            PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT                CL**3
02320            MOVE DC-GREG-DATE-1-MDY  TO  APTHRUO.                     CL**3
02321                                                                   EL632
02322      MOVE AL-UANON               TO  AFORCEA.                     EL632
02323      MOVE PC-FORCE-CODE          TO  AFORCEO.                     EL632
02324      MOVE PC-NO-OF-DAYS-PAID     TO  ADAYSO.                      EL632
02325                                                                   EL632
02326      IF PC-AGE-AT-CLAIM NOT = ZEROS                               EL632
02327          MOVE PC-AGE-AT-CLAIM    TO  AAGEO.                       EL632
02328                                                                   EL632
02329      IF PC-CAUSE-CODE NOT = SPACES                                EL632
02330          MOVE PC-CAUSE-CODE      TO  ACAUSEO.                     EL632
02331                                                                   EL632
02332      MOVE PC-REMAINING-BENEFIT   TO  AREMBENO.                    EL632
02333      MOVE PC-REMAINING-TERM      TO  AREMTRMO.                    EL632
02334                                                                   EL632
02335      IF PC-CREDIT-SELECT-DT NOT = SPACES AND  ZEROS               EL632
02336        AND  LOW-VALUES                                            EL632
02337         MOVE PC-CREDIT-SELECT-DT  TO  DC-BIN-DATE-1               EL632
02338         MOVE SPACES               TO  DC-OPTION-CODE              EL632
02339         PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT                EL632
02340         MOVE DC-GREG-DATE-1-MDY   TO  AEOMDTO                     EL632
02341         INSPECT AEOMDTO CONVERTING SPACES TO '/'.                    CL**8
02342                                                                   EL632
02343      GO TO 6900-EXIT.                                             EL632
02344                                                                   EL632
02345  6500-FORMAT-RESERVE-DATA.                                        EL632
02346      MOVE EL632B                 TO  PI-MAP-NAME.                 EL632
02347      MOVE PC-ACCOUNT             TO  BACCTO.                      EL632
02348                                                                   EL632
02349      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL632
02350          MOVE PC-CARRIER            TO  BCARIERO                  EL632
02351          MOVE PC-GROUPING           TO  BGROUPO                   EL632
02352          MOVE PC-STATE              TO  BSTATEO                   EL632
02353      ELSE                                                         EL632
02354          IF CARR-ST-ACCNT-CNTL                                    EL632
02355              MOVE PC-CARRIER        TO  BCARIERO                  EL632
02356              MOVE PC-STATE          TO  BSTATEO                   EL632
02357          ELSE                                                     EL632
02358              IF CARR-ACCNT-CNTL                                   EL632
02359                  MOVE PC-CARRIER    TO  BCARIERO                  EL632
02360              ELSE                                                 EL632
02361                  IF ST-ACCNT-CNTL                                 EL632
02362                      MOVE PC-STATE  TO  BSTATEO.                  EL632
02363                                                                   EL632
02364      MOVE PC-CERT-EFF-DT         TO  DC-BIN-DATE-1.               EL632
02365      MOVE SPACE                  TO  DC-OPTION-CODE.              EL632
02366                                                                   EL632
02367      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                  EL632
02368                                                                   EL632
02369      MOVE DC-GREG-DATE-1-MDY     TO  BEFFDTEO.                    EL632
02370      MOVE PC-CERT-PRIME          TO  BCERTI.                      EL632
02371                                                                   EL632
02372      IF PC-CERT-SFX NOT = SPACES                                  EL632
02373          MOVE PC-CERT-SFX        TO  BCRTSFXO.                    EL632
02374                                                                   EL632
02375      MOVE PC-CLAIM-NO            TO  BCLAIMO.                     EL632
02376                                                                   EL632
02377      IF WS-MAINT = 'S'                                            EL632
02378          MOVE AL-UANON           TO  BCARIERA                     EL632
02379                                      BGROUPA                      EL632
02380                                      BSTATEA                      EL632
02381                                      BACCTA                       EL632
02382                                      BEFFDTEA                     EL632
02383                                      BCERTA                       EL632
02384                                      BCRTSFXA                     EL632
02385                                      BCLAIMA                      EL632
02386      ELSE                                                         EL632
02387          MOVE AL-SANON           TO  BCARIERA                     EL632
02388                                      BGROUPA                      EL632
02389                                      BSTATEA                      EL632
02390                                      BACCTA                       EL632
02391                                      BEFFDTEA                     EL632
02392                                      BCERTA                       EL632
02393                                      BCRTSFXA                     EL632
02394                                      BCLAIMA.                     EL632
02395                                                                   EL632
02396      IF PC-FUTURE-RESERVE-AMT NOT = ZEROS                         EL632
02397          MOVE PC-FUTURE-RESERVE-AMT  TO  BFUTUREO.                EL632
02398                                                                   EL632
02399      IF PC-IBNR-RESERVE-AMT NOT = ZEROS                           EL632
02400          MOVE PC-IBNR-RESERVE-AMT  TO  BIBNRO.                    EL632
02401                                                                   EL632
02402      IF PC-PTC-RESERVE-AMT NOT = ZEROS                            EL632
02403          MOVE PC-PTC-RESERVE-AMT  TO  BPAYCURO.                   EL632
02404                                                                   EL632
02405      IF PC-MANUAL-RESERVE-AMT NOT = ZEROS                         EL632
02406          MOVE PC-MANUAL-RESERVE-AMT  TO  BMANUALO.                EL632
02407                                                                   EL632
02408      IF PC-FORCE-CODE NOT = SPACES                                EL632
02409          MOVE PC-FORCE-CODE      TO  BFORCEO.                     EL632
02410                                                                   EL632
02411      IF PC-CREDIT-SELECT-DT NOT = SPACES AND  ZEROS               EL632
02412        AND  LOW-VALUES                                            EL632
02413         MOVE PC-CREDIT-SELECT-DT  TO  DC-BIN-DATE-1               EL632
02414         MOVE SPACES               TO  DC-OPTION-CODE              EL632
02415         PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT                EL632
02416         MOVE DC-GREG-DATE-1-MDY   TO  BEOMDTO                     EL632
02417         INSPECT BEOMDTO CONVERTING SPACES TO '/'.                    CL**8
02418                                                                   EL632
02419  6900-EXIT.                                                       EL632
02420      EXIT.                                                        EL632
02421  EJECT                                                            EL632
02422  7000-FORMAT-ERRORS.                                              EL632
02423      IF PC-ERROR-FLAGS = SPACES                                   EL632
02424          GO TO 7090-EXIT.                                         EL632
02425                                                                   EL632
02426      MOVE ER-2800                TO  WS-ERR-CODE.                 EL632
02427      MOVE 1                      TO  SUB.                         EL632
02428                                                                   EL632
02429  7010-ERR-LOOP.                                                   EL632
02430      IF PC-ERR-FLAG (SUB) NOT = SPACES                            EL632
02431          MOVE SUB                TO  WS-ERROR-SUB                 EL632
02432          MOVE WS-ERR-CODE        TO  EMI-ERROR                    EL632
02433          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL632
02434          PERFORM 7200-SET-ATTRBS  THRU  7290-EXIT.                EL632
02435                                                                   EL632
02436      ADD 1                       TO  SUB.                         EL632
02437                                                                   EL632
02438      IF SUB  IS LESS THAN  101                                    EL632
02439          GO TO 7010-ERR-LOOP.                                     EL632
02440                                                                   EL632
02441  7050-SET-ERROR-FLAGS.                                            EL632
02442      IF EMI-FATAL-CTR NOT = ZEROS                                 EL632
02443          MOVE 'X'                TO  PC-FATAL-FLAG.               EL632
02444                                                                   EL632
02445      IF EMI-FORCABLE-CTR NOT = ZEROS                              EL632
02446          IF PC-CLAIM-FORCE                                        EL632
02447            AND  EMI-FATAL-CTR = ZEROS                             EL632
02448              MOVE 'F'            TO  PC-FORCE-ER-CD               EL632
02449              MOVE ER-2600        TO  EMI-ERROR                    EL632
02450              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL632
02451          ELSE                                                     EL632
02452              MOVE 'X'            TO  PC-FORCE-ER-CD.              EL632
02453                                                                   EL632
02454      IF EMI-WARNING-CTR NOT = ZEROS                               EL632
02455          MOVE 'W'                TO  PC-WARN-ER-CD.               EL632
02456                                                                   EL632
02457  7090-EXIT.                                                       EL632
02458      EXIT.                                                        EL632
02459                                                                   EL632
02460  7200-SET-ATTRBS.                                                 EL632
02461 **2801**                                                          EL632
02462      IF SUB = 1                                                   EL632
02463          MOVE AL-UNBON           TO  APAYMNTA                     EL632
02464          MOVE -1                 TO  APAYMNTL                     EL632
02465      ELSE                                                         EL632
02466                                                                   EL632
02467 **2802**                                                          EL632
02468      IF SUB = 2                                                   EL632
02469          MOVE AL-UNBON           TO  AINCURA                      EL632
02470          MOVE -1                 TO  AINCURL                      EL632
02471      ELSE                                                         EL632
02472                                                                   EL632
02473 **2803**                                                          EL632
02474      IF SUB = 3                                                   EL632
02475          MOVE AL-UNBON           TO  APAIDA                       EL632
02476          MOVE -1                 TO  APAIDL                       EL632
02477      ELSE                                                         EL632
02478                                                                   EL632
02479 **2804**                                                          EL632
02480      IF SUB = 4                                                   EL632
02481          MOVE AL-UNBON           TO  AREPORTA                     EL632
02482          MOVE -1                 TO  AREPORTL                     EL632
02483      ELSE                                                         EL632
02484                                                                   EL632
02485 **2805**                                                          EL632
02486      IF SUB = 5                                                   EL632
02487          MOVE AL-UNBON           TO  APTHRUA                      EL632
02488          MOVE -1                 TO  APTHRUL                      EL632
02489      ELSE                                                         EL632
02490                                                                   EL632
02491 **2806**                                                          EL632
02492      IF SUB = 6                                                   EL632
02493          MOVE AL-UNBON           TO  AINCURA                      EL632
02494          MOVE -1                 TO  AINCURL                      EL632
02495      ELSE                                                         EL632
02496                                                                   EL632
02497 **2807**                                                          EL632
02498      IF SUB = 7                                                   EL632
02499          MOVE AL-UNBON           TO  APAIDA                       EL632
02500          MOVE -1                 TO  APAIDL                       EL632
02501      ELSE                                                         EL632
02502                                                                   EL632
02503 **2808**                                                          EL632
02504      IF SUB = 8                                                   EL632
02505          MOVE AL-UNBON           TO  APAIDA                       EL632
02506          MOVE -1                 TO  APAIDL                       EL632
02507      ELSE                                                         EL632
02508                                                                   EL632
02509 **2809**                                                          EL632
02510      IF SUB = 9                                                   EL632
02511          MOVE AL-UNBON           TO  AREPORTA                     EL632
02512          MOVE -1                 TO  AREPORTL                     EL632
02513      ELSE                                                         EL632
02514                                                                   EL632
02515 **2810**                                                          EL632
02516      IF SUB = 10                                                  EL632
02517          MOVE AL-UNBON           TO  AREPORTA                     EL632
02518          MOVE -1                 TO  AREPORTL                     EL632
02519      ELSE                                                         EL632
02520                                                                   EL632
02521 **2811**                                                          EL632
02522      IF SUB = 11                                                  EL632
02523          MOVE AL-UNBON           TO  APTHRUA                      EL632
02524          MOVE -1                 TO  APTHRUL                      EL632
02525      ELSE                                                         EL632
02526                                                                   EL632
02527 **2812**                                                          EL632
02528      IF SUB = 12                                                  EL632
02529          MOVE AL-UNBON           TO  ADAYSA                       EL632
02530          MOVE -1                 TO  ADAYSL                       EL632
02531      ELSE                                                         EL632
02532                                                                   EL632
02533 **2813**                                                          EL632
02534      IF SUB = 13                                                  EL632
02535          MOVE AL-UNBON           TO  BIBNRA                       EL632
02536          MOVE -1                 TO  BIBNRL                       EL632
02537      ELSE                                                         EL632
02538                                                                   EL632
02539 **2814**                                                          EL632
02540      IF SUB = 14                                                  EL632
02541          MOVE AL-UNBON           TO  BPAYCURA                     EL632
02542          MOVE -1                 TO  BPAYCURL                     EL632
02543      ELSE                                                         EL632
02544                                                                   EL632
02545 **2815**                                                          EL632
02546      IF SUB = 15                                                  EL632
02547          MOVE AL-UNBON           TO  BFUTUREA                     EL632
02548          MOVE -1                 TO  BFUTUREL                     EL632
02549      ELSE                                                         EL632
02550                                                                   EL632
02551 **2816**                                                          EL632
02552 *    IF SUB = 16                                                  EL632
02553 *        MOVE AL-UNBON           TO                               EL632
02554 *        MOVE -1                 TO                               EL632
02555 *    ELSE                                                         EL632
02556                                                                   EL632
02557 **2817**                                                          EL632
02558      IF SUB = 17                                                  EL632
02559          MOVE AL-UNBON           TO  ACTYPEA                      EL632
02560          MOVE -1                 TO  ACTYPEL                      EL632
02561      ELSE                                                         EL632
02562                                                                   EL632
02563 **2818**                                                          EL632
02564      IF SUB = 18                                                  EL632
02565          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02566          MOVE -1                 TO  ACLAIML                      EL632
02567      ELSE                                                         EL632
02568                                                                   EL632
02569 **2819**                                                          EL632
02570      IF SUB = 19                                                  EL632
02571          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02572          MOVE -1                 TO  ACLAIML                      EL632
02573      ELSE                                                         EL632
02574                                                                   EL632
02575 **2820**                                                          EL632
02576      IF SUB = 20                                                  EL632
02577          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02578          MOVE -1                 TO  ACLAIML                      EL632
02579      ELSE                                                         EL632
02580                                                                   EL632
02581 **2821**                                                          EL632
02582      IF SUB = 21                                                  EL632
02583          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02584          MOVE -1                 TO  ACLAIML                      EL632
02585      ELSE                                                         EL632
02586                                                                   EL632
02587 **2822**                                                          EL632
02588      IF SUB = 22                                                  EL632
02589          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02590          MOVE -1                 TO  ACLAIML                      EL632
02591      ELSE                                                         EL632
02592                                                                   EL632
02593 **2823**                                                          EL632
02594      IF SUB = 23                                                  EL632
02595          MOVE AL-UNBON           TO  APAYMNTA                     EL632
02596          MOVE -1                 TO  APAYMNTL                     EL632
02597      ELSE                                                         EL632
02598                                                                   EL632
02599 **2824**                                                          EL632
02600      IF SUB = 24                                                  EL632
02601          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02602          MOVE -1                 TO  ACLAIML                      EL632
02603      ELSE                                                         EL632
02604                                                                   EL632
02605 **2825**                                                          EL632
02606      IF SUB = 25                                                  EL632
02607          MOVE AL-UNBON           TO  ACTYPEA                      EL632
02608          MOVE -1                 TO  ACTYPEL                      EL632
02609      ELSE                                                         EL632
02610                                                                   EL632
02611 **2826**                                                          EL632
02612      IF SUB = 26                                                  EL632
02613          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02614          MOVE -1                 TO  ACLAIML                      EL632
02615      ELSE                                                         EL632
02616                                                                   EL632
02617 **2827**                                                          EL632
02618      IF SUB = 27                                                  EL632
02619          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02620          MOVE -1                 TO  ACLAIML                      EL632
02621      ELSE                                                         EL632
02622                                                                   EL632
02623 **2828**                                                          EL632
02624      IF SUB = 28                                                  EL632
02625          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02626          MOVE -1                 TO  ACLAIML                      EL632
02627      ELSE                                                         EL632
02628                                                                   EL632
02629 **2829**                                                          EL632
02630      IF SUB = 29                                                  EL632
02631          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02632          MOVE -1                 TO  ACLAIML                      EL632
02633      ELSE                                                         EL632
02634                                                                   EL632
02635 **2830**                                                          EL632
02636      IF SUB = 30                                                  EL632
02637          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02638          MOVE -1                 TO  ACLAIML                      EL632
02639      ELSE                                                         EL632
02640                                                                   EL632
02641 **2831**                                                          EL632
02642      IF SUB = 31                                                  EL632
02643          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02644          MOVE -1                 TO  ACLAIML                      EL632
02645      ELSE                                                         EL632
02646                                                                   EL632
02647 **2832**                                                          EL632
02648      IF SUB = 32                                                  EL632
02649          MOVE AL-UNBON           TO  ACLAIMA                      EL632
02650          MOVE -1                 TO  ACLAIML                      EL632
02651      ELSE                                                         EL632
02652                                                                   EL632
02653 **2833**                                                          EL632
02654      IF SUB = 33                                                  EL632
02655          IF PI-MAP-NAME = EL632A                                  EL632
02656              MOVE AL-SABOF       TO  ACERTA                       EL632
02657              MOVE -1             TO  ACERTL                       EL632
02658          ELSE                                                     EL632
02659              MOVE AL-SABOF       TO  BCERTA                       EL632
02660              MOVE -1             TO  BCERTL                       EL632
02661      ELSE                                                         EL632
02662                                                                   EL632
02663 **2834**                                                          EL632
02664      IF SUB = 34                                                  EL632
02665          IF PI-MAP-NAME = EL632A                                  EL632
02666              MOVE AL-SABOF       TO  ACERTA                       EL632
02667              MOVE -1             TO  ACERTL                       EL632
02668          ELSE                                                     EL632
02669              MOVE AL-SABOF       TO  BCERTA                       EL632
02670              MOVE -1             TO  BCERTL                       EL632
02671      ELSE                                                         EL632
02672                                                                   EL632
02673 **2835**                                                          EL632
02674      IF SUB = 35                                                  EL632
02675          MOVE AL-UNBON           TO  APTHRUA                      EL632
02676          MOVE -1                 TO  APTHRUL                      EL632
02677      ELSE                                                         EL632
02678                                                                   EL632
02679 **2836**                                                          EL632
02680      IF SUB = 36                                                  EL632
02681          MOVE AL-UNBON           TO  APTHRUA                      EL632
02682          MOVE -1                 TO  APTHRUL                      EL632
02683      ELSE                                                         EL632
02684                                                                   EL632
02685 **2837**                                                          EL632
02686      IF SUB = 37                                                  EL632
02687          MOVE AL-UNBON           TO  APTHRUA                      EL632
02688          MOVE -1                 TO  APTHRUL                      EL632
02689      ELSE                                                         EL632
02690                                                                   EL632
02691 **2838**                                                          EL632
02692      IF SUB = 38                                                  EL632
02693          MOVE -1                 TO  AMAINTL                      EL632
02694      ELSE                                                         EL632
02695                                                                   EL632
02696 **2839**                                                          EL632
02697      IF SUB = 39                                                  EL632
02698          MOVE -1                 TO  AMAINTL                      EL632
02699      ELSE                                                         EL632
02700                                                                   EL632
02701 **2840**                                                          EL632
02702      IF SUB = 40                                                  EL632
02703          IF PI-MAP-NAME = EL632A                                  EL632
02704              MOVE -1             TO  AMAINTL                      EL632
02705          ELSE                                                     EL632
02706              MOVE -1             TO  BMAINTL                      EL632
02707      ELSE                                                         EL632
02708                                                                   EL632
02709 **2841**                                                          EL632
02710      IF SUB = 41                                                  EL632
02711          IF PI-MAP-NAME = EL632A                                  EL632
02712              MOVE -1             TO  AMAINTL                      EL632
02713          ELSE                                                     EL632
02714              MOVE -1             TO  BMAINTL                      EL632
02715      ELSE                                                         EL632
02716                                                                   EL632
02717 **2842**                                                          EL632
02718      IF SUB = 42                                                  EL632
02719          IF PI-MAP-NAME = EL632A                                  EL632
02720              MOVE -1             TO  AMAINTL                      EL632
02721          ELSE                                                     EL632
02722              MOVE -1             TO  BMAINTL                      EL632
02723      ELSE                                                         EL632
02724                                                                   EL632
02725 **2843**                                                          EL632
02726      IF SUB = 43                                                  EL632
02727          IF PI-MAP-NAME = EL632A                                  EL632
02728              MOVE -1             TO  AMAINTL                      EL632
02729          ELSE                                                     EL632
02730              MOVE -1             TO  BMAINTL                      EL632
02731      ELSE                                                         EL632
02732                                                                   EL632
02733 **2844**                                                          EL632
02734      IF SUB = 44                                                  EL632
02735          IF PI-MAP-NAME = EL632A                                  EL632
02736              MOVE AL-UABON       TO  ACARIERA                     EL632
02737              MOVE -1             TO  ACARIERL                     EL632
02738          ELSE                                                     EL632
02739              MOVE AL-UABON       TO  BCARIERA                     EL632
02740              MOVE -1             TO  BCARIERL                     EL632
02741      ELSE                                                         EL632
02742                                                                   EL632
02743 **2845**                                                          EL632
02744      IF SUB = 45                                                  EL632
02745          IF PI-MAP-NAME = EL632A                                  EL632
02746              MOVE AL-UABON       TO  ACARIERA                     EL632
02747              MOVE -1             TO  ACARIERL                     EL632
02748          ELSE                                                     EL632
02749              MOVE AL-UABON       TO  BCARIERA                     EL632
02750              MOVE -1             TO  BCARIERL                     EL632
02751      ELSE                                                         EL632
02752                                                                   EL632
02753 **2846**                                                          EL632
02754      IF SUB = 46                                                  EL632
02755          IF PI-MAP-NAME = EL632A                                  EL632
02756              MOVE AL-UABON       TO  AGROUPA                      EL632
02757              MOVE -1             TO  AGROUPL                      EL632
02758          ELSE                                                     EL632
02759              MOVE AL-UABON       TO  BGROUPA                      EL632
02760              MOVE -1             TO  BGROUPL                      EL632
02761      ELSE                                                         EL632
02762                                                                   EL632
02763 **2847**                                                          EL632
02764      IF SUB = 47                                                  EL632
02765          IF PI-MAP-NAME = EL632A                                  EL632
02766              MOVE AL-UABON       TO  ASTATEA                      EL632
02767              MOVE -1             TO  ASTATEL                      EL632
02768          ELSE                                                     EL632
02769              MOVE AL-UABON       TO  BSTATEA                      EL632
02770              MOVE -1             TO  BSTATEL                      EL632
02771      ELSE                                                         EL632
02772                                                                   EL632
02773 **2848**                                                          EL632
02774      IF SUB = 48                                                  EL632
02775          IF PI-MAP-NAME = EL632A                                  EL632
02776              MOVE AL-UABON       TO  ASTATEA                      EL632
02777              MOVE -1             TO  ASTATEL                      EL632
02778          ELSE                                                     EL632
02779              MOVE AL-UABON       TO  BSTATEA                      EL632
02780              MOVE -1             TO  BSTATEL                      EL632
02781      ELSE                                                         EL632
02782                                                                   EL632
02783 **2849**                                                          EL632
02784      IF SUB = 49                                                  EL632
02785          IF PI-MAP-NAME = EL632A                                  EL632
02786              MOVE AL-UABON       TO  AACCTA                       EL632
02787              MOVE -1             TO  AACCTL                       EL632
02788          ELSE                                                     EL632
02789              MOVE AL-UABON       TO  BACCTA                       EL632
02790              MOVE -1             TO  BACCTL                       EL632
02791      ELSE                                                         EL632
02792                                                                   EL632
02793 **2850**                                                          EL632
02794      IF SUB = 50                                                  EL632
02795          IF PI-MAP-NAME = EL632A                                  EL632
02796              MOVE AL-UABON       TO  AACCTA                       EL632
02797              MOVE -1             TO  AACCTL                       EL632
02798          ELSE                                                     EL632
02799              MOVE AL-UABON       TO  BACCTA                       EL632
02800              MOVE -1             TO  BACCTL                       EL632
02801      ELSE                                                         EL632
02802                                                                   EL632
02803 **2851**                                                          EL632
02804      IF SUB = 51                                                  EL632
02805          IF PI-MAP-NAME = EL632A                                  EL632
02806              MOVE AL-UABON       TO  AACCTA                       EL632
02807              MOVE -1             TO  AACCTL                       EL632
02808          ELSE                                                     EL632
02809              MOVE AL-UABON       TO  BACCTA                       EL632
02810              MOVE -1             TO  BACCTL                       EL632
02811      ELSE                                                         EL632
02812                                                                   EL632
02813 **2852**                                                          EL632
02814      IF SUB = 52                                                  EL632
02815          IF PI-MAP-NAME = EL632A                                  EL632
02816              MOVE AL-UABON       TO  ACLAIMA                      EL632
02817              MOVE -1             TO  ACLAIML                      EL632
02818          ELSE                                                     EL632
02819              MOVE AL-UABON       TO  BCLAIMA                      EL632
02820              MOVE -1             TO  BCLAIML                      EL632
02821      ELSE                                                         EL632
02822                                                                   EL632
02823 **2853**                                                          EL632
02824      IF SUB = 53                                                  EL632
02825          MOVE AL-UABON           TO  APTYPEA                      EL632
02826          MOVE -1                 TO  APTYPEL                      EL632
02827      ELSE                                                         EL632
02828                                                                   EL632
02829 **2854**                                                          EL632
02830      IF SUB = 54                                                  EL632
02831          MOVE AL-UNBON           TO  BMANUALA                     EL632
02832          MOVE -1                 TO  BMANUALL                     EL632
02833      ELSE                                                         EL632
02834                                                                   EL632
02835 **2855**                                                          EL632
02836      IF SUB = 55                                                  EL632
02837          MOVE AL-UABON           TO  APTYPEA                      EL632
02838          MOVE -1                 TO  APTYPEL                      EL632
02839      ELSE                                                         EL632
02840                                                                   EL632
02841 **2856**                                                          EL632
02842      IF SUB = 56                                                  EL632
02843          MOVE AL-UABON           TO  APTYPEA                      EL632
02844          MOVE -1                 TO  APTYPEL                      EL632
02845      ELSE                                                         EL632
02846                                                                   EL632
02847 **2857**                                                          EL632
02848      IF SUB = 57                                                  EL632
02849          MOVE AL-UABON           TO  ACTYPEA                      EL632
02850          MOVE -1                 TO  ACTYPEL                      EL632
02851      ELSE                                                         EL632
02852                                                                   EL632
02853 **2858**                                                          EL632
02854      IF SUB = 58                                                  EL632
02855          MOVE AL-UABON           TO  ACTYPEA                      EL632
02856          MOVE -1                 TO  ACTYPEL                      EL632
02857      ELSE                                                         EL632
02858                                                                   EL632
02859 **2859**                                                          EL632
02860      IF SUB = 59                                                  EL632
02861          MOVE -1                 TO  AMAINTL                      EL632
02862      ELSE                                                         EL632
02863                                                                   EL632
02864 **2860**                                                          EL632
02865      IF SUB = 60                                                  EL632
02866          MOVE AL-UABON           TO  APTYPEA                      EL632
02867          MOVE -1                 TO  APTYPEL                      EL632
02868      ELSE                                                         EL632
02869                                                                   EL632
02870 **2861**                                                          EL632
02871      IF SUB = 61                                                  EL632
02872          MOVE AL-UNBON           TO  APAYMNTA                     EL632
02873          MOVE -1                 TO  APAYMNTL                     EL632
02874      ELSE                                                         EL632
02875                                                                   EL632
02876 **2862**                                                          EL632
02877      IF SUB = 62                                                  EL632
02878          MOVE -1                 TO  AMAINTL                      EL632
02879      ELSE                                                         EL632
02880                                                                   EL632
02881 **2863**                                                          EL632
02882      IF SUB = 63                                                  EL632
02883          MOVE -1                 TO  AMAINTL                      EL632
02884      ELSE                                                         EL632
02885                                                                   EL632
02886 **2864**                                                          EL632
02887      IF SUB = 64                                                  EL632
02888          MOVE AL-UABON           TO  APTYPEA                      EL632
02889          MOVE -1                 TO  APTYPEL                      EL632
02890      ELSE                                                         EL632
02891                                                                   EL632
02892 **2865**                                                          EL632
02893      IF SUB = 65                                                  EL632
02894          MOVE AL-UNBON           TO  APAYMNTA                     EL632
02895          MOVE -1                 TO  APAYMNTL                     EL632
02896      ELSE                                                         EL632
02897                                                                   EL632
02898 **2866**                                                          EL632
02899      IF SUB = 66                                                  EL632
02900          MOVE AL-UNBON           TO  APAYMNTA                     EL632
02901          MOVE -1                 TO  APAYMNTL                     EL632
02902      ELSE                                                         EL632
02903                                                                   EL632
02904 **2867**                                                          EL632
02905      IF SUB = 67                                                  EL632
02906          MOVE -1                 TO  AMAINTL                      EL632
02907      ELSE                                                         EL632
02908                                                                   EL632
02909 **2868**                                                          EL632
02910      IF SUB = 68                                                  EL632
02911          MOVE AL-UNBON           TO  APAIDA                       EL632
02912          MOVE -1                 TO  APAIDL.                      EL632
02913                                                                   EL632
02914  7290-EXIT.                                                       EL632
02915      EXIT.                                                        EL632
02916  EJECT                                                            EL632
02917  8100-SEND-INITIAL-MAP.                                           EL632
02918      IF PI-MAP-NAME = EL632A                                      EL632
02919          NEXT SENTENCE                                            EL632
02920      ELSE                                                         EL632
02921          GO TO 8110-SEND-INITIAL-MAPB.                            EL632
02922                                                                   EL632
02923      MOVE WS-CURRENT-DT          TO  ADATEO.                      EL632
02924      MOVE EIBTIME                TO  TIME-IN.                     EL632
02925      MOVE TIME-OUT               TO  ATIMEO.                      EL632
02926      MOVE PI-LIFE-OVERRIDE-L6    TO  WS-ACLM-TYP-1                EL632
02927                                      WS-ACLM-TYP-3.               EL632
02928      MOVE PI-AH-OVERRIDE-L6      TO  WS-ACLM-TYP-2                EL632
02929                                      WS-ACLM-TYP-4.               EL632
02930      MOVE WS-ACLMTP1             TO  ACLMTP1O.                    EL632
02931      MOVE WS-ACLMTP2             TO  ACLMTP2O.                    EL632
02932      MOVE -1                     TO  AMAINTL.                     EL632
02933      MOVE EMI-MESSAGE-AREA (1)   TO  AERMSG1O.                    EL632
02934      MOVE EMI-MESSAGE-AREA (2)   TO  AERMSG2O.                    EL632
02935      MOVE EMI-MESSAGE-AREA (3)   TO  AERMSG3O.                    EL632
02936                                                                   EL632
02937      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL632
02938          NEXT SENTENCE                                            EL632
02939      ELSE                                                         EL632
02940          IF ST-ACCNT-CNTL                                         EL632
02941              MOVE AL-SADOF              TO  ACARHDGA              EL632
02942                                             AGRPHDGA              EL632
02943              MOVE AL-SANOF              TO  ACARIERA              EL632
02944                                             AGROUPA               EL632
02945          ELSE                                                     EL632
02946              IF CARR-ST-ACCNT-CNTL                                EL632
02947                  MOVE AL-SADOF          TO  AGRPHDGA              EL632
02948                  MOVE AL-SANOF          TO  AGROUPA               EL632
02949              ELSE                                                 EL632
02950                  IF ACCNT-CNTL                                    EL632
02951                      MOVE AL-SADOF      TO  ACARHDGA              EL632
02952                                             AGRPHDGA              EL632
02953                                             ASTHDGA               EL632
02954                      MOVE AL-SANOF      TO  ACARIERA              EL632
02955                                             AGROUPA               EL632
02956                                             ASTATEA               EL632
02957                  ELSE                                             EL632
02958                      IF CARR-ACCNT-CNTL                           EL632
02959                          MOVE AL-SADOF  TO  AGRPHDGA              EL632
02960                                             ASTHDGA               EL632
02961                          MOVE AL-SANOF  TO  AGROUPA               EL632
02962                                             ASTATEA.              EL632
02963                                                                   EL632
02964      IF PI-USES-PAID-TO                                              CL**3
02965         MOVE EL632A-HEADING   TO AHDINGO.                            CL**3
02966                                                                      CL**3
02967      EXEC CICS SEND                                               EL632
02968          MAP     (PI-MAP-NAME)                                    EL632
02969          MAPSET  (MAPSET-NAME)                                    EL632
02970          FROM    (EL632AO)                                        EL632
02971          ERASE                                                    EL632
02972          CURSOR                                                   EL632
02973      END-EXEC.                                                    EL632
02974                                                                   EL632
02975      GO TO 9100-RETURN-TRAN.                                      EL632
02976  EJECT                                                            EL632
02977  8110-SEND-INITIAL-MAPB.                                          EL632
02978      MOVE WS-CURRENT-DT          TO  BDATEO.                      EL632
02979      MOVE EIBTIME                TO  TIME-IN.                     EL632
02980      MOVE TIME-OUT               TO  BTIMEO.                      EL632
02981      MOVE -1                     TO  BMAINTL.                     EL632
02982      MOVE EMI-MESSAGE-AREA (1)   TO  BERMSG1O.                    EL632
02983      MOVE EMI-MESSAGE-AREA (2)   TO  BERMSG2O.                    EL632
02984      MOVE EMI-MESSAGE-AREA (3)   TO  BERMSG3O.                    EL632
02985                                                                   EL632
02986      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL632
02987          NEXT SENTENCE                                            EL632
02988      ELSE                                                         EL632
02989          IF ST-ACCNT-CNTL                                         EL632
02990              MOVE AL-SADOF              TO  BCARHDGA              EL632
02991                                             BGRPHDGA              EL632
02992              MOVE AL-SANOF              TO  BCARIERA              EL632
02993                                             BGROUPA               EL632
02994          ELSE                                                     EL632
02995              IF CARR-ST-ACCNT-CNTL                                EL632
02996                  MOVE AL-SADOF          TO  BGRPHDGA              EL632
02997                  MOVE AL-SANOF          TO  BGROUPA               EL632
02998              ELSE                                                 EL632
02999                  IF ACCNT-CNTL                                    EL632
03000                      MOVE AL-SADOF      TO  BCARHDGA              EL632
03001                                             BGRPHDGA              EL632
03002                                             BSTHDGA               EL632
03003                      MOVE AL-SANOF      TO  BCARIERA              EL632
03004                                             BGROUPA               EL632
03005                                             BSTATEA               EL632
03006                  ELSE                                             EL632
03007                      IF CARR-ACCNT-CNTL                           EL632
03008                          MOVE AL-SADOF  TO  BGRPHDGA              EL632
03009                                             BSTHDGA               EL632
03010                          MOVE AL-SANOF  TO  BGROUPA               EL632
03011                                             BSTATEA.              EL632
03012                                                                   EL632
03013      EXEC CICS SEND                                               EL632
03014          MAP     (PI-MAP-NAME)                                    EL632
03015          MAPSET  (MAPSET-NAME)                                    EL632
03016          FROM    (EL632BO)                                        EL632
03017          ERASE                                                    EL632
03018          CURSOR                                                   EL632
03019      END-EXEC.                                                    EL632
03020                                                                   EL632
03021      GO TO 9100-RETURN-TRAN.                                      EL632
03022  EJECT                                                            EL632
03023  8200-SEND-DATAONLY.                                              EL632
03024      IF EIBAID = DFHPF11                                          EL632
03025          MOVE 'Y'                TO  EMI-ROLL-SWITCH              EL632
03026          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL632
03027                                                                      CL**3
03028      IF PI-MAP-NAME EQUAL EL632A                                     CL**3
03029         IF PI-USES-PAID-TO                                           CL**3
03030            MOVE EL632A-HEADING   TO AHDINGO.                         CL**3
03031                                                                   EL632
03032      IF PI-MAP-NAME = EL632A                                      EL632
03033          MOVE WS-CURRENT-DT         TO  ADATEO                    EL632
03034          MOVE EIBTIME               TO  TIME-IN                   EL632
03035          MOVE TIME-OUT              TO  ATIMEO                    EL632
03036          MOVE PI-LIFE-OVERRIDE-L6   TO  WS-ACLM-TYP-1             EL632
03037                                         WS-ACLM-TYP-3             EL632
03038          MOVE PI-AH-OVERRIDE-L6     TO  WS-ACLM-TYP-2             EL632
03039                                         WS-ACLM-TYP-4             EL632
03040          MOVE WS-ACLMTP1            TO  ACLMTP1O                  EL632
03041          MOVE WS-ACLMTP2            TO  ACLMTP2O                  EL632
03042          MOVE EMI-MESSAGE-AREA (1)  TO  AERMSG1O                  EL632
03043          MOVE EMI-MESSAGE-AREA (2)  TO  AERMSG2O                  EL632
03044          MOVE EMI-MESSAGE-AREA (3)  TO  AERMSG3O                  EL632
03045          EXEC CICS SEND                                           EL632
03046              MAP     (PI-MAP-NAME)                                EL632
03047              MAPSET  (MAPSET-NAME)                                EL632
03048              FROM    (EL632AO)                                    EL632
03049              DATAONLY                                             EL632
03050              ERASEAUP                                             EL632
03051              CURSOR                                               EL632
03052          END-EXEC                                                 EL632
03053      ELSE                                                         EL632
03054          MOVE WS-CURRENT-DT         TO  BDATEO                    EL632
03055          MOVE EIBTIME               TO  TIME-IN                   EL632
03056          MOVE TIME-OUT              TO  BTIMEO                    EL632
03057          MOVE EMI-MESSAGE-AREA (1)  TO  BERMSG1O                  EL632
03058          MOVE EMI-MESSAGE-AREA (2)  TO  BERMSG2O                  EL632
03059          MOVE EMI-MESSAGE-AREA (3)  TO  BERMSG3O                  EL632
03060          EXEC CICS SEND                                           EL632
03061              MAP     (PI-MAP-NAME)                                EL632
03062              MAPSET  (MAPSET-NAME)                                EL632
03063              FROM    (EL632BO)                                    EL632
03064              DATAONLY                                             EL632
03065              ERASEAUP                                             EL632
03066              CURSOR                                               EL632
03067          END-EXEC.                                                EL632
03068                                                                   EL632
03069      GO TO 9100-RETURN-TRAN.                                      EL632
03070                                                                   EL632
03071  8300-SEND-TEXT.                                                  EL632
03072      EXEC CICS SEND TEXT                                          EL632
03073          FROM    (LOGOFF-TEXT)                                    EL632
03074          LENGTH  (LOGOFF-LENGTH)                                  EL632
03075          ERASE                                                    EL632
03076          FREEKB                                                   EL632
03077      END-EXEC.                                                    EL632
03078                                                                   EL632
03079      EXEC CICS RETURN                                             EL632
03080      END-EXEC.                                                    EL632
03081                                                                   EL632
03082  8400-LOG-JOURNAL-RECORD.                                         EL632
03083      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL632
03084      MOVE ERPNDC-FILE-ID         TO  JP-FILE-ID.                  EL632
03085      MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL632
03086                                                                   EL632
03087 *    EXEC CICS JOURNAL                                            EL632
03088 *        JFILEID  (PI-JOURNAL-FILE-ID)                            EL632
03089 *        JTYPEID  ('EL')                                          EL632
03090 *        FROM     (JOURNAL-RECORD)                                EL632
03091 *        LENGTH   (523)                                           EL632
03092 *    END-EXEC.                                                    EL632
03093                                                                   EL632
03094  8500-DATE-CONVERT.                                               EL632
03095      MOVE LINK-CLDATCV           TO  PGM-NAME.                    EL632
03096                                                                   EL632
03097      EXEC CICS LINK                                               EL632
03098          PROGRAM   (PGM-NAME)                                     EL632
03099          COMMAREA  (DATE-CONVERSION-DATA)                         EL632
03100          LENGTH    (DC-COMM-LENGTH)                               EL632
03101      END-EXEC.                                                    EL632
03102                                                                   EL632
03103  8500-EXIT.                                                       EL632
03104      EXIT.                                                        EL632
03105                                                                   EL632
CIDMOD 8600-DEEDIT.                                                        CL**8
CIDMOD     EXEC CICS BIF DEEDIT                                            CL**8
CIDMOD         FIELD   (WS-DEEDIT-FIELD)                                   CL**8
CIDMOD         LENGTH  (11)                                                CL**8
CIDMOD     END-EXEC.                                                       CL**8
CIDMOD                                                                  EL632
CIDMOD 8600-EXIT.                                                          CL**8
CIDMOD     EXIT.                                                           CL**8
03114                                                                   EL632
03115  8800-UNAUTHORIZED-ACCESS.                                        EL632
03116      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL632
03117                                                                   EL632
03118      GO TO 8300-SEND-TEXT.                                        EL632
03119                                                                   EL632
03120  8810-PF23.                                                       EL632
03121      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL632
03122      MOVE XCTL-005               TO  PGM-NAME.                    EL632
03123                                                                   EL632
03124      GO TO 9300-XCTL.                                             EL632
03125                                                                   EL632
03126  9100-RETURN-TRAN.                                                EL632
03127      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL632
03128      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL632
03129                                                                   EL632
03130      EXEC CICS RETURN                                             EL632
03131          TRANSID   (TRANS-ID)                                     EL632
03132          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL632
03133          LENGTH    (PI-COMM-LENGTH)                               EL632
03134      END-EXEC.                                                    EL632
03135                                                                   EL632
03136  9200-RETURN-MAIN-MENU.                                           EL632
03137      MOVE XCTL-626               TO  PGM-NAME.                    EL632
03138                                                                   EL632
03139      GO TO 9300-XCTL.                                             EL632
03140                                                                   EL632
03141  9300-XCTL.                                                       EL632
03142      EXEC CICS XCTL                                               EL632
03143          PROGRAM   (PGM-NAME)                                     EL632
03144          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL632
03145          LENGTH    (PI-COMM-LENGTH)                               EL632
03146      END-EXEC.                                                    EL632
03147                                                                   EL632
03148  9400-CLEAR.                                                      EL632
03149      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL632
03150                                                                   EL632
03151      GO TO 9300-XCTL.                                             EL632
03152                                                                   EL632
03153  9500-PF12.                                                       EL632
03154      MOVE XCTL-010               TO  PGM-NAME.                    EL632
03155                                                                   EL632
03156      GO TO 9300-XCTL.                                             EL632
03157                                                                   EL632
03158  9600-PGMID-ERROR.                                                EL632
03159      EXEC CICS HANDLE CONDITION                                   EL632
03160          PGMIDERR  (8300-SEND-TEXT)                               EL632
03161      END-EXEC.                                                    EL632
03162                                                                   EL632
03163      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL632
03164      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL632
03165      MOVE XCTL-005               TO  PGM-NAME.                    EL632
03166      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL632
03167      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL632
03168                                                                   EL632
03169      GO TO 9300-XCTL.                                             EL632
03170                                                                   EL632
03171  9800-LINK-CLAIMS-EDIT.                                           EL632
03172      MOVE PENDING-CLAIMS         TO  PNDC-RECORD.                 EL632
03173      MOVE ZEROS                  TO  WK-PC-ACCT-ADDR              EL632
03174                                      WK-PC-STATE-ADDR.            EL632
03175      MOVE SPACES                 TO  WK-PC-CNTL-RECORD-FOUND-SW.  EL632
03176      MOVE LINK-053               TO  PGM-NAME.                    EL632
03177                                                                   EL632
03178      EXEC CICS LINK                                               EL632
03179          PROGRAM   (PGM-NAME)                                     EL632
03180          COMMAREA  (PNDC-EDIT-PASS-AREA)                          EL632
03181          LENGTH    (PNDC-EDIT-PASS-AREA-LEN)                      EL632
03182      END-EXEC.                                                    EL632
03183                                                                   EL632
03184      MOVE PNDC-RECORD            TO  PENDING-CLAIMS.              EL632
03185                                                                   EL632
03186  9900-ERROR-FORMAT.                                               EL632
03187      IF NOT EMI-ERRORS-COMPLETE                                   EL632
03188          MOVE LINK-001           TO  PGM-NAME                     EL632
03189          EXEC CICS LINK                                           EL632
03190              PROGRAM   (PGM-NAME)                                 EL632
03191              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL632
03192              LENGTH    (EMI-COMM-LENGTH)                          EL632
03193          END-EXEC.                                                EL632
03194                                                                   EL632
03195  9900-EXIT.                                                       EL632
03196      EXIT.                                                        EL632
03197                                                                   EL632
03198  9990-ABEND.                                                      EL632
03199      MOVE LINK-004               TO  PGM-NAME.                    EL632
03200      MOVE DFHEIBLK               TO  EMI-LINE1                    EL632
03201                                                                   EL632
03202      EXEC CICS LINK                                               EL632
03203          PROGRAM   (PGM-NAME)                                     EL632
03204          COMMAREA  (EMI-LINE1)                                    EL632
03205          LENGTH    (72)                                           EL632
03206      END-EXEC.                                                    EL632
03207                                                                   EL632
03208      IF PI-MAP-NAME = EL632A                                      EL632
03209          MOVE -1                 TO  AMAINTL                      EL632
03210      ELSE                                                         EL632
03211          MOVE -1                 TO  BMAINTL.                     EL632
03212                                                                   EL632
03213      GO TO 8200-SEND-DATAONLY.                                    EL632
03214                                                                   EL632
03215      GOBACK.                                                      EL632
03216                                                                   EL632
03217  9995-SECURITY-VIOLATION.                                         EL632
03218                              COPY ELCSCTP.                        EL632
03219                                                                   EL632
03220  9995-EXIT.                                                       EL632
03221      EXIT.                                                        EL632
03222                                                                   EL632
