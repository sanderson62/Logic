00001  IDENTIFICATION DIVISION.                                         03/08/96
00002                                                                   EL603
00003  PROGRAM-ID.                 EL603 .                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/12/96 16:58:36.                    CL**5
00007 *                            VMOD=2.005                              CL**5
00008 *                                                                 EL603
00008 *                                                                 EL603
00009 *AUTHOR.        LOGIC,INC.                                           CL**5
00010 *               DALLAS, TEXAS.                                       CL**5
00011                                                                   EL603
00012 *DATE-COMPILED.                                                      CL**5
00013                                                                   EL603
00014 *SECURITY.   *****************************************************   CL**5
00015 *            *                                                   *   CL**5
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
00017 *            *                                                   *   CL**5
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
00019 *                                                                *   CL**5
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
00022 *            *                                                   *   CL**5
00023 *            *****************************************************   CL**5
00024                                                                   EL603
00025 *REMARKS.                                                            CL**5
00026 *        TRANSACTION - EXA3 - BUSINESS TYPE DESCRIPTIONS.            CL**5
00027                                                                   EL603
00028  ENVIRONMENT DIVISION.                                            EL603
00029  DATA DIVISION.                                                   EL603
00030  EJECT                                                            EL603
00031  WORKING-STORAGE SECTION.                                         EL603
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL603
00033  77  FILLER  PIC X(32)  VALUE '*    EL603 WORKING STORAGE     *'. EL603
00034  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.005 ***********'.    CL**5
00035                                                                   EL603
00036      COPY ELCSCTM.                                                   CL**5
00037                                                                      CL**5
00038      COPY ELCSCRTY.                                                  CL**5
00039                                                                   EL603
00040  01  WS-DATE-AREA.                                                EL603
00041      12  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL603
00042      12  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL603
00043                                                                   EL603
00044  01  STANDARD-AREAS.                                              EL603
00045      12  GETMAIN-SPACE       PIC X       VALUE SPACE.             EL603
00046      12  MAP-NAME            PIC X(8)    VALUE 'EL603A'.          EL603
00047      12  MAPSET-NAME         PIC X(8)    VALUE 'EL603S'.          EL603
00048      12  TRANS-ID            PIC X(4)    VALUE 'EXA3'.            EL603
00049      12  THIS-PGM            PIC X(8)    VALUE 'EL603'.           EL603
00050      12  PGM-NAME            PIC X(8).                            EL603
00051      12  TIME-IN             PIC S9(7).                           EL603
00052      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL603
00053          16  FILLER          PIC X.                               EL603
00054          16  TIME-OUT        PIC 99V99.                           EL603
00055          16  FILLER          PIC X(2).                            EL603
00056      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL603
00057      12  XCTL-R05            PIC X(8)    VALUE 'CR005'.           EL603
00058      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL603
00059      12  XCTL-EL126          PIC X(8)    VALUE 'EL126'.              CL**2
00060      12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.              CL**2
00061      12  XCTL-EM626          PIC X(8)    VALUE 'EM626'.              CL**2
00062      12  XCTL-GL800          PIC X(8)    VALUE 'GL800'.              CL**2
00063      12  LINK-001            PIC X(8)    VALUE 'EL001'.           EL603
00064      12  LINK-004            PIC X(8)    VALUE 'EL004'.           EL603
00065      12  LINK-CLDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL603
00066      12  FILE-ID             PIC X(8)    VALUE 'ELCNTL'.          EL603
00067                                                                   EL603
00068  01  ELCNTL-LENGTH           PIC S9(4)   VALUE +750 COMP.            CL**4
00069                                                                   EL603
00070  01  MISC-WORK-AREAS.                                             EL603
00071      12  M-SUB               PIC 99 VALUE ZEROS.                  EL603
00072      12  WS-LOW-PAGE         PIC S9(4)  VALUE +20.                EL603
00073      12  WS-HIGH-PAGE        PIC S9(4)  VALUE +99.                EL603
00074      12  WS-LINE-COUNT       PIC 99 VALUE ZEROS.                  EL603
00075      12  WS-LINE-COUNT-X REDEFINES WS-LINE-COUNT PIC XX.          EL603
00076  01  ERROR-MESSAGES.                                              EL603
00077      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL603
00078      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL603
00079      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL603
00080      12  ER-0043                 PIC X(4)  VALUE '0043'.          EL603
00081      12  ER-0048                 PIC X(4)  VALUE '0048'.          EL603
00082      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL603
00083      12  ER-7008                 PIC X(4)  VALUE '7008'.          EL603
00084      12  ER-7717                 PIC X(4)  VALUE '7717'.             CL**3
00085      12  ER-7737                 PIC X(4)  VALUE '7737'.             CL**3
00086                                                                   EL603
00087  01  ACCESS-KEYS.                                                 EL603
00088      12  ELCNTL-KEY.                                              EL603
00089          16  CK-COMP-ID      PIC X(3).                            EL603
00090          16  FILLER          PIC X       VALUE '8'.               EL603
00091          16  FILLER          PIC X(2)    VALUE SPACES.            EL603
00092          16  CK-SEQ          PIC 99      VALUE 0.                 EL603
00093          16  FILLER          PIC S9(4)   VALUE +0    COMP.        EL603
00094  EJECT                                                            EL603
00095  COPY ELCDATE.                                                       CL**5
00096  EJECT                                                            EL603
00097  COPY ELCLOGOF.                                                      CL**5
00098  EJECT                                                            EL603
00099  COPY ELCATTR.                                                       CL**5
00100  EJECT                                                            EL603
00101  COPY ELCEMIB.                                                       CL**5
00102  EJECT                                                            EL603
00103  COPY ELCINTF.                                                       CL**5
00104      12 FILLER REDEFINES PI-PROGRAM-WORK-AREA.                    EL603
00105         16 PI-CURRENT-PAGE       PIC S99.                         EL603
00106         16 FILLER                PIC X(638).                         CL**5
00107  EJECT                                                            EL603
00108  COPY ELCJPFX.                                                       CL**5
00109                              PIC X(750).                             CL**4
00110  EJECT                                                            EL603
00111  COPY ELCAID.                                                        CL**5
00112  01  FILLER    REDEFINES DFHAID.                                  EL603
00113      12  FILLER              PIC X(8).                            EL603
00114      12  PF-VALUES           PIC X       OCCURS 2.                EL603
00115  EJECT                                                            EL603
00116  COPY EL603S.                                                        CL**5
00117  EJECT                                                            EL603
00118  01  EL603AO-R     REDEFINES EL603AI.                             EL603
00119      12  FILLER              PIC X(31).                           EL603
00120      12  SCREEN-TABLE            OCCURS 10 TIMES                  EL603
00121                                  INDEXED BY ST-INX.               EL603
00122          16  ST-NBR-1-L      PIC S9(4)       COMP.                EL603
00123          16  ST-NBR-1-A      PIC X.                               EL603
00124          16  ST-NBR-1        PIC X(3)        JUSTIFIED RIGHT.        CL**3
00125          16  ST-DESC-1-L     PIC S9(4)       COMP.                EL603
00126          16  ST-DESC-1-A     PIC X.                               EL603
00127          16  ST-DECS-1-I     PIC X(19).                              CL**3
00128          16  ST-EXCL-1-L     PIC S9(4)       COMP.                   CL**3
00129          16  ST-EXCL-1-A     PIC X.                                  CL**3
00130          16  ST-EXCL-1-I     PIC X(01).                              CL**3
00131          16  ST-TARG-1-L     PIC S9(4)       COMP.                   CL**3
00132          16  ST-TARG-1-A     PIC X.                                  CL**3
00133          16  ST-TARG-1-I     PIC S9(02)V9(04).                       CL**3
00134          16  ST-NBR-2-L      PIC S9(4)       COMP.                EL603
00135          16  ST-NBR-2-A      PIC X.                               EL603
00136          16  ST-NBR-2-I      PIC X(3)        JUSTIFIED RIGHT.        CL**3
00137          16  ST-DESC-2-L     PIC S9(4)       COMP.                EL603
00138          16  ST-DESC-2-A     PIC X.                               EL603
00139          16  ST-DESC-2-I     PIC X(19).                              CL**3
00140          16  ST-EXCL-2-L     PIC S9(4)       COMP.                   CL**3
00141          16  ST-EXCL-2-A     PIC X.                                  CL**3
00142          16  ST-EXCL-2-I     PIC X(01).                              CL**3
00143          16  ST-TARG-2-L     PIC S9(4)       COMP.                   CL**3
00144          16  ST-TARG-2-A     PIC X.                                  CL**3
00145          16  ST-TARG-2-I     PIC S9(02)V9(04).                       CL**3
00146          16  ST-TARG-2-X-I   REDEFINES ST-TARG-2-I                   CL**3
00147                              PIC X(06).                              CL**3
00148  01  FILLER        REDEFINES EL603AI.                                CL**3
00149      12  FILLER              PIC X(31).                              CL**3
00150      12  FILLER                  OCCURS 10 TIMES                     CL**3
00151                                  INDEXED BY ST-INX-O.                CL**3
00152          16  ST-NBR-1-O-L    PIC S9(4)       COMP.                   CL**3
00153          16  ST-NBR-1-O-A    PIC X.                                  CL**3
00154          16  ST-NBR-1-O      PIC X(3)        JUSTIFIED RIGHT.        CL**3
00155          16  ST-DESC-1-O-L   PIC S9(4)       COMP.                   CL**3
00156          16  ST-DESC-1-O-A   PIC X.                                  CL**3
00157          16  ST-DESC-1-O     PIC X(19).                              CL**3
00158          16  ST-EXCL-1-O-L   PIC S9(4)       COMP.                   CL**3
00159          16  ST-EXCL-1-O-A   PIC X.                                  CL**3
00160          16  ST-EXCL-1-O     PIC X(01).                              CL**3
00161          16  ST-TARG-1-O-L   PIC S9(4)       COMP.                   CL**3
00162          16  ST-TARG-1-O-A   PIC X.                                  CL**3
00163          16  ST-TARG-1-O     PIC 9(01).9(04).                        CL**3
00164          16  ST-NBR-2-O-L    PIC S9(4)       COMP.                   CL**3
00165          16  ST-NBR-2-O-A    PIC X.                                  CL**3
00166          16  ST-NBR-2-O      PIC X(3)        JUSTIFIED RIGHT.        CL**3
00167          16  ST-DESC-2-O-L   PIC S9(4)       COMP.                   CL**3
00168          16  ST-DESC-2-O-A   PIC X.                                  CL**3
00169          16  ST-DESC-2-O     PIC X(19).                              CL**3
00170          16  ST-EXCL-2-O-L   PIC S9(4)       COMP.                   CL**3
00171          16  ST-EXCL-2-O-A   PIC X.                                  CL**3
00172          16  ST-EXCL-2-O     PIC X(01).                              CL**3
00173          16  ST-TARG-2-O-L   PIC S9(4)       COMP.                   CL**3
00174          16  ST-TARG-2-O-A   PIC X.                                  CL**3
00175          16  ST-TARG-2-O     PIC 9(01).9(04).                        CL**3
00176  EJECT                                                            EL603
00177  LINKAGE SECTION.                                                 EL603
00178                                                                   EL603
00179  01  DFHCOMMAREA             PIC X(1024).                         EL603
00180                                                                   EL603
00181 *01 PARMLIST .                                                       CL**5
00182 *    02  FILLER              PIC S9(8)   COMP.                       CL**5
00183 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL**5
00184  EJECT                                                            EL603
00185  COPY ELCCNTL.                                                       CL**5
00186  EJECT                                                            EL603
00187  PROCEDURE DIVISION.                                              EL603
00188                                                                   EL603
00189      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL603
00190      MOVE '5'                   TO DC-OPTION-CODE.                EL603
00191      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL603
00192      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL603
00193      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL603
00194                                                                   EL603
00195      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL603
00196                                                                   EL603
00197      IF EIBCALEN = 0                                              EL603
00198          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL603
00199                                                                   EL603
00200      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL603
00201          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL603
00202              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL603
00203              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL603
00204              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL603
00205              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL603
00206              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL603
00207              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL603
00208              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL603
00209              MOVE THIS-PGM TO PI-CALLING-PROGRAM                  EL603
00210          ELSE                                                     EL603
00211              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL603
00212              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL603
00213              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL603
00214              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL603
00215              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL603
00216              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL603
00217              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL603
00218              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL603
00219                                                                   EL603
00220      EXEC CICS HANDLE CONDITION                                   EL603
00221          PGMIDERR(9600-PGMID-ERROR)                               EL603
00222          ERROR   (9990-ABEND)                                     EL603
00223      END-EXEC.                                                    EL603
00224                                                                   EL603
00225      IF NOT SYSTEM-DISPLAY-CAP                                    EL603
00226         NEXT SENTENCE                                             EL603
00227                                                                      CL**3
00228      ELSE                                                            CL**3
00229          IF EIBTRNID NOT = TRANS-ID                                  CL**3
00230             MOVE SPACES          TO PI-PROGRAM-WORK-AREA             CL**3
00231             MOVE +20             TO PI-CURRENT-PAGE                  CL**3
00232             GO TO 7000-BUILD-INITIAL-MAP.                            CL**3
00233                                                                   EL603
00234      IF EIBAID = DFHCLEAR                                         EL603
00235          GO TO 9400-CLEAR.                                        EL603
00236                                                                   EL603
00237      IF NOT SYSTEM-DISPLAY-CAP                                    EL603
00238          MOVE 'READ'         TO SM-READ                           EL603
00239          PERFORM 9995-SECURITY-VIOLATION                          EL603
00240          MOVE ER-0070        TO EMI-ERROR                         EL603
00241          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL603
00242          MOVE SPACES         TO PI-PROGRAM-WORK-AREA              EL603
00243          MOVE +20            TO PI-CURRENT-PAGE                   EL603
00244          GO TO 7000-BUILD-INITIAL-MAP.                            EL603
00245                                                                   EL603
00246  EJECT                                                            EL603
00247  0200-RECEIVE.                                                    EL603
00248      MOVE LOW-VALUES TO EL603AI.                                  EL603
00249      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL603
00250          MOVE ER-7008 TO EMI-ERROR                                EL603
00251          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL603
00252          MOVE -1 TO ENTERPFL                                      EL603
00253          GO TO 8200-SEND-DATAONLY.                                EL603
00254                                                                   EL603
00255      EXEC CICS RECEIVE                                            EL603
00256          MAP   (MAP-NAME)                                         EL603
00257          MAPSET(MAPSET-NAME)                                      EL603
00258          INTO  (EL603AI)                                          EL603
00259      END-EXEC.                                                    EL603
00260                                                                   EL603
00261      IF ENTERPFL = 0                                              EL603
00262          GO TO 0300-CHECK-PFKEYS.                                 EL603
00263      IF EIBAID NOT = DFHENTER                                     EL603
00264          MOVE ER-0004 TO EMI-ERROR                                EL603
00265          GO TO 0320-INPUT-ERROR.                                  EL603
00266      IF ENTERPFI NOT NUMERIC                                      EL603
00267         GO TO 0300-CHECK-PFKEYS.                                  EL603
00268      IF ENTERPFI GREATER 0 AND LESS 25                            EL603
00269          MOVE PF-VALUES (ENTERPFI) TO EIBAID                      EL603
00270      ELSE                                                         EL603
00271          MOVE ER-0029 TO EMI-ERROR                                EL603
00272          GO TO 0320-INPUT-ERROR.                                  EL603
00273  EJECT                                                            EL603
00274  0300-CHECK-PFKEYS.                                               EL603
00275      IF EIBAID = DFHPF1 AND                                       EL603
00276         PI-CURRENT-PAGE = WS-HIGH-PAGE                            EL603
00277         MOVE +20 TO PI-CURRENT-PAGE                               EL603
00278         GO TO 7000-BUILD-INITIAL-MAP.                             EL603
00279                                                                   EL603
00280      IF EIBAID = DFHPF1                                           EL603
00281         IF PI-CURRENT-PAGE = +80                                  EL603
00282            MOVE +99 TO PI-CURRENT-PAGE                            EL603
00283            GO TO 7000-BUILD-INITIAL-MAP                           EL603
00284         ELSE                                                      EL603
00285            ADD +20 TO PI-CURRENT-PAGE                             EL603
00286            GO TO 7000-BUILD-INITIAL-MAP                           EL603
00287      ELSE                                                         EL603
00288         NEXT SENTENCE.                                            EL603
00289                                                                   EL603
00290      IF EIBAID = DFHPF2                                           EL603
00291         MOVE +20 TO PI-CURRENT-PAGE                               EL603
00292         GO TO 7000-BUILD-INITIAL-MAP.                             EL603
00293                                                                   EL603
00294      IF EIBAID = DFHPF23                                          EL603
00295          GO TO 8810-PF23.                                         EL603
00296      IF EIBAID = DFHPF24                                          EL603
00297          GO TO 9200-RETURN-MAIN-MENU.                             EL603
00298      IF EIBAID = DFHPF12                                          EL603
00299          GO TO 9500-PF12.                                         EL603
00300      IF EIBAID = DFHENTER                                         EL603
00301          GO TO 0330-EDIT-DATA.                                    EL603
00302                                                                   EL603
00303      MOVE ER-0029 TO EMI-ERROR.                                   EL603
00304                                                                   EL603
00305  0320-INPUT-ERROR.                                                EL603
00306      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL603
00307      MOVE AL-UNBON TO ENTERPFA.                                   EL603
00308      MOVE -1       TO ENTERPFL.                                   EL603
00309      GO TO 8200-SEND-DATAONLY.                                    EL603
00310                                                                   EL603
00311  0330-EDIT-DATA.                                                  EL603
00312      SET ST-INX TO 1.                                             EL603
00313                                                                   EL603
00314  0330-ED-10.                                                      EL603
00315      IF  ST-INX GREATER 10                                           CL**3
00316                                                                   EL603
00317          IF  EMI-ERROR GREATER THAN '0000'                           CL**3
00318              GO TO 8200-SEND-DATAONLY                                CL**3
00319                                                                   EL603
00320          ELSE                                                        CL**3
00321              GO TO 1000-UPDATE-FILE.                                 CL**3
00322                                                                      CL**3
00323      IF  ST-DESC-1-L (ST-INX) = ZEROS                                CL**3
00324              AND                                                     CL**3
00325          ST-DESC-2-L (ST-INX) = ZEROS                                CL**3
00326              AND                                                     CL**3
00327          ST-TARG-1-L (ST-INX) = ZEROS                                CL**3
00328              AND                                                     CL**3
00329          ST-TARG-2-L (ST-INX) = ZEROS                                CL**3
00330              AND                                                     CL**3
00331          ST-EXCL-1-L (ST-INX) = ZEROS                                CL**3
00332              AND                                                     CL**3
00333          ST-EXCL-2-L (ST-INX) = ZEROS                                CL**3
00334          SET ST-INX UP BY 1                                          CL**3
00335          GO TO 0330-ED-10.                                           CL**3
00336                                                                      CL**3
00337      IF  ST-EXCL-1-L (ST-INX) GREATER THAN ZEROS                     CL**3
00338           IF ST-EXCL-1-I (ST-INX) EQUAL 'X' OR SPACES                CL**3
00339               NEXT SENTENCE                                          CL**3
00340           ELSE                                                       CL**3
00341              MOVE -1 TO ST-EXCL-1-L (ST-INX)                         CL**3
00342              MOVE AL-UNBON TO ST-EXCL-1-A (ST-INX)                   CL**3
00343              MOVE ER-7737 TO EMI-ERROR                               CL**3
00344              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL603
00345                                                                      CL**3
00346      IF  ST-TARG-1-L (ST-INX) GREATER THAN ZEROS                     CL**3
00347                                                                      CL**3
00348          EXEC CICS BIF                                               CL**3
00349               DEEDIT                                                 CL**3
00350               FIELD  (ST-TARG-1-I (ST-INX))                          CL**3
00351               LENGTH (06)                                            CL**3
00352          END-EXEC                                                    CL**3
00353                                                                      CL**3
00354          IF  ST-TARG-1-I (ST-INX) NUMERIC                            CL**3
00355                  AND                                                 CL**3
00356              ST-TARG-1-I (ST-INX) GREATER THAN +0.0                  CL**3
00357              MOVE AL-UNNON       TO ST-TARG-1-A (ST-INX)             CL**3
00358                                                                      CL**3
00359          ELSE                                                        CL**3
00360              MOVE -1             TO ST-TARG-1-L (ST-INX)             CL**3
00361              MOVE AL-UABON       TO ST-TARG-1-A (ST-INX)             CL**3
00362              MOVE ER-7717        TO EMI-ERROR                        CL**3
00363              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**3
00364                                                                      CL**3
00365      IF  ST-EXCL-2-L (ST-INX) GREATER THAN ZEROS                     CL**3
00366           IF ST-EXCL-2-I (ST-INX) EQUAL 'X' OR SPACES                CL**3
00367               NEXT SENTENCE                                          CL**3
00368           ELSE                                                       CL**3
00369              MOVE -1 TO ST-EXCL-2-L (ST-INX)                         CL**3
00370              MOVE AL-UNBON TO ST-EXCL-2-A (ST-INX)                   CL**3
00371              MOVE ER-7737 TO EMI-ERROR                               CL**3
00372              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**3
00373                                                                      CL**3
00374      IF  ST-TARG-2-L (ST-INX) GREATER THAN ZEROS                     CL**3
00375                                                                      CL**3
00376          EXEC CICS BIF                                               CL**3
00377               DEEDIT                                                 CL**3
00378               FIELD  (ST-TARG-2-I (ST-INX))                          CL**3
00379               LENGTH (06)                                            CL**3
00380          END-EXEC                                                    CL**3
00381                                                                      CL**3
00382          IF  ST-TARG-2-I (ST-INX) NUMERIC                            CL**3
00383                  AND                                                 CL**3
00384              ST-TARG-2-I (ST-INX) GREATER THAN +0.0                  CL**3
00385              MOVE AL-UNNON       TO ST-TARG-2-A (ST-INX)             CL**3
00386                                                                      CL**3
00387          ELSE                                                        CL**3
00388              MOVE -1             TO ST-TARG-2-L (ST-INX)             CL**3
00389              MOVE AL-UABON       TO ST-TARG-2-A (ST-INX)             CL**3
00390              MOVE ER-7717        TO EMI-ERROR                        CL**3
00391              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**3
00392                                                                      CL**3
00393      SET ST-INX UP BY 1.                                             CL**3
00394      GO TO 0330-ED-10.                                               CL**3
00395                                                                   EL603
00396  0330-EDIT-DATA-EXIT.                                             EL603
00397      EXIT.                                                        EL603
00398  EJECT                                                            EL603
00399  1000-UPDATE-FILE.                                                EL603
00400      IF NOT SYSTEM-MODIFY-CAP                                     EL603
00401          MOVE 'UPDATE'       TO SM-READ                           EL603
00402          PERFORM 9995-SECURITY-VIOLATION                          EL603
00403          MOVE ER-0070        TO EMI-ERROR                         EL603
00404          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL603
00405          GO TO 7000-BUILD-INITIAL-MAP.                            EL603
00406                                                                   EL603
00407      MOVE PI-COMPANY-ID TO CK-COMP-ID.                            EL603
00408      MOVE PI-CURRENT-PAGE TO CK-SEQ.                              EL603
00409      EXEC CICS HANDLE CONDITION                                   EL603
00410          NOTFND    (2000-ADD-BUSN)                                EL603
00411          ERROR     (9990-ABEND)                                   EL603
00412      END-EXEC.                                                    EL603
00413                                                                   EL603
00414      EXEC CICS READ                                               EL603
00415          UPDATE                                                   EL603
00416          DATASET('ELCNTL')                                        EL603
00417          SET    (ADDRESS OF CONTROL-FILE)                            CL**5
00418          RIDFLD (ELCNTL-KEY)                                      EL603
00419      END-EXEC.                                                    EL603
00420                                                                   EL603
00421      MOVE 'B'          TO JP-RECORD-TYPE.                         EL603
00422      MOVE CONTROL-FILE TO JP-RECORD-AREA.                         EL603
00423      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL603
00424      MOVE SPACES       TO CF-BUSINESS-TYPE-MASTER-REC.            EL603
00425                                                                   EL603
00426  1000-UPDATE-30.                                                  EL603
00427      MOVE +1 TO M-SUB.                                            EL603
00428      SET ST-INX TO 1.                                             EL603
00429                                                                   EL603
00430  1000-UPDATE-40.                                                  EL603
00431      IF ST-INX GREATER 10                                         EL603
00432          GO TO 1000-UPDATE-100.                                   EL603
00433                                                                   EL603
00434      IF ST-DESC-1-L (ST-INX) GREATER THAN ZEROS                      CL**3
00435         MOVE ST-DECS-1-I (ST-INX) TO CF-BUSINESS-TITLE (M-SUB).      CL**3
00436                                                                   EL603
00437      IF ST-EXCL-1-L (ST-INX) GREATER THAN ZEROS                      CL**3
00438         MOVE ST-EXCL-1-I (ST-INX) TO CF-BUS-EXCL-ST-CALL (M-SUB).    CL**3
00439                                                                   EL603
00440      IF  ST-TARG-1-L (ST-INX) GREATER THAN ZEROS                     CL**3
00441          MOVE ST-TARG-1-I (ST-INX)                                   CL**3
00442              TO CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB)                CL**3
00443 *                                                                    CL**3
00444 *    IF  ST-TARG-1-I (ST-INX) NUMERIC                                CL**3
00445 *            AND                                                     CL**3
00446 *        ST-TARG-1-I (ST-INX) NOT EQUAL +0.0                         CL**3
00447 *        MOVE ST-TARG-1-I (ST-INX)                                   CL**3
00448 *            TO CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB)                CL**3
00449 *                                                                    CL**3
00450 *    ELSE                                                            CL**3
00451 *        MOVE +1.0                                                   CL**3
00452 *            TO CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB).               CL**3
00453                                                                   EL603
00454      ADD +1 TO M-SUB.                                                CL**3
00455                                                                      CL**3
00456      IF ST-DESC-2-L (ST-INX) GREATER THAN ZEROS                      CL**3
00457         MOVE ST-DESC-2-I (ST-INX)                                    CL**3
00458                                  TO CF-BUSINESS-TITLE (M-SUB).       CL**3
00459                                                                      CL**3
00460      IF ST-EXCL-2-L (ST-INX) GREATER THAN ZEROS                      CL**3
00461         MOVE ST-EXCL-2-I (ST-INX) TO CF-BUS-EXCL-ST-CALL (M-SUB).    CL**3
00462                                                                      CL**3
00463      IF  ST-TARG-2-L (ST-INX) GREATER THAN ZEROS                     CL**3
00464          MOVE ST-TARG-2-I (ST-INX)                                   CL**3
00465              TO CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB)                CL**3
00466 *                                                                    CL**3
00467 *    IF  ST-TARG-2-I (ST-INX) NUMERIC                                CL**3
00468 *            AND                                                     CL**3
00469 *        ST-TARG-2-I (ST-INX) NOT EQUAL +0.0                         CL**3
00470 *        MOVE ST-TARG-2-I (ST-INX)                                   CL**3
00471 *            TO CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB)                CL**3
00472 *                                                                    CL**3
00473 *    ELSE                                                            CL**3
00474 *        MOVE +1.0                                                   CL**3
00475 *            TO CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB).               CL**3
00476                                                                      CL**3
00477      ADD +1 TO M-SUB.                                                CL**3
00478                                                                   EL603
00479      SET ST-INX UP BY +1.                                         EL603
00480                                                                   EL603
00481      GO TO 1000-UPDATE-40.                                        EL603
00482                                                                   EL603
00483  1000-UPDATE-100.                                                 EL603
00484      MOVE 'C' TO JP-RECORD-TYPE.                                  EL603
00485      MOVE CONTROL-FILE TO JP-RECORD-AREA.                         EL603
00486      EXEC CICS REWRITE                                            EL603
00487          DATASET('ELCNTL')                                        EL603
00488          FROM(CONTROL-FILE)                                       EL603
00489      END-EXEC.                                                    EL603
00490                                                                   EL603
00491      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL603
00492      MOVE ER-0000 TO EMI-ERROR.                                   EL603
00493      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL603
00494                                                                   EL603
00495      GO TO 7000-BUILD-INITIAL-MAP.                                EL603
00496  EJECT                                                            EL603
00497  2000-ADD-BUSN.                                                   EL603
00498      EXEC CICS GETMAIN                                            EL603
00499          SET    (ADDRESS OF CONTROL-FILE)                            CL**5
00500          INITIMG(GETMAIN-SPACE)                                   EL603
00501          LENGTH (ELCNTL-LENGTH)                                   EL603
00502      END-EXEC.                                                    EL603
00503                                                                   EL603
00504      MOVE ELCNTL-KEY TO CF-CONTROL-PRIMARY.                       EL603
00505      MOVE 'CF'       TO CF-RECORD-ID.                             EL603
00506                                                                   EL603
00507  2000-ADD-M-30.                                                   EL603
00508      MOVE +1 TO M-SUB.                                            EL603
00509      SET ST-INX TO 1.                                             EL603
00510                                                                   EL603
00511  2000-ADD-M-40.                                                   EL603
00512      IF ST-INX GREATER 10                                         EL603
00513          GO TO 2000-ADD-M-100.                                    EL603
00514                                                                   EL603
00515      IF ST-DESC-1-L (ST-INX) = ZEROS                              EL603
00516         NEXT SENTENCE                                             EL603
00517      ELSE                                                         EL603
00518         MOVE ST-DECS-1-I (ST-INX) TO CF-BUSINESS-TITLE (M-SUB).      CL**3
00519                                                                   EL603
00520      IF  ST-TARG-1-I (ST-INX) NUMERIC                                CL**3
00521              AND                                                     CL**3
00522          ST-TARG-1-I (ST-INX) NOT EQUAL +0.0                         CL**3
00523          MOVE ST-TARG-1-I (ST-INX)                                   CL**3
00524              TO CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB)                CL**3
00525                                                                   EL603
00526      ELSE                                                            CL**3
00527          MOVE +1.0                                                   CL**3
00528              TO CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB).               CL**3
00529                                                                      CL**3
00530      IF ST-EXCL-1-L (ST-INX) GREATER THAN ZEROS                      CL**3
00531         MOVE ST-EXCL-1-I (ST-INX) TO CF-BUS-EXCL-ST-CALL (M-SUB).    CL**3
00532                                                                      CL**3
00533      ADD +1 TO M-SUB.                                                CL**3
00534                                                                      CL**3
00535      IF ST-DESC-2-L (ST-INX) GREATER THAN ZEROS                      CL**3
00536         MOVE ST-DESC-2-I (ST-INX)                                    CL**3
00537                                  TO CF-BUSINESS-TITLE (M-SUB).       CL**3
00538                                                                      CL**3
00539      IF  ST-TARG-2-I (ST-INX) NUMERIC                                CL**3
00540              AND                                                     CL**3
00541          ST-TARG-2-I (ST-INX) NOT EQUAL +0.0                         CL**3
00542          MOVE ST-TARG-2-I (ST-INX)                                   CL**3
00543              TO CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB)                CL**3
00544                                                                      CL**3
00545      ELSE                                                         EL603
00546          MOVE +1.0                                                   CL**3
00547              TO CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB).               CL**3
00548                                                                   EL603
00549      IF ST-EXCL-2-L (ST-INX) GREATER THAN ZEROS                      CL**3
00550         MOVE ST-EXCL-2-I (ST-INX) TO CF-BUS-EXCL-ST-CALL (M-SUB).    CL**3
00551                                                                      CL**3
00552      ADD +1 TO M-SUB.                                                CL**3
00553                                                                   EL603
00554      SET ST-INX UP BY +1.                                         EL603
00555                                                                   EL603
00556      GO TO 2000-ADD-M-40.                                         EL603
00557                                                                   EL603
00558  2000-ADD-M-100.                                                  EL603
00559      MOVE CONTROL-FILE TO JP-RECORD-AREA.                         EL603
00560                                                                   EL603
00561      EXEC CICS WRITE                                              EL603
00562          DATASET('ELCNTL')                                        EL603
00563          FROM   (CONTROL-FILE)                                    EL603
00564          RIDFLD (ELCNTL-KEY)                                      EL603
00565      END-EXEC.                                                    EL603
00566                                                                   EL603
00567      MOVE 'A'     TO JP-RECORD-TYPE.                              EL603
00568      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL603
00569      MOVE ER-0000 TO EMI-ERROR.                                   EL603
00570      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL603
00571                                                                   EL603
00572      GO TO 7000-BUILD-INITIAL-MAP.                                EL603
00573                                                                   EL603
00574  2000-ADD-BUSN-EXIT.                                              EL603
00575      EXIT.                                                        EL603
00576  EJECT                                                            EL603
00577  7000-BUILD-INITIAL-MAP.                                          EL603
00578      MOVE LOW-VALUES TO EL603AO.                                  EL603
00579      MOVE -1         TO DC01L.                                    EL603
00580      MOVE SPACES     TO EMI-MESSAGE-AREA (2).                     EL603
00581      IF PI-CURRENT-PAGE = WS-LOW-PAGE                             EL603
00582         MOVE 'FIRST PAGE' TO EMI-MESSAGE-AREA (2).                EL603
00583                                                                   EL603
00584      IF PI-CURRENT-PAGE = WS-HIGH-PAGE                            EL603
00585         MOVE 'LAST PAGE' TO EMI-MESSAGE-AREA (2).                 EL603
00586                                                                   EL603
00587      MOVE PI-CURRENT-PAGE TO CK-SEQ.                              EL603
00588      MOVE PI-COMPANY-ID TO CK-COMP-ID.                            EL603
00589                                                                   EL603
00590      EXEC CICS HANDLE CONDITION                                   EL603
00591          NOTFND(7000-EXIT)                                        EL603
00592      END-EXEC.                                                    EL603
00593                                                                   EL603
00594      EXEC CICS READ                                               EL603
00595          DATASET('ELCNTL')                                        EL603
00596          SET    (ADDRESS OF CONTROL-FILE)                            CL**5
00597          RIDFLD (ELCNTL-KEY)                                      EL603
00598      END-EXEC.                                                    EL603
00599                                                                   EL603
00600      SET ST-INX TO 1.                                             EL603
00601      MOVE +1 TO M-SUB.                                            EL603
00602                                                                   EL603
00603  7000-IBLD-10.                                                    EL603
00604      IF ST-INX GREATER 10                                         EL603
00605          GO TO 7000-EXIT.                                         EL603
00606                                                                   EL603
00607      MOVE CF-BUSINESS-TITLE (M-SUB) TO ST-DECS-1-I (ST-INX).         CL**3
00608      MOVE AL-UANON TO ST-DESC-1-A (ST-INX).                       EL603
00609                                                                   EL603
00610      MOVE CF-BUS-EXCL-ST-CALL (M-SUB) TO ST-EXCL-1-I (ST-INX).       CL**3
00611      MOVE AL-UANON TO ST-EXCL-1-A (ST-INX).                          CL**3
00612                                                                   EL603
00613      IF  CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB) NUMERIC               CL**3
00614          MOVE CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB)                  CL**3
00615                                  TO ST-TARG-1-O (ST-INX)             CL**3
00616                                                                      CL**3
00617      ELSE                                                            CL**3
00618          MOVE +1.0000            TO ST-TARG-1-O (ST-INX).            CL**3
00619                                                                      CL**3
00620      MOVE AL-UNNON TO ST-TARG-1-A (ST-INX).                          CL**3
00621                                                                      CL**3
00622      ADD +1 TO M-SUB.                                             EL603
00623                                                                      CL**3
00624      MOVE CF-BUSINESS-TITLE (M-SUB)  TO ST-DESC-2-O (ST-INX).        CL**3
00625                                                                      CL**3
00626      MOVE CF-BUS-EXCL-ST-CALL (M-SUB) TO ST-EXCL-2-O (ST-INX).       CL**3
00627                                                                      CL**3
00628      IF  CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB) NUMERIC               CL**3
00629          MOVE CF-BUS-MOD-ST-TRGT-LOSS-RATIO (M-SUB)                  CL**3
00630                                  TO ST-TARG-2-O (ST-INX)             CL**3
00631                                                                      CL**3
00632      ELSE                                                            CL**3
00633          MOVE +1.0000            TO ST-TARG-2-O (ST-INX).            CL**3
00634                                                                      CL**3
00635      MOVE AL-UANON TO ST-EXCL-2-A (ST-INX).                          CL**3
00636      MOVE AL-UANON TO ST-DESC-2-A (ST-INX).                       EL603
00637      MOVE AL-UNNON TO ST-TARG-2-A (ST-INX).                          CL**3
00638                                                                   EL603
00639                                                                   EL603
00640      SET ST-INX UP BY +1.                                         EL603
00641      ADD +1 TO M-SUB.                                             EL603
00642                                                                   EL603
00643      GO TO 7000-IBLD-10.                                          EL603
00644                                                                   EL603
00645  7000-EXIT.                                                       EL603
00646      SET ST-INX TO 1.                                             EL603
00647      COMPUTE WS-LINE-COUNT = (PI-CURRENT-PAGE - 20 + 1).          EL603
00648      IF WS-LINE-COUNT = 80                                        EL603
00649          ADD +1 TO WS-LINE-COUNT.                                 EL603
00650                                                                   EL603
00651  7000-500.                                                        EL603
00652      IF ST-INX GREATER 10                                         EL603
00653          GO TO 7000-550.                                          EL603
00654                                                                   EL603
00655      MOVE WS-LINE-COUNT-X        TO ST-NBR-1 (ST-INX).               CL**3
00656      IF WS-LINE-COUNT = 99                                        EL603
00657         MOVE AL-SANOF            TO ST-DESC-2-A (ST-INX)             CL**3
00658                                     ST-EXCL-2-A (ST-INX)             CL**3
00659                                     ST-TARG-2-A (ST-INX)             CL**3
00660         MOVE SPACES              TO ST-NBR-2-I (ST-INX)              CL**3
00661                                     ST-TARG-2-X-I (ST-INX)           CL**3
00662         SET ST-INX UP BY 1                                        EL603
00663         GO TO 7000-500.                                           EL603
00664                                                                   EL603
00665      ADD +1 TO WS-LINE-COUNT.                                     EL603
00666      MOVE WS-LINE-COUNT-X TO ST-NBR-2-I (ST-INX).                    CL**3
00667      ADD +1 TO WS-LINE-COUNT.                                     EL603
00668      SET ST-INX UP BY 1.                                          EL603
00669      GO TO 7000-500.                                              EL603
00670                                                                   EL603
00671  7000-550.                                                        EL603
00672      EXIT.                                                        EL603
00673  EJECT                                                            EL603
00674  8100-SEND-INITIAL-MAP.                                           EL603
00675      MOVE SAVE-DATE            TO RUNDTEO.                        EL603
00676      MOVE EIBTIME              TO TIME-IN.                        EL603
00677      MOVE TIME-OUT             TO RUNTIMEO.                       EL603
00678      MOVE EMI-MESSAGE-AREA (1) TO ERRMSGO.                        EL603
00679      MOVE EMI-MESSAGE-AREA (2) TO PGENBRO.                        EL603
00680      EXEC CICS SEND                                               EL603
00681          MAP   (MAP-NAME)                                         EL603
00682          MAPSET(MAPSET-NAME)                                      EL603
00683          FROM  (EL603AO)                                          EL603
00684          ERASE                                                    EL603
00685          CURSOR                                                   EL603
00686      END-EXEC.                                                    EL603
00687                                                                   EL603
00688      GO TO 9100-RETURN-TRAN.                                      EL603
00689                                                                   EL603
00690  8200-SEND-DATAONLY.                                              EL603
00691      MOVE SAVE-DATE            TO RUNDTEO.                        EL603
00692      MOVE EIBTIME              TO TIME-IN.                        EL603
00693      MOVE TIME-OUT             TO RUNTIMEO.                       EL603
00694      MOVE EMI-MESSAGE-AREA (1) TO ERRMSGO.                        EL603
00695      EXEC CICS SEND                                               EL603
00696          MAP   (MAP-NAME)                                         EL603
00697          MAPSET(MAPSET-NAME)                                      EL603
00698          FROM  (EL603AO)                                          EL603
00699          DATAONLY                                                 EL603
00700          CURSOR                                                   EL603
00701      END-EXEC.                                                    EL603
00702                                                                   EL603
00703      GO TO 9100-RETURN-TRAN.                                      EL603
00704                                                                   EL603
00705  8300-SEND-TEXT.                                                  EL603
00706      EXEC CICS SEND TEXT                                          EL603
00707          FROM  (LOGOFF-TEXT)                                      EL603
00708          LENGTH(LOGOFF-LENGTH)                                    EL603
00709          ERASE                                                    EL603
00710          FREEKB                                                   EL603
00711      END-EXEC.                                                    EL603
00712                                                                   EL603
00713      EXEC CICS RETURN                                             EL603
00714      END-EXEC.                                                    EL603
00715                                                                   EL603
00716  8400-LOG-JOURNAL-RECORD.                                         EL603
00717      MOVE PI-PROCESSOR-ID TO JP-USER-ID.                          EL603
00718      MOVE FILE-ID         TO JP-FILE-ID.                          EL603
00719      MOVE THIS-PGM        TO JP-PROGRAM-ID.                       EL603
00720                                                                   EL603
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZEROS                            EL603
pemuni*        EXEC CICS JOURNAL                                        EL603
pemuni*            JFILEID(PI-JOURNAL-FILE-ID)                          EL603
pemuni*            JTYPEID('EL')                                        EL603
pemuni*            FROM   (JOURNAL-RECORD)                              EL603
pemuni*            LENGTH (773)                                            CL**4
pemuni*        END-EXEC.                                                EL603
00728                                                                   EL603
00729  8800-UNAUTHORIZED-ACCESS.                                        EL603
00730      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL603
00731      GO TO 8300-SEND-TEXT.                                        EL603
00732                                                                   EL603
00733  8810-PF23.                                                       EL603
00734      MOVE EIBAID   TO PI-ENTRY-CD-1.                              EL603
00735      MOVE XCTL-005 TO PGM-NAME.                                   EL603
00736      GO TO 9300-XCTL.                                             EL603
00737                                                                   EL603
00738  8880-NOT-FOUND.                                                  EL603
00739      MOVE ER-0043 TO EMI-ERROR.                                   EL603
00740      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL603
00741      MOVE -1 TO ENTERPFL.                                         EL603
00742      IF EIBTRNID NOT = TRANS-ID                                   EL603
00743          GO TO 8100-SEND-INITIAL-MAP.                             EL603
00744                                                                   EL603
00745      GO TO 8200-SEND-DATAONLY.                                    EL603
00746  EJECT                                                            EL603
00747  9000-RETURN-CICS.                                                EL603
00748      EXEC CICS RETURN                                             EL603
00749      END-EXEC.                                                    EL603
00750                                                                   EL603
00751  9100-RETURN-TRAN.                                                EL603
00752      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL603
00753      MOVE '603A' TO PI-CURRENT-SCREEN-NO.                         EL603
00754      EXEC CICS RETURN                                             EL603
00755          TRANSID (TRANS-ID)                                       EL603
00756          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL603
00757          LENGTH  (PI-COMM-LENGTH)                                 EL603
00758      END-EXEC.                                                    EL603
00759                                                                   EL603
00760  9200-RETURN-MAIN-MENU.                                           EL603
00761                                                                      CL**2
00762      IF  CREDIT-SESSION                                              CL**2
00763          MOVE XCTL-EL626         TO PGM-NAME                         CL**2
00764                                                                      CL**2
00765      ELSE                                                            CL**2
00766          IF  CLAIM-SESSION                                           CL**2
00767              MOVE XCTL-EL126     TO PGM-NAME                         CL**2
00768                                                                      CL**2
00769          ELSE                                                        CL**2
00770              IF  MORTGAGE-SESSION                                    CL**2
00771                  MOVE XCTL-EM626 TO PGM-NAME                         CL**2
00772                                                                      CL**2
00773              ELSE                                                    CL**2
00774                  IF  GENERAL-LEDGER-SESSION                          CL**2
00775                      MOVE XCTL-GL800                                 CL**2
00776                                  TO PGM-NAME.                        CL**2
00777                                                                      CL**2
00778      GO TO 9300-XCTL.                                             EL603
00779                                                                   EL603
00780  9300-XCTL.                                                       EL603
00781      EXEC CICS XCTL                                               EL603
00782          PROGRAM (PGM-NAME)                                       EL603
00783          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL603
00784          LENGTH  (PI-COMM-LENGTH)                                 EL603
00785      END-EXEC.                                                    EL603
00786                                                                   EL603
00787  9400-CLEAR.                                                      EL603
00788      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       EL603
00789      GO TO 9300-XCTL.                                             EL603
00790                                                                   EL603
00791  9500-PF12.                                                       EL603
00792      MOVE XCTL-010 TO PGM-NAME.                                   EL603
00793      GO TO 9300-XCTL.                                             EL603
00794                                                                   EL603
00795  9600-PGMID-ERROR.                                                EL603
00796      EXEC CICS HANDLE CONDITION                                   EL603
00797          PGMIDERR(8300-SEND-TEXT)                                 EL603
00798      END-EXEC.                                                    EL603
00799                                                                   EL603
00800      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                     EL603
00801      MOVE ' '          TO PI-ENTRY-CD-1.                          EL603
00802      MOVE XCTL-005     TO PGM-NAME.                               EL603
00803      MOVE PGM-NAME     TO LOGOFF-PGM.                             EL603
00804      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL603
00805      GO TO 9300-XCTL.                                             EL603
00806                                                                   EL603
00807  9700-LINK-DATE-CONVERT.                                          EL603
00808      EXEC CICS LINK                                               EL603
00809          PROGRAM    ('ELDATCV')                                   EL603
00810          COMMAREA   (DATE-CONVERSION-DATA)                        EL603
00811          LENGTH     (DC-COMM-LENGTH)                              EL603
00812      END-EXEC.                                                    EL603
00813                                                                   EL603
00814  9700-EXIT.                                                       EL603
00815      EXIT.                                                        EL603
00816                                                                   EL603
00817  9900-ERROR-FORMAT.                                               EL603
00818      IF NOT EMI-ERRORS-COMPLETE                                   EL603
00819          MOVE LINK-001 TO PGM-NAME                                EL603
00820          EXEC CICS LINK                                           EL603
00821              PROGRAM(PGM-NAME)                                    EL603
00822              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL603
00823              LENGTH(EMI-COMM-LENGTH)                              EL603
00824          END-EXEC.                                                EL603
00825                                                                   EL603
00826  9900-EXIT.                                                       EL603
00827      EXIT.                                                        EL603
00828                                                                   EL603
00829  9990-ABEND.                                                      EL603
00830      MOVE LINK-004    TO PGM-NAME.                                EL603
00831      MOVE DFHEIBLK    TO EMI-LINE1.                               EL603
00832      EXEC CICS LINK                                               EL603
00833          PROGRAM   (PGM-NAME)                                     EL603
00834          COMMAREA  (EMI-LINE1)                                    EL603
00835          LENGTH    (72)                                           EL603
00836      END-EXEC.                                                    EL603
00837                                                                   EL603
00838      GO TO 8200-SEND-DATAONLY.                                    EL603
00839                                                                   EL603
00840  9995-SECURITY-VIOLATION.                                         EL603
00841             COPY ELCSCTP.                                         EL603
00842                                                                   EL603
00843  9995-EXIT.                                                       EL603
00844       EXIT.                                                       EL603
00845                                                                   EL603
