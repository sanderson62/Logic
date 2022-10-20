00001  ID DIVISION.                                                     03/06/96
00002                                                                   EL157
00003  PROGRAM-ID.                 EL157.                                  LV011
00004 *              PROGRAM CONVERTED BY                                  CL*11
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*11
00006 *              CONVERSION DATE 02/13/96 09:55:16.                    CL*11
00007 *                            VMOD=2.011                              CL*11
00008 *                                                                 EL157
00009 *AUTHOR.     LOGIC,INC.                                              CL*11
00010 *            DALLAS, TEXAS.                                          CL*11
00011                                                                   EL157
00012 *DATE-COMPILED.                                                      CL*11
00013                                                                   EL157
00014 *SECURITY.   *****************************************************   CL*11
00015 *            *                                                   *   CL*11
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*11
00017 *            *                                                   *   CL*11
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*11
00019 *                                                                *   CL*11
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*11
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*11
00022 *            *                                                   *   CL*11
00023 *            *****************************************************   CL*11
00024                                                                   EL157
00025 *REMARKS.    FORM REQUEST.                                           CL**3
00026                                                                   EL157
00027  ENVIRONMENT DIVISION.                                            EL157
00028  DATA DIVISION.                                                   EL157
00029                                                                   EL157
00030      EJECT                                                        EL157
00031  WORKING-STORAGE SECTION.                                         EL157
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL157
00033  77  FILLER  PIC X(32)  VALUE '*    EL157 WORKING STORAGE     *'. EL157
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.011 *********'.    CL*11
00035                                                                   EL157
00036  01  WS-DATE-AREA.                                                EL157
00037      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL157
00038      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL157
00039                                                                   EL157
00040  01  MISC-WORK-AREAS.                                             EL157
00041      12  ACCT-FOUND-SW           PIC X(01)   VALUE LOW-VALUES.       CL**3
00042      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         EL157
00043      12  MAP-NAME                PIC X(8)    VALUE 'EL157A'.      EL157
00044      12  MAPSET-NAME             PIC X(8)    VALUE 'EL157S'.      EL157
00045      12  TRANS-ID                PIC X(4)    VALUE 'EX20'.        EL157
00046      12  THIS-PGM                PIC X(8)    VALUE 'EL157'.       EL157
00047      12  PGM-NAME                PIC X(8).                        EL157
00048      12  TIME-IN                 PIC S9(7).                       EL157
00049      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL157
00050          16  FILLER              PIC X.                           EL157
00051          16  TIME-OUT            PIC 99V99.                       EL157
00052          16  FILLER              PIC X(2).                        EL157
00053      12  XCTL-005                PIC X(5)    VALUE 'EL005'.       EL157
00054      12  XCTL-010                PIC X(5)    VALUE 'EL010'.       EL157
00055      12  XCTL-126                PIC X(5)    VALUE 'EL126'.       EL157
00056      12  XCTL-141                PIC X(5)    VALUE 'EL141'.       EL157
00057      12  LINK-001                PIC X(5)    VALUE 'EL001'.       EL157
00058      12  LINK-004                PIC X(5)    VALUE 'EL004'.       EL157
00059      12  LINK-ELDATCV            PIC X(7)    VALUE 'ELDATCV'.     EL157
00060      12  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL'.      EL157
00061      12  ARCH-ID                 PIC X(8)    VALUE 'ELARCH'.      EL157
00062      12  WS-DEEDIT               PIC X(8).                        EL157
00063      12  WS-DEEDIT-NUM REDEFINES WS-DEEDIT PIC 9(8).              EL157
00064      12  WS-TODAY-DATE           PIC XX      VALUE LOW-VALUES.    EL157
00065      12  WS-SEND-ON-SAVE         PIC XX      VALUE LOW-VALUES.    EL157
00066      12  WS-SEND-SAVE            PIC XX      VALUE LOW-VALUES.    EL157
00067      12  WS-FOLLOW-SAVE          PIC XX      VALUE LOW-VALUES.    EL157
00068      12  ER-0000                 PIC X(4)    VALUE '0000'.        EL157
00069      12  ER-0004                 PIC X(4)    VALUE '0004'.        EL157
00070      12  ER-0008                 PIC X(4)    VALUE '0008'.        EL157
00071      12  ER-0029                 PIC X(4)    VALUE '0029'.        EL157
00072      12  ER-0033                 PIC X(4)    VALUE '0033'.        EL157
00073      12  ER-0070                 PIC X(4)    VALUE '0070'.        EL157
00074      12  ER-0295                 PIC X(4)    VALUE '0295'.        EL157
00075      12  ER-0296                 PIC X(4)    VALUE '0296'.        EL157
00076      12  ER-0532                 PIC X(4)    VALUE '0532'.        EL157
00077      12  ER-0533                 PIC X(4)    VALUE '0533'.        EL157
00078      12  ER-0534                 PIC X(4)    VALUE '0534'.        EL157
00079      12  ER-0535                 PIC X(4)    VALUE '0535'.        EL157
00080      12  ER-0536                 PIC X(4)    VALUE '0536'.        EL157
00081      12  ER-0537                 PIC X(4)    VALUE '0537'.        EL157
00082      12  ER-0550                 PIC X(4)    VALUE '0550'.        EL157
00083      12  ER-0551                 PIC X(4)    VALUE '0551'.        EL157
00084      12  ER-1881                 PIC X(4)    VALUE '1881'.           CL**3
00085                                                                   EL157
00086      12  ERACCT-KEY.                                                 CL**3
00087          16  ACCT-COMP-CD      PIC X(01).                            CL**3
00088          16  ACCT-CARRIER      PIC X(01).                            CL**3
00089          16  ACCT-GROUP        PIC X(06).                            CL**3
00090          16  ACCT-STATE        PIC X(02).                            CL**3
00091          16  ACCT-ACCT-NO      PIC X(10).                            CL**3
00092          16  ACCT-KEY-DATE.                                          CL**3
00093              20  ACCT-EXP-DATE PIC X(02).                            CL**3
00094              20  FILLER        PIC X(04).                            CL**3
00095      12  ELTRLR-KEY.                                              EL157
00096          16  TRLR-COMP-CD      PIC X.                                CL**3
00097          16  TRLR-CARRIER      PIC X.                                CL**3
00098          16  TRLR-CLAIM-NO     PIC X(7).                             CL**3
00099          16  TRLR-CERT-NO      PIC X(11).                            CL**3
00100          16  TRLR-SEQ-NO       PIC S9(4)  COMP.                      CL**3
00101      12  ELMSTR-KEY.                                              EL157
00102          16  MSTR-COMP-CD        PIC X.                           EL157
00103          16  MSTR-CARRIER        PIC X.                           EL157
00104          16  MSTR-CLAIM-NO       PIC X(7).                        EL157
00105          16  MSTR-CERT-NO        PIC X(11).                       EL157
00106      12  CNTL-KEY.                                                EL157
00107          16  CNTL-CO             PIC X(3).                        EL157
00108          16  CNTL-RECORD-TYPE    PIC X       VALUE '1'.           EL157
00109          16  CNTL-GENL.                                           EL157
00110            18 CNTL-GEN1          PIC X(2)    VALUE SPACES.        EL157
00111            18 CNTL-GEN2.                                          EL157
00112              20 CNTL-GEN3        PIC X       VALUE SPACES.        EL157
00113              20 CNTL-GEN4        PIC X       VALUE SPACES.        EL157
00114          16  CNTL-SEQ            PIC S9(4)   VALUE +0    COMP.    EL157
00115      12  ARCH-KEY.                                                EL157
00116          16  ARCH-PARTIAL-KEY.                                    EL157
00117              20  ARCH-CO         PIC X.                           EL157
00118              20  ARCH-NUMBER     PIC S9(8)      COMP.             EL157
00119          16  ARCH-REC-TYPE       PIC X.                           EL157
00120          16  ARCH-SEQ            PIC S9(4)      COMP VALUE +0.    EL157
00121      12  WS-SAVE-KEY.                                             EL157
00122          16  WS-COMP-CD          PIC X.                           EL157
00123          16  WS-CARRIER          PIC X.                           EL157
00124          16  WS-CLAIM            PIC X(7).                        EL157
00125                                                                   EL157
00126      12  BROWSE-SW               PIC 9 VALUE 0.                   EL157
00127      12  ARCH-LENGTH             PIC S9(4)   COMP VALUE +90.         CL**5
00128      12  TRLR-LENGTH             PIC S9(4)   COMP VALUE +200.        CL*10
00129      12  QID.                                                     EL157
00130          16  QID-TERM        PIC X(4).                            EL157
00131          16  FILLER          PIC X(4)    VALUE '157A'.            EL157
00132                                                                   EL157
00133 ***************************************************************   EL157
00134 *    QID-MAP-LENGTH MUST BE ADJUSTED EVERY TIME THAT ANY      *   EL157
00135 *    FIELDS ARE ADDED, DELETED, OR THE SIZE OF ANY FIELD      *   EL157
00136 *    CHANGES WITH IN MAP EL130A.                              *   EL157
00137 ***************************************************************   EL157
00138                                                                   EL157
00139      12  QID-MAP-LENGTH      PIC S9(4)   VALUE +406    COMP.      EL157
00140                                                                   EL157
00141                                                                   EL157
00142      EJECT                                                        EL157
00143                              COPY ELCLOGOF.                          CL*10
00144                                                                   EL157
00145      EJECT                                                        EL157
00146                              COPY ELCATTR.                           CL*10
00147                                                                   EL157
00148      EJECT                                                        EL157
00149                              COPY ELCEMIB.                           CL*10
00150                                                                   EL157
00151      EJECT                                                        EL157
00152                              COPY ELCINTF.                           CL*10
00153      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             EL157
00154          16  PI-RELATED-CLAIMS OCCURS 2 TIMES.                    EL157
00155              20  PI-CARRIER-SAV  PIC X.                           EL157
00156              20  PI-CLAIM-SAV    PIC X(7).                        EL157
00157              20  PI-CERT-SAV     PIC X(11).                       EL157
00158          16  FILLER              PIC X(602).                         CL*11
00159                                                                   EL157
00160      EJECT                                                        EL157
00161                              COPY ELCAID.                            CL*10
00162  01  FILLER    REDEFINES DFHAID.                                  EL157
00163      12  FILLER              PIC X(8).                            EL157
00164      12  PF-VALUES           PIC X       OCCURS 2.                EL157
00165                                                                   EL157
00166      EJECT                                                        EL157
00167                              COPY ELCDATE.                           CL*10
00168      EJECT                                                        EL157
00169                              COPY ELCJPFX.                           CL*10
00170                              PIC X(750).                             CL**9
00171      EJECT                                                        EL157
00172                              COPY EL157S.                            CL*10
00173                                                                   EL157
00174      EJECT                                                        EL157
00175  LINKAGE SECTION.                                                 EL157
00176  01  DFHCOMMAREA             PIC X(1024).                         EL157
00177                                                                   EL157
00178      EJECT                                                        EL157
00179                              COPY ELCMSTR.                           CL*10
00180      EJECT                                                        EL157
00181                              COPY ELCTRLR.                           CL*10
00182      EJECT                                                        EL157
00183                              COPY ELCCNTL.                           CL*10
00184      EJECT                                                        EL157
00185                              COPY ELCARCH.                           CL*10
00186      EJECT                                                           CL**3
00187                              COPY ERCACCT.                           CL*10
00188      EJECT                                                        EL157
00189  PROCEDURE DIVISION.                                              EL157
00190                                                                   EL157
00191      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL157
00192      MOVE '5'                    TO DC-OPTION-CODE.               EL157
00193      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL157
00194      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL157
00195      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL157
00196                                                                   EL157
00197      MOVE 2                      TO EMI-NUMBER-OF-LINES.          EL157
00198      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL157
00199      MOVE EIBTRMID               TO QID-TERM.                     EL157
00200                                                                   EL157
00201      IF EIBCALEN = 0                                              EL157
00202          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL157
00203                                                                   EL157
00204      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL157
00205          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL157
00206              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL157
00207              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL157
00208              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL157
00209              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL157
00210              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL157
00211              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL157
00212              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL157
00213              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL157
00214          ELSE                                                     EL157
00215              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL157
00216              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL157
00217              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL157
00218              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL157
00219              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL157
00220              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL157
00221              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL157
00222              MOVE SPACES               TO PI-SAVED-PROGRAM-6      EL157
00223              GO TO 0600-RECOVER-TEMP-STORAGE.                     EL157
00224                                                                   EL157
00225      EXEC CICS HANDLE CONDITION                                   EL157
00226          PGMIDERR  (9600-PGMID-ERROR)                             EL157
00227          MAPFAIL   (7000-BUILD-SCREEN)                            EL157
00228          QIDERR    (7000-BUILD-SCREEN)                            EL157
00229          ERROR     (9990-ABEND)                                   EL157
00230          END-EXEC.                                                EL157
00231                                                                   EL157
00232      IF EIBTRNID NOT = TRANS-ID                                   EL157
00233         EXEC CICS DELETEQ TS                                      EL157
00234              QUEUE   (QID)                                        EL157
00235              END-EXEC                                             EL157
00236         GO TO 7000-BUILD-SCREEN.                                  EL157
00237                                                                   EL157
00238      IF EIBAID = DFHCLEAR                                         EL157
00239          GO TO 9400-CLEAR.                                        EL157
00240                                                                   EL157
00241      EJECT                                                        EL157
00242  0200-RECEIVE.                                                    EL157
00243      MOVE LOW-VALUES             TO EL157AI.                      EL157
00244      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL157
00245          MOVE ER-0008            TO EMI-ERROR                     EL157
00246          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL157
00247          MOVE -1                 TO FORMTYPL                      EL157
00248          GO TO 8200-SEND-DATAONLY.                                EL157
00249                                                                   EL157
00250      MOVE SAVE-BIN-DATE          TO WS-TODAY-DATE.                EL157
00251                                                                   EL157
00252      EXEC CICS RECEIVE                                            EL157
00253          MAP     (MAP-NAME)                                       EL157
00254          MAPSET  (MAPSET-NAME)                                    EL157
00255          INTO    (EL157AI)                                        EL157
00256          END-EXEC.                                                EL157
00257                                                                   EL157
00258      IF ENTERPFL = 0                                              EL157
00259          GO TO 0300-CHECK-PFKEYS.                                 EL157
00260      IF EIBAID NOT = DFHENTER                                     EL157
00261          MOVE ER-0004               TO EMI-ERROR                  EL157
00262          GO TO 0320-INPUT-ERROR.                                  EL157
00263      IF (ENTERPFI NUMERIC) AND                                    EL157
00264         (ENTERPFI GREATER THAN 0 AND LESS THAN 25)                EL157
00265         MOVE PF-VALUES (ENTERPFI)   TO EIBAID                     EL157
00266      ELSE                                                         EL157
00267         MOVE ER-0029                TO EMI-ERROR                  EL157
00268         GO TO 0320-INPUT-ERROR.                                   EL157
00269                                                                   EL157
00270  0300-CHECK-PFKEYS.                                               EL157
00271      MOVE ' '                       TO PI-ENTRY-CD-1.             EL157
00272      IF EIBAID = DFHPF23                                          EL157
00273          GO TO 8810-PF23.                                         EL157
00274      IF EIBAID = DFHPF24                                          EL157
00275          GO TO 9200-RETURN-MAIN-MENU.                             EL157
00276      IF EIBAID = DFHPF12                                          EL157
00277          GO TO 9500-PF12.                                         EL157
00278      IF EIBAID = DFHPF3                                           EL157
00279          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT          EL157
00280          GO TO 9210-PF3.                                          EL157
00281      IF EIBAID = DFHENTER                                         EL157
00282         GO TO 1000-EDIT-SCREEN.                                   EL157
00283                                                                   EL157
00284      MOVE ER-0029                TO EMI-ERROR.                    EL157
00285  0320-INPUT-ERROR.                                                EL157
00286      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL157
00287      MOVE AL-UNBON               TO ENTERPFA                      EL157
00288      MOVE -1                     TO ENTERPFL.                     EL157
00289      GO TO 8200-SEND-DATAONLY.                                    EL157
00290      EJECT                                                        EL157
00291  0500-CREATE-TEMP-STORAGE.                                        EL157
00292      EXEC CICS WRITEQ TS                                          EL157
00293          QUEUE   (QID)                                            EL157
00294          FROM    (EL157AI)                                        EL157
00295          LENGTH  (QID-MAP-LENGTH)                                 EL157
00296          END-EXEC.                                                EL157
00297                                                                   EL157
00298      EXEC CICS WRITEQ TS                                          EL157
00299          QUEUE   (QID)                                            EL157
00300          FROM    (PROGRAM-INTERFACE-BLOCK)                        EL157
00301          LENGTH  (PI-COMM-LENGTH)                                 EL157
00302          END-EXEC.                                                EL157
00303                                                                   EL157
00304  0599-EXIT.                                                       EL157
00305       EXIT.                                                       EL157
00306      EJECT                                                        EL157
00307  0600-RECOVER-TEMP-STORAGE.                                       EL157
00308      EXEC CICS HANDLE CONDITION                                   EL157
00309          QIDERR  (0690-QIDERR)                                    EL157
00310          END-EXEC.                                                EL157
00311                                                                   EL157
00312      EXEC CICS READQ TS                                           EL157
00313          QUEUE   (QID)                                            EL157
00314          INTO    (EL157AI)                                        EL157
00315          LENGTH  (QID-MAP-LENGTH)                                 EL157
00316          END-EXEC.                                                EL157
00317                                                                   EL157
00318      EXEC CICS READQ TS                                           EL157
00319          QUEUE   (QID)                                            EL157
00320          INTO    (PROGRAM-INTERFACE-BLOCK)                        EL157
00321          LENGTH  (PI-COMM-LENGTH)                                 EL157
00322          END-EXEC.                                                EL157
00323                                                                   EL157
00324      EXEC CICS DELETEQ TS                                         EL157
00325          QUEUE  (QID)                                             EL157
00326          END-EXEC.                                                EL157
00327                                                                   EL157
00328      IF FORMTYPL NOT = ZERO                                       EL157
00329          MOVE AL-UANON           TO FORMTYPA.                     EL157
00330                                                                   EL157
00331      IF SENDONL NOT = ZERO                                        EL157
00332          MOVE AL-UANON           TO SENDONA.                      EL157
00333                                                                   EL157
00334      IF RESENDL NOT = ZERO                                        EL157
00335          MOVE AL-UANON           TO RESENDA.                      EL157
00336                                                                   EL157
00337      IF FOLLOWL NOT = ZERO                                        EL157
00338          MOVE AL-UANON           TO FOLLOWA.                      EL157
00339                                                                   EL157
00340      IF SPEC1L NOT = ZERO                                         EL157
00341          MOVE AL-UANON           TO SPEC1A.                       EL157
00342                                                                   EL157
00343      IF SPEC2L NOT = ZERO                                         EL157
00344          MOVE AL-UANON           TO SPEC2A.                       EL157
00345                                                                   EL157
00346      IF SPEC3L NOT = ZERO                                         EL157
00347          MOVE AL-UANON           TO SPEC3A.                       EL157
00348                                                                   EL157
00349      IF ADDRL NOT EQUAL +0                                           CL**3
00350          MOVE AL-UANON           TO ADDRA.                           CL**3
00351                                                                      CL**3
00352      IF SHORTL NOT EQUAL +0                                          CL**3
00353          MOVE AL-UANON           TO SHORTA.                          CL**3
00354                                                                   EL157
00355      IF CARR1L NOT = ZERO                                         EL157
00356          MOVE AL-UANON           TO CARR1A.                       EL157
00357                                                                   EL157
00358      IF CLAIM1L NOT = ZERO                                        EL157
00359          MOVE AL-UANON           TO CLAIM1A.                      EL157
00360                                                                   EL157
00361      IF CERT1L NOT = ZERO                                         EL157
00362          MOVE AL-UANON           TO CERT1A.                       EL157
00363                                                                   EL157
00364      IF CARR2L NOT = ZERO                                         EL157
00365          MOVE AL-UANON           TO CARR2A.                       EL157
00366                                                                   EL157
00367      IF CLAIM2L NOT = ZERO                                        EL157
00368          MOVE AL-UANON           TO CLAIM2A.                      EL157
00369                                                                   EL157
00370      IF CERT2L NOT = ZERO                                         EL157
00371          MOVE AL-UANON           TO CERT2A.                       EL157
00372                                                                   EL157
00373      GO TO 8100-SEND-INITIAL-MAP.                                 EL157
00374                                                                   EL157
00375                                                                   EL157
00376  0690-QIDERR.                                                     EL157
00377      MOVE ER-0033                TO EMI-ERROR.                    EL157
00378      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL157
00379      GO TO 8100-SEND-INITIAL-MAP.                                 EL157
00380      EJECT                                                        EL157
00381  1000-EDIT-SCREEN.                                                EL157
00382      IF FORMTYPI = 'I' OR 'P'                                     EL157
00383         NEXT SENTENCE                                             EL157
00384      ELSE                                                         EL157
00385         MOVE ER-0532             TO EMI-ERROR                     EL157
00386         MOVE -1                  TO FORMTYPL                      EL157
00387         MOVE AL-UABON            TO FORMTYPA                      EL157
00388         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL157
00389                                                                   EL157
00390      IF SENDONL NOT = 0                                           EL157
00391         MOVE SENDONI             TO WS-DEEDIT                     EL157
00392         EXEC CICS BIF DEEDIT                                      EL157
00393              FIELD   (WS-DEEDIT)                                  EL157
00394              LENGTH  (8)                                          EL157
00395              END-EXEC                                             EL157
00396         MOVE WS-DEEDIT-NUM       TO DC-GREG-DATE-1-MDY            EL157
00397         MOVE '4'                 TO DC-OPTION-CODE                EL157
00398         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             EL157
00399         IF DATE-CONVERSION-ERROR                                  EL157
00400            MOVE ER-0550          TO EMI-ERROR                     EL157
00401            MOVE -1               TO SENDONL                       EL157
00402            MOVE AL-UABON         TO SENDONA                       EL157
00403            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL157
00404            ELSE                                                   EL157
00405            MOVE AL-UANON         TO SENDONA                       EL157
00406            MOVE DC-BIN-DATE-1    TO WS-SEND-ON-SAVE               EL157
00407            MOVE DC-GREG-DATE-1-EDIT  TO SENDONO                   EL157
00408         ELSE                                                      EL157
00409         MOVE WS-TODAY-DATE       TO WS-SEND-ON-SAVE               EL157
00410         MOVE SAVE-DATE           TO SENDONO.                      EL157
00411                                                                   EL157
00412      IF WS-SEND-ON-SAVE NOT = LOW-VALUES                          EL157
00413         IF WS-SEND-ON-SAVE LESS THAN WS-TODAY-DATE                EL157
00414            MOVE ER-0551             TO EMI-ERROR                  EL157
00415            MOVE -1                  TO SENDONL                    EL157
00416            MOVE AL-UNBON            TO SENDONA                    EL157
00417            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL157
00418                                                                   EL157
00419      IF RESENDL NOT = 0                                           EL157
00420         MOVE RESENDI             TO WS-DEEDIT                     EL157
00421         EXEC CICS BIF DEEDIT                                      EL157
00422              FIELD   (WS-DEEDIT)                                  EL157
00423              LENGTH  (8)                                          EL157
00424              END-EXEC                                             EL157
00425         MOVE WS-DEEDIT-NUM       TO DC-GREG-DATE-1-MDY            EL157
00426         MOVE '4'                 TO DC-OPTION-CODE                EL157
00427         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             EL157
00428         IF DATE-CONVERSION-ERROR                                  EL157
00429            MOVE ER-0295          TO EMI-ERROR                     EL157
00430            MOVE -1               TO RESENDL                       EL157
00431            MOVE AL-UABON         TO RESENDA                       EL157
00432            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL157
00433            ELSE                                                   EL157
00434            MOVE AL-UANON         TO RESENDA                       EL157
00435            MOVE DC-BIN-DATE-1    TO WS-SEND-SAVE                  EL157
00436            MOVE DC-GREG-DATE-1-EDIT  TO RESENDO.                  EL157
00437                                                                   EL157
00438      IF WS-SEND-SAVE NOT = LOW-VALUES                             EL157
00439         IF WS-SEND-SAVE LESS THAN WS-TODAY-DATE                   EL157
00440            MOVE ER-0537             TO EMI-ERROR                  EL157
00441            MOVE -1                  TO RESENDL                    EL157
00442            MOVE AL-UNBON            TO RESENDA                    EL157
00443            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL157
00444                                                                   EL157
00445      IF FOLLOWL NOT = 0                                           EL157
00446         MOVE FOLLOWI             TO WS-DEEDIT                     EL157
00447         EXEC CICS BIF DEEDIT                                      EL157
00448              FIELD   (WS-DEEDIT)                                  EL157
00449              LENGTH  (8)                                          EL157
00450              END-EXEC                                             EL157
00451         MOVE WS-DEEDIT-NUM       TO DC-GREG-DATE-1-MDY            EL157
00452         MOVE '4'                 TO DC-OPTION-CODE                EL157
00453         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             EL157
00454         IF DATE-CONVERSION-ERROR                                  EL157
00455            MOVE ER-0296          TO EMI-ERROR                     EL157
00456            MOVE -1               TO FOLLOWL                       EL157
00457            MOVE AL-UABON         TO FOLLOWA                       EL157
00458            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL157
00459            ELSE                                                   EL157
00460            MOVE AL-UANON         TO FOLLOWA                       EL157
00461            MOVE DC-BIN-DATE-1    TO WS-FOLLOW-SAVE                EL157
00462            MOVE DC-GREG-DATE-1-EDIT  TO FOLLOWO.                  EL157
00463                                                                   EL157
00464      IF WS-FOLLOW-SAVE NOT = LOW-VALUES                           EL157
00465         IF WS-FOLLOW-SAVE NOT GREATER THAN WS-TODAY-DATE          EL157
00466            MOVE ER-0533             TO EMI-ERROR                  EL157
00467            MOVE -1                  TO FOLLOWL                    EL157
00468            MOVE AL-UNBON            TO FOLLOWA                    EL157
00469            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL157
00470                                                                   EL157
00471      IF FORMTYPI NOT = 'P' AND                                    EL157
00472         (SPEC1L NOT = ZEROS  OR                                   EL157
00473          SPEC2L NOT = ZEROS  OR                                   EL157
00474          SPEC3L NOT = ZEROS)                                      EL157
00475         MOVE ER-0534             TO EMI-ERROR                     EL157
00476         MOVE -1                  TO FORMTYPL                      EL157
00477         MOVE AL-UABON            TO FORMTYPA                      EL157
00478         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL157
00479                                                                   EL157
00480      IF SHORTL GREATER THAN +0                                       CL**6
00481          IF SHORTI EQUAL ' ' OR 'L' OR 'S'                           CL**6
00482             NEXT SENTENCE                                            CL**3
00483          ELSE                                                        CL**3
00484             MOVE ER-1881             TO EMI-ERROR                    CL**3
00485             MOVE -1                  TO SHORTL                       CL**3
00486             MOVE AL-UABON            TO SHORTA                       CL**3
00487             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 CL**3
00488      ELSE                                                            CL**6
00489          MOVE 'L' TO SHORTI.                                         CL**6
00490                                                                      CL**3
00491      IF ADDRI EQUAL 'I' OR 'A' OR 'O' OR 'Q'                         CL**3
00492         NEXT SENTENCE                                                CL**3
00493      ELSE                                                            CL**3
00494         MOVE ER-0535             TO EMI-ERROR                     EL157
00495         MOVE -1                  TO ADDRL                         EL157
00496         MOVE AL-UABON            TO ADDRA                            CL**5
00497         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL157
00498         GO TO 8200-SEND-DATAONLY.                                 EL157
00499                                                                   EL157
00500      MOVE CLAIM1I                TO PI-CLAIM-SAV   (1)            EL157
00501      MOVE CARR1I                 TO PI-CARRIER-SAV (1)            EL157
00502      MOVE CERT1I                 TO PI-CERT-SAV    (1)            EL157
00503                                                                   EL157
00504      MOVE CLAIM2I                TO PI-CLAIM-SAV   (2)            EL157
00505      MOVE CARR2I                 TO PI-CARRIER-SAV (2)            EL157
00506      MOVE CERT2I                 TO PI-CERT-SAV    (2)            EL157
00507                                                                   EL157
00508      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD                  EL157
00509                                     ARCH-CO                       EL157
00510      MOVE PI-COMPANY-ID          TO CNTL-CO                       EL157
00511                                                                      CL**8
00512      MOVE PI-CARRIER             TO MSTR-CARRIER.                    CL**8
00513      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.                EL157
00514      MOVE PI-CERT-NO             TO MSTR-CERT-NO.                 EL157
00515                                                                   EL157
00516      PERFORM 7900-READ-CLAIM THRU 7900-EXIT                       EL157
00517      IF (ADDRI = 'I'  AND  CL-INSURED-ADDR-CNT EQUAL +0) OR          CL**3
00518         (ADDRI = 'O'  AND  CL-OTHER-1-ADDR-CNT EQUAL +0) OR          CL**3
00519         (ADDRI = 'Q'  AND  CL-OTHER-2-ADDR-CNT EQUAL +0)             CL**3
00520          MOVE ER-0536            TO EMI-ERROR                     EL157
00521          MOVE -1                 TO ADDRL                         EL157
00522          MOVE AL-UABON           TO ADDRA                            CL**5
00523          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL157
00524                                                                   EL157
00525      IF NOT EMI-NO-ERRORS                                         EL157
00526         GO TO 8200-SEND-DATAONLY.                                 EL157
00527                                                                   EL157
00528      EJECT                                                        EL157
00529  5000-UPDATE.                                                     EL157
00530                                                                   EL157
00531      IF NOT MODIFY-CAP                                            EL157
00532         MOVE ER-0070             TO EMI-ERROR                     EL157
00533         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL157
00534         MOVE -1                  TO FORMTYPL                      EL157
00535         GO TO 8200-SEND-DATAONLY.                                 EL157
00536                                                                   EL157
00537      PERFORM 7910-READ-CLAIM-UPDATE THRU 7910-EXIT.               EL157
00538                                                                   EL157
00539      SUBTRACT +1                 FROM CL-TRAILER-SEQ-CNT.            CL**5
00540                                                                      CL**3
00541      MOVE LOW-VALUES       TO ACCT-FOUND-SW                          CL**3
00542                                                                      CL**3
00543      IF (PI-COMPANY-ID EQUAL 'LGX' OR 'CRI')                         CL**3
00544        AND                                                           CL**3
00545         FORMTYPI EQUAL 'I'                                           CL**3
00546         PERFORM 6000-READ-ACCOUNT-MASTER THRU 6099-EXIT.             CL**3
00547                                                                   EL157
00548      PERFORM 6100-BUILD-FORM-TRAILER THRU 6199-EXIT.              EL157
00549      PERFORM 6200-BUILD-ARCHIVE-HEADER THRU 6299-EXIT.            EL157
00550                                                                   EL157
00551      MOVE WS-TODAY-DATE          TO CL-LAST-MAINT-DT              EL157
00552      MOVE PI-PROCESSOR-ID        TO CL-LAST-MAINT-USER            EL157
00553      MOVE EIBTIME                TO CL-LAST-MAINT-HHMMSS.         EL157
00554      MOVE '2'                    TO CL-LAST-MAINT-TYPE.           EL157
00555                                                                      CL**5
00556      IF SHORTI NOT = 'S'                                             CL**6
00557         MOVE 'L'                 TO CL-PROG-FORM-TYPE                CL**6
00558      ELSE                                                            CL**5
00559         MOVE SHORTI              TO CL-PROG-FORM-TYPE.               CL**6
00560                                                                   EL157
00561      IF WS-FOLLOW-SAVE GREATER THAN CL-NEXT-FOLLOWUP-DT           EL157
00562         MOVE WS-FOLLOW-SAVE      TO CL-NEXT-FOLLOWUP-DT.          EL157
00563                                                                   EL157
00564      IF WS-SEND-ON-SAVE GREATER THAN CL-NEXT-FOLLOWUP-DT          EL157
00565         MOVE WS-SEND-ON-SAVE     TO CL-NEXT-FOLLOWUP-DT.          EL157
00566                                                                   EL157
00567      IF WS-SEND-SAVE GREATER THAN CL-NEXT-FOLLOWUP-DT             EL157
00568         MOVE WS-SEND-SAVE        TO CL-NEXT-FOLLOWUP-DT.          EL157
00569                                                                   EL157
00570      EXEC CICS HANDLE CONDITION                                   EL157
00571          DUPKEY (5010-REWRITE-CLAIM)                              EL157
00572          END-EXEC.                                                EL157
00573                                                                   EL157
00574      EXEC CICS REWRITE                                            EL157
00575           DATASET  ('ELMSTR')                                     EL157
00576           FROM     (CLAIM-MASTER)                                 EL157
00577           END-EXEC.                                               EL157
00578  5010-REWRITE-CLAIM.                                              EL157
00579                                                                   EL157
00580      MOVE ER-0000                TO EMI-ERROR                     EL157
00581      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL157
00582      MOVE LOW-VALUE              TO EL157AI                       EL157
00583      MOVE PI-CARRIER-SAV (1)     TO CARR1O                        EL157
00584      MOVE PI-CLAIM-SAV   (1)     TO CLAIM1O                       EL157
00585      MOVE PI-CERT-SAV    (1)     TO CERT1O                        EL157
00586      MOVE PI-CARRIER-SAV (2)     TO CARR2O                        EL157
00587      MOVE PI-CLAIM-SAV   (2)     TO CLAIM2O                       EL157
00588      MOVE PI-CERT-SAV    (2)     TO CERT2O.                          CL**2
00589                                                                   EL157
CIDMOD     GO TO 5025-CHECK-RELATED-CLAIM.                                   000
CIDMOD                                                                       000
CIDMOD     MOVE LOW-VALUES         TO ELMSTR-KEY                             000
CIDMOD     MOVE PI-COMPANY-CD      TO MSTR-COMP-CD                           000
CIDMOD     MOVE PI-CARRIER-SAV (1) TO MSTR-CARRIER                           000
CIDMOD     MOVE PI-CLAIM-SAV (1)   TO MSTR-CLAIM-NO                          000
CIDMOD     MOVE PI-CERT-SAV (1)    TO MSTR-CERT-NO                           000
CIDMOD                                                                       000
CIDMOD     EXEC CICS HANDLE CONDITION                                        000
CIDMOD          NOTFND  (5025-CHECK-RELATED-CLAIM)                           000
CIDMOD          END-EXEC.                                                    000
CIDMOD                                                                       000
CIDMOD     PERFORM 5100-UPDATE-RELATED-CLAIMS THRU 5100-EXIT.                000
CIDMOD                                                                       000
CIDMOD 5025-CHECK-RELATED-CLAIM.                                             000
CIDMOD                                                                       000
CIDMOD     GO TO 5050-EXIT.                                                  000
CIDMOD                                                                       000
CIDMOD     MOVE LOW-VALUES         TO ELMSTR-KEY                             000
CIDMOD     MOVE PI-COMPANY-CD      TO MSTR-COMP-CD                           000
CIDMOD     MOVE PI-CARRIER-SAV (2) TO MSTR-CARRIER                           000
CIDMOD     MOVE PI-CLAIM-SAV (2)   TO MSTR-CLAIM-NO                          000
CIDMOD     MOVE PI-CERT-SAV (2)    TO MSTR-CERT-NO                           000
CIDMOD                                                                       000
CIDMOD     EXEC CICS HANDLE CONDITION                                        000
CIDMOD          NOTFND  (5050-EXIT)                                          000
CIDMOD          END-EXEC.                                                    000
CIDMOD                                                                       000
CIDMOD     PERFORM 5100-UPDATE-RELATED-CLAIMS THRU 5100-EXIT.                000
CIDMOD                                                                       000
CIDMOD 5050-EXIT.                                                            000
CIDMOD
CIDMOD     GO TO 8100-SEND-INITIAL-MAP.                                 EL157
CIDMOD                                                                  EL157
CIDMOD     EJECT                                                             000
CIDMOD                                                                       000
CIDMOD 5100-UPDATE-RELATED-CLAIMS.                                           000
CIDMOD     PERFORM 7910-READ-CLAIM-UPDATE THRU 7910-EXIT.                    000
CIDMOD                                                                       000
CIDMOD     MOVE WS-TODAY-DATE          TO CL-LAST-MAINT-DT                   000
CIDMOD     MOVE PI-PROCESSOR-ID        TO CL-LAST-MAINT-USER                 000
CIDMOD     MOVE EIBTIME                TO CL-LAST-MAINT-HHMMSS.              000
CIDMOD     MOVE '2'                    TO CL-LAST-MAINT-TYPE.                000
CIDMOD     MOVE 'O'                    TO CL-CLAIM-STATUS.                   000
CIDMOD                                                                       000
CIDMOD     EXEC CICS HANDLE CONDITION                                        000
CIDMOD         DUPKEY  (5100-EXIT)                                           000
CIDMOD         END-EXEC.                                                     000
CIDMOD                                                                       000
CIDMOD     EXEC CICS REWRITE                                                 000
CIDMOD          DATASET  ('ELMSTR')                                          000
CIDMOD          FROM     (CLAIM-MASTER)                                      000
CIDMOD          END-EXEC.                                                    000
CIDMOD                                                                       000
CIDMOD 5100-EXIT.                                                            000
CIDMOD     EXIT.                                                             000
00592      EJECT                                                        EL157
00593                                                                   EL157
00594  6000-READ-ACCOUNT-MASTER.                                           CL**3
00595                                                                      CL**3
00596      EXEC CICS HANDLE CONDITION                                      CL**3
00597           NOTFND     (6099-EXIT)                                     CL**3
00598           ENDFILE    (6099-EXIT)                                     CL**3
00599      END-EXEC.                                                       CL**3
00600                                                                      CL**3
00601      MOVE LOW-VALUES       TO ERACCT-KEY                             CL**3
00602      MOVE CL-COMPANY-CD    TO ACCT-COMP-CD                           CL**3
00603      MOVE CL-CERT-CARRIER  TO ACCT-CARRIER                           CL**3
00604      MOVE CL-CERT-GROUPING TO ACCT-GROUP                             CL**3
00605      MOVE CL-CERT-STATE    TO ACCT-STATE                             CL**3
00606      MOVE CL-CERT-ACCOUNT  TO ACCT-ACCT-NO                           CL**3
00607      MOVE CL-CERT-EFF-DT   TO ACCT-EXP-DATE                          CL**3
00608                                                                      CL**3
00609      EXEC CICS READ                                                  CL**3
00610           DATASET  ('ERACCT')                                        CL**3
00611           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL*11
00612           RIDFLD   (ERACCT-KEY)                                      CL**3
00613           GTEQ                                                       CL**3
00614      END-EXEC.                                                       CL**3
00615                                                                      CL**3
00616      IF AM-COMPANY-CD EQUAL CL-COMPANY-CD    AND                     CL**3
00617         AM-CARRIER EQUAL CL-CERT-CARRIER     AND                     CL**3
00618         AM-GROUPING EQUAL CL-CERT-GROUPING   AND                     CL**3
00619         AM-STATE EQUAL CL-CERT-STATE         AND                     CL**3
00620         AM-ACCOUNT EQUAL CL-CERT-ACCOUNT     AND                     CL**3
00621         AM-EXPIRATION-DT GREATER THAN CL-CERT-EFF-DT                 CL**3
00622         MOVE 'F'  TO ACCT-FOUND-SW                                   CL**5
00623      ELSE                                                            CL**6
00624         GO TO 6099-EXIT.                                             CL**5
00625                                                                      CL**5
00626      IF (PI-COMPANY-ID = 'LGX'  OR  'CRI')  AND                      CL**5
00627         (AM-EMPLOYER-STMT-USED = '1' OR '2' OR '3')                  CL**5
00628          NEXT SENTENCE                                               CL**5
00629      ELSE                                                            CL**5
00630          GO TO 6099-EXIT.                                            CL**5
00631                                                                      CL**5
00632      MOVE CL-CERT-EFF-DT             TO DC-BIN-DATE-1.               CL**5
00633      IF PI-COMPANY-ID = 'CRI'                                        CL**5
00634          IF CL-CERT-EFF-DT = CL-INCURRED-DT  AND                     CL**5
00635             CL-LAST-ADD-ON-DT NOT = LOW-VALUES  AND                  CL**5
00636             (O-B-COVERAGE  OR  OPEN-END-COVERAGE)                    CL**5
00637              MOVE CL-LAST-ADD-ON-DT  TO DC-BIN-DATE-1.               CL**5
00638                                                                      CL**5
00639      MOVE CL-INCURRED-DT             TO DC-BIN-DATE-2.               CL**5
00640      MOVE '1'                        TO DC-OPTION-CODE.              CL**5
00641      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL**5
00642                                                                      CL**5
00643      IF (AM-EMPLOYER-STMT-USED = '1'  AND                            CL**5
00644          DC-ELAPSED-DAYS LESS THAN +31)  OR                          CL**5
00645         (AM-EMPLOYER-STMT-USED = '2'  AND                            CL**5
00646          DC-ELAPSED-DAYS LESS THAN +61)  OR                          CL**5
00647         (AM-EMPLOYER-STMT-USED = '3'  AND                            CL**5
00648          DC-ELAPSED-DAYS LESS THAN +91)                              CL**5
00649          MOVE 'Y'                    TO AM-EMPLOYER-STMT-USED.       CL**5
00650                                                                      CL**3
00651  6099-EXIT.                                                          CL**3
00652       EXIT.                                                          CL**3
00653                                                                      CL**3
00654  6100-BUILD-FORM-TRAILER.                                         EL157
00655                                                                      CL**3
00656      EXEC CICS GETMAIN                                            EL157
00657           SET      (ADDRESS OF ACTIVITY-TRAILERS)                    CL*11
00658           LENGTH   (TRLR-LENGTH)                                     CL*10
00659           INITIMG  (GETMAIN-SPACE)                                EL157
00660      END-EXEC.                                                       CL**3
00661                                                                   EL157
00662      MOVE ELMSTR-KEY             TO AT-CONTROL-PRIMARY            EL157
00663      MOVE 'AT'                   TO AT-RECORD-ID                  EL157
00664      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO                EL157
00665      MOVE 'A'                    TO AT-TRAILER-TYPE               EL157
00666      MOVE WS-TODAY-DATE          TO AT-RECORDED-DT                   CL**3
00667                                     AT-FORM-LAST-MAINT-DT            CL**3
00668      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                EL157
00669                                     AT-FORM-LAST-UPDATED-BY          CL**3
00670      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS          EL157
00671      MOVE WS-SEND-ON-SAVE        TO AT-FORM-SEND-ON-DT            EL157
00672      MOVE WS-FOLLOW-SAVE         TO AT-FORM-FOLLOW-UP-DT          EL157
00673      MOVE WS-SEND-SAVE           TO AT-FORM-RE-SEND-DT            EL157
00674      MOVE LOW-VALUES             TO AT-FORM-ANSWERED-DT           EL157
00675                                     AT-EMP-FORM-ANSWERED-DT          CL**3
00676                                     AT-PHY-FORM-ANSWERED-DT          CL**3
00677                                     AT-EMP-FORM-SEND-ON-DT           CL**3
00678                                     AT-PHY-FORM-SEND-ON-DT           CL**3
00679                                     AT-FORM-PRINTED-DT            EL157
00680                                     AT-FORM-REPRINT-DT.           EL157
00681      IF FORMTYPI = 'P'                                            EL157
00682         MOVE '2'                 TO AT-FORM-TYPE                  EL157
00683      ELSE                                                         EL157
00684         MOVE '1'                 TO AT-FORM-TYPE.                 EL157
00685                                                                   EL157
00686      MOVE SPEC1I                 TO AT-INSTRUCT-LN-1              EL157
00687      MOVE SPEC2I                 TO AT-INSTRUCT-LN-2              EL157
00688      MOVE SPEC3I                 TO AT-INSTRUCT-LN-3              EL157
00689                                                                   EL157
00690      IF AT-INSTRUCT-LN-3 = LOW-VALUES                             EL157
00691         MOVE SPACES              TO AT-INSTRUCT-LN-3.             EL157
00692                                                                   EL157
00693      IF AT-INSTRUCT-LN-2 = SPACES OR LOW-VALUES                   EL157
00694         MOVE AT-INSTRUCT-LN-3    TO AT-INSTRUCT-LN-2              EL157
00695         MOVE SPACES              TO AT-INSTRUCT-LN-3.             EL157
00696                                                                   EL157
00697      IF AT-INSTRUCT-LN-1 = SPACES OR LOW-VALUES                   EL157
00698         MOVE AT-INSTRUCT-LN-2    TO AT-INSTRUCT-LN-1              EL157
00699         MOVE AT-INSTRUCT-LN-3    TO AT-INSTRUCT-LN-2              EL157
00700         MOVE SPACES              TO AT-INSTRUCT-LN-3.             EL157
00701                                                                   EL157
00702      IF ADDRI EQUAL 'I'                                              CL**3
00703         MOVE CL-INSURED-ADDR-CNT  TO AT-FORM-ADDR-SEQ-NO             CL**3
00704         MOVE 'I'                  TO AT-FORM-ADDRESS.                CL**3
00705                                                                      CL**3
00706      IF ADDRI EQUAL 'A'                                              CL**3
00707         MOVE CL-ACCOUNT-ADDR-CNT  TO AT-FORM-ADDR-SEQ-NO             CL**3
00708         ADD +20 TO AT-FORM-ADDR-SEQ-NO                               CL**3
00709         MOVE 'A'                  TO AT-FORM-ADDRESS.                CL**3
00710                                                                      CL**3
00711      IF ADDRI EQUAL 'O'                                              CL**3
00712         MOVE CL-OTHER-1-ADDR-CNT  TO AT-FORM-ADDR-SEQ-NO             CL**3
00713         ADD +50 TO AT-FORM-ADDR-SEQ-NO                               CL**3
00714         MOVE 'O'                  TO AT-FORM-ADDRESS.                CL**3
00715                                                                      CL**3
00716      IF ADDRI EQUAL 'Q'                                              CL**3
00717         MOVE CL-OTHER-2-ADDR-CNT  TO AT-FORM-ADDR-SEQ-NO             CL**3
00718         ADD +60 TO AT-FORM-ADDR-SEQ-NO                               CL**3
00719         MOVE 'Q'                  TO AT-FORM-ADDRESS.                CL**3
00720                                                                      CL**3
00721      IF (PI-COMPANY-ID EQUAL 'LGX' OR 'CRI')                         CL**3
00722         IF (FORMTYPI EQUAL 'I')                                      CL**3
00723            MOVE WS-SEND-ON-SAVE  TO AT-PHY-FORM-SEND-ON-DT           CL**3
00724            IF ACCT-FOUND-SW EQUAL 'F'                                CL**3
00725             AND                                                      CL**3
00726               AM-EMPLOYER-STMT-USED EQUAL 'Y'                        CL**3
00727               MOVE WS-SEND-ON-SAVE  TO AT-EMP-FORM-SEND-ON-DT        CL**3
00728            ELSE                                                      CL**3
00729               NEXT SENTENCE                                          CL**3
00730         ELSE                                                         CL**3
00731         IF (FORMTYPI EQUAL 'P')                                      CL**3
00732          AND                                                         CL**3
00733            (SHORTI NOT EQUAL 'S')                                    CL**6
00734            MOVE WS-SEND-ON-SAVE TO AT-PHY-FORM-SEND-ON-DT.           CL**3
00735                                                                   EL157
00736      MOVE PI-RELATED-CLAIMS (1)  TO AT-RELATED-1                  EL157
00737      MOVE PI-RELATED-CLAIMS (2)  TO AT-RELATED-2                  EL157
00738                                                                   EL157
00739      EXEC CICS WRITE                                              EL157
00740           DATASET  ('ELTRLR')                                     EL157
00741           FROM     (ACTIVITY-TRAILERS)                            EL157
00742           RIDFLD   (AT-CONTROL-PRIMARY)                           EL157
00743           END-EXEC.                                               EL157
00744                                                                   EL157
00745  6199-EXIT.                                                       EL157
00746       EXIT.                                                       EL157
00747                                                                   EL157
00748      EJECT                                                        EL157
00749  6200-BUILD-ARCHIVE-HEADER.                                       EL157
00750      MOVE '1'                    TO CNTL-RECORD-TYPE              EL157
00751      MOVE ZEROS                  TO CNTL-SEQ                      EL157
00752      MOVE SPACES                 TO CNTL-GENL                     EL157
00753      EXEC CICS READ                                               EL157
00754           DATASET  (CNTL-ID)                                      EL157
00755           SET      (ADDRESS OF CONTROL-FILE)                         CL*11
00756           RIDFLD   (CNTL-KEY)                                     EL157
00757           UPDATE                                                  EL157
00758           END-EXEC.                                               EL157
00759                                                                      CL*10
00760      ADD 1                       TO CF-CO-ARCHIVE-COUNTER.        EL157
00761      MOVE CF-CO-ARCHIVE-COUNTER  TO ARCH-NUMBER.                  EL157
00762                                                                      CL*10
00763      EXEC CICS REWRITE                                            EL157
00764           FROM     (CONTROL-FILE)                                 EL157
00765           DATASET  (CNTL-ID)                                      EL157
00766           END-EXEC.                                               EL157
00767                                                                   EL157
00768      EXEC CICS HANDLE CONDITION                                   EL157
00769           NOTOPEN  (9990-ABEND)                                   EL157
00770           DUPKEY   (6299-EXIT)                                       CL*10
00771           END-EXEC.                                               EL157
00772                                                                   EL157
00773      EXEC CICS GETMAIN                                            EL157
00774           SET     (ADDRESS OF LETTER-ARCHIVE)                        CL*11
00775           LENGTH  (ARCH-LENGTH)                                   EL157
00776           END-EXEC.                                                  CL*11
00777                                                                      CL*11
00778      MOVE SPACES                 TO LETTER-ARCHIVE.               EL157
00779      MOVE 'LA'                   TO LA-RECORD-ID                  EL157
00780      MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO                 EL157
00781                                     LA-ARCHIVE-NO-A1              EL157
00782      MOVE '4'                    TO LA-RECORD-TYPE                EL157
00783                                     LA-RECORD-TYPE-A1             EL157
00784      MOVE ZEROS                  TO LA-LINE-SEQ-NO                EL157
00785                                     LA-LINE-SEQ-NO-A1             EL157
00786      MOVE ARCH-CO                TO LA-COMPANY-CD                 EL157
00787                                     LA-COMPANY-CD-A1              EL157
00788      MOVE MSTR-CARRIER           TO LA4-CARRIER                   EL157
00789      MOVE PI-CLAIM-NO            TO LA4-CLAIM-NO                  EL157
00790      MOVE PI-CERT-NO             TO LA4-CERT-NO                   EL157
00791      MOVE PI-STATE               TO LA4-STATE.                       CL**7
00792      MOVE  ZEROS                 TO LA4-NO-OF-COPIES.             EL157
00793      MOVE WS-SEND-SAVE           TO LA4-RESEND-DATE               EL157
00794      MOVE PI-PROCESSOR-ID        TO LA4-PROCESSOR-CD              EL157
00795      MOVE WS-TODAY-DATE          TO LA4-CREATION-DT               EL157
00796      MOVE LOW-VALUES             TO LA4-INITIAL-PRINT-DATE        EL157
00797      MOVE LOW-VALUES             TO LA4-RESEND-PRINT-DATE         EL157
00798      MOVE LOW-VALUES             TO LA4-FORM-REM-PRINT-DT            CL**3
00799      MOVE CL-TRAILER-SEQ-CNT     TO LA4-FORM-TRLR-SEQ             EL157
00800      IF FORMTYPI EQUAL 'P'                                           CL**3
00801         MOVE '2'                 TO LA4-FORM-TYPE                 EL157
00802      ELSE                                                         EL157
00803         MOVE '1'                 TO LA4-FORM-TYPE.                EL157
00804                                                                   EL157
00805      EXEC CICS WRITE                                              EL157
00806           DATASET  (ARCH-ID)                                      EL157
00807           FROM     (LETTER-ARCHIVE)                               EL157
00808           RIDFLD   (LA-CONTROL-PRIMARY)                           EL157
00809           END-EXEC.                                               EL157
00810                                                                   EL157
00811  6299-EXIT.                                                       EL157
00812       EXIT.                                                       EL157
00813                                                                   EL157
00814      EJECT                                                        EL157
00815  7000-BUILD-SCREEN.                                               EL157
00816      MOVE LOW-VALUES             TO EL157AI                       EL157
00817      MOVE SPACES                 TO PI-RELATED-CLAIMS (1)         EL157
00818                                     PI-RELATED-CLAIMS (2).        EL157
00819                                                                   EL157
00820      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD                  EL157
00821      MOVE LOW-VALUES             TO MSTR-CERT-NO                  EL157
00822      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO                 EL157
00823      MOVE PI-CARRIER             TO MSTR-CARRIER.                    CL**8
00824                                                                      CL**8
00825      MOVE ELMSTR-KEY             TO WS-SAVE-KEY                   EL157
00826      EXEC CICS HANDLE CONDITION                                   EL157
00827           NOTFND   (7050-END-BROWSE)                              EL157
00828           ENDFILE  (7050-END-BROWSE)                              EL157
00829           DUPKEY   (7020-CHECK-KEY)                               EL157
00830           END-EXEC.                                               EL157
00831      EXEC CICS STARTBR                                            EL157
00832          DATASET  ('ELMSTR')                                      EL157
00833          RIDFLD   (ELMSTR-KEY)                                    EL157
00834          KEYLENGTH (9)                                            EL157
00835          GENERIC                                                  EL157
00836          END-EXEC.                                                EL157
00837                                                                   EL157
00838      MOVE 1                      TO BROWSE-SW.                    EL157
00839                                                                   EL157
00840  7010-READNEXT.                                                   EL157
00841      EXEC CICS READNEXT                                           EL157
00842          DATASET  ('ELMSTR')                                      EL157
00843          SET      (ADDRESS OF CLAIM-MASTER)                          CL*11
00844          RIDFLD   (ELMSTR-KEY)                                    EL157
00845          END-EXEC.                                                EL157
00846                                                                   EL157
00847  7020-CHECK-KEY.                                                  EL157
00848                                                                      CL*11
00849      IF WS-COMP-CD NOT = MSTR-COMP-CD OR                          EL157
00850         WS-CARRIER NOT = MSTR-CARRIER OR                          EL157
00851         WS-CLAIM   NOT = MSTR-CLAIM-NO                            EL157
00852         GO TO 7050-END-BROWSE.                                    EL157
00853                                                                   EL157
00854      IF CL-CERT-NO = PI-CERT-NO                                   EL157
00855         MOVE CL-PROG-FORM-TYPE   TO SHORTI                           CL**6
00856         GO TO 7010-READNEXT.                                      EL157
00857                                                                   EL157
00858      IF CLAIM-IS-CLOSED                                              CL**4
00859         GO TO 7010-READNEXT.                                         CL**4
00860                                                                      CL**4
00861      IF BROWSE-SW = 1                                             EL157
00862         MOVE CL-CLAIM-NO         TO PI-CLAIM-SAV (1)              EL157
00863                                     CLAIM1I                       EL157
00864         MOVE CL-CARRIER          TO PI-CARRIER-SAV (1)            EL157
00865                                     CARR1I                        EL157
00866         MOVE CL-CERT-NO          TO PI-CERT-SAV (1)               EL157
00867                                     CERT1I                        EL157
00868         ADD 1                    TO BROWSE-SW                     EL157
00869         GO TO 7010-READNEXT.                                      EL157
00870                                                                   EL157
00871      MOVE CL-CLAIM-NO         TO PI-CLAIM-SAV (2)                 EL157
00872                                  CLAIM2I                          EL157
00873      MOVE CL-CARRIER          TO PI-CARRIER-SAV (2)               EL157
00874                                  CARR2I                           EL157
00875      MOVE CL-CERT-NO          TO PI-CERT-SAV (2)                  EL157
00876                                  CERT2I.                          EL157
00877                                                                   EL157
00878  7050-END-BROWSE.                                                 EL157
00879      IF BROWSE-SW NOT = 0                                         EL157
00880         EXEC CICS ENDBR                                           EL157
00881              DATASET  ('ELMSTR')                                  EL157
00882              END-EXEC.                                            EL157
00883                                                                   EL157
00884      GO TO 8100-SEND-INITIAL-MAP.                                 EL157
00885      EJECT                                                        EL157
00886  7900-READ-CLAIM.                                                 EL157
00887      EXEC CICS READ                                               EL157
00888          DATASET  ('ELMSTR')                                      EL157
00889          SET      (ADDRESS OF CLAIM-MASTER)                          CL*11
00890          RIDFLD   (ELMSTR-KEY)                                    EL157
00891          END-EXEC.                                                EL157
00892                                                                   EL157
00893  7900-EXIT.                                                       EL157
00894       EXIT.                                                       EL157
00895                                                                   EL157
00896  7910-READ-CLAIM-UPDATE.                                          EL157
00897      EXEC CICS READ                                               EL157
00898          DATASET  ('ELMSTR')                                      EL157
00899          SET      (ADDRESS OF CLAIM-MASTER)                          CL*11
00900          RIDFLD   (ELMSTR-KEY)                                    EL157
00901          UPDATE                                                   EL157
00902          END-EXEC.                                                EL157
00903                                                                   EL157
00904  7910-EXIT.                                                       EL157
00905       EXIT.                                                       EL157
CIDMOD     EJECT                                                             000
CIDMOD                                                                       000
CIDMOD 7950-READ-TRAILER.                                                    000
CIDMOD     EXEC CICS READ                                                    000
CIDMOD         DATASET  ('ELTRLR')                                           000
CIDMOD         SET      (ADDRESS OF ACTIVITY-TRAILERS)                       000
CIDMOD         RIDFLD   (ELTRLR-KEY)                                         000
CIDMOD         END-EXEC.                                                     000
CIDMOD                                                                       000
CIDMOD 7950-EXIT.                                                            000
CIDMOD      EXIT.                                                            000
CIDMOD                                                                       000
CIDMOD 7960-READ-TRAILER-UPDATE.                                             000
CIDMOD     EXEC CICS READ                                                    000
CIDMOD         DATASET  ('ELTRLR')                                           000
CIDMOD         SET      (ADDRESS OF ACTIVITY-TRAILERS)                       000
CIDMOD         RIDFLD   (ELTRLR-KEY)                                         000
CIDMOD         UPDATE                                                        000
CIDMOD         END-EXEC.                                                     000
CIDMOD                                                                       000
CIDMOD 7960-EXIT.                                                            000
CIDMOD      EXIT.                                                            000
CIDMOD     EJECT                                                             000
CIDMOD                                                                       000
00907  8100-SEND-INITIAL-MAP.                                           EL157
00908      MOVE EIBTIME                TO TIME-IN.                      EL157
00909      MOVE TIME-OUT               TO RUNTIMEO.                     EL157
00910      MOVE SAVE-DATE              TO RUNDTEO.                      EL157
00911      MOVE -1                     TO FORMTYPL.                     EL157
00912      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O                      EL157
00913      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     EL157
00914                                                                   EL157
00915      EXEC CICS SEND                                               EL157
00916          MAP     (MAP-NAME)                                       EL157
00917          MAPSET  (MAPSET-NAME)                                    EL157
00918          FROM    (EL157AO)                                        EL157
00919          ERASE                                                    EL157
00920          CURSOR                                                   EL157
00921          END-EXEC.                                                EL157
00922      GO TO 9100-RETURN-TRAN.                                      EL157
00923                                                                   EL157
00924  8200-SEND-DATAONLY.                                              EL157
00925      MOVE SAVE-DATE              TO RUNDTEO.                      EL157
00926      MOVE EIBTIME                TO TIME-IN.                      EL157
00927      MOVE TIME-OUT               TO RUNTIMEO.                     EL157
00928      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O                      EL157
00929      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     EL157
00930      EXEC CICS SEND                                               EL157
00931          MAP     (MAP-NAME)                                       EL157
00932          MAPSET  (MAPSET-NAME)                                    EL157
00933          FROM    (EL157AO)                                        EL157
00934          DATAONLY                                                 EL157
00935          CURSOR                                                   EL157
00936          END-EXEC.                                                EL157
00937      GO TO 9100-RETURN-TRAN.                                      EL157
00938      EJECT                                                        EL157
00939  8300-SEND-TEXT.                                                  EL157
00940      EXEC CICS SEND TEXT                                          EL157
00941          FROM    (LOGOFF-TEXT)                                    EL157
00942          LENGTH  (LOGOFF-LENGTH)                                  EL157
00943          ERASE                                                    EL157
00944          FREEKB                                                   EL157
00945          END-EXEC.                                                EL157
00946      EXEC CICS RETURN                                             EL157
00947          END-EXEC.                                                EL157
00948                                                                   EL157
00949                                                                   EL157
00950      EJECT                                                        EL157
00951  8800-UNAUTHORIZED-ACCESS.                                        EL157
00952      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL157
00953      GO TO 8300-SEND-TEXT.                                        EL157
00954                                                                   EL157
00955  8810-PF23.                                                       EL157
00956      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL157
00957      MOVE XCTL-005               TO PGM-NAME.                     EL157
00958      GO TO 9300-XCTL.                                             EL157
CIDMOD                                                                       000
CIDMOD 9000-RETURN-CICS.                                                     000
CIDMOD     EXEC CICS RETURN                                                  000
CIDMOD         END-EXEC.                                                     000
CIDMOD
00960  9100-RETURN-TRAN.                                                EL157
00961      MOVE EMI-ERROR-NUMBER (1)       TO PI-LAST-ERROR-NO.         EL157
00962      MOVE '171A'                     TO PI-CURRENT-SCREEN-NO.     EL157
00963      EXEC CICS RETURN                                             EL157
00964          TRANSID   (TRANS-ID)                                     EL157
00965          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL157
00966          LENGTH    (PI-COMM-LENGTH)                               EL157
00967          END-EXEC.                                                EL157
00968                                                                   EL157
00969  9200-RETURN-MAIN-MENU.                                           EL157
00970      MOVE XCTL-126               TO PGM-NAME.                     EL157
00971      GO TO 9300-XCTL.                                             EL157
00972                                                                   EL157
00973  9210-PF3.                                                        EL157
00974      MOVE XCTL-141               TO PGM-NAME                      EL157
00975      GO TO 9300-XCTL.                                             EL157
00976                                                                   EL157
00977  9300-XCTL.                                                       EL157
00978      EXEC CICS XCTL                                               EL157
00979          PROGRAM   (PGM-NAME)                                     EL157
00980          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL157
00981          LENGTH    (PI-COMM-LENGTH)                               EL157
00982          END-EXEC.                                                EL157
00983                                                                   EL157
00984  9400-CLEAR.                                                      EL157
00985      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL157
00986      GO TO 9300-XCTL.                                             EL157
00987                                                                   EL157
00988  9500-PF12.                                                       EL157
00989      MOVE XCTL-010               TO PGM-NAME.                     EL157
00990      GO TO 9300-XCTL.                                             EL157
00991                                                                   EL157
00992  9600-PGMID-ERROR.                                                EL157
00993      EXEC CICS HANDLE CONDITION                                   EL157
00994          PGMIDERR  (8300-SEND-TEXT)                               EL157
00995          END-EXEC.                                                EL157
00996      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL157
00997      MOVE ' '                    TO PI-ENTRY-CD-1.                EL157
00998      MOVE XCTL-005               TO PGM-NAME.                     EL157
00999      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL157
01000      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL157
01001      GO TO 9300-XCTL.                                             EL157
01002                                                                   EL157
01003  9700-LINK-DATE-CONVERT.                                          EL157
01004      MOVE LINK-ELDATCV           TO PGM-NAME                      EL157
01005          EXEC CICS LINK                                           EL157
01006              PROGRAM   (PGM-NAME)                                 EL157
01007              COMMAREA  (DATE-CONVERSION-DATA)                     EL157
01008              LENGTH    (DC-COMM-LENGTH)                           EL157
01009              END-EXEC.                                            EL157
01010  9700-EXIT.                                                       EL157
01011       EXIT.                                                       EL157
01012                                                                   EL157
01013  9900-ERROR-FORMAT.                                               EL157
01014      IF NOT EMI-ERRORS-COMPLETE                                   EL157
01015          MOVE LINK-001           TO PGM-NAME                      EL157
01016          EXEC CICS LINK                                           EL157
01017              PROGRAM   (PGM-NAME)                                 EL157
01018              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL157
01019              LENGTH    (EMI-COMM-LENGTH)                          EL157
01020              END-EXEC.                                            EL157
01021  9900-EXIT.                                                       EL157
01022      EXIT.                                                        EL157
01023                                                                   EL157
01024  9990-ABEND.                                                      EL157
01025      MOVE LINK-004               TO PGM-NAME.                     EL157
01026      MOVE DFHEIBLK               TO EMI-LINE1.                    EL157
01027      EXEC CICS LINK                                               EL157
01028          PROGRAM   (PGM-NAME)                                     EL157
01029          COMMAREA  (EMI-LINE1)                                    EL157
01030          LENGTH    (72)                                           EL157
01031          END-EXEC.                                                EL157
01032      GO TO 8200-SEND-DATAONLY.                                    EL157
