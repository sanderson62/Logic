00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL6321
00003  PROGRAM-ID.                 EL6321.                                 LV002
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 02/12/96 10:33:35.                    CL**2
00007 *                            VMOD=2.002                              CL**2
00008 *                                                                 EL6321
00008 *                                                                 EL6321
00009 *AUTHOR.        LOGIC,INC.                                           CL**2
00010 *               DALLAS, TEXAS.                                       CL**2
00011                                                                   EL6321
00012 *DATE-COMPILED.                                                      CL**2
00013                                                                   EL6321
00014 *SECURITY.   *****************************************************   CL**2
00015 *            *                                                   *   CL**2
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**2
00017 *            *                                                   *   CL**2
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00019 *                                                                *   CL**2
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00022 *            *                                                   *   CL**2
00023 *            *****************************************************   CL**2
00024                                                                   EL6321
00025 *REMARKS.                                                            CL**2
00026 *        TRANSACTION - EXB5 - PENDING CLAIMS (COMPANY TOTALS).       CL**2
00027                                                                   EL6321
00028  ENVIRONMENT DIVISION.                                            EL6321
00029  DATA DIVISION.                                                   EL6321
00030  EJECT                                                            EL6321
00031  WORKING-STORAGE SECTION.                                         EL6321
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL6321
00033  77  FILLER  PIC X(32)  VALUE '*    EL6321 WORKING STORAGE    *'. EL6321
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.002 *********'.    CL**2
00035                                                                   EL6321
00036                              COPY ELCSCTM.                           CL**2
00037                              COPY ELCSCRTY.                          CL**2
00038                                                                   EL6321
00039     EJECT                                                         EL6321
00040                                                                   EL6321
00041  01  STANDARD-AREAS.                                              EL6321
00042      12  MAP-NAME            PIC  X(8)      VALUE 'EL6321A'.      EL6321
00043      12  MAPSET-NAME         PIC  X(8)      VALUE 'EL6321S'.      EL6321
00044      12  SCREEN-NUMBER       PIC  X(4)      VALUE '632C'.         EL6321
00045      12  TRANS-ID            PIC  X(4)      VALUE 'EXB5'.         EL6321
00046      12  THIS-PGM            PIC  X(8)      VALUE 'EL6321'.       EL6321
00047      12  PGM-NAME            PIC  X(8).                           EL6321
00048      12  TIME-IN             PIC S9(7).                           EL6321
00049      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6321
00050          16  FILLER          PIC  X.                              EL6321
00051          16  TIME-OUT        PIC  99V99.                          EL6321
00052          16  FILLER          PIC  XX.                             EL6321
00053      12  XCTL-005            PIC  X(8)      VALUE 'EL005'.        EL6321
00054      12  XCTL-010            PIC  X(8)      VALUE 'EL010'.        EL6321
00055      12  XCTL-626            PIC  X(8)      VALUE 'EL626'.        EL6321
00056      12  XCTL-630            PIC  X(8)      VALUE 'EL630'.        EL6321
00057      12  XCTL-6301           PIC  X(8)      VALUE 'EL6301'.       EL6321
00058      12  LINK-001            PIC  X(8)      VALUE 'EL001'.        EL6321
00059      12  LINK-004            PIC  X(8)      VALUE 'EL004'.        EL6321
00060      12  LINK-CLDATCV        PIC  X(8)      VALUE 'ELDATCV'.      EL6321
00061      12  ERPNDC-FILE-ID      PIC  X(8)      VALUE 'ERPNDC'.       EL6321
00062      12  WS-CURRENT-DT       PIC  X(8)      VALUE SPACES.         EL6321
00063      12  ER-0029             PIC  X(4)      VALUE '0029'.         EL6321
00064                                                                   EL6321
00065  01  ACCESS-KEYS.                                                 EL6321
00066      12  ERPNDC-KEY.                                              EL6321
00067          16  PNDC-COMPANY-CD     PIC  X      VALUE SPACE.         EL6321
00068          16  PNDC-CARRIER        PIC  X      VALUE SPACE.         EL6321
00069          16  PNDC-GROUPING       PIC  X(6)   VALUE SPACES.        EL6321
00070          16  PNDC-STATE          PIC  XX     VALUE SPACES.        EL6321
00071          16  PNDC-ACCOUNT        PIC  X(10)  VALUE SPACES.        EL6321
00072          16  PNDC-CERT-EFF-DT    PIC  X(2)   VALUE SPACES.        EL6321
00073          16  PNDC-CERT-NO.                                        EL6321
00074              20  PNDC-CERT-PRIME PIC  X(10)  VALUE SPACES.        EL6321
00075              20  PNDC-CERT-SFX   PIC  X      VALUE SPACE.         EL6321
00076          16  PNDC-CLAIM-NO       PIC  X(7)   VALUE SPACES.        EL6321
00077          16  PNDC-CHECK-NO       PIC  X(7)   VALUE SPACES.        EL6321
00078          16  PNDC-RECORD-TYPE    PIC  X      VALUE '1'.           EL6321
00079          16  PNDC-RECORD-SEQ     PIC S9(4)   VALUE +0   COMP.     EL6321
00080  EJECT                                                            EL6321
00081  01  COMPANY-TOTALS      COMP-3.                                  EL6321
00082      12  LF-PYMNT-GOOD       PIC S9(5)         VALUE ZEROS.       EL6321
00083      12  LF-PYMNT-BAD        PIC S9(5)         VALUE ZEROS.       EL6321
00084      12  LF-PYMNT-AMT        PIC S9(7)V99      VALUE ZEROS.       EL6321
00085      12  AH-PYMNT-GOOD       PIC S9(5)         VALUE ZEROS.       EL6321
00086      12  AH-PYMNT-BAD        PIC S9(5)         VALUE ZEROS.       EL6321
00087      12  AH-PYMNT-AMT        PIC S9(7)V99      VALUE ZEROS.       EL6321
00088      12  RSV-GOOD            PIC S9(5)         VALUE ZEROS.       EL6321
00089      12  RSV-BAD             PIC S9(5)         VALUE ZEROS.       EL6321
00090      12  RSV-FUTURE          PIC S9(6)V99      VALUE ZEROS.       EL6321
00091      12  RSV-PTC             PIC S9(6)V99      VALUE ZEROS.       EL6321
00092      12  RSV-IBNR            PIC S9(6)V99      VALUE ZEROS.       EL6321
00093  EJECT                                                            EL6321
00094                                      COPY ELCDATE.                   CL**2
00095  EJECT                                                            EL6321
00096                                      COPY ELCLOGOF.                  CL**2
00097  EJECT                                                            EL6321
00098                                      COPY ELCATTR.                   CL**2
00099  EJECT                                                            EL6321
00100                                      COPY ELCEMIB.                   CL**2
00101  EJECT                                                            EL6321
00102                                      COPY ELCINTF.                   CL**2
00103      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                 EL6321
00104          16  PI-ACC-NAME                 PIC  X(30).              EL6321
00105          16  PI-MAP-NAME                 PIC  X(8).               EL6321
00106          16  PI-BATCH-AMOUNTS    COMP-3.                          EL6321
00107              20  PI-LF-ISS-REMITTED      PIC S9(8)V99.            EL6321
00108              20  PI-LF-ISS-ENTERED       PIC S9(8)V99.            EL6321
00109              20  PI-LF-CAN-REMITTED      PIC S9(8)V99.            EL6321
00110              20  PI-LF-CAN-ENTERED       PIC S9(8)V99.            EL6321
00111              20  PI-AH-ISS-REMITTED      PIC S9(8)V99.            EL6321
00112              20  PI-AH-ISS-ENTERED       PIC S9(8)V99.            EL6321
00113              20  PI-AH-CAN-REMITTED      PIC S9(8)V99.            EL6321
00114              20  PI-AH-CAN-ENTERED       PIC S9(8)V99.            EL6321
00115              20  PI-ISS-CNT-REMITTED     PIC S9(5).               EL6321
00116              20  PI-ISS-CNT-ENTERED      PIC S9(5).               EL6321
00117              20  PI-CAN-CNT-REMITTED     PIC S9(5).               EL6321
00118              20  PI-CAN-CNT-ENTERED      PIC S9(5).               EL6321
00119          16  PI-MAINT-FUNC               PIC  X.                  EL6321
00120          16  PI-ERROR-SW                 PIC  X.                  EL6321
00121              88  PI-DATA-ERRORS              VALUE 'Y'.           EL6321
00122          16  PI-UPDATE-SW                PIC  X.                  EL6321
00123              88  PI-DATA-UPDATED             VALUE 'Y'.           EL6321
00124          16  PI-DISPLAY-SW               PIC  X.                  EL6321
00125              88  PI-LAST-FUNC-DISPLAY        VALUE 'Y'.           EL6321
00126          16  PI-TABS-RESET-SW            PIC  X.                  EL6321
00127              88  PI-TABS-RESET               VALUE 'Y'.           EL6321
00128          16  PI-SAVE-CALLING-PGM         PIC  X(8).               EL6321
00129          16  PI-LAST-SEQ-NO-ADDED        PIC S9(4)      COMP.     EL6321
00130          16  PI-NEXT-DISPLAY-SEQ-NO      PIC S9(4)      COMP.     EL6321
00131          16  PI-SAV-CARRIER              PIC  X.                  EL6321
00132          16  PI-SAV-GROUPING             PIC  X(6).               EL6321
00133          16  PI-SAV-STATE                PIC  XX.                 EL6321
00134          16  PI-SAV-ACCOUNT              PIC  X(10).              EL6321
00135          16  PI-SAV-CERT-EFF-DT          PIC  XX.                 EL6321
00136          16  PI-SAV-CERT-NO.                                      EL6321
00137              20  PI-SAV-CERT-PRIME       PIC  X(10).              EL6321
00138              20  PI-SAV-CERT-SFX         PIC  X.                  EL6321
00139          16  PI-SAV-ENDING-PNDB-KEY.                              EL6321
00140              20  PI-SAV-COMP-CD          PIC  X.                  EL6321
00141              20  PI-SAV-ENTRY-BATCH      PIC  X(6).               EL6321
00142              20  PI-SAV-BATCH-SEQ        PIC S9(4)     COMP.      EL6321
00143              20  PI-SAV-BATCH-CHG-SEQ    PIC S9(4)     COMP.      EL6321
00144          16  PI-VERIFY-DELETE-SW         PIC  X.                  EL6321
00145              88  PI-DELETE-IS-OK             VALUE 'Y'.           EL6321
00146          16  PI-BATCH-EOF-SW             PIC  X.                  EL6321
00147              88  PI-BATCH-EOF                VALUE 'Y'.           EL6321
00148          16  FILLER                      PIC  X(480).                CL**2
00149  EJECT                                                            EL6321
00150                              COPY ELCJPFX.                           CL**2
00151                              PIC  X(503).                         EL6321
00152  EJECT                                                            EL6321
00153                              COPY ELCAID.                            CL**2
00154  01  FILLER  REDEFINES  DFHAID.                                   EL6321
00155      12  FILLER              PIC  X(8).                           EL6321
00156      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.      EL6321
00157  EJECT                                                            EL6321
00158                              COPY EL6321S.                           CL**2
00159  EJECT                                                            EL6321
00160  LINKAGE SECTION.                                                 EL6321
00161  01  DFHCOMMAREA             PIC  X(1024).                        EL6321
00162  EJECT                                                            EL6321
00163                                                                   EL6321
00164                      COPY ERCPNDC.                                   CL**2
00165                                                                      CL**2
00166  EJECT                                                            EL6321
00167  PROCEDURE DIVISION.                                              EL6321
00168                                                                   EL6321
00169      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL6321
00170                                                                   EL6321
00171      IF EIBCALEN = ZERO                                           EL6321
00172          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6321
00173                                                                   EL6321
00174      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6321
00175      MOVE '5'                    TO  DC-OPTION-CODE.              EL6321
00176                                                                   EL6321
00177      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                  EL6321
00178                                                                   EL6321
00179      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.               EL6321
00180                                                                   EL6321
00181      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6321
00182          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6321
00183              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL6321
00184              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL6321
00185              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL6321
00186              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL6321
00187              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL6321
00188              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL6321
00189              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL6321
00190              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM    EL6321
00191          ELSE                                                     EL6321
00192              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL6321
00193              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL6321
00194              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL6321
00195              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL6321
00196              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL6321
00197              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL6321
00198              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL6321
00199              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.   EL6321
00200                                                                   EL6321
00201      MOVE LOW-VALUES             TO  EL6321AI.                    EL6321
00202      MOVE PI-COMPANY-CD          TO  PNDC-COMPANY-CD.             EL6321
00203                                                                   EL6321
00204      EXEC CICS HANDLE CONDITION                                   EL6321
00205          PGMIDERR  (9600-PGMID-ERROR)                             EL6321
00206          ERROR     (9990-ABEND)                                   EL6321
00207          END-EXEC.                                                EL6321
00208  EJECT                                                            EL6321
00209      IF EIBTRNID NOT = TRANS-ID                                   EL6321
00210          GO TO 0200-PROCESS.                                      EL6321
00211                                                                   EL6321
00212      IF EIBAID = DFHCLEAR              OR  DFHENTER               EL6321
00213          GO TO 9400-CLEAR.                                        EL6321
00214                                                                   EL6321
00215      IF EIBAID = DFHPF23                                          EL6321
00216          GO TO 8810-PF23.                                         EL6321
00217                                                                   EL6321
00218      IF EIBAID = DFHPF24                                          EL6321
00219          GO TO 9200-RETURN-MAIN-MENU.                             EL6321
00220                                                                   EL6321
00221      IF EIBAID = DFHPF12                                          EL6321
00222          GO TO 9500-PF12.                                         EL6321
00223                                                                   EL6321
00224      MOVE ER-0029                TO  EMI-ERROR.                   EL6321
00225                                                                   EL6321
00226      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL6321
00227                                                                   EL6321
00228      MOVE AL-UNBON               TO  PFENTERA.                    EL6321
00229      MOVE -1                     TO  PFENTERL.                    EL6321
00230                                                                   EL6321
00231      GO TO 8200-SEND-DATAONLY.                                    EL6321
00232  EJECT                                                            EL6321
00233  0200-PROCESS.                                                    EL6321
00234      EXEC CICS HANDLE CONDITION                                   EL6321
00235          NOTFND   (0400-END-OF-COMPANY)                           EL6321
00236          ENDFILE  (0400-END-OF-COMPANY)                           EL6321
00237      END-EXEC.                                                    EL6321
00238                                                                   EL6321
00239      EXEC CICS STARTBR                                            EL6321
00240          DATASET  (ERPNDC-FILE-ID)                                EL6321
00241          RIDFLD   (ERPNDC-KEY)                                    EL6321
00242      END-EXEC.                                                    EL6321
00243                                                                   EL6321
00244  0300-PROCESS-LOOP.                                               EL6321
00245      EXEC CICS READNEXT                                           EL6321
00246          DATASET  (ERPNDC-FILE-ID)                                EL6321
00247          SET      (ADDRESS OF PENDING-CLAIMS)                        CL**2
00248          RIDFLD   (ERPNDC-KEY)                                    EL6321
00249      END-EXEC.                                                    EL6321
00250                                                                   EL6321
00251      IF PC-COMPANY-CD NOT = PI-COMPANY-CD                         EL6321
00252          GO TO 0400-END-OF-COMPANY.                               EL6321
00253                                                                   EL6321
00254      IF PC-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL6321
00255          GO TO 0300-PROCESS-LOOP.                                 EL6321
00256                                                                   EL6321
00257      IF PC-FATAL-ERRORS                                           EL6321
00258        OR PC-UNFORCED-ERRORS                                      EL6321
00259          IF PC-CLAIMS                                             EL6321
00260              IF PC-LF-CLAIM                                       EL6321
00261                OR PC-OB-LF-CLAIM                                  EL6321
00262                  ADD +1                 TO  LF-PYMNT-BAD          EL6321
00263              ELSE                                                 EL6321
00264                  ADD +1                 TO  AH-PYMNT-BAD          EL6321
00265          ELSE                                                     EL6321
00266              ADD +1                     TO  RSV-BAD               EL6321
00267      ELSE                                                         EL6321
00268          IF PC-CLAIMS                                             EL6321
00269              IF PC-LF-CLAIM                                       EL6321
00270                OR PC-OB-LF-CLAIM                                  EL6321
00271                  ADD +1                 TO  LF-PYMNT-GOOD         EL6321
00272                  ADD PC-CLAIM-PAYMENT   TO  LF-PYMNT-AMT          EL6321
00273              ELSE                                                 EL6321
00274                  ADD +1                 TO  AH-PYMNT-GOOD         EL6321
00275                  ADD PC-CLAIM-PAYMENT   TO  AH-PYMNT-AMT          EL6321
00276          ELSE                                                     EL6321
00277              ADD +1                     TO  RSV-GOOD              EL6321
00278              ADD PC-FUTURE-RESERVE-AMT  TO  RSV-FUTURE            EL6321
00279              ADD PC-PTC-RESERVE-AMT     TO  RSV-PTC               EL6321
00280              ADD PC-IBNR-RESERVE-AMT    TO  RSV-IBNR.             EL6321
00281                                                                   EL6321
00282      GO TO 0300-PROCESS-LOOP.                                     EL6321
00283                                                                   EL6321
00284  0400-END-OF-COMPANY.                                             EL6321
00285      ADD LF-PYMNT-GOOD  LF-PYMNT-BAD  GIVING  LFTOTO.             EL6321
00286      ADD AH-PYMNT-GOOD  AH-PYMNT-BAD  GIVING  AHTOTO.             EL6321
00287                                                                   EL6321
00288      MOVE LF-PYMNT-GOOD          TO  LFGOODO.                     EL6321
00289      MOVE LF-PYMNT-BAD           TO  LFBADO.                      EL6321
00290      MOVE LF-PYMNT-AMT           TO  LFPMTSO.                     EL6321
00291      MOVE AH-PYMNT-GOOD          TO  AHGOODO.                     EL6321
00292      MOVE AH-PYMNT-BAD           TO  AHBADO.                      EL6321
00293      MOVE AH-PYMNT-AMT           TO  AHPMTSO.                     EL6321
00294                                                                   EL6321
00295      ADD RSV-GOOD  RSV-BAD  GIVING  RETOTO.                       EL6321
00296                                                                   EL6321
00297      MOVE RSV-GOOD               TO  REGOODO.                     EL6321
00298      MOVE RSV-BAD                TO  REBADO.                      EL6321
00299      MOVE RSV-FUTURE             TO  FUTUREO.                     EL6321
00300      MOVE RSV-PTC                TO  PTCO.                        EL6321
00301      MOVE RSV-IBNR               TO  IBNRO.                       EL6321
00302  EJECT                                                            EL6321
00303  8100-SEND-INITIAL-MAP.                                           EL6321
00304      MOVE WS-CURRENT-DT          TO  DATEO.                       EL6321
00305      MOVE EIBTIME                TO  TIME-IN.                     EL6321
00306      MOVE TIME-OUT               TO  TIMEO.                       EL6321
00307      MOVE PI-LIFE-OVERRIDE-L6    TO  CTYPE1O.                     EL6321
00308      MOVE PI-AH-OVERRIDE-L6      TO  CTYPE2O.                     EL6321
00309      MOVE -1                     TO  PFENTERL.                    EL6321
00310      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGO.                     EL6321
00311                                                                   EL6321
00312      EXEC CICS SEND                                               EL6321
00313          MAP     (MAP-NAME)                                       EL6321
00314          MAPSET  (MAPSET-NAME)                                    EL6321
00315          FROM    (EL6321AO)                                       EL6321
00316          ERASE                                                    EL6321
00317          CURSOR                                                   EL6321
00318      END-EXEC.                                                    EL6321
00319                                                                   EL6321
00320      GO TO 9100-RETURN-TRAN.                                      EL6321
00321                                                                   EL6321
00322  8200-SEND-DATAONLY.                                              EL6321
00323      MOVE WS-CURRENT-DT          TO  DATEO.                       EL6321
00324      MOVE EIBTIME                TO  TIME-IN.                     EL6321
00325      MOVE TIME-OUT               TO  TIMEO.                       EL6321
00326      MOVE PI-LIFE-OVERRIDE-L6    TO  CTYPE1O.                     EL6321
00327      MOVE PI-AH-OVERRIDE-L6      TO  CTYPE2O.                     EL6321
00328      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGO.                     EL6321
00329                                                                   EL6321
00330      EXEC CICS SEND                                               EL6321
00331          MAP     (MAP-NAME)                                       EL6321
00332          MAPSET  (MAPSET-NAME)                                    EL6321
00333          FROM    (EL6321AO)                                       EL6321
00334          DATAONLY                                                 EL6321
00335          ERASEAUP                                                 EL6321
00336          CURSOR                                                   EL6321
00337      END-EXEC.                                                    EL6321
00338                                                                   EL6321
00339      GO TO 9100-RETURN-TRAN.                                      EL6321
00340  EJECT                                                            EL6321
00341  8300-SEND-TEXT.                                                  EL6321
00342      EXEC CICS SEND TEXT                                          EL6321
00343          FROM    (LOGOFF-TEXT)                                    EL6321
00344          LENGTH  (LOGOFF-LENGTH)                                  EL6321
00345          ERASE                                                    EL6321
00346          FREEKB                                                   EL6321
00347      END-EXEC.                                                    EL6321
00348                                                                   EL6321
00349      EXEC CICS RETURN                                             EL6321
00350      END-EXEC.                                                    EL6321
00351                                                                   EL6321
00352  8500-DATE-CONVERT.                                               EL6321
00353      MOVE LINK-CLDATCV           TO  PGM-NAME.                    EL6321
00354                                                                   EL6321
00355      EXEC CICS LINK                                               EL6321
00356          PROGRAM   (PGM-NAME)                                     EL6321
00357          COMMAREA  (DATE-CONVERSION-DATA)                         EL6321
00358          LENGTH    (DC-COMM-LENGTH)                               EL6321
00359      END-EXEC.                                                    EL6321
00360                                                                   EL6321
00361  8500-EXIT.                                                       EL6321
00362      EXIT.                                                        EL6321
00363                                                                   EL6321
00364  8800-UNAUTHORIZED-ACCESS.                                        EL6321
00365      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL6321
00366                                                                   EL6321
00367      GO TO 8300-SEND-TEXT.                                        EL6321
00368                                                                   EL6321
00369  8810-PF23.                                                       EL6321
00370      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL6321
00371      MOVE XCTL-005               TO  PGM-NAME.                    EL6321
00372                                                                   EL6321
00373      GO TO 9300-XCTL.                                             EL6321
00374                                                                   EL6321
00375  9100-RETURN-TRAN.                                                EL6321
00376      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL6321
00377      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL6321
00378                                                                   EL6321
00379      EXEC CICS RETURN                                             EL6321
00380          TRANSID   (TRANS-ID)                                     EL6321
00381          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL6321
00382          LENGTH    (PI-COMM-LENGTH)                               EL6321
00383      END-EXEC.                                                    EL6321
00384                                                                   EL6321
00385  9200-RETURN-MAIN-MENU.                                           EL6321
00386      MOVE XCTL-626               TO  PGM-NAME.                    EL6321
00387                                                                   EL6321
00388      GO TO 9300-XCTL.                                             EL6321
00389                                                                   EL6321
00390  9300-XCTL.                                                       EL6321
00391      EXEC CICS XCTL                                               EL6321
00392          PROGRAM   (PGM-NAME)                                     EL6321
00393          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL6321
00394          LENGTH    (PI-COMM-LENGTH)                               EL6321
00395      END-EXEC.                                                    EL6321
00396  EJECT                                                            EL6321
00397  9400-CLEAR.                                                      EL6321
00398      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL6321
00399                                                                   EL6321
00400      GO TO 9300-XCTL.                                             EL6321
00401                                                                   EL6321
00402  9500-PF12.                                                       EL6321
00403      MOVE XCTL-010               TO  PGM-NAME.                    EL6321
00404                                                                   EL6321
00405      GO TO 9300-XCTL.                                             EL6321
00406                                                                   EL6321
00407  9600-PGMID-ERROR.                                                EL6321
00408      EXEC CICS HANDLE CONDITION                                   EL6321
00409          PGMIDERR  (8300-SEND-TEXT)                               EL6321
00410      END-EXEC.                                                    EL6321
00411                                                                   EL6321
00412      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL6321
00413      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL6321
00414      MOVE XCTL-005               TO  PGM-NAME.                    EL6321
00415      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL6321
00416      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL6321
00417                                                                   EL6321
00418      GO TO 9300-XCTL.                                             EL6321
00419                                                                   EL6321
00420  9900-ERROR-FORMAT.                                               EL6321
00421      IF NOT EMI-ERRORS-COMPLETE                                   EL6321
00422          MOVE LINK-001           TO  PGM-NAME                     EL6321
00423          EXEC CICS LINK                                           EL6321
00424              PROGRAM   (PGM-NAME)                                 EL6321
00425              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL6321
00426              LENGTH    (EMI-COMM-LENGTH)                          EL6321
00427          END-EXEC.                                                EL6321
00428                                                                   EL6321
00429  9900-EXIT.                                                       EL6321
00430      EXIT.                                                        EL6321
00431                                                                   EL6321
00432  9990-ABEND.                                                      EL6321
00433      MOVE LINK-004               TO  PGM-NAME.                    EL6321
00434      MOVE DFHEIBLK               TO  EMI-LINE1                    EL6321
00435                                                                   EL6321
00436      EXEC CICS LINK                                               EL6321
00437          PROGRAM   (PGM-NAME)                                     EL6321
00438          COMMAREA  (EMI-LINE1)                                    EL6321
00439          LENGTH    (72)                                           EL6321
00440      END-EXEC.                                                    EL6321
00441                                                                   EL6321
00442      GO TO 8200-SEND-DATAONLY.                                    EL6321
00443                                                                   EL6321
00444      GOBACK.                                                      EL6321
00445                                                                   EL6321
00446  9995-SECURITY-VIOLATION.                                         EL6321
00447                              COPY ELCSCTP.                        EL6321
00448                                                                   EL6321
00449  9995-EXIT.                                                       EL6321
00450      EXIT.                                                        EL6321
00451                                                                   EL6321
