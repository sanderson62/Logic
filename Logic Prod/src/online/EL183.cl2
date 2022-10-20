00001  ID DIVISION.                                                     06/26/96
00002                                                                   EL183
00003  PROGRAM-ID.                 EL183.                                  LV007
00004 *              PROGRAM CONVERTED BY                                  CL**6
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**6
00006 *              CONVERSION DATE 02/12/96 09:29:46.                    CL**6
00007 *                            VMOD=2.007                              CL**7
00008 *                                                                 EL183
00008 *                                                                 EL183
00009 *AUTHOR.     LOGIC,INC.                                              CL**6
00010 *            DALLAS, TEXAS.                                          CL**6
00011                                                                   EL183
00012 *DATE-COMPILED.                                                      CL**6
00013                                                                   EL183
00014 *SECURITY.   *****************************************************   CL**6
00015 *            *                                                   *   CL**6
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**6
00017 *            *                                                   *   CL**6
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**6
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**6
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**6
00021 *            *                                                   *   CL**6
00022 *            *****************************************************   CL**6
00023                                                                   EL183
00024 *REMARKS.    TRANSACTION - EX62 - FORM RELEASE.                      CL**4
00025 *        THIS FUNCTION IS USED TO START THE PRINTING OF FORMS        CL**4
00026 *        OR FORM ALIGNMENT OF FORMS. IT MAY ALSO BE USED             CL**4
00027 *        TO OBTAIN COUNTS OF OUTSTANDING FORMS.                      CL**4
00028                                                                   EL183
00029                                                                   EL183
00030                                                                   EL183
00031      EJECT                                                        EL183
00032  ENVIRONMENT DIVISION.                                            EL183
00033  DATA DIVISION.                                                   EL183
00034  WORKING-STORAGE SECTION.                                         EL183
00035  77  FILLER  PIC X(32)  VALUE '********************************'. EL183
00036  77  FILLER  PIC X(32)  VALUE '*    EL183 WORKING STORAGE     *'. EL183
00037  77  FILLER   PIC X(32) VALUE '******** VMOD=2.007 ************'.    CL**7
00038                                                                   EL183
00039                                  COPY ELCSCTM.                       CL**5
00040                                                                      CL**5
00041                                  COPY ELCSCRTY.                      CL**5
00042      EJECT                                                           CL**5
00043  01  WS-DATE-AREA.                                                EL183
00044      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL183
00045                                                                   EL183
00046  01  STANDARD-AREAS.                                              EL183
00047      12  SC-ITEM                 PIC S9(4)   VALUE +0001 COMP.       CL**5
00048      12  MAP-NAME.                                                EL183
00049          16  MAP-PREFIX          PIC XX      VALUE 'EL'.          EL183
00050          16  MAP-NUMBER          PIC X(4)    VALUE '183A'.        EL183
00051          16  MAP-FILLER          PIC XX      VALUE '  '.          EL183
00052      12  MAPSET-NAME             PIC X(8)    VALUE 'EL183S'.      EL183
00053      12  TRANS-ID                PIC X(4)    VALUE 'EX62'.        EL183
00054      12  PRINT-TRANS             PIC X(4)    VALUE 'EX63'.        EL183
00055      12  PGM-NAME                PIC X(8).                        EL183
00056      12  TIME-IN                 PIC S9(7).                       EL183
00057      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL183
00058          16  FILLER              PIC X.                           EL183
00059          16  TIME-OUT            PIC 99V99.                       EL183
00060          16  FILLER              PIC XX.                          EL183
00061      12  XCTL-005                PIC X(5)    VALUE 'EL005'.       EL183
00062      12  XCTL-010                PIC X(5)    VALUE 'EL010'.       EL183
00063      12  XCTL-126                PIC X(5)    VALUE 'EL126'.       EL183
00064      12  LINK-001                PIC X(5)    VALUE 'EL001'.       EL183
00065      12  LINK-004                PIC X(5)    VALUE 'EL004'.       EL183
00066      12  LINK-ELDATCV            PIC X(7)    VALUE 'ELDATCV'.     EL183
00067      12  THIS-PGM                PIC X(8)    VALUE 'EL183'.          CL**5
00068      12  CURRENT-SAVE            PIC XX.                          EL183
00069      12  DEEDIT-FIELD            PIC X(15).                       EL183
00070      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(15).       EL183
00071      12  WS-COUNTER              PIC 9(5)    COMP-3 VALUE 0.      EL183
00072      12  SUB                     PIC 9       COMP-3.              EL183
00073      12  ARCH-ID                 PIC X(8)    VALUE 'ELARCH2 '.    EL183
00074      12  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL  '.    EL183
00075      12  ACTV-ID                 PIC X(8)    VALUE 'ELTRLR  '.    EL183
00076                                                                   EL183
00077      12  TRANS-DATA-LENGTH       PIC S9(4)   COMP VALUE +133.     EL183
00078      12  WS-MESSAGE.                                              EL183
00079          16  FILLER              PIC X(25)   VALUE                EL183
00080               'FORM WILL BE PRINTED FOR '.                        EL183
00081          16  WS-CARRIER          PIC X.                           EL183
00082          16  FILLER              PIC X       VALUE SPACE.         EL183
00083          16  WS-CLAIM            PIC X(7).                        EL183
00084          16  FILLER              PIC X       VALUE SPACE.         EL183
00085          16  WS-CERT             PIC X(11).                       EL183
00086          16  FILLER              PIC X(10)   VALUE ALL '*'.       EL183
00087          16  WS-ARCH-NO          PIC Z(10).                       EL183
00088          16  FILLER              PIC X(5)    VALUE SPACES.        EL183
00089          16  WS-COUNT            PIC Z(5).                        EL183
00090          16  FILLER              PIC X(5)    VALUE SPACES.        EL183
00091          16  WS-KIND             PIC X(6).                        EL183
00092          16  FILLER              PIC X(5)    VALUE SPACES.        EL183
00093          16  WS-TYPE             PIC X.                           EL183
00094          16  FILLER              PIC X(39)   VALUE SPACES.        EL183
00095                                                                   EL183
00096      12  CNTL-KEY.                                                EL183
00097          16  CNTL-CO             PIC X(3).                        EL183
00098          16  CNTL-RECORD-TYPE    PIC X       VALUE '1'.           EL183
00099          16  CNTL-GENL.                                           EL183
00100            18 CNTL-GEN1          PIC XX      VALUE SPACES.        EL183
00101            18 CNTL-GEN2.                                          EL183
00102              20 CNTL-GEN3        PIC X       VALUE SPACES.        EL183
00103              20 CNTL-GEN4        PIC X       VALUE SPACES.        EL183
00104          16  CNTL-SEQ            PIC S9(4)   VALUE +0    COMP.    EL183
00105      12  ARCH-KEY.                                                EL183
00106          16  ARCH-PARTIAL-KEY.                                    EL183
00107              20  ARCH-CO         PIC X.                           EL183
00108              20  ARCH-REC-TYPE   PIC X.                           EL183
00109          16  ARCH-NUMBER         PIC S9(8)   COMP.                EL183
00110          16  ARCH-SEQ            PIC S9(4)   COMP VALUE +0.       EL183
00111      12  ACTV-KEY.                                                EL183
00112          16  ACTV-CO             PIC X.                           EL183
00113          16  ACTV-CARRIER        PIC X.                           EL183
00114          16  ACTV-CLAIM          PIC X(7).                        EL183
00115          16  ACTV-CERT-NO        PIC X(11).                       EL183
00116          16  ACTV-SEQ            PIC S9(4)   COMP.                EL183
00117      EJECT                                                        EL183
00118      12  ARCH-SAVE-KEY           PIC X(5).                        EL183
00119      12  ARCH-LENGTH             PIC S9(4)   COMP  VALUE +90.     EL183
00120      12  ER-0000                 PIC 9(4)    VALUE 0000.          EL183
00121      12  ER-0004                 PIC 9(4)    VALUE 0004.          EL183
00122      12  ER-0008                 PIC 9(4)    VALUE 0008.          EL183
00123      12  ER-0029                 PIC 9(4)    VALUE 0029.          EL183
00124      12  ER-0042                 PIC 9(4)    VALUE 0042.          EL183
00125      12  ER-0070                 PIC 9(4)    VALUE 0070.             CL**5
00126      12  ER-0172                 PIC 9(4)    VALUE 0172.          EL183
00127      12  ER-0182                 PIC 9(4)    VALUE 0182.          EL183
00128      12  ER-0190                 PIC 9(4)    VALUE 0190.          EL183
00129      12  ER-0408                 PIC 9(4)    VALUE 0408.          EL183
00130      12  ER-0409                 PIC 9(4)    VALUE 0409.          EL183
00131      12  ER-0410                 PIC 9(4)    VALUE 0410.          EL183
00132      12  ER-0412                 PIC 9(4)    VALUE 0412.          EL183
00133      12  ER-0413                 PIC 9(4)    VALUE 0413.          EL183
00134      12  ER-0532                 PIC 9(4)    VALUE 0532.          EL183
00135      12  ER-0553                 PIC 9(4)    VALUE 0553.          EL183
00136      12  ER-0554                 PIC 9(4)    VALUE 0554.          EL183
00137                                                                   EL183
00138      EJECT                                                        EL183
00139                                  COPY ELCDATE.                       CL**5
00140      EJECT                                                        EL183
00141                                  COPY ELCLOGOF.                      CL**5
00142      EJECT                                                        EL183
00143                                  COPY ELCATTR.                       CL**5
00144      EJECT                                                        EL183
00145                                  COPY ELCEMIB.                       CL**5
00146      EJECT                                                           CL**5
00147                                  COPY ELCINTF.                       CL**5
00148      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL183
00149 **********************************************************        EL183
00150 *    NOTE                                                *        EL183
00151 *        THE WORK AREA IS USED BY EL183 AND EL1782       *        EL183
00152 *        AND CANNOT BE REARRANGED WITHOUT COMPILING      *           CL**5
00153 *        BOTH PROGRAMS.                                  *        EL183
00154 **********************************************************        EL183
00155          16  PI-PRINT-DATE       PIC X(8).                        EL183
00156          16  PI-PRINT-DATE-BIN   PIC XX.                          EL183
00157          16  PI-PRINT-ID         PIC X(4).                        EL183
00158          16  PI-FORM-TYPE        PIC X.                           EL183
00159          16  FILLER              PIC X(625).                         CL**6
00160      EJECT                                                        EL183
00161                                  COPY ELCAID.                        CL**5
00162  01  FILLER    REDEFINES DFHAID.                                  EL183
00163      12  FILLER                  PIC X(8).                        EL183
00164      12  PF-VALUES               PIC X       OCCURS 24.           EL183
00165      EJECT                                                        EL183
00166                                  COPY EL183S.                        CL**5
00167      EJECT                                                        EL183
00168  LINKAGE SECTION.                                                 EL183
00169  01  DFHCOMMAREA             PIC X(1024).                         EL183
00170                                                                   EL183
00171                              COPY ELCCNTL.                           CL**5
00172      EJECT                                                        EL183
00173                              COPY ELCARCH.                           CL**5
00174      EJECT                                                           CL**5
00175                              COPY ELCTRLR.                           CL**5
00176      EJECT                                                        EL183
00177  PROCEDURE DIVISION.                                              EL183
00178                                                                   EL183
00179      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL183
00180      MOVE '5'                   TO DC-OPTION-CODE.                EL183
00181      PERFORM 9700-DATE-LINK  THRU  9700-EXIT.                     EL183
00182      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL183
00183      MOVE DC-BIN-DATE-1         TO  CURRENT-SAVE.                 EL183
00184                                                                   EL183
00185      MOVE DFHCOMMAREA           TO PROGRAM-INTERFACE-BLOCK.       EL183
00186      MOVE 1                     TO EMI-NUMBER-OF-LINES.           EL183
00187                                                                   EL183
00188      IF EIBCALEN = 0                                              EL183
00189          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL183
00190                                                                   EL183
00191      IF PI-CALLING-PROGRAM NOT = THIS-PGM                            CL**5
00192          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                      CL**5
00193              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL183
00194              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL183
00195              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL183
00196              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL183
00197              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL183
00198              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL183
00199              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL183
00200              MOVE THIS-PGM             TO PI-CALLING-PROGRAM         CL**5
00201              MOVE ZEROS                TO PI-PRINT-DATE           EL183
00202              MOVE LOW-VALUES           TO EL183AO                 EL183
00203              GO TO 8100-SEND-INITIAL-MAP.                         EL183
00204                                                                   EL183
00205      EXEC CICS HANDLE CONDITION                                   EL183
00206          PGMIDERR  (9600-PGMID-ERROR)                             EL183
00207          ERROR     (9990-ABEND)                                   EL183
00208          END-EXEC.                                                EL183
00209                                                                   EL183
00210      IF EIBAID = DFHCLEAR                                         EL183
00211          GO TO 9400-CLEAR.                                        EL183
00212                                                                      CL**5
00213      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                           CL**5
00214          NEXT SENTENCE                                               CL**5
00215      ELSE                                                            CL**5
00216          EXEC CICS READQ TS                                          CL**5
00217              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                     CL**5
00218              INTO    (SECURITY-CONTROL)                              CL**5
00219              LENGTH  (SC-COMM-LENGTH)                                CL**5
00220              ITEM    (SC-ITEM)                                       CL**5
00221          END-EXEC                                                    CL**5
00222          MOVE SC-CLAIMS-DISPLAY (26)     TO  PI-DISPLAY-CAP          CL**5
00223          MOVE SC-CLAIMS-UPDATE  (26)     TO  PI-MODIFY-CAP           CL**5
00224          IF NOT DISPLAY-CAP                                          CL**5
00225              MOVE 'READ'                 TO  SM-READ                 CL**5
00226              PERFORM 9995-SECURITY-VIOLATION                         CL**5
00227              MOVE ER-0070                TO  EMI-ERROR               CL**5
00228              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**5
00229              GO TO 8100-SEND-INITIAL-MAP.                            CL**5
00230                                                                   EL183
00231      EJECT                                                        EL183
00232  0200-RECEIVE.                                                    EL183
00233      MOVE LOW-VALUES             TO EL183AI.                      EL183
00234      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL183
00235          MOVE ER-0008            TO EMI-ERROR                     EL183
00236          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL183
00237          MOVE -1                 TO OPTIONL                       EL183
00238          GO TO 8200-SEND-DATAONLY.                                EL183
00239                                                                   EL183
00240      EXEC CICS RECEIVE                                            EL183
00241          MAP     (MAP-NAME)                                       EL183
00242          MAPSET  (MAPSET-NAME)                                    EL183
00243          INTO    (EL183AI)                                        EL183
00244          END-EXEC.                                                EL183
00245                                                                   EL183
00246      IF ENTERPFL = 0                                              EL183
00247          GO TO 0300-CHECK-PFKEYS.                                 EL183
00248      IF EIBAID NOT = DFHENTER                                     EL183
00249          MOVE ER-0004            TO EMI-ERROR                     EL183
00250          GO TO 0320-INPUT-ERROR.                                  EL183
00251                                                                   EL183
00252      IF ENTERPFI GREATER 0 AND LESS 25                            EL183
00253          MOVE PF-VALUES (ENTERPFI)  TO EIBAID                     EL183
00254      ELSE                                                         EL183
00255          MOVE ER-0029               TO EMI-ERROR                  EL183
00256          GO TO 0320-INPUT-ERROR.                                  EL183
00257                                                                   EL183
00258  0300-CHECK-PFKEYS.                                               EL183
00259      IF EIBAID = DFHPF23                                          EL183
00260          GO TO 8810-PF23.                                         EL183
00261      IF EIBAID = DFHPF24                                          EL183
00262          GO TO 9200-RETURN-MAIN-MENU.                             EL183
00263      IF EIBAID = DFHPF12                                          EL183
00264          GO TO 9500-PF12.                                         EL183
00265      IF EIBAID = DFHENTER                                         EL183
00266          GO TO 0330-FUNCTION-CHECK.                               EL183
00267                                                                   EL183
00268      MOVE ER-0029                TO EMI-ERROR.                    EL183
00269  0320-INPUT-ERROR.                                                EL183
00270      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL183
00271      MOVE AL-UNBON               TO ENTERPFA.                     EL183
00272      IF ENTERPFL = 0                                              EL183
00273          MOVE -1                 TO OPTIONL                       EL183
00274      ELSE                                                         EL183
00275          MOVE -1                 TO ENTERPFL.                     EL183
00276      GO TO 8200-SEND-DATAONLY.                                    EL183
00277                                                                   EL183
00278      EJECT                                                        EL183
00279  0330-FUNCTION-CHECK.                                             EL183
00280      PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT                     EL183
00281                                                                   EL183
00282      IF NOT EMI-NO-ERRORS                                         EL183
00283         GO TO 8200-SEND-DATAONLY.                                 EL183
00284                                                                      CL**5
00285      IF NOT MODIFY-CAP                                               CL**5
00286          IF (OPTIONI IS EQUAL TO '3' OR '4')                         CL**5
00287              NEXT SENTENCE                                           CL**5
00288          ELSE                                                        CL**5
00289              MOVE 'UPDATE'           TO  SM-READ                     CL**5
00290              PERFORM 9995-SECURITY-VIOLATION                         CL**5
00291              MOVE ER-0070            TO  EMI-ERROR                   CL**5
00292              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**5
00293              GO TO 8100-SEND-INITIAL-MAP.                            CL**5
00294                                                                      CL**5
00295      IF OPTIONI = '1'                                             EL183
00296          GO TO 1000-PRINT-INITIAL-FORMS.                          EL183
00297      IF OPTIONI = '2'                                             EL183
00298          GO TO 2000-PRINT-FOLLOW-UP-FORMS.                        EL183
00299      IF OPTIONI = '3'                                             EL183
00300          GO TO 3000-SHOW-COUNT-OF-INITIAL.                        EL183
00301      IF OPTIONI = '4'                                             EL183
00302          GO TO 4000-SHOW-COUNT-OF-FOLLOW-UP.                      EL183
00303      IF OPTIONI = '5'                                             EL183
00304          GO TO 5000-PRINT-ALIGNMENT-FORMS.                        EL183
00305      IF OPTIONI = '6'                                             EL183
00306          GO TO 6000-REPRINT-FORMS-BY-DATE.                        EL183
00307      EJECT                                                        EL183
00308  0350-EDIT-ROUTINE.                                               EL183
00309      IF OPTIONI LESS '1' OR GREATER '6'                           EL183
00310         MOVE -1                  TO OPTIONL                       EL183
00311         MOVE ER-0409             TO EMI-ERROR                     EL183
00312         MOVE AL-UNBON            TO OPTIONA                       EL183
00313         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL183
00314        ELSE                                                       EL183
00315         MOVE AL-UNNON            TO OPTIONA.                      EL183
00316                                                                   EL183
00317      IF (OPTIONI = '6'  AND  DATEINL = ZEROS)                     EL183
00318         MOVE -1                  TO DATEINL                       EL183
00319         MOVE ER-0410             TO EMI-ERROR                     EL183
00320         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL183
00321                                                                   EL183
00322      IF DATEINL NOT = ZEROS                                       EL183
00323         MOVE DATEINI             TO DEEDIT-FIELD                  EL183
00324         PERFORM 8600-DEEDIT                                       EL183
00325         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            EL183
00326         MOVE '4'                 TO DC-OPTION-CODE                EL183
00327         PERFORM 9700-DATE-LINK  THRU  9700-EXIT                   EL183
00328         IF DATE-CONVERSION-ERROR                                  EL183
00329            MOVE ER-0182          TO EMI-ERROR                     EL183
00330            MOVE -1               TO DATEINL                       EL183
00331            MOVE AL-UABON         TO DATEINA                       EL183
00332            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT             EL183
00333           ELSE                                                    EL183
00334            MOVE AL-UANON         TO DATEINA                       EL183
00335            MOVE DC-BIN-DATE-1    TO PI-PRINT-DATE-BIN             EL183
00336            MOVE DC-GREG-DATE-1-EDIT   TO PI-PRINT-DATE            EL183
00337                                          DATEINI                  EL183
00338        ELSE                                                       EL183
00339         MOVE LOW-VALUE           TO PI-PRINT-DATE-BIN.            EL183
00340                                                                   EL183
00341      IF FORMTYPI = 'P' OR 'I'                                        CL**3
00342         MOVE FORMTYPI            TO PI-FORM-TYPE                     CL**3
00343         MOVE AL-UANON            TO FORMTYPA                         CL**3
00344      ELSE                                                            CL**3
00345         MOVE ER-0532             TO EMI-ERROR                        CL**3
00346         MOVE -1                  TO FORMTYPL                         CL**3
00347         MOVE AL-UABON            TO FORMTYPA                         CL**3
00348         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    CL**3
00349                                                                   EL183
00350  0350-EXIT.                                                       EL183
00351       EXIT.                                                       EL183
00352                                                                   EL183
00353  400-SET-CODES.                                                   EL183
00354      MOVE PI-COMPANY-ID          TO CNTL-CO.                      EL183
00355      MOVE PI-COMPANY-CD          TO ARCH-CO                       EL183
00356                                     ACTV-CO.                      EL183
00357      EJECT                                                        EL183
00358   1000-PRINT-INITIAL-FORMS.                                       EL183
00359      MOVE '1'                    TO PI-ENTRY-CD-1                 EL183
00360                                     PI-ENTRY-CD-2.                EL183
00361      GO TO 7800-START-PRINT.                                      EL183
00362                                                                   EL183
00363                                                                   EL183
00364   2000-PRINT-FOLLOW-UP-FORMS.                                     EL183
00365      MOVE '1'                    TO PI-ENTRY-CD-1.                EL183
00366      MOVE '2'                    TO PI-ENTRY-CD-2.                EL183
00367      GO TO 7800-START-PRINT.                                      EL183
00368                                                                   EL183
00369   3000-SHOW-COUNT-OF-INITIAL.                                     EL183
00370      PERFORM 7500-BROWSE-ARCHIVE THRU 7599-EXIT.                  EL183
00371      GO TO 6500-COMPLETE-COUNT.                                   EL183
00372                                                                   EL183
00373   4000-SHOW-COUNT-OF-FOLLOW-UP.                                   EL183
00374      PERFORM 7500-BROWSE-ARCHIVE THRU 7599-EXIT.                  EL183
00375      GO TO 6500-COMPLETE-COUNT.                                   EL183
00376                                                                   EL183
00377  5000-PRINT-ALIGNMENT-FORMS.                                         CL**2
00378      MOVE SPACES                 TO PI-ENTRY-CD-1.                EL183
00379      MOVE '2'                    TO PI-ENTRY-CD-2.                EL183
00380      GO TO 7800-START-PRINT.                                      EL183
00381                                                                   EL183
00382  6000-REPRINT-FORMS-BY-DATE.                                         CL**2
00383      MOVE SPACES                 TO PI-ENTRY-CD-1.                EL183
00384      MOVE '3'                    TO PI-ENTRY-CD-2.                EL183
00385      GO TO 7800-START-PRINT.                                      EL183
00386                                                                   EL183
00387  6500-COMPLETE-COUNT.                                             EL183
00388      MOVE LOW-VALUES             TO FORMTYPI.                        CL**2
00389      MOVE WS-COUNTER             TO COUNTO.                       EL183
00390      MOVE -1                     TO OPTIONL.                      EL183
00391      MOVE ER-0000                TO EMI-ERROR.                    EL183
00392      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL183
00393      GO TO 8200-SEND-DATAONLY.                                    EL183
00394      EJECT                                                        EL183
00395  7500-BROWSE-ARCHIVE.                                             EL183
00396      MOVE LOW-VALUES             TO ARCH-KEY.                     EL183
00397      PERFORM 400-SET-CODES.                                       EL183
00398      MOVE '4'                    TO ARCH-REC-TYPE.                EL183
00399      EXEC CICS HANDLE CONDITION                                   EL183
00400           NOTFND   (7599-EXIT)                                    EL183
00401           ENDFILE  (7599-EXIT)                                    EL183
00402           NOTOPEN  (8850-ARCH-NOT-OPEN)                           EL183
00403           END-EXEC.                                               EL183
00404      EXEC CICS STARTBR                                            EL183
00405           DATASET    (ARCH-ID)                                    EL183
00406           RIDFLD     (ARCH-KEY)                                   EL183
00407           GENERIC                                                 EL183
00408           KEYLENGTH  (2)                                          EL183
00409           END-EXEC.                                               EL183
00410                                                                   EL183
00411  7505-RESET-HANDLE.                                               EL183
00412      EXEC CICS HANDLE CONDITION                                   EL183
00413           NOTFND  (7599-EXIT)                                     EL183
00414           ENDFILE  (7599-EXIT)                                    EL183
00415           NOTOPEN  (8850-ARCH-NOT-OPEN)                           EL183
00416           END-EXEC.                                               EL183
00417                                                                   EL183
00418  7510-READ-NEXT.                                                  EL183
00419      EXEC CICS READNEXT                                           EL183
00420           DATASET  (ARCH-ID)                                      EL183
00421           RIDFLD   (ARCH-KEY)                                     EL183
00422           SET      (ADDRESS OF LETTER-ARCHIVE)                       CL**6
00423           END-EXEC.                                               EL183
00424                                                                   EL183
00425      IF ARCH-CO NOT = PI-COMPANY-CD OR ARCH-REC-TYPE NOT = '4'    EL183
00426         GO TO 7599-EXIT.                                          EL183
00427                                                                   EL183
00428      IF (FORMTYPI = 'P' AND NOT LA4-PROGRESS-FORM)  OR            EL183
00429         (FORMTYPI = 'I' AND NOT LA4-INITIAL-FORM)                 EL183
00430         GO TO 7510-READ-NEXT.                                     EL183
00431                                                                   EL183
00432      IF OPTIONI = '3'                                             EL183
00433         IF LA4-INITIAL-PRINT-DATE = LOW-VALUES                    EL183
00434               PERFORM 7600-READ-ACTIVITY THRU 7699-EXIT           EL183
00435               GO TO 7510-READ-NEXT                                EL183
00436            ELSE                                                   EL183
00437               GO TO 7510-READ-NEXT.                               EL183
00438                                                                   EL183
00439      IF LA4-RESEND-DATE = LOW-VALUES                              EL183
00440         GO TO 7510-READ-NEXT.                                     EL183
00441                                                                   EL183
00442      IF LA4-RESEND-DATE GREATER THAN CURRENT-SAVE                 EL183
00443         GO TO 7510-READ-NEXT.                                     EL183
00444                                                                   EL183
00445      EXEC CICS HANDLE CONDITION                                   EL183
00446           NOTFND   (7510-READ-NEXT)                               EL183
00447           NOTOPEN  (8870-ACTV-NOT-OPEN)                           EL183
00448           END-EXEC.                                               EL183
00449                                                                   EL183
00450      PERFORM 7700-READ-ACTIVITY THRU 7799-EXIT.                   EL183
00451                                                                   EL183
00452      IF NOT FORM-CONTROL-TR                                       EL183
00453         GO TO 7505-RESET-HANDLE.                                  EL183
00454                                                                   EL183
00455      IF AT-FORM-ANSWERED-DT  = LOW-VALUES  AND                    EL183
00456         AT-FORM-RE-SEND-DT NOT = LOW-VALUES                       EL183
00457         ADD 1                    TO WS-COUNTER                    EL183
00458         PERFORM 7900-WRITE-TD THRU 7900-EXIT                      EL183
00459         GO TO 7505-RESET-HANDLE.                                  EL183
00460                                                                   EL183
00461      EXEC CICS HANDLE CONDITION                                   EL183
00462           NOTOPEN  (8850-ARCH-NOT-OPEN)                           EL183
00463           END-EXEC.                                               EL183
00464                                                                   EL183
00465      EXEC CICS READ                                               EL183
00466           DATASET  ('ELARCH  ')                                   EL183
00467           RIDFLD   (LA-CONTROL-PRIMARY)                           EL183
00468           SET      (ADDRESS OF LETTER-ARCHIVE)                       CL**6
00469           UPDATE                                                  EL183
00470           END-EXEC.                                               EL183
00471                                                                   EL183
00472      PERFORM 7910-WRITE-TD THRU 7910-EXIT.                        EL183
00473      MOVE LOW-VALUES             TO LA4-RESEND-DATE.              EL183
00474      EXEC CICS HANDLE CONDITION                                   EL183
00475          DUPKEY  (7505-RESET-HANDLE)                              EL183
00476          END-EXEC.                                                EL183
00477                                                                   EL183
00478      EXEC CICS REWRITE                                            EL183
00479           DATASET  ('ELARCH  ')                                   EL183
00480           FROM     (LETTER-ARCHIVE)                               EL183
00481           END-EXEC.                                               EL183
00482                                                                   EL183
00483        GO TO 7505-RESET-HANDLE.                                   EL183
00484                                                                   EL183
00485  7599-EXIT.                                                       EL183
00486       EXIT.                                                       EL183
00487      EJECT                                                        EL183
00488                                                                   EL183
00489  7600-READ-ACTIVITY.                                              EL183
00490      EXEC CICS HANDLE CONDITION                                   EL183
00491           NOTFND  (7699-EXIT)                                     EL183
00492           END-EXEC.                                               EL183
00493                                                                   EL183
00494      PERFORM 7700-READ-ACTIVITY THRU 7799-EXIT.                   EL183
00495                                                                   EL183
00496      IF NOT FORM-CONTROL-TR                                       EL183
00497         GO TO 7699-EXIT.                                          EL183
00498                                                                   EL183
00499      IF DATEINL = ZEROS                                           EL183
00500         IF AT-FORM-SEND-ON-DT NOT GREATER THAN CURRENT-SAVE       EL183
00501            ADD 1               TO WS-COUNTER                      EL183
00502            PERFORM 7900-WRITE-TD THRU 7900-EXIT                   EL183
00503            GO TO 7699-EXIT.                                       EL183
00504                                                                   EL183
00505      IF AT-FORM-SEND-ON-DT NOT GREATER THAN PI-PRINT-DATE-BIN     EL183
00506         ADD 1                 TO WS-COUNTER                       EL183
00507         PERFORM 7900-WRITE-TD THRU 7900-EXIT.                     EL183
00508                                                                   EL183
00509  7699-EXIT.                                                       EL183
00510       EXIT.                                                       EL183
00511      EJECT                                                        EL183
00512  7700-READ-ACTIVITY.                                              EL183
00513      MOVE LA4-CARRIER             TO ACTV-CARRIER                 EL183
00514      MOVE LA4-CLAIM-NO            TO ACTV-CLAIM                   EL183
00515      MOVE LA4-CERT-NO             TO ACTV-CERT-NO                 EL183
00516      MOVE LA4-FORM-TRLR-SEQ       TO ACTV-SEQ                     EL183
00517      EXEC CICS READ                                               EL183
00518           DATASET  (ACTV-ID)                                      EL183
00519           RIDFLD   (ACTV-KEY)                                     EL183
00520           SET      (ADDRESS OF ACTIVITY-TRAILERS)                    CL**6
00521           END-EXEC.                                                  CL**6
00522                                                                      CL**6
00523  7799-EXIT.                                                       EL183
00524       EXIT.                                                       EL183
00525      EJECT                                                        EL183
00526                                                                   EL183
00527  7800-START-PRINT.                                                EL183
00528      PERFORM 400-SET-CODES.                                       EL183
00529      EXEC CICS HANDLE CONDITION                                   EL183
00530           NOTOPEN     (8840-CNTL-NOT-OPEN)                        EL183
00531           NOTFND      (7890-NOT-FOUND)                            EL183
00532           TERMIDERR   (8820-TERMID-ERROR)                         EL183
00533           TRANSIDERR  (8830-TRANS-ERROR)                          EL183
00534           END-EXEC.                                               EL183
00535                                                                      CL**7
00536      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.               CL**7
00537      IF PRINTERL NOT = ZEROS                                      EL183
00538         MOVE PRINTERI            TO PI-PRINT-ID                   EL183
00539                                     PI-ALT-DMD-PRT-ID                CL**7
00540         GO TO 7820-START.                                         EL183
00541                                                                      CL**4
00542      IF PI-PROCESSOR-PRINTER IS NOT EQUAL TO SPACES                  CL**4
00543          MOVE PI-PROCESSOR-PRINTER   TO  PI-PRINT-ID                 CL**4
00544          GO TO 7820-START.                                           CL**4
00545                                                                      CL**4
00546      MOVE '1'                    TO CNTL-RECORD-TYPE.             EL183
00547      MOVE SPACES                 TO CNTL-GENL.                    EL183
00548      MOVE ZEROS                  TO CNTL-SEQ.                     EL183
00549      EXEC CICS READ                                               EL183
00550           DATASET  (CNTL-ID)                                      EL183
00551           SET      (ADDRESS OF CONTROL-FILE)                         CL**6
00552           RIDFLD   (CNTL-KEY)                                     EL183
00553           END-EXEC.                                               EL183
00554                                                                      CL**6
00555      MOVE CF-FORMS-PRINTER-ID    TO PI-PRINT-ID.                  EL183
00556                                                                      CL**7
00557  7820-START.                                                      EL183
00558                                                                      CL**7
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**7
00560 *        MOVE EIBTRMID       TO PI-PRINT-ID                          CL**7
00561          EXEC CICS START                                             CL**7
00562               INTERVAL  (0)                                          CL**7
00563               TRANSID   (PRINT-TRANS)                                CL**7
00564               FROM      (PROGRAM-INTERFACE-BLOCK)                    CL**7
00565               LENGTH    (PI-COMM-LENGTH)                             CL**7
00566 *             TERMID    (PI-PRINT-ID)                                CL**7
00567               END-EXEC                                               CL**7
00568      ELSE                                                            CL**7
00569          EXEC CICS START                                             CL**7
00570               INTERVAL  (0)                                          CL**7
00571               TRANSID   (PRINT-TRANS)                                CL**7
00572               FROM      (PROGRAM-INTERFACE-BLOCK)                    CL**7
00573               LENGTH    (PI-COMM-LENGTH)                             CL**7
00574               TERMID    (PI-PRINT-ID)                                CL**7
00575               END-EXEC.                                              CL**7
00576                                                                      CL**7
00577      IF OPTIONI = '5'                                             EL183
00578          MOVE ER-0553            TO EMI-ERROR                     EL183
00579      ELSE                                                         EL183
00580          MOVE ER-0554            TO EMI-ERROR.                    EL183
00581      MOVE -1                     TO OPTIONL.                      EL183
00582      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL183
00583      GO TO 8200-SEND-DATAONLY.                                    EL183
00584                                                                      CL**7
00585  7890-NOT-FOUND.                                                  EL183
00586      MOVE ER-0190                TO EMI-ERROR.                    EL183
00587      MOVE -1                     TO OPTIONL.                      EL183
00588      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                   EL183
00589      GO TO 8200-SEND-DATAONLY.                                    EL183
00590      EJECT                                                        EL183
00591  7900-WRITE-TD.                                                   EL183
00592      IF PI-COMPANY-ID NOT = 'FIA'                                 EL183
00593         GO TO 7900-EXIT.                                          EL183
00594                                                                      CL**7
00595      MOVE LA4-CARRIER            TO WS-CARRIER.                   EL183
00596      MOVE LA4-CLAIM-NO           TO WS-CLAIM.                     EL183
00597      MOVE LA4-CERT-NO            TO WS-CERT.                      EL183
00598      MOVE LA-ARCHIVE-NO          TO WS-ARCH-NO.                   EL183
00599      MOVE WS-COUNTER             TO WS-COUNT.                     EL183
00600                                                                      CL**7
00601      IF OPTIONI = '3'                                             EL183
00602          MOVE 'FIRST'            TO WS-KIND                       EL183
00603      ELSE                                                         EL183
00604          MOVE 'FOLLOW'           TO WS-KIND.                      EL183
00605                                                                      CL**7
00606      MOVE FORMTYPI               TO WS-TYPE.                      EL183
00607                                                                   EL183
00608      EXEC CICS WRITEQ TD                                             CL**7
00609           QUEUE     ('CSMT')                                         CL**7
00610           FROM      (WS-MESSAGE)                                     CL**7
00611           LENGTH    (TRANS-DATA-LENGTH)                              CL**7
00612      END-EXEC.                                                       CL**7
00613                                                                      CL**7
00614  7900-EXIT.                                                       EL183
00615       EXIT.                                                       EL183
00616                                                                      CL**7
00617  7910-WRITE-TD.                                                   EL183
00618      IF PI-COMPANY-ID NOT = 'FIA'                                 EL183
00619         GO TO 7910-EXIT.                                          EL183
00620                                                                      CL**7
00621      MOVE LA4-CARRIER            TO WS-CARRIER.                   EL183
00622      MOVE LA4-CLAIM-NO           TO WS-CLAIM.                     EL183
00623      MOVE LA4-CERT-NO            TO WS-CERT.                      EL183
00624      MOVE LA-ARCHIVE-NO          TO WS-ARCH-NO.                   EL183
00625      MOVE WS-COUNTER             TO WS-COUNT.                     EL183
00626                                                                   EL183
00627      MOVE 'ZEROED'               TO WS-KIND.                      EL183
00628      MOVE FORMTYPI               TO WS-TYPE.                      EL183
00629                                                                   EL183
00630      EXEC CICS WRITEQ TD                                             CL**7
00631          QUEUE     ('CSMT')                                          CL**7
00632          FROM      (WS-MESSAGE)                                      CL**7
00633          LENGTH    (TRANS-DATA-LENGTH)                               CL**7
00634      END-EXEC.                                                       CL**7
00635                                                                      CL**7
00636  7910-EXIT.                                                       EL183
00637       EXIT.                                                       EL183
00638      EJECT                                                        EL183
00639  8100-SEND-INITIAL-MAP.                                           EL183
00640      MOVE SAVE-DATE              TO DATEAO.                       EL183
00641      MOVE EIBTIME                TO TIME-IN.                      EL183
00642      MOVE TIME-OUT               TO TIMEAO.                       EL183
00643                                                                      CL**7
00644      IF NOT EMI-NO-ERRORS                                         EL183
00645          SET EMI-INDX TO 1                                        EL183
00646          MOVE EMI-MESSAGE-AREA (EMI-INDX) TO ERRMSGO              EL183
00647      ELSE                                                         EL183
00648          MOVE SPACES             TO ERRMSGO.                      EL183
00649                                                                      CL**7
00650      MOVE -1                     TO OPTIONL.                      EL183
00651      EXEC CICS SEND                                               EL183
00652          MAP     (MAP-NAME)                                       EL183
00653          MAPSET  (MAPSET-NAME)                                    EL183
00654          FROM    (EL183AO)                                        EL183
00655          ERASE                                                    EL183
00656          CURSOR                                                   EL183
00657          END-EXEC.                                                EL183
00658                                                                      CL**7
00659      GO TO 9100-RETURN-TRAN.                                      EL183
00660                                                                   EL183
00661  8200-SEND-DATAONLY.                                              EL183
00662      MOVE SAVE-DATE              TO DATEAO.                       EL183
00663      MOVE EIBTIME                TO TIME-IN.                      EL183
00664      MOVE TIME-OUT               TO TIMEAO.                       EL183
00665      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO                          CL**7
00666      EXEC CICS SEND                                               EL183
00667          MAP     (MAP-NAME)                                       EL183
00668          MAPSET  (MAPSET-NAME)                                    EL183
00669          FROM    (EL183AO)                                        EL183
00670          DATAONLY                                                 EL183
00671          ERASEAUP                                                 EL183
00672          CURSOR                                                   EL183
00673          END-EXEC.                                                EL183
00674                                                                      CL**7
00675      GO TO 9100-RETURN-TRAN.                                      EL183
00676                                                                   EL183
00677  8300-SEND-TEXT.                                                  EL183
00678      EXEC CICS SEND TEXT                                          EL183
00679          FROM    (LOGOFF-TEXT)                                    EL183
00680          LENGTH  (LOGOFF-LENGTH)                                  EL183
00681          ERASE                                                    EL183
00682          FREEKB                                                   EL183
00683          END-EXEC.                                                EL183
00684                                                                      CL**7
00685      EXEC CICS RETURN                                             EL183
00686          END-EXEC.                                                EL183
00687                                                                   EL183
00688  8600-DEEDIT.                                                     EL183
00689      EXEC CICS BIF DEEDIT                                         EL183
00690           FIELD   (DEEDIT-FIELD)                                  EL183
00691           LENGTH  (15)                                            EL183
00692           END-EXEC.                                               EL183
00693      EJECT                                                        EL183
00694  8800-UNAUTHORIZED-ACCESS.                                        EL183
00695      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL183
00696      GO TO 8300-SEND-TEXT.                                        EL183
00697                                                                   EL183
00698  8810-PF23.                                                       EL183
00699      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL183
00700      MOVE XCTL-005               TO PGM-NAME.                     EL183
00701      GO TO 9300-XCTL.                                             EL183
00702                                                                      CL**7
00703  8820-TERMID-ERROR.                                               EL183
00704      MOVE ER-0412                TO EMI-ERROR.                    EL183
00705      GO TO 8999-OPEN-ERROR.                                       EL183
00706                                                                      CL**7
00707  8830-TRANS-ERROR.                                                EL183
00708      MOVE ER-0413                TO EMI-ERROR.                    EL183
00709      GO TO 8999-OPEN-ERROR.                                       EL183
00710                                                                   EL183
00711  8840-CNTL-NOT-OPEN.                                              EL183
00712      MOVE ER-0042                TO EMI-ERROR.                    EL183
00713      GO TO 8999-OPEN-ERROR.                                       EL183
00714                                                                   EL183
00715  8850-ARCH-NOT-OPEN.                                              EL183
00716      MOVE ER-0408                TO EMI-ERROR.                    EL183
00717      GO TO 8999-OPEN-ERROR.                                       EL183
00718                                                                   EL183
00719  8870-ACTV-NOT-OPEN.                                              EL183
00720      MOVE ER-0172                TO EMI-ERROR.                    EL183
00721      GO TO 8999-OPEN-ERROR.                                       EL183
00722                                                                   EL183
00723  8999-OPEN-ERROR.                                                 EL183
00724      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL183
00725      MOVE -1                     TO OPTIONL.                      EL183
00726      GO TO 8200-SEND-DATAONLY.                                    EL183
00727                                                                      CL**7
00728  9000-RETURN-CICS.                                                EL183
00729      EXEC CICS RETURN                                             EL183
00730          END-EXEC.                                                EL183
00731                                                                   EL183
00732  9100-RETURN-TRAN.                                                EL183
00733      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL183
00734      MOVE MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.           EL183
00735      EXEC CICS RETURN                                             EL183
00736          TRANSID   (TRANS-ID)                                     EL183
00737          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL183
00738          LENGTH    (PI-COMM-LENGTH)                               EL183
00739          END-EXEC.                                                EL183
00740                                                                   EL183
00741  9200-RETURN-MAIN-MENU.                                           EL183
00742      MOVE XCTL-126             TO PGM-NAME.                       EL183
00743      GO TO 9300-XCTL.                                             EL183
00744                                                                   EL183
00745  9300-XCTL.                                                       EL183
00746      EXEC CICS XCTL                                               EL183
00747          PROGRAM   (PGM-NAME)                                     EL183
00748          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL183
00749          LENGTH    (PI-COMM-LENGTH)                               EL183
00750          END-EXEC.                                                EL183
00751                                                                   EL183
00752  9400-CLEAR.                                                      EL183
00753      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL183
00754      GO TO 9300-XCTL.                                             EL183
00755                                                                   EL183
00756  9500-PF12.                                                       EL183
00757      MOVE XCTL-010               TO PGM-NAME.                     EL183
00758      GO TO 9300-XCTL.                                             EL183
00759                                                                   EL183
00760  9600-PGMID-ERROR.                                                EL183
00761      EXEC CICS HANDLE CONDITION                                   EL183
00762          PGMIDERR  (8300-SEND-TEXT)                               EL183
00763          END-EXEC.                                                EL183
00764      MOVE ' '                    TO PI-ENTRY-CD-1.                EL183
00765      MOVE XCTL-005               TO PGM-NAME.                     EL183
00766      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL183
00767      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL183
00768      GO TO 9300-XCTL.                                             EL183
00769                                                                   EL183
00770  9700-DATE-LINK.                                                  EL183
00771      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL183
00772      EXEC CICS LINK                                               EL183
00773          PROGRAM   (PGM-NAME)                                     EL183
00774          COMMAREA  (DATE-CONVERSION-DATA)                         EL183
00775          LENGTH    (DC-COMM-LENGTH)                               EL183
00776          END-EXEC.                                                EL183
00777                                                                      CL**7
00778  9700-EXIT.                                                       EL183
00779       EXIT.                                                       EL183
00780                                                                   EL183
00781  9900-ERROR-FORMAT.                                               EL183
00782      IF NOT EMI-ERRORS-COMPLETE                                   EL183
00783          MOVE LINK-001           TO PGM-NAME                      EL183
00784          EXEC CICS LINK                                           EL183
00785              PROGRAM   (PGM-NAME)                                 EL183
00786              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL183
00787              LENGTH    (EMI-COMM-LENGTH)                          EL183
00788              END-EXEC.                                            EL183
00789  9900-EXIT.                                                       EL183
00790      EXIT.                                                        EL183
00791                                                                   EL183
00792  9990-ABEND.                                                      EL183
00793      MOVE LINK-004               TO PGM-NAME.                     EL183
00794      MOVE DFHEIBLK               TO EMI-LINE1.                    EL183
00795      EXEC CICS LINK                                               EL183
00796          PROGRAM   (PGM-NAME)                                     EL183
00797          COMMAREA  (EMI-LINE1)                                    EL183
00798          LENGTH    (72)                                           EL183
00799          END-EXEC.                                                EL183
00800      GO TO 8200-SEND-DATAONLY.                                    EL183
00801                                                                      CL**5
00802  9995-SECURITY-VIOLATION.                                            CL**5
00803                                  COPY ELCSCTP.                       CL**5
