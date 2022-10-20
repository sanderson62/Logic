00001  ID DIVISION.                                                     EL1284
00002                                                                   EL1284
00003  PROGRAM-ID.                 EL1284.                              EL1284
00008 *                                                                 EL1284
00009 *AUTHOR.           LOGIC,INC.                                     EL1284
00010 *                  DALLAS,TEXAS.                                  EL1284
00011                                                                   EL1284
00012 *DATE-COMPILED.                                                   EL1284
00013 *SECURITY.   *****************************************************EL1284
00014 *            *                                                   *EL1284
00015 *            * THIS PROGRAM IS THE PROPERTY OF LOCIC, INC. *      EL1284
00016 *            *                                                   *EL1284
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL1284
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL1284
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL1284
00020 *            *                                                   *EL1284
00021 *            *****************************************************EL1284
00022 *                                                                 EL1284
00023 *REMARKS.     TRANSACTION - EXXG - CLAIM MEMO MAINTENANCE.        EL1284
00024 *                                                                 EL1284
00025 ******************************************************************EL1284
00026 *                   C H A N G E   L O G                           EL1284
00027 *                                                                 EL1284
00028 * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.                EL1284
00029 *-----------------------------------------------------------------EL1284
00030 *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE            EL1284
00031 * EFFECTIVE    NUMBER                                             EL1284
00032 *-----------------------------------------------------------------EL1284
010416* 010416   2015072900002   TANA  NEW CLAIM MEMO SCREEN            EL1284
040416* 040416   2016021500002   TANA  CHANGE REWRITE FIELD VALID VALUESEL1284
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
00034 ******************************************************************EL1284
00035 *                                                                 EL1284
00036  ENVIRONMENT DIVISION.                                            EL1284
00037                                                                   EL1284
00038      EJECT                                                        EL1284
00039  DATA DIVISION.                                                   EL1284
00040  WORKING-STORAGE SECTION.                                         EL1284
00041  77  FILLER  PIC X(32)  VALUE '********************************'. EL1284
00042  77  FILLER  PIC X(32)  VALUE '*     EL1284 WORKING STORAGE   *'. EL1284
00043  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.005 ***********'. EL1284
00044                                                                   EL1284
00045      COPY ELCSCTM.                                                EL1284
00046      COPY ELCSCRTY.                                               EL1284
00047                                                                   EL1284
00048      EJECT                                                        EL1284
00049  01  STANDARD-AREAS.                                              EL1284
00050      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         EL1284
00051      12  MAP-NAME                PIC X(8)    VALUE 'EL1284A'.     EL1284
00052      12  MAPSET-NAME             PIC X(8)    VALUE 'EL1284S '.    EL1284
00053      12  SCRN-NUMBER             PIC X(4)    VALUE '128D'.        EL1284
00054      12  TRANS-ID                PIC X(4)    VALUE 'EXXG'.        EL1284
00055      12  THIS-PGM                PIC X(8)    VALUE 'EL1284'.      EL1284
00056      12  PGM-NAME                PIC X(8).                        EL1284
00061      12  PGM-EL1276              PIC X(8)    VALUE 'EL1276'.      EL1284
00062                                                                   EL1284
00063      12  ELMEMO-FILE-ID          PIC X(8)    VALUE 'ELMEMO'.      EL1284
00064      12  ELCERT-FILE-ID          PIC X(8)    VALUE 'ELCERT'.      EL1284
00065      12  QID.                                                     EL1284
00066          16  QID-TERM            PIC X(4).                        EL1284
00067          16  FILLER              PIC X(4)    VALUE '128D'.        EL1284
00068      12  QID-ITEM                PIC S9(4)   COMP VALUE +0.       EL1284
00069      12  SC-ITEM                 PIC S9(4)   COMP VALUE +1.       EL1284
00070      12  WS-LINE                 PIC 9(3)    VALUE 0.             EL1284
00074      12  WS-BLANK-LINES          PIC S9(4)   COMP VALUE +0.       EL1284
00075                                                                   EL1284
00076  01  WORK-AREA.                                                   EL1284
00077      12  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL1284
00078      12  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL1284
00079                                                                   EL1284
00080      12  ELMEMO-LENGTH           PIC S9(4)   COMP VALUE +132.     EL1284
00081      12  ELMEMO-KEY-LENGTH       PIC S9(4)   COMP VALUE +23.      EL1284
00082      12  ELMEMO-START-LENGTH     PIC S9(4)   COMP VALUE +21.      EL1284
00083      12  ELMEMO-KEY.                                              EL1284
00084          16  ELMEMO-PARTIAL-KEY.                                  EL1284
00085              20 ELMEMO-COMPANY-CD    PIC X.                       EL1284
00024              20 ELMEMO-RECORD-TYPE   PIC X.                       EL1284
00086              20 ELMEMO-CARRIER       PIC X.                       EL1284
00087              20 ELMEMO-CLAIM-NO      PIC  X(7).                   EL1284
00091              20 ELMEMO-CERT-NO.                                   EL1284
00092                 25 ELMEMO-CERT-PRIME PIC X(10).                   EL1284
00093                 25 ELMEMO-CERT-SFX   PIC X.                       EL1284
00095          16 ELMEMO-SEQ           PIC S9(4) COMP.                  EL1284
00096      12  SV-PRIOR-KEY.                                            EL1284
00097          20 SV-COMPANY-CD            PIC X.                       EL1284
00024          20 SV-RECORD-TYPE           PIC X.                       EL1284
00098          20 SV-CARRIER               PIC X.                       EL1284
00099          20 SV-CLAIM-NO              PIC  X(7).                   EL1284
00103          20 SV-CERT-NO.                                           EL1284
00104             25 SV-CERT-PRIME         PIC X(10).                   EL1284
00105             25 SV-CERT-SFX           PIC X(1).                    EL1284
00107      12  ELCERT-KEY.                                              EL1284
00108          16  ELCERT-COMPANY-CD        PIC X.                      EL1284
00109          16  ELCERT-CARRIER           PIC X.                      EL1284
00110          16  ELCERT-GROUPING          PIC X(6).                   EL1284
00111          16  ELCERT-STATE             PIC XX.                     EL1284
00112          16  ELCERT-ACCOUNT           PIC X(10).                  EL1284
00113          16  ELCERT-EFF-DT            PIC XX.                     EL1284
00114          16  ELCERT-CERT-NO.                                      EL1284
00115              20  ELCERT-CERT-PRIME    PIC X(10).                  EL1284
00116              20  ELCERT-CERT-SFX      PIC X.                      EL1284
00117                                                                   EL1284
00118      12  TIME-IN                 PIC S9(7).                       EL1284
00119      12  TIME-SPLIT REDEFINES TIME-IN.                            EL1284
00120          16  FILLER              PIC X.                           EL1284
00121          16  TIME-OUT            PIC 99V99.                       EL1284
00122          16  FILLER              PIC X(2).                        EL1284
00123      12  XCTL-005                PIC X(8)    VALUE 'EL005'.       EL1284
00124      12  XCTL-126                PIC X(8)    VALUE 'EL126'.       EL1284
00125      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL1284
00126      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL1284
00127      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL1284
00128      12  MAX-LINES               PIC 999     VALUE 300.           EL1284
00129      12  NUM-LINES-PER-SCREEN    PIC 99      VALUE 10.            EL1284
00130      12  TS-NUM-REC-IN-GROUP     PIC 99      VALUE 50.            EL1284
00131      12  TS-GROUP-WORK           PIC 9(5)    VALUE 0  COMP-3.     EL1284
00132      12  TS-LENGTH               PIC S9(4)   VALUE +3650 COMP.    EL1284
00133      12  ROLL-COUNTER            PIC S999    VALUE +0 COMP-3.     EL1284
00134      12  TEMP-CURR-LINE          PIC S9(3)   COMP-3.              EL1284
00135      12  WS-SUB                  PIC S9(3)   COMP-3.              EL1284
00136      12  WS-SUB1                 PIC S9(3)   COMP-3.              EL1284
00239      12  WS-ERASE-EOF            PIC X       VALUE X'80'.         EL1284
00240      12  DEEDIT-DATE-INPUT.                                       EL1284
00241          16  FILLER              PIC XX.                          EL1284
00242          16  DEEDIT-DATE         PIC X(6).                        EL1284
00137      12  WS-CURSOR-SET           PIC X       VALUE 'N'.           EL1284
00138          88 CURSOR-SET                       VALUE 'Y'.           EL1284
                                                                        EL1284
00137      12  WS-SCREEN-LINE          PIC X       VALUE 'N'.           EL1284
00138          88 SCREEN-LINE-FOUND                VALUE 'Y'.           EL1284
00139                                                                   EL1284
00208      12  ELMSTR-KEY.                                              EL1284
00209          16  MSTR-COMP-CD    PIC X.                               EL1284
00210          16  MSTR-CARRIER    PIC X.                               EL1284
00211          16  MSTR-CLAIM-NO   PIC X(7).                            EL1284
00212          16  MSTR-CERT-NO.                                        EL1284
00213              20  MSTR-CERT-NO-PRIME  PIC X(10).                   EL1284
00214              20  MSTR-CERT-NO-SUFX   PIC X.                       EL1284
                                                                        EL1284
00552  01  COMP-LENGTHS.                                                EL1284
00553      12  CNTL-GENERIC-LENGTH     PIC S9(4)   COMP VALUE +8.       EL1284
00554      12  JOURNAL-LENGTH          PIC S9(4)   COMP VALUE +0.       EL1284
00555      12  DATE-LENGTH             PIC S9(4)   COMP VALUE +8.       EL1284
00556      12  MO-YR-LENGTH            PIC S9(4)   COMP VALUE +5.       EL1284
00557      12  TERM-LENGTH             PIC S9(4)   COMP VALUE +3.       EL1284
00558      12  BEN-LENGTH              PIC S9(4)   COMP VALUE +11.      EL1284
00559      12  FREQ-LENGTH             PIC S9(4)   COMP VALUE +2.       EL1284
00560      12  MSTR-LENGTH             PIC S9(4)   COMP VALUE +350.     EL1284
00561      12  CERT-LENGTH             PIC S9(4)   COMP VALUE +450.     EL1284
00562      12  TRLR-LENGTH             PIC S9(4)   COMP VALUE +200.     EL1284
00563      12  ACTQ-LENGTH             PIC S9(4)   COMP VALUE +60.      EL1284
00564      12  CHKQ-LENGTH             PIC S9(4)   COMP VALUE +100.     EL1284
00565      12  ARCH-LENGTH             PIC S9(4)   COMP VALUE +90.      EL1284
00566      12  ALPH-LENGTH             PIC S9(4)   COMP VALUE +128.     EL1284
                                                                        EL1284
00159      EJECT                                                        EL1284
00160  01  ERROR-MESSAGES.                                              EL1284
00161      12  ER-0000             PIC X(04)       VALUE '0000'.        EL1284
00162      12  ER-0004             PIC X(04)       VALUE '0004'.        EL1284
00163      12  ER-0006             PIC X(04)       VALUE '0006'.        EL1284
00164      12  ER-0008             PIC X(04)       VALUE '0008'.        EL1284
00165      12  ER-0023             PIC X(04)       VALUE '0023'.        EL1284
00166      12  ER-0029             PIC X(04)       VALUE '0029'.        EL1284
00167      12  ER-0030             PIC X(04)       VALUE '0030'.        EL1284
00168      12  ER-0031             PIC X(04)       VALUE '0031'.        EL1284
00169      12  ER-0032             PIC X(04)       VALUE '0032'.        EL1284
00170      12  ER-0033             PIC X(04)       VALUE '0033'.        EL1284
00171      12  ER-0041             PIC X(04)       VALUE '0041'.        EL1284
00172      12  ER-0044             PIC X(04)       VALUE '0044'.        EL1284
00173      12  ER-0045             PIC X(04)       VALUE '0045'.        EL1284
00173      12  ER-0046             PIC X(04)       VALUE '0046'.        EL1284
00174      12  ER-0047             PIC X(04)       VALUE '0047'.        EL1284
00175      12  ER-0048             PIC X(04)       VALUE '0048'.        EL1284
00176      12  ER-0049             PIC X(04)       VALUE '0049'.        EL1284
00177      12  ER-0050             PIC X(04)       VALUE '0050'.        EL1284
00178      12  ER-0051             PIC X(04)       VALUE '0051'.        EL1284
00179      12  ER-0066             PIC X(04)       VALUE '0066'.        EL1284
00180      12  ER-0067             PIC X(04)       VALUE '0067'.        EL1284
00181      12  ER-0069             PIC X(04)       VALUE '0069'.        EL1284
00182      12  ER-0070             PIC X(04)       VALUE '0070'.        EL1284
00183      12  ER-0140             PIC X(04)       VALUE '0140'.        EL1284
           12  ER-0314             PIC X(04)       VALUE '0314'.        EL1284
00184      12  ER-2954             PIC X(04)       VALUE '2954'.        EL1284
040416     12  ER-3846             PIC X(04)       VALUE '3846'.        EL1284

00185      EJECT                                                        EL1284
00186                         COPY ELCLOGOF.                            EL1284
00187      EJECT                                                        EL1284
00188                         COPY ELCAID.                              EL1284
00189  01  FILLER  REDEFINES DFHAID.                                    EL1284
00190      12  FILLER                  PIC X(8).                        EL1284
00191      12  PF-VALUES OCCURS 24 TIMES       PIC X.                   EL1284
00192      EJECT                                                        EL1284
00193                         COPY ELCEMIB.                             EL1284
00194      EJECT                                                        EL1284
00195                         COPY ELCINTF.                             EL1284
00196      EJECT                                                        EL1284
00197      12  PI-EL1284-AREA    REDEFINES PI-PROGRAM-WORK-AREA.        EL1284
00198          16  FILLER              PIC X(318).                      EL1284
00199          16  PI-TOTAL-LINES      PIC S9(3).                       EL1284
00200          16  PI-CURRENT-LINE     PIC S9(3)   COMP-3.              EL1284
00201          16  PI-TEMP-STOR-ITEMS  PIC S9(4)   COMP.                EL1284
00202          16  PI-UPDATE-SW        PIC X.                           EL1284
00203              88  PI-CHANGES-MADE             VALUE '1'.           EL1284
               16  PI-LONG-HLTH-APP    PIC  X(01).                      EL1284
               16  PI-REWRITE          PIC  X(01).                      EL1284
               16  PI-CHKCOVG          PIC  X(01).                      EL1284
               16  PI-MR-RELEASE-DATE  PIC  X(08).                      EL1284
00214          16  PI-BILLING-NOTES-EXIST PIC X.                        EL1284
00215          16  PI-CERT-NOTES-EXIST    PIC X.                        EL1284
00216          16  PI-CLAIM-NOTES-EXIST   PIC X.                        EL1284
00217          16  PI-SET-NOTE-CHANGE  PIC X.                           EL1284
00218              88 PI-CHANGE-IN-NOTE-TYPE       VALUE 'Y'.           EL1284
00219      EJECT                                                        EL1284
00220                            COPY ELCATTR.                          EL1284
00221      EJECT                                                        EL1284
                                                                        EL1284
00222                            COPY ELCDATE.                          EL1284
00223      EJECT                                                        EL1284
00224                            COPY EL1284S.                          EL1284
00225      EJECT                                                        EL1284
00226 *01  EL1284R REDEFINES EL1284AI.                                  EL1284
       66 GROUP2  RENAMES LN1L THRU MTDT10I.                            EL1284
       01  EL1284R.                                                     EL1284
00227 *    12  FILLER                  PIC X(202).                      EL1284
00228      12  SC-ALL-LINES.                                            EL1284
00229       14 SC-LINES OCCURS 10 TIMES INDEXED BY SC-INDX.             EL1284
00230          16  SC-LINL             PIC S9(4)   COMP.                EL1284
00231          16  SC-LINA             PIC X.                           EL1284
00232          16  SC-LIN              PIC ZZ9.                         EL1284
00233          16  SC-TEXTL            PIC S9(4)   COMP.                EL1284
00234          16  SC-TEXTA            PIC X.                           EL1284
00235          16  SC-TEXT             PIC X(63).                       EL1284
00233          16  SC-MTBYL            PIC S9(4)   COMP.                EL1284
00234          16  SC-MTBYA            PIC X.                           EL1284
00235          16  SC-MTBY             PIC X(4).                        EL1284
00233          16  SC-MTDTL            PIC S9(4)   COMP.                EL1284
00234          16  SC-MTDTA            PIC X.                           EL1284
00235          16  SC-MTDT             PIC X(6).                        EL1284
00236 *    12  FILLER                  PIC X(116).                      EL1284
00237      EJECT                                                        EL1284
00238  01  RECORD-TABLE                PIC X(21900) VALUE SPACES.       EL1284
00239  01  REC-TABLE  REDEFINES RECORD-TABLE.                           EL1284
00240      12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-INDX PIC X(3650).  EL1284
00241  01  REC-ENTRIES REDEFINES RECORD-TABLE.                          EL1284
00242      12  REC-ENT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.    EL1284
00243          16  REC-TEXT                    PIC X(63).               EL1284
00244          16  REC-LAST-MAINT-BY           PIC XXXX.                EL1284
00245          16  REC-LAST-MAINT-DT           PIC XX.                  EL1284
00246          16  REC-LAST-MAINT-HHMMSS       PIC S9(7) COMP-3.        EL1284
00247                                                                   EL1284
00248  01  TS-WORK-AREA                        PIC X(3650).             EL1284
00249      EJECT                                                        EL1284
00257          COPY ELCMSTR.                                            EL1284
                                                                        EL1284
00251  LINKAGE SECTION.                                                 EL1284
00252  01  DFHCOMMAREA                 PIC X(1500).                     EL1284
00253                                                                   EL1284
00250          COPY ELCMEMO.                                            EL1284
00255                                                                   EL1284
00256          COPY ELCCERT.                                            EL1284
00258  PROCEDURE DIVISION.                                              EL1284
00259                                                                   EL1284
00260      MOVE EIBTRMID               TO QID-TERM.                     EL1284
00261      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL1284
00262      MOVE '5'                    TO DC-OPTION-CODE.               EL1284
00263      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1284
00264      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL1284
00265      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL1284
00266                                                                   EL1284
00267      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK                  EL1284
00268      IF EIBCALEN = ZEROS                                          EL1284
00269          GO TO 8800-UNAUTHORIZED-ACCESS                           EL1284
00270      END-IF.                                                      EL1284
00271                                                                   EL1284
00272 *    IF PI-CALLING-PROGRAM NOT = THIS-PGM AND PGM-EL1276          EL1284
00274 *        PERFORM 4900-SET-NOTES-FLAG THRU 4900-EXIT               EL1284
00275 *        IF CLAIM-SESSION                                         EL1284
00276 *            SET PI-CLAIM-NOTE TO TRUE                            EL1284
00277 *        ELSE                                                     EL1284
00278 *            SET PI-CERT-NOTE TO TRUE                             EL1284
00279 *        END-IF                                                   EL1284
00280 *    END-IF.                                                      EL1284
00281                                                                   EL1284
00282 *    IF PI-CALLING-PROGRAM = PGM-EL1276                           EL1284
00283 *        MOVE THIS-PGM           TO PI-CALLING-PROGRAM            EL1284
00284 *        MOVE 'N'                TO PI-PF5-PRESSED                EL1284
00285 *        MOVE 'N'                TO PI-PF6-PRESSED                EL1284
00286 *    END-IF.                                                      EL1284
00287                                                                   EL1284
00288      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL1284
00289         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                    EL1284
00290            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6        EL1284
00291            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5        EL1284
00292            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4        EL1284
00293            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3        EL1284
00294            MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2        EL1284
00295            MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1        EL1284
00296            MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM      EL1284
00297            MOVE THIS-PGM TO PI-CALLING-PROGRAM                    EL1284
00298         ELSE                                                      EL1284
00299            MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM        EL1284
00300            MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM      EL1284
00301            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1        EL1284
00302            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2        EL1284
00303            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3        EL1284
00304            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4        EL1284
00305            MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5        EL1284
00306            MOVE SPACES               TO PI-SAVED-PROGRAM-6        EL1284
00307         END-IF                                                    EL1284
00308      END-IF.                                                      EL1284
00309                                                                   EL1284
00310  1000-START.                                                      EL1284
00137      MOVE 'N' TO WS-CURSOR-SET                                    EL1284
00311                                                                   EL1284
00312      MOVE LOW-VALUES TO EL1284AI                                  EL1284
                              EL1284R                                   EL1284
00313      MOVE SPACES                 TO  ELMEMO-KEY                   EL1284
00314                                      ELCERT-KEY.                  EL1284
00315                                                                   EL1284
00316      MOVE PI-COMPANY-CD          TO  ELMEMO-COMPANY-CD            EL1284
00317                                      ELCERT-COMPANY-CD.           EL1284
00318      MOVE PI-CARRIER             TO  ELMEMO-CARRIER               EL1284
00319                                      ELCERT-CARRIER.              EL1284
           MOVE PI-CLAIM-NO            TO  ELMEMO-CLAIM-NO              EL1284
00328      MOVE PI-CERT-PRIME          TO  ELMEMO-CERT-PRIME            EL1284
00329                                      ELCERT-CERT-PRIME.           EL1284
00330      MOVE PI-CERT-SFX            TO  ELMEMO-CERT-SFX              EL1284
00331                                      ELCERT-CERT-SFX.             EL1284
00332      MOVE ZEROS                  TO  ELMEMO-RECORD-TYPE           EL1284
00333      MOVE ZEROS                  TO  ELMEMO-SEQ.                  EL1284
00334      MOVE ELMEMO-PARTIAL-KEY     TO  SV-PRIOR-KEY.                EL1284
00335                                                                   EL1284
00336      IF EIBTRNID NOT = TRANS-ID                                   EL1284
              INITIALIZE PI-EL1284-AREA                                 EL1284
00337         MOVE '0'                 TO  PI-UPDATE-SW                 EL1284
00338         IF PI-PROCESSOR-ID NOT = 'LGXX'                           EL1284
00339             EXEC CICS READQ TS                                    EL1284
00340                      QUEUE (PI-SECURITY-TEMP-STORE-ID)            EL1284
00341                      INTO (SECURITY-CONTROL)                      EL1284
00342                      LENGTH (SC-COMM-LENGTH)                      EL1284
00343                      ITEM  (SC-ITEM)                              EL1284
00344             END-EXEC                                              EL1284
00345           MOVE SC-CREDIT-DISPLAY (32) TO PI-DISPLAY-CAP           EL1284
00346           MOVE SC-CREDIT-UPDATE (32)  TO PI-MODIFY-CAP            EL1284
00347         END-IF                                                    EL1284
00348      END-IF.                                                      EL1284
00349                                                                   EL1284
00350      IF NOT DISPLAY-CAP                                           EL1284
00351          MOVE 'READ'             TO  SM-READ                      EL1284
00352          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL1284
00353          MOVE ER-0070            TO  EMI-ERROR                    EL1284
00354          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1284
00355          GO TO 8100-SEND-INITIAL-MAP                              EL1284
00356      END-IF.                                                      EL1284
00357                                                                   EL1284
00358      EXEC CICS HANDLE AID                                         EL1284
00359           CLEAR(9400-CLEAR)                                       EL1284
00360      END-EXEC.                                                    EL1284
00361                                                                   EL1284
00362      EXEC CICS HANDLE CONDITION                                   EL1284
00363           ERROR(9990-ABEND)                                       EL1284
00364           PGMIDERR(9600-PGMID-ERROR)                              EL1284
00365      END-EXEC.                                                    EL1284
00366                                                                   EL1284
00367      IF EIBTRNID NOT = TRANS-ID                                   EL1284
00368          GO TO 7000-BUILD-TABLE                                   EL1284
00369      END-IF.                                                      EL1284
00370                                                                   EL1284
00371      IF PI-CHANGE-IN-NOTE-TYPE                                    EL1284
00372          GO TO 7000-BUILD-TABLE                                   EL1284
00373      END-IF.                                                      EL1284
00374                                                                   EL1284
00375      EJECT                                                        EL1284
00376  2000-RECEIVE.                                                    EL1284
00377      IF EIBAID = DFHPA1 OR                                        EL1284
00378         EIBAID = DFHPA2 OR                                        EL1284
00379         EIBAID = DFHPA3                                           EL1284
00380            MOVE ER-0008 TO EMI-ERROR                              EL1284
00381            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1284
00382            GO TO 8200-SEND-DATAONLY                               EL1284
00383      END-IF.                                                      EL1284
00384                                                                   EL1284
00385      EXEC CICS RECEIVE                                            EL1284
00386           MAP(MAP-NAME)                                           EL1284
00387           MAPSET(MAPSET-NAME)                                     EL1284
00388           INTO(EL1284AI)                                          EL1284
00389      END-EXEC.                                                    EL1284
           MOVE GROUP2 TO EL1284R.                                      EL1284
                                                                        EL1284
00390                                                                   EL1284
00391      IF PFENTERL = ZEROS                                          EL1284
00392         GO TO 2001-CHECK-PFKEYS                                   EL1284
00393      END-IF.                                                      EL1284
00394                                                                   EL1284
00395      IF EIBAID NOT = DFHENTER                                     EL1284
00396         MOVE ER-0004             TO EMI-ERROR                     EL1284
00397         GO TO 2002-INPUT-ERROR                                    EL1284
00398      END-IF.                                                      EL1284
00399                                                                   EL1284
00400      IF PFENTERI NUMERIC AND                                      EL1284
00401         (PFENTERI > 00 AND  < 25)                                 EL1284
00402         MOVE PF-VALUES (PFENTERI) TO EIBAID                       EL1284
00403      ELSE                                                         EL1284
00404         MOVE ER-0029 TO EMI-ERROR                                 EL1284
00405         GO TO 2002-INPUT-ERROR                                    EL1284
00406      END-IF.                                                      EL1284
00407                                                                   EL1284
00408  2001-CHECK-PFKEYS.                                               EL1284
00409      IF EIBAID = DFHPF23                                          EL1284
00410         GO TO 9000-RETURN-CICS                                    EL1284
00411      END-IF.                                                      EL1284
00412                                                                   EL1284
00413      IF EIBAID = DFHPF24                                          EL1284
00414         GO TO 9200-RETURN-MAIN-MENU                               EL1284
00415      END-IF.                                                      EL1284
00416                                                                   EL1284
00417      IF EIBAID = DFHPF12                                          EL1284
00418         GO TO 9500-PF12                                           EL1284
00419      END-IF.                                                      EL1284
00420                                                                   EL1284
00456      IF FUNCTL NOT = ZEROS AND EIBAID NOT = DFHENTER              EL1284
00457         IF FUNCTI = 'A' OR = SPACES                               EL1284
00458            NEXT SENTENCE                                          EL1284
00459         ELSE                                                      EL1284
00460            MOVE ER-0050          TO EMI-ERROR                     EL1284
00461            MOVE -1 TO FUNCTL                                      EL1284
00462            MOVE AL-UABON TO FUNCTA PFENTERA                       EL1284
00463            GO TO 2002-INPUT-ERROR                                 EL1284
00464         END-IF                                                    EL1284
00465      END-IF.                                                      EL1284
00466                                                                   EL1284
00467      IF EIBAID = DFHPF1                                           EL1284
00468         MOVE NUM-LINES-PER-SCREEN TO ROLL-COUNTER                 EL1284
00469         GO TO 7400-PAGE-ROUTINE                                   EL1284
00470      END-IF.                                                      EL1284
00471                                                                   EL1284
00472      IF EIBAID = DFHPF2                                           EL1284
00473         SUBTRACT NUM-LINES-PER-SCREEN FROM ROLL-COUNTER           EL1284
00474         GO TO 7400-PAGE-ROUTINE                                   EL1284
00475      END-IF.                                                      EL1284
00476                                                                   EL1284
00477      IF EIBAID = DFHPF3                                           EL1284
00478         MOVE 5                   TO ROLL-COUNTER                  EL1284
00479         GO TO 7400-PAGE-ROUTINE                                   EL1284
00480      END-IF.                                                      EL1284
00481                                                                   EL1284
00482      IF EIBAID = DFHPF4                                           EL1284
00483         MOVE -5                  TO ROLL-COUNTER                  EL1284
00484         GO TO 7400-PAGE-ROUTINE                                   EL1284
00485      END-IF.                                                      EL1284
00486                                                                   EL1284
00487      IF EIBAID = DFHENTER                                         EL1284
00488         GO TO 2003-EDIT-DATA                                      EL1284
00489      END-IF.                                                      EL1284
00490                                                                   EL1284
00491      MOVE ER-0029                TO EMI-ERROR.                    EL1284
00492                                                                   EL1284
00493  2002-INPUT-ERROR.                                                EL1284
00494      MOVE -1                     TO PFENTERL                      EL1284
00495      MOVE AL-UNBON               TO PFENTERA                      EL1284
00496      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL1284
00497      GO TO 8200-SEND-DATAONLY.                                    EL1284
00498                                                                   EL1284
00499  2003-EDIT-DATA.                                                  EL1284
00500                                                                   EL1284
00501      IF FUNCTI = 'L'                                              EL1284
00502          NEXT SENTENCE                                            EL1284
00503      ELSE                                                         EL1284
00504          IF NOT MODIFY-CAP                                        EL1284
00505              MOVE 'UPDATE'       TO  SM-READ                      EL1284
00506              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       EL1284
00507              MOVE ER-0070        TO  EMI-ERROR                    EL1284
00508              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL1284
00509              GO TO 8100-SEND-INITIAL-MAP                          EL1284
00510          END-IF                                                   EL1284
00511      END-IF.                                                      EL1284
                                                                        EL1284
040416     IF FUNCTI NOT = 'Q'                                          EL1284
              PERFORM 2010-EDIT-CHECK                                   EL1284
040416     END-IF.                                                      EL1284
00512                                                                   EL1284
00513      IF FUNCTL = ZEROS OR FUNCTI = SPACES                         EL1284
00514         GO TO 4000-CHANGE-ROUTINE                                 EL1284
00515      END-IF.                                                      EL1284
00516                                                                   EL1284
00517      IF (FUNCTI = 'S' OR = 'D' OR = 'Q' OR                        EL1284
00518                 = 'I' OR = 'A' OR = 'L')                          EL1284
00519          NEXT SENTENCE                                            EL1284
00520      ELSE                                                         EL1284
00521          MOVE ER-0023            TO EMI-ERROR                     EL1284
00522          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1284
00523          MOVE AL-UABON           TO FUNCTA                        EL1284
00524          MOVE -1                 TO FUNCTL                        EL1284
00525          GO TO 8200-SEND-DATAONLY                                 EL1284
00526      END-IF.                                                      EL1284
00527                                                                   EL1284
00528      IF FUNCTI = 'D'  OR = 'I' OR = 'L'                           EL1284
00529         PERFORM 2500-LINE-CHECK THRU 2599-EXIT                    EL1284
00530      ELSE                                                         EL1284
00531         IF LINE1L NOT = ZEROS OR                                  EL1284
00532            LINE2L NOT = ZEROS                                     EL1284
00533            MOVE ER-0030          TO EMI-ERROR                     EL1284
00534            MOVE -1               TO LINE1L                        EL1284
00535            MOVE AL-UNBON         TO LINE1A LINE2A                 EL1284
00536            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1284
00537            GO TO 8200-SEND-DATAONLY                               EL1284
00538         END-IF                                                    EL1284
00539      END-IF.                                                      EL1284
                                                                        EL1284
00540                                                                   EL1284
00541      IF FUNCTI = 'A'                                              EL1284
00542         GO TO 5000-ADD-NEW-LINES                                  EL1284
00543      END-IF.                                                      EL1284
00544      IF FUNCTI = 'Q'                                              EL1284
00545         GO TO 9410-RETURN                                         EL1284
00546      END-IF.                                                      EL1284
00547      IF FUNCTI = 'S'                                              EL1284
00548         GO TO 4500-SAVE-DATA                                      EL1284
00549      END-IF.                                                      EL1284
00550      IF PI-TOTAL-LINES = 0                                        EL1284
00551         MOVE ER-0048             TO EMI-ERROR                     EL1284
00552         MOVE -1                  TO FUNCTL                        EL1284
00553         MOVE AL-UNBON            TO FUNCTA                        EL1284
00554         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1284
00555         GO TO 8200-SEND-DATAONLY                                  EL1284
00556      END-IF.                                                      EL1284
00557      IF FUNCTI = 'L'                                              EL1284
00558         GO TO 5500-LOOKUP                                         EL1284
00559      END-IF.                                                      EL1284
00560      IF FUNCTI = 'D'                                              EL1284
00561         GO TO 3000-DELETE-LINES                                   EL1284
00562      END-IF.                                                      EL1284
00563                                                                   EL1284
00564      GO TO 3500-INSERT-LINES.                                     EL1284

00565      EJECT                                                        EL1284
00566  2010-EDIT-CHECK.                                                 EL1284
      *
           IF LNGHLAPL > 0
              MOVE LNGHLAPI TO  PI-LONG-HLTH-APP
              MOVE '1'      TO PI-UPDATE-SW
           ELSE
              IF LNGHLAPA = WS-ERASE-EOF
                 MOVE SPACES TO PI-LONG-HLTH-APP
                 MOVE '1'    TO PI-UPDATE-SW
              END-IF
              MOVE PI-LONG-HLTH-APP TO LNGHLAPI
           END-IF.
      *
           IF LNGHLAPI > SPACE
              IF LNGHLAPI = 'N' OR 'Y'
                 CONTINUE
              ELSE
                 MOVE -1                     TO LNGHLAPL                EL1284
                 MOVE AL-UABON               TO LNGHLAPA                EL1284
                 MOVE ER-0046                TO EMI-ERROR               EL1284
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1284
                 GO TO 8200-SEND-DATAONLY                               EL1284
              END-IF
           END-IF.

      *
           IF REWRITEL > 0
              MOVE REWRITEI TO PI-REWRITE
              MOVE '1'      TO PI-UPDATE-SW
           ELSE
              IF REWRITEA = WS-ERASE-EOF
                 MOVE SPACES TO PI-REWRITE
                 MOVE '1'    TO PI-UPDATE-SW
              END-IF
              MOVE PI-REWRITE TO REWRITEI
           END-IF
      *
           IF REWRITEI > SPACE
040416        IF REWRITEI = 'N' OR 'Y' OR 'D'
                 CONTINUE
              ELSE
                 MOVE -1                     TO REWRITEL                EL1284
                 MOVE AL-UABON               TO REWRITEA                EL1284
040416           MOVE ER-3846                TO EMI-ERROR               EL1284
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1284
                 GO TO 8200-SEND-DATAONLY                               EL1284
              END-IF
           END-IF.
      *
           IF CHKCOVGL > 0
              MOVE CHKCOVGI TO PI-CHKCOVG
              MOVE '1'      TO PI-UPDATE-SW
           ELSE
              IF CHKCOVGA = WS-ERASE-EOF
                 MOVE SPACES TO PI-CHKCOVG
                 MOVE '1'    TO PI-UPDATE-SW
              END-IF
              MOVE PI-CHKCOVG TO CHKCOVGI
           END-IF
      *
           IF CHKCOVGI > SPACE
              IF CHKCOVGI = 'N' OR 'Y'
                 CONTINUE
              ELSE
                 MOVE -1                     TO CHKCOVGL                EL1284
                 MOVE AL-UABON               TO CHKCOVGA                EL1284
                 MOVE ER-0046                TO EMI-ERROR               EL1284
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1284
                 GO TO 8200-SEND-DATAONLY                               EL1284
              END-IF
           END-IF.
      *
           IF MRRELDTL > 0
              MOVE MRRELDTI TO PI-MR-RELEASE-DATE
              MOVE '1'      TO PI-UPDATE-SW
           ELSE
              IF MRRELDTA = WS-ERASE-EOF
                 MOVE SPACES TO PI-MR-RELEASE-DATE
                 MOVE '1'    TO PI-UPDATE-SW
              END-IF
              MOVE PI-MR-RELEASE-DATE TO MRRELDTI
           END-IF.
      *
00566  2500-LINE-CHECK.                                                 EL1284
00567      IF LINE1L = ZEROS AND                                        EL1284
00568         LINE2L = ZEROS                                            EL1284
00569         MOVE ER-0069             TO EMI-ERROR                     EL1284
00570         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1284
00571         MOVE -1                  TO LINE1L                        EL1284
00572         GO TO 8200-SEND-DATAONLY                                  EL1284
00573      END-IF.                                                      EL1284
00574                                                                   EL1284
00575      IF LINE1L NOT = ZEROS                                        EL1284
00576         IF LINE1I = ZERO AND FUNCTI EQUAL 'L'                     EL1284
00577             MOVE 1               TO LINE1I                        EL1284
00578         END-IF                                                    EL1284
00579         IF LINE1I = ZERO AND FUNCTI EQUAL 'D'                     EL1284
00580            MOVE ER-0049          TO EMI-ERROR                     EL1284
00581            MOVE AL-UNBON         TO LINE1A                        EL1284
00582            MOVE -1               TO LINE1L                        EL1284
00583            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1284
00584            GO TO 8200-SEND-DATAONLY                               EL1284
00585         END-IF                                                    EL1284
00586         IF LINE1I NOT NUMERIC OR                                  EL1284
00587            LINE1I > PI-TOTAL-LINES                                EL1284
00588            MOVE ER-0031          TO EMI-ERROR                     EL1284
00589            MOVE AL-UNBON         TO LINE1A                        EL1284
00590            MOVE -1               TO LINE1L                        EL1284
00591            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1284
00592            GO TO 8200-SEND-DATAONLY                               EL1284
00593         ELSE                                                      EL1284
00594            IF LINE2L = ZEROS                                      EL1284
00595               MOVE 1             TO LINE2I                        EL1284
00596               IF FUNCTI = 'I'                                     EL1284
00597                   GO TO 2510-MAX-CHECK                            EL1284
00598               ELSE                                                EL1284
00599                   NEXT SENTENCE                                   EL1284
00600               END-IF                                              EL1284
00601            ELSE                                                   EL1284
00602               IF FUNCTI = 'I'                                     EL1284
00603                  GO TO 2510-MAX-CHECK                             EL1284
00604               ELSE                                                EL1284
00605                  IF LINE2I NOT NUMERIC                            EL1284
00606                     MOVE AL-UNBON TO LINE2A                       EL1284
00607                     MOVE ER-0032  TO EMI-ERROR                    EL1284
00608                     MOVE -1       TO LINE2L                       EL1284
00609                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT      EL1284
00610                     GO TO 8200-SEND-DATAONLY                      EL1284
00611                  ELSE                                             EL1284
00612                     NEXT SENTENCE                                 EL1284
00613                  END-IF                                           EL1284
00614               END-IF                                              EL1284
00615            END-IF                                                 EL1284
00616         END-IF                                                    EL1284
00617      ELSE                                                         EL1284
00618         IF LINE2L = ZEROS                                         EL1284
00619            NEXT SENTENCE                                          EL1284
00620         ELSE                                                      EL1284
00621            MOVE -1               TO LINE2L                        EL1284
00622            MOVE ER-0041          TO EMI-ERROR                     EL1284
00623            MOVE AL-UNBON         TO LINE2A                        EL1284
00624            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1284
00625            GO TO 8200-SEND-DATAONLY                               EL1284
00626         END-IF                                                    EL1284
00627      END-IF.                                                      EL1284
00628      GO TO 2599-EXIT.                                             EL1284
00629  2510-MAX-CHECK.                                                  EL1284
00630      IF LINE2I NOT NUMERIC                                        EL1284
00631         MOVE -1                  TO LINE2L                        EL1284
00632         MOVE ER-0032             TO EMI-ERROR                     EL1284
00633         MOVE AL-UNBON            TO LINE2A                        EL1284
00634         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1284
00635         GO TO 8200-SEND-DATAONLY                                  EL1284
00636      ELSE                                                         EL1284
00637         COMPUTE ROLL-COUNTER = LINE2I + PI-TOTAL-LINES            EL1284
00638         IF ROLL-COUNTER GREATER THAN MAX-LINES                    EL1284
00639            MOVE -1               TO LINE2L                        EL1284
00640            MOVE ER-0044          TO EMI-ERROR                     EL1284
00641            MOVE AL-UNBON         TO LINE2A                        EL1284
00642            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1284
00643            GO TO 8200-SEND-DATAONLY                               EL1284
00644         END-IF                                                    EL1284
00645      END-IF.                                                      EL1284
00646  2599-EXIT.                                                       EL1284
00647       EXIT.                                                       EL1284
00648      EJECT                                                        EL1284
00649  3000-DELETE-LINES.                                               EL1284
00650      IF LINE2L = ZEROS AND LINE2I = 1                             EL1284
00651         MOVE LINE1I              TO LINE2I                        EL1284
00652      END-IF.                                                      EL1284
00653                                                                   EL1284
00654      IF LINE2I > PI-TOTAL-LINES OR < LINE1I                       EL1284
00655         MOVE ER-0049             TO EMI-ERROR                     EL1284
00656         MOVE AL-UNBON            TO LINE2A                        EL1284
00657         MOVE -1                  TO LINE2L                        EL1284
00658         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1284
00659         GO TO 8200-SEND-DATAONLY                                  EL1284
00660      END-IF.                                                      EL1284
00661                                                                   EL1284
00662      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1284
00663      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1284
00664              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1284
00665              SC-INDX > NUM-LINES-PER-SCREEN                       EL1284
00666      SET TB-INDX TO LINE1I.                                       EL1284
00667                                                                   EL1284
00668      IF NOT EMI-NO-ERRORS                                         EL1284
00669         GO TO 8200-SEND-DATAONLY                                  EL1284
00670      END-IF.                                                      EL1284
00671                                                                   EL1284
00672      SET TB-INDX TO LINE1I                                        EL1284
00673      COMPUTE ROLL-COUNTER = LINE2I - LINE1I + 1.                  EL1284
00674                                                                   EL1284
00675      IF LINE2I NOT = PI-TOTAL-LINES                               EL1284
00676         SET TB-INDX1 TO LINE2I                                    EL1284
00677         SET TB-INDX1 UP BY 1                                      EL1284
00678         PERFORM 3100-DELETE-TABLE-ENTRIES                         EL1284
00679                 UNTIL TB-INDX1 > PI-TOTAL-LINES                   EL1284
00680      END-IF.                                                      EL1284
00681                                                                   EL1284
00682      PERFORM 3150-BLANK-TABLE-ENTRIES                             EL1284
00683              ROLL-COUNTER TIMES.                                  EL1284
00684      SUBTRACT ROLL-COUNTER FROM PI-TOTAL-LINES.                   EL1284
00685                                                                   EL1284
00686      IF PI-CURRENT-LINE > PI-TOTAL-LINES                          EL1284
00687         MOVE PI-TOTAL-LINES      TO PI-CURRENT-LINE               EL1284
00688         SUBTRACT 1 FROM PI-CURRENT-LINE                           EL1284
00689      END-IF.                                                      EL1284
00690                                                                   EL1284
00691      SET TB-INDX  TO PI-CURRENT-LINE                              EL1284
00692      MOVE LOW-VALUES             TO EL1284AI                      EL1284
                                          EL1284R
00693                                                                   EL1284
           PERFORM 7800-MOVE-SAVE-TO-MAP.                               EL1284
00694      IF PI-CURRENT-LINE > ZERO                                    EL1284
00695          PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                EL1284
00696              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1284
00697              SC-INDX > NUM-LINES-PER-SCREEN                       EL1284
00698          PERFORM 7200-PUT-TEMP-STOR  THRU 7249-EXIT               EL1284
00699      END-IF.                                                      EL1284
00700                                                                   EL1284
00701      MOVE '1'                    TO PI-UPDATE-SW.                 EL1284
00702      IF PI-TOTAL-LINES = ZEROS                                    EL1284
00703         MOVE ZEROS               TO PI-CURRENT-LINE               EL1284
00704      END-IF.                                                      EL1284

040416     MOVE -1 TO FUNCTL                                            EL1284
040416     SET CURSOR-SET TO TRUE                                       EL1284

00706      GO TO 8100-SEND-INITIAL-MAP.                                 EL1284
00707      EJECT                                                        EL1284
00708                                                                   EL1284
00709  3100-DELETE-TABLE-ENTRIES.                                       EL1284
00710      MOVE REC-ENT (TB-INDX1)     TO REC-ENT (TB-INDX)             EL1284
00711      SET TB-INDX TB-INDX1 UP BY 1.                                EL1284
00712                                                                   EL1284
00713  3150-BLANK-TABLE-ENTRIES.                                        EL1284
00714      MOVE SPACES               TO REC-ENT (TB-INDX).              EL1284
00715      MOVE SAVE-BIN-DATE        TO REC-LAST-MAINT-DT (TB-INDX).    EL1284
00716      MOVE EIBTIME              TO REC-LAST-MAINT-HHMMSS (TB-INDX).EL1284
00717      MOVE PI-PROCESSOR-ID      TO REC-LAST-MAINT-BY (TB-INDX).    EL1284
00718      SET TB-INDX UP BY 1.                                         EL1284
00719      EJECT                                                        EL1284
00720  3500-INSERT-LINES.                                               EL1284
00721                                                                   EL1284
00722      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1284
00723      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1284
00724              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1284
00725              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1284
00726                                                                   EL1284
00727      IF NOT EMI-NO-ERRORS                                         EL1284
00728         GO TO 8200-SEND-DATAONLY                                  EL1284
00729      END-IF.                                                      EL1284
00730                                                                   EL1284
00731      SET TB-INDX TO PI-TOTAL-LINES.                               EL1284
00732      ADD LINE2I TO PI-TOTAL-LINES.                                EL1284
00733      SET TB-INDX1 TO PI-TOTAL-LINES.                              EL1284
00734      PERFORM 3600-INSERT-TABLE-ENTRIES                            EL1284
00735              UNTIL TB-INDX = LINE1I.                              EL1284
00736      SET TB-INDX UP BY 1.                                         EL1284
00737                                                                   EL1284
00738      IF LINE1I EQUAL ZERO                                         EL1284
00739          SET PI-CURRENT-LINE TO 1                                 EL1284
00740      ELSE                                                         EL1284
00741          SET PI-CURRENT-LINE TO LINE1I                            EL1284
00742      END-IF.                                                      EL1284
00743                                                                   EL1284
00744      COMPUTE ROLL-COUNTER = PI-CURRENT-LINE +                     EL1284
00745                             NUM-LINES-PER-SCREEN.                 EL1284
00746      IF TB-INDX NOT LESS THAN ROLL-COUNTER OR                     EL1284
00747                     LESS THAN PI-CURRENT-LINE                     EL1284
00748         SET SC-INDX TO 1                                          EL1284
00749         SET SC-INDX DOWN BY 1                                     EL1284
00750      ELSE                                                         EL1284
00751         SET ROLL-COUNTER TO TB-INDX                               EL1284
00752         COMPUTE ROLL-COUNTER = ROLL-COUNTER - PI-CURRENT-LINE     EL1284
00753                   + 1                                             EL1284
00754         SET SC-INDX TO ROLL-COUNTER                               EL1284
00755      END-IF.                                                      EL1284
00756                                                                   EL1284
00757      PERFORM 3150-BLANK-TABLE-ENTRIES LINE2I TIMES.               EL1284
00758      SET TB-INDX TO PI-CURRENT-LINE.                              EL1284
00759      MOVE LOW-VALUES             TO EL1284AI                      EL1284
                                          EL1284R
00760                                                                   EL1284
00761      IF SC-INDX NOT = ZERO                                        EL1284
00762         MOVE -1 TO SC-TEXTL (SC-INDX)                             EL1284
              SET CURSOR-SET TO TRUE                                    EL1284
00763      END-IF.                                                      EL1284
00764                                                                   EL1284
           PERFORM 7800-MOVE-SAVE-TO-MAP.                               EL1284
                                                                        EL1284
00765      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1284
00766             VARYING SC-INDX FROM 1 BY 1 UNTIL                     EL1284
00767             SC-INDX > NUM-LINES-PER-SCREEN.                       EL1284
00768      PERFORM 7200-PUT-TEMP-STOR  THRU 7249-EXIT.                  EL1284
00769      MOVE '1'                    TO PI-UPDATE-SW.                 EL1284

040416     MOVE -1 TO FUNCTL                                            EL1284
040416     SET CURSOR-SET TO TRUE                                       EL1284

00770      GO TO 8100-SEND-INITIAL-MAP.                                 EL1284
00771                                                                   EL1284
00772  3600-INSERT-TABLE-ENTRIES.                                       EL1284
00773      MOVE REC-ENT (TB-INDX)      TO REC-ENT (TB-INDX1).           EL1284
00774      SET TB-INDX TB-INDX1 DOWN BY 1.                              EL1284
00775      EJECT                                                        EL1284
00776                                                                   EL1284
00777  4000-CHANGE-ROUTINE.                                             EL1284
00778      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1284
00779      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1284
00780              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1284
00781              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1284
00782                                                                   EL1284
00783      IF NOT EMI-NO-ERRORS                                         EL1284
00784         GO TO 8200-SEND-DATAONLY                                  EL1284
00785      END-IF.                                                      EL1284
00786                                                                   EL1284
00787      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                   EL1284
00788      MOVE SPACES                 TO ERRMSGBO.                     EL1284
00789      GO TO 8200-SEND-DATAONLY.                                    EL1284
00790                                                                   EL1284
00791      EJECT                                                        EL1284
00792  4500-SAVE-DATA.                                                  EL1284
00793      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1284
00794      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1284
00795              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1284
00796              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1284
00797      IF NOT EMI-NO-ERRORS                                         EL1284
00798         GO TO 8200-SEND-DATAONLY                                  EL1284
00799      END-IF.                                                      EL1284
00800                                                                   EL1284
00801      EXEC CICS HANDLE CONDITION                                   EL1284
00802           NOTFND(4610-ENDBR)                                      EL1284
00803           NOTOPEN(6000-NOT-OPEN)                                  EL1284
00804           ENDFILE(4610-ENDBR)                                     EL1284
00805      END-EXEC.                                                    EL1284
00806                                                                   EL1284
00807  4610-LOOP.                                                       EL1284
00808      EXEC CICS READ                                               EL1284
00809          DATASET (ELMEMO-FILE-ID)                                 EL1284
00810          RIDFLD  (ELMEMO-KEY)                                     EL1284
00811          SET     (ADDRESS OF CLAIM-MEMO-FILE)                     EL1284
00812          GTEQ                                                     EL1284
00813      END-EXEC.                                                    EL1284
00814                                                                   EL1284
00815      MOVE MM-CONTROL-PRIMARY     TO ELMEMO-KEY.                   EL1284
00816                                                                   EL1284
00817      IF ELMEMO-PARTIAL-KEY NOT = SV-PRIOR-KEY                     EL1284
00818          MOVE SV-PRIOR-KEY       TO ELMEMO-PARTIAL-KEY            EL1284
00819          GO TO 4610-ENDBR                                         EL1284
00820      END-IF.                                                      EL1284
00821                                                                   EL1284
00822      EXEC CICS DELETE                                             EL1284
00823          DATASET (ELMEMO-FILE-ID)                                 EL1284
00824          RIDFLD  (ELMEMO-KEY)                                     EL1284
00825      END-EXEC.                                                    EL1284
00826                                                                   EL1284
00827      GO TO 4610-LOOP.                                             EL1284
00828  4610-ENDBR.                                                      EL1284
00829      EXEC CICS GETMAIN                                            EL1284
00830           LENGTH(ELMEMO-LENGTH)                                   EL1284
00831           SET(ADDRESS OF CLAIM-MEMO-FILE)                         EL1284
00832           INITIMG(GETMAIN-SPACE)                                  EL1284
00833      END-EXEC.                                                    EL1284
                                                                        EL1284
           PERFORM 4650-WRITE-HEADER THRU 4650-EXIT.                    EL1284
00834                                                                   EL1284
00835      MOVE 1                      TO  ELMEMO-SEQ.                  EL1284
00836      MOVE ZERO                   TO  WS-BLANK-LINES.              EL1284
00837                                                                   EL1284
00838      PERFORM 4700-WRITE-FILE THRU 4799-EXIT                       EL1284
00839              VARYING TB-INDX FROM 1 BY 1 UNTIL                    EL1284
00840              TB-INDX > PI-TOTAL-LINES.                            EL1284
00841                                                                   EL1284
00842      SUBTRACT WS-BLANK-LINES FROM PI-TOTAL-LINES.                 EL1284
00843                                                                   EL1284
00846      GO TO 9410-RETURN.                                           EL1284
                                                                        EL1284
00848  4650-WRITE-HEADER.                                               EL1284
00853                                                                   EL1284
00854      MOVE SPACES                 TO  CLAIM-MEMO-FILE.             EL1284
00855      MOVE ZERO                   TO  ELMEMO-SEQ.                  EL1284
00856      MOVE ELMEMO-KEY             TO  MM-CONTROL-PRIMARY.          EL1284
00857      MOVE  'MM'                  TO  MM-RECORD-ID.                EL1284
00858      MOVE REC-TEXT (TB-INDX)     TO  MM-CLAIM-MEMO                EL1284
00036      MOVE LNGHLAPI               TO MM-LONG-HEALTH-APP
00037      MOVE REWRITEI               TO MM-REWRITE-IND
00037      MOVE CHKCOVGI               TO MM-CHECKED-OTHER-COVG
01335      IF MRRELDTI > SPACES
01335         MOVE MRRELDTI               TO DEEDIT-DATE-INPUT
01336         EXEC CICS BIF DEEDIT
01337             FIELD    (DEEDIT-DATE-INPUT)
01338             LENGTH   (DATE-LENGTH)
01339         END-EXEC
01340         MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
01341         MOVE '4'            TO DC-OPTION-CODE
01342         PERFORM PERFORM 9700-LINK-DATE-CONVERT
01343         IF NOT DATE-CONVERSION-ERROR
01344             MOVE DC-BIN-DATE-1      TO MM-MR-RELEASED-FROM-DATE
01345 *           MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
01346 *           MOVE AL-UANON           TO INCA
01347 *           MOVE DC-GREG-DATE-1-EDIT TO INCO
01348         ELSE
                  MOVE -1         TO MRRELDTL
01349             MOVE ER-0314    TO EMI-ERROR
01350             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00635             GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF.

01351 *        MOVE AL-UABON   TO INCA
01352 *        MOVE -1         TO INCL
01353 *        MOVE 'X'        TO ERROR-SWITCH.


00866      EXEC CICS WRITE                                              EL1284
00867           DATASET(ELMEMO-FILE-ID)                                 EL1284
00868           FROM(CLAIM-MEMO-FILE)                                   EL1284
00869           RIDFLD(ELMEMO-KEY)                                      EL1284
00870      END-EXEC.                                                    EL1284

00871  4650-EXIT.                                                       EL1284
00872       EXIT.                                                       EL1284

00847                                                                   EL1284
00848  4700-WRITE-FILE.                                                 EL1284
040416*    IF REC-TEXT (TB-INDX)  EQUAL SPACES                          EL1284
040416*        ADD +1                  TO  WS-BLANK-LINES               EL1284
040416*        GO TO 4799-EXIT                                          EL1284
040416*    END-IF.                                                      EL1284
00853                                                                   EL1284
00854      MOVE SPACES                 TO  CLAIM-MEMO-FILE.             EL1284
00855      ADD 1                       TO  ELMEMO-SEQ.                  EL1284
00856      MOVE ELMEMO-KEY             TO  MM-CONTROL-PRIMARY.          EL1284
00857      MOVE  'MM'                  TO  MM-RECORD-ID.                EL1284
00858      MOVE REC-TEXT (TB-INDX)     TO  MM-CLAIM-MEMO                EL1284
00859      MOVE REC-LAST-MAINT-BY (TB-INDX)                             EL1284
00860                                  TO  MM-LAST-MAINT-BY             EL1284
00861      MOVE REC-LAST-MAINT-HHMMSS (TB-INDX)                         EL1284
00862                                  TO  MM-LAST-MAINT-HHMMSS.        EL1284
00863      MOVE REC-LAST-MAINT-DT (TB-INDX)                             EL1284
00864                                  TO  MM-LAST-MAINT-DT.            EL1284
00865                                                                   EL1284
00866      EXEC CICS WRITE                                              EL1284
00867           DATASET(ELMEMO-FILE-ID)                                 EL1284
00868           FROM(CLAIM-MEMO-FILE)                                   EL1284
00869           RIDFLD(ELMEMO-KEY)                                      EL1284
00870      END-EXEC.                                                    EL1284
00871  4799-EXIT.                                                       EL1284
00872       EXIT.                                                       EL1284
00873                                                                   EL1284
00874      EJECT                                                        EL1284
00971                                                                   EL1284
00972  4900-SET-NOTES-FLAG.                                             EL1284
00973                                                                   EL1284
00974      EXEC CICS HANDLE CONDITION                                   EL1284
00975          NOTFND   (4900-EXIT)                                     EL1284
00976      END-EXEC.                                                    EL1284
00977                                                                   EL1284
00978      MOVE SPACES                 TO  ELCERT-KEY.                  EL1284
00979      MOVE PI-COMPANY-CD          TO  ELCERT-COMPANY-CD.           EL1284
00980      MOVE PI-CARRIER             TO  ELCERT-CARRIER.              EL1284
00981      MOVE PI-GROUPING            TO  ELCERT-GROUPING.             EL1284
00982      MOVE PI-STATE               TO  ELCERT-STATE.                EL1284
00983      MOVE PI-ACCOUNT             TO  ELCERT-ACCOUNT.              EL1284
00984      MOVE PI-CERT-EFF-DT         TO  ELCERT-EFF-DT.               EL1284
00985      MOVE PI-CERT-PRIME          TO  ELCERT-CERT-PRIME.           EL1284
00986      MOVE PI-CERT-SFX            TO  ELCERT-CERT-SFX.             EL1284
00987                                                                   EL1284
00988      EXEC CICS READ                                               EL1284
00989      EQUAL                                                        EL1284
00990      DATASET   (ELCERT-FILE-ID)                                   EL1284
00991      SET       (ADDRESS OF CERTIFICATE-MASTER)                    EL1284
00992      RIDFLD    (ELCERT-KEY)                                       EL1284
00993      END-EXEC.                                                    EL1284
00994                                                                   EL1284
00995 *    MOVE 'N'                    TO PI-BILLING-NOTES-EXIST        EL1284
00996 *                                   PI-CERT-NOTES-EXIST           EL1284
00997 *                                   PI-CLAIM-NOTES-EXIST.         EL1284
00998 *    IF CM-NOTE-SW EQUAL '2' OR '3' OR '6' OR '7'                 EL1284
00999 *         MOVE 'Y'               TO PI-BILLING-NOTES-EXIST        EL1284
01000 *    END-IF.                                                      EL1284
01001 *    IF CM-NOTE-SW EQUAL '1' OR '3' OR '5' OR '7'                 EL1284
01002 *         MOVE 'Y'               TO PI-CERT-NOTES-EXIST           EL1284
01003 *    END-IF.                                                      EL1284
01004 *    IF CM-NOTE-SW EQUAL '4' OR '5' OR '6' OR '7'                 EL1284
01005 *         MOVE 'Y'               TO PI-CLAIM-NOTES-EXIST          EL1284
01006 *    END-IF.                                                      EL1284
01007                                                                   EL1284
01008                                                                   EL1284
01009  4900-EXIT.                                                       EL1284
01010       EXIT.                                                       EL1284
01011                                                                   EL1284
01012      EJECT                                                        EL1284
01013  5000-ADD-NEW-LINES.                                              EL1284
01014      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1284
01015      MOVE 'N'                    TO WS-SCREEN-LINE.               EL1284
01016      MOVE 1                      TO WS-SUB.                       EL1284
01017      MOVE ZERO                   TO WS-SUB1.                      EL1284
01018      PERFORM VARYING SC-INDX FROM NUM-LINES-PER-SCREEN BY -1      EL1284
01019              UNTIL SCREEN-LINE-FOUND OR SC-INDX < 1               EL1284
01020          IF SC-TEXT (SC-INDX) >  SPACES                           EL1284
01021              SET WS-SUB          TO SC-INDX                       EL1284
01022              MOVE 'Y'            TO WS-SCREEN-LINE                EL1284
01023          END-IF                                                   EL1284
01024      END-PERFORM.                                                 EL1284
01025      IF PI-TOTAL-LINES = 0                                        EL1284
01026          MOVE 1                  TO WS-SUB1                       EL1284
01027      ELSE                                                         EL1284
01028          MOVE 2                  TO WS-SUB1                       EL1284
01029      END-IF.                                                      EL1284
01030      PERFORM VARYING SC-INDX FROM WS-SUB1 BY 1                    EL1284
01031              UNTIL SC-INDX GREATER THAN WS-SUB                    EL1284
01032          IF SC-TEXTL (SC-INDX) EQUAL ZEROS                        EL1284
01033              MOVE 1      TO SC-TEXTL (SC-INDX)                    EL1284
01034              MOVE SPACES TO SC-TEXT (SC-INDX)                     EL1284
                   MOVE LOW-VALUES TO SC-MTBY (SC-INDX)                 EL1284
                                      SC-MTDT (SC-INDX)                 EL1284
01035          END-IF                                                   EL1284
01036      END-PERFORM.                                                 EL1284
01037      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1284
01038              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1284
01039              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1284
01040                                                                   EL1284
01041      IF NOT EMI-NO-ERRORS                                         EL1284
01042         GO TO 8200-SEND-DATAONLY                                  EL1284
01043      END-IF.                                                      EL1284
01044                                                                   EL1284
01045      MOVE PI-TOTAL-LINES         TO  PI-CURRENT-LINE.             EL1284
01046      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                   EL1284
01047      MOVE LOW-VALUES             TO  EL1284AI                     EL1284
                                           EL1284R
           PERFORM 7800-MOVE-SAVE-TO-MAP.                               EL1284
                                                                        EL1284
01048      SET TB-INDX TO PI-CURRENT-LINE.                              EL1284
01049      MOVE 'A'                    TO FUNCTI.                       EL1284
01050      MOVE -1                     TO  SC-TEXTL (2).                EL1284
           SET CURSOR-SET TO TRUE                                       EL1284
01051      MOVE AL-UANON               TO  FUNCTA.                      EL1284
01052      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1284
01053              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1284
01054              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1284
01055      MOVE '1'                    TO PI-UPDATE-SW.                 EL1284

01056      GO TO 8100-SEND-INITIAL-MAP.                                 EL1284
01057      EJECT                                                        EL1284
01058  5500-LOOKUP.                                                     EL1284
01059      PERFORM 7500-READ-TS THRU 7599-EXIT.                         EL1284
01060      SET TB-INDX TO PI-CURRENT-LINE.                              EL1284
01061      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1284
01062              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1284
01063              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1284
01064                                                                   EL1284
01065      IF NOT EMI-NO-ERRORS                                         EL1284
01066         GO TO 8200-SEND-DATAONLY                                  EL1284
01067      END-IF.                                                      EL1284
01068                                                                   EL1284
01069      MOVE LINE1I                 TO  PI-CURRENT-LINE.             EL1284
01070      SET TB-INDX                 TO PI-CURRENT-LINE.              EL1284
01071      MOVE LOW-VALUES             TO  EL1284AI                     EL1284
                                           EL1284R
           PERFORM 7800-MOVE-SAVE-TO-MAP.                               EL1284
                                                                        EL1284
01072      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1284
01073              VARYING SC-INDX FROM 1 BY 1                          EL1284
01074              UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                EL1284
01075      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                   EL1284

040416     MOVE -1 TO FUNCTL                                            EL1284
040416     SET CURSOR-SET TO TRUE                                       EL1284

01076      GO TO 8100-SEND-INITIAL-MAP.                                 EL1284
01077      EJECT                                                        EL1284
01078  6000-NOT-OPEN.                                                   EL1284
01079      MOVE ER-2954                TO  EMI-ERROR.                   EL1284
01080      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1284
01081                                                                   EL1284
01082      IF EIBAID = DFHCLEAR                                         EL1284
01083          GO TO 9410-RETURN                                        EL1284
01084      ELSE                                                         EL1284
01085          GO TO 8100-SEND-INITIAL-MAP                              EL1284
01086      END-IF.                                                      EL1284
01087      EJECT                                                        EL1284
01088  7000-BUILD-TABLE.                                                EL1284
01089                                                                   EL1284
01090      SET TB-INDX TO 1.                                            EL1284
01091      MOVE ZEROS                  TO  PI-TOTAL-LINES               EL1284
01092                                      PI-CURRENT-LINE              EL1284
01093                                      PI-TEMP-STOR-ITEMS           EL1284
01094                                      PI-UPDATE-SW.                EL1284
01095      MOVE LOW-VALUES             TO  EL1284AI                     EL1284
                                           EL1284R
01099                                                                   EL1284
01100 ****IF TEMP STORAGE EXISTS, DELETE IT.                            EL1284
01101      IF PI-CHANGE-IN-NOTE-TYPE                                    EL1284
01102          MOVE 'N' TO PI-SET-NOTE-CHANGE                           EL1284
01103      ELSE                                                         EL1284
01104          PERFORM 7500-READ-TS THRU 7599-EXIT                      EL1284
01105      END-IF.                                                      EL1284
01106                                                                   EL1284
01107      IF PI-TEMP-STOR-ITEMS NOT = ZERO                             EL1284
01108         PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT              EL1284
01109      END-IF.                                                      EL1284
01110                                                                   EL1284
01111      EXEC CICS HANDLE CONDITION                                   EL1284
01112           NOTFND(7010-ENDBR)                                      EL1284
01113           NOTOPEN(6000-NOT-OPEN)                                  EL1284
01114           ENDFILE(7010-ENDBR)                                     EL1284
01115      END-EXEC.                                                    EL1284
01116                                                                   EL1284
01117      EXEC CICS STARTBR                                            EL1284
01118           DATASET(ELMEMO-FILE-ID)                                 EL1284
01119           RIDFLD(ELMEMO-KEY)                                      EL1284
01120           KEYLENGTH(ELMEMO-START-LENGTH)                          EL1284
01121           GENERIC                                                 EL1284
01122           GTEQ                                                    EL1284
01123      END-EXEC.                                                    EL1284
01124                                                                   EL1284
01125  7001-LOOP.                                                       EL1284
01126      EXEC CICS READNEXT                                           EL1284
01127           SET(ADDRESS OF CLAIM-MEMO-FILE)                         EL1284
01128           DATASET(ELMEMO-FILE-ID)                                 EL1284
01129           RIDFLD(ELMEMO-KEY)                                      EL1284
01130      END-EXEC.                                                    EL1284
01131                                                                   EL1284
01132      IF MM-COMPANY-CD NOT = SV-COMPANY-CD                         EL1284
01133          GO TO 7010-ENDBR                                         EL1284
01134      END-IF.                                                      EL1284
01135                                                                   EL1284
01136      IF (MM-CARRIER = SV-CARRIER)                                 EL1284
01141         AND (MM-CERT-NO = SV-CERT-NO)                             EL1284
01141         AND (MM-CLAIM-NO = SV-CLAIM-NO)                           EL1284
01142         AND (MM-RECORD-TYPE = SV-RECORD-TYPE)                     EL1284
              IF MM-PAYMENT-SEQ-NO = ZERO                               EL1284
                 MOVE MM-LONG-HEALTH-APP TO LNGHLAPO                    EL1284
                                            PI-LONG-HLTH-APP            EL1284
                 MOVE MM-REWRITE-IND TO REWRITEO                        EL1284
                                        PI-REWRITE                      EL1284
                 MOVE MM-CHECKED-OTHER-COVG TO CHKCOVGO                 EL1284
                                               PI-CHKCOVG               EL1284
                 MOVE MM-MR-RELEASED-FROM-DATE TO DC-BIN-DATE-1         EL1284
                 MOVE ' '                    TO DC-OPTION-CODE          EL1284
01433            PERFORM 9700-LINK-DATE-CONVERT                         EL1284
01434            IF DC-EDIT1-MONTH > ZERO                               EL1284
                    MOVE DC-GREG-DATE-1-EDIT    TO MRRELDTO             EL1284
                                                   PI-MR-RELEASE-DATE   EL1284
                 END-IF                                                 EL1284
              ELSE                                                      EL1284
01143            MOVE MM-CLAIM-MEMO TO REC-TEXT (TB-INDX)               EL1284
01144            MOVE MM-LAST-MAINT-BY TO REC-LAST-MAINT-BY (TB-INDX)   EL1284
01145            MOVE MM-LAST-MAINT-DT TO REC-LAST-MAINT-DT (TB-INDX)   EL1284
01146            MOVE MM-LAST-MAINT-HHMMSS TO                           EL1284
01147                           REC-LAST-MAINT-HHMMSS (TB-INDX)         EL1284
01155            SET TB-INDX UP BY 1                                    EL1284
              END-IF                                                    EL1284
01156         GO TO 7001-LOOP                                           EL1284
01157      END-IF.                                                      EL1284
                                                                        EL1284
01158  7010-ENDBR.                                                      EL1284
01159      IF TB-INDX = 1                                               EL1284
01160          MOVE ER-0006            TO EMI-ERROR                     EL1284
01161          MOVE 'A'                TO FUNCTI                        EL1284
01162          MOVE -1                 TO SC-TEXTL (1)                  EL1284
01163          MOVE AL-UANON           TO FUNCTA                        EL1284
01164          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1284
01165          MOVE ZEROS              TO PI-TOTAL-LINES                EL1284
01166          GO TO 8100-SEND-INITIAL-MAP                              EL1284
01167      END-IF.                                                      EL1284
01168                                                                   EL1284
01169      EXEC CICS ENDBR                                              EL1284
01170           DATASET(ELMEMO-FILE-ID)                                 EL1284
01171      END-EXEC.                                                    EL1284
01172                                                                   EL1284
01173      SET TB-INDX DOWN BY 1.                                       EL1284
01174      SET PI-TOTAL-LINES TO TB-INDX.                               EL1284
01175      MOVE 1                      TO PI-CURRENT-LINE.              EL1284
01176                                                                   EL1284
01177  7050-FORMAT-LINES.                                               EL1284
01178      SET TB-INDX TO PI-CURRENT-LINE.                              EL1284
01179      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1284
01180              VARYING SC-INDX FROM 1                               EL1284
01181              BY 1 UNTIL SC-INDX > NUM-LINES-PER-SCREEN.           EL1284
01182      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                   EL1284
01183      GO TO 8100-SEND-INITIAL-MAP.                                 EL1284
01184      EJECT                                                        EL1284
01185  7100-FORMAT-SCREEN.                                              EL1284
01186      IF TB-INDX > PI-TOTAL-LINES                                  EL1284
01187         IF FUNCTI NOT = 'A'                                       EL1284
01188            MOVE AL-PANON         TO SC-TEXTA (SC-INDX)            EL1284
01189         END-IF                                                    EL1284
01190      END-IF.                                                      EL1284
01191                                                                   EL1284
01192      IF TB-INDX > PI-TOTAL-LINES                                  EL1284
01193          GO TO 7100-EXIT                                          EL1284
01194      END-IF.                                                      EL1284
01195                                                                   EL1284
01196      SET WS-LINE TO TB-INDX.                                      EL1284
01201      MOVE WS-LINE TO SC-LIN (SC-INDX).                            EL1284
01202      MOVE REC-TEXT (TB-INDX)     TO SC-TEXT (SC-INDX).            EL1284
           MOVE REC-LAST-MAINT-BY (TB-INDX) TO SC-MTBY (SC-INDX).       EL1284
           MOVE REC-LAST-MAINT-DT (TB-INDX) TO DC-BIN-DATE-1.           EL1284
           MOVE ' '                    TO DC-OPTION-CODE.               EL1284
01433      PERFORM 9700-LINK-DATE-CONVERT.                              EL1284
01434      MOVE DC-GREG-DATE-1-MDY     TO SC-MTDT (SC-INDX).            EL1284
01203      SET ROLL-COUNTER TO TB-INDX.                                 EL1284
01204                                                                   EL1284
01205      IF NOT MODIFY-CAP                                            EL1284
01206          MOVE AL-PANOF           TO SC-TEXTA (SC-INDX)            EL1284
01207          SET TB-INDX UP BY 1                                      EL1284
01208          GO TO 7100-EXIT                                          EL1284
01209      END-IF.                                                      EL1284
01210                                                                   EL1284
01211      SET TB-INDX UP BY 1.                                         EL1284
01212                                                                   EL1284
01213  7100-EXIT.                                                       EL1284
01214       EXIT.                                                       EL1284
01215                                                                   EL1284
01235  7200-PUT-TEMP-STOR.                                              EL1284
01236      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.                EL1284
01237      SET TS-INDX TO 1.                                            EL1284
01238      MOVE 0                      TO PI-TEMP-STOR-ITEMS.           EL1284
01239      PERFORM 7300-WRITE-TS THRU 7399-EXIT                         EL1284
01240              VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP  EL1284
01241              UNTIL TS-GROUP-WORK NOT LESS THAN PI-TOTAL-LINES.    EL1284
01242  7249-EXIT.                                                       EL1284
01243       EXIT.                                                       EL1284
01244  7250-DELETE-TEMP-STOR.                                           EL1284
01245      EXEC CICS HANDLE CONDITION                                   EL1284
01246           QIDERR(7299-EXIT)                                       EL1284
01247      END-EXEC.                                                    EL1284
01248      EXEC CICS DELETEQ TS                                         EL1284
01249           QUEUE(QID)                                              EL1284
01250      END-EXEC.                                                    EL1284
01251  7299-EXIT.                                                       EL1284
01252      EXIT.                                                        EL1284
01253      EJECT                                                        EL1284
01254  7300-WRITE-TS.                                                   EL1284
01255      MOVE TS-GROUP (TS-INDX)     TO TS-WORK-AREA.                 EL1284
01256      SET TS-INDX UP BY 1.                                         EL1284
01257      ADD 1 TO PI-TEMP-STOR-ITEMS.                                 EL1284
01258      EXEC CICS WRITEQ TS                                          EL1284
01259           FROM(TS-WORK-AREA)                                      EL1284
01260           QUEUE(QID)                                              EL1284
01261           LENGTH(TS-LENGTH)                                       EL1284
01262           ITEM(PI-TEMP-STOR-ITEMS)                                EL1284
01263      END-EXEC.                                                    EL1284
01264  7399-EXIT.                                                       EL1284
01265      EXIT.                                                        EL1284
01266      EJECT                                                        EL1284
01267  7400-PAGE-ROUTINE.                                               EL1284
01268                                                                   EL1284
01269      IF PFENTERL NOT = ZEROS                                      EL1284
01270         MOVE -1                  TO PFENTERL                      EL1284
01271         ELSE                                                      EL1284
01272         MOVE -1                  TO FUNCTL                        EL1284
01273      END-IF.                                                      EL1284
01274                                                                   EL1284
01275      IF PI-TOTAL-LINES = 0                                        EL1284
01276         MOVE ER-0047             TO EMI-ERROR                     EL1284
01277         MOVE -1                  TO FUNCTL                        EL1284
01278         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1284
01279         GO TO 8200-SEND-DATAONLY                                  EL1284
01280      END-IF.                                                      EL1284
01281                                                                   EL1284
01282      COMPUTE TEMP-CURR-LINE = PI-CURRENT-LINE + ROLL-COUNTER.     EL1284
01283                                                                   EL1284
01284      IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS         EL1284
01285         MOVE ER-0067             TO EMI-ERROR                     EL1284
01286         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1284
01287         MOVE 1 TO TEMP-CURR-LINE                                  EL1284
01288      END-IF.                                                      EL1284
01289                                                                   EL1284
01290      IF TEMP-CURR-LINE GREATER THAN PI-TOTAL-LINES                EL1284
01291         MOVE ER-0066             TO EMI-ERROR                     EL1284
01292         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1284
01293         COMPUTE TEMP-CURR-LINE = PI-TOTAL-LINES + 1               EL1284
01294                                - NUM-LINES-PER-SCREEN             EL1284
01295         IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS      EL1284
01296            MOVE 1                TO TEMP-CURR-LINE                EL1284
01297         END-IF                                                    EL1284
01298      END-IF.                                                      EL1284
01299                                                                   EL1284
01300      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL1284
01301      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL1284
01302              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL1284
01303              SC-INDX > NUM-LINES-PER-SCREEN.                      EL1284
01304                                                                   EL1284
01305      IF EMI-ERROR = ER-0066 OR = ER-0067 OR = ZEROS               EL1284
01306         NEXT SENTENCE                                             EL1284
01307      ELSE                                                         EL1284
01308         GO TO 8200-SEND-DATAONLY                                  EL1284
01309      END-IF.                                                      EL1284
01310                                                                   EL1284
01311      MOVE TEMP-CURR-LINE         TO PI-CURRENT-LINE.              EL1284
01312      SET TB-INDX TO PI-CURRENT-LINE.                              EL1284
01313      MOVE LOW-VALUES             TO EL1284AI                      EL1284
                                          EL1284R
           PERFORM 7800-MOVE-SAVE-TO-MAP.                               EL1284
                                                                        EL1284
01314      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL1284
01315              VARYING SC-INDX FROM 1 BY 1                          EL1284
01316              UNTIL SC-INDX > NUM-LINES-PER-SCREEN.                EL1284
01317      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                   EL1284
01318      GO TO 8100-SEND-INITIAL-MAP.                                 EL1284
01319      EJECT                                                        EL1284
01320                                                                   EL1284
01321  7450-SET-INDX.                                                   EL1284
01322      IF PI-CURRENT-LINE = 0 AND PI-TOTAL-LINES = 0                EL1284
01323         SET TB-INDX TO 1                                          EL1284
01324      ELSE                                                         EL1284
01325         PERFORM 7500-READ-TS THRU 7599-EXIT                       EL1284
01326         IF PI-CURRENT-LINE = 0                                    EL1284
01327            SET TB-INDX TO 1                                       EL1284
01328         ELSE                                                      EL1284
01329            SET TB-INDX TO PI-CURRENT-LINE                         EL1284
01330         END-IF                                                    EL1284
01331      END-IF.                                                      EL1284
01332  7450-EXIT.                                                       EL1284
01333       EXIT.                                                       EL1284
01334      EJECT                                                        EL1284
01335  7500-READ-TS.                                                    EL1284
01336      EXEC CICS HANDLE CONDITION                                   EL1284
01337           QIDERR(7590-TS-QIDERR)                                  EL1284
01338           ITEMERR(7585-QID-ITEMERR)                               EL1284
01339      END-EXEC.                                                    EL1284
01340      SET TS-INDX TO 1.                                            EL1284
01341      MOVE 1                      TO QID-ITEM.                     EL1284
01342  7501-LOOP.                                                       EL1284
01343      EXEC CICS READQ TS                                           EL1284
01344           INTO(TS-WORK-AREA)                                      EL1284
01345           QUEUE(QID)                                              EL1284
01346           LENGTH(TS-LENGTH)                                       EL1284
01347           ITEM(QID-ITEM)                                          EL1284
01348      END-EXEC.                                                    EL1284
01349      MOVE TS-WORK-AREA           TO TS-GROUP (TS-INDX).           EL1284
01350      SET TS-INDX UP BY 1.                                         EL1284
01351      ADD 1 TO QID-ITEM.                                           EL1284
01352      GO TO 7501-LOOP.                                             EL1284
01353                                                                   EL1284
01354  7585-QID-ITEMERR.                                                EL1284
01355      IF EIBTRNID NOT = TRANS-ID                                   EL1284
01356         SUBTRACT 1 FROM QID-ITEM                                  EL1284
01357         MOVE QID-ITEM            TO PI-TEMP-STOR-ITEMS            EL1284
01358      END-IF.                                                      EL1284
01359      GO TO 7599-EXIT.                                             EL1284
01360                                                                   EL1284
01361  7590-TS-QIDERR.                                                  EL1284
01362      IF EIBTRNID = TRANS-ID                                       EL1284
01363         AND EIBAID = DFHCLEAR                                     EL1284
01364            GO TO 9410-RETURN                                      EL1284
01365      END-IF.                                                      EL1284
01366      IF EIBTRNID = TRANS-ID                                       EL1284
01367         MOVE ER-0033             TO EMI-ERROR                     EL1284
01368         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1284
01369         GO TO 8100-SEND-INITIAL-MAP                               EL1284
01370      END-IF.                                                      EL1284
01371                                                                   EL1284
01372  7599-EXIT.                                                       EL1284
01373       EXIT.                                                       EL1284
01374                                                                   EL1284
01375      EJECT                                                        EL1284
01376  7600-UPDATE-TABLE-FROM-SCREEN.                                   EL1284
01377                                                                   EL1284
01378      IF SC-TEXTL (SC-INDX) NOT = ZEROS                            EL1284
01379          IF TB-INDX NOT > PI-TOTAL-LINES                          EL1284
01380              PERFORM 7700-MOVE-DATA THRU 7700-EXIT                EL1284
01381              SET TB-INDX UP BY 1                                  EL1284
01382          ELSE                                                     EL1284
01383              IF PI-TOTAL-LINES = MAX-LINES                        EL1284
01384                  MOVE ER-0051    TO EMI-ERROR                     EL1284
01385                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL1284
01386                  GO TO 8200-SEND-DATAONLY                         EL1284
01387              ELSE                                                 EL1284
01388                  PERFORM 7700-MOVE-DATA THRU 7700-EXIT            EL1284
01389                  SET TB-INDX UP BY 1                              EL1284
01390                  ADD 1 TO PI-TOTAL-LINES                          EL1284
01391              END-IF                                               EL1284
01392          END-IF                                                   EL1284
01393      ELSE                                                         EL1284
01394         IF TB-INDX NOT > PI-TOTAL-LINES                           EL1284
01395            SET TB-INDX UP BY 1                                    EL1284
01396         END-IF                                                    EL1284
01397      END-IF.                                                      EL1284
01398                                                                   EL1284
01399  7699-EXIT.                                                       EL1284
01400       EXIT.                                                       EL1284
01401                                                                   EL1284
01402  7700-MOVE-DATA.                                                  EL1284
01403      MOVE '1'                    TO PI-UPDATE-SW.                 EL1284
01404                                                                   EL1284
01405      IF SC-TEXTL (SC-INDX) NOT = ZEROS                            EL1284
01406         MOVE SC-TEXT (SC-INDX)  TO REC-TEXT (TB-INDX)             EL1284
01407         MOVE PI-PROCESSOR-ID    TO REC-LAST-MAINT-BY (TB-INDX)    EL1284
01409         MOVE EIBTIME            TO REC-LAST-MAINT-HHMMSS (TB-INDX)EL1284
01411         MOVE SAVE-BIN-DATE      TO REC-LAST-MAINT-DT (TB-INDX)    EL1284
01413      END-IF.                                                      EL1284
01414                                                                   EL1284
01415  7700-EXIT.                                                       EL1284
01416       EXIT.                                                       EL1284
       7800-MOVE-SAVE-TO-MAP.                                           EL1284
           MOVE PI-LONG-HLTH-APP TO LNGHLAPI                            EL1284
           MOVE PI-REWRITE TO REWRITEI                                  EL1284
           MOVE PI-CHKCOVG TO CHKCOVGI                                  EL1284
           MOVE PI-MR-RELEASE-DATE TO MRRELDTI.                         EL1284
                                                                        EL1284
01417      EJECT                                                        EL1284
01418  8100-SEND-INITIAL-MAP.                                           EL1284
01419      MOVE SAVE-DATE              TO DATEO.                        EL1284
01420      MOVE EIBTIME                TO TIME-IN.                      EL1284
01421      MOVE TIME-OUT               TO TIMEO.                        EL1284
01422      MOVE PI-COMPANY-ID          TO CMPNYIDO.                     EL1284
01423      MOVE PI-PROCESSOR-ID        TO USERIDO.                      EL1284
01424      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGBO.                     EL1284
01425      MOVE PI-CARRIER             TO CARRO                         EL1284
01426 *    MOVE PI-GROUPING            TO FGROUPO.                      EL1284
01427 *    MOVE PI-STATE               TO FSTO.                         EL1284
01428 *    MOVE PI-ACCOUNT             TO FACOUNTO.                     EL1284
01429      MOVE PI-CERT-NO             TO CERTNOO                       EL1284
01429      MOVE PI-CLAIM-NO            TO CLAIMNOO                      EL1284
01430 *    MOVE PI-CERT-SFX            TO FCRTSFXO.                     EL1284
      *                                                                 EL1284
           PERFORM 8150-READ-CLAIM.                                     EL1284
      *                                                                 EL1284
01431      MOVE  ' '                   TO DC-OPTION-CODE.               EL1284
01432      MOVE PI-CERT-EFF-DT         TO DC-BIN-DATE-1.                EL1284
01433      PERFORM 9700-LINK-DATE-CONVERT.                              EL1284
01434 *    MOVE DC-GREG-DATE-1-EDIT    TO FEFFDTO.                      EL1284
01435      MOVE PI-TOTAL-LINES         TO TOTO.                         EL1284
01443 *    IF PI-CLAIM-NOTE                                             EL1284
01444 *        MOVE SCR-CLAIM-NOTE-TYPE TO FTYPEO                       EL1284
01445 *        MOVE SCR-PF6-CERT        TO PF6NOTEO                     EL1284
01446 *        MOVE SCR-CERT-YN         TO FTYPEYNO                     EL1284
01447 *        IF PI-CERT-NOTES-EXIST = 'Y'                             EL1284
01448 *            MOVE 'YES'           TO FCERTYNO                     EL1284
01449 *        ELSE                                                     EL1284
01450 *            MOVE 'NO '           TO FCERTYNO                     EL1284
01451 *        END-IF                                                   EL1284
01452 *    ELSE                                                         EL1284
01453 *        MOVE SCR-CERT-NOTE-TYPE TO FTYPEO                        EL1284
01454 *        MOVE SCR-PF6-CLAIM      TO PF6NOTEO                      EL1284
01455 *        MOVE SCR-CLAIM-YN       TO FTYPEYNO                      EL1284
01456 *        IF PI-CLAIM-NOTES-EXIST = 'Y'                            EL1284
01457 *            MOVE 'YES'           TO FCERTYNO                     EL1284
01458 *        ELSE                                                     EL1284
01459 *            MOVE 'NO '           TO FCERTYNO                     EL1284
01460 *        END-IF                                                   EL1284
01461 *    END-IF.                                                      EL1284
01462 *    IF PI-BILLING-NOTES-EXIST = 'Y'                              EL1284
01463 *        MOVE 'YES'           TO FBILLYNO                         EL1284
01464 *    ELSE                                                         EL1284
01465 *        MOVE 'NO '           TO FBILLYNO                         EL1284
01466 *    END-IF                                                       EL1284
01467                                                                   EL1284
           MOVE EL1284R TO GROUP2.                                      EL1284
           IF NOT CURSOR-SET                                            EL1284
01468         MOVE -1                     TO LNGHLAPL                   EL1284
           END-IF                                                       EL1284
01469                                                                   EL1284
01470      EXEC CICS SEND                                               EL1284
01471           MAP(MAP-NAME)                                           EL1284
01472           MAPSET(MAPSET-NAME)                                     EL1284
01473           FROM(EL1284AO)                                          EL1284
01474           ERASE                                                   EL1284
01475           CURSOR                                                  EL1284
01476      END-EXEC.                                                    EL1284
01477                                                                   EL1284
01478      GO TO 9100-RETURN-TRAN.                                      EL1284
01479                                                                   EL1284
01480  8150-READ-CLAIM.                                                 EL1284
01012      EXEC CICS HANDLE CONDITION                                   EL1284
01013          NOTFND   (8150-NOT-FOUND)                                EL1284
01014      END-EXEC.                                                    EL1284
                                                                        EL1284
01016      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.                 EL1284
01017      MOVE PI-CARRIER             TO MSTR-CARRIER.                 EL1284
01018      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.                EL1284
01019      MOVE PI-CERT-NO             TO MSTR-CERT-NO.                 EL1284
01021                                                                   EL1284
01022      EXEC CICS READ                                               EL1284
01023          UPDATE                                                   EL1284
01024          DATASET   ('ELMSTR')                                     EL1284
01025          RIDFLD    (ELMSTR-KEY)                                   EL1284
01026          INTO      (CLAIM-MASTER)                                 EL1284
01027      END-EXEC.                                                    EL1284
                                                                        EL1284
121802     EVALUATE TRUE                                                EL1284
121802        WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1                    EL1284
121802           MOVE PI-AH-OVERRIDE-L6                                 EL1284
                                       TO TYPEO                         EL1284
121802        WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1                  EL1284
121802           MOVE PI-LIFE-OVERRIDE-L6                               EL1284
                                       TO TYPEO                         EL1284
121802        WHEN CL-CLAIM-TYPE = 'I'                                  EL1284
121802           MOVE '  IU  '         TO TYPEO                         EL1284
121802        WHEN CL-CLAIM-TYPE = 'G'                                  EL1284
121802           MOVE ' GAP  '         TO TYPEO                         EL1284
052614        WHEN CL-CLAIM-TYPE = 'F'                                  EL1284
052614           MOVE ' FAM  '         TO TYPEO                         EL1284
080322        WHEN CL-CLAIM-TYPE = 'B'
080322           MOVE ' BRV  '         TO TYPEO
080322        WHEN CL-CLAIM-TYPE = 'H'
080322           MOVE ' HSP '          TO TYPEO
100518        WHEN CL-CLAIM-TYPE = 'O'                                  EL1284
100518           MOVE ' OTH  '         TO TYPEO                         EL1284
121802     END-EVALUATE.                                                EL1284
                                                                        EL1284
           MOVE CL-INSURED-1ST-NAME    TO FSTNMEO.                      EL1284
           MOVE CL-INSURED-LAST-NAME   TO LASTNMEO.                     EL1284
                                                                        EL1284
       8150-NOT-FOUND.                                                  EL1284
           MOVE SPACES    TO TYPEO.                                     EL1284
      *                                                                 EL1284
01480  8200-SEND-DATAONLY.                                              EL1284
01481      MOVE EIBTIME                TO TIME-IN.                      EL1284
01482      MOVE TIME-OUT               TO TIMEO.                        EL1284
01483      MOVE PI-COMPANY-ID          TO CMPNYIDO.                     EL1284
01484      MOVE PI-PROCESSOR-ID        TO USERIDO.                      EL1284
01485      MOVE PI-TOTAL-LINES         TO TOTO.                         EL1284
           MOVE EL1284R TO GROUP2.                                      EL1284
                                                                        EL1284
01486                                                                   EL1284
01487      IF NOT EMI-NO-ERRORS                                         EL1284
01488         MOVE EMI-MESSAGE-AREA (1) TO ERRMSGBO                     EL1284
01489      ELSE                                                         EL1284
01490         MOVE -1                  TO FUNCTL                        EL1284
01491      END-IF.                                                      EL1284
01492                                                                   EL1284
01493      EXEC CICS SEND                                               EL1284
01494           MAP(MAP-NAME)                                           EL1284
01495           MAPSET(MAPSET-NAME)                                     EL1284
01496           FROM(EL1284AO)                                          EL1284
01497           DATAONLY                                                EL1284
01498           CURSOR                                                  EL1284
01499      END-EXEC.                                                    EL1284
01500                                                                   EL1284
01501      GO TO 9100-RETURN-TRAN.                                      EL1284
01502                                                                   EL1284
01503  8300-SEND-TEXT.                                                  EL1284
01504      EXEC CICS SEND TEXT                                          EL1284
01505           FROM(LOGOFF-TEXT)                                       EL1284
01506           ERASE                                                   EL1284
01507           FREEKB                                                  EL1284
01508           LENGTH(LOGOFF-LENGTH)                                   EL1284
01509      END-EXEC.                                                    EL1284
01510                                                                   EL1284
01511      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.                EL1284
01512                                                                   EL1284
01513      EXEC CICS RETURN                                             EL1284
01514      END-EXEC.                                                    EL1284
01515                                                                   EL1284
01516  8800-UNAUTHORIZED-ACCESS.                                        EL1284
01517      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL1284
01518      GO TO 8300-SEND-TEXT.                                        EL1284
01519                                                                   EL1284
01520  9000-RETURN-CICS.                                                EL1284
01521      IF PI-CHANGES-MADE                                           EL1284
01522         MOVE ER-0045             TO EMI-ERROR                     EL1284
01523         MOVE -1                  TO FUNCTL                        EL1284
01524         MOVE SPACES              TO PFENTERO                      EL1284
01525         MOVE AL-UNNOF            TO PFENTERA                      EL1284
01526         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1284
01527         GO TO 8200-SEND-DATAONLY                                  EL1284
01528      END-IF.                                                      EL1284
01529                                                                   EL1284
01530      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL1284
01531      MOVE XCTL-005               TO PGM-NAME.                     EL1284
01532      GO TO 9300-XCTL.                                             EL1284
01533                                                                   EL1284
01534  9100-RETURN-TRAN.                                                EL1284
01535      MOVE SCRN-NUMBER            TO PI-CURRENT-SCREEN-NO.         EL1284
01536      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL1284
01537      EXEC CICS RETURN                                             EL1284
01538           TRANSID(TRANS-ID)                                       EL1284
01539           COMMAREA(PROGRAM-INTERFACE-BLOCK)                       EL1284
01540           LENGTH(PI-COMM-LENGTH)                                  EL1284
01541      END-EXEC.                                                    EL1284
01542                                                                   EL1284
01543                                                                   EL1284
01544  9200-RETURN-MAIN-MENU.                                           EL1284
01545      IF PI-CHANGES-MADE                                           EL1284
01546         MOVE -1                  TO FUNCTL                        EL1284
01547         MOVE SPACES              TO PFENTERO                      EL1284
01548         MOVE AL-UNNOF            TO PFENTERA                      EL1284
01549         MOVE ER-0045             TO EMI-ERROR                     EL1284
01550         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1284
01551         GO TO 8200-SEND-DATAONLY                                  EL1284
01552      END-IF.                                                      EL1284
01553                                                                   EL1284
01554      MOVE XCTL-126               TO PGM-NAME.                     EL1284
01555                                                                   EL1284
01556  9300-XCTL.                                                       EL1284
01557      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.                EL1284
01558      EXEC CICS XCTL                                               EL1284
01559           PROGRAM  (PGM-NAME)                                     EL1284
01560           COMMAREA (PROGRAM-INTERFACE-BLOCK)                      EL1284
01561           LENGTH   (PI-COMM-LENGTH)                               EL1284
01562      END-EXEC.                                                    EL1284
01563                                                                   EL1284
01564  9400-CLEAR.                                                      EL1284
01565                                                                   EL1284
01566      IF PI-CHANGES-MADE                                           EL1284
01567          MOVE ER-0045            TO EMI-ERROR                     EL1284
               MOVE -1                 TO FUNCTL                        EL1284
               SET CURSOR-SET TO TRUE                                   EL1284
01568          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL1284
               PERFORM 7800-MOVE-SAVE-TO-MAP                            EL1284
01569          IF PI-CURRENT-LINE GREATER THAN ZERO                     EL1284
01570              PERFORM 7500-READ-TS THRU 7599-EXIT                  EL1284
01571              SET TB-INDX TO PI-CURRENT-LINE                       EL1284
01572              PERFORM 7100-FORMAT-SCREEN  THRU  7100-EXIT          EL1284
01573                  VARYING SC-INDX FROM 1 BY 1 UNTIL                EL1284
01574                  SC-INDX GREATER NUM-LINES-PER-SCREEN             EL1284
01575          END-IF                                                   EL1284
01576          GO TO 8100-SEND-INITIAL-MAP                              EL1284
01577      END-IF.                                                      EL1284
01578                                                                   EL1284
01579  9410-RETURN.                                                     EL1284
01580 *    IF PF5-PRESSED                                               EL1284
01581 *        MOVE PGM-EL1276           TO PGM-NAME                    EL1284
01582 *    ELSE                                                         EL1284
01583 *        IF PF6-PRESSED                                           EL1284
01584 *            IF PI-CLAIM-NOTE                                     EL1284
01585 *                SET PI-CERT-NOTE  TO TRUE                        EL1284
01586 *            ELSE                                                 EL1284
01587 *                SET PI-CLAIM-NOTE TO TRUE                        EL1284
01588 *            END-IF                                               EL1284
01589 *            SET PI-CHANGE-IN-NOTE-TYPE TO TRUE                   EL1284
01590 *            MOVE 'N'              TO PI-PF6-PRESSED              EL1284
01591 *            GO TO 1000-START                                     EL1284
01592 *        ELSE                                                     EL1284
01593              MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME                EL1284
01594 *        END-IF                                                   EL1284
01595 *    END-IF.                                                      EL1284
01596      GO TO 9300-XCTL.                                             EL1284
01597                                                                   EL1284
01598  9500-PF12.                                                       EL1284
01599      IF PI-CHANGES-MADE                                           EL1284
01600         MOVE -1                  TO FUNCTL                        EL1284
01601         MOVE SPACES              TO PFENTERO                      EL1284
01602         MOVE AL-UNNOF            TO PFENTERA                      EL1284
01603         MOVE ER-0045             TO EMI-ERROR                     EL1284
01604         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL1284
01605         GO TO 8200-SEND-DATAONLY                                  EL1284
01606      END-IF.                                                      EL1284
01607                                                                   EL1284
01608      MOVE 'EL010'                TO PGM-NAME.                     EL1284
01609      GO TO 9300-XCTL.                                             EL1284
01610                                                                   EL1284
01611      EJECT                                                        EL1284
01612                                                                   EL1284
01613      EJECT                                                        EL1284
01614  9600-PGMID-ERROR.                                                EL1284
01615      EXEC CICS HANDLE CONDITION                                   EL1284
01616           PGMIDERR(8300-SEND-TEXT)                                EL1284
01617      END-EXEC.                                                    EL1284
01618                                                                   EL1284
01619      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL1284
01620      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL1284
01621      MOVE XCTL-005               TO  PGM-NAME.                    EL1284
01622      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL1284
01623      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL1284
01624                                                                   EL1284
01625      GO TO 9300-XCTL.                                             EL1284
01626                                                                   EL1284
01627  9700-LINK-DATE-CONVERT.                                          EL1284
01628                                                                   EL1284
01629      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL1284
01630      EXEC CICS LINK                                               EL1284
01631          PROGRAM    (PGM-NAME)                                    EL1284
01632          COMMAREA   (DATE-CONVERSION-DATA)                        EL1284
01633          LENGTH     (DC-COMM-LENGTH)                              EL1284
01634      END-EXEC.                                                    EL1284
01635                                                                   EL1284
01636  9700-EXIT.                                                       EL1284
01637      EXIT.                                                        EL1284
01638                                                                   EL1284
01639  9900-ERROR-FORMAT.                                               EL1284
01640      IF NOT EMI-ERRORS-COMPLETE                                   EL1284
01641         MOVE LINK-001            TO  PGM-NAME                     EL1284
01642         EXEC CICS LINK                                            EL1284
01643              PROGRAM(PGM-NAME)                                    EL1284
01644              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL1284
01645              LENGTH(EMI-COMM-LENGTH)                              EL1284
01646         END-EXEC                                                  EL1284
01647      END-IF.                                                      EL1284
01648  9900-EXIT.                                                       EL1284
01649      EXIT.                                                        EL1284
01650                                                                   EL1284
01651  9990-ABEND.                                                      EL1284
01652      MOVE LINK-004               TO  PGM-NAME.                    EL1284
01653      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL1284
01654                                                                   EL1284
01655      EXEC CICS LINK                                               EL1284
01656          PROGRAM   (PGM-NAME)                                     EL1284
01657          COMMAREA  (EMI-LINE1)                                    EL1284
01658          LENGTH    (72)                                           EL1284
01659      END-EXEC.                                                    EL1284
01660                                                                   EL1284
01661      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGBO.                    EL1284
01662      MOVE -1                     TO  FUNCTL.                      EL1284
01663                                                                   EL1284
01664      GO TO 8100-SEND-INITIAL-MAP.                                 EL1284
01665                                                                   EL1284
01666      GOBACK.                                                      EL1284
01667                                                                   EL1284
01668  9995-SECURITY-VIOLATION.                                         EL1284
01669                                  COPY ELCSCTP.                    EL1284
01670                                                                   EL1284
01671  9995-EXIT.                                                       EL1284
01672      EXIT.                                                        EL1284
