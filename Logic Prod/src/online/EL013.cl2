00001  IDENTIFICATION DIVISION.                                         11/04/97
00002                                                                   EL013
00003  PROGRAM-ID.                 EL013 .                                 LV006
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/12/96 09:28:44.                    CL**5
00007 *                            VMOD=2.006.                             CL**6
00008 *                                                                 EL013
00008 *                                                                 EL013
00009 *AUTHOR.     LOGIC,INC.                                              CL**5
00010 *            DALLAS, TEXAS.                                          CL**5
00011                                                                   EL013
00012 *DATE-COMPILED.                                                      CL**5
00013 *SECURITY.   *****************************************************   CL**5
00014 *            *                                                   *   CL**5
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
00016 *            *                                                   *   CL**5
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
00020 *            *                                                   *   CL**5
00021 *            *****************************************************   CL**5
00022                                                                   EL013
00023 *REMARKS.    TRANSACTION - EXRT - RATING CALCULATIONS                CL**4
00024 *        THIS PROGRAM IS USED TO REVIEW THE RATING                   CL**4
00025 *        CALCULATIONS DONE BY THE SUBROUTINE ELRATE.                 CL**4
00026                                                                   EL013
00027      EJECT                                                        EL013
00028  ENVIRONMENT DIVISION.                                            EL013
00029  DATA DIVISION.                                                   EL013
00030  WORKING-STORAGE SECTION.                                         EL013
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL013
00032  77  FILLER  PIC X(32)  VALUE '*    EL013 WORKING STORAGE     *'. EL013
00033  77  FILLER  PIC X(32)  VALUE '***********VMOD=2.006 **********'.    CL**6
00034                                                                   EL013
00035  01  WS-DATE-AREA.                                                EL013
00036      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL013
00037      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL013
00038                                                                   EL013
00039  01  STANDARD-AREAS.                                              EL013
00040      12  MAP-NAME                PIC X(8)    VALUE 'EL013A'.      EL013
00041      12  MAPSET-NAME             PIC X(8)    VALUE 'EL013S'.      EL013
00042      12  TRANS-ID                PIC X(4)    VALUE 'EXRT'.        EL013
00043      12  PGM-NAME                PIC X(8)    VALUE SPACES.        EL013
00044      12  LINK-ELRTRM             PIC X(8)    VALUE 'ELRTRM'.      EL013
00045      12  LINK-ELRATE             PIC X(8)    VALUE 'ELRATE'.      EL013
00046      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL013
00047      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL013
00048      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL013
00049      12  TIME-IN                 PIC S9(7).                       EL013
00050      12  TIME-OUT-R   REDEFINES TIME-IN.                          EL013
00051          16  FILLER              PIC X.                           EL013
00052          16  TIME-OUT            PIC 99V99.                       EL013
00053          16  FILLER              PIC X(2).                        EL013
00054                                                                   EL013
00055  01  MISC-WORK-AREAS.                                             EL013
00056      12  WS-COMPANY-CODE         PIC S9(4)   VALUE +0    COMP.    EL013
00057      12  WS-COMPANY-CODE-R          REDEFINES WS-COMPANY-CODE.    EL013
00058          16  FILLER              PIC X.                           EL013
00059          16  WS-COMP-CD          PIC X.                           EL013
00060      12  TEXT-AREA               PIC X(66).                       EL013
00061      12  TEXT-LENGTH             PIC S9(4)   VALUE +66   COMP.    EL013
00062                                                                      CL**2
00063      12  WS-EDIT-BEN-CODE        PIC XX.                             CL**2
00064          88  INVALID-BENEFIT-CODE   VALUE '  ' '00'                  CL**2
00065                                           '90' THRU '99'.            CL**2
00066                                                                   EL013
00067      12  ER-0349                 PIC X(4)   VALUE '0349'.            CL**4
00068      12  ER-2474                 PIC X(4)   VALUE '2474'.         EL013
00069      12  ER-2477                 PIC X(4)   VALUE '2477'.         EL013
00070      12  ER-2481                 PIC X(4)   VALUE '2481'.         EL013
00071      12  ER-2482                 PIC X(4)   VALUE '2482'.         EL013
00072      12  ER-2484                 PIC X(4)   VALUE '2484'.            CL**4
00073      12  ER-2485                 PIC X(4)   VALUE '2485'.         EL013
00074      12  ER-2486                 PIC X(4)   VALUE '2486'.         EL013
00075      12  ER-2487                 PIC X(4)   VALUE '2487'.         EL013
00076      12  ER-2488                 PIC X(4)   VALUE '2488'.         EL013
00077      12  ER-2489                 PIC X(4)   VALUE '2489'.         EL013
00078      12  ER-2490                 PIC X(4)   VALUE '2490'.         EL013
00079      12  ER-2496                 PIC X(4)   VALUE '2496'.         EL013
00080      12  ER-2550                 PIC X(4)   VALUE '2550'.         EL013
00081      12  ER-2551                 PIC X(4)   VALUE '2551'.         EL013
00082      12  ER-2552                 PIC X(4)   VALUE '2552'.         EL013
00083      12  ER-2553                 PIC X(4)   VALUE '2553'.         EL013
00084      12  ER-2554                 PIC X(4)   VALUE '2554'.         EL013
00085      12  ER-2555                 PIC X(4)   VALUE '2555'.         EL013
00086      12  ER-2556                 PIC X(4)   VALUE '2556'.         EL013
00087      12  ER-2557                 PIC X(4)   VALUE '2557'.         EL013
00088      12  ER-3032                 PIC X(4)   VALUE '3032'.            CL**4
00089      12  ER-3126                 PIC X(4)   VALUE '3126'.            CL**4
00090                                                                   EL013
00091  01  TEXT-MESSAGES.                                               EL013
00092      12  TRAN-COMPLETE-MSG.                                       EL013
00093          16  FILLER              PIC X(45)                        EL013
00094            VALUE '     CLEAR ENTERED - SESSION ENDED'.            EL013
00095      EJECT                                                        EL013
00096                                  COPY ELCDATE.                       CL**4
00097      EJECT                                                        EL013
00098                                  COPY ELCATTR.                       CL**4
00099      EJECT                                                        EL013
00100                                  COPY ELCEMIB.                       CL**4
00101      EJECT                                                        EL013
00102                                  COPY ELCCALC.                       CL**4
00103      EJECT                                                        EL013
00104                                  COPY ELCAID.                        CL**4
00105      EJECT                                                        EL013
00106                                  COPY EL013S.                        CL**4
00107      EJECT                                                        EL013
00108  LINKAGE SECTION.                                                 EL013
00109  01  DFHCOMMAREA                 PIC X(450).                         CL**6
00110      EJECT                                                        EL013
00111  PROCEDURE DIVISION.                                              EL013
00112      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL013
00113      MOVE '5'                    TO DC-OPTION-CODE.               EL013
00114      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL013
00115      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL013
00116      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL013
00117                                                                   EL013
00118      IF EIBAID = DFHCLEAR                                         EL013
00119          GO TO 8950-CLEAR-RETURN.                                 EL013
00120                                                                   EL013
00121      MOVE LOW-VALUES             TO EL013AO.                      EL013
00122                                                                   EL013
00123      IF EIBCALEN NOT GREATER THAN ZERO                            EL013
00124          GO TO 8100-SEND-INITIAL-MAP.                             EL013
00125                                                                   EL013
00126      MOVE DFHCOMMAREA            TO CALCULATION-PASS-AREA.        EL013
00127                                                                   EL013
00128      EXEC CICS RECEIVE                                            EL013
00129          MAP     (MAP-NAME)                                       EL013
00130          MAPSET  (MAPSET-NAME)                                    EL013
00131          INTO    (EL013AI)                                        EL013
00132      END-EXEC.                                                    EL013
00133                                                                   EL013
00134      EJECT                                                        EL013
00135  1000-MOVE-DATA-TO-PASS-AREA.                                     EL013
00136      IF ORIGTRML NOT = ZEROS                                      EL013
00137          IF ORIGTRMI NUMERIC                                      EL013
00138              IF ORIGTRMI GREATER 0 AND LESS 361                   EL013
00139                  MOVE ORIGTRMI   TO CP-ORIGINAL-TERM              EL013
00140                  MOVE AL-UANON   TO ORIGTRMA                      EL013
00141              ELSE                                                 EL013
00142                  MOVE ER-2550    TO EMI-ERROR                     EL013
00143                  MOVE -1         TO ORIGTRML                      EL013
00144                  MOVE AL-UNBON   TO ORIGTRMA                      EL013
00145                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL013
00146          ELSE                                                     EL013
00147              MOVE ER-2496        TO EMI-ERROR                     EL013
00148              MOVE -1             TO ORIGTRML                      EL013
00149              MOVE AL-UNBON       TO ORIGTRMA                      EL013
00150              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL013
00151      ELSE                                                         EL013
00152          MOVE ER-2477            TO EMI-ERROR                     EL013
00153          MOVE -1                 TO ORIGTRML                      EL013
00154          MOVE AL-UNBON           TO ORIGTRMA                      EL013
00155          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL013
00156                                                                   EL013
00157      IF CLASSCDL NOT = ZEROS                                      EL013
00158          MOVE CLASSCDI           TO CP-CLASS-CODE                 EL013
00159          MOVE AL-UANON           TO CLASSCDA                      EL013
00160      ELSE                                                         EL013
00161          MOVE ER-2551            TO EMI-ERROR                     EL013
00162          MOVE -1                 TO CLASSCDL                      EL013
00163          MOVE AL-UABON           TO CLASSCDA                      EL013
00164          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL013
00165                                                                   EL013
00166      IF DEVCODEL NOT = ZEROS                                      EL013
00167          MOVE DEVCODEI           TO CP-DEVIATION-CODE             EL013
00168          MOVE AL-UANON           TO DEVCODEA                      EL013
00169      ELSE                                                         EL013
00170          MOVE ER-2552            TO EMI-ERROR                     EL013
00171          MOVE -1                 TO DEVCODEL                      EL013
00172          MOVE AL-UABON           TO DEVCODEA                      EL013
00173          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL013
00174                                                                   EL013
00175      IF ORIGBENL NOT = ZEROS                                      EL013
00176          IF ORIGBENI NUMERIC                                      EL013
00177              MOVE ORIGBENI       TO CP-ORIGINAL-BENEFIT           EL013
00178              MOVE AL-UNNON       TO ORIGBENA                      EL013
00179          ELSE                                                     EL013
00180              MOVE ER-2485        TO EMI-ERROR                     EL013
00181              MOVE -1             TO ORIGBENL                      EL013
00182              MOVE AL-UNBON       TO ORIGBENA                      EL013
00183              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL013
00184      ELSE                                                         EL013
00185          MOVE ER-2553            TO EMI-ERROR                     EL013
00186          MOVE -1                 TO ORIGBENL                      EL013
00187          MOVE AL-UNBON           TO ORIGBENA                      EL013
00188          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL013
00189                                                                   EL013
00190      IF ORIGPRML NOT = ZEROS                                         CL**4
00191          IF ORIGPRMI NUMERIC                                         CL**4
00192              MOVE ORIGPRMI       TO CP-ORIGINAL-PREMIUM              CL**4
00193              MOVE AL-UNNON       TO ORIGPRMA                         CL**4
00194          ELSE                                                        CL**4
00195              MOVE ER-2484        TO EMI-ERROR                        CL**4
00196              MOVE -1             TO ORIGPRML                         CL**4
00197              MOVE AL-UNBON       TO ORIGPRMA                         CL**4
00198              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00199      ELSE                                                            CL**4
00200          MOVE ZEROS              TO CP-ORIGINAL-PREMIUM.             CL**4
00201                                                                      CL**4
00202      IF BENTYPEI = 'L' OR 'A'                                     EL013
00203          MOVE BENTYPEI           TO CP-BENEFIT-TYPE               EL013
00204          MOVE AL-UANON           TO BENTYPEA                      EL013
00205          IF BENTYPEI IS EQUAL TO 'L'                                 CL**4
00206              MOVE BENTYPEI       TO CP-LIFE-OVERRIDE-CODE            CL**4
00207          ELSE                                                        CL**4
00208              MOVE BENTYPEI       TO CP-AH-OVERRIDE-CODE              CL**4
00209      ELSE                                                         EL013
00210          MOVE ER-2481            TO EMI-ERROR                     EL013
00211          MOVE -1                 TO BENTYPEL                      EL013
00212          MOVE AL-UABON           TO BENTYPEA                      EL013
00213          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL013
00214                                                                   EL013
00215      IF BENCODEL NOT = ZEROS                                      EL013
00216          MOVE BENCODEI           TO WS-EDIT-BEN-CODE                 CL**2
00217          IF NOT INVALID-BENEFIT-CODE                                 CL**2
00218              MOVE BENCODEI       TO CP-BENEFIT-CD                 EL013
00219              MOVE AL-UANON       TO BENCODEA                         CL**2
00220          ELSE                                                     EL013
00221              MOVE ER-2489        TO EMI-ERROR                     EL013
00222              MOVE -1             TO BENCODEL                      EL013
00223              MOVE AL-UABON       TO BENCODEA                         CL**2
00224              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL013
00225      ELSE                                                         EL013
00226          MOVE ER-2554            TO EMI-ERROR                     EL013
00227          MOVE -1                 TO BENCODEL                      EL013
00228          MOVE AL-UABON           TO BENCODEA                         CL**2
00229          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL013
00230                                                                   EL013
00231      IF STCODEL NOT = ZEROS                                       EL013
00232          MOVE STCODEI            TO CP-STATE                      EL013
00233          MOVE AL-UNNON           TO STCODEA                       EL013
00234      ELSE                                                         EL013
00235          MOVE ER-2555            TO EMI-ERROR                     EL013
00236          MOVE -1                 TO STCODEL                       EL013
00237          MOVE AL-UNBON           TO STCODEA                       EL013
00238          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL013
00239                                                                   EL013
00240      IF STABBRL NOT = ZEROS                                       EL013
00241          MOVE STABBRI            TO CP-STATE-STD-ABBRV            EL013
00242          MOVE AL-UANON           TO STABBRA.                      EL013
00243                                                                   EL013
00244      IF COMPIDL NOT = ZEROS                                       EL013
00245          MOVE COMPIDI            TO CP-COMPANY-ID                 EL013
00246          MOVE AL-UANON           TO COMPIDA.                      EL013
00247                                                                   EL013
00248      IF COMPCDL NOT = ZEROS                                       EL013
00249          IF COMPCDI NUMERIC                                       EL013
00250              MOVE COMPCDI        TO WS-COMPANY-CODE               EL013
00251              MOVE WS-COMP-CD     TO CP-COMPANY-CD                 EL013
00252              MOVE AL-UANON       TO COMPCDA                       EL013
00253          ELSE                                                     EL013
00254              MOVE ER-2488        TO EMI-ERROR                     EL013
00255              MOVE -1             TO COMPCDL                       EL013
00256              MOVE AL-UNBON       TO COMPCDA                       EL013
00257              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL013
00258      ELSE                                                         EL013
00259          MOVE ER-2556            TO EMI-ERROR                     EL013
00260          MOVE -1                 TO COMPCDL                       EL013
00261          MOVE AL-UNBON           TO COMPCDA                       EL013
00262          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL013
00263                                                                   EL013
00264      IF ISSAGEL NOT = ZEROS                                       EL013
00265          IF ISSAGEI NUMERIC                                       EL013
00266              MOVE ISSAGEI        TO CP-ISSUE-AGE                  EL013
00267              MOVE AL-UNNON       TO ISSAGEA                       EL013
00268          ELSE                                                     EL013
00269              MOVE ER-2490        TO EMI-ERROR                     EL013
00270              MOVE -1             TO ISSAGEL                       EL013
00271              MOVE AL-UNBON       TO ISSAGEA                       EL013
00272              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL013
00273      ELSE                                                         EL013
00274          MOVE ER-2557            TO EMI-ERROR                     EL013
00275          MOVE -1                 TO ISSAGEL                       EL013
00276          MOVE AL-UNBON           TO ISSAGEA                       EL013
00277          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL013
00278                                                                   EL013
00279      IF RATMETHL NOT = ZEROS                                      EL013
00280          MOVE RATMETHI           TO CP-EARNING-METHOD             EL013
00281          MOVE AL-UANON           TO RATMETHA.                     EL013
00282                                                                   EL013
00283      IF CALCODEL NOT = ZEROS                                      EL013
00284          MOVE CALCODEI           TO CP-SPECIAL-CALC-CD            EL013
00285          MOVE AL-UANON           TO CALCODEA.                     EL013
00286                                                                   EL013
00287      IF APRL NOT = ZEROS                                          EL013
00288          IF APRI NUMERIC                                          EL013
00289              MOVE APRI           TO CP-LOAN-APR                   EL013
00290              MOVE AL-UNNON       TO APRA                          EL013
00291          ELSE                                                     EL013
00292              MOVE ER-2487        TO EMI-ERROR                     EL013
00293              MOVE -1             TO APRL                          EL013
00294              MOVE AL-UNBON       TO APRA                          EL013
00295              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL013
00296                                                                   EL013
00297      IF CP-EARN-AS-NET-PAY  AND  CP-TRUNCATED-LIFE                EL013
00298          IF CAPTERML NOT = ZEROS                                  EL013
00299              IF CAPTERMI NUMERIC                                  EL013
00300                  IF CAPTERMI GREATER THAN 0 AND LESS THAN 361     EL013
00301                      MOVE CAPTERMI   TO CP-LOAN-TERM              EL013
00302                      MOVE AL-UANON   TO CAPTERMA                  EL013
00303                  ELSE                                             EL013
00304                      MOVE ER-2550    TO EMI-ERROR                 EL013
00305                      MOVE -1         TO CAPTERML                  EL013
00306                      MOVE AL-UNBON   TO CAPTERMA                  EL013
00307                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL013
00308              ELSE                                                 EL013
00309                  MOVE ER-2496        TO EMI-ERROR                 EL013
00310                  MOVE -1             TO CAPTERML                  EL013
00311                  MOVE AL-UNBON       TO CAPTERMA                  EL013
00312                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL013
00313          ELSE                                                     EL013
00314              MOVE ER-2477            TO EMI-ERROR                 EL013
00315              MOVE -1                 TO CAPTERML                  EL013
00316              MOVE AL-UNBON           TO CAPTERMA                  EL013
00317              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL013
00318      ELSE                                                         EL013
00319          MOVE ZEROS              TO CP-LOAN-TERM                  EL013
00320          MOVE AL-UANON           TO CAPTERMA.                     EL013
00321                                                                   EL013
00322      IF PAYFREQL NOT = ZEROS                                      EL013
00323          MOVE PAYFREQI           TO CP-PAY-FREQUENCY              EL013
00324          MOVE AL-UANON           TO PAYFREQA.                     EL013
00325                                                                   EL013
00326      IF CERTISSL NOT = ZEROS                                      EL013
00327          IF CERTISSI NUMERIC                                      EL013
00328              MOVE CERTISSI       TO DC-GREG-DATE-1-MDY            EL013
00329              MOVE 4              TO DC-OPTION-CODE                EL013
00330              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT             EL013
00331              IF NO-CONVERSION-ERROR                               EL013
00332                  MOVE DC-BIN-DATE-1  TO CP-CERT-EFF-DT            EL013
00333                  MOVE AL-UNNON   TO CERTISSA                      EL013
00334              ELSE                                                 EL013
00335                  MOVE ER-2474    TO EMI-ERROR                     EL013
00336                  MOVE -1         TO CERTISSL                      EL013
00337                  MOVE AL-UNBON   TO CERTISSA                      EL013
00338                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL013
00339          ELSE                                                     EL013
00340              MOVE ER-2482        TO EMI-ERROR                     EL013
00341              MOVE -1             TO CERTISSL                      EL013
00342              MOVE AL-UNBON       TO CERTISSA                      EL013
00343              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL013
00344      ELSE                                                         EL013
00345          MOVE EIBDATE            TO DC-JULIAN-YYDDD               EL013
00346          MOVE 5                  TO DC-OPTION-CODE                EL013
00347          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                 EL013
00348          MOVE DC-BIN-DATE-1      TO CP-CERT-EFF-DT.               EL013
00349                                                                   EL013
00350      IF FSTPMTL NOT = ZEROS                                          CL**4
00351          IF FSTPMTI NUMERIC                                          CL**4
00352              MOVE FSTPMTI        TO DC-GREG-DATE-1-MDY               CL**4
00353              MOVE 4              TO DC-OPTION-CODE                   CL**4
00354              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                CL**4
00355              IF NO-CONVERSION-ERROR                                  CL**4
00356                  MOVE DC-BIN-DATE-1  TO CP-FIRST-PAY-DATE            CL**4
00357                  MOVE AL-UNNON   TO FSTPMTA                          CL**4
00358              ELSE                                                    CL**4
00359                  MOVE ER-0349    TO EMI-ERROR                        CL**4
00360                  MOVE -1         TO FSTPMTL                          CL**4
00361                  MOVE AL-UNBON   TO FSTPMTA                          CL**4
00362                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**4
00363          ELSE                                                        CL**4
00364              MOVE ER-0349        TO EMI-ERROR                        CL**4
00365              MOVE -1             TO FSTPMTL                          CL**4
00366              MOVE AL-UNBON       TO FSTPMTA                          CL**4
00367              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00368      ELSE                                                            CL**4
00369          MOVE EIBDATE            TO DC-JULIAN-YYDDD                  CL**4
00370          MOVE 5                  TO DC-OPTION-CODE                   CL**4
00371          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                    CL**4
00372          MOVE DC-BIN-DATE-1      TO CP-CERT-EFF-DT.                  CL**4
00373                                                                      CL**4
00374      IF EXTDAYL NOT = ZEROS                                          CL**4
00375          IF EXTDAYI NUMERIC                                          CL**4
00376              MOVE EXTDAYI        TO CP-TERM-OR-EXT-DAYS              CL**4
00377              MOVE AL-UNNON       TO EXTDAYA                          CL**4
00378          ELSE                                                        CL**4
00379              MOVE ER-3032        TO EMI-ERROR                        CL**4
00380              MOVE -1             TO EXTDAYL                          CL**4
00381              MOVE AL-UNBON       TO EXTDAYA                          CL**4
00382              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**4
00383                                                                      CL**4
00384      IF DEVPCTL NOT = ZEROS                                          CL**4
00385          IF DEVPCTI NUMERIC                                          CL**4
00386              MOVE DEVPCTI        TO CP-RATE-DEV-PCT                  CL**4
00387              MOVE AL-UNNON       TO DEVPCTA                          CL**4
00388          ELSE                                                        CL**4
00389              MOVE ER-3126        TO EMI-ERROR                        CL**4
00390              MOVE -1             TO DEVPCTL                          CL**4
00391              MOVE AL-UNBON       TO DEVPCTA                          CL**4
00392              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**4
00393                                                                      CL**4
00394      IF EMI-ERROR NOT = ZEROS                                     EL013
00395          GO TO 8200-SEND-DATAONLY.                                EL013
00396                                                                      CL**4
00397      IF COMPIDI IS EQUAL TO 'NCB'                                    CL**4
00398          IF STABBRI IS EQUAL TO 'NC'                                 CL**4
00399              IF RATMETHI IS EQUAL TO '5'                             CL**4
00400                  COMPUTE CP-ORIGINAL-BENEFIT =                       CL**4
00401                      CP-ORIGINAL-BENEFIT - CP-ORIGINAL-PREMIUM.      CL**4
00402                                                                   EL013
00403      MOVE LINK-ELRATE            TO PGM-NAME.                     EL013
00404                                                                   EL013
00405      PERFORM 9700-LINK THRU 9700-EXIT.                            EL013
00406                                                                   EL013
00407      EJECT                                                        EL013
00408  2000-FORMAT-RESULTS.                                             EL013
00409      IF CP-RETURN-CODE = ZERO                                     EL013
00410          MOVE CP-CALC-PREMIUM    TO PREMIUMO                      EL013
00411          MOVE CP-CDT-FACTOR      TO FACTORO                       EL013
00412          MOVE CP-PREMIUM-RATE    TO RATEO                         EL013
00413      ELSE                                                         EL013
00414          MOVE CP-RETURN-CODE     TO ERRCODEO.                     EL013
00415                                                                   EL013
00416      EJECT                                                        EL013
00417  8100-SEND-INITIAL-MAP.                                           EL013
00418      MOVE SAVE-DATE              TO RUNDATEO.                     EL013
00419      MOVE EIBTIME                TO TIME-IN.                      EL013
00420      MOVE TIME-OUT               TO RUNTIMEO.                     EL013
00421      MOVE -1                     TO ORIGTRML.                     EL013
00422      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL013
00423                                                                   EL013
00424      EXEC CICS SEND                                               EL013
00425          MAP    (MAP-NAME)                                        EL013
00426          MAPSET (MAPSET-NAME)                                     EL013
00427          FROM   (EL013AO)                                         EL013
00428          ERASE                                                    EL013
00429          CURSOR                                                   EL013
00430      END-EXEC.                                                    EL013
00431                                                                   EL013
00432      GO TO 9100-RETURN-TRAN.                                      EL013
00433                                                                   EL013
00434  8200-SEND-DATAONLY.                                              EL013
00435      MOVE SAVE-DATE              TO RUNDATEO.                     EL013
00436      MOVE EIBTIME                TO TIME-IN.                      EL013
00437      MOVE TIME-OUT               TO RUNTIMEO.                     EL013
00438      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL013
00439                                                                   EL013
00440      EXEC CICS SEND                                               EL013
00441          MAP    (MAP-NAME)                                        EL013
00442          MAPSET (MAPSET-NAME)                                     EL013
00443          FROM   (EL013AO)                                         EL013
00444          DATAONLY                                                 EL013
00445          CURSOR                                                   EL013
00446      END-EXEC.                                                    EL013
00447                                                                   EL013
00448      MOVE LOW-VALUES             TO EL013AO.                      EL013
00449                                                                   EL013
00450      GO TO 9100-RETURN-TRAN.                                      EL013
00451      EJECT                                                        EL013
00452  8500-DATE-CONVERT.                                               EL013
00453      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL013
00454                                                                   EL013
00455      EXEC CICS LINK                                               EL013
00456          PROGRAM    (PGM-NAME)                                    EL013
00457          COMMAREA   (DATE-CONVERSION-DATA)                        EL013
00458          LENGTH     (DC-COMM-LENGTH)                              EL013
00459      END-EXEC.                                                    EL013
00460                                                                   EL013
00461  8500-EXIT.                                                       EL013
00462      EXIT.                                                        EL013
00463                                                                   EL013
00464      EJECT                                                        EL013
00465  8950-CLEAR-RETURN.                                               EL013
00466      MOVE TRAN-COMPLETE-MSG      TO TEXT-AREA.                    EL013
00467                                                                   EL013
00468  8990-SEND-TEXT.                                                  EL013
00469      EXEC CICS SEND TEXT                                          EL013
00470          FROM    (TEXT-AREA)                                      EL013
00471          LENGTH  (TEXT-LENGTH)                                    EL013
00472          ERASE                                                    EL013
00473          FREEKB                                                   EL013
00474      END-EXEC.                                                    EL013
00475                                                                   EL013
00476  9000-RETURN-CICS.                                                EL013
00477      EXEC CICS RETURN                                             EL013
00478      END-EXEC.                                                    EL013
00479                                                                   EL013
00480                                                                   EL013
00481                                                                   EL013
00482  9100-RETURN-TRAN.                                                EL013
00483      EXEC CICS RETURN                                             EL013
00484          TRANSID   (TRANS-ID)                                     EL013
00485          COMMAREA  (CALCULATION-PASS-AREA)                        EL013
00486          LENGTH    (CP-COMM-LENGTH)                               EL013
00487      END-EXEC.                                                    EL013
00488                                                                   EL013
00489  9700-LINK.                                                       EL013
00490      EXEC CICS LINK                                               EL013
00491          PROGRAM   (PGM-NAME)                                     EL013
00492          COMMAREA  (CALCULATION-PASS-AREA)                        EL013
00493          LENGTH    (CP-COMM-LENGTH)                               EL013
00494      END-EXEC.                                                    EL013
00495                                                                   EL013
00496  9700-EXIT.                                                       EL013
00497       EXIT.                                                       EL013
00498                                                                   EL013
00499      EJECT                                                        EL013
00500  9900-ERROR-FORMAT.                                               EL013
00501      IF NOT EMI-ERRORS-COMPLETE                                   EL013
00502          MOVE LINK-001           TO PGM-NAME                      EL013
00503          EXEC CICS LINK                                           EL013
00504              PROGRAM   (PGM-NAME)                                 EL013
00505              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL013
00506              LENGTH    (EMI-COMM-LENGTH)                          EL013
00507          END-EXEC.                                                EL013
00508                                                                   EL013
00509  9900-EXIT.                                                       EL013
00510      EXIT.                                                        EL013
00511                                                                   EL013
00512  9990-ABEND.                                                      EL013
00513      MOVE LINK-004               TO PGM-NAME.                     EL013
00514      MOVE DFHEIBLK               TO EMI-LINE1.                    EL013
00515                                                                   EL013
00516      EXEC CICS LINK                                               EL013
00517          PROGRAM   (PGM-NAME)                                     EL013
00518          COMMAREA  (EMI-LINE1)                                    EL013
00519          LENGTH    (72)                                           EL013
00520      END-EXEC.                                                    EL013
00521                                                                   EL013
00522      GO TO 8200-SEND-DATAONLY.                                    EL013
00523      GOBACK.                                                      EL013
