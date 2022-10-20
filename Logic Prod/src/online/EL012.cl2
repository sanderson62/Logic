00001  IDENTIFICATION DIVISION.                                         08/14/97
00002                                                                   EL012
00003  PROGRAM-ID.                 EL012 .                                 LV010
00004 *              PROGRAM CONVERTED BY                                  CL**8
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**8
00006 *              CONVERSION DATE 02/12/96 09:27:41.                    CL**8
00007 *                            VMOD=2.010.                             CL*10
00008 *                                                                 EL012
00008 *                                                                 EL012
00009 *AUTHOR.     LOGIC,INC.                                              CL**8
00010 *            DALLAS, TEXAS.                                          CL**8
00011                                                                   EL012
00012 *DATE-COMPILED.                                                      CL**8
00013 *SECURITY.   *****************************************************   CL**8
00014 *            *                                                   *   CL**8
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**8
00016 *            *                                                   *   CL**8
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**8
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**8
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**8
00020 *            *                                                   *   CL**8
00021 *            *****************************************************   CL**8
00022                                                                   EL012
00023 *REMARKS.    TRANSACTION - EXRF - REFUND AND TERM CALCULATION        CL**8
00024 *        THIS PROGRAM IS USED TO REVIEW THE REFUND AND TERM          CL**8
00025 *        CALCULATIONS DONE BY THE SUBROUTINE ELRFND.                 CL**8
00026                                                                   EL012
00027                                                                   EL012
00028      EJECT                                                        EL012
00029  ENVIRONMENT DIVISION.                                            EL012
00030  DATA DIVISION.                                                   EL012
00031  WORKING-STORAGE SECTION.                                         EL012
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL012
00033  77  FILLER  PIC X(32)  VALUE '*    EL012 WORKING STORAGE     *'. EL012
00034  77  FILLER  PIC X(32)  VALUE '**********VMOD=2.010 ***********'.    CL*10
00035                                                                   EL012
00036  01  WS-DATE-AREA.                                                EL012
00037      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL012
00038      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL012
00039                                                                   EL012
00040  01  STANDARD-AREAS.                                              EL012
00041      12  MAP-NAME                PIC X(8)    VALUE 'EL012A'.      EL012
00042      12  MAPSET-NAME             PIC X(8)    VALUE 'EL012S'.      EL012
00043      12  TRANS-ID                PIC X(4)    VALUE 'EXRF'.        EL012
00044      12  PGM-NAME                PIC X(8)    VALUE SPACES.        EL012
00045      12  ELCNTL-ID               PIC X(8)    VALUE 'ELCNTL'.         CL**7
00046      12  LINK-ELRAMT             PIC X(8)    VALUE 'ELRAMT'.         CL**7
00047      12  LINK-ELRTRM             PIC X(8)    VALUE 'ELRTRM'.      EL012
00048      12  LINK-ELRFND             PIC X(8)    VALUE 'ELRFND'.      EL012
00049      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL012
00050      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL012
00051      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL012
00052      12  TIME-IN                 PIC S9(7).                       EL012
00053      12  TIME-OUT-R   REDEFINES TIME-IN.                          EL012
00054          16  FILLER              PIC X.                           EL012
00055          16  TIME-OUT            PIC 99V99.                       EL012
00056          16  FILLER              PIC X(2).                        EL012
00057                                                                   EL012
00058  01  MISC-WORK-AREAS.                                             EL012
00059      12  WS-COMPANY-CODE         PIC S9(4)   VALUE +0    COMP.    EL012
00060      12  WS-COMPANY-CODE-R      REDEFINES WS-COMPANY-CODE.        EL012
00061          16  FILLER              PIC X.                           EL012
00062          16  WS-COMP-CD          PIC X.                           EL012
00063      12  TEXT-AREA               PIC X(66).                       EL012
00064      12  TEXT-LENGTH             PIC S9(4)   VALUE +66   COMP.    EL012
00065                                                                      CL**9
00066      12  WS-RESPONSE             PIC S9(8)   COMP.                   CL**9
00067          88 WS-RESP-NORMAL                   VALUE +00.              CL**9
00068          88 WS-RESP-ERROR                    VALUE +01.              CL**9
00069          88 WS-RESP-NOTFND                   VALUE +13.              CL**9
00070          88 WS-RESP-NOTOPEN                  VALUE +19.              CL**9
00071                                                                      CL**7
00072  01  ACCESS-CNTL-KEY.                                                CL**7
00073      12  ELCNTL-KEY.                                                 CL**7
00074          16  CNTL-COMPANY-CD     PIC X(03).                          CL**7
00075          16  CNTL-REC-TYPE       PIC X(01).                          CL**7
00076          16  CNTL-ACCESS         PIC X(04).                          CL**7
00077          16  CNTL-SEQ-NO         PIC S9(04) COMP.                    CL**7
00078                                                                      CL**3
00079      12  WS-EDIT-BEN-CODE        PIC XX.                             CL**3
00080          88  INVALID-BENEFIT-CODE   VALUE '  ' '00'                  CL**3
00081                                           '90' THRU '99'.            CL**3
00082                                                                   EL012
00083      12  ER-0071                 PIC X(4)   VALUE '0071'.            CL**5
00084      12  ER-2223                 PIC X(4)   VALUE '2223'.            CL**5
00085      12  ER-2473                 PIC X(4)   VALUE '2473'.         EL012
00086      12  ER-2474                 PIC X(4)   VALUE '2474'.         EL012
00087      12  ER-2475                 PIC X(4)   VALUE '2475'.         EL012
00088      12  ER-2476                 PIC X(4)   VALUE '2476'.         EL012
00089      12  ER-2477                 PIC X(4)   VALUE '2477'.         EL012
00090      12  ER-2478                 PIC X(4)   VALUE '2478'.         EL012
00091      12  ER-2479                 PIC X(4)   VALUE '2479'.         EL012
00092      12  ER-2480                 PIC X(4)   VALUE '2480'.         EL012
00093      12  ER-2481                 PIC X(4)   VALUE '2481'.         EL012
00094      12  ER-2482                 PIC X(4)   VALUE '2482'.         EL012
00095      12  ER-2483                 PIC X(4)   VALUE '2483'.         EL012
00096      12  ER-2484                 PIC X(4)   VALUE '2484'.         EL012
00097      12  ER-2485                 PIC X(4)   VALUE '2485'.         EL012
00098      12  ER-2486                 PIC X(4)   VALUE '2486'.         EL012
00099      12  ER-2487                 PIC X(4)   VALUE '2487'.         EL012
00100      12  ER-2488                 PIC X(4)   VALUE '2488'.         EL012
00101      12  ER-2489                 PIC X(4)   VALUE '2489'.         EL012
00102      12  ER-2490                 PIC X(4)   VALUE '2490'.         EL012
00103      12  ER-2496                 PIC X(4)   VALUE '2496'.         EL012
00104      12  ER-2550                 PIC X(4)   VALUE '2550'.         EL012
00105      12  ER-2616                 PIC X(4)   VALUE '2616'.            CL**7
00106      12  ER-2617                 PIC X(4)   VALUE '2617'.            CL**7
00107      12  ER-2680                 PIC X(4)   VALUE '2680'.            CL**2
00108      12  ER-2716                 PIC X(4)   VALUE '2716'.            CL**5
00109      12  ER-2848                 PIC X(4)   VALUE '2848'.            CL**9
00110                                                                   EL012
00111  01  TEXT-MESSAGES.                                               EL012
00112      12  TRAN-COMPLETE-MSG.                                       EL012
00113          16  FILLER              PIC X(45)                        EL012
00114            VALUE '     CLEAR ENTERED - SESSION ENDED          '.  EL012
00115      EJECT                                                        EL012
00116                                     COPY ELCDATE.                    CL**6
00117      EJECT                                                        EL012
00118                                     COPY ELCATTR.                    CL**6
00119      EJECT                                                        EL012
00120                                     COPY ELCEMIB.                    CL**6
00121      EJECT                                                        EL012
00122                                     COPY ELCCALC.                    CL**6
00123      EJECT                                                        EL012
00124                                  COPY ELCAID.                        CL**6
00125      EJECT                                                        EL012
00126                                  COPY EL012S.                        CL**6
00127      EJECT                                                        EL012
00128  LINKAGE SECTION.                                                 EL012
00129  01  DFHCOMMAREA                 PIC X(750).                         CL**7
00130                                                                      CL**7
00131 *01 PARMLIST .                                                       CL**8
00132 *    12  FILLER                  PIC S9(8)       COMP.               CL**8
00133 *    12  ELCNTL-POINTER          PIC S9(8)       COMP.               CL**8
00134     EJECT                                                            CL**7
00135                                COPY ELCCNTL.                         CL**7
00136     EJECT                                                         EL012
00137  PROCEDURE DIVISION.                                              EL012
00138                                                                      CL**7
00139      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL012
00140      MOVE '5'                    TO DC-OPTION-CODE.               EL012
00141      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL012
00142      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL012
00143      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL012
00144                                                                   EL012
00145      IF EIBAID = DFHCLEAR                                         EL012
00146          GO TO 8950-CLEAR-RETURN.                                 EL012
00147                                                                   EL012
00148      MOVE LOW-VALUES             TO EL012AO.                      EL012
00149                                                                   EL012
00150      IF EIBCALEN NOT GREATER THAN ZERO                            EL012
00151          GO TO 8100-SEND-INITIAL-MAP.                             EL012
00152                                                                   EL012
00153      MOVE DFHCOMMAREA            TO CALCULATION-PASS-AREA.        EL012
00154                                                                   EL012
00155      EXEC CICS RECEIVE                                            EL012
00156          MAP     (MAP-NAME)                                       EL012
00157          MAPSET  (MAPSET-NAME)                                    EL012
00158          INTO    (EL012AI)                                        EL012
00159      END-EXEC.                                                    EL012
00160                                                                   EL012
00161      EJECT                                                        EL012
00162  1000-MOVE-DATA-TO-PASS-AREA.                                     EL012
00163      IF CERTISSL NOT = ZEROS                                      EL012
00164          IF CERTISSI NUMERIC                                      EL012
00165              MOVE CERTISSI       TO DC-GREG-DATE-1-MDY            EL012
00166              MOVE 4              TO DC-OPTION-CODE                EL012
00167              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT             EL012
00168              IF NO-CONVERSION-ERROR                               EL012
00169                  MOVE DC-BIN-DATE-1  TO CP-CERT-EFF-DT            EL012
00170                  MOVE AL-UNNON   TO CERTISSA                      EL012
00171              ELSE                                                 EL012
00172                  MOVE ER-2474    TO EMI-ERROR                     EL012
00173                  MOVE -1         TO CERTISSL                      EL012
00174                  MOVE AL-UNBON   TO CERTISSA                      EL012
00175                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL012
00176          ELSE                                                     EL012
00177              MOVE ER-2482        TO EMI-ERROR                     EL012
00178              MOVE -1             TO CERTISSL                      EL012
00179              MOVE AL-UNBON       TO CERTISSA                      EL012
00180              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL012
00181      ELSE                                                         EL012
00182          MOVE ER-2473            TO EMI-ERROR                     EL012
00183          MOVE -1                 TO CERTISSL                      EL012
00184          MOVE AL-UNBON           TO CERTISSA                      EL012
00185          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL012
00186                                                                   EL012
00187      IF CERTCANL NOT = ZEROS                                      EL012
00188          IF CERTCANI NUMERIC                                      EL012
00189              MOVE CERTCANI       TO DC-GREG-DATE-1-MDY            EL012
00190              MOVE 4              TO DC-OPTION-CODE                EL012
00191              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT             EL012
00192              IF NO-CONVERSION-ERROR                               EL012
00193                  MOVE DC-BIN-DATE-1  TO CP-VALUATION-DT           EL012
00194                  MOVE AL-UNNON   TO CERTCANA                      EL012
00195              ELSE                                                 EL012
00196                  MOVE ER-2476    TO EMI-ERROR                     EL012
00197                  MOVE -1         TO CERTCANL                      EL012
00198                  MOVE AL-UNBON   TO CERTCANA                      EL012
00199                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL012
00200          ELSE                                                     EL012
00201              MOVE ER-2483        TO EMI-ERROR                     EL012
00202              MOVE -1             TO CERTCANL                      EL012
00203              MOVE AL-UNBON       TO CERTCANA                      EL012
00204              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL012
00205      ELSE                                                         EL012
00206          MOVE ER-2475            TO EMI-ERROR                     EL012
00207          MOVE -1                 TO CERTCANL                      EL012
00208          MOVE AL-UNBON           TO CERTCANA                      EL012
00209          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL012
00210                                                                      CL**2
00211      IF FIRSTPL NOT = ZEROS                                          CL**2
00212          IF FIRSTPI NUMERIC                                          CL**2
00213              MOVE FIRSTPI        TO DC-GREG-DATE-1-MDY               CL**2
00214              MOVE 4              TO DC-OPTION-CODE                   CL**2
00215              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                CL**2
00216              IF NO-CONVERSION-ERROR                                  CL**2
00217                  MOVE DC-BIN-DATE-1  TO CP-FIRST-PAY-DATE            CL**2
00218                  MOVE AL-UNNON   TO FIRSTPA                          CL**2
00219              ELSE                                                    CL**2
00220                  MOVE ER-2680    TO EMI-ERROR                        CL**2
00221                  MOVE -1         TO FIRSTPL                          CL**2
00222                  MOVE AL-UNBON   TO FIRSTPA                          CL**2
00223                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**2
00224          ELSE                                                        CL**2
00225              MOVE ER-2680        TO EMI-ERROR                        CL**2
00226              MOVE -1             TO FIRSTPL                          CL**2
00227              MOVE AL-UNBON       TO FIRSTPA                          CL**2
00228              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**2
00229      ELSE                                                            CL**2
00230          MOVE LOW-VALUE          TO CP-FIRST-PAY-DATE.               CL**2
00231                                                                   EL012
00232      IF ORIGTRML NOT = ZEROS                                      EL012
00233          IF ORIGTRMI NUMERIC                                      EL012
00234              IF ORIGTRMI GREATER THAN 0 AND LESS THAN 361         EL012
00235                  MOVE ORIGTRMI   TO CP-ORIGINAL-TERM              EL012
00236                                     CP-LOAN-TERM                     CL**7
00237                  MOVE AL-UANON   TO ORIGTRMA                      EL012
00238              ELSE                                                 EL012
00239                  MOVE ER-2550    TO EMI-ERROR                     EL012
00240                  MOVE -1         TO ORIGTRML                      EL012
00241                  MOVE AL-UNBON   TO ORIGTRMA                      EL012
00242                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL012
00243          ELSE                                                     EL012
00244              MOVE ER-2496        TO EMI-ERROR                     EL012
00245              MOVE -1             TO ORIGTRML                      EL012
00246              MOVE AL-UNBON       TO ORIGTRMA                      EL012
00247              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL012
00248      ELSE                                                         EL012
00249          MOVE ER-2477            TO EMI-ERROR                     EL012
00250          MOVE -1                 TO ORIGTRML                      EL012
00251          MOVE AL-UNBON           TO ORIGTRMA                      EL012
00252          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL012
00253                                                                   EL012
00254      IF REMCALCL NOT = ZEROS                                      EL012
00255          MOVE REMCALCI           TO CP-REM-TERM-METHOD            EL012
00256          MOVE AL-UANON           TO REMCALCA                      EL012
00257      ELSE                                                         EL012
00258          MOVE ER-2478            TO EMI-ERROR                     EL012
00259          MOVE -1                 TO REMCALCL                      EL012
00260          MOVE AL-UABON           TO REMCALCA                      EL012
00261          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL012
00262                                                                   EL012
00263      IF ORIGPRML  = ZEROS  AND                                    EL012
00264         REFMETHL  = ZEROS  AND                                    EL012
00265         BENTYPEL  = ZEROS  AND                                    EL012
00266         EMI-ERROR = ZEROS                                         EL012
00267          GO TO 1100-LINK.                                         EL012
00268                                                                   EL012
00269      IF ORIGPRML NOT = ZEROS                                      EL012
00270          IF ORIGPRMI NUMERIC                                      EL012
00271              MOVE ORIGPRMI       TO CP-ORIGINAL-PREMIUM           EL012
00272              MOVE AL-UNNON       TO ORIGPRMA                      EL012
00273          ELSE                                                     EL012
00274              MOVE ER-2484        TO EMI-ERROR                     EL012
00275              MOVE -1             TO ORIGPRML                      EL012
00276              MOVE AL-UNBON       TO ORIGPRMA                      EL012
00277              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL012
00278      ELSE                                                         EL012
00279          MOVE ER-2479            TO EMI-ERROR                     EL012
00280          MOVE -1                 TO ORIGPRML                      EL012
00281          MOVE AL-UNBON           TO ORIGPRMA                      EL012
00282          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL012
00283                                                                   EL012
00284      IF REFMETHL NOT = ZEROS                                      EL012
00285          MOVE REFMETHI           TO CP-EARNING-METHOD             EL012
00286                                     CP-RATING-METHOD                 CL**6
00287          MOVE AL-UANON           TO REFMETHA                      EL012
00288      ELSE                                                         EL012
00289          MOVE ER-2480            TO EMI-ERROR                     EL012
00290          MOVE -1                 TO REFMETHL                      EL012
00291          MOVE AL-UABON           TO REFMETHA                      EL012
00292          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL012
00293                                                                   EL012
00294      IF BENTYPEI = 'L' OR 'R' OR 'A' OR 'P' OR 'D'                   CL**7
00295          MOVE BENTYPEI           TO CP-BENEFIT-TYPE               EL012
00296          MOVE AL-UANON           TO BENTYPEA                      EL012
00297      ELSE                                                         EL012
00298          MOVE ER-2481            TO EMI-ERROR                     EL012
00299          MOVE -1                 TO BENTYPEL                      EL012
00300          MOVE AL-UABON           TO BENTYPEA                      EL012
00301          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL012
00302                                                                   EL012
00303      IF REMTYPEL NOT EQUAL ZEROS                                     CL**5
00304          IF REMTYPEI = '1' OR '2' OR '3' OR '4'                      CL**5
00305              MOVE REMTYPEI           TO CP-REM-TRM-CALC-OPTION       CL**5
00306              MOVE AL-UANON           TO REMTYPEA                     CL**5
00307          ELSE                                                        CL**5
00308              MOVE ER-0071            TO EMI-ERROR                    CL**5
00309              MOVE -1                 TO REMTYPEL                     CL**5
00310              MOVE AL-UABON           TO REMTYPEA                     CL**5
00311              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**5
00312      ELSE                                                            CL**5
00313         MOVE SPACE                   TO CP-REM-TRM-CALC-OPTION.      CL**5
00314                                                                      CL**5
00315      IF CLCODEL NOT = ZEROS                                       EL012
00316          MOVE CLCODEI            TO CP-CLASS-CODE                 EL012
00317          MOVE AL-UANON           TO CLCODEA.                      EL012
00318                                                                   EL012
00319      IF DEVCODEL NOT = ZEROS                                      EL012
00320          MOVE DEVCODEI           TO CP-DEVIATION-CODE             EL012
00321          MOVE AL-UANON           TO DEVCODEA.                     EL012
00322                                                                   EL012
00323      IF LOANTRML NOT = ZEROS                                      EL012
00324         IF LOANTRMI NUMERIC                                       EL012
00325            MOVE LOANTRMI         TO CP-LOAN-TERM                  EL012
00326            MOVE AL-UANON         TO LOANTRMA.                        CL**7
00327                                                                   EL012
00328      IF ORIGBENL NOT = ZEROS                                      EL012
00329          IF ORIGBENI NUMERIC                                      EL012
00330              MOVE ORIGBENI       TO CP-ORIGINAL-BENEFIT           EL012
00331                                     CP-RATING-BENEFIT-AMT            CL**7
00332              MOVE AL-UNNON       TO ORIGBENA                      EL012
00333          ELSE                                                     EL012
00334              MOVE ER-2485        TO EMI-ERROR                     EL012
00335              MOVE -1             TO ORIGBENL                      EL012
00336              MOVE AL-UNBON       TO ORIGBENA                      EL012
00337              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL012
00338                                                                   EL012
00339      IF STCODEL NOT = ZEROS                                       EL012
00340          MOVE STCODEI            TO CP-STATE                      EL012
00341          MOVE AL-UANON           TO STCODEA.                      EL012
00342                                                                   EL012
00343      IF STABBRL NOT = ZEROS                                       EL012
00344          MOVE STABBRI            TO CP-STATE-STD-ABBRV            EL012
00345          MOVE AL-UANON           TO STABBRA.                      EL012
00346                                                                      CL**7
00347      IF CP-STATE-STD-ABBRV = 'OR'                                    CL**7
00348          IF CP-AH                                                    CL**7
00349              COMPUTE CP-RATING-BENEFIT-AMT =                         CL**7
00350                        CP-ORIGINAL-BENEFIT * CP-ORIGINAL-TERM.       CL**7
00351                                                                   EL012
00352      IF APRL NOT = ZEROS                                          EL012
00353          IF APRI NUMERIC                                          EL012
00354              MOVE APRI           TO CP-LOAN-APR                   EL012
00355              MOVE AL-UNNON       TO APRA                          EL012
00356          ELSE                                                     EL012
00357              MOVE ER-2487        TO EMI-ERROR                     EL012
00358              MOVE -1             TO APRL                          EL012
00359              MOVE AL-UNBON       TO APRA                          EL012
00360              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL012
00361                                                                   EL012
00362      IF PAYFREQL NOT = ZEROS                                      EL012
00363         IF PAYFREQI NUMERIC                                          CL**5
00364             MOVE PAYFREQI        TO CP-PAY-FREQUENCY                 CL**5
00365             MOVE AL-UANON        TO PAYFREQA                         CL**5
00366          ELSE                                                        CL**5
00367              MOVE ER-2223        TO EMI-ERROR                        CL**5
00368              MOVE -1             TO PAYFREQL                         CL**5
00369              MOVE AL-UNBON       TO PAYFREQA                         CL**5
00370              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**5
00371                                                                   EL012
00372      IF COMPIDL NOT = ZEROS                                       EL012
00373          MOVE COMPIDI            TO CP-COMPANY-ID                 EL012
00374          MOVE AL-UANON           TO COMPIDA.                      EL012
00375                                                                      CL**7
00376      MOVE SPACES                 TO ELCNTL-KEY.                      CL**7
00377      MOVE CP-COMPANY-ID          TO CNTL-COMPANY-CD.                 CL**7
00378      MOVE '1'                    TO CNTL-REC-TYPE.                   CL**7
00379      MOVE SPACES                 TO CNTL-ACCESS.                     CL**7
00380      MOVE +0                     TO CNTL-SEQ-NO.                     CL**7
00381                                                                      CL**7
00382      EXEC CICS HANDLE CONDITION                                      CL**7
00383          NOTFND   (1010-CNTL-NOTFND)                                 CL**7
00384          NOTOPEN  (1020-CNTL-NOTOPEN)                                CL**7
00385      END-EXEC.                                                       CL**7
00386                                                                      CL**7
00387      EXEC CICS READ                                                  CL**7
00388          DATASET  (ELCNTL-ID)                                        CL**7
00389          SET      (ADDRESS OF CONTROL-FILE)                          CL**8
00390          RIDFLD   (ELCNTL-KEY)                                       CL**7
00391          GTEQ                                                        CL**7
00392      END-EXEC.                                                       CL**7
00393                                                                      CL**7
00394      IF CNTL-COMPANY-CD = CF-COMPANY-ID  AND                         CL**7
00395         CNTL-REC-TYPE   = CF-RECORD-TYPE                             CL**7
00396          NEXT SENTENCE                                               CL**7
00397      ELSE                                                            CL**7
00398          GO TO 1010-CNTL-NOTFND.                                     CL**7
00399                                                                      CL**7
00400      IF CP-AH                                                        CL**7
00401          MOVE CF-AH-OVERRIDE-L1    TO CP-AH-OVERRIDE-CODE            CL**7
00402      ELSE                                                            CL**7
00403          MOVE CF-LIFE-OVERRIDE-L1  TO CP-LIFE-OVERRIDE-CODE.         CL**7
00404                                                                      CL**7
00405      GO TO 1030-MOVE-CONTINUE.                                       CL**7
00406                                                                      CL**7
00407  1010-CNTL-NOTFND.                                                   CL**7
00408      MOVE ER-2616                TO  EMI-ERROR.                      CL**7
00409      MOVE -1                     TO COMPIDL.                         CL**7
00410      MOVE AL-UNBON               TO COMPIDA.                         CL**7
00411      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**7
00412                                                                      CL**7
00413      GO TO 1030-MOVE-CONTINUE.                                       CL**7
00414                                                                      CL**7
00415  1020-CNTL-NOTOPEN.                                                  CL**7
00416      MOVE ER-2617                TO  EMI-ERROR.                      CL**7
00417      MOVE -1                     TO COMPIDL.                         CL**7
00418      MOVE AL-UNBON               TO COMPIDA.                         CL**7
00419      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**7
00420                                                                      CL**7
00421  1030-MOVE-CONTINUE.                                                 CL**7
00422                                                                   EL012
00423      IF COMPCDL NOT = ZEROS                                       EL012
00424          IF COMPCDI NUMERIC                                       EL012
00425              MOVE COMPCDI        TO WS-COMPANY-CODE               EL012
00426              MOVE WS-COMP-CD     TO CP-COMPANY-CD                 EL012
00427              MOVE AL-UANON       TO COMPCDA                       EL012
00428          ELSE                                                     EL012
00429              MOVE ER-2488        TO EMI-ERROR                     EL012
00430              MOVE -1             TO COMPCDL                       EL012
00431              MOVE AL-UNBON       TO COMPCDA                       EL012
00432              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL012
00433                                                                   EL012
00434      IF BENCODEL NOT = ZEROS                                      EL012
00435          MOVE BENCODEI           TO WS-EDIT-BEN-CODE                 CL**3
00436          IF NOT INVALID-BENEFIT-CODE                                 CL**3
00437              MOVE BENCODEI       TO CP-BENEFIT-CD                 EL012
00438              MOVE AL-UANON       TO BENCODEA                         CL**3
00439          ELSE                                                     EL012
00440              MOVE ER-2489        TO EMI-ERROR                     EL012
00441              MOVE -1             TO BENCODEL                      EL012
00442              MOVE AL-UABON       TO BENCODEA                         CL**3
00443              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL012
00444                                                                   EL012
00445      IF INSRAGEL NOT = ZEROS                                      EL012
00446          IF INSRAGEI NUMERIC                                      EL012
00447              MOVE INSRAGEI       TO CP-ISSUE-AGE                  EL012
00448              MOVE AL-UNNON       TO INSRAGEA                      EL012
00449          ELSE                                                     EL012
00450              MOVE ER-2490        TO EMI-ERROR                     EL012
00451              MOVE -1             TO INSRAGEL                      EL012
00452              MOVE AL-UNBON       TO INSRAGEA                      EL012
00453              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL012
00454                                                                   EL012
00455      IF SPCCALCL NOT = ZEROS                                      EL012
00456          MOVE SPCCALCI           TO CP-SPECIAL-CALC-CD            EL012
00457          MOVE AL-UANON           TO SPCCALCA.                     EL012
00458                                                                   EL012
00459      IF R78METHL NOT = ZEROS                                      EL012
00460          MOVE R78METHI           TO CP-R78-OPTION                 EL012
00461          MOVE AL-UANON           TO R78METHA.                     EL012
00462                                                                   EL012
00463      IF EMI-ERROR NOT = ZEROS                                     EL012
00464          GO TO 8200-SEND-DATAONLY.                                EL012
00465                                                                   EL012
00466  1100-LINK.                                                       EL012
00467 *    IF CP-TRUNCATED-LIFE                                            CL**7
00468 *        MOVE CP-LOAN-TERM       TO CP-ORIGINAL-TERM.                CL**7
00469                                                                      CL**9
00470 * GET FREE LOOK PERIOD FROM STATE MASTER RECORD                      CL**9
00471                                                                      CL**9
00472      IF STCODEL  = ZEROS OR                                          CL**9
00473         STCODEI  = SPACE OR                                          CL**9
00474         COMPIDL  = ZEROS OR                                          CL**9
00475         COMPIDI  = SPACE                                             CL**9
00476          MOVE ZERO               TO CP-FREE-LOOK                     CL**9
00477          GO TO 1100-LINK-CONT.                                       CL**9
00478                                                                      CL**9
00479      MOVE SPACES                 TO ELCNTL-KEY.                      CL**9
00480      MOVE COMPIDI                TO CNTL-COMPANY-CD.                 CL**9
00481      MOVE '3'                    TO CNTL-REC-TYPE.                   CL**9
00482      MOVE STCODEI                TO CNTL-ACCESS.                     CL**9
00483      MOVE +0                     TO CNTL-SEQ-NO.                     CL**9
00484                                                                      CL**9
00485      EXEC CICS READ                                                  CL**9
00486          DATASET  (ELCNTL-ID)                                        CL**9
00487          SET      (ADDRESS OF CONTROL-FILE)                          CL**9
00488          RIDFLD   (ELCNTL-KEY)                                       CL**9
00489          RESP     (WS-RESPONSE)                                      CL**9
00490      END-EXEC.                                                       CL**9
00491                                                                      CL**9
00492      IF WS-RESP-NOTFND                                               CL**9
00493         MOVE ER-2848                TO  EMI-ERROR                    CL**9
00494         MOVE -1                     TO COMPIDL                       CL**9
00495         MOVE AL-UNBON               TO COMPIDA                       CL**9
00496         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL**9
00497         GO TO 8200-SEND-DATAONLY.                                    CL**9
00498                                                                      CL**9
00499      IF WS-RESP-NOTOPEN                                              CL**9
00500         MOVE ER-2617                TO  EMI-ERROR                    CL**9
00501         MOVE -1                     TO COMPIDL                       CL**9
00502         MOVE AL-UNBON               TO COMPIDA                       CL**9
00503         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL**9
00504         GO TO 8200-SEND-DATAONLY.                                    CL**9
00505                                                                      CL**9
00506      MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.                    CL**9
00507                                                                      CL**9
00508  1100-LINK-CONT.                                                     CL**9
00509                                                                   EL012
00510      MOVE LINK-ELRTRM            TO PGM-NAME                      EL012
00511      PERFORM 9700-LINK THRU 9700-EXIT.                            EL012
00512                                                                   EL012
00513 *    MOVE ORIGTRMI               TO CP-ORIGINAL-TERM                 CL**7
00514                                                                   EL012
00515      IF CP-RETURN-CODE NOT = ZERO                                    CL**7
00516          GO TO 2000-FORMAT-RESULTS.                                  CL**7
00517                                                                   EL012
00518      IF ORIGPRML  = ZEROS  AND                                       CL**7
00519         REFMETHL  = ZEROS  AND                                       CL**7
00520         BENTYPEL  = ZEROS  AND                                       CL**7
00521         EMI-ERROR = ZEROS                                            CL**7
00522          GO TO 2000-FORMAT-RESULTS.                                  CL**7
00523                                                                      CL**7
00524      MOVE CP-REMAINING-TERM-1    TO CP-REMAINING-TERM.               CL*10
00525                                                                      CL*10
00526      MOVE LINK-ELRAMT            TO PGM-NAME.                        CL**7
00527      PERFORM 9700-LINK THRU 9700-EXIT.                               CL**7
00528                                                                      CL**7
00529      IF CP-RETURN-CODE NOT = ZERO                                    CL**7
00530          GO TO 2000-FORMAT-RESULTS.                                  CL**7
00531                                                                      CL**7
00532      MOVE CP-REMAINING-AMT       TO CP-REMAINING-BENEFIT.            CL**7
00533                                                                      CL**7
00534      MOVE CP-REMAINING-TERM-1    TO CP-REMAINING-TERM.               CL**7
00535                                                                      CL*10
00536      MOVE LINK-ELRFND            TO PGM-NAME                      EL012
00537      PERFORM 9700-LINK THRU 9700-EXIT.                            EL012
00538      EJECT                                                        EL012
00539  2000-FORMAT-RESULTS.                                             EL012
00540      MOVE CP-REMAINING-TERM-1    TO REMTRM1O.                     EL012
00541      MOVE CP-REMAINING-TERM-2    TO REMTRM2O.                     EL012
00542      MOVE CP-REMAINING-TERM-3    TO REMTRM3O.                     EL012
00543      MOVE CP-CALC-REFUND         TO REFAMTO.                      EL012
00544      MOVE CP-RETURN-CODE         TO ERRCODEO.                     EL012
00545                                                                   EL012
00546      EJECT                                                        EL012
00547  8100-SEND-INITIAL-MAP.                                           EL012
00548      MOVE SAVE-DATE              TO RUNDATEO.                     EL012
00549      MOVE EIBTIME                TO TIME-IN.                      EL012
00550      MOVE TIME-OUT               TO RUNTIMEO.                     EL012
00551      MOVE -1                     TO CERTISSL.                     EL012
00552      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL012
00553                                                                   EL012
00554      EXEC CICS SEND                                               EL012
00555          MAP    (MAP-NAME)                                        EL012
00556          MAPSET (MAPSET-NAME)                                     EL012
00557          FROM   (EL012AO)                                         EL012
00558          ERASE                                                    EL012
00559          CURSOR                                                   EL012
00560      END-EXEC.                                                    EL012
00561                                                                   EL012
00562      GO TO 9100-RETURN-TRAN.                                      EL012
00563                                                                   EL012
00564  8200-SEND-DATAONLY.                                              EL012
00565      MOVE SAVE-DATE              TO RUNDATEO.                     EL012
00566      MOVE EIBTIME                TO TIME-IN.                      EL012
00567      MOVE TIME-OUT               TO RUNTIMEO.                     EL012
00568      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL012
00569                                                                   EL012
00570      EXEC CICS SEND                                               EL012
00571          MAP    (MAP-NAME)                                        EL012
00572          MAPSET (MAPSET-NAME)                                     EL012
00573          FROM   (EL012AO)                                         EL012
00574          DATAONLY                                                 EL012
00575          CURSOR                                                   EL012
00576      END-EXEC.                                                    EL012
00577                                                                   EL012
00578      MOVE LOW-VALUES             TO EL012AO.                      EL012
00579                                                                   EL012
00580      GO TO 9100-RETURN-TRAN.                                      EL012
00581      EJECT                                                        EL012
00582  8500-DATE-CONVERT.                                               EL012
00583      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL012
00584                                                                   EL012
00585      EXEC CICS LINK                                               EL012
00586          PROGRAM    (PGM-NAME)                                    EL012
00587          COMMAREA   (DATE-CONVERSION-DATA)                        EL012
00588          LENGTH     (DC-COMM-LENGTH)                              EL012
00589      END-EXEC.                                                    EL012
00590                                                                   EL012
00591  8500-EXIT.                                                       EL012
00592      EXIT.                                                        EL012
00593                                                                   EL012
00594      EJECT                                                        EL012
00595  8950-CLEAR-RETURN.                                               EL012
00596      MOVE TRAN-COMPLETE-MSG      TO TEXT-AREA.                    EL012
00597                                                                   EL012
00598  8990-SEND-TEXT.                                                  EL012
00599      EXEC CICS SEND TEXT                                          EL012
00600          FROM    (TEXT-AREA)                                      EL012
00601          LENGTH  (TEXT-LENGTH)                                    EL012
00602          ERASE                                                    EL012
00603          FREEKB                                                   EL012
00604      END-EXEC.                                                    EL012
00605                                                                   EL012
00606  9000-RETURN-CICS.                                                EL012
00607      EXEC CICS RETURN                                             EL012
00608      END-EXEC.                                                    EL012
00609                                                                   EL012
00610  9100-RETURN-TRAN.                                                EL012
00611      EXEC CICS RETURN                                             EL012
00612          TRANSID   (TRANS-ID)                                     EL012
00613          COMMAREA  (CALCULATION-PASS-AREA)                        EL012
00614          LENGTH    (CP-COMM-LENGTH)                               EL012
00615      END-EXEC.                                                    EL012
00616                                                                   EL012
00617  9700-LINK.                                                       EL012
00618      EXEC CICS LINK                                               EL012
00619          PROGRAM   (PGM-NAME)                                     EL012
00620          COMMAREA  (CALCULATION-PASS-AREA)                        EL012
00621          LENGTH    (CP-COMM-LENGTH)                               EL012
00622      END-EXEC.                                                    EL012
00623                                                                   EL012
00624  9700-EXIT.                                                       EL012
00625       EXIT.                                                       EL012
00626                                                                   EL012
00627      EJECT                                                        EL012
00628  9900-ERROR-FORMAT.                                               EL012
00629      IF NOT EMI-ERRORS-COMPLETE                                   EL012
00630          MOVE LINK-001           TO PGM-NAME                      EL012
00631          EXEC CICS LINK                                           EL012
00632              PROGRAM   (PGM-NAME)                                 EL012
00633              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL012
00634              LENGTH    (EMI-COMM-LENGTH)                          EL012
00635          END-EXEC.                                                EL012
00636                                                                   EL012
00637  9900-EXIT.                                                       EL012
00638      EXIT.                                                        EL012
00639                                                                   EL012
00640  9990-ABEND.                                                      EL012
00641      MOVE LINK-004               TO PGM-NAME.                     EL012
00642      MOVE DFHEIBLK               TO EMI-LINE1.                    EL012
00643                                                                   EL012
00644      EXEC CICS LINK                                               EL012
00645          PROGRAM   (PGM-NAME)                                     EL012
00646          COMMAREA  (EMI-LINE1)                                    EL012
00647          LENGTH    (72)                                           EL012
00648      END-EXEC.                                                    EL012
00649                                                                   EL012
00650      GO TO 8200-SEND-DATAONLY.                                    EL012
00651      GOBACK.                                                      EL012
