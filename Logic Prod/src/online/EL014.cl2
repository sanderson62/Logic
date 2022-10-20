00001  IDENTIFICATION DIVISION.                                         11/04/97
00002                                                                   EL014
00003  PROGRAM-ID.                 EL014 .                                 LV007
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/12/96 09:30:08.                    CL**5
00007 *                            VMOD=2.007.                             CL**7
00008 *                                                                 EL014
00008 *                                                                 EL014
00009 *AUTHOR.     LOGIC,INC.                                              CL**5
00010 *            DALLAS, TEXAS.                                          CL**5
00011                                                                   EL014
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
00022                                                                   EL014
00023 *REMARKS.    TRANSACTION - EXRA - REMAINING AMOUNT CALCULATION       CL**4
00024 *        THIS PROGRAM IS USED TO REVIEW THE REMAINING AMOUNT         CL**4
00025 *        CALCULATIONS DONE BY THE SUBROUTINE ELRAMT.                 CL**4
00026                                                                   EL014
00027      EJECT                                                        EL014
00028  ENVIRONMENT DIVISION.                                            EL014
00029  DATA DIVISION.                                                   EL014
00030  WORKING-STORAGE SECTION.                                         EL014
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL014
00032  77  FILLER  PIC X(32)  VALUE '*    EL014 WORKING STORAGE     *'. EL014
00033  77  FILLER  PIC X(32)  VALUE '************VMOD=2.007 *********'.    CL**7
00034                                                                   EL014
00035  01  WS-DATE-AREA.                                                EL014
00036      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL014
00037      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL014
00038                                                                   EL014
00039  01  STANDARD-AREAS.                                              EL014
00040      12  MAP-NAME                PIC X(8)    VALUE 'EL014A'.      EL014
00041      12  MAPSET-NAME             PIC X(8)    VALUE 'EL014S'.      EL014
00042      12  TRANS-ID                PIC X(4)    VALUE 'EXRA'.        EL014
00043      12  PGM-NAME                PIC X(8)    VALUE SPACES.        EL014
00044      12  ELCNTL-ID               PIC X(8)    VALUE 'ELCNTL'.         CL**6
00045      12  LINK-ELRAMT             PIC X(8)    VALUE 'ELRAMT'.      EL014
00046      12  LINK-ELRTRM             PIC X(8)    VALUE 'ELRTRM'.      EL014
00047      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL014
00048      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL014
00049      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL014
00050      12  TIME-IN                 PIC S9(7).                       EL014
00051      12  TIME-OUT-R   REDEFINES TIME-IN.                          EL014
00052          16  FILLER              PIC X.                           EL014
00053          16  TIME-OUT            PIC 99V99.                       EL014
00054          16  FILLER              PIC X(2).                        EL014
00055                                                                   EL014
00056  01  MISC-WORK-AREAS.                                             EL014
00057      12  WS-COMPANY-CODE         PIC S9(4)   VALUE +0    COMP.    EL014
00058      12  WS-COMPANY-CODE-R          REDEFINES WS-COMPANY-CODE.    EL014
00059          16  FILLER              PIC X.                           EL014
00060          16  WS-COMP-CD          PIC X.                           EL014
00061      12  TEXT-AREA               PIC X(66).                       EL014
00062      12  TEXT-LENGTH             PIC S9(4)   VALUE +66   COMP.    EL014
00063                                                                   EL014
00064      12  ELCNTL-KEY.                                                 CL**6
00065          16  CNTL-COMPANY-CD     PIC X(3).                           CL**6
00066          16  CNTL-REC-TYPE       PIC X(1).                           CL**6
00067          16  CNTL-ACCESS         PIC X(4).                           CL**6
00068          16  CNTL-SEQ-NO         PIC S9(4)  COMP.                    CL**6
00069                                                                      CL**6
00070      12  ER-0071                 PIC X(4)   VALUE '0071'.            CL**3
00071      12  ER-0258                 PIC X(4)   VALUE '0258'.         EL014
00072      12  ER-2473                 PIC X(4)   VALUE '2473'.         EL014
00073      12  ER-2474                 PIC X(4)   VALUE '2474'.         EL014
00074      12  ER-2475                 PIC X(4)   VALUE '2475'.         EL014
00075      12  ER-2476                 PIC X(4)   VALUE '2476'.         EL014
00076      12  ER-2477                 PIC X(4)   VALUE '2477'.         EL014
00077      12  ER-2481                 PIC X(4)   VALUE '2481'.         EL014
00078      12  ER-2482                 PIC X(4)   VALUE '2482'.         EL014
00079      12  ER-2483                 PIC X(4)   VALUE '2483'.         EL014
00080      12  ER-2485                 PIC X(4)   VALUE '2485'.         EL014
00081      12  ER-2486                 PIC X(4)   VALUE '2486'.         EL014
00082      12  ER-2487                 PIC X(4)   VALUE '2487'.         EL014
00083      12  ER-2500                 PIC X(4)   VALUE '2500'.         EL014
00084      12  ER-2617                 PIC X(4)   VALUE '2617'.            CL**6
00085      12  ER-2848                 PIC X(4)   VALUE '2848'.            CL**6
00086                                                                   EL014
00087  01  TEXT-MESSAGES.                                               EL014
00088      12  TRAN-COMPLETE-MSG.                                       EL014
00089          16  FILLER              PIC X(45)                        EL014
00090            VALUE '     CLEAR ENTERED - SESSION ENDED'.            EL014
00091      EJECT                                                        EL014
00092                                  COPY ELCDATE.                       CL**4
00093      EJECT                                                        EL014
00094                                  COPY ELCATTR.                       CL**4
00095      EJECT                                                        EL014
00096                                  COPY ELCEMIB.                       CL**4
00097      EJECT                                                        EL014
00098                                  COPY ELCCALC.                       CL**4
00099      EJECT                                                        EL014
00100                                  COPY ELCAID.                        CL**4
00101      EJECT                                                        EL014
00102                                  COPY EL014S.                        CL**4
00103      EJECT                                                        EL014
00104  LINKAGE SECTION.                                                 EL014
00105  01  DFHCOMMAREA                 PIC X(450).                         CL**7
00106      EJECT                                                        EL014
00107                                  COPY ELCCNTL.                       CL**6
00108  PROCEDURE DIVISION.                                              EL014
00109      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL014
00110      MOVE '5'                    TO DC-OPTION-CODE.               EL014
00111      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL014
00112      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL014
00113      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL014
00114                                                                   EL014
00115      IF EIBAID = DFHCLEAR                                         EL014
00116          GO TO 8950-CLEAR-RETURN.                                 EL014
00117                                                                   EL014
00118      MOVE LOW-VALUES             TO EL014AO.                      EL014
00119                                                                   EL014
00120      IF EIBCALEN NOT GREATER THAN ZERO                            EL014
00121          GO TO 8100-SEND-INITIAL-MAP.                             EL014
00122                                                                   EL014
00123      MOVE DFHCOMMAREA            TO CALCULATION-PASS-AREA.        EL014
00124                                                                   EL014
00125      EXEC CICS RECEIVE                                            EL014
00126          MAP     (MAP-NAME)                                       EL014
00127          MAPSET  (MAPSET-NAME)                                    EL014
00128          INTO    (EL014AI)                                        EL014
00129      END-EXEC.                                                    EL014
00130                                                                   EL014
00131      EJECT                                                        EL014
00132  1000-MOVE-DATA-TO-PASS-AREA.                                     EL014
00133      IF ISSDATEL NOT = ZEROS                                      EL014
00134          IF ISSDATEI NUMERIC                                      EL014
00135              MOVE ISSDATEI       TO DC-GREG-DATE-1-MDY            EL014
00136              MOVE 4              TO DC-OPTION-CODE                EL014
00137              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT             EL014
00138              IF NO-CONVERSION-ERROR                               EL014
00139                  MOVE DC-BIN-DATE-1  TO CP-CERT-EFF-DT            EL014
00140                  MOVE AL-UNNON   TO ISSDATEA                      EL014
00141              ELSE                                                 EL014
00142                  MOVE ER-2474    TO EMI-ERROR                     EL014
00143                  MOVE -1         TO ISSDATEL                      EL014
00144                  MOVE AL-UNBON   TO ISSDATEA                      EL014
00145                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL014
00146          ELSE                                                     EL014
00147              MOVE ER-2482        TO EMI-ERROR                     EL014
00148              MOVE -1             TO ISSDATEL                      EL014
00149              MOVE AL-UNBON       TO ISSDATEA                      EL014
00150              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL014
00151      ELSE                                                         EL014
00152          MOVE ER-2473            TO EMI-ERROR                     EL014
00153          MOVE -1                 TO ISSDATEL                      EL014
00154          MOVE AL-UNBON           TO ISSDATEA                      EL014
00155          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL014
00156                                                                   EL014
00157      IF VALDATEL NOT = ZEROS                                      EL014
00158          IF VALDATEI NUMERIC                                      EL014
00159              MOVE VALDATEI       TO DC-GREG-DATE-1-MDY            EL014
00160              MOVE 4              TO DC-OPTION-CODE                EL014
00161              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT             EL014
00162              IF NO-CONVERSION-ERROR                               EL014
00163                  MOVE DC-BIN-DATE-1  TO CP-VALUATION-DT           EL014
00164                  MOVE AL-UNNON   TO VALDATEA                      EL014
00165              ELSE                                                 EL014
00166                  MOVE ER-2476    TO EMI-ERROR                     EL014
00167                  MOVE -1         TO VALDATEL                      EL014
00168                  MOVE AL-UNBON   TO VALDATEA                      EL014
00169                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL014
00170          ELSE                                                     EL014
00171              MOVE ER-2483        TO EMI-ERROR                     EL014
00172              MOVE -1             TO VALDATEL                      EL014
00173              MOVE AL-UNBON       TO VALDATEA                      EL014
00174              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL014
00175      ELSE                                                         EL014
00176          MOVE ER-2500            TO EMI-ERROR                     EL014
00177          MOVE -1                 TO VALDATEL                      EL014
00178          MOVE AL-UNBON           TO VALDATEA                      EL014
00179          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL**4
00180                                                                      CL**4
00181      IF FSTPMTL NOT = ZEROS                                          CL**4
00182          IF FSTPMTI NUMERIC                                          CL**4
00183              MOVE FSTPMTI        TO DC-GREG-DATE-1-MDY               CL**4
00184              MOVE 4              TO DC-OPTION-CODE                   CL**4
00185              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                CL**4
00186              IF NO-CONVERSION-ERROR                                  CL**4
00187                  MOVE DC-BIN-DATE-1  TO CP-FIRST-PAY-DATE            CL**4
00188                  MOVE AL-UNNON   TO FSTPMTA                          CL**4
00189              ELSE                                                    CL**4
00190                  MOVE ER-2476    TO EMI-ERROR                        CL**4
00191                  MOVE -1         TO FSTPMTL                          CL**4
00192                  MOVE AL-UNBON   TO FSTPMTA                          CL**4
00193                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**4
00194          ELSE                                                        CL**4
00195              MOVE ER-2483        TO EMI-ERROR                        CL**4
00196              MOVE -1             TO FSTPMTL                          CL**4
00197              MOVE AL-UNBON       TO FSTPMTA                          CL**4
00198              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00199      ELSE                                                            CL**4
00200          MOVE ER-2500            TO EMI-ERROR                        CL**4
00201          MOVE -1                 TO FSTPMTL                          CL**4
00202          MOVE AL-UNBON           TO FSTPMTA                          CL**4
00203          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL014
00204                                                                   EL014
00205      IF ORIGBENL NOT = ZEROS                                      EL014
00206          IF ORIGBENI NUMERIC                                      EL014
00207              MOVE ORIGBENI       TO CP-ORIGINAL-BENEFIT           EL014
00208              MOVE AL-UNNON       TO ORIGBENA                      EL014
00209          ELSE                                                     EL014
00210              MOVE ER-2485        TO EMI-ERROR                     EL014
00211              MOVE -1             TO ORIGBENL                      EL014
00212              MOVE AL-UNBON       TO ORIGBENA                      EL014
00213              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL014
00214                                                                   EL014
00215      IF ORIGTRML NOT = ZEROS                                      EL014
00216          MOVE ORIGTRMI           TO CP-ORIGINAL-TERM              EL014
00217          MOVE AL-UANON           TO ORIGTRMA                      EL014
00218      ELSE                                                         EL014
00219          MOVE ER-2477            TO EMI-ERROR                     EL014
00220          MOVE -1                 TO ORIGTRML                      EL014
00221          MOVE AL-UABON           TO ORIGTRMA                      EL014
00222          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL014
00223                                                                   EL014
00224      IF CALMETHL NOT = ZEROS                                      EL014
00225          MOVE CALMETHI           TO CP-EARNING-METHOD             EL014
00226          MOVE AL-UNNON           TO CALMETHA.                     EL014
00227                                                                   EL014
00228      IF BENTYPEI = 'L' OR 'A' OR 'R'                              EL014
00229          MOVE BENTYPEI           TO CP-BENEFIT-TYPE               EL014
00230          MOVE AL-UANON           TO BENTYPEA                      EL014
00231      ELSE                                                         EL014
00232          MOVE ER-2481            TO EMI-ERROR                     EL014
00233          MOVE -1                 TO BENTYPEL                      EL014
00234          MOVE AL-UABON           TO BENTYPEA                      EL014
00235          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL**3
00236                                                                      CL**3
00237      IF CALCOPTI = '1' OR '2' OR '3' OR '4' OR ' '                   CL**3
00238          MOVE CALCOPTI           TO CP-REM-TRM-CALC-OPTION           CL**3
00239          MOVE AL-UANON           TO CALCOPTA                         CL**3
00240      ELSE                                                            CL**3
00241          MOVE ER-0071            TO EMI-ERROR                        CL**3
00242          MOVE -1                 TO CALCOPTL                         CL**3
00243          MOVE AL-UABON           TO CALCOPTA                         CL**3
00244          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL014
00245                                                                   EL014
00246      IF COMPIDL NOT = ZEROS                                       EL014
00247          MOVE COMPIDI            TO CP-COMPANY-ID                 EL014
00248          MOVE AL-UANON           TO COMPIDA.                      EL014
00249                                                                   EL014
00250      IF PAYFREQL NOT = ZEROS                                      EL014
00251          IF PAYFREQI NUMERIC                                      EL014
00252              MOVE PAYFREQI           TO CP-PAY-FREQUENCY          EL014
00253              MOVE AL-UANON           TO PAYFREQA                  EL014
00254          ELSE                                                     EL014
00255              MOVE ER-0258        TO EMI-ERROR                     EL014
00256              MOVE -1             TO PAYFREQL                      EL014
00257              MOVE AL-UNBON       TO PAYFREQA                      EL014
00258              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL014
00259                                                                   EL014
00260      IF APRL NOT = ZEROS                                          EL014
00261          IF APRI NUMERIC                                          EL014
00262              MOVE APRI           TO CP-LOAN-APR                   EL014
00263              MOVE AL-UNNON       TO APRA                          EL014
00264          ELSE                                                     EL014
00265              MOVE ER-2487        TO EMI-ERROR                     EL014
00266              MOVE -1             TO APRL                          EL014
00267              MOVE AL-UNBON       TO APRA                          EL014
00268              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL014
00269                                                                   EL014
00270      IF STCODEL NOT = ZEROS                                       EL014
00271          MOVE STCODEI            TO CP-STATE                      EL014
00272          MOVE AL-UNNON           TO STCODEA.                      EL014
00273                                                                   EL014
00274      IF STABBRL NOT = ZEROS                                       EL014
00275          MOVE STABBRI            TO CP-STATE-STD-ABBRV            EL014
00276          MOVE AL-UANON           TO STABBRA.                      EL014
00277                                                                   EL014
00278      IF CALCODEL NOT = ZEROS                                      EL014
00279          MOVE CALCODEI           TO CP-SPECIAL-CALC-CD            EL014
00280          MOVE AL-UANON           TO CALCODEA.                     EL014
00281                                                                   EL014
00282      IF CAPTERML NOT = ZEROS                                      EL014
00283          MOVE CAPTERMI           TO CP-LOAN-TERM                  EL014
00284          MOVE AL-UANON           TO CAPTERMA.                     EL014
00285                                                                      CL**6
00286 ** READ STATE MASTER RECORD FOR FREE LOOK PERIOD **                  CL**6
00287                                                                      CL**6
00288      IF STCODEL = ZEROS OR                                           CL**6
00289         STCODEI = SPACE OR                                           CL**6
00290         COMPIDL = ZEROS OR                                           CL**6
00291         COMPIDI = SPACE                                              CL**6
00292         MOVE ZERO                TO CP-FREE-LOOK                     CL**6
00293         GO TO 1030-MOVE-CONTINUE.                                    CL**6
00294                                                                      CL**6
00295      MOVE SPACES                 TO ELCNTL-KEY.                      CL**6
00296      MOVE COMPIDI                TO CNTL-COMPANY-CD.                 CL**6
00297      MOVE '3'                    TO CNTL-REC-TYPE.                   CL**6
00298      MOVE STCODEI                TO CNTL-ACCESS.                     CL**6
00299      MOVE +0                     TO CNTL-SEQ-NO.                     CL**6
00300                                                                      CL**6
00301      EXEC CICS HANDLE CONDITION                                      CL**6
00302          NOTFND   (1010-CNTL-NOTFND)                                 CL**6
00303          NOTOPEN  (1020-CNTL-NOTOPEN)                                CL**6
00304      END-EXEC.                                                       CL**6
00305                                                                      CL**6
00306      EXEC CICS READ                                                  CL**6
00307          DATASET  (ELCNTL-ID)                                        CL**6
00308          SET      (ADDRESS OF CONTROL-FILE)                          CL**6
00309          RIDFLD   (ELCNTL-KEY)                                       CL**6
00310      END-EXEC.                                                       CL**6
00311                                                                      CL**6
00312      MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.                    CL**6
00313      GO TO 1030-MOVE-CONTINUE.                                       CL**6
00314                                                                      CL**6
00315  1010-CNTL-NOTFND.                                                   CL**6
00316      MOVE ER-2848                TO EMI-ERROR.                       CL**6
00317      MOVE -1                     TO COMPIDL.                         CL**6
00318      MOVE AL-UNBON               TO COMPIDA.                         CL**6
00319      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**6
00320      GO TO 1030-MOVE-CONTINUE.                                       CL**6
00321                                                                      CL**6
00322  1020-CNTL-NOTOPEN.                                                  CL**6
00323      MOVE ER-2617                TO EMI-ERROR.                       CL**6
00324      MOVE -1                     TO COMPIDL.                         CL**6
00325      MOVE AL-UNBON               TO COMPIDA.                         CL**6
00326      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**6
00327                                                                      CL**6
00328  1030-MOVE-CONTINUE.                                                 CL**6
00329                                                                   EL014
00330      IF EMI-ERROR NOT = ZEROS                                     EL014
00331          GO TO 8200-SEND-DATAONLY.                                EL014
00332                                                                   EL014
00333      MOVE LINK-ELRTRM            TO PGM-NAME                      EL014
00334      PERFORM 9700-LINK THRU 9700-EXIT.                            EL014
00335                                                                   EL014
00336      MOVE CP-REMAINING-TERM-2    TO CP-REMAINING-TERM.            EL014
00337                                                                   EL014
00338      MOVE LINK-ELRAMT            TO PGM-NAME                      EL014
00339      PERFORM 9700-LINK THRU 9700-EXIT.                            EL014
00340      EJECT                                                        EL014
00341  2000-FORMAT-RESULTS.                                             EL014
00342      IF EMI-ERROR = ZEROS                                         EL014
00343          MOVE CP-REMAINING-AMT       TO REMAMTO                   EL014
00344          MOVE CP-REMAINING-TERM-2    TO REMTERMO                  EL014
00345      ELSE                                                         EL014
00346          MOVE CP-RETURN-CODE     TO ERRCODEO.                     EL014
00347                                                                   EL014
00348      EJECT                                                        EL014
00349  8100-SEND-INITIAL-MAP.                                           EL014
00350      MOVE SAVE-DATE              TO RUNDATEO.                     EL014
00351      MOVE EIBTIME                TO TIME-IN.                      EL014
00352      MOVE TIME-OUT               TO RUNTIMEO.                     EL014
00353      MOVE -1                     TO ISSDATEL.                     EL014
00354      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL014
00355                                                                   EL014
00356      EXEC CICS SEND                                               EL014
00357          MAP    (MAP-NAME)                                        EL014
00358          MAPSET (MAPSET-NAME)                                     EL014
00359          FROM   (EL014AO)                                         EL014
00360          ERASE                                                    EL014
00361          CURSOR                                                   EL014
00362      END-EXEC.                                                    EL014
00363                                                                   EL014
00364      GO TO 9100-RETURN-TRAN.                                      EL014
00365                                                                   EL014
00366  8200-SEND-DATAONLY.                                              EL014
00367      MOVE SAVE-DATE              TO RUNDATEO.                     EL014
00368      MOVE EIBTIME                TO TIME-IN.                      EL014
00369      MOVE TIME-OUT               TO RUNTIMEO.                     EL014
00370      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL014
00371                                                                   EL014
00372      EXEC CICS SEND                                               EL014
00373          MAP    (MAP-NAME)                                        EL014
00374          MAPSET (MAPSET-NAME)                                     EL014
00375          FROM   (EL014AO)                                         EL014
00376          DATAONLY                                                 EL014
00377          CURSOR                                                   EL014
00378      END-EXEC.                                                    EL014
00379                                                                   EL014
00380      MOVE LOW-VALUES             TO EL014AO.                      EL014
00381                                                                   EL014
00382      GO TO 9100-RETURN-TRAN.                                      EL014
00383      EJECT                                                        EL014
00384  8500-DATE-CONVERT.                                               EL014
00385      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL014
00386                                                                   EL014
00387      EXEC CICS LINK                                               EL014
00388          PROGRAM    (PGM-NAME)                                    EL014
00389          COMMAREA   (DATE-CONVERSION-DATA)                        EL014
00390          LENGTH     (DC-COMM-LENGTH)                              EL014
00391      END-EXEC.                                                    EL014
00392                                                                   EL014
00393  8500-EXIT.                                                       EL014
00394      EXIT.                                                        EL014
00395                                                                   EL014
00396      EJECT                                                        EL014
00397  8950-CLEAR-RETURN.                                               EL014
00398      MOVE TRAN-COMPLETE-MSG      TO TEXT-AREA.                    EL014
00399                                                                   EL014
00400  8990-SEND-TEXT.                                                  EL014
00401      EXEC CICS SEND TEXT                                          EL014
00402          FROM    (TEXT-AREA)                                      EL014
00403          LENGTH  (TEXT-LENGTH)                                    EL014
00404          ERASE                                                    EL014
00405          FREEKB                                                   EL014
00406      END-EXEC.                                                    EL014
00407                                                                   EL014
00408  9000-RETURN-CICS.                                                EL014
00409      EXEC CICS RETURN                                             EL014
00410      END-EXEC.                                                    EL014
00411                                                                   EL014
00412  9100-RETURN-TRAN.                                                EL014
00413      EXEC CICS RETURN                                             EL014
00414          TRANSID   (TRANS-ID)                                     EL014
00415          COMMAREA  (CALCULATION-PASS-AREA)                        EL014
00416          LENGTH    (CP-COMM-LENGTH)                               EL014
00417      END-EXEC.                                                    EL014
00418                                                                   EL014
00419      EJECT                                                        EL014
00420  9700-LINK.                                                       EL014
00421      EXEC CICS LINK                                               EL014
00422          PROGRAM   (PGM-NAME)                                     EL014
00423          COMMAREA  (CALCULATION-PASS-AREA)                        EL014
00424          LENGTH    (CP-COMM-LENGTH)                               EL014
00425      END-EXEC.                                                    EL014
00426                                                                   EL014
00427  9700-EXIT.                                                       EL014
00428       EXIT.                                                       EL014
00429                                                                   EL014
00430  9900-ERROR-FORMAT.                                               EL014
00431      IF NOT EMI-ERRORS-COMPLETE                                   EL014
00432          MOVE LINK-001 TO PGM-NAME                                EL014
00433          EXEC CICS LINK                                           EL014
00434              PROGRAM   (PGM-NAME)                                 EL014
00435              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL014
00436              LENGTH    (EMI-COMM-LENGTH)                          EL014
00437          END-EXEC.                                                EL014
00438                                                                   EL014
00439  9900-EXIT.                                                       EL014
00440      EXIT.                                                        EL014
00441                                                                   EL014
00442  9990-ABEND.                                                      EL014
00443      MOVE LINK-004               TO PGM-NAME.                     EL014
00444      MOVE DFHEIBLK               TO EMI-LINE1.                    EL014
00445                                                                   EL014
00446      EXEC CICS LINK                                               EL014
00447          PROGRAM   (PGM-NAME)                                     EL014
00448          COMMAREA  (EMI-LINE1)                                    EL014
00449          LENGTH    (72)                                           EL014
00450      END-EXEC.                                                    EL014
00451                                                                   EL014
00452      GO TO 8200-SEND-DATAONLY.                                    EL014
00453                                                                   EL014
00454      GOBACK.                                                      EL014
