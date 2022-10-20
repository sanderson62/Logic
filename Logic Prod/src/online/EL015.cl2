00001  IDENTIFICATION DIVISION.                                         11/04/97
00002                                                                   EL015
00003  PROGRAM-ID.                 EL015 .                                 LV006
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/12/96 09:32:22.                    CL**5
00007 *                            VMOD=2.006                              CL**6
00008 *                                                                 EL015
00008 *                                                                 EL015
00009 *AUTHOR.     LOGIC,INC.                                              CL**5
00010 *            DALLAS, TEXAS.                                          CL**5
00011                                                                   EL015
00012 *DATE-COMPILED.                                                      CL**5
00013 *SECURITY.  *****************************************************    CL**5
00014 *           *                                                   *    CL**5
00015 *           * THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.       *    CL**5
00016 *           *                                                   *    CL**5
00017 *           * USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES   *    CL**5
00018 *           * OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT    *    CL**5
00019 *           * THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.       *    CL**5
00020 *           *                                                   *    CL**5
00021 *           *****************************************************    CL**5
00022                                                                   EL015
00023 *REMARKS.    TRANSACTION - EXLR - LOSS RESERVE CALCULATIONS.         CL**5
00024                                                                   EL015
00025                                                                   EL015
00026      EJECT                                                        EL015
00027  ENVIRONMENT DIVISION.                                            EL015
00028  DATA DIVISION.                                                   EL015
00029  WORKING-STORAGE SECTION.                                         EL015
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL015
00031  77  FILLER  PIC X(32)  VALUE '*    EL015 WORKING STORAGE     *'. EL015
00032  77  FILLER  PIC X(32)  VALUE '***********VMOD=2.006 **********'.    CL**6
00033                                                                   EL015
00034  01  WS-DATE-AREA.                                                EL015
00035      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL015
00036      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL015
00037                                                                   EL015
00038                                  COPY ELCRESW1.                      CL**4
00039                                                                   EL015
00040      EJECT                                                        EL015
00041                                  COPY ELCCALC.                       CL**4
00042      EJECT                                                        EL015
00043  01  STANDARD-AREAS.                                              EL015
00044      12  MAP-NAME                PIC X(8)    VALUE 'EL015A'.      EL015
00045      12  MAPSET-NAME             PIC X(8)    VALUE 'EL015S'.      EL015
00046      12  PGM-NAME                PIC X(8)    VALUE SPACES.        EL015
00047      12  LINK-ELRESV             PIC X(8)    VALUE 'ELRESV'.      EL015
00048      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL015
00049      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL015
00050      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL015
00051      12  TIME-IN                 PIC S9(7).                       EL015
00052      12  TIME-OUT-R   REDEFINES TIME-IN.                          EL015
00053          16  FILLER              PIC X.                           EL015
00054          16  TIME-OUT            PIC 99V99.                       EL015
00055          16  FILLER              PIC X(2).                        EL015
00056                                                                   EL015
00057  01  MISC-WORK-AREAS.                                             EL015
00058      12  WS-COMPANY-CODE         PIC S9(4)   VALUE +0    COMP.    EL015
00059      12  WS-COMPANY-CODE-R          REDEFINES WS-COMPANY-CODE.    EL015
00060          16  FILLER              PIC X.                           EL015
00061          16  WS-COMP-CD          PIC X.                           EL015
00062      12  TEXT-AREA               PIC X(66).                       EL015
00063      12  TEXT-LENGTH             PIC S9(4)   VALUE +66   COMP.    EL015
00064                                                                   EL015
00065      12  ER-2473                 PIC X(4)   VALUE '2473'.         EL015
00066      12  ER-2474                 PIC X(4)   VALUE '2474'.         EL015
00067      12  ER-2482                 PIC X(4)   VALUE '2482'.         EL015
00068      12  ER-2491                 PIC X(4)   VALUE '2491'.         EL015
00069      12  ER-2492                 PIC X(4)   VALUE '2492'.         EL015
00070      12  ER-2493                 PIC X(4)   VALUE '2493'.         EL015
00071      12  ER-2494                 PIC X(4)   VALUE '2494'.         EL015
00072      12  ER-2495                 PIC X(4)   VALUE '2495'.         EL015
00073      12  ER-2497                 PIC X(4)   VALUE '2497'.         EL015
00074      12  ER-2498                 PIC X(4)   VALUE '2498'.         EL015
00075      12  ER-2499                 PIC X(4)   VALUE '2499'.         EL015
00076      12  ER-2500                 PIC X(4)   VALUE '2500'.         EL015
00077      12  ER-2501                 PIC X(4)   VALUE '2501'.         EL015
00078      12  ER-2502                 PIC X(4)   VALUE '2502'.         EL015
00079      12  ER-2504                 PIC X(4)   VALUE '2504'.         EL015
00080      12  ER-2505                 PIC X(4)   VALUE '2505'.         EL015
00081      12  ER-2511                 PIC X(4)   VALUE '2511'.         EL015
00082      12  ER-2512                 PIC X(4)   VALUE '2512'.         EL015
00083      12  ER-2513                 PIC X(4)   VALUE '2513'.         EL015
00084      12  ER-2514                 PIC X(4)   VALUE '2514'.         EL015
00085                                                                   EL015
00086  01  TEXT-MESSAGES.                                               EL015
00087      12  TRAN-COMPLETE-MSG.                                       EL015
00088          16  FILLER              PIC X(45)                        EL015
00089            VALUE '     CLEAR ENTERED - SESSION ENDED'.            EL015
00090                                                                   EL015
00091                                  COPY ELCDATE SUPPRESS.              CL**4
00092                                                                   EL015
00093                                  COPY ELCATTR SUPPRESS.              CL**4
00094                                                                   EL015
00095                                  COPY ELCEMIB SUPPRESS.              CL**4
00096                                                                   EL015
00097                                  COPY ELCAID SUPPRESS.               CL**4
00098                                                                   EL015
00099                                  COPY EL015S SUPPRESS.               CL**4
00100                                                                   EL015
00101                                  COPY ELC64CDT SUPPRESS.             CL**4
00102                                                                   EL015
00103  LINKAGE SECTION.                                                 EL015
00104  01  DFHCOMMAREA                 PIC X(450).                         CL**6
00105                                                                   EL015
00106      EJECT                                                        EL015
00107  PROCEDURE DIVISION.                                              EL015
00108      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL015
00109      MOVE '5'                    TO DC-OPTION-CODE.               EL015
00110      PERFORM 8500-DATE-CONVERSION.                                EL015
00111      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL015
00112      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL015
00113                                                                   EL015
00114      IF EIBAID = DFHCLEAR                                         EL015
00115          GO TO 8950-CLEAR-RETURN.                                 EL015
00116                                                                   EL015
00117      MOVE LOW-VALUES             TO EL015AO.                      EL015
00118                                                                   EL015
00119      IF EIBCALEN NOT GREATER THAN ZERO                            EL015
00120          GO TO 8100-SEND-INITIAL-MAP.                             EL015
00121                                                                   EL015
00122      MOVE DFHCOMMAREA            TO CALCULATION-PASS-AREA.        EL015
00123                                                                   EL015
00124      EXEC CICS RECEIVE                                            EL015
00125          MAP     (MAP-NAME)                                       EL015
00126          MAPSET  (MAPSET-NAME)                                    EL015
00127          INTO    (EL015AI)                                        EL015
00128      END-EXEC.                                                    EL015
00129                                                                   EL015
00130      EJECT                                                        EL015
00131  1000-EDIT-INPUT.                                                 EL015
00132 ******************************ISSUE DATE                          EL015
00133      IF ISSDTL NOT = ZEROS                                        EL015
00134          IF ISSDTI NUMERIC                                        EL015
00135              MOVE ISSDTI         TO DC-GREG-DATE-1-MDY            EL015
00136              MOVE 4              TO DC-OPTION-CODE                EL015
00137              PERFORM 8500-DATE-CONVERSION                         EL015
00138              IF NO-CONVERSION-ERROR                               EL015
00139                  MOVE DC-BIN-DATE-1  TO CP-CERT-EFF-DT            EL015
00140                  MOVE AL-UNNON   TO ISSDTA                        EL015
00141              ELSE                                                 EL015
00142                  MOVE ER-2474    TO EMI-ERROR                     EL015
00143                  MOVE -1         TO ISSDTL                        EL015
00144                  MOVE AL-UNBON   TO ISSDTA                        EL015
00145                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL015
00146          ELSE                                                     EL015
00147              MOVE ER-2482        TO EMI-ERROR                     EL015
00148              MOVE -1             TO ISSDTL                        EL015
00149              MOVE AL-UNBON       TO ISSDTA                        EL015
00150              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL015
00151      ELSE                                                         EL015
00152          MOVE ER-2473            TO EMI-ERROR                     EL015
00153          MOVE -1                 TO ISSDTL                        EL015
00154          MOVE AL-UNBON           TO ISSDTA                        EL015
00155          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL015
00156                                                                   EL015
00157 ******************************REPORTED DATE                       EL015
00158      IF RPTDTL NOT = ZEROS                                        EL015
00159          IF RPTDTI NUMERIC                                        EL015
00160              MOVE RPTDTI         TO DC-GREG-DATE-1-MDY            EL015
00161              MOVE 4              TO DC-OPTION-CODE                EL015
00162              PERFORM 8500-DATE-CONVERSION                         EL015
00163              IF NO-CONVERSION-ERROR                               EL015
00164                  MOVE DC-BIN-DATE-1  TO CP-REPORTED-DT            EL015
00165                  MOVE AL-UNNON   TO RPTDTA                        EL015
00166              ELSE                                                 EL015
00167                  MOVE ER-2511    TO EMI-ERROR                     EL015
00168                  MOVE -1         TO RPTDTL                        EL015
00169                  MOVE AL-UNBON   TO RPTDTA                        EL015
00170                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL015
00171          ELSE                                                     EL015
00172              MOVE ER-2512        TO EMI-ERROR                     EL015
00173              MOVE -1             TO RPTDTL                        EL015
00174              MOVE AL-UNBON       TO RPTDTA                        EL015
00175              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL015
00176      ELSE                                                         EL015
00177          MOVE ER-2513            TO EMI-ERROR                     EL015
00178          MOVE -1                 TO RPTDTL                        EL015
00179          MOVE AL-UNBON           TO RPTDTA                        EL015
00180          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL015
00181                                                                   EL015
00182 ******************************VALUATION DATE                      EL015
00183      IF VALDTL NOT = ZEROS                                        EL015
00184          IF VALDTI NUMERIC                                        EL015
00185              MOVE VALDTI         TO DC-GREG-DATE-1-MDY            EL015
00186              MOVE 4              TO DC-OPTION-CODE                EL015
00187              PERFORM 8500-DATE-CONVERSION                         EL015
00188              IF NO-CONVERSION-ERROR                               EL015
00189                  MOVE DC-BIN-DATE-1  TO CP-VALUATION-DT           EL015
00190                  MOVE AL-UNNON   TO VALDTA                        EL015
00191              ELSE                                                 EL015
00192                  MOVE ER-2497    TO EMI-ERROR                     EL015
00193                  MOVE -1         TO VALDTL                        EL015
00194                  MOVE AL-UNBON   TO VALDTA                        EL015
00195                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL015
00196          ELSE                                                     EL015
00197              MOVE ER-2493        TO EMI-ERROR                     EL015
00198              MOVE -1             TO VALDTL                        EL015
00199              MOVE AL-UNBON       TO VALDTA                        EL015
00200              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL015
00201      ELSE                                                         EL015
00202          MOVE ER-2500            TO EMI-ERROR                     EL015
00203          MOVE -1                 TO VALDTL                        EL015
00204          MOVE AL-UNBON           TO VALDTA                        EL015
00205          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL015
00206                                                                   EL015
00207 ******************************MONTHLY BENEFIT                     EL015
00208      EXEC CICS BIF DEEDIT                                         EL015
00209          FIELD  (MONBENI)                                         EL015
00210          LENGTH (12) END-EXEC.                                       CL**4
00211                                                                   EL015
00212      IF MONBENI NUMERIC                                           EL015
00213          MOVE MONBENI            TO CP-ORIGINAL-BENEFIT           EL015
00214 *                                   MONBENO                       EL015
00215          MOVE AL-UNNON           TO MONBENA                       EL015
00216      ELSE                                                         EL015
00217          MOVE ER-2492            TO EMI-ERROR                     EL015
00218          MOVE -1                 TO MONBENL                       EL015
00219          MOVE AL-UNBON           TO MONBENA                       EL015
00220          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL015
00221                                                                   EL015
00222 ****************************INCURRED DATE                         EL015
00223      IF INCDTL NOT = ZEROS                                        EL015
00224          IF INCDTI NUMERIC                                        EL015
00225              MOVE INCDTI         TO DC-GREG-DATE-1-MDY            EL015
00226              MOVE 4              TO DC-OPTION-CODE                EL015
00227              PERFORM 8500-DATE-CONVERSION                         EL015
00228              IF NO-CONVERSION-ERROR                               EL015
00229                  MOVE DC-BIN-DATE-1  TO CP-INCURRED-DT            EL015
00230                  MOVE AL-UNNON   TO INCDTA                        EL015
00231              ELSE                                                 EL015
00232                  MOVE ER-2499    TO EMI-ERROR                     EL015
00233                  MOVE -1         TO INCDTL                        EL015
00234                  MOVE AL-UNBON   TO INCDTA                        EL015
00235                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL015
00236          ELSE                                                     EL015
00237              MOVE ER-2495        TO EMI-ERROR                     EL015
00238              MOVE -1             TO INCDTL                        EL015
00239              MOVE AL-UNBON       TO INCDTA                        EL015
00240              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL015
00241      ELSE                                                         EL015
00242          MOVE ER-2502            TO EMI-ERROR                     EL015
00243          MOVE -1                 TO INCDTL                        EL015
00244          MOVE AL-UNBON           TO INCDTA                        EL015
00245          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL015
00246                                                                   EL015
00247 *******************************ORIGINAL TERM                      EL015
00248      EXEC CICS BIF DEEDIT                                         EL015
00249          FIELD  (ORIGTRMI)                                        EL015
00250          LENGTH (3) END-EXEC.                                     EL015
00251                                                                   EL015
00252      IF ORIGTRMI NUMERIC                                          EL015
00253          MOVE ORIGTRMI           TO CP-ORIGINAL-TERM              EL015
00254                                     ORIGTRMO                      EL015
00255          MOVE AL-UANON           TO ORIGTRMA                      EL015
00256      ELSE                                                         EL015
00257          MOVE ER-2504            TO EMI-ERROR                     EL015
00258          MOVE -1                 TO ORIGTRML                      EL015
00259          MOVE AL-UABON           TO ORIGTRMA                      EL015
00260          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL015
00261                                                                   EL015
00262 *****************************PAID THRU DATE                       EL015
00263      IF PAIDTL NOT = ZEROS                                        EL015
00264          IF PAIDTI NUMERIC  AND                                      CL**4
00265             PAIDTI NOT = ZEROS                                       CL**4
00266              MOVE PAIDTI         TO DC-GREG-DATE-1-MDY            EL015
00267              MOVE 4              TO DC-OPTION-CODE                EL015
00268              PERFORM 8500-DATE-CONVERSION                         EL015
00269              IF NO-CONVERSION-ERROR                               EL015
00270                  MOVE DC-BIN-DATE-1  TO CP-PAID-THRU-DT           EL015
00271                  MOVE AL-UNNON   TO PAIDTA                        EL015
00272              ELSE                                                 EL015
00273                  MOVE ER-2498    TO EMI-ERROR                     EL015
00274                  MOVE -1         TO PAIDTL                        EL015
00275                  MOVE AL-UNBON   TO PAIDTA                        EL015
00276                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL015
00277          ELSE                                                     EL015
00278              MOVE LOW-VALUES     TO CP-PAID-THRU-DT                  CL**4
00279              MOVE ZEROS          TO PAIDTO                           CL**4
00280              MOVE AL-UNNON       TO PAIDTA                           CL**4
00281      ELSE                                                         EL015
00282          MOVE ER-2501            TO EMI-ERROR                     EL015
00283          MOVE -1                 TO PAIDTL                        EL015
00284          MOVE AL-UNBON           TO PAIDTA                        EL015
00285          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL015
00286                                                                   EL015
00287 *****************************ISSUE AGE                            EL015
00288      IF ISSAGEI NUMERIC                                           EL015
00289          MOVE ISSAGEI            TO CP-ISSUE-AGE                  EL015
00290                                     ISSAGEO                       EL015
00291          MOVE AL-UNNON           TO ISSAGEA                       EL015
00292      ELSE                                                         EL015
00293          MOVE ER-2505            TO EMI-ERROR                     EL015
00294          MOVE -1                 TO ISSAGEL                       EL015
00295          MOVE AL-UNBON           TO ISSAGEA                       EL015
00296          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL015
00297                                                                   EL015
00298 *******************************CDT METHOD                         EL015
00299      IF CDTMETHI = '1' OR '2' OR '3' OR '4'                          CL**3
00300          MOVE CDTMETHI           TO CP-CDT-METHOD                 EL015
00301          MOVE AL-UANON           TO CDTMETHA                      EL015
00302      ELSE                                                         EL015
00303          MOVE ER-2491            TO EMI-ERROR                     EL015
00304          MOVE -1                 TO CDTMETHL                      EL015
00305          MOVE AL-UABON           TO CDTMETHA                      EL015
00306          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL015
00307                                                                   EL015
00308 ******************************CDT PERCENT                         EL015
00309      EXEC CICS BIF DEEDIT                                         EL015
00310          FIELD  (CDTPERI)                                         EL015
00311          LENGTH (6) END-EXEC.                                     EL015
00312                                                                   EL015
00313      IF CDTPERI NUMERIC                                           EL015
00314          MOVE CDTPERI            TO CP-CDT-PERCENT                EL015
00315 *                                   CDTPERO                       EL015
00316          MOVE AL-UANON           TO CDTPERA                       EL015
00317      ELSE                                                         EL015
00318          MOVE ER-2514            TO EMI-ERROR                     EL015
00319          MOVE -1                 TO CDTPERL                       EL015
00320          MOVE AL-UABON           TO CDTPERA                       EL015
00321          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL015
00322                                                                   EL015
00323      IF CLIENTL GREATER THAN ZERO                                 EL015
00324          MOVE CLIENTI            TO  CP-COMPANY-ID.               EL015
00325                                                                   EL015
00326      IF CARRCDEL GREATER THAN ZERO                                   CL**4
00327          MOVE CARRCDEI           TO  CP-CARRIER.                     CL**4
00328                                                                      CL**4
00329      EJECT                                                        EL015
00330  2000-PERFORM-CALCULATION.                                        EL015
00331      IF EMI-ERROR NOT = ZEROS                                     EL015
00332          MOVE SPACES             TO ERRCODEO CDTBLO CDTFACTI      EL015
00333                                     PAYCURRI IBNRI  FUTVALI       EL015
00334          GO TO 8200-SEND-DATAONLY.                                EL015
00335                                                                   EL015
00336      MOVE 'A'                    TO CP-BENEFIT-TYPE.              EL015
00337      MOVE 'O'                    TO CP-CLAIM-STATUS.                 CL**4
00338      MOVE ZERO                   TO CP-RETURN-CODE.                  CL**4
00339                                                                   EL015
00340      PERFORM 9900-CALCULATE-RESERVES.                             EL015
00341                                                                   EL015
00342  3000-DISPLAY-RESULTS.                                            EL015
00343      IF CP-RETURN-CODE = ZERO                                     EL015
               MOVE CP-CDT-PERCENT     TO CDTPERO
               MOVE CP-ORIGINAL-BENEFIT TO MONBENO
00344          MOVE SPACE              TO ERRCODEO                         CL**4
00345          MOVE CP-CDT-TABLE       TO CDTBLO                        EL015
00346          MOVE CP-CDT-FACTOR      TO CDTFACTO                      EL015
00347          MOVE CP-PTC-RESERVE     TO PAYCURRO                      EL015
00348          MOVE CP-IBNR-RESERVE    TO IBNRO                         EL015
00349          MOVE CP-FUTURE-RESERVE  TO FUTVALO                       EL015
00350          MOVE AGE-INDEX          TO AIO                           EL015
00351          MOVE AGE-INDEX-LOW      TO AILOWO                        EL015
00352          MOVE AGE-INDEX-HIGH     TO AIHIGHO                       EL015
00353          MOVE AGE-INDEX-DIFF     TO AIDIFFO                       EL015
00354          MOVE TERM-INDEX         TO TIO                           EL015
00355          MOVE TERM-INDEX-LOW     TO TILOWO                        EL015
00356          MOVE TERM-INDEX-HIGH    TO TIHIGHO                       EL015
00357          MOVE TERM-INDEX-DIFF    TO TIDIFFO                       EL015
00358          MOVE WS-ODD-DAYS-OVER   TO ODDDAYSO                      EL015
00359          MOVE WS-DAYS-IN-MONTH   TO DIMO                          EL015
00360          MOVE WS-EXPIRE-DATE     TO DC-BIN-DATE-1                 EL015
00361          MOVE SPACES             TO DC-OPTION-CODE                EL015
00362          PERFORM 8500-DATE-CONVERSION                             EL015
00363          MOVE DC-GREG-DATE-1-EDIT  TO EXPDATEO                    EL015
00364          MOVE WS-EXPIRE-AGE      TO EXPAGEO                       EL015
00365          MOVE WS-REMAINING-TERM  TO REMTERMO                      EL015
00366          MOVE -1                 TO CLIENTL                          CL**4
00367          GO TO 8200-SEND-DATAONLY.                                EL015
00368                                                                   EL015
00369 *********** ERROR CODE RETURNED                                   EL015
00370      MOVE SPACES                 TO CDTBLO CDTFACTI PAYCURRI      EL015
00371                                     IBNRI  FUTVALI.               EL015
00372      MOVE CP-RETURN-CODE         TO ERRCODEO.                     EL015
00373      MOVE -1                     TO CLIENTL.                         CL**4
00374                                                                   EL015
00375      IF CP-ERROR-IN-AMOUNTS                                       EL015
00376         MOVE '** ERROR IN AMOUNTS'  TO EMI-MESSAGE-AREA (1)       EL015
00377      ELSE                                                         EL015
00378         IF CP-ERROR-IN-DATES                                      EL015
00379            MOVE '** ERROR IN DATES'    TO EMI-MESSAGE-AREA (1)    EL015
00380         ELSE                                                      EL015
00381            IF CP-ERROR-IN-TERMS                                   EL015
00382               MOVE '** ERROR IN TERMS'    TO EMI-MESSAGE-AREA (1).EL015
00383                                                                   EL015
00384      GO TO 8200-SEND-DATAONLY.                                    EL015
00385                                                                   EL015
00386      EJECT                                                        EL015
00387  8100-SEND-INITIAL-MAP.                                           EL015
00388      MOVE SAVE-DATE              TO RUNDATEO.                     EL015
00389      MOVE EIBTIME                TO TIME-IN.                      EL015
00390      MOVE TIME-OUT               TO RUNTIMEO.                     EL015
00391      MOVE -1                     TO CLIENTL.                         CL**4
00392      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL015
00393                                                                   EL015
00394      EXEC CICS SEND                                               EL015
00395          MAP    (MAP-NAME)                                        EL015
00396          MAPSET (MAPSET-NAME)                                     EL015
00397          FROM   (EL015AO)                                         EL015
00398          ERASE                                                    EL015
00399      END-EXEC.                                                    EL015
00400                                                                   EL015
00401      GO TO 9100-RETURN-TRAN.                                      EL015
00402                                                                   EL015
00403  8200-SEND-DATAONLY.                                              EL015
00404      MOVE SAVE-DATE              TO RUNDATEO.                     EL015
00405      MOVE EIBTIME                TO TIME-IN.                      EL015
00406      MOVE TIME-OUT               TO RUNTIMEO.                     EL015
00407      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL015
00408                                                                   EL015
00409 *    IF EIBCPOSN GREATER THAN +431                                EL015
00410 *        SUBTRACT +80 FROM EIBCPOSN.                              EL015
00411                                                                   EL015
00412      EXEC CICS SEND                                               EL015
00413          MAP    (MAP-NAME)                                        EL015
00414          MAPSET (MAPSET-NAME)                                     EL015
00415          FROM   (EL015AO)                                         EL015
00416          DATAONLY                                                 EL015
00417          CURSOR                                                   EL015
00418      END-EXEC.                                                    EL015
00419                                                                   EL015
00420      GO TO 9100-RETURN-TRAN.                                      EL015
00421                                                                   EL015
00422      EJECT                                                        EL015
00423  8500-DATE-CONVERSION SECTION.                                    EL015
00424 *    NOTE ******************************************************* EL015
00425 *         *                                                     * EL015
00426 *         *  THIS SECTION CALLS THE DATE CONVERSION SUBROUTINE. * EL015
00427 *         *                                                     * EL015
00428 *         *******************************************************.EL015
00429                                                                   EL015
00430  8510-DATE-CONVERSION.                                            EL015
00431      EXEC CICS LINK                                               EL015
00432          PROGRAM  (WS-DATE-CONVERSION-PROGRAM)                    EL015
00433          COMMAREA (DATE-CONVERSION-DATA)                          EL015
00434          LENGTH   (DC-COMM-LENGTH) END-EXEC.                      EL015
00435                                                                   EL015
00436  8590-EXIT.                                                       EL015
00437      EXIT.                                                        EL015
00438                                                                   EL015
00439  8950-CLEAR-RETURN SECTION.                                       EL015
00440      MOVE SPACES                 TO TEXT-AREA.                    EL015
00441      MOVE TRAN-COMPLETE-MSG      TO TEXT-AREA.                    EL015
00442                                                                   EL015
00443  8990-SEND-TEXT.                                                  EL015
00444      EXEC CICS SEND TEXT                                          EL015
00445          FROM    (TEXT-AREA)                                      EL015
00446          LENGTH  (TEXT-LENGTH)                                    EL015
00447          ERASE                                                    EL015
00448          FREEKB                                                   EL015
00449      END-EXEC.                                                    EL015
00450       EJECT                                                       EL015
00451                                                                   EL015
00452  9000-RETURN-CICS.                                                EL015
00453      EXEC CICS RETURN                                             EL015
00454      END-EXEC.                                                    EL015
00455                                                                   EL015
00456      GOBACK.                                                      EL015
00457                                                                   EL015
00458  9100-RETURN-TRAN.                                                EL015
00459      EXEC CICS RETURN                                             EL015
00460          TRANSID   (EIBTRNID)                                     EL015
00461          COMMAREA  (CALCULATION-PASS-AREA)                        EL015
00462          LENGTH    (CP-COMM-LENGTH)                               EL015
00463      END-EXEC.                                                    EL015
00464                                                                   EL015
00465  9700-LINK.                                                       EL015
00466      EXEC CICS LINK                                               EL015
00467          PROGRAM   (PGM-NAME)                                     EL015
00468          COMMAREA  (CALCULATION-PASS-AREA)                        EL015
00469          LENGTH    (CP-COMM-LENGTH)                               EL015
00470      END-EXEC.                                                    EL015
00471                                                                   EL015
00472  9700-EXIT.                                                       EL015
00473       EXIT.                                                       EL015
00474       EJECT                                                       EL015
uktdel*9900-CALCULATE-RESERVES SECTION. COPY ELCRESP1.                  EL015
uktins 9900-CALCULATE-RESERVES SECTION.
uktins      COPY ELCRESP1.
00476                                                                   EL015
00477       EJECT                                                       EL015
00478  9900-ERROR-FORMAT SECTION.                                       EL015
00479      IF NOT EMI-ERRORS-COMPLETE                                   EL015
00480          MOVE LINK-001           TO PGM-NAME                      EL015
00481          EXEC CICS LINK                                           EL015
00482              PROGRAM   (PGM-NAME)                                 EL015
00483              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL015
00484              LENGTH    (EMI-COMM-LENGTH)                          EL015
00485          END-EXEC.                                                EL015
00486                                                                   EL015
00487  9900-EXIT.                                                       EL015
00488      EXIT.                                                        EL015
00489                                                                   EL015
00490  9990-ABEND SECTION.                                              EL015
00491      MOVE LINK-004               TO PGM-NAME.                     EL015
00492      MOVE DFHEIBLK               TO EMI-LINE1.                    EL015
00493                                                                   EL015
00494      EXEC CICS LINK                                               EL015
00495          PROGRAM   (PGM-NAME)                                     EL015
00496          COMMAREA  (EMI-LINE1)                                    EL015
00497          LENGTH    (72)                                           EL015
00498      END-EXEC.                                                    EL015
00499                                                                   EL015
00500      GO TO 8200-SEND-DATAONLY.                                    EL015
