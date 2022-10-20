00001  IDENTIFICATION DIVISION.                                         01/14/97
00002                                                                   EL6312
00003  PROGRAM-ID.                 EL6312.                                 LV015
00004 *              PROGRAM CONVERTED BY                                  CL*12
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*12
00006 *              CONVERSION DATE 01/12/96 10:01:33.                    CL*12
00007 *                            VMOD=2.015.                             CL*15
00008 *                                                                 EL6312
00008 *                                                                 EL6312
00009 *AUTHOR.        LOGIC,INC.                                           CL*12
00010 *               DALLAS, TEXAS.                                       CL*12
00011                                                                   EL6312
00012 *DATE-COMPILED.                                                      CL*12
00013                                                                      CL**7
00014 *SECURITY.   *****************************************************   CL*12
00015 *            *                                                   *   CL*12
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*12
00017 *            *                                                   *   CL*12
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*12
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*12
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*12
00021 *            *                                                   *   CL*12
00022 *            *****************************************************   CL*12
00023                                                                   EL6312
00024 *REMARKS.                                                            CL**4
00025 *        TRANSACTION - EXB2 - REVIEW AND CORRECTION                  CL**4
00026 *                             DISPLAY EDIT CRITERIA.                 CL**4
00027  EJECT                                                               CL**4
00028  ENVIRONMENT DIVISION.                                            EL6312
00029  DATA DIVISION.                                                   EL6312
00030  WORKING-STORAGE SECTION.                                         EL6312
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6312
00032  77  FILLER  PIC X(32)  VALUE '*    EL6312 WORKING STORAGE    *'. EL6312
00033  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.015 ************'.    CL*15
00034                                                                   EL6312
00035      COPY ELCSCTM.                                                   CL**8
00036                                                                      CL**4
00037      COPY ELCSCRTY.                                                  CL**8
00038  EJECT                                                               CL**4
00039  01  STANDARD-AREAS.                                              EL6312
00040      12  MAP-NAME            PIC X(8)    VALUE 'EL631E'.          EL6312
00041      12  MAPSET-NAME         PIC X(8)    VALUE 'EL6312S'.         EL6312
00042      12  SCREEN-NUMBER       PIC X(4)    VALUE '631E'.            EL6312
00043      12  TRANS-ID            PIC X(4)    VALUE 'EXB2'.            EL6312
00044      12  THIS-PGM            PIC X(8)    VALUE 'EL6312'.          EL6312
00045      12  PGM-NAME            PIC X(8).                            EL6312
00046      12  TIME-IN             PIC S9(7).                           EL6312
00047      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6312
00048          16  FILLER          PIC X.                               EL6312
00049          16  TIME-OUT        PIC 99V99.                           EL6312
00050          16  FILLER          PIC X(2).                            EL6312
00051      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL6312
00052      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL6312
00053      12  XCTL-626            PIC X(8)    VALUE 'EL626'.           EL6312
00054      12  XCTL-6311           PIC X(8)    VALUE 'EL6311'.          EL6312
00055      12  LINK-001            PIC X(8)    VALUE 'EL001'.           EL6312
00056      12  LINK-004            PIC X(8)    VALUE 'EL004'.           EL6312
00057      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL6312
00058      12  WS-PHONE.                                                   CL**2
00059          16  WS-PH1              PIC XXX.                            CL**2
00060          16  WS-PH2              PIC XXX.                            CL**2
00061          16  WS-PH3              PIC XXXX.                           CL**2
00062      12  WS-PHONE-NUM REDEFINES WS-PHONE PIC 9(10).                  CL**2
00063      12  WS-DEV-RT               PIC S9V9(6) COMP-3.                 CL**7
00064                                                                      CL**2
00065  01  ERROR-MESSAGES.                                              EL6312
00066      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL6312
00067      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL6312
00068      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL6312
00069  EJECT                                                               CL**4
00070      COPY ELCDATE.                                                   CL**8
00071  EJECT                                                               CL**4
00072      COPY ELCLOGOF.                                                  CL**8
00073  EJECT                                                               CL**4
00074      COPY ELCATTR.                                                   CL**8
00075  EJECT                                                               CL**4
00076      COPY ELCEMIB.                                                   CL**8
00077  EJECT                                                               CL**4
00078      COPY ELCINTF.                                                   CL**8
00079      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                      CL**8
00080      COPY ELC631PI.                                               EL6312
00081         16  FILLER          PIC X(94).                               CL*13
00082  EJECT                                                               CL**4
00083  01  EDIT-CRITERIA-AREA.                                          EL6312
00084      COPY ELCEDITC.                                               EL6312
00085  EJECT                                                               CL**4
00086      COPY ELCAID.                                                    CL**8
00087                                                                      CL**4
00088  01  FILLER    REDEFINES DFHAID.                                  EL6312
00089      12  FILLER              PIC X(8).                            EL6312
00090      12  PF-VALUES           PIC X       OCCURS 24 TIMES.         EL6312
00091  EJECT                                                               CL**4
00092     COPY EL6312S.                                                    CL**8
00093  EJECT                                                               CL**4
00094  LINKAGE SECTION.                                                 EL6312
00095  01  DFHCOMMAREA             PIC X(1300).                            CL**3
00096  EJECT                                                               CL**4
00097  PROCEDURE DIVISION.                                              EL6312
00098      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL6312
00099      MOVE PI-CRITERIA-DATA       TO EDIT-CRITERIA-DATA.           EL6312
00100                                                                   EL6312
00101      IF EIBCALEN = 0                                              EL6312
00102          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6312
00103                                                                   EL6312
00104      MOVE LOW-VALUES             TO EL631EI.                      EL6312
00105                                                                   EL6312
00106      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6312
00107          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6312
00108              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL6312
00109              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL6312
00110              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL6312
00111              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL6312
00112              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL6312
00113              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL6312
00114              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL6312
00115              MOVE THIS-PGM             TO PI-CALLING-PROGRAM         CL**7
00116              GO TO 0350-DISPLAY-COMMON-DATA.                      EL6312
00117                                                                   EL6312
00118      IF EIBAID = DFHCLEAR                                         EL6312
00119          GO TO 9400-CLEAR.                                        EL6312
00120                                                                   EL6312
00121  EJECT                                                               CL**4
00122  0200-RECEIVE.                                                    EL6312
00123      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6312
00124          MOVE ER-0008            TO EMI-ERROR                     EL6312
00125          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6312
00126          MOVE -1                 TO PFENTERL                      EL6312
00127          GO TO 8100-SEND-INITIAL-MAP.                             EL6312
00128                                                                   EL6312
00129      EXEC CICS RECEIVE                                            EL6312
00130          MAP      (MAP-NAME)                                      EL6312
00131          MAPSET   (MAPSET-NAME)                                   EL6312
00132          INTO     (EL631EI)                                       EL6312
00133      END-EXEC.                                                       CL**7
00134                                                                   EL6312
00135      IF PFENTERL = 0                                              EL6312
00136          GO TO 0300-CHECK-PFKEYS.                                 EL6312
00137      IF EIBAID NOT = DFHENTER                                     EL6312
00138          MOVE ER-0004            TO EMI-ERROR                     EL6312
00139          GO TO 0320-INPUT-ERROR.                                  EL6312
00140      IF (PFENTERI NUMERIC) AND (PFENTERI > 0 AND < 25)            EL6312
00141          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL6312
00142      ELSE                                                         EL6312
00143          MOVE ER-0029            TO EMI-ERROR                     EL6312
00144          GO TO 0320-INPUT-ERROR.                                  EL6312
00145                                                                   EL6312
00146  0300-CHECK-PFKEYS.                                               EL6312
00147      IF EIBAID = DFHPF1                                           EL6312
00148         GO TO 0350-DISPLAY-COMMON-DATA.                           EL6312
00149                                                                   EL6312
00150      IF EIBAID = DFHPF23                                          EL6312
00151          GO TO 8810-PF23.                                         EL6312
00152                                                                   EL6312
00153      IF EIBAID = DFHPF24                                          EL6312
00154          GO TO 9200-RETURN-MAIN-MENU.                             EL6312
00155                                                                   EL6312
00156      IF EIBAID = DFHPF12                                          EL6312
00157          GO TO 9500-PF12.                                         EL6312
00158                                                                   EL6312
00159      IF EIBAID = DFHENTER                                         EL6312
00160          GO TO 8200-SEND-DATAONLY.                                EL6312
00161                                                                   EL6312
00162      MOVE ER-0029                TO EMI-ERROR.                    EL6312
00163  0320-INPUT-ERROR.                                                EL6312
00164      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6312
00165      MOVE AL-UNBON               TO PFENTERA.                     EL6312
00166      MOVE -1                     TO PFENTERL.                     EL6312
00167      GO TO 8200-SEND-DATAONLY.                                    EL6312
00168                                                                   EL6312
00169  EJECT                                                               CL**4
00170  0350-DISPLAY-COMMON-DATA.                                        EL6312
00171      IF EC-CO-TOL-PREM    NUMERIC                                 EL6312
00172         IF EC-CO-TOL-PREM NOT = ZEROS                             EL6312
00173            MOVE EC-CO-TOL-PREM      TO CISSTOLO.                  EL6312
00174                                                                   EL6312
00175      IF EC-CO-TOL-REFUND  NUMERIC                                 EL6312
00176         IF EC-CO-TOL-REFUND NOT = ZEROS                           EL6312
00177            MOVE EC-CO-TOL-REFUND    TO COVSAMTO.                     CL*15
00178                                                                      CL*15
00179      IF EC-CO-OVR-SHT-AMT NUMERIC                                    CL*15
00180         IF EC-CO-OVR-SHT-AMT > +0                                    CL*15
00181            MOVE EC-CO-OVR-SHT-AMT   TO CCANTOLO.                     CL*15
00182                                                                   EL6312
00183      IF EC-CO-MIN-AGE     NUMERIC                                 EL6312
00184         IF EC-CO-MIN-AGE  NOT = ZEROS                             EL6312
00185            MOVE EC-CO-MIN-AGE       TO CMAGEO.                    EL6312
00186                                                                   EL6312
00187      IF EC-CO-MIN-PREMIUM NUMERIC                                 EL6312
00188         IF EC-CO-MIN-PREMIUM NOT = ZEROS                          EL6312
00189            MOVE EC-CO-MIN-PREMIUM   TO CMPREMO.                   EL6312
00190                                                                   EL6312
00191      IF EC-CO-MIN-TERM    NUMERIC                                 EL6312
00192         IF EC-CO-MIN-TERM NOT = ZEROS                             EL6312
00193            MOVE EC-CO-MIN-TERM      TO CMTERMO.                   EL6312
00194                                                                   EL6312
00195      IF EC-CO-MAX-TERM    NUMERIC                                 EL6312
00196         IF EC-CO-MAX-TERM NOT = ZEROS                             EL6312
00197            MOVE EC-CO-MAX-TERM      TO CMXTERMO.                  EL6312
00198                                                                   EL6312
00199      IF EC-CO-REM-TERM-CALC   = '1'                               EL6312
00200        MOVE 'AFTER 15TH'   TO CRTERMO                                CL**9
00201       ELSE                                                           CL**9
00202        IF EC-CO-REM-TERM-CALC   = '2'                                CL**9
00203          MOVE 'FIRST HALF'   TO CRTERMO                              CL**9
00204         ELSE                                                      EL6312
00205          IF EC-CO-REM-TERM-CALC   = '3'                              CL**9
00206            MOVE 'ON 1ST DAY'   TO CRTERMO                            CL**9
00207           ELSE                                                       CL**9
00208            IF EC-CO-REM-TERM-CALC   = '4'                            CL**9
00209              MOVE 'ON LAST DAY'   TO CRTERMO                         CL**9
00210             ELSE                                                     CL**9
00211              IF EC-CO-REM-TERM-CALC   = '5'                          CL**9
00212                MOVE 'NO DAYS'       TO CRTERMO                       CL**9
00213               ELSE                                                EL6312
00214                IF EC-CO-REM-TERM-CALC   = '6'                        CL**9
00215                  MOVE 'AFTER 14TH'    TO CRTERMO                     CL**9
00216                 ELSE                                                 CL**9
00217                  IF EC-CO-REM-TERM-CALC   = '7'                      CL**9
00218                    MOVE 'AFTER 16TH'    TO CRTERMO.                  CL**9
00219                                                                   EL6312
00220      MOVE EC-CO-MONTH-END-DT     TO DC-BIN-DATE-1.                   CL**7
00221      MOVE SPACE                  TO DC-OPTION-CODE.                  CL**7
00222      PERFORM 9700-DATE-LINK.                                         CL**7
00223      IF NO-CONVERSION-ERROR                                       EL6312
00224         MOVE DC-GREG-DATE-1-EDIT TO CMENDO.                       EL6312
00225                                                                   EL6312
00226       IF EIBTRNID  NOT = TRANS-ID                                 EL6312
00227          IF EC-BR-LF-DESC NOT = SPACES                            EL6312
00228             GO TO 500-DISPLAY-LIFE.                               EL6312
00229                                                                   EL6312
00230      IF PI-DISPLAY-AH                                             EL6312
00231         GO TO 500-DISPLAY-LIFE.                                   EL6312
00232  EJECT                                                               CL**4
00233  400-DISPLAY-AH.                                                  EL6312
00234      MOVE 'AH'                   TO PI-DISPLAY-SW.                   CL**7
00235      MOVE PI-AH-OVERRIDE-L6      TO HEADI.                           CL**7
00236      MOVE PI-LIFE-OVERRIDE-L6    TO PFOPTI.                       EL6312
00237                                                                      CL**7
00238      IF EC-BR-AH-EARN-BY-R78                                         CL**7
00239          MOVE 'RULE 78'           TO BEARNMO                         CL**7
00240         ELSE                                                      EL6312
00241          IF EC-BR-AH-EARN-BY-PRO-RATA                                CL**7
00242              MOVE 'PRO-RATA'          TO BEARNMO                     CL**7
00243            ELSE                                                   EL6312
00244              IF EC-BR-AH-EARN-AS-CALIF                               CL**7
00245                  MOVE 'CALIF'          TO BEARNMO                    CL**4
00246               ELSE                                                EL6312
00247                  IF EC-BR-AH-EARN-IS-NET-PAY                         CL**4
00248                     MOVE 'NET PAY'        TO BEARNMO                 CL**4
00249                  ELSE                                                CL**4
00250                     IF EC-BR-AH-EARN-ANTICIPATION                    CL**4
00251                        MOVE 'ANTICIPAT'      TO BEARNMO              CL**4
00252                     ELSE                                             CL**4
00253                        IF EC-BR-AH-EARN-MEAN                         CL**4
00254                           MOVE 'MEAN'        TO BEARNMO              CL**4
00255                        ELSE                                          CL**4
00256                           IF EC-BR-AH-EARN-REG-BALLOON               CL**4
00257                              MOVE 'BALLOON'  TO BEARNMO.             CL**4
00258                                                                   EL6312
00259         IF EC-CF-AH-OUTSTANDING-BAL                               EL6312
00260            MOVE 'OUT BAL'           TO BSPECO                     EL6312
00261         ELSE                                                      EL6312
00262            IF EC-CF-AH-CRITICAL-PERIOD                            EL6312
00263               MOVE 'CRT PERIOD'    TO BSPECO.                     EL6312
00264                                                                   EL6312
00265         EVALUATE TRUE                                                CL*14
00266            WHEN EC-BR-AH-REFD-BY-R78                                 CL*14
00267               MOVE 'RULE 78'           TO BRMETHO                    CL*14
00268            WHEN EC-BR-AH-REFD-BY-PRO-RATA                            CL*14
00269               MOVE 'PRO-RATA'          TO BRMETHO                 EL6312
00270            WHEN EC-BR-AH-REFD-AS-CALIF                               CL*14
00271               MOVE 'CALIFORNIA'        TO BRMETHO                    CL*14
00272            WHEN EC-BR-AH-REFD-AS-TEXAS                               CL*14
00273               MOVE 'IRREG'             TO BRMETHO                    CL*14
00274            WHEN EC-BR-AH-REFD-IS-NET-PAY                             CL*14
00275               MOVE 'NET PAY'        TO BRMETHO                       CL*14
00276            WHEN EC-BR-AH-REFD-ANTICIPATION                           CL*14
00277               MOVE 'ANTICIPAT'      TO BRMETHO                       CL*14
00278            WHEN EC-BR-AH-REFD-IS-MEAN                                CL*14
00279               MOVE 'MEAN'           TO BRMETHO                       CL*14
00280            WHEN EC-BR-AH-REFD-SUM-OF-DIGIT                           CL*14
00281               MOVE 'SUM DIGIT'      TO BRMETHO                       CL*14
00282         END-EVALUATE.                                                CL*14
00283                                                                   EL6312
00284                                                                   EL6312
00285      MOVE EC-BR-AH-DESC          TO BTYPEO.                          CL**7
00286      MOVE EC-BR-AH-COMMENT       TO BCOMMO.                          CL**7
00287      MOVE EC-BR-AH-BEN-I-G-CD    TO BIGO.                            CL**7
00288                                                                   EL6312
00289      IF EC-BR-AH-REM-TERM-CALC = '1'                                 CL**9
00290        MOVE 'AFTER 15TH'   TO BRTERMO                                CL**9
00291       ELSE                                                           CL**9
00292        IF EC-BR-AH-REM-TERM-CALC = '2'                               CL**9
00293          MOVE 'FIRST HALF'   TO BRTERMO                              CL**9
00294         ELSE                                                         CL**9
00295          IF EC-BR-AH-REM-TERM-CALC = '3'                             CL**9
00296            MOVE 'ON 1ST DAY'   TO BRTERMO                            CL**9
00297           ELSE                                                       CL**9
00298            IF EC-BR-AH-REM-TERM-CALC = '4'                           CL**9
00299              MOVE 'ON LAST DAY'   TO BRTERMO                         CL**9
00300             ELSE                                                     CL**9
00301              IF EC-BR-AH-REM-TERM-CALC = '5'                         CL**9
00302                MOVE 'NO DAYS'       TO BRTERMO                       CL**9
00303               ELSE                                                   CL**9
00304                IF EC-BR-AH-REM-TERM-CALC = '6'                       CL**9
00305                  MOVE 'AFTER 14TH'    TO BRTERMO                     CL**9
00306                 ELSE                                                 CL**9
00307                  IF EC-BR-AH-REM-TERM-CALC = '7'                     CL**9
00308                    MOVE 'AFTER 16TH'    TO BRTERMO.                  CL**9
00309                                                                      CL**9
00310      IF EC-ST-TOL-PREM    NUMERIC                                 EL6312
00311         IF EC-ST-TOL-PREM NOT = ZEROS                             EL6312
00312            MOVE EC-ST-TOL-PREM   TO SISSTOLO.                        CL**6
00313                                                                   EL6312
00314      IF EC-ST-TOL-REFUND NUMERIC                                     CL**6
00315         IF EC-ST-TOL-REFUND NOT = ZEROS                           EL6312
00316            MOVE EC-ST-TOL-REFUND TO SCANTOLO.                        CL**6
00317                                                                   EL6312
00318      IF EC-ST-OVR-SHT-AMT NUMERIC                                    CL*15
00319         IF EC-ST-OVR-SHT-AMT >+ 0                                    CL*15
00320            MOVE EC-ST-OVR-SHT-AMT TO SOVSTOLO.                       CL*15
00321                                                                      CL*15
00322      IF EC-ST-TOL-PREM-PCT NUMERIC                                   CL**6
00323         IF EC-ST-TOL-PREM-PCT GREATER THAN +0                        CL**6
00324            MOVE EC-ST-TOL-PREM-PCT                                   CL**6
00325                                  TO SISSPCTO.                        CL**6
00326                                                                      CL**6
00327      IF EC-ST-TOL-REFUND-PCT NUMERIC                                 CL**6
00328         IF EC-ST-TOL-REFUND-PCT GREATER THAN +0                      CL**6
00329            MOVE EC-ST-TOL-REFUND-PCT                                 CL**6
00330                                  TO SCANPCTO.                        CL**6
00331                                                                      CL*15
00332      IF EC-ST-OVR-SHT-PCT NUMERIC                                    CL*15
00333         IF EC-ST-OVR-SHT-PCT > +0                                    CL*15
00334            MOVE EC-ST-OVR-SHT-PCT TO SOVSPCTO.                       CL*15
00335                                                                   EL6312
00336      IF EC-ST-AH-REM-TERM-CALC   = '1'                            EL6312
00337        MOVE 'AFTER 15TH'   TO SRTERMO                                CL**9
00338       ELSE                                                           CL**9
00339        IF EC-ST-AH-REM-TERM-CALC   = '2'                             CL**9
00340          MOVE 'FIRST HALF'   TO SRTERMO                              CL**9
00341         ELSE                                                      EL6312
00342          IF EC-ST-AH-REM-TERM-CALC   = '3'                           CL**9
00343            MOVE 'ON 1ST DAY'     TO SRTERMO                          CL**9
00344           ELSE                                                       CL**9
00345            IF EC-ST-AH-REM-TERM-CALC   = '4'                         CL**9
00346              MOVE 'ON LAST DAY'   TO SRTERMO                         CL**9
00347             ELSE                                                     CL**9
00348              IF EC-ST-AH-REM-TERM-CALC   = '5'                       CL**9
00349                MOVE 'NO DAYS'       TO SRTERMO                       CL**9
00350               ELSE                                                EL6312
00351                IF EC-ST-AH-REM-TERM-CALC   = '6'                     CL**9
00352                  MOVE 'AFTER 14TH'    TO SRTERMO                     CL**9
00353                 ELSE                                                 CL**9
00354                  IF EC-ST-AH-REM-TERM-CALC   = '7'                   CL**9
00355                    MOVE 'AFTER 16TH'    TO SRTERMO.                  CL**9
00356                                                                   EL6312
00357      EVALUATE TRUE                                                   CL*14
00358         WHEN EC-ST-AH-REFD-BY-R78                                    CL*14
00359            MOVE 'RULE 78'        TO SRMETHO                          CL*14
00360         WHEN EC-ST-AH-REFD-BY-PRO-RATA                               CL*14
00361            MOVE 'PRO-RATA'       TO SRMETHO                          CL*14
00362         WHEN EC-ST-AH-REFD-AS-CALIF                                  CL*14
00363            MOVE 'CALIFORNIA'     TO SRMETHO                          CL*14
00364         WHEN EC-ST-AH-REFD-AS-TEXAS                                  CL*14
00365            MOVE 'IRREG'          TO SRMETHO                          CL*14
00366         WHEN EC-ST-AH-REFD-IS-NET-PAY                                CL*14
00367            MOVE 'NET PAY'        TO SRMETHO                          CL*14
00368         WHEN EC-ST-AH-REFD-ANTICIPATION                              CL*14
00369            MOVE 'ANTICIPAT'      TO SRMETHO                          CL*14
00370         WHEN EC-ST-AH-REFD-IS-MEAN                                   CL*14
00371            MOVE 'MEAN'           TO SRMETHO                          CL*14
00372         WHEN EC-ST-AH-REFD-SUM-OF-DIGIT                              CL*14
00373            MOVE 'SUM DIGIT'      TO SRMETHO                          CL*14
00374      END-EVALUATE.                                                   CL*14
00375                                                                   EL6312
00376      IF EC-AM-AH-TOL-PREM    NUMERIC                                 CL*15
00377         IF EC-AM-AH-TOL-PREM NOT = ZEROS                             CL*15
00378            MOVE EC-AM-AH-TOL-PREM   TO AISSTOLO.                     CL*15
00379                                                                   EL6312
00380      IF EC-AM-AH-TOL-REFUND  NUMERIC                                 CL*15
00381         IF EC-AM-AH-TOL-REFUND NOT = ZEROS                           CL*15
00382            MOVE EC-AM-AH-TOL-REFUND TO ACANTOLO.                     CL*15
00383                                                                      CL*15
00384      IF EC-AM-AH-OVR-SHT-AMT NUMERIC                                 CL*15
00385         IF EC-AM-AH-OVR-SHT-AMT > +0                                 CL*15
00386            MOVE EC-AM-AH-OVR-SHT-AMT TO AOVSTOLO.                    CL*15
00387                                                                   EL6312
00388      EVALUATE TRUE                                                   CL*14
00389         WHEN EC-AM-AH-REFD-BY-R78                                    CL*14
00390            MOVE 'RULE 78'           TO ARMETHO                    EL6312
00391         WHEN EC-AM-AH-REFD-BY-PRO-RATA                               CL*14
00392            MOVE 'PRO-RATA'          TO ARMETHO                       CL*14
00393         WHEN EC-AM-AH-REFD-AS-CALIF                                  CL*14
00394            MOVE 'CALIFORNIA'        TO ARMETHO                       CL*14
00395         WHEN EC-AM-AH-REFD-ANTICIPATION                              CL*14
00396            MOVE 'ANTICIPAT'      TO ARMETHO                          CL*14
00397         WHEN EC-AM-AH-REFD-IS-MEAN                                   CL*14
00398            MOVE 'MEAN'           TO ARMETHO                          CL*14
00399         WHEN EC-AM-AH-REFD-IS-NET                                    CL*14
00400           MOVE 'NET PAY'        TO ARMETHO                           CL*14
00401         WHEN EC-AM-AH-REFD-SUM-OF-DIGIT                              CL*14
00402            MOVE 'SUM DIGIT'      TO ARMETHO                          CL*14
00403      END-EVALUATE.                                                   CL*14
00404                                                                   EL6312
00405      MOVE EC-AM-BEN-I-G-CD       TO AIGO.                            CL**2
00406      MOVE EC-AM-CLASS-CD         TO ACLASSO.                         CL**2
00407      MOVE EC-AM-AH-DEVIATION     TO ADEVO.                           CL**2
00408      MOVE EC-AM-PHONE-NO         TO WS-PHONE.                        CL**2
00409      MOVE WS-PHONE-NUM           TO PHONEO.                          CL**2
00410      INSPECT PHONEO REPLACING ALL ' ' BY '-'.                        CL*12
00411                                                                      CL*15
00412      IF EC-AM-AH-DEV-PERCENT NUMERIC                                 CL*15
00413         IF  EC-AM-AH-DEV-PERCENT > +0                                CL*15
00414             MOVE EC-AM-AH-DEV-PERCENT   TO ADEVPCTO.                 CL*15
00415                                                                   EL6312
00416      IF EC-AM-EXPIRATION-DT NOT = HIGH-VALUES                     EL6312
00417         MOVE EC-AM-EXPIRATION-DT    TO DC-BIN-DATE-1              EL6312
00418         MOVE SPACE                  TO DC-OPTION-CODE             EL6312
00419         PERFORM 9700-DATE-LINK                                    EL6312
00420         IF NO-CONVERSION-ERROR                                    EL6312
00421            MOVE DC-GREG-DATE-1-EDIT TO AEXPDTEO                   EL6312
00422           ELSE                                                       CL**7
00423            NEXT SENTENCE                                          EL6312
00424         ELSE                                                      EL6312
00425          MOVE '99/99/99'          TO AEXPDTEO.                       CL**7
00426                                                                   EL6312
00427      MOVE EC-AM-EFFECTIVE-DT     TO DC-BIN-DATE-1.                   CL**7
00428      MOVE SPACE                  TO DC-OPTION-CODE.                  CL**7
00429      PERFORM 9700-DATE-LINK.                                         CL**7
00430      IF NO-CONVERSION-ERROR                                       EL6312
00431         MOVE DC-GREG-DATE-1-EDIT TO AEFFDTO.                      EL6312
00432                                                                   EL6312
00433      IF EC-AM-AH-MAX-ATT-AGE  NUMERIC                             EL6312
00434         IF EC-AM-AH-MAX-ATT-AGE NOT = ZEROS                       EL6312
00435            MOVE EC-AM-AH-MAX-ATT-AGE  TO AMAAGEO.                 EL6312
00436                                                                   EL6312
00437      IF EC-AM-AH-MAX-AGE  NUMERIC                                 EL6312
00438         IF EC-AM-AH-MAX-AGE NOT = ZEROS                           EL6312
00439            MOVE EC-AM-AH-MAX-AGE      TO AMAGEO.                  EL6312
00440                                                                   EL6312
00441      IF EC-AM-AH-MAX-TERM   NUMERIC                               EL6312
00442         IF EC-AM-AH-MAX-TERM NOT = ZEROS                          EL6312
00443            MOVE EC-AM-AH-MAX-TERM   TO AMTERMO.                   EL6312
00444                                                                   EL6312
00445      IF EC-AM-AH-MAX-TOT-BEN NUMERIC                              EL6312
00446         IF EC-AM-AH-MAX-TOT-BEN NOT = ZEROS                       EL6312
00447            MOVE EC-AM-AH-MAX-TOT-BEN  TO AMTBENO.                 EL6312
00448                                                                   EL6312
00449      IF EC-AM-AH-MAX-MON-BEN NUMERIC                              EL6312
00450         IF EC-AM-AH-MAX-MON-BEN NOT = ZEROS                       EL6312
00451            MOVE EC-AM-AH-MAX-MON-BEN  TO AMMBENO                  EL6312
00452            MOVE 'MAX-MON-BEN'         TO AHEADMO.                 EL6312
00453                                                                   EL6312
00454      IF EC-RT-AH-MAX-ATT-AGE  NUMERIC                             EL6312
00455         IF EC-RT-AH-MAX-ATT-AGE NOT = ZEROS                          CL*12
00456            MOVE EC-RT-AH-MAX-ATT-AGE  TO RMAAGEO.                 EL6312
00457                                                                   EL6312
00458      IF EC-RT-AH-MAX-AGE  NUMERIC                                 EL6312
00459         IF EC-RT-AH-MAX-AGE NOT = ZEROS                              CL*12
00460            MOVE EC-RT-AH-MAX-AGE      TO RMAGEO.                  EL6312
00461                                                                   EL6312
00462      IF EC-RT-AH-MAX-TERM   NUMERIC                               EL6312
00463         IF EC-RT-AH-MAX-TERM NOT = ZEROS                          EL6312
00464            MOVE EC-RT-AH-MAX-TERM   TO RMTERMO.                   EL6312
00465                                                                   EL6312
00466      IF EC-RT-AH-MAX-TOT-BEN NUMERIC                              EL6312
00467         IF EC-RT-AH-MAX-TOT-BEN NOT = ZEROS                       EL6312
00468            MOVE EC-RT-AH-MAX-TOT-BEN  TO RMTBENO.                 EL6312
00469                                                                   EL6312
00470      IF EC-RT-AH-MAX-MON-BEN NUMERIC                              EL6312
00471         IF EC-RT-AH-MAX-MON-BEN NOT = ZEROS                       EL6312
00472            MOVE EC-RT-AH-MAX-MON-BEN  TO RMMBENO.                 EL6312
00473                                                                   EL6312
00474      IF EC-RT-AH-RATE   NUMERIC                                   EL6312
00475          IF EC-CF-AH-OUTSTANDING-BAL                              EL6312
00476              MOVE ZEROS           TO RRATESO                      EL6312
00477              MOVE EC-RT-AH-RATE   TO OBRATEO                      EL6312
00478              MOVE ' OB RATE'      TO OBHEADO                         CL**7
00479          ELSE                                                     EL6312
00480              MOVE EC-RT-AH-RATE   TO RRATESO                         CL**7
00481              COMPUTE WS-DEV-RT = EC-RT-AH-RATE *                     CL**7
00482                                  EC-AM-AH-DEV-PERCENT                CL**7
00483              MOVE WS-DEV-RT       TO DRATESO.                        CL**7
00484                                                                   EL6312
00485      MOVE EC-RT-AH-NSP-ST        TO RNSPSTO.                      EL6312
00486                                                                   EL6312
00487      IF EC-RT-AH-NSP-ST NOT = SPACES                              EL6312
00488         IF EC-RT-AH-NSP-RATE   NUMERIC                            EL6312
00489            MOVE EC-RT-AH-NSP-RATE       TO RNSPRTO.               EL6312
00490                                                                   EL6312
00491      IF EC-CM-AH-POLICY-IS-ACTIVE                                 EL6312
00492         MOVE 'ACTIVE'            TO CTSTO                         EL6312
00493         ELSE                                                      EL6312
00494         IF EC-CM-AH-LUMP-SUM-DISAB                                EL6312
00495            MOVE 'LUMP SUM'    TO CTSTO                            EL6312
00496            ELSE                                                   EL6312
00497            IF EC-CM-AH-DEATH-CLAIM-APPLIED                        EL6312
00498               MOVE 'DEATH'    TO CTSTO  CTDATEHO                  EL6312
00499               MOVE 'DEATH-DT' TO CTDATEHO                         EL6312
00500               ELSE                                                EL6312
00501               IF EC-CM-AH-CANCEL-APPLIED                          EL6312
00502                  MOVE 'CANCELLED'   TO CTSTO  CTDATEHO            EL6312
00503                  MOVE 'CANCEL-DT'   TO CTDATEHO.                  EL6312
00504                                                                   EL6312
00505      IF EC-CM-AH-PRIOR-REFUND  NUMERIC                            EL6312
00506         IF EC-CM-AH-PRIOR-REFUND  NOT = ZEROS                     EL6312
00507            MOVE EC-CM-AH-PRIOR-REFUND    TO CTRFUNDO.             EL6312
00508                                                                   EL6312
00509      IF EC-CM-AH-DEATH-CLAIM-APPLIED                              EL6312
00510         MOVE EC-CM-DEATH-DT         TO DC-BIN-DATE-1              EL6312
00511         MOVE SPACE                  TO DC-OPTION-CODE             EL6312
00512         PERFORM 9700-DATE-LINK                                    EL6312
00513         IF NO-CONVERSION-ERROR                                    EL6312
00514            MOVE DC-GREG-DATE-1-EDIT TO CTDATEO.                   EL6312
00515                                                                   EL6312
00516      IF EC-CM-AH-CANCEL-APPLIED                                   EL6312
00517         MOVE EC-CM-AH-CANCEL-DT     TO DC-BIN-DATE-1              EL6312
00518         MOVE SPACE                  TO DC-OPTION-CODE             EL6312
00519         PERFORM 9700-DATE-LINK                                    EL6312
00520         IF NO-CONVERSION-ERROR                                    EL6312
00521            MOVE DC-GREG-DATE-1-EDIT TO CTDATEO.                   EL6312
00522      GO TO 8100-SEND-INITIAL-MAP.                                 EL6312
00523  EJECT                                                               CL**4
00524  500-DISPLAY-LIFE.                                                EL6312
00525      MOVE 'LF'                   TO PI-DISPLAY-SW                 EL6312
00526      MOVE PI-LIFE-OVERRIDE-L6    TO HEADI                         EL6312
00527      MOVE PI-AH-OVERRIDE-L6      TO PFOPTI.                       EL6312
00528                                                                   EL6312
00529      EVALUATE TRUE                                                   CL*14
00530         WHEN EC-BR-LF-EARN-BY-R78                                    CL*14
00531            MOVE 'RULE 78'           TO BEARNMO                       CL*14
00532         WHEN EC-BR-LF-EARN-BY-PRO-RATA                               CL*14
00533            MOVE 'PRO-RATA'          TO BEARNMO                       CL*14
00534         WHEN EC-BR-LF-EARN-AS-TEXAS                                  CL*14
00535            MOVE 'IRREG'             TO BEARNMO                       CL*14
00536         WHEN EC-BR-LF-EARN-AS-FARM-PLAN                              CL*14
00537            MOVE 'FARM'              TO BEARNMO                       CL*14
00538         WHEN EC-BR-LF-EARN-IS-NET-PAY                                CL*14
00539            MOVE 'NET PAY'           TO BEARNMO                       CL*14
00540         WHEN EC-BR-LF-EARN-ANTICIPATION                              CL*14
00541            MOVE 'ANTICIPAT'         TO BEARNMO                       CL*14
00542         WHEN EC-BR-LF-EARN-MEAN                                      CL*14
00543            MOVE 'MEAN'              TO BEARNMO                       CL*14
00544         WHEN EC-BR-LF-EARN-REG-BALLOON                               CL*14
00545            MOVE 'BALLOON'           TO BEARNMO                       CL*14
00546         WHEN EC-BR-LF-EARN-SUM-OF-DIGIT                              CL*14
00547            MOVE 'SUM DIGIT'         TO BEARNMO                       CL*14
00548         WHEN OTHER                                                   CL*14
00549            MOVE SPACES              TO BEARNMO                       CL*14
00550      END-EVALUATE.                                                   CL*14
00551                                                                   EL6312
00552         IF EC-CF-LF-OUTSTANDING-BAL                               EL6312
00553            MOVE 'OUT BAL'           TO BSPECO                     EL6312
00554          ELSE                                                     EL6312
00555            IF EC-CF-LF-ALTERNATE-NET-PAY                          EL6312
00556               MOVE 'ALT NET PY'        TO BSPECO                  EL6312
00557            ELSE                                                   EL6312
00558               IF EC-CF-LF-NET-PAY-SIMPLE                          EL6312
00559                     MOVE 'NET PAY'           TO BSPECO               CL**7
00560                ELSE                                               EL6312
00561                  IF EC-CF-LF-CRITICAL-PERIOD                      EL6312
00562                     MOVE 'CRT PERIOD'    TO BSPECO                EL6312
00563                  ELSE                                             EL6312
00564                        IF EC-CF-LF-TERM-IN-DAYS                   EL6312
00565                           MOVE 'TRM IN DAY'    TO BSPECO.         EL6312
00566                                                                   EL6312
00567      EVALUATE TRUE                                                   CL*14
00568         WHEN EC-BR-LF-REFD-BY-R78                                    CL*14
00569            MOVE 'RULE 78'        TO BRMETHO                          CL*14
00570         WHEN EC-BR-LF-REFD-BY-PRO-RATA                               CL*14
00571            MOVE 'PRO-RATA'       TO BRMETHO                          CL*14
00572         WHEN EC-BR-LF-REFD-AS-TEXAS                                  CL*14
00573            MOVE 'IRREG'          TO BRMETHO                          CL*14
00574         WHEN EC-BR-LF-REFD-IS-NET-PAY                                CL*14
00575            MOVE 'NET PAY'        TO BRMETHO                          CL*14
00576         WHEN EC-BR-LF-REFD-ANTICIPATION                              CL*14
00577            MOVE 'ANTICIPAT'      TO BRMETHO                          CL*14
00578         WHEN EC-BR-LF-REFD-IS-MEAN                                   CL*14
00579            MOVE 'MEAN'           TO BRMETHO                          CL*14
00580         WHEN EC-BR-LF-REFD-IS-SUM-OF-DIGIT                           CL*14
00581            MOVE 'SUM DIGIT'      TO BRMETHO                          CL*14
00582         WHEN OTHER                                                   CL*14
00583            MOVE SPACES           TO BRMETHO                          CL*14
00584      END-EVALUATE.                                                   CL*14
00585                                                                   EL6312
00586      MOVE EC-BR-LF-DESC          TO BTYPEO.                          CL**7
00587      MOVE EC-BR-LF-COMMENT       TO BCOMMO.                          CL**7
00588      MOVE EC-BR-LF-BEN-I-G-CD    TO BIGO.                            CL**7
00589                                                                   EL6312
00590      IF EC-BR-LF-REM-TERM-CALC = '1'                                 CL**9
00591        MOVE 'AFTER 15TH'   TO BRTERMO                                CL**9
00592       ELSE                                                           CL**9
00593        IF EC-BR-LF-REM-TERM-CALC = '2'                               CL**9
00594          MOVE 'FIRST HALF'   TO BRTERMO                              CL**9
00595         ELSE                                                         CL**9
00596          IF EC-BR-LF-REM-TERM-CALC = '3'                             CL**9
00597            MOVE 'ON 1ST DAY'   TO BRTERMO                            CL**9
00598           ELSE                                                       CL**9
00599            IF EC-BR-LF-REM-TERM-CALC = '4'                           CL**9
00600              MOVE 'ON LAST DAY'   TO BRTERMO                         CL**9
00601             ELSE                                                     CL**9
00602              IF EC-BR-LF-REM-TERM-CALC = '5'                         CL**9
00603                MOVE 'NO DAYS'       TO BRTERMO                       CL**9
00604               ELSE                                                   CL**9
00605               IF EC-BR-LF-REM-TERM-CALC = '6'                        CL**9
00606                 MOVE 'AFTER 14TH'    TO BRTERMO                      CL**9
00607                ELSE                                                  CL**9
00608                 IF EC-BR-LF-REM-TERM-CALC = '7'                      CL**9
00609                   MOVE 'AFTER 16TH'    TO BRTERMO.                   CL**9
00610                                                                      CL**9
00611      IF EC-ST-TOL-PREM    NUMERIC                                 EL6312
00612         IF EC-ST-TOL-PREM NOT = ZEROS                             EL6312
00613            MOVE EC-ST-TOL-PREM      TO SISSTOLO.                  EL6312
00614                                                                   EL6312
00615      IF EC-ST-OVR-SHT-AMT NUMERIC                                    CL*15
00616         IF EC-ST-OVR-SHT-AMT > +0                                    CL*15
00617            MOVE EC-ST-OVR-SHT-AMT   TO SOVSTOLO.                     CL*15
00618                                                                      CL*15
00619      IF EC-ST-TOL-REFUND  NUMERIC                                 EL6312
00620         IF EC-ST-TOL-REFUND  NOT = ZEROS                          EL6312
00621            MOVE EC-ST-TOL-REFUND    TO SCANTOLO.                  EL6312
00622                                                                   EL6312
00623      IF EC-ST-TOL-PREM-PCT NUMERIC                                   CL**6
00624         IF EC-ST-TOL-PREM-PCT GREATER THAN +0                        CL**6
00625            MOVE EC-ST-TOL-PREM-PCT                                   CL**6
00626                                  TO SISSPCTO.                        CL**6
00627                                                                      CL**6
00628      IF EC-ST-TOL-REFUND-PCT NUMERIC                                 CL**6
00629         IF EC-ST-TOL-REFUND-PCT GREATER THAN +0                      CL**6
00630            MOVE EC-ST-TOL-REFUND-PCT                                 CL**6
00631                                  TO SCANPCTO.                        CL**6
00632                                                                      CL*15
00633      IF EC-ST-OVR-SHT-PCT NUMERIC                                    CL*15
00634         IF EC-ST-OVR-SHT-PCT > +0                                    CL*15
00635            MOVE EC-ST-OVR-SHT-PCT TO SOVSPCTO.                       CL*15
00636                                                                   EL6312
00637      IF EC-ST-LF-REM-TERM-CALC   = '1'                            EL6312
00638        MOVE 'AFTER 15TH'   TO SRTERMO                                CL**9
00639       ELSE                                                           CL**9
00640        IF EC-ST-LF-REM-TERM-CALC   = '2'                             CL**9
00641          MOVE 'FIRST HALF'   TO SRTERMO                              CL**9
00642         ELSE                                                      EL6312
00643          IF EC-ST-LF-REM-TERM-CALC   = '3'                           CL**9
00644            MOVE 'ON 1ST DAY'     TO SRTERMO                          CL**9
00645           ELSE                                                       CL**9
00646            IF EC-ST-LF-REM-TERM-CALC   = '4'                         CL**9
00647              MOVE 'ON LAST DAY'   TO SRTERMO                         CL**9
00648           ELSE                                                       CL**9
00649            IF EC-ST-LF-REM-TERM-CALC   = '5'                         CL**9
00650              MOVE 'NO DAYS'       TO SRTERMO                         CL**9
00651           ELSE                                                       CL**9
00652            IF EC-ST-LF-REM-TERM-CALC   = '6'                         CL**9
00653              MOVE 'AFTER 14TH'    TO SRTERMO                         CL**9
00654           ELSE                                                       CL**9
00655            IF EC-ST-LF-REM-TERM-CALC   = '7'                         CL**9
00656              MOVE 'AFTER 16TH'    TO SRTERMO.                        CL**9
00657                                                                   EL6312
00658      EVALUATE TRUE                                                   CL*14
00659         WHEN EC-ST-LF-REFD-BY-R78                                    CL*14
00660            MOVE 'RULE 78'           TO SRMETHO                       CL*14
00661         WHEN EC-ST-LF-REFD-BY-PRO-RATA                               CL*14
00662            MOVE 'PRO-RATA'          TO SRMETHO                    EL6312
00663         WHEN EC-ST-LF-REFD-AS-CALIF                                  CL*14
00664            MOVE 'CALIFORNIA'        TO SRMETHO                       CL*14
00665         WHEN EC-ST-LF-REFD-AS-TEXAS                                  CL*14
00666            MOVE 'IRREG'             TO SRMETHO                       CL*14
00667         WHEN EC-ST-LF-REFD-IS-NET-PAY                                CL*14
00668            MOVE 'NET PAY'           TO SRMETHO                       CL*14
00669         WHEN EC-ST-LF-REFD-ANTICIPATION                              CL*14
00670            MOVE 'ANTICIPAT'         TO SRMETHO                       CL*14
00671         WHEN EC-ST-LF-REFD-IS-MEAN                                   CL*14
00672            MOVE 'MEAN'              TO SRMETHO                       CL*14
00673         WHEN EC-ST-LF-REFD-IS-SUM-OF-DIGIT                           CL*14
00674            MOVE 'SUM DIGIT'         TO SRMETHO                       CL*14
00675         WHEN OTHER                                                   CL*14
00676            MOVE SPACES              TO SRMETHO                       CL*14
00677      END-EVALUATE.                                                   CL*14
00678                                                                   EL6312
00679      IF EC-AM-LF-TOL-PREM    NUMERIC                                 CL**6
00680         IF EC-AM-LF-TOL-PREM  NOT = ZEROS                            CL**6
00681            MOVE EC-AM-LF-TOL-PREM   TO AISSTOLO.                     CL**6
00682                                                                   EL6312
00683      IF EC-AM-LF-TOL-REFUND  NUMERIC                                 CL**6
00684         IF EC-AM-LF-TOL-REFUND  NOT = ZEROS                          CL**6
00685            MOVE EC-AM-LF-TOL-REFUND TO ACANTOLO.                     CL**6
00686                                                                      CL*15
00687      IF EC-AM-LF-OVR-SHT-AMT NUMERIC                                 CL*15
00688         IF EC-AM-LF-OVR-SHT-AMT > +0                                 CL*15
00689            MOVE EC-AM-LF-OVR-SHT-AMT TO AOVSTOLO.                    CL*15
00690                                                                   EL6312
00691      EVALUATE TRUE                                                   CL*14
00692         WHEN EC-AM-LF-REFD-BY-R78                                    CL*14
00693            MOVE 'RULE 78'        TO ARMETHO                          CL*14
00694         WHEN EC-AM-LF-REFD-BY-PRO-RATA                               CL*14
00695            MOVE 'PRO-RATA'       TO ARMETHO                          CL*14
00696         WHEN EC-AM-LF-REFD-AS-TEXAS                                  CL*14
00697            MOVE 'IRREG'          TO ARMETHO                          CL*14
00698         WHEN EC-AM-LF-REFD-IS-NET-PAY                                CL*14
00699            MOVE 'NET PAY'        TO ARMETHO                          CL*14
00700         WHEN EC-AM-LF-REFD-ANTICIPATION                              CL*14
00701            MOVE 'ANTICIPAT'      TO ARMETHO                          CL*14
00702         WHEN EC-AM-LF-REFD-IS-MEAN                                   CL*14
00703            MOVE 'MEAN'           TO ARMETHO                          CL*14
00704         WHEN EC-AM-LF-REFD-IS-SUM-OF-DIGIT                           CL*14
00705            MOVE 'SUM DIGIT'      TO ARMETHO                          CL*14
00706         WHEN OTHER                                                   CL*14
00707            MOVE SPACES           TO ARMETHO                          CL*14
00708      END-EVALUATE.                                                   CL*14
00709                                                                   EL6312
00710      MOVE EC-AM-BEN-I-G-CD       TO AIGO.                            CL**2
00711      MOVE EC-AM-CLASS-CD         TO ACLASSO.                         CL**2
00712      MOVE EC-AM-LF-DEVIATION     TO ADEVO.                           CL**2
00713      MOVE EC-AM-LF-DEV-PERCENT   TO ADEVPCTO.                        CL**2
00714      MOVE EC-AM-PHONE-NO         TO WS-PHONE.                        CL**2
00715      MOVE WS-PHONE-NUM           TO PHONEO.                          CL**2
00716      INSPECT PHONEO CONVERTING ' ' TO '-'.                           CL*12
00717                                                                   EL6312
00718      IF EC-AM-EXPIRATION-DT NOT = HIGH-VALUES                     EL6312
00719         MOVE EC-AM-EXPIRATION-DT    TO DC-BIN-DATE-1              EL6312
00720         MOVE SPACE                  TO DC-OPTION-CODE             EL6312
00721         PERFORM 9700-DATE-LINK                                    EL6312
00722         IF NO-CONVERSION-ERROR                                    EL6312
00723            MOVE DC-GREG-DATE-1-EDIT TO AEXPDTEO                   EL6312
00724            ELSE                                                   EL6312
00725            NEXT SENTENCE                                          EL6312
00726         ELSE                                                      EL6312
00727         MOVE '99/99/99'          TO AEXPDTEO.                     EL6312
00728                                                                   EL6312
00729      MOVE EC-AM-EFFECTIVE-DT     TO DC-BIN-DATE-1.                   CL**7
00730      MOVE SPACE                  TO DC-OPTION-CODE.                  CL**7
00731      PERFORM 9700-DATE-LINK.                                         CL**7
00732      IF NO-CONVERSION-ERROR                                       EL6312
00733         MOVE DC-GREG-DATE-1-EDIT TO AEFFDTO.                      EL6312
00734                                                                   EL6312
00735      IF EC-AM-LF-MAX-ATT-AGE  NUMERIC                             EL6312
00736         IF EC-AM-LF-MAX-ATT-AGE  NOT = ZEROS                      EL6312
00737            MOVE EC-AM-LF-MAX-ATT-AGE  TO AMAAGEO.                 EL6312
00738                                                                   EL6312
00739      IF EC-AM-LF-MAX-AGE  NUMERIC                                 EL6312
00740         IF EC-AM-LF-MAX-AGE  NOT = ZEROS                          EL6312
00741            MOVE EC-AM-LF-MAX-AGE      TO AMAGEO.                  EL6312
00742                                                                   EL6312
00743      IF EC-AM-LF-MAX-TERM   NUMERIC                               EL6312
00744         IF EC-AM-LF-MAX-TERM  NOT = ZEROS                         EL6312
00745            MOVE EC-AM-LF-MAX-TERM   TO AMTERMO.                   EL6312
00746                                                                   EL6312
00747      IF EC-AM-LF-MAX-TOT-BEN NUMERIC                              EL6312
00748         IF EC-AM-LF-MAX-TOT-BEN  NOT = ZEROS                      EL6312
00749            MOVE EC-AM-LF-MAX-TOT-BEN  TO AMTBENO.                 EL6312
00750                                                                   EL6312
00751      IF EC-RT-LF-MAX-ATT-AGE  NUMERIC                             EL6312
00752         IF EC-RT-LF-MAX-ATT-AGE  NOT = ZEROS                      EL6312
00753            MOVE EC-RT-LF-MAX-ATT-AGE  TO RMAAGEO.                 EL6312
00754                                                                   EL6312
00755      IF EC-RT-LF-MAX-AGE  NUMERIC                                 EL6312
00756         IF EC-RT-LF-MAX-AGE  NOT = ZEROS                          EL6312
00757            MOVE EC-RT-LF-MAX-AGE      TO RMAGEO.                  EL6312
00758                                                                   EL6312
00759      IF EC-RT-LF-MAX-TERM   NUMERIC                               EL6312
00760         IF EC-RT-LF-MAX-TERM NOT = ZEROS                          EL6312
00761            MOVE EC-RT-LF-MAX-TERM   TO RMTERMO.                   EL6312
00762                                                                   EL6312
00763      IF EC-RT-LF-MAX-TOT-BEN NUMERIC                              EL6312
00764         IF EC-RT-LF-MAX-TOT-BEN NOT = ZEROS                       EL6312
00765            MOVE EC-RT-LF-MAX-TOT-BEN  TO RMTBENO.                 EL6312
00766                                                                   EL6312
00767      IF EC-RT-LF-RATE   NUMERIC                                   EL6312
00768          IF  EC-CF-LF-OUTSTANDING-BAL                             EL6312
00769              MOVE ZEROS           TO RRATESO                      EL6312
00770              MOVE EC-RT-LF-RATE   TO OBRATEO                      EL6312
00771              MOVE ' OB RATE'      TO OBHEADO                         CL**7
00772          ELSE                                                     EL6312
00773              MOVE EC-RT-LF-RATE   TO RRATESO                         CL**7
00774              COMPUTE WS-DEV-RT = EC-RT-LF-RATE *                     CL**7
00775                                  EC-AM-LF-DEV-PERCENT                CL**7
00776              MOVE WS-DEV-RT       TO DRATESO.                        CL**7
00777                                                                   EL6312
00778      MOVE EC-RT-LF-NSP-ST        TO RNSPSTO.                      EL6312
00779                                                                   EL6312
00780      IF EC-RT-LF-NSP-ST NOT = SPACES                              EL6312
00781         IF EC-RT-LF-NSP-RATE   NUMERIC                            EL6312
00782            MOVE EC-RT-LF-NSP-RATE       TO RNSPRTO.               EL6312
00783                                                                   EL6312
00784      IF EC-CM-LF-POLICY-IS-ACTIVE                                 EL6312
00785         MOVE 'ACTIVE'            TO CTSTO                         EL6312
00786         ELSE                                                      EL6312
00787         IF EC-CM-LF-LUMP-SUM-DISAB                                EL6312
00788            MOVE 'LUMP SUM'    TO CTSTO                            EL6312
00789            ELSE                                                   EL6312
00790            IF EC-CM-LF-DEATH-CLAIM-APPLIED                        EL6312
00791               MOVE 'DEATH'    TO CTSTO                            EL6312
00792               MOVE 'DEATH-DT' TO CTDATEHO                         EL6312
00793               ELSE                                                EL6312
00794               IF EC-CM-LF-CANCEL-APPLIED                          EL6312
00795                  MOVE 'CANCELLED'   TO CTSTO                      EL6312
00796                  MOVE 'CANCEL-DT'   TO CTDATEHO.                  EL6312
00797                                                                   EL6312
00798      IF EC-CM-LF-PRIOR-REFUND  NUMERIC                            EL6312
00799         IF EC-CM-LF-PRIOR-REFUND NOT = ZEROS                      EL6312
00800            MOVE EC-CM-LF-PRIOR-REFUND    TO CTRFUNDO.             EL6312
00801                                                                   EL6312
00802      IF EC-CM-LF-DEATH-CLAIM-APPLIED                              EL6312
00803         MOVE EC-CM-DEATH-DT         TO DC-BIN-DATE-1              EL6312
00804         MOVE SPACE                  TO DC-OPTION-CODE             EL6312
00805         PERFORM 9700-DATE-LINK                                    EL6312
00806         IF NO-CONVERSION-ERROR                                    EL6312
00807            MOVE DC-GREG-DATE-1-EDIT TO CTDATEO.                   EL6312
00808                                                                   EL6312
00809      IF EC-CM-LF-CANCEL-APPLIED                                   EL6312
00810         MOVE EC-CM-LF-CANCEL-DT     TO DC-BIN-DATE-1              EL6312
00811         MOVE SPACE                  TO DC-OPTION-CODE             EL6312
00812         PERFORM 9700-DATE-LINK                                    EL6312
00813         IF NO-CONVERSION-ERROR                                    EL6312
00814            MOVE DC-GREG-DATE-1-EDIT TO CTDATEO.                   EL6312
00815  EJECT                                                               CL**4
00816  8100-SEND-INITIAL-MAP.                                           EL6312
00817      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL6312
00818      MOVE '5'                   TO DC-OPTION-CODE.                EL6312
00819      PERFORM 9700-DATE-LINK.                                      EL6312
00820      MOVE DC-GREG-DATE-1-EDIT   TO  DATEO.                        EL6312
00821      MOVE EIBTIME                TO TIME-IN.                      EL6312
00822      MOVE TIME-OUT               TO TIMEO.                        EL6312
00823      MOVE -1                     TO PFENTERL                      EL6312
00824      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL6312
00825      EXEC CICS SEND                                               EL6312
00826          MAP      (MAP-NAME)                                      EL6312
00827          MAPSET   (MAPSET-NAME)                                   EL6312
00828          FROM     (EL631EO)                                       EL6312
00829          ERASE                                                    EL6312
00830          CURSOR                                                   EL6312
00831      END-EXEC.                                                       CL**7
00832                                                                      CL**7
00833      GO TO 9100-RETURN-TRAN.                                      EL6312
00834                                                                   EL6312
00835  8200-SEND-DATAONLY.                                              EL6312
00836      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL6312
00837      MOVE '5'                   TO DC-OPTION-CODE.                EL6312
00838      PERFORM 9700-DATE-LINK.                                      EL6312
00839      MOVE DC-GREG-DATE-1-EDIT   TO  DATEO.                        EL6312
00840      MOVE EIBTIME                TO TIME-IN.                      EL6312
00841      MOVE TIME-OUT               TO TIMEO.                        EL6312
00842      MOVE -1                     TO PFENTERL                      EL6312
00843      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O                      EL6312
00844      EXEC CICS SEND                                               EL6312
00845          MAP      (MAP-NAME)                                      EL6312
00846          MAPSET   (MAPSET-NAME)                                   EL6312
00847          FROM     (EL631EO)                                       EL6312
00848          DATAONLY                                                 EL6312
00849          ERASEAUP                                                 EL6312
00850          CURSOR                                                   EL6312
00851      END-EXEC.                                                       CL**7
00852                                                                      CL**7
00853      GO TO 9100-RETURN-TRAN.                                      EL6312
00854                                                                   EL6312
00855  8300-SEND-TEXT.                                                  EL6312
00856      EXEC CICS SEND TEXT                                          EL6312
00857          FROM     (LOGOFF-TEXT)                                   EL6312
00858          LENGTH   (LOGOFF-LENGTH)                                 EL6312
00859          ERASE                                                    EL6312
00860          FREEKB                                                   EL6312
00861      END-EXEC.                                                       CL**7
00862                                                                      CL**7
00863      EXEC CICS RETURN                                             EL6312
00864      END-EXEC.                                                       CL**7
00865                                                                   EL6312
00866  8800-UNAUTHORIZED-ACCESS.                                        EL6312
00867      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL6312
00868      GO TO 8300-SEND-TEXT.                                        EL6312
00869                                                                   EL6312
00870  8810-PF23.                                                       EL6312
00871      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL6312
00872      MOVE XCTL-005               TO PGM-NAME.                     EL6312
00873      GO TO 9300-XCTL.                                             EL6312
00874  9000-RETURN-CICS.                                                EL6312
00875      EXEC CICS RETURN                                             EL6312
00876      END-EXEC.                                                       CL**7
00877                                                                   EL6312
00878  9100-RETURN-TRAN.                                                EL6312
00879      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL6312
00880      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         EL6312
00881                                                                      CL**7
00882      EXEC CICS RETURN                                             EL6312
00883          TRANSID    (TRANS-ID)                                    EL6312
00884          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6312
00885          LENGTH     (1300)                                           CL**3
00886      END-EXEC.                                                       CL**7
00887                                                                   EL6312
00888  9200-RETURN-MAIN-MENU.                                           EL6312
00889      MOVE XCTL-626               TO PGM-NAME.                     EL6312
00890      GO TO 9300-XCTL.                                             EL6312
00891                                                                   EL6312
00892  9300-XCTL.                                                       EL6312
00893      EXEC CICS XCTL                                               EL6312
00894          PROGRAM    (PGM-NAME)                                    EL6312
00895          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6312
00896          LENGTH     (1300)                                           CL**3
00897      END-EXEC.                                                       CL**7
00898                                                                   EL6312
00899  9400-CLEAR.                                                      EL6312
00900      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL6312
00901                                                                      CL**7
00902      EXEC CICS XCTL                                               EL6312
00903          PROGRAM    (PGM-NAME)                                    EL6312
00904          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6312
00905          LENGTH     (1300)                                           CL**3
00906      END-EXEC.                                                       CL**7
00907                                                                   EL6312
00908  9500-PF12.                                                       EL6312
00909      MOVE XCTL-010               TO PGM-NAME.                     EL6312
00910      GO TO 9300-XCTL.                                             EL6312
00911                                                                   EL6312
00912  9600-PGMID-ERROR.                                                EL6312
00913      EXEC CICS HANDLE CONDITION                                   EL6312
00914          PGMIDERR    (8300-SEND-TEXT)                             EL6312
00915      END-EXEC.                                                       CL**7
00916                                                                      CL**7
00917      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL6312
00918      MOVE ' '                    TO PI-ENTRY-CD-1.                EL6312
00919      MOVE XCTL-005               TO PGM-NAME.                     EL6312
00920      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL6312
00921      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL6312
00922      GO TO 9300-XCTL.                                             EL6312
00923                                                                   EL6312
00924  9700-DATE-LINK.                                                  EL6312
00925      MOVE LINK-ELDATCV           TO PGM-NAME.                        CL**7
00926                                                                      CL**7
00927      EXEC CICS LINK                                               EL6312
00928          PROGRAM    (PGM-NAME)                                    EL6312
00929          COMMAREA   (DATE-CONVERSION-DATA)                        EL6312
00930          LENGTH     (DC-COMM-LENGTH)                              EL6312
00931      END-EXEC.                                                       CL**7
00932                                                                   EL6312
00933  9900-ERROR-FORMAT.                                               EL6312
00934      IF NOT EMI-ERRORS-COMPLETE                                   EL6312
00935          MOVE LINK-001           TO PGM-NAME                      EL6312
00936          EXEC CICS LINK                                           EL6312
00937              PROGRAM    (PGM-NAME)                                EL6312
00938              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL6312
00939              LENGTH     (EMI-COMM-LENGTH)                         EL6312
00940          END-EXEC.                                                   CL**7
00941                                                                      CL**7
00942  9900-EXIT.                                                       EL6312
00943      EXIT.                                                        EL6312
00944                                                                   EL6312
00945  9990-ABEND.                                                      EL6312
00946      MOVE LINK-004               TO PGM-NAME.                     EL6312
00947      MOVE DFHEIBLK               TO EMI-LINE1.                       CL**7
00948      EXEC CICS LINK                                               EL6312
00949          PROGRAM   (PGM-NAME)                                     EL6312
00950          COMMAREA  (EMI-LINE1)                                    EL6312
00951          LENGTH    (72)                                           EL6312
00952      END-EXEC.                                                       CL**7
00953                                                                      CL**7
00954      GO TO 8200-SEND-DATAONLY.                                    EL6312
00955                                                                      CL**7
00956      GOBACK.                                                      EL6312
00957                                                                   EL6312
00958  9995-SECURITY-VIOLATION.                                         EL6312
00959                              COPY ELCSCTP.                        EL6312
00960                                                                   EL6312
00961  9995-EXIT.                                                       EL6312
00962      EXIT.                                                        EL6312
00963                                                                   EL6312
