00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   EL1274
00003  PROGRAM-ID.                 EL1274.                                 LV009
00004 *              PROGRAM CONVERTED BY                                  CL**8
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**8
00006 *              CONVERSION DATE 11/21/95 10:07:32.                    CL**8
00007 *                            VMOD=2.008.                             CL**8
00008 *                                                                 EL1274
00008 *                                                                 EL1274
00009 *AUTHOR.    LOGIC, INC.                                              CL**8
00010 *           DALLAS, TEXAS.                                           CL**8
00011                                                                   EL1274
00012 *DATE-COMPILED.                                                      CL**8
00013                                                                   EL1274
00014 *SECURITY.   *****************************************************   CL**8
00015 *            *                                                   *   CL**8
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**8
00017 *            *                                                   *   CL**8
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**8
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**8
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**8
00021 *            *                                                   *   CL**8
00022 *            *****************************************************   CL**8
00023                                                                   EL1274
00024 *REMARKS.  TRANSACTION - EXX4                                        CL**3
00025                                                                   EL1274
00026 *        CERT PROFILE DISPLAY PROGRAM.                               CL**3
00027                                                                   EL1274
00028 *    SCREENS     - EL127D - CERTIFICATE PROFILE                      CL**3
00029                                                                   EL1274
00030 *    ENTERED BY  - EL1273 - CERTIFICATE UPDATE                       CL**3
00031 *                - EL1275 - CERTIFICATE COVERAGES                    CL**3
00032                                                                   EL1274
00033 *    EXIT TO     - CALLING PROGRAM                                   CL**3
00034                                                                   EL1274
00035 *    INPUT FILE  - ELCERT - CERTIFICATE INFORCE FILE                 CL**3
00036 *                  ELCNTL - CONTROL FILE                             CL**3
00037                                                                   EL1274
00038 *    NARRATIVE   - PROGRAM USES THE KEY TO THE CERTIFICATE MASTER    CL**3
00039 *                  PASSED IN THE COMMAREA TO DISPLAY THE             CL**3
00040 *                  CERTIFICATE AND RETURN WITH THE TRANSACTION       CL**3
00041 *                  OF THE CALLING PROGRAM.                           CL**3
101201******************************************************************
101201*                   C H A N G E   L O G
101201*
101201* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101201*-----------------------------------------------------------------
101201*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101201* EFFECTIVE    NUMBER
101201*-----------------------------------------------------------------
101201* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
101509* 101509    2008100900003  AJRA  CALL NEW CERT NOTE SCREEN
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
101201******************************************************************

00042                                                                   EL1274
00043      EJECT                                                        EL1274
00044  ENVIRONMENT DIVISION.                                            EL1274
00045                                                                   EL1274
00046  DATA DIVISION.                                                   EL1274
00047                                                                   EL1274
00048  WORKING-STORAGE SECTION.                                         EL1274
00049                                                                   EL1274
00050  77  FILLER  PIC X(32)  VALUE '********************************'. EL1274
00051  77  FILLER  PIC X(32)  VALUE '*   EL1274 WORKING STORAGE     *'. EL1274
00052  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.008 *********'.    CL**8
00053                                                                   EL1274
00054  01  WS-DATE-AREA.                                                EL1274
00055      05  SAVE-DATE                   PIC X(8)     VALUE SPACES.   EL1274
00056      05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.   EL1274
00057                                                                   EL1274
00058                                                                   EL1274
00059  01  FILLER                          COMP-3.                      EL1274
00060      05  WS-ERROR-COUNT              PIC S9(3)    VALUE ZERO.     EL1274
00061      05  WS-TIME-WORK                PIC S9(7)    VALUE ZERO.     EL1274
00062      05  WS-TIME                     REDEFINES                    EL1274
00063          WS-TIME-WORK                PIC S9(3)V9(4).              EL1274
00064                                                                   EL1274
00065  01  FILLER.                                                      EL1274
00066      05  WS-ACCOUNT.                                              EL1274
00067          10  FILLER                  PIC X(4).                    EL1274
00068          10  WS-ACCT                 PIC X(6).                    EL1274
00069                                                                   EL1274
00070      05  WS-CERTIFICATE-KEY.                                      EL1274
00071          10  WS-CK-COMPANY-CD        PIC X.                       EL1274
00072          10  WS-CK-CARRIER           PIC X.                       EL1274
00073          10  WS-CK-GROUPING          PIC X(6).                    EL1274
00074          10  WS-CK-STATE             PIC XX.                      EL1274
00075          10  WS-CK-ACCOUNT           PIC X(10).                   EL1274
00076          10  WS-CK-CERT-EFF-DT       PIC XX.                      EL1274
00077          10  WS-CK-CERT-NO.                                       EL1274
00078              15  WS-CK-CERT-PRIME    PIC X(10).                   EL1274
00079              15  WS-CK-CERT-SFX      PIC X.                       EL1274
00080                                                                   EL1274
00081      05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL1274S'.EL1274
00082      05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL127D'. EL1274
00083      05  WS-MAP-NUMBER               PIC X(4)     VALUE '127D'.   EL1274
00084                                                                   EL1274
00085      05  WS-PROGRAM-ID               PIC X(8)     VALUE 'EL1274'. EL1274
00086                                                                   EL1274
00087      05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXX4'.   EL1274
00088      05  EL001                       PIC X(8)     VALUE 'EL001'.  EL1274
00089      05  EL004                       PIC X(8)     VALUE 'EL004'.  EL1274
00090      05  EL005                       PIC X(8)     VALUE 'EL005'.  EL1274
00091      05  EL010                       PIC X(8)     VALUE 'EL010'.  EL1274
00092      05  ELDATCV                     PIC X(8)     VALUE 'ELDATCV'.EL1274
00093                                                                   EL1274
00094      05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'. EL1274
00095      05  WS-CERTIFICATE-MASTER-DSID  PIC X(8)     VALUE 'ELCERT'. EL1274
00096                                                                   EL1274
00097      EJECT                                                        EL1274
00098      05  ERROR-MESSAGES.                                          EL1274
00099          10  ER-0008                 PIC X(4)    VALUE '0008'.    EL1274
00100          10  ER-0029                 PIC X(4)    VALUE '0029'.    EL1274
00101          10  ER-0142                 PIC X(4)    VALUE '0142'.    EL1274
00102                                                                   EL1274
00103      EJECT                                                        EL1274
00104                                      COPY ELCINTF.                   CL**6
00105      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                 EL1274
00106          16  FILLER              PIC X(314).                      EL1274
00107          16  PI-1ST-TIME-SW      PIC X.                           EL1274
00108          16  FILLER              PIC XX.                          EL1274
00109          16  PI-PEND-SW          PIC X.                           EL1274
00110          16  FILLER              PIC X(322).                         CL**8
00111                                                                   EL1274
00112      EJECT                                                        EL1274
00113                                      COPY ELCDATE.                   CL**6
00114      EJECT                                                        EL1274
00115                                      COPY EL1274S.                   CL**6
00116      EJECT                                                        EL1274
00117                                      COPY ELCEMIB.                   CL**6
00118                                                                   EL1274
00119      EJECT                                                        EL1274
00120                                      COPY ELCLOGOF.                  CL**6
00121                                                                   EL1274
00122      EJECT                                                        EL1274
00123                                      COPY ELCATTR.                   CL**6
00124                                                                   EL1274
00125      EJECT                                                        EL1274
00126                                      COPY ELCAID.                    CL**6
00127                                                                   EL1274
00128  01  FILLER  REDEFINES DFHAID.                                       CL**6
00129      05  FILLER                      PIC X(8).                    EL1274
00130      05  PF-VALUES                   PIC X                        EL1274
00131          OCCURS 24 TIMES.                                         EL1274
00132                                                                   EL1274
00133  LINKAGE SECTION.                                                 EL1274
00134                                                                   EL1274
00135  01  DFHCOMMAREA                     PIC X(1024).                 EL1274
00136                                                                   EL1274
00137 *01 PARMLIST                         COMP SYNC.                      CL**8
00138 *    05  FILLER                      PIC S9(9).                      CL**8
00139 *    05  ELCERT-POINTER              PIC S9(9).                      CL**8
00140 *    05  ELCNTL-POINTER              PIC S9(9).                      CL**8
00141                                                                   EL1274
00142      EJECT                                                        EL1274
00143                                      COPY ELCCERT.                   CL**6
00144                                                                   EL1274
00145      EJECT                                                        EL1274
00146                                      COPY ELCCNTL.                   CL**6
00147                                                                   EL1274
00148      EJECT                                                        EL1274
00149  PROCEDURE DIVISION.                                              EL1274
00150                                                                   EL1274
00151      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL1274
00152      MOVE '5'                    TO DC-OPTION-CODE.               EL1274
00153      PERFORM 8500-DATE-CONVERSION.                                EL1274
00154      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL1274
00155      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL1274
00156                                                                   EL1274
00157                                                                   EL1274
00158 *    NOTE ******************************************************* EL1274
00159 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL1274
00160 *         *  FROM ANOTHER MODULE.                               * EL1274
00161 *         *******************************************************.EL1274
00162                                                                   EL1274
00163      IF EIBCALEN NOT GREATER THAN ZERO                            EL1274
00164          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL1274
00165          GO TO 8300-SEND-TEXT.                                    EL1274
00166                                                                   EL1274
00167      EXEC CICS HANDLE CONDITION                                   EL1274
00168          ERROR  (9990-ERROR)                                      EL1274
00169          END-EXEC.                                                EL1274
00170                                                                   EL1274
00171      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL1274
00172                                                                   EL1274
00173 *    IF PI-CALLING-PROGRAM NOT = WS-PROGRAM-ID                    EL1274
00174 *        IF PI-RETURN-TO-PROGRAM NOT = WS-PROGRAM-ID              EL1274
00175 *            MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL1274
00176 *            MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL1274
00177 *            MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL1274
00178 *            MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL1274
00179 *            MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL1274
00180 *            MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL1274
00181 *            MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL1274
00182 *            MOVE WS-PROGRAM-ID        TO  PI-CALLING-PROGRAM     EL1274
00183 *        ELSE                                                     EL1274
00184 *            MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL1274
00185 *            MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL1274
00186 *            MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL1274
00187 *            MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL1274
00188 *            MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL1274
00189 *            MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL1274
00190 *            MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL1274
00191 *            MOVE SPACES               TO  PI-SAVED-PROGRAM-6     EL1274
00192 *    ELSE                                                         EL1274
00193 *        GO TO 0100-RECEIVE.                                      EL1274
00194                                                                   EL1274
00195      IF PI-1ST-TIME-SW  = '1'                                     EL1274
00196          MOVE ' '                TO  PI-1ST-TIME-SW               EL1274
00197          GO TO 0100-RECEIVE.                                      EL1274
00198                                                                   EL1274
00199      MOVE '1'                    TO  PI-1ST-TIME-SW.              EL1274
00200      GO TO 1000-DISPLAY-CERTIFICATE.                              EL1274
00201                                                                   EL1274
00202      EJECT                                                        EL1274
00203  0100-RECEIVE.                                                    EL1274
00204                                                                   EL1274
00205      IF EIBAID = DFHCLEAR                                         EL1274
00206          GO TO 9400-CLEAR.                                        EL1274
00207                                                                   EL1274
00208      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL1274
00209          MOVE ER-0008            TO  EMI-ERROR                    EL1274
00210          PERFORM 9900-ERROR-FORMAT                                EL1274
00211          MOVE -1                 TO  DPFKEYL                      EL1274
00212          GO TO 8200-SEND-DATAONLY.                                EL1274
00213                                                                   EL1274
00214      EXEC CICS RECEIVE                                            EL1274
00215          MAPSET (WS-MAPSET-NAME)                                  EL1274
00216          MAP    (WS-MAP-NAME)                                     EL1274
00217          INTO   (EL127DI)                                         EL1274
00218          END-EXEC.                                                EL1274
00219                                                                   EL1274
00220      IF DPFKEYL = 0                                               EL1274
00221          GO TO 0200-CHECK-PFKEYS.                                 EL1274
00222                                                                   EL1274
00223      IF EIBAID NOT = DFHENTER                                     EL1274
00224          MOVE ER-0008            TO  EMI-ERROR                    EL1274
00225          MOVE -1                 TO  DPFKEYL                      EL1274
00226          GO TO 0300-INPUT-ERROR.                                  EL1274
00227                                                                   EL1274
00228      IF (DPFKEYI NUMERIC) AND (DPFKEYI GREATER 0 AND LESS 25)        CL**3
00229              MOVE PF-VALUES (DPFKEYI)    TO  EIBAID               EL1274
00230      ELSE                                                         EL1274
00231          MOVE ER-0029                    TO  EMI-ERROR            EL1274
00232          GO TO 0300-INPUT-ERROR.                                  EL1274
00233                                                                   EL1274
00234  0200-CHECK-PFKEYS.                                               EL1274
00235      IF EIBAID = DFHPF3                                           EL1274
00236         MOVE '1'                 TO  PI-1ST-TIME-SW               EL1274
00237         MOVE 'EL1273'            TO  WS-PROGRAM-ID                EL1274
00238         GO TO 9300-XCTL.                                          EL1274
00239                                                                   EL1274
00240      IF EIBAID = DFHPF4                                           EL1274
00241          MOVE 'EL1275'           TO  WS-PROGRAM-ID                EL1274
00242          GO TO 9300-XCTL.                                         EL1274
00243                                                                   EL1274
00244      IF EIBAID = DFHPF5                                           EL1274
00245          IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')               CL**7
00246              MOVE ER-0029            TO  EMI-ERROR                   CL**7
00247              MOVE -1                 TO  DPFKEYL                     CL**7
00248              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**7
00249              MOVE '1'                TO  PI-1ST-TIME-SW              CL**7
00250              GO TO 8200-SEND-DATAONLY-CONT                           CL**7
00251          ELSE                                                        CL**7
00252              IF PI-COMPANY-ID = 'DMD'                                CL**8
00253                  MOVE 'EL401DMD'     TO  WS-PROGRAM-ID               CL**8
00254                ELSE                                                  CL**8
101509*00255                  MOVE 'EL1276'       TO  WS-PROGRAM-ID               CL**8
101509                 MOVE 'EL1279'       TO  WS-PROGRAM-ID
00256              END-IF                                                  CL**8
00257              GO TO 9300-XCTL.                                        CL**7
00258                                                                   EL1274
00259      IF PI-MAIL-YES                                               EL1274
00260          IF EIBAID = DFHPF6                                       EL1274
00261              IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')           CL**7
00262                  MOVE ER-0029        TO  EMI-ERROR                   CL**7
00263                  MOVE -1             TO  DPFKEYL                     CL**7
00264                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**7
00265                  MOVE '1'            TO  PI-1ST-TIME-SW              CL**7
00266                  GO TO 8200-SEND-DATAONLY-CONT                       CL**7
00267              ELSE                                                    CL**7
00268                  MOVE 'EL1277'       TO  WS-PROGRAM-ID               CL**7
00269                  GO TO 9300-XCTL.                                    CL**7
00270                                                                   EL1274
00271      IF EIBAID = DFHPF12                                          EL1274
00272          MOVE 'EL010   '         TO  WS-PROGRAM-ID                EL1274
00273          GO TO 9300-XCTL.                                         EL1274
00274                                                                   EL1274
00275      IF EIBAID = DFHPF23                                          EL1274
00276          GO TO 9000-RETURN-CICS.                                  EL1274
00277                                                                   EL1274
00278      IF EIBAID = DFHPF24                                          EL1274
00279          MOVE 'EL126   '         TO  WS-PROGRAM-ID                EL1274
00280          GO TO 9300-XCTL.                                         EL1274
00281                                                                   EL1274
00282      MOVE ER-0029                TO  EMI-ERROR.                   EL1274
00283                                                                   EL1274
00284  0300-INPUT-ERROR.                                                EL1274
00285      PERFORM 9900-ERROR-FORMAT.                                   EL1274
00286      MOVE AL-UNBON               TO  DPFKEYA.                     EL1274
00287      MOVE -1                     TO  DPFKEYL.                     EL1274
00288      MOVE '1'                    TO  PI-1ST-TIME-SW.                 CL**2
00289      GO TO 8200-SEND-DATAONLY-CONT.                                  CL**2
00290                                                                   EL1274
00291      EJECT                                                        EL1274
00292  1000-DISPLAY-CERTIFICATE.                                        EL1274
00293 ******************************************************************EL1274
00294 *               READ THE REQUESTED CERT RECORD                   *EL1274
00295 ******************************************************************EL1274
00296                                                                   EL1274
00297      MOVE PI-COMPANY-CD          TO  WS-CK-COMPANY-CD.            EL1274
00298      MOVE PI-CARRIER             TO  WS-CK-CARRIER.               EL1274
00299      MOVE PI-GROUPING            TO  WS-CK-GROUPING.              EL1274
00300      MOVE PI-STATE               TO  WS-CK-STATE.                 EL1274
00301      MOVE PI-ACCOUNT             TO  WS-CK-ACCOUNT.               EL1274
00302      MOVE PI-CERT-NO             TO  WS-CK-CERT-NO.               EL1274
00303      MOVE PI-CERT-EFF-DT         TO  WS-CK-CERT-EFF-DT.           EL1274
00304                                                                   EL1274
00305      EXEC CICS HANDLE CONDITION                                   EL1274
00306          NOTFND (8880-NOT-FOUND)                                  EL1274
00307          END-EXEC.                                                EL1274
00308                                                                   EL1274
00309      EXEC CICS READ                                               EL1274
00310          DATASET (WS-CERTIFICATE-MASTER-DSID)                     EL1274
00311          RIDFLD  (WS-CERTIFICATE-KEY)                             EL1274
00312          SET     (ADDRESS OF CERTIFICATE-MASTER)                     CL**8
00313          END-EXEC.                                                EL1274
00314                                                                   EL1274
00315      EJECT                                                        EL1274
00316  2000-BUILD-OUTPUT-MAP.                                           EL1274
00317 ******************************************************************EL1274
00318 *              BUILD THE OUTPUT SCREEN TO BE DISPLAYED           *EL1274
00319 ******************************************************************EL1274
00320                                                                   EL1274
00321      MOVE LOW-VALUES             TO  EL127DO.                     EL1274
00322      MOVE CM-CERT-PRIME          TO  DCERTNOO.                    EL1274
00323      MOVE CM-CERT-SFX            TO  DCRTSFXO.                    EL1274
00324      MOVE CM-ACCOUNT             TO  DACCTNOO                     EL1274
00325                                      WS-ACCOUNT.                  EL1274
00326      MOVE CM-STATE               TO  DSTATEO.                     EL1274
00327      MOVE CM-CARRIER             TO  DCARIERO.                    EL1274
00328      MOVE CM-GROUPING            TO  DGROUPO.                     EL1274
00329                                                                   EL1274
00330      IF CM-MEMB-STATE   = CM-STATE AND                            EL1274
00331         CM-MEMB-ACCOUNT = WS-ACCT                                 EL1274
00332              MOVE SPACES         TO  DMEMNOO                      EL1274
00333          ELSE                                                     EL1274
00334              MOVE CM-MEMBER-NO   TO  DMEMNOO.                     EL1274
00335                                                                   EL1274
00336      MOVE CM-INSURED-LAST-NAME   TO  DLNAMEO.                     EL1274
00337      MOVE CM-INSURED-FIRST-NAME  TO  DFNAMEO.                     EL1274
00338      MOVE CM-INSURED-INITIAL2    TO  DINITO.                      EL1274
00339      MOVE CM-INSURED-ISSUE-AGE   TO  DAGEO.                       EL1274
00340      MOVE CM-INSURED-SEX         TO  DSEXO.                       EL1274
00341                                                                   EL1274
00342      IF CM-SSN-STATE = CM-STATE AND                               EL1274
00343          CM-SSN-ACCOUNT = WS-ACCT                                 EL1274
00344              MOVE SPACES         TO  DISSNOO                      EL1274
00345          ELSE                                                     EL1274
00346              MOVE CM-SOC-SEC-NO  TO  DISSNOO.                     EL1274
00347                                                                   EL1274
00348      MOVE CM-JT-LAST-NAME        TO  DJLNAMEO.                    EL1274
00349      MOVE CM-JT-FIRST-NAME       TO  DJFNAMEO.                    EL1274
00350      MOVE CM-JT-INITIAL          TO  DJINITO.                     EL1274
00351      MOVE CM-INSURED-JOINT-AGE   TO  DJAGEO.                      EL1274
00352      MOVE CM-BENEFICIARY         TO  DBNAMEO.                     EL1274
00353      MOVE CM-LOAN-APR            TO  DAPRO.                       EL1274
00354      MOVE CM-PAY-FREQUENCY       TO  DPFREQO.                     EL1274
00355      MOVE CM-LOAN-TERM           TO  DLTERMO.                     EL1274
00356      MOVE CM-PAYMENT-MODE        TO  DPMODEO.                     EL1274
00357      MOVE CM-SKIP-CODE           TO  DSKIPCDO.                    EL1274
00358      MOVE CM-LOAN-OFFICER        TO  DLNOFCO.                     EL1274
00359      MOVE CM-LOAN-NUMBER         TO  DLOANNOO.                    EL1274
00360                                                                      CL**5
00361      IF PI-COMPANY-ID = 'HER'                                        CL**5
00362        IF CM-LOAN-NUMBER = SPACES  OR  ZEROS                         CL**5
00363          MOVE CM-USER-RESERVED   TO  DLOANNOO.                       CL**5
00364                                                                      CL**5
00365      MOVE CM-LOAN-BALANCE        TO  DLNBALO.                     EL1274
00366      MOVE CM-PREMIUM-TYPE        TO  DPREMTPO.                    EL1274
00367      MOVE CM-IND-GRP-TYPE        TO  DINDGRPO.                    EL1274
00368      MOVE CM-LIVES               TO  DLIVESO.                     EL1274
00369      MOVE CM-ENTRY-BATCH         TO  DEBATCHO.                    EL1274
00370      MOVE CM-POLICY-FORM-NO      TO  DFORMNOO.                    EL1274
00371      MOVE CM-USER-FIELD          TO  DUSERCDO.                    EL1274
00372      MOVE CM-COMP-EXCP-SW        TO  DCOMPEXO.                    EL1274
00373      MOVE CM-CLAIM-ATTACHED-COUNT    TO  DCLMCNTO.                EL1274
00374                                                                   EL1274
00375      IF CM-CERT-EFF-DT NOT = LOW-VALUES                           EL1274
00376          MOVE SPACES             TO  DC-OPTION-CODE               EL1274
00377          MOVE CM-CERT-EFF-DT     TO  DC-BIN-DATE-1                EL1274
00378          PERFORM 8500-DATE-CONVERSION                             EL1274
00379          MOVE DC-GREG-DATE-1-EDIT TO DEFFDTO.                     EL1274
00380                                                                   EL1274
00381      IF CM-ENTRY-DT NOT = LOW-VALUES                              EL1274
00382          MOVE SPACES             TO  DC-OPTION-CODE               EL1274
00383          MOVE CM-ENTRY-DT        TO  DC-BIN-DATE-1                EL1274
00384          PERFORM 8500-DATE-CONVERSION                             EL1274
00385          MOVE DC-GREG-DATE-1-EDIT TO DENTDTO.                     EL1274
00386                                                                   EL1274
00387      IF CM-LOAN-1ST-PMT-DT IS NOT = LOW-VALUES AND SPACES         EL1274
00388          MOVE SPACES                     TO  DC-OPTION-CODE       EL1274
00389          MOVE CM-LOAN-1ST-PMT-DT         TO  DC-BIN-DATE-1        EL1274
00390          PERFORM 8500-DATE-CONVERSION                             EL1274
00391          IF DATE-CONVERSION-ERROR                                 EL1274
00392              MOVE SPACES                 TO  DPMTDTO              EL1274
00393          ELSE                                                     EL1274
00394              MOVE DC-GREG-DATE-1-EDIT    TO  DPMTDTO.             EL1274
00395                                                                   EL1274
00396      IF CERT-NOTES-ARE-NOT-PRESENT                                EL1274
00397          MOVE 'NO'               TO  DNOTESO                      EL1274
00398      ELSE                                                         EL1274
00399          MOVE 'YES'              TO  DNOTESO.                     EL1274
00400                                                                   EL1274
00401 ******************************************************************EL1274
00402 *                                                                *EL1274
00403 *               DISPLAY ENTRY STATUS CODES                       *EL1274
00404 *                                                                *EL1274
00405 ******************************************************************EL1274
00406                                                                   EL1274
00407      IF CM-ENTRY-STATUS = '1'                                     EL1274
00408          MOVE 'NORM'             TO  DENTSTO.                     EL1274
00409      IF CM-ENTRY-STATUS = '2'                                     EL1274
00410          MOVE 'PEND'             TO  DENTSTO                      EL1274
00411          MOVE 'P'                TO  PI-PEND-SW.                  EL1274
00412      IF CM-ENTRY-STATUS = '4'                                     EL1274
00413          MOVE 'CONV'             TO  DENTSTO.                     EL1274
00414      IF CM-ENTRY-STATUS = '5'                                     EL1274
00415          MOVE 'REIS'             TO  DENTSTO.                     EL1274
00416      IF CM-ENTRY-STATUS = '9'
00417         MOVE 'REIN'              TO  DENTSTO.
122002     IF CM-ENTRY-STATUS = 'M'                                     EL1274
122002         MOVE 'MTHY'             TO  DENTSTO.                     EL1274
052814     IF CM-ENTRY-STATUS = 'C'                                     EL1274
052814         MOVE 'CASH'             TO  DENTSTO.                     EL1274
00418                                                                      CL**6
00419      IF CM-ENTRY-STATUS = 'D'                                        CL**6
00420          MOVE 'DECL'             TO  DENTSTO.                        CL**6
00421                                                                      CL**6
00422      IF CM-ENTRY-STATUS = 'V'                                        CL**6
00423          MOVE 'VOID'             TO  DENTSTO.                        CL**6
00424                                                                      CL**6
00425      IF CM-ENTRY-STATUS = 'U'                                        CL**6
00426          MOVE 'UNDERWRT'         TO  DENTSTO.                        CL**6
00427                                                                   EL1274
00428 ******************************************************************EL1274
00429 *                                                                *EL1274
00430 *               DISPLAY PAYMENT TYPE DESCRIPTIONS                *EL1274
00431 *                                                                *EL1274
00432 ******************************************************************EL1274
00433                                                                   EL1274
00434      IF CM-SING-PRM                                               EL1274
00435          MOVE 'SIN PREM'         TO  DPTDESCO                     EL1274
00436      ELSE                                                         EL1274
00437          IF CM-O-B-COVERAGE                                       EL1274
00438              MOVE 'OUT BAL'      TO  DPTDESCO                     EL1274
00439          ELSE                                                     EL1274
00440              IF CM-OPEN-END                                       EL1274
00441                  MOVE 'OPEN END' TO  DPTDESCO.                    EL1274
00442                                                                   EL1274
00443      MOVE -1                     TO  DPFKEYL.                     EL1274
00444                                                                   EL1274
00445      IF PI-MAIL-YES                                               EL1274
00446          MOVE AL-SANON           TO  DPFKEY6A                     EL1274
00447      ELSE                                                         EL1274
00448          MOVE AL-SADOF           TO  DPFKEY6A.                    EL1274
00449                                                                   EL1274
00450      IF CM-LF-BENEFIT-CD NOT = '00'                                  CL**4
00451          MOVE PI-LIFE-OVERRIDE-L6    TO  DCVG1O                   EL1274
00452          MOVE AL-SANON               TO  DCVG1A.                  EL1274
00453                                                                   EL1274
00454      IF CM-AH-BENEFIT-CD NOT = '00'                                  CL**4
00455          MOVE PI-AH-OVERRIDE-L6  TO  DCVG2O                       EL1274
00456          MOVE AL-SANON           TO  DCVG2A.                      EL1274
00457                                                                   EL1274
00458                                                                   EL1274
00459      GO TO 8200-SEND-DATAONLY.                                    EL1274
00460      EJECT                                                        EL1274
00461  8100-SEND-INITIAL-MAP           SECTION.                         EL1274
00462                                                                   EL1274
00463      MOVE SAVE-DATE              TO  DDATEO.                      EL1274
00464      MOVE EIBTIME                TO  WS-TIME-WORK.                EL1274
00465      MOVE WS-TIME                TO  DTIMEO.                      EL1274
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00466      MOVE EMI-MESSAGE-AREA (1)   TO  DERMSG1O.                    EL1274
00467      MOVE PI-MEMBER-CAPTION      TO  DMEMCAPO.                    EL1274
00468                                                                   EL1274
00469      EXEC CICS SEND                                               EL1274
00470          FROM   (EL127DI)                                         EL1274
00471          MAPSET (WS-MAPSET-NAME)                                  EL1274
00472          MAP    (WS-MAP-NAME)                                     EL1274
00473          CURSOR                                                   EL1274
00474          ERASE                                                    EL1274
00475          END-EXEC.                                                EL1274
00476                                                                   EL1274
00477      GO TO 9100-RETURN-TRAN.                                      EL1274
00478                                                                   EL1274
00479  8100-EXIT.                                                       EL1274
00480      EXIT.                                                        EL1274
00481                                                                   EL1274
00482      EJECT                                                        EL1274
00483  8200-SEND-DATAONLY SECTION.                                      EL1274
00484                                                                   EL1274
00485      IF PI-1ST-TIME-SW = '1'                                      EL1274
00486          GO TO 8100-SEND-INITIAL-MAP.                             EL1274
00487                                                                      CL**2
00488  8200-SEND-DATAONLY-CONT.                                            CL**2
00489                                                                   EL1274
00490      MOVE SAVE-DATE              TO  DDATEO.                      EL1274
00491      MOVE EIBTIME                TO  WS-TIME-WORK.                EL1274
00492      MOVE WS-TIME                TO  DTIMEO.                      EL1274
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00493      MOVE EMI-MESSAGE-AREA (1)   TO  DERMSG1O.                    EL1274
00494      MOVE PI-MEMBER-CAPTION      TO  DMEMCAPO.                    EL1274
00495                                                                   EL1274
00496      EXEC CICS SEND DATAONLY                                      EL1274
00497          FROM   (EL127DI)                                         EL1274
00498          MAPSET (WS-MAPSET-NAME)                                  EL1274
00499          MAP    (WS-MAP-NAME)                                     EL1274
00500          CURSOR                                                   EL1274
00501          END-EXEC.                                                EL1274
00502                                                                   EL1274
00503      GO TO 9100-RETURN-TRAN.                                      EL1274
00504                                                                   EL1274
00505  8200-EXIT.                                                       EL1274
00506      EXIT.                                                        EL1274
00507                                                                   EL1274
00508      EJECT                                                        EL1274
00509  8300-SEND-TEXT SECTION.                                          EL1274
00510                                                                   EL1274
00511      EXEC CICS SEND TEXT                                          EL1274
00512          FROM   (LOGOFF-TEXT)                                     EL1274
00513          LENGTH (LOGOFF-LENGTH)                                   EL1274
00514          ERASE                                                    EL1274
00515          FREEKB                                                   EL1274
00516          END-EXEC.                                                EL1274
00517                                                                   EL1274
00518      EXEC CICS RETURN                                             EL1274
00519          END-EXEC.                                                EL1274
00520                                                                   EL1274
00521  8300-EXIT.                                                       EL1274
00522      EXIT.                                                        EL1274
00523                                                                   EL1274
00524      EJECT                                                        EL1274
00525  8500-DATE-CONVERSION SECTION.                                    EL1274
00526                                                                   EL1274
00527      EXEC CICS LINK                                               EL1274
00528          PROGRAM  (ELDATCV)                                       EL1274
00529          COMMAREA (DATE-CONVERSION-DATA)                          EL1274
00530          LENGTH   (DC-COMM-LENGTH)                                EL1274
00531          END-EXEC.                                                EL1274
00532                                                                   EL1274
00533  8500-EXIT.                                                       EL1274
00534      EXIT.                                                        EL1274
00535                                                                   EL1274
00536  8880-NOT-FOUND SECTION.                                          EL1274
00537                                                                   EL1274
00538      MOVE -1                     TO  DPFKEYL.                     EL1274
00539      MOVE ER-0142                TO  EMI-ERROR.                   EL1274
00540      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1274
00541      GO TO 8100-SEND-INITIAL-MAP.                                 EL1274
00542                                                                   EL1274
00543  8880-EXIT.                                                       EL1274
00544      EXIT.                                                        EL1274
00545                                                                   EL1274
00546  9000-RETURN-CICS SECTION.                                        EL1274
00547                                                                   EL1274
00548      MOVE EL005                  TO  WS-PROGRAM-ID.               EL1274
00549      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL1274
00550      PERFORM 9300-XCTL.                                           EL1274
00551                                                                   EL1274
00552  9000-EXIT.                                                       EL1274
00553      EXIT.                                                        EL1274
00554                                                                   EL1274
00555  9100-RETURN-TRAN SECTION.                                        EL1274
00556                                                                   EL1274
00557      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL1274
00558      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL1274
00559                                                                   EL1274
00560      EXEC CICS RETURN                                             EL1274
00561          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1274
00562          LENGTH   (PI-COMM-LENGTH)                                EL1274
00563          TRANSID  (WS-TRANS-ID)                                   EL1274
00564          END-EXEC.                                                EL1274
00565                                                                   EL1274
00566  9100-EXIT.                                                       EL1274
00567      EXIT.                                                        EL1274
00568                                                                   EL1274
00569  9300-XCTL SECTION.                                               EL1274
00570                                                                   EL1274
00571      MOVE DFHENTER               TO  EIBAID.                      EL1274
00572                                                                   EL1274
00573      EXEC CICS XCTL                                               EL1274
00574          PROGRAM  (WS-PROGRAM-ID)                                 EL1274
00575          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1274
00576          LENGTH   (PI-COMM-LENGTH)                                EL1274
00577          END-EXEC.                                                EL1274
00578                                                                   EL1274
00579  9300-EXIT.                                                       EL1274
00580      EXIT.                                                        EL1274
00581                                                                   EL1274
00582      EJECT                                                        EL1274
00583  9400-CLEAR SECTION.                                              EL1274
00584                                                                   EL1274
00585      MOVE PI-RETURN-TO-PROGRAM  TO  WS-PROGRAM-ID.                EL1274
00586      GO TO 9300-XCTL.                                             EL1274
00587                                                                   EL1274
00588  9400-EXIT.                                                       EL1274
00589      EXIT.                                                        EL1274
00590                                                                   EL1274
00591  9600-PGMIDERR SECTION.                                           EL1274
00592                                                                   EL1274
00593      EXEC CICS HANDLE CONDITION                                   EL1274
00594          PGMIDERR (8300-SEND-TEXT) END-EXEC.                      EL1274
00595                                                                   EL1274
00596      MOVE WS-PROGRAM-ID          TO  PI-CALLING-PROGRAM.          EL1274
00597                                                                   EL1274
00598      MOVE EL005                  TO  WS-PROGRAM-ID                EL1274
00599                                      LOGOFF-PGM.                  EL1274
00600      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL1274
00601      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL1274
00602      GO TO 9300-XCTL.                                             EL1274
00603                                                                   EL1274
00604  9600-EXIT.                                                       EL1274
00605      EXIT.                                                        EL1274
00606                                                                   EL1274
00607      EJECT                                                        EL1274
00608  9900-ERROR-FORMAT SECTION.                                       EL1274
00609                                                                   EL1274
00610      ADD +1  TO  WS-ERROR-COUNT.                                  EL1274
00611                                                                   EL1274
00612      IF EMI-ERRORS-COMPLETE                                       EL1274
00613          MOVE ZERO               TO  EMI-ERROR                    EL1274
00614          GO TO 9900-EXIT.                                         EL1274
00615                                                                   EL1274
00616      EXEC CICS LINK                                               EL1274
00617          PROGRAM  (EL001)                                         EL1274
00618          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL1274
00619          LENGTH   (EMI-COMM-LENGTH)                               EL1274
00620          END-EXEC.                                                EL1274
00621                                                                   EL1274
00622      MOVE ZERO                   TO  EMI-ERROR.                   EL1274
00623                                                                   EL1274
00624  9900-EXIT.                                                       EL1274
00625      EXIT.                                                        EL1274
00626                                                                      CL**9
00627  9990-ERROR SECTION.                                              EL1274
00628                                                                   EL1274
00629      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL1274
00630                                                                   EL1274
00631      EXEC CICS LINK                                               EL1274
00632          PROGRAM  (EL004)                                         EL1274
00633          COMMAREA (EMI-LINE1)                                     EL1274
00634          LENGTH   (72)                                            EL1274
00635          END-EXEC.                                                EL1274
00636                                                                   EL1274
00637  9990-EXIT.                                                       EL1274
00638      EXIT.                                                        EL1274
00639                                                                   EL1274
00640  9999-LAST-PARAGRAPH SECTION.                                     EL1274
00641                                                                   EL1274
00642      GOBACK.                                                      EL1274

