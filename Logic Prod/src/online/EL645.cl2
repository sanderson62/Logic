00001  IDENTIFICATION DIVISION.                                         04/22/98
00002                                                                   EL645
00003  PROGRAM-ID.                 EL645 .                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/12/96 07:42:58.                    CL**4
00007 *                            VMOD=2.004                              CL**4
00008 *                                                                 EL645
00008 *                                                                 EL645
00009 *AUTHOR.        LOGIC, INC.                                          CL**4
00010 *               DALLAS, TEXAS.                                       CL**4
00011                                                                   EL645
00012 *DATE-COMPILED.                                                      CL**4
00013  SKIP1                                                            EL645
00014 *SECURITY.   *****************************************************   CL**4
00015 *            *                                                   *   CL**4
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00017 *            *                                                   *   CL**4
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00021 *            *                                                   *   CL**4
00022 *            *****************************************************   CL**4
00023                                                                   EL645
00024 *REMARKS.                                                         EL645
00025 *        THIS PROGRAM PROVIDES THE QUALIFICATION NECESSARY FOR    EL645
00026 *        LOSS RATIOS LOOK-UP.                                     EL645
00027                                                                   EL645
00028 *    SCREENS     - EL645A - LOSS RATIOS SELECTION MENU            EL645
00029                                                                   EL645
00030 *    ENTERED BY  - EL626 - MAIN PROCESSING MENU                   EL645
00031                                                                   EL645
00032 *    EXIT TO     - CALLING PROGRAM                                EL645
00033 *                  EL6451 - LOSS RATIOS AS OF (MM/DD/YY)          EL645
00034 *                  EL659  - NAME LOOKUP (ACCT,COMP,REIN)             CL**2
00035                                                                   EL645
00036 *    INPUT FILE  - ERLOSS - LOSS RATIOS                           EL645
00037                                                                   EL645
00038 *    OUTPUT FILE - NONE                                           EL645
00039                                                                   EL645
00040 *    COMMAREA    - PASSED.  THE CONTROL INFORMATION KEYED IN THIS EL645
00041 *                  PROGRAM IS PLACED IN THE APPROPRIATE FIELDS OF EL645
00042 *                  THE COMMAREA FOR REFERENCE BY EL6451.  THE     EL645
00043 *                  PROGRAM WORK AREA OF THE COMMAREA IS USED TO   EL645
00044 *                  PASS THE RECORD KEY INFORMATION NEEDED BY      EL645
00045 *                  EL6451.                                        EL645
00046                                                                   EL645
00047 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL626.  ON     EL645
00048 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE EL645
00049 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVEEL645
00050 *                  ENTRIES (XCTL FROM CICS VIA EXR1) THE SCREEN   EL645
00051 *                  WILL BE READ AND ACTION WILL BE BASED ON THE   EL645
00052 *                  OPTION INDICATED.  PROGRAM FUNCTION KEY 10 WILL   CL**2
00053 *                  TRANSFER CONTROL TO PROGRAM EL659 - NAME LOOKUP   CL**2
00054  EJECT                                                            EL645
00055  ENVIRONMENT DIVISION.                                            EL645
00056  DATA DIVISION.                                                   EL645
00057  WORKING-STORAGE SECTION.                                         EL645
00058                                                                   EL645
00059  77  FILLER  PIC  X(32) VALUE '********************************'. EL645
00060  77  FILLER  PIC  X(32) VALUE '*    EL645 WORKING STORAGE     *'. EL645
00061  77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.004 *********'.    CL**4
00062                                                                   EL645
00063                                  COPY ELCSCTM.                       CL**4
00064                                                                   EL645
00065                                  COPY ELCSCRTY.                      CL**4
00066                                                                   EL645
00067  01  WS-DATE-AREA.                                                EL645
00068      12  SAVE-DATE               PIC  X(08)      VALUE SPACES.    EL645
00069      12  SAVE-BIN-DATE           PIC  X(02)      VALUE SPACES.    EL645
00070                                                                   EL645
00071  01  FILLER                  COMP-3.                              EL645
00072      12  WS-COMPLETED-SUCCESSFUL     PIC S9(01)  VALUE ZERO.      EL645
00073          88  TRANSACTION-SUCCESSFUL              VALUE +1.        EL645
00074      12  TIME-IN                     PIC S9(07)  VALUE ZERO.      EL645
00075      12  TIME-OUT  REDEFINES                                      EL645
00076          TIME-IN                     PIC S9(03)V9(4).             EL645
00077                                                                   EL645
00078  01  FILLER                  COMP SYNC.                           EL645
00079      12  SC-ITEM                 PIC S9(04)      VALUE +0001.     EL645
00080                                                                   EL645
00081  01  FILLER.                                                      EL645
00082      12  WS-MAPSET-NAME          PIC  X(08)      VALUE 'EL645S'.  EL645
00083      12  WS-MAP-NAME             PIC  X(08)    VALUE 'EL645A  '.     CL**4
00084      12  FILLER  REDEFINES  WS-MAP-NAME.                          EL645
00085          16  FILLER              PIC  X(02).                      EL645
00086          16  WS-MAP-NUMBER       PIC  X(04).                      EL645
00087          16  FILLER              PIC  X(02).                         CL**4
00088      12  THIS-PGM                PIC  X(08)      VALUE 'EL645'.   EL645
00089      12  WS-LOSS-RATIO-DSID      PIC  X(08)      VALUE 'ERLOSS'.  EL645
00090      12  WS-TRANS-ID             PIC  X(04)      VALUE 'EXR1'.    EL645
00091      12  ERROR-MESSAGES.                                          EL645
00092          16  ER-0002             PIC  X(04)      VALUE '0002'.    EL645
00093          16  ER-0004             PIC  X(04)      VALUE '0004'.    EL645
00094          16  ER-0005             PIC  X(04)      VALUE '0005'.    EL645
00095          16  ER-0008             PIC  X(04)      VALUE '0008'.    EL645
00096          16  ER-0029             PIC  X(04)      VALUE '0029'.    EL645
00097          16  ER-0070             PIC  X(04)      VALUE '0070'.    EL645
00098          16  ER-0671             PIC  X(04)      VALUE '0671'.    EL645
00099          16  ER-0674             PIC  X(04)      VALUE '0674'.    EL645
00100  EJECT                                                            EL645
00101                                  COPY ELCINTF.                       CL**4
00102                                                                   EL645
00103                                  COPY ELC645PI.                      CL**4
00104  EJECT                                                            EL645
00105                                  COPY ELCEMIB.                       CL**5
00106  EJECT                                                            EL645
00107                                  COPY ELCDATE.                       CL**4
00108  EJECT                                                            EL645
00109                                  COPY ELCLOGOF.                      CL**4
00110  EJECT                                                            EL645
00111                                  COPY EL645S.                        CL**4
00112  EJECT                                                            EL645
00113                                  COPY ELCATTR.                       CL**4
00114  EJECT                                                            EL645
00115                                  COPY ELCAID.                        CL**4
00116                                                                   EL645
00117  01  FILLER  REDEFINES  DFHAID.                                   EL645
00118      12  FILLER                      PIC  X(08).                  EL645
00119      12  PF-VALUES                   PIC  X(01)                   EL645
00120              OCCURS  24  TIMES.                                   EL645
00121  EJECT                                                            EL645
00122  LINKAGE SECTION.                                                 EL645
00123                                                                   EL645
00124  01  DFHCOMMAREA                     PIC  X(1024).                EL645
00125                                                                   EL645
00126 *01 PARMLIST             COMP  SYNC.                                 CL**4
00127 *    12  FILLER                      PIC S9(09).                     CL**4
00128 *    12  ERLOSS-POINTER              PIC S9(09).                     CL**4
00129  EJECT                                                            EL645
00130                                  COPY ERCLOSS.                       CL**4
00131  EJECT                                                            EL645
00132  PROCEDURE DIVISION.                                              EL645
00133                                                                   EL645
00134      CONTINUE.                                                       CL**4
00135                                                                   EL645
00136      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL645
00137      MOVE '5'                    TO  DC-OPTION-CODE.              EL645
00138                                                                   EL645
00139      PERFORM 7000-DATE-CONVERSION.                                EL645
00140                                                                   EL645
00141      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL645
00142      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL645
00143      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL645
00144      MOVE +2                     TO  EMI-NUMBER-OF-LINES          EL645
00145                                      EMI-SWITCH2.                 EL645
00146                                                                   EL645
00147 *    NOTE ******************************************************* EL645
00148 *         *                                                     * EL645
00149 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL645
00150 *         *  FROM ANOTHER MODULE.                               * EL645
00151 *         *                                                     * EL645
00152 *         *******************************************************.EL645
00153                                                                   EL645
00154      IF EIBCALEN  IS NOT GREATER THAN  ZERO                       EL645
00155          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL645
00156          GO TO 6000-SEND-TEXT.                                    EL645
00157                                                                   EL645
00158      EXEC CICS HANDLE CONDITION                                   EL645
00159          PGMIDERR  (9400-PGMIDERR)                                EL645
00160          NOTFND    (1020-NOTFND)                                  EL645
00161          ENDFILE   (1020-NOTFND)                                  EL645
00162          ERROR     (9600-ERROR)                                   EL645
00163      END-EXEC.                                                    EL645
00164  EJECT                                                            EL645
00165  0100-MAIN-LOGIC.                                                 EL645
00166      IF PI-CALLING-PROGRAM  IS NOT EQUAL TO  THIS-PGM             EL645
00167          IF PI-RETURN-TO-PROGRAM  IS NOT EQUAL TO  THIS-PGM       EL645
00168              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL645
00169              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL645
00170              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL645
00171              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL645
00172              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL645
00173              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL645
00174              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL645
00175              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM    EL645
00176          ELSE                                                     EL645
00177              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL645
00178              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL645
00179              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL645
00180              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL645
00181              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL645
00182              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL645
00183              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL645
00184              MOVE SPACES                TO  PI-SAVED-PROGRAM-6    EL645
00185              PERFORM 3000-BUILD-SCREEN                            EL645
00186      ELSE                                                         EL645
00187          GO TO 0120-MAIN-LOGIC.                                   EL645
00188                                                                   EL645
00189  0110-MAIN-LOGIC.                                                 EL645
00190 *    NOTE ******************************************************* EL645
00191 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL645
00192 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL645
00193 *         *******************************************************.EL645
00194                                                                   EL645
00195      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA         EL645
00196                                      PI-RETURN-SWITCH             EL645
00197                                      PI-CONTROL-IN-PROGRESS.      EL645
00198      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL645
00199                                      PI-LINE-COUNT                EL645
00200                                      PI-BROWSE-SW                 EL645
00201                                      PI-KEY-LENGTH                EL645
00202                                      PI-TS-ITEM                   EL645
00203                                      PI-END-OF-FILE               EL645
00204                                      PI-START-SW                  EL645
00205                                      PI-AIX-RECORD-COUNT.         EL645
00206                                                                   EL645
00207 *    NOTE ******************************************************* EL645
00208 *         *      SEND THE INITIAL MAP OUT TO BEGIN PROCESSING   * EL645
00209 *         *  FOR EL645.                                         * EL645
00210 *         *******************************************************.EL645
00211                                                                   EL645
00212      MOVE LOW-VALUES             TO  EL645AO.                     EL645
00213                                                                   EL645
00214      PERFORM 4000-SEND-INITIAL-MAP.                               EL645
00215                                                                   EL645
00216      GO TO 9100-RETURN-TRAN.                                      EL645
00217  EJECT                                                            EL645
00218  0120-MAIN-LOGIC.                                                 EL645
00219      IF PI-REENTERED                                              EL645
00220          GO TO 0900-RESTORE-SCREEN.                               EL645
00221                                                                   EL645
00222 *    NOTE ******************************************************* EL645
00223 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL645
00224 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL645
00225 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL645
00226 *         *******************************************************.EL645
00227                                                                   EL645
00228      IF EIBAID  IS EQUAL TO  DFHCLEAR                             EL645
00229          GO TO 9300-CLEAR.                                        EL645
00230                                                                   EL645
00231      IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3       EL645
00232          MOVE LOW-VALUES         TO  EL645AO                      EL645
00233          MOVE -1                 TO  ASELL                        EL645
00234          MOVE ER-0008            TO  EMI-ERROR                    EL645
00235          PERFORM 5000-SEND-DATA-ONLY                              EL645
00236          GO TO 9100-RETURN-TRAN.                                  EL645
00237                                                                   EL645
00238      EXEC CICS RECEIVE                                            EL645
00239          INTO    (EL645AI)                                        EL645
00240          MAPSET  (WS-MAPSET-NAME)                                 EL645
00241          MAP     (WS-MAP-NAME)                                    EL645
00242      END-EXEC.                                                    EL645
00243                                                                   EL645
00244      IF ASELL  EQUAL  ZERO                                        EL645
00245          GO TO 0125-CHECK-PFKEYS.                                 EL645
00246                                                                   EL645
00247      IF EIBAID  IS NOT EQUAL TO  DFHENTER                         EL645
00248          MOVE ER-0004            TO  EMI-ERROR                    EL645
00249          MOVE AL-UNBOF           TO  ASELA                        EL645
00250          MOVE -1                 TO  ASELL                        EL645
00251          PERFORM 5000-SEND-DATA-ONLY                              EL645
00252          GO TO 9100-RETURN-TRAN.                                  EL645
00253                                                                   EL645
00254      IF ASELI  IS NUMERIC  AND                                    EL645
00255        (ASELI  IS GREATER THAN  0  AND                            EL645
00256         ASELI  IS LESS THAN  25)                                  EL645
00257          MOVE PF-VALUES (ASELI)  TO EIBAID                        EL645
00258      ELSE                                                         EL645
00259          MOVE ER-0029            TO  EMI-ERROR                    EL645
00260          MOVE AL-UNBOF           TO  ASELA                        EL645
00261          MOVE -1                 TO  ASELL                        EL645
00262          PERFORM 5000-SEND-DATA-ONLY                              EL645
00263          GO TO 9100-RETURN-TRAN.                                  EL645
00264                                                                   EL645
00265  0125-CHECK-PFKEYS.                                               EL645
00266      IF EIBAID  IS EQUAL TO  DFHPF1 OR DFHPF2 OR DFHPF3 OR        EL645
00267                              DFHPF4 OR DFHPF5 OR DFHPF6           EL645
00268          GO TO 0130-MAIN-LOGIC.                                   EL645
00269                                                                      CL**2
00270      IF EIBAID  IS EQUAL TO  DFHPF10                                 CL**2
00271          MOVE 'EL659'            TO  THIS-PGM                        CL**2
00272          GO TO 9200-XCTL.                                            CL**2
00273                                                                   EL645
00274      IF EIBAID  IS EQUAL TO  DFHPF12                              EL645
00275          MOVE 'EL010'            TO  THIS-PGM                     EL645
00276          GO TO 9200-XCTL.                                         EL645
00277                                                                   EL645
00278      IF EIBAID  IS EQUAL TO  DFHPF23                              EL645
00279          GO TO 9000-RETURN-CICS.                                  EL645
00280                                                                   EL645
00281      IF EIBAID  IS EQUAL TO  DFHPF24                              EL645
00282          MOVE 'EL126'            TO  THIS-PGM                     EL645
00283          GO TO 9200-XCTL.                                         EL645
00284                                                                   EL645
00285      MOVE ER-0029                TO  EMI-ERROR.                   EL645
00286      MOVE AL-UNBOF               TO  ASELA.                       EL645
00287      MOVE -1                     TO  ASELL.                       EL645
00288      PERFORM 5000-SEND-DATA-ONLY.                                 EL645
00289      GO TO 9100-RETURN-TRAN.                                      EL645
00290  EJECT                                                            EL645
00291  0130-MAIN-LOGIC.                                                 EL645
00292      MOVE SPACES                 TO  PI-SELECTION-CRITERIA        EL645
00293                                      PI-LOSS-RATIO-KEY.           EL645
00294      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD             EL645
00295                                      PI-LRK-COMPANY-CD.           EL645
00296      MOVE ACTONLYI               TO  PI-ACTIVE-ONLY.                 CL**2
00297                                                                   EL645
00298      IF PI-PROCESSOR-ID  IS EQUAL TO  'LGXX'                      EL645
00299          NEXT SENTENCE                                            EL645
00300      ELSE                                                         EL645
00301          EXEC CICS READQ TS                                       EL645
00302              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL645
00303              INTO    (SECURITY-CONTROL)                           EL645
00304              LENGTH  (SC-COMM-LENGTH)                             EL645
00305              ITEM    (SC-ITEM)                                    EL645
00306          END-EXEC                                                 EL645
00307          MOVE SC-CREDIT-DISPLAY (31)                              EL645
00308                                  TO  PI-DISPLAY-CAP               EL645
00309          MOVE SC-CREDIT-UPDATE  (31)                              EL645
00310                                  TO  PI-MODIFY-CAP                EL645
00311          IF NOT DISPLAY-CAP                                       EL645
00312              MOVE 'READ'         TO  SM-READ                      EL645
00313              PERFORM 9700-SECURITY-VIOLATION                      EL645
00314              MOVE ER-0070        TO  EMI-ERROR                    EL645
00315              PERFORM 4000-SEND-INITIAL-MAP                        EL645
00316              GO TO 9100-RETURN-TRAN.                              EL645
00317  EJECT                                                            EL645
00318  0200-OPTION-1-PROCESSING.                                        EL645
00319 ******************************************************************EL645
00320 *           O P T I O N  1  P R O C E S S I N G                  *EL645
00321 ******************************************************************EL645
00322                                                                   EL645
00323      IF EIBAID IS EQUAL TO  DFHPF6                                EL645
00324          GO TO 0700-OPTION-6-PROCESSING.                          EL645
00325                                                                   EL645
00326      IF EIBAID IS EQUAL TO  DFHPF5                                EL645
00327          GO TO 0600-OPTION-5-PROCESSING.                          EL645
00328                                                                   EL645
00329      IF EIBAID IS EQUAL TO  DFHPF4                                EL645
00330          GO TO 0500-OPTION-4-PROCESSING.                          EL645
00331                                                                   EL645
00332      IF EIBAID IS EQUAL TO  DFHPF3                                EL645
00333          GO TO 0400-OPTION-3-PROCESSING.                          EL645
00334                                                                   EL645
00335      IF EIBAID IS EQUAL TO  DFHPF2                                EL645
00336          GO TO 0300-OPTION-2-PROCESSING.                          EL645
00337                                                                   EL645
00338      MOVE LOW-VALUES             TO  PI-SELECTION-CRITERIA.       EL645
00339                                                                   EL645
00340      MOVE 'EL645C'               TO  PI-MAPNAME.                  EL645
00341                                                                   EL645
00342      IF PI-CERT-ACCESS-CONTROL = '1'  OR  '2'  OR  '4'            EL645
00343          IF ACARRL  IS GREATER THAN  ZERO                         EL645
00344              MOVE ACARRI         TO  PI-SC-CARRIER                EL645
00345          ELSE                                                     EL645
00346              MOVE ER-0005        TO  EMI-ERROR                    EL645
00347              MOVE AL-UNBOF       TO  ACARRA                       EL645
00348              MOVE -1             TO  ACARRL                       EL645
00349              PERFORM 5000-SEND-DATA-ONLY                          EL645
00350              GO TO 9100-RETURN-TRAN.                              EL645
00351                                                                   EL645
00352      IF PI-CERT-ACCESS-CONTROL = '1'                              EL645
00353          IF AGROUPL  IS GREATER THAN  ZERO                        EL645
00354              MOVE AGROUPI        TO  PI-SC-GROUPING               EL645
00355          ELSE                                                     EL645
00356              MOVE ER-0005        TO  EMI-ERROR                    EL645
00357              MOVE AL-UNBOF       TO  AGROUPA                      EL645
00358              MOVE -1             TO  AGROUPL                      EL645
00359              PERFORM 5000-SEND-DATA-ONLY                          EL645
00360              GO TO 9100-RETURN-TRAN.                              EL645
00361                                                                   EL645
00362      IF PI-CERT-ACCESS-CONTROL = ' '  OR  '1'  OR  '2'            EL645
00363          IF ASTATEL  IS GREATER THAN  ZERO                        EL645
00364              MOVE ASTATEI        TO  PI-SC-STATE                  EL645
00365          ELSE                                                     EL645
00366              MOVE ER-0005        TO  EMI-ERROR                    EL645
00367              MOVE AL-UNBOF       TO  ASTATEA                      EL645
00368              MOVE -1             TO  ASTATEL                      EL645
00369              PERFORM 5000-SEND-DATA-ONLY                          EL645
00370              GO TO 9100-RETURN-TRAN.                              EL645
00371                                                                   EL645
00372      IF AACCTL  IS GREATER THAN  ZERO                             EL645
00373          MOVE AACCTI             TO  PI-SC-ACCOUNT                EL645
00374      ELSE                                                         EL645
00375          MOVE ER-0005            TO  EMI-ERROR                    EL645
00376          MOVE AL-UNBOF           TO  AACCTA                       EL645
00377          MOVE -1                 TO  AACCTL                       EL645
00378          PERFORM 5000-SEND-DATA-ONLY                              EL645
00379          GO TO 9100-RETURN-TRAN.                                  EL645
00380                                                                   EL645
00381  0210-PROCESS-OPTION-1.                                           EL645
00382                                                                   EL645
00383      MOVE WS-LOSS-RATIO-DSID     TO  PI-DSID.                     EL645
00384      MOVE '1'                    TO  PI-OPTION.                   EL645
00385      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD.            EL645
00386      MOVE 'A'                    TO  PI-SC-RCD-TYPE.              EL645
00387      MOVE +47                    TO  PI-KEY-LENGTH.               EL645
00388      MOVE PI-SELECTION-CRITERIA  TO  PI-LOSS-RATIO-KEY            EL645
00389                                      PI-SAVE-LOSS-RATIO-KEY.      EL645
00390      MOVE -1                     TO  ACARRL.                      EL645
00391                                                                   EL645
00392      PERFORM 1000-READ-LOSS-RATIO.                                EL645
00393      GO TO 9200-XCTL.                                                CL**4
00394  EJECT                                                            EL645
00395  0300-OPTION-2-PROCESSING.                                        EL645
00396 ******************************************************************EL645
00397 *           O P T I O N  2  P R O C E S S I N G                  *EL645
00398 ******************************************************************EL645
00399                                                                   EL645
00400      MOVE LOW-VALUES             TO  PI-SELECTION-CRITERIA.       EL645
00401                                                                   EL645
00402      MOVE 'EL645B'               TO  PI-MAPNAME.                  EL645
00403                                                                   EL645
00404      IF PI-CAR-GROUP-ACCESS-CNTL = ' ' OR '2'                        CL**3
00405          IF ACARRL  IS GREATER THAN  ZERO                            CL**3
00406              MOVE ACARRI         TO  PI-SC-CARRIER                   CL**3
00407          ELSE                                                        CL**3
00408              MOVE ER-0005        TO  EMI-ERROR                       CL**3
00409              MOVE AL-UNBOF       TO  ACARRA                          CL**3
00410              MOVE -1             TO  ACARRL                          CL**3
00411              PERFORM 5000-SEND-DATA-ONLY                             CL**3
00412              GO TO 9100-RETURN-TRAN                                  CL**3
00413      ELSE                                                         EL645
00414          IF PI-CAR-GROUP-ACCESS-CNTL = '1' OR '3'                    CL**3
00415              MOVE '0'            TO  PI-SC-CARRIER.                  CL**3
00416                                                                   EL645
00417      IF PI-CAR-GROUP-ACCESS-CNTL = ' ' OR '1'                        CL**3
00418          IF AGROUPL  IS GREATER THAN  ZERO                           CL**3
00419              MOVE AGROUPI        TO  PI-SC-GROUPING                  CL**3
00420          ELSE                                                        CL**3
00421              MOVE ER-0005        TO  EMI-ERROR                       CL**3
00422              MOVE AL-UNBOF       TO  AGROUPA                         CL**3
00423              MOVE -1             TO  AGROUPL                         CL**3
00424              PERFORM 5000-SEND-DATA-ONLY                             CL**3
00425              GO TO 9100-RETURN-TRAN                                  CL**3
00426      ELSE                                                         EL645
00427          IF PI-CAR-GROUP-ACCESS-CNTL = '2' OR '3'                    CL**3
00428              MOVE '000000'       TO  PI-SC-GROUPING.                 CL**3
00429                                                                   EL645
00430      IF AGENAGTL  IS GREATER THAN  ZERO                           EL645
00431          MOVE AGENAGTI           TO  PI-SC-GA-RPT-CD-2            EL645
00432      ELSE                                                         EL645
00433          MOVE ER-0005            TO  EMI-ERROR                    EL645
00434          MOVE AL-UNBOF           TO  AGENAGTA                     EL645
00435          MOVE -1                 TO  AGENAGTL                     EL645
00436          PERFORM 5000-SEND-DATA-ONLY                              EL645
00437          GO TO 9100-RETURN-TRAN.                                  EL645
00438                                                                   EL645
00439      IF AACCTL  IS GREATER THAN  ZERO                             EL645
00440          MOVE 'EL645C'           TO  PI-MAPNAME                   EL645
00441          MOVE AACCTI             TO  PI-SC-ACCOUNT                EL645
00442      ELSE                                                         EL645
00443          GO TO 0310-PROCESS-OPTION-2.                             EL645
00444                                                                   EL645
00445      IF PI-CERT-ACCESS-CONTROL = ' '  OR  '1'  OR  '2'               CL**3
00446          IF ASTATEL  IS GREATER THAN  ZERO                        EL645
00447 *            MOVE 'EL645C'       TO  PI-MAPNAME                   EL645
00448              MOVE ASTATEI        TO  PI-SC-STATE                  EL645
00449          ELSE                                                     EL645
00450              MOVE ER-0005        TO  EMI-ERROR                    EL645
00451              MOVE AL-UNBOF       TO  ASTATEA                      EL645
00452              MOVE -1             TO  ASTATEL                      EL645
00453              PERFORM 5000-SEND-DATA-ONLY                          EL645
00454              GO TO 9100-RETURN-TRAN.                              EL645
00455                                                                   EL645
00456  0310-PROCESS-OPTION-2.                                           EL645
00457      MOVE WS-LOSS-RATIO-DSID     TO  PI-DSID.                     EL645
00458      MOVE '2'                    TO  PI-OPTION.                   EL645
00459      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD.            EL645
00460      MOVE 'G'                    TO  PI-SC-RCD-TYPE.              EL645
00461      MOVE +47                    TO  PI-KEY-LENGTH.               EL645
00462      MOVE PI-SELECTION-CRITERIA  TO  PI-LOSS-RATIO-KEY            EL645
00463                                      PI-SAVE-LOSS-RATIO-KEY.      EL645
00464      MOVE -1                     TO  ACARRL.                      EL645
00465                                                                   EL645
00466      PERFORM 1000-READ-LOSS-RATIO.                                EL645
00467      GO TO 9200-XCTL.                                                CL**4
00468  EJECT                                                            EL645
00469  0400-OPTION-3-PROCESSING.                                        EL645
00470 ******************************************************************EL645
00471 *           O P T I O N  3  P R O C E S S I N G                  *EL645
00472 ******************************************************************EL645
00473                                                                   EL645
00474      MOVE LOW-VALUES             TO  PI-SELECTION-CRITERIA.       EL645
00475                                                                   EL645
00476      MOVE 'EL645B'               TO  PI-MAPNAME.                  EL645
00477                                                                   EL645
00478      IF AREINCOL  IS GREATER THAN  ZERO                           EL645
00479          MOVE AREINCOI           TO  PI-SC-REIN-CO                EL645
00480      ELSE                                                         EL645
00481          MOVE ER-0005            TO  EMI-ERROR                    EL645
00482          MOVE AL-UNBOF           TO  AREINCOA                     EL645
00483          MOVE -1                 TO  AREINCOL                     EL645
00484          PERFORM 5000-SEND-DATA-ONLY                              EL645
00485          GO TO 9100-RETURN-TRAN.                                  EL645
00486                                                                   EL645
00487      IF AACCTL  IS GREATER THAN  ZERO                             EL645
00488          MOVE 'EL645C'           TO  PI-MAPNAME                   EL645
00489          MOVE AACCTI             TO  PI-SC-ACCOUNT                EL645
00490      ELSE                                                         EL645
00491          GO TO 0410-PROCESS-OPTION-3.                             EL645
00492                                                                   EL645
00493      IF PI-CERT-ACCESS-CONTROL = '1'  OR  '2'  OR  '4'            EL645
00494          IF ACARRL  IS GREATER THAN  ZERO                         EL645
00495              MOVE ACARRI         TO  PI-SC-CARRIER                EL645
00496          ELSE                                                     EL645
00497              MOVE ER-0005        TO  EMI-ERROR                    EL645
00498              MOVE AL-UNBOF       TO  ACARRA                       EL645
00499              MOVE -1             TO  ACARRL                       EL645
00500              PERFORM 5000-SEND-DATA-ONLY                          EL645
00501              GO TO 9100-RETURN-TRAN.                              EL645
00502                                                                   EL645
00503      IF PI-CERT-ACCESS-CONTROL = '1'                              EL645
00504          IF AGROUPL  IS GREATER THAN  ZERO                        EL645
00505              MOVE AGROUPI        TO  PI-SC-GROUPING               EL645
00506          ELSE                                                     EL645
00507              MOVE ER-0005        TO  EMI-ERROR                    EL645
00508              MOVE AL-UNBOF       TO  AGROUPA                      EL645
00509              MOVE -1             TO  AGROUPL                      EL645
00510              PERFORM 5000-SEND-DATA-ONLY                          EL645
00511              GO TO 9100-RETURN-TRAN.                              EL645
00512                                                                   EL645
00513      IF PI-CERT-ACCESS-CONTROL = ' '  OR  '1'  OR  '2'            EL645
00514          IF ASTATEL  IS GREATER THAN  ZERO                        EL645
00515              MOVE ASTATEI        TO  PI-SC-STATE                  EL645
00516          ELSE                                                     EL645
00517              MOVE ER-0005        TO  EMI-ERROR                    EL645
00518              MOVE AL-UNBOF       TO  ASTATEA                      EL645
00519              MOVE -1             TO  ASTATEL                      EL645
00520              PERFORM 5000-SEND-DATA-ONLY                          EL645
00521              GO TO 9100-RETURN-TRAN.                              EL645
00522                                                                   EL645
00523  0410-PROCESS-OPTION-3.                                           EL645
00524      MOVE WS-LOSS-RATIO-DSID     TO  PI-DSID.                     EL645
00525      MOVE '3'                    TO  PI-OPTION.                   EL645
00526      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD.            EL645
00527      MOVE 'R'                    TO  PI-SC-RCD-TYPE.              EL645
00528      MOVE +47                    TO  PI-KEY-LENGTH.               EL645
00529      MOVE PI-SELECTION-CRITERIA  TO  PI-LOSS-RATIO-KEY            EL645
00530                                      PI-SAVE-LOSS-RATIO-KEY.      EL645
00531      MOVE -1                     TO  AREINCOL.                    EL645
00532                                                                   EL645
00533      PERFORM 1000-READ-LOSS-RATIO.                                EL645
00534      GO TO 9200-XCTL.                                                CL**4
00535  EJECT                                                            EL645
00536  0500-OPTION-4-PROCESSING.                                        EL645
00537 ******************************************************************EL645
00538 *           O P T I O N  4  P R O C E S S I N G                  *EL645
00539 ******************************************************************EL645
00540                                                                   EL645
00541      MOVE LOW-VALUES             TO  PI-SELECTION-CRITERIA.       EL645
00542                                                                   EL645
00543      MOVE 'EL645B'               TO  PI-MAPNAME.                  EL645
00544                                                                   EL645
00545      IF ARPTCD1L  IS GREATER THAN  ZERO                           EL645
00546          MOVE ARPTCD1I           TO  PI-SC-RPT-CD-1               EL645
00547      ELSE                                                         EL645
00548          MOVE ER-0005            TO  EMI-ERROR                    EL645
00549          MOVE AL-UNBOF           TO  ARPTCD1A                     EL645
00550          MOVE -1                 TO  ARPTCD1L                     EL645
00551          PERFORM 5000-SEND-DATA-ONLY                              EL645
00552          GO TO 9100-RETURN-TRAN.                                  EL645
00553                                                                   EL645
00554      IF AACCTL  IS GREATER THAN  ZERO                             EL645
00555          MOVE 'EL645C'           TO  PI-MAPNAME                   EL645
00556          MOVE AACCTI             TO  PI-SC-ACCOUNT                EL645
00557      ELSE                                                         EL645
00558          GO TO 0510-PROCESS-OPTION-4.                             EL645
00559                                                                   EL645
00560      IF PI-CERT-ACCESS-CONTROL = '1'  OR  '2'  OR  '4'            EL645
00561          IF ACARRL  IS GREATER THAN  ZERO                         EL645
00562              MOVE ACARRI         TO  PI-SC-CARRIER                EL645
00563          ELSE                                                     EL645
00564              MOVE ER-0005        TO  EMI-ERROR                    EL645
00565              MOVE AL-UNBOF       TO  ACARRA                       EL645
00566              MOVE -1             TO  ACARRL                       EL645
00567              PERFORM 5000-SEND-DATA-ONLY                          EL645
00568              GO TO 9100-RETURN-TRAN.                              EL645
00569                                                                   EL645
00570      IF PI-CERT-ACCESS-CONTROL = '1'                              EL645
00571          IF AGROUPL  IS GREATER THAN  ZERO                        EL645
00572              MOVE AGROUPI        TO  PI-SC-GROUPING               EL645
00573          ELSE                                                     EL645
00574              MOVE ER-0005        TO  EMI-ERROR                    EL645
00575              MOVE AL-UNBOF       TO  AGROUPA                      EL645
00576              MOVE -1             TO  AGROUPL                      EL645
00577              PERFORM 5000-SEND-DATA-ONLY                          EL645
00578              GO TO 9100-RETURN-TRAN.                              EL645
00579                                                                   EL645
00580      IF PI-CERT-ACCESS-CONTROL = ' '  OR  '1'  OR  '2'            EL645
00581          IF ASTATEL  IS GREATER THAN  ZERO                        EL645
00582              MOVE ASTATEI        TO  PI-SC-STATE                  EL645
00583          ELSE                                                     EL645
00584              MOVE ER-0005        TO  EMI-ERROR                    EL645
00585              MOVE AL-UNBOF       TO  ASTATEA                      EL645
00586              MOVE -1             TO  ASTATEL                      EL645
00587              PERFORM 5000-SEND-DATA-ONLY                          EL645
00588              GO TO 9100-RETURN-TRAN.                              EL645
00589                                                                   EL645
00590  0510-PROCESS-OPTION-4.                                           EL645
00591      MOVE WS-LOSS-RATIO-DSID     TO  PI-DSID.                     EL645
00592      MOVE '4'                    TO  PI-OPTION.                   EL645
00593      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD.            EL645
00594      MOVE 'B'                    TO  PI-SC-RCD-TYPE.              EL645
00595      MOVE +47                    TO  PI-KEY-LENGTH.               EL645
00596      MOVE PI-SELECTION-CRITERIA  TO  PI-LOSS-RATIO-KEY            EL645
00597                                      PI-SAVE-LOSS-RATIO-KEY.      EL645
00598      MOVE -1                     TO  ARPTCD1L.                    EL645
00599                                                                   EL645
00600      PERFORM 1000-READ-LOSS-RATIO.                                EL645
00601      GO TO 9200-XCTL.                                                CL**4
00602  EJECT                                                            EL645
00603  0600-OPTION-5-PROCESSING.                                        EL645
00604 ******************************************************************EL645
00605 *           O P T I O N  5  P R O C E S S I N G                  *EL645
00606 ******************************************************************EL645
00607                                                                   EL645
00608      MOVE LOW-VALUES             TO  PI-SELECTION-CRITERIA.       EL645
00609                                                                   EL645
00610      MOVE 'EL645B'               TO  PI-MAPNAME.                  EL645
00611                                                                   EL645
00612      IF ARPTCD2L  IS GREATER THAN  ZERO                           EL645
00613          MOVE ARPTCD2I           TO  PI-SC-GA-RPT-CD-2            EL645
00614      ELSE                                                         EL645
00615          MOVE ER-0005            TO  EMI-ERROR                    EL645
00616          MOVE AL-UNBOF           TO  ARPTCD2A                     EL645
00617          MOVE -1                 TO  ARPTCD2L                     EL645
00618          PERFORM 5000-SEND-DATA-ONLY                              EL645
00619          GO TO 9100-RETURN-TRAN.                                  EL645
00620                                                                   EL645
00621      IF PI-CERT-ACCESS-CONTROL = '1'  OR  '2'  OR  '4'            EL645
00622          IF ACARRL  IS GREATER THAN  ZERO                         EL645
00623              MOVE ACARRI         TO  PI-SC-CARRIER                EL645
00624          ELSE                                                     EL645
00625              MOVE ER-0005        TO  EMI-ERROR                    EL645
00626              MOVE AL-UNBOF       TO  ACARRA                       EL645
00627              MOVE -1             TO  ACARRL                       EL645
00628              PERFORM 5000-SEND-DATA-ONLY                          EL645
00629              GO TO 9100-RETURN-TRAN.                              EL645
00630                                                                   EL645
00631      IF PI-CERT-ACCESS-CONTROL = '1'                              EL645
00632          IF AGROUPL  IS GREATER THAN  ZERO                        EL645
00633              MOVE AGROUPI        TO  PI-SC-GROUPING               EL645
00634          ELSE                                                     EL645
00635              MOVE ER-0005        TO  EMI-ERROR                    EL645
00636              MOVE AL-UNBOF       TO  AGROUPA                      EL645
00637              MOVE -1             TO  AGROUPL                      EL645
00638              PERFORM 5000-SEND-DATA-ONLY                          EL645
00639              GO TO 9100-RETURN-TRAN.                              EL645
00640                                                                   EL645
00641      IF AACCTL  IS GREATER THAN  ZERO                             EL645
00642          MOVE 'EL645C'           TO  PI-MAPNAME                   EL645
00643          MOVE AACCTI             TO  PI-SC-ACCOUNT                EL645
00644      ELSE                                                         EL645
00645          GO TO 0610-PROCESS-OPTION-5.                             EL645
00646                                                                   EL645
00647      IF PI-CERT-ACCESS-CONTROL = ' '  OR  '1'  OR  '2'            EL645
00648          IF ASTATEL  IS GREATER THAN  ZERO                        EL645
00649              MOVE ASTATEI        TO  PI-SC-STATE                  EL645
00650          ELSE                                                     EL645
00651              MOVE ER-0005        TO  EMI-ERROR                    EL645
00652              MOVE AL-UNBOF       TO  ASTATEA                      EL645
00653              MOVE -1             TO  ASTATEL                      EL645
00654              PERFORM 5000-SEND-DATA-ONLY                          EL645
00655              GO TO 9100-RETURN-TRAN.                              EL645
00656                                                                   EL645
00657  0610-PROCESS-OPTION-5.                                           EL645
00658      MOVE WS-LOSS-RATIO-DSID     TO  PI-DSID.                     EL645
00659      MOVE '5'                    TO  PI-OPTION.                   EL645
00660      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD.            EL645
00661      MOVE 'C'                    TO  PI-SC-RCD-TYPE.              EL645
00662      MOVE +47                    TO  PI-KEY-LENGTH.               EL645
00663      MOVE PI-SELECTION-CRITERIA  TO  PI-LOSS-RATIO-KEY            EL645
00664                                      PI-SAVE-LOSS-RATIO-KEY.      EL645
00665      MOVE -1                     TO  ACARRL.                      EL645
00666                                                                   EL645
00667      PERFORM 1000-READ-LOSS-RATIO.                                EL645
00668      GO TO 9200-XCTL.                                                CL**4
00669  EJECT                                                            EL645
00670  0700-OPTION-6-PROCESSING.                                        EL645
00671 ******************************************************************EL645
00672 *           O P T I O N  6  P R O C E S S I N G                  *EL645
00673 ******************************************************************EL645
00674                                                                   EL645
00675      MOVE LOW-VALUES             TO  PI-SELECTION-CRITERIA.       EL645
00676                                                                   EL645
00677      MOVE 'EL645B'               TO  PI-MAPNAME.                  EL645
00678                                                                   EL645
00679      IF ASTATEL  IS GREATER THAN  ZERO                            EL645
00680          MOVE ASTATEI            TO  PI-SC-STATE                  EL645
00681      ELSE                                                         EL645
00682          MOVE ER-0005            TO  EMI-ERROR                    EL645
00683          MOVE AL-UNBOF           TO  ASTATEA                      EL645
00684          MOVE -1                 TO  ASTATEL                      EL645
00685          PERFORM 5000-SEND-DATA-ONLY                              EL645
00686          GO TO 9100-RETURN-TRAN.                                  EL645
00687                                                                   EL645
00688  0710-PROCESS-OPTION-6.                                           EL645
00689      MOVE WS-LOSS-RATIO-DSID     TO  PI-DSID.                     EL645
00690      MOVE '6'                    TO  PI-OPTION.                   EL645
00691      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD.            EL645
00692      MOVE 'S'                    TO  PI-SC-RCD-TYPE.              EL645
00693      MOVE +47                    TO  PI-KEY-LENGTH.               EL645
00694      MOVE PI-SELECTION-CRITERIA  TO  PI-LOSS-RATIO-KEY            EL645
00695                                      PI-SAVE-LOSS-RATIO-KEY.      EL645
00696      MOVE -1                     TO  ASELL.                       EL645
00697                                                                   EL645
00698      PERFORM 1000-READ-LOSS-RATIO.                                EL645
00699      GO TO 9200-XCTL.                                                CL**4
00700  EJECT                                                            EL645
00701  0900-RESTORE-SCREEN.                                             EL645
00702      MOVE SPACE                  TO  PI-RETURN-SWITCH.            EL645
00703      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL645
00704                                      PI-LINE-COUNT                EL645
00705                                      PI-BROWSE-SW                 EL645
00706                                      PI-KEY-LENGTH                EL645
00707                                      PI-TS-ITEM                   EL645
00708                                      PI-END-OF-FILE               EL645
00709                                      PI-START-SW                  EL645
00710                                      PI-AIX-RECORD-COUNT.         EL645
00711      MOVE LOW-VALUES             TO  EL645AO.                     EL645
00712      MOVE PI-SAVE-LRK-REIN-CO    TO  AREINCOO.                    EL645
00713      MOVE PI-SAVE-LRK-RPT-CD-1   TO  ARPTCD1O.                    EL645
00714      MOVE PI-SAVE-LRK-CARRIER    TO  ACARRO.                      EL645
00715      MOVE PI-SAVE-LRK-GROUPING   TO  AGROUPO.                     EL645
00716      IF OPTION-FIVE-SELECTED                                      EL645
00717          MOVE PI-SAVE-LRK-GA-RPT-CD-2                             EL645
00718                                  TO  ARPTCD2O                     EL645
00719      ELSE                                                         EL645
00720          MOVE PI-SAVE-LRK-GA-RPT-CD-2                             EL645
00721                                  TO  AGENAGTO.                    EL645
00722      MOVE PI-SAVE-LRK-STATE      TO  ASTATEO.                     EL645
00723      MOVE PI-SAVE-LRK-ACCOUNT    TO  AACCTO.                      EL645
00724      MOVE AL-UANON               TO  ACTONLYA.                       CL**2
00725      MOVE PI-ACTIVE-ONLY         TO  ACTONLYO.                       CL**2
00726      MOVE -1                     TO  ACARRL.                      EL645
00727                                                                   EL645
00728      PERFORM 5000-SEND-DATA-ONLY.                                 EL645
00729                                                                   EL645
00730      GO TO 9100-RETURN-TRAN.                                      EL645
00731  EJECT                                                            EL645
00732  1000-READ-LOSS-RATIO  SECTION.                                   EL645
00733      EXEC CICS HANDLE CONDITION                                   EL645
00734          DUPKEY   (9200-XCTL)                                     EL645
00735          NOTFND   (1020-NOTFND)                                   EL645
00736          DSIDERR  (1010-DSIDERR)                                  EL645
00737      END-EXEC.                                                    EL645
00738                                                                   EL645
00739      MOVE +1                     TO  PI-START-SW.                 EL645
00740                                                                   EL645
00741      EXEC CICS READ                                               EL645
00742          DATASET    (PI-DSID)                                     EL645
00743          RIDFLD     (PI-LOSS-RATIO-KEY)                           EL645
00744          SET        (ADDRESS OF LOSS-RATIO-MASTER)                   CL**4
00745      END-EXEC.                                                    EL645
00746                                                                   EL645
00747      CONTINUE.                                                       CL**4
00748                                                                   EL645
00749      IF LR-COMPANY-CD  IS EQUAL TO  PI-COMPANY-CD                 EL645
00750          NEXT SENTENCE                                            EL645
00751      ELSE                                                         EL645
00752          GO TO 1020-NOTFND.                                       EL645
00753                                                                   EL645
00754      MOVE 'EL6451'               TO  THIS-PGM.                    EL645
00755                                                                   EL645
00756      GO TO 1099-EXIT.                                                CL**4
00757 *PEM GO TO 9200-XCTL.                                                CL**4
00758                                                                   EL645
00759  1010-DSIDERR.                                                    EL645
00760      MOVE ER-0671                TO  EMI-ERROR.                   EL645
00761                                                                   EL645
00762      PERFORM 5000-SEND-DATA-ONLY.                                 EL645
00763                                                                   EL645
00764      GO TO 9100-RETURN-TRAN.                                      EL645
00765                                                                   EL645
00766  1020-NOTFND.                                                     EL645
00767      MOVE ER-0674                TO  EMI-ERROR.                   EL645
00768                                                                   EL645
00769      PERFORM 5000-SEND-DATA-ONLY.                                 EL645
00770                                                                   EL645
00771      GO TO 9100-RETURN-TRAN.                                      EL645
00772                                                                   EL645
00773  1099-EXIT.                                                       EL645
00774      EXIT.                                                        EL645
00775  EJECT                                                            EL645
00776  3000-BUILD-SCREEN  SECTION.                                      EL645
00777 ******************************************************************EL645
00778 *          REBUILD ORIGNAL SCREEN AND ERROR MESSAGE IF EL6451    *EL645
00779 *          DID NOT FIND ANY CERTIFICATES DURING BROWSE OF FILE.  *EL645
00780 ******************************************************************EL645
00781                                                                   EL645
00782      IF EIBTRNID  IS EQUAL TO  WS-TRANS-ID                        EL645
00783          NEXT SENTENCE                                            EL645
00784      ELSE                                                         EL645
00785          GO TO 3099-EXIT.                                         EL645
00786                                                                   EL645
00787      MOVE LOW-VALUES             TO  EL645AO.                     EL645
00788      MOVE ER-0674                TO  EMI-ERROR.                   EL645
00789                                                                   EL645
00790      MOVE PI-SC-CARRIER          TO  ACARRO.                      EL645
00791      MOVE PI-SC-GROUPING         TO  AGROUPO.                     EL645
00792      MOVE PI-SC-STATE            TO  ASTATEO.                     EL645
00793      MOVE PI-SC-ACCOUNT          TO  AACCTO.                      EL645
00794                                                                   EL645
00795      IF OPTION-TWO-SELECTED                                       EL645
00796          GO TO 3020-OPTION-TWO.                                   EL645
00797                                                                   EL645
00798      IF OPTION-THREE-SELECTED                                     EL645
00799          GO TO 3030-OPTION-THREE.                                 EL645
00800                                                                   EL645
00801      IF OPTION-FOUR-SELECTED                                      EL645
00802          GO TO 3040-OPTION-FOUR.                                  EL645
00803                                                                   EL645
00804      IF OPTION-FIVE-SELECTED                                      EL645
00805          GO TO 3050-OPTION-FIVE.                                  EL645
00806                                                                   EL645
00807      IF OPTION-SIX-SELECTED                                       EL645
00808          GO TO 3060-OPTION-SIX.                                   EL645
00809                                                                   EL645
00810  3010-OPTION-ONE.                                                 EL645
00811      MOVE -1                     TO  ACARRL.                      EL645
00812      MOVE AL-UANON               TO  ACARRA.                      EL645
00813                                                                   EL645
00814      GO TO 3070-INITIALIZE-WORK-AREAS.                            EL645
00815                                                                   EL645
00816  3020-OPTION-TWO.                                                 EL645
00817      MOVE PI-SC-GA-RPT-CD-2      TO  AGENAGTO.                    EL645
00818      MOVE -1                     TO  AGENAGTL.                    EL645
00819      MOVE AL-UANON               TO  AGENAGTA.                    EL645
00820                                                                   EL645
00821      GO TO 3070-INITIALIZE-WORK-AREAS.                            EL645
00822                                                                   EL645
00823  3030-OPTION-THREE.                                               EL645
00824      MOVE PI-SC-REIN-CO          TO  AREINCOO.                    EL645
00825      MOVE -1                     TO  AREINCOL.                    EL645
00826      MOVE AL-UANON               TO  AREINCOA.                    EL645
00827                                                                   EL645
00828      GO TO 3070-INITIALIZE-WORK-AREAS.                            EL645
00829                                                                   EL645
00830  3040-OPTION-FOUR.                                                EL645
00831      MOVE PI-SC-RPT-CD-1         TO  ARPTCD1O.                    EL645
00832      MOVE -1                     TO  ARPTCD1L.                    EL645
00833      MOVE AL-UANON               TO  ARPTCD1A.                    EL645
00834                                                                   EL645
00835      GO TO 3070-INITIALIZE-WORK-AREAS.                            EL645
00836                                                                   EL645
00837  3050-OPTION-FIVE.                                                EL645
00838      MOVE PI-SC-GA-RPT-CD-2      TO  ARPTCD2O.                    EL645
00839      MOVE -1                     TO  ARPTCD2L.                    EL645
00840      MOVE AL-UANON               TO  ARPTCD2A.                    EL645
00841                                                                   EL645
00842      GO TO 3070-INITIALIZE-WORK-AREAS.                            EL645
00843                                                                   EL645
00844  3060-OPTION-SIX.                                                 EL645
00845      MOVE PI-SC-STATE            TO  ASTATEO.                     EL645
00846      MOVE -1                     TO  ASTATEL.                     EL645
00847      MOVE AL-UANON               TO  ASTATEA.                     EL645
00848                                                                   EL645
00849  3070-INITIALIZE-WORK-AREAS.                                      EL645
00850      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA         EL645
00851                                      PI-CONTROL-IN-PROGRESS.      EL645
00852      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL645
00853                                      PI-LINE-COUNT                EL645
00854                                      PI-BROWSE-SW                 EL645
00855                                      PI-KEY-LENGTH                EL645
00856                                      PI-TS-ITEM                   EL645
00857                                      PI-END-OF-FILE               EL645
00858                                      PI-START-SW                  EL645
00859                                      PI-AIX-RECORD-COUNT.         EL645
00860                                                                   EL645
00861      PERFORM 5000-SEND-DATA-ONLY.                                 EL645
00862                                                                   EL645
00863      GO TO 9100-RETURN-TRAN.                                      EL645
00864                                                                   EL645
00865  3099-EXIT.                                                       EL645
00866      EXIT.                                                        EL645
00867  EJECT                                                            EL645
00868  4000-SEND-INITIAL-MAP  SECTION.                                  EL645
00869      MOVE -1                     TO  ACARRL.                      EL645
00870      MOVE SAVE-DATE              TO  ADATEO.                      EL645
00871      MOVE EIBTIME                TO  TIME-IN.                     EL645
00872      MOVE TIME-OUT               TO  ATIMEO.                      EL645
00873      MOVE AL-UANON               TO  ACTONLYA.                       CL**2
00874      MOVE 'Y'                    TO  ACTONLYO.                       CL**2
00875                                                                   EL645
00876      IF EMI-ERROR  IS NOT EQUAL TO  ZERO                          EL645
00877          PERFORM 9500-ERROR-FORMAT                                EL645
00878      ELSE                                                         EL645
00879          IF TRANSACTION-SUCCESSFUL                                EL645
00880              PERFORM 9500-ERROR-FORMAT.                           EL645
00881                                                                   EL645
00882      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.                     EL645
00883      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.                     EL645
00884                                                                   EL645
00885      EXEC CICS SEND                                               EL645
00886          FROM    (EL645AO)                                        EL645
00887          MAPSET  (WS-MAPSET-NAME)                                 EL645
00888          MAP     (WS-MAP-NAME)                                    EL645
00889          CURSOR                                                   EL645
00890          ERASE                                                    EL645
00891      END-EXEC.                                                    EL645
00892                                                                   EL645
00893  4099-EXIT.                                                       EL645
00894      EXIT.                                                        EL645
00895  EJECT                                                            EL645
00896  5000-SEND-DATA-ONLY SECTION.                                     EL645
00897      MOVE SAVE-DATE              TO  ADATEO.                      EL645
00898      MOVE EIBTIME                TO  TIME-IN.                     EL645
00899      MOVE TIME-OUT               TO  ATIMEO.                      EL645
00900                                                                   EL645
00901      IF EMI-ERROR  IS NOT EQUAL TO  ZERO                          EL645
00902          PERFORM 9500-ERROR-FORMAT.                               EL645
00903                                                                   EL645
00904      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.                     EL645
00905      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.                     EL645
00906                                                                   EL645
00907      EXEC CICS SEND DATAONLY                                      EL645
00908          FROM    (EL645AO)                                        EL645
00909          MAPSET  (WS-MAPSET-NAME)                                 EL645
00910          MAP     (WS-MAP-NAME)                                    EL645
00911          CURSOR                                                   EL645
00912      END-EXEC.                                                    EL645
00913                                                                   EL645
00914  5099-EXIT.                                                       EL645
00915      EXIT.                                                        EL645
00916  EJECT                                                            EL645
00917  6000-SEND-TEXT  SECTION.                                         EL645
00918      EXEC CICS SEND TEXT                                          EL645
00919          FROM    (LOGOFF-TEXT)                                    EL645
00920          LENGTH  (LOGOFF-LENGTH)                                  EL645
00921          ERASE                                                    EL645
00922          FREEKB                                                   EL645
00923      END-EXEC.                                                    EL645
00924                                                                   EL645
00925      EXEC CICS RETURN                                             EL645
00926          END-EXEC.                                                EL645
00927                                                                   EL645
00928  6099-EXIT.                                                       EL645
00929      EXIT.                                                        EL645
00930  EJECT                                                            EL645
00931  7000-DATE-CONVERSION  SECTION.                                   EL645
00932      EXEC CICS LINK                                               EL645
00933          PROGRAM   ('ELDATCV')                                    EL645
00934          COMMAREA  (DATE-CONVERSION-DATA)                         EL645
00935          LENGTH    (DC-COMM-LENGTH)                               EL645
00936      END-EXEC.                                                    EL645
00937                                                                   EL645
00938  7099-EXIT.                                                       EL645
00939      EXIT.                                                        EL645
00940  EJECT                                                            EL645
00941  9000-RETURN-CICS  SECTION.                                       EL645
00942      MOVE 'EL005'                TO  THIS-PGM.                    EL645
00943      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL645
00944                                                                   EL645
00945      GO TO 9200-XCTL.                                             EL645
00946                                                                   EL645
00947  9099-EXIT.                                                       EL645
00948      EXIT.                                                        EL645
00949                                                                   EL645
00950  9100-RETURN-TRAN  SECTION.                                       EL645
00951      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL645
00952      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL645
00953                                                                   EL645
00954      EXEC CICS RETURN                                             EL645
00955          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL645
00956          LENGTH    (PI-COMM-LENGTH)                               EL645
00957          TRANSID   (WS-TRANS-ID)                                  EL645
00958      END-EXEC.                                                    EL645
00959                                                                   EL645
00960  9199-EXIT.                                                       EL645
00961      EXIT.                                                        EL645
00962                                                                   EL645
00963  9200-XCTL  SECTION.                                              EL645
00964      MOVE DFHENTER               TO  EIBAID.                      EL645
00965                                                                   EL645
00966      EXEC CICS XCTL                                               EL645
00967          PROGRAM   (THIS-PGM)                                     EL645
00968          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL645
00969          LENGTH    (PI-COMM-LENGTH)                               EL645
00970      END-EXEC.                                                    EL645
00971                                                                   EL645
00972  9299-EXIT.                                                       EL645
00973      EXIT.                                                        EL645
00974  EJECT                                                            EL645
00975  9300-CLEAR  SECTION.                                             EL645
00976      MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.                    EL645
00977                                                                   EL645
00978      GO TO 9200-XCTL.                                             EL645
00979                                                                   EL645
00980  9399-EXIT.                                                       EL645
00981      EXIT.                                                        EL645
00982                                                                   EL645
00983  9400-PGMIDERR  SECTION.                                          EL645
00984      EXEC CICS HANDLE CONDITION                                   EL645
00985          PGMIDERR  (6000-SEND-TEXT)                               EL645
00986      END-EXEC.                                                    EL645
00987                                                                   EL645
00988      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL645
00989                                      LOGOFF-PGM.                  EL645
00990      MOVE 'EL005'                TO  THIS-PGM.                    EL645
00991      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL645
00992      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL645
00993                                                                   EL645
00994      GO TO 9200-XCTL.                                             EL645
00995                                                                   EL645
00996  9499-EXIT.                                                       EL645
00997      EXIT.                                                        EL645
00998  EJECT                                                            EL645
00999  9500-ERROR-FORMAT  SECTION.                                      EL645
01000      EXEC CICS LINK                                               EL645
01001          PROGRAM   ('EL001')                                      EL645
01002          COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)                EL645
01003          LENGTH    (EMI-COMM-LENGTH)                              EL645
01004      END-EXEC.                                                    EL645
01005                                                                   EL645
01006  9599-EXIT.                                                       EL645
01007      EXIT.                                                        EL645
01008                                                                   EL645
01009  9600-ERROR  SECTION.                                             EL645
01010      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL645
01011                                                                   EL645
01012      EXEC CICS LINK                                               EL645
01013          PROGRAM   ('EL004')                                      EL645
01014          COMMAREA  (EMI-LINE1)                                    EL645
01015          LENGTH    (72)                                           EL645
01016      END-EXEC.                                                    EL645
01017                                                                   EL645
01018      PERFORM 5000-SEND-DATA-ONLY.                                 EL645
01019                                                                   EL645
01020      GO TO 9100-RETURN-TRAN.                                      EL645
01021                                                                   EL645
01022  9699-EXIT.                                                       EL645
01023      EXIT.                                                        EL645
01024                                                                   EL645
01025  9700-SECURITY-VIOLATION.                                         EL645
01026                                  COPY ELCSCTP.                    EL645
01027                                                                   EL645
01028  9799-EXIT.                                                       EL645
01029      EXIT.                                                        EL645
01030                                                                   EL645
01031  9999-LAST-PARAGRAPH  SECTION.                                    EL645
01032      GOBACK.                                                      EL645
