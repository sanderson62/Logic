00001  IDENTIFICATION DIVISION.                                         04/22/98
00002                                                                   EL6451
00003  PROGRAM-ID.                 EL6451.                                 LV008
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 02/12/96 08:01:37.                    CL**7
00007 *                            VMOD=2.007.                             CL**7
00008 *                                                                 EL6451
00008 *                                                                 EL6451
00009 *AUTHOR.        LOGIC INC.                                           CL**7
00010 *               DALLAS, TEXAS.                                       CL**7
00011                                                                   EL6451
00012 *DATE-COMPILED.                                                      CL**7
00013                                                                   EL6451
00014 *SECURITY.   *****************************************************   CL**7
00015 *            *                                                   *   CL**7
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00017 *            *                                                   *   CL**7
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00021 *            *                                                   *   CL**7
00022 *            *****************************************************   CL**7
00023                                                                   EL6451
00024 *REMARKS.                                                         EL6451
00025 *        THIS PROGRAM PROVIDES THE BROWSE NECESSARY FOR THE LOSS  EL6451
00026 *        RATIOS LOOK-UP.                                          EL6451
00027                                                                   EL6451
00028 *    SCREENS     - EL645B - LOSS RATIOS AS OF (MM/DD/YY)          EL6451
00029 *                  EL645C - LOSS RATIOS AS OF (MM/DD/YY)          EL6451
00030                                                                   EL6451
00031 *    ENTERED BY  - EL645 - LOSS RATIOS SELECTION MENU             EL6451
00032                                                                   EL6451
00033 *    EXIT TO     - EL645 - LOSS RATIOS SELECTION MENU             EL6451
00034                                                                   EL6451
00035 *    INPUT FILE  - ERLOSS - LOSS RATIOS                           EL6451
00036                                                                   EL6451
00037 *    OUTPUT FILE - NONE                                           EL6451
00038                                                                   EL6451
00039 *    COMMAREA    - PASSED.  THE CONTROL INFORMATION KEYED IN THE  EL6451
00040 *                  CALLING PROGRAM (EL645) IS PLACED IN THE       EL6451
00041 *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR        EL6451
00042 *                  REFERENCE BY THIS PROGRAM.                     EL6451
00043                                                                   EL6451
00044 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL645.  ON     EL6451
00045 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE EL6451
00046 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVEEL6451
00047 *                  ENTRIES (XCTL FROM CICS VIA EXR2) THE SCREEN   EL6451
00048 *                  WILL BE READ AND ACTION WILL BE BASED ON THE   EL6451
00049 *                  MAINTENANCE TYPE INDICATED.                    EL6451
00050  EJECT                                                            EL6451
00051  ENVIRONMENT DIVISION.                                            EL6451
00052  DATA DIVISION.                                                   EL6451
00053  WORKING-STORAGE SECTION.                                         EL6451
00054  77  FILLER  PIC  X(32) VALUE '********************************'. EL6451
00055  77  FILLER  PIC  X(32) VALUE '*   EL6451 WORKING STORAGE     *'. EL6451
00056  77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.007 *********'.    CL**7
00057                                                                   EL6451
00058                              COPY ELCSCTM.                           CL**5
00059                                                                   EL6451
00060                              COPY ELCSCRTY.                          CL**5
00061                                                                   EL6451
00062  01  WS-DATE-AREA.                                                EL6451
00063      12  SAVE-DATE               PIC  X(08)      VALUE SPACES.    EL6451
00064      12  SAVE-BIN-DATE           PIC  X(02)      VALUE SPACES.    EL6451
00065                                                                   EL6451
00066  01  FILLER              COMP-3.                                  EL6451
00067      12  TIME-IN                 PIC S9(07)      VALUE ZERO.      EL6451
00068      12  TIME-OUT  REDEFINES                                      EL6451
00069          TIME-IN                 PIC S9(03)V9(4).                 EL6451
00070                                                                   EL6451
00071  01  FILLER.                                                      EL6451
00072      12  WS-MAPSET-NAME              PIC  X(08)  VALUE 'EL6451S'. EL6451
00073      12  WS-MAP-B-NAME               PIC  X(08)  VALUE 'EL645B'.  EL6451
00074      12  WS-MAP-C-NAME               PIC  X(08)  VALUE 'EL645C'.  EL6451
00075      12  THIS-PGM                    PIC  X(08)  VALUE 'EL6451'.  EL6451
00076      12  WS-TRANS-ID                 PIC  X(04)  VALUE 'EXR2'.    EL6451
00077      12  WS-LR-RUN-DATE.                                          EL6451
00078          16  WS-LR-RUN-YR            PIC  X(02).                  EL6451
00079          16  WS-LR-RUN-MO            PIC  X(02).                  EL6451
00080          16  WS-LR-RUN-DA            PIC  X(02).                  EL6451
00081      12  WS-LR-RUN-DATE-EDIT.                                     EL6451
00082          16  WS-LR-RUN-MO-EDIT       PIC  X(02).                  EL6451
00083          16  FILLER                  PIC  X(01)  VALUE '/'.       EL6451
00084          16  WS-LR-RUN-DA-EDIT       PIC  X(02).                  EL6451
00085          16  FILLER                  PIC  X(01)  VALUE '/'.       EL6451
00086          16  WS-LR-RUN-YR-EDIT       PIC  X(02).                  EL6451
00087      12  WS-CALC-RDNXT               PIC S9(08) COMP VALUE ZERO.  EL6451
00088      12  WORK-COMM               PIC ZZ.ZZ.                          CL**4
00089      12  WORK-TABLE.                                                 CL**4
00090          16  FILLER              PIC X           VALUE SPACES.       CL**4
00091          16  WK-COM-TBL          PIC XXX         VALUE SPACES.       CL**4
00092          16  FILLER              PIC X           VALUE SPACES.       CL**4
00093      12  ERROR-MESSAGES.                                          EL6451
00094          16  ER-0002                 PIC  X(04)  VALUE '0002'.    EL6451
00095          16  ER-0004                 PIC  X(04)  VALUE '0004'.    EL6451
00096          16  ER-0008                 PIC  X(04)  VALUE '0008'.    EL6451
00097          16  ER-0029                 PIC  X(04)  VALUE '0029'.    EL6451
00098          16  ER-0130                 PIC  X(04)  VALUE '0130'.    EL6451
00099          16  ER-0200                 PIC  X(04)  VALUE '0200'.    EL6451
00100          16  ER-0673                 PIC  X(04)  VALUE '0673'.    EL6451
00101          16  ER-0685                 PIC  X(04)  VALUE '0685'.    EL6451
00102  EJECT                                                            EL6451
00103                                  COPY ELCINTF.                       CL**5
00104                                                                   EL6451
00105                                  COPY ELC645PI.                      CL**5
00106  EJECT                                                            EL6451
00107                                  COPY EL6451S.                       CL**5
00108  EJECT                                                            EL6451
00109                                  COPY ELCDATE.                       CL**5
00110  EJECT                                                            EL6451
00111                                  COPY ELCEMIB.                       CL**5
00112  EJECT                                                            EL6451
00113                                  COPY ELCLOGOF.                      CL**5
00114  EJECT                                                            EL6451
00115                                  COPY ELCATTR.                       CL**5
00116  EJECT                                                            EL6451
00117                                  COPY ELCAID.                        CL**5
00118                                                                   EL6451
00119  01  FILLER  REDEFINES  DFHAID.                                   EL6451
00120      12  FILLER                  PIC  X(08).                      EL6451
00121      12  PF-VALUES               PIC  X(01)                       EL6451
00122              OCCURS  24  TIMES.                                   EL6451
00123                                                                   EL6451
00124  LINKAGE SECTION.                                                 EL6451
00125                                                                   EL6451
00126  01  DFHCOMMAREA                 PIC  X(1024).                    EL6451
00127                                                                   EL6451
00128 *01 PARMLIST             COMP.                                       CL**7
00129 *    12  FILLER                  PIC S9(09).                         CL**7
00130 *    12  ERLOSS-POINTER          PIC S9(09).                         CL**7
00131  EJECT                                                            EL6451
00132                              COPY ERCLOSS.                           CL**5
00133  EJECT                                                            EL6451
00134  PROCEDURE DIVISION.                                              EL6451
00135                                                                   EL6451
00136      CONTINUE.                                                       CL**7
00137                                                                   EL6451
00138      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6451
00139      MOVE '5'                    TO  DC-OPTION-CODE.              EL6451
00140                                                                   EL6451
00141      PERFORM 8600-DATE-CONVERSION.                                EL6451
00142                                                                   EL6451
00143      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL6451
00144      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL6451
00145      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL6451
00146                                                                   EL6451
00147 *    NOTE ******************************************************* EL6451
00148 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL6451
00149 *         *  FROM ANOTHER MODULE.                               * EL6451
00150 *         *******************************************************.EL6451
00151                                                                   EL6451
00152      IF EIBCALEN  IS NOT GREATER THAN  ZERO                       EL6451
00153          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL6451
00154          PERFORM 8500-SEND-TEXT.                                  EL6451
00155                                                                   EL6451
00156      EXEC CICS HANDLE CONDITION                                   EL6451
00157          PGMIDERR  (9600-PGMIDERR)                                EL6451
00158          NOTFND    (8700-NOT-FOUND)                               EL6451
00159          ENDFILE   (1200-ENDFILE)                                 EL6451
00160          DUPKEY    (1010-DUPKEY)                                  EL6451
00161          ERROR     (9800-ERROR)                                   EL6451
00162      END-EXEC.                                                    EL6451
00163  EJECT                                                            EL6451
00164  0100-MAIN-LOGIC.                                                 EL6451
00165      IF PI-CALLING-PROGRAM  IS EQUAL TO  THIS-PGM                 EL6451
00166          GO TO 0110-MAIN-LOGIC.                                   EL6451
00167                                                                   EL6451
00168      IF PI-RETURN-TO-PROGRAM  IS NOT EQUAL TO  THIS-PGM           EL6451
00169          MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6        EL6451
00170          MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5        EL6451
00171          MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4        EL6451
00172          MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3        EL6451
00173          MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2        EL6451
00174          MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1        EL6451
00175          MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM      EL6451
00176          MOVE THIS-PGM              TO  PI-CALLING-PROGRAM        EL6451
00177      ELSE                                                         EL6451
00178          MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM        EL6451
00179          MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM      EL6451
00180          MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1        EL6451
00181          MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2        EL6451
00182          MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3        EL6451
00183          MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4        EL6451
00184          MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5        EL6451
00185          MOVE SPACES                TO  PI-SAVED-PROGRAM-6.       EL6451
00186                                                                   EL6451
00187      MOVE PI-LOSS-RATIO-KEY      TO  PI-1ST-KEY.                  EL6451
00188                                                                   EL6451
00189      PERFORM 5100-READ-DIRECT  THRU  5199-EXIT.                   EL6451
00190                                                                   EL6451
00191      CONTINUE.                                                       CL**7
00192                                                                      CL**3
00193      IF PI-MAPNAME  IS EQUAL TO  'EL645B  '                       EL6451
00194          PERFORM 1000-BUILD-SCREEN                                EL6451
00195      ELSE                                                         EL6451
00196          PERFORM 1100-BUILD-SCREEN.                               EL6451
00197  EJECT                                                            EL6451
00198  0110-MAIN-LOGIC.                                                 EL6451
00199 *    NOTE ******************************************************* EL6451
00200 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL6451
00201 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL6451
00202 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL6451
00203 *         *******************************************************.EL6451
00204                                                                   EL6451
00205      IF EIBAID  IS EQUAL TO  DFHCLEAR                             EL6451
00206          PERFORM 9400-CLEAR.                                      EL6451
00207                                                                   EL6451
00208      IF PI-MAPNAME  IS EQUAL TO  'EL645C  '                       EL6451
00209          GO TO 0200-RECEIVE-MAP-C.                                EL6451
00210                                                                   EL6451
00211                                                                   EL6451
00212  0120-RECEIVE-MAP-B.                                              EL6451
00213      MOVE LOW-VALUES             TO  EL645BI.                     EL6451
00214                                                                   EL6451
00215      IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3       EL6451
00216          MOVE LOW-VALUES         TO  EL645BO                      EL6451
00217          MOVE -1                 TO  BPFKL                        EL6451
00218          MOVE ER-0008            TO  EMI-ERROR                    EL6451
00219          PERFORM 8300-SEND-DATAONLY                               EL6451
00220          GO TO 9100-RETURN-TRAN.                                  EL6451
00221                                                                   EL6451
00222      EXEC CICS RECEIVE                                            EL6451
00223          MAPSET  (WS-MAPSET-NAME)                                 EL6451
00224          MAP     (WS-MAP-B-NAME)                                  EL6451
00225          INTO    (EL645BI)                                        EL6451
00226      END-EXEC.                                                    EL6451
00227                                                                   EL6451
00228      IF BPFKL  IS GREATER THAN  ZERO                              EL6451
00229          IF EIBAID  IS NOT EQUAL TO  DFHENTER                     EL6451
00230              MOVE ER-0004        TO  EMI-ERROR                    EL6451
00231              MOVE AL-UNBOF       TO  BPFKA                        EL6451
00232              MOVE -1             TO  BPFKL                        EL6451
00233              PERFORM 8300-SEND-DATAONLY                           EL6451
00234              GO TO 9100-RETURN-TRAN                               EL6451
00235          ELSE                                                     EL6451
00236              IF (BPFKI  IS NUMERIC)                               EL6451
00237                AND (BPFKI  IS GREATER THAN  ZERO                  EL6451
00238                    AND BPFKI  IS LESS THAN  25)                   EL6451
00239                      MOVE PF-VALUES (BPFKI)                       EL6451
00240                                  TO  EIBAID                       EL6451
00241                  ELSE                                             EL6451
00242                      MOVE ER-0029                                 EL6451
00243                                  TO  EMI-ERROR                    EL6451
00244                      MOVE AL-UNBOF                                EL6451
00245                                  TO  BPFKA                        EL6451
00246                      MOVE -1     TO  BPFKL                        EL6451
00247                      PERFORM 8300-SEND-DATAONLY                   EL6451
00248                      GO TO 9100-RETURN-TRAN.                      EL6451
00249                                                                   EL6451
00250 *    NOTE ******************************************************* EL6451
00251 *         *      PF KEY      USAGE                              * EL6451
00252 *         *        PF1       SEARCH FORWARD                     * EL6451
00253 *         *        PF2       SEARCH BACKWARD                    * EL6451
00254 *         *        PF3       ACCOUNT DETAIL                     * EL6451
00255 *         *        PF12      HELP                               * EL6451
00256 *         *        PF23      LOGOFF                             * EL6451
00257 *         *        PF24      RETURN TO MASTER MENU              * EL6451
00258 *         *******************************************************.EL6451
00259                                                                   EL6451
00260      IF EIBAID  IS EQUAL TO  DFHPF12                              EL6451
00261          MOVE 'EL010'            TO  THIS-PGM                     EL6451
00262          GO TO 9300-XCTL.                                         EL6451
00263                                                                   EL6451
00264      IF EIBAID  IS EQUAL TO  DFHPF23                              EL6451
00265          PERFORM 9000-RETURN-CICS.                                EL6451
00266                                                                   EL6451
00267      IF EIBAID  IS EQUAL TO  DFHPF24                              EL6451
00268          MOVE 'EL626'            TO  THIS-PGM                     EL6451
00269          GO TO 9300-XCTL.                                         EL6451
00270  SKIP3                                                            EL6451
00271  0130-MAP-B-LOGIC.                                                EL6451
00272                                                                   EL6451
00273      MOVE PI-SAVED-PROGRAM-1     TO  THIS-PGM.                    EL6451
00274                                                                   EL6451
00275      IF EIBAID  IS EQUAL TO  DFHPF1                               EL6451
00276          GO TO 0140-PF-KEY-ONE.                                   EL6451
00277                                                                   EL6451
00278      IF EIBAID  IS EQUAL TO  DFHPF2                               EL6451
00279          GO TO 0150-PF-KEY-TWO.                                   EL6451
00280                                                                   EL6451
00281      MOVE ER-0029                TO  EMI-ERROR.                   EL6451
00282      MOVE AL-UNBOF               TO  BPFKA.                       EL6451
00283      MOVE -1                     TO  BPFKL.                       EL6451
00284                                                                   EL6451
00285      PERFORM 8300-SEND-DATAONLY.                                  EL6451
00286                                                                   EL6451
00287      GO TO 9100-RETURN-TRAN.                                      EL6451
00288                                                                   EL6451
00289  0140-PF-KEY-ONE.                                                 EL6451
00290      PERFORM 5000-START-BROWSE  THRU  5099-EXIT.                  EL6451
00291                                                                   EL6451
00292      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.                     EL6451
00293                                                                   EL6451
00294      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.                     EL6451
00295                                                                   EL6451
00296      CONTINUE.                                                       CL**7
00297                                                                      CL**3
00298      PERFORM 1000-BUILD-SCREEN.                                   EL6451
00299                                                                   EL6451
00300  0150-PF-KEY-TWO.                                                 EL6451
00301      PERFORM 5000-START-BROWSE  THRU  5099-EXIT.                  EL6451
00302                                                                   EL6451
00303      PERFORM 5300-READ-PREVIOUS  THRU  5399-EXIT.                 EL6451
00304                                                                   EL6451
00305      PERFORM 5300-READ-PREVIOUS  THRU  5399-EXIT.                 EL6451
00306                                                                      CL**3
00307      CONTINUE.                                                       CL**7
00308                                                                   EL6451
00309      PERFORM 1000-BUILD-SCREEN.                                   EL6451
00310  EJECT                                                            EL6451
00311  0200-RECEIVE-MAP-C.                                              EL6451
00312      MOVE LOW-VALUES             TO  EL645CI.                     EL6451
00313                                                                   EL6451
00314      IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3       EL6451
00315          MOVE LOW-VALUES         TO  EL645CO                      EL6451
00316          MOVE -1                 TO  CPFKL                        EL6451
00317          MOVE ER-0008            TO  EMI-ERROR                    EL6451
00318          PERFORM 8400-SEND-DATAONLY                               EL6451
00319          GO TO 9100-RETURN-TRAN.                                  EL6451
00320                                                                   EL6451
00321      EXEC CICS RECEIVE                                            EL6451
00322          MAPSET  (WS-MAPSET-NAME)                                 EL6451
00323          MAP     (WS-MAP-C-NAME)                                  EL6451
00324          INTO    (EL645CI)                                        EL6451
00325      END-EXEC.                                                    EL6451
00326                                                                   EL6451
00327      IF CPFKL  IS GREATER THAN  ZERO                              EL6451
00328          IF EIBAID  IS NOT EQUAL TO  DFHENTER                     EL6451
00329              MOVE ER-0004        TO  EMI-ERROR                    EL6451
00330              MOVE AL-UNBOF       TO  CPFKA                        EL6451
00331              MOVE -1             TO  CPFKL                        EL6451
00332              PERFORM 8400-SEND-DATAONLY                           EL6451
00333              GO TO 9100-RETURN-TRAN                               EL6451
00334          ELSE                                                     EL6451
00335              IF (CPFKI  IS NUMERIC)                               EL6451
00336                AND (CPFKI  IS GREATER THAN  ZERO                  EL6451
00337                    AND CPFKI  IS LESS THAN  25)                   EL6451
00338                      MOVE PF-VALUES (CPFKI)                       EL6451
00339                                  TO  EIBAID                       EL6451
00340                  ELSE                                             EL6451
00341                      MOVE ER-0029                                 EL6451
00342                                  TO  EMI-ERROR                    EL6451
00343                      MOVE AL-UNBOF                                EL6451
00344                                  TO  CPFKA                        EL6451
00345                      MOVE -1     TO  CPFKL                        EL6451
00346                      PERFORM 8400-SEND-DATAONLY                   EL6451
00347                      GO TO 9100-RETURN-TRAN.                      EL6451
00348                                                                   EL6451
00349 *    NOTE ******************************************************* EL6451
00350 *         *      PF KEY      USAGE                              * EL6451
00351 *         *        PF1       SEARCH FORWARD                     * EL6451
00352 *         *        PF2       SEARCH BACKWARD                    * EL6451
00353 *         *        PF3       ACCOUNT DETAIL                     * EL6451
00354 *         *        PF12      HELP                               * EL6451
00355 *         *        PF23      LOGOFF                             * EL6451
00356 *         *        PF24      RETURN TO MASTER MENU              * EL6451
00357 *         *******************************************************.EL6451
00358                                                                   EL6451
00359      IF EIBAID  IS EQUAL TO  DFHPF12                              EL6451
00360          MOVE 'EL010'            TO  THIS-PGM                     EL6451
00361          GO TO 9300-XCTL.                                         EL6451
00362                                                                   EL6451
00363      IF EIBAID  IS EQUAL TO  DFHPF23                              EL6451
00364          PERFORM 9000-RETURN-CICS.                                EL6451
00365                                                                   EL6451
00366      IF EIBAID  IS EQUAL TO  DFHPF24                              EL6451
00367          MOVE 'EL626'            TO  THIS-PGM                     EL6451
00368          GO TO 9300-XCTL.                                         EL6451
00369  SKIP3                                                            EL6451
00370  0210-MAP-C-LOGIC.                                                EL6451
00371      MOVE PI-SAVED-PROGRAM-1     TO  THIS-PGM.                    EL6451
00372                                                                   EL6451
00373      IF EIBAID  IS EQUAL TO  DFHPF1                               EL6451
00374          GO TO 0220-PF-KEY-ONE.                                   EL6451
00375                                                                   EL6451
00376      IF EIBAID  IS EQUAL TO  DFHPF2                               EL6451
00377          GO TO 0250-PF-KEY-TWO.                                      CL**3
00378                                                                   EL6451
00379      MOVE ER-0029                TO  EMI-ERROR.                   EL6451
00380      MOVE AL-UNBOF               TO  CPFKA.                       EL6451
00381      MOVE -1                     TO  CPFKL.                       EL6451
00382                                                                   EL6451
00383      PERFORM 8400-SEND-DATAONLY.                                  EL6451
00384                                                                   EL6451
00385      GO TO 9100-RETURN-TRAN.                                      EL6451
00386                                                                   EL6451
00387  0220-PF-KEY-ONE.                                                 EL6451
00388      PERFORM 5000-START-BROWSE  THRU  5099-EXIT.                  EL6451
00389                                                                   EL6451
00390      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.                     EL6451
00391                                                                   EL6451
00392  0230-READ-NEXT.                                                     CL**3
00393      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.                     EL6451
00394                                                                      CL**3
00395      CONTINUE.                                                       CL**7
00396                                                                      CL**3
00397      IF OPTION-ONE-SELECTED                                          CL**3
00398        OR OPTION-TWO-SELECTED                                        CL**3
00399        OR OPTION-THREE-SELECTED                                      CL**3
00400          NEXT SENTENCE                                               CL**3
00401      ELSE                                                            CL**3
00402          PERFORM 1100-BUILD-SCREEN.                                  CL**3
00403                                                                      CL**3
00404      IF LR-ACCOUNT  IS EQUAL TO  LOW-VALUES                          CL**3
00405          PERFORM 1100-BUILD-SCREEN.                                  CL**3
00406                                                                      CL**3
00407      IF PI-ACCT-ACTIVE                                               CL**3
00408          IF ACCT-ACTIVE                                              CL**3
00409              NEXT SENTENCE                                           CL**3
00410          ELSE                                                        CL**3
00411              GO TO 0230-READ-NEXT.                                   CL**3
00412                                                                   EL6451
00413      PERFORM 1100-BUILD-SCREEN.                                   EL6451
00414                                                                   EL6451
00415  0250-PF-KEY-TWO.                                                    CL**3
00416      PERFORM 5000-START-BROWSE  THRU  5099-EXIT.                  EL6451
00417                                                                   EL6451
00418      PERFORM 5300-READ-PREVIOUS  THRU  5399-EXIT.                 EL6451
00419                                                                   EL6451
00420  0260-READ-PREVIOUS.                                                 CL**3
00421      PERFORM 5300-READ-PREVIOUS  THRU  5399-EXIT.                 EL6451
00422                                                                      CL**3
00423      CONTINUE.                                                       CL**7
00424                                                                      CL**3
00425      IF OPTION-ONE-SELECTED                                          CL**3
00426        OR OPTION-TWO-SELECTED                                        CL**3
00427        OR OPTION-THREE-SELECTED                                      CL**3
00428          NEXT SENTENCE                                               CL**3
00429      ELSE                                                            CL**3
00430          PERFORM 1100-BUILD-SCREEN.                                  CL**3
00431                                                                      CL**3
00432      IF LR-ACCOUNT  IS EQUAL TO  LOW-VALUES                          CL**3
00433          PERFORM 1100-BUILD-SCREEN.                                  CL**3
00434                                                                      CL**3
00435      IF PI-ACCT-ACTIVE                                               CL**3
00436          IF ACCT-ACTIVE                                              CL**3
00437              NEXT SENTENCE                                           CL**3
00438          ELSE                                                        CL**3
00439              GO TO 0260-READ-PREVIOUS.                               CL**3
00440                                                                   EL6451
00441      PERFORM 1100-BUILD-SCREEN.                                   EL6451
00442  EJECT                                                            EL6451
00443  1000-BUILD-SCREEN  SECTION.                                      EL6451
00444      MOVE LOW-VALUES             TO  EL645BO.                     EL6451
00445      MOVE PI-LOSS-RATIO-KEY      TO  PI-PREV-LOSS-RATIO-KEY.      EL6451
00446                                                                   EL6451
00447      GO TO 1020-NEXT-SENTENCE.                                    EL6451
00448                                                                   EL6451
00449  1010-DUPKEY.                                                     EL6451
00450      MOVE ER-0685                TO  EMI-ERROR.                   EL6451
00451      MOVE AL-UNBOF               TO  BPFKA.                       EL6451
00452      MOVE -1                     TO  BPFKL.                       EL6451
00453                                                                   EL6451
00454      PERFORM 8300-SEND-DATAONLY.                                  EL6451
00455                                                                   EL6451
00456      GO TO 9100-RETURN-TRAN.                                      EL6451
00457                                                                   EL6451
00458  1020-NEXT-SENTENCE.                                              EL6451
00459      IF LR-COMPANY-CD  IS EQUAL TO  PI-COMPANY-CD                 EL6451
00460          NEXT SENTENCE                                            EL6451
00461      ELSE                                                         EL6451
00462          GO TO 1200-ENDFILE.                                      EL6451
00463                                                                   EL6451
00464      IF OPTION-ONE-SELECTED                                       EL6451
00465          IF LR-RCD-TYPE  IS EQUAL TO  'A'                         EL6451
00466              GO TO 1030-CONTINUE                                  EL6451
00467          ELSE                                                     EL6451
00468              GO TO 1200-ENDFILE.                                  EL6451
00469                                                                   EL6451
00470      IF OPTION-TWO-SELECTED                                       EL6451
00471          IF LR-CARRIER  IS EQUAL TO  PI-SC-CARRIER                EL6451
00472            AND LR-GROUPING  IS EQUAL TO  PI-SC-GROUPING           EL6451
00473            AND LR-GA-RPT-CD-2  IS EQUAL TO  PI-SC-GA-RPT-CD-2     EL6451
00474            AND LR-RCD-TYPE  IS EQUAL TO  'G'                      EL6451
00475              GO TO 1030-CONTINUE                                  EL6451
00476          ELSE                                                     EL6451
00477              GO TO 1200-ENDFILE.                                  EL6451
00478                                                                   EL6451
00479      IF OPTION-THREE-SELECTED                                     EL6451
00480          IF LR-REIN-CO  IS EQUAL TO  PI-SC-REIN-CO                EL6451
00481            AND LR-RCD-TYPE  IS EQUAL TO  'R'                      EL6451
00482              GO TO 1030-CONTINUE                                  EL6451
00483          ELSE                                                     EL6451
00484              GO TO 1200-ENDFILE.                                  EL6451
00485                                                                   EL6451
00486      IF OPTION-FOUR-SELECTED                                      EL6451
00487          IF LR-RPT-CD-1  IS EQUAL TO  PI-SC-RPT-CD-1              EL6451
00488            AND LR-RCD-TYPE  IS EQUAL TO  'B'                      EL6451
00489              GO TO 1030-CONTINUE                                  EL6451
00490          ELSE                                                     EL6451
00491              GO TO 1200-ENDFILE.                                  EL6451
00492                                                                   EL6451
00493      IF OPTION-FIVE-SELECTED                                      EL6451
00494          IF LR-CARRIER  IS EQUAL TO  PI-SC-CARRIER                EL6451
00495            AND LR-GROUPING  IS EQUAL TO  PI-SC-GROUPING           EL6451
00496            AND LR-GA-RPT-CD-2  IS EQUAL TO  PI-SC-GA-RPT-CD-2     EL6451
00497            AND LR-RCD-TYPE  IS EQUAL TO  'C'                      EL6451
00498              GO TO 1030-CONTINUE                                  EL6451
00499          ELSE                                                     EL6451
00500              GO TO 1200-ENDFILE.                                  EL6451
00501                                                                   EL6451
00502      IF OPTION-SIX-SELECTED                                       EL6451
00503          IF LR-STATE  IS EQUAL TO  PI-SC-STATE                    EL6451
00504            AND LR-RCD-TYPE  IS EQUAL TO  'S'                      EL6451
00505              GO TO 1030-CONTINUE                                  EL6451
00506          ELSE                                                     EL6451
00507              GO TO 1200-ENDFILE.                                  EL6451
00508                                                                   EL6451
00509  1030-CONTINUE.                                                   EL6451
00510 ******************************************************************EL6451
00511 *        SECURITY CHECK FOR CARRIER AND ACCOUNT NUMBER           *EL6451
00512 *                        04/04/84                                *EL6451
00513 ******************************************************************EL6451
00514                                                                   EL6451
00515      IF OPTION-THREE-SELECTED                                     EL6451
00516        OR OPTION-FOUR-SELECTED                                    EL6451
00517        OR OPTION-SIX-SELECTED                                     EL6451
00518          GO TO 1070-MOVE-DATA.                                       CL**3
00519                                                                   EL6451
00520      IF PI-NO-CARRIER-SECURITY                                    EL6451
00521        AND PI-NO-ACCOUNT-SECURITY                                 EL6451
00522          GO TO 1070-MOVE-DATA.                                       CL**3
00523                                                                   EL6451
00524      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES              EL6451
00525          NEXT SENTENCE                                            EL6451
00526      ELSE                                                         EL6451
00527          GO TO 1050-CONTINUE.                                        CL**3
00528                                                                   EL6451
00529  1040-CHECK-CARRIER.                                                 CL**3
00530      IF LR-CARRIER  IS EQUAL TO  PI-CARRIER-SECURITY              EL6451
00531          GO TO 1050-CONTINUE.                                        CL**3
00532                                                                   EL6451
00533      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.                        CL**3
00534                                                                      CL**3
00535      CONTINUE.                                                       CL**7
00536                                                                      CL**3
00537      GO TO 1040-CHECK-CARRIER.                                       CL**3
00538                                                                      CL**3
00539  1050-CONTINUE.                                                      CL**3
00540      IF OPTION-TWO-SELECTED                                       EL6451
00541        OR OPTION-FIVE-SELECTED                                    EL6451
00542          GO TO 1070-MOVE-DATA.                                       CL**3
00543                                                                   EL6451
00544      IF PI-ACCOUNT-SECURITY  IS GREATER THAN  SPACES              EL6451
00545          NEXT SENTENCE                                            EL6451
00546      ELSE                                                         EL6451
00547          GO TO 1070-MOVE-DATA.                                       CL**3
00548                                                                   EL6451
00549  1060-CHECK-ACCOUNT.                                                 CL**3
00550      IF LR-ACCOUNT  IS EQUAL TO  PI-ACCOUNT-SECURITY              EL6451
00551          GO TO 1070-MOVE-DATA.                                       CL**3
00552                                                                   EL6451
00553      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.                        CL**3
00554                                                                      CL**3
00555      CONTINUE.                                                       CL**7
00556                                                                      CL**3
00557      GO TO 1060-CHECK-ACCOUNT.                                       CL**3
00558                                                                      CL**3
00559  1070-MOVE-DATA.                                                     CL**3
00560      MOVE PI-LOSS-RATIO-KEY      TO  PI-LIN1-LOSS-RATIO-KEY.      EL6451
00561      MOVE LR-CONTROL             TO  PI-END-LOSS-RATIO-KEY.       EL6451
00562      MOVE LR-RUN-DATE            TO  WS-LR-RUN-DATE.              EL6451
00563      MOVE WS-LR-RUN-YR           TO  WS-LR-RUN-YR-EDIT.           EL6451
00564      MOVE WS-LR-RUN-MO           TO  WS-LR-RUN-MO-EDIT.           EL6451
00565      MOVE WS-LR-RUN-DA           TO  WS-LR-RUN-DA-EDIT.           EL6451
00566      MOVE WS-LR-RUN-DATE-EDIT    TO  BASOFDTO.                    EL6451
00567                                                                   EL6451
00568      IF OPTION-TWO-SELECTED                                       EL6451
00569          MOVE 'GEN AGT:'         TO  BRPTNGO                      EL6451
00570          MOVE LR-GA-RPT-CD-2     TO  BTYPEO                       EL6451
00571          MOVE LR-G-A-NAME        TO  BNAMEO.                      EL6451
00572                                                                   EL6451
00573      IF OPTION-THREE-SELECTED                                     EL6451
00574          MOVE 'REIN CO:'         TO  BRPTNGO                      EL6451
00575          MOVE LR-REIN-CO         TO  BTYPEO                       EL6451
00576          MOVE LR-REIN-NAME       TO  BNAMEO.                      EL6451
00577                                                                      CL**2
00578      IF OPTION-FOUR-SELECTED                                         CL**6
00579          MOVE 'RPT CD1:'         TO  BRPTNGO                         CL**6
00580          MOVE LR-RPT-CD-1        TO  BTYPEO                          CL**6
00581          MOVE SPACES             TO  BNAMEO.                         CL**6
00582                                                                      CL**6
00583      IF OPTION-FIVE-SELECTED                                         CL**6
00584          MOVE 'RPT CD2:'         TO  BRPTNGO                         CL**6
00585          MOVE LR-GA-RPT-CD-2     TO  BTYPEO                          CL**6
00586          MOVE SPACES             TO  BNAMEO.                         CL**6
00587                                                                      CL**6
00588      IF OPTION-SIX-SELECTED                                          CL**2
00589          MOVE ' STATE :'         TO  BRPTNGO                         CL**2
00590          MOVE LR-STATE           TO  BTYPEO                          CL**2
00591          MOVE LR-ACCT-NAME       TO  BNAMEO.                         CL**2
00592                                                                   EL6451
00593      IF LR-YTD-NET (1)  IS NUMERIC                                EL6451
00594          MOVE LR-YTD-NET (1)     TO  BYTNETO                      EL6451
00595      ELSE                                                         EL6451
00596          MOVE ZEROS              TO  BYTNETO.                     EL6451
00597                                                                   EL6451
00598      IF LR-YTD-EARN (1)  IS NUMERIC                               EL6451
00599          MOVE LR-YTD-EARN (1)    TO  BYTEARNO                     EL6451
00600      ELSE                                                         EL6451
00601          MOVE ZEROS              TO  BYTEARNO.                    EL6451
00602                                                                   EL6451
00603      IF LR-YTD-PAID (1)  IS NUMERIC                               EL6451
00604          MOVE LR-YTD-PAID (1)    TO  BYTPAIDO                     EL6451
00605      ELSE                                                         EL6451
00606          MOVE ZEROS              TO  BYTPAIDO.                    EL6451
00607                                                                   EL6451
00608      IF LR-YTD-RESV (1)  IS NUMERIC                               EL6451
00609          MOVE LR-YTD-RESV (1)    TO  BYTRESO                      EL6451
00610      ELSE                                                         EL6451
00611          MOVE ZEROS              TO  BYTRESO.                     EL6451
00612                                                                   EL6451
00613      IF LR-YTD-INCUR (1)  IS NUMERIC                              EL6451
00614          MOVE LR-YTD-INCUR (1)   TO  BYTINCO                      EL6451
00615      ELSE                                                         EL6451
00616          MOVE ZEROS              TO  BYTINCO.                     EL6451
00617                                                                   EL6451
00618      IF LR-YTD-RATIO (1)  IS NUMERIC                              EL6451
00619          MOVE LR-YTD-RATIO (1)   TO  BYTRATOO                     EL6451
00620      ELSE                                                         EL6451
00621          MOVE ZEROS              TO  BYTRATOO.                    EL6451
00622                                                                   EL6451
00623      IF LR-ITD-NET (1)  IS NUMERIC                                EL6451
00624          MOVE LR-ITD-NET (1)     TO  BITNETO                      EL6451
00625      ELSE                                                         EL6451
00626          MOVE ZEROS              TO  BITNETO.                     EL6451
00627                                                                   EL6451
00628      IF LR-ITD-EARN (1)  IS NUMERIC                               EL6451
00629          MOVE LR-ITD-EARN (1)    TO  BITEARNO                     EL6451
00630      ELSE                                                         EL6451
00631          MOVE ZEROS              TO  BITEARNO.                    EL6451
00632                                                                   EL6451
00633      IF LR-ITD-PAID (1)  IS NUMERIC                               EL6451
00634          MOVE LR-ITD-PAID (1)    TO  BITPAIDO                     EL6451
00635      ELSE                                                         EL6451
00636          MOVE ZEROS              TO  BITPAIDO.                    EL6451
00637                                                                   EL6451
00638      IF LR-ITD-RESV (1)  IS NUMERIC                               EL6451
00639          MOVE LR-ITD-RESV (1)    TO  BITRESO                      EL6451
00640      ELSE                                                         EL6451
00641          MOVE ZEROS              TO  BITRESO.                     EL6451
00642                                                                   EL6451
00643      IF LR-ITD-INCUR (1)  IS NUMERIC                              EL6451
00644          MOVE LR-ITD-INCUR (1)   TO  BITINCO                      EL6451
00645      ELSE                                                         EL6451
00646          MOVE ZEROS              TO  BITINCO.                     EL6451
00647                                                                   EL6451
00648      IF LR-ITD-RATIO (1)  IS NUMERIC                              EL6451
00649          MOVE LR-ITD-RATIO (1)   TO  BITRATOO                     EL6451
00650      ELSE                                                         EL6451
00651          MOVE ZEROS              TO  BITRATOO.                    EL6451
00652                                                                   EL6451
00653      IF LR-YTD-NET (2)  IS NUMERIC                                EL6451
00654          MOVE LR-YTD-NET (2)     TO  BYLNETO                      EL6451
00655      ELSE                                                         EL6451
00656          MOVE ZEROS              TO  BYLNETO.                     EL6451
00657                                                                   EL6451
00658      IF LR-YTD-EARN (2)  IS NUMERIC                               EL6451
00659          MOVE LR-YTD-EARN (2)    TO  BYLEARNO                     EL6451
00660      ELSE                                                         EL6451
00661          MOVE ZEROS              TO  BYLEARNO.                    EL6451
00662                                                                   EL6451
00663      IF LR-YTD-PAID (2)  IS NUMERIC                               EL6451
00664          MOVE LR-YTD-PAID (2)    TO  BYLPAIDO                     EL6451
00665      ELSE                                                         EL6451
00666          MOVE ZEROS              TO  BYLPAIDO.                    EL6451
00667                                                                   EL6451
00668      IF LR-YTD-RESV (2)  IS NUMERIC                               EL6451
00669          MOVE LR-YTD-RESV (2)    TO  BYLRESO                      EL6451
00670      ELSE                                                         EL6451
00671          MOVE ZEROS              TO  BYLRESO.                     EL6451
00672                                                                   EL6451
00673      IF LR-YTD-INCUR (2)  IS NUMERIC                              EL6451
00674          MOVE LR-YTD-INCUR (2)   TO  BYLINCO                      EL6451
00675      ELSE                                                         EL6451
00676          MOVE ZEROS              TO  BYLINCO.                     EL6451
00677                                                                   EL6451
00678      IF LR-YTD-RATIO (2)  IS NUMERIC                              EL6451
00679          MOVE LR-YTD-RATIO (2)   TO  BYLRATOO                     EL6451
00680      ELSE                                                         EL6451
00681          MOVE ZEROS              TO  BYLRATOO.                    EL6451
00682                                                                   EL6451
00683      IF LR-ITD-NET (2)  IS NUMERIC                                EL6451
00684          MOVE LR-ITD-NET (2)     TO  BILNETO                      EL6451
00685      ELSE                                                         EL6451
00686          MOVE ZEROS              TO  BILNETO.                     EL6451
00687                                                                   EL6451
00688      IF LR-ITD-EARN (2)  IS NUMERIC                               EL6451
00689          MOVE LR-ITD-EARN (2)    TO  BILEARNO                     EL6451
00690      ELSE                                                         EL6451
00691          MOVE ZEROS              TO  BILEARNO.                    EL6451
00692                                                                   EL6451
00693      IF LR-ITD-PAID (2)  IS NUMERIC                               EL6451
00694          MOVE LR-ITD-PAID (2)    TO  BILPAIDO                     EL6451
00695      ELSE                                                         EL6451
00696          MOVE ZEROS              TO  BILPAIDO.                    EL6451
00697                                                                   EL6451
00698      IF LR-ITD-RESV (2)  IS NUMERIC                               EL6451
00699          MOVE LR-ITD-RESV (2)    TO  BILRESO                      EL6451
00700      ELSE                                                         EL6451
00701          MOVE ZEROS              TO  BILRESO.                     EL6451
00702                                                                   EL6451
00703      IF LR-ITD-INCUR (2)  IS NUMERIC                              EL6451
00704          MOVE LR-ITD-INCUR (2)   TO  BILINCO                      EL6451
00705      ELSE                                                         EL6451
00706          MOVE ZEROS              TO  BILINCO.                     EL6451
00707                                                                   EL6451
00708      IF LR-ITD-RATIO (2)  IS NUMERIC                              EL6451
00709          MOVE LR-ITD-RATIO (2)   TO  BILRATOO                     EL6451
00710      ELSE                                                         EL6451
00711          MOVE ZEROS              TO  BILRATOO.                    EL6451
00712                                                                   EL6451
00713      IF LR-YTD-NET (3)  IS NUMERIC                                EL6451
00714          MOVE LR-YTD-NET (3)     TO  BYANETO                      EL6451
00715      ELSE                                                         EL6451
00716          MOVE ZEROS              TO  BYANETO.                     EL6451
00717                                                                   EL6451
00718      IF LR-YTD-EARN (3)  IS NUMERIC                               EL6451
00719          MOVE LR-YTD-EARN (3)    TO  BYAEARNO                     EL6451
00720      ELSE                                                         EL6451
00721          MOVE ZEROS              TO  BYAEARNO.                    EL6451
00722                                                                   EL6451
00723      IF LR-YTD-PAID (3)  IS NUMERIC                               EL6451
00724          MOVE LR-YTD-PAID (3)    TO  BYAPAIDO                     EL6451
00725      ELSE                                                         EL6451
00726          MOVE ZEROS              TO  BYAPAIDO.                    EL6451
00727                                                                   EL6451
00728      IF LR-YTD-RESV (3)  IS NUMERIC                               EL6451
00729          MOVE LR-YTD-RESV (3)    TO  BYARESO                      EL6451
00730      ELSE                                                         EL6451
00731          MOVE ZEROS              TO  BYARESO.                     EL6451
00732                                                                   EL6451
00733      IF LR-YTD-INCUR (3)  IS NUMERIC                              EL6451
00734          MOVE LR-YTD-INCUR (3)   TO  BYAINCO                      EL6451
00735      ELSE                                                         EL6451
00736          MOVE ZEROS              TO  BYAINCO.                     EL6451
00737                                                                   EL6451
00738      IF LR-YTD-RATIO (3)  IS NUMERIC                              EL6451
00739          MOVE LR-YTD-RATIO (3)   TO  BYARATOO                     EL6451
00740      ELSE                                                         EL6451
00741          MOVE ZEROS              TO  BYARATOO.                    EL6451
00742                                                                   EL6451
00743      IF LR-ITD-NET (3)  IS NUMERIC                                EL6451
00744          MOVE LR-ITD-NET (3)     TO  BIANETO                      EL6451
00745      ELSE                                                         EL6451
00746          MOVE ZEROS              TO  BIANETO.                     EL6451
00747                                                                   EL6451
00748      IF LR-ITD-EARN (3)  IS NUMERIC                               EL6451
00749          MOVE LR-ITD-EARN (3)    TO  BIAEARNO                     EL6451
00750      ELSE                                                         EL6451
00751          MOVE ZEROS              TO  BIAEARNO.                    EL6451
00752                                                                   EL6451
00753      IF LR-ITD-PAID (3)  IS NUMERIC                               EL6451
00754          MOVE LR-ITD-PAID (3)    TO  BIAPAIDO                     EL6451
00755      ELSE                                                         EL6451
00756          MOVE ZEROS              TO  BIAPAIDO.                    EL6451
00757                                                                   EL6451
00758      IF LR-ITD-RESV (3)  IS NUMERIC                               EL6451
00759          MOVE LR-ITD-RESV (3)    TO  BIARESO                      EL6451
00760      ELSE                                                         EL6451
00761          MOVE ZEROS              TO  BIARESO.                     EL6451
00762                                                                   EL6451
00763      IF LR-ITD-INCUR (3)  IS NUMERIC                              EL6451
00764          MOVE LR-ITD-INCUR (3)   TO  BIAINCO                      EL6451
00765      ELSE                                                         EL6451
00766          MOVE ZEROS              TO  BIAINCO.                     EL6451
00767                                                                   EL6451
00768      IF LR-ITD-RATIO (3)  IS NUMERIC                              EL6451
00769          MOVE LR-ITD-RATIO (3)   TO  BIARATOO                     EL6451
00770      ELSE                                                         EL6451
00771          MOVE ZEROS              TO  BIARATOO.                    EL6451
00772                                                                   EL6451
00773      MOVE 'EL645C  '             TO  PI-MAPNAME.                  EL6451
00774      MOVE AL-UNNON               TO  BPFKA.                       EL6451
00775      MOVE -1                     TO  BPFKL.                       EL6451
00776                                                                   EL6451
00777      IF PI-BROWSE-SW  IS EQUAL TO  +1                             EL6451
00778          PERFORM 5400-END-BROWSE  THRU  5499-EXIT.                EL6451
00779                                                                   EL6451
00780      PERFORM 8300-SEND-DATAONLY.                                  EL6451
00781                                                                   EL6451
00782      GO TO 9100-RETURN-TRAN.                                      EL6451
00783  EJECT                                                            EL6451
00784  1100-BUILD-SCREEN.                                               EL6451
00785      MOVE LOW-VALUES             TO  EL645CO.                     EL6451
00786      MOVE PI-LOSS-RATIO-KEY      TO  PI-PREV-LOSS-RATIO-KEY.      EL6451
00787                                                                   EL6451
00788      IF LR-COMPANY-CD  IS EQUAL TO  PI-COMPANY-CD                 EL6451
00789          NEXT SENTENCE                                            EL6451
00790      ELSE                                                         EL6451
00791          GO TO 1200-ENDFILE.                                      EL6451
00792                                                                   EL6451
00793      IF OPTION-ONE-SELECTED                                       EL6451
00794          IF LR-RCD-TYPE  IS EQUAL TO  'A'                         EL6451
00795              GO TO 1105-CONTINUE                                     CL**3
00796          ELSE                                                     EL6451
00797              GO TO 1200-ENDFILE.                                  EL6451
00798                                                                   EL6451
00799      IF OPTION-TWO-SELECTED                                       EL6451
00800          IF LR-CARRIER  IS EQUAL TO  PI-SC-CARRIER                EL6451
00801            AND LR-GROUPING  IS EQUAL TO  PI-SC-GROUPING           EL6451
00802            AND LR-GA-RPT-CD-2  IS EQUAL TO  PI-SC-GA-RPT-CD-2     EL6451
00803            AND LR-RCD-TYPE  IS EQUAL TO  'G'                      EL6451
00804              GO TO 1105-CONTINUE                                     CL**3
00805          ELSE                                                     EL6451
00806              GO TO 1200-ENDFILE.                                  EL6451
00807                                                                   EL6451
00808      IF OPTION-THREE-SELECTED                                     EL6451
00809          IF LR-REIN-CO  IS EQUAL TO  PI-SC-REIN-CO                EL6451
00810            AND LR-RCD-TYPE  IS EQUAL TO  'R'                      EL6451
00811              GO TO 1105-CONTINUE                                     CL**3
00812          ELSE                                                     EL6451
00813              GO TO 1200-ENDFILE.                                  EL6451
00814                                                                   EL6451
00815      IF OPTION-FOUR-SELECTED                                      EL6451
00816          IF LR-RPT-CD-1  IS EQUAL TO  PI-SC-RPT-CD-1              EL6451
00817            AND LR-RCD-TYPE  IS EQUAL TO  'B'                      EL6451
00818              GO TO 1105-CONTINUE                                     CL**3
00819          ELSE                                                     EL6451
00820              GO TO 1200-ENDFILE.                                  EL6451
00821                                                                   EL6451
00822      IF OPTION-FIVE-SELECTED                                      EL6451
00823          IF LR-CARRIER  IS EQUAL TO  PI-SC-CARRIER                EL6451
00824            AND LR-GROUPING  IS EQUAL TO  PI-SC-GROUPING           EL6451
00825            AND LR-GA-RPT-CD-2  IS EQUAL TO  PI-SC-GA-RPT-CD-2     EL6451
00826            AND LR-RCD-TYPE  IS EQUAL TO  'C'                      EL6451
00827              GO TO 1105-CONTINUE                                     CL**3
00828          ELSE                                                     EL6451
00829              GO TO 1200-ENDFILE.                                  EL6451
00830                                                                   EL6451
00831      IF OPTION-SIX-SELECTED                                       EL6451
00832          IF LR-STATE  IS EQUAL TO  PI-SC-STATE                    EL6451
00833            AND LR-RCD-TYPE  IS EQUAL TO  'S'                      EL6451
00834              GO TO 1105-CONTINUE                                     CL**3
00835          ELSE                                                     EL6451
00836              GO TO 1200-ENDFILE.                                  EL6451
00837                                                                   EL6451
00838  1105-CONTINUE.                                                      CL**3
00839 ******************************************************************EL6451
00840 *        SECURITY CHECK FOR CARRIER AND ACCOUNT NUMBER           *EL6451
00841 *                        04/04/84                                *EL6451
00842 ******************************************************************EL6451
00843                                                                   EL6451
00844      IF OPTION-THREE-SELECTED                                     EL6451
00845        OR OPTION-FOUR-SELECTED                                    EL6451
00846        OR OPTION-SIX-SELECTED                                     EL6451
00847          GO TO 1135-MOVE-DATA.                                       CL**3
00848                                                                   EL6451
00849      IF PI-NO-CARRIER-SECURITY                                    EL6451
00850        AND PI-NO-ACCOUNT-SECURITY                                 EL6451
00851          GO TO 1135-MOVE-DATA.                                       CL**3
00852                                                                   EL6451
00853      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES              EL6451
00854          NEXT SENTENCE                                            EL6451
00855      ELSE                                                         EL6451
00856          GO TO 1120-CONTINUE.                                        CL**3
00857                                                                   EL6451
00858  1110-CHECK-CARRIER.                                                 CL**3
00859      IF LR-CARRIER  IS EQUAL TO  PI-CARRIER-SECURITY              EL6451
00860          GO TO 1120-CONTINUE.                                        CL**3
00861                                                                      CL**3
00862  1115-READ-NEXT.                                                     CL**3
00863      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.                        CL**3
00864                                                                      CL**3
00865      CONTINUE.                                                       CL**7
00866                                                                      CL**3
00867      IF OPTION-ONE-SELECTED                                          CL**3
00868        OR OPTION-TWO-SELECTED                                        CL**3
00869        OR OPTION-THREE-SELECTED                                      CL**3
00870          NEXT SENTENCE                                            EL6451
00871      ELSE                                                         EL6451
00872          GO TO 1110-CHECK-CARRIER.                                   CL**3
00873                                                                   EL6451
00874      IF LR-ACCOUNT  IS EQUAL TO  LOW-VALUES                          CL**3
00875          GO TO 1110-CHECK-CARRIER.                                   CL**3
00876                                                                      CL**3
00877      IF PI-ACCT-ACTIVE                                               CL**3
00878          IF ACCT-ACTIVE                                              CL**3
00879              NEXT SENTENCE                                           CL**3
00880          ELSE                                                        CL**3
00881              GO TO 1115-READ-NEXT.                                   CL**3
00882                                                                      CL**3
00883      GO TO 1110-CHECK-CARRIER.                                       CL**3
00884                                                                      CL**3
00885  1120-CONTINUE.                                                      CL**3
00886      IF OPTION-TWO-SELECTED                                       EL6451
00887        OR OPTION-FIVE-SELECTED                                    EL6451
00888          GO TO 1135-MOVE-DATA.                                       CL**3
00889                                                                   EL6451
00890      IF PI-ACCOUNT-SECURITY  IS GREATER THAN  SPACES              EL6451
00891          NEXT SENTENCE                                            EL6451
00892      ELSE                                                         EL6451
00893          GO TO 1135-MOVE-DATA.                                       CL**3
00894                                                                   EL6451
00895  1125-CHECK-ACCOUNT.                                                 CL**3
00896      IF LR-ACCOUNT  IS EQUAL TO  PI-ACCOUNT-SECURITY              EL6451
00897          GO TO 1135-MOVE-DATA.                                       CL**3
00898                                                                      CL**3
00899  1130-READ-NEXT.                                                     CL**3
00900      PERFORM 5200-READ-NEXT  THRU  5299-EXIT.                        CL**3
00901                                                                      CL**3
00902      CONTINUE.                                                       CL**7
00903                                                                      CL**3
00904      IF OPTION-ONE-SELECTED                                          CL**3
00905        OR OPTION-TWO-SELECTED                                        CL**3
00906        OR OPTION-THREE-SELECTED                                      CL**3
00907          NEXT SENTENCE                                            EL6451
00908      ELSE                                                         EL6451
00909          GO TO 1125-CHECK-ACCOUNT.                                   CL**3
00910                                                                   EL6451
00911      IF LR-ACCOUNT  IS EQUAL TO  LOW-VALUES                          CL**3
00912          GO TO 1125-CHECK-ACCOUNT.                                   CL**3
00913                                                                      CL**3
00914      IF PI-ACCT-ACTIVE                                               CL**3
00915          IF ACCT-ACTIVE                                              CL**3
00916              NEXT SENTENCE                                           CL**3
00917          ELSE                                                        CL**3
00918              GO TO 1130-READ-NEXT.                                   CL**3
00919                                                                      CL**3
00920      GO TO 1125-CHECK-ACCOUNT.                                       CL**3
00921                                                                      CL**3
00922  1135-MOVE-DATA.                                                     CL**3
00923      MOVE PI-LOSS-RATIO-KEY      TO  PI-LIN1-LOSS-RATIO-KEY.      EL6451
00924      MOVE LR-CONTROL             TO  PI-END-LOSS-RATIO-KEY.       EL6451
00925      MOVE LR-RUN-DATE            TO  WS-LR-RUN-DATE.              EL6451
00926      MOVE WS-LR-RUN-YR           TO  WS-LR-RUN-YR-EDIT.           EL6451
00927      MOVE WS-LR-RUN-MO           TO  WS-LR-RUN-MO-EDIT.           EL6451
00928      MOVE WS-LR-RUN-DA           TO  WS-LR-RUN-DA-EDIT.           EL6451
00929      MOVE WS-LR-RUN-DATE-EDIT    TO  CASOFDTO.                    EL6451
00930      MOVE 'ACCOUNT:'             TO  CRPTNGO.                     EL6451
00931      MOVE LR-ACCOUNT             TO  CTYPEO.                      EL6451
00932      MOVE LR-ACCT-NAME           TO  CNAMEO.                      EL6451
00933                                                                      CL**3
00934      IF ACCT-ACTIVE                                                  CL**3
00935          MOVE 'ACTIVE'           TO  CACTO                           CL**3
00936      ELSE                                                            CL**3
00937          MOVE 'INACTIVE'         TO  CACTO.                          CL**3
00938                                                                   EL6451
00939      IF OPTION-ONE-SELECTED                                       EL6451
00940          MOVE AL-PADOF           TO  CRPTNG2A                     EL6451
00941                                      CTYPE2A                      EL6451
00942                                      CNAME2A.                     EL6451
00943                                                                   EL6451
00944      IF OPTION-TWO-SELECTED                                       EL6451
00945          MOVE 'GEN AGT:'         TO  CRPTNG2O                     EL6451
00946          MOVE LR-GA-RPT-CD-2     TO  CTYPE2O                      EL6451
00947          MOVE LR-G-A-NAME        TO  CNAME2O.                     EL6451
00948                                                                   EL6451
00949      IF OPTION-THREE-SELECTED                                     EL6451
00950          MOVE 'REIN CO:'         TO  CRPTNG2O                     EL6451
00951          MOVE LR-REIN-CO         TO  CTYPE2O                      EL6451
00952          MOVE LR-REIN-NAME       TO  CNAME2O.                     EL6451
00953                                                                      CL**6
00954      IF OPTION-FOUR-SELECTED                                         CL**6
00955          MOVE 'RPT CD1:'         TO  CRPTNG2O                        CL**6
00956          MOVE LR-RPT-CD-1        TO  CTYPE2O                         CL**6
00957          MOVE SPACES             TO  CNAME2O.                        CL**6
00958                                                                      CL**6
00959      IF OPTION-FIVE-SELECTED                                         CL**6
00960          MOVE 'RPT CD2:'         TO  CRPTNG2O                        CL**6
00961          MOVE LR-GA-RPT-CD-2     TO  CTYPE2O                         CL**6
00962          MOVE SPACES             TO  CNAME2O.                        CL**6
00963                                                                   EL6451
00964      IF LR-YTD-NET (1)  IS NUMERIC                                EL6451
00965          MOVE LR-YTD-NET (1)     TO  CYTNETO                      EL6451
00966      ELSE                                                         EL6451
00967          MOVE ZEROS              TO  CYTNETO.                     EL6451
00968                                                                   EL6451
00969      IF LR-YTD-EARN (1)  IS NUMERIC                               EL6451
00970          MOVE LR-YTD-EARN (1)    TO  CYTEARNO                     EL6451
00971      ELSE                                                         EL6451
00972          MOVE ZEROS              TO  CYTEARNO.                    EL6451
00973                                                                   EL6451
00974      IF LR-YTD-PAID (1)  IS NUMERIC                               EL6451
00975          MOVE LR-YTD-PAID (1)    TO  CYTPAIDO                     EL6451
00976      ELSE                                                         EL6451
00977          MOVE ZEROS              TO  CYTPAIDO.                    EL6451
00978                                                                   EL6451
00979      IF LR-YTD-RESV (1)  IS NUMERIC                               EL6451
00980          MOVE LR-YTD-RESV (1)    TO  CYTRESO                      EL6451
00981      ELSE                                                         EL6451
00982          MOVE ZEROS              TO  CYTRESO.                     EL6451
00983                                                                   EL6451
00984      IF LR-YTD-INCUR (1)  IS NUMERIC                              EL6451
00985          MOVE LR-YTD-INCUR (1)   TO  CYTINCO                      EL6451
00986      ELSE                                                         EL6451
00987          MOVE ZEROS              TO  CYTINCO.                     EL6451
00988                                                                   EL6451
00989      IF LR-YTD-RATIO (1)  IS NUMERIC                              EL6451
00990          MOVE LR-YTD-RATIO (1)   TO  CYTRATOO                     EL6451
00991      ELSE                                                         EL6451
00992          MOVE ZEROS              TO  CYTRATOO.                    EL6451
00993                                                                   EL6451
00994      IF LR-ITD-NET (1)  IS NUMERIC                                EL6451
00995          MOVE LR-ITD-NET (1)     TO  CITNETO                      EL6451
00996      ELSE                                                         EL6451
00997          MOVE ZEROS              TO  CITNETO.                     EL6451
00998                                                                   EL6451
00999      IF LR-ITD-EARN (1)  IS NUMERIC                               EL6451
01000          MOVE LR-ITD-EARN (1)    TO  CITEARNO                     EL6451
01001      ELSE                                                         EL6451
01002          MOVE ZEROS              TO  CITEARNO.                    EL6451
01003                                                                   EL6451
01004      IF LR-ITD-PAID (1)  IS NUMERIC                               EL6451
01005          MOVE LR-ITD-PAID (1)    TO  CITPAIDO                     EL6451
01006      ELSE                                                         EL6451
01007          MOVE ZEROS              TO  CITPAIDO.                    EL6451
01008                                                                   EL6451
01009      IF LR-ITD-RESV (1)  IS NUMERIC                               EL6451
01010          MOVE LR-ITD-RESV (1)    TO  CITRESO                      EL6451
01011      ELSE                                                         EL6451
01012          MOVE ZEROS              TO  CITRESO.                     EL6451
01013                                                                   EL6451
01014      IF LR-ITD-INCUR (1)  IS NUMERIC                              EL6451
01015          MOVE LR-ITD-INCUR (1)   TO  CITINCO                      EL6451
01016      ELSE                                                         EL6451
01017          MOVE ZEROS              TO  CITINCO.                     EL6451
01018                                                                   EL6451
01019      IF LR-ITD-RATIO (1)  IS NUMERIC                              EL6451
01020          MOVE LR-ITD-RATIO (1)   TO  CITRATOO                     EL6451
01021      ELSE                                                         EL6451
01022          MOVE ZEROS              TO  CITRATOO.                    EL6451
01023                                                                   EL6451
01024      IF LR-YTD-NET (2)  IS NUMERIC                                EL6451
01025          MOVE LR-YTD-NET (2)     TO  CYLNETO                      EL6451
01026      ELSE                                                         EL6451
01027          MOVE ZEROS              TO  CYLNETO.                     EL6451
01028                                                                   EL6451
01029      IF LR-YTD-EARN (2)  IS NUMERIC                               EL6451
01030          MOVE LR-YTD-EARN (2)    TO  CYLEARNO                     EL6451
01031      ELSE                                                         EL6451
01032          MOVE ZEROS              TO  CYLEARNO.                    EL6451
01033                                                                   EL6451
01034      IF LR-YTD-PAID (2)  IS NUMERIC                               EL6451
01035          MOVE LR-YTD-PAID (2)    TO  CYLPAIDO                     EL6451
01036      ELSE                                                         EL6451
01037          MOVE ZEROS              TO  CYLPAIDO.                    EL6451
01038                                                                   EL6451
01039      IF LR-YTD-RESV (2)  IS NUMERIC                               EL6451
01040          MOVE LR-YTD-RESV (2)    TO  CYLRESO                      EL6451
01041      ELSE                                                         EL6451
01042          MOVE ZEROS              TO  CYLRESO.                     EL6451
01043                                                                   EL6451
01044      IF LR-YTD-INCUR (2)  IS NUMERIC                              EL6451
01045          MOVE LR-YTD-INCUR (2)   TO  CYLINCO                      EL6451
01046      ELSE                                                         EL6451
01047          MOVE ZEROS              TO  CYLINCO.                     EL6451
01048                                                                   EL6451
01049      IF LR-YTD-RATIO (2)  IS NUMERIC                              EL6451
01050          MOVE LR-YTD-RATIO (2)   TO  CYLRATOO                     EL6451
01051      ELSE                                                         EL6451
01052          MOVE ZEROS              TO  CYLRATOO.                    EL6451
01053                                                                   EL6451
01054      IF LR-ITD-NET (2)  IS NUMERIC                                EL6451
01055          MOVE LR-ITD-NET (2)     TO  CILNETO                      EL6451
01056      ELSE                                                         EL6451
01057          MOVE ZEROS              TO  CILNETO.                     EL6451
01058                                                                   EL6451
01059      IF LR-ITD-EARN (2)  IS NUMERIC                               EL6451
01060          MOVE LR-ITD-EARN (2)    TO  CILEARNO                     EL6451
01061      ELSE                                                         EL6451
01062          MOVE ZEROS              TO  CILEARNO.                    EL6451
01063                                                                   EL6451
01064      IF LR-ITD-PAID (2)  IS NUMERIC                               EL6451
01065          MOVE LR-ITD-PAID (2)    TO  CILPAIDO                     EL6451
01066      ELSE                                                         EL6451
01067          MOVE ZEROS              TO  CILPAIDO.                    EL6451
01068                                                                   EL6451
01069      IF LR-ITD-RESV (2)  IS NUMERIC                               EL6451
01070          MOVE LR-ITD-RESV (2)    TO  CILRESO                      EL6451
01071      ELSE                                                         EL6451
01072          MOVE ZEROS              TO  CILRESO.                     EL6451
01073                                                                   EL6451
01074      IF LR-ITD-INCUR (2)  IS NUMERIC                              EL6451
01075          MOVE LR-ITD-INCUR (2)   TO  CILINCO                      EL6451
01076      ELSE                                                         EL6451
01077          MOVE ZEROS              TO  CILINCO.                     EL6451
01078                                                                   EL6451
01079      IF LR-ITD-RATIO (2)  IS NUMERIC                              EL6451
01080          MOVE LR-ITD-RATIO (2)   TO  CILRATOO                     EL6451
01081      ELSE                                                         EL6451
01082          MOVE ZEROS              TO  CILRATOO.                    EL6451
01083                                                                   EL6451
01084      IF LR-YTD-NET (3)  IS NUMERIC                                EL6451
01085          MOVE LR-YTD-NET (3)     TO  CYANETO                      EL6451
01086      ELSE                                                         EL6451
01087          MOVE ZEROS              TO  CYANETO.                     EL6451
01088                                                                   EL6451
01089      IF LR-YTD-EARN (3)  IS NUMERIC                               EL6451
01090          MOVE LR-YTD-EARN (3)    TO  CYAEARNO                     EL6451
01091      ELSE                                                         EL6451
01092          MOVE ZEROS              TO  CYAEARNO.                    EL6451
01093                                                                   EL6451
01094      IF LR-YTD-PAID (3)  IS NUMERIC                               EL6451
01095          MOVE LR-YTD-PAID (3)    TO  CYAPAIDO                     EL6451
01096      ELSE                                                         EL6451
01097          MOVE ZEROS              TO  CYAPAIDO.                    EL6451
01098                                                                   EL6451
01099      IF LR-YTD-RESV (3)  IS NUMERIC                               EL6451
01100          MOVE LR-YTD-RESV (3)    TO  CYARESO                      EL6451
01101      ELSE                                                         EL6451
01102          MOVE ZEROS              TO  CYARESO.                     EL6451
01103                                                                   EL6451
01104      IF LR-YTD-INCUR (3)  IS NUMERIC                              EL6451
01105          MOVE LR-YTD-INCUR (3)   TO  CYAINCO                      EL6451
01106      ELSE                                                         EL6451
01107          MOVE ZEROS              TO  CYAINCO.                     EL6451
01108                                                                   EL6451
01109      IF LR-YTD-RATIO (3)  IS NUMERIC                              EL6451
01110          MOVE LR-YTD-RATIO (3)   TO  CYARATOO                     EL6451
01111      ELSE                                                         EL6451
01112          MOVE ZEROS              TO  CYARATOO.                    EL6451
01113                                                                   EL6451
01114      IF LR-ITD-NET (3)  IS NUMERIC                                EL6451
01115          MOVE LR-ITD-NET (3)     TO  CIANETO                      EL6451
01116      ELSE                                                         EL6451
01117          MOVE ZEROS              TO  CIANETO.                     EL6451
01118                                                                   EL6451
01119      IF LR-ITD-EARN (3)  IS NUMERIC                               EL6451
01120          MOVE LR-ITD-EARN (3)    TO  CIAEARNO                     EL6451
01121      ELSE                                                         EL6451
01122          MOVE ZEROS              TO  CIAEARNO.                    EL6451
01123                                                                   EL6451
01124      IF LR-ITD-PAID (3)  IS NUMERIC                               EL6451
01125          MOVE LR-ITD-PAID (3)    TO  CIAPAIDO                     EL6451
01126      ELSE                                                         EL6451
01127          MOVE ZEROS              TO  CIAPAIDO.                    EL6451
01128                                                                   EL6451
01129      IF LR-ITD-RESV (3)  IS NUMERIC                               EL6451
01130          MOVE LR-ITD-RESV (3)    TO  CIARESO                      EL6451
01131      ELSE                                                         EL6451
01132          MOVE ZEROS              TO  CIARESO.                     EL6451
01133                                                                   EL6451
01134      IF LR-ITD-INCUR (3)  IS NUMERIC                              EL6451
01135          MOVE LR-ITD-INCUR (3)   TO  CIAINCO                      EL6451
01136      ELSE                                                         EL6451
01137          MOVE ZEROS              TO  CIAINCO.                     EL6451
01138                                                                   EL6451
01139      IF LR-ITD-RATIO (3)  IS NUMERIC                              EL6451
01140          MOVE LR-ITD-RATIO (3)   TO  CIARATOO                     EL6451
01141      ELSE                                                         EL6451
01142          MOVE ZEROS              TO  CIARATOO.                    EL6451
01143                                                                   EL6451
01144      MOVE LR-EXP-DATE (1)        TO  WS-LR-RUN-DATE.              EL6451
01145      MOVE WS-LR-RUN-YR           TO  WS-LR-RUN-YR-EDIT.           EL6451
01146      MOVE WS-LR-RUN-MO           TO  WS-LR-RUN-MO-EDIT.           EL6451
01147      MOVE WS-LR-RUN-DA           TO  WS-LR-RUN-DA-EDIT.           EL6451
01148      MOVE WS-LR-RUN-DATE-EDIT    TO  CCEXPDTO.                    EL6451
01149      MOVE LR-REI-TAB (1)         TO  CCREINO.                     EL6451
01150      MOVE LR-RETRO (1)           TO  CCRETROO.                    EL6451
01151      MOVE LR-BASIS (1)           TO  CCBASISO.                    EL6451
01152                                                                   EL6451
01153      IF LR-AGT-NO (1 1) NOT EQUAL ZEROS AND SPACES                   CL**5
01154          NEXT SENTENCE                                            EL6451
01155      ELSE                                                         EL6451
01156          GO TO 1140-NEXT-AGENT.                                      CL**4
01157                                                                   EL6451
01158      MOVE LR-AGT-NO (1 1)        TO  CCAGT1O.                     EL6451
01159                                                                   EL6451
01160      IF LR-SNG-PCT (1 1)  IS NUMERIC                              EL6451
01161          COMPUTE WORK-COMM = LR-SNG-PCT (1 1) * +100              EL6451
01162          MOVE WORK-COMM          TO  CCSNG1O                      EL6451
01163      ELSE                                                         EL6451
01164          MOVE LR-SNG-PCT-X (1 1) TO  WK-COM-TBL                      CL**4
01165          MOVE WORK-TABLE         TO  CCSNG1O.                        CL**4
01166                                                                   EL6451
01167      IF LR-JNT-PCT (1 1)  IS NUMERIC                              EL6451
01168          COMPUTE WORK-COMM = LR-JNT-PCT (1 1) * +100              EL6451
01169          MOVE WORK-COMM          TO  CCJNT1O                      EL6451
01170      ELSE                                                         EL6451
01171          MOVE LR-JNT-PCT-X (1 1) TO  WK-COM-TBL                      CL**4
01172          MOVE WORK-TABLE         TO  CCJNT1O.                        CL**4
01173                                                                   EL6451
01174      IF LR-A-H-PCT (1 1)  IS NUMERIC                              EL6451
01175          COMPUTE WORK-COMM = LR-A-H-PCT (1 1) * +100              EL6451
01176          MOVE WORK-COMM          TO  CCAH1O                       EL6451
01177      ELSE                                                         EL6451
01178          MOVE LR-A-H-PCT-X (1 1) TO  WK-COM-TBL                      CL**4
01179          MOVE WORK-TABLE         TO  CCAH1O.                         CL**4
01180                                                                   EL6451
01181  1140-NEXT-AGENT.                                                    CL**3
01182                                                                      CL**4
01183      IF LR-AGT-NO (1 2) NOT EQUAL ZEROS AND SPACES                   CL**5
01184          NEXT SENTENCE                                            EL6451
01185      ELSE                                                         EL6451
01186          GO TO 1145-NEXT-AGENT.                                      CL**3
01187                                                                   EL6451
01188      MOVE LR-AGT-NO (1 2)        TO  CCAGT2O.                     EL6451
01189                                                                   EL6451
01190      IF LR-SNG-PCT (1 2)  IS NUMERIC                              EL6451
01191          COMPUTE WORK-COMM = LR-SNG-PCT (1 2) * +100              EL6451
01192          MOVE WORK-COMM          TO  CCSNG2O                      EL6451
01193      ELSE                                                         EL6451
01194          MOVE LR-SNG-PCT-X (1 2) TO  WK-COM-TBL                      CL**4
01195          MOVE WORK-TABLE         TO  CCSNG2O.                        CL**4
01196                                                                   EL6451
01197      IF LR-JNT-PCT (1 2)  IS NUMERIC                              EL6451
01198          COMPUTE WORK-COMM = LR-JNT-PCT (1 2) * +100              EL6451
01199          MOVE WORK-COMM          TO  CCJNT2O                      EL6451
01200      ELSE                                                         EL6451
01201          MOVE LR-JNT-PCT-X (1 2) TO  WK-COM-TBL                      CL**4
01202          MOVE WORK-TABLE         TO  CCJNT2O.                        CL**4
01203                                                                   EL6451
01204      IF LR-A-H-PCT (1 2)  IS NUMERIC                              EL6451
01205          COMPUTE WORK-COMM = LR-A-H-PCT (1 2) * +100              EL6451
01206          MOVE WORK-COMM          TO  CCAH2O                       EL6451
01207      ELSE                                                         EL6451
01208          MOVE LR-A-H-PCT-X (1 2) TO  WK-COM-TBL                      CL**4
01209          MOVE WORK-TABLE         TO  CCAH2O.                         CL**4
01210                                                                   EL6451
01211  1145-NEXT-AGENT.                                                    CL**3
01212                                                                      CL**4
01213      IF LR-AGT-NO (1 3) NOT EQUAL ZEROS AND SPACES                   CL**5
01214          NEXT SENTENCE                                            EL6451
01215      ELSE                                                         EL6451
01216          GO TO 1150-PREVIOUS.                                        CL**3
01217                                                                   EL6451
01218      MOVE LR-AGT-NO (1 3)        TO  CCAGT3O.                     EL6451
01219                                                                   EL6451
01220      IF LR-SNG-PCT (1 3)  IS NUMERIC                              EL6451
01221          COMPUTE WORK-COMM = LR-SNG-PCT (1 3) * +100              EL6451
01222          MOVE WORK-COMM          TO  CCSNG3O                      EL6451
01223      ELSE                                                         EL6451
01224          MOVE LR-SNG-PCT-X (1 3) TO  WK-COM-TBL                      CL**4
01225          MOVE WORK-TABLE         TO  CCSNG3O.                        CL**4
01226                                                                   EL6451
01227      IF LR-JNT-PCT (1 3)  IS NUMERIC                              EL6451
01228          COMPUTE WORK-COMM = LR-JNT-PCT (1 3) * +100              EL6451
01229          MOVE WORK-COMM          TO  CCJNT3O                      EL6451
01230      ELSE                                                         EL6451
01231          MOVE LR-JNT-PCT-X (1 3) TO  WK-COM-TBL                      CL**4
01232          MOVE WORK-TABLE         TO  CCJNT3O.                        CL**4
01233                                                                   EL6451
01234      IF LR-A-H-PCT (1 3)  IS NUMERIC                              EL6451
01235          COMPUTE WORK-COMM = LR-A-H-PCT (1 3) * +100              EL6451
01236          MOVE WORK-COMM          TO  CCAH3O                       EL6451
01237      ELSE                                                         EL6451
01238          MOVE LR-A-H-PCT-X (1 3) TO  WK-COM-TBL                      CL**4
01239          MOVE WORK-TABLE         TO  CCAH3O.                         CL**4
01240                                                                   EL6451
01241  1150-PREVIOUS.                                                      CL**3
01242                                                                      CL**4
01243      IF LR-EXP-DATE (2)  IS EQUAL TO  SPACES                      EL6451
01244        OR LR-EXP-DATE (2)  IS EQUAL TO  SPACES                    EL6451
01245          GO TO 1165-CONTINUE.                                        CL**3
01246                                                                   EL6451
01247      MOVE LR-EXP-DATE (2)        TO  WS-LR-RUN-DATE.              EL6451
01248      MOVE WS-LR-RUN-YR           TO  WS-LR-RUN-YR-EDIT.           EL6451
01249      MOVE WS-LR-RUN-MO           TO  WS-LR-RUN-MO-EDIT.           EL6451
01250      MOVE WS-LR-RUN-DA           TO  WS-LR-RUN-DA-EDIT.           EL6451
01251      MOVE WS-LR-RUN-DATE-EDIT    TO  CPEXPDTO.                    EL6451
01252      MOVE LR-REI-TAB (2)         TO  CPREINO.                     EL6451
01253      MOVE LR-RETRO (2)           TO  CPRETROO.                    EL6451
01254      MOVE LR-BASIS (2)           TO  CPBASISO.                    EL6451
01255                                                                   EL6451
01256      IF LR-AGT-NO (2 1) NOT EQUAL ZEROS AND SPACES                   CL**5
01257          NEXT SENTENCE                                            EL6451
01258      ELSE                                                         EL6451
01259          GO TO 1155-NEXT-AGENT.                                      CL**3
01260                                                                   EL6451
01261      MOVE LR-AGT-NO (2 1)        TO  CPAGT1O.                     EL6451
01262                                                                   EL6451
01263      IF LR-SNG-PCT (2 1)  IS NUMERIC                              EL6451
01264          COMPUTE WORK-COMM = LR-SNG-PCT (2 1) * +100              EL6451
01265          MOVE WORK-COMM          TO  CPSNG1O                      EL6451
01266      ELSE                                                         EL6451
01267          MOVE LR-SNG-PCT-X (2 1) TO  WK-COM-TBL                      CL**4
01268          MOVE WORK-TABLE         TO  CPSNG1O.                        CL**4
01269                                                                   EL6451
01270      IF LR-JNT-PCT (2 1)  IS NUMERIC                              EL6451
01271          COMPUTE WORK-COMM = LR-JNT-PCT (2 1) * +100              EL6451
01272          MOVE WORK-COMM          TO  CPJNT1O                      EL6451
01273      ELSE                                                         EL6451
01274          MOVE LR-JNT-PCT-X (2 1) TO  WK-COM-TBL                      CL**4
01275          MOVE WORK-TABLE         TO  CPJNT1O.                        CL**4
01276                                                                   EL6451
01277      IF LR-A-H-PCT (2 1)  IS NUMERIC                              EL6451
01278          COMPUTE WORK-COMM = LR-A-H-PCT (2 1) * +100              EL6451
01279          MOVE WORK-COMM          TO  CPAH1O                       EL6451
01280      ELSE                                                         EL6451
01281          MOVE LR-A-H-PCT-X (2 1) TO  WK-COM-TBL                      CL**4
01282          MOVE WORK-TABLE         TO  CPAH1O.                         CL**4
01283                                                                   EL6451
01284  1155-NEXT-AGENT.                                                    CL**3
01285                                                                      CL**4
01286      IF LR-AGT-NO (2 2) NOT EQUAL ZEROS AND SPACES                   CL**5
01287          NEXT SENTENCE                                            EL6451
01288      ELSE                                                         EL6451
01289          GO TO 1160-NEXT-AGENT.                                      CL**3
01290                                                                   EL6451
01291      MOVE LR-AGT-NO (2 2)        TO  CPAGT2O.                     EL6451
01292                                                                   EL6451
01293      IF LR-SNG-PCT (2 2)  IS NUMERIC                              EL6451
01294          COMPUTE WORK-COMM = LR-SNG-PCT (2 2) * +100              EL6451
01295          MOVE WORK-COMM          TO  CPSNG2O                      EL6451
01296      ELSE                                                         EL6451
01297          MOVE LR-SNG-PCT-X (2 2) TO  WK-COM-TBL                      CL**4
01298          MOVE WORK-TABLE         TO  CPSNG2O.                        CL**4
01299                                                                   EL6451
01300      IF LR-JNT-PCT (2 2)  IS NUMERIC                              EL6451
01301          COMPUTE WORK-COMM = LR-JNT-PCT (2 2) * +100              EL6451
01302          MOVE WORK-COMM          TO  CPJNT2O                      EL6451
01303      ELSE                                                         EL6451
01304          MOVE LR-JNT-PCT-X (2 2) TO  WK-COM-TBL                      CL**4
01305          MOVE WORK-TABLE         TO  CPJNT2O.                        CL**4
01306                                                                   EL6451
01307      IF LR-A-H-PCT (2 2)  IS NUMERIC                              EL6451
01308          COMPUTE WORK-COMM = LR-A-H-PCT (2 2) * +100              EL6451
01309          MOVE WORK-COMM          TO  CPAH2O                       EL6451
01310      ELSE                                                         EL6451
01311          MOVE LR-A-H-PCT-X (2 2) TO  WK-COM-TBL                      CL**4
01312          MOVE WORK-TABLE         TO  CPAH2O.                         CL**4
01313                                                                   EL6451
01314  1160-NEXT-AGENT.                                                    CL**3
01315                                                                      CL**4
01316      IF LR-AGT-NO (2 3) NOT EQUAL ZEROS AND SPACES                   CL**5
01317          NEXT SENTENCE                                            EL6451
01318      ELSE                                                         EL6451
01319          GO TO 1165-CONTINUE.                                        CL**3
01320                                                                   EL6451
01321      MOVE LR-AGT-NO (2 3)        TO  CPAGT3O.                     EL6451
01322                                                                   EL6451
01323      IF LR-SNG-PCT (2 3)  IS NUMERIC                              EL6451
01324          COMPUTE WORK-COMM = LR-SNG-PCT (2 3) * +100              EL6451
01325          MOVE WORK-COMM          TO  CPSNG3O                      EL6451
01326      ELSE                                                         EL6451
01327          MOVE LR-SNG-PCT-X (2 3) TO  WK-COM-TBL                      CL**4
01328          MOVE WORK-TABLE         TO  CPSNG3O.                        CL**4
01329                                                                   EL6451
01330      IF LR-JNT-PCT (2 3)  IS NUMERIC                              EL6451
01331          COMPUTE WORK-COMM = LR-JNT-PCT (2 3) * +100              EL6451
01332          MOVE WORK-COMM          TO  CPJNT3O                      EL6451
01333      ELSE                                                         EL6451
01334          MOVE LR-JNT-PCT-X (2 3) TO  WK-COM-TBL                      CL**4
01335          MOVE WORK-TABLE         TO  CPJNT3O.                        CL**4
01336                                                                   EL6451
01337      IF LR-A-H-PCT (2 3)  IS NUMERIC                              EL6451
01338          COMPUTE WORK-COMM = LR-A-H-PCT (2 3) * +100              EL6451
01339          MOVE WORK-COMM          TO  CPAH3O                       EL6451
01340      ELSE                                                         EL6451
01341          MOVE LR-A-H-PCT-X (2 3) TO  WK-COM-TBL                      CL**4
01342          MOVE WORK-TABLE         TO  CPAH3O.                         CL**4
01343                                                                   EL6451
01344  1165-CONTINUE.                                                      CL**3
01345                                                                   EL6451
01346      MOVE AL-UNNON               TO  CPFKA.                       EL6451
01347      MOVE -1                     TO  CPFKL.                       EL6451
01348                                                                   EL6451
01349      IF PI-BROWSE-SW  IS EQUAL TO  +1                             EL6451
01350          PERFORM 5400-END-BROWSE  THRU  5499-EXIT.                EL6451
01351                                                                   EL6451
01352      PERFORM 8400-SEND-DATAONLY.                                  EL6451
01353                                                                   EL6451
01354      GO TO 9100-RETURN-TRAN.                                      EL6451
01355                                                                   EL6451
01356  1200-ENDFILE.                                                    EL6451
01357      MOVE ER-0130                TO  EMI-ERROR.                   EL6451
01358                                                                   EL6451
01359      PERFORM 8300-SEND-DATAONLY.                                  EL6451
01360                                                                   EL6451
01361      GO TO 9100-RETURN-TRAN.                                      EL6451
01362                                                                   EL6451
01363  1499-EXIT.                                                       EL6451
01364      EXIT.                                                        EL6451
01365  EJECT                                                            EL6451
01366  5000-START-BROWSE.                                               EL6451
01367      EXEC CICS STARTBR                                            EL6451
01368          DATASET    (PI-DSID)                                     EL6451
01369          RIDFLD     (PI-LOSS-RATIO-KEY)                           EL6451
01370          GTEQ                                                     EL6451
01371          KEYLENGTH  (PI-KEY-LENGTH)                               EL6451
01372      END-EXEC.                                                    EL6451
01373                                                                   EL6451
01374      MOVE +1                     TO  PI-BROWSE-SW.                EL6451
01375                                                                   EL6451
01376  5099-EXIT.                                                       EL6451
01377      EXIT.                                                        EL6451
01378  EJECT                                                            EL6451
01379  5100-READ-DIRECT.                                                EL6451
01380      EXEC CICS READ                                               EL6451
01381          DATASET  (PI-DSID)                                       EL6451
01382          RIDFLD   (PI-LOSS-RATIO-KEY)                             EL6451
01383          SET      (ADDRESS OF LOSS-RATIO-MASTER)                     CL**7
01384          GTEQ                                                     EL6451
01385      END-EXEC.                                                    EL6451
01386                                                                   EL6451
01387  5199-EXIT.                                                       EL6451
01388      EXIT.                                                        EL6451
01389  EJECT                                                            EL6451
01390  5200-READ-NEXT.                                                  EL6451
01391      EXEC CICS READNEXT                                           EL6451
01392          DATASET  (PI-DSID)                                       EL6451
01393          RIDFLD   (PI-LOSS-RATIO-KEY)                             EL6451
01394          SET      (ADDRESS OF LOSS-RATIO-MASTER)                     CL**7
01395      END-EXEC.                                                    EL6451
01396                                                                   EL6451
01397  5299-EXIT.                                                       EL6451
01398      EXIT.                                                        EL6451
01399  EJECT                                                            EL6451
01400  5300-READ-PREVIOUS.                                              EL6451
01401      EXEC CICS READPREV                                           EL6451
01402          DATASET  (PI-DSID)                                       EL6451
01403          RIDFLD   (PI-LOSS-RATIO-KEY)                             EL6451
01404          SET      (ADDRESS OF LOSS-RATIO-MASTER)                     CL**7
01405      END-EXEC.                                                    EL6451
01406                                                                   EL6451
01407  5399-EXIT.                                                       EL6451
01408      EXIT.                                                        EL6451
01409  EJECT                                                            EL6451
01410  5400-END-BROWSE.                                                 EL6451
01411      EXEC CICS ENDBR                                              EL6451
01412          DATASET  (PI-DSID)                                       EL6451
01413      END-EXEC.                                                    EL6451
01414                                                                   EL6451
01415  5499-EXIT.                                                       EL6451
01416      EXIT.                                                        EL6451
01417  EJECT                                                            EL6451
01418  8100-SEND-INITIAL-MAP  SECTION.                                  EL6451
01419      MOVE SAVE-DATE              TO  BDATEO.                      EL6451
01420      MOVE EIBTIME                TO  TIME-IN.                     EL6451
01421      MOVE TIME-OUT               TO  BTIMEO.                      EL6451
01422      MOVE -1                     TO  BPFKL.                       EL6451
01423                                                                   EL6451
01424      IF EMI-ERROR  IS NOT EQUAL TO  ZERO                          EL6451
01425          PERFORM 9700-ERROR-FORMAT.                               EL6451
01426                                                                   EL6451
01427      MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O.                     EL6451
01428                                                                   EL6451
01429      EXEC CICS SEND                                               EL6451
01430          FROM    (EL645BO)                                        EL6451
01431          MAPSET  (WS-MAPSET-NAME)                                 EL6451
01432          MAP     (WS-MAP-B-NAME)                                  EL6451
01433          CURSOR                                                   EL6451
01434          ERASE                                                    EL6451
01435      END-EXEC.                                                    EL6451
01436                                                                   EL6451
01437  8199-EXIT.                                                       EL6451
01438      EXIT.                                                        EL6451
01439  EJECT                                                            EL6451
01440  8200-SEND-INITIAL-MAP  SECTION.                                  EL6451
01441      MOVE SAVE-DATE              TO  CDATEO.                      EL6451
01442      MOVE EIBTIME                TO  TIME-IN.                     EL6451
01443      MOVE TIME-OUT               TO  CTIMEO.                      EL6451
01444      MOVE -1                     TO  CPFKL.                       EL6451
01445                                                                   EL6451
01446      IF EMI-ERROR  IS NOT EQUAL TO  ZERO                          EL6451
01447          PERFORM 9700-ERROR-FORMAT.                               EL6451
01448                                                                   EL6451
01449      MOVE EMI-MESSAGE-AREA (1)   TO  CEMSG1O.                     EL6451
01450                                                                   EL6451
01451      EXEC CICS SEND                                               EL6451
01452          FROM    (EL645CO)                                        EL6451
01453          MAPSET  (WS-MAPSET-NAME)                                 EL6451
01454          MAP     (WS-MAP-C-NAME)                                  EL6451
01455          CURSOR                                                   EL6451
01456          ERASE                                                    EL6451
01457      END-EXEC.                                                    EL6451
01458                                                                   EL6451
01459  8299-EXIT.                                                       EL6451
01460      EXIT.                                                        EL6451
01461  EJECT                                                            EL6451
01462  8300-SEND-DATAONLY  SECTION.                                     EL6451
01463      MOVE SAVE-DATE              TO  BDATEO.                      EL6451
01464      MOVE EIBTIME                TO  TIME-IN.                     EL6451
01465      MOVE TIME-OUT               TO  BTIMEO.                      EL6451
01466                                                                   EL6451
01467      IF EMI-ERROR  IS NOT EQUAL TO  ZERO                          EL6451
01468          PERFORM 9700-ERROR-FORMAT.                               EL6451
01469                                                                   EL6451
01470      MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O.                     EL6451
01471                                                                   EL6451
01472      EXEC CICS SEND                                               EL6451
01473          FROM    (EL645BO)                                        EL6451
01474          MAPSET  (WS-MAPSET-NAME)                                 EL6451
01475          MAP     (WS-MAP-B-NAME)                                  EL6451
01476          CURSOR                                                   EL6451
01477          ERASE                                                    EL6451
01478      END-EXEC.                                                    EL6451
01479                                                                   EL6451
01480  8399-EXIT.                                                       EL6451
01481      EXIT.                                                        EL6451
01482  EJECT                                                            EL6451
01483  8400-SEND-DATAONLY  SECTION.                                     EL6451
01484      MOVE SAVE-DATE              TO  CDATEO.                      EL6451
01485      MOVE EIBTIME                TO  TIME-IN.                     EL6451
01486      MOVE TIME-OUT               TO  CTIMEO.                      EL6451
01487                                                                   EL6451
01488      IF EMI-ERROR  IS NOT EQUAL TO  ZERO                          EL6451
01489          PERFORM 9700-ERROR-FORMAT.                               EL6451
01490                                                                   EL6451
01491      MOVE EMI-MESSAGE-AREA (1)   TO  CEMSG1O.                     EL6451
01492                                                                   EL6451
01493      EXEC CICS SEND                                               EL6451
01494          FROM    (EL645CO)                                        EL6451
01495          MAPSET  (WS-MAPSET-NAME)                                 EL6451
01496          MAP     (WS-MAP-C-NAME)                                  EL6451
01497          CURSOR                                                   EL6451
01498          ERASE                                                    EL6451
01499      END-EXEC.                                                    EL6451
01500                                                                   EL6451
01501  8499-EXIT.                                                       EL6451
01502      EXIT.                                                        EL6451
01503  EJECT                                                            EL6451
01504  8500-SEND-TEXT  SECTION.                                         EL6451
01505      EXEC CICS SEND TEXT                                          EL6451
01506          FROM    (LOGOFF-TEXT)                                    EL6451
01507          LENGTH  (LOGOFF-LENGTH)                                  EL6451
01508          ERASE                                                    EL6451
01509          FREEKB                                                   EL6451
01510      END-EXEC.                                                    EL6451
01511                                                                   EL6451
01512      EXEC CICS RETURN                                             EL6451
01513      END-EXEC.                                                    EL6451
01514                                                                   EL6451
01515  8599-EXIT.                                                       EL6451
01516      EXIT.                                                        EL6451
01517                                                                   EL6451
01518  8600-DATE-CONVERSION  SECTION.                                   EL6451
01519      EXEC CICS LINK                                               EL6451
01520          PROGRAM   ('ELDATCV')                                    EL6451
01521          COMMAREA  (DATE-CONVERSION-DATA)                         EL6451
01522          LENGTH    (DC-COMM-LENGTH)                               EL6451
01523      END-EXEC.                                                    EL6451
01524                                                                   EL6451
01525  8699-EXIT.                                                       EL6451
01526      EXIT.                                                        EL6451
01527  EJECT                                                            EL6451
01528  8700-NOT-FOUND  SECTION.                                         EL6451
01529      MOVE -1                     TO  BPFKL.                       EL6451
01530      MOVE ER-0673                TO  EMI-ERROR.                   EL6451
01531                                                                   EL6451
01532      PERFORM 8100-SEND-INITIAL-MAP.                               EL6451
01533                                                                   EL6451
01534      GO TO 9100-RETURN-TRAN.                                      EL6451
01535                                                                   EL6451
01536  8799-EXIT.                                                       EL6451
01537      EXIT.                                                        EL6451
01538  EJECT                                                            EL6451
01539  9000-RETURN-CICS  SECTION.                                       EL6451
01540      MOVE 'EL005'                TO  THIS-PGM.                    EL6451
01541      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL6451
01542                                                                   EL6451
01543      GO TO 9300-XCTL.                                             EL6451
01544                                                                   EL6451
01545  9099-EXIT.                                                       EL6451
01546      EXIT.                                                        EL6451
01547                                                                   EL6451
01548  9100-RETURN-TRAN  SECTION.                                       EL6451
01549      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL6451
01550      MOVE '645B'                 TO  PI-CURRENT-SCREEN-NO.        EL6451
01551      MOVE EIBAID                 TO  PI-PREV-AID.                 EL6451
01552                                                                   EL6451
01553      EXEC CICS RETURN                                             EL6451
01554          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL6451
01555          LENGTH    (PI-COMM-LENGTH)                               EL6451
01556          TRANSID   (WS-TRANS-ID)                                  EL6451
01557      END-EXEC.                                                    EL6451
01558                                                                   EL6451
01559  9199-EXIT.                                                       EL6451
01560      EXIT.                                                        EL6451
01561                                                                   EL6451
01562  9300-XCTL  SECTION.                                              EL6451
01563      MOVE DFHENTER               TO  EIBAID.                      EL6451
01564                                                                   EL6451
01565      EXEC CICS XCTL                                               EL6451
01566          PROGRAM   (THIS-PGM)                                     EL6451
01567          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL6451
01568          LENGTH    (PI-COMM-LENGTH)                               EL6451
01569      END-EXEC.                                                    EL6451
01570                                                                   EL6451
01571  9399-EXIT.                                                       EL6451
01572      EXIT.                                                        EL6451
01573  EJECT                                                            EL6451
01574  9400-CLEAR  SECTION.                                             EL6451
01575      MOVE 'Y'                    TO  PI-RETURN-SWITCH.            EL6451
01576      MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.                    EL6451
01577                                                                   EL6451
01578      GO TO 9300-XCTL.                                             EL6451
01579                                                                   EL6451
01580  9499-EXIT.                                                       EL6451
01581      EXIT.                                                        EL6451
01582                                                                   EL6451
01583  9600-PGMIDERR  SECTION.                                          EL6451
01584      EXEC CICS HANDLE CONDITION                                   EL6451
01585          PGMIDERR  (8500-SEND-TEXT)                               EL6451
01586      END-EXEC.                                                    EL6451
01587                                                                   EL6451
01588      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL6451
01589                                      LOGOFF-PGM.                  EL6451
01590      MOVE 'EL005'                TO  THIS-PGM.                    EL6451
01591      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL6451
01592      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL6451
01593                                                                   EL6451
01594      GO TO 9300-XCTL.                                             EL6451
01595                                                                   EL6451
01596  9699-EXIT.                                                       EL6451
01597      EXIT.                                                        EL6451
01598  EJECT                                                            EL6451
01599  9700-ERROR-FORMAT  SECTION.                                      EL6451
01600      IF EMI-ERRORS-COMPLETE                                       EL6451
01601          GO TO 9799-EXIT.                                         EL6451
01602                                                                   EL6451
01603      EXEC CICS LINK                                               EL6451
01604          PROGRAM   ('EL001')                                      EL6451
01605          COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)                EL6451
01606          LENGTH    (EMI-COMM-LENGTH)                              EL6451
01607      END-EXEC.                                                    EL6451
01608                                                                   EL6451
01609  9799-EXIT.                                                       EL6451
01610      EXIT.                                                        EL6451
01611                                                                   EL6451
01612  9800-ERROR  SECTION.                                             EL6451
01613      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL6451
01614                                                                   EL6451
01615      EXEC CICS LINK                                               EL6451
01616          PROGRAM  ('EL004')                                       EL6451
01617          COMMAREA (EMI-LINE1)                                     EL6451
01618          LENGTH   (72)                                            EL6451
01619      END-EXEC.                                                    EL6451
01620                                                                   EL6451
01621      MOVE -1                     TO  BPFKL.                       EL6451
01622                                                                   EL6451
01623      PERFORM 8100-SEND-INITIAL-MAP.                               EL6451
01624                                                                   EL6451
01625      GO TO 9100-RETURN-TRAN.                                      EL6451
01626                                                                   EL6451
01627  9899-EXIT.                                                       EL6451
01628      EXIT.                                                        EL6451
01629                                                                   EL6451
01630  9999-LAST-PARAGRAPH  SECTION.                                    EL6451
01631      GOBACK.                                                      EL6451
