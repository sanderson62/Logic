00001  IDENTIFICATION DIVISION.                                         05/28/98
00002                                                                   EL125
00003  PROGRAM-ID.                 EL125 .                                 LV039
00004 *              PROGRAM CONVERTED BY                                  CL*32
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*32
00006 *              CONVERSION DATE 03/23/95 14:34:59.                    CL*32
00007 *                            VMOD=2.038.                             CL*38
00008 *                                                                 EL125
00008 *                                                                 EL125
00009 *AUTHOR.        LOGIC, INC.                                          CL*32
00010 *               DALLAS, TEXAS.                                       CL*32
00011                                                                   EL125
00012 *DATE-COMPILED.                                                      CL*32
00013 *SECURITY.   *****************************************************   CL*32
00014 *            *                                                   *   CL*32
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*32
00016 *            *                                                   *   CL*32
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*32
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*32
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*32
00020 *            *                                                   *   CL*32
00021 *            *****************************************************   CL*32
011812******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
032612* 032612    2011110200001  PEMA  AHL CHANGES
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
011812******************************************************************
00022                                                                   EL125
00023 *                                                                    CL**8
00024 *REMARKS. TRANSACTION EXCR - LOGON AND SECURITY.                     CL*25
00025      EJECT                                                        EL125
00026  ENVIRONMENT DIVISION.                                            EL125
00027  DATA DIVISION.                                                   EL125
00028  WORKING-STORAGE SECTION.                                         EL125
00029  77  FILLER  PIC X(32)  VALUE '********************************'. EL125
00030  77  FILLER  PIC X(32)  VALUE '*   EL125  WORKING STORAGE     *'. EL125
00031  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.038 **********'.    CL*38
00032                                                                      CL**6
00033  77  SYS-IDX                   PIC S9(04) VALUE +0 COMP.             CL**6
00034  77  APP-IDX                   PIC S9(04) VALUE +0 COMP.             CL**6
00035  77  WS-MORTG-ACCESS-CONTROL   PIC X VALUE SPACE.                    CL**8
00036  77  WS-MORTG-LABEL-CONTROL    PIC X VALUE 'N'.                      CL*13
00037  77  WS-MORTG-BILL-GRP-CODE    PIC X VALUE 'N'.                      CL*14
00038  77  WS-RATE-DEV-AUTHORIZATION PIC X VALUE 'N'.                      CL*16
00039  77  W-POLICY-LINKAGE-IND      PIC X VALUE 'N'.                      CL*31
00040                                                                      CL*10
00041  77  WS-APPLID                 PIC X(8).                             CL*10
00042                                                                      CL*10
00043  01  WS-USERID.                                                      CL*10
00044      06  WS-USERID-2           PIC X(2).                             CL*17
00045      06  WS-USERID-FILL        PIC X(6).                             CL*17
00046                                                                      CL*17
00047  01  WS-SIGNON-WORK.                                                 CL*17
00048      06  WS-TERMINAL-TYPE      PIC S9(9)  COMP  VALUE ZERO.          CL*17
00049      06  WS-TERMID                              VALUE LOW-VALUES.    CL*17
00050          12  WS-TERMID-CHAR    PIC X  OCCURS 4 TIMES.                CL*17
00051      06  WS-MY-USERID                           VALUE LOW-VALUES.    CL*17
00052          12  WS-MY-USERID-4.                                         CL*17
00053              18  WS-MY-USERID-2    PIC X(2).                         CL*17
00054              18  FILLER            PIC X(2).                         CL*17
00055          12  WS-MY-USERID-FILL     PIC X(4).                         CL*17
00056      06  WS-CURRENT-TERM-ON        PIC X(4).                         CL*19
00057                                                                   EL125
00058  01  WS-AREA.                                                     EL125
00059      12  TIME-IN                     PIC S9(7).                      CL**8
00060      12  FILLER REDEFINES TIME-IN.                                EL125
00061         16  FILLER                   PIC X.                          CL**8
00062         16  TIME-OUT                 PIC 99V99.                      CL**8
00063         16  FILLER                   PIC XX.                         CL**8
00064      12  EXCR                        PIC X(4)    VALUE 'EXCR'.       CL**8
00065      12  THIS-PGM                    PIC X(8)    VALUE 'EL125'.      CL**8
00066      12  LGXX-USER                   PIC X(4)    VALUE 'LGXX'.       CL**8
00067      12  LIT-USER-PASSWORD           PIC X(11)   VALUE 'KNI '.       CL*30
00068      12  LIT-COMP-PASSWORD           PIC X(8)    VALUE 'KIGOL'.      CL*30
00069      12  LIT-LOGOFF                  PIC X(8)    VALUE 'EL005'.      CL**8
00070      12  LIT-CREDIT-MASTER           PIC X(8)    VALUE 'EL626'.      CL**8
00071      12  LIT-CREDIT-MASTER-TRNID     PIC X(4)    VALUE 'EXA4'.       CL**8
00072      12  LIT-MORTGAGE-MASTER         PIC X(8)    VALUE 'EM626'.      CL**8
00073      12  LIT-MORTGAGE-MASTER-TRNID   PIC X(4)    VALUE 'MXA4'.       CL**8
00074      12  LIT-CLAIM-MASTER            PIC X(8)    VALUE 'EL126'.      CL**8
00075      12  LIT-CLAIM-MASTER-TRNID      PIC X(4)    VALUE 'EX00'.       CL**8
00076      12  ENTRY-SW                    PIC X       VALUE SPACE.        CL**8
00077          88  ENTRY-VIA-MAIN-MENU                 VALUE 'Y'.          CL**8
00078      12  WS-CURRENT-DATE             PIC X(8)    VALUE SPACES.       CL**8
00079      12  WS-CONTROL-FILE-SAVE        PIC X(750).                     CL*23
00080      12  WS-TERMINAL-PREFIX          PIC X(2)    VALUE SPACES.       CL**8
00081                                                                   EL125
00082      12  QID.                                                     EL125
00083          16  QID-TERM            PIC X(4).                        EL125
00084          16  QID-RCD             PIC X(4)    VALUE '125A'.           CL**6
00085      12  QID-PROC-AREA           PIC XXX.                         EL125
00086      12  QID-LENGTH              PIC S9(4)   VALUE +3  COMP.      EL125
00087      12  QID-ITEM                PIC S9(4)   VALUE +1  COMP.      EL125
00088                                                                   EL125
00089      EJECT                                                           CL**8
00090                                COPY ELCSCRTY.                        CL*24
00091                                                                      CL**8
00092                                COPY ELCSCRT3.                        CL*24
00093                                                                      CL**6
00094                                COPY ELCSCRT4.                        CL*24
00095                                                                   EL125
00096      EJECT                                                           CL**8
00097                                COPY MPCSCRT.                         CL*24
00098                                                                      CL**8
00099      EJECT                                                           CL**8
00100  01  EDIT-WORK-AREA.                                              EL125
00101      12  COUNT-1                 PIC 999.                         EL125
00102      12  HOLD-TERM               PIC X(4).                        EL125
00103      12  CALL-PGM                PIC X(8).                        EL125
00104                                                                   EL125
00105  01  ERROR-NUMBERS.                                               EL125
00106      12  ER-0007                 PIC X(4)   VALUE '0007'.         EL125
00107      12  ER-0008                 PIC X(4)   VALUE '0008'.         EL125
00108      12  ER-0016                 PIC X(4)   VALUE '0016'.         EL125
00109      12  ER-0017                 PIC X(4)   VALUE '0017'.         EL125
00110      12  ER-0018                 PIC X(4)   VALUE '0018'.         EL125
00111      12  ER-0019                 PIC X(4)   VALUE '0019'.         EL125
00112      12  ER-0020                 PIC X(4)   VALUE '0020'.         EL125
00113      12  ER-0029                 PIC X(4)   VALUE '0029'.         EL125
00114      12  ER-2516                 PIC X(4)   VALUE '2516'.         EL125
00115      12  ER-2517                 PIC X(4)   VALUE '2517'.         EL125
00116      12  ER-2519                 PIC X(4)   VALUE '2519'.         EL125
00117      12  ER-2536                 PIC X(4)   VALUE '2536'.         EL125
00118      12  ER-2566                 PIC X(4)   VALUE '2566'.         EL125
00119      12  ER-2567                 PIC X(4)   VALUE '2567'.         EL125
00120      12  ER-2568                 PIC X(4)   VALUE '2568'.         EL125
00121      12  ER-2569                 PIC X(4)   VALUE '2569'.         EL125
00122      12  ER-7000                 PIC X(4)   VALUE '7000'.         EL125
00123      12  ER-7008                 PIC X(4)   VALUE '7080'.         EL125
00124      12  ER-9000                 PIC X(4)   VALUE '9000'.            CL**8
00125      12  ER-9002                 PIC X(4)   VALUE '9002'.            CL**8
00126      12  ER-9303                 PIC X(4)   VALUE '9303'.            CL**9
00127                                                                   EL125
00128  01  ERROR-SWITCHES.                                              EL125
00129      12  ERROR-SWITCH            PIC X.                           EL125
00130          88  SCREEN-ERROR                    VALUE 'X'.           EL125
00131          88  LOGON-ERROR                     VALUE 'Y'.           EL125
00132                                                                   EL125
00133  01  FILE-READ-KEY.                                               EL125
00134      12  COMPANY-ID              PIC X(3).                        EL125
00135      12  RECORD-TYPE             PIC X.                           EL125
00136      12  ACCESS-CD-GENL          PIC X(4).                        EL125
00137      12  SEQUENCE-NO             PIC 9(4)   COMP.                 EL125
00138                                                                   EL125
00139  01  COMP-LENGTHS.                                                EL125
00140      12  HOLD-LENGTH             PIC S9(4)  COMP.                 EL125
00141      EJECT                                                        EL125
00142                                  COPY ELCDATE.                       CL*24
00143      EJECT                                                        EL125
00144                                  COPY ELCATTR.                       CL*24
00145      EJECT                                                        EL125
00146                                  COPY ELCLOGOF.                      CL*24
00147      EJECT                                                        EL125
00148                                  COPY ELCAID.                        CL*24
00149  01  FILLER REDEFINES DFHAID.                                     EL125
00150      12  FILLER                  PIC X(8).                        EL125
00151      12  AID-KEYS OCCURS 24 TIMES.                                EL125
00152          16  FILLER              PIC X.                           EL125
00153      EJECT                                                        EL125
00154                                  COPY EL125S.                        CL*24
00155      EJECT                                                        EL125
00156                                  COPY ELCINTF.                       CL*24
00157      EJECT                                                        EL125
00158                                  COPY ELCEMIB.                       CL*24
00159      EJECT                                                        EL125
00160  LINKAGE SECTION.                                                 EL125
00161  01  DFHCOMMAREA                 PIC X(1024).                     EL125
00162                                                                   EL125
00163 *01 PARM-LIST .                                                      CL*32
00164 *    12  FILLER                  PIC S9(8)  COMP.                    CL*32
00165 *    12  CTRL-PNT                PIC S9(8)  COMP.                    CL*32
00166                                                                   EL125
00167                                  COPY ELCCNTL.                       CL*24
00168      EJECT                                                        EL125
00169  PROCEDURE DIVISION.                                              EL125
00170      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL125
00171      MOVE '5'                    TO DC-OPTION-CODE.               EL125
00172                                                                   EL125
00173      EXEC CICS LINK                                               EL125
00174          PROGRAM  ('ELDATCV')                                     EL125
00175          COMMAREA (DATE-CONVERSION-DATA)                          EL125
00176          LENGTH   (DC-COMM-LENGTH)                                EL125
00177      END-EXEC.                                                    EL125
00178                                                                   EL125
00179      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DATE.              EL125
00180                                                                   EL125
00181      IF EIBCALEN = ZERO                                           EL125
00182          MOVE LOW-VALUES         TO EL125AO                       EL125
00183          MOVE -1                 TO COMPIDL                       EL125
00184          GO TO 8100-SEND-MAP.                                     EL125
00185                                                                   EL125
00186      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL125
00187      MOVE SPACES                 TO ERROR-SWITCHES MSG1O MSG2O.   EL125
00188      MOVE 2                      TO EMI-NUMBER-OF-LINES.          EL125
00189                                                                   EL125
00190      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL125
00191          MOVE LOW-VALUES         TO EL125AO                       EL125
00192          MOVE ER-7008            TO EMI-ERROR                     EL125
00193          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL125
00194          MOVE -1                 TO COMPIDL                       EL125
00195          GO TO 8110-SEND-DATA.                                    EL125
00196                                                                   EL125
00197      IF EIBAID = DFHCLEAR                                         EL125
00198          GO TO 8200-NO-UPDATES.                                   EL125
00199                                                                   EL125
00200      EXEC CICS HANDLE CONDITION                                   EL125
00201          PGMIDERR    (8240-PGMIDERR)                                 CL*19
00202          ERROR       (9990-ABEND)                                 EL125
00203      END-EXEC.                                                    EL125
00204                                                                   EL125
00205      EJECT                                                        EL125
00206 ******************************************************               CL**8
00207 *  CHECK TO SEE IF NEW SYSTEM WAS SELECTED THRU NEW  *               CL**8
00208 *  SYSTEM OPTION AVAILABLE IN ALL SYSTEMS MAIN MENU. *               CL**8
00209 ******************************************************               CL**8
00210 *  ENTRY-SW TELLS PGM (SEND MAP) NOT (SEND DATAONLY) *               CL**8
00211 ******************************************************               CL**8
00212                                                                   EL125
00213      IF EIBTRNID = LIT-CREDIT-MASTER-TRNID  OR                    EL125
00214                    LIT-CLAIM-MASTER-TRNID   OR                       CL**8
00215                    LIT-MORTGAGE-MASTER-TRNID                         CL**8
00216          MOVE LOW-VALUES            TO EL125AO                    EL125
00217          MOVE PI-COMPANY-ID         TO COMPIDI                    EL125
00218          MOVE PI-COMPANY-PASSWORD   TO COMPPWDI                   EL125
00219          MOVE PI-PROCESSOR-ID       TO USERIDI                    EL125
00220          MOVE PI-PROCESSOR-PASSWORD TO USERPWDI                   EL125
00221          MOVE PI-NEW-SYSTEM         TO SYSIDI                     EL125
00222          MOVE +1                    TO COMPIDL  COMPPWDL          EL125
00223                                        USERIDL  USERPWDL          EL125
00224                                        SYSIDL                     EL125
00225          MOVE THIS-PGM              TO PI-CALLING-PROGRAM         EL125
00226          MOVE 'Y'                   TO ENTRY-SW                   EL125
00227          GO TO 0100-SKIP-RECEIVE.                                 EL125
00228                                                                   EL125
00229  0100-RECEIVE.                                                    EL125
00230                                                                      CL**8
00231      EXEC CICS RECEIVE                                            EL125
00232          MAP      ('EL125A')                                      EL125
00233          MAPSET   ('EL125S')                                      EL125
00234      END-EXEC.                                                    EL125
00235                                                                   EL125
00236      IF EIBAID NOT = DFHENTER                                     EL125
00237          MOVE ER-0029              TO EMI-ERROR                   EL125
00238          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL125
00239          MOVE EMI-MESSAGE-AREA (1) TO MSG1O                       EL125
00240          MOVE -1                   TO COMPIDL                     EL125
00241          GO TO 8110-SEND-DATA.                                    EL125
00242                                                                   EL125
00243  0100-SKIP-RECEIVE.                                               EL125
00244                                                                      CL**8
00245      MOVE AL-UANON               TO COMPIDA USERIDA.              EL125
00246      MOVE AL-UADON               TO COMPPWDA USERPWDA.            EL125
00247      PERFORM 1000-EDIT-SCREEN THRU 1020-EXIT.                     EL125
00248                                                                   EL125
00249      IF USERIDI IS EQUAL TO LGXX-USER                                CL*25
00250          IF (COMPIDI IS EQUAL TO 'AIG' OR 'AUK')                     CL*25
00251              IF (SYSIDI IS NOT EQUAL TO 'CL' AND 'CV')               CL*27
00252                  MOVE -1            TO  SYSIDL                       CL*25
00253                  MOVE AL-UABON      TO  SYSIDA                       CL*25
00254                  MOVE 'X'           TO  ERROR-SWITCH                 CL*25
00255                  MOVE ER-7000       TO  EMI-ERROR                    CL*25
00256                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           CL*25
00257                                                                      CL*25
00258      IF LOGON-ERROR                                               EL125
00259          GO TO 8250-LOGON-ERROR.                                     CL*19
00260                                                                   EL125
00261      IF SCREEN-ERROR                                              EL125
00262          GO TO 8110-SEND-DATA.                                    EL125
00263                                                                   EL125
00264      GO TO 8210-XCTL-MAIN-MENU.                                   EL125
00265                                                                   EL125
00266      EJECT                                                        EL125
00267  1000-EDIT-SCREEN.                                                EL125
00268                                                                      CL**8
00269      IF SYSIDL GREATER ZERO                                       EL125
00270          NEXT SENTENCE                                            EL125
00271      ELSE                                                         EL125
00272          IF USERIDI = LGXX-USER                                   EL125
00273              IF (COMPIDI = 'AIG' OR 'AUK')                           CL*25
00274                  MOVE 'CL'       TO  SYSIDI                          CL*25
00275              ELSE                                                    CL*25
00276                  MOVE 'CR'       TO  SYSIDI.                         CL*25
00277                                                                      CL**8
00278      IF SYSIDI = ('CR' OR 'CL' OR 'CV')                              CL*27
00279              NEXT SENTENCE                                           CL*27
00280      ELSE                                                            CL*27
00281          MOVE -1                 TO SYSIDL                           CL*27
00282          MOVE AL-UABON           TO SYSIDA                           CL*27
00283          MOVE 'X'                TO ERROR-SWITCH                     CL*27
00284          MOVE ER-7000            TO EMI-ERROR                        CL*27
00285          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*27
00286                                                                      CL**8
00287 ****************************************************                 CL*27
00288 *  READ COMPANY RECORD (TYPE 1) FROM CONTROL FILE  *                 CL*27
00289 ****************************************************                 CL*27
00290                                                                      CL*27
00291      EXEC CICS HANDLE CONDITION                                      CL*27
00292          NOTOPEN (8230-NOT-OPEN)                                     CL*27
00293          NOTFND  (1010-COMP-ID-ERR)                                  CL*27
00294      END-EXEC.                                                       CL*27
00295                                                                      CL*27
00296      MOVE COMPIDI                TO COMPANY-ID.                      CL*27
00297      MOVE 'N'                    TO RECORD-TYPE.                     CL*27
00298      MOVE SPACES                 TO ACCESS-CD-GENL.                  CL*27
00299      MOVE ZEROS                  TO SEQUENCE-NO.                     CL*27
00300                                                                      CL*27
00301      PERFORM 2000-READ-FILE THRU 2010-EXIT.                          CL*27
00302      MOVE CF-MORTG-ACCESS-CONTROL TO WS-MORTG-ACCESS-CONTROL.        CL*27
00303      MOVE CF-MORTG-LABEL-CONTROL  TO WS-MORTG-LABEL-CONTROL.         CL*27
00304      MOVE CF-MORTG-BILL-GROUPING-CODE                                CL*27
00305                                  TO WS-MORTG-BILL-GRP-CODE.          CL*27
00306      MOVE CF-RATE-DEV-AUTHORIZATION                                  CL*27
00307                                  TO WS-RATE-DEV-AUTHORIZATION.       CL*27
00308      MOVE CF-MP-POLICY-LINKAGE-IND                                   CL*31
00309                                  TO W-POLICY-LINKAGE-IND.            CL*31
00310                                                                      CL*27
00311      MOVE COMPIDI                TO COMPANY-ID.                      CL*27
00312      MOVE '1'                    TO RECORD-TYPE.                     CL*27
00313      MOVE SPACES                 TO ACCESS-CD-GENL.                  CL*27
00314      MOVE ZEROS                  TO SEQUENCE-NO.                     CL*27
00315                                                                      CL*27
00316      PERFORM 2000-READ-FILE THRU 2010-EXIT.                          CL*27
00317                                                                      CL*27
00318      IF USERIDI NOT =  LGXX-USER                                     CL*27
00319          IF ALL-BUT-TERM                                             CL*27
00320              NEXT SENTENCE                                           CL*27
00321          ELSE                                                        CL*27
00322             MOVE CONTROL-FILE    TO WS-CONTROL-FILE-SAVE             CL*27
00323             PERFORM 1500-CHECK-TERM THRU 1530-EXIT                   CL*27
00324             MOVE WS-CONTROL-FILE-SAVE                                CL*27
00325                                  TO CONTROL-FILE                     CL*27
00326      ELSE                                                            CL*27
00327          PERFORM 1700-LGXX-SEC THRU 1720-EXIT                        CL*27
00328          GO TO 1020-EXIT.                                            CL*27
00329                                                                      CL*27
00330      IF SCREEN-ERROR                                                 CL*27
00331          GO TO 1020-EXIT.                                            CL*27
00332                                                                      CL*27
00333 ***********************************************                      CL*27
00334 *  CF-SECURITY-OPTION = (1) ALL-SECURITY      *                      CL*27
00335 *                       (2) COMPANY-VERIFY    *                      CL*27
00336 *                       (3) PROCESSOR-VERIFY  *                      CL*27
00337 *                       (4) NO-SECURITY       *                      CL*27
00338 *                       (5) ALL-BUT-TERM      *                      CL*27
00339 ***********************************************                      CL*27
00340                                                                      CL*27
00341      IF ALL-SECURITY                                                 CL*27
00342          PERFORM 1100-TOTAL-SEC THRU 1120-EXIT                       CL*27
00343      ELSE                                                            CL*27
00344          IF ALL-BUT-TERM                                             CL*27
00345              PERFORM 1100-TOTAL-SEC THRU 1120-EXIT                   CL*27
00346          ELSE                                                        CL*27
00347              IF COMPANY-VERIFY                                       CL*27
00348                  PERFORM 1200-COMP-SEC THRU 1220-EXIT                CL*27
00349              ELSE                                                    CL*27
00350                  IF PROCESSOR-VERIFY                                 CL*27
00351                      PERFORM 1300-PROC-SEC THRU 1320-EXIT            CL*27
00352                  ELSE                                                CL*27
00353                      PERFORM 1400-NO-SEC THRU 1420-EXIT.             CL*27
00354                                                                      CL*27
00355      IF SCREEN-ERROR                                                 CL*27
00356          GO TO 1020-EXIT.                                            CL*27
00357                                                                      CL*27
00358      PERFORM 1640-PROC-TERM-CHK THRU 1660-EXIT.                      CL*27
00359      GO TO 1020-EXIT.                                                CL*27
00360                                                                      CL*27
00361  1010-COMP-ID-ERR.                                                   CL*27
00362                                                                      CL**8
00363      MOVE -1                     TO COMPIDL.                         CL*27
00364      MOVE AL-UABON               TO COMPIDA.                         CL*27
00365      MOVE 'X'                    TO ERROR-SWITCH.                    CL*27
00366      MOVE ER-0016                TO EMI-ERROR.                       CL*27
00367      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*27
00368                                                                      CL*27
00369  1020-EXIT.                                                          CL*27
00370      EXIT.                                                           CL*27
00371                                                                      CL**8
00372      EJECT                                                           CL*27
00373 ***********************************************                      CL*27
00374 *  CF-SECURITY-OPTION = (1) ALL-SECURITY      *                      CL*27
00375 ***********************************************                      CL*27
00376 *  CF-SECURITY-OPTION = (5) ALL-BUT-TERM      *                      CL*27
00377 ***********************************************                      CL*27
00378                                                                   EL125
00379  1100-TOTAL-SEC.                                                     CL*27
00380                                                                      CL*27
00381      PERFORM 1600-COMP-PSWD-CHK THRU 1610-EXIT.                      CL*27
00382      PERFORM 2100-MOVE-COMP     THRU 2110-EXIT.                      CL*27
00383                                                                      CL*27
00384      EXEC CICS HANDLE CONDITION                                      CL*27
00385          NOTFND (1110-PROC-ID-ERR)                                   CL*27
00386      END-EXEC.                                                       CL*27
00387                                                                      CL*27
00388      MOVE '2'                    TO RECORD-TYPE.                     CL*27
00389      MOVE USERIDI                TO ACCESS-CD-GENL.                  CL*27
00390                                                                      CL*27
00391      PERFORM 2000-READ-FILE     THRU 2010-EXIT.                      CL*27
00392      PERFORM 1620-PROC-PSWD-CHK THRU 1630-EXIT.                      CL*27
00393      PERFORM 2200-MOVE-PROC     THRU 2290-EXIT.                      CL*27
00394                                                                      CL*27
00395      IF SYSIDI = ('CR' OR 'CL' OR 'CV')                              CL*27
00396          IF (COMPIDI IS EQUAL TO 'AIG' OR 'AUK')                     CL*27
00397              IF (SYSIDI IS EQUAL TO 'CL' OR 'CV')                    CL*27
00398                  NEXT SENTENCE                                       CL*27
00399              ELSE                                                    CL*27
00400                  MOVE -1         TO SYSIDL                           CL*27
00401                  MOVE AL-UABON   TO SYSIDA                           CL*27
00402                  MOVE 'X'        TO ERROR-SWITCH                     CL*27
00403                  MOVE ER-7000    TO EMI-ERROR                        CL*27
00404                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*27
00405          ELSE                                                        CL*27
00406              NEXT SENTENCE                                           CL*27
00407      ELSE                                                            CL*27
00408          MOVE -1                 TO SYSIDL                           CL*27
00409          MOVE AL-UABON           TO SYSIDA                           CL*27
00410          MOVE 'X'                TO ERROR-SWITCH                     CL*27
00411          MOVE ER-7000            TO EMI-ERROR                        CL*27
00412          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*27
00413                                                                   EL125
00414      GO TO 1120-EXIT.                                                CL*27
00415                                                                      CL*27
00416  1110-PROC-ID-ERR.                                                   CL*27
00417      MOVE -1                     TO USERIDL.                         CL*27
00418      MOVE AL-UABON               TO USERIDA.                         CL*27
00419      MOVE 'X'                    TO ERROR-SWITCH.                    CL*27
00420      MOVE ER-0019                TO EMI-ERROR.                       CL*27
00421      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*27
00422                                                                      CL*27
00423  1120-EXIT.                                                          CL*27
00424      EXIT.                                                           CL*27
00425                                                                   EL125
00426      EJECT                                                           CL*27
00427 ***********************************************                      CL*27
00428 *  CF-SECURITY-OPTION = (2) COMPANY-VERIFY    *                      CL*27
00429 ***********************************************                      CL*27
00430                                                                      CL*27
00431  1200-COMP-SEC.                                                      CL*27
00432      PERFORM 1600-COMP-PSWD-CHK THRU 1610-EXIT.                      CL*27
00433      PERFORM 2100-MOVE-COMP     THRU 2110-EXIT.                      CL*27
00434                                                                      CL*27
00435      EXEC CICS HANDLE CONDITION                                      CL*27
00436          NOTFND (1210-PROC-ID-ERR)                                   CL*27
00437      END-EXEC.                                                       CL*27
00438                                                                      CL*27
00439      MOVE '2'                    TO RECORD-TYPE.                     CL*27
00440      MOVE USERIDI                TO ACCESS-CD-GENL.                  CL*27
00441                                                                      CL*27
00442      PERFORM 2000-READ-FILE THRU 2010-EXIT.                          CL*27
00443      PERFORM 2200-MOVE-PROC THRU 2290-EXIT.                          CL*27
00444      GO TO 1220-EXIT.                                                CL*27
00445                                                                      CL*27
00446  1210-PROC-ID-ERR.                                                   CL*27
00447      MOVE -1                     TO USERIDL.                         CL*27
00448      MOVE AL-UABON               TO USERIDA.                         CL*27
00449      MOVE 'X'                    TO ERROR-SWITCH.                    CL*27
00450      MOVE ER-0019                TO EMI-ERROR.                       CL*27
00451      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*27
00452                                                                      CL*27
00453  1220-EXIT.                                                          CL*27
00454      EXIT.                                                           CL*27
00455                                                                      CL*27
00456      EJECT                                                           CL*27
00457 ***********************************************                      CL*27
00458 *  CF-SECURITY-OPTION = (3) PROCESSOR-VERIFY  *                      CL*27
00459 ***********************************************                      CL*27
00460                                                                      CL*27
00461  1300-PROC-SEC.                                                      CL*27
00462      PERFORM 2100-MOVE-COMP THRU 2110-EXIT.                          CL*27
00463                                                                      CL*27
00464      EXEC CICS HANDLE CONDITION                                      CL*27
00465          NOTFND (1310-PROC-ID-ERR)                                   CL*27
00466      END-EXEC.                                                       CL*27
00467                                                                   EL125
00468      MOVE '2'                    TO RECORD-TYPE.                     CL*27
00469      MOVE USERIDI                TO ACCESS-CD-GENL.                  CL*27
00470                                                                      CL*27
00471      PERFORM 2000-READ-FILE     THRU 2010-EXIT.                      CL*27
00472      PERFORM 1620-PROC-PSWD-CHK THRU 1630-EXIT.                      CL*27
00473      PERFORM 2200-MOVE-PROC     THRU 2290-EXIT.                      CL*27
00474                                                                   EL125
00475      GO TO 1320-EXIT.                                                CL*27
00476                                                                      CL*27
00477  1310-PROC-ID-ERR.                                                   CL*27
00478      MOVE -1                     TO USERIDL.                         CL*27
00479      MOVE AL-UABON               TO USERIDA.                         CL*27
00480      MOVE 'X'                    TO ERROR-SWITCH.                    CL*27
00481      MOVE ER-0019                TO EMI-ERROR.                       CL*27
00482      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*27
00483                                                                      CL*27
00484  1320-EXIT.                                                          CL*27
00485      EXIT.                                                           CL*27
00486                                                                   EL125
00487      EJECT                                                           CL*27
00488 ***********************************************                      CL*27
00489 *  CF-SECURITY-OPTION = (4) NO-SECURITY       *                      CL*27
00490 ***********************************************                      CL*27
00491                                                                      CL*27
00492  1400-NO-SEC.                                                        CL*27
00493                                                                   EL125
00494      PERFORM 2100-MOVE-COMP THRU 2110-EXIT.                          CL*27
00495                                                                      CL*27
00496      EXEC CICS HANDLE CONDITION                                      CL*27
00497          NOTFND (1410-PROC-ID-ERR)                                   CL*27
00498      END-EXEC.                                                       CL*27
00499                                                                      CL*27
00500      MOVE '2'                    TO RECORD-TYPE.                     CL*27
00501      MOVE USERIDI                TO ACCESS-CD-GENL.                  CL*27
00502                                                                      CL*27
00503      PERFORM 2000-READ-FILE THRU 2010-EXIT.                          CL*27
00504      PERFORM 2200-MOVE-PROC THRU 2290-EXIT.                          CL*27
00505                                                                   EL125
00506      GO TO 1420-EXIT.                                                CL*27
00507                                                                      CL*27
00508  1410-PROC-ID-ERR.                                                   CL*27
00509      MOVE -1                     TO USERIDL.                         CL*27
00510      MOVE AL-UABON               TO USERIDA.                         CL*27
00511      MOVE 'X'                    TO ERROR-SWITCH.                    CL*27
00512      MOVE ER-0019                TO EMI-ERROR.                       CL*27
00513      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*27
00514                                                                      CL*27
00515  1420-EXIT.                                                          CL*27
00516      EXIT.                                                           CL*27
00517                                                                   EL125
00518      EJECT                                                           CL*27
00519 *****************************************************                CL*27
00520 *  READ TERMINAL RECORD (TYPE 9) FROM CONTROL FILE  *                CL*27
00521 *****************************************************                CL*27
00522                                                                      CL*27
00523  1500-CHECK-TERM.                                                    CL*27
00524                                                                      CL*27
00525      MOVE EIBTRMID               TO  WS-TERMINAL-PREFIX.             CL*27
00526                                                                      CL*27
00527      IF WS-TERMINAL-PREFIX = 'DU' OR 'WL' OR 'CD'                    CL*27
00528          GO TO 1530-EXIT.                                            CL*27
00529                                                                      CL*27
00530      EXEC CICS HANDLE CONDITION                                      CL*27
00531          NOTOPEN (8230-NOT-OPEN)                                     CL*27
00532          NOTFND  (1520-TERM-INVALID)                                 CL*27
00533      END-EXEC.                                                       CL*27
00534                                                                      CL*27
00535      MOVE COMPIDI                TO COMPANY-ID.                      CL*27
00536      MOVE '9'                    TO RECORD-TYPE.                     CL*27
00537      MOVE SPACES                 TO ACCESS-CD-GENL.                  CL*27
00538      MOVE ZEROS                  TO SEQUENCE-NO.                     CL*27
00539      PERFORM 2000-READ-FILE THRU 2010-EXIT.                          CL*27
00540      MOVE ZEROS                  TO COUNT-1.                         CL*27
00541      MOVE EIBTRMID               TO HOLD-TERM.                       CL*27
00542                                                                      CL*27
00543  1510-TERM-LOOP.                                                     CL*27
00544      ADD 1                       TO COUNT-1.                         CL*27
00545                                                                   EL125
00546      IF COUNT-1 GREATER 120 OR                                       CL*27
00547         CF-TERMINAL-ID (COUNT-1) = SPACES                            CL*27
00548          GO TO 1520-TERM-INVALID.                                    CL*27
00549                                                                      CL*27
00550      IF CF-TERMINAL-ID (COUNT-1) = HOLD-TERM                         CL*27
00551          GO TO 1530-EXIT.                                            CL*27
00552                                                                   EL125
00553      GO TO 1510-TERM-LOOP.                                           CL*27
00554                                                                      CL*27
00555  1520-TERM-INVALID.                                                  CL*27
00556      MOVE -1                     TO COMPIDL.                         CL*27
00557      MOVE AL-UABON               TO COMPIDA.                         CL*27
00558      MOVE 'X'                    TO ERROR-SWITCH.                    CL*27
00559      MOVE ER-0017                TO EMI-ERROR.                       CL*27
00560      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*27
00561                                                                      CL*27
00562  1530-EXIT.                                                          CL*27
00563      EXIT.                                                           CL*27
00564                                                                      CL*27
00565      EJECT                                                           CL*27
00566 ***************************************                              CL*27
00567 *  COMPANY PASSWORD CHECKING ROUTINE  *                              CL*27
00568 ***************************************                              CL*27
00569                                                                      CL*27
00570  1600-COMP-PSWD-CHK.                                                 CL*27
00571                                                                      CL*27
00572      IF COMPPWDI NOT = CF-COMPANY-PASSWORD                           CL*27
00573          MOVE -1                 TO COMPPWDL                         CL*27
00574          MOVE 'X'                TO ERROR-SWITCH                     CL*27
00575          MOVE ER-0018            TO EMI-ERROR                        CL*27
00576          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*27
00577                                                                      CL*27
00578      IF SYSIDI = 'CR'                                                CL*27
00579          IF CO-IS-NOT-USER                                           CL*27
00580              MOVE -1             TO USERIDL                          CL*27
00581              MOVE 'X'            TO ERROR-SWITCH                     CL*27
00582              MOVE ER-2569     TO EMI-ERROR                           CL*27
00583              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*27
00584                                                                      CL*27
00585      IF SYSIDI = 'CL'                                                CL*27
00586          IF CO-IS-NOT-CLAIM-USER                                     CL*27
00587              MOVE -1             TO USERIDL                          CL*27
00588              MOVE 'X'            TO ERROR-SWITCH                     CL*27
00589              MOVE ER-2568     TO EMI-ERROR                           CL*27
00590              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*27
00591                                                                      CL*27
00592      IF SYSIDI = 'CV'                                                CL*27
00593          IF CO-IS-NOT-LIFE-USER                                      CL*27
00594              MOVE -1             TO USERIDL                          CL*27
00595              MOVE 'X'            TO ERROR-SWITCH                     CL*27
00596              MOVE ER-9000     TO EMI-ERROR                           CL*27
00597              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*27
00598                                                                      CL*27
00599  1610-EXIT.                                                          CL*27
00600      EXIT.                                                           CL*27
00601                                                                      CL*27
00602      EJECT                                                           CL*27
00603 *****************************************                            CL*27
00604 *  PROCESSOR PASSWORD CHECKING ROUTINE  *                            CL*27
00605 *****************************************                            CL*27
00606                                                                      CL*27
00607  1620-PROC-PSWD-CHK.                                                 CL*27
00608                                                                      CL*27
00609      IF USERPWDI = CF-PROCESSOR-PASSWORD                             CL*27
00610          NEXT SENTENCE                                               CL*27
00611      ELSE                                                            CL*27
00612          MOVE -1                 TO USERPWDL                         CL*27
00613          MOVE 'X'                TO ERROR-SWITCH                     CL*27
00614          MOVE ER-0020            TO EMI-ERROR                        CL*27
00615          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*27
00616                                                                      CL*27
00617      IF ACCESS-TO-ALL-SYSTEMS                                        CL*27
00618          GO TO 1630-EXIT.                                            CL*27
00619                                                                      CL*27
00620      IF SYSIDI = 'CR'                                                CL*27
00621          IF ACCESS-TO-CREDIT                                         CL*27
00622              GO TO 1630-EXIT                                         CL*27
00623          ELSE                                                        CL*27
00624              MOVE -1             TO USERIDL                          CL*27
00625              MOVE 'X'            TO ERROR-SWITCH                     CL*27
00626              MOVE AL-UABON       TO USERIDA                          CL*27
00627              MOVE ER-2566        TO EMI-ERROR                        CL*27
00628              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*27
00629                                                                      CL*27
00630      IF SYSIDI = 'CL'                                                CL*27
00631          IF ACCESS-TO-CLAIMS                                         CL*27
00632              GO TO 1630-EXIT                                         CL*27
00633          ELSE                                                        CL*27
00634              MOVE -1             TO USERIDL                          CL*27
00635              MOVE 'X'            TO ERROR-SWITCH                     CL*27
00636              MOVE AL-UABON       TO USERIDA                          CL*27
00637              MOVE ER-2567        TO EMI-ERROR                        CL*27
00638              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*27
00639                                                                      CL*27
00640      IF SYSIDI = 'CV'                                                CL*27
00641          IF ACCESS-TO-LIFE                                           CL*27
00642              GO TO 1630-EXIT                                         CL*27
00643          ELSE                                                        CL*27
00644              MOVE -1             TO USERIDL                          CL*27
00645              MOVE 'X'            TO ERROR-SWITCH                     CL*27
00646              MOVE AL-UABON       TO USERIDA                          CL*27
00647              MOVE ER-9002        TO EMI-ERROR                        CL*27
00648              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*27
00649                                                                      CL*27
00650  1630-EXIT.                                                          CL*27
00651      EXIT.                                                           CL*27
00652                                                                   EL125
00653      EJECT                                                           CL*27
00654 ******************************************************               CL*27
00655 *  READ PROCESSOR RECORD (TYPE 2) FROM CONTROL FILE  *               CL*27
00656 ******************************************************               CL*27
00657                                                                      CL*27
00658  1640-PROC-TERM-CHK.                                                 CL*27
00659                                                                      CL*27
00660      PERFORM 2020-READ-FOR-UPDATE THRU 2030-EXIT.                    CL*27
00661                                                                      CL*21
00662      IF (PI-COMPANY-ID = 'MIL' OR 'PLC' OR 'BLC' OR 'NBC' OR         CL*27
020816                         'AIG' OR 'AUK' OR 'CID' OR 'DCC' OR 'VPP'
062121                      or 'AHL' or 'FNL')         
00664         GO TO 1650-UPDATE-PROC-RECORD.                               CL*27
00665                                                                      CL*27
00666 *                                                                    CL*27
00667 *                                                                    CL*27
00668 *  LEAVE THE ABOVE 2 LINES IN TO PREVENT LOSING THE LINES ABOVE      CL*27
00686                                                                      CL*27
00687      IF CF-CURRENT-TERM-ON = SPACES OR EIBTRMID                      CL*27
00688          GO TO 1650-UPDATE-PROC-RECORD.                              CL*27
00689                                                                   EL125
00690      MOVE CF-CURRENT-TERM-ON     TO WS-CURRENT-TERM-ON.              CL*27
00691                                                                      CL**8
00692  1645-ERROR.                                                         CL*27
00693                                                                      CL*27
00694      MOVE 'Y'                    TO ERROR-SWITCH.                    CL*27
00695      MOVE '?'                    TO PI-ENTRY-CD-1.                   CL*27
00696      PERFORM 2060-UNLOCK THRU 2070-EXIT.                             CL*27
00697                                                                      CL**8
00698      GO TO 1660-EXIT.                                                CL*27
00699                                                                      CL*27
00700  1650-UPDATE-PROC-RECORD.                                            CL*27
00701                                                                      CL*27
00702      MOVE EIBTRMID               TO CF-CURRENT-TERM-ON.              CL*27
00703      PERFORM 2040-REWRITE THRU 2050-EXIT.                            CL*27
00704                                                                      CL*27
00705  1660-EXIT.                                                          CL*27
00706      EXIT.                                                           CL*27
00707                                                                      CL*17
00708      EJECT                                                           CL*27
00709 *                                                                    CL*27
00710 *                                                                    CL*27
00711 *  LEAVE THE ABOVE 2 LINE IN TO PREVENT LOSING THE LINES ABOVE       CL*27
00762                                                                      CL**8
00763                                                                      CL*27
00764 ********************************************************             CL*27
00765 *  SECURITY CHECK FOR USER=(LGX) PI-USER-ALMIGHTY-YES  *             CL*27
00766 ********************************************************             CL*27
00767                                                                      CL*10
00768  1700-LGXX-SEC.                                                      CL*27
00769                                                                      CL*35
00770      IF COMPIDI IS EQUAL TO 'SLI'                                    CL*35
00771          MOVE -1                 TO USERIDL                          CL*35
00772          MOVE AL-UABON           TO USERIDA                          CL*35
00773          MOVE 'X'                TO ERROR-SWITCH                     CL*35
00774          MOVE ER-0019            TO EMI-ERROR                        CL*35
00775          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*35
00776          GO TO 1720-EXIT.                                            CL*35
00777                                                                      CL*35
00778      EXEC CICS HANDLE CONDITION                                      CL*27
00779           INVREQ   (1701-CHECK-PASSWORD)                             CL*27
00780           END-EXEC.                                                  CL*27
00781                                                                      CL*27
00782 *                                                                    CL*28
00783 *                                                                    CL*28
00784 *  LEAVE THE ABOVE 2 LINES IN TO PREVENT LOSING THE LINES ABOVE      CL*28
00816                                                                      CL*27
00817  1701-CHECK-PASSWORD.                                                CL*27
00818 *    IF COMPPWDI NOT = LIT-COMP-PASSWORD                             CL*38
00819 *        MOVE -1                 TO COMPPWDL                         CL*38
00820 *        MOVE 'X'                TO ERROR-SWITCH                     CL*38
00821 *        MOVE ER-0018            TO EMI-ERROR                        CL*38
00822 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*38
00823 *    ELSE                                                            CL*38
00824 *        PERFORM 2100-MOVE-COMP THRU 2110-EXIT.                      CL*38
00825 *                                                                    CL*38
00826 *    IF USERPWDI NOT = LIT-USER-PASSWORD                             CL*38
00827 *        MOVE -1                 TO USERPWDL                         CL*38
00828 *        MOVE 'X'                TO ERROR-SWITCH                     CL*38
00829 *        MOVE ER-0020            TO EMI-ERROR                        CL*38
00830 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*38
00831 *    ELSE                                                            CL*38
00832 *        MOVE ALL 'Y'            TO PI-PROCESSOR-CAP-LIST            CL*38
00833 *        MOVE 'Y'                TO PI-PROCESSOR-USER-ALMIGHTY       CL*38
00834 *        MOVE SPACE              TO PI-PROCESSOR-SYS-ACCESS          CL*38
00835 *        MOVE USERPWDI           TO PI-PROCESSOR-PASSWORD            CL*38
00836 *        MOVE USERIDI            TO PI-PROCESSOR-ID                  CL*38
00837 *        MOVE SPACES             TO PI-ACCOUNT-SECURITY              CL*38
00838 *                                   PI-CARRIER-SECURITY.             CL*38
00839                                                                      CL*27
00840  1720-EXIT.                                                          CL*27
00841      EXIT.                                                           CL*27
00842                                                                      CL*27
00843      EJECT                                                           CL*27
00844  2000-READ-FILE.                                                     CL*27
00845                                                                      CL*27
00846      EXEC CICS READ                                                  CL*27
00847          DATASET ('ELCNTL')                                          CL*27
00848          RIDFLD  (FILE-READ-KEY)                                     CL*27
00849          SET     (ADDRESS OF CONTROL-FILE)                           CL*32
00850      END-EXEC.                                                       CL*27
00851                                                                      CL*27
00852      CONTINUE.                                                       CL*32
00853                                                                      CL*27
00854  2010-EXIT.                                                          CL*27
00855      EXIT.                                                           CL*27
00856                                                                      CL*27
00857  2020-READ-FOR-UPDATE.                                               CL*27
00858                                                                      CL*27
00859      EXEC CICS READ                                                  CL*27
00860          DATASET ('ELCNTL')                                          CL*27
00861          RIDFLD  (FILE-READ-KEY)                                     CL*27
00862          SET     (ADDRESS OF CONTROL-FILE)                           CL*32
00863          UPDATE                                                      CL*27
00864      END-EXEC.                                                       CL*27
00865                                                                      CL*27
00866      CONTINUE.                                                       CL*32
00867                                                                      CL*27
00868  2030-EXIT.                                                          CL*27
00869      EXIT.                                                           CL*27
00870                                                                      CL*27
00871  2040-REWRITE.                                                       CL*27
00872      EXEC CICS REWRITE                                               CL*27
00873          FROM    (CONTROL-FILE)                                      CL*27
00874          DATASET ('ELCNTL')                                          CL*27
00875      END-EXEC.                                                       CL*27
00876                                                                      CL*27
00877  2050-EXIT.                                                          CL*27
00878      EXIT.                                                           CL*27
00879                                                                      CL*27
00880      EJECT                                                           CL*27
00881  2060-UNLOCK.                                                        CL*27
00882      EXEC CICS UNLOCK                                                CL*27
00883          DATASET ('ELCNTL')                                          CL*27
00884      END-EXEC.                                                       CL*27
00885                                                                      CL**8
00886  2070-EXIT.                                                          CL*27
00887      EXIT.                                                           CL*27
00888                                                                      CL*27
00889      EJECT                                                           CL*27
00890 ******************************************************************   CL*27
00891 *         INITIALIZE COMPANY CONTROLS PROGRAM INTERFACE              CL*27
00892 ******************************************************************   CL*27
00893                                                                      CL*27
00894  2100-MOVE-COMP.                                                     CL*27
00895                                                                   EL125
00896      MOVE PI-COMM-LENGTH         TO HOLD-LENGTH.                     CL*27
00897      MOVE SPACES                 TO PROGRAM-INTERFACE-BLOCK.         CL*27
00898      MOVE HOLD-LENGTH            TO PI-COMM-LENGTH.                  CL*27
00899      MOVE ZEROES                 TO PI-UPDATE-HHMMSS.                CL*27
00900      MOVE THIS-PGM               TO PI-CALLING-PROGRAM.              CL*27
00901      MOVE CF-COMPANY-ID          TO PI-COMPANY-ID.                   CL*27
00902      MOVE CF-COMPANY-CD          TO PI-COMPANY-CD.                   CL*27
00903      MOVE COMPPWDI               TO PI-COMPANY-PASSWORD.             CL*27
00904      MOVE CF-LOWER-CASE-LETTERS  TO PI-LOWER-CASE-LETTERS.           CL*27
00905                                                                      CL*27
00906      IF CF-NEXT-COMPANY-ID NOT = SPACES AND LOW-VALUES AND ZEROS     CL*27
00907          MOVE CF-COMPANY-ID      TO PI-ORIGINAL-COMPANY-ID           CL*27
00908          MOVE CF-COMPANY-CD      TO PI-ORIGINAL-COMPANY-CD           CL*27
00909      ELSE                                                            CL*27
00910          MOVE SPACES             TO PI-ORIGINAL-COMPANY-ID           CL*27
00911                                     PI-ORIGINAL-COMPANY-CD.          CL*27
00912                                                                      CL*27
00913      MOVE CF-JOURNAL-FILE-ID     TO PI-JOURNAL-FILE-ID.              CL*27
00914                                                                      CL*27
00915 ******************************************************************   CL*27
00916 *           INITIALIZE CREDIT SYSTEM PROGRAM INTERFACE           *   CL*27
00917 ******************************************************************   CL*27
00918                                                                      CL*27
00919      MOVE CF-LGX-CREDIT-USER        TO PI-CREDIT-USER.               CL*27
00920      MOVE CF-SYSTEM-D               TO PI-GA-BILLING-CONTROL.        CL*27
00921      MOVE CF-SYSTEM-E               TO PI-AR-PROCESSING-CNTL.        CL*27
00922      MOVE CF-MAIL-PROCESSING        TO PI-MAIL-PROCESSING.           CL*27
00923      MOVE CF-CERT-ACCESS-CONTROL    TO PI-CERT-ACCESS-CONTROL.       CL*27
00924      MOVE CF-CAR-GROUP-ACCESS-CNTL  TO PI-CAR-GROUP-ACCESS-CNTL.     CL*27
00925      MOVE CF-CARRIER-CONTROL-LEVEL  TO PI-CARRIER-CONTROL-LEVEL.     CL*27
00926      MOVE CF-CR-MONTH-END-DT        TO PI-CR-MONTH-END-DT.           CL*27
00927      MOVE CF-AR-MONTH-END-DT        TO PI-AR-MONTH-END-DT.           CL*27
00928      IF CF-VALID-REM-TRM-OPTION                                      CL*27
00929          MOVE CF-REM-TRM-CALC-OPTION TO PI-REM-TRM-CALC-OPTION       CL*27
00930      ELSE                                                            CL*27
00931          MOVE SPACE                  TO PI-REM-TRM-CALC-OPTION.      CL*27
00932                                                                      CL*27
00933 ******************************************************************   CL*27
00934 *           INITIALIZE CLAIMS SYSTEM PROGRAM INTERFACE           *   CL*27
00935 ******************************************************************   CL*27
00936                                                                      CL*27
00937 *    MOVE CF-CLAIM-ACCESS-CONTROL   TO PI-CLAIM-ACCESS-CONTROL.      CL*27
00938      MOVE CF-LGX-CLAIM-USER         TO PI-CLAIM-USER.                CL*27
00939      MOVE CF-CLAIM-PAID-THRU-TO     TO PI-CLAIM-PAID-THRU-TO.        CL*27
00940      IF  SYSIDI = 'CL'                                               CL*27
00941          MOVE CF-PRINT-ADDRESS-LABELS                                CL*27
00942                                     TO PI-LABEL-CONTROL.             CL*27
00943      IF  SYSIDI = 'CR'                                               CL*27
00944          MOVE CF-CR-PRINT-ADDRESS-LABELS                             CL*27
00945                                     TO PI-LABEL-CONTROL.             CL*27
00946                                                                      CL*27
00947 ******************************************************************   CL*27
00948 *       INITIALIZE CREDIT CARD SYSTEM PROGRAM INTERFACE          *   CL*27
00949 ******************************************************************   CL*27
00950                                                                      CL*27
00951      MOVE CF-CRDTCRD-USER        TO PI-CRDTCRD-USER.                 CL*27
00952      MOVE CF-CC-MONTH-END-DT     TO PI-CC-MONTH-END-DT.              CL*27
00953                                                                      CL*27
00954 ******************************************************************   CL*27
00955 *       INITIALIZE MORTGAGE(LIFE) SYSTEM PROGRAM INTERFACE       *   CL*27
00956 ******************************************************************EL125
00957                                                                      CL*27
00958      MOVE CF-LGX-LIFE-USER       TO PI-MORTGAGE-USER.                CL*27
00959      MOVE WS-MORTG-ACCESS-CONTROL TO PI-MORTGAGE-ACCESS-CONTROL.     CL*27
00960      MOVE WS-MORTG-BILL-GRP-CODE TO PI-BILL-GROUPING-CODE.           CL*27
00961      MOVE W-POLICY-LINKAGE-IND   TO PI-POLICY-LINKAGE-IND.           CL*31
00962      MOVE WS-RATE-DEV-AUTHORIZATION                                  CL*27
00963                                  TO PI-RATE-DEV-AUTHORIZATION.       CL*27
00964      MOVE CF-MP-MONTH-END-DT     TO PI-MP-MONTH-END-DT.              CL*27
00965                                                                   EL125
00966      IF  SYSIDI = 'CV'                                               CL*27
00967          MOVE WS-MORTG-LABEL-CONTROL                                 CL*27
00968                                  TO PI-LABEL-CONTROL.                CL*27
00969                                                                      CL*27
00970 ******************************************************************   CL*27
00971 *           INITIALIZE LIFE AND A/H OVERRIDES                    *   CL*27
00972 ******************************************************************   CL*27
00973                                                                      CL*27
00974      IF CF-LIFE-OVERRIDE-L1 = SPACES OR LOW-VALUES                   CL*27
00975         MOVE 'L'                 TO PI-LIFE-OVERRIDE-L1              CL*27
00976      ELSE                                                            CL*27
00977         MOVE CF-LIFE-OVERRIDE-L1 TO PI-LIFE-OVERRIDE-L1.             CL*27
00978                                                                      CL*27
00979      IF CF-LIFE-OVERRIDE-L2 = SPACES OR LOW-VALUES                   CL*27
00980         MOVE 'LF'                 TO PI-LIFE-OVERRIDE-L2             CL*27
00981      ELSE                                                            CL*27
00982         MOVE CF-LIFE-OVERRIDE-L2 TO PI-LIFE-OVERRIDE-L2.             CL*27
00983                                                                      CL*27
00984      IF CF-LIFE-OVERRIDE-L6 = SPACES OR LOW-VALUES                   CL*27
00985         MOVE ' LIFE '            TO PI-LIFE-OVERRIDE-L6              CL*27
00986      ELSE                                                            CL*27
00987         MOVE CF-LIFE-OVERRIDE-L6 TO PI-LIFE-OVERRIDE-L6.             CL*27
00988                                                                      CL*27
00989      IF CF-LIFE-OVERRIDE-L12 = SPACES OR LOW-VALUES                  CL*27
00990         MOVE '    LIFE    '      TO PI-LIFE-OVERRIDE-L12             CL*27
00991      ELSE                                                            CL*27
00992         MOVE CF-LIFE-OVERRIDE-L12 TO PI-LIFE-OVERRIDE-L12.           CL*27
00993                                                                      CL*27
00994      IF CF-AH-OVERRIDE-L1 = SPACES OR LOW-VALUES                     CL*27
00995         MOVE 'A'                 TO PI-AH-OVERRIDE-L1                CL*27
00996      ELSE                                                            CL*27
00997         MOVE CF-AH-OVERRIDE-L1   TO PI-AH-OVERRIDE-L1.               CL*27
00998                                                                      CL*27
00999      IF CF-AH-OVERRIDE-L2 = SPACES OR LOW-VALUES                     CL*27
01000         MOVE 'AH'                TO PI-AH-OVERRIDE-L2                CL*27
01001      ELSE                                                            CL*27
01002         MOVE CF-AH-OVERRIDE-L2   TO PI-AH-OVERRIDE-L2.               CL*27
01003                                                                      CL*27
01004      IF CF-AH-OVERRIDE-L6 = SPACES OR LOW-VALUES                     CL*27
01005         MOVE ' A/H '             TO PI-AH-OVERRIDE-L6                CL*27
01006      ELSE                                                            CL*27
01007         MOVE CF-AH-OVERRIDE-L6   TO PI-AH-OVERRIDE-L6.               CL*27
01008                                                                      CL*27
01009      IF CF-AH-OVERRIDE-L12 =  SPACES OR LOW-VALUES                   CL*27
01010         MOVE '    A/H     '      TO PI-AH-OVERRIDE-L12               CL*27
01011      ELSE                                                            CL*27
01012         MOVE CF-AH-OVERRIDE-L12  TO PI-AH-OVERRIDE-L12.              CL*27
01013                                                                      CL*27
01014      IF CF-MEMBER-CAPTION = SPACES OR LOW-VALUES                     CL*27
01015          MOVE 'MEMBER NO.'       TO PI-MEMBER-CAPTION                CL*27
01016      ELSE                                                            CL*27
01017          MOVE CF-MEMBER-CAPTION  TO PI-MEMBER-CAPTION.               CL*27
01018                                                                      CL*27
01019                                                                      CL*27
01020  2110-EXIT.                                                          CL*27
01021      EXIT.                                                           CL*27
01022                                                                      CL*27
01023      EJECT                                                           CL*27
01024 ******************************************************************   CL*27
01025 *         INITIALIZE PROCESSOR CONTROLS PROGRAM INTERFACE        *   CL*27
01026 ******************************************************************   CL*27
01027                                                                      CL*27
01028  2200-MOVE-PROC.                                                     CL*27
01029                                                                      CL*27
01030      MOVE CF-MESSAGE-AT-LOGON-CAP                                    CL*27
01031                                  TO PI-MSG-AT-LOGON-CAP.             CL*27
01032      MOVE CF-ACCESS-CD-GENL      TO PI-PROCESSOR-ID.                 CL*27
01033      MOVE CF-PROCESSOR-PRINTER   TO PI-PROCESSOR-PRINTER.            CL*27
01034      MOVE CF-PROCESSOR-PASSWORD  TO PI-PROCESSOR-PASSWORD.           CL*27
01035      MOVE CF-PROCESSOR-USER-ALMIGHTY                                 CL*27
01036                                  TO PI-PROCESSOR-USER-ALMIGHTY.      CL*27
01037      MOVE CF-LANGUAGE-TYPE       TO PI-LANGUAGE-TYPE.                CL*29
011812     MOVE CF-CSR-IND             TO PI-PROCESSOR-CSR-IND.
01038                                                                      CL*27
01039      MOVE 'X'                    TO PI-PROCESSOR-SYS-ACCESS.         CL*27
01040                                                                      CL*27
01041      IF SYSIDI = 'CL'                                                CL*27
01042          IF ACCESS-TO-CLAIMS                                         CL*27
01043              MOVE '1'            TO PI-PROCESSOR-SYS-ACCESS          CL*27
01044              MOVE CF-PROCESSOR-CARRIER                               CL*27
01045                                  TO PI-CARRIER-SECURITY              CL*27
01046              MOVE CF-PROCESSOR-ACCOUNT                               CL*27
01047                                  TO PI-ACCOUNT-SECURITY.             CL*27
01048                                                                      CL*27
01049      IF SYSIDI = 'CR'                                                CL*27
01050          IF ACCESS-TO-CREDIT                                         CL*27
01051              MOVE '2'            TO PI-PROCESSOR-SYS-ACCESS          CL*27
01052              MOVE CF-PROCESSOR-CARRIER                               CL*27
01053                                  TO PI-CARRIER-SECURITY              CL*27
01054              MOVE CF-PROCESSOR-ACCOUNT                               CL*27
01055                                  TO PI-ACCOUNT-SECURITY.             CL*27
01056                                                                      CL*27
01057      IF SYSIDI = 'CV'                                                CL*27
01058          IF ACCESS-TO-LIFE                                           CL*27
01059              MOVE '3'            TO PI-PROCESSOR-SYS-ACCESS.         CL*27
01060                                                                      CL*27
01061      IF ACCESS-TO-ALL-SYSTEMS                                        CL*27
01062          MOVE SPACES             TO PI-PROCESSOR-SYS-ACCESS.         CL*27
01063                                                                      CL*27
01064  2205-PI-AREA-SET.                                                   CL*27
01065                                                                      CL*27
01066      IF SYSIDI = 'CR'                                                CL*27
01067          MOVE +1                 TO SYS-IDX                          CL*27
01068          MOVE CF-APPLICATION-FORCE(SYS-IDX)                          CL*27
01069                                  TO PI-FORCE-CAP                     CL*27
01070          MOVE CF-ADMINISTRATION-CONTROLS(SYS-IDX)                    CL*27
01071                                  TO PI-SYSTEM-CONTROLS.              CL*27
01072                                                                      CL*27
01073      IF SYSIDI = 'CL'                                                CL*27
01074          MOVE +2                 TO SYS-IDX                          CL*27
01075          MOVE CF-APPLICATION-FORCE(SYS-IDX)                          CL*27
01076                                  TO PI-FORCE-CAP                     CL*27
01077          MOVE CF-ADMINISTRATION-CONTROLS(SYS-IDX)                    CL*27
01078                                  TO PI-SYSTEM-CONTROLS.              CL*27
01079                                                                      CL**6
01080      EJECT                                                           CL*27
01081 ******************************************************************   CL*27
01082 * BUILD AND WRITE (CREDIT / CLAIMS) SECURITY TEMP STORAGE RECORD *   CL*27
01083 ******************************************************************   CL*27
01084                                                                      CL*27
01085      MOVE +1                     TO SYS-IDX.                         CL*27
01086                                                                      CL*27
01087      PERFORM 3000-UPDATE-CREDIT-APP THRU 3000-EXIT                   CL*27
01088          VARYING APP-IDX FROM +1 BY +1                               CL*27
01089              UNTIL APP-IDX GREATER THAN +40.                         CL*27
01090                                                                      CL*27
01091      MOVE +2                  TO SYS-IDX.                            CL*27
01092                                                                      CL*27
01093      PERFORM 3100-UPDATE-CLAIMS-APP THRU 3100-EXIT                   CL*27
01094          VARYING APP-IDX FROM +1 BY +1                               CL*27
01095              UNTIL APP-IDX GREATER THAN +30.                         CL*27
01096                                                                      CL*27
01097      EXEC CICS HANDLE CONDITION                                      CL*27
01098          QIDERR (2210-QIDERR)                                        CL*27
01099      END-EXEC.                                                       CL*27
01100                                                                      CL*27
01101      MOVE EIBTRMID TO QID-TERM.                                      CL*27
01102                                                                   EL125
01103      EXEC CICS DELETEQ TS                                            CL*27
01104          QUEUE  (QID)                                                CL*27
01105      END-EXEC.                                                       CL*27
01106                                                                      CL*27
01107  2210-QIDERR.                                                        CL*27
01108                                                                      CL*27
01109      EXEC CICS WRITEQ TS                                             CL*27
01110          QUEUE  (QID)                                                CL*27
01111          FROM   (SECURITY-CONTROL)                                   CL*27
01112          LENGTH (SC-COMM-LENGTH)                                     CL*27
01113          ITEM   (QID-ITEM)                                           CL*27
01114      END-EXEC.                                                       CL*27
01115                                                                      CL**8
01116      MOVE QID                    TO PI-SECURITY-TEMP-STORE-ID.       CL*27
01117                                                                      CL*27
01118      EJECT                                                           CL*27
01119 **************************************************************       CL*27
01120 * BUILD AND WRITE (CREDIT CARD) SECURITY TEMP STORAGE RECORD *       CL*27
01121 **************************************************************       CL*27
01122                                                                      CL*27
01123      IF NOT PI-HAS-CLAS-IC-CRDTCRD                                   CL*27
01124          GO TO 2220-CHECK-ACCT-RECV.                                 CL*27
01125                                                                      CL*27
01126      MOVE +3                     TO SYS-IDX.                         CL*27
01127      MOVE '125C'                 TO QID-RCD.                         CL*27
01128                                                                      CL**6
01129      PERFORM 3200-UPDATE-CRDTCRD-APP THRU 3200-EXIT                  CL*27
01130          VARYING APP-IDX FROM +1 BY +1                               CL*27
01131              UNTIL APP-IDX GREATER THAN +44.                         CL*27
01132                                                                      CL*27
01133      EXEC CICS HANDLE CONDITION                                      CL*27
01134          QIDERR (2215-QIDERR)                                        CL*27
01135      END-EXEC.                                                       CL*27
01136                                                                      CL*27
01137      EXEC CICS DELETEQ TS                                            CL*27
01138          QUEUE  (QID)                                                CL*27
01139      END-EXEC.                                                       CL*27
01140                                                                      CL*27
01141                                                                      CL*27
01142  2215-QIDERR.                                                        CL*27
01143                                                                      CL*27
01144      EXEC CICS WRITEQ TS                                             CL*27
01145          QUEUE  (QID)                                                CL*27
01146          FROM   (SECURITY-CONTROL-C)                                 CL*27
01147          LENGTH (SC-COMM-LENGTH)                                     CL*27
01148          ITEM   (QID-ITEM)                                           CL*27
01149      END-EXEC.                                                       CL*27
01150                                                                      CL**8
01151      EJECT                                                           CL*27
01152 ******************************************************               CL*27
01153 * BUILD AND WRITE (A/R) SECURITY TEMP STORAGE RECORD *               CL*27
01154 ******************************************************               CL*27
01155                                                                      CL*27
01156  2220-CHECK-ACCT-RECV.                                               CL*27
01157                                                                      CL*27
01158      IF NOT PI-AR-PROCESSING                                         CL*27
01159          GO TO 2230-CHECK-MORTGAGE.                                  CL*27
01160                                                                      CL*27
01161      MOVE +4                     TO SYS-IDX.                         CL*27
01162      MOVE '125D'                 TO QID-RCD.                         CL*27
01163                                                                      CL**8
01164      PERFORM 3300-UPDATE-AR-APP THRU 3300-EXIT                       CL*27
01165          VARYING APP-IDX FROM +1 BY +1                               CL*27
01166              UNTIL APP-IDX GREATER THAN +44.                         CL*27
01167                                                                      CL*27
01168      EXEC CICS HANDLE CONDITION                                      CL*27
01169          QIDERR (2225-QIDERR)                                        CL*27
01170      END-EXEC.                                                       CL*27
01171                                                                      CL*27
01172      EXEC CICS DELETEQ TS                                            CL*27
01173          QUEUE  (QID)                                                CL*27
01174      END-EXEC.                                                       CL*27
01175                                                                      CL*27
01176                                                                      CL*27
01177  2225-QIDERR.                                                        CL*27
01178                                                                      CL*27
01179      EXEC CICS WRITEQ TS                                             CL*27
01180          QUEUE  (QID)                                                CL*27
01181          FROM   (SECURITY-CONTROL-D)                                 CL*27
01182          LENGTH (SC-COMM-LENGTH)                                     CL*27
01183          ITEM   (QID-ITEM)                                           CL*27
01184      END-EXEC.                                                       CL*27
01185                                                                      CL**8
01186      EJECT                                                           CL*27
01187 ***********************************************************          CL*27
01188 * BUILD AND WRITE (MORTGAGE) SECURITY TEMP STORAGE RECORD *          CL*27
01189 ***********************************************************          CL*27
01190                                                                      CL*27
01191  2230-CHECK-MORTGAGE.                                                CL*27
01192                                                                      CL*27
01193      IF NOT PI-HAS-CLAS-IC-MORTGAGE                                  CL*27
01194          GO TO 2290-EXIT.                                            CL*27
01195                                                                      CL*27
01196      MOVE +1                     TO SYS-IDX                          CL*27
01197                                     SEQUENCE-NO.                     CL*27
01198                                                                      CL**8
01199      PERFORM 2000-READ-FILE THRU 2010-EXIT.                          CL*27
01200                                                                      CL**8
01201      IF SYSIDI = 'CV'                                                CL*27
01202          MOVE CF-PROCESSOR-CARRIER                                   CL*27
01203                                  TO PI-CARRIER-SECURITY              CL*27
01204          MOVE CF-PROCESSOR-ACCOUNT                                   CL*27
01205                                  TO PI-ACCOUNT-SECURITY              CL*27
01206          MOVE +1                 TO SYS-IDX                          CL*27
01207          MOVE CF-APPLICATION-FORCE(SYS-IDX)                          CL*27
01208                                  TO PI-FORCE-CAP                     CL*27
01209          MOVE CF-ADMINISTRATION-CONTROLS(SYS-IDX)                    CL*27
01210                                  TO PI-SYSTEM-CONTROLS.              CL*27
01211                                                                      CL*27
01212      MOVE +0                     TO SEQUENCE-NO.                     CL*27
01213                                                                      CL*27
01214      MOVE '125E'                 TO QID-RCD.                         CL*27
01215                                                                      CL**8
01216      PERFORM 3400-UPDATE-MP-APP THRU 3400-EXIT                       CL*27
01217          VARYING APP-IDX FROM +1 BY +1                               CL*27
01218              UNTIL APP-IDX GREATER THAN +44.                         CL*27
01219                                                                      CL*27
01220      EXEC CICS HANDLE CONDITION                                      CL*27
01221          QIDERR (2235-QIDERR)                                        CL*27
01222      END-EXEC.                                                       CL*27
01223                                                                      CL*27
01224      EXEC CICS DELETEQ TS                                            CL*27
01225          QUEUE  (QID)                                                CL*27
01226      END-EXEC.                                                       CL*27
01227                                                                      CL**6
01228                                                                      CL*27
01229  2235-QIDERR.                                                        CL*27
01230                                                                      CL*27
01231      EXEC CICS WRITEQ TS                                             CL*27
01232          QUEUE  (QID)                                                CL*27
01233          FROM   (SECURITY-CONTROL-E)                                 CL*27
01234          LENGTH (SC-COMM-LENGTH)                                     CL*27
01235          ITEM   (QID-ITEM)                                           CL*27
01236      END-EXEC.                                                       CL*27
01237                                                                      CL*27
01238  2290-EXIT.                                                          CL*27
01239      EXIT.                                                           CL*27
01240                                                                      CL*27
01241      EJECT                                                           CL*27
01242 **************************************************************       CL*27
01243 *  ROUTINES TO MOVE SECURITY SWITCH SETTINGS FROM PROCESSOR  *       CL*27
01244 *  RECORD TO THE APPROPRIATE TEMP STORAGE SECURITY RECORD.   *       CL*27
01245 **************************************************************       CL*27
01246                                                                      CL**6
01247  3000-UPDATE-CREDIT-APP.                                             CL*27
01248                                                                      CL*27
01249      MOVE CF-APP-SWITCHES(SYS-IDX APP-IDX) TO                        CL*27
01250                                SC-CREDIT-AUTHORIZATION(APP-IDX).     CL*27
01251                                                                      CL**6
01252  3000-EXIT.                                                          CL*27
01253      EXIT.                                                           CL*27
01254                                                                      CL**6
01255  3100-UPDATE-CLAIMS-APP.                                             CL*27
01256                                                                      CL*27
01257      MOVE CF-APP-SWITCHES(SYS-IDX APP-IDX) TO                        CL*27
01258                                SC-CLAIMS-AUTHORIZATION(APP-IDX).     CL*27
01259                                                                      CL**8
01260  3100-EXIT.                                                          CL*27
01261      EXIT.                                                           CL*27
01262                                                                      CL**8
01263  3200-UPDATE-CRDTCRD-APP.                                            CL*27
01264                                                                      CL*27
01265      MOVE CF-APP-SWITCHES(SYS-IDX APP-IDX) TO                        CL*27
01266                                SC-CC-AUTHORIZATION(APP-IDX).         CL*27
01267                                                                      CL**8
01268  3200-EXIT.                                                          CL*27
01269      EXIT.                                                           CL*27
01270                                                                      CL**8
01271  3300-UPDATE-AR-APP.                                                 CL*27
01272                                                                      CL*27
01273      MOVE CF-APP-SWITCHES(SYS-IDX APP-IDX) TO                        CL*27
01274                                SC-AR-AUTHORIZATION(APP-IDX).         CL*27
01275                                                                      CL*27
01276  3300-EXIT.                                                          CL*27
01277      EXIT.                                                           CL*27
01278                                                                      CL*27
01279  3400-UPDATE-MP-APP.                                                 CL*27
01280                                                                      CL*27
01281      MOVE CF-APP-SWITCHES(SYS-IDX APP-IDX) TO                        CL*27
01282                                SC-MP-AUTHORIZATION(APP-IDX).         CL*27
01283                                                                      CL*27
01284  3400-EXIT.                                                          CL*27
01285      EXIT.                                                           CL*27
01286                                                                      CL*27
01287      EJECT                                                           CL*27
01288  8100-SEND-MAP.                                                      CL*27
01289                                                                      CL*27
01290      MOVE WS-CURRENT-DATE        TO DATEO.                           CL*27
01291      MOVE EIBTIME                TO TIME-IN.                         CL*27
01292      MOVE TIME-OUT               TO TIMEO.                           CL*27
01293      MOVE EMI-MESSAGE-AREA (1)   TO MSG1O.                           CL*27
01294      MOVE EMI-MESSAGE-AREA (2)   TO MSG2O.                           CL*27
01295                                                                      CL*27
01296      EXEC CICS SEND                                                  CL*27
01297          MAP      ('EL125A')                                         CL*27
01298          MAPSET   ('EL125S')                                         CL*27
01299          ERASE                                                       CL*27
01300          FREEKB                                                      CL*27
01301          CURSOR                                                      CL*27
01302      END-EXEC.                                                       CL*27
01303                                                                      CL*27
01304      GO TO 9000-RETURN-TRANS.                                        CL*27
01305                                                                      CL*27
01306  8110-SEND-DATA.                                                     CL*27
01307      IF ENTRY-VIA-MAIN-MENU                                          CL*27
01308          GO TO 8100-SEND-MAP.                                        CL*27
01309                                                                      CL*27
01310      MOVE WS-CURRENT-DATE        TO DATEO.                           CL*27
01311      MOVE EIBTIME                TO TIME-IN.                         CL*27
01312      MOVE TIME-OUT               TO TIMEO.                           CL*27
01313      MOVE EMI-MESSAGE-AREA (1)   TO MSG1O.                           CL*27
01314      MOVE EMI-MESSAGE-AREA (2)   TO MSG2O.                           CL*27
01315                                                                      CL*27
01316      EXEC CICS SEND                                                  CL*27
01317          MAP        ('EL125A')                                       CL*27
01318          MAPSET     ('EL125S')                                       CL*27
01319          DATAONLY                                                    CL*27
01320          FREEKB                                                      CL*27
01321          CURSOR                                                      CL*27
01322      END-EXEC.                                                       CL*27
01323                                                                      CL*27
01324      GO TO 9000-RETURN-TRANS.                                        CL*27
01325                                                                      CL*27
01326  8200-NO-UPDATES.                                                    CL*27
01327      MOVE LOW-VALUES             TO PI-ENTRY-CD-1.                   CL*27
01328      MOVE LIT-LOGOFF             TO CALL-PGM.                        CL*27
01329      GO TO 8220-XCTL.                                                CL*27
01330                                                                      CL**8
01331  8210-XCTL-MAIN-MENU.                                                CL*27
01332                                                                      CL*27
01333      MOVE COMPIDI                    TO PI-COMPANY-ID.               CL*27
01334                                                                      CL**8
01335      IF USERIDI = LGXX-USER                                          CL*27
01336          IF (SYSIDI = 'CL' OR 'CR' OR 'CV')                          CL*27
01337              NEXT SENTENCE                                           CL*27
01338          ELSE                                                        CL*27
01339              IF (COMPIDI = 'AIG' OR 'AUK')                           CL*27
01340                  MOVE 'CL'           TO SYSIDI                       CL*27
01341              ELSE                                                    CL*27
01342                  MOVE 'CR'           TO SYSIDI.                      CL*27
01343                                                                      CL*27
01344      IF SYSIDI = 'CL'                                                CL*27
01345          MOVE LIT-CLAIM-MASTER       TO CALL-PGM                     CL*27
01346          MOVE '1'                    TO PI-SESSION-IN-PROGRESS.      CL*27
01347                                                                      CL*27
01348      IF SYSIDI = 'CR'                                                CL*27
01349          MOVE LIT-CREDIT-MASTER      TO CALL-PGM                     CL*27
01350          MOVE '2'                    TO PI-SESSION-IN-PROGRESS.      CL*27
01351                                                                      CL*27
01352      IF SYSIDI = 'CV'                                                CL*27
01353          MOVE LIT-MORTGAGE-MASTER    TO CALL-PGM                     CL*27
01354          MOVE '4'                    TO PI-SESSION-IN-PROGRESS.      CL*27
01355                                                                      CL*27
01356  8220-XCTL.                                                          CL*27
01357      EXEC CICS XCTL                                                  CL*27
01358          PROGRAM  (CALL-PGM)                                         CL*27
01359          COMMAREA (PROGRAM-INTERFACE-BLOCK)                          CL*27
01360          LENGTH   (PI-COMM-LENGTH)                                   CL*27
01361      END-EXEC.                                                       CL*27
01362                                                                      CL*27
01363  8230-NOT-OPEN.                                                      CL*27
01364      MOVE HIGH-VALUES            TO PI-ENTRY-CD-1.                   CL*27
01365      MOVE LIT-LOGOFF             TO CALL-PGM.                        CL*27
01366      GO TO 8220-XCTL.                                                CL*27
01367                                                                      CL*27
01368  8240-PGMIDERR.                                                      CL*27
01369      EXEC CICS HANDLE CONDITION                                      CL*27
01370          PGMIDERR (8990-XCTL-ERROR)                                  CL*27
01371      END-EXEC.                                                       CL*27
01372                                                                   EL125
01373      MOVE CALL-PGM               TO PI-CALLING-PROGRAM.              CL*27
01374      MOVE LIT-LOGOFF             TO CALL-PGM.                        CL*27
01375      GO TO 8220-XCTL.                                                CL*27
01376                                                                   EL125
01377  8250-LOGON-ERROR.                                                   CL*27
01378      EXEC CICS HANDLE CONDITION                                      CL*27
01379          PGMIDERR (8990-XCTL-ERROR)                                  CL*27
01380      END-EXEC.                                                       CL*27
01381                                                                      CL*27
01382      MOVE WS-CURRENT-TERM-ON     TO PI-CALLING-PROGRAM.              CL*27
01383      MOVE LIT-LOGOFF             TO CALL-PGM.                        CL*27
01384      GO TO 8220-XCTL.                                                CL*27
01385                                                                      CL*27
01386  8990-XCTL-ERROR.                                                    CL*27
01387      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                     CL*27
01388      MOVE CALL-PGM               TO LOGOFF-PGM.                      CL*27
01389                                                                      CL*27
01390      EXEC CICS SEND TEXT                                             CL*27
01391          FROM   (LOGOFF-TEXT)                                        CL*27
01392          LENGTH (LOGOFF-LENGTH)                                      CL*27
01393          ERASE                                                       CL*27
01394          FREEKB                                                      CL*27
01395      END-EXEC.                                                       CL*27
01396                                                                      CL*27
01397      GO TO 9100-RETURN-CICS.                                         CL*27
01398      EJECT                                                           CL*27
01399  9000-RETURN-TRANS.                                                  CL*27
01400      EXEC CICS RETURN                                                CL*27
01401          TRANSID  (EXCR)                                             CL*27
01402          COMMAREA (PROGRAM-INTERFACE-BLOCK)                          CL*27
01403          LENGTH   (PI-COMM-LENGTH)                                   CL*27
01404      END-EXEC.                                                       CL*27
01405                                                                      CL*27
01406      GOBACK.                                                         CL*27
01407                                                                      CL*27
01408  9100-RETURN-CICS.                                                   CL*27
01409      EXEC CICS RETURN                                                CL*27
01410      END-EXEC.                                                       CL*27
01411                                                                      CL*27
01412      GOBACK.                                                         CL*27
01413                                                                   EL125
01414  9900-ERROR-FORMAT.                                                  CL*27
01415      IF EMI-ERRORS-COMPLETE                                          CL*27
01416          GO TO 9900-EXIT.                                            CL*27
01417                                                                      CL*27
01418      EXEC CICS LINK                                                  CL*27
01419          PROGRAM  ('EL001')                                          CL*27
01420          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                    CL*27
01421          LENGTH   (EMI-COMM-LENGTH)                                  CL*27
01422      END-EXEC.                                                       CL*27
01423                                                                      CL*27
01424  9900-EXIT.                                                          CL*27
01425      EXIT.                                                           CL*27
01426                                                                      CL*27
01427  9990-ABEND.                                                         CL*27
01428      MOVE DFHEIBLK               TO EMI-LINE1.                       CL*27
01429                                                                      CL*27
01430      EXEC CICS LINK                                                  CL*27
01431          PROGRAM   ('EL004')                                         CL*27
01432          COMMAREA  (EMI-LINE1)                                       CL*27
01433          LENGTH    (72)                                              CL*27
01434      END-EXEC.                                                       CL*27
01435                                                                      CL*27
01436      GO TO 8110-SEND-DATA.                                           CL*27
