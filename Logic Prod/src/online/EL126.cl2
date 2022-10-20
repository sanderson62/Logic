00001  IDENTIFICATION DIVISION.                                         10/18/96
00002                                                                   EL126
00003  PROGRAM-ID.                 EL126 .                                 LV022
00004 *              PROGRAM CONVERTED BY                                  CL*18
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*18
00006 *              CONVERSION DATE 04/22/94 16:21:39.                    CL*18
00007 *                            VMOD=2.022.                             CL*22
00008 *                                                                 EL126
00008 *                                                                 EL126
00009 *AUTHOR.     LOGIC,INC.                                              CL*18
00010 *            DALLAS, TEXAS.                                          CL*18
00011                                                                   EL126
00012 *DATE-COMPILED.                                                      CL*18
00013                                                                      CL*12
00014 *SECURITY.   *****************************************************   CL*18
00015 *            *                                                   *   CL*18
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*18
00017 *            *                                                   *   CL*18
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*18
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*18
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *   CL*18
00021 *            *                                                   *   CL*18
00022 *            *****************************************************   CL*18
00023                                                                   EL126
00024 *REMARKS.    TRANSACTION - EX00 - MASTER MENU                        CL*11
00025 *                                                                    CL**5
00026      EJECT                                                        EL126
00027  ENVIRONMENT DIVISION.                                            EL126
00028  DATA DIVISION.                                                   EL126
00029  WORKING-STORAGE SECTION.                                         EL126
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL126
00031  77  FILLER  PIC X(32)  VALUE '*    EL126 WORKING STORAGE     *'. EL126
00032  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.022 ************'.    CL*22
00033                                                                   EL126
00034      COPY ELCSCTM.                                                   CL*12
00035                                                                      CL*12
00036      COPY ELCSCRTY.                                                  CL*12
00037                                                                   EL126
00038  01  WS-DATE-AREA.                                                EL126
00039      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL126
00040      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL126
00041                                                                   EL126
00042  01  MISC-WORK-AREAS.                                             EL126
00043      12  MAP-NAMEA           PIC X(8)    VALUE 'EL126A'.          EL126
00044      12  MAP-NAMEB           PIC X(8)    VALUE 'EL126B'.          EL126
00045      12  MAPSET-NAME         PIC X(8)    VALUE 'EL126S'.          EL126
00046      12  TRANS-ID            PIC X(4)    VALUE 'EX00'.            EL126
00047      12  PGM-NAME            PIC X(8).                            EL126
00048      12  EZP-NAME            PIC X(8).                               CL*11
00049      12  THIS-PGM            PIC X(8)    VALUE 'EL126'.           EL126
00050      12  TIME-IN             PIC 9(7).                            EL126
00051      12  TIME-OUT-R    REDEFINES TIME-IN.                         EL126
00052          16  FILLER          PIC X.                               EL126
00053          16  TIME-OUT        PIC 99V99.                           EL126
00054          16  FILLER          PIC X(2).                            EL126
00055      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL126
00056      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL126
00057      12  XCTL-101            PIC X(8)    VALUE 'EL101'.           EL126
00058      12  XCTL-109            PIC X(8)    VALUE 'EL109'.              CL**7
00059      12  XCTL-125            PIC X(8)    VALUE 'EL125'.              CL**7
00060      12  XCTL-127            PIC X(8)    VALUE 'EL127'.           EL126
00061      12  XCTL-130            PIC X(8)    VALUE 'EL130'.           EL126
00062      12  XCTL-EM130          PIC X(8)    VALUE 'EM130'.              CL*17
00063      12  XCTL-132            PIC X(8)    VALUE 'EL132'.           EL126
00064      12  XCTL-143            PIC X(8)    VALUE 'EL143'.           EL126
00065      12  XCTL-144            PIC X(8)    VALUE 'EL144'.              CL**5
00066      12  XCTL-152            PIC X(8)    VALUE 'EL152'.           EL126
00067      12  XCTL-155            PIC X(8)    VALUE 'EL155'.           EL126
00068      12  XCTL-160            PIC X(8)    VALUE 'EL160'.           EL126
00069      12  XCTL-162            PIC X(8)    VALUE 'EL162'.           EL126
00070      12  XCTL-171            PIC X(8)    VALUE 'EL171'.           EL126
00071      12  XCTL-179            PIC X(8)    VALUE 'EL179'.           EL126
00072      12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.              CL*10
00073      12  XCTL-EM626          PIC X(8)    VALUE 'EM626'.              CL*10
00074      12  XCTL-725            PIC X(8)    VALUE 'EL725'.              CL**7
00075      12  XCTL-190            PIC X(8)    VALUE 'EL190'.              CL*14
00076                                                                      CL**7
00077      12  LINK-001            PIC X(8)    VALUE 'EL001'.              CL**6
00078      12  LINK-004            PIC X(8)    VALUE 'EL004'.              CL**6
00079      12  LINK-006            PIC X(8)    VALUE 'EZP210L'.            CL*11
00080      12  LINK-CLDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL126
00081      12  CREDIT-SYSTEM       PIC X(2)    VALUE 'CR'.              EL126
00082      12  SV-TODAY            PIC XX.                              EL126
00083      12  ELCNTL-FILE-ID      PIC X(08)   VALUE 'ELCNTL  '.        EL126
00084      12  WS-OLD-COMPANY-ID   PIC X(03)   VALUE SPACES.               CL*13
00085                                                                      CL*11
00086  01  MESG-COMM-AREA          PIC X(40).                              CL*11
00087                                                                   EL126
00088  01  MISC-COMP.                                                   EL126
00089      12  WS-IC               PIC S9(4)   VALUE -1  COMP.          EL126
00090      12  SUB1                PIC S9(4)   VALUE +0  COMP.          EL126
00091      12  SUB2                PIC S9(4)   VALUE +0  COMP.          EL126
00092      12  SC-ITEM             PIC S9(4)   VALUE +0001 COMP.        EL126
00093                                                                   EL126
00094      EJECT                                                        EL126
00095  01  ERROR-NUMBERS.                                               EL126
00096      05  ER-0000                 PIC X(4)  VALUE '0000'.          EL126
00097      05  ER-0002                 PIC X(4)  VALUE '0002'.          EL126
00098      05  ER-0007                 PIC X(4)  VALUE '0007'.          EL126
00099      05  ER-0008                 PIC X(4)  VALUE '0008'.          EL126
00100      05  ER-0004                 PIC X(4)  VALUE '0004'.          EL126
00101      05  ER-0029                 PIC X(4)  VALUE '0029'.          EL126
00102      05  ER-0035                 PIC X(4)  VALUE '0035'.             CL*13
00103      05  ER-0070                 PIC X(4)  VALUE '0070'.          EL126
00104      05  ER-2566                 PIC X(4)  VALUE '2566'.          EL126
00105      05  ER-2569                 PIC X(4)  VALUE '2569'.          EL126
00106      05  ER-8964                 PIC X(4)  VALUE '8964'.             CL*21
00107      05  ER-9303                 PIC X(4)  VALUE '9303'.             CL**5
00108      05  ER-0021                 PIC X(4)  VALUE '0021'.          EL126
00109                                                                   EL126
00110  01  ACCESS-KEYS.                                                 EL126
00111      12  ELCNTL-KEY.                                              EL126
00112          16  CK-COMP-ID      PIC X(3).                            EL126
00113          16  CK-REC-TYPE     PIC X       VALUE 'R'.                  CL**2
00114          16  CK-PROC-ID      PIC X(4).                            EL126
00115          16  FILLER          PIC S9(4)   VALUE +0  COMP.          EL126
00116                                                                   EL126
00117      EJECT                                                        EL126
00118      COPY ELCDATE.                                                   CL*12
00119                                                                   EL126
00120      EJECT                                                        EL126
00121      COPY ELCLOGOF.                                                  CL*12
00122                                                                   EL126
00123      EJECT                                                        EL126
00124      COPY ELCATTR.                                                   CL*12
00125                                                                   EL126
00126      EJECT                                                        EL126
00127      COPY ELCEMIB.                                                   CL*12
00128                                                                   EL126
00129      EJECT                                                        EL126
00130      COPY ELCINTF.                                                   CL*12
00131                                                                   EL126
00132      EJECT                                                        EL126
00133      COPY ELCAID.                                                    CL*12
00134  01  FILLER    REDEFINES DFHAID.                                  EL126
00135      12  FILLER              PIC X(8).                            EL126
00136      12  PF-VALUES           PIC X       OCCURS 2.                EL126
00137                                                                   EL126
00138      EJECT                                                        EL126
00139      COPY EL126S.                                                    CL*12
00140  01  EL126BOR     REDEFINES EL126BI.                              EL126
00141      12  FILLER              PIC X(31).                              CL**2
00142      12  FILLER                 OCCURS 8.                            CL**2
00143          16  FILLER          PIC X(3).                            EL126
00144          16  DATE-OUT        PIC X(8).                            EL126
00145          16  FILLER          PIC X(3).                            EL126
00146          16  MSG-OUT         PIC X(60).                           EL126
00147      12  FILLER              PIC X(73).                           EL126
00148                                                                   EL126
00149      EJECT                                                        EL126
00150  LINKAGE SECTION.                                                 EL126
00151  01  DFHCOMMAREA             PIC X(1024).                         EL126
00152                                                                   EL126
00153 *01 PARMLIST .                                                       CL*18
00154 *    02  FILLER              PIC S9(8)   COMP.                       CL*18
00155 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL*18
00156      EJECT                                                        EL126
00157      COPY ELCCNTL.                                                   CL*12
00158                                                                   EL126
00159      EJECT                                                        EL126
00160  PROCEDURE DIVISION.                                              EL126
00161 *    SERVICE RELOAD PARMLIST.                                        CL*18
00162                                                                   EL126
00163      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL126
00164      MOVE '5'                   TO DC-OPTION-CODE.                EL126
00165      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL126
00166      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL126
00167      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL126
00168                                                                   EL126
00169      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL126
00170                                                                   EL126
00171      IF CREDIT-SESSION                                            EL126
00172          MOVE XCTL-EL626 TO PGM-NAME                                 CL*10
00173          EXEC CICS XCTL                                           EL126
00174              PROGRAM  (PGM-NAME)                                  EL126
00175              COMMAREA (PROGRAM-INTERFACE-BLOCK)                   EL126
00176              LENGTH   (PI-COMM-LENGTH)                            EL126
00177          END-EXEC                                                    CL*10
00178                                                                      CL*10
00179      ELSE                                                            CL*10
00180          IF MORTGAGE-SESSION                                         CL*10
00181              MOVE XCTL-EM626 TO PGM-NAME                             CL*10
00182              EXEC CICS XCTL                                          CL*10
00183                  PROGRAM  (PGM-NAME)                                 CL*10
00184                  COMMAREA (PROGRAM-INTERFACE-BLOCK)                  CL*10
00185                  LENGTH   (PI-COMM-LENGTH)                           CL*10
00186              END-EXEC.                                               CL*10
00187                                                                   EL126
00188      IF EIBCALEN = 0                                              EL126
00189          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL126
00190                                                                   EL126
00191      EXEC CICS HANDLE CONDITION                                   EL126
00192          NOTOPEN  (8820-NOT-OPEN)                                 EL126
00193          NOTFND   (8830-NOT-FOUND)                                EL126
00194          PGMIDERR (9600-PGMID-ERROR)                              EL126
00195          ERROR    (9990-ABEND)                                    EL126
00196      END-EXEC.                                                    EL126
00197                                                                   EL126
00198      IF EIBTRNID = 'EXCR'                                         EL126
00199          GO TO 4000-BUILD-NOTE-SCREEN.                            EL126
00200                                                                   EL126
00201      IF EIBTRNID NOT = TRANS-ID                                   EL126
00202          GO TO 8100-SEND-INITIAL-MAP.                             EL126
00203                                                                   EL126
00204      IF PI-CALLING-PROGRAM = XCTL-125                             EL126
00205          GO TO 8100-SEND-INITIAL-MAP.                             EL126
00206                                                                   EL126
00207      IF EIBAID = DFHCLEAR                                         EL126
00208          GO TO 9400-CLEAR.                                        EL126
00209                                                                   EL126
00210      IF PI-PROCESSOR-ID = 'LGXX'                                  EL126
00211          GO TO 0200-RECEIVE.                                      EL126
00212                                                                   EL126
00213      EXEC CICS READQ TS                                           EL126
00214          QUEUE   (PI-SECURITY-TEMP-STORE-ID)                      EL126
00215          INTO    (SECURITY-CONTROL)                               EL126
00216          LENGTH  (SC-COMM-LENGTH)                                 EL126
00217          ITEM    (SC-ITEM)                                        EL126
00218      END-EXEC.                                                    EL126
00219                                                                   EL126
00220      EJECT                                                        EL126
00221  0200-RECEIVE.                                                    EL126
00222      MOVE LOW-VALUES TO EL126AI.                                  EL126
00223                                                                   EL126
00224      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL126
00225          MOVE ER-0008 TO EMI-ERROR                                EL126
00226          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL126
00227          MOVE -1 TO SELECTL                                       EL126
00228          GO TO 8200-SEND-DATAONLY.                                EL126
00229                                                                   EL126
00230      EXEC CICS RECEIVE                                            EL126
00231          MAP   (MAP-NAMEA)                                        EL126
00232          MAPSET(MAPSET-NAME)                                      EL126
00233          INTO  (EL126AI)                                          EL126
00234      END-EXEC.                                                    EL126
00235                                                                   EL126
00236      IF EIBAID = DFHPF12                                             CL*15
00237         GO TO 9500-PF12.                                             CL*15
00238                                                                      CL*15
00239      IF EIBAID = DFHPF23                                             CL*15
00240         GO TO 8810-PF23.                                             CL*15
00241                                                                      CL*15
00242 *    IF EIBAID = DFHPF24                                             CL*15
00243 *       GO TO 9300-PF24.                                             CL*15
00244                                                                      CL*15
00245      IF PFKEYL = ZEROS                                               CL*15
00246         NEXT SENTENCE                                                CL*15
00247      ELSE                                                            CL*15
00248         IF PFKEYI = '12'                                             CL*15
00249            GO TO 9500-PF12                                           CL*15
00250         ELSE                                                         CL*15
00251            IF PFKEYI = '23'                                          CL*15
00252               GO TO 8810-PF23.                                       CL*15
00253                                                                      CL*15
00254      IF NEWIDI = PI-COMPANY-ID                                       CL*13
00255          MOVE ZEROS              TO NEWIDL.                       EL126
00256                                                                   EL126
00257      IF NEWIDL NOT = ZEROS                                        EL126
00258          MOVE 'CL'               TO PI-NEW-SYSTEM                 EL126
00259          MOVE PI-COMPANY-ID      TO WS-OLD-COMPANY-ID                CL*13
00260          MOVE NEWIDI             TO PI-COMPANY-ID.                EL126
00261                                                                   EL126
00262      IF NEWPWDL NOT = ZEROS                                       EL126
00263          IF NEWIDL NOT = ZEROS                                       CL*13
00264              MOVE NEWPWDI        TO PI-COMPANY-PASSWORD              CL*13
00265          ELSE                                                        CL*13
00266              MOVE 'CL'           TO PI-NEW-SYSTEM                    CL*13
00267              MOVE PI-COMPANY-ID  TO WS-OLD-COMPANY-ID                CL*13
00268              MOVE NEWPWDI        TO PI-COMPANY-PASSWORD.             CL*13
00269                                                                   EL126
00270      IF NEWCDL NOT = ZEROS                                        EL126
00271          IF (NEWCDI NOT = 'CV' AND 'CR' AND 'CL')                    CL*15
00272              MOVE ER-0035        TO EMI-ERROR                        CL*13
00273              MOVE -1             TO NEWCDL                           CL*13
00274              MOVE AL-UABON       TO NEWCDA                           CL*13
00275              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*13
00276              GO TO 8200-SEND-DATAONLY                                CL*14
00277           ELSE                                                       CL*14
00278              IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')           CL*14
00279                  IF NEWCDI IS EQUAL TO 'CR'                          CL*14
00280                      MOVE ER-0035   TO  EMI-ERROR                    CL*14
00281                      MOVE -1        TO  NEWCDL                       CL*14
00282                      MOVE AL-UABON  TO  NEWCDA                       CL*14
00283                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        CL*14
00284                      GO TO 8200-SEND-DATAONLY.                       CL*14
00285                                                                      CL*13
00286      IF NEWCDL NOT = ZEROS                                           CL*13
00287          IF (NEWIDL = ZEROS AND NEWPWDL = ZEROS)                     CL*13
00288              MOVE PI-COMPANY-ID  TO WS-OLD-COMPANY-ID                CL*13
00289              MOVE NEWCDI         TO PI-NEW-SYSTEM                    CL*13
00290          ELSE                                                        CL*13
00291              MOVE NEWCDI         TO PI-NEW-SYSTEM.                   CL*13
00292                                                                   EL126
00293      IF NEWIDL  = ZEROS  AND                                         CL**4
00294         NEWPWDL = ZEROS  AND                                         CL**4
00295         NEWCDL  = ZEROS                                              CL**4
00296          NEXT SENTENCE                                               CL**4
00297      ELSE                                                            CL**4
00298          IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                       CL*13
00299              MOVE XCTL-125           TO  PGM-NAME                    CL*13
00300              GO TO 9300-XCTL                                         CL*13
00301          ELSE                                                        CL*13
00302              MOVE PI-PROCESSOR-ID    TO  CK-PROC-ID                  CL*13
00303              MOVE WS-OLD-COMPANY-ID  TO  CK-COMP-ID                  CL*13
00304              MOVE '2'                TO  CK-REC-TYPE                 CL*13
00305              PERFORM 7000-READ-CNTL-UPDATE THRU 7000-EXIT            CL*13
00306              MOVE SPACES             TO  CF-CURRENT-TERM-ON          CL*13
00307              PERFORM 7010-REWRITE-CNTL THRU 7010-EXIT                CL*13
00308              MOVE XCTL-125           TO  PGM-NAME                    CL*13
00309              GO TO 9300-XCTL.                                        CL*13
00310                                                                   EL126
00311      IF NEWCDL = ZERO                                             EL126
00312          GO TO 0300-CHECK-SELECT-CODE.                            EL126
00313                                                                   EL126
00314      IF PI-MODIFY-CAP NOT = 'C'                                   EL126
00315          IF PI-HAS-CLAS-IC-CREDIT                                 EL126
00316              IF (PI-ACCESS-TO-BOTH-SYSTEMS OR                     EL126
00317                PI-PROCESSOR-SYS-ACCESS = '4')                     EL126
00318                  MOVE XCTL-EL626 TO PGM-NAME                         CL*10
00319                  MOVE '2'        TO PI-SESSION-IN-PROGRESS        EL126
00320                  GO TO 9300-XCTL                                  EL126
00321              ELSE                                                 EL126
00322                  MOVE ER-2566    TO EMI-ERROR                     EL126
00323                  MOVE -1         TO SELECTL                       EL126
00324                  MOVE AL-UNBON   TO SELECTA                       EL126
00325                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL126
00326                  GO TO 8200-SEND-DATAONLY                         EL126
00327          ELSE                                                     EL126
00328              MOVE ER-2569        TO EMI-ERROR                     EL126
00329              MOVE -1             TO SELECTL                       EL126
00330              MOVE AL-UNBON       TO SELECTA                       EL126
00331              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL126
00332              GO TO 8200-SEND-DATAONLY.                            EL126
00333                                                                   EL126
00334  0300-CHECK-SELECT-CODE.                                          EL126
00335      IF SYSSELL = 0                                                  CL*22
00336        AND (PI-CLAIM-PW-SESSION  = '1'                               CL*22
00337        OR   PI-CLAIM-PW-SESSION  = '2')                              CL*22
00338          GO TO 0300-BYPASS-SELECT.                                   CL*22
00339                                                                      CL*22
00340      IF  SYSSELL = 0                                                 CL*21
00341          MOVE 'CR'               TO SYSSELO                          CL*21
00342          MOVE +2                 TO SYSSELL                          CL*21
00343          MOVE AL-UANON           TO SYSSELA                          CL*21
00344                                                                      CL*21
00345      ELSE                                                            CL*21
00346          IF  SYSSELI EQUAL 'CR' OR 'CV'                              CL*21
00347              IF SYSSELI  EQUAL 'CR'                                  CL*22
00348                  MOVE '1'        TO PI-CLAIM-PW-SESSION              CL*22
00349                  MOVE AL-UANON   TO SYSSELA                          CL*22
00350              END-IF                                                  CL*22
00351              IF SYSSELI  EQUAL 'CV'                                  CL*22
00352                  MOVE '2'        TO PI-CLAIM-PW-SESSION              CL*22
00353                  MOVE AL-UANON   TO SYSSELA                          CL*22
00354              END-IF                                                  CL*22
00355                                                                      CL*21
00356          ELSE                                                        CL*21
00357              MOVE ER-8964        TO EMI-ERROR                        CL*21
00358              GO TO 0320-INPUT-ERROR.                                 CL*21
00359                                                                      CL*21
00360      IF SELECTL = 0                                               EL126
00361          MOVE ER-0002            TO EMI-ERROR                     EL126
00362          GO TO 0320-INPUT-ERROR.                                  EL126
00363                                                                   EL126
00364  0300-BYPASS-SELECT.                                                 CL*22
00365      MOVE SPACES TO PI-ENTRY-CD-1.                                EL126
00366                                                                   EL126
00367      IF SELECTI = '01'                                            EL126
00368          MOVE XCTL-101 TO PGM-NAME                                EL126
00369          GO TO 9300-XCTL.                                         EL126
00370                                                                   EL126
00371      IF SELECTI = '02'                                            EL126
00372          MOVE XCTL-171 TO PGM-NAME                                EL126
00373          GO TO 9300-XCTL.                                         EL126
00374                                                                   EL126
00375      IF SELECTI = '06'                                            EL126
00376          MOVE XCTL-132 TO PGM-NAME                                EL126
00377          GO TO 9300-XCTL.                                         EL126
00378                                                                   EL126
00379      IF SELECTI = '07'                                            EL126
00380 *        IF  PI-COMPANY-ID IS EQUAL TO 'CIG' OR 'CUK' OR             CL*21
00381 *                'LGX' OR 'NCX' OR 'BAL' OR 'SLI' OR 'BNS'           CL*21
00382          IF  SYSSELI EQUAL 'CV'                                      CL*21
00383           OR PI-CLAIM-CONVEN                                         CL*22
00384              MOVE XCTL-EM130      TO  PGM-NAME                       CL*17
00385              GO TO 9300-XCTL                                         CL*17
00386          ELSE                                                        CL*17
00387              MOVE XCTL-130        TO  PGM-NAME                       CL*17
00388              GO TO 9300-XCTL.                                        CL*17
00389                                                                   EL126
00390 ****************************************                          EL126
00391 *  TRY XCTL-132 IF YOU HAVE A PROBLEM  *                          EL126
00392 ****************************************                          EL126
00393                                                                   EL126
00394      IF SELECTI = '08'                                            EL126
00395          MOVE XCTL-162 TO PGM-NAME                                   CL**3
00396          GO TO 9300-XCTL.                                            CL**3
00397                                                                   EL126
00398      IF SELECTI = '09'                                               CL**7
00399          MOVE PI-COMPANY-ID TO CK-COMP-ID                         EL126
00400          MOVE '1'           TO CK-REC-TYPE                        EL126
00401          MOVE SPACES        TO CK-PROC-ID                         EL126
00402          EXEC CICS READ                                           EL126
00403               DATASET      (ELCNTL-FILE-ID)                       EL126
00404               SET          (ADDRESS OF CONTROL-FILE)                 CL*18
00405               RIDFLD       (ELCNTL-KEY)                           EL126
00406          END-EXEC                                                 EL126
00407 *        SERVICE RELOAD CONTROL-FILE                                 CL*18
00408          IF CF-PMT-APPROVAL-USED                                     CL*12
00409             MOVE XCTL-143 TO PGM-NAME                             EL126
00410             GO TO 9300-XCTL                                       EL126
00411          ELSE                                                     EL126
00412             MOVE ER-0007    TO EMI-ERROR                          EL126
00413             MOVE -1         TO SELECTL                            EL126
00414             MOVE AL-UNBON   TO SELECTA                            EL126
00415             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL126
00416             GO TO 8200-SEND-DATAONLY.                             EL126
00417                                                                      CL**5
00418      IF SELECTI = '10'                                               CL**5
00419          MOVE XCTL-144 TO PGM-NAME                                   CL**5
00420          GO TO 9300-XCTL.                                            CL**5
00421                                                                   EL126
00422      IF SELECTI = '21'                                            EL126
00423          MOVE XCTL-127 TO PGM-NAME                                EL126
00424          GO TO 9300-XCTL.                                         EL126
00425                                                                   EL126
00426      IF SELECTI = '22'                                            EL126
00427          MOVE XCTL-160 TO PGM-NAME                                EL126
00428          GO TO 9300-XCTL.                                         EL126
00429                                                                   EL126
00430      IF SELECTI = '23'                                            EL126
00431          IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                       CL*16
00432              MOVE XCTL-179               TO  PGM-NAME                CL*16
00433              GO TO 9300-XCTL                                         CL*16
00434          ELSE                                                        CL*16
00435              MOVE SC-CLAIMS-DISPLAY (27) TO  PI-DISPLAY-CAP          CL*16
00436              MOVE SC-CLAIMS-UPDATE  (27) TO  PI-MODIFY-CAP           CL*16
00437              IF NOT DISPLAY-CAP                                      CL*16
00438                  MOVE 'READ'             TO  SM-READ                 CL*16
00439                  PERFORM 9995-SECURITY-VIOLATION                     CL*16
00440                  MOVE -1                 TO  SELECTL                 CL*16
00441                  MOVE AL-UNBON           TO  SELECTA                 CL*16
00442                  MOVE ER-0070            TO  EMI-ERROR               CL*16
00443                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*16
00444                  GO TO 8200-SEND-DATAONLY                            CL*16
00445              ELSE                                                    CL*16
00446                  MOVE XCTL-179           TO  PGM-NAME                CL*16
00447                  GO TO 9300-XCTL.                                    CL*16
00448                                                                      CL**7
00449      IF SELECTI = '24'                                               CL**7
00450          IF PI-HAS-CLAS-IC-CRDTCRD                                   CL**7
00451            MOVE XCTL-725 TO PGM-NAME                                 CL**7
00452            GO TO 9300-XCTL.                                          CL**7
00453                                                                   EL126
00454      IF SELECTI = '30'                                            EL126
00455          MOVE XCTL-109 TO PGM-NAME                                EL126
00456          GO TO 9300-XCTL.                                         EL126
00457                                                                   EL126
00458      IF SELECTI = '31'                                            EL126
00459          MOVE XCTL-155 TO PGM-NAME                                EL126
00460          GO TO 9300-XCTL.                                         EL126
00461                                                                   EL126
00462      IF SELECTI = '32'                                            EL126
00463          MOVE XCTL-152 TO PGM-NAME                                EL126
00464          GO TO 9300-XCTL.                                         EL126
00465                                                                   EL126
00466      IF SELECTI = '33'                                               CL*14
00467          IF (PI-COMPANY-ID = 'AIG' OR 'AUK')                         CL*14
00468              MOVE XCTL-190       TO  PGM-NAME                        CL*14
00469              GO TO 9300-XCTL.                                        CL*14
00470                                                                      CL*14
00471      MOVE ER-0029                TO EMI-ERROR.                    EL126
00472                                                                   EL126
00473  0320-INPUT-ERROR.                                                EL126
00474      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL126
00475                                                                   EL126
00476      MOVE AL-UNBON               TO SELECTA.                      EL126
00477      MOVE -1                     TO SELECTL.                      EL126
00478      GO TO 8200-SEND-DATAONLY.                                    EL126
00479                                                                   EL126
00480      EJECT                                                        EL126
00481  4000-BUILD-NOTE-SCREEN.                                          EL126
00482      IF NOT MSG-AT-LOGON-CAP                                      EL126
00483          GO TO 8100-SEND-INITIAL-MAP.                             EL126
00484                                                                   EL126
00485      IF PI-PROCESSOR-ID = 'LGXX'                                  EL126
00486          GO TO 8100-SEND-INITIAL-MAP.                             EL126
00487                                                                   EL126
00488      MOVE PI-COMPANY-ID   TO CK-COMP-ID.                          EL126
00489      MOVE PI-PROCESSOR-ID TO CK-PROC-ID.                          EL126
00490      MOVE 'R'             TO CK-REC-TYPE                             CL**2
00491                                                                   EL126
00492      EXEC CICS READ                                               EL126
00493          DATASET   (ELCNTL-FILE-ID)                               EL126
00494          SET       (ADDRESS OF CONTROL-FILE)                         CL*18
00495          RIDFLD    (ELCNTL-KEY)                                   EL126
00496      END-EXEC.                                                    EL126
00497                                                                   EL126
00498 *    SERVICE RELOAD CONTROL-FILE.                                    CL*18
00499                                                                   EL126
00500      MOVE LOW-VALUES TO EL126BO.                                  EL126
00501                                                                   EL126
00502      IF DATE-CONVERSION-ERROR                                     EL126
00503          MOVE ER-0021 TO EMI-ERROR                                EL126
00504          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL126
00505          GO TO 8000-SEND-REMINDER-SCREEN.                         EL126
00506                                                                   EL126
00507      MOVE SAVE-BIN-DATE TO SV-TODAY.                              EL126
00508                                                                   EL126
00509      MOVE +1 TO SUB2.                                             EL126
00510                                                                   EL126
00511      PERFORM 4050-LOAD-MESSAGES THRU 4050-EXIT                    EL126
00512          VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 GREATER 8.              CL**2
00513                                                                   EL126
00514      IF SUB2 = +1                                                 EL126
00515          GO TO 8100-SEND-INITIAL-MAP.                             EL126
00516                                                                   EL126
00517      GO TO 8000-SEND-REMINDER-SCREEN.                             EL126
00518                                                                   EL126
00519  4050-LOAD-MESSAGES.                                              EL126
00520      IF CF-START-REMIND-DT (SUB1) GREATER SV-TODAY                EL126
00521          GO TO 4050-EXIT.                                         EL126
00522                                                                   EL126
00523      IF CF-END-REMIND-DT (SUB1) LESS SV-TODAY                     EL126
00524          GO TO 4050-EXIT.                                         EL126
00525                                                                   EL126
00526      MOVE CF-END-REMIND-DT (SUB1) TO DC-BIN-DATE-1.               EL126
00527                                                                   EL126
00528      MOVE ' ' TO DC-OPTION-CODE.                                  EL126
00529      MOVE LINK-CLDATCV TO PGM-NAME.                               EL126
00530      EXEC CICS LINK                                               EL126
00531          PROGRAM  (PGM-NAME)                                      EL126
00532          COMMAREA (DATE-CONVERSION-DATA)                          EL126
00533          LENGTH   (DC-COMM-LENGTH)                                EL126
00534      END-EXEC.                                                    EL126
00535                                                                   EL126
00536      IF DATE-CONVERSION-ERROR                                     EL126
00537          MOVE ER-0021 TO EMI-ERROR                                EL126
00538          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL126
00539          MOVE +7 TO SUB1                                          EL126
00540                     SUB2                                          EL126
00541          GO TO 4050-EXIT.                                         EL126
00542                                                                   EL126
00543      MOVE DC-GREG-DATE-1-EDIT TO DATE-OUT (SUB2).                 EL126
00544                                                                   EL126
00545      MOVE CF-REMINDER-TEXT (SUB1) TO MSG-OUT (SUB2).              EL126
00546                                                                   EL126
00547      ADD +1 TO SUB2.                                              EL126
00548                                                                   EL126
00549  4050-EXIT.                                                       EL126
00550      EXIT.                                                           CL*13
00551                                                                      CL*13
00552      EJECT                                                           CL*13
00553  7000-READ-CNTL-UPDATE.                                              CL*13
00554                                                                      CL*13
00555      EXEC CICS READ                                                  CL*13
00556          DATASET   (ELCNTL-FILE-ID)                                  CL*13
00557          RIDFLD    (ELCNTL-KEY)                                      CL*13
00558          SET       (ADDRESS OF CONTROL-FILE)                         CL*18
00559          UPDATE                                                      CL*13
00560      END-EXEC.                                                       CL*13
00561                                                                      CL*13
00562 *    SERVICE RELOAD CONTROL-FILE.                                    CL*18
00563                                                                      CL*13
00564  7000-EXIT.                                                          CL*13
00565      EXIT.                                                           CL*13
00566                                                                      CL*13
00567  7010-REWRITE-CNTL.                                                  CL*13
00568                                                                      CL*13
00569      EXEC CICS REWRITE                                               CL*13
00570          DATASET   (ELCNTL-FILE-ID)                                  CL*13
00571          FROM      (CONTROL-FILE)                                    CL*13
00572      END-EXEC.                                                       CL*13
00573                                                                      CL*13
00574  7010-EXIT.                                                          CL*13
00575      EXIT.                                                        EL126
00576                                                                   EL126
00577      EJECT                                                        EL126
00578  8000-SEND-REMINDER-SCREEN.                                       EL126
00579      MOVE EIBTIME   TO TIME-IN.                                   EL126
00580      MOVE TIME-OUT  TO RUNTIMBO.                                  EL126
00581      MOVE SAVE-DATE TO RUNDTEBO.                                  EL126
00582                                                                   EL126
00583      MOVE -1 TO DTE1L.                                            EL126
00584                                                                   EL126
00585      IF NOT EMI-NO-ERRORS                                         EL126
00586          SET EMI-INDX TO 1                                        EL126
00587          MOVE EMI-MESSAGE-AREA (EMI-INDX) TO ERRMSGBO.            EL126
00588                                                                   EL126
00589      EXEC CICS SEND                                               EL126
00590          MAP    (MAP-NAMEB)                                       EL126
00591          MAPSET (MAPSET-NAME)                                     EL126
00592          FROM   (EL126BO)                                         EL126
00593          ERASE  CURSOR                                            EL126
00594      END-EXEC.                                                    EL126
00595                                                                   EL126
00596      GO TO 9100-RETURN-TRAN.                                      EL126
00597                                                                   EL126
00598  8100-SEND-INITIAL-MAP.                                           EL126
00599      MOVE LOW-VALUES TO EL126AO.                                  EL126
00600                                                                   EL126
00601      IF PI-NOT-CRDTCRD-USER                                          CL**7
00602          MOVE SPACES             TO CRCRDO.                          CL**7
00603                                                                      CL*14
00604      IF (PI-COMPANY-ID IS NOT EQUAL TO 'AIG' AND 'AUK')              CL*14
00605          MOVE SPACES             TO REJHDO.                          CL*14
00606                                                                      CL**7
00607      MOVE EIBTIME                TO TIME-IN.                         CL**7
00608      MOVE TIME-OUT               TO TIMEO.                           CL**7
00609      IF PI-CLAIM-CONVEN                                              CL*22
00610          MOVE 'CV'               TO SYSSELO                          CL*22
00611      ELSE                                                            CL*22
00612          MOVE 'CR'               TO SYSSELO.                         CL*22
00613                                                                   EL126
00614      MOVE SAVE-DATE              TO DATEO.                        EL126
00615                                                                   EL126
00616      MOVE -1                     TO SELECTL.                      EL126
00617                                                                   EL126
00618      MOVE PI-COMPANY-ID          TO NEWIDO.                       EL126
00619                                                                      CL*20
00620      MOVE SPACES                 TO MESG-COMM-AREA.                  CL*20
00621                                                                      CL**5
00622      PERFORM 8900-CHECK-MAIL  THRU  8900-EXIT.                       CL*11
00623                                                                      CL*11
00624      MOVE MESG-COMM-AREA         TO EMI-MESSAGE-AREA (1).            CL*11
00625      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGAO.                     EL126
00626                                                                   EL126
00627      EXEC CICS SEND                                               EL126
00628          MAP    (MAP-NAMEA)                                       EL126
00629          MAPSET (MAPSET-NAME)                                     EL126
00630          FROM   (EL126AO)                                         EL126
00631          ERASE  CURSOR                                            EL126
00632      END-EXEC.                                                    EL126
00633                                                                   EL126
00634      MOVE 'EL126' TO PI-CALLING-PROGRAM.                          EL126
00635      GO TO 9100-RETURN-TRAN.                                      EL126
00636                                                                   EL126
00637  8200-SEND-DATAONLY.                                              EL126
00638      MOVE EIBTIME                TO TIME-IN.                      EL126
00639      MOVE TIME-OUT               TO TIMEO.                        EL126
00640      MOVE SAVE-DATE              TO DATEO.                        EL126
00641      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGAO.                     EL126
00642      MOVE PI-COMPANY-ID          TO NEWIDO.                       EL126
00643                                                                   EL126
00644      EXEC CICS SEND                                               EL126
00645          MAP    (MAP-NAMEA)                                       EL126
00646          MAPSET (MAPSET-NAME)                                     EL126
00647          FROM   (EL126AO)                                         EL126
00648          DATAONLY CURSOR                                          EL126
00649      END-EXEC.                                                    EL126
00650                                                                   EL126
00651      GO TO 9100-RETURN-TRAN.                                      EL126
00652                                                                   EL126
00653  8300-SEND-TEXT.                                                  EL126
00654      EXEC CICS SEND TEXT                                          EL126
00655          FROM   (LOGOFF-TEXT)                                     EL126
00656          LENGTH (LOGOFF-LENGTH)                                   EL126
00657          ERASE  FREEKB                                            EL126
00658      END-EXEC.                                                    EL126
00659                                                                   EL126
00660      EXEC CICS RETURN                                             EL126
00661      END-EXEC.                                                    EL126
00662                                                                   EL126
00663  8800-UNAUTHORIZED-ACCESS.                                        EL126
00664      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL126
00665                                                                   EL126
00666      GO TO 8300-SEND-TEXT.                                        EL126
00667                                                                   EL126
00668  8810-PF23.                                                       EL126
00669      MOVE DFHPF23  TO PI-ENTRY-CD-1.                                 CL*15
00670      MOVE XCTL-005 TO PGM-NAME.                                   EL126
00671      GO TO 9300-XCTL.                                             EL126
00672                                                                      CL*15
00673                                                                   EL126
00674  8820-NOT-OPEN.                                                   EL126
00675      MOVE 'CONTROL FILE NOT OPEN' TO LOGOFF-MSG.                  EL126
00676      GO TO 8300-SEND-TEXT.                                        EL126
00677                                                                   EL126
00678                                                                      CL*15
00679  8830-NOT-FOUND.                                                  EL126
00680      MOVE 'USER RECORD NOT FOUND' TO LOGOFF-MSG.                  EL126
00681      GO TO 8300-SEND-TEXT.                                        EL126
00682                                                                      CL*15
00683                                                                      CL*11
00684  8900-CHECK-MAIL.                                                    CL*11
00685                                                                      CL*11
00697                                                                      CL*11
00698  8900-EXIT.                                                          CL*11
00699      EXIT.                                                           CL*11
00700                                                                   EL126
00701  9000-RETURN-TO-CICS.                                             EL126
00702      EXEC CICS RETURN                                             EL126
00703      END-EXEC.                                                    EL126
00704                                                                   EL126
00705  9100-RETURN-TRAN.                                                EL126
00706      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL126
00707      MOVE '126A'               TO PI-CURRENT-SCREEN-NO.           EL126
00708                                                                   EL126
00709      EXEC CICS RETURN                                             EL126
00710          TRANSID  (TRANS-ID)                                      EL126
00711          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL126
00712          LENGTH   (PI-COMM-LENGTH)                                EL126
00713      END-EXEC.                                                    EL126
00714                                                                   EL126
00715  9300-XCTL.                                                       EL126
00716      MOVE SPACES TO PI-SAVED-PROGRAM-1                            EL126
00717                     PI-SAVED-PROGRAM-2                            EL126
00718                     PI-SAVED-PROGRAM-3                            EL126
00719                     PI-SAVED-PROGRAM-4                            EL126
00720                     PI-SAVED-PROGRAM-5                            EL126
00721                     PI-SAVED-PROGRAM-6                            EL126
00722                     PI-RETURN-TO-PROGRAM                          EL126
00723                     PI-CONTROL-IN-PROGRESS                        EL126
00724                     PI-ENTRY-CD-2                                 EL126
00725                     PI-RETURN-CODES                               EL126
00726                     PI-UPDATE-BY                                  EL126
00727                     PI-PROGRAM-CONTROLS                           EL126
00728                     PI-PROGRAM-WORK-AREA.                         EL126
00729                                                                   EL126
00730      MOVE ZEROS TO PI-UPDATE-HHMMSS.                              EL126
00731                                                                   EL126
00732      EXEC CICS XCTL                                               EL126
00733          PROGRAM  (PGM-NAME)                                      EL126
00734          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL126
00735          LENGTH   (PI-COMM-LENGTH)                                EL126
00736      END-EXEC.                                                    EL126
00737                                                                   EL126
00738  9400-CLEAR.                                                      EL126
00739      MOVE EIBAID   TO PI-ENTRY-CD-1.                              EL126
00740      MOVE XCTL-005 TO PGM-NAME.                                   EL126
00741      GO TO 9300-XCTL.                                             EL126
00742                                                                   EL126
00743  9500-PF12.                                                       EL126
00744      MOVE XCTL-010 TO PGM-NAME.                                   EL126
00745      GO TO 9300-XCTL.                                             EL126
00746                                                                   EL126
00747                                                                      CL*15
00748  9600-PGMID-ERROR.                                                EL126
00749      EXEC CICS HANDLE CONDITION                                   EL126
00750          PGMIDERR (8300-SEND-TEXT)                                EL126
00751      END-EXEC.                                                    EL126
00752                                                                   EL126
00753      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                     EL126
00754      MOVE ' '          TO PI-ENTRY-CD-1.                          EL126
00755      MOVE XCTL-005     TO PGM-NAME.                               EL126
00756      MOVE PGM-NAME     TO LOGOFF-PGM.                             EL126
00757      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL126
00758      GO TO 9300-XCTL.                                             EL126
00759                                                                      CL*15
00760                                                                   EL126
00761  9700-LINK-DATE-CONVERT.                                          EL126
00762      EXEC CICS LINK                                               EL126
00763          PROGRAM    ('ELDATCV')                                   EL126
00764          COMMAREA   (DATE-CONVERSION-DATA)                        EL126
00765          LENGTH     (DC-COMM-LENGTH)                              EL126
00766      END-EXEC.                                                    EL126
00767                                                                   EL126
00768  9700-EXIT.                                                       EL126
00769      EXIT.                                                        EL126
00770                                                                   EL126
00771  9800-LINK-REM-TERM.                                              EL126
00772  9900-ERROR-FORMAT.                                               EL126
00773      IF NOT EMI-ERRORS-COMPLETE                                   EL126
00774          MOVE LINK-001 TO PGM-NAME                                EL126
00775          EXEC CICS LINK                                           EL126
00776              PROGRAM  (PGM-NAME)                                  EL126
00777              COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)             EL126
00778              LENGTH   (EMI-COMM-LENGTH)                           EL126
00779          END-EXEC.                                                EL126
00780                                                                   EL126
00781  9900-EXIT.                                                       EL126
00782      EXIT.                                                        EL126
00783                                                                   EL126
00784  9990-ABEND.                                                      EL126
00785      MOVE LINK-004      TO PGM-NAME.                              EL126
00786      MOVE DFHEIBLK      TO EMI-LINE1.                             EL126
00787                                                                   EL126
00788      EXEC CICS LINK                                               EL126
00789          PROGRAM  ('EL004')                                       EL126
00790          COMMAREA (EMI-LINE1)                                     EL126
00791          LENGTH   (72)                                            EL126
00792      END-EXEC.                                                    EL126
00793                                                                   EL126
00794      GO TO 8200-SEND-DATAONLY.                                    EL126
00795                                                                   EL126
00796  9995-SECURITY-VIOLATION.                                         EL126
00797                              COPY ELCSCTP.                        EL126
00798                                                                   EL126
00799  9995-EXIT.                                                       EL126
00800      EXIT.                                                        EL126
00801                                                                   EL126
00802  9999-LAST-PARAGRAPH.                                             EL126
00803      GOBACK.                                                      EL126
