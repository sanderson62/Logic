00001  IDENTIFICATION DIVISION.                                         05/06/97
00002                                                                   EL626   
00003  PROGRAM-ID.                 EL626 .                                 LV027
00004 *              PROGRAM CONVERTED BY                                  CL*24
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*24
00006 *              CONVERSION DATE 12/29/94 15:20:51.                    CL*24
00007 *                            VMOD=2.027.                             CL*27
00008 *                                                                 EL626   
00008 *                                                                 EL626   
00009 *AUTHOR.     LOGIC,INC.                                              CL*24
00010 *            DALLAS, TEXAS.                                          CL*24
00011                                                                   EL626   
00012 *DATE-COMPILED.                                                      CL*24
00013                                                                      CL*19
00014 *SECURITY.   *****************************************************   CL*24
00015 *            *                                                   *   CL*24
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*24
00017 *            *                                                   *   CL*24
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*24
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*24
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*24
00021 *            *                                                   *   CL*24
00022 *            *****************************************************   CL*24
011812******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
011812******************************************************************
00023                                                                   EL626   
00024 *REMARKS.    TRANSACTION - EXA4 - MASTER MENU                        CL*10
00025                                                                   EL626   
00026                                                                   EL626   
00027      EJECT                                                        EL626   
00028  ENVIRONMENT DIVISION.                                            EL626   
00029  DATA DIVISION.                                                   EL626   
00030  WORKING-STORAGE SECTION.                                         EL626   
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL626   
00032  77  FILLER  PIC X(32)  VALUE '*    EL626 WORKING STORAGE     *'. EL626   
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.027 *********'.    CL*27
00034                                                                   EL626   
00035                              COPY ELCSCTM.                           CL*17
00036                              COPY ELCSCRTY.                          CL*17
00037                                                                   EL626   
00038     EJECT                                                         EL626   
00039                                                                   EL626   
00040  01  MISC-WORK-AREAS.                                             EL626   
00041      12  SHOT-COMM-LENGTH    PIC S9(4) COMP VALUE +3495.             CL*22
00042      12  SC-ITEM             PIC S9(4) COMP VALUE +1.             EL626   
00043      12  MAP-NAMEA           PIC X(8)    VALUE 'EL626A'.          EL626   
00044      12  MAP-NAMEB           PIC X(8)    VALUE 'EL626B'.          EL626   
00045      12  MAPSET-NAME         PIC X(8)    VALUE 'EL626S'.          EL626   
00046      12  TRANS-ID            PIC X(4)    VALUE 'EXA4'.            EL626   
00047      12  TS-FOUND-SW         PIC X       VALUE SPACE.             EL626   
00048          88 TEMP-NOT-FOUND   VALUE '1'.                           EL626   
00049      12  PGM-NAME            PIC X(8).                            EL626   
00050      12  EZP-NAME            PIC X(8).                               CL*16
00051      12  EDIT-TRANS          PIC X(4)    VALUE 'EXSE'.            EL626   
00052      12  PASS-AREA-LEN       PIC S9(4)   COMP VALUE +16.          EL626   
00053      12  TIME-IN             PIC 9(7).                            EL626   
00054      12  TIME-OUT-R    REDEFINES TIME-IN.                         EL626   
00055          16  FILLER          PIC X.                               EL626   
00056          16  TIME-OUT        PIC 99V99.                           EL626   
00057          16  FILLER          PIC XX.                                 CL*27
00058      12  THIS-PGM            PIC X(8)    VALUE 'EL626'.           EL626   
00059      12  LINK-001            PIC X(8)    VALUE 'EL001'.           EL626   
00060      12  LINK-004            PIC X(8)    VALUE 'EL004'.           EL626   
00061      12  LINK-006            PIC X(8)    VALUE 'EZP210L'.            CL*16
00062      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL626   
00063      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL626   
00064      12  XCTL-109            PIC X(8)    VALUE 'EL109'.           EL626   
00065      12  XCTL-125            PIC X(8)    VALUE 'EL125'.           EL626   
00066      12  XCTL-126            PIC X(8)    VALUE 'EL126'.           EL626   
00067      12  XCTL-127            PIC X(8)    VALUE 'EL127'.           EL626   
PEMTST     12  XCTL-128            PIC X(8)    VALUE 'EL128'.           EL626   
00068      12  XCTL-155            PIC X(8)    VALUE 'EL155'.           EL626   
00069      12  XCTL-179            PIC X(8)    VALUE 'EL179'.           EL626   
00070      12  XCTL-601            PIC X(8)    VALUE 'EL601'.           EL626   
00071      12  XCTL-630            PIC X(8)    VALUE 'EL630'.           EL626   
00072      12  XCTL-631            PIC X(8)    VALUE 'EL631'.           EL626   
00073      12  XCTL-632            PIC X(8)    VALUE 'EL632'.           EL626   
00074      12  XCTL-633            PIC X(8)    VALUE 'EL633'.           EL626   
00075      12  XCTL-633DMD         PIC X(8)    VALUE 'EL633DMD'.           CL*24
00076      12  XCTL-634            PIC X(8)    VALUE 'EL634'.           EL626   
00077      12  XCTL-635            PIC X(8)    VALUE 'EL635'.              CL**5
00078      12  XCTL-640            PIC X(8)    VALUE 'EL640'.           EL626   
00079      12  XCTL-642            PIC X(8)    VALUE 'EL642'.           EL626   
00080      12  XCTL-645            PIC X(8)    VALUE 'EL645'.              CL*11
00081      12  XCTL-PC1            PIC X(8)    VALUE 'PC100'.              CL**4
00082      12  XCTL-SHOT           PIC X(8)    VALUE 'CC00900Z'.           CL*22
00083      12  XCTL-IN1            PIC X(8)    VALUE 'IN001'.              CL*19
00084      12  XCTL-671            PIC X(8)    VALUE 'EL671'.           EL626   
00085      12  XCTL-677            PIC X(8)    VALUE 'EL677'.           EL626   
00086      12  XCTL-680            PIC X(8)    VALUE 'EL680'.           EL626   
00087      12  XCTL-689            PIC X(8)    VALUE 'EL689'.           EL626   
00088      12  XCTL-690            PIC X(8)    VALUE 'EL690'.              CL*22
00089      12  XCTL-700            PIC X(8)    VALUE 'EL700'.              CL**5
00090      12  XCTL-850            PIC X(8)    VALUE 'EL850'.              CL**5
00091      12  XCTL-930            PIC X(8)    VALUE 'EL930'.           EL626   
00092      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.            CL*19
00093      12  SV-TODAY            PIC XX.                              EL626   
00094      12  WS-CURRENT-DATE     PIC X(8).                            EL626   
00095      12  WS-SYSID            PIC X(4).                               CL**3
00096      12  WS-OLD-COMPANY-ID   PIC X(3)    VALUE SPACES.               CL*18
00097                                                                   EL626   
00098      EJECT                                                        EL626   
00099      12  ER-0002                 PIC X(4)  VALUE '0002'.          EL626   
011812     12  ER-0007                 PIC X(4)  VALUE '0007'.
00100      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL626   
00101      12  ER-0021                 PIC X(4)  VALUE '0021'.          EL626   
00102      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL626   
00103      12  ER-0035                 PIC X(4)  VALUE '0035'.             CL**7
00104      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL626   
00105      12  ER-2371                 PIC X(4)  VALUE '2371'.          EL626   
00106      12  ER-2372                 PIC X(4)  VALUE '2372'.          EL626   
00107      12  ER-2537                 PIC X(4)  VALUE '2537'.             CL**3
00108      12  ER-2560                 PIC X(4)  VALUE '2560'.          EL626   
00109      12  ER-2561                 PIC X(4)  VALUE '2561'.          EL626   
00110      12  ER-2921                 PIC X(4)  VALUE '2921'.          EL626   
00111      12  ER-2929                 PIC X(4)  VALUE '2929'.             CL**4
00112      12  ER-2930                 PIC X(4)  VALUE '2930'.             CL**5
00113      12  ER-9303                 PIC X(4)  VALUE '9303'.             CL**6
00114                                                                   EL626   
00115      EJECT                                                        EL626   
00116                                                                      CL*16
00117  01  MESG-COMM-AREA          PIC X(40).                              CL*16
00118                                                                   EL626   
00119  01  BATCH-TO-PROCESS.                                            EL626   
00120      05  EDIT-COMPANY-CD         PIC X.                           EL626   
00121      05  EDIT-BATCH              PIC X(6).                        EL626   
00122      05  EDIT-COMPANY-ID         PIC XXX.                         EL626   
00123      05  EDIT-RESTART-BATCH      PIC X(6).                        EL626   
00124                                                                   EL626   
00125  01  MISC-COMP.                                                   EL626   
00126      12  WS-IC               PIC S9(4)   VALUE -1  COMP.          EL626   
00127      12  SUB1                PIC S9(4)   VALUE +0  COMP.          EL626   
00128      12  SUB2                PIC S9(4)   VALUE +0  COMP.          EL626   
00129                                                                   EL626   
00130  01  ACCESS-KEYS.                                                 EL626   
00131      12  ELCNTL-KEY.                                              EL626   
00132          16  CK-COMP-ID      PIC X(3).                            EL626   
00133          16  CK-REC-TYPE     PIC X.                                  CL*18
00134          16  CK-PROC-ID      PIC X(4).                            EL626   
00135          16  FILLER          PIC S9(4)   VALUE +0  COMP.          EL626   
00136                                                                   EL626   
00137      EJECT                                                        EL626   
00138                              COPY ELCDATE.                           CL*17
00139                                                                   EL626   
00140      EJECT                                                        EL626   
00141                              COPY ELCLOGOF.                          CL*17
00142                                                                   EL626   
00143      EJECT                                                        EL626   
00144                              COPY ELCATTR.                           CL*17
00145                                                                   EL626   
00146      EJECT                                                        EL626   
00147                              COPY ELCEMIB.                           CL*17
00148                                                                   EL626   
00149      EJECT                                                        EL626   
00150                              COPY ELCINTF.                           CL*17
00151      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                      CL*19
00152          16  PI-IM-SYSTEM-ID       PIC XX.                           CL*19
00153          16  PI-IM-CONTROL-ACCESS  PIC X.                            CL*19
00154          16  FILLER                PIC X(637).                       CL*26
00155                                                                      CL*19
00156      EJECT                                                        EL626   
00157                              COPY ELCAID.                            CL*17
00158                                                                   EL626   
00159  01  FILLER    REDEFINES DFHAID.                                  EL626   
00160      12  FILLER              PIC X(8).                            EL626   
00161      12  PF-VALUES           PIC X       OCCURS 2.                EL626   
00162                                                                   EL626   
00163      EJECT                                                        EL626   
00164 *01  MAIN-MAP                PIC X(850).                             CL*15
00165 *01  EL626AI REDEFINES MAIN-MAP  COPY EL626S.                        CL*15
00166                                  COPY EL626S.                        CL*17
00167  01  EL626BOR     REDEFINES EL626BI.                                 CL*15
00168      12  FILLER              PIC X(31).                           EL626   
00169      12  FILLER                 OCCURS 8.                            CL**2
00170          16  FILLER          PIC X(3).                            EL626   
00171          16  DATE-OUT        PIC X(8).                            EL626   
00172          16  FILLER          PIC X(3).                            EL626   
00173          16  MSG-OUT         PIC X(60).                           EL626   
00174      12  FILLER              PIC X(73).                           EL626   
00175                                                                   EL626   
00176      EJECT                                                        EL626   
00177  LINKAGE SECTION.                                                 EL626   
00178  01  DFHCOMMAREA             PIC X(1024).                         EL626   
00179                                                                   EL626   
00180 *01 PARMLIST .                                                       CL*24
00181 *    02  FILLER              PIC S9(8)   COMP.                       CL*24
00182 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL*24
00183      EJECT                                                        EL626   
00184                              COPY ELCCNTL.                           CL*17
00185                                                                   EL626   
00186      EJECT                                                        EL626   
00187                                                                      CL*22
00188  PROCEDURE DIVISION.                                              EL626   
00189                                                                   EL626   
00190      MOVE DFHCOMMAREA         TO PROGRAM-INTERFACE-BLOCK.         EL626   
00191                                                                   EL626   
00192      IF EIBCALEN = 0                                              EL626   
00193         GO TO 8800-UNAUTHORIZED-ACCESS.                           EL626   
00194                                                                   EL626   
00195      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL626   
00196      MOVE '5'                    TO DC-OPTION-CODE.               EL626   
00197      MOVE LINK-ELDATCV           TO PGM-NAME.                        CL*19
00198                                                                   EL626   
00199      EXEC CICS LINK                                               EL626   
00200          PROGRAM  (PGM-NAME)                                      EL626   
00201          COMMAREA (DATE-CONVERSION-DATA)                          EL626   
00202          LENGTH   (DC-COMM-LENGTH)                                EL626   
00203      END-EXEC.                                                    EL626   
00204                                                                   EL626   
00205      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DATE.              EL626   
00206      MOVE DC-BIN-DATE-1          TO SV-TODAY.                     EL626   
00207                                                                   EL626   
00208      EXEC CICS HANDLE CONDITION                                   EL626   
00209          NOTOPEN  (8820-NOT-OPEN)                                 EL626   
00210          NOTFND   (8830-NOT-FOUND)                                EL626   
00211          PGMIDERR (9700-PGMID-ERROR)                                 CL*22
00212          ERROR    (9990-ABEND)                                    EL626   
00213      END-EXEC.                                                    EL626   
00214                                                                   EL626   
00215      IF EIBTRNID = 'EXCR'                                         EL626   
00216          GO TO 4000-BUILD-NOTE-SCREEN.                            EL626   
00217                                                                   EL626   
CIDMOD     IF EIBAID = DFHCLEAR                                                 
CIDMOD        GO TO 8100-SEND-INITIAL-MAP                                       
CIDMOD     END-IF                                                               
CIDMOD                                                                          
00218      IF (EIBTRNID NOT = TRANS-ID) OR (EIBCALEN = 0)               EL626   
00219          GO TO 8100-SEND-INITIAL-MAP.                             EL626   
00220                                                                   EL626   
00221      IF PI-CALLING-PROGRAM = XCTL-125                             EL626   
00222          GO TO 8100-SEND-INITIAL-MAP.                             EL626   
00223                                                                   EL626   
CIDMOD*    IF EIBAID = DFHCLEAR                                         EL626   
CIDMOD     IF EIBAID = DFHPF24                                          EL626   
00225          GO TO 9600-CLEAR.                                           CL*22
00226                                                                   EL626   
00227      IF PI-PROCESSOR-ID = 'LGXX'                                  EL626   
00228          GO TO 0200-RECEIVE.                                      EL626   
00229                                                                   EL626   
00230      EXEC CICS READQ TS                                           EL626   
00231          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       EL626   
00232          INTO   (SECURITY-CONTROL)                                EL626   
00233          LENGTH (SC-COMM-LENGTH)                                  EL626   
00234          ITEM   (SC-ITEM)                                         EL626   
00235      END-EXEC.                                                    EL626   
00236                                                                   EL626   
00237      MOVE SC-CREDIT-DISPLAY (13)  TO PI-DISPLAY-CAP.              EL626   
00238      MOVE SC-CREDIT-UPDATE  (13)  TO PI-MODIFY-CAP.               EL626   
00239                                                                   EL626   
00240      EJECT                                                        EL626   
00241                                                                   EL626   
00242  0200-RECEIVE.                                                    EL626   
00243      MOVE LOW-VALUES             TO EL626BI.                         CL*15
00244                                                                   EL626   
00245      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL626   
00246          MOVE ER-0008            TO EMI-ERROR                     EL626   
00247          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL626   
00248          MOVE -1                 TO SELECTL                       EL626   
00249          GO TO 8200-SEND-DATAONLY.                                EL626   
00250                                                                   EL626   
00251      EXEC CICS RECEIVE                                            EL626   
00252          MAP    (MAP-NAMEA)                                       EL626   
00253          MAPSET (MAPSET-NAME)                                     EL626   
00254          INTO   (EL626BI)                                            CL*15
00255      END-EXEC.                                                    EL626   
00256                                                                   EL626   
00257      IF EIBAID = DFHPF12                                             CL*22
00258         GO TO 9100-HELP.                                             CL*22
00259                                                                      CL*22
00260      IF EIBAID = DFHPF23                                             CL*22
00261         GO TO 9200-PF23.                                             CL*22
00262                                                                      CL*22
00263 *    IF EIBAID = DFHPF24                                             CL*22
00264 *       GO TO 9300-PF24.                                             CL*22
00265                                                                      CL*22
00266      IF PFKEYL = ZEROS                                               CL*22
00267         NEXT SENTENCE                                                CL*22
00268      ELSE                                                            CL*22
00269         IF PFKEYI = '12'                                             CL*22
00270            GO TO 9100-HELP                                           CL*22
00271         ELSE                                                         CL*22
00272            IF PFKEYI = '23'                                          CL*22
00273               GO TO 9200-PF23.                                       CL*22
00274 *           ELSE                                                     CL*22
00275 *              IF PFKEYI = '24'                                      CL*22
00276 *                 GO TO 9300-PF24.                                   CL*22
00277                                                                      CL*22
00278      IF NEWIDI = PI-COMPANY-ID                                    EL626   
00279          MOVE ZEROS TO NEWIDL.                                    EL626   
00280                                                                   EL626   
00281      IF NEWIDL NOT = ZEROS                                        EL626   
00282          MOVE 'CR'               TO PI-NEW-SYSTEM                 EL626   
00283          MOVE PI-COMPANY-ID      TO WS-OLD-COMPANY-ID                CL*18
00284          MOVE NEWIDI             TO PI-COMPANY-ID.                EL626   
00285                                                                   EL626   
00286      IF NEWPWDL NOT = ZEROS                                       EL626   
00287          IF NEWIDL NOT = ZEROS                                       CL*18
00288              MOVE NEWPWDI        TO PI-COMPANY-PASSWORD              CL*18
00289          ELSE                                                        CL*18
00290              MOVE 'CR'           TO PI-NEW-SYSTEM                    CL*18
00291              MOVE PI-COMPANY-ID  TO WS-OLD-COMPANY-ID                CL*18
00292              MOVE NEWPWDL        TO PI-COMPANY-PASSWORD.             CL*18
00293                                                                   EL626   
00294      IF NEWCDL NOT = ZEROS                                           CL*27
00295          IF NEWCDI NOT = 'CV' AND 'CR' AND 'CL'                      CL*27
00296              MOVE ER-0035        TO EMI-ERROR                        CL**7
00297              MOVE -1             TO NEWCDL                           CL*18
00298              MOVE AL-UNBON       TO NEWCDA                           CL*18
00299              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**6
00300              GO TO 8200-SEND-DATAONLY.                               CL**6
00301                                                                      CL*18
00302      IF NEWCDL NOT = ZEROS                                        EL626   
00303          IF (NEWIDL = ZEROS AND NEWPWDL = ZEROS)                     CL*18
00304              MOVE PI-COMPANY-ID  TO WS-OLD-COMPANY-ID                CL*18
00305              MOVE NEWCDI         TO PI-NEW-SYSTEM                    CL*18
00306          ELSE                                                        CL*18
00307              MOVE NEWCDI         TO PI-NEW-SYSTEM.                   CL*18
00308                                                                   EL626   
00309      IF NEWIDL  = ZEROS  AND                                      EL626   
00310         NEWCDL  = ZEROS  AND                                      EL626   
00311         NEWPWDL = ZEROS                                           EL626   
00312          NEXT SENTENCE                                            EL626   
00313      ELSE                                                         EL626   
00314          IF PI-PROCESSOR-ID = 'LGXX'                                 CL*27
00315              MOVE XCTL-125           TO  PGM-NAME                    CL*18
00316              GO TO 9800-XCTL                                         CL*22
00317          ELSE                                                        CL*18
00318              MOVE PI-PROCESSOR-ID    TO CK-PROC-ID                   CL*18
00319              MOVE WS-OLD-COMPANY-ID  TO CK-COMP-ID                   CL*18
00320              MOVE '2'                TO CK-REC-TYPE                  CL*18
00321              PERFORM 7000-READ-CNTL-UPDATE THRU 7000-EXIT            CL*18
00322              MOVE SPACES             TO CF-CURRENT-TERM-ON           CL*18
00323              PERFORM 7010-REWRITE-CNTL THRU 7010-EXIT                CL*18
00324              MOVE XCTL-125           TO PGM-NAME                     CL*18
00325              GO TO 9800-XCTL.                                        CL*22
00326                                                                   EL626   
00327      IF EIBAID NOT = DFHENTER                                     EL626   
00328          MOVE ER-0002            TO EMI-ERROR                     EL626   
00329          GO TO 0320-INPUT-ERROR.                                  EL626   
00330                                                                   EL626   
00331      MOVE SPACES                 TO PI-ENTRY-CD-1.                EL626   
00332                                                                   EL626   
00333      IF PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY            CL*27
00334          GO TO 310-CHECK-SELECTION.                               EL626   
00335                                                                   EL626   
00336      IF SELECTI = '10' OR '11' OR '13'                               CL*27
00337          IF PI-CARRIER-SECURITY > SPACES                             CL*27
00338             IF PI-NO-ACCOUNT-SECURITY                             EL626   
00339                GO TO 310-CHECK-SELECTION.                         EL626   
00340                                                                   EL626   
00341      IF SELECTI = '10' OR '11' OR '12' OR '13'                       CL*27
00342          IF PI-ACCOUNT-SECURITY > SPACES                             CL*27
00343             MOVE ER-2371        TO EMI-ERROR                      EL626   
00344             MOVE -1             TO SELECTL                        EL626   
00345             MOVE AL-UNBON       TO SELECTA                        EL626   
00346             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL626   
00347             GO TO 8200-SEND-DATAONLY.                             EL626   
00348                                                                   EL626   
00349      IF SELECTI = '02' OR '08'                                       CL*27
00350          NEXT SENTENCE                                               CL*27
00351         ELSE                                                         CL**6
00352          GO TO 310-CHECK-SELECTION.                               EL626   
00353                                                                   EL626   
00354      MOVE ER-2372               TO EMI-ERROR.                     EL626   
00355      MOVE -1                    TO SELECTL.                       EL626   
00356      MOVE AL-UNBON              TO SELECTA.                       EL626   
00357      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL626   
00358      GO TO 8200-SEND-DATAONLY.                                    EL626   
00359                                                                   EL626   
00360  310-CHECK-SELECTION.                                             EL626   
00361      IF SELECTI = '01'                                            EL626   
00362        AND PI-MODIFY-CAP NOT = 'C'                                EL626   
00363          MOVE XCTL-601 TO PGM-NAME                                EL626   
00364          GO TO 9800-XCTL.                                            CL*22
00365                                                                   EL626   
00366      IF SELECTI = '02'                                            EL626   
00367        AND PI-MODIFY-CAP NOT = 'C'                                EL626   
00368          MOVE XCTL-671           TO PGM-NAME                      EL626   
00369          GO TO 9800-XCTL.                                            CL*22
00370                                                                   EL626   
00371      IF SELECTI = '03'                                               CL**5
00372          IF PI-AR-PROCESSING                                         CL**5
00373              MOVE XCTL-850       TO PGM-NAME                         CL**5
00374              GO TO 9800-XCTL                                         CL*22
00375          ELSE                                                        CL**5
00376              MOVE -1             TO SELECTL                          CL**5
00377              MOVE ER-2930        TO EMI-ERROR                        CL**5
00378              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**5
00379              GO TO 8200-SEND-DATAONLY.                               CL**5
00380                                                                      CL**5
00381      IF SELECTI = '04'                                               CL**5
00382          IF PI-HAS-CLAS-IC-CRDTCRD                                   CL**6
00383           MOVE XCTL-700           TO PGM-NAME                        CL**6
00384           GO TO 9800-XCTL.                                           CL*22
00385                                                                      CL*19
00386 *    IF SELECTI = '05'                                               CL*27
00387 *        MOVE XCTL-IN1               TO PGM-NAME                     CL*27
00388 *        GO TO 9800-XCTL.                                            CL*27
00389                                                                      CL**5
00390      IF SELECTI = '06'                                            EL626   
00391        AND PI-MODIFY-CAP NOT = 'C'                                EL626   
00392          MOVE XCTL-630           TO PGM-NAME                      EL626   
00393          GO TO 9800-XCTL.                                            CL*22
00394                                                                   EL626   
00395      IF SELECTI = '07'
00396        AND PI-MODIFY-CAP NOT = 'C'                                EL626   
               MOVE ' '                TO PI-ENTRY-CD-1
00397          MOVE XCTL-631           TO PGM-NAME                      EL626   
00398          GO TO 9800-XCTL.                                            CL*22
00399                                                                   EL626   
00395      IF SELECTI = '15'
011812       IF PI-MODIFY-CAP NOT = 'C'
011812       AND PI-PROCESSOR-IS-CSR
               MOVE 'Y'                TO PI-ENTRY-CD-1
00397          MOVE XCTL-631           TO PGM-NAME                      EL626   
011812         GO TO 9800-XCTL
011812       ELSE
011812         MOVE ER-0007    TO EMI-ERROR
011812         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
011812         MOVE -1         TO SELECTL
011812         GO TO 0320-INPUT-ERROR
011812       END-IF
011812     END-IF.
00399                                                                   EL626   
00400      IF BATCHL NOT = ZEROS                                        EL626   
00401         MOVE BATCHI              TO EDIT-RESTART-BATCH            EL626   
00402      ELSE                                                         EL626   
00403         MOVE SPACES              TO EDIT-RESTART-BATCH.           EL626   
00404                                                                      CL**3
00405      EXEC CICS ASSIGN                                                CL**3
00406          SYSID  (WS-SYSID)                                           CL**3
00407      END-EXEC.                                                       CL**3
00408                                                                      CL**3
00409      IF SELECTI = '08'                                               CL**5
CIDMOD         IF (WS-SYSID = 'LGCR') OR                                   CL*10
062121             (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
00411              IF PI-PROCESSOR-ID = 'LGXX' OR 'PEMA'                   CL**5
00412                  NEXT SENTENCE                                       CL**3
00413              ELSE                                                    CL**3
00414                  MOVE ER-2537    TO EMI-ERROR                        CL**3
00415                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**3
00416                  MOVE -1         TO SELECTL                          CL**3
00417                  GO TO 0320-INPUT-ERROR.                             CL**3
00418                                                                   EL626   
00419      IF SELECTI = '08'                                            EL626   
00420          IF MODIFY-CAP                                            EL626   
00421              MOVE PI-COMPANY-CD  TO EDIT-COMPANY-CD               EL626   
00422              MOVE PI-COMPANY-ID  TO EDIT-COMPANY-ID               EL626   
00423              MOVE SPACES         TO EDIT-BATCH                    EL626   
00424              EXEC CICS START                                      EL626   
00425                 TRANSID       (EDIT-TRANS)                        EL626   
00426                 FROM          (BATCH-TO-PROCESS)                  EL626   
00427                 LENGTH        (PASS-AREA-LEN)                     EL626   
00428              END-EXEC                                             EL626   
00429              MOVE ER-2560        TO EMI-ERROR                     EL626   
00430              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL626   
00431              MOVE -1             TO SELECTL                       EL626   
00432              MOVE SPACES         TO SELECTO                       EL626   
00433              GO TO 8200-SEND-DATAONLY                             EL626   
00434          ELSE                                                     EL626   
00435              MOVE 'UPDATE'       TO SM-READ                       EL626   
00436              PERFORM 9995-SECURITY-VIOLATION                      EL626   
00437              MOVE ER-0070        TO EMI-ERROR                     EL626   
00438              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL626   
00439              GO TO 0320-INPUT-ERROR.                              EL626   
00440                                                                   EL626   
00441      IF SELECTI = '09'                                            EL626   
00442          MOVE XCTL-632           TO PGM-NAME                      EL626   
00443          GO TO 9800-XCTL.                                            CL*22
00444                                                                   EL626   
00445      IF SELECTI = '10'                                            EL626   
00446          IF PI-AR-PROCESSING                                         CL**5
00447             MOVE XCTL-635        TO PGM-NAME                         CL**5
00448             GO TO 9800-XCTL                                          CL*22
00449          ELSE                                                        CL**5
00450             IF PI-COMPANY-ID = 'DMD'                                 CL*24
00451                MOVE XCTL-633DMD  TO PGM-NAME                         CL*24
00452                GO TO 9800-XCTL                                       CL*24
00453             ELSE                                                     CL*24
00454                MOVE XCTL-633     TO PGM-NAME                         CL*24
00455                GO TO 9800-XCTL.                                      CL*24
00456                                                                   EL626   
00457      IF SELECTI = '11'                                            EL626   
00458          MOVE XCTL-634           TO PGM-NAME                      EL626   
00459          GO TO 9800-XCTL.                                            CL*22
00460                                                                   EL626   
00461      IF SELECTI = '12'                                            EL626   
00462          MOVE XCTL-677           TO PGM-NAME                      EL626   
00463          GO TO 9800-XCTL.                                            CL*22
00464                                                                   EL626   
00465      IF SELECTI = '13'                                            EL626   
00466          MOVE XCTL-640           TO PGM-NAME                      EL626   
00467          GO TO 9800-XCTL.                                            CL*22
00468                                                                   EL626   
00469      IF SELECTI = '14'                                            EL626   
00470          IF PI-GA-BILLING                                         EL626   
00471             MOVE XCTL-642           TO PGM-NAME                   EL626   
00472             GO TO 9800-XCTL.                                         CL*22
00473                                                                   EL626   
00474      IF SELECTI = '14'                                            EL626   
00475         MOVE -1                  TO SELECTL                       EL626   
00476         MOVE ER-2929             TO EMI-ERROR                     EL626   
00477         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL626   
00478         GO TO 8200-SEND-DATAONLY.                                 EL626   
00479                                                                   EL626   
00480 *    IF SELECTI = '15'                                               CL*27
00481 *        MOVE XCTL-930           TO PGM-NAME                         CL*27
00482 *        GO TO 9800-XCTL.                                            CL*27
00483                                                                   EL626   
00484      IF SELECTI = '21'                                            EL626   
00485          MOVE XCTL-127           TO PGM-NAME                      EL626   
00486          GO TO 9800-XCTL.                                            CL*22
00487                                                                   EL626   
00488      IF SELECTI = '22'                                            EL626   
00489          MOVE XCTL-680           TO PGM-NAME                      EL626   
00490          GO TO 9800-XCTL.                                            CL*22
00491                                                                   EL626   
00492      IF SELECTI = '23'                                            EL626   
00493          MOVE XCTL-179           TO PGM-NAME                      EL626   
00494          GO TO 9800-XCTL.                                            CL*22
00495                                                                   EL626   
00496      IF SELECTI = '24'                                               CL*11
00497          IF PI-PROCESSOR-ID = 'LGXX'                                 CL*27
00498              MOVE XCTL-645               TO  PGM-NAME                CL*17
00499              GO TO 9800-XCTL                                         CL*22
00500          ELSE                                                        CL*17
00501              MOVE SC-CREDIT-DISPLAY (20) TO  PI-DISPLAY-CAP          CL*17
00502              MOVE SC-CREDIT-UPDATE  (20) TO  PI-MODIFY-CAP           CL*26
00503              IF NOT DISPLAY-CAP                                      CL*17
00504                  MOVE XCTL-645           TO  THIS-PGM                CL*17
00505                  MOVE 'READ'             TO  SM-READ                 CL*17
00506                  PERFORM 9995-SECURITY-VIOLATION                     CL*17
00507                  MOVE ER-0070            TO  EMI-ERROR               CL*17
00508                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*17
00509                  MOVE AL-UNBON           TO SELECTA                  CL*17
00510                  MOVE -1                 TO SELECTL                  CL*17
00511                  GO TO 8200-SEND-DATAONLY                            CL*17
00512              ELSE                                                    CL*17
00513                  MOVE XCTL-645           TO  PGM-NAME                CL*17
00514                  GO TO 9800-XCTL.                                    CL*22
00515                                                                      CL*11
PEMTST     IF SELECTI = '25'                                            EL626   
062121        IF PI-COMPANY-ID = 'CID' OR 'CSI' OR 'AHL' or 'FNL'                        
PEMTST           MOVE XCTL-128         TO PGM-NAME                      EL626   
PEMTST           GO TO 9800-XCTL                                           CL*22
PEMTST        END-IF                                                            
PEMTST     END-IF                                                               
PEMTST                                                                  EL626   
00516      IF SELECTI = '30'                                            EL626   
00517          MOVE XCTL-109           TO PGM-NAME                      EL626   
00518          GO TO 9800-XCTL.                                            CL*22
00519                                                                   EL626   
00520      IF SELECTI = '31'                                            EL626   
00521          MOVE XCTL-155           TO PGM-NAME                      EL626   
00522          GO TO 9800-XCTL.                                            CL*22
00523                                                                   EL626   
00524      IF SELECTI = '32'                                            EL626   
00525          MOVE XCTL-689           TO PGM-NAME                      EL626   
00526          GO TO 9800-XCTL.                                            CL*22
00527                                                                   EL626   
00528      IF SELECTI = '33'                                               CL**4
00529          MOVE XCTL-690           TO PGM-NAME                         CL*22
00530          GO TO 9800-XCTL.                                            CL*22
00531                                                                      CL*22
00532      IF SELECTI = '34'                                               CL*22
00533          MOVE XCTL-PC1           TO PGM-NAME                         CL**4
00534          GO TO 9800-XCTL.                                            CL*22
00535                                                                      CL*22
00536      IF SELECTI = '35'                                               CL*22
00537        IF PI-COMPANY-ID = 'NCL' OR 'LGX'                             CL*22
00538          MOVE XCTL-SHOT          TO PGM-NAME                         CL*22
00539          GO TO 9800-XCTL.                                            CL*22
00540                                                                      CL**4
00541      MOVE ER-0029                TO EMI-ERROR.                    EL626   
00542                                                                   EL626   
00543  0320-INPUT-ERROR.                                                EL626   
00544      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL626   
00545      MOVE AL-UNBON               TO SELECTA.                      EL626   
00546      MOVE -1                     TO SELECTL.                      EL626   
00547      GO TO 8200-SEND-DATAONLY.                                    EL626   
00548                                                                   EL626   
00549      EJECT                                                        EL626   
00550                                                                   EL626   
00551  4000-BUILD-NOTE-SCREEN.                                          EL626   
00552      IF NOT MSG-AT-LOGON-CAP                                      EL626   
00553          GO TO 8100-SEND-INITIAL-MAP.                             EL626   
00554                                                                   EL626   
00555      IF PI-PROCESSOR-ID = 'LGXX'                                  EL626   
00556          GO TO 8100-SEND-INITIAL-MAP.                             EL626   
00557                                                                   EL626   
00558      MOVE PI-COMPANY-ID          TO CK-COMP-ID.                   EL626   
00559      MOVE PI-PROCESSOR-ID        TO CK-PROC-ID.                   EL626   
00560      MOVE 'R'                    TO CK-REC-TYPE.                     CL*18
00561                                                                   EL626   
00562      EXEC CICS READ                                               EL626   
00563          DATASET ('ELCNTL')                                       EL626   
00564          SET     (ADDRESS OF CONTROL-FILE)                           CL*24
00565          RIDFLD  (ELCNTL-KEY)                                     EL626   
00566      END-EXEC.                                                    EL626   
00567                                                                   EL626   
00568      MOVE LOW-VALUES             TO EL626BO.                      EL626   
00569      MOVE +1                     TO SUB2.                         EL626   
00570                                                                   EL626   
00571      PERFORM 4050-LOAD-MESSAGES THRU 4050-EXIT                    EL626   
00572          VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 GREATER 8.              CL**2
00573                                                                   EL626   
00574      IF SUB2 = +1                                                 EL626   
00575          GO TO 8100-SEND-INITIAL-MAP.                             EL626   
00576                                                                   EL626   
00577      GO TO 8000-SEND-REMINDER-SCREEN.                             EL626   
00578                                                                   EL626   
00579  4050-LOAD-MESSAGES.                                              EL626   
00580      IF CF-START-REMIND-DT (SUB1) GREATER SV-TODAY                EL626   
00581          GO TO 4050-EXIT.                                         EL626   
00582                                                                   EL626   
00583      IF CF-END-REMIND-DT (SUB1) LESS SV-TODAY                     EL626   
00584          GO TO 4050-EXIT.                                         EL626   
00585                                                                   EL626   
00586      MOVE CF-END-REMIND-DT (SUB1) TO DC-BIN-DATE-1.               EL626   
00587      MOVE ' '                     TO DC-OPTION-CODE.              EL626   
00588      MOVE LINK-ELDATCV            TO PGM-NAME.                       CL*19
00589                                                                   EL626   
00590      EXEC CICS LINK                                               EL626   
00591          PROGRAM  (PGM-NAME)                                      EL626   
00592          COMMAREA (DATE-CONVERSION-DATA)                          EL626   
00593          LENGTH   (DC-COMM-LENGTH)                                EL626   
00594      END-EXEC.                                                    EL626   
00595                                                                   EL626   
00596      IF DATE-CONVERSION-ERROR                                     EL626   
00597          MOVE ER-0021            TO EMI-ERROR                     EL626   
00598          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL626   
00599          MOVE +7                 TO SUB1                          EL626   
00600          MOVE +7                 TO SUB2                          EL626   
00601          GO TO 4050-EXIT.                                         EL626   
00602                                                                   EL626   
00603      MOVE DC-GREG-DATE-1-EDIT     TO DATE-OUT (SUB2).             EL626   
00604      MOVE CF-REMINDER-TEXT (SUB1) TO MSG-OUT (SUB2).              EL626   
00605      ADD +1 TO SUB2.                                              EL626   
00606                                                                   EL626   
00607  4050-EXIT.                                                       EL626   
00608      EXIT.                                                           CL*18
00609                                                                      CL*18
00610      EJECT                                                           CL*18
00611  7000-READ-CNTL-UPDATE.                                              CL*18
00612                                                                      CL*18
00613      EXEC CICS READ                                                  CL*18
00614          DATASET   ('ELCNTL')                                        CL*18
00615          RIDFLD    (ELCNTL-KEY)                                      CL*18
00616          SET       (ADDRESS OF CONTROL-FILE)                         CL*24
00617          UPDATE                                                      CL*18
00618      END-EXEC.                                                       CL*18
00619                                                                      CL*18
00620  7000-EXIT.                                                          CL*18
00621      EXIT.                                                           CL*18
00622                                                                      CL*18
00623      EJECT                                                           CL*18
00624  7010-REWRITE-CNTL.                                                  CL*18
00625                                                                      CL*18
00626      EXEC CICS REWRITE                                               CL*18
00627          DATASET   ('ELCNTL')                                        CL*18
00628          FROM      (CONTROL-FILE)                                    CL*18
00629      END-EXEC.                                                       CL*18
00630                                                                      CL*18
00631  7010-EXIT.                                                          CL*18
00632      EXIT.                                                        EL626   
00633                                                                   EL626   
00634      EJECT                                                        EL626   
00635  8000-SEND-REMINDER-SCREEN.                                       EL626   
00636      MOVE EIBTIME                TO TIME-IN.                      EL626   
00637      MOVE TIME-OUT               TO RUNTIMBO.                     EL626   
00638      MOVE WS-CURRENT-DATE        TO RUNDTEBO.                     EL626   
00639      MOVE -1                     TO DTE1L.                        EL626   
00640                                                                   EL626   
00641      IF NOT EMI-NO-ERRORS                                         EL626   
00642          SET EMI-INDX TO 1                                        EL626   
00643          MOVE EMI-MESSAGE-AREA (EMI-INDX) TO ERRMSGBO.            EL626   
00644                                                                   EL626   
00645      EXEC CICS SEND                                               EL626   
00646          MAP    (MAP-NAMEB)                                       EL626   
00647          MAPSET (MAPSET-NAME)                                     EL626   
00648          FROM   (EL626BO)                                         EL626   
00649          ERASE                                                    EL626   
00650          CURSOR                                                   EL626   
00651      END-EXEC.                                                    EL626   
00652                                                                   EL626   
00653      GO TO 9500-RETURN-TRAN.                                         CL*22
00654                                                                   EL626   
00655      EJECT                                                        EL626   
00656                                                                   EL626   
00657  8100-SEND-INITIAL-MAP.                                           EL626   
00658      MOVE LOW-VALUES             TO EL626AO.                      EL626   
00659                                                                   EL626   
00660 *    IF PI-COMPANY-ID = 'PEM' OR 'MIC' OR 'RGT'                      CL*14
00661 *       MOVE AL-SANOF            TO OPTNBHDA.                     EL626   
00662                                                                   EL626   
00663      IF PI-NOT-CRDTCRD-USER                                          CL**6
00664          MOVE SPACES             TO OPENENDO.                        CL**6
00665                                                                      CL**6
00666      IF PI-COMPANY-ID NOT = 'NCL' AND 'LGX'                          CL*22
00667          MOVE SPACES             TO SHOTO.                           CL*22
00668                                                                      CL**5
062121     IF PI-COMPANY-ID = 'CID' OR 'CSI' OR 'AHL' OR 'FNL'                      
PEMTST        CONTINUE                                                          
PEMTST     ELSE                                                                 
PEMTST        MOVE AL-SADOF            TO OPT25A                                
PEMTST     END-IF                                                               
PEMTST                                                                          
00669      IF PI-AR-PROCESSING                                             CL**5
00670         MOVE AL-SANOF            TO ARSYSTA                          CL**5
00671      ELSE                                                            CL**5
00672         MOVE AL-SADOF            TO ARSYSTA.                         CL**5
00673                                                                      CL**4
00674      IF PI-COMPANY-ID = 'HER' OR 'TII' OR 'HSL' OR 'LGX'             CL*27
00675         MOVE AL-SANOF            TO POLINVA.                         CL*19
00676                                                                      CL*19
00677      MOVE PI-COMPANY-ID          TO NEWIDO.                       EL626   
00678      MOVE EIBTIME                TO TIME-IN.                      EL626   
00679      MOVE TIME-OUT               TO TIMEO.                        EL626   
00680      MOVE WS-CURRENT-DATE        TO DATEO.                        EL626   
00681      MOVE -1                     TO SELECTL.                      EL626   
00682                                                                      CL*25
00683      MOVE SPACES                 TO MESG-COMM-AREA.                  CL*25
00684                                                                   EL626   
00685      PERFORM 8900-CHECK-MAIL  THRU  8900-EXIT.                       CL*16
00686                                                                      CL*16
00687      MOVE MESG-COMM-AREA         TO ERRMSGAO.                        CL*16
00688                                                                      CL*16
00689      EXEC CICS SEND                                               EL626   
00690          MAP    (MAP-NAMEA)                                       EL626   
00691          MAPSET (MAPSET-NAME)                                     EL626   
00692          FROM   (EL626AO)                                         EL626   
00693          ERASE                                                    EL626   
00694          CURSOR                                                   EL626   
00695      END-EXEC.                                                    EL626   
00696                                                                   EL626   
00697      MOVE 'EL626'                TO PI-CALLING-PROGRAM.           EL626   
00698                                                                   EL626   
00699      GO TO 9500-RETURN-TRAN.                                         CL*22
00700                                                                   EL626   
00701  8200-SEND-DATAONLY.                                              EL626   
00702      MOVE PI-COMPANY-ID          TO NEWIDO.                       EL626   
00703                                                                   EL626   
00704      MOVE EIBTIME                TO TIME-IN.                      EL626   
00705      MOVE TIME-OUT               TO TIMEO.                        EL626   
00706      MOVE WS-CURRENT-DATE        TO DATEO.                        EL626   
00707      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGAO.                     EL626   
00708                                                                      CL*27
062121     IF PI-COMPANY-ID = 'CID' OR 'CSI' OR 'AHL' OR 'FNL'                          
PEMTST        CONTINUE                                                          
PEMTST     ELSE                                                                 
PEMTST        MOVE AL-SADOF            TO OPT25A                                
PEMTST     END-IF                                                               
PEMTST                                                                          
00709      IF PI-COMPANY-ID = 'HER' OR 'TII' OR 'LGX'                      CL*27
00710         MOVE AL-SANOF            TO POLINVA.                         CL*19
00711                                                                   EL626   
00712      EXEC CICS SEND                                               EL626   
00713          MAP    (MAP-NAMEA)                                       EL626   
00714          MAPSET (MAPSET-NAME)                                     EL626   
00715          FROM   (EL626AO)                                         EL626   
00716          DATAONLY                                                 EL626   
00717          CURSOR                                                   EL626   
00718      END-EXEC.                                                    EL626   
00719                                                                   EL626   
00720      GO TO 9500-RETURN-TRAN.                                         CL*22
00721                                                                   EL626   
00722  8300-SEND-TEXT.                                                  EL626   
00723      EXEC CICS SEND TEXT                                          EL626   
00724          FROM   (LOGOFF-TEXT)                                     EL626   
00725          LENGTH (LOGOFF-LENGTH)                                   EL626   
00726          ERASE                                                    EL626   
00727          FREEKB                                                   EL626   
00728      END-EXEC.                                                    EL626   
00729                                                                   EL626   
00730      EXEC CICS RETURN                                             EL626   
00731      END-EXEC.                                                    EL626   
00732                                                                   EL626   
00733  8800-UNAUTHORIZED-ACCESS.                                        EL626   
00734      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL626   
00735      GO TO 8300-SEND-TEXT.                                        EL626   
00736                                                                   EL626   
00737  8820-NOT-OPEN.                                                   EL626   
00738      MOVE 'CONTROL FILE NOT OPEN' TO LOGOFF-MSG.                  EL626   
00739      GO TO 8300-SEND-TEXT.                                        EL626   
00740                                                                   EL626   
00741  8830-NOT-FOUND.                                                  EL626   
00742      MOVE 'USER RECORD NOT FOUND' TO LOGOFF-MSG.                  EL626   
00743      GO TO 8300-SEND-TEXT.                                        EL626   
00744                                                                      CL*16
00745  8900-CHECK-MAIL.                                                    CL*16
00746                                                                      CL*16
00758                                                                      CL*16
00759  8900-EXIT.                                                          CL*16
00760      EXIT.                                                           CL*16
00761                                                                   EL626   
00762     EJECT                                                            CL*22
00763                                                                      CL*22
00764  9000-RETURN-TO-CICS.                                             EL626   
00765      EXEC CICS RETURN                                             EL626   
00766      END-EXEC.                                                    EL626   
00767                                                                   EL626   
00768                                                                      CL*22
00769  9100-HELP.                                                          CL*22
00770      MOVE XCTL-010               TO PGM-NAME.                        CL*22
00771      GO TO 9800-XCTL.                                                CL*22
00772                                                                      CL*22
00773                                                                      CL*22
00774  9200-PF23.                                                          CL*22
00775      MOVE DFHPF23                TO PI-ENTRY-CD-1.                   CL*22
00776      MOVE XCTL-005               TO PGM-NAME.                        CL*22
00777      GO TO 9800-XCTL.                                                CL*22
00778                                                                      CL*22
00779                                                                      CL*22
00780 *9300-PF24.                                                          CL*22
00781 *    MOVE EIBAID   TO PI-ENTRY-CD-1.                                 CL*22
00782 *    MOVE XCTL-EL626             TO PGM-NAME.                        CL*22
00783 *    IF CLAIM-SESSION                                                CL*22
00784 *        MOVE XCTL-EL126         TO PGM-NAME.                        CL*22
00785 *    GO TO 9800-XCTL.                                                CL*22
00786                                                                      CL*22
00787                                                                      CL*22
00788  9500-RETURN-TRAN.                                                   CL*22
00789      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL626   
00790      MOVE '626A'               TO PI-CURRENT-SCREEN-NO.           EL626   
00791      EXEC CICS RETURN                                             EL626   
00792          TRANSID  (TRANS-ID)                                      EL626   
00793          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL626   
00794          LENGTH   (PI-COMM-LENGTH)                                EL626   
00795      END-EXEC.                                                    EL626   
00796                                                                   EL626   
00797  9600-CLEAR.                                                         CL*22
00798      MOVE EIBAID                 TO PI-ENTRY-CD-1.                   CL*22
00799      MOVE XCTL-005               TO PGM-NAME.                        CL*22
00800      GO TO 9800-XCTL.                                                CL*22
00801                                                                      CL*22
00802  9700-PGMID-ERROR.                                                   CL*22
00803      EXEC CICS HANDLE CONDITION                                      CL*22
00804          PGMIDERR (8300-SEND-TEXT)                                   CL*22
00805      END-EXEC.                                                       CL*22
00806                                                                      CL*22
00807      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.              CL*22
00808      MOVE ' '                    TO PI-ENTRY-CD-1.                   CL*22
00809      MOVE XCTL-005               TO PGM-NAME.                        CL*22
00810      MOVE PGM-NAME               TO LOGOFF-PGM.                      CL*22
00811      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                     CL*22
00812      GO TO 9800-XCTL.                                                CL*22
00813                                                                      CL*22
00814  9800-XCTL.                                                          CL*22
00815      MOVE SPACES                 TO PI-SAVED-PROGRAM-1            EL626   
00816                                     PI-SAVED-PROGRAM-2            EL626   
00817                                     PI-SAVED-PROGRAM-3            EL626   
00818                                     PI-SAVED-PROGRAM-4            EL626   
00819                                     PI-SAVED-PROGRAM-5            EL626   
00820                                     PI-SAVED-PROGRAM-6            EL626   
00821                                     PI-RETURN-TO-PROGRAM          EL626   
00822                                     PI-CONTROL-IN-PROGRESS        EL626   
00823                                     PI-ENTRY-CD-2                 EL626   
00824                                     PI-RETURN-CODES               EL626   
00825                                     PI-UPDATE-BY                  EL626   
00826                                     PI-PROGRAM-CONTROLS           EL626   
00827                                     PI-PROGRAM-WORK-AREA.         EL626   
00828                                                                      CL*27
00829      MOVE ZEROS                  TO PI-UPDATE-HHMMSS.             EL626   
00830                                                                      CL*22
00831      IF PGM-NAME = XCTL-IN1                                          CL*27
00832          MOVE 'CR'                   TO PI-IM-SYSTEM-ID              CL*20
00833          MOVE PI-CERT-ACCESS-CONTROL TO PI-IM-CONTROL-ACCESS.        CL*20
00834                                                                      CL*22
00835      IF PGM-NAME = XCTL-SHOT                                         CL*22
00836          EXEC CICS XCTL                                              CL*22
00837              PROGRAM  (PGM-NAME)                                     CL*22
00838              COMMAREA (PROGRAM-INTERFACE-BLOCK)                      CL*22
00839              LENGTH   (SHOT-COMM-LENGTH)                             CL*22
00840          END-EXEC.                                                   CL*22
00841                                                                      CL*22
00842      EXEC CICS XCTL                                               EL626   
00843          PROGRAM  (PGM-NAME)                                      EL626   
00844          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL626   
00845          LENGTH   (PI-COMM-LENGTH)                                EL626   
00846      END-EXEC.                                                    EL626   
00847                                                                   EL626   
00848  9900-ERROR-FORMAT.                                               EL626   
00849      IF NOT EMI-ERRORS-COMPLETE                                   EL626   
00850          MOVE LINK-001           TO PGM-NAME                      EL626   
00851          EXEC CICS LINK                                           EL626   
00852              PROGRAM  (PGM-NAME)                                  EL626   
00853              COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)             EL626   
00854              LENGTH   (EMI-COMM-LENGTH)                           EL626   
00855          END-EXEC.                                                EL626   
00856                                                                   EL626   
00857  9900-EXIT.                                                       EL626   
00858      EXIT.                                                        EL626   
00859                                                                   EL626   
00860  9990-ABEND.                                                      EL626   
00861      MOVE LINK-004               TO PGM-NAME.                     EL626   
00862      MOVE DFHEIBLK               TO EMI-LINE1.                    EL626   
00863      EXEC CICS LINK                                               EL626   
00864          PROGRAM  ('EL004')                                       EL626   
00865          COMMAREA (EMI-LINE1)                                     EL626   
00866          LENGTH   (72)                                            EL626   
00867      END-EXEC.                                                    EL626   
00868                                                                   EL626   
00869      GO TO 8200-SEND-DATAONLY.                                    EL626   
00870      GOBACK.                                                      EL626   
00871                                                                   EL626   
00872  9995-SECURITY-VIOLATION.                                         EL626   
00873                              COPY ELCSCTP.                        EL626   
00874                                                                   EL626   
00875  9995-EXIT.                                                       EL626   
00876      EXIT.                                                        EL626   
00877                                                                   EL626   
