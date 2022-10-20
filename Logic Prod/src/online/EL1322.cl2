00001  IDENTIFICATION DIVISION.                                         03/22/96
00002                                                                   EL1322
00003  PROGRAM-ID.                 EL1322.                                 LV016
00004 *              PROGRAM CONVERTED BY                                  CL*13
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*13
00006 *              CONVERSION DATE 06/29/95 09:45:26.                    CL*13
00007 *                            VMOD=2.016.                             CL*16
00008 *                                                                 EL1322
00008 *                                                                 EL1322
00009 *AUTHOR.    LOGIC, INC.                                              CL*13
00010 *           DALLAS, TEXAS.                                           CL*13
00011                                                                   EL1322
00012 *DATE-COMPILED.                                                      CL*13
00013                                                                   EL1322
00014 *SECURITY.   *****************************************************   CL*13
00015 *            *                                                   *   CL*13
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*13
00017 *            *                                                   *   CL*13
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*13
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*13
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*13
00021 *            *                                                   *   CL*13
00022 *            *****************************************************   CL*13
00023                                                                   EL1322
00024 *REMARKS.                                                            CL**2
00025                                                                   EL1322
00026 *        THIS PROGRAM ALLOWS AN OPERATOR DIRECT ACCESS               CL*13
00027 *    TO A CLAIM RECORD OR PERMITS A BROWSE OF THE CLAIM FILE.        CL**2
00028                                                                   EL1322
00029 *    SCREENS     - EL132B - CLAIM LOOK-UP MATCH LIST                 CL**2
00030                                                                   EL1322
00031 *    ENTERED BY  - EL126 - MAINTENANCE MENU                          CL**2
00032 *                  EL130 - NEW CLAIM SET-UP                          CL**2
00033 *                  EL150 - STATUS DISPLAY (ENTERS EL1323 ONLY)       CL**2
00034                                                                   EL1322
00035 *    EXIT TO     - CALLING PROGRAM                                   CL**2
00036 *                  EL130 - NEW CLAIM SETUP                           CL**2
00037                                                                   EL1322
00038 *    INPUT FILES - ELMSTR - CLAIM MASTER FILE                        CL*13
00039 *                - ELRETR - RETRIEVE MASTER FILE                     CL*13
00040                                                                   EL1322
00041 *    OUTPUT FILE - NONE                                              CL**2
00042                                                                   EL1322
00043 *    COMMAREA    - PASSED.  IF A CLAIM IS SELECTED, THE              CL**2
00044 *                  CONTROL OF THAT CLAIM IS PLACED IN THE            CL**2
00045 *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR           CL**2
00046 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM    CL**2
00047 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE     CL**2
00048 *                  RECORD KEY INFORMATION NEEDED BY EL1322 TO        CL**2
00049 *                  LOCATE THE CLAIM.                                 CL**2
00050                                                                   EL1322
00051                                                                   EL1322
00052 *    NARRATIVE   - USING THE CONTROL INFORMATION PASSED FROM         CL**2
00053 *                  EL132, START A BROWSE ON THE CLAIM MASTER FILE    CL**2
00054 *                  IN AN ATTEMPT TO FIND THE RECORDS INDICATED.      CL**2
00055 *                  IF MORE THAN 16 MATCHES ARE FOUND, THE SCREEN     CL**2
00056 *                  WILL SHOW THE FIRST 16, AND WAIT FOR THE          CL**2
00057 *                  OPERATOR TO CONTINUE THE BROWSE.  IF ONLY ONE     CL**2
00058 *                  ENTRY IS FOUND PASS CONTROL TO THE APPROPRIATE    CL**2
00059 *                  PROGRAM.                                          CL**2
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID & CO ID TO SCREEN
101501*                              ADJUST REDEFINES EL132BI FILLER
101501******************************************************************

00060                                                                   EL1322
00061                                                                   EL1322
00062      EJECT                                                        EL1322
00063  ENVIRONMENT DIVISION.                                            EL1322
00064                                                                   EL1322
00065  DATA DIVISION.                                                   EL1322
00066                                                                   EL1322
00067  WORKING-STORAGE SECTION.                                         EL1322
00068  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.      CL*13
00069                                                                   EL1322
00070  77  FILLER  PIC X(32)  VALUE '********************************'. EL1322
00071  77  FILLER  PIC X(32)  VALUE '*   EL1322 WORKING STORAGE     *'. EL1322
00072  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.016 ************'.    CL*16
00073                                                                   EL1322
00074                                  COPY ELCSCTM.                       CL**7
00075                                                                   EL1322
00076                                  COPY ELCSCRTY.                      CL**7
00077                                                                   EL1322
00078  01  WS-DATE-AREA.                                                EL1322
00079      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL1322
00080      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL1322
00081                                                                   EL1322
00082  01  ERROR-MESSAGES.                                              EL1322
00083      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL1322
00084      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL1322
00085      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL1322
00086      12  ER-0019                 PIC X(4)  VALUE '0019'.             CL**7
00087      12  ER-0022                 PIC X(4)  VALUE '0022'.             CL**7
00088      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL1322
00089      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL1322
00090      12  ER-0089                 PIC X(4)  VALUE '0089'.             CL**7
00091      12  ER-0031                 PIC X(4)  VALUE '0031'.          EL1322
00092      12  ER-0130                 PIC X(4)  VALUE '0130'.          EL1322
00093      12  ER-0131                 PIC X(4)  VALUE '0131'.             CL**9
00094      12  ER-0200                 PIC X(4)  VALUE '0200'.          EL1322
00095      12  ER-0228                 PIC X(4)  VALUE '0228'.             CL**7
00096      12  ER-0284                 PIC X(4)  VALUE '0284'.          EL1322
00097      12  ER-0930                 PIC X(4)  VALUE '0930'.             CL*13
00098      12  ER-0955                 PIC X(4)  VALUE '0955'.             CL*14
00099      12  ER-0973                 PIC X(4)  VALUE '0973'.             CL*13
00100      12  ER-8004                 PIC X(4)  VALUE '8004'.             CL*13
00101                                                                   EL1322
00102  01  FILLER                          COMP-3.                      EL1322
00103                                                                   EL1322
00104      05  WS-RECORD-COUNT         PIC S9(3)   VALUE ZERO.          EL1322
00105      05  WS-READNEXT-SW          PIC S9      VALUE ZERO.          EL1322
00106      05  WS-NOT-FOUND            PIC S9      VALUE ZERO.          EL1322
00107      05  WS-ERROR-NUMBER         PIC S9(3)   VALUE ZERO.          EL1322
00108      05  WS-ERROR-COUNT          PIC S9(3)   VALUE ZERO.          EL1322
00109      05  WS-LAST-ERROR-COUNT     PIC S9(3)   VALUE ZERO.          EL1322
00110      05  WS-UPDATE-SW            PIC S9      VALUE ZERO.          EL1322
00111      05  WS-COMPLETED-SUCCESSFUL PIC S9      VALUE ZERO.          EL1322
00112          88  TRANSACTION-SUCCESSFUL          VALUE +1.            EL1322
00113                                                                   EL1322
00114      05  TIME-IN                 PIC S9(7)   VALUE ZERO.             CL**7
00115      05  TIME-OUT         REDEFINES                                  CL**7
00116          TIME-IN                 PIC S9(3)V9(4).                     CL**7
00117                                                                   EL1322
00118      05  WS-MONTH-WORK           PIC S9(3)   VALUE ZERO.          EL1322
00119      05  WS-YEAR-WORK            PIC S9(3)   VALUE ZERO.          EL1322
00120      05  WS-AIX-RECORD-COUNT     PIC S9(5)   VALUE ZERO.          EL1322
00121      05  WS-CLAIMS-SW            PIC S9      VALUE ZERO.          EL1322
00122          88  WS-NO-CLAIMS-FOUND              VALUE ZERO.          EL1322
00123                                                                   EL1322
00124  01  FILLER            COMP SYNCHRONIZED.                         EL1322
00125                                                                   EL1322
00126      05  WS-INDEX                PIC S9(4)   VALUE ZERO.          EL1322
00127                                                                   EL1322
00128      05  WS-JOURNAL-FILE-ID      PIC S9(4)   VALUE +1.            EL1322
00129      05  WS-JOURNAL-RECORD-LENGTH PIC S9(4)  VALUE +527.          EL1322
00130      05  SC-ITEM                 PIC S9(4)   VALUE +0001.         EL1322
00131                                                                   EL1322
00132                                                                   EL1322
00133  01  FILLER.                                                      EL1322
00134                                                                   EL1322
00135      05  WS-COMPARE-INDICATOR    PIC X(01).                          CL**9
00136          88  NAME-FOUND                    VALUE ' '.                CL**9
00137          88  NAME-NOT-FOUND                VALUE 'X'.                CL**9
00138                                                                      CL**9
00139      05  WS-NAME-INDEX           PIC S9(4) COMP.                     CL**9
00140                                                                      CL**9
00141      05  WS-CL-NAME              PIC X(15).                          CL**9
00142      05  WS-CL-NAME-CHAR REDEFINES WS-CL-NAME                        CL**9
00143                                  PIC X(01) OCCURS 15.                CL**9
00144                                                                      CL**9
00145      05  WS-PI-NAME              PIC X(15).                          CL**9
00146      05  WS-PI-NAME-CHAR REDEFINES WS-PI-NAME                        CL**9
00147                                  PIC X(01) OCCURS 15.                CL**9
00148                                                                      CL**9
00149      05  QID.                                                     EL1322
00150          10  QID-TERM            PIC X(4).                        EL1322
00151          10  FILLER              PIC X(4)  VALUE '132A'.          EL1322
00152      10  QID-PROC-AREA           PIC XXX.                         EL1322
00153      10  QID-LENGTH              PIC S9(4) VALUE +3  COMP.        EL1322
00154      10  QID-ITEM                PIC S9(4) VALUE +1  COMP.        EL1322
00155                                                                   EL1322
00156      05  WS-ELCNTL-KEY.                                              CL*13
00157          10  WS-ELCNTL-ID        PIC X(3).                           CL*13
00158          10  WS-ELCNTL-TYPE      PIC X.                              CL*13
00159          10  WS-ELCNTL-USER      PIC X(04) VALUE SPACES.             CL*13
00160          10  WS-ELCNTL-SEQ       PIC S9(4) VALUE +0      COMP.       CL*13
00161                                                                   EL1322
00162      05  WS-MAPSET-NAME          PIC X(8)  VALUE 'EL132S  '.      EL1322
00163      05  WS-MAP-NAME             PIC X(8)  VALUE 'EL132B  '.      EL1322
00164                                                                   EL1322
00165      05  FILLER           REDEFINES                               EL1322
00166          WS-MAP-NAME.                                             EL1322
00167          10  FILLER              PIC XX.                          EL1322
00168          10  WS-MAP-NUMBER       PIC X(4).                        EL1322
00169          10  FILLER              PIC XX.                             CL*13
00170                                                                   EL1322
00171      05  THIS-PGM                PIC X(08)   VALUE 'EL1322'.         CL**7
00172                                                                   EL1322
00173      05  ELCNTL-FILE-ID          PIC X(08)   VALUE 'ELCNTL'.         CL**7
00174      05  ELRETR-FILE-ID          PIC X(08)   VALUE 'ELRETR'.         CL*13
00175      05  ELMSTR-FILE-ID          PIC X(08)   VALUE 'ELMSTR'.         CL*13
00176      05  ELMSTR-FILE-LENGTH      PIC S9(4)   VALUE +350 COMP.        CL*13
00177                                                                      CL*13
00178      05  GETMAIN-SPACE           PIC X       VALUE SPACE.            CL*13
00179                                                                      CL*13
00180      05  WS-CNTL-REC-FOUND-SW    PIC X(01)   VALUE SPACES.           CL**7
00181      05  WS-NEXT-COMPANY-ID      PIC X(03)   VALUE SPACES.           CL**7
00182                                                                   EL1322
00183      05  WS-TRANS-ID             PIC X(4)    VALUE 'EX22'.        EL1322
00184                                                                   EL1322
00185      05  WS-TEMP-STORAGE-KEY.                                     EL1322
00186          10  WS-TSK-TERM-ID      PIC X(4)    VALUE 'XXXX'.        EL1322
00187          10  FILLER              PIC X(4)    VALUE '1322'.        EL1322
00188                                                                   EL1322
00189      05  WS-EL150-TS.                                                CL**9
00190          10  WS-TS1-TERM-ID      PIC X(04)   VALUE 'XXXX'.           CL**9
00191          10  FILLER              PIC X(04)   VALUE '0150'.           CL**9
00192                                                                      CL**9
00193      05  WS-TS-LENGTH            PIC S9(4)   VALUE +1920   COMP   EL1322
00194                                      SYNCHRONIZED.                EL1322
00195                                                                      CL**9
00196      05  WS-WORK-LENGTH          PIC S9(4)   VALUE +640 COMP SYNC.   CL**9
00197                                                                   EL1322
00198      05  WS-CURRENT-DATE         PIC XX VALUE LOW-VALUES.         EL1322
00199                                                                   EL1322
00200      05  WS-DATE-WORK.                                            EL1322
00201          10  WS-DW-MONTH         PIC 99.                          EL1322
00202          10  FILLER              PIC X.                           EL1322
00203          10  WS-DW-DAY           PIC 99.                          EL1322
00204          10  FILLER              PIC X.                           EL1322
00205          10  WS-DW-YEAR          PIC 99.                          EL1322
00206                                                                   EL1322
00207      05  WS-CHAR                 PIC X       VALUE SPACES.        EL1322
00208                                                                   EL1322
00209      05  WS-INITIALS.                                             EL1322
00210          10  WS-INIT1            PIC X.                           EL1322
00211          10  WS-INIT2            PIC X.                           EL1322
00212                                                                   EL1322
00213      05  WS-CALC-RDNXT           PIC S9(8) VALUE ZERO     COMP.   EL1322
00214                                                                   EL1322
00215      EJECT                                                        EL1322
00216      05  WS-KEY-HOLD.                                             EL1322
00217          10  WS-KH-CHAR          PIC X                            EL1322
00218              OCCURS 29 TIMES        INDEXED BY KEY-INDEX.            CL**5
00219                                                                   EL1322
00220      05  WS-KEY-INPUT.                                            EL1322
00221          10  WS-KI-CHAR          PIC X                            EL1322
00222              OCCURS 29 TIMES        INDEXED BY KEY-INDEX2.           CL**5
00223                                                                   EL1322
00224                                  COPY ELCNWA.                        CL**7
00225                                                                   EL1322
00226      EJECT                                                        EL1322
00227                                  COPY ELCINTF.                       CL**7
00228                                                                   EL1322
00229                                  COPY ELC132PI.                      CL**7
00230                                                                   EL1322
00231      EJECT                                                        EL1322
00232                                  COPY EL132S.                        CL**7
00233                                                                   EL1322
00234  01  FILLER       REDEFINES      EL132BI.                         EL1322
101501     05  FILLER                  PIC X(103).                         CL*13
00236                                                                   EL1322
00237      05  EL132B-MAP-LINE         OCCURS 16 TIMES                  EL1322
00238          INDEXED BY EL132B-INDEX                                  EL1322
00239                     EL132B-INDEX2.                                EL1322
00240                                                                   EL1322
00241          10  EL132B-NUM-LENGTH   PIC S9(4)       COMP.            EL1322
00242          10  EL132B-NUM-ATTRB    PIC X.                           EL1322
00243          10  EL132B-NUM          PIC X(2).                        EL1322
00244                                                                   EL1322
00245          10  EL132B-NAME-LENGTH  PIC S9(4)    COMP.               EL1322
00246          10  EL132B-NAME-ATTRB   PIC X.                           EL1322
00247          10  EL132B-NAME         PIC X(22).                          CL*13
00248                                                                   EL1322
00249          10  EL132B-AGE-LENGTH   PIC S9(4)    COMP.               EL1322
00250          10  EL132B-AGE-ATTRB    PIC X.                           EL1322
00251          10  EL132B-AGE          PIC 99.                          EL1322
00252                                                                   EL1322
00253          10  EL132B-STA-LENGTH   PIC S9(4)    COMP.                  CL**3
00254          10  EL132B-STA-ATTRB    PIC X.                              CL**3
00255          10  EL132B-STA          PIC X.                              CL**3
00256                                                                   EL1322
00257          10  EL132B-DATE-INCURRED-LENGTH PIC S9(4)     COMP.      EL1322
00258          10  EL132B-DATE-INCURRED-ATTRB  PIC X.                   EL1322
00259          10  EL132B-DATE-INCURRED    PIC X(8).                    EL1322
00260                                                                   EL1322
00261          10  EL132B-TYPE-LENGTH  PIC S9(4)      COMP.             EL1322
00262          10  EL132B-TYPE-ATTRB   PIC X.                           EL1322
00263          10  EL132B-TYPE         PIC X.                           EL1322
00264                                                                   EL1322
00265          10  EL132B-CARRIER-LENGTH   PIC S9(4)    COMP.           EL1322
00266          10  EL132B-CARRIER-ATTRB    PIC X.                       EL1322
00267          10  EL132B-CARRIER          PIC X.                       EL1322
00268                                                                   EL1322
00269          10  EL132B-CLAIM-LENGTH PIC S9(4)    COMP.               EL1322
00270          10  EL132B-CLAIM-ATTRB  PIC X.                           EL1322
00271          10  EL132B-CLAIM        PIC X(7).                        EL1322
00272                                                                   EL1322
00273          10  EL132B-CERT-NO-LENGTH   PIC S9(4)    COMP.           EL1322
00274          10  EL132B-CERT-NO-ATTRB    PIC X.                       EL1322
00275          10  EL132B-CERT-NO      PIC X(16).                          CL*13
00276                                                                   EL1322
00277          10  EL132B-ACCOUNT-LENGTH   PIC S9(4)    COMP.           EL1322
00278          10  EL132B-ACCOUNT-ATTRB    PIC X.                       EL1322
00279          10  EL132B-ACCOUNT      PIC X(10).                       EL1322
00280      05  FILLER                  PIC X(171).                         CL*16
00281                                                                      CL*16
00282      EJECT                                                        EL1322
00283                                  COPY ELCEMIB.                       CL**7
00284                                                                   EL1322
00285      EJECT                                                           CL**7
00286                                  COPY ELCDATE.                       CL**7
00287                                                                   EL1322
00288      EJECT                                                        EL1322
00289                                  COPY ELCATTR.                       CL**7
00290                                                                   EL1322
00291      EJECT                                                        EL1322
00292                                  COPY ELCLOGOF.                      CL**7
00293                                                                   EL1322
00294      EJECT                                                        EL1322
00295                                  COPY ELCAID.                        CL**7
00296                                                                   EL1322
00297  01  FILLER                      REDEFINES                        EL1322
00298      DFHAID.                                                      EL1322
00299                                                                   EL1322
00300      05  FILLER                  PIC X(8).                        EL1322
00301                                                                   EL1322
00302      05  PF-VALUES               PIC X                            EL1322
00303          OCCURS 24 TIMES.                                         EL1322
00304      EJECT                                                        EL1322
00305  LINKAGE SECTION.                                                 EL1322
00306                                                                   EL1322
00307  01  DFHCOMMAREA                 PIC X(1024).                     EL1322
00308                                                                   EL1322
00309 *01 DFHBLLDS       COMP   SYNCHRONIZED.                              CL*13
00310 *    05  BLLCBAR                     PIC S9(8).                      CL*13
00311 *    05  ELMSTR-POINTER              PIC S9(8).                      CL*13
00312 *    05  ELCNTL-POINTER              PIC S9(8).                      CL*13
00313                                                                   EL1322
00314      EJECT                                                        EL1322
00315                                  COPY ELCMSTR.                       CL**7
00316      EJECT                                                           CL*13
00317                                  COPY ELCRETR.                       CL*13
00318      EJECT                                                        EL1322
00319                                  COPY ELCCNTL.                       CL**7
00320      EJECT                                                        EL1322
00321  PROCEDURE DIVISION.                                              EL1322
00322                                                                   EL1322
00323      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL1322
00324      MOVE '5'                   TO DC-OPTION-CODE.                EL1322
00325      PERFORM 8500-DATE-CONVERSION.                                EL1322
00326      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL1322
00327      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL1322
00328                                                                   EL1322
00329      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL1322
00330                                                                   EL1322
00331 *    NOTE ******************************************************* EL1322
00332 *         *                                                     * EL1322
00333 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL1322
00334 *         *  FROM ANOTHER MODULE.                               * EL1322
00335 *         *                                                     * EL1322
00336 *         *******************************************************.EL1322
00337                                                                   EL1322
00338      IF EIBCALEN NOT GREATER THAN ZERO                            EL1322
00339          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL1322
00340          PERFORM 8300-SEND-TEXT.                                  EL1322
00341                                                                   EL1322
00342      EXEC CICS HANDLE CONDITION                                   EL1322
00343          PGMIDERR (9600-PGMIDERR)                                 EL1322
00344          NOTFND   (8700-NOT-FOUND)                                EL1322
00345          ENDFILE  (4600-ENDFILE)                                  EL1322
00346          DUPKEY   (4015-DUPKEY)                                   EL1322
00347          ITEMERR  (9400-CLEAR)                                       CL**9
00348          QIDERR   (0020-MAIN-LOGIC)                                  CL**9
00349          ERROR    (9990-ERROR)                                       CL**7
00350      END-EXEC.                                                       CL**7
00351                                                                   EL1322
00352      EJECT                                                        EL1322
00353  0010-MAIN-LOGIC.                                                 EL1322
00354                                                                      CL**9
00355      MOVE EIBTRMID               TO  WS-TS1-TERM-ID.                 CL**9
00356                                                                      CL**9
00357      IF PI-CALLING-PROGRAM = 'EL150'                                 CL*13
00358          EXEC CICS READQ TS                                          CL**9
00359              QUEUE   (WS-EL150-TS)                                   CL**9
00360              INTO    (PI-PROGRAM-WORK-AREA)                          CL**9
00361              LENGTH  (WS-WORK-LENGTH)                                CL**9
00362              ITEM    (QID-ITEM)                                      CL**9
00363          END-EXEC                                                    CL**9
00364          MOVE +2                 TO  PI-1ST-TIME-SW.                 CL**9
00365                                                                      CL**9
00366      EXEC CICS DELETEQ TS                                            CL**9
00367          QUEUE   (WS-EL150-TS)                                       CL**9
00368      END-EXEC.                                                       CL**9
00369                                                                      CL**9
00370  0020-MAIN-LOGIC.                                                    CL**9
00371                                                                      CL**9
00372      IF PI-CALLING-PROGRAM = 'EL132'                                 CL*13
00373          MOVE +0                 TO  PI-SCREEN-COUNT.                CL**9
00374                                                                   EL1322
00375      IF PI-CALLING-PROGRAM EQUAL THIS-PGM                            CL**7
00376          GO TO 0100-MAIN-LOGIC.                                   EL1322
00377                                                                   EL1322
00378      IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM                      CL**7
00379          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6         EL1322
00380          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5         EL1322
00381          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4         EL1322
00382          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3         EL1322
00383          MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2         EL1322
00384          MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1         EL1322
00385          MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM       EL1322
00386          MOVE THIS-PGM             TO  PI-CALLING-PROGRAM            CL**7
00387        ELSE                                                       EL1322
00388          MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM         EL1322
00389          MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM       EL1322
00390          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1         EL1322
00391          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2         EL1322
00392          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3         EL1322
00393          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4         EL1322
00394          MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5         EL1322
00395          MOVE SPACES               TO  PI-SAVED-PROGRAM-6.        EL1322
00396                                                                      CL**5
00397      IF PI-1ST-TIME-SW IS EQUAL TO +2                                CL**9
00398          NEXT SENTENCE                                               CL**9
00399      ELSE                                                            CL**9
00400          MOVE ZERO               TO  PI-SCREEN-COUNT                 CL**9
00401          MOVE PI-CLAIM-KEY       TO  PI-1ST-KEY.                     CL**9
00402                                                                   EL1322
00403      IF PI-1ST-TIME-SW EQUAL TO +2                                EL1322
00404          MOVE EIBTRMID           TO  WS-TSK-TERM-ID               EL1322
00405          EXEC CICS READQ TS                                       EL1322
00406              QUEUE  (WS-TEMP-STORAGE-KEY)                         EL1322
00407              ITEM   (PI-TS-ITEM)                                  EL1322
00408              INTO   (EL132BI)                                     EL1322
00409              LENGTH (WS-TS-LENGTH)                                   CL**7
00410          END-EXEC                                                    CL**7
00411          EXEC CICS DELETEQ TS                                     EL1322
00412              QUEUE  (WS-TEMP-STORAGE-KEY)                            CL**7
00413          END-EXEC                                                    CL**7
00414          MOVE ZERO               TO  PI-1ST-TIME-SW               EL1322
00415          MOVE LOW-VALUES         TO  BSELO                        EL1322
00416                                      BPFKO                        EL1322
00417          PERFORM 5200-SET-ATTRB                                   EL1322
00418              VARYING EL132B-INDEX FROM PI-LINE-COUNT BY -1        EL1322
00419                  UNTIL EL132B-INDEX NOT GREATER THAN ZERO         EL1322
00420          GO TO 8100-SEND-INITIAL-MAP.                                CL**7
00421                                                                   EL1322
00422      IF PI-DSID (1:6) = ELRETR-FILE-ID                               CL*13
00423          GO TO 3000-BROWSE-RETRIEVE-FILE                             CL*16
00424        ELSE                                                          CL*13
00425          GO TO 4000-BROWSE-CLAIM-FILE.                               CL*16
00426                                                                   EL1322
00427      EJECT                                                        EL1322
00428  0100-MAIN-LOGIC.                                                 EL1322
00429                                                                   EL1322
00430 *    NOTE ******************************************************* EL1322
00431 *         *                                                     * EL1322
00432 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL1322
00433 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL1322
00434 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL1322
00435 *         *                                                     * EL1322
00436 *         *******************************************************.EL1322
00437                                                                   EL1322
00438      IF EIBAID EQUAL TO DFHCLEAR                                  EL1322
00439          GO TO 9400-CLEAR.                                           CL**7
00440                                                                   EL1322
00441      IF EIBAID EQUAL TO (DFHPA1 OR                                EL1322
00442                          DFHPA2 OR                                EL1322
00443                          DFHPA3)                                  EL1322
00444          MOVE LOW-VALUES         TO  EL132BI                      EL1322
00445          MOVE -1                 TO  BPFKL                        EL1322
00446          MOVE ER-0008               TO  EMI-ERROR                 EL1322
00447          GO TO 8200-SEND-DATAONLY.                                   CL**7
00448                                                                   EL1322
00449      EXEC CICS RECEIVE                                            EL1322
00450          INTO   (EL132BI)                                         EL1322
00451          MAPSET (WS-MAPSET-NAME)                                  EL1322
00452          MAP    (WS-MAP-NAME)                                        CL**7
00453      END-EXEC.                                                       CL**7
00454                                                                   EL1322
00455      IF BPFKL IS GREATER THAN ZERO                                EL1322
00456          IF EIBAID NOT EQUAL TO DFHENTER                          EL1322
00457              MOVE ER-0004           TO  EMI-ERROR                 EL1322
00458              MOVE AL-UNBOF       TO  BPFKA                        EL1322
00459              MOVE -1             TO  BPFKL                        EL1322
00460              GO TO 8200-SEND-DATAONLY                                CL**7
00461            ELSE                                                   EL1322
00462              IF BPFKO IS NUMERIC                                  EL1322
00463                AND BPFKO IS GREATER THAN ZERO                     EL1322
00464                AND BPFKO IS LESS THAN '25'                        EL1322
00465                  MOVE PF-VALUES (BPFKI)  TO  EIBAID               EL1322
00466                ELSE                                               EL1322
00467                  MOVE ER-0029           TO  EMI-ERROR             EL1322
00468                  MOVE AL-UNBOF       TO  BPFKA                    EL1322
00469                  MOVE -1             TO  BPFKL                    EL1322
00470                  GO TO 8200-SEND-DATAONLY.                           CL**7
00471                                                                   EL1322
00472      IF EIBAID IS EQUAL TO DFHPF12                                EL1322
00473          MOVE 'EL010'            TO  THIS-PGM                        CL*13
00474          GO TO 9300-XCTL.                                            CL**7
00475                                                                   EL1322
00476      IF EIBAID IS EQUAL TO DFHPF23                                EL1322
00477          GO TO 9000-RETURN-CICS.                                     CL**7
00478                                                                   EL1322
00479      IF EIBAID IS EQUAL TO DFHPF24                                EL1322
00480          MOVE 'EL126'            TO  THIS-PGM                        CL*13
00481          GO TO 9300-XCTL.                                            CL**7
00482                                                                      CL*14
00483      IF EIBAID = DFHPF3 OR DFHPF4                                    CL*14
00484         IF PI-DSID (1:6) = ELRETR-FILE-ID                            CL*14
00485             MOVE -1                 TO  BSELL                        CL*14
00486             MOVE ER-0955            TO  EMI-ERROR                    CL*14
00487             GO TO 8200-SEND-DATAONLY.                                CL*14
00488                                                                   EL1322
00489      IF EIBAID EQUAL TO (DFHENTER OR                              EL1322
00490                          DFHPF1 OR                                EL1322
00491                          DFHPF2 OR                                EL1322
00492                          DFHPF3 OR                                EL1322
00493                          DFHPF4 OR                                EL1322
00494                          DFHPF5 OR                                EL1322
00495                          DFHPF6 OR                                   CL*13
00496                          DFHPF7)                                     CL*13
00497          NEXT SENTENCE                                            EL1322
00498        ELSE                                                       EL1322
00499          MOVE -1                 TO  BPFKL                        EL1322
00500          MOVE ER-0008               TO  EMI-ERROR                 EL1322
00501          GO TO 8200-SEND-DATAONLY.                                   CL**7
00502                                                                   EL1322
00503      IF EIBAID EQUAL (DFHPF3 OR DFHPF4 OR DFHPF7)                    CL*13
00504        OR BSELL GREATER THAN ZERO                                 EL1322
00505          NEXT SENTENCE                                            EL1322
00506        ELSE                                                       EL1322
00507          GO TO 0120-MAIN-LOGIC.                                   EL1322
00508                                                                   EL1322
00509      IF BSELL GREATER THAN ZERO                                   EL1322
00510         AND BSELI NUMERIC                                         EL1322
00511         NEXT SENTENCE                                             EL1322
00512      ELSE                                                         EL1322
00513         IF PI-SAVED-PROGRAM-1 = 'EL130'                              CL*13
00514            OR EIBAID EQUAL TO DFHPF4 OR DFHPF7                       CL*13
00515              NEXT SENTENCE                                        EL1322
00516          ELSE                                                     EL1322
00517             MOVE -1                 TO  BSELL                     EL1322
00518             MOVE ER-0031               TO  EMI-ERROR              EL1322
00519             GO TO 8200-SEND-DATAONLY.                                CL**7
00520                                                                   EL1322
00521      IF BSELL GREATER THAN ZERO                                   EL1322
00522        AND BSELO GREATER THAN ZERO                                EL1322
00523        AND BSELO LESS THAN '17'                                   EL1322
00524        AND (BSELI NUMERIC AND BSELI NOT GREATER PI-LINE-COUNT)       CL**2
00525          NEXT SENTENCE                                            EL1322
00526        ELSE                                                       EL1322
00527          IF PI-SAVED-PROGRAM-1 EQUAL TO 'EL130'                      CL*13
00528            OR EIBAID EQUAL TO DFHPF4                              EL1322
00529              NEXT SENTENCE                                        EL1322
00530            ELSE                                                   EL1322
00531              MOVE -1                 TO  BSELL                    EL1322
00532              MOVE ER-0200               TO  EMI-ERROR             EL1322
00533              GO TO 8200-SEND-DATAONLY.                               CL**7
00534                                                                      CL**2
00535      IF BSELL IS GREATER THAN ZERO                                   CL**2
00536          IF BSELI IS NUMERIC                                         CL**2
00537              NEXT SENTENCE                                           CL**2
00538          ELSE                                                        CL**2
00539              MOVE -1                 TO  BSELL                       CL**2
00540              MOVE ER-0031            TO  EMI-ERROR                   CL**2
00541              GO TO 8200-SEND-DATAONLY.                               CL**7
00542                                                                   EL1322
00543      IF BSELL GREATER THAN ZERO                                   EL1322
00544          SET EL132B-INDEX                                            CL*13
00545              PI-K-INDEX TO BSELI                                     CL*13
00546          MOVE EL132B-CARRIER (EL132B-INDEX)  TO  PI-CARRIER       EL1322
00547          MOVE EL132B-CLAIM   (EL132B-INDEX)  TO  PI-CLAIM-NO      EL1322
00548          MOVE PI-KEEP-CERT-NO (PI-K-INDEX)   TO  PI-CERT-NO          CL*13
00549          MOVE EL132B-CERT-NO (EL132B-INDEX)  TO  PI-CCN-NO           CL*13
00550          MOVE EL132B-ACCOUNT (EL132B-INDEX)  TO  PI-ACCOUNT       EL1322
00551          SET PI-INDEX TO BSELI                                    EL1322
00552          MOVE PI-SA-GROUP    (PI-INDEX)  TO  PI-GROUPING          EL1322
00553          MOVE PI-SA-STATE    (PI-INDEX)  TO  PI-STATE             EL1322
00554          MOVE PI-SA-EFF-DATE (PI-INDEX)  TO  PI-CERT-EFF-DT       EL1322
00555        ELSE                                                       EL1322
00556          MOVE SPACES             TO  PI-CARRIER                   EL1322
00557                                      PI-CLAIM-NO                  EL1322
00558                                      PI-CERT-NO                   EL1322
00559                                      PI-CCN-NO                       CL*13
00560                                      PI-ACCOUNT                   EL1322
00561                                      PI-GROUPING                  EL1322
00562                                      PI-STATE                     EL1322
00563          MOVE LOW-VALUES         TO  PI-CERT-EFF-DT.              EL1322
00564                                                                   EL1322
00565      MOVE +2                     TO  PI-1ST-TIME-SW.                 CL*13
00566                                                                   EL1322
00567      IF PI-SAVED-PROGRAM-1 = 'EL126'  OR                             CL*13
00568                              'EL127'  OR                             CL*13
00569                              'EL1275'                                CL*13
00570          MOVE 'EL150'                 TO  THIS-PGM                   CL*13
00571      ELSE                                                            CL**9
00572          IF PI-SAVED-PROGRAM-1 = 'EL130'  OR                         CL*13
00573                                  'EL150'  OR                         CL*13
00574                                  'EL1602' OR                         CL*13
00575                                  'EL162'                             CL*13
00576              MOVE PI-SAVED-PROGRAM-1  TO  THIS-PGM                   CL**9
00577          ELSE                                                        CL**9
00578              MOVE 'EL1323'            TO  THIS-PGM.                  CL*13
00579                                                                      CL**9
00580      IF EIBAID EQUAL TO DFHPF3                                    EL1322
00581          MOVE 'EL1323'           TO  THIS-PGM.                       CL*13
00582                                                                   EL1322
00583      IF EIBAID EQUAL TO DFHPF4                                       CL**9
00584          MOVE 'EL130'            TO  THIS-PGM.                       CL*13
00585                                                                      CL**9
00586      IF EIBAID = DFHPF7                                              CL*13
00587          GO TO 0110-RESTORE-CLAIM.                                   CL*13
00588                                                                      CL*13
00589      IF THIS-PGM = 'EL1323' OR                                       CL*13
00590                    'EL150'                                           CL*13
00591          MOVE EIBTRMID   TO  WS-TSK-TERM-ID                       EL1322
00592          MOVE -1         TO  BSELL                                EL1322
00593          EXEC CICS WRITEQ TS                                      EL1322
00594              FROM   (EL132BI)                                     EL1322
00595              LENGTH (WS-TS-LENGTH)                                EL1322
00596              QUEUE  (WS-TEMP-STORAGE-KEY)                         EL1322
00597              ITEM   (PI-TS-ITEM)                                     CL**7
00598          END-EXEC                                                    CL**7
00599        ELSE                                                       EL1322
00600          MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM         EL1322
00601          MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM       EL1322
00602          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1         EL1322
00603          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2         EL1322
00604          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3         EL1322
00605          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4         EL1322
00606          MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5         EL1322
00607          MOVE SPACES               TO  PI-SAVED-PROGRAM-6.        EL1322
00608                                                                   EL1322
00609      GO TO 9300-XCTL.                                                CL**7
00610                                                                   EL1322
00611  EJECT                                                               CL*13
00612  0110-RESTORE-CLAIM.                                                 CL*13
00613      MOVE PI-COMPANY-CD     TO PI-CK-COMPANY-CD.                     CL*13
00614      MOVE PI-CARRIER        TO PI-CK-CARRIER.                        CL*13
00615      MOVE PI-CLAIM-NO       TO PI-CK-CLAIM.                          CL*13
00616      MOVE PI-CERT-NO        TO PI-CK-CERT-NO.                        CL*13
00617                                                                      CL*13
00618      EXEC CICS READ                                                  CL*13
00619          DATASET   (ELRETR-FILE-ID)                                  CL*13
00620          RIDFLD    (PI-CLAIM-KEY)                                    CL*13
00621          SET       (ADDRESS OF RETRIEVE-MASTER)                      CL*13
00622      END-EXEC.                                                       CL*13
00623                                                                      CL*13
00624      IF RL-PURGED-DT NOT = LOW-VALUES AND SPACES                     CL*13
00625          MOVE -1                 TO  BSELL                           CL*13
00626          MOVE ER-0973            TO  EMI-ERROR                       CL*13
00627          GO TO 8200-SEND-DATAONLY.                                   CL*13
00628                                                                      CL*13
00629      EXEC CICS HANDLE CONDITION                                      CL*13
00630          DUPREC   (0115-DUPREC)                                      CL*13
00631      END-EXEC.                                                       CL*13
00632                                                                      CL*13
00633      EXEC CICS GETMAIN                                               CL*13
00634          SET      (ADDRESS OF CLAIM-MASTER)                          CL*13
00635          INITIMG  (GETMAIN-SPACE)                                    CL*13
00636          LENGTH   (ELMSTR-FILE-LENGTH)                               CL*13
00637      END-EXEC.                                                       CL*13
00638                                                                      CL*13
00639      MOVE RETRIEVE-MASTER   TO  CLAIM-MASTER.                        CL*13
00640      MOVE 'CL'              TO  CL-RECORD-ID.                        CL*13
00641      MOVE '4'               TO  CL-LAST-MAINT-TYPE.                  CL*13
00642      MOVE WS-CURRENT-DATE   TO  CL-RESTORED-DT.                      CL*13
00643                                                                      CL*13
00644      EXEC CICS WRITE                                                 CL*13
00645          DATASET   (ELMSTR-FILE-ID)                                  CL*13
00646          RIDFLD    (PI-CLAIM-KEY)                                    CL*13
00647          FROM      (CLAIM-MASTER)                                    CL*13
00648      END-EXEC.                                                       CL*13
00649                                                                      CL*13
00650      EXEC CICS DELETE                                                CL*13
00651          DATASET   (ELRETR-FILE-ID)                                  CL*13
00652          RIDFLD    (PI-CLAIM-KEY)                                    CL*13
00653      END-EXEC.                                                       CL*13
00654                                                                      CL*13
00655      GO TO 9400-CLEAR.                                               CL*15
00656                                                                      CL*13
00657  0115-DUPREC.                                                        CL*13
00658      MOVE -1                 TO  BSELL.                              CL*13
00659      MOVE ER-0930            TO  EMI-ERROR.                          CL*13
00660      GO TO 8200-SEND-DATAONLY.                                       CL*13
00661                                                                      CL*13
00662  EJECT                                                               CL*13
00663  0120-MAIN-LOGIC.                                                 EL1322
00664                                                                   EL1322
00665      IF EIBAID EQUAL TO (DFHENTER OR DFHPF1 OR DFHPF5 OR DFHPF6)  EL1322
00666        OR                                                         EL1322
00667          ((EIBAID EQUAL TO DFHPF2) AND                            EL1322
00668           (PI-SCREEN-COUNT GREATER THAN +1))                      EL1322
00669              NEXT SENTENCE                                        EL1322
00670            ELSE                                                   EL1322
00671              MOVE -1                 TO  BPFKL                    EL1322
00672              MOVE ER-0131            TO  EMI-ERROR                   CL**9
00673              GO TO 8200-SEND-DATAONLY.                               CL**7
00674                                                                   EL1322
00675      IF EIBAID EQUAL DFHPF5                                       EL1322
00676         IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES                EL1322
00677            PERFORM 8600-NEXT-COMPANY THRU 8600-EXIT               EL1322
00678            PERFORM 8650-WRITE-SECURITY-TEMP-STORE THRU 8650-EXIT  EL1322
00679            IF NOT DISPLAY-CAP                                        CL**7
00680                PERFORM 8800-INITIALIZE-MAP VARYING EL132B-INDEX      CL**7
00681                 FROM +1 BY +1 UNTIL EL132B-INDEX GREATER THAN +16    CL**7
00682                MOVE 'READ'           TO  SM-READ                     CL**7
00683                PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT        CL*16
00684                MOVE ER-0070          TO  EMI-ERROR                   CL**7
00685                MOVE -1               TO  BPFKL                       CL**7
00686                GO TO 8100-SEND-INITIAL-MAP                           CL**7
00687            ELSE                                                      CL**7
00688                NEXT SENTENCE                                         CL**7
00689         ELSE                                                      EL1322
00690            MOVE ER-0008              TO  EMI-ERROR                   CL**7
00691            MOVE -1                   TO  BSELL                       CL**7
00692            GO TO 8200-SEND-DATAONLY.                                 CL**7
00693                                                                   EL1322
00694      IF EIBAID EQUAL DFHPF6                                       EL1322
00695         IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES                EL1322
00696            PERFORM 8600-NEXT-COMPANY THRU 8600-EXIT               EL1322
00697            PERFORM 8650-WRITE-SECURITY-TEMP-STORE THRU 8650-EXIT  EL1322
00698            MOVE PI-ORIGINAL-COMPANY-CD TO PI-COMPANY-CD           EL1322
00699                                           PI-CK-COMPANY-CD           CL**7
00700            IF NOT DISPLAY-CAP                                        CL**7
00701                PERFORM 8800-INITIALIZE-MAP VARYING EL132B-INDEX      CL**7
00702                 FROM +1 BY +1 UNTIL EL132B-INDEX GREATER THAN +16    CL**7
00703                MOVE 'READ'           TO  SM-READ                     CL**7
00704                PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT        CL*16
00705                MOVE ER-0070          TO  EMI-ERROR                   CL**7
00706                MOVE -1               TO  BPFKL                       CL**7
00707                GO TO 8100-SEND-INITIAL-MAP                           CL**7
00708            ELSE                                                      CL**7
00709                NEXT SENTENCE                                         CL**7
00710         ELSE                                                         CL**7
00711            MOVE ER-0008              TO  EMI-ERROR                   CL**7
00712            MOVE -1                   TO  BSELL                       CL**7
00713            GO TO 8200-SEND-DATAONLY.                                 CL**7
00714                                                                   EL1322
00715      IF PI-END-OF-FILE GREATER THAN ZERO                          EL1322
00716          IF EIBAID EQUAL TO (DFHENTER OR DFHPF1)                     CL**9
00717              MOVE ER-0130            TO  EMI-ERROR                   CL**9
00718              MOVE -1                 TO  BSELL                       CL**9
00719              GO TO 8200-SEND-DATAONLY.                               CL**9
00720                                                                      CL**9
00721      IF PI-END-OF-FILE GREATER THAN ZERO                             CL**9
00722          IF (EIBAID EQUAL TO DFHPF2 AND                              CL**9
00723              PI-LAST-EIBAID EQUAL TO DFHPF2)                         CL**9
00724                  MOVE ER-0131        TO  EMI-ERROR                   CL**9
00725                  MOVE -1             TO  BSELL                       CL**7
00726                  GO TO 8200-SEND-DATAONLY.                           CL**7
00727                                                                      CL**7
00728      IF (EIBAID IS EQUAL TO DFHENTER OR DFHPF1)                      CL**7
00729          IF PI-CK-COMPANY-CD IS EQUAL TO PI-COMPANY-CD               CL**7
00730              NEXT SENTENCE                                           CL**7
00731          ELSE                                                        CL**7
00732              MOVE -1                 TO  BSELL                       CL**7
00733              GO TO 8200-SEND-DATAONLY.                               CL**7
00734                                                                   EL1322
00735      IF PI-DSID (1:6) = ELRETR-FILE-ID                               CL*13
00736          GO TO 3000-BROWSE-RETRIEVE-FILE                             CL*16
00737        ELSE                                                          CL*13
00738          GO TO 4000-BROWSE-CLAIM-FILE.                               CL*16
00739                                                                      CL*13
00740      EJECT                                                           CL*13
00741  3000-BROWSE-RETRIEVE-FILE SECTION.                                  CL*13
00742                                                                      CL*13
00743      EXEC CICS HANDLE CONDITION                                      CL*13
00744          NOTFND   (8700-NOT-FOUND)                                   CL*13
00745          DUPKEY   (3015-DUPKEY)                                      CL*13
00746      END-EXEC.                                                       CL*13
00747                                                                      CL*13
00748      MOVE EIBAID                 TO  PI-LAST-EIBAID.                 CL*13
00749                                                                      CL*13
00750      MOVE LOW-VALUES             TO  EL132BI.                        CL*13
00751                                                                      CL*13
00752      IF PI-BROWSE-SW = ZERO AND                                      CL*13
00753         PI-START-SW = +1                                             CL*13
00754          EXEC CICS STARTBR                                           CL*13
00755              DATASET   (PI-DSID)                                     CL*13
00756              RIDFLD    (PI-CLAIM-KEY)                                CL*13
00757              GENERIC                                                 CL*13
00758              EQUAL                                                   CL*13
00759              KEYLENGTH (PI-KEY-LENGTH)                               CL*13
00760          END-EXEC                                                    CL*13
00761          GO TO 3005-NEXT-SENTENCE.                                   CL*13
00762                                                                      CL*13
00763      IF EIBAID = DFHPF2                                              CL*13
00764          SUBTRACT 2 FROM PI-SCREEN-COUNT                             CL*13
00765          PERFORM 7000-PF2-POSITION                                   CL*13
00766          GO TO 3005-NEXT-SENTENCE.                                   CL*13
00767                                                                      CL*13
00768      EXEC CICS STARTBR                                               CL*13
00769          DATASET (PI-DSID)                                           CL*13
00770          RIDFLD  (PI-CLAIM-KEY)                                      CL*13
00771          EQUAL                                                       CL*13
00772      END-EXEC.                                                       CL*13
00773                                                                      CL*13
00774  3005-NEXT-SENTENCE.                                                 CL*13
00775      MOVE +1                     TO  PI-BROWSE-SW.                   CL*13
00776      MOVE ZERO                   TO  PI-LINE-COUNT.                  CL*13
00777      MOVE LOW-VALUES             TO  EL132BI.                        CL*13
00778      MOVE PI-CLAIM-KEY           TO  WS-KEY-HOLD.                    CL*13
00779      SET EL132B-INDEX                                                CL*13
00780          PI-K-INDEX   TO +1.                                         CL*13
00781      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.                CL*13
00782      MOVE '5'                    TO  DC-OPTION-CODE.                 CL*13
00783      PERFORM 8500-DATE-CONVERSION.                                   CL*13
00784      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE.                CL*13
00785                                                                      CL*13
00786  3010-READNEXT.                                                      CL*13
00787      EXEC CICS READNEXT                                              CL*13
00788          DATASET (PI-DSID)                                           CL*13
00789          RIDFLD  (PI-CLAIM-KEY)                                      CL*13
00790          SET     (ADDRESS OF RETRIEVE-MASTER)                        CL*13
00791      END-EXEC.                                                       CL*13
00792                                                                      CL*13
00793      IF PI-LINE-COUNT NOT = ZERO                                     CL*13
00794          MOVE ZERO               TO  PI-AIX-RECORD-COUNT.            CL*13
00795                                                                      CL*13
00796  3015-DUPKEY.                                                        CL*13
00797      IF LCP-ONCTR-01 =  0                                            CL*13
00798          ADD 1 TO LCP-ONCTR-01                                       CL*13
00799        ELSE                                                          CL*13
00800          GO TO 3016-NEXT-SENTENCE.                                   CL*13
00801                                                                      CL*13
00802      IF PI-AIX-RECORD-COUNT GREATER THAN +16                         CL*13
00803          SUBTRACT +1 FROM PI-AIX-RECORD-COUNT.                       CL*13
00804                                                                      CL*13
00805  3016-NEXT-SENTENCE.                                                 CL*13
00806      ADD +1  TO  WS-AIX-RECORD-COUNT.                                CL*13
00807                                                                      CL*13
00808      IF EIBAID = DFHPF1 OR DFHENTER                                  CL*13
00809         IF OPTION-TWO-SELECTED                                       CL*13
00810            IF PI-LINE-COUNT = ZERO                                   CL*13
00811               PERFORM 5000R-MOVE-NAME                                CL*13
00812               IF RL-INSURED-LAST-NAME = PI-LAST-NAME                 CL*13
00813                  IF PI-LAST-KEY = RL-CONTROL-PRIMARY                 CL*13
00814                     NEXT SENTENCE                                    CL*13
00815                  ELSE                                                CL*13
00816                     GO TO 3010-READNEXT.                             CL*13
00817                                                                      CL*13
00818      MOVE PI-CLAIM-KEY TO WS-KEY-INPUT.                              CL*13
00819                                                                      CL*13
00820      SET KEY-INDEX                                                   CL*13
00821          KEY-INDEX2 TO +1.                                           CL*13
00822                                                                      CL*13
00823  3020-COMPARE-KEY.                                                   CL*13
00824      IF WS-KH-CHAR (KEY-INDEX) NOT = WS-KI-CHAR (KEY-INDEX2)         CL*13
00825          GO TO 4700-END-OF-BROWSE.                                   CL*13
00826                                                                      CL*13
00827      IF KEY-INDEX LESS THAN PI-KEY-LENGTH                            CL*13
00828          SET KEY-INDEX                                               CL*13
00829              KEY-INDEX2 UP BY +1                                     CL*13
00830          GO TO 3020-COMPARE-KEY.                                     CL*13
00831                                                                      CL*13
00832      IF OPTION-TWO-SELECTED                                          CL*13
00833         IF PI-ACCOUNT-NUMBER NOT = SPACES                            CL*13
00834            IF PI-ACCOUNT-NUMBER = RL-CERT-ACCOUNT                    CL*13
00835               NEXT SENTENCE                                          CL*13
00836            ELSE                                                      CL*13
00837               GO TO 3010-READNEXT.                                   CL*13
00838                                                                      CL*13
00839      IF PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY            CL*13
00840          GO TO 3095-MOVE-DATA.                                       CL*13
00841                                                                      CL*13
00842      IF PI-CARRIER-SECURITY GREATER THAN SPACES                      CL*13
00843         IF RL-CARRIER = PI-CARRIER-SECURITY                          CL*13
00844              NEXT SENTENCE                                           CL*13
00845             ELSE                                                     CL*13
00846              GO TO 3010-READNEXT.                                    CL*13
00847                                                                      CL*13
00848      IF PI-ACCOUNT-SECURITY GREATER SPACES                           CL*13
00849          IF RL-CERT-ACCOUNT = PI-ACCOUNT-SECURITY                    CL*13
00850              NEXT SENTENCE                                           CL*13
00851             ELSE                                                     CL*13
00852              GO TO 3010-READNEXT.                                    CL*13
00853                                                                      CL*13
00854  3095-MOVE-DATA.                                                     CL*13
00855      MOVE +1                     TO WS-CLAIMS-SW.                    CL*13
00856                                                                      CL*13
00857      MOVE WS-KEY-INPUT           TO PI-LAST-KEY.                     CL*13
00858                                                                      CL*13
00859      ADD +1  TO  PI-LINE-COUNT                                       CL*13
00860                  PI-AIX-RECORD-COUNT                                 CL*13
00861                                                                      CL*13
00862      PERFORM 5000R-MOVE-NAME.                                        CL*13
00863                                                                      CL*13
00864      MOVE RL-CONTROL-PRIMARY   TO PI-LAST-KEY.                       CL*13
00865      MOVE RL-INSURED-LAST-NAME TO PI-LAST-NAME.                      CL*13
00866      MOVE WS-NAME-WORK         TO EL132B-NAME (EL132B-INDEX).        CL*13
00867                                                                      CL*13
00868      IF RL-INSURED-BIRTH-DT NOT = LOW-VALUES                         CL*13
00869          MOVE RL-INSURED-BIRTH-DT    TO  DC-BIN-DATE-1               CL*13
00870          MOVE WS-CURRENT-DATE        TO  DC-BIN-DATE-2               CL*13
00871          MOVE '1'                    TO  DC-OPTION-CODE              CL*13
00872          PERFORM 8500-DATE-CONVERSION                                CL*13
00873          DIVIDE DC-ELAPSED-MONTHS BY +12 GIVING EL132B-AGE           CL*13
00874                                                    (EL132B-INDEX).   CL*13
00875                                                                      CL*13
00876      MOVE RL-CLAIM-STATUS      TO  EL132B-STA       (EL132B-INDEX)   CL*13
00877                                                                      CL*13
00878      IF RL-PURGED-DT NOT = LOW-VALUES                                CL*13
00879         MOVE 'P'               TO  EL132B-STA (EL132B-INDEX).        CL*13
00880                                                                      CL*13
00881      IF RL-INCURRED-DT NOT = LOW-VALUES                              CL*13
00882          MOVE SPACES             TO  DC-OPTION-CODE                  CL*13
00883          MOVE RL-INCURRED-DT     TO  DC-BIN-DATE-1                   CL*13
00884          PERFORM 8500-DATE-CONVERSION                                CL*13
00885          MOVE DC-GREG-DATE-1-EDIT  TO  EL132B-DATE-INCURRED          CL*13
00886                                                    (EL132B-INDEX).   CL*13
00887                                                                      CL*13
00888      MOVE RL-CLAIM-TYPE     TO  EL132B-TYPE    (EL132B-INDEX).       CL*13
00889      MOVE RL-CARRIER        TO  EL132B-CARRIER (EL132B-INDEX).       CL*13
00890      MOVE RL-CLAIM-NO       TO  EL132B-CLAIM   (EL132B-INDEX).       CL*13
00891                                                                      CL*13
00892      IF CREDIT-CARD-INDEX                                            CL*13
00893          MOVE RL-CCN-A5     TO  EL132B-CERT-NO (EL132B-INDEX)        CL*13
00894        ELSE                                                          CL*13
00895          MOVE RL-CERT-NO    TO  EL132B-CERT-NO (EL132B-INDEX).       CL*13
00896                                                                      CL*13
00897      MOVE RL-CERT-NO        TO  PI-KEEP-CERT-NO (PI-K-INDEX).        CL*13
00898                                                                      CL*13
00899      MOVE RL-CERT-ACCOUNT   TO  EL132B-ACCOUNT (EL132B-INDEX).       CL*13
00900                                                                      CL*13
00901      SET PI-INDEX  TO  EL132B-INDEX.                                 CL*13
00902                                                                      CL*13
00903      MOVE RL-CERT-GROUPING  TO  PI-SA-GROUP    (PI-INDEX).           CL*13
00904      MOVE RL-CERT-STATE     TO  PI-SA-STATE    (PI-INDEX).           CL*13
00905      MOVE RL-CERT-EFF-DT    TO  PI-SA-EFF-DATE (PI-INDEX).           CL*13
00906                                                                      CL*13
00907      PERFORM 5200-SET-ATTRB.                                         CL*13
00908                                                                      CL*13
00909      IF RL-CERT-NO = RL-PRIME-CERT-NO AND                            CL*13
00910         RL-ASSOC-CERT-TOTAL GREATER THAN +1                          CL*13
00911           MOVE AL-SABON   TO EL132B-NAME-ATTRB (EL132B-INDEX)        CL*13
00912         ELSE                                                         CL*13
00913           MOVE AL-SANON   TO EL132B-NAME-ATTRB (EL132B-INDEX).       CL*13
00914                                                                      CL*13
00915      IF EL132B-INDEX LESS THAN +16                                   CL*13
00916          SET EL132B-INDEX                                            CL*13
00917              PI-K-INDEX  UP BY +1                                    CL*13
00918          GO TO 3010-READNEXT.                                        CL*13
00919                                                                      CL*13
00920      GO TO 4900-ENDBROWSE.                                           CL*13
00921                                                                   EL1322
00922      EJECT                                                        EL1322
00923  4000-BROWSE-CLAIM-FILE SECTION.                                     CL**9
00924                                                                   EL1322
00925      EXEC CICS HANDLE CONDITION                                   EL1322
00926          NOTFND   (8700-NOT-FOUND)                                EL1322
00927          END-EXEC.                                                EL1322
00928                                                                   EL1322
00929      MOVE EIBAID                 TO  PI-LAST-EIBAID               EL1322
00930                                                                   EL1322
00931      MOVE LOW-VALUES             TO  EL132BI                      EL1322
00932                                                                   EL1322
00933      IF PI-BROWSE-SW EQUAL TO ZERO                                EL1322
00934        AND PI-START-SW EQUAL TO +1                                EL1322
00935          EXEC CICS STARTBR                                        EL1322
00936              DATASET   (PI-DSID)                                  EL1322
00937              RIDFLD    (PI-CLAIM-KEY)                             EL1322
00938              GENERIC                                                 CL**7
00939              EQUAL                                                   CL**7
00940              KEYLENGTH (PI-KEY-LENGTH)                               CL**7
00941          END-EXEC                                                    CL**7
00942          GO TO 4005-NEXT-SENTENCE.                                EL1322
00943                                                                   EL1322
00944      IF EIBAID EQUAL TO DFHPF2                                    EL1322
00945          SUBTRACT 2 FROM PI-SCREEN-COUNT                          EL1322
00946          PERFORM 7000-PF2-POSITION                                EL1322
00947          GO TO 4005-NEXT-SENTENCE.                                EL1322
00948                                                                   EL1322
00949      EXEC CICS STARTBR                                            EL1322
00950          DATASET (PI-DSID)                                        EL1322
00951          RIDFLD  (PI-CLAIM-KEY)                                   EL1322
00952          EQUAL                                                       CL**7
00953      END-EXEC.                                                       CL**7
00954                                                                   EL1322
00955  4005-NEXT-SENTENCE.                                              EL1322
00956                                                                   EL1322
00957      MOVE +1                     TO  PI-BROWSE-SW.                EL1322
00958      MOVE ZERO                   TO  PI-LINE-COUNT                EL1322
00959      MOVE LOW-VALUES             TO  EL132BI                      EL1322
00960      MOVE PI-CLAIM-KEY     TO  WS-KEY-HOLD                        EL1322
00961      SET EL132B-INDEX                                                CL*13
00962          PI-K-INDEX  TO +1.                                          CL*13
00963      MOVE EIBDATE                TO  DC-JULIAN-YYDDD              EL1322
00964      MOVE '5'                    TO  DC-OPTION-CODE               EL1322
00965      PERFORM 8500-DATE-CONVERSION                                 EL1322
00966      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE.             EL1322
00967                                                                   EL1322
00968  4010-READNEXT.                                                   EL1322
00969                                                                   EL1322
00970      EXEC CICS READNEXT                                           EL1322
00971          DATASET (PI-DSID)                                        EL1322
00972          RIDFLD  (PI-CLAIM-KEY)                                   EL1322
00973          SET     (ADDRESS OF CLAIM-MASTER)                           CL*13
00974      END-EXEC.                                                       CL**7
00975                                                                   EL1322
00976      IF PI-LINE-COUNT NOT EQUAL TO ZERO                           EL1322
00977          MOVE ZERO               TO  PI-AIX-RECORD-COUNT.         EL1322
00978                                                                   EL1322
00979      IF PI-COMPANY-ID = 'DMD'                                        CL*13
00980         IF CL-LAST-CLOSE-REASON = 'C' OR 'E'                         CL*13
00981             GO TO 4010-READNEXT.                                     CL*13
00982                                                                      CL*13
00983  4015-DUPKEY.                                                     EL1322
00984                                                                   EL1322
00985      IF LCP-ONCTR-01 =  0                                            CL*13
00986          ADD 1 TO LCP-ONCTR-01                                       CL*13
00987        ELSE                                                       EL1322
00988          GO TO 4016-NEXT-SENTENCE.                                EL1322
00989                                                                   EL1322
00990      IF PI-AIX-RECORD-COUNT GREATER THAN +16                      EL1322
00991          SUBTRACT +1 FROM PI-AIX-RECORD-COUNT.                    EL1322
00992                                                                   EL1322
00993  4016-NEXT-SENTENCE.                                              EL1322
00994                                                                   EL1322
00995      ADD +1  TO  WS-AIX-RECORD-COUNT                              EL1322
00996                                                                   EL1322
00997      IF (EIBAID EQUAL DFHPF1 OR DFHENTER)                            CL**9
00998         IF OPTION-TWO-SELECTED                                       CL**5
00999            IF PI-LINE-COUNT EQUAL ZERO                               CL**5
01000               PERFORM 5000-MOVE-NAME                                 CL**5
01001               IF CL-INSURED-LAST-NAME EQUAL PI-LAST-NAME             CL**5
01002                  IF PI-LAST-KEY EQUAL                                CL**5
01003                           CL-CONTROL-PRIMARY                         CL**5
01004                                                                   EL1322
01005                     NEXT SENTENCE                                    CL**5
01006                  ELSE                                                CL**5
01007                     GO TO 4010-READNEXT.                             CL**5
01008                                                                   EL1322
01009      MOVE PI-CLAIM-KEY TO WS-KEY-INPUT.                           EL1322
01010                                                                   EL1322
01011      SET KEY-INDEX                                                EL1322
01012          KEY-INDEX2 TO +1.                                        EL1322
01013                                                                   EL1322
01014  4020-COMPARE-KEY.                                                EL1322
01015                                                                   EL1322
01016      IF WS-KH-CHAR (KEY-INDEX) NOT EQUAL WS-KI-CHAR (KEY-INDEX2)  EL1322
01017          GO TO 4700-END-OF-BROWSE.                                EL1322
01018                                                                   EL1322
01019      IF KEY-INDEX LESS THAN PI-KEY-LENGTH                         EL1322
01020          SET KEY-INDEX                                            EL1322
01021              KEY-INDEX2 UP BY +1                                  EL1322
01022          GO TO 4020-COMPARE-KEY.                                  EL1322
01023                                                                      CL**5
01024      IF OPTION-TWO-SELECTED                                          CL**5
01025         IF PI-ACCOUNT-NUMBER NOT EQUAL SPACES                        CL**5
01026            IF PI-ACCOUNT-NUMBER EQUAL CL-CERT-ACCOUNT                CL**5
01027               NEXT SENTENCE                                          CL**5
01028            ELSE                                                      CL**5
01029               GO TO 4010-READNEXT.                                   CL**5
01030 ******************************************************************EL1322
01031 *                                                                *EL1322
01032 *        SECURITY CHECK FOR CARRIER AND ACCOUNT NUMBER           *EL1322
01033 *                      04/04/84                                  *EL1322
01034 *                                                                *EL1322
01035 ******************************************************************EL1322
01036                                                                   EL1322
01037      IF  PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY        EL1322
01038          GO TO 4030-MOVE-DATA.                                       CL*16
01039                                                                   EL1322
01040      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  EL1322
01041          IF  CL-CARRIER = PI-CARRIER-SECURITY NEXT SENTENCE       EL1322
01042             ELSE                                                  EL1322
01043              GO TO 4010-READNEXT.                                 EL1322
01044                                                                   EL1322
01045      IF  PI-ACCOUNT-SECURITY IS GREATER THAN SPACES               EL1322
01046          IF  CL-CERT-ACCOUNT = PI-ACCOUNT-SECURITY NEXT SENTENCE  EL1322
01047             ELSE                                                  EL1322
01048              GO TO 4010-READNEXT.                                 EL1322
01049                                                                   EL1322
01050  4030-MOVE-DATA.                                                     CL*16
01051                                                                   EL1322
01052      MOVE +1                     TO WS-CLAIMS-SW.                 EL1322
01053                                                                   EL1322
01054      MOVE WS-KEY-INPUT TO PI-LAST-KEY.                            EL1322
01055                                                                   EL1322
01056      ADD +1  TO  PI-LINE-COUNT                                    EL1322
01057                  PI-AIX-RECORD-COUNT.                                CL*13
01058                                                                   EL1322
01059      PERFORM 5000-MOVE-NAME.                                         CL*13
01060                                                                      CL*13
01061      MOVE CL-CONTROL-PRIMARY   TO PI-LAST-KEY.                       CL*13
01062      MOVE CL-INSURED-LAST-NAME TO PI-LAST-NAME.                   EL1322
01063      MOVE WS-NAME-WORK         TO EL132B-NAME (EL132B-INDEX).        CL*13
01064                                                                   EL1322
01065      IF CL-INSURED-BIRTH-DT NOT EQUAL TO LOW-VALUES               EL1322
01066          MOVE CL-INSURED-BIRTH-DT    TO  DC-BIN-DATE-1            EL1322
01067          MOVE WS-CURRENT-DATE        TO  DC-BIN-DATE-2            EL1322
01068          MOVE '1'                    TO  DC-OPTION-CODE           EL1322
01069          PERFORM 8500-DATE-CONVERSION                             EL1322
01070          DIVIDE DC-ELAPSED-MONTHS BY +12 GIVING EL132B-AGE        EL1322
01071                                                    (EL132B-INDEX).EL1322
01072                                                                   EL1322
01073      MOVE CL-CLAIM-STATUS      TO  EL132B-STA (EL132B-INDEX).        CL*13
01074                                                                      CL**4
01075      IF CL-PURGED-DT NOT EQUAL LOW-VALUES                            CL**4
01076         MOVE 'P'               TO  EL132B-STA (EL132B-INDEX).        CL**4
01077                                                                   EL1322
01078      IF CL-INCURRED-DT NOT EQUAL TO LOW-VALUES                    EL1322
01079          MOVE SPACES             TO  DC-OPTION-CODE               EL1322
01080          MOVE CL-INCURRED-DT     TO  DC-BIN-DATE-1                EL1322
01081          PERFORM 8500-DATE-CONVERSION                             EL1322
01082          MOVE DC-GREG-DATE-1-EDIT  TO  EL132B-DATE-INCURRED       EL1322
01083                                                    (EL132B-INDEX).EL1322
01084                                                                   EL1322
01085      MOVE CL-CLAIM-TYPE        TO  EL132B-TYPE    (EL132B-INDEX).    CL*13
01086      MOVE CL-CARRIER           TO  EL132B-CARRIER (EL132B-INDEX).    CL*13
01087      MOVE CL-CLAIM-NO          TO  EL132B-CLAIM   (EL132B-INDEX).    CL*13
01088                                                                   EL1322
01089      IF CREDIT-CARD-INDEX                                            CL*13
01090          MOVE CL-CCN-A5        TO  EL132B-CERT-NO (EL132B-INDEX)     CL*13
01091        ELSE                                                          CL*13
01092          MOVE CL-CERT-NO       TO  EL132B-CERT-NO (EL132B-INDEX).    CL*13
01093                                                                   EL1322
01094      MOVE CL-CERT-NO           TO  PI-KEEP-CERT-NO (PI-K-INDEX).     CL*13
01095                                                                   EL1322
01096      MOVE CL-CERT-ACCOUNT      TO  EL132B-ACCOUNT (EL132B-INDEX)     CL*13
01097                                                                      CL*13
01098      SET PI-INDEX  TO  EL132B-INDEX.                                 CL*13
01099                                                                      CL*13
01100      MOVE CL-CERT-GROUPING  TO  PI-SA-GROUP    (PI-INDEX).           CL*13
01101      MOVE CL-CERT-STATE     TO  PI-SA-STATE    (PI-INDEX).           CL*13
01102      MOVE CL-CERT-EFF-DT    TO  PI-SA-EFF-DATE (PI-INDEX).           CL*13
01103                                                                      CL*13
01104      PERFORM 5200-SET-ATTRB.                                         CL*13
01105                                                                   EL1322
01106      IF CL-CERT-NO EQUAL TO CL-PRIME-CERT-NO AND                     CL**3
01107         CL-ASSOC-CERT-TOTAL GREATER THAN +1                          CL**3
01108         MOVE AL-SABON     TO EL132B-NAME-ATTRB (EL132B-INDEX)        CL**3
01109         ELSE                                                         CL**3
01110         MOVE AL-SANON     TO EL132B-NAME-ATTRB (EL132B-INDEX).       CL**3
01111                                                                      CL**3
01112      IF EL132B-INDEX LESS THAN +16                                EL1322
01113          SET EL132B-INDEX                                            CL*13
01114              PI-K-INDEX  UP BY +1                                    CL*13
01115          GO TO 4010-READNEXT.                                     EL1322
01116                                                                   EL1322
01117      GO TO 4900-ENDBROWSE.                                        EL1322
01118                                                                   EL1322
01119  4600-ENDFILE.                                                    EL1322
01120                                                                   EL1322
01121      MOVE ER-0130                   TO  EMI-ERROR.                EL1322
01122      ADD +1  TO  PI-END-OF-FILE.                                  EL1322
01123                                                                   EL1322
01124  4700-END-OF-BROWSE.                                              EL1322
01125                                                                   EL1322
01126      ADD +1  TO  PI-END-OF-FILE.                                  EL1322
01127                                                                   EL1322
01128      EXEC CICS ENDBR                                              EL1322
01129          DATASET (PI-DSID)                                           CL**7
01130      END-EXEC.                                                       CL**7
01131                                                                      CL**8
01132      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')                   CL**8
01133          IF PI-SAVED-PROGRAM-1 IS EQUAL TO 'EL130'                   CL*13
01134              GO TO 4900-ENDBROWSE.                                   CL*13
01135                                                                   EL1322
01136      IF PI-LINE-COUNT EQUAL TO +1                                 EL1322
01137        AND PI-1ST-TIME-SW EQUAL TO ZERO                           EL1322
01138          NEXT SENTENCE                                            EL1322
01139        ELSE                                                       EL1322
01140          GO TO 4900-ENDBROWSE.                                       CL*13
01141                                                                   EL1322
01142      IF PI-DSID (1:6) = 'ELRETR'                                     CL*13
01143          GO TO 4900-ENDBROWSE.                                       CL*13
01144                                                                   EL1322
01145      MOVE EL132B-CARRIER (1)     TO  PI-CARRIER.                     CL*13
01146      MOVE EL132B-CLAIM   (1)     TO  PI-CLAIM-NO.                    CL*13
01147      MOVE PI-KEEP-CERT-NO (1)    TO  PI-CERT-NO.                     CL*13
01148      MOVE EL132B-CERT-NO (1)     TO  PI-CCN-NO.                      CL*13
01149      MOVE EL132B-ACCOUNT (1)     TO  PI-ACCOUNT.                     CL*13
01150                                                                   EL1322
01151      MOVE PI-SA-GROUP (1)        TO  PI-GROUPING.                    CL*13
01152      MOVE PI-SA-STATE (1)        TO  PI-STATE.                       CL*13
01153      MOVE PI-SA-EFF-DATE (1)     TO  PI-CERT-EFF-DT.                 CL*13
01154                                                                      CL*13
01155      MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM.               CL*13
01156      MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM           EL1322
01157                                    THIS-PGM.                         CL*13
01158      MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1.               CL*13
01159      MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2.               CL*13
01160      MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3.               CL*13
01161      MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4.               CL*13
01162      MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5.               CL*13
01163      MOVE SPACES               TO  PI-SAVED-PROGRAM-6.               CL*13
01164                                                                   EL1322
01165      IF THIS-PGM = 'EL126' OR                                        CL*13
01166                    'EL127' OR                                        CL*13
01167                    'EL1275'                                          CL*13
01168          MOVE 'EL150'            TO  THIS-PGM.                       CL*13
01169                                                                   EL1322
01170      GO TO 9300-XCTL.                                                CL**7
01171                                                                   EL1322
01172  4900-ENDBROWSE.                                                  EL1322
01173                                                                   EL1322
01174 ******************************************************************EL1322
01175 *                                                                *EL1322
01176 *        IF THERE ARE NO CLAIM RECORDS FOUND DURING              *EL1322
01177 *        THE INITIAL ENTRY OF EL1322, RETURN TO THE CALLING      *   CL*13
01178 *        PROGRAM.                                                *EL1322
01179 *                                                                *EL1322
01180 ******************************************************************EL1322
01181                                                                   EL1322
01182      IF  WS-NO-CLAIMS-FOUND                                       EL1322
01183          MOVE +9                 TO  PI-BROWSE-SW                 EL1322
01184          GO TO 9400-CLEAR.                                           CL**7
01185                                                                      CL*13
01186      ADD +1 TO PI-SCREEN-COUNT.                                   EL1322
01187                                                                   EL1322
01188      MOVE +1                     TO  PI-1ST-TIME-SW               EL1322
01189                                                                   EL1322
01190      MOVE -1                     TO  BSELL                        EL1322
01191      GO TO 8100-SEND-INITIAL-MAP.                                    CL**7
01192                                                                   EL1322
01193      EJECT                                                        EL1322
uktdel*5000-MOVE-NAME SECTION. COPY ELCMNS.                             EL1322
uktins 5000-MOVE-NAME SECTION.
uktins     COPY ELCMNS.
01195                                                                      CL*13
01196      EJECT                                                           CL*13
01197  5000R-MOVE-NAME SECTION.                                            CL*13
01198      COPY ELCMNR.                                                    CL*13
01199                                                                   EL1322
01200      EJECT                                                        EL1322
01201  5200-SET-ATTRB SECTION.                                          EL1322
01202                                                                   EL1322
01203      MOVE AL-SANON  TO  EL132B-NAME-ATTRB    (EL132B-INDEX)          CL**3
01204                         EL132B-AGE-ATTRB     (EL132B-INDEX)       EL1322
01205                         EL132B-STA-ATTRB     (EL132B-INDEX)          CL**5
01206                         EL132B-DATE-INCURRED-ATTRB (EL132B-INDEX) EL1322
01207                         EL132B-TYPE-ATTRB    (EL132B-INDEX)       EL1322
01208                         EL132B-CARRIER-ATTRB (EL132B-INDEX)       EL1322
01209                         EL132B-CLAIM-ATTRB   (EL132B-INDEX)       EL1322
01210                         EL132B-CERT-NO-ATTRB (EL132B-INDEX)       EL1322
01211                         EL132B-ACCOUNT-ATTRB (EL132B-INDEX).      EL1322
01212                                                                   EL1322
01213  5200-EXIT.                                                       EL1322
01214                                                                   EL1322
01215      EXIT.                                                        EL1322
01216      EJECT                                                        EL1322
01217  7000-PF2-POSITION         SECTION.                               EL1322
01218      EXEC CICS IGNORE CONDITION                                   EL1322
01219           DUPKEY                                                  EL1322
01220      END-EXEC.                                                       CL*13
01221                                                                      CL*13
01222      EXEC CICS HANDLE CONDITION                                   EL1322
01223          NOTFND (8700-NOT-FOUND)                                  EL1322
01224      END-EXEC.                                                       CL*13
01225                                                                      CL*13
01226      COMPUTE WS-CALC-RDNXT = PI-SCREEN-COUNT * 15.                EL1322
01227                                                                      CL*13
01228      MOVE PI-1ST-KEY    TO PI-CLAIM-KEY.                             CL*13
01229      MOVE ZERO          TO PI-END-OF-FILE.                           CL*13
01230                                                                      CL*13
01231      EXEC CICS STARTBR                                            EL1322
01232           DATASET   (PI-DSID)                                     EL1322
01233           RIDFLD    (PI-CLAIM-KEY)                                EL1322
01234           GENERIC                                                    CL**7
01235           EQUAL                                                      CL**7
01236           KEYLENGTH (PI-KEY-LENGTH)                                  CL**7
01237       END-EXEC.                                                      CL**7
01238                                                                   EL1322
01239  7100-READNEXT-PF2.                                               EL1322
01240                                                                   EL1322
01241      IF WS-CALC-RDNXT GREATER THAN ZERO                           EL1322
01242          NEXT SENTENCE                                            EL1322
01243        ELSE                                                       EL1322
01244          GO TO 7199-EXIT.                                            CL**9
01245                                                                      CL*13
01246      EXEC CICS READNEXT                                           EL1322
01247          DATASET (PI-DSID)                                        EL1322
01248          RIDFLD  (PI-CLAIM-KEY)                                   EL1322
01249          SET     (ADDRESS OF CLAIM-MASTER)                           CL*13
01250      END-EXEC.                                                       CL**7
01251                                                                      CL**7
01252 ******************************************************************EL1322
01253 *    IF THE SECURITY CHECKING ROUTINE OR THE INITIAL CHECKING    *EL1322
01254 *        IS CHANGED HERE, YOU                                    *EL1322
01255 *        MUST ALSO CHANGE THE CORRESPONDING ROUTINE IN           *EL1322
01256 *        PARAGRAPH 4020-COMPARE-KEY OR 4030-CHECK-OPTION.        *EL1322
01257 *                                     KER/080884.                *EL1322
01258 ******************************************************************EL1322
01259                                                                      CL*13
01260      IF PI-COMPANY-ID = 'DMD'                                        CL*13
01261         IF CL-LAST-CLOSE-REASON = 'C' OR 'E'                         CL*13
01262             GO TO 7100-READNEXT-PF2.                                 CL*13
01263                                                                      CL*13
01264      IF  PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY        EL1322
01265          GO TO 7130-CHECK-INITIAL.                                   CL*13
01266                                                                   EL1322
01267      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  EL1322
01268          IF  CL-CARRIER = PI-CARRIER-SECURITY NEXT SENTENCE       EL1322
01269             ELSE                                                  EL1322
01270              GO TO 7100-READNEXT-PF2.                             EL1322
01271                                                                   EL1322
01272      IF  PI-ACCOUNT-SECURITY GREATER THAN SPACES                  EL1322
01273          IF  CL-CERT-ACCOUNT = PI-ACCOUNT-SECURITY NEXT SENTENCE  EL1322
01274             ELSE                                                  EL1322
01275              GO TO 7100-READNEXT-PF2.                             EL1322
01276                                                                   EL1322
01277  7130-CHECK-INITIAL.                                                 CL*13
01278      IF PI-OPTION NOT EQUAL TO '2'                                EL1322
01279          GO TO 7190-COMPUTE.                                      EL1322
01280                                                                   EL1322
01281      IF PI-SC-FIRST-NAME IS EQUAL TO SPACES                          CL**9
01282          GO TO 7190-COMPUTE.                                         CL**9
01283                                                                      CL**9
01284      MOVE PI-SC-FIRST-NAME       TO  WS-PI-NAME.                     CL**9
01285      MOVE CL-INSURED-1ST-NAME    TO  WS-CL-NAME.                     CL**9
01286      MOVE SPACE                  TO  WS-COMPARE-INDICATOR.           CL**9
01287                                                                      CL**9
01288      PERFORM 7200-CHECK-NAME THRU 7200-EXIT                          CL**9
01289          VARYING WS-NAME-INDEX FROM 15 BY -1                         CL**9
01290              UNTIL WS-NAME-INDEX IS EQUAL TO +0.                     CL**9
01291                                                                      CL**9
01292      IF NAME-NOT-FOUND                                               CL**9
01293          GO TO 7100-READNEXT-PF2.                                    CL**9
01294                                                                   EL1322
01295      IF PI-SC-INITIAL2 NOT EQUAL TO SPACES                        EL1322
01296          IF PI-SC-INITIAL2 NOT EQUAL TO CL-INSURED-MID-INIT       EL1322
01297              GO TO 7100-READNEXT-PF2.                             EL1322
01298                                                                   EL1322
01299  7190-COMPUTE.                                                    EL1322
01300      COMPUTE WS-CALC-RDNXT = WS-CALC-RDNXT - 1.                   EL1322
01301      GO TO 7100-READNEXT-PF2.                                     EL1322
01302                                                                      CL**9
01303  7199-EXIT.                                                          CL**9
01304      EXIT.                                                        EL1322
01305                                                                      CL**9
01306  7200-CHECK-NAME  SECTION.                                           CL**9
01307                                                                      CL**9
01308      IF WS-PI-NAME-CHAR (WS-NAME-INDEX) IS NOT EQUAL TO ' ' AND      CL**9
01309         WS-CL-NAME-CHAR (WS-NAME-INDEX)                              CL**9
01310          MOVE 'X'                TO  WS-COMPARE-INDICATOR.           CL**9
01311                                                                      CL**9
01312  7200-EXIT.                                                          CL**9
01313      EXIT.                                                           CL**9
01314                                                                      CL**9
01315      EJECT                                                        EL1322
01316  8100-SEND-INITIAL-MAP SECTION.                                   EL1322
01317                                                                      CL*13
01318      IF CREDIT-CARD-INDEX                                            CL*13
01319          MOVE 'CREDIT CARD #'    TO  BCERTO.                         CL*13
01320                                                                   EL1322
01321      MOVE SAVE-DATE              TO  ADATEO.                      EL1322
01322      MOVE EIBTIME                TO  TIME-IN.                        CL**7
01323      MOVE TIME-OUT               TO  ATIMEO.                         CL**7
101501     MOVE PI-COMPANY-ID          TO  ACOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  AUSERIDO.
01324                                                                   EL1322
01325      IF PI-SAVED-PROGRAM-1 = 'EL126' OR 'EL150'                      CL*13
01326          MOVE '       CLAIM LOOK-UP FOR STATUS  '                    CL*13
01327                                  TO  BHEAD1O                      EL1322
01328        ELSE                                                       EL1322
01329          IF PI-SAVED-PROGRAM-1 = 'EL162'                             CL*13
01330              MOVE '   CLAIM LOOK-UP FOR MAIL RECORDING   '        EL1322
01331                                  TO  BHEAD1O                      EL1322
01332            ELSE                                                   EL1322
01333              IF PI-SAVED-PROGRAM-1 = 'EL130'                         CL*13
01334                  MOVE '  CLAIM LOOK-UP FROM NEW CLAIM SETUP  '    EL1322
01335                                  TO  BHEAD1O.                     EL1322
01336                                                                      CL*13
01337      IF PI-DSID (1:6) = ELRETR-FILE-ID                               CL*13
01338          MOVE '    RETRIEVE LOOK-UP FOR STATUS  '                    CL*13
01339                                  TO BHEAD1O                          CL*13
01340          MOVE AL-PABON           TO BPFK7A.                          CL*13
01341                                                                   EL1322
01342      IF EMI-ERROR NOT EQUAL TO ZERO                               EL1322
01343          PERFORM 9900-ERROR-FORMAT.                               EL1322
01344                                                                   EL1322
01345      MOVE EMI-MESSAGE-AREA (1)    TO  BEMSG1O.                    EL1322
01346                                                                   EL1322
101501*    IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES                   EL1322
101501*       MOVE AL-PABON TO BPFK5A BPFK6A                            EL1322
101501*       MOVE PI-COMPANY-ID TO BCOMPO                              EL1322
101501*    ELSE                                                         EL1322
101501*       MOVE SPACES TO BCOMPO                                     EL1322
101501*       MOVE AL-PADOF TO BPFK5A BPFK6A.                           EL1322
01353                                                                   EL1322
01354                                                                   EL1322
01355      EXEC CICS SEND                                               EL1322
01356          FROM   (EL132BI)                                         EL1322
01357          MAPSET (WS-MAPSET-NAME)                                  EL1322
01358          MAP    (WS-MAP-NAME)                                     EL1322
01359          CURSOR                                                      CL**7
01360          ERASE                                                       CL**7
01361      END-EXEC.                                                       CL**7
01362                                                                   EL1322
01363      GO TO 9100-RETURN-TRAN.                                         CL**7
01364                                                                   EL1322
01365  8100-EXIT.                                                       EL1322
01366      EXIT.                                                        EL1322
01367                                                                   EL1322
01368      EJECT                                                        EL1322
01369  8200-SEND-DATAONLY SECTION.                                      EL1322
01370                                                                   EL1322
01371      MOVE SAVE-DATE              TO  ADATEO.                      EL1322
01372      MOVE EIBTIME                TO  TIME-IN.                        CL**7
01373      MOVE TIME-OUT               TO  ATIMEO.                         CL**7
101501     MOVE PI-COMPANY-ID          TO  ACOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  AUSERIDO.
01374                                                                   EL1322
01375      IF EMI-ERROR NOT EQUAL TO ZERO                               EL1322
01376          PERFORM 9900-ERROR-FORMAT.                               EL1322
01377                                                                   EL1322
01378      MOVE EMI-MESSAGE-AREA (1)    TO  BEMSG1O.                    EL1322
01379                                                                   EL1322
101501*    IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES                   EL1322
101501*       MOVE AL-PABON TO BPFK5A BPFK6A                            EL1322
101501*       MOVE PI-COMPANY-ID TO BCOMPO                              EL1322
101501*    ELSE                                                         EL1322
101501*       MOVE SPACES TO BCOMPO                                     EL1322
101501*       MOVE AL-PADOF TO BPFK5A BPFK6A.                           EL1322
01386                                                                   EL1322
01387      EXEC CICS SEND DATAONLY                                      EL1322
01388          FROM   (EL132BI)                                         EL1322
01389          MAPSET (WS-MAPSET-NAME)                                  EL1322
01390          MAP    (WS-MAP-NAME)                                     EL1322
01391          CURSOR                                                      CL**7
01392      END-EXEC.                                                       CL**7
01393                                                                   EL1322
01394      GO TO 9100-RETURN-TRAN.                                         CL**7
01395                                                                   EL1322
01396  8200-EXIT.                                                       EL1322
01397      EXIT.                                                        EL1322
01398                                                                   EL1322
01399      EJECT                                                        EL1322
01400  8300-SEND-TEXT SECTION.                                          EL1322
01401                                                                   EL1322
01402      EXEC CICS SEND TEXT                                          EL1322
01403          FROM   (LOGOFF-TEXT)                                     EL1322
01404          LENGTH (LOGOFF-LENGTH)                                   EL1322
01405          ERASE                                                       CL**7
01406          FREEKB                                                      CL**7
01407      END-EXEC.                                                       CL**7
01408                                                                   EL1322
01409      EXEC CICS RETURN                                             EL1322
01410          END-EXEC.                                                EL1322
01411                                                                   EL1322
01412  8300-EXIT.                                                       EL1322
01413      EXIT.                                                        EL1322
01414                                                                   EL1322
01415      EJECT                                                        EL1322
01416  8500-DATE-CONVERSION SECTION.                                    EL1322
01417                                                                   EL1322
01418      EXEC CICS LINK                                               EL1322
01419          PROGRAM  ('ELDATCV')                                     EL1322
01420          COMMAREA (DATE-CONVERSION-DATA)                          EL1322
01421          LENGTH   (DC-COMM-LENGTH)                                   CL**7
01422      END-EXEC.                                                       CL**7
01423                                                                   EL1322
01424  8500-EXIT.                                                       EL1322
01425      EXIT.                                                        EL1322
01426                                                                   EL1322
01427      EJECT                                                        EL1322
01428  8600-NEXT-COMPANY SECTION.                                       EL1322
01429 ******************************************************************   CL**7
01430 ****      READ THE CURRENT COMPANY RECORD TO OBTAIN THE       ****   CL**7
01431 ****      NEXT COMPANY ID.                                    ****   CL**7
01432 ******************************************************************   CL**7
01433                                                                   EL1322
01434      MOVE SPACES                     TO  WS-ELCNTL-KEY.              CL*13
01435      MOVE PI-COMPANY-ID              TO  WS-ELCNTL-ID.               CL*13
01436      MOVE '1'                        TO  WS-ELCNTL-TYPE.             CL*13
01437      MOVE +0                         TO  WS-ELCNTL-SEQ.              CL*13
01438                                                                   EL1322
01439      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.                       CL**7
01440                                                                   EL1322
01441      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**7
01442          PERFORM 8800-INITIALIZE-MAP VARYING EL132B-INDEX            CL**7
01443            FROM +1 BY +1 UNTIL EL132B-INDEX IS GREATER THAN +16      CL**7
01444          MOVE ER-0022                 TO  EMI-ERROR                  CL**7
01445          MOVE -1                      TO  BSELL                      CL**7
01446          GO TO 8100-SEND-INITIAL-MAP.                                CL**7
01447                                                                   EL1322
01448      IF EIBAID = DFHPF5                                           EL1322
01449          MOVE CF-NEXT-COMPANY-ID      TO  WS-NEXT-COMPANY-ID.        CL**7
01450                                                                   EL1322
01451      IF EIBAID = DFHPF6                                           EL1322
01452          MOVE PI-ORIGINAL-COMPANY-ID  TO  WS-NEXT-COMPANY-ID.        CL**7
01453                                                                   EL1322
01454      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                        EL1322
01455          GO TO 8600-CONTINUE-NEXT-COMPANY.                           CL**7
01456                                                                      CL**7
01457 ******************************************************************   CL**7
01458 ****      READ THE CURRENT USER RECORD FOR UPDATE AND REMOVE  ****   CL**7
01459 ****      THE TERMINAL ID FROM THE RECORD.                    ****   CL**7
01460 ******************************************************************   CL**7
01461                                                                      CL**7
01462      MOVE PI-COMPANY-ID               TO  WS-ELCNTL-ID.              CL*13
01463      MOVE '2'                         TO  WS-ELCNTL-TYPE.            CL*13
01464      MOVE PI-PROCESSOR-ID             TO  WS-ELCNTL-USER.            CL*13
01465      MOVE +0                          TO  WS-ELCNTL-SEQ.             CL*13
01466                                                                      CL**7
01467      PERFORM 8910-READ-CONTROL-UPDATE THRU 8910-EXIT.                CL**7
01468                                                                      CL**7
01469      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**7
01470          MOVE ER-0019                 TO  EMI-ERROR                  CL**7
01471          MOVE -1                      TO  BSELL                      CL**7
01472          GO TO 8200-SEND-DATAONLY.                                   CL**7
01473                                                                      CL**7
01474      MOVE SPACES                      TO  CF-CURRENT-TERM-ON.        CL**7
01475                                                                      CL**7
01476      PERFORM 8920-REWRITE-CONTROL THRU 8920-EXIT.                    CL**7
01477                                                                      CL**7
01478 ******************************************************************   CL**7
01479 ****      READ THE USER RECORD ON THE "NEXT" COMPANY TO       ****   CL**7
01480 ****      VERIFY THAT A VALID USER RECORD EXISTS:             ****   CL**7
01481 ****        1.  MOVE USER CARRIER/ACCOUNT SECURITY TO PI-AREA ****   CL**7
01482 ****        2.  MOVE USER SECURITY VALUES TO SECURITY CODES   ****   CL**7
01483 ****            IN WORKING STORAGE.                           ****   CL**7
01484 ******************************************************************   CL**7
01485                                                                      CL**7
01486      MOVE WS-NEXT-COMPANY-ID         TO  WS-ELCNTL-ID.               CL*13
01487      MOVE '2'                        TO  WS-ELCNTL-TYPE.             CL*13
01488      MOVE PI-PROCESSOR-ID            TO  WS-ELCNTL-USER.             CL*13
01489      MOVE +0                         TO  WS-ELCNTL-SEQ.              CL*13
01490                                                                   EL1322
01491      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.                       CL**7
01492                                                                   EL1322
01493      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**7
01494          PERFORM 8800-INITIALIZE-MAP VARYING EL132B-INDEX            CL**7
01495             FROM +1 BY +1 UNTIL EL132B-INDEX IS GREATER THAN +16     CL**7
01496          MOVE ER-0228                TO  EMI-ERROR                   CL**7
01497          MOVE -1                     TO  BSELL                       CL**7
01498          GO TO 8100-SEND-INITIAL-MAP.                                CL**7
01499                                                                   EL1322
01500      MOVE CF-PROCESSOR-CARRIER       TO  PI-CARRIER-SECURITY.        CL**7
01501      MOVE CF-PROCESSOR-ACCOUNT       TO  PI-ACCOUNT-SECURITY.        CL**7
01502      MOVE CF-INDIVIDUAL-APP (1)      TO  SC-CREDIT-CODES.            CL**7
01503      MOVE CF-INDIVIDUAL-APP (2)      TO  SC-CLAIMS-CODES.            CL**7
01504      MOVE SC-CLAIMS-DISPLAY (21)     TO  PI-DISPLAY-CAP.             CL**7
01505      MOVE SC-CLAIMS-UPDATE  (21)     TO  PI-MODIFY-CAP.              CL**7
01506                                                                   EL1322
01507 ******************************************************************   CL**7
01508 ****      READ THE USER RECORD ON THE "NEXT" COMPANY FOR      ****   CL**7
01509 ****      UPDATE AND MOVE THE TERMINAL ID INTO THE RECORD.    ****   CL**7
01510 ******************************************************************   CL**7
01511                                                                   EL1322
01512      MOVE WS-NEXT-COMPANY-ID         TO  WS-ELCNTL-ID.               CL*13
01513      MOVE '2'                        TO  WS-ELCNTL-TYPE.             CL*13
01514      MOVE PI-PROCESSOR-ID            TO  WS-ELCNTL-USER.             CL*13
01515      MOVE +0                         TO  WS-ELCNTL-SEQ.              CL*13
01516                                                                      CL**7
01517      PERFORM 8910-READ-CONTROL-UPDATE THRU 8910-EXIT.                CL**7
01518                                                                      CL**7
01519      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**7
01520          MOVE ER-0228                TO  EMI-ERROR                   CL**7
01521          MOVE -1                     TO  BSELL                       CL**7
01522          GO TO 8200-SEND-DATAONLY.                                   CL**7
01523                                                                      CL**7
01524      MOVE EIBTRMID                   TO  CF-CURRENT-TERM-ON.         CL**7
01525                                                                      CL**7
01526      PERFORM 8920-REWRITE-CONTROL THRU 8920-EXIT.                    CL**7
01527                                                                      CL**7
01528  8600-CONTINUE-NEXT-COMPANY.                                         CL**7
01529 ******************************************************************   CL**7
01530 ****      READ THE NEW COMPANY RECORD TO VERIFY THAT IT       ****   CL**7
01531 ****      EXISTS AND THEN MOVE SPECIFIC DATA TO PI-AREA.      ****   CL**7
01532 ******************************************************************   CL**7
01533                                                                      CL**7
01534      MOVE SPACES                     TO  WS-ELCNTL-KEY.              CL*13
01535      MOVE WS-NEXT-COMPANY-ID         TO  WS-ELCNTL-ID.               CL*13
01536      MOVE '1'                        TO  WS-ELCNTL-TYPE.             CL*13
01537      MOVE +0                         TO  WS-ELCNTL-SEQ.              CL*13
01538                                                                      CL**7
01539      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.                       CL**7
01540                                                                      CL**7
01541      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**7
01542          PERFORM 8800-INITIALIZE-MAP VARYING EL132B-INDEX            CL**7
01543             FROM +1 BY +1 UNTIL EL132B-INDEX IS GREATER THAN +16     CL**7
01544          MOVE ER-0089                TO  EMI-ERROR                   CL**7
01545          MOVE -1                     TO  BSELL                       CL**7
01546          GO TO 8100-SEND-INITIAL-MAP.                                CL**7
01547                                                                      CL**7
01548      MOVE SPACES                     TO  PI-PROGRAM-WORK-AREA.       CL**7
01549                                                                      CL**7
01550      MOVE +1                         TO  PI-START-SW                 CL**7
01551                                          PI-KEY-LENGTH.              CL**7
01552                                                                      CL**7
01553      MOVE +0                         TO  PI-1ST-TIME-SW              CL**7
01554                                          PI-LINE-COUNT               CL**7
01555                                          PI-AIX-RECORD-COUNT         CL**7
01556                                          PI-BROWSE-SW                CL**7
01557                                          PI-END-OF-FILE              CL**7
01558                                          PI-TS-ITEM                  CL**7
01559                                          PI-SCREEN-COUNT.            CL**7
01560                                                                      CL**7
01561      MOVE 'ELMSTR  '                 TO  PI-DSID.                    CL**7
01562      MOVE CF-COMPANY-CD              TO  PI-COMPANY-CD            EL1322
01563                                          PI-CK-COMPANY-CD            CL**7
01564                                          PI-1ST-KEY.                 CL**7
01565      MOVE CF-COMPANY-ID              TO  PI-COMPANY-ID.           EL1322
01566      MOVE CF-COMPANY-PASSWORD        TO  PI-COMPANY-PASSWORD.        CL**7
01567      MOVE CF-LGX-CREDIT-USER         TO  PI-CREDIT-USER.          EL1322
01568      MOVE CF-LGX-CLAIM-USER          TO  PI-CLAIM-USER.           EL1322
01569      MOVE CF-CERT-ACCESS-CONTROL     TO  PI-CERT-ACCESS-CONTROL.  EL1322
01570      MOVE CF-CARRIER-CONTROL-LEVEL   TO  PI-CARRIER-CONTROL-LEVEL.EL1322
01571      MOVE CF-JOURNAL-FILE-ID         TO  PI-JOURNAL-FILE-ID.      EL1322
01572      MOVE CF-LOWER-CASE-LETTERS      TO  PI-LOWER-CASE-LETTERS.      CL**7
01573      MOVE CF-CLAIM-PAID-THRU-TO      TO  PI-CLAIM-PAID-THRU-TO.      CL**7
01574                                                                   EL1322
01575      MOVE CF-LIFE-OVERRIDE-L1        TO  PI-LIFE-OVERRIDE-L1.        CL**7
01576      MOVE CF-LIFE-OVERRIDE-L2        TO  PI-LIFE-OVERRIDE-L2.        CL**7
01577      MOVE CF-LIFE-OVERRIDE-L6        TO  PI-LIFE-OVERRIDE-L6.        CL**7
01578      MOVE CF-LIFE-OVERRIDE-L12       TO  PI-LIFE-OVERRIDE-L12.       CL**7
01579                                                                   EL1322
01580      MOVE CF-AH-OVERRIDE-L1          TO  PI-AH-OVERRIDE-L1.          CL**7
01581      MOVE CF-AH-OVERRIDE-L2          TO  PI-AH-OVERRIDE-L2.          CL**7
01582      MOVE CF-AH-OVERRIDE-L6          TO  PI-AH-OVERRIDE-L6.          CL**7
01583      MOVE CF-AH-OVERRIDE-L12         TO  PI-AH-OVERRIDE-L12.         CL**7
01584                                                                      CL**9
01585      IF  CLAIM-SESSION                                               CL**9
01586          MOVE CF-PRINT-ADDRESS-LABELS                                CL**9
01587                                      TO  PI-LABEL-CONTROL            CL**9
01588      ELSE                                                            CL**9
01589          IF  CREDIT-SESSION                                          CL**9
01590              MOVE CF-CR-PRINT-ADDRESS-LABELS                         CL**9
01591                                      TO  PI-LABEL-CONTROL.           CL**9
01592                                                                   EL1322
01593  8600-EXIT.                                                       EL1322
01594      EXIT.                                                        EL1322
01595                                                                      CL**7
01596      EJECT                                                        EL1322
01597  8650-WRITE-SECURITY-TEMP-STORE  SECTION.                         EL1322
01598                                                                   EL1322
01599      EXEC CICS HANDLE CONDITION                                   EL1322
01600          QIDERR  (8651-WRITE-SECURITY)                            EL1322
01601      END-EXEC.                                                    EL1322
01602                                                                   EL1322
01603      MOVE EIBTRMID               TO  QID.                         EL1322
01604                                                                   EL1322
01605      EXEC CICS DELETEQ TS                                         EL1322
01606          QUEUE   (QID)                                            EL1322
01607      END-EXEC.                                                    EL1322
01608                                                                   EL1322
01609  8651-WRITE-SECURITY.                                             EL1322
01610                                                                   EL1322
01611      EXEC CICS WRITEQ TS                                          EL1322
01612          QUEUE   (QID)                                            EL1322
01613          FROM    (SECURITY-CONTROL)                               EL1322
01614          LENGTH  (SC-COMM-LENGTH)                                 EL1322
01615          ITEM    (QID-ITEM)                                       EL1322
01616      END-EXEC.                                                    EL1322
01617                                                                   EL1322
01618      MOVE QID                    TO  PI-SECURITY-TEMP-STORE-ID.   EL1322
01619                                                                   EL1322
01620      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                        EL1322
01621          MOVE ALL 'Y'            TO  SC-CREDIT-CODES              EL1322
01622                                      SC-CLAIMS-CODES              EL1322
01623                                      PI-PROCESSOR-USER-ALMIGHTY.  EL1322
01624                                                                   EL1322
01625  8650-EXIT.                                                       EL1322
01626      EXIT.                                                        EL1322
01627                                                                   EL1322
01628      EJECT                                                        EL1322
01629  8700-NOT-FOUND SECTION.                                          EL1322
01630      PERFORM 8800-INITIALIZE-MAP VARYING EL132B-INDEX             EL1322
01631        FROM +1 BY +1 UNTIL EL132B-INDEX GREATER THAN +16.         EL1322
01632                                                                      CL*13
01633      MOVE -1 TO BSELL.                                            EL1322
01634      MOVE ER-0284                TO EMI-ERROR.                    EL1322
01635      GO TO 8100-SEND-INITIAL-MAP.                                    CL**7
01636                                                                      CL*13
01637  8700-EXIT.                                                       EL1322
01638      EXIT.                                                        EL1322
01639                                                                   EL1322
01640  8800-INITIALIZE-MAP SECTION.                                     EL1322
01641      MOVE LOW-VALUES TO EL132B-MAP-LINE (EL132B-INDEX).           EL1322
01642  8800-EXIT.                                                       EL1322
01643      EXIT.                                                        EL1322
01644                                                                      CL**7
01645      EJECT                                                        EL1322
01646  8900-READ-CONTROL SECTION.                                          CL**7
01647                                                                   EL1322
01648      EXEC CICS HANDLE CONDITION                                      CL**7
01649          NOTFND   (8900-NOTFND)                                      CL**7
01650      END-EXEC.                                                       CL**7
01651                                                                      CL**7
01652      EXEC CICS READ                                                  CL**7
01653          DATASET   (ELCNTL-FILE-ID)                                  CL**7
01654          RIDFLD    (WS-ELCNTL-KEY)                                   CL*13
01655          SET       (ADDRESS OF CONTROL-FILE)                         CL*13
01656      END-EXEC.                                                       CL**7
01657                                                                      CL**7
01658      MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.           CL**7
01659      GO TO 8900-EXIT.                                                CL**7
01660                                                                      CL**7
01661  8900-NOTFND.                                                        CL**7
01662                                                                      CL**7
01663      MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.           CL**7
01664                                                                      CL**7
01665  8900-EXIT.                                                          CL**7
01666      EXIT.                                                           CL**7
01667                                                                      CL**7
01668  8910-READ-CONTROL-UPDATE.                                           CL**7
01669                                                                      CL**7
01670      EXEC CICS HANDLE CONDITION                                      CL**7
01671          NOTFND   (8910-NOTFND)                                      CL**7
01672      END-EXEC.                                                       CL**7
01673                                                                      CL**7
01674      EXEC CICS READ                                                  CL**7
01675          DATASET   (ELCNTL-FILE-ID)                                  CL**7
01676          RIDFLD    (WS-ELCNTL-KEY)                                   CL*13
01677          SET       (ADDRESS OF CONTROL-FILE)                         CL*13
01678          UPDATE                                                      CL**7
01679      END-EXEC.                                                       CL**7
01680                                                                      CL**7
01681      MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.           CL**7
01682      GO TO 8910-EXIT.                                                CL**7
01683                                                                      CL**7
01684  8910-NOTFND.                                                        CL**7
01685      MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.           CL**7
01686                                                                      CL**7
01687  8910-EXIT.                                                          CL**7
01688      EXIT.                                                           CL**7
01689                                                                      CL**7
01690  8920-REWRITE-CONTROL.                                               CL**7
01691                                                                      CL**7
01692      EXEC CICS REWRITE                                               CL**7
01693          DATASET   (ELCNTL-FILE-ID)                                  CL**7
01694          FROM      (CONTROL-FILE)                                    CL**7
01695      END-EXEC.                                                       CL**7
01696                                                                      CL**7
01697  8920-EXIT.                                                          CL**7
01698      EXIT.                                                           CL**7
01699                                                                      CL**7
01700      EJECT                                                           CL**7
01701  9000-RETURN-CICS SECTION.                                        EL1322
01702                                                                   EL1322
01703      MOVE 'EL005   '             TO  THIS-PGM.                       CL**7
01704      MOVE EIBAID                 TO  PI-ENTRY-CD-1                EL1322
01705      GO TO 9300-XCTL.                                                CL**7
01706                                                                   EL1322
01707  9000-EXIT.                                                       EL1322
01708      EXIT.                                                        EL1322
01709                                                                   EL1322
01710  9100-RETURN-TRAN SECTION.                                        EL1322
01711                                                                   EL1322
01712      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO             EL1322
01713      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO         EL1322
01714      MOVE EIBAID                 TO  PI-LAST-EIBAID               EL1322
01715                                                                   EL1322
01716      EXEC CICS RETURN                                             EL1322
01717          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1322
01718          LENGTH   (PI-COMM-LENGTH)                                EL1322
01719          TRANSID  (WS-TRANS-ID)                                      CL**7
01720      END-EXEC.                                                       CL**7
01721                                                                   EL1322
01722  9100-EXIT.                                                       EL1322
01723      EXIT.                                                        EL1322
01724                                                                   EL1322
01725  9300-XCTL SECTION.                                               EL1322
01726                                                                   EL1322
01727      MOVE DFHENTER               TO  EIBAID                       EL1322
01728                                                                      CL**9
01729      IF THIS-PGM IS EQUAL TO 'EL150'                                 CL**9
01730          MOVE EIBTRMID           TO  WS-TS1-TERM-ID                  CL**9
01731          EXEC CICS WRITEQ TS                                         CL**9
01732              FROM     (PI-PROGRAM-WORK-AREA)                         CL**9
01733              LENGTH   (WS-WORK-LENGTH)                               CL**9
01734              QUEUE    (WS-EL150-TS)                                  CL**9
01735              ITEM     (QID-ITEM)                                     CL**9
01736          END-EXEC.                                                   CL**9
01737                                                                   EL1322
01738      EXEC CICS XCTL                                               EL1322
01739          PROGRAM  (THIS-PGM)                                         CL**7
01740          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1322
01741          LENGTH   (PI-COMM-LENGTH)                                   CL**7
01742      END-EXEC.                                                       CL**7
01743                                                                   EL1322
01744  9300-EXIT.                                                       EL1322
01745      EXIT.                                                        EL1322
01746                                                                   EL1322
01747      EJECT                                                        EL1322
01748  9400-CLEAR SECTION.                                              EL1322
01749                                                                   EL1322
01750      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                        CL**7
01751      GO TO 9300-XCTL.                                                CL**7
01752                                                                   EL1322
01753  9400-EXIT.                                                       EL1322
01754      EXIT.                                                        EL1322
01755                                                                   EL1322
01756  9600-PGMIDERR SECTION.                                           EL1322
01757                                                                   EL1322
01758      EXEC CICS HANDLE CONDITION                                   EL1322
01759          PGMIDERR (8300-SEND-TEXT)                                   CL**7
01760      END-EXEC.                                                       CL**7
01761                                                                      CL**7
01762      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM              CL**7
01763                                                                   EL1322
01764      MOVE 'EL005   '             TO  THIS-PGM                        CL**7
01765                                      LOGOFF-PGM                   EL1322
01766      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL                  EL1322
01767      MOVE SPACES                 TO  PI-ENTRY-CD-1                EL1322
01768      GO TO 9300-XCTL.                                                CL**7
01769                                                                   EL1322
01770  9600-EXIT.                                                       EL1322
01771      EXIT.                                                        EL1322
01772                                                                   EL1322
01773      EJECT                                                        EL1322
01774  9900-ERROR-FORMAT SECTION.                                       EL1322
01775                                                                   EL1322
01776      EXEC CICS LINK                                               EL1322
01777          PROGRAM  ('EL001')                                       EL1322
01778          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL1322
01779          LENGTH   (EMI-COMM-LENGTH)                                  CL**7
01780      END-EXEC.                                                       CL**7
01781                                                                   EL1322
01782      MOVE ER-0000                   TO  EMI-ERROR.                EL1322
01783                                                                   EL1322
01784  9900-EXIT.                                                       EL1322
01785      EXIT.                                                        EL1322
01786                                                                   EL1322
01787  9990-ERROR SECTION.                                              EL1322
01788                                                                   EL1322
01789      MOVE DFHEIBLK TO EMI-LINE1.                                  EL1322
01790      EXEC CICS LINK                                               EL1322
01791          PROGRAM  ('EL004')                                       EL1322
01792          COMMAREA (EMI-LINE1)                                     EL1322
01793          LENGTH   (72)                                               CL**7
01794      END-EXEC.                                                       CL**7
01795                                                                      CL**7
01796      GO TO 8100-SEND-INITIAL-MAP.                                    CL**7
01797                                                                   EL1322
01798  9990-EXIT.                                                       EL1322
01799      EXIT.                                                           CL**7
01800                                                                   EL1322
01801  9995-SECURITY-VIOLATION SECTION.                                    CL**7
01802                                  COPY ELCSCTP.                       CL**7
01803                                                                      CL**7
01804  9995-EXIT.                                                          CL**7
01805      EXIT.                                                        EL1322
01806                                                                   EL1322
