00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL6592
00003  PROGRAM-ID.                 EL6592.                                 LV010
00004 *              PROGRAM CONVERTED BY                                  CL*10
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*10
00006 *              CONVERSION DATE 02/14/96 12:02:41.                    CL*10
00007 *                            VMOD=2.010.                             CL*10
00008 *                                                                 EL6592
00008 *                                                                 EL6592
00009 *AUTHOR.        LOGIC INC.                                           CL*10
00010 *               DALLAS, TEXAS.                                       CL*10
00011                                                                   EL6592
00012 *DATE-COMPILED.                                                      CL*10
00013                                                                   EL6592
00014 *SECURITY.   *****************************************************   CL*10
00015 *            *                                                   *   CL*10
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*10
00017 *            *                                                   *   CL*10
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*10
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*10
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*10
00021 *            *                                                   *   CL*10
00022 *            *****************************************************   CL*10
00023                                                                   EL6592
00024 *REMARKS.                                                         EL6592
00025 *        THIS PROGRAM PROVIDES THE BROWSE NECESSARY FOR THE       EL6592
00026 *    ACCOUNT MASTER (ACCOUNT NAME AND/OR MAIL TO NAME), COMPEN-      CL*10
00027 *    SATION MASTER (ACCOUNT NAME AND/OR MAIL TO NAME), AND THE       CL*10
00028 *    REINSURANCE COMPANY NAME (WITH LEVEL NUMBERS) LOOK-UP.       EL6592
00029                                                                   EL6592
00030 *    SCREENS     - EL659B - ACCOUNT/COMPANY NAME LOOK-UP          EL6592
00031                                                                   EL6592
00032 *    ENTERED BY  - EL659 - ACCOUNT/COMPANY NAME QUALIFICATION     EL6592
00033                                                                   EL6592
00034 *    EXIT TO     - EL659 - ACCOUNT/COMPANY NAME QUALIFICATION     EL6592
00035 *                  EL650 - ACCOUNT MAINTENANCE                    EL6592
00036 *                  EL652 - COMPENSATION MAINTENANCE               EL6592
00037 *                  EL651 - REINSURANCE TABLE MAINTENANCE          EL6592
00038                                                                   EL6592
00039 *    INPUT FILE  - ERNAME - ACCOUNT/COMPANY NAME XREF FILE        EL6592
00040                                                                   EL6592
00041 *    OUTPUT FILE - NONE                                           EL6592
00042                                                                   EL6592
00043 *    COMMAREA    - PASSED.  IF AN ACCOUNT/COMPANY IS SELECTED,    EL6592
00044 *                  THE CONTROL OF THAT ACCOUNT/COMPANY IS PLACED  EL6592
00045 *                  IN THE APPROPRIATE FIELDS OF THE COMMAAREA FOR EL6592
00046 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM EL6592
00047 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE  EL6592
00048 *                  RECORD KEY INFORMATION NEEDED BY EL650, EL651  EL6592
00049 *                  AND/OR EL652 TO LOCATE THAT ACCOUNT/COMPANY.   EL6592
00050 *                                                                 EL6592
00051 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL659.  ON     EL6592
00052 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE EL6592
00053 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVEEL6592
00054 *                  ENTRIES (XCTL FROM CICS VIA EX66) THE SCREEN   EL6592
00055 *                  WILL BE READ AND ACTION WILL BE BASED ON THE   EL6592
00056 *                  MAINTENANCE TYPE INDICATED.                    EL6592
00050 *                                                                 EL6592
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101501*                              ADJUST REDEFINES EL659BI FILLER
101501******************************************************************

00057  EJECT                                                            EL6592
00058  ENVIRONMENT DIVISION.                                            EL6592
00059  DATA DIVISION.                                                   EL6592
00060  WORKING-STORAGE SECTION.                                         EL6592
00061  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.      CL*10
00062  77  FILLER  PIC  X(32) VALUE '********************************'. EL6592
00063  77  FILLER  PIC  X(32) VALUE '*   EL6592 WORKING STORAGE     *'. EL6592
00064  77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.010 *********'.    CL*10
00065                                                                   EL6592
00066      COPY ELCSCTM.                                                   CL**5
00067                                                                   EL6592
00068      COPY ELCSCRTY.                                                  CL**5
00069                                                                   EL6592
00070  01  WS-DATE-AREA.                                                EL6592
00071      12  SAVE-DATE               PIC  X(08)      VALUE SPACES.    EL6592
00072      12  SAVE-BIN-DATE           PIC  X(02)      VALUE SPACES.    EL6592
00073                                                                   EL6592
00074  01  FILLER              COMP-3.                                  EL6592
00075      12  TIME-IN                 PIC S9(07)      VALUE ZERO.      EL6592
00076      12  TIME-OUT  REDEFINES                                      EL6592
00077          TIME-IN                 PIC S9(03)V9(4).                 EL6592
00078      12  WS-NAME-SW              PIC S9(01)      VALUE ZERO.      EL6592
00079          88  WS-NO-NAME-FOUND                    VALUE ZERO.      EL6592
00080      12  SUB                     PIC S9(02)      VALUE ZERO.      EL6592
00081                                                                   EL6592
00082  01  FILLER              COMP.                                    EL6592
00083      12  WS-INDEX                PIC S9(04)      VALUE ZERO.      EL6592
00084      12  WS-TS-LENGTH            PIC S9(04)      VALUE +1920.     EL6592
00085      12  SC-ITEM                 PIC S9(04)      VALUE +0001.     EL6592
00086                                                                   EL6592
00087  01  FILLER.                                                      EL6592
00088      12  QID.                                                     EL6592
00089          16  QID-TERM                PIC  X(04).                  EL6592
00090          16  FILLER                  PIC  X(04)  VALUE '659B'.    EL6592
00091      12  QID-PROC-AREA               PIC  X(03).                  EL6592
00092      12  QID-LENGTH                  PIC S9(04)  VALUE +3  COMP.  EL6592
00093      12  QID-ITEM                    PIC S9(04)  VALUE +1  COMP.  EL6592
00094      12  WS-MAPSET-NAME              PIC  X(08)  VALUE 'EL6592S'. EL6592
00095      12  WS-MAP-NAME                 PIC  X(08)  VALUE 'EL659B'.  EL6592
00096      12  FILLER  REDEFINES  WS-MAP-NAME.                          EL6592
00097          16  FILLER                  PIC  X(02).                  EL6592
00098          16  WS-MAP-NUMBER           PIC  X(04).                  EL6592
00099          16  FILLER                  PIC  X(02).                     CL*10
00100      12  THIS-PGM                    PIC  X(08)  VALUE 'EL6592'.  EL6592
00101      12  WS-TRANS-ID                 PIC  X(04)  VALUE 'EX66'.    EL6592
00102      12  WS-TEMP-STORAGE-KEY.                                     EL6592
00103          16  WS-TSK-TERM-ID          PIC  X(04)  VALUE 'XXXX'.    EL6592
00104          16  FILLER                  PIC  X(04)  VALUE '6592'.    EL6592
00105      12  WS-TEMP-STORAGE-KEY1.                                    EL6592
00106          16  WS-TSK-TERM-ID1         PIC  X(04)  VALUE 'XXXX'.    EL6592
00107          16  FILLER                  PIC  X(04)  VALUE '659P'.    EL6592
00108      12  WS-KEY-HOLD.                                             EL6592
00109          16  WS-KH-CHAR              PIC  X(01)                   EL6592
00110                  OCCURS  61  TIMES                                EL6592
00111                      INDEXED BY  KEY-INDEX.                       EL6592
00112      12  WS-KEY-INPUT.                                            EL6592
00113          16  WS-KI-CHAR              PIC  X(01)                   EL6592
00114                  OCCURS  61  TIMES                                EL6592
00115                      INDEXED BY  KEY-INDEX2.                      EL6592
00116                                                                      CL**8
00117      12  WS-CITY-INDEX               PIC  S9(4)  COMP.               CL**8
00118      12  WS-COMPARE-INDICATOR        PIC  X.                         CL**8
00119          88 CITY-FOUND                         VALUE SPACE.          CL**8
00120          88 CITY-NOT-FOUND                     VALUE 'X'.            CL**8
00121      12  WS-NL-CITY                  PIC  X(15).                     CL**8
00122      12  WS-NL-CITY-CHAR  REDEFINES                                  CL**8
00123          WS-NL-CITY                  PIC  X                          CL**8
00124                                      OCCURS 15.                      CL**8
00125      12  WS-PI-CITY                  PIC  X(15).                     CL**8
00126      12  WS-PI-CITY-CHAR  REDEFINES                                  CL**8
00127          WS-PI-CITY                  PIC  X                          CL**8
00128                                      OCCURS 15.                      CL**8
00129                                                                      CL**8
00130      12  WS-CALC-RDNXT               PIC S9(08) COMP VALUE ZERO.  EL6592
00131      12  WS-CONTROL-PRIMARY          PIC X(33).                   EL6592
00132      12  ERROR-MESSAGES.                                          EL6592
00133          16  ER-0004                 PIC  X(04)  VALUE '0004'.    EL6592
00134          16  ER-0008                 PIC  X(04)  VALUE '0008'.    EL6592
00135          16  ER-0029                 PIC  X(04)  VALUE '0029'.    EL6592
00136          16  ER-0130                 PIC  X(04)  VALUE '0130'.    EL6592
00137          16  ER-0200                 PIC  X(04)  VALUE '0200'.    EL6592
00138          16  ER-0673                 PIC  X(04)  VALUE '0201'.    EL6592
00139          16  ER-7323                 PIC  X(04)  VALUE '7323'.       CL**6
00140          16  ER-7324                 PIC  X(04)  VALUE '7324'.       CL**6
00141          16  ER-9262                 PIC  X(04)  VALUE '9262'.       CL**5
00142      12  EL650-TRANS-ID              PIC  X(04)  VALUE 'EXC4'.    EL6592
00143      12  EL651-TRANS-ID              PIC  X(04)  VALUE 'EXD1'.    EL6592
00144      12  EL652-TRANS-ID              PIC  X(04)  VALUE 'EXD4'.    EL6592
00145  EJECT                                                            EL6592
00146  01  ACCOUNT-KEY-LINE.                                            EL6592
00147      12  AKL-FILL-1              PIC  X(05).                      EL6592
00148      12  AKL-CARRIER             PIC  X(01).                      EL6592
00149      12  AKL-FILL-2              PIC  X(07).                      EL6592
00150      12  AKL-GROUPING            PIC  X(06).                      EL6592
00151      12  AKL-FILL-3              PIC  X(07).                      EL6592
00152      12  AKL-STATE               PIC  X(02).                      EL6592
00153      12  AKL-FILL-4              PIC  X(09).                      EL6592
00154      12  AKL-ACCOUNT             PIC  X(10).                      EL6592
00155      12  FILLER                  PIC  X(20).                      EL6592
00156                                                                   EL6592
00157  01  COMPENSATION-KEY-LINE.                                       EL6592
00158      12  CKL-FILL-1              PIC  X(05).                      EL6592
00159      12  CKL-CARRIER             PIC  X(01).                      EL6592
00160      12  CKL-FILL-2              PIC  X(07).                      EL6592
00161      12  CKL-GROUPING            PIC  X(06).                      EL6592
00162      12  CKL-FILL-3              PIC  X(10).                      EL6592
00163      12  CKL-RESP                PIC  X(10).                      EL6592
00164      12  CKL-FILL-4              PIC  X(09).                      EL6592
00165      12  CKL-ACCOUNT             PIC  X(10).                      EL6592
00166      12  CKL-FILL-5              PIC  X(06).                      EL6592
00167      12  CKL-TYPE                PIC  X(01).                      EL6592
00168      12  FILLER                  PIC  X(02).                      EL6592
00169                                                                   EL6592
00170  01  REINSURANCE-KEY-LINE.                                        EL6592
00171      12  RKL-FILL-1              PIC  X(06).                      EL6592
00172      12  RKL-REIN-TABLE          PIC  X(03).                      EL6592
00173      12  RKL-FILL-2              PIC  X(06).                      EL6592
00174      12  RKL-REIN-COMP           PIC  X(03).                      EL6592
00175      12  RKL-FILL-3              PIC  X(05).                      EL6592
00176      12  RKL-REIN-COMP-SUB       PIC  X(03).                      EL6592
00177      12  RKL-FILL-4              PIC  X(09).                      EL6592
00178      12  RKL-LEVELS      OCCURS  11  TIMES.                       EL6592
00179          16  RKL-FILL-5          PIC  X(01).                      EL6592
00180          16  RKL-LEVEL           PIC  9(02).                      EL6592
00181  EJECT                                                            EL6592
00182      COPY ELCINTF.                                                   CL**5
00183                                                                   EL6592
00184      COPY ELC659PI.                                                  CL**5
00185          16  PI-EL659-TO-EL130-CNTRL.                                CL**5
00186              20  PI-EL659-CARRIER    PIC X.                          CL**5
00187              20  PI-EL659-GROUPING   PIC X(6).                       CL**5
00188              20  PI-EL659-STATE      PIC XX.                         CL**5
00189              20  PI-EL659-ACCOUNT    PIC X(10).                      CL**5
00190          16  FILLER                  PIC X(90).                      CL*10
00191  EJECT                                                            EL6592
00192      COPY EL6592S.                                                   CL**5
00193                                                                   EL6592
00194  01  FILLER  REDEFINES  EL659BI.                                  EL6592
101501     12  FILLER                      PIC  X(44).                  EL6592
00196      12  EL659B-MAP-LINE     OCCURS  8  TIMES                     EL6592
00197              INDEXED  BY  EL659B-INDEX  EL659B-INDEX2.            EL6592
00198          16  EL659B-TYPE-LENGTH      PIC S9(04)      COMP.        EL6592
00199          16  EL659B-TYPE-ATTR        PIC  X(01).                  EL6592
00200          16  EL659B-TYPE             PIC  X(01).                  EL6592
00201          16  EL659B-NAME-LENGTH      PIC S9(04)      COMP.        EL6592
00202          16  EL659B-NAME-ATTR        PIC  X(01).                  EL6592
00203          16  EL659B-NAME             PIC  X(30).                  EL6592
00204          16  EL659B-CITY-LENGTH      PIC S9(04)      COMP.           CL**8
00205          16  EL659B-CITY-ATTR        PIC  X(01).                     CL**8
00206          16  EL659B-CITY             PIC  X(15).                     CL**8
00207          16  EL659B-ST-LENGTH        PIC S9(04)      COMP.           CL**8
00208          16  EL659B-ST-ATTR          PIC  X(01).                     CL**8
00209          16  EL659B-ST               PIC  XX.                        CL**8
00210          16  EL659B-AST-LENGTH       PIC S9(04)      COMP.        EL6592
00211          16  EL659B-AST-ATTR         PIC  X(01).                  EL6592
00212          16  EL659B-AST              PIC  X(01).                  EL6592
00213          16  EL659B-KEY-LENGTH       PIC S9(04)      COMP.        EL6592
00214          16  EL659B-KEY-ATTR         PIC  X(01).                  EL6592
00215          16  EL659B-KEY              PIC  X(68).                  EL6592
00216  EJECT                                                            EL6592
00217      COPY ELCDATE.                                                   CL**5
00218  EJECT                                                            EL6592
00219      COPY ELCEMIB.                                                   CL**5
00220  EJECT                                                            EL6592
00221      COPY ELCLOGOF.                                                  CL**5
00222  EJECT                                                            EL6592
00223      COPY ELCATTR.                                                   CL**5
00224  EJECT                                                            EL6592
00225      COPY ELCAID.                                                    CL**5
00226                                                                   EL6592
00227  01  FILLER  REDEFINES  DFHAID.                                   EL6592
00228      12  FILLER                  PIC  X(08).                      EL6592
00229      12  PF-VALUES               PIC  X(01)                       EL6592
00230              OCCURS  24  TIMES.                                   EL6592
00231                                                                   EL6592
00232  LINKAGE SECTION.                                                 EL6592
00233                                                                   EL6592
00234  01  DFHCOMMAREA                 PIC  X(1024).                    EL6592
00235                                                                   EL6592
00236 *01 PARMLIST             COMP.                                       CL*10
00237 *    12  FILLER                  PIC S9(09).                         CL*10
00238 *    12  ERNAME-POINTER          PIC S9(09).                         CL*10
00239  EJECT                                                            EL6592
00240      COPY ERCNAME.                                                   CL**5
00241  EJECT                                                            EL6592
00242  PROCEDURE DIVISION.                                              EL6592
00243                                                                   EL6592
00244      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6592
00245      MOVE '5'                    TO  DC-OPTION-CODE.              EL6592
00246                                                                   EL6592
00247      PERFORM 8500-DATE-CONVERSION.                                EL6592
00248                                                                   EL6592
00249      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL6592
00250      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL6592
00251      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL6592
00252                                                                   EL6592
00253 *    NOTE ******************************************************* EL6592
00254 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL6592
00255 *         *  FROM ANOTHER MODULE.                               * EL6592
00256 *         *******************************************************.EL6592
00257                                                                   EL6592
00258      IF EIBCALEN  IS NOT GREATER THAN  ZERO                       EL6592
00259          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL6592
00260          PERFORM 8300-SEND-TEXT.                                  EL6592
00261                                                                   EL6592
00262      EXEC CICS HANDLE CONDITION                                   EL6592
00263          PGMIDERR  (9600-PGMIDERR)                                EL6592
00264          NOTFND    (8700-NOT-FOUND)                               EL6592
00265          ENDFILE   (4600-ENDFILE)                                 EL6592
00266          DUPKEY    (4015-DUPKEY)                                  EL6592
00267          ERROR     (9990-ERROR)                                   EL6592
00268      END-EXEC.                                                    EL6592
00269  EJECT                                                            EL6592
00270  0010-MAIN-LOGIC.                                                 EL6592
00271      IF PI-CALLING-PROGRAM  IS EQUAL TO  THIS-PGM                 EL6592
00272          GO TO 0100-MAIN-LOGIC.                                   EL6592
00273                                                                   EL6592
00274      IF PI-RETURN-TO-PROGRAM  IS NOT EQUAL TO  THIS-PGM           EL6592
00275          MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6        EL6592
00276          MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5        EL6592
00277          MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4        EL6592
00278          MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3        EL6592
00279          MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2        EL6592
00280          MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1        EL6592
00281          MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM      EL6592
00282          MOVE THIS-PGM              TO  PI-CALLING-PROGRAM        EL6592
00283      ELSE                                                         EL6592
00284          MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM        EL6592
00285          MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM      EL6592
00286          MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1        EL6592
00287          MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2        EL6592
00288          MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3        EL6592
00289          MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4        EL6592
00290          MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5        EL6592
00291          MOVE SPACES                TO  PI-SAVED-PROGRAM-6.       EL6592
00292                                                                   EL6592
00293      MOVE ZERO                   TO  PI-SCREEN-COUNT.             EL6592
00294      MOVE +0                     TO  PI-SUB.                      EL6592
00295      MOVE PI-NAME-LOOKUP-KEY     TO  PI-1ST-KEY.                  EL6592
00296                                                                   EL6592
00297      IF EIBTRNID = EL650-TRANS-ID OR                                 CL**5
00298                    EL651-TRANS-ID OR                                 CL**5
00299                    EL652-TRANS-ID                                    CL**5
00300          MOVE EIBTRMID           TO  WS-TSK-TERM-ID               EL6592
00301                                      WS-TSK-TERM-ID1              EL6592
00302          EXEC CICS READQ TS                                       EL6592
00303              QUEUE   (WS-TEMP-STORAGE-KEY)                        EL6592
00304              ITEM    (SC-ITEM)                                    EL6592
00305              INTO    (EL659BI)                                    EL6592
00306              LENGTH  (WS-TS-LENGTH)                               EL6592
00307          END-EXEC                                                 EL6592
00308          EXEC CICS DELETEQ TS                                     EL6592
00309              QUEUE  (WS-TEMP-STORAGE-KEY)                         EL6592
00310          END-EXEC                                                 EL6592
00311          EXEC CICS READQ TS                                       EL6592
00312              QUEUE   (WS-TEMP-STORAGE-KEY1)                       EL6592
00313              ITEM    (SC-ITEM)                                    EL6592
00314              INTO    (PROGRAM-INTERFACE-BLOCK)                    EL6592
00315              LENGTH  (WS-TS-LENGTH)                               EL6592
00316          END-EXEC                                                 EL6592
00317          EXEC CICS DELETEQ TS                                     EL6592
00318              QUEUE  (WS-TEMP-STORAGE-KEY1)                        EL6592
00319          END-EXEC                                                 EL6592
00320          MOVE ZERO               TO  PI-1ST-TIME-SW               EL6592
00321          MOVE LOW-VALUES         TO  BSELO                        EL6592
00322                                      BPFKO                        EL6592
00323          PERFORM 6000-SET-ATTRB                                   EL6592
00324              VARYING  EL659B-INDEX  FROM  PI-LINE-COUNT  BY  -1   EL6592
00325                  UNTIL  EL659B-INDEX  IS NOT GREATER THAN  ZERO   EL6592
00326          PERFORM 8100-SEND-INITIAL-MAP                            EL6592
00327          GO TO 9100-RETURN-TRAN.                                  EL6592
00328                                                                   EL6592
00329      PERFORM 4000-BROWSE-CERT-FILE.                               EL6592
00330  EJECT                                                            EL6592
00331  0100-MAIN-LOGIC.                                                 EL6592
00332 *    NOTE ******************************************************* EL6592
00333 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL6592
00334 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL6592
00335 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL6592
00336 *         *******************************************************.EL6592
00337                                                                   EL6592
00338      IF EIBAID  IS EQUAL TO  DFHCLEAR                             EL6592
00339          PERFORM 9400-CLEAR.                                      EL6592
00340                                                                   EL6592
00341      IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3       EL6592
00342          MOVE LOW-VALUES         TO  EL659BO                      EL6592
00343          MOVE -1                 TO  BPFKL                        EL6592
00344          MOVE ER-0008            TO  EMI-ERROR                    EL6592
00345          PERFORM 8200-SEND-DATAONLY                               EL6592
00346          GO TO 9100-RETURN-TRAN.                                  EL6592
00347                                                                   EL6592
00348      EXEC CICS RECEIVE                                            EL6592
00349          INTO    (EL659BI)                                        EL6592
00350          MAPSET  (WS-MAPSET-NAME)                                 EL6592
00351          MAP     (WS-MAP-NAME)                                    EL6592
00352      END-EXEC.                                                    EL6592
00353                                                                   EL6592
00354      IF BPFKL  IS GREATER THAN  ZERO                              EL6592
00355          IF EIBAID  IS NOT EQUAL TO  DFHENTER                     EL6592
00356              MOVE ER-0004        TO  EMI-ERROR                    EL6592
00357              MOVE AL-UNBOF       TO  BPFKA                        EL6592
00358              MOVE -1             TO  BPFKL                        EL6592
00359              PERFORM 8200-SEND-DATAONLY                           EL6592
00360              GO TO 9100-RETURN-TRAN                               EL6592
00361          ELSE                                                     EL6592
00362              IF (BPFKI  IS NUMERIC)                               EL6592
00363                AND (BPFKI  IS GREATER THAN  ZERO                  EL6592
00364                    AND BPFKI  IS LESS THAN  25)                   EL6592
00365                      MOVE PF-VALUES (BPFKI)                       EL6592
00366                                  TO  EIBAID                       EL6592
00367                  ELSE                                             EL6592
00368                      MOVE ER-0029                                 EL6592
00369                                  TO  EMI-ERROR                    EL6592
00370                      MOVE AL-UNBOF                                EL6592
00371                                  TO  BPFKA                        EL6592
00372                      MOVE -1     TO  BPFKL                        EL6592
00373                      PERFORM 8200-SEND-DATAONLY                   EL6592
00374                      GO TO 9100-RETURN-TRAN.                      EL6592
00375                                                                   EL6592
00376 *    NOTE ******************************************************* EL6592
00377 *         *      PF KEY      USAGE                              * EL6592
00378 *         *        PF1       SEARCH FORWARD                     * EL6592
00379 *         *        PF2       SEARCH BACKWARD                    * EL6592
00380 *         *        PF3       NEW CLAIM SETUP                    *    CL**5
00381 *         *        PF12      HELP                               * EL6592
00382 *         *        PF23      LOGOFF                             * EL6592
00383 *         *        PF24      RETURN TO MASTER MENU              * EL6592
00384 *         *******************************************************.EL6592
00385                                                                   EL6592
00386      IF EIBAID  IS EQUAL TO  DFHPF12                              EL6592
00387          MOVE 'EL010'            TO  THIS-PGM                     EL6592
00388          GO TO 9300-XCTL.                                         EL6592
00389                                                                   EL6592
00390      IF EIBAID  IS EQUAL TO  DFHPF23                              EL6592
00391          PERFORM 9000-RETURN-CICS.                                EL6592
00392                                                                   EL6592
00393      IF EIBAID  IS EQUAL TO  DFHPF24                              EL6592
00394          MOVE 'EL126'            TO  THIS-PGM                     EL6592
00395          GO TO 9300-XCTL.                                         EL6592
00396                                                                   EL6592
00397      IF BSELL  IS GREATER THAN  ZERO                              EL6592
00398          IF BSELI  IS NUMERIC                                     EL6592
00399              NEXT SENTENCE                                        EL6592
00400          ELSE                                                     EL6592
00401              MOVE -1             TO  BSELL                        EL6592
00402              MOVE ER-0200        TO  EMI-ERROR                    EL6592
00403              PERFORM 8200-SEND-DATAONLY                           EL6592
00404              GO TO 9100-RETURN-TRAN.                              EL6592
00405                                                                   EL6592
00406      IF BSELL  IS GREATER THAN  ZERO                              EL6592
00407          NEXT SENTENCE                                            EL6592
00408      ELSE                                                         EL6592
00409          GO TO 0120-MAIN-LOGIC.                                   EL6592
00410                                                                   EL6592
00411      IF BSELL  IS GREATER THAN  ZERO                              EL6592
00412        AND BSELO  IS GREATER THAN  ZERO                           EL6592
00413        AND BSELO  IS LESS THAN  '9'                               EL6592
00414        AND BSELI  IS NOT GREATER THAN  PI-LINE-COUNT              EL6592
00415          NEXT SENTENCE                                            EL6592
00416      ELSE                                                         EL6592
00417          MOVE -1                 TO  BSELL                        EL6592
00418          MOVE ER-0200            TO  EMI-ERROR                    EL6592
00419          PERFORM 8200-SEND-DATAONLY                                  CL**5
00420          GO TO 9100-RETURN-TRAN.                                     CL**5
00421                                                                      CL**5
00422      IF EIBAID = DFHPF3                                              CL**5
00423       IF PI-SAVED-PROGRAM-1 NOT = 'EL130'                            CL**6
00424         MOVE -1                 TO  BSELL                            CL**6
00425         MOVE ER-7324            TO  EMI-ERROR                        CL**6
00426         PERFORM 8200-SEND-DATAONLY                                   CL**6
00427         GO TO 9100-RETURN-TRAN                                       CL**6
00428        ELSE                                                          CL**6
00429         SET EL659B-INDEX        TO  BSELI                            CL**5
00430        IF EL659B-TYPE (EL659B-INDEX) = 'A'                           CL**5
00431          MOVE EL659B-KEY (EL659B-INDEX)                              CL**5
00432                                  TO  ACCOUNT-KEY-LINE                CL**5
00433          MOVE AKL-CARRIER        TO PI-EL659-CARRIER                 CL**5
00434          MOVE AKL-GROUPING       TO PI-EL659-GROUPING                CL**5
00435          MOVE AKL-STATE          TO PI-EL659-STATE                   CL**5
00436          MOVE AKL-ACCOUNT        TO PI-EL659-ACCOUNT                 CL**5
00437          MOVE 'EL130'            TO THIS-PGM                         CL**5
00438          GO TO 9300-XCTL                                             CL**5
00439        ELSE                                                          CL**5
00440          MOVE -1                 TO  BSELL                           CL**5
00441          MOVE ER-9262            TO  EMI-ERROR                       CL**5
00442          PERFORM 8200-SEND-DATAONLY                               EL6592
00443          GO TO 9100-RETURN-TRAN.                                  EL6592
00444                                                                   EL6592
00445      IF BSELL  IS GREATER THAN  ZERO                              EL6592
00446          MOVE EIBTRMID       TO  WS-TSK-TERM-ID                   EL6592
00447                                  WS-TSK-TERM-ID1                  EL6592
00448          MOVE -1             TO  BSELL                            EL6592
00449          EXEC CICS WRITEQ TS                                      EL6592
00450              FROM    (EL659BO)                                    EL6592
00451              LENGTH  (WS-TS-LENGTH)                               EL6592
00452              QUEUE   (WS-TEMP-STORAGE-KEY)                        EL6592
00453              ITEM    (SC-ITEM)                                    EL6592
00454          END-EXEC                                                 EL6592
00455          EXEC CICS WRITEQ TS                                      EL6592
00456              FROM    (PROGRAM-INTERFACE-BLOCK)                    EL6592
00457              LENGTH  (WS-TS-LENGTH)                               EL6592
00458              QUEUE   (WS-TEMP-STORAGE-KEY1)                       EL6592
00459              ITEM    (SC-ITEM)                                    EL6592
00460          END-EXEC.                                                EL6592
00461  EJECT                                                            EL6592
00462  0110-MAIN-LOGIC.                                                 EL6592
00463      SET EL659B-INDEX            TO  BSELI.                       EL6592
00464                                                                   EL6592
00465      MOVE PI-SAVED-PROGRAM-1     TO  THIS-PGM.                    EL6592
00466                                                                   EL6592
00467      IF EL659B-TYPE (EL659B-INDEX)  IS EQUAL TO  'C'              EL6592
00468          GO TO 0112-SET-COMP-KEY.                                 EL6592
00469                                                                   EL6592
00470      IF EL659B-TYPE (EL659B-INDEX)  IS EQUAL TO  'R'              EL6592
00471          GO TO 0113-SET-REIN-KEY.                                 EL6592
00472                                                                   EL6592
00473  0111-SET-ACCT-KEY.                                               EL6592
00474      MOVE EL659B-KEY (EL659B-INDEX)                               EL6592
00475                                  TO  ACCOUNT-KEY-LINE.            EL6592
00476      MOVE AKL-CARRIER            TO  PI-CR-CARRIER.               EL6592
00477      MOVE AKL-GROUPING           TO  PI-CR-GROUPING.              EL6592
00478      MOVE AKL-STATE              TO  PI-CR-STATE.                 EL6592
00479      MOVE AKL-ACCOUNT            TO  PI-CR-ACCOUNT.               EL6592
00480      MOVE 'EL650'                TO  THIS-PGM.                    EL6592
00481                                                                   EL6592
00482      GO TO 0114-CONTINUE.                                         EL6592
00483                                                                   EL6592
00484  0112-SET-COMP-KEY.                                               EL6592
00485      MOVE EL659B-KEY (EL659B-INDEX)                               EL6592
00486                                  TO  COMPENSATION-KEY-LINE.       EL6592
00487      MOVE CKL-CARRIER            TO  PI-CR-CARRIER.               EL6592
00488      MOVE CKL-TYPE               TO  PI-CR-TYPE.                  EL6592
00489                                                                      CL**7
00490      IF CKL-GROUPING IS EQUAL TO SPACES                              CL**7
00491         MOVE LOW-VALUES          TO  PI-CR-GROUPING                  CL**7
00492      ELSE                                                            CL**7
00493         MOVE CKL-GROUPING        TO  PI-CR-GROUPING.                 CL**7
00494                                                                      CL**4
00495      IF CKL-RESP  IS EQUAL TO  SPACES                                CL**4
00496          MOVE LOW-VALUES         TO  PI-CR-FIN-RESP                  CL**4
00497      ELSE                                                            CL**4
00498          MOVE CKL-RESP           TO  PI-CR-FIN-RESP.                 CL**4
00499                                                                      CL**4
00500      IF CKL-ACCOUNT  IS EQUAL TO  SPACES                             CL**4
00501          MOVE LOW-VALUES         TO  PI-CR-ACCOUNT                   CL**4
00502      ELSE                                                            CL**4
00503          MOVE CKL-ACCOUNT        TO  PI-CR-ACCOUNT.                  CL**4
00504                                                                      CL**4
00505      MOVE 'EL652'                TO  THIS-PGM.                    EL6592
00506                                                                   EL6592
00507      GO TO 0114-CONTINUE.                                         EL6592
00508                                                                   EL6592
00509  0113-SET-REIN-KEY.                                               EL6592
00510      MOVE EL659B-KEY (EL659B-INDEX)                               EL6592
00511                                  TO  REINSURANCE-KEY-LINE.        EL6592
00512      MOVE LOW-VALUES             TO  PI-ERREIN-KEY.               EL6592
00513      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.           EL6592
00514      MOVE 'A'                    TO  PI-ERR-CODE.                 EL6592
00515      MOVE RKL-REIN-TABLE         TO  PI-ERR-TABLE.                EL6592
00516      MOVE RKL-LEVEL (1)          TO  PI-ERR-LEVEL.                EL6592
00517      MOVE 'EL651'                TO  THIS-PGM.                    EL6592
00518                                                                   EL6592
00519  0114-CONTINUE.                                                   EL6592
00520      MOVE +2                     TO  PI-1ST-TIME-SW.              EL6592
00521                                                                   EL6592
00522      IF THIS-PGM  IS NOT EQUAL TO  'EL6592'                       EL6592
00523          MOVE 'EL6592'           TO  PI-CALLING-PROGRAM           EL6592
00524          MOVE PI-SAVED-PROGRAM-1 TO  PI-RETURN-TO-PROGRAM            CL**8
00525          MOVE PI-SAVED-PROGRAM-2 TO  PI-SAVED-PROGRAM-1              CL**8
00526          MOVE PI-SAVED-PROGRAM-3 TO  PI-SAVED-PROGRAM-2              CL**8
00527          MOVE PI-SAVED-PROGRAM-4 TO  PI-SAVED-PROGRAM-3              CL**8
00528          MOVE PI-SAVED-PROGRAM-5 TO  PI-SAVED-PROGRAM-4              CL**8
00529          MOVE PI-SAVED-PROGRAM-6 TO  PI-SAVED-PROGRAM-5              CL**8
00530          MOVE SPACES             TO  PI-SAVED-PROGRAM-6.          EL6592
00531                                                                   EL6592
00532      GO TO 9300-XCTL.                                             EL6592
00533  EJECT                                                            EL6592
00534  0120-MAIN-LOGIC.                                                 EL6592
00535      IF BSELL  = ZERO AND                                            CL**6
00536         EIBAID = DFHPF3                                              CL**6
00537          MOVE ER-7323            TO  EMI-ERROR                       CL**6
00538          MOVE -1                 TO  BSELL                           CL**6
00539          PERFORM 8200-SEND-DATAONLY                                  CL**6
00540          GO TO 9100-RETURN-TRAN.                                     CL**6
00541                                                                      CL**6
00542      IF EIBAID  IS EQUAL TO  (DFHENTER  OR  DFHPF1)               EL6592
00543        OR ((EIBAID  IS EQUAL TO  DFHPF2)                          EL6592
00544        AND (PI-SCREEN-COUNT  IS GREATER THAN  +1))                EL6592
00545          NEXT SENTENCE                                            EL6592
00546      ELSE                                                         EL6592
00547          MOVE ER-0008            TO  EMI-ERROR                    EL6592
00548          MOVE -1                 TO  BPFKL                        EL6592
00549          PERFORM 8200-SEND-DATAONLY                               EL6592
00550          GO TO 9100-RETURN-TRAN.                                  EL6592
00551                                                                   EL6592
00552      IF PI-END-OF-FILE  IS NOT GREATER THAN  ZERO                 EL6592
00553          PERFORM 4000-BROWSE-CERT-FILE.                           EL6592
00554                                                                   EL6592
00555      IF PI-END-OF-FILE  IS GREATER THAN  ZERO                     EL6592
00556          IF EIBAID  IS EQUAL TO  DFHPF2                           EL6592
00557              NEXT SENTENCE                                        EL6592
00558          ELSE                                                     EL6592
00559              MOVE -1             TO  BSELL                        EL6592
00560              PERFORM 8200-SEND-DATAONLY                           EL6592
00561              GO TO 9100-RETURN-TRAN.                              EL6592
00562                                                                   EL6592
00563      IF EIBAID  IS EQUAL TO  DFHPF2                               EL6592
00564          NEXT SENTENCE                                            EL6592
00565      ELSE                                                         EL6592
00566          IF PI-DSID  IS NOT EQUAL TO  'ERNAME'                    EL6592
00567              PERFORM 9400-CLEAR.                                  EL6592
00568                                                                   EL6592
00569      IF (PI-PREV-AID  IS EQUAL TO  (DFHPF1  OR  DFHENTER)         EL6592
00570        AND EIBAID  IS EQUAL TO  DFHPF2)                           EL6592
00571        OR (PI-PREV-AID  IS EQUAL TO  DFHPF2                       EL6592
00572        AND EIBAID  IS EQUAL TO  (DFHPF1  OR  DFHENTER))           EL6592
00573          PERFORM 4000-BROWSE-CERT-FILE                            EL6592
00574      ELSE                                                         EL6592
00575          PERFORM 9400-CLEAR.                                      EL6592
00576  EJECT                                                            EL6592
00577  4000-BROWSE-CERT-FILE  SECTION.                                  EL6592
00578      EXEC CICS HANDLE CONDITION                                   EL6592
00579          NOTFND  (8700-NOT-FOUND)                                 EL6592
00580      END-EXEC.                                                    EL6592
00581                                                                   EL6592
00582      MOVE LOW-VALUES             TO  EL659BO.                     EL6592
00583                                                                   EL6592
00584      IF PI-BROWSE-SW  IS EQUAL TO  ZERO                           EL6592
00585        AND PI-START-SW  IS EQUAL TO  +1                           EL6592
00586          EXEC CICS STARTBR                                        EL6592
00587              DATASET    (PI-DSID)                                 EL6592
00588              RIDFLD     (PI-NAME-LOOKUP-KEY)                      EL6592
00589              GENERIC                                              EL6592
00590              EQUAL                                                EL6592
00591              KEYLENGTH  (PI-KEY-LENGTH)                           EL6592
00592          END-EXEC                                                 EL6592
00593          GO TO 4005-NEXT-SENTENCE.                                EL6592
00594                                                                   EL6592
00595      IF EIBAID  IS EQUAL TO  DFHPF2                               EL6592
00596          SUBTRACT 2              FROM  PI-SCREEN-COUNT            EL6592
00597          PERFORM 7000-PF2-POSITION                                EL6592
00598          GO TO 4005-NEXT-SENTENCE.                                EL6592
00599                                                                   EL6592
00600      EXEC CICS STARTBR                                            EL6592
00601          DATASET  (PI-DSID)                                       EL6592
00602          RIDFLD   (PI-NAME-LOOKUP-KEY)                            EL6592
00603          EQUAL                                                    EL6592
00604      END-EXEC.                                                    EL6592
00605                                                                   EL6592
00606  4005-NEXT-SENTENCE.                                              EL6592
00607      MOVE +1                     TO  PI-BROWSE-SW.                EL6592
00608      MOVE ZERO                   TO  PI-LINE-COUNT.               EL6592
00609      MOVE LOW-VALUES             TO  EL659BO.                     EL6592
00610      MOVE PI-NAME-LOOKUP-KEY     TO  WS-KEY-HOLD.                 EL6592
00611      SET EL659B-INDEX            TO  +1.                          EL6592
00612                                                                   EL6592
00613  4010-READNEXT.                                                   EL6592
00614      MOVE PI-NAME-LOOKUP-KEY     TO  PI-PREV-NAME-LOOKUP-KEY.     EL6592
00615                                                                   EL6592
00616      EXEC CICS READNEXT                                           EL6592
00617          DATASET  (PI-DSID)                                       EL6592
00618          RIDFLD   (PI-NAME-LOOKUP-KEY)                            EL6592
00619          SET      (ADDRESS OF NAME-LOOKUP-MASTER)                    CL*10
00620      END-EXEC.                                                    EL6592
00621                                                                   EL6592
00622  4015-DUPKEY.                                                     EL6592
00623      IF NL-COMPANY-CD GREATER PI-COMPANY-CD                          CL**9
00624          GO TO 4600-ENDFILE.                                         CL**9
00625                                                                      CL**2
00626      IF PI-ST NOT = SPACES                                           CL**8
00627        IF PI-ST = NL-ST                                              CL**8
00628            NEXT SENTENCE                                             CL**8
00629          ELSE                                                        CL**8
00630            GO TO 4010-READNEXT.                                      CL**8
00631                                                                      CL**8
00632      IF PI-CITY = SPACES                                             CL**8
00633          GO TO 4017-CONTINUE.                                     EL6592
00634                                                                      CL**8
00635      MOVE PI-CITY                TO WS-PI-CITY.                      CL**8
00636      MOVE NL-CITY                TO WS-NL-CITY.                      CL**8
00637                                                                      CL**8
00638      MOVE SPACE                  TO WS-COMPARE-INDICATOR.            CL**8
00639      PERFORM 4099-CHECK-CITY THRU 4099-EXIT                          CL**8
00640          VARYING WS-CITY-INDEX FROM PI-CITY-LENGTH BY -1             CL**8
00641              UNTIL WS-CITY-INDEX = ZERO.                             CL**8
00642                                                                      CL**8
00643      IF CITY-NOT-FOUND                                               CL**8
00644          GO TO 4010-READNEXT.                                        CL**8
00645                                                                      CL**8
00646  4017-CONTINUE.                                                      CL**8
00647      IF OPTION-ONE-SELECTED                                          CL**8
00648          GO TO 4018-CONTINUE.                                        CL**8
00649                                                                   EL6592
00650      IF OPTION-TWO-SELECTED                                       EL6592
00651          IF NL-RECORD-TYPE  IS EQUAL TO  PI-SC-RECORD-TYPE        EL6592
00652              GO TO 4018-CONTINUE                                     CL**8
00653          ELSE                                                     EL6592
00654              GO TO 4010-READNEXT.                                 EL6592
00655                                                                   EL6592
00656      IF OPTION-THREE-SELECTED                                     EL6592
00657          IF NL-RECORD-TYPE  IS EQUAL TO  PI-SC-RECORD-TYPE        EL6592
00658              GO TO 4021-CONTINUE                                  EL6592
00659          ELSE                                                     EL6592
00660              GO TO 4010-READNEXT.                                 EL6592
00661                                                                   EL6592
00662  4018-CONTINUE.                                                      CL**8
00663      MOVE PI-NAME-LOOKUP-KEY     TO  WS-KEY-INPUT.                EL6592
00664                                                                   EL6592
00665      SET KEY-INDEX                                                EL6592
00666          KEY-INDEX2              TO  +1.                          EL6592
00667                                                                   EL6592
00668  4020-COMPARE-KEY.                                                EL6592
00669      IF WS-KH-CHAR (KEY-INDEX)                                    EL6592
00670          IS NOT EQUAL TO  WS-KI-CHAR (KEY-INDEX2)                 EL6592
00671              GO TO 4700-END-OF-BROWSE.                            EL6592
00672                                                                   EL6592
00673      IF KEY-INDEX  IS LESS THAN  PI-KEY-LENGTH                    EL6592
00674          SET KEY-INDEX                                            EL6592
00675              KEY-INDEX2  UP  BY  +1                               EL6592
00676          GO TO 4020-COMPARE-KEY.                                  EL6592
00677                                                                   EL6592
00678 ******************************************************************EL6592
00679 *        SECURITY CHECK FOR CARRIER AND ACCOUNT NUMBER           *EL6592
00680 *                        04/04/84                                *EL6592
00681 ******************************************************************EL6592
00682                                                                   EL6592
00683 ******************************************************************EL6592
00684 *    IF THE SECURITY CHECK ROUTINE IS CHANGED HERE, YOU MUST     *EL6592
00685 *        ALSO CHANGE THE SECURITY CHECK ROUTINE IN               *EL6592
00686 *        7100-READNEXT-PF2.                                      *EL6592
00687 ******************************************************************EL6592
00688                                                                   EL6592
00689  4021-CONTINUE.                                                   EL6592
00690      IF NL-RECORD-TYPE  IS EQUAL TO  'R'                          EL6592
00691          GO TO 4090-MOVE-DATA.                                    EL6592
00692                                                                   EL6592
00693      IF PI-NO-CARRIER-SECURITY                                    EL6592
00694        AND PI-NO-ACCOUNT-SECURITY                                 EL6592
00695          GO TO 4090-MOVE-DATA.                                    EL6592
00696                                                                   EL6592
00697      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES              EL6592
00698          NEXT SENTENCE                                            EL6592
00699      ELSE                                                         EL6592
00700          GO TO 4022-CONTINUE.                                     EL6592
00701                                                                   EL6592
00702      IF NL-RECORD-TYPE  IS EQUAL TO  'A'                          EL6592
00703          IF NL-AM-CARRIER  IS EQUAL TO  PI-CARRIER-SECURITY       EL6592
00704              GO TO 4022-CONTINUE                                  EL6592
00705          ELSE                                                     EL6592
00706              GO TO 4010-READNEXT.                                 EL6592
00707                                                                   EL6592
00708      IF NL-CO-CARRIER  IS EQUAL TO  PI-CARRIER-SECURITY           EL6592
00709          NEXT SENTENCE                                            EL6592
00710      ELSE                                                         EL6592
00711          GO TO 4010-READNEXT.                                     EL6592
00712                                                                   EL6592
00713  4022-CONTINUE.                                                   EL6592
00714      IF PI-ACCOUNT-SECURITY  IS GREATER THAN  SPACES              EL6592
00715          NEXT SENTENCE                                            EL6592
00716      ELSE                                                         EL6592
00717          GO TO 4090-MOVE-DATA.                                    EL6592
00718                                                                   EL6592
00719      IF NL-RECORD-TYPE  IS EQUAL TO  'A'                          EL6592
00720          IF NL-AM-ACCOUNT  IS EQUAL TO  PI-ACCOUNT-SECURITY       EL6592
00721              GO TO 4090-MOVE-DATA                                 EL6592
00722          ELSE                                                     EL6592
00723              GO TO 4010-READNEXT.                                 EL6592
00724                                                                   EL6592
00725      IF NL-CO-ACCOUNT  IS EQUAL TO  PI-ACCOUNT-SECURITY           EL6592
00726          NEXT SENTENCE                                            EL6592
00727      ELSE                                                         EL6592
00728          GO TO 4010-READNEXT.                                     EL6592
00729                                                                   EL6592
00730  4090-MOVE-DATA.                                                  EL6592
CIDMOD     MOVE LOW-VALUES             TO  ACCOUNT-KEY-LINE
CIDMOD
00731      IF LCP-ONCTR-01 =  0                                            CL*10
00732          ADD 1 TO LCP-ONCTR-01                                       CL*10
00733          MOVE +1                 TO  WS-NAME-SW                      CL**8
00734          MOVE PI-NAME-LOOKUP-KEY TO  PI-LIN1-NAME-LOOKUP-KEY.        CL**8
00735                                                                   EL6592
00736      ADD +1                      TO  PI-LINE-COUNT.                  CL**8
00737                                                                   EL6592
00738      IF EL659B-INDEX  IS EQUAL TO  1                              EL6592
00739          MOVE NL-RECORD-KEY      TO  PI-START-NAME-LOOKUP-KEY.    EL6592
00740                                                                   EL6592
00741      MOVE NL-RECORD-KEY          TO  PI-END-NAME-LOOKUP-KEY.      EL6592
00742      MOVE NL-RECORD-TYPE         TO  EL659B-TYPE (EL659B-INDEX).  EL6592
00743      MOVE NL-NAME                TO  EL659B-NAME (EL659B-INDEX).  EL6592
00744      MOVE NL-CITY                TO  EL659B-CITY (EL659B-INDEX).     CL**8
00745      MOVE NL-ST                  TO  EL659B-ST   (EL659B-INDEX).     CL**8
00746                                                                   EL6592
00747      IF NL-RECORD-TYPE  IS EQUAL TO  'C'                          EL6592
00748          GO TO 4100-MOVE-COMP.                                    EL6592
00749                                                                   EL6592
00750      IF NL-RECORD-TYPE  IS EQUAL TO  'R'                          EL6592
00751          GO TO 4110-MOVE-REIN.                                    EL6592
00752                                                                   EL6592
00753      MOVE SPACES                 TO  ACCOUNT-KEY-LINE.               CL*10
00754      MOVE 'CARR '                TO  AKL-FILL-1.                  EL6592
00755      MOVE NL-AM-CARRIER          TO  AKL-CARRIER.                 EL6592
00756      MOVE ' GROUP '              TO  AKL-FILL-2.                  EL6592
00757      MOVE NL-AM-GROUPING         TO  AKL-GROUPING.                EL6592
00758      MOVE ' STATE '              TO  AKL-FILL-3.                  EL6592
00759      MOVE NL-AM-STATE            TO  AKL-STATE.                   EL6592
00760      MOVE ' ACCOUNT '            TO  AKL-FILL-4.                  EL6592
00761      MOVE NL-AM-ACCOUNT          TO  AKL-ACCOUNT.                 EL6592
00762      MOVE ACCOUNT-KEY-LINE       TO  EL659B-KEY (EL659B-INDEX).   EL6592
00763                                                                   EL6592
00764      GO TO 4200-CONTINUE.                                         EL6592
00765                                                                   EL6592
00766  4099-CHECK-CITY.                                                    CL**8
00767      IF WS-NL-CITY-CHAR (WS-CITY-INDEX) NOT =                        CL**8
00768         WS-PI-CITY-CHAR (WS-CITY-INDEX)                              CL**8
00769           MOVE 'X'               TO WS-COMPARE-INDICATOR.            CL**8
00770                                                                      CL**8
00771  4099-EXIT.                                                          CL**8
00772       EXIT.                                                          CL**8
00773                                                                      CL**8
00774  4100-MOVE-COMP.                                                  EL6592
CIDMOD     MOVE LOW-VALUES             TO  COMPENSATION-KEY-LINE
CIDMOD
00775      MOVE 'CARR '                TO  CKL-FILL-1.                  EL6592
00776      MOVE NL-CO-CARRIER          TO  CKL-CARRIER.                 EL6592
00777      MOVE ' GROUP '              TO  CKL-FILL-2.                  EL6592
00778                                                                      CL**7
00779      IF NL-CO-GROUPING IS EQUAL TO LOW-VALUES                        CL**7
00780          MOVE SPACES             TO  CKL-GROUPING                    CL**8
00781      ELSE                                                            CL**7
00782          MOVE NL-CO-GROUPING     TO  CKL-GROUPING.                   CL**8
00783                                                                      CL**7
00784      MOVE ' FIN RESP '           TO  CKL-FILL-3.                  EL6592
00785                                                                      CL**4
00786      IF NL-CO-RESP-NO  IS EQUAL TO  LOW-VALUES                       CL**4
00787          MOVE SPACES             TO  CKL-RESP                        CL**4
00788      ELSE                                                            CL**4
00789          MOVE NL-CO-RESP-NO      TO  CKL-RESP.                       CL**4
00790                                                                      CL**4
00791      MOVE ' ACCOUNT '            TO  CKL-FILL-4.                  EL6592
00792                                                                      CL**4
00793      IF NL-CO-ACCOUNT  IS EQUAL TO  LOW-VALUES                       CL**4
00794          MOVE SPACES             TO  CKL-ACCOUNT                     CL**4
00795      ELSE                                                            CL**4
00796          MOVE NL-CO-ACCOUNT      TO  CKL-ACCOUNT.                    CL**4
00797                                                                      CL**4
00798      MOVE ' TYPE '               TO  CKL-FILL-5.                  EL6592
00799      MOVE NL-CO-TYPE             TO  CKL-TYPE.                    EL6592
00800      MOVE COMPENSATION-KEY-LINE  TO  EL659B-KEY (EL659B-INDEX).   EL6592
00801                                                                   EL6592
00802      GO TO 4200-CONTINUE.                                         EL6592
00803                                                                   EL6592
00804  4110-MOVE-REIN.                                                  EL6592
CIDMOD     MOVE LOW-VALUES             TO  REINSURANCE-KEY-LINE
CIDMOD
00805      IF NL-RE-LEVEL (12)  IS NOT EQUAL TO  SPACES                 EL6592
00806          MOVE '*'                TO  EL659B-AST (EL659B-INDEX).   EL6592
00807                                                                   EL6592
00808      MOVE 'TABLE '               TO  RKL-FILL-1.                  EL6592
00809      MOVE NL-RE-TABLE            TO  RKL-REIN-TABLE.              EL6592
00810      MOVE ' COMP '               TO  RKL-FILL-2.                  EL6592
00811      MOVE NL-RE-COMP             TO  RKL-REIN-COMP.               EL6592
00812      MOVE ' SUB '                TO  RKL-FILL-3.                  EL6592
00813      MOVE NL-RE-CO-SUB           TO  RKL-REIN-COMP-SUB.           EL6592
00814      MOVE ' ON LEVEL'            TO  RKL-FILL-4.                  EL6592
00815                                                                   EL6592
00816      IF NL-RE-LEVEL (1)  IS NUMERIC                               EL6592
00817          IF NL-RE-LEVEL (1)  IS GREATER THAN  ZERO                EL6592
00818              MOVE ' '            TO  RKL-FILL-5 (1)               EL6592
00819              MOVE NL-RE-LEVEL (1)                                 EL6592
00820                                  TO  RKL-LEVEL (1).               EL6592
00821                                                                   EL6592
00822      IF NL-RE-LEVEL (2)  IS NUMERIC                               EL6592
00823          IF NL-RE-LEVEL (2)  IS GREATER THAN  ZERO                EL6592
00824              MOVE ' '            TO  RKL-FILL-5 (2)               EL6592
00825              MOVE NL-RE-LEVEL (2)                                 EL6592
00826                                  TO  RKL-LEVEL (2).               EL6592
00827                                                                   EL6592
00828      IF NL-RE-LEVEL (3)  IS NUMERIC                               EL6592
00829          IF NL-RE-LEVEL (3)  IS GREATER THAN  ZERO                EL6592
00830              MOVE ' '            TO  RKL-FILL-5 (3)               EL6592
00831              MOVE NL-RE-LEVEL (3)                                 EL6592
00832                                  TO  RKL-LEVEL (3).               EL6592
00833                                                                   EL6592
00834      IF NL-RE-LEVEL (4)  IS NUMERIC                               EL6592
00835          IF NL-RE-LEVEL (4)  IS GREATER THAN  ZERO                EL6592
00836              MOVE ' '            TO  RKL-FILL-5 (4)               EL6592
00837              MOVE NL-RE-LEVEL (4)                                 EL6592
00838                                  TO  RKL-LEVEL (4).               EL6592
00839                                                                   EL6592
00840      IF NL-RE-LEVEL (5)  IS NUMERIC                               EL6592
00841          IF NL-RE-LEVEL (5)  IS GREATER THAN  ZERO                EL6592
00842              MOVE ' '            TO  RKL-FILL-5 (5)               EL6592
00843              MOVE NL-RE-LEVEL (5)                                 EL6592
00844                                  TO  RKL-LEVEL (5).               EL6592
00845                                                                   EL6592
00846      IF NL-RE-LEVEL (6)  IS NUMERIC                               EL6592
00847          IF NL-RE-LEVEL (6)  IS GREATER THAN  ZERO                EL6592
00848              MOVE ' '            TO  RKL-FILL-5 (6)               EL6592
00849              MOVE NL-RE-LEVEL (6)                                 EL6592
00850                                  TO  RKL-LEVEL (6).               EL6592
00851                                                                   EL6592
00852      IF NL-RE-LEVEL (7)  IS NUMERIC                               EL6592
00853          IF NL-RE-LEVEL (7)  IS GREATER THAN  ZERO                EL6592
00854              MOVE ' '            TO  RKL-FILL-5 (7)               EL6592
00855              MOVE NL-RE-LEVEL (7)                                 EL6592
00856                                  TO  RKL-LEVEL (7).               EL6592
00857                                                                   EL6592
00858      IF NL-RE-LEVEL (8)  IS NUMERIC                               EL6592
00859          IF NL-RE-LEVEL (8)  IS GREATER THAN  ZERO                EL6592
00860              MOVE ' '            TO  RKL-FILL-5 (8)               EL6592
00861              MOVE NL-RE-LEVEL (8)                                 EL6592
00862                                  TO  RKL-LEVEL (8).               EL6592
00863                                                                   EL6592
00864      IF NL-RE-LEVEL (9)  IS NUMERIC                               EL6592
00865          IF NL-RE-LEVEL (9)  IS GREATER THAN  ZERO                EL6592
00866              MOVE ' '            TO  RKL-FILL-5 (9)               EL6592
00867              MOVE NL-RE-LEVEL (9)                                 EL6592
00868                                  TO  RKL-LEVEL (9).               EL6592
00869                                                                   EL6592
00870      IF NL-RE-LEVEL (10)  IS NUMERIC                              EL6592
00871          IF NL-RE-LEVEL (10)  IS GREATER THAN  ZERO               EL6592
00872              MOVE ' '            TO  RKL-FILL-5 (10)              EL6592
00873              MOVE NL-RE-LEVEL (10)                                EL6592
00874                                  TO  RKL-LEVEL (10).              EL6592
00875                                                                   EL6592
00876      IF NL-RE-LEVEL (11)  IS NUMERIC                              EL6592
00877          IF NL-RE-LEVEL (11)  IS GREATER THAN  ZERO               EL6592
00878              MOVE ' '            TO  RKL-FILL-5 (11)              EL6592
00879              MOVE NL-RE-LEVEL (11)                                EL6592
00880                                  TO  RKL-LEVEL (11).              EL6592
00881                                                                   EL6592
00882      MOVE REINSURANCE-KEY-LINE   TO  EL659B-KEY (EL659B-INDEX).   EL6592
00883                                                                   EL6592
00884  4200-CONTINUE.                                                   EL6592
00885      PERFORM 6000-SET-ATTRB  THRU  6000-EXIT.                     EL6592
00886                                                                   EL6592
00887      IF EL659B-INDEX  IS LESS THAN  +8                            EL6592
00888          SET EL659B-INDEX  UP  BY  +1                             EL6592
00889          GO TO 4010-READNEXT.                                     EL6592
00890                                                                   EL6592
00891      GO TO 4900-ENDBROWSE.                                        EL6592
00892                                                                   EL6592
00893  4600-ENDFILE.                                                    EL6592
00894      MOVE ER-0130                TO  EMI-ERROR.                   EL6592
00895                                                                   EL6592
00896  4700-END-OF-BROWSE.                                              EL6592
00897      ADD +1                      TO  PI-END-OF-FILE.              EL6592
00898                                                                   EL6592
00899  4900-ENDBROWSE.                                                  EL6592
00900      ADD 1                       TO  PI-SCREEN-COUNT.             EL6592
00901                                                                   EL6592
00902      EXEC CICS ENDBR                                              EL6592
00903          DATASET  (PI-DSID)                                       EL6592
00904      END-EXEC.                                                    EL6592
00905                                                                   EL6592
00906  4910-SEND-MAP.                                                   EL6592
00907      IF WS-NO-NAME-FOUND                                          EL6592
00908          MOVE +9                 TO  PI-BROWSE-SW                 EL6592
00909          PERFORM 9400-CLEAR.                                      EL6592
00910                                                                   EL6592
00911      MOVE -1                     TO  BSELL.                       EL6592
00912                                                                   EL6592
00913      PERFORM 8100-SEND-INITIAL-MAP.                               EL6592
00914                                                                   EL6592
00915      GO TO 9100-RETURN-TRAN.                                      EL6592
00916                                                                   EL6592
00917  4990-EXIT.                                                       EL6592
00918      EXIT.                                                        EL6592
00919  EJECT                                                            EL6592
00920  6000-SET-ATTRB  SECTION.                                         EL6592
00921      MOVE AL-SABON           TO  EL659B-TYPE-ATTR (EL659B-INDEX)     CL**8
00922                                  EL659B-NAME-ATTR (EL659B-INDEX)  EL6592
00923                                  EL659B-CITY-ATTR (EL659B-INDEX)     CL**8
00924                                  EL659B-ST-ATTR   (EL659B-INDEX)     CL**8
00925                                  EL659B-AST-ATTR  (EL659B-INDEX).    CL**8
00926      MOVE AL-SANON           TO  EL659B-KEY-ATTR  (EL659B-INDEX).    CL**8
00927                                                                   EL6592
00928      MOVE AL-UNNON               TO  BSELA.                       EL6592
00929                                                                   EL6592
00930  6000-EXIT.                                                       EL6592
00931      EXIT.                                                        EL6592
00932  EJECT                                                            EL6592
00933  7000-PF2-POSITION  SECTION.                                      EL6592
00934      EXEC CICS IGNORE CONDITION                                   EL6592
00935          DUPKEY                                                   EL6592
00936      END-EXEC.                                                    EL6592
00937                                                                   EL6592
00938      EXEC CICS HANDLE CONDITION                                   EL6592
00939          NOTFND  (8700-NOT-FOUND)                                 EL6592
00940      END-EXEC.                                                    EL6592
00941                                                                   EL6592
00942      COMPUTE WS-CALC-RDNXT  =  PI-SCREEN-COUNT  *  7.             EL6592
00943                                                                   EL6592
00944      MOVE PI-1ST-KEY             TO  PI-NAME-LOOKUP-KEY.          EL6592
00945      MOVE ZERO                   TO  PI-END-OF-FILE.              EL6592
00946                                                                   EL6592
00947      EXEC CICS STARTBR                                            EL6592
00948          DATASET    (PI-DSID)                                     EL6592
00949          RIDFLD     (PI-NAME-LOOKUP-KEY)                          EL6592
00950          GENERIC                                                  EL6592
00951          EQUAL                                                    EL6592
00952          KEYLENGTH  (PI-KEY-LENGTH)                               EL6592
00953      END-EXEC.                                                    EL6592
00954                                                                   EL6592
00955  7100-READNEXT-PF2.                                               EL6592
00956      IF WS-CALC-RDNXT  IS GREATER THAN  ZERO                      EL6592
00957          NEXT SENTENCE                                            EL6592
00958      ELSE                                                         EL6592
00959          GO TO 7999-EXIT.                                         EL6592
00960                                                                   EL6592
00961      EXEC CICS READNEXT                                           EL6592
00962          DATASET  (PI-DSID)                                       EL6592
00963          RIDFLD   (PI-NAME-LOOKUP-KEY)                            EL6592
00964          SET      (ADDRESS OF NAME-LOOKUP-MASTER)                    CL*10
00965      END-EXEC.                                                    EL6592
00966                                                                   EL6592
00967 ******************************************************************EL6592
00968 *    IF THE SECURITY CHECKING ROUTINE, INITIAL COMPARE ROUTINE   *EL6592
00969 *        OR THE ACCOUNT COMPARE ROUTINE IS CHANGED HERE, YOU     *EL6592
00970 *        MUST ALSO CHANGE THE CORRESPONDING ROUTINE IN           *EL6592
00971 *        PARAGRAPH 4020-COMPARE-KEY OR 4030-CHECK-OPTION.        *EL6592
00972 ******************************************************************EL6592
00973                                                                      CL**2
00974      IF NL-COMPANY-CD  IS EQUAL TO  PI-COMPANY-CD                    CL**2
00975          NEXT SENTENCE                                               CL**2
00976      ELSE                                                            CL**2
00977          GO TO 7100-READNEXT-PF2.                                    CL**2
00978                                                                   EL6592
00979      IF OPTION-ONE-SELECTED                                       EL6592
00980          GO TO 7110-CHECK-SECURITY.                               EL6592
00981                                                                   EL6592
00982      IF OPTION-TWO-SELECTED OR                                       CL**8
00983         OPTION-THREE-SELECTED                                        CL**8
00984          IF NL-RECORD-TYPE  IS EQUAL TO  PI-SC-RECORD-TYPE        EL6592
00985              GO TO 7110-CHECK-SECURITY                            EL6592
00986          ELSE                                                     EL6592
00987              GO TO 7100-READNEXT-PF2.                             EL6592
00988                                                                   EL6592
00989  7110-CHECK-SECURITY.                                             EL6592
00990      IF NL-RECORD-TYPE  IS EQUAL TO  'R'                          EL6592
00991          GO TO 7190-COMPUTE.                                      EL6592
00992                                                                   EL6592
00993      IF PI-NO-CARRIER-SECURITY                                    EL6592
00994        AND PI-NO-ACCOUNT-SECURITY                                 EL6592
00995          GO TO 7190-COMPUTE.                                      EL6592
00996                                                                   EL6592
00997      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES              EL6592
00998          NEXT SENTENCE                                            EL6592
00999      ELSE                                                         EL6592
01000          GO TO 7120-CONTINUE.                                     EL6592
01001                                                                   EL6592
01002      IF NL-RECORD-TYPE  IS EQUAL TO  'A'                          EL6592
01003          IF NL-AM-CARRIER  IS EQUAL TO  PI-CARRIER-SECURITY       EL6592
01004              GO TO 7120-CONTINUE                                  EL6592
01005          ELSE                                                     EL6592
01006              GO TO 7100-READNEXT-PF2.                             EL6592
01007                                                                   EL6592
01008      IF NL-CO-CARRIER  IS EQUAL TO  PI-CARRIER-SECURITY           EL6592
01009          NEXT SENTENCE                                            EL6592
01010      ELSE                                                         EL6592
01011          GO TO 7100-READNEXT-PF2.                                 EL6592
01012                                                                   EL6592
01013  7120-CONTINUE.                                                   EL6592
01014      IF PI-ACCOUNT-SECURITY  IS GREATER THAN  SPACES              EL6592
01015          NEXT SENTENCE                                            EL6592
01016      ELSE                                                         EL6592
01017          GO TO 7190-COMPUTE.                                      EL6592
01018                                                                   EL6592
01019      IF NL-RECORD-TYPE  IS EQUAL TO  'A'                          EL6592
01020          IF NL-AM-ACCOUNT  IS EQUAL TO  PI-ACCOUNT-SECURITY       EL6592
01021              GO TO 7190-COMPUTE                                   EL6592
01022          ELSE                                                     EL6592
01023              GO TO 7100-READNEXT-PF2.                             EL6592
01024                                                                   EL6592
01025      IF NL-CO-ACCOUNT  IS EQUAL TO  PI-ACCOUNT-SECURITY           EL6592
01026          NEXT SENTENCE                                            EL6592
01027      ELSE                                                         EL6592
01028          GO TO 7100-READNEXT-PF2.                                 EL6592
01029                                                                   EL6592
01030  7190-COMPUTE.                                                    EL6592
01031      COMPUTE WS-CALC-RDNXT  =  WS-CALC-RDNXT  -  1.               EL6592
01032                                                                   EL6592
01033      GO TO 7100-READNEXT-PF2.                                     EL6592
01034                                                                   EL6592
01035  7999-EXIT.                                                       EL6592
01036      EXIT.                                                        EL6592
01037  EJECT                                                            EL6592
01038  8100-SEND-INITIAL-MAP  SECTION.                                  EL6592
01039      MOVE SAVE-DATE              TO  BDATEO.                      EL6592
01040      MOVE EIBTIME                TO  TIME-IN.                     EL6592
01041      MOVE TIME-OUT               TO  BTIMEO.                      EL6592
101501     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101501     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01042                                                                   EL6592
01043      IF EMI-ERROR  IS NOT EQUAL TO  ZERO                          EL6592
01044          PERFORM 9900-ERROR-FORMAT.                               EL6592
01045                                                                   EL6592
01046      MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O.                     EL6592
01047                                                                   EL6592
01048      EXEC CICS SEND                                               EL6592
01049          FROM    (EL659BO)                                        EL6592
01050          MAPSET  (WS-MAPSET-NAME)                                 EL6592
01051          MAP     (WS-MAP-NAME)                                    EL6592
01052          CURSOR                                                   EL6592
01053          ERASE                                                    EL6592
01054      END-EXEC.                                                    EL6592
01055                                                                   EL6592
01056  8100-EXIT.                                                       EL6592
01057      EXIT.                                                        EL6592
01058                                                                   EL6592
01059  EJECT                                                            EL6592
01060  8200-SEND-DATAONLY  SECTION.                                     EL6592
01061      MOVE SAVE-DATE              TO  BDATEO.                      EL6592
01062      MOVE EIBTIME                TO  TIME-IN.                     EL6592
01063      MOVE TIME-OUT               TO  BTIMEO.                      EL6592
101501     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101501     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01064                                                                   EL6592
01065      IF EMI-ERROR  IS NOT EQUAL TO  ZERO                          EL6592
01066          PERFORM 9900-ERROR-FORMAT.                               EL6592
01067                                                                   EL6592
01068      MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O.                     EL6592
01069                                                                   EL6592
01070      EXEC CICS SEND DATAONLY                                      EL6592
01071          FROM    (EL659BO)                                        EL6592
01072          MAPSET  (WS-MAPSET-NAME)                                 EL6592
01073          MAP     (WS-MAP-NAME)                                    EL6592
01074          CURSOR                                                   EL6592
01075      END-EXEC.                                                    EL6592
01076                                                                   EL6592
01077  8200-EXIT.                                                       EL6592
01078      EXIT.                                                        EL6592
01079  EJECT                                                            EL6592
01080  8300-SEND-TEXT  SECTION.                                         EL6592
01081      EXEC CICS SEND TEXT                                          EL6592
01082          FROM    (LOGOFF-TEXT)                                    EL6592
01083          LENGTH  (LOGOFF-LENGTH)                                  EL6592
01084          ERASE                                                    EL6592
01085          FREEKB                                                   EL6592
01086      END-EXEC.                                                    EL6592
01087                                                                   EL6592
01088      EXEC CICS RETURN                                             EL6592
01089      END-EXEC.                                                    EL6592
01090                                                                   EL6592
01091  8300-EXIT.                                                       EL6592
01092      EXIT.                                                        EL6592
01093                                                                   EL6592
01094  8500-DATE-CONVERSION  SECTION.                                   EL6592
01095      EXEC CICS LINK                                               EL6592
01096          PROGRAM   ('ELDATCV')                                    EL6592
01097          COMMAREA  (DATE-CONVERSION-DATA)                         EL6592
01098          LENGTH    (DC-COMM-LENGTH)                               EL6592
01099      END-EXEC.                                                    EL6592
01100                                                                   EL6592
01101  8500-EXIT.                                                       EL6592
01102      EXIT.                                                        EL6592
01103  EJECT                                                            EL6592
01104  8650-WRITE-SECURITY-TEMP-STORE  SECTION.                         EL6592
01105      EXEC CICS HANDLE CONDITION                                   EL6592
01106          QIDERR  (8651-WRITE-SECURITY)                            EL6592
01107      END-EXEC.                                                    EL6592
01108                                                                   EL6592
01109      MOVE EIBTRMID               TO  QID.                         EL6592
01110                                                                   EL6592
01111  8651-WRITE-SECURITY.                                             EL6592
01112      EXEC CICS WRITEQ TS                                          EL6592
01113          QUEUE   (QID)                                            EL6592
01114          FROM    (SECURITY-CONTROL)                               EL6592
01115          LENGTH  (SC-COMM-LENGTH)                                 EL6592
01116          ITEM    (QID-ITEM)                                       EL6592
01117      END-EXEC.                                                    EL6592
01118                                                                   EL6592
01119      MOVE QID                    TO  PI-SECURITY-TEMP-STORE-ID.   EL6592
01120                                                                   EL6592
01121      IF PI-PROCESSOR-ID  IS EQUAL TO  'LGXX'                      EL6592
01122          MOVE ALL 'Y'            TO  SC-CREDIT-CODES              EL6592
01123                                      SC-CLAIMS-CODES              EL6592
01124                                      PI-PROCESSOR-USER-ALMIGHTY.  EL6592
01125                                                                   EL6592
01126  8650-EXIT.                                                       EL6592
01127      EXIT.                                                        EL6592
01128  EJECT                                                            EL6592
01129  8700-NOT-FOUND  SECTION.                                         EL6592
01130      PERFORM 8800-INITIALIZE-MAP                                  EL6592
01131          VARYING  EL659B-INDEX  FROM  +1  BY  +1                  EL6592
01132              UNTIL  EL659B-INDEX  IS GREATER THAN  +8.            EL6592
01133                                                                   EL6592
01134      MOVE -1                     TO  BSELL.                       EL6592
01135      MOVE ER-0673                TO  EMI-ERROR.                   EL6592
01136                                                                   EL6592
01137      PERFORM 8100-SEND-INITIAL-MAP.                               EL6592
01138                                                                   EL6592
01139      GO TO 9100-RETURN-TRAN.                                      EL6592
01140                                                                   EL6592
01141  8700-EXIT.                                                       EL6592
01142      EXIT.                                                        EL6592
01143                                                                   EL6592
01144  8800-INITIALIZE-MAP  SECTION.                                    EL6592
01145      MOVE LOW-VALUES         TO  EL659B-MAP-LINE (EL659B-INDEX).  EL6592
01146                                                                   EL6592
01147  8800-EXIT.                                                       EL6592
01148      EXIT.                                                        EL6592
01149  EJECT                                                            EL6592
01150  9000-RETURN-CICS  SECTION.                                       EL6592
01151      MOVE 'EL005'                TO  THIS-PGM.                    EL6592
01152      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL6592
01153                                                                   EL6592
01154      GO TO 9300-XCTL.                                             EL6592
01155                                                                   EL6592
01156  9000-EXIT.                                                       EL6592
01157      EXIT.                                                        EL6592
01158                                                                   EL6592
01159  9100-RETURN-TRAN  SECTION.                                       EL6592
01160      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL6592
01161      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL6592
01162      MOVE EIBAID                 TO  PI-PREV-AID.                 EL6592
01163                                                                   EL6592
01164      EXEC CICS RETURN                                             EL6592
01165          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL6592
01166          LENGTH    (PI-COMM-LENGTH)                               EL6592
01167          TRANSID   (WS-TRANS-ID)                                  EL6592
01168      END-EXEC.                                                    EL6592
01169                                                                   EL6592
01170  9100-EXIT.                                                       EL6592
01171      EXIT.                                                        EL6592
01172                                                                   EL6592
01173  9300-XCTL  SECTION.                                              EL6592
01174      MOVE DFHENTER               TO  EIBAID.                      EL6592
01175                                                                   EL6592
01176      EXEC CICS XCTL                                               EL6592
01177          PROGRAM   (THIS-PGM)                                     EL6592
01178          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL6592
01179          LENGTH    (PI-COMM-LENGTH)                               EL6592
01180      END-EXEC.                                                    EL6592
01181                                                                   EL6592
01182  9300-EXIT.                                                       EL6592
01183      EXIT.                                                        EL6592
01184  EJECT                                                            EL6592
01185  9400-CLEAR  SECTION.                                             EL6592
01186      MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.                    EL6592
01187                                                                   EL6592
01188      GO TO 9300-XCTL.                                             EL6592
01189                                                                   EL6592
01190  9400-EXIT.                                                       EL6592
01191      EXIT.                                                        EL6592
01192                                                                   EL6592
01193  9600-PGMIDERR  SECTION.                                          EL6592
01194      EXEC CICS HANDLE CONDITION                                   EL6592
01195          PGMIDERR  (8300-SEND-TEXT)                               EL6592
01196      END-EXEC.                                                    EL6592
01197                                                                   EL6592
01198      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL6592
01199                                      LOGOFF-PGM.                  EL6592
01200      MOVE 'EL005'                TO  THIS-PGM.                    EL6592
01201      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL6592
01202      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL6592
01203                                                                   EL6592
01204      GO TO 9300-XCTL.                                             EL6592
01205                                                                   EL6592
01206  9600-EXIT.                                                       EL6592
01207      EXIT.                                                        EL6592
01208  EJECT                                                            EL6592
01209  9900-ERROR-FORMAT  SECTION.                                      EL6592
01210      IF EMI-ERRORS-COMPLETE                                       EL6592
01211          GO TO 9900-EXIT.                                         EL6592
01212                                                                   EL6592
01213      EXEC CICS LINK                                               EL6592
01214          PROGRAM   ('EL001')                                      EL6592
01215          COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)                EL6592
01216          LENGTH    (EMI-COMM-LENGTH)                              EL6592
01217      END-EXEC.                                                    EL6592
01218                                                                   EL6592
01219  9900-EXIT.                                                       EL6592
01220      EXIT.                                                        EL6592
01221                                                                   EL6592
01222  9990-ERROR  SECTION.                                             EL6592
01223      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL6592
01224                                                                   EL6592
01225      EXEC CICS LINK                                               EL6592
01226          PROGRAM  ('EL004')                                       EL6592
01227          COMMAREA (EMI-LINE1)                                     EL6592
01228          LENGTH   (72)                                            EL6592
01229      END-EXEC.                                                    EL6592
01230                                                                   EL6592
01231      MOVE -1                     TO  BSELL.                       EL6592
01232                                                                   EL6592
01233      PERFORM 8100-SEND-INITIAL-MAP.                               EL6592
01234                                                                   EL6592
01235      GO TO 9100-RETURN-TRAN.                                      EL6592
01236                                                                   EL6592
01237  9990-EXIT.                                                       EL6592
01238      EXIT.                                                        EL6592
01239                                                                   EL6592
01240  9999-LAST-PARAGRAPH  SECTION.                                    EL6592
01241      GOBACK.                                                      EL6592
