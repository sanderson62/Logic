00001  ID DIVISION.                                                     03/06/96
00002                                                                   EL6564
00003  PROGRAM-ID.                 EL6564.                                 LV014
00004 *              PROGRAM CONVERTED BY                                  CL*14
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*14
00006 *              CONVERSION DATE 02/12/96 17:32:14.                    CL*14
00007 *                            VMOD=2.014                              CL*14
00008 *                                                                 EL6564
00008 *                                                                 EL6564
00009 *AUTHOR.     LOGIC,INC.                                              CL*14
00010 *            DALLAS, TEXAS.                                          CL*14
00011                                                                   EL6564
00012 *DATE-COMPILED.                                                      CL*14
00013 *SECURITY.   *****************************************************   CL*14
00014 *            *                                                   *   CL*14
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*14
00016 *            *                                                   *   CL*14
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*14
00018 *                                                                *   CL*14
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*14
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*14
00021 *            *                                                   *   CL*14
00022 *            *****************************************************   CL*14
00023                                                                   EL6564
00024 *REMARKS.    TRANSACTION - EXE5 - RATE MASTER MAINTENANCE            CL*14
00025 *                                 (INTERPOLATION WORKSHEET).         CL*14
00026      EJECT                                                        EL6564
081413******************************************************************
081413*                   C H A N G E   L O G
081413*
081413* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081413*-----------------------------------------------------------------
081413*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081413* EFFECTIVE    NUMBER
081413*-----------------------------------------------------------------
081413* 081413    2013080700002  PEMA ADD JOURNALING OF ERRATE FILE
081413******************************************************************
00027  ENVIRONMENT DIVISION.                                            EL6564
00028  DATA DIVISION.                                                   EL6564
00029  WORKING-STORAGE SECTION.                                         EL6564
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL6564
00031  77  FILLER  PIC X(32)  VALUE '*    EL6564 WORKING STORAGE    *'. EL6564
00032  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.014 ***********'.    CL*14
00033                                                                      CL**6
00034  01  WS-RATE-AREA.                                                EL6564
00035      05  MISC-SAVE-AREAS.                                         EL6564
00036          10  WS-SCREEN-B-RATES.                                   EL6564
00037              15  WS-RATES-B        OCCURS 30 TIMES.               EL6564
00038                  20  WS-ST-RATE    PIC S99V9(5).                     CL**4
00039                  20  WS-ST-MONTH   PIC 999.                       EL6564
00040                  20  WS-END-RATE   PIC S99V9(5).                     CL**4
00041                  20  WS-END-MONTH  PIC 999.                       EL6564
00042                                                                   EL6564
00043          10  WS-SCREEN-C-RATES REDEFINES WS-SCREEN-B-RATES.          CL**4
00044              15  WS-RATES-C        OCCURS 30 TIMES.               EL6564
00045                  20  WS-RATES      PIC S99V9(5).                     CL**4
00046                  20  WS-ST-YEAR    PIC 99.                        EL6564
00047                  20  WS-END-YEAR   PIC 99.                        EL6564
00048                  20  WS-START-MO   PIC 999.                       EL6564
00049                  20  WS-END-MO     PIC 999.                       EL6564
00050              15  FILLER            PIC X(90).                        CL*14
00051                                                                   EL6564
00052  01  WS-DATE-AREA.                                                EL6564
00053      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL6564
00054      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.               CL**4
00055                                                                   EL6564
00056  01  STANDARD-AREAS.                                              EL6564
00057      05  GETMAIN-SPACE       PIC X       VALUE SPACE.             EL6564
00058      05  TRANS-ID            PIC X(4)    VALUE 'EXE5'.            EL6564
00059      05  THIS-PGM            PIC X(8)    VALUE 'EL6564'.          EL6564
00060      05  WS-MAPNAME          PIC X(8)    VALUE 'EL6564A'.         EL6564
00061      05  WS-MAPSET-NAME      PIC X(8)    VALUE 'EL6564S'.         EL6564
00062      05  PGM-NAME            PIC X(8).                            EL6564
00063      05  SUB1                PIC S999    VALUE +0.                EL6564
00064      05  SUB2                PIC S999    VALUE +0.                EL6564
00065      05  SUB3                PIC S999    VALUE +0.                EL6564
00066      05  SC-ITEM             PIC S9(4)   VALUE +1     COMP.       EL6564
00067      05  WS-COMP-CD-R.                                            EL6564
00068          10  FILLER          PIC X.                               EL6564
00069          10  WS-COMP-CD-X    PIC X.                               EL6564
00070      05  WS-COMP-CD   REDEFINES WS-COMP-CD-R  PIC S9(4)     COMP. EL6564
00071      05  TIME-IN             PIC S9(7).                           EL6564
00072      05  TIME-OUT-R  REDEFINES TIME-IN.                           EL6564
00073          10  FILLER          PIC X.                               EL6564
00074          10  TIME-OUT        PIC 99V99.                           EL6564
00075          10  FILLER          PIC XX.                                 CL**4
00076      05  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL6564
00077      05  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL6564
00078      05  XCTL-626            PIC X(8)    VALUE 'EL126'.           EL6564
00079      05  XCTL-656            PIC X(8)    VALUE 'EL656'.           EL6564
00080      05  XCTL-6562           PIC X(8)    VALUE 'EL6562'.          EL6564
00081      05  LINK-001            PIC X(8)    VALUE 'EL001'.           EL6564
00082      05  LINK-004            PIC X(8)    VALUE 'EL004'.           EL6564
00083      05  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL6564
00084                                                                   EL6564
00085      05  ER-0000             PIC X(4)    VALUE  '0000'.           EL6564
00086      05  ER-0004             PIC X(4)    VALUE  '0004'.           EL6564
00087      05  ER-0008             PIC X(4)    VALUE  '0008'.           EL6564
00088      05  ER-0029             PIC X(4)    VALUE  '0029'.           EL6564
00089      05  ER-0068             PIC X(4)    VALUE  '0068'.           EL6564
00090      05  ER-0070             PIC X(4)    VALUE  '0070'.           EL6564
00091      05  ER-0142             PIC X(4)    VALUE  '0142'.           EL6564
00092      05  ER-2055             PIC X(4)    VALUE  '2055'.           EL6564
00093      05  ER-2280             PIC X(4)    VALUE  '2280'.           EL6564
00094      05  ER-2281             PIC X(4)    VALUE  '2281'.           EL6564
00095      05  ER-2282             PIC X(4)    VALUE  '2282'.           EL6564
00096      05  ER-2283             PIC X(4)    VALUE  '2283'.           EL6564
00097      05  ER-2284             PIC X(4)    VALUE  '2284'.           EL6564
00098      05  ER-2286             PIC X(4)    VALUE  '2286'.           EL6564
00099      05  ER-2288             PIC X(4)    VALUE  '2288'.           EL6564
00100      05  ER-2289             PIC X(4)    VALUE  '2289'.           EL6564
00101      05  ER-2290             PIC X(4)    VALUE  '2290'.           EL6564
00102      05  ER-2291             PIC X(4)    VALUE  '2291'.           EL6564
00103      05  ER-2292             PIC X(4)    VALUE  '2292'.           EL6564
00104      05  ER-2903             PIC X(4)    VALUE  '2903'.           EL6564
00105      05  ER-7400             PIC X(4)    VALUE  '7400'.              CL**5
00106      05  ER-7743             PIC X(4)    VALUE  '7743'.              CL*10
00107                                                                   EL6564
00108      05  RATE-FILE-ID        PIC X(8)    VALUE 'ERRATE'.          EL6564
00109      05  OE-RATE-FILE-ID     PIC X(8)    VALUE 'OERATE'.          EL6564
00110      05  FILE-ID             PIC X(8)    VALUE  SPACES.           EL6564
00111      05  CNTL-FILE-ID        PIC X(8)    VALUE 'ELCNTL'.          EL6564
00112      05  BIN-CURRENT-SAVE    PIC XX      VALUE  SPACES.              CL**4
00113                                                                   EL6564
00114      05  DEEDIT-FIELD        PIC X(15).                              CL*10
00115      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL6564
00116      05  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(14)V9(1).    CL*10
00117      05  DEEDIT-FIELD-V2  REDEFINES DEEDIT-FIELD PIC S9(13)V9(2).    CL*10
00118      05  DEEDIT-FIELD-V3  REDEFINES DEEDIT-FIELD PIC S9(12)V9(3).    CL*10
00119      05  DEEDIT-FIELD-V4  REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).    CL*10
00120      05  DEEDIT-FIELD-V5  REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).    CL*10
00121      05  W-FIELD-V0          PIC S9(15).                             CL*10
00122      05  W-FIELD-V1          PIC S9(14)V9(1).                        CL*10
00123      05  W-FIELD-V2          PIC S9(13)V9(2).                        CL*10
00124      05  W-FIELD-V3          PIC S9(12)V9(3).                        CL*10
00125      05  W-FIELD-V4          PIC S9(11)V9(4).                        CL*10
00126      05  W-FIELD-V5          PIC S9(10)V9(5).                        CL*10
00127                                                                   EL6564
00128      05  ERRATE-LENGTH       PIC S9(4)   VALUE +1765 COMP.        EL6564
00129                                                                   EL6564
00130      05  WS-PREV-MONTH       PIC 999      VALUE ZERO.             EL6564
00131      05  WS-PREV-YEAR        PIC 99       VALUE ZERO.             EL6564
00132                                                                   EL6564
00133      05  WS-CHECK-YEAR       PIC XX       VALUE ZERO.             EL6564
00134          88  VALID-YEAR                   VALUE '01' THRU '30'.   EL6564
00135                                                                   EL6564
00136      05  ELCNTL-KEY.                                              EL6564
00137          10  CNTL-COMP-ID    PIC X(3)    VALUE SPACES.            EL6564
00138          10  CNTL-REC-TYPE   PIC X       VALUE SPACES.            EL6564
00139          10  CNTL-ACCESS     PIC X(4)    VALUE SPACES.            EL6564
00140          10  CNTL-SEQ-NO     PIC S9(4)   VALUE +0  COMP.          EL6564
00141                                                                   EL6564
00142      05  WS-DFACT            PIC S99V9(5)    VALUE +0.               CL**6
00143      05  WS-RATE-DIFF        PIC S99V9(5)    VALUE +0.               CL**4
00144      05  WS-MO-DIFF          PIC S999        VALUE +0.            EL6564
00145      05  WS-MO-RATE          PIC S99V9(11)   VALUE +0.               CL**4
00146      05  WS-RATE             PIC S99V9(5)    VALUE +0.               CL**4
00147      05  WS-SAVE-END-RATE    PIC S99V9(5)    VALUE +0.               CL**4
00148      05  WS-SAVE-END-MONTH   PIC 999.                             EL6564
00149                                                                   EL6564
00150      05  LAST-INDX           PIC S9(4)  COMP.                     EL6564
00151                                                                   EL6564
081413 01  filler.
081413     12  WS-RESPONSE             PIC S9(8)   COMP.                    
081413         88  WS-RESP-NORMAL              VALUE +00.               
081413         88  WS-RESP-ERROR               VALUE +01.               
081413         88  WS-RESP-NOTFND              VALUE +13.               
081413         88  WS-RESP-DUPKEY              VALUE +15.
081413         88  WS-RESP-NOTOPEN             VALUE +19.
081413         88  WS-RESP-ENDFILE             VALUE +20.

00152      EJECT                                                        EL6564
00153                                    COPY ELCSCTM.                     CL*14
00154      EJECT                                                        EL6564
00155                                    COPY ELCSCRTY.                    CL*14
00156      EJECT                                                        EL6564
00157                                    COPY ELCDATE.                     CL*14
00158      EJECT                                                        EL6564
00159                                    COPY ELCLOGOF.                    CL*14
00160      EJECT                                                        EL6564
00161                                    COPY ELCATTR.                     CL*14
00162      EJECT                                                           CL*14
00163                                    COPY ELCEMIB.                     CL*14
00164      EJECT                                                           CL*14
00165                                    COPY ELCINTF.                     CL*14
00166      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             EL6564
00167          16  PI-FILE-ID                    PIC XX.                EL6564
00168          16  PI-MAINT                      PIC X.                 EL6564
00169                                                                   EL6564
00170          16  PI-ERRATE-KEY.                                       EL6564
00171              20  PI-RATE-COMPANY-CD         PIC X.                EL6564
00172              20  PI-RATE-STATE-CODE.                              EL6564
00173                  24  PI-RATE-CODE           PIC XX.               EL6564
00174                  24  PI-RATE-CLASS          PIC XX.               EL6564
00175                  24  PI-RATE-DEV            PIC XXX.                 CL**4
00176              20  PI-RATE-L-AH-CODE.                               EL6564
00177                  24  PI-RATE-L-AH           PIC X.                EL6564
00178                  24  PI-RATE-LAH-NUM        PIC XX.                  CL**8
00179              20  PI-RATE-LIMITS.                                  EL6564
00180                  24  PI-RATE-HIGH-AGE       PIC 99.               EL6564
00181                  24  PI-RATE-HIGH-AMT       PIC 9(6).             EL6564
00182                  24  PI-RATE-FUTURE         PIC XX.               EL6564
00183                  24  PI-RATE-SEX            PIC X.                EL6564
00184              20  PI-RATE-EXPIRY-DATE.                             EL6564
00185                  24  PI-RATE-EXP-YR         PIC 99.               EL6564
00186                  24  PI-RATE-EXP-MO         PIC 99.               EL6564
00187                  24  PI-RATE-EXP-DA         PIC 99.               EL6564
00188                                                                   EL6564
00189          16  PI-SAVE-ERRATE-KEY             PIC X(28).            EL6564
00190                                                                   EL6564
00191          16  PI-BROWSE-SW                  PIC X.                 EL6564
00192              88  BROWSE-STARTED                  VALUE 'Y'.       EL6564
00193          16  PI-ERRATE-EOF-SW              PIC X.                 EL6564
00194              88  ERRATE-EOF                      VALUE 'Y'.       EL6564
00195                                                                   EL6564
00196          16  PI-MAPNAME                    PIC X(8).              EL6564
00197              88  SCREEN-A                        VALUE 'EL6564A'. EL6564
00198              88  SCREEN-B                        VALUE 'EL6564B'. EL6564
00199              88  SCREEN-C                        VALUE 'EL6564C'. EL6564
00200          16  PI-DECIMAL-NUMBER             PIC 9(01).                CL*10
00201          16  FILLER                        PIC X(570).               CL*14
00202                                                                   EL6564
00203      EJECT                                                        EL6564
00204                              COPY ELCJPFX.                           CL*14
081413                             PIC X(2000).                            CL*13
00206                                                                   EL6564
00207      EJECT                                                        EL6564
00208                              COPY ELCAID.                            CL*14
00209  01  FILLER    REDEFINES DFHAID.                                  EL6564
00210      05  FILLER              PIC X(8).                            EL6564
00211      05  PF-VALUES           PIC X       OCCURS 2.                EL6564
00212                                                                   EL6564
00213      EJECT                                                        EL6564
00214                              COPY EL6564S.                           CL*14
00215                                                                   EL6564
00216  01  EL6564BO-R  REDEFINES EL6564BI.                              EL6564
00217      05  FILLER             PIC X(31).                            EL6564
00218      05  SCREEN-TABLE-B     OCCURS 30 TIMES                       EL6564
00219                             INDEXED BY STB-INDX.                  EL6564
00220          10  ST-RATE-L      PIC S9(4)         COMP.               EL6564
00221          10  ST-RATE-A      PIC X.                                EL6564
00222          10  ST-RATE        PIC 9(8).                             EL6564
00223          10  ST-RATE1       REDEFINES                             EL6564
00224              ST-RATE        PIC ZZ.99999.                         EL6564
00225                                                                   EL6564
00226          10  ST-MONTH-L     PIC S9(4)         COMP.               EL6564
00227          10  ST-MONTH-A     PIC X.                                EL6564
00228          10  ST-MONTH       PIC 999.                              EL6564
00229                                                                   EL6564
00230          10  END-RATE-L     PIC S9(4)         COMP.               EL6564
00231          10  END-RATE-A     PIC X.                                EL6564
00232          10  END-RATE       PIC 9(8).                             EL6564
00233          10  END-RATE1      REDEFINES                             EL6564
00234              END-RATE       PIC ZZ.99999.                         EL6564
00235                                                                   EL6564
00236          10  END-MONTH-L    PIC S9(4)         COMP.               EL6564
00237          10  END-MONTH-A    PIC X.                                EL6564
00238          10  END-MONTH      PIC 999.                              EL6564
00239      05  FILLER             PIC X(80).                               CL*14
00240                                                                   EL6564
00241  01  EL6564CO-R   REDEFINES EL6564CI.                             EL6564
00242      05  FILLER             PIC X(31).                            EL6564
00243      05  SCREEN-TABLE-C     OCCURS 30 TIMES                          CL**6
00244                             INDEXED BY STC-INDX.                  EL6564
00245          10  RATE-L         PIC S9(4)         COMP.               EL6564
00246          10  RATE-A         PIC X.                                EL6564
00247          10  RATE           PIC 9(8).                             EL6564
00248          10  RATE1          REDEFINES                             EL6564
00249              RATE           PIC ZZ.99999.                         EL6564
00250                                                                   EL6564
00251          10  ST-YEAR-L      PIC S9(4)         COMP.               EL6564
00252          10  ST-YEAR-A      PIC X.                                EL6564
00253          10  ST-YEAR        PIC 99.                               EL6564
00254                                                                   EL6564
00255          10  END-YEAR-L     PIC S9(4)         COMP.               EL6564
00256          10  END-YEAR-A     PIC X.                                EL6564
00257          10  END-YEAR       PIC 99.                               EL6564
00258                                                                      CL**6
00259      05  FILLER             PIC X(80).                               CL**6
00260      05  FACTOR-AREA.                                                CL**6
00261          10  FACT-L         PIC S9(4)         COMP.                  CL**6
00262          10  FACT-A         PIC X.                                   CL**6
00263          10  FACT           PIC 9(8).                                CL**6
00264          10  FACT1          REDEFINES                                CL**6
00265              FACT           PIC ZZ.99999.                            CL**6
00266      05  FILLER             PIC X(371).                              CL*14
00267                                                                   EL6564
00268      EJECT                                                        EL6564
00269                                                                   EL6564
00270  LINKAGE SECTION.                                                 EL6564
00271                                                                   EL6564
00272  01  DFHCOMMAREA             PIC X(1024).                         EL6564
00273                                                                   EL6564
00274 *01 PARMLIST .                                                       CL*14
00275 *    02  FILLER              PIC S9(8)   COMP.                       CL*14
00276 *    02  ERRATE-POINTER      PIC S9(8)   COMP.                       CL*14
00277 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL*14
00278      EJECT                                                        EL6564
00279                              COPY ERCRATE.                           CL*14
00280      EJECT                                                        EL6564
00281                              COPY ELCCNTL SUPPRESS.                  CL*14
00282      EJECT                                                        EL6564
00283                                                                   EL6564
00284  PROCEDURE DIVISION.                                              EL6564
00285                                                                   EL6564
00286      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL6564
00287      MOVE '5'                   TO DC-OPTION-CODE.                EL6564
00288      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6564
00289      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL6564
00290      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL6564
00291                                                                   EL6564
00292      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL6564
00293                                                                   EL6564
00294  0100-START.                                                      EL6564
00295      IF EIBCALEN = 0                                              EL6564
00296          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6564
00297                                                                   EL6564
00298                                                                      CL*11
00299      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6564
00300          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6564
00301              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL6564
00302              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL6564
00303              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL6564
00304              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL6564
00305              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL6564
00306              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL6564
00307              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL6564
00308              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL6564
00309              MOVE 5                    TO PI-DECIMAL-NUMBER          CL*11
00310          ELSE                                                     EL6564
00311              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL6564
00312              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL6564
00313              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL6564
00314              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL6564
00315              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL6564
00316              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL6564
00317              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL6564
00318              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL6564
00319                                                                   EL6564
00320      EXEC CICS HANDLE CONDITION                                   EL6564
00321          NOTOPEN  (9990-ABEND)                                    EL6564
00322          NOTFND   (8880-NOT-FOUND)                                EL6564
00323          PGMIDERR (9600-PGMID-ERROR)                              EL6564
00324          ERROR    (9990-ABEND)                                    EL6564
00325      END-EXEC.                                                    EL6564
00326                                                                   EL6564
00327      IF PI-FILE-ID = 'OE'                                         EL6564
00328         MOVE OE-RATE-FILE-ID TO RATE-FILE-ID.                     EL6564
00329                                                                   EL6564
00330      IF EIBTRNID NOT = TRANS-ID                                   EL6564
00331          MOVE LOW-VALUES     TO EL6564AI                             CL**4
00332          MOVE -1             TO AMETHODL                             CL**4
00333          MOVE 'EL6564A'      TO PI-MAPNAME                           CL**4
00334          GO TO 8100-SEND-INITIAL-MAP.                             EL6564
00335                                                                   EL6564
00336      IF EIBAID = DFHCLEAR                                         EL6564
00337          GO TO 9400-CLEAR.                                        EL6564
00338                                                                   EL6564
00339      IF NOT DISPLAY-CAP                                           EL6564
00340          MOVE 'UPDATE'        TO SM-READ                          EL6564
00341          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL6564
00342          MOVE ER-0070         TO  EMI-ERROR                          CL**4
00343          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6564
00344          GO TO 8100-SEND-INITIAL-MAP.                             EL6564
00345      EJECT                                                        EL6564
00346  0200-RECEIVE.                                                    EL6564
00347      IF SCREEN-A                                                  EL6564
00348          MOVE LOW-VALUES     TO EL6564AI                          EL6564
00349      ELSE                                                         EL6564
00350          IF SCREEN-B                                              EL6564
00351              MOVE LOW-VALUES TO EL6564BI                          EL6564
00352          ELSE                                                     EL6564
00353              MOVE LOW-VALUES TO EL6564CI.                         EL6564
00354                                                                   EL6564
00355      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6564
00356          MOVE ER-0008 TO EMI-ERROR                                EL6564
00357          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6564
00358          GO TO 8200-SEND-DATAONLY.                                EL6564
00359                                                                   EL6564
00360      EXEC CICS RECEIVE                                            EL6564
00361          MAP    (PI-MAPNAME)                                      EL6564
00362          MAPSET (WS-MAPSET-NAME)                                  EL6564
00363          INTO   (EL6564BI)                                        EL6564
00364      END-EXEC.                                                    EL6564
00365                                                                   EL6564
00366      IF SCREEN-B                                                  EL6564
00367          IF BPFKEYL = 0                                           EL6564
00368              GO TO 0300-CHECK-PFENTERS.                           EL6564
00369                                                                   EL6564
00370      IF SCREEN-C                                                  EL6564
00371          IF CPFKEYL = 0                                           EL6564
00372              GO TO 0300-CHECK-PFENTERS.                           EL6564
00373                                                                   EL6564
00374      IF EIBAID NOT = DFHENTER                                     EL6564
00375         IF SCREEN-A                                                  CL**3
00376            GO TO 0300-CHECK-PFENTERS                                 CL**3
00377          ELSE                                                        CL**4
00378            MOVE ER-0004 TO EMI-ERROR                                 CL**3
00379            GO TO 0310-INPUT-ERROR.                                   CL**3
00380                                                                   EL6564
00381      IF SCREEN-B                                                  EL6564
00382          IF (BPFKEYI NUMERIC) AND (BPFKEYI GREATER 0 AND LESS 25) EL6564
00383              MOVE PF-VALUES (BPFKEYI) TO EIBAID                   EL6564
00384              GO TO 0300-CHECK-PFENTERS                            EL6564
00385          ELSE                                                     EL6564
00386              MOVE ER-0029 TO EMI-ERROR                            EL6564
00387              GO TO 0310-INPUT-ERROR.                              EL6564
00388                                                                   EL6564
00389      IF SCREEN-C                                                  EL6564
00390          IF (CPFKEYI NUMERIC) AND (CPFKEYI GREATER 0 AND LESS 25) EL6564
00391              MOVE PF-VALUES (CPFKEYI) TO EIBAID                   EL6564
00392              GO TO 0300-CHECK-PFENTERS                            EL6564
00393          ELSE                                                     EL6564
00394              MOVE ER-0029 TO EMI-ERROR                            EL6564
00395              GO TO 0310-INPUT-ERROR.                              EL6564
00396                                                                   EL6564
00397      EJECT                                                        EL6564
00398                                                                   EL6564
00399  0300-CHECK-PFENTERS.                                             EL6564
00400      IF EIBAID = DFHPF23                                          EL6564
00401          GO TO 8810-PF23.                                         EL6564
00402                                                                   EL6564
00403      IF EIBAID = DFHPF24                                          EL6564
00404          GO TO 9200-RETURN-MAIN-MENU.                             EL6564
00405                                                                   EL6564
00406      IF EIBAID = DFHPF12                                          EL6564
00407          GO TO 9500-PF12.                                         EL6564
00408                                                                   EL6564
00409      IF SCREEN-B                                                  EL6564
00410          IF EIBAID = DFHPF1                                       EL6564
00411              GO TO 2000-UPDATE.                                   EL6564
00412                                                                   EL6564
00413      IF SCREEN-C                                                  EL6564
00414          IF EIBAID = DFHPF1 OR DFHPF2 OR DFHPF3
120206            OR DFHPF4
00415              GO TO 2000-UPDATE.                                   EL6564
00416                                                                      CL**5
00417      IF EIBAID = DFHENTER                                         EL6564
00418          GO TO 0320-MAINT.                                        EL6564
00419                                                                   EL6564
00420      MOVE ER-0029 TO EMI-ERROR.                                   EL6564
00421                                                                   EL6564
00422  0310-INPUT-ERROR.                                                EL6564
00423      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6564
00424                                                                   EL6564
00425      IF SCREEN-B                                                  EL6564
00426          MOVE AL-UNBON TO BPFKEYA                                 EL6564
00427          MOVE -1       TO BPFKEYL.                                EL6564
00428                                                                   EL6564
00429      IF SCREEN-C                                                  EL6564
00430          MOVE AL-UNBON TO CPFKEYA                                 EL6564
00431          MOVE -1       TO CPFKEYL.                                EL6564
00432                                                                   EL6564
00433      GO TO 8200-SEND-DATAONLY.                                    EL6564
00434                                                                   EL6564
00435  0320-MAINT.                                                      EL6564
00436      IF SCREEN-A                                                  EL6564
00437          PERFORM 5000-EDIT-SCREEN-A THRU 5099-EXIT.               EL6564
00438                                                                   EL6564
00439      IF SCREEN-B                                                  EL6564
00440          PERFORM 5200-EDIT-SCREEN-B THRU 5299-EXIT.               EL6564
00441                                                                   EL6564
00442      IF SCREEN-C                                                  EL6564
00443          PERFORM 5400-EDIT-SCREEN-C THRU 5499-EXIT.               EL6564
00444                                                                   EL6564
00445      IF EMI-FORCABLE-CTR = ZEROS  AND                                CL**9
00446         EMI-FATAL-CTR = ZEROS                                        CL**9
00447          NEXT SENTENCE                                            EL6564
00448        ELSE                                                          CL**4
00449          GO TO 8200-SEND-DATAONLY.                                EL6564
00450                                                                   EL6564
00451      IF SCREEN-B                                                  EL6564
00452         MOVE ER-2284 TO EMI-ERROR                                 EL6564
00453        ELSE                                                       EL6564
00454         MOVE ER-2903 TO EMI-ERROR.                                EL6564
00455                                                                   EL6564
00456      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6564
00457                                                                   EL6564
00458      IF SCREEN-B                                                  EL6564
00459          MOVE -1 TO BPFKEYL                                       EL6564
00460        ELSE                                                          CL**4
00461          IF SCREEN-C                                              EL6564
00462              MOVE -1 TO CPFKEYL.                                  EL6564
00463                                                                   EL6564
00464      GO TO 8100-SEND-INITIAL-MAP.                                 EL6564
00465                                                                   EL6564
00466      EJECT                                                        EL6564
00467                                                                   EL6564
00468  2000-UPDATE.                                                     EL6564
00469      IF SCREEN-B                                                  EL6564
00470          PERFORM 5200-EDIT-SCREEN-B THRU 5299-EXIT                EL6564
00471        ELSE                                                          CL**4
00472          IF SCREEN-C                                              EL6564
00473              PERFORM 5400-EDIT-SCREEN-C THRU 5499-EXIT.           EL6564
00474                                                                   EL6564
00475      IF EMI-FORCABLE-CTR = ZEROS  AND                                CL**9
00476         EMI-FATAL-CTR = ZEROS                                        CL**9
00477          NEXT SENTENCE                                            EL6564
00478        ELSE                                                          CL**4
00479          GO TO 8200-SEND-DATAONLY.                                EL6564
00480                                                                   EL6564
00481      PERFORM 7750-READ-ERRATE-UPDATE THRU 7750-EXIT.              EL6564
00482                                                                   EL6564
00483      MOVE RATE-RECORD TO JP-RECORD-AREA.                          EL6564
00484                                                                   EL6564
00485      IF SCREEN-B                                                  EL6564
00486          PERFORM 4000-GENERATE-RATES THRU 4099-EXIT               EL6564
00487        ELSE                                                          CL**4
00488          IF SCREEN-C                                              EL6564
00489              PERFORM 4200-GENERATE-RATES THRU 4299-EXIT.          EL6564
00490                                                                   EL6564
00491      IF RT-LAST-MAINT-USER   = PI-UPDATE-BY OR                    EL6564
00492         RT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL6564
00493          NEXT SENTENCE                                            EL6564
00494        ELSE                                                          CL**4
00495          EXEC CICS UNLOCK                                         EL6564
00496               DATASET  (RATE-FILE-ID)                             EL6564
00497          END-EXEC                                                 EL6564
00498          MOVE ER-0068 TO EMI-ERROR                                EL6564
00499          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6564
00500          GO TO 8200-SEND-DATAONLY.                                EL6564
00501                                                                   EL6564
00502      EJECT                                                        EL6564
00503      MOVE PI-PROCESSOR-ID     TO RT-LAST-MAINT-USER.              EL6564
00504      MOVE EIBTIME             TO RT-LAST-MAINT-HHMMSS.            EL6564
00505                                                                   EL6564
00506      MOVE SAVE-BIN-DATE       TO RT-LAST-MAINT-DT                 EL6564
00507                                  BIN-CURRENT-SAVE.                EL6564
00508      MOVE 'B'                 TO JP-RECORD-TYPE.                  EL6564
00509      MOVE RATE-FILE-ID        TO FILE-ID.                         EL6564
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00511      MOVE RATE-RECORD         TO JP-RECORD-AREA.                  EL6564
00512                                                                   EL6564
00513      EXEC CICS REWRITE                                            EL6564
00514          DATASET  (RATE-FILE-ID)                                  EL6564
00515          FROM     (RATE-RECORD)                                   EL6564
00516      END-EXEC.                                                    EL6564
00517                                                                   EL6564
00518      MOVE 'C'                 TO JP-RECORD-TYPE.                  EL6564
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00520      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL6564
00521      MOVE ER-0000 TO EMI-ERROR.                                   EL6564
00522      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6564
00523                                                                   EL6564
00524      MOVE XCTL-6562 TO PGM-NAME.                                  EL6564
00525      GO TO 9300-XCTL.                                             EL6564
00526                                                                   EL6564
00527  2000-EXIT.                                                       EL6564
00528      EXIT.                                                        EL6564
00529      EJECT                                                        EL6564
00530                                                                   EL6564
00531  4000-GENERATE-RATES.                                             EL6564
00532      MOVE +1 TO SUB1                                              EL6564
00533                 SUB2                                              EL6564
00534                 SUB3.                                             EL6564
00535                                                                   EL6564
00536  4025-CONSTRUCT-RATES.                                            EL6564
00537      IF (SUB1 GREATER +30)  OR                                    EL6564
00538         (SUB2 GREATER +360)                                       EL6564
00539              MOVE +1 TO SUB1                                      EL6564
00540                         SUB2                                      EL6564
00541              GO TO 4070-PUT-LAST-RATE.                            EL6564
00542                                                                   EL6564
00543      IF WS-ST-RATE (SUB1) = ZEROS                                 EL6564
00544          ADD +1 TO SUB1                                           EL6564
00545          GO TO 4025-CONSTRUCT-RATES.                              EL6564
00546                                                                   EL6564
00547      MOVE ZEROS TO WS-RATE                                        EL6564
00548                    WS-RATE-DIFF                                   EL6564
00549                    WS-MO-DIFF                                     EL6564
00550                    WS-MO-RATE.                                    EL6564
00551                                                                   EL6564
00552      MOVE +0    TO SUB3.                                          EL6564
00553                                                                   EL6564
00554      SUBTRACT WS-ST-RATE (SUB1) FROM WS-END-RATE (SUB1)           EL6564
00555          GIVING WS-RATE-DIFF.                                     EL6564
00556                                                                   EL6564
00557      SUBTRACT WS-ST-MONTH (SUB1) FROM WS-END-MONTH (SUB1)         EL6564
00558          GIVING WS-MO-DIFF.                                       EL6564
00559                                                                   EL6564
00560      DIVIDE WS-RATE-DIFF BY WS-MO-DIFF GIVING WS-MO-RATE.         EL6564
00561                                                                   EL6564
00562  4050-ENTER-RATES.                                                EL6564
00563      IF SUB2 = WS-END-MONTH (SUB1)                                EL6564
00564         MOVE WS-END-MONTH (SUB1) TO WS-SAVE-END-MONTH                CL*12
00565         MOVE WS-END-RATE (SUB1)  TO DEEDIT-FIELD-V5                  CL*12
00566         PERFORM 6000-ROUND-AS-REQUESTED THRU 6000-EXIT               CL*12
00567         MOVE DEEDIT-FIELD-V5     TO WS-SAVE-END-RATE.                CL*12
00568                                                                   EL6564
00569      IF SUB2 GREATER    WS-ST-MONTH (SUB1) OR                     EL6564
00570         SUB2 =          WS-ST-MONTH (SUB1)                        EL6564
00571            IF  SUB2  LESS   WS-END-MONTH (SUB1)                   EL6564
00572 *          IF  SUB2  LESS   WS-END-MONTH (SUB1) OR                EL6564
00573 *              SUB2  =      WS-END-MONTH (SUB1)                   EL6564
00574                   NEXT SENTENCE                                   EL6564
00575            ELSE                                                   EL6564
00576                ADD +1 TO SUB1                                     EL6564
00577                GO TO 4025-CONSTRUCT-RATES                         EL6564
00578                                                                      CL*12
00579      ELSE                                                         EL6564
00580          ADD +1 TO SUB2                                           EL6564
00581          GO TO 4050-ENTER-RATES.                                  EL6564
00582                                                                   EL6564
00583      MOVE ZEROS TO WS-RATE.                                       EL6564
00584      MULTIPLY WS-MO-RATE BY SUB3 GIVING WS-RATE ROUNDED.          EL6564
00585      ADD WS-ST-RATE (SUB1) TO WS-RATE.                            EL6564
00586                                                                   EL6564
00587      MOVE WS-RATE                TO DEEDIT-FIELD-V5.                 CL*10
00588      PERFORM 6000-ROUND-AS-REQUESTED THRU 6000-EXIT.                 CL*10
00589      MOVE DEEDIT-FIELD-V5        TO WS-RATE.                         CL*10
00590                                                                      CL*10
00591      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                          EL6564
00592          MOVE WS-RATE TO RT-AH-RATE (SUB2).                       EL6564
00593                                                                   EL6564
00594      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                        EL6564
00595          MOVE WS-RATE TO RT-L-RATE (SUB2).                        EL6564
00596                                                                   EL6564
00597      ADD +1 TO SUB2                                               EL6564
00598                SUB3.                                              EL6564
00599                                                                   EL6564
00600      GO TO 4050-ENTER-RATES.                                      EL6564
00601                                                                   EL6564
00602  4070-PUT-LAST-RATE.                                              EL6564
00603      IF WS-SAVE-END-MONTH NOT NUMERIC OR                             CL**2
00604         WS-SAVE-END-MONTH = ZEROS                                    CL**4
00605         GO TO 4099-EXIT.                                             CL**2
00606                                                                      CL**2
00607      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                          EL6564
00608          MOVE WS-SAVE-END-RATE TO RT-AH-RATE (WS-SAVE-END-MONTH). EL6564
00609                                                                   EL6564
00610      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                        EL6564
00611          MOVE WS-SAVE-END-RATE TO RT-L-RATE (WS-SAVE-END-MONTH).  EL6564
00612                                                                   EL6564
00613  4099-EXIT.                                                       EL6564
00614      EXIT.                                                        EL6564
00615      EJECT                                                        EL6564
00616                                                                   EL6564
00617  4200-GENERATE-RATES.                                             EL6564
00618                                                                      CL**7
00619      IF EIBAID = DFHPF1                                              CL**7
00620          MOVE 'S'  TO RT-TYPE-RATE                                   CL**7
00621       ELSE                                                           CL**7
00622          MOVE 'O'  TO RT-TYPE-RATE.                                  CL**7
00623                                                                      CL**7
00624      MOVE +1 TO SUB1                                              EL6564
00625                 SUB2                                              EL6564
00626                 SUB3.                                             EL6564
00627                                                                   EL6564
00628  4225-CONSTRUCT-RATES.                                            EL6564
00629      IF (SUB1  GREATER +30)   OR                                  EL6564
00630         (SUB2  GREATER +360)                                      EL6564
00631              MOVE +1 TO SUB1                                      EL6564
00632                         SUB2                                      EL6564
00633              GO TO 4299-EXIT.                                     EL6564
00634                                                                   EL6564
00635      IF WS-RATES (SUB1) = ZEROS                                   EL6564
00636          ADD +1 TO SUB1                                           EL6564
00637          GO TO 4225-CONSTRUCT-RATES.                              EL6564
00638                                                                   EL6564
00639      MOVE ZEROS                  TO WS-MO-RATE
           IF EIBAID = DFHPF4
              MOVE WS-RATES (SUB1)     TO WS-MO-RATE
           ELSE
00640         DIVIDE WS-RATES (SUB1) BY +12 GIVING WS-MO-RATE
           END-IF
00641                                                                   EL6564
00642      COMPUTE                                                      EL6564
00643         WS-START-MO (SUB1) = (((WS-ST-YEAR (SUB1) - 1) * 12) + 1).EL6564
00644                                                                   EL6564
00645      COMPUTE                                                      EL6564
00646         WS-END-MO (SUB1) = (WS-END-YEAR (SUB1) * 12).             EL6564
00647                                                                   EL6564
00648      MOVE WS-START-MO (SUB1) TO SUB3.                             EL6564
00649                                                                   EL6564
00650  4250-ENTER-RATES.                                                EL6564
00651      IF SUB2 GREATER    WS-START-MO (SUB1)   OR                   EL6564
00652         SUB2 =          WS-START-MO (SUB1)                        EL6564
00653            IF  SUB2  LESS   WS-END-MO (SUB1)     OR               EL6564
00654                SUB2  =      WS-END-MO (SUB1)                      EL6564
00655                   NEXT SENTENCE                                   EL6564
00656                 ELSE                                                 CL**4
00657                   ADD +1 TO SUB1                                     CL**4
00658                   GO TO 4225-CONSTRUCT-RATES                         CL**4
00659        ELSE                                                          CL**4
00660          ADD +1 TO SUB2                                           EL6564
00661          GO TO 4250-ENTER-RATES.                                  EL6564
00662                                                                   EL6564
00663      MOVE ZEROS TO WS-RATE.                                       EL6564
00664                                                                   EL6564
           EVALUATE EIBAID
              WHEN DFHPF1
                 COMPUTE WS-RATE ROUNDED = (WS-MO-RATE * SUB3)
              WHEN DFHPF2
                 COMPUTE WS-RATE ROUNDED =
                    (WS-MO-RATE * (SUB3 + 1) * .6)
              WHEN DFHPF3
                 COMPUTE WS-RATE ROUNDED =
                    (WS-MO-RATE * (SUB3 + 1) * .6)
                    / (1 + WS-DFACT * SUB2)
              WHEN DFHPF4
                 COMPUTE WS-RATE ROUNDED = (((SUB3 + 1) * WS-MO-RATE)
                    / (20 * (1 + 0.0019 * SUB3)) - .00005)
           END-EVALUATE

00675      MOVE WS-RATE                TO DEEDIT-FIELD-V5.                 CL*10
00676      PERFORM 6000-ROUND-AS-REQUESTED THRU 6000-EXIT.                 CL*10
00677      MOVE DEEDIT-FIELD-V5        TO WS-RATE.                         CL*10
00678                                                                      CL*10
00679      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                          EL6564
00680          MOVE WS-RATE TO RT-AH-RATE (SUB2).                       EL6564
00681                                                                   EL6564
00682      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                        EL6564
00683          MOVE WS-RATE TO RT-L-RATE (SUB2).                        EL6564
00684                                                                   EL6564
00685      ADD +1 TO SUB2                                               EL6564
00686                SUB3.                                              EL6564
00687                                                                      CL**4
00688      GO TO 4250-ENTER-RATES.                                      EL6564
00689                                                                   EL6564
00690  4299-EXIT.                                                       EL6564
00691      EXIT.                                                        EL6564
00692      EJECT                                                        EL6564
00693                                                                   EL6564
00694  5000-EDIT-SCREEN-A.                                              EL6564
00695                                                                      CL*10
00696      IF  DECINUML GREATER ZERO                                       CL*10
00697          EXEC CICS BIF DEEDIT                                        CL*10
00698              FIELD   (DECINUMI)                                      CL*10
00699              LENGTH  (1)                                             CL*10
00700              END-EXEC                                                CL*10
00701                                                                      CL*10
00702          IF  DECINUMI NOT NUMERIC                                    CL*10
00703                  OR                                                  CL*11
00704              DECINUMI GREATER THAN 5                                 CL*11
00705                  OR                                                  CL*11
00706              DECINUMI LESS THAN 1                                    CL*11
00707              MOVE -1             TO DECINUML                         CL*10
00708              MOVE AL-UNBON       TO DECINUMA                         CL*10
00709              MOVE ER-7743        TO EMI-ERROR                        CL*10
00710              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*10
00711                                                                      CL*10
00712          ELSE                                                        CL*10
00713              MOVE DECINUMI       TO PI-DECIMAL-NUMBER                CL*11
00714                                                                      CL*11
00715      ELSE                                                            CL*11
00716          MOVE PI-DECIMAL-NUMBER  TO DECINUMO.                        CL*11
00717                                                                      CL*10
00718      IF AMETHODL GREATER ZERO                                     EL6564
00719          IF AMETHODI = 'A'                                        EL6564
00720              MOVE 'EL6564B'  TO PI-MAPNAME                           CL**4
00721                                                                      CL*11
00722          ELSE                                                     EL6564
00723              IF AMETHODI = 'B'                                    EL6564
00724                  MOVE 'EL6564C'  TO PI-MAPNAME                       CL**4
00725                                                                      CL*11
00726              ELSE                                                 EL6564
00727                  MOVE -1       TO AMETHODL                        EL6564
00728                  MOVE AL-UABON TO AMETHODA                        EL6564
00729                  MOVE ER-2286  TO EMI-ERROR                       EL6564
00730                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6564
00731      ELSE                                                         EL6564
00732          MOVE -1       TO AMETHODL                                EL6564
00733          MOVE AL-UABON TO AMETHODA                                EL6564
00734          MOVE ER-2286  TO EMI-ERROR                               EL6564
00735          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL6564
00736                                                                   EL6564
00737      IF  EMI-ERROR GREATER THAN ZEROS                                CL*11
00738          MOVE 'EL6564A'  TO PI-MAPNAME                               CL*11
00739          GO TO 8200-SEND-DATAONLY                                    CL*11
00740                                                                      CL*11
00741      ELSE                                                            CL*11
00742          MOVE LOW-VALUES TO EL6564BO                                 CL*11
00743                             EL6564CO                                 CL*14
00744          GO TO 8100-SEND-INITIAL-MAP.                                CL*11
00745                                                                      CL*11
00746                                                                   EL6564
00747  5099-EXIT.                                                       EL6564
00748      EXIT.                                                        EL6564
00749      EJECT                                                        EL6564
00750                                                                   EL6564
00751  5200-EDIT-SCREEN-B.                                              EL6564
00752       SET STB-INDX TO +1.                                         EL6564
00753       MOVE +1  TO  SUB1.                                          EL6564
00754                                                                   EL6564
00755  5250-CONT.                                                       EL6564
00756      IF STB-INDX GREATER +30                                      EL6564
00757          IF SUB1 GREATER +30                                      EL6564
00758              GO TO 5299-EXIT                                      EL6564
00759          ELSE                                                     EL6564
00760              MOVE ZEROS TO WS-ST-RATE   (SUB1)                       CL**4
00761                            WS-END-RATE  (SUB1)                       CL**4
00762                            WS-ST-MONTH  (SUB1)                       CL**4
00763                            WS-END-MONTH (SUB1)                    EL6564
00764              ADD +1 TO SUB1                                       EL6564
00765              GO TO 5250-CONT.                                     EL6564
00766                                                                   EL6564
00767      IF  ST-RATE-L (STB-INDX) GREATER ZERO                           CL*10
00768          MOVE ST-RATE (STB-INDX) TO DEEDIT-FIELD                  EL6564
00769          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL6564
00770                                                                      CL*10
00771          IF  DEEDIT-FIELD-V0 NUMERIC                                 CL*10
00772              MOVE DEEDIT-FIELD-V5                                    CL*10
00773                                  TO WS-ST-RATE (SUB1)                CL*10
00774                                     ST-RATE1 (STB-INDX)              CL*10
00775              MOVE AL-UNNON       TO ST-RATE-A (STB-INDX)             CL*10
00776                                                                      CL*10
00777          ELSE                                                     EL6564
00778              MOVE -1       TO ST-RATE-L (STB-INDX)                EL6564
00779              MOVE AL-UNBON TO ST-RATE-A (STB-INDX)                EL6564
00780              MOVE ER-2280  TO EMI-ERROR                           EL6564
00781              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6564
00782      ELSE                                                         EL6564
00783          IF ST-MONTH-L  (STB-INDX) GREATER ZERO OR                   CL**4
00784             END-MONTH-L (STB-INDX) GREATER ZERO                   EL6564
00785                 PERFORM 5300-CHECK-LAST-RATE THRU 5399-EXIT       EL6564
00786          ELSE                                                     EL6564
00787              MOVE ZEROS TO WS-ST-RATE (SUB1).                     EL6564
00788                                                                   EL6564
00789      IF ST-MONTH-L (STB-INDX) GREATER ZERO                        EL6564
00790          IF ST-MONTH (STB-INDX) NUMERIC                           EL6564
00791              IF ST-MONTH (STB-INDX) GREATER +360                  EL6564
00792                  MOVE -1       TO ST-MONTH-L (STB-INDX)           EL6564
00793                  MOVE AL-UNBON TO ST-MONTH-A (STB-INDX)           EL6564
00794                  MOVE ER-2290  TO EMI-ERROR                       EL6564
00795                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6564
00796              ELSE                                                 EL6564
00797                  IF ST-MONTH (STB-INDX) LESS WS-PREV-MONTH        EL6564
00798                      MOVE -1       TO ST-MONTH-L (STB-INDX)       EL6564
00799                      MOVE AL-UNBON TO ST-MONTH-A (STB-INDX)       EL6564
00800                      MOVE ER-2282  TO EMI-ERROR                   EL6564
00801                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL6564
00802                  ELSE                                             EL6564
00803                      MOVE ST-MONTH (STB-INDX) TO                  EL6564
00804                                       WS-ST-MONTH (SUB1)          EL6564
00805                                       WS-PREV-MONTH               EL6564
00806                      MOVE AL-UNNON TO ST-MONTH-A (STB-INDX)       EL6564
00807          ELSE                                                     EL6564
00808              MOVE -1       TO ST-MONTH-L (STB-INDX)               EL6564
00809              MOVE AL-UNBON TO ST-MONTH-A (STB-INDX)               EL6564
00810              MOVE ER-2290  TO EMI-ERROR                           EL6564
00811              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6564
00812      ELSE                                                         EL6564
00813          IF ST-RATE-L (STB-INDX) GREATER ZERO                     EL6564
00814              MOVE -1       TO ST-MONTH-L (STB-INDX)               EL6564
00815              MOVE AL-UNBON TO ST-MONTH-A (STB-INDX)               EL6564
00816              MOVE ER-2290  TO EMI-ERROR                           EL6564
00817              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6564
00818          ELSE                                                     EL6564
00819              MOVE ZEROS TO WS-ST-MONTH (SUB1).                    EL6564
00820                                                                   EL6564
00821      IF  END-RATE-L (STB-INDX) GREATER ZERO                          CL*10
00822          MOVE END-RATE (STB-INDX) TO DEEDIT-FIELD                 EL6564
00823          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL6564
00824                                                                      CL*10
00825          IF  DEEDIT-FIELD-V0 NUMERIC                                 CL*10
00826              MOVE DEEDIT-FIELD-V5                                    CL*10
00827                                  TO WS-END-RATE (SUB1)               CL*10
00828                                     END-RATE1 (STB-INDX)             CL*10
00829                                                                      CL*10
00830              IF  WS-END-RATE (SUB1) GREATER WS-ST-RATE (SUB1)        CL*10
00831                  MOVE AL-UNNON        TO END-RATE-A (STB-INDX)    EL6564
00832                                                                      CL*10
00833              ELSE                                                 EL6564
00834                  MOVE -1       TO END-RATE-L (STB-INDX)           EL6564
00835                  MOVE AL-UNBON TO END-RATE-A (STB-INDX)           EL6564
00836                  MOVE ER-2281  TO EMI-ERROR                       EL6564
00837                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6564
00838          ELSE                                                     EL6564
00839              MOVE -1       TO END-RATE-L (STB-INDX)               EL6564
00840              MOVE AL-UNBON TO END-RATE-A (STB-INDX)               EL6564
00841              MOVE ER-2280  TO EMI-ERROR                           EL6564
00842              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6564
00843      ELSE                                                         EL6564
00844          IF ST-RATE-L (STB-INDX) GREATER ZERO                     EL6564
00845              MOVE -1       TO END-RATE-L (STB-INDX)               EL6564
00846              MOVE AL-UNBON TO END-RATE-A (STB-INDX)               EL6564
00847              MOVE ER-2280  TO EMI-ERROR                           EL6564
00848              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6564
00849          ELSE                                                     EL6564
00850              MOVE ZEROS TO WS-END-RATE (SUB1).                    EL6564
00851                                                                   EL6564
00852      IF END-MONTH-L (STB-INDX) GREATER ZERO                       EL6564
00853          IF END-MONTH (STB-INDX) NUMERIC                          EL6564
00854              IF END-MONTH (STB-INDX) GREATER +360                 EL6564
00855                  MOVE -1       TO END-MONTH-L (STB-INDX)          EL6564
00856                  MOVE AL-UNBON TO END-MONTH-A (STB-INDX)          EL6564
00857                  MOVE ER-2290  TO EMI-ERROR                       EL6564
00858                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6564
00859              ELSE                                                 EL6564
00860                  IF END-MONTH (STB-INDX) GREATER WS-PREV-MONTH    EL6564
00861                      MOVE END-MONTH (STB-INDX) TO                 EL6564
00862                                      WS-END-MONTH (SUB1)          EL6564
00863                                      WS-PREV-MONTH                EL6564
00864                      MOVE AL-UNNON TO END-MONTH-A (STB-INDX)      EL6564
00865                  ELSE                                             EL6564
00866                      MOVE -1       TO END-MONTH-L (STB-INDX)      EL6564
00867                      MOVE AL-UNBON TO END-MONTH-A (STB-INDX)      EL6564
00868                      MOVE ER-2292  TO EMI-ERROR                   EL6564
00869                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL6564
00870          ELSE                                                     EL6564
00871              MOVE -1       TO END-MONTH-L (STB-INDX)              EL6564
00872              MOVE AL-UNBON TO END-MONTH-A (STB-INDX)              EL6564
00873              MOVE ER-2290  TO EMI-ERROR                           EL6564
00874              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6564
00875      ELSE                                                         EL6564
00876          IF ST-RATE-L (STB-INDX) GREATER ZERO                     EL6564
00877              MOVE -1       TO END-MONTH-L (STB-INDX)              EL6564
00878              MOVE AL-UNBON TO END-MONTH-A (STB-INDX)              EL6564
00879              MOVE ER-2290  TO EMI-ERROR                           EL6564
00880              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6564
00881          ELSE                                                     EL6564
00882              MOVE ZEROS TO WS-END-MONTH (SUB1).                   EL6564
00883                                                                   EL6564
00884      SET STB-INDX UP BY +1.                                       EL6564
00885      ADD +1 TO SUB1.                                              EL6564
00886                                                                   EL6564
00887      GO TO 5250-CONT.                                             EL6564
00888                                                                   EL6564
00889  5299-EXIT.                                                       EL6564
00890      EXIT.                                                        EL6564
00891      EJECT                                                        EL6564
00892  5300-CHECK-LAST-RATE.                                            EL6564
00893      IF SUB1 = 1                                                  EL6564
00894           MOVE -1       TO ST-RATE-L (SUB1)                       EL6564
00895           MOVE AL-UNBON TO ST-RATE-A (SUB1)                       EL6564
00896           MOVE ER-2280  TO EMI-ERROR                              EL6564
00897           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                EL6564
00898           MOVE ZEROS    TO ST-RATE1     (SUB1)                    EL6564
00899                            WS-ST-RATE   (SUB1)                    EL6564
00900           GO TO 5399-EXIT.                                        EL6564
00901                                                                   EL6564
00902      COMPUTE LAST-INDX = SUB1 - 1.                                EL6564
00903      MOVE WS-END-RATE (LAST-INDX) TO WS-ST-RATE (SUB1)            EL6564
00904                                      ST-RATE1   (SUB1).           EL6564
00905      MOVE AL-UNNON   TO ST-RATE-A (SUB1).                         EL6564
00906                                                                      CL**4
00907  5399-EXIT.                                                       EL6564
00908      EXIT.                                                        EL6564
00909      EJECT                                                        EL6564
00910                                                                   EL6564
00911  5400-EDIT-SCREEN-C.                                              EL6564
00912       SET STC-INDX TO +1.                                         EL6564
00913       MOVE +1  TO  SUB1.                                          EL6564
00914                                                                   EL6564
00915  5450-CONT.                                                       EL6564
00916      IF STC-INDX GREATER +30                                      EL6564
00917          IF SUB1 GREATER +30                                      EL6564
00918              GO TO 5490-DFACT                                        CL**4
00919          ELSE                                                     EL6564
00920              MOVE ZEROS TO WS-RATES    (SUB1)                        CL**4
00921                            WS-ST-YEAR  (SUB1)                        CL**4
00922                            WS-END-YEAR (SUB1)                     EL6564
00923              ADD +1 TO SUB1                                       EL6564
00924              GO TO 5450-CONT.                                     EL6564
00925                                                                   EL6564
00926      IF  RATE-L (STC-INDX) GREATER ZERO                              CL*10
00927          MOVE RATE (STC-INDX) TO DEEDIT-FIELD                     EL6564
00928          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL6564
00929                                                                      CL*10
00930          IF  DEEDIT-FIELD-V0 NUMERIC                                 CL*10
00931              MOVE DEEDIT-FIELD-V5                                    CL*10
00932                                  TO WS-RATES (SUB1)                  CL*10
00933                                     RATE1  (STC-INDX)                CL*10
00934              MOVE AL-UNNON       TO RATE-A (STC-INDX)                CL*10
00935                                                                      CL*10
00936          ELSE                                                     EL6564
00937              MOVE -1       TO RATE-L (STC-INDX)                   EL6564
00938              MOVE AL-UNBON TO RATE-A (STC-INDX)                   EL6564
00939              MOVE ER-2280  TO EMI-ERROR                           EL6564
00940              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6564
00941      ELSE                                                         EL6564
00942          IF ST-YEAR-L (STC-INDX)  GREATER ZERO OR                 EL6564
00943             END-YEAR-L (STC-INDX) GREATER ZERO                    EL6564
00944                 MOVE -1       TO RATE-L (STC-INDX)                EL6564
00945                 MOVE AL-UNBON TO RATE-A (STC-INDX)                EL6564
00946                 MOVE ER-2280  TO EMI-ERROR                        EL6564
00947                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          EL6564
00948          ELSE                                                     EL6564
00949              MOVE ZEROS TO WS-RATES (SUB1).                       EL6564
00950                                                                   EL6564
00951      IF ST-YEAR-L (STC-INDX) GREATER ZERO                         EL6564
00952          IF ST-YEAR (STC-INDX) NUMERIC                            EL6564
00953              MOVE ST-YEAR (STC-INDX) TO WS-CHECK-YEAR             EL6564
00954              IF VALID-YEAR                                        EL6564
00955                  IF ST-YEAR (STC-INDX) GREATER WS-PREV-YEAR       EL6564
00956                      MOVE ST-YEAR (STC-INDX) TO                   EL6564
00957                                       WS-ST-YEAR (SUB1)           EL6564
00958                                       WS-PREV-YEAR                EL6564
00959                      MOVE AL-UNNON TO ST-YEAR-A (STC-INDX)        EL6564
00960                  ELSE                                             EL6564
00961                      MOVE -1       TO ST-YEAR-L (STC-INDX)        EL6564
00962                      MOVE AL-UNBON TO ST-YEAR-A (STC-INDX)        EL6564
00963                      MOVE ER-2288  TO EMI-ERROR                   EL6564
00964                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL6564
00965              ELSE                                                 EL6564
00966                  MOVE -1       TO ST-YEAR-L (STC-INDX)            EL6564
00967                  MOVE AL-UNBON TO ST-YEAR-A (STC-INDX)            EL6564
00968                  MOVE ER-2289  TO EMI-ERROR                       EL6564
00969                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6564
00970          ELSE                                                     EL6564
00971              MOVE -1       TO ST-YEAR-L (STC-INDX)                EL6564
00972              MOVE AL-UNBON TO ST-YEAR-A (STC-INDX)                EL6564
00973              MOVE ER-2289  TO EMI-ERROR                           EL6564
00974              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6564
00975      ELSE                                                         EL6564
00976          IF RATE-L (STC-INDX) GREATER ZERO                        EL6564
00977              MOVE -1       TO ST-YEAR-L (STC-INDX)                EL6564
00978              MOVE AL-UNBON TO ST-YEAR-A (STC-INDX)                EL6564
00979              MOVE ER-2289  TO EMI-ERROR                           EL6564
00980              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6564
00981          ELSE                                                     EL6564
00982              MOVE ZEROS TO WS-ST-YEAR (SUB1).                     EL6564
00983                                                                   EL6564
00984      IF END-YEAR-L (STC-INDX) GREATER ZERO                        EL6564
00985          IF END-YEAR (STC-INDX) NUMERIC                           EL6564
00986              MOVE END-YEAR (STC-INDX) TO WS-CHECK-YEAR            EL6564
00987              IF VALID-YEAR                                        EL6564
00988                  IF END-YEAR (STC-INDX) LESS WS-PREV-YEAR            CL**4
00989                      MOVE -1       TO END-YEAR-L (STC-INDX)       EL6564
00990                      MOVE AL-UNBON TO END-YEAR-A (STC-INDX)       EL6564
00991                      MOVE ER-2291  TO EMI-ERROR                   EL6564
00992                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL6564
00993                  ELSE                                             EL6564
00994                      MOVE END-YEAR (STC-INDX) TO                  EL6564
00995                                      WS-END-YEAR (SUB1)           EL6564
00996                                      WS-PREV-YEAR                 EL6564
00997                      MOVE AL-UNNON TO END-YEAR-A (STC-INDX)       EL6564
00998              ELSE                                                 EL6564
00999                  MOVE -1       TO END-YEAR-L (STC-INDX)           EL6564
01000                  MOVE AL-UNBON TO END-YEAR-A (STC-INDX)           EL6564
01001                  MOVE ER-2289  TO EMI-ERROR                       EL6564
01002                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6564
01003          ELSE                                                     EL6564
01004              MOVE -1       TO END-YEAR-L (STC-INDX)               EL6564
01005              MOVE AL-UNBON TO END-YEAR-A (STC-INDX)               EL6564
01006              MOVE ER-2289  TO EMI-ERROR                           EL6564
01007              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6564
01008      ELSE                                                         EL6564
01009          IF RATE-L (STC-INDX) GREATER ZERO                        EL6564
01010              MOVE -1       TO END-YEAR-L (STC-INDX)               EL6564
01011              MOVE AL-UNBON TO END-YEAR-A (STC-INDX)               EL6564
01012              MOVE ER-2289  TO EMI-ERROR                           EL6564
01013              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6564
01014          ELSE                                                     EL6564
01015              MOVE ZEROS TO WS-END-YEAR (SUB1).                    EL6564
01016                                                                   EL6564
01017      SET STC-INDX UP BY +1.                                       EL6564
01018      ADD +1 TO SUB1.                                              EL6564
01019                                                                   EL6564
01020      GO TO 5450-CONT.                                             EL6564
01021                                                                      CL**4
01022  5490-DFACT.                                                         CL**4
01023                                                                      CL**6
01024      MOVE ZERO                    TO WS-DFACT.                       CL*10
01025                                                                      CL*10
01026      IF  FACT-L GREATER ZERO                                         CL*10
01027                                                                      CL*10
01028          MOVE FACT                TO DEEDIT-FIELD                    CL*10
01029          PERFORM 7500-DEEDIT THRU 7500-EXIT                          CL**4
01030                                                                      CL*10
01031          IF  DEEDIT-FIELD-V0 NUMERIC                                 CL*10
01032              MOVE DEEDIT-FIELD-V5 TO FACT1                           CL*10
01033                                      WS-DFACT                        CL**6
01034              COMPUTE WS-DFACT = WS-DFACT / +24                       CL**5
01035              MOVE AL-UNNON        TO FACT-A                          CL**6
01036                                                                      CL*10
01037           ELSE                                                       CL**4
01038              MOVE -1              TO FACT-L                          CL**6
01039              MOVE AL-UNBON        TO FACT-A                          CL**6
01040              MOVE ER-2280         TO EMI-ERROR                       CL**4
01041              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**6
01042                                                                      CL**6
01043      IF EIBAID = DFHPF3                                              CL**6
01044          IF WS-DFACT = ZERO                                          CL**6
01045              MOVE -1              TO FACT-L                          CL**6
01046              MOVE AL-UNBON        TO FACT-A                          CL**6
01047              MOVE ER-7400         TO EMI-ERROR                       CL**6
01048              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**6
01049                                                                   EL6564
01050  5499-EXIT.                                                       EL6564
01051      EXIT.                                                        EL6564
01052                                  EJECT                               CL*10
01053  6000-ROUND-AS-REQUESTED.                                            CL*10
01054                                                                      CL*10
01055      IF  PI-DECIMAL-NUMBER EQUAL 5                                   CL*10
01056          GO TO 6000-EXIT.                                            CL*10
01057                                                                      CL*10
01058      IF  PI-DECIMAL-NUMBER EQUAL 0                                   CL*10
01059          COMPUTE W-FIELD-V0 ROUNDED                                  CL*10
01060              = DEEDIT-FIELD-V5                                       CL*10
01061          MOVE W-FIELD-V0         TO DEEDIT-FIELD-V5                  CL*10
01062                                                                      CL*10
01063      ELSE                                                            CL*10
01064          IF  PI-DECIMAL-NUMBER EQUAL 1                               CL*10
01065              COMPUTE W-FIELD-V1 ROUNDED                              CL*10
01066                  = DEEDIT-FIELD-V5                                   CL*10
01067              MOVE W-FIELD-V1     TO DEEDIT-FIELD-V5                  CL*10
01068                                                                      CL*10
01069          ELSE                                                        CL*10
01070              IF  PI-DECIMAL-NUMBER EQUAL 2                           CL*10
01071                  COMPUTE W-FIELD-V2 ROUNDED                          CL*12
01072                      = DEEDIT-FIELD-V5                               CL*10
01073                  MOVE W-FIELD-V2 TO DEEDIT-FIELD-V5                  CL*10
01074                                                                      CL*10
01075              ELSE                                                    CL*10
01076                  IF  PI-DECIMAL-NUMBER EQUAL 3                       CL*10
01077                      COMPUTE W-FIELD-V3 ROUNDED                      CL*10
01078                          = DEEDIT-FIELD-V5                           CL*10
01079                      MOVE W-FIELD-V3                                 CL*10
01080                                  TO DEEDIT-FIELD-V5                  CL*10
01081                                                                      CL*10
01082                  ELSE                                                CL*10
01083                      IF  PI-DECIMAL-NUMBER EQUAL 4                   CL*10
01084                          COMPUTE W-FIELD-V4 ROUNDED                  CL*10
01085                              = DEEDIT-FIELD-V5                       CL*10
01086                          MOVE W-FIELD-V4                             CL*10
01087                                  TO DEEDIT-FIELD-V5.                 CL*10
01088                                                                      CL*10
01089  6000-EXIT.                                                          CL*10
01090      EXIT.                                                           CL*10
01091                                  EJECT                               CL*10
01092                                                                   EL6564
01093  7500-DEEDIT.                                                     EL6564
01094      EXEC CICS BIF                                                EL6564
01095           DEEDIT                                                  EL6564
01096           FIELD  (DEEDIT-FIELD)                                   EL6564
01097           LENGTH (15)                                             EL6564
01098      END-EXEC.                                                    EL6564
01099                                                                   EL6564
01100  7500-EXIT.                                                       EL6564
01101      EXIT.                                                        EL6564
01102      EJECT                                                        EL6564
01103                                                                   EL6564
01104  7750-READ-ERRATE-UPDATE.                                         EL6564
01105      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.                  EL6564
01106                                                                   EL6564
01107      EXEC CICS READ                                               EL6564
01108           DATASET  (RATE-FILE-ID)                                 EL6564
01109           SET      (ADDRESS OF RATE-RECORD)                          CL*14
01110           RIDFLD   (PI-ERRATE-KEY)                                EL6564
01111           UPDATE                                                  EL6564
01112      END-EXEC.                                                    EL6564
01113                                                                   EL6564
01114  7750-EXIT.                                                       EL6564
01115      EXIT.                                                        EL6564
01116      EJECT                                                        EL6564
01117                                                                   EL6564
01118  8000-UPDATE-MAINT-DATE.                                          EL6564
01119      MOVE SPACES                 TO ELCNTL-KEY.                   EL6564
01120                                                                   EL6564
01121      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL6564
01122      MOVE '1'                    TO CNTL-REC-TYPE.                EL6564
01123      MOVE +0                     TO CNTL-SEQ-NO.                  EL6564
01124                                                                   EL6564
01125      EXEC CICS HANDLE CONDITION                                   EL6564
01126          NOTFND   (8000-EXIT)                                     EL6564
01127      END-EXEC.                                                    EL6564
01128                                                                   EL6564
01129      EXEC CICS READ                                               EL6564
01130          UPDATE                                                   EL6564
01131          DATASET   (CNTL-FILE-ID)                                 EL6564
01132          SET       (ADDRESS OF CONTROL-FILE)                         CL*14
01133          RIDFLD    (ELCNTL-KEY)                                   EL6564
01134      END-EXEC.                                                    EL6564
01135                                                                   EL6564
01136      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               EL6564
01137      MOVE 'B'                    TO JP-RECORD-TYPE.               EL6564
01138      MOVE CNTL-FILE-ID           TO FILE-ID.                      EL6564
01139 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6564
01140                                                                   EL6564
01141      MOVE BIN-CURRENT-SAVE       TO CF-RATES-FILE-MAINT-DT.       EL6564
01142                                                                   EL6564
01143      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               EL6564
01144      MOVE 'C'                    TO JP-RECORD-TYPE.               EL6564
01145      MOVE CNTL-FILE-ID           TO FILE-ID.                      EL6564
01146                                                                   EL6564
01147      EXEC CICS REWRITE                                            EL6564
01148          DATASET   (CNTL-FILE-ID)                                 EL6564
01149          FROM      (CONTROL-FILE)                                 EL6564
01150      END-EXEC.                                                    EL6564
01151                                                                   EL6564
01152 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6564
01153                                                                   EL6564
01154  8000-EXIT.                                                       EL6564
01155       EXIT.                                                       EL6564
01156      EJECT                                                        EL6564
01157                                                                   EL6564
01158  8100-SEND-INITIAL-MAP.                                           EL6564
01159      MOVE EIBTIME                  TO TIME-IN.                       CL**4
01160                                                                   EL6564
01161      IF SCREEN-A                                                  EL6564
01162          MOVE SAVE-DATE            TO ADATEO                         CL**4
01163          MOVE TIME-OUT             TO ATIMEO                         CL**4
01164          MOVE EMI-MESSAGE-AREA (1) TO AERRMSGO                    EL6564
01165          MOVE -1                   TO AMETHODL.                      CL**4
01166                                                                   EL6564
01167      IF SCREEN-B                                                  EL6564
01168          MOVE SAVE-DATE            TO BDATEO                         CL**4
01169          MOVE TIME-OUT             TO BTIMEO                         CL**4
01170          MOVE EMI-MESSAGE-AREA (1) TO BERRMSGO                    EL6564
01171          MOVE -1                   TO ST-RATE-L (1).                 CL**4
01172                                                                   EL6564
01173      IF SCREEN-C                                                  EL6564
01174          MOVE SAVE-DATE            TO CDATEO                         CL**4
01175          MOVE TIME-OUT             TO CTIMEO                         CL**4
01176          MOVE EMI-MESSAGE-AREA (1) TO CERRMSGO                    EL6564
01177          MOVE -1                   TO RATE-L (1).                    CL**4
01178                                                                   EL6564
01179      EXEC CICS SEND                                               EL6564
01180          MAP   (PI-MAPNAME)                                       EL6564
01181          MAPSET(WS-MAPSET-NAME)                                   EL6564
01182          FROM  (EL6564BO)                                         EL6564
01183          ERASE                                                    EL6564
01184          CURSOR                                                   EL6564
01185      END-EXEC.                                                    EL6564
01186                                                                   EL6564
01187      GO TO 9100-RETURN-TRAN.                                      EL6564
01188                                                                   EL6564
01189  8200-SEND-DATAONLY.                                              EL6564
01190      MOVE EIBTIME                  TO TIME-IN.                       CL**4
01191                                                                   EL6564
01192      IF SCREEN-A                                                  EL6564
01193          MOVE SAVE-DATE            TO ADATEO                         CL**4
01194          MOVE TIME-OUT             TO ATIMEO                         CL**4
01195          MOVE EMI-MESSAGE-AREA (1) TO AERRMSGO                    EL6564
01196                                                                      CL*11
01197          IF  EMI-ERROR EQUAL ZEROS                                   CL*11
01198              MOVE -1               TO AMETHODL.                      CL*11
01199                                                                   EL6564
01200      IF SCREEN-B                                                  EL6564
01201          MOVE SAVE-DATE            TO BDATEO                         CL**4
01202          MOVE TIME-OUT             TO BTIMEO                         CL**4
01203          MOVE EMI-MESSAGE-AREA (1) TO BERRMSGO.                   EL6564
01204                                                                   EL6564
01205      IF SCREEN-C                                                  EL6564
01206          MOVE SAVE-DATE            TO CDATEO                         CL**4
01207          MOVE TIME-OUT             TO CTIMEO                         CL**4
01208          MOVE EMI-MESSAGE-AREA (1) TO CERRMSGO.                   EL6564
01209                                                                   EL6564
01210      EXEC CICS SEND                                               EL6564
01211          MAP   (PI-MAPNAME)                                       EL6564
01212          MAPSET(WS-MAPSET-NAME)                                   EL6564
01213          FROM  (EL6564BO)                                         EL6564
01214          DATAONLY                                                 EL6564
01215          CURSOR                                                   EL6564
01216      END-EXEC.                                                    EL6564
01217                                                                   EL6564
01218      GO TO 9100-RETURN-TRAN.                                      EL6564
01219                                                                   EL6564
01220  8300-SEND-TEXT.                                                  EL6564
01221      EXEC CICS SEND TEXT                                          EL6564
01222          FROM  (LOGOFF-TEXT)                                      EL6564
01223          LENGTH(LOGOFF-LENGTH)                                    EL6564
01224          ERASE                                                    EL6564
01225          FREEKB                                                   EL6564
01226      END-EXEC.                                                    EL6564
01227                                                                   EL6564
01228      EXEC CICS RETURN                                             EL6564
01229      END-EXEC.                                                    EL6564
01230                                                                   EL6564
081413 8400-LOG-JOURNAL-RECORD.
081413
081413     if pi-journal-file-id = 0
081413        go to 8400-exit
081413     end-if
081413
081413     move eibdate                to jp-date
081413     move eibtime                to jp-time
081413     move RATE-FILE-ID           TO JP-FILE-ID
081413     MOVE PI-PROCESSOR-ID        TO JP-USER-ID
081413     MOVE 02                     TO PI-JOURNAL-FILE-ID
081413     MOVE THIS-PGM               TO JP-PROGRAM-ID
081413
081413     EXEC CICS JOURNAL
081413        JFILEID   (PI-JOURNAL-FILE-ID)
081413        JTYPEID   ('EL')
081413        FROM      (JOURNAL-RECORD)
081413        LENGTH    (2000)
081413        resp      (ws-response)
081413     END-EXEC
081413
081413     if ws-resp-normal
081413        continue
081413     else
081413        display ' error-el6564-journal ' ws-response
081413     end-if
081413
081413     .
081413 8400-exit.
081413     exit.

01244  8800-UNAUTHORIZED-ACCESS.                                        EL6564
01245      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL6564
01246      GO TO 8300-SEND-TEXT.                                        EL6564
01247                                                                   EL6564
01248  8810-PF23.                                                       EL6564
01249      MOVE EIBAID   TO PI-ENTRY-CD-1.                              EL6564
01250      MOVE XCTL-005 TO PGM-NAME.                                   EL6564
01251      GO TO 9300-XCTL.                                             EL6564
01252                                                                   EL6564
01253  8880-NOT-FOUND.                                                  EL6564
01254      MOVE ER-0142 TO EMI-ERROR.                                   EL6564
01255      MOVE -1      TO BPFKEYL.                                        CL**4
01256                                                                   EL6564
01257      IF EIBTRNID NOT = TRANS-ID                                   EL6564
01258          GO TO 8100-SEND-INITIAL-MAP.                             EL6564
01259                                                                   EL6564
01260      GO TO 8200-SEND-DATAONLY.                                    EL6564
01261                                                                   EL6564
01262  9100-RETURN-TRAN.                                                EL6564
01263      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL6564
01264      MOVE '656E'               TO PI-CURRENT-SCREEN-NO.              CL**3
01265                                                                   EL6564
01266      EXEC CICS RETURN                                             EL6564
01267          TRANSID (TRANS-ID)                                       EL6564
01268          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL6564
01269          LENGTH  (PI-COMM-LENGTH)                                 EL6564
01270      END-EXEC.                                                    EL6564
01271                                                                   EL6564
01272  9200-RETURN-MAIN-MENU.                                           EL6564
01273      MOVE XCTL-626 TO PGM-NAME.                                   EL6564
01274      GO TO 9300-XCTL.                                             EL6564
01275                                                                   EL6564
01276  9300-XCTL.                                                       EL6564
01277      EXEC CICS XCTL                                               EL6564
01278          PROGRAM (PGM-NAME)                                       EL6564
01279          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL6564
01280          LENGTH  (PI-COMM-LENGTH)                                 EL6564
01281      END-EXEC.                                                    EL6564
01282                                                                   EL6564
01283  9400-CLEAR.                                                      EL6564
01284      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       EL6564
01285      GO TO 9300-XCTL.                                             EL6564
01286                                                                   EL6564
01287  9500-PF12.                                                       EL6564
01288      MOVE XCTL-010 TO PGM-NAME.                                   EL6564
01289      GO TO 9300-XCTL.                                             EL6564
01290                                                                   EL6564
01291  9600-PGMID-ERROR.                                                EL6564
01292      EXEC CICS HANDLE CONDITION                                   EL6564
01293          PGMIDERR(8300-SEND-TEXT)                                 EL6564
01294      END-EXEC.                                                    EL6564
01295                                                                   EL6564
01296      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                     EL6564
01297      MOVE ' '          TO PI-ENTRY-CD-1.                          EL6564
01298      MOVE XCTL-005     TO PGM-NAME.                               EL6564
01299      MOVE PGM-NAME     TO LOGOFF-PGM.                             EL6564
01300      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL6564
01301      GO TO 9300-XCTL.                                             EL6564
01302                                                                   EL6564
01303  9700-LINK-DATE-CONVERT.                                          EL6564
01304      MOVE LINK-ELDATCV TO PGM-NAME.                               EL6564
01305                                                                   EL6564
01306      EXEC CICS LINK                                               EL6564
01307          PROGRAM (PGM-NAME)                                       EL6564
01308          COMMAREA(DATE-CONVERSION-DATA)                           EL6564
01309          LENGTH  (DC-COMM-LENGTH)                                 EL6564
01310      END-EXEC.                                                    EL6564
01311                                                                   EL6564
01312  9700-EXIT.                                                       EL6564
01313      EXIT.                                                        EL6564
01314                                                                   EL6564
01315  9900-ERROR-FORMAT.                                               EL6564
01316      IF NOT EMI-ERRORS-COMPLETE                                   EL6564
01317          MOVE LINK-001 TO PGM-NAME                                EL6564
01318          EXEC CICS LINK                                           EL6564
01319              PROGRAM (PGM-NAME)                                   EL6564
01320              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL6564
01321              LENGTH  (EMI-COMM-LENGTH)                            EL6564
01322          END-EXEC.                                                EL6564
01323                                                                   EL6564
01324  9900-EXIT.                                                       EL6564
01325      EXIT.                                                        EL6564
01326                                                                   EL6564
01327  9990-ABEND.                                                      EL6564
01328      MOVE LINK-004   TO PGM-NAME.                                 EL6564
01329      MOVE DFHEIBLK   TO EMI-LINE1.                                EL6564
01330                                                                   EL6564
01331      EXEC CICS LINK                                               EL6564
01332          PROGRAM   (PGM-NAME)                                     EL6564
01333          COMMAREA  (EMI-LINE1)                                    EL6564
01334          LENGTH    (72)                                           EL6564
01335      END-EXEC.                                                    EL6564
01336                                                                   EL6564
01337      GO TO 8200-SEND-DATAONLY.                                    EL6564
01338                                                                   EL6564
01339      GOBACK.                                                      EL6564
01340                                                                   EL6564
01341  9995-SECURITY-VIOLATION.                                         EL6564
01342                              COPY ELCSCTP.                        EL6564
01343                                                                   EL6564
01344  9995-EXIT.                                                       EL6564
01345       EXIT.                                                       EL6564
