00001  ID DIVISION.                                                     03/06/96
00002                                                                   EL6563
00003  PROGRAM-ID.                 EL6563.                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/12/96 16:34:40.                    CL**5
00007 *                            VMOD=2.005                              CL**5
00008 *                                                                 EL6563
00008 *                                                                 EL6563
00009 *AUTHOR.     LOGIC,INC.                                              CL**5
00010 *            DALLAS, TEXAS.                                          CL**5
00011                                                                   EL6563
00012 *DATE-COMPILED.                                                      CL**5
00013 *SECURITY.   *****************************************************   CL**5
00014 *            *                                                   *   CL**5
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
00016 *            *                                                   *   CL**5
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
00018 *                                                                *   CL**5
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
00021 *            *                                                   *   CL**5
00022 *            *****************************************************   CL**5
00023                                                                   EL6563
00024 *REMARKS.    TRANSACTION - EXE4 - RATE MASTER MAINTENANCE            CL**5
00025 *                                 (BANDED WORKSHEET).                CL**5
00026      EJECT                                                        EL6563
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
00027  ENVIRONMENT DIVISION.                                            EL6563
00028  DATA DIVISION.                                                   EL6563
00029  WORKING-STORAGE SECTION.                                         EL6563
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL6563
00031  77  FILLER  PIC X(32)  VALUE '*    EL6563 WORKING STORAGE    *'. EL6563
00032  77  FILLER  PIC X(32)  VALUE '************ V/M 2.005 *********'.    CL**5
00033                                                                   EL6563
00034  01  WS-DATE-AREA.                                                EL6563
00035      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL6563
00036      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL6563
00037                                                                   EL6563
00038  01  STANDARD-AREAS.                                              EL6563
00039      05  GETMAIN-SPACE       PIC X       VALUE SPACE.             EL6563
00040      05  TRANS-ID            PIC X(4)    VALUE 'EXE4'.            EL6563
00041      05  THIS-PGM            PIC X(8)    VALUE 'EL6563'.          EL6563
00042      05  WS-MAPNAME          PIC X(8)    VALUE 'EL6563A'.         EL6563
00043      05  WS-MAPSET-NAME      PIC X(8)    VALUE 'EL6563S'.         EL6563
00044      05  PGM-NAME            PIC X(8).                            EL6563
00045      05  SUB1                PIC S999    VALUE +0.                EL6563
00046      05  SUB2                PIC S999    VALUE +0.                EL6563
00047      05  SC-ITEM             PIC S9(4)   VALUE +1     COMP.       EL6563
00048      05  WS-COMP-CD-R.                                            EL6563
00049          10  FILLER          PIC X.                               EL6563
00050          10  WS-COMP-CD-X    PIC X.                               EL6563
00051      05  WS-COMP-CD   REDEFINES WS-COMP-CD-R  PIC S9(4)     COMP. EL6563
00052      05  TIME-IN             PIC S9(7).                           EL6563
00053      05  TIME-OUT-R  REDEFINES TIME-IN.                           EL6563
00054          10  FILLER          PIC X.                               EL6563
00055          10  TIME-OUT        PIC 99V99.                           EL6563
00056          10  FILLER          PIC X(2).                            EL6563
00057      05  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL6563
00058      05  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL6563
00059      05  XCTL-626            PIC X(8)    VALUE 'EL126'.           EL6563
00060      05  XCTL-656            PIC X(8)    VALUE 'EL656'.           EL6563
00061      05  XCTL-6562           PIC X(8)    VALUE 'EL6562'.          EL6563
00062      05  LINK-001            PIC X(8)    VALUE 'EL001'.           EL6563
00063      05  LINK-004            PIC X(8)    VALUE 'EL004'.           EL6563
00064      05  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL6563
00065                                                                   EL6563
00066      05  ER-0000             PIC X(4)    VALUE  '0000'.           EL6563
00067      05  ER-0004             PIC X(4)    VALUE  '0004'.           EL6563
00068      05  ER-0008             PIC X(4)    VALUE  '0008'.           EL6563
00069      05  ER-0029             PIC X(4)    VALUE  '0029'.           EL6563
00070      05  ER-0068             PIC X(4)    VALUE  '0068'.           EL6563
00071      05  ER-0070             PIC X(4)    VALUE  '0070'.           EL6563
00072      05  ER-0142             PIC X(4)    VALUE  '0142'.           EL6563
00073      05  ER-2055             PIC X(4)    VALUE  '2055'.           EL6563
00074      05  ER-2280             PIC X(4)    VALUE  '2280'.           EL6563
00075      05  ER-2282             PIC X(4)    VALUE  '2282'.           EL6563
00076      05  ER-2283             PIC X(4)    VALUE  '2283'.           EL6563
00077      05  ER-2284             PIC X(4)    VALUE  '2284'.           EL6563
00078      05  ER-2290             PIC X(4)    VALUE  '2290'.           EL6563
00079                                                                   EL6563
00080      05  RATE-FILE-ID        PIC X(8)    VALUE 'ERRATE'.          EL6563
00081      05  OE-RATE-FILE-ID     PIC X(8)    VALUE 'OERATE'.          EL6563
00082      05  FILE-ID             PIC X(8)    VALUE  SPACES.           EL6563
00083      05  CNTL-FILE-ID        PIC X(8)    VALUE 'ELCNTL'.          EL6563
00084      05  BIN-CURRENT-SAVE    PIC XX      VALUE SPACES.            EL6563
00085                                                                   EL6563
00086                                                                   EL6563
00087      05  DEEDIT-FIELD            PIC X(15).                       EL6563
00088      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL6563
00089      05  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(10)V99999.EL6563
00090                                                                   EL6563
00091      05  ERRATE-LENGTH       PIC S9(4)   VALUE +1765 COMP.        EL6563
00092                                                                   EL6563
00093      05  WS-PREV-MONTH       PIC 999      VALUE ZERO.             EL6563
00094                                                                   EL6563
00095      05  ELCNTL-KEY.                                              EL6563
00096          10  CNTL-COMP-ID    PIC X(3)    VALUE SPACES.            EL6563
00097          10  CNTL-REC-TYPE   PIC X       VALUE SPACES.            EL6563
00098          10  CNTL-ACCESS     PIC X(4)    VALUE SPACES.            EL6563
00099          10  CNTL-SEQ-NO     PIC S9(4)   VALUE +0  COMP.          EL6563
00100                                                                   EL6563
00101      05  MISC-SAVE-AREAS.                                         EL6563
00102          10  WS-RATE-TABLE     OCCURS 12 TIMES.                   EL6563
00103              15  WS-RATE       PIC S9(2)V9(5).                    EL6563
00104              15  WS-ST-MONTH   PIC 999.                           EL6563
00105              15  WS-END-MONTH  PIC 999.                           EL6563
00106                                                                   EL6563
081413 01  filler.
081413     12  WS-RESPONSE             PIC S9(8)   COMP.                    
081413         88  WS-RESP-NORMAL              VALUE +00.               
081413         88  WS-RESP-ERROR               VALUE +01.               
081413         88  WS-RESP-NOTFND              VALUE +13.               
081413         88  WS-RESP-DUPKEY              VALUE +15.
081413         88  WS-RESP-NOTOPEN             VALUE +19.
081413         88  WS-RESP-ENDFILE             VALUE +20.

00107  EJECT                                                            EL6563
00108                        COPY ELCSCTM.                                 CL**5
00109                                                                      CL**5
00110  EJECT                                                            EL6563
00111                        COPY ELCSCRTY.                                CL**5
00112                                                                      CL**5
00113      EJECT                                                        EL6563
00114                        COPY ELCDATE.                                 CL**5
00115                                                                      CL**5
00116      EJECT                                                        EL6563
00117                        COPY ELCLOGOF.                                CL**5
00118                                                                      CL**5
00119      EJECT                                                        EL6563
00120                        COPY ELCATTR.                                 CL**5
00121                                                                      CL**5
00122      EJECT                                                        EL6563
00123                        COPY ELCEMIB.                                 CL**5
00124                                                                      CL**5
00125      EJECT                                                        EL6563
00126                        COPY ELCINTF.                                 CL**5
00127                                                                      CL**5
00128      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             EL6563
00129          16  PI-FILE-ID                    PIC XX.                EL6563
00130          16  PI-MAINT                      PIC X.                 EL6563
00131          16  PI-ERRATE-KEY.                                       EL6563
00132              20  PI-RATE-COMPANY-CD         PIC X.                EL6563
00133              20  PI-RATE-STATE-CODE.                              EL6563
00134                  24  PI-RATE-CODE           PIC XX.               EL6563
00135                  24  PI-RATE-CLASS          PIC XX.               EL6563
00136                  24  PI-RATE-DEV            PIC X(3).             EL6563
00137              20  PI-RATE-L-AH-CODE.                               EL6563
00138                  24  PI-RATE-L-AH           PIC X.                EL6563
00139                  24  PI-RATE-LAH-NUM        PIC XX.                  CL**3
00140              20  PI-RATE-LIMITS.                                  EL6563
00141                  24  PI-RATE-HIGH-AGE       PIC 99.               EL6563
00142                  24  PI-RATE-HIGH-AMT       PIC 9(6).             EL6563
00143                  24  PI-RATE-FUTURE         PIC XX.               EL6563
00144                  24  PI-RATE-SEX            PIC X.                EL6563
00145              20  PI-RATE-EXPIRY-DATE.                             EL6563
00146                  24  PI-RATE-EXP-YR         PIC 99.               EL6563
00147                  24  PI-RATE-EXP-MO         PIC 99.               EL6563
00148                  24  PI-RATE-EXP-DA         PIC 99.               EL6563
00149                                                                   EL6563
00150          16  PI-SAVE-ERRATE-KEY             PIC X(28).            EL6563
00151                                                                   EL6563
00152          16  PI-BROWSE-SW                   PIC X.                EL6563
00153              88  BROWSE-STARTED                  VALUE 'Y'.       EL6563
00154          16  PI-ERRATE-EOF-SW              PIC X.                 EL6563
00155              88  ERRATE-EOF                      VALUE 'Y'.       EL6563
00156                                                                   EL6563
00157          16  PI-SUB                        PIC S999.              EL6563
00158          16  PI-YEAR                       PIC S99.               EL6563
00159          16  FILLER                        PIC X(574).               CL**5
00160                                                                   EL6563
00161      EJECT                                                        EL6563
00162                      COPY ELCJPFX.                                   CL**5
081413                             PIC X(2000).                            CL**4
00164                                                                   EL6563
00165      EJECT                                                        EL6563
00166                      COPY ELCAID.                                    CL**5
00167                                                                      CL**5
00168  01  FILLER    REDEFINES DFHAID.                                  EL6563
00169      05  FILLER              PIC X(8).                            EL6563
00170      05  PF-VALUES           PIC X       OCCURS 2.                EL6563
00171                                                                   EL6563
00172      EJECT                                                        EL6563
00173                      COPY EL6563S.                                   CL**5
00174                                                                      CL**5
00175                                                                   EL6563
00176  01  EL6563AO-R   REDEFINES EL6563AI.                             EL6563
00177      05  FILLER             PIC X(31).                            EL6563
00178      05  SCREEN-TABLE       OCCURS 12 TIMES                       EL6563
00179                             INDEXED BY ST-INDX.                   EL6563
00180          10  RATE-L         PIC S9(4)         COMP.               EL6563
00181          10  RATE-A         PIC X.                                EL6563
00182          10  RATE           PIC 9(8).                             EL6563
00183          10  RATE1          REDEFINES                             EL6563
00184              RATE           PIC ZZ.99999.                         EL6563
00185                                                                   EL6563
00186          10  ST-MONTH-L     PIC S9(4)         COMP.               EL6563
00187          10  ST-MONTH-A     PIC X.                                EL6563
00188          10  ST-MONTH       PIC 999.                              EL6563
00189                                                                   EL6563
00190          10  END-MONTH-L    PIC S9(4)         COMP.               EL6563
00191          10  END-MONTH-A    PIC X.                                EL6563
00192          10  END-MONTH      PIC 999.                              EL6563
00193                                                                   EL6563
00194      05  FILLER             PIC X(5).                             EL6563
00195      05  FILLER             PIC X(75).                               CL**5
00196                                                                   EL6563
00197      EJECT                                                        EL6563
00198                                                                   EL6563
00199  LINKAGE SECTION.                                                 EL6563
00200                                                                   EL6563
00201  01  DFHCOMMAREA             PIC X(1024).                         EL6563
00202                                                                   EL6563
00203      EJECT                                                        EL6563
00204                   COPY ERCRATE.                                      CL**5
00205                                                                      CL**5
00206      EJECT                                                        EL6563
00207                   COPY ELCCNTL.                                      CL**5
00208                                                                      CL**5
00209      EJECT                                                        EL6563
00210                                                                   EL6563
00211  PROCEDURE DIVISION.                                              EL6563
00212                                                                   EL6563
00213      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL6563
00214      MOVE '5'                   TO DC-OPTION-CODE.                EL6563
00215      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6563
00216      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL6563
00217      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL6563
00218                                                                   EL6563
00219      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL6563
00220                                                                   EL6563
00221  0100-START.                                                      EL6563
00222      IF EIBCALEN = 0                                              EL6563
00223          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6563
00224                                                                   EL6563
00225      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6563
00226          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6563
00227              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL6563
00228              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL6563
00229              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL6563
00230              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL6563
00231              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL6563
00232              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL6563
00233              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL6563
00234              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL6563
00235          ELSE                                                     EL6563
00236              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL6563
00237              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL6563
00238              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL6563
00239              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL6563
00240              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL6563
00241              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL6563
00242              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL6563
00243              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL6563
00244                                                                   EL6563
00245      EXEC CICS HANDLE CONDITION                                   EL6563
00246          NOTOPEN  (9990-ABEND)                                    EL6563
00247          NOTFND   (8880-NOT-FOUND)                                EL6563
00248          PGMIDERR (9600-PGMID-ERROR)                              EL6563
00249          ERROR    (9990-ABEND)                                    EL6563
00250          END-EXEC.                                                EL6563
00251                                                                   EL6563
00252      IF PI-FILE-ID = 'OE'                                         EL6563
00253         MOVE OE-RATE-FILE-ID TO RATE-FILE-ID.                     EL6563
00254                                                                   EL6563
00255      IF EIBTRNID NOT = TRANS-ID                                   EL6563
00256          MOVE LOW-VALUES TO EL6563AI                              EL6563
00257          MOVE -1         TO RATE-L (1)                            EL6563
00258          GO TO 8100-SEND-INITIAL-MAP.                             EL6563
00259                                                                   EL6563
00260      IF EIBAID = DFHCLEAR                                         EL6563
00261          GO TO 9400-CLEAR.                                        EL6563
00262                                                                   EL6563
00263      IF NOT MODIFY-CAP                                            EL6563
00264          MOVE 'UPDATE'       TO SM-READ                           EL6563
00265          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL6563
00266          MOVE ER-0070        TO  EMI-ERROR                        EL6563
00267          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6563
00268          GO TO 8100-SEND-INITIAL-MAP.                             EL6563
00269      EJECT                                                        EL6563
00270                                                                   EL6563
00271  0200-RECEIVE.                                                    EL6563
00272      MOVE LOW-VALUES TO EL6563AI.                                 EL6563
00273                                                                   EL6563
00274      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6563
00275          MOVE ER-0008 TO EMI-ERROR                                EL6563
00276          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6563
00277          MOVE -1   TO RATE-L (1)                                  EL6563
00278          GO TO 8200-SEND-DATAONLY.                                EL6563
00279                                                                   EL6563
00280      EXEC CICS RECEIVE                                            EL6563
00281          MAP    (WS-MAPNAME)                                      EL6563
00282          MAPSET (WS-MAPSET-NAME)                                  EL6563
00283          INTO   (EL6563AI)                                        EL6563
00284          END-EXEC.                                                EL6563
00285                                                                   EL6563
00286      IF PFENTERL = 0                                              EL6563
00287          GO TO 0300-CHECK-PFENTERS.                               EL6563
00288                                                                   EL6563
00289      IF EIBAID NOT = DFHENTER                                     EL6563
00290          MOVE ER-0004 TO EMI-ERROR                                EL6563
00291          GO TO 0310-INPUT-ERROR.                                  EL6563
00292                                                                   EL6563
00293      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)   EL6563
00294          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL6563
00295          GO TO 0300-CHECK-PFENTERS                                EL6563
00296      ELSE                                                         EL6563
00297          MOVE ER-0029 TO EMI-ERROR                                EL6563
00298          GO TO 0310-INPUT-ERROR.                                  EL6563
00299                                                                   EL6563
00300      EJECT                                                        EL6563
00301                                                                   EL6563
00302  0300-CHECK-PFENTERS.                                             EL6563
00303      IF EIBAID = DFHPF23                                          EL6563
00304          GO TO 8810-PF23.                                         EL6563
00305                                                                   EL6563
00306      IF EIBAID = DFHPF24                                          EL6563
00307          GO TO 9200-RETURN-MAIN-MENU.                             EL6563
00308                                                                   EL6563
00309      IF EIBAID = DFHPF12                                          EL6563
00310          GO TO 9500-PF12.                                         EL6563
00311                                                                   EL6563
00312      IF EIBAID = DFHPF1                                           EL6563
00313          GO TO 2000-UPDATE.                                       EL6563
00314                                                                   EL6563
00315      IF EIBAID = DFHENTER                                         EL6563
00316          GO TO 0320-MAINT.                                        EL6563
00317                                                                   EL6563
00318      MOVE ER-0029 TO EMI-ERROR.                                   EL6563
00319                                                                   EL6563
00320  0310-INPUT-ERROR.                                                EL6563
00321      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6563
00322      MOVE AL-UNBON TO PFENTERA.                                   EL6563
00323      MOVE -1       TO PFENTERL.                                   EL6563
00324                                                                   EL6563
00325      GO TO 8200-SEND-DATAONLY.                                    EL6563
00326                                                                   EL6563
00327  0320-MAINT.                                                      EL6563
00328      PERFORM 5000-EDIT THRU 5099-EXIT.                            EL6563
00329                                                                   EL6563
00330      IF EMI-NO-ERRORS                                             EL6563
00331          MOVE ER-2284 TO EMI-ERROR                                EL6563
00332          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6563
00333          MOVE -1 TO PFENTERL                                      EL6563
00334          GO TO 8200-SEND-DATAONLY                                 EL6563
00335      ELSE                                                         EL6563
00336          GO TO 8200-SEND-DATAONLY.                                EL6563
00337                                                                   EL6563
00338      EJECT                                                        EL6563
00339                                                                   EL6563
00340  2000-UPDATE.                                                     EL6563
00341      PERFORM 5000-EDIT THRU 5099-EXIT.                            EL6563
00342                                                                   EL6563
00343      IF EMI-NO-ERRORS                                             EL6563
00344          NEXT SENTENCE                                            EL6563
00345      ELSE                                                         EL6563
00346          GO TO 8200-SEND-DATAONLY.                                EL6563
00347                                                                   EL6563
00348      PERFORM 7750-READ-ERRATE-UPDATE THRU 7750-EXIT.              EL6563
00349                                                                   EL6563
00350      MOVE RATE-RECORD TO JP-RECORD-AREA.                          EL6563
00351                                                                   EL6563
00352      PERFORM 4000-GENERATE-RATES THRU 4099-EXIT.                  EL6563
00353                                                                   EL6563
00354      IF RT-LAST-MAINT-USER   = PI-UPDATE-BY OR                    EL6563
00355         RT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL6563
00356          NEXT SENTENCE                                            EL6563
00357      ELSE                                                         EL6563
00358          EXEC CICS UNLOCK                                         EL6563
00359               DATASET  (RATE-FILE-ID)                             EL6563
00360          END-EXEC                                                 EL6563
00361          MOVE ER-0068 TO EMI-ERROR                                EL6563
00362          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6563
00363          GO TO 8200-SEND-DATAONLY.                                EL6563
00364                                                                   EL6563
00365      MOVE PI-PROCESSOR-ID     TO RT-LAST-MAINT-USER.              EL6563
00366      MOVE EIBTIME             TO RT-LAST-MAINT-HHMMSS.            EL6563
00367                                                                   EL6563
00368      MOVE SAVE-BIN-DATE       TO RT-LAST-MAINT-DT                 EL6563
00369                                  BIN-CURRENT-SAVE.                EL6563
00370      MOVE 'B'                 TO JP-RECORD-TYPE.                  EL6563
00371      MOVE RATE-FILE-ID        TO FILE-ID.                         EL6563
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00373      MOVE RATE-RECORD         TO JP-RECORD-AREA.                  EL6563
00374                                                                   EL6563
00375      EXEC CICS REWRITE                                            EL6563
00376          DATASET  (RATE-FILE-ID)                                  EL6563
00377          FROM     (RATE-RECORD)                                   EL6563
00378      END-EXEC.                                                    EL6563
00379                                                                   EL6563
00380      MOVE 'C'                 TO JP-RECORD-TYPE.                  EL6563
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00382      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL6563
00383      MOVE ER-0000 TO EMI-ERROR.                                   EL6563
00384      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6563
00385                                                                   EL6563
00386      MOVE XCTL-6562 TO PGM-NAME.                                  EL6563
00387      GO TO 9300-XCTL.                                             EL6563
00388                                                                   EL6563
00389  2000-EXIT.                                                       EL6563
00390      EXIT.                                                        EL6563
00391      EJECT                                                        EL6563
00392                                                                   EL6563
00393  4000-GENERATE-RATES.                                             EL6563
00394      MOVE +1 TO SUB1                                              EL6563
00395                 SUB2.                                             EL6563
00396                                                                   EL6563
00397  4025-CONSTRUCT-RATES.                                            EL6563
00398      IF  SUB1 GREATER +12  OR                                     EL6563
00399          SUB2 GREATER +360                                        EL6563
00400             MOVE +1 TO SUB1                                       EL6563
00401                        SUB2                                       EL6563
00402             GO TO 4099-EXIT.                                      EL6563
00403                                                                   EL6563
00404      IF WS-RATE (SUB1) = ZEROS                                    EL6563
00405          ADD +1 TO SUB1                                           EL6563
00406          GO TO 4025-CONSTRUCT-RATES.                              EL6563
00407                                                                   EL6563
00408      IF SUB2 GREATER   WS-ST-MONTH (SUB1)   OR                    EL6563
00409         SUB2 =         WS-ST-MONTH (SUB1)                         EL6563
00410            IF  SUB2  LESS  WS-END-MONTH (SUB1)  OR                EL6563
00411                SUB2  =     WS-END-MONTH (SUB1)                    EL6563
00412                   NEXT SENTENCE                                   EL6563
00413            ELSE                                                   EL6563
00414                ADD +1 TO SUB1                                     EL6563
00415                GO TO 4025-CONSTRUCT-RATES                         EL6563
00416      ELSE                                                         EL6563
00417          ADD +1 TO SUB2                                           EL6563
00418          GO TO 4025-CONSTRUCT-RATES.                              EL6563
00419                                                                   EL6563
00420      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1                          EL6563
00421          MOVE WS-RATE (SUB1) TO RT-AH-RATE (SUB2).                EL6563
00422                                                                   EL6563
00423      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1                        EL6563
00424          MOVE WS-RATE (SUB1) TO RT-L-RATE (SUB2).                 EL6563
00425                                                                   EL6563
00426      ADD +1 TO SUB2.                                              EL6563
00427      GO TO 4025-CONSTRUCT-RATES.                                  EL6563
00428                                                                   EL6563
00429  4099-EXIT.                                                       EL6563
00430      EXIT.                                                        EL6563
00431      EJECT                                                        EL6563
00432                                                                   EL6563
00433  5000-EDIT.                                                       EL6563
00434       SET ST-INDX TO +1.                                          EL6563
00435       MOVE +1  TO  SUB1.                                          EL6563
00436                                                                   EL6563
00437  5050-CONT.                                                       EL6563
00438      IF ST-INDX GREATER +12                                       EL6563
00439          IF SUB1 GREATER +12                                      EL6563
00440              MOVE +1 TO SUB1                                      EL6563
00441              SET ST-INDX TO +1                                    EL6563
00442              GO TO 5099-EXIT                                      EL6563
00443          ELSE                                                     EL6563
00444              MOVE ZEROS TO WS-RATE (SUB1)                         EL6563
00445                            WS-ST-MONTH (SUB1)                     EL6563
00446                            WS-END-MONTH (SUB1)                    EL6563
00447              ADD +1 TO SUB1                                       EL6563
00448              GO TO 5050-CONT.                                     EL6563
00449                                                                   EL6563
00450      IF RATE-L (ST-INDX) GREATER ZERO                             EL6563
00451          MOVE RATE (ST-INDX) TO DEEDIT-FIELD                      EL6563
00452          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL6563
00453          IF DEEDIT-FIELD-V0 NUMERIC                               EL6563
00454              MOVE DEEDIT-FIELD-V1 TO WS-RATE (SUB1)               EL6563
00455                                      RATE1 (ST-INDX)              EL6563
00456              MOVE AL-UNNON        TO RATE-A (ST-INDX)             EL6563
00457          ELSE                                                     EL6563
00458              MOVE -1       TO RATE-L (ST-INDX)                    EL6563
00459              MOVE AL-UNBON TO RATE-A (ST-INDX)                    EL6563
00460              MOVE ER-2280  TO EMI-ERROR                           EL6563
00461              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6563
00462      ELSE                                                         EL6563
00463          IF ST-MONTH-L (ST-INDX)  GREATER ZERO OR                 EL6563
00464             END-MONTH-L (ST-INDX) GREATER ZERO                    EL6563
00465                 MOVE -1       TO RATE-L (ST-INDX)                 EL6563
00466                 MOVE AL-UNBON TO RATE-A (ST-INDX)                 EL6563
00467                 MOVE ER-2280  TO EMI-ERROR                        EL6563
00468                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT          EL6563
00469          ELSE                                                     EL6563
00470              MOVE ZEROS TO WS-RATE (SUB1).                        EL6563
00471                                                                   EL6563
00472      IF ST-MONTH-L (ST-INDX) GREATER ZERO                         EL6563
00473          IF ST-MONTH (ST-INDX) NUMERIC                            EL6563
00474              IF ST-MONTH (ST-INDX) GREATER +360                   EL6563
00475                  MOVE -1       TO ST-MONTH-L (ST-INDX)            EL6563
00476                  MOVE AL-UNBON TO ST-MONTH-A (ST-INDX)            EL6563
00477                  MOVE ER-2290  TO EMI-ERROR                       EL6563
00478                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6563
00479              ELSE                                                 EL6563
00480                  IF ST-MONTH (ST-INDX) GREATER WS-PREV-MONTH      EL6563
00481                      MOVE ST-MONTH (ST-INDX) TO                   EL6563
00482                                       WS-ST-MONTH (SUB1)          EL6563
00483                                       WS-PREV-MONTH               EL6563
00484                      MOVE AL-UNNON TO ST-MONTH-A (ST-INDX)        EL6563
00485                  ELSE                                             EL6563
00486                      MOVE -1       TO ST-MONTH-L (ST-INDX)        EL6563
00487                      MOVE AL-UNBON TO ST-MONTH-A (ST-INDX)        EL6563
00488                      MOVE ER-2282  TO EMI-ERROR                   EL6563
00489                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL6563
00490          ELSE                                                     EL6563
00491              MOVE -1       TO ST-MONTH-L (ST-INDX)                EL6563
00492              MOVE AL-UNBON TO ST-MONTH-A (ST-INDX)                EL6563
00493              MOVE ER-2290  TO EMI-ERROR                           EL6563
00494              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6563
00495      ELSE                                                         EL6563
00496          IF RATE-L (ST-INDX) GREATER ZERO                         EL6563
00497              MOVE -1       TO ST-MONTH-L (ST-INDX)                EL6563
00498              MOVE AL-UNBON TO ST-MONTH-A (ST-INDX)                EL6563
00499              MOVE ER-2290  TO EMI-ERROR                           EL6563
00500              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6563
00501          ELSE                                                     EL6563
00502              MOVE ZEROS TO WS-ST-MONTH (SUB1).                    EL6563
00503                                                                   EL6563
00504      IF END-MONTH-L (ST-INDX) GREATER ZERO                        EL6563
00505          IF END-MONTH (ST-INDX) NUMERIC                           EL6563
00506              IF END-MONTH (ST-INDX) GREATER +360                  EL6563
00507                  MOVE -1       TO END-MONTH-L (ST-INDX)           EL6563
00508                  MOVE AL-UNBON TO END-MONTH-A (ST-INDX)           EL6563
00509                  MOVE ER-2290  TO EMI-ERROR                       EL6563
00510                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6563
00511              ELSE                                                 EL6563
00512                  IF END-MONTH (ST-INDX) LESS THAN WS-PREV-MONTH   EL6563
00513                      MOVE -1       TO END-MONTH-L (ST-INDX)       EL6563
00514                      MOVE AL-UNBON TO END-MONTH-A (ST-INDX)       EL6563
00515                      MOVE ER-2283  TO EMI-ERROR                   EL6563
00516                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL6563
00517                  ELSE                                             EL6563
00518                      MOVE END-MONTH (ST-INDX) TO                  EL6563
00519                                      WS-END-MONTH (SUB1)          EL6563
00520                                      WS-PREV-MONTH                EL6563
00521                      MOVE AL-UNNON TO END-MONTH-A (ST-INDX)       EL6563
00522          ELSE                                                     EL6563
00523              MOVE -1       TO END-MONTH-L (ST-INDX)               EL6563
00524              MOVE AL-UNBON TO END-MONTH-A (ST-INDX)               EL6563
00525              MOVE ER-2290  TO EMI-ERROR                           EL6563
00526              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6563
00527      ELSE                                                         EL6563
00528          IF RATE-L (ST-INDX) GREATER ZERO                         EL6563
00529              MOVE -1       TO END-MONTH-L (ST-INDX)               EL6563
00530              MOVE AL-UNBON TO END-MONTH-A (ST-INDX)               EL6563
00531              MOVE ER-2290  TO EMI-ERROR                           EL6563
00532              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6563
00533          ELSE                                                     EL6563
00534              MOVE ZEROS TO WS-END-MONTH (SUB1).                   EL6563
00535                                                                   EL6563
00536      SET ST-INDX UP BY +1.                                        EL6563
00537      ADD +1 TO SUB1.                                              EL6563
00538                                                                   EL6563
00539      GO TO 5050-CONT.                                             EL6563
00540                                                                   EL6563
00541  5099-EXIT.                                                       EL6563
00542      EXIT.                                                        EL6563
00543      EJECT                                                        EL6563
00544                                                                   EL6563
00545  7500-DEEDIT.                                                     EL6563
00546      EXEC CICS BIF                                                EL6563
00547           DEEDIT                                                  EL6563
00548           FIELD  (DEEDIT-FIELD)                                   EL6563
00549           LENGTH (15)                                             EL6563
00550      END-EXEC.                                                    EL6563
00551                                                                   EL6563
00552  7500-EXIT.                                                       EL6563
00553      EXIT.                                                        EL6563
00554      EJECT                                                        EL6563
00555                                                                   EL6563
00556  7750-READ-ERRATE-UPDATE.                                         EL6563
00557      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.                  EL6563
00558                                                                   EL6563
00559      EXEC CICS READ                                               EL6563
00560           DATASET  (RATE-FILE-ID)                                 EL6563
00561           SET      (ADDRESS OF RATE-RECORD)                          CL**5
00562           RIDFLD   (PI-ERRATE-KEY)                                EL6563
00563           UPDATE                                                  EL6563
00564      END-EXEC.                                                    EL6563
00565                                                                   EL6563
00566  7750-EXIT.                                                       EL6563
00567      EXIT.                                                        EL6563
00568      EJECT                                                        EL6563
00569                                                                   EL6563
00570  8000-UPDATE-MAINT-DATE.                                          EL6563
00571      MOVE SPACES                 TO ELCNTL-KEY.                   EL6563
00572                                                                   EL6563
00573      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL6563
00574      MOVE '1'                    TO CNTL-REC-TYPE.                EL6563
00575      MOVE +0                     TO CNTL-SEQ-NO.                  EL6563
00576                                                                   EL6563
00577      EXEC CICS HANDLE CONDITION                                   EL6563
00578          NOTFND   (8000-EXIT)                                     EL6563
00579          END-EXEC.                                                EL6563
00580                                                                   EL6563
00581      EXEC CICS READ                                               EL6563
00582          UPDATE                                                   EL6563
00583          DATASET   (CNTL-FILE-ID)                                 EL6563
00584          SET       (ADDRESS OF CONTROL-FILE)                         CL**5
00585          RIDFLD    (ELCNTL-KEY)                                   EL6563
00586      END-EXEC.                                                    EL6563
00587                                                                   EL6563
00588      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               EL6563
00589      MOVE 'B'                    TO JP-RECORD-TYPE.               EL6563
00590      MOVE CNTL-FILE-ID           TO FILE-ID.                      EL6563
00591 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6563
00592                                                                   EL6563
00593      MOVE BIN-CURRENT-SAVE       TO CF-RATES-FILE-MAINT-DT.       EL6563
00594                                                                   EL6563
00595      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               EL6563
00596      MOVE 'C'                    TO JP-RECORD-TYPE.               EL6563
00597      MOVE CNTL-FILE-ID           TO FILE-ID.                      EL6563
00598                                                                   EL6563
00599      EXEC CICS REWRITE                                            EL6563
00600          DATASET   (CNTL-FILE-ID)                                 EL6563
00601          FROM      (CONTROL-FILE)                                 EL6563
00602      END-EXEC.                                                    EL6563
00603                                                                   EL6563
00604 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6563
00605                                                                   EL6563
00606  8000-EXIT.                                                       EL6563
00607       EXIT.                                                       EL6563
00608      EJECT                                                        EL6563
00609                                                                   EL6563
00610  8100-SEND-INITIAL-MAP.                                           EL6563
00611      MOVE EIBTIME              TO TIME-IN.                        EL6563
00612      MOVE SAVE-DATE            TO RUNDATEO.                       EL6563
00613      MOVE TIME-OUT             TO RUNTIMEO.                       EL6563
00614      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL6563
00615      MOVE -1           TO RATE-L (1).                             EL6563
00616      EXEC CICS SEND                                               EL6563
00617          MAP   (WS-MAPNAME)                                       EL6563
00618          MAPSET(WS-MAPSET-NAME)                                   EL6563
00619          FROM  (EL6563AO)                                         EL6563
00620          ERASE                                                    EL6563
00621          CURSOR                                                   EL6563
00622      END-EXEC.                                                    EL6563
00623                                                                   EL6563
00624      GO TO 9100-RETURN-TRAN.                                      EL6563
00625                                                                   EL6563
00626  8200-SEND-DATAONLY.                                              EL6563
00627      MOVE EIBTIME              TO TIME-IN.                        EL6563
00628      MOVE SAVE-DATE            TO RUNDATEO.                       EL6563
00629      MOVE TIME-OUT             TO RUNTIMEO.                       EL6563
00630      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.                       EL6563
00631      EXEC CICS SEND                                               EL6563
00632          MAP   (WS-MAPNAME)                                       EL6563
00633          MAPSET(WS-MAPSET-NAME)                                   EL6563
00634          FROM  (EL6563AO)                                         EL6563
00635          DATAONLY                                                 EL6563
00636          CURSOR                                                   EL6563
00637      END-EXEC.                                                    EL6563
00638                                                                   EL6563
00639      GO TO 9100-RETURN-TRAN.                                      EL6563
00640                                                                   EL6563
00641  8300-SEND-TEXT.                                                  EL6563
00642      EXEC CICS SEND TEXT                                          EL6563
00643          FROM  (LOGOFF-TEXT)                                      EL6563
00644          LENGTH(LOGOFF-LENGTH)                                    EL6563
00645          ERASE                                                    EL6563
00646          FREEKB                                                   EL6563
00647      END-EXEC.                                                    EL6563
00648                                                                   EL6563
00649      EXEC CICS RETURN                                             EL6563
00650      END-EXEC.                                                    EL6563
00651                                                                   EL6563
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
081413        display ' error-el6563-journal ' ws-response
081413     end-if
081413
081413     .
081413 8400-exit.
081413     exit.

00665  8800-UNAUTHORIZED-ACCESS.                                        EL6563
00666      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL6563
00667      GO TO 8300-SEND-TEXT.                                        EL6563
00668                                                                   EL6563
00669  8810-PF23.                                                       EL6563
00670      MOVE EIBAID   TO PI-ENTRY-CD-1.                              EL6563
00671      MOVE XCTL-005 TO PGM-NAME.                                   EL6563
00672      GO TO 9300-XCTL.                                             EL6563
00673                                                                   EL6563
00674  8880-NOT-FOUND.                                                  EL6563
00675      MOVE ER-0142 TO EMI-ERROR.                                   EL6563
00676      MOVE -1   TO RATE-L (1).                                     EL6563
00677                                                                   EL6563
00678      IF EIBTRNID NOT = TRANS-ID                                   EL6563
00679          GO TO 8100-SEND-INITIAL-MAP.                             EL6563
00680                                                                   EL6563
00681      GO TO 8200-SEND-DATAONLY.                                    EL6563
00682                                                                   EL6563
00683  9100-RETURN-TRAN.                                                EL6563
00684      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL6563
00685      MOVE '656D'               TO PI-CURRENT-SCREEN-NO.              CL**2
00686                                                                   EL6563
00687      EXEC CICS RETURN                                             EL6563
00688          TRANSID (TRANS-ID)                                       EL6563
00689          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL6563
00690          LENGTH  (PI-COMM-LENGTH)                                 EL6563
00691      END-EXEC.                                                    EL6563
00692                                                                   EL6563
00693  9200-RETURN-MAIN-MENU.                                           EL6563
00694      MOVE XCTL-626 TO PGM-NAME.                                   EL6563
00695      GO TO 9300-XCTL.                                             EL6563
00696                                                                   EL6563
00697  9300-XCTL.                                                       EL6563
00698      EXEC CICS XCTL                                               EL6563
00699          PROGRAM (PGM-NAME)                                       EL6563
00700          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL6563
00701          LENGTH  (PI-COMM-LENGTH)                                 EL6563
00702      END-EXEC.                                                    EL6563
00703                                                                   EL6563
00704  9400-CLEAR.                                                      EL6563
00705      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       EL6563
00706      GO TO 9300-XCTL.                                             EL6563
00707                                                                   EL6563
00708  9500-PF12.                                                       EL6563
00709      MOVE XCTL-010 TO PGM-NAME.                                   EL6563
00710      GO TO 9300-XCTL.                                             EL6563
00711                                                                   EL6563
00712  9600-PGMID-ERROR.                                                EL6563
00713      EXEC CICS HANDLE CONDITION                                   EL6563
00714          PGMIDERR(8300-SEND-TEXT)                                 EL6563
00715      END-EXEC.                                                    EL6563
00716                                                                   EL6563
00717      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                     EL6563
00718      MOVE ' '          TO PI-ENTRY-CD-1.                          EL6563
00719      MOVE XCTL-005     TO PGM-NAME.                               EL6563
00720      MOVE PGM-NAME     TO LOGOFF-PGM.                             EL6563
00721      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL6563
00722      GO TO 9300-XCTL.                                             EL6563
00723                                                                   EL6563
00724  9700-LINK-DATE-CONVERT.                                          EL6563
00725      MOVE LINK-ELDATCV TO PGM-NAME.                               EL6563
00726                                                                   EL6563
00727      EXEC CICS LINK                                               EL6563
00728          PROGRAM (PGM-NAME)                                       EL6563
00729          COMMAREA(DATE-CONVERSION-DATA)                           EL6563
00730          LENGTH  (DC-COMM-LENGTH)                                 EL6563
00731      END-EXEC.                                                    EL6563
00732                                                                   EL6563
00733  9700-EXIT.                                                       EL6563
00734      EXIT.                                                        EL6563
00735                                                                   EL6563
00736  9900-ERROR-FORMAT.                                               EL6563
00737      IF NOT EMI-ERRORS-COMPLETE                                   EL6563
00738          MOVE LINK-001 TO PGM-NAME                                EL6563
00739          EXEC CICS LINK                                           EL6563
00740              PROGRAM (PGM-NAME)                                   EL6563
00741              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL6563
00742              LENGTH  (EMI-COMM-LENGTH)                            EL6563
00743          END-EXEC.                                                EL6563
00744                                                                   EL6563
00745  9900-EXIT.                                                       EL6563
00746      EXIT.                                                        EL6563
00747                                                                   EL6563
00748  9990-ABEND.                                                      EL6563
00749      MOVE LINK-004     TO PGM-NAME.                               EL6563
00750      MOVE DFHEIBLK     TO EMI-LINE1.                              EL6563
00751      EXEC CICS LINK                                               EL6563
00752          PROGRAM   (PGM-NAME)                                     EL6563
00753          COMMAREA  (EMI-LINE1)                                    EL6563
00754          LENGTH    (72)                                           EL6563
00755      END-EXEC.                                                    EL6563
00756                                                                   EL6563
00757      GO TO 8200-SEND-DATAONLY.                                    EL6563
00758                                                                   EL6563
00759      GOBACK.                                                      EL6563
00760                                                                   EL6563
00761  9995-SECURITY-VIOLATION.                                         EL6563
00762                              COPY ELCSCTP.                        EL6563
00763                                                                   EL6563
00764  9995-EXIT.                                                       EL6563
00765       EXIT.                                                       EL6563
