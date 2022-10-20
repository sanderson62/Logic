00001  ID DIVISION.                                                     03/08/96
00002                                                                   EL610
00003  PROGRAM-ID.                 EL610.                                  LV003
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/12/96 17:01:58.                    CL**3
00007 *                            VMOD=2.003.                             CL**3
00008 *                                                                 EL610
00008 *                                                                 EL610
00009 *AUTHOR.     LOGIC,INC.                                              CL**3
00010 *            DALLAS, TEXAS.                                          CL**3
00011                                                                   EL610
00012 *DATE-COMPILED.                                                      CL**3
00013                                                                   EL610
00014 *SECURITY.   *****************************************************   CL**3
00015 *            *                                                   *   CL**3
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00017 *            *                                                   *   CL**3
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00019 *                                                                *   CL**3
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00022 *            *                                                   *   CL**3
00023 *            *****************************************************   CL**3
00024                                                                   EL610
00025 *REMARKS.    TRANSACTION - EXA8 - LOAN OFFICER PENETRATION.          CL**3
00026                                                                   EL610
00027  ENVIRONMENT DIVISION.                                            EL610
00028                                                                   EL610
110105******************************************************************
110105*                   C H A N G E   L O G
110105*
110105* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
110105*-----------------------------------------------------------------
110105*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
110105* EFFECTIVE    NUMBER
110105*-----------------------------------------------------------------
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
110105******************************************************************
00030  DATA DIVISION.                                                   EL610
00031  WORKING-STORAGE SECTION.                                         EL610
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL610
00033  77  FILLER  PIC X(32)  VALUE '*    EL610  WORKING STORAGE    *'. EL610
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.003 *********'.    CL**3
00035                                                                   EL610
00036  COPY ELCSCTM.                                                       CL**3
00037                                                                      CL**3
00038  COPY ELCSCRTY.                                                      CL**3
00039                                                                   EL610
00040      EJECT                                                        EL610
00041                                                                   EL610
00042  01  WS-DATE-AREA.                                                EL610
00043      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    EL610
00044      05  SAVE-BIN-DATE               PIC X(2)    VALUE SPACES.    EL610
00045                                                                   EL610
00046  01  STANDARD-AREAS.                                              EL610
00047      12  SC-ITEM                     PIC S9(4) COMP VALUE +1.     EL610
00048      12  GETMAIN-SPACE               PIC X       VALUE SPACE.     EL610
00049      12  MAP-NAME                    PIC X(8)    VALUE 'EL610A'.  EL610
00050      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL610S'.  EL610
00051      12  SCREEN-NUMBER               PIC X(4)    VALUE '610A'.    EL610
00052      12  TRANS-ID                    PIC X(4)    VALUE 'EXA8'.    EL610
00053      12  THIS-PGM                    PIC X(8)    VALUE 'EL610'.   EL610
00054      12  PGM-NAME                    PIC X(8).                    EL610
00055      12  TIME-IN                     PIC S9(7).                   EL610
00056      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL610
00057          16  FILLER                  PIC X.                       EL610
00058          16  TIME-OUT                PIC 99V99.                   EL610
00059          16  FILLER                  PIC X(2).                    EL610
00060      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   EL610
00061      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   EL610
00062      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.   EL610
00063      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   EL610
00064      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   EL610
00065      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL610
00066      12  ERLOFC-ID                   PIC X(8)    VALUE 'ERLOFC'.  EL610
00067      12  ERACCT-ALT-FILE-ID          PIC X(8)    VALUE 'ERACCT2'. EL610
00068      12  WS-JOURNAL-RECORD-LENGTH PIC S9(4)   COMP VALUE +763.    EL610
00069                                                                   EL610
00070      12  DEEDIT-FIELD                PIC X(15).                   EL610
00071      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).     EL610
00072      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.  EL610
00073                                                                   EL610
00074      12  RETURN-FROM                 PIC X(8).                    EL610
00075      12  QID.                                                     EL610
00076          16  QID-TERM                PIC X(4).                    EL610
00077          16  FILLER                  PIC X(4)    VALUE '627D'.    EL610
00078      12  WS-SUB1                     PIC  S99 COMP.               EL610
00079      12  WS-DATA-KEYED-SW            PIC X       VALUE 'N'.       EL610
00080          88  DATA-WAS-KEYED                      VALUE 'Y'.       EL610
00081      12  WS-SCREEN-KEYED-SW          PIC X       VALUE 'N'.       EL610
00082          88  SCREEN-WAS-KEYED                    VALUE 'Y'.       EL610
00083      12  WS-OFFICER-FOUND-SW         PIC X       VALUE 'N'.       EL610
00084          88  OFFICER-WAS-FOUND                   VALUE 'Y'.       EL610
00085          88  OFFICER-WAS-NOT-FOUND               VALUE 'N'.       EL610
00086      12  WS-FIRST-TIME-SW            PIC X       VALUE 'Y'.       EL610
00087          88  FIRST-TIME                          VALUE 'Y'.       EL610
00088          88  NOT-FIRST-TIME                      VALUE 'N'.       EL610
00089                                                                   EL610
00090      EJECT                                                        EL610
00091  01   ERROR-MESSAGES.                                             EL610
00092      12  ER-0000                     PIC  X(4)   VALUE '0000'.    EL610
00093      12  ER-0004                     PIC  X(4)   VALUE '0004'.    EL610
00094      12  ER-0023                     PIC  X(4)   VALUE '0023'.    EL610
00095      12  ER-0029                     PIC  X(4)   VALUE '0029'.    EL610
00096      12  ER-0050                     PIC  X(4)   VALUE '0050'.    EL610
00097      12  ER-0070                     PIC  X(4)   VALUE '0070'.    EL610
00098      12  ER-0132                     PIC  X(4)   VALUE '0132'.    EL610
00099      12  ER-0142                     PIC  X(4)   VALUE '0142'.    EL610
00100      12  ER-0194                     PIC  X(4)   VALUE '0194'.    EL610
00101      12  ER-0195                     PIC  X(4)   VALUE '0195'.    EL610
00102      12  ER-0196                     PIC  X(4)   VALUE '0196'.    EL610
00103      12  ER-0197                     PIC  X(4)   VALUE '0197'.    EL610
00104      12  ER-2210                     PIC  X(4)   VALUE '2210'.    EL610
00105      12  ER-2237                     PIC  X(4)   VALUE '2237'.    EL610
00106      12  ER-2238                     PIC  X(4)   VALUE '2238'.    EL610
00107      12  ER-3001                     PIC  X(4)   VALUE '3001'.    EL610
00108      12  ER-3002                     PIC  X(4)   VALUE '3002'.    EL610
00109      12  ER-3003                     PIC  X(4)   VALUE '3003'.    EL610
00110      12  ER-3004                     PIC  X(4)   VALUE '3004'.    EL610
00111      12  ER-3005                     PIC  X(4)   VALUE '3005'.    EL610
00112      12  ER-3006                     PIC  X(4)   VALUE '3006'.    EL610
00113      12  ER-3007                     PIC  X(4)   VALUE '3007'.    EL610
00114      12  ER-3008                     PIC  X(4)   VALUE '3008'.    EL610
00115      12  ER-3009                     PIC  X(4)   VALUE '3009'.    EL610
00116      12  ER-3010                     PIC  X(4)   VALUE '3010'.    EL610
00117      12  ER-3011                     PIC  X(4)   VALUE '3011'.    EL610
00118      12  ER-3012                     PIC  X(4)   VALUE '3012'.    EL610
00119      12  ER-3013                     PIC  X(4)   VALUE '3013'.    EL610
00120      12  ER-3014                     PIC  X(4)   VALUE '3014'.    EL610
00121      12  ER-3015                     PIC  X(4)   VALUE '3015'.    EL610
00122      12  ER-3016                     PIC  X(4)   VALUE '3016'.    EL610
00123      12  ER-3018                     PIC  X(4)   VALUE '3018'.    EL610
00124      12  ER-3019                     PIC  X(4)   VALUE '3019'.    EL610
00125      12  ER-7008                     PIC  X(4)   VALUE '7008'.    EL610
00126                                                                   EL610
00127      EJECT                                                        EL610
00128                                                                   EL610
00129  01  ACCESS-KEYS.                                                 EL610
00130      12  ERLOFC-KEY.                                              EL610
00131          16  ERLOFC-COMPANY-CD          PIC  X.                   EL610
00132          16  ERLOFC-CARRIER             PIC  X.                   EL610
00133          16  ERLOFC-GROUPING            PIC  X(6).                EL610
00134          16  ERLOFC-STATE               PIC  XX.                  EL610
00135          16  ERLOFC-ACCOUNT             PIC  X(10).               EL610
110105         16  ERLOFC-OFFICER-CODE        PIC  X(5).
00136 *        16  ERLOFC-OFFICER-CODE        PIC  X(3).                EL610
00137                                                                   EL610
00138      12  ERLOFC-RECORD-LENGTH    PIC S9(4) COMP VALUE +670.       EL610
00139                                                                   EL610
00140      12  ERACCT-ALT-KEY.                                          EL610
00141          16  ERACCT-A-CO-CD      PIC X     VALUE SPACES.          EL610
00142          16  ERACCT-A-CARRIER    PIC X     VALUE SPACES.          EL610
00143          16  ERACCT-A-GROUPING   PIC X(6)  VALUE SPACES.          EL610
00144          16  ERACCT-A-STATE      PIC XX    VALUE SPACES.          EL610
00145          16  ERACCT-A-ACCOUNT    PIC X(10) VALUE SPACES.          EL610
00146          16  ERACCT-A-EXP-DATE   PIC XX    VALUE SPACES.          EL610
00147                                                                   EL610
00148      12  ERACCT-RECORD-LENGTH    PIC S9(4) COMP VALUE +1464.      EL610
00149                                                                   EL610
00150      EJECT                                                        EL610
00151                                                                   EL610
00152  COPY ELCDATE.                                                       CL**3
00153                                                                   EL610
00154      EJECT                                                        EL610
00155  COPY ELCLOGOF.                                                      CL**3
00156                                                                   EL610
00157      EJECT                                                        EL610
00158  COPY ELCATTR.                                                       CL**3
00159                                                                   EL610
00160      EJECT                                                        EL610
00161  COPY ELCEMIB.                                                       CL**3
00162                                                                   EL610
00163      EJECT                                                        EL610
00164  COPY ELCINTF.                                                       CL**3
00165      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                 EL610
00166          16  PI-MAINT                PIC  X.                      EL610
110105         16  PI-LOAN-OFFICER         PIC  X(5).
00167 *        16  PI-LOAN-OFFICER         PIC  X(3).                   EL610
00168          16  PI-CRLOFC-KEY.                                       EL610
00169              20  PI-CRLOFC-COMPANY-CD          PIC  X.            EL610
00170              20  PI-CRLOFC-CARRIER             PIC  X.            EL610
00171              20  PI-CRLOFC-GROUPING            PIC  X(6).         EL610
00172              20  PI-CRLOFC-STATE               PIC  XX.           EL610
00173              20  PI-CRLOFC-ACCOUNT             PIC  X(10).        EL610
110105             20  PI-CRLOFC-OFFICER-CODE        PIC  X(5).
00174 *            20  PI-CRLOFC-OFFICER-CODE        PIC  X(3).         EL610
00175                                                                   EL610
00176          16  PI-PREV-CRLOFC-KEY                PIC X(25).         EL610
110105         16  FILLER                            PIC X(584).
00177 *        16  FILLER                            PIC X(590).           CL**3
00178                                                                   EL610
00179      EJECT                                                        EL610
00180                                                                   EL610
00181  COPY ELCJPFX.                                                       CL**3
00182                              PIC X(523).                          EL610
00183      EJECT                                                        EL610
00184                                                                   EL610
00185  COPY ELCAID.                                                        CL**3
00186  01  FILLER    REDEFINES DFHAID.                                  EL610
00187      12  FILLER              PIC X(8).                            EL610
00188      12  PF-VALUES           PIC X       OCCURS 24 TIMES.         EL610
00189                                                                   EL610
00190      EJECT                                                        EL610
00191                                                                   EL610
00192  COPY EL610S.                                                        CL**3
00193                                                                   EL610
00194      EJECT                                                        EL610
00195                                                                   EL610
00196  01  FILLER REDEFINES EL610AO.                                    EL610
110105     12  FILLER                      PIC X(135).
00197 *    12  FILLER                      PIC X(133).                  EL610
00198      12  ALOAN-OFC-INFO OCCURS 12 TIMES.                          EL610
00199          16  AMO-LENGTH              PIC S9(4) COMP.              EL610
00200          16  AMO-ATTRB               PIC X.                       EL610
00201          16  AMO                     PIC 99.                      EL610
00202          16  ALN-CNT-LENGTH          PIC S9(4) COMP.              EL610
00203          16  ALN-CNT-ATTRB           PIC X.                       EL610
00204          16  ALN-CNT-IN              PIC 9(6).                    EL610
00205          16  ALN-CNT-OUT REDEFINES                                EL610
00206              ALN-CNT-IN              PIC ZZ,ZZZ.                  EL610
00207          16  ALN-VOL-LENGTH          PIC S9(4) COMP.              EL610
00208          16  ALN-VOL-ATTRB           PIC X.                       EL610
00209          16  ALN-VOL-IN              PIC 9(11).                   EL610
00210          16  ALN-VOL-OUT REDEFINES                                EL610
00211              ALN-VOL-IN              PIC ZZZ,ZZZ,ZZZ.             EL610
00212          16  ALF-CNT-LENGTH          PIC S9(4) COMP.              EL610
00213          16  ALF-CNT-ATTRB           PIC X.                       EL610
00214          16  ALF-CNT-IN              PIC 9(6).                    EL610
00215          16  ALF-CNT-OUT REDEFINES                                EL610
00216              ALF-CNT-IN              PIC ZZ,ZZZ.                  EL610
00217          16  ALF-PRM-LENGTH          PIC S9(4) COMP.              EL610
00218          16  ALF-PRM-ATTRB           PIC X.                       EL610
00219          16  ALF-PRM-IN              PIC 9(9).                    EL610
00220          16  ALF-PRM-OUT REDEFINES                                EL610
00221              ALF-PRM-IN              PIC Z,ZZZ,ZZZ.               EL610
00222          16  ALF-AMT-LENGTH          PIC S9(4) COMP.              EL610
00223          16  ALF-AMT-ATTRB           PIC X.                       EL610
00224          16  ALF-AMT-IN              PIC 9(11).                   EL610
00225          16  ALF-AMT-OUT REDEFINES                                EL610
00226              ALF-AMT-IN              PIC ZZZ,ZZZ,ZZZ.             EL610
00227          16  AAH-CNT-LENGTH          PIC S9(4) COMP.              EL610
00228          16  AAH-CNT-ATTRB           PIC X.                       EL610
00229          16  AAH-CNT-IN              PIC 9(6).                    EL610
00230          16  AAH-CNT-OUT REDEFINES                                EL610
00231              AAH-CNT-IN              PIC ZZ,ZZZ.                  EL610
00232          16  AAH-PRM-LENGTH          PIC S9(4) COMP.              EL610
00233          16  AAH-PRM-ATTRB           PIC X.                       EL610
00234          16  AAH-PRM-IN              PIC 9(9).                    EL610
00235          16  AAH-PRM-OUT REDEFINES                                EL610
00236              AAH-PRM-IN              PIC Z,ZZZ,ZZZ.               EL610
00237          16  AAH-AMT-LENGTH          PIC S9(4) COMP.              EL610
00238          16  AAH-AMT-ATTRB           PIC X.                       EL610
00239          16  AAH-AMT-IN              PIC 9(11).                   EL610
00240          16  AAH-AMT-OUT REDEFINES                                EL610
00241              AAH-AMT-IN              PIC ZZZ,ZZZ,ZZZ.             EL610
00242                                                                   EL610
00243      EJECT                                                        EL610
00244                                                                   EL610
00245  01  WS-OFFICER-INFO.                                             EL610
00246      12  FILLER            OCCURS 12 TIMES.                       EL610
00247          16  WS-LN-CNT                  PIC 9(5)   COMP-3.        EL610
00248          16  WS-LN-VOL                  PIC 9(9)   COMP-3.        EL610
00249          16  WS-LF-CNT                  PIC 9(5)   COMP-3.        EL610
00250          16  WS-LF-PRM                  PIC 9(7)   COMP-3.        EL610
00251          16  WS-LF-AMT                  PIC 9(9)   COMP-3.        EL610
00252          16  WS-AH-CNT                  PIC 9(5)   COMP-3.        EL610
00253          16  WS-AH-PRM                  PIC 9(7)   COMP-3.        EL610
00254          16  WS-AH-AMT                  PIC 9(9)   COMP-3.        EL610
00255                                                                   EL610
00256      EJECT                                                        EL610
00257                                                                   EL610
00258  LINKAGE SECTION.                                                 EL610
00259  01  DFHCOMMAREA             PIC X(1024).                         EL610
00260                                                                   EL610
00261      EJECT                                                        EL610
00262                                                                   EL610
00263 *01 PARMLIST .                                                       CL**3
00264 *    12  FILLER                      PIC S9(8)   COMP.               CL**3
00265 *    12  ERLOFC-POINTER              PIC S9(8)   COMP.               CL**3
00266 *    12  ERACCT-POINTER              PIC S9(8)   COMP.               CL**3
00267                                                                   EL610
00268      EJECT                                                        EL610
00269                                                                   EL610
00270  COPY ERCLOFC.                                                       CL**3
00271                                                                   EL610
00272      EJECT                                                        EL610
00273  COPY ERCACCT.                                                       CL**3
00274                                                                   EL610
00275      EJECT                                                        EL610
00276  PROCEDURE DIVISION.                                              EL610
00277                                                                   EL610
00278      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL610
00279      MOVE '5'                    TO DC-OPTION-CODE.               EL610
00280      PERFORM 9700-DATE-LINK.                                      EL610
00281      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL610
00282      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL610
00283                                                                   EL610
00284      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL610
00285      MOVE 1                      TO EMI-NUMBER-OF-LINES.          EL610
00286      MOVE EIBTRMID               TO QID-TERM.                     EL610
00287                                                                   EL610
00288      IF EIBCALEN = 0                                              EL610
00289          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL610
00290                                                                   EL610
00291      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL610
00292          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL610
00293              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL610
00294              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL610
00295              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL610
00296              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL610
00297              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL610
00298              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL610
00299              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL610
00300              MOVE THIS-PGM TO PI-CALLING-PROGRAM                  EL610
00301          ELSE                                                     EL610
00302              MOVE PI-CALLING-PROGRAM   TO RETURN-FROM             EL610
00303              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL610
00304              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL610
00305              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL610
00306              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL610
00307              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL610
00308              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL610
00309              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL610
00310              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL610
00311                                                                   EL610
00312      EXEC CICS    HANDLE    CONDITION                             EL610
00313           PGMIDERR          (9600-PGMID-ERROR)                    EL610
00314           ERROR             (9990-ABEND)                          EL610
00315      END-EXEC.                                                    EL610
00316                                                                   EL610
00317      IF  EIBTRNID NOT = TRANS-ID                                  EL610
00318          MOVE LOW-VALUES         TO EL610AI                       EL610
00319          GO TO 8100-SEND-INITIAL-MAP.                             EL610
00320                                                                   EL610
00321      IF EIBAID = DFHPF5                                           EL610
00322          MOVE LOW-VALUES         TO EL610AI                       EL610
00323          MOVE 'A'                TO AMAINTO                       EL610
00324          MOVE AL-UANON           TO AMAINTA                       EL610
00325          GO TO 8100-SEND-INITIAL-MAP.                             EL610
00326                                                                   EL610
00327      IF EIBAID = DFHCLEAR                                         EL610
00328          GO TO 9400-CLEAR.                                        EL610
00329                                                                   EL610
00330      IF PI-PROCESSOR-ID = 'LGXX'                                  EL610
00331          GO TO 0200-RECEIVE.                                      EL610
00332                                                                   EL610
00333      EXEC CICS READQ TS                                           EL610
00334          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       EL610
00335          INTO   (SECURITY-CONTROL)                                EL610
00336          LENGTH (SC-COMM-LENGTH)                                  EL610
00337          ITEM   (SC-ITEM)                                         EL610
00338      END-EXEC.                                                    EL610
00339                                                                   EL610
00340      MOVE SC-CREDIT-DISPLAY (10)  TO PI-DISPLAY-CAP.              EL610
00341      MOVE SC-CREDIT-UPDATE  (10)  TO PI-MODIFY-CAP.               EL610
00342                                                                   EL610
00343      IF NOT DISPLAY-CAP                                           EL610
00344          MOVE 'READ'          TO SM-READ                          EL610
00345          PERFORM 9995-SECURITY-VIOLATION                          EL610
00346          MOVE ER-0070         TO  EMI-ERROR                       EL610
00347          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL610
00348          GO TO 8100-SEND-INITIAL-MAP.                             EL610
00349                                                                   EL610
00350      EJECT                                                        EL610
00351                                                                   EL610
00352  0200-RECEIVE.                                                    EL610
00353      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL610
00354          MOVE ER-7008            TO EMI-ERROR                     EL610
00355          PERFORM 9900-ERROR-FORMAT                                EL610
00356          MOVE -1                 TO AMAINTL                       EL610
00357          GO TO 8200-SEND-DATAONLY.                                EL610
00358                                                                   EL610
00359      EXEC CICS RECEIVE                                            EL610
00360          MAP      (MAP-NAME)                                      EL610
00361          MAPSET   (MAPSET-NAME)                                   EL610
00362          INTO     (EL610AI)                                       EL610
00363      END-EXEC.                                                    EL610
00364                                                                   EL610
00365      IF  APFENTRL = 0                                             EL610
00366          GO TO 0300-CHECK-PFKEYS.                                 EL610
00367                                                                   EL610
00368      IF  EIBAID NOT = DFHENTER                                    EL610
00369          MOVE ER-0004            TO EMI-ERROR                     EL610
00370          GO TO 0320-INPUT-ERROR.                                  EL610
00371                                                                   EL610
00372      IF  APFENTRI NUMERIC                                         EL610
00373          IF  APFENTRI = '01' OR '02' OR '03' OR '04' OR           EL610
00374                         '12' OR '23' OR '24'                      EL610
00375              MOVE PF-VALUES (APFENTRI) TO EIBAID                  EL610
00376          ELSE                                                     EL610
00377              MOVE ER-0029        TO EMI-ERROR                     EL610
00378              GO TO 0320-INPUT-ERROR.                              EL610
00379                                                                   EL610
00380  0300-CHECK-PFKEYS.                                               EL610
00381      IF EIBAID = DFHPF23                                          EL610
00382          GO TO 8810-PF23.                                         EL610
00383                                                                   EL610
00384      IF EIBAID = DFHPF24                                          EL610
00385          GO TO 9200-RETURN-MAIN-MENU.                             EL610
00386                                                                   EL610
00387      IF EIBAID = DFHPF12                                          EL610
00388          GO TO 9500-PF12.                                         EL610
00389                                                                   EL610
00390      IF  EIBAID = DFHPF1 OR DFHPF2 OR DFHPF3 OR DFHPF4            EL610
00391          IF  AMAINTL GREATER THAN +0                              EL610
00392              MOVE -1             TO  AMAINTL                      EL610
00393              MOVE  ER-0050       TO  EMI-ERROR                    EL610
00394              PERFORM 9900-ERROR-FORMAT                            EL610
00395              GO TO 8200-SEND-DATAONLY.                            EL610
00396                                                                   EL610
00397      IF  EIBAID = DFHPF1                                          EL610
00398          PERFORM 7000-BROWSE-FWRD-NEXT-ACCOUNT                    EL610
00399          GO TO 3000-SHOW-LOAN-OFFICER.                            EL610
00400                                                                   EL610
00401      IF  EIBAID = DFHPF2                                          EL610
00402          PERFORM 7100-BROWSE-BWRD-NEXT-ACCOUNT                    EL610
00403          GO TO 3000-SHOW-LOAN-OFFICER.                            EL610
00404                                                                   EL610
00405      IF  EIBAID = DFHPF3                                          EL610
00406          PERFORM 7200-BROWSE-FWRD-NEXT-OFFICER                    EL610
00407          GO TO 3000-SHOW-LOAN-OFFICER.                            EL610
00408                                                                   EL610
00409      IF  EIBAID = DFHPF4                                          EL610
00410          PERFORM 7300-BROWSE-BWRD-NEXT-OFFICER                    EL610
00411          GO TO 3000-SHOW-LOAN-OFFICER.                            EL610
00412                                                                   EL610
00413      IF EIBAID = DFHENTER                                         EL610
00414          GO TO 0400-EDIT-INPUT-DATA.                              EL610
00415                                                                   EL610
00416      MOVE ER-0029                TO EMI-ERROR.                    EL610
00417                                                                   EL610
00418  0320-INPUT-ERROR.                                                EL610
00419      PERFORM 9900-ERROR-FORMAT.                                   EL610
00420      MOVE AL-UNBON               TO APFENTRA.                     EL610
00421      IF APFENTRL = 0                                              EL610
00422          MOVE -1                 TO AMAINTL                       EL610
00423      ELSE                                                         EL610
00424          MOVE -1                 TO APFENTRL.                     EL610
00425                                                                   EL610
00426      GO TO 8200-SEND-DATAONLY.                                    EL610
00427                                                                   EL610
00428      EJECT                                                        EL610
00429                                                                   EL610
00430  0400-EDIT-INPUT-DATA.                                            EL610
00431      IF AMAINTI = 'A' OR 'C' OR 'D' OR 'S' OR 'K'                 EL610
00432         NEXT SENTENCE                                             EL610
00433        ELSE                                                       EL610
00434         MOVE  ER-0023            TO EMI-ERROR                     EL610
00435         MOVE -1                  TO AMAINTL                       EL610
00436         MOVE AL-UABON            TO AMAINTA                       EL610
00437         PERFORM 9900-ERROR-FORMAT                                 EL610
00438         GO TO 8200-SEND-DATAONLY.                                 EL610
00439                                                                   EL610
00440      IF NOT MODIFY-CAP AND                                        EL610
00441         (AMAINTI = 'A' OR 'C' OR 'D' OR 'K')                      EL610
00442          MOVE 'UPDATE'       TO SM-READ                           EL610
00443          PERFORM 9995-SECURITY-VIOLATION                          EL610
00444          MOVE ER-0070        TO EMI-ERROR                         EL610
00445          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL610
00446          GO TO 8100-SEND-INITIAL-MAP.                             EL610
00447                                                                   EL610
00448      IF AACCTL GREATER THAN ZEROS                                 EL610
00449          MOVE AL-UANON           TO AACCTA                        EL610
00450          MOVE AACCTI             TO PI-CR-ACCOUNT                 EL610
00451      ELSE                                                         EL610
00452          MOVE -1                 TO AACCTL                        EL610
00453          MOVE AL-UABON           TO AACCTA                        EL610
00454          MOVE  ER-0197           TO EMI-ERROR                     EL610
00455          PERFORM 9900-ERROR-FORMAT.                               EL610
00456                                                                   EL610
00457      IF ACARRL GREATER THAN ZEROS                                 EL610
00458          MOVE AL-UANON           TO ACARRA                        EL610
00459          MOVE ACARRI             TO PI-CR-CARRIER                 EL610
00460      ELSE                                                         EL610
00461          IF NOT ST-ACCNT-CNTL AND NOT ACCNT-CNTL                  EL610
00462              MOVE -1             TO ACARRL                        EL610
00463              MOVE AL-UABON       TO ACARRA                        EL610
00464              MOVE  ER-0194       TO EMI-ERROR                     EL610
00465              PERFORM 9900-ERROR-FORMAT.                           EL610
00466                                                                   EL610
00467      IF AGRPL GREATER THAN ZEROS                                  EL610
00468          MOVE AL-UANON           TO AGRPA                         EL610
00469          MOVE AGRPI              TO PI-CR-GROUPING                EL610
00470      ELSE                                                         EL610
00471          IF CARR-GROUP-ST-ACCNT-CNTL                              EL610
00472              MOVE -1             TO AGRPL                         EL610
00473              MOVE AL-UABON       TO AGRPA                         EL610
00474              MOVE  ER-0195       TO EMI-ERROR                     EL610
00475              PERFORM 9900-ERROR-FORMAT.                           EL610
00476                                                                   EL610
00477      IF ASTL GREATER THAN ZEROS                                   EL610
00478          MOVE AL-UANON           TO ASTA                          EL610
00479          MOVE ASTI               TO PI-CR-STATE                   EL610
00480      ELSE                                                         EL610
00481          IF NOT ACCNT-CNTL AND NOT CARR-ACCNT-CNTL                EL610
00482              MOVE -1             TO ASTL                          EL610
00483              MOVE AL-UABON       TO ASTA                          EL610
00484              MOVE  ER-0196       TO EMI-ERROR                     EL610
00485              PERFORM 9900-ERROR-FORMAT.                           EL610
00486                                                                   EL610
00487      IF AOFCRL GREATER THAN +0                                    EL610
00488         MOVE AL-UANON            TO AOFCRA                        EL610
00489         MOVE AOFCRI              TO PI-LOAN-OFFICER               EL610
00490       ELSE                                                        EL610
00491         MOVE SPACES              TO PI-LOAN-OFFICER.              EL610
00492                                                                   EL610
00493      IF ACOMPL GREATER THAN +0                                    EL610
00494         AND ACOMPI = 'Y' OR 'N'   NEXT SENTENCE                   EL610
00495        ELSE                                                       EL610
00496         MOVE -1                  TO ACOMPL                        EL610
00497         MOVE ER-3002             TO EMI-ERROR                     EL610
00498         PERFORM 9900-ERROR-FORMAT.                                EL610
00499                                                                   EL610
00500      IF APRINTL GREATER THAN +0                                   EL610
00501         AND APRINTI = 'D' OR 'S'   NEXT SENTENCE                  EL610
00502        ELSE                                                       EL610
00503         MOVE   ER-3003           TO EMI-ERROR                     EL610
00504         MOVE -1                  TO APRINTL                       EL610
00505         PERFORM 9900-ERROR-FORMAT.                                EL610
00506                                                                   EL610
00507      IF EMI-ERROR = ZEROS NEXT SENTENCE                           EL610
00508        ELSE                                                       EL610
00509         GO TO 8200-SEND-DATAONLY.                                 EL610
00510                                                                   EL610
00511      IF  AMAINTI = 'D' OR 'S'                                     EL610
00512          GO 0450-CHECK-MAINT.                                     EL610
00513                                                                   EL610
00514      PERFORM 7800-READ-ACCOUNT-MASTER.                            EL610
00515                                                                   EL610
00516      IF EMI-ERROR = ZEROS NEXT SENTENCE                           EL610
00517        ELSE                                                       EL610
00518         GO TO 8200-SEND-DATAONLY.                                 EL610
00519                                                                   EL610
00520      EJECT                                                        EL610
00521                                                                   EL610
00522  0450-CHECK-MAINT.                                                EL610
00523      IF AMAINTI = 'A'                                             EL610
00524         GO TO 1000-ADD-LOAN-OFFICER.                              EL610
00525                                                                   EL610
00526      IF AMAINTI = 'C'                                             EL610
00527         GO TO 2000-CHANGE-LOAN-OFFICER.                           EL610
00528                                                                   EL610
00529      IF AMAINTI = 'D'                                             EL610
00530         GO TO 2500-DELETE-LOAN-OFFICER.                           EL610
00531                                                                   EL610
00532      IF AMAINTI = 'S'                                             EL610
00533         IF PI-LOAN-OFFICER = SPACES                               EL610
00534            PERFORM 7200-BROWSE-FWRD-NEXT-OFFICER                  EL610
00535            GO TO 3000-SHOW-LOAN-OFFICER                           EL610
00536        ELSE                                                       EL610
00537            GO TO 3000-SHOW-LOAN-OFFICER.                          EL610
00538                                                                   EL610
00539      GO TO 3500-REWRITE-KEY.                                      EL610
00540                                                                   EL610
00541      EJECT                                                        EL610
00542                                                                   EL610
00543  1000-ADD-LOAN-OFFICER           SECTION.                         EL610
00544      MOVE PI-COMPANY-CD          TO ERLOFC-COMPANY-CD.            EL610
00545      MOVE PI-CR-CARRIER          TO ERLOFC-CARRIER.               EL610
00546      MOVE PI-CR-GROUPING         TO ERLOFC-GROUPING.              EL610
00547      MOVE PI-CR-STATE            TO ERLOFC-STATE.                 EL610
00548      MOVE PI-CR-ACCOUNT          TO ERLOFC-ACCOUNT.               EL610
00549      MOVE PI-LOAN-OFFICER        TO ERLOFC-OFFICER-CODE.          EL610
00550                                                                   EL610
00551      PERFORM 7400-READ-LOAN-MASTER.                               EL610
00552                                                                   EL610
00553      IF OFFICER-WAS-NOT-FOUND                                     EL610
00554         GO TO 1100-PROCESS-NEW-OFFICER.                           EL610
00555                                                                   EL610
00556      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL610
00557          MOVE -1                 TO ACARRL                        EL610
00558      ELSE                                                         EL610
00559          IF ST-ACCNT-CNTL                                         EL610
00560              MOVE -1             TO ASTHDGL                       EL610
00561          ELSE                                                     EL610
00562              IF CARR-ST-ACCNT-CNTL                                EL610
00563                  MOVE -1         TO ACARRL                        EL610
00564              ELSE                                                 EL610
00565                  IF ACCNT-CNTL                                    EL610
00566                      MOVE -1     TO   AACCTL                      EL610
00567                  ELSE                                             EL610
00568                      IF CARR-ACCNT-CNTL                           EL610
00569                          MOVE -1 TO ACARRL.                       EL610
00570                                                                   EL610
00571      MOVE  ER-0132               TO  EMI-ERROR.                   EL610
00572      PERFORM 9900-ERROR-FORMAT.                                   EL610
00573      GO TO 8200-SEND-DATAONLY.                                    EL610
00574                                                                   EL610
00575  1100-PROCESS-NEW-OFFICER.                                        EL610
00576      MOVE +0                     TO WS-SUB1.                      EL610
00577                                                                   EL610
00578  1200-INITIALIZE-COUNTS.                                          EL610
00579      ADD +1                      TO WS-SUB1.                      EL610
00580                                                                   EL610
00581      MOVE ZERO                  TO WS-LN-VOL  (WS-SUB1)           EL610
00582                                    WS-LN-CNT  (WS-SUB1)           EL610
00583                                    WS-LF-CNT  (WS-SUB1)           EL610
00584                                    WS-LF-PRM  (WS-SUB1)           EL610
00585                                    WS-LF-AMT  (WS-SUB1)           EL610
00586                                    WS-AH-CNT  (WS-SUB1)           EL610
00587                                    WS-AH-PRM  (WS-SUB1)           EL610
00588                                    WS-AH-AMT  (WS-SUB1).          EL610
00589                                                                   EL610
00590      IF WS-SUB1 NOT = +12                                         EL610
00591         GO TO 1200-INITIALIZE-COUNTS.                             EL610
00592                                                                   EL610
00593      IF  ANAMEL GREATER THAN +0                                   EL610
00594          NEXT SENTENCE                                            EL610
00595         ELSE                                                      EL610
00596          MOVE -1                 TO ANAMEL                        EL610
00597          MOVE  ER-3016           TO EMI-ERROR                     EL610
00598          PERFORM 9900-ERROR-FORMAT.                               EL610
00599                                                                   EL610
00600      PERFORM 6500-EDIT-LOAN-DATA.                                 EL610
00601                                                                   EL610
00602      IF  EMI-ERROR = ZEROS NEXT SENTENCE                          EL610
00603         ELSE                                                      EL610
00604          GO TO 8200-SEND-DATAONLY.                                EL610
00605                                                                   EL610
00606 *    IF  SCREEN-WAS-KEYED                                         EL610
00607 *        GO TO 1710-ADD-LOAN-OFFICER.                             EL610
00608                                                                   EL610
00609 *    MOVE -1                     TO ACARRL.                       EL610
00610 *    MOVE  ER-3004               TO EMI-ERROR.                    EL610
00611 *    PERFORM 9900-ERROR-FORMAT.                                   EL610
00612                                                                   EL610
00613  1710-ADD-LOAN-OFFICER.                                           EL610
00614      EXEC CICS GETMAIN                                            EL610
00615         SET      (ADDRESS OF LOAN-OFFICER-MASTER)                    CL**3
00616         LENGTH   (ERLOFC-RECORD-LENGTH)
00617         INITIMG  (GETMAIN-SPACE)                                  EL610
00618      END-EXEC.                                                    EL610
00619                                                                   EL610
00620      MOVE  SPACES                TO LOAN-OFFICER-MASTER.          EL610
00621                                                                   EL610
00622      MOVE  'LO'                  TO LO-RECORD-ID.                 EL610
00623                                                                   EL610
00624      MOVE  ERLOFC-KEY            TO LO-CONTROL-PRIMARY.           EL610
00625                                                                   EL610
00626      MOVE  ANAMEI                TO LO-OFFICER-NAME.              EL610
00627      MOVE  PI-PROCESSOR-ID       TO LO-LAST-USER.                 EL610
00628      MOVE  SAVE-BIN-DATE         TO LO-LAST-MAINT-DT.             EL610
00629      MOVE  EIBTIME               TO LO-LAST-MAINT-HHMMSS.         EL610
00630      MOVE  ACOMPI                TO LO-COMP-CONTROL.              EL610
00631      MOVE  APRINTI               TO LO-DETAIL-CONTROL.            EL610
00632                                                                   EL610
00633      MOVE  AM-CARRIER            TO LO-SV-CARRIER.                EL610
00634      MOVE  AM-GROUPING           TO LO-SV-GROUPING.               EL610
00635      MOVE  AM-STATE              TO LO-SV-STATE.                  EL610
00636                                                                   EL610
00637      MOVE  WS-OFFICER-INFO       TO LO-OFFICER-INFO.              EL610
00638                                                                   EL610
00639      MOVE 'A'                    TO  JP-RECORD-TYPE.              EL610
00640      MOVE LOAN-OFFICER-MASTER    TO  JP-RECORD-AREA.              EL610
00641                                                                   EL610
00642      PERFORM 7500-WRITE-LOAN-MASTER.                              EL610
00643                                                                   EL610
00644      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL610
00645                                                                   EL610
00646      IF  EMI-ERROR = ZEROS NEXT SENTENCE                          EL610
00647         ELSE                                                      EL610
00648          GO TO 3000-SHOW-LOAN-OFFICER.                            EL610
00649                                                                   EL610
00650      MOVE    ER-0000             TO EMI-ERROR.                    EL610
00651      PERFORM 9900-ERROR-FORMAT.                                   EL610
00652      GO TO 8100-SEND-INITIAL-MAP.                                 EL610
00653                                                                   EL610
00654  1900-EXIT.                                                       EL610
00655      EXIT.                                                        EL610
00656                                                                   EL610
00657      EJECT                                                        EL610
00658  2000-CHANGE-LOAN-OFFICER SECTION.                                EL610
00659      MOVE PI-COMPANY-CD          TO ERLOFC-COMPANY-CD.            EL610
00660      MOVE PI-CR-CARRIER          TO ERLOFC-CARRIER.               EL610
00661      MOVE PI-CR-GROUPING         TO ERLOFC-GROUPING.              EL610
00662      MOVE PI-CR-STATE            TO ERLOFC-STATE.                 EL610
00663      MOVE PI-CR-ACCOUNT          TO ERLOFC-ACCOUNT.               EL610
00664      MOVE PI-LOAN-OFFICER        TO ERLOFC-OFFICER-CODE.          EL610
00665                                                                   EL610
00666      IF ERLOFC-KEY = PI-CRLOFC-KEY                                EL610
00667         NEXT SENTENCE                                             EL610
00668        ELSE                                                       EL610
00669         GO TO 2380-CHANGE-ERROR.                                  EL610
00670                                                                   EL610
00671      PERFORM 7460-READ-LOAN-MST-UPDT.                             EL610
00672                                                                   EL610
00673      IF  OFFICER-WAS-FOUND  NEXT SENTENCE                         EL610
00674         ELSE                                                      EL610
00675          GO TO 2390-OFFICER-NOT-FOUND.                            EL610
00676                                                                   EL610
00677      MOVE 'C'                    TO JP-RECORD-TYPE.               EL610
00678      MOVE LOAN-OFFICER-MASTER    TO JP-RECORD-AREA.               EL610
00679                                                                   EL610
00680      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL610
00681                                                                   EL610
00682      IF  ANAMEL GREATER THAN +0                                   EL610
00683          MOVE ANAMEI             TO LO-OFFICER-NAME.              EL610
00684      IF  ACOMPL GREATER THAN +0                                   EL610
00685          MOVE ACOMPI             TO LO-COMP-CONTROL.              EL610
00686      IF  APRINTL GREATER THAN +0                                  EL610
00687          MOVE APRINTI            TO LO-DETAIL-CONTROL.            EL610
00688                                                                   EL610
00689      MOVE LO-OFFICER-INFO        TO WS-OFFICER-INFO.              EL610
00690                                                                   EL610
00691      PERFORM 6500-EDIT-LOAN-DATA.                                 EL610
00692                                                                   EL610
00693      IF  EMI-ERROR = ZEROS NEXT SENTENCE                          EL610
00694         ELSE                                                      EL610
00695          GO TO 8200-SEND-DATAONLY.                                EL610
00696                                                                   EL610
00697      MOVE WS-OFFICER-INFO        TO LO-OFFICER-INFO.              EL610
00698                                                                   EL610
00699      MOVE  PI-PROCESSOR-ID       TO LO-LAST-USER.                 EL610
00700      MOVE  SAVE-BIN-DATE         TO LO-LAST-MAINT-DT.             EL610
00701      MOVE  EIBTIME               TO LO-LAST-MAINT-HHMMSS.         EL610
00702                                                                   EL610
00703      MOVE 'C'                    TO JP-RECORD-TYPE.               EL610
00704      MOVE LOAN-OFFICER-MASTER    TO JP-RECORD-AREA.               EL610
00705                                                                   EL610
00706      PERFORM 7600-REWRITE-LOAN-MASTER.                            EL610
00707                                                                   EL610
00708      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL610
00709                                                                   EL610
00710      GO TO 3000-SHOW-LOAN-OFFICER.                                EL610
00711                                                                   EL610
00712  2380-CHANGE-ERROR.                                               EL610
00713      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL610
00714          MOVE -1                 TO ACARRL                        EL610
00715      ELSE                                                         EL610
00716          IF ST-ACCNT-CNTL                                         EL610
00717              MOVE -1             TO ASTHDGL                       EL610
00718          ELSE                                                     EL610
00719              IF CARR-ST-ACCNT-CNTL                                EL610
00720                  MOVE -1         TO ACARRL                        EL610
00721              ELSE                                                 EL610
00722                  IF ACCNT-CNTL                                    EL610
00723                      MOVE -1     TO   AACCTL                      EL610
00724                  ELSE                                             EL610
00725                      IF CARR-ACCNT-CNTL                           EL610
00726                          MOVE -1 TO ACARRL.                       EL610
00727                                                                   EL610
00728      MOVE  ER-3005               TO EMI-ERROR.                    EL610
00729      PERFORM 9900-ERROR-FORMAT.                                   EL610
00730      GO TO 8200-SEND-DATAONLY.                                    EL610
00731                                                                   EL610
00732  2390-OFFICER-NOT-FOUND.                                          EL610
00733      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL610
00734          MOVE -1                 TO ACARRL                        EL610
00735      ELSE                                                         EL610
00736          IF ST-ACCNT-CNTL                                         EL610
00737              MOVE -1             TO ASTHDGL                       EL610
00738          ELSE                                                     EL610
00739              IF CARR-ST-ACCNT-CNTL                                EL610
00740                  MOVE -1         TO ACARRL                        EL610
00741              ELSE                                                 EL610
00742                  IF ACCNT-CNTL                                    EL610
00743                      MOVE -1     TO   AACCTL                      EL610
00744                  ELSE                                             EL610
00745                      IF CARR-ACCNT-CNTL                           EL610
00746                          MOVE -1 TO ACARRL.                       EL610
00747                                                                   EL610
00748      MOVE  ER-3006               TO EMI-ERROR.                    EL610
00749      PERFORM 9900-ERROR-FORMAT.                                   EL610
00750      GO TO 8200-SEND-DATAONLY.                                    EL610
00751                                                                   EL610
00752  2400-EXIT.                                                       EL610
00753      EXIT.                                                        EL610
00754                                                                   EL610
00755      EJECT                                                        EL610
00756                                                                   EL610
00757  2500-DELETE-LOAN-OFFICER  SECTION.                               EL610
00758      EXEC CICS HANDLE CONDITION                                   EL610
00759          NOTFND   (2890-OFFICER-NOT-FOUND)                        EL610
00760      END-EXEC.                                                    EL610
00761                                                                   EL610
00762      MOVE PI-COMPANY-CD          TO ERLOFC-COMPANY-CD.            EL610
00763      MOVE PI-CR-CARRIER          TO ERLOFC-CARRIER.               EL610
00764      MOVE PI-CR-GROUPING         TO ERLOFC-GROUPING.              EL610
00765      MOVE PI-CR-STATE            TO ERLOFC-STATE.                 EL610
00766      MOVE PI-CR-ACCOUNT          TO ERLOFC-ACCOUNT.               EL610
00767      MOVE PI-LOAN-OFFICER        TO ERLOFC-OFFICER-CODE.          EL610
00768                                                                   EL610
00769      IF ERLOFC-KEY = PI-CRLOFC-KEY                                EL610
00770         NEXT SENTENCE                                             EL610
00771        ELSE                                                       EL610
00772         GO TO 2880-DELETE-ERROR.                                  EL610
00773                                                                   EL610
00774      PERFORM 7460-READ-LOAN-MST-UPDT.                             EL610
00775                                                                   EL610
00776      IF  OFFICER-WAS-FOUND  NEXT SENTENCE                         EL610
00777         ELSE                                                      EL610
00778          GO TO 2890-OFFICER-NOT-FOUND.                            EL610
00779                                                                   EL610
00780      MOVE 'D'                    TO JP-RECORD-TYPE.               EL610
00781      MOVE LOAN-OFFICER-MASTER    TO JP-RECORD-AREA.               EL610
00782                                                                   EL610
00783      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL610
00784                                                                   EL610
00785      PERFORM 7700-DELETE-LOAN-MASTER.                             EL610
00786                                                                   EL610
00787      MOVE 'D'                    TO PI-MAINT.                     EL610
00788                                                                   EL610
00789      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL610
00790          MOVE -1                 TO ACARRL                        EL610
00791      ELSE                                                         EL610
00792          IF ST-ACCNT-CNTL                                         EL610
00793              MOVE -1             TO ASTHDGL                       EL610
00794          ELSE                                                     EL610
00795              IF CARR-ST-ACCNT-CNTL                                EL610
00796                  MOVE -1         TO ACARRL                        EL610
00797              ELSE                                                 EL610
00798                  IF ACCNT-CNTL                                    EL610
00799                      MOVE -1     TO   AACCTL                      EL610
00800                  ELSE                                             EL610
00801                      IF CARR-ACCNT-CNTL                           EL610
00802                          MOVE -1 TO ACARRL.                       EL610
00803                                                                   EL610
00804      MOVE    ER-0000             TO EMI-ERROR.                    EL610
00805      PERFORM  9900-ERROR-FORMAT.                                  EL610
00806      GO TO 8100-SEND-INITIAL-MAP.                                 EL610
00807                                                                   EL610
00808  2880-DELETE-ERROR.                                               EL610
00809      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL610
00810          MOVE -1                 TO ACARRL                        EL610
00811      ELSE                                                         EL610
00812          IF ST-ACCNT-CNTL                                         EL610
00813              MOVE -1             TO ASTHDGL                       EL610
00814          ELSE                                                     EL610
00815              IF CARR-ST-ACCNT-CNTL                                EL610
00816                  MOVE -1         TO ACARRL                        EL610
00817              ELSE                                                 EL610
00818                  IF ACCNT-CNTL                                    EL610
00819                      MOVE -1     TO   AACCTL                      EL610
00820                  ELSE                                             EL610
00821                      IF CARR-ACCNT-CNTL                           EL610
00822                          MOVE -1 TO ACARRL.                       EL610
00823                                                                   EL610
00824      MOVE  ER-3005               TO EMI-ERROR.                    EL610
00825      PERFORM 9900-ERROR-FORMAT.                                   EL610
00826      GO TO 8200-SEND-DATAONLY.                                    EL610
00827                                                                   EL610
00828  2890-OFFICER-NOT-FOUND.                                          EL610
00829      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL610
00830          MOVE -1                 TO ACARRL                        EL610
00831      ELSE                                                         EL610
00832          IF ST-ACCNT-CNTL                                         EL610
00833              MOVE -1             TO ASTHDGL                       EL610
00834          ELSE                                                     EL610
00835              IF CARR-ST-ACCNT-CNTL                                EL610
00836                  MOVE -1         TO ACARRL                        EL610
00837              ELSE                                                 EL610
00838                  IF ACCNT-CNTL                                    EL610
00839                      MOVE -1     TO   AACCTL                      EL610
00840                  ELSE                                             EL610
00841                      IF CARR-ACCNT-CNTL                           EL610
00842                          MOVE -1 TO ACARRL.                       EL610
00843                                                                   EL610
00844      MOVE  ER-3006               TO EMI-ERROR.                    EL610
00845      PERFORM 9900-ERROR-FORMAT.                                   EL610
00846      GO TO 8200-SEND-DATAONLY.                                    EL610
00847                                                                   EL610
00848  2900-EXIT.                                                       EL610
00849      EXIT.                                                        EL610
00850                                                                   EL610
00851      EJECT                                                        EL610
00852  3000-SHOW-LOAN-OFFICER          SECTION.                         EL610
00853      IF AMAINTI = 'S'                                             EL610
00854         MOVE PI-COMPANY-CD       TO ERLOFC-COMPANY-CD             EL610
00855         MOVE PI-CR-CARRIER       TO ERLOFC-CARRIER                EL610
00856         MOVE PI-CR-GROUPING      TO ERLOFC-GROUPING               EL610
00857         MOVE PI-CR-STATE         TO ERLOFC-STATE                  EL610
00858         MOVE PI-CR-ACCOUNT       TO ERLOFC-ACCOUNT                EL610
00859         IF PI-LOAN-OFFICER NOT = SPACES                           EL610
00860           MOVE PI-LOAN-OFFICER     TO ERLOFC-OFFICER-CODE.        EL610
00861                                                                   EL610
00862      PERFORM 7400-READ-LOAN-MASTER.                               EL610
00863                                                                   EL610
00864      IF  OFFICER-WAS-NOT-FOUND                                    EL610
00865          GO TO 3350-OFFICER-NOT-FOUND.                            EL610
00866                                                                   EL610
00867      MOVE AMAINTI                TO PI-MAINT.                     EL610
00868      MOVE LOW-VALUE              TO AMAINTO.                      EL610
00869      MOVE ERLOFC-KEY             TO PI-CRLOFC-KEY                 EL610
00870                                     PI-PREV-CRLOFC-KEY.           EL610
00871                                                                   EL610
00872      MOVE ERLOFC-CARRIER         TO PI-CR-CARRIER.                EL610
00873      MOVE ERLOFC-GROUPING        TO PI-CR-GROUPING.               EL610
00874      MOVE ERLOFC-STATE           TO PI-CR-STATE                   EL610
00875      MOVE ERLOFC-ACCOUNT         TO PI-CR-ACCOUNT.                EL610
00876      MOVE ERLOFC-OFFICER-CODE    TO PI-LOAN-OFFICER.              EL610
00877                                                                   EL610
00878      MOVE  LO-CARRIER            TO ACARRO.                       EL610
00879      MOVE  LO-GROUPING           TO AGRPO.                        EL610
00880      MOVE  LO-STATE              TO ASTO.                         EL610
00881      MOVE  LO-ACCOUNT            TO AACCTO.                       EL610
00882      MOVE  LO-OFFICER-CODE       TO AOFCRO.                       EL610
00883      MOVE  LO-OFFICER-NAME       TO ANAMEO.                       EL610
00884      MOVE  LO-COMP-CONTROL       TO ACOMPO.                       EL610
00885      MOVE  LO-DETAIL-CONTROL     TO APRINTO.                      EL610
00886                                                                   EL610
00887      MOVE AL-UANON               TO ACARRA                        EL610
00888                                     AGRPA                         EL610
00889                                     ASTA                          EL610
00890                                     AACCTA                        EL610
00891                                     AOFCRA.                       EL610
00892                                                                   EL610
00893      MOVE +0                     TO WS-SUB1.                      EL610
00894                                                                   EL610
00895  3050-PROCESS-OFFICER-INFO.                                       EL610
00896      ADD +1                      TO WS-SUB1                       EL610
00897                                                                   EL610
00898      IF  WS-SUB1 GREATER THAN +12                                 EL610
00899          GO TO 3200-DISPLAY-OFFICER.                              EL610
00900                                                                   EL610
00901      MOVE LO-LOAN-COUNT  (WS-SUB1)   TO ALN-CNT-OUT (WS-SUB1).    EL610
00902      MOVE LO-LOAN-VOLUME (WS-SUB1)   TO ALN-VOL-OUT (WS-SUB1).    EL610
00903      MOVE LO-LF-COUNT    (WS-SUB1)   TO ALF-CNT-OUT (WS-SUB1).    EL610
00904      MOVE LO-LF-PREM     (WS-SUB1)   TO ALF-PRM-OUT (WS-SUB1).    EL610
00905      MOVE LO-LF-BENEFIT  (WS-SUB1)   TO ALF-AMT-OUT (WS-SUB1).    EL610
00906      MOVE LO-AH-COUNT    (WS-SUB1)   TO AAH-CNT-OUT (WS-SUB1).    EL610
00907      MOVE LO-AH-PREM     (WS-SUB1)   TO AAH-PRM-OUT (WS-SUB1).    EL610
00908      MOVE LO-AH-BENEFIT  (WS-SUB1)   TO AAH-AMT-OUT (WS-SUB1).    EL610
00909                                                                   EL610
00910      GO TO 3050-PROCESS-OFFICER-INFO.                             EL610
00911                                                                   EL610
00912  3200-DISPLAY-OFFICER.                                            EL610
00913      MOVE -1                         TO AMAINTL.                  EL610
00914      MOVE    ER-0000             TO EMI-ERROR.                    EL610
00915      PERFORM 9900-ERROR-FORMAT.                                   EL610
00916      GO TO 8100-SEND-INITIAL-MAP.                                 EL610
00917                                                                   EL610
00918  3350-OFFICER-NOT-FOUND.                                          EL610
00919      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL610
00920          MOVE -1                 TO ACARRL                        EL610
00921      ELSE                                                         EL610
00922          IF ST-ACCNT-CNTL                                         EL610
00923              MOVE -1             TO ASTHDGL                       EL610
00924          ELSE                                                     EL610
00925              IF CARR-ST-ACCNT-CNTL                                EL610
00926                  MOVE -1         TO ACARRL                        EL610
00927              ELSE                                                 EL610
00928                  IF ACCNT-CNTL                                    EL610
00929                      MOVE -1     TO   AACCTL                      EL610
00930                  ELSE                                             EL610
00931                      IF CARR-ACCNT-CNTL                           EL610
00932                          MOVE -1 TO ACARRL.                       EL610
00933                                                                   EL610
00934                                                                   EL610
00935      MOVE  ER-3006               TO EMI-ERROR.                    EL610
00936      PERFORM 9900-ERROR-FORMAT.                                   EL610
00937      GO TO 8200-SEND-DATAONLY.                                    EL610
00938                                                                   EL610
00939  3400-EXIT.                                                       EL610
00940      EXIT.                                                        EL610
00941                                                                   EL610
00942      EJECT                                                        EL610
00943  3500-REWRITE-KEY          SECTION.                               EL610
00944      MOVE PI-COMPANY-CD          TO ERLOFC-COMPANY-CD.            EL610
00945      MOVE PI-CR-CARRIER          TO ERLOFC-CARRIER.               EL610
00946      MOVE PI-CR-GROUPING         TO ERLOFC-GROUPING.              EL610
00947      MOVE PI-CR-STATE            TO ERLOFC-STATE.                 EL610
00948      MOVE PI-CR-ACCOUNT          TO ERLOFC-ACCOUNT.               EL610
00949      MOVE PI-LOAN-OFFICER        TO ERLOFC-OFFICER-CODE.          EL610
00950                                                                   EL610
00951      PERFORM 7400-READ-LOAN-MASTER.                               EL610
00952                                                                   EL610
00953      IF OFFICER-WAS-NOT-FOUND                                     EL610
00954         GO TO 3550-PROCESS-NEW-OFFICER.                           EL610
00955                                                                   EL610
00956      MOVE -1                     TO  AMAINTL.                     EL610
00957      MOVE  ER-0132               TO  EMI-ERROR.                   EL610
00958      PERFORM 9900-ERROR-FORMAT.                                   EL610
00959      GO TO 8200-SEND-DATAONLY.                                    EL610
00960                                                                   EL610
00961  3550-PROCESS-NEW-OFFICER.                                        EL610
00962      MOVE PI-CRLOFC-KEY          TO ERLOFC-KEY.                   EL610
00963                                                                   EL610
00964      PERFORM 7460-READ-LOAN-MST-UPDT.                             EL610
00965                                                                   EL610
00966      IF  OFFICER-WAS-FOUND  NEXT SENTENCE                         EL610
00967         ELSE                                                      EL610
00968          GO TO 3590-OFFICER-NOT-FOUND.                            EL610
00969                                                                   EL610
00970      MOVE LO-OFFICER-INFO        TO WS-OFFICER-INFO.              EL610
00971      PERFORM 6500-EDIT-LOAN-DATA.                                 EL610
00972      IF  EMI-ERROR = ZEROS NEXT SENTENCE                          EL610
00973         ELSE                                                      EL610
00974          GO TO 8200-SEND-DATAONLY.                                EL610
00975                                                                   EL610
00976      MOVE 'D'                    TO JP-RECORD-TYPE.               EL610
00977      MOVE LOAN-OFFICER-MASTER    TO JP-RECORD-AREA.               EL610
00978                                                                   EL610
00979      PERFORM 7700-DELETE-LOAN-MASTER.                             EL610
00980                                                                   EL610
00981      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL610
00982                                                                   EL610
00983      EXEC CICS GETMAIN                                            EL610
00984         SET      (ADDRESS OF LOAN-OFFICER-MASTER)                    CL**3
00985         LENGTH   (ERLOFC-RECORD-LENGTH)
00986         INITIMG  (GETMAIN-SPACE)                                  EL610
00987      END-EXEC.                                                    EL610
00988                                                                   EL610
00989      MOVE JP-RECORD-AREA         TO LOAN-OFFICER-MASTER.          EL610
00990                                                                   EL610
00991      IF ACARRL GREATER THAN +0                                    EL610
00992         MOVE ACARRI              TO LO-CARRIER.                   EL610
00993                                                                   EL610
00994      IF AGRPL  GREATER THAN +0                                    EL610
00995         MOVE AGRPI               TO LO-GROUPING.                  EL610
00996                                                                   EL610
00997      IF ASTL   GREATER THAN +0                                    EL610
00998         MOVE ASTI                TO LO-STATE.                     EL610
00999                                                                   EL610
01000      IF AACCTL GREATER THAN +0                                    EL610
01001         MOVE AACCTI               TO LO-ACCOUNT.                  EL610
01002                                                                   EL610
01003      IF AOFCRL GREATER THAN +0                                    EL610
01004         MOVE AOFCRI              TO LO-OFFICER-CODE.              EL610
01005                                                                   EL610
01006      IF  ANAMEL GREATER THAN +0                                   EL610
01007          MOVE ANAMEI             TO LO-OFFICER-NAME.              EL610
01008      IF  ACOMPL GREATER THAN +0                                   EL610
01009          MOVE ACOMPI             TO LO-COMP-CONTROL.              EL610
01010      IF  APRINTL GREATER THAN +0                                  EL610
01011          MOVE APRINTI            TO LO-DETAIL-CONTROL.            EL610
01012                                                                   EL610
01013      MOVE WS-OFFICER-INFO        TO LO-OFFICER-INFO.              EL610
01014                                                                   EL610
01015      MOVE  PI-PROCESSOR-ID       TO LO-LAST-USER.                 EL610
01016      MOVE  SAVE-BIN-DATE         TO LO-LAST-MAINT-DT.             EL610
01017      MOVE  EIBTIME               TO LO-LAST-MAINT-HHMMSS.         EL610
01018                                                                   EL610
01019      MOVE 'A'                    TO JP-RECORD-TYPE.               EL610
01020      MOVE LOAN-OFFICER-MASTER    TO JP-RECORD-AREA.               EL610
01021                                                                   EL610
01022      MOVE LO-CONTROL-PRIMARY     TO ERLOFC-KEY.                   EL610
01023                                                                   EL610
01024      PERFORM 7500-WRITE-LOAN-MASTER.                              EL610
01025                                                                   EL610
01026      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL610
01027                                                                   EL610
01028      GO TO 3000-SHOW-LOAN-OFFICER.                                EL610
01029                                                                   EL610
01030  3590-OFFICER-NOT-FOUND.                                          EL610
01031      MOVE -1                     TO AMAINTL.                         CL**2
01032      MOVE  ER-3006               TO EMI-ERROR.                    EL610
01033      PERFORM 9900-ERROR-FORMAT.                                   EL610
01034      GO TO 8200-SEND-DATAONLY.                                    EL610
01035                                                                   EL610
01036  3595-EXIT.                                                       EL610
01037      EXIT.                                                        EL610
01038                                                                   EL610
01039      EJECT                                                        EL610
01040  6500-EDIT-LOAN-DATA        SECTION.                              EL610
01041      MOVE +0                     TO WS-SUB1.                      EL610
01042                                                                   EL610
01043  6520-PROCESS-LOAN-DATA.                                          EL610
01044      ADD +1                      TO WS-SUB1.                      EL610
01045                                                                   EL610
01046      IF WS-SUB1 IS GREATER THAN +12                               EL610
01047         GO TO 6590-EXIT.                                          EL610
01048                                                                   EL610
01049      MOVE 'N'                    TO WS-DATA-KEYED-SW.             EL610
01050                                                                   EL610
01051      IF  ALN-CNT-LENGTH (WS-SUB1) GREATER THAN +0 OR              EL610
01052          ALN-VOL-LENGTH (WS-SUB1) GREATER THAN +0                 EL610
01053              NEXT SENTENCE                                        EL610
01054        ELSE                                                       EL610
01055          GO TO 6550-CHK-PROCESSOR-ID.                             EL610
01056                                                                   EL610
01057      MOVE 'Y'                    TO WS-SCREEN-KEYED-SW.           EL610
01058                                                                   EL610
01059      IF  ALN-CNT-LENGTH (WS-SUB1) GREATER THAN +0                 EL610
01060          MOVE ALN-CNT-IN (WS-SUB1)   TO DEEDIT-FIELD              EL610
01061          PERFORM 8600-DEEDIT                                      EL610
01062          IF  DEEDIT-FIELD-V0     NOT NUMERIC                      EL610
01063              MOVE -1             TO ALN-CNT-LENGTH (WS-SUB1)      EL610
01064              MOVE AL-UNBON       TO ALN-CNT-ATTRB  (WS-SUB1)      EL610
01065              MOVE  ER-3008       TO EMI-ERROR                     EL610
01066              PERFORM 9900-ERROR-FORMAT                            EL610
01067          ELSE                                                     EL610
01068              MOVE DEEDIT-FIELD-V0     TO WS-LN-CNT      (WS-SUB1) EL610
01069                                          ALN-CNT-OUT    (WS-SUB1).EL610
01070                                                                   EL610
01071      IF  ALN-VOL-LENGTH (WS-SUB1) GREATER THAN +0                 EL610
01072          MOVE ALN-VOL-IN (WS-SUB1)   TO DEEDIT-FIELD              EL610
01073          PERFORM 8600-DEEDIT                                      EL610
01074          IF  DEEDIT-FIELD-V0     NOT NUMERIC                      EL610
01075              MOVE -1             TO ALN-VOL-LENGTH (WS-SUB1)      EL610
01076              MOVE AL-UNBON       TO ALN-VOL-ATTRB  (WS-SUB1)      EL610
01077              MOVE  ER-3009       TO EMI-ERROR                     EL610
01078              PERFORM 9900-ERROR-FORMAT                            EL610
01079          ELSE                                                     EL610
01080              MOVE DEEDIT-FIELD-V0     TO WS-LN-VOL      (WS-SUB1) EL610
01081                                          ALN-VOL-OUT    (WS-SUB1).EL610
01082                                                                   EL610
01083 *    IF  WS-LN-CNT (WS-SUB1) = ZEROS OR                           EL610
01084 *        WS-LN-VOL (WS-SUB1) = ZEROS                              EL610
01085 *        MOVE -1                 TO ALN-CNT-LENGTH (WS-SUB1)      EL610
01086 *        MOVE  ER-3007           TO EMI-ERROR                     EL610
01087 *        PERFORM 9900-ERROR-FORMAT.                               EL610
01088                                                                   EL610
01089  6550-CHK-PROCESSOR-ID.                                           EL610
01090      IF  PI-PROCESSOR-ID NOT = 'LGXX'                             EL610
01091          GO TO 6520-PROCESS-LOAN-DATA.                            EL610
01092                                                                   EL610
01093      IF  ALF-CNT-LENGTH (WS-SUB1) GREATER THAN +0                 EL610
01094          MOVE 'Y'                    TO WS-DATA-KEYED-SW          EL610
01095          MOVE ALF-CNT-IN (WS-SUB1)   TO DEEDIT-FIELD              EL610
01096          PERFORM 8600-DEEDIT                                      EL610
01097          IF  DEEDIT-FIELD-V0     NOT NUMERIC                      EL610
01098              MOVE -1                 TO ALF-CNT-LENGTH (WS-SUB1)  EL610
01099              MOVE AL-UNBON           TO ALF-CNT-ATTRB  (WS-SUB1)  EL610
01100              MOVE  ER-3010           TO EMI-ERROR                 EL610
01101              PERFORM 9900-ERROR-FORMAT                            EL610
01102          ELSE                                                     EL610
01103              MOVE DEEDIT-FIELD-V0    TO WS-LF-CNT      (WS-SUB1)  EL610
01104                                         ALF-CNT-OUT    (WS-SUB1). EL610
01105                                                                   EL610
01106      IF  ALF-PRM-LENGTH (WS-SUB1) GREATER THAN +0                 EL610
01107          MOVE 'Y'                    TO WS-DATA-KEYED-SW          EL610
01108          MOVE ALF-PRM-IN (WS-SUB1)   TO DEEDIT-FIELD              EL610
01109          PERFORM 8600-DEEDIT                                      EL610
01110          IF  DEEDIT-FIELD-V0     NOT NUMERIC                      EL610
01111              MOVE -1             TO ALF-PRM-LENGTH (WS-SUB1)      EL610
01112              MOVE AL-UNBON       TO ALF-PRM-ATTRB  (WS-SUB1)      EL610
01113              MOVE  ER-3011       TO EMI-ERROR                     EL610
01114              PERFORM 9900-ERROR-FORMAT                            EL610
01115          ELSE                                                     EL610
01116              MOVE DEEDIT-FIELD-V0     TO WS-LF-PRM   (WS-SUB1)    EL610
01117                                          ALF-PRM-OUT (WS-SUB1).   EL610
01118                                                                   EL610
01119      IF  ALF-AMT-LENGTH (WS-SUB1) GREATER THAN +0                 EL610
01120          MOVE 'Y'                    TO WS-DATA-KEYED-SW          EL610
01121          MOVE ALF-AMT-IN (WS-SUB1)   TO DEEDIT-FIELD              EL610
01122          PERFORM 8600-DEEDIT                                      EL610
01123          IF  DEEDIT-FIELD-V0     NOT NUMERIC                      EL610
01124              MOVE -1             TO ALF-AMT-LENGTH (WS-SUB1)      EL610
01125              MOVE AL-UNBON       TO ALF-AMT-ATTRB  (WS-SUB1)      EL610
01126              MOVE  ER-3012       TO EMI-ERROR                     EL610
01127              PERFORM 9900-ERROR-FORMAT                            EL610
01128          ELSE                                                     EL610
01129              MOVE DEEDIT-FIELD-V0     TO WS-LF-AMT   (WS-SUB1)    EL610
01130                                          ALF-AMT-OUT (WS-SUB1).   EL610
01131                                                                   EL610
01132      IF  AAH-CNT-LENGTH (WS-SUB1) GREATER THAN +0                 EL610
01133          MOVE 'Y'                    TO WS-DATA-KEYED-SW          EL610
01134          MOVE AAH-CNT-IN (WS-SUB1)   TO DEEDIT-FIELD              EL610
01135          PERFORM 8600-DEEDIT                                      EL610
01136          IF  DEEDIT-FIELD-V0     NOT NUMERIC                      EL610
01137              MOVE -1             TO AAH-CNT-LENGTH (WS-SUB1)      EL610
01138              MOVE AL-UNBON       TO AAH-CNT-ATTRB  (WS-SUB1)      EL610
01139              MOVE  ER-3013       TO EMI-ERROR                     EL610
01140              PERFORM 9900-ERROR-FORMAT                            EL610
01141          ELSE                                                     EL610
01142              MOVE DEEDIT-FIELD-V0     TO WS-AH-CNT      (WS-SUB1) EL610
01143                                          AAH-CNT-OUT    (WS-SUB1).EL610
01144                                                                   EL610
01145      IF  AAH-PRM-LENGTH (WS-SUB1) GREATER THAN +0                 EL610
01146          MOVE 'Y'                    TO WS-DATA-KEYED-SW          EL610
01147          MOVE AAH-PRM-IN (WS-SUB1)   TO DEEDIT-FIELD              EL610
01148          PERFORM 8600-DEEDIT                                      EL610
01149          IF  DEEDIT-FIELD-V0     NOT NUMERIC                      EL610
01150              MOVE -1             TO AAH-PRM-LENGTH (WS-SUB1)      EL610
01151              MOVE AL-UNBON       TO AAH-PRM-ATTRB  (WS-SUB1)      EL610
01152              MOVE  ER-3014       TO EMI-ERROR                     EL610
01153              PERFORM 9900-ERROR-FORMAT                            EL610
01154          ELSE                                                     EL610
01155              MOVE DEEDIT-FIELD-V0     TO WS-AH-PRM   (WS-SUB1)    EL610
01156                                          AAH-PRM-OUT (WS-SUB1).   EL610
01157                                                                   EL610
01158      IF  AAH-AMT-LENGTH (WS-SUB1) GREATER THAN +0                 EL610
01159          MOVE 'Y'                    TO WS-DATA-KEYED-SW          EL610
01160          MOVE AAH-AMT-IN (WS-SUB1)   TO DEEDIT-FIELD              EL610
01161          PERFORM 8600-DEEDIT                                      EL610
01162          IF  DEEDIT-FIELD-V0     NOT NUMERIC                      EL610
01163              MOVE -1             TO AAH-AMT-LENGTH (WS-SUB1)      EL610
01164              MOVE AL-UNBON       TO AAH-AMT-ATTRB  (WS-SUB1)      EL610
01165              MOVE  ER-3015       TO EMI-ERROR                     EL610
01166              PERFORM 9900-ERROR-FORMAT                            EL610
01167          ELSE                                                     EL610
01168              MOVE DEEDIT-FIELD-V0     TO WS-AH-AMT   (WS-SUB1)    EL610
01169                                          AAH-AMT-OUT (WS-SUB1).   EL610
01170                                                                   EL610
01171 *    IF  DATA-WAS-KEYED                                           EL610
01172 *        IF  WS-LN-CNT (WS-SUB1) = ZEROS                          EL610
01173 *            IF WS-LN-VOL (WS-SUB1) = ZEROS                       EL610
01174 *               MOVE -1              TO ALN-CNT-LENGTH (WS-SUB1)  EL610
01175 *               MOVE  ER-3007    TO EMI-ERROR                     EL610
01176 *               PERFORM 9900-ERROR-FORMAT.                        EL610
01177                                                                   EL610
01178      IF  DATA-WAS-KEYED                                           EL610
01179          MOVE 'Y'                TO WS-SCREEN-KEYED-SW.           EL610
01180                                                                   EL610
01181      GO TO 6520-PROCESS-LOAN-DATA.                                EL610
01182                                                                   EL610
01183  6590-EXIT.                                                       EL610
01184      EXIT.                                                        EL610
01185                                                                   EL610
01186      EJECT                                                        EL610
01187  7000-BROWSE-FWRD-NEXT-ACCOUNT   SECTION.                         EL610
01188      IF  ACARRL GREATER THAN +0                                   EL610
01189          MOVE ACARRI             TO PI-CR-CARRIER                 EL610
01190         ELSE                                                      EL610
01191          MOVE SPACES             TO PI-CR-CARRIER.                EL610
01192                                                                   EL610
01193      IF  AGRPL GREATER THAN +0                                    EL610
01194          MOVE AGRPI              TO PI-CR-GROUPING                EL610
01195         ELSE                                                      EL610
01196          MOVE SPACES             TO PI-CR-GROUPING.               EL610
01197                                                                   EL610
01198      IF  ASTL GREATER THAN +0                                     EL610
01199          MOVE ASTI               TO PI-CR-STATE                   EL610
01200         ELSE                                                      EL610
01201          MOVE SPACES             TO PI-CR-STATE.                  EL610
01202                                                                   EL610
01203      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.          EL610
01204                                                                   EL610
01205      EXEC CICS HANDLE CONDITION                                   EL610
01206          NOTFND   (7080-END-OF-SEARCH)                            EL610
01207      END-EXEC.                                                    EL610
01208                                                                   EL610
01209      MOVE PI-COMPANY-CD          TO ERLOFC-COMPANY-CD.            EL610
01210      MOVE PI-CR-CARRIER          TO ERLOFC-CARRIER.               EL610
01211      MOVE PI-CR-GROUPING         TO ERLOFC-GROUPING.              EL610
01212      MOVE PI-CR-STATE            TO ERLOFC-STATE.                 EL610
01213      MOVE PI-CR-ACCOUNT          TO ERLOFC-ACCOUNT.               EL610
01214      MOVE HIGH-VALUES            TO ERLOFC-OFFICER-CODE.          EL610
01215                                                                   EL610
01216      EXEC CICS STARTBR                                            EL610
01217          DATASET  (ERLOFC-ID)                                     EL610
01218          RIDFLD   (ERLOFC-KEY)                                    EL610
01219      END-EXEC.                                                    EL610
01220                                                                   EL610
01221      EXEC CICS HANDLE CONDITION                                   EL610
01222          NOTFND   (7070-END-OF-BROWSE)                            EL610
01223          ENDFILE  (7070-END-OF-BROWSE)                            EL610
01224      END-EXEC.                                                    EL610
01225                                                                   EL610
01226  7010-READ-FWRD-NEXT-RECORD.                                      EL610
01227      EXEC CICS READNEXT                                           EL610
01228          DATASET  (ERLOFC-ID)                                     EL610
01229          RIDFLD   (ERLOFC-KEY)                                    EL610
01230          SET      (ADDRESS OF LOAN-OFFICER-MASTER)                   CL**3
01231      END-EXEC.                                                    EL610
01232                                                                   EL610
01233      IF  PI-CR-ACCOUNT = SPACES                                   EL610
01234          MOVE ERLOFC-CARRIER     TO PI-CR-CARRIER                 EL610
01235          MOVE ERLOFC-GROUPING    TO PI-CR-GROUPING                EL610
01236          MOVE ERLOFC-STATE       TO PI-CR-STATE.                  EL610
01237                                                                   EL610
01238      IF  ERLOFC-COMPANY-CD   = PI-COMPANY-CD                      EL610
01239          NEXT SENTENCE                                            EL610
01240         ELSE                                                      EL610
01241          GO TO 7070-END-OF-BROWSE.                                EL610
01242                                                                   EL610
01243      MOVE 'Y'                    TO WS-OFFICER-FOUND-SW.          EL610
01244                                                                   EL610
01245  7070-END-OF-BROWSE.                                              EL610
01246      EXEC CICS ENDBR                                              EL610
01247          DATASET  (ERLOFC-ID)                                     EL610
01248      END-EXEC.                                                    EL610
01249                                                                   EL610
01250       IF OFFICER-WAS-NOT-FOUND NEXT SENTENCE                      EL610
01251         ELSE                                                      EL610
01252          GO TO 7090-EXIT.                                         EL610
01253                                                                   EL610
01254  7080-END-OF-SEARCH.                                              EL610
01255      MOVE -1                     TO AMAINTL.                      EL610
01256      MOVE  ER-2237               TO EMI-ERROR.                    EL610
01257      PERFORM 9900-ERROR-FORMAT.                                   EL610
01258      GO TO 8200-SEND-DATAONLY.                                    EL610
01259                                                                   EL610
01260  7090-EXIT.                                                       EL610
01261      EXIT.                                                        EL610
01262                                                                   EL610
01263      EJECT                                                        EL610
01264  7100-BROWSE-BWRD-NEXT-ACCOUNT   SECTION.                         EL610
01265      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.          EL610
01266                                                                   EL610
01267      EXEC CICS HANDLE CONDITION                                   EL610
01268          NOTFND   (7180-END-OF-SEARCH)                            EL610
01269      END-EXEC.                                                    EL610
01270                                                                   EL610
01271      MOVE PI-PREV-CRLOFC-KEY     TO ERLOFC-KEY.                   EL610
01272                                                                   EL610
01273      EXEC CICS STARTBR                                            EL610
01274          DATASET  (ERLOFC-ID)                                     EL610
01275          RIDFLD   (ERLOFC-KEY)                                    EL610
01276      END-EXEC.                                                    EL610
01277                                                                   EL610
01278      EXEC CICS HANDLE CONDITION                                   EL610
01279          NOTFND   (7170-END-OF-BROWSE)                            EL610
01280          ENDFILE  (7170-END-OF-BROWSE)                            EL610
01281      END-EXEC.                                                    EL610
01282                                                                   EL610
01283  7110-READ-BWRD-NEXT-RECORD.                                      EL610
01284      EXEC CICS READPREV                                           EL610
01285          DATASET  (ERLOFC-ID)                                     EL610
01286          RIDFLD   (ERLOFC-KEY)                                    EL610
01287          SET      (ADDRESS OF LOAN-OFFICER-MASTER)                   CL**3
01288      END-EXEC.                                                    EL610
01289                                                                   EL610
01290      IF  ERLOFC-COMPANY-CD   = PI-COMPANY-CD                      EL610
01291          AND ERLOFC-CARRIER  = PI-CR-CARRIER                      EL610
01292          AND ERLOFC-GROUPING = PI-CR-GROUPING                     EL610
01293          AND ERLOFC-STATE    = PI-CR-STATE                        EL610
01294          AND ERLOFC-ACCOUNT  = PI-CR-ACCOUNT                      EL610
01295          GO TO 7110-READ-BWRD-NEXT-RECORD.                        EL610
01296                                                                   EL610
01297      EXEC CICS ENDBR                                              EL610
01298          DATASET  (ERLOFC-ID)                                     EL610
01299      END-EXEC.                                                    EL610
01300                                                                   EL610
01301      MOVE LOW-VALUES             TO ERLOFC-OFFICER-CODE           EL610
01302                                                                   EL610
01303      EXEC CICS STARTBR                                            EL610
01304          DATASET  (ERLOFC-ID)                                     EL610
01305          RIDFLD   (ERLOFC-KEY)                                    EL610
01306      END-EXEC.                                                    EL610
01307                                                                   EL610
01308      EXEC CICS READNEXT                                           EL610
01309          DATASET  (ERLOFC-ID)                                     EL610
01310          RIDFLD   (ERLOFC-KEY)                                    EL610
01311          SET      (ADDRESS OF LOAN-OFFICER-MASTER)                   CL**3
01312      END-EXEC.                                                    EL610
01313                                                                   EL610
01314      IF  ERLOFC-COMPANY-CD   = PI-COMPANY-CD                      EL610
01315          NEXT SENTENCE                                            EL610
01316         ELSE                                                      EL610
01317          GO TO 7170-END-OF-BROWSE.                                EL610
01318                                                                   EL610
01319      MOVE 'Y'                    TO WS-OFFICER-FOUND-SW.          EL610
01320                                                                   EL610
01321  7170-END-OF-BROWSE.                                              EL610
01322      EXEC CICS ENDBR                                              EL610
01323          DATASET  (ERLOFC-ID)                                     EL610
01324      END-EXEC.                                                    EL610
01325                                                                   EL610
01326      IF OFFICER-WAS-NOT-FOUND NEXT SENTENCE                       EL610
01327        ELSE                                                       EL610
01328         GO TO 7190-EXIT.                                          EL610
01329                                                                   EL610
01330  7180-END-OF-SEARCH.                                              EL610
01331      MOVE -1                     TO AMAINTL.                      EL610
01332      MOVE  ER-2238               TO EMI-ERROR.                    EL610
01333      PERFORM 9900-ERROR-FORMAT.                                   EL610
01334      GO TO 8200-SEND-DATAONLY.                                    EL610
01335                                                                   EL610
01336   7190-EXIT.                                                      EL610
01337      EXIT.                                                        EL610
01338                                                                   EL610
01339      EJECT                                                        EL610
01340  7200-BROWSE-FWRD-NEXT-OFFICER   SECTION.                         EL610
01341      IF  ACARRL GREATER THAN +0                                   EL610
01342          MOVE ACARRI             TO PI-CR-CARRIER                 EL610
01343         ELSE                                                      EL610
01344          MOVE SPACES             TO PI-CR-CARRIER.                EL610
01345                                                                   EL610
01346      IF  AGRPL GREATER THAN +0                                    EL610
01347          MOVE AGRPI              TO PI-CR-GROUPING                EL610
01348         ELSE                                                      EL610
01349          MOVE SPACES             TO PI-CR-GROUPING.               EL610
01350                                                                   EL610
01351      IF  ASTL GREATER THAN +0                                     EL610
01352          MOVE ASTI               TO PI-CR-STATE                   EL610
01353         ELSE                                                      EL610
01354          MOVE SPACES             TO PI-CR-STATE.                  EL610
01355                                                                   EL610
01356      IF  AOFCRL GREATER THAN +0                                   EL610
01357          MOVE AOFCRI             TO PI-LOAN-OFFICER               EL610
01358         ELSE                                                      EL610
01359          MOVE SPACES             TO PI-LOAN-OFFICER.              EL610
01360                                                                   EL610
01361      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.          EL610
01362                                                                   EL610
01363      EXEC CICS HANDLE CONDITION                                   EL610
01364          NOTFND   (7280-END-OF-SEARCH)                            EL610
01365      END-EXEC.                                                    EL610
01366                                                                   EL610
01367      MOVE PI-COMPANY-CD          TO ERLOFC-COMPANY-CD.            EL610
01368      MOVE PI-CR-CARRIER          TO ERLOFC-CARRIER.               EL610
01369      MOVE PI-CR-GROUPING         TO ERLOFC-GROUPING.              EL610
01370      MOVE PI-CR-STATE            TO ERLOFC-STATE.                 EL610
01371      MOVE PI-CR-ACCOUNT          TO ERLOFC-ACCOUNT.               EL610
01372      MOVE PI-LOAN-OFFICER        TO ERLOFC-OFFICER-CODE.          EL610
01373                                                                   EL610
01374      EXEC CICS STARTBR                                            EL610
01375          DATASET  (ERLOFC-ID)                                     EL610
01376          RIDFLD   (ERLOFC-KEY)                                    EL610
01377      END-EXEC.                                                    EL610
01378                                                                   EL610
01379      EXEC CICS HANDLE CONDITION                                   EL610
01380          NOTFND   (7270-END-OF-BROWSE)                            EL610
01381          ENDFILE  (7270-END-OF-BROWSE)                            EL610
01382      END-EXEC.                                                    EL610
01383                                                                   EL610
01384  7210-READ-FWRD-NEXT-RECORD.                                      EL610
01385      EXEC CICS READNEXT                                           EL610
01386          DATASET  (ERLOFC-ID)                                     EL610
01387          RIDFLD   (ERLOFC-KEY)                                    EL610
01388          SET      (ADDRESS OF LOAN-OFFICER-MASTER)                   CL**3
01389      END-EXEC.                                                    EL610
01390                                                                   EL610
01391      IF  ERLOFC-KEY = PI-CRLOFC-KEY                               EL610
01392          GO TO 7210-READ-FWRD-NEXT-RECORD.                        EL610
01393                                                                   EL610
01394      IF  PI-CR-ACCOUNT = SPACES                                   EL610
01395          MOVE ERLOFC-CARRIER     TO PI-CR-CARRIER                 EL610
01396          MOVE ERLOFC-GROUPING    TO PI-CR-GROUPING                EL610
01397          MOVE ERLOFC-STATE       TO PI-CR-STATE                   EL610
01398          MOVE ERLOFC-ACCOUNT     TO PI-CR-ACCOUNT.                EL610
01399                                                                   EL610
01400      IF  ERLOFC-COMPANY-CD   = PI-COMPANY-CD                      EL610
01401          AND ERLOFC-CARRIER  = PI-CR-CARRIER                      EL610
01402          AND ERLOFC-GROUPING = PI-CR-GROUPING                     EL610
01403          AND ERLOFC-STATE    = PI-CR-STATE                        EL610
01404          AND ERLOFC-ACCOUNT  = PI-CR-ACCOUNT                      EL610
01405          NEXT SENTENCE                                            EL610
01406         ELSE                                                      EL610
01407          GO TO 7270-END-OF-BROWSE.                                EL610
01408                                                                   EL610
01409      MOVE 'Y'                    TO WS-OFFICER-FOUND-SW.          EL610
01410                                                                   EL610
01411  7270-END-OF-BROWSE.                                              EL610
01412      EXEC CICS ENDBR                                              EL610
01413          DATASET  (ERLOFC-ID)                                     EL610
01414      END-EXEC.                                                    EL610
01415                                                                   EL610
01416       IF OFFICER-WAS-NOT-FOUND NEXT SENTENCE                      EL610
01417         ELSE                                                      EL610
01418          GO TO 7290-EXIT.                                         EL610
01419                                                                   EL610
01420  7280-END-OF-SEARCH.                                              EL610
01421      MOVE -1                     TO AMAINTL.                      EL610
01422      MOVE  ER-3018               TO EMI-ERROR.                    EL610
01423      PERFORM 9900-ERROR-FORMAT.                                   EL610
01424      GO TO 8200-SEND-DATAONLY.                                    EL610
01425                                                                   EL610
01426  7290-EXIT.                                                       EL610
01427      EXIT.                                                        EL610
01428                                                                   EL610
01429      EJECT                                                        EL610
01430  7300-BROWSE-BWRD-NEXT-OFFICER   SECTION.                         EL610
01431      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.          EL610
01432                                                                   EL610
01433      EXEC CICS HANDLE CONDITION                                   EL610
01434          NOTFND   (7380-END-OF-SEARCH)                            EL610
01435      END-EXEC.                                                    EL610
01436                                                                   EL610
01437      MOVE PI-PREV-CRLOFC-KEY     TO ERLOFC-KEY.                   EL610
01438                                                                   EL610
01439      EXEC CICS STARTBR                                            EL610
01440          DATASET  (ERLOFC-ID)                                     EL610
01441          RIDFLD   (ERLOFC-KEY)                                    EL610
01442      END-EXEC.                                                    EL610
01443                                                                   EL610
01444      EXEC CICS HANDLE CONDITION                                   EL610
01445          NOTFND   (7370-END-OF-BROWSE)                            EL610
01446          ENDFILE  (7370-END-OF-BROWSE)                            EL610
01447      END-EXEC.                                                    EL610
01448                                                                   EL610
01449  7310-READ-BWRD-NEXT-RECORD.                                      EL610
01450      EXEC CICS READPREV                                           EL610
01451          DATASET  (ERLOFC-ID)                                     EL610
01452          RIDFLD   (ERLOFC-KEY)                                    EL610
01453          SET      (ADDRESS OF LOAN-OFFICER-MASTER)                   CL**3
01454      END-EXEC.                                                    EL610
01455                                                                   EL610
01456      IF  ERLOFC-KEY = PI-CRLOFC-KEY                               EL610
01457          GO TO 7310-READ-BWRD-NEXT-RECORD.                        EL610
01458                                                                   EL610
01459      IF  ERLOFC-COMPANY-CD   = PI-COMPANY-CD                      EL610
01460          AND ERLOFC-CARRIER  = PI-CR-CARRIER                      EL610
01461          AND ERLOFC-GROUPING = PI-CR-GROUPING                     EL610
01462          AND ERLOFC-STATE    = PI-CR-STATE                        EL610
01463          AND ERLOFC-ACCOUNT  = PI-CR-ACCOUNT                      EL610
01464          NEXT SENTENCE                                            EL610
01465         ELSE                                                      EL610
01466          GO TO 7370-END-OF-BROWSE.                                EL610
01467                                                                   EL610
01468      MOVE 'Y'                    TO WS-OFFICER-FOUND-SW.          EL610
01469                                                                   EL610
01470  7370-END-OF-BROWSE.                                              EL610
01471      EXEC CICS ENDBR                                              EL610
01472          DATASET  (ERLOFC-ID)                                     EL610
01473      END-EXEC.                                                    EL610
01474                                                                   EL610
01475      IF OFFICER-WAS-NOT-FOUND NEXT SENTENCE                       EL610
01476        ELSE                                                       EL610
01477         GO TO 7390-EXIT.                                          EL610
01478                                                                   EL610
01479  7380-END-OF-SEARCH.                                              EL610
01480      MOVE -1                     TO AMAINTL.                      EL610
01481      MOVE  ER-3019               TO EMI-ERROR.                    EL610
01482      PERFORM 9900-ERROR-FORMAT.                                   EL610
01483      GO TO 8200-SEND-DATAONLY.                                    EL610
01484                                                                   EL610
01485  7390-EXIT.                                                       EL610
01486      EXIT.                                                        EL610
01487                                                                   EL610
01488      EJECT                                                        EL610
01489  7400-READ-LOAN-MASTER      SECTION.                              EL610
01490      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.          EL610
01491                                                                   EL610
01492      EXEC CICS HANDLE CONDITION                                   EL610
01493          NOTFND   (7440-OFFICER-NOTFND)                           EL610
01494      END-EXEC.                                                    EL610
01495                                                                   EL610
01496      EXEC CICS READ                                               EL610
01497          DATASET   (ERLOFC-ID)                                    EL610
01498          SET       (ADDRESS OF LOAN-OFFICER-MASTER)                  CL**3
01499          RIDFLD    (ERLOFC-KEY)                                   EL610
01500          EQUAL                                                    EL610
01501      END-EXEC.                                                    EL610
01502                                                                   EL610
01503      MOVE 'Y'                    TO WS-OFFICER-FOUND-SW.          EL610
01504                                                                   EL610
01505      GO TO 7450-EXIT.                                             EL610
01506                                                                   EL610
01507  7440-OFFICER-NOTFND.                                             EL610
01508      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.          EL610
01509                                                                   EL610
01510  7450-EXIT.                                                       EL610
01511      EXIT.                                                        EL610
01512      EJECT                                                        EL610
01513                                                                   EL610
01514  7460-READ-LOAN-MST-UPDT    SECTION.                              EL610
01515      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.          EL610
01516                                                                   EL610
01517      EXEC CICS HANDLE CONDITION                                   EL610
01518          NOTFND   (7480-OFFICER-NOTFND)                           EL610
01519      END-EXEC.                                                    EL610
01520                                                                   EL610
01521      EXEC CICS READ                                               EL610
01522          DATASET   (ERLOFC-ID)                                    EL610
01523          SET       (ADDRESS OF LOAN-OFFICER-MASTER)                  CL**3
01524          RIDFLD    (ERLOFC-KEY)                                   EL610
01525          EQUAL                                                    EL610
01526          UPDATE                                                   EL610
01527      END-EXEC.                                                    EL610
01528                                                                   EL610
01529      MOVE 'Y'                    TO WS-OFFICER-FOUND-SW.          EL610
01530                                                                   EL610
01531      GO TO 7490-EXIT.                                             EL610
01532                                                                   EL610
01533  7480-OFFICER-NOTFND.                                             EL610
01534      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.          EL610
01535                                                                   EL610
01536  7490-EXIT.                                                       EL610
01537      EXIT.                                                        EL610
01538      EJECT                                                        EL610
01539  7500-WRITE-LOAN-MASTER          SECTION.                         EL610
01540      EXEC CICS HANDLE CONDITION                                   EL610
01541          DUPREC   (7550-DUPLICATE-RECORD)                         EL610
01542      END-EXEC.                                                    EL610
01543                                                                   EL610
01544      EXEC CICS WRITE                                              EL610
01545          FROM      (LOAN-OFFICER-MASTER)                          EL610
01546          DATASET   (ERLOFC-ID)                                    EL610
01547          RIDFLD    (ERLOFC-KEY)                                   EL610
01548      END-EXEC.                                                    EL610
01549                                                                   EL610
01550      GO TO 7590-EXIT.                                             EL610
01551                                                                   EL610
01552  7550-DUPLICATE-RECORD.                                           EL610
01553      MOVE -1                     TO AMAINTL.                      EL610
01554      MOVE  ER-0132               TO EMI-ERROR.                    EL610
01555      PERFORM 9900-ERROR-FORMAT.                                   EL610
01556      GO TO 8200-SEND-DATAONLY.                                    EL610
01557                                                                   EL610
01558  7590-EXIT.                                                       EL610
01559      EXIT.                                                        EL610
01560                                                                   EL610
01561      EJECT                                                        EL610
01562  7600-REWRITE-LOAN-MASTER    SECTION.                             EL610
01563      EXEC CICS REWRITE                                            EL610
01564          FROM     (LOAN-OFFICER-MASTER)                           EL610
01565          DATASET  (ERLOFC-ID)                                     EL610
01566      END-EXEC.                                                    EL610
01567                                                                   EL610
01568  7690-EXIT.                                                       EL610
01569      EXIT.                                                        EL610
01570  7700-DELETE-LOAN-MASTER    SECTION.                              EL610
01571      EXEC CICS DELETE                                             EL610
01572          DATASET  (ERLOFC-ID)                                     EL610
01573      END-EXEC.                                                    EL610
01574                                                                   EL610
01575  7790-EXIT.                                                       EL610
01576      EXIT.                                                        EL610
01577                                                                   EL610
01578      EJECT                                                        EL610
01579  7800-READ-ACCOUNT-MASTER   SECTION.                              EL610
01580      MOVE PI-COMPANY-CD          TO ERACCT-A-CO-CD.               EL610
01581      MOVE PI-CR-CARRIER          TO ERACCT-A-CARRIER.             EL610
01582      MOVE PI-CR-GROUPING         TO ERACCT-A-GROUPING.            EL610
01583      MOVE PI-CR-STATE            TO ERACCT-A-STATE.               EL610
01584      MOVE PI-CR-ACCOUNT          TO ERACCT-A-ACCOUNT.             EL610
01585      MOVE LOW-VALUES             TO ERACCT-A-EXP-DATE.            EL610
01586                                                                   EL610
01587      EXEC CICS HANDLE CONDITION                                   EL610
01588          NOTFND   (7870-ACCOUNT-INVALID)                          EL610
01589      END-EXEC.                                                    EL610
01590                                                                   EL610
01591      EXEC CICS READ                                               EL610
01592          DATASET   (ERACCT-ALT-FILE-ID)                           EL610
01593          SET       (ADDRESS OF ACCOUNT-MASTER)                       CL**3
01594          RIDFLD    (ERACCT-ALT-KEY)                               EL610
01595          GTEQ                                                     EL610
01596      END-EXEC.                                                    EL610
01597                                                                   EL610
01598      IF PI-COMPANY-CD  = AM-COMPANY-CD-A1  AND                    EL610
01599         PI-CR-CARRIER  = AM-VG-CARRIER     AND                    EL610
01600         PI-CR-GROUPING = AM-VG-GROUPING    AND                    EL610
01601         PI-CR-STATE    = AM-VG-STATE       AND                    EL610
01602         PI-CR-ACCOUNT  = AM-VG-ACCOUNT                            EL610
01603            GO TO 7890-EXIT.                                       EL610
01604                                                                   EL610
01605  7870-ACCOUNT-INVALID.                                            EL610
01606      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL610
01607          MOVE -1                 TO ACARRL                        EL610
01608          MOVE AL-UABON           TO ACARRA                        EL610
01609                                     AGRPA                         EL610
01610                                     ASTA                          EL610
01611                                     AACCTA                        EL610
01612      ELSE                                                         EL610
01613          IF ST-ACCNT-CNTL                                         EL610
01614              MOVE -1             TO ASTL                          EL610
01615              MOVE AL-UABON       TO ASTA                          EL610
01616                                     AACCTA                        EL610
01617          ELSE                                                     EL610
01618              IF CARR-ST-ACCNT-CNTL                                EL610
01619                  MOVE -1             TO ACARRL                    EL610
01620                  MOVE AL-UABON       TO ACARRA                    EL610
01621                                         ASTA                      EL610
01622                                         AACCTA                    EL610
01623              ELSE                                                 EL610
01624                  IF ACCNT-CNTL                                    EL610
01625                      MOVE -1         TO AACCTL                    EL610
01626                      MOVE AL-UABON   TO AACCTA                    EL610
01627                  ELSE                                             EL610
01628                      MOVE AL-UABON   TO ACARRA                    EL610
01629                                         AACCTA.                   EL610
01630                                                                   EL610
01631      MOVE  ER-2210               TO EMI-ERROR.                    EL610
01632      PERFORM 9900-ERROR-FORMAT.                                   EL610
01633                                                                   EL610
01634  7890-EXIT.                                                       EL610
01635      EXIT.                                                        EL610
01636                                                                   EL610
01637      EJECT                                                        EL610
01638  8100-SEND-INITIAL-MAP SECTION.                                   EL610
01639      MOVE SAVE-DATE              TO ADATEO.                       EL610
01640      MOVE EIBTIME                TO TIME-IN.                      EL610
01641      MOVE TIME-OUT               TO ATIMEO.                       EL610
01642      MOVE -1                     TO AMAINTL.                      EL610
01643      MOVE EMI-MESSAGE-AREA (1)   TO AERRMSGO.                     EL610
01644                                                                   EL610
01645      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL610
01646          NEXT SENTENCE                                            EL610
01647      ELSE                                                         EL610
01648          IF ST-ACCNT-CNTL                                         EL610
01649              MOVE AL-SADOF       TO ACARHDGA                      EL610
01650                                     AGRPHDGA                      EL610
01651              MOVE AL-SANOF       TO ACARRA                        EL610
01652                                     AGRPA                         EL610
01653          ELSE                                                     EL610
01654              IF CARR-ST-ACCNT-CNTL                                EL610
01655                  MOVE AL-SADOF   TO AGRPHDGA                      EL610
01656                  MOVE AL-SANOF   TO AGRPA                         EL610
01657              ELSE                                                 EL610
01658                  IF ACCNT-CNTL                                    EL610
01659                      MOVE AL-SADOF TO ACARHDGA                    EL610
01660                                       AGRPHDGA                    EL610
01661                                       ASTHDGA                     EL610
01662                      MOVE AL-SANOF TO ACARRA                      EL610
01663                                       AGRPA                       EL610
01664                                       ASTA                        EL610
01665                  ELSE                                             EL610
01666                      IF CARR-ACCNT-CNTL                           EL610
01667                          MOVE AL-SADOF TO AGRPHDGA                EL610
01668                                           ASTHDGA                 EL610
01669                          MOVE AL-SANOF TO AGRPA                   EL610
01670                                           ASTA.                   EL610
01671                                                                   EL610
01672      IF  PI-PROCESSOR-ID = 'LGXX'                                 EL610
01673          NEXT SENTENCE                                            EL610
01674         ELSE                                                      EL610
01675          GO TO 8150-SEND-INITIAL-MAP.                             EL610
01676                                                                   EL610
01677      MOVE +0                     TO WS-SUB1.                      EL610
01678                                                                   EL610
01679  8110-SET-INITIAL-ATTRIBUTES.                                     EL610
01680      ADD +1                      TO WS-SUB1.                      EL610
01681                                                                   EL610
01682      IF  WS-SUB1 GREATER THAN +12                                 EL610
01683          GO TO 8150-SEND-INITIAL-MAP.                             EL610
01684                                                                   EL610
01685      MOVE AL-UANOF               TO ALF-CNT-ATTRB    (WS-SUB1)    EL610
01686                                     ALF-PRM-ATTRB    (WS-SUB1)    EL610
01687                                     ALF-AMT-ATTRB    (WS-SUB1)    EL610
01688                                     AAH-CNT-ATTRB    (WS-SUB1)    EL610
01689                                     AAH-PRM-ATTRB    (WS-SUB1)    EL610
01690                                     AAH-AMT-ATTRB    (WS-SUB1).   EL610
01691                                                                   EL610
01692      GO TO 8110-SET-INITIAL-ATTRIBUTES.                           EL610
01693                                                                   EL610
01694  8150-SEND-INITIAL-MAP.                                           EL610
01695      EXEC CICS SEND                                               EL610
01696          MAP      (MAP-NAME)                                      EL610
01697          MAPSET   (MAPSET-NAME)                                   EL610
01698          FROM     (EL610AO)                                       EL610
01699          ERASE                                                    EL610
01700          CURSOR                                                   EL610
01701      END-EXEC.                                                    EL610
01702                                                                   EL610
01703      GO TO 9100-RETURN-TRAN.                                      EL610
01704                                                                   EL610
01705  8200-SEND-DATAONLY     SECTION.                                  EL610
01706      MOVE SAVE-DATE              TO ADATEO.                       EL610
01707      MOVE EIBTIME                TO TIME-IN.                      EL610
01708      MOVE TIME-OUT               TO ATIMEO.                       EL610
01709      MOVE EMI-MESSAGE-AREA (1)   TO AERRMSGO.                     EL610
01710      EXEC CICS SEND                                               EL610
01711          MAP      (MAP-NAME)                                      EL610
01712          MAPSET   (MAPSET-NAME)                                   EL610
01713          FROM     (EL610AO)                                       EL610
01714          DATAONLY                                                 EL610
01715          CURSOR                                                   EL610
01716      END-EXEC.                                                    EL610
01717                                                                   EL610
01718      GO TO 9100-RETURN-TRAN.                                      EL610
01719                                                                   EL610
01720      EJECT                                                        EL610
01721  8300-SEND-TEXT         SECTION.                                  EL610
01722      EXEC CICS SEND TEXT                                          EL610
01723          FROM     (LOGOFF-TEXT)                                   EL610
01724          LENGTH   (LOGOFF-LENGTH)                                 EL610
01725          ERASE                                                    EL610
01726          FREEKB                                                   EL610
01727      END-EXEC.                                                    EL610
01728                                                                   EL610
01729      EXEC CICS RETURN                                             EL610
01730      END-EXEC.                                                    EL610
01731                                                                   EL610
01732      EJECT                                                        EL610
01733  8400-LOG-JOURNAL-RECORD         SECTION.                         EL610
01734      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL610
01735      MOVE ERLOFC-ID              TO JP-FILE-ID.                   EL610
01736      MOVE THIS-PGM               TO JP-PROGRAM-ID.                EL610
pemuni*    EXEC CICS JOURNAL                                            EL610
pemuni*        JFILEID     (PI-JOURNAL-FILE-ID)                         EL610
pemuni*        JTYPEID     ('CR')                                       EL610
pemuni*        FROM        (JOURNAL-RECORD)                             EL610
pemuni*        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   EL610
pemuni*    END-EXEC.                                                    EL610
01743                                                                   EL610
01744  8400-EXIT.                                                       EL610
01745      EXIT.                                                        EL610
01746                                                                   EL610
01747                                                                   EL610
01748  8600-DEEDIT           SECTION.                                   EL610
01749      EXEC CICS BIF DEEDIT                                         EL610
01750           FIELD   (DEEDIT-FIELD)                                  EL610
01751           LENGTH  (15)                                            EL610
01752       END-EXEC.                                                   EL610
01753                                                                   EL610
01754  8800-UNAUTHORIZED-ACCESS        SECTION.                         EL610
01755      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL610
01756      GO TO 8300-SEND-TEXT.                                        EL610
01757                                                                   EL610
01758  8810-PF23              SECTION.                                  EL610
01759      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL610
01760      MOVE XCTL-005               TO PGM-NAME.                     EL610
01761      GO TO 9300-XCTL.                                             EL610
01762                                                                   EL610
01763  9000-RETURN-CICS       SECTION.                                  EL610
01764      EXEC CICS RETURN                                             EL610
01765      END-EXEC.                                                    EL610
01766                                                                   EL610
01767  9100-RETURN-TRAN       SECTION.                                  EL610
01768      MOVE    EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.          EL610
01769      MOVE SCREEN-NUMBER             TO PI-CURRENT-SCREEN-NO.      EL610
01770      EXEC CICS RETURN                                             EL610
01771          TRANSID    (TRANS-ID)                                    EL610
01772          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL610
01773          LENGTH     (PI-COMM-LENGTH)                              EL610
01774      END-EXEC.                                                    EL610
01775                                                                   EL610
01776  9200-RETURN-MAIN-MENU SECTION.                                   EL610
01777      MOVE XCTL-626               TO PGM-NAME.                     EL610
01778      GO TO 9300-XCTL.                                             EL610
01779                                                                   EL610
01780  9300-XCTL             SECTION.                                   EL610
01781      EXEC CICS XCTL                                               EL610
01782          PROGRAM    (PGM-NAME)                                    EL610
01783          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL610
01784          LENGTH     (PI-COMM-LENGTH)                              EL610
01785      END-EXEC.                                                    EL610
01786                                                                   EL610
01787  9400-CLEAR            SECTION.                                   EL610
01788      MOVE SPACES                 TO PI-CR-CONTROL-IN-PROGRESS     EL610
01789                                     PI-LOAN-OFFICER.              EL610
01790                                                                   EL610
01791      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL610
01792      GO TO 9300-XCTL.                                             EL610
01793                                                                   EL610
01794  9500-PF12             SECTION.                                   EL610
01795      MOVE XCTL-010               TO PGM-NAME.                     EL610
01796      GO TO 9300-XCTL.                                             EL610
01797                                                                   EL610
01798  9600-PGMID-ERROR      SECTION.                                   EL610
01799      EXEC CICS HANDLE CONDITION                                   EL610
01800          PGMIDERR    (8300-SEND-TEXT)                             EL610
01801          END-EXEC.                                                EL610
01802                                                                   EL610
01803      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL610
01804      MOVE ' '                    TO PI-ENTRY-CD-1.                EL610
01805      MOVE XCTL-005               TO PGM-NAME.                     EL610
01806      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL610
01807      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL610
01808      GO TO 9300-XCTL.                                             EL610
01809                                                                   EL610
01810  9700-DATE-LINK         SECTION.                                  EL610
01811      MOVE LINK-ELDATCV           TO PGM-NAME                      EL610
01812      EXEC CICS LINK                                               EL610
01813          PROGRAM    (PGM-NAME)                                    EL610
01814          COMMAREA   (DATE-CONVERSION-DATA)                        EL610
01815          LENGTH     (DC-COMM-LENGTH)                              EL610
01816      END-EXEC.                                                    EL610
01817                                                                   EL610
01818  9900-ERROR-FORMAT       SECTION.                                 EL610
01819      IF NOT EMI-ERRORS-COMPLETE                                   EL610
01820          MOVE LINK-001           TO PGM-NAME                      EL610
01821          EXEC CICS LINK                                           EL610
01822              PROGRAM    (PGM-NAME)                                EL610
01823              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL610
01824              LENGTH     (EMI-COMM-LENGTH)                         EL610
01825          END-EXEC.                                                EL610
01826                                                                   EL610
01827  9900-EXIT.                                                       EL610
01828      EXIT.                                                        EL610
01829                                                                   EL610
01830  9990-ABEND             SECTION.                                  EL610
01831      MOVE LINK-004               TO PGM-NAME.                     EL610
01832      MOVE DFHEIBLK               TO EMI-LINE1                     EL610
01833                                                                   EL610
01834      EXEC CICS LINK                                               EL610
01835          PROGRAM   (PGM-NAME)                                     EL610
01836          COMMAREA  (EMI-LINE1)                                    EL610
01837          LENGTH    (72)                                           EL610
01838      END-EXEC.                                                    EL610
01839                                                                   EL610
01840      MOVE -1                     TO AMAINTL.                      EL610
01841      GO TO 8200-SEND-DATAONLY.                                    EL610
01842                                                                   EL610
01843  9995-SECURITY-VIOLATION.                                         EL610
01844             COPY ELCSCTP.                                         EL610
01845                                                                   EL610
01846  9995-EXIT.                                                       EL610
01847       EXIT.                                                       EL610
01848                                                                   EL610
01849                                                                   EL610
