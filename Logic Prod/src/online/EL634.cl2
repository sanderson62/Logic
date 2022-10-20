00001  IDENTIFICATION DIVISION.                                         04/22/98
00002                                                                   EL634
00003  PROGRAM-ID.                 EL634 .                                 LV012
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 02/12/96 09:51:04.                    CL**7
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE               CL*10
00008 *                            VMOD=2.009                              CL*10
00009                                                                   EL634
00010 *AUTHOR.        LOGIC,INC.                                           CL**7
00011 *               DALLAS, TEXAS.                                       CL**7
00012                                                                   EL634
00013 *DATE-COMPILED.                                                      CL**7
00014                                                                   EL634
00015 *SECURITY.   *****************************************************   CL**7
00016 *            *                                                   *   CL**7
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00018 *            *                                                   *   CL**7
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00022 *            *                                                   *   CL**7
00023 *            *****************************************************   CL**7
00024                                                                   EL634
00025 *REMARKS.                                                            CL**2
00026 *        TRANSACTION - EXB9 - RETRO/REINSURANCE ADJUSTMENTS          CL**8
00027                                                                   EL634
00028  ENVIRONMENT DIVISION.                                            EL634
00029  DATA DIVISION.                                                   EL634
00030  EJECT                                                            EL634
00031  WORKING-STORAGE SECTION.                                         EL634
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL634
00033  77  FILLER  PIC X(32)  VALUE '*    EL634 WORKING STORAGE     *'. EL634
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.009 *********'.    CL*10
00035                                                                   EL634
00036                              COPY ELCSCTM.                           CL**4
00037                              COPY ELCSCRTY.                          CL**4
00038     EJECT                                                         EL634
00039                                                                   EL634
00040  01  STANDARD-AREAS.                                              EL634
00041      12  SC-ITEM             PIC  S9(4) COMP VALUE +1.            EL634
00042      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.         EL634
00043      12  EL634A              PIC  X(8)       VALUE 'EL634A'.      EL634
00044      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL634S'.      EL634
00045      12  SCREEN-NUMBER       PIC  X(4)       VALUE '634A'.        EL634
00046      12  TRANS-ID            PIC  X(4)       VALUE 'EXB9'.        EL634
00047      12  THIS-PGM            PIC  X(8)       VALUE 'EL634'.       EL634
00048      12  PGM-NAME            PIC  X(8).                           EL634
00049      12  TIME-IN             PIC S9(7).                           EL634
00050      12  TIME-OUT-R  REDEFINES  TIME-IN.                          EL634
00051          16  FILLER          PIC  X.                              EL634
00052          16  TIME-OUT        PIC  99V99.                          EL634
00053          16  FILLER          PIC  X(2).                           EL634
00054      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.       EL634
00055      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.       EL634
00056      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.       EL634
00057      12  LINK-001            PIC  X(8)       VALUE 'EL001'.       EL634
00058      12  LINK-004            PIC  X(8)       VALUE 'EL004'.       EL634
00059      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.     EL634
00060      12  ELCNTL-FILE-ID      PIC  X(8)       VALUE 'ELCNTL'.      EL634
00061      12  ERACCT-FILE-ID      PIC  X(8)       VALUE 'ERACCT'.      EL634
00062      12  ERACCT-ALT-FILE-ID  PIC  X(8)       VALUE 'ERACCT2'.     EL634
00063      12  ERREPY-FILE-ID      PIC  X(8)       VALUE 'ERREPY'.      EL634
00064      12  ERREIN-FILE-ID      PIC  X(8)       VALUE 'ERREIN'.      EL634
00065      12  RETURNED-FROM       PIC  X(8)       VALUE SPACES.        EL634
00066      12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.        EL634
00067      12  WS-CURRENT-BIN-DT   PIC  X(2)       VALUE SPACES.        EL634
00068      12  WS-EXP-DT-HLD       PIC  X(2)       VALUE SPACES.        EL634
00069      12  WS-EFF-DT-HLD       PIC  X(2)       VALUE SPACES.        EL634
00070      12  WS-INFORCEI         PIC  S9(9)V99.                          CL**5
00071      12  WS-MORTAMTI         PIC  S9(9)V99.                          CL**5
00072      12  WS-FUTUREI          PIC  S9(9)V99.                          CL**5
00073      12  WS-PTCI             PIC  S9(9)V99.                          CL**5
00074      12  WS-IBNRI            PIC  S9(9)V99.                          CL**5
00075      12  WS-CLAIMI           PIC  S9(9)V99.                          CL**5
00076      12  WS-EXPI             PIC  S9(9)V99.                          CL**5
00077      12  WS-PYMNTI           PIC  S9(9)V99.                          CL**5
00078      12  WS-OCOMMI           PIC  S9(9)V99.                          CL**5
00079      12  WS-RPREMI           PIC  S9(9)V99.                          CL**5
00080                                                                   EL634
00081  01  WORK-AREAS.                                                  EL634
00082      12  WS-SV-CARRIER               PIC  X      VALUE SPACES.    EL634
00083      12  WS-SV-GROUPING              PIC  X(6)   VALUE SPACES.    EL634
00084      12  WS-SV-STATE                 PIC  X(2)   VALUE SPACES.    EL634
CIDMOD     12  WS-DEEDIT-FIELD             PIC S9(9)V9(2).                   000
CIDMOD     12  WS-DEEDIT-FIELD-V0  REDEFINES                                 000
CIDMOD         WS-DEEDIT-FIELD             PIC S9(11).                       000
00085      12  WS-DEEDIT-DATE              PIC  X(8)   VALUE SPACES.    EL634
00086      12  FILLER  REDEFINES  WS-DEEDIT-DATE.                       EL634
00087          16  FILLER                  PIC  X(2).                   EL634
00088          16  WS-DEEDIT-DATE-6        PIC  X(6).                   EL634
00089      12  EDIT-SUB                    PIC  9(3)   VALUE ZEROS.     EL634
00090      12  ED-SUB                      PIC  9(3)   VALUE ZEROS.        CL**4
00091      12  WS-EDITED-BEN-CD            PIC  X(2)   VALUE SPACES.       CL**3
00092      12  WS-BIN-EFF-DATE-ENTERED     PIC  X(2)   VALUE SPACES.    EL634
00093      12  WS-EFF-DATE-ENTERED         PIC  X(8).                   EL634
00094      12  WS-PI-END-DATE.                                          EL634
00095          16  WS-PI-END-MO            PIC  99.                     EL634
00096          16  WS-PI-END-DA            PIC  99.                     EL634
00097          16  WS-PI-END-YR            PIC  99.                     EL634
00098      12  WS-END-DATE1.                                            EL634
               16  WS-END-CENT1            PIC  99   VALUE 20.
00099          16  WS-END-YR1              PIC  99.                     EL634
00100          16  WS-END-MO1              PIC  99.                     EL634
00101      12  WS-END-DATE2.                                            EL634
               16  WS-END-CENT2            PIC  99   VALUE 20.
00102          16  WS-END-YR2              PIC  99.                     EL634
00103          16  WS-END-MO2              PIC  99.                     EL634
00104  EJECT                                                            EL634
00105  01  ACCESS-KEYS.                                                 EL634
00106      12  ELCNTL-KEY.                                              EL634
00107          16  CNTL-COMP-ID        PIC  X(3)   VALUE SPACES.        EL634
00108          16  CNTL-REC-TYPE       PIC  X      VALUE SPACES.        EL634
00109          16  CNTL-ACCESS.                                         EL634
00110              20  CNTL-STATE      PIC  X(2)   VALUE SPACES.        EL634
00111              20  CNTL-HI-BEN-CD.                                     CL**4
00112                  24  FILLER       PIC  X      VALUE SPACES.          CL**4
00113                  24  CNTL-CARRIER PIC  X      VALUE SPACES.          CL**4
00114          16  CNTL-SEQ            PIC S9(4)   VALUE +0   COMP.     EL634
00115      12  ERACCT-KEY.                                              EL634
00116          16  ERACCT-COMP-KEY.                                     EL634
00117              20  ACCT-CO         PIC  X      VALUE SPACES.        EL634
00118              20  ACCT-CARRIER    PIC  X      VALUE SPACES.        EL634
00119              20  ACCT-GROUPING   PIC  X(6)   VALUE SPACES.        EL634
00120              20  ACCT-STATE      PIC  X(2)   VALUE SPACES.        EL634
00121              20  ACCT-ACCOUNT    PIC  X(10)  VALUE SPACES.        EL634
00122          16  ACCT-EXP-DATE       PIC  X(2)   VALUE SPACES.        EL634
00123      12  ERACCT-SAVE-KEY-22.                                         CL**3
00124          16  ERACCT-SAVE-KEY     PIC  X(20)  VALUE SPACES.           CL**3
00125          16  FILLER              PIC  XX.                            CL**3
00126      12  ERREPY-KEY.                                              EL634
00127          16  REPY-COMPANY-CD     PIC  X      VALUE SPACE.         EL634
00128          16  REPY-CARRIER        PIC  X      VALUE SPACE.         EL634
00129          16  REPY-GROUPING       PIC  X(6)   VALUE SPACES.        EL634
00130          16  REPY-STATE          PIC  X(2)   VALUE SPACES.        EL634
00131          16  REPY-ACCOUNT        PIC  X(10)  VALUE SPACES.        EL634
00132          16  REPY-RECORD-SEQ     PIC S9(8)   VALUE +0   COMP.     EL634
00133          16  REPY-RECORD-TYPE    PIC  X      VALUE '1'.           EL634
00134      12  ERREIN-KEY.                                              EL634
00135          16  REIN-COMPANY-CD     PIC  X      VALUE SPACE.         EL634
00136          16  REIN-CODE           PIC  X      VALUE SPACE.         EL634
00137          16  REIN-TABLE          PIC  X(6)   VALUE SPACES.        EL634
00138  EJECT                                                            EL634
00139  01  ERROR-NUMBERS.                                               EL634
00140      12  ER-0000             PIC  X(4)       VALUE '0000'.        EL634
00141      12  ER-0004             PIC  X(4)       VALUE '0004'.        EL634
00142      12  ER-0008             PIC  X(4)       VALUE '0008'.        EL634
00143      12  ER-0023             PIC  X(4)       VALUE '0023'.        EL634
00144      12  ER-0026             PIC  X(4)       VALUE '0026'.        EL634
00145      12  ER-0029             PIC  X(4)       VALUE '0029'.        EL634
00146      12  ER-0070             PIC  X(4)       VALUE '0070'.        EL634
00147      12  ER-0194             PIC  X(4)       VALUE '0194'.        EL634
00148      12  ER-0195             PIC  X(4)       VALUE '0195'.        EL634
00149      12  ER-0196             PIC  X(4)       VALUE '0196'.        EL634
00150      12  ER-0197             PIC  X(4)       VALUE '0197'.        EL634
00151      12  ER-0216             PIC  X(4)       VALUE '0216'.        EL634
00152      12  ER-0231             PIC  X(4)       VALUE '0231'.        EL634
00153      12  ER-2056             PIC  X(4)       VALUE '2056'.        EL634
00154      12  ER-2208             PIC  X(4)       VALUE '2208'.        EL634
00155      12  ER-2209             PIC  X(4)       VALUE '2209'.        EL634
00156      12  ER-2210             PIC  X(4)       VALUE '2210'.        EL634
00157      12  ER-2237             PIC  X(4)       VALUE '2237'.        EL634
00158      12  ER-2238             PIC  X(4)       VALUE '2238'.        EL634
00159      12  ER-2423             PIC  X(4)       VALUE '2423'.        EL634
00160      12  ER-2427             PIC  X(4)       VALUE '2427'.        EL634
00161      12  ER-2540             PIC  X(4)       VALUE '2540'.        EL634
00162      12  ER-2543             PIC  X(4)       VALUE '2543'.        EL634
00163      12  ER-2575             PIC  X(4)       VALUE '2575'.        EL634
00164      12  ER-2576             PIC  X(4)       VALUE '2576'.        EL634
00165      12  ER-2577             PIC  X(4)       VALUE '2577'.        EL634
00166      12  ER-2578             PIC  X(4)       VALUE '2578'.        EL634
00167      12  ER-2579             PIC  X(4)       VALUE '2579'.        EL634
00168      12  ER-2580             PIC  X(4)       VALUE '2580'.        EL634
00169      12  ER-2581             PIC  X(4)       VALUE '2581'.        EL634
00170      12  ER-2582             PIC  X(4)       VALUE '2582'.        EL634
00171      12  ER-2584             PIC  X(4)       VALUE '2584'.        EL634
00172      12  ER-2585             PIC  X(4)       VALUE '2585'.        EL634
00173      12  ER-2586             PIC  X(4)       VALUE '2586'.        EL634
00174      12  ER-2604             PIC  X(4)       VALUE '2604'.           CL**4
00175      12  ER-2605             PIC  X(4)       VALUE '2605'.           CL**4
00176      12  ER-2705             PIC  X(4)       VALUE '2705'.           CL**6
00177  EJECT                                                            EL634
00178                                      COPY ELCDATE.                   CL**4
00179  EJECT                                                            EL634
00180                                      COPY ELCLOGOF.                  CL**4
00181  EJECT                                                            EL634
00182                                      COPY ELCATTR.                   CL**4
00183  EJECT                                                            EL634
00184                                      COPY ELCEMIB.                   CL**4
00185  EJECT                                                            EL634
00186                                      COPY ELCINTF.                   CL**4
00187      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                 EL634
00188          16  PI-EOF-SW               PIC  X.                      EL634
00189              88  PI-FILE-EOF                 VALUE 'Y'.           EL634
00190          16  PI-PREV-MAINT           PIC  X.                      EL634
00191          16  PI-WORK-SEQ-NO          PIC S9(9)          COMP-3.   EL634
00192          16  PI-ERREPY-KEY.                                       EL634
00193              20  PI-REPY-COMP-CD     PIC  X.                      EL634
00194              20  PI-REPY-CARRIER     PIC  X.                      EL634
00195              20  PI-REPY-GROUPING    PIC  X(6).                   EL634
00196              20  PI-REPY-STATE       PIC  X(2).                   EL634
00197              20  PI-REPY-ACCOUNT     PIC  X(10).                  EL634
00198              20  PI-RECORD-SEQUENCE  PIC S9(8)          COMP.     EL634
00199              20  PI-REPY-REC-TYP     PIC  X.                      EL634
00200          16  FILLER                  PIC  X(608).                    CL**7
00201 *                                                                 EL634
00202 *01  JOURNAL-RECORD          COPY ELCJPFX.                        EL634
00203 *                            PIC  X(200).                         EL634
00204  EJECT                                                            EL634
00205                              COPY ELCAID.                            CL**4
00206                                                                   EL634
00207  01  FILLER REDEFINES DFHAID.                                     EL634
00208      12  FILLER              PIC  X(8).                           EL634
00209      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.      EL634
00210  EJECT                                                            EL634
00211                              COPY EL634S.                            CL**4
00212  EJECT                                                            EL634
00213  LINKAGE SECTION.                                                 EL634
00214                                                                   EL634
00215  01  DFHCOMMAREA             PIC  X(1024).                        EL634
00216  EJECT                                                            EL634
00217 *01 PARMLIST         COMP.                                           CL**7
00218 *    12  FILLER              PIC S9(8).                              CL**7
00219 *    12  ELCNTL-POINTER      PIC S9(8).                              CL**7
00220 *    12  ERACCT-POINTER      PIC S9(8).                              CL**7
00221 *    12  ERREPY-POINTER      PIC S9(8).                              CL**7
00222 *    12  ERREIN-POINTER      PIC S9(8).                              CL**7
00223  EJECT                                                            EL634
00224                                      COPY ELCCNTL.                   CL**4
00225  EJECT                                                            EL634
00226                                      COPY ERCACCT.                   CL**4
00227  EJECT                                                            EL634
00228                                      COPY ERCREPY.                   CL**4
00229  EJECT                                                            EL634
00230                                      COPY ERCREIN.                   CL**4
00231  EJECT                                                            EL634
00232  PROCEDURE DIVISION.                                              EL634
00233                                                                   EL634
00234      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL634
00235      MOVE PI-LIFE-OVERRIDE-L6    TO  EMI-LIFE-OVERRIDE-L6.           CL**4
00236      MOVE PI-AH-OVERRIDE-L6      TO  EMI-AH-OVERRIDE-L6.             CL**4
00237                                                                   EL634
00238      IF EIBCALEN = ZERO                                           EL634
00239          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL634
00240                                                                   EL634
00241      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL634
00242      MOVE '5'                    TO  DC-OPTION-CODE.              EL634
00243                                                                   EL634
00244      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.                  EL634
00245                                                                   EL634
00246      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.           EL634
00247      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.               EL634
00248      MOVE 3                      TO  EMI-NUMBER-OF-LINES.         EL634
00249      MOVE 2                      TO  EMI-SWITCH2.                 EL634
00250                                                                   EL634
00251      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL634
00252          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL634
00253              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL634
00254              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL634
00255              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL634
00256              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL634
00257              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL634
00258              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL634
00259              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL634
00260              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM    EL634
00261          ELSE                                                     EL634
00262              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL634
00263              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL634
00264              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL634
00265              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL634
00266              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL634
00267              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL634
00268              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL634
00269              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.   EL634
00270  EJECT                                                            EL634
00271      MOVE LOW-VALUES             TO  EL634AI.                     EL634
00272                                                                   EL634
00273      IF EIBTRNID NOT = TRANS-ID                                   EL634
00274          GO TO 8100-SEND-INITIAL-MAP.                             EL634
00275                                                                   EL634
00276      COMPUTE PI-WORK-SEQ-NO  =  EIBTIME  *  10.                   EL634
00277                                                                   EL634
00278      MOVE PI-COMPANY-CD          TO  REPY-COMPANY-CD              EL634
00279                                      PI-REPY-COMP-CD.             EL634
00280                                                                   EL634
00281      EXEC CICS HANDLE CONDITION                                   EL634
00282          PGMIDERR  (9600-PGMID-ERROR)                             EL634
00283          ERROR     (9999-ABEND)                                   EL634
00284      END-EXEC.                                                    EL634
00285                                                                   EL634
00286      IF EIBAID = DFHCLEAR                                         EL634
00287          GO TO 9400-CLEAR.                                        EL634
00288                                                                   EL634
00289      IF PI-PROCESSOR-ID = 'LGXX'                                  EL634
00290          GO TO 0200-RECEIVE.                                      EL634
00291                                                                   EL634
00292      EXEC CICS READQ TS                                           EL634
00293          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       EL634
00294          INTO   (SECURITY-CONTROL)                                EL634
00295          LENGTH (SC-COMM-LENGTH)                                  EL634
00296          ITEM   (SC-ITEM)                                         EL634
00297      END-EXEC.                                                    EL634
00298                                                                   EL634
00299      MOVE SC-CREDIT-DISPLAY (16)  TO PI-DISPLAY-CAP.              EL634
00300      MOVE SC-CREDIT-UPDATE  (16)  TO PI-MODIFY-CAP.               EL634
00301                                                                   EL634
00302      IF NOT DISPLAY-CAP                                           EL634
00303          MOVE 'READ'          TO SM-READ                          EL634
00304          PERFORM 9995-SECURITY-VIOLATION                          EL634
00305          MOVE ER-0070         TO  EMI-ERROR                       EL634
00306          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL634
00307          GO TO 8100-SEND-INITIAL-MAP.                             EL634
00308                                                                   EL634
00309  EJECT                                                            EL634
00310  0200-RECEIVE.                                                    EL634
00311      IF EIBAID = DFHPA1 OR  DFHPA2  OR  DFHPA3                    EL634
00312          MOVE ER-0008            TO  EMI-ERROR                    EL634
00313          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL634
00314          MOVE -1                 TO  PFENTERL                     EL634
00315          GO TO 8200-SEND-DATAONLY.                                EL634
00316                                                                   EL634
00317      EXEC CICS RECEIVE                                            EL634
00318          MAP     (EL634A)                                         EL634
00319          MAPSET  (MAPSET-NAME)                                    EL634
00320          INTO    (EL634AI)                                        EL634
00321      END-EXEC.                                                    EL634
00322                                                                   EL634
00323      IF PFENTERL GREATER ZERO                                     EL634
00324          IF EIBAID NOT = DFHENTER                                 EL634
00325              MOVE ER-0004                   TO  EMI-ERROR         EL634
00326              MOVE AL-UNBOF                  TO  PFENTERA          EL634
00327              MOVE -1                        TO  PFENTERL          EL634
00328              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL634
00329              GO TO 8200-SEND-DATAONLY                             EL634
00330          ELSE                                                     EL634
00331              IF PFENTERI NUMERIC                                  EL634
00332                AND PFENTERI GREATER ZERO                          EL634
00333                AND PFENTERI LESS 25                               EL634
00334                  MOVE PF-VALUES (PFENTERI)  TO  EIBAID            EL634
00335              ELSE                                                 EL634
00336                  MOVE ER-0029               TO  EMI-ERROR         EL634
00337                  MOVE AL-UNBOF              TO  PFENTERA          EL634
00338                  MOVE -1                    TO  PFENTERL          EL634
00339                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL634
00340                  GO TO 8200-SEND-DATAONLY.                        EL634
00341  EJECT                                                            EL634
00342  0300-CHECK-PFKEYS.                                               EL634
00343      IF EIBAID = DFHPF23                                          EL634
00344          GO TO 8900-PF23.                                         EL634
00345                                                                   EL634
00346      IF EIBAID = DFHPF24                                          EL634
00347          GO TO 9200-RETURN-MAIN-MENU.                             EL634
00348                                                                   EL634
00349      IF EIBAID = DFHPF12                                          EL634
00350          GO TO 9500-PF12.                                         EL634
00351                                                                   EL634
00352      IF EIBAID = DFHPF1              OR  DFHPF2                   EL634
00353          GO TO 5000-BROWSE-FILE.                                  EL634
00354                                                                   EL634
00355      IF EIBAID = DFHENTER                                         EL634
00356          GO TO 1000-EDIT-DATA.                                    EL634
00357                                                                   EL634
00358      MOVE ER-0029                TO  EMI-ERROR.                   EL634
00359                                                                   EL634
00360  0310-INPUT-ERROR.                                                EL634
00361      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
00362                                                                   EL634
00363      MOVE -1                     TO  PFENTERL                     EL634
00364                                                                   EL634
00365      GO TO 8200-SEND-DATAONLY.                                    EL634
00366  EJECT                                                            EL634
00367  1000-EDIT-DATA.                                                  EL634
00368      IF MAINTI = 'S' OR  'C'  OR  'A'  OR  'D'                    EL634
00369          MOVE AL-UANON           TO  MAINTA                       EL634
00370          IF (MAINTI = 'C' OR  'D')                                EL634
00371              IF PI-PREV-MAINT = 'S'                               EL634
00372                  NEXT SENTENCE                                    EL634
00373              ELSE                                                 EL634
00374                  MOVE ER-2056    TO  EMI-ERROR                    EL634
00375                  MOVE -1         TO  MAINTL                       EL634
00376                  MOVE AL-UABON   TO  MAINTA                       EL634
00377                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL634
00378                  GO TO 1010-EDIT-COMPLETE                         EL634
00379          ELSE                                                     EL634
00380              NEXT SENTENCE                                        EL634
00381      ELSE                                                         EL634
00382          MOVE ER-0023            TO  EMI-ERROR                    EL634
00383          MOVE -1                 TO  MAINTL                       EL634
00384          MOVE AL-UABON           TO  MAINTA                       EL634
00385          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL634
00386                                                                   EL634
00387      IF MODIFY-CAP                                                EL634
00388          NEXT SENTENCE                                            EL634
00389        ELSE                                                       EL634
00390      IF MAINTI NOT = 'S'                                          EL634
00391          MOVE 'UPDATE'       TO SM-READ                           EL634
00392          PERFORM 9995-SECURITY-VIOLATION                          EL634
00393          MOVE ER-0070        TO EMI-ERROR                         EL634
00394          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL634
00395          GO TO 8100-SEND-INITIAL-MAP.                             EL634
00396                                                                   EL634
00397      IF CARRIERI NOT = LOW-VALUES                                 EL634
00398          MOVE AL-UANON           TO  CARRIERA                     EL634
00399          PERFORM 1200-VERIFY-CARRIER-ID  THRU  1299-EXIT          EL634
00400          MOVE CARRIERI           TO  PI-REPY-CARRIER              EL634
00401      ELSE                                                         EL634
00402          IF NOT  ST-ACCNT-CNTL  AND  NOT  ACCNT-CNTL              EL634
00403              MOVE -1             TO  CARRIERL                     EL634
00404              MOVE AL-UABON       TO  CARRIERA                     EL634
00405              MOVE ER-0194        TO  EMI-ERROR                    EL634
00406              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL634
00407                                                                   EL634
00408      IF  GROUPI NOT = LOW-VALUES                                  EL634
00409          MOVE AL-UANON           TO  GROUPA                       EL634
00410          MOVE GROUPI             TO  PI-REPY-GROUPING             EL634
00411      ELSE                                                         EL634
00412          IF CARR-GROUP-ST-ACCNT-CNTL                              EL634
00413              MOVE -1             TO  GROUPL                       EL634
00414              MOVE AL-UABON       TO  GROUPA                       EL634
00415              MOVE ER-0195        TO  EMI-ERROR                    EL634
00416              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL634
00417                                                                   EL634
00418      IF STATEI NOT = LOW-VALUES                                   EL634
00419          MOVE AL-UANON           TO  STATEA                       EL634
00420          PERFORM 1300-VERIFY-STATE-ID  THRU  1399-EXIT            EL634
00421          MOVE STATEI             TO  PI-REPY-STATE                EL634
00422      ELSE                                                         EL634
00423          IF NOT  ACCNT-CNTL  AND  NOT  CARR-ACCNT-CNTL            EL634
00424              MOVE -1             TO  STATEL                       EL634
00425              MOVE AL-UABON       TO  STATEA                       EL634
00426              MOVE ER-0196        TO  EMI-ERROR                    EL634
00427              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL634
00428                                                                   EL634
00429      IF ACCTI NOT = LOW-VALUES                                    EL634
00430          MOVE AL-UANON           TO  ACCTA                        EL634
00431          PERFORM 1400-VERIFY-ACCOUNT  THRU  1499-EXIT             EL634
00432          MOVE ACCTI              TO  PI-REPY-ACCOUNT              EL634
00433      ELSE                                                         EL634
00434          MOVE -1                 TO  ACCTL                        EL634
00435          MOVE AL-UABON           TO  ACCTA                        EL634
00436          MOVE ER-0197            TO  EMI-ERROR                    EL634
00437          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL634
00438                                                                   EL634
00439      IF EMI-ERROR = ZEROS                                         EL634
00440          PERFORM 1600-VALID-EFF-DATE  THRU  1699-EXIT.            EL634
00441                                                                   EL634
00442      IF MAINTI = 'S' OR  'D'                                      EL634
00443          GO TO 1010-EDIT-COMPLETE.                                EL634
00444                                                                   EL634
00445      IF RCOMPI NOT = LOW-VALUES                                   EL634
00446          MOVE AL-UANON           TO  RCOMPA                       EL634
00447          PERFORM 1100-VERIFY-REIN-COMP  THRU  1199-EXIT.          EL634
00448                                                                   EL634
00449      IF BENCDI NOT = LOW-VALUES                                   EL634
00450          MOVE AL-UANON           TO  BENCDA                       EL634
00451      ELSE                                                         EL634
00452          MOVE ER-0026            TO  EMI-ERROR                    EL634
00453          MOVE -1                 TO  BENCDL                       EL634
00454          MOVE AL-UABON           TO  BENCDA                       EL634
00455          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL634
00456                                                                   EL634
00457      IF BENTYPI NOT = LOW-VALUES                                  EL634
00458          IF BENTYPI = PI-AH-OVERRIDE-L1                           EL634
00459            OR  BENTYPI = PI-LIFE-OVERRIDE-L1                      EL634
00460              MOVE AL-UANON       TO  BENTYPA                      EL634
00461          ELSE                                                     EL634
00462              MOVE ER-2580        TO  EMI-ERROR                    EL634
00463              MOVE -1             TO  BENTYPL                      EL634
00464              MOVE AL-UABON       TO  BENTYPA                      EL634
00465              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL634
00466      ELSE                                                         EL634
00467          MOVE ER-2575            TO  EMI-ERROR                    EL634
00468          MOVE -1                 TO  BENTYPL                      EL634
00469          MOVE AL-UABON           TO  BENTYPA                      EL634
00470          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL634
00471                                                                   EL634
00472      IF BENCDI NOT = LOW-VALUES                                   EL634
00473        AND  BENTYPI = PI-AH-OVERRIDE-L1                           EL634
00474            OR  BENTYPI = PI-LIFE-OVERRIDE-L1                      EL634
00475          PERFORM 1500-VERIFY-CODES  THRU  1599-EXIT.              EL634
00476  EJECT                                                            EL634
00477      IF MTHYRI = LOW-VALUES                                       EL634
00478          MOVE ER-2581               TO  EMI-ERROR                 EL634
00479          MOVE -1                    TO  MTHYRL                    EL634
00480          MOVE AL-UNBON              TO  MTHYRA                    EL634
00481          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL634
00482      ELSE                                                         EL634
00483          IF MTHYRI NUMERIC                                        EL634
00484              IF MTHI GREATER ZERO                                 EL634
00485                AND LESS 13                                        EL634
CIDMOD*                IF YRI GREATER ZEROS                             EL634
CIDMOD                 IF YRI NOT < ZEROS                               EL634
00487                      MOVE AL-UNNON  TO  MTHYRA                    EL634
00488                  ELSE                                             EL634
00489                      MOVE ER-2578   TO  EMI-ERROR                 EL634
00490                      MOVE -1        TO  MTHYRL                    EL634
00491                      MOVE AL-UNBON  TO  MTHYRA                    EL634
00492                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT   EL634
00493              ELSE                                                 EL634
00494                  MOVE ER-2576       TO  EMI-ERROR                 EL634
00495                  MOVE -1            TO  MTHYRL                    EL634
00496                  MOVE AL-UNBON      TO  MTHYRA                    EL634
00497                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL634
00498          ELSE                                                     EL634
00499              MOVE ER-2577           TO  EMI-ERROR                 EL634
00500              MOVE -1                TO  MTHYRL                    EL634
00501              MOVE AL-UNBON          TO  MTHYRA                    EL634
00502              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL634
00503                                                                   EL634
00504      MOVE PI-CR-MONTH-END-DT        TO DC-BIN-DATE-1.             EL634
00505      MOVE ' '                       TO DC-OPTION-CODE.            EL634
00506                                                                   EL634
00507      PERFORM 8500-DATE-CONVERT     THRU   8599-EXIT.              EL634
00508                                                                   EL634
00509      MOVE DC-GREG-DATE-1-MDY         TO  WS-PI-END-DATE.          EL634
00510                                                                   EL634
00511      MOVE WS-PI-END-MO               TO  WS-END-MO1.              EL634
00512      MOVE WS-PI-END-YR               TO  WS-END-YR1.              EL634
00513      MOVE MTHI                       TO  WS-END-MO2.              EL634
00514      MOVE YRI                        TO  WS-END-YR2.              EL634

           IF WS-END-YR1 > 80
              MOVE 19                  TO WS-END-CENT1
           END-IF

           IF WS-END-YR2 > 80
              MOVE 19                  TO WS-END-CENT2
           END-IF

00516      IF WS-END-DATE1 < WS-END-DATE2
00517          MOVE ER-2582                TO  EMI-ERROR                EL634
00518          MOVE -1                     TO  MTHYRL                   EL634
00519          MOVE AL-UNBON               TO  MTHYRA                   EL634
00520          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL634
00521                                                                      CL**2
00522      IF EOMDTL IS NOT EQUAL TO ZEROS                                 CL**2
00523          MOVE AL-UANON           TO  EOMDTA                          CL**2
00524          MOVE EOMDTI             TO  WS-DEEDIT-DATE                  CL**2
00525          PERFORM 8700-DEEDIT-DATE THRU 8799-EXIT                     CL**2
00526          MOVE WS-DEEDIT-DATE     TO  EOMDTI.                         CL**2
00527                                                                   EL634
00528 *    IF INFORCEL NOT = ZEROS                                      EL634
00529 *        MOVE AL-UNNON           TO  INFORCEA                     EL634
CIDMOD*        MOVE INFORCEI           TO  WS-DEEDIT-FIELD                   000
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT                          000
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-INFORCEI.                      000
00535                                                                   EL634
00536 *    IF MORTAMTL NOT = ZEROS                                      EL634
00537 *        MOVE AL-UNNON           TO  MORTAMTA                     EL634
CIDMOD*        MOVE MORTAMTI           TO  WS-DEEDIT-FIELD                   000
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT                          000
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-MORTAMTI.                      000
00543                                                                   EL634
00544 *    IF FUTUREL NOT = ZEROS                                       EL634
00545 *        MOVE AL-UNNON           TO  FUTUREA                      EL634
CIDMOD*        MOVE FUTUREI            TO  WS-DEEDIT-FIELD                   000
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT                          000
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-FUTUREI.                       000
00551                                                                   EL634
00552 *    IF PTCL NOT = ZEROS                                          EL634
00553 *        MOVE AL-UNNON           TO  PTCA                         EL634
CIDMOD*        MOVE PTCI               TO  WS-DEEDIT-FIELD                   000
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT                          000
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-PTCI.                          000
00559                                                                   EL634
00560 *    IF IBNRL NOT = ZEROS                                         EL634
00561 *        MOVE AL-UNNON           TO  IBNRA                        EL634
CIDMOD*        MOVE IBNRI              TO  WS-DEEDIT-FIELD                   000
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT                          000
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-IBNRI.                         000
00567                                                                   EL634
00568 *    IF CLAIML NOT = ZEROS                                        EL634
00569 *        MOVE AL-UNNON           TO  CLAIMA                       EL634
CIDMOD*        MOVE CLAIMI             TO  WS-DEEDIT-FIELD                   000
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT                          000
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-CLAIMI.                        000
00575                                                                   EL634
00576 *    IF EXPL NOT = ZEROS                                          EL634
00577 *        MOVE AL-UNNON           TO  EXPA                         EL634
CIDMOD*        MOVE EXPI               TO  WS-DEEDIT-FIELD                   000
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT                          000
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-EXPI.                          000
00583                                                                   EL634
00528      IF INFORCEL NOT = ZEROS                                      EL634
00529          MOVE AL-UNNON           TO  INFORCEA                     EL634
00530          EXEC CICS BIF DEEDIT                                        CL**8
00531              FIELD   (INFORCEI)                                      CL**8
00532              LENGTH  (11)                                            CL**8
00533          END-EXEC                                                    CL**8
00534          MOVE INFORCEI           TO  WS-INFORCEI.                    CL**8
00535                                                                   EL634
00536      IF MORTAMTL NOT = ZEROS                                      EL634
00537          MOVE AL-UNNON           TO  MORTAMTA                     EL634
00538          EXEC CICS BIF DEEDIT                                        CL**8
00539              FIELD   (MORTAMTI)                                      CL**8
00540              LENGTH  (11)                                            CL**8
00541          END-EXEC                                                    CL**8
00542          MOVE MORTAMTI           TO  WS-MORTAMTI.                    CL**8
00543                                                                   EL634
00544      IF FUTUREL NOT = ZEROS                                       EL634
00545          MOVE AL-UNNON           TO  FUTUREA                      EL634
00546          EXEC CICS BIF DEEDIT                                        CL**8
00547              FIELD   (FUTUREI)                                       CL**8
00548              LENGTH  (11)                                            CL**8
00549          END-EXEC                                                    CL**8
00550          MOVE FUTUREI            TO  WS-FUTUREI.                     CL**8
00551                                                                   EL634
00552      IF PTCL NOT = ZEROS                                          EL634
00553          MOVE AL-UNNON           TO  PTCA                         EL634
00554          EXEC CICS BIF DEEDIT                                        CL**8
00555              FIELD   (PTCI)                                          CL**8
00556              LENGTH  (11)                                            CL**8
00557          END-EXEC                                                    CL**8
00558          MOVE PTCI               TO  WS-PTCI.                        CL**8
00559                                                                   EL634
00560      IF IBNRL NOT = ZEROS                                         EL634
00561          MOVE AL-UNNON           TO  IBNRA                        EL634
00562          EXEC CICS BIF DEEDIT                                        CL**8
00563              FIELD   (IBNRI)                                         CL**8
00564              LENGTH  (11)                                            CL**8
00565          END-EXEC                                                    CL**8
00566          MOVE IBNRI              TO  WS-IBNRI.                       CL**8
00567                                                                   EL634
00568      IF CLAIML NOT = ZEROS                                        EL634
00569          MOVE AL-UNNON           TO  CLAIMA                       EL634
00570          EXEC CICS BIF DEEDIT                                        CL**8
00571              FIELD   (CLAIMI)                                        CL**8
00572              LENGTH  (11)                                            CL**8
00573          END-EXEC                                                    CL**8
00574          MOVE CLAIMI             TO  WS-CLAIMI.                      CL**8
00575                                                                   EL634
00576      IF EXPL NOT = ZEROS                                          EL634
00577          MOVE AL-UNNON           TO  EXPA                         EL634
00578          EXEC CICS BIF DEEDIT                                        CL**8
00579              FIELD   (EXPI)                                          CL**8
00580              LENGTH  (11)                                            CL**8
00581          END-EXEC                                                    CL**8
00582          MOVE EXPI               TO  WS-EXPI.                        CL**8
00583                                                                   EL634
00584      IF PYMNTL NOT = ZEROS                                        EL634
00585          MOVE AL-UNNON           TO  PYMNTA                       EL634
00586          EXEC CICS BIF DEEDIT                                        CL**8
00587              FIELD   (PYMNTI)                                        CL**8
00588              LENGTH  (11)                                            CL**8
00589          END-EXEC                                                    CL**8
00590          MOVE PYMNTI             TO  WS-PYMNTI.                      CL**8
00584 *    IF PYMNTL NOT = ZEROS                                        EL634
00585 *        MOVE AL-UNNON           TO  PYMNTA                       EL634
CIDMOD*        MOVE PYMNTI             TO  WS-DEEDIT-FIELD                   000
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT                          000
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-PYMNTI.                        000
00591  EJECT                                                            EL634
00592      IF OCOMML NOT = ZEROS                                        EL634
00593          MOVE AL-UNNON           TO  OCOMMA                       EL634
00594          EXEC CICS BIF DEEDIT                                        CL**8
00595              FIELD   (OCOMMI)                                        CL**8
00596              LENGTH  (11)                                            CL**8
00597          END-EXEC                                                    CL**8
00598          MOVE OCOMMI             TO  WS-OCOMMI.                      CL**8
00599                                                                   EL634
00592 *    IF OCOMML NOT = ZEROS                                        EL634
00593 *        MOVE AL-UNNON           TO  OCOMMA                       EL634
CIDMOD*        MOVE OCOMMI             TO  WS-DEEDIT-FIELD                   000
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT                          000
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-OCOMMI.                        000
00599                                                                   EL634
00600      IF RPREML NOT = ZEROS                                        EL634
00601          MOVE AL-UNNON           TO  RPREMA                       EL634
00602          EXEC CICS BIF DEEDIT                                        CL**8
00603              FIELD   (RPREMI)                                        CL**8
00604              LENGTH  (11)                                            CL**8
00605          END-EXEC                                                    CL**8
00606          MOVE RPREMI             TO  WS-RPREMI.                      CL**8
00607                                                                   EL634
00600 *    IF RPREML NOT = ZEROS                                        EL634
00601 *        MOVE AL-UNNON           TO  RPREMA                       EL634
CIDMOD*        MOVE RPREMI             TO  WS-DEEDIT-FIELD                   000
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT                          000
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-RPREMI.                        000
CIDMOD*                                                                 EL634
00608      IF MAINTI = 'A'                                              EL634
00609          NEXT SENTENCE                                            EL634
00610      ELSE                                                         EL634
00611          GO TO 1010-EDIT-COMPLETE.                                EL634
00612                                                                   EL634
00613      IF INFORCEL = ZEROS                                          EL634
00614        AND  MORTAMTL = ZEROS                                      EL634
00615        AND  FUTUREL = ZEROS                                       EL634
00616        AND  PTCL = ZEROS                                          EL634
00617        AND  IBNRL = ZEROS                                         EL634
00618        AND  CLAIML = ZEROS                                        EL634
00619        AND  EXPL = ZEROS                                          EL634
00620        AND  PYMNTL = ZEROS                                        EL634
00621        AND  OCOMML = ZEROS                                        EL634
00622        AND  RPREML = ZEROS                                        EL634
00623          MOVE ER-2585            TO  EMI-ERROR                    EL634
00624          MOVE -1                 TO  INFORCEL                     EL634
00625          MOVE AL-UNBON           TO  INFORCEA                     EL634
00626                                      MORTAMTA                     EL634
00627                                      FUTUREA                      EL634
00628                                      PTCA                         EL634
00629                                      IBNRA                        EL634
00630                                      CLAIMA                       EL634
00631                                      EXPA                         EL634
00632                                      PYMNTA                       EL634
00633                                      OCOMMA                       EL634
00634                                      RPREMA                       EL634
00635          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL634
00636                                                                   EL634
00637      IF EXPL NOT EQUAL TO ZEROS                                   EL634
00638          IF RCOMPI NOT EQUAL TO LOW-VALUES                        EL634
00639              MOVE ER-2543        TO  EMI-ERROR                    EL634
00640              MOVE -1             TO  EXPL                         EL634
00641              MOVE AL-UNBON       TO  EXPA                         EL634
00642              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL634
00643                                                                   EL634
00644      IF OCOMML NOT EQUAL TO ZEROS                                 EL634
00645          IF RCOMPI NOT EQUAL TO LOW-VALUES                        EL634
00646              MOVE ER-2543        TO  EMI-ERROR                    EL634
00647              MOVE -1             TO  OCOMML                       EL634
00648              MOVE AL-UNBON       TO  OCOMMA                       EL634
00649              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL634
00650                                                                   EL634
00651      IF RPREML NOT EQUAL TO ZEROS                                 EL634
00652          IF RCOMPI  EQUAL TO LOW-VALUES                           EL634
00653              MOVE ER-2540        TO  EMI-ERROR                    EL634
00654              MOVE -1             TO  RPREML                       EL634
00655              MOVE AL-UNBON       TO  RPREMA                       EL634
00656              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL634
00657                                                                   EL634
00658  1010-EDIT-COMPLETE.                                              EL634
00659      IF EMI-ERROR NOT = ZEROS                                     EL634
00660          GO TO 8200-SEND-DATAONLY.                                EL634
00661                                                                   EL634
00662      MOVE MAINTI                 TO  PI-PREV-MAINT.               EL634
00663                                                                   EL634
00664      IF MAINTI = 'A'                                              EL634
00665          GO TO 2000-ADD-RECORD.                                   EL634
00666                                                                   EL634
00667      IF MAINTI = 'C'                                              EL634
00668          GO TO 3000-CHANGE-RECORD.                                EL634
00669                                                                   EL634
00670      IF MAINTI = 'D'                                              EL634
00671          GO TO 4000-DELETE-RECORD.                                EL634
00672                                                                   EL634
00673      IF MAINTI = 'S'                                              EL634
00674          GO TO 5000-BROWSE-FILE.                                  EL634
00675  EJECT                                                            EL634
00676  1100-VERIFY-REIN-COMP.                                           EL634
00677      EXEC CICS HANDLE CONDITION                                   EL634
00678          NOTFND  (1110-NO-REIN-REC)                               EL634
00679          END-EXEC.                                                EL634
00680                                                                   EL634
00681      MOVE PI-COMPANY-CD          TO  REIN-COMPANY-CD.             EL634
00682      MOVE 'B'                    TO  REIN-CODE.                   EL634
00683      MOVE RCOMPI                 TO  REIN-TABLE.                  EL634
00684                                                                   EL634
00685      EXEC CICS READ                                               EL634
00686          DATASET  (ERREIN-FILE-ID)                                EL634
00687          SET      (ADDRESS OF REINSURANCE-RECORD)                    CL**7
00688          RIDFLD   (ERREIN-KEY)                                    EL634
00689      END-EXEC.                                                    EL634
00690                                                                   EL634
00691      GO TO 1199-EXIT.                                             EL634
00692                                                                   EL634
00693  1110-NO-REIN-REC.                                                EL634
00694      MOVE ER-2579                TO  EMI-ERROR.                   EL634
00695      MOVE -1                     TO  RCOMPL.                      EL634
00696      MOVE AL-UABON               TO  RCOMPA.                      EL634
00697                                                                   EL634
00698      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
00699                                                                   EL634
00700  1199-EXIT.                                                       EL634
00701      EXIT.                                                        EL634
00702  EJECT                                                            EL634
00703  1200-VERIFY-CARRIER-ID.                                          EL634
00704      MOVE SPACES                 TO  ELCNTL-KEY.                  EL634
00705      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL634
00706      MOVE '6'                    TO  CNTL-REC-TYPE.               EL634
00707      MOVE CARRIERI               TO  CNTL-CARRIER.                EL634
00708      MOVE +0                     TO  CNTL-SEQ.                    EL634
00709                                                                   EL634
00710      EXEC CICS HANDLE CONDITION                                   EL634
00711          NOTFND  (1210-NO-CARRIER)                                EL634
00712      END-EXEC.                                                    EL634
00713                                                                   EL634
00714      EXEC CICS READ                                               EL634
00715          DATASET  (ELCNTL-FILE-ID)                                EL634
00716          SET      (ADDRESS OF CONTROL-FILE)                          CL**7
00717          RIDFLD   (ELCNTL-KEY)                                    EL634
00718      END-EXEC.                                                    EL634
00719                                                                   EL634
00720      GO TO 1299-EXIT.                                             EL634
00721                                                                   EL634
00722  1210-NO-CARRIER.                                                 EL634
00723      MOVE ER-2208                TO  EMI-ERROR.                   EL634
00724      MOVE -1                     TO  CARRIERL.                    EL634
00725      MOVE AL-UABON               TO  CARRIERA.                    EL634
00726                                                                   EL634
00727      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
00728                                                                   EL634
00729  1299-EXIT.                                                       EL634
00730      EXIT.                                                        EL634
00731  EJECT                                                            EL634
00732  1300-VERIFY-STATE-ID.                                            EL634
00733      MOVE SPACES                 TO  ELCNTL-KEY.                  EL634
00734      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL634
00735      MOVE '3'                    TO  CNTL-REC-TYPE.               EL634
00736      MOVE STATEI                 TO  CNTL-STATE.                  EL634
00737      MOVE +0                     TO  CNTL-SEQ.                    EL634
00738                                                                   EL634
00739      EXEC CICS HANDLE CONDITION                                   EL634
00740          NOTFND  (1310-NO-STATE)                                  EL634
00741      END-EXEC.                                                    EL634
00742                                                                   EL634
00743      EXEC CICS READ                                               EL634
00744          DATASET  (ELCNTL-FILE-ID)                                EL634
00745          SET      (ADDRESS OF CONTROL-FILE)                          CL**7
00746          RIDFLD   (ELCNTL-KEY)                                    EL634
00747      END-EXEC.                                                    EL634
00748                                                                   EL634
00749      GO TO 1399-EXIT.                                             EL634
00750                                                                   EL634
00751  1310-NO-STATE.                                                   EL634
00752      MOVE ER-2209                TO  EMI-ERROR.                   EL634
00753      MOVE -1                     TO  STATEL.                      EL634
00754      MOVE AL-UABON               TO  STATEA.                      EL634
00755                                                                   EL634
00756      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
00757                                                                   EL634
00758  1399-EXIT.                                                       EL634
00759      EXIT.                                                        EL634
00760  EJECT                                                            EL634
00761  1400-VERIFY-ACCOUNT.                                             EL634
00762      IF CARRIERL GREATER ZEROS                                    EL634
00763          MOVE CARRIERI           TO  ACCT-CARRIER                 EL634
00764      ELSE                                                         EL634
00765          MOVE SPACES             TO  ACCT-CARRIER.                EL634
00766                                                                   EL634
00767      IF GROUPL GREATER ZEROS                                      EL634
00768          MOVE GROUPI             TO  ACCT-GROUPING                EL634
00769      ELSE                                                         EL634
00770          MOVE SPACES             TO  ACCT-GROUPING.               EL634
00771                                                                   EL634
00772      IF STATEL GREATER ZEROS                                      EL634
00773          MOVE STATEI             TO  ACCT-STATE                   EL634
00774      ELSE                                                         EL634
00775          MOVE SPACES             TO  ACCT-STATE.                  EL634
00776                                                                   EL634
00777      MOVE ACCTI                  TO  ACCT-ACCOUNT.                EL634
00778      MOVE PI-COMPANY-CD          TO  ACCT-CO.                     EL634
00779      MOVE LOW-VALUES             TO  ACCT-EXP-DATE.               EL634
00780                                                                   EL634
00781      EXEC CICS HANDLE CONDITION                                   EL634
00782          NOTFND  (1410-ACCOUNT-INVALID)                           EL634
00783          END-EXEC.                                                EL634
00784                                                                   EL634
00785      EXEC CICS READ                                               EL634
00786          DATASET  (ERACCT-ALT-FILE-ID)                            EL634
00787          SET      (ADDRESS OF ACCOUNT-MASTER)                        CL**7
00788          RIDFLD   (ERACCT-KEY)                                    EL634
00789          GTEQ                                                     EL634
00790      END-EXEC.                                                    EL634
00791                                                                   EL634
00792      MOVE AM-CONTROL-BY-VAR-GRP  TO  ERACCT-SAVE-KEY-22.             CL**3
00793                                                                   EL634
00794      IF ERACCT-COMP-KEY NOT = ERACCT-SAVE-KEY                     EL634
00795          GO TO 1410-ACCOUNT-INVALID.                              EL634
00796                                                                   EL634
00797      GO TO 1499-EXIT.                                             EL634
00798                                                                   EL634
00799  1410-ACCOUNT-INVALID.                                            EL634
00800      PERFORM 6000-SET-VG-CONTROL  THRU  6099-EXIT.                EL634
00801                                                                   EL634
00802      MOVE ER-2210                TO  EMI-ERROR.                   EL634
00803                                                                   EL634
00804      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
00805                                                                   EL634
00806  1499-EXIT.                                                       EL634
00807      EXIT.                                                        EL634
00808  EJECT                                                            EL634
00809  1500-VERIFY-CODES.                                               EL634
00810      MOVE SPACES                 TO  ELCNTL-KEY.                  EL634
00811      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL634
00812                                                                      CL**5
00813      IF BENTYPI = PI-LIFE-OVERRIDE-L1                                CL**5
00814         MOVE 'L'                    TO  CNTL-REC-TYPE                CL**9
00815         ELSE                                                         CL**5
00816         MOVE 'A'                    TO  CNTL-REC-TYPE.               CL**5
00817                                                                      CL**5
00818      MOVE +0                     TO  CNTL-SEQ.                    EL634
00819                                                                   EL634
00820      EXEC CICS HANDLE CONDITION                                   EL634
00821          NOTFND  (1530-NO-RECORD)                                    CL**4
00822      END-EXEC.                                                    EL634
00823                                                                   EL634
00824      EXEC CICS READ                                               EL634
00825          DATASET  (ELCNTL-FILE-ID)                                EL634
00826          SET      (ADDRESS OF CONTROL-FILE)                          CL**7
00827          RIDFLD   (ELCNTL-KEY)                                    EL634
00828      END-EXEC.                                                    EL634
00829                                                                   EL634
00830      MOVE BENCDI                TO  WS-EDITED-BEN-CD.                CL**4
00831                                                                      CL**4
00832      MOVE +1                     TO  EDIT-SUB.                    EL634
00833                                                                   EL634
00834      IF BENTYPI = PI-AH-OVERRIDE-L1                               EL634
00835          GO TO 1520-AH-SEARCH-LOOP.                               EL634
00836                                                                   EL634
00837  1510-LIFE-SEARCH-LOOP.                                           EL634
00838      IF CF-LIFE-CODE-OUT (EDIT-SUB) = ZEROS                       EL634
00839          GO TO 1540-LF-BEN-CNTL-SEARCH.                              CL**4
00840                                                                   EL634
00841      IF BENCDI = CF-LIFE-CODE-IN (EDIT-SUB)                       EL634
00842          MOVE CF-LIFE-CODE-OUT (EDIT-SUB)  TO  WS-EDITED-BEN-CD   EL634
00843          GO TO 1540-LF-BEN-CNTL-SEARCH.                              CL**4
00844                                                                   EL634
00845      ADD 1                       TO  EDIT-SUB.                    EL634
00846                                                                   EL634
00847      IF EDIT-SUB GREATER 120                                      EL634
00848          GO TO 1540-LF-BEN-CNTL-SEARCH.                              CL**4
00849                                                                   EL634
00850      GO TO 1510-LIFE-SEARCH-LOOP.                                 EL634
00851                                                                   EL634
00852  1520-AH-SEARCH-LOOP.                                             EL634
00853      IF CF-AH-CODE-OUT (EDIT-SUB) = ZEROS                         EL634
00854          GO TO 1550-AH-BEN-CNTL-SEARCH.                              CL**4
00855                                                                   EL634
00856      IF BENCDI = CF-AH-CODE-IN (EDIT-SUB)                         EL634
00857          MOVE CF-AH-CODE-OUT (EDIT-SUB)  TO  WS-EDITED-BEN-CD     EL634
00858          GO TO 1550-AH-BEN-CNTL-SEARCH.                              CL**4
00859                                                                   EL634
00860      ADD 1                       TO  EDIT-SUB.                    EL634
00861                                                                   EL634
00862      IF EDIT-SUB GREATER 96                                       EL634
00863          GO TO 1550-AH-BEN-CNTL-SEARCH.                              CL**4
00864                                                                   EL634
00865      GO TO 1520-AH-SEARCH-LOOP.                                   EL634
00866                                                                   EL634
00867                                                                   EL634
00868  1530-NO-RECORD.                                                     CL**4
00869                                                                   EL634
00870      IF BENTYPI = PI-LIFE-OVERRIDE-L1                                CL**4
00871          MOVE ER-2423           TO  EMI-ERROR                        CL**4
00872      ELSE                                                            CL**4
00873          MOVE ER-2427           TO  EMI-ERROR.                       CL**4
00874                                                                      CL**4
00875      MOVE  -1                   TO  BENCDL.                          CL**4
00876      MOVE  AL-UABON             TO  BENCDA.                          CL**4
00877                                                                      CL**4
00878      PERFORM  9900-ERROR-FORMAT  THRU  9900-EXIT.                    CL**4
00879                                                                   EL634
00880      GO TO 1599-EXIT.                                             EL634
00881                                                                   EL634
00882  1540-LF-BEN-CNTL-SEARCH.                                            CL**4
00883                                                                   EL634
00884      MOVE  SPACES               TO  ELCNTL-KEY.                      CL**4
00885      MOVE  PI-COMPANY-ID        TO  CNTL-COMP-ID.                    CL**4
00886      MOVE  '4'                  TO  CNTL-REC-TYPE.                   CL**4
00887      MOVE  +0                   TO  CNTL-SEQ.                        CL**4
00888      MOVE  WS-EDITED-BEN-CD     TO  CNTL-HI-BEN-CD.                  CL**4
00889                                                                   EL634
00890      EXEC CICS READ                                                  CL**4
00891          DATASET   (ELCNTL-FILE-ID)                                  CL**4
00892          SET       (ADDRESS OF CONTROL-FILE)                         CL**7
00893          RIDFLD    (ELCNTL-KEY)                                      CL**4
00894          GTEQ                                                        CL**4
00895      END-EXEC.                                                       CL**4
00896                                                                      CL**4
00897      PERFORM 1560-FIND-BENEFIT  THRU 1560-EXIT.                      CL**4
00898                                                                      CL**4
00899      IF  CF-BENEFIT-CODE (ED-SUB)   NOT =  WS-EDITED-BEN-CD          CL**4
00900          MOVE  ER-2604          TO  EMI-ERROR                        CL**4
00901          MOVE  -1               TO  BENCDL                           CL**4
00902          MOVE  AL-UABON         TO  BENCDA                           CL**4
00903          PERFORM  9900-ERROR-FORMAT  THRU  9900-EXIT.                CL**4
00904                                                                      CL**4
00905      GO TO  1599-EXIT.                                               CL**4
00906                                                                      CL**4
00907  1550-AH-BEN-CNTL-SEARCH.                                            CL**4
00908                                                                      CL**4
00909      MOVE  SPACES               TO  ELCNTL-KEY.                      CL**4
00910      MOVE  PI-COMPANY-ID        TO  CNTL-COMP-ID.                    CL**4
00911      MOVE  '5'                  TO  CNTL-REC-TYPE.                   CL**4
00912      MOVE  +0                   TO  CNTL-SEQ.                        CL**4
00913      MOVE  WS-EDITED-BEN-CD     TO  CNTL-HI-BEN-CD.                  CL**4
00914                                                                      CL**4
00915      EXEC CICS READ                                                  CL**4
00916          DATASET   (ELCNTL-FILE-ID)                                  CL**4
00917          SET       (ADDRESS OF CONTROL-FILE)                         CL**7
00918          RIDFLD    (ELCNTL-KEY)                                      CL**4
00919          GTEQ                                                        CL**4
00920      END-EXEC.                                                       CL**4
00921                                                                      CL**4
00922      PERFORM 1560-FIND-BENEFIT  THRU 1560-EXIT.                      CL**4
00923                                                                      CL**4
00924      IF  CF-BENEFIT-CODE (ED-SUB)  NOT =  WS-EDITED-BEN-CD           CL**4
00925          MOVE  ER-2605          TO  EMI-ERROR                        CL**4
00926          MOVE  -1               TO  BENCDL                           CL**4
00927          MOVE  AL-UABON         TO  BENCDA                           CL**4
00928          PERFORM  9900-ERROR-FORMAT  THRU  9900-EXIT.                CL**4
00929                                                                      CL**4
00930      GO TO  1599-EXIT.                                               CL**4
00931                                                                      CL**4
00932  1560-FIND-BENEFIT.                                                  CL**4
00933                                                                      CL**4
00934      PERFORM  1560-BENEFIT-DUMMY  THRU  1560-DUMMY-EXIT              CL**4
00935         VARYING  ED-SUB  FROM  +1  BY  +1  UNTIL                     CL**4
00936            ((ED-SUB   GREATER THAN  8)  OR                           CL**4
00937                (CF-BENEFIT-NUMERIC (ED-SUB)  =                       CL**4
00938                                     WS-EDITED-BEN-CD)).              CL**4
00939                                                                      CL**4
00940  1560-EXIT.                                                          CL**4
00941                                                                      CL**4
00942  1560-BENEFIT-DUMMY.                                                 CL**4
00943                                                                      CL**4
00944  1560-DUMMY-EXIT.                                                    CL**4
00945      EXIT.                                                           CL**4
00946                                                                   EL634
00947  1599-EXIT.                                                       EL634
00948      EXIT.                                                        EL634
00949  EJECT                                                            EL634
00950  1600-VALID-EFF-DATE.                                             EL634
00951      IF EFFDTI = LOW-VALUES                                       EL634
00952          MOVE WS-CURRENT-DT      TO  EFFDTI.                      EL634
00953                                                                   EL634
00954      MOVE AL-UNNON               TO  EFFDTA.                      EL634
00955      MOVE EFFDTI                 TO  WS-DEEDIT-DATE.              EL634
00956                                                                   EL634
00957      PERFORM 8700-DEEDIT-DATE  THRU  8799-EXIT.                   EL634
00958                                                                   EL634
00959      MOVE WS-DEEDIT-DATE-6       TO  DC-GREG-DATE-1-MDY.          EL634
00960      MOVE '4'                    TO  DC-OPTION-CODE.              EL634
00961                                                                   EL634
00962      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.                  EL634
00963                                                                   EL634
00964      IF NO-CONVERSION-ERROR                                       EL634
00965          MOVE DC-BIN-DATE-1      TO  WS-BIN-EFF-DATE-ENTERED      EL634
00966      ELSE                                                         EL634
00967          MOVE ER-0231            TO  EMI-ERROR                    EL634
00968          MOVE -1                 TO  EFFDTL                       EL634
00969          MOVE AL-UNBON           TO  EFFDTA                       EL634
00970          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL634
00971 *        GO TO 1699-EXIT                                          EL634
00972 *    ELSE                                                         EL634
00973 *        MOVE ER-0216            TO  EMI-ERROR                    EL634
00974 *        MOVE -1                 TO  EFFDTL                       EL634
00975 *        MOVE AL-UNBON           TO  EFFDTA                       EL634
00976 *        PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL634
00977          GO TO 1699-EXIT.                                         EL634
00978                                                                   EL634
00979  1610-HANDLE.                                                     EL634
00980      EXEC CICS HANDLE CONDITION                                   EL634
00981          ENDFILE  (1630-NO-MATCH)                                 EL634
00982      END-EXEC.                                                    EL634
00983                                                                   EL634
00984 *    MOVE WS-SV-CARRIER            TO  ACCT-CARRIER.              EL634
00985 *    MOVE WS-SV-GROUPING           TO  ACCT-GROUPING.             EL634
00986 *    MOVE WS-SV-STATE              TO  ACCT-STATE.                EL634
00987 *    MOVE ACCTI                    TO  ACCT-ACCOUNT.              EL634
00988      MOVE WS-BIN-EFF-DATE-ENTERED  TO  ACCT-EXP-DATE.             EL634
00989      MOVE ERACCT-KEY               TO  ERACCT-SAVE-KEY-22.           CL**3
00990                                                                   EL634
00991      EXEC CICS STARTBR                                            EL634
00992          DATASET  (ERACCT-ALT-FILE-ID)                            EL634
00993          RIDFLD   (ERACCT-SAVE-KEY-22)                               CL**3
00994      END-EXEC.                                                    EL634
00995  EJECT                                                            EL634
00996  1620-READ-LOOP.                                                  EL634
00997      EXEC CICS READNEXT                                           EL634
00998          DATASET  (ERACCT-ALT-FILE-ID)                            EL634
00999          SET      (ADDRESS OF ACCOUNT-MASTER)                        CL**7
01000          RIDFLD   (ERACCT-SAVE-KEY-22)                               CL**3
01001      END-EXEC.                                                    EL634
01002                                                                   EL634
01003      IF AM-VG-CARRIER NOT = ACCT-CARRIER                          EL634
01004        OR AM-VG-STATE NOT = ACCT-STATE                            EL634
01005        OR AM-VG-GROUPING NOT = ACCT-GROUPING                      EL634
01006        OR AM-VG-ACCOUNT NOT = ACCT-ACCOUNT                        EL634
01007          GO TO 1630-NO-MATCH.                                     EL634
01008                                                                   EL634
01009      IF WS-BIN-EFF-DATE-ENTERED LESS AM-VG-EXPIRATION-DT          EL634
01010          NEXT SENTENCE                                            EL634
01011      ELSE                                                         EL634
01012          GO TO 1620-READ-LOOP.                                    EL634
01013                                                                   EL634
01014      IF WS-BIN-EFF-DATE-ENTERED LESS AM-EFFECTIVE-DT              EL634
01015          GO TO 1630-NO-MATCH.                                     EL634
01016                                                                   EL634
01017      MOVE AM-CARRIER             TO  WS-SV-CARRIER.               EL634
01018      MOVE AM-GROUPING            TO  WS-SV-GROUPING.              EL634
01019      MOVE AM-STATE               TO  WS-SV-STATE.                 EL634
01020      MOVE AM-EFFECTIVE-DT        TO  WS-EFF-DT-HLD                EL634
01021                                      DC-BIN-DATE-1.               EL634
01022      MOVE SPACE                  TO  DC-OPTION-CODE.              EL634
01023                                                                   EL634
01024      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.                  EL634
01025                                                                   EL634
01026      MOVE DC-GREG-DATE-1-EDIT    TO  EFFDTI.                      EL634
01027                                                                      CL**6
01028 ***  Y2K PROJ 7744                                                   CL*11
01029      IF AM-HI-CERT-DATE = ZEROS                                      CL*11
01030          MOVE ER-2705            TO  EMI-ERROR                       CL**6
01031          MOVE -1                 TO  MAINTL                          CL**6
01032          PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                  CL**6
01033 ***  Y2K PROJ 7744                                                   CL*11
01034                                                                      CL**6
01035      MOVE AM-EXPIRATION-DT       TO  WS-EXP-DT-HLD.               EL634
01036                                                                   EL634
01037      IF AM-EXPIRATION-DT = HIGH-VALUES                            EL634
01038          MOVE '99/99/99'           TO  EXPDTI                     EL634
01039      ELSE                                                         EL634
01040          MOVE AM-EXPIRATION-DT     TO  DC-BIN-DATE-1              EL634
01041          MOVE SPACE                TO  DC-OPTION-CODE             EL634
01042          PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT               EL634
01043          MOVE DC-GREG-DATE-1-EDIT  TO  EXPDTI                     EL634
01044          GO TO 1640-END-BROWSE.                                   EL634
01045                                                                   EL634
01046      IF WS-BIN-EFF-DATE-ENTERED = AM-EXPIRATION-DT                EL634
01047          GO TO 1620-READ-LOOP.                                    EL634
01048                                                                   EL634
01049      GO TO 1699-EXIT.                                             EL634
01050  EJECT                                                            EL634
01051  1630-NO-MATCH.                                                   EL634
01052      MOVE ER-2584                TO  EMI-ERROR.                   EL634
01053      MOVE -1                     TO  EFFDTL.                      EL634
01054      MOVE AL-UNBON               TO  EFFDTA.                      EL634
01055                                                                   EL634
01056      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
01057                                                                   EL634
01058  1640-END-BROWSE.                                                 EL634
01059      EXEC CICS ENDBR                                              EL634
01060          DATASET  (ERACCT-ALT-FILE-ID)                            EL634
01061      END-EXEC.                                                    EL634
01062                                                                   EL634
01063  1699-EXIT.                                                       EL634
01064      EXIT.                                                        EL634
01065  EJECT                                                            EL634
01066  2000-ADD-RECORD.                                                 EL634
01067      EXEC CICS GETMAIN                                            EL634
01068          SET      (ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS)        CL**7
01069          LENGTH   (200)                                           EL634
01070          INITIMG  (GETMAIN-SPACE)                                 EL634
01071      END-EXEC.                                                    EL634
01072                                                                   EL634
01073      MOVE 'RP'                   TO  RP-RECORD-ID.                EL634
01074      MOVE PI-COMPANY-CD          TO  RP-COMPANY-CD.               EL634
01075                                                                   EL634
01076      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL634
01077        OR CARR-ST-ACCNT-CNTL                                      EL634
01078        OR CARR-ACCNT-CNTL                                         EL634
01079          MOVE CARRIERI           TO  RP-CARRIER                   EL634
01080      ELSE                                                         EL634
01081          MOVE SPACES             TO  RP-CARRIER.                  EL634
01082                                                                   EL634
01083      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL634
01084          MOVE GROUPI             TO  RP-GROUPING                  EL634
01085      ELSE                                                         EL634
01086          MOVE SPACES             TO  RP-GROUPING.                 EL634
01087                                                                   EL634
01088      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL634
01089        OR CARR-ST-ACCNT-CNTL                                      EL634
01090        OR ST-ACCNT-CNTL                                           EL634
01091          MOVE STATEI             TO  RP-STATE                     EL634
01092      ELSE                                                         EL634
01093          MOVE SPACES             TO  RP-STATE.                    EL634
01094                                                                   EL634
01095      MOVE ACCTI                  TO  RP-ACCOUNT.                  EL634
01096      MOVE PI-WORK-SEQ-NO         TO  RP-FILE-SEQ-NO.              EL634
01097      ADD +1                      TO  PI-WORK-SEQ-NO.              EL634
01098      MOVE '1'                    TO  RP-RECORD-TYPE.              EL634
01099      MOVE PI-PROCESSOR-ID        TO  RP-LAST-MAINT-BY.            EL634
01100      MOVE EIBTIME                TO  RP-LAST-MAINT-HHMMSS.        EL634
01101      MOVE WS-CURRENT-BIN-DT      TO  RP-LAST-MAINT-DT             EL634
01102                                      RP-INPUT-DT.                 EL634
01103      MOVE LOW-VALUES             TO  RP-CREDIT-ACCEPT-DT.         EL634
01104                                                                      CL**2
01105      IF EOMDTL IS GREATER THAN ZEROS                                 CL**2
01106          MOVE EOMDTI                 TO  DC-GREG-DATE-1-MDY          CL**2
01107          MOVE '4'                    TO  DC-OPTION-CODE              CL**2
01108          PERFORM 8500-DATE-CONVERT  THRU 8599-EXIT                   CL**2
01109          IF NO-CONVERSION-ERROR                                      CL**2
01110              MOVE DC-BIN-DATE-1      TO  RP-CREDIT-SELECT-DT         CL**2
01111          ELSE                                                        CL**2
01112              MOVE PI-CR-MONTH-END-DT TO  RP-CREDIT-SELECT-DT         CL**2
01113      ELSE                                                            CL**2
01114          MOVE PI-CR-MONTH-END-DT     TO  RP-CREDIT-SELECT-DT.        CL**2
01115                                                                      CL**2
01116      MOVE RP-CREDIT-SELECT-DT        TO  DC-BIN-DATE-1.              CL**2
01117      MOVE ' '                        TO  DC-OPTION-CODE.             CL**2
01118      PERFORM 8500-DATE-CONVERT THRU 8599-EXIT.                       CL**2
01119      IF NO-CONVERSION-ERROR                                          CL**2
01120          MOVE DC-GREG-DATE-1-EDIT    TO  EOMDTO                      CL**2
01121      ELSE                                                            CL**2
01122          MOVE SPACES                 TO  EOMDTO.                     CL**2
01123                                                                   EL634
01124      IF RCOMPL NOT = ZEROS                                        EL634
01125          MOVE RCOMPI             TO  RP-REIN-COMP-NO.             EL634
01126                                                                   EL634
01127      MOVE WS-EDITED-BEN-CD       TO  RP-BENEFIT-CD.               EL634
01128      MOVE BENTYPI                TO  RP-BENEFIT-TYPE.             EL634
01129                                                                   EL634
01130      IF MTHYRL NOT = ZEROS                                        EL634
01131          MOVE MTHYRI             TO  RP-EPEC-ADJ-DT               EL634
01132      ELSE                                                         EL634
01133          MOVE ZEROS              TO  RP-EPEC-ADJ-DT.              EL634
01134                                                                   EL634
01135      MOVE WS-EFF-DT-HLD            TO  RP-ACCOUNT-EFF-DT          EL634
01136      MOVE WS-EXP-DT-HLD            TO  RP-ACCOUNT-EXP-DT          EL634
01137      MOVE WS-BIN-EFF-DATE-ENTERED  TO  RP-EFF-DATE-ENTERED        EL634
01138      MOVE WS-SV-CARRIER            TO  RP-SV-CARRIER.             EL634
01139      MOVE WS-SV-GROUPING           TO  RP-SV-GROUPING.            EL634
01140      MOVE WS-SV-STATE              TO  RP-SV-STATE.               EL634
01141      MOVE ZEROS                    TO  RP-INS-AMT-INFORCE         EL634
01142                                        RP-LIFE-MORTALITY-AMT      EL634
01143                                        RP-FUTURE-RESERVE          EL634
01144                                        RP-PTC-RESERVE             EL634
01145                                        RP-IBNR-RESERVE            EL634
01146                                        RP-CLAIM-ADJ-AMT           EL634
01147                                        RP-EXPENSES                EL634
01148                                        RP-PAYMENTS                EL634
01149                                        RP-OTHER-COMM              EL634
01150                                        RP-REIN-PREM-ADJ.          EL634
01151                                                                   EL634
01152      MOVE RP-LAST-MAINT-BY       TO  MAINTBYO.                    EL634
01153      MOVE WS-CURRENT-DT          TO  MAINTDTO.                    EL634
01154      MOVE RP-LAST-MAINT-HHMMSS   TO  TIME-IN.                     EL634
01155      MOVE TIME-OUT               TO  MAINTATO.                    EL634
01156                                                                   EL634
01157      IF INFORCEL NOT = ZEROS                                      EL634
01158          MOVE WS-INFORCEI         TO  RP-INS-AMT-INFORCE             CL**5
01159          MOVE RP-INS-AMT-INFORCE  TO  INFORCEO.                   EL634
01160                                                                   EL634
01161      IF MORTAMTL NOT = ZEROS                                      EL634
01162          MOVE WS-MORTAMTI            TO  RP-LIFE-MORTALITY-AMT       CL**5
01163          MOVE RP-LIFE-MORTALITY-AMT  TO  MORTAMTO.                EL634
01164                                                                   EL634
01165      IF FUTUREL NOT = ZEROS                                       EL634
01166          MOVE WS-FUTUREI         TO  RP-FUTURE-RESERVE               CL**5
01167          MOVE RP-FUTURE-RESERVE  TO  FUTUREO.                     EL634
01168                                                                   EL634
01169      IF PTCL NOT = ZEROS                                          EL634
01170          MOVE WS-PTCI            TO  RP-PTC-RESERVE                  CL**5
01171          MOVE RP-PTC-RESERVE     TO  PTCO.                        EL634
01172                                                                   EL634
01173      IF IBNRL NOT = ZEROS                                         EL634
01174          MOVE WS-IBNRI           TO  RP-IBNR-RESERVE                 CL**5
01175          MOVE RP-IBNR-RESERVE    TO  IBNRO.                       EL634
01176                                                                   EL634
01177      IF CLAIML NOT = ZEROS                                        EL634
01178          MOVE WS-CLAIMI          TO  RP-CLAIM-ADJ-AMT                CL**5
01179          MOVE RP-CLAIM-ADJ-AMT   TO  CLAIMO.                      EL634
01180                                                                   EL634
01181      IF EXPL NOT = ZEROS                                          EL634
01182          MOVE WS-EXPI            TO  RP-EXPENSES                     CL**5
01183          MOVE RP-EXPENSES        TO  EXPO.                        EL634
01184                                                                   EL634
01185      IF PYMNTL NOT = ZEROS                                        EL634
01186          MOVE WS-PYMNTI          TO  RP-PAYMENTS                     CL**5
01187          MOVE RP-PAYMENTS        TO  PYMNTO.                      EL634
01188                                                                   EL634
01189      IF OCOMML NOT = ZEROS                                        EL634
01190          MOVE WS-OCOMMI          TO  RP-OTHER-COMM                   CL**5
01191          MOVE RP-OTHER-COMM      TO  OCOMMO.                      EL634
01192                                                                   EL634
01193      IF RPREML NOT = ZEROS                                        EL634
01194          MOVE WS-RPREMI          TO  RP-REIN-PREM-ADJ                CL**5
01195          MOVE RP-REIN-PREM-ADJ   TO  RPREMO.                      EL634
01196                                                                   EL634
01197  2100-WRITE-RECORD.                                               EL634
01198      MOVE RP-CONTROL-PRIMARY              TO  PI-ERREPY-KEY.      EL634
01199 *    MOVE 'A'                             TO  JP-RECORD-TYPE.     EL634
01200 *    MOVE PENDING-RETRO-REIN-ADJUSTMENTS  TO  JP-RECORD-AREA.     EL634
01201                                                                   EL634
01202      EXEC CICS WRITE                                              EL634
01203          DATASET  (ERREPY-FILE-ID)                                EL634
01204          FROM     (PENDING-RETRO-REIN-ADJUSTMENTS)                EL634
01205          RIDFLD   (RP-CONTROL-PRIMARY)                            EL634
01206          END-EXEC.                                                EL634
01207                                                                   EL634
01208 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL634
01209                                                                   EL634
01210      IF NOT  EMI-NO-ERRORS                                        EL634
01211          GO TO 8200-SEND-DATAONLY.                                EL634
01212                                                                   EL634
01213      MOVE ER-0000                TO  EMI-ERROR                    EL634
01214                                                                   EL634
01215      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
01216                                                                   EL634
01217      GO TO 8100-SEND-INITIAL-MAP.                                 EL634
01218  EJECT                                                            EL634
01219  3000-CHANGE-RECORD.                                              EL634
01220      EXEC CICS HANDLE CONDITION                                   EL634
01221          NOTFND  (3900-RECORD-NOTFND)                             EL634
01222      END-EXEC.                                                    EL634
01223                                                                   EL634
01224      EXEC CICS READ                                               EL634
01225          DATASET  (ERREPY-FILE-ID)                                EL634
01226          SET      (ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS)        CL**7
01227          RIDFLD   (PI-ERREPY-KEY)                                 EL634
01228          UPDATE                                                   EL634
01229      END-EXEC.                                                    EL634
01230                                                                   EL634
01231 *    MOVE 'B'                             TO  JP-RECORD-TYPE.     EL634
01232 *    MOVE PENDING-RETRO-REIN-ADJUSTMENTS  TO  JP-RECORD-AREA.     EL634
01233                                                                   EL634
01234 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL634
01235                                                                   EL634
01236      IF RCOMPL NOT = ZEROS                                        EL634
01237          MOVE RCOMPI             TO  RP-REIN-COMP-NO.             EL634
01238                                                                   EL634
01239      IF BENCDL NOT = ZEROS                                        EL634
01240          MOVE WS-EDITED-BEN-CD   TO  RP-BENEFIT-CD.               EL634
01241                                                                   EL634
01242      IF BENTYPL NOT = ZEROS                                       EL634
01243          MOVE BENTYPI            TO  RP-BENEFIT-TYPE.             EL634
01244                                                                   EL634
01245      IF MTHYRL NOT = ZEROS                                        EL634
01246          MOVE MTHYRI             TO  RP-EPEC-ADJ-DT.              EL634
01247                                                                   EL634
01248      MOVE WS-EFF-DT-HLD            TO  RP-ACCOUNT-EFF-DT.         EL634
01249      MOVE WS-EXP-DT-HLD            TO  RP-ACCOUNT-EXP-DT.         EL634
01250      MOVE WS-BIN-EFF-DATE-ENTERED  TO  RP-EFF-DATE-ENTERED.       EL634
01251                                                                   EL634
01252      IF EOMDTL IS NOT EQUAL TO ZEROS                                 CL**2
01253          MOVE EOMDTI                  TO  DC-GREG-DATE-1-MDY         CL**2
01254          MOVE '4'                     TO  DC-OPTION-CODE             CL**2
01255          PERFORM 8500-DATE-CONVERT THRU 8599-EXIT                    CL**2
01256          IF NO-CONVERSION-ERROR                                      CL**2
01257              MOVE DC-BIN-DATE-1       TO  RP-CREDIT-SELECT-DT        CL**2
01258          ELSE                                                        CL**2
01259              MOVE PI-CR-MONTH-END-DT  TO  RP-CREDIT-SELECT-DT.       CL**2
01260                                                                      CL**2
01261      MOVE RP-CREDIT-SELECT-DT         TO  DC-BIN-DATE-1.             CL**2
01262      MOVE ' '                         TO  DC-OPTION-CODE.            CL**2
01263      PERFORM 8500-DATE-CONVERT THRU 8599-EXIT.                       CL**2
01264      IF NO-CONVERSION-ERROR                                          CL**2
01265          MOVE DC-GREG-DATE-1-EDIT     TO  EOMDTO                     CL**2
01266      ELSE                                                            CL**2
01267          MOVE SPACES                  TO  EOMDTO.                    CL**2
01268                                                                      CL**2
01269  EJECT                                                               CL**2
01270      IF INFORCEL NOT = ZEROS                                      EL634
01271          MOVE WS-INFORCEI         TO  RP-INS-AMT-INFORCE             CL**5
01272          MOVE RP-INS-AMT-INFORCE  TO  INFORCEO.                   EL634
01273                                                                   EL634
01274      IF MORTAMTL NOT = ZEROS                                      EL634
01275          MOVE WS-MORTAMTI         TO  RP-LIFE-MORTALITY-AMT          CL**5
01276          MOVE RP-LIFE-MORTALITY-AMT  TO  MORTAMTO.                EL634
01277      IF FUTUREL NOT = ZEROS                                       EL634
01278          MOVE WS-FUTUREI         TO  RP-FUTURE-RESERVE               CL**5
01279          MOVE RP-FUTURE-RESERVE  TO  FUTUREO.                     EL634
01280                                                                   EL634
01281      IF PTCL NOT = ZEROS                                          EL634
01282          MOVE WS-PTCI            TO  RP-PTC-RESERVE                  CL**5
01283          MOVE RP-PTC-RESERVE     TO  PTCO.                        EL634
01284                                                                   EL634
01285      IF IBNRL NOT = ZEROS                                         EL634
01286          MOVE WS-IBNRI           TO  RP-IBNR-RESERVE                 CL**5
01287          MOVE RP-IBNR-RESERVE    TO  IBNRO.                       EL634
01288                                                                   EL634
01289      IF CLAIML NOT = ZEROS                                        EL634
01290          MOVE WS-CLAIMI          TO  RP-CLAIM-ADJ-AMT                CL**5
01291          MOVE RP-CLAIM-ADJ-AMT   TO  CLAIMO.                      EL634
01292                                                                   EL634
01293      IF EXPL NOT = ZEROS                                          EL634
01294          MOVE WS-EXPI            TO  RP-EXPENSES                     CL**5
01295          MOVE RP-EXPENSES        TO  EXPO.                        EL634
01296                                                                   EL634
01297      IF PYMNTL NOT = ZEROS                                        EL634
01298          MOVE WS-PYMNTI          TO  RP-PAYMENTS                     CL**5
01299          MOVE RP-PAYMENTS        TO  PYMNTO.                      EL634
01300                                                                   EL634
01301      IF OCOMML NOT = ZEROS                                        EL634
01302          MOVE WS-OCOMMI          TO  RP-OTHER-COMM                   CL**5
01303          MOVE RP-OTHER-COMM      TO  OCOMMO.                      EL634
01304                                                                   EL634
01305      IF RPREML NOT = ZEROS                                        EL634
01306          MOVE WS-RPREMI          TO  RP-REIN-PREM-ADJ                CL**5
01307          MOVE RP-REIN-PREM-ADJ   TO  RPREMO.                      EL634
01308                                                                   EL634
01309  3100-REWRITE-RECORD.                                             EL634
01310      MOVE PI-PROCESSOR-ID        TO  RP-LAST-MAINT-BY.            EL634
01311      MOVE EIBTIME                TO  RP-LAST-MAINT-HHMMSS.        EL634
01312      MOVE WS-CURRENT-BIN-DT      TO  RP-LAST-MAINT-DT.            EL634
01313      MOVE RP-LAST-MAINT-BY       TO  MAINTBYO.                    EL634
01314      MOVE WS-CURRENT-DT          TO  MAINTDTO.                    EL634
01315      MOVE RP-LAST-MAINT-HHMMSS   TO  TIME-IN.                     EL634
01316      MOVE TIME-OUT               TO  MAINTATO.                    EL634
01317 *    MOVE 'C'                    TO  JP-RECORD-TYPE.              EL634
01318 *    MOVE PENDING-RETRO-REIN-ADJUSTMENTS                          EL634
01319 *                                TO  JP-RECORD-AREA.              EL634
01320                                                                   EL634
01321      EXEC CICS REWRITE                                            EL634
01322          DATASET  (ERREPY-FILE-ID)                                EL634
01323          FROM     (PENDING-RETRO-REIN-ADJUSTMENTS)                EL634
01324      END-EXEC.                                                    EL634
01325                                                                   EL634
01326 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL634
01327                                                                   EL634
01328      MOVE ER-0000  TO  EMI-ERROR                                  EL634
01329                                                                   EL634
01330      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
01331                                                                   EL634
01332      GO TO 8100-SEND-INITIAL-MAP.                                 EL634
01333                                                                   EL634
01334  3900-RECORD-NOTFND.                                              EL634
01335      MOVE ER-2586                TO  EMI-ERROR.                   EL634
01336                                                                   EL634
01337      PERFORM 6000-SET-VG-CONTROL  THRU  6099-EXIT.                EL634
01338                                                                   EL634
01339      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
01340                                                                   EL634
01341      GO TO 8200-SEND-DATAONLY.                                    EL634
01342  EJECT                                                            EL634
01343  4000-DELETE-RECORD.                                              EL634
01344      EXEC CICS HANDLE CONDITION                                   EL634
01345          NOTFND  (4900-RECORD-NOTFND)                             EL634
01346      END-EXEC.                                                    EL634
01347                                                                   EL634
01348 *    MOVE 'D'                             TO  JP-RECORD-TYPE.     EL634
01349 *    MOVE PENDING-RETRO-REIN-ADJUSTMENTS  TO  JP-RECORD-AREA.     EL634
01350                                                                   EL634
01351      EXEC CICS DELETE                                             EL634
01352          DATASET  (ERREPY-FILE-ID)                                EL634
01353          RIDFLD   (PI-ERREPY-KEY)                                 EL634
01354      END-EXEC.                                                    EL634
01355                                                                   EL634
01356 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL634
01357                                                                   EL634
01358      MOVE LOW-VALUES             TO  EL634AO.                     EL634
01359      MOVE ER-0000                TO  EMI-ERROR                    EL634
01360                                                                   EL634
01361      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
01362                                                                   EL634
01363      GO TO 8100-SEND-INITIAL-MAP.                                 EL634
01364                                                                   EL634
01365  4900-RECORD-NOTFND.                                              EL634
01366      MOVE ER-2586                TO  EMI-ERROR.                   EL634
01367                                                                   EL634
01368      PERFORM 6000-SET-VG-CONTROL  THRU  6099-EXIT.                EL634
01369                                                                   EL634
01370      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
01371                                                                   EL634
01372      GO TO 8200-SEND-DATAONLY.                                    EL634
01373  EJECT                                                            EL634
01374  5000-BROWSE-FILE.                                                EL634
01375      EXEC CICS HANDLE CONDITION                                   EL634
01376          NOTFND   (5800-NO-RECORD)                                EL634
01377          ENDFILE  (5900-END-OF-FILE)                              EL634
01378      END-EXEC.                                                    EL634
01379                                                                   EL634
01380      MOVE PI-ERREPY-KEY          TO  ERREPY-KEY.                  EL634
01381                                                                   EL634
01382      IF EIBAID = DFHENTER                                         EL634
01383          MOVE ZEROS              TO  REPY-RECORD-SEQ.             EL634
01384                                                                   EL634
01385      IF EIBAID = DFHPF2                                           EL634
01386          GO TO 5100-BROWSE-BKWD.                                  EL634
01387                                                                   EL634
01388  5010-READ-LOOP.                                                  EL634
01389 *    IF EIBAID = DFHPF1                                           EL634
01390          ADD +1                  TO  REPY-RECORD-SEQ.             EL634
01391                                                                   EL634
01392      EXEC CICS READ                                               EL634
01393          DATASET  (ERREPY-FILE-ID)                                EL634
01394          SET      (ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS)        CL**7
01395          RIDFLD   (ERREPY-KEY)                                    EL634
01396          GTEQ                                                     EL634
01397      END-EXEC.                                                    EL634
01398                                                                   EL634
01399      IF RP-COMPANY-CD NOT = PI-COMPANY-CD                         EL634
01400          IF EIBAID = DFHENTER                                     EL634
01401              GO TO 5800-NO-RECORD                                 EL634
01402          ELSE                                                     EL634
01403              GO TO 5900-END-OF-FILE.                              EL634
01404                                                                   EL634
01405      IF RP-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL634
01406 *        IF EIBAID = DFHENTER                                     EL634
01407 *            GO TO 5800-NO-RECORD                                 EL634
01408 *        ELSE                                                     EL634
01409              MOVE RP-CONTROL-PRIMARY  TO  ERREPY-KEY              EL634
01410              GO TO 5010-READ-LOOP.                                EL634
01411                                                                   EL634
01412      IF EIBAID = DFHENTER                                         EL634
01413          IF REPY-CARRIER = RP-CARRIER                             EL634
01414            AND REPY-GROUPING = RP-GROUPING                        EL634
01415            AND REPY-STATE = RP-STATE                              EL634
01416            AND REPY-ACCOUNT = RP-ACCOUNT                          EL634
01417              GO TO 5200-FORMAT-SCREEN                             EL634
01418          ELSE                                                     EL634
01419              GO TO 5800-NO-RECORD.                                EL634
01420                                                                   EL634
01421      GO TO 5200-FORMAT-SCREEN.                                    EL634
01422  EJECT                                                            EL634
01423  5100-BROWSE-BKWD.                                                EL634
01424      EXEC CICS STARTBR                                            EL634
01425          DATASET  (ERREPY-FILE-ID)                                EL634
01426          RIDFLD   (ERREPY-KEY)                                    EL634
01427      END-EXEC.                                                    EL634
01428                                                                   EL634
01429      EXEC CICS READPREV                                           EL634
01430          DATASET  (ERREPY-FILE-ID)                                EL634
01431          SET      (ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS)        CL**7
01432          RIDFLD   (ERREPY-KEY)                                    EL634
01433      END-EXEC.                                                    EL634
01434                                                                   EL634
01435  5110-READ-LOOP.                                                  EL634
01436      IF PI-FILE-EOF                                               EL634
01437          MOVE SPACE              TO  PI-EOF-SW                    EL634
01438      ELSE                                                         EL634
01439          EXEC CICS READPREV                                       EL634
01440              DATASET  (ERREPY-FILE-ID)                            EL634
01441              SET      (ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS)    CL**7
01442              RIDFLD   (ERREPY-KEY)                                EL634
01443          END-EXEC.                                                EL634
01444                                                                   EL634
01445      IF RP-COMPANY-CD NOT = PI-COMPANY-CD                         EL634
01446          GO TO 5900-END-OF-FILE.                                  EL634
01447                                                                   EL634
01448      IF RP-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL634
01449          GO TO 5110-READ-LOOP.                                    EL634
01450  EJECT                                                            EL634
01451  5200-FORMAT-SCREEN.                                              EL634
01452      MOVE 'S'                    TO  MAINTI  PI-PREV-MAINT.       EL634
01453      MOVE 1                      TO  MAINTL.                      EL634
01454      MOVE RP-CONTROL-PRIMARY     TO  PI-ERREPY-KEY.               EL634
01455      MOVE LOW-VALUES             TO  EL634AI.                     EL634
01456      MOVE RP-CARRIER             TO  CARRIERO.                    EL634
01457      MOVE RP-GROUPING            TO  GROUPO.                      EL634
01458      MOVE RP-STATE               TO  STATEO.                      EL634
01459      MOVE RP-ACCOUNT             TO  ACCTO.                       EL634
01460                                                                   EL634
01461      IF RP-REIN-COMP-NO NOT = SPACES                              EL634
01462          MOVE RP-REIN-COMP-NO    TO  RCOMPO.                      EL634
01463                                                                   EL634
01464      MOVE RP-BENEFIT-CD          TO  BENCDO.                      EL634
01465      MOVE RP-BENEFIT-TYPE        TO  BENTYPO.                     EL634
01466                                                                      CL**2
01467      IF RP-CREDIT-SELECT-DT IS NOT EQUAL TO LOW-VALUES AND SPACES    CL**2
01468          MOVE RP-CREDIT-SELECT-DT      TO  DC-BIN-DATE-1             CL**2
01469          MOVE ' '                      TO  DC-OPTION-CODE            CL**2
01470          PERFORM 8500-DATE-CONVERT THRU 8599-EXIT                    CL**2
01471          IF NO-CONVERSION-ERROR                                      CL**2
01472              MOVE DC-GREG-DATE-1-EDIT  TO  EOMDTO                    CL**2
01473          ELSE                                                        CL**2
01474              MOVE SPACES               TO  EOMDTO.                   CL**2
01475                                                                   EL634
01476      IF RP-EPEC-ADJ-DT NOT = ZEROS                                EL634
01477          MOVE RP-EPEC-ADJ-DT           TO  MTHYRO                 EL634
01478          MOVE RP-ACCOUNT-EFF-DT        TO  DC-BIN-DATE-1          EL634
01479          MOVE SPACE                    TO  DC-OPTION-CODE         EL634
01480          PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT               EL634
01481          MOVE DC-GREG-DATE-1-EDIT      TO  EFFDTO                 EL634
01482          IF RP-ACCOUNT-EXP-DT = HIGH-VALUES                       EL634
01483              MOVE '99/99/99'           TO  EXPDTO                 EL634
01484          ELSE                                                     EL634
01485              MOVE RP-ACCOUNT-EXP-DT    TO  DC-BIN-DATE-1          EL634
01486              MOVE SPACE                TO  DC-OPTION-CODE         EL634
01487              PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT           EL634
01488              MOVE DC-GREG-DATE-1-EDIT  TO  EXPDTO.                EL634
01489                                                                   EL634
01490      MOVE RP-LAST-MAINT-BY       TO  MAINTBYO.                    EL634
01491      MOVE ' '                    TO  DC-OPTION-CODE.              EL634
01492      MOVE RP-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               EL634
01493                                                                   EL634
01494      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.                  EL634
01495                                                                   EL634
01496      IF DATE-CONVERSION-ERROR                                     EL634
01497          MOVE ZEROS                TO  MAINTDTO                   EL634
01498      ELSE                                                         EL634
01499          MOVE DC-GREG-DATE-1-EDIT  TO  MAINTDTO.                  EL634
01500                                                                   EL634
01501      MOVE RP-LAST-MAINT-HHMMSS   TO  TIME-IN.                     EL634
01502      MOVE TIME-OUT               TO  MAINTATO.                    EL634
01503                                                                   EL634
01504      IF RP-INS-AMT-INFORCE NOT = ZEROS                            EL634
01505          MOVE RP-INS-AMT-INFORCE  TO  INFORCEO.                   EL634
01506                                                                   EL634
01507      IF RP-LIFE-MORTALITY-AMT NOT = ZEROS                         EL634
01508          MOVE RP-LIFE-MORTALITY-AMT  TO  MORTAMTO.                EL634
01509                                                                   EL634
01510      IF RP-FUTURE-RESERVE NOT = ZEROS                             EL634
01511          MOVE RP-FUTURE-RESERVE  TO  FUTUREO.                     EL634
01512                                                                   EL634
01513      IF RP-PTC-RESERVE NOT = ZEROS                                EL634
01514          MOVE RP-PTC-RESERVE     TO  PTCO.                        EL634
01515                                                                   EL634
01516      IF RP-IBNR-RESERVE NOT = ZEROS                               EL634
01517          MOVE RP-IBNR-RESERVE    TO  IBNRO.                       EL634
01518                                                                   EL634
01519      IF RP-CLAIM-ADJ-AMT NOT = ZEROS                              EL634
01520          MOVE RP-CLAIM-ADJ-AMT   TO  CLAIMO.                      EL634
01521                                                                   EL634
01522      IF RP-EXPENSES NOT = ZEROS                                   EL634
01523          MOVE RP-EXPENSES        TO  EXPO.                        EL634
01524                                                                   EL634
01525      IF RP-PAYMENTS NOT = ZEROS                                   EL634
01526          MOVE RP-PAYMENTS        TO  PYMNTO.                      EL634
01527                                                                   EL634
01528      IF RP-OTHER-COMM NOT = ZEROS                                 EL634
01529          MOVE RP-OTHER-COMM      TO  OCOMMO.                      EL634
01530                                                                   EL634
01531      IF RP-REIN-PREM-ADJ NOT = ZEROS                              EL634
01532          MOVE RP-REIN-PREM-ADJ   TO  RPREMO.                      EL634
01533                                                                   EL634
01534      GO TO 8100-SEND-INITIAL-MAP.                                 EL634
01535  EJECT                                                            EL634
01536  5800-NO-RECORD.                                                  EL634
01537      IF EIBAID = DFHPF2                                           EL634
01538          GO TO 5900-END-OF-FILE.                                  EL634
01539                                                                   EL634
01540      MOVE ER-2586                TO  EMI-ERROR.                   EL634
01541                                                                   EL634
01542      PERFORM 6000-SET-VG-CONTROL  THRU  6099-EXIT.                EL634
01543                                                                   EL634
01544      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
01545                                                                   EL634
01546      GO TO 8200-SEND-DATAONLY.                                    EL634
01547                                                                   EL634
01548  5900-END-OF-FILE.                                                EL634
01549      IF EIBAID = DFHPF1                                           EL634
01550          MOVE 'Y'                TO  PI-EOF-SW                    EL634
01551          MOVE ER-2237            TO  EMI-ERROR                    EL634
01552      ELSE                                                         EL634
01553          MOVE SPACES             TO  PI-ERREPY-KEY                EL634
01554          MOVE ER-2238            TO  EMI-ERROR.                   EL634
01555                                                                   EL634
01556      MOVE -1                     TO  MAINTL.                      EL634
01557                                                                   EL634
01558      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL634
01559                                                                   EL634
01560      GO TO 8200-SEND-DATAONLY.                                    EL634
01561  EJECT                                                            EL634
01562  6000-SET-VG-CONTROL.                                             EL634
01563      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL634
01564          MOVE -1                    TO  CARRIERL                  EL634
01565          MOVE AL-UABON              TO  CARRIERA                  EL634
01566                                         GROUPA                    EL634
01567                                         STATEA                    EL634
01568                                         ACCTA                     EL634
01569      ELSE                                                         EL634
01570          IF ST-ACCNT-CNTL                                         EL634
01571              MOVE -1                TO  STATEL                    EL634
01572              MOVE AL-UABON          TO  STATEA                    EL634
01573                                         ACCTA                     EL634
01574          ELSE                                                     EL634
01575              IF CARR-ST-ACCNT-CNTL                                EL634
01576                  MOVE -1            TO  CARRIERL                  EL634
01577                  MOVE AL-UABON      TO  CARRIERA                  EL634
01578                                         STATEA                    EL634
01579                                         ACCTA                     EL634
01580              ELSE                                                 EL634
01581                  IF ACCNT-CNTL                                    EL634
01582                      MOVE -1        TO  ACCTL                     EL634
01583                      MOVE AL-UABON  TO  ACCTA                     EL634
01584                  ELSE                                             EL634
01585                      MOVE -1        TO  CARRIERL                  EL634
01586                      MOVE AL-UABON  TO  CARRIERA                  EL634
01587                                         ACCTA.                    EL634
01588                                                                   EL634
01589  6099-EXIT.                                                       EL634
01590      EXIT.                                                        EL634
01591  EJECT                                                            EL634
01592  8100-SEND-INITIAL-MAP.                                           EL634
01593      MOVE WS-CURRENT-DT          TO  DATEO.                       EL634
01594      MOVE EIBTIME                TO  TIME-IN.                     EL634
01595      MOVE TIME-OUT               TO  TIMEO.                       EL634
01596      MOVE -1                     TO  MAINTL.                      EL634
01597      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL634
01598      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                    EL634
01599      MOVE EMI-MESSAGE-AREA (3)   TO  ERRMSG3O.                    EL634
01600      MOVE PI-LIFE-OVERRIDE-L6    TO  LFHDGO.                         CL**2
01601                                                                   EL634
01602      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL634
01603          NEXT SENTENCE                                            EL634
01604      ELSE                                                         EL634
01605          IF ST-ACCNT-CNTL                                         EL634
01606              MOVE AL-SADOF              TO  CARHDGA               EL634
01607                                             GRPHDGA               EL634
01608              MOVE AL-SANOF              TO  CARRIERA              EL634
01609                                             GROUPA                EL634
01610          ELSE                                                     EL634
01611              IF CARR-ST-ACCNT-CNTL                                EL634
01612                  MOVE AL-SADOF          TO  GRPHDGA               EL634
01613                  MOVE AL-SANOF          TO  GROUPA                EL634
01614              ELSE                                                 EL634
01615                  IF ACCNT-CNTL                                    EL634
01616                      MOVE AL-SADOF      TO  CARHDGA               EL634
01617                                             GRPHDGA               EL634
01618                                             STHDGA                EL634
01619                      MOVE AL-SANOF      TO  CARRIERA              EL634
01620                                             GROUPA                EL634
01621                                             STATEA                EL634
01622                  ELSE                                             EL634
01623                      IF CARR-ACCNT-CNTL                           EL634
01624                          MOVE AL-SADOF  TO  GRPHDGA               EL634
01625                                             STHDGA                EL634
01626                          MOVE AL-SANOF  TO  GROUPA                EL634
01627                                             STATEA.               EL634
01628                                                                   EL634
01629      EXEC CICS SEND                                               EL634
01630          MAP     (EL634A)                                         EL634
01631          MAPSET  (MAPSET-NAME)                                    EL634
01632          FROM    (EL634AO)                                        EL634
01633          ERASE                                                    EL634
01634          CURSOR                                                   EL634
01635      END-EXEC.                                                    EL634
01636                                                                   EL634
01637      GO TO 9100-RETURN-TRAN.                                      EL634
01638  EJECT                                                            EL634
01639  8200-SEND-DATAONLY.                                              EL634
01640      MOVE WS-CURRENT-DT          TO  DATEO.                       EL634
01641      MOVE EIBTIME                TO  TIME-IN.                     EL634
01642      MOVE TIME-OUT               TO  TIMEO.                       EL634
01643      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL634
01644      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                    EL634
01645      MOVE EMI-MESSAGE-AREA (3)   TO  ERRMSG3O.                    EL634
01646      MOVE PI-LIFE-OVERRIDE-L6    TO  LFHDGO.                         CL**2
01647                                                                   EL634
01648      EXEC CICS SEND                                               EL634
01649          MAP     (EL634A)                                         EL634
01650          MAPSET  (MAPSET-NAME)                                    EL634
01651          FROM    (EL634AO)                                        EL634
01652          DATAONLY                                                 EL634
01653          ERASEAUP                                                 EL634
01654          CURSOR                                                   EL634
01655      END-EXEC.                                                    EL634
01656                                                                   EL634
01657      GO TO 9100-RETURN-TRAN.                                      EL634
01658  EJECT                                                            EL634
01659  8300-SEND-TEXT.                                                  EL634
01660      EXEC CICS SEND TEXT                                          EL634
01661          FROM    (LOGOFF-TEXT)                                    EL634
01662          LENGTH  (LOGOFF-LENGTH)                                  EL634
01663          ERASE                                                    EL634
01664          FREEKB                                                   EL634
01665      END-EXEC.                                                    EL634
01666                                                                   EL634
01667      EXEC CICS RETURN                                             EL634
01668      END-EXEC.                                                    EL634
01669                                                                   EL634
01670 *8400-LOG-JOURNAL-RECORD.                                         EL634
01671 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL634
01672 *    MOVE ERREPY-FILE-ID         TO  JP-FILE-ID.                  EL634
01673 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL634
01674                                                                   EL634
01675 *    EXEC CICS JOURNAL                                            EL634
01676 *        JFILEID  (PI-JOURNAL-FILE-ID)                            EL634
01677 *        JTYPEID  ('EL')                                          EL634
01678 *        FROM     (JOURNAL-RECORD)                                EL634
01679 *        LENGTH   (423)                                           EL634
01680 *    END-EXEC.                                                    EL634
01681                                                                   EL634
01682  8500-DATE-CONVERT.                                               EL634
01683      MOVE LINK-CLDATCV           TO  PGM-NAME.                    EL634
01684                                                                   EL634
01685      EXEC CICS LINK                                               EL634
01686          PROGRAM   (PGM-NAME)                                     EL634
01687          COMMAREA  (DATE-CONVERSION-DATA)                         EL634
01688          LENGTH    (DC-COMM-LENGTH)                               EL634
01689      END-EXEC.                                                    EL634
01690                                                                   EL634
01691  8599-EXIT.                                                       EL634
01692      EXIT.                                                        EL634
01693                                                                   EL634
CIDMOD/                                                                 EL634
CIDMOD 8600-DEEDIT.                                                          000
CIDMOD     EXEC CICS BIF DEEDIT                                              000
CIDMOD         FIELD   (WS-DEEDIT-FIELD)                                     000
CIDMOD         LENGTH  (11)                                                  000
CIDMOD     END-EXEC.                                                         000
CIDMOD                                                                       000
CIDMOD 8699-EXIT.                                                            000
CIDMOD     EXIT.                                                             000
CIDMOD/
01695  8700-DEEDIT-DATE.                                                EL634
01696      EXEC CICS BIF DEEDIT                                         EL634
01697          FIELD   (WS-DEEDIT-DATE)                                 EL634
01698          LENGTH  (8)                                              EL634
01699      END-EXEC.                                                    EL634
01700                                                                   EL634
01701  8799-EXIT.                                                       EL634
01702      EXIT.                                                        EL634
01703                                                                   EL634
01704  8800-UNAUTHORIZED-ACCESS.                                        EL634
01705      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL634
01706                                                                   EL634
01707      GO TO 8300-SEND-TEXT.                                        EL634
01708                                                                   EL634
01709  8900-PF23.                                                       EL634
01710      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL634
01711      MOVE XCTL-005               TO  PGM-NAME.                    EL634
01712                                                                   EL634
01713      GO TO 9300-XCTL.                                             EL634
01714  EJECT                                                            EL634
01715  9000-RETURN-CICS.                                                EL634
01716      EXEC CICS RETURN                                             EL634
01717          END-EXEC.                                                EL634
01718                                                                   EL634
01719  9100-RETURN-TRAN.                                                EL634
01720      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL634
01721      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL634
01722                                                                   EL634
01723      EXEC CICS RETURN                                             EL634
01724          TRANSID   (TRANS-ID)                                     EL634
01725          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL634
01726          LENGTH    (PI-COMM-LENGTH)                               EL634
01727      END-EXEC.                                                    EL634
01728                                                                   EL634
01729  9200-RETURN-MAIN-MENU.                                           EL634
01730      MOVE XCTL-626               TO  PGM-NAME.                    EL634
01731                                                                   EL634
01732      GO TO 9300-XCTL.                                             EL634
01733                                                                   EL634
01734  9300-XCTL.                                                       EL634
01735      EXEC CICS XCTL                                               EL634
01736          PROGRAM   (PGM-NAME)                                     EL634
01737          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL634
01738          LENGTH    (PI-COMM-LENGTH)                               EL634
01739      END-EXEC.                                                    EL634
01740                                                                   EL634
01741  9400-CLEAR.                                                      EL634
01742      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL634
01743                                                                   EL634
01744      GO TO 9300-XCTL.                                             EL634
01745                                                                   EL634
01746  9500-PF12.                                                       EL634
01747      MOVE XCTL-010               TO  PGM-NAME.                    EL634
01748                                                                   EL634
01749      GO TO 9300-XCTL.                                             EL634
01750                                                                   EL634
01751  9600-PGMID-ERROR.                                                EL634
01752      EXEC CICS HANDLE CONDITION                                   EL634
01753          PGMIDERR  (8300-SEND-TEXT)                               EL634
01754      END-EXEC.                                                    EL634
01755                                                                   EL634
01756      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL634
01757      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL634
01758      MOVE XCTL-005               TO  PGM-NAME.                    EL634
01759      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL634
01760      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL634
01761                                                                   EL634
01762      GO TO 9300-XCTL.                                             EL634
01763  EJECT                                                            EL634
01764  9900-ERROR-FORMAT.                                               EL634
01765      IF NOT  EMI-ERRORS-COMPLETE                                  EL634
01766          MOVE LINK-001           TO  PGM-NAME                     EL634
01767          EXEC CICS LINK                                           EL634
01768              PROGRAM   (PGM-NAME)                                 EL634
01769              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL634
01770              LENGTH    (EMI-COMM-LENGTH)                          EL634
01771          END-EXEC.                                                EL634
01772                                                                   EL634
01773  9900-EXIT.                                                       EL634
01774      EXIT.                                                        EL634
01775                                                                   EL634
01776  9999-ABEND.                                                      EL634
01777      MOVE LINK-004               TO  PGM-NAME.                    EL634
01778      MOVE DFHEIBLK               TO  EMI-LINE1                    EL634
01779                                                                   EL634
01780      EXEC CICS LINK                                               EL634
01781          PROGRAM   (PGM-NAME)                                     EL634
01782          COMMAREA  (EMI-LINE1)                                    EL634
01783          LENGTH    (72)                                           EL634
01784      END-EXEC.                                                    EL634
01785                                                                   EL634
01786      MOVE -1                     TO  PFENTERL.                    EL634
01787                                                                   EL634
01788      GO TO 8200-SEND-DATAONLY.                                    EL634
01789                                                                   EL634
01790      GOBACK.                                                      EL634
01791                                                                   EL634
01792  9995-SECURITY-VIOLATION.                                         EL634
01793                              COPY ELCSCTP.                        EL634
01794                                                                   EL634
01795  9995-EXIT.                                                       EL634
01796      EXIT.                                                        EL634
01797                                                                   EL634
