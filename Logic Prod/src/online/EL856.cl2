00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL856
00003  PROGRAM-ID.                 EL856 .                                 LV003
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/14/96 08:09:25.                    CL**3
00007 *                            VMOD=2.003                              CL**3
00008 *                                                                 EL856
00008 *                                                                 EL856
00009 *AUTHOR.     LOGIC,INC.                                              CL**3
00010 *            DALLAS, TEXAS.                                          CL**3
00011                                                                   EL856
00012 *DATE-COMPILED.                                                      CL**3
00013 *SECURITY.   *****************************************************   CL**3
00014 *            *                                                   *   CL**3
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00016 *            *                                                   *   CL**3
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00018 *                                                                *   CL**3
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00021 *            *                                                   *   CL**3
00022 *            *****************************************************   CL**3
00023                                                                   EL856
00024 *REMARKS. TRANSACTION - EXJ8 - ACCOUNTS RECEIVABLE                   CL**3
00025 *                              SUMMARY CROSS REFERENCE.              CL**3
00026                                                                   EL856
00027  ENVIRONMENT DIVISION.                                            EL856
00028                                                                   EL856
00029      EJECT                                                        EL856
00030  DATA DIVISION.                                                   EL856
00031  WORKING-STORAGE SECTION.                                         EL856
00032                                                                   EL856
00033  77  FILLER  PIC X(32)  VALUE '********************************'. EL856
00034  77  FILLER  PIC X(32)  VALUE '*    EL856 WORKING STORAGE     *'. EL856
00035  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.003 *********'.    CL**3
00036                                                                   EL856
00037     EJECT                                                         EL856
00038                                                                   EL856
00039                              COPY ELCSCTM.                           CL**3
00040                              COPY ELCSCRTY.                          CL**3
00041                                                                   EL856
00042     EJECT                                                         EL856
00043                                                                   EL856
00044 ******************************************************************EL856
00045 *                                                                *EL856
00046 *              S T A N D A R D   A R E A S                       *EL856
00047 *                                                                *EL856
00048 ******************************************************************EL856
00049                                                                   EL856
00050  01  STANDARD-AREAS.                                              EL856
00051      12  SC-ITEM                 PIC S9(4)   VALUE +1 COMP.       EL856
00052      12  QID.                                                     EL856
00053          16  QID-TERM            PIC X(4)      VALUE SPACES.      EL856
00054          16  FILLER              PIC X(4)      VALUE '125D'.      EL856
00055                                                                   EL856
00056      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         EL856
00057      12  EL856A                  PIC X(8)    VALUE 'EL856A'.      EL856
00058      12  MAPSET-EL856S           PIC X(8)    VALUE 'EL856S '.     EL856
00059      12  TRANS-EXJ8              PIC X(4)    VALUE 'EXJ8'.        EL856
00060      12  TRANS-EXD4              PIC X(4)    VALUE 'EXD4'.        EL856
00061      12  THIS-PGM                PIC X(8)    VALUE 'EL856'.       EL856
00062      12  PGM-NAME                PIC X(8).                        EL856
00063      12  TIME-IN                 PIC S9(7).                       EL856
00064      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL856
00065          16  FILLER              PIC X.                           EL856
00066          16  TIME-OUT            PIC 99V99.                       EL856
00067          16  FILLER              PIC X(2).                        EL856
00068      12  LINK-EL001              PIC X(8)    VALUE 'EL001'.       EL856
00069      12  LINK-EL004              PIC X(8)    VALUE 'EL004'.       EL856
00070      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.       EL856
00071      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.       EL856
00072      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.       EL856
00073      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL856
00074      12  FILE-ID-ERSUMM          PIC X(8)    VALUE 'ERSUMM'.      EL856
00075      12  WS-CURRENT-DT           PIC X(8)    VALUE SPACES.        EL856
00076      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.        EL856
00077      12  WS-BROWSE-STARTED-SW    PIC X       VALUE SPACE.         EL856
00078          88 WS-BROWSE-STARTED                VALUE 'Y'.           EL856
00079      12  WS-SUB1                 PIC S9(4)   VALUE ZEROS COMP.    EL856
00080      12  WS-ACCESS-KEY           PIC X(34)   VALUE SPACES.        EL856
00081                                                                   EL856
00082      EJECT                                                        EL856
00083                                                                   EL856
00084 ******************************************************************EL856
00085 *                                                                *EL856
00086 *                E R R O R   M E S S A G E S                     *EL856
00087 *                                                                *EL856
00088 ******************************************************************EL856
00089                                                                   EL856
00090  01  ERROR-MESSAGES.                                              EL856
00091      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL856
00092      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL856
00093      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL856
00094      12  ER-0023                 PIC X(4)  VALUE '0023'.          EL856
00095      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL856
00096      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL856
00097      12  ER-2132                 PIC X(4)  VALUE '2132'.          EL856
00098      12  ER-2237                 PIC X(4)  VALUE '2237'.          EL856
00099      12  ER-2238                 PIC X(4)  VALUE '2238'.          EL856
00100      12  ER-3133                 PIC X(4)  VALUE '3133'.          EL856
00101      12  ER-3135                 PIC X(4)  VALUE '3135'.          EL856
00102                                                                   EL856
00103      EJECT                                                        EL856
00104                                                                   EL856
00105 ******************************************************************EL856
00106 *                                                                *EL856
00107 *              A C C E S S   K E Y S                             *EL856
00108 *                                                                *EL856
00109 ******************************************************************EL856
00110                                                                   EL856
00111  01  ACCESS-KEYS.                                                 EL856
00112                                                                   EL856
00113      12  ERSUMM-KEY.                                              EL856
00114          16  ERSUMM-COMPANY-CD      PIC X     VALUE SPACE.        EL856
00115          16  ERSUMM-SUMMARY         PIC X(6)  VALUE SPACE.        EL856
00116          16  ERSUMM-CARRIER         PIC X     VALUE SPACE.        EL856
00117          16  ERSUMM-GROUP           PIC X(6)  VALUE SPACE.        EL856
00118          16  ERSUMM-FIN-RESP        PIC X(10) VALUE SPACE.        EL856
00119          16  ERSUMM-ACCT-AGENT      PIC X(10) VALUE SPACE.        EL856
00120                                                                   EL856
00121      EJECT                                                        EL856
00122                                                                   EL856
00123                              COPY ELCDATE.                           CL**3
00124                                                                   EL856
00125      EJECT                                                        EL856
00126                              COPY ELCLOGOF.                          CL**3
00127                                                                   EL856
00128      EJECT                                                        EL856
00129                              COPY ELCATTR.                           CL**3
00130                                                                   EL856
00131      EJECT                                                        EL856
00132                              COPY ELCEMIB.                           CL**3
00133                                                                   EL856
00134      EJECT                                                        EL856
00135                              COPY ELCINTF.                           CL**3
00136      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   EL856
00137                                                                   EL856
00138          16  PI-SUMMARY-CODE         PIC X(6).                    EL856
00139          16  PI-1ST-SUM-KEY          PIC X(34).                   EL856
00140          16  PI-LST-SUM-KEY          PIC X(34).                   EL856
00141                                                                   EL856
00142          16  PI-TOP-OF-FILE-SW       PIC X.                       EL856
00143              88  PI-TOP-OF-FILE            VALUE 'Y'.             EL856
00144                                                                   EL856
00145          16  PI-END-OF-FILE-SW       PIC X.                       EL856
00146              88  PI-END-OF-FILE            VALUE 'Y'.             EL856
00147          16  FILLER                  PIC X(564).                     CL**3
00148                                                                      CL**3
00149      EJECT                                                        EL856
00150                              COPY ELCJPFX.                           CL**3
00151                              PIC X(223).                          EL856
00152                                                                   EL856
00153      EJECT                                                        EL856
00154                              COPY ELCAID.                            CL**3
00155  01  FILLER    REDEFINES DFHAID.                                  EL856
00156      12  FILLER              PIC X(8).                            EL856
00157      12  PF-VALUES           PIC X       OCCURS 2.                EL856
00158                                                                   EL856
00159      EJECT                                                        EL856
00160                              COPY EL856S.                            CL**3
00161  01  DISPLAY-MAP REDEFINES EL856AI.                               EL856
00162      12  FILLER                  PIC X(73).                       EL856
00163      12  SM-CROSS-REFERENCE OCCURS 15 TIMES.                      EL856
00164          16  SM-CAR-LEN          PIC S9(4)   COMP.                EL856
00165          16  SM-CAR-ATTRB        PIC X.                           EL856
00166          16  SM-CAR              PIC X.                           EL856
00167          16  SM-GRP-LEN          PIC S9(4)   COMP.                EL856
00168          16  SM-GRP-ATTRB        PIC X.                           EL856
00169          16  SM-GRP              PIC X(6).                        EL856
00170          16  SM-FIN-RESP-LEN     PIC S9(4)   COMP.                EL856
00171          16  SM-FIN-RESP-ATTRB   PIC X.                           EL856
00172          16  SM-FIN-RESP         PIC X(10).                       EL856
00173          16  SM-AGENT-LEN        PIC S9(4)   COMP.                EL856
00174          16  SM-AGENT-ATTRB      PIC X.                           EL856
00175          16  SM-AGENT            PIC X(10).                       EL856
00176          16  SM-AGENT-NAME-LEN   PIC S9(4)   COMP.                EL856
00177          16  SM-AGENT-NAME-ATTRB PIC X.                           EL856
00178          16  SM-AGENT-NAME       PIC X(30).                       EL856
00179                                                                   EL856
00180      EJECT                                                        EL856
00181                                                                   EL856
00182  LINKAGE SECTION.                                                 EL856
00183  01  DFHCOMMAREA             PIC X(1024).                         EL856
00184                                                                   EL856
00185      EJECT                                                        EL856
00186                                                                   EL856
00187                              COPY ERCSUMM.                           CL**3
00188      EJECT                                                        EL856
00189                                                                   EL856
00190  PROCEDURE DIVISION.                                              EL856
00191                                                                   EL856
00192      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL856
00193      MOVE 1                      TO EMI-NUMBER-OF-LINES.          EL856
00194                                                                   EL856
00195      IF EIBCALEN = 0                                              EL856
00196          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL856
00197                                                                   EL856
00198      MOVE EIBTRMID               TO QID-TERM.                     EL856
00199      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL856
00200      MOVE '5'                    TO DC-OPTION-CODE.               EL856
00201      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL856
00202      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL856
00203      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                EL856
00204                                                                   EL856
00205      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL856
00206          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL856
00207              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL856
00208              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL856
00209              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL856
00210              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL856
00211              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL856
00212              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL856
00213              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL856
00214              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL856
00215          ELSE                                                     EL856
00216              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL856
00217              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL856
00218              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL856
00219              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL856
00220              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL856
00221              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL856
00222              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL856
00223              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL856
00224                                                                   EL856
00225      MOVE LOW-VALUES             TO EL856AI.                      EL856
00226                                                                   EL856
00227      EXEC CICS HANDLE CONDITION                                   EL856
00228          PGMIDERR  (9600-PGMID-ERROR)                             EL856
00229          ERROR     (9990-ABEND)                                   EL856
00230      END-EXEC.                                                    EL856
00231                                                                   EL856
00232      IF EIBTRNID = TRANS-EXD4                                     EL856
00233         MOVE SPACES              TO PI-PROGRAM-WORK-AREA          EL856
00234         MOVE PI-AR-SUMMARY-CODE  TO PI-SUMMARY-CODE               EL856
00235         MOVE -1                      TO SUMNAMEL                  EL856
00236         GO TO 3000-DISPLAY-SM-CROSS-REF.                          EL856
00237                                                                   EL856
00238      IF EIBTRNID NOT = TRANS-EXJ8                                 EL856
00239         MOVE SPACES              TO PI-PROGRAM-WORK-AREA          EL856
00240         MOVE -1                      TO SUMCODEL                  EL856
00241         GO TO 8100-SEND-INITIAL-MAP.                              EL856
00242                                                                   EL856
00243      IF EIBAID = DFHCLEAR                                         EL856
00244          GO TO 9400-CLEAR.                                        EL856
00245                                                                   EL856
00246                                                                   EL856
00247      IF PI-PROCESSOR-ID = 'LGXX'                                  EL856
00248          GO TO 0200-RECEIVE.                                      EL856
00249                                                                   EL856
00250      EXEC CICS READQ TS                                           EL856
00251          QUEUE  (QID)                                             EL856
00252          INTO   (SECURITY-CONTROL)                                EL856
00253          LENGTH (SC-COMM-LENGTH)                                  EL856
00254          ITEM   (SC-ITEM)                                         EL856
00255      END-EXEC.                                                    EL856
00256                                                                   EL856
00257      MOVE SC-CREDIT-DISPLAY (4)   TO PI-DISPLAY-CAP.              EL856
00258      MOVE SC-CREDIT-UPDATE  (4)   TO PI-MODIFY-CAP.               EL856
00259                                                                   EL856
00260      IF NOT DISPLAY-CAP                                           EL856
00261          MOVE 'READ'          TO SM-READ                          EL856
00262          PERFORM 9995-SECURITY-VIOLATION                          EL856
00263          MOVE ER-0070         TO  EMI-ERROR                       EL856
00264          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL856
00265          GO TO 8100-SEND-INITIAL-MAP.                             EL856
00266                                                                   EL856
00267      EJECT                                                        EL856
00268                                                                   EL856
00269 ******************************************************************EL856
00270 *                                                                *EL856
00271 *              R E C E I V E   M A P S                           *EL856
00272 *                                                                *EL856
00273 ******************************************************************EL856
00274                                                                   EL856
00275  0200-RECEIVE.                                                    EL856
00276                                                                   EL856
00277      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL856
00278          MOVE ER-0008            TO EMI-ERROR                     EL856
00279          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL856
00280          MOVE -1                 TO PFENTERL                      EL856
00281          GO TO 8200-SEND-DATAONLY.                                EL856
00282                                                                   EL856
00283      EXEC CICS RECEIVE                                            EL856
00284          MAP      (EL856A)                                        EL856
00285          MAPSET   (MAPSET-EL856S)                                 EL856
00286          INTO     (EL856AI)                                       EL856
00287      END-EXEC.                                                    EL856
00288                                                                   EL856
00289      IF PFENTERL = ZERO                                              CL**2
00290          GO TO 0300-CHECK-PFKEYS.                                    CL**2
00291                                                                      CL**2
00292      IF EIBAID NOT = DFHENTER                                        CL**2
00293          MOVE ER-0004          TO EMI-ERROR                          CL**2
00294          MOVE AL-UNBOF         TO PFENTERA                           CL**2
00295          MOVE -1               TO PFENTERL                           CL**2
00296          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**2
00297          GO TO 8200-SEND-DATAONLY.                                   CL**2
00298                                                                      CL**2
00299      IF (PFENTERI NUMERIC)                                           CL**2
00300          AND  (PFENTERI GREATER ZERO AND LESS THAN 25)               CL**2
00301          MOVE PF-VALUES (PFENTERI)  TO  EIBAID                       CL**2
00302      ELSE                                                            CL**2
00303          MOVE ER-0029          TO EMI-ERROR                          CL**2
00304          MOVE AL-UNBOF         TO PFENTERA                           CL**2
00305          MOVE -1               TO PFENTERL                           CL**2
00306          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**2
00307          GO TO 8200-SEND-DATAONLY.                                   CL**2
00308                                                                   EL856
00309      EJECT                                                        EL856
00310                                                                   EL856
00311 ******************************************************************EL856
00312 *                                                                *EL856
00313 *              C H E C K   P F K E Y S                           *EL856
00314 *                                                                *EL856
00315 ******************************************************************EL856
00316                                                                   EL856
00317  0300-CHECK-PFKEYS.                                               EL856
00318                                                                   EL856
00319      IF EIBAID = DFHPF23                                          EL856
00320          GO TO 8810-PF23.                                         EL856
00321                                                                   EL856
00322      IF EIBAID = DFHPF24                                          EL856
00323          GO TO 9200-RETURN-MAIN-MENU.                             EL856
00324                                                                   EL856
00325      IF EIBAID = DFHPF12                                          EL856
00326          GO TO 9500-PF12.                                         EL856
00327                                                                   EL856
00328      IF EIBAID = DFHENTER                                         EL856
00329          GO TO 1000-EDIT-MAP.                                     EL856
00330                                                                   EL856
00331      IF EIBAID = DFHPF1                                           EL856
00332         IF PI-END-OF-FILE                                         EL856
00333            MOVE ER-2237 TO EMI-ERROR                              EL856
00334            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL856
00335            GO TO 8200-SEND-DATAONLY                               EL856
00336         ELSE                                                      EL856
00337            GO TO 3000-DISPLAY-SM-CROSS-REF.                       EL856
00338                                                                   EL856
00339      IF EIBAID = DFHPF2                                           EL856
00340         IF PI-TOP-OF-FILE                                         EL856
00341            MOVE ER-2238 TO EMI-ERROR                              EL856
00342            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL856
00343            GO TO 8200-SEND-DATAONLY                               EL856
00344         ELSE                                                      EL856
00345            PERFORM 4100-READ-PREV-SUMMARY-CODE THRU 4190-EXIT     EL856
00346            GO TO 3000-DISPLAY-SM-CROSS-REF.                       EL856
00347                                                                   EL856
00348      IF EIBAID = DFHPF3                                           EL856
00349         IF PI-END-OF-FILE                                         EL856
00350            MOVE ER-2237 TO EMI-ERROR                              EL856
00351            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL856
00352            GO TO 8200-SEND-DATAONLY                               EL856
00353         ELSE                                                      EL856
00354            GO TO 3000-DISPLAY-SM-CROSS-REF.                       EL856
00355                                                                   EL856
00356      IF EIBAID = DFHPF4                                           EL856
00357         IF PI-TOP-OF-FILE                                         EL856
00358            MOVE ER-2238 TO EMI-ERROR                              EL856
00359            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL856
00360            GO TO 8200-SEND-DATAONLY                               EL856
00361         ELSE                                                      EL856
00362            GO TO 3100-DISPLAY-PREV-SM-CROSS-REF.                  EL856
00363                                                                   EL856
00364      MOVE ER-0008 TO EMI-ERROR.                                   EL856
00365      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL856
00366      MOVE -1                     TO PFENTERL.                     EL856
00367      GO TO 8200-SEND-DATAONLY.                                    EL856
00368                                                                   EL856
00369      EJECT                                                        EL856
00370                                                                   EL856
00371 ******************************************************************EL856
00372 *                                                                *EL856
00373 *                  E D I T    M A P                              *EL856
00374 *                                                                *EL856
00375 ******************************************************************EL856
00376                                                                   EL856
00377  1000-EDIT-MAP.                                                   EL856
00378                                                                   EL856
00379                                                                   EL856
00380      IF SUMCODEL GREATER THAN ZEROS                               EL856
00381         MOVE SUMCODEI            TO PI-SUMMARY-CODE               EL856
00382         PERFORM 4200-READ-SUM-FILE THRU 4290-EXIT                 EL856
00383         MOVE AL-UANON            TO SUMCODEA                      EL856
00384      ELSE                                                         EL856
00385         MOVE ER-3135             TO EMI-ERROR                     EL856
00386         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL856
00387         MOVE -1                  TO SUMNAMEL                      EL856
00388         GO TO 8200-SEND-DATAONLY.                                 EL856
00389                                                                   EL856
00390      IF SUMNAMEL GREATER THAN ZEROS                               EL856
00391         PERFORM 2000-UPDATE-SUMMARY-NAME THRU 2090-EXIT.          EL856
00392                                                                   EL856
00393      GO TO 3000-DISPLAY-SM-CROSS-REF.                             EL856
00394                                                                   EL856
00395      EJECT                                                        EL856
00396                                                                   EL856
00397 ******************************************************************EL856
00398 *                                                                *EL856
00399 *        U P D A T E   S U M M A R Y   N A M E                   *EL856
00400 *                                                                *EL856
00401 ******************************************************************EL856
00402                                                                   EL856
00403  2000-UPDATE-SUMMARY-NAME.                                        EL856
00404                                                                   EL856
00405      MOVE LOW-VALUES             TO ERSUMM-KEY.                   EL856
00406      MOVE PI-COMPANY-CD          TO ERSUMM-COMPANY-CD.            EL856
00407      MOVE PI-SUMMARY-CODE        TO ERSUMM-SUMMARY.               EL856
00408                                                                   EL856
00409      EXEC CICS HANDLE CONDITION                                   EL856
00410          NOTFND   (2080-SUMMARY-NOTFND)                           EL856
00411      END-EXEC.                                                    EL856
00412                                                                   EL856
00413      EXEC CICS READ                                               EL856
00414          DATASET   (FILE-ID-ERSUMM)                               EL856
00415          SET       (ADDRESS OF SUMM-CROSS-REFERENCE)                 CL**3
00416          RIDFLD    (ERSUMM-KEY)                                   EL856
00417          UPDATE                                                   EL856
00418      END-EXEC.                                                    EL856
00419                                                                   EL856
00420      MOVE 'B'                    TO JP-RECORD-TYPE.               EL856
00421      MOVE SUMM-CROSS-REFERENCE   TO JP-RECORD-AREA.               EL856
00422                                                                   EL856
00423      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL856
00424                                                                   EL856
00425      MOVE SUMNAMEI               TO SX-SUMM-OR-AGT-NAME.          EL856
00426                                                                   EL856
00427      MOVE 'C'                    TO JP-RECORD-TYPE.               EL856
00428      MOVE SUMM-CROSS-REFERENCE   TO JP-RECORD-AREA.               EL856
00429                                                                   EL856
00430      EXEC CICS REWRITE                                            EL856
00431          DATASET (FILE-ID-ERSUMM)                                 EL856
00432          FROM    (SUMM-CROSS-REFERENCE)                           EL856
00433      END-EXEC.                                                    EL856
00434                                                                   EL856
00435      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL856
00436                                                                   EL856
00437      GO TO 2090-EXIT.                                             EL856
00438                                                                   EL856
00439  2080-SUMMARY-NOTFND.                                             EL856
00440                                                                   EL856
00441      MOVE ER-3133                TO EMI-ERROR.                    EL856
00442      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL856
00443      MOVE -1                     TO SUMNAMEL.                     EL856
00444      GO TO 8200-SEND-DATAONLY.                                    EL856
00445                                                                   EL856
00446  2090-EXIT.                                                       EL856
00447      EXIT.                                                        EL856
00448                                                                   EL856
00449      EJECT                                                        EL856
00450                                                                   EL856
00451 ******************************************************************EL856
00452 *                                                                *EL856
00453 *  D I S P L A Y   S U M M A R Y   C R O S S   R E F E R E N C E *EL856
00454 *                                                                *EL856
00455 ******************************************************************EL856
00456                                                                   EL856
00457  3000-DISPLAY-SM-CROSS-REF.                                       EL856
00458                                                                   EL856
00459      EXEC CICS HANDLE CONDITION                                   EL856
00460          NOTFND  (3080-REQUEST-NOTFND)                            EL856
00461      END-EXEC.                                                    EL856
00462                                                                   EL856
00463      IF EIBAID = DFHPF3                                           EL856
00464         IF PI-LST-SUM-KEY GREATER THAN SPACES                     EL856
00465            MOVE PI-LST-SUM-KEY   TO ERSUMM-KEY                    EL856
00466            GO TO 3010-PROCESS-SM-CROSS-REF                        EL856
00467         ELSE                                                      EL856
00468            MOVE PI-1ST-SUM-KEY   TO ERSUMM-KEY.                   EL856
00469                                                                   EL856
00470      IF EIBAID = DFHENTER                                         EL856
00471         MOVE LOW-VALUES       TO ERSUMM-KEY                       EL856
00472         MOVE PI-COMPANY-CD    TO ERSUMM-COMPANY-CD                EL856
00473         MOVE PI-SUMMARY-CODE  TO ERSUMM-SUMMARY.                  EL856
00474                                                                   EL856
00475      IF EIBAID = DFHPF1                                           EL856
00476         MOVE HIGH-VALUES      TO ERSUMM-KEY                       EL856
00477         MOVE PI-COMPANY-CD    TO ERSUMM-COMPANY-CD                EL856
00478         MOVE PI-SUMMARY-CODE  TO ERSUMM-SUMMARY.                  EL856
00479                                                                   EL856
00480  3010-PROCESS-SM-CROSS-REF.                                       EL856
00481                                                                   EL856
00482      EXEC CICS STARTBR                                            EL856
00483          DATASET (FILE-ID-ERSUMM)                                 EL856
00484          RIDFLD  (ERSUMM-KEY)                                     EL856
00485      END-EXEC.                                                    EL856
00486                                                                   EL856
00487      MOVE SPACE                  TO PI-END-OF-FILE-SW             EL856
00488                                     PI-TOP-OF-FILE-SW.            EL856
00489                                                                   EL856
00490      MOVE +0                     TO WS-SUB1.                      EL856
00491                                                                   EL856
00492  3020-READ-SUMMARY-FILE.                                          EL856
00493                                                                   EL856
00494      EXEC CICS HANDLE CONDITION                                   EL856
00495          ENDFILE (3060-END-OF-FILE)                               EL856
00496      END-EXEC.                                                    EL856
00497                                                                   EL856
00498      EXEC CICS READNEXT                                           EL856
00499          SET     (ADDRESS OF SUMM-CROSS-REFERENCE)                   CL**3
00500          DATASET (FILE-ID-ERSUMM)                                 EL856
00501          RIDFLD  (ERSUMM-KEY)                                     EL856
00502      END-EXEC.                                                    EL856
00503                                                                   EL856
00504      IF SX-COMPANY-CD NOT = PI-COMPANY-CD                         EL856
00505         GO TO 3060-END-OF-FILE.                                   EL856
00506                                                                   EL856
00507      IF PI-LST-SUM-KEY = ERSUMM-KEY                               EL856
00508         GO TO 3020-READ-SUMMARY-FILE.                             EL856
00509                                                                   EL856
00510      IF WS-BROWSE-STARTED                                         EL856
00511         IF SX-SUMMARY NOT = PI-SUMMARY-CODE                       EL856
00512            GO TO 3070-DISPLAY-PROCESSED.                          EL856
00513                                                                   EL856
00514      IF SX-CARRIER = LOW-VALUES                                   EL856
00515         MOVE SX-SUMMARY          TO PI-SUMMARY-CODE               EL856
00516         MOVE 'Y'                 TO WS-BROWSE-STARTED-SW          EL856
00517         GO TO 3020-READ-SUMMARY-FILE.                             EL856
00518                                                                   EL856
00519      IF WS-BROWSE-STARTED                                         EL856
00520         NEXT SENTENCE                                             EL856
00521      ELSE                                                         EL856
00522         MOVE 'Y'                 TO WS-BROWSE-STARTED-SW          EL856
00523         MOVE SX-SUMMARY          TO PI-SUMMARY-CODE.              EL856
00524                                                                   EL856
00525      ADD +1                      TO WS-SUB1.                      EL856
00526                                                                   EL856
00527      IF WS-SUB1 GREATER THAN +15                                  EL856
00528         GO TO 3070-DISPLAY-PROCESSED.                             EL856
00529                                                                   EL856
00530      IF WS-SUB1 = +1                                              EL856
00531         MOVE PI-SUMMARY-CODE     TO SUMCODEO                      EL856
00532         MOVE AL-UANON            TO SUMCODEA                      EL856
00533         MOVE ERSUMM-KEY          TO PI-1ST-SUM-KEY.               EL856
00534                                                                   EL856
00535      IF WS-SUB1 = +15                                             EL856
00536         MOVE ERSUMM-KEY          TO PI-LST-SUM-KEY.               EL856
00537                                                                   EL856
00538      MOVE PI-SUMMARY-CODE        TO SUMCODEO.                     EL856
00539      MOVE SX-CARRIER             TO SM-CAR        (WS-SUB1).      EL856
00540      MOVE SX-GROUP               TO SM-GRP        (WS-SUB1).      EL856
00541      MOVE SX-FIN-RESP            TO SM-FIN-RESP   (WS-SUB1).      EL856
00542      MOVE SX-ACCT-AGENT          TO SM-AGENT      (WS-SUB1).      EL856
00543      MOVE SX-SUMM-OR-AGT-NAME    TO SM-AGENT-NAME (WS-SUB1).      EL856
00544                                                                   EL856
00545      GO TO 3020-READ-SUMMARY-FILE.                                EL856
00546                                                                   EL856
00547  3060-END-OF-FILE.                                                EL856
00548                                                                   EL856
00549      MOVE 'Y'                    TO PI-END-OF-FILE-SW.            EL856
00550      MOVE ER-2237                TO EMI-ERROR.                    EL856
00551      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL856
00552                                                                   EL856
00553  3070-DISPLAY-PROCESSED.                                          EL856
00554                                                                   EL856
00555      EXEC CICS ENDBR                                              EL856
00556          DATASET (FILE-ID-ERSUMM)                                 EL856
00557      END-EXEC.                                                    EL856
00558                                                                   EL856
00559      MOVE PI-SUMMARY-CODE        TO SUMCODEO.                     EL856
00560      PERFORM 4200-READ-SUM-FILE THRU 4290-EXIT.                   EL856
00561      MOVE SX-SUMM-OR-AGT-NAME    TO SUMNAMEO.                     EL856
00562                                                                   EL856
00563      IF EIBTRNID = TRANS-EXD4                                     EL856
00564         MOVE -1                  TO SUMNAMEL                      EL856
00565      ELSE                                                         EL856
00566         MOVE -1                  TO SUMCODEL.                     EL856
00567                                                                   EL856
00568      GO TO 8100-SEND-INITIAL-MAP.                                 EL856
00569                                                                   EL856
00570  3080-REQUEST-NOTFND.                                             EL856
00571                                                                   EL856
00572      MOVE ER-3133                TO EMI-ERROR.                    EL856
00573      MOVE -1                     TO SUMCODEL.                     EL856
00574      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL856
00575      GO TO 8200-SEND-DATAONLY.                                    EL856
00576                                                                   EL856
00577  3090-EXIT.                                                       EL856
00578       EXIT.                                                       EL856
00579                                                                   EL856
00580      EJECT                                                        EL856
00581                                                                   EL856
00582 ******************************************************************EL856
00583 *                                                                *EL856
00584 *     D I S P L A Y   P R E V.   C R O S S   R E F E R E N C E   *EL856
00585 *                                                                *EL856
00586 ******************************************************************EL856
00587                                                                   EL856
00588  3100-DISPLAY-PREV-SM-CROSS-REF.                                  EL856
00589                                                                   EL856
00590      EXEC CICS HANDLE CONDITION                                   EL856
00591          NOTFND  (3180-REQUEST-NOTFND)                            EL856
00592      END-EXEC.                                                    EL856
00593                                                                   EL856
00594      MOVE PI-1ST-SUM-KEY        TO ERSUMM-KEY.                    EL856
00595                                                                   EL856
00596      EXEC CICS STARTBR                                            EL856
00597          DATASET (FILE-ID-ERSUMM)                                 EL856
00598          RIDFLD  (ERSUMM-KEY)                                     EL856
00599      END-EXEC.                                                    EL856
00600                                                                   EL856
00601      MOVE SPACE                  TO PI-END-OF-FILE-SW             EL856
00602                                     PI-TOP-OF-FILE-SW.            EL856
00603                                                                   EL856
00604      MOVE +16                    TO WS-SUB1.                      EL856
00605                                                                   EL856
00606  3110-READ-SUMMARY-FILE.                                          EL856
00607                                                                   EL856
00608      EXEC CICS HANDLE CONDITION                                   EL856
00609          ENDFILE (3160-END-OF-FILE)                               EL856
00610      END-EXEC.                                                    EL856
00611                                                                   EL856
00612      EXEC CICS READPREV                                           EL856
00613          SET     (ADDRESS OF SUMM-CROSS-REFERENCE)                   CL**3
00614          DATASET (FILE-ID-ERSUMM)                                 EL856
00615          RIDFLD  (ERSUMM-KEY)                                     EL856
00616      END-EXEC.                                                    EL856
00617                                                                   EL856
00618      IF SX-COMPANY-CD NOT = PI-COMPANY-CD                         EL856
00619         GO TO 3160-END-OF-FILE.                                   EL856
00620                                                                   EL856
00621      IF SX-SUMMARY LESS THAN PI-SUMMARY-CODE                      EL856
00622         GO TO 3170-DISPLAY-PROCESSED.                             EL856
00623                                                                   EL856
00624      IF SX-CARRIER = LOW-VALUES                                   EL856
00625         GO TO 3170-DISPLAY-PROCESSED.                             EL856
00626                                                                   EL856
00627      IF WS-BROWSE-STARTED                                         EL856
00628         NEXT SENTENCE                                             EL856
00629      ELSE                                                         EL856
00630         MOVE 'Y'                 TO WS-BROWSE-STARTED-SW          EL856
00631         MOVE ERSUMM-SUMMARY      TO PI-SUMMARY-CODE.              EL856
00632                                                                   EL856
00633      IF PI-1ST-SUM-KEY = ERSUMM-KEY                               EL856
00634         GO TO 3110-READ-SUMMARY-FILE.                             EL856
00635                                                                   EL856
00636      IF WS-SUB1 = +1                                              EL856
00637         MOVE ERSUMM-KEY          TO PI-1ST-SUM-KEY.               EL856
00638                                                                   EL856
00639      IF WS-SUB1 = +15                                             EL856
00640         MOVE ERSUMM-KEY          TO PI-LST-SUM-KEY.               EL856
00641                                                                   EL856
00642      SUBTRACT +1                 FROM WS-SUB1.                    EL856
00643                                                                   EL856
00644      IF WS-SUB1 LESS THAN +1                                      EL856
00645         GO TO 3170-DISPLAY-PROCESSED.                             EL856
00646                                                                   EL856
00647      MOVE PI-SUMMARY-CODE        TO SUMCODEO.                     EL856
00648      MOVE SX-CARRIER             TO SM-CAR        (WS-SUB1).      EL856
00649      MOVE SX-GROUP               TO SM-GRP        (WS-SUB1).      EL856
00650      MOVE SX-FIN-RESP            TO SM-FIN-RESP   (WS-SUB1).      EL856
00651      MOVE SX-ACCT-AGENT          TO SM-AGENT      (WS-SUB1).      EL856
00652      MOVE SX-SUMM-OR-AGT-NAME    TO SM-AGENT-NAME (WS-SUB1).      EL856
00653                                                                   EL856
00654      GO TO 3110-READ-SUMMARY-FILE.                                EL856
00655                                                                   EL856
00656  3160-END-OF-FILE.                                                EL856
00657                                                                   EL856
00658      MOVE 'Y'                    TO PI-TOP-OF-FILE-SW.            EL856
00659      MOVE ER-2238                TO EMI-ERROR.                    EL856
00660      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL856
00661                                                                   EL856
00662  3170-DISPLAY-PROCESSED.                                          EL856
00663                                                                   EL856
00664      EXEC CICS ENDBR                                              EL856
00665          DATASET (FILE-ID-ERSUMM)                                 EL856
00666      END-EXEC.                                                    EL856
00667                                                                   EL856
00668      PERFORM 4200-READ-SUM-FILE THRU 4290-EXIT.                   EL856
00669      MOVE SX-SUMM-OR-AGT-NAME TO SUMNAMEO.                        EL856
00670                                                                   EL856
00671      MOVE -1                     TO SUMCODEL.                     EL856
00672                                                                   EL856
00673      GO TO 8100-SEND-INITIAL-MAP.                                 EL856
00674                                                                   EL856
00675  3180-REQUEST-NOTFND.                                             EL856
00676                                                                   EL856
00677      MOVE ERSUMM-SUMMARY         TO SUMCODEO.                     EL856
00678      MOVE AL-UANON               TO SUMCODEA.                     EL856
00679      MOVE ER-2132                TO EMI-ERROR.                    EL856
00680      MOVE -1                     TO SUMCODEL.                     EL856
00681      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL856
00682      GO TO 8200-SEND-DATAONLY.                                    EL856
00683                                                                   EL856
00684  3190-EXIT.                                                       EL856
00685       EXIT.                                                       EL856
00686      EJECT                                                        EL856
00687                                                                   EL856
00688 ******************************************************************EL856
00689 *                                                                *EL856
00690 *         R E A D   P R E V   S U M M A R Y   C O D E            *EL856
00691 *                                                                *EL856
00692 ******************************************************************EL856
00693                                                                   EL856
00694  4100-READ-PREV-SUMMARY-CODE.                                     EL856
00695                                                                   EL856
00696      MOVE LOW-VALUES             TO ERSUMM-KEY.                   EL856
00697      MOVE PI-COMPANY-CD          TO ERSUMM-COMPANY-CD.            EL856
00698      MOVE PI-SUMMARY-CODE        TO ERSUMM-SUMMARY.               EL856
00699                                                                   EL856
00700      EXEC CICS HANDLE CONDITION                                   EL856
00701          NOTFND  (4180-SUMMARY-NOTFND)                            EL856
00702      END-EXEC.                                                    EL856
00703                                                                   EL856
00704      MOVE SPACE                  TO PI-END-OF-FILE-SW             EL856
00705                                     PI-TOP-OF-FILE-SW.            EL856
00706                                                                   EL856
00707      MOVE SPACE                  TO WS-BROWSE-STARTED-SW.         EL856
00708                                                                   EL856
00709      EXEC CICS STARTBR                                            EL856
00710          DATASET (FILE-ID-ERSUMM)                                 EL856
00711          RIDFLD  (ERSUMM-KEY)                                     EL856
00712      END-EXEC.                                                    EL856
00713                                                                   EL856
00714      EXEC CICS HANDLE CONDITION                                   EL856
00715          ENDFILE (4160-END-OF-FILE)                               EL856
00716      END-EXEC.                                                    EL856
00717                                                                   EL856
00718  4110-READ-SUMMARY-FILE.                                          EL856
00719                                                                   EL856
00720      EXEC CICS READPREV                                           EL856
00721          SET     (ADDRESS OF SUMM-CROSS-REFERENCE)                   CL**3
00722          DATASET (FILE-ID-ERSUMM)                                 EL856
00723          RIDFLD  (ERSUMM-KEY)                                     EL856
00724      END-EXEC.                                                    EL856
00725                                                                   EL856
00726      IF SX-COMPANY-CD NOT = PI-COMPANY-CD                         EL856
00727         GO TO 4160-END-OF-FILE.                                   EL856
00728                                                                   EL856
00729      IF WS-BROWSE-STARTED                                         EL856
00730         NEXT SENTENCE                                             EL856
00731      ELSE                                                         EL856
00732         MOVE 'Y'                 TO WS-BROWSE-STARTED-SW          EL856
00733         GO TO 4110-READ-SUMMARY-FILE.                             EL856
00734                                                                   EL856
00735      EXEC CICS ENDBR                                              EL856
00736          DATASET (FILE-ID-ERSUMM)                                 EL856
00737      END-EXEC.                                                    EL856
00738                                                                   EL856
00739      MOVE ERSUMM-SUMMARY         TO PI-SUMMARY-CODE.              EL856
00740                                                                   EL856
00741      MOVE LOW-VALUES             TO ERSUMM-KEY.                   EL856
00742      MOVE PI-COMPANY-CD          TO ERSUMM-COMPANY-CD.            EL856
00743      MOVE PI-SUMMARY-CODE        TO ERSUMM-SUMMARY.               EL856
00744                                                                   EL856
00745      EXEC CICS READ                                               EL856
00746          SET     (ADDRESS OF SUMM-CROSS-REFERENCE)                   CL**3
00747          DATASET (FILE-ID-ERSUMM)                                 EL856
00748          RIDFLD  (ERSUMM-KEY)                                     EL856
00749      END-EXEC.                                                    EL856
00750                                                                   EL856
00751      GO TO 4190-EXIT.                                             EL856
00752                                                                   EL856
00753  4160-END-OF-FILE.                                                EL856
00754                                                                   EL856
00755      EXEC CICS ENDBR                                              EL856
00756          DATASET (FILE-ID-ERSUMM)                                 EL856
00757      END-EXEC.                                                    EL856
00758                                                                   EL856
00759      MOVE 'Y'                    TO PI-TOP-OF-FILE-SW.            EL856
00760      MOVE ER-2238                TO EMI-ERROR.                    EL856
00761      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL856
00762      MOVE -1                    TO PFENTERL.                      EL856
00763      GO TO 8200-SEND-DATAONLY.                                    EL856
00764                                                                   EL856
00765  4180-SUMMARY-NOTFND.                                             EL856
00766                                                                   EL856
00767      MOVE ERSUMM-SUMMARY        TO SUMCODEO.                      EL856
00768      MOVE AL-UANON              TO SUMCODEA.                      EL856
00769      MOVE ER-3133               TO EMI-ERROR.                     EL856
00770      MOVE -1                    TO PFENTERL.                      EL856
00771      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL856
00772      GO TO 8200-SEND-DATAONLY.                                    EL856
00773                                                                   EL856
00774  4190-EXIT.                                                       EL856
00775      EXIT.                                                        EL856
00776                                                                   EL856
00777      EJECT                                                        EL856
00778                                                                   EL856
00779 ******************************************************************EL856
00780 *                                                                *EL856
00781 *             R E A D   S U M M A R Y   H E A D E R              *EL856
00782 *                                                                *EL856
00783 ******************************************************************EL856
00784                                                                   EL856
00785                                                                   EL856
00786  4200-READ-SUM-FILE.                                              EL856
00787                                                                   EL856
00788      MOVE LOW-VALUES             TO ERSUMM-KEY.                   EL856
00789      MOVE PI-COMPANY-CD          TO ERSUMM-COMPANY-CD.            EL856
00790      MOVE PI-SUMMARY-CODE        TO ERSUMM-SUMMARY.               EL856
00791                                                                   EL856
00792      EXEC CICS HANDLE CONDITION                                   EL856
00793          NOTFND   (4280-SUMMARY-NOTFND)                           EL856
00794      END-EXEC.                                                    EL856
00795                                                                   EL856
00796      EXEC CICS READ                                               EL856
00797          DATASET   (FILE-ID-ERSUMM)                               EL856
00798          SET       (ADDRESS OF SUMM-CROSS-REFERENCE)                 CL**3
00799          RIDFLD    (ERSUMM-KEY)                                   EL856
00800      END-EXEC.                                                    EL856
00801                                                                   EL856
00802      GO TO 4290-EXIT.                                             EL856
00803                                                                   EL856
00804  4280-SUMMARY-NOTFND.                                             EL856
00805                                                                   EL856
00806      MOVE -1                     TO SUMCODEL.                     EL856
00807      MOVE ER-3133                TO EMI-ERROR.                    EL856
00808      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL856
00809      GO TO 8200-SEND-DATAONLY.                                    EL856
00810                                                                   EL856
00811  4290-EXIT.                                                       EL856
00812      EXIT.                                                        EL856
00813                                                                   EL856
00814      EJECT                                                        EL856
00815                                                                   EL856
00816 ******************************************************************EL856
00817 *                                                                *EL856
00818 *            S  E N D    I N I T I A L   M A P                   *EL856
00819 *                                                                *EL856
00820 ******************************************************************EL856
00821                                                                   EL856
00822  8100-SEND-INITIAL-MAP.                                           EL856
00823                                                                   EL856
00824      MOVE EIBTIME                    TO TIME-IN.                  EL856
00825                                                                   EL856
00826      MOVE EMI-MESSAGE-AREA (1)       TO ERMESGO.                  EL856
00827      MOVE WS-CURRENT-DT              TO DATEO.                    EL856
00828      MOVE TIME-OUT                   TO TIMEO.                    EL856
00829                                                                   EL856
00830                                                                   EL856
00831      EXEC CICS SEND                                               EL856
00832          MAP      (EL856A)                                        EL856
00833          MAPSET   (MAPSET-EL856S)                                 EL856
00834          FROM     (EL856AI)                                       EL856
00835          ERASE                                                    EL856
00836          CURSOR                                                   EL856
00837      END-EXEC.                                                    EL856
00838                                                                   EL856
00839      GO TO 9100-RETURN-TRAN.                                      EL856
00840                                                                   EL856
00841      EJECT                                                        EL856
00842                                                                   EL856
00843 ******************************************************************EL856
00844 *                                                                *EL856
00845 *              S E N D    D A T A O N L Y                        *EL856
00846 *                                                                *EL856
00847 ******************************************************************EL856
00848                                                                   EL856
00849  8200-SEND-DATAONLY.                                              EL856
00850                                                                   EL856
00851      MOVE EIBTIME                TO TIME-IN.                      EL856
00852                                                                   EL856
00853      MOVE EMI-MESSAGE-AREA (1)       TO ERMESGO.                  EL856
00854      MOVE WS-CURRENT-DT              TO DATEO.                    EL856
00855      MOVE TIME-OUT                   TO TIMEO.                    EL856
00856      MOVE -1                         TO SUMCODEL.                 EL856
00857                                                                   EL856
00858      EXEC CICS SEND                                               EL856
00859           MAP      (EL856A)                                       EL856
00860           MAPSET   (MAPSET-EL856S)                                EL856
00861           FROM     (EL856AI)                                      EL856
00862           DATAONLY                                                EL856
00863           CURSOR                                                  EL856
00864      END-EXEC.                                                    EL856
00865                                                                   EL856
00866      GO TO 9100-RETURN-TRAN.                                      EL856
00867                                                                   EL856
00868      EJECT                                                        EL856
00869                                                                   EL856
00870  8300-SEND-TEXT.                                                  EL856
00871      EXEC CICS SEND TEXT                                          EL856
00872          FROM     (LOGOFF-TEXT)                                   EL856
00873          LENGTH   (LOGOFF-LENGTH)                                 EL856
00874          ERASE                                                    EL856
00875          FREEKB                                                   EL856
00876      END-EXEC.                                                    EL856
00877                                                                   EL856
00878      EXEC CICS RETURN                                             EL856
00879      END-EXEC.                                                    EL856
00880                                                                   EL856
00881                                                                   EL856
00882  8400-LOG-JOURNAL-RECORD.                                         EL856
00883      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL856
00884      MOVE THIS-PGM                TO JP-PROGRAM-ID.               EL856
00885                                                                   EL856
00886 *    EXEC CICS JOURNAL                                            EL856
00887 *        JFILEID     (PI-JOURNAL-FILE-ID)                         EL856
00888 *        JTYPEID     ('EL')                                       EL856
00889 *        FROM        (JOURNAL-RECORD)                             EL856
00890 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   EL856
00891 *        END-EXEC.                                                EL856
00892                                                                   EL856
00893  8500-DATE-CONVERT.                                               EL856
00894      EXEC CICS LINK                                               EL856
00895          PROGRAM  (LINK-ELDATCV)                                  EL856
00896          COMMAREA (DATE-CONVERSION-DATA)                          EL856
00897          LENGTH   (DC-COMM-LENGTH)                                EL856
00898      END-EXEC.                                                    EL856
00899                                                                   EL856
00900  8500-EXIT.                                                       EL856
00901      EXIT.                                                        EL856
00902                                                                   EL856
00903      EJECT                                                        EL856
00904                                                                   EL856
00905  8800-UNAUTHORIZED-ACCESS.                                        EL856
00906      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL856
00907      GO TO 8300-SEND-TEXT.                                        EL856
00908                                                                   EL856
00909  8810-PF23.                                                       EL856
00910      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL856
00911      MOVE XCTL-EL005             TO PGM-NAME.                     EL856
00912      GO TO 9300-XCTL.                                             EL856
00913                                                                   EL856
00914  9200-RETURN-MAIN-MENU.                                           EL856
00915      MOVE XCTL-EL626             TO PGM-NAME.                     EL856
00916      GO TO 9300-XCTL.                                             EL856
00917                                                                   EL856
00918  9000-RETURN-CICS.                                                EL856
00919      EXEC CICS RETURN                                             EL856
00920      END-EXEC.                                                    EL856
00921                                                                   EL856
00922  9100-RETURN-TRAN.                                                EL856
00923                                                                   EL856
00924      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL856
00925      MOVE '852A'                 TO PI-CURRENT-SCREEN-NO.         EL856
00926                                                                   EL856
00927      EXEC CICS RETURN                                             EL856
00928          TRANSID    (TRANS-EXJ8)                                  EL856
00929          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL856
00930          LENGTH     (PI-COMM-LENGTH)                              EL856
00931      END-EXEC.                                                    EL856
00932                                                                   EL856
00933  9300-XCTL.                                                       EL856
00934      EXEC CICS XCTL                                               EL856
00935          PROGRAM    (PGM-NAME)                                    EL856
00936          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL856
00937          LENGTH     (PI-COMM-LENGTH)                              EL856
00938      END-EXEC.                                                    EL856
00939                                                                   EL856
00940  9400-CLEAR.                                                      EL856
00941      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      EL856
00942      GO TO 9300-XCTL.                                             EL856
00943                                                                   EL856
00944  9500-PF12.                                                       EL856
00945      MOVE XCTL-EL010             TO PGM-NAME.                     EL856
00946      GO TO 9300-XCTL.                                             EL856
00947                                                                   EL856
00948  9600-PGMID-ERROR.                                                EL856
00949                                                                   EL856
00950      EXEC CICS HANDLE CONDITION                                   EL856
00951          PGMIDERR    (8300-SEND-TEXT)                             EL856
00952      END-EXEC.                                                    EL856
00953                                                                   EL856
00954      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL856
00955      MOVE ' '                    TO PI-ENTRY-CD-1.                EL856
00956      MOVE XCTL-EL005             TO PGM-NAME.                     EL856
00957      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL856
00958      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL856
00959      GO TO 9300-XCTL.                                             EL856
00960                                                                   EL856
00961  9900-ERROR-FORMAT.                                               EL856
00962                                                                   EL856
00963      IF NOT EMI-ERRORS-COMPLETE                                   EL856
00964          MOVE LINK-EL001         TO PGM-NAME                      EL856
00965          EXEC CICS LINK                                           EL856
00966              PROGRAM    (PGM-NAME)                                EL856
00967              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL856
00968              LENGTH     (EMI-COMM-LENGTH)                         EL856
00969          END-EXEC.                                                EL856
00970                                                                   EL856
00971  9900-EXIT.                                                       EL856
00972      EXIT.                                                        EL856
00973                                                                   EL856
00974  9990-ABEND.                                                      EL856
00975      MOVE LINK-EL004             TO PGM-NAME.                     EL856
00976      MOVE DFHEIBLK               TO EMI-LINE1.                    EL856
00977      EXEC CICS LINK                                               EL856
00978          PROGRAM   (PGM-NAME)                                     EL856
00979          COMMAREA  (EMI-LINE1)                                    EL856
00980          LENGTH    (72)                                           EL856
00981      END-EXEC.                                                    EL856
00982                                                                   EL856
00983      MOVE -1                     TO PFENTERL.                     EL856
00984                                                                   EL856
00985      GO TO 8200-SEND-DATAONLY.                                    EL856
00986                                                                   EL856
00987      GOBACK.                                                      EL856
00988                                                                   EL856
00989      EJECT                                                        EL856
00990                                                                   EL856
00991  9995-SECURITY-VIOLATION.                                         EL856
00992                              COPY ELCSCTP.                        EL856
00993                                                                   EL856
00994  9995-EXIT.                                                       EL856
00995      EXIT.                                                        EL856
00996                                                                   EL856
