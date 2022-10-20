00001  IDENTIFICATION DIVISION.                                         03/09/98
00002                                                                   ECS054
00003  PROGRAM-ID.                ECS054.                                  LV006
00004 *              PROGRAM CONVERTED BY                               ECS054
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS054
00006 *              CONVERSION DATE 02/08/96 10:02:27.                 ECS054
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS054
00008 *                           VMOD=2.008.                           ECS054
00009                                                                   ECS054
00010 *AUTHOR.     LOGIC, INC.                                          ECS054
00011 *            DALLAS, TEXAS.                                       ECS054
00012                                                                   ECS054
00013 *DATE-COMPILED.                                                   ECS054
00014                                                                   ECS054
00015 *SECURITY.   *****************************************************ECS054
00016 *            *                                                   *ECS054
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS054
00018 *            *                                                   *ECS054
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS054
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS054
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS054
00022 *            *                                                   *ECS054
00023 *            *****************************************************ECS054
00024                                                                   ECS054
00025 *REMARKS.                                                         ECS054
00026                                                                   ECS054
00027 *         PROGRAM PRINTS THE SORTED ALPHA LIST EXTRACTS.          ECS054
00028                                                                   ECS054
00029  ENVIRONMENT DIVISION.                                            ECS054
00030  INPUT-OUTPUT SECTION.                                            ECS054
00031  FILE-CONTROL.                                                    ECS054
00032      SELECT  XTRACT      ASSIGN TO SYS013-UT-2400-S-SYS013.       ECS054
00033      SELECT  DISK-DATE   ASSIGN TO SYS019-UT-FBA1-S-SYS019.       ECS054
00034      SELECT  PRINTS      ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS054
00035      SELECT  X-RECS      ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.      ECS054
00036      SELECT  FICH        ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS054
00037                                                                   ECS054
00038      EJECT                                                        ECS054
00039  DATA DIVISION.                                                   ECS054
00040  FILE SECTION.                                                    ECS054
00041  FD  XTRACT                                                       ECS054
00042                              COPY ECSAEXFD.                       ECS054
00043                              COPY ECSAEX01.                       ECS054
00044                                                                   ECS054
00045  FD  PRINTS                                                       ECS054
00046                              COPY ELCPRTFD.                       ECS054
00047                                                                   ECS054
00048  SD  X-RECS.                                                      ECS054
00049                                                                   ECS054
00050  01  SORT-X.                                                      ECS054
00051      12  XID.                                                     ECS054
00052          16  XNAME   PIC X(15).                                   ECS054
00053          16  XINIT.                                               ECS054
00054              20  XINIT1 PIC X.                                    ECS054
00055              20  XINIT2 PIC X.                                    ECS054
00056      12  XCARR       PIC X.                                       ECS054
00057      12  XST         PIC XX.                                      ECS054
00058      12  XGRP        PIC X(6).                                    ECS054
00059      12  XACCT       PIC X(10).                                   ECS054
00060      12  XDT         PIC 9(11)     COMP-3.                           CL**4
00061      12  XCERT       PIC X(11).                                   ECS054
00062      12  XTRM        PIC S999      COMP-3.                        ECS054
00063      12  XREMAIN     PIC S9(9)V99  COMP-3.                        ECS054
00064                                                                   ECS054
00065  FD  DISK-DATE                                                    ECS054
00066                              COPY ELCDTEFD.                       ECS054
00067                                                                   ECS054
00068  FD  FICH                                                         ECS054
00069                              COPY ECSFICH.                        ECS054
00070                                                                   ECS054
00071      EJECT                                                        ECS054
00072  WORKING-STORAGE SECTION.                                         ECS054
00073  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS054
00074  77  FILLER  PIC X(32) VALUE '********************************'.  ECS054
00075  77  FILLER  PIC X(32) VALUE '     ECS054 WORKING-STORAGE     '.  ECS054
00076  77  FILLER  PIC X(32) VALUE '***********VMOD=2.008***********'.  ECS054
00077                                                                   ECS054
00078  77  CK-COMP         PIC X,  VALUE SPACE.                         ECS054
00079  77  ENDSWT          PIC 9      VALUE 0.                          ECS054
00080  77  X               PIC X.                                       ECS054
00081  77  BLANK-COUNT     PIC S9(7)      VALUE ZERO   COMP-3.          ECS054
00082  77  PRT-SW          PIC X          VALUE SPACES.                 ECS054
00083                                                                   ECS054
00084  01  WS.                                                          ECS054
00085      12  WS-RETURN-CODE       PIC S9(4) COMP      VALUE +0.       ECS054
00086      12  WS-ABEND-MESSAGE     PIC  X(80)          VALUE SPACES.   ECS054
00087      12  WS-ABEND-FILE-STATUS PIC  XX             VALUE ZEROS.    ECS054
00088      12  WS-ZERO              PIC S9(1)           VALUE +0.       ECS054
00089      12  WS-XDT               PIC 9(11).                             CL**2
00090      12  WS-XDT-R REDEFINES WS-XDT.                                  CL**2
00091          16  FILLER  PIC 999.                                     ECS054
00092          16  XCCYY   PIC 9(04).                                   ECS054
00093          16  XCCYR   REDEFINES XCCYY.                             ECS054
00094              20  XCC PIC 99.                                      ECS054
00095              20  XYR PIC 99.                                      ECS054
00096          16  XMO     PIC 99.                                      ECS054
00097          16  XDA     PIC 99.                                      ECS054
00098                                                                   ECS054
00099  01  WORK-DATES.                                                  ECS054
00100      12  EXP-DATE            PIC S9(5)    COMP-3.                 ECS054
00101      12  CUR-DATE            PIC S9(5)    COMP-3.                 ECS054
00102                                                                   ECS054
00103                                                                   ECS054
00104  01  PAGEHOLDER SYNC.                                             ECS054
00105      03  PAGE-HOLD       OCCURS 134 TIMES.                        ECS054
00106          05  PH-NAME PIC X(15).                                   ECS054
00107          05  PH-INIT PIC XX.                                      ECS054
00108          05  PH-CARR PIC X.                                       ECS054
00109          05  PH-ST   PIC XX.                                      ECS054
00110          05  PH-GRP  PIC X(6).                                    ECS054
00111          05  PH-ACCT PIC X(10).                                   ECS054
00112          05  PH-DT.                                               ECS054
00113            07 PH-YR  PIC XX.                                      ECS054
00114            07 PH-MO  PIC XX.                                      ECS054
00115            07 PH-DA  PIC XX.                                      ECS054
00116          05  PH-CERT PIC X(11).                                   ECS054
00117  01  OPT-PAGEHOLDER REDEFINES PAGEHOLDER.                         ECS054
00118      03  OPT-PAGE-HOLD       OCCURS 134 TIMES.                    ECS054
00119          05  FILLER  PIC X(36).                                   ECS054
00120          05  PH-RAMT PIC S9(7)V99  COMP-3.                        ECS054
00121          05  FILLER  PIC X(12).                                   ECS054
00122                                                                   ECS054
00123  01  PG-SECTION SYNC.                                             ECS054
00124      03  PG-NAME     PIC X(15).                                   ECS054
00125      03  FILLER      PIC X.                                       ECS054
00126      03  PG-INIT     PIC XX.                                      ECS054
00127      03  FILLER      PIC XX.                                      ECS054
00128      03  PG-CARR     PIC X.                                       ECS054
00129      03  FILLER      PIC XX.                                      ECS054
00130      03  PG-GRP      PIC X(6).                                    ECS054
00131      03  FILLER      PIC X.                                       ECS054
00132      03  PG-ST       PIC XX.                                      ECS054
00133      03  FILLER      PIC X.                                       ECS054
00134      03  PG-ACCT     PIC X(10).                                   ECS054
00135      03  PG-DT.                                                   ECS054
00136          05  FILLER      PIC X.                                   ECS054
00137          05  PG-MO       PIC XX.                                  ECS054
00138          05  AST1        PIC X.                                   ECS054
00139          05  PG-DA       PIC XX.                                  ECS054
00140          05  AST2        PIC X.                                   ECS054
00141          05  PG-YR       PIC XX.                                  ECS054
00142      03  PG-REMAIN REDEFINES PG-DT.                               ECS054
00143          05  FILLER      PIC X.                                   ECS054
00144          05  PG-RAMT     PIC ZZZZ,ZZZ.                            ECS054
00145      03  FILLER      PIC X.                                       ECS054
00146      03  PG-CERT     PIC X(11).                                   ECS054
00147                                                                   ECS054
00148  01  PAGE-LINE SYNC.                                              ECS054
00149      03  SCT-1       PIC X(66).                                   ECS054
00150      03  SCT-2       PIC X(66).                                   ECS054
00151                                                                   ECS054
00152  01  HEAD-1.                                                      ECS054
00153      03  FILLER      PIC X(50)   VALUE SPACES.                    ECS054
00154      03  FILLER      PIC X(24)   VALUE 'CERTIFICATE ALPHA LISTIN'.ECS054
00155      03  FILLER      PIC X       VALUE 'G'.                       ECS054
00156      03  FILLER      PIC X(44)   VALUE SPACES.                    ECS054
00157      03  FILLER      PIC X(8)    VALUE 'ECS054  '.                ECS054
00158                                                                   ECS054
00159  01  HEAD-2.                                                      ECS054
00160      03  FILLER      PIC X(47)   VALUE SPACES.                    ECS054
00161      03  COM-NAM     PIC X(30).                                   ECS054
00162      03  FILLER      PIC X(42)   VALUE SPACES.                    ECS054
00163      03  H2-IPL      PIC X(8).                                    ECS054
00164                                                                   ECS054
00165  01  BINARY-COUNTERS            COMP.                             ECS054
00166      12  X1      PIC S999.                                        ECS054
00167      12  X2      PIC S999.                                        ECS054
00168      12  X3      PIC S999.                                        ECS054
00169      12  B1      PIC S999,    VALUE +1.                           ECS054
00170      12  MAX-RECS PIC S999    VALUE +102.                         ECS054
00171      12  COL2    PIC S999,    VALUE +51.                          ECS054
00172                                                                   ECS054
00173  01  HEAD-4.                                                      ECS054
00174      03  FILLER      PIC X(53)   VALUE SPACES.                    ECS054
00175      03  P-DATE      PIC X(18).                                   ECS054
00176      03  FILLER      PIC X(48)   VALUE SPACES.                    ECS054
00177      03  FILLER      PIC X(5)    VALUE 'PAGE '.                   ECS054
00178      03  P-NUM       PIC ZZ,ZZZ.                                  ECS054
00179                                                                   ECS054
00180  01  SUB-HEAD-1 SYNC.                                             ECS054
00181      03  FILLER      PIC X(66)  VALUE SPACES.                     ECS054
00182                                                                   ECS054
00183  01  SUB-HEAD-2 SYNC.                                             ECS054
00184      03  FILLER      PIC X(23)  VALUE 'NAME          INIT CAR '.  ECS054
00185      03  FILLER      PIC X(23)  VALUE 'GROUP  ST   ACCOUNT    '.  ECS054
00186      03  FILLER      PIC X(20)  VALUE 'DATE   CERT'.              ECS054
00187                                                                   ECS054
00188  01  OPT-2-SUB-HD1  SYNC.                                         ECS054
00189      03  FILLER  PIC X(45) VALUE SPACES.                          ECS054
00190      03  FILLER  PIC X(21) VALUE 'REMAIN.'.                       ECS054
00191  01  OPT-2-SUB-HD2  SYNC.                                         ECS054
00192      03  FILLER      PIC X(23)  VALUE 'NAME          INIT CAR '.  ECS054
00193      03  FILLER      PIC X(23)  VALUE 'GROUP  ST   ACCOUNT   A'.  ECS054
00194      03  FILLER      PIC X(20)  VALUE 'MOUNT  CERT'.              ECS054
00195  01  TAIL-LINE SYNC.                                              ECS054
00196      03  FILLER      PIC X(84)   VALUE SPACES.                    ECS054
00197      03  BEGIN-NAME  PIC X(15).                                   ECS054
00198      03  FILLER      PIC X       VALUE SPACES.                    ECS054
00199      03  B-INIT      PIC XX.                                      ECS054
00200      03  FILLER      PIC XXX     VALUE SPACES.                    ECS054
00201      03  FILLER      PIC X(6) VALUE ' ---- '.                     ECS054
00202      03  LAST-NAME   PIC X(15).                                   ECS054
00203      03  FILLER      PIC X       VALUE SPACES.                    ECS054
00204      03  L-INIT      PIC XX.                                      ECS054
00205      03  FILLER      PIC XXX     VALUE SPACES.                    ECS054
00206  01  PGM-SUB         PIC S999    COMP    VALUE +054.              ECS054
00207                                                                   ECS054
00208                              COPY ELCDTECX.                       ECS054
00209                                                                   ECS054
00210                              COPY ELCDTEVR.                       ECS054
00211                                                                   ECS054
00212  01  NUM-FIELDS SYNC.                                             ECS054
00213      03  REC-NUM     PIC S999 VALUE ZERO COMPUTATIONAL.           ECS054
00214      03  PG-CT       PIC S9(5) COMP-3  VALUE +0.                     CL**6
00215                                                                   ECS054
00216      EJECT                                                        ECS054
00217  PROCEDURE DIVISION.                                              ECS054
00218  0100-MAIN-LOOP SECTION.                                          ECS054
00219  0110-START--X.                                                   ECS054
00220                              COPY ELCDTERX.                       ECS054
00221      COMPUTE CUR-DATE = (RUN-CCYY * 12) + RUN-MO.                 ECS054
00222                                                                   ECS054
00223      IF FICH-ONLY                                                 ECS054
00224          MOVE +104 TO MAX-RECS                                    ECS054
00225          MOVE  +53 TO COL2.                                       ECS054
00226                                                                   ECS054
00227      MOVE ALPH-DATE       TO P-DATE.                                 CL**6
00228      MOVE COMPANY-NAME    TO COM-NAM.                             ECS054
00229      MOVE WS-CURRENT-DATE TO H2-IPL.                              ECS054
00230                                                                      CL**6
00231      IF DTE-PGM-OPT = '1' OR '9'                                  ECS054
00232          MOVE '1' TO PRT-SW.                                      ECS054
00233      IF DTE-PGM-OPT = '2'                                         ECS054
00234          MOVE '2' TO PRT-SW.                                      ECS054
00235      IF PRT-SW = '2'                                              ECS054
00236          MOVE OPT-2-SUB-HD1 TO SUB-HEAD-1                         ECS054
00237          MOVE OPT-2-SUB-HD2 TO SUB-HEAD-2.                        ECS054
00238                                                                   ECS054
00239      EJECT                                                        ECS054
00240  0120-SORT-EM             SECTION.                                ECS054
00241  0130-SORT-PARAGRAPH.                                             ECS054
00242      SORT X-RECS    ASCENDING KEY  XID  XACCT  XDT                ECS054
00243                     INPUT PROCEDURE 0150-MAKE-X                   ECS054
00244                     OUTPUT PROCEDURE 0190-PRINT-ALPH-EXT.         ECS054
00245  0140-E-SORT-SECT.                                                ECS054
00246                                                                   ECS054
00247      IF SORT-RETURN NOT = ZERO                                    ECS054
00248              MOVE '0101' TO WS-RETURN-CODE                        ECS054
00249              GO TO ABEND-PGM.                                     ECS054
00250                                                                   ECS054
00251      GO TO 0350-EOJ-SECT.                                         ECS054
00252                                                                   ECS054
00253  0150-MAKE-X              SECTION.                                ECS054
00254                                                                   ECS054
00255  0160-OPEN-EM.                                                    ECS054
00256      OPEN INPUT XTRACT.                                           ECS054
00257                                                                   ECS054
00258  0170-R-LOOP.                                                     ECS054
00259      READ XTRACT  AT END  GO TO 0180-E-MAKE-X.                    ECS054
00260                                                                   ECS054
00261      IF AX-JOINT-ALPHA OR AX-REIN-ALPHA OR AX-JOINT-REIN-ALPHA    ECS054
00262          GO TO 0170-R-LOOP.                                       ECS054
00263                                                                   ECS054
00264      IF AX-LNAME EQUAL SPACES                                     ECS054
00265          ADD +1 TO BLANK-COUNT                                    ECS054
00266          GO TO 0170-R-LOOP.                                       ECS054
00267                                                                   ECS054
00268      MOVE AX-LNAME   TO XNAME.                                    ECS054
00269      MOVE AX-1ST-INIT-FNAME TO XINIT1.                            ECS054
00270      MOVE AX-INIT    TO XINIT2.                                   ECS054
00271      MOVE AX-CARRIER TO XCARR.                                    ECS054
00272      MOVE AX-STATE   TO XST.                                      ECS054
00273      MOVE AX-GROUPING TO XGRP.                                    ECS054
00274      MOVE AX-ACCOUNT TO XACCT.                                    ECS054
00275      MOVE AX-DT      TO XDT                                       ECS054
00276                         WS-XDT.                                      CL**3
00277      MOVE AX-CERT-NO TO XCERT.                                    ECS054
00278      COMPUTE XREMAIN = AX-LF-REMAMT + AX-LF-REMAMT-ALT.           ECS054
00279                                                                   ECS054
00280      IF AX-LF-TERM GREATER THAN ZEROS                             ECS054
00281         MOVE AX-LF-TERM TO XTRM                                   ECS054
00282      ELSE                                                         ECS054
00283         MOVE AX-AH-TERM TO XTRM.                                  ECS054
00284                                                                   ECS054
00285      RELEASE SORT-X.                                              ECS054
00286                                                                   ECS054
00287      GO TO 0170-R-LOOP.                                           ECS054
00288                                                                   ECS054
00289  0180-E-MAKE-X.                                                   ECS054
00290      EXIT.                                                        ECS054
00291                                                                   ECS054
00292  0190-PRINT-ALPH-EXT SECTION.                                     ECS054
00293                                                                   ECS054
00294  0200-SET-ALPH.                                                   ECS054
00295      OPEN OUTPUT PRINTS.                                          ECS054
00296                                                                   ECS054
00297      CLOSE XTRACT.                                                ECS054
00298                                                                   ECS054
00299      IF BLANK-COUNT NOT = ZERO                                    ECS054
00300          MOVE SPACES TO PRT  MOVE '1' TO X                        ECS054
00301          PERFORM 0280-PUT-RTN THRU 0290-PUT-RTN-EXIT              ECS054
00302          DISPLAY '  ' BLANK-COUNT                                 ECS054
00303                  ' EXTRACTS OMITTED DUE TO BLANK NAME'.           ECS054
00304                                                                   ECS054
00305  0210-PROCESS.                                                    ECS054
00306      RETURN X-RECS AT END GO TO 0320-END-FILE.                    ECS054
00307                                                                   ECS054
CIDMOD     MOVE XDT TO WS-XDT.                                             CL**5
00308      COMPUTE EXP-DATE = (XCCYY * 12) + XMO + XTRM + 12.           ECS054
00309                                                                   ECS054
00310      IF EXP-DATE LESS THAN CUR-DATE                               ECS054
00311          GO TO 0210-PROCESS.                                      ECS054
00312                                                                   ECS054
00313  0220-PRO-3-A.                                                    ECS054
00314      ADD B1 TO REC-NUM.                                           ECS054
00315      IF REC-NUM = B1                                              ECS054
00316          MOVE SPACES TO PAGEHOLDER.                               ECS054
00317                                                                   ECS054
00318      MOVE XNAME TO PH-NAME (REC-NUM).                             ECS054
00319      MOVE XINIT TO PH-INIT (REC-NUM).                             ECS054
00320      MOVE XCARR TO PH-CARR (REC-NUM).                             ECS054
00321      MOVE XST TO PH-ST (REC-NUM).                                 ECS054
00322      MOVE XGRP  TO PH-GRP  (REC-NUM).                             ECS054
00323      MOVE XACCT TO PH-ACCT (REC-NUM).                             ECS054
00324                                                                   ECS054
00325      IF PRT-SW = '1'                                              ECS054
CIDMOD**       MOVE XDT TO WS-XDT                                          CL**5
00327          MOVE XMO TO PH-MO (REC-NUM)                              ECS054
00328          MOVE XDA TO PH-DA (REC-NUM)                              ECS054
00329          MOVE XYR TO PH-YR (REC-NUM)                              ECS054
00330        ELSE                                                       ECS054
00331          MOVE XREMAIN TO PH-RAMT (REC-NUM).                       ECS054
00332                                                                   ECS054
00333      MOVE XCERT TO PH-CERT (REC-NUM).                             ECS054
00334                                                                   ECS054
00335      IF REC-NUM = MAX-RECS                                        ECS054
00336          GO TO 0230-PRNT-PAGE-RTN.                                ECS054
00337                                                                   ECS054
00338      GO TO 0210-PROCESS.                                          ECS054
00339                                                                   ECS054
00340  0230-PRNT-PAGE-RTN.                                              ECS054
00341      MOVE '1' TO X.                                               ECS054
00342      MOVE HEAD-1 TO P-DATA.                                       ECS054
00343      PERFORM 0280-PUT-RTN THRU 0290-PUT-RTN-EXIT.                 ECS054
00344                                                                   ECS054
00345      MOVE ' ' TO X.                                               ECS054
00346      MOVE HEAD-2 TO P-DATA.                                       ECS054
00347      PERFORM 0280-PUT-RTN THRU 0290-PUT-RTN-EXIT.                 ECS054
00348                                                                   ECS054
00349      MOVE ' ' TO X.                                               ECS054
00350      ADD 1 TO PG-CT.                                              ECS054
00351      MOVE PG-CT TO P-NUM.                                         ECS054
00352      MOVE HEAD-4 TO P-DATA.                                       ECS054
00353      PERFORM 0280-PUT-RTN THRU 0290-PUT-RTN-EXIT.                 ECS054
00354                                                                   ECS054
00355      MOVE '0' TO X.                                               ECS054
00356      MOVE SUB-HEAD-1 TO SCT-1,                                    ECS054
00357                         SCT-2.                                    ECS054
00358      MOVE PAGE-LINE TO P-DATA.                                    ECS054
00359      PERFORM 0280-PUT-RTN THRU 0290-PUT-RTN-EXIT.                 ECS054
00360                                                                   ECS054
00361      MOVE SUB-HEAD-2 TO SCT-1,                                    ECS054
00362                         SCT-2.                                    ECS054
00363      MOVE PAGE-LINE TO P-DATA.                                    ECS054
00364      MOVE ' ' TO X.                                               ECS054
00365      PERFORM 0280-PUT-RTN THRU 0290-PUT-RTN-EXIT.                 ECS054
00366                                                                   ECS054
00367      MOVE '-' TO X.                                               ECS054
00368      MOVE B1 TO X1.                                               ECS054
00369      MOVE COL2 TO X2.                                             ECS054
00370                                                                   ECS054
00371  0240-LOAD-SECT-1.                                                ECS054
00372      MOVE SPACES TO PAGE-LINE PG-SECTION.                         ECS054
00373                                                                   ECS054
00374      IF X1 GREATER REC-NUM                                        ECS054
00375          GO TO 0250-LOAD-SECT-2.                                  ECS054
00376                                                                   ECS054
00377      MOVE PH-NAME (X1) TO PG-NAME.                                ECS054
00378      MOVE PH-INIT (X1) TO PG-INIT.                                ECS054
00379      MOVE PH-CARR (X1) TO PG-CARR.                                ECS054
00380      MOVE PH-ST   (X1) TO PG-ST.                                  ECS054
00381      MOVE PH-ACCT (X1) TO PG-ACCT.                                ECS054
00382      MOVE PH-GRP  (X1) TO PG-GRP.                                 ECS054
00383                                                                   ECS054
00384      IF PRT-SW = '1'                                              ECS054
00385          MOVE '-' TO AST1 AST2                                    ECS054
00386          MOVE PH-MO (X1) TO PG-MO                                 ECS054
00387          MOVE PH-DA (X1) TO PG-DA                                 ECS054
00388          MOVE PH-YR (X1) TO PG-YR                                 ECS054
00389        ELSE                                                       ECS054
00390          MOVE PH-RAMT (X1) TO PG-RAMT.                            ECS054
00391                                                                   ECS054
00392      MOVE PH-CERT (X1) TO PG-CERT.                                ECS054
00393      MOVE PG-SECTION   TO SCT-1.                                  ECS054
00394                                                                   ECS054
00395  0250-LOAD-SECT-2.                                                ECS054
00396      IF X2 GREATER REC-NUM                                        ECS054
00397          GO TO 0270-STOP-LOAD.                                    ECS054
00398                                                                   ECS054
00399      MOVE PH-NAME (X2) TO PG-NAME.                                ECS054
00400      MOVE PH-INIT (X2) TO PG-INIT.                                ECS054
00401      MOVE PH-CARR (X2) TO PG-CARR.                                ECS054
00402      MOVE PH-ST   (X2) TO PG-ST.                                  ECS054
00403      MOVE PH-GRP  (X2) TO PG-GRP.                                 ECS054
00404      MOVE PH-ACCT (X2) TO PG-ACCT.                                ECS054
00405                                                                   ECS054
00406      IF PRT-SW = '1'                                              ECS054
00407          MOVE '-' TO AST1 AST2                                    ECS054
00408          MOVE PH-MO (X2) TO PG-MO                                 ECS054
00409          MOVE PH-DA (X2) TO PG-DA                                 ECS054
00410          MOVE PH-YR (X2) TO PG-YR                                 ECS054
00411         ELSE                                                      ECS054
00412          MOVE PH-RAMT (X2) TO PG-RAMT.                            ECS054
00413                                                                   ECS054
00414      MOVE PH-CERT (X2) TO PG-CERT.                                ECS054
00415      MOVE PG-SECTION   TO SCT-2.                                  ECS054
00416                                                                   ECS054
00417                                                                   ECS054
00418  0270-STOP-LOAD.                                                  ECS054
00419      MOVE PAGE-LINE TO P-DATA.                                    ECS054
00420  0280-PUT-RTN.                                                    ECS054
00421                              COPY ELCPRT2.                        ECS054
00422  0290-PUT-RTN-EXIT.                                               ECS054
00423      EXIT.                                                        ECS054
00424                                                                   ECS054
00425  0300-CONTINUE.                                                   ECS054
00426      ADD B1 TO X1                                                 ECS054
00427                X2.                                                ECS054
00428      IF X2 GREATER THAN MAX-RECS                                  ECS054
00429          GO TO 0310-END-PRINT.                                    ECS054
00430                                                                   ECS054
00431      MOVE ' ' TO X.                                               ECS054
00432      GO TO 0240-LOAD-SECT-1.                                      ECS054
00433                                                                   ECS054
00434  0310-END-PRINT.                                                  ECS054
00435      MOVE '0' TO X.                                               ECS054
00436      MOVE PH-NAME (1)       TO BEGIN-NAME.                        ECS054
00437      MOVE PH-INIT (1)       TO B-INIT.                            ECS054
00438      MOVE PH-NAME (REC-NUM) TO LAST-NAME.                         ECS054
00439      MOVE PH-INIT (REC-NUM) TO L-INIT.                            ECS054
00440      MOVE ZEROS             TO REC-NUM.                           ECS054
00441      MOVE TAIL-LINE         TO P-DATA.                            ECS054
00442      PERFORM 0280-PUT-RTN THRU 0290-PUT-RTN-EXIT.                 ECS054
00443                                                                   ECS054
00444      MOVE SPACES            TO TAIL-LINE.                         ECS054
00445                                                                   ECS054
00446      IF ENDSWT = 1                                                ECS054
00447          GO TO 0330-END-RUN.                                      ECS054
00448                                                                   ECS054
00449      GO TO 0210-PROCESS.                                          ECS054
00450                                                                   ECS054
00451  0320-END-FILE.                                                   ECS054
00452      MOVE 1 TO ENDSWT.                                            ECS054
00453      GO TO 0230-PRNT-PAGE-RTN.                                    ECS054
00454                                                                   ECS054
00455  0330-END-RUN.                                                    ECS054
00456      MOVE '1' TO X.                                               ECS054
00457      MOVE SPACE TO P-DATA.                                        ECS054
00458      PERFORM 0280-PUT-RTN THRU 0290-PUT-RTN-EXIT.                 ECS054
00459                                                                   ECS054
00460  0340-E-PRT-SECT.                                                 ECS054
00461      EXIT.                                                        ECS054
00462                                                                   ECS054
00463  0350-EOJ-SECT            SECTION.                                ECS054
00464                                                                   ECS054
00465  0360-CLOSE-FICH.                                                 ECS054
00466                              COPY ELCPRTC.                        ECS054
00467      CLOSE PRINTS.                                                ECS054
00468                                                                   ECS054
00469  0370-E-R-X.                                                      ECS054
00470      GOBACK.                                                      ECS054
00471                                                                   ECS054
00472      EJECT                                                        ECS054
00473  ABEND-PGM SECTION.                                               ECS054
00474                         COPY ELCABEND SUPPRESS.                   ECS054
00475                                                                   ECS054
