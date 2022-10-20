00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL172
00003  PROGRAM-ID.                 EL172 .                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**5
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**5
00006 *              CONVERSION DATE 02/13/96 09:30:06.                    CL**5
00007 *                            VMOD=2.005                              CL**5
00008 *                                                                 EL172
00008 *                                                                 EL172
00009 *AUTHOR.     LOGIC,INC.                                              CL**5
00010 *            DALLAS, TEXAS.                                          CL**5
00011                                                                   EL172
00012 *DATE-COMPILED.                                                      CL**5
00013                                                                   EL172
00014 *SECURITY.   *****************************************************   CL**5
00015 *            *                                                   *   CL**5
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**5
00017 *            *                                                   *   CL**5
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**5
00019 *                                                                *   CL**5
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**5
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**5
00022 *            *                                                   *   CL**5
00023 *            *****************************************************   CL**5
00024                                                                   EL172
00025 *REMARKS.    TRANSACTION - EX02 - STATUS REPORT.                     CL**3
00026                                                                   EL172
00027  ENVIRONMENT DIVISION.                                            EL172
00028                                                                   EL172
00029      EJECT                                                        EL172
00030  DATA DIVISION.                                                   EL172
00031  WORKING-STORAGE SECTION.                                         EL172
00032  01  LCP-TIME-OF-DAY-XX.                                             CL**5
00033      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**5
00034      05  FILLER                    PIC 99.                           CL**5
00035  01  LCP-CICS-TIME                 PIC 9(15).                        CL**5
00036  77  FILLER  PIC X(32)  VALUE '********************************'. EL172
00037  77  FILLER  PIC X(32)  VALUE '*    EL172 WORKING STORAGE     *'. EL172
00038  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.005 *********'.    CL**5
00039                                                                   EL172
00040  01  WS-DATE-AREA.                                                EL172
00041      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL172
00042      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL172
00043                                                                   EL172
00044  01  STANDARD-AREAS.                                              EL172
00045      12  MAP-NAME.                                                EL172
00046          16  MAP-NAME-PR     PIC XX      VALUE  'EL'.             EL172
00047          16  MAP-NUMBER      PIC X(4)    VALUE '172A'.            EL172
00048          16  MAP-FILL        PIC XX      VALUE SPACES.            EL172
00049      12  MAPSET-NAME         PIC X(8)    VALUE 'EL172S  '.        EL172
00050      12  TRANS-ID            PIC X(4)    VALUE 'EX02'.            EL172
00051      12  LGXX-ID             PIC X(4)    VALUE 'LGXX'.            EL172
00052      12  PGM-NAME            PIC X(8).                            EL172
00053      12  TIME-IN             PIC S9(7).                           EL172
00054      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL172
00055          16  FILLER          PIC X.                               EL172
00056          16  TIME-OUT        PIC 99V99.                           EL172
00057          16  FILLER          PIC X(2).                            EL172
00058      12  XCTL-005            PIC X(5)    VALUE 'EL005'.           EL172
00059      12  XCTL-010            PIC X(5)    VALUE 'EL010'.           EL172
00060      12  XCTL-126            PIC X(5)    VALUE 'EL126'.           EL172
00061      12  LINK-001            PIC X(5)    VALUE 'EL001'.           EL172
00062      12  LINK-004            PIC X(5)    VALUE 'EL004'.           EL172
00063      12  LINK-ELDATCV        PIC X(7)    VALUE 'ELDATCV'.         EL172
00064      12  PGM-EL172           PIC X(8)    VALUE 'EL172   '.        EL172
00065      12  ELNTL-ID            PIC X(8)    VALUE 'ELCNTL  '.        EL172
00066      12  CLAIM-ID            PIC X(8)    VALUE 'ELMSTR  '.        EL172
00067      12  ACTV-ID             PIC X(8)    VALUE 'ELTRLR  '.        EL172
00068      12  REPT-FILE-ID        PIC X(8)    VALUE 'ELREPT  '.        EL172
00069      12  SUB                 PIC 99.                              EL172
00070      12  WS-RF-LINE-NO       PIC S9(8)   COMP.                    EL172
00071      12  NDX                 PIC S9(3) COMP-3.                    EL172
00072  01  ACCESS-KEYS-AND-TEMP-STOR.                                   EL172
00073      12  ELCNTL-KEY.                                              EL172
00074          16  CK-COMP-ID      PIC X(3).                            EL172
00075          16  FILLER          PIC X       VALUE 'R'.                  CL**2
00076          16  CK-PROC         PIC X(4)    VALUE SPACES.            EL172
00077          16  CK-SEQ          PIC S9(4)   VALUE +0    COMP.        EL172
00078      12  ACTIVITY-KEY.                                            EL172
00079        14  ACTV-PARTIAL-KEY.                                      EL172
00080          16  ACTV-COMP-CD        PIC X.                           EL172
00081          16  ACTV-CARRIER        PIC X.                           EL172
00082          16  ACTV-CLAIM-NO       PIC X(7).                        EL172
00083          16  ACTV-CERT-NO        PIC X(11).                       EL172
00084        14  ACTV-SEQ-NO         PIC S9(4)   COMP.                  EL172
00085                                                                   EL172
00086      12  CLAIM-PROCESSOR         PIC X(4).                        EL172
00087                                                                   EL172
00088      12  CLAIM-KEY               VALUE LOW-VALUES.                EL172
00089          16  CLAIM-COMP-CD       PIC X.                           EL172
00090          16  CLAIM-CARRIER       PIC X.                           EL172
00091          16  CLAIM-NUMBER        PIC X(7).                        EL172
00092          16  CLAIM-CERT-NO       PIC X(11).                       EL172
00093      12  CLAIM-KEY-SAVE          PIC X(20)   VALUE LOW-VALUES.    EL172
00094      12  ACTV-KEY-SAVE           PIC X(20).                       EL172
00095      12  TEMP-LENGTH             PIC S9(4)   COMP VALUE +1476.    EL172
00096      12  TEMP-IDB.                                                EL172
00097          16  T-IDB-TERM          PIC X(4).                        EL172
00098          16  FILLER              PIC X(4)    VALUE '172B'.        EL172
00099      12  TEMP-IDC.                                                EL172
00100          16  T-IDC-TERM          PIC X(4).                        EL172
00101          16  FILLER              PIC X(4)    VALUE '172C'.        EL172
00102      12  TEMP-IDD.                                                EL172
00103          16  T-IDD-TERM          PIC X(4).                        EL172
00104          16  FILLER              PIC X(4)    VALUE '172D'.        EL172
00105      12  TEMP-IDE.                                                EL172
00106          16  T-IDE-TERM          PIC X(4).                        EL172
00107          16  FILLER              PIC X(4)    VALUE '172E'.        EL172
00108      12  TEMP-IDF.                                                EL172
00109          16  T-IDF-TERM          PIC X(4).                        EL172
00110          16  FILLER              PIC X(4)    VALUE '172F'.        EL172
00111      12  TEMP-ITEM-B             PIC S9(4)   COMP VALUE +0.       EL172
00112      12  TEMP-ITEM-C             PIC S9(4)   COMP VALUE +0.       EL172
00113      12  TEMP-ITEM-D             PIC S9(4)   COMP VALUE +0.       EL172
00114      12  TEMP-ITEM-E             PIC S9(4)   COMP VALUE +0.       EL172
00115      12  TEMP-ITEM-F             PIC S9(4)   COMP VALUE +0.       EL172
00116      12  TEMP-STOR-ID            PIC X(8).                        EL172
00117      12  TEMP-ITEM-NO            PIC S9(4)   COMP.                EL172
00118      12  NEW-TEMP-ITEM           PIC S9(4)   COMP.                EL172
00119      12  NEW-TEMP-ID.                                             EL172
00120          16  NEW-TERM            PIC X(4).                        EL172
00121          16  FILLER              PIC X(4)    VALUE '172 '.        EL172
00122      12  NEW-TEMP-LENGTH         PIC S9(4)   COMP VALUE +1628.    EL172
00123      12  CURRENT-DATE-SAVE       PIC XX.                          EL172
00124      EJECT                                                        EL172
00125  01  ERROR-MESSAGES.                                              EL172
00126      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL172
00127      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL172
00128      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL172
00129      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL172
00130      12  ER-0042                 PIC X(4)  VALUE '0042'.          EL172
00131      12  ER-0047                 PIC X(4)  VALUE '0047'.          EL172
00132      12  ER-0066                 PIC X(4)  VALUE '0066'.          EL172
00133      12  ER-0067                 PIC X(4)  VALUE '0067'.          EL172
00134      12  ER-0157                 PIC X(4)  VALUE '0157'.          EL172
00135      12  ER-0172                 PIC X(4)  VALUE '0172'.          EL172
00136      12  ER-0270                 PIC X(4)  VALUE '0270'.          EL172
00137      12  ER-0281                 PIC X(4)  VALUE '0281'.          EL172
00138      12  ER-0305                 PIC X(4)  VALUE '0305'.          EL172
00139      12  ER-0415                 PIC X(4)  VALUE '0415'.          EL172
00140      12  ER-4003                 PIC X(4)  VALUE '4003'.          EL172
00141      EJECT                                                        EL172
00142  01  CLAIM-ACTION-HEADING.                                        EL172
00143      12  FILLER                  PIC X(79)                        EL172
00144          VALUE ' ACTION DATE   CLAIM NO  CARR     CERT NO    TYPE EL172
00145 -    '  LOC   NAME'.                                              EL172
00146  01  AUTO-PAY-HEADING.                                            EL172
00147      12  FILLER                  PIC X(79)                        EL172
00148          VALUE '  CLAIM NO  CARR   CERT NO       LAST PMT  FINAL  EL172
00149 -    '        SET UP     BY   LOC '.                              EL172
00150  01  ONE-MONTH-HEADING.                                           EL172
00151      12  FILLER                  PIC X(79)                        EL172
00152          VALUE '  CLAIM NO  CARR   CERT NO    TYPE  LAST MAINT    EL172
00153 -    'ACTION    REQ OUT?  LOC     '.                              EL172
00154  01  DELINQUENT-MAIL-HEADING.                                     EL172
00155      12  FILLER                  PIC X(79)                        EL172
00156          VALUE '  CLAIM NO  CARR   CERT NO    TYPE  SENT ON     BYEL172
00157 -    '    RE SEND   ARCHIVE   LOC '.                              EL172
00158  01  PRIORITY-STATUS-HEADING.                                     EL172
00159      12  FILLER                  PIC X(79)                        EL172
00160          VALUE '  CLAIM NO  CARR   CERT NO    TYPE  ESTABLISH   BYEL172
00161 -    '   LAST MAINT    LOC        '.                              EL172
00162      EJECT                                                        EL172
00163  01  CLAIM-ACTION-REMINDERS-SCREEN.                               EL172
00164      12 CLAIM-ACTION-PRINT-LINES OCCURS 6 TIMES INDEXED BY B-INDX.EL172
00165        14 B-LINE1.                                                EL172
00166          16  B-FILLER            PIC X(3).                        EL172
00167          16  FILLER              PIC X(3).                        EL172
00168          16  B-ACTION-DATE       PIC X(8).                        EL172
00169          16  FILLER              PIC X(4).                        EL172
00170          16  B-CLAIM-NO          PIC X(7).                        EL172
00171          16  FILLER              PIC X(4).                        EL172
00172          16  B-CARRIER           PIC X.                           EL172
00173          16  FILLER              PIC X(5).                        EL172
00174          16  B-CERT-NO           PIC X(11).                       EL172
00175          16  FILLER              PIC X(4).                        EL172
00176          16  B-TYPE              PIC X.                           EL172
00177          16  FILLER              PIC X(4).                        EL172
00178          16  B-LOCATION          PIC X(4).                        EL172
00179          16  FILLER              PIC X(2).                        EL172
00180          16  B-LAST-NAME         PIC X(15).                       EL172
00181          16  FILLER              PIC X(6).                        EL172
00182        14 B-LINE2.                                                EL172
00183          16  B-FILLER2           PIC X(3).                        EL172
00184          16  FILLER              PIC X(7).                        EL172
00185          16  B-PROMPT-1          PIC X(72).                       EL172
00186        14 B-LINE3.                                                EL172
00187          16  B-FILLER3           PIC X(3).                        EL172
00188          16  FILLER              PIC X(7).                        EL172
00189          16  B-PROMPT-2          PIC X(72).                       EL172
00190      EJECT                                                        EL172
00191  01  AUTO-PAY-SCREEN.                                             EL172
00192      12  AUTO-PAY-PRINT-LINES OCCURS 18 TIMES INDEXED BY C-INDX.  EL172
00193          16  C-FILLER            PIC X(3).                        EL172
00194          16  FILLER              PIC X(2).                        EL172
00195          16  C-CLAIM             PIC X(7).                        EL172
00196          16  FILLER              PIC X(4).                        EL172
00197          16  C-CARRIER           PIC X.                           EL172
00198          16  FILLER              PIC X(3).                        EL172
00199          16  C-CERT-NO           PIC X(11).                       EL172
00200          16  FILLER              PIC X(5).                        EL172
00201          16  C-LAST-PAY-DATE     PIC X(8).                        EL172
00202          16  FILLER              PIC X(3).                        EL172
00203          16  C-FINAL             PIC X(3).                        EL172
00204          16  FILLER              PIC X(10).                       EL172
00205          16  C-SET-UP-DATE       PIC X(8).                        EL172
00206          16  FILLER              PIC X(3).                        EL172
00207          16  C-BY                PIC X(4).                        EL172
00208          16  FILLER              PIC XX.                          EL172
00209          16  C-LOCATION          PIC X(4).                        EL172
00210          16  FILLER              PIC X.                           EL172
00211      EJECT                                                        EL172
00212  01  ONE-MONTH-INACTIVE-SCREEN.                                   EL172
00213      12  ONE-MONTH-PRINT-LINES OCCURS 18 TIMES INDEXED BY D-INDX. EL172
00214          16  D-FILLER            PIC X(3).                        EL172
00215          16  FILLER              PIC X(2).                        EL172
00216          16  D-CLAIM             PIC X(7).                        EL172
00217          16  FILLER              PIC X(4).                        EL172
00218          16  D-CARRIER           PIC X.                           EL172
00219          16  FILLER              PIC X(4).                        EL172
00220          16  D-CERT-NO           PIC X(11).                       EL172
00221          16  FILLER              PIC X(4).                        EL172
00222          16  D-TYPE              PIC X.                           EL172
00223          16  FILLER              PIC X(4).                        EL172
00224          16  D-LAST-MAINT-DATE   PIC X(8).                        EL172
00225          16  FILLER              PIC X(4).                        EL172
00226          16  D-ACTION            PIC X(8).                        EL172
00227          16  FILLER              PIC X(6).                        EL172
00228          16  D-Y-OR-N            PIC X.                           EL172
00229          16  FILLER              PIC X(6).                        EL172
00230          16  D-LOCATION          PIC X(4).                        EL172
00231          16  FILLER              PIC X(4).                           CL**4
00232      EJECT                                                        EL172
00233  01  DELINQUENT-MAIL-SCREEN.                                      EL172
00234      12  DELINQUENT-PRINT-LINES OCCURS 9 TIMES INDEXED BY E-INDX. EL172
00235        14 E-LINE1.                                                EL172
00236          16  E-FILLER            PIC X(3).                        EL172
00237          16  FILLER              PIC X(2).                        EL172
00238          16  E-CLAIM             PIC X(7).                        EL172
00239          16  FILLER              PIC X(4).                        EL172
00240          16  E-CARRIER           PIC X.                           EL172
00241          16  FILLER              PIC X(3).                        EL172
00242          16  E-CERT-NO           PIC X(11).                       EL172
00243          16  FILLER              PIC X(3).                        EL172
00244          16  E-TYPE              PIC X.                           EL172
00245          16  FILLER              PIC X(4).                        EL172
00246          16  E-SENT-DATE         PIC X(8).                        EL172
00247          16  FILLER              PIC X(3).                        EL172
00248          16  E-BY                PIC X(4).                        EL172
00249          16  FILLER              PIC X(3).                        EL172
00250          16  E-RESEND-DATE       PIC X(8).                        EL172
00251          16  FILLER              PIC X(2).                        EL172
00252          16  E-ARCHIVE           PIC X(6).                        EL172
00253          16  FILLER              PIC X(4).                        EL172
00254          16  E-LOCATION          PIC X(4).                        EL172
00255          16  FILLER              PIC X.                           EL172
00256       14 E-LINE2.                                                 EL172
00257          16  E-FILLER2           PIC X(3).                        EL172
00258          16  FILLER              PIC X(8).                        EL172
00259          16  E-REASON            PIC X(71).                       EL172
00260      EJECT                                                        EL172
00261  01  PRIORITY-STATUS-SCREEN.                                      EL172
00262      12  PRIORITY-PRINT-LINES OCCURS 18 TIMES INDEXED BY F-INDX.  EL172
00263          16  F-FILLER            PIC X(3).                        EL172
00264          16  FILLER              PIC X(2).                        EL172
00265          16  F-CLAIM             PIC X(7).                        EL172
00266          16  FILLER              PIC X(4).                        EL172
00267          16  F-CARRIER           PIC X.                           EL172
00268          16  FILLER              PIC X(3).                        EL172
00269          16  F-CERT-NO           PIC X(11).                       EL172
00270          16  FILLER              PIC X(3).                        EL172
00271          16  F-TYPE              PIC X.                           EL172
00272          16  FILLER              PIC X(4).                        EL172
00273          16  F-ESTAB-DATE        PIC X(8).                        EL172
00274          16  FILLER              PIC X(3).                        EL172
00275          16  F-USER              PIC X(4).                        EL172
00276          16  FILLER              PIC X(3).                        EL172
00277          16  F-LAST-MAINT-DATE   PIC X(8).                        EL172
00278          16  FILLER              PIC X(5).                        EL172
00279          16  F-LOCATION          PIC X(4).                        EL172
00280          16  FILLER              PIC X(8).                        EL172
00281      EJECT                                                        EL172
00282                              COPY ELCDATE.                           CL**4
00283      EJECT                                                        EL172
00284                              COPY ELCREPT.                           CL**4
00285      EJECT                                                        EL172
00286  01  REPORT-WORKING-STORAGE.                                      EL172
00287      03  PRT-CNT                 PIC S9(3)   VALUE +99  COMP-3.   EL172
00288      03  WS-LINE-NUMBER          PIC S9(7)   VALUE ZERO COMP-3.   EL172
00289      03  WS-PAGE                 PIC S9(5)   VALUE ZERO COMP-3.   EL172
00290      03  WS-REPORT-SW            PIC S9      VALUE ZERO COMP-3.   EL172
00291                                                                   EL172
00292      EJECT                                                        EL172
00293                              COPY ELCLOGOF.                          CL**4
00294                                                                   EL172
00295      EJECT                                                        EL172
00296                              COPY ELCATTR.                           CL**4
00297                                                                   EL172
00298      EJECT                                                        EL172
00299                              COPY ELCEMIB.                           CL**4
00300      EJECT                                                        EL172
00301                              COPY ELCINTF.                           CL**4
00302      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                EL172
00303          16  PI-USER             PIC X(4).                        EL172
00304          16  PI-TOTAL-SCREENS    PIC S9(4) COMP.                  EL172
00305          16  PI-CURRENT-SCREEN   PIC S9(4) COMP.                  EL172
00306          16  PI-SAVE-CURRENT-SCREEN   PIC S9(4) COMP.             EL172
00307          16  FILLER              PIC X(630).                         CL**5
00308                                                                   EL172
00309      EJECT                                                        EL172
00310                              COPY ELCAID.                            CL**4
00311  01  FILLER    REDEFINES DFHAID.                                  EL172
00312      12  FILLER              PIC X(8).                            EL172
00313      12  PF-VALUES           PIC X       OCCURS 2.                EL172
00314                                                                   EL172
00315      EJECT                                                        EL172
00316                              COPY EL172S.                            CL**4
00317                                                                   EL172
00318  01  MAP-REDEF REDEFINES EL172AI.                                 EL172
00319      12  P-TITLE             PIC X(70).                           EL172
00320      12  P-HEADING-LINE.                                          EL172
00321          16  FILLER              PIC X(3).                        EL172
00322          16  P-HEADING           PIC X(79).                       EL172
00323      12  P-LINES.                                                 EL172
00324        14  PRINT-LINES OCCURS 18 TIMES INDEXED BY P-INDX.         EL172
00325          16  FILLER              PIC X(3).                        EL172
00326          16  P-FLINE.                                             EL172
00327              18  FILLER          PIC X.                           EL172
00328              18  P-DATE          PIC X(8).                        EL172
00329              18  FILLER          PIC X(5).                        EL172
00330              18  P-TEXT          PIC X(50).                       EL172
00331              18  FILLER          PIC X(15).                       EL172
00332      EJECT                                                        EL172
00333  LINKAGE SECTION.                                                 EL172
00334  01  DFHCOMMAREA             PIC X(1024).                         EL172
00335                                                                   EL172
00336      EJECT                                                        EL172
00337 *01 PARMLIST .                                                       CL**5
00338 *    02  FILLER              PIC S9(8)   COMP.                       CL**5
00339 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL**5
00340 *    02  CLAIM-POINTER       PIC S9(8)   COMP.                       CL**5
00341 *    02  ACTV-POINTER        PIC S9(8)   COMP.                       CL**5
00342                                                                   EL172
00343                                  COPY ELCCNTL.                       CL**4
00344      EJECT                                                        EL172
00345                                  COPY ELCMSTR.                       CL**4
00346      EJECT                                                        EL172
00347                                  COPY ELCTRLR.                       CL**4
00348                                                                   EL172
00349      EJECT                                                        EL172
00350  PROCEDURE DIVISION.                                              EL172
00351                                                                   EL172
00352      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL172
00353      MOVE '5'                   TO DC-OPTION-CODE.                EL172
00354      PERFORM 9700-DATE-LINK.                                      EL172
00355      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL172
00356      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL172
00357                                                                   EL172
00358      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL172
00359      MOVE 1 TO EMI-NUMBER-OF-LINES.                               EL172
00360      IF EIBCALEN = 0                                              EL172
00361          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL172
00362                                                                   EL172
00363      MOVE EIBTRMID             TO NEW-TERM                        EL172
00364                                   T-IDB-TERM                      EL172
00365                                   T-IDC-TERM                      EL172
00366                                   T-IDD-TERM                      EL172
00367                                   T-IDE-TERM                      EL172
00368                                   T-IDF-TERM.                     EL172
00369                                                                   EL172
00370      IF PI-CALLING-PROGRAM NOT = PGM-EL172                        EL172
00371          IF PI-RETURN-TO-PROGRAM NOT = PGM-EL172                  EL172
00372              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL172
00373              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL172
00374              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL172
00375              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL172
00376              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL172
00377              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL172
00378              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL172
00379              MOVE PGM-EL172 TO PI-CALLING-PROGRAM                 EL172
00380              MOVE LOW-VALUES TO EL172AO                           EL172
00381              MOVE ZEROS TO PI-TOTAL-SCREENS                       EL172
00382                            PI-CURRENT-SCREEN                      EL172
00383              PERFORM 6600-DELETE-TEMP-STOR THRU 6699-EXIT         EL172
00384              GO TO 5000-BUILD-SCREENS.                            EL172
00385                                                                   EL172
00386      EXEC CICS HANDLE CONDITION                                   EL172
00387          PGMIDERR(9600-PGMID-ERROR)                               EL172
00388          ERROR(9990-ABEND)                                        EL172
00389          END-EXEC.                                                EL172
00390                                                                   EL172
00391      IF EIBAID = DFHCLEAR                                         EL172
00392          GO TO 9400-CLEAR.                                        EL172
00393                                                                   EL172
00394      EJECT                                                        EL172
00395  0200-RECEIVE.                                                    EL172
00396      MOVE LOW-VALUES TO EL172AI.                                  EL172
00397      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL172
00398          MOVE ER-0008 TO EMI-ERROR                                EL172
00399          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL172
00400          MOVE -1 TO PAGEL                                         EL172
00401          GO TO 8200-SEND-DATAONLY.                                EL172
00402      EXEC CICS RECEIVE                                            EL172
00403          MAP(MAP-NAME)                                            EL172
00404          MAPSET(MAPSET-NAME)                                      EL172
00405          INTO(EL172AI)                                            EL172
00406          END-EXEC.                                                EL172
00407                                                                   EL172
00408      IF ENTERPFL = 0                                              EL172
00409          GO TO 0300-CHECK-PFKEYS.                                 EL172
00410      IF EIBAID NOT = DFHENTER                                     EL172
00411          MOVE ER-0004 TO EMI-ERROR                                EL172
00412          GO TO 0320-INPUT-ERROR.                                  EL172
00413      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)            EL172
00414          MOVE PF-VALUES (ENTERPFI) TO EIBAID                      EL172
00415      ELSE                                                         EL172
00416          MOVE ER-0029 TO EMI-ERROR                                EL172
00417          GO TO 0320-INPUT-ERROR.                                  EL172
00418                                                                   EL172
00419  0300-CHECK-PFKEYS.                                               EL172
00420      IF EIBAID = DFHPF23                                          EL172
00421          GO TO 8810-PF23.                                         EL172
00422      IF EIBAID = DFHPF24                                          EL172
00423          GO TO 9200-RETURN-MAIN-MENU.                             EL172
00424      IF EIBAID = DFHPF12                                          EL172
00425          GO TO 9500-PF12.                                         EL172
00426      IF EIBAID = DFHPF1                                           EL172
00427          GO TO 1000-FORWARD-PAGE.                                 EL172
00428      IF EIBAID = DFHPF2                                           EL172
00429          GO TO 2000-BACKWARD-PAGE.                                EL172
00430      IF EIBAID = DFHPF4                                           EL172
00431         MOVE 1                   TO PI-CURRENT-SCREEN             EL172
00432         GO TO 3000-PAGE-BUILD.                                    EL172
00433      IF EIBAID = DFHPF5                                           EL172
00434         MOVE PI-TOTAL-SCREENS    TO PI-CURRENT-SCREEN             EL172
00435         GO TO 3000-PAGE-BUILD.                                    EL172
00436                                                                   EL172
00437      IF EIBAID = DFHPF6                                           EL172
00438         GO TO 4000-PRINT-TEMP-STORAGE.                            EL172
00439                                                                   EL172
00440      IF EIBAID = DFHENTER                                         EL172
00441          GO TO 0330-EDIT-DATA.                                    EL172
00442      MOVE ER-0029 TO EMI-ERROR.                                   EL172
00443  0320-INPUT-ERROR.                                                EL172
00444      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL172
00445      MOVE AL-UNBON TO ENTERPFA.                                   EL172
00446      IF ENTERPFL = 0                                              EL172
00447          MOVE -1 TO PAGEL                                         EL172
00448      ELSE                                                         EL172
00449          MOVE -1 TO ENTERPFL.                                     EL172
00450      GO TO 8200-SEND-DATAONLY.                                    EL172
00451  0330-EDIT-DATA.                                                  EL172
00452      IF PAGEL = ZEROS                                             EL172
00453         MOVE -1 TO PAGEL                                          EL172
00454         GO TO 8200-SEND-DATAONLY.                                 EL172
00455      IF PAGEI NOT NUMERIC                                         EL172
00456         MOVE ER-0305             TO EMI-ERROR                     EL172
00457         MOVE -1                  TO PAGEL                         EL172
00458         MOVE AL-UNBON            TO PAGEA                         EL172
00459         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL172
00460         GO TO 8200-SEND-DATAONLY.                                 EL172
00461                                                                   EL172
00462      IF PAGEI GREATER THAN PI-TOTAL-SCREENS OR = 0                EL172
00463         MOVE ER-0305             TO EMI-ERROR                     EL172
00464         MOVE -1                  TO PAGEL                         EL172
00465         MOVE AL-UNBON            TO PAGEA                         EL172
00466         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL172
00467         GO TO 8200-SEND-DATAONLY.                                 EL172
00468      MOVE PAGEI                  TO PI-CURRENT-SCREEN.            EL172
00469      GO TO 3000-PAGE-BUILD.                                       EL172
00470      EJECT                                                        EL172
00471  1000-FORWARD-PAGE.                                               EL172
00472      IF PI-CURRENT-SCREEN = PI-TOTAL-SCREENS                      EL172
00473         MOVE ER-0066             TO EMI-ERROR                     EL172
00474         MOVE -1                  TO ENTERPFL                      EL172
00475         MOVE AL-UNBON            TO ENTERPFA                      EL172
00476         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL172
00477         GO TO 8200-SEND-DATAONLY.                                 EL172
00478      ADD 1                       TO PI-CURRENT-SCREEN.            EL172
00479      GO TO 3000-PAGE-BUILD.                                       EL172
00480                                                                   EL172
00481  2000-BACKWARD-PAGE.                                              EL172
00482      IF PI-CURRENT-SCREEN = 1                                     EL172
00483         MOVE ER-0067             TO EMI-ERROR                     EL172
00484         MOVE -1                  TO ENTERPFL                      EL172
00485         MOVE AL-UNBON            TO ENTERPFA                      EL172
00486         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL172
00487         GO TO 8200-SEND-DATAONLY.                                 EL172
00488      SUBTRACT 1                  FROM PI-CURRENT-SCREEN.          EL172
00489  3000-PAGE-BUILD.                                                 EL172
00490      IF PI-TOTAL-SCREENS = ZEROS                                  EL172
00491         MOVE ER-0047             TO EMI-ERROR                     EL172
00492         MOVE -1                  TO PAGEL                         EL172
00493         MOVE AL-UNBON            TO PAGEA                         EL172
00494         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL172
00495         GO TO 8200-SEND-DATAONLY.                                 EL172
00496      PERFORM 6300-GET-NEW-SCREENS.                                EL172
00497      GO TO 8100-SEND-INITIAL-MAP.                                 EL172
00498                                                                   EL172
00499      EJECT                                                        EL172
00500  4000-PRINT-TEMP-STORAGE.                                         EL172
00501      IF PI-TOTAL-SCREENS = 0                                      EL172
00502         MOVE ER-0000             TO EMI-ERROR                     EL172
00503         GO TO 0320-INPUT-ERROR.                                   EL172
00504                                                                   EL172
00505      EXEC CICS  HANDLE CONDITION                                  EL172
00506             NOTFND   (4510-WRITE-INITIAL-TRAILER)                 EL172
00507      END-EXEC.                                                    EL172
00508                                                                   EL172
00509      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL172
00510      MOVE 'RF'                   TO  RF-RECORD-ID.                EL172
00511      MOVE '2'                    TO  RF-RECORD-TYPE.              EL172
00512      MOVE 'EL172'                TO  RF-REPORT-ID.                EL172
00513      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL172
00514                                                                   EL172
00515      EXEC CICS READ                                               EL172
00516          DATASET (REPT-FILE-ID)                                   EL172
00517          INTO    (REPORT-SAVE-FILE)                               EL172
00518          RIDFLD  (RF-CONTROL-PRIMARY)                             EL172
00519          END-EXEC.                                                EL172
00520                                                                   EL172
00521      MOVE ER-4003                TO EMI-ERROR.                    EL172
00522      GO TO 0320-INPUT-ERROR.                                      EL172
00523                                                                   EL172
00524  4510-WRITE-INITIAL-TRAILER.                                      EL172
00525      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL172
00526      MOVE 'RF'                   TO  RF-RECORD-ID.                EL172
00527      MOVE '2'                    TO  RF-RECORD-TYPE.              EL172
00528      MOVE 'EL172'                TO  RF-REPORT-ID.                EL172
00529      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL172
00530      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL172
00531      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**5
00532      END-EXEC                                                        CL**5
00533      EXEC CICS FORMATTIME                                            CL**5
00534                ABSTIME(LCP-CICS-TIME)                                CL**5
00535                TIME(LCP-TIME-OF-DAY-XX)                              CL**5
00536      END-EXEC                                                        CL**5
00537      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL**5
00538      MOVE 'STARTED'              TO  RF-CURRENT-DATE.             EL172
00539                                                                   EL172
00540      EXEC CICS WRITE                                              EL172
00541          DATASET (REPT-FILE-ID)                                   EL172
00542          FROM    (REPORT-SAVE-FILE)                               EL172
00543          RIDFLD  (RF-CONTROL-PRIMARY)                             EL172
00544          END-EXEC.                                                EL172
00545     EJECT                                                         EL172
00546  4520-DELETE-REC.                                                 EL172
00547      MOVE 1 TO RF-LINE-NUMBER.                                    EL172
00548      EXEC CICS  HANDLE CONDITION                                  EL172
00549             NOTFND   (4540-DELETE-REC)                            EL172
00550      END-EXEC.                                                    EL172
00551  4530-DELETE-1.                                                   EL172
00552      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL172
00553      MOVE 'RF'       TO RF-RECORD-ID.                             EL172
00554      MOVE '1'        TO RF-RECORD-TYPE.                           EL172
00555      MOVE 'EL172'    TO RF-REPORT-ID.                             EL172
00556      EXEC CICS DELETE                                             EL172
00557          DATASET (REPT-FILE-ID)                                   EL172
00558          RIDFLD  (RF-CONTROL-PRIMARY)                             EL172
00559          KEYLENGTH (11)                                           EL172
00560          END-EXEC.                                                EL172
00561      ADD 1 TO RF-LINE-NUMBER.                                     EL172
00562      GO TO 4530-DELETE-1.                                         EL172
00563  4540-DELETE-REC.                                                 EL172
00564      EXEC CICS  HANDLE CONDITION                                  EL172
00565             NOTFND   (4600-PRINT-IT)                              EL172
00566      END-EXEC.                                                    EL172
00567  4550-DELETE-2.                                                   EL172
00568      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL172
00569      MOVE 'RF'       TO RF-RECORD-ID.                             EL172
00570      MOVE '2'        TO RF-RECORD-TYPE.                           EL172
00571      MOVE 'EL172'    TO RF-REPORT-ID.                             EL172
00572      EXEC CICS DELETE                                             EL172
00573          DATASET (REPT-FILE-ID)                                   EL172
00574          RIDFLD  (RF-CONTROL-PRIMARY)                             EL172
00575          KEYLENGTH (11)                                           EL172
00576          END-EXEC.                                                EL172
00577      ADD 1 TO RF-LINE-NUMBER.                                     EL172
00578      GO TO 4550-DELETE-2.                                         EL172
00579                                                                   EL172
00580  4600-PRINT-IT.                                                   EL172
00581      MOVE PI-CURRENT-SCREEN TO PI-SAVE-CURRENT-SCREEN.            EL172
00582      MOVE 1                 TO PI-CURRENT-SCREEN.                 EL172
00583      MOVE 0                 TO WS-RF-LINE-NO.                     EL172
00584                                                                   EL172
00585  4650-GET-SCREEN.                                                 EL172
00586      PERFORM 6300-GET-NEW-SCREENS.                                EL172
00587                                                                   EL172
00588      MOVE TITLEO    TO RF-DATA-133.                               EL172
00589      MOVE '1'  TO RF-CTL-CHAR-133.                                EL172
00590      PERFORM 4800-WRITE-CLREPT.                                   EL172
00591                                                                   EL172
00592      MOVE P-HEADING TO RF-DATA-133.                               EL172
00593      MOVE ' '  TO RF-CTL-CHAR-133.                                EL172
00594      PERFORM 4800-WRITE-CLREPT.                                   EL172
00595                                                                   EL172
00596      MOVE 0 TO NDX.                                               EL172
00597  4660-PRT-IT.                                                     EL172
00598      ADD 1 TO NDX.                                                EL172
00599      IF NDX GREATER 18                                            EL172
00600          GO TO 4670-CK-END.                                       EL172
00601      MOVE ' '  TO RF-CTL-CHAR-133.                                EL172
00602      MOVE P-FLINE (NDX)  TO RF-DATA-133.                          EL172
00603                                                                   EL172
00604      PERFORM 4800-WRITE-CLREPT.                                   EL172
00605      GO TO 4660-PRT-IT.                                           EL172
00606                                                                   EL172
00607  4670-CK-END.                                                     EL172
00608      IF PI-CURRENT-SCREEN LESS PI-TOTAL-SCREENS                   EL172
00609          ADD 1 TO PI-CURRENT-SCREEN                               EL172
00610          GO TO 4650-GET-SCREEN.                                   EL172
00611                                                                   EL172
00612  4700-END-CLREPT.                                                 EL172
00613      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL172
00614      MOVE 'RF'          TO RF-RECORD-ID.                          EL172
00615      MOVE '2'           TO RF-RECORD-TYPE.                        EL172
00616      MOVE 'EL172'       TO RF-REPORT-ID.                          EL172
00617      MOVE ZEROS         TO RF-LINE-NUMBER.                        EL172
00618                                                                   EL172
00619      EXEC CICS DELETE                                             EL172
00620          DATASET (REPT-FILE-ID)                                   EL172
00621          RIDFLD  (RF-CONTROL-PRIMARY)                             EL172
00622          KEYLENGTH (11)                                           EL172
00623          END-EXEC.                                                EL172
00624                                                                   EL172
00625  4750-WRITE-TRAILER.                                              EL172
00626      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL172
00627      MOVE 'RF'                   TO  RF-RECORD-ID.                EL172
00628      MOVE '2'                    TO  RF-RECORD-TYPE.              EL172
00629      MOVE 'EL172'                TO  RF-REPORT-ID.                EL172
00630      ADD +1  TO  WS-RF-LINE-NO.                                   EL172
00631      MOVE WS-RF-LINE-NO          TO  RF-LINE-NUMBER.              EL172
00632                                                                   EL172
00633      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL172
00634      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**5
00635      END-EXEC                                                        CL**5
00636      EXEC CICS FORMATTIME                                            CL**5
00637                ABSTIME(LCP-CICS-TIME)                                CL**5
00638                TIME(LCP-TIME-OF-DAY-XX)                              CL**5
00639      END-EXEC                                                        CL**5
00640      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL**5
00641      MOVE SAVE-DATE              TO  RF-CURRENT-DATE.             EL172
00642                                                                   EL172
00643      EXEC CICS WRITE                                              EL172
00644          DATASET (REPT-FILE-ID)                                   EL172
00645          FROM    (REPORT-SAVE-FILE)                               EL172
00646          RIDFLD  (RF-CONTROL-PRIMARY)                             EL172
00647          END-EXEC.                                                EL172
00648                                                                   EL172
00649      MOVE PI-SAVE-CURRENT-SCREEN TO PI-CURRENT-SCREEN.            EL172
00650      MOVE ER-0000                TO EMI-ERROR.                    EL172
00651      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL172
00652      GO TO 3000-PAGE-BUILD.                                       EL172
00653                                                                   EL172
00654  4800-WRITE-CLREPT.                                               EL172
00655      INSPECT RF-DATA-133 REPLACING ALL LOW-VALUES BY SPACES.         CL**5
00656      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                         EL172
00657      MOVE 'RF'          TO RF-RECORD-ID.                          EL172
00658      MOVE '1'           TO RF-RECORD-TYPE.                        EL172
00659      MOVE 'EL172'       TO RF-REPORT-ID.                          EL172
00660      ADD +1  TO  WS-RF-LINE-NO.                                   EL172
00661      MOVE WS-RF-LINE-NO          TO  RF-LINE-NUMBER.              EL172
00662                                                                   EL172
00663      EXEC CICS WRITE                                              EL172
00664          DATASET (REPT-FILE-ID)                                   EL172
00665          FROM    (REPORT-SAVE-FILE)                               EL172
00666          RIDFLD  (RF-CONTROL-PRIMARY)                             EL172
00667          END-EXEC.                                                EL172
00668  4800-EXIT.                                                       EL172
00669       EXIT.                                                       EL172
00670                                                                   EL172
00671      EJECT                                                        EL172
00672  5000-BUILD-SCREENS.                                              EL172
00673      MOVE '  -PERSONAL REMINDERS - ' TO TITLEI.                   EL172
00674      SET P-INDX TO 1.                                             EL172
00675      MOVE PI-COMPANY-ID          TO CK-COMP-ID.                   EL172
00676      IF PI-USER NOT = SPACES                                      EL172
00677         MOVE PI-USER             TO CK-PROC                       EL172
00678                                     CLAIM-PROCESSOR               EL172
00679         ELSE                                                      EL172
00680         MOVE PI-PROCESSOR-ID TO CK-PROC                           EL172
00681                                 CLAIM-PROCESSOR.                  EL172
00682      EXEC CICS HANDLE CONDITION                                   EL172
00683           NOTFND(8820-CNTL-NOT-FOUND)                             EL172
00684           NOTOPEN(8870-NOTOPEN)                                   EL172
00685           END-EXEC.                                               EL172
00686      EXEC CICS READ                                               EL172
00687           DATASET(ELNTL-ID)                                       EL172
00688           RIDFLD(ELCNTL-KEY)                                      EL172
00689           SET(ADDRESS OF CONTROL-FILE)                               CL**5
00690           END-EXEC.                                               EL172
00691      MOVE '    ON        ACTION' TO P-HEADING.                    EL172
00692      MOVE 1 TO SUB.                                               EL172
00693  5010-LOOP.                                                       EL172
00694      IF CF-END-REMIND-DT (SUB) NOT = LOW-VALUES                   EL172
00695         MOVE SPACES                 TO P-FLINE (P-INDX)           EL172
00696         MOVE CF-REMINDER-TEXT (SUB) TO P-TEXT (P-INDX)            EL172
00697         MOVE CF-END-REMIND-DT (SUB) TO DC-BIN-DATE-1              EL172
00698         MOVE SPACES                 TO DC-OPTION-CODE             EL172
00699         PERFORM 9700-DATE-LINK                                    EL172
00700         MOVE DC-GREG-DATE-1-EDIT    TO P-DATE (P-INDX)            EL172
00701         SET P-INDX UP BY 1.                                       EL172
00702                                                                   EL172
00703      IF SUB NOT = 6                                               EL172
00704         ADD 1 TO SUB                                              EL172
00705         GO TO 5010-LOOP.                                          EL172
00706      IF P-INDX = 1                                                EL172
00707         MOVE '** NO PERSONAL REMINDERS **'  TO P-FLINE (5).       EL172
00708                                                                   EL172
00709      MOVE 1                      TO NEW-TEMP-ITEM.                EL172
00710      EXEC CICS WRITEQ TS                                          EL172
00711           QUEUE(NEW-TEMP-ID)                                      EL172
00712           FROM(EL172AI)                                           EL172
00713           LENGTH(NEW-TEMP-LENGTH)                                 EL172
00714           ITEM(NEW-TEMP-ITEM)                                     EL172
00715           END-EXEC.                                               EL172
00716      ADD 1                       TO PI-TOTAL-SCREENS.             EL172
00717      MOVE 1                      TO PI-CURRENT-SCREEN.            EL172
00718      MOVE LOW-VALUES             TO CLAIM-ACTION-REMINDERS-SCREEN EL172
00719                                     AUTO-PAY-SCREEN               EL172
00720                                     ONE-MONTH-INACTIVE-SCREEN     EL172
00721                                     PRIORITY-STATUS-SCREEN        EL172
00722                                     DELINQUENT-MAIL-SCREEN        EL172
00723                                     EL172AO.                      EL172
00724      SET B-INDX TO 1.                                             EL172
00725      SET C-INDX TO 1.                                             EL172
00726      SET D-INDX TO 1.                                             EL172
00727      SET E-INDX TO 1.                                             EL172
00728      SET F-INDX TO 1.                                             EL172
00729                                                                   EL172
00730                                                                   EL172
00731      MOVE SAVE-BIN-DATE          TO CURRENT-DATE-SAVE.            EL172
00732                                                                   EL172
00733      MOVE PI-COMPANY-CD          TO CLAIM-COMP-CD.                EL172
00734      EXEC CICS HANDLE CONDITION                                   EL172
00735           NOTOPEN(8840-CLAIM-NOT-OPEN)                            EL172
00736           NOTFND(5200-NO-MORE-DATA)                               EL172
00737           ENDFILE(5200-NO-MORE-DATA)                              EL172
00738           END-EXEC.                                               EL172
00739      EXEC CICS STARTBR                                            EL172
00740           DATASET(CLAIM-ID)                                       EL172
00741           RIDFLD(CLAIM-KEY)                                       EL172
00742           END-EXEC.                                               EL172
00743      MOVE CLAIM-KEY TO CLAIM-KEY-SAVE.                            EL172
00744  5020-READ-NEXT-CLAIM.                                            EL172
00745      EXEC CICS HANDLE CONDITION                                   EL172
00746           NOTFND(5200-NO-MORE-DATA)                               EL172
00747           NOTOPEN(8840-CLAIM-NOT-OPEN)                            EL172
00748           ENDFILE(5200-NO-MORE-DATA)                              EL172
00749           DUPKEY(5025-CHECK-KEY)                                  EL172
00750           END-EXEC.                                               EL172
00751      EXEC CICS READNEXT                                           EL172
00752          SET(ADDRESS OF CLAIM-MASTER)                                CL**5
00753          DATASET(CLAIM-ID)                                        EL172
00754          RIDFLD(CLAIM-KEY)                                        EL172
00755          END-EXEC.                                                EL172
00756  5025-CHECK-KEY.                                                  EL172
00757                                                                   EL172
00758      IF CL-COMPANY-CD NOT EQUAL TO PI-COMPANY-CD                  EL172
00759          GO TO 5200-NO-MORE-DATA.                                 EL172
00760                                                                   EL172
00761      IF CLAIM-PROCESSOR NOT EQUAL TO CL-PROCESSOR-ID              EL172
00762          GO TO 5020-READ-NEXT-CLAIM.                              EL172
00763                                                                   EL172
00764      IF HIGHEST-PRIORITY                                          EL172
00765         PERFORM 5400-BUILD-PRIORITY-SCREEN THRU 5499-EXIT.        EL172
00766                                                                   EL172
00767      MOVE CL-LAST-MAINT-DT       TO DC-BIN-DATE-1.                EL172
00768      MOVE CURRENT-DATE-SAVE      TO DC-BIN-DATE-2.                EL172
00769      MOVE '1'                    TO DC-OPTION-CODE.               EL172
00770      PERFORM 9700-DATE-LINK.                                      EL172
00771      IF DC-ELAPSED-DAYS GREATER THAN 30                           EL172
00772        AND CLAIM-IS-OPEN                                          EL172
00773         PERFORM 5500-BUILD-ONE-MONTH-INACTIVE THRU 5599-EXIT.     EL172
00774                                                                      CL**5
00775      MOVE CL-COMPANY-CD          TO ACTV-COMP-CD.                 EL172
00776      MOVE CL-CARRIER             TO ACTV-CARRIER.                 EL172
00777      MOVE CL-CLAIM-NO            TO ACTV-CLAIM-NO.                EL172
00778      MOVE CL-CERT-NO             TO ACTV-CERT-NO.                 EL172
00779                                                                      CL**5
00780      IF CL-AUTO-PAY-SEQ NOT = ZEROS                               EL172
00781         PERFORM 5600-BUILD-AUTO-PAY THRU 5699-EXIT.               EL172
00782                                                                      CL**5
00783  5027-CHECK-FOLLOW-UP.                                            EL172
00784      IF CL-NEXT-FOLLOWUP-DT = LOW-VALUES                          EL172
00785         GO TO 5020-READ-NEXT-CLAIM.                               EL172
00786      MOVE 1                      TO ACTV-SEQ-NO.                  EL172
00787      MOVE ACTV-PARTIAL-KEY       TO ACTV-KEY-SAVE.                EL172
00788      EXEC CICS HANDLE CONDITION                                   EL172
00789           NOTOPEN(8850-ACTIVITY-NOT-OPEN)                         EL172
00790           ENDFILE(5040-END-BROWSE)                                EL172
00791           NOTFND(5040-END-BROWSE)                                 EL172
00792           END-EXEC.                                               EL172
00793      EXEC CICS STARTBR                                            EL172
00794           DATASET(ACTV-ID)                                        EL172
00795           RIDFLD(ACTIVITY-KEY)                                    EL172
00796           KEYLENGTH(17)                                           EL172
00797           GENERIC                                                 EL172
00798           GTEQ                                                    EL172
00799           END-EXEC.                                               EL172
00800  5030-READ-NEXT-ACTIVITY.                                         EL172
00801      EXEC CICS READNEXT                                           EL172
00802           SET(ADDRESS OF ACTIVITY-TRAILERS)                          CL**5
00803           DATASET(ACTV-ID)                                        EL172
00804           RIDFLD(ACTIVITY-KEY)                                    EL172
00805           END-EXEC.                                               EL172
00806      IF ACTV-PARTIAL-KEY NOT = ACTV-KEY-SAVE                      EL172
00807         GO TO 5040-END-BROWSE.                                    EL172
00808      IF AT-TRAILER-TYPE = '4'                                     EL172
00809         PERFORM 5700-BUILD-DELINQUENT-MAIL THRU 5799-EXIT         EL172
00810         GO TO 5030-READ-NEXT-ACTIVITY.                            EL172
00811      IF AT-TRAILER-TYPE = '7'                                     EL172
00812         PERFORM 5800-BUILD-CLAIM-ACTION THRU 5899-EXIT.           EL172
00813      GO TO 5030-READ-NEXT-ACTIVITY.                               EL172
00814                                                                   EL172
00815  5040-END-BROWSE.                                                 EL172
00816      EXEC CICS ENDBR                                              EL172
00817           DATASET(ACTV-ID)                                        EL172
00818           END-EXEC.                                               EL172
00819      GO TO 5020-READ-NEXT-CLAIM.                                  EL172
00820      EJECT                                                        EL172
00821  5200-NO-MORE-DATA.                                               EL172
00822      IF B-INDX = 1                                                EL172
00823         IF TEMP-ITEM-B = 0                                        EL172
00824            MOVE '** NO CLAIM ACTION REMINDERS **'  TO P-FLINE (5) EL172
00825            MOVE TEMP-IDB            TO TEMP-STOR-ID               EL172
00826            ADD 1                    TO TEMP-ITEM-B                EL172
00827            MOVE TEMP-ITEM-B         TO TEMP-ITEM-NO               EL172
00828            PERFORM 6000-PUT-TEMP-SCREENS                          EL172
00829            ELSE                                                   EL172
00830            NEXT SENTENCE                                          EL172
00831          ELSE                                                     EL172
00832          MOVE CLAIM-ACTION-REMINDERS-SCREEN TO P-LINES            EL172
00833          MOVE TEMP-IDB            TO TEMP-STOR-ID                 EL172
00834          ADD 1                    TO TEMP-ITEM-B                  EL172
00835          MOVE TEMP-ITEM-B         TO TEMP-ITEM-NO                 EL172
00836          PERFORM 6000-PUT-TEMP-SCREENS.                           EL172
00837                                                                   EL172
00838      IF C-INDX = 1                                                EL172
00839         IF TEMP-ITEM-C = 0                                        EL172
00840            MOVE '** NO AUTO PAY EXPIRATIONS **'  TO P-FLINE (5)   EL172
00841            MOVE TEMP-IDC            TO TEMP-STOR-ID               EL172
00842            ADD 1                    TO TEMP-ITEM-C                EL172
00843            MOVE TEMP-ITEM-C         TO TEMP-ITEM-NO               EL172
00844            PERFORM 6000-PUT-TEMP-SCREENS                          EL172
00845            ELSE                                                   EL172
00846            NEXT SENTENCE                                          EL172
00847          ELSE                                                     EL172
00848          MOVE AUTO-PAY-SCREEN     TO P-LINES                      EL172
00849          MOVE TEMP-IDC            TO TEMP-STOR-ID                 EL172
00850          ADD 1                    TO TEMP-ITEM-C                  EL172
00851          MOVE TEMP-ITEM-C         TO TEMP-ITEM-NO                 EL172
00852          PERFORM 6000-PUT-TEMP-SCREENS.                           EL172
00853                                                                   EL172
00854      IF D-INDX = 1                                                EL172
00855         IF TEMP-ITEM-D = 0                                        EL172
00856            MOVE '** NO INACTIVE RECORDS **'  TO P-FLINE (5)       EL172
00857            MOVE TEMP-IDD            TO TEMP-STOR-ID               EL172
00858            ADD 1                    TO TEMP-ITEM-D                EL172
00859            MOVE TEMP-ITEM-D         TO TEMP-ITEM-NO               EL172
00860            PERFORM 6000-PUT-TEMP-SCREENS                          EL172
00861            ELSE                                                   EL172
00862            NEXT SENTENCE                                          EL172
00863          ELSE                                                     EL172
00864          MOVE ONE-MONTH-INACTIVE-SCREEN TO P-LINES                EL172
00865          MOVE TEMP-IDD            TO TEMP-STOR-ID                 EL172
00866          ADD 1                    TO TEMP-ITEM-D                  EL172
00867          MOVE TEMP-ITEM-D         TO TEMP-ITEM-NO                 EL172
00868          PERFORM 6000-PUT-TEMP-SCREENS.                           EL172
00869                                                                   EL172
00870      IF E-INDX = 1                                                EL172
00871         IF TEMP-ITEM-E = 0                                        EL172
00872            MOVE '** NO DELINQUENT MAIL **'  TO P-FLINE (5)        EL172
00873            MOVE TEMP-IDE            TO TEMP-STOR-ID               EL172
00874            ADD 1                    TO TEMP-ITEM-E                EL172
00875            MOVE TEMP-ITEM-E         TO TEMP-ITEM-NO               EL172
00876            PERFORM 6000-PUT-TEMP-SCREENS                          EL172
00877            ELSE                                                   EL172
00878            NEXT SENTENCE                                          EL172
00879          ELSE                                                     EL172
00880          MOVE DELINQUENT-MAIL-SCREEN TO P-LINES                   EL172
00881          MOVE TEMP-IDE            TO TEMP-STOR-ID                 EL172
00882          ADD 1                    TO TEMP-ITEM-E                  EL172
00883          MOVE TEMP-ITEM-E         TO TEMP-ITEM-NO                 EL172
00884          PERFORM 6000-PUT-TEMP-SCREENS.                           EL172
00885                                                                   EL172
00886      IF F-INDX = 1                                                EL172
00887         IF TEMP-ITEM-F = 0                                        EL172
00888            MOVE '** NO PRIORITY *9* STATUS **'  TO P-FLINE (5)    EL172
00889            MOVE TEMP-IDF            TO TEMP-STOR-ID               EL172
00890            ADD 1                    TO TEMP-ITEM-F                EL172
00891            MOVE TEMP-ITEM-F         TO TEMP-ITEM-NO               EL172
00892            PERFORM 6000-PUT-TEMP-SCREENS                          EL172
00893            ELSE                                                   EL172
00894            NEXT SENTENCE                                          EL172
00895          ELSE                                                     EL172
00896          MOVE PRIORITY-STATUS-SCREEN TO P-LINES                   EL172
00897          MOVE TEMP-IDF            TO TEMP-STOR-ID                 EL172
00898          ADD 1                    TO TEMP-ITEM-F                  EL172
00899          MOVE TEMP-ITEM-F         TO TEMP-ITEM-NO                 EL172
00900          PERFORM 6000-PUT-TEMP-SCREENS.                           EL172
00901                                                                   EL172
00902                                                                   EL172
00903      MOVE TEMP-IDB               TO TEMP-STOR-ID.                 EL172
00904      MOVE ZEROS                  TO TEMP-ITEM-NO.                 EL172
00905      PERFORM 6500-GET-CLAIM-ACTION-SCREENS                        EL172
00906              TEMP-ITEM-B TIMES.                                   EL172
00907      MOVE TEMP-IDC               TO TEMP-STOR-ID.                 EL172
00908      MOVE ZEROS                  TO TEMP-ITEM-NO.                 EL172
00909      PERFORM 6510-GET-AUTO-PAY-SCREENS                            EL172
00910              TEMP-ITEM-C TIMES.                                   EL172
00911      MOVE TEMP-IDD               TO TEMP-STOR-ID.                 EL172
00912      MOVE ZEROS                  TO TEMP-ITEM-NO.                 EL172
00913      PERFORM 6520-GET-ONE-MONTH-SCREENS                           EL172
00914              TEMP-ITEM-D TIMES.                                   EL172
00915      MOVE TEMP-IDE               TO TEMP-STOR-ID.                 EL172
00916      MOVE ZEROS                  TO TEMP-ITEM-NO.                 EL172
00917      PERFORM 6530-GET-DELINQUENT-SCREENS                          EL172
00918              TEMP-ITEM-E TIMES.                                   EL172
00919      MOVE TEMP-IDF               TO TEMP-STOR-ID.                 EL172
00920      MOVE ZEROS                  TO TEMP-ITEM-NO.                 EL172
00921      PERFORM 6540-GET-PRIORITY-SCREENS                            EL172
00922              TEMP-ITEM-F TIMES.                                   EL172
00923      MOVE 1                      TO PI-CURRENT-SCREEN.            EL172
00924      PERFORM 6300-GET-NEW-SCREENS.                                EL172
00925      GO TO 8100-SEND-INITIAL-MAP.                                 EL172
00926      EJECT                                                        EL172
00927  5400-BUILD-PRIORITY-SCREEN.                                      EL172
00928      MOVE SPACES                 TO PRIORITY-PRINT-LINES (F-INDX).EL172
00929      MOVE LOW-VALUES             TO F-FILLER (F-INDX).            EL172
00930      MOVE CL-CLAIM-NO            TO F-CLAIM (F-INDX).             EL172
00931      MOVE CL-CARRIER             TO F-CARRIER (F-INDX).           EL172
00932      MOVE CL-CERT-NO             TO F-CERT-NO (F-INDX).           EL172
00933      MOVE CL-CLAIM-TYPE          TO F-TYPE (F-INDX).              EL172
00934      MOVE CL-PROCESSOR-ID        TO F-USER (F-INDX).              EL172
00935      MOVE CL-FILE-LOCATION       TO F-LOCATION (F-INDX).          EL172
00936      MOVE SPACES                 TO DC-OPTION-CODE.               EL172
00937      MOVE CL-FILE-ESTABLISH-DT   TO DC-BIN-DATE-1.                EL172
00938      PERFORM 9700-DATE-LINK.                                      EL172
00939      MOVE DC-GREG-DATE-1-EDIT    TO F-ESTAB-DATE (F-INDX).        EL172
00940      MOVE SPACES                 TO DC-OPTION-CODE.               EL172
00941      MOVE CL-LAST-MAINT-DT       TO DC-BIN-DATE-1.                EL172
00942      PERFORM 9700-DATE-LINK.                                      EL172
00943      MOVE DC-GREG-DATE-1-EDIT    TO F-LAST-MAINT-DATE (F-INDX).   EL172
00944      IF F-INDX = 18                                               EL172
00945         MOVE PRIORITY-STATUS-SCREEN TO P-LINES                    EL172
00946         MOVE TEMP-IDF               TO TEMP-STOR-ID               EL172
00947         ADD 1                       TO TEMP-ITEM-F                EL172
00948         MOVE TEMP-ITEM-F            TO TEMP-ITEM-NO               EL172
00949         PERFORM 6000-PUT-TEMP-SCREENS                             EL172
00950         SET F-INDX TO 1                                           EL172
00951         MOVE LOW-VALUES          TO PRIORITY-STATUS-SCREEN        EL172
00952         ELSE                                                      EL172
00953         SET F-INDX UP BY 1.                                       EL172
00954                                                                   EL172
00955  5499-EXIT.                                                       EL172
00956       EXIT.                                                       EL172
00957      EJECT                                                        EL172
00958  5500-BUILD-ONE-MONTH-INACTIVE.                                   EL172
00959      MOVE SPACES                TO ONE-MONTH-PRINT-LINES (D-INDX).EL172
00960      MOVE LOW-VALUES             TO D-FILLER (D-INDX).            EL172
00961      MOVE CL-CLAIM-NO            TO D-CLAIM (D-INDX).             EL172
00962      MOVE CL-CARRIER             TO D-CARRIER (D-INDX).           EL172
00963      MOVE CL-CERT-NO             TO D-CERT-NO (D-INDX).           EL172
00964      MOVE CL-CLAIM-TYPE          TO D-TYPE (D-INDX).              EL172
00965      MOVE CL-LAST-MAINT-DT       TO DC-BIN-DATE-1.                EL172
00966      MOVE SPACES                 TO DC-OPTION-CODE.               EL172
00967      PERFORM 9700-DATE-LINK.                                      EL172
00968      MOVE DC-GREG-DATE-1-EDIT    TO D-LAST-MAINT-DATE (D-INDX).   EL172
00969      IF CLAIM-SET-UP                                              EL172
00970         MOVE 'SET UP'            TO D-ACTION (D-INDX).            EL172
00971      IF PAYMENT-MADE                                              EL172
00972         MOVE 'PAYMENT'           TO D-ACTION (D-INDX).            EL172
00973      IF LETTER-SENT                                               EL172
00974         MOVE 'LETTER'            TO D-ACTION (D-INDX).            EL172
00975      IF MASTER-WAS-ALTERED                                        EL172
00976         MOVE 'ALTERED'           TO D-ACTION (D-INDX).            EL172
00977      IF INCURRED-DATE-CHANGED                                     EL172
00978          MOVE 'DATE CHG'         TO D-ACTION (D-INDX).            EL172
00979      IF FILE-CONVERTED                                            EL172
00980         MOVE 'CONVERT'           TO D-ACTION (D-INDX).            EL172
00981      IF CL-NEXT-FOLLOWUP-DT = LOW-VALUES                          EL172
00982         MOVE 'N'                 TO D-Y-OR-N (D-INDX)             EL172
00983         ELSE                                                      EL172
00984         MOVE 'Y'                 TO D-Y-OR-N (D-INDX).            EL172
00985      MOVE CL-FILE-LOCATION       TO D-LOCATION (D-INDX).          EL172
00986      IF D-INDX = 18                                               EL172
00987         MOVE ONE-MONTH-INACTIVE-SCREEN TO P-LINES                 EL172
00988         MOVE TEMP-IDD            TO TEMP-STOR-ID                  EL172
00989         ADD 1                    TO TEMP-ITEM-D                   EL172
00990         MOVE TEMP-ITEM-D         TO TEMP-ITEM-NO                  EL172
00991         PERFORM 6000-PUT-TEMP-SCREENS                             EL172
00992         SET D-INDX TO 1                                           EL172
00993         MOVE LOW-VALUES          TO ONE-MONTH-INACTIVE-SCREEN     EL172
00994         ELSE                                                      EL172
00995         SET D-INDX UP BY 1.                                       EL172
00996                                                                   EL172
00997  5599-EXIT.                                                       EL172
00998       EXIT.                                                       EL172
00999      EJECT                                                        EL172
01000  5600-BUILD-AUTO-PAY.                                             EL172
01001                                                                      CL**5
01002      MOVE CL-AUTO-PAY-SEQ        TO ACTV-SEQ-NO.                  EL172
01003                                                                      CL**5
01004      EXEC CICS HANDLE CONDITION                                   EL172
01005           NOTOPEN(8850-ACTIVITY-NOT-OPEN)                         EL172
01006           NOTFND(5690-AUTOPAY-NOT-FOUND)                             CL**5
01007           END-EXEC.                                               EL172
01008                                                                      CL**5
01009      EXEC CICS READ                                               EL172
01010           DATASET(ACTV-ID)                                        EL172
01011           RIDFLD(ACTIVITY-KEY)                                    EL172
01012           SET(ADDRESS OF ACTIVITY-TRAILERS)                          CL**5
01013           END-EXEC.                                               EL172
01014                                                                      CL**5
01015      MOVE CURRENT-DATE-SAVE      TO DC-BIN-DATE-1.                EL172
01016      MOVE AT-SCHEDULE-END-DT     TO DC-BIN-DATE-2.                EL172
01017      MOVE '1'                    TO DC-OPTION-CODE.               EL172
01018      PERFORM 9700-DATE-LINK.                                      EL172
01019      IF DC-ELAPSED-DAYS GREATER THAN 15                           EL172
01020         GO TO 5699-EXIT.                                          EL172
01021                                                                   EL172
01022      MOVE SPACES                 TO AUTO-PAY-PRINT-LINES (C-INDX).EL172
01023      MOVE LOW-VALUES             TO C-FILLER (C-INDX).            EL172
01024      MOVE CL-CLAIM-NO            TO C-CLAIM (C-INDX).             EL172
01025      MOVE CL-CARRIER             TO C-CARRIER (C-INDX).           EL172
01026      MOVE CL-CERT-NO             TO C-CERT-NO (C-INDX).           EL172
01027                                                                   EL172
01028      IF CL-LAST-PMT-DT = LOW-VALUES                               EL172
01029         MOVE 'NO'                TO C-FINAL (C-INDX)              EL172
01030         GO TO 5650-RECORD-DT.                                     EL172
01031                                                                   EL172
01032      MOVE CL-LAST-PMT-DT         TO DC-BIN-DATE-1.                EL172
01033      MOVE SPACES                 TO DC-OPTION-CODE.               EL172
01034      PERFORM 9700-DATE-LINK.                                      EL172
01035      MOVE DC-GREG-DATE-1-EDIT    TO C-LAST-PAY-DATE (C-INDX).     EL172
01036                                                                   EL172
01037      IF CL-LAST-PMT-DT NOT LESS THAN AT-SCHEDULE-END-DT           EL172
01038         IF LAST-PMT-IS-FINAL                                      EL172
01039            MOVE 'YES'            TO C-FINAL (C-INDX)              EL172
01040            ELSE                                                   EL172
01041            MOVE 'NO'             TO C-FINAL (C-INDX)              EL172
01042         ELSE                                                      EL172
01043         MOVE 'NO'                TO C-FINAL (C-INDX).             EL172
01044                                                                   EL172
01045  5650-RECORD-DT.                                                  EL172
01046                                                                      CL**3
01047      MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.                EL172
01048      MOVE SPACES                 TO DC-OPTION-CODE.               EL172
01049      PERFORM 9700-DATE-LINK.                                      EL172
01050      MOVE DC-GREG-DATE-1-EDIT    TO C-SET-UP-DATE (C-INDX).       EL172
01051      MOVE AT-RECORDED-BY         TO C-BY (C-INDX).                EL172
01052      MOVE CL-FILE-LOCATION       TO C-LOCATION (C-INDX).          EL172
01053                                                                   EL172
01054      IF C-INDX = 18                                               EL172
01055         MOVE AUTO-PAY-SCREEN     TO P-LINES                       EL172
01056         MOVE TEMP-IDC            TO TEMP-STOR-ID                  EL172
01057         ADD 1                    TO TEMP-ITEM-C                   EL172
01058         MOVE TEMP-ITEM-C         TO TEMP-ITEM-NO                  EL172
01059         PERFORM 6000-PUT-TEMP-SCREENS                             EL172
01060         SET C-INDX TO 1                                           EL172
01061         MOVE LOW-VALUES          TO AUTO-PAY-SCREEN               EL172
01062         ELSE                                                      EL172
01063         SET C-INDX UP BY 1.                                       EL172
01064                                                                   EL172
01065      GO TO 5699-EXIT.                                                CL**5
01066                                                                      CL**5
01067  5690-AUTOPAY-NOT-FOUND.                                             CL**5
01068                                                                      CL**5
01069      MOVE ER-0270                TO EMI-ERROR.                       CL**5
01070      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**5
01071      MOVE -1 TO PAGEL.                                               CL**5
01072                                                                      CL**5
01073  5699-EXIT.                                                       EL172
01074       EXIT.                                                       EL172
01075                                                                      CL**5
01076      EJECT                                                        EL172
01077  5700-BUILD-DELINQUENT-MAIL.                                      EL172
01078      IF AT-RECEIPT-FOLLOW-UP = LOW-VALUES                         EL172
01079         GO TO 5799-EXIT.                                          EL172
01080      IF AT-RECEIPT-FOLLOW-UP LESS THAN CURRENT-DATE-SAVE   AND    EL172
01081         AT-LETTER-ANSWERED-DT = LOW-VALUES                        EL172
01082         NEXT SENTENCE                                             EL172
01083         ELSE                                                      EL172
01084         GO TO 5799-EXIT.                                          EL172
01085                                                                   EL172
01086                                                                   EL172
01087      MOVE SPACES               TO DELINQUENT-PRINT-LINES (E-INDX).EL172
01088      MOVE LOW-VALUES             TO E-FILLER (E-INDX)             EL172
01089                                     E-FILLER2 (E-INDX).           EL172
01090      MOVE CL-CLAIM-NO            TO E-CLAIM (E-INDX).             EL172
01091      MOVE CL-CARRIER             TO E-CARRIER (E-INDX).           EL172
01092      MOVE CL-CERT-NO             TO E-CERT-NO (E-INDX).           EL172
01093      MOVE CL-CLAIM-TYPE          TO E-TYPE (E-INDX).              EL172
01094      MOVE AT-LETTER-SENT-DT      TO DC-BIN-DATE-1.                EL172
01095      MOVE SPACES                 TO DC-OPTION-CODE.               EL172
01096      PERFORM 9700-DATE-LINK.                                      EL172
01097      MOVE DC-GREG-DATE-1-EDIT    TO E-SENT-DATE (E-INDX).         EL172
01098      MOVE AT-RECORDED-BY         TO E-BY (E-INDX).                EL172
01099      MOVE AT-AUTO-RE-SEND-DT     TO DC-BIN-DATE-1.                EL172
01100      MOVE SPACES                 TO DC-OPTION-CODE.               EL172
01101      PERFORM 9700-DATE-LINK.                                      EL172
01102      MOVE DC-GREG-DATE-1-EDIT    TO E-RESEND-DATE (E-INDX).       EL172
01103      MOVE AT-LETTER-ARCHIVE-NO   TO E-ARCHIVE (E-INDX).           EL172
01104      MOVE CL-FILE-LOCATION       TO E-LOCATION (E-INDX).          EL172
01105      MOVE AT-REASON-TEXT         TO E-REASON (E-INDX).            EL172
01106      IF E-INDX = 9                                                EL172
01107         MOVE DELINQUENT-MAIL-SCREEN  TO P-LINES                   EL172
01108         MOVE TEMP-IDE            TO TEMP-STOR-ID                  EL172
01109         ADD 1                    TO TEMP-ITEM-E                   EL172
01110         MOVE TEMP-ITEM-C         TO TEMP-ITEM-NO                  EL172
01111         PERFORM 6000-PUT-TEMP-SCREENS                             EL172
01112         SET E-INDX TO 1                                           EL172
01113         MOVE LOW-VALUES          TO DELINQUENT-MAIL-SCREEN        EL172
01114         ELSE                                                      EL172
01115         SET E-INDX UP BY 1.                                       EL172
01116                                                                   EL172
01117  5799-EXIT.                                                       EL172
01118       EXIT.                                                       EL172
01119      EJECT                                                        EL172
01120  5800-BUILD-CLAIM-ACTION.                                         EL172
01121      IF AT-PROMPT-START-DT GREATER THAN SAVE-BIN-DATE             EL172
01122         GO TO 5899-EXIT.                                          EL172
01123                                                                   EL172
01124      IF AT-PROMPT-END-DT LESS THAN SAVE-BIN-DATE                  EL172
01125         GO TO 5899-EXIT.                                          EL172
01126                                                                   EL172
01127      MOVE SPACES             TO CLAIM-ACTION-PRINT-LINES (B-INDX).EL172
01128      MOVE LOW-VALUES             TO B-FILLER (B-INDX)             EL172
01129                                    B-FILLER2 (B-INDX)             EL172
01130                                    B-FILLER3 (B-INDX).            EL172
01131      MOVE CL-CLAIM-NO            TO B-CLAIM-NO (B-INDX).          EL172
01132      MOVE CL-CARRIER             TO B-CARRIER  (B-INDX).          EL172
01133      MOVE CL-CERT-NO             TO B-CERT-NO  (B-INDX).          EL172
01134      MOVE CL-CLAIM-TYPE          TO B-TYPE     (B-INDX).          EL172
01135      MOVE CL-FILE-LOCATION       TO B-LOCATION (B-INDX).          EL172
01136      MOVE CL-INSURED-LAST-NAME   TO B-LAST-NAME (B-INDX).         EL172
01137      IF AT-PROMPT-START-DT = SPACES OR LOW-VALUES                    CL**3
01138         MOVE SPACES TO B-ACTION-DATE (B-INDX)                        CL**3
01139         ELSE                                                         CL**3
01140         MOVE AT-PROMPT-START-DT     TO DC-BIN-DATE-1                 CL**3
01141         MOVE SPACES                 TO DC-OPTION-CODE                CL**3
01142         PERFORM 9700-DATE-LINK                                       CL**3
01143         MOVE DC-GREG-DATE-1-EDIT    TO B-ACTION-DATE (B-INDX).       CL**3
01144                                                                      CL**3
01145      MOVE AT-PROMPT-LINE-1       TO B-PROMPT-1 (B-INDX).          EL172
01146      MOVE AT-PROMPT-LINE-2       TO B-PROMPT-2 (B-INDX).          EL172
01147      IF B-INDX = 6                                                EL172
01148         MOVE CLAIM-ACTION-REMINDERS-SCREEN TO P-LINES             EL172
01149         MOVE TEMP-IDB            TO TEMP-STOR-ID                  EL172
01150         ADD 1                    TO TEMP-ITEM-B                   EL172
01151         MOVE TEMP-ITEM-B         TO TEMP-ITEM-NO                  EL172
01152         PERFORM 6000-PUT-TEMP-SCREENS                             EL172
01153         SET B-INDX TO 1                                           EL172
01154         MOVE LOW-VALUES          TO CLAIM-ACTION-REMINDERS-SCREEN EL172
01155         ELSE                                                      EL172
01156         SET B-INDX UP BY 1.                                       EL172
01157                                                                   EL172
01158  5899-EXIT.                                                       EL172
01159       EXIT.                                                       EL172
01160      EJECT                                                        EL172
01161  6000-PUT-TEMP-SCREENS.                                           EL172
01162      EXEC CICS WRITEQ TS                                          EL172
01163           QUEUE(TEMP-STOR-ID)                                     EL172
01164           FROM (P-LINES)                                          EL172
01165           LENGTH(TEMP-LENGTH)                                     EL172
01166           END-EXEC.                                               EL172
01167      ADD 1 TO PI-TOTAL-SCREENS.                                   EL172
01168      MOVE LOW-VALUES             TO P-LINES.                      EL172
01169                                                                   EL172
01170                                                                   EL172
01171  6100-GET-TEMP-SCREENS.                                           EL172
01172      EXEC CICS READQ TS                                           EL172
01173           QUEUE(TEMP-STOR-ID)                                     EL172
01174           ITEM(TEMP-ITEM-NO)                                      EL172
01175           INTO(P-LINES)                                           EL172
01176           LENGTH(TEMP-LENGTH)                                     EL172
01177           END-EXEC.                                               EL172
01178                                                                   EL172
01179                                                                   EL172
01180  6200-PUT-NEW-SCREENS.                                            EL172
01181      ADD 1 TO NEW-TEMP-ITEM.                                      EL172
01182      EXEC CICS WRITEQ TS                                          EL172
01183           QUEUE(NEW-TEMP-ID)                                      EL172
01184           FROM(EL172AI)                                           EL172
01185           ITEM(NEW-TEMP-ITEM)                                     EL172
01186           LENGTH(NEW-TEMP-LENGTH)                                 EL172
01187           END-EXEC.                                               EL172
01188                                                                   EL172
01189                                                                   EL172
01190  6300-GET-NEW-SCREENS.                                            EL172
01191      EXEC CICS READQ TS                                           EL172
01192           QUEUE(NEW-TEMP-ID)                                      EL172
01193           INTO(EL172AI)                                           EL172
01194           ITEM(PI-CURRENT-SCREEN)                                 EL172
01195           LENGTH(NEW-TEMP-LENGTH)                                 EL172
01196           END-EXEC.                                               EL172
01197      EJECT                                                        EL172
01198  6500-GET-CLAIM-ACTION-SCREENS.                                   EL172
01199      ADD 1                       TO TEMP-ITEM-NO.                 EL172
01200      PERFORM 6100-GET-TEMP-SCREENS.                               EL172
01201      MOVE CLAIM-ACTION-HEADING   TO P-HEADING.                    EL172
01202      MOVE '- CLAIM ACTION REMINDERS -'  TO TITLEI.                EL172
01203      PERFORM 6200-PUT-NEW-SCREENS.                                EL172
01204                                                                   EL172
01205  6510-GET-AUTO-PAY-SCREENS.                                       EL172
01206      ADD 1                       TO TEMP-ITEM-NO.                 EL172
01207      PERFORM 6100-GET-TEMP-SCREENS.                               EL172
01208      MOVE AUTO-PAY-HEADING       TO P-HEADING.                    EL172
01209      MOVE ' - AUTO PAY EXPIRATIONS - '  TO TITLEI.                EL172
01210      PERFORM 6200-PUT-NEW-SCREENS.                                EL172
01211                                                                   EL172
01212  6520-GET-ONE-MONTH-SCREENS.                                      EL172
01213      ADD 1                       TO TEMP-ITEM-NO.                 EL172
01214      PERFORM 6100-GET-TEMP-SCREENS.                               EL172
01215      MOVE ONE-MONTH-HEADING      TO P-HEADING.                    EL172
01216      MOVE '  - ONE MONTH INACTIVE -  '  TO TITLEI.                EL172
01217      PERFORM 6200-PUT-NEW-SCREENS.                                EL172
01218                                                                   EL172
01219  6530-GET-DELINQUENT-SCREENS.                                     EL172
01220      ADD 1                        TO TEMP-ITEM-NO.                EL172
01221      PERFORM 6100-GET-TEMP-SCREENS.                               EL172
01222      MOVE DELINQUENT-MAIL-HEADING TO P-HEADING.                   EL172
01223      MOVE '   - DELINQUENT MAIL -    '  TO TITLEI.                EL172
01224      PERFORM 6200-PUT-NEW-SCREENS.                                EL172
01225                                                                   EL172
01226  6540-GET-PRIORITY-SCREENS.                                       EL172
01227      ADD 1                       TO TEMP-ITEM-NO.                 EL172
01228      PERFORM 6100-GET-TEMP-SCREENS.                               EL172
01229      MOVE PRIORITY-STATUS-HEADING TO P-HEADING.                   EL172
01230      MOVE ' - PRIORITY *9* STATUS -  '  TO TITLEI.                EL172
01231      PERFORM 6200-PUT-NEW-SCREENS.                                EL172
01232      EJECT                                                        EL172
01233                                                                      CL**5
01234  6600-DELETE-TEMP-STOR.                                           EL172
01235      EXEC CICS HANDLE CONDITION                                   EL172
01236           QIDERR(6699-EXIT)                                          CL**5
01237           END-EXEC.                                               EL172
01238                                                                   EL172
01239      MOVE NEW-TEMP-ID            TO TEMP-STOR-ID.                 EL172
01240      PERFORM 6700-DELETE THRU 6799-EXIT.                          EL172
01241                                                                   EL172
01242      MOVE TEMP-IDB               TO TEMP-STOR-ID.                 EL172
01243      PERFORM 6700-DELETE THRU 6799-EXIT.                          EL172
01244                                                                   EL172
01245      MOVE TEMP-IDC               TO TEMP-STOR-ID.                 EL172
01246      PERFORM 6700-DELETE THRU 6799-EXIT.                          EL172
01247                                                                   EL172
01248      MOVE TEMP-IDD               TO TEMP-STOR-ID.                 EL172
01249      PERFORM 6700-DELETE THRU 6799-EXIT.                          EL172
01250                                                                   EL172
01251      MOVE TEMP-IDE               TO TEMP-STOR-ID.                 EL172
01252      PERFORM 6700-DELETE THRU 6799-EXIT.                          EL172
01253                                                                   EL172
01254      MOVE TEMP-IDF               TO TEMP-STOR-ID.                 EL172
01255      PERFORM 6700-DELETE THRU 6799-EXIT.                          EL172
01256                                                                   EL172
01257  6699-EXIT.                                                       EL172
01258       EXIT.                                                       EL172
01259                                                                   EL172
01260  6700-DELETE.                                                     EL172
01261      EXEC CICS DELETEQ TS                                         EL172
01262           QUEUE(TEMP-STOR-ID)                                     EL172
01263           END-EXEC.                                               EL172
01264  6799-EXIT.                                                       EL172
01265       EXIT.                                                       EL172
01266      EJECT                                                        EL172
01267  8100-SEND-INITIAL-MAP.                                           EL172
01268      MOVE SAVE-DATE              TO RUNDTEO.                      EL172
01269      MOVE EIBTIME                TO TIME-IN.                      EL172
01270      MOVE TIME-OUT               TO RUNTIMEO.                     EL172
01271      MOVE -1                     TO PAGEL.                        EL172
01272      MOVE PI-CURRENT-SCREEN      TO PAGEO                         EL172
01273      MOVE PI-TOTAL-SCREENS       TO PAGETOTO                      EL172
01274      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL172
01275      EXEC CICS SEND                                               EL172
01276          MAP(MAP-NAME)                                            EL172
01277          MAPSET(MAPSET-NAME)                                      EL172
01278          FROM(EL172AO)                                            EL172
01279          ERASE                                                    EL172
01280          CURSOR                                                   EL172
01281          END-EXEC.                                                EL172
01282      GO TO 9100-RETURN-TRAN.                                      EL172
01283                                                                   EL172
01284  8200-SEND-DATAONLY.                                              EL172
01285      MOVE SAVE-DATE              TO RUNDTEO.                      EL172
01286      MOVE EIBTIME                TO TIME-IN.                      EL172
01287      MOVE TIME-OUT               TO RUNTIMEO.                     EL172
01288      MOVE PI-CURRENT-SCREEN      TO PAGEO.                        EL172
01289      MOVE PI-TOTAL-SCREENS       TO PAGETOTO.                     EL172
01290      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL172
01291      EXEC CICS SEND                                               EL172
01292          MAP(MAP-NAME)                                            EL172
01293          MAPSET(MAPSET-NAME)                                      EL172
01294          FROM(EL172AO)                                            EL172
01295          DATAONLY                                                 EL172
01296          CURSOR                                                   EL172
01297          END-EXEC.                                                EL172
01298      GO TO 9100-RETURN-TRAN.                                      EL172
01299                                                                   EL172
01300  8300-SEND-TEXT.                                                  EL172
01301      EXEC CICS SEND TEXT                                          EL172
01302          FROM(LOGOFF-TEXT)                                        EL172
01303          LENGTH(LOGOFF-LENGTH)                                    EL172
01304          ERASE                                                    EL172
01305          FREEKB                                                   EL172
01306          END-EXEC.                                                EL172
01307      EXEC CICS RETURN                                             EL172
01308          END-EXEC.                                                EL172
01309                                                                   EL172
01310      EJECT                                                        EL172
01311  8800-UNAUTHORIZED-ACCESS.                                        EL172
01312      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL172
01313      GO TO 8300-SEND-TEXT.                                        EL172
01314                                                                   EL172
01315  8810-PF23.                                                       EL172
01316      MOVE EIBAID TO PI-ENTRY-CD-1.                                EL172
01317      MOVE XCTL-005 TO PGM-NAME.                                   EL172
01318      GO TO 9300-XCTL.                                             EL172
01319  8820-CNTL-NOT-FOUND.                                             EL172
01320      IF PI-PROCESSOR-ID  =  LGXX-ID                               EL172
01321         MOVE PI-PROCESSOR-ID     TO PI-USER.                      EL172
01322                                                                   EL172
01323      IF PI-USER NOT = SPACES                                      EL172
01324         MOVE ER-0415             TO EMI-ERROR                     EL172
01325         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL172
01326         MOVE PI-USER             TO EMI-TEXT-VARIABLE (1)         EL172
01327         ELSE                                                      EL172
01328         MOVE ER-0281             TO EMI-ERROR                     EL172
01329         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL172
01330      GO TO 8100-SEND-INITIAL-MAP.                                 EL172
01331                                                                   EL172
01332  8840-CLAIM-NOT-OPEN.                                             EL172
01333      MOVE ER-0157                TO EMI-ERROR.                    EL172
01334      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL172
01335      MOVE -1 TO PAGEL.                                            EL172
01336      GO TO 5200-NO-MORE-DATA.                                     EL172
01337                                                                   EL172
01338  8850-ACTIVITY-NOT-OPEN.                                          EL172
01339      MOVE ER-0172                TO EMI-ERROR.                    EL172
01340      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL172
01341      MOVE -1 TO PAGEL.                                            EL172
01342      GO TO 5200-NO-MORE-DATA.                                     EL172
01343                                                                   EL172
01344  8870-NOTOPEN.                                                    EL172
01345      MOVE ER-0042                TO EMI-ERROR.                    EL172
01346      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL172
01347      MOVE -1 TO PAGEL.                                            EL172
01348      GO TO 8100-SEND-INITIAL-MAP.                                 EL172
01349                                                                   EL172
01350  9100-RETURN-TRAN.                                                EL172
01351      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL172
01352      MOVE MAP-NUMBER TO PI-CURRENT-SCREEN-NO.                     EL172
01353      EXEC CICS RETURN                                             EL172
01354          TRANSID(TRANS-ID)                                        EL172
01355          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL172
01356          LENGTH(PI-COMM-LENGTH)                                   EL172
01357          END-EXEC.                                                EL172
01358                                                                   EL172
01359  9200-RETURN-MAIN-MENU.                                           EL172
01360      MOVE XCTL-126 TO PGM-NAME.                                   EL172
01361      GO TO 9300-XCTL.                                             EL172
01362                                                                   EL172
01363  9300-XCTL.                                                       EL172
01364      EXEC CICS XCTL                                               EL172
01365          PROGRAM(PGM-NAME)                                        EL172
01366          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL172
01367          LENGTH(PI-COMM-LENGTH)                                   EL172
01368          END-EXEC.                                                EL172
01369                                                                   EL172
01370  9400-CLEAR.                                                      EL172
01371      PERFORM 6600-DELETE-TEMP-STOR THRU 6699-EXIT.                EL172
01372      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       EL172
01373      GO TO 9300-XCTL.                                             EL172
01374                                                                   EL172
01375  9500-PF12.                                                       EL172
01376      MOVE XCTL-010 TO PGM-NAME.                                   EL172
01377      GO TO 9300-XCTL.                                             EL172
01378                                                                   EL172
01379  9600-PGMID-ERROR.                                                EL172
01380      EXEC CICS HANDLE CONDITION                                   EL172
01381          PGMIDERR(8300-SEND-TEXT)                                 EL172
01382          END-EXEC.                                                EL172
01383      MOVE PGM-NAME TO PI-CALLING-PROGRAM.                         EL172
01384      MOVE ' ' TO PI-ENTRY-CD-1.                                   EL172
01385      MOVE XCTL-005 TO PGM-NAME.                                   EL172
01386      MOVE PGM-NAME TO LOGOFF-PGM.                                 EL172
01387      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL172
01388      GO TO 9300-XCTL.                                             EL172
01389                                                                   EL172
01390                                                                   EL172
01391  9700-DATE-LINK.                                                  EL172
01392      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL172
01393      EXEC CICS LINK                                               EL172
01394           PROGRAM  (PGM-NAME)                                     EL172
01395           COMMAREA (DATE-CONVERSION-DATA)                         EL172
01396           LENGTH   (DC-COMM-LENGTH)                               EL172
01397           END-EXEC.                                               EL172
01398      EJECT                                                        EL172
01399  9900-ERROR-FORMAT.                                               EL172
01400      IF NOT EMI-ERRORS-COMPLETE                                   EL172
01401          MOVE LINK-001 TO PGM-NAME                                EL172
01402          EXEC CICS LINK                                           EL172
01403              PROGRAM  (PGM-NAME)                                  EL172
01404              COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)             EL172
01405              LENGTH   (EMI-COMM-LENGTH)                           EL172
01406              END-EXEC.                                            EL172
01407  9900-EXIT.                                                       EL172
01408      EXIT.                                                        EL172
01409                                                                   EL172
01410  9990-ABEND.                                                      EL172
01411      MOVE LINK-004 TO PGM-NAME.                                   EL172
01412      MOVE DFHEIBLK TO EMI-LINE1.                                  EL172
01413      EXEC CICS LINK                                               EL172
01414          PROGRAM(PGM-NAME)                                        EL172
01415          COMMAREA(EMI-LINE1)                                      EL172
01416          LENGTH(72)                                               EL172
01417          END-EXEC.                                                EL172
01418      GO TO 8200-SEND-DATAONLY.                                    EL172
