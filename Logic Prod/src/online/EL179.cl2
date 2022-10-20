00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL179
00003  PROGRAM-ID.                 EL179 .                                 LV011
00004 *              PROGRAM CONVERTED BY                                  CL*10
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*10
00006 *              CONVERSION DATE 02/13/96 09:38:02.                    CL*10
00007 *                            VMOD=2.011.                             CL*11
00008 *                                                                 EL179
00009 *AUTHOR.        LOGIC, INC.                                          CL*10
00010 *               DALLAS, TEXAS.                                       CL*10
00011                                                                   EL179
00012 *DATE-COMPILED.                                                      CL*10
00013                                                                   EL179
00014 *SECURITY.   *****************************************************   CL*10
00015 *            *                                                   *   CL*10
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*10
00017 *            *                                                   *   CL*10
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*10
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*10
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*10
00021 *            *                                                   *   CL*10
00022 *            *****************************************************   CL*10
00023 *REMARKS. TRANSACTION EX49 - REPORT REVIEW.                          CL**3
00024      EJECT                                                        EL179
00025  ENVIRONMENT DIVISION.                                            EL179
00026  DATA DIVISION.                                                   EL179
00027  WORKING-STORAGE SECTION.                                         EL179
00028  01  LCP-TIME-OF-DAY-XX.                                             CL*10
00029      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL*10
00030      05  FILLER                    PIC 99.                           CL*10
00031  01  LCP-CICS-TIME                 PIC 9(15).                        CL*10
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL179
00033  77  FILLER  PIC X(32)  VALUE '*   EL179  WORKING STORAGE     *'. EL179
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.011 *********'.    CL*11
00035                                                                   EL179
00036      EJECT                                                           CL**7
00037  01  WS-DATE-AREA.                                                EL179
00038      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL179
00039      05  SAVE-BIN-DATE           PIC XX      VALUE SPACES.        EL179
00040                                                                   EL179
00041  01  LITERALS-NUMBERS.                                            EL179
00042      12  W-APPL-SCRTY-NDX        PIC S9(04) COMP VALUE +36.          CL**4
00043      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.       EL179
00044      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.       EL179
00045      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.       EL179
00046      12  XCTL-EM626              PIC X(8)    VALUE 'EM626'.          CL**5
00047      12  XCTL-GL800              PIC X(8)    VALUE 'GL800'.          CL**5
00048      12  THIS-PGM                PIC X(8)    VALUE 'EL179'.       EL179
00049      12  DATE-CONV               PIC X(8)    VALUE 'ELDATCV'.     EL179
00050      12  LIT-TRAN                PIC X(4)    VALUE 'EX49'.        EL179
00051      12  LIT-TRAN-PR             PIC X(4)    VALUE 'EX59'.        EL179
00052      12  LIT-MAP                 PIC X(4)    VALUE '179A'.        EL179
00053      12  LIT-MAP-2               PIC X(4)    VALUE '179B'.        EL179
00054                                                                   EL179
00055  01  EDIT-WORK-AREA.                                              EL179
00056                                                                      CL**4
00057      12  W-TIME-IN               PIC S9(07).                         CL**4
00058      12  FILLER   REDEFINES W-TIME-IN.                               CL**4
00059          16  FILLER              PIC  X(01).                         CL**4
00060          16  W-TIME-OUT          PIC  9(02)V9(02).                   CL**4
00061          16  FILLER              PIC  X(02).                         CL**4
00062      12  CALL-PGM                PIC X(8).                        EL179
00063      12  TRANS-ID                PIC X(4).                        EL179
00064      12  USER-TERM               PIC X(4).                        EL179
00065      12  CHECK-PFKEYS            PIC 99.                          EL179
00066      12  TEST-RESP               PIC X.                           EL179
00067      12  COUNT-1                 PIC 99.                          EL179
00068      12  COUNT-2                 PIC 99.                          EL179
00069      12  COUNT-3                 PIC 99.                          EL179
00070      12  COUNT-4                 PIC 99   VALUE  0.                  CL*10
00071      12  DISPLAY-LINE.                                            EL179
00072          16  REP-ID              PIC X(5).                        EL179
00073          16  FILLER              PIC X(4).                        EL179
00074          16  REP-NAME            PIC X(44).                       EL179
00075          16  FILLER              PIC XX.                          EL179
00076          16  REP-CREATED         PIC X(8).                        EL179
00077          16  FILLER              PIC XX.                          EL179
00078          16  REP-TIME            PIC X(5).                        EL179
00079          16  FILLER              PIC XX.                          EL179
00080          16  REP-LINES           PIC X(5).                        EL179
00081          16  FILLER              PIC XX.                          EL179
00082      12  PGMN-KEY                PIC X(5).                        EL179
00083      12  CONVERT-LINES           PIC 9(8).                        EL179
00084      12  HOLD-LINES.                                              EL179
00085          16  FILLER              PIC X(3).                        EL179
00086          16  DISPLAY-LINES       PIC 9(5).                        EL179
00087      12  EDIT-LINES              PIC ZZZZ9.                       EL179
00088      12  OPTION-HOLD             PIC X.                           EL179
00089          88  PRINT-REPORT                    VALUE '1'.           EL179
00090          88  REVIEW-ONLINE                   VALUE '2'.           EL179
00091          88  PURGE-REPORT                    VALUE '3'.           EL179
00092          88  REVIEW-QUE                      VALUE '4'.           EL179
00093          88  VALID-OPTION                VALUE '1' '2' '3' '4'.   EL179
00094                                                                   EL179
00095  01  TIME-UNFORMATTED.                                            EL179
00096      12  UN-HOURS                PIC XX.                          EL179
00097      12  UN-MINUTES              PIC XX.                          EL179
00098      12  FILLER                  PIC XX.                          EL179
00099                                                                   EL179
00100  01  TIME-FORMATTED.                                              EL179
00101      12  FOR-HOURS               PIC XX.                          EL179
00102      12  FILLER                  PIC X       VALUE '.'.           EL179
00103      12  FOR-MINUTES             PIC XX.                          EL179
00104                                                                   EL179
00105  01  ERROR-NUMBERS.                                               EL179
00106      12  ER-0000                 PIC X(4)    VALUE '0000'.        EL179
00107      12  ER-0029                 PIC X(4)    VALUE '0029'.        EL179
00108      12  ER-0042                 PIC X(4)    VALUE '0042'.        EL179
00109      12  ER-0065                 PIC X(4)    VALUE '0065'.           CL**4
00110      12  ER-0070                 PIC X(4)    VALUE '0070'.        EL179
00111      12  ER-0087                 PIC X(4)    VALUE '0087'.        EL179
00112      12  ER-0088                 PIC X(4)    VALUE '0088'.        EL179
00113      12  ER-0130                 PIC X(4)    VALUE '0130'.        EL179
00114      12  ER-0131                 PIC X(4)    VALUE '0131'.        EL179
00115      12  ER-0162                 PIC X(4)    VALUE '0162'.        EL179
00116      12  ER-0275                 PIC X(4)    VALUE '0275'.        EL179
00117      12  ER-0412                 PIC X(4)    VALUE '0412'.        EL179
00118      12  ER-0413                 PIC X(4)    VALUE '0413'.        EL179
00119      12  ER-0487                 PIC X(4)    VALUE '0487'.        EL179
00120      12  ER-2358                 PIC X(4)    VALUE '2358'.        EL179
00121      12  ER-2359                 PIC X(4)    VALUE '2359'.        EL179
00122      12  ER-2360                 PIC X(4)    VALUE '2360'.        EL179
00123      12  ER-2545                 PIC X(4)    VALUE '2545'.        EL179
00124      12  ER-3340                 PIC X(4)    VALUE '3340'.           CL**4
00125      12  ER-7008                 PIC X(4)    VALUE '7008'.           CL**4
00126      12  ER-9096                 PIC X(4)    VALUE '9096'.           CL**4
00127      12  ER-9097                 PIC X(4)    VALUE '9097'.           CL**4
00128                                                                   EL179
00129  01  ERROR-SWITCHES.                                              EL179
00130      12  ERROR-SWITCH            PIC X.                           EL179
00131          88  SCREEN-ERROR                    VALUE 'X'.           EL179
00132      12  PGMN-SWITCH             PIC X.                           EL179
00133          88  PGMN-FOUND                      VALUE '1'.           EL179
00134      12  SCREEN-SWITCH           PIC X.                           EL179
00135          88  END-OF-FILE                     VALUE 'E'.           EL179
00136          88  SCREEN-FULL                     VALUE 'F'.           EL179
00137                                                                   EL179
00138  01  REPT-KEY.                                                    EL179
00139      12  REPT-COMPANY-CODE       PIC X.                           EL179
00140      12  REPT-RECORD-TYPE        PIC X.                           EL179
00141      12  REPT-REPORT-ID          PIC X(5).                        EL179
00142      12  REPT-LINE-NUMBER        PIC S9(8)   COMP.                EL179
00143                                                                   EL179
00144  01  CNTL-KEY.                                                    EL179
00145      12  CNTL-COMPANY-ID         PIC X(3).                        EL179
00146      12  CNTL-RECORD-TYPE        PIC X.                           EL179
00147      12  FILLER                  PIC X(4).                        EL179
00148      12  CNTL-SEQ-NUMBER         PIC S9(4)   COMP.                EL179
00149                                                                   EL179
00150  01  COMP-LENGTHS.                                                EL179
CIDMOD     12  LIT-IC                  PIC S9(4)   COMP VALUE -1.            000
00151      12  DELETE-LENGTH           PIC S9(4)   COMP VALUE +7.       EL179
00152      EJECT                                                        EL179
00153                                  COPY ELCLOGOF.                      CL**7
00154      EJECT                                                        EL179
00155                                  COPY ELCATTR.                       CL**7
00156      EJECT                                                           CL**7
00157                                  COPY ELCAID.                        CL**7
00158                                                                   EL179
00159  01  FILLER REDEFINES DFHAID.                                     EL179
00160      12  FILLER                  PIC X(8).                        EL179
00161      12  AID-KEYS OCCURS 24 TIMES.                                EL179
00162          16  FILLER              PIC X.                           EL179
00163      EJECT                                                        EL179
00164                                  COPY EL179S.                        CL**7
00165      EJECT                                                        EL179
00166                                  COPY ELCINTF.                       CL**7
00167      12  EL179-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.          EL179
00168          16  PI-PRINT-REPORT     PIC X(5).                        EL179
00169          16  PI-START-COUNT      PIC S9(8)   COMP.                EL179
00170          16  PI-TOTAL-COUNT      PIC S9(8)   COMP.                EL179
00171          16  PI-START-PAGE-NO    PIC S9(4).                       EL179
00172          16  PI-ACCUM-PAGE-NO    PIC S9(4).                       EL179
00173          16  PI-END-PAGE-NO      PIC S9(4).                       EL179
00174          16  PI-FORMS-PRINTER-ID PIC X(4).                        EL179
00175          16  PI-SAVE-BEGIN       PIC X(5).                        EL179
00176          16  PI-SAVE-ENDING      PIC X(5).                        EL179
00177          16  PI-VIEW-SWITCH      PIC X.                           EL179
00178          16  PI-DELETE-SW        PIC X.                              CL**4
00179              88  FIRST-REQUEST VALUE ' '.                            CL**4
00180              88  DELETE-OK     VALUE 'Y'.                            CL**4
00181          16  PI-ELREPT-BROWSE-SW PIC X.                              CL**4
00182              88  STARTED-BROWSE VALUE 'Y'.                           CL**4
00183          16  FILLER              PIC X(598).                         CL*10
00184                                                                      CL**4
00185      EJECT                                                        EL179
00186                                  COPY ELCEMIB.                       CL**7
00187      EJECT                                                           CL**4
00188                                  COPY ELCDATE.                       CL**7
00189      EJECT                                                           CL**4
00190                                  COPY ELCSCTM.                       CL**7
00191      EJECT                                                           CL**7
00192                                  COPY ELCSCRTY.                      CL**7
00193      EJECT                                                           CL**7
00194                                  COPY MPCSCRT.                       CL**7
00195      EJECT                                                        EL179
00196  LINKAGE SECTION.                                                 EL179
00197  01  DFHCOMMAREA                 PIC X(1024).                     EL179
00198                                                                   EL179
00199 *01 PARM-LIST .                                                      CL*10
00200 *    12  FILLER                  PIC S9(8)   COMP.                   CL*10
00201 *    12  REPT-PNT                PIC S9(8)   COMP.                   CL*10
00202 *    12  PGMN-PNT                PIC S9(8)   COMP.                   CL*10
00203 *    12  CNTL-PNT                PIC S9(8)   COMP.                   CL*10
00204      EJECT                                                        EL179
00205                                  COPY ELCREPT.                       CL**7
00206      EJECT                                                        EL179
00207                                  COPY ELCPGMN.                       CL**7
00208      EJECT                                                        EL179
00209                                  COPY ELCCNTL.                       CL**7
00210      EJECT                                                        EL179
00211  PROCEDURE DIVISION.                                              EL179
00212                                                                   EL179
00213      CONTINUE.                                                       CL*10
00214                                                                   EL179
00215      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL179
00216      MOVE '5'                    TO DC-OPTION-CODE.               EL179
00217      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL179
00218      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE.                    EL179
00219      MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE.                EL179
00220      MOVE SPACES                 TO PI-ELREPT-BROWSE-SW.             CL**4
00221                                                                   EL179
00222      IF EIBCALEN = ZERO                                           EL179
00223          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL179
00224                                                                   EL179
00225      EXEC CICS HANDLE CONDITION                                   EL179
00226           PGMIDERR      (8820-XCTL-ERROR)                         EL179
00227           TERMIDERR     (1110-TERMID-ERROR)                       EL179
00228           TRANSIDERR    (1120-TRANS-ERROR)                        EL179
00229           ERROR         (9990-ABEND)                              EL179
00230      END-EXEC.                                                    EL179
00231                                                                   EL179
00232      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL179
00233      MOVE SPACES                 TO ERROR-SWITCHES.               EL179
00234      MOVE LIT-TRAN               TO TRANS-ID.                     EL179
00235      MOVE LOW-VALUES             TO EL179AI EL179BI.              EL179
00236                                                                   EL179
00237      IF THIS-PGM NOT = PI-CALLING-PROGRAM                         EL179
00238          MOVE LOW-VALUES         TO EL179AO                       EL179
00239          MOVE ZEROS              TO PI-START-COUNT                EL179
00240          PERFORM 0100-UPDATE-PI THRU 0120-EXIT                    EL179
00241          GO TO 2000-PAGE-FORWARD.                                    CL**7
00242                                                                   EL179
00243      IF  EIBAID = DFHCLEAR                                           CL**4
00244              OR                                                      CL**4
00245          NOT DISPLAY-CAP                                             CL**4
00246          GO TO 8200-RETURN-PRIOR.                                 EL179
00247                                                                   EL179
00248      IF PI-CURRENT-SCREEN-NO = LIT-MAP-2                          EL179
00249          MOVE ZEROS              TO COUNT-2                       EL179
00250          GO TO 5000-PROCESS-REVIEW.                               EL179
00251                                                                   EL179
00252      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL179
00253          MOVE LOW-VALUES         TO EL179AO                       EL179
00254          MOVE ER-7008            TO EMI-ERROR                     EL179
00255          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL179
00256          MOVE -1                 TO OPTIONL                       EL179
00257          GO TO 8110-SEND-DATA.                                    EL179
00258                                                                   EL179
00259      EXEC CICS RECEIVE                                            EL179
00260          MAP     ('EL179A')                                       EL179
00261          MAPSET  ('EL179S')                                       EL179
00262      END-EXEC.                                                    EL179
00263                                                                   EL179
00264      IF PFKEYL GREATER THAN ZERO                                  EL179
00265          PERFORM 0200-TRANS-PF THRU 0210-EXIT.                    EL179
00266                                                                   EL179
00267      IF SCREEN-ERROR                                              EL179
00268          MOVE ER-7008            TO EMI-ERROR                     EL179
00269          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL179
00270          MOVE -1                 TO OPTIONL                       EL179
00271          GO TO 8110-SEND-DATA.                                    EL179
00272                                                                   EL179
00273      IF EIBAID = DFHPF12                                          EL179
00274          GO TO 8300-GET-HELP.                                     EL179
00275                                                                   EL179
00276      IF EIBAID = DFHPF23                                          EL179
00277          GO TO 8810-PF23-ENTERED.                                 EL179
00278                                                                   EL179
00279      IF EIBAID = DFHPF24                                          EL179
00280          GO TO 8400-RETURN-MASTER.                                EL179
00281                                                                   EL179
00282      IF EIBAID = DFHPF1                                           EL179
00283          GO TO 2000-PAGE-FORWARD.                                 EL179
00284                                                                   EL179
00285      IF EIBAID = DFHPF2                                           EL179
00286          GO TO 3000-PAGE-BACKWARD.                                EL179
00287                                                                   EL179
00288      PERFORM 1000-EDIT-SCREEN THRU 1000-EXIT.                     EL179
00289                                                                   EL179
00290      IF SCREEN-ERROR                                              EL179
00291          GO TO 8110-SEND-DATA.                                    EL179
00292                                                                   EL179
00293      IF EIBAID NOT = DFHENTER                                     EL179
00294          MOVE ER-0029            TO EMI-ERROR                     EL179
00295          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL179
00296          MOVE -1                 TO OPTIONL                       EL179
00297          GO TO 8110-SEND-DATA.                                    EL179
00298                                                                   EL179
00299      IF REVIEW-ONLINE                                             EL179
00300          IF PI-START-PAGE-NO GREATER ZERO                         EL179
00301              PERFORM 6500-PAGE-SEARCH THRU 6500-EXIT              EL179
00302              GO TO 1200-REVIEW-REPORT                             EL179
00303          ELSE                                                     EL179
00304              GO TO 1200-REVIEW-REPORT.                            EL179
00305                                                                   EL179
00306      IF PRINT-REPORT                                                 CL**4
00307          IF PI-START-PAGE-NO GREATER ZERO                            CL**4
00308              PERFORM 6500-PAGE-SEARCH THRU 6500-EXIT                 CL**4
00309              GO TO 1100-START-PRINTER                                CL**4
00310          ELSE                                                        CL**4
00311              GO TO 1100-START-PRINTER.                               CL**4
00312                                                                      CL**4
00313      IF PURGE-REPORT                                              EL179
00314         IF DELETE-OK                                                 CL**4
00315             MOVE SPACES          TO PI-DELETE-SW                     CL**4
00316             GO TO 1300-DELETE-REPORT                                 CL**4
00317          ELSE                                                        CL**4
00318             MOVE 'Y'                TO PI-DELETE-SW                  CL**4
00319             MOVE ER-0065            TO EMI-ERROR                     CL**4
00320             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 CL**4
00321             MOVE AL-UNNON           TO OPTIONA                       CL**9
00322             MOVE +1                 TO OPTIONL                       CL**9
00323             MOVE '3'                TO OPTIONI                       CL**9
00324             MOVE AL-UANON           TO REPA                          CL**9
00325             MOVE +5                 TO REPL                          CL**9
00326             MOVE PI-PRINT-REPORT    TO REPI                          CL**9
00327             GO TO 2000-PAGE-FORWARD.                                 CL**9
00328                                                                   EL179
00329      GO TO 2000-PAGE-FORWARD.                                     EL179
00330      EJECT                                                        EL179
00331  0100-UPDATE-PI.                                                  EL179
00332      IF PI-RETURN-TO-PROGRAM = THIS-PGM                           EL179
00333          GO TO 0110-UPDATE-UP.                                    EL179
00334                                                                   EL179
00335      MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6.           EL179
00336      MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5.           EL179
00337      MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4.           EL179
00338      MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3.           EL179
00339      MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2.           EL179
00340      MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1.           EL179
00341      MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM.         EL179
00342      MOVE THIS-PGM               TO PI-CALLING-PROGRAM.           EL179
00343      MOVE HIGH-VALUES            TO PI-SAVE-BEGIN.                EL179
00344      MOVE LOW-VALUES             TO PI-SAVE-ENDING.               EL179
00345      MOVE SPACES                 TO PI-FORMS-PRINTER-ID.          EL179
00346      MOVE SPACES                 TO PI-DELETE-SW.                    CL**4
00347      PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT.                CL**4
00348      GO TO 0120-EXIT.                                             EL179
00349                                                                   EL179
00350  0110-UPDATE-UP.                                                  EL179
00351      MOVE PI-RETURN-TO-PROGRAM   TO PI-CALLING-PROGRAM.           EL179
00352      MOVE PI-SAVED-PROGRAM-1     TO PI-RETURN-TO-PROGRAM.         EL179
00353      MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-1.           EL179
00354      MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-2.           EL179
00355      MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-3.           EL179
00356      MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-4.           EL179
00357      MOVE PI-SAVED-PROGRAM-6     TO PI-SAVED-PROGRAM-5.           EL179
00358      MOVE SPACES                 TO PI-SAVED-PROGRAM-6.           EL179
00359                                                                   EL179
00360  0120-EXIT.                                                       EL179
00361      EXIT.                                                        EL179
00362                                                                   EL179
00363  0200-TRANS-PF.                                                   EL179
00364      IF EIBAID NOT = DFHENTER                                     EL179
00365          MOVE 'X'                TO ERROR-SWITCH                  EL179
00366          GO TO 0210-EXIT.                                         EL179
00367                                                                   EL179
00368      IF PFKEYI NOT NUMERIC                                        EL179
00369          MOVE 'X'                TO ERROR-SWITCH                  EL179
00370          GO TO 0210-EXIT.                                         EL179
00371                                                                   EL179
00372      MOVE PFKEYI                 TO CHECK-PFKEYS.                 EL179
00373                                                                   EL179
00374      IF CHECK-PFKEYS LESS 1 OR GREATER 24                         EL179
00375          MOVE 'X'                TO ERROR-SWITCH                  EL179
00376          GO TO 0210-EXIT.                                         EL179
00377                                                                   EL179
00378      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.                      EL179
00379                                                                   EL179
00380  0210-EXIT.                                                       EL179
00381      EXIT.                                                        EL179
00382      EJECT                                                        EL179
00383  1000-EDIT-SCREEN.                                                EL179
00384      MOVE ZEROS                  TO PI-START-COUNT.               EL179
00385                                                                   EL179
00386      MOVE OPTIONI                TO OPTION-HOLD.                  EL179
00387                                                                   EL179
00388      IF NOT VALID-OPTION                                          EL179
00389          MOVE ER-0087            TO EMI-ERROR                     EL179
00390          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL179
00391          MOVE AL-UNBON           TO OPTIONA                       EL179
00392          MOVE -1                 TO OPTIONL                       EL179
00393          GO TO 1000-EXIT.                                         EL179
00394                                                                   EL179
00395      IF PI-PROCESSOR-PRINTER IS NOT EQUAL SPACES                     CL**3
Y2KMOD                                          AND  LOW-VALUES            CL**3
00396          MOVE PI-PROCESSOR-PRINTER   TO  PI-FORMS-PRINTER-ID.        CL**3
00397                                                                      CL**3
00398      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.               CL*11
00399      IF ALTPRNTL GREATER ZERO                                     EL179
00400          MOVE ALTPRNTI           TO PI-FORMS-PRINTER-ID              CL*11
00401                                     PI-ALT-DMD-PRT-ID.               CL*11
00402                                                                   EL179
00403      IF REVIEW-QUE                                                EL179
00404        AND                                                        EL179
00405         REPI NOT GREATER THAN SPACES                              EL179
00406          MOVE AL-UNNON           TO OPTIONA                       EL179
00407          MOVE AL-UANOF           TO REPA                          EL179
00408          MOVE LOW-VALUES         TO STPAGEO                       EL179
00409          MOVE AL-UNNOF           TO STPAGEA                       EL179
00410          GO TO 1000-EXIT.                                         EL179
00411                                                                      CL**4
00412      IF NOT PURGE-REPORT                                             CL**4
00413          MOVE SPACES             TO PI-DELETE-SW.                    CL**4
00414                                                                   EL179
00415      IF REVIEW-ONLINE OR PRINT-REPORT                             EL179
00416          IF STPAGEL GREATER ZERO                                  EL179
00417              IF STPAGEI NUMERIC                                   EL179
00418                  MOVE ZEROS      TO PI-ACCUM-PAGE-NO              EL179
00419                  MOVE STPAGEI    TO PI-START-PAGE-NO              EL179
00420                  MOVE PI-START-PAGE-NO                               CL*10
00421                                  TO STPAGEO                          CL*10
00422                  MOVE AL-UNNON   TO STPAGEA                       EL179
00423              ELSE                                                 EL179
00424                  MOVE AL-UNBON   TO STPAGEA                       EL179
00425                  MOVE -1         TO STPAGEL                       EL179
00426                  MOVE 'X'        TO ERROR-SWITCH                  EL179
00427                  MOVE ER-2358    TO EMI-ERROR                     EL179
00428                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL179
00429          ELSE                                                     EL179
00430              MOVE ZEROS          TO PI-START-PAGE-NO              EL179
00431                                     PI-ACCUM-PAGE-NO              EL179
00432      ELSE                                                         EL179
00433          MOVE ZEROS              TO PI-START-PAGE-NO              EL179
00434                                     PI-ACCUM-PAGE-NO              EL179
00435          MOVE LOW-VALUES         TO STPAGEO                       EL179
00436          MOVE AL-UNNOF           TO STPAGEA.                      EL179
00437                                                                   EL179
00438      IF PRINT-REPORT                                              EL179
00439          IF ENDPAGEL GREATER ZERO                                 EL179
00440              IF ENDPAGEI NUMERIC                                  EL179
00441                  MOVE ENDPAGEI   TO PI-END-PAGE-NO                EL179
00442                  MOVE PI-END-PAGE-NO                                 CL*10
00443                                  TO ENDPAGEO                         CL*10
00444                  MOVE AL-UNNON   TO ENDPAGEA                      EL179
00445              ELSE                                                 EL179
00446                  MOVE AL-UNBON   TO ENDPAGEA                      EL179
00447                  MOVE -1         TO ENDPAGEL                      EL179
00448                  MOVE 'X'        TO ERROR-SWITCH                  EL179
00449                  MOVE ER-2358    TO EMI-ERROR                     EL179
00450                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL179
00451          ELSE                                                     EL179
00452              MOVE ZEROS          TO PI-END-PAGE-NO                EL179
00453      ELSE                                                         EL179
00454          MOVE ZEROS              TO PI-END-PAGE-NO                EL179
00455          MOVE LOW-VALUES         TO ENDPAGEO                      EL179
00456          MOVE AL-UNNOF           TO ENDPAGEA.                     EL179
00457                                                                   EL179
00458      IF NOT REVIEW-QUE                                            EL179
00459        AND                                                        EL179
00460         EIBAID NOT = DFHENTER                                     EL179
00461          GO TO 1000-PFKEY-ERROR.                                  EL179
00462                                                                   EL179
00463      MOVE REPI                   TO PGMN-KEY  PI-PRINT-REPORT.    EL179
00464                                                                   EL179
00465      PERFORM 1600-READ-PGMN THRU 1600-EXIT.                       EL179
00466                                                                   EL179
00467      IF PGMN-FOUND                                                EL179
00468         GO TO 1000-EXIT.                                          EL179
00469                                                                   EL179
00470      MOVE ER-0162                TO EMI-ERROR.                    EL179
00471      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
00472      MOVE 'X'                    TO ERROR-SWITCH.                 EL179
00473      MOVE -1                     TO REPL.                         EL179
00474      MOVE AL-UABON               TO REPA.                         EL179
00475      GO TO 1000-EXIT.                                             EL179
00476                                                                   EL179
00477  1000-PFKEY-ERROR.                                                EL179
00478      MOVE ER-7008                TO EMI-ERROR.                    EL179
00479      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
00480      MOVE 'X'                    TO ERROR-SWITCH.                 EL179
00481      MOVE -1                     TO OPTIONL.                      EL179
00482                                                                   EL179
00483  1000-EXIT.                                                       EL179
00484      EXIT.                                                        EL179
00485      EJECT                                                        EL179
00486  1100-START-PRINTER.                                              EL179
00487                                                                      CL**4
00488      IF  NOT MODIFY-CAP                                              CL**4
00489          MOVE 'UPDATE'           TO SM-READ                          CL**4
00490          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT              CL**4
00491          MOVE ER-9096            TO EMI-ERROR                        CL**4
00492          PERFORM 9900-ERROR-FORMAT THRU 9910-EXIT                    CL**4
00493          MOVE -1                 TO OPTIONL                          CL**4
00494          GO TO 8110-SEND-DATA.                                       CL**4
00495                                                                      CL**4
00496      MOVE SPACES                 TO REPT-KEY.                     EL179
00497      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.            EL179
00498      MOVE '2'                    TO REPT-RECORD-TYPE.             EL179
00499      MOVE REPI                   TO REPT-REPORT-ID PI-PRINT-REPORTEL179
00500      MOVE ZEROS                  TO REPT-LINE-NUMBER.             EL179
00501      PERFORM 1400-FIND-REPORT THRU 1400-EXIT.                     EL179
00502                                                                   EL179
00503      IF SCREEN-ERROR                                              EL179
00504          GO TO 8110-SEND-DATA.                                    EL179
00505                                                                   EL179
00506      IF PI-FORMS-PRINTER-ID = SPACES                              EL179
Y2KMOD                            OR  LOW-VALUES                        EL179
00507          MOVE SPACES             TO CNTL-KEY                      EL179
00508          MOVE PI-COMPANY-ID      TO CNTL-COMPANY-ID               EL179
00509          MOVE '1'                TO CNTL-RECORD-TYPE              EL179
00510          MOVE ZEROS              TO CNTL-SEQ-NUMBER               EL179
00511          PERFORM 1500-FIND-PRINTER THRU 1500-EXIT                 EL179
00512          IF SCREEN-ERROR                                          EL179
00513              MOVE -1             TO OPTIONL                       EL179
00514              GO TO 8110-SEND-DATA                                 EL179
00515          ELSE                                                     EL179
00516              MOVE CF-FORMS-PRINTER-ID TO USER-TERM                EL179
CIDMOD                                         PI-FORMS-PRINTER-ID           000
CIDMOD         END-IF
00517      ELSE                                                         EL179
00518          MOVE PI-FORMS-PRINTER-ID    TO USER-TERM.                EL179
CIDMOD                                                                       000
CIDMOD     MOVE PI-FORMS-PRINTER-ID TO PI-PROCESSOR-PRINTER.                 000
CIDMOD                                                                       000
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*11
CIDMOD         EXEC CICS START                                             CL*11
CIDMOD             TRANSID   (LIT-TRAN-PR)                                 CL*11
CIDMOD             FROM      (PROGRAM-INTERFACE-BLOCK)                     CL*11
CIDMOD             LENGTH    (PI-COMM-LENGTH)                              CL*11
PEMMOD*            TERMID    ('A046')
CIDMOD         END-EXEC                                                    CL*11
00528      ELSE                                                            CL*11
00529          EXEC CICS START                                             CL*11
00530              TRANSID   (LIT-TRAN-PR)                                 CL*11
00531              FROM      (PROGRAM-INTERFACE-BLOCK)                     CL*11
00532              LENGTH    (PI-COMM-LENGTH)                              CL*11
00533              TERMID    (USER-TERM)                                   CL*11
00534          END-EXEC.                                                   CL*11
00535                                                                   EL179
00536      MOVE ER-2360                TO EMI-ERROR.                    EL179
00537      GO TO 1130-SEND-MESSAGE.                                     EL179
00538                                                                   EL179
00539  1110-TERMID-ERROR.                                               EL179
00540      MOVE ER-0412                TO EMI-ERROR.                    EL179
00541      GO TO 1130-SEND-MESSAGE.                                     EL179
00542                                                                   EL179
00543  1120-TRANS-ERROR.                                                EL179
00544      MOVE ER-0413                TO EMI-ERROR.                    EL179
00545                                                                   EL179
00546  1130-SEND-MESSAGE.                                               EL179
00547      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
00548      MOVE -1                     TO OPTIONL.                      EL179
00549      GO TO 8110-SEND-DATA.                                        EL179
00550      EJECT                                                        EL179
00551  1200-REVIEW-REPORT.                                              EL179
00552      MOVE SPACES                 TO REPT-KEY.                     EL179
00553      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.            EL179
00554      MOVE '2'                    TO REPT-RECORD-TYPE.             EL179
00555      MOVE REPI                   TO REPT-REPORT-ID.               EL179
00556      MOVE ZEROS                  TO REPT-LINE-NUMBER.             EL179
00557      PERFORM 1400-FIND-REPORT THRU 1400-EXIT.                     EL179
00558                                                                   EL179
00559      IF RF-LINE-NUMBER = ZERO                                     EL179
00560         MOVE ER-2545             TO EMI-ERROR                     EL179
00561         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL179
00562         MOVE -1                  TO OPTIONL                       EL179
00563         MOVE AL-UNBON            TO OPTIONA                       EL179
00564         GO TO 8110-SEND-DATA.                                     EL179
00565                                                                   EL179
00566      IF SCREEN-ERROR                                              EL179
00567          MOVE -1                 TO REPL                          EL179
00568          MOVE AL-UABON           TO REPA                          EL179
00569          GO TO 8110-SEND-DATA.                                    EL179
00570                                                                   EL179
00571      MOVE RF-LINE-NUMBER         TO PI-TOTAL-COUNT.               EL179
00572      MOVE LOW-VALUES             TO EL179BO.                      EL179
00573      MOVE '1'                    TO PI-VIEW-SWITCH.               EL179
00574                                                                   EL179
00575      IF PI-START-COUNT GREATER ZERO                               EL179
00576          NEXT SENTENCE                                            EL179
00577      ELSE                                                         EL179
00578          MOVE 1                  TO PI-START-COUNT.               EL179
00579                                                                   EL179
00580      MOVE REPI                   TO PI-PRINT-REPORT.              EL179
00581      PERFORM 6000-BUILD-FIRST THRU 6000-EXIT.                     EL179
00582      MOVE -1                     TO PFKEYBL.                      EL179
00583      GO TO 8140-SEND-MAP-B.                                       EL179
00584      EJECT                                                        EL179
00585  1300-DELETE-REPORT.                                              EL179
00586                                                                      CL**4
00587      IF  NOT MODIFY-CAP                                              CL**4
00588          MOVE 'UPDATE'           TO SM-READ                          CL**4
00589          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT              CL**4
00590          MOVE ER-9096            TO EMI-ERROR                        CL**4
00591          PERFORM 9900-ERROR-FORMAT THRU 9910-EXIT                    CL**4
00592          MOVE -1                 TO OPTIONL                          CL**4
00593          GO TO 8110-SEND-DATA.                                       CL**4
00594                                                                      CL**4
00595      MOVE SPACES                 TO REPT-KEY.                     EL179
00596      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.            EL179
00597      MOVE '2'                    TO REPT-RECORD-TYPE.             EL179
00598      MOVE REPI                   TO REPT-REPORT-ID.               EL179
00599      MOVE ZEROS                  TO REPT-LINE-NUMBER.             EL179
00600      PERFORM 1400-FIND-REPORT THRU 1400-EXIT.                     EL179
00601                                                                   EL179
00602      IF SCREEN-ERROR                                              EL179
00603          MOVE -1                 TO REPL                          EL179
00604          MOVE AL-UABON           TO REPA                          EL179
00605          GO TO 8110-SEND-DATA.                                    EL179
00606                                                                   EL179
00607 ********************************  DELTETE TRAILER RECORD ********    CL**3
00608      EXEC CICS DELETE                                             EL179
00609          DATASET ('ELREPT')                                       EL179
00610          RIDFLD  (RF-CONTROL-PRIMARY)                             EL179
00611          END-EXEC.                                                EL179
00612                                                                   EL179
00613      MOVE '1'                    TO REPT-RECORD-TYPE.             EL179
00614      MOVE ZEROS                  TO REPT-LINE-NUMBER.             EL179
00615                                                                   EL179
00616  1305-READ-LOOP.                                                  EL179
00617      EXEC CICS READ                                               EL179
00618          SET     (ADDRESS OF REPORT-SAVE-FILE)                       CL*10
00619          DATASET ('ELREPT')                                       EL179
00620          RIDFLD  (REPT-KEY)                                       EL179
00621          GTEQ                                                     EL179
00622          END-EXEC.                                                EL179
00623                                                                   EL179
00624      CONTINUE.                                                       CL*10
00625                                                                   EL179
00626      IF PI-COMPANY-CD = RF-COMPANY-CD  AND                        EL179
00627         REPI          = RF-REPORT-ID                              EL179
00628          MOVE RF-CONTROL-PRIMARY TO REPT-KEY                      EL179
00629      ELSE                                                         EL179
00630          GO TO 1310-DELETE-DETAIL-COMPLETE.                       EL179
00631                                                                   EL179
00632      EXEC CICS DELETE                                             EL179
00633          DATASET ('ELREPT')                                       EL179
00634          RIDFLD  (REPT-KEY)                                       EL179
00635      END-EXEC.                                                    EL179
00636                                                                   EL179
00637      ADD +1 TO REPT-LINE-NUMBER.                                  EL179
00638      MOVE REPT-LINE-NUMBER   TO  COUNT-4.                            CL**8
00639      IF COUNT-4  =  0                                                CL**8
00640          EXEC CICS                                                   CL**8
00641              SYNCPOINT                                               CL**8
00642          END-EXEC.                                                   CL**8
00643                                                                      CL**8
00644      GO TO 1305-READ-LOOP.                                        EL179
00645                                                                   EL179
00646                                                                   EL179
00647  1310-DELETE-DETAIL-COMPLETE.                                     EL179
00648      MOVE SPACES                 TO OPTIONO REPO.                 EL179
00649      MOVE ZEROS                  TO COUNT-1.                      EL179
00650      PERFORM 2200-FILL-SCREEN THRU 2210-EXIT                      EL179
00651          UNTIL SCREEN-FULL.                                       EL179
00652      MOVE ER-0000                TO EMI-ERROR.                    EL179
00653      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
00654      MOVE -1                     TO OPTIONL.                      EL179
00655      GO TO 8110-SEND-DATA.                                        EL179
00656      EJECT                                                        EL179
00657  1400-FIND-REPORT.                                                EL179
00658      EXEC CICS HANDLE CONDITION                                   EL179
00659          NOTFND   (1400-REPORT-NOTFND)                            EL179
00660          NOTOPEN  (1400-REPT-NOT-OPEN)                            EL179
00661      END-EXEC.                                                    EL179
00662                                                                   EL179
00663      EXEC CICS READ                                               EL179
00664          SET      (ADDRESS OF REPORT-SAVE-FILE)                      CL*10
00665          DATASET  ('ELREPT')                                      EL179
00666          RIDFLD   (REPT-KEY)                                      EL179
00667          GTEQ                                                     EL179
00668      END-EXEC.                                                    EL179
00669                                                                   EL179
00670      CONTINUE.                                                       CL*10
00671                                                                   EL179
00672      IF RF-REPORT-ID = REPI                                       EL179
00673          GO TO 1400-EXIT.                                         EL179
00674                                                                   EL179
00675  1400-REPORT-NOTFND.                                              EL179
00676      MOVE ER-0088                TO EMI-ERROR.                    EL179
00677      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
00678      MOVE 'X'                    TO ERROR-SWITCH.                 EL179
00679      GO TO 1400-EXIT.                                             EL179
00680                                                                   EL179
00681  1400-REPT-NOT-OPEN.                                              EL179
00682      MOVE ER-0275                TO EMI-ERROR.                    EL179
00683      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
00684      MOVE 'X'                    TO ERROR-SWITCH.                 EL179
00685                                                                   EL179
00686  1400-EXIT.                                                       EL179
00687      EXIT.                                                        EL179
00688      EJECT                                                        EL179
00689  1500-FIND-PRINTER.                                               EL179
00690      EXEC CICS HANDLE CONDITION                                   EL179
00691          NOTOPEN (1500-CNTL-NOT-OPEN)                             EL179
00692      END-EXEC.                                                    EL179
00693                                                                   EL179
00694      EXEC CICS READ                                               EL179
00695          SET      (ADDRESS OF CONTROL-FILE)                          CL*10
00696          DATASET  ('ELCNTL')                                      EL179
00697          RIDFLD   (CNTL-KEY)                                      EL179
00698      END-EXEC.                                                    EL179
00699                                                                   EL179
00700      CONTINUE.                                                       CL*10
00701      GO TO 1500-EXIT.                                             EL179
00702                                                                   EL179
00703  1500-CNTL-NOT-OPEN.                                              EL179
00704      MOVE ER-0042                TO EMI-ERROR.                    EL179
00705      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
00706      MOVE 'X'                    TO ERROR-SWITCH.                 EL179
00707                                                                   EL179
00708  1500-EXIT.                                                       EL179
00709      EXIT.                                                        EL179
00710      EJECT                                                        EL179
00711  1600-READ-PGMN.                                                  EL179
00712      MOVE SPACES                 TO PGMN-SWITCH                   EL179
00713                                                                   EL179
00714      EXEC CICS HANDLE CONDITION                                   EL179
00715          NOTFND (1600-EXIT)                                       EL179
00716      END-EXEC.                                                    EL179
00717                                                                   EL179
00718      EXEC CICS READ                                               EL179
00719          SET      (ADDRESS OF PROGRAM-DESCRIPTIONS)                  CL*10
00720          DATASET  ('ELPGMN')                                      EL179
00721          RIDFLD   (PGMN-KEY)                                      EL179
00722      END-EXEC.                                                    EL179
00723                                                                   EL179
00724      CONTINUE.                                                       CL*10
00725      MOVE '1'                    TO PGMN-SWITCH.                  EL179
00726                                                                   EL179
00727  1600-EXIT.                                                       EL179
00728      EXIT.                                                        EL179
00729      EJECT                                                        EL179
00730  2000-PAGE-FORWARD.                                               EL179
00731      MOVE ZEROS                  TO COUNT-1.                      EL179
00732      MOVE SPACES                 TO SCREEN-SWITCH MSGO REPT-KEY.  EL179
00733      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.            EL179
00734      MOVE '2'                    TO REPT-RECORD-TYPE.             EL179
00735      MOVE ZEROS                  TO REPT-LINE-NUMBER.             EL179
00736                                                                   EL179
00737      IF REPL = ZEROS                                              EL179
00738          MOVE PI-SAVE-ENDING     TO REPT-REPORT-ID                EL179
00739      ELSE                                                         EL179
00740          MOVE REPI               TO REPT-REPORT-ID.               EL179
00741                                                                   EL179
00742      EXEC CICS HANDLE CONDITION                                   EL179
00743          NOTOPEN  (3500-NOT-OPEN)                                 EL179
00744          NOTFND   (2010-REC-NOT-FND)                              EL179
00745      END-EXEC.                                                    EL179
00746                                                                   EL179
00747      PERFORM 7000-STARTBR THRU 7000-EXIT                          EL179
00748      PERFORM 2100-BUILD-FWD-PAGE THRU 2120-EXIT                   EL179
00749          UNTIL SCREEN-FULL OR END-OF-FILE.                        EL179
00750      PERFORM 7020-ENDBR THRU 7020-EXIT                            EL179
00751                                                                   EL179
00752      MOVE -1                     TO OPTIONL.                      EL179
00753                                                                      CL**9
00754      IF PI-DELETE-SW NOT EQUAL 'Y'                                   CL**9
00755          MOVE SPACES                 TO REPO.                        CL**9
00756                                                                      CL**9
00757      GO TO 8110-SEND-DATA.                                           CL**6
00758                                                                   EL179
00759  2010-REC-NOT-FND.                                                EL179
00760      MOVE HIGH-VALUES            TO PI-SAVE-BEGIN PI-SAVE-ENDING. EL179
00761      PERFORM 2200-FILL-SCREEN THRU 2210-EXIT                      EL179
00762          UNTIL SCREEN-FULL.                                       EL179
00763                                                                   EL179
00764      MOVE -1                     TO OPTIONL.                      EL179
00765      MOVE ER-0130                TO EMI-ERROR.                    EL179
00766                                                                   EL179
00767      PERFORM 9900-ERROR-FORMAT                                    EL179
00768          THRU 9900-EXIT.                                          EL179
00769                                                                   EL179
00770      GO TO 8110-SEND-DATA.                                        EL179
00771      EJECT                                                        EL179
00772  2100-BUILD-FWD-PAGE.                                             EL179
00773      EXEC CICS HANDLE CONDITION                                   EL179
00774          ENDFILE  (2110-END-FILE)                                 EL179
00775          NOTFND   (2110-END-FILE)                                 EL179
00776      END-EXEC.                                                    EL179
00777                                                                   EL179
00778      PERFORM 7010-READNEXT THRU 7010-EXIT.                        EL179
00779                                                                   EL179
00780      IF RF-RECORD-TYPE NOT = '2' OR                               EL179
00781         RF-COMPANY-CD  NOT = PI-COMPANY-CD                        EL179
00782          GO TO 2110-END-FILE.                                     EL179
00783                                                                   EL179
00784      MOVE SPACES                 TO DISPLAY-LINE.                 EL179
00785      MOVE RF-REPORT-ID           TO REP-ID PGMN-KEY.              EL179
00786      MOVE RF-CURRENT-DATE        TO REP-CREATED.                  EL179
00787      MOVE RF-PRINT-HH-MM-SS      TO TIME-UNFORMATTED.             EL179
00788      MOVE UN-HOURS               TO FOR-HOURS.                    EL179
00789      MOVE UN-MINUTES             TO FOR-MINUTES.                  EL179
00790      MOVE TIME-FORMATTED         TO REP-TIME.                     EL179
00791      MOVE RF-LINE-NUMBER         TO CONVERT-LINES.                EL179
00792      MOVE CONVERT-LINES          TO HOLD-LINES.                   EL179
00793      MOVE DISPLAY-LINES          TO EDIT-LINES.                   EL179
00794      MOVE EDIT-LINES             TO REP-LINES.                    EL179
00795      PERFORM 1600-READ-PGMN THRU 1600-EXIT.                       EL179
00796                                                                   EL179
00797      IF PGMN-FOUND                                                EL179
00798         MOVE PN-PROGRAM-DESCRIPTION TO REP-NAME                   EL179
00799      ELSE                                                         EL179
00800         MOVE 'DESCRIPTION NOT FOUND' TO REP-NAME.                 EL179
00801                                                                   EL179
00802      ADD 1 TO COUNT-1.                                            EL179
00803      MOVE DISPLAY-LINE           TO REPLINEO (COUNT-1).           EL179
00804                                                                   EL179
00805      IF COUNT-1 = 1                                               EL179
00806          MOVE RF-REPORT-ID       TO PI-SAVE-BEGIN.                EL179
00807                                                                   EL179
00808      IF COUNT-1 = 11                                              EL179
00809          MOVE RF-REPORT-ID       TO PI-SAVE-ENDING                EL179
00810          MOVE 'F'                TO SCREEN-SWITCH.                EL179
00811                                                                   EL179
00812      GO TO 2120-EXIT.                                             EL179
00813                                                                   EL179
00814  2110-END-FILE.                                                   EL179
00815      PERFORM 2200-FILL-SCREEN THRU 2210-EXIT                      EL179
00816          UNTIL SCREEN-FULL.                                       EL179
00817      MOVE ER-0130                TO EMI-ERROR.                    EL179
00818      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
00819      MOVE 'E'                    TO SCREEN-SWITCH.                EL179
00820                                                                   EL179
00821  2120-EXIT.                                                       EL179
00822      EXIT.                                                        EL179
00823                                                                   EL179
00824  2200-FILL-SCREEN.                                                EL179
00825      ADD 1 TO COUNT-1.                                            EL179
00826      MOVE SPACES                 TO REPLINEO (COUNT-1).           EL179
00827                                                                   EL179
00828      IF COUNT-1 GREATER THAN 10                                   EL179
00829          MOVE 'F'                TO SCREEN-SWITCH.                EL179
00830                                                                   EL179
00831  2210-EXIT.                                                       EL179
00832      EXIT.                                                        EL179
00833      EJECT                                                        EL179
00834  3000-PAGE-BACKWARD.                                              EL179
00835      MOVE 11                     TO COUNT-1.                      EL179
00836      MOVE SPACES                 TO SCREEN-SWITCH MSGO REPT-KEY.  EL179
00837      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.            EL179
00838      MOVE '2'                    TO REPT-RECORD-TYPE.             EL179
00839      MOVE ZEROS                  TO REPT-LINE-NUMBER.             EL179
00840                                                                   EL179
00841      IF REPL = ZEROS                                              EL179
00842          MOVE PI-SAVE-BEGIN      TO REPT-REPORT-ID                EL179
00843      ELSE                                                         EL179
00844          MOVE REPI               TO REPT-REPORT-ID.               EL179
00845                                                                   EL179
00846      EXEC CICS HANDLE CONDITION                                   EL179
00847          NOTOPEN  (3500-NOT-OPEN)                                 EL179
00848          NOTFND   (3020-REC-NOT-FND)                              EL179
00849      END-EXEC.                                                    EL179
00850                                                                   EL179
00851      PERFORM 7000-STARTBR THRU 7000-EXIT.                         EL179
00852                                                                   EL179
00853      IF REPT-KEY = HIGH-VALUES                                    EL179
00854          GO TO 3010-SKIP-RESET.                                   EL179
00855                                                                   EL179
00856      PERFORM 7010-READNEXT THRU 7010-EXIT.                        EL179
00857                                                                   EL179
00858      EXEC CICS RESETBR                                            EL179
00859          DATASET  ('ELREPT')                                      EL179
00860          RIDFLD   (REPT-KEY)                                      EL179
00861          EQUAL                                                    EL179
00862      END-EXEC.                                                    EL179
00863                                                                   EL179
00864  3010-SKIP-RESET.                                                 EL179
00865      PERFORM 3100-BUILD-BCK-PAGE THRU 3120-EXIT                   EL179
00866          UNTIL SCREEN-FULL OR END-OF-FILE.                        EL179
00867      PERFORM 7020-ENDBR THRU 7020-EXIT                            EL179
00868                                                                   EL179
00869      MOVE COUNT-1                TO COUNT-3.                      EL179
00870      PERFORM 3130-RAISE-PAGE THRU 3140-EXIT                       EL179
00871          COUNT-3 TIMES.                                           EL179
00872                                                                   EL179
00873      MOVE -1                     TO OPTIONL.                      EL179
00874      MOVE SPACES                 TO REPO.                         EL179
00875      GO TO 8110-SEND-DATA.                                        EL179
00876                                                                   EL179
00877                                                                   EL179
00878  3020-REC-NOT-FND.                                                EL179
00879      MOVE ER-0487                TO EMI-ERROR                     EL179
00880      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL179
00881      MOVE -1                     TO OPTIONL                       EL179
00882      GO TO 8110-SEND-DATA.                                        EL179
00883      EJECT                                                        EL179
00884  3100-BUILD-BCK-PAGE.                                             EL179
00885      EXEC CICS HANDLE CONDITION                                   EL179
00886          ENDFILE  (3110-END-FILE)                                 EL179
00887          NOTFND   (3110-END-FILE)                                 EL179
00888      END-EXEC.                                                    EL179
00889                                                                   EL179
00890      PERFORM 7040-READPREV THRU 7040-EXIT.                        EL179
00891                                                                   EL179
00892      IF RF-RECORD-TYPE NOT = '2' OR                                  CL**4
00893         RF-COMPANY-CD  NOT = PI-COMPANY-CD                           CL**4
00894          GO TO 3110-END-FILE.                                     EL179
00895                                                                   EL179
00896      MOVE SPACES                 TO DISPLAY-LINE.                 EL179
00897      MOVE RF-REPORT-ID           TO REP-ID PGMN-KEY.              EL179
00898      MOVE RF-CURRENT-DATE        TO REP-CREATED.                  EL179
00899      MOVE RF-PRINT-HH-MM-SS      TO TIME-UNFORMATTED.             EL179
00900      MOVE UN-HOURS               TO FOR-HOURS.                    EL179
00901      MOVE UN-MINUTES             TO FOR-MINUTES.                  EL179
00902      MOVE TIME-FORMATTED         TO REP-TIME.                     EL179
00903      MOVE RF-LINE-NUMBER         TO CONVERT-LINES.                EL179
00904      MOVE CONVERT-LINES          TO HOLD-LINES.                   EL179
00905      MOVE DISPLAY-LINES          TO EDIT-LINES.                   EL179
00906      MOVE EDIT-LINES             TO REP-LINES.                    EL179
00907      PERFORM 1600-READ-PGMN THRU 1600-EXIT.                       EL179
00908                                                                   EL179
00909      IF PGMN-FOUND                                                EL179
00910         MOVE PN-PROGRAM-DESCRIPTION TO REP-NAME                   EL179
00911      ELSE                                                         EL179
00912         MOVE 'DESCRIPTION NOT FOUND' TO REP-NAME.                 EL179
00913                                                                   EL179
00914      MOVE DISPLAY-LINE           TO REPLINEO (COUNT-1).           EL179
00915                                                                   EL179
00916      IF COUNT-1 = 11                                              EL179
00917          MOVE RF-REPORT-ID       TO PI-SAVE-ENDING.               EL179
00918                                                                   EL179
00919      IF COUNT-1 = 1                                               EL179
00920          MOVE RF-REPORT-ID       TO PI-SAVE-BEGIN                 EL179
00921          MOVE 'F'                TO SCREEN-SWITCH.                EL179
00922                                                                   EL179
00923      SUBTRACT 1 FROM COUNT-1.                                     EL179
00924      GO TO 3120-EXIT.                                             EL179
00925                                                                   EL179
00926  3110-END-FILE.                                                   EL179
00927      MOVE RF-REPORT-ID           TO PI-SAVE-BEGIN.                EL179
00928      MOVE ER-0131                TO EMI-ERROR.                    EL179
00929      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
00930      MOVE 'E'                    TO SCREEN-SWITCH.                EL179
00931                                                                   EL179
00932  3120-EXIT.                                                       EL179
00933      EXIT.                                                        EL179
00934                                                                   EL179
00935                                                                   EL179
00936  3130-RAISE-PAGE.                                                 EL179
00937      MOVE 1                      TO COUNT-1.                      EL179
00938      MOVE 2                      TO COUNT-2.                      EL179
00939      PERFORM 3150-SHIFT-SCREEN THRU 3160-EXIT                     EL179
00940           UNTIL COUNT-1 = 11.                                     EL179
00941      MOVE SPACES                 TO REPLINEO (COUNT-1).           EL179
00942                                                                   EL179
00943  3140-EXIT.                                                       EL179
00944      EXIT.                                                        EL179
00945                                                                   EL179
00946  3150-SHIFT-SCREEN.                                               EL179
00947      MOVE REPLINEO (COUNT-2)     TO REPLINEO (COUNT-1).           EL179
00948      ADD 1 TO COUNT-1 COUNT-2.                                    EL179
00949                                                                   EL179
00950  3160-EXIT.                                                       EL179
00951      EXIT.                                                        EL179
00952      EJECT                                                        EL179
00953  3500-NOT-OPEN.                                                   EL179
00954      MOVE ER-0275                TO EMI-ERROR.                    EL179
00955      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
00956      GO TO 8110-SEND-DATA.                                        EL179
00957      EJECT                                                        EL179
00958  5000-PROCESS-REVIEW.                                             EL179
00959      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL179
00960          MOVE LOW-VALUES         TO EL179BO                       EL179
00961          MOVE ER-7008            TO EMI-ERROR                     EL179
00962          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL179
00963          GO TO 8110-SEND-DATA.                                    EL179
00964                                                                   EL179
00965      EXEC CICS RECEIVE                                            EL179
00966          MAP     ('EL179B')                                       EL179
00967          MAPSET  ('EL179S')                                       EL179
00968      END-EXEC.                                                    EL179
00969                                                                   EL179
00970      IF PFKEYBL GREATER THAN ZERO                                 EL179
00971          PERFORM 5010-TRANS-PF THRU 5010-EXIT.                    EL179
00972                                                                   EL179
00973      IF SCREEN-ERROR                                              EL179
00974          MOVE ER-7008            TO EMI-ERROR                     EL179
00975          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL179
00976          GO TO 8150-SEND-DATA-B.                                  EL179
00977                                                                   EL179
00978      IF EIBAID = DFHPF1 OR DFHPF3                                 EL179
00979          GO TO 5100-PAGE-FORWARD.                                 EL179
00980                                                                   EL179
00981      IF EIBAID = DFHPF2 OR DFHPF4                                 EL179
00982          GO TO 5200-PAGE-BACKWARD.                                EL179
00983                                                                   EL179
00984      IF EIBAID = DFHPF5                                           EL179
00985          GO TO 5500-RESET-TOP.                                    EL179
00986                                                                   EL179
00987      IF EIBAID = DFHPF6                                           EL179
00988          GO TO 5600-RESET-BOTTOM.                                 EL179
00989                                                                   EL179
00990      IF EIBAID = DFHPF7                                           EL179
00991          GO TO 5700-SHIFT-VIEW.                                   EL179
00992                                                                   EL179
00993      IF EIBAID = DFHPF12                                          EL179
00994          GO TO 8300-GET-HELP.                                     EL179
00995                                                                   EL179
00996      IF EIBAID = DFHPF23                                          EL179
00997          GO TO 8810-PF23-ENTERED.                                 EL179
00998                                                                   EL179
00999      IF EIBAID = DFHPF24                                          EL179
01000          GO TO 8400-RETURN-MASTER.                                EL179
01001                                                                   EL179
01002      MOVE ER-0029                TO EMI-ERROR.                    EL179
01003      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
01004      MOVE -1                     TO PFKEYBL.                      EL179
01005      GO TO 8150-SEND-DATA-B.                                      EL179
01006                                                                   EL179
01007  5010-TRANS-PF.                                                   EL179
01008      IF EIBAID NOT = DFHENTER                                     EL179
01009          MOVE 'X'                TO ERROR-SWITCH                  EL179
01010          GO TO 5010-EXIT.                                            CL**2
01011                                                                   EL179
01012      IF PFKEYBI NOT NUMERIC                                       EL179
01013          MOVE 'X'                TO ERROR-SWITCH                  EL179
01014          GO TO 5010-EXIT.                                            CL**2
01015                                                                   EL179
01016      MOVE PFKEYBI                TO CHECK-PFKEYS.                 EL179
01017                                                                   EL179
01018      IF CHECK-PFKEYS LESS 1 OR GREATER 24                         EL179
01019          MOVE 'X'                TO ERROR-SWITCH                  EL179
01020          GO TO 5010-EXIT.                                            CL**2
01021                                                                   EL179
01022      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.                      EL179
01023                                                                   EL179
01024  5010-EXIT.                                                       EL179
01025      EXIT.                                                        EL179
01026      EJECT                                                        EL179
01027  5100-PAGE-FORWARD.                                               EL179
01028      IF EIBAID = DFHPF1                                           EL179
01029         ADD 16                   TO PI-START-COUNT                EL179
01030      ELSE                                                         EL179
01031         ADD 5                    TO PI-START-COUNT.               EL179
01032                                                                   EL179
01033      IF PI-START-COUNT NOT LESS THAN PI-TOTAL-COUNT               EL179
01034          GO TO 5600-RESET-BOTTOM.                                 EL179
01035                                                                   EL179
01036      PERFORM 6000-BUILD-FIRST THRU 6000-EXIT.                     EL179
01037      MOVE -1                     TO PFKEYBL.                      EL179
01038      GO TO 8150-SEND-DATA-B.                                      EL179
01039                                                                   EL179
01040  5200-PAGE-BACKWARD.                                              EL179
01041      IF EIBAID = DFHPF2                                           EL179
01042         SUBTRACT 1               FROM PI-START-COUNT              EL179
01043      ELSE                                                         EL179
01044         ADD  +10                 TO PI-START-COUNT.               EL179
01045                                                                   EL179
01046      IF PI-START-COUNT LESS THAN 1                                EL179
01047          GO TO 5500-RESET-TOP.                                    EL179
01048                                                                   EL179
01049      IF PI-START-COUNT LESS THAN PI-TOTAL-COUNT                   EL179
01050          NEXT SENTENCE                                            EL179
01051      ELSE                                                         EL179
01052          GO TO 5600-RESET-BOTTOM.                                 EL179
01053                                                                   EL179
01054      PERFORM 6010-BUILD-FIRST THRU 6010-EXIT.                     EL179
01055                                                                   EL179
01056      MOVE -1                     TO PFKEYBL.                      EL179
01057      GO TO 8150-SEND-DATA-B.                                      EL179
01058                                                                   EL179
01059  5500-RESET-TOP.                                                  EL179
01060      MOVE 1                      TO PI-START-COUNT.               EL179
01061      PERFORM 6000-BUILD-FIRST THRU 6000-EXIT.                     EL179
01062      MOVE -1                     TO PFKEYBL.                      EL179
01063      GO TO 8150-SEND-DATA-B.                                      EL179
01064      EJECT                                                        EL179
01065  5600-RESET-BOTTOM.                                               EL179
01066      MOVE PI-TOTAL-COUNT TO PI-START-COUNT.                       EL179
01067      SUBTRACT 1 FROM PI-START-COUNT.                              EL179
01068      PERFORM 6010-BUILD-FIRST THRU 6010-EXIT                      EL179
01069      MOVE -1 TO PFKEYBL.                                          EL179
01070      GO TO 8150-SEND-DATA-B.                                      EL179
01071                                                                   EL179
01072  5700-SHIFT-VIEW.                                                 EL179
01073      IF PI-VIEW-SWITCH = '1'                                      EL179
01074          MOVE '2'                TO PI-VIEW-SWITCH                EL179
01075      ELSE                                                         EL179
01076          MOVE '1'                TO PI-VIEW-SWITCH.               EL179
01077                                                                   EL179
01078      PERFORM 6000-BUILD-FIRST THRU 6000-EXIT.                     EL179
01079      MOVE -1                     TO PFKEYBL.                      EL179
01080      GO TO 8150-SEND-DATA-B.                                      EL179
01081      EJECT                                                        EL179
01082  6000-BUILD-FIRST.                                                EL179
01083      MOVE PI-COMPANY-CD          TO REPT-COMPANY-CODE.            EL179
01084      MOVE '1'                    TO REPT-RECORD-TYPE.             EL179
01085      MOVE PI-PRINT-REPORT        TO REPT-REPORT-ID REPIDO PGMN-KEYEL179
01086      MOVE PI-START-COUNT         TO REPT-LINE-NUMBER.             EL179
01087      PERFORM 1600-READ-PGMN THRU 1600-EXIT.                       EL179
01088                                                                   EL179
01089      IF PGMN-FOUND                                                EL179
01090         MOVE PN-PROGRAM-DESCRIPTION TO REPNMO                     EL179
01091      ELSE                                                         EL179
01092         MOVE 'DESCRIPTION NOT FOUND' TO REPNMO.                   EL179
01093                                                                   EL179
01094      MOVE ZEROS                  TO COUNT-1.                      EL179
01095                                                                   EL179
01096      EXEC CICS HANDLE CONDITION                                   EL179
01097          NOTFND   (6000-BUILD-NOT-FND)                               CL**4
01098          ENDFILE  (6000-BUILD-FIRST-END)                          EL179
01099      END-EXEC.                                                    EL179
01100                                                                   EL179
01101      PERFORM 7000-STARTBR THRU 7000-EXIT.                         EL179
01102                                                                   EL179
01103  6000-BUILD-LOOP.                                                 EL179
01104      EXEC CICS HANDLE CONDITION                                      CL**4
01105          NOTFND   (6000-BUILD-FIRST-END)                             CL**4
01106          ENDFILE  (6000-BUILD-FIRST-END)                             CL**4
01107      END-EXEC.                                                       CL**4
01108                                                                      CL**4
01109      PERFORM 7010-READNEXT THRU 7010-EXIT.                        EL179
01110                                                                   EL179
01111      IF RF-COMPANY-CD  NOT = PI-COMPANY-CD  OR                    EL179
01112         RF-RECORD-TYPE NOT = '1'            OR                    EL179
01113         RF-REPORT-ID   NOT = PI-PRINT-REPORT                      EL179
01114          GO TO 6000-BUILD-FIRST-END.                              EL179
01115                                                                   EL179
01116      ADD 1 TO COUNT-1.                                            EL179
01117                                                                   EL179
01118      IF PI-VIEW-SWITCH = '1'                                      EL179
01119         MOVE RF-DATA-2-81        TO LINEREPO (COUNT-1)            EL179
01120      ELSE                                                         EL179
01121         MOVE RF-DATA-55-133      TO LINEREPO (COUNT-1).           EL179
01122                                                                   EL179
01123      IF COUNT-1 = 1                                               EL179
01124          MOVE RF-LINE-NUMBER     TO PI-START-COUNT.               EL179
01125                                                                   EL179
01126      IF COUNT-1 LESS THAN 16                                      EL179
01127          GO TO 6000-BUILD-LOOP.                                   EL179
01128                                                                   EL179
01129      GO TO 6000-EXIT.                                             EL179
01130                                                                   EL179
01131  6000-BUILD-FIRST-END.                                            EL179
01132                                                                      CL**4
01133      PERFORM 6200-BLANK-SCREEN THRU 6200-EXIT                     EL179
01134          UNTIL COUNT-1 GREATER THAN 15.                           EL179
01135      PERFORM 7020-ENDBR THRU 7020-EXIT.                           EL179
01136      MOVE ER-0130                TO EMI-ERROR.                    EL179
01137      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
01138                                                                   EL179
01139      GO TO 6000-EXIT.                                                CL**4
01140                                                                      CL**4
01141  6000-BUILD-NOT-FND.                                                 CL**4
01142                                                                      CL**4
01143      PERFORM 6200-BLANK-SCREEN THRU 6200-EXIT                        CL**4
01144          UNTIL COUNT-1 GREATER THAN 15.                              CL**4
01145      MOVE ER-3340                TO EMI-ERROR.                       CL**4
01146      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01147                                                                      CL**4
01148      GO TO 6000-EXIT.                                                CL**4
01149  6000-EXIT.                                                       EL179
01150      EXIT.                                                        EL179
01151      EJECT                                                        EL179
01152  6010-BUILD-FIRST.                                                EL179
01153      MOVE PI-COMPANY-CD         TO REPT-COMPANY-CODE.             EL179
01154      MOVE '1'                   TO REPT-RECORD-TYPE.              EL179
01155      MOVE PI-PRINT-REPORT       TO REPT-REPORT-ID REPIDO PGMN-KEY.EL179
01156      MOVE PI-START-COUNT        TO REPT-LINE-NUMBER.              EL179
01157      PERFORM 1600-READ-PGMN THRU 1600-EXIT.                       EL179
01158                                                                   EL179
01159      IF PGMN-FOUND                                                EL179
01160         MOVE PN-PROGRAM-DESCRIPTION TO REPNMO                     EL179
01161      ELSE                                                         EL179
01162         MOVE 'DESCRIPTION NOT FOUND' TO REPNMO.                   EL179
01163                                                                   EL179
01164      MOVE 16                     TO COUNT-1.                      EL179
01165                                                                   EL179
01166      EXEC CICS HANDLE CONDITION                                   EL179
01167          NOTFND   (6010-BUILD-FIRST-END)                          EL179
01168          ENDFILE  (6010-BUILD-FIRST-END)                          EL179
01169      END-EXEC.                                                    EL179
01170                                                                   EL179
01171      PERFORM 7030-STARTBR THRU 7030-EXIT.                         EL179
01172                                                                   EL179
01173  6010-BUILD-LOOP.                                                 EL179
01174      PERFORM 7040-READPREV THRU 7040-EXIT.                        EL179
01175                                                                   EL179
01176      IF RF-COMPANY-CD  NOT = PI-COMPANY-CD OR                     EL179
01177         RF-RECORD-TYPE NOT = '1'           OR                     EL179
01178         RF-REPORT-ID   NOT = PI-PRINT-REPORT                      EL179
01179          GO TO 6010-BUILD-FIRST-END.                              EL179
01180                                                                   EL179
01181      IF PI-VIEW-SWITCH = '1'                                      EL179
01182         MOVE RF-DATA-2-81        TO LINEREPO (COUNT-1)            EL179
01183      ELSE                                                         EL179
01184         MOVE RF-DATA-55-133      TO LINEREPO (COUNT-1).           EL179
01185                                                                   EL179
01186      SUBTRACT 1 FROM COUNT-1.                                     EL179
01187                                                                   EL179
01188      IF COUNT-1 LESS THAN 1                                       EL179
01189         MOVE RF-LINE-NUMBER         TO PI-START-COUNT             EL179
01190         PERFORM 7020-ENDBR THRU 7020-EXIT                         EL179
01191         GO TO 6010-EXIT.                                          EL179
01192                                                                   EL179
01193      GO TO 6010-BUILD-LOOP.                                       EL179
01194                                                                   EL179
01195  6010-BUILD-FIRST-END.                                            EL179
01196      MOVE   1                    TO PI-START-COUNT.               EL179
01197      PERFORM 7020-ENDBR THRU 7020-EXIT.                           EL179
01198      MOVE ER-0131                TO EMI-ERROR.                    EL179
01199      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL179
01200      PERFORM 6000-BUILD-FIRST                                     EL179
01201         THRU 6000-EXIT.                                           EL179
01202                                                                   EL179
01203  6010-EXIT.                                                       EL179
01204      EXIT.                                                        EL179
01205      EJECT                                                        EL179
01206  6200-BLANK-SCREEN.                                               EL179
01207      ADD 1 TO COUNT-1.                                            EL179
01208      MOVE SPACES                 TO LINEREPO (COUNT-1).           EL179
01209                                                                   EL179
01210  6200-EXIT.                                                       EL179
01211      EXIT.                                                        EL179
01212                                                                   EL179
01213      EJECT                                                        EL179
01214  6500-PAGE-SEARCH.                                                EL179
01215      MOVE PI-COMPANY-CD         TO REPT-COMPANY-CODE.             EL179
01216      MOVE '1'                   TO REPT-RECORD-TYPE.              EL179
01217      MOVE PI-PRINT-REPORT       TO REPT-REPORT-ID REPIDO PGMN-KEY.EL179
01218      MOVE PI-START-COUNT        TO REPT-LINE-NUMBER.              EL179
01219      PERFORM 1600-READ-PGMN THRU 1600-EXIT.                       EL179
01220                                                                   EL179
01221      IF PGMN-FOUND                                                EL179
01222         MOVE PN-PROGRAM-DESCRIPTION TO REPNMO                     EL179
01223      ELSE                                                         EL179
01224         MOVE 'DESCRIPTION NOT FOUND' TO REPNMO.                   EL179
01225                                                                   EL179
01226      MOVE ZEROS                  TO COUNT-1.                      EL179
01227                                                                   EL179
01228      EXEC CICS HANDLE CONDITION                                   EL179
01229                                                                      CL**6
01230          ENDFILE  (6500-NOTFND)                                   EL179
01231      END-EXEC.                                                    EL179
01232                                                                   EL179
01233      PERFORM 7000-STARTBR THRU 7000-EXIT.                         EL179
01234                                                                   EL179
01235  6500-PAGE-LOOP.                                                  EL179
01236      PERFORM 7010-READNEXT THRU 7010-EXIT.                        EL179
01237                                                                   EL179
01238      IF RF-COMPANY-CD  NOT = PI-COMPANY-CD  OR                    EL179
01239         RF-RECORD-TYPE NOT = '1'            OR                    EL179
01240         RF-REPORT-ID   NOT = PI-PRINT-REPORT                      EL179
01241          GO TO 6500-NOTFND.                                       EL179
01242                                                                   EL179
01243      IF RF-CTL-CHAR-133 = '1'                                     EL179
01244          ADD +1  TO  PI-ACCUM-PAGE-NO.                            EL179
01245                                                                   EL179
01246      IF PI-ACCUM-PAGE-NO LESS THAN PI-START-PAGE-NO               EL179
01247          GO TO 6500-PAGE-LOOP.                                    EL179
01248                                                                   EL179
01249      MOVE REPT-LINE-NUMBER       TO PI-START-COUNT.               EL179
01250                                                                   EL179
01251  6500-NOTFND.                                                     EL179
01252      IF REPT-LINE-NUMBER = PI-START-COUNT                         EL179
01253          PERFORM 7020-ENDBR THRU 7020-EXIT                        EL179
01254          GO TO 6500-EXIT                                          EL179
01255      ELSE                                                         EL179
01256          MOVE ER-2359            TO EMI-ERROR                     EL179
01257          MOVE -1                 TO STPAGEL                       EL179
01258          MOVE 'X'                TO ERROR-SWITCH                  EL179
01259          MOVE AL-UNBON           TO STPAGEA                       EL179
01260          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL179
01261                                                                   EL179
01262      PERFORM 7020-ENDBR THRU 7020-EXIT.                           EL179
01263                                                                   EL179
01264      IF SCREEN-ERROR                                              EL179
01265          GO TO 8110-SEND-DATA.                                    EL179
01266                                                                   EL179
01267  6500-EXIT.                                                       EL179
01268      EXIT.                                                        EL179
01269                                                                   EL179
01270      EJECT                                                        EL179
01271  7000-STARTBR.                                                    EL179
01272      EXEC CICS STARTBR                                            EL179
01273          DATASET  ('ELREPT')                                      EL179
01274          RIDFLD   (REPT-KEY)                                      EL179
01275      END-EXEC.                                                    EL179
01276                                                                   EL179
01277      MOVE 'Y'                 TO PI-ELREPT-BROWSE-SW.                CL**4
01278                                                                      CL**4
01279  7000-EXIT.                                                       EL179
01280      EXIT.                                                        EL179
01281      EJECT                                                        EL179
01282  7010-READNEXT.                                                   EL179
01283      EXEC CICS READNEXT                                           EL179
01284          SET      (ADDRESS OF REPORT-SAVE-FILE)                      CL*10
01285          DATASET  ('ELREPT')                                      EL179
01286          RIDFLD   (REPT-KEY)                                      EL179
01287      END-EXEC.                                                    EL179
01288                                                                   EL179
01289      CONTINUE.                                                       CL*10
01290                                                                   EL179
01291  7010-EXIT.                                                       EL179
01292      EXIT.                                                        EL179
01293      EJECT                                                        EL179
01294  7020-ENDBR.                                                      EL179
01295      IF STARTED-BROWSE                                               CL**4
01296         EXEC CICS ENDBR                                              CL**4
01297             DATASET ('ELREPT')                                       CL**4
01298         END-EXEC.                                                    CL**4
01299                                                                      CL**4
01300      MOVE SPACES               TO PI-ELREPT-BROWSE-SW.               CL**4
01301                                                                   EL179
01302  7020-EXIT.                                                       EL179
01303      EXIT.                                                        EL179
01304      EJECT                                                        EL179
01305  7030-STARTBR.                                                    EL179
01306      EXEC CICS STARTBR                                            EL179
01307          DATASET  ('ELREPT')                                      EL179
01308          RIDFLD   (REPT-KEY)                                      EL179
01309          EQUAL                                                    EL179
01310      END-EXEC.                                                    EL179
01311                                                                      CL**4
01312      MOVE 'Y'                  TO PI-ELREPT-BROWSE-SW.               CL**4
01313                                                                   EL179
01314  7030-EXIT.                                                       EL179
01315      EXIT.                                                        EL179
01316      EJECT                                                        EL179
01317  7040-READPREV.                                                   EL179
01318      EXEC CICS READPREV                                           EL179
01319          SET      (ADDRESS OF REPORT-SAVE-FILE)                      CL*10
01320          DATASET  ('ELREPT')                                      EL179
01321          RIDFLD   (REPT-KEY)                                      EL179
01322      END-EXEC.                                                    EL179
01323                                                                   EL179
01324      CONTINUE.                                                       CL*10
01325                                                                   EL179
01326  7040-EXIT.                                                       EL179
01327      EXIT.                                                        EL179
01328      EJECT                                                        EL179
01329  8100-SEND-MAP.                                                   EL179
01330      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.                EL179
01331                                                                   EL179
01332      EXEC CICS SEND                                               EL179
01333          MAP     ('EL179A')                                       EL179
01334          MAPSET  ('EL179S')                                       EL179
01335          ERASE                                                    EL179
01336          FREEKB                                                   EL179
01337          CURSOR                                                   EL179
01338      END-EXEC.                                                    EL179
01339                                                                   EL179
01340      GO TO 9000-RETURN-TRANS.                                     EL179
01341                                                                   EL179
01342  8110-SEND-DATA.                                                  EL179
01343                                                                      CL**6
01344      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.                EL179
01345                                                                   EL179
01346      EXEC CICS SEND                                               EL179
01347          MAP     ('EL179A')                                       EL179
01348          MAPSET  ('EL179S')                                       EL179
01349          ERASE                                                       CL**6
01350          FREEKB                                                   EL179
01351          CURSOR                                                   EL179
01352      END-EXEC.                                                    EL179
01353                                                                   EL179
01354      GO TO 9000-RETURN-TRANS.                                     EL179
01355                                                                   EL179
01356  8120-FORMAT-TIME-DATE.                                           EL179
01357      MOVE SAVE-DATE              TO DATEO.                        EL179
01358      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL*10
01359      END-EXEC                                                        CL*10
01360      EXEC CICS FORMATTIME                                            CL*10
01361                ABSTIME(LCP-CICS-TIME)                                CL*10
01362                TIME(LCP-TIME-OF-DAY-XX)                              CL*10
01363      END-EXEC                                                        CL*10
01364      MOVE  LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.                   CL*10
01365      MOVE UN-HOURS               TO FOR-HOURS.                    EL179
01366      MOVE UN-MINUTES             TO FOR-MINUTES.                  EL179
01367      MOVE TIME-FORMATTED         TO TIMEO.                        EL179
01368      MOVE LIT-MAP                TO PI-CURRENT-SCREEN-NO.         EL179
01369      MOVE EMI-MESSAGE-AREA (1)   TO MSGO.                         EL179
01370      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL179
01371                                                                   EL179
01372  8130-EXIT.                                                       EL179
01373      EXIT.                                                        EL179
01374                                                                   EL179
01375  8140-SEND-MAP-B.                                                 EL179
01376      PERFORM 8160-FORMAT-TIME-DATE THRU 8170-EXIT.                EL179
01377                                                                   EL179
01378      EXEC CICS SEND                                               EL179
01379          MAP     ('EL179B')                                       EL179
01380          MAPSET  ('EL179S')                                       EL179
01381          ERASE                                                    EL179
01382          FREEKB                                                   EL179
01383          CURSOR                                                   EL179
01384      END-EXEC.                                                    EL179
01385                                                                   EL179
01386      GO TO 9000-RETURN-TRANS.                                     EL179
01387                                                                   EL179
01388  8150-SEND-DATA-B.                                                EL179
01389      PERFORM 8160-FORMAT-TIME-DATE THRU 8170-EXIT.                EL179
01390                                                                   EL179
01391      EXEC CICS SEND                                               EL179
01392          MAP     ('EL179B')                                       EL179
01393          MAPSET  ('EL179S')                                       EL179
01394          DATAONLY                                                 EL179
01395          FREEKB                                                   EL179
01396          CURSOR                                                   EL179
01397      END-EXEC.                                                    EL179
01398                                                                   EL179
01399      GO TO 9000-RETURN-TRANS.                                     EL179
01400                                                                   EL179
01401  8160-FORMAT-TIME-DATE.                                           EL179
01402      MOVE SAVE-DATE              TO DATEBO.                       EL179
01403      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL*10
01404      END-EXEC                                                        CL*10
01405      EXEC CICS FORMATTIME                                            CL*10
01406                ABSTIME(LCP-CICS-TIME)                                CL*10
01407                TIME(LCP-TIME-OF-DAY-XX)                              CL*10
01408      END-EXEC                                                        CL*10
01409      MOVE  LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.                   CL*10
01410      MOVE UN-HOURS               TO FOR-HOURS.                    EL179
01411      MOVE UN-MINUTES             TO FOR-MINUTES.                  EL179
01412      MOVE TIME-FORMATTED         TO TIMEBO.                       EL179
01413      MOVE LIT-MAP-2              TO PI-CURRENT-SCREEN-NO.         EL179
01414      MOVE EMI-MESSAGE-AREA (1)   TO MSGBO.                        EL179
01415      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL179
01416                                                                   EL179
01417  8170-EXIT.                                                       EL179
01418      EXIT.                                                        EL179
01419                                                                   EL179
01420  8200-RETURN-PRIOR.                                               EL179
01421      IF PI-CURRENT-SCREEN-NO = LIT-MAP-2                          EL179
01422          MOVE HIGH-VALUES        TO PI-SAVE-BEGIN                 EL179
01423          MOVE LOW-VALUES         TO PI-SAVE-ENDING                EL179
01424          MOVE LOW-VALUES         TO EL179AO                       EL179
01425          MOVE -1                 TO OPTIONL                       EL179
01426          GO TO 2000-PAGE-FORWARD.                                    CL**6
01427                                                                   EL179
01428      MOVE SPACE                  TO PI-RETURN-CD-1.               EL179
01429      MOVE PI-RETURN-TO-PROGRAM   TO CALL-PGM.                     EL179
01430      GO TO 9200-XCTL.                                             EL179
01431                                                                   EL179
01432  8300-GET-HELP.                                                   EL179
01433      MOVE XCTL-EL010             TO CALL-PGM.                     EL179
01434      GO TO 9200-XCTL.                                             EL179
01435                                                                   EL179
01436  8400-RETURN-MASTER.                                              EL179
01437      MOVE SPACE                  TO PI-RETURN-CD-1.               EL179
01438                                                                   EL179
01439      IF  CREDIT-SESSION                                              CL**5
01440              OR                                                      CL**5
01441          CLAIM-SESSION                                               CL**5
01442          MOVE XCTL-EL126         TO CALL-PGM                         CL**5
01443                                                                   EL179
01444      ELSE                                                            CL**5
01445          IF  MORTGAGE-SESSION                                        CL**5
01446              MOVE XCTL-EM626     TO CALL-PGM                         CL**5
01447                                                                      CL**5
01448          ELSE                                                        CL**5
01449              IF  WARRANTY-SESSION                                    CL**5
01450                  MOVE XCTL-GL800 TO CALL-PGM.                        CL**5
01451                                                                   EL179
01452      GO TO 9200-XCTL.                                             EL179
01453                                                                   EL179
01454  8800-UNAUTHORIZED-ACCESS.                                        EL179
01455      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL179
01456      GO TO 8990-SEND-TEXT.                                        EL179
01457                                                                   EL179
01458  8810-PF23-ENTERED.                                               EL179
01459      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL179
01460      MOVE XCTL-EL005             TO CALL-PGM.                     EL179
01461      GO TO 9200-XCTL.                                             EL179
01462                                                                   EL179
01463  8820-XCTL-ERROR.                                                 EL179
01464      EXEC CICS HANDLE CONDITION                                   EL179
01465          PGMIDERR (8990-SEND-TEXT)                                EL179
01466      END-EXEC.                                                    EL179
01467                                                                   EL179
01468      MOVE SPACE                  TO PI-ENTRY-CD-1.                EL179
01469      MOVE CALL-PGM               TO PI-CALLING-PROGRAM LOGOFF-PGM.EL179
01470      MOVE XCTL-EL005             TO CALL-PGM.                     EL179
01471      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL179
01472      GO TO 9200-XCTL.                                             EL179
01473                                                                   EL179
01474  8990-SEND-TEXT.                                                  EL179
01475      EXEC CICS SEND TEXT                                          EL179
01476          FROM    (LOGOFF-TEXT)                                    EL179
01477          LENGTH  (LOGOFF-LENGTH)                                  EL179
01478          ERASE                                                    EL179
01479          FREEKB                                                   EL179
01480      END-EXEC.                                                    EL179
01481                                                                   EL179
01482      GO TO 9100-RETURN-CICS.                                      EL179
01483      EJECT                                                        EL179
01484  9000-RETURN-TRANS.                                               EL179
01485      EXEC CICS RETURN                                             EL179
01486          TRANSID   (TRANS-ID)                                     EL179
01487          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL179
01488          LENGTH    (PI-COMM-LENGTH)                               EL179
01489      END-EXEC.                                                    EL179
01490                                                                   EL179
01491  9100-RETURN-CICS.                                                EL179
01492      EXEC CICS RETURN                                             EL179
01493      END-EXEC.                                                    EL179
01494                                                                   EL179
01495      GOBACK.                                                      EL179
01496                                                                   EL179
01497  9200-XCTL.                                                       EL179
01498      EXEC CICS XCTL                                               EL179
01499          PROGRAM   (CALL-PGM)                                     EL179
01500          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL179
01501          LENGTH    (PI-COMM-LENGTH)                               EL179
01502      END-EXEC.                                                    EL179
01503                                                                   EL179
01504  9700-LINK-DATE-CONVERT.                                          EL179
01505      EXEC CICS LINK                                               EL179
01506          PROGRAM    ('ELDATCV')                                   EL179
01507          COMMAREA   (DATE-CONVERSION-DATA)                        EL179
01508          LENGTH     (DC-COMM-LENGTH)                              EL179
01509          END-EXEC.                                                EL179
01510                                                                   EL179
01511  9700-EXIT.                                                       EL179
01512      EXIT.                                                        EL179
01513                                                                   EL179
01514  9900-ERROR-FORMAT.                                               EL179
01515      IF EMI-ERRORS-COMPLETE                                       EL179
01516          GO TO 9900-EXIT.                                         EL179
01517                                                                   EL179
01518      EXEC CICS LINK                                               EL179
01519          PROGRAM   ('EL001')                                      EL179
01520          COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)                EL179
01521          LENGTH    (EMI-COMM-LENGTH)                              EL179
01522      END-EXEC.                                                    EL179
01523                                                                   EL179
01524  9900-EXIT.                                                       EL179
01525      EXIT.                                                        EL179
01526      EJECT                                                           CL**4
01527  9910-INITIALIZE-SECURITY.                                           CL**4
01528 ******************************************************************   CL**4
01529 *                                                                *   CL**4
01530 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *   CL**4
01531 *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *   CL**4
01532 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *   CL**4
01533 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *   CL**4
01534 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *   CL**4
01535 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *   CL**4
01536 *                                                                *   CL**4
01537 ******************************************************************   CL**4
01538                                                                   EL179
01539      IF  PI-PROCESSOR-ID NOT EQUAL 'LGXX'                            CL**4
01540                                                                      CL**4
01541          IF  MORTGAGE-SESSION                                        CL**4
01542              MOVE '125E'                 TO SC-QUID-SYSTEM           CL**4
01543              MOVE EIBTRMID               TO SC-QUID-TERMINAL         CL**4
01544                                                                      CL**4
01545              EXEC CICS READQ TS                                      CL**4
01546                  QUEUE  (SC-QUID-KEY)                                CL**4
01547                  INTO   (SECURITY-CONTROL-E)                         CL**4
01548                  LENGTH (SC-COMM-LENGTH-E)                           CL**4
01549                  ITEM   (SC-ITEM)                                    CL**4
01550              END-EXEC                                                CL**4
01551                                                                      CL**4
01552              MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)                   CL**4
01553                                      TO PI-DISPLAY-CAP               CL**4
01554              MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)                    CL**4
01555                                      TO PI-MODIFY-CAP                CL**4
01556                                                                      CL**4
01557              IF  NOT DISPLAY-CAP                                     CL**4
01558                  MOVE 'READ'         TO SM-READ                      CL**4
01559                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT      CL**4
01560                  MOVE ER-9097        TO EMI-ERROR                    CL**4
01561                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**4
01562                  GO TO 8100-SEND-MAP                                 CL**4
01563                                                                      CL**4
01564              ELSE                                                    CL**4
01565                  NEXT SENTENCE                                       CL**4
01566          ELSE                                                        CL**7
01567              IF CLAIM-SESSION                                        CL**7
01568                  EXEC CICS READQ TS                                  CL**7
01569                      QUEUE    (PI-SECURITY-TEMP-STORE-ID)            CL**7
01570                      INTO     (SECURITY-CONTROL)                     CL**7
01571                      LENGTH   (SC-COMM-LENGTH)                       CL**7
01572                      ITEM     (SC-ITEM)                              CL**7
01573                  END-EXEC                                            CL**7
01574              MOVE SC-CLAIMS-DISPLAY (27)  TO  PI-DISPLAY-CAP         CL**7
01575              MOVE SC-CLAIMS-UPDATE  (27)  TO  PI-MODIFY-CAP          CL**7
01576              IF NOT DISPLAY-CAP                                      CL**7
01577                  MOVE 'READ'              TO  SM-READ                CL**7
01578                  PERFORM 9995-SECURITY-VIOLATION                     CL**7
01579                  MOVE ER-0070             TO  EMI-ERROR              CL**7
01580                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**7
01581                  GO TO 8100-SEND-MAP                                 CL**7
01582              ELSE                                                    CL**7
01583                  NEXT SENTENCE                                       CL**7
01584          ELSE                                                        CL**4
01585              MOVE 'Y'                TO PI-DISPLAY-CAP               CL**4
01586                                         PI-MODIFY-CAP.               CL**4
01587                                                                      CL**4
01588  9910-EXIT.                                                          CL**4
01589      EXIT.                                                           CL**4
01590      EJECT                                                           CL**4
01591  9990-ABEND.                                                      EL179
01592      MOVE DFHEIBLK               TO EMI-LINE1.                    EL179
01593                                                                   EL179
01594      EXEC CICS LINK                                               EL179
01595          PROGRAM   ('EL004')                                      EL179
01596          COMMAREA  (EMI-LINE1)                                    EL179
01597          LENGTH    (72)                                           EL179
01598      END-EXEC.                                                    EL179
01599                                                                   EL179
01600      GO TO 8110-SEND-DATA.                                        EL179
01601      EJECT                                                           CL**4
01602  9995-SECURITY-VIOLATION.                                            CL**4
01603 ******************************************************************   CL**4
01604 *                                                                *   CL**4
01605 *    THIS LOGIC CONTAINS THE COMMON SECURITY-MESSAGE LINK        *   CL**4
01606 *    THE COPYBOOK ELCSCTM MUST RESIDE IN WORKING-STORAGE.        *   CL**4
01607 *                                                                *   CL**4
01608 *    THE FOLLOWING ADDITION FIELDS MUST RESIDE IN WORKING-       *   CL**4
01609 *                                                                *   CL**4
01610 *    THIS-PGM            PIC  X(08)                              *   CL**4
01611 *                                                                *   CL**4
01612 *    W-TIME-IN           PIC S9(07).                             *   CL**4
01613 *    FILLER   REDEFINES W-TIME-IN.                               *   CL**4
01614 *    FILLER              PIC  X(01).                             *   CL**4
01615 *    W-TIME-OUT          PIC  9(02)V9(02).                       *   CL**4
01616 *    FILLER              PIC  X(02).                             *   CL**4
01617 *                                                                *   CL**4
01618 ******************************************************************   CL**4
01619                                                                      CL**4
01620      MOVE EIBDATE          TO SM-JUL-DATE.                           CL**4
01621      MOVE EIBTRMID         TO SM-TERMID.                             CL**4
01622      MOVE THIS-PGM         TO SM-PGM.                                CL**4
01623      MOVE EIBTIME          TO W-TIME-IN.                             CL**4
01624      MOVE W-TIME-OUT       TO SM-TIME.                               CL**4
01625      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.                       CL**4
01626                                                                      CL**4
01627      EXEC CICS LINK                                                  CL**4
01628           PROGRAM  ('EL003')                                         CL**4
01629           COMMAREA (SECURITY-MESSAGE)                                CL**4
01630           LENGTH   (80)                                              CL**4
01631      END-EXEC.                                                       CL**4
01632                                                                      CL**4
01633  9995-EXIT.                                                          CL**4
01634      EXIT.                                                           CL**4
