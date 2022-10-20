00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL65091
00003  PROGRAM-ID.                 EL65091.                                LV003
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 02/12/96 08:35:33.                    CL**2
00007 *                            VMOD=2.003.                             CL**3
00008 *                                                                 EL65091
00008 *                                                                 EL65091
00009 *AUTHOR.     LOGIC INC.                                              CL**2
00010 *            DALLAS, TEXAS.                                          CL**2
00011                                                                   EL65091
00012 *DATE-COMPILED.                                                      CL**2
00013 *SECURITY.   *****************************************************   CL**2
00014 *            *                                                   *   CL**2
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**2
00016 *            *                                                   *   CL**2
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00020 *            *                                                   *   CL**2
00021 *            *****************************************************   CL**2
00022                                                                   EL65091
00023 *REMARKS.    TRANSACTION - EXDB - ACCOUNT NOTEPAD PRINT ROUTINE.     CL**2
00024                                                                   EL65091
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
110706* 110706  CR2006071700004  PEMA  ADD BRANCH LOCATIONS
110706*           AND SHIPPING ADDRESS TO ACCOUNT NOTES FILE
101101******************************************************************
00025  ENVIRONMENT DIVISION.                                            EL65091
00026                                                                   EL65091
00027      EJECT                                                        EL65091
00028  DATA DIVISION.                                                   EL65091
00029  WORKING-STORAGE SECTION.                                         EL65091
00030  77  FILLER  PIC X(32) VALUE '********************************'.  EL65091
00031  77  FILLER  PIC X(32) VALUE '*   EL65091  WORKING-STORAGE   *'.  EL65091
00032  77  FILLER  PIC X(32) VALUE '*********** V/M 2.003 **********'.     CL**3
00033                                                                   EL65091
00034  77  WS-LEN  PIC S9(4)  COMP    VALUE +326.                       EL65091
00035                                                                   EL65091
00036  77  WS-SAVED-PI-COMPANY-ID     PIC XXX.                             CL**3
00037                                                                      CL**3
00038  01  STANDARD-AREAS.                                              EL65091
00039      12  W-DELAY-CNT             PIC S9(3)  COMP-3 VALUE ZERO.    EL65091
00040      12  W-DELAY-INTERVAL        PIC S9(7)  COMP-3 VALUE +2.      EL65091
00041      12  PGM-NAME                PIC X(8).                        EL65091
00042      12  LINK-001                PIC X(8)    VALUE  'EL001'.      EL65091
00043      12  LINK-004                PIC X(8)    VALUE  'EL004'.      EL65091
00044      12  LINK-ELDATCV            PIC X(8)    VALUE  'ELDATCV'.    EL65091
00045      12  ERROR-LINE              PIC X(70).                          CL**3
00046                                                                   EL65091
00047      12  W-DATE-AREA.                                             EL65091
00048          16  W-CURRENT-DATE      PIC X(8)    VALUE SPACES.        EL65091
00049          16  W-CURRENT-BIN-DATE  PIC X(2)    VALUE SPACES.        EL65091
00050                                                                   EL65091
00051      12  LINE-CNT                PIC S9(3)  COMP-3 VALUE ZERO.    EL65091
00052      12  WS-TERMINAL-ID.                                          EL65091
00053          16  WS-TERM-PREFIX      PIC XX.                          EL65091
00054          16  FILLER              PIC XX.                          EL65091
00055      12  ERACNT-FILE-ID          PIC X(8) VALUE 'ERACNT'.         EL65091
00056      12  ERACCT-FILE-ID          PIC X(8) VALUE 'ERACCT'.         EL65091
00057      12  FIRST-TIME-SW           PIC X  VALUE 'Y'.                EL65091
00058          88 FIRST-ONE            VALUE 'Y'.                       EL65091
00059      12  ERACNT-KEY-LENGTH       PIC S9(4)   COMP  VALUE +23.     EL65091
00060      12  ERACNT-START-LENGTH     PIC S9(4)   COMP  VALUE +21.     EL65091
00061                                                                   EL65091
00062      12  ERACNT-KEY.                                              EL65091
00063          16  ERACNT-PARTIAL-KEY.                                  EL65091
00064              20  ERACNT-COMPANY-CD    PIC X.                      EL65091
00066              20  ERACNT-CARRIER       PIC X.                      EL65091
00067              20  ERACNT-GROUPING      PIC X(06).                  EL65091
00068              20  ERACNT-STATE         PIC X(02).                  EL65091
00069              20  ERACNT-ACCOUNT       PIC X(10).                  EL65091
110706             20  ERACNT-REC-TYP       PIC X.                      EL65091
00070          16  ERACNT-SEQ          PIC S9(4) COMP.                  EL65091
00071                                                                   EL65091
00072      12  ERACCT-KEY.                                              EL65091
00073          16  ERACCT-PARTIAL-KEY.                                  EL65091
00074              20  ERACCT-COMPANY-CD    PIC X.                      EL65091
00075              20  ERACCT-CARRIER       PIC X.                      EL65091
00076              20  ERACCT-GROUPING      PIC X(06).                  EL65091
00077              20  ERACCT-STATE         PIC X(02).                  EL65091
00078              20  ERACCT-ACCOUNT       PIC X(10).                  EL65091
00079          16  ERACCT-EXP-DT       PIC XX.                          EL65091
00080                                                                   EL65091
00081      12  W-PRIOR-KEY.                                             EL65091
00082          16  W-PRIOR-PARTIAL-KEY.                                 EL65091
00083              20  W-PRIOR-COMPANY-CD   PIC X.                      EL65091
00085              20  W-PRIOR-CARRIER      PIC X.                      EL65091
00086              20  W-PRIOR-GROUPING     PIC X(06).                  EL65091
00087              20  W-PRIOR-STATE        PIC X(02).                  EL65091
00088              20  W-PRIOR-ACCOUNT      PIC X(10).                  EL65091
110706             20  W-PRIOR-REC-TYP      PIC X.                      EL65091
00089          16  W-PRIOR-SEQ         PIC S9(4) COMP.                  EL65091
00090                                                                   EL65091
00091  EJECT                                                            EL65091
00092  01  PRT-LINES.                                                   EL65091
00093      05  HEAD-LINE-1.                                             EL65091
00094          10  FILLER              PIC X(01)    VALUE SPACES.       EL65091
00095          10  H1-DATE             PIC X(08)    VALUE SPACES.       EL65091
00096          10  FILLER              PIC X(22)    VALUE SPACES.       EL65091
00097          10  FILLER              PIC X(18)    VALUE               EL65091
00098              ' ACCOUNT NOTEPAD  '.                                EL65091
00099          10  FILLER              PIC X(23)    VALUE SPACES.       EL65091
00100          10  THIS-PGM            PIC X(7)     VALUE 'EL65091'.       CL**3
00101          10  FILLER              PIC X(01)    VALUE SPACES.       EL65091
00102                                                                   EL65091
00103      05  HEAD-LINE-ACCOUNT.                                       EL65091
00104          10  FILLER              PIC X(01)    VALUE SPACES.       EL65091
00105          10  FILLER              PIC X(07)    VALUE               EL65091
00106             'CARRIER'.                                            EL65091
00107          10  FILLER              PIC X(01)    VALUE SPACES.       EL65091
00108          10  FILLER              PIC X(08)    VALUE               EL65091
00109             'GROUPING'.                                           EL65091
00110          10  FILLER              PIC X(02)    VALUE SPACES.       EL65091
00111          10  FILLER              PIC X(05)    VALUE               EL65091
00112             'STATE'.                                              EL65091
00113          10  FILLER              PIC X(04)    VALUE SPACES.       EL65091
00114          10  FILLER              PIC X(07)    VALUE               EL65091
00115             'ACCOUNT'.                                            EL65091
00116          10  FILLER              PIC X(07)    VALUE SPACES.       EL65091
00117          10  FILLER              PIC X(04)    VALUE               EL65091
00118             'NAME'.                                               EL65091
00119          10  FILLER              PIC X(34)    VALUE SPACES.       EL65091
00120                                                                   EL65091
00121      05  DETAIL-LINE-ACCOUNT.                                     EL65091
00122          10  FILLER              PIC X(01)    VALUE SPACES.       EL65091
00123          10  FILLER              PIC X(03)    VALUE SPACES.       EL65091
00124          10  DA-CARRIER          PIC X(01)    VALUE SPACES.       EL65091
00125          10  FILLER              PIC X(05)    VALUE SPACES.       EL65091
00126          10  DA-GROUPING         PIC X(06)    VALUE SPACES.       EL65091
00127          10  FILLER              PIC X(05)    VALUE SPACES.       EL65091
00128          10  DA-STATE            PIC X(02)    VALUE SPACES.       EL65091
00129          10  FILLER              PIC X(04)    VALUE SPACES.       EL65091
00130          10  DA-ACCOUNT          PIC X(10)    VALUE SPACES.       EL65091
00131          10  FILLER              PIC X(05)    VALUE SPACES.       EL65091
00132          10  DA-ACCT-NAME        PIC X(30)    VALUE SPACES.       EL65091
00133          10  FILLER              PIC X(08)    VALUE SPACES.       EL65091
00134                                                                   EL65091
00135      05  HEAD-LINE-2.                                             EL65091
00136          10  FILLER             PIC X(69)    VALUE SPACES.        EL65091
00137          10  FILLER             PIC X(10)    VALUE                EL65091
00138             'CHG       '.                                         EL65091
00139          10  FILLER             PIC X(01)    VALUE SPACES.        EL65091
00140                                                                   EL65091
00141      05  HEAD-LINE-3.                                             EL65091
00142          10  FILLER             PIC X(01)    VALUE SPACES.        EL65091
00143          10  FILLER             PIC X(12)    VALUE                EL65091
00144              'LINE    TEXT'.                                      EL65091
00145          10  FILLER             PIC X(57)    VALUE SPACES.        EL65091
00146          10  FILLER             PIC X(09)    VALUE                EL65091
00147             'BY  DATE '.                                          EL65091
00148          10  FILLER             PIC X(01)    VALUE SPACES.        EL65091
00149                                                                   EL65091
00150      05  DETAIL-LINE-1.                                           EL65091
00151          10  FILLER             PIC X        VALUE SPACES.        EL65091
00152          10  D1-LINE-NO         PIC Z99      VALUE ZEROS.         EL65091
00153          10  FILLER             PIC X(01)    VALUE ' '.           EL65091
00154          10  D1-TEXT            PIC X(60)    VALUE '*'.           EL65091
00155          10  FILLER             PIC X(01)    VALUE SPACES.        EL65091
00156          10  FILLER             PIC X(01)    VALUE SPACES.        EL65091
00157          10  FILLER             PIC X(01)    VALUE SPACES.        EL65091
00158          10  D1-MAINT-BY        PIC X(04)    VALUE SPACES.        EL65091
00159          10  FILLER             PIC X        VALUE SPACES.        EL65091
00160          10  D1-MAINT-DT        PIC X(6)     VALUE SPACES.        EL65091
00161          10  FILLER             PIC X        VALUE SPACES.        EL65091
00162                                  COPY ELCDMD34.                      CL**3
00163      EJECT                                                        EL65091
00164                                  COPY ELCAID.                     EL65091
00165  01  PF-AID REDEFINES DFHAID.                                     EL65091
00166      05  FILLER                  PIC X(8).                        EL65091
00167      05  PF-VALUES  OCCURS 24    PIC X.                           EL65091
00168      EJECT                                                        EL65091
00169                                  COPY ELPRTCVD.                   EL65091
00170      EJECT                                                        EL65091
00171                                  COPY ELCINTF.                    EL65091
00172      EJECT                                                        EL65091
00173                                  COPY ELC650PI.                   EL65091
00174      EJECT                                                        EL65091
00175                                  COPY ELCDATE.                    EL65091
00176      EJECT                                                        EL65091
00177                                                                   EL65091
00178  LINKAGE SECTION.                                                 EL65091
00179  01  DFHCOMMAREA                 PIC X(1500).                     EL65091
00180                                                                   EL65091
00181      EJECT                                                        EL65091
00182 *01  PARMLIST.                                                       CL**2
00183 *    05  FILLER                  PIC S9(8)   COMP.                   CL**2
00184 *    05  ERACNT-POINTER          PIC S9(8)   COMP.                   CL**2
00185 *    05  ERACCT-POINTER          PIC S9(8)   COMP.                   CL**2
00186      EJECT                                                        EL65091
00187                                  COPY ERCACNT.                    EL65091
00188      EJECT                                                        EL65091
00189                                  COPY ERCACCT.                    EL65091
00190      EJECT                                                        EL65091
00191  PROCEDURE DIVISION.                                              EL65091
00192                                                                   EL65091
00193      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL65091
00194      MOVE '5'                    TO DC-OPTION-CODE.               EL65091
00195      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL65091
00196      MOVE DC-GREG-DATE-1-EDIT    TO  W-CURRENT-DATE.              EL65091
00197      MOVE DC-BIN-DATE-1          TO  W-CURRENT-BIN-DATE.          EL65091
00198      MOVE SPACES                 TO DL34-PROCESS-TYPE.               CL**3
00199                                                                   EL65091
00200  0100-RETRIEVE.                                                   EL65091
00201      EXEC CICS  HANDLE CONDITION                                  EL65091
00202          ERROR    (8300-ABEND)                                    EL65091
00203          PGMIDERR (8900-PGMIDERR)                                 EL65091
00204          ENDDATA  (9999-FINALIZE)                                 EL65091
00205          NOTFND   (8500-NOTFND)                                   EL65091
00206      END-EXEC.                                                    EL65091
00207                                                                   EL65091
00208      EXEC CICS  RETRIEVE                                          EL65091
00209          INTO    (PROGRAM-INTERFACE-BLOCK)                        EL65091
00210          LENGTH  (PI-COMM-LENGTH)                                    CL**3
00211      END-EXEC.                                                    EL65091
00212                                                                      CL**3
00213 * DLO034 OPEN WHEN DMD OR CID                                        CL**3
00214      MOVE PI-COMPANY-ID             TO WS-SAVED-PI-COMPANY-ID.       CL**3
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**3
00216          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                     CL**3
00217              MOVE 'O'                TO DL34-PROCESS-TYPE            CL**3
00218              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL**3
00219              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL**3
00220              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL**3
00221              MOVE SPACES             TO DL34-PRINT-LINE              CL**3
00222              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL**3
00223              EXEC CICS LINK                                          CL**3
00224                  PROGRAM    ('DLO034')                               CL**3
00225                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL**3
00226                  LENGTH     (DLO034-REC-LENGTH)                      CL**3
00227              END-EXEC                                                CL**3
00228              IF DL34-RETURN-CODE NOT = 'OK'                          CL**3
00229                  MOVE  '**DLO034 OPEN ERROR - ABORT**'               CL**3
00230                                      TO   ERROR-LINE                 CL**3
00231                  PERFORM 8400-SEND-TEXT                              CL**3
00232                  EXEC CICS RETURN                                    CL**3
00233                  END-EXEC.                                           CL**3
00234                                                                   EL65091
00235  1000-PRINT-RESULTS.                                              EL65091
00236      MOVE SPACES                 TO  ERACNT-KEY.                  EL65091
00237      MOVE PI-COMPANY-CD          TO  ERACNT-COMPANY-CD.           EL65091
00238      MOVE '1'                    TO  ERACNT-REC-TYP.              EL65091
00239      MOVE PI-ACCT-CARRIER        TO  ERACNT-CARRIER.              EL65091
00240      MOVE PI-ACCT-GROUPING       TO  ERACNT-GROUPING.             EL65091
00241      MOVE PI-ACCT-STATE          TO  ERACNT-STATE.                EL65091
00242      MOVE PI-ACCT-ACCOUNT        TO  ERACNT-ACCOUNT.              EL65091
00243                                                                   EL65091
00244      PERFORM 2000-PRINT-HEADINGS  THRU 2000-EXIT.                 EL65091
00245                                                                   EL65091
00246      MOVE ERACNT-KEY             TO  W-PRIOR-KEY.                 EL65091
00247                                                                   EL65091
00248      EXEC CICS HANDLE CONDITION                                   EL65091
00249           NOTFND(1400-NOTFND)                                     EL65091
00250           NOTOPEN(1600-NOT-OPEN)                                  EL65091
00251           ENDFILE(1500-ENDBR)                                     EL65091
00252      END-EXEC.                                                    EL65091
00253                                                                   EL65091
00254      EXEC CICS STARTBR                                            EL65091
00255           DATASET(ERACNT-FILE-ID)                                 EL65091
00256           RIDFLD(ERACNT-KEY)                                      EL65091
00257           KEYLENGTH(ERACNT-START-LENGTH)                          EL65091
00258           GENERIC                                                 EL65091
00259           GTEQ                                                    EL65091
00260      END-EXEC.                                                    EL65091
00261                                                                   EL65091
00262  1001-LOOP.                                                       EL65091
00263      EXEC CICS READNEXT                                           EL65091
00264           SET(ADDRESS OF NOTE-FILE)                                  CL**2
00265           DATASET(ERACNT-FILE-ID)                                 EL65091
00266           RIDFLD(ERACNT-KEY)                                      EL65091
00267      END-EXEC.                                                    EL65091
00268                                                                   EL65091
00269      ADD 1                       TO  W-DELAY-CNT.                 EL65091
00270      IF W-DELAY-CNT GREATER THAN +50                              EL65091
00271          MOVE +0 TO  W-DELAY-CNT                                  EL65091
00272          EXEC CICS DELAY                                          EL65091
00273               INTERVAL (W-DELAY-INTERVAL)                         EL65091
00274               END-EXEC.                                           EL65091
00275                                                                   EL65091
00276      IF FIRST-ONE                                                 EL65091
00277          IF ERACNT-PARTIAL-KEY NOT = W-PRIOR-PARTIAL-KEY          EL65091
00278              GO TO 1400-NOTFND                                    EL65091
00279          ELSE                                                     EL65091
00280              MOVE SPACE TO FIRST-TIME-SW                          EL65091
00281              MOVE NT-CONTROL-PRIMARY TO W-PRIOR-KEY.              EL65091
00282                                                                   EL65091
00283      MOVE NT-CONTROL-PRIMARY        TO ERACNT-KEY.                EL65091
00284                                                                   EL65091
00285      IF W-PRIOR-PARTIAL-KEY = ERACNT-PARTIAL-KEY                  EL65091
00286          NEXT SENTENCE                                            EL65091
00287      ELSE                                                         EL65091
00288          GO TO 1500-ENDBR.                                        EL65091
00289                                                                   EL65091
00290      IF WS-LINE-CNT GREATER 55                                    EL65091
00291          PERFORM 2000-PRINT-HEADINGS  THRU 2000-EXIT.             EL65091
00292                                                                   EL65091
00293      ADD 1 TO LINE-CNT.                                           EL65091
00294      MOVE LINE-CNT      TO D1-LINE-NO.                            EL65091
00295      MOVE NT-NOTE-LINE   TO D1-TEXT.                              EL65091
00296      MOVE NT-LAST-MAINT-BY  TO D1-MAINT-BY.                       EL65091
00297                                                                   EL65091
00298      IF NT-LAST-MAINT-DT = LOW-VALUES OR SPACES                   EL65091
00299           MOVE SPACES   TO D1-MAINT-DT                            EL65091
00300       ELSE                                                        EL65091
00301         MOVE NT-LAST-MAINT-DT    TO DC-BIN-DATE-1                 EL65091
00302         MOVE SPACE               TO DC-OPTION-CODE                EL65091
00303         PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT                EL65091
00304         MOVE DC-GREG-DATE-1-MDY   TO D1-MAINT-DT.                 EL65091
00305                                                                   EL65091
00306      MOVE DETAIL-LINE-1        TO WS-PASSED-DATA.                 EL65091
00307      MOVE SPACE                TO WS-PASSED-CNTL-CHAR.            EL65091
00308      PERFORM ELPRTCVP    THRU ELPRTCVP-EXIT.                      EL65091
00309                                                                   EL65091
00310      MOVE NT-CONTROL-PRIMARY    TO W-PRIOR-KEY.                   EL65091
00311                                                                   EL65091
00312      GO TO 1001-LOOP.                                             EL65091
00313                                                                   EL65091
00314  1400-NOTFND.                                                     EL65091
00315      MOVE 'NOTES NOT FOUND ON FILE' TO WS-PASSED-DATA.            EL65091
00316      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL65091
00317      GO TO 9999-FINALIZE.                                         EL65091
00318  1500-ENDBR.                                                      EL65091
00319      MOVE SPACES TO WS-PASSED-DATA.                               EL65091
00320      MOVE 'X'  TO WS-PROG-END.                                    EL65091
00321      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL65091
CIDMOD*    GO TO 9999-FINALIZE.                                            CL**3
CIDMOD     GO TO 8999-RETURN-CICS.                                           EL6
00323  1600-NOT-OPEN.                                                   EL65091
00324      MOVE 'NOTE FILE NOT OPEN' TO WS-PASSED-DATA.                 EL65091
00325      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL65091
00326      GO TO 9999-FINALIZE.                                         EL65091
00327      EJECT                                                        EL65091
00328  2000-PRINT-HEADINGS.                                             EL65091
00329      MOVE W-CURRENT-DATE TO H1-DATE.                              EL65091
00330      MOVE '1'            TO WS-PASSED-CNTL-CHAR.                  EL65091
00331      MOVE HEAD-LINE-1    TO WS-PASSED-DATA.                       EL65091
00332      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL65091
00333                                                                   EL65091
00334      PERFORM 5000-GET-ACCOUNT THRU 5999-EXIT.                     EL65091
00335                                                                   EL65091
00336      MOVE HEAD-LINE-2       TO WS-PASSED-DATA.                    EL65091
00337      MOVE '0'               TO WS-PASSED-CNTL-CHAR.               EL65091
00338      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL65091
00339                                                                   EL65091
00340      MOVE HEAD-LINE-3       TO WS-PASSED-DATA.                    EL65091
00341      MOVE SPACE             TO WS-PASSED-CNTL-CHAR.               EL65091
00342      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL65091
00343                                                                   EL65091
00344  2000-EXIT.                                                       EL65091
00345      EXIT.                                                        EL65091
00346  EJECT                                                            EL65091
00347                                                                   EL65091
00348  5000-GET-ACCOUNT.                                                EL65091
00349      IF FIRST-ONE                                                 EL65091
00350          NEXT SENTENCE                                            EL65091
00351      ELSE                                                         EL65091
00352          GO TO 5500-PUT-LINES.                                    EL65091
00353                                                                   EL65091
00354      EXEC CICS HANDLE CONDITION                                   EL65091
00355          NOTFND (5400-NOTFND)                                     EL65091
00356      END-EXEC.                                                    EL65091
00357                                                                   EL65091
00358      MOVE PI-COMPANY-CD          TO  ERACCT-COMPANY-CD.           EL65091
00359      MOVE PI-ACCT-CARRIER        TO  ERACCT-CARRIER.              EL65091
00360      MOVE PI-ACCT-GROUPING       TO  ERACCT-GROUPING.             EL65091
00361      MOVE PI-ACCT-STATE          TO  ERACCT-STATE.                EL65091
00362      MOVE PI-ACCT-ACCOUNT        TO  ERACCT-ACCOUNT.              EL65091
00363      MOVE PI-ACCT-EXP-DT         TO  ERACCT-EXP-DT.               EL65091
00364                                                                   EL65091
00365      EXEC CICS READ                                               EL65091
00366          DATASET (ERACCT-FILE-ID)                                 EL65091
00367          RIDFLD  (ERACCT-KEY)                                     EL65091
00368          SET     (ADDRESS OF ACCOUNT-MASTER)                         CL**2
00369          EQUAL                                                    EL65091
00370          END-EXEC.                                                EL65091
00371                                                                   EL65091
00372      MOVE AM-CARRIER             TO  DA-CARRIER.                  EL65091
00373      MOVE AM-GROUPING            TO  DA-GROUPING.                 EL65091
00374      MOVE AM-STATE               TO  DA-STATE.                    EL65091
00375      MOVE AM-ACCOUNT             TO  DA-ACCOUNT.                  EL65091
00376                                                                   EL65091
00377      MOVE AM-NAME                TO  DA-ACCT-NAME.                EL65091
00378                                                                   EL65091
00379      GO TO 5500-PUT-LINES.                                        EL65091
00380                                                                   EL65091
00381  5400-NOTFND.                                                     EL65091
00382       MOVE 'ACCOUNT MASTER NOT ON FILE '  TO WS-PASSED-DATA.      EL65091
00383       MOVE '0'                   TO WS-PASSED-CNTL-CHAR.          EL65091
00384       PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                       EL65091
00385       GO TO 5999-EXIT.                                            EL65091
00386                                                                   EL65091
00387  5500-PUT-LINES.                                                  EL65091
00388       MOVE HEAD-LINE-ACCOUNT     TO WS-PASSED-DATA.               EL65091
00389       MOVE '0'                   TO WS-PASSED-CNTL-CHAR.          EL65091
00390       PERFORM ELPRTCVP    THRU ELPRTCVP-EXIT.                     EL65091
00391       MOVE DETAIL-LINE-ACCOUNT   TO WS-PASSED-DATA.               EL65091
00392       MOVE ' '                   TO WS-PASSED-CNTL-CHAR.          EL65091
00393       PERFORM ELPRTCVP    THRU ELPRTCVP-EXIT.                     EL65091
00394                                                                   EL65091
00395  5999-EXIT.  EXIT.                                                EL65091
00396  EJECT                                                            EL65091
00397 ***************************************************************** EL65091
uktdel*PRINT-RTN.  COPY ELPRTCVP.                                       EL65091
uktins PRINT-RTN.
uktins     COPY ELPRTCVP.
00399                                                                      CL**3
00400                                                                      CL**3
00401 ***************************************************************** EL65091
00402  EJECT                                                            EL65091
00403  8300-ABEND.                                                      EL65091
00404                                                                   EL65091
00405      MOVE SPACES                 TO WS-PASSED-DATA.               EL65091
00406      MOVE DFHEIBLK               TO WS-PASSED-DATA.               EL65091
00407      MOVE LINK-004               TO PGM-NAME.                     EL65091
00408                                                                   EL65091
00409      EXEC CICS LINK                                               EL65091
00410          PROGRAM   (PGM-NAME)                                     EL65091
00411          COMMAREA  (WS-PASSED-DATA)                               EL65091
00412          LENGTH    (72)                                           EL65091
00413      END-EXEC.                                                    EL65091
00414                                                                   EL65091
00415      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL65091
00416                                                                   EL65091
00417      GO TO 9999-FINALIZE.                                         EL65091
00418                                                                      CL**3
00419  8400-SEND-TEXT.                                                     CL**3
00420      EXEC CICS SEND TEXT                                             CL**3
00421           FROM    (ERROR-LINE)                                       CL**3
00422           LENGTH  (70)                                               CL**3
00423           END-EXEC.                                                  CL**3
00424                                                                      CL**3
00425      EJECT                                                           CL**3
00426                                                                   EL65091
00427  8500-DATE-CONVERT.                                               EL65091
00428      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL65091
00429                                                                   EL65091
00430      EXEC CICS LINK                                               EL65091
00431          PROGRAM    (PGM-NAME)                                    EL65091
00432          COMMAREA   (DATE-CONVERSION-DATA)                        EL65091
00433          LENGTH     (DC-COMM-LENGTH)                              EL65091
00434          END-EXEC.                                                EL65091
00435  8500-EXIT.                                                       EL65091
00436      EXIT.                                                        EL65091
00437                                                                   EL65091
00438      SKIP3                                                        EL65091
00439  8500-NOTFND.                                                     EL65091
00440      MOVE 'NO COMMUNICATION AREA FOUND' TO WS-PASSED-DATA.        EL65091
00441      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL65091
00442      GO TO 9999-FINALIZE.                                         EL65091
00443      SKIP3                                                        EL65091
00444  8900-PGMIDERR.                                                   EL65091
00445      MOVE '* PROG NOT FOUND, NOTIFY DATA PROCESSING *'            EL65091
00446                           TO WS-PASSED-DATA.                      EL65091
00447      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL65091
00448      GO TO 9999-FINALIZE.                                         EL65091
00449      GOBACK.                                                      EL65091
00450  8900-EXIT.                                                       EL65091
00451      EXIT.                                                           CL**3
00452      EJECT                                                        EL65091
00453                                                                   EL65091
00454  8999-RETURN-CICS.                                                EL65091
00455                                                                      CL**3
00456      MOVE WS-SAVED-PI-COMPANY-ID    TO PI-COMPANY-ID.                CL**3
00457                                                                   EL65091
00458      EXEC CICS RETURN                                             EL65091
00459      END-EXEC.                                                    EL65091
00460                                                                   EL65091
00461  8999-EXIT. EXIT.                                                 EL65091
00462                                                                   EL65091
00463  9999-FINALIZE.                                                   EL65091
00464 * DLO034 CLOSE                                                       CL**3
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**3
00466          IF DL34-PROCESS-TYPE IS NOT EQUAL TO SPACES                 CL**3
00467              MOVE 'C'                TO DL34-PROCESS-TYPE            CL**3
00468              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL**3
00469              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL**3
00470              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL**3
00471              MOVE SPACES             TO DL34-PRINT-LINE              CL**3
00472                                         DL34-OVERRIDE-PRINTER-ID     CL**3
00473              EXEC CICS LINK                                          CL**3
00474                  PROGRAM    ('DLO034')                               CL**3
00475                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL**3
00476                  LENGTH     (DLO034-REC-LENGTH)                      CL**3
00477              END-EXEC                                                CL**3
00478              IF DL34-RETURN-CODE NOT = 'OK'                          CL**3
00479                  MOVE  '**DLO034 CLOSE ERROR - ABORT**'              CL**3
00480                                      TO ERROR-LINE                   CL**3
00481                  PERFORM 8400-SEND-TEXT                              CL**3
00482                  EXEC CICS RETURN                                    CL**3
00483                  END-EXEC.                                           CL**3
00484                                                                      CL**3
00485      MOVE SPACES TO WS-PASSED-DATA.                               EL65091
00486      MOVE 'X'  TO WS-PROG-END.                                    EL65091
00487      PERFORM ELPRTCVP  THRU ELPRTCVP-EXIT.                        EL65091
00488      GO TO 8999-RETURN-CICS.                                      EL65091
00489  9999-EXIT.                                                       EL65091
