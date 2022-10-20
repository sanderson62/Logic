00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL180
00003  PROGRAM-ID.                 EL180 .                                 LV003
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 02/12/96 08:26:28.                    CL**2
00007 *                            VMOD=2.003                              CL**3
00008 *                                                                 EL180
00008 *                                                                 EL180
00009 *AUTHOR.    LOGIC, INC.                                              CL**2
00010 *           DALLAS, TEXAS.                                           CL**2
00011                                                                   EL180
00012 *DATE-COMPILED.                                                      CL**2
00013                                                                   EL180
00014 *SECURITY.   *****************************************************   CL**2
00015 *            *                                                   *   CL**2
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**2
00017 *            *                                                   *   CL**2
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00021 *            *                                                   *   CL**2
00022 *            *****************************************************   CL**2
00023                                                                   EL180
00024                                                                   EL180
00025 *REMARKS.                                                            CL**2
00026 *    SCREENS     - EL180A - CLAIM STATUS PRINTING                    CL**2
00027                                                                   EL180
00028 *    ENTERED BY  - EL171 - REPORT MENU                               CL**2
00029                                                                   EL180
00030 *    EXIT TO     - EL171 - REPORT MENU                               CL**2
00031 *                - EL1802- STATUS PRINTER - VIA START                CL**2
00032                                                                   EL180
00033 *    INPUT FILE  - ELCNTL - CONTROL FILE                             CL**2
00034                                                                   EL180
00035 *    OUTPUT FILE - NONE                                              CL**2
00036                                                                   EL180
00037 *    COMMAREA    - PASSED                                            CL**2
00038                                                                   EL180
00039 *    ERROR CODES ACCESSED  - 42, 22, 50, 29, 08, 412, 413, 2379      CL**2
00040                                                                   EL180
00041 *    NARRATIVE   - UPON ENTRY, EL008 IS STARTED TO PRINT             CL**2
00042 *                  CLAIM STATUS ON DESIGNATED PRINTER,               CL**2
00043 *                  IF AN ALTERNATE PRINTER ID IS ENTERED             CL**2
00044 *                  FROM EL180A, THAT PRINTER ID REPLACES THE         CL**2
00045 *                  NORMAL PRINTER ID OF THE CONTROL RECORD.          CL**2
030612******************************************************************
030612*                   C H A N G E   L O G
030612*
030612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030612*-----------------------------------------------------------------
030612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030612* EFFECTIVE    NUMBER
030612*-----------------------------------------------------------------
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
030612******************************************************************
00046                                                                   EL180
00047                                                                   EL180
00048      EJECT                                                        EL180
00049  ENVIRONMENT DIVISION.                                            EL180
00050                                                                   EL180
00051  DATA DIVISION.                                                   EL180
00052                                                                   EL180
00053  WORKING-STORAGE SECTION.                                         EL180
00054                                                                   EL180
00055  77  FILLER  PIC X(32)  VALUE '********************************'. EL180
00056  77  FILLER  PIC X(32)  VALUE '*   EL180  WORKING STORAGE     *'. EL180
00057  77  FILLER  PIC X(32)  VALUE '************ V/M 2.003 *********'.    CL**3
00058                                                                   EL180
00059      COPY ELCSCTM.                                                   CL**2
00060                                                                   EL180
00061      COPY ELCSCRTY.                                                  CL**2
00062                                                                   EL180
00063  01  WS-DATE-AREA.                                                EL180
00064      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL180
00065      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL180
00066                                                                   EL180
00067  01  ERROR-NUMBERS.                                               EL180
00068      12  ER-0022                 PIC X(4)    VALUE '0022'.        EL180
00069      12  ER-0029                 PIC X(4)    VALUE '0029'.        EL180
00070      12  ER-0042                 PIC X(4)    VALUE '0042'.        EL180
00071      12  ER-0050                 PIC X(4)    VALUE '0050'.        EL180
00072      12  ER-0070                 PIC X(4)    VALUE '0070'.        EL180
00073      12  ER-7008                 PIC X(4)    VALUE '7008'.        EL180
00074      12  ER-2379                 PIC X(4)    VALUE '2379'.        EL180
00075      12  ER-0423                 PIC X(4)    VALUE '0423'.        EL180
00076      12  ER-0412                 PIC X(4)    VALUE '0412'.        EL180
00077      12  ER-0413                 PIC X(4)    VALUE '0413'.        EL180
00078                                                                   EL180
00079  01  WS-SCRATCH-AREA.                                             EL180
00080      05  SC-ITEM                 PIC S9(4)  VALUE +0001  COMP.    EL180
00081                                                                   EL180
00082 *    05  WS-COMM-LENGTH          PIC S9(4) VALUE +512  COMP.      EL180
00083                                                                   EL180
00084      05  THIS-PGM                PIC X(8)  VALUE 'EL180'.         EL180
00085                                                                   EL180
00086      05  WS-CURSOR               PIC S9(4)  VALUE -1  COMP.       EL180
00087                                                                   EL180
00088      05  WS-START-TRANS-ID       PIC X(4)   VALUE 'EX58'.         EL180
00089                                                                   EL180
00090      05  WS-TRANS-ID             PIC X(4)   VALUE 'EX50'.         EL180
00091      05  WS-PRINTER-ID           PIC X(4)   VALUE SPACES.         EL180
00092                                                                   EL180
00093      05  PF-SUB                  PIC 99     VALUE ZEROS.          EL180
00094                                                                   EL180
00095      05  WS-CONTROL-PRIMARY.                                      EL180
00096          10  WS-COMPANY-ID       PIC XXX.                         EL180
00097          10  WS-RECORD-TYPE      PIC X.                           EL180
00098          10  WS-ACCESS-CD-GENL   PIC XXXX.                        EL180
00099          10  WS-SEQUENCE-NO      PIC S9(4) COMP.                  EL180
00100                                                                   EL180
00101      05  TIME-OUT.                                                EL180
00102          10  WS-TRANS-HOUR       PIC XX  VALUE SPACE.             EL180
00103          10  FILLER              PIC X   VALUE '.'.               EL180
00104          10  WS-TRANS-MINUTE     PIC XX  VALUE SPACE.             EL180
00105                                                                   EL180
00106      05  TIME-IN                 PIC 9(7).                        EL180
00107      05  WS-TIME  REDEFINES TIME-IN.                              EL180
00108          10  FILLER              PIC 9.                           EL180
00109          10  WS-HOUR             PIC 99.                          EL180
00110          10  WS-MINUTE           PIC 99.                          EL180
00111          10  FILLER              PIC 99.                          EL180
00112                                                                   EL180
00113      05  WS-CNTL-ERROR-SW        PIC X  VALUE SPACE.              EL180
00114          88  NO-COMPANY-RECORD       VALUE 'X'.                   EL180
00115                                                                   EL180
00116      EJECT                                                        EL180
00117      COPY ELCAID.                                                    CL**2
00118                                                                   EL180
00119  01  PF-AID REDEFINES DFHAID.                                     EL180
00120      05  FILLER                  PIC X(8).                        EL180
00121      05  PF-VALUES  OCCURS 24    PIC X.                           EL180
00122      EJECT                                                        EL180
00123      COPY ELCATTR.                                                   CL**2
00124      EJECT                                                        EL180
00125      COPY ELCINTF.                                                   CL**2
00126      12  WS-WORK REDEFINES  PI-PROGRAM-WORK-AREA.                 EL180
00127          24  PI-PROCESS-CNT      PIC X.                           EL180
00128          24  PI-PROCESS-NAME     PIC X(30).                       EL180
00129          24  FILLER              PIC X(609).                         CL**2
00130                                                                   EL180
00131      EJECT                                                        EL180
00132      COPY ELCEMIB.                                                   CL**2
00133      EJECT                                                        EL180
00134      COPY ELCLOGOF.                                                  CL**2
00135      EJECT                                                        EL180
00136      COPY ELCDATE.                                                   CL**2
00137      EJECT                                                        EL180
00138      COPY EL180S.                                                    CL**2
00139      EJECT                                                        EL180
00140  LINKAGE SECTION.                                                 EL180
00141  01  DFHCOMMAREA                 PIC X(1024).                     EL180
00142                                                                      CL**2
00143      COPY ELCCNTL.                                                   CL**2
00144      EJECT                                                        EL180
00145  PROCEDURE DIVISION.                                              EL180
00146                                                                   EL180
00147      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL180
00148      MOVE '5'                   TO DC-OPTION-CODE.                EL180
00149      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL180
00150      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL180
00151      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL180
00152                                                                   EL180
00153  0001-PROCESSING-EXITS.                                           EL180
00154      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL180
00155                                                                   EL180
00156      IF EIBCALEN NOT GREATER THAN ZEROS                           EL180
00157        GO TO 8460-UNAUTHERR.                                      EL180
00158                                                                   EL180
00159      IF PI-PROCESS-CNT  = 'X'                                     EL180
00160          GO TO 8800-CLEAR.                                        EL180
00161                                                                   EL180
00162      EXEC CICS  HANDLE AID                                        EL180
00163             ENTER  (0700-ENTER)                                   EL180
00164             ANYKEY (0800-OTHER)                                   EL180
00165             CLEAR  (8800-CLEAR)                                   EL180
00166      END-EXEC.                                                    EL180
00167                                                                   EL180
00168      EXEC CICS  HANDLE CONDITION                                  EL180
00169             ERROR    (8300-ABEND)                                 EL180
00170             MAPFAIL  (0200-FIRST-TIME-IN)                         EL180
00171             PGMIDERR (8440-PGMIDERR)                              EL180
00172      END-EXEC.                                                    EL180
00173                                                                   EL180
00174      IF PI-PROCESSOR-ID = 'LGXX'                                  EL180
00175          NEXT SENTENCE                                            EL180
00176      ELSE                                                         EL180
00177          EXEC CICS READQ TS                                       EL180
00178              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL180
00179              INTO    (SECURITY-CONTROL)                           EL180
00180              LENGTH  (SC-COMM-LENGTH)                             EL180
00181              ITEM    (SC-ITEM)                                    EL180
00182          END-EXEC                                                 EL180
00183          MOVE SC-CLAIMS-DISPLAY (19)  TO  PI-DISPLAY-CAP          EL180
00184          MOVE SC-CLAIMS-UPDATE  (19)  TO  PI-MODIFY-CAP.          EL180
00185                                                                   EL180
00186      PERFORM 0100-INITIALIZE   THRU 0100-EXIT.                    EL180
00187                                                                   EL180
00188       IF PI-CALLING-PROGRAM   NOT = THIS-PGM                      EL180
00189          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL180
00190             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6       EL180
00191             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5       EL180
00192             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4       EL180
00193             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3       EL180
00194             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2       EL180
00195             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1       EL180
00196             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM     EL180
00197             MOVE 'EL180'              TO PI-CALLING-PROGRAM       EL180
00198             GO TO 0200-FIRST-TIME-IN                              EL180
00199          ELSE                                                     EL180
00200              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL180
00201              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL180
00202              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL180
00203              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL180
00204              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL180
00205              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL180
00206              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL180
00207              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL180
00208                                                                   EL180
00209      PERFORM 1000-RECEIVE      THRU 1000-EXIT.                    EL180
00210                                                                   EL180
00211      EJECT                                                        EL180
00212  0002-PROCESSING-MAINLINE.                                        EL180
00213      IF NOT MODIFY-CAP                                            EL180
00214         MOVE 'UPDATE'            TO  SM-READ                      EL180
00215         PERFORM 9995-SECURITY-VIOLATION                           EL180
00216         MOVE ER-0070             TO  EMI-ERROR                    EL180
00217         PERFORM 8200-GET-ERROR-DESC THRU 8200-EXIT                EL180
00218         PERFORM 1250-SEND-MAP-ERASE                               EL180
00219         GO TO 8400-RETURN-TRANS.                                  EL180
00220                                                                   EL180
00221      MOVE PI-COMPANY-ID          TO WS-COMPANY-ID.                EL180
00222      MOVE SPACES                 TO WS-ACCESS-CD-GENL.            EL180
00223      MOVE ZEROS                  TO WS-SEQUENCE-NO.               EL180
00224      MOVE '1'                    TO WS-RECORD-TYPE.               EL180
00225                                                                   EL180
00226      PERFORM 1400-READ-CNTL       THRU 1400-EXIT.                 EL180
00227                                                                   EL180
00228      IF NO-COMPANY-RECORD                                         EL180
00229          PERFORM 9960-NO-CNTL-ERROR   THRU 9960-EXIT              EL180
00230          GO TO 8400-RETURN-TRANS.                                 EL180
00231                                                                   EL180
00232      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.               CL**3
00233                                                                      CL**3
00234      IF (NOT PI-NO-CARRIER-SECURITY OR                            EL180
00235          NOT PI-NO-ACCOUNT-SECURITY)                              EL180
00236         IF MPRTIDL NOT GREATER THAN 0                             EL180
00237            MOVE ER-2379          TO EMI-ERROR                     EL180
00238            PERFORM 8200-GET-ERROR-DESC THRU 8200-EXIT             EL180
00239            MOVE -1               TO MPRTIDL                       EL180
00240            MOVE AL-UABON         TO MPRTIDA                       EL180
00241            PERFORM 1200-SEND-MAP THRU 1200-EXIT                   EL180
00242            GO TO 8400-RETURN-TRANS                                EL180
00243         ELSE                                                      EL180
00244            MOVE MPRTIDI          TO WS-PRINTER-ID                 EL180
00245                                     PI-ALT-DMD-PRT-ID                CL**3
00246      ELSE                                                         EL180
00247         IF MPRTIDI  = (LOW-VALUES      OR SPACES)                 EL180
00248            MOVE  CF-FORMS-PRINTER-ID  TO  WS-PRINTER-ID           EL180
00249         ELSE                                                      EL180
00250            MOVE  MPRTIDI         TO WS-PRINTER-ID                    CL**3
00251                                     PI-ALT-DMD-PRT-ID.               CL**3
00252                                                                   EL180
00253      MOVE CF-CL-MAIL-TO-NAME     TO  PI-PROCESS-NAME.             EL180
00254      MOVE  'X'                   TO  PI-PROCESS-CNT.              EL180
00255                                                                   EL180
00256      EXEC CICS  HANDLE CONDITION                                  EL180
00257                 TERMIDERR  (9920-TERMIDERR)                       EL180
00258                 TRANSIDERR (9930-TRANSIDERR)                      EL180
00259      END-EXEC.                                                    EL180
00260                                                                   EL180
062121     IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL' OR 'FNL'
00262 *        MOVE EIBTRMID       TO WS-PRINTER-ID                        CL**3
00263          EXEC CICS  START                                            CL**3
00264                     TRANSID (WS-START-TRANS-ID)                      CL**3
00265 *                   TERMID  (WS-PRINTER-ID)                          CL**3
00266                     FROM    (PROGRAM-INTERFACE-BLOCK)                CL**3
00267                     LENGTH  (PI-COMM-LENGTH)                         CL**3
00268          END-EXEC                                                    CL**3
00269      ELSE                                                            CL**3
00270          EXEC CICS  START                                            CL**3
00271                     TRANSID (WS-START-TRANS-ID)                      CL**3
00272                     TERMID  (WS-PRINTER-ID)                          CL**3
00273                     FROM    (PROGRAM-INTERFACE-BLOCK)                CL**3
00274                     LENGTH  (PI-COMM-LENGTH)                         CL**3
00275          END-EXEC.                                                   CL**3
00276                                                                   EL180
00277      PERFORM 9900-NO-ERRORS     THRU 9900-EXIT                    EL180
00278      GO TO 8400-RETURN-TRANS.                                     EL180
00279      EJECT                                                        EL180
00280  0100-INITIALIZE.                                                 EL180
00281      MOVE '180A'                 TO PI-CURRENT-SCREEN-NO.         EL180
00282                                                                   EL180
00283      MOVE EIBTIME                TO TIME-IN.                      EL180
00284      MOVE WS-HOUR                TO WS-TRANS-HOUR.                EL180
00285      MOVE WS-MINUTE              TO WS-TRANS-MINUTE.              EL180
00286                                                                   EL180
00287      MOVE TIME-OUT               TO MRNTIMEO.                     EL180
00288      MOVE SAVE-DATE              TO MRNDATEO.                     EL180
00289                                                                   EL180
00290  0100-EXIT.                                                       EL180
00291       EXIT.                                                       EL180
00292                                                                   EL180
00293  0200-FIRST-TIME-IN.                                              EL180
00294      PERFORM 1070-PRIME-MAP THRU 1070-EXIT                        EL180
00295      PERFORM 0100-INITIALIZE   THRU 0100-EXIT                     EL180
00296      PERFORM 1250-SEND-MAP-ERASE THRU 1250-EXIT                   EL180
00297      GO TO 8400-RETURN-TRANS.                                     EL180
00298                                                                   EL180
00299  0700-ENTER.                                                      EL180
00300      IF MPFNUMBI = SPACES                                         EL180
00301        OR MPFNUMBI = LOW-VALUES                                   EL180
00302         GO TO 0002-PROCESSING-MAINLINE                            EL180
00303      ELSE                                                         EL180
00304         NEXT SENTENCE.                                            EL180
00305                                                                   EL180
00306      IF MPFNUMBI GREATER 0 AND LESS 25                            EL180
00307         NEXT SENTENCE                                             EL180
00308      ELSE                                                         EL180
00309         GO TO 9996-INVALID-PF.                                    EL180
00310                                                                   EL180
00311      IF MPFNUMBI = 12                                             EL180
00312         MOVE  'EL010'            TO THIS-PGM                      EL180
00313         GO TO 8600-XCTL.                                          EL180
00314                                                                   EL180
00315      IF MPFNUMBI = 23                                             EL180
00316         GO TO 8700-XCTL-EL005.                                    EL180
00317                                                                   EL180
00318      IF MPFNUMBI  = 24                                            EL180
00319         MOVE  'EL126'            TO THIS-PGM                      EL180
00320         GO TO 8600-XCTL.                                          EL180
00321                                                                   EL180
00322      GO TO 9996-INVALID-PF.                                       EL180
00323                                                                   EL180
00324  0700-EXIT.                                                       EL180
00325       EXIT.                                                       EL180
00326                                                                   EL180
00327  0800-OTHER.                                                      EL180
00328      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL180
00329         GO TO 9998-WRONG-KEY                                      EL180
00330      ELSE                                                         EL180
00331         NEXT SENTENCE.                                            EL180
00332                                                                   EL180
00333      IF MPFNUMBI = SPACES                                         EL180
00334                     OR LOW-VALUES                                 EL180
00335         NEXT SENTENCE                                             EL180
00336      ELSE                                                         EL180
00337         GO TO 9995-MULT-FUNC-ERROR.                               EL180
00338                                                                   EL180
00339      IF EIBAID = 12                                               EL180
00340         MOVE  'EL010'            TO THIS-PGM                      EL180
00341         GO TO 8600-XCTL.                                          EL180
00342                                                                   EL180
00343      IF EIBAID = DFHCLEAR                                         EL180
00344         GO TO 8800-CLEAR.                                         EL180
00345                                                                   EL180
00346      IF EIBAID = 23                                               EL180
00347         GO TO 8700-XCTL-EL005.                                    EL180
00348                                                                   EL180
00349      IF EIBAID = 24                                               EL180
00350         MOVE  'EL126'            TO THIS-PGM                      EL180
00351         GO TO 8600-XCTL.                                          EL180
00352                                                                   EL180
00353      GO TO 9998-WRONG-KEY.                                        EL180
00354                                                                   EL180
00355  0800-EXIT.  EXIT.                                                EL180
00356      EJECT                                                        EL180
00357  1000-RECEIVE.                                                    EL180
00358      EXEC CICS  RECEIVE                                           EL180
00359             MAP    ('EL180A')                                     EL180
00360             MAPSET ('EL180S')                                     EL180
00361      END-EXEC.                                                    EL180
00362                                                                   EL180
00363  1000-EXIT.                                                       EL180
00364       EXIT.                                                       EL180
00365                                                                   EL180
00366  1070-PRIME-MAP.                                                  EL180
00367      MOVE LOW-VALUES             TO EL180AO.                      EL180
00368      MOVE WS-CURSOR              TO MPRTIDL.                      EL180
00369                                                                   EL180
00370  1070-EXIT.                                                       EL180
00371       EXIT.                                                       EL180
00372                                                                   EL180
00373  1200-SEND-MAP.                                                   EL180
00374      EXEC CICS  SEND                                              EL180
00375                 MAPSET ('EL180S')                                 EL180
00376                 MAP    ('EL180A')                                 EL180
00377                 FREEKB                                            EL180
00378                 DATAONLY                                          EL180
00379                 CURSOR                                            EL180
00380      END-EXEC.                                                    EL180
00381                                                                   EL180
00382  1200-EXIT.                                                       EL180
00383       EXIT.                                                       EL180
00384                                                                   EL180
00385  1250-SEND-MAP-ERASE.                                             EL180
00386      EXEC CICS  SEND                                              EL180
00387                 MAPSET ('EL180S')                                 EL180
00388                 MAP    ('EL180A')                                 EL180
00389                 FREEKB                                            EL180
00390                 ERASE                                             EL180
00391                 CURSOR                                            EL180
00392      END-EXEC.                                                    EL180
00393                                                                   EL180
00394  1250-EXIT.                                                       EL180
00395       EXIT.                                                       EL180
00396                                                                   EL180
00397  1400-READ-CNTL.                                                  EL180
00398      EXEC CICS  HANDLE CONDITION                                  EL180
00399             NOTFND  (1401-NOTFND)                                 EL180
00400             NOTOPEN (1402-NOTOPEN)                                EL180
00401      END-EXEC.                                                    EL180
00402                                                                   EL180
00403      EXEC CICS  READ                                              EL180
00404             SET      (ADDRESS OF CONTROL-FILE)                       CL**2
00405             DATASET  ('ELCNTL')                                   EL180
00406             RIDFLD   (WS-CONTROL-PRIMARY)                         EL180
00407      END-EXEC.                                                    EL180
00408                                                                   EL180
00409      GO TO 1400-EXIT.                                             EL180
00410                                                                   EL180
00411  1401-NOTFND.                                                     EL180
00412      MOVE  'X'                   TO WS-CNTL-ERROR-SW.             EL180
00413                                                                   EL180
00414  1402-NOTOPEN.                                                    EL180
00415      PERFORM 9940-NOTOPEN-CNTL  THRU 9940-EXIT.                   EL180
00416      GO TO 8400-RETURN-TRANS.                                     EL180
00417                                                                   EL180
00418  1400-EXIT. EXIT.                                                 EL180
00419      EJECT                                                        EL180
00420                                                                   EL180
00421  8200-GET-ERROR-DESC.                                             EL180
00422      EXEC CICS LINK                                               EL180
00423             PROGRAM  ('EL001')                                    EL180
00424             COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)              EL180
00425             LENGTH   (EMI-COMM-LENGTH)                            EL180
00426      END-EXEC.                                                    EL180
00427                                                                   EL180
00428      MOVE EMI-LINE1              TO MERMSG1O.                     EL180
00429 *    MOVE EMI-LINE2              TO MERMSG2O.                     EL180
00430                                                                   EL180
00431  8200-EXIT.                                                       EL180
00432       EXIT.                                                       EL180
00433                                                                   EL180
00434  8300-ABEND.                                                      EL180
00435      MOVE DFHEIBLK               TO EMI-LINE1.                    EL180
00436                                                                   EL180
00437      EXEC CICS LINK                                               EL180
00438            PROGRAM  ('EL004')                                     EL180
00439            COMMAREA (EMI-LINE1)                                   EL180
00440            LENGTH   (72)                                          EL180
00441      END-EXEC.                                                    EL180
00442                                                                   EL180
00443      MOVE EMI-LINE1              TO MERMSG1O.                     EL180
00444      PERFORM 1200-SEND-MAP.                                       EL180
00445      GO TO 8400-RETURN-TRANS.                                     EL180
00446                                                                   EL180
00447  8400-RETURN-TRANS.                                               EL180
00448      EXEC CICS  RETURN                                            EL180
00449                 TRANSID  (WS-TRANS-ID)                            EL180
00450                 COMMAREA (PROGRAM-INTERFACE-BLOCK)                EL180
00451                 LENGTH   (PI-COMM-LENGTH)                         EL180
00452      END-EXEC.                                                    EL180
00453                                                                   EL180
00454  8400-EXIT.                                                       EL180
00455       EXIT.                                                       EL180
00456                                                                   EL180
00457  8440-PGMIDERR.                                                   EL180
00458      EXEC CICS  HANDLE CONDITION                                  EL180
00459                 PGMIDERR (8480-SEND-TEXT)                         EL180
00460      END-EXEC.                                                    EL180
00461                                                                   EL180
00462      MOVE THIS-PGM               TO LOGOFF-PGM PI-CALLING-PROGRAM.EL180
00463      MOVE SPACES                 TO PI-ENTRY-CD-1.                EL180
00464      MOVE 'EL005'                TO THIS-PGM.                     EL180
00465      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL180
00466      GO TO 8600-XCTL.                                             EL180
00467                                                                   EL180
00468  8440-EXIT.                                                       EL180
00469       EXIT.                                                       EL180
00470                                                                   EL180
00471  8460-UNAUTHERR.                                                  EL180
00472      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL180
00473                                                                   EL180
00474  8480-SEND-TEXT.                                                  EL180
00475      EXEC CICS  SEND  TEXT                                        EL180
00476                 FROM     (LOGOFF-TEXT)                            EL180
00477                 ERASE                                             EL180
00478                 FREEKB                                            EL180
00479                 LENGTH   (LOGOFF-LENGTH)                          EL180
00480      END-EXEC.                                                    EL180
00481                                                                   EL180
00482      GO TO 8500-RETURN-CICS.                                      EL180
00483                                                                   EL180
00484  8460-EXIT.                                                       EL180
00485       EXIT.                                                       EL180
00486                                                                   EL180
00487  8500-RETURN-CICS.                                                EL180
00488 *    PERFORM 1300-SEND-TEXT THRU 1300-EXIT.                       EL180
00489                                                                   EL180
00490      EXEC CICS  RETURN                                            EL180
00491      END-EXEC.                                                    EL180
00492                                                                   EL180
00493  8600-XCTL.                                                       EL180
00494      EXEC CICS XCTL                                               EL180
00495            PROGRAM  (THIS-PGM)                                    EL180
00496            COMMAREA (PROGRAM-INTERFACE-BLOCK)                     EL180
00497            LENGTH   (PI-COMM-LENGTH)                              EL180
00498      END-EXEC.                                                    EL180
00499                                                                   EL180
00500  8600-EXIT.                                                       EL180
00501       EXIT.                                                       EL180
00502                                                                   EL180
00503  8700-XCTL-EL005.                                                 EL180
00504      MOVE   'EL005'              TO THIS-PGM.                     EL180
00505      MOVE '.'                    TO PI-ENTRY-CD-1.                EL180
00506 *     NOTE, PERIOD IS EQUIVALENT TO '4B' THE VALUE OF DFHPF23     EL180
00507      GO TO 8600-XCTL.                                             EL180
00508                                                                   EL180
00509  8700-EXIT.                                                       EL180
00510                                                                   EL180
00511  8800-CLEAR.                                                      EL180
00512      MOVE PI-RETURN-TO-PROGRAM   TO THIS-PGM.                     EL180
00513      GO TO 8600-XCTL.                                             EL180
00514                                                                   EL180
00515  8800-EXIT.                                                       EL180
00516                                                                   EL180
00517  9700-LINK-DATE-CONVERT.                                          EL180
00518      EXEC CICS LINK                                               EL180
00519          PROGRAM    ('ELDATCV')                                   EL180
00520          COMMAREA   (DATE-CONVERSION-DATA)                        EL180
00521          LENGTH     (DC-COMM-LENGTH)                              EL180
00522          END-EXEC.                                                EL180
00523                                                                   EL180
00524  9700-EXIT.                                                       EL180
00525      EXIT.                                                        EL180
00526                                                                   EL180
00527      EJECT                                                        EL180
00528  9900-NO-ERRORS.                                                  EL180
00529      MOVE ER-0423                TO EMI-ERROR                     EL180
00530      PERFORM 8200-GET-ERROR-DESC THRU   8200-EXIT.                EL180
00531      MOVE WS-CURSOR              TO MPRTIDL.                      EL180
00532      PERFORM 1200-SEND-MAP       THRU   1200-EXIT.                EL180
00533                                                                   EL180
00534  9900-EXIT.                                                       EL180
00535       EXIT.                                                       EL180
00536                                                                   EL180
00537  9920-TERMIDERR.                                                  EL180
00538      MOVE ER-0412                TO EMI-ERROR.                    EL180
00539      PERFORM 8200-GET-ERROR-DESC  THRU 8200-EXIT.                 EL180
00540      MOVE WS-CURSOR              TO MPRTIDL.                      EL180
00541      PERFORM 1200-SEND-MAP       THRU 1200-EXIT.                  EL180
00542      GO TO 8400-RETURN-TRANS.                                     EL180
00543                                                                   EL180
00544  9920-EXIT.                                                       EL180
00545       EXIT.                                                       EL180
00546                                                                   EL180
00547  9930-TRANSIDERR.                                                 EL180
00548      MOVE ER-0413                TO EMI-ERROR.                    EL180
00549      PERFORM 8200-GET-ERROR-DESC  THRU 8200-EXIT.                 EL180
00550      MOVE WS-CURSOR              TO MPRTIDL.                      EL180
00551      PERFORM 1200-SEND-MAP       THRU 1200-EXIT.                  EL180
00552      GO TO 8400-RETURN-TRANS.                                     EL180
00553                                                                   EL180
00554  9930-EXIT.                                                       EL180
00555       EXIT.                                                       EL180
00556                                                                   EL180
00557  9940-NOTOPEN-CNTL.                                               EL180
00558      MOVE ER-0042                TO EMI-ERROR.                    EL180
00559      PERFORM 8200-GET-ERROR-DESC  THRU 8200-EXIT.                 EL180
00560      MOVE WS-CURSOR              TO MPRTIDL.                      EL180
00561      PERFORM 1200-SEND-MAP       THRU 1200-EXIT.                  EL180
00562      GO TO 8400-RETURN-TRANS.                                     EL180
00563                                                                   EL180
00564  9940-EXIT.                                                       EL180
00565       EXIT.                                                       EL180
00566                                                                   EL180
00567  9960-NO-CNTL-ERROR.                                              EL180
00568      MOVE ER-0022                TO EMI-ERROR                     EL180
00569      PERFORM 8200-GET-ERROR-DESC  THRU 8200-EXIT                  EL180
00570      MOVE WS-CURSOR              TO MPRTIDL.                      EL180
00571      PERFORM 1200-SEND-MAP       THRU   1200-EXIT.                EL180
00572                                                                   EL180
00573  9960-EXIT.                                                       EL180
00574       EXIT.                                                       EL180
00575                                                                   EL180
00576  9995-MULT-FUNC-ERROR.                                            EL180
00577      MOVE ER-0050                TO EMI-ERROR                     EL180
00578      PERFORM 8200-GET-ERROR-DESC  THRU 8200-EXIT.                 EL180
00579      MOVE WS-CURSOR              TO MPRTIDL.                      EL180
00580      PERFORM 1200-SEND-MAP       THRU 1200-EXIT.                  EL180
00581      GO TO 8400-RETURN-TRANS.                                     EL180
00582                                                                   EL180
00583  9995-MULT-FUNC-EXIT.                                             EL180
00584      EXIT.                                                        EL180
00585                                                                   EL180
00586  9995-SECURITY-VIOLATION.                                         EL180
00587                              COPY ELCSCTP.                        EL180
00588                                                                   EL180
00589  9995-EXIT.                                                       EL180
00590      EXIT.                                                        EL180
00591                                                                   EL180
00592  9996-INVALID-PF.                                                 EL180
00593      MOVE AL-UNBON               TO MPFNUMBA.                     EL180
00594      MOVE ER-0029                TO EMI-ERROR.                    EL180
00595      PERFORM 8200-GET-ERROR-DESC  THRU 8200-EXIT.                 EL180
00596      MOVE WS-CURSOR              TO MPFNUMBL.                     EL180
00597      PERFORM 1200-SEND-MAP       THRU 1200-EXIT.                  EL180
00598      GO TO 8400-RETURN-TRANS.                                     EL180
00599                                                                   EL180
00600  9996-EXIT.                                                       EL180
00601       EXIT.                                                       EL180
00602                                                                   EL180
00603  9998-WRONG-KEY.                                                  EL180
00604      PERFORM 1070-PRIME-MAP  THRU 1070-EXIT.                      EL180
00605      MOVE ER-7008                TO EMI-ERROR                     EL180
00606      PERFORM 8200-GET-ERROR-DESC  THRU 8200-EXIT.                 EL180
00607      MOVE WS-CURSOR              TO MPRTIDL.                      EL180
00608      PERFORM 1200-SEND-MAP       THRU 1200-EXIT.                  EL180
00609      GO TO 8400-RETURN-TRANS.                                     EL180
00610                                                                   EL180
00611  9998-EXIT.                                                       EL180
00612       EXIT.                                                       EL180
00613                                                                   EL180
00614  9999-PROGRAM-END.                                                EL180
00615      GOBACK.                                                      EL180
