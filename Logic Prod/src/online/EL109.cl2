00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL109
00003  PROGRAM-ID.                 EL109 .                                 LV003
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/12/96 09:46:48.                    CL**3
00007 *                            VMOD=2.003.                             CL**3
00008 *                                                                 EL109
00008 *                                                                 EL109
00009 *AUTHOR.    LOGIC, INC.                                              CL**3
00010 *           DALLAS, TEXAS.                                           CL**3
00011                                                                   EL109
00012 *DATE-COMPILED.                                                      CL**3
00013                                                                   EL109
00014 *SECURITY.   *****************************************************   CL**3
00015 *            *                                                   *   CL**3
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00017 *            *                                                   *   CL**3
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00021 *            *                                                   *   CL**3
00022 *            *****************************************************   CL**3
00023                                                                   EL109
00024 *REMARKS.                                                            CL**3
00025                                                                   EL109
00026 *    THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED          CL**3
00027 *    FOR THE PERSONAL REMINDERS OF THE CONTROL-FILE                  CL**3
00028 *    AN OPERATOR CAN MAINTAIN OWN REMINDERS, A SUPERVISOR            CL**3
00029 *    OR PERSON AUTHORIZED, CAN ACCESS/MAINTAIN OTHER OPERATORS       CL**3
00030 *    PERSONAL REMINDERS, CONTROL IS BASED ON PROG INTERFACE OPTNS.   CL**3
00031                                                                   EL109
00032 *    SCREENS     - EL109S - PERSONAL REMINDERS                       CL**3
00033                                                                   EL109
00034 *    ENTERED BY  - EL101 - MAINTENANCE MENU                          CL**3
00035                                                                   EL109
00036 *    EXIT TO     - EL101 - MAINTENANCE MENU                          CL**3
00037                                                                   EL109
00038 *    INPUT FILE  - ELCNTL - CONTROL FILE - PROCESSOR RECORDS         CL**3
00039                                                                   EL109
00040 *    OUTPUT FILE - ELCNTL - CONTROL FILE - PROCESSOR RECORDS         CL**3
00041                                                                   EL109
00042 *    COMMAREA    - PASSED                                            CL**3
00043                                                                   EL109
00044 *    ERROR-CODES ACCESSED - 70, 21, 22, 23, 29, 08, 04               CL**3
00045                                                                   EL109
00046 *    NARRATIVE   - PROVIDE ACCESS AND MODIFICATION CAPABILITY        CL**3
00047 *                  FOR THE PERSONAL REMINDERS IN THE CONTROL FILE    CL**3
00048 *                  OF THE OPERATOR. ALSO PROVIDE ACCESS AND          CL**3
00049 *                  MODIFICATION FUNCTION FOR THE SUPERVISOR          CL**3
00050 *                  OVER A GROUP OF OPERATORS.  PROVIDE INSTRUC-      CL**3
00051 *                  TIONS AND NOTES TO SUBORDINATES-                  CL**3
00052                                                                   EL109
00053      EJECT                                                        EL109
00054  ENVIRONMENT DIVISION.                                            EL109
00055  DATA DIVISION.                                                   EL109
00056  WORKING-STORAGE SECTION.                                         EL109
00057                                                                   EL109
00058  77  FILLER  PIC X(32)  VALUE '********************************'. EL109
00059  77  FILLER  PIC X(32)  VALUE '*    EL109 WORKING STORAGE     *'. EL109
00060  77  FILLER  PIC X(32)  VALUE '************ V/M 2.003 *********'.    CL**3
00061                                                                   EL109
00062  01  WS-DATE-AREA.                                                EL109
00063      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL109
00064      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL109
00065                                                                   EL109
00066  01  WS-SCRATCH-AREA.                                             EL109
00067      05  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL109S'.    EL109
00068                                                                   EL109
00069      05  WS-MAP-NAME                 PIC X(8)  VALUE 'EL109A'.    EL109
00070                                                                   EL109
00071      05  THIS-PGM                    PIC X(8)  VALUE 'EL109'.     EL109
00072                                                                   EL109
00073      05  WS-JOURNAL-TYPE-ID          PIC XX    VALUE 'EL'.        EL109
00074                                                                   EL109
00075      05  WS-TRANS-ID                 PIC X(4)    VALUE 'EX07'.    EL109
00076                                                                   EL109
00077      05  WS-CURSOR                   PIC S9(4)   VALUE -1  COMP.  EL109
00078                                                                   EL109
00079      05  WS-FIRST-MESSAGE           PIC X(70)   VALUE             EL109
00080        '    EXISTING MEMOS ARE SHOWN, IF BLANK LINES, NO MEMOS'.  EL109
00081      05  CF-SUB                      PIC S9(4) COMP VALUE ZEROS.  EL109
00082                                                                   EL109
00083      05  PF-SUB                      PIC 99    VALUE ZEROS.       EL109
00084      05  CURRENT-SAVE                PIC XX    VALUE LOW-VALUES.  EL109
00085      05  WS-MAINT-FUNC-SW            PIC X  VALUE SPACE.          EL109
00086          88  LOOKUP-REQUIRED         VALUE 'S'.                   EL109
00087          88  CHANGE-REQUIRED         VALUE 'C'.                   EL109
00088                                                                   EL109
00089      05  WS-AUTH-SW                  PIC X  VALUE SPACE.          EL109
00090          88  AUTHORIZED-CHANGE       VALUE 'C'.                   EL109
00091          88  AUTHORIZED-LOOKUP       VALUE 'S'.                   EL109
00092                                                                   EL109
00093      05  WS-INPUT-SW                 PIC X  VALUE SPACE.          EL109
00094          88  INPUT-ERROR             VALUE 'X'.                   EL109
00095                                                                   EL109
00096      05  TIME-IN                     PIC 9(7).                    EL109
00097      05  WS-TIME  REDEFINES TIME-IN.                              EL109
00098          10  FILLER                  PIC X.                       EL109
00099          10  TIME-OUT                PIC 99V99.                   EL109
00100          10  FILLER                  PIC XX.                      EL109
00101                                                                   EL109
00102      05  WS-DATE-MMDDYY-UNEDIT.                                   EL109
00103          10  FILLER                  PIC XX.                      EL109
00104          10  WS-DATE-MMDDYY              PIC X(6).                EL109
00105                                                                   EL109
00106      05  WS-SAVED-MSG-LINES      OCCURS 8 TIMES                   EL109
00107                                  INDEXED BY SAVED-INDEX.          EL109
00108          10  WS-SAVED-NOTIFY-DT  PIC XX.                          EL109
00109          10  WS-SAVED-ACTION-DT  PIC XX.                          EL109
00110          10  WS-SAVED-MSG-TXT    PIC X(50).                       EL109
00111      05  WS-CONTROL-FILE-KEY.                                     EL109
00112          10  WS-CFK-COMPANY-ID       PIC X(3).                    EL109
00113          10  WS-CFK-RECORD-TYPE      PIC X.                       EL109
00114          10  WS-CFK-PROCESSOR-ID     PIC XXXX.                    EL109
00115          10  WS-CFK-SEQUENCE-NO      PIC S9(4)  COMP.             EL109
00116                                                                   EL109
00117  01  ERROR-MESSAGES.                                              EL109
00118      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL109
00119      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL109
00120      12  ER-0021                 PIC X(4)  VALUE '0021'.          EL109
00121      12  ER-0022                 PIC X(4)  VALUE '0022'.          EL109
00122      12  ER-0023                 PIC X(4)  VALUE '0023'.          EL109
00123      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL109
00124      12  ER-0042                 PIC X(4)  VALUE '0042'.          EL109
00125      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL109
00126      12  ER-0484                 PIC X(4)  VALUE '0484'.          EL109
00127      12  ER-0485                 PIC X(4)  VALUE '0485'.          EL109
00128      12  ER-7008                 PIC X(4)  VALUE '7008'.          EL109
00129      EJECT                                                        EL109
00130      COPY ELCAID.                                                    CL**3
00131  01  PF-AID REDEFINES DFHAID.                                     EL109
00132      05  FILLER                      PIC X(8).                    EL109
00133      05  PF-VALUES  OCCURS 24        PIC X.                       EL109
00134      EJECT                                                        EL109
00135      COPY ELCINTF.                                                   CL**3
00136      EJECT                                                        EL109
00137      COPY ELCLOGOF.                                                  CL**3
00138      EJECT                                                        EL109
00139      COPY ELCATTR.                                                   CL**3
00140      EJECT                                                        EL109
00141      COPY ELCJPFX.                                                   CL**3
00142                          PIC X(750).                                 CL**3
00143      EJECT                                                        EL109
00144      COPY ELCDATE.                                                   CL**3
00145      EJECT                                                        EL109
00146      COPY ELCEMIB.                                                   CL**3
00147      EJECT                                                        EL109
00148      COPY ELCSCTM.                                                   CL**3
00149      EJECT                                                        EL109
00150      COPY EL109S SUPPRESS.                                           CL**3
00151  01  WS-M-MAP-AREA  REDEFINES EL109AI.                            EL109
00152      05  FILLER                      PIC X(72).                   EL109
00153                                                                   EL109
00154      05  M-MESSAGE-TEXT-LINES.                                    EL109
00155        07  FILLER               OCCURS 08 TIMES                   EL109
00156          INDEXED BY M-INDEX.                                      EL109
00157                                                                   EL109
00158          10  M-NOTIFY-DT-LENGTH PIC S9(4) COMP.                   EL109
00159          10  M-NOTIFY-DT-ATTRB  PIC X.                            EL109
00160          10  M-NOTIFY-DT        PIC X(8).                         EL109
00161                                                                   EL109
00162          10  M-ACTION-DT-LENGTH PIC S9(4) COMP.                   EL109
00163          10  M-ACTION-DT-ATTRB  PIC X.                            EL109
00164          10  M-ACTION-DT        PIC X(8).                         EL109
00165                                                                   EL109
00166          10  M-MSG-TXT-LENGTH   PIC S9(4) COMP.                   EL109
00167          10  M-MSG-TXT-ATTRB    PIC X.                            EL109
00168          10  M-MSG-TXT          PIC X(50).                        EL109
00169      EJECT                                                        EL109
00170  LINKAGE SECTION.                                                 EL109
00171  01  DFHCOMMAREA                     PIC X(1024).                 EL109
00172 *01 PARM-LIST .                                                      CL**3
00173 *    05  FILLER                      PIC S9(8) COMP.                 CL**3
00174 *    05  CNTL-POINTER                PIC S9(8) COMP.                 CL**3
00175      EJECT                                                        EL109
00176      COPY ELCCNTL.                                                   CL**3
00177      EJECT                                                        EL109
00178  PROCEDURE DIVISION.                                              EL109
00179                                                                   EL109
00180      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL109
00181      MOVE '5'                   TO DC-OPTION-CODE.                EL109
00182      PERFORM 8100-GET-DATE  THRU 8100-EXIT.                       EL109
00183      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL109
00184      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL109
00185                                                                   EL109
00186  0001-PROCESSING-EXITS.                                           EL109
00187      MOVE DFHCOMMAREA  TO  PROGRAM-INTERFACE-BLOCK.               EL109
00188                                                                   EL109
00189      IF EIBCALEN NOT GREATER THAN ZEROS                           EL109
00190        GO TO 8460-UNAUTHERR.                                      EL109
00191                                                                   EL109
00192      EXEC CICS  HANDLE CONDITION                                  EL109
00193             ERROR  (8300-ABEND)                                   EL109
00194             PGMIDERR (8440-PGMIDERR)                              EL109
00195      END-EXEC.                                                    EL109
00196                                                                   EL109
00197      EXEC CICS  HANDLE AID                                        EL109
00198             ENTER  (0700-ENTER)                                   EL109
00199             ANYKEY (0800-OTHER)                                   EL109
00200             CLEAR  (8800-CLEAR)                                   EL109
00201      END-EXEC.                                                    EL109
00202 *                                                                 EL109
00203      IF PI-CALLING-PROGRAM  NOT  = THIS-PGM                       EL109
00204          IF PI-RETURN-TO-PROGRAM NOT  = THIS-PGM                  EL109
00205              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL109
00206              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL109
00207              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL109
00208              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL109
00209              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL109
00210              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL109
00211              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL109
00212              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL109
00213              PERFORM 1070-PRIME-MAP      THRU 1070-EXIT           EL109
00214              PERFORM 0100-INITIALIZE     THRU 0100-EXIT           EL109
00215              PERFORM 3000-BUILD-KEY      THRU 3000-EXIT           EL109
00216              MOVE PI-PROCESSOR-ID  TO WS-CFK-PROCESSOR-ID         EL109
00217              PERFORM 1400-READ-CNTL-REC  THRU 1400-EXIT           EL109
00218              PERFORM 3500-BUILD-SCREEN   THRU 3500-EXIT           EL109
00219              MOVE WS-FIRST-MESSAGE       TO MERMSG1O              EL109
00220              PERFORM 1250-SEND-MAP-ERASE THRU 1250-EXIT           EL109
00221              GO TO 8400-RETURN-TRANS                              EL109
00222          ELSE                                                     EL109
00223              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL109
00224              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL109
00225              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL109
00226              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL109
00227              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL109
00228              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL109
00229              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL109
00230              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL109
00231      PERFORM 1000-RECEIVE      THRU 1000-EXIT.                    EL109
00232                                                                   EL109
00233      EJECT                                                        EL109
00234  0002-PROCESSING-MAINLINE.                                        EL109
00235      PERFORM 0100-INITIALIZE  THRU 0100-EXIT.                     EL109
00236                                                                   EL109
00237      IF LOOKUP-REQUIRED  OR CHANGE-REQUIRED                       EL109
00238          NEXT SENTENCE                                            EL109
00239      ELSE                                                         EL109
00240          GO TO 9992-FUNCTION-ERROR.                               EL109
00241                                                                   EL109
00242 ******************************************************************EL109
00243 ******    OPERATOR ACCESSING OWN REMINDERS      ******************EL109
00244 ******************************************************************EL109
00245      IF PI-PROCESSOR-ID = MPRCSSRI                                EL109
00246          MOVE 'C'  TO WS-AUTH-SW                                  EL109
00247      ELSE                                                         EL109
00248          PERFORM 0400-DETERMINE-AUTHORIZATION THRU 0400-EXIT.     EL109
00249                                                                   EL109
00250      IF CHANGE-REQUIRED                                           EL109
00251        AND AUTHORIZED-CHANGE                                      EL109
00252          SET M-INDEX TO 1                                         EL109
00253          SET SAVED-INDEX TO 1                                     EL109
00254          PERFORM 2100-EDIT-TEXT-LINE THRU 2100-EXIT 8 TIMES       EL109
00255              IF INPUT-ERROR                                       EL109
00256                  PERFORM 1200-SEND-MAP THRU 1200-EXIT             EL109
00257                  GO TO 8400-RETURN-TRANS                          EL109
00258              ELSE                                                 EL109
00259                  PERFORM 3000-BUILD-KEY        THRU 3000-EXIT     EL109
00260                  PERFORM 1600-READ-UP-CNTL-REC THRU 1600-EXIT     EL109
00261                  MOVE CONTROL-FILE             TO JP-RECORD-AREA  EL109
00262                  PERFORM 1900-WRITE-JOURNAL-B  THRU 1900-EXIT     EL109
00263                  PERFORM 3700-REBUILD-CNTL-REC THRU 3700-EXIT     EL109
00264                  MOVE CONTROL-FILE             TO JP-RECORD-AREA  EL109
00265                  PERFORM 1800-REWRITE-CNTL-REC THRU 1800-EXIT     EL109
00266                  PERFORM 1910-WRITE-JOURNAL-C  THRU 1910-EXIT     EL109
00267                  PERFORM 1400-READ-CNTL-REC    THRU 1400-EXIT     EL109
00268                  PERFORM 3500-BUILD-SCREEN     THRU 3500-EXIT     EL109
00269                  PERFORM 9990-NO-ERRORS        THRU 9990-EXIT     EL109
00270                  MOVE WS-CURSOR                TO MFMAINTL        EL109
00271                  MOVE 'S'                      TO MFMAINTO        EL109
00272                  PERFORM 1200-SEND-MAP         THRU 1200-EXIT     EL109
00273                  GO TO 8400-RETURN-TRANS.                         EL109
00274                                                                   EL109
00275      IF LOOKUP-REQUIRED                                           EL109
00276          IF AUTHORIZED-CHANGE                                     EL109
00277           OR AUTHORIZED-LOOKUP                                    EL109
00278 ******************************************************************EL109
00279 ******    CONTROL FILE INQUIRY & DISPLAY RTN    ******************EL109
00280 ******************************************************************EL109
00281              PERFORM 3000-BUILD-KEY      THRU 3000-EXIT           EL109
00282              PERFORM 1400-READ-CNTL-REC  THRU 1400-EXIT           EL109
00283              PERFORM 3500-BUILD-SCREEN   THRU 3500-EXIT           EL109
00284              MOVE WS-CURSOR              TO MFMAINTL              EL109
00285              PERFORM 9990-NO-ERRORS      THRU 9990-EXIT           EL109
00286              PERFORM 1250-SEND-MAP-ERASE THRU 1250-EXIT           EL109
00287              GO TO 8400-RETURN-TRANS.                             EL109
00288                                                                   EL109
00289 ******************************************************************EL109
00290 ******    OPERATOR NOT AUTHORIZED TO USE RECORD AS ENTERED *******EL109
00291 ******************************************************************EL109
00292      PERFORM 9991-NOAUTH-ERROR  THRU 9991-EXIT.                   EL109
00293      PERFORM 1200-SEND-MAP THRU 1200-EXIT                         EL109
00294      GO TO 8400-RETURN-TRANS.                                     EL109
00295                                                                   EL109
00296  0002-EXIT.                                                       EL109
00297      EJECT                                                        EL109
00298  0100-INITIALIZE.                                                 EL109
00299      MOVE 2 TO EMI-NUMBER-OF-LINES.                               EL109
00300      MOVE SPACE TO DC-ERROR-CODE.                                 EL109
00301                                                                   EL109
00302      MOVE '109A'       TO PI-CURRENT-SCREEN-NO.                   EL109
00303                                                                   EL109
00304      MOVE PI-PROCESSOR-ID   TO JP-USER-ID.                        EL109
00305      MOVE 'ELACCT '         TO JP-FILE-ID.                        EL109
00306      MOVE 'EL109'           TO JP-PROGRAM-ID.                     EL109
00307      MOVE ZEROS             TO JP-GENERIC-KEY-LENGTH.             EL109
00308                                                                   EL109
00309      MOVE   MFMAINTI        TO WS-MAINT-FUNC-SW.                  EL109
00310                                                                   EL109
00311      MOVE EIBTIME       TO TIME-IN.                               EL109
00312      MOVE TIME-OUT      TO MRNTIMEO.                              EL109
00313                                                                   EL109
00314      IF MPFNUMBI GREATER 0 AND LESS 25                            EL109
00315          MOVE MPFNUMBI  TO PF-SUB.                                EL109
00316                                                                   EL109
00317      MOVE SAVE-DATE       TO MRNDATEO.                            EL109
00318                                                                   EL109
00319      MOVE SAVE-BIN-DATE   TO CURRENT-SAVE.                        EL109
00320                                                                   EL109
00321      SET M-INDEX TO 1.                                            EL109
00322                                                                   EL109
00323      MOVE SPACES TO MERMSG1O.                                     EL109
00324      MOVE SPACES TO MERMSG2O.                                     EL109
00325                                                                   EL109
00326      SET SAVED-INDEX TO 1.                                        EL109
00327      PERFORM 0110-ERASE  8 TIMES.                                 EL109
00328                                                                   EL109
00329  0100-EXIT.                                                       EL109
00330       EXIT.                                                       EL109
00331                                                                   EL109
00332  0110-ERASE.                                                      EL109
00333      MOVE LOW-VALUES TO WS-SAVED-NOTIFY-DT (SAVED-INDEX).         EL109
00334      MOVE LOW-VALUES TO WS-SAVED-ACTION-DT (SAVED-INDEX).         EL109
00335      MOVE LOW-VALUES TO WS-SAVED-MSG-TXT   (SAVED-INDEX).         EL109
00336      SET SAVED-INDEX UP BY 1.                                     EL109
00337                                                                   EL109
00338  0110-EXIT.                                                       EL109
00339       EXIT.                                                       EL109
00340                                                                   EL109
00341  0400-DETERMINE-AUTHORIZATION.                                    EL109
00342      IF SYSTEM-MODIFY-CAP                                         EL109
00343          MOVE 'C'  TO WS-AUTH-SW                                  EL109
00344          GO TO 0400-EXIT.                                         EL109
00345      IF SYSTEM-DISPLAY-CAP                                        EL109
00346          MOVE 'S'  TO WS-AUTH-SW                                  EL109
00347          GO TO 0400-EXIT.                                         EL109
00348                                                                   EL109
00349       MOVE SPACE TO WS-AUTH-SW.                                   EL109
00350                                                                   EL109
00351  0400-EXIT.                                                       EL109
00352       EXIT.                                                       EL109
00353                                                                   EL109
00354  0700-ENTER.                                                      EL109
00355      IF MPFNUMBI = SPACES OR LOW-VALUES                           EL109
00356         GO TO 0002-PROCESSING-MAINLINE.                           EL109
00357                                                                   EL109
00358      IF MPFNUMBI GREATER 0 AND LESS 25                            EL109
00359         NEXT SENTENCE                                             EL109
00360      ELSE                                                         EL109
00361         GO TO 9996-INVALID-PF.                                    EL109
00362                                                                   EL109
00363      IF MPFNUMBI = 12                                             EL109
00364         MOVE  'EL010'   TO  THIS-PGM                              EL109
00365         GO TO 8600-XCTL.                                          EL109
00366      IF MPFNUMBI = 23                                             EL109
00367         GO TO 8700-XCTL-CL005.                                    EL109
00368      IF MPFNUMBI = 24                                             EL109
00369         MOVE  'EL126'   TO  THIS-PGM                              EL109
00370         GO TO 8600-XCTL.                                          EL109
00371                                                                   EL109
00372      GO TO 9996-INVALID-PF.                                       EL109
00373                                                                   EL109
00374  0700-EXIT.                                                       EL109
00375       EXIT.                                                       EL109
00376                                                                   EL109
00377  0800-OTHER.                                                      EL109
00378      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL109
00379         GO TO 9998-WRONG-KEY.                                     EL109
00380                                                                   EL109
00381      IF MPFNUMBI = LOW-VALUES OR SPACES                           EL109
00382           NEXT SENTENCE                                           EL109
00383      ELSE                                                         EL109
00384           GO TO 9995-MULT-FUNC-ERROR.                             EL109
00385                                                                   EL109
00386      IF EIBAID = DFHPF12                                          EL109
00387         MOVE  'EL010'   TO  THIS-PGM                              EL109
00388         GO TO 8600-XCTL.                                          EL109
00389                                                                   EL109
00390      IF EIBAID = DFHPF23                                          EL109
00391         GO TO 8700-XCTL-CL005.                                    EL109
00392                                                                   EL109
00393      IF EIBAID = DFHPF24                                          EL109
00394          MOVE 'EL126'  TO THIS-PGM                                EL109
00395          GO TO 8600-XCTL.                                         EL109
00396                                                                   EL109
00397      GO TO 9998-WRONG-KEY.                                        EL109
00398                                                                   EL109
00399  0800-EXIT.                                                       EL109
00400       EXIT.                                                       EL109
00401                                                                   EL109
00402  1000-RECEIVE.                                                    EL109
00403      EXEC CICS  HANDLE CONDITION                                  EL109
00404             MAPFAIL (9998-WRONG-KEY)                              EL109
00405      END-EXEC.                                                    EL109
00406                                                                   EL109
00407      EXEC CICS  RECEIVE                                           EL109
00408             MAP    (WS-MAP-NAME)                                  EL109
00409             MAPSET (WS-MAPSET-NAME)                               EL109
00410             INTO   (EL109AI)                                      EL109
00411      END-EXEC.                                                    EL109
00412                                                                   EL109
00413  1000-EXIT.                                                       EL109
00414       EXIT.                                                       EL109
00415                                                                   EL109
00416  1070-PRIME-MAP.                                                  EL109
00417      MOVE LOW-VALUES             TO EL109AI.                      EL109
00418      MOVE WS-CURSOR              TO MFMAINTL.                     EL109
00419      MOVE 'S'                    TO MFMAINTO.                     EL109
00420      MOVE PI-PROCESSOR-ID        TO MPRCSSRO.                     EL109
00421      MOVE AL-UANON               TO MFMAINTA                      EL109
00422                                     MPRCSSRA.                     EL109
00423                                                                   EL109
00424  1070-EXIT.                                                       EL109
00425       EXIT.                                                       EL109
00426      EJECT                                                        EL109
00427  1200-SEND-MAP.                                                   EL109
00428      MOVE EMI-LINE1          TO MERMSG1O.                         EL109
00429      MOVE EMI-LINE2          TO MERMSG2O.                         EL109
00430      EXEC CICS  SEND                                              EL109
00431                 FROM   (EL109AI)                                  EL109
00432                 MAPSET (WS-MAPSET-NAME)                           EL109
00433                 MAP    (WS-MAP-NAME)                              EL109
00434                 FREEKB                                            EL109
00435                 DATAONLY                                          EL109
00436                 CURSOR                                            EL109
00437      END-EXEC.                                                    EL109
00438                                                                   EL109
00439  1200-EXIT.                                                       EL109
00440       EXIT.                                                       EL109
00441                                                                   EL109
00442  1250-SEND-MAP-ERASE.                                             EL109
00443      MOVE EMI-LINE1          TO MERMSG1O.                         EL109
00444      MOVE EMI-LINE2          TO MERMSG2O.                         EL109
00445      EXEC CICS  SEND                                              EL109
00446                 FROM   (EL109AI)                                  EL109
00447                 MAPSET (WS-MAPSET-NAME)                           EL109
00448                 MAP    (WS-MAP-NAME)                              EL109
00449                 FREEKB                                            EL109
00450                 ERASE                                             EL109
00451                 CURSOR                                            EL109
00452      END-EXEC.                                                    EL109
00453                                                                   EL109
00454  1250-EXIT.                                                       EL109
00455       EXIT.                                                       EL109
00456      EJECT                                                        EL109
00457  1400-READ-CNTL-REC.                                              EL109
00458      PERFORM 1700-HANDLE-NOTFND    THRU 1700-EXIT.                EL109
00459      EXEC CICS  READ                                              EL109
00460             SET      (ADDRESS OF CONTROL-FILE)                       CL**3
00461             DATASET  ('ELCNTL')                                   EL109
00462             RIDFLD   (WS-CONTROL-FILE-KEY)                        EL109
00463      END-EXEC.                                                    EL109
00464                                                                   EL109
00465  1400-EXIT.                                                       EL109
00466       EXIT.                                                       EL109
00467                                                                   EL109
00468  1600-READ-UP-CNTL-REC.                                           EL109
00469      PERFORM 1700-HANDLE-NOTFND    THRU 1700-EXIT.                EL109
00470      EXEC CICS  READ                                              EL109
00471             SET      (ADDRESS OF CONTROL-FILE)                       CL**3
00472             DATASET  ('ELCNTL')                                   EL109
00473             RIDFLD   (WS-CONTROL-FILE-KEY)                        EL109
00474             UPDATE                                                EL109
00475      END-EXEC.                                                    EL109
00476                                                                   EL109
00477  1600-EXIT.                                                       EL109
00478       EXIT.                                                       EL109
00479                                                                   EL109
00480  1700-HANDLE-NOTFND.                                              EL109
00481      EXEC CICS  HANDLE CONDITION                                  EL109
00482             NOTFND  (9994-NOTFND)                                 EL109
00483             NOTOPEN (9981-NOTOPEN-CNTL)                           EL109
00484      END-EXEC.                                                    EL109
00485                                                                   EL109
00486  1700-EXIT.                                                       EL109
00487       EXIT.                                                       EL109
00488                                                                   EL109
00489  1800-REWRITE-CNTL-REC.                                           EL109
00490      EXEC CICS  REWRITE                                           EL109
00491             FROM     (CONTROL-FILE)                               EL109
00492             DATASET  ('ELCNTL')                                   EL109
00493      END-EXEC.                                                    EL109
00494                                                                   EL109
00495  1800-EXIT.                                                       EL109
00496       EXIT.                                                       EL109
00497      EJECT                                                        EL109
00498  1900-WRITE-JOURNAL-B.                                            EL109
00499      IF PI-JOURNAL-FILE-ID = 0                                    EL109
00500          GO TO 1900-EXIT.                                         EL109
00501                                                                   EL109
00502      MOVE 'B'               TO JP-RECORD-TYPE.                    EL109
00503 *    EXEC CICS  JOURNAL                                           EL109
00504 *           JFILEID   (PI-JOURNAL-FILE-ID)                        EL109
00505 *           JTYPEID   (WS-JOURNAL-TYPE-ID)                        EL109
00506 *           FROM      (JOURNAL-RECORD)                            EL109
00507 *           LENGTH    (773)                                          CL**2
00508 *    END-EXEC.                                                    EL109
00509                                                                   EL109
00510  1900-EXIT.                                                       EL109
00511       EXIT.                                                       EL109
00512                                                                   EL109
00513  1910-WRITE-JOURNAL-C.                                            EL109
00514      IF PI-JOURNAL-FILE-ID = 0                                    EL109
00515          GO TO 1910-EXIT.                                         EL109
00516                                                                   EL109
00517      MOVE 'C'               TO JP-RECORD-TYPE.                    EL109
00518 *    EXEC CICS  JOURNAL                                           EL109
00519 *           JFILEID   (1)                                         EL109
00520 *           JTYPEID   (WS-JOURNAL-TYPE-ID)                        EL109
00521 *           FROM      (JOURNAL-RECORD)                            EL109
00522 *           LENGTH    (773)                                          CL**2
00523 *    END-EXEC.                                                    EL109
00524                                                                   EL109
00525  1910-EXIT.                                                       EL109
00526       EXIT.                                                       EL109
00527      EJECT                                                        EL109
00528  2100-EDIT-TEXT-LINE.                                             EL109
00529      IF M-NOTIFY-DT (M-INDEX) = SPACES OR = LOW-VALUES            EL109
00530         MOVE LOW-VALUES   TO WS-SAVED-NOTIFY-DT (SAVED-INDEX)     EL109
00531        ELSE                                                       EL109
00532         MOVE M-NOTIFY-DT (M-INDEX) TO WS-DATE-MMDDYY-UNEDIT       EL109
00533         EXEC CICS  BIF  DEEDIT                                    EL109
00534                    FIELD (WS-DATE-MMDDYY-UNEDIT)                  EL109
00535                    LENGTH (8)                                     EL109
00536                    END-EXEC                                       EL109
00537          MOVE '4' TO DC-OPTION-CODE                               EL109
00538          MOVE WS-DATE-MMDDYY TO DC-GREG-DATE-1-MDY                EL109
00539          PERFORM 8100-GET-DATE  THRU 8100-EXIT                    EL109
00540          IF  DATE-CONVERSION-ERROR                                EL109
00541              MOVE ER-0021        TO EMI-ERROR                     EL109
00542              MOVE AL-UABON TO M-NOTIFY-DT-ATTRB  (M-INDEX)        EL109
00543              MOVE -1       TO M-NOTIFY-DT-LENGTH (M-INDEX)        EL109
00544              MOVE 'X'            TO WS-INPUT-SW                   EL109
00545              PERFORM 8200-GET-ERROR-DESC THRU 8200-EXIT           EL109
00546             ELSE                                                  EL109
00547              MOVE AL-UANON TO M-NOTIFY-DT-ATTRB  (M-INDEX)        EL109
00548              MOVE DC-BIN-DATE-1  TO                               EL109
00549                                WS-SAVED-NOTIFY-DT (SAVED-INDEX)   EL109
00550              MOVE DC-GREG-DATE-1-EDIT  TO M-NOTIFY-DT (M-INDEX).  EL109
00551                                                                   EL109
00552      IF M-ACTION-DT (M-INDEX) = SPACES OR = LOW-VALUES            EL109
00553         MOVE LOW-VALUES   TO WS-SAVED-ACTION-DT (SAVED-INDEX)     EL109
00554        ELSE                                                       EL109
00555         MOVE M-ACTION-DT (M-INDEX) TO WS-DATE-MMDDYY-UNEDIT       EL109
00556         EXEC CICS  BIF  DEEDIT                                    EL109
00557                    FIELD (WS-DATE-MMDDYY-UNEDIT)                  EL109
00558                    LENGTH (8)                                     EL109
00559                    END-EXEC                                       EL109
00560          MOVE '4' TO DC-OPTION-CODE                               EL109
00561          MOVE WS-DATE-MMDDYY TO DC-GREG-DATE-1-MDY                EL109
00562          PERFORM 8100-GET-DATE  THRU 8100-EXIT                    EL109
00563          IF  DATE-CONVERSION-ERROR                                EL109
00564              MOVE ER-0021        TO EMI-ERROR                     EL109
00565              MOVE AL-UABON TO M-ACTION-DT-ATTRB  (M-INDEX)        EL109
00566              MOVE -1       TO M-ACTION-DT-LENGTH (M-INDEX)        EL109
00567              MOVE 'X'            TO WS-INPUT-SW                   EL109
00568              PERFORM 8200-GET-ERROR-DESC THRU 8200-EXIT           EL109
00569             ELSE                                                  EL109
00570              MOVE AL-UANON TO M-ACTION-DT-ATTRB  (M-INDEX)        EL109
00571              MOVE DC-BIN-DATE-1  TO                               EL109
00572                                WS-SAVED-ACTION-DT (SAVED-INDEX)   EL109
00573              MOVE DC-GREG-DATE-1-EDIT  TO M-ACTION-DT (M-INDEX).  EL109
00574                                                                   EL109
00575      IF M-MSG-TXT-LENGTH (M-INDEX) NOT = ZEROS                    EL109
00576        MOVE M-MSG-TXT (M-INDEX) TO WS-SAVED-MSG-TXT (SAVED-INDEX) EL109
00577       ELSE                                                        EL109
00578        MOVE SPACES             TO WS-SAVED-MSG-TXT (SAVED-INDEX). EL109
00579                                                                   EL109
00580      IF WS-SAVED-NOTIFY-DT (SAVED-INDEX) NOT = LOW-VALUES AND     EL109
00581         WS-SAVED-ACTION-DT (SAVED-INDEX) NOT = LOW-VALUES         EL109
00582         IF WS-SAVED-ACTION-DT (SAVED-INDEX) LESS THAN             EL109
00583            WS-SAVED-NOTIFY-DT (SAVED-INDEX)                       EL109
00584            MOVE ER-0484          TO EMI-ERROR                     EL109
00585            MOVE AL-UABON TO M-ACTION-DT-ATTRB  (M-INDEX)          EL109
00586            MOVE AL-UABON TO M-NOTIFY-DT-ATTRB  (M-INDEX)          EL109
00587            MOVE -1       TO M-NOTIFY-DT-LENGTH (M-INDEX)          EL109
00588            MOVE 'X'            TO WS-INPUT-SW                     EL109
00589            PERFORM 8200-GET-ERROR-DESC THRU 8200-EXIT.            EL109
00590                                                                   EL109
00591      IF WS-SAVED-ACTION-DT (SAVED-INDEX) NOT = LOW-VALUES         EL109
00592        IF WS-SAVED-ACTION-DT (SAVED-INDEX) LESS THAN CURRENT-SAVE EL109
00593            MOVE ER-0485          TO EMI-ERROR                     EL109
00594            MOVE AL-UABON TO M-ACTION-DT-ATTRB  (M-INDEX)          EL109
00595            MOVE -1       TO M-ACTION-DT-LENGTH (M-INDEX)          EL109
00596            MOVE 'X'            TO WS-INPUT-SW                     EL109
00597            PERFORM 8200-GET-ERROR-DESC THRU 8200-EXIT.            EL109
00598                                                                   EL109
00599      IF WS-SAVED-NOTIFY-DT (SAVED-INDEX) NOT = LOW-VALUES         EL109
00600        IF WS-SAVED-NOTIFY-DT (SAVED-INDEX) LESS THAN CURRENT-SAVE EL109
00601            MOVE ER-0485          TO EMI-ERROR                     EL109
00602            MOVE AL-UABON TO M-NOTIFY-DT-ATTRB  (M-INDEX)          EL109
00603            MOVE -1       TO M-NOTIFY-DT-LENGTH (M-INDEX)          EL109
00604            MOVE 'X'            TO WS-INPUT-SW                     EL109
00605            PERFORM 8200-GET-ERROR-DESC THRU 8200-EXIT.            EL109
00606                                                                   EL109
00607      SET M-INDEX UP BY 1.                                         EL109
00608      SET SAVED-INDEX UP BY 1.                                     EL109
00609                                                                   EL109
00610  2100-EXIT.                                                       EL109
00611       EXIT.                                                       EL109
00612      EJECT                                                        EL109
00613  3000-BUILD-KEY.                                                  EL109
00614      MOVE PI-COMPANY-ID     TO WS-CFK-COMPANY-ID.                 EL109
00615      MOVE 'R'               TO WS-CFK-RECORD-TYPE.                EL109
00616      MOVE MPRCSSRI          TO WS-CFK-PROCESSOR-ID.               EL109
00617      MOVE ZEROS             TO WS-CFK-SEQUENCE-NO.                EL109
00618                                                                   EL109
00619  3000-EXIT.                                                       EL109
00620       EXIT.                                                       EL109
00621                                                                   EL109
00622  3500-BUILD-SCREEN.                                               EL109
00623      MOVE CF-LAST-MAINT-DT   TO DC-BIN-DATE-1.                    EL109
00624      MOVE ' '  TO DC-OPTION-CODE.                                 EL109
00625      PERFORM 8100-GET-DATE   THRU 8100-EXIT.                      EL109
00626      MOVE DC-GREG-DATE-1-EDIT  TO MMODDATO.                       EL109
00627      MOVE CF-LAST-MAINT-BY     TO MMODRIDO.                       EL109
00628      MOVE CF-LAST-MAINT-HHMMSS TO TIME-IN                         EL109
00629      MOVE TIME-OUT             TO MMODTMEO.                       EL109
00630                                                                   EL109
00631      IF SYSTEM-MODIFY-CAP                                         EL109
00632          MOVE '*' TO MMODSUPO.                                    EL109
00633      MOVE 1 TO CF-SUB.                                            EL109
00634      SET M-INDEX TO 1.                                            EL109
00635      PERFORM 3510-BUILD-SCREEN  THRU 3510-EXIT 8 TIMES.           EL109
00636      MOVE WS-CURSOR TO MFMAINTL.                                  EL109
00637                                                                   EL109
00638  3500-EXIT.                                                       EL109
00639       EXIT.                                                       EL109
00640                                                                   EL109
00641  3510-BUILD-SCREEN.                                               EL109
00642      IF  CF-START-REMIND-DT (CF-SUB)  = SPACES                    EL109
00643                                             OR LOW-VALUES         EL109
00644          MOVE SPACES              TO M-NOTIFY-DT (M-INDEX)        EL109
00645          GO TO 3511-END-DATE.                                     EL109
00646                                                                   EL109
00647      MOVE CF-START-REMIND-DT (CF-SUB)  TO                         EL109
00648           DC-BIN-DATE-1.                                          EL109
00649                                                                   EL109
00650      MOVE ' ' TO DC-OPTION-CODE.                                  EL109
00651      PERFORM 8100-GET-DATE  THRU 8100-EXIT.                       EL109
00652      IF DATE-CONVERSION-ERROR                                     EL109
00653          MOVE SPACES TO M-NOTIFY-DT (M-INDEX)                     EL109
00654      ELSE                                                         EL109
00655          MOVE DC-GREG-DATE-1-EDIT TO M-NOTIFY-DT (M-INDEX).       EL109
00656                                                                   EL109
00657  3511-END-DATE.                                                   EL109
00658      IF  CF-END-REMIND-DT (CF-SUB)  = SPACES                      EL109
00659                                             OR LOW-VALUES         EL109
00660          MOVE SPACES              TO M-ACTION-DT (M-INDEX)        EL109
00661          GO TO 3512-TEXT.                                         EL109
00662                                                                   EL109
00663      MOVE CF-END-REMIND-DT (CF-SUB)  TO                           EL109
00664           DC-BIN-DATE-1.                                          EL109
00665                                                                   EL109
00666      MOVE ' ' TO DC-OPTION-CODE.                                  EL109
00667      PERFORM 8100-GET-DATE  THRU 8100-EXIT.                       EL109
00668      IF DATE-CONVERSION-ERROR                                     EL109
00669          MOVE SPACES TO M-ACTION-DT (M-INDEX)                     EL109
00670      ELSE                                                         EL109
00671          MOVE DC-GREG-DATE-1-EDIT TO                              EL109
00672                        M-ACTION-DT (M-INDEX).                     EL109
00673                                                                   EL109
00674  3512-TEXT.                                                       EL109
00675      MOVE CF-REMINDER-TEXT (CF-SUB)  TO                           EL109
00676                    M-MSG-TXT (M-INDEX).                           EL109
00677      ADD 1 TO CF-SUB.                                             EL109
00678      SET M-INDEX UP BY 1.                                         EL109
00679                                                                   EL109
00680  3510-EXIT.                                                       EL109
00681       EXIT.                                                       EL109
00682                                                                   EL109
00683  3700-REBUILD-CNTL-REC.                                           EL109
00684      MOVE WS-CONTROL-FILE-KEY  TO  CF-CONTROL-PRIMARY.            EL109
00685      MOVE PI-PROCESSOR-ID      TO CF-LAST-MAINT-BY.               EL109
00686      MOVE TIME-IN              TO CF-LAST-MAINT-HHMMSS.           EL109
00687      MOVE CURRENT-SAVE         TO CF-LAST-MAINT-DT.               EL109
00688      MOVE 1                    TO CF-SUB.                         EL109
00689      SET SAVED-INDEX TO 1.                                        EL109
00690      PERFORM 3710-MOVE-SAVED-ARRAY THRU 3710-EXIT  8 TIMES.       EL109
00691                                                                   EL109
00692      IF CF-SUB NOT = 9                                            EL109
00693         PERFORM 3720-CLEAR THRU 3720-EXIT                         EL109
00694             VARYING CF-SUB FROM CF-SUB BY 1 UNTIL CF-SUB = 9.     EL109
00695                                                                   EL109
00696  3700-EXIT.                                                       EL109
00697       EXIT.                                                       EL109
00698                                                                   EL109
00699  3710-MOVE-SAVED-ARRAY.                                           EL109
00700      IF WS-SAVED-NOTIFY-DT (SAVED-INDEX) = LOW-VALUES AND         EL109
00701         WS-SAVED-ACTION-DT (SAVED-INDEX) = LOW-VALUES AND         EL109
00702         WS-SAVED-MSG-TXT   (SAVED-INDEX) = SPACES                 EL109
00703         SET SAVED-INDEX UP BY 1                                   EL109
00704        ELSE                                                       EL109
00705         MOVE WS-SAVED-MSG-TXT   (SAVED-INDEX)  TO                 EL109
00706              CF-REMINDER-TEXT   (CF-SUB)                          EL109
00707         MOVE WS-SAVED-NOTIFY-DT (SAVED-INDEX) TO                  EL109
00708              CF-START-REMIND-DT (CF-SUB)                          EL109
00709         MOVE WS-SAVED-ACTION-DT (SAVED-INDEX) TO                  EL109
00710              CF-END-REMIND-DT   (CF-SUB)                          EL109
00711         ADD 1 TO CF-SUB                                           EL109
00712         SET SAVED-INDEX UP BY 1.                                  EL109
00713                                                                   EL109
00714  3710-EXIT.                                                       EL109
00715       EXIT.                                                       EL109
00716                                                                   EL109
00717  3720-CLEAR.                                                      EL109
00718      MOVE LOW-VALUES             TO CF-START-REMIND-DT (CF-SUB)   EL109
00719      MOVE LOW-VALUES             TO CF-END-REMIND-DT   (CF-SUB)   EL109
00720      MOVE SPACES                 TO CF-REMINDER-TEXT   (CF-SUB).  EL109
00721                                                                   EL109
00722  3720-EXIT.                                                       EL109
00723       EXIT.                                                       EL109
00724      EJECT                                                        EL109
00725  8100-GET-DATE.                                                   EL109
00726      EXEC CICS LINK                                               EL109
00727             PROGRAM  ('ELDATCV')                                  EL109
00728             COMMAREA (DATE-CONVERSION-DATA)                       EL109
00729             LENGTH   (DC-COMM-LENGTH)                             EL109
00730      END-EXEC.                                                    EL109
00731                                                                   EL109
00732  8100-EXIT.                                                       EL109
00733       EXIT.                                                       EL109
00734                                                                   EL109
00735  8200-GET-ERROR-DESC.                                             EL109
00736      EXEC CICS LINK                                               EL109
00737             PROGRAM  ('EL001')                                    EL109
00738             COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)              EL109
00739             LENGTH   (EMI-COMM-LENGTH)                            EL109
00740      END-EXEC.                                                    EL109
00741                                                                   EL109
00742  8200-EXIT.                                                       EL109
00743       EXIT.                                                       EL109
00744                                                                   EL109
00745  8300-ABEND.                                                      EL109
00746      MOVE DFHEIBLK               TO EMI-LINE1.                    EL109
00747      EXEC CICS LINK                                               EL109
00748          PROGRAM   ('EL004')                                      EL109
00749          COMMAREA  (EMI-LINE1)                                    EL109
00750          LENGTH    (72)                                           EL109
00751          END-EXEC.                                                EL109
00752                                                                   EL109
00753      PERFORM 1200-SEND-MAP.                                       EL109
00754      GO TO 8400-RETURN-TRANS.                                     EL109
00755                                                                   EL109
00756  8400-RETURN-TRANS.                                               EL109
00757      EXEC CICS  RETURN                                            EL109
00758                 TRANSID  (WS-TRANS-ID)                            EL109
00759                 COMMAREA (PROGRAM-INTERFACE-BLOCK)                EL109
00760                 LENGTH   (PI-COMM-LENGTH)                         EL109
00761      END-EXEC.                                                    EL109
00762                                                                   EL109
00763  8400-EXIT.                                                       EL109
00764       EXIT.                                                       EL109
00765                                                                   EL109
00766  8440-PGMIDERR.                                                   EL109
00767      EXEC CICS  HANDLE CONDITION                                  EL109
00768                 PGMIDERR (8480-SEND-TEXT)                         EL109
00769      END-EXEC.                                                    EL109
00770      MOVE THIS-PGM        TO LOGOFF-PGM  PI-CALLING-PROGRAM.      EL109
00771      MOVE SPACES          TO PI-ENTRY-CD-1.                       EL109
00772      MOVE 'EL005'         TO THIS-PGM.                            EL109
00773      MOVE PGMIDERR-MSG    TO LOGOFF-FILL.                         EL109
00774      GO TO 8600-XCTL.                                             EL109
00775                                                                   EL109
00776  8440-EXIT.                                                       EL109
00777       EXIT.                                                       EL109
00778                                                                   EL109
00779  8460-UNAUTHERR.                                                  EL109
00780      MOVE UNACCESS-MSG    TO LOGOFF-MSG.                          EL109
00781                                                                   EL109
00782  8480-SEND-TEXT.                                                  EL109
00783      EXEC CICS  SEND  TEXT                                        EL109
00784                 FROM     (LOGOFF-TEXT)                            EL109
00785                 ERASE                                             EL109
00786                 FREEKB                                            EL109
00787                 LENGTH   (LOGOFF-LENGTH)                          EL109
00788      END-EXEC.                                                    EL109
00789                                                                   EL109
00790      GO TO 8500-RETURN-CICS.                                      EL109
00791                                                                   EL109
00792  8460-EXIT.                                                       EL109
00793       EXIT.                                                       EL109
00794                                                                   EL109
00795  8500-RETURN-CICS.                                                EL109
00796 *    PERFORM 1300-SEND-TEXT THRU 1300-EXIT.                       EL109
00797      EXEC CICS  RETURN                                            EL109
00798      END-EXEC.                                                    EL109
00799                                                                   EL109
00800  8600-XCTL.                                                       EL109
00801      EXEC CICS  HANDLE CONDITION                                  EL109
00802             PGMIDERR (8900-PGMIDERR)                              EL109
00803      END-EXEC.                                                    EL109
00804      EXEC CICS XCTL                                               EL109
00805            PROGRAM  (THIS-PGM)                                    EL109
00806            COMMAREA (PROGRAM-INTERFACE-BLOCK)                     EL109
00807            LENGTH   (PI-COMM-LENGTH)                              EL109
00808      END-EXEC.                                                    EL109
00809                                                                   EL109
00810  8600-EXIT.                                                       EL109
00811       EXIT.                                                       EL109
00812                                                                   EL109
00813  8700-XCTL-CL005.                                                 EL109
00814      MOVE 'EL005' TO THIS-PGM.                                    EL109
00815      MOVE '.'       TO   PI-ENTRY-CD-1.                           EL109
00816 *     NOTE, PERIOD IS EQUIVALENT TO '4B' THE VALUE OF DFHPF23     EL109
00817      GO TO 8600-XCTL.                                             EL109
00818                                                                   EL109
00819  8700-EXIT.                                                       EL109
00820                                                                   EL109
00821  8800-CLEAR.                                                      EL109
00822      MOVE PI-RETURN-TO-PROGRAM TO THIS-PGM.                       EL109
00823      GO TO 8600-XCTL.                                             EL109
00824  8800-EXIT.                                                       EL109
00825                                                                   EL109
00826  8900-PGMIDERR.                                                   EL109
00827      MOVE ' '  TO PI-ENTRY-CD-1.                                  EL109
00828      MOVE 'EL005' TO THIS-PGM.                                    EL109
00829      GO TO 8600-XCTL.                                             EL109
00830                                                                   EL109
00831  8900-EXIT.                                                       EL109
00832      EJECT                                                        EL109
00833  9990-NO-ERRORS.                                                  EL109
00834      MOVE ER-0000   TO EMI-ERROR                                  EL109
00835      PERFORM 8200-GET-ERROR-DESC THRU   8200-EXIT.                EL109
00836                                                                   EL109
00837  9990-EXIT.                                                       EL109
00838       EXIT.                                                       EL109
00839                                                                   EL109
00840  9981-NOTOPEN-CNTL.                                               EL109
00841      MOVE ER-0042   TO EMI-ERROR                                  EL109
00842      PERFORM 8200-GET-ERROR-DESC  THRU 8200-EXIT.                 EL109
00843      MOVE WS-CURSOR TO MFMAINTL.                                  EL109
00844      PERFORM 1200-SEND-MAP       THRU 1200-EXIT.                  EL109
00845      GO TO 8400-RETURN-TRANS.                                     EL109
00846                                                                   EL109
00847  9981-EXIT.                                                       EL109
00848       EXIT.                                                       EL109
00849                                                                   EL109
00850  9991-NOAUTH-ERROR.                                               EL109
00851      MOVE AL-UABON  TO MPRCSSRA.                                  EL109
00852      MOVE ER-0070   TO EMI-ERROR.                                 EL109
00853      PERFORM 9993-SECURITY-VIOLATION THRU 9993-EXIT.              EL109
00854      PERFORM 8200-GET-ERROR-DESC THRU   8200-EXIT.                EL109
00855      SET M-INDEX  TO 1.                                           EL109
00856      MOVE SPACES      TO MFMAINTO                                 EL109
00857                          MPRCSSRO.                                EL109
00858      MOVE WS-CURSOR   TO MFMAINTL.                                EL109
00859                                                                   EL109
00860  9991-EXIT.                                                       EL109
00861       EXIT.                                                       EL109
00862                                                                   EL109
00863  9992-FUNCTION-ERROR.                                             EL109
00864      MOVE AL-UABON TO MFMAINTA.                                   EL109
00865      MOVE SPACES                 TO M-MESSAGE-TEXT-LINES.         EL109
00866      MOVE ER-0023   TO EMI-ERROR                                  EL109
00867      PERFORM 8200-GET-ERROR-DESC  THRU 8200-EXIT                  EL109
00868      MOVE WS-CURSOR   TO MFMAINTL.                                EL109
00869      PERFORM 1250-SEND-MAP-ERASE THRU 1250-EXIT.                  EL109
00870      GO TO 8400-RETURN-TRANS.                                     EL109
00871                                                                   EL109
00872  9992-EXIT.                                                       EL109
00873       EXIT.                                                       EL109
00874                                                                   EL109
uktdel*9993-SECURITY-VIOLATION.   COPY ELCSCTP.                         EL109
uktins 9993-SECURITY-VIOLATION.
uktins      COPY ELCSCTP.
00876                                                                   EL109
00877  9993-EXIT.                                                       EL109
00878       EXIT.                                                       EL109
00879                                                                   EL109
00880  9994-NOTFND.                                                     EL109
00881      MOVE AL-UABON TO MPRCSSRA.                                   EL109
00882      MOVE ER-0022   TO EMI-ERROR.                                 EL109
00883      MOVE SPACES                 TO M-MESSAGE-TEXT-LINES.         EL109
00884      PERFORM 8200-GET-ERROR-DESC THRU 8200-EXIT.                  EL109
00885      MOVE WS-CURSOR TO MFMAINTL.                                  EL109
00886      PERFORM 1250-SEND-MAP-ERASE THRU 1250-EXIT.                  EL109
00887      GO TO 8400-RETURN-TRANS.                                     EL109
00888                                                                   EL109
00889  9994-EXIT.                                                       EL109
00890       EXIT.                                                       EL109
00891                                                                   EL109
00892  9995-MULT-FUNC-ERROR.                                            EL109
00893      MOVE AL-UNBON TO MPFNUMBA.                                   EL109
00894      MOVE ER-0004   TO EMI-ERROR                                  EL109
00895      PERFORM 8200-GET-ERROR-DESC  THRU 8200-EXIT.                 EL109
00896      MOVE WS-CURSOR TO MPFNUMBL.                                  EL109
00897      PERFORM 1200-SEND-MAP       THRU 1200-EXIT.                  EL109
00898      GO TO 8400-RETURN-TRANS.                                     EL109
00899                                                                   EL109
00900  9995-EXIT.                                                       EL109
00901       EXIT.                                                       EL109
00902                                                                   EL109
00903  9996-INVALID-PF.                                                 EL109
00904      MOVE AL-UNBON TO MPFNUMBA.                                   EL109
00905      MOVE ER-0029   TO EMI-ERROR                                  EL109
00906      PERFORM 8200-GET-ERROR-DESC  THRU 8200-EXIT.                 EL109
00907      MOVE WS-CURSOR TO MPFNUMBL.                                  EL109
00908      PERFORM 1200-SEND-MAP       THRU 1200-EXIT.                  EL109
00909      GO TO 8400-RETURN-TRANS.                                     EL109
00910                                                                   EL109
00911  9996-EXIT.                                                       EL109
00912       EXIT.                                                       EL109
00913                                                                   EL109
00914  9998-WRONG-KEY.                                                  EL109
00915      PERFORM 1070-PRIME-MAP  THRU 1070-EXIT.                      EL109
00916      MOVE ER-7008   TO EMI-ERROR                                  EL109
00917      PERFORM 8200-GET-ERROR-DESC  THRU 8200-EXIT.                 EL109
00918      PERFORM 1200-SEND-MAP       THRU 1200-EXIT.                  EL109
00919      GO TO 8400-RETURN-TRANS.                                     EL109
00920      GOBACK.                                                      EL109
00921                                                                   EL109
00922  9998-EXIT.                                                       EL109
00923       EXIT.                                                       EL109
