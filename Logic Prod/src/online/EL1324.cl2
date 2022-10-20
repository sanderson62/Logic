00001  IDENTIFICATION DIVISION.                                         06/04/96
00002                                                                   EL1324
00003  PROGRAM-ID.                 EL1324.                                 LV003
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 02/13/96 09:48:12.                    CL**2
00007 *                            VMOD=2.003                              CL**3
00008 *                                                                 EL1324
00008 *                                                                 EL1324
00009 *AUTHOR.    LOGIC, INC.                                              CL**2
00010 *           DALLAS, TEXAS.                                           CL**2
00011                                                                   EL1324
00012 *DATE-COMPILED.                                                      CL**2
00013                                                                   EL1324
00014 *SECURITY.   *****************************************************   CL**2
00015 *            *                                                   *   CL**2
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**2
00017 *            *                                                   *   CL**2
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00021 *            *                                                   *   CL**2
00022 *            *****************************************************   CL**2
00023                                                                   EL1324
00024 *REMARKS.                                                         EL1324
00025 *        THIS PROGRAM PROVIDES THE QUALIFICATION NECESSARY FOR    EL1324
00026 *    THE ALPHA LOOK-UP.                                           EL1324
00027                                                                   EL1324
00028 *    SCREENS     - EL132D  - ALPHA LOOK-UP QUALIFICATION          EL1324
00029                                                                   EL1324
00030 *    ENTERED BY  - EL126 - MAINTENANCE MENU                       EL1324
00031 *                  EL132 - CLAIM LOOK-UP                          EL1324
00032                                                                   EL1324
00033 *    EXIT TO     - CALLING PROGRAM                                EL1324
00034 *                  EL1322 - CLAIM LOOK-UP FOR STATUS              EL1324
00035                                                                   EL1324
00036 *    INPUT FILE  - ELALPH - ALPHA FILE                            EL1324
00037                                                                   EL1324
00038 *    OUTPUT FILE - NONE                                           EL1324
00039                                                                   EL1324
00040 *    COMMAREA    - PASSED.  IF A NAME IS SELECED, THE CONTROL OF  EL1324
00041 *                  THAT NAME IS PLACED IN THE APPROPRIATE FIELDS  EL1324
00042 *                  OF THE COMMAREA FOR REFERENCE BY SUCCESSIVE    EL1324
00043 *                  PROGRAMS.  THE PROGRAM WORK AREA OF THE        EL1324
00044 *                  COMMAREA IS USED TO PASS THE RECORD KEY        EL1324
00045 *                  INFORMATION NEEDED BY EL1325 TO LOCATE THE     EL1324
00046 *                  NAME.                                          EL1324
00047                                                                   EL1324
00048 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON     EL1324
00049 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE EL1324
00050 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVEEL1324
00051 *                  ENTRIES (XCTL FROM CICS VIA EX21) THE SCREEN   EL1324
00052 *                  WILL BE READ AND ACTION WILL BE BASED ON THE   EL1324
00053 *                  MAINTENANCE TYPE INDICATED.                    EL1324
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID & CO ID TO SCREEN
101501******************************************************************

00054                                                                   EL1324
00055      EJECT                                                        EL1324
00056  ENVIRONMENT DIVISION.                                            EL1324
00057                                                                   EL1324
00058  DATA DIVISION.                                                   EL1324
00059                                                                   EL1324
00060  WORKING-STORAGE SECTION.                                         EL1324
00061                                                                   EL1324
00062  77  FILLER  PIC X(32)  VALUE '********************************'. EL1324
00063  77  FILLER  PIC X(32)  VALUE '*  EL1324  WORKING STORAGE     *'. EL1324
00064  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.003 **********'.    CL**3
00065                                                                   EL1324
00066                                  COPY ELCSCTM.                    EL1324
00067                                                                   EL1324
00068                                  COPY ELCSCRTY.                   EL1324
00069                                                                   EL1324
00070  01  WS-DATE-AREA.                                                EL1324
00071      12  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL1324
00072      12  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL1324
00073                                                                   EL1324
00074  01  FILLER                          COMP-3.                      EL1324
00075                                                                   EL1324
00076      12  TIME-IN                 PIC S9(7)   VALUE ZERO.          EL1324
00077      12  TIME-OUT                    REDEFINES                    EL1324
00078          TIME-IN                 PIC S9(3)V9(4).                  EL1324
00079      12  WS-CALL-SW              PIC S9      VALUE ZERO.          EL1324
00080                                                                   EL1324
00081  01  FILLER                          COMP  SYNC.                  EL1324
00082                                                                   EL1324
00083      12  SC-ITEM                 PIC S9(4)   VALUE +0001.         EL1324
00084      12  WS-INDEX                PIC S9(4)   VALUE ZERO.          EL1324
00085      EJECT                                                        EL1324
00086                                                                   EL1324
00087  01  FILLER.                                                      EL1324
00088                                                                   EL1324
00089      12  ALPHA-INDEX-DSID        PIC X(8)    VALUE 'ELALPH'.      EL1324
00090      12  ALPHA2-INDEX-DSID       PIC X(8)    VALUE 'ELALPH2'.     EL1324
00091      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL1324
00092      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL1324
00093      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL1324
00094                                                                   EL1324
00095      12  XCTL-005                PIC X(8)    VALUE 'EL005'.       EL1324
00096      12  XCTL-010                PIC X(8)    VALUE 'EL010'.       EL1324
00097      12  XCTL-126                PIC X(8)    VALUE 'EL126'.       EL1324
00098      12  XCTL-132                PIC X(8)    VALUE 'EL132'.       EL1324
00099      12  XCTL-150                PIC X(8)    VALUE 'EL150'.       EL1324
00100      12  XCTL-1324               PIC X(8)    VALUE 'EL1324'.      EL1324
00101      12  XCTL-1325               PIC X(8)    VALUE 'EL1325'.      EL1324
00102      12  THIS-PGM                PIC X(8)    VALUE 'EL1324'.      EL1324
00103                                                                   EL1324
00104      12  WS-MAPSET-NAME          PIC X(8)  VALUE 'EL1324S'.       EL1324
00105      12  WS-MAP-NAME             PIC X(8)  VALUE 'EL1324A'.       EL1324
00106      12  FILLER                      REDEFINES                    EL1324
00107          WS-MAP-NAME.                                             EL1324
00108          16  FILLER              PIC X(2).                        EL1324
00109          16  WS-MAP-NUMBER       PIC X(4).                        EL1324
00110          16  FILLER              PIC X(2).                        EL1324
00111                                                                   EL1324
00112      12  WS-TRANS-ID             PIC X(4)   VALUE 'E033'.         EL1324
00113                                                                   EL1324
00114      12  WS-INPUT-FIELD          PIC X(50)  VALUE SPACES.         EL1324
00115      12  WS-INPUT-CHAR  REDEFINES  WS-INPUT-FIELD  PIC X(01)      EL1324
00116               OCCURS 50 TIMES   INDEXED BY  INPUT-INDEX.          EL1324
00117                                                                   EL1324
00118  01  ERROR-MESSAGES.                                              EL1324
00119      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL1324
00120      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL1324
00121      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL1324
00122      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL1324
00123      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL1324
00124      12  ER-0194                 PIC X(4)  VALUE '0194'.          EL1324
00125      12  ER-0195                 PIC X(4)  VALUE '0195'.          EL1324
00126      12  ER-0196                 PIC X(4)  VALUE '0196'.          EL1324
00127      12  ER-0197                 PIC X(4)  VALUE '0197'.          EL1324
00128      12  ER-0203                 PIC X(4)  VALUE '0203'.          EL1324
00129      12  ER-0209                 PIC X(4)  VALUE '0209'.          EL1324
00130      12  ER-0216                 PIC X(4)  VALUE '0216'.          EL1324
00131      12  ER-0284                 PIC X(4)  VALUE '0284'.          EL1324
00132      12  ER-0348                 PIC X(4)  VALUE '0348'.          EL1324
00133      12  ER-0488                 PIC X(4)  VALUE '0488'.          EL1324
00134      12  ER-0777                 PIC X(4)  VALUE '0777'.          EL1324
00135      12  ER-2370                 PIC X(4)  VALUE '2370'.          EL1324
00136      12  ER-2374                 PIC X(4)  VALUE '2374'.          EL1324
00137                                                                   EL1324
00138      EJECT                                                        EL1324
00139                                  COPY ELCINTF.                    EL1324
00140                                                                   EL1324
00141                                  COPY ELC132PI.                   EL1324
00142      EJECT                                                        EL1324
00143                                  COPY ELCEMIB.                    EL1324
00144                                                                   EL1324
00145      EJECT                                                        EL1324
00146                                  COPY ELCDATE.                    EL1324
00147                                                                   EL1324
00148      EJECT                                                        EL1324
00149                                  COPY EL1324S.                    EL1324
00150                                                                   EL1324
00151      EJECT                                                        EL1324
00152                                  COPY ELCLOGOF.                   EL1324
00153                                                                   EL1324
00154      EJECT                                                        EL1324
00155                                  COPY ELCATTR.                    EL1324
00156                                                                   EL1324
00157      EJECT                                                        EL1324
00158                                  COPY ELCAID.                     EL1324
00159                                                                   EL1324
00160  01  FILLER      REDEFINES                                        EL1324
00161      DFHAID.                                                      EL1324
00162                                                                   EL1324
00163      12  FILLER                  PIC X(8).                        EL1324
00164                                                                   EL1324
00165      12  PF-VALUES               PIC X                            EL1324
00166          OCCURS 24 TIMES.                                         EL1324
00167      EJECT                                                        EL1324
00168  LINKAGE SECTION.                                                 EL1324
00169                                                                   EL1324
00170  01  DFHCOMMAREA                 PIC X(1024).                     EL1324
00171                                                                   EL1324
00172      EJECT                                                        EL1324
00173                                  COPY ELCALPH.                    EL1324
00174                                                                   EL1324
00175      EJECT                                                        EL1324
00176  PROCEDURE DIVISION.                                              EL1324
00177                                                                   EL1324
00178      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL1324
00179      MOVE '5'                    TO DC-OPTION-CODE.               EL1324
00180      PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT.                 EL1324
00181      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL1324
00182      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL1324
00183                                                                   EL1324
00184      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL1324
00185                                                                   EL1324
00186      MOVE +2                     TO  EMI-NUMBER-OF-LINES          EL1324
00187                                      EMI-SWITCH2.                 EL1324
00188                                                                   EL1324
00189 *    NOTE ******************************************************* EL1324
00190 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL1324
00191 *         *  FROM ANOTHER MODULE.                               * EL1324
00192 *         *******************************************************.EL1324
00193                                                                   EL1324
00194      IF EIBCALEN NOT GREATER ZERO                                 EL1324
00195          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL1324
00196          GO TO 8300-SEND-TEXT.                                    EL1324
00197                                                                   EL1324
00198      EXEC CICS HANDLE CONDITION                                   EL1324
00199          PGMIDERR (9600-PGMIDERR)                                 EL1324
00200          NOTOPEN  (8800-NOTOPEN)                                  EL1324
00201          ERROR    (9990-ERROR)                                    EL1324
00202      END-EXEC.                                                    EL1324
00203                                                                   EL1324
00204      EJECT                                                        EL1324
00205  0010-MAIN-LOGIC.                                                 EL1324
00206 *    NOTE ******************************************************* EL1324
00207 *         *      IF THE TRANSACTION CODE OF THE TASK THAT       * EL1324
00208 *         *  INVOKED THIS MODULE IS NOT EX21, THIS IS THE FIRST * EL1324
00209 *         *  TIME THROUGH THIS MODULE.                          * EL1324
00210 *         *******************************************************.EL1324
00211                                                                   EL1324
00212      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL1324
00213          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL1324
00214              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL1324
00215              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL1324
00216              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL1324
00217              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL1324
00218              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL1324
00219              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL1324
00220              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL1324
00221              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM     EL1324
00222            ELSE                                                   EL1324
00223              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL1324
00224              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL1324
00225              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL1324
00226              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL1324
00227              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL1324
00228              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL1324
00229              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL1324
00230              MOVE SPACES               TO  PI-SAVED-PROGRAM-6     EL1324
00231              PERFORM 7000-BUILD-SCREEN                            EL1324
00232              MOVE +1             TO  WS-CALL-SW                   EL1324
00233        ELSE                                                       EL1324
00234          GO TO 0020-MAIN-LOGIC.                                   EL1324
00235                                                                   EL1324
00236  0015-MAIN-LOGIC.                                                 EL1324
00237 *    NOTE ******************************************************* EL1324
00238 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL1324
00239 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL1324
00240 *         *******************************************************.EL1324
00241                                                                   EL1324
00242      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.        EL1324
00243                                                                   EL1324
00244      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL1324
00245                                      PI-LINE-COUNT                EL1324
00246                                      PI-AIX-RECORD-COUNT          EL1324
00247                                      PI-BROWSE-SW                 EL1324
00248                                      PI-KEY-LENGTH                EL1324
00249                                      PI-TS-ITEM                   EL1324
00250                                      PI-END-OF-FILE               EL1324
00251                                      PI-START-SW.                 EL1324
00252                                                                   EL1324
00253      MOVE LOW-VALUES             TO  EL1324AO.                    EL1324
00254                                                                   EL1324
00255  0018-MAIN-LOGIC.                                                 EL1324
00256      MOVE SPACES                 TO  PI-CONTROL-IN-PROGRESS.      EL1324
00257                                                                   EL1324
00258      GO TO 8100-SEND-INITIAL-MAP.                                 EL1324
00259                                                                   EL1324
00260      EJECT                                                        EL1324
00261  0020-MAIN-LOGIC.                                                 EL1324
00262      IF PI-1ST-TIME-SW NOT = ZERO                                 EL1324
00263          GO TO 0015-MAIN-LOGIC.                                   EL1324
00264                                                                   EL1324
00265 *    NOTE ******************************************************* EL1324
00266 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL1324
00267 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL1324
00268 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL1324
00269 *         *******************************************************.EL1324
00270                                                                   EL1324
00271      IF EIBAID = DFHCLEAR                                         EL1324
00272          GO TO 9400-CLEAR.                                        EL1324
00273                                                                   EL1324
00274      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3                   EL1324
00275          MOVE LOW-VALUES         TO  EL1324AI                     EL1324
00276          MOVE -1                 TO  ALFPFKL                      EL1324
00277          MOVE ER-0008            TO  EMI-ERROR                    EL1324
00278          GO TO 8200-SEND-DATAONLY.                                   CL**2
00279                                                                   EL1324
00280      EXEC CICS RECEIVE                                            EL1324
00281          INTO   (EL1324AO)                                        EL1324
00282          MAPSET (WS-MAPSET-NAME)                                  EL1324
00283          MAP    (WS-MAP-NAME)                                     EL1324
00284      END-EXEC.                                                    EL1324
00285                                                                   EL1324
00286      IF ALFPFKL GREATER THAN  ZERO                                EL1324
00287          IF EIBAID NOT = DFHENTER                                 EL1324
00288              MOVE ER-0004        TO  EMI-ERROR                    EL1324
00289              MOVE AL-UNBOF       TO  ALFPFKA                      EL1324
00290              MOVE -1             TO  ALFPFKL                      EL1324
00291              GO TO  8200-SEND-DATAONLY                            EL1324
00292          ELSE                                                     EL1324
00293              IF ALFPFKO  GREATER ZERO AND LESS '25'               EL1324
00294                  MOVE PF-VALUES (ALFPFKI) TO  EIBAID              EL1324
00295              ELSE                                                 EL1324
00296                  MOVE ER-0029        TO  EMI-ERROR                EL1324
00297                  MOVE AL-UNBOF       TO  ALFPFKA                  EL1324
00298                  MOVE -1             TO  ALFPFKL                  EL1324
00299                  GO TO  8200-SEND-DATAONLY.                       EL1324
00300                                                                   EL1324
00301      IF EIBAID = DFHPF12                                          EL1324
00302          MOVE XCTL-010           TO  THIS-PGM                     EL1324
00303          GO TO 9300-XCTL.                                         EL1324
00304                                                                   EL1324
00305      IF EIBAID = DFHPF23                                          EL1324
00306          GO TO 9000-RETURN-CICS.                                  EL1324
00307                                                                   EL1324
00308      IF EIBAID = DFHPF24                                          EL1324
00309          MOVE XCTL-126           TO  THIS-PGM                     EL1324
00310          GO TO 9300-XCTL.                                         EL1324
00311                                                                   EL1324
00312      IF EIBAID = DFHENTER                                         EL1324
00313          GO TO 0025-MAIN-LOGIC.                                   EL1324
00314                                                                   EL1324
00315      IF EIBAID NOT = DFHENTER                                     EL1324
00316          MOVE -1                 TO  ALFPFKL                      EL1324
00317          MOVE ER-0008            TO  EMI-ERROR                    EL1324
00318          GO TO 8200-SEND-DATAONLY.                                EL1324
00319  EJECT                                                            EL1324
00320  0025-MAIN-LOGIC.                                                 EL1324
00321                                                                   EL1324
00322      MOVE  SPACES                    TO  PI-ALPH-CLAIM-KEY        EL1324
00323                                          PI-ALPH-ADMIN-KEY.       EL1324
00324                                                                   EL1324
00325      MOVE  PI-COMPANY-CD             TO  PI-ALPH-CO-CD            EL1324
00326                                          PI-ADM-COMP-CD           EL1324
00327                                          PI-CLM-COMP-CD.          EL1324
00328                                                                   EL1324
00329      MOVE  XCTL-1325                 TO  THIS-PGM.                EL1324
00330                                                                   EL1324
00331      IF PI-PROCESSOR-ID = 'LGXX'                                  EL1324
00332          NEXT SENTENCE                                            EL1324
00333      ELSE                                                         EL1324
00334          EXEC CICS READQ TS                                       EL1324
00335              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL1324
00336              INTO    (SECURITY-CONTROL)                           EL1324
00337              LENGTH  (SC-COMM-LENGTH)                             EL1324
00338              ITEM    (SC-ITEM)                                    EL1324
00339          END-EXEC                                                 EL1324
00340          MOVE SC-CLAIMS-DISPLAY (21)   TO  PI-DISPLAY-CAP         EL1324
00341          MOVE SC-CLAIMS-UPDATE  (21)   TO  PI-MODIFY-CAP          EL1324
00342          IF NOT DISPLAY-CAP                                       EL1324
00343              MOVE 'READ'               TO  SM-READ                EL1324
00344              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT          CL**2
00345              MOVE ER-0070              TO  EMI-ERROR              EL1324
00346              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL1324
00347              GO TO 8100-SEND-INITIAL-MAP.                         EL1324
00348                                                                   EL1324
00349 ******************************************************************EL1324
00350 *           O P T I O N  1  P R O C E S S I N G                  *EL1324
00351 ******************************************************************EL1324
00352                                                                   EL1324
00353      IF PI-SESSION-IN-PROGRESS = '1'                              EL1324
00354          NEXT SENTENCE                                            EL1324
00355      ELSE                                                         EL1324
00356         IF PI-SESSION-IN-PROGRESS  =  '2'                         EL1324
00357            MOVE -1              TO ALFCARRL                       EL1324
00358            GO TO 0200-MAIN-LOGIC.                                 EL1324
00359                                                                   EL1324
00360      IF (ALLNAMEL  GREATER THAN ZERO)  OR                         EL1324
00361         (ALFNAMEL  GREATER THAN ZERO)  OR                         EL1324
00362         (ALINTALL  GREATER THAN ZERO)                             EL1324
00363          NEXT SENTENCE                                            EL1324
00364      ELSE                                                         EL1324
00365          GO TO 0100-MAIN-LOGIC.                                   EL1324
00366                                                                   EL1324
00367      IF (ALFNAMEL GREATER THAN +0    OR                           EL1324
00368          ALINTALL GREATER THAN +0)   AND                          EL1324
00369          ALLNAMEL NOT GREATER  ZERO                               EL1324
00370           MOVE ER-0488          TO  EMI-ERROR                     EL1324
00371           MOVE -1               TO  ALLNAMEL                      EL1324
00372           GO TO 8200-SEND-DATAONLY.                               EL1324
00373                                                                   EL1324
00374      IF ALLNAMEL GREATER THAN ZEROS                               EL1324
00375         IF ALLNAMEI  IS EQUAL TO  SPACES OR LOW-VALUES            EL1324
00376            MOVE ER-0284          TO  EMI-ERROR                    EL1324
00377            MOVE -1               TO  ALLNAMEL                     EL1324
00378            GO TO 8200-SEND-DATAONLY.                              EL1324
00379                                                                   EL1324
00380      MOVE ALPHA-INDEX-DSID       TO  PI-DSID.                     EL1324
00381      MOVE '1'                    TO  PI-OPTION.                   EL1324
00382      MOVE PI-COMPANY-CD          TO  PI-ALPH-CO-CD.               EL1324
00383      MOVE 'C'                    TO  PI-ALPH-SOURCE.              EL1324
00384                                                                   EL1324
00385      MOVE +2                     TO  PI-KEY-LENGTH.               EL1324
00386                                                                   EL1324
00387      IF ALLNAMEL GREATER ZERO                                     EL1324
00388         MOVE ALLNAMEI            TO  PI-ALPH-LAST-NAME            EL1324
00389                                      WS-INPUT-FIELD               EL1324
00390         IF (ALFNAMEL  EQUAL +0)   AND                             EL1324
00391            (ALINTALL  EQUAL +0)                                   EL1324
00392               PERFORM 0030-MAIN-LOGIC THRU 0030-MAIN-LOGIC-EXIT   EL1324
00393                  VARYING INPUT-INDEX FROM ALLNAMEL BY -1          EL1324
00394                    UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE  EL1324
00395            ADD ALLNAMEL         TO  PI-KEY-LENGTH                 EL1324
00396         ELSE                                                      EL1324
00397            MOVE +17             TO  PI-KEY-LENGTH.                   CL**3
00398                                                                   EL1324
00399      IF ALFNAMEL  GREATER ZERO                                    EL1324
00400         MOVE ALFNAMEI           TO  PI-ALPH-FRST-NAME             EL1324
00401         IF ALINTALL  GREATER ZERO                                 EL1324
00402            ADD +13              TO  PI-KEY-LENGTH                 EL1324
00403            MOVE ALINTALI        TO  PI-ALPH-MID-INIT              EL1324
00404         ELSE                                                      EL1324
00405            MOVE ALFNAMEI       TO  WS-INPUT-FIELD                 EL1324
00406               PERFORM 0050-MAIN-LOGIC THRU 0050-MAIN-LOGIC-EXIT   EL1324
00407                  VARYING INPUT-INDEX FROM ALFNAMEL BY -1          EL1324
00408                    UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE  EL1324
00409            ADD ALFNAMEL           TO  PI-KEY-LENGTH.              EL1324
00410                                                                   EL1324
00411      IF ALLNAMEL GREATER ZERO                                     EL1324
00412          MOVE AL-UABON           TO  ALLNAMEA                     EL1324
00413      ELSE                                                         EL1324
00414          MOVE AL-UABOF           TO  ALLNAMEA.                    EL1324
00415                                                                   EL1324
00416      IF ALFNAMEL GREATER ZERO                                     EL1324
00417          MOVE AL-UABON           TO  ALFNAMEA                     EL1324
00418      ELSE                                                         EL1324
00419          MOVE AL-UABOF           TO  ALFNAMEA.                    EL1324
00420                                                                   EL1324
00421      IF ALINTALL GREATER ZERO                                     EL1324
00422          MOVE AL-UABON           TO  ALINTALA                     EL1324
00423      ELSE                                                         EL1324
00424          MOVE AL-UABOF           TO  ALINTALA.                    EL1324
00425                                                                   EL1324
00426      MOVE -1                     TO  ALLNAMEL.                    EL1324
00427      PERFORM 4000-READ-ALPHA-FILE.                                   CL**2
00428                                                                   EL1324
00429      MOVE +1                     TO  PI-1ST-TIME-SW.              EL1324
00430                                                                   EL1324
00431      IF PI-RETURN-TO-PROGRAM = 'EL132   '                         EL1324
00432          MOVE  XCTL-150          TO  THIS-PGM                     EL1324
00433      ELSE                                                         EL1324
00434          MOVE  XCTL-1325         TO  THIS-PGM.                    EL1324
00435                                                                   EL1324
00436      PERFORM 9300-XCTL.                                           EL1324
00437                                                                   EL1324
00438  0030-MAIN-LOGIC.                                                 EL1324
00439      SUBTRACT +1 FROM ALLNAMEL.                                   EL1324
00440                                                                   EL1324
00441  0030-MAIN-LOGIC-EXIT.                                            EL1324
00442      EXIT.                                                        EL1324
00443                                                                   EL1324
00444  0050-MAIN-LOGIC.                                                 EL1324
00445      SUBTRACT +1 FROM ALFNAMEL.                                   EL1324
00446                                                                   EL1324
00447  0050-MAIN-LOGIC-EXIT.                                            EL1324
00448      EXIT.                                                        EL1324
00449                                                                   EL1324
00450      EJECT                                                        EL1324
00451  0100-MAIN-LOGIC SECTION.                                         EL1324
00452                                                                   EL1324
00453 ******************************************************************EL1324
00454 *           O P T I O N  2  P R O C E S S I N G                  *EL1324
00455 ******************************************************************EL1324
00456                                                                   EL1324
00457      IF (ALCLMNOL  GREATER ZERO)  OR                              EL1324
00458         (ALCARNOL  GREATER ZERO)  OR                              EL1324
00459         (ALCERTL   GREATER ZERO)  OR                              EL1324
00460         (ALCRTSXL  GREATER ZERO)                                  EL1324
00461          NEXT SENTENCE                                            EL1324
00462        ELSE                                                       EL1324
00463          GO TO 0200-MAIN-LOGIC.                                   EL1324
00464                                                                   EL1324
00465      MOVE ALPHA2-INDEX-DSID      TO  PI-DSID.                     EL1324
00466      MOVE '2'                    TO  PI-OPTION.                   EL1324
00467      MOVE PI-COMPANY-CD          TO  PI-ADM-COMP-CD               EL1324
00468                                      PI-CLM-COMP-CD.              EL1324
00469      MOVE 'C'                    TO  PI-CLM-SOURCE.               EL1324
00470                                                                   EL1324
00471      IF ALCLMNOL  GREATER THAN ZERO                               EL1324
00472         IF ALCLMNOI  IS EQUAL TO  SPACES OR LOW-VALUES            EL1324
00473             MOVE ER-0284         TO  EMI-ERROR                    EL1324
00474             MOVE -1              TO  ALCLMNOL                     EL1324
00475             GO TO 8200-SEND-DATAONLY.                             EL1324
00476                                                                   EL1324
00477 ******************************************************************EL1324
00478 *              SECURITY CHECK FOR CARRIER                        *EL1324
00479 *                    04/02/84                                    *EL1324
00480 ******************************************************************EL1324
00481                                                                   EL1324
00482      IF  PI-NO-CARRIER-SECURITY                                   EL1324
00483          GO TO 0128-BUILD-CARRIER-NO.                             EL1324
00484                                                                   EL1324
00485      IF ALCARNOL GREATER ZERO                                     EL1324
00486         IF  ALCARNOI = PI-CARRIER-SECURITY                        EL1324
00487             GO TO 0128-BUILD-CARRIER-NO.                          EL1324
00488                                                                   EL1324
00489      MOVE ER-2370                TO  EMI-ERROR.                   EL1324
00490      MOVE -1                     TO  ALCARNOL.                    EL1324
00491      MOVE AL-UABON               TO  ALCARNOA.                    EL1324
00492      GO TO 8200-SEND-DATAONLY.                                    EL1324
00493                                                                   EL1324
00494  0128-BUILD-CARRIER-NO.                                           EL1324
00495      IF ALCARNOL GREATER ZERO                                     EL1324
00496          MOVE ALCARNOI           TO  PI-CLM-CARRIER               EL1324
00497                                      PI-CARRIER                   EL1324
00498          MOVE +3                 TO  PI-KEY-LENGTH                EL1324
00499      ELSE                                                         EL1324
00500          MOVE ER-0194            TO  EMI-ERROR                    EL1324
00501          PERFORM 9900-ERROR-FORMAT                                EL1324
00502          MOVE -1                 TO  ALCARNOL.                    EL1324
00503                                                                   EL1324
00504      IF ALCLMNOL  GREATER ZERO                                    EL1324
00505          MOVE ALCLMNOI           TO  PI-CLM-CLAIM-NO              EL1324
00506                                      PI-CLAIM-NO                  EL1324
00507                                      WS-INPUT-FIELD               EL1324
00508          PERFORM 0130-MAIN-LOGIC THRU 0130-MAIN-LOGIC-EXIT        EL1324
00509              VARYING INPUT-INDEX FROM ALCLMNOL  BY -1             EL1324
00510                  UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE    EL1324
00511          ADD +3  ALCLMNOL GIVING PI-KEY-LENGTH                    EL1324
00512        ELSE                                                       EL1324
00513          MOVE ER-0209           TO  EMI-ERROR                     EL1324
00514          PERFORM 9900-ERROR-FORMAT                                EL1324
00515          MOVE -1                TO  ALCLMNOL.                     EL1324
00516                                                                   EL1324
00517      IF ALCERTL  GREATER ZERO                                     EL1324
00518          MOVE ALCERTI            TO  PI-CLM-CERT-PRM              EL1324
00519                                      WS-INPUT-FIELD               EL1324
00520          MOVE +20                TO  PI-KEY-LENGTH.               EL1324
00521                                                                   EL1324
00522      IF ALCRTSXL GREATER ZERO                                     EL1324
00523          MOVE +21                TO  PI-KEY-LENGTH                EL1324
00524          MOVE ALCRTSXI           TO  PI-CLM-CERT-SFX.             EL1324
00525                                                                   EL1324
00526      MOVE PI-CLM-CERT-NUMBER     TO  PI-CERT-NO.                  EL1324
00527                                                                   EL1324
00528      IF EMI-FATAL-CTR GREATER ZERO                                EL1324
00529          GO TO 8200-SEND-DATAONLY.                                EL1324
00530                                                                   EL1324
00531      IF ALCLMNOL  GREATER ZERO                                    EL1324
00532          MOVE AL-UABON           TO  ALCLMNOA                     EL1324
00533        ELSE                                                       EL1324
00534          MOVE AL-UABOF           TO  ALCLMNOA.                    EL1324
00535                                                                   EL1324
00536      IF ALCARNOL GREATER ZERO                                     EL1324
00537          MOVE AL-UABON           TO  ALCARNOA                     EL1324
00538        ELSE                                                       EL1324
00539          MOVE AL-UABOF           TO  ALCARNOA.                    EL1324
00540                                                                   EL1324
00541      IF ALCERTL  GREATER ZERO                                     EL1324
00542          MOVE AL-UABON           TO  ALCERTA                      EL1324
00543        ELSE                                                       EL1324
00544          MOVE AL-UABOF           TO  ALCERTA.                     EL1324
00545                                                                   EL1324
00546      IF ALCRTSXL GREATER ZERO                                     EL1324
00547          MOVE AL-UABON           TO  ALCRTSXA                     EL1324
00548        ELSE                                                       EL1324
00549          MOVE AL-UABOF           TO  ALCRTSXA.                    EL1324
00550                                                                   EL1324
00551      MOVE -1                     TO  ALCLMNOL.                    EL1324
00552      PERFORM 4000-READ-ALPHA-FILE.                                EL1324
00553                                                                   EL1324
00554  0130-MAIN-LOGIC.                                                 EL1324
00555      SUBTRACT +1 FROM ALCLMNOL.                                   EL1324
00556                                                                   EL1324
00557  0130-MAIN-LOGIC-EXIT.                                            EL1324
00558      EXIT.                                                        EL1324
00559                                                                   EL1324
00560      EJECT                                                        EL1324
00561  0200-MAIN-LOGIC SECTION.                                         EL1324
00562 ******************************************************************EL1324
00563 *           O P T I O N  3  P R O C E S S I N G                  *EL1324
00564 ******************************************************************EL1324
00565                                                                   EL1324
00566      IF (ALFCARRL  GREATER ZERO)  OR                              EL1324
00567         (ALFGRPL   GREATER ZERO)  OR                              EL1324
00568         (ALFSTL    GREATER ZERO)  OR                              EL1324
00569         (ALFACCTL  GREATER ZERO)  OR                              EL1324
00570         (ALFEFDTL  GREATER ZERO)  OR                              EL1324
00571         (ALFCERTL  GREATER ZERO)                                  EL1324
00572          NEXT SENTENCE                                            EL1324
00573        ELSE                                                       EL1324
00574          GO TO 0300-MAIN-LOGIC.                                   EL1324
00575                                                                   EL1324
00576      MOVE '3'                    TO  PI-OPTION.                   EL1324
00577      MOVE PI-COMPANY-CD          TO  PI-ADM-COMP-CD.              EL1324
00578      MOVE 'A'                    TO  PI-ADM-SOURCE.               EL1324
00579                                                                   EL1324
00580 ******************************************************************EL1324
00581 *              SECURITY CHECK FOR CARRIER                        *EL1324
00582 *                    04/02/84                                    *EL1324
00583 ******************************************************************EL1324
00584                                                                   EL1324
00585      IF  PI-NO-CARRIER-SECURITY                                   EL1324
00586          GO TO 0200-BUILD-ADMIN-KEY.                              EL1324
00587                                                                   EL1324
00588      IF ALFCARRL GREATER ZERO                                     EL1324
00589         IF  ALFCARRI = PI-CARRIER-SECURITY                        EL1324
00590             GO TO 0200-BUILD-ADMIN-KEY.                           EL1324
00591                                                                   EL1324
00592      MOVE ER-2370                TO  EMI-ERROR.                   EL1324
00593      MOVE -1                     TO  ALFCARRL.                    EL1324
00594      MOVE AL-UABON               TO  ALFCARRA.                    EL1324
00595      GO TO 8200-SEND-DATAONLY.                                    EL1324
00596                                                                   EL1324
00597  0200-BUILD-ADMIN-KEY.                                            EL1324
00598      IF ALFCARRL GREATER ZERO                                     EL1324
00599          MOVE ALFCARRI           TO  PI-ADM-CARRIER               EL1324
00600                                      PI-CARRIER                   EL1324
00601          MOVE +1                 TO  PI-KEY-LENGTH                EL1324
00602      ELSE                                                         EL1324
00603          MOVE ER-0194            TO  EMI-ERROR                    EL1324
00604          PERFORM  9900-ERROR-FORMAT THRU 9900-EXIT                EL1324
00605          MOVE  -1                TO  ALFCARRL.                    EL1324
00606                                                                   EL1324
00607      IF ALFGRPL  GREATER THAN  +0                                 EL1324
00608         MOVE ALFGRPI             TO  PI-ADM-GROUPING              EL1324
00609                                      PI-GROUPING                  EL1324
00610         ADD  +6                  TO  PI-KEY-LENGTH                EL1324
00611      ELSE                                                         EL1324
00612         MOVE ER-0195             TO  EMI-ERROR                    EL1324
00613         PERFORM  9900-ERROR-FORMAT THRU 9900-EXIT                 EL1324
00614         MOVE  -1                 TO  ALFGRPL.                     EL1324
00615                                                                   EL1324
00616      IF ALFSTL  GREATER THAN  +0                                  EL1324
00617         MOVE ALFSTI              TO  PI-ADM-STATE                 EL1324
00618                                      PI-STATE                     EL1324
00619         ADD  +2                  TO  PI-KEY-LENGTH                EL1324
00620      ELSE                                                         EL1324
00621         MOVE ER-0196             TO  EMI-ERROR                    EL1324
00622         PERFORM  9900-ERROR-FORMAT THRU 9900-EXIT                 EL1324
00623         MOVE  -1                 TO  ALFSTL.                      EL1324
00624                                                                   EL1324
00625      IF ALFACCTL  GREATER THAN  +0                                EL1324
00626         MOVE ALFACCTI            TO  PI-ADM-PRODUCER              EL1324
00627                                      PI-PRODUCER                  EL1324
00628         ADD +10                  TO  PI-KEY-LENGTH                EL1324
00629      ELSE                                                         EL1324
00630         MOVE ER-0197             TO  EMI-ERROR                    EL1324
00631         PERFORM  9900-ERROR-FORMAT THRU 9900-EXIT                 EL1324
00632         MOVE  -1                 TO  ALFACCTL.                    EL1324
00633                                                                   EL1324
00634      IF ALFEFDTL  GREATER THAN  +0                                EL1324
00635         MOVE  ALFEFDTI           TO  DC-GREG-DATE-1-EDIT          EL1324
00636         MOVE  '2'                TO  DC-OPTION-CODE               EL1324
00637         PERFORM 8500-DATE-CONVERSION  THRU 8500-EXIT              EL1324
00638         IF DATE-CONVERSION-ERROR                                  EL1324
00639            MOVE  ER-0348         TO  EMI-ERROR                    EL1324
00640            MOVE  -1              TO  ALFEFDTL                     EL1324
00641            MOVE AL-UABON         TO  ALFEFDTA                     EL1324
00642            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1324
00643         ELSE                                                      EL1324
00644            MOVE  DC-BIN-DATE-1   TO  PI-ADM-CERT-EFF-DT           EL1324
00645                                      PI-CERT-EFF-DT               EL1324
00646            ADD +08               TO  PI-KEY-LENGTH                EL1324
00647      ELSE                                                         EL1324
00648         MOVE ER-0216             TO  EMI-ERROR                    EL1324
00649         PERFORM  9900-ERROR-FORMAT THRU 9900-EXIT                 EL1324
00650         MOVE  -1                 TO  ALFEFDTL.                    EL1324
00651                                                                   EL1324
00652      IF ALFCERTL  GREATER THAN  +0                                EL1324
00653          MOVE ALFCERTI           TO  PI-ADM-CERT-PRM              EL1324
00654          ADD  +10                TO  PI-KEY-LENGTH                EL1324
00655      ELSE                                                         EL1324
00656         MOVE ER-0203             TO  EMI-ERROR                    EL1324
00657         PERFORM  9900-ERROR-FORMAT THRU 9900-EXIT                 EL1324
00658         MOVE  -1                 TO  ALFEFDTL.                    EL1324
00659                                                                   EL1324
00660      IF ALFCRTXL GREATER ZERO                                     EL1324
00661          ADD  +01                TO  PI-KEY-LENGTH                EL1324
00662          MOVE ALFCRTXI           TO  PI-ADM-CERT-SFX.             EL1324
00663                                                                   EL1324
00664      MOVE PI-CLM-CERT-NUMBER     TO  PI-CERT-NO.                  EL1324
00665                                                                   EL1324
00666      IF ALFCARRL  GREATER THAN  +0                                EL1324
00667          MOVE AL-UABON           TO  ALFCARRA                     EL1324
00668      ELSE                                                         EL1324
00669          MOVE AL-UABOF           TO  ALFCARRA.                    EL1324
00670                                                                   EL1324
00671      IF ALFGRPL   GREATER THAN  +0                                EL1324
00672          MOVE AL-UABON           TO  ALFGRPA                      EL1324
00673      ELSE                                                         EL1324
00674          MOVE AL-UABOF           TO  ALFGRPA.                     EL1324
00675                                                                   EL1324
00676      IF ALFSTL    GREATER THAN  +0                                EL1324
00677          MOVE AL-UABON           TO  ALFSTA                       EL1324
00678      ELSE                                                         EL1324
00679          MOVE AL-UABOF           TO  ALFSTA.                      EL1324
00680                                                                   EL1324
00681      IF ALFACCTL  GREATER THAN  +0                                EL1324
00682          MOVE AL-UABON           TO  ALFACCTA                     EL1324
00683      ELSE                                                         EL1324
00684          MOVE AL-UABOF           TO  ALFACCTA.                    EL1324
00685                                                                   EL1324
00686      IF ALFEFDTL  GREATER THAN  +0                                EL1324
00687          MOVE AL-UABON           TO  ALFEFDTA                     EL1324
00688      ELSE                                                         EL1324
00689          MOVE AL-UABOF           TO  ALFEFDTA.                    EL1324
00690                                                                   EL1324
00691      IF ALFCERTL  GREATER THAN  +0                                EL1324
00692          MOVE AL-UABON           TO  ALFCERTA                     EL1324
00693      ELSE                                                         EL1324
00694          MOVE AL-UABOF           TO  ALFCERTA.                    EL1324
00695                                                                   EL1324
00696      IF ALFCRTXL  GREATER THAN  +0                                EL1324
00697          MOVE AL-UABON           TO  ALFCRTXA                     EL1324
00698      ELSE                                                         EL1324
00699          MOVE AL-UABOF           TO  ALFCRTXA.                    EL1324
00700                                                                   EL1324
00701      MOVE -1                     TO  ALFCARRL.                    EL1324
00702      MOVE ALPHA2-INDEX-DSID      TO  PI-DSID.                     EL1324
00703      PERFORM 4000-READ-ALPHA-FILE.                                EL1324
00704                                                                   EL1324
00705  0220-MAIN-LOGIC-EXIT.                                            EL1324
00706      EXIT.                                                        EL1324
00707                                                                   EL1324
00708      EJECT                                                        EL1324
00709  0300-MAIN-LOGIC SECTION.                                         EL1324
00710 ******************************************************************EL1324
00711 *           O P T I O N  4  P R O C E S S I N G                  *EL1324
00712 ******************************************************************EL1324
00713                                                                   EL1324
00714      IF (ALFLNMEL  GREATER ZERO)  OR                              EL1324
00715         (ALFFNMEL  GREATER ZERO)  OR                              EL1324
00716         (ALFINTLL  GREATER ZERO)                                  EL1324
00717          NEXT SENTENCE                                            EL1324
00718        ELSE                                                       EL1324
00719          GO TO 0400-MAIN-LOGIC.                                   EL1324
00720                                                                   EL1324
00721      IF (ALFFNMEL GREATER THAN ZERO  OR                           EL1324
00722          ALFINTLL GREATER THAN ZERO) AND                          EL1324
00723          ALFLNMEL NOT GREATER  ZERO                               EL1324
00724          MOVE ER-0488            TO  EMI-ERROR                    EL1324
00725          MOVE -1                 TO  ALFLNMEL                     EL1324
00726          GO TO 8200-SEND-DATAONLY.                                EL1324
00727                                                                   EL1324
00728      MOVE ALPHA-INDEX-DSID       TO  PI-DSID.                     EL1324
00729      MOVE '4'                    TO  PI-OPTION.                   EL1324
00730      MOVE PI-COMPANY-CD          TO  PI-ALPH-CO-CD.               EL1324
00731      MOVE 'A'                    TO  PI-ALPH-SOURCE.              EL1324
00732                                                                   EL1324
00733      MOVE +1                     TO  PI-KEY-LENGTH.               EL1324
00734                                                                   EL1324
00735      IF ALFLNMEL GREATER ZERO                                     EL1324
00736         MOVE ALFLNMEI            TO  PI-ALPH-LAST-NAME            EL1324
00737                                      WS-INPUT-FIELD               EL1324
00738         IF ALFFNMEL  EQUAL +0                                     EL1324
00739                   AND                                             EL1324
00740            ALFINTLL  EQUAL +0                                     EL1324
00741            PERFORM 0320-MAIN-LOGIC THRU 0320-MAIN-LOGIC-EXIT      EL1324
00742              VARYING INPUT-INDEX FROM ALFLNMEL BY -1              EL1324
00743              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE        EL1324
00744            ADD ALFLNMEL  TO  PI-KEY-LENGTH                        EL1324
00745         ELSE                                                      EL1324
00746            MOVE +15     TO  PI-KEY-LENGTH.                        EL1324
00747                                                                   EL1324
00748      IF ALFFNMEL  GREATER ZERO                                    EL1324
00749         MOVE ALFFNMEI           TO  PI-ALPH-FRST-NAME             EL1324
00750         IF ALFINTLL  GREATER ZERO                                 EL1324
00751            ADD +13              TO  PI-KEY-LENGTH                 EL1324
00752            MOVE ALFINTLI        TO  PI-ALPH-MID-INIT              EL1324
00753         ELSE                                                      EL1324
00754            MOVE ALFFNMEI       TO  WS-INPUT-FIELD                 EL1324
00755            PERFORM 0325-MAIN-LOGIC THRU 0325-MAIN-LOGIC-EXIT      EL1324
00756              VARYING INPUT-INDEX FROM ALFFNMEL BY -1              EL1324
00757              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE        EL1324
00758            ADD ALFFNMEL           TO  PI-KEY-LENGTH.              EL1324
00759                                                                   EL1324
00760      IF ALFLNMEL GREATER ZERO                                     EL1324
00761          MOVE AL-UABON           TO  ALFLNMEA                     EL1324
00762      ELSE                                                         EL1324
00763          MOVE AL-UABOF           TO  ALFLNMEA.                    EL1324
00764                                                                   EL1324
00765      IF ALFFNMEL GREATER ZERO                                     EL1324
00766          MOVE AL-UABON           TO  ALFFNMEA                     EL1324
00767      ELSE                                                         EL1324
00768          MOVE AL-UABOF           TO  ALFFNMEA.                    EL1324
00769                                                                   EL1324
00770      IF ALFINTLL GREATER ZERO                                     EL1324
00771          MOVE AL-UABON           TO  ALFINTLA                     EL1324
00772      ELSE                                                         EL1324
00773          MOVE AL-UABOF           TO  ALFINTLA.                    EL1324
00774                                                                   EL1324
00775      MOVE -1                     TO  ALFLNMEL.                    EL1324
00776      PERFORM 4000-READ-ALPHA-FILE.                                EL1324
00777                                                                   EL1324
00778  0320-MAIN-LOGIC.                                                 EL1324
00779      SUBTRACT +1 FROM ALFLNMEL.                                   EL1324
00780                                                                   EL1324
00781  0320-MAIN-LOGIC-EXIT.                                            EL1324
00782      EXIT.                                                        EL1324
00783                                                                   EL1324
00784  0325-MAIN-LOGIC.                                                 EL1324
00785      SUBTRACT +1 FROM ALFFNMEL.                                   EL1324
00786                                                                   EL1324
00787  0325-MAIN-LOGIC-EXIT.                                            EL1324
00788      EXIT.                                                        EL1324
00789                                                                   EL1324
00790      EJECT                                                        EL1324
00791  0400-MAIN-LOGIC.                                                 EL1324
00792                                                                   EL1324
00793      MOVE ZERO                   TO  PI-OPTION.                   EL1324
00794      MOVE +1                     TO  PI-KEY-LENGTH.               EL1324
00795      MOVE ALPHA-INDEX-DSID       TO  PI-DSID.                     EL1324
00796      MOVE ZERO                   TO  PI-OPTION.                   EL1324
00797      MOVE -1                     TO  ALLNAMEL.                    EL1324
00798      PERFORM 4000-READ-ALPHA-FILE.                                EL1324
00799                                                                   EL1324
00800      EJECT                                                        EL1324
00801  4000-READ-ALPHA-FILE SECTION.                                    EL1324
00802                                                                   EL1324
00803      EXEC CICS HANDLE CONDITION                                   EL1324
00804          DUPKEY (9300-XCTL)                                       EL1324
00805          NOTFND (4080-NOTFND)                                     EL1324
00806      END-EXEC.                                                    EL1324
00807                                                                   EL1324
00808      IF (PI-OPTION  =  ZERO) OR                                   EL1324
00809         (PI-OPTION  =  '1')  OR                                   EL1324
00810         (PI-OPTION  =  '4')                                       EL1324
00811           NEXT SENTENCE                                           EL1324
00812      ELSE                                                         EL1324
00813          GO TO 4010-READ-ALPHA-FILE.                              EL1324
00814                                                                   EL1324
00815      IF (PI-DSID = ALPHA-INDEX-DSID)  AND                         EL1324
00816         (PI-KEY-LENGTH  LESS THAN +31)                            EL1324
00817                      OR                                           EL1324
00818         (PI-DSID = ALPHA-INDEX-DSID) AND                          EL1324
00819         (PI-KEY-LENGTH  LESS THAN +31)                            EL1324
00820           MOVE +1             TO  PI-START-SW                     EL1324
00821           EXEC CICS READ                                          EL1324
00822               DATASET   (PI-DSID)                                 EL1324
00823               RIDFLD    (PI-ALPH-CLAIM-KEY)                       EL1324
00824               SET       (ADDRESS OF ALPHA-INDEX)                     CL**2
00825               GENERIC   EQUAL                                     EL1324
00826               KEYLENGTH (PI-KEY-LENGTH)                           EL1324
00827           END-EXEC                                                EL1324
00828      ELSE                                                         EL1324
00829           MOVE ZERO           TO  PI-START-SW                     EL1324
00830           EXEC CICS READ                                          EL1324
00831               DATASET   (PI-DSID)                                 EL1324
00832               RIDFLD    (PI-ALPH-CLAIM-KEY)                       EL1324
00833               SET       (ADDRESS OF ALPHA-INDEX)                     CL**2
00834           END-EXEC.                                               EL1324
00835                                                                   EL1324
00836      GO TO 9300-XCTL.                                             EL1324
00837                                                                   EL1324
00838  4010-READ-ALPHA-FILE.                                            EL1324
00839                                                                   EL1324
00840      IF (PI-DSID = ALPHA2-INDEX-DSID) AND                         EL1324
00841         (PI-KEY-LENGTH  LESS THAN +22)                            EL1324
00842                     OR                                            EL1324
00843         (PI-KEY-LENGTH  LESS THAN +41)                            EL1324
00844           MOVE +1             TO  PI-START-SW                     EL1324
00845           EXEC CICS READ                                          EL1324
00846               DATASET   (PI-DSID)                                 EL1324
00847               RIDFLD    (PI-ALPH-ADMIN-KEY)                       EL1324
00848               SET       (ADDRESS OF ALPHA-INDEX)                     CL**2
00849               GENERIC   EQUAL                                     EL1324
00850               KEYLENGTH (PI-KEY-LENGTH)                           EL1324
00851           END-EXEC                                                EL1324
00852      ELSE                                                         EL1324
00853           MOVE ZERO           TO  PI-START-SW                     EL1324
00854           EXEC CICS READ                                          EL1324
00855               DATASET   (PI-DSID)                                 EL1324
00856               RIDFLD    (PI-ALPH-ADMIN-KEY)                       EL1324
00857               SET       (ADDRESS OF ALPHA-INDEX)                     CL**2
00858           END-EXEC.                                               EL1324
00859                                                                   EL1324
00860      GO TO 9300-XCTL.                                             EL1324
00861                                                                   EL1324
00862  4000-EXIT.                                                       EL1324
00863      EXIT.                                                        EL1324
00864  4080-NOTFND.                                                     EL1324
00865      MOVE ER-0284                   TO  EMI-ERROR.                EL1324
00866                                                                   EL1324
00867      IF PI-RETURN-TO-PROGRAM = 'EL127   '                         EL1324
00868        AND WS-CALL-SW = ZERO                                      EL1324
00869          GO TO 4090-EXIT.                                         EL1324
00870                                                                   EL1324
00871      GO TO 8200-SEND-DATAONLY.                                    EL1324
00872                                                                   EL1324
00873  4090-EXIT.                                                       EL1324
00874      EXIT.                                                        EL1324
00875                                                                   EL1324
00876      EJECT                                                        EL1324
00877  7000-BUILD-SCREEN     SECTION.                                   EL1324
00878 ******************************************************************EL1324
00879 *          REBUILD ORIGNAL SCREEN AND ERROR MESSAGE IF EL1322    *EL1324
00880 *          DID NOT FIND ANY CLAIM RECORDS DURING BROWSE OF FILE. *EL1324
00881 ******************************************************************EL1324
00882      IF EIBTRNID = WS-TRANS-ID                                    EL1324
00883          NEXT SENTENCE                                            EL1324
00884        ELSE                                                       EL1324
00885          GO TO 7099-EXIT.                                         EL1324
00886                                                                   EL1324
00887      IF  PI-BROWSE-SW = +9                                        EL1324
00888          NEXT SENTENCE                                            EL1324
00889         ELSE                                                      EL1324
00890          GO TO 7099-EXIT.                                         EL1324
00891                                                                   EL1324
00892      MOVE LOW-VALUES             TO  EL1324AO.                    EL1324
00893                                                                   EL1324
00894      MOVE ER-2374                TO  EMI-ERROR.                   EL1324
00895                                                                   EL1324
00896      IF OPTION-ONE-SELECTED                                       EL1324
00897         GO TO 7099-EXIT.                                          EL1324
00898                                                                   EL1324
00899      IF OPTION-TWO-SELECTED                                       EL1324
00900         NEXT SENTENCE                                             EL1324
00901      ELSE                                                         EL1324
00902         GO TO 7030-OPTION-THREE.                                  EL1324
00903                                                                   EL1324
00904  7020-OPTION-TWO.                                                 EL1324
00905                                                                   EL1324
00906      IF  PI-CLM-CLAIM-NO   GREATER SPACES                         EL1324
00907          MOVE PI-CLM-CLAIM-NO        TO ALCLMNOO                  EL1324
00908          MOVE AL-UANON               TO ALCLMNOA.                 EL1324
00909                                                                   EL1324
00910      IF  PI-CLM-CARRIER    GREATER SPACES                         EL1324
00911          MOVE PI-CLM-CARRIER         TO ALCARNOO                  EL1324
00912          MOVE AL-UANON               TO ALCARNOA.                 EL1324
00913                                                                   EL1324
00914      IF  PI-CLM-CERT-PRM   GREATER SPACES                         EL1324
00915          MOVE PI-CLM-CERT-PRM        TO ALCERTO                   EL1324
00916          MOVE AL-UANON               TO ALCERTA.                  EL1324
00917                                                                   EL1324
00918      IF  PI-CLM-CERT-SFX   GREATER SPACES                         EL1324
00919          MOVE PI-CLM-CERT-SFX        TO ALCRTSXO                  EL1324
00920          MOVE AL-UANON               TO ALCRTSXA.                 EL1324
00921                                                                   EL1324
00922      GO TO   7090-INITIALIZE-WORK-AREAS.                          EL1324
00923                                                                   EL1324
00924  7030-OPTION-THREE.                                               EL1324
00925                                                                   EL1324
00926      IF OPTION-THREE-SELECTED                                     EL1324
00927         NEXT SENTENCE                                             EL1324
00928      ELSE                                                         EL1324
00929         GO TO 7040-OPTION-FOUR.                                   EL1324
00930                                                                   EL1324
00931      MOVE -1                          TO ALFCARRL.                EL1324
00932                                                                   EL1324
00933      IF PI-ADM-CARRIER   GREATER SPACES                           EL1324
00934         MOVE PI-ADM-CARRIER           TO ALFCARRO                 EL1324
00935         MOVE AL-UANON                 TO ALFCARRA.                EL1324
00936                                                                   EL1324
00937      IF PI-ADM-GROUPING  GREATER SPACES                           EL1324
00938         MOVE PI-ADM-GROUPING          TO ALFGRPO                  EL1324
00939         MOVE AL-UANON                 TO ALFGRPA.                 EL1324
00940                                                                   EL1324
00941      IF PI-ADM-STATE     GREATER SPACES                           EL1324
00942         MOVE PI-ADM-STATE             TO ALFSTO                   EL1324
00943         MOVE AL-UANON                 TO ALFSTA.                  EL1324
00944                                                                   EL1324
00945      IF PI-ADM-PRODUCER  GREATER SPACES                           EL1324
00946         MOVE PI-ADM-PRODUCER          TO ALFACCTO                 EL1324
00947         MOVE AL-UANON                 TO ALFACCTA.                EL1324
00948                                                                   EL1324
00949      IF PI-ADM-CERT-EFF-DT   GREATER SPACES                       EL1324
00950         MOVE PI-ADM-CERT-EFF-DT       TO ALFEFDTO                 EL1324
00951         MOVE AL-UANON                 TO ALFEFDTA.                EL1324
00952                                                                   EL1324
00953      IF PI-ADM-CERT-NUMBER   GREATER SPACES                       EL1324
00954         MOVE PI-ADM-CERT-PRM          TO ALFCERTO                 EL1324
00955         MOVE AL-UANON                 TO ALFCERTA                 EL1324
00956         MOVE PI-ADM-CERT-SFX          TO ALFCRTXO                 EL1324
00957         MOVE AL-UANON                 TO ALFCRTXA.                EL1324
00958                                                                   EL1324
00959      GO TO   7090-INITIALIZE-WORK-AREAS.                          EL1324
00960                                                                   EL1324
00961  7040-OPTION-FOUR.                                                EL1324
00962      MOVE -1                    TO ALFLNMEL.                      EL1324
00963                                                                   EL1324
00964      IF  PI-ALPH-LAST-NAME GREATER SPACES                         EL1324
00965          MOVE PI-ALPH-LAST-NAME       TO ALFLNMEO                 EL1324
00966          MOVE AL-UANON                TO ALFLNMEA                 EL1324
00967          IF  PI-ALPH-F-INIT GREATER SPACES                        EL1324
00968              MOVE PI-ALPH-FRST-NAME   TO ALFFNMEO                 EL1324
00969              MOVE PI-ALPH-MID-INIT    TO ALFINTLO                 EL1324
00970              MOVE AL-UANON            TO ALFFNMEA                 EL1324
00971              MOVE AL-UANON            TO ALFINTLA.                EL1324
00972                                                                   EL1324
00973                                                                   EL1324
00974  7090-INITIALIZE-WORK-AREAS.                                      EL1324
00975      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.        EL1324
00976                                                                   EL1324
00977      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL1324
00978                                      PI-LINE-COUNT                EL1324
00979                                      PI-AIX-RECORD-COUNT          EL1324
00980                                      PI-BROWSE-SW                 EL1324
00981                                      PI-KEY-LENGTH                EL1324
00982                                      PI-TS-ITEM                   EL1324
00983                                      PI-END-OF-FILE               EL1324
00984                                      PI-START-SW.                 EL1324
00985                                                                   EL1324
00986      GO TO 8100-SEND-INITIAL-MAP.                                 EL1324
00987                                                                   EL1324
00988  7099-EXIT.                                                       EL1324
00989      EXIT.                                                        EL1324
00990                                                                   EL1324
00991      EJECT                                                        EL1324
00992  8100-SEND-INITIAL-MAP SECTION.                                   EL1324
00993                                                                   EL1324
00994      MOVE EIBTIME                TO  TIME-IN.                     EL1324
00995      MOVE SAVE-DATE              TO  ALDATEO.                     EL1324
00996      MOVE TIME-OUT               TO  ALTIMEO.                     EL1324
101501     MOVE PI-COMPANY-ID          TO  ALCOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00997                                                                   EL1324
00998      IF EMI-ERROR NOT = ZERO                                      EL1324
00999          PERFORM 9900-ERROR-FORMAT.                               EL1324
01000                                                                   EL1324
01001      IF PI-SESSION-IN-PROGRESS  =  '1'                            EL1324
01002          MOVE -1                 TO  ALLNAMEL                     EL1324
01003      ELSE                                                         EL1324
01004          MOVE -1                 TO  ALFCARRL.                    EL1324
01005                                                                   EL1324
01006      MOVE EMI-MESSAGE-AREA (1)   TO  ALFMSG1O.                    EL1324
01007      MOVE EMI-MESSAGE-AREA (2)   TO  ALFMSG2O.                    EL1324
01008                                                                   EL1324
101501*    IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES                   EL1324
101501*        MOVE PI-COMPANY-ID          TO  ALCOMPO                  EL1324
101501*    ELSE                                                         EL1324
101501*        MOVE SPACES                 TO  ALCOMPO.                 EL1324
01013                                                                   EL1324
01014      EXEC CICS SEND                                               EL1324
01015          FROM   (EL1324AO)                                        EL1324
01016          MAPSET (WS-MAPSET-NAME)                                  EL1324
01017          MAP    (WS-MAP-NAME)                                     EL1324
01018          CURSOR                                                   EL1324
01019          ERASE                                                    EL1324
01020      END-EXEC.                                                    EL1324
01021                                                                   EL1324
01022      GO TO 9100-RETURN-TRAN.                                      EL1324
01023                                                                   EL1324
01024  8100-EXIT.                                                       EL1324
01025      EXIT.                                                        EL1324
01026                                                                   EL1324
01027      EJECT                                                        EL1324
01028  8200-SEND-DATAONLY SECTION.                                      EL1324
01029                                                                   EL1324
01030      MOVE EIBTIME                TO  TIME-IN.                     EL1324
01031      MOVE SAVE-DATE              TO  ALDATEO.                     EL1324
01032      MOVE TIME-OUT               TO  ALTIMEO.                     EL1324
101501     MOVE PI-COMPANY-ID          TO  ALCOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01033                                                                   EL1324
01034      IF EMI-ERROR NOT = ZERO                                      EL1324
01035          PERFORM 9900-ERROR-FORMAT.                               EL1324
01036                                                                   EL1324
01037      MOVE EMI-MESSAGE-AREA (1)   TO  ALFMSG1O.                    EL1324
01038      MOVE EMI-MESSAGE-AREA (2)   TO  ALFMSG2O.                    EL1324
01039                                                                   EL1324
101501*    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                       EL1324
101501*        MOVE PI-COMPANY-ID      TO  ALCOMPO                      EL1324
101501*    ELSE                                                         EL1324
101501*        MOVE SPACES             TO  ALCOMPO.                     EL1324
01044                                                                   EL1324
01045      EXEC CICS SEND DATAONLY                                      EL1324
01046          FROM   (EL1324AO)                                        EL1324
01047          MAPSET (WS-MAPSET-NAME)                                  EL1324
01048          MAP    (WS-MAP-NAME)                                     EL1324
01049          CURSOR                                                   EL1324
01050      END-EXEC.                                                    EL1324
01051                                                                   EL1324
01052      GO TO 9100-RETURN-TRAN.                                      EL1324
01053                                                                   EL1324
01054      EJECT                                                        EL1324
01055  8300-SEND-TEXT SECTION.                                          EL1324
01056      EXEC CICS SEND TEXT                                          EL1324
01057          FROM   (LOGOFF-TEXT)                                     EL1324
01058          LENGTH (LOGOFF-LENGTH)                                   EL1324
01059          ERASE  FREEKB                                            EL1324
01060      END-EXEC.                                                    EL1324
01061                                                                   EL1324
01062      EXEC CICS RETURN                                             EL1324
01063      END-EXEC.                                                    EL1324
01064                                                                   EL1324
01065  8300-EXIT.                                                       EL1324
01066      EXIT.                                                        EL1324
01067                                                                   EL1324
01068  8500-DATE-CONVERSION SECTION.                                    EL1324
01069      EXEC CICS LINK                                               EL1324
01070          PROGRAM  ('ELDATCV')                                     EL1324
01071          COMMAREA (DATE-CONVERSION-DATA)                          EL1324
01072          LENGTH   (DC-COMM-LENGTH)                                EL1324
01073      END-EXEC.                                                    EL1324
01074                                                                   EL1324
01075  8500-EXIT.                                                       EL1324
01076      EXIT.                                                        EL1324
01077                                                                   EL1324
01078      EJECT                                                        EL1324
01079  8800-NOTOPEN SECTION.                                            EL1324
01080                                                                   EL1324
01081      MOVE ER-0777                TO  EMI-ERROR                    EL1324
01082      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                   EL1324
01083      GO TO  8200-SEND-DATAONLY.                                   EL1324
01084                                                                   EL1324
01085                                                                   EL1324
01086  9000-RETURN-CICS SECTION.                                        EL1324
01087      MOVE XCTL-005               TO  THIS-PGM.                    EL1324
01088      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL1324
01089      PERFORM 9300-XCTL.                                           EL1324
01090                                                                   EL1324
01091  9000-EXIT.                                                       EL1324
01092      EXIT.                                                        EL1324
01093                                                                   EL1324
01094  9100-RETURN-TRAN SECTION.                                        EL1324
01095      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL1324
01096      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL1324
01097                                                                   EL1324
01098      EXEC CICS RETURN                                             EL1324
01099          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1324
01100          LENGTH   (PI-COMM-LENGTH)                                EL1324
01101          TRANSID  (WS-TRANS-ID)                                   EL1324
01102      END-EXEC.                                                    EL1324
01103                                                                   EL1324
01104  9100-EXIT.                                                       EL1324
01105      EXIT.                                                        EL1324
01106                                                                   EL1324
01107  9300-XCTL SECTION.                                               EL1324
01108      MOVE DFHENTER               TO  EIBAID.                      EL1324
01109                                                                   EL1324
01110      EXEC CICS XCTL                                               EL1324
01111          PROGRAM  (THIS-PGM)                                      EL1324
01112          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1324
01113          LENGTH   (PI-COMM-LENGTH)                                EL1324
01114      END-EXEC.                                                    EL1324
01115                                                                   EL1324
01116  9300-EXIT.                                                       EL1324
01117      EXIT.                                                        EL1324
01118                                                                   EL1324
01119      EJECT                                                        EL1324
01120  9400-CLEAR SECTION.                                              EL1324
01121      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM                      EL1324
01122      GO TO 9300-XCTL.                                                CL**2
01123                                                                   EL1324
01124  9400-EXIT.                                                       EL1324
01125      EXIT.                                                        EL1324
01126                                                                   EL1324
01127  9600-PGMIDERR SECTION.                                           EL1324
01128      EXEC CICS HANDLE CONDITION                                   EL1324
01129          PGMIDERR (8300-SEND-TEXT)                                EL1324
01130      END-EXEC.                                                    EL1324
01131                                                                   EL1324
01132      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL1324
01133                                      LOGOFF-PGM.                  EL1324
01134                                                                   EL1324
01135      MOVE XCTL-005               TO  THIS-PGM.                    EL1324
01136      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL1324
01137      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL1324
01138      GO TO 9300-XCTL.                                                CL**2
01139                                                                   EL1324
01140  9600-EXIT.                                                       EL1324
01141      EXIT.                                                        EL1324
01142                                                                   EL1324
01143      EJECT                                                        EL1324
01144  9900-ERROR-FORMAT SECTION.                                       EL1324
01145                                                                   EL1324
01146      MOVE LINK-001                TO  THIS-PGM.                   EL1324
01147                                                                   EL1324
01148      EXEC CICS LINK                                               EL1324
01149          PROGRAM  (THIS-PGM)                                      EL1324
01150          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL1324
01151          LENGTH   (EMI-COMM-LENGTH)                               EL1324
01152      END-EXEC.                                                    EL1324
01153                                                                   EL1324
01154      MOVE ER-0000                   TO  EMI-ERROR.                EL1324
01155                                                                   EL1324
01156  9900-EXIT.                                                       EL1324
01157      EXIT.                                                        EL1324
01158                                                                   EL1324
01159      EJECT                                                        EL1324
01160  9990-ERROR SECTION.                                              EL1324
01161      MOVE DFHEIBLK TO EMI-LINE1.                                  EL1324
01162      MOVE LINK-004                TO  THIS-PGM.                   EL1324
01163                                                                   EL1324
01164      EXEC CICS LINK                                               EL1324
01165          PROGRAM  (THIS-PGM)                                      EL1324
01166          COMMAREA (EMI-LINE1)                                     EL1324
01167          LENGTH   (72)                                            EL1324
01168      END-EXEC.                                                    EL1324
01169                                                                   EL1324
01170      GO TO 8200-SEND-DATAONLY.                                    EL1324
01171                                                                   EL1324
01172  9990-EXIT.                                                       EL1324
01173      EXIT.                                                        EL1324
01174                                                                   EL1324
01175  9995-SECURITY-VIOLATION.                                         EL1324
01176             COPY ELCSCTP.                                         EL1324
01177                                                                   EL1324
01178  9995-EXIT.                                                       EL1324
01179      EXIT.                                                           CL**2
01180                                                                      CL**2
01181                                                                   EL1324
