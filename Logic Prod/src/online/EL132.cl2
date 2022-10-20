00001  IDENTIFICATION DIVISION.                                         03/27/96
00002                                                                   EL132
00003  PROGRAM-ID.                 EL132 .                                 LV016
00004 *              PROGRAM CONVERTED BY                                  CL*13
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*13
00006 *              CONVERSION DATE 06/29/95 09:01:19.                    CL*13
00007 *                            VMOD=2.016                              CL*16
00008 *                                                                 EL132
00008 *                                                                 EL132
00009 *AUTHOR.    LOGIC, INC.                                              CL*13
00010 *           DALLAS, TEXAS.                                           CL*13
00011                                                                   EL132
00012 *DATE-COMPILED.                                                      CL*13
00013                                                                   EL132
00014 *SECURITY.   *****************************************************   CL*13
00015 *            *                                                   *   CL*13
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*13
00017 *            *                                                   *   CL*13
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*13
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*13
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*13
00021 *            *                                                   *   CL*13
00022 *            *****************************************************   CL*13
00023                                                                   EL132
00024 *REMARKS.                                                            CL**2
00025 *        THIS PROGRAM PROVIDES THE QUALIFICATION NECESSARY FOR       CL**2
00026 *    THE CLAIM LOOK-UP.                                              CL**2
00027                                                                      CL*13
00028 *    TRANS-ID    - EX21                                              CL*13
00029                                                                   EL132
00030 *    SCREENS     - EL132A - CLAIM LOOK-UP QUALIFICATION              CL**2
00031                                                                   EL132
00032 *    ENTERED BY  - EL126 - MAINTENANCE MENU                          CL**2
00033 *                  EL130 - NEW CLAIM SET-UP                          CL**2
00034 *                  EL150 - STATUS DISPLAY (ENTERS EL1323 ONLY)       CL**2
00035 *                  EL1275 - CERTIFICATE COVERAGES                    CL*12
00036                                                                   EL132
00037 *    EXIT TO     - CALLING PROGRAM                                   CL**2
00038 *                  EL130 - NEW CLAIM SETUP                           CL**2
00039                                                                   EL132
00040 *    INPUT FILES - ELCERT - CERTIFICATE INFORCE FILE                 CL*13
00041 *                  ELACCT - ACCOUNT MASTER FILE                      CL**2
00042 *                  ELMSTR - CLAIMS MASTER                            CL*13
00043 *                  ELRETR - RETRIEVE MASTER                          CL*13
00044                                                                   EL132
00045 *    OUTPUT FILE - NONE                                              CL**2
00046                                                                   EL132
00047 *    COMMAREA    - PASSED.  IF A CERTIFICATE IS SELECTED, THE        CL**2
00048 *                  CONTROL OF THAT CERTIFICATE IS PLACED IN THE      CL**2
00049 *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR           CL**2
00050 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM    CL**2
00051 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE     CL**2
00052 *                  RECORD KEY INFORMATION NEEDED BY EL1322 TO        CL**2
00053 *                  LOCATE THE CERTIFICATE.                           CL**2
00054                                                                   EL132
00055 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON        CL**2
00056 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE    CL**2
00057 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE   CL**2
00058 *                  ENTRIES (XCTL FROM CICS VIA EX21) THE SCREEN      CL**2
00059 *                  WILL BE READ AND ACTION WILL BE BASED ON THE      CL**2
00060 *                  MAINTENANCE TYPE INDICATED.                       CL**2
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

00061                                                                   EL132
00062      EJECT                                                        EL132
00063  ENVIRONMENT DIVISION.                                            EL132
00064                                                                   EL132
00065  DATA DIVISION.                                                   EL132
00066                                                                   EL132
00067  WORKING-STORAGE SECTION.                                         EL132
00068                                                                   EL132
00069  77  FILLER  PIC X(32)  VALUE '********************************'. EL132
00070  77  FILLER  PIC X(32)  VALUE '*   EL132  WORKING STORAGE     *'. EL132
00071  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.016 **********'.    CL*16
00072                                                                   EL132
00073                                  COPY ELCSCTM.                       CL**6
00074                                                                   EL132
00075                                  COPY ELCSCRTY.                      CL**6
00076                                                                   EL132
00077  01  WS-DATE-AREA.                                                EL132
00078      12  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL132
00079      12  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL132
00080                                                                   EL132
00081  01  FILLER                          COMP-3.                      EL132
00082      12  WS-RECORD-COUNT         PIC S9(3)   VALUE ZERO.          EL132
00083      12  WS-READNEXT-SW          PIC S9      VALUE ZERO.          EL132
00084      12  WS-ERROR-NUMBER         PIC S9(3)   VALUE ZERO.          EL132
00085      12  WS-ERROR-COUNT          PIC S9(3)   VALUE ZERO.          EL132
00086      12  WS-LAST-ERROR-COUNT     PIC S9(3)   VALUE ZERO.          EL132
00087      12  WS-UPDATE-SW            PIC S9      VALUE ZERO.          EL132
00088      12  WS-COMPLETED-SUCCESSFUL PIC S9      VALUE ZERO.          EL132
00089          88  TRANSACTION-SUCCESSFUL          VALUE +1.            EL132
00090                                                                   EL132
00091      12  WS-CALL-SW              PIC S9      VALUE ZERO.          EL132
00092                                                                   EL132
00093      12  TIME-IN                 PIC S9(7)   VALUE ZERO.          EL132
00094      12  TIME-OUT                    REDEFINES                    EL132
00095          TIME-IN                 PIC S9(3)V9(4).                  EL132
00096                                                                   EL132
00097  01  FILLER                          COMP  SYNC.                  EL132
00098      12  SC-ITEM                 PIC S9(4)   VALUE +0001.         EL132
00099      12  WS-INDEX                PIC S9(4)   VALUE ZERO.          EL132
00100                                                                   EL132
00101      12  WS-JOURNAL-FILE-ID      PIC S9(4)   VALUE +1.            EL132
00102      12  WS-JOURNAL-RECORD-LENGTH  PIC S9(4)   VALUE +527.        EL132
00103                                                                   EL132
00104      12  WS-KEY-LENGTH           PIC S9(4)   VALUE ZERO.          EL132
00105                                                                   EL132
00106  01  FILLER.                                                      EL132
00107      12  QID.                                                     EL132
00108          16  QID-TERM            PIC X(4).                        EL132
00109          16  FILLER              PIC X(4)   VALUE '132A'.         EL132
00110      12  QID-PROC-AREA           PIC XXX.                         EL132
00111      12  QID-LENGTH              PIC S9(4)  VALUE +3  COMP.       EL132
00112      12  QID-ITEM                PIC S9(4)  VALUE +1  COMP.       EL132
00113                                                                   EL132
00114      12  WS-ELCNTL-KEY.                                              CL*13
00115          16  WS-ELCNTL-ID        PIC X(3).                           CL*13
00116          16  WS-ELCNTL-TYPE      PIC X.                              CL*13
00117          16  WS-ELCNTL-USER      PIC X(4)   VALUE SPACES.            CL*13
00118          16  WS-ELCNTL-SEQ       PIC S9(4)  VALUE +0       COMP.     CL*13
00119                                                                   EL132
00120      12  WS-MAPSET-NAME          PIC X(8)  VALUE 'EL132S'.        EL132
00121      12  WS-MAP-NAME             PIC X(8)  VALUE 'EL132A'.        EL132
00122                                                                   EL132
00123      12  FILLER                      REDEFINES                    EL132
00124          WS-MAP-NAME.                                             EL132
00125          16  FILLER              PIC XX.                          EL132
00126          16  WS-MAP-NUMBER       PIC X(4).                        EL132
00127          16  FILLER              PIC XX.                             CL*13
00128                                                                   EL132
00129      12  THIS-PGM                PIC X(8)  VALUE 'EL132'.         EL132
00130                                                                   EL132
00131      12  ELMSTR-FILE-ID          PIC X(8) VALUE 'ELMSTR'.            CL*13
00132      12  ELMSTR2-FILE-ID         PIC X(8) VALUE 'ELMSTR2'.           CL*13
00133      12  ELMSTR3-FILE-ID         PIC X(8) VALUE 'ELMSTR3'.           CL*13
00134      12  ELMSTR4-FILE-ID         PIC X(8) VALUE 'ELMSTR4'.           CL*13
00135      12  ELMSTR5-FILE-ID         PIC X(8) VALUE 'ELMSTR5'.           CL*13
00136      12  ELMSTR6-FILE-ID         PIC X(8) VALUE 'ELMSTR6'.           CL*13
00137                                                                      CL*13
00138      12  ELCNTL-FILE-ID          PIC X(8) VALUE 'ELCNTL'.            CL**6
00139                                                                      CL*13
00140      12  ELRETR-FILE-ID          PIC X(8) VALUE 'ELRETR'.            CL*13
00141      12  ELRETR2-FILE-ID         PIC X(8) VALUE 'ELRETR2'.           CL*13
00142      12  ELRETR3-FILE-ID         PIC X(8) VALUE 'ELRETR3'.           CL*13
00143      12  ELRETR4-FILE-ID         PIC X(8) VALUE 'ELRETR4'.           CL*13
00144      12  ELRETR5-FILE-ID         PIC X(8) VALUE 'ELRETR5'.           CL*13
00145      12  ELRETR6-FILE-ID         PIC X(8) VALUE 'ELRETR6'.           CL*13
00146                                                                   EL132
00147      12  WS-JOURNAL-TYPE-ID      PIC XX      VALUE 'EL'.          EL132
00148                                                                   EL132
00149      12  WS-LOW-VALUES           PIC X VALUE LOW-VALUES.          EL132
00150      12  WS-SPACES               PIC X       VALUE SPACES.        EL132
00151                                                                   EL132
00152      12  WS-TRANS-ID             PIC X(4)    VALUE 'EX21'.        EL132
00153                                                                      CL**6
00154      12  WS-CNTL-REC-FOUND-SW    PIC X(01)   VALUE SPACES.           CL**6
00155      12  WS-NEXT-COMPANY-ID      PIC X(03)   VALUE SPACES.           CL**6
00156                                                                   EL132
00157      12  WS-INPUT-FIELD          PIC X(50)   VALUE SPACES.        EL132
00158                                                                   EL132
00159      12  WS-INPUT-CHAR    REDEFINES                               EL132
00160          WS-INPUT-FIELD          PIC X                            EL132
00161          OCCURS 50 TIMES             INDEXED BY INPUT-INDEX.      EL132
00162                                                                   EL132
00163  01  ERROR-MESSAGES.                                              EL132
00164      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL132
00165      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL132
00166      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL132
00167      12  ER-0019                 PIC X(4)  VALUE '0019'.             CL**6
00168      12  ER-0022                 PIC X(4)  VALUE '0022'.          EL132
00169      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL132
00170      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL132
00171      12  ER-0089                 PIC X(4)  VALUE '0089'.             CL**6
00172      12  ER-0194                 PIC X(4)  VALUE '0194'.          EL132
00173      12  ER-0209                 PIC X(4)  VALUE '0209'.          EL132
00174      12  ER-0210                 PIC X(4)  VALUE '0210'.          EL132
00175      12  ER-0228                 PIC X(4)  VALUE '0228'.             CL**6
00176      12  ER-0284                 PIC X(4)  VALUE '0284'.          EL132
00177      12  ER-0488                 PIC X(4)  VALUE '0488'.          EL132
00178      12  ER-2370                 PIC X(4)  VALUE '2370'.          EL132
00179      12  ER-2374                 PIC X(4)  VALUE '2374'.          EL132
00180                                                                   EL132
00181      EJECT                                                        EL132
00182                                  COPY ELCINTF.                       CL**6
00183                                                                   EL132
00184                                  COPY ELC132PI.                      CL**6
00185                                                                   EL132
00186      EJECT                                                        EL132
00187                                  COPY ELCEMIB.                       CL**6
00188                                                                   EL132
00189      EJECT                                                        EL132
00190                                  COPY ELCDATE.                       CL**6
00191                                                                   EL132
00192      EJECT                                                        EL132
00193                                  COPY EL132S.                        CL**6
00194                                                                   EL132
00195      EJECT                                                        EL132
00196                                  COPY ELCLOGOF.                      CL**6
00197                                                                   EL132
00198      EJECT                                                        EL132
00199                                  COPY ELCATTR.                       CL**6
00200                                                                   EL132
00201      EJECT                                                        EL132
00202                                  COPY ELCAID.                        CL**6
00203                                                                   EL132
00204  01  FILLER      REDEFINES                                        EL132
00205      DFHAID.                                                      EL132
00206                                                                   EL132
00207      12  FILLER                  PIC X(8).                        EL132
00208                                                                   EL132
00209      12  PF-VALUES               PIC X                            EL132
00210          OCCURS 24 TIMES.                                         EL132
00211      EJECT                                                        EL132
00212  LINKAGE SECTION.                                                 EL132
00213                                                                   EL132
00214  01  DFHCOMMAREA                 PIC X(1024).                     EL132
00215                                                                   EL132
00216 *01 DFHBLLDS    COMP                                                 CL*13
00217 *               SYNCHRONIZED.                                        CL*13
00218 *    12  FILLER                  PIC S9(8).                          CL*13
00219 *    12  ELMSTR-POINTER          PIC S9(8).                          CL*13
00220 *    12  ELCNTL-POINTER          PIC S9(8).                          CL*13
00221                                                                   EL132
00222      EJECT                                                        EL132
00223                                  COPY ELCMSTR.                       CL**6
00224                                                                      CL*13
00225      EJECT                                                           CL*13
00226                                  COPY ELCRETR.                       CL*13
00227                                                                   EL132
00228      EJECT                                                        EL132
00229                                  COPY ELCCNTL.                       CL**6
00230                                                                   EL132
00231      EJECT                                                        EL132
00232  PROCEDURE DIVISION.                                              EL132
00233                                                                   EL132
00234      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL132
00235      MOVE '5'                    TO DC-OPTION-CODE.               EL132
00236      PERFORM 8500-DATE-CONVERSION.                                EL132
00237      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL132
00238      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL132
00239                                                                   EL132
00240      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL132
00241                                                                   EL132
00242      MOVE +2                     TO  EMI-NUMBER-OF-LINES          EL132
00243                                      EMI-SWITCH2.                 EL132
00244                                                                   EL132
00245 *    NOTE ******************************************************* EL132
00246 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL132
00247 *         *  FROM ANOTHER MODULE.                               * EL132
00248 *         *******************************************************.EL132
00249                                                                   EL132
00250      IF EIBCALEN NOT GREATER ZERO                                 EL132
00251          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL132
00252          GO TO 8300-SEND-TEXT.                                    EL132
00253                                                                   EL132
00254      EXEC CICS HANDLE CONDITION                                   EL132
00255          PGMIDERR (9600-PGMIDERR)                                 EL132
00256          ERROR    (9990-ERROR)                                    EL132
00257      END-EXEC.                                                    EL132
00258                                                                   EL132
00259      EJECT                                                        EL132
00260  0010-MAIN-LOGIC.                                                 EL132
00261 *    NOTE ******************************************************* EL132
00262 *         *      IF THE TRANSACTION CODE OF THE TASK THAT       * EL132
00263 *         *  INVOKED THIS MODULE IS NOT EX21, THIS IS THE FIRST * EL132
00264 *         *  TIME THROUGH THIS MODULE.                          * EL132
00265 *         *******************************************************.EL132
00266                                                                   EL132
00267      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL132
00268          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL132
00269              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL132
00270              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL132
00271              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL132
00272              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL132
00273              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL132
00274              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL132
00275              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL132
00276              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM     EL132
00277            ELSE                                                   EL132
00278              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL132
00279              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL132
00280              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL132
00281              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL132
00282              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL132
00283              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL132
00284              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL132
00285              MOVE SPACES               TO  PI-SAVED-PROGRAM-6     EL132
00286              PERFORM 7000-BUILD-SCREEN                            EL132
00287              MOVE +1             TO  WS-CALL-SW                   EL132
00288        ELSE                                                       EL132
00289          GO TO 0020-MAIN-LOGIC.                                   EL132
00290                                                                   EL132
00291        IF PI-CALLING-PROGRAM   = 'EL132'   AND                       CL*13
00292           PI-RETURN-TO-PROGRAM = 'EL1275'  AND                       CL*13
00293           WS-CALL-SW IS EQUAL TO +1                                  CL*12
00294            GO TO 9400-CLEAR.                                         CL*12
00295                                                                      CL*12
00296  0015-MAIN-LOGIC.                                                 EL132
00297 *    NOTE ******************************************************* EL132
00298 *         *     INITIALIZE THE WORK FIELDS FOR THE PROGRAM      *    CL*13
00299 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL132
00300 *         *******************************************************.EL132
00301                                                                   EL132
00302      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.        EL132
00303                                                                   EL132
00304      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL132
00305                                      PI-LINE-COUNT                EL132
00306                                      PI-AIX-RECORD-COUNT          EL132
00307                                      PI-BROWSE-SW                 EL132
00308                                      PI-KEY-LENGTH                EL132
00309                                      PI-TS-ITEM                   EL132
00310                                      PI-END-OF-FILE               EL132
00311                                      PI-START-SW.                 EL132
00312                                                                   EL132
00313 *    NOTE ******************************************************* EL132
00314 *         *      IF CONTROL WAS PASSED FROM THE CERTIFICATE     * EL132
00315 *         *  LOOK-UP (EL127) PASS CONTROL TO THE CLAIM LOOK-UP  * EL132
00316 *         *  ACCESSING THE CLAIM MASTER BY CERTIFICATE NUMBER   * EL132
00317 *         *******************************************************.EL132
00318                                                                   EL132
00319      MOVE LOW-VALUES             TO  EL132AO.                     EL132
00320                                                                   EL132
00321      IF (PI-RETURN-TO-PROGRAM = 'EL127' OR 'EL1275')                 CL*13
00322                   AND                                                CL*12
00323         WS-CALL-SW = ZERO                                            CL*12
00324          NEXT SENTENCE                                            EL132
00325      ELSE                                                            CL*12
00326          GO TO 0018-MAIN-LOGIC.                                   EL132
00327                                                                   EL132
00328      MOVE PI-COMPANY-CD      TO  PI-CK-COMPANY-CD.                EL132
00329      MOVE PI-CERT-NO         TO  PI-CK-CERT-NO-A4.                EL132
00330      MOVE PI-CLAIM-KEY       TO  PI-SELECTION-CRITERIA.           EL132
00331      MOVE +12                TO  PI-KEY-LENGTH.                   EL132
00332      MOVE '4'                TO  PI-OPTION.                       EL132
00333      MOVE ELMSTR5-FILE-ID    TO  PI-DSID.                            CL*13
00334      MOVE 'EL1322'           TO  THIS-PGM.                        EL132
00335                                                                      CL*12
00336      IF PI-RETURN-TO-PROGRAM = 'EL127'                               CL*13
00337          MOVE +12            TO  PI-KEY-LENGTH                       CL*12
00338      ELSE                                                            CL*12
00339          MOVE +11            TO  PI-KEY-LENGTH.                      CL*12
00340                                                                   EL132
00341      PERFORM 4000-READ-ELMSTR.                                       CL*16
00342                                                                      CL*16
00343      MOVE -1                 TO ACRTNOL.                             CL*16
00344      MOVE PI-CERT-PRIME      TO ACRTNOO.                             CL*16
00345      MOVE PI-CERT-SFX        TO ACRTSXO.                             CL*16
00346      MOVE AL-UABON           TO ACRTNOA                              CL*16
00347                                 ACRTSXA.                             CL*16
00348                                                                      CL*16
00349      GO TO 8100-SEND-INITIAL-MAP.                                    CL*16
00350                                                                   EL132
00351  0018-MAIN-LOGIC.                                                 EL132
00352                                                                      CL*13
00353      MOVE PI-COMPANY-ID          TO  WS-ELCNTL-ID.                   CL*13
00354      MOVE '1'                    TO  WS-ELCNTL-TYPE.                 CL*13
00355      MOVE SPACES                 TO  WS-ELCNTL-USER.                 CL*13
00356      MOVE +0                     TO  WS-ELCNTL-SEQ.                  CL*13
00357                                                                      CL*13
00358      PERFORM 6000-READ-CONTROL THRU 6000-EXIT.                       CL*13
00359                                                                      CL*13
00360      MOVE CF-CLAIMS-CREDIT-CARD-INDEX                                CL*13
00361                                  TO  PI-CREDIT-CARD-INDEX.           CL*13
00362                                                                      CL*12
00363      MOVE SPACES                 TO  PI-CONTROL-IN-PROGRESS.      EL132
00364                                                                   EL132
00365      GO TO 8100-SEND-INITIAL-MAP.                                    CL**6
00366                                                                   EL132
00367      EJECT                                                        EL132
00368  0020-MAIN-LOGIC.                                                 EL132
00369      IF PI-1ST-TIME-SW NOT = ZERO                                 EL132
00370          GO TO 0015-MAIN-LOGIC.                                   EL132
00371                                                                   EL132
00372 *    NOTE ******************************************************* EL132
00373 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL132
00374 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL132
00375 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL132
00376 *         *******************************************************.EL132
00377                                                                   EL132
00378      IF EIBAID = DFHCLEAR                                         EL132
00379          GO TO 9400-CLEAR.                                        EL132
00380                                                                   EL132
00381      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL132
00382          MOVE LOW-VALUES         TO  EL132AO                      EL132
00383          MOVE -1                 TO  APFKL                        EL132
00384          MOVE ER-0008            TO  EMI-ERROR                    EL132
00385          GO TO 8200-SEND-DATAONLY.                                   CL**6
00386                                                                   EL132
00387      EXEC CICS RECEIVE                                            EL132
00388          INTO   (EL132AO)                                         EL132
00389          MAPSET (WS-MAPSET-NAME)                                  EL132
00390          MAP    (WS-MAP-NAME)                                     EL132
00391      END-EXEC.                                                    EL132
00392                                                                   EL132
00393      IF APFKL GREATER ZERO                                        EL132
00394          IF EIBAID NOT = DFHENTER                                 EL132
00395              MOVE ER-0004           TO  EMI-ERROR                 EL132
00396              MOVE AL-UNBOF       TO  APFKA                        EL132
00397              MOVE -1             TO  APFKL                        EL132
00398              GO TO 8200-SEND-DATAONLY                                CL**6
00399            ELSE                                                   EL132
00400              IF APFKO GREATER ZERO AND LESS '25'                  EL132
00401                  MOVE PF-VALUES (APFKI)  TO  EIBAID               EL132
00402                ELSE                                               EL132
00403                  MOVE ER-0029           TO  EMI-ERROR             EL132
00404                  MOVE AL-UNBOF       TO  APFKA                    EL132
00405                  MOVE -1             TO  APFKL                    EL132
00406                  GO TO 8200-SEND-DATAONLY.                           CL**6
00407                                                                   EL132
00408      IF EIBAID = DFHPF12                                          EL132
00409          MOVE 'EL010'            TO  THIS-PGM                     EL132
00410          GO TO 9300-XCTL.                                         EL132
00411                                                                   EL132
00412      IF EIBAID = DFHPF23                                          EL132
00413          GO TO 9000-RETURN-CICS.                                  EL132
00414                                                                   EL132
00415      IF EIBAID = DFHPF24                                          EL132
00416          MOVE 'EL126'            TO  THIS-PGM                     EL132
00417          GO TO 9300-XCTL.                                         EL132
00418                                                                   EL132
00419      IF EIBAID = DFHPF1                                              CL**7
00420          MOVE 'EL1324'           TO  THIS-PGM                        CL**7
00421          GO TO 9300-XCTL.                                            CL**7
00422                                                                      CL*13
00423      IF EIBAID = DFHPF2                                              CL*13
00424          GO TO 0025-MAIN-LOGIC.                                      CL*13
00425                                                                      CL**7
00426      IF EIBAID NOT = DFHENTER AND DFHPF5 AND DFHPF6               EL132
00427          MOVE -1                 TO  APFKL                        EL132
00428          MOVE ER-0008            TO  EMI-ERROR                    EL132
00429          GO TO 8200-SEND-DATAONLY.                                   CL**6
00430                                                                   EL132
00431      EJECT                                                        EL132
00432  0025-MAIN-LOGIC.                                                 EL132
00433      IF EIBAID = DFHPF5                                           EL132
00434         IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                    EL132
00435            PERFORM 5000-NEXT-COMPANY THRU 5000-EXIT               EL132
00436            PERFORM 5500-WRITE-SECURITY-TEMP-STORE THRU 5500-EXIT  EL132
00437         ELSE                                                      EL132
00438            MOVE -1                 TO  APFKL                      EL132
00439            MOVE ER-0008            TO  EMI-ERROR                  EL132
00440            GO TO 8200-SEND-DATAONLY.                                 CL**6
00441                                                                   EL132
00442      IF EIBAID = DFHPF6                                           EL132
00443         IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                    EL132
00444            PERFORM 5000-NEXT-COMPANY THRU 5000-EXIT               EL132
00445            PERFORM 5500-WRITE-SECURITY-TEMP-STORE THRU 5500-EXIT  EL132
00446            MOVE PI-ORIGINAL-COMPANY-CD TO PI-COMPANY-CD           EL132
00447            MOVE PI-ORIGINAL-COMPANY-ID TO PI-COMPANY-ID           EL132
00448         ELSE                                                      EL132
00449            MOVE -1                 TO  APFKL                      EL132
00450            MOVE ER-0008            TO  EMI-ERROR                  EL132
00451            GO TO 8200-SEND-DATAONLY.                                 CL**6
00452                                                                   EL132
00453      MOVE SPACES                 TO  PI-SELECTION-CRITERIA        EL132
00454                                      PI-CLAIM-KEY.                EL132
00455                                                                   EL132
00456      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD             EL132
00457                                      PI-CK-COMPANY-CD.            EL132
00458                                                                   EL132
00459      MOVE 'EL1322'               TO  THIS-PGM.                    EL132
00460                                                                   EL132
00461      IF PI-PROCESSOR-ID = 'LGXX'                                  EL132
00462          NEXT SENTENCE                                            EL132
00463      ELSE                                                         EL132
00464          EXEC CICS READQ TS                                       EL132
00465              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL132
00466              INTO    (SECURITY-CONTROL)                           EL132
00467              LENGTH  (SC-COMM-LENGTH)                             EL132
00468              ITEM    (SC-ITEM)                                    EL132
00469          END-EXEC                                                 EL132
00470          MOVE SC-CLAIMS-DISPLAY (21)   TO  PI-DISPLAY-CAP         EL132
00471          MOVE SC-CLAIMS-UPDATE  (21)   TO  PI-MODIFY-CAP          EL132
00472          IF NOT DISPLAY-CAP                                       EL132
00473              MOVE 'READ'               TO  SM-READ                EL132
00474              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT          CL*15
00475              MOVE ER-0070              TO  EMI-ERROR              EL132
00476              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL132
00477              GO TO 8100-SEND-INITIAL-MAP.                            CL**6
00478                                                                   EL132
00479 ******************************************************************EL132
00480 *           O P T I O N  1  P R O C E S S I N G                  *EL132
00481 ******************************************************************EL132
00482                                                                   EL132
00483      IF ACLAIML  GREATER ZERO  OR                                 EL132
00484         ACARIERL GREATER ZERO  OR                                 EL132
00485         ACERTNOL GREATER ZERO  OR                                 EL132
00486         ACERTSXL GREATER ZERO                                     EL132
00487          NEXT SENTENCE                                            EL132
00488        ELSE                                                       EL132
00489          GO TO 0100-MAIN-LOGIC.                                   EL132
00490                                                                   EL132
00491      MOVE '1'                    TO  PI-OPTION.                   EL132
00492                                                                   EL132
00493      IF ACLAIML GREATER THAN ZERO                                    CL**7
00494          IF ACLAIMI  IS EQUAL TO  SPACES OR LOW-VALUES               CL**7
00495              MOVE ER-0284            TO  EMI-ERROR                   CL**7
00496              MOVE -1                 TO  ALNAMEL                     CL**7
00497              GO TO 8200-SEND-DATAONLY.                               CL**7
00498                                                                      CL**7
00499 ******************************************************************EL132
00500 *              SECURITY CHECK FOR CARRIER                        *EL132
00501 *                    04/02/84                                    *EL132
00502 ******************************************************************EL132
00503                                                                   EL132
00504      IF  PI-NO-CARRIER-SECURITY                                   EL132
00505          GO TO 0028-BUILD-CARRIER-NO.                             EL132
00506                                                                   EL132
00507      IF ACARIERL GREATER ZERO                                     EL132
00508         IF  ACARIERI = PI-CARRIER-SECURITY                        EL132
00509             GO TO 0028-BUILD-CARRIER-NO.                          EL132
00510                                                                   EL132
00511      MOVE ER-2370                TO  EMI-ERROR.                   EL132
00512      MOVE -1                     TO  ACARIERL.                    EL132
00513      MOVE AL-UABON               TO  ACARIERA.                    EL132
00514      GO TO 8200-SEND-DATAONLY.                                       CL**6
00515                                                                   EL132
00516  0028-BUILD-CARRIER-NO.                                           EL132
00517      IF ACARIERL GREATER ZERO                                     EL132
00518          MOVE ACARIERI           TO  PI-SC-CARRIER                EL132
00519                                      PI-CK-CARRIER                EL132
00520                                      PI-CARRIER                   EL132
00521          MOVE +2                 TO  PI-KEY-LENGTH                EL132
00522      ELSE                                                            CL**5
00523          MOVE ER-0194            TO  EMI-ERROR                       CL**5
00524          PERFORM 9900-ERROR-FORMAT                                   CL**5
00525          MOVE -1                 TO  ACARIERL.                       CL**5
00526                                                                   EL132
00527      IF ACLAIML GREATER ZERO                                      EL132
00528          MOVE ACLAIMI           TO  PI-SC-CLAIM-NO                EL132
00529                                      PI-CK-CLAIM                  EL132
00530                                      PI-CLAIM-NO                  EL132
00531                                      WS-INPUT-FIELD               EL132
00532          PERFORM 0030-MAIN-LOGIC THRU 0030-MAIN-LOGIC-EXIT        EL132
00533              VARYING INPUT-INDEX FROM ACLAIML BY -1               EL132
00534                  UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE    EL132
00535          ADD +2  ACLAIML GIVING PI-KEY-LENGTH                     EL132
00536        ELSE                                                       EL132
00537          MOVE ER-0209           TO  EMI-ERROR                     EL132
00538          PERFORM 9900-ERROR-FORMAT                                EL132
00539          MOVE -1                TO  ACLAIML.                      EL132
00540                                                                   EL132
00541      IF ACERTNOL GREATER ZERO                                     EL132
00542          MOVE ACERTNOI           TO  PI-SC-CERT-PRIME             EL132
00543                                      WS-INPUT-FIELD               EL132
00544          MOVE +19                TO  PI-KEY-LENGTH.               EL132
00545                                                                   EL132
00546      IF ACERTSXL GREATER ZERO                                     EL132
00547          MOVE +20                TO  PI-KEY-LENGTH                EL132
00548          MOVE ACERTSXI           TO  PI-SC-CERT-SFX.              EL132
00549                                                                   EL132
00550      MOVE PI-SC-CERT-NO          TO  PI-CK-CERT-NO                EL132
00551                                      PI-CERT-NO.                  EL132
00552                                                                   EL132
00553      IF EMI-FATAL-CTR GREATER ZERO                                EL132
00554          GO TO 8200-SEND-DATAONLY.                                   CL**6
00555                                                                   EL132
00556      IF ACLAIML GREATER ZERO                                      EL132
00557          MOVE AL-UABON           TO  ACLAIMA                      EL132
00558        ELSE                                                       EL132
00559          MOVE AL-UABOF           TO  ACLAIMA.                     EL132
00560                                                                   EL132
00561      IF ACARIERL GREATER ZERO                                     EL132
00562          MOVE AL-UABON           TO  ACARIERA                     EL132
00563        ELSE                                                       EL132
00564          MOVE AL-UABOF           TO  ACARIERA.                    EL132
00565                                                                   EL132
00566      IF ACERTNOL GREATER ZERO                                     EL132
00567          MOVE AL-UABON           TO  ACERTNOA                     EL132
00568        ELSE                                                       EL132
00569          MOVE AL-UABOF           TO  ACERTNOA.                    EL132
00570                                                                   EL132
00571      IF ACERTSXL GREATER ZERO                                     EL132
00572          MOVE AL-UABON           TO  ACERTSXA                     EL132
00573        ELSE                                                       EL132
00574          MOVE AL-UABOF           TO  ACERTSXA.                    EL132
00575                                                                   EL132
00576      MOVE -1                     TO  ACLAIML.                     EL132
00577                                                                      CL*13
00578      IF EIBAID = DFHPF2                                              CL*15
00579           MOVE ELRETR-FILE-ID    TO  PI-DSID                         CL*15
00580           PERFORM 3000-READ-ELRETR                                   CL*16
00581        ELSE                                                          CL*15
00582           MOVE ELMSTR-FILE-ID    TO  PI-DSID                         CL*15
00583           PERFORM 4000-READ-ELMSTR.                                  CL*16
00584                                                                      CL*16
00585      MOVE +1                     TO  PI-1ST-TIME-SW.                 CL*16
00586                                                                      CL*16
00587      IF PI-RETURN-TO-PROGRAM = 'EL126'                               CL*16
00588          MOVE 'EL150'            TO  THIS-PGM                        CL*16
00589        ELSE                                                          CL*16
00590          MOVE 'EL1323'           TO  THIS-PGM.                       CL*16
00591                                                                      CL*16
00592      PERFORM 9300-XCTL.                                              CL*16
00593                                                                   EL132
00594  0030-MAIN-LOGIC.                                                 EL132
00595      SUBTRACT +1 FROM ACLAIML.                                    EL132
00596                                                                   EL132
00597  0030-MAIN-LOGIC-EXIT.                                            EL132
00598      EXIT.                                                        EL132
00599                                                                   EL132
00600      EJECT                                                        EL132
00601  0100-MAIN-LOGIC.                                                 EL132
00602 ******************************************************************EL132
00603 *           O P T I O N  2  P R O C E S S I N G                  *EL132
00604 ******************************************************************EL132
00605                                                                   EL132
00606      IF ALNAMEL GREATER ZERO  OR                                  EL132
00607         AFNAMEL GREATER ZERO  OR                                     CL**3
00608         AMINITL GREATER ZERO  OR                                     CL**3
00609         AACCTL  GREATER ZERO                                         CL**3
00610          NEXT SENTENCE                                            EL132
00611        ELSE                                                       EL132
00612          GO TO 0200-MAIN-LOGIC.                                   EL132
00613                                                                   EL132
00614      IF (AFNAMEL GREATER THAN +0 OR                                  CL**3
00615          AMINITL GREATER THAN +0 OR                                  CL**3
00616          AACCTL  GREATER THAN +0)                                    CL**3
00617        AND ALNAMEL NOT GREATER ZERO                               EL132
00618          MOVE ER-0488            TO  EMI-ERROR                    EL132
00619          MOVE -1                 TO  ALNAMEL                      EL132
00620          GO TO 8200-SEND-DATAONLY.                                   CL**6
00621                                                                   EL132
00622      IF ALNAMEL GREATER THAN ZERO                                    CL**7
00623          IF ALNAMEI  IS EQUAL TO  SPACES OR LOW-VALUES               CL**7
00624              MOVE ER-0284            TO  EMI-ERROR                   CL**7
00625              MOVE -1                 TO  ALNAMEL                     CL**7
00626              GO TO 8200-SEND-DATAONLY.                               CL**7
00627                                                                      CL**7
00628      MOVE '2'                    TO  PI-OPTION.                   EL132
00629      MOVE PI-COMPANY-CD          TO  PI-SELECTION-CRITERIA        EL132
00630                                      PI-CK-COMPANY-CD.            EL132
00631                                                                   EL132
00632      MOVE +1                     TO  PI-KEY-LENGTH.               EL132
00633                                                                   EL132
00634      IF ALNAMEL GREATER ZERO                                      EL132
00635         MOVE ALNAMEI            TO  PI-CK-INSURED-LAST-NAME          CL**3
00636                                     PI-SC-LAST-NAME                  CL**3
00637                                     WS-INPUT-FIELD                   CL**3
00638         IF AFNAMEL EQUAL +0                                          CL**3
00639          AND                                                         CL**3
00640            AMINITL EQUAL +0                                          CL**3
00641            PERFORM 0120-MAIN-LOGIC THRU 0120-MAIN-LOGIC-EXIT         CL**3
00642              VARYING INPUT-INDEX FROM ALNAMEL BY -1               EL132
00643              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE           CL**3
00644            ADD ALNAMEL  TO  PI-KEY-LENGTH                            CL**3
00645         ELSE                                                         CL**3
00646            MOVE +16     TO  PI-KEY-LENGTH.                           CL**3
00647                                                                   EL132
00648      IF AFNAMEL GREATER ZERO                                         CL**3
00649         MOVE AFNAMEI            TO  PI-SC-FIRST-NAME                 CL**3
00650         IF AMINITL GREATER ZERO                                      CL**3
00651            ADD +13              TO  PI-KEY-LENGTH                    CL**3
00652            MOVE AMINITI         TO  PI-SC-INITIAL2                   CL**4
00653         ELSE                                                         CL**4
00654            MOVE AFNAMEI         TO  WS-INPUT-FIELD                   CL**4
00655            PERFORM 0125-MAIN-LOGIC THRU 0125-MAIN-LOGIC-EXIT         CL**4
00656              VARYING INPUT-INDEX FROM AFNAMEL BY -1                  CL**4
00657              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE           CL**4
00658            ADD AFNAMEL           TO  PI-KEY-LENGTH.                  CL**4
00659                                                                   EL132
00660      IF AACCTL GREATER THAN +0                                       CL**3
00661         MOVE AACCTI              TO  PI-ACCOUNT-NUMBER               CL**3
00662      ELSE                                                            CL**3
00663         MOVE SPACES              TO  PI-ACCOUNT-NUMBER.              CL**3
00664                                                                   EL132
00665      MOVE PI-SELECTION-CRITERIA  TO  PI-CLAIM-KEY.                EL132
00666                                                                   EL132
00667      IF ALNAMEL GREATER ZERO                                      EL132
00668          MOVE AL-UABON           TO  ALNAMEA                      EL132
00669      ELSE                                                            CL**2
00670          MOVE AL-UABOF           TO  ALNAMEA.                     EL132
00671                                                                   EL132
00672      IF AFNAMEL GREATER ZERO                                         CL**3
00673          MOVE AL-UABON           TO  AFNAMEA                         CL**3
00674      ELSE                                                            CL**2
00675          MOVE AL-UABOF           TO  AFNAMEA.                        CL**3
00676                                                                      CL**3
00677      IF AMINITL GREATER ZERO                                         CL**3
00678          MOVE AL-UABON           TO  AMINITA                         CL**3
00679      ELSE                                                            CL**3
00680          MOVE AL-UABOF           TO  AMINITA.                        CL**3
00681                                                                      CL**3
00682      IF AACCTL  GREATER ZERO                                         CL**3
00683          MOVE AL-UABON           TO  AACCTA                          CL**3
00684      ELSE                                                            CL**3
00685          MOVE AL-UABOF           TO  AACCTA.                         CL**3
00686                                                                   EL132
00687      MOVE -1                     TO  ALNAMEL.                        CL**2
00688                                                                      CL*13
00689      IF EIBAID = DFHPF2                                              CL*13
00690           MOVE ELRETR2-FILE-ID   TO  PI-DSID                         CL*13
00691           PERFORM 3000-READ-ELRETR                                   CL*16
00692        ELSE                                                          CL*13
00693           MOVE ELMSTR2-FILE-ID   TO  PI-DSID                         CL*13
00694           PERFORM 4000-READ-ELMSTR.                                  CL*16
00695                                                                   EL132
00696  0120-MAIN-LOGIC.                                                 EL132
00697      SUBTRACT +1 FROM ALNAMEL.                                    EL132
00698                                                                   EL132
00699  0120-MAIN-LOGIC-EXIT.                                            EL132
00700      EXIT.                                                           CL**4
00701                                                                      CL**4
00702  0125-MAIN-LOGIC.                                                    CL**4
00703      SUBTRACT +1 FROM AFNAMEL.                                       CL**4
00704                                                                      CL**4
00705  0125-MAIN-LOGIC-EXIT.                                               CL**4
00706      EXIT.                                                        EL132
00707                                                                   EL132
00708      EJECT                                                        EL132
00709  0200-MAIN-LOGIC.                                                 EL132
00710 ******************************************************************EL132
00711 *           O P T I O N  3  P R O C E S S I N G                  *EL132
00712 ******************************************************************EL132
00713                                                                   EL132
00714      IF ASSNL GREATER ZERO                                        EL132
00715          NEXT SENTENCE                                            EL132
00716        ELSE                                                       EL132
00717          GO TO 0300-MAIN-LOGIC.                                   EL132
00718                                                                   EL132
00719      IF ASSNL GREATER THAN ZERO                                      CL**7
00720          IF ASSNI    IS EQUAL TO  SPACES OR LOW-VALUES               CL**7
00721              MOVE ER-0284            TO  EMI-ERROR                   CL**7
00722              MOVE -1                 TO  ASSNL                       CL**7
00723              GO TO 8200-SEND-DATAONLY.                               CL**7
00724                                                                      CL**7
00725      MOVE '3'                    TO  PI-OPTION.                   EL132
00726                                                                   EL132
00727      MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CD             EL132
00728                                      PI-SC-COMPANY-CD.            EL132
00729      MOVE ASSNI                  TO  PI-CK-SOC-SEC-NO             EL132
00730                                      PI-SC-SOC-SEC-NO             EL132
00731                                      WS-INPUT-FIELD.              EL132
00732                                                                   EL132
00733      PERFORM 0220-MAIN-LOGIC THRU 0220-MAIN-LOGIC-EXIT            EL132
00734          VARYING INPUT-INDEX FROM ASSNL BY -1                     EL132
00735              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE        EL132
00736                                                                   EL132
00737      MOVE AL-UABON               TO  ASSNA.                       EL132
00738                                                                   EL132
00739      ADD +1  ASSNL  GIVING  PI-KEY-LENGTH.                        EL132
00740      MOVE -1                     TO  ASSNL.                       EL132
00741                                                                      CL*13
00742      IF EIBAID = DFHPF2                                              CL*13
00743          MOVE ELRETR3-FILE-ID    TO PI-DSID                          CL*13
00744          PERFORM 3000-READ-ELRETR                                    CL*16
00745      ELSE                                                            CL*13
00746          MOVE ELMSTR3-FILE-ID    TO PI-DSID                          CL*13
00747          PERFORM 4000-READ-ELMSTR.                                   CL*16
00748                                                                   EL132
00749  0220-MAIN-LOGIC.                                                 EL132
00750      SUBTRACT +1 FROM ASSNL.                                      EL132
00751                                                                   EL132
00752  0220-MAIN-LOGIC-EXIT.                                            EL132
00753      EXIT.                                                        EL132
00754                                                                   EL132
00755      EJECT                                                        EL132
00756  0300-MAIN-LOGIC.                                                 EL132
00757 ******************************************************************EL132
00758 *           O P T I O N  4  P R O C E S S I N G                  *EL132
00759 ******************************************************************EL132
00760                                                                   EL132
00761      IF ACRTNOL GREATER ZERO  OR                                  EL132
00762         ACRTSXL GREATER ZERO                                      EL132
00763          NEXT SENTENCE                                            EL132
00764        ELSE                                                       EL132
00765          GO TO 0350-MAIN-LOGIC.                                      CL*13
00766                                                                   EL132
00767      IF ACRTNOL GREATER THAN ZERO                                    CL**7
00768          IF ACRTNOI  IS EQUAL TO  SPACES OR LOW-VALUES               CL**7
00769              MOVE ER-0284            TO  EMI-ERROR                   CL**7
00770              MOVE -1                 TO  ACRTNOL                     CL**7
00771              GO TO 8200-SEND-DATAONLY.                               CL**7
00772                                                                      CL**7
00773      MOVE '4'                    TO  PI-OPTION.                   EL132
00774                                                                   EL132
00775      IF ACRTSXL GREATER ZERO                                      EL132
00776        AND ACRTNOL NOT GREATER ZERO                               EL132
00777          MOVE ER-0210            TO  EMI-ERROR                    EL132
00778          MOVE -1                 TO  ACLAIML                         CL**7
00779          GO TO 8200-SEND-DATAONLY.                                   CL**6
00780                                                                   EL132
00781      IF ACRTNOL GREATER ZERO                                      EL132
00782          MOVE ACRTNOI            TO  PI-SC-CERT-PRIME-A4          EL132
00783                                      WS-INPUT-FIELD               EL132
00784          MOVE +11                TO PI-KEY-LENGTH.                EL132
00785                                                                   EL132
00786      IF ACRTSXL GREATER ZERO                                      EL132
00787          MOVE ACRTSXI            TO  PI-SC-CERT-SFX-A4            EL132
00788          MOVE +12                TO  PI-KEY-LENGTH.               EL132
00789                                                                   EL132
00790      MOVE PI-SC-CERT-NO-A4  TO  PI-CK-CERT-NO-A4.                 EL132
00791                                                                   EL132
00792      IF ACRTNOL GREATER ZERO                                      EL132
00793          MOVE AL-UABON           TO  ACRTNOA                      EL132
00794        ELSE                                                       EL132
00795          MOVE AL-UABOF           TO  ACRTNOA.                     EL132
00796                                                                   EL132
00797      IF ACRTSXL GREATER ZERO                                      EL132
00798          MOVE AL-UABON           TO  ACRTSXA                      EL132
00799        ELSE                                                       EL132
00800          MOVE AL-UABOF           TO  ACRTSXA.                     EL132
00801                                                                   EL132
00802      MOVE -1                     TO  ACRTNOL.                     EL132
00803                                                                      CL*13
00804      IF EIBAID = DFHPF2                                              CL*13
00805          MOVE ELRETR5-FILE-ID    TO PI-DSID                          CL*13
00806          PERFORM 3000-READ-ELRETR                                    CL*16
00807      ELSE                                                            CL*13
00808          MOVE ELMSTR5-FILE-ID    TO PI-DSID                          CL*13
00809          PERFORM 4000-READ-ELMSTR.                                   CL*16
00810                                                                      CL*13
00811      EJECT                                                           CL*13
00812  0350-MAIN-LOGIC.                                                    CL*13
00813 ******************************************************************   CL*13
00814 *           O P T I O N  5  P R O C E S S I N G                  *   CL*13
00815 ******************************************************************   CL*13
00816                                                                      CL*13
00817      IF CREDIT-CARD-INDEX  AND                                       CL*13
00818         ACCNNOL GREATER ZERO                                         CL*13
00819          NEXT SENTENCE                                               CL*13
00820        ELSE                                                          CL*13
00821          GO TO 0400-MAIN-LOGIC.                                      CL*13
00822                                                                      CL*13
00823      IF ACCNNOL GREATER ZERO                                         CL*13
00824          IF ACCNNOI = SPACES OR LOW-VALUES                           CL*13
00825              MOVE ER-0284            TO  EMI-ERROR                   CL*13
00826              MOVE -1                 TO  ACCNNOL                     CL*13
00827              GO TO 8200-SEND-DATAONLY.                               CL*13
00828                                                                      CL*13
00829      MOVE '5'                   TO  PI-OPTION.                       CL*13
00830                                                                      CL*13
00831      MOVE ACCNNOI               TO  WS-INPUT-FIELD.                  CL*13
00832      MOVE ACCNNOL               TO  PI-KEY-LENGTH.                   CL*14
00833                                                                      CL*14
00834      SET INPUT-INDEX TO +16.                                         CL*14
00835      IF WS-INPUT-CHAR (INPUT-INDEX) = SPACE                          CL*14
00836          NEXT SENTENCE                                               CL*14
00837       ELSE                                                           CL*14
00838          GO TO 0355-FULL-KEY.                                        CL*14
00839                                                                      CL*13
00840      PERFORM 0360-MAIN-LOGIC THRU 0360-MAIN-LOGIC-EXIT               CL*13
00841          VARYING INPUT-INDEX FROM ACCNNOL BY -1                      CL*13
00842              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE.          CL*14
00843                                                                      CL*13
00844      ADD +1  ACCNNOL  GIVING PI-KEY-LENGTH.                          CL*13
00845                                                                      CL*14
00846  0355-FULL-KEY.                                                      CL*14
00847      MOVE AL-UABON               TO  ACCNNOA.                        CL*14
00848      MOVE -1                     TO  ACCNNOL.                        CL*13
00849                                                                      CL*13
00850      IF WS-INPUT-CHAR (INPUT-INDEX) = '*'                            CL*13
00851          SUBTRACT 1 FROM PI-KEY-LENGTH                               CL*13
00852        ELSE                                                          CL*13
00853          MOVE +16               TO  PI-KEY-LENGTH.                   CL*13
00854                                                                      CL*13
00855      MOVE ACCNNOI (1:PI-KEY-LENGTH) TO PI-SC-CCN-NO-A5               CL*14
00856                                        PI-CK-CCN-NO-A5.              CL*14
00857                                                                      CL*13
00858      IF EIBAID = DFHPF2                                              CL*13
00859          MOVE ELRETR6-FILE-ID   TO  PI-DSID                          CL*13
00860          PERFORM 3000-READ-ELRETR                                    CL*16
00861        ELSE                                                          CL*13
00862          MOVE ELMSTR6-FILE-ID   TO  PI-DSID                          CL*13
00863          PERFORM 4000-READ-ELMSTR.                                   CL*16
00864                                                                      CL*13
00865  0360-MAIN-LOGIC.                                                    CL*13
00866      SUBTRACT +1 FROM ACCNNOL.                                       CL*13
00867                                                                      CL*13
00868  0360-MAIN-LOGIC-EXIT.                                               CL*13
00869      EXIT.                                                           CL*13
00870                                                                   EL132
00871      EJECT                                                        EL132
00872  0400-MAIN-LOGIC.                                                 EL132
00873      MOVE ZERO                   TO PI-OPTION.                    EL132
00874      MOVE +1                     TO  PI-KEY-LENGTH.               EL132
00875      MOVE -1                     TO  ACLAIML.                     EL132
00876                                                                      CL*13
00877      IF EIBAID = DFHPF2                                              CL*13
00878          MOVE ELRETR-FILE-ID    TO  PI-DSID                          CL*13
00879          PERFORM 3000-READ-ELRETR                                    CL*16
00880        ELSE                                                          CL*13
00881          MOVE ELMSTR-FILE-ID    TO  PI-DSID                          CL*13
00882          PERFORM 4000-READ-ELMSTR.                                   CL*16
00883                                                                   EL132
00884      EJECT                                                        EL132
00885  3000-READ-ELRETR SECTION.                                           CL*13
00886                                                                      CL**3
00887      EXEC CICS HANDLE CONDITION                                   EL132
00888          DUPKEY (9300-XCTL)                                       EL132
00889          NOTFND (4080-NOTFND)                                     EL132
00890      END-EXEC.                                                    EL132
00891                                                                   EL132
00892      IF (PI-DSID = ELRETR-FILE-ID AND                                CL*13
00893          PI-KEY-LENGTH LESS +19)                                  EL132
00894        OR                                                         EL132
00895         (PI-DSID = ELRETR2-FILE-ID AND                               CL*13
00896          PI-KEY-LENGTH LESS +29)                                     CL**3
00897        OR                                                         EL132
00898         (PI-DSID = ELRETR3-FILE-ID AND                               CL*13
00899          PI-KEY-LENGTH LESS +12)                                  EL132
00900        OR                                                         EL132
00901         (PI-DSID = ELRETR5-FILE-ID AND                               CL*13
00902          PI-KEY-LENGTH LESS +12)                                  EL132
00903        OR                                                            CL*13
00904         (PI-DSID = ELRETR6-FILE-ID AND                               CL*13
00905          PI-KEY-LENGTH LESS +16)                                     CL*13
00906              MOVE +1             TO  PI-START-SW                  EL132
00907              EXEC CICS READ                                       EL132
00908                  DATASET   (PI-DSID)                              EL132
00909                  RIDFLD    (PI-CLAIM-KEY)                         EL132
00910                  SET       (ADDRESS OF RETRIEVE-MASTER)              CL*13
00911                  GENERIC   EQUAL                                  EL132
00912                  KEYLENGTH (PI-KEY-LENGTH)                        EL132
00913              END-EXEC                                             EL132
00914            ELSE                                                   EL132
00915              MOVE ZERO           TO  PI-START-SW                  EL132
00916              EXEC CICS READ                                       EL132
00917                  DATASET   (PI-DSID)                              EL132
00918                  RIDFLD    (PI-CLAIM-KEY)                         EL132
00919                  SET       (ADDRESS OF RETRIEVE-MASTER)              CL*13
00920              END-EXEC.                                            EL132
00921                                                                   EL132
00922      GO TO 9300-XCTL.                                                CL*13
00923                                                                      CL*13
00924      EJECT                                                           CL*13
00925  4000-READ-ELMSTR SECTION.                                           CL*13
00926                                                                      CL*13
00927      EXEC CICS HANDLE CONDITION                                      CL*13
00928          DUPKEY (9300-XCTL)                                          CL*13
00929          NOTFND (4080-NOTFND)                                        CL*13
00930      END-EXEC.                                                       CL*13
00931                                                                      CL*13
00932      IF (PI-DSID = ELMSTR-FILE-ID AND                                CL*13
00933          PI-KEY-LENGTH LESS +19)                                     CL*13
00934        OR                                                            CL*13
00935         (PI-DSID = ELMSTR2-FILE-ID AND                               CL*13
00936          PI-KEY-LENGTH LESS +29)                                     CL*13
00937        OR                                                            CL*13
00938         (PI-DSID = ELMSTR3-FILE-ID AND                               CL*13
00939          PI-KEY-LENGTH LESS +12)                                     CL*13
00940        OR                                                            CL*13
00941         (PI-DSID = ELMSTR5-FILE-ID AND                               CL*13
00942          PI-KEY-LENGTH LESS +12)                                     CL*13
00943        OR                                                            CL*13
00944         (PI-DSID = ELMSTR6-FILE-ID AND                               CL*13
00945          PI-KEY-LENGTH LESS +16)                                     CL*13
00946              MOVE +1             TO  PI-START-SW                     CL*13
00947              EXEC CICS READ                                          CL*13
00948                  DATASET   (PI-DSID)                                 CL*13
00949                  RIDFLD    (PI-CLAIM-KEY)                            CL*13
00950                  SET       (ADDRESS OF CLAIM-MASTER)                 CL*13
00951                  GENERIC   EQUAL                                     CL*13
00952                  KEYLENGTH (PI-KEY-LENGTH)                           CL*13
00953              END-EXEC                                                CL*13
00954            ELSE                                                      CL*13
00955              MOVE ZERO           TO  PI-START-SW                     CL*13
00956              EXEC CICS READ                                          CL*13
00957                  DATASET   (PI-DSID)                                 CL*13
00958                  RIDFLD    (PI-CLAIM-KEY)                            CL*13
00959                  SET       (ADDRESS OF CLAIM-MASTER)                 CL*13
00960              END-EXEC.                                               CL*13
00961                                                                      CL*13
00962      IF PI-COMPANY-ID = 'DMD'  AND                                   CL*13
00963         PI-START-SW = ZERO     AND                                   CL*13
00964         CL-LAST-CLOSE-REASON = 'C' OR 'E'                            CL*13
00965             GO TO 4080-NOTFND.                                       CL*13
00966                                                                   EL132
00967      GO TO 9300-XCTL.                                             EL132
00968                                                                   EL132
00969  4080-NOTFND.                                                     EL132
00970      MOVE ER-0284                   TO  EMI-ERROR.                EL132
00971      GO TO 8200-SEND-DATAONLY.                                       CL**6
00972                                                                   EL132
00973                                                                   EL132
00974      EJECT                                                        EL132
00975  5000-NEXT-COMPANY SECTION.                                       EL132
00976 ******************************************************************   CL**6
00977 ****      READ CURRENT COMPANY RECORD TO OBTAIN THE NEXT      ****   CL**6
00978 ****      COMPANY ID.                                         ****   CL**6
00979 ******************************************************************   CL**6
00980                                                                   EL132
00981      MOVE PI-COMPANY-ID              TO  WS-ELCNTL-ID.               CL*13
00982      MOVE '1'                        TO  WS-ELCNTL-TYPE.             CL*13
00983      MOVE SPACES                     TO  WS-ELCNTL-USER.             CL*13
00984      MOVE +0                         TO  WS-ELCNTL-SEQ.              CL*13
00985                                                                      CL**6
00986      PERFORM 6000-READ-CONTROL THRU 6000-EXIT.                       CL**6
00987                                                                   EL132
00988      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**6
00989          MOVE ER-0022                TO  EMI-ERROR                   CL**6
00990          MOVE -1                     TO  APFKL                       CL**6
00991          GO TO 8200-SEND-DATAONLY.                                   CL**6
00992                                                                   EL132
00993      IF EIBAID = DFHPF5                                           EL132
00994          MOVE CF-NEXT-COMPANY-ID     TO  WS-NEXT-COMPANY-ID.         CL**6
00995                                                                   EL132
00996      IF EIBAID = DFHPF6                                           EL132
00997          MOVE PI-ORIGINAL-COMPANY-ID TO  WS-NEXT-COMPANY-ID.         CL**6
00998                                                                   EL132
00999      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                        EL132
01000          GO TO 5000-CONTINUE-NEXT-COMPANY.                           CL**6
01001                                                                   EL132
01002 ******************************************************************   CL**6
01003 ****      READ THE CURRENT USER RECORD FOR UPDATE AND REMOVE  ****   CL**6
01004 ****      THE TERMINAL ID FROM THE RECORD.                    ****   CL**6
01005 ******************************************************************   CL**6
01006                                                                   EL132
01007      MOVE PI-COMPANY-ID              TO  WS-ELCNTL-ID.               CL*13
01008      MOVE '2'                        TO  WS-ELCNTL-TYPE.             CL*13
01009      MOVE PI-PROCESSOR-ID            TO  WS-ELCNTL-USER.             CL*13
01010      MOVE +0                         TO  WS-ELCNTL-SEQ.              CL*13
01011                                                                   EL132
01012      PERFORM 6010-READ-CONTROL-UPDATE THRU 6010-EXIT.                CL**6
01013                                                                      CL**6
01014      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**6
01015          MOVE ER-0019                TO  EMI-ERROR                   CL**6
01016          MOVE -1                     TO  APFKL                       CL**6
01017          GO TO 8200-SEND-DATAONLY.                                   CL**6
01018                                                                      CL**6
01019      MOVE SPACES                     TO  CF-CURRENT-TERM-ON.         CL**6
01020                                                                      CL**6
01021      PERFORM 6020-REWRITE-CONTROL THRU 6020-EXIT.                    CL**6
01022                                                                      CL**6
01023 ******************************************************************   CL**6
01024 ****      READ THE USER RECORD ON THE "NEXT" COMPANY TO       ****   CL**6
01025 ****      VERIFY THAT A VALID RECORD EXISTS:                  ****   CL**6
01026 ****        1.  MOVE USER CARRIER/ACCOUNT SECURITY TO PI-AREA ****   CL**6
01027 ****        2.  MOVE USER SECURITY VALUES TO SECURITY CONTROL ****   CL**6
01028 ****            IN WORKING STORAGE.                           ****   CL**6
01029 ******************************************************************   CL**6
01030                                                                      CL**6
01031      MOVE WS-NEXT-COMPANY-ID         TO  WS-ELCNTL-ID.               CL*13
01032      MOVE '2'                        TO  WS-ELCNTL-TYPE.             CL*13
01033      MOVE PI-PROCESSOR-ID            TO  WS-ELCNTL-USER.             CL*13
01034      MOVE +0                         TO  WS-ELCNTL-SEQ.              CL*13
01035                                                                      CL**6
01036      PERFORM 6000-READ-CONTROL THRU 6000-EXIT.                       CL**6
01037                                                                      CL**6
01038      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**6
01039          MOVE ER-0228                TO  EMI-ERROR                   CL**6
01040          MOVE -1                     TO  APFKL                       CL**6
01041          GO TO 8200-SEND-DATAONLY.                                   CL**6
01042                                                                      CL**6
01043      MOVE CF-PROCESSOR-CARRIER       TO  PI-CARRIER-SECURITY.        CL**6
01044      MOVE CF-PROCESSOR-ACCOUNT       TO  PI-ACCOUNT-SECURITY.        CL**6
01045      MOVE CF-INDIVIDUAL-APP (1)      TO  SC-CREDIT-CODES.            CL**6
01046      MOVE CF-INDIVIDUAL-APP (2)      TO  SC-CLAIMS-CODES.            CL**6
01047                                                                      CL**6
01048 ******************************************************************   CL**6
01049 ****      READ THE USER RECORD FOR UPDATE AND MOVE THE        ****   CL**6
01050 ****      TERMINAL ID TO THE RECORD.                          ****   CL**6
01051 ******************************************************************   CL**6
01052                                                                      CL**6
01053      MOVE WS-NEXT-COMPANY-ID         TO  WS-ELCNTL-ID.               CL*13
01054      MOVE '2'                        TO  WS-ELCNTL-TYPE.             CL*13
01055      MOVE PI-PROCESSOR-ID            TO  WS-ELCNTL-USER.             CL*13
01056      MOVE +0                         TO  WS-ELCNTL-SEQ.              CL*13
01057                                                                      CL**6
01058      PERFORM 6010-READ-CONTROL-UPDATE THRU 6010-EXIT.                CL**6
01059                                                                      CL**6
01060      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**6
01061          MOVE ER-0228                TO  EMI-ERROR                   CL**6
01062          MOVE -1                     TO  APFKL                       CL**6
01063          GO TO 8200-SEND-DATAONLY.                                   CL**6
01064                                                                      CL**6
01065      MOVE EIBTRMID                   TO  CF-CURRENT-TERM-ON.         CL**6
01066                                                                      CL**6
01067      PERFORM 6020-REWRITE-CONTROL THRU 6020-EXIT.                    CL**6
01068                                                                      CL**6
01069  5000-CONTINUE-NEXT-COMPANY.                                         CL**6
01070 ******************************************************************   CL**6
01071 ****      READ THE NEW COMPANY RECORD TO VERIFY THAT IT       ****   CL**6
01072 ****      EXISTS AND THEN MOVE SPECIFIC DATA TO THE PI-AREA.  ****   CL**6
01073 ******************************************************************   CL**6
01074                                                                      CL**6
01075      MOVE WS-NEXT-COMPANY-ID         TO  WS-ELCNTL-ID.               CL*13
01076      MOVE '1'                        TO  WS-ELCNTL-TYPE.             CL*13
01077      MOVE SPACES                     TO  WS-ELCNTL-USER.             CL*13
01078      MOVE +0                         TO  WS-ELCNTL-SEQ.              CL*13
01079                                                                      CL**6
01080      PERFORM 6000-READ-CONTROL THRU 6000-EXIT.                       CL**6
01081                                                                      CL**6
01082      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                         CL**6
01083          MOVE ER-0089                TO  EMI-ERROR                   CL**6
01084          MOVE -1                     TO  APFKL                       CL**6
01085          GO TO 8200-SEND-DATAONLY.                                   CL**6
01086                                                                   EL132
01087      MOVE CF-COMPANY-CD            TO  PI-COMPANY-CD.             EL132
01088      MOVE CF-COMPANY-ID            TO  PI-COMPANY-ID.             EL132
01089      MOVE CF-COMPANY-PASSWORD      TO  PI-COMPANY-PASSWORD.          CL**6
01090      MOVE CF-LGX-CREDIT-USER       TO  PI-CREDIT-USER.            EL132
01091      MOVE CF-LGX-CLAIM-USER        TO  PI-CLAIM-USER.             EL132
01092      MOVE CF-CERT-ACCESS-CONTROL   TO  PI-CERT-ACCESS-CONTROL.    EL132
01093      MOVE CF-CARRIER-CONTROL-LEVEL TO  PI-CARRIER-CONTROL-LEVEL.  EL132
01094      MOVE CF-JOURNAL-FILE-ID       TO  PI-JOURNAL-FILE-ID.        EL132
01095      MOVE CF-LOWER-CASE-LETTERS    TO  PI-LOWER-CASE-LETTERS.        CL**6
01096      MOVE CF-CLAIM-PAID-THRU-TO    TO  PI-CLAIM-PAID-THRU-TO.        CL**6
01097                                                                   EL132
01098      MOVE CF-LIFE-OVERRIDE-L1      TO  PI-LIFE-OVERRIDE-L1.          CL**6
01099      MOVE CF-LIFE-OVERRIDE-L2      TO  PI-LIFE-OVERRIDE-L2.          CL**6
01100      MOVE CF-LIFE-OVERRIDE-L6      TO  PI-LIFE-OVERRIDE-L6.          CL**6
01101      MOVE CF-LIFE-OVERRIDE-L12     TO  PI-LIFE-OVERRIDE-L12.         CL**6
01102                                                                   EL132
01103      MOVE CF-AH-OVERRIDE-L1        TO  PI-AH-OVERRIDE-L1.            CL**6
01104      MOVE CF-AH-OVERRIDE-L2        TO  PI-AH-OVERRIDE-L2.            CL**6
01105      MOVE CF-AH-OVERRIDE-L6        TO  PI-AH-OVERRIDE-L6.            CL**6
01106      MOVE CF-AH-OVERRIDE-L12       TO  PI-AH-OVERRIDE-L12.           CL**6
01107                                                                      CL**9
01108      IF  CLAIM-SESSION                                               CL**9
01109          MOVE CF-PRINT-ADDRESS-LABELS                                CL**9
01110                                    TO  PI-LABEL-CONTROL              CL**9
01111      ELSE                                                            CL**9
01112          IF  CREDIT-SESSION                                          CL**9
01113              MOVE CF-CR-PRINT-ADDRESS-LABELS                         CL**9
01114                                    TO  PI-LABEL-CONTROL.             CL**9
01115                                                                   EL132
01116  5000-EXIT.                                                       EL132
01117      EXIT.                                                        EL132
01118                                                                      CL**6
01119      EJECT                                                        EL132
01120  5500-WRITE-SECURITY-TEMP-STORE  SECTION.                         EL132
01121                                                                   EL132
01122      EXEC CICS HANDLE CONDITION                                   EL132
01123          QIDERR   (5501-WRITE-SECURITY)                           EL132
01124      END-EXEC.                                                    EL132
01125                                                                   EL132
01126      MOVE EIBTRMID                 TO  QID.                       EL132
01127                                                                   EL132
01128      EXEC CICS DELETEQ TS                                         EL132
01129          QUEUE   (QID)                                            EL132
01130      END-EXEC.                                                    EL132
01131                                                                   EL132
01132  5501-WRITE-SECURITY.                                             EL132
01133                                                                   EL132
01134      EXEC CICS WRITEQ TS                                          EL132
01135          QUEUE   (QID)                                            EL132
01136          FROM    (SECURITY-CONTROL)                               EL132
01137          LENGTH  (SC-COMM-LENGTH)                                 EL132
01138          ITEM    (QID-ITEM)                                       EL132
01139      END-EXEC.                                                    EL132
01140                                                                   EL132
01141      MOVE QID                      TO  PI-SECURITY-TEMP-STORE-ID. EL132
01142                                                                   EL132
01143      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                        EL132
01144          MOVE ALL 'Y'              TO  SC-CREDIT-CODES            EL132
01145                                        SC-CLAIMS-CODES            EL132
01146                                        PI-PROCESSOR-USER-ALMIGHTY.EL132
01147                                                                   EL132
01148  5500-EXIT.                                                       EL132
01149      EXIT.                                                        EL132
01150                                                                   EL132
01151      EJECT                                                        EL132
01152  6000-READ-CONTROL SECTION.                                       EL132
01153      EXEC CICS HANDLE CONDITION                                   EL132
01154           NOTFND (6000-NOT-FOUND)                                 EL132
01155      END-EXEC.                                                    EL132
01156                                                                   EL132
01157      EXEC CICS READ                                               EL132
01158           DATASET  (ELCNTL-FILE-ID)                                  CL**6
01159           RIDFLD   (WS-ELCNTL-KEY)                                   CL*13
01160           SET      (ADDRESS OF CONTROL-FILE)                         CL*13
01161      END-EXEC.                                                    EL132
01162                                                                   EL132
01163      MOVE 'Y'                        TO  WS-CNTL-REC-FOUND-SW.       CL**6
01164      GO TO 6000-EXIT.                                             EL132
01165                                                                   EL132
01166  6000-NOT-FOUND.                                                  EL132
01167                                                                      CL**6
01168      MOVE 'N'                        TO  WS-CNTL-REC-FOUND-SW.       CL**6
01169                                                                   EL132
01170  6000-EXIT.                                                       EL132
01171      EXIT.                                                        EL132
01172                                                                      CL**6
01173  6010-READ-CONTROL-UPDATE.                                           CL**6
01174                                                                      CL**6
01175      EXEC CICS HANDLE CONDITION                                      CL**6
01176          NOTFND   (6010-NOTFND)                                      CL**6
01177      END-EXEC.                                                       CL**6
01178                                                                      CL**6
01179      EXEC CICS READ                                                  CL**6
01180          DATASET   (ELCNTL-FILE-ID)                                  CL**6
01181          RIDFLD    (WS-ELCNTL-KEY)                                   CL*13
01182          SET       (ADDRESS OF CONTROL-FILE)                         CL*13
01183          UPDATE                                                      CL**6
01184      END-EXEC.                                                       CL**6
01185                                                                      CL**6
01186      MOVE 'Y'                        TO  WS-CNTL-REC-FOUND-SW.       CL**6
01187      GO TO 6010-EXIT.                                                CL**6
01188                                                                      CL**6
01189  6010-NOTFND.                                                        CL**6
01190                                                                      CL**6
01191      MOVE 'N'                        TO  WS-CNTL-REC-FOUND-SW.       CL**6
01192                                                                      CL**6
01193  6010-EXIT.                                                          CL**6
01194      EXIT.                                                           CL**6
01195                                                                      CL**6
01196  6020-REWRITE-CONTROL.                                               CL**6
01197                                                                      CL**6
01198      EXEC CICS REWRITE                                               CL**6
01199          DATASET   (ELCNTL-FILE-ID)                                  CL**6
01200          FROM      (CONTROL-FILE)                                    CL**6
01201      END-EXEC.                                                       CL**6
01202                                                                      CL**6
01203  6020-EXIT.                                                          CL**6
01204      EXIT.                                                           CL**6
01205                                                                      CL**6
01206      EJECT                                                        EL132
01207  7000-BUILD-SCREEN     SECTION.                                   EL132
01208 ******************************************************************EL132
01209 *          REBUILD ORIGNAL SCREEN AND ERROR MESSAGE IF EL1322    *EL132
01210 *          DID NOT FIND ANY CLAIM RECORDS DURING BROWSE OF FILE. *EL132
01211 ******************************************************************EL132
01212                                                                   EL132
01213      IF EIBTRNID = WS-TRANS-ID                                    EL132
01214          NEXT SENTENCE                                            EL132
01215        ELSE                                                       EL132
01216          GO TO 7099-EXIT.                                         EL132
01217                                                                   EL132
01218      IF  PI-BROWSE-SW = +9                                        EL132
01219          NEXT SENTENCE                                            EL132
01220         ELSE                                                      EL132
01221          GO TO 7099-EXIT.                                         EL132
01222                                                                   EL132
01223      MOVE LOW-VALUES             TO  EL132AO.                     EL132
01224                                                                   EL132
01225      MOVE ER-2374                TO  EMI-ERROR.                   EL132
01226                                                                   EL132
01227      IF OPTION-ONE-SELECTED                                       EL132
01228         GO TO 7099-EXIT.                                          EL132
01229                                                                   EL132
01230      IF OPTION-TWO-SELECTED                                       EL132
01231         NEXT SENTENCE                                             EL132
01232        ELSE                                                       EL132
01233         GO TO 7030-OPTION-THREE.                                  EL132
01234                                                                   EL132
01235  7020-OPTION-TWO.                                                 EL132
01236                                                                      CL**3
01237      IF  PI-SC-LAST-NAME GREATER SPACES                           EL132
01238          MOVE PI-SC-LAST-NAME TO ALNAMEO                          EL132
01239          MOVE AL-UANON        TO ALNAMEA                          EL132
01240          IF  PI-SC-INITIALS  GREATER SPACES                       EL132
01241              MOVE PI-SC-FIRST-NAME TO AFNAMEO                        CL**3
01242              MOVE PI-SC-INITIAL2  TO AMINITO                         CL**3
01243              MOVE AL-UANON        TO AFNAMEA                         CL**3
01244              MOVE AL-UANON        TO AMINITA.                        CL**3
01245                                                                      CL**3
01246      IF PI-ACCOUNT-NUMBER NOT EQUAL SPACES                           CL**3
01247         MOVE PI-ACCOUNT-NUMBER    TO AACCTO                          CL**3
01248         MOVE AL-UANON             TO AACCTA.                         CL**3
01249                                                                   EL132
01250      GO TO   7090-INITIALIZE-WORK-AREAS.                          EL132
01251                                                                   EL132
01252  7030-OPTION-THREE.                                               EL132
01253      IF OPTION-THREE-SELECTED                                     EL132
01254         NEXT SENTENCE                                             EL132
01255        ELSE                                                       EL132
01256         GO TO 7040-OPTION-FOUR.                                   EL132
01257                                                                   EL132
01258      MOVE -1                          TO ASSNL.                   EL132
01259      IF PI-SC-SOC-SEC-NO GREATER SPACES                           EL132
01260         MOVE PI-SC-SOC-SEC-NO         TO ASSNO                    EL132
01261         MOVE AL-UANON                 TO ASSNA.                   EL132
01262                                                                   EL132
01263      GO TO   7090-INITIALIZE-WORK-AREAS.                          EL132
01264                                                                   EL132
01265  7040-OPTION-FOUR.                                                EL132
01266      IF OPTION-FOUR-SELECTED                                         CL*13
01267         NEXT SENTENCE                                                CL*13
01268        ELSE                                                          CL*13
01269         GO TO 7050-OPTION-FIVE.                                      CL*13
01270                                                                      CL*13
01271       MOVE -1                    TO ACRTNOL.                      EL132
01272                                                                   EL132
01273       IF  PI-SC-CERT-PRIME-A4 GREATER SPACES                      EL132
01274           MOVE PI-SC-CERT-PRIME-A4  TO  ACRTNOO                   EL132
01275           MOVE AL-UANON             TO  ACRTNOA.                  EL132
01276                                                                      CL*13
01277       IF  PI-SC-CERT-SFX-A4   GREATER SPACES                      EL132
01278           MOVE PI-SC-CERT-SFX-A4    TO  ACRTSXO                   EL132
01279           MOVE AL-UANON             TO  ACRTSXA.                  EL132
01280                                                                   EL132
01281      GO TO 7090-INITIALIZE-WORK-AREAS.                               CL*13
01282                                                                      CL*13
01283  7050-OPTION-FIVE.                                                   CL*13
01284       MOVE -1                    TO ACCNNOL.                         CL*13
01285                                                                      CL*13
01286       IF PI-SC-CCN-NO-A5 GREATER SPACES                              CL*13
01287           MOVE PI-SC-CCN-NO-A5   TO  ACCNNOO                         CL*13
01288           MOVE AL-UANON          TO  ACCNNOA.                        CL*13
01289                                                                   EL132
01290  7090-INITIALIZE-WORK-AREAS.                                      EL132
01291      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.        EL132
01292                                                                   EL132
01293      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL132
01294                                      PI-LINE-COUNT                EL132
01295                                      PI-AIX-RECORD-COUNT          EL132
01296                                      PI-BROWSE-SW                 EL132
01297                                      PI-KEY-LENGTH                EL132
01298                                      PI-TS-ITEM                   EL132
01299                                      PI-END-OF-FILE               EL132
01300                                      PI-START-SW.                 EL132
01301                                                                   EL132
01302      GO TO 8100-SEND-INITIAL-MAP.                                    CL**6
01303                                                                   EL132
01304  7099-EXIT.                                                       EL132
01305      EXIT.                                                        EL132
01306                                                                   EL132
01307      EJECT                                                        EL132
01308  8100-SEND-INITIAL-MAP SECTION.                                   EL132
01309                                                                      CL**6
01310      IF CREDIT-CARD-INDEX                                            CL*13
01311          MOVE AL-SANOF           TO  ACCNH1A                         CL*13
01312                                      ACCNH2A                         CL*13
01313          MOVE AL-UANOF           TO  ACCNNOA                         CL*13
01314       ELSE                                                           CL*13
01315          MOVE AL-SADOF           TO  ACCNH1A                         CL*13
01316                                      ACCNH2A                         CL*13
01317          MOVE AL-SADOF           TO  ACCNNOA.                        CL*13
01318                                                                      CL**6
01319      IF SOC-SEC-NO-USED                                           EL132
01320         MOVE AL-SANOF TO AOPT3A ASSOPTA                           EL132
01321         MOVE AL-UANOF TO ASSNA                                    EL132
01322      ELSE                                                         EL132
01323         MOVE AL-SANOF TO AOPT3A ASSOPTA ASSNA.                    EL132
01324                                                                   EL132
01325                                                                   EL132
01326      IF ACRTNOL NOT = -1                                          EL132
01327          MOVE -1                 TO  ACLAIML.                     EL132
01328                                                                   EL132
01329      MOVE SAVE-DATE              TO  ADATEO.                      EL132
01330      MOVE EIBTIME                TO  TIME-IN.                     EL132
01331      MOVE TIME-OUT               TO  ATIMEO.                      EL132
101501     MOVE PI-COMPANY-ID          TO  ACOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  AUSERIDO.
01332                                                                   EL132
01333      IF PI-RETURN-TO-PROGRAM = 'EL126' OR 'EL150'                    CL*13
01334          MOVE '       CLAIM LOOK-UP FOR STATUS  '                    CL*13
01335                                  TO  AHEAD1O                      EL132
01336        ELSE                                                       EL132
01337          IF PI-RETURN-TO-PROGRAM = 'EL162'                           CL*13
01338              MOVE '   CLAIM LOOK-UP FOR MAIL RECORDING   '        EL132
01339                                  TO  AHEAD1O                      EL132
01340            ELSE                                                   EL132
01341              IF PI-RETURN-TO-PROGRAM = 'EL130'                       CL*13
01342                  MOVE '  CLAIM LOOK-UP FROM NEW CLAIM SETUP  '    EL132
01343                                  TO  AHEAD1O.                     EL132
01344                                                                   EL132
01345      IF EMI-ERROR NOT = ZERO                                      EL132
01346          PERFORM 9900-ERROR-FORMAT.                               EL132
01347                                                                   EL132
01348      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                    EL132
01349      MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.                    EL132
01350                                                                   EL132
101501*    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                       EL132
101501*       MOVE PI-COMPANY-ID TO ACOMPO                              EL132
101501*       MOVE AL-PANOF      TO APFK5A                              EL132
101501*       MOVE AL-PANOF      TO APFK6A                              EL132
101501*    ELSE                                                         EL132
101501*       MOVE SPACES        TO ACOMPO                              EL132
101501*       MOVE AL-PADOF      TO APFK5A                              EL132
101501*       MOVE AL-PADOF      TO APFK6A.                             EL132
01359                                                                   EL132
01360      EXEC CICS SEND                                               EL132
01361          FROM   (EL132AO)                                         EL132
01362          MAPSET (WS-MAPSET-NAME)                                  EL132
01363          MAP    (WS-MAP-NAME)                                     EL132
01364          CURSOR ERASE                                             EL132
01365      END-EXEC.                                                    EL132
01366                                                                   EL132
01367      GO TO 9100-RETURN-TRAN.                                         CL**6
01368                                                                      CL**6
01369  8100-EXIT.                                                       EL132
01370      EXIT.                                                        EL132
01371                                                                   EL132
01372      EJECT                                                        EL132
01373  8200-SEND-DATAONLY SECTION.                                      EL132
01374      MOVE SAVE-DATE              TO  ADATEO.                      EL132
01375      MOVE EIBTIME                TO  TIME-IN.                     EL132
01376      MOVE TIME-OUT               TO  ATIMEO.                      EL132
101501     MOVE PI-COMPANY-ID          TO  ACOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  AUSERIDO.
01377                                                                   EL132
01378      IF EMI-ERROR NOT = ZERO                                      EL132
01379          PERFORM 9900-ERROR-FORMAT.                               EL132
01380                                                                   EL132
01381      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                    EL132
01382      MOVE EMI-MESSAGE-AREA (2)    TO  AEMSG2O.                    EL132
01383                                                                   EL132
101501*    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                       EL132
101501*       MOVE PI-COMPANY-ID TO ACOMPO                              EL132
101501*       MOVE AL-PANOF      TO APFK5A                              EL132
101501*                             APFK6A                              EL1CL*13
101501*    ELSE                                                         EL132
101501*       MOVE SPACES        TO ACOMPO                              EL132
101501*       MOVE AL-PADOF      TO APFK5A                              EL132
101501*                             APFK6A.                                CL*13
01392                                                                   EL132
01393      EXEC CICS SEND DATAONLY                                      EL132
01394          FROM   (EL132AO)                                         EL132
01395          MAPSET (WS-MAPSET-NAME)                                  EL132
01396          MAP    (WS-MAP-NAME)                                     EL132
01397          CURSOR                                                   EL132
01398      END-EXEC.                                                    EL132
01399                                                                      CL**6
01400      GO TO 9100-RETURN-TRAN.                                         CL**6
01401                                                                   EL132
01402  8200-EXIT.                                                       EL132
01403      EXIT.                                                        EL132
01404                                                                   EL132
01405      EJECT                                                        EL132
01406  8300-SEND-TEXT SECTION.                                          EL132
01407      EXEC CICS SEND TEXT                                          EL132
01408          FROM   (LOGOFF-TEXT)                                     EL132
01409          LENGTH (LOGOFF-LENGTH)                                   EL132
01410          ERASE  FREEKB                                            EL132
01411      END-EXEC.                                                    EL132
01412                                                                   EL132
01413      EXEC CICS RETURN                                             EL132
01414      END-EXEC.                                                    EL132
01415                                                                   EL132
01416  8300-EXIT.                                                       EL132
01417      EXIT.                                                        EL132
01418                                                                   EL132
01419  8500-DATE-CONVERSION SECTION.                                    EL132
01420      EXEC CICS LINK                                               EL132
01421          PROGRAM  ('ELDATCV')                                     EL132
01422          COMMAREA (DATE-CONVERSION-DATA)                          EL132
01423          LENGTH   (DC-COMM-LENGTH)                                EL132
01424      END-EXEC.                                                    EL132
01425                                                                   EL132
01426  8500-EXIT.                                                       EL132
01427      EXIT.                                                        EL132
01428                                                                   EL132
01429      EJECT                                                        EL132
01430  9000-RETURN-CICS SECTION.                                        EL132
01431      MOVE 'EL005'                TO  THIS-PGM.                    EL132
01432      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL132
01433      PERFORM 9300-XCTL.                                           EL132
01434                                                                   EL132
01435  9000-EXIT.                                                       EL132
01436      EXIT.                                                        EL132
01437                                                                   EL132
01438  9100-RETURN-TRAN SECTION.                                        EL132
01439      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL132
01440      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL132
01441                                                                   EL132
01442      EXEC CICS RETURN                                             EL132
01443          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL132
01444          LENGTH   (PI-COMM-LENGTH)                                EL132
01445          TRANSID  (WS-TRANS-ID)                                   EL132
01446      END-EXEC.                                                    EL132
01447                                                                   EL132
01448  9100-EXIT.                                                       EL132
01449      EXIT.                                                        EL132
01450                                                                   EL132
01451  9300-XCTL SECTION.                                               EL132
01452      MOVE DFHENTER               TO  EIBAID.                      EL132
01453                                                                   EL132
01454      EXEC CICS XCTL                                               EL132
01455          PROGRAM  (THIS-PGM)                                      EL132
01456          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL132
01457          LENGTH   (PI-COMM-LENGTH)                                EL132
01458      END-EXEC.                                                    EL132
01459                                                                   EL132
01460  9300-EXIT.                                                       EL132
01461      EXIT.                                                        EL132
01462                                                                   EL132
01463      EJECT                                                        EL132
01464  9400-CLEAR SECTION.                                              EL132
01465      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM                      EL132
01466      PERFORM 9300-XCTL.                                           EL132
01467                                                                   EL132
01468  9400-EXIT.                                                       EL132
01469      EXIT.                                                        EL132
01470                                                                   EL132
01471  9600-PGMIDERR SECTION.                                           EL132
01472      EXEC CICS HANDLE CONDITION                                   EL132
01473          PGMIDERR (8300-SEND-TEXT)                                EL132
01474      END-EXEC.                                                    EL132
01475                                                                   EL132
01476      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM.          EL132
01477                                                                   EL132
01478      MOVE 'EL005'                TO  THIS-PGM                     EL132
01479                                      LOGOFF-PGM.                  EL132
01480      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL132
01481      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL132
01482      PERFORM 9300-XCTL.                                           EL132
01483                                                                   EL132
01484  9600-EXIT.                                                       EL132
01485      EXIT.                                                        EL132
01486                                                                   EL132
01487      EJECT                                                        EL132
01488  9900-ERROR-FORMAT SECTION.                                       EL132
01489      EXEC CICS LINK                                               EL132
01490          PROGRAM  ('EL001')                                       EL132
01491          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL132
01492          LENGTH   (EMI-COMM-LENGTH)                               EL132
01493      END-EXEC.                                                    EL132
01494                                                                   EL132
01495      MOVE ER-0000                   TO  EMI-ERROR.                EL132
01496                                                                   EL132
01497  9900-EXIT.                                                       EL132
01498      EXIT.                                                        EL132
01499                                                                   EL132
01500      EJECT                                                        EL132
01501  9990-ERROR SECTION.                                              EL132
01502      MOVE DFHEIBLK TO EMI-LINE1.                                  EL132
01503      EXEC CICS LINK                                               EL132
01504          PROGRAM  ('EL004')                                       EL132
01505          COMMAREA (EMI-LINE1)                                     EL132
01506          LENGTH   (72)                                            EL132
01507      END-EXEC.                                                    EL132
01508                                                                   EL132
01509      GO TO 8200-SEND-DATAONLY.                                       CL**6
01510                                                                   EL132
01511  9990-EXIT.                                                       EL132
01512      EXIT.                                                        EL132
01513                                                                   EL132
01514  9995-SECURITY-VIOLATION.                                         EL132
01515             COPY ELCSCTP.                                         EL132
01516                                                                   EL132
01517  9995-EXIT.                                                       EL132
01518       EXIT.                                                       EL132
01519                                                                   EL132
