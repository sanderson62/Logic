00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   EL1325
00003  PROGRAM-ID.                 EL1325.                                 LV004
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/13/96 09:49:45.                    CL**3
00007 *                            VMOD=2.003.                             CL**3
00008 *                                                                 EL1325
00008 *                                                                 EL1325
00009 *AUTHOR.    LOGIC, INC.                                              CL**3
00010 *           DALLAS, TEXAS.                                           CL**3
00011                                                                   EL1325
00012 *DATE-COMPILED.                                                      CL**3
00013                                                                   EL1325
00014 *SECURITY.   *****************************************************   CL**3
00015 *            *                                                   *   CL**3
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00017 *            *                                                   *   CL**3
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00021 *            *                                                   *   CL**3
00022 *            *****************************************************   CL**3
00023                                                                   EL1325
00024 *REMARKS.                                                         EL1325
00025                                                                   EL1325
00026 *        THIS PROGRAM PROVIDES ALLOWS AN OPERATOR DIRECT ACCESS   EL1325
00027 *    TO A CLAIM RECORD OR PERMITS A BROWSE OF THE CLAIM FILE.     EL1325
00028                                                                   EL1325
00029 *    SCREENS     - EL1324 - ALPHA LOOK-UP MATCH LIST              EL1325
00030                                                                   EL1325
00031 *    ENTERED BY  - EL126 - MAINTENANCE MENU                       EL1325
00032 *                  EL130 - NEW CLAIM SET-UP                       EL1325
00033 *                  EL150 - STATUS DISPLAY (ENTERS EL1325 ONLY)    EL1325
00034                                                                   EL1325
00035 *    EXIT TO     - CALLING PROGRAM                                EL1325
00036 *                  EL1324 - ALPHA INDEXT LOOK-UP                  EL1325
00037                                                                   EL1325
00038 *    INPUT FILE  - ELMSTR - CLAIM MASTER FILE                     EL1325
00039 *                  ELALPH - ALPHA INDEX FILE                      EL1325
00040                                                                   EL1325
00041 *    OUTPUT FILE - NONE                                           EL1325
00042                                                                   EL1325
00043 *    COMMAREA    - PASSED.  IF A CLAIM IS SELECTED, THE           EL1325
00044 *                  CONTROL OF THAT CLAIM IS PLACED IN THE         EL1325
00045 *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR        EL1325
00046 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM EL1325
00047 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE  EL1325
00048 *                  RECORD KEY INFORMATION NEEDED BY EL1324 TO     EL1325
00049 *                  LOCATE THE CLAIM.                              EL1325
00050                                                                   EL1325
00051                                                                   EL1325
00052 *    NARRATIVE  - USING THE CONTROL INFORMATION PASSED FROM       EL1325
00053 *                 EL1324, START A BROWSE ON THE CLAIM MASTER FILE EL1325
00054 *                 IN AN ATTEMPT TO FIND THE RECORDS INDICATED.    EL1325
00055 *                 IF MORE THAN 16 MATCHES ARE FOUND, THE SCREEN   EL1325
00056 *                 WILL SHOW THE FIRST 16, AND WAIT FOR THE        EL1325
00057 *                 OPERATOR TO CONTINUE THE BROWSE.  IF ONLY ONE   EL1325
00058 *                 ENTRY IS FOUND PASS CONTROL TO THE APPROPRIATE  EL1325
00059 *                 PROGRAM.                                        EL1325
101201******************************************************************
101201*                   C H A N G E   L O G
101201*
101201* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101201*-----------------------------------------------------------------
101201*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101201* EFFECTIVE    NUMBER
101201*-----------------------------------------------------------------
101201* 101201    2001100100006  SMVA  ADD USERID TO SCREEN HEADER
101201*                              ADJUSTED REDEFINES EL1325AI FILLER
101201******************************************************************

00060                                                                   EL1325
00061                                                                   EL1325
00062      EJECT                                                        EL1325
00063  ENVIRONMENT DIVISION.                                            EL1325
00064                                                                   EL1325
00065  DATA DIVISION.                                                   EL1325
00066                                                                   EL1325
00067  WORKING-STORAGE SECTION.                                         EL1325
00068  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.      CL**3
00069                                                                   EL1325
00070  77  FILLER  PIC X(32)  VALUE '********************************'. EL1325
00071  77  FILLER  PIC X(32)  VALUE '*   EL1325 WORKING STORAGE     *'. EL1325
00072  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.003 ************'.    CL**3
00073                                                                   EL1325
00074                                  COPY ELCSCTM.                    EL1325
00075                                                                   EL1325
00076                                  COPY ELCSCRTY.                   EL1325
00077                                                                   EL1325
00078  01  WS-DATE-AREA.                                                EL1325
00079      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL1325
00080      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL1325
00081                                                                   EL1325
00082  01  ERROR-MESSAGES.                                              EL1325
00083      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL1325
00084      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL1325
00085      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL1325
00086      12  ER-0019                 PIC X(4)  VALUE '0019'.          EL1325
00087      12  ER-0022                 PIC X(4)  VALUE '0022'.          EL1325
00088      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL1325
00089      12  ER-0031                 PIC X(4)  VALUE '0031'.          EL1325
00090      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL1325
00091      12  ER-0089                 PIC X(4)  VALUE '0089'.          EL1325
00092      12  ER-0130                 PIC X(4)  VALUE '0130'.          EL1325
00093      12  ER-0200                 PIC X(4)  VALUE '0200'.          EL1325
00094      12  ER-0228                 PIC X(4)  VALUE '0228'.          EL1325
00095      12  ER-0284                 PIC X(4)  VALUE '0284'.          EL1325
00096                                                                   EL1325
00097  01  FILLER                          COMP-3.                      EL1325
00098                                                                   EL1325
00099      05  WS-RECORD-COUNT         PIC S9(3)   VALUE ZERO.          EL1325
00100      05  WS-READNEXT-SW          PIC S9      VALUE ZERO.          EL1325
00101      05  WS-NOT-FOUND            PIC S9      VALUE ZERO.          EL1325
00102      05  WS-ERROR-NUMBER         PIC S9(3)   VALUE ZERO.          EL1325
00103      05  WS-ERROR-COUNT          PIC S9(3)   VALUE ZERO.          EL1325
00104      05  WS-LAST-ERROR-COUNT     PIC S9(3)   VALUE ZERO.          EL1325
00105      05  WS-UPDATE-SW            PIC S9      VALUE ZERO.          EL1325
00106      05  WS-COMPLETED-SUCCESSFUL PIC S9      VALUE ZERO.          EL1325
00107          88  TRANSACTION-SUCCESSFUL          VALUE +1.            EL1325
00108                                                                   EL1325
00109      05  TIME-IN                 PIC S9(7)   VALUE ZERO.          EL1325
00110      05  TIME-OUT         REDEFINES                               EL1325
00111          TIME-IN                 PIC S9(3)V9(4).                  EL1325
00112                                                                   EL1325
00113      05  WS-MONTH-WORK           PIC S9(3)   VALUE ZERO.          EL1325
00114      05  WS-YEAR-WORK            PIC S9(3)   VALUE ZERO.          EL1325
00115      05  WS-AIX-RECORD-COUNT     PIC S9(5)   VALUE ZERO.          EL1325
00116      05  WS-CLAIMS-SW            PIC S9      VALUE ZERO.          EL1325
00117          88  WS-NO-CLAIMS-FOUND              VALUE ZERO.          EL1325
00118  01  FILLER            COMP SYNCHRONIZED.                         EL1325
00119                                                                   EL1325
00120      05  WS-INDEX                PIC S9(4)   VALUE ZERO.          EL1325
00121                                                                   EL1325
00122      05  WS-JOURNAL-FILE-ID      PIC S9(4)   VALUE +1.            EL1325
00123      05  WS-JOURNAL-RECORD-LENGTH PIC S9(4)  VALUE +527.          EL1325
00124      05  SC-ITEM                 PIC S9(4)   VALUE +0001.         EL1325
00125                                                                   EL1325
00126                                                                   EL1325
00127  01  FILLER.                                                      EL1325
00128                                                                   EL1325
00129      05  QID.                                                     EL1325
00130          10  QID-TERM            PIC X(4).                        EL1325
00131          10  FILLER              PIC X(4)  VALUE '132E'.          EL1325
00132      05  QID-PROC-AREA           PIC XXX.                         EL1325
00133      05  QID-LENGTH              PIC S9(4) VALUE +3  COMP.        EL1325
00134      05  QID-ITEM                PIC S9(4) VALUE +1  COMP.        EL1325
00135                                                                   EL1325
00136      05  WS-CLCNTL-KEY.                                           EL1325
00137          10  WS-CLCNTL-ID        PIC X(3).                        EL1325
00138          10  WS-CLCNTL-TYPE      PIC X.                           EL1325
00139          10  WS-CLCNTL-USER      PIC X(04) VALUE SPACES.          EL1325
00140          10  WS-CLCNTL-SEQ       PIC S9(4) VALUE +0      COMP.    EL1325
00141                                                                   EL1325
00142      05  WS-MAPSET-NAME          PIC X(8)  VALUE 'EL1325S '.      EL1325
00143      05  WS-MAP-NAME             PIC X(8)  VALUE 'EL1325A '.      EL1325
00144                                                                   EL1325
00145      05  FILLER           REDEFINES                               EL1325
00146          WS-MAP-NAME.                                             EL1325
00147          10  FILLER              PIC XX.                          EL1325
00148          10  WS-MAP-NUMBER       PIC X(4).                        EL1325
00149          10  FILLER              PIC XX.                             CL**3
00150                                                                   EL1325
00151      05  THIS-PGM                PIC X(08)   VALUE 'EL1325'.      EL1325
00152                                                                   EL1325
00153      05  ALPHA-INDEX-DSID        PIC X(08)   VALUE 'ELALPH'.      EL1325
00154      05  ELMSTR-DSID             PIC X(08)   VALUE 'ELMSTR'.      EL1325
00155      05  ELCNTL-FILE-ID          PIC X(08)   VALUE 'ELCNTL'.      EL1325
00156      05  WS-CNTL-REC-FOUND-SW    PIC X(01)   VALUE SPACES.        EL1325
00157      05  WS-NEXT-COMPANY-ID      PIC X(03)   VALUE SPACES.        EL1325
00158                                                                   EL1325
00159      05  WS-TRANS-ID             PIC X(4)    VALUE 'E034'.        EL1325
00160                                                                   EL1325
00161      05  WS-TEMP-STORAGE-KEY.                                     EL1325
00162          10  WS-TSK-TERM-ID      PIC X(4)    VALUE 'XXXX'.        EL1325
00163          10  FILLER              PIC X(4)    VALUE '1325'.        EL1325
00164                                                                   EL1325
00165      05  WS-TS-LENGTH            PIC S9(4)   VALUE +1920   COMP   EL1325
00166                                      SYNCHRONIZED.                EL1325
00167                                                                   EL1325
00168      05  WS-CURRENT-DATE         PIC XX VALUE LOW-VALUES.         EL1325
00169      05  WS-SAVE-CLAIM-NO        PIC X(07)  VALUE SPACES.         EL1325
00170      05  WS-CLAIMS-FOUND-SW      PIC X(01).                       EL1325
00171          88  WS-CLAIMS-MATCH         VALUE 'Y'.                   EL1325
00172          88  WS-CLAIMS-NO-MATCH      VALUE 'N'.                   EL1325
00173                                                                   EL1325
00174      05  WS-DATE-WORK.                                            EL1325
00175          10  WS-DW-MONTH         PIC 99.                          EL1325
00176          10  FILLER              PIC X.                           EL1325
00177          10  WS-DW-DAY           PIC 99.                          EL1325
00178          10  FILLER              PIC X.                           EL1325
00179          10  WS-DW-YEAR          PIC 99.                          EL1325
00180                                                                   EL1325
00181      05  WS-CHAR                 PIC X       VALUE SPACES.        EL1325
00182                                                                   EL1325
00183      05  WS-INITIALS.                                             EL1325
00184          10  WS-INIT1            PIC X.                           EL1325
00185          10  WS-INIT2            PIC X.                           EL1325
00186                                                                   EL1325
00187      05  WS-CALC-RDNXT           PIC S9(8) VALUE ZERO     COMP.   EL1325
00188                                                                   EL1325
00189      EJECT                                                        EL1325
00190      05  WS-KEY-HOLD.                                             EL1325
00191          10  WS-KH-CHAR          PIC X                            EL1325
00192              OCCURS 30 TIMES        INDEXED BY KEY-INDEX.         EL1325
00193                                                                   EL1325
00194      05  WS-KEY-INPUT.                                            EL1325
00195          10  WS-KI-CHAR          PIC X                            EL1325
00196              OCCURS 30 TIMES        INDEXED BY KEY-INDEX2.        EL1325
00197                                                                   EL1325
00198                                  COPY ELCNWA.                     EL1325
00199                                                                   EL1325
00200      EJECT                                                        EL1325
00201                                  COPY ELCINTF.                    EL1325
00202                                                                   EL1325
00203                                 COPY ELC132PI.                    EL1325
00204                                                                   EL1325
00205      EJECT                                                        EL1325
00206                                  COPY EL1325S.                    EL1325
00207                                                                   EL1325
00208  01  FILLER       REDEFINES      EL1325AI.                        EL1325
101201     05  FILLER                  PIC X(85).                       EL1325
00210                                                                   EL1325
00211      05  EL1325-MAP-LINE         OCCURS 16 TIMES                  EL1325
00212          INDEXED BY EL1325-INDEX                                  EL1325
00213                     EL1325-INDEX2.                                EL1325
00214                                                                   EL1325
00215          10  EL1325-NUM-LENGTH   PIC S9(4)    COMP.               EL1325
00216          10  EL1325-NUM-ATTRB    PIC X.                           EL1325
00217          10  EL1325-NUM          PIC X(2).                        EL1325
00218                                                                   EL1325
00219          10  EL1325-NAME-LENGTH  PIC S9(4)    COMP.               EL1325
00220          10  EL1325-NAME-ATTRB   PIC X.                           EL1325
00221          10  EL1325-NAME         PIC X(27).                       EL1325
00222                                                                   EL1325
00223          10  EL1325-AGE-LENGTH   PIC S9(4)    COMP.               EL1325
00224          10  EL1325-AGE-ATTRB    PIC X.                           EL1325
00225          10  EL1325-AGE          PIC 99.                          EL1325
00226                                                                   EL1325
00227          10  EL1325-STA-LENGTH   PIC S9(4)    COMP.               EL1325
00228          10  EL1325-STA-ATTRB    PIC X.                           EL1325
00229          10  EL1325-STA          PIC X.                           EL1325
00230                                                                   EL1325
00231          10  EL1325-DATE-INCURRED-LENGTH PIC S9(4)     COMP.      EL1325
00232          10  EL1325-DATE-INCURRED-ATTRB  PIC X.                   EL1325
00233          10  EL1325-DATE-INCURRED    PIC X(8).                    EL1325
00234                                                                   EL1325
00235          10  EL1325-TYPE-LENGTH  PIC S9(4)      COMP.             EL1325
00236          10  EL1325-TYPE-ATTRB   PIC X.                           EL1325
00237          10  EL1325-TYPE         PIC X.                           EL1325
00238                                                                   EL1325
00239          10  EL1325-CARRIER-LENGTH   PIC S9(4)    COMP.           EL1325
00240          10  EL1325-CARRIER-ATTRB    PIC X.                       EL1325
00241          10  EL1325-CARRIER          PIC X.                       EL1325
00242                                                                   EL1325
00243          10  EL1325-CLAIM-LENGTH PIC S9(4)    COMP.               EL1325
00244          10  EL1325-CLAIM-ATTRB  PIC X.                           EL1325
00245          10  EL1325-CLAIM        PIC X(7).                        EL1325
00246                                                                   EL1325
00247          10  EL1325-CERT-NO-LENGTH   PIC S9(4)    COMP.           EL1325
00248          10  EL1325-CERT-NO-ATTRB    PIC X.                       EL1325
00249          10  EL1325-CERT-NO      PIC X(11).                       EL1325
00250                                                                   EL1325
00251          10  EL1325-ACCOUNT-LENGTH   PIC S9(4)    COMP.           EL1325
00252          10  EL1325-ACCOUNT-ATTRB    PIC X.                       EL1325
00253          10  EL1325-ACCOUNT      PIC X(10).                       EL1325
00254      05  FILLER                  PIC X(160).                         CL**3
00255      EJECT                                                        EL1325
00256                                  COPY ELCEMIB.                    EL1325
00257                                                                   EL1325
00258      EJECT                                                        EL1325
00259                                  COPY ELCDATE.                    EL1325
00260                                                                   EL1325
00261      EJECT                                                        EL1325
00262                                  COPY ELCATTR.                    EL1325
00263                                                                   EL1325
00264      EJECT                                                        EL1325
00265                                  COPY ELCLOGOF.                   EL1325
00266                                                                   EL1325
00267      EJECT                                                        EL1325
00268                                  COPY ELCAID.                     EL1325
00269                                                                   EL1325
00270  01  FILLER                      REDEFINES                        EL1325
00271      DFHAID.                                                      EL1325
00272                                                                   EL1325
00273      05  FILLER                  PIC X(8).                        EL1325
00274                                                                   EL1325
00275      05  PF-VALUES               PIC X                            EL1325
00276          OCCURS 24 TIMES.                                         EL1325
00277      EJECT                                                        EL1325
00278  LINKAGE SECTION.                                                 EL1325
00279                                                                   EL1325
00280  01  DFHCOMMAREA                 PIC X(1024).                     EL1325
00281                                                                   EL1325
00282      EJECT                                                        EL1325
00283                                  COPY ELCMSTR.                    EL1325
00284                                                                   EL1325
00285      EJECT                                                        EL1325
00286                                  COPY ELCCNTL.                    EL1325
00287                                                                   EL1325
00288      EJECT                                                        EL1325
00289                                  COPY ELCALPH.                    EL1325
00290                                                                   EL1325
00291      EJECT                                                        EL1325
00292  PROCEDURE DIVISION.                                              EL1325
00293                                                                   EL1325
00294      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL1325
00295      MOVE '5'                   TO DC-OPTION-CODE.                EL1325
00296      PERFORM 8500-DATE-CONVERSION.                                EL1325
00297      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL1325
00298      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL1325
00299                                                                   EL1325
00300                                                                   EL1325
00301      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL1325
00302                                                                   EL1325
00303 *    NOTE ******************************************************* EL1325
00304 *         *                                                     * EL1325
00305 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL1325
00306 *         *  FROM ANOTHER MODULE.                               * EL1325
00307 *         *                                                     * EL1325
00308 *         *******************************************************.EL1325
00309                                                                   EL1325
00310      IF EIBCALEN NOT GREATER THAN ZERO                            EL1325
00311          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL1325
00312          PERFORM 8300-SEND-TEXT.                                  EL1325
00313                                                                   EL1325
00314      EXEC CICS HANDLE CONDITION                                   EL1325
00315          PGMIDERR (9600-PGMIDERR)                                 EL1325
00316          NOTFND   (8700-NOT-FOUND)                                EL1325
00317          ENDFILE  (3600-ENDFILE)                                  EL1325
00318          DUPKEY   (3015-DUPKEY)                                   EL1325
00319          ERROR    (9990-ERROR)                                    EL1325
00320      END-EXEC.                                                    EL1325
00321                                                                   EL1325
00322      EJECT                                                        EL1325
00323  0010-MAIN-LOGIC.                                                 EL1325
00324                                                                   EL1325
00325      IF PI-CALLING-PROGRAM EQUAL THIS-PGM                         EL1325
00326          GO TO 0100-MAIN-LOGIC.                                   EL1325
00327                                                                   EL1325
00328      IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM                   EL1325
00329          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6         EL1325
00330          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5         EL1325
00331          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4         EL1325
00332          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3         EL1325
00333          MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2         EL1325
00334          MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1         EL1325
00335          MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM       EL1325
00336          MOVE THIS-PGM             TO  PI-CALLING-PROGRAM         EL1325
00337        ELSE                                                       EL1325
00338          MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM         EL1325
00339          MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM       EL1325
00340          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1         EL1325
00341          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2         EL1325
00342          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3         EL1325
00343          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4         EL1325
00344          MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5         EL1325
00345          MOVE SPACES               TO  PI-SAVED-PROGRAM-6.        EL1325
00346                                                                   EL1325
00347      MOVE ZERO TO PI-SCREEN-COUNT.                                EL1325
00348                                                                   EL1325
00349      IF (PI-OPTION   =  ZERO) OR                                  EL1325
00350         (PI-OPTION   =   '1') OR                                  EL1325
00351         (PI-OPTION   =   '4')                                     EL1325
00352          MOVE PI-ALPH-CLAIM-KEY    TO PI-1ST-ALPH-KEY             EL1325
00353      ELSE                                                         EL1325
00354          MOVE PI-ALPH-ADMIN-KEY    TO PI-1ST-ALPH-KEY.            EL1325
00355                                                                   EL1325
00356      IF PI-1ST-TIME-SW EQUAL TO +2                                EL1325
00357          MOVE EIBTRMID           TO  WS-TSK-TERM-ID               EL1325
00358          EXEC CICS READQ TS                                       EL1325
00359              QUEUE  (WS-TEMP-STORAGE-KEY)                         EL1325
00360              ITEM   (PI-TS-ITEM)                                  EL1325
00361              INTO   (EL1325AI)                                    EL1325
00362              LENGTH (WS-TS-LENGTH)                                EL1325
00363          END-EXEC                                                 EL1325
00364          EXEC CICS DELETEQ TS                                     EL1325
00365              QUEUE  (WS-TEMP-STORAGE-KEY)                         EL1325
00366          END-EXEC                                                 EL1325
00367          MOVE ZERO               TO  PI-1ST-TIME-SW               EL1325
00368          MOVE LOW-VALUES         TO  ASELO                        EL1325
00369                                      APFKO                        EL1325
00370          PERFORM 5200-SET-ATTRB                                   EL1325
00371              VARYING EL1325-INDEX FROM PI-LINE-COUNT BY -1        EL1325
00372                  UNTIL EL1325-INDEX NOT GREATER THAN ZERO         EL1325
00373          GO TO 8100-SEND-INITIAL-MAP.                             EL1325
00374                                                                   EL1325
00375      GO TO 3000-BROWSE-ALPHA-FILE.                                   CL**3
00376                                                                   EL1325
00377      EJECT                                                        EL1325
00378  0100-MAIN-LOGIC.                                                 EL1325
00379                                                                   EL1325
00380 *    NOTE ******************************************************* EL1325
00381 *         *                                                     * EL1325
00382 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL1325
00383 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL1325
00384 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL1325
00385 *         *                                                     * EL1325
00386 *         *******************************************************.EL1325
00387                                                                   EL1325
00388      IF EIBAID EQUAL TO DFHCLEAR                                  EL1325
00389          GO TO 9400-CLEAR.                                        EL1325
00390                                                                   EL1325
00391      IF EIBAID EQUAL TO (DFHPA1 OR                                EL1325
00392                          DFHPA2 OR                                EL1325
00393                          DFHPA3)                                  EL1325
00394          MOVE LOW-VALUES            TO  EL1325AI                  EL1325
00395          MOVE -1                    TO  APFKL                     EL1325
00396          MOVE ER-0008               TO  EMI-ERROR                 EL1325
00397          GO TO 8200-SEND-DATAONLY.                                EL1325
00398                                                                   EL1325
00399      EXEC CICS RECEIVE                                            EL1325
00400          INTO   (EL1325AI)                                        EL1325
00401          MAPSET (WS-MAPSET-NAME)                                  EL1325
00402          MAP    (WS-MAP-NAME)                                     EL1325
00403      END-EXEC.                                                    EL1325
00404                                                                   EL1325
00405      IF APFKL IS GREATER THAN ZERO                                EL1325
00406          IF EIBAID NOT EQUAL TO DFHENTER                          EL1325
00407              MOVE ER-0004           TO  EMI-ERROR                 EL1325
00408              MOVE AL-UNBOF          TO  APFKA                     EL1325
00409              MOVE -1                TO  APFKL                     EL1325
00410              GO TO 8200-SEND-DATAONLY                             EL1325
00411            ELSE                                                   EL1325
00412              IF APFKO IS NUMERIC                                  EL1325
00413                AND APFKO IS GREATER THAN ZERO                     EL1325
00414                AND APFKO IS LESS THAN '25'                        EL1325
00415                  MOVE PF-VALUES (APFKI)  TO  EIBAID               EL1325
00416                ELSE                                               EL1325
00417                  MOVE ER-0029           TO  EMI-ERROR             EL1325
00418                  MOVE AL-UNBOF       TO  APFKA                    EL1325
00419                  MOVE -1             TO  APFKL                    EL1325
00420                  GO TO 8200-SEND-DATAONLY.                        EL1325
00421                                                                   EL1325
00422      IF EIBAID IS EQUAL TO DFHPF12                                EL1325
00423          MOVE 'EL010   '         TO  THIS-PGM                     EL1325
00424          GO TO 9300-XCTL.                                         EL1325
00425                                                                   EL1325
00426      IF EIBAID IS EQUAL TO DFHPF23                                EL1325
00427          GO TO 9000-RETURN-CICS.                                  EL1325
00428                                                                   EL1325
00429      IF EIBAID IS EQUAL TO DFHPF24                                EL1325
00430          MOVE 'EL126   '         TO  THIS-PGM                     EL1325
00431          GO TO 9300-XCTL.                                         EL1325
00432                                                                   EL1325
00433      IF EIBAID EQUAL TO (DFHENTER OR                              EL1325
00434                          DFHPF1 OR                                EL1325
00435                          DFHPF2 OR                                EL1325
00436                          DFHPF3 OR                                EL1325
00437                          DFHPF5 OR                                EL1325
00438                          DFHPF6)                                  EL1325
00439          NEXT SENTENCE                                            EL1325
00440        ELSE                                                       EL1325
00441          MOVE -1                 TO  APFKL                        EL1325
00442          MOVE ER-0008               TO  EMI-ERROR                 EL1325
00443          GO TO 8200-SEND-DATAONLY.                                EL1325
00444                                                                   EL1325
00445      IF EIBAID EQUAL DFHPF3                                       EL1325
00446        OR ASELL GREATER THAN ZERO                                 EL1325
00447          NEXT SENTENCE                                            EL1325
00448        ELSE                                                       EL1325
00449          GO TO 0120-MAIN-LOGIC.                                   EL1325
00450                                                                   EL1325
00451      IF ASELL GREATER THAN ZERO                                   EL1325
00452         AND ASELI NUMERIC                                         EL1325
00453         NEXT SENTENCE                                             EL1325
00454      ELSE                                                         EL1325
00455         IF PI-SAVED-PROGRAM-1 EQUAL TO 'EL130   '                 EL1325
00456              NEXT SENTENCE                                        EL1325
00457          ELSE                                                     EL1325
00458             MOVE -1                    TO  ASELL                  EL1325
00459             MOVE ER-0031               TO  EMI-ERROR              EL1325
00460             GO TO 8200-SEND-DATAONLY.                             EL1325
00461                                                                   EL1325
00462      IF ASELL GREATER THAN ZERO                                   EL1325
00463        AND ASELO GREATER THAN ZERO                                EL1325
00464        AND ASELO LESS THAN '17'                                   EL1325
00465        AND (ASELI NUMERIC AND ASELI NOT GREATER PI-LINE-COUNT)    EL1325
00466          NEXT SENTENCE                                            EL1325
00467        ELSE                                                       EL1325
00468          IF PI-SAVED-PROGRAM-1 EQUAL TO 'EL130   '                EL1325
00469              NEXT SENTENCE                                        EL1325
00470            ELSE                                                   EL1325
00471              MOVE -1                    TO  ASELL                 EL1325
00472              MOVE ER-0200               TO  EMI-ERROR             EL1325
00473              GO TO 8200-SEND-DATAONLY.                            EL1325
00474                                                                   EL1325
00475      IF ASELL IS GREATER THAN ZERO                                EL1325
00476          IF ASELI IS NUMERIC                                      EL1325
00477              NEXT SENTENCE                                        EL1325
00478          ELSE                                                     EL1325
00479              MOVE -1                 TO  ASELL                    EL1325
00480              MOVE ER-0031            TO  EMI-ERROR                EL1325
00481              GO TO 8200-SEND-DATAONLY.                            EL1325
00482                                                                   EL1325
00483      IF ASELL GREATER THAN ZERO                                   EL1325
00484          SET EL1325-INDEX TO ASELI                                EL1325
00485          MOVE EL1325-CARRIER (EL1325-INDEX)  TO  PI-CARRIER       EL1325
00486          MOVE EL1325-CLAIM   (EL1325-INDEX)  TO  PI-CLAIM-NO      EL1325
00487          MOVE EL1325-CERT-NO (EL1325-INDEX)  TO  PI-CERT-NO       EL1325
00488          MOVE EL1325-ACCOUNT (EL1325-INDEX)  TO  PI-ACCOUNT       EL1325
00489          SET PI-INDEX TO ASELI                                    EL1325
00490          MOVE PI-SA-GROUP    (PI-INDEX)  TO  PI-GROUPING          EL1325
00491          MOVE PI-SA-STATE    (PI-INDEX)  TO  PI-STATE             EL1325
00492          MOVE PI-SA-EFF-DATE (PI-INDEX)  TO  PI-CERT-EFF-DT       EL1325
00493        ELSE                                                       EL1325
00494          MOVE SPACES             TO  PI-CARRIER                   EL1325
00495                                      PI-CLAIM-NO                  EL1325
00496                                      PI-CERT-NO                   EL1325
00497                                      PI-ACCOUNT                   EL1325
00498                                      PI-GROUPING                  EL1325
00499                                      PI-STATE                     EL1325
00500          MOVE LOW-VALUES         TO  PI-CERT-EFF-DT.              EL1325
00501                                                                   EL1325
00502      MOVE +2                     TO  PI-1ST-TIME-SW               EL1325
00503                                                                   EL1325
00504      IF EIBAID EQUAL TO DFHPF3                                    EL1325
00505          MOVE 'EL1323  '         TO  THIS-PGM                     EL1325
00506      ELSE                                                         EL1325
00507          IF PI-SAVED-PROGRAM-1 EQUAL TO ('EL126   ' OR            EL1325
00508                                          'EL127   ' OR            EL1325
00509                                          'EL130   ' OR            EL1325
00510                                          'EL132   ' OR            EL1325
00511                                          'EL150   ' OR            EL1325
00512                                          'EL1602  ' OR            EL1325
00513                                          'EL162   ')              EL1325
00514             MOVE PI-SAVED-PROGRAM-1  TO  THIS-PGM                 EL1325
00515          ELSE                                                     EL1325
00516             MOVE 'EL1323  '  TO  THIS-PGM.                        EL1325
00517                                                                   EL1325
00518      IF THIS-PGM EQUAL TO 'EL1323  '                              EL1325
00519          MOVE EIBTRMID   TO  WS-TSK-TERM-ID                       EL1325
00520          MOVE -1         TO  ASELL                                EL1325
00521          EXEC CICS WRITEQ TS                                      EL1325
00522              FROM   (EL1325AI)                                    EL1325
00523              LENGTH (WS-TS-LENGTH)                                EL1325
00524              QUEUE  (WS-TEMP-STORAGE-KEY)                         EL1325
00525              ITEM   (PI-TS-ITEM)                                  EL1325
00526          END-EXEC                                                 EL1325
00527        ELSE                                                       EL1325
00528          MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM         EL1325
00529          MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM       EL1325
00530          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1         EL1325
00531          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2         EL1325
00532          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3         EL1325
00533          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4         EL1325
00534          MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5         EL1325
00535          MOVE SPACES               TO  PI-SAVED-PROGRAM-6.        EL1325
00536                                                                   EL1325
00537      IF THIS-PGM EQUAL TO ('EL126   ' OR 'EL127   ' OR 'EL132   ')EL1325
00538          MOVE 'EL150   '         TO  THIS-PGM.                    EL1325
00539                                                                   EL1325
00540      GO TO 9300-XCTL.                                             EL1325
00541                                                                   EL1325
00542  0120-MAIN-LOGIC.                                                 EL1325
00543                                                                   EL1325
00544      IF EIBAID EQUAL TO (DFHENTER OR DFHPF1 OR DFHPF5 OR DFHPF6)  EL1325
00545        OR                                                         EL1325
00546          ((EIBAID EQUAL TO DFHPF2) AND                            EL1325
00547           (PI-SCREEN-COUNT GREATER THAN +1))                      EL1325
00548              NEXT SENTENCE                                        EL1325
00549            ELSE                                                   EL1325
00550              MOVE -1                 TO  APFKL                    EL1325
00551              MOVE ER-0008            TO  EMI-ERROR                EL1325
00552              GO TO 8200-SEND-DATAONLY.                            EL1325
00553                                                                   EL1325
00554      IF EIBAID EQUAL DFHPF5                                       EL1325
00555         IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES                EL1325
00556            PERFORM 8600-NEXT-COMPANY THRU 8600-EXIT               EL1325
00557            PERFORM 8650-WRITE-SECURITY-TEMP-STORE THRU 8650-EXIT  EL1325
00558            IF NOT DISPLAY-CAP                                     EL1325
00559                PERFORM 8800-INITIALIZE-MAP VARYING EL1325-INDEX   EL1325
00560                 FROM +1 BY +1 UNTIL EL1325-INDEX GREATER THAN +16 EL1325
00561                MOVE 'READ'           TO  SM-READ                  EL1325
00562                PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT        CL**3
00563                MOVE ER-0070          TO  EMI-ERROR                EL1325
00564                MOVE -1               TO  APFKL                    EL1325
00565                GO TO 8100-SEND-INITIAL-MAP                        EL1325
00566            ELSE                                                   EL1325
00567                NEXT SENTENCE                                      EL1325
00568         ELSE                                                      EL1325
00569            MOVE ER-0008              TO  EMI-ERROR                EL1325
00570            MOVE -1                   TO  ASELL                    EL1325
00571            GO TO 8200-SEND-DATAONLY.                              EL1325
00572                                                                   EL1325
00573      IF EIBAID EQUAL DFHPF6                                       EL1325
00574         IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES                EL1325
00575            PERFORM 8600-NEXT-COMPANY THRU 8600-EXIT               EL1325
00576            PERFORM 8650-WRITE-SECURITY-TEMP-STORE THRU 8650-EXIT  EL1325
00577            MOVE PI-ORIGINAL-COMPANY-CD TO PI-COMPANY-CD           EL1325
00578                                           PI-CK-COMPANY-CD        EL1325
00579            IF NOT DISPLAY-CAP                                     EL1325
00580                PERFORM 8800-INITIALIZE-MAP VARYING EL1325-INDEX   EL1325
00581                 FROM +1 BY +1 UNTIL EL1325-INDEX GREATER THAN +16 EL1325
00582                MOVE 'READ'           TO  SM-READ                  EL1325
00583                PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT        CL**3
00584                MOVE ER-0070          TO  EMI-ERROR                EL1325
00585                MOVE -1               TO  APFKL                    EL1325
00586                GO TO 8100-SEND-INITIAL-MAP                        EL1325
00587            ELSE                                                   EL1325
00588                NEXT SENTENCE                                      EL1325
00589         ELSE                                                      EL1325
00590            MOVE ER-0008              TO  EMI-ERROR                EL1325
00591            MOVE -1                   TO  ASELL                    EL1325
00592            GO TO 8200-SEND-DATAONLY.                              EL1325
00593                                                                   EL1325
00594      IF PI-END-OF-FILE GREATER THAN ZERO                          EL1325
00595        AND ((EIBAID EQUAL TO (DFHENTER OR DFHPF1) AND             EL1325
00596              PI-LAST-EIBAID EQUAL TO (DFHENTER OR DFHPF1))        EL1325
00597            OR                                                     EL1325
00598             (EIBAID EQUAL TO DFHPF2 AND                           EL1325
00599              PI-LAST-EIBAID EQUAL TO DFHPF2))                     EL1325
00600                  MOVE -1             TO  ASELL                    EL1325
00601                  GO TO 8200-SEND-DATAONLY.                        EL1325
00602                                                                   EL1325
00603      IF (EIBAID IS EQUAL TO DFHENTER OR DFHPF1)                   EL1325
00604          IF PI-ALPH-CO-CD  IS EQUAL TO PI-COMPANY-CD              EL1325
00605              NEXT SENTENCE                                        EL1325
00606          ELSE                                                     EL1325
00607              MOVE -1                 TO  ASELL                    EL1325
00608              GO TO 8200-SEND-DATAONLY.                            EL1325
00609                                                                   EL1325
00610      GO TO 3000-BROWSE-ALPHA-FILE.                                   CL**3
00611                                                                   EL1325
00612      EJECT                                                        EL1325
00613  3000-BROWSE-ALPHA-FILE SECTION.                                  EL1325
00614                                                                   EL1325
00615      EXEC CICS HANDLE CONDITION                                   EL1325
00616          NOTFND   (8700-NOT-FOUND)                                EL1325
00617          END-EXEC.                                                EL1325
00618                                                                   EL1325
00619      MOVE EIBAID                 TO  PI-LAST-EIBAID.              EL1325
00620                                                                   EL1325
00621      MOVE LOW-VALUES             TO  EL1325AI.                    EL1325
00622                                                                   EL1325
00623      IF PI-OPTION  =  '2'                                         EL1325
00624          MOVE PI-CLM-CLAIM-NO    TO  WS-SAVE-CLAIM-NO.            EL1325
00625                                                                   EL1325
00626      IF (PI-OPTION   = ZERO)  OR                                  EL1325
00627         (PI-OPTION   =  '1')  OR                                  EL1325
00628         (PI-OPTION   =  '4')                                      EL1325
00629          NEXT SENTENCE                                            EL1325
00630      ELSE                                                         EL1325
00631          GO TO 3000-READ-ALPHA-ADMIN.                             EL1325
00632                                                                   EL1325
00633      IF (PI-BROWSE-SW EQUAL TO ZERO)  AND                         EL1325
00634         (PI-START-SW EQUAL TO +1)                                 EL1325
00635           EXEC CICS STARTBR                                       EL1325
00636              DATASET    (PI-DSID)                                 EL1325
00637              RIDFLD     (PI-ALPH-CLAIM-KEY)                       EL1325
00638              KEYLENGTH  (PI-KEY-LENGTH)                           EL1325
00639              GENERIC                                              EL1325
00640              EQUAL                                                EL1325
00641           END-EXEC                                                EL1325
00642           GO TO 3005-NEXT-SENTENCE.                               EL1325
00643                                                                   EL1325
00644  3000-READ-ALPHA-ADMIN.                                           EL1325
00645                                                                   EL1325
00646      IF (PI-BROWSE-SW EQUAL TO ZERO)  AND                         EL1325
00647         (PI-START-SW EQUAL TO +1)                                 EL1325
00648           EXEC CICS STARTBR                                       EL1325
00649              DATASET    (PI-DSID)                                 EL1325
00650              RIDFLD     (PI-ALPH-ADMIN-KEY)                       EL1325
00651              KEYLENGTH  (PI-KEY-LENGTH)                           EL1325
00652              GENERIC                                              EL1325
00653              EQUAL                                                EL1325
00654           END-EXEC                                                EL1325
00655           GO TO 3005-NEXT-SENTENCE.                               EL1325
00656                                                                   EL1325
00657      IF EIBAID EQUAL TO DFHPF2                                    EL1325
00658          SUBTRACT 2 FROM PI-SCREEN-COUNT                          EL1325
00659          PERFORM 7000-PF2-POSITION                                EL1325
00660          GO TO 3005-NEXT-SENTENCE.                                EL1325
00661                                                                   EL1325
00662      IF (PI-OPTION   = ZERO) OR                                   EL1325
00663         (PI-OPTION   =  '1') OR                                   EL1325
00664         (PI-OPTION   =  '4')                                      EL1325
00665          EXEC CICS STARTBR                                        EL1325
00666            DATASET      (PI-DSID)                                 EL1325
00667            RIDFLD       (PI-ALPH-CLAIM-KEY)                       EL1325
00668            KEYLENGTH    (PI-KEY-LENGTH)                           EL1325
00669            GENERIC                                                EL1325
00670            EQUAL                                                  EL1325
00671          END-EXEC                                                 EL1325
00672      ELSE                                                         EL1325
00673          EXEC CICS STARTBR                                        EL1325
00674            DATASET      (PI-DSID)                                 EL1325
00675            RIDFLD       (PI-ALPH-ADMIN-KEY)                       EL1325
00676            KEYLENGTH    (PI-KEY-LENGTH)                           EL1325
00677            GENERIC                                                EL1325
00678            EQUAL                                                  EL1325
00679          END-EXEC.                                                EL1325
00680                                                                   EL1325
00681  3005-NEXT-SENTENCE.                                              EL1325
00682                                                                   EL1325
00683      MOVE +1                     TO  PI-BROWSE-SW.                EL1325
00684      MOVE ZERO                   TO  PI-LINE-COUNT.               EL1325
00685      MOVE LOW-VALUES             TO  EL1325AI.                    EL1325
00686                                                                   EL1325
00687      IF (PI-OPTION   = ZERO) OR                                   EL1325
00688         (PI-OPTION   =  '1') OR                                   EL1325
00689         (PI-OPTION   =  '4')                                      EL1325
00690          MOVE PI-ALPH-CLAIM-KEY  TO  WS-KEY-HOLD                  EL1325
00691      ELSE                                                         EL1325
00692          MOVE PI-ALPH-ADMIN-KEY  TO  WS-KEY-HOLD.                 EL1325
00693                                                                   EL1325
00694      SET EL1325-INDEX            TO  +1.                          EL1325
00695      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL1325
00696      MOVE '5'                    TO  DC-OPTION-CODE.              EL1325
00697      PERFORM 8500-DATE-CONVERSION.                                EL1325
00698      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE.             EL1325
00699                                                                   EL1325
00700  3010-READNEXT.                                                   EL1325
00701                                                                   EL1325
00702      IF (PI-OPTION   = ZERO) OR                                   EL1325
00703         (PI-OPTION   =  '1') OR                                   EL1325
00704         (PI-OPTION   =  '4')                                      EL1325
00705          EXEC CICS READNEXT                                       EL1325
00706              DATASET (PI-DSID)                                    EL1325
00707              RIDFLD  (PI-ALPH-CLAIM-KEY)                          EL1325
00708              SET     (ADDRESS OF ALPHA-INDEX)                        CL**3
00709          END-EXEC                                                 EL1325
00710      ELSE                                                         EL1325
00711          EXEC CICS READNEXT                                       EL1325
00712              DATASET (PI-DSID)                                    EL1325
00713              RIDFLD  (PI-ALPH-ADMIN-KEY)                          EL1325
00714              SET     (ADDRESS OF ALPHA-INDEX)                        CL**3
00715          END-EXEC.                                                EL1325
00716                                                                   EL1325
00717      IF PI-LINE-COUNT NOT EQUAL TO ZERO                           EL1325
00718          MOVE ZERO               TO  PI-AIX-RECORD-COUNT.         EL1325
00719                                                                   EL1325
00720  3015-DUPKEY.                                                     EL1325
00721                                                                   EL1325
00722      IF LCP-ONCTR-01 =  0                                            CL**3
00723          ADD 1 TO LCP-ONCTR-01                                       CL**3
00724        ELSE                                                       EL1325
00725          GO TO 3016-NEXT-SENTENCE.                                EL1325
00726                                                                   EL1325
00727      IF PI-AIX-RECORD-COUNT GREATER THAN +16                      EL1325
00728          SUBTRACT +1 FROM PI-AIX-RECORD-COUNT.                    EL1325
00729                                                                   EL1325
00730  3016-NEXT-SENTENCE.                                              EL1325
00731                                                                   EL1325
00732      ADD +1  TO  WS-AIX-RECORD-COUNT                              EL1325
00733                                                                   EL1325
00734      IF EIBAID EQUAL DFHPF1                                       EL1325
00735         IF OPTION-ONE-SELECTED                                    EL1325
00736            IF PI-LINE-COUNT EQUAL ZERO                            EL1325
00737               PERFORM 5000-MOVE-NAME                              EL1325
00738               IF AI-LAST-NAME  EQUAL  PI-ALPH-LAST-NAME           EL1325
00739                  IF PI-LAST-ALPH-KEY EQUAL                        EL1325
00740                           AI-CONTROL-PRIMARY                      EL1325
00741                     NEXT SENTENCE                                 EL1325
00742                  ELSE                                             EL1325
00743                     GO TO 3010-READNEXT.                          EL1325
00744                                                                   EL1325
00745      IF (PI-OPTION  = ZERO) OR                                    EL1325
00746         (PI-OPTION  =  '1') OR                                    EL1325
00747         (PI-OPTION  =  '4')                                       EL1325
00748          MOVE PI-ALPH-CLAIM-KEY     TO  WS-KEY-INPUT              EL1325
00749      ELSE                                                         EL1325
00750          MOVE PI-ALPH-ADMIN-KEY     TO  WS-KEY-INPUT.             EL1325
00751                                                                   EL1325
00752      SET KEY-INDEX                                                EL1325
00753          KEY-INDEX2 TO +1.                                        EL1325
00754                                                                   EL1325
00755  3020-COMPARE-KEY.                                                EL1325
00756                                                                   EL1325
00757      IF WS-KH-CHAR (KEY-INDEX) NOT EQUAL WS-KI-CHAR (KEY-INDEX2)  EL1325
00758          GO TO 3700-END-OF-BROWSE.                                EL1325
00759                                                                   EL1325
00760      IF KEY-INDEX LESS THAN PI-KEY-LENGTH                         EL1325
00761          SET KEY-INDEX                                            EL1325
00762              KEY-INDEX2 UP BY +1                                  EL1325
00763          GO TO 3020-COMPARE-KEY.                                  EL1325
00764                                                                   EL1325
00765      IF OPTION-TWO-SELECTED                                       EL1325
00766         IF PI-CLM-CLAIM-NO   NOT EQUAL SPACES                     EL1325
00767            IF PI-CLM-CLAIM-NO   EQUAL AI-CL-CLAIM-NUMBER          EL1325
00768               NEXT SENTENCE                                       EL1325
00769            ELSE                                                   EL1325
00770               GO TO 3010-READNEXT.                                EL1325
00771 ******************************************************************EL1325
00772 *                                                                *EL1325
00773 *        SECURITY CHECK FOR CARRIER AND ACCOUNT NUMBER           *EL1325
00774 *                      04/04/84                                  *EL1325
00775 *                                                                *EL1325
00776 ******************************************************************EL1325
00777                                                                   EL1325
00778      IF  PI-NO-CARRIER-SECURITY                                   EL1325
00779          GO TO 3095-MOVE-DATA.                                    EL1325
00780                                                                   EL1325
00781      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  EL1325
00782          IF  AI-CL-CARRIER = PI-CARRIER-SECURITY                  EL1325
00783              NEXT SENTENCE                                        EL1325
00784          ELSE                                                     EL1325
00785              GO TO 3010-READNEXT.                                 EL1325
00786                                                                   EL1325
00787      IF  PI-CLM-CERT-NUMBER   NOT EQUAL SPACES                    EL1325
00788          IF PI-CLM-CERT-NUMBER  =   AI-CL-CERTIFICATE-NUMBER      EL1325
00789              NEXT SENTENCE                                        EL1325
00790          ELSE                                                     EL1325
00791              GO TO 3010-READNEXT.                                 EL1325
00792                                                                   EL1325
00793  3095-MOVE-DATA.                                                  EL1325
00794                                                                   EL1325
00795      MOVE +1                     TO WS-CLAIMS-SW.                 EL1325
00796      MOVE 'N'                    TO WS-CLAIMS-FOUND-SW.           EL1325
00797                                                                   EL1325
00798      MOVE WS-KEY-INPUT TO PI-LAST-ALPH-KEY.                       EL1325
00799                                                                   EL1325
00800      ADD +1  TO  PI-LINE-COUNT                                    EL1325
00801                  PI-AIX-RECORD-COUNT.                             EL1325
00802                                                                   EL1325
00803      PERFORM 5000-MOVE-NAME                                       EL1325
00804                                                                   EL1325
00805      MOVE AI-CONTROL-PRIMARY     TO PI-LAST-ALPH-KEY.             EL1325
00806      MOVE AI-LAST-NAME           TO PI-LAST-NAME                  EL1325
00807                                     PI-CK-INSURED-LAST-NAME.      EL1325
00808      MOVE WS-NAME-WORK           TO EL1325-NAME (EL1325-INDEX).   EL1325
00809                                                                   EL1325
00810      MOVE AI-CL-CARRIER          TO EL1325-CARRIER (EL1325-INDEX).EL1325
00811      MOVE AI-CL-CLAIM-NUMBER     TO EL1325-CLAIM   (EL1325-INDEX).EL1325
00812      MOVE AI-CL-CERTIFICATE-NUMBER                                EL1325
00813                                  TO EL1325-CERT-NO (EL1325-INDEX).EL1325
00814                                                                   EL1325
00815      IF AI-CL-CLOSE-DATE NOT EQUAL TO LOW-VALUES                  EL1325
00816          MOVE SPACES             TO  DC-OPTION-CODE               EL1325
00817          MOVE AI-CL-CLOSE-DATE   TO  DC-BIN-DATE-1                EL1325
00818          PERFORM 8500-DATE-CONVERSION                             EL1325
00819          MOVE DC-GREG-DATE-1-EDIT  TO  EL1325-ACCOUNT             EL1325
00820                                                    (EL1325-INDEX).EL1325
00821      PERFORM 5200-SET-ATTRB.                                      EL1325
00822                                                                   EL1325
00823      PERFORM 4000-READ-CLAIM-FILE THRU 4990-EXIT.                 EL1325
00824                                                                   EL1325
00825      IF WS-CLAIMS-MATCH                                           EL1325
00826         IF CL-INCURRED-DT NOT EQUAL TO LOW-VALUES                 EL1325
00827            MOVE SPACES             TO  DC-OPTION-CODE             EL1325
00828            MOVE CL-INCURRED-DT     TO  DC-BIN-DATE-1              EL1325
00829            PERFORM 8500-DATE-CONVERSION                           EL1325
00830            MOVE DC-GREG-DATE-1-EDIT  TO  EL1325-DATE-INCURRED     EL1325
00831                                                    (EL1325-INDEX).EL1325
00832                                                                   EL1325
00833      IF WS-CLAIMS-NO-MATCH                                        EL1325
00834         IF AI-CL-INCURRED-DATE  NOT EQUAL TO LOW-VALUES           EL1325
00835            MOVE SPACES              TO  DC-OPTION-CODE            EL1325
00836            MOVE AI-CL-INCURRED-DATE TO  DC-BIN-DATE-1             EL1325
00837            PERFORM 8500-DATE-CONVERSION                           EL1325
00838            MOVE DC-GREG-DATE-1-EDIT  TO  EL1325-DATE-INCURRED     EL1325
00839                                                    (EL1325-INDEX).EL1325
00840                                                                   EL1325
00841      IF EL1325-INDEX LESS THAN +16                                EL1325
00842          SET EL1325-INDEX UP BY +1                                EL1325
00843          GO TO 3010-READNEXT.                                     EL1325
00844                                                                   EL1325
00845      GO TO 3900-ENDBROWSE.                                        EL1325
00846                                                                   EL1325
00847  3600-ENDFILE.                                                    EL1325
00848                                                                   EL1325
00849      MOVE ER-0130                   TO  EMI-ERROR.                EL1325
00850      ADD +1  TO  PI-END-OF-FILE.                                  EL1325
00851                                                                   EL1325
00852  3700-END-OF-BROWSE.                                              EL1325
00853                                                                   EL1325
00854      ADD +1  TO  PI-END-OF-FILE.                                  EL1325
00855                                                                   EL1325
00856      EXEC CICS ENDBR                                              EL1325
00857          DATASET (PI-DSID)                                        EL1325
00858      END-EXEC.                                                    EL1325
00859                                                                   EL1325
00860  3900-ENDBROWSE.                                                  EL1325
00861                                                                   EL1325
00862 ******************************************************************EL1325
00863 *                                                                *EL1325
00864 *        IF THERE ARE NO CLAIM RECORDS FOUND DURING              *EL1325
00865 *        THE INITIAL ENTRY OF EL6322, RETURN TO THE CALLING      *EL1325
00866 *        PROGRAM.                                                *EL1325
00867 *                                                                *EL1325
00868 ******************************************************************EL1325
00869                                                                   EL1325
00870      IF  WS-NO-CLAIMS-FOUND                                       EL1325
00871          MOVE +9                 TO  PI-BROWSE-SW                 EL1325
00872          MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM                  EL1325
00873          GO TO 9400-CLEAR.                                        EL1325
00874      ADD +1 TO PI-SCREEN-COUNT.                                   EL1325
00875                                                                   EL1325
00876      MOVE +1                     TO  PI-1ST-TIME-SW               EL1325
00877                                                                   EL1325
00878      MOVE -1                     TO  ASELL                        EL1325
00879      GO TO 8100-SEND-INITIAL-MAP.                                 EL1325
00880                                                                   EL1325
00881      EJECT                                                        EL1325
00882  4000-READ-CLAIM-FILE  SECTION.                                   EL1325
00883                                                                   EL1325
00884      EXEC CICS HANDLE CONDITION                                   EL1325
00885          NOTFND   (4990-EXIT)                                     EL1325
00886          END-EXEC.                                                EL1325
00887                                                                   EL1325
00888      MOVE LOW-VALUES                 TO  PI-CLAIM-KEY.            EL1325
00889      MOVE AI-CL-COMPANY-CD           TO  PI-CK-COMPANY-CD         EL1325
00890      MOVE AI-CL-CARRIER              TO  PI-CK-CARRIER            EL1325
00891      MOVE AI-CL-CLAIM-NUMBER         TO  PI-CK-CLAIM              EL1325
00892      MOVE AI-CL-CERTIFICATE-NUMBER   TO  PI-CK-CERT-NO.           EL1325
00893                                                                   EL1325
00894      EXEC CICS READ                                               EL1325
00895          DATASET   ('ELMSTR')                                     EL1325
00896          SET       (ADDRESS OF CLAIM-MASTER)                         CL**3
00897          RIDFLD    (PI-CLAIM-KEY)                                 EL1325
00898       END-EXEC.                                                   EL1325
00899                                                                   EL1325
00900      IF (AI-CL-COMPANY-CD          EQUAL  PI-CK-COMPANY-CD) AND   EL1325
00901         (AI-CL-CARRIER             EQUAL  PI-CK-CARRIER)    AND   EL1325
00902         (AI-CL-CLAIM-NUMBER        EQUAL  PI-CK-CLAIM)      AND   EL1325
00903         (AI-CL-CERTIFICATE-NUMBER  EQUAL PI-CK-CERT-NO)           EL1325
00904           MOVE  'Y'                   TO  WS-CLAIMS-FOUND-SW.     EL1325
00905                                                                   EL1325
00906      IF CL-INSURED-BIRTH-DT NOT EQUAL TO LOW-VALUES               EL1325
00907          MOVE CL-INSURED-BIRTH-DT    TO  DC-BIN-DATE-1            EL1325
00908          MOVE WS-CURRENT-DATE        TO  DC-BIN-DATE-2            EL1325
00909          MOVE '1'                    TO  DC-OPTION-CODE           EL1325
00910          PERFORM 8500-DATE-CONVERSION                             EL1325
00911          DIVIDE DC-ELAPSED-MONTHS BY +12 GIVING EL1325-AGE        EL1325
00912                                                    (EL1325-INDEX).EL1325
00913                                                                   EL1325
00914      MOVE CL-CLAIM-STATUS      TO  EL1325-STA     (EL1325-INDEX). EL1325
00915                                                                   EL1325
00916      IF CL-PURGED-DT NOT EQUAL LOW-VALUES                         EL1325
00917         MOVE 'P'               TO  EL1325-STA (EL1325-INDEX).     EL1325
00918                                                                   EL1325
00919      IF CL-INCURRED-DT NOT EQUAL TO LOW-VALUES                    EL1325
00920          MOVE SPACES             TO  DC-OPTION-CODE               EL1325
00921          MOVE CL-INCURRED-DT     TO  DC-BIN-DATE-1                EL1325
00922          PERFORM 8500-DATE-CONVERSION                             EL1325
00923          MOVE DC-GREG-DATE-1-EDIT  TO  EL1325-DATE-INCURRED       EL1325
00924                                                    (EL1325-INDEX).EL1325
00925                                                                   EL1325
00926      MOVE CL-CLAIM-TYPE        TO  EL1325-TYPE      (EL1325-INDEX)EL1325
00927      MOVE CL-CERT-ACCOUNT      TO  EL1325-ACCOUNT   (EL1325-INDEX)EL1325
00928                                                                   EL1325
00929      SET PI-INDEX  TO  EL1325-INDEX                               EL1325
00930                                                                   EL1325
00931      MOVE CL-CERT-GROUPING  TO  PI-SA-GROUP    (PI-INDEX)         EL1325
00932      MOVE CL-CERT-STATE     TO  PI-SA-STATE    (PI-INDEX)         EL1325
00933      MOVE CL-CERT-EFF-DT    TO  PI-SA-EFF-DATE (PI-INDEX)         EL1325
00934                                                                   EL1325
00935      IF CL-CERT-NO EQUAL TO CL-PRIME-CERT-NO AND                  EL1325
00936         CL-ASSOC-CERT-TOTAL GREATER THAN +1                       EL1325
00937         MOVE AL-SABON     TO EL1325-NAME-ATTRB (EL1325-INDEX)     EL1325
00938         ELSE                                                      EL1325
00939         MOVE AL-SANON     TO EL1325-NAME-ATTRB (EL1325-INDEX).    EL1325
00940                                                                   EL1325
00941  4990-EXIT.                                                       EL1325
00942      EXIT.                                                        EL1325
00943      EJECT                                                        EL1325
00944  5000-MOVE-NAME SECTION.                                          EL1325
00945                                                                   EL1325
00946      MOVE SPACES                  TO  WS-NAME-WORK-AREA.          EL1325
00947      MOVE ZEROS                   TO  WS-NAME-SW.                 EL1325
00948      SET NWA-INDEX                TO  +1.                         EL1325
00949                                                                   EL1325
00950      IF (AI-FIRST-NAME  =  SPACES) AND                            EL1325
00951         (AI-MIDDLE-INIT =  SPACES)                                EL1325
00952          MOVE +1                  TO  WS-NAME-SW.                 EL1325
00953                                                                   EL1325
00954      MOVE AI-LAST-NAME            TO  WS-NAME-WORK2.              EL1325
00955      PERFORM 5100-MOVE-NAME  THRU  5190-EXIT.                     EL1325
00956                                                                   EL1325
00957      MOVE AI-FIRST-NAME           TO  WS-NAME-WORK2.              EL1325
00958      PERFORM 5100-MOVE-NAME  THRU  5190-EXIT.                     EL1325
00959                                                                   EL1325
00960      SET NWA-INDEX  UP BY +1                                      EL1325
00961      MOVE AI-MIDDLE-INIT          TO  WS-NAME-WORK2.              EL1325
00962      PERFORM 5100-MOVE-NAME  THRU  5190-EXIT.                     EL1325
00963                                                                   EL1325
00964  5000-EXIT.                                                       EL1325
00965      EXIT.                                                        EL1325
00966  5100-MOVE-NAME SECTION.                                          EL1325
00967                                                                   EL1325
00968      IF WS-NAME-SW  GREATER THAN  +1                              EL1325
00969          GO TO 5190-EXIT.                                         EL1325
00970                                                                   EL1325
00971      IF WS-NAME-WORK2  =  SPACES                                  EL1325
00972          GO TO 5190-EXIT.                                         EL1325
00973                                                                   EL1325
00974      SET NWA-INDEX2  TO +1.                                       EL1325
00975      SET NWA-INDEX3  TO +2.                                       EL1325
00976                                                                   EL1325
00977  5110-MOVE-NAME.                                                  EL1325
00978                                                                   EL1325
00979      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).             EL1325
00980                                                                   EL1325
00981      IF NWA-INDEX  LESS THAN  +30                                 EL1325
00982          SET NWA-INDEX UP BY +1                                   EL1325
00983      ELSE                                                         EL1325
00984          ADD +2          TO  WS-NAME-SW                           EL1325
00985          GO TO  5190-EXIT.                                        EL1325
00986                                                                   EL1325
00987      IF NWA-INDEX2  LESS THAN  +20                                EL1325
00988          SET NWA-INDEX3  UP BY +1                                 EL1325
00989          SET NWA-INDEX2  UP BY +1.                                EL1325
00990                                                                   EL1325
00991      IF WS-NW2 (NWA-INDEX2) = SPACES  AND                         EL1325
00992         WS-NW2 (NWA-INDEX3) = SPACES                              EL1325
00993          IF WS-NAME-SW  =  ZERO                                   EL1325
00994              MOVE ' '           TO  WS-NW (NWA-INDEX)             EL1325
00995              SET  NWA-INDEX UP BY +2                              EL1325
00996              MOVE +1            TO  WS-NAME-SW                    EL1325
00997              GO TO 5190-EXIT                                      EL1325
00998          ELSE                                                     EL1325
00999              GO TO 5190-EXIT.                                     EL1325
01000                                                                   EL1325
01001      GO TO 5110-MOVE-NAME.                                        EL1325
01002                                                                   EL1325
01003  5190-EXIT.                                                       EL1325
01004      EXIT.                                                        EL1325
01005      EJECT                                                        EL1325
01006  5200-SET-ATTRB SECTION.                                          EL1325
01007                                                                   EL1325
01008      MOVE AL-SANON  TO  EL1325-NAME-ATTRB    (EL1325-INDEX)       EL1325
01009                         EL1325-AGE-ATTRB     (EL1325-INDEX)       EL1325
01010                         EL1325-STA-ATTRB     (EL1325-INDEX)       EL1325
01011                         EL1325-DATE-INCURRED-ATTRB (EL1325-INDEX) EL1325
01012                         EL1325-TYPE-ATTRB    (EL1325-INDEX)       EL1325
01013                         EL1325-CARRIER-ATTRB (EL1325-INDEX)       EL1325
01014                         EL1325-CLAIM-ATTRB   (EL1325-INDEX)       EL1325
01015                         EL1325-CERT-NO-ATTRB (EL1325-INDEX)       EL1325
01016                         EL1325-ACCOUNT-ATTRB (EL1325-INDEX).      EL1325
01017                                                                   EL1325
01018  5200-EXIT.                                                       EL1325
01019      EXIT.                                                        EL1325
01020      EJECT                                                        EL1325
01021  7000-PF2-POSITION         SECTION.                               EL1325
01022                                                                   EL1325
01023      EXEC CICS IGNORE CONDITION                                   EL1325
01024           DUPKEY                                                  EL1325
01025           END-EXEC.                                               EL1325
01026                                                                   EL1325
01027      EXEC CICS HANDLE CONDITION                                   EL1325
01028          NOTFND (8700-NOT-FOUND)                                  EL1325
01029          END-EXEC.                                                EL1325
01030                                                                   EL1325
01031      COMPUTE WS-CALC-RDNXT = PI-SCREEN-COUNT * 15.                EL1325
01032      MOVE PI-1ST-ALPH-KEY TO PI-ALPH-CLAIM-KEY.                   EL1325
01033      MOVE ZERO TO PI-END-OF-FILE.                                 EL1325
01034                                                                   EL1325
01035      IF (PI-OPTION = ZERO) OR                                     EL1325
01036         (PI-OPTION = '1') OR                                      EL1325
01037         (PI-OPTION = '4')                                         EL1325
01038          EXEC CICS STARTBR                                        EL1325
01039              DATASET   (PI-DSID)                                  EL1325
01040              RIDFLD    (PI-ALPH-CLAIM-KEY)                        EL1325
01041              KEYLENGTH (PI-KEY-LENGTH)                            EL1325
01042              GENERIC                                              EL1325
01043              EQUAL                                                EL1325
01044          END-EXEC                                                 EL1325
01045      ELSE                                                         EL1325
01046          EXEC CICS STARTBR                                        EL1325
01047              DATASET   (PI-DSID)                                  EL1325
01048              RIDFLD    (PI-ALPH-ADMIN-KEY)                        EL1325
01049              KEYLENGTH (PI-KEY-LENGTH)                            EL1325
01050              GENERIC                                              EL1325
01051              EQUAL                                                EL1325
01052          END-EXEC.                                                EL1325
01053                                                                   EL1325
01054  7100-READNEXT-PF2.                                               EL1325
01055                                                                   EL1325
01056      IF WS-CALC-RDNXT GREATER THAN ZERO                           EL1325
01057        THEN                                                       EL1325
01058          NEXT SENTENCE                                            EL1325
01059        ELSE                                                       EL1325
01060          GO TO 7999-EXIT.                                         EL1325
01061                                                                   EL1325
01062      IF (PI-OPTION = ZERO) OR                                     EL1325
01063         (PI-OPTION = '1') OR                                      EL1325
01064         (PI-OPTION = '4')                                         EL1325
01065          EXEC CICS READNEXT                                       EL1325
01066              DATASET (PI-DSID)                                    EL1325
01067              RIDFLD  (PI-ALPH-CLAIM-KEY)                          EL1325
01068              SET     (ADDRESS OF ALPHA-INDEX)                        CL**3
01069          END-EXEC                                                 EL1325
01070      ELSE                                                         EL1325
01071          EXEC CICS READNEXT                                       EL1325
01072              DATASET (PI-DSID)                                    EL1325
01073              RIDFLD  (PI-ALPH-ADMIN-KEY)                          EL1325
01074              SET     (ADDRESS OF ALPHA-INDEX)                        CL**3
01075          END-EXEC.                                                EL1325
01076                                                                   EL1325
01077 ******************************************************************EL1325
01078 *    IF THE SECURITY CHECKING ROUTINE OR THE INITIAL CHECKING    *EL1325
01079 *        IS CHANGED HERE, YOU                                    *EL1325
01080 *        MUST ALSO CHANGE THE CORRESPONDING ROUTINE IN           *EL1325
01081 *        PARAGRAPH 4020-COMPARE-KEY OR 4030-CHECK-OPTION.        *EL1325
01082 *                                     KER/080884.                *EL1325
01083 ******************************************************************EL1325
01084                                                                   EL1325
01085      IF  PI-NO-CARRIER-SECURITY                                   EL1325
01086          GO TO 7130-CHECK-INITAL.                                 EL1325
01087                                                                   EL1325
01088      IF  PI-CARRIER-SECURITY GREATER THAN SPACES                  EL1325
01089          IF  AI-CL-CARRIER = PI-CARRIER-SECURITY                  EL1325
01090              NEXT SENTENCE                                        EL1325
01091          ELSE                                                     EL1325
01092              GO TO 7100-READNEXT-PF2.                             EL1325
01093                                                                   EL1325
01094      IF  PI-CLM-CLAIM-NO  NOT EQUAL SPACES                        EL1325
01095          IF  PI-CLM-CLAIM-NO  EQUAL  AI-CL-CLAIM-NUMBER           EL1325
01096              NEXT SENTENCE                                        EL1325
01097          ELSE                                                     EL1325
01098              GO TO 7100-READNEXT-PF2.                             EL1325
01099                                                                   EL1325
01100      IF  PI-CLM-CERT-NUMBER   NOT EQUAL SPACES                    EL1325
01101          IF PI-CLM-CERT-NUMBER  =   AI-CL-CERTIFICATE-NUMBER      EL1325
01102              NEXT SENTENCE                                        EL1325
01103          ELSE                                                     EL1325
01104              GO TO 7100-READNEXT-PF2.                             EL1325
01105                                                                   EL1325
01106  7130-CHECK-INITAL.                                               EL1325
01107                                                                   EL1325
01108      IF  PI-OPTION  NOT EQUAL '1'                                 EL1325
01109          GO TO 7190-COMPUTE.                                      EL1325
01110                                                                   EL1325
01111      IF PI-ALPH-FRST-NAME NOT EQUAL TO SPACES                     EL1325
01112         IF PI-ALPH-FRST-NAME NOT EQUAL AI-FIRST-NAME              EL1325
01113            GO TO 7100-READNEXT-PF2.                               EL1325
01114                                                                   EL1325
01115      IF PI-ALPH-MID-INIT NOT EQUAL TO SPACES                      EL1325
01116          IF PI-ALPH-MID-INIT NOT EQUAL TO AI-MIDDLE-INIT          EL1325
01117              GO TO 7100-READNEXT-PF2.                             EL1325
01118                                                                   EL1325
01119  7190-COMPUTE.                                                    EL1325
01120                                                                   EL1325
01121      COMPUTE WS-CALC-RDNXT = WS-CALC-RDNXT - 1.                   EL1325
01122      GO TO 7100-READNEXT-PF2.                                     EL1325
01123                                                                   EL1325
01124  7999-EXIT.                                                       EL1325
01125      EXIT.                                                        EL1325
01126      EJECT                                                        EL1325
01127  8000-READ-CONTROL-FILE SECTION.                                  EL1325
01128                                                                   EL1325
01129      MOVE ZERO                   TO  WS-NOT-FOUND.                EL1325
01130      EJECT                                                        EL1325
01131  8100-SEND-INITIAL-MAP SECTION.                                   EL1325
01132                                                                   EL1325
01133      MOVE SAVE-DATE              TO  ADATEO.                      EL1325
01134      MOVE EIBTIME                TO  TIME-IN.                     EL1325
01135      MOVE TIME-OUT               TO  ATIMEO.                      EL1325
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01136                                                                   EL1325
01137      IF EMI-ERROR NOT EQUAL TO ZERO                               EL1325
01138          PERFORM 9900-ERROR-FORMAT.                               EL1325
01139                                                                   EL1325
01140      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                    EL1325
01141                                                                   EL1325
01142      IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES                   EL1325
01143         MOVE AL-PABON TO APFK5A APFK6A                            EL1325
01144         MOVE PI-COMPANY-ID TO ACOMPO                              EL1325
01145      ELSE                                                         EL1325
01146         MOVE SPACES TO ACOMPO                                     EL1325
01147         MOVE AL-PADOF TO APFK5A APFK6A.                           EL1325
01148                                                                   EL1325
01149      EXEC CICS SEND                                               EL1325
01150          FROM   (EL1325AI)                                        EL1325
01151          MAPSET (WS-MAPSET-NAME)                                  EL1325
01152          MAP    (WS-MAP-NAME)                                     EL1325
01153          CURSOR                                                   EL1325
01154          ERASE                                                    EL1325
01155      END-EXEC.                                                    EL1325
01156                                                                   EL1325
01157      GO TO 9100-RETURN-TRAN.                                      EL1325
01158                                                                   EL1325
01159  8100-EXIT.                                                       EL1325
01160      EXIT.                                                        EL1325
01161                                                                   EL1325
01162      EJECT                                                        EL1325
01163  8200-SEND-DATAONLY SECTION.                                      EL1325
01164                                                                   EL1325
01165                                                                   EL1325
01166      MOVE SAVE-DATE              TO  ADATEO.                      EL1325
01167      MOVE EIBTIME                TO  TIME-IN.                     EL1325
01168      MOVE TIME-OUT               TO  ATIMEO.                      EL1325
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01169                                                                   EL1325
01170      IF EMI-ERROR NOT EQUAL TO ZERO                               EL1325
01171          PERFORM 9900-ERROR-FORMAT.                               EL1325
01172                                                                   EL1325
01173      MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.                    EL1325
01174                                                                   EL1325
01175      IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES                   EL1325
01176         MOVE AL-PABON TO APFK5A APFK6A                            EL1325
01177         MOVE PI-COMPANY-ID TO ACOMPO                              EL1325
01178      ELSE                                                         EL1325
01179         MOVE SPACES TO ACOMPO                                     EL1325
01180         MOVE AL-PADOF TO APFK5A APFK6A.                           EL1325
01181                                                                   EL1325
01182                                                                   EL1325
01183      EXEC CICS SEND DATAONLY                                      EL1325
01184          FROM   (EL1325AI)                                        EL1325
01185          MAPSET (WS-MAPSET-NAME)                                  EL1325
01186          MAP    (WS-MAP-NAME)                                     EL1325
01187          CURSOR                                                   EL1325
01188      END-EXEC.                                                    EL1325
01189                                                                   EL1325
01190      GO TO 9100-RETURN-TRAN.                                      EL1325
01191                                                                   EL1325
01192  8200-EXIT.                                                       EL1325
01193      EXIT.                                                        EL1325
01194                                                                   EL1325
01195      EJECT                                                        EL1325
01196  8300-SEND-TEXT SECTION.                                          EL1325
01197                                                                   EL1325
01198      EXEC CICS SEND TEXT                                          EL1325
01199          FROM   (LOGOFF-TEXT)                                     EL1325
01200          LENGTH (LOGOFF-LENGTH)                                   EL1325
01201          ERASE                                                    EL1325
01202          FREEKB                                                   EL1325
01203      END-EXEC.                                                    EL1325
01204                                                                   EL1325
01205      EXEC CICS RETURN                                             EL1325
01206          END-EXEC.                                                EL1325
01207                                                                   EL1325
01208  8300-EXIT.                                                       EL1325
01209      EXIT.                                                        EL1325
01210                                                                   EL1325
01211      EJECT                                                        EL1325
01212  8500-DATE-CONVERSION SECTION.                                    EL1325
01213                                                                   EL1325
01214      EXEC CICS LINK                                               EL1325
01215          PROGRAM  ('ELDATCV')                                     EL1325
01216          COMMAREA (DATE-CONVERSION-DATA)                          EL1325
01217          LENGTH   (DC-COMM-LENGTH)                                EL1325
01218      END-EXEC.                                                    EL1325
01219                                                                   EL1325
01220  8500-EXIT.                                                       EL1325
01221      EXIT.                                                        EL1325
01222                                                                   EL1325
01223      EJECT                                                        EL1325
01224  8600-NEXT-COMPANY SECTION.                                       EL1325
01225 ******************************************************************EL1325
01226 ****      READ THE CURRENT COMPANY RECORD TO OBTAIN THE       ****EL1325
01227 ****      NEXT COMPANY ID.                                    ****EL1325
01228 ******************************************************************EL1325
01229                                                                   EL1325
01230      MOVE SPACES                     TO  WS-CLCNTL-KEY.           EL1325
01231      MOVE PI-COMPANY-ID              TO  WS-CLCNTL-ID.            EL1325
01232      MOVE '1'                        TO  WS-CLCNTL-TYPE.          EL1325
01233      MOVE +0                         TO  WS-CLCNTL-SEQ.           EL1325
01234                                                                   EL1325
01235      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.                    EL1325
01236                                                                   EL1325
01237      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                      EL1325
01238          PERFORM 8800-INITIALIZE-MAP VARYING EL1325-INDEX         EL1325
01239            FROM +1 BY +1 UNTIL EL1325-INDEX IS GREATER THAN +16   EL1325
01240          MOVE ER-0022                 TO  EMI-ERROR               EL1325
01241          MOVE -1                      TO  ASELL                   EL1325
01242          GO TO 8100-SEND-INITIAL-MAP.                             EL1325
01243                                                                   EL1325
01244      IF EIBAID = DFHPF5                                           EL1325
01245          MOVE CF-NEXT-COMPANY-ID      TO  WS-NEXT-COMPANY-ID.     EL1325
01246                                                                   EL1325
01247      IF EIBAID = DFHPF6                                           EL1325
01248          MOVE PI-ORIGINAL-COMPANY-ID  TO  WS-NEXT-COMPANY-ID.     EL1325
01249                                                                   EL1325
01250      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                        EL1325
01251          GO TO 8600-CONTINUE-NEXT-COMPANY.                        EL1325
01252                                                                   EL1325
01253 ******************************************************************EL1325
01254 ****      READ THE CURRENT USER RECORD FOR UPDATE AND REMOVE  ****EL1325
01255 ****      THE TERMINAL ID FROM THE RECORD.                    ****EL1325
01256 ******************************************************************EL1325
01257                                                                   EL1325
01258      MOVE PI-COMPANY-ID               TO  WS-CLCNTL-ID.           EL1325
01259      MOVE '2'                         TO  WS-CLCNTL-TYPE.         EL1325
01260      MOVE PI-PROCESSOR-ID             TO  WS-CLCNTL-USER.         EL1325
01261      MOVE +0                          TO  WS-CLCNTL-SEQ.          EL1325
01262                                                                   EL1325
01263      PERFORM 8910-READ-CONTROL-UPDATE THRU 8910-EXIT.             EL1325
01264                                                                   EL1325
01265      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                      EL1325
01266          MOVE ER-0019                 TO  EMI-ERROR               EL1325
01267          MOVE -1                      TO  ASELL                   EL1325
01268          GO TO 8200-SEND-DATAONLY.                                EL1325
01269                                                                   EL1325
01270      MOVE SPACES                      TO  CF-CURRENT-TERM-ON.     EL1325
01271                                                                   EL1325
01272      PERFORM 8920-REWRITE-CONTROL THRU 8920-EXIT.                 EL1325
01273                                                                   EL1325
01274 ******************************************************************EL1325
01275 ****      READ THE USER RECORD ON THE "NEXT" COMPANY TO       ****EL1325
01276 ****      VERIFY THAT A VALID USER RECORD EXISTS:             ****EL1325
01277 ****        1.  MOVE USER CARRIER/ACCOUNT SECURITY TO PI-AREA ****EL1325
01278 ****        2.  MOVE USER SECURITY VALUES TO SECURITY CODES   ****EL1325
01279 ****            IN WORKING STORAGE.                           ****EL1325
01280 ******************************************************************EL1325
01281                                                                   EL1325
01282      MOVE WS-NEXT-COMPANY-ID         TO  WS-CLCNTL-ID.            EL1325
01283      MOVE '2'                        TO  WS-CLCNTL-TYPE.          EL1325
01284      MOVE PI-PROCESSOR-ID            TO  WS-CLCNTL-USER.          EL1325
01285      MOVE +0                         TO  WS-CLCNTL-SEQ.           EL1325
01286                                                                   EL1325
01287      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.                    EL1325
01288                                                                   EL1325
01289      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                      EL1325
01290          PERFORM 8800-INITIALIZE-MAP VARYING EL1325-INDEX         EL1325
01291             FROM +1 BY +1 UNTIL EL1325-INDEX IS GREATER THAN +16  EL1325
01292          MOVE ER-0228                TO  EMI-ERROR                EL1325
01293          MOVE -1                     TO  ASELL                    EL1325
01294          GO TO 8100-SEND-INITIAL-MAP.                             EL1325
01295                                                                   EL1325
01296      MOVE CF-PROCESSOR-CARRIER       TO  PI-CARRIER-SECURITY.     EL1325
01297      MOVE CF-PROCESSOR-ACCOUNT       TO  PI-ACCOUNT-SECURITY.     EL1325
01298      MOVE CF-INDIVIDUAL-APP (1)      TO  SC-CREDIT-CODES.         EL1325
01299      MOVE CF-INDIVIDUAL-APP (2)      TO  SC-CLAIMS-CODES.         EL1325
01300      MOVE SC-CLAIMS-DISPLAY (21)     TO  PI-DISPLAY-CAP.          EL1325
01301      MOVE SC-CLAIMS-UPDATE  (21)     TO  PI-MODIFY-CAP.           EL1325
01302                                                                   EL1325
01303 ******************************************************************EL1325
01304 ****      READ THE USER RECORD ON THE "NEXT" COMPANY FOR      ****EL1325
01305 ****      UPDATE AND MOVE THE TERMINAL ID INTO THE RECORD.    ****EL1325
01306 ******************************************************************EL1325
01307                                                                   EL1325
01308      MOVE WS-NEXT-COMPANY-ID         TO  WS-CLCNTL-ID.            EL1325
01309      MOVE '2'                        TO  WS-CLCNTL-TYPE.          EL1325
01310      MOVE PI-PROCESSOR-ID            TO  WS-CLCNTL-USER.          EL1325
01311      MOVE +0                         TO  WS-CLCNTL-SEQ.           EL1325
01312                                                                   EL1325
01313      PERFORM 8910-READ-CONTROL-UPDATE THRU 8910-EXIT.             EL1325
01314                                                                   EL1325
01315      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                      EL1325
01316          MOVE ER-0228                TO  EMI-ERROR                EL1325
01317          MOVE -1                     TO  ASELL                    EL1325
01318          GO TO 8200-SEND-DATAONLY.                                EL1325
01319                                                                   EL1325
01320      MOVE EIBTRMID                   TO  CF-CURRENT-TERM-ON.      EL1325
01321                                                                   EL1325
01322      PERFORM 8920-REWRITE-CONTROL THRU 8920-EXIT.                 EL1325
01323                                                                   EL1325
01324  8600-CONTINUE-NEXT-COMPANY.                                      EL1325
01325 ******************************************************************EL1325
01326 ****      READ THE NEW COMPANY RECORD TO VERIFY THAT IT       ****EL1325
01327 ****      EXISTS AND THEN MOVE SPECIFIC DATA TO PI-AREA.      ****EL1325
01328 ******************************************************************EL1325
01329                                                                   EL1325
01330      MOVE SPACES                     TO  WS-CLCNTL-KEY.           EL1325
01331      MOVE WS-NEXT-COMPANY-ID         TO  WS-CLCNTL-ID.            EL1325
01332      MOVE '1'                        TO  WS-CLCNTL-TYPE.          EL1325
01333      MOVE +0                         TO  WS-CLCNTL-SEQ.           EL1325
01334                                                                   EL1325
01335      PERFORM 8900-READ-CONTROL THRU 8900-EXIT.                    EL1325
01336                                                                   EL1325
01337      IF WS-CNTL-REC-FOUND-SW IS EQUAL TO 'N'                      EL1325
01338          PERFORM 8800-INITIALIZE-MAP VARYING EL1325-INDEX         EL1325
01339             FROM +1 BY +1 UNTIL EL1325-INDEX IS GREATER THAN +16  EL1325
01340          MOVE ER-0089                TO  EMI-ERROR                EL1325
01341          MOVE -1                     TO  ASELL                    EL1325
01342          GO TO 8100-SEND-INITIAL-MAP.                             EL1325
01343                                                                   EL1325
01344      MOVE SPACES                     TO  PI-PROGRAM-WORK-AREA.    EL1325
01345                                                                   EL1325
01346      MOVE +1                         TO  PI-START-SW              EL1325
01347                                          PI-KEY-LENGTH.           EL1325
01348                                                                   EL1325
01349      MOVE +0                         TO  PI-1ST-TIME-SW           EL1325
01350                                          PI-LINE-COUNT            EL1325
01351                                          PI-AIX-RECORD-COUNT      EL1325
01352                                          PI-BROWSE-SW             EL1325
01353                                          PI-END-OF-FILE           EL1325
01354                                          PI-TS-ITEM               EL1325
01355                                          PI-SCREEN-COUNT.         EL1325
01356                                                                   EL1325
01357      MOVE ALPHA-INDEX-DSID           TO  PI-DSID.                 EL1325
01358      MOVE CF-COMPANY-CD              TO  PI-COMPANY-CD            EL1325
01359                                          PI-CK-COMPANY-CD         EL1325
01360                                          PI-1ST-KEY.              EL1325
01361      MOVE CF-COMPANY-ID              TO  PI-COMPANY-ID.           EL1325
01362      MOVE CF-COMPANY-PASSWORD        TO  PI-COMPANY-PASSWORD.     EL1325
01363      MOVE CF-LGX-CREDIT-USER         TO  PI-CREDIT-USER.          EL1325
01364      MOVE CF-LGX-CLAIM-USER          TO  PI-CLAIM-USER.           EL1325
01365      MOVE CF-CERT-ACCESS-CONTROL     TO  PI-CERT-ACCESS-CONTROL.  EL1325
01366      MOVE CF-CARRIER-CONTROL-LEVEL   TO  PI-CARRIER-CONTROL-LEVEL.EL1325
01367      MOVE CF-JOURNAL-FILE-ID         TO  PI-JOURNAL-FILE-ID.      EL1325
01368      MOVE CF-LOWER-CASE-LETTERS      TO  PI-LOWER-CASE-LETTERS.   EL1325
01369      MOVE CF-CLAIM-PAID-THRU-TO      TO  PI-CLAIM-PAID-THRU-TO.   EL1325
01370                                                                   EL1325
01371      MOVE CF-LIFE-OVERRIDE-L1        TO  PI-LIFE-OVERRIDE-L1.     EL1325
01372      MOVE CF-LIFE-OVERRIDE-L2        TO  PI-LIFE-OVERRIDE-L2.     EL1325
01373      MOVE CF-LIFE-OVERRIDE-L6        TO  PI-LIFE-OVERRIDE-L6.     EL1325
01374      MOVE CF-LIFE-OVERRIDE-L12       TO  PI-LIFE-OVERRIDE-L12.    EL1325
01375                                                                   EL1325
01376      MOVE CF-AH-OVERRIDE-L1          TO  PI-AH-OVERRIDE-L1.       EL1325
01377      MOVE CF-AH-OVERRIDE-L2          TO  PI-AH-OVERRIDE-L2.       EL1325
01378      MOVE CF-AH-OVERRIDE-L6          TO  PI-AH-OVERRIDE-L6.       EL1325
01379      MOVE CF-AH-OVERRIDE-L12         TO  PI-AH-OVERRIDE-L12.      EL1325
01380                                                                      CL**2
01381      IF  CLAIM-SESSION                                               CL**2
01382          MOVE CF-PRINT-ADDRESS-LABELS                                CL**2
01383                                      TO  PI-LABEL-CONTROL            CL**2
01384                                                                      CL**2
01385      ELSE                                                            CL**2
01386          IF  CREDIT-SESSION                                          CL**2
01387              MOVE CF-CR-PRINT-ADDRESS-LABELS                         CL**2
01388                                      TO  PI-LABEL-CONTROL.           CL**2
01389                                                                   EL1325
01390  8600-EXIT.                                                       EL1325
01391      EXIT.                                                        EL1325
01392                                                                   EL1325
01393      EJECT                                                        EL1325
01394  8650-WRITE-SECURITY-TEMP-STORE  SECTION.                         EL1325
01395                                                                   EL1325
01396      EXEC CICS HANDLE CONDITION                                   EL1325
01397          QIDERR  (8651-WRITE-SECURITY)                            EL1325
01398      END-EXEC.                                                    EL1325
01399                                                                   EL1325
01400      MOVE EIBTRMID               TO  QID.                         EL1325
01401                                                                   EL1325
01402      EXEC CICS DELETEQ TS                                         EL1325
01403          QUEUE   (QID)                                            EL1325
01404      END-EXEC.                                                    EL1325
01405                                                                   EL1325
01406  8651-WRITE-SECURITY.                                             EL1325
01407                                                                   EL1325
01408      EXEC CICS WRITEQ TS                                          EL1325
01409          QUEUE   (QID)                                            EL1325
01410          FROM    (SECURITY-CONTROL)                               EL1325
01411          LENGTH  (SC-COMM-LENGTH)                                 EL1325
01412          ITEM    (QID-ITEM)                                       EL1325
01413      END-EXEC.                                                    EL1325
01414                                                                   EL1325
01415      MOVE QID                    TO  PI-SECURITY-TEMP-STORE-ID.   EL1325
01416                                                                   EL1325
01417      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'                        EL1325
01418          MOVE ALL 'Y'            TO  SC-CREDIT-CODES              EL1325
01419                                      SC-CLAIMS-CODES              EL1325
01420                                      PI-PROCESSOR-USER-ALMIGHTY.  EL1325
01421                                                                   EL1325
01422  8650-EXIT.                                                       EL1325
01423      EXIT.                                                        EL1325
01424                                                                   EL1325
01425      EJECT                                                        EL1325
01426  8700-NOT-FOUND SECTION.                                          EL1325
01427      PERFORM 8800-INITIALIZE-MAP VARYING EL1325-INDEX             EL1325
01428        FROM +1 BY +1 UNTIL EL1325-INDEX GREATER THAN +16.         EL1325
01429      MOVE -1 TO ASELL.                                            EL1325
01430      MOVE ER-0284                TO EMI-ERROR.                    EL1325
01431      GO TO 8100-SEND-INITIAL-MAP.                                 EL1325
01432  8700-EXIT.                                                       EL1325
01433      EXIT.                                                        EL1325
01434                                                                   EL1325
01435  8800-INITIALIZE-MAP SECTION.                                     EL1325
01436      MOVE LOW-VALUES TO EL1325-MAP-LINE (EL1325-INDEX).           EL1325
01437  8800-EXIT.                                                       EL1325
01438      EXIT.                                                        EL1325
01439                                                                   EL1325
01440      EJECT                                                        EL1325
01441  8900-READ-CONTROL SECTION.                                       EL1325
01442                                                                   EL1325
01443      EXEC CICS HANDLE CONDITION                                   EL1325
01444          NOTFND   (8900-NOTFND)                                   EL1325
01445      END-EXEC.                                                    EL1325
01446                                                                   EL1325
01447      EXEC CICS READ                                               EL1325
01448          DATASET   (ELCNTL-FILE-ID)                               EL1325
01449          RIDFLD    (WS-CLCNTL-KEY)                                EL1325
01450          SET       (ADDRESS OF CONTROL-FILE)                         CL**3
01451      END-EXEC.                                                    EL1325
01452                                                                   EL1325
01453      MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.        EL1325
01454      GO TO 8900-EXIT.                                             EL1325
01455                                                                   EL1325
01456  8900-NOTFND.                                                     EL1325
01457                                                                   EL1325
01458      MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.        EL1325
01459                                                                   EL1325
01460  8900-EXIT.                                                       EL1325
01461      EXIT.                                                        EL1325
01462                                                                   EL1325
01463  8910-READ-CONTROL-UPDATE.                                        EL1325
01464                                                                   EL1325
01465      EXEC CICS HANDLE CONDITION                                   EL1325
01466          NOTFND   (8910-NOTFND)                                   EL1325
01467      END-EXEC.                                                    EL1325
01468                                                                   EL1325
01469      EXEC CICS READ                                               EL1325
01470          DATASET   (ELCNTL-FILE-ID)                               EL1325
01471          RIDFLD    (WS-CLCNTL-KEY)                                EL1325
01472          SET       (ADDRESS OF CONTROL-FILE)                         CL**3
01473          UPDATE                                                   EL1325
01474      END-EXEC.                                                    EL1325
01475                                                                   EL1325
01476      MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.        EL1325
01477      GO TO 8910-EXIT.                                             EL1325
01478                                                                   EL1325
01479  8910-NOTFND.                                                     EL1325
01480      MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.        EL1325
01481                                                                   EL1325
01482  8910-EXIT.                                                       EL1325
01483      EXIT.                                                        EL1325
01484                                                                   EL1325
01485  8920-REWRITE-CONTROL.                                            EL1325
01486                                                                   EL1325
01487      EXEC CICS REWRITE                                            EL1325
01488          DATASET   (ELCNTL-FILE-ID)                               EL1325
01489          FROM      (CONTROL-FILE)                                 EL1325
01490      END-EXEC.                                                    EL1325
01491                                                                   EL1325
01492  8920-EXIT.                                                       EL1325
01493      EXIT.                                                        EL1325
01494                                                                   EL1325
01495      EJECT                                                        EL1325
01496  9000-RETURN-CICS SECTION.                                        EL1325
01497                                                                   EL1325
01498      MOVE 'EL005   '             TO  THIS-PGM.                    EL1325
01499      MOVE EIBAID                 TO  PI-ENTRY-CD-1                EL1325
01500      GO TO 9300-XCTL.                                             EL1325
01501                                                                   EL1325
01502  9000-EXIT.                                                       EL1325
01503      EXIT.                                                        EL1325
01504                                                                   EL1325
01505  9100-RETURN-TRAN SECTION.                                        EL1325
01506                                                                   EL1325
01507      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO             EL1325
01508      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO         EL1325
01509      MOVE EIBAID                 TO  PI-LAST-EIBAID               EL1325
01510                                                                   EL1325
01511      EXEC CICS RETURN                                             EL1325
01512          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1325
01513          LENGTH   (PI-COMM-LENGTH)                                EL1325
01514          TRANSID  (WS-TRANS-ID)                                   EL1325
01515      END-EXEC.                                                    EL1325
01516                                                                   EL1325
01517  9100-EXIT.                                                       EL1325
01518      EXIT.                                                        EL1325
01519                                                                   EL1325
01520  9300-XCTL SECTION.                                               EL1325
01521                                                                   EL1325
01522      MOVE DFHENTER               TO  EIBAID                       EL1325
01523                                                                   EL1325
01524      EXEC CICS XCTL                                               EL1325
01525          PROGRAM  (THIS-PGM)                                      EL1325
01526          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1325
01527          LENGTH   (PI-COMM-LENGTH)                                EL1325
01528      END-EXEC.                                                    EL1325
01529                                                                   EL1325
01530  9300-EXIT.                                                       EL1325
01531      EXIT.                                                        EL1325
01532                                                                   EL1325
01533      EJECT                                                        EL1325
01534  9400-CLEAR SECTION.                                              EL1325
01535                                                                   EL1325
01536      IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM                   EL1325
01537          MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM         EL1325
01538          MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM       EL1325
01539          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1         EL1325
01540          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2         EL1325
01541          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3         EL1325
01542          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4         EL1325
01543          MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5         EL1325
01544          MOVE SPACES               TO  PI-SAVED-PROGRAM-6         EL1325
01545          MOVE PI-RETURN-TO-PROGRAM TO  THIS-PGM.                  EL1325
01546                                                                   EL1325
01547      GO TO 9300-XCTL.                                             EL1325
01548                                                                   EL1325
01549  9400-EXIT.                                                       EL1325
01550      EXIT.                                                        EL1325
01551                                                                   EL1325
01552  9600-PGMIDERR SECTION.                                           EL1325
01553                                                                   EL1325
01554      EXEC CICS HANDLE CONDITION                                   EL1325
01555          PGMIDERR (8300-SEND-TEXT)                                EL1325
01556      END-EXEC.                                                    EL1325
01557                                                                   EL1325
01558      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL1325
01559                                                                   EL1325
01560      MOVE 'EL005   '             TO  THIS-PGM                     EL1325
01561                                      LOGOFF-PGM                   EL1325
01562      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL                  EL1325
01563      MOVE SPACES                 TO  PI-ENTRY-CD-1                EL1325
01564      GO TO 9300-XCTL.                                             EL1325
01565                                                                   EL1325
01566  9600-EXIT.                                                       EL1325
01567      EXIT.                                                        EL1325
01568                                                                   EL1325
01569      EJECT                                                        EL1325
01570  9900-ERROR-FORMAT SECTION.                                       EL1325
01571                                                                   EL1325
01572      EXEC CICS LINK                                               EL1325
01573          PROGRAM  ('EL001')                                       EL1325
01574          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL1325
01575          LENGTH   (EMI-COMM-LENGTH)                               EL1325
01576      END-EXEC.                                                    EL1325
01577                                                                   EL1325
01578      MOVE ER-0000                   TO  EMI-ERROR.                EL1325
01579                                                                   EL1325
01580  9900-EXIT.                                                       EL1325
01581      EXIT.                                                        EL1325
01582                                                                   EL1325
01583  9990-ERROR SECTION.                                              EL1325
01584                                                                   EL1325
01585      MOVE DFHEIBLK TO EMI-LINE1.                                  EL1325
01586      EXEC CICS LINK                                               EL1325
01587          PROGRAM  ('EL004')                                       EL1325
01588          COMMAREA (EMI-LINE1)                                     EL1325
01589          LENGTH   (72)                                            EL1325
01590      END-EXEC.                                                    EL1325
01591                                                                   EL1325
01592      GO TO 8100-SEND-INITIAL-MAP.                                 EL1325
01593                                                                   EL1325
01594  9990-EXIT.                                                       EL1325
01595      EXIT.                                                        EL1325
01596                                                                   EL1325
01597  9995-SECURITY-VIOLATION SECTION.                                 EL1325
01598                                  COPY ELCSCTP.                    EL1325
01599                                                                   EL1325
01600  9995-EXIT.                                                       EL1325
01601      EXIT.                                                        EL1325
01602                                                                   EL1325
