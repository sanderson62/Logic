00001  IDENTIFICATION DIVISION.                                         06/26/96
00002                                                                   EL174
00003  PROGRAM-ID.                 EL174 .                                 LV008
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 02/25/96 01:08:24.                    CL**7
00007 *                            VMOD=2.008                              CL**8
00008 *                                                                 EL174
00008 *                                                                 EL174
00009 *AUTHOR.    LOGIC, INC.                                              CL**7
00010 *           DALLAS, TEXAS.                                           CL**7
00011                                                                   EL174
00012 *DATE-COMPILED.                                                      CL**7
00013                                                                   EL174
00014 *SECURITY.   *****************************************************   CL**7
00015 *            *                                                   *   CL**7
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00017 *            *                                                   *   CL**7
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00021 *            *                                                   *   CL**7
00022 *            *****************************************************   CL**7
00023                                                                   EL174
00024 *REMARKS.                                                            CL**2
00025 *        THIS PROGRAM PRODUCES A REPORT SHOWING ALL CHECKS THAT      CL**2
00026 *    ARE WAITING TO BE PRINTED.                                      CL**2
00027                                                                   EL174
00028 *    SCREENS     - EL174A - REQUEST FOR REVIEW                       CL**2
00029                                                                   EL174
00030 *    ENTERED BY  - EL171  - REPORT MENU                              CL**2
00031                                                                   EL174
00032 *    EXIT TO     - EL171  - RESULT OF CLEAR OR END OF JOB            CL**2
00033                                                                   EL174
00034                                                                   EL174
00035      EJECT                                                        EL174
00036  ENVIRONMENT DIVISION.                                            EL174
00037                                                                   EL174
00038  DATA DIVISION.                                                   EL174
00039                                                                   EL174
00040  WORKING-STORAGE SECTION.                                         EL174
00041                                                                   EL174
00042  77  FILLER  PIC X(32)  VALUE '********************************'. EL174
00043  77  FILLER  PIC X(32)  VALUE '*   EL174  WORKING STORAGE     *'. EL174
00044  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.008 *********'.    CL**8
00045                                                                   EL174
00046                              COPY ELCSCTM.                           CL**5
00047                                                                   EL174
00048                              COPY ELCSCRTY.                          CL**5
00049                                                                   EL174
00050  01  WS-DATE-AREA.                                                EL174
00051      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL174
00052      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL174
00053                                                                   EL174
00054  01  FILLER                          COMP-3.                      EL174
00055      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.  EL174
00056                                                                   EL174
00057      05  TIME-IN                     PIC S9(7)       VALUE ZERO.  EL174
00058      05  TIME-OUT                    REDEFINES                    EL174
00059          TIME-IN                     PIC S9(3)V9(4).              EL174
00060                                                                   EL174
00061  01  FILLER                          COMP SYNC.                   EL174
00062      05  WS-TS-LENGTH                PIC S9(4)       VALUE +1920. EL174
00063      05  SC-ITEM                     PIC S9(4)       VALUE +0001. EL174
00064                                                                   EL174
00065  01  FILLER.                                                      EL174
00066      05  WS-ACTIVITY-TRAILERS-KEY.                                EL174
00067          10  WS-ATK-COMPANY-CD       PIC X.                       EL174
00068          10  WS-ATK-CARRIER          PIC X.                       EL174
00069          10  WS-ATK-CLAIM-NO         PIC X(7).                    EL174
00070          10  WS-ATK-CERT-NO          PIC X(11).                   EL174
00071          10  WS-ATK-SEQUENCE-NO      PIC S9(4)                    EL174
00072                                      COMP.                        EL174
00073                                                                   EL174
00074      05  CNTL-KEY.                                                EL174
00075          10  CNTL-CO             PIC X(3).                        EL174
00076          10  CNTL-RECORD-TYPE    PIC X       VALUE '1'.           EL174
00077          10  CNTL-GENL           PIC X(4).                        EL174
00078          10  CNTL-SEQ            PIC S9(4)   VALUE +0    COMP.    EL174
00079                                                                   EL174
00080      05  WS-START-CNTLNO         PIC S9(8)   VALUE +0    COMP.    EL174
00081                                                                   EL174
00082      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL174S'.EL174
00083      05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL174A'.EL174
00084                                                                   EL174
00085      05  FILLER                      REDEFINES                    EL174
00086          WS-MAP-NAME.                                             EL174
00087          20  FILLER                  PIC XX.                      EL174
00088          20  WS-MAP-NUMBER           PIC X(6).                    EL174
00089                                                                   EL174
00090      05  THIS-PGM                    PIC X(8)      VALUE 'EL174'. EL174
00091                                                                   EL174
00092      05  WS-CHECK-QUEUE-DSID        PIC X(8) VALUE 'ELCHKQ'.      EL174
00093      05  WS-CHECK-QUEUE-AIX-DSID    PIC X(8) VALUE 'ELCHKQ2'.        CL**6
00094      05  WS-ACTIVITY-TRAILERS-DSID  PIC X(8) VALUE 'ELTRLR'.      EL174
00095      05  WS-CONTROL-DSID            PIC X(8) VALUE 'ELCNTL'.      EL174
00096                                                                   EL174
00097      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EX04'.EL174
00098      05  WS-PRINT-TRAN-ID            PIC X(4)        VALUE 'EX64'.EL174
00099      05  WS-PRINTER-ID               PIC X(4).                    EL174
00100                                                                   EL174
00101      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70    EL174
00102                                      COMP                         EL174
00103                                      SYNCHRONIZED.                EL174
00104                                                                   EL174
00105      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.EL174
00106                                                                   EL174
00107      05  WS-SUCCESSFUL-MESSAGE       PIC X(79)       VALUE        EL174
00108          '0000 TRANSACTION SUCCESSFUL'.                           EL174
00109                                                                   EL174
00110      05  WS-TO-BE-PRINTED-DESC       PIC X(24)       VALUE        EL174
00111          '- CHECKS TO BE PRINTED -'.                              EL174
00112                                                                   EL174
00113      05  WS-CHECKS-PRINTED-DESC      PIC X(24)       VALUE        EL174
00114          '-    PRINTED CHECKS    -'.                              EL174
00115                                                                   EL174
00116      05  WS-TO-BE-PRINTED-PFDESC     PIC X(29)       VALUE        EL174
00117          'PF4=LIST PRINTED CHECKS      '.                         EL174
00118                                                                   EL174
00119      05  WS-CHECKS-PRINTED-PFDESC    PIC X(29)       VALUE        EL174
00120          'PF4=LIST TO BE PRINTED CHECKS'.                         EL174
00121                                                                   EL174
00122      05  WS-SAVE-CHECK-MODE          PIC X           VALUE SPACE. EL174
00123                                                                   EL174
00124      05  WS-TEMP-STORAGE-KEY.                                     EL174
00125          10  WS-TS-TERM-ID           PIC X(4).                    EL174
00126          10  FILLER                  PIC X(4)        VALUE '174'. EL174
00127                                                                   EL174
00128      05  WS-TEMP-STORAGE-ITEM        PIC S9(4)       VALUE ZERO   EL174
00129                                      COMP SYNC.                   EL174
00130                                                                   EL174
00131      EJECT                                                        EL174
00132  01  ERROR-MESSAGES.                                              EL174
00133      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL174
00134      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL174
00135      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL174
00136      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL174
00137      12  ER-0312                 PIC X(4)  VALUE '0312'.          EL174
00138      12  ER-0375                 PIC X(4)  VALUE '0375'.          EL174
00139      12  ER-0412                 PIC X(4)  VALUE '0412'.          EL174
00140      12  ER-0413                 PIC X(4)  VALUE '0413'.          EL174
00141      12  ER-0567                 PIC X(4)  VALUE '0567'.          EL174
00142      12  ER-2295                 PIC X(4)  VALUE '2295'.          EL174
00143      EJECT                                                        EL174
00144                                  COPY ELCINTF.                       CL**5
00145      12  FILLER                      REDEFINES                    EL174
00146          PI-PROGRAM-WORK-AREA.                                    EL174
00147          16  PI-CHECK-AIX-KEY.                                       CL**6
00148              20  PI-CK-COMPANY-CODE     PIC X.                    EL174
00149              20  PI-CK-CONTROL-NO       PIC S9(8)          COMP.  EL174
00150              20  PI-CK-CARRIER          PIC X(01).                   CL**6
00151              20  PI-CK-GROUPING         PIC X(06).                   CL**6
00152              20  PI-CK-STATE            PIC X(02).                   CL**6
00153              20  PI-CK-BENE-ACCT        PIC X(10).                   CL**6
00154              20  PI-CK-SEQUENCE-NO      PIC S9(4)          COMP.  EL174
00155                                                                   EL174
00156          16  PI-PREV-CHECK-AIX-KEY.                                  CL**6
00157              20  PI-PREV-CK-COMPANY-CODE     PIC X.               EL174
00158              20  PI-PREV-CK-CONTROL-NO       PIC S9(8) COMP.      EL174
00159              20  PI-PREV-CK-CARRIER          PIC X(01).              CL**6
00160              20  PI-PREV-CK-GROUPING         PIC X(06).              CL**6
00161              20  PI-PREV-CK-STATE            PIC X(02).              CL**6
00162              20  PI-PREV-CK-BENE-ACCT        PIC X(10).              CL**6
00163              20  PI-PREV-CK-SEQUENCE-NO      PIC S9(4) COMP.      EL174
00164                                                                   EL174
00165          16  PI-TEMP-STORAGE-ITEM    PIC S9(4) COMP SYNC.         EL174
00166                                                                   EL174
00167          16  PI-END-OF-FILE          PIC S9    COMP-3.            EL174
00168                                                                   EL174
00169          16 PI-CONTROL-TOTALS.                                    EL174
00170             20 PI-CONTROL-TOT        PIC S9(7)V99 COMP-3.         EL174
00171             20 PI-CONTROL-GRAND-TOT  PIC S9(7)V99 COMP-3.         EL174
00172             20 PI-CONTROL-SAVE-CONTROL                            EL174
00173                                      PIC S9(8) COMP.              EL174
00174          16 PI-FIRST-TIME-SWT        PIC X.                       EL174
00175             88 PI-FIRST-TIME         VALUE 'Y'.                   EL174
00176             88 PI-NOT-FIRST-TIME     VALUE 'N'.                   EL174
00177          16 PI-EOF-SWT               PIC X.                       EL174
00178             88 PI-EOF                VALUE 'Y'.                   EL174
00179             88 PI-NOT-EOF            VALUE 'N'.                   EL174
00180          16 PI-SEND-TOT-SWT          PIC X.                       EL174
00181             88 PI-SEND-TOT           VALUE 'Y'.                   EL174
00182             88 PI-NOT-SEND-TOT       VALUE 'N'.                   EL174
00183          16  PI-CHECK-MODE           PIC X.                          CL**7
00184              88  CHECKS-PRINTED      VALUE 'Y'.                      CL**7
00185              88  CHECKS-TO-BE-PRINTED VALUE SPACE.                   CL**7
00186                                                                      CL**4
00187          16  PI-START-CONTROL-NO     PIC S9(08) COMP.                CL**7
00188          16  FILLER                  PIC X(563).                     CL**7
00189                                                                      CL**4
00190      EJECT                                                        EL174
00191                                      COPY EL174S.                    CL**6
00192                                                                   EL174
00193  01  FILLER                          REDEFINES                    EL174
00194      EL174AI.                                                     EL174
00195      05  FILLER                      PIC X(74).                      CL**6
00196                                                                   EL174
00197      05  FILLER                      OCCURS 18 TIMES              EL174
00198                                      INDEXED BY EL174A-INDEX.     EL174
00199                                                                   EL174
00200          15  EL174A-CONTROL-LENGTH   PIC S9(4)                    EL174
00201                                      COMP.                        EL174
00202          15  EL174A-CONTROL-ATTRB    PIC X.                       EL174
00203          15  EL174A-CONTROL          PIC 9(7).                    EL174
00204                                                                   EL174
00205          15  EL174A-CHECK-NO-LENGTH  PIC S9(4)                    EL174
00206                                      COMP.                        EL174
00207          15  EL174A-CHECK-NO-ATTRB   PIC X.                       EL174
00208          15  EL174A-CHECK-NO         PIC X(7).                    EL174
00209                                                                   EL174
00210          15  EL174A-PMT-TYPE-LENGTH  PIC S9(4)                    EL174
00211                                      COMP.                        EL174
00212          15  EL174A-PMT-TYPE-ATTRB   PIC X.                       EL174
00213          15  EL174A-PMT-TYPE         PIC X(11).                   EL174
00214                                                                   EL174
00215          15  EL174A-CLAIM-NO-LENGTH  PIC S9(4)                    EL174
00216                                      COMP.                        EL174
00217          15  EL174A-CLAIM-NO-ATTRB   PIC X.                       EL174
00218          15  EL174A-CLAIM-NO         PIC X(7).                    EL174
00219                                                                   EL174
00220          15  EL174A-CARRIER-LENGTH   PIC S9(4)                    EL174
00221                                      COMP.                        EL174
00222          15  EL174A-CARRIER-ATTRB    PIC X.                       EL174
00223          15  EL174A-CARRIER          PIC X.                       EL174
00224                                                                   EL174
00225          15  EL174A-CERT-NO-LENGTH   PIC S9(4)                    EL174
00226                                      COMP.                        EL174
00227          15  EL174A-CERT-NO-ATTRB    PIC X.                       EL174
00228          15  EL174A-CERT-NO          PIC X(11).                   EL174
00229                                                                   EL174
00230          15  EL174A-AMT-LENGTH       PIC S9(4)                    EL174
00231                                      COMP.                        EL174
00232          15  EL174A-AMT-ATTRB        PIC X.                       EL174
00233          15  EL174A-AMT              PIC Z,ZZZ,ZZ9.99-.           EL174
00234                                                                   EL174
00235          15  EL174A-PAYEE-LENGTH     PIC S9(4)                    EL174
00236                                      COMP.                        EL174
00237          15  EL174A-PAYEE-ATTRB      PIC X.                       EL174
00238          15  EL174A-PAYEE            PIC X(7).                    EL174
00239                                                                   EL174
00240          15  EL174A-BY-LENGTH        PIC S9(4)                    EL174
00241                                      COMP.                        EL174
00242          15  EL174A-BY-ATTRB         PIC X.                       EL174
00243          15  EL174A-BY               PIC X(4).                    EL174
00244                                                                   EL174
00245      EJECT                                                        EL174
00246                                  COPY ELCEMIB.                       CL**5
00247      EJECT                                                        EL174
00248                                  COPY ELCDATE.                       CL**5
00249      EJECT                                                        EL174
00250                                  COPY ELCLOGOF.                      CL**5
00251      EJECT                                                        EL174
00252                                  COPY ELCATTR.                       CL**5
00253      EJECT                                                        EL174
00254                                  COPY ELCAID.                        CL**5
00255                                                                   EL174
00256  01  FILLER                      REDEFINES                        EL174
00257      DFHAID.                                                      EL174
00258                                                                   EL174
00259      05  FILLER                      PIC X(8).                    EL174
00260                                                                   EL174
00261      05  PF-VALUES                   PIC X                        EL174
00262          OCCURS 24 TIMES.                                         EL174
00263      EJECT                                                        EL174
00264  LINKAGE SECTION.                                                 EL174
00265                                                                   EL174
00266  01  DFHCOMMAREA                     PIC X(1024).                 EL174
00267                                                                   EL174
00268                                  COPY ELCCHKQ.                       CL**5
00269      EJECT                                                        EL174
00270                                  COPY ELCTRLR.                       CL**5
00271      EJECT                                                        EL174
00272                                  COPY ELCCNTL.                       CL**5
00273      EJECT                                                        EL174
00274  PROCEDURE DIVISION.                                              EL174
00275                                                                   EL174
00276      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL174
00277      MOVE '5'                   TO DC-OPTION-CODE.                EL174
00278      PERFORM 8500-DATE-CONVERSION.                                EL174
00279      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL174
00280      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL174
00281                                                                   EL174
00282      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL174
00283                                                                   EL174
00284 *    NOTE ******************************************************* EL174
00285 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL174
00286 *         *  FROM ANOTHER MODULE.                               * EL174
00287 *         *******************************************************.EL174
00288                                                                   EL174
00289      IF EIBCALEN NOT GREATER THAN ZERO                            EL174
00290          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL174
00291          GO TO 8300-SEND-TEXT.                                    EL174
00292                                                                   EL174
00293      EXEC CICS HANDLE CONDITION                                   EL174
00294          PGMIDERR (9600-PGMIDERR)                                 EL174
00295          ENDFILE  (4800-END-OF-FILE)                              EL174
00296          NOTFND   (4800-END-OF-FILE)                                 CL**3
00297          ERROR    (9990-ERROR) END-EXEC.                          EL174
00298                                                                   EL174
00299      EJECT                                                        EL174
00300  0010-MAIN-LOGIC.                                                 EL174
00301      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL174
00302          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL174
00303              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL174
00304              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL174
00305              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL174
00306              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL174
00307              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL174
00308              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL174
00309              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL174
00310              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM     EL174
00311            ELSE                                                   EL174
00312              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL174
00313              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL174
00314              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL174
00315              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL174
00316              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL174
00317              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL174
00318              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL174
00319              MOVE SPACES               TO  PI-SAVED-PROGRAM-6     EL174
00320        ELSE                                                       EL174
00321          GO TO 0020-MAIN-LOGIC.                                   EL174
00322                                                                   EL174
00323  0015-MAIN-LOGIC.                                                 EL174
00324 *    NOTE ******************************************************* EL174
00325 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL174
00326 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL174
00327 *         *******************************************************.EL174
00328                                                                   EL174
00329      EXEC CICS HANDLE CONDITION                                   EL174
00330          QIDERR (0015-NEXT-SENTENCE) END-EXEC.                    EL174
00331                                                                   EL174
00332      MOVE EIBTRMID               TO  WS-TS-TERM-ID.               EL174
00333                                                                   EL174
00334      EXEC CICS DELETEQ TS                                         EL174
00335          QUEUE (WS-TEMP-STORAGE-KEY) END-EXEC.                    EL174
00336                                                                   EL174
00337  0015-NEXT-SENTENCE.                                              EL174
00338      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.        EL174
00339                                                                   EL174
00340      MOVE LOW-VALUES             TO  PI-CHECK-AIX-KEY                CL**6
00341                                      PI-PREV-CHECK-AIX-KEY.          CL**6
00342                                                                   EL174
00343      MOVE ZERO                   TO  PI-END-OF-FILE               EL174
00344                                      PI-TEMP-STORAGE-ITEM         EL174
00345                                      PI-CONTROL-TOT               EL174
00346                                      PI-CONTROL-SAVE-CONTROL      EL174
00347                                      PI-CONTROL-GRAND-TOT.        EL174
00348                                                                   EL174
00349      MOVE WS-SAVE-CHECK-MODE     TO  PI-CHECK-MODE.               EL174
00350      MOVE PI-COMPANY-CD          TO PI-CK-COMPANY-CODE.           EL174
00351      MOVE 'Y'                    TO PI-FIRST-TIME-SWT.            EL174
00352      MOVE 'N'                    TO PI-SEND-TOT-SWT.              EL174
00353      MOVE 'N'                    TO PI-EOF-SWT.                   EL174
00354                                                                   EL174
00355      IF WS-START-CNTLNO GREATER ZERO                              EL174
00356          MOVE WS-START-CNTLNO    TO  PI-CK-CONTROL-NO.            EL174
00357                                                                   EL174
00358      IF EIBAID = DFHCLEAR                                         EL174
00359          GO TO 9400-CLEAR.                                        EL174
00360                                                                   EL174
00361      IF PI-PROCESSOR-ID = 'LGXX'                                  EL174
00362          NEXT SENTENCE                                            EL174
00363      ELSE                                                         EL174
00364          EXEC CICS READQ TS                                       EL174
00365              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL174
00366              INTO    (SECURITY-CONTROL)                           EL174
00367              LENGTH  (SC-COMM-LENGTH)                             EL174
00368              ITEM    (SC-ITEM)                                    EL174
00369          END-EXEC                                                 EL174
00370          MOVE SC-CLAIMS-DISPLAY (11)   TO  PI-DISPLAY-CAP         EL174
00371          MOVE SC-CLAIMS-UPDATE  (11)   TO  PI-MODIFY-CAP          EL174
00372          IF NOT DISPLAY-CAP                                       EL174
00373              MOVE 'READ'               TO  SM-READ                EL174
00374              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT          CL**7
00375              MOVE ER-0070              TO  EMI-ERROR              EL174
00376              GO TO 8100-SEND-INITIAL-MAP.                         EL174
00377                                                                   EL174
00378      PERFORM 4000-BROWSE-CHECK-QUEUE-FILE.                        EL174
00379                                                                   EL174
00380      EJECT                                                        EL174
00381  0020-MAIN-LOGIC.                                                 EL174
00382 *    NOTE ******************************************************* EL174
00383 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL174
00384 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL174
00385 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL174
00386 *         *******************************************************.EL174
00387                                                                   EL174
00388      IF EIBAID = DFHCLEAR                                         EL174
00389          GO TO 9400-CLEAR.                                        EL174
00390                                                                   EL174
00391      IF NOT MODIFY-CAP                                            EL174
00392          IF EIBAID = DFHPF1 OR DFHPF2                             EL174
00393              NEXT SENTENCE                                        EL174
00394          ELSE                                                     EL174
00395              MOVE 'UPDATE'          TO  SM-READ                   EL174
00396              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT          CL**7
00397              MOVE ER-0070           TO  EMI-ERROR                 EL174
00398              GO TO 8100-SEND-INITIAL-MAP.                         EL174
00399                                                                   EL174
00400      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL174
00401          MOVE ER-0008               TO  EMI-ERROR                 EL174
00402          MOVE -1                 TO  APFKL                        EL174
00403          PERFORM 8200-SEND-DATAONLY.                              EL174
00404                                                                   EL174
00405      EXEC CICS RECEIVE                                            EL174
00406          INTO   (EL174AI)                                         EL174
00407          MAPSET (WS-MAPSET-NAME)                                  EL174
00408          MAP    (WS-MAP-NAME) END-EXEC.                           EL174
00409                                                                   EL174
00410      IF APFKL IS GREATER THAN ZERO                                EL174
00411          IF EIBAID NOT = DFHENTER                                 EL174
00412              MOVE ER-0004           TO  EMI-ERROR                 EL174
00413              MOVE AL-UNBOF       TO  APFKA                        EL174
00414              MOVE -1             TO  APFKL                        EL174
00415              PERFORM 8200-SEND-DATAONLY                           EL174
00416            ELSE                                                   EL174
00417              IF APFKO IS NUMERIC                                  EL174
00418                AND APFKO IS GREATER THAN ZERO                     EL174
00419                AND APFKO IS LESS THAN '25'                        EL174
00420                  MOVE PF-VALUES (APFKI)  TO  EIBAID               EL174
00421                ELSE                                               EL174
00422                  MOVE ER-0029           TO  EMI-ERROR             EL174
00423                  MOVE AL-UNBOF       TO  APFKA                    EL174
00424                  MOVE -1             TO  APFKL                    EL174
00425                  PERFORM 8200-SEND-DATAONLY.                      EL174
00426                                                                   EL174
00427      IF EIBAID = DFHPF12                                          EL174
00428          MOVE 'EL010'         TO  THIS-PGM                        EL174
00429          GO TO 9300-XCTL.                                         EL174
00430                                                                   EL174
00431      IF EIBAID = DFHPF23                                          EL174
00432          GO TO 9000-RETURN-CICS.                                  EL174
00433                                                                   EL174
00434      IF EIBAID = DFHPF24                                          EL174
00435          MOVE 'EL126   '         TO  THIS-PGM                     EL174
00436          GO TO 9300-XCTL.                                         EL174
00437                                                                   EL174
00438      IF EIBAID = DFHENTER OR DFHPF1 OR DFHPF2                     EL174
00439                           OR DFHPF3 OR DFHPF4                     EL174
00440          NEXT SENTENCE                                            EL174
00441        ELSE                                                       EL174
00442          MOVE ER-0008            TO  EMI-ERROR                    EL174
00443          MOVE -1                 TO  APFKL                        EL174
00444          PERFORM 8200-SEND-DATAONLY.                              EL174
00445                                                                   EL174
00446      EJECT                                                        EL174
00447  0100-MAIN-LOGIC.                                                 EL174
00448      IF EIBAID = DFHPF3                                           EL174
00449          IF CNTLNOL IS GREATER THAN 0                                CL**4
00450              MOVE CNTLNOI        TO  PI-START-CONTROL-NO             CL**4
00451              GO TO 0130-MAIN-LOGIC                                EL174
00452          ELSE                                                     EL174
00453              GO TO 0130-MAIN-LOGIC.                                  CL**4
00454                                                                   EL174
00455      IF EIBAID = DFHPF4                                           EL174
00456          IF CHECKS-TO-BE-PRINTED                                  EL174
00457              MOVE 'Y' TO WS-SAVE-CHECK-MODE                       EL174
00458              IF CNTLNOL GREATER ZERO                              EL174
00459                  IF CNTLNOI NUMERIC                               EL174
00460                      MOVE CNTLNOI TO WS-START-CNTLNO              EL174
00461                      GO TO 0015-MAIN-LOGIC                        EL174
00462                  ELSE                                             EL174
00463                      GO TO 0015-MAIN-LOGIC                        EL174
00464              ELSE                                                 EL174
00465                  GO TO 0015-MAIN-LOGIC                            EL174
00466          ELSE                                                     EL174
00467              IF CHECKS-PRINTED                                    EL174
00468                  MOVE SPACE       TO WS-SAVE-CHECK-MODE              CL**4
00469                  IF CNTLNOL IS GREATER THAN ZERO                     CL**4
00470                      IF CNTLNOI IS NUMERIC                           CL**4
00471                          MOVE CNTLNOI    TO  WS-START-CNTLNO         CL**4
00472                          GO TO 0015-MAIN-LOGIC                       CL**4
00473                       ELSE                                           CL**4
00474                           GO TO 0015-MAIN-LOGIC                      CL**4
00475                  ELSE                                                CL**4
00476                      GO TO 0015-MAIN-LOGIC.                          CL**4
00477                                                                   EL174
00478      IF EIBAID = DFHPF1 OR DFHPF2                                 EL174
00479          GO TO 0110-MAIN-LOGIC.                                   EL174
00480                                                                   EL174
00481      IF CNTLNOL GREATER ZERO                                      EL174
00482          IF CNTLNOI NUMERIC                                       EL174
00483              MOVE CNTLNOI TO WS-START-CNTLNO                      EL174
00484              MOVE PI-CHECK-MODE TO WS-SAVE-CHECK-MODE             EL174
00485              GO TO 0015-MAIN-LOGIC.                               EL174
00486                                                                   EL174
00487      IF PI-END-OF-FILE NOT = ZERO                                 EL174
00488          PERFORM 9400-CLEAR.                                      EL174
00489                                                                   EL174
00490      PERFORM 4000-BROWSE-CHECK-QUEUE-FILE.                        EL174
00491                                                                   EL174
00492  0110-MAIN-LOGIC.                                                 EL174
00493      IF APAGEI NUMERIC                                            EL174
00494         MOVE APAGEI              TO  WS-TEMP-STORAGE-ITEM         EL174
00495        ELSE                                                       EL174
00496         MOVE 1                   TO  WS-TEMP-STORAGE-ITEM.        EL174
00497                                                                   EL174
00498      IF EIBAID = DFHPF1 AND                                       EL174
00499         PI-SEND-TOT     AND                                       EL174
00500         PI-EOF          AND                                       EL174
00501         WS-TEMP-STORAGE-ITEM = PI-TEMP-STORAGE-ITEM               EL174
00502          MOVE LOW-VALUE           TO EL174AI                      EL174
00503          MOVE WS-TEMP-STORAGE-ITEM TO APAGEO                      EL174
00504          MOVE -1                   TO APFKL                       EL174
00505          MOVE ER-0375              TO EMI-ERROR                   EL174
00506          SET EL174A-INDEX          TO 1                           EL174
00507          MOVE 'CONTL TOTAL'     TO EL174A-PMT-TYPE (EL174A-INDEX) EL174
00508          MOVE PI-CONTROL-TOT       TO EL174A-AMT (EL174A-INDEX)   EL174
00509          SET EL174A-INDEX UP BY +1                                EL174
00510          MOVE PI-CONTROL-GRAND-TOT TO EL174A-AMT (EL174A-INDEX)   EL174
00511          MOVE 'GRAND TOTAL'     TO EL174A-PMT-TYPE (EL174A-INDEX) EL174
00512          PERFORM 8100-SEND-INITIAL-MAP.                           EL174
00513                                                                   EL174
00514      IF EIBAID = DFHPF1                                           EL174
00515         IF WS-TEMP-STORAGE-ITEM LESS THAN PI-TEMP-STORAGE-ITEM    EL174
00516          ADD +1  TO  WS-TEMP-STORAGE-ITEM                         EL174
00517          GO TO 0120-MAIN-LOGIC                                    EL174
00518         ELSE                                                      EL174
00519          IF PI-NOT-EOF                                            EL174
00520             GO TO 4000-BROWSE-CHECK-QUEUE-FILE                    EL174
00521          ELSE                                                     EL174
00522             NEXT SENTENCE                                         EL174
00523      ELSE                                                         EL174
00524         NEXT SENTENCE.                                            EL174
00525                                                                   EL174
00526      IF EIBAID = DFHPF2                                           EL174
00527        AND WS-TEMP-STORAGE-ITEM GREATER THAN +1                   EL174
00528          SUBTRACT +1 FROM WS-TEMP-STORAGE-ITEM                    EL174
00529          GO TO 0120-MAIN-LOGIC.                                   EL174
00530                                                                   EL174
00531      MOVE ER-0312                TO  EMI-ERROR.                   EL174
00532      MOVE -1                     TO  APFKL.                       EL174
00533      PERFORM 8200-SEND-DATAONLY.                                  EL174
00534      EJECT                                                        EL174
00535  0120-MAIN-LOGIC.                                                 EL174
00536      MOVE EIBTRMID               TO  WS-TS-TERM-ID.               EL174
00537                                                                   EL174
00538      EXEC CICS READQ TS                                           EL174
00539          QUEUE  (WS-TEMP-STORAGE-KEY)                             EL174
00540          ITEM   (WS-TEMP-STORAGE-ITEM)                            EL174
00541          INTO   (EL174AI)                                         EL174
00542          LENGTH (WS-TS-LENGTH) END-EXEC.                          EL174
00543                                                                   EL174
00544      MOVE WS-TEMP-STORAGE-ITEM  TO  APAGEO.                       EL174
00545                                                                   EL174
00546      PERFORM 8100-SEND-INITIAL-MAP.                               EL174
00547      EJECT                                                        EL174
00548  0130-MAIN-LOGIC.                                                 EL174
00549      EXEC CICS HANDLE CONDITION                                   EL174
00550           TERMIDERR    (0130-TERMID-ERROR)                        EL174
00551           TRANSIDERR   (0130-TRANS-ERROR)                         EL174
00552           END-EXEC.                                               EL174
00553                                                                   EL174
00554      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.               CL**8
00555                                                                      CL**8
00556      IF PRINTERL NOT = ZEROS                                      EL174
00557         MOVE PRINTERI            TO WS-PRINTER-ID                 EL174
00558                                     PI-ALT-DMD-PRT-ID                CL**8
00559         GO TO 0130-START.                                         EL174
00560                                                                      CL**3
00561      IF PI-PROCESSOR-PRINTER IS NOT EQUAL TO SPACES                  CL**3
00562          MOVE PI-PROCESSOR-PRINTER   TO  WS-PRINTER-ID               CL**3
00563          GO TO 0130-START.                                           CL**3
00564                                                                   EL174
00565      MOVE PI-COMPANY-ID          TO CNTL-CO.                      EL174
00566      MOVE '1'                    TO CNTL-RECORD-TYPE.             EL174
00567      MOVE SPACES                 TO CNTL-GENL.                    EL174
00568      MOVE ZEROS                  TO CNTL-SEQ.                     EL174
00569      EXEC CICS READ                                               EL174
00570           DATASET   (WS-CONTROL-DSID)                             EL174
00571           SET       (ADDRESS OF CONTROL-FILE)                        CL**7
00572           RIDFLD    (CNTL-KEY)                                    EL174
00573           END-EXEC.                                               EL174
00574                                                                   EL174
00575      MOVE CF-FORMS-PRINTER-ID    TO WS-PRINTER-ID.                EL174
00576                                                                   EL174
00577  0130-START.                                                      EL174
00578                                                                      CL**8
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**8
00580 *       MOVE EIBTRMID   TO WS-PRINTER-ID                             CL**8
00581          EXEC CICS START                                             CL**8
00582              INTERVAL(0)                                             CL**8
00583              TRANSID    (WS-PRINT-TRAN-ID)                           CL**8
00584              FROM       (PROGRAM-INTERFACE-BLOCK)                    CL**8
00585              LENGTH     (PI-COMM-LENGTH)                             CL**8
00586 *            TERMID     (WS-PRINTER-ID)                              CL**8
00587          END-EXEC                                                    CL**8
00588        ELSE                                                          CL**8
00589          EXEC CICS START                                             CL**8
00590              INTERVAL(0)                                             CL**8
00591              TRANSID    (WS-PRINT-TRAN-ID)                           CL**8
00592              FROM       (PROGRAM-INTERFACE-BLOCK)                    CL**8
00593              LENGTH     (PI-COMM-LENGTH)                             CL**8
00594              TERMID     (WS-PRINTER-ID)                              CL**8
00595          END-EXEC.                                                   CL**8
00596                                                                   EL174
00597      MOVE ER-0567                TO EMI-ERROR.                    EL174
00598      MOVE -1                     TO APFKL.                        EL174
00599      GO TO 8200-SEND-DATAONLY.                                    EL174
00600                                                                   EL174
00601  0130-TERMID-ERROR.                                               EL174
00602      MOVE ER-0412                TO EMI-ERROR.                    EL174
00603      MOVE -1                     TO APFKL.                        EL174
00604      GO TO 8200-SEND-DATAONLY.                                    EL174
00605                                                                   EL174
00606  0130-TRANS-ERROR.                                                EL174
00607      MOVE ER-0413                TO EMI-ERROR.                    EL174
00608      MOVE -1                     TO APFKL.                        EL174
00609      GO TO 8200-SEND-DATAONLY.                                    EL174
00610                                                                   EL174
00611      EJECT                                                        EL174
00612  4000-BROWSE-CHECK-QUEUE-FILE SECTION.                            EL174
00613      MOVE LOW-VALUES             TO  EL174AI.                     EL174
00614                                                                   EL174
00615      EXEC CICS STARTBR                                            EL174
00616          DATASET (WS-CHECK-QUEUE-AIX-DSID)                           CL**6
00617          RIDFLD  (PI-CHECK-AIX-KEY)                                  CL**6
00618          GTEQ    END-EXEC.                                        EL174
00619                                                                   EL174
00620      SET EL174A-INDEX TO +1.                                      EL174
00621                                                                   EL174
00622  4100-READNEXT.                                                   EL174
00623      MOVE PI-CHECK-AIX-KEY           TO  PI-PREV-CHECK-AIX-KEY.      CL**6
00624                                                                   EL174
00625      EXEC CICS READNEXT                                           EL174
00626          DATASET (WS-CHECK-QUEUE-AIX-DSID)                           CL**6
00627          RIDFLD  (PI-CHECK-AIX-KEY)                                  CL**6
00628          SET     (ADDRESS OF CHECK-QUE) END-EXEC.                    CL**7
00629                                                                   EL174
00630      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD                         EL174
00631          GO TO 4800-END-OF-FILE.                                  EL174
00632                                                                   EL174
00633      IF CQ-ENTRY-TYPE NOT = 'Q'                                   EL174
00634          GO TO 4100-READNEXT.                                     EL174
00635                                                                   EL174
00636      IF NOT PI-NO-CARRIER-SECURITY                                EL174
00637          IF CQ-CARRIER NOT = PI-CARRIER-SECURITY                  EL174
00638             GO TO 4100-READNEXT.                                  EL174
00639                                                                   EL174
00640      IF CQ-TIMES-PRINTED NOT = ZERO                               EL174
00641          IF CHECKS-PRINTED                                        EL174
00642              NEXT SENTENCE                                        EL174
00643          ELSE                                                     EL174
00644              GO TO 4100-READNEXT                                  EL174
00645      ELSE                                                         EL174
00646          IF CHECKS-TO-BE-PRINTED                                  EL174
00647              NEXT SENTENCE                                        EL174
00648          ELSE                                                     EL174
00649              GO TO 4100-READNEXT.                                 EL174
00650                                                                      CL**4
00651      IF PI-FIRST-TIME                                                CL**4
00652         IF CNTLNOL IS EQUAL TO 0                                     CL**4
00653             MOVE PI-CK-CONTROL-NO    TO  PI-START-CONTROL-NO.        CL**4
00654                                                                   EL174
00655      IF EL174A-INDEX LESS THAN +18                                EL174
00656         IF PI-CONTROL-SAVE-CONTROL NOT = CQ-CONTROL-NUMBER        EL174
00657            IF PI-FIRST-TIME                                       EL174
00658               MOVE CQ-CONTROL-NUMBER TO PI-CONTROL-SAVE-CONTROL   EL174
00659            ELSE                                                   EL174
00660               MOVE 'CONTL TOTAL' TO EL174A-PMT-TYPE (EL174A-INDEX)EL174
00661               MOVE PI-CONTROL-TOT TO EL174A-AMT (EL174A-INDEX)    EL174
00662               SET EL174A-INDEX UP BY +1                           EL174
00663               MOVE ZEROS TO PI-CONTROL-TOT                        EL174
00664               MOVE CQ-CONTROL-NUMBER TO PI-CONTROL-SAVE-CONTROL.  EL174
00665                                                                   EL174
00666      MOVE 'N'                    TO PI-FIRST-TIME-SWT.            EL174
00667                                                                   EL174
00668      IF EL174A-INDEX GREATER THAN +17                             EL174
00669         MOVE +1 TO WS-READNEXT-SW.                                EL174
00670                                                                   EL174
00671      IF WS-READNEXT-SW GREATER THAN ZERO                          EL174
00672          GO TO 4900-ENDBROWSE.                                    EL174
00673                                                                   EL174
00674      ADD CQ-CHECK-AMOUNT TO PI-CONTROL-TOT.                       EL174
00675      MOVE CQ-CONTROL-NUMBER     TO  EL174A-CONTROL (EL174A-INDEX) EL174
00676      MOVE CQ-CHECK-NUMBER       TO  EL174A-CHECK-NO (EL174A-INDEX)EL174
00677                                                                   EL174
00678      IF CQ-PAYMENT-TYPE = '1'                                     EL174
00679          MOVE 'PARTIAL'          TO EL174A-PMT-TYPE (EL174A-INDEX)EL174
00680        ELSE                                                       EL174
00681      IF CQ-PAYMENT-TYPE = '2'                                     EL174
00682          MOVE 'FINAL'            TO EL174A-PMT-TYPE (EL174A-INDEX)EL174
00683        ELSE                                                       EL174
00684      IF CQ-PAYMENT-TYPE = '3'                                     EL174
00685          MOVE 'LUMP SUM'         TO EL174A-PMT-TYPE (EL174A-INDEX)EL174
00686        ELSE                                                       EL174
00687      IF CQ-PAYMENT-TYPE = '4'                                     EL174
00688          MOVE 'ADDITIONAL'       TO EL174A-PMT-TYPE (EL174A-INDEX)EL174
00689        ELSE                                                       EL174
00690      IF CQ-PAYMENT-TYPE = '5'                                     EL174
00691          MOVE 'CHG EXP'          TO EL174A-PMT-TYPE (EL174A-INDEX)EL174
00692        ELSE                                                       EL174
00693      IF CQ-PAYMENT-TYPE = '6'                                     EL174
00694          MOVE 'NON CHG EXP'      TO EL174A-PMT-TYPE (EL174A-INDEX)EL174
00695        ELSE                                                       EL174
00696      IF CQ-PAYMENT-TYPE = '7'                                     EL174
00697          MOVE 'LIFE REFUND'      TO EL174A-PMT-TYPE (EL174A-INDEX)EL174
00698        ELSE                                                       EL174
00699      IF CQ-PAYMENT-TYPE = '8'                                     EL174
00700          MOVE 'A&H REFUND'      TO EL174A-PMT-TYPE (EL174A-INDEX).EL174
00701                                                                   EL174
00702      ADD CQ-CHECK-AMOUNT TO PI-CONTROL-GRAND-TOT.                 EL174
00703      MOVE CQ-CLAIM-NO           TO EL174A-CLAIM-NO (EL174A-INDEX).EL174
00704      MOVE CQ-CARRIER            TO EL174A-CARRIER  (EL174A-INDEX).EL174
00705      MOVE CQ-CERT-NO            TO EL174A-CERT-NO  (EL174A-INDEX).EL174
00706      MOVE CQ-CHECK-AMOUNT       TO EL174A-AMT      (EL174A-INDEX).EL174
00707                                                                   EL174
00708      MOVE SPACES                 TO  WS-ACTIVITY-TRAILERS-KEY.    EL174
00709                                                                   EL174
00710      MOVE CQ-COMPANY-CD          TO  WS-ATK-COMPANY-CD.           EL174
00711      MOVE CQ-CARRIER             TO  WS-ATK-CARRIER.              EL174
00712      MOVE CQ-CLAIM-NO            TO  WS-ATK-CLAIM-NO.             EL174
00713      MOVE CQ-CERT-NO             TO  WS-ATK-CERT-NO.              EL174
00714      MOVE CQ-PMT-TRLR-SEQUENCE   TO  WS-ATK-SEQUENCE-NO.          EL174
00715                                                                   EL174
00716      EXEC CICS READ                                               EL174
00717          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      EL174
00718          RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                       EL174
00719          SET     (ADDRESS OF ACTIVITY-TRAILERS) END-EXEC.            CL**7
00720                                                                   EL174
00721      IF AT-PAYEE-TYPE EQUAL 'I'                                      CL**2
00722          MOVE 'INSURED'          TO  EL174A-PAYEE (EL174A-INDEX)  EL174
00723        ELSE                                                       EL174
00724      IF AT-PAYEE-TYPE EQUAL 'B'                                      CL**2
00725          MOVE 'BENEFICIARY'      TO  EL174A-PAYEE (EL174A-INDEX)  EL174
00726        ELSE                                                       EL174
00727      IF AT-PAYEE-TYPE EQUAL 'A'                                      CL**2
00728          MOVE 'ACCOUNT'          TO  EL174A-PAYEE (EL174A-INDEX)  EL174
00729        ELSE                                                       EL174
00730      IF AT-PAYEE-TYPE EQUAL 'O'                                      CL**2
00731          MOVE 'OTHER 1'          TO  EL174A-PAYEE (EL174A-INDEX)  EL174
00732        ELSE                                                       EL174
00733      IF AT-PAYEE-TYPE EQUAL 'Q'                                      CL**2
00734          MOVE 'OTHER 2'          TO  EL174A-PAYEE (EL174A-INDEX)  EL174
00735        ELSE                                                       EL174
00736      IF AT-PAYEE-TYPE EQUAL 'P'                                      CL**2
00737          MOVE 'DOCTOR'           TO  EL174A-PAYEE (EL174A-INDEX)     CL**5
00738        ELSE                                                          CL**5
00739      IF AT-PAYEE-TYPE EQUAL 'E'                                      CL**5
00740          MOVE 'EMPLOYER'         TO  EL174A-PAYEE (EL174A-INDEX).    CL**5
00741                                                                   EL174
00742      MOVE AT-RECORDED-BY         TO EL174A-BY (EL174A-INDEX).     EL174
00743                                                                   EL174
00744      IF EL174A-INDEX LESS THAN +18                                EL174
00745          SET EL174A-INDEX UP BY +1                                EL174
00746          GO TO 4100-READNEXT.                                     EL174
00747                                                                   EL174
00748      MOVE +1                     TO  WS-READNEXT-SW.              EL174
00749      GO TO 4100-READNEXT.                                         EL174
00750                                                                   EL174
00751  4800-END-OF-FILE.                                                EL174
00752      MOVE +1                     TO  PI-END-OF-FILE.              EL174
00753                                                                   EL174
00754      MOVE 'Y'                    TO PI-EOF-SWT.                   EL174
00755      IF EL174A-INDEX GREATER +16                                  EL174
00756         MOVE 'Y' TO PI-SEND-TOT-SWT                               EL174
00757         GO TO 4900-ENDBROWSE.                                     EL174
00758                                                                   EL174
00759      MOVE ER-0375                   TO  EMI-ERROR.                EL174
00760      IF EL174A-INDEX LESS THAN +18                                EL174
00761               MOVE 'CONTL TOTAL' TO EL174A-PMT-TYPE (EL174A-INDEX)EL174
00762               MOVE PI-CONTROL-TOT TO EL174A-AMT (EL174A-INDEX)    EL174
00763               SET EL174A-INDEX UP BY +1                           EL174
00764      ELSE                                                         EL174
00765         NEXT SENTENCE.                                            EL174
00766      IF EL174A-INDEX LESS THAN +18                                EL174
00767         SET EL174A-INDEX UP BY +1                                 EL174
00768         MOVE PI-CONTROL-GRAND-TOT TO EL174A-AMT (EL174A-INDEX)    EL174
00769         MOVE 'GRAND TOTAL' TO EL174A-PMT-TYPE (EL174A-INDEX)      EL174
00770      ELSE                                                         EL174
00771         NEXT SENTENCE.                                            EL174
00772                                                                   EL174
00773  4900-ENDBROWSE.                                                  EL174
00774      MOVE -1                     TO  APFKL.                       EL174
00775                                                                   EL174
00776      MOVE EIBTRMID               TO  WS-TS-TERM-ID.               EL174
00777                                                                   EL174
00778      EXEC CICS WRITEQ TS                                          EL174
00779          QUEUE  (WS-TEMP-STORAGE-KEY)                             EL174
00780          ITEM   (PI-TEMP-STORAGE-ITEM)                            EL174
00781          FROM   (EL174AI)                                         EL174
00782          LENGTH (WS-TS-LENGTH) END-EXEC.                          EL174
00783                                                                   EL174
00784      MOVE PI-TEMP-STORAGE-ITEM  TO  APAGEO.                       EL174
00785                                                                   EL174
00786      PERFORM 8100-SEND-INITIAL-MAP.                               EL174
00787                                                                   EL174
00788  4990-EXIT.                                                       EL174
00789      EXIT.                                                        EL174
00790                                                                   EL174
00791      EJECT                                                        EL174
00792  8100-SEND-INITIAL-MAP SECTION.                                   EL174
00793      IF CHECKS-TO-BE-PRINTED                                      EL174
00794          MOVE WS-TO-BE-PRINTED-DESC    TO TITLEO                  EL174
00795          MOVE 'EL174A'                 TO RPTIDO                     CL**6
00796          MOVE WS-TO-BE-PRINTED-PFDESC  TO PFDESCO                 EL174
00797      ELSE                                                         EL174
00798          MOVE WS-CHECKS-PRINTED-PFDESC TO PFDESCO                 EL174
00799          MOVE 'EL174B'                 TO RPTIDO                     CL**6
00800          MOVE WS-CHECKS-PRINTED-DESC   TO TITLEO.                 EL174
00801                                                                   EL174
00802      IF EMI-ERROR NOT = ZERO                                      EL174
00803          PERFORM 9900-ERROR-FORMAT.                               EL174
00804                                                                   EL174
00805      MOVE EIBTIME                TO  TIME-IN.                     EL174
00806                                                                   EL174
00807      MOVE SAVE-DATE              TO  ADATEO.                      EL174
00808      MOVE TIME-OUT               TO  ATIMEO.                      EL174
00809      MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O.                      EL174
00810                                                                   EL174
00811      EXEC CICS SEND                                               EL174
00812          FROM   (EL174AI)                                         EL174
00813          MAPSET (WS-MAPSET-NAME)                                  EL174
00814          MAP    (WS-MAP-NAME)                                     EL174
00815          CURSOR ERASE END-EXEC.                                   EL174
00816                                                                   EL174
00817      PERFORM 9100-RETURN-TRAN.                                    EL174
00818                                                                   EL174
00819  8100-EXIT.                                                       EL174
00820      EXIT.                                                        EL174
00821                                                                   EL174
00822      EJECT                                                        EL174
00823  8200-SEND-DATAONLY SECTION.                                      EL174
00824      IF CHECKS-TO-BE-PRINTED                                      EL174
00825          MOVE WS-TO-BE-PRINTED-DESC    TO TITLEO                  EL174
00826          MOVE 'EL174A'                 TO RPTIDO                     CL**6
00827          MOVE WS-TO-BE-PRINTED-PFDESC  TO PFDESCO                 EL174
00828      ELSE                                                         EL174
00829          MOVE WS-CHECKS-PRINTED-PFDESC TO PFDESCO                 EL174
00830          MOVE 'EL174B'                 TO RPTIDO                     CL**6
00831          MOVE WS-CHECKS-PRINTED-DESC   TO TITLEO.                 EL174
00832                                                                   EL174
00833      IF EMI-ERROR NOT = ZERO                                      EL174
00834          PERFORM 9900-ERROR-FORMAT.                               EL174
00835                                                                   EL174
00836      MOVE EIBTIME                TO  TIME-IN.                     EL174
00837      MOVE SAVE-DATE              TO  ADATEO.                      EL174
00838      MOVE TIME-OUT               TO  ATIMEO.                      EL174
00839      MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O.                      EL174
00840                                                                   EL174
00841      EXEC CICS SEND DATAONLY                                      EL174
00842          FROM   (EL174AI)                                         EL174
00843          MAPSET (WS-MAPSET-NAME)                                  EL174
00844          MAP    (WS-MAP-NAME)                                     EL174
00845          CURSOR END-EXEC.                                         EL174
00846                                                                   EL174
00847      PERFORM 9100-RETURN-TRAN.                                    EL174
00848                                                                   EL174
00849  8100-EXIT.                                                       EL174
00850      EXIT.                                                        EL174
00851                                                                   EL174
00852      EJECT                                                        EL174
00853  8300-SEND-TEXT SECTION.                                          EL174
00854      EXEC CICS SEND TEXT                                          EL174
00855          FROM   (LOGOFF-TEXT)                                     EL174
00856          LENGTH (LOGOFF-LENGTH)                                   EL174
00857          ERASE  FREEKB END-EXEC.                                  EL174
00858                                                                   EL174
00859      EXEC CICS RETURN                                             EL174
00860          END-EXEC.                                                EL174
00861                                                                   EL174
00862  8300-EXIT.                                                       EL174
00863      EXIT.                                                        EL174
00864                                                                   EL174
00865      EJECT                                                        EL174
00866  8500-DATE-CONVERSION SECTION.                                    EL174
00867      EXEC CICS LINK                                               EL174
00868          PROGRAM  ('ELDATCV')                                     EL174
00869          COMMAREA (DATE-CONVERSION-DATA)                          EL174
00870          LENGTH   (DC-COMM-LENGTH) END-EXEC.                      EL174
00871                                                                   EL174
00872  8500-EXIT.                                                       EL174
00873      EXIT.                                                        EL174
00874                                                                   EL174
00875      EJECT                                                        EL174
00876  9000-RETURN-CICS SECTION.                                        EL174
00877      MOVE 'EL005'                TO  THIS-PGM.                    EL174
00878      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL174
00879      PERFORM 9300-XCTL.                                           EL174
00880                                                                   EL174
00881  9000-EXIT.                                                       EL174
00882      EXIT.                                                        EL174
00883                                                                   EL174
00884  9100-RETURN-TRAN SECTION.                                        EL174
00885      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL174
00886      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL174
00887                                                                   EL174
00888      EXEC CICS RETURN                                             EL174
00889          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL174
00890          LENGTH   (PI-COMM-LENGTH)                                EL174
00891          TRANSID  (WS-TRANS-ID) END-EXEC.                         EL174
00892                                                                   EL174
00893  9100-EXIT.                                                       EL174
00894      EXIT.                                                        EL174
00895                                                                   EL174
00896  9300-XCTL SECTION.                                               EL174
00897      EXEC CICS HANDLE CONDITION                                   EL174
00898          QIDERR (9300-NEXT-SENTENCE) END-EXEC.                    EL174
00899                                                                   EL174
00900      MOVE EIBTRMID               TO  WS-TS-TERM-ID.               EL174
00901                                                                   EL174
00902      EXEC CICS DELETEQ TS                                         EL174
00903          QUEUE (WS-TEMP-STORAGE-KEY) END-EXEC.                    EL174
00904                                                                   EL174
00905  9300-NEXT-SENTENCE.                                              EL174
00906      MOVE DFHENTER               TO  EIBAID.                      EL174
00907                                                                   EL174
00908      EXEC CICS XCTL                                               EL174
00909          PROGRAM  (THIS-PGM)                                      EL174
00910          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL174
00911          LENGTH   (PI-COMM-LENGTH) END-EXEC.                      EL174
00912                                                                   EL174
00913  9300-EXIT.                                                       EL174
00914      EXIT.                                                        EL174
00915                                                                   EL174
00916      EJECT                                                        EL174
00917  9400-CLEAR SECTION.                                              EL174
00918      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                     EL174
00919      PERFORM 9300-XCTL.                                           EL174
00920                                                                   EL174
00921  9400-EXIT.                                                       EL174
00922      EXIT.                                                        EL174
00923                                                                   EL174
00924  9600-PGMIDERR SECTION.                                           EL174
00925      EXEC CICS HANDLE CONDITION                                   EL174
00926          PGMIDERR (8300-SEND-TEXT) END-EXEC.                      EL174
00927                                                                   EL174
00928      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL174
00929                                      LOGOFF-PGM.                  EL174
00930                                                                   EL174
00931      MOVE 'EL005'                TO  THIS-PGM.                    EL174
00932      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL174
00933                                                                   EL174
00934      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL174
00935                                                                   EL174
00936      PERFORM 9300-XCTL.                                           EL174
00937                                                                   EL174
00938  9600-EXIT.                                                       EL174
00939      EXIT.                                                        EL174
00940                                                                   EL174
00941      EJECT                                                        EL174
00942  9900-ERROR-FORMAT SECTION.                                       EL174
00943      EXEC CICS LINK                                               EL174
00944          PROGRAM  ('EL001')                                       EL174
00945          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL174
00946          LENGTH   (EMI-COMM-LENGTH) END-EXEC.                     EL174
00947                                                                   EL174
00948  9900-EXIT.                                                       EL174
00949      EXIT.                                                        EL174
00950                                                                   EL174
00951  9990-ERROR SECTION.                                              EL174
00952      MOVE DFHEIBLK TO EMI-LINE1.                                  EL174
00953      EXEC CICS LINK                                               EL174
00954          PROGRAM  ('EL004')                                       EL174
00955          COMMAREA (EMI-LINE1)                                     EL174
00956          LENGTH   (72) END-EXEC.                                  EL174
00957                                                                   EL174
00958      PERFORM 8100-SEND-INITIAL-MAP.                               EL174
00959      GO TO 9100-RETURN-TRAN.                                      EL174
00960                                                                   EL174
00961  9990-EXIT.                                                       EL174
00962      EXIT.                                                        EL174
00963                                                                   EL174
00964  9995-SECURITY-VIOLATION.                                         EL174
00965                              COPY ELCSCTP.                        EL174
00966                                                                   EL174
00967  9995-EXIT.                                                       EL174
00968      EXIT.                                                        EL174
00969                                                                   EL174
