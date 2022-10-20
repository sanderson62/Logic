00001  IDENTIFICATION DIVISION.                                         10/18/96
00002                                                                   EL686
00003  PROGRAM-ID.                 EL686 .                                 LV018
00004 *              PROGRAM CONVERTED BY                                  CL*16
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*16
00006 *              CONVERSION DATE 02/12/96 09:57:46.                    CL*16
00007 *                            VMOD=2.018                              CL*18
00008 *                                                                 EL686
00009 *AUTHOR.    LOGIC, INC.                                              CL*16
00010 *           DALLAS, TEXAS.                                           CL*16
00011                                                                   EL686
00012 *DATE-COMPILED.                                                      CL*16
00013                                                                   EL686
00014 *SECURITY.   *****************************************************   CL*16
00015 *            *                                                   *   CL*16
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*16
00017 *            *                                                   *   CL*16
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*16
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*16
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*16
00021 *            *                                                   *   CL*16
00022 *            *****************************************************   CL*16
00023                                                                   EL686
00024 *REMARKS.     TRANSACTION - EXG2                                     CL**2
00025 *                                                                    CL**2
00026 *        THIS PROGRAM IS USED TO INDICATE THE CHECKS BEING           CL**2
00027 *    RELEASED TO PRINT.  EACH RELEASE CONSTITUTES A CONTROL GROUP    CL**2
00028 *    THAT IS REFERENCED BY THE CHECK WRITER (EL687) AND THE CHECK    CL**2
00029 *    PRINT STARTER (EL686).                                          CL**2
00030 *                                                                    CL**2
00031 *    SCREENS     - EL686A - CHECK RELEASE                            CL**2
00032 *                                                                    CL**2
00033 *    ENTERED BY  - EL671  - REPORT MENU                              CL**2
00034 *                                                                    CL**2
00035 *    EXIT TO     - EL671  - RESULT OF CLEAR                          CL**2
00036 *                                                                    CL**2
00037 *    INPUT FILES - ERPYAJ - PENDING PAYMENT AND ADJUSTMENTS          CL**2
00038 *                  ERCHEK - CHECK MAINTENANCE FILE                   CL**2
00039 *                  ERCMCK - COMMISSION CHECK MAINTENANCE FILE        CL**2
00040 *                  ELCNTL - CONTROL FILE                             CL**2
00041 *                                                                    CL**2
00042 *    OUTPUT FILES - ERPYAJ - PENDING PAYMENTS AND ADJUSTMENTS        CL**2
00043 *                   ERCHEK - CHECK MAINTENANCE FILE                  CL**2
00044 *                   ELCNTL - CONTROL FILE                            CL**2
00045 *                   ERCHKQ - CHECK QUEUE                             CL**2
00046 *                   ERCMKQ - COMMISSION CHECK QUEUE                  CL**2
00047 *                                                                    CL**2
00048 *    COMMAREA    - PASSED.                                           CL**2
00049 *                                                                    CL**2
030612******************************************************************
030612*                   C H A N G E   L O G
030612*
030612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030612*-----------------------------------------------------------------
030612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030612* EFFECTIVE    NUMBER
030612*-----------------------------------------------------------------
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
030612******************************************************************
00050                                                                   EL686
00051      EJECT                                                        EL686
00052  ENVIRONMENT DIVISION.                                            EL686
00053                                                                   EL686
00054  DATA DIVISION.                                                   EL686
00055                                                                   EL686
00056  WORKING-STORAGE SECTION.                                         EL686
00057                                                                   EL686
00058  77  FILLER  PIC X(32)  VALUE '********************************'. EL686
00059  77  FILLER  PIC X(32)  VALUE '*    EL686 WORKING STORAGE     *'. EL686
00060  77  FILLER  PIC X(32)  VALUE '************ V/M 2.018 *********'.    CL*18
00061                                                                   EL686
00062      COPY ELCSCTM.                                                   CL*16
00063      COPY ELCSCRTY.                                                  CL*16
00064  01  FILLER                          COMP-3.                      EL686
00065      05  WS-NDX                      PIC S9(3)       VALUE ZERO.     CL**2
00066      05  WS-SEQ-NO                   PIC S9(9)       VALUE ZERO.     CL**2
00067                                                                   EL686
00068      05  WS-TIME-WORK                PIC S9(7)       VALUE ZERO.  EL686
00069      05  WS-TIME                     REDEFINES                    EL686
00070          WS-TIME-WORK                PIC S9(3)V9(4).              EL686
00071      05  WS-HHMM                     REDEFINES                    EL686
00072          WS-TIME-WORK                PIC S9(5)V99.                EL686
00073                                                                   EL686
00074      05  WS-ERPYAJ-BROWSE-SW         PIC S9          VALUE +0.    EL686
00075          88  WS-ERPYAJ-BROWSE-NOT-STARTED            VALUE +0.    EL686
00076          88  WS-ERPYAJ-BROWSE-STARTED                VALUE +1.    EL686
00077      05  WS-ERCHEK-BROWSE-SW         PIC S9          VALUE +0.    EL686
00078          88  WS-ERCHEK-BROWSE-NOT-STARTED            VALUE +0.    EL686
00079          88  WS-ERCHEK-BROWSE-STARTED                VALUE +1.    EL686
00080      05  WS-ERCMCK-BROWSE-SW         PIC S9          VALUE +0.       CL**2
00081          88  WS-ERCMCK-BROWSE-NOT-STARTED            VALUE +0.       CL**2
00082          88  WS-ERCMCK-BROWSE-STARTED                VALUE +1.       CL**2
00083                                                                   EL686
00084      05  WS-RELEASED-COUNT           PIC S9(5)       VALUE ZERO.  EL686
00085      05  WS-RELEASED-AMOUNT          PIC S9(9)V99    VALUE ZERO.     CL*14
00086                                                                   EL686
00087  01  FILLER                          COMP SYNC.                      CL*14
00088                                                                   EL686
00089      05  SC-ITEM                     PIC S9(4)       VALUE  +1.      CL*10
00090      05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  EL686
00091      05  WS-ERCHKQ-LENGTH            PIC S9(4)       VALUE +100.  EL686
00092      05  WS-ERCMKQ-LENGTH            PIC S9(4)       VALUE +1800.    CL**8
00093      05  WS-TS-LENGTH                PIC S9(4)       VALUE +1920. EL686
00094                                                                   EL686
00095      05  WS-CHECK-QUE-COUNTER        PIC S9(8)       VALUE ZERO.  EL686
00096      05  WS-CHECK-COUNTER            PIC S9(4)       VALUE +10.   EL686
00097                                                                   EL686
00098      05  WS-JOURNAL-FILE-ID          PIC S9(4)       VALUE +1.    EL686
00099      05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)       VALUE +773.     CL**9
00100                                                                   EL686
00101  01  FILLER.                                                      EL686
00102                                                                   EL686
00103      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL686S'.EL686
00104      05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL686A'.EL686
00105                                                                   EL686
00106      05  FILLER                      REDEFINES                    EL686
00107          WS-MAP-NAME.                                             EL686
00108          20  FILLER                  PIC XX.                      EL686
00109          20  WS-MAP-NUMBER           PIC X(6).                    EL686
00110                                                                   EL686
00111      05  DEEDIT-FIELD                PIC X(15).                      CL*14
00112      05  DEEDIT-FIELD-V0  REDEFINES                                  CL*14
00113          DEEDIT-FIELD                PIC S9(15).                     CL*14
00114                                                                      CL*14
00115      05  QID.                                                        CL*10
00116          10  QID-TERM                PIC X(4)   VALUE SPACES.        CL*10
00117          10  FILLER                  PIC X(4)   VALUE '125D'.        CL*10
00118                                                                      CL*14
00119      05  TIME-IN                     PIC S9(7).                      CL*10
00120      05  TIME-OUT-R   REDEFINES TIME-IN.                             CL*10
00121          10  FILLER                  PIC X.                          CL*10
00122          10  TIME-OUT                PIC 99V99.                      CL*10
00123          10  FILLER                  PIC X(2).                       CL*10
00124                                                                      CL*14
00125      05  WS-CHECK-NUMBER.                                         EL686
00126          10  FILLER                  PIC X           VALUE ZERO.  EL686
00127          10  WS-CHECK-NO             PIC X(6)        VALUE ZERO.  EL686
00128                                                                   EL686
00129      05  WS-CONTROL-FILE-KEY.                                     EL686
00130          10  WS-CFK-COMPANY-ID       PIC X(3)        VALUE SPACES.EL686
00131          10  WS-CFK-RECORD-TYPE      PIC X           VALUE SPACES.EL686
00132          10  FILLER                  PIC XX          VALUE SPACES.EL686
00133          10  WS-CFK-BENEFIT-NO       PIC XX          VALUE SPACES.EL686
00134          10  WS-CFK-SEQUENCE-NO      PIC S9(4)       VALUE ZERO   EL686
00135                                      COMP.                        EL686
00136                                                                   EL686
00137      05  WS-PENDING-PAYMENTS-KEY.                                 EL686
00138          10  WS-PPK-COMPANY-CD       PIC X.                       EL686
00139          10  WS-PPK-CARRIER          PIC X.                       EL686
00140          10  WS-PPK-GROUPING         PIC X(6).                    EL686
00141          10  WS-PPK-FIN-RESP         PIC X(10).                   EL686
00142          10  WS-PPK-ACCOUNT          PIC X(10).                   EL686
00143          10  WS-PPK-FILE-SEQ-NO      PIC S9(8)                    EL686
00144                                      COMP.                        EL686
00145          10  WS-PPK-RECORD-TYPE      PIC X.                       EL686
00146                                                                   EL686
00147      05  WS-COMCK-MAINT-KEY.                                         CL**2
00148          10  WS-CMK-COMPANY-CD       PIC X.                          CL**2
00149          10  WS-CMK-CARRIER          PIC X.                          CL**2
00150          10  WS-CMK-GROUPING         PIC X(6).                       CL**2
00151          10  WS-CMK-PAYEE            PIC X(10).                      CL**2
00152          10  WS-CMK-PAYEE-SEQ        PIC S9(4)      COMP.            CL**5
00153          10  WS-CMK-SEQUENCE-NO      PIC S9(4)      COMP.            CL**2
00154                                                                      CL**2
00155      05  WS-CHECK-MAINT-KEY.                                      EL686
00156          10  WS-CHK-COMPANY-CD       PIC X.                       EL686
00157          10  WS-CHK-CARRIER          PIC X.                       EL686
00158          10  WS-CHK-GROUPING         PIC X(6).                    EL686
00159          10  WS-CHK-STATE            PIC XX.                      EL686
00160          10  WS-CHK-ACCOUNT          PIC X(10).                   EL686
00161          10  WS-CHK-CERT-EFF-DT      PIC XX.                      EL686
00162          10  WS-CHK-CERT-NO.                                      EL686
00163              15  WS-CHK-CERT-PRIME   PIC X(10).                   EL686
00164              15  WS-CHK-CERT-SFX     PIC X.                       EL686
00165          10  WS-CHK-SEQ-NO           PIC S9(4)                    EL686
00166                                      COMP.                        EL686
00167                                                                   EL686
00168      05  THIS-PGM                    PIC X(8) VALUE 'EL686'.         CL*14
00169                                                                   EL686
00170      05  EL001                       PIC X(8) VALUE 'EL001'.      EL686
00171      05  EL004                       PIC X(8) VALUE 'EL004'.      EL686
00172      05  EL005                       PIC X(8) VALUE 'EL005'.      EL686
00173      05  EL010                       PIC X(8) VALUE 'EL010'.      EL686
00174      05  EL126                       PIC X(8) VALUE 'EL126'.      EL686
00175      05  ELDATCV                     PIC X(8) VALUE 'ELDATCV'.    EL686
00176                                                                   EL686
00177      05  WS-ERCHKQ-DSID              PIC X(8) VALUE 'ERCHKQ'.        CL*14
00178      05  WS-ERCHEK-DSID              PIC X(8) VALUE 'ERCHEK'.        CL*14
00179      05  WS-ERCMKQ-DSID              PIC X(8) VALUE 'ERCMKQ'.        CL*14
00180      05  WS-ERCMCK-DSID              PIC X(8) VALUE 'ERCMCK'.        CL*14
00181      05  WS-ERPYAJ-DSID              PIC X(8) VALUE 'ERPYAJ'.        CL*14
00182      05  WS-ELCNTL-DSID              PIC X(8) VALUE 'ELCNTL'.        CL*14
00183                                                                   EL686
00184      05  WS-JOURNAL-TYPE-ID          PIC XX          VALUE 'EL'.  EL686
00185                                                                   EL686
00186      05  WS-SPACES                   PIC X           VALUE SPACES.EL686
00187                                                                   EL686
00188      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EXG2'.EL686
00189      05  WS-PRINT-TRANS-ID           PIC X(4)        VALUE 'EXG9'.EL686
00190                                                                   EL686
00191      05  WS-ERROR-FLAG-AREA.                                      EL686
00192          10  ER-0002                 PIC 9(4)        VALUE 0002.  EL686
00193          10  ER-0004                 PIC 9(4)        VALUE 0004.  EL686
00194          10  ER-0008                 PIC 9(4)        VALUE 0008.  EL686
00195          10  ER-0029                 PIC 9(4)        VALUE 0029.  EL686
00196          10  ER-0070                 PIC 9(4)        VALUE 0070.     CL*10
00197          10  ER-0078                 PIC 9(4)        VALUE 0078.     CL*14
00198          10  ER-0330                 PIC 9(4)        VALUE 0330.  EL686
00199          10  ER-0331                 PIC 9(4)        VALUE 0331.  EL686
00200          10  ER-0348                 PIC 9(4)        VALUE 0348.     CL*14
00201          10  ER-0568                 PIC 9(4)        VALUE 0568.  EL686
00202          10  ER-0766                 PIC 9(4)        VALUE 0766.     CL*14
00203          10  ER-3048                 PIC 9(4)        VALUE 3048.     CL*14
00204                                                                   EL686
00205      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70    EL686
00206                                      COMP SYNC.                      CL*14
00207                                                                   EL686
00208      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.EL686
00209                                                                   EL686
00210      05  WS-TOTAL-LINE1.                                          EL686
00211          10  FILLER                  PIC X(14)       VALUE        EL686
00212              'CONTROL GROUP'.                                     EL686
00213          10  WS-TL1-CONTROL-GROUP    PIC 9(7)-.                   EL686
00214          10  WS-TL1-RELEASE          PIC X(20)       VALUE           CL*14
00215              ' RELEASED'.                                         EL686
00216                                                                   EL686
00217      05  WS-TOTAL-LINE2.                                          EL686
00218          10  WS-TL1-COUNT            PIC ZZ,ZZ9.                  EL686
00219          10  FILLER                  PIC X(6)        VALUE           CL*14
00220              ' CHECK'.                                               CL*16
00221          10  WS-TL1-PLURAL           PIC X           VALUE           CL*14
00222              'S'.                                                    CL*14
00223          10  FILLER                  PIC X(18)       VALUE           CL*14
00224              ' IN THE AMOUNT OF'.                                    CL*14
00225          10  WS-TL1-AMOUNT           PIC Z,ZZZ,ZZ9.99.            EL686
00226                                                                   EL686
00227      05  WS-TEMP-STORAGE-KEY.                                     EL686
00228          10  WS-TS-TERM-ID           PIC X(4).                    EL686
00229          10  FILLER                  PIC X(4)        VALUE '685'. EL686
00230                                                                   EL686
00231      05  WS-TEMP-STORAGE-ITEM        PIC S9(4)       VALUE ZERO   EL686
00232                                      COMP SYNC.                      CL*14
00233                                                                   EL686
00234      EJECT                                                        EL686
00235      COPY ELCINTF.                                                   CL*14
00236                                                                      CL*10
00237      12  FILLER                      REDEFINES                    EL686
00238          PI-PROGRAM-WORK-AREA.                                    EL686
00239              16  FILLER              PIC X.                       EL686
00240              16  PI-CK-CONTROL-NO    PIC S9(8)      COMP.         EL686
00241              16  PI-CURRENT-DATE     PIC X(8).                    EL686
00242              16  PI-CURRENT-DATE-BIN PIC XX.                      EL686
00243              16  PI-PROC-SW          PIC S9.                         CL*14
00244                  88  PI-SCREEN-PROCESSED        VALUE +1.            CL*14
00245              16  PI-CERT-SW          PIC S9.                         CL*16
00246                  88  PI-CERT-NOT-MATCHED        VALUE +0.            CL*14
00247                  88  PI-CERT-MATCHED            VALUE +1.            CL*14
00248              16  PI-CHECK-AMOUNT     PIC S9(7)  COMP-3.              CL*14
00249              16  BIN-EFFDT           PIC XX   OCCURS 15.             CL*14
00250              16  FILLER              PIC X(589).                     CL*16
00251                                                                   EL686
00252      COPY EL686S.                                                    CL*14
00253                                                                   EL686
00254      EJECT                                                        EL686
00255      COPY ELCJPFX.                                                   CL*14
00256                 PIC X(750).                                          CL*14
00257                                                                      CL*14
00258      EJECT                                                           CL*14
00259      COPY ELCEMIB.                                                   CL*14
00260                                                                   EL686
00261      EJECT                                                        EL686
00262      COPY ELCDATE.                                                   CL*14
00263                                                                   EL686
00264      EJECT                                                           CL*14
00265      COPY ELCLOGOF.                                                  CL*14
00266                                                                   EL686
00267      COPY ELCATTR.                                                   CL*14
00268                                                                      CL*14
00269      COPY ELCAID.                                                    CL*14
00270                                                                   EL686
00271  01  FILLER                      REDEFINES                        EL686
00272      DFHAID.                                                      EL686
00273                                                                   EL686
00274      05  FILLER                      PIC X(8).                    EL686
00275                                                                   EL686
00276      05  PF-VALUES                   PIC X                        EL686
00277          OCCURS 24 TIMES.                                         EL686
00278                                                                   EL686
00279  LINKAGE SECTION.                                                 EL686
00280                                                                   EL686
00281  01  DFHCOMMAREA                     PIC X(1024).                 EL686
00282                                                                   EL686
00283 *01 PARMLIST                         COMP SYNC.                      CL*16
00284 *    05  FILLER                      PIC S9(9).                      CL*16
00285 *    05  ERCHKQ-POINTER              PIC S9(9).                      CL*16
00286 *    05  ELCNTL-POINTER              PIC S9(9).                      CL*16
00287 *    05  ERPYAJ-POINTER              PIC S9(9).                      CL*16
00288 *    05  ERCHEK-POINTER              PIC S9(9).                      CL*16
00289 *    05  ERCMCK-POINTER              PIC S9(9).                      CL*16
00290 *    05  ERCMKQ-POINTER              PIC S9(9).                      CL*16
00291                                                                      CL**2
00292      EJECT                                                           CL**2
00293      COPY ERCCHKQ.                                                   CL*14
00294                                                                      CL**2
00295      EJECT                                                           CL**2
00296      COPY ELCCNTL.                                                   CL*14
00297                                                                      CL*14
00298      EJECT                                                           CL*14
00299      COPY ERCPYAJ.                                                   CL*14
00300                                                                      CL*14
00301      EJECT                                                           CL*14
00302      COPY ERCCHEK.                                                   CL*14
00303                                                                      CL*14
00304      EJECT                                                           CL*14
00305      COPY ERCCMCK.                                                   CL*14
00306                                                                      CL*14
00307      EJECT                                                           CL*14
00308      COPY ERCCMKQ.                                                   CL*14
00309                                                                   EL686
00310      EJECT                                                        EL686
00311  PROCEDURE DIVISION.                                              EL686
00312                                                                   EL686
00313      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL686
00314                                                                   EL686
00315 *    NOTE ******************************************************* EL686
00316 *         *                                                     * EL686
00317 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL686
00318 *         *  FROM ANOTHER MODULE.                               * EL686
00319 *         *                                                     * EL686
00320 *         *******************************************************.EL686
00321                                                                   EL686
00322      IF EIBCALEN NOT GREATER THAN ZERO                            EL686
00323          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL686
00324          GO TO 8300-SEND-TEXT.                                    EL686
00325                                                                   EL686
00326      EXEC CICS HANDLE CONDITION                                   EL686
00327          PGMIDERR   (9600-PGMIDERR)                               EL686
00328          NOTFND     (0140-NOT-FOUND)                                 CL*14
00329          ENDFILE    (0400-END-OF-SEARCH)                             CL*14
00330          ENQBUSY    (0910-ENQ-BUSY)                               EL686
00331          TERMIDERR  (7010-TERMID-ERROR)                           EL686
00332          TRANSIDERR (7020-TRANS-ERROR)                            EL686
00333          ERROR      (9990-ERROR)                                     CL*14
00334      END-EXEC.                                                       CL*14
00335                                                                   EL686
00336      MOVE EIBTRMID               TO  QID-TERM.                       CL*10
00337      MOVE +2                     TO  EMI-NUMBER-OF-LINES          EL686
00338                                      EMI-SWITCH2.                 EL686
00339                                                                   EL686
00340      MOVE EIBTIME                TO  WS-TIME-WORK                    CL*14
00341                                      TIME-IN.                        CL*14
00342                                                                   EL686
00343      IF PI-CALLING-PROGRAM NOT = THIS-PGM                            CL*14
00344          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                      CL*14
00345              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL686
00346              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL686
00347              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL686
00348              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL686
00349              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL686
00350              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL686
00351              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL686
00352              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM        CL*14
00353            ELSE                                                   EL686
00354              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL686
00355              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL686
00356              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL686
00357              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL686
00358              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL686
00359              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL686
00360              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL686
00361              MOVE SPACES               TO  PI-SAVED-PROGRAM-6     EL686
00362        ELSE                                                       EL686
00363          GO TO 0110-PROCESS-INPUT.                                   CL*14
00364                                                                   EL686
00365  EJECT                                                               CL*14
00366  0100-INITIALIZE.                                                    CL*14
00367                                                                   EL686
00368 *    NOTE ******************************************************* EL686
00369 *         *                                                     * EL686
00370 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL686
00371 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL686
00372 *         *                                                     * EL686
00373 *         *******************************************************.EL686
00374                                                                   EL686
00375      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.           CL**6
00376      MOVE ZERO                   TO  PI-PROC-SW.                     CL*14
00377                                                                   EL686
00378      MOVE LOW-VALUES             TO  EL686AI.                     EL686
00379      MOVE -1                     TO  AOPTIONL.                    EL686
00380                                                                   EL686
00381      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.                CL**6
00382      MOVE '5'                    TO  DC-OPTION-CODE.                 CL*14
00383      PERFORM 8500-DATE-CONVERSION.                                   CL**6
00384      MOVE DC-BIN-DATE-1          TO  PI-CURRENT-DATE-BIN.            CL**6
00385      MOVE DC-GREG-DATE-1-EDIT    TO  PI-CURRENT-DATE.             EL686
00386                                                                   EL686
00387      PERFORM 8100-SEND-INITIAL-MAP.                               EL686
00388                                                                   EL686
00389      EJECT                                                        EL686
00390  0110-PROCESS-INPUT.                                                 CL*14
00391                                                                   EL686
00392 *    NOTE ******************************************************* EL686
00393 *         *                                                     * EL686
00394 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL686
00395 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL686
00396 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL686
00397 *         *                                                     * EL686
00398 *         *******************************************************.EL686
00399                                                                   EL686
00400      IF EIBAID EQUAL TO DFHCLEAR                                  EL686
00401          GO TO 9400-CLEAR.                                        EL686
00402                                                                   EL686
00403      IF EIBAID EQUAL TO (DFHPA1 OR                                EL686
00404                          DFHPA2 OR                                EL686
00405                          DFHPA3)                                  EL686
00406          MOVE ER-0008            TO  EMI-ERROR                    EL686
00407          MOVE -1                 TO  APFKL                        EL686
00408          PERFORM 8200-SEND-DATAONLY.                              EL686
00409                                                                   EL686
00410      EXEC CICS RECEIVE                                            EL686
00411          INTO   (EL686AI)                                         EL686
00412          MAPSET (WS-MAPSET-NAME)                                  EL686
00413          MAP    (WS-MAP-NAME) END-EXEC.                           EL686
00414                                                                   EL686
00415      IF APFKL IS GREATER THAN ZERO                                EL686
00416          IF EIBAID NOT = DFHENTER                                 EL686
00417              MOVE ER-0004        TO  EMI-ERROR                    EL686
00418              MOVE AL-UNBOF       TO  APFKA                        EL686
00419              MOVE -1             TO  APFKL                        EL686
00420              PERFORM 8200-SEND-DATAONLY                           EL686
00421            ELSE                                                   EL686
00422              IF APFKO IS NUMERIC                                  EL686
00423                AND APFKO IS GREATER THAN ZERO                     EL686
00424                AND APFKO IS LESS THAN '25'                        EL686
00425                  MOVE PF-VALUES (APFKI)  TO  EIBAID               EL686
00426                ELSE                                               EL686
00427                  MOVE ER-0029        TO  EMI-ERROR                EL686
00428                  MOVE AL-UNBOF       TO  APFKA                    EL686
00429                  MOVE -1             TO  APFKL                    EL686
00430                  PERFORM 8200-SEND-DATAONLY.                      EL686
00431                                                                   EL686
00432      IF EIBAID IS EQUAL TO DFHPF12                                EL686
00433          MOVE EL010              TO  THIS-PGM                        CL*14
00434          GO TO 9300-XCTL.                                         EL686
00435                                                                   EL686
00436      IF EIBAID IS EQUAL TO DFHPF23                                EL686
00437          GO TO 9000-RETURN-CICS.                                  EL686
00438                                                                   EL686
00439      IF EIBAID IS EQUAL TO DFHPF24                                EL686
00440          MOVE EL126              TO  THIS-PGM                        CL*14
00441          GO TO 9300-XCTL.                                         EL686
00442                                                                      CL*14
00443      IF EIBAID = DFHPF1 AND                                          CL*14
00444         PI-SCREEN-PROCESSED                                          CL*14
00445           GO TO 0200-PROCESS-CHECK-RELEASE.                          CL*14
00446                                                                   EL686
00447      IF EIBAID NOT = DFHENTER                                     EL686
00448          MOVE ER-0008            TO  EMI-ERROR                    EL686
00449          MOVE -1                 TO  APFKL                        EL686
00450          PERFORM 8200-SEND-DATAONLY.                              EL686
00451                                                                   EL686
00452      EJECT                                                        EL686
00453                                                                   EL686
00454 *    NOTE ******************************************************* EL686
00455 *         *                                                     * EL686
00456 *         *      OPTION          MEANING                        * EL686
00457 *         *                                                     * EL686
00458 *         *        1         OPTIONS 2 THRU 4                   *    CL**2
00459 *         *        2         BILLING PAYMENTS                   * EL686
00460 *         *        3         REFUND REIMBURSMENTS               * EL686
00461 *         *        4         MAINTENANCE PAYMENTS               * EL686
00462 *         *                  ACCOUNT RECEIVABLE PAYMENTS        *    CL*18
00463 *         *                  5 = CHECKS   6 = ACH               *    CL*18
00464 *         *                                                     * EL686
00465 *         *******************************************************.EL686
00466                                                                   EL686
00467      IF AOPTIONL GREATER ZERO  AND                                   CL*14
00468         AOPTIONI = '1' OR '2' OR '3' OR '4' OR '5' OR '6'            CL*18
00469          MOVE AL-UNNON           TO  AOPTIONA                     EL686
00470        ELSE                                                       EL686
00471          MOVE -1                 TO  AOPTIONL                     EL686
00472          MOVE AL-UNBON           TO  AOPTIONA                     EL686
00473          MOVE ER-0330            TO  EMI-ERROR                    EL686
00474          PERFORM 9900-ERROR-FORMAT.                                  CL*14
00475                                                                   EL686
00476      IF PI-COMPANY-ID = 'POS'                                        CL*14
00477         AND ACARRL NOT GREATER ZERO                               EL686
00478              MOVE -1             TO  ACARRL                       EL686
00479              MOVE AL-UABOF       TO  ACARRA                       EL686
00480              MOVE ER-0568        TO  EMI-ERROR                    EL686
00481              PERFORM 9900-ERROR-FORMAT.                              CL*14
00482                                                                      CL*14
00483      IF AAMTL GREATER ZERO                                           CL*14
00484         EXEC CICS BIF DEEDIT                                         CL*14
00485              FIELD (AAMTI)                                           CL*14
00486              LENGTH (7)                                              CL*14
00487         END-EXEC                                                     CL*14
00488         IF AAMTI NOT NUMERIC                                         CL*14
00489             MOVE -1             TO  AAMTL                            CL*14
00490             MOVE AL-UABON       TO  AAMTA                            CL*14
00491             MOVE ER-0078        TO  EMI-ERROR                        CL*14
00492             PERFORM 9900-ERROR-FORMAT                                CL*14
00493          ELSE                                                        CL*14
00494             MOVE AAMTI          TO  PI-CHECK-AMOUNT                  CL*14
00495                                                                      CL*18
00496             MOVE AL-UANON       TO  AAMTA.                           CL*14
00497                                                                      CL*14
00498 *    NOTE *******************************************************    CL*14
00499 *         *                                                     *    CL*14
00500 *         *  CHECK TO SEE THAT THE CERTIFICATE NUMBER HAS       *    CL*14
00501 *         *  THE EFFECTIVE DATE ENTERED.                        *    CL*14
00502 *         *                                                     *    CL*14
00503 *         *******************************************************.   CL*14
00504                                                                      CL*14
00505      IF CERTO01I = SPACES                                            CL*14
00506           MOVE +0               TO CERTO01L.                         CL*14
00507      IF EFFDT01I = SPACES                                            CL*14
00508           MOVE +0               TO EFFDT01L.                         CL*14
00509      IF CERTO02I = SPACES                                            CL*14
00510           MOVE +0               TO CERTO02L.                         CL*14
00511      IF EFFDT02I = SPACES                                            CL*14
00512           MOVE +0               TO EFFDT02L.                         CL*14
00513      IF CERTO03I = SPACES                                            CL*14
00514           MOVE +0               TO CERTO03L.                         CL*14
00515      IF EFFDT03I = SPACES                                            CL*14
00516           MOVE +0               TO EFFDT03L.                         CL*14
00517      IF CERTO04I = SPACES                                            CL*14
00518           MOVE +0               TO CERTO04L.                         CL*14
00519      IF EFFDT04I = SPACES                                            CL*14
00520           MOVE +0               TO EFFDT04L.                         CL*14
00521      IF CERTO05I = SPACES                                            CL*14
00522           MOVE +0               TO CERTO05L.                         CL*14
00523      IF EFFDT05I = SPACES                                            CL*14
00524           MOVE +0               TO EFFDT05L.                         CL*14
00525      IF CERTO06I = SPACES                                            CL*14
00526           MOVE +0               TO CERTO06L.                         CL*14
00527      IF EFFDT06I = SPACES                                            CL*14
00528           MOVE +0               TO EFFDT06L.                         CL*14
00529      IF CERTO07I = SPACES                                            CL*14
00530           MOVE +0               TO CERTO07L.                         CL*14
00531      IF EFFDT07I = SPACES                                            CL*14
00532           MOVE +0               TO EFFDT07L.                         CL*14
00533      IF CERTO08I = SPACES                                            CL*14
00534           MOVE +0               TO CERTO08L.                         CL*14
00535      IF EFFDT08I = SPACES                                            CL*14
00536           MOVE +0               TO EFFDT08L.                         CL*14
00537      IF CERTO09I = SPACES                                            CL*14
00538           MOVE +0               TO CERTO09L.                         CL*14
00539      IF EFFDT09I = SPACES                                            CL*14
00540           MOVE +0               TO EFFDT09L.                         CL*14
00541      IF CERTO10I = SPACES                                            CL*14
00542           MOVE +0               TO CERTO10L.                         CL*14
00543      IF EFFDT10I = SPACES                                            CL*14
00544           MOVE +0               TO EFFDT10L.                         CL*14
00545      IF CERTO11I = SPACES                                            CL*14
00546           MOVE +0               TO CERTO11L.                         CL*14
00547      IF EFFDT11I = SPACES                                            CL*14
00548           MOVE +0               TO EFFDT11L.                         CL*14
00549      IF CERTO12I = SPACES                                            CL*14
00550           MOVE +0               TO CERTO12L.                         CL*14
00551      IF EFFDT12I = SPACES                                            CL*14
00552           MOVE +0               TO EFFDT12L.                         CL*14
00553      IF CERTO13I = SPACES                                            CL*14
00554           MOVE +0               TO CERTO13L.                         CL*14
00555      IF EFFDT13I = SPACES                                            CL*14
00556           MOVE +0               TO EFFDT13L.                         CL*14
00557      IF CERTO14I = SPACES                                            CL*14
00558           MOVE +0               TO CERTO14L.                         CL*14
00559      IF EFFDT14I = SPACES                                            CL*14
00560           MOVE +0               TO EFFDT14L.                         CL*14
00561      IF CERTO15I = SPACES                                            CL*14
00562           MOVE +0               TO CERTO15L.                         CL*14
00563      IF EFFDT15I = SPACES                                            CL*14
00564           MOVE +0               TO EFFDT15L.                         CL*14
00565                                                                      CL*14
00566      IF (CERTO01L     > 0 AND EFFDT01L NOT > 0) OR                   CL*14
00567         (CERTO01L NOT > 0 AND EFFDT01L     > 0) OR                   CL*14
00568         (CERTO02L     > 0 AND EFFDT02L NOT > 0) OR                   CL*14
00569         (CERTO02L NOT > 0 AND EFFDT02L     > 0) OR                   CL*14
00570         (CERTO03L     > 0 AND EFFDT03L NOT > 0) OR                   CL*14
00571         (CERTO03L NOT > 0 AND EFFDT03L     > 0) OR                   CL*14
00572         (CERTO04L     > 0 AND EFFDT04L NOT > 0) OR                   CL*14
00573         (CERTO04L NOT > 0 AND EFFDT04L     > 0) OR                   CL*14
00574         (CERTO05L     > 0 AND EFFDT05L NOT > 0) OR                   CL*14
00575         (CERTO05L NOT > 0 AND EFFDT05L     > 0) OR                   CL*14
00576         (CERTO06L     > 0 AND EFFDT06L NOT > 0) OR                   CL*14
00577         (CERTO06L NOT > 0 AND EFFDT06L     > 0) OR                   CL*14
00578         (CERTO07L     > 0 AND EFFDT07L NOT > 0) OR                   CL*14
00579         (CERTO07L NOT > 0 AND EFFDT07L     > 0) OR                   CL*14
00580         (CERTO08L     > 0 AND EFFDT08L NOT > 0) OR                   CL*14
00581         (CERTO08L NOT > 0 AND EFFDT08L     > 0) OR                   CL*14
00582         (CERTO09L     > 0 AND EFFDT09L NOT > 0) OR                   CL*14
00583         (CERTO09L NOT > 0 AND EFFDT09L     > 0) OR                   CL*14
00584         (CERTO10L     > 0 AND EFFDT10L NOT > 0) OR                   CL*14
00585         (CERTO10L NOT > 0 AND EFFDT10L     > 0) OR                   CL*14
00586         (CERTO11L     > 0 AND EFFDT11L NOT > 0) OR                   CL*14
00587         (CERTO11L NOT > 0 AND EFFDT11L     > 0) OR                   CL*14
00588         (CERTO12L     > 0 AND EFFDT12L NOT > 0) OR                   CL*14
00589         (CERTO12L NOT > 0 AND EFFDT12L     > 0) OR                   CL*14
00590         (CERTO13L     > 0 AND EFFDT13L NOT > 0) OR                   CL*14
00591         (CERTO13L NOT > 0 AND EFFDT13L     > 0) OR                   CL*14
00592         (CERTO14L     > 0 AND EFFDT14L NOT > 0) OR                   CL*14
00593         (CERTO14L NOT > 0 AND EFFDT14L     > 0) OR                   CL*14
00594         (CERTO15L     > 0 AND EFFDT15L NOT > 0) OR                   CL*14
00595         (CERTO15L NOT > 0 AND EFFDT15L     > 0)                      CL*14
00596          MOVE ER-0766            TO EMI-ERROR                        CL*14
00597          MOVE -1                 TO CERTO01L                         CL*14
00598          PERFORM 9900-ERROR-FORMAT.                                  CL*14
00599                                                                      CL*14
00600      IF EFFDT01L GREATER ZERO                                        CL*14
00601          MOVE EFFDT01I                 TO  DEEDIT-FIELD              CL*14
00602          PERFORM 8600-DEEDIT                                         CL*14
00603          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00604          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00605          PERFORM 8500-DATE-CONVERSION                                CL*14
00606          IF DATE-CONVERSION-ERROR                                    CL*14
00607              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00608              MOVE -1                   TO  EFFDT01L                  CL*14
00609              MOVE AL-UABON             TO  EFFDT01A                  CL*14
00610              PERFORM 9900-ERROR-FORMAT                               CL*14
00611            ELSE                                                      CL*14
00612              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT01I                  CL*14
00613              MOVE AL-UANON             TO  EFFDT01A                  CL*14
00614              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (1).            CL*14
00615                                                                      CL*14
00616      IF EFFDT02L GREATER ZERO                                        CL*14
00617          MOVE EFFDT02I                 TO  DEEDIT-FIELD              CL*14
00618          PERFORM 8600-DEEDIT                                         CL*14
00619          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00620          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00621          PERFORM 8500-DATE-CONVERSION                                CL*14
00622          IF DATE-CONVERSION-ERROR                                    CL*14
00623              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00624              MOVE -1                   TO  EFFDT02L                  CL*14
00625              MOVE AL-UABON             TO  EFFDT02A                  CL*14
00626              PERFORM 9900-ERROR-FORMAT                               CL*14
00627            ELSE                                                      CL*14
00628              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT02I                  CL*14
00629              MOVE AL-UANON             TO  EFFDT02A                  CL*14
00630              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (2).            CL*14
00631                                                                      CL*14
00632      IF EFFDT03L GREATER ZERO                                        CL*14
00633          MOVE EFFDT03I                 TO  DEEDIT-FIELD              CL*14
00634          PERFORM 8600-DEEDIT                                         CL*14
00635          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00636          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00637          PERFORM 8500-DATE-CONVERSION                                CL*14
00638          IF DATE-CONVERSION-ERROR                                    CL*14
00639              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00640              MOVE -1                   TO  EFFDT03L                  CL*14
00641              MOVE AL-UABON             TO  EFFDT03A                  CL*14
00642              PERFORM 9900-ERROR-FORMAT                               CL*14
00643            ELSE                                                      CL*14
00644              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT03I                  CL*14
00645              MOVE AL-UANON             TO  EFFDT03A                  CL*14
00646              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (3).            CL*14
00647                                                                   EL686
00648      IF EFFDT04L GREATER ZERO                                        CL*14
00649          MOVE EFFDT04I                 TO  DEEDIT-FIELD              CL*14
00650          PERFORM 8600-DEEDIT                                         CL*14
00651          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00652          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00653          PERFORM 8500-DATE-CONVERSION                                CL*14
00654          IF DATE-CONVERSION-ERROR                                    CL*14
00655              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00656              MOVE -1                   TO  EFFDT04L                  CL*14
00657              MOVE AL-UABON             TO  EFFDT04A                  CL*14
00658              PERFORM 9900-ERROR-FORMAT                               CL*14
00659            ELSE                                                      CL*14
00660              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT04I                  CL*14
00661              MOVE AL-UANON             TO  EFFDT04A                  CL*14
00662              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (4).            CL*14
00663                                                                   EL686
00664      IF EFFDT05L GREATER ZERO                                        CL*14
00665          MOVE EFFDT05I                 TO  DEEDIT-FIELD              CL*14
00666          PERFORM 8600-DEEDIT                                         CL*14
00667          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00668          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00669          PERFORM 8500-DATE-CONVERSION                                CL*14
00670          IF DATE-CONVERSION-ERROR                                    CL*14
00671              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00672              MOVE -1                   TO  EFFDT05L                  CL*14
00673              MOVE AL-UABON             TO  EFFDT05A                  CL*14
00674              PERFORM 9900-ERROR-FORMAT                               CL*14
00675            ELSE                                                      CL*14
00676              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT05I                  CL*14
00677              MOVE AL-UANON             TO  EFFDT05A                  CL*14
00678              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (5).            CL*14
00679                                                                   EL686
00680      IF EFFDT06L GREATER ZERO                                        CL*14
00681          MOVE EFFDT06I                 TO  DEEDIT-FIELD              CL*14
00682          PERFORM 8600-DEEDIT                                         CL*14
00683          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00684          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00685          PERFORM 8500-DATE-CONVERSION                                CL*14
00686          IF DATE-CONVERSION-ERROR                                    CL*14
00687              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00688              MOVE -1                   TO  EFFDT06L                  CL*14
00689              MOVE AL-UABON             TO  EFFDT06A                  CL*14
00690              PERFORM 9900-ERROR-FORMAT                               CL*14
00691            ELSE                                                      CL*14
00692              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT06I                  CL*14
00693              MOVE AL-UANON             TO  EFFDT06A                  CL*14
00694              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (6).            CL*14
00695                                                                      CL*14
00696      IF EFFDT07L GREATER ZERO                                        CL*14
00697          MOVE EFFDT07I                 TO  DEEDIT-FIELD              CL*14
00698          PERFORM 8600-DEEDIT                                         CL*14
00699          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00700          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00701          PERFORM 8500-DATE-CONVERSION                                CL*14
00702          IF DATE-CONVERSION-ERROR                                    CL*14
00703              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00704              MOVE -1                   TO  EFFDT07L                  CL*14
00705              MOVE AL-UABON             TO  EFFDT07A                  CL*14
00706              PERFORM 9900-ERROR-FORMAT                               CL*14
00707            ELSE                                                      CL*14
00708              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT07I                  CL*14
00709              MOVE AL-UANON             TO  EFFDT07A                  CL*14
00710              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (7).            CL*14
00711                                                                      CL*14
00712      IF EFFDT08L GREATER ZERO                                        CL*14
00713          MOVE EFFDT08I                 TO  DEEDIT-FIELD              CL*14
00714          PERFORM 8600-DEEDIT                                         CL*14
00715          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00716          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00717          PERFORM 8500-DATE-CONVERSION                                CL*14
00718          IF DATE-CONVERSION-ERROR                                    CL*14
00719              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00720              MOVE -1                   TO  EFFDT08L                  CL*14
00721              MOVE AL-UABON             TO  EFFDT08A                  CL*14
00722              PERFORM 9900-ERROR-FORMAT                               CL*14
00723            ELSE                                                      CL*14
00724              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT08I                  CL*14
00725              MOVE AL-UANON             TO  EFFDT08A                  CL*14
00726              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (8).            CL*14
00727                                                                      CL*14
00728      IF EFFDT09L GREATER ZERO                                        CL*14
00729          MOVE EFFDT09I                 TO  DEEDIT-FIELD              CL*14
00730          PERFORM 8600-DEEDIT                                         CL*14
00731          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00732          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00733          PERFORM 8500-DATE-CONVERSION                                CL*14
00734          IF DATE-CONVERSION-ERROR                                    CL*14
00735              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00736              MOVE -1                   TO  EFFDT09L                  CL*14
00737              MOVE AL-UABON             TO  EFFDT09A                  CL*14
00738              PERFORM 9900-ERROR-FORMAT                               CL*14
00739            ELSE                                                      CL*14
00740              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT09I                  CL*14
00741              MOVE AL-UANON             TO  EFFDT09A                  CL*14
00742              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (9).            CL*14
00743                                                                      CL*14
00744      IF EFFDT10L GREATER ZERO                                        CL*14
00745          MOVE EFFDT10I                 TO  DEEDIT-FIELD              CL*14
00746          PERFORM 8600-DEEDIT                                         CL*14
00747          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00748          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00749          PERFORM 8500-DATE-CONVERSION                                CL*14
00750          IF DATE-CONVERSION-ERROR                                    CL*14
00751              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00752              MOVE -1                   TO  EFFDT10L                  CL*14
00753              MOVE AL-UABON             TO  EFFDT10A                  CL*14
00754              PERFORM 9900-ERROR-FORMAT                               CL*14
00755            ELSE                                                      CL*14
00756              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT10I                  CL*14
00757              MOVE AL-UANON             TO  EFFDT10A                  CL*14
00758              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (10).           CL*14
00759                                                                      CL*14
00760      IF EFFDT11L GREATER ZERO                                        CL*14
00761          MOVE EFFDT11I                 TO  DEEDIT-FIELD              CL*14
00762          PERFORM 8600-DEEDIT                                         CL*14
00763          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00764          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00765          PERFORM 8500-DATE-CONVERSION                                CL*14
00766          IF DATE-CONVERSION-ERROR                                    CL*14
00767              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00768              MOVE -1                   TO  EFFDT11L                  CL*14
00769              MOVE AL-UABON             TO  EFFDT11A                  CL*14
00770              PERFORM 9900-ERROR-FORMAT                               CL*14
00771            ELSE                                                      CL*14
00772              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT11I                  CL*14
00773              MOVE AL-UANON             TO  EFFDT11A                  CL*14
00774              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (11).           CL*14
00775                                                                      CL*14
00776      IF EFFDT12L GREATER ZERO                                        CL*14
00777          MOVE EFFDT12I                 TO  DEEDIT-FIELD              CL*14
00778          PERFORM 8600-DEEDIT                                         CL*14
00779          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00780          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00781          PERFORM 8500-DATE-CONVERSION                                CL*14
00782          IF DATE-CONVERSION-ERROR                                    CL*14
00783              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00784              MOVE -1                   TO  EFFDT12L                  CL*14
00785              MOVE AL-UABON             TO  EFFDT12A                  CL*14
00786              PERFORM 9900-ERROR-FORMAT                               CL*14
00787            ELSE                                                      CL*14
00788              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT12I                  CL*14
00789              MOVE AL-UANON             TO  EFFDT12A                  CL*14
00790              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (12).           CL*14
00791                                                                      CL*14
00792      IF EFFDT13L GREATER ZERO                                        CL*14
00793          MOVE EFFDT13I                 TO  DEEDIT-FIELD              CL*14
00794          PERFORM 8600-DEEDIT                                         CL*14
00795          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00796          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00797          PERFORM 8500-DATE-CONVERSION                                CL*14
00798          IF DATE-CONVERSION-ERROR                                    CL*14
00799              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00800              MOVE -1                   TO  EFFDT13L                  CL*14
00801              MOVE AL-UABON             TO  EFFDT13A                  CL*14
00802              PERFORM 9900-ERROR-FORMAT                               CL*14
00803            ELSE                                                      CL*14
00804              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT13I                  CL*14
00805              MOVE AL-UANON             TO  EFFDT13A                  CL*14
00806              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (13).           CL*14
00807                                                                      CL*14
00808      IF EFFDT14L GREATER ZERO                                        CL*14
00809          MOVE EFFDT14I                 TO  DEEDIT-FIELD              CL*14
00810          PERFORM 8600-DEEDIT                                         CL*14
00811          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00812          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00813          PERFORM 8500-DATE-CONVERSION                                CL*14
00814          IF DATE-CONVERSION-ERROR                                    CL*14
00815              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00816              MOVE -1                   TO  EFFDT14L                  CL*14
00817              MOVE AL-UABON             TO  EFFDT14A                  CL*14
00818              PERFORM 9900-ERROR-FORMAT                               CL*14
00819            ELSE                                                      CL*14
00820              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT14I                  CL*14
00821              MOVE AL-UANON             TO  EFFDT14A                  CL*14
00822              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (14).           CL*14
00823                                                                      CL*14
00824      IF EFFDT15L GREATER ZERO                                        CL*14
00825          MOVE EFFDT15I                 TO  DEEDIT-FIELD              CL*14
00826          PERFORM 8600-DEEDIT                                         CL*14
00827          MOVE DEEDIT-FIELD-V0          TO  DC-GREG-DATE-1-MDY        CL*14
00828          MOVE '4'                      TO  DC-OPTION-CODE            CL*14
00829          PERFORM 8500-DATE-CONVERSION                                CL*14
00830          IF DATE-CONVERSION-ERROR                                    CL*14
00831              MOVE ER-0348              TO  EMI-ERROR                 CL*14
00832              MOVE -1                   TO  EFFDT15L                  CL*14
00833              MOVE AL-UABON             TO  EFFDT15A                  CL*14
00834              PERFORM 9900-ERROR-FORMAT                               CL*14
00835            ELSE                                                      CL*14
00836              MOVE DC-GREG-DATE-1-EDIT  TO  EFFDT15I                  CL*14
00837              MOVE AL-UANON             TO  EFFDT15A                  CL*14
00838              MOVE DC-BIN-DATE-1        TO  BIN-EFFDT (15).           CL*14
00839                                                                      CL*14
00840      IF EMI-ERROR NOT = ZERO                                         CL*14
00841          MOVE +0                     TO PI-PROC-SW                   CL*14
00842          PERFORM 8200-SEND-DATAONLY                                  CL*14
00843       ELSE                                                           CL*14
00844          GO TO 0200-PROCESS-CHECK-RELEASE.                           CL*14
00845                                                                      CL*14
00846  0140-NOT-FOUND.                                                     CL*14
00847                                                                   EL686
00848      MOVE ER-0331                TO  EMI-ERROR.                   EL686
00849      PERFORM 8200-SEND-DATAONLY.                                  EL686
00850                                                                   EL686
00851  0200-PROCESS-CHECK-RELEASE.                                         CL*14
00852                                                                   EL686
00853 *    NOTE ******************************************************* EL686
00854 *         *                                                     * EL686
00855 *         *      OBTAIN EXCLUSIVE CONTROL OF THE CHECK QUEUE    * EL686
00856 *         *  DATASET DURING THE GENERATION OF THE CHECK QUEUE   * EL686
00857 *         *  RECORDS.                                           * EL686
00858 *         *                                                     * EL686
00859 *         *******************************************************.EL686
00860                                                                   EL686
00861 ******  ACTUAL CHECK RELEASE WILL NOT TAKE PLACE                     CL*14
00862 ******         UNLESS PF1 WAS PRESSED                                CL*14
00863                                                                   EL686
00864      IF EIBAID = DFHPF1                                              CL*14
00865          EXEC CICS ENQ                                               CL*14
00866              RESOURCE (WS-ERCHKQ-DSID)                               CL*14
00867              LENGTH   (11)                                           CL*14
00868          END-EXEC                                                    CL*14
00869          EXEC CICS ENQ                                               CL*14
00870              RESOURCE (WS-ERCMKQ-DSID)                               CL*14
00871              LENGTH   (11)                                           CL*14
00872          END-EXEC.                                                   CL*14
00873                                                                   EL686
00874      EJECT                                                        EL686
00875 *    NOTE ******************************************************* EL686
00876 *         *                                                     * EL686
00877 *         *      GET THE CONTROL GROUP NUMBER FROM THE COMPANY  * EL686
00878 *         *  CONTROL RECORD OF THE CONTROL FILE.                * EL686
00879 *         *                                                     * EL686
00880 *         *******************************************************.EL686
00881                                                                   EL686
00882      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.            CL**6
00883                                                                   EL686
00884      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.              CL**6
00885      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.             CL**6
00886      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.             CL**6
00887                                                                   EL686
00888      IF EIBAID = DFHPF1                                              CL*14
00889          EXEC CICS READ UPDATE                                       CL*14
00890              DATASET (WS-ELCNTL-DSID)                                CL*14
00891              RIDFLD  (WS-CONTROL-FILE-KEY)                           CL*14
00892              SET     (ADDRESS OF CONTROL-FILE)                       CL*16
00893          END-EXEC                                                    CL*14
00894       ELSE                                                           CL*14
00895          EXEC CICS READ                                              CL*14
00896              DATASET (WS-ELCNTL-DSID)                                CL*14
00897              RIDFLD  (WS-CONTROL-FILE-KEY)                           CL*14
00898              SET     (ADDRESS OF CONTROL-FILE)                       CL*16
00899          END-EXEC.                                                   CL*14
00900                                                                   EL686
00901      ADD +1                      TO  CF-CR-CHECK-QUE-COUNTER.        CL**6
00902                                                                   EL686
00903      IF CR-QUE-COUNT-RESET                                        EL686
00904          MOVE +1                 TO  CF-CR-CHECK-QUE-COUNTER.     EL686
00905                                                                   EL686
00906      MOVE CF-CR-CHECK-QUE-COUNTER  TO  WS-CHECK-QUE-COUNTER       EL686
00907                                        PI-CK-CONTROL-NO.          EL686
00908                                                                   EL686
00909      MOVE WS-ELCNTL-DSID         TO  JP-FILE-ID.                     CL*14
00910      MOVE 'C'                    TO  JP-RECORD-TYPE.                 CL**6
00911      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.                 CL**6
00912                                                                   EL686
00913      IF EIBAID = DFHPF1                                              CL*14
00914          EXEC CICS REWRITE                                           CL*14
00915              DATASET (WS-ELCNTL-DSID)                                CL*14
00916              FROM    (CONTROL-FILE)                                  CL*14
00917          END-EXEC                                                    CL*14
00918          PERFORM 8400-LOG-JOURNAL-RECORD.                            CL*14
00919                                                                   EL686
00920      EJECT                                                        EL686
00921 *    NOTE ******************************************************* EL686
00922 *         *                                                     * EL686
00923 *         *      GET STORAGE FOR WRITING OF THE CHECK QUEUE     * EL686
00924 *         *  RECORDS.                                           * EL686
00925 *         *                                                     * EL686
00926 *         *******************************************************.EL686
00927                                                                   EL686
00928      IF  AOPTIONI     = '5' OR '6'                                   CL*18
00929          EXEC CICS GETMAIN                                           CL**2
00930              SET (ADDRESS OF COMMISSION-CHECK-QUE)                   CL*16
00931              LENGTH (WS-ERCMKQ-LENGTH)                               CL**2
00932              INITIMG (WS-SPACES) END-EXEC                            CL**2
00933          COMPUTE WS-SEQ-NO = WS-TIME-WORK  *  10                     CL**2
00934          MOVE AOPTIONI  TO  PI-PGM-PRINT-OPT                         CL**2
00935          GO TO 0280-OPTION-5                                         CL*18
00936      ELSE                                                            CL*18
00937          EXEC CICS GETMAIN                                           CL*18
00938              SET (ADDRESS OF CHECK-QUE)                              CL*18
00939              LENGTH (WS-ERCHKQ-LENGTH)                               CL*18
00940              INITIMG (WS-SPACES) END-EXEC.                           CL*18
00941                                                                   EL686
00942      IF AOPTIONI EQUAL '1' OR '2'                                    CL*10
00943          NEXT SENTENCE                                            EL686
00944      ELSE                                                         EL686
00945          GO TO 0260-END-OF-ERPYAJ.                                   CL*14
00946                                                                   EL686
00947 *    NOTE ******************************************************* EL686
00948 *         *                                                     * EL686
00949 *         *      BROWSE THE PENDING PAYMENTS AND ADJUSTMENTS    * EL686
00950 *         *  FILE FOR PAYMENTS PENDING.                         * EL686
00951 *         *                                                     * EL686
00952 *         *      WHEN A PAYMENT IS PENDING, GENERATE A CHECK    * EL686
00953 *         *  QUEUE RECORD FOR RELEASE.                          * EL686
00954 *         *                                                     * EL686
00955 *         *******************************************************.EL686
00956                                                                   EL686
00957      MOVE LOW-VALUES             TO  WS-PENDING-PAYMENTS-KEY.     EL686
00958      MOVE PI-COMPANY-CD          TO  WS-PPK-COMPANY-CD.           EL686
00959                                                                   EL686
00960  0225-STARTBR-ERPYAJ.                                                CL*14
00961                                                                   EL686
00962      IF WS-ERPYAJ-BROWSE-NOT-STARTED                              EL686
00963          EXEC CICS STARTBR                                        EL686
00964              DATASET (WS-ERPYAJ-DSID)                                CL*14
00965              RIDFLD  (WS-PENDING-PAYMENTS-KEY)                    EL686
00966              GTEQ                                                 EL686
00967              END-EXEC.                                            EL686
00968                                                                   EL686
00969      MOVE +1 TO WS-ERPYAJ-BROWSE-SW.                              EL686
00970                                                                   EL686
00971      EXEC CICS HANDLE CONDITION                                   EL686
00972          ENDFILE    (0260-END-OF-ERPYAJ)                             CL*14
00973          END-EXEC.                                                EL686
00974                                                                   EL686
00975  0250-READNEXT-ERPYAJ.                                               CL*14
00976                                                                   EL686
00977      EXEC CICS READNEXT                                           EL686
00978          DATASET (WS-ERPYAJ-DSID)                                    CL*14
00979          RIDFLD  (WS-PENDING-PAYMENTS-KEY)                        EL686
00980          SET     (ADDRESS OF PENDING-PAY-ADJ) END-EXEC.              CL*16
00981                                                                   EL686
00982      IF WS-PPK-COMPANY-CD EQUAL PI-COMPANY-CD                     EL686
00983          NEXT SENTENCE                                            EL686
00984      ELSE                                                         EL686
00985          IF AOPTIONI EQUAL '1'                                    EL686
00986              GO TO 0260-END-OF-ERPYAJ                                CL*14
00987          ELSE                                                     EL686
00988              GO TO 0400-END-OF-SEARCH.                               CL*14
00989                                                                   EL686
00990      IF ACARRL GREATER ZERO                                       EL686
00991          IF PY-CARRIER NOT = ACARRI                                  CL*14
00992             GO TO 0250-READNEXT-ERPYAJ.                              CL*14
00993                                                                      CL*14
00994      IF AGROUPL GREATER ZERO                                         CL*14
00995         IF PY-GROUPING NOT = AGROUPI                                 CL*14
00996             GO TO 0250-READNEXT-ERPYAJ.                              CL*14
00997                                                                      CL*14
00998      IF ABYL GREATER ZERO                                            CL*14
00999         IF PY-LAST-MAINT-BY NOT = ABYI                               CL*14
01000             GO TO 0250-READNEXT-ERPYAJ.                              CL*14
01001                                                                      CL*14
01002      IF AAMTL GREATER ZERO                                           CL*14
01003          IF PY-ENTRY-AMT LESS PI-CHECK-AMOUNT                        CL*14
01004              NEXT SENTENCE                                           CL*14
01005            ELSE                                                      CL*14
01006              GO TO 0250-READNEXT-ERPYAJ.                             CL*14
01007                                                                   EL686
01008      IF  PY-CHARGE-TO-AGENT                                          CL*10
01009           IF PY-BILLING-CHECK OR                                     CL*10
01010              PY-GA-CHECK                                             CL*10
01011              NEXT SENTENCE                                        EL686
01012           ELSE                                                       CL*10
01013              GO TO 0250-READNEXT-ERPYAJ                              CL*14
01014      ELSE                                                         EL686
01015           GO TO 0250-READNEXT-ERPYAJ.                                CL*14
01016                                                                   EL686
01017      IF PY-CHECK-QUE-CONTROL  EQUAL ZEROS AND                     EL686
01018         PY-CHECK-QUE-SEQUENCE EQUAL ZEROS                         EL686
01019          NEXT SENTENCE                                            EL686
01020      ELSE                                                         EL686
01021          GO TO 0250-READNEXT-ERPYAJ.                                 CL*14
01022                                                                   EL686
01023 *    NOTE ******************************************************* EL686
01024 *         *                                                     * EL686
01025 *         *      THE PAYMENT TRAILER HAS MET ALL OF THE         * EL686
01026 *         *  QUALIFICATIONS FOR THIS CHECK RELEASE, NOW         * EL686
01027 *         *  GENERATE A CHECK QUEUE RECORD.                     * EL686
01028 *         *                                                     * EL686
01029 *         *******************************************************.EL686
01030                                                                   EL686
01031      ADD +1                      TO  WS-RELEASED-COUNT.              CL**6
01032      ADD PY-ENTRY-AMT            TO  WS-RELEASED-AMOUNT.             CL**6
01033                                                                   EL686
01034      IF EIBAID NOT = DFHPF1                                          CL*14
01035          GO TO 0250-READNEXT-ERPYAJ.                                 CL*14
01036                                                                      CL*14
01037      IF WS-ERPYAJ-BROWSE-STARTED                                  EL686
01038          EXEC CICS ENDBR                                          EL686
01039              DATASET (WS-ERPYAJ-DSID)                                CL*14
01040              END-EXEC                                             EL686
01041          MOVE +0 TO WS-ERPYAJ-BROWSE-SW.                          EL686
01042                                                                   EL686
01043      EXEC CICS READ UPDATE                                        EL686
01044          DATASET (WS-ERPYAJ-DSID)                                    CL*14
01045          RIDFLD  (WS-PENDING-PAYMENTS-KEY)                        EL686
01046          SET     (ADDRESS OF PENDING-PAY-ADJ)                        CL*16
01047      END-EXEC.                                                       CL*14
01048                                                                   EL686
01049      MOVE SPACES                 TO  CHECK-QUE.                      CL**6
01050      MOVE 'CQ'                   TO  CQ-RECORD-ID.                   CL**6
01051      MOVE PY-COMPANY-CD          TO  CQ-COMPANY-CD.                  CL**6
01052      MOVE WS-CHECK-QUE-COUNTER   TO  CQ-CONTROL-NUMBER            EL686
01053                                      PY-CHECK-QUE-CONTROL         EL686
01054      MOVE WS-CHECK-COUNTER       TO  CQ-SEQUENCE-NUMBER           EL686
01055                                      PY-CHECK-QUE-SEQUENCE.          CL**6
01056      ADD +1                      TO  WS-CHECK-COUNTER.               CL**6
01057      MOVE 'Q'                    TO  CQ-ENTRY-TYPE.                  CL**6
01058      MOVE PY-CARRIER             TO  CQ-PYAJ-CARRIER.                CL**6
01059      MOVE PY-GROUPING            TO  CQ-PYAJ-GROUPING.               CL**6
01060      MOVE PY-FIN-RESP            TO  CQ-PYAJ-FIN-RESP.               CL**6
01061      MOVE PY-ACCOUNT             TO  CQ-PYAJ-ACCOUNT.                CL**6
01062      MOVE PY-FILE-SEQ-NO         TO  CQ-PYAJ-SEQ.                    CL**6
01063      MOVE PY-RECORD-TYPE         TO  CQ-PYAJ-REC-TYPE.               CL**6
01064      MOVE PY-ENTRY-AMT           TO  CQ-CHECK-AMOUNT.                CL**6
01065      MOVE SPACES                 TO  WS-CHECK-NUMBER.                CL*12
01066      MOVE PY-CHECK-NUMBER        TO  WS-CHECK-NO.                    CL**6
01067      MOVE WS-CHECK-NUMBER        TO  CQ-CHECK-NUMBER.                CL**6
01068      MOVE '1'                    TO  CQ-PAYMENT-TYPE.                CL**6
01069      MOVE ZERO                   TO  CQ-TIMES-PRINTED                CL*14
01070                                      CQ-PRINT-AT-HHMM.               CL*14
01071      MOVE PI-CURRENT-DATE-BIN    TO  CQ-CHECK-WRITTEN-DT.            CL**6
01072      MOVE PY-REPORTED-DT         TO  CQ-CHECK-BY-USER.               CL**6
01073      MOVE +6860                  TO  CQ-LAST-UPDATED-BY.             CL**6
01074      MOVE CHECK-QUE              TO  JP-RECORD-AREA.                 CL**6
01075      MOVE 'A'                    TO  JP-RECORD-TYPE.                 CL**6
01076      MOVE WS-ERCHKQ-DSID         TO  JP-FILE-ID.                     CL*14
01077                                                                   EL686
01078      EXEC CICS WRITE                                              EL686
01079          DATASET (WS-ERCHKQ-DSID)                                    CL*14
01080          FROM    (CHECK-QUE)                                      EL686
01081          RIDFLD  (CQ-CONTROL-PRIMARY) END-EXEC.                      CL**6
01082                                                                   EL686
01083      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**6
01084                                                                   EL686
01085      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.            EL686
01086                                                                   EL686
01087      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.                 CL**6
01088      MOVE 'C'                    TO  JP-RECORD-TYPE.                 CL**6
01089      MOVE WS-ERPYAJ-DSID            TO  JP-FILE-ID.                  CL*14
01090                                                                   EL686
01091      EXEC CICS REWRITE                                            EL686
01092          DATASET (WS-ERPYAJ-DSID)                                    CL*14
01093          FROM    (PENDING-PAY-ADJ) END-EXEC.                         CL**6
01094                                                                   EL686
01095      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**6
01096                                                                   EL686
01097      GO TO 0225-STARTBR-ERPYAJ.                                      CL*14
01098                                                                   EL686
01099      EJECT                                                        EL686
01100                                                                   EL686
01101  0260-END-OF-ERPYAJ.                                                 CL*14
01102                                                                   EL686
01103      IF AOPTIONI EQUAL '1' OR '3' OR '4'                             CL*10
01104          NEXT SENTENCE                                            EL686
01105      ELSE                                                         EL686
01106          GO TO 0400-END-OF-SEARCH.                                   CL*14
01107                                                                   EL686
01108      IF WS-ERPYAJ-BROWSE-STARTED                                  EL686
01109          EXEC CICS ENDBR                                          EL686
01110              DATASET (WS-ERPYAJ-DSID)                                CL*14
01111              END-EXEC                                             EL686
01112          MOVE +0 TO WS-ERPYAJ-BROWSE-SW.                          EL686
01113                                                                   EL686
01114 *    NOTE ******************************************************* EL686
01115 *         *                                                     * EL686
01116 *         *      BROWSE THE CHECK-MAINTENANCE FILE FOR ANY      * EL686
01117 *         *  PENDING PAYMENTS FOR RELEASE                       * EL686
01118 *         *                                                     * EL686
01119 *         *      WHEN A PAYMENT IS PENDING, GENERATE A CHECK    * EL686
01120 *         *  QUEUE RECORD FOR RELEASE.                          * EL686
01121 *         *                                                     * EL686
01122 *         *******************************************************.EL686
01123                                                                   EL686
01124      MOVE LOW-VALUES             TO  WS-CHECK-MAINT-KEY.          EL686
01125      MOVE PI-COMPANY-CD          TO  WS-CHK-COMPANY-CD.           EL686
01126                                                                   EL686
01127  0265-STARTBR-ERCHEK.                                                CL*14
01128                                                                   EL686
01129      IF WS-ERCHEK-BROWSE-NOT-STARTED                                 CL*10
01130          EXEC CICS STARTBR                                        EL686
01131              DATASET (WS-ERCHEK-DSID)                                CL*14
01132              RIDFLD  (WS-CHECK-MAINT-KEY)                         EL686
01133              GTEQ                                                 EL686
01134              END-EXEC.                                            EL686
01135                                                                   EL686
01136      MOVE +1 TO WS-ERCHEK-BROWSE-SW.                              EL686
01137                                                                   EL686
01138      EXEC CICS HANDLE CONDITION                                   EL686
01139          ENDFILE    (0400-END-OF-SEARCH)                             CL*14
01140          END-EXEC.                                                EL686
01141                                                                   EL686
01142  0270-READNEXT-ERCHEK.                                               CL*14
01143                                                                   EL686
01144      EXEC CICS READNEXT                                           EL686
01145          DATASET (WS-ERCHEK-DSID)                                    CL*14
01146          RIDFLD  (WS-CHECK-MAINT-KEY)                             EL686
01147          SET     (ADDRESS OF CHECK-RECORDS) END-EXEC.                CL*16
01148                                                                   EL686
01149      IF WS-CHK-COMPANY-CD NOT = PI-COMPANY-CD                     EL686
01150          GO TO 0400-END-OF-SEARCH.                                   CL*14
01151                                                                      CL*14
01152      IF ACARRL GREATER ZERO                                          CL*14
01153          IF CH-CARRIER NOT = ACARRI                                  CL*14
01154              GO TO 0270-READNEXT-ERCHEK.                             CL*14
01155                                                                   EL686
01156      IF AGROUPL GREATER ZERO                                         CL*14
01157         IF CH-GROUPING NOT = AGROUPI                                 CL*14
01158             GO TO 0270-READNEXT-ERCHEK.                              CL*14
01159                                                                      CL*14
01160      IF ASTATL GREATER ZERO                                          CL*14
01161         IF CH-STATE NOT = ASTATI                                     CL*14
01162             GO TO 0270-READNEXT-ERCHEK.                              CL*14
01163                                                                      CL*14
01164      IF AACCTL GREATER ZERO                                          CL*14
01165         IF CH-ACCOUNT NOT = AACCTI                                   CL*14
01166             GO TO 0270-READNEXT-ERCHEK.                              CL*14
01167                                                                      CL*14
01168      IF ABYL  GREATER ZERO                                           CL*14
01169         IF CH-RECORDED-BY NOT = ABYI                                 CL*14
01170             GO TO 0270-READNEXT-ERCHEK.                              CL*14
01171                                                                      CL*14
01172      IF AAMTL GREATER ZERO                                           CL*14
01173         IF CH-AMOUNT-PAID LESS PI-CHECK-AMOUNT                       CL*14
01174              NEXT SENTENCE                                           CL*14
01175            ELSE                                                      CL*14
01176              GO TO 0270-READNEXT-ERCHEK.                             CL*14
01177                                                                      CL*14
01178      MOVE ZERO               TO PI-CERT-SW.                          CL*14
01179                                                                      CL*14
01180      IF ZERO = CERTO01L AND CERTO02L AND CERTO03L AND                CL*14
01181                CERTO04L AND CERTO05L AND CERTO06L AND                CL*14
01182                CERTO07L AND CERTO08L AND CERTO09L AND                CL*14
01183                CERTO10L AND CERTO11L AND CERTO12L AND                CL*14
01184                CERTO13L AND CERTO14L AND CERTO15L                    CL*14
01185           MOVE +1           TO PI-CERT-SW                            CL*14
01186        ELSE                                                          CL*14
01187      IF CERTO01I       = CH-CERT-NO     AND                          CL*14
01188         BIN-EFFDT (1)  = CH-CERT-EFF-DT                              CL*14
01189           MOVE +1           TO PI-CERT-SW                            CL*14
01190       ELSE                                                           CL*14
01191      IF CERTO02I       = CH-CERT-NO     AND                          CL*14
01192         BIN-EFFDT (2)  = CH-CERT-EFF-DT                              CL*14
01193           MOVE +1           TO PI-CERT-SW                            CL*14
01194       ELSE                                                           CL*14
01195      IF CERTO03I       = CH-CERT-NO     AND                          CL*14
01196         BIN-EFFDT (3)  = CH-CERT-EFF-DT                              CL*14
01197           MOVE +1           TO PI-CERT-SW                            CL*14
01198       ELSE                                                           CL*14
01199      IF CERTO04I       = CH-CERT-NO     AND                          CL*14
01200         BIN-EFFDT (4)  = CH-CERT-EFF-DT                              CL*14
01201           MOVE +1           TO PI-CERT-SW                            CL*14
01202       ELSE                                                           CL*14
01203      IF CERTO05I       = CH-CERT-NO     AND                          CL*14
01204         BIN-EFFDT (5)  = CH-CERT-EFF-DT                              CL*14
01205           MOVE +1           TO PI-CERT-SW                            CL*14
01206       ELSE                                                           CL*14
01207      IF CERTO06I       = CH-CERT-NO     AND                          CL*14
01208         BIN-EFFDT (6)  = CH-CERT-EFF-DT                              CL*14
01209           MOVE +1           TO PI-CERT-SW                            CL*14
01210       ELSE                                                           CL*14
01211      IF CERTO07I       = CH-CERT-NO     AND                          CL*14
01212         BIN-EFFDT (7)  = CH-CERT-EFF-DT                              CL*14
01213           MOVE +1           TO PI-CERT-SW                            CL*14
01214       ELSE                                                           CL*14
01215      IF CERTO08I       = CH-CERT-NO     AND                          CL*14
01216         BIN-EFFDT (8)  = CH-CERT-EFF-DT                              CL*14
01217           MOVE +1           TO PI-CERT-SW                            CL*14
01218       ELSE                                                           CL*14
01219      IF CERTO09I       = CH-CERT-NO     AND                          CL*14
01220         BIN-EFFDT (9)  = CH-CERT-EFF-DT                              CL*14
01221           MOVE +1           TO PI-CERT-SW                            CL*14
01222       ELSE                                                           CL*14
01223      IF CERTO10I       = CH-CERT-NO     AND                          CL*14
01224         BIN-EFFDT (10) = CH-CERT-EFF-DT                              CL*14
01225           MOVE +1           TO PI-CERT-SW                            CL*14
01226       ELSE                                                           CL*14
01227      IF CERTO11I       = CH-CERT-NO     AND                          CL*14
01228         BIN-EFFDT (11) = CH-CERT-EFF-DT                              CL*14
01229           MOVE +1           TO PI-CERT-SW                            CL*14
01230       ELSE                                                           CL*14
01231      IF CERTO12I       = CH-CERT-NO     AND                          CL*14
01232         BIN-EFFDT (12) = CH-CERT-EFF-DT                              CL*14
01233           MOVE +1           TO PI-CERT-SW                            CL*14
01234       ELSE                                                           CL*14
01235      IF CERTO13I       = CH-CERT-NO     AND                          CL*14
01236         BIN-EFFDT (13) = CH-CERT-EFF-DT                              CL*14
01237           MOVE +1           TO PI-CERT-SW                            CL*14
01238       ELSE                                                           CL*14
01239      IF CERTO14I       = CH-CERT-NO     AND                          CL*14
01240         BIN-EFFDT (14) = CH-CERT-EFF-DT                              CL*14
01241           MOVE +1           TO PI-CERT-SW                            CL*14
01242       ELSE                                                           CL*14
01243      IF CERTO15I       = CH-CERT-NO     AND                          CL*14
01244         BIN-EFFDT (15) = CH-CERT-EFF-DT                              CL*14
01245           MOVE +1           TO PI-CERT-SW.                           CL*14
01246                                                                      CL*14
01247      IF PI-CERT-NOT-MATCHED                                          CL*14
01248          GO TO 0270-READNEXT-ERCHEK.                                 CL*14
01249                                                                   EL686
01250      IF CH-MAINT-CHECK                                               CL*10
01251          IF AOPTIONI EQUAL '1' OR '4'                                CL*10
01252              NEXT SENTENCE                                           CL*10
01253          ELSE                                                        CL*10
01254              GO TO 0270-READNEXT-ERCHEK.                             CL*14
01255                                                                      CL*10
01256      IF CH-REFUND-CHECK                                              CL*10
01257          IF AOPTIONI EQUAL '1' OR '3'                                CL*10
01258              NEXT SENTENCE                                           CL*10
01259          ELSE                                                        CL*10
01260              GO TO 0270-READNEXT-ERCHEK.                             CL*14
01261                                                                      CL*10
01262      IF CH-CHECK-QUE-CONTROL  EQUAL ZEROS AND                     EL686
01263         CH-CHECK-QUE-SEQUENCE EQUAL ZEROS                         EL686
01264          NEXT SENTENCE                                            EL686
01265      ELSE                                                         EL686
01266          GO TO 0270-READNEXT-ERCHEK.                                 CL*14
01267                                                                   EL686
01268 *    NOTE ******************************************************* EL686
01269 *         *                                                     * EL686
01270 *         *      THE CHECK MAINT TRAILER HAS MET ALL OF THE     * EL686
01271 *         *  QUALIFICATIONS FOR THIS CHECK RELEASE, NOW         * EL686
01272 *         *  GENERATE A CHECK QUEUE RECORD.                     * EL686
01273 *         *                                                     * EL686
01274 *         *******************************************************.EL686
01275                                                                   EL686
01276      IF CH-VOID-DT = LOW-VALUES  AND                                 CL*13
01277        NOT MANUAL-CHECK-WRITTEN                                      CL*13
01278          ADD +1                  TO  WS-RELEASED-COUNT               CL*13
01279          ADD CH-AMOUNT-PAID      TO  WS-RELEASED-AMOUNT.             CL*13
01280                                                                   EL686
01281      IF EIBAID NOT = DFHPF1                                          CL*14
01282          GO TO 0270-READNEXT-ERCHEK.                                 CL*14
01283                                                                      CL*14
01284      IF WS-ERCHEK-BROWSE-STARTED                                  EL686
01285          EXEC CICS ENDBR                                          EL686
01286              DATASET (WS-ERCHEK-DSID)                                CL*14
01287              END-EXEC                                             EL686
01288          MOVE +0 TO WS-ERCHEK-BROWSE-SW.                          EL686
01289                                                                   EL686
01290      EXEC CICS READ UPDATE                                        EL686
01291          DATASET (WS-ERCHEK-DSID)                                    CL*14
01292          RIDFLD  (WS-CHECK-MAINT-KEY)                             EL686
01293          SET     (ADDRESS OF CHECK-RECORDS) END-EXEC.                CL*16
01294                                                                   EL686
01295      MOVE SPACES                 TO  CHECK-QUE.                      CL**6
01296      MOVE 'CQ'                   TO  CQ-RECORD-ID.                   CL**6
01297      MOVE CH-COMPANY-CD          TO  CQ-COMPANY-CD.                  CL**6
01298      MOVE WS-CHECK-QUE-COUNTER   TO  CQ-CONTROL-NUMBER            EL686
01299                                      CH-CHECK-QUE-CONTROL.           CL**6
01300      MOVE WS-CHECK-COUNTER       TO  CQ-SEQUENCE-NUMBER           EL686
01301                                      CH-CHECK-QUE-SEQUENCE.          CL**6
01302      ADD +1                      TO  WS-CHECK-COUNTER.               CL**6
01303      MOVE 'Q'                    TO  CQ-ENTRY-TYPE.                  CL**6
01304      MOVE ZERO                   TO  CQ-TIMES-PRINTED                CL*13
01305                                      CQ-PRINT-AT-HHMM.               CL*13
01306      MOVE PI-CURRENT-DATE-BIN    TO  CQ-CHECK-WRITTEN-DT.            CL*13
01307      IF MANUAL-CHECK-WRITTEN                                         CL*13
01308          MOVE 'M'                TO  CQ-ENTRY-TYPE                   CL*13
01309          MOVE +1                 TO  CQ-TIMES-PRINTED                CL*13
01310          MOVE CH-CHECK-NO        TO  CQ-CHECK-NUMBER                 CL*13
01311          MOVE CH-CHECK-WRITTEN-DT TO CQ-CHECK-WRITTEN-DT.            CL*13
01312      MOVE LOW-VALUES             TO  CQ-CHECK-VOIDED-DT.             CL*13
01313      IF CH-VOID-DT NOT = LOW-VALUES                                  CL*13
01314          MOVE 'V'                TO  CQ-ENTRY-TYPE                   CL*13
01315                                      CQ-VOID-INDICATOR               CL*13
01316          MOVE CH-VOID-DT         TO  CQ-CHECK-VOIDED-DT.             CL*13
01317      MOVE CH-CARRIER             TO  CQ-CHEK-CARRIER.                CL**6
01318      MOVE CH-GROUPING            TO  CQ-CHEK-GROUPING.               CL**6
01319      MOVE CH-STATE               TO  CQ-CHEK-STATE.                  CL**6
01320      MOVE CH-ACCOUNT             TO  CQ-CHEK-ACCOUNT.                CL**6
01321      MOVE CH-SEQUENCE-NO         TO  CQ-CHEK-SEQ-NO.                 CL**6
01322      MOVE CH-CERT-EFF-DT         TO  CQ-CHEK-CERT-EFF-DT.            CL**6
01323      MOVE CH-CERT-NO             TO  CQ-CHEK-CERT-NO.                CL**6
01324      MOVE CH-COMP-FIN-RESP       TO  CQ-CHEK-FIN-RESP.               CL*10
01325      MOVE CH-CHECK-NO            TO  CQ-CHECK-NUMBER.                CL**6
01326                                                                      CL*10
01327      IF CH-REFUND-CHECK                                              CL*10
01328          MOVE '2'                TO  CQ-PAYMENT-TYPE                 CL*10
01329      ELSE                                                            CL*10
01330          MOVE '3'                TO  CQ-PAYMENT-TYPE.                CL*10
01331                                                                      CL*10
01332      MOVE CH-AMOUNT-PAID         TO  CQ-CHECK-AMOUNT.                CL**6
01333      MOVE CH-RECORDED-BY         TO  CQ-CHECK-BY-USER.               CL**6
01334      MOVE +6860                  TO  CQ-LAST-UPDATED-BY.             CL**6
01335      MOVE CHECK-QUE              TO  JP-RECORD-AREA.                 CL**6
01336      MOVE 'A'                    TO  JP-RECORD-TYPE.                 CL**6
01337      MOVE WS-ERCHKQ-DSID         TO  JP-FILE-ID.                     CL*14
01338                                                                   EL686
01339      EXEC CICS WRITE                                              EL686
01340          DATASET (WS-ERCHKQ-DSID)                                    CL*14
01341          FROM    (CHECK-QUE)                                      EL686
01342          RIDFLD  (CQ-CONTROL-PRIMARY) END-EXEC.                      CL**6
01343                                                                   EL686
01344      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**6
01345                                                                   EL686
01346      MOVE CHECK-RECORDS          TO  JP-RECORD-AREA.                 CL**6
01347      MOVE 'C'                    TO  JP-RECORD-TYPE.                 CL**6
01348      MOVE WS-ERCHEK-DSID         TO  JP-FILE-ID.                     CL*14
01349                                                                   EL686
01350      EXEC CICS REWRITE                                            EL686
01351          DATASET (WS-ERCHEK-DSID)                                    CL*14
01352          FROM    (CHECK-RECORDS) END-EXEC.                           CL**6
01353                                                                   EL686
01354      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**6
01355                                                                   EL686
01356      GO TO 0265-STARTBR-ERCHEK.                                      CL*14
01357                                                                   EL686
01358      EJECT                                                        EL686
01359                                                                      CL**2
01360  0280-OPTION-5.                                                      CL*14
01361                                                                      CL**2
01362      IF PI-PROCESSOR-ID  =  'LGXX'                                   CL*10
01363          GO TO 0281-CONTINUE.                                        CL*10
01364                                                                      CL*14
01365      EXEC CICS READQ TS                                              CL*10
01366          QUEUE  (QID)                                                CL*10
01367          INTO   (SECURITY-CONTROL)                                   CL*10
01368          LENGTH (SC-COMM-LENGTH)                                     CL*10
01369          ITEM   (SC-ITEM)                                            CL*10
01370      END-EXEC.                                                       CL*10
01371                                                                      CL**2
01372      MOVE SC-CREDIT-DISPLAY (7)  TO PI-DISPLAY-CAP.                  CL*10
01373      MOVE SC-CREDIT-UPDATE  (7)  TO PI-MODIFY-CAP.                   CL*10
01374                                                                      CL*10
01375      IF NOT MODIFY-CAP                                               CL*10
01376          MOVE 'READ'             TO  SM-READ                         CL*10
01377          PERFORM 9995-SECURITY-VIOLATION                             CL*10
01378          MOVE ER-0070            TO EMI-ERROR                        CL*10
01379          PERFORM 9900-ERROR-FORMAT                                   CL*14
01380          GO TO 8100-SEND-INITIAL-MAP.                                CL*10
01381                                                                      CL*10
01382  0281-CONTINUE.                                                      CL*10
01383      MOVE LOW-VALUES             TO  WS-COMCK-MAINT-KEY.             CL**2
01384      MOVE PI-COMPANY-CD          TO  WS-CMK-COMPANY-CD.              CL**2
01385                                                                      CL**2
01386  0285-STARTBR-ERCMCK.                                                CL*14
01387                                                                      CL**2
01388      EXEC CICS STARTBR                                               CL*14
01389          DATASET (WS-ERCMCK-DSID)                                    CL*14
01390          RIDFLD  (WS-COMCK-MAINT-KEY)                                CL*14
01391          GTEQ                                                        CL*14
01392      END-EXEC.                                                       CL*14
01393                                                                      CL**2
01394      MOVE +1  TO  WS-ERCMCK-BROWSE-SW.                               CL**2
01395                                                                      CL**2
01396      EXEC CICS HANDLE CONDITION                                      CL**2
01397          ENDFILE    (0400-END-OF-SEARCH)                             CL*14
01398          END-EXEC.                                                   CL**2
01399                                                                      CL**2
01400  0290-READNEXT-ERCMCK.                                               CL*14
01401                                                                      CL**2
01402      EXEC CICS READNEXT                                              CL**2
01403          DATASET (WS-ERCMCK-DSID)                                    CL*14
01404          RIDFLD  (WS-COMCK-MAINT-KEY)                                CL**2
01405          SET     (ADDRESS OF COMM-CHECK-RECORDS) END-EXEC.           CL*16
01406                                                                      CL**2
01407      IF WS-CMK-COMPANY-CD NOT = PI-COMPANY-CD                        CL**2
01408          GO TO 0400-END-OF-SEARCH.                                   CL*14
01409                                                                      CL**2
01410      IF ACARRL GREATER ZERO                                          CL**2
01411          IF CK-CARRIER NOT = ACARRI                                  CL*14
01412              GO TO 0290-READNEXT-ERCMCK.                             CL*14
01413                                                                      CL**2
01414      IF AGROUPL GREATER ZERO                                         CL*14
01415          IF CK-GROUPING NOT = AGROUPI                                CL*14
01416              GO TO 0290-READNEXT-ERCMCK.                             CL*14
01417                                                                      CL*14
01418      IF ABYL GREATER ZERO                                            CL*14
01419          IF CK-RECORDED-BY NOT = ABYI                                CL*14
01420              GO TO 0290-READNEXT-ERCMCK.                             CL*14
01421                                                                      CL*14
01422      IF AAMTL GREATER ZERO                                           CL*14
01423          IF CK-AMOUNT-PAID LESS PI-CHECK-AMOUNT                      CL*14
01424              NEXT SENTENCE                                           CL*14
01425            ELSE                                                      CL*14
01426              GO TO 0290-READNEXT-ERCMCK.                             CL*14
01427                                                                      CL*14
01428      IF CK-VOID-DT NOT = LOW-VALUES                                  CL*14
01429          GO TO 0290-READNEXT-ERCMCK.                                 CL*14
01430                                                                      CL**2
01431      IF CK-QUE-CONTROL-NUMBER  EQUAL ZEROS AND                       CL**2
01432         CK-QUE-SEQ-NO          EQUAL ZEROS                           CL**2
01433             NEXT SENTENCE                                            CL**2
01434      ELSE                                                            CL**2
01435          GO TO 0290-READNEXT-ERCMCK.                                 CL*14
01436                                                                      CL**2
01437 *    NOTE *******************************************************    CL**2
01438 *         *                                                     *    CL**2
01439 *         *      THE CHECK MAINT TRAILER HAS MET ALL OF THE     *    CL**2
01440 *         *  QUALIFICATIONS FOR THIS CHECK RELEASE, NOW         *    CL**2
01441 *         *  GENERATE A CHECK QUEUE RECORD.                     *    CL**2
01442 *         *                                                     *    CL**2
01443 *         *******************************************************.   CL**2
01444                                                                      CL**2
01445      IF AOPTIONI  IS EQUAL TO '6'                                    CL*18
01446         IF CK-ACH-PAYMENT IS EQUAL TO 'P'                            CL*18
01447            CONTINUE                                                  CL*18
01448         ELSE                                                         CL*18
01449            GO TO 0290-READNEXT-ERCMCK.                               CL*18
01450                                                                      CL*18
01451      IF AOPTIONI  IS EQUAL TO '5'                                    CL*18
01452         IF CK-ACH-PAYMENT IS EQUAL TO 'P'                            CL*18
01453            GO TO 0290-READNEXT-ERCMCK                                CL*18
01454         ELSE                                                         CL*18
01455            CONTINUE.                                                 CL*18
01456                                                                      CL*18
01457      IF NOT CK-TEXT                                                  CL**8
01458          ADD +1                  TO  WS-RELEASED-COUNT.              CL**8
01459      ADD CK-AMOUNT-PAID          TO  WS-RELEASED-AMOUNT.             CL**6
01460                                                                      CL**2
01461      IF EIBAID NOT = DFHPF1                                          CL*14
01462          GO TO 0290-READNEXT-ERCMCK.                                 CL*14
01463                                                                      CL*14
01464      EXEC CICS ENDBR                                                 CL**2
01465          DATASET (WS-ERCMCK-DSID)                                    CL*14
01466          END-EXEC.                                                   CL**6
01467                                                                      CL**2
01468      MOVE +0  TO  WS-ERCMCK-BROWSE-SW.                               CL**2
01469                                                                      CL**2
01470      EXEC CICS READ UPDATE                                           CL**2
01471          DATASET (WS-ERCMCK-DSID)                                    CL*14
01472          RIDFLD  (WS-COMCK-MAINT-KEY)                                CL**2
01473          SET     (ADDRESS OF COMM-CHECK-RECORDS) END-EXEC.           CL*16
01474                                                                      CL**2
01475      MOVE SPACES                 TO  COMMISSION-CHECK-QUE.           CL**6
01476      MOVE 'MQ'                   TO  MQ-RECORD-ID.                   CL**6
01477      MOVE CK-COMPANY-CD          TO  MQ-COMPANY-CD                   CL**2
01478                                      MQ-COMPANY-CD-A1.               CL**6
01479      MOVE WS-CHECK-QUE-COUNTER   TO  MQ-CONTROL-NUMBER               CL**2
01480                                      CK-QUE-CONTROL-NUMBER           CL**2
01481                                      MQ-CONTROL-NUMBER-A1.           CL**6
01482      MOVE WS-CHECK-COUNTER       TO  MQ-SEQUENCE-NUMBER              CL**2
01483                                      CK-QUE-SEQ-NO                   CL**2
01484                                      MQ-SEQUENCE-NUMBER-A1.          CL**6
01485      ADD +1                      TO  WS-CHECK-COUNTER.               CL**6
01486                                                                      CL*18
01487      IF AOPTIONI  IS EQUAL TO '6'                                    CL*18
01488         MOVE CK-ACH-PAYMENT      TO  MQ-ENTRY-TYPE                   CL*18
01489      ELSE                                                            CL*18
01490         MOVE 'Q'                 TO  MQ-ENTRY-TYPE.                  CL*18
01491                                                                      CL*18
01492      MOVE CK-CSR                 TO  MQ-CHEK-CSR                     CL*10
01493                                      MQ-CSR-A1.                      CL*10
01494      MOVE CK-CARRIER             TO  MQ-CHEK-CARRIER                 CL**2
01495                                      MQ-CARRIER-A1.                  CL**6
01496      MOVE CK-GROUPING            TO  MQ-CHEK-GROUPING                CL**2
01497                                      MQ-GROUPING-A1.                 CL**6
01498      MOVE CK-PAYEE               TO  MQ-CHEK-PAYEE                   CL**2
01499                                      MQ-PAYEE-A1.                    CL**6
01500      MOVE CK-PAYEE-SEQ           TO  MQ-CHEK-PAYEE-SEQ               CL**5
01501                                      MQ-PAYEE-SEQ-A1.                CL**6
01502      MOVE CK-SEQUENCE-NO         TO  MQ-CHEK-SEQ-NO.                 CL**6
01503      MOVE CK-CHECK-NO            TO  MQ-CHECK-NUMBER.                CL**6
01504      MOVE CK-RECORD-TYPE         TO  MQ-RECORD-TYPE.                 CL**8
01505      MOVE CK-PAYEE-INFO          TO  MQ-PAYEE-INFO.                  CL**6
01506      MOVE ZERO                   TO  MQ-TIMES-PRINTED                CL*14
01507                                      MQ-PRINT-AT-HHMM.               CL*14
01508      MOVE CK-AMOUNT-PAID         TO  MQ-CHECK-AMOUNT.                CL**6
01509      MOVE PI-CURRENT-DATE-BIN    TO  MQ-CHECK-RELEASE-DT.            CL**7
01510      MOVE CK-RECORDED-BY         TO  MQ-CHECK-BY-USER.               CL**6
01511      MOVE EIBTIME                TO  MQ-LAST-MAINT-HHMMSS.           CL**6
01512      MOVE PI-CURRENT-DATE-BIN    TO  MQ-LAST-MAINT-DT.               CL**6
01513      MOVE 'E686'                 TO  MQ-LAST-MAINT-BY.               CL**7
01514      MOVE CK-AR-STATEMENT-DT     TO  MQ-AR-STATEMENT-DT.             CL**8
01515      MOVE LOW-VALUES             TO  MQ-CHECK-WRITTEN-DT             CL*14
01516                                      MQ-VOID-DT.                     CL*14
01517      MOVE COMMISSION-CHECK-QUE   TO  JP-RECORD-AREA.                 CL**6
01518      MOVE 'A'                    TO  JP-RECORD-TYPE.                 CL**6
01519      MOVE WS-ERCMKQ-DSID         TO  JP-FILE-ID.                     CL*14
01520      MOVE +1                     TO  WS-NDX.                         CL**2
01521                                                                      CL**2
01522  0290-LOOP.                                                          CL**2
01523                                                                      CL**2
01524      IF WS-NDX GREATER THAN 15                                       CL**2
01525          GO TO 0300-LOOP-EXIT.                                       CL**8
01526                                                                      CL**8
01527      IF CK-TEXT                                                      CL**8
01528          MOVE CK-STUB-TEXT (WS-NDX)  TO   MQ-STUB-TEXT (WS-NDX)      CL**8
01529          ADD  +1                     TO   WS-NDX                     CL**8
01530          GO TO 0290-LOOP.                                            CL**8
01531                                                                      CL**2
01532      IF CK-STUB-LINE (WS-NDX) = SPACES                               CL**2
01533          MOVE SPACES TO MQ-CHECK-STUB-LINE (WS-NDX)                  CL**2
01534          MOVE ZEROS TO MQ-PYAJ-AMT (WS-NDX)                          CL**2
01535                        MQ-PYAJ-SEQ (WS-NDX)                          CL**2
01536          ADD +1     TO WS-NDX                                        CL**2
01537          GO TO 0290-LOOP.                                            CL**2
01538                                                                      CL**2
01539      MOVE CK-STUB-COMMENT (WS-NDX)    TO                             CL**2
01540                     MQ-STUB-COMMENT  (WS-NDX).                       CL**6
01541      MOVE CK-ACCT-AGENT (WS-NDX)      TO                             CL**2
01542                     MQ-ACCT-AGENT  (WS-NDX).                         CL**6
01543      MOVE CK-INVOICE (WS-NDX)    TO   MQ-INVOICE (WS-NDX).           CL**6
01544      MOVE CK-REFERENCE (WS-NDX)  TO   MQ-REFERENCE (WS-NDX).         CL**6
01545      MOVE CK-LEDGER-NO (WS-NDX)  TO   MQ-LEDGER-NO (WS-NDX).         CL**6
01546      MOVE ZEROS                  TO   MQ-PYAJ-AMT (WS-NDX).          CL**8
01547      IF CK-DETAIL-AMT (WS-NDX) NUMERIC                               CL**8
01548          MOVE CK-DETAIL-AMT (WS-NDX)                                 CL**8
01549                                  TO   MQ-PYAJ-AMT (WS-NDX).          CL**8
01550      MOVE CK-PAYMENT-TYPE (WS-NDX)                                   CL**3
01551                                  TO   MQ-PAYMENT-TYPE (WS-NDX).      CL**6
01552      MOVE CK-PAYMENT-TYPE (WS-NDX)                                   CL**4
01553                                  TO   MQ-PYAJ-REC-TYPE (WS-NDX).     CL**6
01554      MOVE CK-LAST-MAINT-APPLIED (WS-NDX)                             CL**8
01555                             TO   MQ-LAST-MAINT-APPLIED (WS-NDX).     CL**8
01556                                                                      CL**8
01557      MOVE CK-NON-AR-ITEM (WS-NDX) TO  MQ-NON-AR-ITEM (WS-NDX).       CL**8
01558                                                                      CL**4
01559      MOVE 'C'                    TO   MQ-PYAJ-REC-TYPE (WS-NDX).     CL*11
01560                                                                      CL**4
01561      MOVE CK-PYAJ-PMT-APPLIED (WS-NDX)                               CL*11
01562                                  TO   MQ-PYAJ-PMT-APPLIED (WS-NDX)   CL*11
01563      MOVE WS-SEQ-NO              TO   MQ-PYAJ-SEQ (WS-NDX).          CL**3
01564      ADD  +1                     TO   WS-SEQ-NO                      CL**3
01565                                       WS-NDX.                        CL**3
01566                                                                      CL**2
01567      GO TO 0290-LOOP.                                                CL**2
01568                                                                      CL**2
01569  0300-LOOP-EXIT.                                                     CL**8
01570                                                                      CL**2
01571      EXEC CICS WRITE                                                 CL**2
01572          DATASET (WS-ERCMKQ-DSID)                                    CL*14
01573          FROM    (COMMISSION-CHECK-QUE)                              CL**2
01574          RIDFLD  (MQ-CONTROL-PRIMARY) END-EXEC.                      CL**6
01575                                                                      CL**2
01576      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**6
01577                                                                      CL**2
01578      MOVE COMM-CHECK-RECORDS        TO  JP-RECORD-AREA.              CL**6
01579      MOVE 'C'                       TO  JP-RECORD-TYPE.              CL**6
01580      MOVE WS-ERCMCK-DSID            TO  JP-FILE-ID.                  CL*14
01581                                                                      CL**2
01582      EXEC CICS REWRITE                                               CL**2
01583          DATASET (WS-ERCMCK-DSID)                                    CL*14
01584          FROM    (COMM-CHECK-RECORDS) END-EXEC.                      CL**6
01585                                                                      CL**2
01586      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**6
01587                                                                      CL**2
01588      GO TO 0285-STARTBR-ERCMCK.                                      CL*14
01589                                                                      CL**2
01590      EJECT                                                           CL**2
01591  0400-END-OF-SEARCH.                                                 CL*14
01592                                                                   EL686
01593      IF WS-ERPYAJ-BROWSE-STARTED                                  EL686
01594          EXEC CICS ENDBR                                          EL686
01595              DATASET (WS-ERPYAJ-DSID)                                CL*14
01596              END-EXEC.                                            EL686
01597                                                                   EL686
01598      IF WS-ERCHEK-BROWSE-STARTED                                  EL686
01599          EXEC CICS ENDBR                                          EL686
01600              DATASET (WS-ERCHEK-DSID)                                CL*14
01601              END-EXEC.                                               CL**2
01602                                                                      CL**2
01603      IF WS-ERCMCK-BROWSE-STARTED                                     CL**2
01604          EXEC CICS ENDBR                                             CL**2
01605              DATASET (WS-ERCMCK-DSID)                                CL*14
01606              END-EXEC.                                            EL686
01607                                                                   EL686
01608      MOVE WS-CHECK-QUE-COUNTER   TO  WS-TL1-CONTROL-GROUP.           CL**6
01609      MOVE WS-RELEASED-COUNT      TO  WS-TL1-COUNT.                   CL**6
01610      MOVE WS-RELEASED-AMOUNT     TO  WS-TL1-AMOUNT.                  CL**6
01611                                                                      CL*14
01612      IF WS-RELEASED-COUNT GREATER 1                                  CL*14
01613          MOVE 'S'                TO  WS-TL1-PLURAL                   CL*14
01614       ELSE                                                           CL*14
01615          MOVE ' '                TO  WS-TL1-PLURAL.                  CL*14
01616                                                                      CL*14
01617      IF EIBAID = DFHPF1                                              CL*14
01618          MOVE LOW-VALUES         TO EL686AI                          CL*14
01619          MOVE 'RELEASED'         TO WS-TL1-RELEASE                   CL*14
01620        ELSE                                                          CL*14
01621         IF WS-RELEASED-COUNT GREATER ZERO                            CL*14
01622             MOVE 'TO BE RELEASED'   TO WS-TL1-RELEASE                CL*14
01623           ELSE                                                       CL*14
01624             MOVE +0                 TO PI-PROC-SW                    CL*14
01625             MOVE -1                 TO AOPTIONL                      CL*14
01626             MOVE ER-3048            TO EMI-ERROR                     CL*14
01627             PERFORM 8200-SEND-DATAONLY.                              CL*14
01628                                                                      CL*14
01629      MOVE WS-TOTAL-LINE1         TO  EMI-MESSAGE-AREA (1).           CL**6
01630      MOVE WS-TOTAL-LINE2         TO  EMI-MESSAGE-AREA (2).           CL**6
01631                                                                   EL686
01632      IF EIBAID = DFHPF1                                              CL*14
01633          PERFORM 7000-PRINT-CHECKS-WAITING                           CL*14
01634          GO TO 0100-INITIALIZE                                       CL*14
01635        ELSE                                                          CL*14
01636          MOVE +1                 TO PI-PROC-SW                       CL*14
01637          MOVE -1                 TO AOPTIONL                         CL*14
01638          MOVE AL-UNBON           TO AOPTIONA                         CL*14
01639                                     AAMTA                            CL*14
01640          MOVE AL-UABON  TO ABYA ACARRA AGROUPA ASTATA AACCTA         CL*14
01641                            CERTO01A EFFDT01A CERTO02A EFFDT02A       CL*14
01642                            CERTO03A EFFDT03A CERTO04A EFFDT04A       CL*14
01643                            CERTO05A EFFDT05A CERTO06A EFFDT06A       CL*14
01644                            CERTO07A EFFDT07A CERTO08A EFFDT08A       CL*14
01645                            CERTO09A EFFDT09A CERTO10A EFFDT10A       CL*14
01646                            CERTO11A EFFDT11A CERTO12A EFFDT12A       CL*14
01647                            CERTO13A EFFDT13A CERTO14A EFFDT14A       CL*14
01648                            CERTO15A EFFDT15A                         CL*14
01649          PERFORM 8200-SEND-DATAONLY.                                 CL*14
01650                                                                   EL686
01651      EJECT                                                        EL686
01652  0910-ENQ-BUSY.                                                   EL686
01653                                                                   EL686
01654 *    NOTE ******************************************************* EL686
01655 *         *                                                     * EL686
01656 *         *      IF ONE OF THE OTHER PROGRAMS (EL176 OR EL177)  *    CL*14
01657 *         *  HAS EXCLUSIVE CONTROL OF THE CHECK QUEUE DSID,     *    CL*14
01658 *         *  SEND A MESSAGE TO THE OPERATOR TO WAIT A FEW       *    CL*14
01659 *         *  MOMENTS AND TRY AGAIN.                             * EL686
01660 *         *                                                     * EL686
01661 *         *******************************************************.EL686
01662                                                                   EL686
01663      MOVE +395                   TO  EMI-ERROR.                      CL**6
01664      MOVE -1                     TO  AOPTIONL.                       CL**6
01665      PERFORM 8200-SEND-DATAONLY.                                  EL686
01666                                                                   EL686
01667      EJECT                                                        EL686
01668  7000-PRINT-CHECKS-WAITING SECTION.                               EL686
01669                                                                   EL686
01670      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.            CL**6
01671                                                                   EL686
01672      MOVE PI-COMPANY-ID          TO WS-CFK-COMPANY-ID.               CL**6
01673      MOVE '1'                    TO WS-CFK-RECORD-TYPE.              CL**6
01674      MOVE ZEROS                  TO WS-CFK-SEQUENCE-NO.              CL**6
01675                                                                   EL686
01676      EXEC CICS READ                                               EL686
01677           DATASET   (WS-ELCNTL-DSID)                                 CL*14
01678           SET       (ADDRESS OF CONTROL-FILE)                        CL*16
01679           RIDFLD    (WS-CONTROL-FILE-KEY) END-EXEC.                  CL**6
01680                                                                   EL686
030612     IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'AHL'                      CL*17
CIDMOD         MOVE CF-FORMS-PRINTER-ID  TO  PI-PROCESSOR-PRINTER            000
01682 *        MOVE EIBTRMID       TO CF-FORMS-PRINTER-ID                  CL*17
01683          EXEC CICS START                                             CL*17
01684               TRANSID    (WS-PRINT-TRANS-ID)                         CL*17
01685               FROM       (PROGRAM-INTERFACE-BLOCK)                   CL*17
01686               LENGTH     (PI-COMM-LENGTH)                            CL*17
01687 *             TERMID     (CF-FORMS-PRINTER-ID)                       CL*17
01688          END-EXEC                                                    CL*17
01689      ELSE                                                            CL*17
01690          EXEC CICS START                                             CL*17
01691               TRANSID    (WS-PRINT-TRANS-ID)                         CL*17
01692               FROM       (PROGRAM-INTERFACE-BLOCK)                   CL*17
01693               LENGTH     (PI-COMM-LENGTH)                            CL*17
01694               TERMID     (CF-FORMS-PRINTER-ID)                       CL*17
01695          END-EXEC.                                                   CL*17
01696                                                                   EL686
01697      GO TO 7090-EXIT.                                             EL686
01698                                                                   EL686
01699  7010-TERMID-ERROR.                                               EL686
01700                                                                   EL686
01701      MOVE 0412                   TO EMI-ERROR.                       CL**6
01702      MOVE -1                     TO APFKL.                           CL**6
01703      PERFORM 8200-SEND-DATAONLY.                                  EL686
01704                                                                   EL686
01705  7020-TRANS-ERROR.                                                EL686
01706                                                                   EL686
01707      MOVE 0413                   TO EMI-ERROR.                       CL**6
01708      MOVE -1                     TO APFKL.                           CL**6
01709      PERFORM 8200-SEND-DATAONLY.                                  EL686
01710                                                                   EL686
01711  7090-EXIT.                                                       EL686
01712      EXIT.                                                        EL686
01713                                                                   EL686
01714      EJECT                                                        EL686
01715  8100-SEND-INITIAL-MAP SECTION.                                   EL686
01716                                                                   EL686
01717      MOVE EIBTIME                TO  WS-TIME-WORK.                EL686
01718                                                                   EL686
01719      MOVE PI-CURRENT-DATE        TO  ADATEO.                         CL**6
01720      MOVE WS-TIME                TO  ATIMEO.                         CL**6
01721      MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O.                         CL**6
01722      MOVE EMI-MESSAGE-AREA (2)   TO AEMSG2O.                         CL**6
01723                                                                   EL686
01724      EXEC CICS SEND                                               EL686
01725          FROM   (EL686AI)                                         EL686
01726          MAPSET (WS-MAPSET-NAME)                                  EL686
01727          MAP    (WS-MAP-NAME)                                     EL686
01728          CURSOR ERASE END-EXEC.                                   EL686
01729                                                                   EL686
01730      PERFORM 9100-RETURN-TRAN.                                    EL686
01731                                                                   EL686
01732  8100-EXIT.                                                       EL686
01733      EXIT.                                                        EL686
01734                                                                   EL686
01735      EJECT                                                        EL686
01736  8200-SEND-DATAONLY SECTION.                                      EL686
01737                                                                   EL686
01738      IF EMI-ERROR = ZERO                                             CL*14
01739          MOVE AL-PABOF           TO APF1A                            CL*14
01740          MOVE AL-PADOF           TO ACOMPA                           CL*14
01741       ELSE                                                           CL*14
01742          PERFORM 9900-ERROR-FORMAT                                   CL*14
01743          MOVE AL-PADOF           TO APF1A                            CL*14
01744          MOVE AL-PABOF           TO ACOMPA.                          CL*14
01745                                                                   EL686
01746      MOVE EIBTIME                TO  WS-TIME-WORK.                EL686
01747                                                                   EL686
01748      MOVE PI-CURRENT-DATE        TO ADATEO.                          CL*14
01749      MOVE WS-TIME                TO ATIMEO.                          CL*14
01750      MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O.                         CL*14
01751      MOVE EMI-MESSAGE-AREA (2)   TO AEMSG2O.                         CL*14
01752                                                                   EL686
01753      EXEC CICS SEND DATAONLY                                      EL686
01754          FROM   (EL686AI)                                         EL686
01755          MAPSET (WS-MAPSET-NAME)                                  EL686
01756          MAP    (WS-MAP-NAME)                                     EL686
01757          CURSOR END-EXEC.                                         EL686
01758                                                                   EL686
01759      PERFORM 9100-RETURN-TRAN.                                    EL686
01760                                                                   EL686
01761  8200-EXIT.                                                       EL686
01762      EXIT.                                                        EL686
01763                                                                   EL686
01764      EJECT                                                        EL686
01765  8300-SEND-TEXT SECTION.                                          EL686
01766                                                                   EL686
01767      EXEC CICS SEND TEXT                                          EL686
01768          FROM   (LOGOFF-TEXT)                                     EL686
01769          LENGTH (LOGOFF-LENGTH)                                   EL686
01770          ERASE  FREEKB END-EXEC.                                  EL686
01771                                                                   EL686
01772      EXEC CICS RETURN                                             EL686
01773          END-EXEC.                                                EL686
01774                                                                   EL686
01775  8300-EXIT.                                                       EL686
01776      EXIT.                                                        EL686
01777                                                                   EL686
01778      EJECT                                                        EL686
01779  8400-LOG-JOURNAL-RECORD SECTION.                                 EL686
01780                                                                   EL686
01781      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                     CL**6
01782      MOVE THIS-PGM               TO  JP-PROGRAM-ID.                  CL*14
01783                                                                   EL686
pemuni*    EXEC CICS JOURNAL                                            EL686
pemuni*        JFILEID (PI-JOURNAL-FILE-ID)                             EL686
pemuni*        JTYPEID (WS-JOURNAL-TYPE-ID)                             EL686
pemuni*        FROM    (JOURNAL-RECORD)                                 EL686
pemuni*        LENGTH  (WS-JOURNAL-RECORD-LENGTH) END-EXEC.             EL686
01789                                                                   EL686
01790  8400-EXIT.                                                       EL686
01791      EXIT.                                                        EL686
01792                                                                   EL686
01793      EJECT                                                        EL686
01794  8500-DATE-CONVERSION SECTION.                                    EL686
01795                                                                   EL686
01796      EXEC CICS LINK                                               EL686
01797          PROGRAM  (ELDATCV)                                       EL686
01798          COMMAREA (DATE-CONVERSION-DATA)                          EL686
01799          LENGTH   (DC-COMM-LENGTH) END-EXEC.                      EL686
01800                                                                   EL686
01801  8500-EXIT.                                                       EL686
01802      EXIT.                                                           CL*14
01803                                                                   EL686
01804  8600-DEEDIT  SECTION.                                               CL*14
01805      EXEC CICS  BIF DEEDIT                                           CL*14
01806          FIELD   (DEEDIT-FIELD)                                      CL*14
01807          LENGTH  (15)                                                CL*14
01808      END-EXEC.                                                       CL*14
01809                                                                      CL*14
01810  8600-EXIT.                                                          CL*14
01811      EXIT.                                                        EL686
01812                                                                   EL686
01813      EJECT                                                        EL686
01814  9000-RETURN-CICS SECTION.                                        EL686
01815                                                                   EL686
01816      MOVE EL005                  TO  THIS-PGM.                       CL*14
01817      MOVE EIBAID                 TO  PI-ENTRY-CD-1.                  CL**6
01818      PERFORM 9300-XCTL.                                           EL686
01819                                                                   EL686
01820  9000-EXIT.                                                       EL686
01821      EXIT.                                                        EL686
01822                                                                   EL686
01823  9100-RETURN-TRAN SECTION.                                        EL686
01824                                                                   EL686
01825      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.               CL*14
01826      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.           CL**6
01827                                                                   EL686
01828      EXEC CICS RETURN                                             EL686
01829          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL686
01830          LENGTH   (PI-COMM-LENGTH)                                EL686
01831          TRANSID  (WS-TRANS-ID) END-EXEC.                         EL686
01832                                                                   EL686
01833  9100-EXIT.                                                       EL686
01834      EXIT.                                                        EL686
01835                                                                   EL686
01836  9300-XCTL SECTION.                                               EL686
01837                                                                   EL686
01838      MOVE DFHENTER               TO  EIBAID.                         CL**6
01839                                                                   EL686
01840      EXEC CICS XCTL                                               EL686
01841          PROGRAM  (THIS-PGM)                                         CL*14
01842          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL686
01843          LENGTH   (PI-COMM-LENGTH) END-EXEC.                      EL686
01844                                                                   EL686
01845  9300-EXIT.                                                       EL686
01846      EXIT.                                                        EL686
01847                                                                   EL686
01848      EJECT                                                        EL686
01849  9400-CLEAR SECTION.                                              EL686
01850                                                                   EL686
01851      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                        CL*14
01852      PERFORM 9300-XCTL.                                           EL686
01853                                                                   EL686
01854  9400-EXIT.                                                       EL686
01855      EXIT.                                                        EL686
01856                                                                   EL686
01857  9600-PGMIDERR SECTION.                                           EL686
01858                                                                   EL686
01859      EXEC CICS HANDLE CONDITION                                   EL686
01860          PGMIDERR (8300-SEND-TEXT) END-EXEC.                      EL686
01861                                                                   EL686
01862      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM.             CL*14
01863                                                                      CL**6
01864      MOVE EL005                  TO  THIS-PGM                        CL*14
01865                                      LOGOFF-PGM.                     CL**6
01866      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                    CL**6
01867                                                                   EL686
01868      MOVE SPACES                 TO  PI-ENTRY-CD-1.                  CL**6
01869      PERFORM 9300-XCTL.                                           EL686
01870                                                                   EL686
01871  9600-EXIT.                                                       EL686
01872      EXIT.                                                        EL686
01873                                                                   EL686
01874      EJECT                                                        EL686
01875  9900-ERROR-FORMAT SECTION.                                       EL686
01876                                                                   EL686
01877      EXEC CICS LINK                                               EL686
01878          PROGRAM  (EL001)                                         EL686
01879          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL686
01880          LENGTH   (EMI-COMM-LENGTH) END-EXEC.                     EL686
01881                                                                   EL686
01882  9900-EXIT.                                                       EL686
01883      EXIT.                                                        EL686
01884                                                                   EL686
01885      EJECT                                                        EL686
01886  9990-ERROR SECTION.                                              EL686
01887                                                                   EL686
01888      MOVE DFHEIBLK TO EMI-LINE1.                                  EL686
01889      EXEC CICS LINK                                               EL686
01890          PROGRAM  (EL004)                                         EL686
01891          COMMAREA (EMI-LINE1)                                     EL686
01892          LENGTH   (72) END-EXEC.                                  EL686
01893      MOVE -1 TO APFKL.                                            EL686
01894      PERFORM 8200-SEND-DATAONLY.                                  EL686
01895      GO TO 9100-RETURN-TRAN.                                      EL686
01896                                                                   EL686
01897  9990-EXIT.                                                       EL686
01898      EXIT.                                                        EL686
01899                                                                      CL*10
01900  9995-SECURITY-VIOLATION.                                            CL*10
01901      COPY ELCSCTP.                                                   CL*14
01902                                                                      CL*14
01903  9995-EXIT.                                                          CL*10
01904      EXIT.                                                           CL*10
01905                                                                      CL*10
01906  9999-LAST-PARAGRAPH SECTION.                                     EL686
01907                                                                   EL686
01908      GOBACK.                                                      EL686
01909                                                                   EL686
