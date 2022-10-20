00001  IDENTIFICATION DIVISION.                                         08/12/97
00002                                                                   EL850
00003  PROGRAM-ID.                 EL850 .                                 LV018
00004 *              PROGRAM CONVERTED BY                                  CL*15
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*15
00006 *              CONVERSION DATE 02/03/95 09:37:46.                    CL*15
00007 *                            VMOD=2.018.                             CL*18
00008 *                                                                 EL850
00008 *                                                                 EL850
00009 *AUTHOR.     LOGIC,INC.                                              CL*15
00010 *            DALLAS, TEXAS.                                          CL*15
00011                                                                   EL850
00012 *DATE-COMPILED.                                                      CL*15
00013 *SECURITY.   *****************************************************   CL*15
00014 *            *                                                   *   CL*15
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*15
00016 *            *                                                   *   CL*15
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*15
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*15
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*15
00020 *            *                                                   *   CL*15
00021 *            *****************************************************   CL*15
00022                                                                   EL850
00023 *REMARKS.    TRANSACTION - EXJ1 - ACCOUNTS RECEIVABLE MASTER MENU    CL*15
00024                                                                   EL850
00025                                                                   EL850
00026      EJECT                                                        EL850
00027  ENVIRONMENT DIVISION.                                            EL850
00028  DATA DIVISION.                                                   EL850
00029  WORKING-STORAGE SECTION.                                         EL850
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL850
00031  77  FILLER  PIC X(32)  VALUE '*    EL850 WORKING STORAGE     *'. EL850
00032  77  FILLER  PIC X(32)  VALUE '************VMOD=2.018 *********'.    CL*18
00033                                                                   EL850
00034                              COPY ELCSCTM.                           CL*12
00035                              COPY ELCSCRT4.                          CL*12
00036                              COPY MPCSCRT.                           CL*12
00037                                                                   EL850
00038     EJECT                                                         EL850
00039                                                                   EL850
00040  01  MISC-WORK-AREAS.                                             EL850
00041      12  W-APPL-SCRTY-NDX    PIC S9(4) COMP VALUE +37.               CL**7
00042      12  MAP-NAMEA           PIC X(8)    VALUE 'EL850A'.          EL850
00043      12  MAPSET-NAME         PIC X(8)    VALUE 'EL850S'.          EL850
00044      12  TRANS-ID            PIC X(4)    VALUE 'EXJ1'.            EL850
00045      12  PGM-NAME            PIC X(8).                            EL850
00046      12  PASS-AREA-LEN       PIC S9(4)   COMP VALUE +16.          EL850
00047      12  TIME-IN             PIC 9(7).                            EL850
00048      12  TIME-OUT-R    REDEFINES TIME-IN.                         EL850
00049          16  FILLER          PIC X.                               EL850
00050          16  TIME-OUT        PIC 99V99.                           EL850
00051          16  FILLER          PIC X(2).                            EL850
00052      12  THIS-PGM            PIC X(8)    VALUE 'EL850'.           EL850
00053      12  LINK-001            PIC X(8)    VALUE 'EL001'.           EL850
00054      12  LINK-004            PIC X(8)    VALUE 'EL004'.           EL850
00055      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL850
00056      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL850
00057      12  XCTL-626            PIC X(8)    VALUE 'EL626'.           EL850
00058      12  XCTL-636            PIC X(8)    VALUE 'EL636'.              CL**5
00059      12  XCTL-637            PIC X(8)    VALUE 'EL637'.              CL**5
00060      12  XCTL-685            PIC X(8)    VALUE 'EL685'.              CL**8
00061      12  XCTL-686            PIC X(8)    VALUE 'EL686'.              CL**3
00062      12  XCTL-687            PIC X(8)    VALUE 'EL687'.              CL**3
00063      12  XCTL-696            PIC X(8)    VALUE 'EL696'.              CL*15
00064      12  XCTL-697            PIC X(8)    VALUE 'EL697'.              CL*10
00065      12  XCTL-852            PIC X(8)    VALUE 'EL852'.           EL850
00066      12  XCTL-856            PIC X(8)    VALUE 'EL856'.           EL850
00067      12  XCTL-EL880          PIC X(8)    VALUE 'EL880'.              CL**6
00068      12  XCTL-EM880          PIC X(8)    VALUE 'EM880'.              CL**6
00069      12  XCTL-850            PIC X(8)    VALUE 'EL850'.           EL850
00070      12  XCTL-882            PIC X(8)    VALUE 'EL882'.           EL850
00071      12  LINK-CLDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL850
00072      12  SV-TODAY            PIC XX.                              EL850
00073      12  WS-CURRENT-DATE     PIC X(8).                            EL850
00074      12  WS-SYSID            PIC X(4).                            EL850
00075      12  QID.                                                     EL850
00076          16  QID-TERM            PIC X(4)      VALUE SPACES.      EL850
00077          16  FILLER              PIC X(4)      VALUE '125D'.      EL850
00078      12  QID-ITEM                PIC S9(4)     VALUE +1  COMP.       CL*10
00079                                                                   EL850
00080      EJECT                                                        EL850
00081      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL850
00082      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL850
00083      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL850
00084      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL850
00085      12  ER-1999                 PIC X(4)  VALUE '1999'.             CL*11
00086      12  ER-9097                 PIC X(4)  VALUE '9097'.             CL**7
00087                                                                   EL850
00088      EJECT                                                        EL850
00089                                                                   EL850
00090  01  BATCH-TO-PROCESS.                                            EL850
00091      05  EDIT-COMPANY-CD         PIC X.                           EL850
00092      05  EDIT-BATCH              PIC X(6).                        EL850
00093      05  EDIT-COMPANY-ID         PIC XXX.                         EL850
00094      05  EDIT-RESTART-BATCH      PIC X(6).                        EL850
00095                                                                   EL850
00096  01  MISC-COMP.                                                   EL850
00097      12  WS-IC               PIC S9(4)   VALUE -1  COMP.          EL850
00098      12  SUB1                PIC S9(4)   VALUE +0  COMP.          EL850
00099      12  SUB2                PIC S9(4)   VALUE +0  COMP.          EL850
00100                                                                   EL850
00101  01  ACCESS-KEYS.                                                 EL850
00102      12  ELCNTL-KEY.                                              EL850
00103          16  CK-COMP-ID      PIC X(3).                            EL850
00104          16  FILLER          PIC X       VALUE 'R'.               EL850
00105          16  CK-PROC-ID      PIC X(4).                            EL850
00106          16  FILLER          PIC S9(4)   VALUE +0  COMP.          EL850
00107                                                                   EL850
00108      EJECT                                                        EL850
00109                              COPY ELCDATE.                           CL*12
00110                                                                   EL850
00111      EJECT                                                        EL850
00112                              COPY ELCLOGOF.                          CL*12
00113                                                                   EL850
00114      EJECT                                                        EL850
00115                              COPY ELCATTR.                           CL*12
00116                                                                   EL850
00117      EJECT                                                        EL850
00118                              COPY ELCEMIB.                           CL*12
00119                                                                   EL850
00120      EJECT                                                        EL850
00121                              COPY ELCINTF.                           CL*12
00122                                                                   EL850
00123      12  FILLER                      REDEFINES                       CL**8
00124          PI-PROGRAM-WORK-AREA.                                       CL**8
00125          16  FILLER                  PIC X(45).                      CL**8
00126          16  PI-AR-MODE              PIC X.                          CL**8
00127          16  FILLER                  PIC X(594).                     CL*16
00128      EJECT                                                        EL850
00129                                                                   EL850
00130                              COPY ELCAID.                            CL*12
00131                                                                   EL850
00132  01  FILLER    REDEFINES DFHAID.                                  EL850
00133      12  FILLER              PIC X(8).                            EL850
00134      12  PF-VALUES           PIC X       OCCURS 2.                EL850
00135                                                                   EL850
00136      EJECT                                                        EL850
00137                                                                   EL850
00138                              COPY EL850S.                            CL*12
00139                                                                   EL850
00140      EJECT                                                        EL850
00141  LINKAGE SECTION.                                                 EL850
00142  01  DFHCOMMAREA             PIC X(1024).                         EL850
00143                                                                   EL850
00144                                                                   EL850
00145      EJECT                                                        EL850
00146  PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.                   EL850
00147                                                                   EL850
00148                                                                   EL850
00149      MOVE DFHCOMMAREA         TO PROGRAM-INTERFACE-BLOCK.         EL850
00150                                                                   EL850
00151      IF EIBCALEN = 0                                              EL850
00152         GO TO 8800-UNAUTHORIZED-ACCESS.                           EL850
00153                                                                   EL850
00154      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL850
00155      MOVE '5'                    TO DC-OPTION-CODE.               EL850
00156      MOVE LINK-CLDATCV           TO PGM-NAME.                     EL850
00157                                                                   EL850
00158      EXEC CICS LINK                                               EL850
00159          PROGRAM  (PGM-NAME)                                      EL850
00160          COMMAREA (DATE-CONVERSION-DATA)                          EL850
00161          LENGTH   (DC-COMM-LENGTH)                                EL850
00162      END-EXEC.                                                    EL850
00163                                                                   EL850
00164      MOVE EIBTRMID               TO QID-TERM.                     EL850
00165      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DATE.              EL850
00166      MOVE DC-BIN-DATE-1          TO SV-TODAY.                     EL850
00167                                                                   EL850
00168      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL850
00169          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL850
00170              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL850
00171              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL850
00172              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL850
00173              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL850
00174              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL850
00175              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL850
00176              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL850
00177              MOVE THIS-PGM TO PI-CALLING-PROGRAM                  EL850
00178              PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT         CL**7
00179          ELSE                                                     EL850
00180              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL850
00181              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL850
00182              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL850
00183              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL850
00184              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL850
00185              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL850
00186              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL850
00187              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL850
00188                                                                   EL850
00189      EXEC CICS HANDLE CONDITION                                   EL850
00190          NOTFND   (8830-NOT-FOUND)                                EL850
00191          PGMIDERR (9600-PGMID-ERROR)                              EL850
00192          ERROR    (9990-ABEND)                                    EL850
00193      END-EXEC.                                                    EL850
00194                                                                   EL850
00195      IF (EIBTRNID NOT = TRANS-ID) OR (EIBCALEN = 0)               EL850
00196          GO TO 8100-SEND-INITIAL-MAP.                             EL850
00197                                                                   EL850
00198      IF  EIBAID = DFHCLEAR                                           CL**7
00199              OR                                                      CL**7
00200          NOT DISPLAY-CAP                                             CL**7
00201          GO TO 9400-CLEAR.                                        EL850
00202                                                                   EL850
00203      EJECT                                                        EL850
00204                                                                   EL850
00205  0200-RECEIVE.                                                    EL850
00206      MOVE LOW-VALUES             TO EL850AI.                      EL850
00207                                                                   EL850
00208      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL850
00209          MOVE ER-0008            TO EMI-ERROR                     EL850
00210          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL850
00211          MOVE -1                 TO SELECTL                       EL850
00212          GO TO 8200-SEND-DATAONLY.                                EL850
00213                                                                   EL850
00214      EXEC CICS RECEIVE                                            EL850
00215          MAP    (MAP-NAMEA)                                       EL850
00216          MAPSET (MAPSET-NAME)                                     EL850
00217          INTO   (EL850AI)                                         EL850
00218      END-EXEC.                                                    EL850
00219                                                                   EL850
00220      IF PFENTERL = ZERO                                           EL850
00221          GO TO 250-CHECK-PFKEYS.                                  EL850
00222                                                                   EL850
00223      IF EIBAID NOT = DFHENTER                                     EL850
00224          MOVE ER-0004           TO EMI-ERROR                         CL**9
00225          GO TO 0320-INPUT-ERROR.                                     CL**9
00226                                                                      CL**9
00227      IF (PFENTERI IS NUMERIC)                                        CL**9
00228        AND (PFENTERI IS GREATER THAN ZERO                            CL**9
00229        AND  IS LESS THAN 25)                                         CL**9
00230          MOVE PF-VALUES (PFENTERI) TO  EIBAID                        CL**9
00231      ELSE                                                            CL**9
00232          MOVE ER-0029            TO EMI-ERROR                        CL**9
00233          GO TO 0320-INPUT-ERROR.                                  EL850
00234                                                                   EL850
00235      EJECT                                                        EL850
00236                                                                   EL850
00237  250-CHECK-PFKEYS.                                                EL850
00238                                                                   EL850
00239      IF EIBAID = DFHPF23                                          EL850
00240          GO TO 8810-PF23.                                         EL850
00241                                                                   EL850
00242      IF EIBAID = DFHPF24                                          EL850
00243          GO TO 9200-RETURN-MAIN-MENU.                             EL850
00244                                                                   EL850
00245      IF EIBAID = DFHPF12                                          EL850
00246          GO TO 9500-PF12.                                         EL850
00247                                                                   EL850
00248      EJECT                                                        EL850
00249                                                                   EL850
00250  310-CHECK-SELECTION.                                             EL850
00251      IF SELECTI = '01'                                            EL850
00252          MOVE XCTL-852 TO PGM-NAME                                EL850
00253          GO TO 9300-XCTL.                                         EL850
00254                                                                   EL850
00255      IF SELECTI = '02'                                            EL850
00256          MOVE XCTL-856           TO PGM-NAME                      EL850
00257          GO TO 9300-XCTL.                                         EL850
00258                                                                   EL850
00259      IF SELECTI = '03'                                               CL**4
00260         IF MORTGAGE-SESSION                                          CL**6
00261             MOVE XCTL-EM880      TO PGM-NAME                         CL**6
00262             GO TO 9300-XCTL                                          CL**6
00263         ELSE                                                         CL**6
00264             MOVE XCTL-EL880      TO PGM-NAME                         CL**6
00265             GO TO 9300-XCTL.                                         CL**6
00266                                                                   EL850
00267      IF SELECTI = '04'                                               CL**3
00268          MOVE XCTL-636           TO PGM-NAME                         CL**3
00269          GO TO 9300-XCTL.                                            CL**3
00270                                                                      CL**3
00271      IF SELECTI = '05'                                               CL**3
00272          MOVE XCTL-686           TO PGM-NAME                         CL**3
00273          GO TO 9300-XCTL.                                            CL**3
00274                                                                      CL**3
00275      IF SELECTI = '06'                                               CL**3
00276          IF PI-COMPANY-ID = 'LGX' OR 'HER' OR 'CVL' OR 'CNL'         CL*18
00277              MOVE XCTL-697           TO PGM-NAME                     CL*11
00278              GO TO 9300-XCTL                                         CL*11
00279          ELSE                                                        CL*11
00280              IF PI-COMPANY-ID = 'NSL'                                CL*15
00281                  MOVE XCTL-696       TO PGM-NAME                     CL*15
00282                  GO TO 9300-XCTL                                     CL*15
00283              ELSE                                                    CL*15
00284                  MOVE ER-1999        TO EMI-ERROR                    CL*15
00285                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*15
00286                  MOVE AL-UNBON       TO SELECTA                      CL*15
00287                  MOVE -1             TO SELECTL                      CL*15
00288                  GO TO 8200-SEND-DATAONLY.                           CL*15
00289                                                                      CL**5
00290      IF SELECTI = '07'                                               CL**5
00291          MOVE XCTL-637           TO PGM-NAME                         CL**5
00292          GO TO 9300-XCTL.                                            CL**8
00293                                                                      CL**8
00294      IF SELECTI = '08'                                               CL**8
00295          MOVE 'A'                TO PI-AR-MODE                       CL**8
00296          MOVE XCTL-685           TO PGM-NAME                         CL**8
00297          GO TO 9300-XCTL.                                            CL**3
00298                                                                   EL850
00299      MOVE ER-0029                TO EMI-ERROR.                    EL850
00300                                                                   EL850
00301  0320-INPUT-ERROR.                                                EL850
00302      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL850
00303      MOVE AL-UNBON               TO SELECTA.                      EL850
00304      MOVE -1                     TO SELECTL.                      EL850
00305      GO TO 8200-SEND-DATAONLY.                                    EL850
00306                                                                   EL850
00307      EJECT                                                        EL850
00308                                                                   EL850
00309  8100-SEND-INITIAL-MAP.                                           EL850
00310      MOVE LOW-VALUES             TO EL850AO.                      EL850
00311      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL850
00312                                                                   EL850
00313      MOVE EIBTIME                TO TIME-IN.                      EL850
00314      MOVE TIME-OUT               TO TIMEO.                        EL850
00315      MOVE WS-CURRENT-DATE        TO DATEO.                        EL850
00316      MOVE -1                     TO SELECTL.                      EL850
00317                                                                   EL850
00318      EXEC CICS SEND                                               EL850
00319          MAP    (MAP-NAMEA)                                       EL850
00320          MAPSET (MAPSET-NAME)                                     EL850
00321          FROM   (EL850AO)                                         EL850
00322          ERASE                                                    EL850
00323          CURSOR                                                   EL850
00324      END-EXEC.                                                    EL850
00325                                                                   EL850
00326      MOVE 'EL850'                TO PI-CALLING-PROGRAM.           EL850
00327                                                                   EL850
00328      GO TO 9100-RETURN-TRAN.                                      EL850
00329                                                                   EL850
00330  8200-SEND-DATAONLY.                                              EL850
00331                                                                   EL850
00332      MOVE EIBTIME                TO TIME-IN.                      EL850
00333      MOVE TIME-OUT               TO TIMEO.                        EL850
00334      MOVE WS-CURRENT-DATE        TO DATEO.                        EL850
00335      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL850
00336                                                                   EL850
00337      EXEC CICS SEND                                               EL850
00338          MAP    (MAP-NAMEA)                                       EL850
00339          MAPSET (MAPSET-NAME)                                     EL850
00340          FROM   (EL850AO)                                         EL850
00341          DATAONLY                                                 EL850
00342          CURSOR                                                   EL850
00343      END-EXEC.                                                    EL850
00344                                                                   EL850
00345      GO TO 9100-RETURN-TRAN.                                      EL850
00346                                                                   EL850
00347  8300-SEND-TEXT.                                                  EL850
00348      EXEC CICS SEND TEXT                                          EL850
00349          FROM   (LOGOFF-TEXT)                                     EL850
00350          LENGTH (LOGOFF-LENGTH)                                   EL850
00351          ERASE                                                    EL850
00352          FREEKB                                                   EL850
00353      END-EXEC.                                                    EL850
00354                                                                   EL850
00355      EXEC CICS RETURN                                             EL850
00356      END-EXEC.                                                    EL850
00357                                                                   EL850
00358  8800-UNAUTHORIZED-ACCESS.                                        EL850
00359      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL850
00360      GO TO 8300-SEND-TEXT.                                        EL850
00361                                                                   EL850
00362  8810-PF23.                                                       EL850
00363      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL850
00364      MOVE XCTL-005               TO PGM-NAME.                     EL850
00365      GO TO 9300-XCTL.                                             EL850
00366                                                                   EL850
00367  8830-NOT-FOUND.                                                  EL850
00368      MOVE 'USER RECORD NOT FOUND' TO LOGOFF-MSG.                  EL850
00369      GO TO 8300-SEND-TEXT.                                        EL850
00370                                                                   EL850
00371  9100-RETURN-TRAN.                                                EL850
00372      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL850
00373      MOVE '850A'               TO PI-CURRENT-SCREEN-NO.           EL850
00374      EXEC CICS RETURN                                             EL850
00375          TRANSID  (TRANS-ID)                                      EL850
00376          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL850
00377          LENGTH   (PI-COMM-LENGTH)                                EL850
00378      END-EXEC.                                                    EL850
00379                                                                   EL850
00380  9200-RETURN-MAIN-MENU.                                           EL850
00381      MOVE XCTL-626               TO PGM-NAME.                     EL850
00382      GO TO 9300-XCTL.                                             EL850
00383                                                                   EL850
00384  9300-XCTL.                                                       EL850
00385      EXEC CICS XCTL                                               EL850
00386          PROGRAM  (PGM-NAME)                                      EL850
00387          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL850
00388          LENGTH   (PI-COMM-LENGTH)                                EL850
00389      END-EXEC.                                                    EL850
00390                                                                   EL850
00391  9400-CLEAR.                                                      EL850
00392      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL850
00393      GO TO 9300-XCTL.                                             EL850
00394                                                                   EL850
00395  9500-PF12.                                                       EL850
00396      MOVE XCTL-010               TO PGM-NAME.                     EL850
00397      GO TO 9300-XCTL.                                             EL850
00398                                                                   EL850
00399  9600-PGMID-ERROR.                                                EL850
00400      EXEC CICS HANDLE CONDITION                                   EL850
00401          PGMIDERR (8300-SEND-TEXT)                                EL850
00402      END-EXEC.                                                    EL850
00403                                                                   EL850
00404      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL850
00405      MOVE ' '                    TO PI-ENTRY-CD-1.                EL850
00406      MOVE XCTL-005               TO PGM-NAME.                     EL850
00407      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL850
00408      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL850
00409      GO TO 9300-XCTL.                                             EL850
00410                                                                   EL850
00411  9900-ERROR-FORMAT.                                               EL850
00412      IF NOT EMI-ERRORS-COMPLETE                                   EL850
00413          MOVE LINK-001           TO PGM-NAME                      EL850
00414          EXEC CICS LINK                                           EL850
00415              PROGRAM  (PGM-NAME)                                  EL850
00416              COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)             EL850
00417              LENGTH   (EMI-COMM-LENGTH)                           EL850
00418          END-EXEC.                                                EL850
00419                                                                   EL850
00420  9900-EXIT.                                                       EL850
00421      EXIT.                                                        EL850
00422      EJECT                                                           CL**7
00423  9910-INITIALIZE-SECURITY.                                           CL**7
00424 ******************************************************************   CL**7
00425 *                                                                *   CL**7
00426 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *   CL**7
00427 *       USER SECURITY RECORD SET UP BY EL125.  IT THEN           *   CL**7
00428 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *   CL**7
00429 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *   CL**7
00430 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *   CL**7
00431 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *   CL**7
00432 *                                                                *   CL**7
00433 ******************************************************************   CL**7
00434                                                                      CL**7
00435      IF  PI-PROCESSOR-ID NOT EQUAL 'LGXX'                            CL**7
00436                                                                      CL**7
00437          IF  MORTGAGE-SESSION                                        CL**7
00438              MOVE '125E'             TO SC-QUID-SYSTEM               CL**7
00439              MOVE EIBTRMID           TO SC-QUID-TERMINAL             CL**7
00440                                                                      CL**7
00441              EXEC CICS READQ TS                                      CL**7
00442                  QUEUE  (SC-QUID-KEY)                                CL**7
00443                  INTO   (SECURITY-CONTROL-E)                         CL**7
00444                  LENGTH (SC-COMM-LENGTH-E)                           CL**7
00445                  ITEM   (SC-ITEM)                                    CL**7
00446              END-EXEC                                                CL**7
00447                                                                      CL**7
00448              MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)                   CL**7
00449                                      TO PI-DISPLAY-CAP               CL**7
00450              MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)                    CL**7
00451                                      TO PI-MODIFY-CAP                CL**7
00452                                                                   EL850
00453              IF  NOT DISPLAY-CAP                                     CL**7
00454                  MOVE 'READ'         TO SM-READ                      CL**7
00455                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT      CL**7
00456                  MOVE ER-9097        TO EMI-ERROR                    CL**7
00457                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**7
00458                  GO TO 8100-SEND-INITIAL-MAP                         CL**7
00459                                                                      CL**7
00460              ELSE                                                    CL**7
00461                  GO TO 9910-EXIT                                     CL**7
00462                                                                      CL**7
00463          ELSE                                                        CL**7
00464              EXEC CICS  READQ TS                                     CL**7
00465                  QUEUE   (QID)                                       CL*10
00466                  INTO    (SECURITY-CONTROL-D)                        CL*10
00467                  LENGTH  (SC-COMM-LENGTH-D)                          CL*10
00468                  ITEM    (QID-ITEM)                                  CL*10
00469                  END-EXEC                                            CL**7
00470                                                                      CL**7
00471              MOVE SC-AR-DISPLAY (01)                                 CL*10
00472                                  TO PI-DISPLAY-CAP                   CL**7
00473              MOVE SC-AR-UPDATE  (01)                                 CL*10
00474                                  TO PI-MODIFY-CAP                    CL**7
00475                                                                      CL**7
00476              IF  NOT DISPLAY-CAP                                     CL**7
00477                  MOVE 'READ'     TO SM-READ                          CL**7
00478                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT      CL**7
00479                  MOVE ER-0070    TO  EMI-ERROR                       CL**7
00480                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**7
00481                  GO TO 8100-SEND-INITIAL-MAP.                        CL**7
00482                                                                      CL**7
00483  9910-EXIT.                                                          CL**7
00484      EXIT.                                                           CL**7
00485      EJECT                                                           CL**7
00486  9990-ABEND.                                                      EL850
00487      MOVE LINK-004               TO PGM-NAME.                     EL850
00488      MOVE DFHEIBLK               TO EMI-LINE1.                    EL850
00489      EXEC CICS LINK                                               EL850
00490          PROGRAM  ('EL004')                                       EL850
00491          COMMAREA (EMI-LINE1)                                     EL850
00492          LENGTH   (72)                                            EL850
00493      END-EXEC.                                                    EL850
00494                                                                   EL850
00495      GO TO 8200-SEND-DATAONLY.                                    EL850
00496      GOBACK.                                                      EL850
00497                                                                   EL850
00498  9995-SECURITY-VIOLATION.                                         EL850
00499                              COPY ELCSCTP.                        EL850
00500                                                                   EL850
00501  9995-EXIT.                                                       EL850
00502      EXIT.                                                        EL850
00503                                                                   EL850
