00001  IDENTIFICATION DIVISION.                                         08/14/97
00002                                                                   EL116
00003  PROGRAM-ID.                 EL116 .                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 04/18/95 08:30:37.                    CL**4
00007 *                            VMOD=2.005.                             CL**5
00008 *                                                                 EL116
00008 *                                                                 EL116
00009 *AUTHOR.    LOGIC, INC.                                              CL**4
00010 *           DALLAS, TEXAS.                                           CL**4
00011                                                                   EL116
00012 *DATE-COMPILED.                                                      CL**4
00013                                                                   EL116
00014 *SECURITY.   *****************************************************   CL**4
00015 *            *                                                   *   CL**4
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00017 *            *                                                   *   CL**4
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00021 *            *                                                   *   CL**4
00022 *            *****************************************************   CL**4
00023                                                                   EL116
00024 *REMARKS.                                                            CL**4
00025 *        COPY ALL RECORDS FOR A COMPANY.                             CL**4
00026 *                                                                    CL**4
00027 *        ENTER - EX41 XXX YYY NNN ZZZ                                CL**4
00028 *                                                                    CL**4
00029 *        WHERE XXX = COMPANY ID TO BE COPIED                         CL**4
00030 *              YYY = NEW COMPANY ID                                  CL**4
00031 *              NNN = NEW COMPANY CD                                  CL**4
00032 *              ZZZ = "ALL" IF ALL FILES ARE TO BE COPIED             CL**4
00033                                                                   EL116
00034                                                                   EL116
00035      EJECT                                                        EL116
00036  ENVIRONMENT DIVISION.                                            EL116
00037                                                                   EL116
00038  DATA DIVISION.                                                   EL116
00039                                                                   EL116
00040  WORKING-STORAGE SECTION.                                         EL116
00041                                                                   EL116
00042  77  FILLER  PIC X(32)  VALUE '********************************'. EL116
00043  77  FILLER  PIC X(32)  VALUE '*     EL116 WORKING STORAGE    *'. EL116
00044  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.005 ************'.    CL**5
00045                                                                   EL116
00046  01  FILLER                          COMP-3.                      EL116
00047      05  WS-RECORD-COUNT             PIC S9(7)       VALUE ZERO.  EL116
00048      05  WS-RELEASED-AMOUNT          PIC S9(9)V99 VALUE ZERO.     EL116
00049                                                                   EL116
00050      05  WS-DELETE-CNTL              PIC S9          VALUE ZERO.  EL116
00051        88  DELETE-CNTL                               VALUE ZERO.  EL116
00052        88  DONT-DELETE-CNTL                          VALUE +1.    EL116
00053                                                                   EL116
00054      05  WS-DUP-COMPANY-CD           PIC S9          VALUE ZERO.  EL116
00055                                                                   EL116
00056  01  FILLER                          COMP                         EL116
00057                                      SYNC.                        EL116
00058                                                                   EL116
00059      05  WS-JOURNAL-FILE-ID          PIC S9(4)       VALUE +1.    EL116
00060      05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)       VALUE +773.     CL**3
00061      05  WS-JOURNAL-PREFIX-LENGTH    PIC S9(4)       VALUE +23.   EL116
00062      05  WS-TIOA-LENGTH              PIC S9(4)       VALUE ZERO.  EL116
00063      05  WS-RECORD-LENGTH            PIC S9(4)       VALUE ZERO.  EL116
00064      05  WS-SCREEN-LENGTH            PIC S9(4)       VALUE ZERO.  EL116
00065                                                                   EL116
00066  01  FILLER.                                                      EL116
00067                                                                   EL116
00068      05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.     EL116
00069                                                                   EL116
00070      05  WS-CONTROL-FILE-KEY.                                     EL116
00071          10  WS-CFK-COMPANY-ID       PIC XXX.                     EL116
00072          10  WS-CFK-RECORD-TYPE      PIC X.                       EL116
00073          10  WS-CFK-ACCESS           PIC X(4).                    EL116
00074          10  WS-CFK-SEQUENCE-NO      PIC S9(4)                    EL116
00075                                      COMP.                        EL116
00076                                                                   EL116
00077      05  WS-KEY.                                                  EL116
00078          10  WS-KEY-COMPANY-ID.                                   EL116
00079              15  WS-KEY-COMPANY-CD   PIC X.                       EL116
00080              15  FILLER              PIC XX.                      EL116
00081          10  FILLER                  PIC X(30).                   EL116
00082                                                                   EL116
00083      05  WS-NEW-COMPANY-ID           PIC X(3).                    EL116
00084                                                                   EL116
00085      05  WS-COMPANY-CODE             PIC S9(4)       COMP.        EL116
00086      05  FILLER                      REDEFINES                    EL116
00087          WS-COMPANY-CODE.                                         EL116
00088          10  FILLER                  PIC X.                       EL116
00089          10  WS-NEW-COMPANY-CD       PIC X.                       EL116
00090                                                                   EL116
00091      05  WS-OLD-COMPANY-NUMBER       PIC S9(4)       VALUE ZERO   EL116
00092                                      COMP.                        EL116
00093                                                                   EL116
00094      05  FILLER                      REDEFINES                    EL116
00095          WS-OLD-COMPANY-NUMBER.                                   EL116
00096          10  FILLER                  PIC X.                       EL116
00097          10  WS-OLD-COMPANY-CD-X     PIC X.                       EL116
00098                                                                   EL116
00099      05  WS-OLD-COMPANY-CD           PIC X.                       EL116
00100                                                                   EL116
00101      05  WS-DSID-AREA.                                            EL116
00102          10  WS-ELCNTL-DSID          PIC X(8) VALUE 'ELCNTL'.     EL116
00103          10  WS-ELPGMS-DSID          PIC X(8) VALUE 'ELPGMS'.     EL116
00104          10  WS-ELLETR-DSID          PIC X(8) VALUE 'ELLETR'.     EL116
00105          10  WS-ELFORM-DSID          PIC X(8) VALUE 'ELFORM'.     EL116
00106          10  WS-ELHELP-DSID          PIC X(8) VALUE 'ELHELP'.     EL116
00107          10  WS-ELACTQ-DSID          PIC X(8) VALUE 'ELACTQ'.     EL116
00108          10  WS-ELTRLR-DSID          PIC X(8) VALUE 'ELTRLR'.     EL116
00109          10  WS-ELCERT-DSID          PIC X(8) VALUE 'ELCERT'.     EL116
00110          10  WS-ELMSTR-DSID          PIC X(8) VALUE 'ELMSTR'.     EL116
00111          10  WS-ELCHKQ-DSID          PIC X(8) VALUE 'ELCHKQ'.     EL116
00112          10  WS-ELARCH-DSID          PIC X(8) VALUE 'ELARCH'.     EL116
00113          10  WS-ELREPT-DSID          PIC X(8) VALUE 'ELREPT'.     EL116
00114                                                                   EL116
00115      05  WS-DSID-NAME                REDEFINES                    EL116
00116          WS-DSID-AREA                PIC X(8)                     EL116
00117          OCCURS 12 TIMES             INDEXED BY DSID-INDEX.       EL116
00118                                                                   EL116
00119      05  DSID-INDEX-MAX              PIC S9(4)       VALUE +2     EL116
00120                                      COMP                         EL116
00121                                      SYNC.                        EL116
00122                                                                   EL116
00123      05  WS-DSID                     PIC X(8)        VALUE SPACES.EL116
00124                                                                   EL116
00125      05  WS-PROGRAM-ID               PIC X(8)      VALUE 'EL116'. EL116
00126                                                                   EL116
00127      05  WS-JOURNAL-TYPE-ID          PIC XX          VALUE 'EL'.  EL116
00128                                                                   EL116
00129      05  WS-SECOND-READ-SW           PIC X           VALUE '1'.   EL116
00130          88  FIRST-READ              VALUE '0'.                   EL116
00131          88  SECOND-READ             VALUE '1'.                   EL116
00132                                                                   EL116
00133      05  WS-BLANK                    PIC X           VALUE ' '.   EL116
00134                                                                   EL116
00135      05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.      EL116
00136      05  WS-SPACES                   PIC X           VALUE SPACES.EL116
00137                                                                   EL116
00138      05  WS-CURRENT-DATE             PIC XX VALUE LOW-VALUES.     EL116
00139                                                                   EL116
00140      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EX41'.EL116
00141      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70    EL116
00142                                      COMP                         EL116
00143                                      SYNCHRONIZED.                EL116
00144                                                                   EL116
00145      05  WS-TEXT-MESSAGE             PIC X(70)       VALUE SPACES.EL116
00146                                                                   EL116
00147      05  FILLER                      REDEFINES                    EL116
00148          WS-TEXT-MESSAGE.                                         EL116
00149          10  WS-TEXT-DSID            PIC X(6).                    EL116
00150          10  FILLER                  PIC X(30).                   EL116
00151          10  WS-TEXT-COUNT           PIC Z,ZZZ,ZZ9-.              EL116
00152          10  FILLER                  PIC X(24).                      CL**4
00153                                                                   EL116
00154      05  FILLER                      REDEFINES                    EL116
00155          WS-TEXT-MESSAGE.                                         EL116
00156          10  WS-TEXT-COMPANY-ID      PIC X(3).                    EL116
00157          10  FILLER                  PIC X(26).                   EL116
00158          10  WS-TEXT-COMPANY-NUMBER  PIC 9(3).                    EL116
00159          10  FILLER                  PIC X(38).                      CL**4
00160                                                                   EL116
00161      05  DFHTIOA.                                                 EL116
00162          10  FILLER                      PIC X(5).                EL116
00163          10  TIOA-COMPANY-ID             PIC X(3).                EL116
00164          10  FILLER                      PIC X.                   EL116
00165          10  TIOA-NEW-COMPANY-ID         PIC X(3).                EL116
00166          10  FILLER                      PIC X.                   EL116
00167          10  TIOA-NEW-COMPANY-CD         PIC 9(3).                EL116
00168          10  FILLER                      PIC X.                   EL116
00169          10  TIOA-ALL                    PIC X(3).                EL116
00170                                                                   EL116
00171      05  WS-SCREEN-AREA.                                          EL116
00172          10  WS-SCREEN-LINE          PIC X(80)                    EL116
00173              OCCURS 24 TIMES         INDEXED BY SCREEN-INDEX.     EL116
00174                                                                   EL116
00175                     COPY ELCJPFX .                                   CL**2
00176                                      PIC X(512).                  EL116
00177                                                                   EL116
00178                                                                   EL116
00179  LINKAGE SECTION.                                                 EL116
00180                                                                   EL116
00181  01  DFHCOMMAREA                     PIC X(512).                  EL116
00182                                                                   EL116
00183 *01 DFHBLLDS                         COMP                            CL**4
00184 *                                    SYNC.                           CL**4
00185 *    05  BLLCBAR                     PIC S9(9).                      CL**4
00186 *    05  TIOABAR                     PIC S9(9).                      CL**4
00187 *    05  FIOABAR                     PIC S9(9).                      CL**4
00188 *    05  CFCBAR                      PIC S9(9).                      CL**4
00189 *    05  ELFCBAR                     PIC S9(9).                      CL**4
00190 *    05  CMFCBAR                     PIC S9(9).                      CL**4
00191 *    05  LAFCBAR                     PIC S9(9).                      CL**4
00192                                                                   EL116
00193  01  TERMINAL-INPUT-OUTPUT-AREA.                                  EL116
00194      05  FILLER                      PIC X                        EL116
00195          OCCURS 20 TIMES                                          EL116
00196          DEPENDING ON WS-TIOA-LENGTH.                             EL116
00197                                                                   EL116
00198  01  DFHFIOA.                                                     EL116
00199      05  FILLER                      PIC XX.                      EL116
00200      05  FIOA-KEY.                                                EL116
00201          10  FIOA-COMPANY-ID.                                     EL116
00202              15  FIOA-COMPANY-CD     PIC X.                       EL116
00203              15  FILLER              PIC XX.                      EL116
00204      05  FILLER                      PIC X(745).                     CL**5
00205                                                                   EL116
00206                   COPY ELCCNTL .                                     CL**2
00207                                                                   EL116
00208                   COPY ELCMSTR .                                     CL**2
00209                                                                   EL116
00210                   COPY ELCCERT .                                     CL**2
00211                                                                   EL116
00212                   COPY ELCARCH .                                     CL**2
00213                                                                   EL116
00214      EJECT                                                        EL116
00215  PROCEDURE DIVISION.                                              EL116
00216                                                                   EL116
00217      EXEC CICS HANDLE CONDITION                                   EL116
00218          NOTFND  (0800-MAIN-LOGIC)                                EL116
00219          DUPREC  (0700-MAIN-LOGIC)                                EL116
00220          NOSPACE (0290-MAIN-LOGIC)                                EL116
00221          ERROR   (0900-MAIN-LOGIC)                                EL116
00222          ENDFILE (0400-MAIN-LOGIC)                                EL116
00223      END-EXEC.                                                    EL116
00224                                                                   EL116
00225      MOVE LOW-VALUES             TO  WS-SCREEN-AREA.              EL116
00226      SET SCREEN-INDEX TO +2.                                      EL116
00227                                                                   EL116
00228      EXEC CICS RECEIVE                                            EL116
00229          SET    (ADDRESS OF TERMINAL-INPUT-OUTPUT-AREA)              CL**4
00230          LENGTH (WS-TIOA-LENGTH)                                  EL116
00231      END-EXEC.                                                    EL116
00232                                                                   EL116
00233      MOVE WS-TIOA-LENGTH         TO  WS-TIOA-LENGTH.              EL116
00234                                                                   EL116
00235      MOVE TERMINAL-INPUT-OUTPUT-AREA  TO  WS-SCREEN-LINE (1)      EL116
00236                                           DFHTIOA.                EL116
00237                                                                   EL116
00238      IF WS-TIOA-LENGTH NOT GREATER +7                             EL116
00239          MOVE 'NO COMPANY ID ENTERED'  TO  WS-TEXT-MESSAGE        EL116
00240          GO TO 1000-MAIN-LOGIC.                                   EL116
00241                                                                   EL116
00242      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.         EL116
00243      MOVE TIOA-COMPANY-ID        TO  WS-CFK-COMPANY-ID.           EL116
00244      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.          EL116
00245      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          EL116
00246                                                                   EL116
00247      EXEC CICS READ                                               EL116
00248          DATASET (WS-CONTROL-FILE-DSID)                           EL116
00249          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL116
00250          SET     (ADDRESS OF CONTROL-FILE)                           CL**4
00251      END-EXEC.                                                    EL116
00252                                                                   EL116
00253      MOVE CF-COMPANY-CD          TO  WS-OLD-COMPANY-CD.           EL116
00254                                                                   EL116
00255      IF TIOA-NEW-COMPANY-ID = SPACES                              EL116
00256          MOVE 'NO NEW COMPANY CODE ENTERED'  TO  WS-TEXT-MESSAGE  EL116
00257          GO TO 1000-MAIN-LOGIC.                                   EL116
00258                                                                   EL116
00259      IF TIOA-NEW-COMPANY-CD NUMERIC                               EL116
00260        AND TIOA-NEW-COMPANY-CD GREATER ZERO                       EL116
00261        AND TIOA-NEW-COMPANY-CD NOT GREATER 255                    EL116
00262          MOVE TIOA-NEW-COMPANY-CD  TO  WS-COMPANY-CODE            EL116
00263        ELSE                                                       EL116
00264          MOVE 'NEW COMPANY CODE IS NOT VALID'  TO  WS-TEXT-MESSAGEEL116
00265          GO TO 1000-MAIN-LOGIC.                                   EL116
00266                                                                   EL116
00267      IF TIOA-ALL = 'ALL'                                          EL116
00268          MOVE +12                TO  DSID-INDEX-MAX.              EL116
00269                                                                   EL116
00270      MOVE 'A'                    TO  JP-RECORD-TYPE.              EL116
00271      MOVE 'LGXX'                 TO  JP-USER-ID.                  EL116
00272      MOVE 'EL116   '             TO  JP-PROGRAM-ID.               EL116
00273                                                                   EL116
00274      SET DSID-INDEX TO +1.                                        EL116
00275      MOVE WS-DSID-NAME (1)       TO  WS-DSID.                     EL116
00276                                                                   EL116
00277      PERFORM 3000-FIND-COMPANY-CODE.                              EL116
00278                                                                   EL116
00279      MOVE LOW-VALUES             TO  WS-KEY.                      EL116
00280      MOVE TIOA-COMPANY-ID        TO  WS-KEY-COMPANY-ID.           EL116
00281                                                                   EL116
00282      EXEC CICS GETMAIN                                            EL116
00283           LENGTH    (750)                                            CL**3
00284           SET       (ADDRESS OF DFHFIOA)                             CL**4
00285           INITIMG   (WS-BLANK)                                    EL116
00286      END-EXEC.                                                    EL116
00287                                                                   EL116
00288      SET ADDRESS OF CONTROL-FILE       TO ADDRESS OF DFHFIOA         CL**4
00289      SET ADDRESS OF CLAIM-MASTER       TO ADDRESS OF DFHFIOA         CL**4
00290      SET ADDRESS OF CERTIFICATE-MASTER TO ADDRESS OF DFHFIOA         CL**4
00291      SET ADDRESS OF LETTER-ARCHIVE     TO ADDRESS OF DFHFIOA.        CL**4
00292                                                                   EL116
00293                                                                   EL116
00294  0100-MAIN-LOGIC.                                                 EL116
00295      EXEC CICS HANDLE CONDITION                                   EL116
00296          ENDFILE (0400-MAIN-LOGIC)                                EL116
00297          NOTFND  (0400-MAIN-LOGIC)                                EL116
00298      END-EXEC.                                                    EL116
00299                                                                   EL116
00300      MOVE WS-DSID                 TO JP-FILE-ID.                  EL116
00301                                                                   EL116
00302      MOVE ZERO                   TO  WS-RECORD-COUNT.             EL116
00303                                                                   EL116
00304  0105-MAIN-LOGIC.                                                 EL116
00305      EXEC CICS STARTBR                                            EL116
00306          DATASET (WS-DSID)                                        EL116
00307          RIDFLD  (WS-KEY)                                         EL116
00308          GTEQ                                                     EL116
00309      END-EXEC.                                                    EL116
00310                                                                   EL116
00311  0200-MAIN-LOGIC.                                                 EL116
00312      EXEC CICS READNEXT                                           EL116
00313          DATASET (WS-DSID)                                        EL116
00314          RIDFLD  (WS-KEY)                                         EL116
00315          INTO    (DFHFIOA)                                        EL116
00316      END-EXEC.                                                    EL116
00317                                                                   EL116
00318      IF FIRST-READ                                                EL116
00319         MOVE '1'                 TO WS-SECOND-READ-SW             EL116
00320         GO TO 0200-MAIN-LOGIC.                                    EL116
00321                                                                   EL116
00322      EXEC CICS ENDBR                                              EL116
00323          DATASET (WS-DSID)                                        EL116
00324      END-EXEC.                                                    EL116
00325                                                                   EL116
00326      MOVE '0'                    TO WS-SECOND-READ-SW.            EL116
00327                                                                   EL116
00328      IF WS-DSID = WS-CONTROL-FILE-DSID                            EL116
00329          IF WS-CFK-COMPANY-ID = WS-KEY-COMPANY-ID                 EL116
00330              MOVE TIOA-NEW-COMPANY-ID  TO  FIOA-COMPANY-ID        EL116
00331              IF CF-RECORD-TYPE = '1'                              EL116
00332                  MOVE WS-NEW-COMPANY-CD  TO  CF-COMPANY-CD        EL116
00333                ELSE                                               EL116
00334                  NEXT SENTENCE                                    EL116
00335            ELSE                                                   EL116
00336              GO TO 0400-MAIN-LOGIC                                EL116
00337        ELSE                                                       EL116
00338          IF WS-KEY-COMPANY-CD NOT = WS-OLD-COMPANY-CD             EL116
00339              GO TO 0400-MAIN-LOGIC                                EL116
00340            ELSE                                                   EL116
00341              MOVE WS-NEW-COMPANY-CD  TO  FIOA-COMPANY-CD          EL116
00342              IF WS-DSID = 'ELMSTR  '                              EL116
00343                  MOVE WS-NEW-COMPANY-CD  TO  CL-COMPANY-CD-A1     EL116
00344                                              CL-COMPANY-CD-A2     EL116
00345                                              CL-COMPANY-CD-A4     EL116
00346                                              CL-COMPANY-CD-A5        CL**4
00347                ELSE                                               EL116
00348              IF WS-DSID = 'ELCERT  '                              EL116
00349                  MOVE WS-NEW-COMPANY-CD  TO  CM-COMPANY-CD-A1     EL116
00350                                              CM-COMPANY-CD-A2     EL116
00351                                              CM-COMPANY-CD-A4     EL116
00352                                              CM-COMPANY-CD-A5     EL116
00353                ELSE                                               EL116
00354              IF WS-DSID = 'ELARCH  '                              EL116
00355                  MOVE WS-NEW-COMPANY-CD  TO  LA-COMPANY-CD-A1.    EL116
00356                                                                   EL116
00357                                                                   EL116
00358      MOVE DFHFIOA                TO  JP-RECORD-AREA.              EL116
00359                                                                   EL116
00360      EXEC CICS WRITE                                              EL116
00361          DATASET (WS-DSID)                                        EL116
00362          RIDFLD  (FIOA-KEY)                                       EL116
00363          FROM    (DFHFIOA)                                        EL116
00364      END-EXEC.                                                    EL116
00365                                                                   EL116
00366      ADD WS-RECORD-LENGTH                                         EL116
00367          WS-JOURNAL-PREFIX-LENGTH GIVING WS-JOURNAL-RECORD-LENGTH.EL116
00368                                                                   EL116
pemuni*    EXEC CICS JOURNAL                                            EL116
pemuni*        JFILEID (WS-JOURNAL-FILE-ID)                             EL116
pemuni*        JTYPEID (WS-JOURNAL-TYPE-ID)                             EL116
pemuni*        FROM   (JOURNAL-RECORD)                                  EL116
pemuni*        LENGTH (WS-JOURNAL-RECORD-LENGTH)                        EL116
pemuni*    END-EXEC.                                                    EL116
00375                                                                   EL116
00376      ADD +1  TO  WS-RECORD-COUNT.                                 EL116
00377                                                                   EL116
00378      GO TO 0105-MAIN-LOGIC.                                       EL116
00379                                                                   EL116
00380  0290-MAIN-LOGIC.                                                 EL116
00381      MOVE 'ELXXXX HAS NO MORE SPACE AVAILABLE'                    EL116
00382                                  TO  WS-TEXT-MESSAGE.             EL116
00383      MOVE WS-DSID                TO  WS-TEXT-DSID.                EL116
00384      PERFORM 2000-SEND.                                           EL116
00385                                                                   EL116
00386  0400-MAIN-LOGIC.                                                 EL116
00387      MOVE 'ELXXXX DATASET SUCCESSFULLY COPIED  X,XXX,XXX RECORDS' EL116
00388                                  TO  WS-TEXT-MESSAGE.             EL116
00389      MOVE WS-DSID                TO  WS-TEXT-DSID.                EL116
00390      MOVE WS-RECORD-COUNT        TO  WS-TEXT-COUNT.               EL116
00391                                                                   EL116
00392      PERFORM 2000-SEND.                                           EL116
00393                                                                   EL116
00394      EXEC CICS SYNCPOINT                                          EL116
00395          END-EXEC.                                                EL116
00396                                                                   EL116
00397      IF DSID-INDEX LESS DSID-INDEX-MAX                            EL116
00398          SET DSID-INDEX UP BY +1                                  EL116
00399          MOVE WS-DSID-NAME (DSID-INDEX) TO  WS-DSID               EL116
00400          MOVE LOW-VALUES         TO  WS-KEY                       EL116
00401          MOVE WS-OLD-COMPANY-CD  TO  WS-KEY-COMPANY-CD            EL116
00402          GO TO 0100-MAIN-LOGIC.                                   EL116
00403                                                                   EL116
00404  0500-MAIN-LOGIC.                                                 EL116
00405      MOVE 'COMPANY COPY SUCCESSFULLY COMPLETED'                   EL116
00406                                  TO  WS-TEXT-MESSAGE.             EL116
00407      GO TO 1000-MAIN-LOGIC.                                       EL116
00408                                                                   EL116
00409  0700-MAIN-LOGIC.                                                 EL116
00410      MOVE 'ELXXXX - COMPANY ALREADY EXISTS'  TO  WS-TEXT-MESSAGE.    CL**4
00411      MOVE WS-DSID                TO  WS-TEXT-DSID.                EL116
00412      GO TO 1000-MAIN-LOGIC.                                       EL116
00413                                                                   EL116
00414  0800-MAIN-LOGIC.                                                 EL116
00415      MOVE 'COMPANY NOT FOUND ON CONTROL FILE'  TO  WS-TEXT-MESSAGEEL116
00416      GO TO 1000-MAIN-LOGIC.                                       EL116
00417                                                                   EL116
00418  0900-MAIN-LOGIC.                                                 EL116
00419      MOVE 'ERROR OCCURRED'       TO  WS-TEXT-MESSAGE.                CL**4
00420                                                                   EL116
00421  1000-MAIN-LOGIC.                                                 EL116
00422      PERFORM 2000-SEND.                                           EL116
00423                                                                   EL116
00424      EXEC CICS RETURN                                             EL116
00425          END-EXEC.                                                EL116
00426                                                                   EL116
00427  2000-SEND SECTION.                                               EL116
00428      MOVE WS-TEXT-MESSAGE  TO  WS-SCREEN-LINE (SCREEN-INDEX).     EL116
00429                                                                   EL116
00430      SET WS-SCREEN-LENGTH TO SCREEN-INDEX.                        EL116
00431      MULTIPLY +80 BY WS-SCREEN-LENGTH.                            EL116
00432                                                                   EL116
00433      EXEC CICS SEND                                               EL116
00434          FROM (WS-SCREEN-AREA)                                    EL116
00435          LENGTH (WS-SCREEN-LENGTH)                                EL116
00436          ERASE                                                    EL116
00437      END-EXEC.                                                    EL116
00438                                                                   EL116
00439      IF SCREEN-INDEX LESS +24                                     EL116
00440          SET SCREEN-INDEX UP BY +1                                EL116
00441        ELSE                                                       EL116
00442          SET SCREEN-INDEX TO +2.                                  EL116
00443                                                                   EL116
00444  2000-EXIT.                                                       EL116
00445      EXIT.                                                        EL116
00446                                                                   EL116
00447      EJECT                                                        EL116
00448  3000-FIND-COMPANY-CODE SECTION.                                  EL116
00449      EXEC CICS HANDLE CONDITION                                   EL116
00450          ENDFILE (3080-ENDFILE)                                   EL116
00451      END-EXEC.                                                    EL116
00452                                                                   EL116
00453      MOVE LOW-VALUES             TO  WS-KEY.                      EL116
00454                                                                   EL116
00455      EXEC CICS STARTBR                                            EL116
00456          DATASET (WS-DSID)                                        EL116
00457          RIDFLD  (WS-KEY)                                         EL116
00458          GTEQ                                                     EL116
00459      END-EXEC.                                                    EL116
00460                                                                   EL116
00461  3010-READNEXT.                                                   EL116
00462      EXEC CICS READNEXT                                           EL116
00463          DATASET (WS-DSID)                                        EL116
00464          RIDFLD  (WS-KEY)                                         EL116
00465          SET     (ADDRESS OF DFHFIOA)                                CL**4
00466      END-EXEC.                                                    EL116
00467                                                                   EL116
00468      SET ADDRESS OF CONTROL-FILE       TO ADDRESS OF DFHFIOA         CL**4
00469      SET ADDRESS OF CLAIM-MASTER       TO ADDRESS OF DFHFIOA         CL**4
00470      SET ADDRESS OF CERTIFICATE-MASTER TO ADDRESS OF DFHFIOA         CL**4
00471      SET ADDRESS OF LETTER-ARCHIVE     TO ADDRESS OF DFHFIOA.        CL**4
00472                                                                   EL116
00473      IF CF-RECORD-TYPE NOT = '1'                                  EL116
00474          GO TO 3010-READNEXT.                                     EL116
00475                                                                   EL116
00476      MOVE 'XXX COMPANY ID HAS A CODE OF XXX'                      EL116
00477                                  TO  WS-TEXT-MESSAGE.             EL116
00478      MOVE CF-COMPANY-ID          TO  WS-TEXT-COMPANY-ID.          EL116
00479      MOVE CF-COMPANY-CD          TO  WS-OLD-COMPANY-CD-X.         EL116
00480      MOVE WS-OLD-COMPANY-NUMBER  TO  WS-TEXT-COMPANY-NUMBER.      EL116
00481      PERFORM 2000-SEND.                                           EL116
00482                                                                   EL116
00483      IF CF-COMPANY-CD = WS-NEW-COMPANY-CD                         EL116
00484          MOVE +1                 TO  WS-DUP-COMPANY-CD            EL116
00485          GO TO 1000-MAIN-LOGIC.                                   EL116
00486                                                                   EL116
00487      GO TO 3010-READNEXT.                                         EL116
00488                                                                   EL116
00489  3080-ENDFILE.                                                    EL116
00490      EXEC CICS ENDBR                                              EL116
00491          DATASET (WS-DSID)                                        EL116
00492      END-EXEC.                                                    EL116
00493                                                                   EL116
00494      IF WS-DUP-COMPANY-CD = +1                                    EL116
00495          MOVE 'COMPANY CODE ALREADY EXISTS'  TO  WS-TEXT-MESSAGE     CL**4
00496          GO TO 1000-MAIN-LOGIC.                                   EL116
00497                                                                   EL116
00498  3090-EXIT.                                                       EL116
00499      EXIT.                                                        EL116
00500                                                                   EL116
00501  9999-LAST-PARAGRAPH SECTION.                                     EL116
00502      GOBACK.                                                      EL116
