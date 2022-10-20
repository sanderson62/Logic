00001  IDENTIFICATION DIVISION.                                         03/22/96
00002                                                                   EL117
00003  PROGRAM-ID.                 EL117 .                                 LV004
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/13/96 09:57:39.                    CL**4
00007 *                            VMOD=2.004.                             CL**4
00008 *                                                                 EL117
00008 *                                                                 EL117
00009 *AUTHOR.    LOGIC, INC.                                              CL**4
00010 *           DALLAS, TEXAS.                                           CL**4
00011                                                                   EL117
00012 *DATE-COMPILED.                                                      CL**4
00013                                                                   EL117
00014 *SECURITY.   *****************************************************   CL**4
00015 *            *                                                   *   CL**4
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00017 *            *                                                   *   CL**4
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00019 *                                                                *   CL**4
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00022 *            *                                                   *   CL**4
00023 *            *****************************************************   CL**4
00024                                                                   EL117
00025 *REMARKS.                                                            CL**3
00026 *        DELETE ALL RECORDS FOR A COMPANY.                           CL**3
00027 *                                                                    CL**3
00028 *        ENTER - EX42 XXX YYYYYYYY                                   CL**3
00029 *                                                                    CL**3
00030 *        WHERE XXX      = COMPANY ID TO BE DELETED                   CL**3
00031 *              YYYYYYYY = FILE NAME IF ONLY ONE (1) IS TO BE         CL**3
00032 *                         DELETED OR "NOCNTL" IF ALL FILES ARE       CL**3
00033 *                         TO BE DELETED EXECPT CONTROL FILE OR       CL**3
00034 *                         BLANK IF ALL FILES ARE TO BE DELETED.      CL**3
00035 *                         ENTER "CREDIT" IF ONLY THE FILES           CL**3
00036 *                         STARTING WITH 'ER' ARE TO BE DELETED.      CL**3
00037                                                                   EL117
00038      EJECT                                                        EL117
00039  ENVIRONMENT DIVISION.                                            EL117
00040                                                                   EL117
00041  DATA DIVISION.                                                   EL117
00042                                                                   EL117
00043  WORKING-STORAGE SECTION.                                         EL117
00044                                                                   EL117
00045  77  FILLER  PIC X(32)  VALUE '*******************************'.  EL117
00046  77  FILLER  PIC X(32)  VALUE '*    EL117 WORKING STORAGE    *'.  EL117
00047  77  FILLER  PIC X(32)  VALUE '********* VMOD 2.004 **********'.     CL**4
00048                                                                   EL117
00049  01  FILLER                          COMP-3.                      EL117
00050      05  WS-RECORD-COUNT             PIC S9(7)       VALUE ZERO.  EL117
00051      05  WS-WORK                     PIC S9(7)       VALUE ZERO.  EL117
00052      05  WS-REMAINDER                PIC S9(7)       VALUE ZERO.  EL117
00053      05  WS-RELEASED-AMOUNT          PIC S9(9)V99 VALUE ZERO.     EL117
00054                                                                   EL117
00055      05  WS-DELETE-CNTL              PIC S9          VALUE ZERO.  EL117
00056        88  DELETE-CNTL                               VALUE ZERO.  EL117
00057        88  DONT-DELETE-CNTL                          VALUE +1.    EL117
00058                                                                   EL117
00059      05  WS-BROWSE-SW                PIC S9          VALUE ZERO.  EL117
00060                                                                   EL117
00061  01  FILLER                          COMP                         EL117
00062                                      SYNC.                        EL117
00063      05  WS-JOURNAL-FILE-ID          PIC S9(4)       VALUE +1.    EL117
00064      05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)       VALUE +527.  EL117
00065      05  WS-JOURNAL-PREFIX-LENGTH    PIC S9(4)       VALUE +23.   EL117
00066      05  WS-TIOA-LENGTH              PIC S9(4)       VALUE ZERO.  EL117
00067      05  WS-RECORD-LENGTH            PIC S9(4)       VALUE ZERO.  EL117
00068      05  WS-SCREEN-LENGTH            PIC S9(4)       VALUE ZERO.  EL117
00069                                                                   EL117
00070  01  FILLER.                                                      EL117
00071      05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.     EL117
00072                                                                   EL117
00073      05  WS-CONTROL-FILE-KEY.                                     EL117
00074          10  WS-CFK-COMPANY-ID       PIC XXX.                     EL117
00075          10  WS-CFK-RECORD-TYPE      PIC X.                       EL117
00076          10  WS-CFK-ACCESS           PIC X(4).                    EL117
00077          10  WS-CFK-SEQUENCE-NO      PIC S9(4)                    EL117
00078                                      COMP.                        EL117
00079                                                                   EL117
00080      05  SAVE-COMPANY-CD             PIC XX.                      EL117
00081                                                                   EL117
00082      05  WS-KEY.                                                  EL117
00083          10  WS-KEY-COMPANY-ID.                                   EL117
00084              15  WS-KEY-COMPANY-CD   PIC X.                       EL117
00085              15  FILLER              PIC XX.                      EL117
00086          10  FILLER                  PIC X(30).                   EL117
00087                                                                   EL117
00088      05  WS-DSID-AREA.                                            EL117
00089          10  WS-ELCERT-DSID          PIC X(8) VALUE 'ELCERT'.        CL**2
00090          10  WS-ELACTQ-DSID          PIC X(8) VALUE 'ELACTQ'.     EL117
00091          10  WS-ELTRLR-DSID          PIC X(8) VALUE 'ELTRLR'.     EL117
00092          10  WS-ELMSTR-DSID          PIC X(8) VALUE 'ELMSTR'.     EL117
00093          10  WS-ELCHKQ-DSID          PIC X(8) VALUE 'ELCHKQ'.     EL117
00094          10  WS-ELARCH-DSID          PIC X(8) VALUE 'ELARCH'.     EL117
00095          10  WS-ELPGMS-DSID          PIC X(8) VALUE 'ELPGMS'.     EL117
00096          10  WS-ELLETR-DSID          PIC X(8) VALUE 'ELLETR'.     EL117
00097          10  WS-ELFORM-DSID          PIC X(8) VALUE 'ELFORM'.     EL117
00098          10  WS-ELHELP-DSID          PIC X(8) VALUE 'ELHELP'.     EL117
00099          10  WS-ELREPT-DSID          PIC X(8) VALUE 'ELREPT'.     EL117
00100          10  WS-ERACCT-DSID          PIC X(8) VALUE 'ERACCT'.     EL117
00101          10  WS-ERBILL-DSID          PIC X(8) VALUE 'ERBILL'.     EL117
00102          10  WS-ERCRTC-DSID          PIC X(8) VALUE 'ERCRTC'.     EL117
00103          10  WS-ERCHEK-DSID          PIC X(8) VALUE 'ERCHEK'.     EL117
00104          10  WS-ERCHKQ-DSID          PIC X(8) VALUE 'ERCHKQ'.     EL117
00105          10  WS-ERCTBL-DSID          PIC X(8) VALUE 'ERCTBL'.     EL117
00106          10  WS-ERCOMP-DSID          PIC X(8) VALUE 'ERCOMP'.     EL117
00107          10  WS-ERCOMP-DSID          PIC X(8) VALUE 'ERNOTE'.     EL117
00108          10  WS-ERPNDB-DSID          PIC X(8) VALUE 'ERPNDB'.     EL117
00109          10  WS-ERPNDC-DSID          PIC X(8) VALUE 'ERPNDC'.     EL117
00110          10  WS-ERPYAJ-DSID          PIC X(8) VALUE 'ERPYAJ'.     EL117
00111          10  WS-ERREIN-DSID          PIC X(8) VALUE 'ERREIN'.     EL117
00112          10  WS-ERREPY-DSID          PIC X(8) VALUE 'ERREPY'.     EL117
00113          10  WS-ERPNDM-DSID          PIC X(8) VALUE 'ERPNDM'.        CL**3
00114          10  WS-ERRATE-DSID          PIC X(8) VALUE 'ERRATE'.        CL**3
00115                                                                   EL117
00116      05  WS-DSID-NAME                REDEFINES                    EL117
00117          WS-DSID-AREA                PIC X(8)                     EL117
00118          OCCURS 26 TIMES             INDEXED BY DSID-INDEX.          CL**3
00119                                                                   EL117
00120      05  DSID-INDEX-MAX              PIC S9(4)       VALUE +26       CL**3
00121                                      COMP                         EL117
00122                                      SYNC.                        EL117
00123                                                                   EL117
00124      05  WS-DSID                     PIC X(8)        VALUE SPACES.EL117
00125                                                                   EL117
00126      05  WS-PROGRAM-ID               PIC X(8)      VALUE 'EL117'. EL117
00127                                                                   EL117
00128      05  WS-JOURNAL-TYPE-ID          PIC XX          VALUE 'EL'.  EL117
00129                                                                   EL117
00130      05  WS-LOW-VALUES               PIC X VALUE LOW-VALUES.      EL117
00131      05  WS-SPACES                   PIC X           VALUE SPACES.EL117
00132                                                                   EL117
00133      05  WS-CURRENT-DATE             PIC XX VALUE LOW-VALUES.     EL117
00134                                                                   EL117
00135      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EX42'.EL117
00136      05  WS-TEXT-MESSAGE-LENGTH      PIC S9(4)       VALUE +70    EL117
00137                                      COMP                         EL117
00138                                      SYNC.                        EL117
00139                                                                   EL117
00140      05  WS-TEXT-MESSAGE             PIC X(80)       VALUE SPACES.EL117
00141                                                                   EL117
00142      05  FILLER                      REDEFINES                    EL117
00143          WS-TEXT-MESSAGE.                                         EL117
00144          10  WS-TEXT-DSID            PIC X(6).                    EL117
00145          10  FILLER                  PIC X(30).                   EL117
00146          10  WS-TEXT-COUNT           PIC Z,ZZZ,ZZ9-.              EL117
00147          10  FILLER                  PIC X(34).                      CL**4
00148                                                                   EL117
00149      05  DFHTIOA.                                                 EL117
00150          10  FILLER                      PIC X(5).                EL117
00151          10  TIOA-COMPANY-ID             PIC X(3).                EL117
00152          10  FILLER                      PIC X.                   EL117
00153          10  TIOA-NO-CNTL                PIC X(8).                EL117
00154          10  TIOA-DSID                   REDEFINES                EL117
00155              TIOA-NO-CNTL                PIC X(8).                EL117
00156                                                                   EL117
00157      05  WS-SCREEN-AREA.                                          EL117
00158          10  WS-SCREEN-LINE          PIC X(80)                    EL117
00159              OCCURS 24 TIMES         INDEXED BY SCREEN-INDEX.     EL117
00160                                                                   EL117
00161                   COPY ELCCNTL SUPPRESS.                             CL**3
00162                                                                   EL117
00163  LINKAGE SECTION.                                                 EL117
00164  01  DFHCOMMAREA                     PIC X(1024).                 EL117
00165                                                                   EL117
00166  01  TERMINAL-INPUT-OUTPUT-AREA.                                  EL117
00167      05  FILLER                      PIC X                        EL117
00168          OCCURS 20 TIMES                                          EL117
00169          DEPENDING ON WS-TIOA-LENGTH.                             EL117
00170                                                                   EL117
00171  01  DFHFIOA                         PIC X(1024).                 EL117
00172                                                                   EL117
00173      EJECT                                                        EL117
00174  PROCEDURE DIVISION.                                              EL117
00175                                                                   EL117
00176      EXEC CICS HANDLE CONDITION                                   EL117
00177          NOTFND  (0800-MAIN-LOGIC)                                EL117
00178          ERROR   (0900-MAIN-LOGIC)                                EL117
00179          ENDFILE (0400-MAIN-LOGIC)                                EL117
00180      END-EXEC.                                                    EL117
00181                                                                   EL117
00182      MOVE LOW-VALUES             TO  WS-SCREEN-AREA.              EL117
00183      SET SCREEN-INDEX TO +2.                                      EL117
00184                                                                   EL117
00185      EXEC CICS RECEIVE                                            EL117
00186          SET    (ADDRESS OF TERMINAL-INPUT-OUTPUT-AREA)              CL**4
00187          LENGTH (WS-TIOA-LENGTH)                                  EL117
00188      END-EXEC.                                                    EL117
00189                                                                   EL117
00190      MOVE TERMINAL-INPUT-OUTPUT-AREA  TO  WS-SCREEN-LINE (1)      EL117
00191                                           DFHTIOA.                EL117
00192                                                                   EL117
00193      IF WS-TIOA-LENGTH NOT GREATER +7                             EL117
00194          MOVE 'NO COMPANY ID ENTERED'  TO  WS-TEXT-MESSAGE        EL117
00195          GO TO 1000-MAIN-LOGIC.                                   EL117
00196                                                                   EL117
00197      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.         EL117
00198      MOVE TIOA-COMPANY-ID        TO  WS-CFK-COMPANY-ID.           EL117
00199      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.          EL117
00200      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          EL117
00201                                                                   EL117
00202      EXEC CICS READ                                               EL117
00203          DATASET (WS-CONTROL-FILE-DSID)                           EL117
00204          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL117
00205          INTO   (CONTROL-FILE)                                    EL117
00206      END-EXEC.                                                    EL117
00207                                                                   EL117
00208      SET DSID-INDEX TO +1.                                        EL117
00209      MOVE WS-DSID-NAME (1)       TO  WS-DSID.                     EL117
00210                                                                   EL117
00211      MOVE LOW-VALUES             TO  WS-KEY.                      EL117
00212      MOVE CF-COMPANY-CD          TO  WS-KEY-COMPANY-CD            EL117
00213                                      SAVE-COMPANY-CD.             EL117
00214                                                                   EL117
00215      IF TIOA-NO-CNTL = 'NOCNTL  '                                 EL117
00216          MOVE +1                 TO  WS-DELETE-CNTL               EL117
00217          GO TO 0100-MAIN-LOGIC.                                   EL117
00218                                                                   EL117
00219      IF TIOA-DSID = SPACES                                        EL117
00220          GO TO 0100-MAIN-LOGIC.                                   EL117
00221                                                                   EL117
00222      IF TIOA-DSID = 'CREDIT  '                                    EL117
00223          MOVE SPACES  TO TIOA-DSID                                EL117
00224          SET DSID-INDEX TO     +13                                EL117
00225          MOVE WS-DSID-NAME (DSID-INDEX) TO WS-DSID                EL117
00226          GO TO 0100-MAIN-LOGIC.                                   EL117
00227                                                                   EL117
00228  0050-MAIN-LOGIC.                                                 EL117
00229      IF TIOA-DSID = WS-DSID-NAME (DSID-INDEX)                     EL117
00230          MOVE TIOA-DSID          TO  WS-DSID                      EL117
00231          MOVE +2                 TO  WS-DELETE-CNTL               EL117
00232          GO TO 0100-MAIN-LOGIC.                                   EL117
00233                                                                   EL117
00234      IF DSID-INDEX LESS THAN DSID-INDEX-MAX                       EL117
00235          SET DSID-INDEX UP BY +1                                  EL117
00236          GO TO 0050-MAIN-LOGIC.                                   EL117
00237                                                                   EL117
00238      MOVE 'DSID NOT VALID'  TO  WS-TEXT-MESSAGE                   EL117
00239      GO TO 1000-MAIN-LOGIC.                                       EL117
00240                                                                   EL117
00241  0100-MAIN-LOGIC.                                                 EL117
00242      EXEC CICS HANDLE CONDITION                                   EL117
00243          NOTFND (0400-MAIN-LOGIC)                                 EL117
00244      END-EXEC.                                                    EL117
00245                                                                   EL117
00246      MOVE ZERO                   TO  WS-RECORD-COUNT.             EL117
00247                                                                   EL117
00248  0110-MAIN-LOGIC.                                                 EL117
00249      EXEC CICS STARTBR                                            EL117
00250          DATASET (WS-DSID)                                        EL117
00251          RIDFLD  (WS-KEY)                                         EL117
00252          GTEQ                                                     EL117
00253      END-EXEC.                                                    EL117
00254                                                                   EL117
00255      MOVE +1                     TO  WS-BROWSE-SW.                EL117
00256                                                                   EL117
00257  0200-MAIN-LOGIC.                                                 EL117
00258      EXEC CICS READNEXT                                           EL117
00259          DATASET (WS-DSID)                                        EL117
00260          RIDFLD  (WS-KEY)                                         EL117
00261          SET     (ADDRESS OF DFHFIOA)                                CL**4
00262      END-EXEC.                                                    EL117
00263                                                                   EL117
00264      IF WS-DSID = WS-CONTROL-FILE-DSID                            EL117
00265          IF WS-CFK-COMPANY-ID = WS-KEY-COMPANY-ID                 EL117
00266              NEXT SENTENCE                                        EL117
00267            ELSE                                                   EL117
00268              GO TO 0400-MAIN-LOGIC                                EL117
00269        ELSE                                                       EL117
00270          IF WS-KEY-COMPANY-CD = SAVE-COMPANY-CD                   EL117
00271              NEXT SENTENCE                                        EL117
00272            ELSE                                                   EL117
00273              GO TO 0400-MAIN-LOGIC.                               EL117
00274                                                                   EL117
00275      EXEC CICS ENDBR                                              EL117
00276          DATASET (WS-DSID)                                        EL117
00277      END-EXEC.                                                    EL117
00278                                                                   EL117
00279      MOVE ZERO                   TO  WS-BROWSE-SW.                EL117
00280                                                                   EL117
00281      EXEC CICS READ UPDATE                                        EL117
00282          DATASET (WS-DSID)                                        EL117
00283          RIDFLD  (WS-KEY)                                         EL117
00284          LENGTH  (WS-RECORD-LENGTH)                               EL117
00285          SET     (ADDRESS OF DFHFIOA)                                CL**4
00286      END-EXEC.                                                    EL117
00287                                                                   EL117
00288      EXEC CICS DELETE                                             EL117
00289          DATASET (WS-DSID)                                        EL117
00290      END-EXEC.                                                    EL117
00291                                                                   EL117
00292      ADD WS-RECORD-LENGTH                                         EL117
00293          WS-JOURNAL-PREFIX-LENGTH GIVING WS-JOURNAL-RECORD-LENGTH.EL117
00294                                                                   EL117
00295      ADD +1  TO  WS-RECORD-COUNT.                                 EL117
00296                                                                   EL117
00297      DIVIDE WS-RECORD-COUNT BY +50                                EL117
00298          GIVING WS-WORK REMAINDER WS-REMAINDER.                   EL117
00299                                                                   EL117
00300      IF WS-REMAINDER = ZERO                                       EL117
00301          EXEC CICS SYNCPOINT                                      EL117
00302          END-EXEC.                                                EL117
00303                                                                   EL117
00304      GO TO 0110-MAIN-LOGIC.                                       EL117
00305                                                                   EL117
00306  0400-MAIN-LOGIC.                                                 EL117
00307      IF WS-BROWSE-SW = +1                                         EL117
00308          MOVE ZERO               TO  WS-BROWSE-SW                 EL117
00309          EXEC CICS ENDBR                                          EL117
00310              DATASET (WS-DSID)                                    EL117
00311          END-EXEC.                                                EL117
00312                                                                   EL117
00313      EXEC CICS SYNCPOINT                                          EL117
00314      END-EXEC.                                                    EL117
00315                                                                   EL117
00316      MOVE 'ELXXXX DATASET SUCCESSFULLY DELETED X,XXX,XXX RECORDS' EL117
00317                                  TO  WS-TEXT-MESSAGE.             EL117
00318      MOVE WS-DSID                TO  WS-TEXT-DSID.                EL117
00319      MOVE WS-RECORD-COUNT        TO  WS-TEXT-COUNT.               EL117
00320                                                                   EL117
00321      PERFORM 2000-SEND.                                           EL117
00322                                                                   EL117
00323      IF WS-DELETE-CNTL = +2                                       EL117
00324          GO TO 0500-MAIN-LOGIC.                                   EL117
00325                                                                   EL117
00326      IF DSID-INDEX LESS THAN DSID-INDEX-MAX                       EL117
00327          SET DSID-INDEX UP BY +1                                  EL117
00328          MOVE WS-DSID-NAME (DSID-INDEX) TO  WS-DSID               EL117
00329          MOVE LOW-VALUES         TO  WS-KEY                       EL117
00330          MOVE SAVE-COMPANY-CD    TO  WS-KEY-COMPANY-CD            EL117
00331          GO TO 0100-MAIN-LOGIC.                                   EL117
00332                                                                   EL117
00333      IF WS-DELETE-CNTL = +1                                       EL117
00334          MOVE 'ELCNTL DATASET BYPASSED' TO  WS-TEXT-MESSAGE       EL117
00335          PERFORM 2000-SEND                                        EL117
00336          GO TO 0500-MAIN-LOGIC.                                   EL117
00337                                                                   EL117
00338      IF WS-DSID NOT = WS-CONTROL-FILE-DSID                        EL117
00339          MOVE WS-CONTROL-FILE-DSID   TO  WS-DSID                  EL117
00340          MOVE WS-CFK-COMPANY-ID      TO  WS-KEY                   EL117
00341          GO TO 0100-MAIN-LOGIC.                                   EL117
00342                                                                   EL117
00343  0500-MAIN-LOGIC.                                                 EL117
00344      MOVE 'COMPANY PURGE SUCCESSFULLY COMPLETED'                  EL117
00345                                  TO  WS-TEXT-MESSAGE.             EL117
00346      GO TO 1000-MAIN-LOGIC.                                       EL117
00347                                                                   EL117
00348  0800-MAIN-LOGIC.                                                 EL117
00349      MOVE 'COMPANY NOT FOUND ON CONTROL FILE' TO WS-TEXT-MESSAGE. EL117
00350      GO TO 1000-MAIN-LOGIC.                                       EL117
00351                                                                   EL117
00352  0900-MAIN-LOGIC.                                                 EL117
00353      MOVE DFHEIBLK               TO  WS-TEXT-MESSAGE.             EL117
00354                                                                   EL117
00355      EXEC CICS LINK                                               EL117
00356          PROGRAM  ('EL004')                                       EL117
00357          COMMAREA (WS-TEXT-MESSAGE)                               EL117
00358          LENGTH   (80)                                            EL117
00359      END-EXEC.                                                    EL117
00360                                                                   EL117
00361  1000-MAIN-LOGIC.                                                 EL117
00362      PERFORM 2000-SEND.                                           EL117
00363                                                                   EL117
00364      EXEC CICS RETURN                                             EL117
00365      END-EXEC.                                                    EL117
00366                                                                   EL117
00367  2000-SEND SECTION.                                               EL117
00368      MOVE WS-TEXT-MESSAGE  TO  WS-SCREEN-LINE (SCREEN-INDEX).     EL117
00369                                                                   EL117
00370      SET WS-SCREEN-LENGTH TO SCREEN-INDEX.                        EL117
00371      MULTIPLY +80 BY WS-SCREEN-LENGTH.                            EL117
00372                                                                   EL117
00373      EXEC CICS SEND                                               EL117
00374          FROM (WS-SCREEN-AREA)                                    EL117
00375          LENGTH (WS-SCREEN-LENGTH)                                EL117
00376          ERASE                                                    EL117
00377      END-EXEC.                                                    EL117
00378                                                                   EL117
00379      IF SCREEN-INDEX LESS THAN +24                                EL117
00380          SET SCREEN-INDEX UP BY +1                                EL117
00381        ELSE                                                       EL117
00382          SET SCREEN-INDEX TO +2.                                  EL117
00383                                                                   EL117
00384  2000-EXIT.                                                       EL117
00385      EXIT.                                                        EL117
00386                                                                   EL117
00387  9999-LAST-PARAGRAPH SECTION.                                     EL117
00388      GOBACK.                                                      EL117
00389                                                                   EL117
