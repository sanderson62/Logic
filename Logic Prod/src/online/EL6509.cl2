00001  ID DIVISION.                                                     06/26/96
00002                                                                   EL6509
00003  PROGRAM-ID.                 EL6509.                                 LV005
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 04/06/95 08:37:08.                    CL**4
00007 *                            VMOD=2.005.                             CL**5
00008 *                                                                 EL6509
00009 *AUTHOR.           LOGIC,INC.                                        CL**4
00010 *                  DALLAS,TEXAS.                                     CL**4
00011                                                                   EL6509
00012 *DATE-COMPILED.                                                      CL**4
00013 *SECURITY.   *****************************************************   CL**4
00014 *            *                                                   *   CL**4
00015 *            * THIS PROGRAM IS THE PROPERTY OF LOCIC, INC. *         CL**4
00016 *            *                                                   *   CL**4
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00020 *            *                                                   *   CL**4
00021 *            *****************************************************   CL**4
00022 *                                                                 EL6509
00023 *REMARKS.     TRANSACTION - EXDA - ACCOUNT NOTE MAINTENANCE.         CL**4
00022 *                                                                 EL6509
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL6509AI FILLER
110706* 110706  CR2006071700004  PEMA  ADD BRANCH LOCATIONS
110706*           AND SHIPPING ADDRESS TO ACCOUNT NOTES FILE
110209* 110209  CR2008092300001  AJRA  DELETE TEMP STORE ON INITIAL BUILD
101101******************************************************************

00025  ENVIRONMENT DIVISION.                                            EL6509
00026                                                                   EL6509
00027      EJECT                                                        EL6509
00028  DATA DIVISION.                                                   EL6509
00029  WORKING-STORAGE SECTION.                                         EL6509
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL6509
00031  77  FILLER  PIC X(32)  VALUE '*     EL6509 WORKING STORAGE   *'. EL6509
00032  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.005 ***********'.    CL**5
00033                                                                   EL6509
00034      COPY ELCSCTM.                                                EL6509
00035      COPY ELCSCRTY.                                               EL6509
00036                                                                   EL6509
00037      EJECT                                                        EL6509
00038  01  STANDARD-AREAS.                                              EL6509
00039      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         EL6509
00040      12  MAP-NAME                PIC X(8)    VALUE 'EL6509A'.     EL6509
00041      12  MAPSET-NAME             PIC X(8)    VALUE 'EL6509S '.    EL6509
00042      12  SCRN-NUMBER             PIC X(4)    VALUE '650I'.        EL6509
00043      12  TRANS-ID                PIC X(4)    VALUE 'EXDA'.        EL6509
00044      12  W-PRINT-TRANS           PIC X(4)    VALUE 'EXDB'.        EL6509
00045      12  THIS-PGM                PIC X(8)    VALUE 'EL6509'.      EL6509
00046      12  PGM-NAME                PIC X(8).                        EL6509
00047      12  PGM-EL650               PIC X(8)    VALUE 'EL650'.       EL6509
00048                                                                   EL6509
00049      12  ERACNT-FILE-ID          PIC X(8)    VALUE 'ERACNT'.      EL6509
00050      12  ELCNTL-FILE-ID          PIC X(8)    VALUE 'ELCNTL'.      EL6509
00051      12  QID.                                                     EL6509
00052          16  QID-TERM            PIC X(4).                        EL6509
00053          16  FILLER              PIC X(4)    VALUE '650I'.        EL6509
00054      12  QID-ITEM                PIC S9(4)   COMP VALUE +0.       EL6509
00055      12  SC-ITEM                 PIC S9(4)   COMP VALUE +1.       EL6509
00056                                                                   EL6509
00057  01  WORK-AREA.                                                   EL6509
00058      12  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL6509
00059      12  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL6509
00060                                                                   EL6509
00061      12  FN-CONVERT              PIC XX.                          EL6509
00062      12  FN-BIN REDEFINES FN-CONVERT  PIC S9(4)  COMP.            EL6509
00063                                                                   EL6509
00064      12  ERACNT-LENGTH           PIC S9(4)   COMP VALUE +120.     EL6509
00065      12  ERACNT-KEY-LENGTH       PIC S9(4)   COMP VALUE +23.      EL6509
00066      12  ERACNT-START-LENGTH     PIC S9(4)   COMP VALUE +21.      EL6509
00067      12  ERACNT-KEY.                                              EL6509
00068          16  ERACNT-PARTIAL-KEY.                                  EL6509
00069              20 ERACNT-COMPANY-CD    PIC X.                       EL6509
00071              20 ERACNT-CARRIER       PIC X.                       EL6509
00072              20 ERACNT-GROUPING      PIC X(06).                   EL6509
00073              20 ERACNT-STATE         PIC XX.                      EL6509
00074              20 ERACNT-ACCOUNT       PIC X(10).                   EL6509
110706             20 ERACNT-REC-TYP       PIC X.                       EL6509
00075          16 ERACNT-SEQ           PIC S9(4) COMP.                  EL6509
00076      12  SV-PRIOR-KEY.                                            EL6509
00077          20 SV-COMPANY-CD            PIC X.                       EL6509
00079          20 SV-CARRIER               PIC X.                       EL6509
00080          20 SV-GROUPING              PIC X(06).                   EL6509
00081          20 SV-STATE                 PIC XX.                      EL6509
00082          20 SV-ACCOUNT               PIC X(10).                   EL6509
110706         20 SV-REC-TYP               PIC X.                       EL6509
00083          20 SV-SEQ                   PIC S9(4) COMP.              EL6509
00084      12  W-START-BR              PIC X       VALUE SPACES.        EL6509
00085      12  VG-CNTL-KEY.                                                CL**4
00086          20 VG-CARRIER               PIC X.                          CL**4
00087          20 VG-GROUPING              PIC X(06).                      CL**4
00088          20 VG-STATE                 PIC XX.                         CL**4
00089          20 VG-ACCOUNT               PIC X(10).                      CL**4
00090      12  W-WORK-CONT.                                             EL6509
00091          16  W-WORK-PRIME        PIC X(14)   VALUE SPACES.        EL6509
00092          16  FILLER              PIC X       VALUE '-'.           EL6509
00093          16  W-WORK-SFX          PIC X       VALUE SPACES.        EL6509
00094      12  ELCNTL-KEY.                                              EL6509
00095          16  ELCNTL-COMPANY      PIC XXX.                         EL6509
00096          16  ELCNTL-REC-TYPE     PIC X.                           EL6509
00097          16  FILLER              PIC X(4).                        EL6509
00098          16  ELCNTL-SEQ-NO       PIC S9(4)   COMP.                EL6509
00099      12  TIME-IN                 PIC S9(7).                       EL6509
00100      12  TIME-SPLIT REDEFINES TIME-IN.                            EL6509
00101          16  FILLER              PIC X.                           EL6509
00102          16  TIME-OUT            PIC 99V99.                       EL6509
00103          16  FILLER              PIC 9(2).                        EL6509
00104      12  XCTL-005                PIC X(8)    VALUE 'EL005'.       EL6509
00105      12  XCTL-126                PIC X(8)    VALUE 'EL126'.       EL6509
00106      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL6509
00107      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL6509
00108      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL6509
00109      12  MAX-LINES               PIC 999     VALUE 300.           EL6509
00110      12  NUM-LINES-PER-SCREEN    PIC 99      VALUE 12.            EL6509
00111      12  TS-NUM-REC-IN-GROUP     PIC 99      VALUE 50.            EL6509
00112      12  TS-GROUP-WORK           PIC 9(5)    VALUE 0  COMP-3.     EL6509
00113      12  TS-LENGTH               PIC S9(4)   VALUE +3600 COMP.    EL6509
00114      12  ROLL-COUNTER            PIC S999    VALUE +0 COMP-3.     EL6509
00115      12  TEMP-CURR-LINE          PIC S9(3)   COMP-3.              EL6509
00116      EJECT                                                        EL6509
00117  01  ERROR-MESSAGES.                                              EL6509
00118      12  ER-0000             PIC X(04)       VALUE '0000'.        EL6509
00119      12  ER-0004             PIC X(04)       VALUE '0004'.        EL6509
00120      12  ER-0006             PIC X(04)       VALUE '0006'.        EL6509
00121      12  ER-0008             PIC X(04)       VALUE '0008'.        EL6509
00122      12  ER-0023             PIC X(04)       VALUE '0023'.        EL6509
00123      12  ER-0029             PIC X(04)       VALUE '0029'.        EL6509
00124      12  ER-0030             PIC X(04)       VALUE '0030'.        EL6509
00125      12  ER-0031             PIC X(04)       VALUE '0031'.        EL6509
00126      12  ER-0032             PIC X(04)       VALUE '0032'.        EL6509
00127      12  ER-0033             PIC X(04)       VALUE '0033'.        EL6509
00128      12  ER-0041             PIC X(04)       VALUE '0041'.        EL6509
00129      12  ER-0044             PIC X(04)       VALUE '0044'.        EL6509
00130      12  ER-0045             PIC X(04)       VALUE '0045'.        EL6509
00131      12  ER-0047             PIC X(04)       VALUE '0047'.        EL6509
00132      12  ER-0048             PIC X(04)       VALUE '0048'.        EL6509
00133      12  ER-0049             PIC X(04)       VALUE '0049'.        EL6509
00134      12  ER-0050             PIC X(04)       VALUE '0050'.        EL6509
00135      12  ER-0051             PIC X(04)       VALUE '0051'.        EL6509
00136      12  ER-0066             PIC X(04)       VALUE '0066'.        EL6509
00137      12  ER-0067             PIC X(04)       VALUE '0067'.           CL**2
00138      12  ER-0069             PIC X(04)       VALUE '0069'.        EL6509
00139      12  ER-0070             PIC X(04)       VALUE '0070'.        EL6509
00140      12  ER-0140             PIC X(04)       VALUE '0140'.        EL6509
00141      12  ER-0412             PIC X(04)       VALUE '0412'.        EL6509
00142      12  ER-0413             PIC X(04)       VALUE '0413'.        EL6509
00143      12  ER-3784             PIC X(04)       VALUE '3784'.        EL6509
00144      12  ER-3786             PIC X(04)       VALUE '3786'.        EL6509
00145      12  ER-3787             PIC X(04)       VALUE '3787'.        EL6509
00146      12  ER-3788             PIC X(04)       VALUE '3788'.           CL**2
00147      EJECT                                                        EL6509
00148                         COPY ELCLOGOF.                            EL6509
00149      EJECT                                                        EL6509
00150                         COPY ELCAID.                              EL6509
00151  01  FILLER  REDEFINES DFHAID.                                    EL6509
00152      12  FILLER                  PIC X(8).                        EL6509
00153      12  PF-VALUES OCCURS 24 TIMES       PIC X.                   EL6509
00154      EJECT                                                        EL6509
00155                         COPY ELCEMIB.                             EL6509
00156      EJECT                                                        EL6509
00157                         COPY ELCINTF.                             EL6509
00158      EJECT                                                        EL6509
00159                         COPY ELC650PI.                            EL6509
00160      EJECT                                                        EL6509
00161                            COPY ELCATTR.                          EL6509
00162      EJECT                                                        EL6509
00163                            COPY ELCDATE.                          EL6509
00164      EJECT                                                        EL6509
00165                            COPY EL6509S.                          EL6509
00166      EJECT                                                        EL6509
00167  01  EL6509R REDEFINES EL6509AI.                                  EL6509
101101     12  FILLER                  PIC X(114).                      EL6509
00169      12  SC-ALL-LINES.                                            EL6509
00170       14 SC-LINES OCCURS 12 TIMES INDEXED BY SC-INDX.             EL6509
00171          16  SC-LINL             PIC S9(4)   COMP.                EL6509
00172          16  SC-LINA             PIC X.                           EL6509
00173          16  SC-LIN              PIC Z99.                         EL6509
00174          16  SC-TEXTL            PIC S9(4)   COMP.                EL6509
00175          16  SC-TEXTA            PIC X.                           EL6509
00176          16  SC-TEXT             PIC X(60).                       EL6509
00177          16  SC-MAINT-BYL        PIC S9(4)   COMP.                EL6509
00178          16  SC-MAINT-BYA        PIC X.                           EL6509
00179          16  SC-MAINT-BY         PIC XXXX.                        EL6509
00180          16  SC-MAINT-DTL        PIC S9(4)   COMP.                EL6509
00181          16  SC-MAINT-DTA        PIC X.                           EL6509
00182          16  SC-MAINT-DT         PIC X(6).                        EL6509
00183      12  FILLER                  PIC X(23).                       EL6509
00184      EJECT                                                        EL6509
00185  01  RECORD-TABLE                PIC X(21600) VALUE SPACES.       EL6509
00186  01  REC-TABLE  REDEFINES RECORD-TABLE.                           EL6509
00187      12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-INDX PIC X(3600).  EL6509
00188  01  REC-ENTRIES REDEFINES RECORD-TABLE.                          EL6509
00189      12  REC-ENT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.    EL6509
00190          16  REC-TEXT                    PIC X(60).               EL6509
00191          16  REC-LAST-MAINT-BY           PIC XXXX.                EL6509
00192          16  REC-LAST-MAINT-DT           PIC XX.                  EL6509
00193          16  REC-LAST-MAINT-HHMMSS       PIC S9(7) COMP-3.        EL6509
00194          16  REC-PC                      PIC X(2).                EL6509
00195                                                                   EL6509
00196  01  TS-WORK-AREA                        PIC X(3600).             EL6509
00197      EJECT                                                        EL6509
00198                                                                   EL6509
00199  LINKAGE SECTION.                                                 EL6509
00200  01  DFHCOMMAREA                 PIC X(1500).                     EL6509
00201                                                                   EL6509
00204           COPY ERCACNT.                                           EL6509

00206           COPY ELCCNTL.                                           EL6509

00208  PROCEDURE DIVISION.                                              EL6509
00209                                                                   EL6509
00210      MOVE EIBTRMID               TO QID-TERM.                        CL**4
00211      MOVE EIBDATE                TO DC-JULIAN-YYDDD.                 CL**4
00212      MOVE '5'                    TO DC-OPTION-CODE.                  CL**4
00213      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6509
00214      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                      CL**4
00215      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.                  CL**4
00216                                                                   EL6509
00217      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK                  EL6509
00218      IF EIBCALEN = ZEROS                                          EL6509
00219          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6509
00220                                                                   EL6509
00221      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6509
00222         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                    EL6509
00223            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6        EL6509
00224            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5        EL6509
00225            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4        EL6509
00226            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3        EL6509
00227            MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2        EL6509
00228            MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1        EL6509
00229            MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM      EL6509
00230            MOVE THIS-PGM TO PI-CALLING-PROGRAM                    EL6509
00231         ELSE                                                      EL6509
00232            MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM        EL6509
00233            MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM      EL6509
00234            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1        EL6509
00235            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2        EL6509
00236            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3        EL6509
00237            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4        EL6509
00238            MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5        EL6509
00239            MOVE SPACES               TO PI-SAVED-PROGRAM-6.       EL6509
00240                                                                   EL6509
CIDMOD     MOVE LOW-VALUES TO EL6509AI.
00241      MOVE SPACES                 TO  ERACNT-KEY.                  EL6509
00242                                                                   EL6509
00243      IF PI-RETURN-TO-PROGRAM = PGM-EL650                          EL6509
00244         OR PI-ACCT-NOTE                                           EL6509
00245         MOVE '1'                 TO  ERACNT-REC-TYP               EL6509
00246         MOVE 'A'                 TO  PI-NOTE-TYPE                 EL6509
00247         MOVE PI-ACCT-CARRIER     TO  ERACNT-CARRIER               EL6509
00248                                      PI-CR-CARRIER                EL6509
00249                                      VG-CARRIER                      CL**4
00250         MOVE PI-ACCT-GROUPING    TO  ERACNT-GROUPING              EL6509
00251                                      VG-GROUPING                     CL**4
00252                                      PI-CR-GROUPING               EL6509
00253         MOVE PI-ACCT-STATE       TO  ERACNT-STATE                 EL6509
00254                                      PI-CR-STATE                  EL6509
00255                                      VG-STATE                        CL**4
00256         MOVE PI-ACCT-ACCOUNT     TO  ERACNT-ACCOUNT               EL6509
00257                                      PI-CR-ACCOUNT                   CL**4
00258                                      VG-ACCOUNT.                     CL**4
00259                                                                      CL**4
00260      IF ACCNT-CNTL                                                   CL**4
00261          MOVE SPACES             TO  ERACNT-CARRIER                  CL**4
00262                                      VG-CARRIER                      CL**4
00263                                      ERACNT-GROUPING                 CL**4
00264                                      VG-GROUPING                     CL**4
00265                                      ERACNT-STATE                    CL**4
00266                                      VG-STATE.                       CL**4
00267                                                                      CL**4
00268      IF ST-ACCNT-CNTL                                                CL**4
00269          MOVE SPACES             TO  ERACNT-CARRIER                  CL**4
00270                                      VG-CARRIER                      CL**4
00271                                      VG-GROUPING                     CL**4
00272                                      ERACNT-GROUPING.                CL**4
00273                                                                      CL**4
00274      IF CARR-ACCNT-CNTL                                              CL**4
00275          MOVE SPACES             TO  ERACNT-GROUPING                 CL**4
00276                                      ERACNT-STATE                    CL**4
00277                                      VG-STATE.                       CL**4
00278                                                                      CL**4
00279      IF CARR-ST-ACCNT-CNTL                                           CL**4
00280          MOVE SPACES             TO  ERACNT-GROUPING.                CL**4
00281                                                                   EL6509
00282      MOVE PI-COMPANY-CD          TO  ERACNT-COMPANY-CD.           EL6509
00283      MOVE ZEROS                  TO  ERACNT-SEQ.                  EL6509
00284      MOVE ERACNT-PARTIAL-KEY     TO  SV-PRIOR-KEY.                EL6509
00285                                                                   EL6509
00286      IF EIBTRNID NOT = TRANS-ID                                   EL6509
00287         MOVE '0'                 TO  PI-UPDATE-SW                 EL6509
00288         IF PI-PROCESSOR-ID NOT = 'LGXX'                           EL6509
00289            EXEC CICS READQ TS                                     EL6509
00290                      QUEUE (PI-SECURITY-TEMP-STORE-ID)            EL6509
00291                      INTO (SECURITY-CONTROL)                      EL6509
00292                      LENGTH (SC-COMM-LENGTH)                      EL6509
00293                      ITEM  (SC-ITEM)                              EL6509
00294                      END-EXEC                                     EL6509
00295           MOVE SC-CREDIT-DISPLAY (25) TO PI-DISPLAY-CAP           EL6509
00296           MOVE SC-CREDIT-UPDATE (25)  TO PI-MODIFY-CAP.           EL6509
00297                                                                   EL6509
00298      IF NOT DISPLAY-CAP                                           EL6509
00299          MOVE 'READ'             TO  SM-READ                      EL6509
00300          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL6509
00301          MOVE ER-0070            TO  EMI-ERROR                    EL6509
00302          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6509
00303          GO TO 8100-SEND-INITIAL-MAP.                             EL6509
00304                                                                      CL**4
00305      EXEC CICS HANDLE AID                                            CL**4
00306           CLEAR(9400-CLEAR)                                          CL**4
00307      END-EXEC.                                                       CL**4
00308                                                                   EL6509
00309      EXEC CICS HANDLE CONDITION                                   EL6509
00310           ERROR(9990-ABEND)                                       EL6509
00311           PGMIDERR(9600-PGMID-ERROR)                              EL6509
00312      END-EXEC.                                                       CL**2
00313                                                                      CL**2
00314      IF EIBTRNID NOT = TRANS-ID                                      CL**2
00315          GO TO 7000-BUILD-TABLE.                                     CL**2
00316                                                                   EL6509
00317      EJECT                                                        EL6509
00318  2000-RECEIVE.                                                    EL6509
00319      IF EIBAID = DFHPA1 OR                                        EL6509
00320         EIBAID = DFHPA2 OR                                        EL6509
00321         EIBAID = DFHPA3                                           EL6509
00322            MOVE ER-0008 TO EMI-ERROR                              EL6509
00323            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL6509
00324            GO TO 8200-SEND-DATAONLY.                              EL6509
00325                                                                   EL6509
00326      EXEC CICS RECEIVE                                            EL6509
00327           MAP(MAP-NAME)                                           EL6509
00328           MAPSET(MAPSET-NAME)                                     EL6509
00329           INTO(EL6509AI)                                          EL6509
00330           END-EXEC.                                               EL6509
00331                                                                   EL6509
00332      IF PFENTERL = ZEROS                                          EL6509
00333         GO TO 2001-CHECK-PFKEYS.                                  EL6509
00334                                                                   EL6509
00335      IF EIBAID NOT = DFHENTER                                     EL6509
00336         MOVE ER-0004             TO EMI-ERROR                        CL**4
00337         GO TO 2002-INPUT-ERROR.                                   EL6509
00338                                                                   EL6509
00339      IF PFENTERI NUMERIC AND                                      EL6509
00340         (PFENTERI > 00 AND  < 25)                                 EL6509
00341         MOVE PF-VALUES (PFENTERI) TO EIBAID                       EL6509
00342         ELSE                                                      EL6509
00343         MOVE ER-0029 TO EMI-ERROR                                 EL6509
00344         GO TO 2002-INPUT-ERROR.                                   EL6509
00345                                                                   EL6509
00346  2001-CHECK-PFKEYS.                                               EL6509
00347      IF EIBAID = DFHPF23                                          EL6509
00348         GO TO 9000-RETURN-CICS.                                   EL6509
00349                                                                   EL6509
00350      IF EIBAID = DFHPF24                                          EL6509
00351         GO TO 9200-RETURN-MAIN-MENU.                              EL6509
00352                                                                   EL6509
00353      IF EIBAID = DFHPF12                                          EL6509
00354         GO TO 9500-PF12.                                          EL6509
00355                                                                   EL6509
00356      IF FUNCTL NOT = ZEROS AND EIBAID NOT = DFHENTER              EL6509
00357         IF FUNCTI = 'A' OR = SPACES                               EL6509
00358            NEXT SENTENCE                                          EL6509
00359            ELSE                                                   EL6509
00360            MOVE ER-0050          TO EMI-ERROR                        CL**4
00361            MOVE -1 TO FUNCTL                                      EL6509
00362            MOVE AL-UABON TO FUNCTA PFENTERA                       EL6509
00363            GO TO 2002-INPUT-ERROR.                                EL6509
00364                                                                   EL6509
00365      IF EIBAID = DFHPF1                                           EL6509
00366         MOVE NUM-LINES-PER-SCREEN TO ROLL-COUNTER                 EL6509
00367         GO TO 7400-PAGE-ROUTINE.                                  EL6509
00368                                                                   EL6509
00369      IF EIBAID = DFHPF2                                           EL6509
00370         SUBTRACT NUM-LINES-PER-SCREEN FROM ROLL-COUNTER           EL6509
00371         GO TO 7400-PAGE-ROUTINE.                                  EL6509
00372                                                                   EL6509
00373      IF EIBAID = DFHPF3                                           EL6509
00374         MOVE 5                   TO ROLL-COUNTER                     CL**4
00375         GO TO 7400-PAGE-ROUTINE.                                  EL6509
00376                                                                   EL6509
00377      IF EIBAID = DFHPF4                                           EL6509
00378         MOVE -5                  TO ROLL-COUNTER                     CL**4
00379         GO TO 7400-PAGE-ROUTINE.                                  EL6509
00380                                                                   EL6509
00381      IF EIBAID = DFHPF8                                           EL6509
00382         GO TO 4500-SAVE-DATA.                                     EL6509
00383                                                                   EL6509
00384      IF EIBAID = DFHENTER                                         EL6509
00385         GO TO 2003-EDIT-DATA.                                     EL6509
00386                                                                   EL6509
00387      MOVE ER-0029                TO EMI-ERROR.                       CL**4
00388                                                                   EL6509
00389  2002-INPUT-ERROR.                                                EL6509
00390      MOVE -1                     TO PFENTERL                         CL**4
00391      MOVE AL-UNBON               TO PFENTERA                         CL**4
00392      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL6509
00393      GO TO 8200-SEND-DATAONLY.                                    EL6509
00394                                                                   EL6509
00395  2003-EDIT-DATA.                                                  EL6509
00396                                                                   EL6509
00397      IF FUNCTI = 'L'                                              EL6509
00398          NEXT SENTENCE                                            EL6509
00399      ELSE                                                         EL6509
00400          IF NOT MODIFY-CAP                                        EL6509
00401              MOVE 'UPDATE'       TO  SM-READ                         CL**4
00402              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       EL6509
00403              MOVE ER-0070        TO  EMI-ERROR                       CL**4
00404              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6509
00405              GO TO 8100-SEND-INITIAL-MAP.                         EL6509
00406                                                                   EL6509
00407      IF FUNCTL = ZEROS OR FUNCTI = SPACES                         EL6509
00408         GO TO 4000-CHANGE-ROUTINE.                                EL6509
00409                                                                   EL6509
00410      IF (FUNCTI = 'S' OR = 'D' OR = 'Q' OR                        EL6509
00411                 = 'I' OR = 'A' OR = 'L')                          EL6509
00412          NEXT SENTENCE                                            EL6509
00413      ELSE                                                         EL6509
00414          MOVE ER-0023            TO EMI-ERROR                        CL**4
00415          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6509
00416          MOVE AL-UABON           TO FUNCTA                           CL**4
00417          MOVE -1                 TO FUNCTL                           CL**4
00418          GO TO 8200-SEND-DATAONLY.                                EL6509
00419                                                                   EL6509
00420      IF FUNCTI = 'D'  OR = 'I' OR = 'L'                           EL6509
00421         PERFORM 2500-LINE-CHECK THRU 2599-EXIT                    EL6509
00422         ELSE                                                      EL6509
00423         IF LINE1L NOT = ZEROS OR                                  EL6509
00424            LINE2L NOT = ZEROS                                     EL6509
00425            MOVE ER-0030          TO EMI-ERROR                        CL**4
00426            MOVE -1               TO LINE1L                           CL**4
00427            MOVE AL-UNBON         TO LINE1A LINE2A                    CL**4
00428            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL6509
00429            GO TO 8200-SEND-DATAONLY.                              EL6509
00430                                                                      CL**4
00431      IF FUNCTI = 'A'                                              EL6509
00432         GO TO 5000-ADD-NEW-LINES.                                 EL6509
00433      IF FUNCTI = 'Q'                                              EL6509
00434         GO TO 9410-RETURN.                                        EL6509
00435      IF FUNCTI = 'S'                                              EL6509
00436         GO TO 4500-SAVE-DATA.                                     EL6509
00437      IF PI-TOTAL-LINES = 0                                        EL6509
00438         MOVE ER-0048             TO EMI-ERROR                        CL**4
00439         MOVE -1                  TO FUNCTL                           CL**4
00440         MOVE AL-UNBON            TO FUNCTA                           CL**4
00441         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
00442         GO TO 8200-SEND-DATAONLY.                                 EL6509
00443      IF FUNCTI = 'L'                                              EL6509
00444         GO TO 5500-LOOKUP.                                        EL6509
00445      IF FUNCTI = 'D'                                              EL6509
00446         GO TO 3000-DELETE-LINES.                                  EL6509
00447                                                                      CL**4
00448      GO TO 3500-INSERT-LINES.                                     EL6509
00449      EJECT                                                        EL6509
00450  2500-LINE-CHECK.                                                 EL6509
00451      IF LINE1L = ZEROS AND                                        EL6509
00452         LINE2L = ZEROS                                            EL6509
00453         MOVE ER-0069             TO EMI-ERROR                        CL**4
00454         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
00455         MOVE -1                  TO LINE1L                           CL**4
00456         GO TO 8200-SEND-DATAONLY.                                 EL6509
00457                                                                   EL6509
00458      IF LINE1L NOT = ZEROS                                        EL6509
00459         IF LINE1I NOT NUMERIC OR                                  EL6509
00460            LINE1I > PI-TOTAL-LINES                                EL6509
00461            MOVE ER-0031          TO EMI-ERROR                        CL**4
00462            MOVE AL-UNBON         TO LINE1A                           CL**4
00463            MOVE -1               TO LINE1L                           CL**4
00464            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL6509
00465            GO TO 8200-SEND-DATAONLY                               EL6509
00466          ELSE                                                     EL6509
00467            IF LINE2L = ZEROS                                      EL6509
00468               MOVE 1             TO LINE2I                           CL**4
00469               IF FUNCTI = 'I'                                     EL6509
00470                   GO TO 2510-MAX-CHECK                            EL6509
00471                ELSE NEXT SENTENCE                                 EL6509
00472            ELSE                                                   EL6509
00473               IF FUNCTI = 'I'                                     EL6509
00474                  GO TO 2510-MAX-CHECK                             EL6509
00475               ELSE                                                EL6509
00476                  IF LINE2I NOT NUMERIC                            EL6509
00477                     MOVE AL-UNBON TO LINE2A                       EL6509
00478                     MOVE ER-0032  TO EMI-ERROR                       CL**4
00479                     MOVE -1       TO LINE2L                          CL**4
00480                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT      EL6509
00481                     GO TO 8200-SEND-DATAONLY                      EL6509
00482                  ELSE                                             EL6509
00483                     NEXT SENTENCE                                 EL6509
00484      ELSE                                                         EL6509
00485         IF LINE2L = ZEROS                                         EL6509
00486            NEXT SENTENCE                                          EL6509
00487          ELSE                                                     EL6509
00488            MOVE -1               TO LINE2L                           CL**4
00489            MOVE ER-0041          TO EMI-ERROR                        CL**4
00490            MOVE AL-UNBON         TO LINE2A                           CL**4
00491            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL6509
00492            GO TO 8200-SEND-DATAONLY.                              EL6509
00493      GO TO 2599-EXIT.                                             EL6509
00494  2510-MAX-CHECK.                                                  EL6509
00495      IF LINE2I NOT NUMERIC                                        EL6509
00496         MOVE -1                  TO LINE2L                           CL**4
00497         MOVE ER-0032             TO EMI-ERROR                        CL**4
00498         MOVE AL-UNBON            TO LINE2A                           CL**4
00499         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
00500         GO TO 8200-SEND-DATAONLY                                  EL6509
00501         ELSE                                                      EL6509
00502         COMPUTE ROLL-COUNTER = LINE2I + PI-TOTAL-LINES            EL6509
00503         IF ROLL-COUNTER GREATER THAN MAX-LINES                    EL6509
00504            MOVE -1               TO LINE2L                           CL**4
00505            MOVE ER-0044          TO EMI-ERROR                        CL**4
00506            MOVE AL-UNBON         TO LINE2A                           CL**4
00507            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL6509
00508            GO TO 8200-SEND-DATAONLY.                              EL6509
00509  2599-EXIT.                                                       EL6509
00510       EXIT.                                                       EL6509
00511      EJECT                                                        EL6509
00512  3000-DELETE-LINES.                                               EL6509
00513      IF LINE2L = ZEROS AND LINE2I = 1                             EL6509
00514         MOVE LINE1I              TO LINE2I.                          CL**4
00515                                                                   EL6509
00516      IF LINE2I > PI-TOTAL-LINES OR < LINE1I                       EL6509
00517         MOVE ER-0049             TO EMI-ERROR                        CL**4
00518         MOVE AL-UNBON            TO LINE2A                           CL**4
00519         MOVE -1                  TO LINE2L                           CL**4
00520         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
00521         GO TO 8200-SEND-DATAONLY.                                 EL6509
00522                                                                      CL**4
00523      PERFORM 7450-SET-INDX THRU 7450-EXIT.                        EL6509
00524      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6509
00525              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL6509
00526              SC-INDX > NUM-LINES-PER-SCREEN                       EL6509
00527      SET TB-INDX TO LINE1I.                                       EL6509
00528                                                                      CL**4
00529      PERFORM 3050-CHECK-FOR-CURRENT-DATE                          EL6509
00530         UNTIL TB-INDX > LINE2I.                                   EL6509
00531                                                                      CL**4
00532      IF NOT EMI-NO-ERRORS                                         EL6509
00533         GO TO 8200-SEND-DATAONLY.                                 EL6509
00534                                                                      CL**4
00535      SET TB-INDX TO LINE1I                                        EL6509
00536      COMPUTE ROLL-COUNTER = LINE2I - LINE1I + 1.                  EL6509
00537                                                                      CL**4
00538      IF LINE2I NOT = PI-TOTAL-LINES                               EL6509
00539         SET TB-INDX1 TO LINE2I                                    EL6509
00540         SET TB-INDX1 UP BY 1                                      EL6509
00541         PERFORM 3100-DELETE-TABLE-ENTRIES                         EL6509
00542                 UNTIL TB-INDX1 > PI-TOTAL-LINES.                  EL6509
00543                                                                      CL**4
00544      PERFORM 3150-BLANK-TABLE-ENTRIES                             EL6509
00545              ROLL-COUNTER TIMES                                   EL6509
00546      SUBTRACT ROLL-COUNTER FROM PI-TOTAL-LINES                    EL6509
00547                                                                      CL**4
00548      IF PI-CURRENT-LINE > PI-TOTAL-LINES                          EL6509
00549         MOVE PI-TOTAL-LINES      TO PI-CURRENT-LINE                  CL**4
00550         SUBTRACT 1 FROM PI-CURRENT-LINE.                          EL6509
00551                                                                   EL6509
00552      SET TB-INDX  TO PI-CURRENT-LINE                              EL6509
00553      MOVE LOW-VALUES             TO EL6509AI                         CL**4
00554                                                                      CL**4
00555      IF PI-CURRENT-LINE > ZERO                                       CL**4
00556          PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                   CL**4
00557              VARYING SC-INDX FROM 1 BY 1 UNTIL                       CL**4
00558              SC-INDX > NUM-LINES-PER-SCREEN                          CL**4
00559          PERFORM 7200-PUT-TEMP-STOR  THRU 7249-EXIT.                 CL**4
00560                                                                      CL**4
00561      MOVE '1'                    TO PI-UPDATE-SW.                    CL**4
00562      IF PI-TOTAL-LINES = ZEROS                                    EL6509
00563         MOVE ZEROS               TO PI-CURRENT-LINE.                 CL**4
00564      GO TO 8100-SEND-INITIAL-MAP.                                 EL6509
00565      EJECT                                                        EL6509
00566                                                                   EL6509
00567  3050-CHECK-FOR-CURRENT-DATE.                                     EL6509
00568      IF (REC-LAST-MAINT-DT (TB-INDX) = SAVE-BIN-DATE)
              OR (PI-PROCESSOR-ID = 'PEMA' OR 'AJRA')
00569         NEXT SENTENCE                                             EL6509
00570      ELSE                                                         EL6509
00571         MOVE ER-3786             TO EMI-ERROR                        CL**4
00572         MOVE AL-UNBON            TO FUNCTA                           CL**4
00573         MOVE -1                  TO FUNCTL                           CL**4
00574         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
00575         GO TO 8200-SEND-DATAONLY.                                 EL6509
00576                                                                   EL6509
00577      IF REC-LAST-MAINT-BY (TB-INDX) = PI-PROCESSOR-ID             EL6509
              OR (PI-PROCESSOR-ID = 'PEMA' OR 'AJRA')
00578         NEXT SENTENCE                                             EL6509
00579      ELSE                                                         EL6509
00580         MOVE ER-3787             TO EMI-ERROR                        CL**4
00581         MOVE AL-UNBON            TO FUNCTA                           CL**4
00582         MOVE -1                  TO FUNCTL                           CL**4
00583         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
00584         GO TO 8200-SEND-DATAONLY.                                 EL6509
00585                                                                   EL6509
00586      SET TB-INDX TB-INDX1 UP BY 1.                                EL6509
00587                                                                   EL6509
00588  3100-DELETE-TABLE-ENTRIES.                                       EL6509
00589      MOVE REC-ENT (TB-INDX1)     TO REC-ENT (TB-INDX)                CL**4
00590      SET TB-INDX TB-INDX1 UP BY 1.                                EL6509
00591                                                                   EL6509
00592  3150-BLANK-TABLE-ENTRIES.                                        EL6509
00593      MOVE SPACES                 TO REC-ENT (TB-INDX).               CL**4
00594      MOVE LOW-VALUES             TO REC-LAST-MAINT-DT (TB-INDX).     CL**4
00595      MOVE ZERO                 TO REC-LAST-MAINT-HHMMSS (TB-INDX).   CL**4
00596      SET TB-INDX UP BY 1.                                         EL6509
00597      EJECT                                                        EL6509
00598  3500-INSERT-LINES.                                               EL6509
00599                                                                      CL**4
00600      PERFORM 7450-SET-INDX THRU 7450-EXIT                         EL6509
00601      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6509
00602              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL6509
00603              SC-INDX > NUM-LINES-PER-SCREEN                       EL6509
00604                                                                      CL**4
00605      IF NOT EMI-NO-ERRORS                                         EL6509
00606         GO TO 8200-SEND-DATAONLY.                                 EL6509
00607                                                                      CL**4
00608      SET TB-INDX TO PI-TOTAL-LINES                                EL6509
00609      ADD LINE2I TO PI-TOTAL-LINES                                 EL6509
00610      SET TB-INDX1 TO PI-TOTAL-LINES                               EL6509
00611      PERFORM 3600-INSERT-TABLE-ENTRIES                            EL6509
00612              UNTIL TB-INDX = LINE1I                               EL6509
00613      SET TB-INDX UP BY 1.                                            CL**4
00614                                                                   EL6509
00615      COMPUTE ROLL-COUNTER = PI-CURRENT-LINE + NUM-LINES-PER-SCREENEL6509
00616      IF TB-INDX NOT LESS THAN ROLL-COUNTER OR                     EL6509
00617                     LESS THAN PI-CURRENT-LINE                     EL6509
00618         SET SC-INDX TO 1                                          EL6509
00619         SET SC-INDX DOWN BY 1                                     EL6509
00620      ELSE                                                            CL**4
00621         SET ROLL-COUNTER TO TB-INDX                               EL6509
00622         COMPUTE ROLL-COUNTER = ROLL-COUNTER - PI-CURRENT-LINE     EL6509
00623                   + 1                                             EL6509
00624         SET SC-INDX TO ROLL-COUNTER.                              EL6509
00625                                                                   EL6509
00626      PERFORM 3150-BLANK-TABLE-ENTRIES LINE2I TIMES                EL6509
00627      SET TB-INDX TO PI-CURRENT-LINE                               EL6509
00628      MOVE LOW-VALUES             TO EL6509AI                         CL**4
00629                                                                      CL**4
00630      IF SC-INDX NOT = ZERO                                        EL6509
00631         MOVE -1 TO SC-TEXTL (SC-INDX).                            EL6509
00632                                                                      CL**4
00633      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL6509
00634             VARYING SC-INDX FROM 1 BY 1 UNTIL                     EL6509
00635             SC-INDX > NUM-LINES-PER-SCREEN                        EL6509
00636      PERFORM 7200-PUT-TEMP-STOR  THRU 7249-EXIT                   EL6509
00637      MOVE '1'                    TO PI-UPDATE-SW.                    CL**4
00638      GO TO 8100-SEND-INITIAL-MAP.                                 EL6509
00639                                                                   EL6509
00640  3600-INSERT-TABLE-ENTRIES.                                       EL6509
00641      MOVE REC-ENT (TB-INDX)      TO REC-ENT (TB-INDX1)               CL**4
00642      SET TB-INDX TB-INDX1 DOWN BY 1.                              EL6509
00643      EJECT                                                        EL6509
00644                                                                      CL**4
00645  4000-CHANGE-ROUTINE.                                             EL6509
00646      PERFORM 7450-SET-INDX THRU 7450-EXIT                         EL6509
00647      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6509
00648              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL6509
00649              SC-INDX > NUM-LINES-PER-SCREEN.                         CL**4
00650                                                                      CL**4
00651      IF NOT EMI-NO-ERRORS                                         EL6509
00652         GO TO 8200-SEND-DATAONLY.                                 EL6509
00653                                                                      CL**4
00654      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT                    EL6509
00655      MOVE SPACES                 TO ERRMSGBO                         CL**4
00656      GO TO 8200-SEND-DATAONLY.                                    EL6509
00657                                                                   EL6509
00658      EJECT                                                        EL6509
00659  4500-SAVE-DATA.                                                  EL6509
00660      PERFORM 7450-SET-INDX THRU 7450-EXIT.                           CL**4
00661      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6509
00662              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL6509
00663              SC-INDX > NUM-LINES-PER-SCREEN.                         CL**4
00664      IF NOT EMI-NO-ERRORS                                         EL6509
00665         GO TO 8200-SEND-DATAONLY.                                 EL6509
00666                                                                   EL6509
00667      EXEC CICS HANDLE CONDITION                                   EL6509
00668           NOTFND(4610-ENDBR)                                      EL6509
00669           NOTOPEN(6000-NOT-OPEN)                                  EL6509
00670           ENDFILE(4610-ENDBR)                                     EL6509
00671           END-EXEC.                                               EL6509
00672                                                                   EL6509
00673  4610-LOOP.                                                       EL6509
00674      EXEC CICS READ                                               EL6509
00675          DATASET (ERACNT-FILE-ID)                                 EL6509
00676          RIDFLD  (ERACNT-KEY)                                     EL6509
00677          SET     (ADDRESS OF NOTE-FILE)                              CL**4
00678          GTEQ                                                     EL6509
00679          END-EXEC.                                                EL6509
00680                                                                   EL6509
00681      MOVE NT-CONTROL-PRIMARY     TO ERACNT-KEY.                      CL**4
00682                                                                   EL6509
00683      IF ERACNT-PARTIAL-KEY NOT = SV-PRIOR-KEY                     EL6509
00684          MOVE SV-PRIOR-KEY       TO ERACNT-PARTIAL-KEY            EL6509
00685          GO TO 4610-ENDBR.                                        EL6509
00686                                                                   EL6509
00687      EXEC CICS DELETE                                             EL6509
00688          DATASET (ERACNT-FILE-ID)                                 EL6509
00689          RIDFLD  (ERACNT-KEY)                                     EL6509
00690          END-EXEC.                                                EL6509
00691                                                                   EL6509
00692      GO TO 4610-LOOP.                                             EL6509
00693  4610-ENDBR.                                                      EL6509
00694      EXEC CICS GETMAIN                                            EL6509
00695           LENGTH(ERACNT-LENGTH)                                   EL6509
00696           SET(ADDRESS OF NOTE-FILE)                                  CL**4
00697           INITIMG(GETMAIN-SPACE)                                  EL6509
00698           END-EXEC.                                               EL6509
00699                                                                   EL6509
00700      PERFORM 4700-WRITE-FILE THRU 4799-EXIT                       EL6509
00701              VARYING TB-INDX FROM 1 BY 1 UNTIL                    EL6509
00702              TB-INDX > PI-TOTAL-LINES.                            EL6509
00703                                                                   EL6509
00704      IF EIBAID = DFHPF8                                           EL6509
00705            GO TO 9550-START-PRINT.                                EL6509
00706                                                                   EL6509
00707      GO TO 9410-RETURN.                                           EL6509
00708                                                                   EL6509
00709  4700-WRITE-FILE.                                                 EL6509
00710      MOVE SPACES                 TO  NOTE-FILE.                      CL**4
00711      ADD 1 TO ERACNT-SEQ                                          EL6509
00712      MOVE ERACNT-KEY             TO  NT-CONTROL-PRIMARY.          EL6509
00713      MOVE  'NT'                  TO  NT-FILE-ID.                  EL6509
00714      MOVE REC-TEXT (TB-INDX)     TO  NT-NOTE-LINE                 EL6509
00715      MOVE REC-LAST-MAINT-BY (TB-INDX)                             EL6509
00716                                  TO  NT-LAST-MAINT-BY.            EL6509
00717      MOVE REC-LAST-MAINT-HHMMSS (TB-INDX)                         EL6509
00718                                  TO  NT-LAST-MAINT-HHMMSS.        EL6509
00719      MOVE REC-LAST-MAINT-DT (TB-INDX)                             EL6509
00720                                  TO  NT-LAST-MAINT-DT.            EL6509
00721                                                                   EL6509
00722      EXEC CICS WRITE                                              EL6509
00723           DATASET(ERACNT-FILE-ID)                                 EL6509
00724           FROM(NOTE-FILE)                                         EL6509
00725           RIDFLD(ERACNT-KEY)                                      EL6509
00726           END-EXEC.                                               EL6509
00727  4799-EXIT.                                                       EL6509
00728       EXIT.                                                       EL6509
00729                                                                   EL6509
00730      EJECT                                                        EL6509
00731  5000-ADD-NEW-LINES.                                              EL6509
00732      PERFORM 7450-SET-INDX THRU 7450-EXIT                         EL6509
00733      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6509
00734              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL6509
00735              SC-INDX > NUM-LINES-PER-SCREEN                       EL6509
00736                                                                      CL**4
00737      IF NOT EMI-NO-ERRORS                                         EL6509
00738         GO TO 8200-SEND-DATAONLY.                                 EL6509
00739                                                                      CL**4
00740      MOVE PI-TOTAL-LINES         TO  PI-CURRENT-LINE                 CL**4
00741      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT                    EL6509
00742      MOVE LOW-VALUES             TO  EL6509AI                        CL**4
00743      SET TB-INDX TO PI-CURRENT-LINE                               EL6509
00744      MOVE 'A'                    TO FUNCTI                           CL**4
00745      MOVE -1                     TO  SC-TEXTL (2)                    CL**4
00746      MOVE AL-UANON               TO  FUNCTA                          CL**4
00747      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL6509
00748              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL6509
00749              SC-INDX > NUM-LINES-PER-SCREEN                       EL6509
00750      MOVE '1'                              TO PI-UPDATE-SW.          CL**4
00751      GO TO 8100-SEND-INITIAL-MAP.                                 EL6509
00752      EJECT                                                        EL6509
00753  5500-LOOKUP.                                                     EL6509
00754      PERFORM 7500-READ-TS THRU 7599-EXIT                          EL6509
00755      SET TB-INDX TO PI-CURRENT-LINE                               EL6509
00756      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6509
00757              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL6509
00758              SC-INDX > NUM-LINES-PER-SCREEN                       EL6509
00759                                                                      CL**4
00760      IF NOT EMI-NO-ERRORS                                         EL6509
00761         GO TO 8200-SEND-DATAONLY.                                 EL6509
00762                                                                      CL**4
00763      MOVE LINE1I                 TO  PI-CURRENT-LINE                 CL**4
00764      SET TB-INDX TO PI-CURRENT-LINE                               EL6509
00765      MOVE LOW-VALUES             TO  EL6509AI                        CL**4
00766      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL6509
00767              VARYING SC-INDX FROM 1 BY 1                          EL6509
00768              UNTIL SC-INDX > NUM-LINES-PER-SCREEN                 EL6509
00769      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                   EL6509
00770      GO TO 8100-SEND-INITIAL-MAP.                                 EL6509
00771      EJECT                                                        EL6509
00772  6000-NOT-OPEN.                                                   EL6509
00773      MOVE ER-3788                TO  EMI-ERROR.                      CL**4
00774      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL6509
00775                                                                      CL**4
00776      IF EIBAID = DFHCLEAR                                            CL**2
00777          GO TO 9410-RETURN                                           CL**2
00778      ELSE                                                         EL6509
00779          GO TO 8100-SEND-INITIAL-MAP.                                CL**2
00780      EJECT                                                        EL6509
00781  7000-BUILD-TABLE.                                                EL6509
00782                                                                      CL**4
00783      SET TB-INDX TO 1.                                            EL6509
00784      MOVE ZEROS                  TO  PI-TOTAL-LINES                  CL**4
00785                                      PI-CURRENT-LINE                 CL**4
00786                                      PI-TEMP-STOR-ITEMS              CL**4
00787                                      PI-UPDATE-SW.                   CL**4
00788      MOVE LOW-VALUES             TO  EL6509AI.                       CL**4
00789      PERFORM 7500-READ-TS THRU 7599-EXIT                          EL6509
00790                                                                      CL**4
110209****IF TEMP STORAGE IS FOUND, DELETE IT.
00791      IF PI-TEMP-STOR-ITEMS NOT = ZERO                             EL6509
110209        PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT
110209*00792         MOVE ER-0140             TO  EMI-ERROR                       CL**4
110209*00793         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
110209*00794         MULTIPLY PI-TEMP-STOR-ITEMS BY TS-NUM-REC-IN-GROUP GIVING EL6509
110209*00795                  PI-TOTAL-LINES                                   EL6509
110209*00796         MOVE 1                   TO  PI-CURRENT-LINE                 CL**4
110209*00797         SET TB-INDX TO 1                                          EL6509
110209*00798         PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                 EL6509
110209*00799                 VARYING SC-INDX FROM 1                            EL6509
110209*00800                 BY 1 UNTIL SC-INDX > NUM-LINES-PER-SCREEN         EL6509
110209*00801         GO TO 8100-SEND-INITIAL-MAP.                              EL6509
110209     END-IF.
00802                                                                   EL6509
00803      EXEC CICS HANDLE CONDITION                                   EL6509
00804           NOTFND(7010-ENDBR)                                      EL6509
00805           NOTOPEN(6000-NOT-OPEN)                                  EL6509
00806           ENDFILE(7010-ENDBR)                                     EL6509
00807      END-EXEC.                                                    EL6509
00808                                                                   EL6509
00809      EXEC CICS STARTBR                                            EL6509
00810           DATASET(ERACNT-FILE-ID)                                 EL6509
00811           RIDFLD(ERACNT-KEY)                                      EL6509
00812           KEYLENGTH(ERACNT-START-LENGTH)                          EL6509
00813           GENERIC                                                 EL6509
00814           GTEQ                                                    EL6509
00815      END-EXEC.                                                    EL6509
00816                                                                   EL6509
00817  7001-LOOP.                                                       EL6509
00818      EXEC CICS READNEXT                                           EL6509
00819           SET(ADDRESS OF NOTE-FILE)                                  CL**4
00820           DATASET(ERACNT-FILE-ID)                                 EL6509
00821           RIDFLD(ERACNT-KEY)                                      EL6509
00822           END-EXEC.                                               EL6509
00823                                                                   EL6509
00824      IF NT-COMPANY-CD NOT = SV-COMPANY-CD                         EL6509
00825          GO TO 7010-ENDBR.                                        EL6509
00826                                                                   EL6509
00827      IF (NT-CARRIER = SV-CARRIER)
00828         AND (NT-GROUPING = SV-GROUPING)
00829         AND (NT-STATE = SV-STATE)
00830         AND (NT-ACCOUNT = SV-ACCOUNT)
110706        AND (NT-RECORD-TYPE = '1')
00831           MOVE NT-NOTE-LINE TO REC-TEXT (TB-INDX)                 EL6509
00832           MOVE NT-LAST-MAINT-BY TO REC-LAST-MAINT-BY (TB-INDX)    EL6509
00833           MOVE NT-LAST-MAINT-DT TO REC-LAST-MAINT-DT (TB-INDX)    EL6509
00834           MOVE NT-LAST-MAINT-HHMMSS TO                            EL6509
00835                          REC-LAST-MAINT-HHMMSS (TB-INDX)          EL6509
00836           SET TB-INDX UP BY 1                                     EL6509
00837           GO TO 7001-LOOP.                                        EL6509
00838  7010-ENDBR.                                                      EL6509
00839      IF TB-INDX = 1                                               EL6509
00840          MOVE ER-0006            TO EMI-ERROR                        CL**4
00841          MOVE 'A'                TO FUNCTI                           CL**4
00842          MOVE -1                 TO SC-TEXTL (1)                     CL**4
00843          MOVE AL-UANON           TO FUNCTA                           CL**4
00844          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6509
00845          MOVE ZEROS              TO PI-TOTAL-LINES                   CL**4
00846          GO TO 8100-SEND-INITIAL-MAP.                             EL6509
00847                                                                   EL6509
00848      EXEC CICS ENDBR                                              EL6509
00849           DATASET(ERACNT-FILE-ID)                                 EL6509
00850           END-EXEC.                                               EL6509
00851                                                                   EL6509
00852      SET TB-INDX DOWN BY 1.                                       EL6509
00853      SET PI-TOTAL-LINES TO TB-INDX.                               EL6509
00854      MOVE 1                      TO PI-CURRENT-LINE.                 CL**4
00855                                                                   EL6509
00856  7050-FORMAT-LINES.                                               EL6509
00857      SET TB-INDX TO PI-CURRENT-LINE.                              EL6509
00858      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL6509
00859              VARYING SC-INDX FROM 1                               EL6509
00860              BY 1 UNTIL SC-INDX > NUM-LINES-PER-SCREEN.           EL6509
00861      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT                    EL6509
00862      GO TO 8100-SEND-INITIAL-MAP.                                 EL6509
00863      EJECT                                                        EL6509
00864  7100-FORMAT-SCREEN.                                              EL6509
00865      IF TB-INDX > PI-TOTAL-LINES                                  EL6509
00866         IF FUNCTI NOT = 'A'                                       EL6509
00867            MOVE AL-PANON         TO SC-TEXTA (SC-INDX).              CL**4
00868                                                                   EL6509
00869      IF TB-INDX > PI-TOTAL-LINES                                  EL6509
00870          GO TO 7100-EXIT.                                         EL6509
00871                                                                   EL6509
00872      MOVE REC-TEXT (TB-INDX)     TO SC-TEXT (SC-INDX).               CL**4
00873      MOVE REC-LAST-MAINT-BY (TB-INDX)                                CL**4
00874                                  TO SC-MAINT-BY (SC-INDX).           CL**4
00875      PERFORM 7110-FORMAT-DATE  THRU 7110-EXIT.                    EL6509
00876      SET ROLL-COUNTER TO TB-INDX.                                 EL6509
00877      MOVE ROLL-COUNTER           TO SC-LIN (SC-INDX).                CL**4
00878                                                                   EL6509
00879      IF NOT MODIFY-CAP                                            EL6509
00880          MOVE AL-PANOF           TO SC-TEXTA (SC-INDX)               CL**4
00881          SET TB-INDX UP BY 1                                      EL6509
00882          GO TO 7100-EXIT.                                         EL6509
00883                                                                   EL6509
00884      IF (REC-LAST-MAINT-BY (TB-INDX) NOT = SPACES AND LOW-VALUES)    CL**2
00885           AND                                                     EL6509
00886         (REC-LAST-MAINT-BY (TB-INDX) NOT EQUAL PI-PROCESSOR-ID)   EL6509
                AND
              (PI-PROCESSOR-ID NOT = 'PEMA' AND 'AJRA')
00887            MOVE AL-PANOF         TO SC-TEXTA (SC-INDX)               CL**4
00888            SET TB-INDX UP BY 1                                    EL6509
00889            GO TO 7100-EXIT.                                       EL6509
00890                                                                   EL6509
00891      IF (REC-LAST-MAINT-DT (TB-INDX) = SAVE-BIN-DATE OR           EL6509
00892                               LOW-VALUES OR SPACES)               EL6509
              OR (PI-PROCESSOR-ID = 'PEMA' OR 'AJRA')
00893            SET TB-INDX UP BY 1                                    EL6509
00894         ELSE                                                      EL6509
00895            MOVE AL-PANOF         TO SC-TEXTA (SC-INDX)               CL**4
00896            SET TB-INDX UP BY 1.                                   EL6509
00897                                                                   EL6509
00898  7100-EXIT.                                                       EL6509
00899       EXIT.                                                       EL6509
00900                                                                   EL6509
00901  7110-FORMAT-DATE.                                                EL6509
00902      IF REC-LAST-MAINT-DT (TB-INDX) = LOW-VALUES OR SPACES        EL6509
00903             NEXT SENTENCE                                         EL6509
00904       ELSE                                                        EL6509
00905         MOVE REC-LAST-MAINT-DT (TB-INDX)                             CL**4
00906                                  TO DC-BIN-DATE-1                    CL**4
00907         MOVE SPACES              TO DC-OPTION-CODE                   CL**4
00908         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             EL6509
00909         MOVE DC-GREG-DATE-1-MDY  TO SC-MAINT-DT (SC-INDX).        EL6509
00910                                                                   EL6509
00911  7110-EXIT.  EXIT.                                                EL6509
00912                                                                   EL6509
00913  7200-PUT-TEMP-STOR.                                              EL6509
00914      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT                 EL6509
00915      SET TS-INDX TO 1                                             EL6509
00916      MOVE 0                      TO PI-TEMP-STOR-ITEMS               CL**4
00917      PERFORM 7300-WRITE-TS THRU 7399-EXIT                         EL6509
00918              VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP  EL6509
00919              UNTIL TS-GROUP-WORK NOT LESS THAN PI-TOTAL-LINES.    EL6509
00920  7249-EXIT.                                                       EL6509
00921       EXIT.                                                       EL6509
00922  7250-DELETE-TEMP-STOR.                                           EL6509
00923      EXEC CICS HANDLE CONDITION                                   EL6509
00924           QIDERR(7299-EXIT)                                       EL6509
00925           END-EXEC.                                               EL6509
00926      EXEC CICS DELETEQ TS                                         EL6509
00927           QUEUE(QID)                                              EL6509
00928           END-EXEC.                                               EL6509
00929  7299-EXIT.                                                       EL6509
00930      EXIT.                                                        EL6509
00931      EJECT                                                        EL6509
00932  7300-WRITE-TS.                                                   EL6509
00933      MOVE TS-GROUP (TS-INDX)     TO TS-WORK-AREA                     CL**4
00934      SET TS-INDX UP BY 1                                          EL6509
00935      ADD 1 TO PI-TEMP-STOR-ITEMS                                  EL6509
00936      EXEC CICS WRITEQ TS                                          EL6509
00937           FROM(TS-WORK-AREA)                                      EL6509
00938           QUEUE(QID)                                              EL6509
00939           LENGTH(TS-LENGTH)                                       EL6509
00940           ITEM(PI-TEMP-STOR-ITEMS)                                EL6509
00941           END-EXEC.                                               EL6509
00942  7399-EXIT.                                                       EL6509
00943      EXIT.                                                        EL6509
00944      EJECT                                                        EL6509
00945  7400-PAGE-ROUTINE.                                               EL6509
00946                                                                      CL**4
00947      IF PFENTERL NOT = ZEROS                                      EL6509
00948         MOVE -1                  TO PFENTERL                         CL**4
00949         ELSE                                                      EL6509
00950         MOVE -1                  TO FUNCTL.                          CL**4
00951                                                                      CL**4
00952      IF PI-TOTAL-LINES = 0                                        EL6509
00953         MOVE ER-0047             TO EMI-ERROR                        CL**4
00954         MOVE -1                  TO FUNCTL                           CL**4
00955         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
00956         GO TO 8200-SEND-DATAONLY.                                 EL6509
00957                                                                      CL**4
00958      COMPUTE TEMP-CURR-LINE = PI-CURRENT-LINE + ROLL-COUNTER.     EL6509
00959                                                                      CL**4
00960      IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS         EL6509
00961         MOVE ER-0067             TO EMI-ERROR                        CL**4
00962         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
00963         MOVE 1 TO TEMP-CURR-LINE.                                 EL6509
00964                                                                      CL**4
00965      IF TEMP-CURR-LINE GREATER THAN PI-TOTAL-LINES                EL6509
00966         MOVE ER-0066             TO EMI-ERROR                        CL**4
00967         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
00968         COMPUTE TEMP-CURR-LINE = PI-TOTAL-LINES + 1               EL6509
00969                                - NUM-LINES-PER-SCREEN             EL6509
00970         IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS      EL6509
00971            MOVE 1                TO TEMP-CURR-LINE.                  CL**4
00972                                                                      CL**4
00973      PERFORM 7450-SET-INDX THRU 7450-EXIT                         EL6509
00974      PERFORM 7600-UPDATE-TABLE-FROM-SCREEN THRU 7699-EXIT         EL6509
00975              VARYING SC-INDX FROM 1 BY 1 UNTIL                    EL6509
00976              SC-INDX > NUM-LINES-PER-SCREEN                       EL6509
00977                                                                      CL**4
00978      IF EMI-ERROR = ER-0066 OR = ER-0067 OR = ZEROS               EL6509
00979         NEXT SENTENCE                                             EL6509
00980         ELSE                                                      EL6509
00981         GO TO 8200-SEND-DATAONLY.                                 EL6509
00982                                                                      CL**4
00983      MOVE TEMP-CURR-LINE         TO PI-CURRENT-LINE                  CL**4
00984      SET TB-INDX TO PI-CURRENT-LINE                               EL6509
00985      MOVE LOW-VALUES             TO EL6509AI                         CL**4
00986      PERFORM 7100-FORMAT-SCREEN THRU 7100-EXIT                    EL6509
00987              VARYING SC-INDX FROM 1 BY 1                          EL6509
00988              UNTIL SC-INDX > NUM-LINES-PER-SCREEN                 EL6509
00989      PERFORM 7200-PUT-TEMP-STOR THRU 7249-EXIT.                   EL6509
00990      GO TO 8100-SEND-INITIAL-MAP.                                 EL6509
00991      EJECT                                                        EL6509
00992                                                                      CL**4
00993  7450-SET-INDX.                                                   EL6509
00994      IF PI-CURRENT-LINE = 0 AND PI-TOTAL-LINES = 0                EL6509
00995         SET TB-INDX TO 1                                          EL6509
00996      ELSE                                                         EL6509
00997         PERFORM 7500-READ-TS THRU 7599-EXIT                       EL6509
00998         IF PI-CURRENT-LINE = 0                                    EL6509
00999            SET TB-INDX TO 1                                       EL6509
01000         ELSE                                                      EL6509
01001            SET TB-INDX TO PI-CURRENT-LINE.                        EL6509
01002  7450-EXIT.                                                       EL6509
01003       EXIT.                                                       EL6509
01004      EJECT                                                        EL6509
01005  7500-READ-TS.                                                    EL6509
01006      EXEC CICS HANDLE CONDITION                                   EL6509
01007           QIDERR(7590-TS-QIDERR)                                  EL6509
01008           ITEMERR(7585-QID-ITEMERR)                               EL6509
01009           END-EXEC.                                               EL6509
01010      SET TS-INDX TO 1.                                            EL6509
01011      MOVE 1                      TO QID-ITEM.                        CL**4
01012  7501-LOOP.                                                       EL6509
01013      EXEC CICS READQ TS                                           EL6509
01014           INTO(TS-WORK-AREA)                                      EL6509
01015           QUEUE(QID)                                              EL6509
01016           LENGTH(TS-LENGTH)                                       EL6509
01017           ITEM(QID-ITEM)                                          EL6509
01018           END-EXEC.                                               EL6509
01019      MOVE TS-WORK-AREA           TO TS-GROUP (TS-INDX).              CL**4
01020      SET TS-INDX UP BY 1.                                         EL6509
01021      ADD 1 TO QID-ITEM.                                           EL6509
01022      GO TO 7501-LOOP.                                             EL6509
01023                                                                   EL6509
01024  7585-QID-ITEMERR.                                                EL6509
01025      IF EIBTRNID NOT = TRANS-ID                                   EL6509
01026         SUBTRACT 1 FROM QID-ITEM                                  EL6509
01027         MOVE QID-ITEM            TO PI-TEMP-STOR-ITEMS.              CL**4
01028         GO TO 7599-EXIT.                                          EL6509
01029                                                                   EL6509
01030  7590-TS-QIDERR.                                                  EL6509
01031      IF EIBTRNID = TRANS-ID                                       EL6509
01032         AND EIBAID = DFHCLEAR                                     EL6509
01033            GO TO 9410-RETURN.                                     EL6509
01034      IF EIBTRNID = TRANS-ID                                       EL6509
01035         MOVE ER-0033             TO EMI-ERROR                        CL**4
01036         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
01037         GO TO 8100-SEND-INITIAL-MAP.                              EL6509
01038                                                                   EL6509
01039  7599-EXIT.                                                       EL6509
01040       EXIT.                                                       EL6509
01041                                                                   EL6509
01042      EJECT                                                        EL6509
01043  7600-UPDATE-TABLE-FROM-SCREEN.                                   EL6509
01044                                                                   EL6509
01045      IF SC-TEXTL (SC-INDX) NOT = ZEROS                            EL6509
01046          IF TB-INDX NOT > PI-TOTAL-LINES                          EL6509
01047              PERFORM 7700-MOVE-DATA THRU 7700-EXIT                EL6509
01048              SET TB-INDX UP BY 1                                  EL6509
01049          ELSE                                                     EL6509
01050              IF PI-TOTAL-LINES = MAX-LINES                        EL6509
01051                  MOVE ER-0051    TO EMI-ERROR                        CL**4
01052                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6509
01053                  GO TO 8200-SEND-DATAONLY                         EL6509
01054              ELSE                                                 EL6509
01055                  PERFORM 7700-MOVE-DATA THRU 7700-EXIT            EL6509
01056                  SET TB-INDX UP BY 1                              EL6509
01057                  ADD 1 TO PI-TOTAL-LINES                          EL6509
01058      ELSE                                                         EL6509
01059         IF TB-INDX NOT > PI-TOTAL-LINES                           EL6509
01060            SET TB-INDX UP BY 1.                                   EL6509
01061                                                                   EL6509
01062  7699-EXIT.                                                       EL6509
01063       EXIT.                                                       EL6509
01064                                                                   EL6509
01065  7700-MOVE-DATA.                                                  EL6509
01066      MOVE '1'                    TO PI-UPDATE-SW.                    CL**4
01067                                                                   EL6509
01068      IF SC-TEXTL (SC-INDX) NOT = ZEROS                            EL6509
01069          MOVE SC-TEXT (SC-INDX)  TO REC-TEXT (TB-INDX).              CL**4
01070                                                                   EL6509
01071      IF REC-LAST-MAINT-DT (TB-INDX) = LOW-VALUES OR SPACES        EL6509
01072         MOVE PI-PROCESSOR-ID     TO REC-LAST-MAINT-BY (TB-INDX)      CL**4
01073                                   SC-MAINT-BY (SC-INDX)           EL6509
01074         MOVE EIBTIME            TO REC-LAST-MAINT-HHMMSS (TB-INDX)   CL**4
01075         MOVE SAVE-BIN-DATE       TO REC-LAST-MAINT-DT (TB-INDX).     CL**4
01076                                                                   EL6509
01077      PERFORM 7110-FORMAT-DATE.                                    EL6509
01078                                                                   EL6509
01079  7700-EXIT.                                                       EL6509
01080       EXIT.                                                       EL6509
01081      EJECT                                                        EL6509
01082  8100-SEND-INITIAL-MAP.                                           EL6509
01083      MOVE SAVE-DATE              TO DATEO.                           CL**4
01084      MOVE EIBTIME                TO TIME-IN.                         CL**4
01085      MOVE TIME-OUT               TO TIMEO.                           CL**4
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO USERIDO.
01086      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGBO.                        CL**4
01087      MOVE VG-CARRIER             TO CARRO.                           CL**4
01088      MOVE VG-GROUPING            TO GROUPO.                          CL**4
01089      MOVE VG-STATE               TO STATEO.                          CL**4
01090      MOVE VG-ACCOUNT             TO ACCTO.                           CL**4
01091      MOVE PI-TOTAL-LINES         TO TOTI.                         EL6509
01092      MOVE PI-ACCNAME             TO NAMEO.                        EL6509
01093      MOVE -1                     TO FUNCTL.                          CL**4
01094                                                                   EL6509
01095      EXEC CICS SEND                                               EL6509
01096           MAP(MAP-NAME)                                           EL6509
01097           MAPSET(MAPSET-NAME)                                     EL6509
01098           FROM(EL6509AO)                                          EL6509
01099           ERASE                                                   EL6509
01100           CURSOR                                                  EL6509
01101           END-EXEC.                                               EL6509
01102                                                                   EL6509
01103      GO TO 9100-RETURN-TRAN.                                      EL6509
01104                                                                   EL6509
01105  8200-SEND-DATAONLY.                                              EL6509
01106      MOVE EIBTIME                TO TIME-IN                          CL**4
01107      MOVE TIME-OUT               TO TIMEO                            CL**4
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO
101101     MOVE PI-PROCESSOR-ID        TO USERIDO
01108      MOVE PI-TOTAL-LINES         TO TOTI                             CL**4
01109                                                                   EL6509
01110      IF NOT EMI-NO-ERRORS                                         EL6509
01111         MOVE EMI-MESSAGE-AREA (1) TO ERRMSGBO                     EL6509
01112         ELSE                                                      EL6509
01113         MOVE -1                  TO FUNCTL.                          CL**4
01114                                                                      CL**4
01115      EXEC CICS SEND                                               EL6509
01116           MAP(MAP-NAME)                                           EL6509
01117           MAPSET(MAPSET-NAME)                                     EL6509
01118           FROM(EL6509AO)                                          EL6509
01119           DATAONLY                                                EL6509
01120           CURSOR                                                  EL6509
01121           END-EXEC.                                               EL6509
01122                                                                   EL6509
01123      GO TO 9100-RETURN-TRAN.                                      EL6509
01124                                                                   EL6509
01125  8300-SEND-TEXT.                                                  EL6509
01126      EXEC CICS SEND TEXT                                          EL6509
01127           FROM(LOGOFF-TEXT)                                       EL6509
01128           ERASE                                                   EL6509
01129           FREEKB                                                  EL6509
01130           LENGTH(LOGOFF-LENGTH)                                   EL6509
01131           END-EXEC.                                               EL6509
01132                                                                   EL6509
01133      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.                EL6509
01134                                                                   EL6509
01135      EXEC CICS RETURN                                             EL6509
01136           END-EXEC.                                               EL6509
01137                                                                   EL6509
01138  8800-UNAUTHORIZED-ACCESS.                                        EL6509
01139      MOVE UNACCESS-MSG           TO LOGOFF-MSG                       CL**4
01140      GO TO 8300-SEND-TEXT.                                        EL6509
01141                                                                   EL6509
01142  9000-RETURN-CICS.                                                EL6509
01143      IF PI-CHANGES-MADE                                           EL6509
01144         MOVE ER-0045             TO EMI-ERROR                        CL**4
01145         MOVE -1                  TO FUNCTL                           CL**4
01146         MOVE SPACES              TO PFENTERO                         CL**4
01147         MOVE AL-UNNOF            TO PFENTERA                         CL**4
01148         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
01149         GO TO 8200-SEND-DATAONLY.                                 EL6509
01150                                                                      CL**4
01151      MOVE EIBAID                 TO PI-ENTRY-CD-1                    CL**4
01152      MOVE XCTL-005               TO PGM-NAME.                     EL6509
01153      GO TO 9300-XCTL.                                             EL6509
01154                                                                   EL6509
01155  9100-RETURN-TRAN.                                                EL6509
01156      MOVE SCRN-NUMBER            TO PI-CURRENT-SCREEN-NO             CL**4
01157      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO                 CL**4
01158      EXEC CICS RETURN                                             EL6509
01159           TRANSID(TRANS-ID)                                       EL6509
01160           COMMAREA(PROGRAM-INTERFACE-BLOCK)                       EL6509
01161           LENGTH(PI-COMM-LENGTH)                                     CL**5
01162           END-EXEC.                                               EL6509
01163                                                                   EL6509
01164                                                                   EL6509
01165  9200-RETURN-MAIN-MENU.                                           EL6509
01166      IF PI-CHANGES-MADE                                           EL6509
01167         MOVE -1                  TO FUNCTL                        EL6509
01168         MOVE SPACES              TO PFENTERO                      EL6509
01169         MOVE AL-UNNOF            TO PFENTERA                      EL6509
01170         MOVE ER-0045             TO EMI-ERROR                        CL**4
01171         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
01172         GO TO 8200-SEND-DATAONLY.                                 EL6509
01173                                                                   EL6509
01174      MOVE XCTL-126               TO PGM-NAME.                     EL6509
01175                                                                   EL6509
01176  9300-XCTL.                                                       EL6509
01177      PERFORM 7250-DELETE-TEMP-STOR THRU 7299-EXIT.                EL6509
01178      EXEC CICS XCTL                                               EL6509
01179           PROGRAM  (PGM-NAME)                                     EL6509
01180           COMMAREA (PROGRAM-INTERFACE-BLOCK)                      EL6509
01181           LENGTH   (PI-COMM-LENGTH)                                  CL**5
01182           END-EXEC.                                               EL6509
01183                                                                   EL6509
01184  9400-CLEAR.                                                      EL6509
01185                                                                      CL**4
01186      IF PI-CHANGES-MADE                                           EL6509
01187          MOVE ER-0045            TO EMI-ERROR                        CL**4
01188          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**4
01189          IF PI-CURRENT-LINE GREATER THAN ZERO                        CL**4
01190              PERFORM 7500-READ-TS THRU 7599-EXIT                     CL**4
01191              SET TB-INDX TO PI-CURRENT-LINE                          CL**4
01192              PERFORM 7100-FORMAT-SCREEN  THRU  7100-EXIT             CL**4
01193                  VARYING SC-INDX FROM 1 BY 1 UNTIL                   CL**4
01194                  SC-INDX GREATER NUM-LINES-PER-SCREEN                CL**4
01195          END-IF                                                      CL**4
01197          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
01198                                                                   EL6509
01199  9410-RETURN.                                                     EL6509
01200      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                         CL**4
01201      GO TO 9300-XCTL.                                             EL6509
01202                                                                   EL6509
01203  9500-PF12.                                                       EL6509
01204      IF PI-CHANGES-MADE                                           EL6509
01205         MOVE -1                  TO FUNCTL                           CL**4
01206         MOVE SPACES              TO PFENTERO                         CL**4
01207         MOVE AL-UNNOF            TO PFENTERA                         CL**4
01208         MOVE ER-0045             TO EMI-ERROR                        CL**4
01209         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6509
01210         GO TO 8200-SEND-DATAONLY.                                 EL6509
01211                                                                      CL**4
01212      MOVE 'EL010'                TO PGM-NAME                         CL**4
01213      GO TO 9300-XCTL.                                             EL6509
01214                                                                   EL6509
01215      EJECT                                                        EL6509
01216  9550-START-PRINT.                                                EL6509
01217      PERFORM 7200-PUT-TEMP-STOR     THRU 7249-EXIT.               EL6509
01218                                                                   EL6509
01219      MOVE SPACES                 TO  ELCNTL-KEY.                  EL6509
01220      MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY.              EL6509
01221      MOVE '1'                    TO  ELCNTL-REC-TYPE.             EL6509
01222      MOVE ZEROS                  TO  ELCNTL-SEQ-NO.               EL6509
01223                                                                   EL6509
01224      EXEC CICS HANDLE CONDITION                                   EL6509
01225                NOTFND(9410-RETURN)                                EL6509
01226                END-EXEC.                                          EL6509
01227                                                                   EL6509
01228      EXEC CICS READ DATASET(ELCNTL-FILE-ID)                       EL6509
01229                     SET (ADDRESS OF CONTROL-FILE)                    CL**4
01230                     RIDFLD(ELCNTL-KEY)                            EL6509
01231                     END-EXEC.                                     EL6509
01232                                                                   EL6509
01233      EXEC CICS HANDLE CONDITION                                   EL6509
01234                  TERMIDERR (9560-BAD-TERMID)                      EL6509
01235                  TRANSIDERR (9570-BAD-TRANSID)                    EL6509
01236                  END-EXEC.                                        EL6509
01237                                                                   EL6509
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**5
01239 *        MOVE EIBTRMID       TO CF-FORMS-PRINTER-ID                  CL**5
01240          EXEC CICS START                                             CL**5
01241                    TRANSID (W-PRINT-TRANS)                           CL**5
01242 *                  TERMID (CF-FORMS-PRINTER-ID)                      CL**5
01243                    FROM (PROGRAM-INTERFACE-BLOCK)                    CL**5
01244                    LENGTH (PI-COMM-LENGTH)                           CL**5
01245          END-EXEC                                                    CL**5
01246      ELSE                                                            CL**5
01247          EXEC CICS START                                             CL**5
01248                    TRANSID (W-PRINT-TRANS)                           CL**5
01249                    TERMID (CF-FORMS-PRINTER-ID)                      CL**5
01250                    FROM (PROGRAM-INTERFACE-BLOCK)                    CL**5
01251                    LENGTH (PI-COMM-LENGTH)                           CL**5
01252          END-EXEC.                                                   CL**5
01253                                                                   EL6509
01254      MOVE -1 TO LINE1L.                                           EL6509
01255      MOVE ER-3784    TO EMI-ERROR                                 EL6509
01256      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6509
01257      GO TO 8200-SEND-DATAONLY.                                    EL6509
01258                                                                   EL6509
01259  9560-BAD-TERMID.                                                 EL6509
01260      MOVE ER-0412    TO EMI-ERROR                                 EL6509
01261      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6509
01262      GO TO 8200-SEND-DATAONLY.                                    EL6509
01263                                                                   EL6509
01264  9570-BAD-TRANSID.                                                EL6509
01265      MOVE ER-0413    TO EMI-ERROR                                 EL6509
01266      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6509
01267      GO TO 8200-SEND-DATAONLY.                                    EL6509
01268                                                                   EL6509
01269      EJECT                                                        EL6509
01270  9600-PGMID-ERROR.                                                EL6509
01271      EXEC CICS HANDLE CONDITION                                   EL6509
01272           PGMIDERR(8300-SEND-TEXT)                                EL6509
01273           END-EXEC.                                               EL6509
01274                                                                   EL6509
01275      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL6509
01276      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL6509
01277      MOVE XCTL-005               TO  PGM-NAME.                    EL6509
01278      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL6509
01279      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL6509
01280                                                                   EL6509
01281      GO TO 9300-XCTL.                                             EL6509
01282                                                                   EL6509
01283  9700-LINK-DATE-CONVERT.                                          EL6509
01284                                                                   EL6509
01285      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL6509
01286      EXEC CICS LINK                                               EL6509
01287          PROGRAM    (PGM-NAME)                                    EL6509
01288          COMMAREA   (DATE-CONVERSION-DATA)                        EL6509
01289          LENGTH     (DC-COMM-LENGTH)                              EL6509
01290          END-EXEC.                                                EL6509
01291                                                                   EL6509
01292  9700-EXIT.                                                       EL6509
01293      EXIT.                                                        EL6509
01294                                                                   EL6509
01295  9900-ERROR-FORMAT.                                               EL6509
01296      IF NOT EMI-ERRORS-COMPLETE                                   EL6509
01297         MOVE LINK-001            TO  PGM-NAME                     EL6509
01298         EXEC CICS LINK                                            EL6509
01299              PROGRAM(PGM-NAME)                                    EL6509
01300              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL6509
01301              LENGTH(EMI-COMM-LENGTH)                              EL6509
01302              END-EXEC.                                            EL6509
01303  9900-EXIT.       EXIT.                                           EL6509
01304                                                                   EL6509
01305  9990-ABEND.                                                      EL6509
01306      MOVE LINK-004               TO  PGM-NAME.                    EL6509
01307      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL6509
01308                                                                   EL6509
01309      EXEC CICS LINK                                               EL6509
01310          PROGRAM   (PGM-NAME)                                     EL6509
01311          COMMAREA  (EMI-LINE1)                                    EL6509
01312          LENGTH    (72)                                           EL6509
01313          END-EXEC.                                                EL6509
01314                                                                   EL6509
01315      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGBO.                    EL6509
01316      MOVE -1                     TO  FUNCTL.                         CL**2
01317                                                                   EL6509
01318      GO TO 8100-SEND-INITIAL-MAP.                                    CL**2
01319                                                                   EL6509
01320      GOBACK.                                                      EL6509
01321                                                                   EL6509
01322  9995-SECURITY-VIOLATION.                                         EL6509
01323                                  COPY ELCSCTP.                    EL6509
01324                                                                   EL6509
01325  9995-EXIT.                                                       EL6509
01326      EXIT.                                                        EL6509
