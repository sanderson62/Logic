00001  ID DIVISION.                                                     06/18/97
00002                                                                   EL6302
00003  PROGRAM-ID.                 EL6302.                                 LV009
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 02/10/96 11:32:44.                    CL**7
00007 *                            VMOD=2.009                              CL**9
00008 *                                                                 EL6302
00009 *AUTHOR.     LOGIC,INC.                                              CL**7
00010 *            DALLAS, TEXAS.                                          CL**7
00011                                                                   EL6302
00012 *DATE-COMPILED.                                                      CL**7
00013 *SECURITY.   *****************************************************   CL**7
00014 *            *                                                   *   CL**7
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00016 *            *                                                   *   CL**7
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00020 *            *                                                   *   CL**7
00021 *            *****************************************************   CL**7
00022                                                                   EL6302
00023 *REMARKS. TRANSACTION - EXA7 - NEW BUSINESS - DATA ENTRY (CANCEL).   CL**7
00024                                                                   EL6302
00025  ENVIRONMENT DIVISION.                                            EL6302
00026                                                                   EL6302
00027      EJECT                                                        EL6302
00028  DATA DIVISION.                                                   EL6302
00029  WORKING-STORAGE SECTION.                                         EL6302
00030  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL**7
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6302
00032  77  FILLER  PIC X(32)  VALUE '*    EL6302 WORKING STORAGE    *'. EL6302
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.009 *********'.    CL**9
00034                                                                   EL6302
00035      COPY ELCSCTM.                                                   CL**6
00036      COPY ELCSCRTY.                                                  CL**6
00037                                                                   EL6302
00038     EJECT                                                         EL6302
00039                                                                   EL6302
00040  01  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1900.         CL**5
00041  01  STANDARD-AREAS.                                              EL6302
00042      12  MAP-NAME                PIC X(8)    VALUE 'EL630D'.      EL6302
00043      12  MAPSET-NAME             PIC X(8)    VALUE 'EL6302S'.     EL6302
00044      12  SCREEN-NUMBER           PIC X(4)    VALUE '630C'.        EL6302
00045      12  TRANS-ID                PIC X(4)    VALUE 'EXA7'.        EL6302
00046      12  THIS-PGM                PIC X(8)    VALUE 'EL6302'.      EL6302
00047      12  PGM-NAME                PIC X(8).                        EL6302
00048      12  TIME-IN                 PIC S9(7)   VALUE ZEROS.            CL**7
00049      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6302
00050          16  FILLER              PIC X.                           EL6302
00051          16  TIME-OUT            PIC 99V99.                       EL6302
00052          16  FILLER              PIC X(2).                        EL6302
00053      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL6302
00054      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL6302
00055      12  XCTL-005                PIC X(8)    VALUE 'EL005'.       EL6302
00056      12  XCTL-010                PIC X(8)    VALUE 'EL010'.       EL6302
00057      12  XCTL-626                PIC X(8)    VALUE 'EL626'.       EL6302
00058      12  XCTL-630                PIC X(8)    VALUE 'EL630'.       EL6302
00059      12  XCTL-6301               PIC X(8)    VALUE 'EL6301'.      EL6302
00060      12  LINK-CLDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL6302
00061      12  ERPNDB-FILE-ID          PIC X(8)    VALUE 'ERPNDB'.      EL6302
00062      12  ELCNTL-FILE-ID          PIC X(8)    VALUE 'ELCNTL'.      EL6302
00063      12  ELCERT-FILE-ID          PIC X(8)    VALUE 'ELCERT'.         CL**9
00064      12  DATA-UPDATE-SW          PIC X       VALUE SPACES.        EL6302
00065          88  DATA-CHANGED                      VALUE 'Y'.         EL6302
00066      12  ERROR-SW                PIC X       VALUE SPACES.        EL6302
00067          88  EDIT-ERRORS                       VALUE 'Y'.         EL6302
00068      12  WS-DEEDIT-FIELD         PIC S9(7)V99.                    EL6302
00069      12  WS-CURRENT-DT           PIC X(8)    VALUE SPACES.        EL6302
00070      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.        EL6302
00071                                                                      CL**3
00072      12  WS-PRM-HEADER.                                              CL**3
00073          16  WS-PRM-OVERRIDE     PIC XX    VALUE SPACES.             CL**3
00074          16  FILLER              PIC X(8)  VALUE '-PREMIUM'.         CL**3
00075      12  WS-REFUND-HEADER.                                           CL**3
00076          16  WS-REFUND-OVERRIDE  PIC XX    VALUE SPACES.             CL**3
00077          16  FILLER              PIC X(7)  VALUE '-REFUND'.          CL**3
00078                                                                   EL6302
00079      12  ER-0002                 PIC X(4)  VALUE '0002'.          EL6302
00080      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL6302
00081      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL6302
00082      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL6302
00083      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL6302
00084      12  ER-2212                 PIC X(4)  VALUE '2122'.          EL6302
00085      12  ER-2233                 PIC X(4)  VALUE '2233'.          EL6302
00086      12  ER-2237                 PIC X(4)  VALUE '2237'.          EL6302
00087      12  ER-2238                 PIC X(4)  VALUE '2238'.          EL6302
00088      12  ER-2433                 PIC X(4)  VALUE '2433'.          EL6302
00089                                                                   EL6302
00090      EJECT                                                        EL6302
00091  01  ACCESS-KEYS.                                                 EL6302
00092      12  ELCNTL-KEY.                                              EL6302
00093          16  ELCNTL-COMP-ID      PIC X(3)  VALUE SPACES.          EL6302
00094          16  ELCNTL-REC-TYPE     PIC X     VALUE '1'.             EL6302
00095          16  ELCNTL-ACCESS       PIC X(4)  VALUE SPACES.          EL6302
00096          16  ELCNTL-SEQ          PIC S9(4) VALUE +0 COMP.         EL6302
00097                                                                   EL6302
00098      12  ELCNTL-RECORD-LENGTH        PIC S9(4) COMP VALUE +504.   EL6302
00099      12  ELCNTL-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +527.   EL6302
00100                                                                      CL**9
00101      12  ELCERT-KEY.                                                 CL**9
00102          16  ELCERT-COMPANY-CD       PIC X.                          CL**9
00103          16  ELCERT-CARRIER          PIC X.                          CL**9
00104          16  ELCERT-GROUPING         PIC X(6).                       CL**9
00105          16  ELCERT-STATE            PIC XX.                         CL**9
00106          16  ELCERT-ACCOUNT          PIC X(10).                      CL**9
00107          16  ELCERT-CERT-EFF-DT      PIC XX.                         CL**9
00108          16  ELCERT-CERT-NO.                                         CL**9
00109              20  ELCERT-CERT-PRIME   PIC X(10).                      CL**9
00110              20  ELCERT-CERT-SFX     PIC X.                          CL**9
00111                                                                      CL**9
00112      12  ELCERT-RECORD-LENGTH        PIC S9(4) COMP VALUE +450.      CL**9
00113                                                                   EL6302
00114      12  ERPNDB-KEY.                                              EL6302
00115          16  ERPNDB-COMP-CD       PIC X     VALUE SPACE.          EL6302
00116          16  ERPNDB-ENTRY-BATCH   PIC X(6)  VALUE SPACES.         EL6302
00117          16  ERPNDB-BATCH-SEQ     PIC S9(4) VALUE +0 COMP.        EL6302
00118          16  ERPNDB-BATCH-CHG-SEQ PIC S9(4) VALUE +0 COMP.        EL6302
00119                                                                   EL6302
00120      12  ERPNDB-RECORD-LENGTH        PIC S9(4) COMP VALUE +585.   EL6302
00121      12  ERPNDB-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +608.   EL6302
00122                                                                   EL6302
00123      12  ERPNDB-UPDATE-KEY.                                       EL6302
00124          16  ERPNDB-COMP-CODE    PIC X     VALUE SPACE.           EL6302
00125          16  ERPNDB-BATCH        PIC X(6)  VALUE SPACES.          EL6302
00126          16  ERPNDB-SEQ-NO       PIC S9(4) VALUE +0 COMP.         EL6302
00127          16  ERPNDB-CHG-SEQ-NO   PIC S9(4) VALUE +0 COMP.         EL6302
00128                                                                   EL6302
00129      EJECT                                                        EL6302
00130                                                                   EL6302
00131      COPY ELCDATE.                                                   CL**6
00132                                                                   EL6302
00133      EJECT                                                        EL6302
00134      COPY ELCLOGOF.                                                  CL**6
00135                                                                   EL6302
00136      EJECT                                                        EL6302
00137      COPY ELCATTR.                                                   CL**6
00138                                                                   EL6302
00139      EJECT                                                        EL6302
00140      COPY ELCEMIB.                                                   CL**6
00141                                                                   EL6302
00142      EJECT                                                        EL6302
00143      COPY ELCINTF.                                                   CL**6
00144      COPY ELC630PI.                                               EL6302
00145                                                                   EL6302
00146      EJECT                                                        EL6302
00147      COPY ELCJPFX.                                                   CL**6
00148                              PIC X(585).                          EL6302
00149                                                                   EL6302
00150      EJECT                                                        EL6302
00151      COPY ELCAID.                                                    CL**6
00152  01  FILLER    REDEFINES DFHAID.                                  EL6302
00153      12  FILLER              PIC X(8).                            EL6302
00154      12  PF-VALUES           PIC X       OCCURS 2.                EL6302
00155                                                                   EL6302
00156      EJECT                                                        EL6302
00157      COPY EL6302S.                                                   CL**6
00158  01  MAP-IN REDEFINES EL630DI.                                    EL6302
00159      12  FILLER                  PIC X(142).                         CL**3
00160      12  DATA-IN.                                                 EL6302
00161          16  FILLER           OCCURS 16 TIMES                     EL6302
00162                               INDEXED BY INDX.                    EL6302
00163              20  SEQ-LEN             PIC S9(4)  COMP.             EL6302
00164              20  SEQ-ATTRB           PIC X.                       EL6302
00165              20  SEQ                 PIC 9(4).                    EL6302
00166              20  CERT-LEN            PIC S9(4)  COMP.             EL6302
00167              20  CERT-ATTRB          PIC X.                       EL6302
00168              20  CERT                PIC X(11).                   EL6302
00169              20  LFPREM-LEN          PIC S9(4)  COMP.             EL6302
00170              20  LFPREM-ATTRB        PIC X.                       EL6302
00171              20  LFPREM              PIC S9(9)V99.                EL6302
00172              20  ALT-LFPREM-LEN      PIC S9(4)  COMP.             EL6302
00173              20  ALT-LFPREM-ATTRB    PIC X.                       EL6302
00174              20  ALT-LFPREM          PIC S9(9)V99.                EL6302
00175              20  AHPREM-LEN          PIC S9(4)  COMP.             EL6302
00176              20  AHPREM-ATTRB        PIC X.                       EL6302
00177              20  AHPREM              PIC S9(9)V99.                EL6302
00178              20  LFCANCEL-LEN        PIC S9(4)  COMP.             EL6302
00179              20  LFCANCEL-ATTRB      PIC X.                       EL6302
00180              20  LFCANCEL            PIC S9(9)V99.                EL6302
00181              20  AHCANCEL-LEN        PIC S9(4)  COMP.             EL6302
00182              20  AHCANCEL-ATTRB      PIC X.                       EL6302
00183              20  AHCANCEL            PIC S9(9)V99.                EL6302
00184      12  DATA-OUT REDEFINES DATA-IN.                              EL6302
00185          16  FILLER           OCCURS 16 TIMES                     EL6302
00186                               INDEXED BY ONDX.                    EL6302
00187              20  FILLER              PIC X(3).                    EL6302
00188              20  SEQ-OUT             PIC 9(4).                    EL6302
00189              20  FILLER              PIC X(3).                    EL6302
00190              20  CERT-OUT            PIC X(11).                   EL6302
00191              20  FILLER              PIC X(3).                    EL6302
00192              20  LFPREM-ED           PIC Z(7).99-.                EL6302
00193              20  FILLER              PIC X(3).                    EL6302
00194              20  ALT-LFPREM-ED       PIC Z(7).99-.                EL6302
00195              20  FILLER              PIC X(3).                    EL6302
00196              20  AHPREM-ED           PIC Z(7).99-.                EL6302
00197              20  FILLER              PIC X(3).                    EL6302
00198              20  LFCANCEL-ED         PIC Z(7).99-.                EL6302
00199              20  FILLER              PIC X(3).                    EL6302
00200              20  AHCANCEL-ED         PIC Z(7).99-.                EL6302
00201      12  FILLER                  PIC X(84).                          CL**7
00202                                                                   EL6302
00203      EJECT                                                        EL6302
00204                                                                   EL6302
00205  LINKAGE SECTION.                                                 EL6302
00206  01  DFHCOMMAREA             PIC X(1900).                            CL**5
00207                                                                   EL6302
00208      EJECT                                                        EL6302
00209      COPY ERCPNDB.                                                   CL**6
00210                                                                      CL**9
00211      EJECT                                                        EL6302
00212      COPY ELCCERT.                                                   CL**9
00213                                                                   EL6302
00214      EJECT                                                           CL**9
00215      COPY ELCCNTL.                                                   CL**6
00216                                                                   EL6302
00217      EJECT                                                        EL6302
00218                                                                   EL6302
00219  PROCEDURE DIVISION.                                              EL6302
00220                                                                   EL6302
00221      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL6302
00222                                                                   EL6302
00223      IF EIBCALEN = 0                                              EL6302
00224          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6302
00225                                                                   EL6302
00226      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL6302
00227      MOVE '5'                    TO DC-OPTION-CODE.               EL6302
00228      MOVE LINK-CLDATCV           TO PGM-NAME.                     EL6302
00229                                                                   EL6302
00230      EXEC CICS LINK                                               EL6302
00231          PROGRAM  (PGM-NAME)                                      EL6302
00232          COMMAREA (DATE-CONVERSION-DATA)                          EL6302
00233          LENGTH   (DC-COMM-LENGTH)                                EL6302
00234      END-EXEC.                                                    EL6302
00235                                                                   EL6302
00236      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL6302
00237      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                EL6302
00238                                                                   EL6302
00239      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6302
00240          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6302
00241              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL6302
00242              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL6302
00243              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL6302
00244              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL6302
00245              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL6302
00246              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL6302
00247              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL6302
00248              MOVE THIS-PGM             TO PI-CALLING-PROGRAM         CL**5
00249          ELSE                                                     EL6302
00250              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL6302
00251              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL6302
00252              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL6302
00253              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL6302
00254              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL6302
00255              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL6302
00256              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL6302
00257              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL6302
00258                                                                   EL6302
00259      MOVE LOW-VALUES             TO EL630DI.                      EL6302
00260                                                                   EL6302
00261      IF EIBTRNID NOT = TRANS-ID                                   EL6302
00262          MOVE ZEROS              TO PI-SAV-BATCH-SEQ              EL6302
00263                                     PI-SAV-BATCH-CHG-SEQ          EL6302
00264          MOVE PI-COMPANY-CD      TO ERPNDB-COMP-CODE              EL6302
00265          MOVE PI-SAV-ENTRY-BATCH TO ERPNDB-BATCH                  EL6302
00266          MOVE DFHENTER           TO EIBAID                        EL6302
00267          GO TO 2000-BROWSE-FORWARD.                               EL6302
00268                                                                   EL6302
00269      EXEC CICS HANDLE CONDITION                                   EL6302
00270          PGMIDERR  (9600-PGMID-ERROR)                             EL6302
00271          ERROR     (9990-ABEND)                                   EL6302
00272      END-EXEC.                                                    EL6302
00273                                                                   EL6302
00274      IF EIBAID = DFHCLEAR                                         EL6302
00275          GO TO 9400-CLEAR.                                        EL6302
00276                                                                   EL6302
00277      EJECT                                                        EL6302
00278                                                                   EL6302
00279  0200-RECEIVE.                                                    EL6302
00280      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6302
00281          MOVE ER-0008            TO EMI-ERROR                     EL6302
00282          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6302
00283          MOVE -1                 TO PFENTERL                      EL6302
00284          GO TO 8200-SEND-DATAONLY.                                EL6302
00285                                                                   EL6302
00286      EXEC CICS RECEIVE                                            EL6302
00287          MAP      (MAP-NAME)                                      EL6302
00288          MAPSET   (MAPSET-NAME)                                   EL6302
00289          INTO     (EL630DI)                                       EL6302
00290      END-EXEC.                                                    EL6302
00291                                                                   EL6302
00292      IF PFENTERL = 0                                              EL6302
00293          GO TO 0300-CHECK-PFKEYS.                                 EL6302
00294                                                                   EL6302
00295      IF EIBAID NOT = DFHENTER                                     EL6302
00296          MOVE ER-0004            TO EMI-ERROR                     EL6302
00297          GO TO 0320-INPUT-ERROR.                                  EL6302
00298                                                                   EL6302
00299      IF (PFENTERI NUMERIC)                                        EL6302
00300        AND (PFENTERI GREATER 0 AND LESS 25)                          CL**5
00301          MOVE PF-VALUES (PFENTERI) TO EIBAID                      EL6302
00302      ELSE                                                         EL6302
00303          MOVE ER-0029            TO EMI-ERROR                     EL6302
00304          GO TO 0320-INPUT-ERROR.                                  EL6302
00305                                                                   EL6302
00306  0300-CHECK-PFKEYS.                                               EL6302
00307      IF EIBAID = DFHPF12                                          EL6302
00308          GO TO 9500-PF12.                                         EL6302
00309                                                                   EL6302
00310      IF EIBAID = DFHENTER OR DFHPF1 OR DFHPF2                        CL**5
00311          GO TO 1000-EDIT-DATA.                                    EL6302
00312                                                                   EL6302
00313  0320-INPUT-ERROR.                                                EL6302
00314      MOVE ER-0029                TO EMI-ERROR.                    EL6302
00315      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6302
00316      MOVE AL-UNBON               TO PFENTERA.                     EL6302
00317      MOVE -1                     TO PFENTERL.                     EL6302
00318      GO TO 8200-SEND-DATAONLY.                                    EL6302
00319                                                                   EL6302
00320      EJECT                                                        EL6302
00321                                                                   EL6302
00322  1000-EDIT-DATA.                                                  EL6302
00323      IF NOT MODIFY-CAP                                            EL6302
00324          MOVE 'UPDATE'       TO SM-READ                           EL6302
00325          PERFORM 9995-SECURITY-VIOLATION                          EL6302
00326          MOVE ER-0070        TO EMI-ERROR                         EL6302
00327          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6302
00328          GO TO 8100-SEND-INITIAL-MAP.                             EL6302
00329                                                                   EL6302
00330      MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CODE.             EL6302
00331      MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-BATCH.                 EL6302
00332                                                                   EL6302
00333      IF EMI-ERROR NOT = ZEROS                                     EL6302
00334          GO TO 8200-SEND-DATAONLY.                                EL6302
00335                                                                   EL6302
00336      SET INDX                    TO 1.                            EL6302
00337                                                                   EL6302
00338  1100-EDIT-LOOP.                                                  EL6302
00339      IF LFPREM-LEN (INDX) NOT = ZEROS                             EL6302
00340          EXEC CICS BIF DEEDIT                                        CL**8
00341              FIELD(LFPREM (INDX))                                    CL**8
00342              LENGTH(11)                                              CL**8
00343          END-EXEC                                                    CL**8
00344          MOVE 'Y'                TO DATA-UPDATE-SW.                  CL**2
00345                                                                   EL6302
00346      IF ALT-LFPREM-LEN (INDX) NOT = ZEROS                         EL6302
00347          EXEC CICS BIF DEEDIT                                        CL**8
00348              FIELD(ALT-LFPREM (INDX))                                CL**8
00349              LENGTH(11)                                              CL**8
00350          END-EXEC                                                    CL**8
00351          MOVE 'Y'                TO DATA-UPDATE-SW.                  CL**2
00352                                                                   EL6302
00353      IF LFCANCEL-LEN (INDX) NOT = ZEROS                           EL6302
00354          EXEC CICS BIF DEEDIT                                        CL**8
00355              FIELD(LFCANCEL (INDX))                                  CL**8
00356              LENGTH(11)                                              CL**8
00357          END-EXEC                                                    CL**8
00358          MOVE 'Y'                TO DATA-UPDATE-SW.               EL6302
00359                                                                   EL6302
00360      IF AHPREM-LEN (INDX) NOT = ZEROS                             EL6302
00361          EXEC CICS BIF DEEDIT                                        CL**8
00362              FIELD(AHPREM (INDX))                                    CL**8
00363              LENGTH(11)                                              CL**8
00364          END-EXEC                                                    CL**8
00365          MOVE 'Y'                TO DATA-UPDATE-SW.                  CL**2
00366                                                                   EL6302
00367      IF AHCANCEL-LEN (INDX) NOT = ZEROS                           EL6302
00368          EXEC CICS BIF DEEDIT                                        CL**8
00369              FIELD(AHCANCEL (INDX))                                  CL**8
00370              LENGTH(11)                                              CL**8
00371          END-EXEC                                                    CL**8
00372          MOVE 'Y'                TO DATA-UPDATE-SW.               EL6302
00373                                                                   EL6302
00374      IF DATA-CHANGED AND EMI-ERROR = ZEROS                        EL6302
00375          PERFORM 5000-UPDATE-PNDB-FILE THRU 5990-EXIT.               CL**2
00376                                                                      CL**2
00377      MOVE SPACE                  TO DATA-UPDATE-SW.                  CL**2
00378                                                                   EL6302
00379      IF EMI-ERROR NOT = ZEROS                                     EL6302
00380          MOVE 'Y'                TO ERROR-SW                      EL6302
00381          MOVE ZEROS              TO EMI-ERROR.                    EL6302
00382                                                                   EL6302
00383      IF INDX LESS THAN 16                                         EL6302
00384          SET INDX UP BY 1                                         EL6302
00385          GO TO 1100-EDIT-LOOP.                                    EL6302
00386                                                                   EL6302
00387      IF EDIT-ERRORS                                               EL6302
00388          GO TO 8200-SEND-DATAONLY.                                EL6302
00389                                                                   EL6302
00390      IF EIBAID = DFHPF1 OR DFHENTER                               EL6302
00391          GO TO 2000-BROWSE-FORWARD                                EL6302
00392      ELSE                                                         EL6302
00393          GO TO 3000-BROWSE-BACKWARD.                              EL6302
00394                                                                   EL6302
00395      EJECT                                                        EL6302
00396                                                                   EL6302
00397  2000-BROWSE-FORWARD.                                             EL6302
00398      IF PI-BATCH-EOF                                              EL6302
00399          MOVE SPACE              TO PI-BATCH-EOF-SW               EL6302
00400          GO TO 2100-SKIP-ADD.                                     EL6302
00401                                                                   EL6302
00402      IF PI-SAV-BATCH-SEQ NOT = ZEROS                              EL6302
00403          ADD +16                 TO PI-SAV-BATCH-SEQ.             EL6302
00404                                                                   EL6302
00405  2100-SKIP-ADD.                                                   EL6302
00406      PERFORM 6000-PNDB-START-BROWSE THRU 6090-EXIT.               EL6302
00407      IF EMI-ERROR NOT = ZEROS                                     EL6302
00408          GO TO 8200-SEND-DATAONLY.                                EL6302
00409                                                                   EL6302
00410      PERFORM 4000-FORMAT-SCREEN THRU 4990-EXIT.                   EL6302
00411                                                                   EL6302
00412      IF EMI-ERROR = ZEROS                                         EL6302
00413          GO TO 8100-SEND-INITIAL-MAP                              EL6302
00414      ELSE                                                         EL6302
00415          GO TO 8200-SEND-DATAONLY.                                EL6302
00416                                                                   EL6302
00417  3000-BROWSE-BACKWARD.                                            EL6302
00418      SUBTRACT 16                 FROM PI-SAV-BATCH-SEQ.           EL6302
00419                                                                   EL6302
00420      IF PI-SAV-BATCH-SEQ LESS THAN 1                              EL6302
00421          MOVE +1                 TO PI-SAV-BATCH-SEQ              EL6302
00422          MOVE ER-2238            TO EMI-ERROR                     EL6302
00423          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6302
00424          MOVE ZEROS              TO EMI-ERROR.                    EL6302
00425                                                                   EL6302
00426      PERFORM 6000-PNDB-START-BROWSE THRU 6090-EXIT.               EL6302
00427                                                                   EL6302
00428      IF EMI-ERROR NOT = ZEROS                                     EL6302
00429          GO TO 8200-SEND-DATAONLY.                                EL6302
00430                                                                   EL6302
00431      PERFORM 4000-FORMAT-SCREEN THRU 4990-EXIT.                   EL6302
00432                                                                   EL6302
00433      IF EMI-ERROR = ZEROS                                         EL6302
00434          GO TO 8100-SEND-INITIAL-MAP                              EL6302
00435      ELSE                                                         EL6302
00436          GO TO 8200-SEND-DATAONLY.                                EL6302
00437      EJECT                                                        EL6302
00438                                                                   EL6302
00439  4000-FORMAT-SCREEN.                                              EL6302
00440      MOVE LOW-VALUES             TO DATA-OUT.                     EL6302
00441      SET ONDX                    TO 1.                            EL6302
00442                                                                   EL6302
00443  4100-FORMAT-LOOP.                                                EL6302
00444      PERFORM 6100-PNDB-READ-NEXT THRU 6190-EXIT.                  EL6302
00445                                                                   EL6302
00446      IF PB-COMPANY-CD  = PI-SAV-COMP-CD   AND                        CL**5
00447         PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                          CL**5
00448          NEXT SENTENCE                                            EL6302
00449      ELSE                                                         EL6302
00450          MOVE ER-2237            TO EMI-ERROR                     EL6302
00451          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6302
00452          MOVE ZEROS              TO EMI-ERROR                     EL6302
00453          PERFORM 6200-PNDB-END-BROWSE THRU 6290-EXIT              EL6302
00454          PERFORM 7000-PROTECT-FIELDS VARYING INDX FROM ONDX       EL6302
00455                                   BY 1 UNTIL INDX GREATER 16         CL**5
00456          MOVE 'Y'                TO PI-BATCH-EOF-SW               EL6302
00457          GO TO 4990-EXIT.                                         EL6302
00458                                                                   EL6302
00459      IF ONDX = 1                                                  EL6302
00460          MOVE PB-BATCH-SEQ-NO    TO PI-SAV-BATCH-SEQ.             EL6302
00461                                                                   EL6302
00462      MOVE PB-BATCH-SEQ-NO        TO SEQ-OUT (ONDX).               EL6302
00463      SET INDX                    TO ONDX.                         EL6302
00464      MOVE AL-SANON               TO SEQ-ATTRB (INDX).             EL6302
00465      MOVE PB-CERT-NO             TO CERT-OUT (ONDX).              EL6302
00466                                                                      CL**5
00467      IF PB-ISSUE                                                  EL6302
00468          NEXT SENTENCE                                            EL6302
00469      ELSE                                                         EL6302
00470          GO TO 4200-FORMAT-CANCEL.                                EL6302
00471                                                                   EL6302
00472      IF PB-I-LF-PREMIUM-AMT NOT = ZEROS                           EL6302
00473          MOVE PB-I-LF-PREMIUM-AMT TO LFPREM-ED (ONDX).            EL6302
00474                                                                   EL6302
00475      IF PB-I-LF-ALT-PREMIUM-AMT NOT = ZEROS                       EL6302
00476          MOVE PB-I-LF-ALT-PREMIUM-AMT TO ALT-LFPREM-ED (ONDX).    EL6302
00477                                                                   EL6302
00478      IF PB-I-AH-PREMIUM-AMT NOT = ZEROS                           EL6302
00479          MOVE PB-I-AH-PREMIUM-AMT TO AHPREM-ED (ONDX).            EL6302
00480                                                                   EL6302
00481      MOVE AL-SANOF               TO LFCANCEL-ATTRB (INDX)         EL6302
00482                                     AHCANCEL-ATTRB (INDX).        EL6302
00483                                                                   EL6302
00484      GO TO 4300-INDX-CHECK.                                       EL6302
00485                                                                   EL6302
00486  4200-FORMAT-CANCEL.                                              EL6302
00487      IF PB-C-LF-CANCEL-AMT NOT = ZEROS                            EL6302
00488          MOVE PB-C-LF-CANCEL-AMT TO LFCANCEL-ED (ONDX).           EL6302
00489                                                                   EL6302
00490      IF PB-C-AH-CANCEL-AMT NOT = ZEROS                            EL6302
00491          MOVE PB-C-AH-CANCEL-AMT TO AHCANCEL-ED (ONDX).           EL6302
00492                                                                   EL6302
00493      MOVE AL-SANOF               TO LFPREM-ATTRB (INDX)           EL6302
00494                                     ALT-LFPREM-ATTRB (INDX)       EL6302
00495                                     AHPREM-ATTRB (INDX).          EL6302
00496                                                                   EL6302
00497  4300-INDX-CHECK.                                                 EL6302
00498      IF ONDX LESS THAN 16                                         EL6302
00499          SET ONDX UP BY 1                                         EL6302
00500          GO TO 4100-FORMAT-LOOP.                                  EL6302
00501                                                                   EL6302
00502      PERFORM 6200-PNDB-END-BROWSE THRU 6290-EXIT.                 EL6302
00503                                                                   EL6302
00504  4990-EXIT.                                                       EL6302
00505      EXIT.                                                        EL6302
00506      EJECT                                                        EL6302
00507                                                                   EL6302
00508  5000-UPDATE-PNDB-FILE.                                           EL6302
00509      MOVE SEQ (INDX)             TO ERPNDB-SEQ-NO.                EL6302
00510      EXEC CICS HANDLE CONDITION                                   EL6302
00511          NOTFND (5800-REC-NOTFND)                                 EL6302
00512      END-EXEC.                                                    EL6302
00513                                                                   EL6302
00514      EXEC CICS READ                                               EL6302
00515          SET     (ADDRESS OF PENDING-BUSINESS)                       CL**7
00516          DATASET (ERPNDB-FILE-ID)                                 EL6302
00517          RIDFLD  (ERPNDB-UPDATE-KEY)                              EL6302
00518          UPDATE                                                   EL6302
00519      END-EXEC.                                                    EL6302
00520                                                                   EL6302
00521      MOVE 'B'                    TO JP-RECORD-TYPE.               EL6302
00522      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               EL6302
00523      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6302
00524                                                                   EL6302
00525      IF NOT PB-ISSUE                                              EL6302
00526          GO TO 5100-UPDATE-CANCEL.                                EL6302
00527                                                                   EL6302
00528      IF LFPREM-LEN (INDX) NOT = ZEROS                                CL**9
00529          SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED      EL6302
00530          ADD LFPREM (INDX)       TO PI-LF-ISS-ENTERED             EL6302
00531          MOVE LFPREM (INDX)      TO PB-I-LF-PREMIUM-AMT.          EL6302
00532                                                                   EL6302
00533      IF ALT-LFPREM-LEN (INDX) NOT = ZEROS                            CL**9
00534          SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED  EL6302
00535          ADD  ALT-LFPREM (INDX)   TO PI-LF-ISS-ENTERED            EL6302
00536          MOVE ALT-LFPREM (INDX)   TO PB-I-LF-ALT-PREMIUM-AMT.     EL6302
00537                                                                   EL6302
00538      IF AHPREM-LEN (INDX) NOT = ZEROS                             EL6302
00539          SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED      EL6302
00540          ADD AHPREM (INDX)       TO PI-AH-ISS-ENTERED             EL6302
00541          MOVE AHPREM (INDX)      TO PB-I-AH-PREMIUM-AMT.          EL6302
00542                                                                   EL6302
00543      GO TO 5200-PNDB-REWRITE.                                     EL6302
00544                                                                   EL6302
00545  5100-UPDATE-CANCEL.                                              EL6302
00546      IF LFCANCEL-LEN (INDX) NOT = ZEROS                           EL6302
00547          SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED       EL6302
00548          ADD LFCANCEL (INDX)       TO PI-LF-CAN-ENTERED           EL6302
00549          MOVE LFCANCEL (INDX)      TO PB-C-LF-CANCEL-AMT.         EL6302
00550                                                                   EL6302
00551      IF AHCANCEL-LEN (INDX) NOT = ZEROS                           EL6302
00552          SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED       EL6302
00553          ADD AHCANCEL (INDX)       TO PI-AH-CAN-ENTERED           EL6302
00554          MOVE AHCANCEL (INDX)      TO PB-C-AH-CANCEL-AMT.         EL6302
00555                                                                      CL**9
00556      IF PI-COMPANY-ID NOT = 'DMD'                                    CL**9
00557          GO TO 5200-PNDB-REWRITE.                                    CL**9
00558                                                                      CL**9
00559      EXEC CICS HANDLE CONDITION                                      CL**9
00560          NOTFND (5200-PNDB-REWRITE)                                  CL**9
00561      END-EXEC.                                                       CL**9
00562                                                                      CL**9
00563      MOVE PB-CONTROL-BY-ACCOUNT  TO  ELCERT-KEY.                     CL**9
00564                                                                      CL**9
00565      EXEC CICS READ                                                  CL**9
00566          SET     (ADDRESS OF CERTIFICATE-MASTER)                     CL**9
00567          DATASET (ELCERT-FILE-ID)                                    CL**9
00568          RIDFLD  (ELCERT-KEY)                                        CL**9
00569          LENGTH  (ELCERT-RECORD-LENGTH)                              CL**9
00570      END-EXEC.                                                       CL**9
00571                                                                      CL**9
00572      MOVE CM-POLICY-FORM-NO      TO PB-C-POLICY-FORM-NO.             CL**9
00573                                                                   EL6302
00574  5200-PNDB-REWRITE.                                               EL6302
00575      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             EL6302
00576      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         EL6302
00577      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             EL6302
00578      MOVE 'C'                    TO JP-RECORD-TYPE.               EL6302
00579      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               EL6302
00580                                                                   EL6302
00581      EXEC CICS REWRITE                                            EL6302
00582          DATASET(ERPNDB-FILE-ID)                                  EL6302
00583          FROM   (PENDING-BUSINESS)                                EL6302
00584      END-EXEC.                                                    EL6302
00585                                                                   EL6302
00586      MOVE 'Y'                    TO PI-UPDATE-SW.                 EL6302
00587      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6302
00588      GO TO 5990-EXIT.                                             EL6302
00589                                                                   EL6302
00590  5800-REC-NOTFND.                                                 EL6302
00591      MOVE ER-2433                TO EMI-ERROR                     EL6302
00592      MOVE -1                     TO LFPREM-LEN (INDX).            EL6302
00593      MOVE AL-SANOF               TO SEQ-ATTRB (INDX).             EL6302
00594      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6302
00595                                                                   EL6302
00596  5990-EXIT.                                                       EL6302
00597      EXIT.                                                        EL6302
00598      EJECT                                                        EL6302
00599  6000-PNDB-START-BROWSE.                                          EL6302
00600      MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.               EL6302
00601      MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.           EL6302
00602      MOVE PI-SAV-BATCH-SEQ       TO ERPNDB-BATCH-SEQ.             EL6302
00603      EXEC CICS HANDLE CONDITION                                   EL6302
00604          NOTFND (6010-REC-NOT-FND)                                EL6302
00605      END-EXEC.                                                    EL6302
00606                                                                   EL6302
00607      EXEC CICS STARTBR                                            EL6302
00608          DATASET(ERPNDB-FILE-ID)                                  EL6302
00609          RIDFLD (ERPNDB-KEY)                                      EL6302
00610      END-EXEC.                                                    EL6302
00611                                                                   EL6302
00612      GO TO 6090-EXIT.                                             EL6302
00613                                                                   EL6302
00614  6010-REC-NOT-FND.                                                EL6302
00615      MOVE -1                     TO PFENTERL.                     EL6302
00616      MOVE ER-2212                TO EMI-ERROR.                    EL6302
00617      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6302
00618                                                                   EL6302
00619  6090-EXIT.                                                       EL6302
00620      EXIT.                                                        EL6302
00621      EJECT                                                        EL6302
00622  6100-PNDB-READ-NEXT.                                             EL6302
00623      EXEC CICS HANDLE CONDITION                                   EL6302
00624          ENDFILE (6110-END-OF-FILE)                               EL6302
00625      END-EXEC.                                                    EL6302
00626                                                                   EL6302
00627      EXEC CICS READNEXT                                           EL6302
00628          SET     (ADDRESS OF PENDING-BUSINESS)                       CL**7
00629          DATASET (ERPNDB-FILE-ID)                                 EL6302
00630          RIDFLD  (ERPNDB-KEY)                                     EL6302
00631      END-EXEC.                                                    EL6302
00632                                                                   EL6302
00633      IF PB-BATCH-TRAILER                                          EL6302
00634          MOVE HIGH-VALUES        TO PB-CONTROL-PRIMARY.           EL6302
00635                                                                   EL6302
00636      GO TO 6190-EXIT.                                             EL6302
00637                                                                   EL6302
00638  6110-END-OF-FILE.                                                EL6302
00639      MOVE HIGH-VALUES            TO PB-CONTROL-PRIMARY.           EL6302
00640                                                                   EL6302
00641  6190-EXIT.                                                       EL6302
00642      EXIT.                                                        EL6302
00643      EJECT                                                        EL6302
00644  6200-PNDB-END-BROWSE.                                            EL6302
00645      EXEC CICS ENDBR                                              EL6302
00646          DATASET (ERPNDB-FILE-ID)                                 EL6302
00647      END-EXEC.                                                    EL6302
00648                                                                   EL6302
00649  6290-EXIT.                                                       EL6302
00650      EXIT.                                                        EL6302
00651      EJECT                                                        EL6302
00652  7000-PROTECT-FIELDS.                                             EL6302
00653      MOVE AL-SANOF               TO SEQ-ATTRB        (INDX)          CL**5
00654                                     LFPREM-ATTRB     (INDX)          CL**5
00655                                     ALT-LFPREM-ATTRB (INDX)       EL6302
00656                                     AHPREM-ATTRB     (INDX)          CL**5
00657                                     LFCANCEL-ATTRB   (INDX)          CL**5
00658                                     AHCANCEL-ATTRB   (INDX).         CL**5
00659      EJECT                                                        EL6302
00660  8100-SEND-INITIAL-MAP.                                           EL6302
00661      MOVE PI-SAV-ENTRY-BATCH     TO BATCHO.                       EL6302
00662      MOVE PI-SAV-CARRIER         TO CARRIERO.                     EL6302
00663      MOVE PI-SAV-GROUPING        TO GROUPO.                       EL6302
00664      MOVE PI-SAV-STATE           TO STATEO.                       EL6302
00665      MOVE PI-SAV-ACCOUNT         TO ACCOUNTO.                     EL6302
00666      MOVE WS-CURRENT-DT          TO DATEO.                        EL6302
00667      MOVE EIBTIME                TO TIME-IN.                      EL6302
00668      MOVE TIME-OUT               TO TIMEO.                        EL6302
00669      MOVE -1                     TO LFPRE1L.                      EL6302
00670      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL6302
00671                                                                      CL**3
00672      MOVE PI-LIFE-OVERRIDE-L2    TO WS-PRM-OVERRIDE                  CL**3
00673                                     WS-REFUND-OVERRIDE.              CL**3
00674      MOVE WS-PRM-HEADER          TO LFPMHDO.                         CL**3
00675      MOVE WS-REFUND-HEADER       TO LFRFHDO.                         CL**3
00676                                                                      CL**3
00677      MOVE PI-AH-OVERRIDE-L2      TO WS-PRM-OVERRIDE                  CL**3
00678                                     WS-REFUND-OVERRIDE.              CL**3
00679      MOVE WS-PRM-HEADER          TO AHPMHDO.                         CL**3
00680      MOVE WS-REFUND-HEADER       TO AHRFHDO.                         CL**3
00681                                                                      CL**3
00682      EXEC CICS SEND                                               EL6302
00683          MAP      (MAP-NAME)                                      EL6302
00684          MAPSET   (MAPSET-NAME)                                   EL6302
00685          FROM     (EL630DO)                                       EL6302
00686          ERASE                                                    EL6302
00687          CURSOR                                                   EL6302
00688      END-EXEC.                                                    EL6302
00689                                                                   EL6302
00690      GO TO 9100-RETURN-TRAN.                                      EL6302
00691                                                                   EL6302
00692  8200-SEND-DATAONLY.                                              EL6302
00693      MOVE WS-CURRENT-DT          TO DATEO.                        EL6302
00694      MOVE EIBTIME                TO TIME-IN.                      EL6302
00695      MOVE TIME-OUT               TO TIMEO.                        EL6302
00696      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL6302
00697      EXEC CICS SEND                                               EL6302
00698          MAP      (MAP-NAME)                                      EL6302
00699          MAPSET   (MAPSET-NAME)                                   EL6302
00700          FROM     (EL630DO)                                       EL6302
00701          DATAONLY                                                 EL6302
00702          ERASEAUP                                                 EL6302
00703          CURSOR                                                   EL6302
00704      END-EXEC.                                                    EL6302
00705                                                                   EL6302
00706      GO TO 9100-RETURN-TRAN.                                      EL6302
00707                                                                   EL6302
00708  8300-SEND-TEXT.                                                  EL6302
00709      EXEC CICS SEND TEXT                                          EL6302
00710          FROM     (LOGOFF-TEXT)                                   EL6302
00711          LENGTH   (LOGOFF-LENGTH)                                 EL6302
00712          ERASE                                                    EL6302
00713          FREEKB                                                   EL6302
00714      END-EXEC.                                                    EL6302
00715      EXEC CICS RETURN                                             EL6302
00716      END-EXEC.                                                    EL6302
00717                                                                   EL6302
00718  8400-LOG-JOURNAL-RECORD.                                         EL6302
00719      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL6302
00720      MOVE ERPNDB-FILE-ID         TO JP-FILE-ID.                   EL6302
00721      MOVE THIS-PGM               TO JP-PROGRAM-ID.                EL6302
00722                                                                   EL6302
00723 *    EXEC CICS JOURNAL                                            EL6302
00724 *        JFILEID     (PI-JOURNAL-FILE-ID)                         EL6302
00725 *        JTYPEID     ('EL')                                       EL6302
00726 *        FROM        (JOURNAL-RECORD)                             EL6302
00727 *        LENGTH      (503)                                        EL6302
00728 *        END-EXEC.                                                EL6302
00729                                                                   EL6302
00730  8600-DEEDIT.                                                     EL6302
00731      EXEC CICS BIF DEEDIT                                         EL6302
00732          FIELD(WS-DEEDIT-FIELD)                                   EL6302
00733          LENGTH(09)                                               EL6302
00734      END-EXEC.                                                    EL6302
00735                                                                   EL6302
00736  8600-EXIT.                                                       EL6302
00737      EXIT.                                                        EL6302
00738  8800-UNAUTHORIZED-ACCESS.                                        EL6302
00739      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL6302
00740      GO TO 8300-SEND-TEXT.                                        EL6302
00741                                                                   EL6302
CIDMOD 8810-PF23.                                                            000
CIDMOD     MOVE EIBAID                 TO PI-ENTRY-CD-1.                     000
CIDMOD     MOVE XCTL-005               TO PGM-NAME.                          000
CIDMOD     GO TO 9300-XCTL.                                                  000
CIDMOD                                                                       000
CIDMOD 9000-RETURN-CICS.                                                     000
CIDMOD     EXEC CICS RETURN                                                  000
CIDMOD     END-EXEC.                                                         000
CIDMOD                                                                       000
00742  9100-RETURN-TRAN.                                                EL6302
00743      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL6302
00744      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         EL6302
00745      EXEC CICS RETURN                                             EL6302
00746          TRANSID    (TRANS-ID)                                    EL6302
00747          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6302
00748          LENGTH     (WS-COMM-LENGTH)                                 CL**5
00749      END-EXEC.                                                    EL6302
00750                                                                   EL6302
00751  9200-RETURN-MAIN-MENU.                                           EL6302
00752      MOVE XCTL-626               TO PGM-NAME.                     EL6302
00753      GO TO 9300-XCTL.                                             EL6302
00754                                                                   EL6302
00755  9300-XCTL.                                                       EL6302
00756      EXEC CICS XCTL                                               EL6302
00757          PROGRAM    (PGM-NAME)                                    EL6302
00758          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6302
00759          LENGTH     (WS-COMM-LENGTH)                                 CL**5
00760      END-EXEC.                                                    EL6302
00761                                                                   EL6302
00762  9400-CLEAR.                                                      EL6302
00763      MOVE XCTL-630               TO PGM-NAME.                     EL6302
00764      GO TO 9300-XCTL.                                             EL6302
00765                                                                   EL6302
00766  9500-PF12.                                                       EL6302
00767      MOVE XCTL-010               TO PGM-NAME.                     EL6302
00768      GO TO 9300-XCTL.                                             EL6302
00769                                                                   EL6302
00770  9600-PGMID-ERROR.                                                EL6302
00771      EXEC CICS HANDLE CONDITION                                   EL6302
00772          PGMIDERR    (8300-SEND-TEXT)                             EL6302
00773      END-EXEC.                                                    EL6302
00774                                                                   EL6302
00775      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL6302
00776      MOVE ' '                    TO PI-ENTRY-CD-1.                EL6302
00777      MOVE XCTL-005               TO PGM-NAME.                     EL6302
00778      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL6302
00779      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL6302
00780      GO TO 9300-XCTL.                                             EL6302
00781                                                                   EL6302
00782  9900-ERROR-FORMAT.                                               EL6302
00783      IF NOT EMI-ERRORS-COMPLETE                                   EL6302
00784          MOVE LINK-001           TO PGM-NAME                      EL6302
00785          EXEC CICS LINK                                           EL6302
00786              PROGRAM    (PGM-NAME)                                EL6302
00787              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL6302
00788              LENGTH     (EMI-COMM-LENGTH)                         EL6302
00789          END-EXEC.                                                EL6302
00790                                                                   EL6302
00791  9900-EXIT.                                                       EL6302
00792      EXIT.                                                        EL6302
00793                                                                   EL6302
00794  9990-ABEND.                                                      EL6302
00795      MOVE LINK-004               TO PGM-NAME.                     EL6302
00796      MOVE DFHEIBLK               TO EMI-LINE1                     EL6302
00797      EXEC CICS LINK                                               EL6302
00798          PROGRAM   (PGM-NAME)                                     EL6302
00799          COMMAREA  (EMI-LINE1)                                    EL6302
00800          LENGTH    (72)                                           EL6302
00801      END-EXEC.                                                    EL6302
00802                                                                   EL6302
00803      MOVE -1                     TO PFENTERL.                     EL6302
00804      GO TO 8200-SEND-DATAONLY.                                    EL6302
00805                                                                   EL6302
00806      GOBACK.                                                      EL6302
00807                                                                   EL6302
00808  9995-SECURITY-VIOLATION.                                         EL6302
00809                              COPY ELCSCTP.                        EL6302
00810                                                                   EL6302
00811  9995-EXIT.                                                       EL6302
00812      EXIT.                                                        EL6302
00813                                                                   EL6302
00814                                                                   EL6302
00815                                                                   EL6302
