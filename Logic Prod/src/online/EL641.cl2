00001  ID DIVISION.                                                     06/26/96
00002                                                                   EL641
00003  PROGRAM-ID.                 EL641.                                  LV004
00004 *              PROGRAM CONVERTED BY                                  CL**3
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**3
00006 *              CONVERSION DATE 02/12/96 09:55:30.                    CL**3
00007 *                            VMOD=2.004.                             CL**4
00008 *                                                                 EL641
00009 *AUTHOR.     LOGIC,INC.                                              CL**3
00010 *            DALLAS, TEXAS.                                          CL**3
00011                                                                   EL641
00012 *DATE-COMPILED.                                                      CL**3
00013 *SECURITY.   *****************************************************   CL**3
00014 *            *                                                   *   CL**3
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**3
00016 *            *                                                   *   CL**3
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**3
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**3
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**3
00020 *            *                                                   *   CL**3
00021 *            *****************************************************   CL**3
00022                                                                   EL641
00023 *REMARKS.    TRANSACTION - EXC3 - BILLING STATEMENT RELEASE.         CL**2
00024 *        THIS FUNCTION IS USED TO START THE PRINTING OF STORED       CL**2
00025 *        BILLING STATEMENTS AND ADDRESS LABELS OR TO SHOW COUNTS     CL**2
00026 *        OF OUTSTANDING STATEMENTS.                                  CL**2
00027                                                                   EL641
00028      EJECT                                                        EL641
00029  ENVIRONMENT DIVISION.                                            EL641
00030  DATA DIVISION.                                                   EL641
00031  WORKING-STORAGE SECTION.                                         EL641
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL641
00033  77  FILLER  PIC X(32)  VALUE '*    EL641 WORKING STORAGE     *'. EL641
00034  77  FILLER  PIC X(32)  VALUE '********* VMOD2.004 *********'.       CL**4
00035                                                                   EL641
00036     COPY ELCSCTM.                                                    CL**3
00037     COPY ELCSCRTY.                                                   CL**3
00038                                                                   EL641
00039     EJECT                                                         EL641
00040                                                                   EL641
00041  01  WS-DATE-AREA.                                                EL641
00042      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL641
00043      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL641
00044                                                                   EL641
00045  01  STANDARD-AREAS.                                              EL641
00046      12  MAP-NAME.                                                EL641
00047          16  MAP-PREFIX          PIC X(2)    VALUE 'EL'.          EL641
00048          16  MAP-NUMBER          PIC X(4)    VALUE '641A'.        EL641
00049          16  MAP-FILLER          PIC X(2)    VALUE '  '.          EL641
00050      12  SC-ITEM                 PIC S9(4) COMP VALUE +1.         EL641
00051      12  MAPSET-NAME             PIC X(8)    VALUE 'EL641S'.      EL641
00052      12  TRANS-ID                PIC X(4)    VALUE 'EXC3'.        EL641
00053      12  PRINT-TRANS             PIC X(4)    VALUE 'EXC9'.        EL641
00054      12  PGM-NAME                PIC X(8).                        EL641
00055      12  TIME-IN                 PIC S9(7).                       EL641
00056      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL641
00057          16  FILLER              PIC X.                           EL641
00058          16  TIME-OUT            PIC 99V99.                       EL641
00059          16  FILLER              PIC X(2).                        EL641
00060      12  XCTL-005                PIC X(8)    VALUE 'EL005'.       EL641
00061      12  XCTL-010                PIC X(8)    VALUE 'EL010'.       EL641
00062      12  XCTL-126                PIC X(8)    VALUE 'EL126'.       EL641
00063      12  XCTL-6401               PIC X(8)    VALUE 'EL6401'.      EL641
00064      12  LINK-001                PIC X(8)    VALUE 'EL001'.       EL641
00065      12  LINK-004                PIC X(8)    VALUE 'EL004'.       EL641
00066      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL641
00067      12  THIS-PGM                PIC X(8)    VALUE 'EL641'.       EL641
00068      12  CURRENT-SAVE            PIC XX.                          EL641
00069      12  DEEDIT-FIELD            PIC X(15).                       EL641
00070      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(15).       EL641
00071      12  WS-UNPRINTED-COUNT      PIC 9(5)  COMP-3 VALUE 0.        EL641
00072      12  WS-PRINTED-COUNT        PIC 9(5)  COMP-3 VALUE 0.        EL641
00073      12  SUB                     PIC 9  COMP-3.                   EL641
00074      12  BILL-ID                 PIC X(8)    VALUE 'ERBILL'.      EL641
00075      12  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL'.      EL641
00076                                                                   EL641
00077      12  CNTL-KEY.                                                EL641
00078          16  CNTL-CO             PIC X(3).                        EL641
00079          16  CNTL-RECORD-TYPE    PIC X       VALUE '1'.           EL641
00080          16  CNTL-GENL.                                           EL641
00081            18 CNTL-GEN1          PIC X(2)    VALUE SPACES.        EL641
00082            18 CNTL-GEN2.                                          EL641
00083              20 CNTL-GEN3         PIC X       VALUE SPACES.       EL641
00084              20 CNTL-GEN4         PIC X       VALUE SPACES.       EL641
00085          16  CNTL-SEQ             PIC S9(4)   VALUE +0    COMP.   EL641
00086                                                                   EL641
00087      12  BILL-KEY.                                                EL641
00088          16  BILL-CO              PIC X.                          EL641
00089          16  BILL-CARRIER         PIC X.                          EL641
00090          16  BILL-GROUPING        PIC X(6).                       EL641
00091          16  BILL-ACCOUNT         PIC X(10).                      EL641
00092          16  BILL-FIN-RESP        PIC X(10).                      EL641
00093          16  BILL-RECORD-TYPE     PIC X.                          EL641
00094          16  BILL-LINE-SEQ-NO     PIC S9(4)  COMP.                EL641
00095                                                                   EL641
00096      EJECT                                                        EL641
00097      12  BILL-SAVE-KEY           PIC X(20).                       EL641
00098      12  BILL-LENGTH             PIC S9(4)  COMP  VALUE +200.     EL641
00099      12  ER-0070                 PIC 9(4)    VALUE 0070.          EL641
00100      12  ER-0182                 PIC 9(4)    VALUE 0182.          EL641
00101      12  ER-0189                 PIC 9(4)    VALUE 0189.          EL641
00102      12  ER-0190                 PIC 9(4)    VALUE 0190.          EL641
00103      12  ER-0409                 PIC 9(4)    VALUE 0409.          EL641
00104      12  ER-0410                 PIC 9(4)    VALUE 0410.          EL641
00105      12  ER-0411                 PIC 9(4)    VALUE 0411.          EL641
00106      12  ER-0412                 PIC 9(4)    VALUE 0412.          EL641
00107      12  ER-0413                 PIC 9(4)    VALUE 0413.          EL641
00108                                                                   EL641
00109      EJECT                                                        EL641
00110      COPY ELCDATE.                                                   CL**3
00111                                                                   EL641
00112      EJECT                                                        EL641
00113      COPY ELCLOGOF.                                                  CL**3
00114                                                                   EL641
00115      EJECT                                                        EL641
00116      COPY ELCATTR.                                                   CL**3
00117      EJECT                                                        EL641
00118      COPY ELCEMIB.                                                   CL**3
00119      EJECT                                                        EL641
00120      COPY ELCINTF.                                                   CL**3
00121      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    EL641
00122 **********************************************************        EL641
00123 *    NOTE                                                *        EL641
00124 *        THE WORK AREA IS USED BY EL641 AND EL6411       *        EL641
00125 *        AND CANNOT BE REARRANGED WITHOUT COMPILING      *        EL641
00126 *        BOTH PROGRAMS.                                  *        EL641
00127 *                                                        *        EL641
00128 **********************************************************        EL641
00129          16  PI-PRINT-DATE       PIC X(8).                        EL641
00130          16  PI-PRINT-DATE-BIN   PIC XX.                          EL641
00131          16  PI-PRINT-ID         PIC X(4).                        EL641
00132          16  FILLER              PIC X(626).                         CL**3
00133      EJECT                                                        EL641
00134      COPY ELCAID.                                                    CL**3
00135  01  FILLER    REDEFINES DFHAID.                                  EL641
00136      12  FILLER              PIC X(8).                            EL641
00137      12  PF-VALUES           PIC X       OCCURS 24 TIMES.         EL641
00138      EJECT                                                        EL641
00139      COPY EL641S.                                                    CL**3
00140      EJECT                                                        EL641
00141  LINKAGE SECTION.                                                 EL641
00142  01  DFHCOMMAREA             PIC X(1024).                         EL641
00143                                                                   EL641
00144 *01 PARMLIST .                                                       CL**3
00145 *    02  FILLER            PIC S9(8)   COMP.                         CL**3
00146 *    02  CNTL-POINTER      PIC S9(8)   COMP.                         CL**3
00147 *    02  BILL-POINTER      PIC S9(8)   COMP.                         CL**3
00148      EJECT                                                        EL641
00149      COPY ELCCNTL.                                                   CL**3
00150      EJECT                                                        EL641
00151      COPY ERCBILL.                                                   CL**3
00152      EJECT                                                        EL641
00153                                                                   EL641
00154  PROCEDURE DIVISION.                                              EL641
00155                                                                   EL641
00156      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL641
00157      MOVE '5'                   TO DC-OPTION-CODE.                EL641
00158      PERFORM 9700-DATE-LINK.                                      EL641
00159      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL641
00160      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL641
00161                                                                   EL641
00162      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL641
00163                                                                   EL641
00164      MOVE 1 TO EMI-NUMBER-OF-LINES.                               EL641
00165                                                                   EL641
00166      IF EIBCALEN = 0                                              EL641
00167          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL641
00168                                                                   EL641
00169      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL641
00170          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL641
00171              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL641
00172              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL641
00173              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL641
00174              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL641
00175              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL641
00176              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL641
00177              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL641
00178              MOVE THIS-PGM TO PI-CALLING-PROGRAM                  EL641
00179          ELSE                                                     EL641
00180              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL641
00181              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL641
00182              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL641
00183              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL641
00184              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL641
00185              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL641
00186              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL641
00187              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL641
00188                                                                   EL641
00189                                                                   EL641
00190      MOVE SAVE-BIN-DATE       TO CURRENT-SAVE.                    EL641
00191                                                                   EL641
00192      EXEC CICS HANDLE CONDITION                                   EL641
00193          PGMIDERR (9600-PGMID-ERROR)                              EL641
00194          ERROR    (9990-ABEND)                                    EL641
00195      END-EXEC.                                                    EL641
00196                                                                   EL641
00197      IF EIBTRNID NOT = TRANS-ID                                   EL641
00198          MOVE ZEROS      TO PI-PRINT-DATE                         EL641
00199          MOVE LOW-VALUES TO EL641AO                               EL641
00200          GO TO 8100-SEND-INITIAL-MAP.                             EL641
00201                                                                   EL641
00202      IF EIBAID = DFHCLEAR                                         EL641
00203          GO TO 9400-CLEAR.                                        EL641
00204                                                                   EL641
00205      IF PI-PROCESSOR-ID = 'LGXX'                                  EL641
00206          GO TO 0200-RECEIVE.                                      EL641
00207                                                                   EL641
00208      EXEC CICS READQ TS                                           EL641
00209          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       EL641
00210          INTO   (SECURITY-CONTROL)                                EL641
00211          LENGTH (SC-COMM-LENGTH)                                  EL641
00212          ITEM   (SC-ITEM)                                         EL641
00213      END-EXEC.                                                    EL641
00214                                                                   EL641
00215      MOVE SC-CREDIT-DISPLAY (21)  TO PI-DISPLAY-CAP.              EL641
00216      MOVE SC-CREDIT-UPDATE  (21)  TO PI-MODIFY-CAP.               EL641
00217                                                                   EL641
00218      IF NOT DISPLAY-CAP                                           EL641
00219          MOVE 'READ'          TO SM-READ                          EL641
00220          PERFORM 9995-SECURITY-VIOLATION                          EL641
00221          MOVE ER-0070         TO  EMI-ERROR                       EL641
00222          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL641
00223          GO TO 8100-SEND-INITIAL-MAP.                             EL641
00224                                                                   EL641
00225      EJECT                                                        EL641
00226                                                                   EL641
00227  0200-RECEIVE.                                                    EL641
00228      MOVE LOW-VALUES TO EL641AI.                                  EL641
00229      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL641
00230          MOVE 0008 TO EMI-ERROR                                   EL641
00231          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL641
00232          MOVE -1 TO OPTIONL                                       EL641
00233          GO TO 8200-SEND-DATAONLY.                                EL641
00234                                                                   EL641
00235      EXEC CICS RECEIVE                                            EL641
00236          MAP(MAP-NAME)                                            EL641
00237          MAPSET(MAPSET-NAME)                                      EL641
00238          INTO(EL641AI)                                            EL641
00239      END-EXEC.                                                    EL641
00240                                                                   EL641
00241      IF ENTERPFL = 0                                              EL641
00242          GO TO 0300-CHECK-PFKEYS.                                 EL641
00243      IF EIBAID NOT = DFHENTER                                     EL641
00244          MOVE 0004 TO EMI-ERROR                                   EL641
00245          GO TO 0320-INPUT-ERROR.                                  EL641
00246      IF ENTERPFI GREATER 0 AND LESS 25                            EL641
00247          MOVE PF-VALUES (ENTERPFI) TO EIBAID                      EL641
00248      ELSE                                                         EL641
00249          MOVE 0029 TO EMI-ERROR                                   EL641
00250          GO TO 0320-INPUT-ERROR.                                  EL641
00251                                                                   EL641
00252  0300-CHECK-PFKEYS.                                               EL641
00253      IF EIBAID = DFHPF23                                          EL641
00254          GO TO 8810-PF23.                                         EL641
00255      IF EIBAID = DFHPF24                                          EL641
00256          GO TO 9200-RETURN-MAIN-MENU.                             EL641
00257      IF EIBAID = DFHPF12                                          EL641
00258          GO TO 9500-PF12.                                         EL641
00259      IF EIBAID = DFHENTER                                         EL641
00260          GO TO 0330-FUNCTION-CHECK.                               EL641
00261      MOVE 0029 TO EMI-ERROR.                                      EL641
00262                                                                   EL641
00263  0320-INPUT-ERROR.                                                EL641
00264      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL641
00265      MOVE AL-UNBON TO ENTERPFA.                                   EL641
00266                                                                   EL641
00267      IF ENTERPFL = 0                                              EL641
00268          MOVE -1 TO OPTIONL                                       EL641
00269      ELSE                                                         EL641
00270          MOVE -1 TO ENTERPFL.                                     EL641
00271                                                                   EL641
00272      GO TO 8200-SEND-DATAONLY.                                    EL641
00273                                                                   EL641
00274      EJECT                                                        EL641
00275  0330-FUNCTION-CHECK.                                             EL641
00276                                                                   EL641
00277      PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT                     EL641
00278                                                                   EL641
00279      IF NOT EMI-NO-ERRORS                                         EL641
00280         GO TO 8200-SEND-DATAONLY.                                 EL641
00281                                                                   EL641
00282      IF OPTIONI = '1'                                             EL641
00283          GO TO 1000-PRINT-INITIAL-STATEMENT.                      EL641
00284      IF OPTIONI = '2'                                             EL641
00285          GO TO 2000-SHOW-COUNT-OF-STATEMENTS.                     EL641
00286      IF OPTIONI = '3'                                             EL641
00287          GO TO 3000-PRINT-ADDRESS-LABELS.                         EL641
00288      IF OPTIONI = '4'                                             EL641
00289          GO TO 4000-PRINT-STATEMENT-BY-DATE.                      EL641
00290      IF OPTIONI = '5'                                             EL641
00291          MOVE XCTL-6401 TO PGM-NAME                               EL641
00292          GO TO 9300-XCTL.                                         EL641
00293      EJECT                                                        EL641
00294  0350-EDIT-ROUTINE.                                               EL641
00295                                                                   EL641
00296      IF OPTIONI = '2' OR '5'                                      EL641
00297         NEXT SENTENCE                                             EL641
00298      ELSE                                                         EL641
00299         IF NOT MODIFY-CAP                                         EL641
00300            MOVE 'UPDATE'       TO SM-READ                         EL641
00301            PERFORM 9995-SECURITY-VIOLATION                        EL641
00302            MOVE ER-0070        TO EMI-ERROR                       EL641
00303            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL641
00304            GO TO 8100-SEND-INITIAL-MAP.                           EL641
00305                                                                   EL641
00306      IF OPTIONI LESS '1' OR GREATER '5'                           EL641
00307         MOVE -1                  TO OPTIONL                       EL641
00308         MOVE ER-0409             TO EMI-ERROR                     EL641
00309         MOVE AL-UNBON            TO OPTIONA                       EL641
00310         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL641
00311        ELSE                                                       EL641
00312         MOVE AL-UNNON            TO OPTIONA.                      EL641
00313                                                                   EL641
00314      IF OPTIONI = '4'  AND                                        EL641
00315         DATEINL = ZEROS                                           EL641
00316         MOVE -1                  TO DATEINL                       EL641
00317         MOVE ER-0410             TO EMI-ERROR                     EL641
00318         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL641
00319                                                                   EL641
00320      IF DATEINL NOT = ZEROS                                       EL641
00321         MOVE DATEINI             TO DEEDIT-FIELD                  EL641
00322         PERFORM 8600-DEEDIT                                       EL641
00323         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            EL641
00324         MOVE '4'                 TO DC-OPTION-CODE                EL641
00325         PERFORM 9700-DATE-LINK  THRU  9700-EXIT                   EL641
00326         IF DATE-CONVERSION-ERROR                                  EL641
00327            MOVE ER-0182          TO EMI-ERROR                     EL641
00328            MOVE -1               TO DATEINL                       EL641
00329            MOVE AL-UABON         TO DATEINA                       EL641
00330            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT             EL641
00331            ELSE                                                   EL641
00332            MOVE AL-UANON         TO DATEINA                       EL641
00333            MOVE DC-BIN-DATE-1    TO PI-PRINT-DATE-BIN             EL641
00334            MOVE DC-GREG-DATE-1-EDIT   TO PI-PRINT-DATE            EL641
00335                                          DATEINI                  EL641
00336         ELSE                                                      EL641
00337             MOVE LOW-VALUE       TO PI-PRINT-DATE-BIN.            EL641
00338                                                                   EL641
00339  0350-EXIT.                                                       EL641
00340       EXIT.                                                       EL641
00341                                                                   EL641
00342  0400-SET-CODES.                                                  EL641
00343      MOVE PI-COMPANY-ID TO CNTL-CO                                EL641
00344      MOVE PI-COMPANY-CD TO BILL-CO.                               EL641
00345                                                                   EL641
00346      EJECT                                                        EL641
00347                                                                   EL641
00348  1000-PRINT-INITIAL-STATEMENT.                                    EL641
00349      MOVE '1'                    TO PI-ENTRY-CD-1                 EL641
00350                                     PI-ENTRY-CD-2.                EL641
00351      GO TO 7800-START-PRINT.                                      EL641
00352                                                                   EL641
00353  2000-SHOW-COUNT-OF-STATEMENTS.                                   EL641
00354      PERFORM 7500-BROWSE-BILLING THRU 7599-EXIT                   EL641
00355      GO TO 6500-COMPLETE-COUNT.                                   EL641
00356                                                                   EL641
00357  3000-PRINT-ADDRESS-LABELS.                                       EL641
00358      MOVE SPACES                 TO PI-ENTRY-CD-1                 EL641
00359      MOVE '2'                    TO PI-ENTRY-CD-2                 EL641
00360      GO TO 7800-START-PRINT.                                      EL641
00361                                                                   EL641
00362  4000-PRINT-STATEMENT-BY-DATE.                                    EL641
00363      MOVE SPACES                 TO PI-ENTRY-CD-1                 EL641
00364      MOVE '3'                    TO PI-ENTRY-CD-2                 EL641
00365      GO TO 7800-START-PRINT.                                      EL641
00366                                                                   EL641
00367  6500-COMPLETE-COUNT.                                             EL641
00368      MOVE WS-UNPRINTED-COUNT     TO UCOUNTO                       EL641
00369      MOVE WS-PRINTED-COUNT       TO PCOUNTO                       EL641
00370      MOVE -1                     TO OPTIONL                       EL641
00371      MOVE ZEROS                  TO EMI-ERROR                     EL641
00372      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL641
00373      GO TO 8200-SEND-DATAONLY.                                    EL641
00374      EJECT                                                        EL641
00375                                                                   EL641
00376  7500-BROWSE-BILLING.                                             EL641
00377      MOVE LOW-VALUES             TO BILL-KEY                      EL641
00378      PERFORM 0400-SET-CODES                                       EL641
00379      EXEC CICS HANDLE CONDITION                                   EL641
00380           NOTFND (7599-EXIT)                                      EL641
00381           ENDFILE(7599-EXIT)                                      EL641
00382           NOTOPEN(8850-BILL-NOT-OPEN)                             EL641
00383      END-EXEC                                                     EL641
00384                                                                   EL641
00385      EXEC CICS STARTBR                                            EL641
00386           DATASET(BILL-ID)                                        EL641
00387           RIDFLD (BILL-KEY)                                       EL641
00388       END-EXEC.                                                   EL641
00389                                                                   EL641
00390  7505-RESET-HANDLE.                                               EL641
00391      EXEC CICS HANDLE CONDITION                                   EL641
00392           NOTFND (7599-EXIT)                                      EL641
00393           ENDFILE(7599-EXIT)                                      EL641
00394           NOTOPEN(8850-BILL-NOT-OPEN)                             EL641
00395      END-EXEC.                                                    EL641
00396                                                                   EL641
00397  7510-READ-NEXT.                                                  EL641
00398      EXEC CICS READNEXT                                           EL641
00399           DATASET(BILL-ID)                                        EL641
00400           RIDFLD (BILL-KEY)                                       EL641
00401           SET    (ADDRESS OF BILLING-STATEMENT)                      CL**3
00402      END-EXEC.                                                    EL641
00403                                                                   EL641
00404      IF BILL-CO NOT = PI-COMPANY-CD                               EL641
00405         GO TO 7599-EXIT.                                          EL641
00406                                                                   EL641
00407      IF BILL-RECORD-TYPE NOT = '1'                                EL641
00408         MOVE '3'                 TO BILL-RECORD-TYPE              EL641
00409         MOVE 9999                TO BILL-LINE-SEQ-NO              EL641
00410         GO TO 7510-READ-NEXT.                                     EL641
00411                                                                   EL641
00412         IF BI-INITIAL-PRINT-DATE = LOW-VALUES                     EL641
00413            ADD 1                 TO WS-UNPRINTED-COUNT            EL641
00414        ELSE                                                       EL641
00415            ADD 1                 TO WS-PRINTED-COUNT.             EL641
00416                                                                   EL641
00417         MOVE '3'                 TO BILL-RECORD-TYPE.             EL641
00418         MOVE 9999                TO BILL-LINE-SEQ-NO.             EL641
00419         GO TO 7510-READ-NEXT.                                     EL641
00420                                                                   EL641
00421  7599-EXIT.                                                       EL641
00422       EXIT.                                                       EL641
00423      EJECT                                                        EL641
00424                                                                   EL641
00425  7800-START-PRINT.                                                EL641
00426      PERFORM 0400-SET-CODES.                                      EL641
00427                                                                   EL641
00428      EXEC CICS HANDLE CONDITION                                   EL641
00429           NOTOPEN     (8840-CNTL-NOT-OPEN)                        EL641
00430           NOTFND      (7890-NOT-FOUND)                            EL641
00431           TERMIDERR   (8820-TERMID-ERROR)                         EL641
00432           TRANSIDERR  (8830-TRANS-ERROR)                          EL641
00433       END-EXEC.                                                   EL641
00434                                                                   EL641
00435      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.               CL**4
00436      IF PRINTERL NOT = ZEROS                                      EL641
00437         MOVE PRINTERI            TO PI-PRINT-ID                   EL641
00438                                     PI-ALT-DMD-PRT-ID                CL**4
00439         GO TO 7820-START.                                         EL641
00440                                                                      CL**2
00441      IF PI-PROCESSOR-PRINTER IS NOT EQUAL TO SPACES                  CL**2
00442          MOVE PI-PROCESSOR-PRINTER   TO  PI-PRINT-ID                 CL**2
00443          GO TO 7820-START.                                           CL**2
00444                                                                   EL641
CIDMOD     MOVE '9'                    TO CNTL-RECORD-TYPE.             EL641
00446      MOVE SPACES                 TO CNTL-GENL.                    EL641
00447      MOVE ZEROS                  TO CNTL-SEQ.                     EL641
00448                                                                   EL641
00449      EXEC CICS READ                                               EL641
00450           DATASET(CNTL-ID)                                        EL641
00451           SET    (ADDRESS OF CONTROL-FILE)                           CL**3
00452           RIDFLD (CNTL-KEY)                                       EL641
00453      END-EXEC.                                                    EL641
00454                                                                   EL641
00455      MOVE CF-FORMS-PRINTER-ID    TO PI-PRINT-ID.                  EL641
00456                                                                   EL641
00457  7820-START.                                                      EL641
00458                                                                      CL**4
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**4
00460 *        MOVE EIBTRMID       TO PI-PRINT-ID                          CL**4
00461          EXEC CICS START                                             CL**4
00462               INTERVAL    (0)                                        CL**4
00463               TRANSID     (PRINT-TRANS)                              CL**4
00464               FROM        (PROGRAM-INTERFACE-BLOCK)                  CL**4
00465               LENGTH      (PI-COMM-LENGTH)                           CL**4
00466 *             TERMID      (PI-PRINT-ID)                              CL**4
00467          END-EXEC                                                    CL**4
00468      ELSE                                                            CL**4
00469          EXEC CICS START                                             CL**4
00470               INTERVAL    (0)                                        CL**4
00471               TRANSID     (PRINT-TRANS)                              CL**4
00472               FROM        (PROGRAM-INTERFACE-BLOCK)                  CL**4
00473               LENGTH      (PI-COMM-LENGTH)                           CL**4
00474               TERMID      (PI-PRINT-ID)                              CL**4
00475          END-EXEC.                                                   CL**4
00476                                                                   EL641
00477      IF OPTIONI = '3'                                             EL641
00478          MOVE ER-0411            TO EMI-ERROR                     EL641
00479      ELSE                                                         EL641
00480          MOVE ER-0189            TO EMI-ERROR.                    EL641
00481                                                                   EL641
00482      MOVE -1                     TO OPTIONL.                      EL641
00483      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL641
00484      GO TO 8200-SEND-DATAONLY.                                    EL641
00485                                                                   EL641
00486  7890-NOT-FOUND.                                                  EL641
00487      MOVE ER-0190                TO EMI-ERROR.                    EL641
00488      MOVE -1                     TO OPTIONL.                      EL641
00489      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                   EL641
00490      GO TO 8200-SEND-DATAONLY.                                    EL641
00491      EJECT                                                        EL641
00492                                                                   EL641
00493  8100-SEND-INITIAL-MAP.                                           EL641
00494      MOVE SAVE-DATE              TO DATEAO.                       EL641
00495      MOVE EIBTIME                TO TIME-IN.                      EL641
00496      MOVE TIME-OUT               TO TIMEAO.                       EL641
00497      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO                       EL641
00498      MOVE -1                     TO OPTIONL.                      EL641
00499      EXEC CICS SEND                                               EL641
00500          MAP   (MAP-NAME)                                         EL641
00501          MAPSET(MAPSET-NAME)                                      EL641
00502          FROM  (EL641AO)                                          EL641
00503          ERASE                                                    EL641
00504          CURSOR                                                   EL641
00505      END-EXEC.                                                    EL641
00506                                                                   EL641
00507      GO TO 9100-RETURN-TRAN.                                      EL641
00508                                                                   EL641
00509  8200-SEND-DATAONLY.                                              EL641
00510      MOVE SAVE-DATE              TO DATEAO.                       EL641
00511      MOVE EIBTIME                TO TIME-IN.                      EL641
00512      MOVE TIME-OUT               TO TIMEAO.                       EL641
00513      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO                       EL641
00514      EXEC CICS SEND                                               EL641
00515          MAP   (MAP-NAME)                                         EL641
00516          MAPSET(MAPSET-NAME)                                      EL641
00517          FROM  (EL641AO)                                          EL641
00518          DATAONLY                                                 EL641
00519          ERASEAUP                                                 EL641
00520          CURSOR                                                   EL641
00521      END-EXEC.                                                    EL641
00522      GO TO 9100-RETURN-TRAN.                                      EL641
00523                                                                   EL641
00524  8300-SEND-TEXT.                                                  EL641
00525      EXEC CICS SEND TEXT                                          EL641
00526          FROM  (LOGOFF-TEXT)                                      EL641
00527          LENGTH(LOGOFF-LENGTH)                                    EL641
00528          ERASE                                                    EL641
00529          FREEKB                                                   EL641
00530      END-EXEC.                                                    EL641
00531                                                                   EL641
00532      EXEC CICS RETURN                                             EL641
00533          END-EXEC.                                                EL641
00534                                                                   EL641
00535  8600-DEEDIT.                                                     EL641
00536      EXEC CICS BIF DEEDIT                                         EL641
00537           FIELD (DEEDIT-FIELD)                                    EL641
00538           LENGTH(15)                                              EL641
00539      END-EXEC.                                                    EL641
00540      EJECT                                                        EL641
00541  8800-UNAUTHORIZED-ACCESS.                                        EL641
00542      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL641
00543      GO TO 8300-SEND-TEXT.                                        EL641
00544                                                                   EL641
00545  8810-PF23.                                                       EL641
00546      MOVE EIBAID   TO PI-ENTRY-CD-1.                              EL641
00547      MOVE XCTL-005 TO PGM-NAME.                                   EL641
00548      GO TO 9300-XCTL.                                             EL641
00549                                                                   EL641
00550  8820-TERMID-ERROR.                                               EL641
00551      MOVE ER-0412                TO EMI-ERROR                     EL641
00552      GO TO 8999-OPEN-ERROR.                                       EL641
00553  8830-TRANS-ERROR.                                                EL641
00554      MOVE ER-0413                TO EMI-ERROR                     EL641
00555      GO TO 8999-OPEN-ERROR.                                       EL641
00556                                                                   EL641
00557  8840-CNTL-NOT-OPEN.                                              EL641
00558      MOVE 0042 TO EMI-ERROR.                                      EL641
00559      GO TO 8999-OPEN-ERROR.                                       EL641
00560                                                                   EL641
00561  8850-BILL-NOT-OPEN.                                              EL641
00562      MOVE 0408 TO EMI-ERROR.                                      EL641
00563      GO TO 8999-OPEN-ERROR.                                       EL641
00564                                                                   EL641
00565  8870-ACTV-NOT-OPEN.                                              EL641
00566      MOVE 0172 TO EMI-ERROR.                                      EL641
00567      GO TO 8999-OPEN-ERROR.                                       EL641
00568                                                                   EL641
00569                                                                   EL641
00570  8999-OPEN-ERROR.                                                 EL641
00571      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL641
00572      MOVE -1 TO OPTIONL.                                          EL641
00573      GO TO 8200-SEND-DATAONLY.                                    EL641
00574  9000-RETURN-CICS.                                                EL641
00575      EXEC CICS RETURN                                             EL641
00576          END-EXEC.                                                EL641
00577                                                                   EL641
00578  9100-RETURN-TRAN.                                                EL641
00579      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               EL641
00580      MOVE MAP-NUMBER           TO PI-CURRENT-SCREEN-NO.           EL641
00581      EXEC CICS RETURN                                             EL641
00582          TRANSID(TRANS-ID)                                        EL641
00583          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL641
00584          LENGTH(PI-COMM-LENGTH)                                   EL641
00585      END-EXEC.                                                    EL641
00586                                                                   EL641
00587  9200-RETURN-MAIN-MENU.                                           EL641
00588      MOVE XCTL-126 TO PGM-NAME.                                   EL641
00589      GO TO 9300-XCTL.                                             EL641
00590                                                                   EL641
00591  9300-XCTL.                                                       EL641
00592      EXEC CICS XCTL                                               EL641
00593          PROGRAM(PGM-NAME)                                        EL641
00594          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL641
00595          LENGTH(PI-COMM-LENGTH)                                   EL641
00596      END-EXEC.                                                    EL641
00597                                                                   EL641
00598  9400-CLEAR.                                                      EL641
00599      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME                        EL641
00600      GO TO 9300-XCTL.                                             EL641
00601                                                                   EL641
00602  9500-PF12.                                                       EL641
00603      MOVE XCTL-010 TO PGM-NAME.                                   EL641
00604      GO TO 9300-XCTL.                                             EL641
00605                                                                   EL641
00606  9600-PGMID-ERROR.                                                EL641
00607      EXEC CICS HANDLE CONDITION                                   EL641
00608          PGMIDERR(8300-SEND-TEXT)                                 EL641
00609      END-EXEC.                                                    EL641
00610                                                                   EL641
00611      MOVE ' '          TO PI-ENTRY-CD-1.                          EL641
00612      MOVE XCTL-005     TO PGM-NAME.                               EL641
00613      MOVE PGM-NAME     TO LOGOFF-PGM.                             EL641
00614      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL641
00615      GO TO 9300-XCTL.                                             EL641
00616                                                                   EL641
00617  9700-DATE-LINK.                                                  EL641
00618      MOVE LINK-ELDATCV TO PGM-NAME                                EL641
00619      EXEC CICS LINK                                               EL641
00620          PROGRAM    (PGM-NAME)                                    EL641
00621          COMMAREA   (DATE-CONVERSION-DATA)                        EL641
00622          LENGTH     (DC-COMM-LENGTH)                              EL641
00623      END-EXEC.                                                    EL641
00624                                                                   EL641
00625  9700-EXIT.                                                       EL641
00626       EXIT.                                                       EL641
00627                                                                   EL641
00628  9900-ERROR-FORMAT.                                               EL641
00629      IF NOT EMI-ERRORS-COMPLETE                                   EL641
00630          MOVE LINK-001 TO PGM-NAME                                EL641
00631          EXEC CICS LINK                                           EL641
00632              PROGRAM(PGM-NAME)                                    EL641
00633              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL641
00634              LENGTH(EMI-COMM-LENGTH)                              EL641
00635          END-EXEC.                                                EL641
00636                                                                   EL641
00637  9900-EXIT.                                                       EL641
00638      EXIT.                                                        EL641
00639                                                                   EL641
00640  9990-ABEND.                                                      EL641
00641      MOVE LINK-004 TO PGM-NAME.                                   EL641
00642      MOVE DFHEIBLK TO EMI-LINE1.                                  EL641
00643      EXEC CICS LINK                                               EL641
00644          PROGRAM(PGM-NAME)                                        EL641
00645          COMMAREA(EMI-LINE1)                                      EL641
00646          LENGTH(72)                                               EL641
00647      END-EXEC.                                                    EL641
00648                                                                   EL641
00649      GO TO 8200-SEND-DATAONLY.                                    EL641
00650      GOBACK.                                                      EL641
00651                                                                   EL641
00652  9995-SECURITY-VIOLATION.                                         EL641
00653                              COPY ELCSCTP.                        EL641
00654                                                                   EL641
00655  9995-EXIT.                                                       EL641
00656      EXIT.                                                        EL641
00657                                                                   EL641
