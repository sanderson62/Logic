00001  IDENTIFICATION DIVISION.                                         04/24/97
00002                                                                   EL6851
00003  PROGRAM-ID.                 EL6851.                                 LV013
00004 *              PROGRAM CONVERTED BY                                  CL*11
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*11
00006 *              CONVERSION DATE 02/12/96 09:56:47.                    CL*11
00007 *                            VMOD=2.013                              CL*13
00008 *                                                                 EL6851
00009 *AUTHOR.    LOGIC, INC.                                              CL*11
00010 *           DALLAS, TEXAS.                                           CL*11
00011                                                                   EL6851
00012 *DATE-COMPILED.                                                      CL*11
00013                                                                   EL6851
00014 *SECURITY.   *****************************************************   CL*11
00015 *            *                                                   *   CL*11
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*11
00017 *            *                                                   *   CL*11
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*11
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*11
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*11
00021 *            *                                                   *   CL*11
00022 *            *****************************************************   CL*11
00023                                                                   EL6851
00024 *REMARKS.                                                            CL**5
00025 *        THIS PROGRAM WILL PRINT ONE OF THREE REPORTS                CL**5
00026 *          1)  CHECKS WAITING TO BE PRINTED                          CL**5
00027 *          2)  CHECKS PRINTED                                        CL**5
00028 *          3)  CHECKS RELEASED REPORT                                CL**5
00029 *                                                                    CL**4
00030 *    ENTERED BY  - EL685  - CHECKS TO BE PRINTED REPORT              CL**4
00031 *                           CHECKS PRINTED REPORT                    CL**4
00032 *                  EL686  - A/R CHECKS RELEASED REPORT               CL**4
00033                                                                   EL6851
00034      EJECT                                                        EL6851
00035  ENVIRONMENT DIVISION.                                            EL6851
00036                                                                   EL6851
00037  DATA DIVISION.                                                   EL6851
00038                                                                   EL6851
00039  WORKING-STORAGE SECTION.                                         EL6851
00040                                                                   EL6851
00041  77  THIS-PGM PIC X(6)  VALUE 'EL6851'.                              CL*12
00042  77  FILLER  PIC X(32)  VALUE '********************************'. EL6851
00043  77  FILLER  PIC X(32)  VALUE '*   EL6851 WORKING STORAGE     *'. EL6851
00044  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.013 *********'.    CL*13
00045                                                                   EL6851
00046  01  WS-DATE-AREA.                                                EL6851
00047      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL6851
00048      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.               CL**2
00049                                                                   EL6851
00050  01  FILLER   COMP-3.                                                CL**2
00051      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.  EL6851
00052      05  WS-TIME-WORK                PIC S9(7)       VALUE ZERO.  EL6851
00053      05  WS-TIME                     REDEFINES                    EL6851
00054          WS-TIME-WORK                PIC S9(3)V9(4).              EL6851
00055                                                                   EL6851
00056  01  FILLER.                                                      EL6851
00057      05  ERROR-LINE                 PIC X(80).                    EL6851
00058      05  WS-CHECK-QUEUE-DSID        PIC X(8) VALUE 'ERCHKQ'.      EL6851
00059      05  WS-COMCK-QUEUE-DSID        PIC X(8) VALUE 'ERCMKQ'.         CL**2
00060      05  WS-PENDING-PAYMENTS-DSID   PIC X(8) VALUE 'ERPYAJ'.      EL6851
00061      05  WS-NO-CHECKS               PIC S9(4) VALUE +0.           EL6851
00062                                                                      CL**4
00063  01  FILLER.                                                         CL**4
00064      05  WS-TO-BE-PRINTED-DESC      PIC X(28)        VALUE           CL**4
00065          '-   CHECKS TO BE PRINTED   -'.                             CL**4
00066                                                                      CL**4
00067      05  WS-CHECKS-PRINTED-DESC     PIC X(28)        VALUE           CL**4
00068          '-      PRINTED CHECKS      -'.                             CL**4
00069                                                                      CL**4
00070      05  WS-AR-TO-BE-PRINTED-DESC   PIC X(28)        VALUE           CL**4
00071          '- A/R CHECKS TO BE PRINTED -'.                             CL**4
00072                                                                      CL**4
00073      05  WS-AR-CHECKS-PRINTED-DESC  PIC X(28)        VALUE           CL**4
00074          '-    A/R PRINTED CHECKS    -'.                             CL**4
00075                                                                      CL**4
00076      05  WS-AR-CHECKS-RELEASED-DESC PIC X(28)        VALUE           CL**4
00077          '-   A/R CHECKS RELEASED   - '.                             CL**4
00078                                                                   EL6851
00079                              COPY ELCDMD34.                          CL*12
00080      EJECT                                                           CL**7
00081                              COPY ELCINTF.                           CL**7
00082      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                      CL**2
00083          16  PI-CHECK-QUE-KEY.                                    EL6851
00084              20  PI-CK-COMPANY-CODE     PIC X.                    EL6851
00085              20  PI-CK-CONTROL-NO       PIC S9(8)          COMP.  EL6851
00086              20  PI-CK-SEQUENCE-NO      PIC S9(4)          COMP.  EL6851
00087                                                                   EL6851
00088          16  PI-PREV-CHECK-QUE-KEY.                               EL6851
00089              20  PI-PREV-CK-COMPANY-CODE     PIC X.               EL6851
00090              20  PI-PREV-CK-CONTROL-NO       PIC S9(8) COMP.      EL6851
00091              20  PI-PREV-CK-SEQUENCE-NO      PIC S9(4) COMP.      EL6851
00092                                                                   EL6851
00093          16  FILLER                          PIC X(03).              CL**7
00094                                                                   EL6851
00095          16  PI-CONTROL-TOTALS.                                      CL**7
00096              20  PI-CONTROL-TOT              PIC S9(7)V99 COMP-3.    CL**7
00097              20  PI-CONTROL-GRAND-TOT        PIC S9(7)V99 COMP-3.    CL**7
00098              20  PI-CONTROL-SAVE-CONTROL     PIC S9(8) COMP.         CL**7
00099                                                                   EL6851
00100          16  PI-FIRST-TIME-SWT               PIC X.                  CL**7
00101              88  PI-FIRST-TIME                  VALUE 'Y'.           CL**7
00102              88  PI-NOT-FIRST-TIME              VALUE 'N'.           CL**7
00103                                                                      CL**4
00104          16  FILLER                          PIC X(02).              CL**7
00105          16  PI-CHECK-MODE                   PIC X.                  CL**7
00106              88 CHECKS-PRINTED                  VALUE 'Y'.           CL**7
00107              88 CHECKS-TO-BE-PRINTED            VALUE SPACE.         CL**7
00108                                                                   EL6851
00109          16  FILLER                          PIC X(10).              CL**7
00110                                                                      CL**7
00111          16  PI-AR-MODE                      PIC X.                  CL**7
00112              88 PI-ORIG-CALL-AR-MENU            VALUE 'A'.           CL**7
00113                                                                      CL**7
00114          16  FILLER                          PIC X(01).              CL**7
00115                                                                      CL**7
00116          16  PI-START-CONTROL-NO             PIC S9(08)  COMP.       CL**7
00117                                                                      CL**7
00118          16  PI-PAGE                         PIC S999    COMP-3.     CL**7
00119          16  PI-LINE-COUNT                   PIC S999    COMP-3.     CL**7
00120          16  PI-TOT-CHECKS                   PIC S9(4).              CL**7
00121          16  FILLER                          PIC X(581).             CL*11
00122                                                                      CL**7
00123      EJECT                                                           CL**7
00124  01  HEADING-1.                                                   EL6851
00125      12  FILLER                  PIC XX      VALUE SPACES.        EL6851
00126      12  ADATE                   PIC X(8).                        EL6851
00127      12  FILLER                  PIC XX      VALUE SPACES.        EL6851
00128      12  ATIME                   PIC 99.99.                       EL6851
00129      12  FILLER                  PIC X(8)    VALUE SPACES.           CL**2
00130      12  ACOMP                   PIC XXX.                            CL**2
00131      12  FILLER                  PIC X       VALUE SPACES.        EL6851
00132      12  ATITLEO                 PIC X(28).                          CL**4
00133      12  FILLER                  PIC X(14)   VALUE SPACES.        EL6851
00134      12  FILLER                  PIC X(5)    VALUE 'PAGE'.        EL6851
00135      12  PAGE-NO                 PIC ZZ9.                         EL6851
00136                                                                   EL6851
00137  01  HEADING-2.                                                   EL6851
00138      12  FILLER                  PIC XX      VALUE SPACES.        EL6851
00139      12  FILLER                  PIC X(42)   VALUE                   CL**8
00140          'CONTROL CHECK NO  PMT TYPE   CAR  GROUP   '.               CL**8
00141      12  HD2-DESC-1              PIC X(08)   VALUE 'FIN.RESP'.       CL**8
00142      12  FILLER                  PIC X(03)   VALUE SPACES.           CL**4
00143      12  HD2-DESC-2              PIC X(08)   VALUE ' ACCOUNT'.       CL**8
00144      12  FILLER                  PIC X(17)   VALUE                   CL**4
00145          '       PMT AMOUNT'.                                        CL**4
00146                                                                      CL**4
00147  01  HEADING-2B.                                                     CL**4
00148      12  FILLER                  PIC XX      VALUE SPACES.           CL**4
00149      12  FILLER                  PIC X(36)   VALUE                   CL**4
00150          'CONTROL  CHK NO   CHK DT  CAR  GRP  '.                     CL**4
00151      12  FILLER                  PIC X(08)   VALUE '   PAYEE'.       CL**4
00152      12  FILLER                  PIC X(04)   VALUE SPACES.           CL**4
00153      12  FILLER                  PIC X(04)   VALUE 'SEQ '.           CL**4
00154      12  FILLER                  PIC X(11)   VALUE                   CL**4
00155          ' PAYEE NAME'.                                              CL**4
00156      12  FILLER                  PIC X(05)   VALUE SPACES.           CL**4
00157      12  FILLER                  PIC X(10)   VALUE                   CL**4
00158          'PMT AMOUNT'.                                               CL**4
00159      EJECT                                                        EL6851
00160                                                                   EL6851
00161  01  DETAIL-LINE.                                                 EL6851
00162      15  FILLER                      PIC X.                          CL**4
00163      15  EL685A-CONTROL              PIC 9(7).                       CL**4
00164      15  FILLER                      PIC XX.                         CL**4
00165      15  EL685A-CHECK-NO             PIC X(7).                       CL**8
00166      15  FILLER                      PIC X.                          CL**8
00167      15  EL685A-PMT-TYPE             PIC X(11).                      CL**4
00168      15  FILLER                      PIC X(3).                       CL**4
00169      15  EL685A-CARRIER              PIC X.                          CL**4
00170      15  FILLER                      PIC XX.                         CL**4
00171      15  EL685A-GROUP                PIC X(6).                       CL**4
00172      15  FILLER                      PIC XX.                         CL**4
00173      15  EL685A-FIN-RESP             PIC X(10).                      CL**4
00174      15  FILLER                      PIC X.                          CL**4
00175      15  EL685A-ACCOUNT              PIC X(11).                      CL**8
00176      15  FILLER                      PIC X.                          CL**4
00177      15  EL685A-AMT                  PIC Z,ZZZ,ZZ9.99-.              CL**4
00178      15  FILLER                      PIC XX.                         CL**8
00179                                                                      CL**4
00180  01  DETAIL-LINEB                    REDEFINES                       CL**4
00181      DETAIL-LINE.                                                    CL**4
00182      15  FILLER                      PIC XX.                         CL**4
00183      15  EL685B-CONTROL              PIC 9(7).                       CL**4
00184      15  FILLER                      PIC XX.                         CL**4
00185      15  EL685B-CHECK-NO             PIC X(6).                       CL**4
00186      15  FILLER                      PIC XX.                         CL**4
00187      15  EL685B-CHK-DT               PIC X(08).                      CL**4
00188      15  FILLER                      PIC X(2).                       CL**4
00189      15  EL685B-CARRIER              PIC X.                          CL**4
00190      15  FILLER                      PIC X.                          CL**4
00191      15  EL685B-GROUP                PIC X(6).                       CL**4
00192      15  FILLER                      PIC XX.                         CL**4
00193      15  EL685B-PAYEE                PIC X(10).                      CL**4
00194      15  FILLER                      PIC X.                          CL**4
00195      15  EL685B-PAYEE-SEQ            PIC ZZZ9.                       CL**4
00196      15  FILLER                      PIC X.                          CL**4
00197      15  EL685B-PAYEE-NA             PIC X(12).                      CL**4
00198      15  FILLER                      PIC X.                          CL**4
00199      15  EL685B-AMT                  PIC Z,ZZZ,ZZ9.99-.              CL**4
00200                                                                   EL6851
00201  01  TOTAL-LINE                      REDEFINES                    EL6851
00202      DETAIL-LINE.                                                 EL6851
00203      15  FILLER                      PIC X(19).                      CL**4
00204      15  EL685A-NO-CHECKS-DESC       PIC X(11).                      CL**4
00205      15  EL685A-NO-CHECKS            PIC ZZZ9.                       CL**4
00206      15  FILLER                      PIC X(12).                      CL**4
00207      15  EL685A-CNTL-DESC            PIC X(12).                      CL**4
00208      15  FILLER                      PIC X(8).                       CL**8
00209      15  EL685A-CNTL-TOTAL           PIC Z,ZZZ,ZZ9.99-.              CL**4
00210      15  FILLER                      PIC XX.                         CL**8
00211                                                                      CL**4
00212  01  TOTAL-LINE2                     REDEFINES                       CL**4
00213      DETAIL-LINE.                                                    CL**4
00214      15  FILLER                      PIC X(19).                      CL**4
CIDMOD     15  EL685B-NO-CHECKS-DESC       PIC X(11).                        000
CIDMOD     15  EL685B-NO-CHECKS            PIC ZZZ9.                         000
00217      15  FILLER                      PIC X(12).                      CL**4
00218      15  EL685B-CNTL-DESC            PIC X(12).                      CL**4
00219      15  FILLER                      PIC X(8).                       CL**8
00220      15  EL685B-CNTL-TOTAL           PIC Z,ZZZ,ZZ9.99-.              CL**4
00221      15  FILLER                      PIC XX.                         CL**8
00222                                                                      CL**4
00223      EJECT                                                        EL6851
00224                                  COPY ELPRTCVD.                      CL**7
00225      EJECT                                                        EL6851
00226                                  COPY ELCDATE.                       CL**7
00227      EJECT                                                        EL6851
00228  LINKAGE SECTION.                                                 EL6851
00229                                                                   EL6851
00230 *01 DFHBLLDS  COMP SYNC.                                             CL*11
00231 *    05  BLLCBAR                     PIC S9(9).                      CL*11
00232 *    05  CQFCBAR                     PIC S9(9).                      CL*11
00233 *    05  CMFCBAR                     PIC S9(9).                      CL*11
00234 *    05  PPFCBAR                     PIC S9(9).                      CL*11
00235      EJECT                                                        EL6851
00236                                  COPY ERCCHKQ.                       CL**7
00237      EJECT                                                           CL**2
00238                                  COPY ERCCMKQ.                       CL**7
00239      EJECT                                                        EL6851
00240                                  COPY ERCPYAJ.                       CL**7
00241      EJECT                                                        EL6851
00242  PROCEDURE DIVISION.                                              EL6851
00243                                                                   EL6851
00244      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL6851
00245      MOVE '5'                   TO DC-OPTION-CODE.                EL6851
00246      PERFORM 9950-LINK-DATE-CONVERT THRU 9950-EXIT.               EL6851
00247      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL6851
00248      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL6851
00249      MOVE SPACES                TO DL34-PROCESS-TYPE                 CL*12
00250                                                                   EL6851
00251      MOVE 80                     TO WS-LINE-LEN.                  EL6851
CIDMOD*    MOVE 'N'                    TO CSO-PRINT-STARTED-SW.              000
00252  0100-RETRIEVE-LOOP.                                              EL6851
00253      EXEC CICS HANDLE CONDITION                                   EL6851
00254           ENDDATA (200-END-DATA)                                     CL**2
00255           ENDFILE (4800-END-OF-FILE)                                 CL**2
00256           NOTFND  (300-NOT-FOUND)                                    CL**2
00257      END-EXEC.                                                       CL**2
00258                                                                      CL**2
00259      EXEC CICS RETRIEVE                                           EL6851
00260           INTO   (PROGRAM-INTERFACE-BLOCK)                           CL**2
00261           LENGTH (PI-COMM-LENGTH)                                    CL**2
00262      END-EXEC.                                                       CL**2
00263                                                                      CL*12
00264 * DLO034 OPEN WHEN DMD OR CID                                        CL*12
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*12
00266          IF DL34-PROCESS-TYPE IS EQUAL TO SPACES                     CL*12
00267              MOVE 'O'                TO DL34-PROCESS-TYPE            CL*12
00268              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL*12
00269              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL*12
00270              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL*12
00271              MOVE SPACES             TO DL34-PRINT-LINE              CL*12
00272              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL*12
00273                                                                      CL*12
00274              EXEC CICS LINK                                          CL*12
00275                  PROGRAM    ('DLO034')                               CL*12
00276                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL*12
00277                  LENGTH     (DLO034-REC-LENGTH)                      CL*12
00278              END-EXEC                                                CL*12
00279                                                                      CL*12
00280              IF DL34-RETURN-CODE NOT = 'OK'                          CL*12
00281                  MOVE  '**DLO034 OPEN ERROR - ABORT**'               CL*12
00282                                      TO ERROR-LINE                   CL*12
00283                  PERFORM 400-SEND-TEXT                               CL*12
00284                  EXEC CICS RETURN                                    CL*12
00285                  END-EXEC.                                           CL*12
00286                                                                   EL6851
CIDMOD*    MOVE PI-PROCESSOR-PRINTER TO CSO-PRINT-ID.                        000
CIDMOD*    MOVE 'F'                TO  DRS-SW.                               000
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                              000
CIDMOD*    MOVE ' '                TO  DRS-SW.                               000
CIDMOD*                                                                      000
00287      PERFORM 1000-INITIALIZE.                                     EL6851
00288                                                                   EL6851
00289      PERFORM 4000-BROWSE-CHECK-QUEUE-FILE.                        EL6851
00290                                                                   EL6851
00291      MOVE SPACES                 TO TOTAL-LINE                       CL**4
00292                                     TOTAL-LINE2.                     CL**4
00293      MOVE 'CONTL TOTAL'          TO EL685A-CNTL-DESC                 CL**4
00294                                     EL685B-CNTL-DESC.                CL**4
00295      MOVE PI-CONTROL-TOT         TO EL685A-CNTL-TOTAL                CL**4
00296                                     EL685B-CNTL-TOTAL.               CL**4
CIDMOD     MOVE 'NO. CHECKS '          TO EL685A-NO-CHECKS-DESC              000
CIDMOD                                    EL685B-NO-CHECKS-DESC.             000
CIDMOD     MOVE WS-NO-CHECKS           TO EL685A-NO-CHECKS                   000
CIDMOD                                    EL685B-NO-CHECKS.                  000
00297                                                                      CL**4
00298      IF (PI-CALLING-PROGRAM = 'EL686   '  AND                        CL**4
00299           PI-PGM-PRINT-OPT = '5')         OR                         CL**4
00300          PI-ORIG-CALL-AR-MENU                                        CL**4
00301          MOVE TOTAL-LINE2        TO WS-PASSED-DATA                   CL**4
00302      ELSE                                                            CL**4
00303          MOVE TOTAL-LINE         TO WS-PASSED-DATA.                  CL**4
00304                                                                      CL**4
00305      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.             CL**2
00306      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6851
00307      PERFORM 3000-BUMP-LINE-COUNT.                                   CL**2
00308                                                                   EL6851
00309      MOVE PI-CONTROL-GRAND-TOT   TO EL685A-CNTL-TOTAL                CL**4
00310                                     EL685B-CNTL-TOTAL.               CL**4
00311      MOVE 'GRAND TOTAL'          TO EL685A-CNTL-DESC                 CL**4
00312                                     EL685B-CNTL-DESC.                CL**4
CIDMOD     MOVE 'NO. CHECKS '          TO EL685A-NO-CHECKS-DESC              000
CIDMOD                                    EL685B-NO-CHECKS-DESC.             000
CIDMOD     MOVE PI-TOT-CHECKS          TO EL685A-NO-CHECKS                   000
CIDMOD                                    EL685B-NO-CHECKS.                  000
00313                                                                      CL**4
00314      IF (PI-CALLING-PROGRAM = 'EL686   '  AND                        CL**4
00315           PI-PGM-PRINT-OPT = '5')         OR                         CL**4
00316          PI-ORIG-CALL-AR-MENU                                        CL**4
00317          MOVE TOTAL-LINE2        TO WS-PASSED-DATA                   CL**4
00318      ELSE                                                            CL**4
00319          MOVE TOTAL-LINE         TO WS-PASSED-DATA.                  CL**4
00320                                                                      CL**4
00321      MOVE '0'                    TO WS-PASSED-CNTL-CHAR.             CL**2
00322      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6851
00323      MOVE 'X'                    TO WS-PROG-END.                  EL6851
CIDMOD*    MOVE 'L'                    TO DRS-SW.                            000
CIDMOD*    MOVE SPACES                 TO WS-PASSED-DATA.                    000
CIDMOD*    MOVE SPACES                 TO WS-PASSED-CNTL-CHAR.               000
00324      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6851
00325      GO TO 0100-RETRIEVE-LOOP.                                    EL6851
00326                                                                   EL6851
00327  200-END-DATA.                                                    EL6851
00328                                                                      CL*12
00329 * DLO034 CLOSE                                                       CL*12
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL*12
00331         IF DL34-PROCESS-TYPE NOT EQUAL TO SPACES                     CL*12
00332              MOVE 'C'                TO DL34-PROCESS-TYPE            CL*12
00333              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL*12
00334              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL*12
00335              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL*12
00336              MOVE SPACES             TO DL34-PRINT-LINE              CL*12
00337                                         DL34-OVERRIDE-PRINTER-ID     CL*12
00338              EXEC CICS LINK                                          CL*12
00339                  PROGRAM    ('DLO034')                               CL*12
00340                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL*12
00341                  LENGTH     (DLO034-REC-LENGTH)                      CL*12
00342              END-EXEC                                                CL*12
00343                                                                      CL*12
00344              IF DL34-RETURN-CODE NOT = 'OK'                          CL*12
00345                  MOVE  '**DLO034 CLOSE ERROR - ABORT**'              CL*12
00346                                      TO ERROR-LINE                   CL*12
00347                  PERFORM 400-SEND-TEXT.                              CL*12
00348                                                                      CL*12
CIDMOD*    MOVE 'L'  TO  DRS-SW.                                             000
CIDMOD*    PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                              000
00349      EXEC CICS RETURN                                             EL6851
00350      END-EXEC.                                                       CL**2
00351                                                                   EL6851
00352  300-NOT-FOUND.                                                   EL6851
00353      MOVE 'NO COMMUNICATION AREA FOUND' TO ERROR-LINE.               CL**2
00354      PERFORM 400-SEND-TEXT.                                          CL**2
00355      GO TO 200-END-DATA.                                          EL6851
00356                                                                   EL6851
00357  400-SEND-TEXT.                                                   EL6851
00358      EXEC CICS SEND TEXT                                          EL6851
00359           FROM   (ERROR-LINE)                                        CL**2
00360           LENGTH (70)                                                CL**2
00361      END-EXEC.                                                       CL**2
00362      EJECT                                                        EL6851
00363  1000-INITIALIZE    SECTION.                                      EL6851
00364                                                                   EL6851
00365 *    NOTE ******************************************************* EL6851
00366 *         *                                                     * EL6851
00367 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL6851
00368 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL6851
00369 *         *                                                     * EL6851
00370 *         *******************************************************.EL6851
00371                                                                   EL6851
00372      IF PI-CALLING-PROGRAM = 'EL686   '                           EL6851
00373          MOVE LOW-VALUES               TO  PI-CK-COMPANY-CODE        CL**7
00374          MOVE ZEROS                    TO  PI-CK-SEQUENCE-NO         CL**7
00375      ELSE                                                         EL6851
00376          IF CHECKS-PRINTED                                           CL**7
00377              MOVE LOW-VALUES           TO  PI-CHECK-QUE-KEY          CL**7
00378              MOVE PI-START-CONTROL-NO  TO  PI-CK-CONTROL-NO          CL**7
00379          ELSE                                                        CL**7
00380              MOVE LOW-VALUES           TO  PI-CHECK-QUE-KEY.         CL**7
00381                                                                   EL6851
00382      MOVE LOW-VALUES             TO  PI-PREV-CHECK-QUE-KEY.       EL6851
00383                                                                   EL6851
00384      MOVE ZEROS                  TO  PI-CONTROL-TOT               EL6851
00385                                      PI-CONTROL-SAVE-CONTROL      EL6851
00386                                      PI-CONTROL-GRAND-TOT         EL6851
00387                                      PI-TOT-CHECKS                   CL**9
00388                                      WS-NO-CHECKS.                   CL**9
00389                                                                   EL6851
00390      MOVE 1                      TO  PI-PAGE                      EL6851
00391                                      PI-LINE-COUNT.                  CL**2
00392                                                                   EL6851
00393      MOVE PI-COMPANY-CD          TO  PI-CK-COMPANY-CODE.             CL**2
00394      MOVE 'Y'                    TO  PI-FIRST-TIME-SWT.              CL**2
00395                                                                   EL6851
00396      MOVE EIBTIME                TO  WS-TIME-WORK.                EL6851
00397                                                                   EL6851
00398      MOVE SAVE-DATE              TO  ADATE.                          CL**2
00399      MOVE WS-TIME                TO  ATIME.                       EL6851
00400      MOVE PI-COMPANY-ID          TO  ACOMP.                       EL6851
00401                                                                   EL6851
00402      PERFORM 2000-PRINT-HEADING.                                  EL6851
00403                                                                   EL6851
00404      EJECT                                                        EL6851
00405  2000-PRINT-HEADING   SECTION.                                    EL6851
00406                                                                      CL**4
00407      IF (PI-CALLING-PROGRAM = 'EL686   '  AND                        CL**4
00408           PI-PGM-PRINT-OPT = '5')                                    CL**4
00409              MOVE WS-AR-CHECKS-RELEASED-DESC  TO ATITLEO             CL**4
00410              GO TO 2000-CONTINUE.                                    CL**4
00411                                                                      CL**4
00412      IF CHECKS-TO-BE-PRINTED                                         CL**4
00413          IF PI-ORIG-CALL-AR-MENU                                     CL**4
00414              MOVE WS-AR-TO-BE-PRINTED-DESC    TO ATITLEO             CL**4
00415          ELSE                                                        CL**4
00416              MOVE WS-TO-BE-PRINTED-DESC       TO ATITLEO             CL**4
00417      ELSE                                                            CL**4
00418          IF PI-ORIG-CALL-AR-MENU                                     CL**4
00419              MOVE WS-AR-CHECKS-PRINTED-DESC   TO ATITLEO             CL**4
00420          ELSE                                                        CL**4
00421              MOVE WS-CHECKS-PRINTED-DESC      TO ATITLEO.            CL**4
00422                                                                      CL**4
00423  2000-CONTINUE.                                                      CL**4
00424      MOVE PI-PAGE                TO PAGE-NO.                         CL**2
00425      MOVE HEADING-1              TO WS-PASSED-DATA.                  CL**2
00426      MOVE '1'                    TO WS-PASSED-CNTL-CHAR.             CL**2
00427      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6851
00428      MOVE 1                      TO PI-LINE-COUNT.                EL6851
00429                                                                   EL6851
00430      IF PI-COMPANY-ID = 'LAP'  OR  'RMC'                             CL**8
00431          MOVE ' ACCOUNT'         TO HD2-DESC-1                       CL**8
00432          MOVE 'CERT NO.'         TO HD2-DESC-2.                      CL**8
00433                                                                      CL**8
00434      IF (PI-CALLING-PROGRAM = 'EL686   '  AND                        CL**4
00435           PI-PGM-PRINT-OPT = '5')         OR                         CL**4
00436          PI-ORIG-CALL-AR-MENU                                        CL**4
00437      MOVE HEADING-2B             TO WS-PASSED-DATA                   CL**4
00438      ELSE                                                            CL**4
00439      MOVE HEADING-2              TO WS-PASSED-DATA.                  CL**2
00440                                                                      CL**4
00441      MOVE '0'                    TO WS-PASSED-CNTL-CHAR.             CL**2
00442      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6851
00443      ADD 2                       TO PI-LINE-COUNT.                EL6851
00444                                                                   EL6851
00445      EJECT                                                        EL6851
00446  3000-BUMP-LINE-COUNT   SECTION.                                  EL6851
00447      IF WS-PASSED-CNTL-CHAR = SPACES                              EL6851
00448         ADD 1                    TO PI-LINE-COUNT                 EL6851
00449        ELSE                                                          CL**2
00450         ADD  2                   TO PI-LINE-COUNT.                EL6851
00451                                                                   EL6851
00452      IF PI-LINE-COUNT GREATER 60                                     CL**2
00453         MOVE ZEROS TO PI-LINE-COUNT                               EL6851
00454         ADD 1   TO PI-PAGE                                        EL6851
00455         PERFORM 2000-PRINT-HEADING.                               EL6851
00456                                                                   EL6851
00457      EJECT                                                        EL6851
00458                                                                      CL**2
00459  4000-BROWSE-CHECK-QUEUE-FILE SECTION.                            EL6851
00460                                                                      CL**2
00461      IF (PI-CALLING-PROGRAM = 'EL686   '  AND                        CL**4
00462           PI-PGM-PRINT-OPT = '5')         OR                         CL**4
00463          PI-ORIG-CALL-AR-MENU                                        CL**4
00464               PERFORM 5000-BROWSE-COMM-CHECK-QUEUE                   CL**2
00465               GO TO 4990-EXIT.                                       CL**2
00466                                                                   EL6851
00467      EXEC CICS STARTBR                                            EL6851
00468          DATASET (WS-CHECK-QUEUE-DSID)                            EL6851
00469          RIDFLD  (PI-CHECK-QUE-KEY)                               EL6851
00470          GTEQ                                                        CL**2
00471      END-EXEC.                                                       CL**2
00472                                                                   EL6851
00473  4100-READNEXT.                                                   EL6851
00474      MOVE PI-CHECK-QUE-KEY        TO  PI-PREV-CHECK-QUE-KEY.         CL**2
00475                                                                   EL6851
00476      EXEC CICS READNEXT                                           EL6851
00477          DATASET (WS-CHECK-QUEUE-DSID)                            EL6851
00478          RIDFLD  (PI-CHECK-QUE-KEY)                               EL6851
00479          SET     (ADDRESS OF CHECK-QUE)                              CL*11
00480      END-EXEC.                                                       CL**2
00481                                                                   EL6851
00482      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD                         EL6851
00483          GO TO 4800-END-OF-FILE.                                  EL6851
00484                                                                   EL6851
00485      IF CQ-ENTRY-TYPE NOT = 'Q'                                   EL6851
00486          GO TO 4100-READNEXT.                                        CL**8
00487                                                                      CL**8
00488      IF CQ-VOID-INDICATOR = 'V'                                      CL**8
00489          GO TO 4100-READNEXT.                                        CL**8
00490                                                                      CL**8
00491      IF CQ-CHECK-AMOUNT = ZEROS                                      CL**8
00492          GO TO 4100-READNEXT.                                     EL6851
00493                                                                   EL6851
00494      IF PI-CALLING-PROGRAM IS EQUAL TO 'EL686   '                    CL**7
00495          IF PI-CONTROL-SAVE-CONTROL NOT EQUAL CQ-CONTROL-NUMBER      CL**7
00496              IF NOT PI-FIRST-TIME                                    CL**7
00497                  GO TO 4800-END-OF-FILE.                             CL**7
00498                                                                      CL**7
00499      IF CQ-TIMES-PRINTED NOT = ZERO                               EL6851
00500          IF CHECKS-PRINTED                                           CL**4
00501              NEXT SENTENCE                                           CL**4
00502          ELSE                                                        CL**4
00503              GO TO 4100-READNEXT                                     CL**4
00504      ELSE                                                            CL**4
00505          IF CHECKS-TO-BE-PRINTED                                     CL**4
00506              NEXT SENTENCE                                           CL**4
00507          ELSE                                                        CL**4
00508              GO TO 4100-READNEXT.                                    CL**4
00509                                                                   EL6851
00510      IF PI-CONTROL-SAVE-CONTROL NOT = CQ-CONTROL-NUMBER           EL6851
00511         IF PI-FIRST-TIME                                          EL6851
00512            NEXT SENTENCE                                          EL6851
00513         ELSE                                                      EL6851
00514            MOVE SPACES           TO  TOTAL-LINE                   EL6851
00515            MOVE 'CONTL TOTAL'    TO EL685A-CNTL-DESC              EL6851
00516            MOVE PI-CONTROL-TOT   TO EL685A-CNTL-TOTAL             EL6851
00517            MOVE 'NO. CHECKS '    TO EL685A-NO-CHECKS-DESC         EL6851
00518            MOVE WS-NO-CHECKS     TO EL685A-NO-CHECKS              EL6851
00519            MOVE TOTAL-LINE       TO WS-PASSED-DATA                EL6851
00520            MOVE ' '              TO WS-PASSED-CNTL-CHAR           EL6851
00521            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                    EL6851
00522            PERFORM 3000-BUMP-LINE-COUNT                           EL6851
00523            MOVE SPACES           TO WS-PASSED-DATA                EL6851
00524            MOVE ' '              TO WS-PASSED-CNTL-CHAR           EL6851
00525            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                    EL6851
00526            PERFORM 3000-BUMP-LINE-COUNT                           EL6851
00527            MOVE ZEROS            TO PI-CONTROL-TOT                EL6851
00528                                     WS-NO-CHECKS.                 EL6851
00529                                                                   EL6851
00530      MOVE CQ-CONTROL-NUMBER      TO PI-CONTROL-SAVE-CONTROL.      EL6851
00531      MOVE 'N'                    TO PI-FIRST-TIME-SWT.            EL6851
00532                                                                   EL6851
00533      ADD +1                      TO WS-NO-CHECKS                  EL6851
00534                                     PI-TOT-CHECKS.                EL6851
00535      MOVE SPACES                 TO DETAIL-LINE.                     CL**2
00536      ADD CQ-CHECK-AMOUNT         TO PI-CONTROL-TOT                EL6851
00537                                     PI-CONTROL-GRAND-TOT.         EL6851
00538      MOVE CQ-CONTROL-NUMBER      TO EL685A-CONTROL.                  CL**2
00539      MOVE CQ-CHECK-NUMBER        TO EL685A-CHECK-NO.                 CL**2
00540                                                                   EL6851
00541      IF CQ-BILLING-CREDIT                                         EL6851
00542          MOVE 'BILL CREDIT'      TO EL685A-PMT-TYPE               EL6851
00543      ELSE                                                         EL6851
00544          IF CQ-REFUND-PMT                                         EL6851
00545              MOVE 'REFUND PMT '  TO EL685A-PMT-TYPE               EL6851
00546          ELSE                                                     EL6851
00547              MOVE 'CHECK MAINT'  TO EL685A-PMT-TYPE.              EL6851
00548                                                                   EL6851
00549      IF CQ-CHECK-MAINT-PMT OR CQ-REFUND-PMT                          CL**7
00550          IF PI-COMPANY-ID = 'LAP'  OR  'RMC'                         CL**8
00551              MOVE CQ-CHEK-CARRIER    TO  EL685A-CARRIER              CL**8
00552              MOVE CQ-CHEK-GROUPING   TO  EL685A-GROUP                CL**8
00553              MOVE CQ-CHEK-ACCOUNT    TO  EL685A-FIN-RESP             CL**8
00554              MOVE CQ-CHEK-CERT-NO    TO  EL685A-ACCOUNT              CL**8
00555          ELSE                                                        CL**8
00556              MOVE CQ-CHEK-CARRIER    TO  EL685A-CARRIER              CL**8
00557              MOVE CQ-CHEK-GROUPING   TO  EL685A-GROUP                CL**8
00558              MOVE CQ-CHEK-FIN-RESP   TO  EL685A-FIN-RESP             CL**8
00559              MOVE CQ-CHEK-ACCOUNT    TO  EL685A-ACCOUNT              CL**8
00560      ELSE                                                         EL6851
00561          MOVE CQ-PYAJ-CARRIER    TO  EL685A-CARRIER               EL6851
00562          MOVE CQ-PYAJ-GROUPING   TO  EL685A-GROUP                 EL6851
00563          MOVE CQ-PYAJ-FIN-RESP   TO  EL685A-FIN-RESP              EL6851
00564          MOVE CQ-PYAJ-ACCOUNT    TO  EL685A-ACCOUNT.              EL6851
00565                                                                   EL6851
00566      MOVE CQ-CHECK-AMOUNT        TO  EL685A-AMT.                     CL**2
00567      MOVE DETAIL-LINE            TO WS-PASSED-DATA.                  CL**2
00568      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.             CL**2
00569      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL6851
00570      PERFORM 3000-BUMP-LINE-COUNT.                                   CL**2
00571                                                                   EL6851
00572      GO TO 4100-READNEXT.                                         EL6851
00573                                                                   EL6851
00574  4800-END-OF-FILE.                                                EL6851
00575      IF PI-PREV-CHECK-QUE-KEY  = LOW-VALUES                       EL6851
00576         GO TO 4990-EXIT.                                          EL6851
00577                                                                   EL6851
00578      EXEC CICS ENDBR                                              EL6851
00579          DATASET (WS-CHECK-QUEUE-DSID)                               CL**2
00580      END-EXEC.                                                       CL**2
00581                                                                   EL6851
00582  4990-EXIT.                                                       EL6851
00583      EXIT.                                                        EL6851
00584      EJECT                                                           CL**2
00585  5000-BROWSE-COMM-CHECK-QUEUE SECTION.                               CL**2
00586                                                                      CL**2
00587      EXEC CICS STARTBR                                               CL**2
00588          DATASET (WS-COMCK-QUEUE-DSID)                               CL**2
00589          RIDFLD  (PI-CHECK-QUE-KEY)                                  CL**2
00590          GTEQ                                                        CL**2
00591      END-EXEC.                                                       CL**2
00592                                                                      CL**2
00593      EXEC CICS HANDLE CONDITION                                      CL*13
00594           ENDFILE (5800-END-OF-FILE)                                 CL*13
00595      END-EXEC.                                                       CL*13
00596                                                                      CL*13
00597  5100-READNEXT.                                                      CL**2
00598      MOVE PI-CHECK-QUE-KEY        TO  PI-PREV-CHECK-QUE-KEY.         CL**2
00599                                                                      CL**2
00600      EXEC CICS READNEXT                                              CL**2
00601          DATASET (WS-COMCK-QUEUE-DSID)                               CL**2
00602          RIDFLD  (PI-CHECK-QUE-KEY)                                  CL**2
00603          SET     (ADDRESS OF COMMISSION-CHECK-QUE)                   CL*11
00604      END-EXEC.                                                       CL**2
00605                                                                      CL**2
00606      IF MQ-COMPANY-CD NOT = PI-COMPANY-CD                            CL**2
00607          GO TO 5800-END-OF-FILE.                                     CL**2
00608                                                                      CL**6
00609      IF MQ-TEXT                                                      CL**6
00610          GO TO 5100-READNEXT.                                        CL**6
00611                                                                      CL**2
00612      IF MQ-ENTRY-TYPE NOT = 'Q'                                      CL**2
00613          GO TO 5100-READNEXT.                                        CL**2
00614                                                                      CL**2
00615      IF MQ-VOID-DT NOT = LOW-VALUES                                  CL**3
00616          GO TO 5100-READNEXT.                                        CL**3
00617                                                                      CL**7
00618      IF PI-CALLING-PROGRAM IS EQUAL TO 'EL686   '                    CL**7
00619          IF PI-CONTROL-SAVE-CONTROL NOT EQUAL MQ-CONTROL-NUMBER      CL**7
00620              IF NOT PI-FIRST-TIME                                    CL**7
00621                  GO TO 5800-END-OF-FILE.                             CL**7
00622                                                                      CL**3
00623      IF PI-CALLING-PROGRAM = 'EL686   '   AND                        CL**4
00624           PI-PGM-PRINT-OPT = '5'          AND                        CL**4
00625           MQ-TIMES-PRINTED NOT = ZERO                                CL**4
00626             GO TO 5100-READNEXT.                                     CL**4
00627                                                                      CL**4
00628      IF MQ-TIMES-PRINTED NOT = ZERO                                  CL**2
00629          IF CHECKS-PRINTED                                           CL**4
00630             NEXT SENTENCE                                            CL**4
00631          ELSE                                                        CL**4
00632             GO TO 5100-READNEXT                                      CL**4
00633      ELSE                                                            CL**4
00634          IF CHECKS-TO-BE-PRINTED                                     CL**4
00635             NEXT SENTENCE                                            CL**4
00636          ELSE                                                        CL**4
00637             GO TO 5100-READNEXT.                                     CL**4
00638                                                                      CL**2
00639      IF PI-CONTROL-SAVE-CONTROL NOT = MQ-CONTROL-NUMBER              CL**2
00640         IF PI-FIRST-TIME                                             CL**2
00641            NEXT SENTENCE                                             CL**2
00642         ELSE                                                         CL**2
00643            MOVE SPACES           TO  TOTAL-LINE2                     CL**4
00644            MOVE 'CONTL TOTAL'    TO EL685B-CNTL-DESC                 CL**4
00645            MOVE PI-CONTROL-TOT   TO EL685B-CNTL-TOTAL                CL**4
CIDMOD           MOVE 'NO. CHECKS '    TO EL685B-NO-CHECKS-DESC              000
CIDMOD           MOVE WS-NO-CHECKS     TO EL685B-NO-CHECKS                   000
00646            MOVE TOTAL-LINE2      TO WS-PASSED-DATA                   CL**4
00647            MOVE ' '              TO WS-PASSED-CNTL-CHAR              CL**2
00648            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                       CL**2
00649            PERFORM 3000-BUMP-LINE-COUNT                              CL**2
00650            MOVE SPACES           TO WS-PASSED-DATA                   CL**2
00651            MOVE ' '              TO WS-PASSED-CNTL-CHAR              CL**2
00652            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                       CL**2
00653            PERFORM 3000-BUMP-LINE-COUNT                              CL**2
00654            MOVE ZEROS            TO PI-CONTROL-TOT                   CL**2
00655                                     WS-NO-CHECKS.                    CL**2
00656                                                                      CL**2
00657      MOVE MQ-CONTROL-NUMBER      TO PI-CONTROL-SAVE-CONTROL.         CL**2
00658      MOVE 'N'                    TO PI-FIRST-TIME-SWT.               CL**2
00659                                                                      CL**2
00660      ADD +1                      TO WS-NO-CHECKS                     CL**2
00661                                     PI-TOT-CHECKS.                   CL**2
00662      MOVE SPACES                 TO DETAIL-LINE.                     CL**2
00663      ADD MQ-CHECK-AMOUNT         TO PI-CONTROL-TOT                   CL**2
00664                                     PI-CONTROL-GRAND-TOT.            CL**2
00665      MOVE MQ-CONTROL-NUMBER      TO EL685B-CONTROL.                  CL**4
00666      MOVE MQ-CHECK-NUMBER        TO EL685B-CHECK-NO.                 CL**4
00667      MOVE MQ-CHECK-AMOUNT        TO  EL685B-AMT.                     CL**4
00668      MOVE MQ-CHEK-CARRIER        TO  EL685B-CARRIER.                 CL**4
00669      MOVE MQ-CHEK-GROUPING       TO  EL685B-GROUP.                   CL**4
00670      MOVE MQ-CHEK-PAYEE          TO  EL685B-PAYEE.                   CL**4
00671      MOVE MQ-PAYEE-NAME          TO  EL685B-PAYEE-NA.                CL**4
00672      MOVE MQ-PAYEE-SEQ-A1        TO  EL685B-PAYEE-SEQ.               CL**4
00673                                                                      CL**2
00674      MOVE MQ-CHECK-WRITTEN-DT    TO  DC-BIN-DATE-1.                  CL**4
00675      MOVE ' '                    TO  DC-OPTION-CODE.                 CL**4
00676      PERFORM 9950-LINK-DATE-CONVERT THRU 9950-EXIT.                  CL**4
00677      MOVE DC-GREG-DATE-1-EDIT    TO  EL685B-CHK-DT.                  CL**4
00678                                                                      CL**4
00679      MOVE DETAIL-LINEB           TO WS-PASSED-DATA.                  CL**4
00680      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.             CL**2
00681      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL**2
00682      PERFORM 3000-BUMP-LINE-COUNT                                    CL**2
00683                                                                      CL**2
00684      GO TO 5100-READNEXT.                                            CL**2
00685                                                                      CL**2
00686  5800-END-OF-FILE.                                                   CL**2
00687      IF PI-PREV-CHECK-QUE-KEY  = LOW-VALUES                          CL**2
00688         GO TO 5990-EXIT.                                             CL**2
00689                                                                      CL**2
00690      EXEC CICS ENDBR                                                 CL**2
00691          DATASET (WS-COMCK-QUEUE-DSID) END-EXEC.                     CL**2
00692                                                                      CL**2
00693  5990-EXIT.                                                          CL**2
00694      EXIT.                                                           CL*13
00695      EJECT                                                        EL6851
00696  9500-PRINT-ROUTINE SECTION.  COPY ELPRTCVP.                      EL6851
00697                                                                      CL*12
00698                                                                   EL6851
00699  9900-LAST-PARAGRAPH SECTION.                                     EL6851
00700      GOBACK.                                                      EL6851
00701                                                                   EL6851
00702  9950-LINK-DATE-CONVERT.                                          EL6851
00703                                                                   EL6851
00704      EXEC CICS LINK                                               EL6851
00705          PROGRAM    ('ELDATCV')                                   EL6851
00706          COMMAREA   (DATE-CONVERSION-DATA)                        EL6851
00707          LENGTH     (DC-COMM-LENGTH)                              EL6851
00708          END-EXEC.                                                EL6851
00709  9950-EXIT.                                                       EL6851
00710      EXIT.                                                        EL6851
00711                                                                   EL6851
