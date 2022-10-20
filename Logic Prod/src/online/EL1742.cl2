00001  IDENTIFICATION DIVISION.                                         04/03/97
00002                                                                   EL1742
00003  PROGRAM-ID.                 EL1742.                                 LV010
00004 *              PROGRAM CONVERTED BY                                  CL**8
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**8
00006 *              CONVERSION DATE 02/13/96 09:34:05.                    CL**8
00007 *                            VMOD=2.010                              CL*10
00008 *                                                                 EL1742
00009 *AUTHOR.    LOGIC, INC.                                              CL**8
00010 *           DALLAS, TEXAS.                                           CL**8
00011                                                                   EL1742
00012 *DATE-COMPILED.                                                      CL**8
00013                                                                   EL1742
00014 *SECURITY.   *****************************************************   CL**8
00015 *            *                                                   *   CL**8
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**8
00017 *            *                                                   *   CL**8
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**8
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**8
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**8
00021 *            *                                                   *   CL**8
00022 *            *****************************************************   CL**8
00023                                                                   EL1742
00024 *REMARKS.                                                            CL**2
00025 *        THIS PROGRAM PRINTS A REPORT SHOWING ALL CHECKS THAT        CL**2
00026 *    ARE WAITING TO BE PRINTED.                                      CL**2
00027                                                                   EL1742
00028 *    ENTERED BY  - EL174   EX64 - CHECKS TO BE PRINTED REPORT.       CL**2
00029                                                                   EL1742
00030      EJECT                                                        EL1742
00031  ENVIRONMENT DIVISION.                                            EL1742
00032                                                                   EL1742
00033  DATA DIVISION.                                                   EL1742
00034                                                                   EL1742
00035  WORKING-STORAGE SECTION.                                         EL1742
00036                                                                   EL1742
00037  77  THIS-PGM PIC X(6)  VALUE 'EL1742'.                              CL**9
00038  77  FILLER  PIC X(32)  VALUE '********************************'. EL1742
00039  77  FILLER  PIC X(32)  VALUE '*   EL1742 WORKING STORAGE     *'. EL1742
00040  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.010 *********'.    CL*10
00041                                                                   EL1742
00042      COPY ELCDMD34.                                                  CL**9
00043  01  WS-DATE-AREA.                                                EL1742
00044      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL1742
00045      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL1742
00046                                                                   EL1742
00047  01  FILLER                          COMP-3.                      EL1742
00048      05  WS-READNEXT-SW              PIC S9          VALUE ZERO.  EL1742
00049                                                                   EL1742
00050      05  WS-TIME-WORK                PIC S9(7)       VALUE ZERO.  EL1742
00051      05  WS-TIME                     REDEFINES                    EL1742
00052          WS-TIME-WORK                PIC S9(3)V9(4).              EL1742
00053                                                                   EL1742
00054  01  FILLER.                                                      EL1742
00055      05  WS-ACTIVITY-TRAILERS-KEY.                                EL1742
00056          10  WS-ATK-COMPANY-CD       PIC X.                       EL1742
00057          10  WS-ATK-CARRIER          PIC X.                       EL1742
00058          10  WS-ATK-CLAIM-NO         PIC X(7).                    EL1742
00059          10  WS-ATK-CERT-NO          PIC X(11).                   EL1742
00060          10  WS-ATK-SEQUENCE-NO      PIC S9(4)  COMP.                CL**4
00061                                                                   EL1742
00062      05  ERROR-LINE                 PIC X(80).                    EL1742
00063      05  WS-CHECK-AIX-DSID          PIC X(8) VALUE 'ELCHKQ2'.        CL**7
00064      05  WS-ACTIVITY-TRAILERS-DSID  PIC X(8) VALUE 'ELTRLR'.      EL1742
00065      05  WS-NO-CHECKS-L             PIC S9(4) VALUE +0.              CL**4
00066      05  WS-NO-CHECKS-A             PIC S9(4) VALUE +0.              CL**4
00067      05  WS-NO-CHECKS               PIC S9(4) VALUE +0.           EL1742
00068                                                                   EL1742
00069      COPY ELCINTF.                                                   CL**5
00070                                                                   EL1742
00071      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                      CL**4
00072          16  PI-CHECK-AIX-KEY.                                       CL**7
00073              20  PI-CK-COMPANY-CODE     PIC X.                    EL1742
00074              20  PI-CK-CONTROL-NO       PIC S9(8)          COMP.  EL1742
00075              20  PI-CK-CARRIER          PIC X.                       CL*10
00076              20  PI-CK-GROUPING         PIC X(6).                    CL*10
00077              20  PI-CK-STATE            PIC XX.                      CL*10
00078              20  PI-CK-BENE-ACCT        PIC X(10).                   CL**7
00079              20  PI-CK-SEQUENCE-NO      PIC S9(4)          COMP.  EL1742
00080                                                                   EL1742
00081          16  PI-PREV-CHECK-AIX-KEY.                                  CL**7
00082              20  PI-PREV-CK-COMPANY-CODE     PIC X.               EL1742
00083              20  PI-PREV-CK-CONTROL-NO       PIC S9(8) COMP.      EL1742
00084              20  PI-PREV-CK-CARRIER          PIC X.                  CL*10
00085              20  PI-PREV-CK-GROUPING         PIC X(06).              CL**7
00086              20  PI-PREV-CK-STATE            PIC XX.                 CL*10
00087              20  PI-PREV-CK-BENE-ACCT        PIC X(10).              CL**7
00088              20  PI-PREV-CK-SEQUENCE-NO      PIC S9(4) COMP.      EL1742
00089                                                                   EL1742
00090          16 FILLER                       PIC XXX.                    CL**4
00091                                                                   EL1742
00092          16 PI-CONTROL-TOTALS.                                    EL1742
00093             20 PI-CONTROL-TOT            PIC S9(7)V99 COMP-3.     EL1742
00094             20 PI-CONTROL-GRAND-TOT      PIC S9(7)V99 COMP-3.     EL1742
00095             20 PI-CONTROL-SAVE-CONTROL   PIC S9(8) COMP.          EL1742
00096                                                                   EL1742
00097          16 PI-FIRST-TIME-SWT        PIC X.                       EL1742
00098             88 PI-FIRST-TIME         VALUE 'Y'.                   EL1742
00099             88 PI-NOT-FIRST-TIME     VALUE 'N'.                   EL1742
00100                                                                      CL**3
00101          16 FILLER                   PIC XX.                         CL**4
00102                                                                      CL**3
00103          16 PI-CHECK-MODE            PIC X.                          CL**4
00104             88 CHECKS-TO-BE-PRINTED  VALUE SPACE.                    CL**3
00105             88 CHECKS-RELEASED       VALUE 'R'.                      CL**7
00106             88 CHECKS-PRINTED        VALUE 'Y'.                      CL**7
00107                                                                      CL**3
00108          16 PI-START-CONTROL-NO      PIC S9(8)   COMP.               CL**4
00109                                                                      CL**3
00110          16 PI-PAGE                  PIC S999     COMP-3.            CL**3
00111          16 PI-LINE-COUNT            PIC S999     COMP-3.            CL**3
00112                                                                   EL1742
00113          16 PI-TOT-CHECKS            PIC S9(4).                   EL1742
00114                                                                   EL1742
00115          16 PI-ADDTL-TOTALS.                                         CL**4
00116             20 PI-CONTROL-TOT-L          PIC S9(7)V99 COMP-3.        CL**4
00117             20 PI-CONTROL-TOT-A          PIC S9(7)V99 COMP-3.        CL**4
00118             20 PI-CONTROL-GRAND-TOT-L    PIC S9(7)V99 COMP-3.        CL**4
00119             20 PI-CONTROL-GRAND-TOT-A    PIC S9(7)V99 COMP-3.        CL**4
00120             20 PI-TOT-CHECKS-L           PIC S9(4).                  CL**4
00121             20 PI-TOT-CHECKS-A           PIC S9(4).                  CL**4
00122                                                                      CL**4
00123          16  PI-END-CONTROL-NO       PIC S9(08)    COMP.             CL**7
00124          16  PI-MONTH-END-SAVE       PIC XX.                         CL*10
00125          16  PI-NON-CASH-REL-CNT     PIC S9(05)    COMP-3.           CL**7
00126          16  PI-NON-CASH-REL-AMT     PIC S9(9)V99  COMP-3.           CL**7
00127                                                                      CL**7
00128          16  FILLER                  PIC X(512).                     CL**8
00129  EJECT                                                               CL**4
00130  01  HEADING-1.                                                      CL**4
00131      12  FILLER                  PIC XX      VALUE SPACES.        EL1742
00132      12  ADATE                   PIC X(8).                        EL1742
00133      12  FILLER                  PIC XX      VALUE SPACES.        EL1742
00134      12  ATIME                   PIC 99.99.                       EL1742
00135      12  FILLER                  PIC X(8)    VALUE SPACES.           CL**4
00136      12  ACOMP                   PIC XXX.                            CL**4
00137      12  FILLER                  PIC X       VALUE SPACES.        EL1742
00138      12  HDG1                    PIC X(25)   VALUE SPACES.           CL**7
00139      12  FILLER                  PIC X(7)    VALUE SPACES.           CL**7
00140      12  FILLER                  PIC X(5)    VALUE 'PAGE'.        EL1742
00141      12  PAGE-NO                 PIC ZZ9.                         EL1742
00142      12  FILLER                  PIC X(3)    VALUE SPACES.           CL**7
00143      12  HDG1-RPT                PIC X(6)    VALUE 'EL174A'.         CL**7
00144                                                                   EL1742
00145  01  FILLER.                                                         CL**3
00146      12  VAR-HEADING1A           PIC X(24)   VALUE                   CL**3
00147              '- CHECKS TO BE PRINTED -'.                             CL**3
00148      12  VAR-HEADING1B           PIC X(24)   VALUE                   CL**3
00149              '-    PRINTED CHECKS    -'.                             CL**3
00150      12  VAR-HEADING1C           PIC X(27)   VALUE                   CL**7
00151              '- RELEASED CONTROL GROUPS -'.                          CL**7
00152                                                                   EL1742
00153  01  HEADING-2.                                                   EL1742
00154      12  FILLER                  PIC XX      VALUE SPACES.        EL1742
00155      12  FILLER                  PIC X(54)                           CL**5
00156      VALUE 'CONTROL CHK NO  PMT TYPE  L/A CLAIM  CAR CERT NO'.       CL**5
00157      12  FILLER                  PIC X(22)                           CL**5
00158      VALUE 'PMT AMOUNT  PAYEE   BY'.                                 CL**5
00159      EJECT                                                        EL1742
00160                                                                   EL1742
00161  01  DETAIL-LINE.                                                 EL1742
00162          15  FILLER                  PIC XX.                      EL1742
00163          15  EL174A-CONTROL          PIC 9(7).                    EL1742
00164          15  FILLER                  PIC X.                          CL**5
00165          15  EL174A-CHECK-NO         PIC X(7).                    EL1742
00166          15  FILLER                  PIC X.                       EL1742
00167          15  EL174A-PMT-TYPE         PIC X(10).                      CL**5
00168          15  FILLER                  PIC X.                          CL**5
00169          15  EL174A-COV-TYPE         PIC X.                          CL**5
00170          15  FILLER                  PIC X.                       EL1742
00171          15  EL174A-CLAIM-NO         PIC X(7).                    EL1742
00172          15  FILLER                  PIC XX.                      EL1742
00173          15  EL174A-CARRIER          PIC X.                       EL1742
00174          15  FILLER                  PIC X.                       EL1742
00175          15  EL174A-CERT-NO          PIC X(11).                   EL1742
00176          15  FILLER                  PIC X.                       EL1742
00177          15  EL174A-AMT              PIC Z,ZZZ,ZZ9.99-.           EL1742
00178          15  FILLER                  PIC X.                       EL1742
00179          15  EL174A-PAYEE            PIC X(7).                    EL1742
00180          15  FILLER                  PIC X.                       EL1742
00181          15  EL174A-BY               PIC X(4).                    EL1742
00182                                                                   EL1742
00183  01  TOTAL-LINE                      REDEFINES                    EL1742
00184      DETAIL-LINE.                                                 EL1742
00185          15  FILLER                  PIC X(13).                   EL1742
00186          15  EL174A-NO-CHECKS-DESC   PIC X(13).                      CL**5
00187          15  EL174A-NO-CHECKS        PIC ZZZ9.                    EL1742
00188          15  FILLER                  PIC X(10).                      CL**5
00189          15  EL174A-CNTL-DESC        PIC X(12).                   EL1742
00190          15  EL174A-CNTL-TOTAL       PIC Z,ZZZ,ZZ9.99-.           EL1742
00191          15  FILLER                  PIC X(15).                   EL1742
00192      EJECT                                                        EL1742
00193      COPY ELPRTCVD.                                                  CL**5
00194      EJECT                                                        EL1742
00195      COPY ELCDATE.                                                   CL**5
00196      EJECT                                                        EL1742
00197  LINKAGE SECTION.                                                 EL1742
00198                                                                   EL1742
00199      COPY ELCCHKQ.                                                   CL**5
00200      EJECT                                                        EL1742
00201      COPY ELCTRLR.                                                   CL**5
00202      EJECT                                                        EL1742
00203  PROCEDURE DIVISION.                                              EL1742
00204                                                                   EL1742
00205      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL1742
00206      MOVE '5'                   TO DC-OPTION-CODE.                EL1742
00207      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1742
00208      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL1742
00209      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL1742
00210                                                                   EL1742
00211      MOVE 80                     TO WS-LINE-LEN.                  EL1742
00212                                                                      CL**4
00213      MOVE SPACES                 TO DL34-PROCESS-TYPE.               CL**9
00214                                                                      CL**9
00215  0100-RETRIEVE-LOOP.                                              EL1742
00216      EXEC CICS HANDLE CONDITION                                   EL1742
00217           ENDDATA    (200-END-DATA)                               EL1742
00218           ENDFILE    (4800-END-OF-FILE)                           EL1742
00219           NOTFND     (300-NOT-FOUND)                              EL1742
00220      END-EXEC.                                                       CL**4
00221                                                                      CL**4
00222      EXEC CICS RETRIEVE                                           EL1742
00223           INTO(PROGRAM-INTERFACE-BLOCK)                           EL1742
00224           LENGTH(PI-COMM-LENGTH)                                  EL1742
00225      END-EXEC.                                                       CL**4
00226                                                                      CL**9
00227 * DLO034 OPEN WHEN DMD OR CID                                        CL**9
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**9
00229          IF DL34-PROCESS-TYPE = SPACES                               CL*10
00230              MOVE 'O'                TO DL34-PROCESS-TYPE            CL**9
00231              MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID              CL**9
00232              MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID        CL**9
00233              MOVE PI-PROCESSOR-ID    TO DL34-USERID                  CL**9
00234              MOVE SPACES             TO DL34-PRINT-LINE              CL**9
00235              MOVE PI-ALT-DMD-PRT-ID  TO DL34-OVERRIDE-PRINTER-ID     CL**9
00236              EXEC CICS LINK                                          CL**9
00237                  PROGRAM    ('DLO034')                               CL**9
00238                  COMMAREA   (DLO034-COMMUNICATION-AREA)              CL**9
00239                  LENGTH     (DLO034-REC-LENGTH)                      CL**9
00240              END-EXEC                                                CL**9
00241              IF DL34-RETURN-CODE NOT = 'OK'                          CL**9
00242                  MOVE  '**DLO034 OPEN ERROR - ABORT**'               CL**9
00243                                      TO ERROR-LINE                   CL**9
00244                  PERFORM 400-SEND-TEXT                               CL**9
00245                  EXEC CICS                                           CL**9
00246                      RETURN                                          CL**9
00247                  END-EXEC.                                           CL**9
00248                                                                   EL1742
00249      PERFORM 1000-INITIALIZE.                                     EL1742
00250                                                                   EL1742
00251      PERFORM 4000-BROWSE-CHECK-QUEUE-FILE.                        EL1742
00252                                                                   EL1742
00253      MOVE SPACES                 TO TOTAL-LINE.                      CL**4
00254      MOVE PI-LIFE-OVERRIDE-L6    TO EL174A-NO-CHECKS-DESC.           CL**4
00255      MOVE WS-NO-CHECKS-L         TO EL174A-NO-CHECKS.                CL**4
00256      MOVE PI-CONTROL-TOT-L       TO EL174A-CNTL-TOTAL.               CL**4
00257      MOVE TOTAL-LINE             TO WS-PASSED-DATA.                  CL**4
00258      MOVE '0'                    TO WS-PASSED-CNTL-CHAR.             CL**4
00259      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1742
00260      PERFORM 3000-BUMP-LINE-COUNT.                                   CL**4
00261                                                                      CL**4
00262      MOVE SPACES                 TO TOTAL-LINE.                      CL**4
00263      MOVE PI-AH-OVERRIDE-L6      TO EL174A-NO-CHECKS-DESC.           CL**4
00264      MOVE WS-NO-CHECKS-A         TO EL174A-NO-CHECKS.                CL**4
00265      MOVE PI-CONTROL-TOT-A       TO EL174A-CNTL-TOTAL.               CL**4
00266      MOVE TOTAL-LINE             TO WS-PASSED-DATA.                  CL**4
00267      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.             CL**4
00268      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL**4
00269      PERFORM 3000-BUMP-LINE-COUNT.                                   CL**4
00270                                                                   EL1742
00271      MOVE SPACES                 TO TOTAL-LINE.                      CL**4
00272      MOVE 'NO. CHECKS'           TO EL174A-NO-CHECKS-DESC.           CL**4
00273      MOVE WS-NO-CHECKS           TO EL174A-NO-CHECKS.                CL**4
00274      MOVE 'CONTL TOTAL'          TO EL174A-CNTL-DESC.                CL**4
00275      MOVE PI-CONTROL-TOT         TO EL174A-CNTL-TOTAL.               CL**4
00276      MOVE TOTAL-LINE             TO WS-PASSED-DATA.                  CL**4
00277      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.             CL**4
00278      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL**4
00279      PERFORM 3000-BUMP-LINE-COUNT.                                   CL**4
00280                                                                      CL**4
00281      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*10
00282        IF PI-CALLING-PROGRAM = 'EL175'                               CL*10
00283          MOVE SPACES              TO TOTAL-LINE                      CL*10
00284          MOVE 'NON CASH  '        TO EL174A-NO-CHECKS-DESC           CL*10
00285          MOVE PI-NON-CASH-REL-CNT TO EL174A-NO-CHECKS                CL*10
00286          MOVE PI-NON-CASH-REL-AMT TO EL174A-CNTL-TOTAL               CL*10
00287          MOVE TOTAL-LINE          TO WS-PASSED-DATA                  CL*10
00288          MOVE '0'                 TO WS-PASSED-CNTL-CHAR             CL*10
00289          PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                         CL**7
00290          PERFORM 3000-BUMP-LINE-COUNT                                CL**7
00291          ADD PI-NON-CASH-REL-CNT TO PI-TOT-CHECKS                    CL**7
00292          ADD PI-NON-CASH-REL-AMT TO PI-CONTROL-GRAND-TOT.            CL**7
00293                                                                      CL**7
00294      MOVE SPACES                 TO TOTAL-LINE.                      CL**4
00295      MOVE PI-LIFE-OVERRIDE-L6    TO EL174A-NO-CHECKS-DESC.           CL**4
00296      MOVE PI-TOT-CHECKS-L        TO EL174A-NO-CHECKS.                CL**4
00297      MOVE PI-CONTROL-GRAND-TOT-L TO EL174A-CNTL-TOTAL.               CL**4
00298      MOVE TOTAL-LINE             TO WS-PASSED-DATA.                  CL**4
00299      MOVE '0'                    TO WS-PASSED-CNTL-CHAR.             CL**4
00300      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL**4
00301      PERFORM 3000-BUMP-LINE-COUNT.                                   CL**4
00302                                                                      CL**4
00303      MOVE SPACES                 TO TOTAL-LINE.                      CL**4
00304      MOVE PI-AH-OVERRIDE-L6      TO EL174A-NO-CHECKS-DESC.           CL**4
00305      MOVE PI-TOT-CHECKS-A        TO EL174A-NO-CHECKS.                CL**4
00306      MOVE PI-CONTROL-GRAND-TOT-A TO EL174A-CNTL-TOTAL.               CL**4
00307      MOVE TOTAL-LINE             TO WS-PASSED-DATA.                  CL**4
00308      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.             CL**4
00309      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                            CL**4
00310      PERFORM 3000-BUMP-LINE-COUNT.                                   CL**4
00311                                                                      CL**4
00312      MOVE SPACES                 TO TOTAL-LINE.                      CL**4
00313      MOVE 'NO. CHECKS'           TO EL174A-NO-CHECKS-DESC.           CL**4
00314      MOVE PI-TOT-CHECKS          TO EL174A-NO-CHECKS.                CL**4
00315      MOVE 'GRAND TOTAL'          TO EL174A-CNTL-DESC.                CL**4
00316      MOVE PI-CONTROL-GRAND-TOT   TO EL174A-CNTL-TOTAL.               CL**4
00317      MOVE TOTAL-LINE             TO WS-PASSED-DATA.                  CL**4
00318      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.             CL**4
00319      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1742
00320      MOVE 'X'                    TO WS-PROG-END.                  EL1742
00321      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1742
00322      GO TO 0100-RETRIEVE-LOOP.                                    EL1742
00323                                                                   EL1742
00324  200-END-DATA.                                                    EL1742
00325                                                                      CL**9
00326 * DLO034 CLOSE                                                       CL**9
pemuni     IF PI-COMPANY-ID = 'DMD' OR 'XXX'                               CL**9
00328          MOVE 'C'                TO DL34-PROCESS-TYPE                CL**9
00329          MOVE PI-COMPANY-ID      TO DL34-COMPANY-ID                  CL**9
00330          MOVE THIS-PGM           TO DL34-PRINT-PROGRAM-ID            CL**9
00331          MOVE PI-PROCESSOR-ID    TO DL34-USERID                      CL**9
00332          MOVE SPACES             TO DL34-PRINT-LINE                  CL**9
00333                                     DL34-OVERRIDE-PRINTER-ID         CL**9
00334          EXEC CICS LINK                                              CL**9
00335              PROGRAM    ('DLO034')                                   CL**9
00336              COMMAREA   (DLO034-COMMUNICATION-AREA)                  CL**9
00337              LENGTH     (DLO034-REC-LENGTH)                          CL**9
00338          END-EXEC                                                    CL**9
00339          IF DL34-RETURN-CODE NOT = 'OK'                              CL**9
00340              MOVE  '**DLO034 CLOSE ERROR - ABORT**'                  CL**9
00341                                  TO ERROR-LINE                       CL**9
00342              PERFORM 400-SEND-TEXT.                                  CL**9
00343                                                                      CL**9
00344      EXEC CICS RETURN                                             EL1742
00345      END-EXEC.                                                       CL**4
00346                                                                   EL1742
00347  300-NOT-FOUND.                                                   EL1742
00348      MOVE 'NO COMMUNICATION AREA FOUND' TO ERROR-LINE.               CL**4
00349                                                                      CL*10
00350      PERFORM 400-SEND-TEXT.                                          CL**4
00351                                                                      CL*10
00352      GO TO 200-END-DATA.                                          EL1742
00353                                                                   EL1742
00354  400-SEND-TEXT.                                                   EL1742
00355      EXEC CICS SEND TEXT                                          EL1742
00356           FROM  (ERROR-LINE)                                         CL**4
00357           LENGTH(70)                                              EL1742
00358      END-EXEC.                                                       CL**4
00359                                                                      CL*10
00360      EJECT                                                        EL1742
00361  1000-INITIALIZE    SECTION.                                      EL1742
00362                                                                   EL1742
00363 *    NOTE ******************************************************* EL1742
00364 *         *                                                     * EL1742
00365 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL1742
00366 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL1742
00367 *         *                                                     * EL1742
00368 *         *******************************************************.EL1742
00369                                                                   EL1742
00370      IF PI-CALLING-PROGRAM = 'EL175'                                 CL**4
00371          MOVE LOW-VALUES         TO  PI-CK-COMPANY-CODE           EL1742
00372                                      PI-CK-CARRIER                   CL**7
00373                                      PI-CK-GROUPING                  CL**7
00374                                      PI-CK-STATE                     CL**7
00375                                      PI-CK-BENE-ACCT                 CL**7
00376          MOVE ZEROS              TO  PI-CK-SEQUENCE-NO            EL1742
00377          MOVE 'R'                TO  PI-CHECK-MODE                   CL**7
00378      ELSE                                                         EL1742
00379          IF CHECKS-PRINTED                                           CL**3
00380              MOVE LOW-VALUES             TO  PI-CHECK-AIX-KEY        CL**7
00381              MOVE PI-START-CONTROL-NO    TO  PI-CK-CONTROL-NO        CL**3
00382          ELSE                                                        CL**3
00383              MOVE LOW-VALUES             TO  PI-CHECK-AIX-KEY.       CL**7
00384                                                                   EL1742
00385      MOVE LOW-VALUES             TO  PI-PREV-CHECK-AIX-KEY.          CL**7
00386                                                                   EL1742
00387      MOVE ZEROS                  TO  PI-CONTROL-TOT               EL1742
00388                                      PI-CONTROL-TOT-L                CL**4
00389                                      PI-CONTROL-TOT-A                CL**4
00390                                      PI-CONTROL-SAVE-CONTROL      EL1742
00391                                      PI-CONTROL-GRAND-TOT         EL1742
00392                                      PI-CONTROL-GRAND-TOT-L          CL**4
00393                                      PI-CONTROL-GRAND-TOT-A          CL**4
00394                                      PI-TOT-CHECKS                EL1742
00395                                      PI-TOT-CHECKS-L                 CL**4
00396                                      PI-TOT-CHECKS-A                 CL**4
00397                                      WS-NO-CHECKS-L                  CL**4
00398                                      WS-NO-CHECKS-A                  CL**4
00399                                      WS-NO-CHECKS.                EL1742
00400      MOVE 1                      TO  PI-PAGE                      EL1742
00401                                      PI-LINE-COUNT.                  CL**4
00402                                                                   EL1742
00403      MOVE PI-COMPANY-CD          TO PI-CK-COMPANY-CODE.              CL**4
00404      MOVE 'Y'                    TO PI-FIRST-TIME-SWT.               CL**4
00405                                                                   EL1742
00406      MOVE EIBTIME                TO  WS-TIME-WORK.                EL1742
00407                                                                   EL1742
00408      MOVE SAVE-DATE              TO  ADATE.                          CL**4
00409      MOVE WS-TIME                TO  ATIME.                       EL1742
00410      MOVE PI-COMPANY-ID          TO  ACOMP.                       EL1742
00411      PERFORM 2000-PRINT-HEADING.                                  EL1742
00412                                                                      CL*10
00413      EJECT                                                        EL1742
00414  2000-PRINT-HEADING   SECTION.                                    EL1742
00415      MOVE PI-PAGE                TO PAGE-NO.                         CL**4
00416                                                                      CL*10
00417      IF CHECKS-TO-BE-PRINTED                                         CL**3
00418          MOVE VAR-HEADING1A      TO HDG1                             CL**3
00419      ELSE                                                            CL**3
00420          IF CHECKS-PRINTED                                           CL**7
00421              MOVE VAR-HEADING1B  TO HDG1                             CL**7
00422              MOVE 'EL174B'       TO HDG1-RPT                         CL**7
00423          ELSE                                                        CL**7
00424              MOVE VAR-HEADING1C  TO HDG1                             CL**7
00425              MOVE 'EL175A'       TO HDG1-RPT.                        CL**7
00426                                                                      CL**6
00427      MOVE '1'                    TO WS-PASSED-CNTL-CHAR.             CL*10
00428                                                                      CL*10
00429      IF PI-COMPANY-ID = 'AFL' OR 'AFC' OR 'RIC' OR 'SRL'             CL*10
00430          IF PI-FIRST-TIME-SWT = 'Y'                                  CL*10
00431              MOVE ' '            TO WS-PASSED-CNTL-CHAR.             CL*10
00432                                                                      CL**6
00433      MOVE HEADING-1              TO WS-PASSED-DATA.                  CL**4
00434      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1742
00435      MOVE 1                      TO PI-LINE-COUNT.                EL1742
00436                                                                   EL1742
00437      MOVE HEADING-2              TO WS-PASSED-DATA.                  CL**4
00438      MOVE '0'                    TO WS-PASSED-CNTL-CHAR.             CL**4
00439      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1742
00440      ADD 2                       TO PI-LINE-COUNT.                EL1742
00441                                                                   EL1742
00442      EJECT                                                        EL1742
00443  3000-BUMP-LINE-COUNT   SECTION.                                  EL1742
00444      IF WS-PASSED-CNTL-CHAR = SPACES                              EL1742
00445         ADD 1                    TO PI-LINE-COUNT                 EL1742
00446       ELSE                                                           CL**4
00447         ADD  2                   TO PI-LINE-COUNT.                EL1742
00448                                                                   EL1742
00449      IF PI-LINE-COUNT GREATER 60                                     CL**4
00450         MOVE ZEROS TO PI-LINE-COUNT                               EL1742
00451         ADD 1   TO PI-PAGE                                        EL1742
00452         PERFORM 2000-PRINT-HEADING.                               EL1742
00453                                                                   EL1742
00454      EJECT                                                        EL1742
00455  4000-BROWSE-CHECK-QUEUE-FILE SECTION.                            EL1742
00456                                                                   EL1742
00457      EXEC CICS STARTBR                                            EL1742
00458          DATASET (WS-CHECK-AIX-DSID)                                 CL**7
00459          RIDFLD  (PI-CHECK-AIX-KEY)                                  CL**7
00460          GTEQ                                                        CL**4
00461      END-EXEC.                                                       CL**4
00462                                                                   EL1742
00463  4100-READNEXT.                                                   EL1742
00464      MOVE PI-CHECK-AIX-KEY           TO  PI-PREV-CHECK-AIX-KEY.      CL**7
00465                                                                   EL1742
00466      EXEC CICS READNEXT                                           EL1742
00467          DATASET (WS-CHECK-AIX-DSID)                                 CL**7
00468          RIDFLD  (PI-CHECK-AIX-KEY)                                  CL**7
00469          SET     (ADDRESS OF CHECK-QUE)                              CL**8
00470      END-EXEC.                                                       CL**4
00471                                                                   EL1742
00472      IF CQ-COMPANY-CD NOT = PI-COMPANY-CD                         EL1742
00473          GO TO 4800-END-OF-FILE.                                  EL1742
00474                                                                   EL1742
00475      IF PI-CALLING-PROGRAM = 'EL175'                                 CL**4
00476         IF PI-COMPANY-ID = 'DMD'                                     CL*10
00477            IF CQ-CONTROL-NUMBER NOT = PI-END-CONTROL-NO              CL*10
00478               GO TO 4100-READNEXT                                    CL*10
00479             ELSE                                                     CL*10
00480               NEXT SENTENCE                                          CL*10
00481           ELSE                                                       CL*10
00482         IF PI-CONTROL-SAVE-CONTROL NOT = CQ-CONTROL-NUMBER        EL1742
00483            IF NOT PI-FIRST-TIME                                   EL1742
00484               IF CQ-CONTROL-NUMBER > PI-END-CONTROL-NO               CL*10
00485                  GO TO 4800-END-OF-FILE.                             CL**7
00486                                                                   EL1742
00487      IF CQ-ENTRY-TYPE NOT = 'Q'                                   EL1742
00488          GO TO 4100-READNEXT.                                     EL1742
00489                                                                   EL1742
00490      IF CHECKS-TO-BE-PRINTED                                         CL**3
00491          IF CQ-TIMES-PRINTED NOT = ZERO                              CL**3
00492              GO TO 4100-READNEXT.                                    CL**3
00493                                                                      CL**3
00494      IF CHECKS-PRINTED                                               CL**3
00495          IF CQ-TIMES-PRINTED = ZERO                                  CL**4
00496              GO TO 4100-READNEXT.                                    CL**3
00497                                                                   EL1742
00498      IF PI-COMPANY-ID = 'DMD'                                        CL*10
00499      IF CHECKS-PRINTED                                               CL*10
00500         IF PI-START-CONTROL-NO > CQ-CONTROL-NUMBER                   CL*10
00501            GO TO 4100-READNEXT.                                      CL*10
00502                                                                      CL*10
00503      IF PI-CONTROL-SAVE-CONTROL NOT = CQ-CONTROL-NUMBER           EL1742
00504         IF PI-FIRST-TIME                                          EL1742
00505            NEXT SENTENCE                                          EL1742
00506         ELSE                                                      EL1742
00507            MOVE SPACES              TO  TOTAL-LINE                   CL**4
00508            MOVE PI-LIFE-OVERRIDE-L6 TO EL174A-NO-CHECKS-DESC         CL**4
00509            MOVE WS-NO-CHECKS-L      TO EL174A-NO-CHECKS              CL**4
00510            MOVE PI-CONTROL-TOT-L    TO EL174A-CNTL-TOTAL             CL**4
00511            MOVE TOTAL-LINE          TO WS-PASSED-DATA                CL**4
00512            MOVE '0'                 TO WS-PASSED-CNTL-CHAR           CL**4
00513            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                    EL1742
00514            PERFORM 3000-BUMP-LINE-COUNT                           EL1742
00515            MOVE SPACES              TO TOTAL-LINE                    CL**4
00516            MOVE PI-AH-OVERRIDE-L6   TO EL174A-NO-CHECKS-DESC         CL**4
00517            MOVE WS-NO-CHECKS-A      TO EL174A-NO-CHECKS              CL**4
00518            MOVE PI-CONTROL-TOT-A    TO EL174A-CNTL-TOTAL             CL**4
00519            MOVE TOTAL-LINE          TO WS-PASSED-DATA                CL**4
00520            MOVE ' '                 TO WS-PASSED-CNTL-CHAR           CL**4
00521            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                    EL1742
00522            PERFORM 3000-BUMP-LINE-COUNT                           EL1742
00523            MOVE SPACES                 TO TOTAL-LINE                 CL**4
00524            MOVE 'NO. CHECKS'           TO EL174A-NO-CHECKS-DESC      CL**4
00525            MOVE WS-NO-CHECKS           TO EL174A-NO-CHECKS           CL**4
00526            MOVE 'CONTL TOTAL'          TO EL174A-CNTL-DESC           CL**4
00527            MOVE PI-CONTROL-TOT         TO EL174A-CNTL-TOTAL          CL**4
00528            MOVE TOTAL-LINE             TO WS-PASSED-DATA             CL**4
00529            MOVE ' '                    TO WS-PASSED-CNTL-CHAR        CL**4
00530            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                       CL**4
00531            PERFORM 3000-BUMP-LINE-COUNT                              CL**4
00532            MOVE SPACES                 TO WS-PASSED-DATA             CL**4
00533            MOVE '0'                    TO WS-PASSED-CNTL-CHAR        CL**4
00534            PERFORM ELPRTCVP THRU ELPRTCVP-EXIT                       CL**4
00535            PERFORM 3000-BUMP-LINE-COUNT                              CL**4
00536            MOVE ZEROS            TO PI-CONTROL-TOT                EL1742
00537                                     PI-CONTROL-TOT-L                 CL**4
00538                                     PI-CONTROL-TOT-A                 CL**4
00539                                     WS-NO-CHECKS-L                   CL**4
00540                                     WS-NO-CHECKS-A                   CL**4
00541                                     WS-NO-CHECKS.                 EL1742
00542                                                                   EL1742
00543      MOVE CQ-CONTROL-NUMBER      TO PI-CONTROL-SAVE-CONTROL.      EL1742
00544      MOVE 'N'                    TO PI-FIRST-TIME-SWT.            EL1742
00545                                                                   EL1742
00546      ADD +1                      TO WS-NO-CHECKS                  EL1742
00547                                     PI-TOT-CHECKS.                EL1742
00548      IF CQ-LIFE-CLAIM                                                CL**4
00549          ADD +1                  TO WS-NO-CHECKS-L                   CL**4
00550                                     PI-TOT-CHECKS-L                  CL**4
00551          ADD CQ-CHECK-AMOUNT     TO PI-CONTROL-TOT-L                 CL**4
00552                                     PI-CONTROL-GRAND-TOT-L           CL**4
00553       ELSE                                                           CL**4
00554      IF CQ-AH-CLAIM                                                  CL**4
00555          ADD +1                  TO WS-NO-CHECKS-A                   CL**4
00556                                     PI-TOT-CHECKS-A                  CL**4
00557          ADD CQ-CHECK-AMOUNT     TO PI-CONTROL-TOT-A                 CL**4
00558                                     PI-CONTROL-GRAND-TOT-A.          CL**4
00559                                                                      CL**4
00560      MOVE SPACES                 TO DETAIL-LINE.                     CL**4
00561      ADD CQ-CHECK-AMOUNT         TO PI-CONTROL-TOT.               EL1742
00562      MOVE CQ-CONTROL-NUMBER      TO EL174A-CONTROL.                  CL**4
00563      MOVE CQ-CHECK-NUMBER        TO EL174A-CHECK-NO.                 CL**4
00564                                                                   EL1742
00565      IF CQ-PAYMENT-TYPE = '1'                                     EL1742
00566          MOVE 'PARTIAL'          TO EL174A-PMT-TYPE                  CL**4
00567        ELSE                                                       EL1742
00568      IF CQ-PAYMENT-TYPE = '2'                                     EL1742
00569          MOVE 'FINAL'            TO EL174A-PMT-TYPE                  CL**4
00570        ELSE                                                       EL1742
00571      IF CQ-PAYMENT-TYPE = '3'                                     EL1742
00572          MOVE 'LUMP SUM'         TO EL174A-PMT-TYPE                  CL**4
00573        ELSE                                                       EL1742
00574      IF CQ-PAYMENT-TYPE = '4'                                     EL1742
00575          MOVE 'ADDITIONAL'       TO EL174A-PMT-TYPE                  CL**4
00576        ELSE                                                       EL1742
00577      IF CQ-PAYMENT-TYPE = '5'                                     EL1742
00578          MOVE 'CHG EXP'          TO EL174A-PMT-TYPE                  CL**4
00579        ELSE                                                       EL1742
00580      IF CQ-PAYMENT-TYPE = '6'                                     EL1742
00581          MOVE 'NON CHG EXP'      TO EL174A-PMT-TYPE               EL1742
00582        ELSE                                                       EL1742
00583      IF CQ-PAYMENT-TYPE = '7'                                     EL1742
00584          MOVE 'LIFE REFUND'      TO EL174A-PMT-TYPE               EL1742
00585        ELSE                                                       EL1742
00586      IF CQ-PAYMENT-TYPE = '8'                                     EL1742
00587          MOVE 'A&H REFUND'       TO EL174A-PMT-TYPE.                 CL**4
00588                                                                      CL**5
00589      IF CQ-LIFE-CLAIM                                                CL**5
00590          MOVE 'L'                TO EL174A-COV-TYPE                  CL**5
00591        ELSE                                                          CL**5
00592          MOVE 'A'                TO EL174A-COV-TYPE.                 CL**5
00593                                                                   EL1742
00594      ADD CQ-CHECK-AMOUNT        TO  PI-CONTROL-GRAND-TOT.            CL**4
00595      MOVE CQ-CLAIM-NO           TO  EL174A-CLAIM-NO.                 CL**4
00596      MOVE CQ-CARRIER            TO  EL174A-CARRIER.                  CL**4
00597      MOVE CQ-CERT-NO            TO  EL174A-CERT-NO.                  CL**4
00598      MOVE CQ-CHECK-AMOUNT       TO  EL174A-AMT.                      CL**4
00599                                                                   EL1742
00600      MOVE SPACES                TO  WS-ACTIVITY-TRAILERS-KEY.        CL**4
00601                                                                   EL1742
00602      MOVE CQ-COMPANY-CD         TO  WS-ATK-COMPANY-CD.               CL**4
00603      MOVE CQ-CARRIER            TO  WS-ATK-CARRIER.                  CL**4
00604      MOVE CQ-CLAIM-NO           TO  WS-ATK-CLAIM-NO.                 CL**4
00605      MOVE CQ-CERT-NO            TO  WS-ATK-CERT-NO.                  CL**4
00606      MOVE CQ-PMT-TRLR-SEQUENCE  TO  WS-ATK-SEQUENCE-NO.              CL**4
00607                                                                   EL1742
00608      EXEC CICS READ                                               EL1742
00609          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      EL1742
00610          RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                       EL1742
00611          SET     (ADDRESS OF ACTIVITY-TRAILERS)                      CL**8
00612      END-EXEC.                                                       CL**4
00613                                                                   EL1742
00614      IF AT-PAYEE-TYPE = 'I'                                          CL**4
00615          MOVE 'INSURED'          TO  EL174A-PAYEE                    CL**4
00616        ELSE                                                       EL1742
00617      IF AT-PAYEE-TYPE = 'B'                                          CL**4
00618          MOVE 'BENEFICIARY'      TO  EL174A-PAYEE                 EL1742
00619        ELSE                                                       EL1742
00620      IF AT-PAYEE-TYPE = 'A'                                          CL**4
00621          MOVE 'ACCOUNT'          TO  EL174A-PAYEE                    CL**4
00622        ELSE                                                       EL1742
00623      IF AT-PAYEE-TYPE = 'O'                                          CL**4
00624          MOVE 'OTHER 1'          TO  EL174A-PAYEE                    CL**4
00625        ELSE                                                       EL1742
00626      IF AT-PAYEE-TYPE = 'Q'                                          CL**4
00627          MOVE 'OTHER 2'          TO  EL174A-PAYEE                    CL**4
00628        ELSE                                                       EL1742
00629      IF AT-PAYEE-TYPE = 'P'                                          CL**4
00630          MOVE 'DOCTOR'           TO  EL174A-PAYEE                    CL**7
00631        ELSE                                                          CL**7
00632      IF AT-PAYEE-TYPE = 'E'                                          CL**7
00633          MOVE 'EMPLOYER'         TO  EL174A-PAYEE.                   CL**7
00634                                                                   EL1742
00635      MOVE AT-RECORDED-BY         TO  EL174A-BY.                      CL**4
00636                                                                   EL1742
00637      MOVE DETAIL-LINE            TO WS-PASSED-DATA.                  CL**4
00638      MOVE ' '                    TO WS-PASSED-CNTL-CHAR.             CL**4
00639      PERFORM ELPRTCVP THRU ELPRTCVP-EXIT.                         EL1742
00640      PERFORM 3000-BUMP-LINE-COUNT.                                   CL**4
00641                                                                   EL1742
00642      GO TO 4100-READNEXT.                                         EL1742
00643                                                                   EL1742
00644  4800-END-OF-FILE.                                                EL1742
00645      IF PI-PREV-CHECK-AIX-KEY  = LOW-VALUES                          CL**7
00646         GO TO 4990-EXIT.                                          EL1742
00647                                                                   EL1742
00648      EXEC CICS ENDBR                                              EL1742
00649          DATASET (WS-CHECK-AIX-DSID)                                 CL**7
00650      END-EXEC.                                                       CL**4
00651                                                                   EL1742
00652  4990-EXIT.                                                       EL1742
00653       EXIT.                                                          CL**4
00654      EJECT                                                        EL1742
uktdel*9500-PRINT-ROUTINE SECTION.  COPY ELPRTCVP.                      EL1742
uktins 9500-PRINT-ROUTINE SECTION.
uktins     COPY ELPRTCVP.
00656                                                                   EL1742
00657  9700-LINK-DATE-CONVERT  SECTION.                                 EL1742
00658                                                                   EL1742
00659      EXEC CICS LINK                                               EL1742
00660          PROGRAM    ('ELDATCV')                                   EL1742
00661          COMMAREA   (DATE-CONVERSION-DATA)                        EL1742
00662          LENGTH     (DC-COMM-LENGTH)                              EL1742
00663      END-EXEC.                                                       CL**4
00664                                                                   EL1742
00665  9700-EXIT.                                                       EL1742
00666      EXIT.                                                        EL1742
00667                                                                   EL1742
00668  9900-LAST-PARAGRAPH SECTION.                                     EL1742
00669      GOBACK.                                                      EL1742
