00001  IDENTIFICATION DIVISION.                                         03/18/96
00002                                                                   ELCNTCV2
00002                                                                   ELCNTCV2
00003  PROGRAM-ID.                 ELCNTCV2.                               LV002
00004 *              PROGRAM CONVERTED BY                                  CL**2
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**2
00006 *              CONVERSION DATE 03/05/96 14:35:59.                    CL**2
00007 *                            VMOD=2.002.                             CL**2
00008 *                                                                 ELCNTCV2
00009 *AUTHOR.        LOGIC, INC.                                          CL**2
00010 *               DALLAS, TEXAS.                                       CL**2
00011                                                                   ELCNTCV2
00012 *DATE-COMPILED.                                                      CL**2
00013                                                                   ELCNTCV2
00014 *SECURITY.   *****************************************************   CL**2
00015 *            *                                                   *   CL**2
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**2
00017 *            *                                                   *   CL**2
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**2
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**2
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**2
00021 *            *                                                   *   CL**2
00022 *            *****************************************************   CL**2
00023                                                                   ELCNTCV2
00024 *REMARKS.                                                            CL**2
00025 *        CONVERT ELCCNTL (STATE BENEFIT CODES) FROM 20 OCCURRENCES   CL**2
00026 *                                                TO 50.              CL**2
00027                                                                   ELCNTCV2
00028  EJECT                                                            ELCNTCV2
00029  ENVIRONMENT DIVISION.                                            ELCNTCV2
00030  CONFIGURATION SECTION.                                           ELCNTCV2
00031  INPUT-OUTPUT SECTION.                                            ELCNTCV2
00032  FILE-CONTROL.                                                    ELCNTCV2
00033                                                                   ELCNTCV2
00034      SELECT CONTROL-TAPE-IN  ASSIGN TO INPUT-S-ELCNTLI.           ELCNTCV2
00035                                                                   ELCNTCV2
00036      SELECT CONTROL-FILE-OUT ASSIGN TO SYS022-FBA1-ELCNTLO        ELCNTCV2
00037                              ORGANIZATION INDEXED                 ELCNTCV2
00038                              ACCESS       DYNAMIC                 ELCNTCV2
00039                              RECORD KEY   CF-CONTROL-PRIMARY      ELCNTCV2
00040                              FILE STATUS  ELCNTL-FILE-STATUS.     ELCNTCV2
00041                                                                   ELCNTCV2
00042  EJECT                                                            ELCNTCV2
00043  DATA DIVISION.                                                   ELCNTCV2
00044  FILE SECTION.                                                    ELCNTCV2
00045                                                                   ELCNTCV2
00046      EJECT                                                        ELCNTCV2
00047  FD  CONTROL-TAPE-IN                                              ELCNTCV2
00048      BLOCK CONTAINS 0 RECORDS
00049      RECORDING MODE F.                                               CL**2
00050  01  CNTL-IN.                                                     ELCNTCV2
00051      12  CI-RECORD-ID                        PIC XX.              ELCNTCV2
00052                                                                   ELCNTCV2
00053      12  CI-CONTROL-PRIMARY.                                      ELCNTCV2
00054          16  CI-COMPANY-ID                  PIC XXX.              ELCNTCV2
00055          16  CI-RECORD-TYPE                 PIC X.                ELCNTCV2
00056          16  CI-ACCESS-CD-GENL              PIC X(4).             ELCNTCV2
00057          16  CI-SEQUENCE-NO                 PIC S9(4)   COMP.     ELCNTCV2
00058                                                                   ELCNTCV2
00059      12  CI-LAST-MAINT-DT                   PIC XX.               ELCNTCV2
00060      12  CI-LAST-MAINT-BY                   PIC X(4).             ELCNTCV2
00061      12  CI-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.   ELCNTCV2
00062                                                                   ELCNTCV2
00063      12  CI-RECORD-BODY                     PIC X(728).           ELCNTCV2
00064                                                                   ELCNTCV2
00065      12  CI-STATE-MASTER-REC  REDEFINES  CI-RECORD-BODY.          ELCNTCV2
00066          16  CI-STATE-ABBREVIATION          PIC XX.               ELCNTCV2
00067          16  CI-STATE-NAME                  PIC X(25).            ELCNTCV2
00068          16  CI-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3. ELCNTCV2
00069          16  CI-ST-CALC-QUOTE-TOLERANCE.                          ELCNTCV2
00070              20  CI-ST-TOL-CLAIM            PIC S999V99   COMP-3. ELCNTCV2
00071              20  CI-ST-TOL-PREM             PIC S999V99   COMP-3. ELCNTCV2
00072              20  CI-ST-TOL-REFUND           PIC S999V99   COMP-3. ELCNTCV2
00073              20  CI-ST-CLAIM-REJECT-SW      PIC X.                ELCNTCV2
00074                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.      ELCNTCV2
00075                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.        ELCNTCV2
00076              20  CI-ST-PREM-REJECT-SW       PIC X.                ELCNTCV2
00077                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.      ELCNTCV2
00078                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.        ELCNTCV2
00079              20  CI-ST-REF-REJECT-SW        PIC X.                ELCNTCV2
00080                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.      ELCNTCV2
00081                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.        ELCNTCV2
00082          16  CI-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3. ELCNTCV2
00083          16  CI-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3. ELCNTCV2
00084          16  CI-ST-REFUND-RULES.                                  ELCNTCV2
00085              20  CI-ST-REFUND-MIN           PIC S999V99    COMP-3.ELCNTCV2
00086              20  CI-ST-REFUND-DAYS-FIRST    PIC 99.               ELCNTCV2
00087              20  CI-ST-REFUND-DAYS-SUBSEQ   PIC 99.               ELCNTCV2
00088          16  CI-ST-FST-PMT-EXTENSION.                             ELCNTCV2
00089              20  CI-ST-FST-PMT-DAYS-MAX     PIC 999.              ELCNTCV2
00090              20  CI-ST-FST-PMT-DAYS-CHG     PIC X.                ELCNTCV2
00091                  88  CI-ST-EXT-NO-CHG           VALUE ' '.        ELCNTCV2
00092                  88  CI-ST-EXT-CHG-LF           VALUE '1'.        ELCNTCV2
00093                  88  CI-ST-EXT-CHG-AH           VALUE '2'.        ELCNTCV2
00094                  88  CI-ST-EXT-CHG-LF-AH        VALUE '3'.        ELCNTCV2
00095          16  CI-ST-STATE-CALL.                                    ELCNTCV2
00096              20  CI-ST-CALL-UNEARNED        PIC X.                ELCNTCV2
00097              20  CI-ST-CALL-RPT-CNTL        PIC X.                ELCNTCV2
00098              20  CI-ST-CALL-RATE-DEV        PIC XXX.              ELCNTCV2
00099          16  CI-REPLACEMENT-LAW-SW          PIC X.                ELCNTCV2
00100              88  CI-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.        ELCNTCV2
00101              88  CI-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.        ELCNTCV2
00102          16  CI-REPLACEMENT-LETTER          PIC X(4).             ELCNTCV2
00103          16  CI-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.   ELCNTCV2
00104          16  CI-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.   ELCNTCV2
00105          16  CI-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.   ELCNTCV2
00106          16  CI-ST-SPLIT-PAYMENT            PIC X.                ELCNTCV2
00107          16  FILLER                         PIC X.                ELCNTCV2
00108          16  CI-STATE-BENEFIT-CNTL  OCCURS 20 TIMES.              ELCNTCV2
00109              20  CI-ST-BENEFIT-CD           PIC XX.               ELCNTCV2
00110              20  CI-ST-BENEFIT-KIND         PIC X.                ELCNTCV2
00111              20  CI-ST-POLICY-FORM          PIC X(12).            ELCNTCV2
00112              20  CI-ST-REM-TERM-CALC        PIC X.                ELCNTCV2
00113              20  CI-ST-REFUND-CALC          PIC X.                ELCNTCV2
00114              20  CI-ST-EARNING-CALC         PIC X.                ELCNTCV2
00115              20  CI-ST-OVRD-EARNINGS-CALC   PIC X.                ELCNTCV2
00116              20  FILLER                     PIC X.                ELCNTCV2
00117                                                                   ELCNTCV2
00118          16  CI-ST-COMMISSION-CAPS.                               ELCNTCV2
00119              20  CI-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.   ELCNTCV2
00120              20  CI-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.   ELCNTCV2
00121              20  CI-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.   ELCNTCV2
00122              20  CI-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.   ELCNTCV2
00123          16  CI-COMM-CAP-LIMIT-TO           PIC X.                ELCNTCV2
00124                                                                   ELCNTCV2
00125          16  FILLER                         PIC X(233).           ELCNTCV2
00126                                                                   ELCNTCV2
00127                                                                   ELCNTCV2
00128      EJECT                                                        ELCNTCV2
00129  FD  CONTROL-FILE-OUT.                                               CL**2
00130                                                                      CL**2
00131      COPY ELCCNTL.                                                ELCNTCV2
00132                                                                   ELCNTCV2
00133  EJECT                                                            ELCNTCV2
00134  WORKING-STORAGE SECTION.                                         ELCNTCV2
00135  77  FILLER  PIC X(32)  VALUE '********************************'. ELCNTCV2
00136  77  FILLER  PIC X(32)  VALUE '*   ELCNTCV2 WORKING-STORAGE    '. ELCNTCV2
00137  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.002 ************'.    CL**2
00138                                                                   ELCNTCV2
00139  77  CNTL-IN-CNT               PIC S9(7)   COMP-3  VALUE +0.      ELCNTCV2
00140  77  CNTL-OUT-CNT              PIC S9(7)   COMP-3  VALUE +0.      ELCNTCV2
00141                                                                   ELCNTCV2
00142  01  WS.                                                          ELCNTCV2
00143      12  ELCNTL-FILE-STATUS    PIC XX             VALUE ZERO.     ELCNTCV2
00144      12  WS-RETURN-CODE        PIC S9(4)   COMP   VALUE +0.       ELCNTCV2
00145      12  WS-ABEND-MESSAGE      PIC X(80)          VALUE SPACES.   ELCNTCV2
00146      12  WS-ABEND-FILE-STATUS  PIC XX             VALUE ZEROS.    ELCNTCV2
00147      12  WS-ZERO               PIC S9      COMP-3 VALUE +0.       ELCNTCV2
00148      12  IDX                   PIC S99     COMP-3 VALUE +0.       ELCNTCV2
00149                                                                   ELCNTCV2
00150                                                                   ELCNTCV2
00151      EJECT                                                        ELCNTCV2
00152  PROCEDURE DIVISION.                                              ELCNTCV2
00153                                                                   ELCNTCV2
00154  0100-OPEN-FILES.                                                 ELCNTCV2
00155      OPEN INPUT  CONTROL-TAPE-IN                                  ELCNTCV2
00156           I-O    CONTROL-FILE-OUT.                                ELCNTCV2
00157                                                                   ELCNTCV2
00158      IF ELCNTL-FILE-STATUS   = '00' OR '97'                       ELCNTCV2
00159          NEXT SENTENCE                                            ELCNTCV2
00160        ELSE                                                       ELCNTCV2
00161          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          ELCNTCV2
00162          MOVE 'ELCNTL OPEN ERR'  TO WS-ABEND-MESSAGE              ELCNTCV2
00163          GO TO ABEND-PGM.                                         ELCNTCV2
00164                                                                   ELCNTCV2
00165  0200-READ-ELCNTL.                                                ELCNTCV2
00166      READ CONTROL-TAPE-IN                                         ELCNTCV2
00167          AT END GO TO 9000-EOJ.                                   ELCNTCV2
00168                                                                   ELCNTCV2
00169      ADD +1 TO CNTL-IN-CNT.                                       ELCNTCV2
00170                                                                   ELCNTCV2
00171      MOVE CNTL-IN             TO CONTROL-FILE.                    ELCNTCV2
00172                                                                   ELCNTCV2
00173      IF CF-RECORD-TYPE = '3'                                      ELCNTCV2
00174          MOVE ZERO            TO IDX                              ELCNTCV2
00175          PERFORM 1000-MOVE-STATE THRU 1000-EXIT.                  ELCNTCV2
00176                                                                   ELCNTCV2
00177  0300-WRITE-NEW-ELCNTL.                                           ELCNTCV2
00178      WRITE CONTROL-FILE.                                          ELCNTCV2
00179                                                                   ELCNTCV2
00180      IF ELCNTL-FILE-STATUS   = '00' OR '97'                       ELCNTCV2
00181          NEXT SENTENCE                                            ELCNTCV2
00182        ELSE                                                       ELCNTCV2
00183          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          ELCNTCV2
00184          MOVE 'ELCNTL WRITE ERR' TO WS-ABEND-MESSAGE              ELCNTCV2
00185          GO TO ABEND-PGM.                                         ELCNTCV2
00186                                                                   ELCNTCV2
00187      ADD +1 TO CNTL-OUT-CNT.                                      ELCNTCV2
00188                                                                   ELCNTCV2
00189      GO TO 0200-READ-ELCNTL.                                      ELCNTCV2
00190                                                                   ELCNTCV2
00191  1000-MOVE-STATE.                                                 ELCNTCV2
00192      ADD +1 TO IDX.                                               ELCNTCV2
00193      IF IDX GREATER +50                                           ELCNTCV2
00194          GO TO 1000-EXIT.                                         ELCNTCV2
00195      IF IDX GREATER +20                                           ELCNTCV2
00196          GO TO 1000-ZERO-STATE.                                   ELCNTCV2
00197      MOVE CI-ST-BENEFIT-CD    (IDX) TO CF-ST-BENEFIT-CD    (IDX). ELCNTCV2
00198      MOVE CI-ST-BENEFIT-KIND  (IDX) TO CF-ST-BENEFIT-KIND  (IDX). ELCNTCV2
00199      MOVE CI-ST-REM-TERM-CALC (IDX) TO CF-ST-REM-TERM-CALC (IDX). ELCNTCV2
00200      MOVE CI-ST-REFUND-CALC   (IDX) TO CF-ST-REFUND-CALC   (IDX). ELCNTCV2
00201      MOVE CI-ST-EARNING-CALC  (IDX) TO CF-ST-EARNING-CALC  (IDX). ELCNTCV2
00202      MOVE CI-ST-OVRD-EARNINGS-CALC                                ELCNTCV2
00203                               (IDX) TO CF-ST-OVRD-EARNINGS-CALC   ELCNTCV2
00204                                                            (IDX). ELCNTCV2
00205      GO TO 1000-MOVE-STATE.                                       ELCNTCV2
00206                                                                   ELCNTCV2
00207  1000-ZERO-STATE.                                                 ELCNTCV2
00208      MOVE SPACES        TO CF-STATE-BENEFIT-CNTL (IDX).           ELCNTCV2
00209      GO TO 1000-MOVE-STATE.                                       ELCNTCV2
00210                                                                   ELCNTCV2
00211  1000-EXIT.                                                       ELCNTCV2
00212      EXIT.                                                        ELCNTCV2
00213                                                                   ELCNTCV2
00214      EJECT                                                        ELCNTCV2
00215  ABEND-PGM.                                                       ELCNTCV2
00216      COPY ELCABEND.                                               ELCNTCV2
00217                                                                   ELCNTCV2
00218  9000-EOJ.                                                        ELCNTCV2
00219      CLOSE CONTROL-TAPE-IN                                        ELCNTCV2
00220            CONTROL-FILE-OUT.                                      ELCNTCV2
00221                                                                   ELCNTCV2
00222      IF ELCNTL-FILE-STATUS   = '00' OR '97'                       ELCNTCV2
00223          NEXT SENTENCE                                            ELCNTCV2
00224        ELSE                                                       ELCNTCV2
00225          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          ELCNTCV2
00226          MOVE 'ELCNTL CLOSE ERR' TO WS-ABEND-MESSAGE              ELCNTCV2
00227          GO TO ABEND-PGM.                                         ELCNTCV2
00228                                                                   ELCNTCV2
00229      DISPLAY '* CNTL CONVERSION -- STATE BENEFIT CODE EXPANSION'. ELCNTCV2
00230      DISPLAY ' '.                                                 ELCNTCV2
00231      DISPLAY 'RECORDS IN  ' CNTL-IN-CNT.                          ELCNTCV2
00232      DISPLAY 'RECORDS OUT ' CNTL-OUT-CNT.                         ELCNTCV2
00233      GOBACK.                                                         CL**2
