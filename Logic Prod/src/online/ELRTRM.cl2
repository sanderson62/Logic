00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ELRTRM
00003  PROGRAM-ID.                 ELRTRM.                                 LV004
00004 *              PROGRAM CONVERTED BY                               ELRTRM
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ELRTRM
00006 *              CONVERSION DATE 03/05/96 16:23:13.                 ELRTRM
00007 *                            VMOD=2.009                           ELRTRM
00008 *                                                                 ELRTRM
00009 *AUTHOR.       LOGIC, INC.                                        ELRTRM
00010 *              DALLAS, TEXAS.                                     ELRTRM
00011                                                                   ELRTRM
00012 *DATE-COMPILED.                                                   ELRTRM
00013                                                                   ELRTRM
00014 *SECURITY.   *****************************************************ELRTRM
00015 *            *                                                   *ELRTRM
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ELRTRM
00017 *            *                                                   *ELRTRM
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ELRTRM
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ELRTRM
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ELRTRM
00021 *            *                                                   *ELRTRM
00022 *            *****************************************************ELRTRM
00023                                                                   ELRTRM
00024 *REMARKS.    *****************************************************ELRTRM
00025 *            *                                                   *ELRTRM
00026 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *ELRTRM
00027 *            *    OPTION SPECIFIED, COMPUTE REMAINING TERMS      *ELRTRM
00028 *            *    FOR A CERTIFICATE.                             *ELRTRM
00029 *            *                                                   *ELRTRM
00030 *            *    START DATE = CP-CERT-EFF-DATE + EXTENTION DAYS *ELRTRM
00031 *            *    END-DATE = VALUATION-DATE                      *ELRTRM
00032 *            *    METHOD = CP-REM-TERM-METHOD                    *ELRTRM
00033 *            *    IF COMPANY SPECIAL METHOD METHOD - PUT COMPANY *ELRTRM
00034 *            *    I.D. IN CP-COMPANY-ID.                         *ELRTRM
00035 *            *                                                   *ELRTRM
00036 *            *****************************************************ELRTRM
00037  ENVIRONMENT DIVISION.                                            ELRTRM
00038                                                                   ELRTRM
00039  DATA DIVISION.                                                   ELRTRM
00040      EJECT                                                        ELRTRM
00041  WORKING-STORAGE SECTION.                                         ELRTRM
00042  77  FILLER   PIC X(32) VALUE '********************************'. ELRTRM
00043  77  FILLER   PIC X(32) VALUE '**  ELRTRM  WORKING STORAGE   **'. ELRTRM
00044  77  FILLER   PIC X(32) VALUE '********* VMOD 2.009 ***********'. ELRTRM
00045                                                                   ELRTRM
00046 *01  RT-BGN-DATE.                                                 ELRTRM
00047 *    12  RT-BGN-YR          PIC 99.                               ELRTRM
00048 *    12  RT-BGN-MO          PIC 99.                               ELRTRM
00049 *    12  RT-BGN-DA          PIC 99.                               ELRTRM
00050                                                                   ELRTRM
00051 *01  RT-END-DATE.                                                 ELRTRM
00052 *    12  RT-END-YR          PIC 99.                               ELRTRM
00053 *    12  RT-END-MO          PIC 99.                               ELRTRM
00054 *    12  RT-END-DA          PIC 99.                               ELRTRM
00055                                                                   ELRTRM
00056 *01  RT-SAVE-DATE           PIC XX.                               ELRTRM
00057                                                                   ELRTRM
00058  01  MISC-CALCULATION-AREAS.                                      ELRTRM
00059      03  SAVE-ODD-DAYS      PIC S99      VALUE ZERO.              ELRTRM
00060      03  MONTHS-EARNED      PIC 9        VALUE ZERO.              ELRTRM
00061      03  MONTHS-EARNED-1    PIC 9        VALUE ZERO.              ELRTRM
00062      03  MONTHS-EARNED-2    PIC 9        VALUE ZERO.              ELRTRM
00063      03  TERM-IN-MONTHS     PIC S9(3)    VALUE ZERO.              ELRTRM
00064                                                                   ELRTRM
00065      03  WS-TOP-NUMBER          PIC 9(8).                         ELRTRM
00066      03  WS-TOP-NUMBER-R  REDEFINES  WS-TOP-NUMBER.               ELRTRM
00067          05  WS-TOP-CCYR        PIC 9(4).                         ELRTRM
00068          05  WS-TOP-MO          PIC 99.                           ELRTRM
00069          05  WS-TOP-DA          PIC 99.                           ELRTRM
00070                                                                   ELRTRM
00071      03  WS-BOTTOM-NUMBER       PIC 9(8).                         ELRTRM
00072      03  WS-BOTTOM-NUMBER-R  REDEFINES  WS-BOTTOM-NUMBER.         ELRTRM
00073          05  WS-BOTTOM-CCYR     PIC 9(4).                            CL**2
00074          05  WS-BOTTOM-MO       PIC 99.                           ELRTRM
00075          05  WS-BOTTOM-DA       PIC 99.                           ELRTRM
00076                                                                   ELRTRM
00077      03  WS-RESULT-NUMBER.                                        ELRTRM
00078          05  WS-RESULT-CCYR     PIC S9(4).                        ELRTRM
00079          05  WS-RESULT-MO       PIC S99.                          ELRTRM
00080          05  WS-RESULT-DA       PIC S99.                          ELRTRM
00081                                                                   ELRTRM
00082      03  WS-SAVE-EFFECTIVE-DATE PIC 9(8).                         ELRTRM
00083      03  WS-SAVE-EFF-DATE-R  REDEFINES  WS-SAVE-EFFECTIVE-DATE.   ELRTRM
00084          05  FILLER             PIC 9(6).                         ELRTRM
00085          05  SAVE-EFF-DAYS      PIC 99.                           ELRTRM
00086                                                                   ELRTRM
00087      03  WS-SAVE-PAYMENT-DATE   PIC 9(8).                         ELRTRM
00088      03  WS-SAVE-PAYMENT-DATE-R  REDEFINES  WS-SAVE-PAYMENT-DATE. ELRTRM
00089          05  FILLER             PIC 9(4).                         ELRTRM
00090          05  SAVE-PAY-MO        PIC 99.                           ELRTRM
00091          05  SAVE-PAY-DAYS      PIC 99.                           ELRTRM
00092                                                                   ELRTRM
00093      03  WS-SAVE-VALUATION-DATE PIC 9(8).                         ELRTRM
00094      03  WS-SAVE-VAL-DATE-R  REDEFINES  WS-SAVE-VALUATION-DATE.   ELRTRM
00095          05  FILLER             PIC 9(6).                         ELRTRM
00096          05  SAVE-VAL-DAYS      PIC 99.                           ELRTRM
00097                                                                   ELRTRM
00098      03  WS-SAVE-EXPIRATION-DATE PIC 9(8).                        ELRTRM
00099      03  WS-SAVE-EXP-DATE-R  REDEFINES  WS-SAVE-EXPIRATION-DATE.  ELRTRM
00100          05  FILLER             PIC 9(6).                         ELRTRM
00101          05  SAVE-EXP-DAYS      PIC 99.                           ELRTRM
00102                                                                   ELRTRM
00103      03  WS-SAVE-BINARY-DATE    PIC XX.                           ELRTRM
00104                                                                   ELRTRM
00105 *    03  WS-SAVE-MONTH-END-DATE.                                  ELRTRM
00106 *        05  FILLER             PIC S99.                          ELRTRM
00107 *        05  FILLER             PIC S99.                          ELRTRM
00108 *        05  WS-DAYS-IN-THIS-MO PIC S99.                          ELRTRM
00109                                                                   ELRTRM
00110  01  MISC-SWITCHES.                                               ELRTRM
00111      03  PAYMENT-EOM        PIC X.                                ELRTRM
00112          88 PAYMENT-DATE-EOM    VALUE 'Y'.                        ELRTRM
00113      03  EFFECTIVE-EOM      PIC X.                                ELRTRM
00114          88 EFFECTIVE-DATE-EOM  VALUE 'Y'.                        ELRTRM
00115      03  VALUATION-EOM      PIC X.                                ELRTRM
00116          88 VALUATION-DATE-EOM  VALUE 'Y'.                        ELRTRM
00117      03  TOP-EOM            PIC X.                                ELRTRM
00118          88 TOP-NUMBER-EOM      VALUE 'Y'.                        ELRTRM
00119      03  BOTTOM-EOM         PIC X.                                ELRTRM
00120          88 BOTTOM-NUMBER-EOM   VALUE 'Y'.                        ELRTRM
00121      03  EXPIRATION-EOM     PIC X.                                ELRTRM
00122          88 EXPIRATION-DATE-EOM VALUE 'Y'.                        ELRTRM
00123                                                                   ELRTRM
00124      EJECT                                                        ELRTRM
00125                             COPY ELCDATE.                            CL**4
00126                                                                   ELRTRM
00127      EJECT                                                        ELRTRM
00128                             COPY ELCCALC.                         ELRTRM
00129                                                                   ELRTRM
00130  LINKAGE SECTION.                                                 ELRTRM
00131  01  DFHCOMMAREA              PIC X(450).                         ELRTRM
00132                                                                   ELRTRM
00133      EJECT                                                        ELRTRM
00134  PROCEDURE DIVISION.                                              ELRTRM
00135      MOVE DFHCOMMAREA            TO  CALCULATION-PASS-AREA.       ELRTRM
00136                                                                   ELRTRM
00137  000-RT-REMAINING-TERM-RTN.                                       ELRTRM
00138                      COPY ELCRTMPD.                               ELRTRM
00139                                                                   ELRTRM
00140 *    GO TO 9999-RT-REMAINING-TERM-XIT.                            ELRTRM
00141                                                                   ELRTRM
00142  8000-DATE-CONVERT.                                               ELRTRM
00143      EXEC CICS LINK                                               ELRTRM
00144           PROGRAM  ('ELDATCV')                                       CL**3
00145           COMMAREA (DATE-CONVERSION-DATA)                         ELRTRM
00146           LENGTH   (DC-COMM-LENGTH)                               ELRTRM
00147      END-EXEC.                                                    ELRTRM
00148  8000-EXIT.                                                       ELRTRM
00149      EXIT.                                                        ELRTRM
00150                                                                   ELRTRM
00151  9999-RT-REMAINING-TERM-XIT.                                      ELRTRM
00152      MOVE CALCULATION-PASS-AREA  TO  DFHCOMMAREA.                 ELRTRM
00153                                                                   ELRTRM
00154      EXEC CICS RETURN                                             ELRTRM
00155      END-EXEC.                                                    ELRTRM
00156                                                                   ELRTRM
00157      GOBACK.                                                      ELRTRM
