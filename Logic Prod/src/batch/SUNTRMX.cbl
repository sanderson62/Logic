00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ELRTRMX
SUNPSD PROGRAM-ID.                 SUNTRMX.                                LV016
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 12/20/95 07:27:00.                    CL**7
00007 *                            VMOD=2.009                              CL*13
00008 *                                                                 ELRTRMX
00009 *AUTHOR.       LOGIC, INC.                                           CL**7
00010 *              DALLAS, TEXAS.                                        CL**7
00011                                                                   ELRTRMX
00012 *DATE-COMPILED.                                                      CL**7
00013                                                                   ELRTRMX
00014 *SECURITY.   *****************************************************   CL**7
00015 *            *                                                   *   CL**7
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00017 *            *                                                   *   CL**7
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00021 *            *                                                   *   CL**7
00022 *            *****************************************************   CL**7
00023                                                                   ELRTRMX
00024 *REMARKS.    *****************************************************   CL**7
00025 *            *                                                   *   CL**7
00026 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *   CL**7
00027 *            *    OPTION SPECIFIED, COMPUTE REMAINING TERMS      *   CL**7
00028 *            *    FOR A CERTIFICATE.                             *   CL**7
00029 *            *                                                   *   CL**7
00030 *            *    START DATE = CP-CERT-EFF-DATE                  *   CL**7
00031 *            *    END-DATE   = VALUATION-DATE                    *   CL**7
00032 *            *    METHOD     = CP-REM-TERM-METHOD                *   CL**7
00033 *            *    IF COMPANY SPECIAL METHOD METHOD - PUT COMPANY *   CL**7
00034 *            *    I.D. IN CP-COMPANY-ID.                         *   CL**7
00035 *            *                                                   *   CL**7
00036 *            *****************************************************   CL**7
00037  ENVIRONMENT DIVISION.                                            ELRTRMX
00038                                                                   ELRTRMX
00039  DATA DIVISION.                                                   ELRTRMX
00040      EJECT                                                        ELRTRMX
00041  WORKING-STORAGE SECTION.                                         ELRTRMX
00042  77  FILLER   PIC X(32) VALUE '********************************'. ELRTRMX
00043  77  FILLER   PIC X(32) VALUE '**  ELRTRMX WORKING STORAGE   **'. ELRTRMX
00044  77  FILLER   PIC X(32) VALUE '******** VMOD=2.009 ************'.    CL*13
00045                                                                   ELRTRMX
00046 *01  RT-BGN-DATE.                                                    CL*11
00047 *    12  RT-BGN-YR          PIC 99.                                  CL*11
00048 *    12  RT-BGN-MO          PIC 99.                                  CL*11
00049 *    12  RT-BGN-DA          PIC 99.                                  CL*11
00050                                                                   ELRTRMX
00051 *01  RT-END-DATE.                                                    CL*11
00052 *    12  RT-END-YR          PIC 99.                                  CL*11
00053 *    12  RT-END-MO          PIC 99.                                  CL*11
00054 *    12  RT-END-DA          PIC 99.                                  CL*11
00055                                                                   ELRTRMX
00056 *01  RT-SAVE-DATE           PIC XX.                                  CL*11
00057                                                                   ELRTRMX
00058  01  MISC-CALCULATION-AREAS.                                         CL**3
00059      03  MONTHS-EARNED          PIC 9             VALUE ZERO.        CL**3
00060      03  MONTHS-EARNED-1        PIC 9             VALUE ZERO.        CL**3
00061      03  MONTHS-EARNED-2        PIC 9             VALUE ZERO.        CL**3
00062      03  TERM-IN-MONTHS         PIC S9(3)         VALUE ZERO.        CL**3
00063                                                                      CL**3
00064      03  WS-TOP-NUMBER          PIC 9(8).                            CL*13
00065      03  WS-TOP-NUMBER-R  REDEFINES  WS-TOP-NUMBER.                  CL*11
00066          05  WS-TOP-CCYR        PIC 9(4).                            CL*11
00067          05  WS-TOP-MO          PIC 99.                              CL*11
00068          05  WS-TOP-DA          PIC 99.                              CL*11
00069                                                                      CL*11
00070      03  WS-BOTTOM-NUMBER       PIC 9(8).                            CL*13
00071      03  WS-BOTTOM-NUMBER-R  REDEFINES  WS-BOTTOM-NUMBER.            CL*11
00072          05  WS-BOTTOM-CCYR     PIC 9(4).                            CL*11
00073          05  WS-BOTTOM-MO       PIC 99.                              CL*11
00074          05  WS-BOTTOM-DA       PIC 99.                              CL*11
00075                                                                      CL*11
00076      03  WS-RESULT-NUMBER.                                           CL*14
00077          05  WS-RESULT-CCYR     PIC S9(4).                           CL*14
00078          05  WS-RESULT-MO       PIC S99.                             CL*14
00079          05  WS-RESULT-DA       PIC S99.                             CL*14
00080                                                                      CL**3
00081      03  WS-SAVE-EFFECTIVE-DATE PIC 9(8).                            CL*13
00082      03  WS-SAVE-EFF-DATE-R  REDEFINES  WS-SAVE-EFFECTIVE-DATE.      CL*11
00083          05  FILLER             PIC 9(6).                            CL*13
00084          05  SAVE-EFF-DAYS      PIC 99.                              CL*11
00085                                                                      CL**3
00086      03  WS-SAVE-PAYMENT-DATE   PIC 9(8).                            CL*13
00087      03  WS-SAVE-PAY-DATE-R  REDEFINES  WS-SAVE-PAYMENT-DATE.        CL*11
00088          05  FILLER             PIC 9(4).                            CL*13
00089          05  SAVE-PAY-MO        PIC 99.                              CL*11
00090          05  SAVE-PAY-DAYS      PIC 99.                              CL*11
00091                                                                      CL**3
00092      03  WS-SAVE-VALUATION-DATE PIC 9(8).                            CL*13
00093      03  WS-SAVE-VAL-DATE-R  REDEFINES  WS-SAVE-VALUATION-DATE.      CL*11
00094          05  FILLER             PIC 9(6).                            CL*13
00095          05  SAVE-VAL-DAYS      PIC 99.                              CL*11
00096                                                                      CL**3
00097      03  WS-SAVE-EXPIRATION-DATE PIC 9(8).                           CL*13
00098      03  WS-SAVE-EXP-DATE-R  REDEFINES  WS-SAVE-EXPIRATION-DATE.     CL*11
00099          05  FILLER             PIC 9(6).                            CL*13
00100          05  SAVE-EXP-DAYS      PIC 99.                              CL*11
00101                                                                      CL**3
00102      03  WS-SAVE-BINARY-DATE    PIC XX.                              CL**3
00103                                                                      CL**3
00104 *    03  WS-SAVE-MONTH-END-DATE PIC 9(8).                            CL*13
00105 *    03  WS-SAVE-MTH-END-DT-R REDEFINES WS-SAVE-MONTH-END-DATE.      CL*13
00106 *        05  FILLER             PIC 9(6).                            CL*13
00107 *        05  WS-DAYS-IN-THIS-MO PIC 99.                              CL*13
00108                                                                      CL**3
00109  01  MISC-SWITCHES.                                                  CL**3
00110      03  PAYMENT-EOM        PIC X.                                   CL**3
00111          88 PAYMENT-DATE-EOM    VALUE 'Y'.                           CL**3
00112      03  EFFECTIVE-EOM      PIC X.                                   CL**3
00113          88 EFFECTIVE-DATE-EOM  VALUE 'Y'.                           CL**3
00114      03  VALUATION-EOM      PIC X.                                   CL**3
00115          88 VALUATION-DATE-EOM  VALUE 'Y'.                           CL**3
00116      03  TOP-EOM            PIC X.                                   CL**3
00117          88 TOP-NUMBER-EOM      VALUE 'Y'.                           CL**3
00118      03  BOTTOM-EOM         PIC X.                                   CL**3
00119          88 BOTTOM-NUMBER-EOM   VALUE 'Y'.                           CL**3
00120      03  EXPIRATION-EOM     PIC X.                                   CL**8
00121          88 EXPIRATION-DATE-EOM VALUE 'Y'.                           CL**8
00122                                                                      CL**3
00123      EJECT                                                        ELRTRMX
00124      COPY ELCDATE.                                                   CL*16
00125      EJECT                                                        ELRTRMX
00126  LINKAGE SECTION.                                                 ELRTRMX
00127      COPY ELCCALC.                                                   CL**4
00128                                                                   ELRTRMX
00129      EJECT                                                        ELRTRMX
00130  PROCEDURE DIVISION USING CALCULATION-PASS-AREA.                  ELRTRMX
00131                                                                   ELRTRMX
00132                                                                      CL*10
00133  000-RT-REMAINING-TERM-RTN.                                          CL**3
00134                             COPY ELCRTMPD.                           CL**3
00135                                                                   ELRTRMX
00136  8000-DATE-CONVERT.                                                  CL**3
SUNPSD     CALL 'SUNATCX' USING DATE-CONVERSION-DATA.                   ELRTRMX
00138  8000-EXIT.                                                          CL**3
00139      EXIT.                                                        ELRTRMX
00140                                                                   ELRTRMX
00141  9999-RT-REMAINING-TERM-XIT.                                         CL**3
00142      GOBACK.                                                      ELRTRMX
