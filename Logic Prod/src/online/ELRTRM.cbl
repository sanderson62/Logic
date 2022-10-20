00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 ELRTRM.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/05/96 16:23:13.
00007 *                            VMOD=2.009
00008 *
00009 *AUTHOR.       LOGIC, INC.
00010 *              DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS.    *****************************************************
00025 *            *                                                   *
00026 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *
00027 *            *    OPTION SPECIFIED, COMPUTE REMAINING TERMS      *
00028 *            *    FOR A CERTIFICATE.                             *
00029 *            *                                                   *
00030 *            *    START DATE = CP-CERT-EFF-DATE + EXTENTION DAYS *
00031 *            *    END-DATE = VALUATION-DATE                      *
00032 *            *    METHOD = CP-REM-TERM-METHOD                    *
00033 *            *    IF COMPANY SPECIAL METHOD METHOD - PUT COMPANY *
00034 *            *    I.D. IN CP-COMPANY-ID.                         *
00035 *            *                                                   *
00036 *            *****************************************************
00037  ENVIRONMENT DIVISION.
00038
00039  DATA DIVISION.
00040      EJECT
00041  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00042  77  FILLER   PIC X(32) VALUE '********************************'.
00043  77  FILLER   PIC X(32) VALUE '**  ELRTRM  WORKING STORAGE   **'.
00044  77  FILLER   PIC X(32) VALUE '********* VMOD 2.009 ***********'.
00045
00046 *01  RT-BGN-DATE.
00047 *    12  RT-BGN-YR          PIC 99.
00048 *    12  RT-BGN-MO          PIC 99.
00049 *    12  RT-BGN-DA          PIC 99.
00050
00051 *01  RT-END-DATE.
00052 *    12  RT-END-YR          PIC 99.
00053 *    12  RT-END-MO          PIC 99.
00054 *    12  RT-END-DA          PIC 99.
00055
00056 *01  RT-SAVE-DATE           PIC XX.
00057
00058  01  MISC-CALCULATION-AREAS.
00059      03  SAVE-ODD-DAYS      PIC S99      VALUE ZERO.
00060      03  MONTHS-EARNED      PIC 9        VALUE ZERO.
00061      03  MONTHS-EARNED-1    PIC 9        VALUE ZERO.
00062      03  MONTHS-EARNED-2    PIC 9        VALUE ZERO.
00063      03  TERM-IN-MONTHS     PIC S9(3)    VALUE ZERO.
00064
00065      03  WS-TOP-NUMBER          PIC 9(8).
00066      03  WS-TOP-NUMBER-R  REDEFINES  WS-TOP-NUMBER.
00067          05  WS-TOP-CCYR        PIC 9(4).
00068          05  WS-TOP-MO          PIC 99.
00069          05  WS-TOP-DA          PIC 99.
00070
00071      03  WS-BOTTOM-NUMBER       PIC 9(8).
00072      03  WS-BOTTOM-NUMBER-R  REDEFINES  WS-BOTTOM-NUMBER.
00073          05  WS-BOTTOM-CCYR     PIC 9(4).
00074          05  WS-BOTTOM-MO       PIC 99.
00075          05  WS-BOTTOM-DA       PIC 99.
00076
00077      03  WS-RESULT-NUMBER.
00078          05  WS-RESULT-CCYR     PIC S9(4).
00079          05  WS-RESULT-MO       PIC S99.
00080          05  WS-RESULT-DA       PIC S99.
00081
00082      03  WS-SAVE-EFFECTIVE-DATE PIC 9(8).
00083      03  WS-SAVE-EFF-DATE-R  REDEFINES  WS-SAVE-EFFECTIVE-DATE.
00084          05  FILLER             PIC 9(6).
00085          05  SAVE-EFF-DAYS      PIC 99.
00086
00087      03  WS-SAVE-PAYMENT-DATE   PIC 9(8).
00088      03  WS-SAVE-PAYMENT-DATE-R  REDEFINES  WS-SAVE-PAYMENT-DATE.
00089          05  FILLER             PIC 9(4).
00090          05  SAVE-PAY-MO        PIC 99.
00091          05  SAVE-PAY-DAYS      PIC 99.
00092
00093      03  WS-SAVE-VALUATION-DATE PIC 9(8).
00094      03  WS-SAVE-VAL-DATE-R  REDEFINES  WS-SAVE-VALUATION-DATE.
00095          05  FILLER             PIC 9(6).
00096          05  SAVE-VAL-DAYS      PIC 99.
00097
00098      03  WS-SAVE-EXPIRATION-DATE PIC 9(8).
00099      03  WS-SAVE-EXP-DATE-R  REDEFINES  WS-SAVE-EXPIRATION-DATE.
00100          05  FILLER             PIC 9(6).
00101          05  SAVE-EXP-DAYS      PIC 99.
00102
00103      03  WS-SAVE-BINARY-DATE    PIC XX.
00104
00105 *    03  WS-SAVE-MONTH-END-DATE.
00106 *        05  FILLER             PIC S99.
00107 *        05  FILLER             PIC S99.
00108 *        05  WS-DAYS-IN-THIS-MO PIC S99.
00109
00110  01  MISC-SWITCHES.
00111      03  PAYMENT-EOM        PIC X.
00112          88 PAYMENT-DATE-EOM    VALUE 'Y'.
00113      03  EFFECTIVE-EOM      PIC X.
00114          88 EFFECTIVE-DATE-EOM  VALUE 'Y'.
00115      03  VALUATION-EOM      PIC X.
00116          88 VALUATION-DATE-EOM  VALUE 'Y'.
00117      03  TOP-EOM            PIC X.
00118          88 TOP-NUMBER-EOM      VALUE 'Y'.
00119      03  BOTTOM-EOM         PIC X.
00120          88 BOTTOM-NUMBER-EOM   VALUE 'Y'.
00121      03  EXPIRATION-EOM     PIC X.
00122          88 EXPIRATION-DATE-EOM VALUE 'Y'.
00123
00124      EJECT
00125 *                           COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
00126
00127      EJECT
00128 *                           COPY ELCCALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCALC.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
00008 *                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
00009 *                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
00010 *                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
00011 *                                                                *
00012 *  PASSED TO ELRTRM                                              *
00013 *  -----------------                                             *
00014 *  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
00015 *  ORIGINAL TERM                                                 *
00016 *  BEGINNING DATE                                                *
00017 *  ENDING DATE                                                   *
00018 *  COMPANY I.D.                                                  *
00019 *  ACCOUNT MASTER USER FIELD                                     *
00020 *  PROCESS SWITCH (CANCEL, CLAIM)                                *
00021 *  FREE LOOK DAYS                                                *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  REMAINING TERM 1 - USED FOR EARNINGS                          *
00026 *  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
00027 *  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
00028 *  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
00029 *----------------------------------------------------------------*
00030 *  PASSED TO ELRAMT                                              *
00031 *  ----------------                                              *
00032 *  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
00033 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00034 *  ORIGINAL AMOUNT                                               *
00035 *  ALTERNATE BENEFIT (BALLON)                                    *
00036 *  A.P.R. - NET PAY ONLY                                         *
00037 *  METHOD
00038 *  PAYMENT FREQUENCY - FOR FARM PLAN                             *
00039 *  COMPANY I.D.                                                  *
00040 *  BENEFIT TYPE                                                  *
00041 *                                                                *
00042 *  RETURNED FROM ELRAMT                                          *
00043 *  --------------------                                          *
00044 *  REMAINING AMOUNT 1 - CURRENT                                  *
00045 *  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
00046 *  REMAINING AMOUNT FACTOR
00047 *----------------------------------------------------------------*
00048 *  PASSED TO ELRESV                                              *
00049 *  -----------------                                             *
00050 *  CERTIFICATE EFFECTIVE DATE                                    *
00051 *  VALUATION DATE                                                *
00052 *  PAID THRU DATE                                                *
00053 *  BENEFIT                                                       *
00054 *  INCURRED DATE                                                 *
00055 *  REPORTED DATE                                                 *
00056 *  ISSUE AGE                                                     *
00057 *  TERM                                                          *
00058 *  CDT PERCENT                                                   *
00059 *  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
00060 * *CLAIM TYPE (LIFE, A/H)                                        *
00061 * *REMAINING BENEFIT (FROM ELRAMT)                               *
00062 * *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
00063 *                                                                *
00064 *  RETURNED FROM ELRESV                                          *
00065 *  --------------------                                          *
00066 *  CDT TABLE USED                                                *
00067 *  CDT FACTOR USED                                               *
00068 *  PAY TO CURRENT RESERVE                                        *
00069 *  I.B.N.R. - A/H ONLY                                           *
00070 *  FUTURE (ACCRUED) AH ONLY                                      *
00071 *----------------------------------------------------------------*
00072 *  PASSED TO ELRATE                                              *
00073 *  ----------------                                              *
00074 *  CERT ISSUE DATE                                               *
00075 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00076 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00077 *  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
00078 *  STATE CODE (CLIENT DEFINED)                                   *
00079 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00080 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00081 *  DEVIATION CODE                                                *
00082 *  ISSUE AGE                                                     *
00083 *  ORIGINAL BENEFIT AMOUNT                                       *
00084 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00085 *  PROCESS TYPE (ISSUE OR CANCEL)                                *
00086 *  BENEFIT KIND (LIFE OR A/H)                                    *
00087 *  A.P.R.                                                        *
00088 *  METHOD
00089 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00090 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00091 *  COMPANY I.D. (3 CHARACTER)                                    *
00092 *  BENEFIT CODE                                                  *
00093 *  BENEFIT OVERRIDE CODE                                         *
00094 *  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
00095 *  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
00096 *  JOINT INDICATOR (CSL ONLY)                                    *
00097 *  FIRST PAYMENT DATE (CSL ONLY)                                 *
00098 *  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
00099 *                                                                *
00100 *  RETURNED FROM ELRATE                                          *
00101 *  --------------------                                          *
00102 *  CALCULATED PREMIUM                                            *
00103 *  PREMIUM RATE                                                  *
00104 *  MORTALITY CODE                                                *
00105 *  MAX ATTAINED AGE                                              *
00106 *  MAX AGE                                                       *
00107 *  MAX TERM                                                      *
00108 *  MAX MONTHLY BENEFIT                                           *
00109 *  MAX TOTAL BENIFIT                                             *
00110 *  COMPOSITE RATE (OPEN-END ONLY)                                *
00111 *----------------------------------------------------------------*
00112 *  PASSED TO ELRFND                                              *
00113 *  ----------------                                              *
00114 *  CERT ISSUE DATE                                               *
00115 *  REFUND DATE                                                   *
00116 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00117 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00118 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00119 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00120 *  STATE CODE (CLIENT DEFINED)                                   *
00121 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00122 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00123 *  DEVIATION CODE                                                *
00124 *  ISSUE AGE                                                     *
00125 *  ORIGINAL BENEFIT AMOUNT                                       *
00126 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00127 *  PROCESS TYPE (CANCEL)                                         *
00128 *  BENEFIT KIND (LIFE OR A/H)                                    *
00129 *  A.P.R.                                                        *
00130 *  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
00131 *  RATING METHOD -  (CODE FROM BENEFIT)                          *
00132 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00133 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00134 *  COMPANY I.D. (3 CHARACTER)                                    *
00135 *  BENEFIT CODE                                                  *
00136 *  BENEFIT OVERRIDE CODE                                         *
00137 *                                                                *
00138 *  RETURNED FROM ELRFND                                          *
00139 *  --------------------                                          *
00140 *  CALCULATED REFUND                                             *
00141 *----------------------------------------------------------------*
00142 *  PASSED TO ELEARN                                              *
00143 *  ----------------                                              *
00144 *  CERT ISSUE DATE                                               *
00145 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00146 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00147 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00148 *  STATE CODE (CLIENT DEFINED)                                   *
00149 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00150 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00151 *  DEVIATION CODE                                                *
00152 *  ISSUE AGE                                                     *
00153 *  ORIGINAL BENEFIT AMOUNT                                       *
00154 *  BENEFIT KIND (LIFE OR A/H)                                    *
00155 *  A.P.R.                                                        *
00156 *  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
00157 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00158 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00159 *  COMPANY I.D. (3 CHARACTER)                                    *
00160 *  BENEFIT CODE                                                  *
00161 *  BENEFIT OVERRIDE CODE                                         *
00162 *                                                                *
00163 *  RETURNED FROM ELEARN                                          *
00164 *  --------------------                                          *
00165 *  INDICATED  EARNINGS                                           *
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 450                                   *
00168 *                                                                *
00169 ******************************************************************
010303******************************************************************
010303*                   C H A N G E   L O G
010303*
010303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010303*-----------------------------------------------------------------
010303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010303* EFFECTIVE    NUMBER
010303*-----------------------------------------------------------------
010303* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
101807* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
010410* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
010410* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
041310* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
041710* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
010303******************************************************************
00170
00171  01  CALCULATION-PASS-AREA.
00172      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
00173                                      COMP.
00174
00175      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CP-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
00178                                   '9' 'A' 'B' 'C' 'D' 'E' 'H'.
00179        88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
00180        88  CP-ERROR-IN-DATES                       VALUE '2'.
00181        88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
00182        88  CP-ERROR-IN-TERMS                       VALUE '4'.
00183        88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
00184        88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
00185        88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
00186        88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
00187        88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
00188        88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
00189        88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
00190        88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
00191        88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
00192        88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
00193        88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
00194        88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
00195        88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
00196
00197      12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
00198        88  NO-CP-ERROR-2                           VALUE ZERO.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
00202          16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
00203          16  CP-CERT-EFF-DT        PIC XX.
00204          16  CP-VALUATION-DT       PIC XX.
00205          16  CP-PAID-THRU-DT       PIC XX.
00206          16  CP-BENEFIT-TYPE       PIC X.
00207            88  CP-AH                               VALUE 'A' 'D'
00208                                                    'I' 'U'.
00209            88  CP-REDUCING-LIFE                    VALUE 'R'.
00210            88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
00211          16  CP-INCURRED-DT        PIC XX.
00212          16  CP-REPORTED-DT        PIC XX.
00213          16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
00214          16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
00215          16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
00216                                      COMP-3.
00217          16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
00218                                      COMP-3.
00219          16  CP-CDT-METHOD         PIC X.
00220            88  CP-CDT-ROUND-NEAR                   VALUE '1'.
00221            88  CP-CDT-ROUND-HIGH                   VALUE '2'.
00222            88  CP-CDT-INTERPOLATED                 VALUE '3'.
00223          16  CP-CLAIM-TYPE         PIC X.
00224            88  CP-AH-CLAIM                         VALUE 'A'.
00225            88  CP-LIFE-CLAIM                       VALUE 'L'.
00226          16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
00227                                      COMP-3.
00228          16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
00229                                      COMP-3.
00230          16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
00231                                      COMP-3.
00232          16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
00233                                      COMP-3.
00234          16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
00235                                      COMP-3.
00236          16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
00237                                      COMP-3.
00238          16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
00239                                      COMP-3.
00240          16  CP-REM-TERM-METHOD    PIC X.
00241            88  CP-EARN-AFTER-15TH                  VALUE '1'.
00242            88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
00243            88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
00244            88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
00245            88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
00246            88  CP-EARN-AFTER-14TH                  VALUE '6'.
00247            88  CP-EARN-AFTER-16TH                  VALUE '7'.
00248          16  CP-EARNING-METHOD     PIC X.
00249            88  CP-EARN-BY-R78                      VALUE '1' 'R'.
00250            88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
00251            88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
00252            88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
00253            88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
00254            88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
00255            88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
00256            88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
00257            88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
00258            88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
033104           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
033104           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
00259          16  CP-PROCESS-TYPE       PIC X.
00260            88  CP-CLAIM                            VALUE '1'.
00261            88  CP-CANCEL                           VALUE '2'.
00262            88  CP-ISSUE                            VALUE '3'.
00263          16  CP-SPECIAL-CALC-CD    PIC X.
00264            88  CP-OUTSTANDING-BAL              VALUE 'O'.
00265            88  CP-1-MTH-INTEREST               VALUE ' '.
00266            88  CP-0-MTH-INTEREST               VALUE 'A'.
00267            88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
00268            88  CP-CRITICAL-PERIOD              VALUE 'C'.
00269            88  CP-TERM-IS-DAYS                 VALUE 'D'.
00270            88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
00271            88  CP-FARM-PLAN                    VALUE 'F'.
00272            88  CP-RATE-AS-STANDARD             VALUE 'G'.
00273            88  CP-2-MTH-INTEREST               VALUE 'I'.
00274            88  CP-3-MTH-INTEREST               VALUE 'J'.
00275            88  CP-4-MTH-INTEREST               VALUE 'K'.
00276            88  CP-BALLOON-LAST-PMT             VALUE 'L'.
00277            88  CP-MORTGAGE-REC                 VALUE 'M'.
00278            88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
00279            88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
00280            88  CP-NET-PAY-SIMPLE               VALUE 'S'.
00281            88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
00282                                                      'W' 'X'.
00283            88  CP-TRUNCATE-0-MTH               VALUE 'T'.
00284            88  CP-TRUNCATE-1-MTH               VALUE 'U'.
00285            88  CP-TRUNCATE-2-MTH               VALUE 'V'.
00286            88  CP-TRUNCATE-3-MTH               VALUE 'W'.
00287            88  CP-TRUNCATE-4-MTH               VALUE 'X'.
00288            88  CP-SUMMARY-REC                  VALUE 'Z'.
00289            88  CP-PROPERTY-BENEFIT             VALUE '2'.
00290            88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
00291            88  CP-AD-D-BENEFIT                 VALUE '4'.
00292            88  CP-CSL-METH-1                   VALUE '5'.
00293            88  CP-CSL-METH-2                   VALUE '6'.
00294            88  CP-CSL-METH-3                   VALUE '7'.
00295            88  CP-CSL-METH-4                   VALUE '8'.
00296
00297          16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
00298                                      COMP-3.
00299          16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
00300          16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
00301          16  CP-STATE              PIC XX          VALUE SPACE.
00302          16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
00303          16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
00304            88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
00305                '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
00306          16  CP-R78-OPTION         PIC X.
00307            88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
00308            88  CP-TERM-TIMES-TERM                  VALUE '1'.
00309
00310          16  CP-COMPANY-CD         PIC X             VALUE SPACE.
00311          16  CP-IBNR-RESERVE-SW    PIC X.
00312          16  CP-CLAIM-STATUS       PIC X.
00313          16  CP-RATE-FILE          PIC X.
00314          16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
00315                                      COMP-3.
00316
00317          16  CP-LIFE-OVERRIDE-CODE PIC X.
00318          16  CP-AH-OVERRIDE-CODE   PIC X.
00319
00320          16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
00321                                      COMP-3.
00322          16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
00323                                      COMP-3.
00324          16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
00325                                      COMP-3.
00326          16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
00327                                      COMP-3.
00328
00329          16  CP-PAID-FROM-DATE     PIC X(02).
00330          16  CP-CLAIM-CALC-METHOD  PIC X(01).
00331          16  CP-EXT-DAYS-CALC      PIC X.
00332            88  CP-EXT-NO-CHG                   VALUE ' '.
00333            88  CP-EXT-CHG-LF                   VALUE '1'.
00334            88  CP-EXT-CHG-AH                   VALUE '2'.
00335            88  CP-EXT-CHG-LF-AH                VALUE '3'.
00336          16  CP-DOMICILE-STATE     PIC XX.
00337          16  CP-CARRIER            PIC X.
00338          16  CP-REIN-FLAG          PIC X.
00339          16  CP-REM-TRM-CALC-OPTION PIC X.
00340            88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
00341                       '2' '3' '4' '5'.
00342            88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
00343            88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
00344            88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
00345            88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
00346            88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
00347            88  CP-EXT-30-DAY-MONTH          VALUE '3'.
00348            88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
                 88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
00349          16  CP-SIG-SWITCH         PIC X.
00350          16  CP-RATING-METHOD      PIC X.
00351            88  CP-RATE-AS-R78                      VALUE '1' 'R'.
00352            88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
00353            88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
00354            88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
00355            88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
00356            88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
00357            88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
00358            88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
00359            88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
00360          16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
00361                                      COMP-3.
090803         16  CP-BEN-CATEGORY       PIC X.
011904         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
                                         PIC S99V9(5) COMP-3.
011904         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
                                         PIC S99V9(5) COMP-3.
080305         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
               16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
041310         16  CP-EXPIRE-DT          PIC XX.
041710         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
041710         16  FILLER                PIC X(35).
090803*        16  FILLER                PIC X(50).
00363
00364 ***************    OUTPUT FROM ELRESV   ************************
00365
00366          16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
00367
00368          16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
00369                                      COMP-3.
101807         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  FILLER                PIC X(09).
00377 ***************    OUTPUT FROM ELRTRM   *************************
00378
00379          16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
00380                                      COMP-3.
00381          16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
00382                                      COMP-3.
00383          16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
00384                                      COMP-3.
00385          16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
00386                                      COMP-3.
00387          16  FILLER                PIC X(12).
00388
00389 ***************    OUTPUT FROM ELRAMT   *************************
00390
00391          16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
00392                                      COMP-3.
00393          16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
00394                                      COMP-3.
00395          16  FILLER                PIC X(12).
00396
00397 ***************    OUTPUT FROM ELRATE   *************************
00398
00399          16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
00400                                      COMP-3.
00401          16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
00402                                      COMP-3.
00403          16  CP-MORTALITY-CODE     PIC X(4).
00404          16  CP-RATE-EDIT-FLAG     PIC X.
00405              88  CP-RATES-NEED-APR                  VALUE '1'.
00406          16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
00407                                      COMP-3.
00408          16  CP-POLICY-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
032905         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
               16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
                                         PIC S9(7)V99 COMP-3.
00409          16  FILLER                PIC X(07).
00410
00411 ***************    OUTPUT FROM ELRFND   *************************
00412
00413          16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
00414                                      COMP-3.
00415          16  CP-REFUND-TYPE-USED   PIC X.
00416            88  CP-R-AS-R78                         VALUE '1'.
00417            88  CP-R-AS-PRORATA                     VALUE '2'.
00418            88  CP-R-AS-CALIF                       VALUE '3'.
00419            88  CP-R-AS-TEXAS                       VALUE '4'.
00420            88  CP-R-AS-FARM-PLAN                   VALUE '4'.
00421            88  CP-R-AS-NET-PAY                     VALUE '5'.
00422            88  CP-R-AS-ANTICIPATION                VALUE '6'.
00423            88  CP-R-AS-MEAN                        VALUE '8'.
00424            88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
033104           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
033104           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
00425          16  FILLER                PIC X(12).
00426
00427 ***************    OUTPUT FROM ELEARN   *************************
00428
00429          16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
00430                                      COMP-3.
00431          16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
00432                                      COMP-3.
00433          16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
00434                                      COMP-3.
00435          16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
00436                                      COMP-3.
00437          16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
00438                                      COMP-3.
00439          16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
00440                                      COMP-3.
00441          16  CP-EARNING-TYPE-USED  PIC X.
00442            88  CP-E-AS-SPECIAL                     VALUE 'S'.
00443            88  CP-E-AS-R78                         VALUE '1'.
00444            88  CP-E-AS-PRORATA                     VALUE '2'.
00445            88  CP-E-AS-TEXAS                       VALUE '4'.
00446            88  CP-E-AS-FARM-PLAN                   VALUE '4'.
00447            88  CP-E-AS-NET-PAY                     VALUE '5'.
00448            88  CP-E-AS-ANTICIPATION                VALUE '6'.
00449            88  CP-E-AS-MEAN                        VALUE '8'.
00450            88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
00451          16  FILLER                PIC X(12).
00452
00453 ***************    OUTPUT FROM ELPMNT   *************************
00454
00455          16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
00456                                      COMP-3.
00457          16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
00458                                      COMP-3.
00459          16  FILLER                PIC X(12).
00460
00461 ***************   MISC WORK AREAS    *****************************
00462          16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
00463                                      COMP-3.
00464          16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
00465                                      COMP-3.
00466          16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
00467                                      COMP-3.
00468          16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
00469                                      COMP-3.
00470          16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
00471                                      COMP-3.
00472          16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
00473                                      COMP-3.
00474          16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
00475              88  OPEN-RATE-FILE                   VALUE 'O'.
00476              88  CLOSE-RATE-FILE                  VALUE 'C'.
00477              88  IO-ERROR                         VALUE 'E'.
00478
00479          16  CP-FIRST-PAY-DATE     PIC XX.
00480
00481          16  CP-JOINT-INDICATOR    PIC X.
00482
00483          16  CP-RESERVE-REMAINING-TERM
00484                                    PIC S9(4)V9    VALUE ZERO
00485                                      COMP-3.
00486
00487          16  CP-INSURED-BIRTH-DT   PIC XX.
00488
00489          16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
00490                                      COMP-3.
00491
00492          16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
00493                                      COMP-3.
00494
00495          16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
00496                                      COMP-3.
00497
00498          16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
00499                                      COMP-3.
00500
00501          16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
00502                                      COMP-3.
00503
00504          16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
00505                                      COMP-3.
00506
00507          16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
00508                                      COMP-3.
00509
00510          16  CP-ROA-REFUND         PIC X          VALUE 'N'.
00511              88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
00512
010303         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
010303                                     COMP-3.
041710         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
041710                                     COMP-3.
041710         16  FILLER                PIC X(17).
00514 ******************************************************************
00129
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00131  01  DFHCOMMAREA              PIC X(450).
00132
00133      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'ELRTRM' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00135      MOVE DFHCOMMAREA            TO  CALCULATION-PASS-AREA.
00136
00137  000-RT-REMAINING-TERM-RTN.
00138 *                    COPY ELCRTMPD.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCRTMPD                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012.                         *
00006 *                                                                *
00007 ******************************************************************
00008
00009      MOVE SPACE                  TO EFFECTIVE-EOM
00010                                     VALUATION-EOM
00011                                     PAYMENT-EOM
00012                                     EXPIRATION-EOM
00013                                     TOP-EOM
00014                                     BOTTOM-EOM.
00015
00016      MOVE ZERO                   TO CP-RETURN-CODE
00017                                     CP-RETURN-CODE-2
00018                                     CP-REMAINING-TERM-1
00019                                     CP-REMAINING-TERM-2
00020                                     CP-REMAINING-TERM-3
00021                                     CP-ODD-DAYS.
00022
00023 ******************************************************************
00024 *   IF THE ORIGINAL TERM IS NOT VALID, THEN EXIT FROM THE        *
00025 *  ROUTINE INDICATING THAT THERE WAS AN ERROR DETECTED WITH      *
00026 *  THE TERM.                                                     *
00027 ******************************************************************
00028
00029      IF CP-ORIGINAL-TERM NOT NUMERIC
00030          MOVE ZERO               TO CP-ORIGINAL-TERM.
00031
00032      IF CP-ORIGINAL-TERM = ZERO
00033          MOVE '4'                TO CP-RETURN-CODE
00034          GO TO 9999-RT-REMAINING-TERM-XIT.
00035
00036      EJECT
00037
00038      IF CP-FIRST-PAY-DATE = SPACES  OR  LOW-VALUES
00039          NEXT SENTENCE
00040      ELSE
00041          IF CP-FIRST-PAY-DATE NOT GREATER THAN CP-CERT-EFF-DT
00042              MOVE LOW-VALUES     TO CP-FIRST-PAY-DATE.
00043
00044 ******************************************************************
00045 *   IF THE FIRST PAYMENT DATE IS NOT A VALID DATE, THEN          *
00046 *  CALCULATE A DEFAULT PAYMENT DATE I.E. ONE MONTH AFTER         *
00047 *  THE EFFECTIVE, AND CONTINUE PROCESSING.                       *
00048 ******************************************************************
00049
00050      IF CP-FIRST-PAY-DATE = SPACES  OR  LOW-VALUES
00051          MOVE CP-CERT-EFF-DT     TO DC-BIN-DATE-1
00052          MOVE +1                 TO DC-ELAPSED-MONTHS
00053          MOVE ZEROS              TO DC-ELAPSED-DAYS
00054          MOVE '6'                TO DC-OPTION-CODE
00055          PERFORM 8000-DATE-CONVERT THRU 8000-EXIT
00056          IF NO-CONVERSION-ERROR
00057              MOVE DC-BIN-DATE-2  TO CP-FIRST-PAY-DATE
00058          ELSE
00059              MOVE '2'            TO CP-RETURN-CODE
00060              GO TO 9999-RT-REMAINING-TERM-XIT.
00061
00062      MOVE CP-ORIGINAL-TERM       TO CP-REMAINING-TERM-1
00063                                     CP-REMAINING-TERM-2
00064                                     CP-REMAINING-TERM-3.
00065
00066      EJECT
00067 ******************************************************************
00068 *   IF THE CERT EFFECTIVE DATE EQUAL TO THE CANCEL DATE,         *
00069 *  EXIT FROM THE ROUTINE WITH THE ORIGINAL TERM EQUAL TO THE     *
00070 *  REMAINING TERM - NO PREMIUM EARNED.                           *
00071 ******************************************************************
00072
00073      IF CP-CERT-EFF-DT NOT LESS THAN CP-VALUATION-DT
00074          GO TO 9999-RT-REMAINING-TERM-XIT.
00075
00076      IF VALID-REM-TRM-CALC-OPTION
00077          NEXT SENTENCE
00078      ELSE
00079          GO TO 1000-CONTROL-CALCULATION.
00080
00081 ******************************************************************
00082 *  IF THE POLICY IS CANCELED DURING A "FREE-LOOK" PERIOD,        *
00083 *  EXIT FROM THE ROUTINE WITH THE ORGINAL TERM EQUAL TO THE      *
00084 *  REMAINING TERM - NO PREMIUM EARNED.                           *
00085 *  (ELASPED DAYS FROM EFFECTIVE DATE TO CANCEL DATE IS COMPARED  *
00086 *  TO FREE LOOK DAYS)                                            *
00087 ******************************************************************
00088
00089      IF CP-FREE-LOOK NOT NUMERIC
00090           MOVE ZERO               TO CP-FREE-LOOK.
00091
00092      IF CP-FREE-LOOK NOT = ZERO
00093           MOVE CP-CERT-EFF-DT     TO DC-BIN-DATE-1
00094           MOVE CP-VALUATION-DT    TO DC-BIN-DATE-2
00095           MOVE '1'                TO DC-OPTION-CODE
00096           PERFORM 8000-DATE-CONVERT THRU 8000-EXIT
00097           IF DATE-CONVERSION-ERROR
00098             MOVE '2'                TO CP-RETURN-CODE
00099             GO TO 9999-RT-REMAINING-TERM-XIT
00100           ELSE
00101              IF DC-ELAPSED-DAYS NOT GREATER THAN CP-FREE-LOOK
00102                 GO TO 9999-RT-REMAINING-TERM-XIT.
00103
00104 ******************************************************************
00105 *   INITIALIZE DATE FIELDS USED LATER TO CALCULATE ELAPSED TIME  *
00106 ******************************************************************
00107
00108      MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1.
00109      MOVE ' '                    TO DC-OPTION-CODE.
00110      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT
00111      IF DATE-CONVERSION-ERROR
00112          MOVE '2'                TO CP-RETURN-CODE
00113          GO TO 9999-RT-REMAINING-TERM-XIT.
00114
00115      MOVE DC-GREG-DATE-CYMD      TO WS-SAVE-EFFECTIVE-DATE.
00116      IF SAVE-EFF-DAYS EQUAL DC-DAYS-IN-MONTH
00117          MOVE 'Y'                TO EFFECTIVE-EOM.
00118
00119      MOVE CP-VALUATION-DT        TO DC-BIN-DATE-1.
00120      MOVE ' '                    TO DC-OPTION-CODE.
00121      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT
00122      IF DATE-CONVERSION-ERROR
00123          MOVE '2'                TO CP-RETURN-CODE
00124          GO TO 9999-RT-REMAINING-TERM-XIT.
00125
00126      MOVE DC-GREG-DATE-CYMD      TO WS-SAVE-VALUATION-DATE.
00127      IF SAVE-VAL-DAYS EQUAL DC-DAYS-IN-MONTH
00128          MOVE 'Y'                TO VALUATION-EOM.
00129
00130
00131      MOVE CP-FIRST-PAY-DATE      TO DC-BIN-DATE-1.
00132      MOVE ' '                    TO DC-OPTION-CODE.
00133      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT
00134      IF DATE-CONVERSION-ERROR
00135          MOVE '2'                TO CP-RETURN-CODE
00136          GO TO 9999-RT-REMAINING-TERM-XIT.
00137
00138      MOVE DC-GREG-DATE-CYMD      TO WS-SAVE-PAYMENT-DATE.
00139      IF SAVE-PAY-DAYS EQUAL DC-DAYS-IN-MONTH
00140          MOVE 'Y'                TO PAYMENT-EOM.
00141
00142
00143  EJECT
00144 ****************************************************************
00145 *   THREE OPTIONS:                                             *
00146 *     1) CALCULATE TIME ELAPSED FROM PAYMENT DATE              *
00147 *       TO CANCEL DATE CONSIDERING EXTENSION DAYS              *
00148 *                                                              *
00149 *     2) CALCULATE TIME ELAPSED FROM EFFECTIVE DATE            *
00150 *       TO CANCEL DATE NOT CONSIDERING EXTENSION DAYS          *
00151 *                                                              *
00152 *     3) IF COMPANY ID IS 'NCL' AND ELASPED TIME METHOD IS     *
00153 *       '3' OR '4', CALCULATE REMAINING TIME AS                *
00154 *      (EXPIRATION DATE - CANCEL DATE = REMAINING TIME)        *
00155 *                                                              *
00156 *   IF THE CANCEL DATE IS LESS THAN THE 1ST PAYMENT DATE,      *
00157 *   OPTION 2 WILL BE USED (DOES NOT APPLY IF OPTION 3 IS TRUE) *
00158 ****************************************************************
00159
00160  001-CONTROL-OPTIONS.
00161
           IF (CP-CONSIDER-EXTENSION)
                       AND
              ((CP-COMPANY-ID = 'NCL')
                OR (CP-USE-EXP-AND-1ST-PMT))
              GO TO 600-PROCESS-FOR-NCL
           END-IF
00162 *    IF CP-CONSIDER-EXTENSION AND
00163 *       CP-COMPANY-ID = 'NCL'
00164 *        GO TO 600-PROCESS-FOR-NCL.
00165
00166      IF CP-VALUATION-DT LESS THAN CP-FIRST-PAY-DATE
00167          GO TO 100-CANCELLED-BEFORE-PAYMENT.
00168
00169      IF CP-CONSIDER-EXTENSION
00170          GO TO 200-PROCESS-WITH-1ST-PMT-DATE
00171      ELSE
00172          GO TO 300-PROCESS-WITH-CERT-EFF-DATE.
00173
00174  EJECT
00175
00176 ****************************************************************
00177 *                                                              *
00178 *   PROCESSING WHEN VALUATION DATE LESS THAN 1ST PAYMENT DATE  *
00179 *   OR WHEN REMAINING TERM OPTION '1' OR '2' IS CHOSEN.        *
00180 *                                                              *
00181 ****************************************************************
00182
00183 ****************************************************************
00184 *                                                              *
00185 *   CALCULATE THE TIME ELAPSED BETWEEN THE CANCEL DATE AND     *
00186 *   THE CERT EFFECTIVE DATE.                                   *
00187 *                                                              *
00188 ****************************************************************
00189  100-CANCELLED-BEFORE-PAYMENT.
00190
00191      MOVE WS-SAVE-EFFECTIVE-DATE TO WS-BOTTOM-NUMBER.
00192
00193      MOVE WS-SAVE-VALUATION-DATE TO WS-TOP-NUMBER.
00194      MOVE CP-VALUATION-DT        TO WS-SAVE-BINARY-DATE.
00195      IF VALUATION-DATE-EOM AND EFFECTIVE-DATE-EOM
00196          MOVE ZEROS              TO WS-TOP-DA
00197                                     WS-BOTTOM-DA.
00198
00199      PERFORM 500-SIMPLE-ARITHMETIC THRU 500-EXIT.
00200
00201      IF CP-EARN-AFTER-15TH
00202         GO TO 110-METHOD-1.
00203      IF CP-EARN-ON-HALF-MONTH
00204         GO TO 120-METHOD-2.
00205      IF CP-EARN-ON-1ST-DAY
00206         GO TO 130-METHOD-3.
00207      IF CP-EARN-ON-FULL-MONTH
00208         GO TO 140-METHOD-4.
00209      IF CP-EARN-WITH-NO-DAYS
00210         GO TO 150-METHOD-5.
00211      IF CP-EARN-AFTER-14TH
00212         GO TO 160-METHOD-6.
00213      IF CP-EARN-AFTER-16TH
00214         GO TO 170-METHOD-7.
00215
00216 ****************************************************************
00217 *                                                              *
00218 *   IF NONE OF THE ABOVE OPTIONS HAVE BEEN SPECIFIED, THEN     *
00219 *   INDICATE THERE WAS AN ERROR IN THE OPTION SELECTION AND    *
00220 *   EXIT FROM THE ROUTINE.                                     *
00221 *                                                              *
00222 ****************************************************************
00223      MOVE '3' TO CP-RETURN-CODE
00224      GO TO 9999-RT-REMAINING-TERM-XIT.
00225
00226  EJECT
00227 ****************************************************************
00228 *                                                              *
00229 *                       NUMBER OF DAYS                         *
00230 *         POLICY MUST BE INFORCE FOR 15 DAYS TO EARN A MONTH   *
00231 *                                                              *
00232 ****************************************************************
00233
00234  110-METHOD-1.
00235
00236
00237      IF WS-RESULT-DA     GREATER THAN +15  OR
00238         TERM-IN-MONTHS    GREATER THAN +0
00239          COMPUTE CP-REMAINING-TERM-1 =
00240                              CP-REMAINING-TERM-1 - (+1).
00241
00242      GO TO 199-END-CERT-DATE-PROCESSING.
00243
00244 ****************************************************************
00245 *                                                              *
00246 *                       HALF MONTH ADJUSTMENT                  *
00247 *            PREMIUM EARNS ON 1/2 MONTH MORE ASSUMPTION        *
00248 *                                                              *
00249 ****************************************************************
00250
00251  120-METHOD-2.
00252
00253      IF TERM-IN-MONTHS    GREATER THAN +0
00254          COMPUTE CP-REMAINING-TERM-1 =
00255                        CP-REMAINING-TERM-1 - (+1.5)
00256      ELSE
00257          COMPUTE CP-REMAINING-TERM-1 =
00258                        CP-REMAINING-TERM-1 - (+.5).
00259
00260      IF CP-REMAINING-TERM-1 NEGATIVE
00261          MOVE ZEROS                 TO CP-REMAINING-TERM-1.
00262
00263
00264      GO TO 199-END-CERT-DATE-PROCESSING.
00265
00266
00267  EJECT
00268 ****************************************************************
00269 *                                                              *
00270 *                       FIRST DAY METHOD                       *
00271 *                  PREMIUM EARNS ON FIRST DAY                  *
00272 *                                                              *
00273 ****************************************************************
00274
00275  130-METHOD-3.
00276
00277      IF WS-RESULT-DA     GREATER THAN +0  OR
00278         TERM-IN-MONTHS    GREATER THAN +0
00279          COMPUTE CP-REMAINING-TERM-1 =
00280                              CP-REMAINING-TERM-1 - (+1).
00281
00282      GO TO 199-END-CERT-DATE-PROCESSING.
00283
00284 ****************************************************************
00285 *                                                              *
00286 *                       FULL MONTH METHOD                      *
00287 *          PREMIUM EARNS ON FULL MONTH - SAME AS BENEFIT       *
00288 *                                                              *
00289 ****************************************************************
00290
00291  140-METHOD-4.
00292
00293
00294 *    WILL NOT REDUCE REMAINING TERM ON 'FULL' MONTH UNTIL
00295 *        PAST THE FIRST PAYMENT DATE.
00296
00297      GO TO 199-END-CERT-DATE-PROCESSING.
00298
00299
00300  EJECT
00301 ****************************************************************
00302 *                                                              *
00303 *                       FULL MONTH - NO DAYS                   *
00304 *          PREMIUM EARNS A FULL MONTH STARTING THE FIRST MONTH *
00305 *       (THIS METHOD WILL EARN WITHOUT A FLUCTUATION IN SHORT  *
00306 *        MONTHS )                                              *
00307 ****************************************************************
00308
00309  150-METHOD-5.
00310
00311
00312      COMPUTE CP-REMAINING-TERM-1 =
00313                              CP-REMAINING-TERM-1 - (+1).
00314
00315      GO TO 199-END-CERT-DATE-PROCESSING.
00316
00317  EJECT
00318 ****************************************************************
00319 *                                                              *
00320 *                       NUMBER OF DAYS                         *
00321 *         POLICY MUST BE INFORCE FOR 14 DAYS TO EARN A MONTH   *
00322 *                                                              *
00323 ****************************************************************
00324
00325  160-METHOD-6.
00326
00327
00328      IF WS-RESULT-DA     GREATER THAN +14  OR
00329         TERM-IN-MONTHS    GREATER THAN +0
00330          COMPUTE CP-REMAINING-TERM-1 =
00331                              CP-REMAINING-TERM-1 - (+1).
00332
00333      GO TO 199-END-CERT-DATE-PROCESSING.
00334
00335 ****************************************************************
00336 *                                                              *
00337 *                       NUMBER OF DAYS                         *
00338 *         POLICY MUST BE INFORCE FOR 16 DAYS TO EARN A MONTH   *
00339 *                                                              *
00340 ****************************************************************
00341
00342  170-METHOD-7.
00343
00344
00345      IF WS-RESULT-DA     GREATER THAN +16  OR
00346         TERM-IN-MONTHS    GREATER THAN +0
00347          COMPUTE CP-REMAINING-TERM-1 =
00348                              CP-REMAINING-TERM-1 - (+1).
00349
00350      GO TO 199-END-CERT-DATE-PROCESSING.
00351
00352
00353  EJECT
00354  199-END-CERT-DATE-PROCESSING.
00355
00356      IF CP-REMAINING-TERM-1 NEGATIVE
00357         MOVE ZERO                TO CP-REMAINING-TERM-1
00358                                     CP-REMAINING-TERM-2
00359                                     CP-REMAINING-TERM-3
00360         GO TO 9999-RT-REMAINING-TERM-XIT.
00361
00362      MOVE WS-RESULT-DA           TO CP-ODD-DAYS.
00363
00364      IF CP-EARN-AS-REG-BALLOON
00365         IF CP-BALLOON-LAST-PMT
00366            ADD +1 TO CP-REMAINING-TERM-3.
00367
00368      GO TO 9999-RT-REMAINING-TERM-XIT.
00369
00370  EJECT
00371
00372 ****************************************************************
00373 *                                                              *
00374 *  PROCESSING FOR VALUATION DATE ON OR AFTER 1ST PAYMENT DATE  *
00375 *  AND CONSIDERING EXTENSION DAYS.                             *
00376 *                                                              *
00377 ****************************************************************
00378
00379  200-PROCESS-WITH-1ST-PMT-DATE.
00380
00381 ****************************************************************
00382 *                                                              *
00383 *  COMPUTE AN EXPIRATION DATE USING THE 1ST PAYMENT DATE  AND  *
00384 *  THE (REMAINING TERM - 1). IF THE VALUATION DATE IS GREATER  *
00385 *  THAN THIS ESTIMATED DATE, THEN THE TERM IS CONSIDERED TO    *
00386 *  BE FULLY EXPIRED.                                           *
00387 *                                                              *
00388 ****************************************************************
00389
00390      MOVE CP-FIRST-PAY-DATE      TO DC-BIN-DATE-1.
00391
00392      COMPUTE DC-ELAPSED-MONTHS EQUAL
00393          (CP-ORIGINAL-TERM - +1).
00394
00395      MOVE ZEROS                  TO DC-ELAPSED-DAYS.
00396      MOVE '6'                    TO DC-OPTION-CODE.
00397      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT.
00398
00399      IF DATE-CONVERSION-ERROR
00400         MOVE '2' TO CP-RETURN-CODE
00401         GO TO 9999-RT-REMAINING-TERM-XIT
00402      ELSE
00403         IF DC-BIN-DATE-2 NOT GREATER CP-VALUATION-DT
00404             MOVE ZEROS TO CP-REMAINING-TERM-1
00405                           CP-REMAINING-TERM-2
00406                           CP-REMAINING-TERM-3
00407             GO TO 9999-RT-REMAINING-TERM-XIT.
00408
00409
00410  EJECT
00411 ****************************************************************
00412 *  REMAINING-TERM-2 REFLECTS THE NUMBER OF PAYMENTS THAT A     *
00413 *  INSURED HAS LEFT TO PAY. REQUARDLESS OF THE AMOUNT OF TIME  *
00414 *  THAT HAS ELAPSED FROM THE EFFECTIVE DATE TO THE FIRST       *
00415 *  PAYMENT DATE, ONLY ONE MONTH IS EARNED.  THE NUMBER OF      *
00416 *  PAYMENTS IS THEN EVALUATED FROM THE FIRST PAYMENT DATE TO   *
00417 *  THE VALUATION DATE.                                         *
00418 *                                                              *
00419 *  IF THE VALUATION DATE IS GREATER THAN THE 1ST PAYMENT       *
00420 *  DATE, ONE MONTH HAS BEEN EARNED FOR REMAINING TERM-2.       *
00421 *                                                              *
00422 *   NOTE : THE MAXIMUM MONTHS EARNED FORM THE EFFECTIVE DATE   *
00423 *     TO THE PAYMENT DATE IS 1.                                *
00424 *                                                              *
00425 ****************************************************************
00426
00427      IF CP-VALUATION-DT NOT LESS THAN CP-FIRST-PAY-DATE
00428          MOVE 1                  TO MONTHS-EARNED-2
00429      ELSE
00430          MOVE 0                  TO MONTHS-EARNED-2.
00431
00432      MOVE WS-SAVE-VALUATION-DATE TO WS-TOP-NUMBER.
00433      MOVE CP-VALUATION-DT        TO WS-SAVE-BINARY-DATE.
00434
00435      MOVE WS-SAVE-PAYMENT-DATE   TO WS-BOTTOM-NUMBER.
00436
00437      PERFORM 500-SIMPLE-ARITHMETIC THRU 500-EXIT.
00438
00439      COMPUTE CP-REMAINING-TERM-2 = CP-ORIGINAL-TERM -
00440                           (TERM-IN-MONTHS  + MONTHS-EARNED-2).
00441
00442      IF CP-REMAINING-TERM-2 NEGATIVE
00443         MOVE 0                   TO CP-REMAINING-TERM-1
00444                                     CP-REMAINING-TERM-2
00445                                     CP-REMAINING-TERM-3
00446                                     CP-ODD-DAYS
00447         GO TO 9999-RT-REMAINING-TERM-XIT.
00448
00449      MOVE CP-REMAINING-TERM-2    TO CP-REMAINING-TERM-3.
00450
00451      IF CP-EARN-AS-REG-BALLOON
00452         IF CP-BALLOON-LAST-PMT
00453            ADD +1 TO CP-REMAINING-TERM-3.
00454
00455      IF CP-EARN-ON-FULL-MONTH
00456         MOVE CP-REMAINING-TERM-2    TO CP-REMAINING-TERM-1
00457         MOVE 0                      TO CP-ODD-DAYS
00458         GO TO 9999-RT-REMAINING-TERM-XIT.
00459
00460
00461  EJECT
00462 ****************************************************************
00463 *  REMAINING-TERM-1 IS USED FOR REFUNDS ETC.  THE TERM IS      *
00464 *  CALCULATED FROM THE FIRST PAYMENT DATE TO THE VALUATION     *
00465 *  DATE EARNING NO MORE THAN ONE MONTH FROM EFFECTIVE DATE TO  *
00466 *  1ST PAYMENT DATE.                                           *
00467 *                                                              *
00468 *  THE TIME ELAPSED FROM THE EFFECTIVE DATE AND THE FIRST      *
00469 *  PAYMENT DATE MUST BE 1 MONTH BEFORE IT IS CONSIDERED        *
00470 *  EARNED.                                                     *
00471 *                                                              *
00472 *   NOTE : THE MAXIMUM MONTHS EARNED FORM THE EFFECTIVE DATE   *
00473 *     TO THE PAYMENT DATE IS 1.                                *
00474 *                                                              *
00475 ****************************************************************
00476
00477      MOVE WS-SAVE-PAYMENT-DATE   TO WS-TOP-NUMBER.
00478      MOVE CP-FIRST-PAY-DATE      TO WS-SAVE-BINARY-DATE.
00479
00480      MOVE WS-SAVE-EFFECTIVE-DATE TO WS-BOTTOM-NUMBER.
00481
00482      IF PAYMENT-DATE-EOM AND EFFECTIVE-DATE-EOM
00483          MOVE ZEROS              TO WS-TOP-DA
00484                                     WS-BOTTOM-DA.
00485
00486      PERFORM 500-SIMPLE-ARITHMETIC THRU 500-EXIT.
00487
00488      MOVE ZERO                   TO MONTHS-EARNED-1.
00489
00490      IF TERM-IN-MONTHS  GREATER THAN +0
00491          MOVE 1                    TO MONTHS-EARNED-1
00492          MOVE WS-SAVE-PAYMENT-DATE TO WS-BOTTOM-NUMBER
00493          GO TO 205-1ST-PAY-TO-VALUATION-DATE
00494      ELSE
00495          MOVE WS-SAVE-EFFECTIVE-DATE TO WS-BOTTOM-NUMBER
00496          GO TO 205-1ST-PAY-TO-VALUATION-DATE.
00497
00498  EJECT
00499 ****************************************************************
00500 *                                                              *
00501 *  COMPUTE THE ELAPSED TIME BETWEEN THE 1ST PAYMENT DATE       *
00502 *  AND THE VALUATION/CANCEL DATE.                              *
00503 *   THE REMAINING TERM THEN BECOMES :                          *
00504 *                                                              *
00505 *  ORIGINAL TERM - (CALCULATED ELAPSED MONTHS + MONTHS EARNED) *
00506 *                                                              *
00507 ****************************************************************
00508
00509  205-1ST-PAY-TO-VALUATION-DATE.
00510
00511      MOVE WS-SAVE-VALUATION-DATE TO WS-TOP-NUMBER.
00512      MOVE CP-VALUATION-DT        TO WS-SAVE-BINARY-DATE.
00513
00514      PERFORM 500-SIMPLE-ARITHMETIC THRU 500-EXIT.
00515
00516      MOVE WS-RESULT-DA           TO CP-ODD-DAYS.
00517
00518      COMPUTE CP-REMAINING-TERM-1 = CP-ORIGINAL-TERM -
00519                           (TERM-IN-MONTHS  + MONTHS-EARNED-1).
00520
00521  EJECT
00522 ****************************************************************
00523 * IF THIS TERM IS NEGATIVE, THE TERM IS CONSIDERED TO BE       *
00524 *  EXPIRED AND ALL PREMIUM IS EARNED.                          *
00525 ****************************************************************
00526      IF CP-REMAINING-TERM-1 NEGATIVE
00527         MOVE ZERO                TO CP-REMAINING-TERM-1
00528                                     CP-ODD-DAYS
00529         GO TO 9999-RT-REMAINING-TERM-XIT.
00530
00531      IF CP-EARN-AFTER-15TH
00532         GO TO 410-METHOD-1.
00533      IF CP-EARN-ON-HALF-MONTH
00534         GO TO 420-METHOD-2.
00535      IF CP-EARN-ON-1ST-DAY
00536         GO TO 430-METHOD-3.
00537      IF CP-EARN-ON-FULL-MONTH
00538         GO TO 440-METHOD-4.
00539      IF CP-EARN-WITH-NO-DAYS
00540         GO TO 450-METHOD-5.
00541      IF CP-EARN-AFTER-14TH
00542         GO TO 460-METHOD-6.
00543      IF CP-EARN-AFTER-16TH
00544         GO TO 470-METHOD-7.
00545
00546
00547 ****************************************************************
00548 *                                                              *
00549 *   IF NONE OF THE ABOVE OPTIONS HAVE BEEN SPECIFIED, THEN     *
00550 *   INDICATE THERE WAS AN ERROR IN THE OPTION SELECTION AND    *
00551 *   EXIT FROM THE ROUTINE.                                     *
00552 *                                                              *
00553 ****************************************************************
00554
00555      MOVE '3' TO CP-RETURN-CODE
00556      GO TO 9999-RT-REMAINING-TERM-XIT.
00557
00558  EJECT
00559 ****************************************************************
00560 *                                                              *
00561 *  PROCESSING FOR VALUATION DATE AFTER THE CERT-EFFECTIVE DATE *
00562 *  NOT CONSIDERING EXTENSION DAYS.                             *
00563 *                                                              *
00564 ****************************************************************
00565
00566  300-PROCESS-WITH-CERT-EFF-DATE.
00567
00568 ****************************************************************
00569 *                                                              *
00570 *  COMPUTE AN EXPIRATION DATE USING THE CERTIFICATE EFFECTIVE  *
00571 *  DATE AND ORIGINAL TERM. IF THE CANCEL DATE IS GREATER THAN  *
00572 *  THIS ESTIMATED DATE, THEN THE TERM IS CONSIDERED TO BE FULLY*
00573 *  EXHAUSTED.                                                  *
00574 *                                                              *
00575 ****************************************************************
00576
00577      MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1.
00578      MOVE CP-ORIGINAL-TERM       TO DC-ELAPSED-MONTHS.
00579      MOVE ZEROS                  TO DC-ELAPSED-DAYS.
00580      MOVE '6'                    TO DC-OPTION-CODE.
00581      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT.
00582
00583      IF DATE-CONVERSION-ERROR
00584         MOVE '2' TO CP-RETURN-CODE
00585         GO TO 9999-RT-REMAINING-TERM-XIT
00586      ELSE
00587         IF DC-BIN-DATE-2 LESS THAN CP-VALUATION-DT
00588             MOVE ZEROS TO CP-REMAINING-TERM-1
00589                           CP-REMAINING-TERM-2
00590                           CP-REMAINING-TERM-3
00591             GO TO 9999-RT-REMAINING-TERM-XIT.
00592  EJECT
00593 ****************************************************************
00594 *                                                              *
00595 *  COMPUTE THE ELAPSED TIME BETWEEN THE EFFECTIVE DATE         *
00596 *  AND THE CANCEL DATE.                                        *
00597 *                                                              *
00598 ****************************************************************
00599
00600  305-EFF-TO-VALUATION-DATE.
00601
00602      MOVE WS-SAVE-VALUATION-DATE TO WS-TOP-NUMBER.
00603      MOVE CP-VALUATION-DT        TO WS-SAVE-BINARY-DATE.
00604
00605      MOVE WS-SAVE-EFFECTIVE-DATE TO WS-BOTTOM-NUMBER.
00606
00607      IF VALUATION-DATE-EOM AND EFFECTIVE-DATE-EOM
00608          MOVE ZEROS              TO WS-TOP-DA
00609                                     WS-BOTTOM-DA.
00610
00611      PERFORM 500-SIMPLE-ARITHMETIC THRU 500-EXIT.
00612
00613      COMPUTE CP-REMAINING-TERM-1 = CP-ORIGINAL-TERM -
00614                             TERM-IN-MONTHS.
00615
00616      MOVE WS-RESULT-DA           TO CP-ODD-DAYS.
00617
00618  EJECT
00619 ****************************************************************
00620 * IF THIS TERM IS NEGATIVE, THE TERM IS CONSIDERED TO BE       *
00621 *  EXPIRED AND ALL PREMIUM IS EARNED.                          *
00622 ****************************************************************
00623      IF CP-REMAINING-TERM-1 NEGATIVE
00624         MOVE ZERO                TO CP-REMAINING-TERM-1
00625                                     CP-REMAINING-TERM-2
00626                                     CP-REMAINING-TERM-3
00627                                     CP-ODD-DAYS
00628         GO TO 9999-RT-REMAINING-TERM-XIT.
00629
00630      MOVE CP-REMAINING-TERM-1    TO CP-REMAINING-TERM-2
00631                                     CP-REMAINING-TERM-3.
00632
00633      IF CP-EARN-AS-REG-BALLOON
00634         IF CP-BALLOON-LAST-PMT
00635            ADD +1 TO CP-REMAINING-TERM-3.
00636
00637      IF CP-EARN-AFTER-15TH
00638         GO TO 410-METHOD-1.
00639      IF CP-EARN-ON-HALF-MONTH
00640         GO TO 420-METHOD-2.
00641      IF CP-EARN-ON-1ST-DAY
00642         GO TO 430-METHOD-3.
00643      IF CP-EARN-ON-FULL-MONTH
00644         GO TO 440-METHOD-4.
00645      IF CP-EARN-WITH-NO-DAYS
00646         GO TO 450-METHOD-5.
00647      IF CP-EARN-AFTER-14TH
00648         GO TO 460-METHOD-6.
00649      IF CP-EARN-AFTER-16TH
00650         GO TO 470-METHOD-7.
00651
00652
00653 ****************************************************************
00654 *                                                              *
00655 *   IF NONE OF THE ABOVE OPTIONS HAVE BEEN SPECIFIED, THEN     *
00656 *   INDICATE THERE WAS AN ERROR IN THE OPTION SELECTION AND    *
00657 *   EXIT FROM THE ROUTINE.                                     *
00658 *                                                              *
00659 ****************************************************************
00660
00661      MOVE '3' TO CP-RETURN-CODE
00662      GO TO 9999-RT-REMAINING-TERM-XIT.
00663
00664  EJECT
00665 ****************************************************************
00666 *                                                              *
00667 *                       NUMBER OF DAYS                         *
00668 *         POLICY MUST BE INFORCE FOR 15 DAYS TO EARN A MONTH   *
00669 *                                                              *
00670 ****************************************************************
00671
00672  410-METHOD-1.
00673
00674
00675      IF WS-RESULT-DA     GREATER THAN +15
00676          COMPUTE CP-REMAINING-TERM-1 =
00677                              CP-REMAINING-TERM-1 - (+1).
00678
00679      GO TO 9999-RT-REMAINING-TERM-XIT.
00680
00681
00682 ****************************************************************
00683 *                                                              *
00684 *                       HALF MONTH ADJUSTMENT                  *
00685 *            PREMIUM EARNS ON 1/2 MONTH MORE ASSUMPTION        *
00686 *                                                              *
00687 ****************************************************************
00688
00689  420-METHOD-2.
00690
00691      IF CP-REMAINING-TERM-1 NOT = ZERO
00692          COMPUTE CP-REMAINING-TERM-1 =
00693                              CP-REMAINING-TERM-1 - (+.5).
00694
00695      GO TO 9999-RT-REMAINING-TERM-XIT.
00696
00697 *     NO ADJUSTMENT TO CP-REMAINING-TERM-3 FOR DAYS - TYPE 2.
00698
00699
00700  EJECT
00701 ****************************************************************
00702 *                                                              *
00703 *                       FIRST DAY METHOD                       *
00704 *                  PREMIUM EARNS ON FIRST DAY                  *
00705 *                                                              *
00706 ****************************************************************
00707
00708  430-METHOD-3.
00709
00710      IF WS-RESULT-DA     GREATER THAN +0
00711          COMPUTE CP-REMAINING-TERM-1 =
00712                              CP-REMAINING-TERM-1 - (+1).
00713
00714      GO TO 9999-RT-REMAINING-TERM-XIT.
00715
00716
00717 ****************************************************************
00718 *                                                              *
00719 *                       FULL MONTH METHOD                      *
00720 *          PREMIUM EARNS ON FULL MONTH - SAME AS BENEFIT       *
00721 *                                                              *
00722 ****************************************************************
00723
00724  440-METHOD-4.
00725
00726 *  NO ADJUSTMENT FOR CP-REMAINING-TERM-1 - TYPE 4.
00727
00728
00729      GO TO 9999-RT-REMAINING-TERM-XIT.
00730
00731
00732  EJECT
00733 ****************************************************************
00734 *                                                              *
00735 *                       FULL MONTH - NO DAYS                   *
00736 *          PREMIUM EARNS A FULL MONTH STARTING THE FIRST MONTH *
00737 *       (THIS METHOD WILL EARN WITHOUT A FLUCTUATION IN SHORT  *
00738 *        MONTHS )                                              *
00739 ****************************************************************
00740
00741  450-METHOD-5.
00742
00743
00744      IF CP-REMAINING-TERM-1 NOT = ZERO
00745          COMPUTE CP-REMAINING-TERM-1 =
00746                              CP-REMAINING-TERM-1 - (+1).
00747
00748      GO TO 9999-RT-REMAINING-TERM-XIT.
00749
00750 ****************************************************************
00751 *                                                              *
00752 *                       NUMBER OF DAYS                         *
00753 *         POLICY MUST BE INFORCE FOR 14 DAYS TO EARN A MONTH   *
00754 *                                                              *
00755 ****************************************************************
00756
00757  460-METHOD-6.
00758
00759
00760      IF WS-RESULT-DA     GREATER THAN +14
00761          COMPUTE CP-REMAINING-TERM-1 =
00762                              CP-REMAINING-TERM-1 - (+1).
00763
00764      GO TO 9999-RT-REMAINING-TERM-XIT.
00765
00766 ****************************************************************
00767 *                                                              *
00768 *                       NUMBER OF DAYS                         *
00769 *     POLICY MUST BE INFORCE FOR 16 DAYS TO EARN A MONTH       *
00770 *                                                              *
00771 ****************************************************************
00772
00773  470-METHOD-7.
00774
00775
00776      IF WS-RESULT-DA     GREATER THAN +16
00777          COMPUTE CP-REMAINING-TERM-1 =
00778                              CP-REMAINING-TERM-1 - (+1).
00779
00780      GO TO 9999-RT-REMAINING-TERM-XIT.
00781
00782 ****************************************************************
00783 *                                                              *
00784 *                                                              *
00785 *                                                              *
00786 *                                                              *
00787 *                                                              *
00788 ****************************************************************
00789
00790  500-SIMPLE-ARITHMETIC.
00791
00792  500-BORROW-IF-LESS.
00793      IF WS-TOP-DA NOT LESS THAN WS-BOTTOM-DA
00794         GO TO 500-CHECK-MONTH.
00795
00796 * BORROW BASED ON 30 DAY MONTH
00797
00798      IF CP-30-DAY-MONTH
00799          COMPUTE WS-TOP-DA = WS-TOP-DA + 30
00800      ELSE
00801          MOVE WS-SAVE-BINARY-DATE    TO DC-BIN-DATE-1
00802          MOVE -1                     TO DC-ELAPSED-MONTHS
00803          MOVE '1'                    TO DC-END-OF-MONTH
00804          MOVE ZERO                   TO DC-ELAPSED-DAYS
00805          MOVE '6'                    TO DC-OPTION-CODE
00806          PERFORM 8000-DATE-CONVERT THRU 8000-EXIT
00807          IF DATE-CONVERSION-ERROR
00808              MOVE '2'                TO CP-RETURN-CODE
00809              GO TO 500-EXIT
00810          ELSE
00811              COMPUTE WS-TOP-DA = WS-TOP-DA + DC-DAYS-IN-MONTH.
00812
00813      COMPUTE WS-TOP-MO = WS-TOP-MO - 1.
00814
00815      IF WS-TOP-MO EQUAL 0
00816         COMPUTE WS-TOP-CCYR = WS-TOP-CCYR - 1
00817         MOVE 12             TO WS-TOP-MO.
00818
00819  500-CHECK-MONTH.
00820      IF WS-TOP-MO NOT LESS THAN WS-BOTTOM-MO
00821         GO TO 500-SUBTRACT.
00822
00823      COMPUTE WS-TOP-CCYR = WS-TOP-CCYR - 1.
00824
00825      COMPUTE WS-TOP-MO = WS-TOP-MO + 12.
00826
00827  500-SUBTRACT.
00828
00829      COMPUTE WS-RESULT-CCYR = WS-TOP-CCYR - WS-BOTTOM-CCYR.
00830
00831      COMPUTE WS-RESULT-MO = WS-TOP-MO - WS-BOTTOM-MO.
00832      IF WS-RESULT-CCYR GREATER THAN ZERO
00833         COMPUTE TERM-IN-MONTHS = WS-RESULT-MO +
00834                       (WS-RESULT-CCYR * 12)
00835      ELSE
00836         MOVE WS-RESULT-MO          TO TERM-IN-MONTHS.
00837
00838      COMPUTE WS-RESULT-DA = WS-TOP-DA - WS-BOTTOM-DA.
00839
00840 *    IF CP-30-DAY-MONTH
00841 *       AND WS-RESULT-DA EQUAL 30
00842 *         COMPUTE WS-RESULT-MO = WS-RESULT-MO + 1
00843 *         MOVE 0                   TO WS-RESULT-DA.
00844
00845
00846  500-EXIT.
00847      EXIT.
00848      EJECT
00849 ****************************************************************
00850  600-PROCESS-FOR-NCL.
00851 ****************************************************************
00852 *                                                              *
00853 *  COMPUTE AN EXPIRATION DATE USING THE 1ST PAYMENT DATE  AND  *
00854 *  THE (REMAINING TERM - 1). IF THE VALUATION DATE IS GREATER  *
00855 *  THAN THIS ESTIMATED DATE, THEN THE TERM IS CONSIDERED TO    *
00856 *  BE FULLY EXPIRED.                                           *
00857 *                                                              *
00858 ****************************************************************
00859
00860      MOVE CP-FIRST-PAY-DATE      TO DC-BIN-DATE-1.
00861
00862      COMPUTE DC-ELAPSED-MONTHS EQUAL
00863          (CP-ORIGINAL-TERM - +1).
00864
00865      MOVE ZEROS                  TO DC-ELAPSED-DAYS.
00866      MOVE '6'                    TO DC-OPTION-CODE.
00867      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT.
00868
00869      IF DATE-CONVERSION-ERROR
00870         MOVE '2' TO CP-RETURN-CODE
00871         GO TO 9999-RT-REMAINING-TERM-XIT
00872      ELSE
00873         IF DC-BIN-DATE-2 NOT GREATER CP-VALUATION-DT
00874             MOVE ZEROS TO CP-REMAINING-TERM-1
00875                           CP-REMAINING-TERM-2
00876                           CP-REMAINING-TERM-3
00877             GO TO 9999-RT-REMAINING-TERM-XIT.
00878
00879      MOVE DC-BIN-DATE-2          TO WS-SAVE-BINARY-DATE.
00880      MOVE DC-BIN-DATE-2          TO DC-BIN-DATE-1.
00881      MOVE ' '                    TO DC-OPTION-CODE.
00882      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT
00883      IF DATE-CONVERSION-ERROR
00884          MOVE '2'                TO CP-RETURN-CODE
00885          GO TO 9999-RT-REMAINING-TERM-XIT.
00886
00887      MOVE DC-GREG-DATE-CYMD      TO WS-SAVE-EXPIRATION-DATE.
00888      IF SAVE-EXP-DAYS EQUAL DC-DAYS-IN-MONTH
00889          MOVE 'Y'                TO EXPIRATION-EOM.
00890 ****************************************************************
00891 *                                                              *
00892 *  COMPUTE THE ELAPSED TIME BETWEEN THE CANCEL DATE AND        *
00893 *  EXPIRATION (MATURITY DATE). THIS WILL BE THE REMAINING      *
00894 *  TERM.                                                       *
00895 ****************************************************************
00896
00897      MOVE WS-SAVE-EXPIRATION-DATE TO WS-TOP-NUMBER.
00898      MOVE DC-BIN-DATE-2          TO WS-SAVE-BINARY-DATE.
00899
00900      MOVE WS-SAVE-VALUATION-DATE TO WS-BOTTOM-NUMBER.
00901
00902      IF VALUATION-DATE-EOM AND EXPIRATION-DATE-EOM
00903          MOVE ZEROS              TO WS-TOP-DA
00904                                     WS-BOTTOM-DA.
00905
00906      PERFORM 500-SIMPLE-ARITHMETIC THRU 500-EXIT.
00907
00908      MOVE TERM-IN-MONTHS         TO CP-REMAINING-TERM-1
00909
00910      MOVE WS-RESULT-DA           TO CP-ODD-DAYS.
00911
00912  EJECT
00913 ****************************************************************
00914 * IF THIS TERM IS NEGATIVE, THE TERM IS CONSIDERED TO BE       *
00915 *  EXPIRED AND ALL PREMIUM IS EARNED.                          *
00916 ****************************************************************
00917      IF CP-REMAINING-TERM-1 NEGATIVE
00918         MOVE ZERO                TO CP-REMAINING-TERM-1
00919                                     CP-REMAINING-TERM-2
00920                                     CP-REMAINING-TERM-3
00921                                     CP-ODD-DAYS
00922         GO TO 9999-RT-REMAINING-TERM-XIT.
00923
00924      MOVE CP-REMAINING-TERM-1    TO CP-REMAINING-TERM-2
00925                                     CP-REMAINING-TERM-3.
00926
00927      IF CP-ODD-DAYS GREATER THAN ZERO
00928         ADD +1                   TO CP-REMAINING-TERM-2
00929         ADD +1                   TO CP-REMAINING-TERM-3.
00930
00931      IF CP-EARN-AS-REG-BALLOON
00932         IF CP-BALLOON-LAST-PMT
00933            ADD +1 TO CP-REMAINING-TERM-3.
00934
00935      IF CP-EARN-AFTER-15TH
00936         GO TO 610-METHOD-1.
00937      IF CP-EARN-ON-HALF-MONTH
00938         GO TO 620-METHOD-2.
00939      IF CP-EARN-ON-1ST-DAY
00940         GO TO 630-METHOD-3.
00941      IF CP-EARN-ON-FULL-MONTH
00942         GO TO 640-METHOD-4.
00943      IF CP-EARN-WITH-NO-DAYS
00944         GO TO 650-METHOD-5.
00945      IF CP-EARN-AFTER-14TH
00946         GO TO 660-METHOD-6.
00947      IF CP-EARN-AFTER-16TH
00948         GO TO 670-METHOD-7.
00949 ****************************************************************
00950 *                                                              *
00951 *   IF NONE OF THE ABOVE OPTIONS HAVE BEEN SPECIFIED, THEN     *
00952 *   INDICATE THERE WAS AN ERROR IN THE OPTION SELECTION AND    *
00953 *   EXIT FROM THE ROUTINE.                                     *
00954 *                                                              *
00955 ****************************************************************
00956
00957
00958      MOVE '3' TO CP-RETURN-CODE
00959      GO TO 9999-RT-REMAINING-TERM-XIT.
00960
00961  EJECT
00962
00963 ****************************************************************
00964 *                                                              *
00965 *                       NUMBER OF DAYS                         *
00966 *         POLICY MUST BE INFORCE FOR 15 DAYS TO EARN A MONTH   *
00967 *                                                              *
00968 ****************************************************************
00969
00970  610-METHOD-1.
00971
00972      IF WS-RESULT-DA     GREATER THAN +14
00973          COMPUTE CP-REMAINING-TERM-1 =
00974                              CP-REMAINING-TERM-1 + (+1).
00975
00976      GO TO 680-END-NCL-PROCESSING.
00977
00978
00979 ****************************************************************
00980 *                                                              *
00981 *                       HALF MONTH ADJUSTMENT                  *
00982 *            PREMIUM EARNS ON 1/2 MONTH MORE ASSUMPTION        *
00983 *                                                              *
00984 ****************************************************************
00985
00986  620-METHOD-2.
00987
00988      COMPUTE CP-REMAINING-TERM-1 =
00989                              CP-REMAINING-TERM-1 + (+.5).
00990
00991      GO TO 680-END-NCL-PROCESSING.
00992
00993
00994  EJECT
00995 ****************************************************************
00996 *                                                              *
00997 *                       FIRST DAY METHOD                       *
00998 *                  PREMIUM EARNS ON FIRST DAY                  *
00999 *                                                              *
01000 ****************************************************************
01001
01002  630-METHOD-3.
01003
01004 *  NO ADJUSTMENT FOR CP-REMAINING-TERM-1 - TYPE 3 FOR NCL.
01005
01006      GO TO 680-END-NCL-PROCESSING.
01007
01008
01009 ****************************************************************
01010 *                                                              *
01011 *                       FULL MONTH METHOD                      *
01012 *          PREMIUM EARNS ON FULL MONTH - SAME AS BENEFIT       *
01013 *                                                              *
01014 ****************************************************************
01015
01016  640-METHOD-4.
01017
01018      MOVE CP-REMAINING-TERM-2       TO CP-REMAINING-TERM-1.
01019      MOVE 0                         TO CP-ODD-DAYS.
01020
01021      GO TO 680-END-NCL-PROCESSING.
01022
01023
01024  EJECT
01025 ****************************************************************
01026 *                                                              *
01027 *                       FULL MONTH - NO DAYS                   *
01028 *          PREMIUM EARNS A FULL MONTH STARTING THE FIRST MONTH *
01029 *       (THIS METHOD WILL EARN WITHOUT A FLUCTUATION IN SHORT  *
01030 *        MONTHS )                                              *
01031 ****************************************************************
01032
01033  650-METHOD-5.
01034
01035
01036 *  NO ADJUSTMENT FOR CP-REMAINING-TERM-1 - TYPE 5 FOR NCL.
01037
01038      GO TO 680-END-NCL-PROCESSING.
01039
01040
01041 ****************************************************************
01042 *                                                              *
01043 *                       NUMBER OF DAYS                         *
01044 *         POLICY MUST BE INFORCE FOR 14 DAYS TO EARN A MONTH   *
01045 *                                                              *
01046 ****************************************************************
01047
01048  660-METHOD-6.
01049
01050
01051      IF WS-RESULT-DA     GREATER THAN +13
01052          COMPUTE CP-REMAINING-TERM-1 =
01053                              CP-REMAINING-TERM-1 + (+1).
01054
01055      GO TO 680-END-NCL-PROCESSING.
01056
01057
01058 ****************************************************************
01059 *                                                              *
01060 *                       NUMBER OF DAYS                         *
01061 *     POLICY MUST BE INFORCE FOR 16 DAYS TO EARN A MONTH       *
01062 *                                                              *
01063 ****************************************************************
01064
01065  670-METHOD-7.
01066
01067      IF WS-RESULT-DA     GREATER THAN +15
01068          COMPUTE CP-REMAINING-TERM-1 =
01069                              CP-REMAINING-TERM-1 + (+1).
01070
01071      GO TO 680-END-NCL-PROCESSING.
01072
01073
01074  680-END-NCL-PROCESSING.
01075
01076      IF CP-REMAINING-TERM-1 > CP-ORIGINAL-TERM
01077         MOVE CP-ORIGINAL-TERM    TO CP-REMAINING-TERM-1
01078                                     CP-REMAINING-TERM-2
01079                                     CP-REMAINING-TERM-3
01080         MOVE ZEROS               TO CP-ODD-DAYS.
01081
01082      GO TO 9999-RT-REMAINING-TERM-XIT.
01083
01084  EJECT
01085
01086  1000-CONTROL-CALCULATION.
01087
01088      IF CP-VALUATION-DT NOT LESS THAN CP-FIRST-PAY-DATE
01089          GO TO 2000-PROCESS-WITH-1ST-PMT-DATE.
01090
01091
01092 ****************************************************************
01093 *                                                              *
01094 *  ORIGINAL                                                    *
01095 *   PROCESSING WHEN VALUATION DATE LESS THAN 1ST PAYMENT DATE  *
01096 *                                                              *
01097 ****************************************************************
01098
01099
01100  1000-PROCESS-WITH-CERT-EFF-DT.
01101
01102      MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1.
01103      MOVE CP-VALUATION-DT        TO DC-BIN-DATE-2.
01104      MOVE '1'                    TO DC-OPTION-CODE.
01105      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT
01106      IF DATE-CONVERSION-ERROR
01107          MOVE '2'                TO CP-RETURN-CODE
01108          GO TO 9999-RT-REMAINING-TERM-XIT.
01109
01110      IF CP-EARN-AFTER-15TH
01111         GO TO 1110-METHOD-1.
01112      IF CP-EARN-ON-HALF-MONTH
01113         GO TO 1200-METHOD-2.
01114      IF CP-EARN-ON-1ST-DAY
01115         GO TO 1300-METHOD-3.
01116      IF CP-EARN-ON-FULL-MONTH
01117         GO TO 1400-METHOD-4.
01118      IF CP-EARN-WITH-NO-DAYS
01119         GO TO 1500-METHOD-5.
01120
01121      IF CP-EARN-AFTER-14TH
01122         GO TO 1600-METHOD-6.
01123      IF CP-EARN-AFTER-16TH
01124         GO TO 1700-METHOD-7.
01125
01126      MOVE '3' TO CP-RETURN-CODE
01127      GO TO 9999-RT-REMAINING-TERM-XIT.
01128
01129  1110-METHOD-1.
01130
01131 ****************************************************************
01132 *                                                              *
01133 *  ORIGINAL                                                    *
01134 *                       NUMBER OF DAYS                         *
01135 *         POLICY MUST BE INFORCE FOR 15 DAYS TO EARN A MONTH   *
01136 *                                                              *
01137 ****************************************************************
01138
01139
01140      IF DC-ODD-DAYS-OVER GREATER THAN +15  OR
01141         DC-ELAPSED-MONTHS GREATER THAN +0
01142          COMPUTE CP-REMAINING-TERM-1 =
01143                              CP-REMAINING-TERM-1 - (+1).
01144
01145
01146      GO TO 1999-END-CERT-DATE-PROCESSING.
01147
01148  1200-METHOD-2.
01149
01150 ****************************************************************
01151 *                                                              *
01152 *  ORIGINAL                                                    *
01153 *                       HALF MONTH ADJUSTMENT                  *
01154 *            PREMIUM EARNS ON 1/2 MONTH MORE ASSUMPTION        *
01155 *                                                              *
01156 ****************************************************************
01157
01158      IF DC-ELAPSED-MONTHS GREATER THAN +0
01159          COMPUTE CP-REMAINING-TERM-1 =
01160                        CP-REMAINING-TERM-1 - (+1.5)
01161      ELSE
01162          COMPUTE CP-REMAINING-TERM-1 =
01163                        CP-REMAINING-TERM-1 - (+.5).
01164
01165      IF CP-REMAINING-TERM-1 NEGATIVE
01166          MOVE ZEROS                 TO CP-REMAINING-TERM-1.
01167
01168
01169      GO TO 1999-END-CERT-DATE-PROCESSING.
01170
01171
01172  1300-METHOD-3.
01173
01174 ****************************************************************
01175 *                                                              *
01176 *  ORIGINAL                                                    *
01177 *                       FIRST DAY METHOD                       *
01178 *                  PREMIUM EARNS ON FIRST DAY                  *
01179 *                                                              *
01180 ****************************************************************
01181
01182      IF DC-ODD-DAYS-OVER GREATER THAN +0  OR
01183         DC-ELAPSED-MONTHS GREATER THAN +0
01184          COMPUTE CP-REMAINING-TERM-1 =
01185                              CP-REMAINING-TERM-1 - (+1).
01186
01187      GO TO 1999-END-CERT-DATE-PROCESSING.
01188
01189  1400-METHOD-4.
01190
01191 ****************************************************************
01192 *                                                              *
01193 *  ORIGINAL                                                    *
01194 *                       FULL MONTH METHOD                      *
01195 *          PREMIUM EARNS ON FULL MONTH - SAME AS BENEFIT       *
01196 *                                                              *
01197 ****************************************************************
01198
01199
01200 *    WILL NOT REDUCE REMAINING TERM ON 'FULL' MONTH UNTIL
01201 *        PAST THE FIRST PAYMENT DATE.
01202
01203      GO TO 1999-END-CERT-DATE-PROCESSING.
01204
01205
01206  1500-METHOD-5.
01207
01208 ****************************************************************
01209 *                                                              *
01210 *  ORIGINAL                                                    *
01211 *                       FULL MONTH - NO DAYS                   *
01212 *          PREMIUM EARNS A FULL MONTH STARTING THE FIRST MONTH *
01213 *       (THIS METHOD WILL EARN WITHOUT A FLUCTUATION IN SHORT  *
01214 *        MONTHS )                                              *
01215 ****************************************************************
01216
01217
01218      COMPUTE CP-REMAINING-TERM-1 =
01219                              CP-REMAINING-TERM-1 - (+1).
01220
01221      GO TO 1999-END-CERT-DATE-PROCESSING.
01222
01223  1600-METHOD-6.
01224
01225 ****************************************************************
01226 *                                                              *
01227 *  ORIGINAL                                                    *
01228 *                       NUMBER OF DAYS                         *
01229 *         POLICY MUST BE INFORCE FOR 14 DAYS TO EARN A MONTH   *
01230 *                                                              *
01231 ****************************************************************
01232
01233
01234      IF DC-ODD-DAYS-OVER GREATER THAN +14  OR
01235         DC-ELAPSED-MONTHS GREATER THAN +0
01236          COMPUTE CP-REMAINING-TERM-1 =
01237                              CP-REMAINING-TERM-1 - (+1).
01238
01239
01240      GO TO 1999-END-CERT-DATE-PROCESSING.
01241
01242  1700-METHOD-7.
01243
01244 ****************************************************************
01245 *                                                              *
01246 *  ORIGINAL                                                    *
01247 *                       NUMBER OF DAYS                         *
01248 *         POLICY MUST BE INFORCE FOR 16 DAYS TO EARN A MONTH   *
01249 *                                                              *
01250 ****************************************************************
01251
01252
01253      IF DC-ODD-DAYS-OVER GREATER THAN +16  OR
01254         DC-ELAPSED-MONTHS GREATER THAN +0
01255          COMPUTE CP-REMAINING-TERM-1 =
01256                              CP-REMAINING-TERM-1 - (+1).
01257
01258
01259      GO TO 1999-END-CERT-DATE-PROCESSING.
01260
01261
01262
01263  1999-END-CERT-DATE-PROCESSING.
01264
01265      IF CP-REMAINING-TERM-1 NEGATIVE
01266         MOVE ZERO                TO CP-REMAINING-TERM-1
01267                                     CP-REMAINING-TERM-2
01268                                     CP-REMAINING-TERM-3
01269         GO TO 9999-RT-REMAINING-TERM-XIT.
01270
01271      MOVE DC-ODD-DAYS-OVER       TO CP-ODD-DAYS.
01272
01273      IF CP-EARN-AS-REG-BALLOON
01274         IF CP-BALLOON-LAST-PMT
01275            ADD +1 TO CP-REMAINING-TERM-3.
01276
01277      GO TO 9999-RT-REMAINING-TERM-XIT.
01278
01279
01280 ****************************************************************
01281 *                                                              *
01282 *  ORIGINAL                                                    *
01283 *  PROCESSING FOR VALUATION DATE ON OR AFTER 1ST PAYMENT DATE  *
01284 *                                                              *
01285 ****************************************************************
01286
01287
01288  2000-PROCESS-WITH-1ST-PMT-DATE.
01289
01290      MOVE CP-FIRST-PAY-DATE      TO DC-BIN-DATE-1
01291      COMPUTE DC-ELAPSED-MONTHS EQUAL
01292          (CP-ORIGINAL-TERM - +1)
01293      MOVE ZEROS                  TO DC-ELAPSED-DAYS
01294      MOVE '6'                    TO DC-OPTION-CODE
01295      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT
01296
01297      IF DATE-CONVERSION-ERROR
01298         MOVE '2' TO CP-RETURN-CODE
01299         GO TO 9999-RT-REMAINING-TERM-XIT
01300      ELSE
01301      IF DC-BIN-DATE-2 LESS THAN CP-VALUATION-DT
01302         MOVE ZEROS TO CP-REMAINING-TERM-1
01303                       CP-REMAINING-TERM-2
01304                       CP-REMAINING-TERM-3
01305         GO TO 9999-RT-REMAINING-TERM-XIT.
01306
01307      MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1.
01308      MOVE CP-FIRST-PAY-DATE      TO DC-BIN-DATE-2.
01309      MOVE '1'                    TO DC-OPTION-CODE.
01310      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT.
01311      IF DATE-CONVERSION-ERROR
01312          MOVE '2' TO CP-RETURN-CODE
01313          GO TO 9999-RT-REMAINING-TERM-XIT.
01314
01315      MOVE ZERO                   TO MONTHS-EARNED.
01316
01317      IF DC-ELAPSED-MONTHS GREATER THAN +0  OR
01318         CP-EARN-WITH-NO-DAYS OR
01319         CP-EARN-ON-FULL-MONTH
01320          MOVE 1                  TO MONTHS-EARNED
01321          GO TO 2050-1ST-PAY-TO-VALUATION-DATE.
01322
01323      IF CP-EARN-ON-1ST-DAY
01324          IF DC-ODD-DAYS-OVER GREATER THAN +0
01325              MOVE 1              TO MONTHS-EARNED
01326              GO TO 2050-1ST-PAY-TO-VALUATION-DATE.
01327
01328      IF CP-EARN-AFTER-15TH
01329          IF DC-ODD-DAYS-OVER GREATER THAN +15
01330              MOVE 1              TO MONTHS-EARNED
01331              GO TO 2050-1ST-PAY-TO-VALUATION-DATE.
01332
01333      IF CP-EARN-AFTER-14TH
01334          IF DC-ODD-DAYS-OVER GREATER THAN +14
01335              MOVE 1              TO MONTHS-EARNED
01336              GO TO 2050-1ST-PAY-TO-VALUATION-DATE.
01337
01338      IF CP-EARN-AFTER-16TH
01339          IF DC-ODD-DAYS-OVER GREATER THAN +16
01340              MOVE 1              TO MONTHS-EARNED.
01341
01342  2050-1ST-PAY-TO-VALUATION-DATE.
01343
01344      MOVE CP-FIRST-PAY-DATE      TO DC-BIN-DATE-1.
01345      MOVE CP-VALUATION-DT        TO DC-BIN-DATE-2.
01346      MOVE '1'                    TO DC-OPTION-CODE.
01347      PERFORM 8000-DATE-CONVERT THRU 8000-EXIT.
01348      IF DATE-CONVERSION-ERROR
01349          MOVE '2' TO CP-RETURN-CODE
01350          GO TO 9999-RT-REMAINING-TERM-XIT.
01351
01352      COMPUTE CP-REMAINING-TERM-1 = CP-ORIGINAL-TERM -
01353                               (DC-ELAPSED-MONTHS + MONTHS-EARNED).
01354
01355      MOVE DC-ODD-DAYS-OVER       TO CP-ODD-DAYS.
01356
01357      IF CP-REMAINING-TERM-1 NEGATIVE
01358         MOVE ZERO                TO CP-REMAINING-TERM-1
01359                                     CP-REMAINING-TERM-2
01360                                     CP-REMAINING-TERM-3
01361                                     CP-ODD-DAYS
01362         GO TO 9999-RT-REMAINING-TERM-XIT.
01363
01364      MOVE CP-REMAINING-TERM-1    TO CP-REMAINING-TERM-2
01365                                     CP-REMAINING-TERM-3.
01366
01367      IF CP-EARN-AS-REG-BALLOON
01368         IF CP-BALLOON-LAST-PMT
01369            ADD +1 TO CP-REMAINING-TERM-3.
01370
01371      IF CP-EARN-AFTER-15TH
01372         GO TO 2100-METHOD-1.
01373      IF CP-EARN-ON-HALF-MONTH
01374         GO TO 2200-METHOD-2.
01375      IF CP-EARN-ON-1ST-DAY
01376         GO TO 2300-METHOD-3.
01377      IF CP-EARN-ON-FULL-MONTH
01378         GO TO 2400-METHOD-4.
01379      IF CP-EARN-WITH-NO-DAYS
01380         GO TO 2500-METHOD-5.
01381
01382      IF CP-EARN-AFTER-14TH
01383         GO TO 2600-METHOD-6.
01384      IF CP-EARN-AFTER-16TH
01385         GO TO 2700-METHOD-7.
01386
01387      MOVE '3' TO CP-RETURN-CODE
01388      GO TO 9999-RT-REMAINING-TERM-XIT.
01389
01390  2100-METHOD-1.
01391
01392 ****************************************************************
01393 *  ORIGINAL                                                    *
01394 *                                                              *
01395 *                       NUMBER OF DAYS                         *
01396 *         POLICY MUST BE INFORCE FOR 15 DAYS TO EARN A MONTH   *
01397 *                                                              *
01398 ****************************************************************
01399
01400
01401      IF DC-ODD-DAYS-OVER GREATER THAN +15
01402          COMPUTE CP-REMAINING-TERM-1 =
01403                              CP-REMAINING-TERM-1 - (+1).
01404
01405      GO TO 9999-RT-REMAINING-TERM-XIT.
01406
01407
01408  2200-METHOD-2.
01409
01410 ****************************************************************
01411 *                                                              *
01412 *  ORIGINAL                                                    *
01413 *                       HALF MONTH ADJUSTMENT                  *
01414 *            PREMIUM EARNS ON 1/2 MONTH MORE ASSUMPTION        *
01415 *                                                              *
01416 ****************************************************************
01417
01418      IF CP-REMAINING-TERM-1 NOT = ZERO
01419          COMPUTE CP-REMAINING-TERM-1 =
01420                              CP-REMAINING-TERM-1 - (+.5).
01421
01422      GO TO 9999-RT-REMAINING-TERM-XIT.
01423
01424 *     NO ADJUSTMENT TO CP-REMAINING-TERM-3 FOR DAYS - TYPE 2.
01425
01426
01427  2300-METHOD-3.
01428
01429 ****************************************************************
01430 *                                                              *
01431 *  ORIGINAL                                                    *
01432 *                       FIRST DAY METHOD                       *
01433 *                  PREMIUM EARNS ON FIRST DAY                  *
01434 *                                                              *
01435 ****************************************************************
01436
01437      IF DC-ODD-DAYS-OVER GREATER THAN +0
01438          COMPUTE CP-REMAINING-TERM-1 =
01439                              CP-REMAINING-TERM-1 - (+1).
01440
01441      GO TO 9999-RT-REMAINING-TERM-XIT.
01442
01443
01444  2400-METHOD-4.
01445
01446 ****************************************************************
01447 *                                                              *
01448 *  ORIGINAL                                                    *
01449 *                       FULL MONTH METHOD                      *
01450 *          PREMIUM EARNS ON FULL MONTH - SAME AS BENEFIT       *
01451 *                                                              *
01452 ****************************************************************
01453
01454 *  NO ADJUSTMENT FOR CP-REMAINING-TERM-1 - TYPE 4.
01455
01456
01457      GO TO 9999-RT-REMAINING-TERM-XIT.
01458
01459
01460  2500-METHOD-5.
01461
01462 ****************************************************************
01463 *                                                              *
01464 *  ORIGINAL                                                    *
01465 *                       FULL MONTH - NO DAYS                   *
01466 *          PREMIUM EARNS A FULL MONTH STARTING THE FIRST MONTH *
01467 *       (THIS METHOD WILL EARN WITHOUT A FLUCTUATION IN SHORT  *
01468 *        MONTHS )                                              *
01469 ****************************************************************
01470
01471
01472      IF CP-REMAINING-TERM-1 NOT = ZERO
01473          COMPUTE CP-REMAINING-TERM-1 =
01474                              CP-REMAINING-TERM-1 - (+1).
01475
01476      GO TO 9999-RT-REMAINING-TERM-XIT.
01477
01478  2600-METHOD-6.
01479
01480 ****************************************************************
01481 *  ORIGINAL                                                    *
01482 *                                                              *
01483 *                       NUMBER OF DAYS                         *
01484 *         POLICY MUST BE INFORCE FOR 14 DAYS TO EARN A MONTH   *
01485 *                                                              *
01486 ****************************************************************
01487
01488
01489      IF DC-ODD-DAYS-OVER GREATER THAN +14
01490          COMPUTE CP-REMAINING-TERM-1 =
01491                              CP-REMAINING-TERM-1 - (+1).
01492
01493      GO TO 9999-RT-REMAINING-TERM-XIT.
01494
01495  2700-METHOD-7.
01496
01497 ****************************************************************
01498 *  ORIGINAL                                                    *
01499 *                                                              *
01500 *                       NUMBER OF DAYS                         *
01501 *         POLICY MUST BE INFORCE FOR 16 DAYS TO EARN A MONTH   *
01502 *                                                              *
01503 ****************************************************************
01504
01505
01506      IF DC-ODD-DAYS-OVER GREATER THAN +16
01507          COMPUTE CP-REMAINING-TERM-1 =
01508                              CP-REMAINING-TERM-1 - (+1).
01509
01510      GO TO 9999-RT-REMAINING-TERM-XIT.
01511
01512
01513 ******************************************************************
00139
00140 *    GO TO 9999-RT-REMAINING-TERM-XIT.
00141
00142  8000-DATE-CONVERT.
00143      
      * EXEC CICS LINK
00144 *         PROGRAM  ('ELDATCV')
00145 *         COMMAREA (DATE-CONVERSION-DATA)
00146 *         LENGTH   (DC-COMM-LENGTH)
00147 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00002506' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00148  8000-EXIT.
00149      EXIT.
00150
00151  9999-RT-REMAINING-TERM-XIT.
00152      MOVE CALCULATION-PASS-AREA  TO  DFHCOMMAREA.
00153
00154      
      * EXEC CICS RETURN
00155 *    END-EXEC.
      *    MOVE '.(                    &   #00002517' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00156
00157      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRTRM' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRTRM' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELRTRM' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
