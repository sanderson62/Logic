00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 ELPMNT.
00004 *                            VMOD=2.006
00005
00006 *AUTHOR.     LOGIC,INC.
00007 *            DALLAS, TEXAS.
00008 *
00009 *DATE-COMPILED.
00010
00011 *SECURITY.   *****************************************************
00012 *            *                                                   *
00013 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00014 *            *                                                   *
00015 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00016 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00017 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00018 *            *                                                   *
00019 *            *****************************************************
00020
00021 *REMARKS.    THIS SUBROUTINE CALCULATES A CLAIM PAYMENT
00022 *            FROM THE INFORMATION PASSED VIA THE ELCCALC
00023 *            COPYBOOK.
00024
00025      EJECT
00026  ENVIRONMENT DIVISION.
00027
00028  DATA DIVISION.
00029
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*   ELPMNT WORKING STORAGE     *'.
00034  77  FILLER  PIC X(32)  VALUE '******* VMOD=2.006 *************'.
00035
00036 *                                   COPY ELCDATE.
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
00037
00038  EJECT
00039 *                                   COPY ELCCALC.
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
00040
00041  EJECT
00042  01  MISC-WORK-AREAS.
00043      12  WS-DAYS-TOL         PIC S9(5)     VALUE +0      COMP-3.
00044      12  WS-PMT-TOL          PIC S9(7)V99  VALUE +0      COMP-3.
00045      12  WS-MAX-DAYS-TOL     PIC S9(5)     VALUE +0      COMP-3.
00046      12  WS-MAX-PMT-TOL      PIC S9(7)V99  VALUE +0      COMP-3.
00047      12  WS-MAX-LF-PMT-TOL   PIC S9(7)V99  VALUE +0      COMP-3.
00048      12  WS-AH-BEN-AMT       PIC S9(7)V999 VALUE +0      COMP-3.
00049      12  WS-COMPUTE-AMT      PIC S9(9)V999 VALUE +0      COMP-3.
00050      12  WS-TOL-SEVERITY     PIC X         VALUE ' '.
00051      12  WS-EARNING-METHOD   PIC X         VALUE ' '.
00052      12  WS-CALC-METHOD      PIC X         VALUE ' '.
00053      12  WS-PAY-TYPE         PIC X         VALUE ' '.
00054      12  WORK-DATE-FROM      PIC 9(8)      VALUE ZERO.
00055      12  WORK-DATE-FROM-R  REDEFINES  WORK-DATE-FROM.
00056          16  WDF-CCYY        PIC 9(4).
00057          16  WDF-CCYR  REDEFINES  WDF-CCYY.
00058              20  WDF-CC      PIC 99.
00059              20  WDF-YR      PIC 99.
00060          16  WDF-MONTH       PIC 99.
00061          16  WDF-DAY         PIC 99.
00062      12  WORK-DATE-THRU      PIC 9(8)      VALUE ZERO.
00063      12  WORK-DATE-THRU-R  REDEFINES  WORK-DATE-THRU.
00064          16  WDT-CCYY        PIC 9(4).
00065          16  WDT-CCYR  REDEFINES  WDT-CCYY.
00066              20  WDT-CC      PIC 99.
00067              20  WDT-YR      PIC 99.
00068          16  WDT-MONTH       PIC 99.
00069          16  WDT-DAY         PIC 99.
00070      12  FROM-DAYS-IN-MONTH  PIC 99        VALUE ZERO.
00071      12  THRU-DAYS-IN-MONTH  PIC 99        VALUE ZERO.
00072      12  FROM-PMT-DAYS       PIC 99        VALUE ZERO.
00073      12  THRU-PMT-DAYS       PIC 99        VALUE ZERO.
00074      12  SAVE-ELAPSED-MONTHS PIC 999       VALUE ZERO.
00075      12  SAVE-ELAPSED-DAYS   PIC 9(04)     VALUE ZERO.
00076      12  WS-SAVE-ODD-DAYS    PIC S9(04)    VALUE ZERO.
00077      12  SAVE-ODD-DAYS-OVER  PIC 9(03)     VALUE ZERO.
00078      12  WS-DAILY-RATE       PIC S9(3)V99.
00079      12  WST-DAILY-RATE      PIC S9(3)V99999.
00080      12  WS-PY-WORK          PIC S9(8)V99.
00081      12  WS-PY-WK REDEFINES WS-PY-WORK.
00082          16  WS-PY-NUM       PIC S9(7)V99.
00083          16  WS-PY-SIGN      PIC X.
00084      12  WS-DY-WORK          PIC S9(5).
00085      12  WS-DY-WK REDEFINES WS-DY-WORK.
00086          16  WS-DY-NUM       PIC S9(4).
00087          16  WS-DY-SIGN      PIC X.
00088      12  WS-ACCESS.
00089          16  FILLER          PIC XX      VALUE SPACES.
00090          16  WS-BEN-CD       PIC 99.
00091      12  DATE-WORK.
00092          16  FILLER          PIC X(2).
00093          16  NUM-WORK        PIC X(6).
00094      12  WS-SAVE-INPUT.
00095          16  WS-PMTTYPE      PIC X       VALUE SPACE.
00096          16  WS-PAYEE        PIC X(02)   VALUE SPACE.
00097          16  WS-PMTNOTE1     PIC X(40)   VALUE SPACES.
00098          16  WS-PMTNOTE2     PIC X(40)   VALUE SPACES.
00099          16  WS-OFFLINE      PIC X       VALUE SPACE.
00100          16  WS-CHECKNO      PIC X(7)    VALUE SPACES.
00101          16  WS-HOLDTIL      PIC 9(6)    VALUE 0.
00102          16  WS-EDAYS        PIC S9(5)    VALUE +0.
00103          16  WS-EPYAMT       PIC S9(7)V99 VALUE +0.
00104          16  WS-ERESV        PIC S9(7)V99 VALUE 0.
00105          16  WS-EEXPENS      PIC 9(6)V99  VALUE 0.
00106          16  WS-ETYPE        PIC X       VALUE SPACE.
00107          16  WS-CASH         PIC X       VALUE SPACE.
00108          16  WS-GROUPED      PIC X       VALUE SPACE.
00109      12  LAST-ERROR-LINE.
00110          16  WS-FATAL-CTR        PIC ZZ9.
00111          16  FILLER              PIC X(30)
00112              VALUE ' FATAL ERRORS ENCOUNTERED'.
00113          16  WS-FORCE-CTR        PIC ZZ9.
00114          16  FILLER              PIC X(28)
00115              VALUE ' FORCABLE ERRORS ENCOUNTERED'.
00116      12  WS-CHECK-AREA.
00117          16  WS-CHECK-CARR       PIC X.
00118          16  WS-CHECK-NUM        PIC 9(6).
00119      12  WS-CHECK-NUMERIC REDEFINES WS-CHECK-AREA PIC 9(7).
00120      12  OPTION-1-SAVE-AREA.
00121          16  SAVE1-FROM-DAYS-IN-MONTH    PIC 999  VALUE ZEROS.
00122          16  SAVE1-THRU-DAYS-IN-MONTH    PIC 999  VALUE ZEROS.
00123
00124      EJECT
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
00126  01  DFHCOMMAREA             PIC X(450).
00127
00128      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'ELPMNT' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00130
00131      MOVE DFHCOMMAREA       TO CALCULATION-PASS-AREA.
00132
00133      MOVE ZERO              TO CP-RETURN-CODE.
00134
00135 *    SAMPLE DATE RANGES AND DAYS PAID FOR OPTION 1 AND 6
00136
00137 ****************************************************************
00138 ****************************************************************
00139 *                  CALCULATED FROM      PAYMENT      PAYMENT
00140 *   DATES           DATE ROUTINE        OPT  1       OPT  6
00141 * FROM    THRU    MONTHS ODD ELAPSED   DAYS-PAID    DAYS-PAID
00142 *1-01-83  1-15-83    0    15     15        15           15
00143 *1-01-83  1-30-83    0    30     30        30           30
00144 *1-01-83  1-31-83    1     0     31        30           30
00145 *1-01-83  2-01-83    1     1     32        31           31
00146 *1-01-83  2-15-83    1    15     46        45           45
00147 *1-01-83  2-28-83    2     0     59        60           60
00148 *1-01-83  3-01-83    2     1     60        61           61
00149 *1-01-83  3-31-83    3     0     90        90           90
00150 *1-15-83  1-31-83    0    17     17        17           16
00151 *1-15-83  2-01-83    0    18     18        18           17
00152 *1-15-83  2-28-83    1    14     45        44           46
00153 *1-21-83  3-09-83    1    20     48        50           49
00154 *2-05-83  3-06-83    1     2     30        32           32
00155 *2-05-83  3-23-83    1    19     47        49           49
00156 *2-15-83  2-28-83    0    14     14        14           16
00157 *2-15-83  3-01-83    0    15     15        15           17
00158 *2-21-83  3-19-83    0    27     27        27           29
00159 *2-21-83  3-20-83    1     0     28        30           30
00160 *2-21-83  4-10-83    1    18     49        30           50
00161 *2-21-83  5-19-83    2    27     88        87           89
00162 *2-21-83  5-22-83    3     2     91        92           92
00163 *3-21-83  5-19-83    1    30     60        60           59
00164 ****************************************************************
00165      EJECT
00166  0000-CALC-PAYMENT.
00167
00168      MOVE CP-PAID-FROM-DATE      TO DC-BIN-DATE-1
00169      MOVE ' '                    TO DC-OPTION-CODE
00170      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00171      IF NO-CONVERSION-ERROR
00172         MOVE DC-DAYS-IN-MONTH    TO FROM-DAYS-IN-MONTH
00173         MOVE DC-GREG-DATE-CYMD   TO WORK-DATE-FROM
00174      ELSE
00175         MOVE '2'                 TO CP-RETURN-CODE
00176         GO TO 0100-RETURN.
00177
00178      MOVE CP-PAID-THRU-DT        TO DC-BIN-DATE-1
00179      MOVE ' '                    TO DC-OPTION-CODE
00180      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00181      IF NO-CONVERSION-ERROR
00182         MOVE DC-GREG-DATE-CYMD      TO WORK-DATE-THRU
00183         MOVE DC-DAYS-IN-MONTH       TO THRU-DAYS-IN-MONTH
00184      ELSE
00185         MOVE '2'                 TO CP-RETURN-CODE
00186         GO TO 0100-RETURN.
00187
00188      MOVE CP-PAID-FROM-DATE      TO DC-BIN-DATE-1
00189      MOVE -1                     TO DC-ELAPSED-DAYS
00190      MOVE ZEROS                  TO DC-ELAPSED-MONTHS
00191      MOVE '6'                    TO DC-OPTION-CODE
00192      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00193      MOVE DC-DAYS-IN-MONTH       TO SAVE1-FROM-DAYS-IN-MONTH
00194
00195      MOVE DC-BIN-DATE-2          TO DC-BIN-DATE-1
00196      MOVE CP-PAID-THRU-DT        TO DC-BIN-DATE-2
00197      MOVE '1'                    TO DC-OPTION-CODE
00198      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00199
00200      MOVE DC-ELAPSED-MONTHS      TO SAVE-ELAPSED-MONTHS
00201      MOVE DC-ODD-DAYS-OVER       TO SAVE-ODD-DAYS-OVER
00202      MOVE DC-ELAPSED-DAYS        TO SAVE-ELAPSED-DAYS
00203                                     CP-ACTUAL-DAYS
00204
00205 ******************************************************************
00206 *   THE FOLLOWING STATEMENT ADJUSTS THE ODD DAYS OVER BY THE     *
00207 *     DIFFERENCE BETWEEN DAYS IN MONTH (FROM MONTH) AND          *
00208 *     DAYS IN MONTH (ONE MONTH PRIOR TO THRU MONTH)              *
00209 ******************************************************************
00210
00211      IF CP-CLAIM-CALC-METHOD EQUAL '1'  AND
00212         DC-ELAPSED-MONTHS GREATER ZERO AND
00213         DC-ODD-DAYS-OVER NOT = ZEROS   AND
00214         WDF-DAY GREATER THAN WDT-DAY
00215         MOVE CP-PAID-THRU-DT     TO DC-BIN-DATE-1
00216         MOVE ZEROS               TO DC-ELAPSED-DAYS
00217         MOVE -1                  TO DC-ELAPSED-MONTHS
00218         MOVE '6'                 TO DC-OPTION-CODE
00219         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00220         MOVE DC-DAYS-IN-MONTH    TO SAVE1-THRU-DAYS-IN-MONTH
00221         COMPUTE WS-SAVE-ODD-DAYS =
00222          (SAVE1-THRU-DAYS-IN-MONTH - SAVE1-FROM-DAYS-IN-MONTH)
00223         ADD WS-SAVE-ODD-DAYS     TO SAVE-ODD-DAYS-OVER.
00224
00225      MOVE SAVE-ELAPSED-MONTHS    TO DC-ELAPSED-MONTHS.
00226      MOVE SAVE-ODD-DAYS-OVER     TO DC-ODD-DAYS-OVER
00227      MOVE SAVE-ELAPSED-DAYS      TO DC-ELAPSED-DAYS.
00228
00229      IF (CP-COMPANY-ID EQUAL 'FIA' AND
00230         CP-ACCOUNT-NUMBER EQUAL '0000011043')
00231         COMPUTE WS-DAILY-RATE ROUNDED =
00232                     (CP-ORIGINAL-BENEFIT * 13) / 365
00233          COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL
00234                            WS-DAILY-RATE * DC-ELAPSED-DAYS
00235          GO TO 0100-RETURN.
00236
00237      IF CP-CLAIM-CALC-METHOD EQUAL '1'
00238          COMPUTE WST-DAILY-RATE ROUNDED EQUAL
00239                   CP-ORIGINAL-BENEFIT / 30
00240          COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL
00241              (CP-ORIGINAL-BENEFIT * DC-ELAPSED-MONTHS) +
00242              (WST-DAILY-RATE     * DC-ODD-DAYS-OVER)
00243               MOVE WST-DAILY-RATE TO WS-DAILY-RATE
00244          GO TO 0100-RETURN.
00245
00246      IF CP-CLAIM-CALC-METHOD EQUAL '2'
00247          COMPUTE WS-DAILY-RATE ROUNDED =
00248                    (CP-ORIGINAL-BENEFIT * 12) / 365
00249          COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL
00250            (CP-ORIGINAL-BENEFIT * DC-ELAPSED-MONTHS) +
00251              (WS-DAILY-RATE     * DC-ODD-DAYS-OVER)
00252          GO TO 0100-RETURN.
00253
00254      IF CP-CLAIM-CALC-METHOD EQUAL '4'
00255          MOVE CP-ORIGINAL-BENEFIT     TO  WS-AH-BEN-AMT
00256          COMPUTE WST-DAILY-RATE ROUNDED = (WS-AH-BEN-AMT / 30)
00257          MOVE WST-DAILY-RATE TO WS-DAILY-RATE
00258          COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL
00259                            WST-DAILY-RATE * DC-ELAPSED-DAYS
00260          GO TO 0100-RETURN.
00261
00262      IF CP-CLAIM-CALC-METHOD EQUAL '5'
00263          COMPUTE WS-DAILY-RATE ROUNDED =
00264                    (CP-ORIGINAL-BENEFIT * 12) / 365
00265          COMPUTE CP-CLAIM-PAYMENT ROUNDED =
00266                            WS-DAILY-RATE * DC-ELAPSED-DAYS
00267          GO TO 0100-RETURN.
00268
00269      IF CP-CLAIM-CALC-METHOD NOT EQUAL '3'
00270         GO TO 0010-CHECK-PAYMENT.
00271
00272 ******   CALCULATION METHOD  3  ROUTINE
00273      COMPUTE WS-DAILY-RATE =
00274              CP-ORIGINAL-BENEFIT / FROM-DAYS-IN-MONTH.
00275
00276 ******CHECK TO SEE IF WITHIN SAME MONTH
00277      IF (WDF-MONTH = WDT-MONTH) AND (WDF-CCYY = WDT-CCYY)
00278         COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL
00279               SAVE-ELAPSED-DAYS *
00280               (CP-ORIGINAL-BENEFIT / FROM-DAYS-IN-MONTH)
00281          GO TO 0100-RETURN.
00282
00283 ******CHECK TO SEE IF EVEN MONTHS AND NO ODD DAYS
00284      IF SAVE-ELAPSED-MONTHS NOT = ZEROS   AND
00285              SAVE-ODD-DAYS-OVER = ZEROS
00286       COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL
00287             CP-ORIGINAL-BENEFIT * SAVE-ELAPSED-MONTHS
00288       GO TO 0100-RETURN.
00289
00290 ******BUMP THE FROM DATE WITH THE NUMBER OF MONTHS
00291 ******AND FIND THE NUMBER OF DAYS LEFT IN THIS MONTH.
00292      IF SAVE-ELAPSED-MONTHS NOT = ZERO
00293         ADD SAVE-ELAPSED-MONTHS  TO WDF-MONTH
00294         IF WDF-MONTH GREATER THAN 12
00295            ADD 1                 TO WDF-CCYY
00296            SUBTRACT 12         FROM WDF-MONTH.
00297 ******CONVERT THE NEW FROM MONTH TO BINARY
00298      IF SAVE-ELAPSED-MONTHS NOT = ZERO
00299         MOVE WORK-DATE-FROM TO DC-GREG-DATE-CYMD
00300         MOVE 'L' TO DC-OPTION-CODE
00301         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00302         MOVE DC-DAYS-IN-MONTH    TO FROM-DAYS-IN-MONTH.
00303
00304      IF (WDF-MONTH = WDT-MONTH) AND (WDF-CCYY = WDT-CCYY)
00305        COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL
00306                       ((WDT-DAY  -  WDF-DAY)  *
00307              (CP-ORIGINAL-BENEFIT / FROM-DAYS-IN-MONTH)) +
00308                 (CP-ORIGINAL-BENEFIT * SAVE-ELAPSED-MONTHS)
00309         ELSE
00310        COMPUTE FROM-PMT-DAYS = FROM-DAYS-IN-MONTH - WDF-DAY
00311        MOVE WDT-DAY              TO THRU-PMT-DAYS
00312        COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL
00313        (FROM-PMT-DAYS * (CP-ORIGINAL-BENEFIT /
00314                          FROM-DAYS-IN-MONTH))
00315        +
00316        (THRU-PMT-DAYS * (CP-ORIGINAL-BENEFIT /
00317                          THRU-DAYS-IN-MONTH))
00318        +
00319        (CP-ORIGINAL-BENEFIT * SAVE-ELAPSED-MONTHS).
00320      GO TO 0100-RETURN.
00321      EJECT
00322  0010-CHECK-PAYMENT.
00323 ******PAYMENT OPTION 6
00324 ******CHECK TO SEE IF LESS THAN 1 MONTH AND WITHIN SAME MONTH
00325      IF SAVE-ELAPSED-MONTHS GREATER THAN ZERO
00326         IF SAVE-ODD-DAYS-OVER = ZEROS
00327            GO TO 0020-CHECK-TO-DAY
00328         ELSE
00329            IF WDF-DAY GREATER THAN WDT-DAY
00330               IF WDF-MONTH = 02
00331                   PERFORM 0050-CHECK-LEAP-YEAR THRU 0050-EXIT
00332                   ADD 2          TO  SAVE-ODD-DAYS-OVER
00333                   GO TO 0020-CHECK-TO-DAY
00334               ELSE
00335                   IF FROM-DAYS-IN-MONTH = 31 AND WDF-DAY NOT = 31
00336                      SUBTRACT 1 FROM SAVE-ODD-DAYS-OVER
00337                      GO TO 0020-CHECK-TO-DAY
00338                   ELSE
00339                      GO TO 0020-CHECK-TO-DAY
00340            ELSE
00341                GO TO 0020-CHECK-TO-DAY.
00342
00343      IF WDF-MONTH NOT = WDT-MONTH
00344         IF WDF-MONTH = 02
00345            PERFORM 0050-CHECK-LEAP-YEAR THRU 0050-EXIT
00346            IF WDF-DAY = 29
00347               ADD 1              TO SAVE-ODD-DAYS-OVER
00348            ELSE
00349               ADD 2              TO SAVE-ODD-DAYS-OVER
00350         ELSE
00351            IF FROM-DAYS-IN-MONTH = 31 AND WDF-DAY NOT = 31
00352               SUBTRACT 1         FROM SAVE-ODD-DAYS-OVER.
00353
00354  0020-CHECK-TO-DAY.
00355         IF WDT-MONTH = 02 AND SAVE-ODD-DAYS-OVER NOT = 0
00356            IF WDT-DAY = 28
00357               ADD 2              TO SAVE-ODD-DAYS-OVER
00358            ELSE
00359               IF WDT-DAY = 29
00360                  ADD 1           TO SAVE-ODD-DAYS-OVER
00361               ELSE
00362                  NEXT SENTENCE
00363         ELSE
00364            IF WDT-DAY = 31 AND SAVE-ODD-DAYS-OVER NOT = 0
00365               SUBTRACT 1         FROM SAVE-ODD-DAYS-OVER.
00366
00367      COMPUTE WST-DAILY-RATE ROUNDED EQUAL
00368                             CP-ORIGINAL-BENEFIT / 30
00369      MOVE WST-DAILY-RATE          TO WS-DAILY-RATE
00370      COMPUTE CP-CLAIM-PAYMENT ROUNDED EQUAL
00371          (CP-ORIGINAL-BENEFIT * SAVE-ELAPSED-MONTHS) +
00372          (WST-DAILY-RATE     * SAVE-ODD-DAYS-OVER).
00373
00374      GO TO 0100-RETURN.
00375
00376  0050-CHECK-LEAP-YEAR.
00377
00378      IF FROM-DAYS-IN-MONTH = 29
00379         SUBTRACT 1 FROM SAVE-ODD-DAYS-OVER.
00380
00381  0050-EXIT.
00382      EXIT.
00383
00384      EJECT
00385  0100-RETURN.
00386
00387      MOVE CALCULATION-PASS-AREA TO DFHCOMMAREA.
00388
00389      
      * EXEC CICS RETURN
00390 *    END-EXEC.
      *    MOVE '.(                    &   #00001232' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00391
00392      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELPMNT' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00393
00394  9700-LINK-DATE-CONVERT.
00395
00396      
      * EXEC CICS LINK
00397 *         PROGRAM    ('ELDATCV')
00398 *         COMMAREA   (DATE-CONVERSION-DATA)
00399 *         LENGTH     (DC-COMM-LENGTH)
00400 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00001239' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00401
00402  9700-EXIT.
00403      EXIT.
00404      EJECT


       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELPMNT' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELPMNT' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
