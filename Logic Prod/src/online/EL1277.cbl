00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL1277.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 10:00:12.
00007 *                            VMOD=2.005.
00008 *
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *                                                                *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023 *
00024 *REMARKS.    TRANSACTION - EXX7 - CERTIFICATE MAILING DATA.
00025 *
101201******************************************************************
101201*                   C H A N G E   L O G
101201*
101201* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101201*-----------------------------------------------------------------
101201*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101201* EFFECTIVE    NUMBER
101201*-----------------------------------------------------------------
101201* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
080406* 080406    2006051800002  AJRA  ADD POSTCARD MAIL STATUS
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
070313* 070313  CR2013052300001  PEMA  REMOVE SSN FROM MAP.
120513* 120513  IR2013112500001  PEMA  CHANGE EDIT ON ADDRESS
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
100217* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
042221* 042221  IR2021041300001  PEMA  Fix birth date problems.
101201******************************************************************
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL1277 WORKING STORAGE    *'.
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.005 *********'.
00034
00035 *    COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
00036
00037 *    COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
00038
00039  01  WS-DATE-AREA.
00040      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00041      05  SAVE-BIN-DATE               PIC X(2)    VALUE SPACES.
00042
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-DUPKEY                  VALUE +15.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.
00043  01  STANDARD-AREAS.
           12  FILE-ID-ELCNTL      PIC X(8)    VALUE 'ELCNTL'.
           12  ELCNTL-KEY.
               16  ELCNTL-COMPANY-ID   PIC X(3)  VALUE SPACES.
               16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.
               16  ELCNTL-ACCESS.
                   20  FILLER          PIC XX.
                   20  FILLER          PIC XX.
               16  ELCNTL-SEQ          PIC S9(4) VALUE +0 COMP.
00044      12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.
00045      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00046      12  MAP-NAME                    PIC X(8)    VALUE 'EL127G'.
00047      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL1277S'.
00048      12  SCREEN-NUMBER               PIC X(4)    VALUE '127G'.
00049      12  TRANS-ID                    PIC X(4)    VALUE 'EXX7'.
00050      12  THIS-PGM                    PIC X(8)    VALUE 'EL1277'.
00051      12  PGM-NAME                    PIC X(8).
00052      12  TIME-IN                     PIC S9(7).
00053      12  TIME-OUT-R  REDEFINES TIME-IN.
00054          16  FILLER                  PIC X.
00055          16  TIME-OUT                PIC 99V99.
00056          16  FILLER                  PIC X(2).
00057      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00058      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00059      12  XCTL-126                    PIC X(8)    VALUE 'EL126'.
00060      12  XCTL-1272                   PIC X(8)    VALUE 'EL1272'.
00061      12  XCTL-1273                   PIC X(8)    VALUE 'EL1273'.
00062      12  XCTL-1274                   PIC X(8)    VALUE 'EL1274'.
00063      12  XCTL-1275                   PIC X(8)    VALUE 'EL1275'.
00064      12  XCTL-1276                   PIC X(8)    VALUE 'EL1276'.
00065      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00066      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00067      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00068      12  ERMAIL-ID                   PIC X(8)    VALUE 'ERMAIL'.
00069      12  ELCERT-ID                   PIC X(8)    VALUE 'ELCERT'.
00070      12  WS-RECORD-LENGTHS   COMP.
CIDMOD*        16  WS-ERMAIL-RECORD-LENGTH  PIC S9(4)  VALUE +250.
CIDMOD         16  WS-ERMAIL-RECORD-LENGTH  PIC S9(4)  VALUE +374.
00072          16  WS-ELCERT-RECORD-LENGTH  PIC S9(4)  VALUE +450.
00073
00074      12  DEEDIT-FIELD                PIC X(15).
00075      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
00076
00077      12  WS-BIRTHDT                  PIC XX.
090408     12  WS-JNT-BIRTHDT              PIC XX.
00078
00079      12  RETURN-FROM                 PIC X(8).
00080      12  QID.
00081          16  QID-TERM                PIC X(4).
00082          16  FILLER                  PIC X(4)    VALUE '127G'.
00083      12  WS-RECORD-FOUND-SW          PIC  X      VALUE SPACE.
00084          88  RECORD-FOUND                        VALUE 'Y'.
00085          88  RECORD-NOT-FOUND                    VALUE 'N'.
00086      12  WS-DUPREC-SW                PIC  X      VALUE SPACE.
00087          88  DUPLICATE-RECORD-FOUND              VALUE 'Y'.
00088      12  W-ZIP-TEST                  PIC  X(1)   VALUE SPACE.
00089          88  W-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00090
00091      EJECT
00092      12  ERROR-MESSAGES.
00093          16  ER-0000                 PIC  X(4)   VALUE '0000'.
00094          16  ER-0004                 PIC  X(4)   VALUE '0004'.
00095          16  ER-0008                 PIC  X(4)   VALUE '0008'.
00096          16  ER-0023                 PIC  X(4)   VALUE '0023'.
00097          16  ER-0029                 PIC  X(4)   VALUE '0029'.
00098          16  ER-0070                 PIC  X(4)   VALUE '0070'.
00099          16  ER-0142                 PIC  X(4)   VALUE '0142'.
00100          16  ER-0219                 PIC  X(4)   VALUE '0219'.
00101          16  ER-0220                 PIC  X(4)   VALUE '0220'.
100217         16  er-2050                 pic  x(4)   value '2050'.
00102          16  ER-2187                 PIC  X(4)   VALUE '2187'.
00103          16  ER-2199                 PIC  X(4)   VALUE '2199'.
               16  ER-2209                 PIC  X(4)   VALUE '2209'.
100217         16  er-3061                 pic  x(4)   value '3061'.
100217         16  er-3062                 pic  x(4)   value '3062'.
100217         16  er-3063                 pic  x(4)   value '3063'.
00104          16  ER-7049                 PIC  X(4)   VALUE '7049'.
00105          16  ER-7428                 PIC  X(4)   VALUE '7428'.
00106
00107  01  WS-PHONE                        PIC 9(10)   VALUE ZEROS.
00108  01  WS-PHONE-R REDEFINES WS-PHONE.
00109      12  WS-PHONE-AC                 PIC 999.
00110      12  WS-PHONE-EXT                PIC 999.
00111      12  WS-PHONE-NO                 PIC 9999.
00112  01  EDITED-PHONE-NO                 PIC X(12)   VALUE ZEROS.
00113  01  EDITED-PHONE-NO-R REDEFINES EDITED-PHONE-NO.
00114      12  ED-PHONE-AC                 PIC 999.
00115      12  ED-PHONE-DASH1              PIC X.
00116      12  ED-PHONE-EXT                PIC 999.
00117      12  ED-PHONE-DASH2              PIC X.
00118      12  ED-PHONE-NO                 PIC 9999.
00119
00120
00121  01  WS-CM-CONTROL-PRIMARY.
00122      05  WS-CM-COMPANY-CD            PIC  X.
00123      05  WS-CM-CARRIER               PIC  X.
00124      05  WS-CM-GROUPING              PIC  X(6).
00125      05  WS-CM-STATE                 PIC  XX.
00126      05  WS-CM-ACCOUNT               PIC  X(10).
00127      05  WS-CM-CERT-EFF-DT           PIC  XX.
00128      05  WS-CM-CERT-NO.
00129          10  WS-CM-CERT-PRIME        PIC  X(10).
00130          10  WS-CM-CERT-SFX          PIC  X.
00131
100217 01  zipcd-pass-area-len         pic s9(4) comp value +67.
100217 01  zipcd-pass-area.
100217     03  PA-zip                  PIC X(5).
100217     03  PA-ErrorCode-zip        PIC X(10).
100217     03  PA-city                 PIC x(50).
100217     03  PA-state                PIC Xx.
080406 01  WS-MAILED-AREA.
080406     05  WS-SUB                      PIC S9(5) COMP VALUE +0.
080406     05  WS-MAILED-BLD-STATUS.
080406         10  WS-MAILED-STAT          PIC X(5).
080406         10  FILLER                  PIC X(1) VALUE SPACE.
080406         10  WS-MAILED-DATE          PIC X(8).
080406
080406     05  WS-MAILED-DATA OCCURS 7 TIMES.
080406         10  WS-MAILED-TYPE          PIC X(3).
080406         10  WS-MAILED-STATUS        PIC X(14).
00132
00134 *                                COPY ELCDATE.
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
00138 *                                COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
00141 *                                COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
00144 *                                COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00147 *                                COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
00148      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00149          16  FILLER                  PIC X(317).
00150          16  PI-PEND-SW              PIC X.
00151          16  FILLER                  PIC X(322).
00152
00153      EJECT
00154
00155 *    COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  '�'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00156  01  FILLER    REDEFINES DFHAID.
00157      12  FILLER                      PIC X(8).
00158      12  PF-VALUES                   PIC X       OCCURS 24 TIMES.
00159
00160      EJECT
00161 *    COPY EL1277S.
       01  EL127GI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  GDATEL PIC S9(0004) COMP.
           05  GDATEF PIC  X(0001).
           05  FILLER REDEFINES GDATEF.
               10  GDATEA PIC  X(0001).
           05  GDATEI PIC  X(0008).
      *    -------------------------------
           05  GTIMEL PIC S9(0004) COMP.
           05  GTIMEF PIC  X(0001).
           05  FILLER REDEFINES GTIMEF.
               10  GTIMEA PIC  X(0001).
           05  GTIMEI PIC  999V99.
      *    -------------------------------
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  GMAINTL PIC S9(0004) COMP.
           05  GMAINTF PIC  X(0001).
           05  FILLER REDEFINES GMAINTF.
               10  GMAINTA PIC  X(0001).
           05  GMAINTI PIC  X(0001).
      *    -------------------------------
           05  GCERTNOL PIC S9(0004) COMP.
           05  GCERTNOF PIC  X(0001).
           05  FILLER REDEFINES GCERTNOF.
               10  GCERTNOA PIC  X(0001).
           05  GCERTNOI PIC  X(0010).
      *    -------------------------------
           05  GCRTSFXL PIC S9(0004) COMP.
           05  GCRTSFXF PIC  X(0001).
           05  FILLER REDEFINES GCRTSFXF.
               10  GCRTSFXA PIC  X(0001).
           05  GCRTSFXI PIC  X(0001).
      *    -------------------------------
           05  GACCTNOL PIC S9(0004) COMP.
           05  GACCTNOF PIC  X(0001).
           05  FILLER REDEFINES GACCTNOF.
               10  GACCTNOA PIC  X(0001).
           05  GACCTNOI PIC  X(0010).
      *    -------------------------------
           05  GSTATEL PIC S9(0004) COMP.
           05  GSTATEF PIC  X(0001).
           05  FILLER REDEFINES GSTATEF.
               10  GSTATEA PIC  X(0001).
           05  GSTATEI PIC  X(0002).
      *    -------------------------------
           05  GCARIERL PIC S9(0004) COMP.
           05  GCARIERF PIC  X(0001).
           05  FILLER REDEFINES GCARIERF.
               10  GCARIERA PIC  X(0001).
           05  GCARIERI PIC  X(0001).
      *    -------------------------------
           05  GGROUPL PIC S9(0004) COMP.
           05  GGROUPF PIC  X(0001).
           05  FILLER REDEFINES GGROUPF.
               10  GGROUPA PIC  X(0001).
           05  GGROUPI PIC  X(0006).
      *    -------------------------------
           05  GEFFDTL PIC S9(0004) COMP.
           05  GEFFDTF PIC  X(0001).
           05  FILLER REDEFINES GEFFDTF.
               10  GEFFDTA PIC  X(0001).
           05  GEFFDTI PIC  X(0008).
      *    -------------------------------
           05  GSTATUSL PIC S9(0004) COMP.
           05  GSTATUSF PIC  X(0001).
           05  FILLER REDEFINES GSTATUSF.
               10  GSTATUSA PIC  X(0001).
           05  GSTATUSI PIC  X(0007).
      *    -------------------------------
           05  GADDBYL PIC S9(0004) COMP.
           05  GADDBYF PIC  X(0001).
           05  FILLER REDEFINES GADDBYF.
               10  GADDBYA PIC  X(0001).
           05  GADDBYI PIC  X(0004).
      *    -------------------------------
           05  GADDDTL PIC S9(0004) COMP.
           05  GADDDTF PIC  X(0001).
           05  FILLER REDEFINES GADDDTF.
               10  GADDDTA PIC  X(0001).
           05  GADDDTI PIC  X(0008).
      *    -------------------------------
           05  GLSTUSRL PIC S9(0004) COMP.
           05  GLSTUSRF PIC  X(0001).
           05  FILLER REDEFINES GLSTUSRF.
               10  GLSTUSRA PIC  X(0001).
           05  GLSTUSRI PIC  X(0004).
      *    -------------------------------
           05  GLSTDTL PIC S9(0004) COMP.
           05  GLSTDTF PIC  X(0001).
           05  FILLER REDEFINES GLSTDTF.
               10  GLSTDTA PIC  X(0001).
           05  GLSTDTI PIC  X(0008).
      *    -------------------------------
           05  GLSTIMEL PIC S9(0004) COMP.
           05  GLSTIMEF PIC  X(0001).
           05  FILLER REDEFINES GLSTIMEF.
               10  GLSTIMEA PIC  X(0001).
           05  GLSTIMEI PIC  999V99.
      *    -------------------------------
           05  GCODE1L PIC S9(0004) COMP.
           05  GCODE1F PIC  X(0001).
           05  FILLER REDEFINES GCODE1F.
               10  GCODE1A PIC  X(0001).
           05  GCODE1I PIC  X(0002).
      *    -------------------------------
           05  GCODE2L PIC S9(0004) COMP.
           05  GCODE2F PIC  X(0001).
           05  FILLER REDEFINES GCODE2F.
               10  GCODE2A PIC  X(0001).
           05  GCODE2I PIC  X(0002).
      *    -------------------------------
           05  GCODE3L PIC S9(0004) COMP.
           05  GCODE3F PIC  X(0001).
           05  FILLER REDEFINES GCODE3F.
               10  GCODE3A PIC  X(0001).
           05  GCODE3I PIC  X(0002).
      *    -------------------------------
           05  GCODE4L PIC S9(0004) COMP.
           05  GCODE4F PIC  X(0001).
           05  FILLER REDEFINES GCODE4F.
               10  GCODE4A PIC  X(0001).
           05  GCODE4I PIC  X(0002).
      *    -------------------------------
           05  GCODE5L PIC S9(0004) COMP.
           05  GCODE5F PIC  X(0001).
           05  FILLER REDEFINES GCODE5F.
               10  GCODE5A PIC  X(0001).
           05  GCODE5I PIC  X(0002).
      *    -------------------------------
           05  GLNAMEL PIC S9(0004) COMP.
           05  GLNAMEF PIC  X(0001).
           05  FILLER REDEFINES GLNAMEF.
               10  GLNAMEA PIC  X(0001).
           05  GLNAMEI PIC  X(0015).
      *    -------------------------------
           05  GFNAMEL PIC S9(0004) COMP.
           05  GFNAMEF PIC  X(0001).
           05  FILLER REDEFINES GFNAMEF.
               10  GFNAMEA PIC  X(0001).
           05  GFNAMEI PIC  X(0010).
      *    -------------------------------
           05  GINITL PIC S9(0004) COMP.
           05  GINITF PIC  X(0001).
           05  FILLER REDEFINES GINITF.
               10  GINITA PIC  X(0001).
           05  GINITI PIC  X(0001).
      *    -------------------------------
           05  GADDR1L PIC S9(0004) COMP.
           05  GADDR1F PIC  X(0001).
           05  FILLER REDEFINES GADDR1F.
               10  GADDR1A PIC  X(0001).
           05  GADDR1I PIC  X(0030).
      *    -------------------------------
           05  GADDR2L PIC S9(0004) COMP.
           05  GADDR2F PIC  X(0001).
           05  FILLER REDEFINES GADDR2F.
               10  GADDR2A PIC  X(0001).
           05  GADDR2I PIC  X(0030).
      *    -------------------------------
           05  GCITYL PIC S9(0004) COMP.
           05  GCITYF PIC  X(0001).
           05  FILLER REDEFINES GCITYF.
               10  GCITYA PIC  X(0001).
           05  GCITYI PIC  X(0028).
      *    -------------------------------
           05  GISTATEL PIC S9(0004) COMP.
           05  GISTATEF PIC  X(0001).
           05  FILLER REDEFINES GISTATEF.
               10  GISTATEA PIC  X(0001).
           05  GISTATEI PIC  X(0002).
      *    -------------------------------
           05  GZIP1L PIC S9(0004) COMP.
           05  GZIP1F PIC  X(0001).
           05  FILLER REDEFINES GZIP1F.
               10  GZIP1A PIC  X(0001).
           05  GZIP1I PIC  X(0005).
      *    -------------------------------
           05  GZIP2L PIC S9(0004) COMP.
           05  GZIP2F PIC  X(0001).
           05  FILLER REDEFINES GZIP2F.
               10  GZIP2A PIC  X(0001).
           05  GZIP2I PIC  X(0004).
      *    -------------------------------
           05  GPHONEL PIC S9(0004) COMP.
           05  GPHONEF PIC  X(0001).
           05  FILLER REDEFINES GPHONEF.
               10  GPHONEA PIC  X(0001).
           05  GPHONEI PIC  X(0012).
      *    -------------------------------
           05  GSSNOL PIC S9(0004) COMP.
           05  GSSNOF PIC  X(0001).
           05  FILLER REDEFINES GSSNOF.
               10  GSSNOA PIC  X(0001).
           05  GSSNOI PIC  X(0011).
      *    -------------------------------
           05  GAGEL PIC S9(0004) COMP.
           05  GAGEF PIC  X(0001).
           05  FILLER REDEFINES GAGEF.
               10  GAGEA PIC  X(0001).
           05  GAGEI PIC  X(0002).
      *    -------------------------------
           05  GBDTL PIC S9(0004) COMP.
           05  GBDTF PIC  X(0001).
           05  FILLER REDEFINES GBDTF.
               10  GBDTA PIC  X(0001).
           05  GBDTI PIC  X(0008).
      *    -------------------------------
           05  GSEXL PIC S9(0004) COMP.
           05  GSEXF PIC  X(0001).
           05  FILLER REDEFINES GSEXF.
               10  GSEXA PIC  X(0001).
           05  GSEXI PIC  X(0001).
      *    -------------------------------
           05  GJDOBL PIC S9(0004) COMP.
           05  GJDOBF PIC  X(0001).
           05  FILLER REDEFINES GJDOBF.
               10  GJDOBA PIC  X(0001).
           05  GJDOBI PIC  X(0008).
      *    -------------------------------
           05  GBNAMEL PIC S9(0004) COMP.
           05  GBNAMEF PIC  X(0001).
           05  FILLER REDEFINES GBNAMEF.
               10  GBNAMEA PIC  X(0001).
           05  GBNAMEI PIC  X(0025).
      *    -------------------------------
           05  GBADD1L PIC S9(0004) COMP.
           05  GBADD1F PIC  X(0001).
           05  FILLER REDEFINES GBADD1F.
               10  GBADD1A PIC  X(0001).
           05  GBADD1I PIC  X(0030).
      *    -------------------------------
           05  GBADD2L PIC S9(0004) COMP.
           05  GBADD2F PIC  X(0001).
           05  FILLER REDEFINES GBADD2F.
               10  GBADD2A PIC  X(0001).
           05  GBADD2I PIC  X(0030).
      *    -------------------------------
           05  GBCITYL PIC S9(0004) COMP.
           05  GBCITYF PIC  X(0001).
           05  FILLER REDEFINES GBCITYF.
               10  GBCITYA PIC  X(0001).
           05  GBCITYI PIC  X(0028).
      *    -------------------------------
           05  GBSTATEL PIC S9(0004) COMP.
           05  GBSTATEF PIC  X(0001).
           05  FILLER REDEFINES GBSTATEF.
               10  GBSTATEA PIC  X(0001).
           05  GBSTATEI PIC  X(0002).
      *    -------------------------------
           05  GBZIP1L PIC S9(0004) COMP.
           05  GBZIP1F PIC  X(0001).
           05  FILLER REDEFINES GBZIP1F.
               10  GBZIP1A PIC  X(0001).
           05  GBZIP1I PIC  X(0005).
      *    -------------------------------
           05  GBZIP2L PIC S9(0004) COMP.
           05  GBZIP2F PIC  X(0001).
           05  FILLER REDEFINES GBZIP2F.
               10  GBZIP2A PIC  X(0001).
           05  GBZIP2I PIC  X(0004).
      *    -------------------------------
           05  GTYPE1L PIC S9(0004) COMP.
           05  GTYPE1F PIC  X(0001).
           05  FILLER REDEFINES GTYPE1F.
               10  GTYPE1A PIC  X(0001).
           05  GTYPE1I PIC  X(0003).
      *    -------------------------------
           05  GSTAT1L PIC S9(0004) COMP.
           05  GSTAT1F PIC  X(0001).
           05  FILLER REDEFINES GSTAT1F.
               10  GSTAT1A PIC  X(0001).
           05  GSTAT1I PIC  X(0014).
      *    -------------------------------
           05  GTYPE2L PIC S9(0004) COMP.
           05  GTYPE2F PIC  X(0001).
           05  FILLER REDEFINES GTYPE2F.
               10  GTYPE2A PIC  X(0001).
           05  GTYPE2I PIC  X(0003).
      *    -------------------------------
           05  GSTAT2L PIC S9(0004) COMP.
           05  GSTAT2F PIC  X(0001).
           05  FILLER REDEFINES GSTAT2F.
               10  GSTAT2A PIC  X(0001).
           05  GSTAT2I PIC  X(0014).
      *    -------------------------------
           05  GTYPE3L PIC S9(0004) COMP.
           05  GTYPE3F PIC  X(0001).
           05  FILLER REDEFINES GTYPE3F.
               10  GTYPE3A PIC  X(0001).
           05  GTYPE3I PIC  X(0003).
      *    -------------------------------
           05  GSTAT3L PIC S9(0004) COMP.
           05  GSTAT3F PIC  X(0001).
           05  FILLER REDEFINES GSTAT3F.
               10  GSTAT3A PIC  X(0001).
           05  GSTAT3I PIC  X(0014).
      *    -------------------------------
           05  GTYPE4L PIC S9(0004) COMP.
           05  GTYPE4F PIC  X(0001).
           05  FILLER REDEFINES GTYPE4F.
               10  GTYPE4A PIC  X(0001).
           05  GTYPE4I PIC  X(0003).
      *    -------------------------------
           05  GSTAT4L PIC S9(0004) COMP.
           05  GSTAT4F PIC  X(0001).
           05  FILLER REDEFINES GSTAT4F.
               10  GSTAT4A PIC  X(0001).
           05  GSTAT4I PIC  X(0014).
      *    -------------------------------
           05  GTYPE5L PIC S9(0004) COMP.
           05  GTYPE5F PIC  X(0001).
           05  FILLER REDEFINES GTYPE5F.
               10  GTYPE5A PIC  X(0001).
           05  GTYPE5I PIC  X(0003).
      *    -------------------------------
           05  GSTAT5L PIC S9(0004) COMP.
           05  GSTAT5F PIC  X(0001).
           05  FILLER REDEFINES GSTAT5F.
               10  GSTAT5A PIC  X(0001).
           05  GSTAT5I PIC  X(0014).
      *    -------------------------------
           05  GTYPE6L PIC S9(0004) COMP.
           05  GTYPE6F PIC  X(0001).
           05  FILLER REDEFINES GTYPE6F.
               10  GTYPE6A PIC  X(0001).
           05  GTYPE6I PIC  X(0003).
      *    -------------------------------
           05  GSTAT6L PIC S9(0004) COMP.
           05  GSTAT6F PIC  X(0001).
           05  FILLER REDEFINES GSTAT6F.
               10  GSTAT6A PIC  X(0001).
           05  GSTAT6I PIC  X(0014).
      *    -------------------------------
           05  GTYPE7L PIC S9(0004) COMP.
           05  GTYPE7F PIC  X(0001).
           05  FILLER REDEFINES GTYPE7F.
               10  GTYPE7A PIC  X(0001).
           05  GTYPE7I PIC  X(0003).
      *    -------------------------------
           05  GSTAT7L PIC S9(0004) COMP.
           05  GSTAT7F PIC  X(0001).
           05  FILLER REDEFINES GSTAT7F.
               10  GSTAT7A PIC  X(0001).
           05  GSTAT7I PIC  X(0014).
      *    -------------------------------
           05  GERMSG1L PIC S9(0004) COMP.
           05  GERMSG1F PIC  X(0001).
           05  FILLER REDEFINES GERMSG1F.
               10  GERMSG1A PIC  X(0001).
           05  GERMSG1I PIC  X(0079).
      *    -------------------------------
           05  GERMSG2L PIC S9(0004) COMP.
           05  GERMSG2F PIC  X(0001).
           05  FILLER REDEFINES GERMSG2F.
               10  GERMSG2A PIC  X(0001).
           05  GERMSG2I PIC  X(0079).
      *    -------------------------------
           05  GPFKEYL PIC S9(0004) COMP.
           05  GPFKEYF PIC  X(0001).
           05  FILLER REDEFINES GPFKEYF.
               10  GPFKEYA PIC  X(0001).
           05  GPFKEYI PIC  99.
       01  EL127GO REDEFINES EL127GI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GCERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GCRTSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GACCTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GCARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GEFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSTATUSO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GADDBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GADDDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GLSTUSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GLSTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GLSTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GCODE1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GCODE2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GCODE3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GCODE4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GCODE5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GFNAMEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GADDR2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GCITYO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GISTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GZIP1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GZIP2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GPHONEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSSNOO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GBDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GJDOBO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GBNAMEO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GBADD1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GBADD2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GBCITYO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GBSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GBZIP1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GBZIP2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GTYPE1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSTAT1O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GTYPE2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSTAT2O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GTYPE3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSTAT3O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GTYPE4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSTAT4O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GTYPE5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSTAT5O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GTYPE6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSTAT6O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GTYPE7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GSTAT7O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GERMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GERMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GPFKEYO PIC  99.
      *    -------------------------------
00162
00163      EJECT
00164
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
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
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
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
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00166  01  DFHCOMMAREA                     PIC X(1024).
00167
      *                                COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
00169
00170 *    COPY ERCMAIL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCMAIL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
00013 *   ALTERNATE PATH    = NOT USED                                 *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
080406******************************************************************
080406*                   C H A N G E   L O G
080406*
080406* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080406*-----------------------------------------------------------------
080406*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080406* EFFECTIVE    NUMBER
080406*-----------------------------------------------------------------
080406* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
111108* 111108                   PEMA  ADD CRED BENE ADDR2
00017 ******************************************************************
00018
00019  01  MAILING-DATA.
00020      12  MA-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'MA'.
00022
00023      12  MA-CONTROL-PRIMARY.
00024          16  MA-COMPANY-CD                 PIC X.
00025          16  MA-CARRIER                    PIC X.
00026          16  MA-GROUPING.
00027              20  MA-GROUPING-PREFIX        PIC XXX.
00028              20  MA-GROUPING-PRIME         PIC XXX.
00029          16  MA-STATE                      PIC XX.
00030          16  MA-ACCOUNT.
00031              20  MA-ACCOUNT-PREFIX         PIC X(4).
00032              20  MA-ACCOUNT-PRIME          PIC X(6).
00033          16  MA-CERT-EFF-DT                PIC XX.
00034          16  MA-CERT-NO.
00035              20  MA-CERT-PRIME             PIC X(10).
00036              20  MA-CERT-SFX               PIC X.
00037
00038      12  FILLER                            PIC XX.
00039
00040      12  MA-ACCESS-CONTROL.
00041          16  MA-SOURCE-SYSTEM              PIC XX.
00042              88  MA-FROM-CREDIT                VALUE 'CR'.
00043              88  MA-FROM-VSI                   VALUE 'VS'.
00044              88  MA-FROM-WARRANTY              VALUE 'WA'.
00045              88  MA-FROM-OTHER                 VALUE 'OT'.
00046          16  MA-RECORD-ADD-DT              PIC XX.
00047          16  MA-RECORD-ADDED-BY            PIC XXXX.
00048          16  MA-LAST-MAINT-DT              PIC XX.
00049          16  MA-LAST-MAINT-BY              PIC XXXX.
00050          16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00051
00052      12  MA-PROFILE-INFO.
00053          16  MA-QUALIFY-CODE-1             PIC XX.
00054          16  MA-QUALIFY-CODE-2             PIC XX.
00055          16  MA-QUALIFY-CODE-3             PIC XX.
00056          16  MA-QUALIFY-CODE-4             PIC XX.
00057          16  MA-QUALIFY-CODE-5             PIC XX.
00058
00059          16  MA-INSURED-LAST-NAME          PIC X(15).
00060          16  MA-INSURED-FIRST-NAME         PIC X(10).
00061          16  MA-INSURED-MIDDLE-INIT        PIC X.
00062          16  MA-INSURED-ISSUE-AGE          PIC 99.
00063          16  MA-INSURED-BIRTH-DT           PIC XX.
00064          16  MA-INSURED-SEX                PIC X.
00065              88  MA-SEX-MALE                   VALUE 'M'.
00066              88  MA-SEX-FEMALE                 VALUE 'F'.
00067          16  MA-INSURED-SOC-SEC-NO         PIC X(11).
00068
080406         16  MA-ADDRESS-CORRECTED          PIC X.
081108         16  MA-JOINT-BIRTH-DT             PIC XX.
00069 *        16  FILLER                        PIC X(12).
00070
00071          16  MA-ADDRESS-LINE-1             PIC X(30).
00072          16  MA-ADDRESS-LINE-2             PIC X(30).
00073          16  MA-CITY-STATE.
                   20  MA-CITY                   PIC X(28).
                   20  MA-ADDR-STATE             PIC XX.
00074          16  MA-ZIP.
00075              20  MA-ZIP-CODE.
00076                  24  MA-ZIP-CODE-1ST       PIC X(1).
00077                      88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00078                  24  FILLER                PIC X(4).
00079              20  MA-ZIP-PLUS4              PIC X(4).
00080          16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
00081              20  MA-CAN-POSTAL-CODE-1      PIC X(3).
00082              20  MA-CAN-POSTAL-CODE-2      PIC X(3).
00083              20  FILLER                    PIC X(3).
00084
00085          16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
00086
               16  FILLER                        PIC XXX.
00087 *        16  FILLER                        PIC X(10).
00088
           12  MA-CRED-BENE-INFO.
CIDMOD         16  MA-CRED-BENE-NAME                 PIC X(25).
CIDMOD         16  MA-CRED-BENE-ADDR                 PIC X(30).
               16  MA-CRED-BENE-ADDR2                PIC X(30).
CIDMOD         16  MA-CRED-BENE-CTYST.
                   20  MA-CRED-BENE-CITY             PIC X(28).
                   20  MA-CRED-BENE-STATE            PIC XX.
CIDMOD         16  MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-ZIP-CODE.
CIDMOD                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
CIDMOD                     88  MA-CB-CANADIAN-POST-CODE
                                                 VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                    PIC X(4).
CIDMOD             20  MA-CB-ZIP-PLUS4               PIC X(4).
CIDMOD         16  MA-CB-CANADIAN-POSTAL-CODE
                                  REDEFINES MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
CIDMOD             20  FILLER                        PIC X(3).
080406     12  MA-POST-CARD-MAIL-DATA.
080406         16  MA-MAIL-DATA OCCURS 7.
080406             20  MA-MAIL-TYPE              PIC X.
080406                 88  MA-12MO-MAILING           VALUE '1'.
080406                 88  MA-EXP-MAILING            VALUE '2'.
080406             20  MA-MAIL-STATUS            PIC X.
080406                 88  MA-MAIL-ST-MAILED         VALUE '1'.
080406                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  MA-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC XX.
080406*    12  FILLER                            PIC X(30).
00090 ******************************************************************
00171
00172      EJECT
00173
00174 *    COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
010716* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
090314         16  cm-temp-epiq                  pic xx.
090314             88  EPIQ-CLASS                  value 'EQ'.
090314*        16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
010716         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
011410         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
011410                                           PIC S9(5)V99  COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
062017         16  CM-REF-INTERFACE-SW           PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
062017     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
032612     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
032612     12  FILLER REDEFINES CM-USER-RESERVED.
032612         16  CM-AH-CLASS-CD                PIC XX.
032612         16  F                             PIC X.
00286 ******************************************************************
00175
00176      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE
                                MAILING-DATA CERTIFICATE-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1277' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00178
00179      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00180      MOVE '5'                    TO  DC-OPTION-CODE.
00181      PERFORM 9700-DATE-LINK.
00182      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00183      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00184
00185      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00186      MOVE 1                      TO  EMI-NUMBER-OF-LINES.
00187      MOVE EIBTRMID               TO  QID-TERM.
00188
00189      IF EIBCALEN = 0
00190          GO TO 8800-UNAUTHORIZED-ACCESS.
00191
00192      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00193          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00194              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00195              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00196              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00197              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00198              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00199              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00200              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00201              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00202          ELSE
00203              MOVE PI-CALLING-PROGRAM   TO  RETURN-FROM
00204              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00205              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00206              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00207              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00208              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00209              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00210              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00211              MOVE SPACES               TO  PI-SAVED-PROGRAM-6.
00212
00213      IF EIBAID = DFHCLEAR
00214          GO TO 9400-CLEAR.
00215
00216      IF PI-PROCESSOR-ID = 'LGXX'
00217          NEXT SENTENCE
00218      ELSE
00219          
      * EXEC CICS READQ TS
00220 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00221 *            INTO    (SECURITY-CONTROL)
00222 *            LENGTH  (SC-COMM-LENGTH)
00223 *            ITEM    (SC-ITEM)
00224 *        END-EXEC
      *    MOVE '*$II   L              ''   #00003639' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00225          MOVE SC-CREDIT-DISPLAY (33)   TO  PI-DISPLAY-CAP
00226          MOVE SC-CREDIT-UPDATE  (33)   TO  PI-MODIFY-CAP
00227          IF NOT DISPLAY-CAP
00228              MOVE 'READ'               TO  SM-READ
00229              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00230              MOVE ER-0070              TO  EMI-ERROR
00231              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00232              GO TO 8100-SEND-INITIAL-MAP.
00233
00234      IF  EIBTRNID NOT = TRANS-ID
00235          MOVE LOW-VALUES         TO  EL127GI
00236          PERFORM 7000-FORMAT-SCREEN
00237          GO TO 8100-SEND-INITIAL-MAP.
00238
00239      IF NOT MODIFY-CAP
00240          MOVE 'UPDATE'            TO  SM-READ
00241          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00242          MOVE ER-0070             TO  EMI-ERROR
00243          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00244          GO TO 8100-SEND-INITIAL-MAP.
00245
00246      
      * EXEC CICS    HANDLE    CONDITION
00247 *         PGMIDERR          (9600-PGMID-ERROR)
00248 *         ERROR             (9990-ABEND)
00249 *         END-EXEC.
      *    MOVE '"$L.                  ! " #00003666' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033363636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00250
00251      EJECT
00252
00253  0200-RECEIVE.
00254      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00255          MOVE ER-0008            TO  EMI-ERROR
00256          PERFORM 9900-ERROR-FORMAT
00257          MOVE -1                 TO  GMAINTL
00258          GO TO 8200-SEND-DATAONLY.
00259
00260      
      * EXEC CICS RECEIVE
00261 *        MAP      (MAP-NAME)
00262 *        MAPSET   (MAPSET-NAME)
00263 *        INTO     (EL127GI)
00264 *    END-EXEC.
           MOVE LENGTH OF
            EL127GI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003680' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127GI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00265
00266      IF  GPFKEYL = 0
00267          GO TO 0300-CHECK-PFKEYS.
00268
00269      IF  EIBAID NOT = DFHENTER
00270          MOVE ER-0004            TO  EMI-ERROR
00271          GO TO 0320-INPUT-ERROR.
00272
00273      IF  GPFKEYI NUMERIC
00274          IF  GPFKEYI = '12' OR '23' OR '24'
00275              MOVE PF-VALUES (GPFKEYI) TO  EIBAID
00276          ELSE
00277              MOVE ER-0029             TO  EMI-ERROR
00278              GO TO 0320-INPUT-ERROR.
00279
00280  0300-CHECK-PFKEYS.
00281      IF EIBAID = DFHPF23
00282          GO TO 8810-PF23.
00283
00284      IF EIBAID = DFHPF24
00285          GO TO 9200-RETURN-MAIN-MENU.
00286
00287      IF EIBAID = DFHPF12
00288          GO TO 9500-PF12.
00289
100217     IF EIBAID = DFHENTER or dfhpf7
00291          GO TO 400-EDIT-INPUT-DATA.
00292
00293      MOVE ER-0029                TO  EMI-ERROR.
00294
00295  0320-INPUT-ERROR.
00296      PERFORM 9900-ERROR-FORMAT.
00297      MOVE AL-UNBON               TO  GPFKEYA.
00298      IF GPFKEYL = 0
00299          MOVE -1                 TO  GMAINTL
00300      ELSE
00301          MOVE -1                 TO  GPFKEYL.
00302      GO TO 8200-SEND-DATAONLY.
00303
00304      EJECT
00305
00306  400-EDIT-INPUT-DATA.
00307      IF GMAINTI = 'A' OR 'C' OR 'S'
00308          NEXT SENTENCE
00309      ELSE
00310          MOVE ER-0023            TO  EMI-ERROR
00311          MOVE -1                 TO  GMAINTL
00312          MOVE AL-UABON           TO  GMAINTA
00313          PERFORM 9900-ERROR-FORMAT
00314          GO TO 8200-SEND-DATAONLY.
00315
00316  410-CHECK-ERRORS.
00317      IF EMI-ERROR = ZEROS
00318          NEXT SENTENCE
00319      ELSE
00320          GO TO 8200-SEND-DATAONLY.
00321
00322      IF GMAINTI = 'A'
00323         GO TO 1000-ADD-RECORD.
00324
00325      IF GMAINTI = 'C'
00326         GO TO 2000-CHANGE-RECORD.
00327
00328      IF GMAINTI = 'S'
00329         PERFORM 7000-FORMAT-SCREEN
00330         GO TO 8200-SEND-DATAONLY.
00331
00332      EJECT
00333
00334  1000-ADD-RECORD       SECTION.
00335      PERFORM 3000-EDIT-MAIL-DATA.
00336
00337      IF NOT EMI-NO-ERRORS
00338          GO TO 8200-SEND-DATAONLY.
00339
00340      
      * EXEC CICS GETMAIN
00341 *        SET      (ADDRESS OF MAILING-DATA)
00342 *        LENGTH   (WS-ERMAIL-RECORD-LENGTH)
00343 *        INITIMG  (GETMAIN-SPACE)
00344 *    END-EXEC.
      *    MOVE ',"IL                  $   #00003760' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-ERMAIL-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00345
00346      MOVE  SPACES                TO  MAILING-DATA.
00347
00348      MOVE  'MA'                  TO  MA-RECORD-ID.
00349
00350      MOVE  PI-CARRIER            TO  WS-CM-CARRIER.
00351      MOVE  PI-GROUPING           TO  WS-CM-GROUPING.
00352      MOVE  PI-STATE              TO  WS-CM-STATE.
00353      MOVE  PI-ACCOUNT            TO  WS-CM-ACCOUNT.
00354      MOVE  PI-CERT-PRIME         TO  WS-CM-CERT-PRIME.
00355      MOVE  PI-CERT-SFX           TO  WS-CM-CERT-SFX.
00356      MOVE  PI-CERT-EFF-DT        TO  WS-CM-CERT-EFF-DT.
00357      MOVE  PI-COMPANY-CD         TO  WS-CM-COMPANY-CD.
00358
00359      MOVE  WS-CM-CONTROL-PRIMARY TO  MA-CONTROL-PRIMARY.
00360
00361      MOVE PI-PROCESSOR-ID        TO  MA-RECORD-ADDED-BY
00362                                      MA-LAST-MAINT-BY.
00363      MOVE EIBTIME                TO  MA-LAST-MAINT-HHMMSS.
00364      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00365      MOVE '5'                    TO  DC-OPTION-CODE.
00366      PERFORM 9700-DATE-LINK.
00367      IF DATE-CONVERSION-ERROR
00368          MOVE LOW-VALUES         TO  MA-RECORD-ADD-DT
00369                                      MA-LAST-MAINT-DT
00370      ELSE
00371          MOVE DC-BIN-DATE-1      TO  MA-RECORD-ADD-DT
00372                                      MA-LAST-MAINT-DT.
00373
00374      IF  GCODE1L GREATER ZERO
00375          MOVE GCODE1I            TO  MA-QUALIFY-CODE-1
00376      ELSE
00377          MOVE SPACES             TO  MA-QUALIFY-CODE-1.
00378
00379      IF  GCODE2L GREATER ZERO
00380          MOVE GCODE2I            TO  MA-QUALIFY-CODE-2
00381      ELSE
00382          MOVE SPACES             TO  MA-QUALIFY-CODE-2.
00383
00384      IF  GCODE3L GREATER ZERO
00385          MOVE GCODE3I            TO  MA-QUALIFY-CODE-3
00386      ELSE
00387          MOVE SPACES             TO  MA-QUALIFY-CODE-3.
00388
00389      IF GCODE4L GREATER ZERO
00390          MOVE GCODE4I            TO  MA-QUALIFY-CODE-4
00391      ELSE
00392          MOVE SPACES             TO  MA-QUALIFY-CODE-4.
00393
00394      IF GCODE5L GREATER ZERO
00395          MOVE GCODE5I            TO  MA-QUALIFY-CODE-5
00396      ELSE
00397          MOVE SPACES             TO  MA-QUALIFY-CODE-5.
00398
00399      IF GLNAMEL GREATER ZERO
00400          MOVE GLNAMEI            TO  MA-INSURED-LAST-NAME
00401      ELSE
00402          MOVE SPACES             TO  MA-INSURED-LAST-NAME.
00403
00404      IF GFNAMEL GREATER ZERO
00405          MOVE GFNAMEI            TO  MA-INSURED-FIRST-NAME
00406      ELSE
00407          MOVE SPACES             TO  MA-INSURED-FIRST-NAME.
00408
00409      IF GINITL GREATER ZERO
00410          MOVE GINITI             TO  MA-INSURED-MIDDLE-INIT
00411      ELSE
00412          MOVE SPACES             TO  MA-INSURED-MIDDLE-INIT.
00413
00414      IF GADDR1L GREATER ZERO
00415          MOVE GADDR1I            TO  MA-ADDRESS-LINE-1
00416      ELSE
00417          MOVE SPACES             TO  MA-ADDRESS-LINE-1.
00418
00419      IF GADDR2L GREATER ZERO
00420          MOVE GADDR2I            TO  MA-ADDRESS-LINE-2
00421      ELSE
00422          MOVE SPACES             TO  MA-ADDRESS-LINE-2.
00423
00424      IF GCITYL GREATER ZERO
00425          MOVE GCITYI           TO  MA-CITY
00426      ELSE
00427          MOVE SPACES             TO  MA-CITY.
00424      IF GISTATEL GREATER ZERO
00425          MOVE GISTATEI           TO  MA-ADDR-STATE
00426      ELSE
00427          MOVE SPACES             TO  MA-ADDR-STATE.
00429      IF GZIP1L GREATER ZERO
00430          MOVE GZIP1I             TO  W-ZIP-TEST
00431
00432          IF  W-CANADIAN-POST-CODE
00433              MOVE GZIP1I         TO  MA-CAN-POSTAL-CODE-1
00434
00435          ELSE
00436              MOVE GZIP1I         TO  MA-ZIP-CODE
00437
00438      ELSE
00439          MOVE ZEROS              TO  MA-ZIP-CODE.
00440
00441      IF GZIP2L GREATER ZERO
00442
00443          IF  MA-CANADIAN-POST-CODE
00444              MOVE GZIP2I         TO  MA-CAN-POSTAL-CODE-2
00445
00446          ELSE
00447              MOVE GZIP2I         TO  MA-ZIP-PLUS4
00448
00449      ELSE
00450          MOVE ZEROS              TO  MA-ZIP-PLUS4.
00451
00452      IF GPHONEL GREATER ZERO
00453          MOVE WS-PHONE           TO  MA-PHONE-NO
00454      ELSE
00455          MOVE ZEROS              TO  MA-PHONE-NO.
00456
070313*    IF GSSNOL GREATER ZERO
070313*        MOVE GSSNOI             TO  MA-INSURED-SOC-SEC-NO
070313*    ELSE
070313*        MOVE ZEROS              TO  MA-INSURED-SOC-SEC-NO.
00461
00462      IF GAGEL GREATER ZERO
00463          MOVE GAGEI              TO  MA-INSURED-ISSUE-AGE
00464      ELSE
00465          MOVE ZEROS              TO  MA-INSURED-ISSUE-AGE.
00466
00467      IF GBDTL GREATER ZERO
00468          MOVE WS-BIRTHDT         TO  MA-INSURED-BIRTH-DT
00469      ELSE
00470          MOVE LOW-VALUES         TO  MA-INSURED-BIRTH-DT.
090408     IF GJDOBL > ZERO
090408        MOVE WS-JNT-BIRTHDT      TO MA-JOINT-BIRTH-DT
090408     ELSE
090408        MOVE LOW-VALUES          TO MA-JOINT-BIRTH-DT
090408     END-IF
00472      IF GSEXL GREATER ZERO
00473          MOVE GSEXI              TO  MA-INSURED-SEX
00474      ELSE
00475          MOVE SPACES             TO  MA-INSURED-SEX.
           IF GBNAMEL > 0
              MOVE GBNAMEI             TO MA-CRED-BENE-NAME
           ELSE
              MOVE SPACES              TO MA-CRED-BENE-NAME
           END-IF
           IF GBADD1L > 0
              MOVE GBADD1I             TO MA-CRED-BENE-ADDR
           ELSE
              MOVE SPACES              TO MA-CRED-BENE-ADDR
           END-IF
           IF GBADD2L > 0
              MOVE GBADD2I             TO MA-CRED-BENE-ADDR2
           ELSE
              MOVE SPACES              TO MA-CRED-BENE-ADDR2
           END-IF
           IF GBCITYL > 0
              MOVE GBCITYI            TO MA-CRED-BENE-CITY
           ELSE
              MOVE SPACES              TO MA-CRED-BENE-CITY
           END-IF
           IF GBSTATEL > 0
              MOVE GBSTATEI            TO MA-CRED-BENE-STATE
           ELSE
              MOVE SPACES              TO MA-CRED-BENE-STATE
           END-IF
           IF GBZIP1L > 0
              MOVE GZIP1I              TO W-ZIP-TEST
              IF W-CANADIAN-POST-CODE
                 MOVE GBZIP1I          TO MA-CB-CAN-POSTAL-CODE-1
              ELSE
                 MOVE GBZIP1I          TO MA-CB-ZIP-CODE
              END-IF
           ELSE
              MOVE ZEROS               TO MA-CB-ZIP-CODE
           END-IF
           IF GBZIP2L > 0
              IF MA-CB-CANADIAN-POST-CODE
                 MOVE GBZIP2I          TO MA-CB-CAN-POSTAL-CODE-2
              ELSE
                 MOVE GBZIP2I          TO MA-CB-ZIP-PLUS4
              END-IF
           ELSE
              MOVE ZEROS               TO MA-CB-ZIP-PLUS4
           END-IF
           .
00477  1700-WRITE-RECORD.
00478
00479      PERFORM 6400-WRITE-MAIL-RECORD.
00480
00481      IF  DUPLICATE-RECORD-FOUND
00482          GO TO 1800-DUPLICATE-RECORD.
00483
00484      MOVE ER-0000                TO  EMI-ERROR.
00485      PERFORM 9900-ERROR-FORMAT.
00486      PERFORM 7000-FORMAT-SCREEN.
00487      PERFORM 4000-CERTIFICATE-UPDATE.
00488      GO TO 8100-SEND-INITIAL-MAP.
00489
00490  1800-DUPLICATE-RECORD.
00491      MOVE ER-2199                TO  EMI-ERROR.
00492      PERFORM 9900-ERROR-FORMAT.
00493      PERFORM 7000-FORMAT-SCREEN.
00494      GO TO 8100-SEND-INITIAL-MAP.
00495
00496  1900-EXIT.
00497      EXIT.
00498
00499      EJECT
00500
00501  2000-CHANGE-RECORD   SECTION.
00502      PERFORM 3000-EDIT-MAIL-DATA.
100217     if (emi-fatal-ctr = zeros)
100217        and (emi-forcable-ctr = 1)
100217        and (emi-error = er-3061)
100217        and (eibaid = dfhpf7)
100217        move '2'                 to emi-switch1
100217        continue
100217     else
00504         IF NOT EMI-NO-ERRORS
00505            GO TO 8200-SEND-DATAONLY
100217        end-if
100217     end-if
00507      MOVE  PI-CARRIER            TO  WS-CM-CARRIER.
00508      MOVE  PI-GROUPING           TO  WS-CM-GROUPING.
00509      MOVE  PI-STATE              TO  WS-CM-STATE.
00510      MOVE  PI-ACCOUNT            TO  WS-CM-ACCOUNT.
00511      MOVE  PI-CERT-PRIME         TO  WS-CM-CERT-PRIME.
00512      MOVE  PI-CERT-SFX           TO  WS-CM-CERT-SFX.
00513      MOVE  PI-CERT-EFF-DT        TO  WS-CM-CERT-EFF-DT.
00514      MOVE  PI-COMPANY-CD         TO  WS-CM-COMPANY-CD.
00515
00516      PERFORM 6300-READ-MAIL-FILE-UPDT.
00517
00518      IF  RECORD-NOT-FOUND
00519          GO TO 1000-ADD-RECORD.
00520
00521      MOVE PI-PROCESSOR-ID        TO  MA-LAST-MAINT-BY.
00522      MOVE EIBTIME                TO  MA-LAST-MAINT-HHMMSS.
00523      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00524      MOVE '5'                    TO  DC-OPTION-CODE.
00525
00526      PERFORM 9700-DATE-LINK.
00527      IF DATE-CONVERSION-ERROR
00528          MOVE LOW-VALUES         TO  MA-LAST-MAINT-DT
00529      ELSE
00530          MOVE DC-BIN-DATE-1      TO  MA-LAST-MAINT-DT.
00531
00532      IF  GCODE1L GREATER ZERO
00533          MOVE GCODE1I            TO  MA-QUALIFY-CODE-1.
00534      IF  GCODE2L GREATER ZERO
00535          MOVE GCODE2I            TO  MA-QUALIFY-CODE-2.
00536      IF  GCODE3L GREATER ZERO
00537          MOVE GCODE3I            TO  MA-QUALIFY-CODE-3.
00538      IF  GCODE4L GREATER ZERO
00539          MOVE GCODE4I            TO  MA-QUALIFY-CODE-4.
00540      IF  GCODE5L GREATER ZERO
00541          MOVE GCODE5I            TO  MA-QUALIFY-CODE-5.
00542      IF  GLNAMEL GREATER ZERO
00543          MOVE GLNAMEI            TO  MA-INSURED-LAST-NAME.
00544      IF  GFNAMEL GREATER ZERO
00545          MOVE GFNAMEI            TO  MA-INSURED-FIRST-NAME.
00546      IF  GINITL GREATER ZERO
00547          MOVE GINITI             TO  MA-INSURED-MIDDLE-INIT.
00548      IF  GADDR1L GREATER ZERO
00549          MOVE GADDR1I            TO  MA-ADDRESS-LINE-1.
00550      IF  GADDR2L GREATER ZERO
00551          MOVE GADDR2I            TO  MA-ADDRESS-LINE-2.
00552      IF  GCITYL GREATER ZERO
00553          MOVE GCITYI           TO  MA-CITY.
00552      IF  GISTATEL GREATER ZERO
00553          MOVE GISTATEI          TO  MA-ADDR-STATE.
100217     IF GZIP1L <> 0
00556          MOVE GZIP1I             TO  W-ZIP-TEST
00557
00558          IF  W-CANADIAN-POST-CODE
00559              MOVE GZIP1I         TO  MA-CAN-POSTAL-CODE-1
00560
00561          ELSE
00562              MOVE GZIP1I         TO  MA-ZIP-CODE
00563
00564      ELSE
00565          MOVE ZEROS              TO  MA-ZIP-CODE.
00566
00567      IF GZIP2L GREATER ZERO
00568
00569          IF  MA-CANADIAN-POST-CODE
00570              MOVE GZIP2I         TO  MA-CAN-POSTAL-CODE-2
00571
00572          ELSE
00573              MOVE GZIP2I         TO  MA-ZIP-PLUS4
00574
00575      ELSE
00576          MOVE ZEROS              TO  MA-ZIP-PLUS4.
00577
00578      IF GPHONEL GREATER ZERO
00579          MOVE WS-PHONE           TO  MA-PHONE-NO.
070313*    IF  GSSNOL GREATER ZERO
070313*        MOVE GSSNOI             TO  MA-INSURED-SOC-SEC-NO.
00582      IF  GAGEL GREATER ZERO
00583          MOVE GAGEI              TO  MA-INSURED-ISSUE-AGE.
00584      IF  GBDTL GREATER ZERO
00585          MOVE WS-BIRTHDT         TO  MA-INSURED-BIRTH-DT.
090408     IF GJDOBL > 0
090408        MOVE WS-JNT-BIRTHDT      TO MA-JOINT-BIRTH-DT
090408     END-IF
00586      IF  GSEXL GREATER ZERO
00587          MOVE GSEXI              TO  MA-INSURED-SEX.
           IF GBNAMEL > 0
              MOVE GBNAMEI             TO MA-CRED-BENE-NAME
           END-IF
           IF GBADD1L > 0
              MOVE GBADD1I             TO MA-CRED-BENE-ADDR
           END-IF
           IF GBADD2L > 0
              MOVE GBADD2I             TO MA-CRED-BENE-ADDR2
           END-IF
           IF GBCITYL > 0
              MOVE GBCITYI            TO MA-CRED-BENE-CITY
           END-IF
           IF GBSTATEL > 0
              MOVE GBSTATEI            TO MA-CRED-BENE-STATE
           END-IF
           IF GBZIP1L > 0
              MOVE GBZIP1I             TO W-ZIP-TEST
              IF W-CANADIAN-POST-CODE
                 MOVE GBZIP1I          TO MA-CB-CAN-POSTAL-CODE-1
              ELSE
                 MOVE GBZIP1I          TO MA-CB-ZIP-CODE
              END-IF
           ELSE
              MOVE ZEROS               TO MA-CB-ZIP-CODE
           END-IF
           IF GBZIP2L > 0
              IF MA-CB-CANADIAN-POST-CODE
                 MOVE GBZIP2I          TO MA-CB-CAN-POSTAL-CODE-2
              ELSE
                 MOVE GBZIP2I          TO MA-CB-ZIP-PLUS4
              END-IF
           ELSE
              MOVE ZEROS               TO MA-CB-ZIP-PLUS4
           END-IF
           .
00589  2700-REWRITE-RECORD.
00591      PERFORM 6500-REWRITE-MAIL-RECORD.
020816     IF PI-COMPANY-ID NOT = 'DCC' OR 'VPP'
              IF GBNAMEL > 0
                 
      * EXEC CICS READ
      *             EQUAL
      *             DATASET   (ELCERT-ID)
      *             SET       (ADDRESS OF CERTIFICATE-MASTER)
      *             RIDFLD    (WS-CM-CONTROL-PRIMARY)
      *             UPDATE
      *             RESP       (WS-RESPONSE)
      *          END-EXEC
      *    MOVE '&"S        EU         (  N#00004109' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034313039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF RESP-NORMAL
                    IF GBNAMEI NOT = CM-BENEFICIARY
                       MOVE GBNAMEI       TO CM-BENEFICIARY
                       
      * EXEC CICS REWRITE
      *                   FROM    (CERTIFICATE-MASTER)
      *                   DATASET (ELCERT-ID)
      *                   RESP    (WS-RESPONSE)
      *                END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00004120' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303034313230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                    ELSE
                       
      * EXEC CICS UNLOCK
      *                   DATASET  (ELCERT-ID)
      *                   RESP     (WS-RESPONSE)
      *                END-EXEC
      *    MOVE '&*                    #  N#00004126' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'204E233030303034313236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                    END-IF
                 END-IF
              END-IF
           END-IF
00593      MOVE ER-0000                TO  EMI-ERROR.
00594      PERFORM 9900-ERROR-FORMAT.
00595      PERFORM 7000-FORMAT-SCREEN.
00596      GO TO 8100-SEND-INITIAL-MAP.
00597
00598  2900-EXIT.
00599      EXIT.
00600
00601      EJECT
00602
00603  3000-EDIT-MAIL-DATA   SECTION.
00604      IF GAGEL NOT = 0
00605          IF GAGEI NOT NUMERIC
00606              MOVE -1             TO  GAGEL
00607              MOVE AL-UABON       TO  GAGEA
00608              MOVE ER-2187        TO  EMI-ERROR
00609              PERFORM 9900-ERROR-FORMAT.
00610
00611      IF GBDTL NOT = 0
00612         MOVE GBDTI               TO DEEDIT-FIELD
00613         PERFORM 8600-DEEDIT
00614         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
00615         MOVE '4'                 TO DC-OPTION-CODE
00616         PERFORM 9700-DATE-LINK
00617         IF DATE-CONVERSION-ERROR
00618            MOVE -1               TO GBDTL
00619            MOVE AL-UABON         TO GBDTA
00620            MOVE ER-0220          TO EMI-ERROR
00621            PERFORM 9900-ERROR-FORMAT
00622         ELSE
042221           if dc-bin-date-1 > pi-cert-eff-dt
042221              MOVE DEEDIT-FIELD-V0
042221                                 TO DC-GREG-DATE-1-MDY
042221              MOVE '4'           TO DC-OPTION-CODE
042221              move '1'           to dc-century-adjustment
042221              PERFORM 9700-DATE-LINK
042221              if no-conversion-error
042221                 MOVE DC-BIN-DATE-1
042221                                 TO WS-BIRTHDT
042221              end-if
042221           else
042221              MOVE DC-BIN-DATE-1 TO WS-BIRTHDT
042221           end-if
042221        end-if
042221     end-if
090408     IF GJDOBL NOT = 0
090408         MOVE GJDOBI          TO  DEEDIT-FIELD
090408         PERFORM 8600-DEEDIT
090408         MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
090408         MOVE '4'                TO  DC-OPTION-CODE
090408         PERFORM 9700-DATE-LINK
090408         IF DATE-CONVERSION-ERROR
090408             MOVE -1             TO  GJDOBL
090408             MOVE AL-UABON       TO  GJDOBA
090408             MOVE ER-0220        TO  EMI-ERROR
090408             PERFORM 9900-ERROR-FORMAT
090408         ELSE
042221           if dc-bin-date-1 > pi-cert-eff-dt
042221              MOVE DEEDIT-FIELD-V0
042221                                 TO DC-GREG-DATE-1-MDY
042221              MOVE '4'           TO DC-OPTION-CODE
042221              move '1'           to dc-century-adjustment
042221              PERFORM 9700-DATE-LINK
042221              if no-conversion-error
042221                 MOVE DC-BIN-DATE-1
042221                                 TO WS-JNT-BIRTHDT
042221              end-if
042221           else
042221              MOVE DC-BIN-DATE-1 TO WS-JNT-BIRTHDT
042221           end-if
042221        end-if
042221     end-if
00625      IF GSEXL NOT = 0
00626          IF GSEXI NOT = 'M' AND 'F'
00627              MOVE -1             TO  GSEXL
00628              MOVE AL-UABON       TO  GSEXA
00629              MOVE ER-0219        TO  EMI-ERROR
00630              PERFORM 9900-ERROR-FORMAT.
00631
00632      IF GPHONEL NOT = 0
00633          MOVE GPHONEI            TO  DEEDIT-FIELD
00634          PERFORM 8600-DEEDIT
00635          MOVE DEEDIT-FIELD-V0    TO  WS-PHONE.
00636
120513     if  (GCITYI NOT GREATER THAN SPACES)
120513             or
120513         (GISTATEI NOT GREATER THAN SPACES)
00642          MOVE -1                 TO  GCITYL
                                           GISTATEL
00645          MOVE AL-UABON           TO  GCITYA
                                           GISTATEA
00648          MOVE ER-7428            TO  EMI-ERROR
00649          PERFORM 9900-ERROR-FORMAT.
           IF GISTATEL > +0
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE GISTATEI            TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              
      * EXEC CICS READ
      *          DATASET   (FILE-ID-ELCNTL)
      *          SET       (ADDRESS OF CONTROL-FILE)
      *          RIDFLD    (ELCNTL-KEY)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
      *    MOVE '&"S        E          (  N#00004233' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034323333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 MOVE AL-UANON         TO GISTATEA
              ELSE
                 MOVE ER-2209          TO EMI-ERROR
                 MOVE -1               TO GISTATEL
                 MOVE AL-UABON         TO GISTATEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
100217     if gzip1l <> zeros
100217        if (gzip1i numeric)
100217           and (gzip1i <> zeros)
100217           continue
100217        else
100217           move er-2050          to emi-error
100217           move -1               to gzip1l
100217           move al-uabon         to gzip1a
100217           perform 9900-error-format
100217                                 thru 9900-exit
100217        end-if
100217     end-if
100217
100217     IF EMI-NO-ERRORS
100217        move spaces              to zipcd-pass-area
100217        move gzip1i              to pa-zip
100217        move gcityi              to pa-city
100217        move gistatei            to pa-state
100217        perform 7790-call-zip-verify
100217                                 thru 7790-exit
100217        if pa-errorcode-zip (1:1) = ' '
100217           continue
100217        else
100217           MOVE ER-3061          TO emi-ERROR
100217           move -1               to gzip1l
100217           PERFORM 9900-ERROR-FORMAT
100217                                 THRU 9900-EXIT
100217        end-if
100217     end-if
           IF (GBSTATEL > +0)
              AND (GBSTATEI NOT = SPACES AND LOW-VALUES)
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE GBSTATEI            TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              
      * EXEC CICS READ
      *          DATASET   (FILE-ID-ELCNTL)
      *          SET       (ADDRESS OF CONTROL-FILE)
      *          RIDFLD    (ELCNTL-KEY)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
      *    MOVE '&"S        E          (  N#00004285' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034323835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 MOVE AL-UANON         TO GBSTATEA
              ELSE
                 MOVE ER-2209          TO EMI-ERROR
                 MOVE -1               TO GBSTATEL
                 MOVE AL-UABON         TO GBSTATEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
           .
00653  4000-CERTIFICATE-UPDATE         SECTION.
00654      
      * EXEC CICS HANDLE CONDITION
00655 *        NOTFND   (4400-NOT-FOUND)
00656 *    END-EXEC.
      *    MOVE '"$I                   ! # #00004303' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034333033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00657
00658      
      * EXEC CICS READ
00659 *        EQUAL
00660 *        DATASET   (ELCERT-ID)
00661 *        SET       (ADDRESS OF CERTIFICATE-MASTER)
00662 *        RIDFLD    (WS-CM-CONTROL-PRIMARY)
00663 *        UPDATE
00664 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004307' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00665
00666  4200-REWRITE-CERT-MASTER.
00667      MOVE '1'                    TO  CM-INSURED-ADDRESS-SW
020816     IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
              IF GBNAMEL > 0
                 IF GBNAMEI NOT = CM-BENEFICIARY
                    MOVE GBNAMEI          TO CM-BENEFICIARY
                 END-IF
              END-IF
           END-IF
00669      
      * EXEC CICS REWRITE
00670 *        FROM      (CERTIFICATE-MASTER)
00671 *        DATASET   (ELCERT-ID)
00672 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004324' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00673
00674      GO TO 4500-EXIT.
00675
00676  4400-NOT-FOUND.
00677      MOVE -1                     TO  GMAINTL.
00678      MOVE ER-0142                TO  EMI-ERROR.
00679      PERFORM 9900-ERROR-FORMAT.
00680      GO TO 8200-SEND-DATAONLY.
00681
00682  4500-EXIT.
00683      EXIT.
00684
00685      EJECT
00686
00687  6200-READ-MAIL-FILE             SECTION.
00688      
      * EXEC CICS HANDLE CONDITION
00689 *        NOTFND   (6250-NOT-FOUND)
00690 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00004343' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034333433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00691
00692      
      * EXEC CICS READ
00693 *        EQUAL
00694 *        DATASET   (ERMAIL-ID)
00695 *        SET       (ADDRESS OF MAILING-DATA)
00696 *        RIDFLD    (WS-CM-CONTROL-PRIMARY)
00697 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004347' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00698
00699      GO TO 6290-EXIT.
00700
00701  6250-NOT-FOUND.
00702      MOVE 'N'                    TO  WS-RECORD-FOUND-SW.
00703
00704  6290-EXIT.
00705       EXIT.
00706
00707      EJECT
00708
00709  6300-READ-MAIL-FILE-UPDT        SECTION.
00710      
      * EXEC CICS HANDLE CONDITION
00711 *        NOTFND   (6350-NOT-FOUND)
00712 *    END-EXEC.
      *    MOVE '"$I                   ! % #00004365' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034333635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00713
00714      
      * EXEC CICS READ
00715 *        EQUAL
00716 *        DATASET   (ERMAIL-ID)
00717 *        SET       (ADDRESS OF MAILING-DATA)
00718 *        RIDFLD    (WS-CM-CONTROL-PRIMARY)
00719 *        UPDATE
00720 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004369' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00721
00722      GO TO 6390-EXIT.
00723
00724  6350-NOT-FOUND.
00725      MOVE 'N'                    TO  WS-RECORD-FOUND-SW.
00726
00727  6390-EXIT.
00728       EXIT.
00729
00730       EJECT
00731
00732  6400-WRITE-MAIL-RECORD          SECTION.
00733      
      * EXEC CICS HANDLE CONDITION
00734 *        DUPREC   (6450-DUPLICATE-RECORD)
00735 *    END-EXEC.
      *    MOVE '"$%                   ! & #00004388' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034333838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00736
00737      
      * EXEC CICS WRITE
00738 *        FROM      (MAILING-DATA)
00739 *        DATASET   (ERMAIL-ID)
00740 *        RIDFLD    (WS-CM-CONTROL-PRIMARY)
00741 *    END-EXEC.
           MOVE LENGTH OF
            MAILING-DATA
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004392' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-ID, 
                 MAILING-DATA, 
                 DFHEIV11, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00742
00743      GO TO 6490-EXIT.
00744
00745  6450-DUPLICATE-RECORD.
00746      MOVE 'Y'                    TO  WS-DUPREC-SW.
00747
00748  6490-EXIT.
00749      EXIT.
00750
00751      EJECT
00752
00753  6500-REWRITE-MAIL-RECORD        SECTION.
00754      
      * EXEC CICS REWRITE
00755 *        FROM      (MAILING-DATA)
00756 *        DATASET   (ERMAIL-ID)
00757 *    END-EXEC.
           MOVE LENGTH OF
            MAILING-DATA
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004409' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-ID, 
                 MAILING-DATA, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00758
00759  6590-EXIT.
00760      EXIT.
00761
00762      EJECT
00763
00764  6600-DELETE-MAIL-RECORD         SECTION.
00765      
      * EXEC CICS HANDLE CONDITION
00766 *        NOTFND (6650-RECORD-PREVS-DELETED)
00767 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00004420' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034343230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00768
00769      
      * EXEC CICS DELETE
00770 *        DATASET   (ERMAIL-ID)
00771 *    END-EXEC.
      *    MOVE '&(                    &   #00004424' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00772
00773      GO TO 6690-EXIT.
00774
00775  6650-RECORD-PREVS-DELETED.
00776      MOVE 'N'                    TO  WS-RECORD-FOUND-SW.
00777
00778  6690-EXIT.
00779      EXIT.
00780
00781      EJECT
00782
00783  7000-FORMAT-SCREEN              SECTION.
00784      MOVE LOW-VALUES             TO  EL127GO.
00785
100217     move  -1                    to  gmaintl
00786      MOVE  PI-CARRIER            TO  GCARIERO.
00787      MOVE  PI-GROUPING           TO  GGROUPO.
00788      MOVE  PI-STATE              TO  GSTATEO.
00789      MOVE  PI-ACCOUNT            TO  GACCTNOO.
00790      MOVE  PI-CERT-PRIME         TO  GCERTNOO.
00791      MOVE  PI-CERT-SFX           TO  GCRTSFXO.
00792      MOVE  ' '                   TO  DC-OPTION-CODE.
00793      MOVE  PI-CERT-EFF-DT        TO  DC-BIN-DATE-1.
00794      PERFORM 9700-DATE-LINK.
00795      MOVE DC-GREG-DATE-1-EDIT    TO  GEFFDTO.
00796
00797      MOVE  PI-CARRIER            TO  WS-CM-CARRIER.
00798      MOVE  PI-GROUPING           TO  WS-CM-GROUPING.
00799      MOVE  PI-STATE              TO  WS-CM-STATE.
00800      MOVE  PI-ACCOUNT            TO  WS-CM-ACCOUNT.
00801      MOVE  PI-CERT-PRIME         TO  WS-CM-CERT-PRIME.
00802      MOVE  PI-CERT-SFX           TO  WS-CM-CERT-SFX.
00803      MOVE  PI-CERT-EFF-DT        TO  WS-CM-CERT-EFF-DT.
00804      MOVE  PI-COMPANY-CD         TO  WS-CM-COMPANY-CD.
00805
00806      PERFORM 6200-READ-MAIL-FILE.
00807
00808      IF RECORD-NOT-FOUND
00809          IF PI-PEND-SW = 'P'
00810              MOVE 'S'            TO  GMAINTI
00811              MOVE AL-SANON       TO  GMAINTA
00812              MOVE ER-7049        TO  EMI-ERROR
00813              PERFORM 9900-ERROR-FORMAT
100217             GO TO 7000-EXIT
00815          ELSE
00816              MOVE 'A'            TO  GMAINTI
00817              MOVE AL-UANON       TO  GMAINTA
100217             GO TO 7000-EXIT.
00819
00820      MOVE 'C'                    TO  GMAINTO.
00821      MOVE AL-UANON               TO  GMAINTA.
00822
00823      IF PI-PEND-SW = 'P'
00824          MOVE 'S'                TO  GMAINTI
00825          MOVE AL-SANON           TO  GMAINTA
00826          MOVE ER-7049            TO  EMI-ERROR
00827          PERFORM 9900-ERROR-FORMAT.
00828
00829      MOVE MA-LAST-MAINT-BY       TO  GLSTUSRO.
00830      MOVE MA-LAST-MAINT-HHMMSS   TO  TIME-IN.
00831      MOVE TIME-OUT               TO  GLSTIMEO.
00832      MOVE ' '                    TO  DC-OPTION-CODE.
00833      MOVE MA-LAST-MAINT-DT       TO  DC-BIN-DATE-1.
00834      PERFORM 9700-DATE-LINK.
00835      IF DATE-CONVERSION-ERROR
00836          MOVE ZEROS                  TO  GLSTDTO
00837      ELSE
00838          MOVE DC-GREG-DATE-1-EDIT    TO  GLSTDTO.
00839
00840      MOVE MA-RECORD-ADDED-BY     TO  GADDBYO.
00841      MOVE ' '                    TO  DC-OPTION-CODE.
00842      MOVE MA-RECORD-ADD-DT       TO  DC-BIN-DATE-1.
00843      PERFORM 9700-DATE-LINK.
00844      IF DATE-CONVERSION-ERROR
00845          MOVE ZEROS                  TO  GADDDTO
00846      ELSE
00847          MOVE DC-GREG-DATE-1-EDIT    TO  GADDDTO.
00848
00849      MOVE MA-QUALIFY-CODE-1      TO  GCODE1O.
00850      MOVE MA-QUALIFY-CODE-2      TO  GCODE2O.
00851      MOVE MA-QUALIFY-CODE-3      TO  GCODE3O.
00852      MOVE MA-QUALIFY-CODE-4      TO  GCODE4O.
00853      MOVE MA-QUALIFY-CODE-5      TO  GCODE5O.
00854      MOVE MA-INSURED-LAST-NAME   TO  GLNAMEO.
00855      MOVE MA-INSURED-FIRST-NAME  TO  GFNAMEO.
00856      MOVE MA-INSURED-MIDDLE-INIT TO  GINITO.
00857      MOVE MA-ADDRESS-LINE-1      TO  GADDR1O.
00858      MOVE MA-ADDRESS-LINE-2      TO  GADDR2O.
00859      MOVE MA-CITY                TO  GCITYO.
           MOVE MA-ADDR-STATE          TO  GISTATEO
00860
00861      IF  MA-CANADIAN-POST-CODE
00862          MOVE MA-CAN-POSTAL-CODE-1 TO GZIP1O
00863
00864      ELSE
00865          MOVE MA-ZIP-CODE        TO  GZIP1O.
00866
00867      IF  MA-CANADIAN-POST-CODE
00868          MOVE MA-CAN-POSTAL-CODE-2
00869                                  TO  GZIP2O
00870
00871      ELSE
00872          MOVE MA-ZIP-PLUS4       TO  GZIP2O.
00873
070313*    MOVE MA-INSURED-SOC-SEC-NO  TO  GSSNOO.
00875
00876      IF MA-INSURED-SEX NOT = SPACES
00877          MOVE MA-INSURED-SEX     TO  GSEXO
00878          MOVE AL-UANON           TO  GSEXA.
00879
00880      IF MA-INSURED-ISSUE-AGE NOT NUMERIC OR
00881          MA-INSURED-ISSUE-AGE = ZEROS
00882              MOVE LOW-VALUES             TO  GAGEO
00883              MOVE AL-UANOF               TO  GAGEA
00884      ELSE
00885              MOVE AL-UANON               TO  GAGEA
00886              MOVE MA-INSURED-ISSUE-AGE   TO  GAGEO.
00887
00888      IF MA-INSURED-BIRTH-DT IS EQUAL TO LOW-VALUES OR SPACES
00889          MOVE SPACES                 TO  GBDTO
00890          MOVE AL-UANOF               TO  GBDTA
00891      ELSE
00892          MOVE ' '                    TO  DC-OPTION-CODE
00893          MOVE MA-INSURED-BIRTH-DT    TO  DC-BIN-DATE-1
00894          PERFORM 9700-DATE-LINK
00895          IF DATE-CONVERSION-ERROR
00896              MOVE LOW-VALUES             TO  GBDTO
00897              MOVE AL-UANOF               TO  GBDTA
00898          ELSE
00899              MOVE AL-UANON               TO  GBDTA
00900              MOVE DC-GREG-DATE-1-EDIT    TO  GBDTO.
090408     IF MA-JOINT-BIRTH-DT = LOW-VALUES OR SPACES
090408        MOVE SPACES              TO GJDOBO
090408        MOVE AL-UANOF            TO GJDOBA
090408     ELSE
090408        MOVE ' '                 TO DC-OPTION-CODE
090408        MOVE MA-JOINT-BIRTH-DT   TO DC-BIN-DATE-1
090408        PERFORM 9700-DATE-LINK
090408        IF DATE-CONVERSION-ERROR
090408           MOVE LOW-VALUES       TO GJDOBO
090408           MOVE AL-UANOF         TO GJDOBA
090408        ELSE
090408           MOVE AL-UANON         TO GJDOBA
090408           MOVE DC-GREG-DATE-1-EDIT
090408                                 TO  GJDOBO
090408        END-IF
090408     END-IF
00902      IF MA-PHONE-NO NOT NUMERIC OR
00903          MA-PHONE-NO = ZEROS
00904              MOVE LOW-VALUES             TO  GPHONEO
00905              MOVE AL-UANOF               TO  GPHONEA
00906      ELSE
00907              MOVE MA-PHONE-NO            TO  WS-PHONE
00908              MOVE WS-PHONE-AC            TO  ED-PHONE-AC
00909              MOVE WS-PHONE-EXT           TO  ED-PHONE-EXT
00910              MOVE WS-PHONE-NO            TO  ED-PHONE-NO
00911              MOVE '-'                    TO  ED-PHONE-DASH1
00912                                              ED-PHONE-DASH2
00913              MOVE EDITED-PHONE-NO        TO  GPHONEO
00914              MOVE AL-UANON               TO  GPHONEA
           END-IF
           MOVE MA-CRED-BENE-NAME      TO GBNAMEO
           MOVE MA-CRED-BENE-ADDR      TO GBADD1O
           MOVE MA-CRED-BENE-ADDR2     TO GBADD2O
           MOVE MA-CRED-BENE-CITY      TO GBCITYO
           MOVE MA-CRED-BENE-STATE     TO GBSTATEO
           IF MA-CB-CANADIAN-POST-CODE
              MOVE MA-CB-CAN-POSTAL-CODE-1
                                       TO GBZIP1O
           ELSE
              MOVE MA-CB-ZIP-CODE      TO GBZIP1O
           END-IF
           IF MA-CB-CANADIAN-POST-CODE
              MOVE MA-CB-CAN-POSTAL-CODE-2
                                       TO GBZIP2O
           ELSE
              MOVE MA-CB-ZIP-PLUS4     TO GBZIP2O
           END-IF
080406     MOVE +0 TO WS-SUB.
080406     PERFORM 7 TIMES
080406         ADD +1 TO WS-SUB
080406         IF MA-MAIL-TYPE (WS-SUB) NOT = LOW-VALUES AND SPACES
080406             IF MA-MAIL-TYPE (WS-SUB) = '1'
080406                MOVE '12M' TO WS-MAILED-TYPE (WS-SUB)
080406             ELSE
080406                MOVE 'EXP' TO WS-MAILED-TYPE (WS-SUB)
080406             END-IF
080406             IF MA-MAIL-STATUS (WS-SUB) = '3'
080406                MOVE 'NOT MAILED'  TO  WS-MAILED-STATUS (WS-SUB)
080406             ELSE
080406                 IF MA-MAIL-STATUS (WS-SUB) = '2'
080406                     MOVE 'RETRN'  TO  WS-MAILED-STAT
080406                 ELSE
080406                     MOVE 'MAILD'  TO  WS-MAILED-STAT
080406                 END-IF
080406                 MOVE MA-MAIL-DATE (WS-SUB) TO  DC-BIN-DATE-1
080406                 PERFORM 9700-DATE-LINK
080406                 MOVE DC-GREG-DATE-1-EDIT   TO WS-MAILED-DATE
080406                 MOVE WS-MAILED-BLD-STATUS TO
080406                                   WS-MAILED-STATUS (WS-SUB)
080406             END-IF
080406         ELSE
080406             MOVE SPACES TO WS-MAILED-TYPE (WS-SUB)
080406                            WS-MAILED-STATUS (WS-SUB)
080406         END-IF
080406     END-PERFORM.
080406
080406     MOVE WS-MAILED-TYPE (1)   TO GTYPE1O.
080406     MOVE WS-MAILED-STATUS (1) TO GSTAT1O.
080406     MOVE WS-MAILED-TYPE (2)   TO GTYPE2O.
080406     MOVE WS-MAILED-STATUS (2) TO GSTAT2O.
080406     MOVE WS-MAILED-TYPE (3)   TO GTYPE3O.
080406     MOVE WS-MAILED-STATUS (3) TO GSTAT3O.
080406     MOVE WS-MAILED-TYPE (4)   TO GTYPE4O.
080406     MOVE WS-MAILED-STATUS (4) TO GSTAT4O.
080406     MOVE WS-MAILED-TYPE (5)   TO GTYPE5O.
080406     MOVE WS-MAILED-STATUS (5) TO GSTAT5O.
080406     MOVE WS-MAILED-TYPE (6)   TO GTYPE6O.
080406     MOVE WS-MAILED-STATUS (6) TO GSTAT6O.
080406     MOVE WS-MAILED-TYPE (7)   TO GTYPE7O.
080406     MOVE WS-MAILED-STATUS (7) TO GSTAT7O.
00915
070313     move al-sadof               to  gssnoa
00916      MOVE AL-UANON               TO  GCODE1A   GCODE2A    GCODE3A
00917                                      GCODE4A   GCODE5A    GLNAMEA
070313                                     GFNAMEA   GINITA
00919                                      GADDR1A   GADDR2A    GCITYA
                                           GISTATEA
00920                                      GZIP1A    GZIP2A     GBNAMEA
                                           GBADD1A   GBADD2A    GBCITYA
                                           GBSTATEA
                                           GBZIP1A   GBZIP2A
           .
100217 7000-EXIT.
00923      EXIT.
100217 7790-CALL-ZIP-VERIFY section.
100217
100217     
      * EXEC CICS LINK
100217*        PROGRAM    ('WSZIPCD')
100217*        COMMAREA   (ZIPCD-PASS-AREA)
100217*        LENGTH     (ZIPCD-PASS-AREA-LEN)
100217*    END-EXEC.
           MOVE 'WSZIPCD' TO DFHEIV1
      *    MOVE '."C                   (   #00004664' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ZIPCD-PASS-AREA, 
                 ZIPCD-PASS-AREA-LEN, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
100217
100217 7790-EXIT.
100217     EXIT.
00927  8100-SEND-INITIAL-MAP SECTION.
00928      MOVE SAVE-DATE              TO  GDATEO.
00929      MOVE EIBTIME                TO  TIME-IN.
00930      MOVE TIME-OUT               TO  GTIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00931      MOVE EMI-MESSAGE-AREA (1)   TO  GERMSG1O.
00932
00933      IF PI-PEND-SW = 'P'
00934          MOVE -1                 TO  GPFKEYL
00935      ELSE
00936          MOVE -1                 TO  GMAINTL.
00937
00938      
      * EXEC CICS SEND
00939 *        MAP      (MAP-NAME)
00940 *        MAPSET   (MAPSET-NAME)
00941 *        FROM     (EL127GO)
00942 *        ERASE
00943 *        CURSOR
00944 *     END-EXEC.
           MOVE LENGTH OF
            EL127GO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004685' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127GO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00945
00946      GO TO 9100-RETURN-TRAN.
00947
00948  8200-SEND-DATAONLY     SECTION.
00949      MOVE SAVE-DATE              TO  GDATEO.
00950      MOVE EIBTIME                TO  TIME-IN.
00951      MOVE TIME-OUT               TO  GTIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00952      MOVE EMI-MESSAGE-AREA (1)   TO  GERMSG1O.
00953
00954      IF PI-PEND-SW = 'P'
00955          MOVE -1                 TO  GPFKEYL.
00956 *    ELSE
00957 *        MOVE -1                 TO  GMAINTL.
00958
00959      
      * EXEC CICS SEND
00960 *        MAP      (MAP-NAME)
00961 *        MAPSET   (MAPSET-NAME)
00962 *        FROM     (EL127GO)
00963 *        DATAONLY
00964 *        ERASEAUP
00965 *        CURSOR
00966 *     END-EXEC.
           MOVE LENGTH OF
            EL127GO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00004708' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127GO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00967
00968      GO TO 9100-RETURN-TRAN.
00969
00970      EJECT
00971
00972  8300-SEND-TEXT         SECTION.
00973      
      * EXEC CICS SEND TEXT
00974 *        FROM     (LOGOFF-TEXT)
00975 *        LENGTH   (LOGOFF-LENGTH)
00976 *        ERASE
00977 *        FREEKB
00978 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004722' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00979
00980      
      * EXEC CICS RETURN
00981 *    END-EXEC.
      *    MOVE '.(                    ''   #00004729' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00982
00983      EJECT
00984
00985
00986  8600-DEEDIT           SECTION.
00987      
      * EXEC CICS BIF DEEDIT
00988 *         FIELD   (DEEDIT-FIELD)
00989 *         LENGTH  (15)
00990 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004736' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00991
00992  8800-UNAUTHORIZED-ACCESS        SECTION.
00993      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
00994      GO TO 8300-SEND-TEXT.
00995
00996  8810-PF23              SECTION.
00997      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
00998      MOVE XCTL-005               TO  PGM-NAME.
00999      GO TO 9300-XCTL.
01000
01001  9100-RETURN-TRAN       SECTION.
01002      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01003      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01004      
      * EXEC CICS RETURN
01005 *        TRANSID    (TRANS-ID)
01006 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01007 *        LENGTH     (PI-COMM-LENGTH)
01008 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00004753' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01009
01010  9200-RETURN-MAIN-MENU SECTION.
01011      MOVE XCTL-126               TO  PGM-NAME.
01012      GO TO 9300-XCTL.
01013
01014  9300-XCTL             SECTION.
01015      
      * EXEC CICS XCTL
01016 *        PROGRAM    (PGM-NAME)
01017 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01018 *        LENGTH     (PI-COMM-LENGTH)
01019 *    END-EXEC.
      *    MOVE '.$C                   %   #00004764' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01020
01021  9400-CLEAR            SECTION.
01022      MOVE ' '                    TO  PI-PEND-SW.
01023      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
01024      GO TO 9300-XCTL.
01025
01026  9500-PF12             SECTION.
01027      MOVE XCTL-010               TO  PGM-NAME.
01028      GO TO 9300-XCTL.
01029
01030  9600-PGMID-ERROR      SECTION.
01031      
      * EXEC CICS HANDLE CONDITION
01032 *        PGMIDERR    (8300-SEND-TEXT)
01033 *    END-EXEC.
      *    MOVE '"$L                   ! ( #00004780' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034373830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01034      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
01035      MOVE ' '                    TO  PI-ENTRY-CD-1.
01036      MOVE XCTL-005               TO  PGM-NAME.
01037      MOVE PGM-NAME               TO  LOGOFF-PGM.
01038      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01039      GO TO 9300-XCTL.
01040
01041  9700-DATE-LINK         SECTION.
01042      MOVE LINK-ELDATCV           TO  PGM-NAME
01043      
      * EXEC CICS LINK
01044 *        PROGRAM    (PGM-NAME)
01045 *        COMMAREA   (DATE-CONVERSION-DATA)
01046 *        LENGTH     (DC-COMM-LENGTH)
01047 *    END-EXEC.
      *    MOVE '."C                   (   #00004792' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01048
01049
01050  9900-ERROR-FORMAT       SECTION.
01051      IF NOT EMI-ERRORS-COMPLETE
01052          MOVE LINK-001           TO  PGM-NAME
01053          
      * EXEC CICS LINK
01054 *            PROGRAM    (PGM-NAME)
01055 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01056 *            LENGTH     (EMI-COMM-LENGTH)
01057 *        END-EXEC.
      *    MOVE '."C                   (   #00004802' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01058
01059  9900-EXIT.
01060      EXIT.
01061
01062  9990-ABEND             SECTION.
01063      MOVE LINK-004               TO  PGM-NAME.
01064      MOVE DFHEIBLK               TO  EMI-LINE1.
01065
01066      
      * EXEC CICS LINK
01067 *        PROGRAM   (PGM-NAME)
01068 *        COMMAREA  (EMI-LINE1)
01069 *        LENGTH    (72)
01070 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00004815' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01071
01072      MOVE -1                     TO  GMAINTL.
01073
01074      GO TO 8200-SEND-DATAONLY.
01075
01076  9995-SECURITY-VIOLATION.
01077 *                            COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00004843' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
01078
01079  9995-EXIT.
01080      EXIT.
01081

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1277' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 4400-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 6250-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 6350-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 6450-DUPLICATE-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 6650-RECORD-PREVS-DELETED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1277' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
