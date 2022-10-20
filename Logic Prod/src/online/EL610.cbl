00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL610.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 17:01:58.
00007 *                            VMOD=2.003.
00008 *
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *                                                                *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024
00025 *REMARKS.    TRANSACTION - EXA8 - LOAN OFFICER PENETRATION.
00026
00027  ENVIRONMENT DIVISION.
00028
110105******************************************************************
110105*                   C H A N G E   L O G
110105*
110105* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
110105*-----------------------------------------------------------------
110105*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
110105* EFFECTIVE    NUMBER
110105*-----------------------------------------------------------------
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
110105******************************************************************
00030  DATA DIVISION.
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*    EL610  WORKING STORAGE    *'.
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.003 *********'.
00035
00036 *COPY ELCSCTM.
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
00037
00038 *COPY ELCSCRTY.
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
00039
00040      EJECT
00041
00042  01  WS-DATE-AREA.
00043      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00044      05  SAVE-BIN-DATE               PIC X(2)    VALUE SPACES.
00045
00046  01  STANDARD-AREAS.
00047      12  SC-ITEM                     PIC S9(4) COMP VALUE +1.
00048      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00049      12  MAP-NAME                    PIC X(8)    VALUE 'EL610A'.
00050      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL610S'.
00051      12  SCREEN-NUMBER               PIC X(4)    VALUE '610A'.
00052      12  TRANS-ID                    PIC X(4)    VALUE 'EXA8'.
00053      12  THIS-PGM                    PIC X(8)    VALUE 'EL610'.
00054      12  PGM-NAME                    PIC X(8).
00055      12  TIME-IN                     PIC S9(7).
00056      12  TIME-OUT-R  REDEFINES TIME-IN.
00057          16  FILLER                  PIC X.
00058          16  TIME-OUT                PIC 99V99.
00059          16  FILLER                  PIC X(2).
00060      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00061      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00062      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.
00063      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00064      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00065      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00066      12  ERLOFC-ID                   PIC X(8)    VALUE 'ERLOFC'.
00067      12  ERACCT-ALT-FILE-ID          PIC X(8)    VALUE 'ERACCT2'.
00068      12  WS-JOURNAL-RECORD-LENGTH PIC S9(4)   COMP VALUE +763.
00069
00070      12  DEEDIT-FIELD                PIC X(15).
00071      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
00072      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.
00073
00074      12  RETURN-FROM                 PIC X(8).
00075      12  QID.
00076          16  QID-TERM                PIC X(4).
00077          16  FILLER                  PIC X(4)    VALUE '627D'.
00078      12  WS-SUB1                     PIC  S99 COMP.
00079      12  WS-DATA-KEYED-SW            PIC X       VALUE 'N'.
00080          88  DATA-WAS-KEYED                      VALUE 'Y'.
00081      12  WS-SCREEN-KEYED-SW          PIC X       VALUE 'N'.
00082          88  SCREEN-WAS-KEYED                    VALUE 'Y'.
00083      12  WS-OFFICER-FOUND-SW         PIC X       VALUE 'N'.
00084          88  OFFICER-WAS-FOUND                   VALUE 'Y'.
00085          88  OFFICER-WAS-NOT-FOUND               VALUE 'N'.
00086      12  WS-FIRST-TIME-SW            PIC X       VALUE 'Y'.
00087          88  FIRST-TIME                          VALUE 'Y'.
00088          88  NOT-FIRST-TIME                      VALUE 'N'.
00089
00090      EJECT
00091  01   ERROR-MESSAGES.
00092      12  ER-0000                     PIC  X(4)   VALUE '0000'.
00093      12  ER-0004                     PIC  X(4)   VALUE '0004'.
00094      12  ER-0023                     PIC  X(4)   VALUE '0023'.
00095      12  ER-0029                     PIC  X(4)   VALUE '0029'.
00096      12  ER-0050                     PIC  X(4)   VALUE '0050'.
00097      12  ER-0070                     PIC  X(4)   VALUE '0070'.
00098      12  ER-0132                     PIC  X(4)   VALUE '0132'.
00099      12  ER-0142                     PIC  X(4)   VALUE '0142'.
00100      12  ER-0194                     PIC  X(4)   VALUE '0194'.
00101      12  ER-0195                     PIC  X(4)   VALUE '0195'.
00102      12  ER-0196                     PIC  X(4)   VALUE '0196'.
00103      12  ER-0197                     PIC  X(4)   VALUE '0197'.
00104      12  ER-2210                     PIC  X(4)   VALUE '2210'.
00105      12  ER-2237                     PIC  X(4)   VALUE '2237'.
00106      12  ER-2238                     PIC  X(4)   VALUE '2238'.
00107      12  ER-3001                     PIC  X(4)   VALUE '3001'.
00108      12  ER-3002                     PIC  X(4)   VALUE '3002'.
00109      12  ER-3003                     PIC  X(4)   VALUE '3003'.
00110      12  ER-3004                     PIC  X(4)   VALUE '3004'.
00111      12  ER-3005                     PIC  X(4)   VALUE '3005'.
00112      12  ER-3006                     PIC  X(4)   VALUE '3006'.
00113      12  ER-3007                     PIC  X(4)   VALUE '3007'.
00114      12  ER-3008                     PIC  X(4)   VALUE '3008'.
00115      12  ER-3009                     PIC  X(4)   VALUE '3009'.
00116      12  ER-3010                     PIC  X(4)   VALUE '3010'.
00117      12  ER-3011                     PIC  X(4)   VALUE '3011'.
00118      12  ER-3012                     PIC  X(4)   VALUE '3012'.
00119      12  ER-3013                     PIC  X(4)   VALUE '3013'.
00120      12  ER-3014                     PIC  X(4)   VALUE '3014'.
00121      12  ER-3015                     PIC  X(4)   VALUE '3015'.
00122      12  ER-3016                     PIC  X(4)   VALUE '3016'.
00123      12  ER-3018                     PIC  X(4)   VALUE '3018'.
00124      12  ER-3019                     PIC  X(4)   VALUE '3019'.
00125      12  ER-7008                     PIC  X(4)   VALUE '7008'.
00126
00127      EJECT
00128
00129  01  ACCESS-KEYS.
00130      12  ERLOFC-KEY.
00131          16  ERLOFC-COMPANY-CD          PIC  X.
00132          16  ERLOFC-CARRIER             PIC  X.
00133          16  ERLOFC-GROUPING            PIC  X(6).
00134          16  ERLOFC-STATE               PIC  XX.
00135          16  ERLOFC-ACCOUNT             PIC  X(10).
110105         16  ERLOFC-OFFICER-CODE        PIC  X(5).
00136 *        16  ERLOFC-OFFICER-CODE        PIC  X(3).
00137
00138      12  ERLOFC-RECORD-LENGTH    PIC S9(4) COMP VALUE +670.
00139
00140      12  ERACCT-ALT-KEY.
00141          16  ERACCT-A-CO-CD      PIC X     VALUE SPACES.
00142          16  ERACCT-A-CARRIER    PIC X     VALUE SPACES.
00143          16  ERACCT-A-GROUPING   PIC X(6)  VALUE SPACES.
00144          16  ERACCT-A-STATE      PIC XX    VALUE SPACES.
00145          16  ERACCT-A-ACCOUNT    PIC X(10) VALUE SPACES.
00146          16  ERACCT-A-EXP-DATE   PIC XX    VALUE SPACES.
00147
00148      12  ERACCT-RECORD-LENGTH    PIC S9(4) COMP VALUE +1464.
00149
00150      EJECT
00151
00152 *COPY ELCDATE.
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
00153
00154      EJECT
00155 *COPY ELCLOGOF.
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
00156
00157      EJECT
00158 *COPY ELCATTR.
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
00159
00160      EJECT
00161 *COPY ELCEMIB.
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
00070      12  FILLER                      PIC X(137)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00162
00163      EJECT
00164 *COPY ELCINTF.
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
00131      12  FILLER                          PIC X(4).
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
00165      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00166          16  PI-MAINT                PIC  X.
110105         16  PI-LOAN-OFFICER         PIC  X(5).
00167 *        16  PI-LOAN-OFFICER         PIC  X(3).
00168          16  PI-CRLOFC-KEY.
00169              20  PI-CRLOFC-COMPANY-CD          PIC  X.
00170              20  PI-CRLOFC-CARRIER             PIC  X.
00171              20  PI-CRLOFC-GROUPING            PIC  X(6).
00172              20  PI-CRLOFC-STATE               PIC  XX.
00173              20  PI-CRLOFC-ACCOUNT             PIC  X(10).
110105             20  PI-CRLOFC-OFFICER-CODE        PIC  X(5).
00174 *            20  PI-CRLOFC-OFFICER-CODE        PIC  X(3).
00175
00176          16  PI-PREV-CRLOFC-KEY                PIC X(25).
110105         16  FILLER                            PIC X(584).
00177 *        16  FILLER                            PIC X(590).
00178
00179      EJECT
00180
00181 *COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
00182                              PIC X(523).
00183      EJECT
00184
00185 *COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00186  01  FILLER    REDEFINES DFHAID.
00187      12  FILLER              PIC X(8).
00188      12  PF-VALUES           PIC X       OCCURS 24 TIMES.
00189
00190      EJECT
00191
00192 *COPY EL610S.
       01  EL610AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  ADATEL PIC S9(0004) COMP.
           05  ADATEF PIC  X(0001).
           05  FILLER REDEFINES ADATEF.
               10  ADATEA PIC  X(0001).
           05  ADATEI PIC  X(0008).
      *    -------------------------------
           05  ATIMEL PIC S9(0004) COMP.
           05  ATIMEF PIC  X(0001).
           05  FILLER REDEFINES ATIMEF.
               10  ATIMEA PIC  X(0001).
           05  ATIMEI PIC  X(0005).
      *    -------------------------------
           05  AMAINTL PIC S9(0004) COMP.
           05  AMAINTF PIC  X(0001).
           05  FILLER REDEFINES AMAINTF.
               10  AMAINTA PIC  X(0001).
           05  AMAINTI PIC  X(0001).
      *    -------------------------------
           05  ACARHDGL PIC S9(0004) COMP.
           05  ACARHDGF PIC  X(0001).
           05  FILLER REDEFINES ACARHDGF.
               10  ACARHDGA PIC  X(0001).
           05  ACARHDGI PIC  X(0004).
      *    -------------------------------
           05  AGRPHDGL PIC S9(0004) COMP.
           05  AGRPHDGF PIC  X(0001).
           05  FILLER REDEFINES AGRPHDGF.
               10  AGRPHDGA PIC  X(0001).
           05  AGRPHDGI PIC  X(0005).
      *    -------------------------------
           05  ASTHDGL PIC S9(0004) COMP.
           05  ASTHDGF PIC  X(0001).
           05  FILLER REDEFINES ASTHDGF.
               10  ASTHDGA PIC  X(0001).
           05  ASTHDGI PIC  X(0002).
      *    -------------------------------
           05  ACARRL PIC S9(0004) COMP.
           05  ACARRF PIC  X(0001).
           05  FILLER REDEFINES ACARRF.
               10  ACARRA PIC  X(0001).
           05  ACARRI PIC  X(0001).
      *    -------------------------------
           05  AGRPL PIC S9(0004) COMP.
           05  AGRPF PIC  X(0001).
           05  FILLER REDEFINES AGRPF.
               10  AGRPA PIC  X(0001).
           05  AGRPI PIC  X(0006).
      *    -------------------------------
           05  ASTL PIC S9(0004) COMP.
           05  ASTF PIC  X(0001).
           05  FILLER REDEFINES ASTF.
               10  ASTA PIC  X(0001).
           05  ASTI PIC  X(0002).
      *    -------------------------------
           05  AACCTL PIC S9(0004) COMP.
           05  AACCTF PIC  X(0001).
           05  FILLER REDEFINES AACCTF.
               10  AACCTA PIC  X(0001).
           05  AACCTI PIC  X(0010).
      *    -------------------------------
           05  AOFCRL PIC S9(0004) COMP.
           05  AOFCRF PIC  X(0001).
           05  FILLER REDEFINES AOFCRF.
               10  AOFCRA PIC  X(0001).
           05  AOFCRI PIC  X(0005).
      *    -------------------------------
           05  ACOMPL PIC S9(0004) COMP.
           05  ACOMPF PIC  X(0001).
           05  FILLER REDEFINES ACOMPF.
               10  ACOMPA PIC  X(0001).
           05  ACOMPI PIC  X(0001).
      *    -------------------------------
           05  APRINTL PIC S9(0004) COMP.
           05  APRINTF PIC  X(0001).
           05  FILLER REDEFINES APRINTF.
               10  APRINTA PIC  X(0001).
           05  APRINTI PIC  X(0001).
      *    -------------------------------
           05  ANAMEL PIC S9(0004) COMP.
           05  ANAMEF PIC  X(0001).
           05  FILLER REDEFINES ANAMEF.
               10  ANAMEA PIC  X(0001).
           05  ANAMEI PIC  X(0030).
      *    -------------------------------
           05  AMO1L PIC S9(0004) COMP.
           05  AMO1F PIC  X(0001).
           05  FILLER REDEFINES AMO1F.
               10  AMO1A PIC  X(0001).
           05  AMO1I PIC  X(0002).
      *    -------------------------------
           05  ALNCNT1L PIC S9(0004) COMP.
           05  ALNCNT1F PIC  X(0001).
           05  FILLER REDEFINES ALNCNT1F.
               10  ALNCNT1A PIC  X(0001).
           05  ALNCNT1I PIC  9(6).
      *    -------------------------------
           05  ALNVOL1L PIC S9(0004) COMP.
           05  ALNVOL1F PIC  X(0001).
           05  FILLER REDEFINES ALNVOL1F.
               10  ALNVOL1A PIC  X(0001).
           05  ALNVOL1I PIC  9(11).
      *    -------------------------------
           05  ALFCNT1L PIC S9(0004) COMP.
           05  ALFCNT1F PIC  X(0001).
           05  FILLER REDEFINES ALFCNT1F.
               10  ALFCNT1A PIC  X(0001).
           05  ALFCNT1I PIC  X(0006).
      *    -------------------------------
           05  ALFPRM1L PIC S9(0004) COMP.
           05  ALFPRM1F PIC  X(0001).
           05  FILLER REDEFINES ALFPRM1F.
               10  ALFPRM1A PIC  X(0001).
           05  ALFPRM1I PIC  X(0009).
      *    -------------------------------
           05  ALFAMT1L PIC S9(0004) COMP.
           05  ALFAMT1F PIC  X(0001).
           05  FILLER REDEFINES ALFAMT1F.
               10  ALFAMT1A PIC  X(0001).
           05  ALFAMT1I PIC  X(0011).
      *    -------------------------------
           05  AAHCNT1L PIC S9(0004) COMP.
           05  AAHCNT1F PIC  X(0001).
           05  FILLER REDEFINES AAHCNT1F.
               10  AAHCNT1A PIC  X(0001).
           05  AAHCNT1I PIC  X(0006).
      *    -------------------------------
           05  AAHPRM1L PIC S9(0004) COMP.
           05  AAHPRM1F PIC  X(0001).
           05  FILLER REDEFINES AAHPRM1F.
               10  AAHPRM1A PIC  X(0001).
           05  AAHPRM1I PIC  X(0009).
      *    -------------------------------
           05  AAHAMT1L PIC S9(0004) COMP.
           05  AAHAMT1F PIC  X(0001).
           05  FILLER REDEFINES AAHAMT1F.
               10  AAHAMT1A PIC  X(0001).
           05  AAHAMT1I PIC  X(0011).
      *    -------------------------------
           05  AMO2L PIC S9(0004) COMP.
           05  AMO2F PIC  X(0001).
           05  FILLER REDEFINES AMO2F.
               10  AMO2A PIC  X(0001).
           05  AMO2I PIC  X(0002).
      *    -------------------------------
           05  ALNCNT2L PIC S9(0004) COMP.
           05  ALNCNT2F PIC  X(0001).
           05  FILLER REDEFINES ALNCNT2F.
               10  ALNCNT2A PIC  X(0001).
           05  ALNCNT2I PIC  9(6).
      *    -------------------------------
           05  ALNVOL2L PIC S9(0004) COMP.
           05  ALNVOL2F PIC  X(0001).
           05  FILLER REDEFINES ALNVOL2F.
               10  ALNVOL2A PIC  X(0001).
           05  ALNVOL2I PIC  9(11).
      *    -------------------------------
           05  ALFCNT2L PIC S9(0004) COMP.
           05  ALFCNT2F PIC  X(0001).
           05  FILLER REDEFINES ALFCNT2F.
               10  ALFCNT2A PIC  X(0001).
           05  ALFCNT2I PIC  X(0006).
      *    -------------------------------
           05  ALFPRM2L PIC S9(0004) COMP.
           05  ALFPRM2F PIC  X(0001).
           05  FILLER REDEFINES ALFPRM2F.
               10  ALFPRM2A PIC  X(0001).
           05  ALFPRM2I PIC  X(0009).
      *    -------------------------------
           05  ALFAMT2L PIC S9(0004) COMP.
           05  ALFAMT2F PIC  X(0001).
           05  FILLER REDEFINES ALFAMT2F.
               10  ALFAMT2A PIC  X(0001).
           05  ALFAMT2I PIC  X(0011).
      *    -------------------------------
           05  AAHCNT2L PIC S9(0004) COMP.
           05  AAHCNT2F PIC  X(0001).
           05  FILLER REDEFINES AAHCNT2F.
               10  AAHCNT2A PIC  X(0001).
           05  AAHCNT2I PIC  X(0006).
      *    -------------------------------
           05  AAHPRM2L PIC S9(0004) COMP.
           05  AAHPRM2F PIC  X(0001).
           05  FILLER REDEFINES AAHPRM2F.
               10  AAHPRM2A PIC  X(0001).
           05  AAHPRM2I PIC  X(0009).
      *    -------------------------------
           05  AAHAMT2L PIC S9(0004) COMP.
           05  AAHAMT2F PIC  X(0001).
           05  FILLER REDEFINES AAHAMT2F.
               10  AAHAMT2A PIC  X(0001).
           05  AAHAMT2I PIC  X(0011).
      *    -------------------------------
           05  AMO3L PIC S9(0004) COMP.
           05  AMO3F PIC  X(0001).
           05  FILLER REDEFINES AMO3F.
               10  AMO3A PIC  X(0001).
           05  AMO3I PIC  X(0002).
      *    -------------------------------
           05  ALNCNT3L PIC S9(0004) COMP.
           05  ALNCNT3F PIC  X(0001).
           05  FILLER REDEFINES ALNCNT3F.
               10  ALNCNT3A PIC  X(0001).
           05  ALNCNT3I PIC  9(6).
      *    -------------------------------
           05  ALNVOL3L PIC S9(0004) COMP.
           05  ALNVOL3F PIC  X(0001).
           05  FILLER REDEFINES ALNVOL3F.
               10  ALNVOL3A PIC  X(0001).
           05  ALNVOL3I PIC  9(11).
      *    -------------------------------
           05  ALFCNT3L PIC S9(0004) COMP.
           05  ALFCNT3F PIC  X(0001).
           05  FILLER REDEFINES ALFCNT3F.
               10  ALFCNT3A PIC  X(0001).
           05  ALFCNT3I PIC  X(0006).
      *    -------------------------------
           05  ALFPRM3L PIC S9(0004) COMP.
           05  ALFPRM3F PIC  X(0001).
           05  FILLER REDEFINES ALFPRM3F.
               10  ALFPRM3A PIC  X(0001).
           05  ALFPRM3I PIC  X(0009).
      *    -------------------------------
           05  ALFAMT3L PIC S9(0004) COMP.
           05  ALFAMT3F PIC  X(0001).
           05  FILLER REDEFINES ALFAMT3F.
               10  ALFAMT3A PIC  X(0001).
           05  ALFAMT3I PIC  X(0011).
      *    -------------------------------
           05  AAHCNT3L PIC S9(0004) COMP.
           05  AAHCNT3F PIC  X(0001).
           05  FILLER REDEFINES AAHCNT3F.
               10  AAHCNT3A PIC  X(0001).
           05  AAHCNT3I PIC  X(0006).
      *    -------------------------------
           05  AAHPRM3L PIC S9(0004) COMP.
           05  AAHPRM3F PIC  X(0001).
           05  FILLER REDEFINES AAHPRM3F.
               10  AAHPRM3A PIC  X(0001).
           05  AAHPRM3I PIC  X(0009).
      *    -------------------------------
           05  AAHAMT3L PIC S9(0004) COMP.
           05  AAHAMT3F PIC  X(0001).
           05  FILLER REDEFINES AAHAMT3F.
               10  AAHAMT3A PIC  X(0001).
           05  AAHAMT3I PIC  X(0011).
      *    -------------------------------
           05  AMO4L PIC S9(0004) COMP.
           05  AMO4F PIC  X(0001).
           05  FILLER REDEFINES AMO4F.
               10  AMO4A PIC  X(0001).
           05  AMO4I PIC  X(0002).
      *    -------------------------------
           05  ALNCNT4L PIC S9(0004) COMP.
           05  ALNCNT4F PIC  X(0001).
           05  FILLER REDEFINES ALNCNT4F.
               10  ALNCNT4A PIC  X(0001).
           05  ALNCNT4I PIC  9(6).
      *    -------------------------------
           05  ALNVOL4L PIC S9(0004) COMP.
           05  ALNVOL4F PIC  X(0001).
           05  FILLER REDEFINES ALNVOL4F.
               10  ALNVOL4A PIC  X(0001).
           05  ALNVOL4I PIC  9(11).
      *    -------------------------------
           05  ALFCNT4L PIC S9(0004) COMP.
           05  ALFCNT4F PIC  X(0001).
           05  FILLER REDEFINES ALFCNT4F.
               10  ALFCNT4A PIC  X(0001).
           05  ALFCNT4I PIC  X(0006).
      *    -------------------------------
           05  ALFPRM4L PIC S9(0004) COMP.
           05  ALFPRM4F PIC  X(0001).
           05  FILLER REDEFINES ALFPRM4F.
               10  ALFPRM4A PIC  X(0001).
           05  ALFPRM4I PIC  X(0009).
      *    -------------------------------
           05  ALFAMT4L PIC S9(0004) COMP.
           05  ALFAMT4F PIC  X(0001).
           05  FILLER REDEFINES ALFAMT4F.
               10  ALFAMT4A PIC  X(0001).
           05  ALFAMT4I PIC  X(0011).
      *    -------------------------------
           05  AAHCNT4L PIC S9(0004) COMP.
           05  AAHCNT4F PIC  X(0001).
           05  FILLER REDEFINES AAHCNT4F.
               10  AAHCNT4A PIC  X(0001).
           05  AAHCNT4I PIC  X(0006).
      *    -------------------------------
           05  AAHPRM4L PIC S9(0004) COMP.
           05  AAHPRM4F PIC  X(0001).
           05  FILLER REDEFINES AAHPRM4F.
               10  AAHPRM4A PIC  X(0001).
           05  AAHPRM4I PIC  X(0009).
      *    -------------------------------
           05  AAHAMT4L PIC S9(0004) COMP.
           05  AAHAMT4F PIC  X(0001).
           05  FILLER REDEFINES AAHAMT4F.
               10  AAHAMT4A PIC  X(0001).
           05  AAHAMT4I PIC  X(0011).
      *    -------------------------------
           05  AMO5L PIC S9(0004) COMP.
           05  AMO5F PIC  X(0001).
           05  FILLER REDEFINES AMO5F.
               10  AMO5A PIC  X(0001).
           05  AMO5I PIC  X(0002).
      *    -------------------------------
           05  ALNCNT5L PIC S9(0004) COMP.
           05  ALNCNT5F PIC  X(0001).
           05  FILLER REDEFINES ALNCNT5F.
               10  ALNCNT5A PIC  X(0001).
           05  ALNCNT5I PIC  9(6).
      *    -------------------------------
           05  ALNVOL5L PIC S9(0004) COMP.
           05  ALNVOL5F PIC  X(0001).
           05  FILLER REDEFINES ALNVOL5F.
               10  ALNVOL5A PIC  X(0001).
           05  ALNVOL5I PIC  9(11).
      *    -------------------------------
           05  ALFCNT5L PIC S9(0004) COMP.
           05  ALFCNT5F PIC  X(0001).
           05  FILLER REDEFINES ALFCNT5F.
               10  ALFCNT5A PIC  X(0001).
           05  ALFCNT5I PIC  X(0006).
      *    -------------------------------
           05  ALFPRM5L PIC S9(0004) COMP.
           05  ALFPRM5F PIC  X(0001).
           05  FILLER REDEFINES ALFPRM5F.
               10  ALFPRM5A PIC  X(0001).
           05  ALFPRM5I PIC  X(0009).
      *    -------------------------------
           05  ALFAMT5L PIC S9(0004) COMP.
           05  ALFAMT5F PIC  X(0001).
           05  FILLER REDEFINES ALFAMT5F.
               10  ALFAMT5A PIC  X(0001).
           05  ALFAMT5I PIC  X(0011).
      *    -------------------------------
           05  AAHCNT5L PIC S9(0004) COMP.
           05  AAHCNT5F PIC  X(0001).
           05  FILLER REDEFINES AAHCNT5F.
               10  AAHCNT5A PIC  X(0001).
           05  AAHCNT5I PIC  X(0006).
      *    -------------------------------
           05  AAHPRM5L PIC S9(0004) COMP.
           05  AAHPRM5F PIC  X(0001).
           05  FILLER REDEFINES AAHPRM5F.
               10  AAHPRM5A PIC  X(0001).
           05  AAHPRM5I PIC  X(0009).
      *    -------------------------------
           05  AAHAMT5L PIC S9(0004) COMP.
           05  AAHAMT5F PIC  X(0001).
           05  FILLER REDEFINES AAHAMT5F.
               10  AAHAMT5A PIC  X(0001).
           05  AAHAMT5I PIC  X(0011).
      *    -------------------------------
           05  AMO6L PIC S9(0004) COMP.
           05  AMO6F PIC  X(0001).
           05  FILLER REDEFINES AMO6F.
               10  AMO6A PIC  X(0001).
           05  AMO6I PIC  X(0002).
      *    -------------------------------
           05  ALNCNT6L PIC S9(0004) COMP.
           05  ALNCNT6F PIC  X(0001).
           05  FILLER REDEFINES ALNCNT6F.
               10  ALNCNT6A PIC  X(0001).
           05  ALNCNT6I PIC  9(6).
      *    -------------------------------
           05  ALNVOL6L PIC S9(0004) COMP.
           05  ALNVOL6F PIC  X(0001).
           05  FILLER REDEFINES ALNVOL6F.
               10  ALNVOL6A PIC  X(0001).
           05  ALNVOL6I PIC  9(11).
      *    -------------------------------
           05  ALFCNT6L PIC S9(0004) COMP.
           05  ALFCNT6F PIC  X(0001).
           05  FILLER REDEFINES ALFCNT6F.
               10  ALFCNT6A PIC  X(0001).
           05  ALFCNT6I PIC  X(0006).
      *    -------------------------------
           05  ALFPRM6L PIC S9(0004) COMP.
           05  ALFPRM6F PIC  X(0001).
           05  FILLER REDEFINES ALFPRM6F.
               10  ALFPRM6A PIC  X(0001).
           05  ALFPRM6I PIC  X(0009).
      *    -------------------------------
           05  ALFAMT6L PIC S9(0004) COMP.
           05  ALFAMT6F PIC  X(0001).
           05  FILLER REDEFINES ALFAMT6F.
               10  ALFAMT6A PIC  X(0001).
           05  ALFAMT6I PIC  X(0011).
      *    -------------------------------
           05  AAHCNT6L PIC S9(0004) COMP.
           05  AAHCNT6F PIC  X(0001).
           05  FILLER REDEFINES AAHCNT6F.
               10  AAHCNT6A PIC  X(0001).
           05  AAHCNT6I PIC  X(0006).
      *    -------------------------------
           05  AAHPRM6L PIC S9(0004) COMP.
           05  AAHPRM6F PIC  X(0001).
           05  FILLER REDEFINES AAHPRM6F.
               10  AAHPRM6A PIC  X(0001).
           05  AAHPRM6I PIC  X(0009).
      *    -------------------------------
           05  AAHAMT6L PIC S9(0004) COMP.
           05  AAHAMT6F PIC  X(0001).
           05  FILLER REDEFINES AAHAMT6F.
               10  AAHAMT6A PIC  X(0001).
           05  AAHAMT6I PIC  X(0011).
      *    -------------------------------
           05  AMO7L PIC S9(0004) COMP.
           05  AMO7F PIC  X(0001).
           05  FILLER REDEFINES AMO7F.
               10  AMO7A PIC  X(0001).
           05  AMO7I PIC  X(0002).
      *    -------------------------------
           05  ALNCNT7L PIC S9(0004) COMP.
           05  ALNCNT7F PIC  X(0001).
           05  FILLER REDEFINES ALNCNT7F.
               10  ALNCNT7A PIC  X(0001).
           05  ALNCNT7I PIC  9(6).
      *    -------------------------------
           05  ALNVOL7L PIC S9(0004) COMP.
           05  ALNVOL7F PIC  X(0001).
           05  FILLER REDEFINES ALNVOL7F.
               10  ALNVOL7A PIC  X(0001).
           05  ALNVOL7I PIC  9(11).
      *    -------------------------------
           05  ALFCNT7L PIC S9(0004) COMP.
           05  ALFCNT7F PIC  X(0001).
           05  FILLER REDEFINES ALFCNT7F.
               10  ALFCNT7A PIC  X(0001).
           05  ALFCNT7I PIC  X(0006).
      *    -------------------------------
           05  ALFPRM7L PIC S9(0004) COMP.
           05  ALFPRM7F PIC  X(0001).
           05  FILLER REDEFINES ALFPRM7F.
               10  ALFPRM7A PIC  X(0001).
           05  ALFPRM7I PIC  X(0009).
      *    -------------------------------
           05  ALFAMT7L PIC S9(0004) COMP.
           05  ALFAMT7F PIC  X(0001).
           05  FILLER REDEFINES ALFAMT7F.
               10  ALFAMT7A PIC  X(0001).
           05  ALFAMT7I PIC  X(0011).
      *    -------------------------------
           05  AAHCNT7L PIC S9(0004) COMP.
           05  AAHCNT7F PIC  X(0001).
           05  FILLER REDEFINES AAHCNT7F.
               10  AAHCNT7A PIC  X(0001).
           05  AAHCNT7I PIC  X(0006).
      *    -------------------------------
           05  AAHPRM7L PIC S9(0004) COMP.
           05  AAHPRM7F PIC  X(0001).
           05  FILLER REDEFINES AAHPRM7F.
               10  AAHPRM7A PIC  X(0001).
           05  AAHPRM7I PIC  X(0009).
      *    -------------------------------
           05  AAHAMT7L PIC S9(0004) COMP.
           05  AAHAMT7F PIC  X(0001).
           05  FILLER REDEFINES AAHAMT7F.
               10  AAHAMT7A PIC  X(0001).
           05  AAHAMT7I PIC  X(0011).
      *    -------------------------------
           05  AMO8L PIC S9(0004) COMP.
           05  AMO8F PIC  X(0001).
           05  FILLER REDEFINES AMO8F.
               10  AMO8A PIC  X(0001).
           05  AMO8I PIC  X(0002).
      *    -------------------------------
           05  ALNCNT8L PIC S9(0004) COMP.
           05  ALNCNT8F PIC  X(0001).
           05  FILLER REDEFINES ALNCNT8F.
               10  ALNCNT8A PIC  X(0001).
           05  ALNCNT8I PIC  9(6).
      *    -------------------------------
           05  ALNVOL8L PIC S9(0004) COMP.
           05  ALNVOL8F PIC  X(0001).
           05  FILLER REDEFINES ALNVOL8F.
               10  ALNVOL8A PIC  X(0001).
           05  ALNVOL8I PIC  9(11).
      *    -------------------------------
           05  ALFCNT8L PIC S9(0004) COMP.
           05  ALFCNT8F PIC  X(0001).
           05  FILLER REDEFINES ALFCNT8F.
               10  ALFCNT8A PIC  X(0001).
           05  ALFCNT8I PIC  X(0006).
      *    -------------------------------
           05  ALFPRM8L PIC S9(0004) COMP.
           05  ALFPRM8F PIC  X(0001).
           05  FILLER REDEFINES ALFPRM8F.
               10  ALFPRM8A PIC  X(0001).
           05  ALFPRM8I PIC  X(0009).
      *    -------------------------------
           05  ALFAMT8L PIC S9(0004) COMP.
           05  ALFAMT8F PIC  X(0001).
           05  FILLER REDEFINES ALFAMT8F.
               10  ALFAMT8A PIC  X(0001).
           05  ALFAMT8I PIC  X(0011).
      *    -------------------------------
           05  AAHCNT8L PIC S9(0004) COMP.
           05  AAHCNT8F PIC  X(0001).
           05  FILLER REDEFINES AAHCNT8F.
               10  AAHCNT8A PIC  X(0001).
           05  AAHCNT8I PIC  X(0006).
      *    -------------------------------
           05  AAHPRM8L PIC S9(0004) COMP.
           05  AAHPRM8F PIC  X(0001).
           05  FILLER REDEFINES AAHPRM8F.
               10  AAHPRM8A PIC  X(0001).
           05  AAHPRM8I PIC  X(0009).
      *    -------------------------------
           05  AAHAMT8L PIC S9(0004) COMP.
           05  AAHAMT8F PIC  X(0001).
           05  FILLER REDEFINES AAHAMT8F.
               10  AAHAMT8A PIC  X(0001).
           05  AAHAMT8I PIC  X(0011).
      *    -------------------------------
           05  AMO9L PIC S9(0004) COMP.
           05  AMO9F PIC  X(0001).
           05  FILLER REDEFINES AMO9F.
               10  AMO9A PIC  X(0001).
           05  AMO9I PIC  X(0002).
      *    -------------------------------
           05  ALNCNT9L PIC S9(0004) COMP.
           05  ALNCNT9F PIC  X(0001).
           05  FILLER REDEFINES ALNCNT9F.
               10  ALNCNT9A PIC  X(0001).
           05  ALNCNT9I PIC  9(6).
      *    -------------------------------
           05  ALNVOL9L PIC S9(0004) COMP.
           05  ALNVOL9F PIC  X(0001).
           05  FILLER REDEFINES ALNVOL9F.
               10  ALNVOL9A PIC  X(0001).
           05  ALNVOL9I PIC  9(11).
      *    -------------------------------
           05  ALFCNT9L PIC S9(0004) COMP.
           05  ALFCNT9F PIC  X(0001).
           05  FILLER REDEFINES ALFCNT9F.
               10  ALFCNT9A PIC  X(0001).
           05  ALFCNT9I PIC  X(0006).
      *    -------------------------------
           05  ALFPRM9L PIC S9(0004) COMP.
           05  ALFPRM9F PIC  X(0001).
           05  FILLER REDEFINES ALFPRM9F.
               10  ALFPRM9A PIC  X(0001).
           05  ALFPRM9I PIC  X(0009).
      *    -------------------------------
           05  ALFAMT9L PIC S9(0004) COMP.
           05  ALFAMT9F PIC  X(0001).
           05  FILLER REDEFINES ALFAMT9F.
               10  ALFAMT9A PIC  X(0001).
           05  ALFAMT9I PIC  X(0011).
      *    -------------------------------
           05  AAHCNT9L PIC S9(0004) COMP.
           05  AAHCNT9F PIC  X(0001).
           05  FILLER REDEFINES AAHCNT9F.
               10  AAHCNT9A PIC  X(0001).
           05  AAHCNT9I PIC  X(0006).
      *    -------------------------------
           05  AAHPRM9L PIC S9(0004) COMP.
           05  AAHPRM9F PIC  X(0001).
           05  FILLER REDEFINES AAHPRM9F.
               10  AAHPRM9A PIC  X(0001).
           05  AAHPRM9I PIC  X(0009).
      *    -------------------------------
           05  AAHAMT9L PIC S9(0004) COMP.
           05  AAHAMT9F PIC  X(0001).
           05  FILLER REDEFINES AAHAMT9F.
               10  AAHAMT9A PIC  X(0001).
           05  AAHAMT9I PIC  X(0011).
      *    -------------------------------
           05  AMOAL PIC S9(0004) COMP.
           05  AMOAF PIC  X(0001).
           05  FILLER REDEFINES AMOAF.
               10  AMOAA PIC  X(0001).
           05  AMOAI PIC  X(0002).
      *    -------------------------------
           05  ALNCNTAL PIC S9(0004) COMP.
           05  ALNCNTAF PIC  X(0001).
           05  FILLER REDEFINES ALNCNTAF.
               10  ALNCNTAA PIC  X(0001).
           05  ALNCNTAI PIC  9(6).
      *    -------------------------------
           05  ALNVOLAL PIC S9(0004) COMP.
           05  ALNVOLAF PIC  X(0001).
           05  FILLER REDEFINES ALNVOLAF.
               10  ALNVOLAA PIC  X(0001).
           05  ALNVOLAI PIC  9(11).
      *    -------------------------------
           05  ALFCNTAL PIC S9(0004) COMP.
           05  ALFCNTAF PIC  X(0001).
           05  FILLER REDEFINES ALFCNTAF.
               10  ALFCNTAA PIC  X(0001).
           05  ALFCNTAI PIC  X(0006).
      *    -------------------------------
           05  ALFPRMAL PIC S9(0004) COMP.
           05  ALFPRMAF PIC  X(0001).
           05  FILLER REDEFINES ALFPRMAF.
               10  ALFPRMAA PIC  X(0001).
           05  ALFPRMAI PIC  X(0009).
      *    -------------------------------
           05  ALFAMTAL PIC S9(0004) COMP.
           05  ALFAMTAF PIC  X(0001).
           05  FILLER REDEFINES ALFAMTAF.
               10  ALFAMTAA PIC  X(0001).
           05  ALFAMTAI PIC  X(0011).
      *    -------------------------------
           05  AAHCNTAL PIC S9(0004) COMP.
           05  AAHCNTAF PIC  X(0001).
           05  FILLER REDEFINES AAHCNTAF.
               10  AAHCNTAA PIC  X(0001).
           05  AAHCNTAI PIC  X(0006).
      *    -------------------------------
           05  AAHPRMAL PIC S9(0004) COMP.
           05  AAHPRMAF PIC  X(0001).
           05  FILLER REDEFINES AAHPRMAF.
               10  AAHPRMAA PIC  X(0001).
           05  AAHPRMAI PIC  X(0009).
      *    -------------------------------
           05  AAHAMTAL PIC S9(0004) COMP.
           05  AAHAMTAF PIC  X(0001).
           05  FILLER REDEFINES AAHAMTAF.
               10  AAHAMTAA PIC  X(0001).
           05  AAHAMTAI PIC  X(0011).
      *    -------------------------------
           05  AMO11L PIC S9(0004) COMP.
           05  AMO11F PIC  X(0001).
           05  FILLER REDEFINES AMO11F.
               10  AMO11A PIC  X(0001).
           05  AMO11I PIC  X(0002).
      *    -------------------------------
           05  ALNCNTBL PIC S9(0004) COMP.
           05  ALNCNTBF PIC  X(0001).
           05  FILLER REDEFINES ALNCNTBF.
               10  ALNCNTBA PIC  X(0001).
           05  ALNCNTBI PIC  9(6).
      *    -------------------------------
           05  ALNVOLBL PIC S9(0004) COMP.
           05  ALNVOLBF PIC  X(0001).
           05  FILLER REDEFINES ALNVOLBF.
               10  ALNVOLBA PIC  X(0001).
           05  ALNVOLBI PIC  9(11).
      *    -------------------------------
           05  ALFCNTBL PIC S9(0004) COMP.
           05  ALFCNTBF PIC  X(0001).
           05  FILLER REDEFINES ALFCNTBF.
               10  ALFCNTBA PIC  X(0001).
           05  ALFCNTBI PIC  X(0006).
      *    -------------------------------
           05  ALFPRMBL PIC S9(0004) COMP.
           05  ALFPRMBF PIC  X(0001).
           05  FILLER REDEFINES ALFPRMBF.
               10  ALFPRMBA PIC  X(0001).
           05  ALFPRMBI PIC  X(0009).
      *    -------------------------------
           05  ALFAMTBL PIC S9(0004) COMP.
           05  ALFAMTBF PIC  X(0001).
           05  FILLER REDEFINES ALFAMTBF.
               10  ALFAMTBA PIC  X(0001).
           05  ALFAMTBI PIC  X(0011).
      *    -------------------------------
           05  AAHCNTBL PIC S9(0004) COMP.
           05  AAHCNTBF PIC  X(0001).
           05  FILLER REDEFINES AAHCNTBF.
               10  AAHCNTBA PIC  X(0001).
           05  AAHCNTBI PIC  X(0006).
      *    -------------------------------
           05  AAHPRMBL PIC S9(0004) COMP.
           05  AAHPRMBF PIC  X(0001).
           05  FILLER REDEFINES AAHPRMBF.
               10  AAHPRMBA PIC  X(0001).
           05  AAHPRMBI PIC  X(0009).
      *    -------------------------------
           05  AAHAMTBL PIC S9(0004) COMP.
           05  AAHAMTBF PIC  X(0001).
           05  FILLER REDEFINES AAHAMTBF.
               10  AAHAMTBA PIC  X(0001).
           05  AAHAMTBI PIC  X(0011).
      *    -------------------------------
           05  AMOCL PIC S9(0004) COMP.
           05  AMOCF PIC  X(0001).
           05  FILLER REDEFINES AMOCF.
               10  AMOCA PIC  X(0001).
           05  AMOCI PIC  X(0002).
      *    -------------------------------
           05  ALNCNTCL PIC S9(0004) COMP.
           05  ALNCNTCF PIC  X(0001).
           05  FILLER REDEFINES ALNCNTCF.
               10  ALNCNTCA PIC  X(0001).
           05  ALNCNTCI PIC  9(6).
      *    -------------------------------
           05  ALNVOLCL PIC S9(0004) COMP.
           05  ALNVOLCF PIC  X(0001).
           05  FILLER REDEFINES ALNVOLCF.
               10  ALNVOLCA PIC  X(0001).
           05  ALNVOLCI PIC  9(11).
      *    -------------------------------
           05  ALFCNTCL PIC S9(0004) COMP.
           05  ALFCNTCF PIC  X(0001).
           05  FILLER REDEFINES ALFCNTCF.
               10  ALFCNTCA PIC  X(0001).
           05  ALFCNTCI PIC  X(0006).
      *    -------------------------------
           05  ALFPRMCL PIC S9(0004) COMP.
           05  ALFPRMCF PIC  X(0001).
           05  FILLER REDEFINES ALFPRMCF.
               10  ALFPRMCA PIC  X(0001).
           05  ALFPRMCI PIC  X(0009).
      *    -------------------------------
           05  ALFAMTCL PIC S9(0004) COMP.
           05  ALFAMTCF PIC  X(0001).
           05  FILLER REDEFINES ALFAMTCF.
               10  ALFAMTCA PIC  X(0001).
           05  ALFAMTCI PIC  X(0011).
      *    -------------------------------
           05  AAHCNTCL PIC S9(0004) COMP.
           05  AAHCNTCF PIC  X(0001).
           05  FILLER REDEFINES AAHCNTCF.
               10  AAHCNTCA PIC  X(0001).
           05  AAHCNTCI PIC  X(0006).
      *    -------------------------------
           05  AAHPRMCL PIC S9(0004) COMP.
           05  AAHPRMCF PIC  X(0001).
           05  FILLER REDEFINES AAHPRMCF.
               10  AAHPRMCA PIC  X(0001).
           05  AAHPRMCI PIC  X(0009).
      *    -------------------------------
           05  AAHAMTCL PIC S9(0004) COMP.
           05  AAHAMTCF PIC  X(0001).
           05  FILLER REDEFINES AAHAMTCF.
               10  AAHAMTCA PIC  X(0001).
           05  AAHAMTCI PIC  X(0011).
      *    -------------------------------
           05  AERRMSGL PIC S9(0004) COMP.
           05  AERRMSGF PIC  X(0001).
           05  FILLER REDEFINES AERRMSGF.
               10  AERRMSGA PIC  X(0001).
           05  AERRMSGI PIC  X(0077).
      *    -------------------------------
           05  APFENTRL PIC S9(0004) COMP.
           05  APFENTRF PIC  X(0001).
           05  FILLER REDEFINES APFENTRF.
               10  APFENTRA PIC  X(0001).
           05  APFENTRI PIC  9(2).
       01  EL610AO REDEFINES EL610AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARHDGO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRPHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTHDGO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOFCRO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APRINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMO1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNCNT1O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNVOL1O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCNT1O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPRM1O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFAMT1O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCNT1O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHPRM1O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHAMT1O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMO2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNCNT2O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNVOL2O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCNT2O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPRM2O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFAMT2O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCNT2O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHPRM2O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHAMT2O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMO3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNCNT3O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNVOL3O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCNT3O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPRM3O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFAMT3O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCNT3O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHPRM3O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHAMT3O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMO4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNCNT4O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNVOL4O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCNT4O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPRM4O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFAMT4O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCNT4O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHPRM4O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHAMT4O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMO5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNCNT5O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNVOL5O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCNT5O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPRM5O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFAMT5O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCNT5O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHPRM5O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHAMT5O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMO6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNCNT6O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNVOL6O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCNT6O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPRM6O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFAMT6O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCNT6O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHPRM6O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHAMT6O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMO7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNCNT7O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNVOL7O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCNT7O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPRM7O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFAMT7O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCNT7O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHPRM7O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHAMT7O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMO8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNCNT8O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNVOL8O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCNT8O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPRM8O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFAMT8O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCNT8O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHPRM8O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHAMT8O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMO9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNCNT9O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNVOL9O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCNT9O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPRM9O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFAMT9O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCNT9O PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHPRM9O PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHAMT9O PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMOAO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNCNTAO PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNVOLAO PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCNTAO PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPRMAO PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFAMTAO PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCNTAO PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHPRMAO PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHAMTAO PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMO11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNCNTBO PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNVOLBO PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCNTBO PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPRMBO PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFAMTBO PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCNTBO PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHPRMBO PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHAMTBO PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMOCO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNCNTCO PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALNVOLCO PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCNTCO PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPRMCO PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFAMTCO PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCNTCO PIC  ZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHPRMCO PIC  Z,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHAMTCO PIC  ZZZ,ZZZ,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AERRMSGO PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFENTRO PIC  X(0002).
      *    -------------------------------
00193
00194      EJECT
00195
00196  01  FILLER REDEFINES EL610AO.
110105     12  FILLER                      PIC X(135).
00197 *    12  FILLER                      PIC X(133).
00198      12  ALOAN-OFC-INFO OCCURS 12 TIMES.
00199          16  AMO-LENGTH              PIC S9(4) COMP.
00200          16  AMO-ATTRB               PIC X.
00201          16  AMO                     PIC 99.
00202          16  ALN-CNT-LENGTH          PIC S9(4) COMP.
00203          16  ALN-CNT-ATTRB           PIC X.
00204          16  ALN-CNT-IN              PIC 9(6).
00205          16  ALN-CNT-OUT REDEFINES
00206              ALN-CNT-IN              PIC ZZ,ZZZ.
00207          16  ALN-VOL-LENGTH          PIC S9(4) COMP.
00208          16  ALN-VOL-ATTRB           PIC X.
00209          16  ALN-VOL-IN              PIC 9(11).
00210          16  ALN-VOL-OUT REDEFINES
00211              ALN-VOL-IN              PIC ZZZ,ZZZ,ZZZ.
00212          16  ALF-CNT-LENGTH          PIC S9(4) COMP.
00213          16  ALF-CNT-ATTRB           PIC X.
00214          16  ALF-CNT-IN              PIC 9(6).
00215          16  ALF-CNT-OUT REDEFINES
00216              ALF-CNT-IN              PIC ZZ,ZZZ.
00217          16  ALF-PRM-LENGTH          PIC S9(4) COMP.
00218          16  ALF-PRM-ATTRB           PIC X.
00219          16  ALF-PRM-IN              PIC 9(9).
00220          16  ALF-PRM-OUT REDEFINES
00221              ALF-PRM-IN              PIC Z,ZZZ,ZZZ.
00222          16  ALF-AMT-LENGTH          PIC S9(4) COMP.
00223          16  ALF-AMT-ATTRB           PIC X.
00224          16  ALF-AMT-IN              PIC 9(11).
00225          16  ALF-AMT-OUT REDEFINES
00226              ALF-AMT-IN              PIC ZZZ,ZZZ,ZZZ.
00227          16  AAH-CNT-LENGTH          PIC S9(4) COMP.
00228          16  AAH-CNT-ATTRB           PIC X.
00229          16  AAH-CNT-IN              PIC 9(6).
00230          16  AAH-CNT-OUT REDEFINES
00231              AAH-CNT-IN              PIC ZZ,ZZZ.
00232          16  AAH-PRM-LENGTH          PIC S9(4) COMP.
00233          16  AAH-PRM-ATTRB           PIC X.
00234          16  AAH-PRM-IN              PIC 9(9).
00235          16  AAH-PRM-OUT REDEFINES
00236              AAH-PRM-IN              PIC Z,ZZZ,ZZZ.
00237          16  AAH-AMT-LENGTH          PIC S9(4) COMP.
00238          16  AAH-AMT-ATTRB           PIC X.
00239          16  AAH-AMT-IN              PIC 9(11).
00240          16  AAH-AMT-OUT REDEFINES
00241              AAH-AMT-IN              PIC ZZZ,ZZZ,ZZZ.
00242
00243      EJECT
00244
00245  01  WS-OFFICER-INFO.
00246      12  FILLER            OCCURS 12 TIMES.
00247          16  WS-LN-CNT                  PIC 9(5)   COMP-3.
00248          16  WS-LN-VOL                  PIC 9(9)   COMP-3.
00249          16  WS-LF-CNT                  PIC 9(5)   COMP-3.
00250          16  WS-LF-PRM                  PIC 9(7)   COMP-3.
00251          16  WS-LF-AMT                  PIC 9(9)   COMP-3.
00252          16  WS-AH-CNT                  PIC 9(5)   COMP-3.
00253          16  WS-AH-PRM                  PIC 9(7)   COMP-3.
00254          16  WS-AH-AMT                  PIC 9(9)   COMP-3.
00255
00256      EJECT
00257
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
00259  01  DFHCOMMAREA             PIC X(1024).
00260
00261      EJECT
00262
00263 *01 PARMLIST .
00264 *    12  FILLER                      PIC S9(8)   COMP.
00265 *    12  ERLOFC-POINTER              PIC S9(8)   COMP.
00266 *    12  ERACCT-POINTER              PIC S9(8)   COMP.
00267
00268      EJECT
00269
00270 *COPY ERCLOFC.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCLOFC                             *
00003 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00004 *                            VMOD=2.003                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = LOAN OFFICER CONTROLS                     *
00007 *                                                                *
00008 *   FILE TYPE = VSAM,KSDS                                        *
00009 *   RECORD SIZE = 670   RECFORM = FIX                            *
00010 *                                                                *
00011 *   BASE CLUSTER NAME = ERLOFC                   RKP=2,LEN=25    *
00012 *                                                                *
00013 *   LOG = YES                                                    *
00014 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00015 *                                                                *
00016 ******************************************************************
00017
00018  01  LOAN-OFFICER-MASTER.
00019      12  LO-RECORD-ID                PIC XX.
00020          88  VALID-LO-ID                VALUE 'LO'.
00021
00022      12  LO-CONTROL-PRIMARY.
00023          16  LO-COMPANY-CD           PIC X.
00024          16  LO-CARRIER              PIC X.
00025          16  LO-GROUPING.
00026              20  LO-GROUPING-PREFIX  PIC XXX.
00027              20  LO-GROUPING-PRIME   PIC XXX.
00028          16  LO-STATE                PIC XX.
00029          16  LO-ACCOUNT.
00030              20  LO-ACCOUNT-PREFIX   PIC X(4).
00031              20  LO-ACCOUNT-PRIME    PIC X(6).
00032          16  LO-OFFICER-CODE         PIC X(5).
00033
00034      12  LO-OFFICER-NAME             PIC X(30).
00035
00036      12  LO-LAST-MAINT-DT            PIC XX.
00037      12  LO-LAST-USER                PIC X(4).
00038      12  LO-LAST-MAINT-HHMMSS        PIC S9(6) COMP-3.
00039
00040      12  LO-COMP-CONTROL             PIC X.
00041              88  LO-SHOW-COMP           VALUE 'Y'.
00042              88  LO-SUPPRESS-COMP       VALUE 'N'.
00043      12  LO-DETAIL-CONTROL           PIC X.
00044              88  LO-PRINT-DETAIL        VALUE 'D'.
00045              88  LO-PRINT-SUMMARY       VALUE 'S'.
00046
00047      12  LO-SV-CARRIER               PIC X.
00048      12  LO-SV-GROUPING              PIC X(6).
00049      12  LO-SV-STATE                 PIC XX.
00050
00051      12  LO-PROCESS-MO-YR            PIC 9(7)  COMP-3.
00052      12  LO-TOTAL-COMMISSION         PIC S9(9)V99 COMP-3.
00053
00054      12  LO-OFFICER-INFO.
00055          16  FILLER OCCURS 12 TIMES.
00056              20  LO-LOAN-COUNT       PIC S9(5) COMP-3.
00057              20  LO-LOAN-VOLUME      PIC S9(9) COMP-3.
00058              20  LO-LF-COUNT         PIC S9(5) COMP-3.
00059              20  LO-LF-PREM          PIC S9(7) COMP-3.
00060              20  LO-LF-BENEFIT       PIC S9(9) COMP-3.
00061              20  LO-AH-COUNT         PIC S9(5) COMP-3.
00062              20  LO-AH-PREM          PIC S9(7) COMP-3.
00063              20  LO-AH-BENEFIT       PIC S9(9) COMP-3.
110105     12  FILLER                      PIC X(198).
00064 ******************************************************************
00271
00272      EJECT
00273 *COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
090803     12  FILLER                            PIC X(5).
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
00274
00275      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                LOAN-OFFICER-MASTER ACCOUNT-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL610' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00277
00278      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00279      MOVE '5'                    TO DC-OPTION-CODE.
00280      PERFORM 9700-DATE-LINK.
00281      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00282      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00283
00284      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00285      MOVE 1                      TO EMI-NUMBER-OF-LINES.
00286      MOVE EIBTRMID               TO QID-TERM.
00287
00288      IF EIBCALEN = 0
00289          GO TO 8800-UNAUTHORIZED-ACCESS.
00290
00291      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00292          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00293              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00294              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00295              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00296              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00297              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00298              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00299              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00300              MOVE THIS-PGM TO PI-CALLING-PROGRAM
00301          ELSE
00302              MOVE PI-CALLING-PROGRAM   TO RETURN-FROM
00303              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00304              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00305              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00306              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00307              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00308              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00309              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00310              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00311
00312      
      * EXEC CICS    HANDLE    CONDITION
00313 *         PGMIDERR          (9600-PGMID-ERROR)
00314 *         ERROR             (9990-ABEND)
00315 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00002829' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032383239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00316
00317      IF  EIBTRNID NOT = TRANS-ID
00318          MOVE LOW-VALUES         TO EL610AI
00319          GO TO 8100-SEND-INITIAL-MAP.
00320
00321      IF EIBAID = DFHPF5
00322          MOVE LOW-VALUES         TO EL610AI
00323          MOVE 'A'                TO AMAINTO
00324          MOVE AL-UANON           TO AMAINTA
00325          GO TO 8100-SEND-INITIAL-MAP.
00326
00327      IF EIBAID = DFHCLEAR
00328          GO TO 9400-CLEAR.
00329
00330      IF PI-PROCESSOR-ID = 'LGXX'
00331          GO TO 0200-RECEIVE.
00332
00333      
      * EXEC CICS READQ TS
00334 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00335 *        INTO   (SECURITY-CONTROL)
00336 *        LENGTH (SC-COMM-LENGTH)
00337 *        ITEM   (SC-ITEM)
00338 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00002850' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00339
00340      MOVE SC-CREDIT-DISPLAY (10)  TO PI-DISPLAY-CAP.
00341      MOVE SC-CREDIT-UPDATE  (10)  TO PI-MODIFY-CAP.
00342
00343      IF NOT DISPLAY-CAP
00344          MOVE 'READ'          TO SM-READ
00345          PERFORM 9995-SECURITY-VIOLATION
00346          MOVE ER-0070         TO  EMI-ERROR
00347          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00348          GO TO 8100-SEND-INITIAL-MAP.
00349
00350      EJECT
00351
00352  0200-RECEIVE.
00353      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00354          MOVE ER-7008            TO EMI-ERROR
00355          PERFORM 9900-ERROR-FORMAT
00356          MOVE -1                 TO AMAINTL
00357          GO TO 8200-SEND-DATAONLY.
00358
00359      
      * EXEC CICS RECEIVE
00360 *        MAP      (MAP-NAME)
00361 *        MAPSET   (MAPSET-NAME)
00362 *        INTO     (EL610AI)
00363 *    END-EXEC.
           MOVE LENGTH OF
            EL610AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002876' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL610AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00364
00365      IF  APFENTRL = 0
00366          GO TO 0300-CHECK-PFKEYS.
00367
00368      IF  EIBAID NOT = DFHENTER
00369          MOVE ER-0004            TO EMI-ERROR
00370          GO TO 0320-INPUT-ERROR.
00371
00372      IF  APFENTRI NUMERIC
00373          IF  APFENTRI = '01' OR '02' OR '03' OR '04' OR
00374                         '12' OR '23' OR '24'
00375              MOVE PF-VALUES (APFENTRI) TO EIBAID
00376          ELSE
00377              MOVE ER-0029        TO EMI-ERROR
00378              GO TO 0320-INPUT-ERROR.
00379
00380  0300-CHECK-PFKEYS.
00381      IF EIBAID = DFHPF23
00382          GO TO 8810-PF23.
00383
00384      IF EIBAID = DFHPF24
00385          GO TO 9200-RETURN-MAIN-MENU.
00386
00387      IF EIBAID = DFHPF12
00388          GO TO 9500-PF12.
00389
00390      IF  EIBAID = DFHPF1 OR DFHPF2 OR DFHPF3 OR DFHPF4
00391          IF  AMAINTL GREATER THAN +0
00392              MOVE -1             TO  AMAINTL
00393              MOVE  ER-0050       TO  EMI-ERROR
00394              PERFORM 9900-ERROR-FORMAT
00395              GO TO 8200-SEND-DATAONLY.
00396
00397      IF  EIBAID = DFHPF1
00398          PERFORM 7000-BROWSE-FWRD-NEXT-ACCOUNT
00399          GO TO 3000-SHOW-LOAN-OFFICER.
00400
00401      IF  EIBAID = DFHPF2
00402          PERFORM 7100-BROWSE-BWRD-NEXT-ACCOUNT
00403          GO TO 3000-SHOW-LOAN-OFFICER.
00404
00405      IF  EIBAID = DFHPF3
00406          PERFORM 7200-BROWSE-FWRD-NEXT-OFFICER
00407          GO TO 3000-SHOW-LOAN-OFFICER.
00408
00409      IF  EIBAID = DFHPF4
00410          PERFORM 7300-BROWSE-BWRD-NEXT-OFFICER
00411          GO TO 3000-SHOW-LOAN-OFFICER.
00412
00413      IF EIBAID = DFHENTER
00414          GO TO 0400-EDIT-INPUT-DATA.
00415
00416      MOVE ER-0029                TO EMI-ERROR.
00417
00418  0320-INPUT-ERROR.
00419      PERFORM 9900-ERROR-FORMAT.
00420      MOVE AL-UNBON               TO APFENTRA.
00421      IF APFENTRL = 0
00422          MOVE -1                 TO AMAINTL
00423      ELSE
00424          MOVE -1                 TO APFENTRL.
00425
00426      GO TO 8200-SEND-DATAONLY.
00427
00428      EJECT
00429
00430  0400-EDIT-INPUT-DATA.
00431      IF AMAINTI = 'A' OR 'C' OR 'D' OR 'S' OR 'K'
00432         NEXT SENTENCE
00433        ELSE
00434         MOVE  ER-0023            TO EMI-ERROR
00435         MOVE -1                  TO AMAINTL
00436         MOVE AL-UABON            TO AMAINTA
00437         PERFORM 9900-ERROR-FORMAT
00438         GO TO 8200-SEND-DATAONLY.
00439
00440      IF NOT MODIFY-CAP AND
00441         (AMAINTI = 'A' OR 'C' OR 'D' OR 'K')
00442          MOVE 'UPDATE'       TO SM-READ
00443          PERFORM 9995-SECURITY-VIOLATION
00444          MOVE ER-0070        TO EMI-ERROR
00445          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00446          GO TO 8100-SEND-INITIAL-MAP.
00447
00448      IF AACCTL GREATER THAN ZEROS
00449          MOVE AL-UANON           TO AACCTA
00450          MOVE AACCTI             TO PI-CR-ACCOUNT
00451      ELSE
00452          MOVE -1                 TO AACCTL
00453          MOVE AL-UABON           TO AACCTA
00454          MOVE  ER-0197           TO EMI-ERROR
00455          PERFORM 9900-ERROR-FORMAT.
00456
00457      IF ACARRL GREATER THAN ZEROS
00458          MOVE AL-UANON           TO ACARRA
00459          MOVE ACARRI             TO PI-CR-CARRIER
00460      ELSE
00461          IF NOT ST-ACCNT-CNTL AND NOT ACCNT-CNTL
00462              MOVE -1             TO ACARRL
00463              MOVE AL-UABON       TO ACARRA
00464              MOVE  ER-0194       TO EMI-ERROR
00465              PERFORM 9900-ERROR-FORMAT.
00466
00467      IF AGRPL GREATER THAN ZEROS
00468          MOVE AL-UANON           TO AGRPA
00469          MOVE AGRPI              TO PI-CR-GROUPING
00470      ELSE
00471          IF CARR-GROUP-ST-ACCNT-CNTL
00472              MOVE -1             TO AGRPL
00473              MOVE AL-UABON       TO AGRPA
00474              MOVE  ER-0195       TO EMI-ERROR
00475              PERFORM 9900-ERROR-FORMAT.
00476
00477      IF ASTL GREATER THAN ZEROS
00478          MOVE AL-UANON           TO ASTA
00479          MOVE ASTI               TO PI-CR-STATE
00480      ELSE
00481          IF NOT ACCNT-CNTL AND NOT CARR-ACCNT-CNTL
00482              MOVE -1             TO ASTL
00483              MOVE AL-UABON       TO ASTA
00484              MOVE  ER-0196       TO EMI-ERROR
00485              PERFORM 9900-ERROR-FORMAT.
00486
00487      IF AOFCRL GREATER THAN +0
00488         MOVE AL-UANON            TO AOFCRA
00489         MOVE AOFCRI              TO PI-LOAN-OFFICER
00490       ELSE
00491         MOVE SPACES              TO PI-LOAN-OFFICER.
00492
00493      IF ACOMPL GREATER THAN +0
00494         AND ACOMPI = 'Y' OR 'N'   NEXT SENTENCE
00495        ELSE
00496         MOVE -1                  TO ACOMPL
00497         MOVE ER-3002             TO EMI-ERROR
00498         PERFORM 9900-ERROR-FORMAT.
00499
00500      IF APRINTL GREATER THAN +0
00501         AND APRINTI = 'D' OR 'S'   NEXT SENTENCE
00502        ELSE
00503         MOVE   ER-3003           TO EMI-ERROR
00504         MOVE -1                  TO APRINTL
00505         PERFORM 9900-ERROR-FORMAT.
00506
00507      IF EMI-ERROR = ZEROS NEXT SENTENCE
00508        ELSE
00509         GO TO 8200-SEND-DATAONLY.
00510
00511      IF  AMAINTI = 'D' OR 'S'
00512          GO 0450-CHECK-MAINT.
00513
00514      PERFORM 7800-READ-ACCOUNT-MASTER.
00515
00516      IF EMI-ERROR = ZEROS NEXT SENTENCE
00517        ELSE
00518         GO TO 8200-SEND-DATAONLY.
00519
00520      EJECT
00521
00522  0450-CHECK-MAINT.
00523      IF AMAINTI = 'A'
00524         GO TO 1000-ADD-LOAN-OFFICER.
00525
00526      IF AMAINTI = 'C'
00527         GO TO 2000-CHANGE-LOAN-OFFICER.
00528
00529      IF AMAINTI = 'D'
00530         GO TO 2500-DELETE-LOAN-OFFICER.
00531
00532      IF AMAINTI = 'S'
00533         IF PI-LOAN-OFFICER = SPACES
00534            PERFORM 7200-BROWSE-FWRD-NEXT-OFFICER
00535            GO TO 3000-SHOW-LOAN-OFFICER
00536        ELSE
00537            GO TO 3000-SHOW-LOAN-OFFICER.
00538
00539      GO TO 3500-REWRITE-KEY.
00540
00541      EJECT
00542
00543  1000-ADD-LOAN-OFFICER           SECTION.
00544      MOVE PI-COMPANY-CD          TO ERLOFC-COMPANY-CD.
00545      MOVE PI-CR-CARRIER          TO ERLOFC-CARRIER.
00546      MOVE PI-CR-GROUPING         TO ERLOFC-GROUPING.
00547      MOVE PI-CR-STATE            TO ERLOFC-STATE.
00548      MOVE PI-CR-ACCOUNT          TO ERLOFC-ACCOUNT.
00549      MOVE PI-LOAN-OFFICER        TO ERLOFC-OFFICER-CODE.
00550
00551      PERFORM 7400-READ-LOAN-MASTER.
00552
00553      IF OFFICER-WAS-NOT-FOUND
00554         GO TO 1100-PROCESS-NEW-OFFICER.
00555
00556      IF CARR-GROUP-ST-ACCNT-CNTL
00557          MOVE -1                 TO ACARRL
00558      ELSE
00559          IF ST-ACCNT-CNTL
00560              MOVE -1             TO ASTHDGL
00561          ELSE
00562              IF CARR-ST-ACCNT-CNTL
00563                  MOVE -1         TO ACARRL
00564              ELSE
00565                  IF ACCNT-CNTL
00566                      MOVE -1     TO   AACCTL
00567                  ELSE
00568                      IF CARR-ACCNT-CNTL
00569                          MOVE -1 TO ACARRL.
00570
00571      MOVE  ER-0132               TO  EMI-ERROR.
00572      PERFORM 9900-ERROR-FORMAT.
00573      GO TO 8200-SEND-DATAONLY.
00574
00575  1100-PROCESS-NEW-OFFICER.
00576      MOVE +0                     TO WS-SUB1.
00577
00578  1200-INITIALIZE-COUNTS.
00579      ADD +1                      TO WS-SUB1.
00580
00581      MOVE ZERO                  TO WS-LN-VOL  (WS-SUB1)
00582                                    WS-LN-CNT  (WS-SUB1)
00583                                    WS-LF-CNT  (WS-SUB1)
00584                                    WS-LF-PRM  (WS-SUB1)
00585                                    WS-LF-AMT  (WS-SUB1)
00586                                    WS-AH-CNT  (WS-SUB1)
00587                                    WS-AH-PRM  (WS-SUB1)
00588                                    WS-AH-AMT  (WS-SUB1).
00589
00590      IF WS-SUB1 NOT = +12
00591         GO TO 1200-INITIALIZE-COUNTS.
00592
00593      IF  ANAMEL GREATER THAN +0
00594          NEXT SENTENCE
00595         ELSE
00596          MOVE -1                 TO ANAMEL
00597          MOVE  ER-3016           TO EMI-ERROR
00598          PERFORM 9900-ERROR-FORMAT.
00599
00600      PERFORM 6500-EDIT-LOAN-DATA.
00601
00602      IF  EMI-ERROR = ZEROS NEXT SENTENCE
00603         ELSE
00604          GO TO 8200-SEND-DATAONLY.
00605
00606 *    IF  SCREEN-WAS-KEYED
00607 *        GO TO 1710-ADD-LOAN-OFFICER.
00608
00609 *    MOVE -1                     TO ACARRL.
00610 *    MOVE  ER-3004               TO EMI-ERROR.
00611 *    PERFORM 9900-ERROR-FORMAT.
00612
00613  1710-ADD-LOAN-OFFICER.
00614      
      * EXEC CICS GETMAIN
00615 *       SET      (ADDRESS OF LOAN-OFFICER-MASTER)
00616 *       LENGTH   (ERLOFC-RECORD-LENGTH)
00617 *       INITIMG  (GETMAIN-SPACE)
00618 *    END-EXEC.
      *    MOVE ',"IL                  $   #00003131' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERLOFC-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF LOAN-OFFICER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00619
00620      MOVE  SPACES                TO LOAN-OFFICER-MASTER.
00621
00622      MOVE  'LO'                  TO LO-RECORD-ID.
00623
00624      MOVE  ERLOFC-KEY            TO LO-CONTROL-PRIMARY.
00625
00626      MOVE  ANAMEI                TO LO-OFFICER-NAME.
00627      MOVE  PI-PROCESSOR-ID       TO LO-LAST-USER.
00628      MOVE  SAVE-BIN-DATE         TO LO-LAST-MAINT-DT.
00629      MOVE  EIBTIME               TO LO-LAST-MAINT-HHMMSS.
00630      MOVE  ACOMPI                TO LO-COMP-CONTROL.
00631      MOVE  APRINTI               TO LO-DETAIL-CONTROL.
00632
00633      MOVE  AM-CARRIER            TO LO-SV-CARRIER.
00634      MOVE  AM-GROUPING           TO LO-SV-GROUPING.
00635      MOVE  AM-STATE              TO LO-SV-STATE.
00636
00637      MOVE  WS-OFFICER-INFO       TO LO-OFFICER-INFO.
00638
00639      MOVE 'A'                    TO  JP-RECORD-TYPE.
00640      MOVE LOAN-OFFICER-MASTER    TO  JP-RECORD-AREA.
00641
00642      PERFORM 7500-WRITE-LOAN-MASTER.
00643
00644      PERFORM 8400-LOG-JOURNAL-RECORD.
00645
00646      IF  EMI-ERROR = ZEROS NEXT SENTENCE
00647         ELSE
00648          GO TO 3000-SHOW-LOAN-OFFICER.
00649
00650      MOVE    ER-0000             TO EMI-ERROR.
00651      PERFORM 9900-ERROR-FORMAT.
00652      GO TO 8100-SEND-INITIAL-MAP.
00653
00654  1900-EXIT.
00655      EXIT.
00656
00657      EJECT
00658  2000-CHANGE-LOAN-OFFICER SECTION.
00659      MOVE PI-COMPANY-CD          TO ERLOFC-COMPANY-CD.
00660      MOVE PI-CR-CARRIER          TO ERLOFC-CARRIER.
00661      MOVE PI-CR-GROUPING         TO ERLOFC-GROUPING.
00662      MOVE PI-CR-STATE            TO ERLOFC-STATE.
00663      MOVE PI-CR-ACCOUNT          TO ERLOFC-ACCOUNT.
00664      MOVE PI-LOAN-OFFICER        TO ERLOFC-OFFICER-CODE.
00665
00666      IF ERLOFC-KEY = PI-CRLOFC-KEY
00667         NEXT SENTENCE
00668        ELSE
00669         GO TO 2380-CHANGE-ERROR.
00670
00671      PERFORM 7460-READ-LOAN-MST-UPDT.
00672
00673      IF  OFFICER-WAS-FOUND  NEXT SENTENCE
00674         ELSE
00675          GO TO 2390-OFFICER-NOT-FOUND.
00676
00677      MOVE 'C'                    TO JP-RECORD-TYPE.
00678      MOVE LOAN-OFFICER-MASTER    TO JP-RECORD-AREA.
00679
00680      PERFORM 8400-LOG-JOURNAL-RECORD.
00681
00682      IF  ANAMEL GREATER THAN +0
00683          MOVE ANAMEI             TO LO-OFFICER-NAME.
00684      IF  ACOMPL GREATER THAN +0
00685          MOVE ACOMPI             TO LO-COMP-CONTROL.
00686      IF  APRINTL GREATER THAN +0
00687          MOVE APRINTI            TO LO-DETAIL-CONTROL.
00688
00689      MOVE LO-OFFICER-INFO        TO WS-OFFICER-INFO.
00690
00691      PERFORM 6500-EDIT-LOAN-DATA.
00692
00693      IF  EMI-ERROR = ZEROS NEXT SENTENCE
00694         ELSE
00695          GO TO 8200-SEND-DATAONLY.
00696
00697      MOVE WS-OFFICER-INFO        TO LO-OFFICER-INFO.
00698
00699      MOVE  PI-PROCESSOR-ID       TO LO-LAST-USER.
00700      MOVE  SAVE-BIN-DATE         TO LO-LAST-MAINT-DT.
00701      MOVE  EIBTIME               TO LO-LAST-MAINT-HHMMSS.
00702
00703      MOVE 'C'                    TO JP-RECORD-TYPE.
00704      MOVE LOAN-OFFICER-MASTER    TO JP-RECORD-AREA.
00705
00706      PERFORM 7600-REWRITE-LOAN-MASTER.
00707
00708      PERFORM 8400-LOG-JOURNAL-RECORD.
00709
00710      GO TO 3000-SHOW-LOAN-OFFICER.
00711
00712  2380-CHANGE-ERROR.
00713      IF CARR-GROUP-ST-ACCNT-CNTL
00714          MOVE -1                 TO ACARRL
00715      ELSE
00716          IF ST-ACCNT-CNTL
00717              MOVE -1             TO ASTHDGL
00718          ELSE
00719              IF CARR-ST-ACCNT-CNTL
00720                  MOVE -1         TO ACARRL
00721              ELSE
00722                  IF ACCNT-CNTL
00723                      MOVE -1     TO   AACCTL
00724                  ELSE
00725                      IF CARR-ACCNT-CNTL
00726                          MOVE -1 TO ACARRL.
00727
00728      MOVE  ER-3005               TO EMI-ERROR.
00729      PERFORM 9900-ERROR-FORMAT.
00730      GO TO 8200-SEND-DATAONLY.
00731
00732  2390-OFFICER-NOT-FOUND.
00733      IF CARR-GROUP-ST-ACCNT-CNTL
00734          MOVE -1                 TO ACARRL
00735      ELSE
00736          IF ST-ACCNT-CNTL
00737              MOVE -1             TO ASTHDGL
00738          ELSE
00739              IF CARR-ST-ACCNT-CNTL
00740                  MOVE -1         TO ACARRL
00741              ELSE
00742                  IF ACCNT-CNTL
00743                      MOVE -1     TO   AACCTL
00744                  ELSE
00745                      IF CARR-ACCNT-CNTL
00746                          MOVE -1 TO ACARRL.
00747
00748      MOVE  ER-3006               TO EMI-ERROR.
00749      PERFORM 9900-ERROR-FORMAT.
00750      GO TO 8200-SEND-DATAONLY.
00751
00752  2400-EXIT.
00753      EXIT.
00754
00755      EJECT
00756
00757  2500-DELETE-LOAN-OFFICER  SECTION.
00758      
      * EXEC CICS HANDLE CONDITION
00759 *        NOTFND   (2890-OFFICER-NOT-FOUND)
00760 *    END-EXEC.
      *    MOVE '"$I                   ! # #00003275' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033323735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00761
00762      MOVE PI-COMPANY-CD          TO ERLOFC-COMPANY-CD.
00763      MOVE PI-CR-CARRIER          TO ERLOFC-CARRIER.
00764      MOVE PI-CR-GROUPING         TO ERLOFC-GROUPING.
00765      MOVE PI-CR-STATE            TO ERLOFC-STATE.
00766      MOVE PI-CR-ACCOUNT          TO ERLOFC-ACCOUNT.
00767      MOVE PI-LOAN-OFFICER        TO ERLOFC-OFFICER-CODE.
00768
00769      IF ERLOFC-KEY = PI-CRLOFC-KEY
00770         NEXT SENTENCE
00771        ELSE
00772         GO TO 2880-DELETE-ERROR.
00773
00774      PERFORM 7460-READ-LOAN-MST-UPDT.
00775
00776      IF  OFFICER-WAS-FOUND  NEXT SENTENCE
00777         ELSE
00778          GO TO 2890-OFFICER-NOT-FOUND.
00779
00780      MOVE 'D'                    TO JP-RECORD-TYPE.
00781      MOVE LOAN-OFFICER-MASTER    TO JP-RECORD-AREA.
00782
00783      PERFORM 8400-LOG-JOURNAL-RECORD.
00784
00785      PERFORM 7700-DELETE-LOAN-MASTER.
00786
00787      MOVE 'D'                    TO PI-MAINT.
00788
00789      IF CARR-GROUP-ST-ACCNT-CNTL
00790          MOVE -1                 TO ACARRL
00791      ELSE
00792          IF ST-ACCNT-CNTL
00793              MOVE -1             TO ASTHDGL
00794          ELSE
00795              IF CARR-ST-ACCNT-CNTL
00796                  MOVE -1         TO ACARRL
00797              ELSE
00798                  IF ACCNT-CNTL
00799                      MOVE -1     TO   AACCTL
00800                  ELSE
00801                      IF CARR-ACCNT-CNTL
00802                          MOVE -1 TO ACARRL.
00803
00804      MOVE    ER-0000             TO EMI-ERROR.
00805      PERFORM  9900-ERROR-FORMAT.
00806      GO TO 8100-SEND-INITIAL-MAP.
00807
00808  2880-DELETE-ERROR.
00809      IF CARR-GROUP-ST-ACCNT-CNTL
00810          MOVE -1                 TO ACARRL
00811      ELSE
00812          IF ST-ACCNT-CNTL
00813              MOVE -1             TO ASTHDGL
00814          ELSE
00815              IF CARR-ST-ACCNT-CNTL
00816                  MOVE -1         TO ACARRL
00817              ELSE
00818                  IF ACCNT-CNTL
00819                      MOVE -1     TO   AACCTL
00820                  ELSE
00821                      IF CARR-ACCNT-CNTL
00822                          MOVE -1 TO ACARRL.
00823
00824      MOVE  ER-3005               TO EMI-ERROR.
00825      PERFORM 9900-ERROR-FORMAT.
00826      GO TO 8200-SEND-DATAONLY.
00827
00828  2890-OFFICER-NOT-FOUND.
00829      IF CARR-GROUP-ST-ACCNT-CNTL
00830          MOVE -1                 TO ACARRL
00831      ELSE
00832          IF ST-ACCNT-CNTL
00833              MOVE -1             TO ASTHDGL
00834          ELSE
00835              IF CARR-ST-ACCNT-CNTL
00836                  MOVE -1         TO ACARRL
00837              ELSE
00838                  IF ACCNT-CNTL
00839                      MOVE -1     TO   AACCTL
00840                  ELSE
00841                      IF CARR-ACCNT-CNTL
00842                          MOVE -1 TO ACARRL.
00843
00844      MOVE  ER-3006               TO EMI-ERROR.
00845      PERFORM 9900-ERROR-FORMAT.
00846      GO TO 8200-SEND-DATAONLY.
00847
00848  2900-EXIT.
00849      EXIT.
00850
00851      EJECT
00852  3000-SHOW-LOAN-OFFICER          SECTION.
00853      IF AMAINTI = 'S'
00854         MOVE PI-COMPANY-CD       TO ERLOFC-COMPANY-CD
00855         MOVE PI-CR-CARRIER       TO ERLOFC-CARRIER
00856         MOVE PI-CR-GROUPING      TO ERLOFC-GROUPING
00857         MOVE PI-CR-STATE         TO ERLOFC-STATE
00858         MOVE PI-CR-ACCOUNT       TO ERLOFC-ACCOUNT
00859         IF PI-LOAN-OFFICER NOT = SPACES
00860           MOVE PI-LOAN-OFFICER     TO ERLOFC-OFFICER-CODE.
00861
00862      PERFORM 7400-READ-LOAN-MASTER.
00863
00864      IF  OFFICER-WAS-NOT-FOUND
00865          GO TO 3350-OFFICER-NOT-FOUND.
00866
00867      MOVE AMAINTI                TO PI-MAINT.
00868      MOVE LOW-VALUE              TO AMAINTO.
00869      MOVE ERLOFC-KEY             TO PI-CRLOFC-KEY
00870                                     PI-PREV-CRLOFC-KEY.
00871
00872      MOVE ERLOFC-CARRIER         TO PI-CR-CARRIER.
00873      MOVE ERLOFC-GROUPING        TO PI-CR-GROUPING.
00874      MOVE ERLOFC-STATE           TO PI-CR-STATE
00875      MOVE ERLOFC-ACCOUNT         TO PI-CR-ACCOUNT.
00876      MOVE ERLOFC-OFFICER-CODE    TO PI-LOAN-OFFICER.
00877
00878      MOVE  LO-CARRIER            TO ACARRO.
00879      MOVE  LO-GROUPING           TO AGRPO.
00880      MOVE  LO-STATE              TO ASTO.
00881      MOVE  LO-ACCOUNT            TO AACCTO.
00882      MOVE  LO-OFFICER-CODE       TO AOFCRO.
00883      MOVE  LO-OFFICER-NAME       TO ANAMEO.
00884      MOVE  LO-COMP-CONTROL       TO ACOMPO.
00885      MOVE  LO-DETAIL-CONTROL     TO APRINTO.
00886
00887      MOVE AL-UANON               TO ACARRA
00888                                     AGRPA
00889                                     ASTA
00890                                     AACCTA
00891                                     AOFCRA.
00892
00893      MOVE +0                     TO WS-SUB1.
00894
00895  3050-PROCESS-OFFICER-INFO.
00896      ADD +1                      TO WS-SUB1
00897
00898      IF  WS-SUB1 GREATER THAN +12
00899          GO TO 3200-DISPLAY-OFFICER.
00900
00901      MOVE LO-LOAN-COUNT  (WS-SUB1)   TO ALN-CNT-OUT (WS-SUB1).
00902      MOVE LO-LOAN-VOLUME (WS-SUB1)   TO ALN-VOL-OUT (WS-SUB1).
00903      MOVE LO-LF-COUNT    (WS-SUB1)   TO ALF-CNT-OUT (WS-SUB1).
00904      MOVE LO-LF-PREM     (WS-SUB1)   TO ALF-PRM-OUT (WS-SUB1).
00905      MOVE LO-LF-BENEFIT  (WS-SUB1)   TO ALF-AMT-OUT (WS-SUB1).
00906      MOVE LO-AH-COUNT    (WS-SUB1)   TO AAH-CNT-OUT (WS-SUB1).
00907      MOVE LO-AH-PREM     (WS-SUB1)   TO AAH-PRM-OUT (WS-SUB1).
00908      MOVE LO-AH-BENEFIT  (WS-SUB1)   TO AAH-AMT-OUT (WS-SUB1).
00909
00910      GO TO 3050-PROCESS-OFFICER-INFO.
00911
00912  3200-DISPLAY-OFFICER.
00913      MOVE -1                         TO AMAINTL.
00914      MOVE    ER-0000             TO EMI-ERROR.
00915      PERFORM 9900-ERROR-FORMAT.
00916      GO TO 8100-SEND-INITIAL-MAP.
00917
00918  3350-OFFICER-NOT-FOUND.
00919      IF CARR-GROUP-ST-ACCNT-CNTL
00920          MOVE -1                 TO ACARRL
00921      ELSE
00922          IF ST-ACCNT-CNTL
00923              MOVE -1             TO ASTHDGL
00924          ELSE
00925              IF CARR-ST-ACCNT-CNTL
00926                  MOVE -1         TO ACARRL
00927              ELSE
00928                  IF ACCNT-CNTL
00929                      MOVE -1     TO   AACCTL
00930                  ELSE
00931                      IF CARR-ACCNT-CNTL
00932                          MOVE -1 TO ACARRL.
00933
00934
00935      MOVE  ER-3006               TO EMI-ERROR.
00936      PERFORM 9900-ERROR-FORMAT.
00937      GO TO 8200-SEND-DATAONLY.
00938
00939  3400-EXIT.
00940      EXIT.
00941
00942      EJECT
00943  3500-REWRITE-KEY          SECTION.
00944      MOVE PI-COMPANY-CD          TO ERLOFC-COMPANY-CD.
00945      MOVE PI-CR-CARRIER          TO ERLOFC-CARRIER.
00946      MOVE PI-CR-GROUPING         TO ERLOFC-GROUPING.
00947      MOVE PI-CR-STATE            TO ERLOFC-STATE.
00948      MOVE PI-CR-ACCOUNT          TO ERLOFC-ACCOUNT.
00949      MOVE PI-LOAN-OFFICER        TO ERLOFC-OFFICER-CODE.
00950
00951      PERFORM 7400-READ-LOAN-MASTER.
00952
00953      IF OFFICER-WAS-NOT-FOUND
00954         GO TO 3550-PROCESS-NEW-OFFICER.
00955
00956      MOVE -1                     TO  AMAINTL.
00957      MOVE  ER-0132               TO  EMI-ERROR.
00958      PERFORM 9900-ERROR-FORMAT.
00959      GO TO 8200-SEND-DATAONLY.
00960
00961  3550-PROCESS-NEW-OFFICER.
00962      MOVE PI-CRLOFC-KEY          TO ERLOFC-KEY.
00963
00964      PERFORM 7460-READ-LOAN-MST-UPDT.
00965
00966      IF  OFFICER-WAS-FOUND  NEXT SENTENCE
00967         ELSE
00968          GO TO 3590-OFFICER-NOT-FOUND.
00969
00970      MOVE LO-OFFICER-INFO        TO WS-OFFICER-INFO.
00971      PERFORM 6500-EDIT-LOAN-DATA.
00972      IF  EMI-ERROR = ZEROS NEXT SENTENCE
00973         ELSE
00974          GO TO 8200-SEND-DATAONLY.
00975
00976      MOVE 'D'                    TO JP-RECORD-TYPE.
00977      MOVE LOAN-OFFICER-MASTER    TO JP-RECORD-AREA.
00978
00979      PERFORM 7700-DELETE-LOAN-MASTER.
00980
00981      PERFORM 8400-LOG-JOURNAL-RECORD.
00982
00983      
      * EXEC CICS GETMAIN
00984 *       SET      (ADDRESS OF LOAN-OFFICER-MASTER)
00985 *       LENGTH   (ERLOFC-RECORD-LENGTH)
00986 *       INITIMG  (GETMAIN-SPACE)
00987 *    END-EXEC.
      *    MOVE ',"IL                  $   #00003500' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERLOFC-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF LOAN-OFFICER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00988
00989      MOVE JP-RECORD-AREA         TO LOAN-OFFICER-MASTER.
00990
00991      IF ACARRL GREATER THAN +0
00992         MOVE ACARRI              TO LO-CARRIER.
00993
00994      IF AGRPL  GREATER THAN +0
00995         MOVE AGRPI               TO LO-GROUPING.
00996
00997      IF ASTL   GREATER THAN +0
00998         MOVE ASTI                TO LO-STATE.
00999
01000      IF AACCTL GREATER THAN +0
01001         MOVE AACCTI               TO LO-ACCOUNT.
01002
01003      IF AOFCRL GREATER THAN +0
01004         MOVE AOFCRI              TO LO-OFFICER-CODE.
01005
01006      IF  ANAMEL GREATER THAN +0
01007          MOVE ANAMEI             TO LO-OFFICER-NAME.
01008      IF  ACOMPL GREATER THAN +0
01009          MOVE ACOMPI             TO LO-COMP-CONTROL.
01010      IF  APRINTL GREATER THAN +0
01011          MOVE APRINTI            TO LO-DETAIL-CONTROL.
01012
01013      MOVE WS-OFFICER-INFO        TO LO-OFFICER-INFO.
01014
01015      MOVE  PI-PROCESSOR-ID       TO LO-LAST-USER.
01016      MOVE  SAVE-BIN-DATE         TO LO-LAST-MAINT-DT.
01017      MOVE  EIBTIME               TO LO-LAST-MAINT-HHMMSS.
01018
01019      MOVE 'A'                    TO JP-RECORD-TYPE.
01020      MOVE LOAN-OFFICER-MASTER    TO JP-RECORD-AREA.
01021
01022      MOVE LO-CONTROL-PRIMARY     TO ERLOFC-KEY.
01023
01024      PERFORM 7500-WRITE-LOAN-MASTER.
01025
01026      PERFORM 8400-LOG-JOURNAL-RECORD.
01027
01028      GO TO 3000-SHOW-LOAN-OFFICER.
01029
01030  3590-OFFICER-NOT-FOUND.
01031      MOVE -1                     TO AMAINTL.
01032      MOVE  ER-3006               TO EMI-ERROR.
01033      PERFORM 9900-ERROR-FORMAT.
01034      GO TO 8200-SEND-DATAONLY.
01035
01036  3595-EXIT.
01037      EXIT.
01038
01039      EJECT
01040  6500-EDIT-LOAN-DATA        SECTION.
01041      MOVE +0                     TO WS-SUB1.
01042
01043  6520-PROCESS-LOAN-DATA.
01044      ADD +1                      TO WS-SUB1.
01045
01046      IF WS-SUB1 IS GREATER THAN +12
01047         GO TO 6590-EXIT.
01048
01049      MOVE 'N'                    TO WS-DATA-KEYED-SW.
01050
01051      IF  ALN-CNT-LENGTH (WS-SUB1) GREATER THAN +0 OR
01052          ALN-VOL-LENGTH (WS-SUB1) GREATER THAN +0
01053              NEXT SENTENCE
01054        ELSE
01055          GO TO 6550-CHK-PROCESSOR-ID.
01056
01057      MOVE 'Y'                    TO WS-SCREEN-KEYED-SW.
01058
01059      IF  ALN-CNT-LENGTH (WS-SUB1) GREATER THAN +0
01060          MOVE ALN-CNT-IN (WS-SUB1)   TO DEEDIT-FIELD
01061          PERFORM 8600-DEEDIT
01062          IF  DEEDIT-FIELD-V0     NOT NUMERIC
01063              MOVE -1             TO ALN-CNT-LENGTH (WS-SUB1)
01064              MOVE AL-UNBON       TO ALN-CNT-ATTRB  (WS-SUB1)
01065              MOVE  ER-3008       TO EMI-ERROR
01066              PERFORM 9900-ERROR-FORMAT
01067          ELSE
01068              MOVE DEEDIT-FIELD-V0     TO WS-LN-CNT      (WS-SUB1)
01069                                          ALN-CNT-OUT    (WS-SUB1).
01070
01071      IF  ALN-VOL-LENGTH (WS-SUB1) GREATER THAN +0
01072          MOVE ALN-VOL-IN (WS-SUB1)   TO DEEDIT-FIELD
01073          PERFORM 8600-DEEDIT
01074          IF  DEEDIT-FIELD-V0     NOT NUMERIC
01075              MOVE -1             TO ALN-VOL-LENGTH (WS-SUB1)
01076              MOVE AL-UNBON       TO ALN-VOL-ATTRB  (WS-SUB1)
01077              MOVE  ER-3009       TO EMI-ERROR
01078              PERFORM 9900-ERROR-FORMAT
01079          ELSE
01080              MOVE DEEDIT-FIELD-V0     TO WS-LN-VOL      (WS-SUB1)
01081                                          ALN-VOL-OUT    (WS-SUB1).
01082
01083 *    IF  WS-LN-CNT (WS-SUB1) = ZEROS OR
01084 *        WS-LN-VOL (WS-SUB1) = ZEROS
01085 *        MOVE -1                 TO ALN-CNT-LENGTH (WS-SUB1)
01086 *        MOVE  ER-3007           TO EMI-ERROR
01087 *        PERFORM 9900-ERROR-FORMAT.
01088
01089  6550-CHK-PROCESSOR-ID.
01090      IF  PI-PROCESSOR-ID NOT = 'LGXX'
01091          GO TO 6520-PROCESS-LOAN-DATA.
01092
01093      IF  ALF-CNT-LENGTH (WS-SUB1) GREATER THAN +0
01094          MOVE 'Y'                    TO WS-DATA-KEYED-SW
01095          MOVE ALF-CNT-IN (WS-SUB1)   TO DEEDIT-FIELD
01096          PERFORM 8600-DEEDIT
01097          IF  DEEDIT-FIELD-V0     NOT NUMERIC
01098              MOVE -1                 TO ALF-CNT-LENGTH (WS-SUB1)
01099              MOVE AL-UNBON           TO ALF-CNT-ATTRB  (WS-SUB1)
01100              MOVE  ER-3010           TO EMI-ERROR
01101              PERFORM 9900-ERROR-FORMAT
01102          ELSE
01103              MOVE DEEDIT-FIELD-V0    TO WS-LF-CNT      (WS-SUB1)
01104                                         ALF-CNT-OUT    (WS-SUB1).
01105
01106      IF  ALF-PRM-LENGTH (WS-SUB1) GREATER THAN +0
01107          MOVE 'Y'                    TO WS-DATA-KEYED-SW
01108          MOVE ALF-PRM-IN (WS-SUB1)   TO DEEDIT-FIELD
01109          PERFORM 8600-DEEDIT
01110          IF  DEEDIT-FIELD-V0     NOT NUMERIC
01111              MOVE -1             TO ALF-PRM-LENGTH (WS-SUB1)
01112              MOVE AL-UNBON       TO ALF-PRM-ATTRB  (WS-SUB1)
01113              MOVE  ER-3011       TO EMI-ERROR
01114              PERFORM 9900-ERROR-FORMAT
01115          ELSE
01116              MOVE DEEDIT-FIELD-V0     TO WS-LF-PRM   (WS-SUB1)
01117                                          ALF-PRM-OUT (WS-SUB1).
01118
01119      IF  ALF-AMT-LENGTH (WS-SUB1) GREATER THAN +0
01120          MOVE 'Y'                    TO WS-DATA-KEYED-SW
01121          MOVE ALF-AMT-IN (WS-SUB1)   TO DEEDIT-FIELD
01122          PERFORM 8600-DEEDIT
01123          IF  DEEDIT-FIELD-V0     NOT NUMERIC
01124              MOVE -1             TO ALF-AMT-LENGTH (WS-SUB1)
01125              MOVE AL-UNBON       TO ALF-AMT-ATTRB  (WS-SUB1)
01126              MOVE  ER-3012       TO EMI-ERROR
01127              PERFORM 9900-ERROR-FORMAT
01128          ELSE
01129              MOVE DEEDIT-FIELD-V0     TO WS-LF-AMT   (WS-SUB1)
01130                                          ALF-AMT-OUT (WS-SUB1).
01131
01132      IF  AAH-CNT-LENGTH (WS-SUB1) GREATER THAN +0
01133          MOVE 'Y'                    TO WS-DATA-KEYED-SW
01134          MOVE AAH-CNT-IN (WS-SUB1)   TO DEEDIT-FIELD
01135          PERFORM 8600-DEEDIT
01136          IF  DEEDIT-FIELD-V0     NOT NUMERIC
01137              MOVE -1             TO AAH-CNT-LENGTH (WS-SUB1)
01138              MOVE AL-UNBON       TO AAH-CNT-ATTRB  (WS-SUB1)
01139              MOVE  ER-3013       TO EMI-ERROR
01140              PERFORM 9900-ERROR-FORMAT
01141          ELSE
01142              MOVE DEEDIT-FIELD-V0     TO WS-AH-CNT      (WS-SUB1)
01143                                          AAH-CNT-OUT    (WS-SUB1).
01144
01145      IF  AAH-PRM-LENGTH (WS-SUB1) GREATER THAN +0
01146          MOVE 'Y'                    TO WS-DATA-KEYED-SW
01147          MOVE AAH-PRM-IN (WS-SUB1)   TO DEEDIT-FIELD
01148          PERFORM 8600-DEEDIT
01149          IF  DEEDIT-FIELD-V0     NOT NUMERIC
01150              MOVE -1             TO AAH-PRM-LENGTH (WS-SUB1)
01151              MOVE AL-UNBON       TO AAH-PRM-ATTRB  (WS-SUB1)
01152              MOVE  ER-3014       TO EMI-ERROR
01153              PERFORM 9900-ERROR-FORMAT
01154          ELSE
01155              MOVE DEEDIT-FIELD-V0     TO WS-AH-PRM   (WS-SUB1)
01156                                          AAH-PRM-OUT (WS-SUB1).
01157
01158      IF  AAH-AMT-LENGTH (WS-SUB1) GREATER THAN +0
01159          MOVE 'Y'                    TO WS-DATA-KEYED-SW
01160          MOVE AAH-AMT-IN (WS-SUB1)   TO DEEDIT-FIELD
01161          PERFORM 8600-DEEDIT
01162          IF  DEEDIT-FIELD-V0     NOT NUMERIC
01163              MOVE -1             TO AAH-AMT-LENGTH (WS-SUB1)
01164              MOVE AL-UNBON       TO AAH-AMT-ATTRB  (WS-SUB1)
01165              MOVE  ER-3015       TO EMI-ERROR
01166              PERFORM 9900-ERROR-FORMAT
01167          ELSE
01168              MOVE DEEDIT-FIELD-V0     TO WS-AH-AMT   (WS-SUB1)
01169                                          AAH-AMT-OUT (WS-SUB1).
01170
01171 *    IF  DATA-WAS-KEYED
01172 *        IF  WS-LN-CNT (WS-SUB1) = ZEROS
01173 *            IF WS-LN-VOL (WS-SUB1) = ZEROS
01174 *               MOVE -1              TO ALN-CNT-LENGTH (WS-SUB1)
01175 *               MOVE  ER-3007    TO EMI-ERROR
01176 *               PERFORM 9900-ERROR-FORMAT.
01177
01178      IF  DATA-WAS-KEYED
01179          MOVE 'Y'                TO WS-SCREEN-KEYED-SW.
01180
01181      GO TO 6520-PROCESS-LOAN-DATA.
01182
01183  6590-EXIT.
01184      EXIT.
01185
01186      EJECT
01187  7000-BROWSE-FWRD-NEXT-ACCOUNT   SECTION.
01188      IF  ACARRL GREATER THAN +0
01189          MOVE ACARRI             TO PI-CR-CARRIER
01190         ELSE
01191          MOVE SPACES             TO PI-CR-CARRIER.
01192
01193      IF  AGRPL GREATER THAN +0
01194          MOVE AGRPI              TO PI-CR-GROUPING
01195         ELSE
01196          MOVE SPACES             TO PI-CR-GROUPING.
01197
01198      IF  ASTL GREATER THAN +0
01199          MOVE ASTI               TO PI-CR-STATE
01200         ELSE
01201          MOVE SPACES             TO PI-CR-STATE.
01202
01203      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.
01204
01205      
      * EXEC CICS HANDLE CONDITION
01206 *        NOTFND   (7080-END-OF-SEARCH)
01207 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00003722' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033373232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01208
01209      MOVE PI-COMPANY-CD          TO ERLOFC-COMPANY-CD.
01210      MOVE PI-CR-CARRIER          TO ERLOFC-CARRIER.
01211      MOVE PI-CR-GROUPING         TO ERLOFC-GROUPING.
01212      MOVE PI-CR-STATE            TO ERLOFC-STATE.
01213      MOVE PI-CR-ACCOUNT          TO ERLOFC-ACCOUNT.
01214      MOVE HIGH-VALUES            TO ERLOFC-OFFICER-CODE.
01215
01216      
      * EXEC CICS STARTBR
01217 *        DATASET  (ERLOFC-ID)
01218 *        RIDFLD   (ERLOFC-KEY)
01219 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003733' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01220
01221      
      * EXEC CICS HANDLE CONDITION
01222 *        NOTFND   (7070-END-OF-BROWSE)
01223 *        ENDFILE  (7070-END-OF-BROWSE)
01224 *    END-EXEC.
      *    MOVE '"$I''                  ! % #00003738' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033373338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01225
01226  7010-READ-FWRD-NEXT-RECORD.
01227      
      * EXEC CICS READNEXT
01228 *        DATASET  (ERLOFC-ID)
01229 *        RIDFLD   (ERLOFC-KEY)
01230 *        SET      (ADDRESS OF LOAN-OFFICER-MASTER)
01231 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003744' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LOAN-OFFICER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01232
01233      IF  PI-CR-ACCOUNT = SPACES
01234          MOVE ERLOFC-CARRIER     TO PI-CR-CARRIER
01235          MOVE ERLOFC-GROUPING    TO PI-CR-GROUPING
01236          MOVE ERLOFC-STATE       TO PI-CR-STATE.
01237
01238      IF  ERLOFC-COMPANY-CD   = PI-COMPANY-CD
01239          NEXT SENTENCE
01240         ELSE
01241          GO TO 7070-END-OF-BROWSE.
01242
01243      MOVE 'Y'                    TO WS-OFFICER-FOUND-SW.
01244
01245  7070-END-OF-BROWSE.
01246      
      * EXEC CICS ENDBR
01247 *        DATASET  (ERLOFC-ID)
01248 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003763' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01249
01250       IF OFFICER-WAS-NOT-FOUND NEXT SENTENCE
01251         ELSE
01252          GO TO 7090-EXIT.
01253
01254  7080-END-OF-SEARCH.
01255      MOVE -1                     TO AMAINTL.
01256      MOVE  ER-2237               TO EMI-ERROR.
01257      PERFORM 9900-ERROR-FORMAT.
01258      GO TO 8200-SEND-DATAONLY.
01259
01260  7090-EXIT.
01261      EXIT.
01262
01263      EJECT
01264  7100-BROWSE-BWRD-NEXT-ACCOUNT   SECTION.
01265      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.
01266
01267      
      * EXEC CICS HANDLE CONDITION
01268 *        NOTFND   (7180-END-OF-SEARCH)
01269 *    END-EXEC.
      *    MOVE '"$I                   ! & #00003784' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303033373834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01270
01271      MOVE PI-PREV-CRLOFC-KEY     TO ERLOFC-KEY.
01272
01273      
      * EXEC CICS STARTBR
01274 *        DATASET  (ERLOFC-ID)
01275 *        RIDFLD   (ERLOFC-KEY)
01276 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003790' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01277
01278      
      * EXEC CICS HANDLE CONDITION
01279 *        NOTFND   (7170-END-OF-BROWSE)
01280 *        ENDFILE  (7170-END-OF-BROWSE)
01281 *    END-EXEC.
      *    MOVE '"$I''                  ! '' #00003795' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303033373935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01282
01283  7110-READ-BWRD-NEXT-RECORD.
01284      
      * EXEC CICS READPREV
01285 *        DATASET  (ERLOFC-ID)
01286 *        RIDFLD   (ERLOFC-KEY)
01287 *        SET      (ADDRESS OF LOAN-OFFICER-MASTER)
01288 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00003801' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LOAN-OFFICER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01289
01290      IF  ERLOFC-COMPANY-CD   = PI-COMPANY-CD
01291          AND ERLOFC-CARRIER  = PI-CR-CARRIER
01292          AND ERLOFC-GROUPING = PI-CR-GROUPING
01293          AND ERLOFC-STATE    = PI-CR-STATE
01294          AND ERLOFC-ACCOUNT  = PI-CR-ACCOUNT
01295          GO TO 7110-READ-BWRD-NEXT-RECORD.
01296
01297      
      * EXEC CICS ENDBR
01298 *        DATASET  (ERLOFC-ID)
01299 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003814' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01300
01301      MOVE LOW-VALUES             TO ERLOFC-OFFICER-CODE
01302
01303      
      * EXEC CICS STARTBR
01304 *        DATASET  (ERLOFC-ID)
01305 *        RIDFLD   (ERLOFC-KEY)
01306 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003820' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01307
01308      
      * EXEC CICS READNEXT
01309 *        DATASET  (ERLOFC-ID)
01310 *        RIDFLD   (ERLOFC-KEY)
01311 *        SET      (ADDRESS OF LOAN-OFFICER-MASTER)
01312 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003825' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LOAN-OFFICER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01313
01314      IF  ERLOFC-COMPANY-CD   = PI-COMPANY-CD
01315          NEXT SENTENCE
01316         ELSE
01317          GO TO 7170-END-OF-BROWSE.
01318
01319      MOVE 'Y'                    TO WS-OFFICER-FOUND-SW.
01320
01321  7170-END-OF-BROWSE.
01322      
      * EXEC CICS ENDBR
01323 *        DATASET  (ERLOFC-ID)
01324 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003839' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01325
01326      IF OFFICER-WAS-NOT-FOUND NEXT SENTENCE
01327        ELSE
01328         GO TO 7190-EXIT.
01329
01330  7180-END-OF-SEARCH.
01331      MOVE -1                     TO AMAINTL.
01332      MOVE  ER-2238               TO EMI-ERROR.
01333      PERFORM 9900-ERROR-FORMAT.
01334      GO TO 8200-SEND-DATAONLY.
01335
01336   7190-EXIT.
01337      EXIT.
01338
01339      EJECT
01340  7200-BROWSE-FWRD-NEXT-OFFICER   SECTION.
01341      IF  ACARRL GREATER THAN +0
01342          MOVE ACARRI             TO PI-CR-CARRIER
01343         ELSE
01344          MOVE SPACES             TO PI-CR-CARRIER.
01345
01346      IF  AGRPL GREATER THAN +0
01347          MOVE AGRPI              TO PI-CR-GROUPING
01348         ELSE
01349          MOVE SPACES             TO PI-CR-GROUPING.
01350
01351      IF  ASTL GREATER THAN +0
01352          MOVE ASTI               TO PI-CR-STATE
01353         ELSE
01354          MOVE SPACES             TO PI-CR-STATE.
01355
01356      IF  AOFCRL GREATER THAN +0
01357          MOVE AOFCRI             TO PI-LOAN-OFFICER
01358         ELSE
01359          MOVE SPACES             TO PI-LOAN-OFFICER.
01360
01361      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.
01362
01363      
      * EXEC CICS HANDLE CONDITION
01364 *        NOTFND   (7280-END-OF-SEARCH)
01365 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00003880' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303033383830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01366
01367      MOVE PI-COMPANY-CD          TO ERLOFC-COMPANY-CD.
01368      MOVE PI-CR-CARRIER          TO ERLOFC-CARRIER.
01369      MOVE PI-CR-GROUPING         TO ERLOFC-GROUPING.
01370      MOVE PI-CR-STATE            TO ERLOFC-STATE.
01371      MOVE PI-CR-ACCOUNT          TO ERLOFC-ACCOUNT.
01372      MOVE PI-LOAN-OFFICER        TO ERLOFC-OFFICER-CODE.
01373
01374      
      * EXEC CICS STARTBR
01375 *        DATASET  (ERLOFC-ID)
01376 *        RIDFLD   (ERLOFC-KEY)
01377 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003891' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01378
01379      
      * EXEC CICS HANDLE CONDITION
01380 *        NOTFND   (7270-END-OF-BROWSE)
01381 *        ENDFILE  (7270-END-OF-BROWSE)
01382 *    END-EXEC.
      *    MOVE '"$I''                  ! ) #00003896' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303033383936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01383
01384  7210-READ-FWRD-NEXT-RECORD.
01385      
      * EXEC CICS READNEXT
01386 *        DATASET  (ERLOFC-ID)
01387 *        RIDFLD   (ERLOFC-KEY)
01388 *        SET      (ADDRESS OF LOAN-OFFICER-MASTER)
01389 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003902' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LOAN-OFFICER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01390
01391      IF  ERLOFC-KEY = PI-CRLOFC-KEY
01392          GO TO 7210-READ-FWRD-NEXT-RECORD.
01393
01394      IF  PI-CR-ACCOUNT = SPACES
01395          MOVE ERLOFC-CARRIER     TO PI-CR-CARRIER
01396          MOVE ERLOFC-GROUPING    TO PI-CR-GROUPING
01397          MOVE ERLOFC-STATE       TO PI-CR-STATE
01398          MOVE ERLOFC-ACCOUNT     TO PI-CR-ACCOUNT.
01399
01400      IF  ERLOFC-COMPANY-CD   = PI-COMPANY-CD
01401          AND ERLOFC-CARRIER  = PI-CR-CARRIER
01402          AND ERLOFC-GROUPING = PI-CR-GROUPING
01403          AND ERLOFC-STATE    = PI-CR-STATE
01404          AND ERLOFC-ACCOUNT  = PI-CR-ACCOUNT
01405          NEXT SENTENCE
01406         ELSE
01407          GO TO 7270-END-OF-BROWSE.
01408
01409      MOVE 'Y'                    TO WS-OFFICER-FOUND-SW.
01410
01411  7270-END-OF-BROWSE.
01412      
      * EXEC CICS ENDBR
01413 *        DATASET  (ERLOFC-ID)
01414 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003929' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01415
01416       IF OFFICER-WAS-NOT-FOUND NEXT SENTENCE
01417         ELSE
01418          GO TO 7290-EXIT.
01419
01420  7280-END-OF-SEARCH.
01421      MOVE -1                     TO AMAINTL.
01422      MOVE  ER-3018               TO EMI-ERROR.
01423      PERFORM 9900-ERROR-FORMAT.
01424      GO TO 8200-SEND-DATAONLY.
01425
01426  7290-EXIT.
01427      EXIT.
01428
01429      EJECT
01430  7300-BROWSE-BWRD-NEXT-OFFICER   SECTION.
01431      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.
01432
01433      
      * EXEC CICS HANDLE CONDITION
01434 *        NOTFND   (7380-END-OF-SEARCH)
01435 *    END-EXEC.
      *    MOVE '"$I                   ! * #00003950' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303033393530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01436
01437      MOVE PI-PREV-CRLOFC-KEY     TO ERLOFC-KEY.
01438
01439      
      * EXEC CICS STARTBR
01440 *        DATASET  (ERLOFC-ID)
01441 *        RIDFLD   (ERLOFC-KEY)
01442 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003956' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01443
01444      
      * EXEC CICS HANDLE CONDITION
01445 *        NOTFND   (7370-END-OF-BROWSE)
01446 *        ENDFILE  (7370-END-OF-BROWSE)
01447 *    END-EXEC.
      *    MOVE '"$I''                  ! + #00003961' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303033393631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01448
01449  7310-READ-BWRD-NEXT-RECORD.
01450      
      * EXEC CICS READPREV
01451 *        DATASET  (ERLOFC-ID)
01452 *        RIDFLD   (ERLOFC-KEY)
01453 *        SET      (ADDRESS OF LOAN-OFFICER-MASTER)
01454 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00003967' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LOAN-OFFICER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01455
01456      IF  ERLOFC-KEY = PI-CRLOFC-KEY
01457          GO TO 7310-READ-BWRD-NEXT-RECORD.
01458
01459      IF  ERLOFC-COMPANY-CD   = PI-COMPANY-CD
01460          AND ERLOFC-CARRIER  = PI-CR-CARRIER
01461          AND ERLOFC-GROUPING = PI-CR-GROUPING
01462          AND ERLOFC-STATE    = PI-CR-STATE
01463          AND ERLOFC-ACCOUNT  = PI-CR-ACCOUNT
01464          NEXT SENTENCE
01465         ELSE
01466          GO TO 7370-END-OF-BROWSE.
01467
01468      MOVE 'Y'                    TO WS-OFFICER-FOUND-SW.
01469
01470  7370-END-OF-BROWSE.
01471      
      * EXEC CICS ENDBR
01472 *        DATASET  (ERLOFC-ID)
01473 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003988' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01474
01475      IF OFFICER-WAS-NOT-FOUND NEXT SENTENCE
01476        ELSE
01477         GO TO 7390-EXIT.
01478
01479  7380-END-OF-SEARCH.
01480      MOVE -1                     TO AMAINTL.
01481      MOVE  ER-3019               TO EMI-ERROR.
01482      PERFORM 9900-ERROR-FORMAT.
01483      GO TO 8200-SEND-DATAONLY.
01484
01485  7390-EXIT.
01486      EXIT.
01487
01488      EJECT
01489  7400-READ-LOAN-MASTER      SECTION.
01490      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.
01491
01492      
      * EXEC CICS HANDLE CONDITION
01493 *        NOTFND   (7440-OFFICER-NOTFND)
01494 *    END-EXEC.
      *    MOVE '"$I                   ! , #00004009' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303034303039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01495
01496      
      * EXEC CICS READ
01497 *        DATASET   (ERLOFC-ID)
01498 *        SET       (ADDRESS OF LOAN-OFFICER-MASTER)
01499 *        RIDFLD    (ERLOFC-KEY)
01500 *        EQUAL
01501 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004013' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LOAN-OFFICER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01502
01503      MOVE 'Y'                    TO WS-OFFICER-FOUND-SW.
01504
01505      GO TO 7450-EXIT.
01506
01507  7440-OFFICER-NOTFND.
01508      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.
01509
01510  7450-EXIT.
01511      EXIT.
01512      EJECT
01513
01514  7460-READ-LOAN-MST-UPDT    SECTION.
01515      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.
01516
01517      
      * EXEC CICS HANDLE CONDITION
01518 *        NOTFND   (7480-OFFICER-NOTFND)
01519 *    END-EXEC.
      *    MOVE '"$I                   ! - #00004034' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303034303334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01520
01521      
      * EXEC CICS READ
01522 *        DATASET   (ERLOFC-ID)
01523 *        SET       (ADDRESS OF LOAN-OFFICER-MASTER)
01524 *        RIDFLD    (ERLOFC-KEY)
01525 *        EQUAL
01526 *        UPDATE
01527 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004038' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LOAN-OFFICER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01528
01529      MOVE 'Y'                    TO WS-OFFICER-FOUND-SW.
01530
01531      GO TO 7490-EXIT.
01532
01533  7480-OFFICER-NOTFND.
01534      MOVE 'N'                    TO WS-OFFICER-FOUND-SW.
01535
01536  7490-EXIT.
01537      EXIT.
01538      EJECT
01539  7500-WRITE-LOAN-MASTER          SECTION.
01540      
      * EXEC CICS HANDLE CONDITION
01541 *        DUPREC   (7550-DUPLICATE-RECORD)
01542 *    END-EXEC.
      *    MOVE '"$%                   ! . #00004057' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303034303537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01543
01544      
      * EXEC CICS WRITE
01545 *        FROM      (LOAN-OFFICER-MASTER)
01546 *        DATASET   (ERLOFC-ID)
01547 *        RIDFLD    (ERLOFC-KEY)
01548 *    END-EXEC.
           MOVE LENGTH OF
            LOAN-OFFICER-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004061' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 LOAN-OFFICER-MASTER, 
                 DFHEIV11, 
                 ERLOFC-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01549
01550      GO TO 7590-EXIT.
01551
01552  7550-DUPLICATE-RECORD.
01553      MOVE -1                     TO AMAINTL.
01554      MOVE  ER-0132               TO EMI-ERROR.
01555      PERFORM 9900-ERROR-FORMAT.
01556      GO TO 8200-SEND-DATAONLY.
01557
01558  7590-EXIT.
01559      EXIT.
01560
01561      EJECT
01562  7600-REWRITE-LOAN-MASTER    SECTION.
01563      
      * EXEC CICS REWRITE
01564 *        FROM     (LOAN-OFFICER-MASTER)
01565 *        DATASET  (ERLOFC-ID)
01566 *    END-EXEC.
           MOVE LENGTH OF
            LOAN-OFFICER-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004080' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 LOAN-OFFICER-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01567
01568  7690-EXIT.
01569      EXIT.
01570  7700-DELETE-LOAN-MASTER    SECTION.
01571      
      * EXEC CICS DELETE
01572 *        DATASET  (ERLOFC-ID)
01573 *    END-EXEC.
      *    MOVE '&(                    &   #00004088' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERLOFC-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01574
01575  7790-EXIT.
01576      EXIT.
01577
01578      EJECT
01579  7800-READ-ACCOUNT-MASTER   SECTION.
01580      MOVE PI-COMPANY-CD          TO ERACCT-A-CO-CD.
01581      MOVE PI-CR-CARRIER          TO ERACCT-A-CARRIER.
01582      MOVE PI-CR-GROUPING         TO ERACCT-A-GROUPING.
01583      MOVE PI-CR-STATE            TO ERACCT-A-STATE.
01584      MOVE PI-CR-ACCOUNT          TO ERACCT-A-ACCOUNT.
01585      MOVE LOW-VALUES             TO ERACCT-A-EXP-DATE.
01586
01587      
      * EXEC CICS HANDLE CONDITION
01588 *        NOTFND   (7870-ACCOUNT-INVALID)
01589 *    END-EXEC.
      *    MOVE '"$I                   ! / #00004104' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303034313034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01590
01591      
      * EXEC CICS READ
01592 *        DATASET   (ERACCT-ALT-FILE-ID)
01593 *        SET       (ADDRESS OF ACCOUNT-MASTER)
01594 *        RIDFLD    (ERACCT-ALT-KEY)
01595 *        GTEQ
01596 *    END-EXEC.
      *    MOVE '&"S        G          (   #00004108' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-ALT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01597
01598      IF PI-COMPANY-CD  = AM-COMPANY-CD-A1  AND
01599         PI-CR-CARRIER  = AM-VG-CARRIER     AND
01600         PI-CR-GROUPING = AM-VG-GROUPING    AND
01601         PI-CR-STATE    = AM-VG-STATE       AND
01602         PI-CR-ACCOUNT  = AM-VG-ACCOUNT
01603            GO TO 7890-EXIT.
01604
01605  7870-ACCOUNT-INVALID.
01606      IF CARR-GROUP-ST-ACCNT-CNTL
01607          MOVE -1                 TO ACARRL
01608          MOVE AL-UABON           TO ACARRA
01609                                     AGRPA
01610                                     ASTA
01611                                     AACCTA
01612      ELSE
01613          IF ST-ACCNT-CNTL
01614              MOVE -1             TO ASTL
01615              MOVE AL-UABON       TO ASTA
01616                                     AACCTA
01617          ELSE
01618              IF CARR-ST-ACCNT-CNTL
01619                  MOVE -1             TO ACARRL
01620                  MOVE AL-UABON       TO ACARRA
01621                                         ASTA
01622                                         AACCTA
01623              ELSE
01624                  IF ACCNT-CNTL
01625                      MOVE -1         TO AACCTL
01626                      MOVE AL-UABON   TO AACCTA
01627                  ELSE
01628                      MOVE AL-UABON   TO ACARRA
01629                                         AACCTA.
01630
01631      MOVE  ER-2210               TO EMI-ERROR.
01632      PERFORM 9900-ERROR-FORMAT.
01633
01634  7890-EXIT.
01635      EXIT.
01636
01637      EJECT
01638  8100-SEND-INITIAL-MAP SECTION.
01639      MOVE SAVE-DATE              TO ADATEO.
01640      MOVE EIBTIME                TO TIME-IN.
01641      MOVE TIME-OUT               TO ATIMEO.
01642      MOVE -1                     TO AMAINTL.
01643      MOVE EMI-MESSAGE-AREA (1)   TO AERRMSGO.
01644
01645      IF CARR-GROUP-ST-ACCNT-CNTL
01646          NEXT SENTENCE
01647      ELSE
01648          IF ST-ACCNT-CNTL
01649              MOVE AL-SADOF       TO ACARHDGA
01650                                     AGRPHDGA
01651              MOVE AL-SANOF       TO ACARRA
01652                                     AGRPA
01653          ELSE
01654              IF CARR-ST-ACCNT-CNTL
01655                  MOVE AL-SADOF   TO AGRPHDGA
01656                  MOVE AL-SANOF   TO AGRPA
01657              ELSE
01658                  IF ACCNT-CNTL
01659                      MOVE AL-SADOF TO ACARHDGA
01660                                       AGRPHDGA
01661                                       ASTHDGA
01662                      MOVE AL-SANOF TO ACARRA
01663                                       AGRPA
01664                                       ASTA
01665                  ELSE
01666                      IF CARR-ACCNT-CNTL
01667                          MOVE AL-SADOF TO AGRPHDGA
01668                                           ASTHDGA
01669                          MOVE AL-SANOF TO AGRPA
01670                                           ASTA.
01671
01672      IF  PI-PROCESSOR-ID = 'LGXX'
01673          NEXT SENTENCE
01674         ELSE
01675          GO TO 8150-SEND-INITIAL-MAP.
01676
01677      MOVE +0                     TO WS-SUB1.
01678
01679  8110-SET-INITIAL-ATTRIBUTES.
01680      ADD +1                      TO WS-SUB1.
01681
01682      IF  WS-SUB1 GREATER THAN +12
01683          GO TO 8150-SEND-INITIAL-MAP.
01684
01685      MOVE AL-UANOF               TO ALF-CNT-ATTRB    (WS-SUB1)
01686                                     ALF-PRM-ATTRB    (WS-SUB1)
01687                                     ALF-AMT-ATTRB    (WS-SUB1)
01688                                     AAH-CNT-ATTRB    (WS-SUB1)
01689                                     AAH-PRM-ATTRB    (WS-SUB1)
01690                                     AAH-AMT-ATTRB    (WS-SUB1).
01691
01692      GO TO 8110-SET-INITIAL-ATTRIBUTES.
01693
01694  8150-SEND-INITIAL-MAP.
01695      
      * EXEC CICS SEND
01696 *        MAP      (MAP-NAME)
01697 *        MAPSET   (MAPSET-NAME)
01698 *        FROM     (EL610AO)
01699 *        ERASE
01700 *        CURSOR
01701 *    END-EXEC.
           MOVE LENGTH OF
            EL610AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004212' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL610AO, 
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
           
01702
01703      GO TO 9100-RETURN-TRAN.
01704
01705  8200-SEND-DATAONLY     SECTION.
01706      MOVE SAVE-DATE              TO ADATEO.
01707      MOVE EIBTIME                TO TIME-IN.
01708      MOVE TIME-OUT               TO ATIMEO.
01709      MOVE EMI-MESSAGE-AREA (1)   TO AERRMSGO.
01710      
      * EXEC CICS SEND
01711 *        MAP      (MAP-NAME)
01712 *        MAPSET   (MAPSET-NAME)
01713 *        FROM     (EL610AO)
01714 *        DATAONLY
01715 *        CURSOR
01716 *    END-EXEC.
           MOVE LENGTH OF
            EL610AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004227' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL610AO, 
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
           
01717
01718      GO TO 9100-RETURN-TRAN.
01719
01720      EJECT
01721  8300-SEND-TEXT         SECTION.
01722      
      * EXEC CICS SEND TEXT
01723 *        FROM     (LOGOFF-TEXT)
01724 *        LENGTH   (LOGOFF-LENGTH)
01725 *        ERASE
01726 *        FREEKB
01727 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004239' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323339' TO DFHEIV0(25:11)
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
           
01728
01729      
      * EXEC CICS RETURN
01730 *    END-EXEC.
      *    MOVE '.(                    &   #00004246' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01731
01732      EJECT
01733  8400-LOG-JOURNAL-RECORD         SECTION.
01734      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
01735      MOVE ERLOFC-ID              TO JP-FILE-ID.
01736      MOVE THIS-PGM               TO JP-PROGRAM-ID.
pemuni*    EXEC CICS JOURNAL
pemuni*        JFILEID     (PI-JOURNAL-FILE-ID)
pemuni*        JTYPEID     ('CR')
pemuni*        FROM        (JOURNAL-RECORD)
pemuni*        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
pemuni*    END-EXEC.
01743
01744  8400-EXIT.
01745      EXIT.
01746
01747
01748  8600-DEEDIT           SECTION.
01749      
      * EXEC CICS BIF DEEDIT
01750 *         FIELD   (DEEDIT-FIELD)
01751 *         LENGTH  (15)
01752 *     END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004266' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01753
01754  8800-UNAUTHORIZED-ACCESS        SECTION.
01755      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01756      GO TO 8300-SEND-TEXT.
01757
01758  8810-PF23              SECTION.
01759      MOVE EIBAID                 TO PI-ENTRY-CD-1.
01760      MOVE XCTL-005               TO PGM-NAME.
01761      GO TO 9300-XCTL.
01762
01763  9000-RETURN-CICS       SECTION.
01764      
      * EXEC CICS RETURN
01765 *    END-EXEC.
      *    MOVE '.(                    &   #00004281' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01766
01767  9100-RETURN-TRAN       SECTION.
01768      MOVE    EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01769      MOVE SCREEN-NUMBER             TO PI-CURRENT-SCREEN-NO.
01770      
      * EXEC CICS RETURN
01771 *        TRANSID    (TRANS-ID)
01772 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01773 *        LENGTH     (PI-COMM-LENGTH)
01774 *    END-EXEC.
      *    MOVE '.(CT                  &   #00004287' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01775
01776  9200-RETURN-MAIN-MENU SECTION.
01777      MOVE XCTL-626               TO PGM-NAME.
01778      GO TO 9300-XCTL.
01779
01780  9300-XCTL             SECTION.
01781      
      * EXEC CICS XCTL
01782 *        PROGRAM    (PGM-NAME)
01783 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01784 *        LENGTH     (PI-COMM-LENGTH)
01785 *    END-EXEC.
      *    MOVE '.$C                   $   #00004298' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01786
01787  9400-CLEAR            SECTION.
01788      MOVE SPACES                 TO PI-CR-CONTROL-IN-PROGRESS
01789                                     PI-LOAN-OFFICER.
01790
01791      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
01792      GO TO 9300-XCTL.
01793
01794  9500-PF12             SECTION.
01795      MOVE XCTL-010               TO PGM-NAME.
01796      GO TO 9300-XCTL.
01797
01798  9600-PGMID-ERROR      SECTION.
01799      
      * EXEC CICS HANDLE CONDITION
01800 *        PGMIDERR    (8300-SEND-TEXT)
01801 *        END-EXEC.
      *    MOVE '"$L                   ! 0 #00004316' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303034333136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01802
01803      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
01804      MOVE ' '                    TO PI-ENTRY-CD-1.
01805      MOVE XCTL-005               TO PGM-NAME.
01806      MOVE PGM-NAME               TO LOGOFF-PGM.
01807      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01808      GO TO 9300-XCTL.
01809
01810  9700-DATE-LINK         SECTION.
01811      MOVE LINK-ELDATCV           TO PGM-NAME
01812      
      * EXEC CICS LINK
01813 *        PROGRAM    (PGM-NAME)
01814 *        COMMAREA   (DATE-CONVERSION-DATA)
01815 *        LENGTH     (DC-COMM-LENGTH)
01816 *    END-EXEC.
      *    MOVE '."C                   ''   #00004329' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01817
01818  9900-ERROR-FORMAT       SECTION.
01819      IF NOT EMI-ERRORS-COMPLETE
01820          MOVE LINK-001           TO PGM-NAME
01821          
      * EXEC CICS LINK
01822 *            PROGRAM    (PGM-NAME)
01823 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01824 *            LENGTH     (EMI-COMM-LENGTH)
01825 *        END-EXEC.
      *    MOVE '."C                   ''   #00004338' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01826
01827  9900-EXIT.
01828      EXIT.
01829
01830  9990-ABEND             SECTION.
01831      MOVE LINK-004               TO PGM-NAME.
01832      MOVE DFHEIBLK               TO EMI-LINE1
01833
01834      
      * EXEC CICS LINK
01835 *        PROGRAM   (PGM-NAME)
01836 *        COMMAREA  (EMI-LINE1)
01837 *        LENGTH    (72)
01838 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00004351' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01839
01840      MOVE -1                     TO AMAINTL.
01841      GO TO 8200-SEND-DATAONLY.
01842
01843  9995-SECURITY-VIOLATION.
01844 *           COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00004378' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
01845
01846  9995-EXIT.
01847       EXIT.
01848
01849

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL610' TO DFHEIV1
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
               GO TO 2890-OFFICER-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 7080-END-OF-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7070-END-OF-BROWSE,
                     7070-END-OF-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7180-END-OF-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7170-END-OF-BROWSE,
                     7170-END-OF-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7280-END-OF-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 7270-END-OF-BROWSE,
                     7270-END-OF-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 7380-END-OF-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7370-END-OF-BROWSE,
                     7370-END-OF-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 7440-OFFICER-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 7480-OFFICER-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 7550-DUPLICATE-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 7870-ACCOUNT-INVALID
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL610' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
