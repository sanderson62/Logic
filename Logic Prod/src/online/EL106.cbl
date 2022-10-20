00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL106.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:41:00.
00007 *                            VMOD=2.026.
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
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022
00023 *REMARKS.    TRANSACTION- EX10 - STATE MAINTENANCE
100108******************************************************************
100108*                   C H A N G E   L O G
100108*
100108* Changes are marked by the Change Effective date.
100108*-----------------------------------------------------------------
100108*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100108* EFFECTIVE    NUMBER
100108*-----------------------------------------------------------------
100108* 100108    2008022800002  AJRA  ADD CHECK NUMBER TO STATE FOR AK
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
011211* 011211    2010030900001  AJRA  ADD OPTION 4 AND 5 TO REFUND IND
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
031512* 031512    2011120900003  AJRA  ADD AHL COMPANY CODE
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032514* 032514    2013111100001  AJRA  VERIFY SSN FOR DISAB PMTS
102717* 102717  CR2017062000003  PEMA  MOVE COM CAP STUFF TO EL1062
100108******************************************************************
00024
00025      EJECT
00026  ENVIRONMENT DIVISION.
00027  DATA DIVISION.
00028  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00029  77  FILLER  PIC X(32)  VALUE '********************************'.
00030  77  FILLER  PIC X(32)  VALUE '*    EL106 WORKING STORAGE     *'.
00031  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.026 ***********'.
00032
00033 *                                COPY ELCSCTM.
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
00034 *                                COPY ELCSCRTY.
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
092308*                                COPY MPCSCRT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCSCRT                             *
00004 *                            VMOD=1.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        ACQUIRED BY SIGN-ON PROGRAM EL125.                      *
00008 *                                      (MP MORTGAGE PROTECTION)  *
00009 *                                                                *
00010 ******************************************************************
00011 *
00012  01  SECURITY-CONTROL-E.
00013      12  SC-COMM-LENGTH-E             PIC S9(04) VALUE +144 COMP.
00014      12  FILLER                       PIC  X(02) VALUE 'SC'.
00015      12  SC-QUID-KEY.
00016          16  SC-QUID-TERMINAL         PIC  X(04).
00017          16  SC-QUID-SYSTEM           PIC  X(04).
00018      12  SC-ITEM                      PIC S9(04) VALUE +1   COMP.
00019      12  SC-SECURITY-ACCESS-CODE      PIC  X(01).
00020      12  SC-PRODUCER-AUTHORIZED-SW    PIC  X(01).
00021          88 SC-PRODUCER-AUTHORIZED               VALUE ' '.
00022          88 SC-PRODUCER-NOT-AUTHORIZED           VALUE 'N'.
00023      12  SC-MP-CODES.
00024          16  SC-MP-AUTHORIZATION OCCURS 44 TIMES.
00025              20  SC-MP-DISPLAY        PIC  X(01).
00026              20  SC-MP-UPDATE         PIC  X(01).
00027      12  FILLER                       PIC  X(40).
00035
00036  01  WS-DATE-AREA.
00037      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00038      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
00039
00040  01  WS.
092308     05  W-APPL-SCRTY-NDX    PIC S9(04) COMP   VALUE +29.
092308     05  SC-ITEM-CL-CR       PIC S9(4)         VALUE +1   COMP.
00041      05  WS-COMM-LENGTH      PIC S9(4) COMP VALUE +1500.
00042      05  WS-MAP-LENGTH       PIC S9(4) COMP VALUE +500.
00043      05  RETURNED-FROM       PIC X(8)    VALUE SPACES.
00044      05  QID.
00045          16  QID-TERM        PIC X(4).
00046          16  FILLER          PIC X(4)    VALUE '106A'.
00047
00048      05  WS-ST-TOL-PREM-PCT  PIC S9V9(4)    VALUE +0.
00049      05  WS-ST-TOL-REF-PCT   PIC S9V9(4)    VALUE +0.
00050      05  WS-ST-OVR-SHT-PCT   PIC S9V9(4)    VALUE +0.
PEMMOD     05  WS-ST-LF-PREM-TAX   PIC S9V9(4)    VALUE +0.
PEMMOD     05  WS-ST-AH-PREM-TAX-I PIC S9V9(4)    VALUE +0.
PEMMOD     05  WS-ST-AH-PREM-TAX-G PIC S9V9(4)    VALUE +0.
00051      05  WS-ST-LF-EXP-PCT    PIC S9(3)V9(4) VALUE +0.
00052      05  WS-ST-AH-EXP-PCT    PIC S9(3)V9(4) VALUE +0.
00053      05  WS-ST-TARGET-LOSS-RATIO
00054                              PIC S9V9(4)    VALUE +0.
00055      05  WS-ST-CALC-INTEREST PIC S9V9(4)    VALUE +0.
00060      05  WS-RESIDENT-TAX     PIC S9V9(4)    VALUE +0.
00061      05  WS-IRATE            PIC S9V9(4)    VALUE +0.
00062      05  WS-IRATE1           PIC S9V9(4)    VALUE +0.
00063      05  WS-IRATE2           PIC S9V9(4)    VALUE +0.
00064      05  WS-IRATE3           PIC S9V9(4)    VALUE +0.
00065      05  WS-FREE-LOOK-DAYS   PIC S9(3)      VALUE +0.
00066
00067  01  STANDARD-AREAS.
00068      12  MAP-NAME            PIC X(8)    VALUE 'EL106A'.
00069      12  MAPSET-NAME         PIC X(8)    VALUE 'EL106S'.
00070      12  TRANS-ID            PIC X(4)    VALUE 'EX10'.
00071      12  PGM-NAME            PIC X(8).
00072      12  TIME-IN             PIC S9(7).
00073      12  TIME-OUT-R  REDEFINES TIME-IN.
00074          16  FILLER          PIC X.
00075          16  TIME-OUT        PIC 99V99.
00076          16  FILLER          PIC XX.
00077      12  XCTL-EL005          PIC X(8)    VALUE 'EL005'.
00078      12  XCTL-EL010          PIC X(8)    VALUE 'EL010'.
00079      12  XCTL-EL126          PIC X(8)    VALUE 'EL126'.
00080      12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.
00081      12  XCTL-EL6565         PIC X(8)    VALUE 'EL6565'.
00082      12  XCTL-EL6507         PIC X(8)    VALUE 'EL6507'.
00083      12  XCTL-EL1061         PIC X(8)    VALUE 'EL1061'.
102717     12  XCTL-EL1062         PIC X(8)    VALUE 'EL1062'.
00084      12  XCTL-EM626          PIC X(8)    VALUE 'EM626'.
00085      12  XCTL-GL800          PIC X(8)    VALUE 'GL800'.
00086      12  LINK-EL001          PIC X(8)    VALUE 'EL001'.
00087      12  LINK-EL004          PIC X(8)    VALUE 'EL004'.
00088      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00089      12  THIS-PGM            PIC X(8)    VALUE 'EL106'.
00090      12  ELCNTL-ID           PIC X(8)    VALUE 'ELCNTL'.
00091      12  ELLETR-ID           PIC X(8)    VALUE 'ELLETR'.
00092      12  SUB                 PIC S9(4)   COMP.
00093      12  GETMAIN-SPACE       PIC X       VALUE SPACE.
00094      12  WS-FIRST-TIME-SW    PIC X       VALUE 'Y'.
00095          88  FIRST-TIME                  VALUE 'Y'.
00096      12  WS-DISPLAY-SW       PIC X       VALUE 'N'.
00097          88  RETURN-DISPLAY              VALUE 'Y'.
00098
00099  01  ACCESS-KEYS.
00100      12  ELCNTL-KEY.
00101          16  CK-COMP-ID      PIC X(3).
00102          16  FILLER          PIC X       VALUE '3'.
00103          16  CK-STATE-CD     PIC X(4)    VALUE SPACES.
00104          16  CK-SEQ          PIC S9(4)   VALUE +0    COMP.
00105      12  W-WORKING-TEXT-KEY.
00106          16  W-TEXT-COMPANY-CD
00107                              PIC X.
00108          16  W-TEXT-FORM-NO  PIC X(12).
00109          16  W-TEXT-LINE-SEQ PIC S9(4)   COMP.
00110
00111  01  ERROR-MESSAGES.
00112      12  ER-0000                 PIC X(4)  VALUE '0000'.
00113      12  ER-0004                 PIC X(4)  VALUE '0004'.
00114      12  ER-0013                 PIC X(4)  VALUE '0013'.
00115      12  ER-0023                 PIC X(4)  VALUE '0023'.
00116      12  ER-0029                 PIC X(4)  VALUE '0029'.
00117      12  ER-0042                 PIC X(4)  VALUE '0042'.
00118      12  ER-0050                 PIC X(4)  VALUE '0050'.
00119      12  ER-0068                 PIC X(4)  VALUE '0068'.
00120      12  ER-0070                 PIC X(4)  VALUE '0070'.
00121      12  ER-0141                 PIC X(4)  VALUE '0141'.
00122      12  ER-0144                 PIC X(4)  VALUE '0144'.
00123      12  ER-0145                 PIC X(4)  VALUE '0145'.
00124      12  ER-0146                 PIC X(4)  VALUE '0146'.
00125      12  ER-0147                 PIC X(4)  VALUE '0147'.
00126      12  ER-0148                 PIC X(4)  VALUE '0148'.
00127      12  ER-0149                 PIC X(4)  VALUE '0149'.
00128      12  ER-0150                 PIC X(4)  VALUE '0150'.
00129      12  ER-0151                 PIC X(4)  VALUE '0151'.
00130      12  ER-0152                 PIC X(4)  VALUE '0152'.
00131      12  ER-0153                 PIC X(4)  VALUE '0153'.
00132      12  ER-0159                 PIC X(4)  VALUE '0159'.
00133      12  ER-0160                 PIC X(4)  VALUE '0160'.
00134      12  ER-0161                 PIC X(4)  VALUE '0161'.
CIDMOD     12  ER-0582                 PIC X(4)  VALUE '0582'.
00135      12  ER-0805                 PIC X(4)  VALUE '0805'.
00136      12  ER-1614                 PIC X(4)  VALUE '1614'.
102717     12  er-1964                 pic x(4)  value '1964'.
00137      12  ER-2009                 PIC X(4)  VALUE '2009'.
00138      12  ER-2010                 PIC X(4)  VALUE '2010'.
00139      12  ER-2012                 PIC X(4)  VALUE '2012'.
00140      12  ER-2014                 PIC X(4)  VALUE '2014'.
00141      12  ER-2024                 PIC X(4)  VALUE '2024'.
00142      12  ER-2028                 PIC X(4)  VALUE '2028'.
00143      12  ER-2032                 PIC X(4)  VALUE '2032'.
00144      12  ER-2033                 PIC X(4)  VALUE '2033'.
PEMMOD     12  ER-2082                 PIC X(4)  VALUE '2082'.
PEMMOD     12  ER-2084                 PIC X(4)  VALUE '2084'.
00145      12  ER-2137                 PIC X(4)  VALUE '2137'.
00146      12  ER-2298                 PIC X(4)  VALUE '2298'.
00147      12  ER-2299                 PIC X(4)  VALUE '2299'.
00148      12  ER-3030                 PIC X(4)  VALUE '3030'.
00149      12  ER-3031                 PIC X(4)  VALUE '3031'.
00150      12  ER-3032                 PIC X(4)  VALUE '3032'.
00151      12  ER-3033                 PIC X(4)  VALUE '3033'.
00152      12  ER-3034                 PIC X(4)  VALUE '3034'.
00153      12  ER-3035                 PIC X(4)  VALUE '3035'.
011410     12  ER-3036                 PIC X(4)  VALUE '3036'.
061511     12  ER-3040                 PIC X(4)  VALUE '3040'.
102717     12  er-3064                 pic x(4)  value '3064'.
00154      12  ER-7008                 PIC X(4)  VALUE '7008'.
00155      12  ER-7346                 PIC X(4)  VALUE '7346'.
00156      12  ER-7531                 PIC X(4)  VALUE '7531'.
00157      12  ER-7532                 PIC X(4)  VALUE '7532'.
00158      12  ER-7536                 PIC X(4)  VALUE '7536'.
012913     12  ER-7578                 PIC X(4)  VALUE '7578'.
032514     12  ER-7581                 PIC X(4)  VALUE '7581'.
00159      12  ER-7717                 PIC X(4)  VALUE '7717'.
00160      12  ER-7735                 PIC X(4)  VALUE '7735'.
00161      12  ER-8159                 PIC X(4)  VALUE '8159'.
00162      12  ER-9074                 PIC X(4)  VALUE '9074'.
092308     12  ER-9097                 PIC X(4)  VALUE '9097'.
00163      12  ER-9447                 PIC X(4)  VALUE '9447'.
00164      12  ER-9448                 PIC X(4)  VALUE '9448'.
00165      12  ER-9478                 PIC X(4)  VALUE '9478'.
PEMMOD     12  ER-9999                 PIC X(4)  VALUE '9999'.
00166      EJECT
00167 *    COPY ELCDATE.
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
00168      EJECT
00169 *    COPY ELCLOGOF.
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
00170      EJECT
00171 *    COPY ELCATTR.
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
00172      EJECT
00173 *    COPY ELCEMIB.
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
00174      EJECT
00175 *    COPY ELCJPFX.
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
           12  jp-date                     pic s9(5) comp-3.
           12  jp-time                     pic s9(7) comp-3.
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
00176              PIC X(530).
00177      EJECT
00178 *    COPY ELCINTF.
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
00179      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00180          16  FILLER              PIC X(101).
00181          16  PI-WS-STATE         PIC XX.
00182          16  PI-WS-CLASS         PIC XX.
00183          16  PI-WS-DEV           PIC XXX.
00184          16  PI-WS-TYPE          PIC X.
00185          16  PI-WS-PLAN          PIC XX.
00186          16  PI-PREV-STATE       PIC X(4).
00187          16  FILLER              PIC X(525).
00188
00189      EJECT
00190 *    COPY ELCAID.
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
00191  01  FILLER REDEFINES DFHAID.
00192      12  FILLER              PIC X(8).
00193      12  PF-VALUES           PIC X       OCCURS 2.
00194
00195      EJECT
00196 *    COPY EL106S.
       01  EL106AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEL PIC S9(0004) COMP.
           05  RUNDTEF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEF.
               10  RUNDTEA PIC  X(0001).
           05  RUNDTEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  LSTUSRL PIC S9(0004) COMP.
           05  LSTUSRF PIC  X(0001).
           05  FILLER REDEFINES LSTUSRF.
               10  LSTUSRA PIC  X(0001).
           05  LSTUSRI PIC  X(0004).
      *    -------------------------------
           05  LSTDTEL PIC S9(0004) COMP.
           05  LSTDTEF PIC  X(0001).
           05  FILLER REDEFINES LSTDTEF.
               10  LSTDTEA PIC  X(0001).
           05  LSTDTEI PIC  X(0008).
      *    -------------------------------
           05  LSTTIMEL PIC S9(0004) COMP.
           05  LSTTIMEF PIC  X(0001).
           05  FILLER REDEFINES LSTTIMEF.
               10  LSTTIMEA PIC  X(0001).
           05  LSTTIMEI PIC  X(0005).
      *    -------------------------------
           05  STCDL PIC S9(0004) COMP.
           05  STCDF PIC  X(0001).
           05  FILLER REDEFINES STCDF.
               10  STCDA PIC  X(0001).
           05  STCDI PIC  X(0002).
      *    -------------------------------
           05  STABRL PIC S9(0004) COMP.
           05  STABRF PIC  X(0001).
           05  FILLER REDEFINES STABRF.
               10  STABRA PIC  X(0001).
           05  STABRI PIC  X(0002).
      *    -------------------------------
           05  STNAMEL PIC S9(0004) COMP.
           05  STNAMEF PIC  X(0001).
           05  FILLER REDEFINES STNAMEF.
               10  STNAMEA PIC  X(0001).
           05  STNAMEI PIC  X(0025).
      *    -------------------------------
           05  LEXPLBLL PIC S9(0004) COMP.
           05  LEXPLBLF PIC  X(0001).
           05  FILLER REDEFINES LEXPLBLF.
               10  LEXPLBLA PIC  X(0001).
           05  LEXPLBLI PIC  X(0006).
      *    -------------------------------
           05  LFEXPL PIC S9(0004) COMP.
           05  LFEXPF PIC  X(0001).
           05  FILLER REDEFINES LFEXPF.
               10  LFEXPA PIC  X(0001).
           05  LFEXPI PIC  S9(3)V9999.
      *    -------------------------------
           05  AEXPLBLL PIC S9(0004) COMP.
           05  AEXPLBLF PIC  X(0001).
           05  FILLER REDEFINES AEXPLBLF.
               10  AEXPLBLA PIC  X(0001).
           05  AEXPLBLI PIC  X(0006).
      *    -------------------------------
           05  AHEXPL PIC S9(0004) COMP.
           05  AHEXPF PIC  X(0001).
           05  FILLER REDEFINES AHEXPF.
               10  AHEXPA PIC  X(0001).
           05  AHEXPI PIC  S9(3)V9999.
      *    -------------------------------
           05  REMTERML PIC S9(0004) COMP.
           05  REMTERMF PIC  X(0001).
           05  FILLER REDEFINES REMTERMF.
               10  REMTERMA PIC  X(0001).
           05  REMTERMI PIC  X(0001).
      *    -------------------------------
           05  QUOTCALL PIC S9(0004) COMP.
           05  QUOTCALF PIC  X(0001).
           05  FILLER REDEFINES QUOTCALF.
               10  QUOTCALA PIC  X(0001).
           05  QUOTCALI PIC  S9(4)V99.
      *    -------------------------------
           05  PREMTOLL PIC S9(0004) COMP.
           05  PREMTOLF PIC  X(0001).
           05  FILLER REDEFINES PREMTOLF.
               10  PREMTOLA PIC  X(0001).
           05  PREMTOLI PIC  S9(4)V99.
      *    -------------------------------
           05  REFTOLL PIC S9(0004) COMP.
           05  REFTOLF PIC  X(0001).
           05  FILLER REDEFINES REFTOLF.
               10  REFTOLA PIC  X(0001).
           05  REFTOLI PIC  S9(4)V99.
      *    -------------------------------
           05  PRMPCTL PIC S9(0004) COMP.
           05  PRMPCTF PIC  X(0001).
           05  FILLER REDEFINES PRMPCTF.
               10  PRMPCTA PIC  X(0001).
           05  PRMPCTI PIC  S9(1)V9(4).
      *    -------------------------------
           05  REFPCTL PIC S9(0004) COMP.
           05  REFPCTF PIC  X(0001).
           05  FILLER REDEFINES REFPCTF.
               10  REFPCTA PIC  X(0001).
           05  REFPCTI PIC  S9(1)V9(4).
      *    -------------------------------
           05  OVSAMTL PIC S9(0004) COMP.
           05  OVSAMTF PIC  X(0001).
           05  FILLER REDEFINES OVSAMTF.
               10  OVSAMTA PIC  X(0001).
           05  OVSAMTI PIC  S9(4)V99.
      *    -------------------------------
           05  OVSPCTL PIC S9(0004) COMP.
           05  OVSPCTF PIC  X(0001).
           05  FILLER REDEFINES OVSPCTF.
               10  OVSPCTA PIC  X(0001).
           05  OVSPCTI PIC  S9(1)V9(4).
      *    -------------------------------
           05  AGTSIGL PIC S9(0004) COMP.
           05  AGTSIGF PIC  X(0001).
           05  FILLER REDEFINES AGTSIGF.
               10  AGTSIGA PIC  X(0001).
           05  AGTSIGI PIC  X(0001).
      *    -------------------------------
           05  CLREJECL PIC S9(0004) COMP.
           05  CLREJECF PIC  X(0001).
           05  FILLER REDEFINES CLREJECF.
               10  CLREJECA PIC  X(0001).
           05  CLREJECI PIC  X(0001).
      *    -------------------------------
           05  ISSREJL PIC S9(0004) COMP.
           05  ISSREJF PIC  X(0001).
           05  FILLER REDEFINES ISSREJF.
               10  ISSREJA PIC  X(0001).
           05  ISSREJI PIC  X(0001).
      *    -------------------------------
           05  REFREJL PIC S9(0004) COMP.
           05  REFREJF PIC  X(0001).
           05  FILLER REDEFINES REFREJF.
               10  REFREJA PIC  X(0001).
           05  REFREJI PIC  X(0001).
      *    -------------------------------
           05  REFMINL PIC S9(0004) COMP.
           05  REFMINF PIC  X(0001).
           05  FILLER REDEFINES REFMINF.
               10  REFMINA PIC  X(0001).
           05  REFMINI PIC  S9(4)V99.
      *    -------------------------------
           05  REFDAY1L PIC S9(0004) COMP.
           05  REFDAY1F PIC  X(0001).
           05  FILLER REDEFINES REFDAY1F.
               10  REFDAY1A PIC  X(0001).
           05  REFDAY1I PIC  X(0002).
      *    -------------------------------
           05  REFDAYSL PIC S9(0004) COMP.
           05  REFDAYSF PIC  X(0001).
           05  FILLER REDEFINES REFDAYSF.
               10  REFDAYSA PIC  X(0001).
           05  REFDAYSI PIC  X(0002).
      *    -------------------------------
           05  SPLPMTL PIC S9(0004) COMP.
           05  SPLPMTF PIC  X(0001).
           05  FILLER REDEFINES SPLPMTF.
               10  SPLPMTA PIC  X(0001).
           05  SPLPMTI PIC  X(0001).
      *    -------------------------------
           05  STUEL PIC S9(0004) COMP.
           05  STUEF PIC  X(0001).
           05  FILLER REDEFINES STUEF.
               10  STUEA PIC  X(0001).
           05  STUEI PIC  X(0001).
      *    -------------------------------
           05  STCNTLL PIC S9(0004) COMP.
           05  STCNTLF PIC  X(0001).
           05  FILLER REDEFINES STCNTLF.
               10  STCNTLA PIC  X(0001).
           05  STCNTLI PIC  X(0001).
      *    -------------------------------
           05  STDEVL PIC S9(0004) COMP.
           05  STDEVF PIC  X(0001).
           05  FILLER REDEFINES STDEVF.
               10  STDEVA PIC  X(0001).
           05  STDEVI PIC  X(0003).
      *    -------------------------------
           05  EXTDAYSL PIC S9(0004) COMP.
           05  EXTDAYSF PIC  X(0001).
           05  FILLER REDEFINES EXTDAYSF.
               10  EXTDAYSA PIC  X(0001).
           05  EXTDAYSI PIC  X(0003).
      *    -------------------------------
           05  EXTCHGL PIC S9(0004) COMP.
           05  EXTCHGF PIC  X(0001).
           05  FILLER REDEFINES EXTCHGF.
               10  EXTCHGA PIC  X(0001).
           05  EXTCHGI PIC  X(0001).
      *    -------------------------------
           05  LFTAXL PIC S9(0004) COMP.
           05  LFTAXF PIC  X(0001).
           05  FILLER REDEFINES LFTAXF.
               10  LFTAXA PIC  X(0001).
           05  LFTAXI PIC  S9(1)V9(4).
      *    -------------------------------
           05  REPLAWL PIC S9(0004) COMP.
           05  REPLAWF PIC  X(0001).
           05  FILLER REDEFINES REPLAWF.
               10  REPLAWA PIC  X(0001).
           05  REPLAWI PIC  X(0001).
      *    -------------------------------
           05  REPLETRL PIC S9(0004) COMP.
           05  REPLETRF PIC  X(0001).
           05  FILLER REDEFINES REPLETRF.
               10  REPLETRA PIC  X(0001).
           05  REPLETRI PIC  X(0004).
      *    -------------------------------
           05  AHITAXL PIC S9(0004) COMP.
           05  AHITAXF PIC  X(0001).
           05  FILLER REDEFINES AHITAXF.
               10  AHITAXA PIC  X(0001).
           05  AHITAXI PIC  S9(1)V9(4).
      *    -------------------------------
           05  TARRATL PIC S9(0004) COMP.
           05  TARRATF PIC  X(0001).
           05  FILLER REDEFINES TARRATF.
               10  TARRATA PIC  X(0001).
           05  TARRATI PIC  S9(2)V9(4).
      *    -------------------------------
           05  CALCINTL PIC S9(0004) COMP.
           05  CALCINTF PIC  X(0001).
           05  FILLER REDEFINES CALCINTF.
               10  CALCINTA PIC  X(0001).
           05  CALCINTI PIC  S9(2)V9(4).
      *    -------------------------------
           05  AHGTAXL PIC S9(0004) COMP.
           05  AHGTAXF PIC  X(0001).
           05  FILLER REDEFINES AHGTAXF.
               10  AHGTAXA PIC  X(0001).
           05  AHGTAXI PIC  S9(1)V9(4).
      *    -------------------------------
           05  CCREQL PIC S9(0004) COMP.
           05  CCREQF PIC  X(0001).
           05  FILLER REDEFINES CCREQF.
               10  CCREQA PIC  X(0001).
           05  CCREQI PIC  X(0001).
      *    -------------------------------
           05  XINTL PIC S9(0004) COMP.
           05  XINTF PIC  X(0001).
           05  FILLER REDEFINES XINTF.
               10  XINTA PIC  X(0001).
           05  XINTI PIC  X.
      *    -------------------------------
           05  XPMTSL PIC S9(0004) COMP.
           05  XPMTSF PIC  X(0001).
           05  FILLER REDEFINES XPMTSF.
               10  XPMTSA PIC  X(0001).
           05  XPMTSI PIC  X.
      *    -------------------------------
           05  LFREDL PIC S9(0004) COMP.
           05  LFREDF PIC  X(0001).
           05  FILLER REDEFINES LFREDF.
               10  LFREDA PIC  X(0001).
           05  LFREDI PIC  X(0001).
      *    -------------------------------
           05  LFLEVL PIC S9(0004) COMP.
           05  LFLEVF PIC  X(0001).
           05  FILLER REDEFINES LFLEVF.
               10  LFLEVA PIC  X(0001).
           05  LFLEVI PIC  X(0001).
      *    -------------------------------
           05  LFNETL PIC S9(0004) COMP.
           05  LFNETF PIC  X(0001).
           05  FILLER REDEFINES LFNETF.
               10  LFNETA PIC  X(0001).
           05  LFNETI PIC  X(0001).
      *    -------------------------------
           05  AHAHL PIC S9(0004) COMP.
           05  AHAHF PIC  X(0001).
           05  FILLER REDEFINES AHAHF.
               10  AHAHA PIC  X(0001).
           05  AHAHI PIC  X(0001).
      *    -------------------------------
           05  AHCPL PIC S9(0004) COMP.
           05  AHCPF PIC  X(0001).
           05  FILLER REDEFINES AHCPF.
               10  AHCPA PIC  X(0001).
           05  AHCPI PIC  X(0001).
      *    -------------------------------
           05  STCHKHDL PIC S9(0004) COMP.
           05  STCHKHDF PIC  X(0001).
           05  FILLER REDEFINES STCHKHDF.
               10  STCHKHDA PIC  X(0001).
           05  STCHKHDI PIC  X(0006).
      *    -------------------------------
           05  STCHKNOL PIC S9(0004) COMP.
           05  STCHKNOF PIC  X(0001).
           05  FILLER REDEFINES STCHKNOF.
               10  STCHKNOA PIC  X(0001).
           05  STCHKNOI PIC  9(8).
      *    -------------------------------
           05  RESTAXL PIC S9(0004) COMP.
           05  RESTAXF PIC  X(0001).
           05  FILLER REDEFINES RESTAXF.
               10  RESTAXA PIC  X(0001).
           05  RESTAXI PIC  S99V9(4).
      *    -------------------------------
           05  FREELKL PIC S9(0004) COMP.
           05  FREELKF PIC  X(0001).
           05  FILLER REDEFINES FREELKF.
               10  FREELKA PIC  X(0001).
           05  FREELKI PIC  X(0003).
      *    -------------------------------
           05  CAUSALL PIC S9(0004) COMP.
           05  CAUSALF PIC  X(0001).
           05  FILLER REDEFINES CAUSALF.
               10  CAUSALA PIC  X(0001).
           05  CAUSALI PIC  X(0001).
      *    -------------------------------
           05  REFCLML PIC S9(0004) COMP.
           05  REFCLMF PIC  X(0001).
           05  FILLER REDEFINES REFCLMF.
               10  REFCLMA PIC  X(0001).
           05  REFCLMI PIC  X(0001).
      *    -------------------------------
           05  VFYBENEL PIC S9(0004) COMP.
           05  VFYBENEF PIC  X(0001).
           05  FILLER REDEFINES VFYBENEF.
               10  VFYBENEA PIC  X(0001).
           05  VFYBENEI PIC  X(0001).
      *    -------------------------------
           05  NETONLYL PIC S9(0004) COMP.
           05  NETONLYF PIC  X(0001).
           05  FILLER REDEFINES NETONLYF.
               10  NETONLYA PIC  X(0001).
           05  NETONLYI PIC  X(0001).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0076).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0076).
      *    -------------------------------
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  9(2).
       01  EL106AO REDEFINES EL106AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTUSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STCDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STABRO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STNAMEO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LEXPLBLO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFEXPO PIC  99.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEXPLBLO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEXPO PIC  99.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REMTERMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  QUOTCALO PIC  ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PREMTOLO PIC  ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFTOLO PIC  ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRMPCTO PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFPCTO PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OVSAMTO PIC  ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OVSPCTO PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTSIGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLREJECO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ISSREJO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFREJO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFMINO PIC  ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFDAY1O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFDAYSO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SPLPMTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STUEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STCNTLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STDEVO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTDAYSO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTCHGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTAXO PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPLAWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPLETRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHITAXO PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TARRATO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CALCINTO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHGTAXO PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCREQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  XINTO PIC  9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  XPMTSO PIC  9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFREDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFLEVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFNETO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHAHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHCPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STCHKHDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STCHKNOO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RESTAXO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FREELKO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAUSALO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFCLMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VFYBENEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NETONLYO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
00197      EJECT
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
00199  01  DFHCOMMAREA             PIC X(1500).
00200 *01 PARMLIST .
00201 *    02  FILLER              PIC S9(8)   COMP.
00202 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.
00203 *    02  ELLETR-POINTER      PIC S9(8)   COMP.
00204      EJECT
00205 *    COPY ELCCNTL.
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
00206      EJECT
00207 *    COPY ELCTEXT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCTEXT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT FILES FOR HELP DISPLAY,              *
00008 *                                     FORM LETTERS,              *
00009 *                                     CERT FORM DISPLAY.
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 100   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ELLETR (LETTERS)   RKP=2,LEN=15          *
00015 *       ALTERNATE INDEX = NONE                                   *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ELFORM (FORMS)     RKP=2,LEN=15          *
00018 *       ALTERNATE INDEX = NONE                                   *
00019 *                                                                *
00020 *   BASE CLUSTER NAME = ELHELP (HELP)      RKP=2,LEN=15          *
00021 *       ALTERNATE INDEX = NONE                                   *
00022 *                                                                *
00023 *   LOG = NO                                                     *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  TEXT-FILES.
00027      12  TEXT-FILE-ID                PIC XX.
00028          88  FORMS-FILE-TEXT            VALUE 'TF'.
00029          88  LETTER-FILE-TEXT           VALUE 'TL'.
00030          88  HELP-FILE-TEXT             VALUE 'TH'.
00031
00032      12  TX-CONTROL-PRIMARY.
00033          16  TX-COMPANY-CD           PIC X.
00034              88  TX-SYSTEM-WIDE-FILE    VALUE LOW-VALUE.
00035          16  TX-ACCESS-CD-GENL       PIC X(12).
00036
00037          16  TX-LETTER-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00038              20  TX-LETTER-NO        PIC X(4).
00039              20  FILLER              PIC X(8).
00040
00041          16  TX-FORM-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00042              20  TX-FORM-NO          PIC X(12).
00043
00044          16  TX-HELP-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00045              20  TX-HELP-TYPE        PIC X.
00046                  88  HELP-FOR-GENERAL   VALUE ' '.
00047                  88  HELP-BY-SCREEN     VALUE 'S'.
00048                  88  HELP-BY-ERROR      VALUE 'E'.
00049              20  TX-SCREEN-OR-ERROR  PIC X(4).
00050                  88  GENERAL-INFO-HELP  VALUE '0000'.
00051              20  TX-HELP-FOR-COMPANY  PIC XXX.
00052                  88  NOT-COMPANY-SPECIFIC VALUE '   '.
00053              20  FILLER              PIC X(4).
00054
00055          16  TX-LINE-SEQUENCE        PIC S9(4)     COMP.
00056
00057      12  TX-PROCESS-CONTROL          PIC XX.
00058          88  LETTER-LINE-SKIPS          VALUE '01' THRU '99'.
00059
00060      12  TX-TEXT-LINE                PIC X(70).
00061
00062      12  TX-FORM-SQUEEZE-CONTROL     PIC X.
00063          88  TX-FORM-SQUEEZE-ON         VALUE 'Y'.
00064          88  TX-FORM-SQUEEZE-OFF        VALUE SPACES.
00065          88  TX-VALID-FORM-SQUEEZE-VALUE
00066                                         VALUE 'Y' ' '.
00067
00068      12  TX-LINE-SQUEEZE-CONTROL     PIC X.
00069          88  TX-ADJUST-TO-LINE-LENGTH   VALUE 'A'.
00070          88  TX-CONTINUE-PARAGRAPH      VALUE 'C'.
00071          88  TX-DO-NOT-ADJUST           VALUE 'N'.
00072          88  TX-FORM-CONTROL-LINE       VALUE 'K'.
00073          88  TX-NEW-PARAGRAPH           VALUE 'P'.
00074          88  TX-NO-SPECIAL-INSTRUCTION  VALUE ' '.
00075          88  TX-VALID-LINE-SQ-VALUE     VALUE 'A' 'C' 'P'
00076                                               'K' 'N' ' '
00077                                               'Z'.
00078
00079      12  TX-ARCHIVE-SW               PIC X.
00080          88  TX-ARCHIVE-THIS-LETTER     VALUE 'Y'.
00081          88  TX-DO-NOT-ARCHIVE          VALUE SPACES.
00082          88  TX-VALID-ARCHIVE-VALUE     VALUE 'Y' ' '.
00083
00084      12  TX-LAST-MAINTENANCED-BY     PIC X(4).
00085      12  TX-LAST-MAINTENANCED-DT     PIC X(2).
00086
00087      12  TX-BSR-CODE                 PIC X.
00088          88  TX-BSR-LETTER              VALUE 'B'.
00089          88  TX-NON-BSR-LETTER          VALUE ' '.
00090
00091      12  FILLER                      PIC X.
00092 *****************************************************************
00208      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE
                                TEXT-FILES.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL106' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00210
00211      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00212      MOVE '5'                   TO DC-OPTION-CODE.
00213      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00214      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00215      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00216
00217      MOVE DFHCOMMAREA           TO PROGRAM-INTERFACE-BLOCK.
00218      MOVE 2                     TO EMI-NUMBER-OF-LINES
00219                                    EMI-SWITCH2.
00220
00221      MOVE EIBTRMID              TO QID-TERM.
00222
00223      IF EIBCALEN = 0
00224          GO TO 8800-UNAUTHORIZED-ACCESS.
00225
00226      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00227          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM
00228        ELSE
00229          MOVE SPACES             TO RETURNED-FROM.
00230
00231      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00232          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00233              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00234              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00235              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00236              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00237              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00238              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00239              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00240              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
092308             PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
00241          ELSE
00242              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00243              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00244              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00245              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00246              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00247              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00248              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00249              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00250
00251      MOVE 'N' TO WS-DISPLAY-SW.
00252
00253      
      * EXEC CICS HANDLE CONDITION
00254 *        DUPREC  (8850-DUPREC)
00255 *        NOTOPEN (8870-NOTOPEN)
00256 *        NOTFND  (8880-NOT-FOUND)
00257 *        PGMIDERR(9600-PGMID-ERROR)
00258 *        ERROR   (9990-ABEND)
00259 *    END-EXEC.
      *    MOVE '"$%JIL.               ! " #00003326' TO DFHEIV0
           MOVE X'2224254A494C2E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033333236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00260
00261      IF EIBTRNID NOT = TRANS-ID
00262        IF RETURNED-FROM = XCTL-EL6565
00263            MOVE LOW-VALUES           TO EL106AO
00264            PERFORM 6500-RECOVER-TEMP-STORAGE THRU 6500-EXIT
00265            GO TO 8100-SEND-INITIAL-MAP
00266         ELSE
102717       IF RETURNED-FROM = XCTL-EL1061 or XCTL-EL1062
00268            MOVE LOW-VALUES           TO EL106AO
00269            IF PI-WS-STATE EQUAL LOW-VALUES OR SPACES
00270                GO TO 8100-SEND-INITIAL-MAP
00271            ELSE
00272                MOVE PI-WS-STATE TO STCDI
00273                MOVE 'Y' TO WS-DISPLAY-SW
00274                GO TO 1000-SHOW-STATE
00275         ELSE
00276        IF PI-RETURN-TO-PROGRAM = XCTL-EL6507
00277            MOVE LOW-VALUES           TO EL106AO
00278            IF PI-WS-STATE EQUAL LOW-VALUES OR SPACES
00279                GO TO 8100-SEND-INITIAL-MAP
00280            ELSE
00281                MOVE PI-WS-STATE TO STCDI
00282                MOVE 'Y' TO WS-DISPLAY-SW
00283                GO TO 1000-SHOW-STATE
00284         ELSE
00285          MOVE LOW-VALUES           TO EL106AO
00286          GO TO 8100-SEND-INITIAL-MAP.
00287
00288      IF EIBAID = DFHCLEAR
00289          GO TO 9400-CLEAR.
00290
00291      IF NOT DISPLAY-CAP
00292          MOVE 'READ'         TO SM-READ
00293          PERFORM 9995-SECURITY-VIOLATION
00294          MOVE ER-0070        TO EMI-ERROR
00295          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00296          GO TO 8100-SEND-INITIAL-MAP.
00297
00298      EJECT
00299  0200-RECEIVE.
00300      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00301          MOVE LOW-VALUES TO EL106AI
00302          MOVE ER-7008    TO EMI-ERROR
00303          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00304          MOVE -1 TO MAINTL
00305          GO TO 8200-SEND-DATAONLY.
00306
00307      
      * EXEC CICS RECEIVE
00308 *        MAP   (MAP-NAME)
00309 *        MAPSET(MAPSET-NAME)
00310 *        INTO  (EL106AI)
00311 *    END-EXEC.
           MOVE LENGTH OF
            EL106AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003380' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL106AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00312
00313      IF ENTERPFL = 0
00314          GO TO 0300-CHECK-PFKEYS.
00315
00316      IF EIBAID NOT = DFHENTER
00317          MOVE ER-0004 TO EMI-ERROR
00318          GO TO 0320-INPUT-ERROR.
00319
00320      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)
00321          MOVE PF-VALUES (ENTERPFI) TO EIBAID
00322      ELSE
00323          MOVE ER-0029 TO EMI-ERROR
00324          GO TO 0320-INPUT-ERROR.
00325
00326  0300-CHECK-PFKEYS.
00327      IF EIBAID = DFHPF23
00328          GO TO 8810-PF23.
00329
00330      IF EIBAID = DFHPF24
00331          GO TO 9200-RETURN-MAIN-MENU.
00332
00333      IF EIBAID = DFHPF12
00334          GO TO 9500-PF12.
00335
00336      IF MAINTL NOT = 0  AND
00337         EIBAID NOT = DFHENTER
00338           MOVE ER-0050 TO EMI-ERROR
00339           GO TO 0320-INPUT-ERROR.
00340
00341      IF EIBAID = DFHPF1
00342          GO TO 5000-FIND-NEXT-STATE.
00343
00344      IF EIBAID = DFHPF2
00345          GO TO 5500-FIND-PREV-STATE.
00346
00347      IF EIBAID = DFHPF3
00348          IF NOT MORTGAGE-SESSION
00349              IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
00350                  MOVE ER-0029        TO  EMI-ERROR
00351                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00352                  MOVE -1             TO  ENTERPFL
00353                  MOVE AL-UNBON       TO  ENTERPFA
00354                  GO TO 8200-SEND-DATAONLY
00355              ELSE
00356                  MOVE PI-PREV-STATE  TO  PI-WS-STATE
00357                  MOVE SPACES         TO  PI-WS-CLASS
00358                                          PI-WS-DEV
00359                                          PI-WS-TYPE
00360                                          PI-WS-PLAN
00361                  PERFORM 6400-CREATE-TEMP-STORAGE THRU 6400-EXIT
00362                  MOVE XCTL-EL6565    TO  PGM-NAME
00363                  GO TO 9300-XCTL
00364          ELSE
00365              MOVE ER-7536 TO EMI-ERROR
00366              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00367              MOVE AL-UNBON TO ENTERPFA
00368              IF ENTERPFL = 0
00369                  MOVE -1 TO MAINTL
00370                  GO TO 8200-SEND-DATAONLY
00371              ELSE
00372                  MOVE -1 TO ENTERPFL
00373                  GO TO 8200-SEND-DATAONLY.
00374
00375      IF EIBAID = DFHPF4
00376          IF NOT MORTGAGE-SESSION
00377              IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')
00378                  MOVE ER-0029        TO  EMI-ERROR
00379                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00380                  MOVE -1             TO  ENTERPFL
00381                  MOVE AL-UNBON       TO  ENTERPFA
00382                  GO TO 8200-SEND-DATAONLY
00383              ELSE
00384                  MOVE PI-PREV-STATE  TO  PI-WS-STATE
00385                  MOVE SPACES         TO  PI-WS-CLASS
00386                                          PI-WS-DEV
00387                                          PI-WS-TYPE
00388                                          PI-WS-PLAN
00389                  MOVE XCTL-EL1061    TO  PGM-NAME
00390                  GO TO 9300-XCTL
00391          ELSE
00392              MOVE ER-7536 TO EMI-ERROR
00393              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00394              MOVE AL-UNBON TO ENTERPFA
00395              IF ENTERPFL = 0
00396                  MOVE -1 TO MAINTL
00397                  GO TO 8200-SEND-DATAONLY
00398              ELSE
00399                  MOVE -1 TO ENTERPFL
00400                  GO TO 8200-SEND-DATAONLY.
102717     IF EIBAID = DFHPF5
102717        MOVE PI-PREV-STATE       TO PI-WS-STATE
102717        MOVE SPACES              TO PI-WS-CLASS
102717                                    PI-WS-DEV
102717                                    PI-WS-TYPE
102717                                    PI-WS-PLAN
102717        MOVE XCTL-EL1062         TO PGM-NAME
102717        GO TO 9300-XCTL
102717     END-IF
00402      IF EIBAID = DFHENTER
00403          GO TO 0330-EDIT-DATA.
00404
00405      MOVE ER-0029 TO EMI-ERROR.
00406  0320-INPUT-ERROR.
00407      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00408      MOVE AL-UNBON TO ENTERPFA.
00409
00410      IF ENTERPFL = 0
00411          MOVE -1 TO MAINTL
00412      ELSE
00413          MOVE -1 TO ENTERPFL.
00414
00415      GO TO 8200-SEND-DATAONLY.
00416
00417      EJECT
00418  0330-EDIT-DATA.
00419      IF STCDL = ZERO AND
00420         STABRL = ZERO
00421          MOVE ER-0144       TO EMI-ERROR
00422          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00423          MOVE -1            TO STCDL
00424          MOVE AL-UABON      TO STCDA
00425          GO TO 8200-SEND-DATAONLY.
00426
00427      IF STCDL = ZERO AND
00428         STABRL NOT = ZERO
00429          PERFORM 0500-GET-STATE-CD THRU 0600-EXIT.
00430
00431      IF MAINTI = 'S'
00432          GO TO 1000-SHOW-STATE.
00433
00434      IF MODIFY-CAP
00435         NEXT SENTENCE
00436        ELSE
00437         IF MAINTI = 'A' OR 'C' OR 'D'
00438          MOVE 'UPDATE'       TO SM-READ
00439          PERFORM 9995-SECURITY-VIOLATION
00440          MOVE ER-0070        TO EMI-ERROR
00441          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00442          MOVE LOW-VALUES     TO EL106AO
00443          GO TO 8100-SEND-INITIAL-MAP.
00444
00445      IF MAINTI = 'C'
00446          GO TO 2000-CHANGE-STATE.
00447
00448      IF MAINTI = 'A'
00449          GO TO 3000-ADD-STATE.
00450
00451      IF MAINTI = 'D'
00452          GO TO 4000-DELETE-STATE.
00453
00454      MOVE ER-0023 TO EMI-ERROR.
00455      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00456      MOVE -1       TO MAINTL.
00457      MOVE AL-UABON TO MAINTA.
00458      GO TO 8200-SEND-DATAONLY.
00459
00460      EJECT
00461  0500-GET-STATE-CD.
00462      MOVE PI-COMPANY-ID  TO CK-COMP-ID.
00463      MOVE LOW-VALUES     TO CK-STATE-CD.
00464      MOVE +0             TO CK-SEQ.
00465
00466      
      * EXEC CICS HANDLE CONDITION
00467 *        ENDFILE(8880-NOT-FOUND)
00468 *        NOTFND (8880-NOT-FOUND)
00469 *    END-EXEC.
      *    MOVE '"$''I                  ! # #00003547' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033353437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00470
00471      
      * EXEC CICS STARTBR
00472 *        DATASET  (ELCNTL-ID)
00473 *        RIDFLD   (ELCNTL-KEY)
00474 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003552' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00475
00476  0510-GET-NEXT-CD.
00477      
      * EXEC CICS READNEXT
00478 *        DATASET(ELCNTL-ID)
00479 *        SET    (ADDRESS OF CONTROL-FILE)
00480 *        RIDFLD (ELCNTL-KEY)
00481 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003558' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00482
00483      IF CF-COMPANY-ID NOT = PI-COMPANY-ID
00484          
      * EXEC CICS ENDBR
00485 *            DATASET  (ELCNTL-ID)
00486 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003565' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00487          GO TO 8880-NOT-FOUND.
00488
00489      IF CF-RECORD-TYPE NOT = '3'
00490          GO TO 0510-GET-NEXT-CD.
00491
00492      IF CF-STATE-ABBREVIATION = STABRI
00493          NEXT SENTENCE
00494        ELSE
00495          GO TO 0510-GET-NEXT-CD.
00496
00497      MOVE CF-STATE-CODE      TO STCDI.
00498      MOVE AL-UANON           TO STCDA.
00499      MOVE +2                 TO STCDL.
00500
00501      
      * EXEC CICS ENDBR
00502 *        DATASET  (ELCNTL-ID)
00503 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003582' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00504
00505  0600-EXIT.
00506       EXIT.
00507      EJECT
00508  1000-SHOW-STATE.
00509      IF RETURN-DISPLAY
00510          
      * EXEC CICS HANDLE CONDITION
00511 *            NOTFND  (8100-SEND-INITIAL-MAP)
00512 *        END-EXEC.
      *    MOVE '"$I                   ! $ #00003591' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033353931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00513
00514      MOVE PI-COMPANY-ID TO CK-COMP-ID.
00515      MOVE STCDI         TO CK-STATE-CD
00516                            PI-WS-STATE.
00517
00518      
      * EXEC CICS READ
00519 *        DATASET(ELCNTL-ID)
00520 *        SET    (ADDRESS OF CONTROL-FILE)
00521 *        RIDFLD (ELCNTL-KEY)
00522 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003599' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00523
00524      GO TO 7000-BUILD-OUTPUT-MAP.
00525
00526      EJECT
00527  2000-CHANGE-STATE.
00528      IF STCDI NOT = PI-PREV-STATE
00529          MOVE ER-0145 TO EMI-ERROR
00530          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00531          MOVE -1       TO STCDL
00532          MOVE AL-UABON TO STCDA
00533          GO TO 8200-SEND-DATAONLY.
00534
00535      PERFORM 6000-EDIT-INPUT-DATA THRU 6099-EXIT.
00536
00537      IF NOT EMI-NO-ERRORS
00538          GO TO 8200-SEND-DATAONLY.
00539
00540      MOVE PI-COMPANY-ID TO CK-COMP-ID.
00541      MOVE STCDI         TO CK-STATE-CD
00542                            PI-WS-STATE.
00543
00544      
      * EXEC CICS READ
00545 *        UPDATE
00546 *        DATASET(ELCNTL-ID)
00547 *        SET    (ADDRESS OF CONTROL-FILE)
00548 *        RIDFLD (ELCNTL-KEY)
00549 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00003625' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00550
00551      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY    OR
00552         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
00553          
      * EXEC CICS UNLOCK
00554 *            DATASET(ELCNTL-ID)
00555 *        END-EXEC
      *    MOVE '&*                    #   #00003634' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00556          MOVE ER-0068 TO EMI-ERROR
00557          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00558          GO TO 1000-SHOW-STATE.
102717     if cf-st-ga-comm-cap-sl not numeric
102717        move zeros               to cf-st-ga-comm-cap-sl
102717     end-if
102717     if cf-st-ga-comm-cap-jl not numeric
102717        move zeros               to cf-st-ga-comm-cap-jl
102717     end-if
102717     if cf-st-ga-comm-cap-sa not numeric
102717        move zeros               to cf-st-ga-comm-cap-sa
102717     end-if
102717     if cf-st-ga-comm-cap-ja not numeric
102717        move zeros               to cf-st-ga-comm-cap-ja
102717     end-if
102717
102717     if cf-st-tot-comm-cap-sl not numeric
102717        move zeros               to cf-st-tot-comm-cap-sl
102717     end-if
102717     if cf-st-tot-comm-cap-jl not numeric
102717        move zeros               to cf-st-tot-comm-cap-jl
102717     end-if
102717     if cf-st-tot-comm-cap-sa not numeric
102717        move zeros               to cf-st-tot-comm-cap-sa
102717     end-if
102717     if cf-st-tot-comm-cap-ja not numeric
102717        move zeros               to cf-st-tot-comm-cap-ja
102717     end-if
102717     if (cf-commission-cap-required = 'Y')
                  or
              (ccreqi = 'Y')
              display ' found YYY'
102717        if (cf-st-comm-cap-sl = zeros or spaces
                      or low-values)
102717           and (cf-st-comm-cap-jl = zeros or spaces
                      or low-values)
102717           and (cf-st-comm-cap-sa = zeros or spaces
                      or low-values)
102717           and (cf-st-comm-cap-ja = zeros or spaces
                      or low-values)
102717           and (cf-st-ga-comm-cap-sl = zeros or spaces
                      or low-values)
102717           and (cf-st-ga-comm-cap-jl = zeros or spaces
                      or low-values)
102717           and (cf-st-ga-comm-cap-sa = zeros or spaces
                      or low-values)
102717           and (cf-st-ga-comm-cap-ja = zeros or spaces
                      or low-values)
102717           and (cf-st-tot-comm-cap-sl = zeros or spaces
                      or low-values)
102717           and (cf-st-tot-comm-cap-jl = zeros or spaces
                      or low-values)
102717           and (cf-st-tot-comm-cap-sa = zeros or spaces
                      or low-values)
102717           and (cf-st-tot-comm-cap-ja = zeros or spaces
                      or low-values)
                      display ' about to unlock '
102717           
      * EXEC CICS UNLOCK
102717*             DATASET(ELCNTL-ID)
102717*          END-EXEC
      *    MOVE '&*                    #   #00003694' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
102717           MOVE ER-1964          TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                 THRU 9900-EXIT
102717           GO TO 1000-SHOW-STATE
102717        end-if
102717     end-if
00560      MOVE 'B'                    TO JP-RECORD-TYPE.
00561      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00562      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
00563
00564      MOVE PI-PROCESSOR-ID TO CF-LAST-MAINT-BY.
00565      MOVE EIBTIME         TO CF-LAST-MAINT-HHMMSS.
00566      MOVE EIBDATE         TO DC-JULIAN-YYDDD.
00567      MOVE '5'             TO DC-OPTION-CODE.
00568      MOVE LINK-ELDATCV    TO PGM-NAME.
00569
00570      
      * EXEC CICS LINK
00571 *        PROGRAM (PGM-NAME)
00572 *        COMMAREA(DATE-CONVERSION-DATA)
00573 *        LENGTH  (DC-COMM-LENGTH)
00574 *    END-EXEC.
      *    MOVE '."C                   (   #00003713' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00575
00576      IF DATE-CONVERSION-ERROR
00577          MOVE LOW-VALUES          TO CF-LAST-MAINT-DT
00578      ELSE
00579          MOVE DC-BIN-DATE-1       TO CF-LAST-MAINT-DT.
00580
00581      IF STABRL NOT = ZEROS
00582          MOVE STABRI              TO CF-STATE-ABBREVIATION.
00583
00584      IF STNAMEL NOT = ZEROS
00585          MOVE STNAMEI             TO CF-STATE-NAME.
00586
00587      IF LFEXPL NOT = ZEROS
00588          MOVE WS-ST-LF-EXP-PCT    TO CF-ST-LF-EXP-PCT.
00589
00590      IF AHEXPL NOT = ZEROS
00591          MOVE WS-ST-AH-EXP-PCT    TO CF-ST-AH-EXP-PCT.
00592
00593      IF QUOTCALL NOT = ZEROS
00594          MOVE QUOTCALI            TO CF-ST-TOL-CLAIM.
00595
00596      IF PREMTOLL NOT = ZEROS
00597          MOVE PREMTOLI            TO CF-ST-TOL-PREM.
00598
00599      IF REFTOLL NOT = ZEROS
00600          MOVE REFTOLI             TO CF-ST-TOL-REFUND.
00601
00602      IF OVSAMTL > 0
00603          MOVE OVSAMTI TO CF-ST-OVR-SHT-AMT
00604      END-IF.
00605
00606      IF PRMPCTL NOT = ZEROS
00607          MOVE WS-ST-TOL-PREM-PCT  TO CF-ST-TOL-PREM-PCT.
00608
00609      IF REFPCTL NOT = ZEROS
00610          MOVE  WS-ST-TOL-REF-PCT  TO CF-ST-TOL-REF-PCT.
00611
00612      IF OVSPCTL > +0
00613          MOVE WS-ST-OVR-SHT-PCT TO CF-ST-OVR-SHT-PCT
00614      END-IF.
00615
00616      IF CLREJECL NOT = ZEROS
00617          MOVE CLREJECI            TO CF-ST-CLAIM-REJECT-SW.
00618
00619      IF ISSREJL NOT = ZEROS
00620          MOVE ISSREJI             TO CF-ST-PREM-REJECT-SW.
040915     IF AGTSIGL NOT = ZEROS
040915        MOVE AGTSIGI             TO CF-ST-AGENT-SIG-EDIT
040915     END-IF
070115     IF NETONLYL NOT = ZEROS
070115        MOVE NETONLYI            TO CF-ST-NET-ONLY-STATE
070115     END-IF
102717     if ccreql <> zeros
102717        move ccreqi              to cf-commission-cap-required
102717     end-if
00622      IF REFREJL NOT = ZEROS
00623          MOVE REFREJI             TO CF-ST-REF-REJECT-SW.
00624
00625      IF REFMINL NOT = ZEROS
00626          MOVE REFMINI             TO CF-ST-REFUND-MIN.
00627
00628      IF REFDAY1L NOT = ZEROS
00629          MOVE REFDAY1I            TO CF-ST-REFUND-DAYS-FIRST.
00630
00631      IF REFDAYSL NOT = ZEROS
00632          MOVE REFDAYSI            TO CF-ST-REFUND-DAYS-SUBSEQ.
00633
00634      IF SPLPMTL NOT = ZEROS
00635          MOVE SPLPMTI             TO CF-ST-SPLIT-PAYMENT.
00636
00637      IF EXTDAYSL NOT = ZEROS
00638          MOVE EXTDAYSI            TO CF-ST-FST-PMT-DAYS-MAX.
00639
PEMMOD*    IF INTDAYSL NOT = ZEROS
PEMMOD*        MOVE INTDAYSI            TO CF-ST-NO-DAYS-ELAPSED.
00642
00643      IF EXTCHGL NOT = ZEROS
00644          MOVE EXTCHGI             TO CF-ST-FST-PMT-DAYS-CHG.
00645
CIDMOD     IF REMTERML NOT = ZEROS
CIDMOD        MOVE REMTERMI             TO CF-ST-RT-CALC
CIDMOD     END-IF
CIDMOD
PEMMOD     IF LFREDL NOT = ZEROS
PEMMOD        MOVE LFREDI               TO CF-ST-RF-LR-CALC
PEMMOD     END-IF
PEMMOD
PEMMOD     IF LFLEVL NOT = ZEROS
PEMMOD        MOVE LFLEVI               TO CF-ST-RF-LL-CALC
PEMMOD     END-IF
PEMMOD
PEMMOD     IF LFNETL NOT = ZEROS
PEMMOD        MOVE LFNETI               TO CF-ST-RF-LN-CALC
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHAHL NOT = ZEROS
PEMMOD        MOVE AHAHI                TO CF-ST-RF-AH-CALC
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHCPL NOT = ZEROS
PEMMOD        MOVE AHCPI                TO CF-ST-RF-CP-CALC
PEMMOD     END-IF
PEMMOD
PEMMOD     IF LFTAXL > +0
PEMMOD        MOVE WS-ST-LF-PREM-TAX   TO CF-ST-LF-PREM-TAX
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHITAXL > +0
PEMMOD        MOVE WS-ST-AH-PREM-TAX-I TO CF-ST-AH-PREM-TAX-I
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHGTAXL > +0
PEMMOD        MOVE WS-ST-AH-PREM-TAX-G TO CF-ST-AH-PREM-TAX-G
PEMMOD     END-IF
PEMMOD
022415     IF (XINTL NOT = ZEROS)
022415        and (xinti numeric)
022415        MOVE XINTO               TO CF-ST-EXTRA-INTEREST-PERIODS
022415     END-IF
022415     IF (XPMTSL NOT = ZEROS)
022415        and (xpmtsi numeric)
022415        MOVE XPMTSO              TO CF-ST-EXTRA-PAYMENTS
022415     END-IF
011410     IF REFCLML NOT = ZEROS
011410         MOVE REFCLMI             TO CF-ST-REF-AH-DEATH-IND
011410     END-IF.
011410
061511     IF VFYBENEL NOT = ZEROS
061511         MOVE VFYBENEI            TO CF-ST-VFY-2ND-BENE
061511     END-IF.
061511
012913     IF CAUSALL NOT = ZEROS
012913         MOVE CAUSALI             TO CF-ST-CAUSAL-STATE
012913     END-IF.
012913
00646      IF STUEL NOT = ZEROS
00647          MOVE STUEI               TO CF-ST-CALL-UNEARNED.
00648
00649      IF STCNTLL NOT = ZEROS
00650          MOVE STCNTLI            TO CF-ST-CALL-RPT-CNTL.
00651
00652      IF STDEVL NOT = ZEROS
00653          MOVE STDEVI             TO CF-ST-CALL-RATE-DEV.
00654
00655      MOVE REPLAWI                TO CF-REPLACEMENT-LAW-SW.
00656      MOVE REPLETRI               TO CF-REPLACEMENT-LETTER.
00657
00660 *    MOVE STATII                 TO CF-ST-STAT-DATE-FROM.
00685
00686      IF WS-IRATE NUMERIC AND
00687         WS-IRATE NOT = ZEROS
00688          MOVE WS-IRATE           TO CF-ST-STAT-INTEREST
00689       ELSE
00690          MOVE ZEROS              TO CF-ST-STAT-INTEREST.
00691
00692      IF WS-IRATE1 NUMERIC AND
00693         WS-IRATE1 NOT = ZEROS
00694          MOVE WS-IRATE1          TO CF-ST-STAT-INTEREST-1
00695       ELSE
00696          MOVE ZEROS              TO CF-ST-STAT-INTEREST-1.
00697
00698      IF WS-IRATE2 NUMERIC AND
00699         WS-IRATE2 NOT = ZEROS
00700          MOVE WS-IRATE2          TO CF-ST-STAT-INTEREST-2
00701       ELSE
00702          MOVE ZEROS              TO CF-ST-STAT-INTEREST-2.
00703
00704      IF WS-IRATE3 NUMERIC AND
00705         WS-IRATE3 NOT = ZEROS
00706          MOVE WS-IRATE3          TO CF-ST-STAT-INTEREST-3
00707       ELSE
00708          MOVE ZEROS              TO CF-ST-STAT-INTEREST-3.
00709
00710      IF WS-RESIDENT-TAX NUMERIC AND
00711         WS-RESIDENT-TAX NOT = ZEROS
00712          MOVE WS-RESIDENT-TAX    TO CF-ST-RES-TAX-PCT
00713       ELSE
00714          MOVE ZEROS              TO CF-ST-RES-TAX-PCT.
00715
00716      IF WS-FREE-LOOK-DAYS NUMERIC
00717              AND
00718         WS-FREE-LOOK-DAYS NOT EQUAL ZEROS
00719          MOVE WS-FREE-LOOK-DAYS  TO CF-ST-FREE-LOOK-PERIOD
00720       ELSE
00721          MOVE ZEROS              TO CF-ST-FREE-LOOK-PERIOD.
00722
00723      IF  WS-ST-TARGET-LOSS-RATIO NUMERIC
00724              AND
00725          WS-ST-TARGET-LOSS-RATIO NOT EQUAL ZEROS
00726          MOVE WS-ST-TARGET-LOSS-RATIO
00727                                  TO CF-ST-TARGET-LOSS-RATIO
00728      ELSE
00729          MOVE ZEROS              TO CF-ST-TARGET-LOSS-RATIO.
00730
00731      IF  WS-ST-CALC-INTEREST NUMERIC
00732              AND
00733          WS-ST-CALC-INTEREST NOT EQUAL ZEROS
00734          MOVE WS-ST-CALC-INTEREST
00735                                  TO CF-ST-CALC-INTEREST
00736      ELSE
00737          MOVE ZEROS              TO CF-ST-CALC-INTEREST.
00738
00739      MOVE 'C'                    TO JP-RECORD-TYPE.
00740      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00741      
      * EXEC CICS REWRITE
00742 *        DATASET(ELCNTL-ID)
00743 *        FROM   (CONTROL-FILE)
00744 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003922' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00745
00746      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
00747      MOVE ER-0000 TO EMI-ERROR.
00748      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00749      MOVE LOW-VALUES  TO EL106AO.
00750      MOVE -1          TO MAINTL.
00751      MOVE SPACES      TO PI-PREV-STATE.
00752      MOVE CK-STATE-CD TO STCDO.
00753      MOVE AL-UANON    TO STCDA.
00754      GO TO 1000-SHOW-STATE.
00755
00756      EJECT
00757  3000-ADD-STATE.
00758      PERFORM 6000-EDIT-INPUT-DATA THRU 6099-EXIT.
00759
00760      IF NOT EMI-NO-ERRORS
00761          GO TO 8200-SEND-DATAONLY.
00762
00763      
      * EXEC CICS GETMAIN
00764 *        SET    (ADDRESS OF CONTROL-FILE)
00765 *        LENGTH (750)
00766 *        INITIMG(GETMAIN-SPACE)
00767 *    END-EXEC.
           MOVE 750
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00003944' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00768
00769      MOVE 'CF'          TO CF-RECORD-ID.
00770      MOVE PI-COMPANY-ID TO CF-COMPANY-ID.
00771      MOVE '3'           TO CF-RECORD-TYPE.
00772      MOVE STCDI         TO CF-ACCESS-OF-STATE.
00773      MOVE +0            TO CF-SEQUENCE-NO.
00774
00775      MOVE EIBDATE       TO DC-JULIAN-YYDDD.
00776      MOVE '5'           TO DC-OPTION-CODE.
00777      MOVE LINK-ELDATCV  TO PGM-NAME.
00778      
      * EXEC CICS LINK
00779 *        PROGRAM (PGM-NAME)
00780 *        COMMAREA(DATE-CONVERSION-DATA)
00781 *        LENGTH  (DC-COMM-LENGTH)
00782 *    END-EXEC.
      *    MOVE '."C                   (   #00003959' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00783
00784      IF DATE-CONVERSION-ERROR
00785          MOVE LOW-VALUES    TO CF-LAST-MAINT-DT
00786        ELSE
00787          MOVE DC-BIN-DATE-1 TO CF-LAST-MAINT-DT.
00788
00789      MOVE PI-PROCESSOR-ID   TO CF-LAST-MAINT-BY.
00790      MOVE EIBTIME           TO CF-LAST-MAINT-HHMMSS.
00791
00792      MOVE STNAMEI           TO CF-STATE-NAME.
00793      MOVE STABRI            TO CF-STATE-ABBREVIATION.
00794
00795      IF LFEXPL = ZEROS
00796          MOVE ZEROS               TO CF-ST-LF-EXP-PCT
00797        ELSE
00798          MOVE WS-ST-LF-EXP-PCT    TO CF-ST-LF-EXP-PCT.
00799
00800      IF AHEXPL = ZEROS
00801          MOVE ZEROS               TO CF-ST-AH-EXP-PCT
00802        ELSE
00803          MOVE WS-ST-AH-EXP-PCT    TO CF-ST-AH-EXP-PCT.
00804
00805      IF QUOTCALL = ZEROS
00806          MOVE ZEROS               TO CF-ST-TOL-CLAIM
00807        ELSE
00808          MOVE QUOTCALI            TO CF-ST-TOL-CLAIM.
00809
00810      IF PREMTOLL = ZEROS
00811          MOVE ZEROS               TO CF-ST-TOL-PREM
00812        ELSE
00813          MOVE PREMTOLI            TO CF-ST-TOL-PREM.
00814
00815      IF OVSAMTL = ZEROS
00816          MOVE ZEROS              TO CF-ST-OVR-SHT-AMT
00817        ELSE
00818          MOVE OVSAMTI            TO CF-ST-OVR-SHT-AMT.
00819
00820      IF REFTOLL = ZEROS
00821          MOVE ZEROS              TO CF-ST-TOL-REFUND
00822        ELSE
00823          MOVE REFTOLI            TO CF-ST-TOL-REFUND.
00824
00825      IF PRMPCTL = ZEROS
00826          MOVE ZEROS              TO CF-ST-TOL-PREM-PCT
00827        ELSE
00828          MOVE WS-ST-TOL-PREM-PCT TO CF-ST-TOL-PREM-PCT.
00829
00830      IF OVSPCTL = ZEROS
00831          MOVE ZEROS              TO CF-ST-OVR-SHT-PCT
00832        ELSE
00833          MOVE WS-ST-OVR-SHT-PCT  TO CF-ST-OVR-SHT-PCT.
00834
00835      IF REFPCTL = ZEROS
00836          MOVE ZEROS              TO CF-ST-TOL-REF-PCT
00837        ELSE
00838          MOVE WS-ST-TOL-REF-PCT  TO CF-ST-TOL-REF-PCT.
00839
00840      IF CLREJECL = ZEROS
00841          MOVE SPACE              TO CF-ST-CLAIM-REJECT-SW
00842        ELSE
00843          MOVE CLREJECI           TO CF-ST-CLAIM-REJECT-SW.
00844
00845      IF ISSREJL = ZEROS
00846          MOVE SPACE              TO CF-ST-PREM-REJECT-SW
00847        ELSE
00848          MOVE ISSREJI            TO CF-ST-PREM-REJECT-SW.
040915     IF AGTSIGL = ZEROS
040915        MOVE SPACES              TO CF-ST-AGENT-SIG-EDIT
040915     ELSE
040915        MOVE AGTSIGI             TO CF-ST-AGENT-SIG-EDIT
040915     END-IF
070115     IF NETONLYL = ZEROS
070115        MOVE SPACES              TO CF-ST-NET-ONLY-STATE
070115     ELSE
070115        MOVE NETONLYI            TO CF-ST-NET-ONLY-STATE
070115     END-IF
102717     if ccreql = zeros
102717        MOVE SPACES              TO CF-COMMISSION-CAP-REQUIRED
102717     else
102717        move ccreqi              to cf-commission-cap-required
102717     end-if
00850      IF REFREJL = ZEROS
00851          MOVE SPACE              TO CF-ST-REF-REJECT-SW
00852        ELSE
00853          MOVE REFREJI            TO CF-ST-REF-REJECT-SW.
00854
00855      IF REFMINL = ZEROS
00856          MOVE ZEROS              TO CF-ST-REFUND-MIN
00857        ELSE
00858          MOVE REFMINI            TO CF-ST-REFUND-MIN.
00859
00860      IF REFDAY1L  = ZERO
00861          MOVE ZEROS              TO CF-ST-REFUND-DAYS-FIRST
00862        ELSE
00863          MOVE REFDAY1I           TO CF-ST-REFUND-DAYS-FIRST.
00864
00865      IF REFDAYSL = ZERO
00866          MOVE ZEROS              TO CF-ST-REFUND-DAYS-SUBSEQ
00867        ELSE
00868          MOVE REFDAYSI           TO CF-ST-REFUND-DAYS-SUBSEQ.
00869
00870      IF SPLPMTL = ZERO
00871          MOVE 'N'                TO CF-ST-SPLIT-PAYMENT
00872      ELSE
00873          MOVE SPLPMTI            TO CF-ST-SPLIT-PAYMENT.
00874
00875      IF EXTDAYSL = ZERO
00876          MOVE ZEROS              TO CF-ST-FST-PMT-DAYS-MAX
00877        ELSE
00878          MOVE EXTDAYSI           TO CF-ST-FST-PMT-DAYS-MAX.
00879
PEMMOD*    IF INTDAYSL = ZERO
PEMMOD*        MOVE ZEROS              TO CF-ST-NO-DAYS-ELAPSED
PEMMOD*      ELSE
PEMMOD*        MOVE INTDAYSI           TO CF-ST-NO-DAYS-ELAPSED.
00884
00885      IF EXTCHGL = ZEROS
00886          MOVE SPACES             TO CF-ST-FST-PMT-DAYS-CHG
00887        ELSE
00888          MOVE EXTCHGI            TO CF-ST-FST-PMT-DAYS-CHG.
00889
CIDMOD     IF REMTERML = ZEROS
CIDMOD        MOVE SPACES              TO CF-ST-RT-CALC
CIDMOD     ELSE
CIDMOD        MOVE REMTERMI            TO CF-ST-RT-CALC
CIDMOD     END-IF
CIDMOD
PEMMOD     IF LFTAXL = ZEROS
PEMMOD        MOVE ZEROS               TO CF-ST-LF-PREM-TAX
PEMMOD     ELSE
PEMMOD        MOVE WS-ST-LF-PREM-TAX   TO CF-ST-LF-PREM-TAX
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHITAXL = ZEROS
PEMMOD        MOVE ZEROS               TO CF-ST-AH-PREM-TAX-I
PEMMOD     ELSE
PEMMOD        MOVE WS-ST-AH-PREM-TAX-I TO CF-ST-AH-PREM-TAX-I
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHGTAXL = ZEROS
PEMMOD        MOVE ZEROS               TO CF-ST-AH-PREM-TAX-G
PEMMOD     ELSE
PEMMOD        MOVE WS-ST-AH-PREM-TAX-G TO CF-ST-AH-PREM-TAX-G
PEMMOD     END-IF
PEMMOD
00890      IF STUEL = ZEROS
00891          MOVE SPACES             TO CF-ST-CALL-UNEARNED
00892        ELSE
00893          MOVE STUEI              TO CF-ST-CALL-UNEARNED.
00894
00895      IF STCNTLL = ZEROS
00896          MOVE SPACES             TO CF-ST-CALL-RPT-CNTL
00897        ELSE
00898          MOVE STCNTLI            TO CF-ST-CALL-RPT-CNTL.
00899
00900      IF STDEVL = ZEROS
00901          MOVE SPACES             TO CF-ST-CALL-RATE-DEV
00902        ELSE
00903          MOVE STDEVI             TO CF-ST-CALL-RATE-DEV.
00904
00905      IF  WS-ST-TARGET-LOSS-RATIO NUMERIC
00906              AND
00907          WS-ST-TARGET-LOSS-RATIO NOT EQUAL ZEROS
00908          MOVE WS-ST-TARGET-LOSS-RATIO
00909                                  TO CF-ST-TARGET-LOSS-RATIO
00910      ELSE
00911          MOVE ZEROS              TO CF-ST-TARGET-LOSS-RATIO.
00912
00913      IF  WS-ST-CALC-INTEREST NUMERIC
00914              AND
00915          WS-ST-CALC-INTEREST NOT EQUAL ZEROS
00916          MOVE WS-ST-CALC-INTEREST
00917                                  TO CF-ST-CALC-INTEREST
00918      ELSE
00919          MOVE ZEROS              TO CF-ST-CALC-INTEREST.
00920
00921      MOVE REPLAWI                TO CF-REPLACEMENT-LAW-SW.
00922      MOVE REPLETRI               TO CF-REPLACEMENT-LETTER.
00923
00926 *    MOVE STATII                 TO CF-ST-STAT-DATE-FROM.
00927
00948      IF RESTAXL = ZEROS
00949          MOVE ZEROS              TO CF-ST-RES-TAX-PCT
00950        ELSE
00951          MOVE WS-RESIDENT-TAX    TO CF-ST-RES-TAX-PCT.
00952
00953      IF FREELKL = ZEROS
00954          MOVE ZEROS              TO CF-ST-FREE-LOOK-PERIOD
00955        ELSE
00956          MOVE WS-FREE-LOOK-DAYS  TO CF-ST-FREE-LOOK-PERIOD.
00957
00958 *    IF STATRL = ZEROS
00959          MOVE ZEROS              TO CF-ST-STAT-INTEREST
00960 *      ELSE
00961 *        MOVE WS-IRATE           TO CF-ST-STAT-INTEREST.
00962
00963 *    IF STATR1L = ZEROS
00964          MOVE ZEROS              TO CF-ST-STAT-INTEREST-1
00965 *      ELSE
00966 *        MOVE WS-IRATE1          TO CF-ST-STAT-INTEREST-1.
00967
00968 *    IF STATR2L = ZEROS
00969          MOVE ZEROS              TO CF-ST-STAT-INTEREST-2
00970 *      ELSE
00971 *        MOVE WS-IRATE2          TO CF-ST-STAT-INTEREST-2.
00972
00973 *    IF STATR3L = ZEROS
00974          MOVE ZEROS              TO CF-ST-STAT-INTEREST-3
00975 *      ELSE
00976 *        MOVE WS-IRATE3          TO CF-ST-STAT-INTEREST-3.
00977
00978      PERFORM 6100-INITIALIZE-BENEFIT-CNTL THRU 6199-EXIT
00979              VARYING SUB FROM +1 BY +1
00980              UNTIL SUB GREATER THAN +50.
00981
00982      MOVE 'A'                    TO JP-RECORD-TYPE.
00983      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00984
00985      
      * EXEC CICS WRITE
00986 *        FROM   (CONTROL-FILE)
00987 *        DATASET(ELCNTL-ID)
00988 *        RIDFLD (CF-CONTROL-PRIMARY)
00989 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004182' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00990
00991      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
00992      MOVE ER-0000 TO EMI-ERROR.
00993      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00994      MOVE LOW-VALUES  TO EL106AO.
00995      MOVE -1          TO MAINTL.
00996      MOVE SPACES      TO PI-PREV-STATE.
00997      MOVE CK-STATE-CD TO STCDO.
00998      MOVE AL-UANON    TO STCDA.
00999      GO TO 8100-SEND-INITIAL-MAP.
01000
01001      EJECT
01002  4000-DELETE-STATE.
01003      IF STCDI NOT = PI-PREV-STATE
01004          MOVE ER-0145  TO EMI-ERROR
01005          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01006          MOVE -1       TO STCDL
01007          MOVE AL-UABON TO STCDA
01008          GO TO 8200-SEND-DATAONLY.
01009
01010      MOVE PI-COMPANY-ID TO CK-COMP-ID.
01011      MOVE STCDI         TO CK-STATE-CD.
01012      
      * EXEC CICS READ
01013 *        UPDATE
01014 *        DATASET(ELCNTL-ID)
01015 *        SET    (ADDRESS OF CONTROL-FILE)
01016 *        RIDFLD (ELCNTL-KEY)
01017 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004209' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01018
01019      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY    OR
01020         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
01021          
      * EXEC CICS UNLOCK
01022 *            DATASET(ELCNTL-ID)
01023 *        END-EXEC
      *    MOVE '&*                    #   #00004218' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01024          MOVE ER-0068 TO EMI-ERROR
01025          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01026          GO TO 1000-SHOW-STATE.
01027
01028      MOVE 'D'                    TO JP-RECORD-TYPE.
01029      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
01030      
      * EXEC CICS DELETE
01031 *        DATASET(ELCNTL-ID)
01032 *    END-EXEC.
      *    MOVE '&(                    &   #00004227' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01033
01034      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT
01035      MOVE ER-0000 TO EMI-ERROR.
01036      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01037      MOVE LOW-VALUES  TO EL106AO.
01038      MOVE -1          TO MAINTL.
01039      MOVE SPACES      TO PI-PREV-STATE
01040      MOVE CK-STATE-CD TO STCDO.
01041      MOVE AL-UANON    TO STCDA.
01042      GO TO 8100-SEND-INITIAL-MAP.
01043
01044      EJECT
01045  5000-FIND-NEXT-STATE.
01046      MOVE PI-COMPANY-ID  TO CK-COMP-ID.
01047
01048      IF STCDL = 0
01049          MOVE LOW-VALUES TO CK-STATE-CD
01050          MOVE +0         TO CK-SEQ
01051      ELSE
01052          MOVE STCDI      TO CK-STATE-CD
01053          MOVE +1         TO CK-SEQ.
01054
01055      MOVE SPACES TO PI-PREV-STATE.
01056
01057      
      * EXEC CICS HANDLE CONDITION
01058 *        NOTFND (8860-ENDFILE)
01059 *    END-EXEC.
      *    MOVE '"$I                   ! % #00004254' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034323534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01060
01061      
      * EXEC CICS READ
01062 *        DATASET(ELCNTL-ID)
01063 *        SET    (ADDRESS OF CONTROL-FILE)
01064 *        RIDFLD (ELCNTL-KEY)
01065 *        GTEQ
01066 *    END-EXEC.
      *    MOVE '&"S        G          (   #00004258' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01067
01068      IF CF-COMPANY-ID  NOT = PI-COMPANY-ID  OR
01069         CF-RECORD-TYPE NOT = '3'
01070          GO TO 8860-ENDFILE.
01071
01072      IF STCDL = 0
01073          MOVE ER-0146 TO EMI-ERROR
01074          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01075
01076      GO TO 7000-BUILD-OUTPUT-MAP.
01077
01078      EJECT
01079  5500-FIND-PREV-STATE.
01080      MOVE PI-COMPANY-ID          TO  CK-COMP-ID.
01081      MOVE PI-PREV-STATE          TO  CK-STATE-CD.
01082
01083      IF STCDL GREATER +0
01084          MOVE STCDI              TO  CK-STATE-CD.
01085
01086      MOVE SPACES                 TO  PI-PREV-STATE.
01087
01088      
      * EXEC CICS HANDLE CONDITION
01089 *        NOTFND(8860-ENDFILE)
01090 *    END-EXEC.
      *    MOVE '"$I                   ! & #00004285' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034323835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01091
01092      
      * EXEC CICS STARTBR
01093 *        DATASET  (ELCNTL-ID)
01094 *        RIDFLD   (ELCNTL-KEY)
01095 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004289' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01096
01097  5600-READ-PREV-STATE-RECORD.
01098      
      * EXEC CICS READPREV
01099 *        DATASET  (ELCNTL-ID)
01100 *        SET      (ADDRESS OF CONTROL-FILE)
01101 *        RIDFLD   (ELCNTL-KEY)
01102 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004295' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01103
01104      IF FIRST-TIME
01105          MOVE 'N'                TO  WS-FIRST-TIME-SW
01106          GO TO 5600-READ-PREV-STATE-RECORD.
01107
01108      IF CF-COMPANY-ID  NOT = PI-COMPANY-ID  OR
01109         CF-RECORD-TYPE NOT = '3'
01110          GO TO 8860-ENDFILE.
01111
01112      IF STCDL = 0
01113          MOVE ER-0146 TO EMI-ERROR
01114          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01115
01116      GO TO 7000-BUILD-OUTPUT-MAP.
01117
01118      EJECT
01119  6000-EDIT-INPUT-DATA.
01120      IF STABRL = ZEROS AND MAINTI = 'A'
01121         MOVE -1  TO STABRL
01122         MOVE ER-0152 TO EMI-ERROR
01123         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01124
01125      IF STNAMEL = ZEROS AND MAINTI = 'A'
01126         MOVE -1   TO STNAMEL
01127         MOVE ER-0153 TO EMI-ERROR
01128         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01129
01130      IF LFEXPL NOT = ZEROS
01131         
      * EXEC CICS BIF
01132 *            DEEDIT
01133 *            FIELD (LFEXPI)
01134 *            LENGTH(7)
01135 *       END-EXEC
           MOVE 7
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004328' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LFEXPI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01136       IF LFEXPI NUMERIC
01137             MOVE AL-UNNON        TO LFEXPA
01138             MOVE LFEXPI          TO WS-ST-LF-EXP-PCT
01139         ELSE
01140             MOVE ER-7531         TO EMI-ERROR
01141             MOVE -1              TO LFEXPL
01142             MOVE AL-UNBON        TO LFEXPA
01143             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01144
01145      IF AHEXPL NOT = ZEROS
01146         
      * EXEC CICS BIF
01147 *            DEEDIT
01148 *            FIELD (AHEXPI)
01149 *            LENGTH(7)
01150 *       END-EXEC
           MOVE 7
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004343' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AHEXPI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01151       IF AHEXPI NUMERIC
01152             MOVE AL-UNNON        TO AHEXPA
01153             MOVE AHEXPI          TO WS-ST-AH-EXP-PCT
01154         ELSE
01155             MOVE ER-7531         TO EMI-ERROR
01156             MOVE -1              TO AHEXPL
01157             MOVE AL-UNBON        TO AHEXPA
01158             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01159
01160      IF QUOTCALL NOT = ZEROS
01161         
      * EXEC CICS BIF
01162 *            DEEDIT
01163 *            FIELD (QUOTCALI)
01164 *            LENGTH(6)
01165 *       END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004358' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QUOTCALI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01166         IF QUOTCALI NOT NUMERIC
01167            MOVE -1               TO QUOTCALL
01168            MOVE AL-UNBON         TO QUOTCALA
01169            MOVE ER-2009          TO EMI-ERROR
01170            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01171         ELSE
01172         IF QUOTCALI GREATER THAN 9999
01173            MOVE -1               TO QUOTCALL
01174            MOVE AL-UNBON         TO QUOTCALA
01175            MOVE ER-2009          TO EMI-ERROR
01176            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01177
01178      IF OVSAMTL NOT = ZEROS
01179         
      * EXEC CICS BIF
01180 *            DEEDIT
01181 *            FIELD (OVSAMTI)
01182 *            LENGTH(6)
01183 *       END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004376' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 OVSAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01184         IF OVSAMTI  NOT NUMERIC
01185            MOVE -1               TO OVSAMTL
01186            MOVE AL-UNBON         TO OVSAMTA
01187            MOVE ER-2010          TO EMI-ERROR
01188            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01189         ELSE
01190         IF OVSAMTI  GREATER THAN 9999
01191            MOVE -1               TO OVSAMTL
01192            MOVE AL-UNBON         TO OVSAMTA
01193            MOVE ER-2010          TO EMI-ERROR
01194            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01195
01196      IF PREMTOLL NOT = ZEROS
01197         
      * EXEC CICS BIF
01198 *            DEEDIT
01199 *            FIELD (PREMTOLI)
01200 *            LENGTH(6)
01201 *       END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004394' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PREMTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01202         IF PREMTOLI NOT NUMERIC
01203            MOVE -1               TO PREMTOLL
01204            MOVE AL-UNBON         TO PREMTOLA
01205            MOVE ER-2010          TO EMI-ERROR
01206            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01207         ELSE
01208         IF PREMTOLI GREATER THAN 9999
01209            MOVE -1               TO PREMTOLL
01210            MOVE AL-UNBON         TO PREMTOLA
01211            MOVE ER-2010          TO EMI-ERROR
01212            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01213
01214      IF REFTOLL NOT = ZEROS
01215         
      * EXEC CICS BIF
01216 *            DEEDIT
01217 *            FIELD(REFTOLI)
01218 *            LENGTH(6)
01219 *       END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004412' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REFTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01220         IF REFTOLI NOT NUMERIC
01221            MOVE -1               TO REFTOLL
01222            MOVE AL-UNBON         TO REFTOLA
01223            MOVE ER-2014          TO EMI-ERROR
01224            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01225         ELSE
01226         IF REFTOLI GREATER THAN 9999
01227            MOVE -1               TO REFTOLL
01228            MOVE AL-UNBON         TO REFTOLA
01229            MOVE ER-2014          TO EMI-ERROR
01230            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01231
01232      IF CLREJECL NOT = ZEROS
01233         IF CLREJECI = SPACES OR '1'
01234            NEXT SENTENCE
01235           ELSE
01236            MOVE -1               TO CLREJECL
01237            MOVE AL-UABON         TO CLREJECA
01238            MOVE ER-2024          TO EMI-ERROR
01239            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01240
01241      IF ISSREJL NOT = ZEROS
01242         IF ISSREJI = SPACES OR '1'
01243            NEXT SENTENCE
01244           ELSE
01245            MOVE -1               TO ISSREJL
01246            MOVE AL-UABON         TO ISSREJA
01247            MOVE ER-2012          TO EMI-ERROR
01248            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
040915     IF AGTSIGL NOT = ZEROS
040915        IF AGTSIGI = ' ' OR 'Y' OR 'N'
040915           CONTINUE
040915        ELSE
040915           MOVE -1               TO AGTSIGL
040915           MOVE AL-UABON         TO AGTSIGA
040915           MOVE ER-9999          TO EMI-ERROR
040915           PERFORM 9900-ERROR-FORMAT
040915                                 THRU 9900-EXIT
040915        END-IF
040915     END-IF
070115     IF NETONLYL NOT = ZEROS
070115        IF NETONLYI = ' ' OR 'Y' OR 'N'
070115           CONTINUE
070115        ELSE
070115           MOVE -1               TO NETONLYL
070115           MOVE AL-UABON         TO NETONLYA
070115           MOVE ER-9999          TO EMI-ERROR
070115           PERFORM 9900-ERROR-FORMAT
070115                                 THRU 9900-EXIT
070115        END-IF
070115     END-IF
102717     IF CCREQL NOT = ZEROS
102717        IF CCREQI = ' ' OR 'Y' OR 'N'
102717           CONTINUE
102717        ELSE
102717           MOVE -1               TO CCREQL
102717           MOVE AL-UABON         TO CCREQA
102717           MOVE ER-3064          TO EMI-ERROR
102717           PERFORM 9900-ERROR-FORMAT
102717                                 THRU 9900-EXIT
102717        END-IF
102717     END-IF
01250      IF REFREJL NOT = ZEROS
01251         IF REFREJI = SPACES OR '1'
01252            NEXT SENTENCE
01253           ELSE
01254            MOVE -1               TO REFREJL
01255            MOVE AL-UABON         TO REFREJA
01256            MOVE ER-2028          TO EMI-ERROR
01257            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01258
01259      IF REFMINL NOT = ZEROS
01260         
      * EXEC CICS BIF
01261 *            DEEDIT
01262 *            FIELD (REFMINI)
01263 *            LENGTH(6)
01264 *       END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004489' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REFMINI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01265         IF REFMINI NOT NUMERIC
01266            MOVE -1               TO REFMINL
01267            MOVE AL-UNBON         TO REFMINA
01268            MOVE ER-3030          TO EMI-ERROR
01269            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01270
01271      IF REFDAY1L NOT = ZERO
01272         IF REFDAY1I LESS '00' OR GREATER '31'
01273             MOVE -1               TO REFDAY1L
01274             MOVE AL-UNBON         TO REFDAY1A
01275             MOVE ER-3031          TO EMI-ERROR
01276             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01277
01278      IF REFDAYSL NOT = ZERO
01279         IF REFDAYSI LESS '00' OR GREATER '31'
01280             MOVE -1               TO REFDAYSL
01281             MOVE AL-UNBON         TO REFDAYSA
01282             MOVE ER-3031          TO EMI-ERROR
01283             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01284
01285      IF SPLPMTL NOT = ZERO
01286         IF (SPLPMTI IS EQUAL TO 'Y' OR 'N' OR ' ')
01287             NEXT SENTENCE
01288         ELSE
01289             MOVE -1               TO SPLPMTL
01290             MOVE AL-UABON         TO SPLPMTA
01291             MOVE ER-0805          TO EMI-ERROR
01292             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01293
01294      IF EXTDAYSL NOT = ZERO
01295         IF EXTDAYSI NOT NUMERIC
01296             MOVE -1               TO EXTDAYSL
01297             MOVE AL-UNBON         TO EXTDAYSA
01298             MOVE ER-3032          TO EMI-ERROR
01299             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01300
PEMMOD*    IF INTDAYSL NOT = ZERO
PEMMOD*       IF INTDAYSI NOT NUMERIC
PEMMOD*           MOVE -1               TO INTDAYSL
PEMMOD*           MOVE AL-UNBON         TO INTDAYSA
PEMMOD*           MOVE ER-0141          TO EMI-ERROR
PEMMOD*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01307
01308      IF EXTCHGL NOT = ZEROS
01309         IF EXTCHGI = SPACES OR '1' OR '2' OR '3'
01310            NEXT SENTENCE
01311           ELSE
01312            MOVE -1               TO EXTCHGL
01313            MOVE AL-UABON         TO EXTCHGA
01314            MOVE ER-3033          TO EMI-ERROR
01315            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01316
CIDMOD     IF REMTERML NOT = ZEROS
CIDMOD        IF (REMTERMI = ' ') OR
CIDMOD           (REMTERMI > '0' AND < '8')
CIDMOD           CONTINUE
CIDMOD        ELSE
CIDMOD           MOVE -1               TO REMTERML
CIDMOD           MOVE AL-UABON         TO REMTERMA
CIDMOD           MOVE ER-2298          TO EMI-ERROR
CIDMOD           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
PEMMOD     IF LFREDL NOT = ZEROS
PEMMOD        IF LFREDI = ' ' OR '1' OR '2' OR '3' OR '4' OR '5'
PEMMOD                 OR '6' OR '8' OR '9'
PEMMOD           CONTINUE
PEMMOD        ELSE
PEMMOD           MOVE -1               TO LFREDL
PEMMOD           MOVE AL-UABON         TO LFREDA
PEMMOD           MOVE ER-0582          TO EMI-ERROR
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF LFLEVL NOT = ZEROS
PEMMOD        IF LFLEVI = ' ' OR '1' OR '2' OR '3' OR '4' OR '5'
PEMMOD                 OR '6' OR '8' OR '9'
PEMMOD           CONTINUE
PEMMOD        ELSE
PEMMOD           MOVE -1               TO LFLEVL
PEMMOD           MOVE AL-UABON         TO LFLEVA
PEMMOD           MOVE ER-0582          TO EMI-ERROR
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF LFNETL NOT = ZEROS
PEMMOD        IF LFNETI = ' ' OR '1' OR '2' OR '3' OR '4' OR '5'
PEMMOD                 OR '6' OR '8' OR '9'
PEMMOD           CONTINUE
PEMMOD        ELSE
PEMMOD           MOVE -1               TO LFNETL
PEMMOD           MOVE AL-UABON         TO LFNETA
PEMMOD           MOVE ER-0582          TO EMI-ERROR
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHAHL  NOT = ZEROS
PEMMOD        IF AHAHI  = ' ' OR '1' OR '2' OR '3' OR '4' OR '5'
PEMMOD                 OR '6' OR '8' OR '9'
PEMMOD           CONTINUE
PEMMOD        ELSE
PEMMOD           MOVE -1               TO AHAHL
PEMMOD           MOVE AL-UABON         TO AHAHA
PEMMOD           MOVE ER-0582          TO EMI-ERROR
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHCPL  NOT = ZEROS
PEMMOD        IF AHCPI  = ' ' OR '1' OR '2' OR '3' OR '4' OR '5'
PEMMOD                 OR '6' OR '8' OR '9'
PEMMOD           CONTINUE
PEMMOD        ELSE
PEMMOD           MOVE -1               TO AHCPL
PEMMOD           MOVE AL-UABON         TO AHCPA
PEMMOD           MOVE ER-0582          TO EMI-ERROR
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
01317      IF STUEL NOT = ZEROS
01318         IF STUEI = ' ' OR '1' OR '2' OR '3' OR '4' OR '5'
01319            NEXT SENTENCE
01320           ELSE
01321            MOVE -1               TO STUEL
01322            MOVE AL-UABON         TO STUEA
01323            MOVE ER-3034          TO EMI-ERROR
01324            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01325
01326      IF STCNTLL NOT = ZEROS
01327         IF STCNTLI = SPACES
01328            NEXT SENTENCE
01329         ELSE
01330            IF STCNTLI = '1' OR '2' OR '3' OR '4' OR
01331                         '5' OR '6' OR '7' OR '8' OR
01332                         '9' OR 'A' OR 'B' OR 'X'
01333               NEXT SENTENCE
01334            ELSE
01335               MOVE -1               TO STCNTLL
01336               MOVE AL-UABON         TO STCNTLA
01337               MOVE ER-3035          TO EMI-ERROR
01338               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01339
01340      IF  REPLAWL GREATER THAN ZEROS
01341          IF  REPLAWI = 'Y'
01342              MOVE REPLAWI        TO REPLAWO
01343              MOVE AL-UANON       TO REPLAWA
01344          ELSE
01345              IF  REPLAWI = 'N' OR SPACES OR LOW-VALUES
01346                  MOVE 'N'        TO REPLAWO
01347                  MOVE AL-UANON   TO REPLAWA
01348              ELSE
01349                  MOVE ER-9074    TO EMI-ERROR
01350                  MOVE -1         TO REPLAWL
01351                  MOVE AL-UABON   TO REPLAWA
01352                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01353
01354      IF  REPLETRL GREATER THAN +0
01355          IF  REPLAWI = 'Y'
01356              IF  REPLETRI GREATER THAN SPACES
01357                  MOVE SPACES     TO W-WORKING-TEXT-KEY
01358                  MOVE PI-COMPANY-CD
01359                                  TO W-TEXT-COMPANY-CD
01360                  MOVE REPLETRI   TO W-TEXT-FORM-NO
01361                  MOVE +1         TO W-TEXT-LINE-SEQ
01362                  
      * EXEC CICS HANDLE CONDITION
01363 *                    NOTFND   (6010-LETR-NOT-FOUND)
01364 *                    ENDFILE  (6010-LETR-NOT-FOUND)
01365 *                    NOTOPEN  (8890-LETR-NOT-OPEN)
01366 *                END-EXEC
      *    MOVE '"$I''J                 ! '' #00004668' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034363638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01367                  
      * EXEC CICS READ
01368 *                    SET     (ADDRESS OF TEXT-FILES)
01369 *                    DATASET (ELLETR-ID)
01370 *                    RIDFLD  (W-WORKING-TEXT-KEY)
01371 *                END-EXEC
      *    MOVE '&"S        E          (   #00004673' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELLETR-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-WORKING-TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01372                  MOVE AL-UANON   TO REPLETRA
01373              ELSE
01374                  MOVE ER-9448    TO EMI-ERROR
01375                  MOVE -1         TO REPLETRL
01376                  MOVE AL-UABON   TO REPLETRA
01377                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01378          ELSE
01379              IF  REPLETRI EQUAL SPACES OR LOW-VALUES
01380                  MOVE LOW-VALUES TO REPLETRO
01381                  MOVE AL-UANON   TO REPLETRA
01382              ELSE
01383                  MOVE ER-9478    TO EMI-ERROR
01384                  MOVE -1         TO REPLAWL
01385                  MOVE AL-UABON   TO REPLAWA
01386                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01387      ELSE
01388          IF  REPLAWI = 'Y'
01389              MOVE ER-9448        TO EMI-ERROR
01390              MOVE -1             TO REPLETRL
01391              MOVE AL-UABON       TO REPLETRA
01392              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01407
01408 *    IF STATIL GREATER ZEROS
01409 *        IF STATII = 'I' OR 'R'
01410 *            MOVE STATII         TO STATIO
01411 *            MOVE AL-UANON       TO STATIA
01412 *         ELSE
01413 *            IF STATII = SPACES OR LOW-VALUES
01414 *                MOVE ' '        TO STATIO
01415 *                MOVE AL-UANON   TO STATIA
01416 *        ELSE
01417 *            MOVE ER-7346    TO EMI-ERROR
01418 *            MOVE -1         TO STATIL
01419 *            MOVE AL-UABON   TO STATIA
01420 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01486      IF RESTAXL GREATER ZEROS
01487         
      * EXEC CICS BIF
01488 *            DEEDIT
01489 *            FIELD (RESTAXI)
01490 *            LENGTH(6)
01491 *       END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004714' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RESTAXI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01492       IF RESTAXI NUMERIC
01493             MOVE AL-UNNON        TO RESTAXA
01494             MOVE RESTAXI         TO WS-RESIDENT-TAX
01495                                     RESTAXO
01496         ELSE
01497             MOVE ER-1614         TO EMI-ERROR
01498             MOVE -1              TO RESTAXL
01499             MOVE AL-UNBON        TO RESTAXA
01500             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01501
01502      IF FREELKI  NUMERIC
01503             MOVE AL-UNNON        TO FREELKA
01504             MOVE FREELKI         TO WS-FREE-LOOK-DAYS
01505                                     FREELKO
01506         ELSE
01507             MOVE -1               TO FREELKL
01508             MOVE AL-UNBON         TO FREELKA
01509             MOVE ER-8159          TO EMI-ERROR
01510             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01511
01512 *    IF STATRL GREATER ZEROS
01513 *       EXEC CICS BIF
01514 *            DEEDIT
01515 *            FIELD (STATRI)
01516 *            LENGTH(6)
01517 *       END-EXEC
01518 *     IF STATRI NUMERIC
01519 *           MOVE AL-UNNON        TO STATRA
01520 *           MOVE STATRI          TO WS-IRATE
01521 *                                   STATRO
01522 *       ELSE
01523 *           MOVE ER-1614         TO EMI-ERROR
01524 *           MOVE -1              TO STATRL
01525 *           MOVE AL-UNBON        TO STATRA
01526 *           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01527
01528 *    IF STATR1L GREATER ZEROS
01529 *       EXEC CICS BIF
01530 *            DEEDIT
01531 *            FIELD (STATR1I)
01532 *            LENGTH(6)
01533 *       END-EXEC
01534 *     IF STATR1I NUMERIC
01535 *           MOVE AL-UNNON        TO STATR1A
01536 *           MOVE STATR1I         TO WS-IRATE1
01537 *                                   STATR1O
01538 *       ELSE
01539 *           MOVE ER-1614         TO EMI-ERROR
01540 *           MOVE -1              TO STATR1L
01541 *           MOVE AL-UNBON        TO STATR1A
01542 *           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01543 *
01544 *    IF STATR2L GREATER ZEROS
01545 *       EXEC CICS BIF
01546 *            DEEDIT
01547 *            FIELD (STATR2I)
01548 *            LENGTH(6)
01549 *       END-EXEC
01550 *     IF STATR2I NUMERIC
01551 *           MOVE AL-UNNON        TO STATR2A
01552 *           MOVE STATR2I         TO WS-IRATE2
01553 *                                   STATR2O
01554 *       ELSE
01555 *           MOVE ER-1614         TO EMI-ERROR
01556 *           MOVE -1              TO STATR2L
01557 *           MOVE AL-UNBON        TO STATR2A
01558 *           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01559 *
01560 *    IF STATR3L GREATER ZEROS
01561 *       EXEC CICS BIF
01562 *            DEEDIT
01563 *            FIELD (STATR3I)
01564 *            LENGTH(6)
01565 *       END-EXEC
01566 *     IF STATR3I NUMERIC
01567 *           MOVE AL-UNNON        TO STATR3A
01568 *           MOVE STATR3I         TO WS-IRATE3
01569 *                                   STATR3O
01570 *       ELSE
01571 *           MOVE ER-1614         TO EMI-ERROR
01572 *           MOVE -1              TO STATR3L
01573 *           MOVE AL-UNBON        TO STATR3A
01574 *           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01575
01576      GO TO 6020-CONTINUE.
01577
01578  6010-LETR-NOT-FOUND.
01579      MOVE ER-9447                TO EMI-ERROR.
01580      MOVE -1                     TO REPLETRL.
01581      MOVE AL-UABON               TO REPLETRA.
01582      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01583
01584  6020-CONTINUE.
01585
01586      IF  TARRATL NOT = ZEROS
01587          
      * EXEC CICS BIF
01588 *             DEEDIT
01589 *             FIELD (TARRATI)
01590 *             LENGTH (6)
01591 *        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004814' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TARRATI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01592
01593          IF  TARRATI NUMERIC
01594              MOVE AL-UNNON       TO TARRATA
01595              MOVE TARRATI        TO WS-ST-TARGET-LOSS-RATIO
01596                                     TARRATO
01597          ELSE
01598              MOVE ER-7717        TO EMI-ERROR
01599              MOVE -1             TO TARRATL
01600              MOVE AL-UNBON       TO TARRATA
01601              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01602
01603      IF  CALCINTL NOT = ZEROS
01604          
      * EXEC CICS BIF
01605 *             DEEDIT
01606 *             FIELD (CALCINTI)
01607 *             LENGTH (6)
01608 *        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004831' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALCINTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01609
01610          IF  CALCINTI NUMERIC
01611              MOVE AL-UNNON       TO CALCINTA
01612              MOVE CALCINTI       TO WS-ST-CALC-INTEREST
01613                                     CALCINTO
01614          ELSE
01615              MOVE ER-7735        TO EMI-ERROR
01616              MOVE -1             TO CALCINTL
01617              MOVE AL-UNBON       TO CALCINTA
01618              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01619
01620      IF PRMPCTL NOT = ZEROS
01621         
      * EXEC CICS BIF
01622 *            DEEDIT
01623 *            FIELD (PRMPCTI)
01624 *            LENGTH(5)
01625 *       END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004848' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PRMPCTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01626       IF PRMPCTI NUMERIC
01627             MOVE AL-UNNON        TO PRMPCTA
01628             MOVE PRMPCTI         TO WS-ST-TOL-PREM-PCT
01629         ELSE
01630             MOVE ER-7532         TO EMI-ERROR
01631             MOVE -1              TO PRMPCTL
01632             MOVE AL-UNBON        TO PRMPCTA
01633             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01634
01635      IF OVSPCTL NOT = ZEROS
01636         
      * EXEC CICS BIF
01637 *            DEEDIT
01638 *            FIELD (OVSPCTI)
01639 *            LENGTH(5)
01640 *       END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004863' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 OVSPCTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01641       IF OVSPCTI NUMERIC
01642             MOVE AL-UNNON        TO OVSPCTA
01643             MOVE OVSPCTI         TO WS-ST-OVR-SHT-PCT
01644         ELSE
01645             MOVE ER-7532         TO EMI-ERROR
01646             MOVE -1              TO OVSPCTL
01647             MOVE AL-UNBON        TO OVSPCTA
01648             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01649
01650      IF REFPCTL NOT = ZEROS
01651         
      * EXEC CICS BIF
01652 *            DEEDIT
01653 *            FIELD (REFPCTI)
01654 *            LENGTH(5)
01655 *       END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004878' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REFPCTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01656       IF REFPCTI NUMERIC
01657             MOVE AL-UNNON        TO REFPCTA
01658             MOVE REFPCTI         TO WS-ST-TOL-REF-PCT
01659         ELSE
01660             MOVE ER-7532         TO EMI-ERROR
01661             MOVE -1              TO REFPCTL
01662             MOVE AL-UNBON        TO REFPCTA
01663             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01664
PEMMOD     IF LFTAXL NOT = ZEROS
PEMMOD        
      * EXEC CICS BIF
PEMMOD*            DEEDIT
PEMMOD*            FIELD (LFTAXI)
PEMMOD*            LENGTH(5)
PEMMOD*       END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004893' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LFTAXI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
PEMMOD        IF LFTAXI NUMERIC
PEMMOD           MOVE AL-UNNON         TO LFTAXA
PEMMOD           MOVE LFTAXI           TO WS-ST-LF-PREM-TAX
PEMMOD        ELSE
PEMMOD           MOVE ER-2082          TO EMI-ERROR
PEMMOD           MOVE -1               TO LFTAXL
PEMMOD           MOVE AL-UNBON         TO LFTAXA
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHITAXL NOT = ZEROS
PEMMOD        
      * EXEC CICS BIF
PEMMOD*            DEEDIT
PEMMOD*            FIELD (AHITAXI)
PEMMOD*            LENGTH(5)
PEMMOD*       END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004911' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AHITAXI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
PEMMOD        IF AHITAXI NUMERIC
PEMMOD           MOVE AL-UNNON         TO AHITAXA
PEMMOD           MOVE AHITAXI          TO WS-ST-AH-PREM-TAX-I
PEMMOD        ELSE
PEMMOD           MOVE ER-2084          TO EMI-ERROR
PEMMOD           MOVE -1               TO AHITAXL
PEMMOD           MOVE AL-UNBON         TO AHITAXA
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHGTAXL NOT = ZEROS
PEMMOD        
      * EXEC CICS BIF
PEMMOD*            DEEDIT
PEMMOD*            FIELD (AHGTAXI)
PEMMOD*            LENGTH(5)
PEMMOD*       END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004929' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AHGTAXI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
PEMMOD        IF AHGTAXI NUMERIC
PEMMOD           MOVE AL-UNNON         TO AHGTAXA
PEMMOD           MOVE AHGTAXI          TO WS-ST-AH-PREM-TAX-G
PEMMOD        ELSE
PEMMOD           MOVE ER-2084          TO EMI-ERROR
PEMMOD           MOVE -1               TO AHGTAXL
PEMMOD           MOVE AL-UNBON         TO AHGTAXA
PEMMOD           PERFORM 9900-ERROR-FORMAT
PEMMOD                                 THRU 9900-EXIT
PEMMOD        END-IF
PEMMOD     END-IF
011410
011410     IF REFCLML NOT = ZEROS
011211        IF REFCLMI = SPACES OR '1' OR '2' OR '3' OR '4' OR '5'
011410           NEXT SENTENCE
011410        ELSE
011410           MOVE -1               TO REFCLML
011410           MOVE AL-UABON         TO REFCLMA
011410           MOVE ER-3036          TO EMI-ERROR
011410           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
011410        END-IF
011410     END-IF.
011410
061511     IF VFYBENEL NOT = ZEROS
032514        IF VFYBENEI = SPACES OR 'A' OR 'L' OR 'B'
061511           NEXT SENTENCE
061511        ELSE
061511           MOVE -1               TO VFYBENEL
061511           MOVE AL-UABON         TO VFYBENEA
032514           MOVE ER-7581          TO EMI-ERROR
061511           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061511        END-IF
061511     END-IF.
061511
012913     IF CAUSALL NOT = ZEROS
012913        IF CAUSALI = SPACES OR 'A' OR 'L' OR 'B'
012913           NEXT SENTENCE
012913        ELSE
012913           MOVE -1               TO CAUSALL
012913           MOVE AL-UABON         TO CAUSALA
012913           MOVE ER-7578          TO EMI-ERROR
012913           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
012913        END-IF
012913     END-IF.
012913
PEMMOD     .
01665  6099-EXIT.
01666      EXIT.
01667
01668      EJECT
01669  6100-INITIALIZE-BENEFIT-CNTL.
01670
01671      MOVE SPACES   TO   CF-ST-BENEFIT-CD         (SUB)
01672                         CF-ST-BENEFIT-KIND       (SUB)
01673                         CF-ST-REM-TERM-CALC      (SUB)
01674                         CF-ST-REFUND-CALC        (SUB)
01675                         CF-ST-EARNING-CALC       (SUB)
01676                         CF-ST-OVRD-EARNINGS-CALC (SUB).
01677
01678  6199-EXIT.
01679      EXIT.
01680
01681      EJECT
01682  6400-CREATE-TEMP-STORAGE.
01683      PERFORM 6600-DELETE-TEMP-STORAGE THRU 6600-EXIT.
01684
01685      
      * EXEC CICS WRITEQ TS
01686 *        QUEUE   (QID)
01687 *        FROM    (EL106AO)
01688 *        LENGTH  (WS-MAP-LENGTH)
01689 *    END-EXEC.
      *    MOVE '*"     L              ''   #00005000' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL106AO, 
                 WS-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01690
01691  6400-EXIT.
01692       EXIT.
01693
01694  6500-RECOVER-TEMP-STORAGE.
01695      
      * EXEC CICS READQ TS
01696 *        QUEUE   (QID)
01697 *        INTO    (EL106AO)
01698 *        LENGTH  (WS-MAP-LENGTH)
01699 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00005010' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL106AO, 
                 WS-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01700
01701      PERFORM 6600-DELETE-TEMP-STORAGE THRU 6600-EXIT.
01702
01703  6500-EXIT.
01704       EXIT.
01705
01706  6600-DELETE-TEMP-STORAGE.
01707      
      * EXEC CICS HANDLE CONDITION
01708 *        QIDERR  (6600-EXIT)
01709 *    END-EXEC.
      *    MOVE '"$N                   ! ( #00005022' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303035303232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01710
01711      
      * EXEC CICS DELETEQ TS
01712 *        QUEUE  (QID)
01713 *    END-EXEC.
      *    MOVE '*&                    #   #00005026' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01714
01715  6600-EXIT.
01716       EXIT.
01717
01718      EJECT
01719 ***************************************************************
01720 *                                                             *
01721 *     BUILD THE OUTPUT SCREEN TO BE DISPLAYED                 *
01722 *                                                             *
01723 ***************************************************************
01724  7000-BUILD-OUTPUT-MAP.
01725      MOVE LOW-VALUES            TO EL106AO.
01726      MOVE CF-STATE-CODE         TO STCDO
01727                                    PI-WS-STATE.
01728      MOVE CF-ST-TOL-CLAIM       TO QUOTCALO.
01729      MOVE CF-ST-TOL-PREM        TO PREMTOLO.
01730      MOVE CF-ST-TOL-REFUND      TO REFTOLO.
01731      MOVE CF-ST-CLAIM-REJECT-SW TO CLREJECO.
01732      MOVE CF-ST-PREM-REJECT-SW  TO ISSREJO.
040915     if cf-st-agent-sig-edit = 'Y'
040915        move 'Y'                to agtsigo
040915     else
040915        move 'N'                to agtsigo
040915     end-if
070115     if cf-st-net-only-state = 'Y'
070115        move 'Y'                to netonlyo
070115     else
070115        move 'N'                to netonlyo
070115     end-if
102717     if cf-commission-cap-required = 'Y'
102717        move 'Y'                to ccreqo
102717     else
102717        move 'N'                to ccreqo
102717     end-if
01733      MOVE CF-ST-REF-REJECT-SW   TO REFREJO.
01734      MOVE CF-ST-FST-PMT-DAYS-CHG
01735                                 TO EXTCHGO.
CIDMOD     MOVE CF-ST-RT-CALC         TO REMTERMO
PEMMOD     MOVE CF-ST-RF-LR-CALC      TO LFREDO
PEMMOD     MOVE CF-ST-RF-LL-CALC      TO LFLEVO
PEMMOD     MOVE CF-ST-RF-LN-CALC      TO LFNETO
PEMMOD     MOVE CF-ST-RF-AH-CALC      TO AHAHO
PEMMOD     MOVE CF-ST-RF-CP-CALC      TO AHCPO
01736      MOVE CF-ST-CALL-UNEARNED   TO STUEO.
01737      MOVE CF-ST-CALL-RPT-CNTL   TO STCNTLO.
01738      MOVE CF-ST-CALL-RATE-DEV   TO STDEVO.
01739      MOVE CF-STATE-ABBREVIATION TO STABRO.
01740      MOVE CF-STATE-NAME         TO STNAMEO.
011410     MOVE CF-ST-REF-AH-DEATH-IND TO REFCLMO.
061511     MOVE CF-ST-VFY-2ND-BENE    TO VFYBENEO.
012913     MOVE CF-ST-CAUSAL-STATE    TO CAUSALO.
100108     IF  CF-ST-CHECK-COUNTER NUMERIC
100108        MOVE CF-ST-CHECK-COUNTER TO STCHKNOO
100108     ELSE
100108        MOVE ZEROS             TO STCHKNOO
100108     END-IF.
022415     IF CF-ST-EXTRA-INTEREST-PERIODS NOT NUMERIC
022415        MOVE ZEROS               TO CF-ST-EXTRA-INTEREST-PERIODS
022415     END-IF
022415     IF CF-ST-EXTRA-PAYMENTS NOT NUMERIC
022415        MOVE ZEROS               TO CF-ST-EXTRA-PAYMENTS
022415     END-IF
           MOVE CF-ST-EXTRA-INTEREST-PERIODS
                                       TO XINTO
           MOVE CF-ST-EXTRA-PAYMENTS   TO XPMTSO
01742      IF CF-ST-LF-EXP-PCT      NUMERIC
01743          IF CF-ST-LF-EXP-PCT NOT = ZEROS
01744              MOVE CF-ST-LF-EXP-PCT  TO LFEXPO.
01745
01746      IF CF-ST-AH-EXP-PCT      NUMERIC
01747          IF CF-ST-AH-EXP-PCT NOT = ZEROS
01748              MOVE CF-ST-AH-EXP-PCT  TO AHEXPO.
01749
01750      IF CF-ST-TOL-PREM-PCT    NUMERIC
01751          IF CF-ST-TOL-PREM-PCT NOT = ZEROS
01752              MOVE CF-ST-TOL-PREM-PCT TO PRMPCTO.
01753
01754      IF CF-ST-OVR-SHT-AMT NUMERIC
01755         IF CF-ST-OVR-SHT-AMT > ZEROS
01756            MOVE CF-ST-OVR-SHT-AMT TO OVSAMTO
01757         END-IF
01758      END-IF.
01759
01760      IF CF-ST-OVR-SHT-PCT NUMERIC
01761          IF CF-ST-OVR-SHT-PCT > ZEROS
01762              MOVE CF-ST-OVR-SHT-PCT TO OVSPCTO
01763          END-IF
01764      END-IF.
01765
01766      IF CF-ST-TOL-REF-PCT     NUMERIC
01767          IF CF-ST-TOL-REF-PCT NOT = ZEROS
01768              MOVE CF-ST-TOL-REF-PCT TO REFPCTO.
01769
01770      IF CF-ST-REFUND-MIN      NUMERIC
01771          IF CF-ST-REFUND-MIN NOT = ZEROS
01772              MOVE CF-ST-REFUND-MIN  TO REFMINO.
01773
01774      IF CF-ST-REFUND-DAYS-FIRST NUMERIC
01775        IF CF-ST-REFUND-DAYS-FIRST NOT = ZEROS
01776          MOVE CF-ST-REFUND-DAYS-FIRST
01777                                 TO REFDAY1O.
01778
01779      IF CF-ST-REFUND-DAYS-SUBSEQ NUMERIC
01780        IF CF-ST-REFUND-DAYS-SUBSEQ NOT = ZEROS
01781          MOVE CF-ST-REFUND-DAYS-SUBSEQ
01782                                 TO REFDAYSO.
01783
01784      IF CF-ST-SPLIT-PAYMENT IS EQUAL TO ' '
01785          MOVE 'N'               TO SPLPMTO
01786      ELSE
01787          MOVE CF-ST-SPLIT-PAYMENT
01788                                 TO SPLPMTO.
01789
01790      IF CF-ST-FST-PMT-DAYS-MAX NUMERIC
01791        IF CF-ST-FST-PMT-DAYS-MAX NOT = ZEROS
01792          MOVE CF-ST-FST-PMT-DAYS-MAX
01793                                 TO EXTDAYSO.
01794
PEMMOD     IF CF-ST-LF-PREM-TAX NUMERIC
PEMMOD        IF CF-ST-LF-PREM-TAX > ZEROS
PEMMOD           MOVE CF-ST-LF-PREM-TAX
PEMMOD                                 TO LFTAXO
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF CF-ST-AH-PREM-TAX-I NUMERIC
PEMMOD        IF CF-ST-AH-PREM-TAX-I > ZEROS
PEMMOD           MOVE CF-ST-AH-PREM-TAX-I
PEMMOD                                 TO AHITAXO
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF CF-ST-AH-PREM-TAX-G NUMERIC
PEMMOD        IF CF-ST-AH-PREM-TAX-G > ZEROS
PEMMOD           MOVE CF-ST-AH-PREM-TAX-G
PEMMOD                                 TO AHGTAXO
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD*    IF PI-COMPANY-ID = 'NCL'
PEMMOD*    IF CF-ST-NO-DAYS-ELAPSED NUMERIC
PEMMOD*      IF CF-ST-NO-DAYS-ELAPSED NOT = ZEROS
PEMMOD*        MOVE CF-ST-NO-DAYS-ELAPSED
PEMMOD*                               TO INTDAYSO.
01800
01801      IF  CF-REPLACEMENT-LAW-SW NOT EQUAL 'Y'
01802              AND
01803          CF-REPLACEMENT-LAW-SW NOT EQUAL 'N'
01804          MOVE 'N'               TO REPLAWO
01805          MOVE LOW-VALUES        TO REPLETRO
01806      ELSE
01807          MOVE CF-REPLACEMENT-LAW-SW
01808                                 TO REPLAWO
01809
01810          IF  CF-REPLACEMENT-LETTER GREATER THAN SPACES
01811              MOVE CF-REPLACEMENT-LETTER
01812                                 TO REPLETRO
01813          ELSE
01814              MOVE LOW-VALUES    TO REPLETRO.
01815
01816      IF  CF-ST-TARGET-LOSS-RATIO NUMERIC
01817          IF  CF-ST-TARGET-LOSS-RATIO NOT = ZEROS
01818              MOVE CF-ST-TARGET-LOSS-RATIO
01819                                 TO TARRATO
01820          ELSE
01821              MOVE ZEROS         TO TARRATO
01822      ELSE
01823          MOVE ZEROS             TO TARRATO.
01824
01825      IF  CF-ST-CALC-INTEREST NUMERIC
01826          IF  CF-ST-CALC-INTEREST NOT = ZEROS
01827              MOVE CF-ST-CALC-INTEREST
01828                                 TO CALCINTO
01829          ELSE
01830              MOVE ZEROS         TO CALCINTO
01831      ELSE
01832          MOVE ZEROS             TO CALCINTO.
01869
01870      IF  CF-ST-RES-TAX-PCT NUMERIC
01871          IF  CF-ST-RES-TAX-PCT NOT = ZEROS
01872              MOVE CF-ST-RES-TAX-PCT
01873                                 TO RESTAXO
01874          ELSE
01875              MOVE ZEROS         TO RESTAXO
01876      ELSE
01877          MOVE ZEROS             TO RESTAXO.
01878
01879      IF CF-ST-FREE-LOOK-PERIOD NUMERIC
01880         IF CF-ST-FREE-LOOK-PERIOD NOT = ZEROS
01881              MOVE CF-ST-FREE-LOOK-PERIOD
01882                                 TO FREELKO
01883         ELSE
01884              MOVE ZEROS         TO FREELKO
01885      ELSE
01886          MOVE ZEROS             TO FREELKO.
01887
01888 *    IF PI-COMPANY-ID = 'NCL'
01889 *    IF  CF-ST-STAT-INTEREST NUMERIC
01890 *        IF  CF-ST-STAT-INTEREST NOT = ZEROS
01891 *            MOVE CF-ST-STAT-INTEREST
01892 *                               TO STATRO
01893 *        ELSE
01894 *            MOVE ZEROS         TO STATRO
01895 *    ELSE
01896 *        MOVE ZEROS             TO STATRO.
01897
01898 *    IF PI-COMPANY-ID = 'NCL'
01899 *    IF  CF-ST-STAT-INTEREST-1 NUMERIC
01900 *        IF  CF-ST-STAT-INTEREST-1 NOT = ZEROS
01901 *            MOVE CF-ST-STAT-INTEREST-1
01902 *                               TO STATR1O
01903 *        ELSE
01904 *            MOVE ZEROS         TO STATR1O
01905 *    ELSE
01906 *        MOVE ZEROS             TO STATR1O.
01907
01908 *    IF PI-COMPANY-ID = 'NCL'
01909 *    IF  CF-ST-STAT-INTEREST-2 NUMERIC
01910 *        IF  CF-ST-STAT-INTEREST-2 NOT = ZEROS
01911 *            MOVE CF-ST-STAT-INTEREST-2
01912 *                               TO STATR2O
01913 *        ELSE
01914 *            MOVE ZEROS         TO STATR2O
01915 *    ELSE
01916 *        MOVE ZEROS             TO STATR2O.
01917
01918 *    IF PI-COMPANY-ID = 'NCL'
01919 *    IF  CF-ST-STAT-INTEREST-3 NUMERIC
01920 *        IF  CF-ST-STAT-INTEREST-3 NOT = ZEROS
01921 *            MOVE CF-ST-STAT-INTEREST-3
01922 *                               TO STATR3O
01923 *        ELSE
01924 *            MOVE ZEROS         TO STATR3O
01925 *    ELSE
01926 *        MOVE ZEROS             TO STATR3O.
01927
PEMMOD*    MOVE CF-ST-STAT-DATE-FROM  TO STATIO.
01930
01931      MOVE CF-LAST-MAINT-BY      TO LSTUSRO.
01932      MOVE ' '                   TO DC-OPTION-CODE.
01933      MOVE CF-LAST-MAINT-DT      TO DC-BIN-DATE-1.
01934      MOVE LINK-ELDATCV          TO PGM-NAME.
01935      
      * EXEC CICS LINK
01936 *        PROGRAM (PGM-NAME)
01937 *        COMMAREA(DATE-CONVERSION-DATA)
01938 *        LENGTH  (DC-COMM-LENGTH)
01939 *    END-EXEC.
      *    MOVE '."C                   (   #00005271' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01940
01941      IF DATE-CONVERSION-ERROR
01942          MOVE ZEROS              TO LSTDTEO
01943      ELSE
01944          MOVE DC-GREG-DATE-1-EDIT
01945                                  TO LSTDTEO.
01946
01947      MOVE CF-LAST-MAINT-HHMMSS   TO TIME-IN.
01948      MOVE TIME-OUT               TO LSTTIMEO.
01949      MOVE -1                     TO MAINTL.
01950      MOVE CF-LAST-MAINT-BY       TO PI-UPDATE-BY.
01951      MOVE CF-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
01952      MOVE CF-STATE-CODE          TO PI-PREV-STATE.
01953      MOVE AL-UANOF               TO MAINTA.
01954      MOVE AL-UANON               TO STCDA
01955                                   STNAMEA
01956                                   STABRA
01957                                   CLREJECA ISSREJA REFREJA
040915                                  AGTSIGA netonlya
01958                                   EXTCHGA STUEA STCNTLA STDEVA
CIDMOD                                  REMTERMA CCREQA
PEMMOD                                  LFREDA LFLEVA LFNETA
022415                                  XINTA  XPMTSA
PEMMOD                                  AHAHA  AHCPA
01959                                   REPLAWA REPLETRA SPLPMTA.
PEMMOD*    IF PI-COMPANY-ID = 'NCL'
PEMMOD*        MOVE AL-UANON           TO STATIA.
01963
01964      MOVE AL-UNNON               TO LFEXPA AHEXPA
01965                                   QUOTCALA PREMTOLA REFTOLA
01966                                   OVSAMTA  OVSPCTA
PEMMOD                                  LFTAXA   AHITAXA  AHGTAXA
01967                                   PRMPCTA  REFPCTA
01968                                   REFDAY1A REFDAYSA
01969                                   REFMINA EXTDAYSA
01970                                   TARRATA
01971                                   CALCINTA
01972                                   RESTAXA FREELKA.
01974
PEMMOD*    IF PI-COMPANY-ID = 'NCL'
PEMMOD*        MOVE AL-UNNON           TO STATRA STATR1A STATR2A
PEMMOD*                                                  STATR3A
PEMMOD*                                                  INTDAYSA.
01979
01980      IF RETURN-DISPLAY
01981          GO TO 8100-SEND-INITIAL-MAP
01982      ELSE
01983          GO TO 8200-SEND-DATAONLY.
01984
01985      EJECT
01986  8100-SEND-INITIAL-MAP.
031512     IF (PI-COMPANY-ID = 'CID' OR 'AHL')
031512       AND PI-WS-STATE = 'AK'
100108        MOVE AL-SANON TO STCHKHDA STCHKNOA
100108     ELSE
100108        MOVE AL-SADOF TO STCHKHDA STCHKNOA
100108     END-IF.
01987      MOVE SAVE-DATE              TO RUNDTEO.
01988      MOVE EIBTIME                TO TIME-IN.
01989      MOVE TIME-OUT               TO RUNTIMEO.
01990      MOVE -1                     TO MAINTL.
01991      MOVE PI-LIFE-OVERRIDE-L6    TO LEXPLBLO.
01992      MOVE PI-AH-OVERRIDE-L6      TO AEXPLBLO.
01993      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
01994      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
01995
PEMMOD*    IF PI-COMPANY-ID = 'NCL'
PEMMOD*       MOVE AL-SANON            TO STATZA
01998 *                                   INTDAYZA
01999 *                                   STATRZA
02000 *                                   RNG1A
02001 *                                   RNG2A
02002 *                                   RNG3A
02003 *       MOVE AL-UANON            TO STATIA
02004 *                                   STATRA
02005 *                                   STATR1A
02006 *                                   STATR2A
02007 *                                   STATR3A
PEMMOD*       MOVE AL-UNNON            TO INTDAYSA.
02009
02010      
      * EXEC CICS SEND
02011 *        MAP   (MAP-NAME)
02012 *        MAPSET(MAPSET-NAME)
02013 *        FROM  (EL106AO)
02014 *        ERASE
02015 *        CURSOR
02016 *    END-EXEC.
           MOVE LENGTH OF
            EL106AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005356' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL106AO, 
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
           
02017
02018      GO TO 9100-RETURN-TRAN.
02019      EJECT
02020  8200-SEND-DATAONLY.
100108
031512     IF (PI-COMPANY-ID = 'CID' OR 'AHL')
031512       AND PI-WS-STATE = 'AK'
100108        MOVE AL-SANON TO STCHKHDA STCHKNOA
100108     ELSE
100108        MOVE AL-SADOF TO STCHKHDA STCHKNOA
100108     END-IF.
100108
02021      MOVE SAVE-DATE      TO RUNDTEO.
02022      MOVE EIBTIME        TO TIME-IN.
02023      MOVE TIME-OUT       TO RUNTIMEO.
02024
02025      IF QUOTCALL NOT = ZEROS
02026         IF QUOTCALI NUMERIC
02027            MOVE QUOTCALI TO QUOTCALO.
02028
02029      IF PREMTOLL NOT = ZEROS
02030         IF PREMTOLI NUMERIC
02031            MOVE PREMTOLI TO PREMTOLO.
02032
02033      IF OVSAMTL NOT = ZEROS
02034         IF OVSAMTI NUMERIC
02035            MOVE OVSAMTI TO OVSAMTO.
02036
02037      IF REFTOLL NOT = ZEROS
02038         IF REFTOLI NUMERIC
02039            MOVE REFTOLI TO REFTOLO.
02040
02041      IF PRMPCTL NOT = ZEROS
02042         IF PRMPCTI NUMERIC
02043            MOVE PRMPCTI TO PRMPCTO.
02044
02045      IF OVSPCTL NOT = ZEROS
02046         IF OVSPCTI NUMERIC
02047            MOVE OVSPCTI TO OVSPCTO.
02048
PEMMOD     IF LFTAXL NOT = ZEROS
PEMMOD        IF LFTAXI NUMERIC
PEMMOD           MOVE LFTAXI           TO LFTAXO
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHITAXL NOT = ZEROS
PEMMOD        IF AHITAXI NUMERIC
PEMMOD           MOVE AHITAXI          TO AHITAXO
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     IF AHGTAXL NOT = ZEROS
PEMMOD        IF AHGTAXI NUMERIC
PEMMOD           MOVE AHGTAXI          TO AHGTAXO
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
02049      IF REFPCTL NOT = ZEROS
02050         IF REFPCTI NUMERIC
02051            MOVE REFPCTI TO REFPCTO.
02052
02053      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
02054      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
02055
02056      
      * EXEC CICS SEND
02057 *        MAP   (MAP-NAME)
02058 *        MAPSET(MAPSET-NAME)
02059 *        FROM  (EL106AO)
02060 *        DATAONLY
02061 *        ERASEAUP
02062 *        CURSOR
02063 *    END-EXEC.
           MOVE LENGTH OF
            EL106AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00005428' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL106AO, 
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
           
02064
02065      GO TO 9100-RETURN-TRAN.
02066      EJECT
02067  8300-SEND-TEXT.
02068      
      * EXEC CICS SEND TEXT
02069 *        FROM  (LOGOFF-TEXT)
02070 *        LENGTH(LOGOFF-LENGTH)
02071 *        ERASE
02072 *        FREEKB
02073 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005440' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343430' TO DFHEIV0(25:11)
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
           
02074
02075      
      * EXEC CICS RETURN
02076 *    END-EXEC.
      *    MOVE '.(                    ''   #00005447' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02077
02078      EJECT
02079  8400-LOG-JOURNAL-RECORD.
02080      IF PI-JOURNAL-FILE-ID = 0
02081          GO TO 8400-EXIT.
02082
02083      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
02084      MOVE ELCNTL-ID              TO JP-FILE-ID.
02085      MOVE THIS-PGM               TO JP-PROGRAM-ID.
pemuni*    EXEC CICS JOURNAL
pemuni*         JFILEID(PI-JOURNAL-FILE-ID)
pemuni*         JTYPEID('EL')
pemuni*         FROM   (JOURNAL-RECORD)
pemuni*         LENGTH (773)
pemuni*    END-EXEC.
02092
02093  8400-EXIT.
02094      EXIT.
02095
02096  8800-UNAUTHORIZED-ACCESS.
02097      MOVE UNACCESS-MSG TO LOGOFF-MSG.
02098      GO TO 8300-SEND-TEXT.
02099
02100  8810-PF23.
02101      MOVE EIBAID   TO PI-ENTRY-CD-1.
02102      MOVE XCTL-EL005 TO PGM-NAME.
02103      GO TO 9300-XCTL.
02104
02105  8850-DUPREC.
02106      MOVE ER-0147 TO EMI-ERROR.
02107      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02108      MOVE -1       TO STCDL.
02109      MOVE AL-UABON TO STCDA.
02110      GO TO 8200-SEND-DATAONLY.
02111
02112  8860-ENDFILE.
02113      MOVE LOW-VALUES TO EL106AO.
02114      MOVE ER-0148    TO EMI-ERROR.
02115      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02116      MOVE -1 TO MAINTL.
100108     MOVE SPACES TO PI-WS-STATE.
02117      GO TO 8100-SEND-INITIAL-MAP.
02118
02119  8870-NOTOPEN.
02120      MOVE ER-0042 TO EMI-ERROR.
02121      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02122      MOVE -1 TO MAINTL.
02123      GO TO 8200-SEND-DATAONLY.
02124
02125  8880-NOT-FOUND.
02126      MOVE ER-0149 TO EMI-ERROR.
02127      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02128      MOVE -1       TO STCDL.
02129      MOVE AL-UABON TO STCDA.
02130      GO TO 8200-SEND-DATAONLY.
02131
02132  8890-LETR-NOT-OPEN.
02133      MOVE ER-0013                TO EMI-ERROR.
02134      MOVE -1                     TO REPLETRL.
02135      MOVE AL-UABON               TO REPLETRA.
02136      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02137      GO TO 8200-SEND-DATAONLY.
02138
02139  9000-RETURN-CICS.
02140      
      * EXEC CICS RETURN
02141 *    END-EXEC.
      *    MOVE '.(                    ''   #00005513' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02142
02143  9100-RETURN-TRAN.
02144      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
02145      MOVE '106A'                 TO PI-CURRENT-SCREEN-NO.
02146
02147      
      * EXEC CICS RETURN
02148 *        TRANSID (TRANS-ID)
02149 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
02150 *        LENGTH  (WS-COMM-LENGTH)
02151 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005520' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02152
02153  9200-RETURN-MAIN-MENU.
02154      IF  CREDIT-SESSION
02155          MOVE XCTL-EL626           TO PGM-NAME
02156      ELSE
02157      IF  CLAIM-SESSION
02158          MOVE XCTL-EL126           TO PGM-NAME
02159      ELSE
02160      IF  MORTGAGE-SESSION
02161          MOVE XCTL-EM626           TO PGM-NAME
02162      ELSE
02163      IF  GENERAL-LEDGER-SESSION
02164          MOVE XCTL-GL800           TO PGM-NAME.
02165
02166      GO TO 9300-XCTL.
02167
02168  9300-XCTL.
02169      
      * EXEC CICS XCTL
02170 *        PROGRAM (PGM-NAME)
02171 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
02172 *        LENGTH  (WS-COMM-LENGTH)
02173 *    END-EXEC.
      *    MOVE '.$C                   %   #00005542' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02174
02175  9400-CLEAR.
02176      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
02177      GO TO 9300-XCTL.
02178
02179  9500-PF12.
02180      MOVE XCTL-EL010 TO PGM-NAME.
02181      GO TO 9300-XCTL.
02182
02183  9600-PGMID-ERROR.
02184      
      * EXEC CICS HANDLE CONDITION
02185 *        PGMIDERR(8300-SEND-TEXT)
02186 *    END-EXEC.
      *    MOVE '"$L                   ! ) #00005557' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035353537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02187
02188      MOVE PGM-NAME      TO PI-CALLING-PROGRAM.
02189      MOVE ' '           TO PI-ENTRY-CD-1.
02190      MOVE XCTL-EL005    TO PGM-NAME.
02191      MOVE PGM-NAME      TO LOGOFF-PGM.
02192      MOVE PGMIDERR-MSG  TO LOGOFF-FILL.
02193      GO TO 9300-XCTL.
02194
02195  9700-LINK-DATE-CONVERT.
02196      
      * EXEC CICS LINK
02197 *        PROGRAM    ('ELDATCV')
02198 *        COMMAREA   (DATE-CONVERSION-DATA)
02199 *        LENGTH     (DC-COMM-LENGTH)
02200 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005569' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02201
02202  9700-EXIT.
02203      EXIT.
02204
02205  9900-ERROR-FORMAT.
02206      IF NOT EMI-ERRORS-COMPLETE
02207          MOVE LINK-EL001 TO PGM-NAME
02208          
      * EXEC CICS LINK
02209 *            PROGRAM (PGM-NAME)
02210 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
02211 *            LENGTH  (EMI-COMM-LENGTH)
02212 *        END-EXEC.
      *    MOVE '."C                   (   #00005581' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02213
02214  9900-EXIT.
02215      EXIT.
02216
092308 9910-INITIALIZE-SECURITY.
      ******************************************************************
      *                                                                *
      *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
      *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *
      *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
      *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
      *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
      *       ERROR CONDITION AND EXITS THE PROGRAM.                   *
      *                                                                *
      ******************************************************************
           IF  PI-PROCESSOR-ID NOT = 'LGXX'
               IF  MORTGAGE-SESSION
                   MOVE '125E'             TO SC-QUID-SYSTEM
                   MOVE EIBTRMID           TO SC-QUID-TERMINAL
                   
      * EXEC CICS READQ TS
      *                QUEUE  (SC-QUID-KEY)
      *                INTO   (SECURITY-CONTROL-E)
      *                LENGTH (SC-COMM-LENGTH-E)
      *                ITEM   (SC-ITEM)
      *            END-EXEC
      *    MOVE '*$II   L              ''   #00005605' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SC-QUID-KEY, 
                 SECURITY-CONTROL-E, 
                 SC-COMM-LENGTH-E, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                   MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)
                                           TO PI-DISPLAY-CAP
                   MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)
                                           TO PI-MODIFY-CAP
                   IF  NOT DISPLAY-CAP
                       MOVE 'READ'         TO SM-READ
                       PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
                       MOVE ER-9097        TO EMI-ERROR
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                       GO TO 8100-SEND-INITIAL-MAP
                   ELSE
                       GO TO 9910-EXIT
               ELSE
                   
      * EXEC CICS  READQ TS
      *                QUEUE   (PI-SECURITY-TEMP-STORE-ID)
      *                INTO    (SECURITY-CONTROL)
      *                LENGTH  (SC-COMM-LENGTH)
      *                ITEM    (SC-ITEM-CL-CR)
      *                END-EXEC
      *    MOVE '*$II   L              ''   #00005624' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM-CL-CR, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                   MOVE SC-CREDIT-DISPLAY (29)
                                       TO PI-DISPLAY-CAP
                   MOVE SC-CREDIT-UPDATE  (29)
                                       TO PI-MODIFY-CAP
                   IF  NOT DISPLAY-CAP
                       MOVE 'READ'     TO SM-READ
                       PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
                       MOVE ER-0070    TO  EMI-ERROR
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                       GO TO 8100-SEND-INITIAL-MAP.
092308 9910-EXIT.
           EXIT.
02217  9990-ABEND.
02218      MOVE LINK-EL004             TO PGM-NAME.
02219      MOVE DFHEIBLK               TO EMI-LINE1.
02220      
      * EXEC CICS LINK
02221 *        PROGRAM   (PGM-NAME)
02222 *        COMMAREA  (EMI-LINE1)
02223 *        LENGTH    (72)
02224 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005645' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02225
02226      GO TO 8200-SEND-DATAONLY.
02227
02228  9995-SECURITY-VIOLATION.
02229 *           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00005671' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363731' TO DFHEIV0(25:11)
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
02230
02231  9995-EXIT.
02232       EXIT.
02233

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL106' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8850-DUPREC,
                     8870-NOTOPEN,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8880-NOT-FOUND,
                     8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8100-SEND-INITIAL-MAP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8860-ENDFILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8860-ENDFILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 6010-LETR-NOT-FOUND,
                     6010-LETR-NOT-FOUND,
                     8890-LETR-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 6600-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL106' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
