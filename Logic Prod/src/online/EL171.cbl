00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL171.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 09:31:11.
00007 *                            VMOD=2.004.
00008 *
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00025 *REMARKS.    REPORT MENU.
00026
012103******************************************************************
012103*                   C H A N G E   L O G
012103*
012103* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
012103*-----------------------------------------------------------------
012103*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
012103* EFFECTIVE    NUMBER
012103*-----------------------------------------------------------------
012103* 012103                   SMVA  FIX SELECT CODE FUNCTIONALITY
121703* 121703                   SMVA  DISABLE PF11 CHECK RECON
012103******************************************************************
00027      EJECT
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL171 WORKING STORAGE     *'.
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.004 *********'.
00034
00035 *                                COPY ELCSCTM.
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
00037 *                                COPY ELCSCRTY.
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
00040      05  SAVE-DATE               PIC X(8)    VALUE SPACES.
00041      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.
00042
00043  01  MISC-WORK-AREAS.
00044      12  SC-ITEM                 PIC S9(4)   VALUE +0001  COMP.
00045      12  MAP-NAME                PIC X(8)    VALUE 'EL171A  '.
00046      12  MAPSET-NAME             PIC X(8)    VALUE 'EL171S  '.
00047      12  TRANS-ID                PIC X(4)    VALUE 'EX01'.
00048      12  THIS-PGM                PIC X(8)    VALUE 'EL171   '.
00049      12  PGM-NAME                PIC X(8).
00050      12  TIME-IN                 PIC S9(7).
00051      12  TIME-OUT-R  REDEFINES TIME-IN.
00052          16  FILLER              PIC X.
00053          16  TIME-OUT            PIC 99V99.
00054          16  FILLER              PIC X(2).
00055      12  XCTL-005                PIC X(5)    VALUE 'EL005'.
00056      12  XCTL-010                PIC X(5)    VALUE 'EL010'.
00057      12  XCTL-172                PIC X(5)    VALUE 'EL172'.
00058      12  XCTL-173                PIC X(5)    VALUE 'EL173'.
00059      12  XCTL-174                PIC X(5)    VALUE 'EL174'.
00060      12  XCTL-175                PIC X(5)    VALUE 'EL175'.
00061      12  XCTL-176                PIC X(5)    VALUE 'EL176'.
00062      12  XCTL-178                PIC X(5)    VALUE 'EL178'.
00063      12  XCTL-179                PIC X(5)    VALUE 'EL179'.
00064      12  XCTL-180                PIC X(5)    VALUE 'EL180'.
00065      12  XCTL-181                PIC X(5)    VALUE 'EL181'.
00066      12  XCTL-183                PIC X(5)    VALUE 'EL183'.
00067      12  XCTL-126                PIC X(5)    VALUE 'EL126'.
00068      12  XCTL-146                PIC X(5)    VALUE 'EL146'.
00069      12  LINK-001                PIC X(5)    VALUE 'EL001'.
00070      12  LINK-004                PIC X(5)    VALUE 'EL004'.
00071
00072      12  ER-0004                 PIC X(4)    VALUE '0004'.
00073      12  ER-0070                 PIC X(4)    VALUE '0070'.
00074      12  ER-7008                 PIC X(4)    VALUE '7008'.
00075      12  ER-0029                 PIC X(4)    VALUE '0029'.
00076      12  ER-2371                 PIC X(4)    VALUE '2371'.
121703     12  ER-9812                 PIC X(4)    VALUE '9812'.
00077
00078      EJECT
00079 *                                COPY ELCLOGOF.
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
00080
00081      EJECT
00082 *                                COPY ELCDATE.
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
00083
00084      EJECT
00085 *                                COPY ELCATTR.
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
00086
00087      EJECT
00088 *                                COPY ELCEMIB.
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
00089
00090      EJECT
00091 *                                COPY ELCINTF.
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
00092
00093      EJECT
00094 *                                COPY ELCAID.
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
00095
00096  01  FILLER    REDEFINES DFHAID.
00097      12  FILLER                  PIC X(8).
012103     12  PF-VALUES               PIC X       OCCURS 24.
00099
00100      EJECT
00101 *                                COPY EL171S.
       01  EL171AI.
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
           05  SELCODEL PIC S9(0004) COMP.
           05  SELCODEF PIC  X(0001).
           05  FILLER REDEFINES SELCODEF.
               10  SELCODEA PIC  X(0001).
           05  SELCODEI PIC  99.
      *    -------------------------------
           05  USERCDL PIC S9(0004) COMP.
           05  USERCDF PIC  X(0001).
           05  FILLER REDEFINES USERCDF.
               10  USERCDA PIC  X(0001).
           05  USERCDI PIC  X(0004).
      *    -------------------------------
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  ERRMSGI PIC  X(0070).
       01  EL171AO REDEFINES EL171AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SELCODEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERCDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0070).
      *    -------------------------------
00102
00103      EJECT
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
00105  01  DFHCOMMAREA                 PIC X(1024).
00106
00107      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL171' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00109
00110      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00111
00112      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00113      MOVE '5'                    TO DC-OPTION-CODE.
00114      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00115      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE.
00116      MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE.
00117
00118      IF EIBCALEN = 0
00119          GO TO 8800-UNAUTHORIZED-ACCESS.
00120
00121      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00122          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00123              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00124              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00125              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00126              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00127              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00128              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00129              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00130              MOVE THIS-PGM       TO PI-CALLING-PROGRAM
00131          ELSE
00132              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00133              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00134              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00135              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00136              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00137              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00138              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00139              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00140
00141      IF EIBTRNID NOT = TRANS-ID
00142          GO TO 8100-SEND-INITIAL-MAP.
00143
00144      
      * EXEC CICS HANDLE CONDITION
00145 *        PGMIDERR  (9600-PGMID-ERROR)
00146 *        ERROR     (9990-ABEND)
00147 *        END-EXEC.
      *    MOVE '"$L.                  ! " #00000997' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303030393937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00148
00149      IF EIBAID = DFHCLEAR
00150          GO TO 9400-CLEAR.
00151
00152      IF PI-PROCESSOR-ID = 'LGXX'
00153          GO TO 0200-RECEIVE.
00154
00155      
      * EXEC CICS READQ TS
00156 *        QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00157 *        INTO    (SECURITY-CONTROL)
00158 *        LENGTH  (SC-COMM-LENGTH)
00159 *        ITEM    (SC-ITEM)
00160 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00001008' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00161
00162      EJECT
00163  0200-RECEIVE.
00164      MOVE LOW-VALUES             TO EL171AI.
00165
00166      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00167          MOVE ER-7008            TO EMI-ERROR
00168          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00169          MOVE -1                 TO SELCODEL
00170          GO TO 8200-SEND-DATAONLY.
00171
00172      
      * EXEC CICS RECEIVE
00173 *        MAP     (MAP-NAME)
00174 *        MAPSET  (MAPSET-NAME)
00175 *        INTO    (EL171AI)
00176 *        END-EXEC.
           MOVE LENGTH OF
            EL171AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001025' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL171AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00177
00178      IF SELCODEL = 0
00179          GO TO 0300-CHECK-PFKEYS.
00180
00181      IF EIBAID NOT = DFHENTER
00182          MOVE ER-0004            TO EMI-ERROR
00183          GO TO 0320-INPUT-ERROR.
00184
00185      IF (SELCODEI NUMERIC) AND (SELCODEI GREATER 0 AND LESS 25)
00186          MOVE PF-VALUES (SELCODEI) TO EIBAID
00187      ELSE
00188          MOVE ER-0029            TO EMI-ERROR
00189          GO TO 0320-INPUT-ERROR.
00190
00191  0300-CHECK-PFKEYS.
00192      MOVE ' '                    TO PI-ENTRY-CD-1.
00193
00194      IF EIBAID = DFHPF23
00195          GO TO 8810-PF23.
00196
00197      IF EIBAID = DFHPF24
00198          GO TO 9200-RETURN-MAIN-MENU.
00199
00200      IF EIBAID = DFHPF12
00201          GO TO 9500-PF12.
00202
00203      IF USERCDL = ZERO
00204          MOVE SPACES             TO PI-PROGRAM-WORK-AREA
00205      ELSE
00206          MOVE USERCDI            TO PI-PROGRAM-WORK-AREA.
00207
00208      IF EIBAID = DFHPF1
00209          MOVE XCTL-172           TO PGM-NAME
00210          IF (NOT PI-NO-CARRIER-SECURITY) OR
00211             (NOT PI-NO-ACCOUNT-SECURITY)
00212             MOVE PI-PROCESSOR-ID TO PI-PROGRAM-WORK-AREA
00213             GO TO 9300-XCTL
00214          ELSE
00215             GO TO 9300-XCTL.
00216
00217      IF EIBAID = DFHPF2
00218          IF PI-PROCESSOR-ID = 'LGXX'
00219              MOVE XCTL-173                  TO PGM-NAME
00220              GO TO 9300-XCTL
00221          ELSE
00222              IF PI-NO-ACCOUNT-SECURITY
00223                 MOVE SC-CLAIMS-DISPLAY (17) TO  PI-DISPLAY-CAP
00224                 MOVE SC-CLAIMS-UPDATE  (17) TO  PI-MODIFY-CAP
00225                 IF NOT DISPLAY-CAP
00226                     MOVE XCTL-173           TO  THIS-PGM
00227                     MOVE 'READ'             TO  SM-READ
00228                     PERFORM 9995-SECURITY-VIOLATION
00229                                       THRU 9995-EXIT
00230                     MOVE ER-0070            TO  EMI-ERROR
00231                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00232                     GO TO 8100-SEND-INITIAL-MAP
00233                 ELSE
00234                     MOVE XCTL-173        TO PGM-NAME
00235                     GO TO 9300-XCTL
00236              ELSE
00237                  MOVE ER-2371         TO EMI-ERROR
00238                  GO TO 0320-INPUT-ERROR.
00239
00240      IF EIBAID = DFHPF3
00241          IF PI-PROCESSOR-ID = 'LGXX'
00242              MOVE XCTL-174              TO  PGM-NAME
00243              GO TO 9300-XCTL
00244          ELSE
00245              IF PI-NO-ACCOUNT-SECURITY
00246                  MOVE SC-CLAIMS-DISPLAY (11) TO  PI-DISPLAY-CAP
00247                  MOVE SC-CLAIMS-UPDATE  (11) TO  PI-MODIFY-CAP
00248                  IF NOT DISPLAY-CAP
00249                      MOVE XCTL-174           TO  THIS-PGM
00250                      MOVE 'READ'             TO  SM-READ
00251                      PERFORM 9995-SECURITY-VIOLATION
00252                                              THRU 9995-EXIT
00253                      MOVE ER-0070            TO  EMI-ERROR
00254                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00255                      GO TO 8100-SEND-INITIAL-MAP
00256                  ELSE
00257                      MOVE XCTL-174        TO PGM-NAME
00258                      GO TO 9300-XCTL
00259              ELSE
00260                  MOVE ER-2371         TO EMI-ERROR
00261                  GO TO 0320-INPUT-ERROR.
00262
00263      IF EIBAID = DFHPF4
00264          IF PI-PROCESSOR-ID = 'LGXX'
00265              MOVE XCTL-175                  TO  PGM-NAME
00266              GO TO 9300-XCTL
00267          ELSE
00268              IF PI-NO-ACCOUNT-SECURITY
00269                  MOVE SC-CLAIMS-DISPLAY (12) TO  PI-DISPLAY-CAP
00270                  MOVE SC-CLAIMS-UPDATE  (12) TO  PI-MODIFY-CAP
00271                  IF NOT DISPLAY-CAP
00272                      MOVE XCTL-175           TO  THIS-PGM
00273                      MOVE 'READ'             TO  SM-READ
00274                      PERFORM 9995-SECURITY-VIOLATION
00275                                              THRU 9995-EXIT
00276                      MOVE ER-0070            TO  EMI-ERROR
00277                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00278                      GO TO 8100-SEND-INITIAL-MAP
00279                  ELSE
00280                      MOVE XCTL-175        TO PGM-NAME
00281                      GO TO 9300-XCTL
00282              ELSE
00283                  MOVE ER-2371         TO EMI-ERROR
00284                  GO TO 0320-INPUT-ERROR.
00285
00286      IF EIBAID = DFHPF5
00287          IF PI-PROCESSOR-ID = 'LGXX'
00288              MOVE XCTL-176              TO  PGM-NAME
00289              GO TO 9300-XCTL
00290          ELSE
00291              IF PI-NO-ACCOUNT-SECURITY
00292                  MOVE SC-CLAIMS-DISPLAY (13) TO  PI-DISPLAY-CAP
00293                  MOVE SC-CLAIMS-UPDATE  (13) TO  PI-MODIFY-CAP
00294                  IF NOT DISPLAY-CAP
00295                      MOVE XCTL-176           TO  THIS-PGM
00296                      MOVE 'READ'             TO  SM-READ
00297                      PERFORM 9995-SECURITY-VIOLATION
00298                                              THRU 9995-EXIT
00299                      MOVE ER-0070            TO  EMI-ERROR
00300                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00301                      GO TO 8100-SEND-INITIAL-MAP
00302                  ELSE
00303                      MOVE XCTL-176        TO PGM-NAME
00304                      GO TO 9300-XCTL
00305              ELSE
00306                  MOVE ER-2371         TO EMI-ERROR
00307                  GO TO 0320-INPUT-ERROR.
00308
00309      IF EIBAID = DFHPF6
00310          IF PI-PROCESSOR-ID = 'LGXX'
00311              MOVE XCTL-178                   TO  PGM-NAME
00312              GO TO 9300-XCTL
00313          ELSE
00314              IF PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY
00315                  MOVE SC-CLAIMS-DISPLAY (20) TO  PI-DISPLAY-CAP
00316                  MOVE SC-CLAIMS-UPDATE  (20) TO  PI-MODIFY-CAP
00317                  IF NOT DISPLAY-CAP
00318                      MOVE XCTL-178           TO  THIS-PGM
00319                      MOVE 'READ'             TO  SM-READ
00320                      PERFORM 9995-SECURITY-VIOLATION
00321                                              THRU 9995-EXIT
00322                      MOVE ER-0070            TO  EMI-ERROR
00323                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00324                      GO TO 8100-SEND-INITIAL-MAP
00325                  ELSE
00326                      MOVE XCTL-178           TO PGM-NAME
00327                      GO TO 9300-XCTL
00328              ELSE
00329                  MOVE ER-2371                TO EMI-ERROR
00330                  GO TO 0320-INPUT-ERROR.
00331
00332      IF EIBAID = DFHPF7
00333          IF PI-PROCESSOR-ID = 'LGXX'
00334              MOVE XCTL-181               TO  PGM-NAME
00335              GO TO 9300-XCTL
00336          ELSE
00337              MOVE SC-CLAIMS-DISPLAY (18) TO  PI-DISPLAY-CAP
00338              MOVE SC-CLAIMS-UPDATE  (18) TO  PI-MODIFY-CAP
00339              IF NOT DISPLAY-CAP
00340                  MOVE XCTL-181           TO  THIS-PGM
00341                  MOVE 'READ'             TO  SM-READ
00342                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00343                  MOVE ER-0070            TO  EMI-ERROR
00344                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00345                  GO TO 8100-SEND-INITIAL-MAP
00346              ELSE
00347                  MOVE XCTL-181           TO PGM-NAME
00348                  GO TO 9300-XCTL.
00349
00350      IF EIBAID = DFHPF8
00351          IF PI-PROCESSOR-ID = 'LGXX'
00352              MOVE XCTL-180                  TO PGM-NAME
00353              GO TO 9300-XCTL
00354          ELSE
00355              IF PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY
00356                  MOVE SC-CLAIMS-DISPLAY (19) TO  PI-DISPLAY-CAP
00357                  MOVE SC-CLAIMS-UPDATE  (19) TO  PI-MODIFY-CAP
00358                  IF NOT DISPLAY-CAP
00359                      MOVE XCTL-180           TO  THIS-PGM
00360                      MOVE 'READ'             TO  SM-READ
00361                      PERFORM 9995-SECURITY-VIOLATION
00362                                              THRU 9995-EXIT
00363                      MOVE ER-0070            TO  EMI-ERROR
00364                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00365                      GO TO 8100-SEND-INITIAL-MAP
00366                  ELSE
00367                      MOVE XCTL-180        TO PGM-NAME
00368                      GO TO 9300-XCTL
00369              ELSE
00370                  MOVE ER-2371         TO EMI-ERROR
00371                  GO TO 0320-INPUT-ERROR.
00372
00373      IF EIBAID = DFHPF9
00374          IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'
00375              MOVE XCTL-179                   TO  PGM-NAME
00376              GO TO 9300-XCTL
00377          ELSE
00378              IF PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY
00379                  MOVE SC-CLAIMS-DISPLAY (27) TO  PI-DISPLAY-CAP
00380                  MOVE SC-CLAIMS-UPDATE  (27) TO  PI-MODIFY-CAP
00381                  IF NOT DISPLAY-CAP
00382                      MOVE XCTL-179           TO  PGM-NAME
00383                      MOVE 'READ'             TO  SM-READ
00384                      PERFORM 9995-SECURITY-VIOLATION
00385                                              THRU 9995-EXIT
00386                      MOVE ER-0070            TO  EMI-ERROR
00387                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00388                      GO TO 8100-SEND-INITIAL-MAP
00389                  ELSE
00390                      MOVE XCTL-179           TO  PGM-NAME
00391                      GO TO 9300-XCTL
00392              ELSE
00393                  MOVE ER-2371                TO  EMI-ERROR
00394                  GO TO 0320-INPUT-ERROR.
00395
00396      IF EIBAID = DFHPF10
00397          IF PI-PROCESSOR-ID = 'LGXX'
00398              MOVE XCTL-183                   TO  PGM-NAME
00399              GO TO 9300-XCTL
00400          ELSE
00401              IF PI-NO-CARRIER-SECURITY AND PI-NO-ACCOUNT-SECURITY
00402                  MOVE SC-CLAIMS-DISPLAY (26) TO  PI-DISPLAY-CAP
00403                  MOVE SC-CLAIMS-UPDATE  (26) TO  PI-MODIFY-CAP
00404                  IF NOT DISPLAY-CAP
00405                      MOVE XCTL-183           TO  THIS-PGM
00406                      MOVE 'READ'             TO  SM-READ
00407                      PERFORM 9995-SECURITY-VIOLATION
00408                                             THRU 9995-EXIT
00409                      MOVE ER-0070            TO  EMI-ERROR
00410                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00411                      GO TO 8100-SEND-INITIAL-MAP
00412                  ELSE
00413                      MOVE XCTL-183           TO PGM-NAME
00414                      GO TO 9300-XCTL
00415              ELSE
00416                  MOVE ER-2371                TO EMI-ERROR
00417                  GO TO 0320-INPUT-ERROR.
00418
00419      IF EIBAID = DFHPF11
121703         MOVE ER-9812                       TO EMI-ERROR
121703         PERFORM 9900-ERROR-FORMAT          THRU 9900-EXIT
121703         GO TO 8100-SEND-INITIAL-MAP
121703     END-IF.
012103*        IF PI-PROCESSOR-ID = 'LGXX'
012103*            MOVE XCTL-146                  TO PGM-NAME
012103*            GO TO 9300-XCTL
012103*        ELSE
121703*            MOVE SC-CLAIMS-DISPLAY (23)    TO  PI-DISPLAY-CAP
121703*            MOVE SC-CLAIMS-UPDATE  (23)    TO  PI-MODIFY-CAP
121703*            IF NOT DISPLAY-CAP
121703*                MOVE XCTL-146              TO  THIS-PGM
121703*                MOVE 'READ'                TO  SM-READ
121703*                PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
121703*                MOVE ER-0070               TO  EMI-ERROR
121703*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121703*                GO TO 8100-SEND-INITIAL-MAP
121703*            ELSE
121703*                MOVE XCTL-146              TO PGM-NAME
121703*                GO TO 9300-XCTL.
00436
00437      MOVE ER-0029                TO EMI-ERROR.
00438
00439
00440      EJECT
00441  0320-INPUT-ERROR.
00442      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00443      MOVE AL-UNBON               TO SELCODEA.
00444      MOVE -1                     TO SELCODEL.
00445      GO TO 8200-SEND-DATAONLY.
00446
00447
00448
00449  8100-SEND-INITIAL-MAP.
00450      MOVE LOW-VALUES             TO EL171AO.
00451      MOVE EIBTIME                TO TIME-IN.
00452      MOVE TIME-OUT               TO RUNTIMEO.
00453      MOVE SAVE-DATE              TO RUNDTEO.
00454      MOVE -1                     TO SELCODEL.
00455      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
00456
00457      
      * EXEC CICS SEND
00458 *        MAP     (MAP-NAME)
00459 *        MAPSET  (MAPSET-NAME)
00460 *        FROM    (EL171AO)
00461 *        ERASE
00462 *        CURSOR
00463 *        END-EXEC.
           MOVE LENGTH OF
            EL171AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00001314' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL171AO, 
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
           
00464
00465      GO TO 9100-RETURN-TRAN.
00466
00467
00468      EJECT
00469  8200-SEND-DATAONLY.
00470      MOVE SAVE-DATE              TO RUNDTEO.
00471      MOVE EIBTIME                TO TIME-IN.
00472      MOVE TIME-OUT               TO RUNTIMEO.
00473      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
00474
00475      
      * EXEC CICS SEND
00476 *        MAP     (MAP-NAME)
00477 *        MAPSET  (MAPSET-NAME)
00478 *        FROM    (EL171AO)
00479 *        DATAONLY
00480 *        CURSOR
00481 *        END-EXEC.
           MOVE LENGTH OF
            EL171AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00001332' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL171AO, 
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
           
00482
00483      GO TO 9100-RETURN-TRAN.
00484
00485
00486
00487  8300-SEND-TEXT.
00488
00489      
      * EXEC CICS SEND TEXT
00490 *        FROM    (LOGOFF-TEXT)
00491 *        LENGTH  (LOGOFF-LENGTH)
00492 *        ERASE
00493 *        FREEKB
00494 *        END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001346' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333436' TO DFHEIV0(25:11)
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
           
00495
00496      
      * EXEC CICS RETURN
00497 *        END-EXEC.
      *    MOVE '.(                    &   #00001353' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00498
00499
00500
00501  8800-UNAUTHORIZED-ACCESS.
00502      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
00503      GO TO 8300-SEND-TEXT.
00504
00505
00506
00507  8810-PF23.
00508      MOVE EIBAID                 TO PI-ENTRY-CD-1.
00509      MOVE XCTL-005               TO PGM-NAME.
00510      GO TO 9300-XCTL.
00511
00512      EJECT
00511
CIDMOD 9000-RETURN-CICS.
CIDMOD     
      * EXEC CICS RETURN
CIDMOD*        END-EXEC.
      *    MOVE '.(                    &   #00001372' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
00513
00514  9100-RETURN-TRAN.
00515      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
00516      MOVE '171A'                 TO PI-CURRENT-SCREEN-NO.
00517
00518      
      * EXEC CICS RETURN
00519 *        TRANSID   (TRANS-ID)
00520 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00521 *        LENGTH    (PI-COMM-LENGTH)
00522 *        END-EXEC.
      *    MOVE '.(CT                  &   #00001380' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00523
00524
00525
00526  9200-RETURN-MAIN-MENU.
00527      MOVE XCTL-126               TO PGM-NAME.
00528      GO TO 9300-XCTL.
00529
00530
00531
00532  9300-XCTL.
00533      MOVE SPACES                 TO PI-CONTROL-IN-PROGRESS.
00534      MOVE SPACES                 TO PI-ENTRY-CD-2.
00535      MOVE SPACES                 TO PI-RETURN-CODES.
00536      MOVE SPACES                 TO PI-UPDATE-BY.
00537      MOVE ZEROS                  TO PI-UPDATE-HHMMSS.
00538      MOVE SPACES                 TO PI-PROGRAM-CONTROLS.
00539
00540      
      * EXEC CICS XCTL
00541 *        PROGRAM   (PGM-NAME)
00542 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00543 *        LENGTH  (PI-COMM-LENGTH)
00544 *        END-EXEC.
      *    MOVE '.$C                   $   #00001402' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00545
00546
00547
00548  9400-CLEAR.
00549      MOVE XCTL-126               TO PGM-NAME.
00550      GO TO 9300-XCTL.
00551
00552
00553
00554  9500-PF12.
00555      MOVE XCTL-010               TO PGM-NAME.
00556      GO TO 9300-XCTL.
00557
00558
00559      EJECT
00560  9600-PGMID-ERROR.
00561      
      * EXEC CICS HANDLE CONDITION
00562 *        PGMIDERR  (8300-SEND-TEXT)
00563 *        END-EXEC.
      *    MOVE '"$L                   ! # #00001423' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031343233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00564
00565      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
00566      MOVE ' '                    TO PI-ENTRY-CD-1.
00567      MOVE XCTL-005               TO PGM-NAME.
00568      MOVE PGM-NAME               TO LOGOFF-PGM.
00569      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
00570      GO TO 9300-XCTL.
00571
00572
00573  9700-LINK-DATE-CONVERT.
00574      
      * EXEC CICS LINK
00575 *        PROGRAM    ('ELDATCV')
00576 *        COMMAREA   (DATE-CONVERSION-DATA)
00577 *        LENGTH     (DC-COMM-LENGTH)
00578 *        END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00001436' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00579
00580  9700-EXIT.
00581      EXIT.
00582
00583
00584  9900-ERROR-FORMAT.
00585      IF NOT EMI-ERRORS-COMPLETE
00586          MOVE LINK-001           TO PGM-NAME
00587          
      * EXEC CICS LINK
00588 *            PROGRAM   (PGM-NAME)
00589 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
00590 *            LENGTH    (EMI-COMM-LENGTH)
00591 *            END-EXEC.
      *    MOVE '."C                   ''   #00001449' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00592
00593  9900-EXIT.
00594      EXIT.
00595
00596
00597  9990-ABEND.
00598      MOVE LINK-004               TO PGM-NAME.
00599      MOVE DFHEIBLK               TO EMI-LINE1.
00600
00601      
      * EXEC CICS LINK
00602 *        PROGRAM   (PGM-NAME)
00603 *        COMMAREA  (EMI-LINE1)
00604 *        LENGTH    (72)
00605 *        END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00001463' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00606
00607      GO TO 8200-SEND-DATAONLY.
00608
00609  9995-SECURITY-VIOLATION.
00610 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00001489' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343839' TO DFHEIV0(25:11)
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
00611
00612  9995-EXIT.
00613      EXIT.
00614
CIDMOD 9999-LAST-PARAGRAPH.
CIDMOD     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL171' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL171' TO DFHEIV1
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
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL171' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
