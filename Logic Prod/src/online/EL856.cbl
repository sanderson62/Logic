00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL856 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/14/96 08:09:25.
00007 *                            VMOD=2.003
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
00023
00024 *REMARKS. TRANSACTION - EXJ8 - ACCOUNTS RECEIVABLE
00025 *                              SUMMARY CROSS REFERENCE.
00026
00027  ENVIRONMENT DIVISION.
00028
00029      EJECT
00030  DATA DIVISION.
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032
00033  77  FILLER  PIC X(32)  VALUE '********************************'.
00034  77  FILLER  PIC X(32)  VALUE '*    EL856 WORKING STORAGE     *'.
00035  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.003 *********'.
00036
00037     EJECT
00038
00039 *                            COPY ELCSCTM.
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
00040 *                            COPY ELCSCRTY.
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
00041
00042     EJECT
00043
00044 ******************************************************************
00045 *                                                                *
00046 *              S T A N D A R D   A R E A S                       *
00047 *                                                                *
00048 ******************************************************************
00049
00050  01  STANDARD-AREAS.
00051      12  SC-ITEM                 PIC S9(4)   VALUE +1 COMP.
00052      12  QID.
00053          16  QID-TERM            PIC X(4)      VALUE SPACES.
00054          16  FILLER              PIC X(4)      VALUE '125D'.
00055
00056      12  GETMAIN-SPACE           PIC X       VALUE SPACE.
00057      12  EL856A                  PIC X(8)    VALUE 'EL856A'.
00058      12  MAPSET-EL856S           PIC X(8)    VALUE 'EL856S '.
00059      12  TRANS-EXJ8              PIC X(4)    VALUE 'EXJ8'.
00060      12  TRANS-EXD4              PIC X(4)    VALUE 'EXD4'.
00061      12  THIS-PGM                PIC X(8)    VALUE 'EL856'.
00062      12  PGM-NAME                PIC X(8).
00063      12  TIME-IN                 PIC S9(7).
00064      12  TIME-OUT-R  REDEFINES TIME-IN.
00065          16  FILLER              PIC X.
00066          16  TIME-OUT            PIC 99V99.
00067          16  FILLER              PIC X(2).
00068      12  LINK-EL001              PIC X(8)    VALUE 'EL001'.
00069      12  LINK-EL004              PIC X(8)    VALUE 'EL004'.
00070      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.
00071      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.
00072      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.
00073      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
00074      12  FILE-ID-ERSUMM          PIC X(8)    VALUE 'ERSUMM'.
00075      12  WS-CURRENT-DT           PIC X(8)    VALUE SPACES.
00076      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.
00077      12  WS-BROWSE-STARTED-SW    PIC X       VALUE SPACE.
00078          88 WS-BROWSE-STARTED                VALUE 'Y'.
00079      12  WS-SUB1                 PIC S9(4)   VALUE ZEROS COMP.
00080      12  WS-ACCESS-KEY           PIC X(34)   VALUE SPACES.
00081
00082      EJECT
00083
00084 ******************************************************************
00085 *                                                                *
00086 *                E R R O R   M E S S A G E S                     *
00087 *                                                                *
00088 ******************************************************************
00089
00090  01  ERROR-MESSAGES.
00091      12  ER-0000                 PIC X(4)  VALUE '0000'.
00092      12  ER-0004                 PIC X(4)  VALUE '0004'.
00093      12  ER-0008                 PIC X(4)  VALUE '0008'.
00094      12  ER-0023                 PIC X(4)  VALUE '0023'.
00095      12  ER-0029                 PIC X(4)  VALUE '0029'.
00096      12  ER-0070                 PIC X(4)  VALUE '0070'.
00097      12  ER-2132                 PIC X(4)  VALUE '2132'.
00098      12  ER-2237                 PIC X(4)  VALUE '2237'.
00099      12  ER-2238                 PIC X(4)  VALUE '2238'.
00100      12  ER-3133                 PIC X(4)  VALUE '3133'.
00101      12  ER-3135                 PIC X(4)  VALUE '3135'.
00102
00103      EJECT
00104
00105 ******************************************************************
00106 *                                                                *
00107 *              A C C E S S   K E Y S                             *
00108 *                                                                *
00109 ******************************************************************
00110
00111  01  ACCESS-KEYS.
00112
00113      12  ERSUMM-KEY.
00114          16  ERSUMM-COMPANY-CD      PIC X     VALUE SPACE.
00115          16  ERSUMM-SUMMARY         PIC X(6)  VALUE SPACE.
00116          16  ERSUMM-CARRIER         PIC X     VALUE SPACE.
00117          16  ERSUMM-GROUP           PIC X(6)  VALUE SPACE.
00118          16  ERSUMM-FIN-RESP        PIC X(10) VALUE SPACE.
00119          16  ERSUMM-ACCT-AGENT      PIC X(10) VALUE SPACE.
00120
00121      EJECT
00122
00123 *                            COPY ELCDATE.
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
00124
00125      EJECT
00126 *                            COPY ELCLOGOF.
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
00127
00128      EJECT
00129 *                            COPY ELCATTR.
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
00130
00131      EJECT
00132 *                            COPY ELCEMIB.
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
00133
00134      EJECT
00135 *                            COPY ELCINTF.
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
00136      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00137
00138          16  PI-SUMMARY-CODE         PIC X(6).
00139          16  PI-1ST-SUM-KEY          PIC X(34).
00140          16  PI-LST-SUM-KEY          PIC X(34).
00141
00142          16  PI-TOP-OF-FILE-SW       PIC X.
00143              88  PI-TOP-OF-FILE            VALUE 'Y'.
00144
00145          16  PI-END-OF-FILE-SW       PIC X.
00146              88  PI-END-OF-FILE            VALUE 'Y'.
00147          16  FILLER                  PIC X(564).
00148
00149      EJECT
00150 *                            COPY ELCJPFX.
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
00151                              PIC X(223).
00152
00153      EJECT
00154 *                            COPY ELCAID.
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
00155  01  FILLER    REDEFINES DFHAID.
00156      12  FILLER              PIC X(8).
00157      12  PF-VALUES           PIC X       OCCURS 2.
00158
00159      EJECT
00160 *                            COPY EL856S.
       01  EL856AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  SUMCODEL PIC S9(0004) COMP.
           05  SUMCODEF PIC  X(0001).
           05  FILLER REDEFINES SUMCODEF.
               10  SUMCODEA PIC  X(0001).
           05  SUMCODEI PIC  X(0006).
      *    -------------------------------
           05  SUMNAMEL PIC S9(0004) COMP.
           05  SUMNAMEF PIC  X(0001).
           05  FILLER REDEFINES SUMNAMEF.
               10  SUMNAMEA PIC  X(0001).
           05  SUMNAMEI PIC  X(0030).
      *    -------------------------------
           05  CAR1L PIC S9(0004) COMP.
           05  CAR1F PIC  X(0001).
           05  FILLER REDEFINES CAR1F.
               10  CAR1A PIC  X(0001).
           05  CAR1I PIC  X(0001).
      *    -------------------------------
           05  GRP1L PIC S9(0004) COMP.
           05  GRP1F PIC  X(0001).
           05  FILLER REDEFINES GRP1F.
               10  GRP1A PIC  X(0001).
           05  GRP1I PIC  X(0006).
      *    -------------------------------
           05  FINRSP1L PIC S9(0004) COMP.
           05  FINRSP1F PIC  X(0001).
           05  FILLER REDEFINES FINRSP1F.
               10  FINRSP1A PIC  X(0001).
           05  FINRSP1I PIC  X(0010).
      *    -------------------------------
           05  AGENT1L PIC S9(0004) COMP.
           05  AGENT1F PIC  X(0001).
           05  FILLER REDEFINES AGENT1F.
               10  AGENT1A PIC  X(0001).
           05  AGENT1I PIC  X(0010).
      *    -------------------------------
           05  AGTNAM1L PIC S9(0004) COMP.
           05  AGTNAM1F PIC  X(0001).
           05  FILLER REDEFINES AGTNAM1F.
               10  AGTNAM1A PIC  X(0001).
           05  AGTNAM1I PIC  X(0030).
      *    -------------------------------
           05  CAR2L PIC S9(0004) COMP.
           05  CAR2F PIC  X(0001).
           05  FILLER REDEFINES CAR2F.
               10  CAR2A PIC  X(0001).
           05  CAR2I PIC  X(0001).
      *    -------------------------------
           05  GRP2L PIC S9(0004) COMP.
           05  GRP2F PIC  X(0001).
           05  FILLER REDEFINES GRP2F.
               10  GRP2A PIC  X(0001).
           05  GRP2I PIC  X(0006).
      *    -------------------------------
           05  FINRSP2L PIC S9(0004) COMP.
           05  FINRSP2F PIC  X(0001).
           05  FILLER REDEFINES FINRSP2F.
               10  FINRSP2A PIC  X(0001).
           05  FINRSP2I PIC  X(0010).
      *    -------------------------------
           05  AGENT2L PIC S9(0004) COMP.
           05  AGENT2F PIC  X(0001).
           05  FILLER REDEFINES AGENT2F.
               10  AGENT2A PIC  X(0001).
           05  AGENT2I PIC  X(0010).
      *    -------------------------------
           05  AGTNAM2L PIC S9(0004) COMP.
           05  AGTNAM2F PIC  X(0001).
           05  FILLER REDEFINES AGTNAM2F.
               10  AGTNAM2A PIC  X(0001).
           05  AGTNAM2I PIC  X(0030).
      *    -------------------------------
           05  CAR3L PIC S9(0004) COMP.
           05  CAR3F PIC  X(0001).
           05  FILLER REDEFINES CAR3F.
               10  CAR3A PIC  X(0001).
           05  CAR3I PIC  X(0001).
      *    -------------------------------
           05  GRP3L PIC S9(0004) COMP.
           05  GRP3F PIC  X(0001).
           05  FILLER REDEFINES GRP3F.
               10  GRP3A PIC  X(0001).
           05  GRP3I PIC  X(0006).
      *    -------------------------------
           05  FINRSP3L PIC S9(0004) COMP.
           05  FINRSP3F PIC  X(0001).
           05  FILLER REDEFINES FINRSP3F.
               10  FINRSP3A PIC  X(0001).
           05  FINRSP3I PIC  X(0010).
      *    -------------------------------
           05  AGENT3L PIC S9(0004) COMP.
           05  AGENT3F PIC  X(0001).
           05  FILLER REDEFINES AGENT3F.
               10  AGENT3A PIC  X(0001).
           05  AGENT3I PIC  X(0010).
      *    -------------------------------
           05  AGTNAM3L PIC S9(0004) COMP.
           05  AGTNAM3F PIC  X(0001).
           05  FILLER REDEFINES AGTNAM3F.
               10  AGTNAM3A PIC  X(0001).
           05  AGTNAM3I PIC  X(0030).
      *    -------------------------------
           05  CAR4L PIC S9(0004) COMP.
           05  CAR4F PIC  X(0001).
           05  FILLER REDEFINES CAR4F.
               10  CAR4A PIC  X(0001).
           05  CAR4I PIC  X(0001).
      *    -------------------------------
           05  GRP4L PIC S9(0004) COMP.
           05  GRP4F PIC  X(0001).
           05  FILLER REDEFINES GRP4F.
               10  GRP4A PIC  X(0001).
           05  GRP4I PIC  X(0006).
      *    -------------------------------
           05  FINRSP4L PIC S9(0004) COMP.
           05  FINRSP4F PIC  X(0001).
           05  FILLER REDEFINES FINRSP4F.
               10  FINRSP4A PIC  X(0001).
           05  FINRSP4I PIC  X(0010).
      *    -------------------------------
           05  AGENT4L PIC S9(0004) COMP.
           05  AGENT4F PIC  X(0001).
           05  FILLER REDEFINES AGENT4F.
               10  AGENT4A PIC  X(0001).
           05  AGENT4I PIC  X(0010).
      *    -------------------------------
           05  AGTNAM4L PIC S9(0004) COMP.
           05  AGTNAM4F PIC  X(0001).
           05  FILLER REDEFINES AGTNAM4F.
               10  AGTNAM4A PIC  X(0001).
           05  AGTNAM4I PIC  X(0030).
      *    -------------------------------
           05  CAR5L PIC S9(0004) COMP.
           05  CAR5F PIC  X(0001).
           05  FILLER REDEFINES CAR5F.
               10  CAR5A PIC  X(0001).
           05  CAR5I PIC  X(0001).
      *    -------------------------------
           05  GRP5L PIC S9(0004) COMP.
           05  GRP5F PIC  X(0001).
           05  FILLER REDEFINES GRP5F.
               10  GRP5A PIC  X(0001).
           05  GRP5I PIC  X(0006).
      *    -------------------------------
           05  FINRSP5L PIC S9(0004) COMP.
           05  FINRSP5F PIC  X(0001).
           05  FILLER REDEFINES FINRSP5F.
               10  FINRSP5A PIC  X(0001).
           05  FINRSP5I PIC  X(0010).
      *    -------------------------------
           05  AGENT5L PIC S9(0004) COMP.
           05  AGENT5F PIC  X(0001).
           05  FILLER REDEFINES AGENT5F.
               10  AGENT5A PIC  X(0001).
           05  AGENT5I PIC  X(0010).
      *    -------------------------------
           05  AGTNAM5L PIC S9(0004) COMP.
           05  AGTNAM5F PIC  X(0001).
           05  FILLER REDEFINES AGTNAM5F.
               10  AGTNAM5A PIC  X(0001).
           05  AGTNAM5I PIC  X(0030).
      *    -------------------------------
           05  CAR6L PIC S9(0004) COMP.
           05  CAR6F PIC  X(0001).
           05  FILLER REDEFINES CAR6F.
               10  CAR6A PIC  X(0001).
           05  CAR6I PIC  X(0001).
      *    -------------------------------
           05  GRP6L PIC S9(0004) COMP.
           05  GRP6F PIC  X(0001).
           05  FILLER REDEFINES GRP6F.
               10  GRP6A PIC  X(0001).
           05  GRP6I PIC  X(0006).
      *    -------------------------------
           05  FINRSP6L PIC S9(0004) COMP.
           05  FINRSP6F PIC  X(0001).
           05  FILLER REDEFINES FINRSP6F.
               10  FINRSP6A PIC  X(0001).
           05  FINRSP6I PIC  X(0010).
      *    -------------------------------
           05  AGENT6L PIC S9(0004) COMP.
           05  AGENT6F PIC  X(0001).
           05  FILLER REDEFINES AGENT6F.
               10  AGENT6A PIC  X(0001).
           05  AGENT6I PIC  X(0010).
      *    -------------------------------
           05  AGTNAM6L PIC S9(0004) COMP.
           05  AGTNAM6F PIC  X(0001).
           05  FILLER REDEFINES AGTNAM6F.
               10  AGTNAM6A PIC  X(0001).
           05  AGTNAM6I PIC  X(0030).
      *    -------------------------------
           05  CAR7L PIC S9(0004) COMP.
           05  CAR7F PIC  X(0001).
           05  FILLER REDEFINES CAR7F.
               10  CAR7A PIC  X(0001).
           05  CAR7I PIC  X(0001).
      *    -------------------------------
           05  GRP7L PIC S9(0004) COMP.
           05  GRP7F PIC  X(0001).
           05  FILLER REDEFINES GRP7F.
               10  GRP7A PIC  X(0001).
           05  GRP7I PIC  X(0006).
      *    -------------------------------
           05  FINRSP7L PIC S9(0004) COMP.
           05  FINRSP7F PIC  X(0001).
           05  FILLER REDEFINES FINRSP7F.
               10  FINRSP7A PIC  X(0001).
           05  FINRSP7I PIC  X(0010).
      *    -------------------------------
           05  AGENT7L PIC S9(0004) COMP.
           05  AGENT7F PIC  X(0001).
           05  FILLER REDEFINES AGENT7F.
               10  AGENT7A PIC  X(0001).
           05  AGENT7I PIC  X(0010).
      *    -------------------------------
           05  AGTNAM7L PIC S9(0004) COMP.
           05  AGTNAM7F PIC  X(0001).
           05  FILLER REDEFINES AGTNAM7F.
               10  AGTNAM7A PIC  X(0001).
           05  AGTNAM7I PIC  X(0030).
      *    -------------------------------
           05  CAR8L PIC S9(0004) COMP.
           05  CAR8F PIC  X(0001).
           05  FILLER REDEFINES CAR8F.
               10  CAR8A PIC  X(0001).
           05  CAR8I PIC  X(0001).
      *    -------------------------------
           05  GRP8L PIC S9(0004) COMP.
           05  GRP8F PIC  X(0001).
           05  FILLER REDEFINES GRP8F.
               10  GRP8A PIC  X(0001).
           05  GRP8I PIC  X(0006).
      *    -------------------------------
           05  FINRSP8L PIC S9(0004) COMP.
           05  FINRSP8F PIC  X(0001).
           05  FILLER REDEFINES FINRSP8F.
               10  FINRSP8A PIC  X(0001).
           05  FINRSP8I PIC  X(0010).
      *    -------------------------------
           05  AGENT8L PIC S9(0004) COMP.
           05  AGENT8F PIC  X(0001).
           05  FILLER REDEFINES AGENT8F.
               10  AGENT8A PIC  X(0001).
           05  AGENT8I PIC  X(0010).
      *    -------------------------------
           05  AGTNAM8L PIC S9(0004) COMP.
           05  AGTNAM8F PIC  X(0001).
           05  FILLER REDEFINES AGTNAM8F.
               10  AGTNAM8A PIC  X(0001).
           05  AGTNAM8I PIC  X(0030).
      *    -------------------------------
           05  CAR9L PIC S9(0004) COMP.
           05  CAR9F PIC  X(0001).
           05  FILLER REDEFINES CAR9F.
               10  CAR9A PIC  X(0001).
           05  CAR9I PIC  X(0001).
      *    -------------------------------
           05  GRP9L PIC S9(0004) COMP.
           05  GRP9F PIC  X(0001).
           05  FILLER REDEFINES GRP9F.
               10  GRP9A PIC  X(0001).
           05  GRP9I PIC  X(0006).
      *    -------------------------------
           05  FINRSP9L PIC S9(0004) COMP.
           05  FINRSP9F PIC  X(0001).
           05  FILLER REDEFINES FINRSP9F.
               10  FINRSP9A PIC  X(0001).
           05  FINRSP9I PIC  X(0010).
      *    -------------------------------
           05  AGENT9L PIC S9(0004) COMP.
           05  AGENT9F PIC  X(0001).
           05  FILLER REDEFINES AGENT9F.
               10  AGENT9A PIC  X(0001).
           05  AGENT9I PIC  X(0010).
      *    -------------------------------
           05  AGTNAM9L PIC S9(0004) COMP.
           05  AGTNAM9F PIC  X(0001).
           05  FILLER REDEFINES AGTNAM9F.
               10  AGTNAM9A PIC  X(0001).
           05  AGTNAM9I PIC  X(0030).
      *    -------------------------------
           05  CARAL PIC S9(0004) COMP.
           05  CARAF PIC  X(0001).
           05  FILLER REDEFINES CARAF.
               10  CARAA PIC  X(0001).
           05  CARAI PIC  X(0001).
      *    -------------------------------
           05  GRPAL PIC S9(0004) COMP.
           05  GRPAF PIC  X(0001).
           05  FILLER REDEFINES GRPAF.
               10  GRPAA PIC  X(0001).
           05  GRPAI PIC  X(0006).
      *    -------------------------------
           05  FINRSPAL PIC S9(0004) COMP.
           05  FINRSPAF PIC  X(0001).
           05  FILLER REDEFINES FINRSPAF.
               10  FINRSPAA PIC  X(0001).
           05  FINRSPAI PIC  X(0010).
      *    -------------------------------
           05  AGENTAL PIC S9(0004) COMP.
           05  AGENTAF PIC  X(0001).
           05  FILLER REDEFINES AGENTAF.
               10  AGENTAA PIC  X(0001).
           05  AGENTAI PIC  X(0010).
      *    -------------------------------
           05  AGTNAMAL PIC S9(0004) COMP.
           05  AGTNAMAF PIC  X(0001).
           05  FILLER REDEFINES AGTNAMAF.
               10  AGTNAMAA PIC  X(0001).
           05  AGTNAMAI PIC  X(0030).
      *    -------------------------------
           05  CARBL PIC S9(0004) COMP.
           05  CARBF PIC  X(0001).
           05  FILLER REDEFINES CARBF.
               10  CARBA PIC  X(0001).
           05  CARBI PIC  X(0001).
      *    -------------------------------
           05  GRPBL PIC S9(0004) COMP.
           05  GRPBF PIC  X(0001).
           05  FILLER REDEFINES GRPBF.
               10  GRPBA PIC  X(0001).
           05  GRPBI PIC  X(0006).
      *    -------------------------------
           05  FINRSPBL PIC S9(0004) COMP.
           05  FINRSPBF PIC  X(0001).
           05  FILLER REDEFINES FINRSPBF.
               10  FINRSPBA PIC  X(0001).
           05  FINRSPBI PIC  X(0010).
      *    -------------------------------
           05  AGENTBL PIC S9(0004) COMP.
           05  AGENTBF PIC  X(0001).
           05  FILLER REDEFINES AGENTBF.
               10  AGENTBA PIC  X(0001).
           05  AGENTBI PIC  X(0010).
      *    -------------------------------
           05  AGTNAMBL PIC S9(0004) COMP.
           05  AGTNAMBF PIC  X(0001).
           05  FILLER REDEFINES AGTNAMBF.
               10  AGTNAMBA PIC  X(0001).
           05  AGTNAMBI PIC  X(0030).
      *    -------------------------------
           05  CARCL PIC S9(0004) COMP.
           05  CARCF PIC  X(0001).
           05  FILLER REDEFINES CARCF.
               10  CARCA PIC  X(0001).
           05  CARCI PIC  X(0001).
      *    -------------------------------
           05  GRPCL PIC S9(0004) COMP.
           05  GRPCF PIC  X(0001).
           05  FILLER REDEFINES GRPCF.
               10  GRPCA PIC  X(0001).
           05  GRPCI PIC  X(0006).
      *    -------------------------------
           05  FINRSPCL PIC S9(0004) COMP.
           05  FINRSPCF PIC  X(0001).
           05  FILLER REDEFINES FINRSPCF.
               10  FINRSPCA PIC  X(0001).
           05  FINRSPCI PIC  X(0010).
      *    -------------------------------
           05  AGENTCL PIC S9(0004) COMP.
           05  AGENTCF PIC  X(0001).
           05  FILLER REDEFINES AGENTCF.
               10  AGENTCA PIC  X(0001).
           05  AGENTCI PIC  X(0010).
      *    -------------------------------
           05  AGTNAMCL PIC S9(0004) COMP.
           05  AGTNAMCF PIC  X(0001).
           05  FILLER REDEFINES AGTNAMCF.
               10  AGTNAMCA PIC  X(0001).
           05  AGTNAMCI PIC  X(0030).
      *    -------------------------------
           05  CARDL PIC S9(0004) COMP.
           05  CARDF PIC  X(0001).
           05  FILLER REDEFINES CARDF.
               10  CARDA PIC  X(0001).
           05  CARDI PIC  X(0001).
      *    -------------------------------
           05  GRPDL PIC S9(0004) COMP.
           05  GRPDF PIC  X(0001).
           05  FILLER REDEFINES GRPDF.
               10  GRPDA PIC  X(0001).
           05  GRPDI PIC  X(0006).
      *    -------------------------------
           05  FINRSPDL PIC S9(0004) COMP.
           05  FINRSPDF PIC  X(0001).
           05  FILLER REDEFINES FINRSPDF.
               10  FINRSPDA PIC  X(0001).
           05  FINRSPDI PIC  X(0010).
      *    -------------------------------
           05  AGENTDL PIC S9(0004) COMP.
           05  AGENTDF PIC  X(0001).
           05  FILLER REDEFINES AGENTDF.
               10  AGENTDA PIC  X(0001).
           05  AGENTDI PIC  X(0010).
      *    -------------------------------
           05  AGTNAMDL PIC S9(0004) COMP.
           05  AGTNAMDF PIC  X(0001).
           05  FILLER REDEFINES AGTNAMDF.
               10  AGTNAMDA PIC  X(0001).
           05  AGTNAMDI PIC  X(0030).
      *    -------------------------------
           05  CAREL PIC S9(0004) COMP.
           05  CAREF PIC  X(0001).
           05  FILLER REDEFINES CAREF.
               10  CAREA PIC  X(0001).
           05  CAREI PIC  X(0001).
      *    -------------------------------
           05  GRPEL PIC S9(0004) COMP.
           05  GRPEF PIC  X(0001).
           05  FILLER REDEFINES GRPEF.
               10  GRPEA PIC  X(0001).
           05  GRPEI PIC  X(0006).
      *    -------------------------------
           05  FINRSPEL PIC S9(0004) COMP.
           05  FINRSPEF PIC  X(0001).
           05  FILLER REDEFINES FINRSPEF.
               10  FINRSPEA PIC  X(0001).
           05  FINRSPEI PIC  X(0010).
      *    -------------------------------
           05  AGENTEL PIC S9(0004) COMP.
           05  AGENTEF PIC  X(0001).
           05  FILLER REDEFINES AGENTEF.
               10  AGENTEA PIC  X(0001).
           05  AGENTEI PIC  X(0010).
      *    -------------------------------
           05  AGTNAMEL PIC S9(0004) COMP.
           05  AGTNAMEF PIC  X(0001).
           05  FILLER REDEFINES AGTNAMEF.
               10  AGTNAMEA PIC  X(0001).
           05  AGTNAMEI PIC  X(0030).
      *    -------------------------------
           05  CARFL PIC S9(0004) COMP.
           05  CARFF PIC  X(0001).
           05  FILLER REDEFINES CARFF.
               10  CARFA PIC  X(0001).
           05  CARFI PIC  X(0001).
      *    -------------------------------
           05  GRPFL PIC S9(0004) COMP.
           05  GRPFF PIC  X(0001).
           05  FILLER REDEFINES GRPFF.
               10  GRPFA PIC  X(0001).
           05  GRPFI PIC  X(0006).
      *    -------------------------------
           05  FINRSPFL PIC S9(0004) COMP.
           05  FINRSPFF PIC  X(0001).
           05  FILLER REDEFINES FINRSPFF.
               10  FINRSPFA PIC  X(0001).
           05  FINRSPFI PIC  X(0010).
      *    -------------------------------
           05  AGENTFL PIC S9(0004) COMP.
           05  AGENTFF PIC  X(0001).
           05  FILLER REDEFINES AGENTFF.
               10  AGENTFA PIC  X(0001).
           05  AGENTFI PIC  X(0010).
      *    -------------------------------
           05  AGTNAMFL PIC S9(0004) COMP.
           05  AGTNAMFF PIC  X(0001).
           05  FILLER REDEFINES AGTNAMFF.
               10  AGTNAMFA PIC  X(0001).
           05  AGTNAMFI PIC  X(0030).
      *    -------------------------------
           05  ERMESGL PIC S9(0004) COMP.
           05  ERMESGF PIC  X(0001).
           05  FILLER REDEFINES ERMESGF.
               10  ERMESGA PIC  X(0001).
           05  ERMESGI PIC  X(0079).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
       01  EL856AO REDEFINES EL856AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUMCODEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUMNAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSP1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAM1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSP2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAM2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSP3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAM3O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSP4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAM4O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSP5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAM5O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSP6O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT6O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAM6O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSP7O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT7O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAM7O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSP8O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT8O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAM8O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSP9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAM9O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARAO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPAO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSPAO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENTAO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAMAO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARBO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPBO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSPBO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENTBO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAMBO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARCO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPCO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSPCO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENTCO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAMCO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSPDO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENTDO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAMDO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAREO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSPEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENTEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPFO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSPFO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENTFO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGTNAMFO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERMESGO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
00161  01  DISPLAY-MAP REDEFINES EL856AI.
00162      12  FILLER                  PIC X(73).
00163      12  SM-CROSS-REFERENCE OCCURS 15 TIMES.
00164          16  SM-CAR-LEN          PIC S9(4)   COMP.
00165          16  SM-CAR-ATTRB        PIC X.
00166          16  SM-CAR              PIC X.
00167          16  SM-GRP-LEN          PIC S9(4)   COMP.
00168          16  SM-GRP-ATTRB        PIC X.
00169          16  SM-GRP              PIC X(6).
00170          16  SM-FIN-RESP-LEN     PIC S9(4)   COMP.
00171          16  SM-FIN-RESP-ATTRB   PIC X.
00172          16  SM-FIN-RESP         PIC X(10).
00173          16  SM-AGENT-LEN        PIC S9(4)   COMP.
00174          16  SM-AGENT-ATTRB      PIC X.
00175          16  SM-AGENT            PIC X(10).
00176          16  SM-AGENT-NAME-LEN   PIC S9(4)   COMP.
00177          16  SM-AGENT-NAME-ATTRB PIC X.
00178          16  SM-AGENT-NAME       PIC X(30).
00179
00180      EJECT
00181
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
00183  01  DFHCOMMAREA             PIC X(1024).
00184
00185      EJECT
00186
00187 *                            COPY ERCSUMM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCSUMM                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = AR SUMMARY CROSS REFERENCE                *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 150           RECFORM = FIXED                  *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERSUMM                   RKP=2,LEN=34    *
00015 *                                                                *
00016 *       ALTERNATE PATH1 = ERSUMM2  (BY CO SUMMARY CARR           *
00017 *                                      GROUP F.R. AGENT)         *
00018 *                                                 RKP=36 ,LEN=34 *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 ******************************************************************
00024
00025  01  SUMM-CROSS-REFERENCE.
00026      12  SX-RECORD-ID                PIC XX.
00027          88  VALID-SX-ID             VALUE 'SX'.
00028
00029      12  SX-CONTROL-PRIMARY.
00030          16  SX-COMPANY-CD           PIC X.
00031          16  SX-SUMMARY              PIC X(6).
00032          16  SX-CARRIER              PIC X.
00033          16  SX-GROUP                PIC X(6).
00034          16  SX-FIN-RESP             PIC X(10).
00035          16  SX-ACCT-AGENT           PIC X(10).
00036
00037      12  SX-CONTROL-A1.
00038          16  SX-COMPANY-A1           PIC X.
00039          16  SX-ACCT-AGENT-A1        PIC X(10).
00040          16  SX-SUMMARY-A1           PIC X(6).
00041          16  SX-CARR-A1              PIC X.
00042          16  SX-GROUP-A1             PIC X(6).
00043          16  SX-FIN-RESP-A1          PIC X(10).
00044
00045      12  SX-MAINT-INFORMATION.
00046          16  SX-LAST-MAINT-DT        PIC XX.
00047          16  SX-LAST-MAINT-BY        PIC X(4).
00048          16  SX-LAST-MAINT-HHMMSS    PIC S9(7)  COMP-3.
00049
00050      12  SX-SUMM-OR-AGT-NAME         PIC X(30).
00051
00052      12  FILLER                      PIC X(40).
00053
00054 ******************************************************************
00188      EJECT
00189
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                SUMM-CROSS-REFERENCE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL856' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00191
00192      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00193      MOVE 1                      TO EMI-NUMBER-OF-LINES.
00194
00195      IF EIBCALEN = 0
00196          GO TO 8800-UNAUTHORIZED-ACCESS.
00197
00198      MOVE EIBTRMID               TO QID-TERM.
00199      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00200      MOVE '5'                    TO DC-OPTION-CODE.
00201      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00202      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.
00203      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.
00204
00205      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00206          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00207              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00208              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00209              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00210              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00211              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00212              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00213              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00214              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00215          ELSE
00216              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00217              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00218              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00219              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00220              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00221              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00222              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00223              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00224
00225      MOVE LOW-VALUES             TO EL856AI.
00226
00227      
      * EXEC CICS HANDLE CONDITION
00228 *        PGMIDERR  (9600-PGMID-ERROR)
00229 *        ERROR     (9990-ABEND)
00230 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00001853' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031383533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00231
00232      IF EIBTRNID = TRANS-EXD4
00233         MOVE SPACES              TO PI-PROGRAM-WORK-AREA
00234         MOVE PI-AR-SUMMARY-CODE  TO PI-SUMMARY-CODE
00235         MOVE -1                      TO SUMNAMEL
00236         GO TO 3000-DISPLAY-SM-CROSS-REF.
00237
00238      IF EIBTRNID NOT = TRANS-EXJ8
00239         MOVE SPACES              TO PI-PROGRAM-WORK-AREA
00240         MOVE -1                      TO SUMCODEL
00241         GO TO 8100-SEND-INITIAL-MAP.
00242
00243      IF EIBAID = DFHCLEAR
00244          GO TO 9400-CLEAR.
00245
00246
00247      IF PI-PROCESSOR-ID = 'LGXX'
00248          GO TO 0200-RECEIVE.
00249
00250      
      * EXEC CICS READQ TS
00251 *        QUEUE  (QID)
00252 *        INTO   (SECURITY-CONTROL)
00253 *        LENGTH (SC-COMM-LENGTH)
00254 *        ITEM   (SC-ITEM)
00255 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00001876' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00256
00257      MOVE SC-CREDIT-DISPLAY (4)   TO PI-DISPLAY-CAP.
00258      MOVE SC-CREDIT-UPDATE  (4)   TO PI-MODIFY-CAP.
00259
00260      IF NOT DISPLAY-CAP
00261          MOVE 'READ'          TO SM-READ
00262          PERFORM 9995-SECURITY-VIOLATION
00263          MOVE ER-0070         TO  EMI-ERROR
00264          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00265          GO TO 8100-SEND-INITIAL-MAP.
00266
00267      EJECT
00268
00269 ******************************************************************
00270 *                                                                *
00271 *              R E C E I V E   M A P S                           *
00272 *                                                                *
00273 ******************************************************************
00274
00275  0200-RECEIVE.
00276
00277      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00278          MOVE ER-0008            TO EMI-ERROR
00279          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00280          MOVE -1                 TO PFENTERL
00281          GO TO 8200-SEND-DATAONLY.
00282
00283      
      * EXEC CICS RECEIVE
00284 *        MAP      (EL856A)
00285 *        MAPSET   (MAPSET-EL856S)
00286 *        INTO     (EL856AI)
00287 *    END-EXEC.
           MOVE LENGTH OF
            EL856AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001909' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL856A, 
                 EL856AI, 
                 DFHEIV11, 
                 MAPSET-EL856S, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00288
00289      IF PFENTERL = ZERO
00290          GO TO 0300-CHECK-PFKEYS.
00291
00292      IF EIBAID NOT = DFHENTER
00293          MOVE ER-0004          TO EMI-ERROR
00294          MOVE AL-UNBOF         TO PFENTERA
00295          MOVE -1               TO PFENTERL
00296          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00297          GO TO 8200-SEND-DATAONLY.
00298
00299      IF (PFENTERI NUMERIC)
00300          AND  (PFENTERI GREATER ZERO AND LESS THAN 25)
00301          MOVE PF-VALUES (PFENTERI)  TO  EIBAID
00302      ELSE
00303          MOVE ER-0029          TO EMI-ERROR
00304          MOVE AL-UNBOF         TO PFENTERA
00305          MOVE -1               TO PFENTERL
00306          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00307          GO TO 8200-SEND-DATAONLY.
00308
00309      EJECT
00310
00311 ******************************************************************
00312 *                                                                *
00313 *              C H E C K   P F K E Y S                           *
00314 *                                                                *
00315 ******************************************************************
00316
00317  0300-CHECK-PFKEYS.
00318
00319      IF EIBAID = DFHPF23
00320          GO TO 8810-PF23.
00321
00322      IF EIBAID = DFHPF24
00323          GO TO 9200-RETURN-MAIN-MENU.
00324
00325      IF EIBAID = DFHPF12
00326          GO TO 9500-PF12.
00327
00328      IF EIBAID = DFHENTER
00329          GO TO 1000-EDIT-MAP.
00330
00331      IF EIBAID = DFHPF1
00332         IF PI-END-OF-FILE
00333            MOVE ER-2237 TO EMI-ERROR
00334            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00335            GO TO 8200-SEND-DATAONLY
00336         ELSE
00337            GO TO 3000-DISPLAY-SM-CROSS-REF.
00338
00339      IF EIBAID = DFHPF2
00340         IF PI-TOP-OF-FILE
00341            MOVE ER-2238 TO EMI-ERROR
00342            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00343            GO TO 8200-SEND-DATAONLY
00344         ELSE
00345            PERFORM 4100-READ-PREV-SUMMARY-CODE THRU 4190-EXIT
00346            GO TO 3000-DISPLAY-SM-CROSS-REF.
00347
00348      IF EIBAID = DFHPF3
00349         IF PI-END-OF-FILE
00350            MOVE ER-2237 TO EMI-ERROR
00351            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00352            GO TO 8200-SEND-DATAONLY
00353         ELSE
00354            GO TO 3000-DISPLAY-SM-CROSS-REF.
00355
00356      IF EIBAID = DFHPF4
00357         IF PI-TOP-OF-FILE
00358            MOVE ER-2238 TO EMI-ERROR
00359            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00360            GO TO 8200-SEND-DATAONLY
00361         ELSE
00362            GO TO 3100-DISPLAY-PREV-SM-CROSS-REF.
00363
00364      MOVE ER-0008 TO EMI-ERROR.
00365      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00366      MOVE -1                     TO PFENTERL.
00367      GO TO 8200-SEND-DATAONLY.
00368
00369      EJECT
00370
00371 ******************************************************************
00372 *                                                                *
00373 *                  E D I T    M A P                              *
00374 *                                                                *
00375 ******************************************************************
00376
00377  1000-EDIT-MAP.
00378
00379
00380      IF SUMCODEL GREATER THAN ZEROS
00381         MOVE SUMCODEI            TO PI-SUMMARY-CODE
00382         PERFORM 4200-READ-SUM-FILE THRU 4290-EXIT
00383         MOVE AL-UANON            TO SUMCODEA
00384      ELSE
00385         MOVE ER-3135             TO EMI-ERROR
00386         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00387         MOVE -1                  TO SUMNAMEL
00388         GO TO 8200-SEND-DATAONLY.
00389
00390      IF SUMNAMEL GREATER THAN ZEROS
00391         PERFORM 2000-UPDATE-SUMMARY-NAME THRU 2090-EXIT.
00392
00393      GO TO 3000-DISPLAY-SM-CROSS-REF.
00394
00395      EJECT
00396
00397 ******************************************************************
00398 *                                                                *
00399 *        U P D A T E   S U M M A R Y   N A M E                   *
00400 *                                                                *
00401 ******************************************************************
00402
00403  2000-UPDATE-SUMMARY-NAME.
00404
00405      MOVE LOW-VALUES             TO ERSUMM-KEY.
00406      MOVE PI-COMPANY-CD          TO ERSUMM-COMPANY-CD.
00407      MOVE PI-SUMMARY-CODE        TO ERSUMM-SUMMARY.
00408
00409      
      * EXEC CICS HANDLE CONDITION
00410 *        NOTFND   (2080-SUMMARY-NOTFND)
00411 *    END-EXEC.
      *    MOVE '"$I                   ! # #00002035' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032303335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00412
00413      
      * EXEC CICS READ
00414 *        DATASET   (FILE-ID-ERSUMM)
00415 *        SET       (ADDRESS OF SUMM-CROSS-REFERENCE)
00416 *        RIDFLD    (ERSUMM-KEY)
00417 *        UPDATE
00418 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00002039' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00419
00420      MOVE 'B'                    TO JP-RECORD-TYPE.
00421      MOVE SUMM-CROSS-REFERENCE   TO JP-RECORD-AREA.
00422
00423      PERFORM 8400-LOG-JOURNAL-RECORD.
00424
00425      MOVE SUMNAMEI               TO SX-SUMM-OR-AGT-NAME.
00426
00427      MOVE 'C'                    TO JP-RECORD-TYPE.
00428      MOVE SUMM-CROSS-REFERENCE   TO JP-RECORD-AREA.
00429
00430      
      * EXEC CICS REWRITE
00431 *        DATASET (FILE-ID-ERSUMM)
00432 *        FROM    (SUMM-CROSS-REFERENCE)
00433 *    END-EXEC.
           MOVE LENGTH OF
            SUMM-CROSS-REFERENCE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002056' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 SUMM-CROSS-REFERENCE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00434
00435      PERFORM 8400-LOG-JOURNAL-RECORD.
00436
00437      GO TO 2090-EXIT.
00438
00439  2080-SUMMARY-NOTFND.
00440
00441      MOVE ER-3133                TO EMI-ERROR.
00442      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00443      MOVE -1                     TO SUMNAMEL.
00444      GO TO 8200-SEND-DATAONLY.
00445
00446  2090-EXIT.
00447      EXIT.
00448
00449      EJECT
00450
00451 ******************************************************************
00452 *                                                                *
00453 *  D I S P L A Y   S U M M A R Y   C R O S S   R E F E R E N C E *
00454 *                                                                *
00455 ******************************************************************
00456
00457  3000-DISPLAY-SM-CROSS-REF.
00458
00459      
      * EXEC CICS HANDLE CONDITION
00460 *        NOTFND  (3080-REQUEST-NOTFND)
00461 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00002085' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032303835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00462
00463      IF EIBAID = DFHPF3
00464         IF PI-LST-SUM-KEY GREATER THAN SPACES
00465            MOVE PI-LST-SUM-KEY   TO ERSUMM-KEY
00466            GO TO 3010-PROCESS-SM-CROSS-REF
00467         ELSE
00468            MOVE PI-1ST-SUM-KEY   TO ERSUMM-KEY.
00469
00470      IF EIBAID = DFHENTER
00471         MOVE LOW-VALUES       TO ERSUMM-KEY
00472         MOVE PI-COMPANY-CD    TO ERSUMM-COMPANY-CD
00473         MOVE PI-SUMMARY-CODE  TO ERSUMM-SUMMARY.
00474
00475      IF EIBAID = DFHPF1
00476         MOVE HIGH-VALUES      TO ERSUMM-KEY
00477         MOVE PI-COMPANY-CD    TO ERSUMM-COMPANY-CD
00478         MOVE PI-SUMMARY-CODE  TO ERSUMM-SUMMARY.
00479
00480  3010-PROCESS-SM-CROSS-REF.
00481
00482      
      * EXEC CICS STARTBR
00483 *        DATASET (FILE-ID-ERSUMM)
00484 *        RIDFLD  (ERSUMM-KEY)
00485 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002108' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00486
00487      MOVE SPACE                  TO PI-END-OF-FILE-SW
00488                                     PI-TOP-OF-FILE-SW.
00489
00490      MOVE +0                     TO WS-SUB1.
00491
00492  3020-READ-SUMMARY-FILE.
00493
00494      
      * EXEC CICS HANDLE CONDITION
00495 *        ENDFILE (3060-END-OF-FILE)
00496 *    END-EXEC.
      *    MOVE '"$''                   ! % #00002120' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303032313230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00497
00498      
      * EXEC CICS READNEXT
00499 *        SET     (ADDRESS OF SUMM-CROSS-REFERENCE)
00500 *        DATASET (FILE-ID-ERSUMM)
00501 *        RIDFLD  (ERSUMM-KEY)
00502 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00002124' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00503
00504      IF SX-COMPANY-CD NOT = PI-COMPANY-CD
00505         GO TO 3060-END-OF-FILE.
00506
00507      IF PI-LST-SUM-KEY = ERSUMM-KEY
00508         GO TO 3020-READ-SUMMARY-FILE.
00509
00510      IF WS-BROWSE-STARTED
00511         IF SX-SUMMARY NOT = PI-SUMMARY-CODE
00512            GO TO 3070-DISPLAY-PROCESSED.
00513
00514      IF SX-CARRIER = LOW-VALUES
00515         MOVE SX-SUMMARY          TO PI-SUMMARY-CODE
00516         MOVE 'Y'                 TO WS-BROWSE-STARTED-SW
00517         GO TO 3020-READ-SUMMARY-FILE.
00518
00519      IF WS-BROWSE-STARTED
00520         NEXT SENTENCE
00521      ELSE
00522         MOVE 'Y'                 TO WS-BROWSE-STARTED-SW
00523         MOVE SX-SUMMARY          TO PI-SUMMARY-CODE.
00524
00525      ADD +1                      TO WS-SUB1.
00526
00527      IF WS-SUB1 GREATER THAN +15
00528         GO TO 3070-DISPLAY-PROCESSED.
00529
00530      IF WS-SUB1 = +1
00531         MOVE PI-SUMMARY-CODE     TO SUMCODEO
00532         MOVE AL-UANON            TO SUMCODEA
00533         MOVE ERSUMM-KEY          TO PI-1ST-SUM-KEY.
00534
00535      IF WS-SUB1 = +15
00536         MOVE ERSUMM-KEY          TO PI-LST-SUM-KEY.
00537
00538      MOVE PI-SUMMARY-CODE        TO SUMCODEO.
00539      MOVE SX-CARRIER             TO SM-CAR        (WS-SUB1).
00540      MOVE SX-GROUP               TO SM-GRP        (WS-SUB1).
00541      MOVE SX-FIN-RESP            TO SM-FIN-RESP   (WS-SUB1).
00542      MOVE SX-ACCT-AGENT          TO SM-AGENT      (WS-SUB1).
00543      MOVE SX-SUMM-OR-AGT-NAME    TO SM-AGENT-NAME (WS-SUB1).
00544
00545      GO TO 3020-READ-SUMMARY-FILE.
00546
00547  3060-END-OF-FILE.
00548
00549      MOVE 'Y'                    TO PI-END-OF-FILE-SW.
00550      MOVE ER-2237                TO EMI-ERROR.
00551      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00552
00553  3070-DISPLAY-PROCESSED.
00554
00555      
      * EXEC CICS ENDBR
00556 *        DATASET (FILE-ID-ERSUMM)
00557 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002181' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00558
00559      MOVE PI-SUMMARY-CODE        TO SUMCODEO.
00560      PERFORM 4200-READ-SUM-FILE THRU 4290-EXIT.
00561      MOVE SX-SUMM-OR-AGT-NAME    TO SUMNAMEO.
00562
00563      IF EIBTRNID = TRANS-EXD4
00564         MOVE -1                  TO SUMNAMEL
00565      ELSE
00566         MOVE -1                  TO SUMCODEL.
00567
00568      GO TO 8100-SEND-INITIAL-MAP.
00569
00570  3080-REQUEST-NOTFND.
00571
00572      MOVE ER-3133                TO EMI-ERROR.
00573      MOVE -1                     TO SUMCODEL.
00574      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00575      GO TO 8200-SEND-DATAONLY.
00576
00577  3090-EXIT.
00578       EXIT.
00579
00580      EJECT
00581
00582 ******************************************************************
00583 *                                                                *
00584 *     D I S P L A Y   P R E V.   C R O S S   R E F E R E N C E   *
00585 *                                                                *
00586 ******************************************************************
00587
00588  3100-DISPLAY-PREV-SM-CROSS-REF.
00589
00590      
      * EXEC CICS HANDLE CONDITION
00591 *        NOTFND  (3180-REQUEST-NOTFND)
00592 *    END-EXEC.
      *    MOVE '"$I                   ! & #00002216' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303032323136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00593
00594      MOVE PI-1ST-SUM-KEY        TO ERSUMM-KEY.
00595
00596      
      * EXEC CICS STARTBR
00597 *        DATASET (FILE-ID-ERSUMM)
00598 *        RIDFLD  (ERSUMM-KEY)
00599 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002222' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00600
00601      MOVE SPACE                  TO PI-END-OF-FILE-SW
00602                                     PI-TOP-OF-FILE-SW.
00603
00604      MOVE +16                    TO WS-SUB1.
00605
00606  3110-READ-SUMMARY-FILE.
00607
00608      
      * EXEC CICS HANDLE CONDITION
00609 *        ENDFILE (3160-END-OF-FILE)
00610 *    END-EXEC.
      *    MOVE '"$''                   ! '' #00002234' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303032323334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00611
00612      
      * EXEC CICS READPREV
00613 *        SET     (ADDRESS OF SUMM-CROSS-REFERENCE)
00614 *        DATASET (FILE-ID-ERSUMM)
00615 *        RIDFLD  (ERSUMM-KEY)
00616 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00002238' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00617
00618      IF SX-COMPANY-CD NOT = PI-COMPANY-CD
00619         GO TO 3160-END-OF-FILE.
00620
00621      IF SX-SUMMARY LESS THAN PI-SUMMARY-CODE
00622         GO TO 3170-DISPLAY-PROCESSED.
00623
00624      IF SX-CARRIER = LOW-VALUES
00625         GO TO 3170-DISPLAY-PROCESSED.
00626
00627      IF WS-BROWSE-STARTED
00628         NEXT SENTENCE
00629      ELSE
00630         MOVE 'Y'                 TO WS-BROWSE-STARTED-SW
00631         MOVE ERSUMM-SUMMARY      TO PI-SUMMARY-CODE.
00632
00633      IF PI-1ST-SUM-KEY = ERSUMM-KEY
00634         GO TO 3110-READ-SUMMARY-FILE.
00635
00636      IF WS-SUB1 = +1
00637         MOVE ERSUMM-KEY          TO PI-1ST-SUM-KEY.
00638
00639      IF WS-SUB1 = +15
00640         MOVE ERSUMM-KEY          TO PI-LST-SUM-KEY.
00641
00642      SUBTRACT +1                 FROM WS-SUB1.
00643
00644      IF WS-SUB1 LESS THAN +1
00645         GO TO 3170-DISPLAY-PROCESSED.
00646
00647      MOVE PI-SUMMARY-CODE        TO SUMCODEO.
00648      MOVE SX-CARRIER             TO SM-CAR        (WS-SUB1).
00649      MOVE SX-GROUP               TO SM-GRP        (WS-SUB1).
00650      MOVE SX-FIN-RESP            TO SM-FIN-RESP   (WS-SUB1).
00651      MOVE SX-ACCT-AGENT          TO SM-AGENT      (WS-SUB1).
00652      MOVE SX-SUMM-OR-AGT-NAME    TO SM-AGENT-NAME (WS-SUB1).
00653
00654      GO TO 3110-READ-SUMMARY-FILE.
00655
00656  3160-END-OF-FILE.
00657
00658      MOVE 'Y'                    TO PI-TOP-OF-FILE-SW.
00659      MOVE ER-2238                TO EMI-ERROR.
00660      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00661
00662  3170-DISPLAY-PROCESSED.
00663
00664      
      * EXEC CICS ENDBR
00665 *        DATASET (FILE-ID-ERSUMM)
00666 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002290' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00667
00668      PERFORM 4200-READ-SUM-FILE THRU 4290-EXIT.
00669      MOVE SX-SUMM-OR-AGT-NAME TO SUMNAMEO.
00670
00671      MOVE -1                     TO SUMCODEL.
00672
00673      GO TO 8100-SEND-INITIAL-MAP.
00674
00675  3180-REQUEST-NOTFND.
00676
00677      MOVE ERSUMM-SUMMARY         TO SUMCODEO.
00678      MOVE AL-UANON               TO SUMCODEA.
00679      MOVE ER-2132                TO EMI-ERROR.
00680      MOVE -1                     TO SUMCODEL.
00681      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00682      GO TO 8200-SEND-DATAONLY.
00683
00684  3190-EXIT.
00685       EXIT.
00686      EJECT
00687
00688 ******************************************************************
00689 *                                                                *
00690 *         R E A D   P R E V   S U M M A R Y   C O D E            *
00691 *                                                                *
00692 ******************************************************************
00693
00694  4100-READ-PREV-SUMMARY-CODE.
00695
00696      MOVE LOW-VALUES             TO ERSUMM-KEY.
00697      MOVE PI-COMPANY-CD          TO ERSUMM-COMPANY-CD.
00698      MOVE PI-SUMMARY-CODE        TO ERSUMM-SUMMARY.
00699
00700      
      * EXEC CICS HANDLE CONDITION
00701 *        NOTFND  (4180-SUMMARY-NOTFND)
00702 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00002326' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303032333236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00703
00704      MOVE SPACE                  TO PI-END-OF-FILE-SW
00705                                     PI-TOP-OF-FILE-SW.
00706
00707      MOVE SPACE                  TO WS-BROWSE-STARTED-SW.
00708
00709      
      * EXEC CICS STARTBR
00710 *        DATASET (FILE-ID-ERSUMM)
00711 *        RIDFLD  (ERSUMM-KEY)
00712 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002335' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00713
00714      
      * EXEC CICS HANDLE CONDITION
00715 *        ENDFILE (4160-END-OF-FILE)
00716 *    END-EXEC.
      *    MOVE '"$''                   ! ) #00002340' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303032333430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00717
00718  4110-READ-SUMMARY-FILE.
00719
00720      
      * EXEC CICS READPREV
00721 *        SET     (ADDRESS OF SUMM-CROSS-REFERENCE)
00722 *        DATASET (FILE-ID-ERSUMM)
00723 *        RIDFLD  (ERSUMM-KEY)
00724 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00002346' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00725
00726      IF SX-COMPANY-CD NOT = PI-COMPANY-CD
00727         GO TO 4160-END-OF-FILE.
00728
00729      IF WS-BROWSE-STARTED
00730         NEXT SENTENCE
00731      ELSE
00732         MOVE 'Y'                 TO WS-BROWSE-STARTED-SW
00733         GO TO 4110-READ-SUMMARY-FILE.
00734
00735      
      * EXEC CICS ENDBR
00736 *        DATASET (FILE-ID-ERSUMM)
00737 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002361' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00738
00739      MOVE ERSUMM-SUMMARY         TO PI-SUMMARY-CODE.
00740
00741      MOVE LOW-VALUES             TO ERSUMM-KEY.
00742      MOVE PI-COMPANY-CD          TO ERSUMM-COMPANY-CD.
00743      MOVE PI-SUMMARY-CODE        TO ERSUMM-SUMMARY.
00744
00745      
      * EXEC CICS READ
00746 *        SET     (ADDRESS OF SUMM-CROSS-REFERENCE)
00747 *        DATASET (FILE-ID-ERSUMM)
00748 *        RIDFLD  (ERSUMM-KEY)
00749 *    END-EXEC.
      *    MOVE '&"S        E          (   #00002371' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00750
00751      GO TO 4190-EXIT.
00752
00753  4160-END-OF-FILE.
00754
00755      
      * EXEC CICS ENDBR
00756 *        DATASET (FILE-ID-ERSUMM)
00757 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002381' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00758
00759      MOVE 'Y'                    TO PI-TOP-OF-FILE-SW.
00760      MOVE ER-2238                TO EMI-ERROR.
00761      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00762      MOVE -1                    TO PFENTERL.
00763      GO TO 8200-SEND-DATAONLY.
00764
00765  4180-SUMMARY-NOTFND.
00766
00767      MOVE ERSUMM-SUMMARY        TO SUMCODEO.
00768      MOVE AL-UANON              TO SUMCODEA.
00769      MOVE ER-3133               TO EMI-ERROR.
00770      MOVE -1                    TO PFENTERL.
00771      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00772      GO TO 8200-SEND-DATAONLY.
00773
00774  4190-EXIT.
00775      EXIT.
00776
00777      EJECT
00778
00779 ******************************************************************
00780 *                                                                *
00781 *             R E A D   S U M M A R Y   H E A D E R              *
00782 *                                                                *
00783 ******************************************************************
00784
00785
00786  4200-READ-SUM-FILE.
00787
00788      MOVE LOW-VALUES             TO ERSUMM-KEY.
00789      MOVE PI-COMPANY-CD          TO ERSUMM-COMPANY-CD.
00790      MOVE PI-SUMMARY-CODE        TO ERSUMM-SUMMARY.
00791
00792      
      * EXEC CICS HANDLE CONDITION
00793 *        NOTFND   (4280-SUMMARY-NOTFND)
00794 *    END-EXEC.
      *    MOVE '"$I                   ! * #00002418' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303032343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00795
00796      
      * EXEC CICS READ
00797 *        DATASET   (FILE-ID-ERSUMM)
00798 *        SET       (ADDRESS OF SUMM-CROSS-REFERENCE)
00799 *        RIDFLD    (ERSUMM-KEY)
00800 *    END-EXEC.
      *    MOVE '&"S        E          (   #00002422' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00801
00802      GO TO 4290-EXIT.
00803
00804  4280-SUMMARY-NOTFND.
00805
00806      MOVE -1                     TO SUMCODEL.
00807      MOVE ER-3133                TO EMI-ERROR.
00808      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00809      GO TO 8200-SEND-DATAONLY.
00810
00811  4290-EXIT.
00812      EXIT.
00813
00814      EJECT
00815
00816 ******************************************************************
00817 *                                                                *
00818 *            S  E N D    I N I T I A L   M A P                   *
00819 *                                                                *
00820 ******************************************************************
00821
00822  8100-SEND-INITIAL-MAP.
00823
00824      MOVE EIBTIME                    TO TIME-IN.
00825
00826      MOVE EMI-MESSAGE-AREA (1)       TO ERMESGO.
00827      MOVE WS-CURRENT-DT              TO DATEO.
00828      MOVE TIME-OUT                   TO TIMEO.
00829
00830
00831      
      * EXEC CICS SEND
00832 *        MAP      (EL856A)
00833 *        MAPSET   (MAPSET-EL856S)
00834 *        FROM     (EL856AI)
00835 *        ERASE
00836 *        CURSOR
00837 *    END-EXEC.
           MOVE LENGTH OF
            EL856AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002457' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL856A, 
                 EL856AI, 
                 DFHEIV12, 
                 MAPSET-EL856S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00838
00839      GO TO 9100-RETURN-TRAN.
00840
00841      EJECT
00842
00843 ******************************************************************
00844 *                                                                *
00845 *              S E N D    D A T A O N L Y                        *
00846 *                                                                *
00847 ******************************************************************
00848
00849  8200-SEND-DATAONLY.
00850
00851      MOVE EIBTIME                TO TIME-IN.
00852
00853      MOVE EMI-MESSAGE-AREA (1)       TO ERMESGO.
00854      MOVE WS-CURRENT-DT              TO DATEO.
00855      MOVE TIME-OUT                   TO TIMEO.
00856      MOVE -1                         TO SUMCODEL.
00857
00858      
      * EXEC CICS SEND
00859 *         MAP      (EL856A)
00860 *         MAPSET   (MAPSET-EL856S)
00861 *         FROM     (EL856AI)
00862 *         DATAONLY
00863 *         CURSOR
00864 *    END-EXEC.
           MOVE LENGTH OF
            EL856AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002484' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL856A, 
                 EL856AI, 
                 DFHEIV12, 
                 MAPSET-EL856S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00865
00866      GO TO 9100-RETURN-TRAN.
00867
00868      EJECT
00869
00870  8300-SEND-TEXT.
00871      
      * EXEC CICS SEND TEXT
00872 *        FROM     (LOGOFF-TEXT)
00873 *        LENGTH   (LOGOFF-LENGTH)
00874 *        ERASE
00875 *        FREEKB
00876 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002497' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343937' TO DFHEIV0(25:11)
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
           
00877
00878      
      * EXEC CICS RETURN
00879 *    END-EXEC.
      *    MOVE '.(                    &   #00002504' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00880
00881
00882  8400-LOG-JOURNAL-RECORD.
00883      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
00884      MOVE THIS-PGM                TO JP-PROGRAM-ID.
00885
00886 *    EXEC CICS JOURNAL
00887 *        JFILEID     (PI-JOURNAL-FILE-ID)
00888 *        JTYPEID     ('EL')
00889 *        FROM        (JOURNAL-RECORD)
00890 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
00891 *        END-EXEC.
00892
00893  8500-DATE-CONVERT.
00894      
      * EXEC CICS LINK
00895 *        PROGRAM  (LINK-ELDATCV)
00896 *        COMMAREA (DATE-CONVERSION-DATA)
00897 *        LENGTH   (DC-COMM-LENGTH)
00898 *    END-EXEC.
      *    MOVE '."C                   ''   #00002520' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00899
00900  8500-EXIT.
00901      EXIT.
00902
00903      EJECT
00904
00905  8800-UNAUTHORIZED-ACCESS.
00906      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
00907      GO TO 8300-SEND-TEXT.
00908
00909  8810-PF23.
00910      MOVE EIBAID                 TO PI-ENTRY-CD-1.
00911      MOVE XCTL-EL005             TO PGM-NAME.
00912      GO TO 9300-XCTL.
00913
00914  9200-RETURN-MAIN-MENU.
00915      MOVE XCTL-EL626             TO PGM-NAME.
00916      GO TO 9300-XCTL.
00917
00918  9000-RETURN-CICS.
00919      
      * EXEC CICS RETURN
00920 *    END-EXEC.
      *    MOVE '.(                    &   #00002545' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00921
00922  9100-RETURN-TRAN.
00923
00924      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
00925      MOVE '852A'                 TO PI-CURRENT-SCREEN-NO.
00926
00927      
      * EXEC CICS RETURN
00928 *        TRANSID    (TRANS-EXJ8)
00929 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
00930 *        LENGTH     (PI-COMM-LENGTH)
00931 *    END-EXEC.
      *    MOVE '.(CT                  &   #00002553' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-EXJ8, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00932
00933  9300-XCTL.
00934      
      * EXEC CICS XCTL
00935 *        PROGRAM    (PGM-NAME)
00936 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
00937 *        LENGTH     (PI-COMM-LENGTH)
00938 *    END-EXEC.
      *    MOVE '.$C                   $   #00002560' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00939
00940  9400-CLEAR.
00941      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME
00942      GO TO 9300-XCTL.
00943
00944  9500-PF12.
00945      MOVE XCTL-EL010             TO PGM-NAME.
00946      GO TO 9300-XCTL.
00947
00948  9600-PGMID-ERROR.
00949
00950      
      * EXEC CICS HANDLE CONDITION
00951 *        PGMIDERR    (8300-SEND-TEXT)
00952 *    END-EXEC.
      *    MOVE '"$L                   ! + #00002576' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303032353736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00953
00954      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
00955      MOVE ' '                    TO PI-ENTRY-CD-1.
00956      MOVE XCTL-EL005             TO PGM-NAME.
00957      MOVE PGM-NAME               TO LOGOFF-PGM.
00958      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
00959      GO TO 9300-XCTL.
00960
00961  9900-ERROR-FORMAT.
00962
00963      IF NOT EMI-ERRORS-COMPLETE
00964          MOVE LINK-EL001         TO PGM-NAME
00965          
      * EXEC CICS LINK
00966 *            PROGRAM    (PGM-NAME)
00967 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
00968 *            LENGTH     (EMI-COMM-LENGTH)
00969 *        END-EXEC.
      *    MOVE '."C                   ''   #00002591' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00970
00971  9900-EXIT.
00972      EXIT.
00973
00974  9990-ABEND.
00975      MOVE LINK-EL004             TO PGM-NAME.
00976      MOVE DFHEIBLK               TO EMI-LINE1.
00977      
      * EXEC CICS LINK
00978 *        PROGRAM   (PGM-NAME)
00979 *        COMMAREA  (EMI-LINE1)
00980 *        LENGTH    (72)
00981 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002603' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00982
00983      MOVE -1                     TO PFENTERL.
00984
00985      GO TO 8200-SEND-DATAONLY.
00986
00987      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL856' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00988
00989      EJECT
00990
00991  9995-SECURITY-VIOLATION.
00992 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00002635' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363335' TO DFHEIV0(25:11)
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
00993
00994  9995-EXIT.
00995      EXIT.
00996

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL856' TO DFHEIV1
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
               GO TO 2080-SUMMARY-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3080-REQUEST-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3060-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3180-REQUEST-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3160-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4180-SUMMARY-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 4160-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 4280-SUMMARY-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL856' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
