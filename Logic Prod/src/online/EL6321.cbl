00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL6321.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 10:33:35.
00007 *                            VMOD=2.002
00008 *
00008 *
00009 *AUTHOR.        LOGIC,INC.
00010 *               DALLAS, TEXAS.
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
00025 *REMARKS.
00026 *        TRANSACTION - EXB5 - PENDING CLAIMS (COMPANY TOTALS).
00027
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  EJECT
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*    EL6321 WORKING STORAGE    *'.
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.002 *********'.
00035
00036 *                            COPY ELCSCTM.
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
00037 *                            COPY ELCSCRTY.
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
00039     EJECT
00040
00041  01  STANDARD-AREAS.
00042      12  MAP-NAME            PIC  X(8)      VALUE 'EL6321A'.
00043      12  MAPSET-NAME         PIC  X(8)      VALUE 'EL6321S'.
00044      12  SCREEN-NUMBER       PIC  X(4)      VALUE '632C'.
00045      12  TRANS-ID            PIC  X(4)      VALUE 'EXB5'.
00046      12  THIS-PGM            PIC  X(8)      VALUE 'EL6321'.
00047      12  PGM-NAME            PIC  X(8).
00048      12  TIME-IN             PIC S9(7).
00049      12  TIME-OUT-R  REDEFINES TIME-IN.
00050          16  FILLER          PIC  X.
00051          16  TIME-OUT        PIC  99V99.
00052          16  FILLER          PIC  XX.
00053      12  XCTL-005            PIC  X(8)      VALUE 'EL005'.
00054      12  XCTL-010            PIC  X(8)      VALUE 'EL010'.
00055      12  XCTL-626            PIC  X(8)      VALUE 'EL626'.
00056      12  XCTL-630            PIC  X(8)      VALUE 'EL630'.
00057      12  XCTL-6301           PIC  X(8)      VALUE 'EL6301'.
00058      12  LINK-001            PIC  X(8)      VALUE 'EL001'.
00059      12  LINK-004            PIC  X(8)      VALUE 'EL004'.
00060      12  LINK-CLDATCV        PIC  X(8)      VALUE 'ELDATCV'.
00061      12  ERPNDC-FILE-ID      PIC  X(8)      VALUE 'ERPNDC'.
00062      12  WS-CURRENT-DT       PIC  X(8)      VALUE SPACES.
00063      12  ER-0029             PIC  X(4)      VALUE '0029'.
00064
00065  01  ACCESS-KEYS.
00066      12  ERPNDC-KEY.
00067          16  PNDC-COMPANY-CD     PIC  X      VALUE SPACE.
00068          16  PNDC-CARRIER        PIC  X      VALUE SPACE.
00069          16  PNDC-GROUPING       PIC  X(6)   VALUE SPACES.
00070          16  PNDC-STATE          PIC  XX     VALUE SPACES.
00071          16  PNDC-ACCOUNT        PIC  X(10)  VALUE SPACES.
00072          16  PNDC-CERT-EFF-DT    PIC  X(2)   VALUE SPACES.
00073          16  PNDC-CERT-NO.
00074              20  PNDC-CERT-PRIME PIC  X(10)  VALUE SPACES.
00075              20  PNDC-CERT-SFX   PIC  X      VALUE SPACE.
00076          16  PNDC-CLAIM-NO       PIC  X(7)   VALUE SPACES.
00077          16  PNDC-CHECK-NO       PIC  X(7)   VALUE SPACES.
00078          16  PNDC-RECORD-TYPE    PIC  X      VALUE '1'.
00079          16  PNDC-RECORD-SEQ     PIC S9(4)   VALUE +0   COMP.
00080  EJECT
00081  01  COMPANY-TOTALS      COMP-3.
00082      12  LF-PYMNT-GOOD       PIC S9(5)         VALUE ZEROS.
00083      12  LF-PYMNT-BAD        PIC S9(5)         VALUE ZEROS.
00084      12  LF-PYMNT-AMT        PIC S9(7)V99      VALUE ZEROS.
00085      12  AH-PYMNT-GOOD       PIC S9(5)         VALUE ZEROS.
00086      12  AH-PYMNT-BAD        PIC S9(5)         VALUE ZEROS.
00087      12  AH-PYMNT-AMT        PIC S9(7)V99      VALUE ZEROS.
00088      12  RSV-GOOD            PIC S9(5)         VALUE ZEROS.
00089      12  RSV-BAD             PIC S9(5)         VALUE ZEROS.
00090      12  RSV-FUTURE          PIC S9(6)V99      VALUE ZEROS.
00091      12  RSV-PTC             PIC S9(6)V99      VALUE ZEROS.
00092      12  RSV-IBNR            PIC S9(6)V99      VALUE ZEROS.
00093  EJECT
00094 *                                    COPY ELCDATE.
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
00095  EJECT
00096 *                                    COPY ELCLOGOF.
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
00097  EJECT
00098 *                                    COPY ELCATTR.
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
00099  EJECT
00100 *                                    COPY ELCEMIB.
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
00101  EJECT
00102 *                                    COPY ELCINTF.
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
00103      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00104          16  PI-ACC-NAME                 PIC  X(30).
00105          16  PI-MAP-NAME                 PIC  X(8).
00106          16  PI-BATCH-AMOUNTS    COMP-3.
00107              20  PI-LF-ISS-REMITTED      PIC S9(8)V99.
00108              20  PI-LF-ISS-ENTERED       PIC S9(8)V99.
00109              20  PI-LF-CAN-REMITTED      PIC S9(8)V99.
00110              20  PI-LF-CAN-ENTERED       PIC S9(8)V99.
00111              20  PI-AH-ISS-REMITTED      PIC S9(8)V99.
00112              20  PI-AH-ISS-ENTERED       PIC S9(8)V99.
00113              20  PI-AH-CAN-REMITTED      PIC S9(8)V99.
00114              20  PI-AH-CAN-ENTERED       PIC S9(8)V99.
00115              20  PI-ISS-CNT-REMITTED     PIC S9(5).
00116              20  PI-ISS-CNT-ENTERED      PIC S9(5).
00117              20  PI-CAN-CNT-REMITTED     PIC S9(5).
00118              20  PI-CAN-CNT-ENTERED      PIC S9(5).
00119          16  PI-MAINT-FUNC               PIC  X.
00120          16  PI-ERROR-SW                 PIC  X.
00121              88  PI-DATA-ERRORS              VALUE 'Y'.
00122          16  PI-UPDATE-SW                PIC  X.
00123              88  PI-DATA-UPDATED             VALUE 'Y'.
00124          16  PI-DISPLAY-SW               PIC  X.
00125              88  PI-LAST-FUNC-DISPLAY        VALUE 'Y'.
00126          16  PI-TABS-RESET-SW            PIC  X.
00127              88  PI-TABS-RESET               VALUE 'Y'.
00128          16  PI-SAVE-CALLING-PGM         PIC  X(8).
00129          16  PI-LAST-SEQ-NO-ADDED        PIC S9(4)      COMP.
00130          16  PI-NEXT-DISPLAY-SEQ-NO      PIC S9(4)      COMP.
00131          16  PI-SAV-CARRIER              PIC  X.
00132          16  PI-SAV-GROUPING             PIC  X(6).
00133          16  PI-SAV-STATE                PIC  XX.
00134          16  PI-SAV-ACCOUNT              PIC  X(10).
00135          16  PI-SAV-CERT-EFF-DT          PIC  XX.
00136          16  PI-SAV-CERT-NO.
00137              20  PI-SAV-CERT-PRIME       PIC  X(10).
00138              20  PI-SAV-CERT-SFX         PIC  X.
00139          16  PI-SAV-ENDING-PNDB-KEY.
00140              20  PI-SAV-COMP-CD          PIC  X.
00141              20  PI-SAV-ENTRY-BATCH      PIC  X(6).
00142              20  PI-SAV-BATCH-SEQ        PIC S9(4)     COMP.
00143              20  PI-SAV-BATCH-CHG-SEQ    PIC S9(4)     COMP.
00144          16  PI-VERIFY-DELETE-SW         PIC  X.
00145              88  PI-DELETE-IS-OK             VALUE 'Y'.
00146          16  PI-BATCH-EOF-SW             PIC  X.
00147              88  PI-BATCH-EOF                VALUE 'Y'.
00148          16  FILLER                      PIC  X(480).
00149  EJECT
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
00151                              PIC  X(503).
00152  EJECT
00153 *                            COPY ELCAID.
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
00154  01  FILLER  REDEFINES  DFHAID.
00155      12  FILLER              PIC  X(8).
00156      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.
00157  EJECT
00158 *                            COPY EL6321S.
       01  EL6321AI.
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
           05  CTYPE1L PIC S9(0004) COMP.
           05  CTYPE1F PIC  X(0001).
           05  FILLER REDEFINES CTYPE1F.
               10  CTYPE1A PIC  X(0001).
           05  CTYPE1I PIC  X(0006).
      *    -------------------------------
           05  LFTOTL PIC S9(0004) COMP.
           05  LFTOTF PIC  X(0001).
           05  FILLER REDEFINES LFTOTF.
               10  LFTOTA PIC  X(0001).
           05  LFTOTI PIC  X(0004).
      *    -------------------------------
           05  LFBADL PIC S9(0004) COMP.
           05  LFBADF PIC  X(0001).
           05  FILLER REDEFINES LFBADF.
               10  LFBADA PIC  X(0001).
           05  LFBADI PIC  X(0004).
      *    -------------------------------
           05  LFGOODL PIC S9(0004) COMP.
           05  LFGOODF PIC  X(0001).
           05  FILLER REDEFINES LFGOODF.
               10  LFGOODA PIC  X(0001).
           05  LFGOODI PIC  X(0004).
      *    -------------------------------
           05  LFPMTSL PIC S9(0004) COMP.
           05  LFPMTSF PIC  X(0001).
           05  FILLER REDEFINES LFPMTSF.
               10  LFPMTSA PIC  X(0001).
           05  LFPMTSI PIC  X(0010).
      *    -------------------------------
           05  CTYPE2L PIC S9(0004) COMP.
           05  CTYPE2F PIC  X(0001).
           05  FILLER REDEFINES CTYPE2F.
               10  CTYPE2A PIC  X(0001).
           05  CTYPE2I PIC  X(0006).
      *    -------------------------------
           05  AHTOTL PIC S9(0004) COMP.
           05  AHTOTF PIC  X(0001).
           05  FILLER REDEFINES AHTOTF.
               10  AHTOTA PIC  X(0001).
           05  AHTOTI PIC  X(0004).
      *    -------------------------------
           05  AHBADL PIC S9(0004) COMP.
           05  AHBADF PIC  X(0001).
           05  FILLER REDEFINES AHBADF.
               10  AHBADA PIC  X(0001).
           05  AHBADI PIC  X(0004).
      *    -------------------------------
           05  AHGOODL PIC S9(0004) COMP.
           05  AHGOODF PIC  X(0001).
           05  FILLER REDEFINES AHGOODF.
               10  AHGOODA PIC  X(0001).
           05  AHGOODI PIC  X(0004).
      *    -------------------------------
           05  AHPMTSL PIC S9(0004) COMP.
           05  AHPMTSF PIC  X(0001).
           05  FILLER REDEFINES AHPMTSF.
               10  AHPMTSA PIC  X(0001).
           05  AHPMTSI PIC  X(0010).
      *    -------------------------------
           05  RETOTL PIC S9(0004) COMP.
           05  RETOTF PIC  X(0001).
           05  FILLER REDEFINES RETOTF.
               10  RETOTA PIC  X(0001).
           05  RETOTI PIC  X(0005).
      *    -------------------------------
           05  REBADL PIC S9(0004) COMP.
           05  REBADF PIC  X(0001).
           05  FILLER REDEFINES REBADF.
               10  REBADA PIC  X(0001).
           05  REBADI PIC  X(0005).
      *    -------------------------------
           05  REGOODL PIC S9(0004) COMP.
           05  REGOODF PIC  X(0001).
           05  FILLER REDEFINES REGOODF.
               10  REGOODA PIC  X(0001).
           05  REGOODI PIC  X(0005).
      *    -------------------------------
           05  FUTUREL PIC S9(0004) COMP.
           05  FUTUREF PIC  X(0001).
           05  FILLER REDEFINES FUTUREF.
               10  FUTUREA PIC  X(0001).
           05  FUTUREI PIC  X(0009).
      *    -------------------------------
           05  PTCL PIC S9(0004) COMP.
           05  PTCF PIC  X(0001).
           05  FILLER REDEFINES PTCF.
               10  PTCA PIC  X(0001).
           05  PTCI PIC  X(0009).
      *    -------------------------------
           05  IBNRL PIC S9(0004) COMP.
           05  IBNRF PIC  X(0001).
           05  FILLER REDEFINES IBNRF.
               10  IBNRA PIC  X(0001).
           05  IBNRI PIC  X(0009).
      *    -------------------------------
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  ERRMSGI PIC  X(0076).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
       01  EL6321AO REDEFINES EL6321AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTYPE1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTOTO PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFBADO PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFGOODO PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPMTSO PIC  ZZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTYPE2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTOTO PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHBADO PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHGOODO PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPMTSO PIC  ZZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETOTO PIC  ZZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REBADO PIC  ZZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REGOODO PIC  ZZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FUTUREO PIC  ZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PTCO PIC  ZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IBNRO PIC  ZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
00159  EJECT
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
00161  01  DFHCOMMAREA             PIC  X(1024).
00162  EJECT
00163
00164 *                    COPY ERCPNDC.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDC                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING CLAIM TRANSACTIONS                *
00008 *                      PAYMENTS, RESERVES, EXPENSES              *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 500  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERPNDC                         RKP=2,LEN=50   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
00019
00020  01  PENDING-CLAIMS.
00021      12  PC-RECORD-ID                     PIC XX.
00022          88  VALID-PC-ID                      VALUE 'PC'.
00023
00024      12  PC-CONTROL-PRIMARY.
00025          16  PC-COMPANY-CD                PIC X.
00026          16  PC-CARRIER                   PIC X.
00027          16  PC-GROUPING.
00028              20  PC-GROUPING-PREFIX       PIC XXX.
00029              20  PC-GROUPING-PRIME        PIC XXX.
00030          16  PC-STATE                     PIC XX.
00031          16  PC-ACCOUNT.
00032              20  PC-ACCOUNT-PREFIX        PIC X(4).
00033              20  PC-ACCOUNT-PRIME         PIC X(6).
00034          16  PC-CERT-EFF-DT               PIC XX.
00035          16  PC-CERT-NO.
00036              20  PC-CERT-PRIME            PIC X(10).
00037              20  PC-CERT-SFX              PIC X.
00038          16  PC-CLAIM-NO                  PIC X(7).
00039          16  PC-CHECK-NO                  PIC X(7).
00040
00041          16  PC-RECORD-TYPE               PIC X.
00042              88  PC-CLAIMS                    VALUE '1'.
00043              88  PC-RESERVES                  VALUE '2'.
00044          16  PC-RECORD-SEQUENCE           PIC S9(4)     COMP.
00045
00046      12  PC-LAST-MAINT-DT                 PIC XX.
00047      12  PC-LAST-MAINT-BY                 PIC X(4).
00048      12  PC-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00049
00050      12  PC-CLAIM-RECORD.
00051          16  PC-CLAIM-TYPE                PIC X.
00052              88  PC-LF-CLAIM                  VALUE '1'.
00053              88  PC-AH-CLAIM                  VALUE '2'.
00054              88  PC-OB-LF-CLAIM               VALUE '3'.
00055              88  PC-OB-AH-CLAIM               VALUE '4'.
00056          16  PC-PAYMENT-DT                PIC XX.
00057          16  PC-PAID-THRU-DT              PIC XX.
00058          16  PC-REPORTED-DT               PIC XX.
00059          16  PC-INCURRED-DT               PIC XX.
00060          16  PC-NO-OF-DAYS-PAID           PIC S9(3)     COMP-3.
00061          16  PC-CLAIM-PAYMENT             PIC S9(7)V99  COMP-3.
00062          16  PC-AGE-AT-CLAIM              PIC 99.
00063          16  FILLER                       PIC XX.
00064          16  PC-PAYMENT-TYPE              PIC X.
00065              88  PC-PARTIAL-PAYMENT           VALUE '1'.
00066              88  PC-FINAL-PAYMENT             VALUE '2'.
00067              88  PC-LUMP-SUM-PAYMENT          VALUE '3'.
00068              88  PC-ADDITIONAL-PAYMENT        VALUE '4'.
00069              88  PC-CHARGEBLE-EXPENSE         VALUE '5'.
00070              88  PC-NON-CHARGEBLE-EXPENSE     VALUE '6'.
00071              88  PC-VOIDED-PAYMENT            VALUE '9'.
00072
00073          16  PC-FUTURE-RESERVE-AMT        PIC S9(7)V99  COMP-3.
00074          16  PC-IBNR-RESERVE-AMT          PIC S9(7)V99  COMP-3.
00075          16  PC-PTC-RESERVE-AMT           PIC S9(7)V99  COMP-3.
00076          16  PC-MANUAL-RESERVE-AMT        PIC S9(7)V99  COMP-3.
00077
00078          16  PC-SV-CARRIER                PIC X.
00079          16  PC-SV-GROUPING               PIC X(6).
00080          16  PC-SV-STATE                  PIC XX.
00081
00082          16  PC-VOID-SW                   PIC X.
00083              88  PC-PUT-CERT-INFORCE          VALUE '1'.
00084
00085          16  PC-CAUSE-CODE                PIC X(6).
00086          16  FILLER                       PIC X(48).
00087
00088          16  PC-CLAIMED-CERT-DATA.
00089              20  PC-CC-INSURED-NAME.
00090                  24  PC-CC-LAST-NAME      PIC X(15).
00091                  24  PC-CC-INITIALS       PIC XX.
00092              20  PC-CC-INSURED-AGE        PIC S99.
00093              20  PC-CC-INSURED-SEX        PIC X.
00094              20  PC-CC-ORIG-TERM          PIC S999        COMP-3.
00095              20  PC-CC-LF-BENEFIT-CD      PIC XX.
00096              20  PC-CC-LIFE-BENEFIT-AMT   PIC S9(9)V99    COMP-3.
00097              20  PC-CC-ALT-LF-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00098              20  PC-CC-LIFE-PREMIUM       PIC S9(7)V99    COMP-3.
00099              20  PC-CC-AH-BENEFIT-CD      PIC XX.
00100              20  PC-CC-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00101              20  PC-CC-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00102              20  PC-CC-RATE-CLASS         PIC XX.
00103              20  PC-CC-RATE-DEV           PIC XXX.
00104              20  PC-CC-OB-FLAG            PIC X.
00105                  88  PC-CC-OB                VALUE 'B'.
00106              20  PC-CC-AH-POLICY-STATUS   PIC X.
00107                  88  PC-CCA-POLICY-IS-ACTIVE        VALUE '1' '3'
00108                                                '4' '5' '9' '2'.
00109                  88  PC-CCA-NORMAL-ENTRY            VALUE '1'.
00110                  88  PC-CCA-POLICY-PENDING          VALUE '2'.
00111                  88  PC-CCA-POLICY-IS-RESTORE       VALUE '3'.
00112                  88  PC-CCA-CONVERSION-ENTRY        VALUE '4'.
00113                  88  PC-CCA-POLICY-IS-REISSUE       VALUE '5'.
00114                  88  PC-CCA-LUMP-SUM-DISAB          VALUE '6'.
00115                  88  PC-CCA-DEATH-CLAIM-APPLIED     VALUE '7'.
00116                  88  PC-CCA-CANCEL-APPLIED          VALUE '8'.
00117                  88  PC-CCA-REIN-ONLY               VALUE '9'.
00118              20  PC-CC-LF-POLICY-STATUS   PIC X.
00119                  88  PC-CCL-POLICY-IS-ACTIVE        VALUE '1' '3'
00120                                                '4' '5' '9' '2'.
00121                  88  PC-CCL-NORMAL-ENTRY            VALUE '1'.
00122                  88  PC-CCL-POLICY-PENDING          VALUE '2'.
00123                  88  PC-CCL-POLICY-IS-RESTORE       VALUE '3'.
00124                  88  PC-CCL-CONVERSION-ENTRY        VALUE '4'.
00125                  88  PC-CCL-POLICY-IS-REISSUE       VALUE '5'.
00126                  88  PC-CCL-LUMP-SUM-DISAB          VALUE '6'.
00127                  88  PC-CCL-DEATH-CLAIM-APPLIED     VALUE '7'.
00128                  88  PC-CCL-CANCEL-APPLIED          VALUE '8'.
00129                  88  PC-CCL-REIN-ONLY               VALUE '9'.
00130              20  PC-CC-PAY-FREQUENCY      PIC 99.
00131              20  PC-CC-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00132              20  PC-CC-SOC-SEC-NO         PIC X(11).
00133              20  PC-CC-MEMBER-NO          PIC X(12).
00134              20  PC-CC-INT-CODE           PIC X.
00135                  88  PC-CC-ADD-ON                  VALUE 'A'.
00136                  88  PC-CC-SIMPLE                  VALUE 'S'.
00137              20  PC-CC-CAPPED-TERM        PIC 999.
00138              20  PC-CC-PRIOR-LUMP-PMT     PIC S9(7)V99  COMP-3.
00139              20  PC-CC-PRIOR-DEATH-AMT    PIC S9(9)V99  COMP-3.
00140              20  PC-CC-CANCEL-DT          PIC XX.
00141              20  PC-CC-DEATH-DT           PIC XX.
00142              20  PC-CC-SETTLEMENT-DT      PIC XX.
00143              20  PC-CC-PRIOR-STATUS       PIC X.
00144              20  PC-CC-CERT-ENTRY-STATUS  PIC X.
00145          16  PC-TRLR-SEQ-NO               PIC S9(4)     COMP.
070105         16  PC-CC-CLP-STATE              PIC XX.
070105         16  FILLER                       PIC X(14).
00147          16  PC-REMAINING-BENEFIT         PIC S9(9)V99  COMP-3.
00148          16  PC-REMAINING-TERM            PIC S9(3)     COMP-3.
00149          16  FILLER                       PIC X(34).
00150
00151      12  PC-RECORD-STATUS.
00152          16  PC-CREDIT-SELECT-DT          PIC XX.
00153          16  PC-CREDIT-ACCEPT-DT          PIC XX.
00154          16  FILLER                       PIC XX.
00155          16  PC-FATAL-FLAG                PIC X.
00156              88  PC-FATAL-ERRORS             VALUE 'X'.
00157          16  PC-FORCE-CODE                PIC X.
00158              88  PC-FORCE-OFF                VALUE ' ' '0'.
00159              88  PC-CLAIM-FORCE              VALUE '6' '7'
00160                                                      '8'.
00161          16  PC-FORCE-ER-CD               PIC X.
00162              88  PC-FORCE-ERRORS             VALUE 'F'.
00163              88  PC-UNFORCED-ERRORS          VALUE 'X'.
00164          16  PC-WARN-ER-CD                PIC X.
00165              88  PC-WARNING-ERRORS           VALUE 'W'.
00166          16  PC-LF-OVERRIDE-L1            PIC X.
00167          16  PC-AH-OVERRIDE-L1            PIC X.
00168          16  FILLER                       PIC X(17).
00169          16  PC-CERT-UPDATE-SW            PIC X.
00170              88  PC-CERT-DATA-CAPTURED       VALUE '1'.
00171          16  PC-COMPANY-ID                PIC XXX.
00172          16  PC-INPUT-DT                  PIC XX.
00173
00174      12  PC-ERROR-FLAGS.
00175          16  PC-STANDARD-ERRORS.
00176              20  PC-STD-ERROR-FLAGS   OCCURS 25 TIMES PIC X.
00177          16  PC-TRANSACTION-ERRORS.
00178              20  PC-TRN-ERROR-FLAGS   OCCURS 75 TIMES PIC X.
00179
00180      12  PC-ERR-FLAGS-R REDEFINES  PC-ERROR-FLAGS.
00181          16  PC-ERR-FLAG              OCCURS 100 TIMES PIC X.
00182
00183      12  FILLER                           PIC X(25).
00184
00185 ******************************************************************
00165
00166  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PENDING-CLAIMS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6321' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00168
00169      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00170
00171      IF EIBCALEN = ZERO
00172          GO TO 8800-UNAUTHORIZED-ACCESS.
00173
00174      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00175      MOVE '5'                    TO  DC-OPTION-CODE.
00176
00177      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
00178
00179      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.
00180
00181      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00182          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00183              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00184              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00185              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00186              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00187              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00188              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00189              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00190              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00191          ELSE
00192              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00193              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00194              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00195              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00196              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00197              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00198              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00199              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00200
00201      MOVE LOW-VALUES             TO  EL6321AI.
00202      MOVE PI-COMPANY-CD          TO  PNDC-COMPANY-CD.
00203
00204      
      * EXEC CICS HANDLE CONDITION
00205 *        PGMIDERR  (9600-PGMID-ERROR)
00206 *        ERROR     (9990-ABEND)
00207 *        END-EXEC.
      *    MOVE '"$L.                  ! " #00001412' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031343132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00208  EJECT
00209      IF EIBTRNID NOT = TRANS-ID
00210          GO TO 0200-PROCESS.
00211
00212      IF EIBAID = DFHCLEAR              OR  DFHENTER
00213          GO TO 9400-CLEAR.
00214
00215      IF EIBAID = DFHPF23
00216          GO TO 8810-PF23.
00217
00218      IF EIBAID = DFHPF24
00219          GO TO 9200-RETURN-MAIN-MENU.
00220
00221      IF EIBAID = DFHPF12
00222          GO TO 9500-PF12.
00223
00224      MOVE ER-0029                TO  EMI-ERROR.
00225
00226      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00227
00228      MOVE AL-UNBON               TO  PFENTERA.
00229      MOVE -1                     TO  PFENTERL.
00230
00231      GO TO 8200-SEND-DATAONLY.
00232  EJECT
00233  0200-PROCESS.
00234      
      * EXEC CICS HANDLE CONDITION
00235 *        NOTFND   (0400-END-OF-COMPANY)
00236 *        ENDFILE  (0400-END-OF-COMPANY)
00237 *    END-EXEC.
      *    MOVE '"$I''                  ! # #00001442' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031343432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00238
00239      
      * EXEC CICS STARTBR
00240 *        DATASET  (ERPNDC-FILE-ID)
00241 *        RIDFLD   (ERPNDC-KEY)
00242 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001447' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00243
00244  0300-PROCESS-LOOP.
00245      
      * EXEC CICS READNEXT
00246 *        DATASET  (ERPNDC-FILE-ID)
00247 *        SET      (ADDRESS OF PENDING-CLAIMS)
00248 *        RIDFLD   (ERPNDC-KEY)
00249 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001453' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-CLAIMS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00250
00251      IF PC-COMPANY-CD NOT = PI-COMPANY-CD
00252          GO TO 0400-END-OF-COMPANY.
00253
00254      IF PC-CREDIT-ACCEPT-DT NOT = LOW-VALUES
00255          GO TO 0300-PROCESS-LOOP.
00256
00257      IF PC-FATAL-ERRORS
00258        OR PC-UNFORCED-ERRORS
00259          IF PC-CLAIMS
00260              IF PC-LF-CLAIM
00261                OR PC-OB-LF-CLAIM
00262                  ADD +1                 TO  LF-PYMNT-BAD
00263              ELSE
00264                  ADD +1                 TO  AH-PYMNT-BAD
00265          ELSE
00266              ADD +1                     TO  RSV-BAD
00267      ELSE
00268          IF PC-CLAIMS
00269              IF PC-LF-CLAIM
00270                OR PC-OB-LF-CLAIM
00271                  ADD +1                 TO  LF-PYMNT-GOOD
00272                  ADD PC-CLAIM-PAYMENT   TO  LF-PYMNT-AMT
00273              ELSE
00274                  ADD +1                 TO  AH-PYMNT-GOOD
00275                  ADD PC-CLAIM-PAYMENT   TO  AH-PYMNT-AMT
00276          ELSE
00277              ADD +1                     TO  RSV-GOOD
00278              ADD PC-FUTURE-RESERVE-AMT  TO  RSV-FUTURE
00279              ADD PC-PTC-RESERVE-AMT     TO  RSV-PTC
00280              ADD PC-IBNR-RESERVE-AMT    TO  RSV-IBNR.
00281
00282      GO TO 0300-PROCESS-LOOP.
00283
00284  0400-END-OF-COMPANY.
00285      ADD LF-PYMNT-GOOD  LF-PYMNT-BAD  GIVING  LFTOTO.
00286      ADD AH-PYMNT-GOOD  AH-PYMNT-BAD  GIVING  AHTOTO.
00287
00288      MOVE LF-PYMNT-GOOD          TO  LFGOODO.
00289      MOVE LF-PYMNT-BAD           TO  LFBADO.
00290      MOVE LF-PYMNT-AMT           TO  LFPMTSO.
00291      MOVE AH-PYMNT-GOOD          TO  AHGOODO.
00292      MOVE AH-PYMNT-BAD           TO  AHBADO.
00293      MOVE AH-PYMNT-AMT           TO  AHPMTSO.
00294
00295      ADD RSV-GOOD  RSV-BAD  GIVING  RETOTO.
00296
00297      MOVE RSV-GOOD               TO  REGOODO.
00298      MOVE RSV-BAD                TO  REBADO.
00299      MOVE RSV-FUTURE             TO  FUTUREO.
00300      MOVE RSV-PTC                TO  PTCO.
00301      MOVE RSV-IBNR               TO  IBNRO.
00302  EJECT
00303  8100-SEND-INITIAL-MAP.
00304      MOVE WS-CURRENT-DT          TO  DATEO.
00305      MOVE EIBTIME                TO  TIME-IN.
00306      MOVE TIME-OUT               TO  TIMEO.
00307      MOVE PI-LIFE-OVERRIDE-L6    TO  CTYPE1O.
00308      MOVE PI-AH-OVERRIDE-L6      TO  CTYPE2O.
00309      MOVE -1                     TO  PFENTERL.
00310      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGO.
00311
00312      
      * EXEC CICS SEND
00313 *        MAP     (MAP-NAME)
00314 *        MAPSET  (MAPSET-NAME)
00315 *        FROM    (EL6321AO)
00316 *        ERASE
00317 *        CURSOR
00318 *    END-EXEC.
           MOVE LENGTH OF
            EL6321AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00001520' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6321AO, 
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
           
00319
00320      GO TO 9100-RETURN-TRAN.
00321
00322  8200-SEND-DATAONLY.
00323      MOVE WS-CURRENT-DT          TO  DATEO.
00324      MOVE EIBTIME                TO  TIME-IN.
00325      MOVE TIME-OUT               TO  TIMEO.
00326      MOVE PI-LIFE-OVERRIDE-L6    TO  CTYPE1O.
00327      MOVE PI-AH-OVERRIDE-L6      TO  CTYPE2O.
00328      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSGO.
00329
00330      
      * EXEC CICS SEND
00331 *        MAP     (MAP-NAME)
00332 *        MAPSET  (MAPSET-NAME)
00333 *        FROM    (EL6321AO)
00334 *        DATAONLY
00335 *        ERASEAUP
00336 *        CURSOR
00337 *    END-EXEC.
           MOVE LENGTH OF
            EL6321AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00001538' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6321AO, 
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
           
00338
00339      GO TO 9100-RETURN-TRAN.
00340  EJECT
00341  8300-SEND-TEXT.
00342      
      * EXEC CICS SEND TEXT
00343 *        FROM    (LOGOFF-TEXT)
00344 *        LENGTH  (LOGOFF-LENGTH)
00345 *        ERASE
00346 *        FREEKB
00347 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001550' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353530' TO DFHEIV0(25:11)
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
           
00348
00349      
      * EXEC CICS RETURN
00350 *    END-EXEC.
      *    MOVE '.(                    &   #00001557' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00351
00352  8500-DATE-CONVERT.
00353      MOVE LINK-CLDATCV           TO  PGM-NAME.
00354
00355      
      * EXEC CICS LINK
00356 *        PROGRAM   (PGM-NAME)
00357 *        COMMAREA  (DATE-CONVERSION-DATA)
00358 *        LENGTH    (DC-COMM-LENGTH)
00359 *    END-EXEC.
      *    MOVE '."C                   ''   #00001563' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00360
00361  8500-EXIT.
00362      EXIT.
00363
00364  8800-UNAUTHORIZED-ACCESS.
00365      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
00366
00367      GO TO 8300-SEND-TEXT.
00368
00369  8810-PF23.
00370      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
00371      MOVE XCTL-005               TO  PGM-NAME.
00372
00373      GO TO 9300-XCTL.
00374
00375  9100-RETURN-TRAN.
00376      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
00377      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
00378
00379      
      * EXEC CICS RETURN
00380 *        TRANSID   (TRANS-ID)
00381 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00382 *        LENGTH    (PI-COMM-LENGTH)
00383 *    END-EXEC.
      *    MOVE '.(CT                  &   #00001587' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00384
00385  9200-RETURN-MAIN-MENU.
00386      MOVE XCTL-626               TO  PGM-NAME.
00387
00388      GO TO 9300-XCTL.
00389
00390  9300-XCTL.
00391      
      * EXEC CICS XCTL
00392 *        PROGRAM   (PGM-NAME)
00393 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00394 *        LENGTH    (PI-COMM-LENGTH)
00395 *    END-EXEC.
      *    MOVE '.$C                   $   #00001599' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00396  EJECT
00397  9400-CLEAR.
00398      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
00399
00400      GO TO 9300-XCTL.
00401
00402  9500-PF12.
00403      MOVE XCTL-010               TO  PGM-NAME.
00404
00405      GO TO 9300-XCTL.
00406
00407  9600-PGMID-ERROR.
00408      
      * EXEC CICS HANDLE CONDITION
00409 *        PGMIDERR  (8300-SEND-TEXT)
00410 *    END-EXEC.
      *    MOVE '"$L                   ! $ #00001616' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031363136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00411
00412      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
00413      MOVE ' '                    TO  PI-ENTRY-CD-1.
00414      MOVE XCTL-005               TO  PGM-NAME.
00415      MOVE PGM-NAME               TO  LOGOFF-PGM.
00416      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
00417
00418      GO TO 9300-XCTL.
00419
00420  9900-ERROR-FORMAT.
00421      IF NOT EMI-ERRORS-COMPLETE
00422          MOVE LINK-001           TO  PGM-NAME
00423          
      * EXEC CICS LINK
00424 *            PROGRAM   (PGM-NAME)
00425 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
00426 *            LENGTH    (EMI-COMM-LENGTH)
00427 *        END-EXEC.
      *    MOVE '."C                   ''   #00001631' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00428
00429  9900-EXIT.
00430      EXIT.
00431
00432  9990-ABEND.
00433      MOVE LINK-004               TO  PGM-NAME.
00434      MOVE DFHEIBLK               TO  EMI-LINE1
00435
00436      
      * EXEC CICS LINK
00437 *        PROGRAM   (PGM-NAME)
00438 *        COMMAREA  (EMI-LINE1)
00439 *        LENGTH    (72)
00440 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00001644' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00441
00442      GO TO 8200-SEND-DATAONLY.
00443
00444      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6321' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00445
00446  9995-SECURITY-VIOLATION.
00447 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00001672' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363732' TO DFHEIV0(25:11)
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
00448
00449  9995-EXIT.
00450      EXIT.
00451

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6321' TO DFHEIV1
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
               GO TO 0400-END-OF-COMPANY,
                     0400-END-OF-COMPANY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6321' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
