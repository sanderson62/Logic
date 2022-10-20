00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL679.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/14/96 07:57:55.
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
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022
00023 *REMARKS.    TRANSACTION - EXF5 - BILLING STATUS REPORT.
00024 *            PAYMENT SCREEN.
00025  ENVIRONMENT DIVISION.
00026
00027      EJECT
00028  DATA DIVISION.
00029  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00030  77  FILLER  PIC X(32)  VALUE '********************************'.
00031  77  FILLER  PIC X(32)  VALUE '*     EL679 WORKING STORAGE    *'.
00032  77  FILLER  PIC X(32)  VALUE '************ V/M 2.003 *********'.
00033
00034  01  WS-DATE-AREA.
00035      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00036      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00037
00038  01  STANDARD-AREAS.
00039      12  MAP-NAME            PIC X(8)    VALUE 'EL679A'.
00040      12  MAPSET-NAME         PIC X(8)    VALUE 'EL679S'.
00041      12  SCREEN-NUMBER       PIC X(4)    VALUE '640C'.
00042      12  TRANS-ID            PIC X(4)    VALUE 'EXF5'.
00043      12  TRANS-6791          PIC X(4)    VALUE 'EXG5'.
00044      12  THIS-PGM            PIC X(8)    VALUE 'EL679'.
00045      12  PGM-NAME            PIC X(8).
00046      12  TIME-IN             PIC S9(7).
00047      12  TIME-OUT-R  REDEFINES TIME-IN.
00048          16  FILLER          PIC X.
00049          16  TIME-OUT        PIC 99V99.
00050          16  FILLER          PIC X(2).
00051      12  XCTL-005            PIC X(8)    VALUE 'EL005'.
00052      12  XCTL-010            PIC X(8)    VALUE 'EL010'.
00053      12  XCTL-626            PIC X(8)    VALUE 'EL626'.
00054      12  LINK-001            PIC X(8)    VALUE 'EL001'.
00055      12  LINK-004            PIC X(8)    VALUE 'EL004'.
00056      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00057      12  ERBILL-FILE-ID      PIC X(8)    VALUE 'ERBILL'.
00058      12  ELCNTL-FILE-ID      PIC X(8)    VALUE 'ELCNTL'.
00059
00060  01  WORK-AREAS.
00061      12  TEXT-SUB            PIC 99      VALUE ZEROS.
00062      12  NDX                 PIC 99      VALUE ZEROS.
00063      EJECT
00064  01  ACCESS-KEYS.
00065      12  ERBILL-KEY.
00066          16  ERBILL-CO-CD        PIC X       VALUE SPACE.
00067          16  ERBILL-COMPARE.
00068              20  ERBILL-CARRIER  PIC X       VALUE SPACE.
00069              20  ERBILL-GROUP    PIC X(6)    VALUE SPACES.
00070              20  ERBILL-ACCT     PIC X(10)   VALUE SPACES.
00071              20  ERBILL-FIN-RESP PIC X(10)   VALUE SPACES.
00072          16  ERBILL-REC-TYPE     PIC X       VALUE SPACE.
00073          16  ERBILL-LINE-SEQ-NO  PIC S9(4)   COMP VALUE ZEROS.
00074
00075      EJECT
00076 *    COPY ELCDATE.
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
00077
00078      EJECT
00079 *    COPY ELCLOGOF.
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
00082 *    COPY ELCATTR.
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
00083
00084      EJECT
00085 *    COPY ELCEMIB.
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
00086
00087      EJECT
00088 *    COPY ELCINTF.
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
00089      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00090          16  PI-SAV-ERBILL-KEY.
00091              20  PI-SAV-CO-CD            PIC X.
00092              20  PI-SAV-CARRIER          PIC X.
00093              20  PI-SAV-GROUP            PIC X(6).
00094              20  PI-SAV-ACCOUNT          PIC X(10).
00095              20  PI-SAV-FIN-RESP         PIC X(10).
00096              20  PI-SAV-REC-TYPE         PIC X.
00097              20  PI-SAV-LINE-SEQ-NO      PIC S9(4)   COMP.
00098          16  PI-PREV-ERBILL-KEY.
00099              20  PI-PREV-CO-CD           PIC X.
00100              20  PI-PREV-CARRIER         PIC X.
00101              20  PI-PREV-GROUP           PIC X(6).
00102              20  PI-PREV-ACCOUNT         PIC X(10).
00103              20  PI-PREV-FIN-RESP        PIC X(10).
00104              20  PI-PREV-REC-TYPE        PIC X.
00105              20  PI-PREV-LINE-SEQ-NO     PIC S9(4)   COMP.
00106          16  PI-ACCT-EOF-SW              PIC X.
00107              88  PI-ACCT-EOF           VALUE 'Y'.
00108          16  FILLER                      PIC X(577).
00109      EJECT
00110 *    COPY ELCJPFX.
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
00111                              PIC X(750).
00112
00113      EJECT
00114 *    COPY ELCAID.
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
00115  01  FILLER    REDEFINES DFHAID.
00116      12  FILLER              PIC X(8).
00117      12  PF-VALUES           PIC X       OCCURS 2.
00118
00119      EJECT
00120 *    COPY EL679S.
       01  EL679AI.
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
           05  CARR1L PIC S9(0004) COMP.
           05  CARR1F PIC  X(0001).
           05  FILLER REDEFINES CARR1F.
               10  CARR1A PIC  X(0001).
           05  CARR1I PIC  X(0001).
      *    -------------------------------
           05  GROUP1L PIC S9(0004) COMP.
           05  GROUP1F PIC  X(0001).
           05  FILLER REDEFINES GROUP1F.
               10  GROUP1A PIC  X(0001).
           05  GROUP1I PIC  X(0006).
      *    -------------------------------
           05  ACCT1L PIC S9(0004) COMP.
           05  ACCT1F PIC  X(0001).
           05  FILLER REDEFINES ACCT1F.
               10  ACCT1A PIC  X(0001).
           05  ACCT1I PIC  X(0010).
      *    -------------------------------
           05  FINRSP1L PIC S9(0004) COMP.
           05  FINRSP1F PIC  X(0001).
           05  FILLER REDEFINES FINRSP1F.
               10  FINRSP1A PIC  X(0001).
           05  FINRSP1I PIC  X(0010).
      *    -------------------------------
           05  BALFWD1L PIC S9(0004) COMP.
           05  BALFWD1F PIC  X(0001).
           05  FILLER REDEFINES BALFWD1F.
               10  BALFWD1A PIC  X(0001).
           05  BALFWD1I PIC  X(0013).
      *    -------------------------------
           05  NAME1L PIC S9(0004) COMP.
           05  NAME1F PIC  X(0001).
           05  FILLER REDEFINES NAME1F.
               10  NAME1A PIC  X(0001).
           05  NAME1I PIC  X(0028).
      *    -------------------------------
           05  PREM1L PIC S9(0004) COMP.
           05  PREM1F PIC  X(0001).
           05  FILLER REDEFINES PREM1F.
               10  PREM1A PIC  X(0001).
           05  PREM1I PIC  X(0013).
      *    -------------------------------
           05  BILLDT1L PIC S9(0004) COMP.
           05  BILLDT1F PIC  X(0001).
           05  FILLER REDEFINES BILLDT1F.
               10  BILLDT1A PIC  X(0001).
           05  BILLDT1I PIC  X(0008).
      *    -------------------------------
           05  REMIT1L PIC S9(0004) COMP.
           05  REMIT1F PIC  X(0001).
           05  FILLER REDEFINES REMIT1F.
               10  REMIT1A PIC  X(0001).
           05  REMIT1I PIC  X(0013).
      *    -------------------------------
           05  PRTDT1L PIC S9(0004) COMP.
           05  PRTDT1F PIC  X(0001).
           05  FILLER REDEFINES PRTDT1F.
               10  PRTDT1A PIC  X(0001).
           05  PRTDT1I PIC  X(0008).
      *    -------------------------------
           05  ICOMP1L PIC S9(0004) COMP.
           05  ICOMP1F PIC  X(0001).
           05  FILLER REDEFINES ICOMP1F.
               10  ICOMP1A PIC  X(0001).
           05  ICOMP1I PIC  X(0013).
      *    -------------------------------
           05  CCOMP1L PIC S9(0004) COMP.
           05  CCOMP1F PIC  X(0001).
           05  FILLER REDEFINES CCOMP1F.
               10  CCOMP1A PIC  X(0001).
           05  CCOMP1I PIC  X(0013).
      *    -------------------------------
           05  ADJMNT1L PIC S9(0004) COMP.
           05  ADJMNT1F PIC  X(0001).
           05  FILLER REDEFINES ADJMNT1F.
               10  ADJMNT1A PIC  X(0001).
           05  ADJMNT1I PIC  X(0013).
      *    -------------------------------
           05  DISBUR1L PIC S9(0004) COMP.
           05  DISBUR1F PIC  X(0001).
           05  FILLER REDEFINES DISBUR1F.
               10  DISBUR1A PIC  X(0001).
           05  DISBUR1I PIC  X(0013).
      *    -------------------------------
           05  ENDBAL1L PIC S9(0004) COMP.
           05  ENDBAL1F PIC  X(0001).
           05  FILLER REDEFINES ENDBAL1F.
               10  ENDBAL1A PIC  X(0001).
           05  ENDBAL1I PIC  X(0013).
      *    -------------------------------
           05  CARR2L PIC S9(0004) COMP.
           05  CARR2F PIC  X(0001).
           05  FILLER REDEFINES CARR2F.
               10  CARR2A PIC  X(0001).
           05  CARR2I PIC  X(0001).
      *    -------------------------------
           05  GROUP2L PIC S9(0004) COMP.
           05  GROUP2F PIC  X(0001).
           05  FILLER REDEFINES GROUP2F.
               10  GROUP2A PIC  X(0001).
           05  GROUP2I PIC  X(0006).
      *    -------------------------------
           05  ACCT2L PIC S9(0004) COMP.
           05  ACCT2F PIC  X(0001).
           05  FILLER REDEFINES ACCT2F.
               10  ACCT2A PIC  X(0001).
           05  ACCT2I PIC  X(0010).
      *    -------------------------------
           05  FINRSP2L PIC S9(0004) COMP.
           05  FINRSP2F PIC  X(0001).
           05  FILLER REDEFINES FINRSP2F.
               10  FINRSP2A PIC  X(0001).
           05  FINRSP2I PIC  X(0010).
      *    -------------------------------
           05  BALFWD2L PIC S9(0004) COMP.
           05  BALFWD2F PIC  X(0001).
           05  FILLER REDEFINES BALFWD2F.
               10  BALFWD2A PIC  X(0001).
           05  BALFWD2I PIC  X(0013).
      *    -------------------------------
           05  NAME2L PIC S9(0004) COMP.
           05  NAME2F PIC  X(0001).
           05  FILLER REDEFINES NAME2F.
               10  NAME2A PIC  X(0001).
           05  NAME2I PIC  X(0028).
      *    -------------------------------
           05  PREM2L PIC S9(0004) COMP.
           05  PREM2F PIC  X(0001).
           05  FILLER REDEFINES PREM2F.
               10  PREM2A PIC  X(0001).
           05  PREM2I PIC  X(0013).
      *    -------------------------------
           05  BILLDT2L PIC S9(0004) COMP.
           05  BILLDT2F PIC  X(0001).
           05  FILLER REDEFINES BILLDT2F.
               10  BILLDT2A PIC  X(0001).
           05  BILLDT2I PIC  X(0008).
      *    -------------------------------
           05  REMIT2L PIC S9(0004) COMP.
           05  REMIT2F PIC  X(0001).
           05  FILLER REDEFINES REMIT2F.
               10  REMIT2A PIC  X(0001).
           05  REMIT2I PIC  X(0013).
      *    -------------------------------
           05  PRTDT2L PIC S9(0004) COMP.
           05  PRTDT2F PIC  X(0001).
           05  FILLER REDEFINES PRTDT2F.
               10  PRTDT2A PIC  X(0001).
           05  PRTDT2I PIC  X(0008).
      *    -------------------------------
           05  ICOMP2L PIC S9(0004) COMP.
           05  ICOMP2F PIC  X(0001).
           05  FILLER REDEFINES ICOMP2F.
               10  ICOMP2A PIC  X(0001).
           05  ICOMP2I PIC  X(0013).
      *    -------------------------------
           05  CCOMP2L PIC S9(0004) COMP.
           05  CCOMP2F PIC  X(0001).
           05  FILLER REDEFINES CCOMP2F.
               10  CCOMP2A PIC  X(0001).
           05  CCOMP2I PIC  X(0013).
      *    -------------------------------
           05  ADJMNT2L PIC S9(0004) COMP.
           05  ADJMNT2F PIC  X(0001).
           05  FILLER REDEFINES ADJMNT2F.
               10  ADJMNT2A PIC  X(0001).
           05  ADJMNT2I PIC  X(0013).
      *    -------------------------------
           05  DISPUR2L PIC S9(0004) COMP.
           05  DISPUR2F PIC  X(0001).
           05  FILLER REDEFINES DISPUR2F.
               10  DISPUR2A PIC  X(0001).
           05  DISPUR2I PIC  X(0013).
      *    -------------------------------
           05  ENDBAL2L PIC S9(0004) COMP.
           05  ENDBAL2F PIC  X(0001).
           05  FILLER REDEFINES ENDBAL2F.
               10  ENDBAL2A PIC  X(0001).
           05  ENDBAL2I PIC  X(0013).
      *    -------------------------------
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  ERRMSGI PIC  X(0078).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
       01  EL679AO REDEFINES EL679AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARR1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUP1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSP1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALFWD1O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAME1O PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PREM1O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BILLDT1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REMIT1O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRTDT1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ICOMP1O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCOMP1O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJMNT1O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DISBUR1O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENDBAL1O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARR2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUP2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRSP2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALFWD2O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAME2O PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PREM2O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BILLDT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REMIT2O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRTDT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ICOMP2O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCOMP2O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADJMNT2O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DISPUR2O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENDBAL2O PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0078).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
00121  01  MAP-A REDEFINES EL679AO.
00122      12  FILLER                  PIC X(31).
00123      12  FILLER   OCCURS 2 TIMES.
00124          16  FILLER              PIC X(3).
00125          16  CARRIER             PIC X.
00126          16  FILLER              PIC X(3).
00127          16  GROUPING            PIC X(6).
00128          16  FILLER              PIC X(3).
00129          16  ACCOUNT             PIC X(10).
00130          16  FILLER              PIC X(3).
00131          16  FINRESP             PIC X(10).
00132          16  FILLER              PIC X(3).
00133          16  BALFRWD             PIC Z,ZZZ,ZZ9.99-.
00134          16  FILLER              PIC X(3).
uktdel*        16  NAME                PIC X(28).
uktins         16  NAMEX               PIC X(28).
00136          16  FILLER              PIC X(3).
00137          16  PREMIUM             PIC Z,ZZZ,ZZ9.99-.
00138          16  FILLER              PIC X(3).
00139          16  BILL-DATE           PIC X(8).
00140          16  FILLER              PIC X(3).
00141          16  REMITTED            PIC Z,ZZZ,ZZ9.99-.
00142          16  FILLER              PIC X(3).
00143          16  PRT-DATE            PIC X(8).
00144          16  FILLER              PIC X(3).
00145          16  ISSUE-COMP          PIC Z,ZZZ,ZZ9.99-.
00146          16  FILLER              PIC X(3).
00147          16  CANCEL-COMP         PIC Z,ZZZ,ZZ9.99-.
00148          16  FILLER              PIC X(3).
00149          16  ADJUSTMENTS         PIC Z,ZZZ,ZZ9.99-.
00150          16  FILLER              PIC X(3).
00151          16  DISBURSED           PIC Z,ZZZ,ZZ9.99-.
00152          16  FILLER              PIC X(3).
00153          16  END-BAL             PIC Z,ZZZ,ZZ9.99-.
00154      EJECT
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
00156  01  DFHCOMMAREA             PIC X(1024).
00157
00158      EJECT
00159 *01 PARMLIST .
00160 *    02  FILLER              PIC S9(8)   COMP.
00161 *    02  ERBILL-POINTER      PIC S9(8)   COMP.
00162
00163 *    COPY ERCBILL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCBILL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = BILLING STATEMENTS FOR PRINTING           *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 210  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERBILL                        RKP=2,LEN=31    *
00013 *                                                                *
00014 *   LOG = NO                                                     *
00015 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
00017  01  BILLING-STATEMENT.
00018      02 BILLING-STATEMENT-FILE.
00019      12  BI-RECORD-ID                PIC XX.
00020          88  VALID-BI-ID                VALUE 'BI'.
00021
00022      12  BI-CONTROL-PRIMARY.
00023          16  BI-COMPANY-CD           PIC X.
00024          16  BI-CARRIER              PIC X.
00025          16  BI-GROUPING             PIC X(6).
00026          16  BI-ACCOUNT              PIC X(10).
00027          16  BI-FIN-RESP             PIC X(10).
00028          16  BI-RECORD-TYPE          PIC X.
00029              88  BI-HEADER-DATA         VALUE '1'.
00030              88  BI-ADDRESS-DATA        VALUE '2'.
00031              88  BI-TEXT-DATA           VALUE '3'.
00032          16  BI-LINE-SEQ-NO          PIC S9(4)     COMP.
00033
00034      12  BI-TEXT-RECORD.
00035          16  BI-SKIP-CONTROL         PIC X.
00036          16  BI-TEXT-LINE            PIC X(157).
00037          16  BI-TEXT-LINE-1 REDEFINES BI-TEXT-LINE.
00038              20  BI-ADDR-LIT         PIC X(14).
00039              20  BI-CO               PIC X(7).
00040              20  BI-DASH             PIC X.
00041              20  BI-ACCT             PIC X(10).
00042              20  FILLER              PIC XX.
00043              20  BI-ACCT-ADDR        PIC X(30).
00044              20  FILLER              PIC X.
00045              20  BI-REMIT-LIT        PIC X(11).
00046              20  FILLER              PIC XX.
00047              20  BI-REMIT-ADDR       PIC X(30).
00048              20  FILLER              PIC X(49).
00049          16  BI-TEXT-LINE-2 REDEFINES BI-TEXT-LINE.
00050              20  BI-INS-LAST-NAME    PIC X(15).
00051              20  FILLER              PIC X.
00052              20  BI-INS-1ST-NAME     PIC X(10).
00053              20  FILLER REDEFINES BI-INS-1ST-NAME.
00054                  24  BI-INS-INITS    PIC XX.
00055                  24  FILLER          PIC X(8).
00056              20  FILLER              PIC X.
00057              20  BI-UNDRWRTR.
00058                  24  BI-INS-INIT     PIC X.
00059                  24  FILLER          PIC XXX.
00060              20  BI-CERT             PIC X(11).
00061              20  FILLER              PIC XX.
00062              20  BI-EFF-DT           PIC X(8).
00063              20  FILLER              PIC XX.
00064              20  BI-CAN-DT           PIC X(8).
00065              20  FILLER              PIC XX.
00066              20  BI-ED-TERM          PIC ZZZ.
00067              20  FILLER              PIC XX.
00068              20  BI-TYPE             PIC XXXX.
00069              20  FILLER              PIC X.
00070              20  BI-PREM             PIC ZZ,ZZZ,ZZZ.ZZ-.
00071              20  FILLER              PIC X(3).
00072              20  BI-ED-RATE          PIC ZZZ.ZZZ.
00073              20  FILLER              PIC XX.
00074              20  BI-COMM             PIC ZZ,ZZZ,ZZZ.ZZ-.
00075              20  FILLER              PIC XX.
00076              20  BI-FACE-AMT         PIC ZZZ,ZZZ,ZZZ.ZZ-.
00077              20  FILLER              PIC X(26).
00078          16  BI-TEXT-LINE-3 REDEFINES BI-TEXT-LINE.
00079              20  FILLER              PIC X(42).
00080              20  BI-TOT-DESC         PIC X(20).
00081              20  FILLER REDEFINES BI-TOT-DESC.
00082                  24  BI-TOT-LIT      PIC X(6).
00083                  24  BI-OVERRIDE-L6  PIC X(14).
00084              20  FILLER              PIC X(11).
00085              20  BI-TOT-PREM         PIC ZZZ,ZZZ,ZZZ.99-.
00086              20  BI-TOT-DASH REDEFINES
00087                  BI-TOT-PREM         PIC X(15).
00088              20  FILLER              PIC X(11).
00089              20  BI-COM-TOT          PIC ZZZ,ZZZ,ZZZ.99-.
00090              20  FILLER              PIC XX.
00091              20  BI-FACE-TOT         PIC ZZZ,ZZZ,ZZZ.99-.
00092              20  FILLER              PIC X(26).
00093          16  BI-TEXT-LINE-4 REDEFINES BI-TEXT-LINE.
00094              20  BI-ENTRY-DESC       PIC X(30).
00095              20  FILLER              PIC X(11).
00096              20  BI-ENTRY-AMT        PIC ZZZZ,ZZZ,ZZZ.99-.
00097              20  FILLER              PIC X(31).
00098              20  BI-ACCTG-COMMENT    PIC X(30).
00099              20  FILLER              PIC X(39).
00100          16  BI-TEXT-LINE-5 REDEFINES BI-TEXT-LINE.
00101              20  FILLER              PIC X(42).
00102              20  BI-TOT-DESC5        PIC X(20).
00103              20  FILLER              PIC X(11).
00104              20  BI-TOT-PREM5        PIC ZZZ,ZZZ,ZZZ.99-.
00105              20  BI-COM-TOT5         PIC ZZZ,ZZZ,ZZZ.99-.
00106              20  BI-NON-PREM5        PIC ZZZ,ZZZ,ZZZ.99-.
00107              20  BI-NON-COMM5        PIC ZZZ,ZZZ,ZZZ.99-.
00108              20  FILLER              PIC X(24).
00109          16  BI-TEXT-FIRST REDEFINES BI-TEXT-LINE.
00110              20  BI-TEXT-2-81        PIC X(80).
00111              20  FILLER              PIC X(77).
00112          16  BI-TEXT-LAST REDEFINES BI-TEXT-LINE.
00113              20  FILLER              PIC X(53).
00114              20  BI-TEXT-55-133      PIC X(79).
00115              20  FILLER              PIC X(25).
00116          16  BI-TEXT-TYPE            PIC X.
00117              88 DETAIL-LINE              VALUE 'D'.
00118          16  BI-TERM                 PIC S999.
00119          16  BI-BENEFIT-AMT          PIC S9(9)V99 COMP-3.
00120          16  BI-PREMIUM-AMT          PIC S9(7)V99 COMP-3.
00121          16  BI-RATE                 PIC S99V9(5) COMP-3.
00122
00123      12  BI-ADDRESS-RECORD  REDEFINES  BI-TEXT-RECORD.
00124          16  FILLER                  PIC X.
00125          16  BI-ACCT-ADDRESS-LINE    PIC X(30).
00126          16  FILLER                  PIC X(10).
00127          16  BI-REMIT-ADDRESS-LINE   PIC X(30).
00128          16  FILLER                  PIC X(106).
00129
00130      12  BI-HEADER-RECORD  REDEFINES  BI-TEXT-RECORD.
00131          16  BI-PROCESSOR-CD         PIC X(4).
00132          16  BI-STATEMENT-TYPE       PIC X.
00133              88  BI-PREVIEW-ONLY         VALUE 'P'.
00134          16  BI-NO-OF-COPIES         PIC S9.
00135          16  BI-CREATION-DT          PIC XX.
00136          16  BI-INITIAL-PRINT-DATE   PIC XX.
00137          16  BI-ACCOUNT-TOTALS.
00138              20  BI-BAL-FRWD         PIC S9(9)V99     COMP-3.
00139              20  BI-PREMIUM          PIC S9(9)V99     COMP-3.
00140              20  BI-REMITTED         PIC S9(9)V99     COMP-3.
00141              20  BI-TOT-ISS-COMP     PIC S9(9)V99     COMP-3.
00142              20  BI-TOT-CAN-COMP     PIC S9(9)V99     COMP-3.
00143              20  BI-ADJUSTMNTS       PIC S9(9)V99     COMP-3.
00144              20  BI-DISBURSED        PIC S9(9)V99     COMP-3.
00145              20  BI-END-BAL          PIC S9(9)V99     COMP-3.
00146          16  BI-FIN-RESP-ACCT        PIC X(10).
00147          16  BI-FIN-RESP-NAME        PIC X(30).
00148          16  FILLER                  PIC X(79).
00149
00150
00151      02 GA-BILLING-STATEMENT REDEFINES BILLING-STATEMENT-FILE.
00152      12  FILLER                      PIC XX.
00153
00154      12  GA-CONTROL-PRIMARY.
00155          16  FILLER                  PIC X(31).
00156
00157      12  GA-TEXT-RECORD.
00158          16  GA-SKIP-CONTROL         PIC X.
00159          16  GA-TEXT-LINE            PIC X(132).
00160          16  GA-TEXT-LINE-1 REDEFINES GA-TEXT-LINE.
00161              20  FILLER              PIC X.
00162              20  GA-CARRIER          PIC X.
00163              20  GA-GROUPING         PIC X(6).
00164              20  GA-DASH             PIC X.
00165              20  GA-AGENT            PIC X(10).
00166              20  FILLER              PIC X.
00167              20  GA-AGENT-ADDR       PIC X(30).
00168              20  FILLER              PIC X(82).
00169          16  GA-TEXT-LINE-2 REDEFINES GA-TEXT-LINE.
00170              20  GA-ACCT             PIC X(10).
00171              20  FILLER              PIC X.
00172              20  GA-ACCT-NAME        PIC X(30).
00173              20  GA-BEG-BAL          PIC ZZZZ,ZZZ.99-.
00174              20  GA-NET-PREM         PIC ZZZZ,ZZZ.ZZ-.
00175              20  GA-ACCT-COMP        PIC ZZZZ,ZZZ.ZZ-.
00176              20  GA-PMTS-ADJS        PIC ZZZZ,ZZZ.ZZ-.
00177              20  GA-UNPAID-NET-PREM  PIC ZZZZ,ZZZ.ZZ-.
00178              20  GA-BEN-OVERRIDE-L6  PIC X(6).
00179              20  FILLER              PIC X.
00180              20  GA-OVERWRITE        PIC ZZZZ,ZZZ.ZZ-.
00181              20  GA-AMT-DUE          PIC ZZZ,ZZZ.99-.
00182              20  FILLER              PIC X.
00183          16  GA-TEXT-LINE-3 REDEFINES GA-TEXT-LINE.
00184              20  FILLER              PIC X(11).
00185              20  GA-ENTRY-DESC       PIC X(30).
00186              20  FILLER              PIC X(60).
00187              20  GA-ENTRY-COMMENT    PIC X(30).
00188              20  FILLER              PIC X.
00189          16  FILLER.
00190              20  GA-BENEFIT-CD       PIC XX.
00191              20  GA-BEG-BAL-AMT      PIC S9(7)V99 COMP-3.
00192              20  GA-END-BAL-AMT      PIC S9(7)V99 COMP-3.
00193              20  GA-NET-PREM-AMT     PIC S9(7)V99 COMP-3.
00194              20  GA-ACCT-COMP-AMT    PIC S9(7)V99 COMP-3.
00195              20  GA-PMTS-ADJS-AMT    PIC S9(7)V99 COMP-3.
00196              20  GA-UNPAID-NET-AMT   PIC S9(7)V99 COMP-3.
00197              20  GA-OVERWRITE-AMT    PIC S9(7)V99 COMP-3.
00198              20  GA-AMT-DUE-AMT      PIC S9(7)V99 COMP-3.
00199
00200      12  GA-ADDRESS-RECORD  REDEFINES  GA-TEXT-RECORD.
00201          16  FILLER                  PIC X.
00202          16  GA-ACCT-ADDRESS-LINE    PIC X(30).
00203          16  FILLER                  PIC X(144).
00204
00205      12  GA-HEADER-RECORD  REDEFINES  GA-TEXT-RECORD.
00206          16  GA-PROCESSOR-CD         PIC X(4).
00207          16  GA-STATEMENT-TYPE       PIC X.
00208              88  GA-PREVIEW-ONLY         VALUE 'P'.
00209          16  GA-NO-OF-COPIES         PIC S9.
00210          16  GA-CREATION-DT          PIC XX.
00211          16  GA-INITIAL-PRINT-DATE   PIC XX.
00212          16  GA-AGENT-TOTALS.
00213              20  GA-NET-UNPD         PIC S9(9)V99     COMP-3.
00214              20  GA-COMP-UNPD-PREM   PIC S9(9)V99     COMP-3.
00215              20  GA-PREMIUM          PIC S9(9)V99     COMP-3.
00216              20  GA-REMITTED         PIC S9(9)V99     COMP-3.
00217              20  GA-TOT-ISS-COMP     PIC S9(9)V99     COMP-3.
00218              20  GA-TOT-CAN-COMP     PIC S9(9)V99     COMP-3.
00219              20  GA-ADJUSTMNTS       PIC S9(9)V99     COMP-3.
00220              20  GA-DISBURSED        PIC S9(9)V99     COMP-3.
00221              20  GA-END-BALANCE      PIC S9(9)V99     COMP-3.
00222          16  GA-AGENTS-NAME          PIC X(30).
00223          16  FILLER                  PIC X(81).
00224      12  FILLER                      PIC XX.
00225
00226
00164
00165      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA BILLING-STATEMENT.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL679' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00167
00168      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00169      MOVE '5'                   TO DC-OPTION-CODE.
00170      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00171      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00172      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00173
00174      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00175      IF EIBCALEN = 0
00176          GO TO 8800-UNAUTHORIZED-ACCESS.
00177
00178      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00179          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00180              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00181              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00182              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00183              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00184              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00185              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00186              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00187              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00188            ELSE
00189              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00190              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00191              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00192              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00193              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00194              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00195              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00196              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00197
00198      MOVE LOW-VALUES             TO EL679AI.
00199      MOVE PI-COMPANY-CD          TO PI-SAV-CO-CD
00200                                     PI-PREV-CO-CD.
00201      IF EIBTRNID NOT = TRANS-ID
00202          MOVE '1'                TO PI-SAV-REC-TYPE
00203          MOVE +0                 TO PI-SAV-LINE-SEQ-NO
00204          MOVE DFHENTER           TO EIBAID
00205          GO TO 1000-BROWSE-FORWARD.
00206
00207      
      * EXEC CICS HANDLE CONDITION
00208 *        PGMIDERR  (9600-PGMID-ERROR)
00209 *        ERROR     (9990-ABEND)
00210 *        END-EXEC.
      *    MOVE '"$L.                  ! " #00001533' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031353333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00211
00212      IF EIBAID = DFHCLEAR
00213          GO TO 9400-CLEAR.
00214
00215      EJECT
00216  0200-RECEIVE.
00217      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00218          MOVE 0008               TO EMI-ERROR
00219          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00220          MOVE -1                 TO PFENTERL
00221          GO TO 8200-SEND-DATAONLY.
00222
00223      
      * EXEC CICS RECEIVE
00224 *        MAP      (MAP-NAME)
00225 *        MAPSET   (MAPSET-NAME)
00226 *        INTO     (EL679AI)
00227 *        END-EXEC.
           MOVE LENGTH OF
            EL679AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001549' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL679AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00228
00229      IF PFENTERL = 0
00230          GO TO 0300-CHECK-PFKEYS.
00231      IF EIBAID NOT = DFHENTER
00232          MOVE 0004               TO EMI-ERROR
00233          GO TO 0320-INPUT-ERROR.
00234      IF (PFENTERI NUMERIC) AND (PFENTERI > 0 AND < 25)
00235          MOVE PF-VALUES (PFENTERI) TO EIBAID
00236      ELSE
00237          MOVE 0029               TO EMI-ERROR
00238          GO TO 0320-INPUT-ERROR.
00239
00240  0300-CHECK-PFKEYS.
00241      IF EIBAID = DFHPF23
00242          GO TO 8810-PF23.
00243      IF EIBAID = DFHPF24
00244          GO TO 9200-RETURN-MAIN-MENU.
00245      IF EIBAID = DFHPF12
00246          GO TO 9500-PF12.
00247
00248      IF EIBAID = DFHENTER
00249        OR DFHPF1
00250          GO TO 1000-BROWSE-FORWARD.
00251
00252      IF EIBAID = DFHPF2
00253          GO TO 2000-BROWSE-BACKWARD.
00254      IF EIBAID = DFHPF3
00255          GO TO 4000-START-REPORT-TRANSACTION.
00256
00257  0320-INPUT-ERROR.
00258      MOVE 0029                   TO EMI-ERROR.
00259      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00260      MOVE AL-UNBON               TO PFENTERA.
00261      MOVE -1                     TO PFENTERL.
00262      GO TO 8200-SEND-DATAONLY.
00263
00264      EJECT
00265  1000-BROWSE-FORWARD.
00266      MOVE PI-SAV-ERBILL-KEY      TO ERBILL-KEY.
00267      PERFORM 5000-START-BROWSE THRU 5099-EXIT.
00268
00269      IF EMI-ERROR NOT = ZEROS
00270          GO TO 8200-SEND-DATAONLY.
00271
00272      PERFORM 1100-BROWSE-LOOP THRU 1100-EXIT
00273          VARYING NDX FROM 1 BY 1
00274          UNTIL NDX GREATER THAN 2.
00275
00276      IF EMI-ERROR NOT = ZEROS
00277          GO TO 8200-SEND-DATAONLY.
00278
00279      GO TO 8100-SEND-INITIAL-MAP.
00280
00281  1100-BROWSE-LOOP.
00282      IF EIBTRNID = TRANS-ID
00283        OR NDX GREATER THAN 1
00284          MOVE '3'                TO ERBILL-REC-TYPE
00285          MOVE 9999               TO ERBILL-LINE-SEQ-NO.
00286
00287      PERFORM 5100-READ-NEXT-RECORD THRU 5199-EXIT.
00288
00289      IF EMI-ERROR NOT = ZEROS
00290          MOVE 3                  TO NDX
00291          IF EMI-ERROR = 2558
00292              MOVE ZEROS          TO EMI-ERROR
00293              GO TO 1100-EXIT
00294          ELSE
00295              GO TO 1100-EXIT.
00296
00297      IF NDX = 1
00298          MOVE BI-CONTROL-PRIMARY TO PI-PREV-ERBILL-KEY
00299      ELSE
00300          MOVE BI-CONTROL-PRIMARY TO PI-SAV-ERBILL-KEY.
00301
00302      PERFORM 3000-FORMAT-SCREEN THRU 3099-EXIT.
00303
00304  1100-EXIT.
00305      EXIT.
00306      EJECT
00307  2000-BROWSE-BACKWARD.
00308      MOVE 2                      TO NDX.
00309  2100-BROWSE-LOOP.
00310      MOVE PI-PREV-ERBILL-KEY     TO ERBILL-KEY.
00311      PERFORM 5000-START-BROWSE THRU 5099-EXIT.
00312
00313      IF EMI-ERROR NOT = ZEROS
00314          GO TO 8200-SEND-DATAONLY.
00315
00316      PERFORM 5200-READPREV THRU 5299-EXIT 2 TIMES
00317
00318      IF EMI-ERROR NOT = ZEROS
00319          GO TO 8200-SEND-DATAONLY.
00320
00321      MOVE BI-CONTROL-PRIMARY     TO PI-PREV-ERBILL-KEY.
00322      MOVE '1'                    TO PI-PREV-REC-TYPE.
00323      MOVE ZEROS                  TO PI-PREV-LINE-SEQ-NO.
00324
00325      PERFORM 5400-END-BROWSE THRU 5499-EXIT.
00326      PERFORM 5300-READ-RECORD THRU 5399-EXIT.
00327
00328      IF EMI-ERROR NOT = ZEROS
00329          GO TO 8200-SEND-DATAONLY.
00330      IF NDX = 2
00331          MOVE BI-CONTROL-PRIMARY TO PI-SAV-ERBILL-KEY.
00332
00333      PERFORM 3000-FORMAT-SCREEN THRU 3099-EXIT.
00334
00335      SUBTRACT 1 FROM NDX.
00336      IF NDX LESS THAN 1
00337          GO TO 8100-SEND-INITIAL-MAP.
00338
00339      GO TO 2100-BROWSE-LOOP.
00340      EJECT
00341  3000-FORMAT-SCREEN.
00342      MOVE BI-CARRIER             TO CARRIER (NDX).
00343      MOVE BI-GROUPING            TO GROUPING (NDX).
00344      MOVE BI-ACCOUNT             TO ACCOUNT (NDX).
00345      MOVE BI-FIN-RESP            TO FINRESP (NDX).
00346      MOVE BI-BAL-FRWD            TO BALFRWD (NDX).
00347      MOVE BI-PREMIUM             TO PREMIUM (NDX).
00348      MOVE BI-REMITTED            TO REMITTED (NDX).
00349      MOVE BI-TOT-ISS-COMP        TO ISSUE-COMP (NDX).
00350      MOVE BI-TOT-CAN-COMP        TO CANCEL-COMP (NDX).
00351      MOVE BI-ADJUSTMNTS          TO ADJUSTMENTS (NDX).
00352      MOVE BI-DISBURSED           TO DISBURSED (NDX).
00353      MOVE BI-END-BAL             TO END-BAL (NDX).
uktdel*    MOVE BI-FIN-RESP-NAME       TO NAME (NDX).
uktins     MOVE BI-FIN-RESP-NAME       TO NAMEX (NDX).
00355
00356      MOVE BI-CREATION-DT         TO DC-BIN-DATE-1
00357      MOVE SPACE                  TO DC-OPTION-CODE.
00358      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00359      MOVE DC-GREG-DATE-1-EDIT    TO BILL-DATE (NDX).
00360
00361      IF BI-INITIAL-PRINT-DATE NOT = LOW-VALUES
00362          MOVE BI-INITIAL-PRINT-DATE  TO DC-BIN-DATE-1
00363          MOVE SPACE                  TO DC-OPTION-CODE
00364          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00365          MOVE DC-GREG-DATE-1-EDIT    TO PRT-DATE (NDX).
00366
00367  3099-EXIT.
00368      EXIT.
00369      EJECT
00370  4000-START-REPORT-TRANSACTION.
00371      
      * EXEC CICS START
00372 *         TRANSID       (TRANS-6791)
00373 *         FROM          (PROGRAM-INTERFACE-BLOCK)
00374 *         LENGTH        (PI-COMM-LENGTH)
00375 *    END-EXEC.
      *    MOVE '0( LF                 0   #00001698' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 TRANS-6791, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
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
           
00376
00377      MOVE 2559 TO EMI-ERROR.
00378      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00379      GO TO 8200-SEND-DATAONLY.
00380      EJECT
00381  5000-START-BROWSE.
00382      
      * EXEC CICS HANDLE CONDITION
00383 *        NOTFND (5010-REC-NOT-FND)
00384 *        END-EXEC.
      *    MOVE '"$I                   ! # #00001709' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031373039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00385
00386      
      * EXEC CICS STARTBR
00387 *        DATASET(ERBILL-FILE-ID)
00388 *        RIDFLD(ERBILL-KEY)
00389 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001713' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERBILL-FILE-ID, 
                 ERBILL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00390
00391      GO TO 5099-EXIT.
00392
00393  5010-REC-NOT-FND.
00394      MOVE 2212                   TO EMI-ERROR.
00395      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00396  5099-EXIT.
00397      EXIT.
00398      EJECT
00399  5100-READ-NEXT-RECORD.
00400      
      * EXEC CICS HANDLE CONDITION
00401 *        ENDFILE (5110-END-OF-FILE)
00402 *        END-EXEC.
      *    MOVE '"$''                   ! $ #00001727' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031373237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00403
00404      
      * EXEC CICS READNEXT
00405 *        SET (ADDRESS OF BILLING-STATEMENT)
00406 *        DATASET (ERBILL-FILE-ID)
00407 *        RIDFLD (ERBILL-KEY)
00408 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001731' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERBILL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERBILL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BILLING-STATEMENT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00409
00410      IF BI-COMPANY-CD NOT = PI-SAV-CO-CD
00411          GO TO 5110-END-OF-FILE.
00412
00413      IF NOT BI-HEADER-DATA
00414          GO TO 5100-READ-NEXT-RECORD.
00415
00416      IF BI-ACCOUNT = LOW-VALUES
00417          GO TO 5100-READ-NEXT-RECORD.
00418
00419      GO TO 5199-EXIT.
00420
00421  5110-END-OF-FILE.
00422      IF NDX = 2
00423          MOVE 2558               TO EMI-ERROR
00424      ELSE
00425          MOVE 2237               TO EMI-ERROR.
00426
00427      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00428
00429  5199-EXIT.
00430      EXIT.
00431      EJECT
00432  5200-READPREV.
00433      
      * EXEC CICS HANDLE CONDITION
00434 *        ENDFILE (5210-END-OF-FILE)
00435 *        END-EXEC.
      *    MOVE '"$''                   ! % #00001760' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303031373630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00436
00437      
      * EXEC CICS READPREV
00438 *        DATASET (ERBILL-FILE-ID)
00439 *        SET (ADDRESS OF BILLING-STATEMENT)
00440 *        RIDFLD (ERBILL-KEY)
00441 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00001764' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERBILL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERBILL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BILLING-STATEMENT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00442
00443      IF BI-COMPANY-CD NOT = PI-PREV-CO-CD
00444          GO TO 5210-END-OF-FILE.
00445
00446      GO TO 5299-EXIT.
00447
00448  5210-END-OF-FILE.
00449      MOVE 2238                   TO EMI-ERROR
00450      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00451
00452  5299-EXIT.
00453      EXIT.
00454      EJECT
00455  5300-READ-RECORD.
00456      
      * EXEC CICS HANDLE CONDITION
00457 *        NOTFND (5310-NOT-FOUND)
00458 *        END-EXEC.
      *    MOVE '"$I                   ! & #00001783' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303031373833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00459
00460      
      * EXEC CICS READ
00461 *        SET (ADDRESS OF BILLING-STATEMENT)
00462 *        DATASET (ERBILL-FILE-ID)
00463 *        RIDFLD (PI-PREV-ERBILL-KEY)
00464 *        END-EXEC.
      *    MOVE '&"S        E          (   #00001787' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERBILL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-PREV-ERBILL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BILLING-STATEMENT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00465
00466      GO TO 5399-EXIT.
00467
00468  5310-NOT-FOUND.
00469      MOVE 2212                   TO EMI-ERROR
00470      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00471
00472  5399-EXIT.
00473      EXIT.
00474      EJECT
00475  5400-END-BROWSE.
00476      
      * EXEC CICS ENDBR
00477 *        DATASET (ERBILL-FILE-ID)
00478 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001803' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERBILL-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00479
00480  5499-EXIT.
00481      EXIT.
00482      EJECT
00483      EJECT
00484  8100-SEND-INITIAL-MAP.
00485      MOVE SAVE-DATE              TO DATEO.
00486      MOVE EIBTIME                TO TIME-IN.
00487      MOVE TIME-OUT               TO TIMEO.
00488      MOVE -1                     TO PFENTERL.
00489      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
00490
00491      
      * EXEC CICS SEND
00492 *        MAP      (MAP-NAME)
00493 *        MAPSET   (MAPSET-NAME)
00494 *        FROM     (EL679AO)
00495 *        ERASE
00496 *        CURSOR
00497 *        END-EXEC.
           MOVE LENGTH OF
            EL679AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00001818' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL679AO, 
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
           
00498
00499      GO TO 9100-RETURN-TRAN.
00500
00501  8200-SEND-DATAONLY.
00502      IF EIBTRNID NOT = TRANS-ID
00503          GO TO 8100-SEND-INITIAL-MAP.
00504
00505      MOVE SAVE-DATE              TO DATEO.
00506      MOVE EIBTIME                TO TIME-IN.
00507      MOVE TIME-OUT               TO TIMEO.
00508      MOVE -1                     TO PFENTERL.
00509      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
00510
00511      
      * EXEC CICS SEND
00512 *        MAP      (MAP-NAME)
00513 *        MAPSET   (MAPSET-NAME)
00514 *        FROM     (EL679AO)
00515 *        DATAONLY
00516 *        ERASEAUP
00517 *        CURSOR
00518 *        END-EXEC.
           MOVE LENGTH OF
            EL679AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00001838' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL679AO, 
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
           
00519
00520      GO TO 9100-RETURN-TRAN.
00521
00522  8300-SEND-TEXT.
00523      
      * EXEC CICS SEND TEXT
00524 *        FROM     (LOGOFF-TEXT)
00525 *        LENGTH   (LOGOFF-LENGTH)
00526 *        ERASE
00527 *        FREEKB
00528 *        END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001850' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383530' TO DFHEIV0(25:11)
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
           
00529
00530      
      * EXEC CICS RETURN
00531 *        END-EXEC.
      *    MOVE '.(                    &   #00001857' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00532
00533  8400-LOG-JOURNAL-RECORD.
00534      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
00535      MOVE ERBILL-FILE-ID         TO JP-FILE-ID.
00536      MOVE THIS-PGM               TO JP-PROGRAM-ID.
00537
pemuni*    EXEC CICS JOURNAL
pemuni*        JFILEID     (PI-JOURNAL-FILE-ID)
pemuni*        JTYPEID     ('CL')
pemuni*        FROM        (JOURNAL-RECORD)
pemuni*        LENGTH      (773)
pemuni*        END-EXEC.
00544
00545  8500-DATE-CONVERT.
00546      MOVE LINK-ELDATCV           TO PGM-NAME.
00547      
      * EXEC CICS LINK
00548 *        PROGRAM    (PGM-NAME)
00549 *        COMMAREA   (DATE-CONVERSION-DATA)
00550 *        LENGTH     (DC-COMM-LENGTH)
00551 *        END-EXEC.
      *    MOVE '."C                   ''   #00001874' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00552
00553  8500-EXIT.
00554      EXIT.
00555
00556  8800-UNAUTHORIZED-ACCESS.
00557      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
00558      GO TO 8300-SEND-TEXT.
00559
00560  8810-PF23.
00561      MOVE EIBAID                 TO PI-ENTRY-CD-1.
00562      MOVE XCTL-005               TO PGM-NAME.
00563      GO TO 9300-XCTL.
00564
00565  9000-RETURN-CICS.
00566      
      * EXEC CICS RETURN
00567 *        END-EXEC.
      *    MOVE '.(                    &   #00001893' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00568
00569  9100-RETURN-TRAN.
00570      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
00571      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
00572      
      * EXEC CICS RETURN
00573 *        TRANSID    (TRANS-ID)
00574 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
00575 *        LENGTH     (PI-COMM-LENGTH)
00576 *        END-EXEC.
      *    MOVE '.(CT                  &   #00001899' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00577
00578  9200-RETURN-MAIN-MENU.
00579      MOVE XCTL-626               TO PGM-NAME.
00580      GO TO 9300-XCTL.
00581
00582  9300-XCTL.
00583      
      * EXEC CICS XCTL
00584 *        PROGRAM    (PGM-NAME)
00585 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
00586 *        LENGTH     (PI-COMM-LENGTH)
00587 *        END-EXEC.
      *    MOVE '.$C                   $   #00001910' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00588
00589  9400-CLEAR.
00590      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME
00591      GO TO 9300-XCTL.
00592
00593  9500-PF12.
00594      MOVE XCTL-010               TO PGM-NAME.
00595      GO TO 9300-XCTL.
00596
00597  9600-PGMID-ERROR.
00598      
      * EXEC CICS HANDLE CONDITION
00599 *        PGMIDERR    (8300-SEND-TEXT)
00600 *        END-EXEC.
      *    MOVE '"$L                   ! '' #00001925' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303031393235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00601
00602      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
00603      MOVE ' '                    TO PI-ENTRY-CD-1.
00604      MOVE XCTL-005               TO PGM-NAME.
00605      MOVE PGM-NAME               TO LOGOFF-PGM.
00606      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
00607      GO TO 9300-XCTL.
00608
00609  9900-ERROR-FORMAT.
00610      IF NOT EMI-ERRORS-COMPLETE
00611          MOVE LINK-001           TO PGM-NAME
00612          
      * EXEC CICS LINK
00613 *            PROGRAM    (PGM-NAME)
00614 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
00615 *            LENGTH     (EMI-COMM-LENGTH)
00616 *            END-EXEC.
      *    MOVE '."C                   ''   #00001939' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00617  9900-EXIT.
00618      EXIT.
00619
00620  9990-ABEND.
00621      MOVE LINK-004               TO PGM-NAME.
00622      MOVE DFHEIBLK               TO EMI-LINE1
00623
00624      
      * EXEC CICS LINK
00625 *        PROGRAM   (PGM-NAME)
00626 *        COMMAREA  (EMI-LINE1)
00627 *        LENGTH    (72)
00628 *        END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00001951' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00629
00630      GO TO 8200-SEND-DATAONLY.
00631
00632      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL679' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL679' TO DFHEIV1
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
               GO TO 5010-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 5110-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 5210-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 5310-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL679' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
