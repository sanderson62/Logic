00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL013 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:28:44.
00007 *                            VMOD=2.006.
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
00023 *REMARKS.    TRANSACTION - EXRT - RATING CALCULATIONS
00024 *        THIS PROGRAM IS USED TO REVIEW THE RATING
00025 *        CALCULATIONS DONE BY THE SUBROUTINE ELRATE.
00026
00027      EJECT
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL013 WORKING STORAGE     *'.
00033  77  FILLER  PIC X(32)  VALUE '***********VMOD=2.006 **********'.
00034
00035  01  WS-DATE-AREA.
00036      05  SAVE-DATE               PIC X(8)    VALUE SPACES.
00037      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.
00038
00039  01  STANDARD-AREAS.
00040      12  MAP-NAME                PIC X(8)    VALUE 'EL013A'.
00041      12  MAPSET-NAME             PIC X(8)    VALUE 'EL013S'.
00042      12  TRANS-ID                PIC X(4)    VALUE 'EXRT'.
00043      12  PGM-NAME                PIC X(8)    VALUE SPACES.
00044      12  LINK-ELRTRM             PIC X(8)    VALUE 'ELRTRM'.
00045      12  LINK-ELRATE             PIC X(8)    VALUE 'ELRATE'.
00046      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
00047      12  LINK-001                PIC X(8)    VALUE 'EL001'.
00048      12  LINK-004                PIC X(8)    VALUE 'EL004'.
00049      12  TIME-IN                 PIC S9(7).
00050      12  TIME-OUT-R   REDEFINES TIME-IN.
00051          16  FILLER              PIC X.
00052          16  TIME-OUT            PIC 99V99.
00053          16  FILLER              PIC X(2).
00054
00055  01  MISC-WORK-AREAS.
00056      12  WS-COMPANY-CODE         PIC S9(4)   VALUE +0    COMP.
00057      12  WS-COMPANY-CODE-R          REDEFINES WS-COMPANY-CODE.
00058          16  FILLER              PIC X.
00059          16  WS-COMP-CD          PIC X.
00060      12  TEXT-AREA               PIC X(66).
00061      12  TEXT-LENGTH             PIC S9(4)   VALUE +66   COMP.
00062
00063      12  WS-EDIT-BEN-CODE        PIC XX.
00064          88  INVALID-BENEFIT-CODE   VALUE '  ' '00'
00065                                           '90' THRU '99'.
00066
00067      12  ER-0349                 PIC X(4)   VALUE '0349'.
00068      12  ER-2474                 PIC X(4)   VALUE '2474'.
00069      12  ER-2477                 PIC X(4)   VALUE '2477'.
00070      12  ER-2481                 PIC X(4)   VALUE '2481'.
00071      12  ER-2482                 PIC X(4)   VALUE '2482'.
00072      12  ER-2484                 PIC X(4)   VALUE '2484'.
00073      12  ER-2485                 PIC X(4)   VALUE '2485'.
00074      12  ER-2486                 PIC X(4)   VALUE '2486'.
00075      12  ER-2487                 PIC X(4)   VALUE '2487'.
00076      12  ER-2488                 PIC X(4)   VALUE '2488'.
00077      12  ER-2489                 PIC X(4)   VALUE '2489'.
00078      12  ER-2490                 PIC X(4)   VALUE '2490'.
00079      12  ER-2496                 PIC X(4)   VALUE '2496'.
00080      12  ER-2550                 PIC X(4)   VALUE '2550'.
00081      12  ER-2551                 PIC X(4)   VALUE '2551'.
00082      12  ER-2552                 PIC X(4)   VALUE '2552'.
00083      12  ER-2553                 PIC X(4)   VALUE '2553'.
00084      12  ER-2554                 PIC X(4)   VALUE '2554'.
00085      12  ER-2555                 PIC X(4)   VALUE '2555'.
00086      12  ER-2556                 PIC X(4)   VALUE '2556'.
00087      12  ER-2557                 PIC X(4)   VALUE '2557'.
00088      12  ER-3032                 PIC X(4)   VALUE '3032'.
00089      12  ER-3126                 PIC X(4)   VALUE '3126'.
00090
00091  01  TEXT-MESSAGES.
00092      12  TRAN-COMPLETE-MSG.
00093          16  FILLER              PIC X(45)
00094            VALUE '     CLEAR ENTERED - SESSION ENDED'.
00095      EJECT
00096 *                                COPY ELCDATE.
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
00097      EJECT
00098 *                                COPY ELCATTR.
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
00099      EJECT
00100 *                                COPY ELCEMIB.
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
00101      EJECT
00102 *                                COPY ELCCALC.
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
00103      EJECT
00104 *                                COPY ELCAID.
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
00105      EJECT
00106 *                                COPY EL013S.
       01  EL013AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDATEL PIC S9(0004) COMP.
           05  RUNDATEF PIC  X(0001).
           05  FILLER REDEFINES RUNDATEF.
               10  RUNDATEA PIC  X(0001).
           05  RUNDATEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  ORIGTRML PIC S9(0004) COMP.
           05  ORIGTRMF PIC  X(0001).
           05  FILLER REDEFINES ORIGTRMF.
               10  ORIGTRMA PIC  X(0001).
           05  ORIGTRMI PIC  S9(3).
      *    -------------------------------
           05  ERRCODEL PIC S9(0004) COMP.
           05  ERRCODEF PIC  X(0001).
           05  FILLER REDEFINES ERRCODEF.
               10  ERRCODEA PIC  X(0001).
           05  ERRCODEI PIC  X(0001).
      *    -------------------------------
           05  CLASSCDL PIC S9(0004) COMP.
           05  CLASSCDF PIC  X(0001).
           05  FILLER REDEFINES CLASSCDF.
               10  CLASSCDA PIC  X(0001).
           05  CLASSCDI PIC  X(0002).
      *    -------------------------------
           05  DEVCODEL PIC S9(0004) COMP.
           05  DEVCODEF PIC  X(0001).
           05  FILLER REDEFINES DEVCODEF.
               10  DEVCODEA PIC  X(0001).
           05  DEVCODEI PIC  X(0003).
      *    -------------------------------
           05  RATEL PIC S9(0004) COMP.
           05  RATEF PIC  X(0001).
           05  FILLER REDEFINES RATEF.
               10  RATEA PIC  X(0001).
           05  RATEI PIC  X(0008).
      *    -------------------------------
           05  ORIGBENL PIC S9(0004) COMP.
           05  ORIGBENF PIC  X(0001).
           05  FILLER REDEFINES ORIGBENF.
               10  ORIGBENA PIC  X(0001).
           05  ORIGBENI PIC  S9(9)V99.
      *    -------------------------------
           05  PREMIUML PIC S9(0004) COMP.
           05  PREMIUMF PIC  X(0001).
           05  FILLER REDEFINES PREMIUMF.
               10  PREMIUMA PIC  X(0001).
           05  PREMIUMI PIC  X(0010).
      *    -------------------------------
           05  ORIGPRML PIC S9(0004) COMP.
           05  ORIGPRMF PIC  X(0001).
           05  FILLER REDEFINES ORIGPRMF.
               10  ORIGPRMA PIC  X(0001).
           05  ORIGPRMI PIC  S9(7)V99.
      *    -------------------------------
           05  FACTORL PIC S9(0004) COMP.
           05  FACTORF PIC  X(0001).
           05  FILLER REDEFINES FACTORF.
               10  FACTORA PIC  X(0001).
           05  FACTORI PIC  X(0008).
      *    -------------------------------
           05  BENTYPEL PIC S9(0004) COMP.
           05  BENTYPEF PIC  X(0001).
           05  FILLER REDEFINES BENTYPEF.
               10  BENTYPEA PIC  X(0001).
           05  BENTYPEI PIC  X(0001).
      *    -------------------------------
           05  BENCODEL PIC S9(0004) COMP.
           05  BENCODEF PIC  X(0001).
           05  FILLER REDEFINES BENCODEF.
               10  BENCODEA PIC  X(0001).
           05  BENCODEI PIC  X(0002).
      *    -------------------------------
           05  STCODEL PIC S9(0004) COMP.
           05  STCODEF PIC  X(0001).
           05  FILLER REDEFINES STCODEF.
               10  STCODEA PIC  X(0001).
           05  STCODEI PIC  X(0002).
      *    -------------------------------
           05  STABBRL PIC S9(0004) COMP.
           05  STABBRF PIC  X(0001).
           05  FILLER REDEFINES STABBRF.
               10  STABBRA PIC  X(0001).
           05  STABBRI PIC  X(0002).
      *    -------------------------------
           05  COMPIDL PIC S9(0004) COMP.
           05  COMPIDF PIC  X(0001).
           05  FILLER REDEFINES COMPIDF.
               10  COMPIDA PIC  X(0001).
           05  COMPIDI PIC  X(0003).
      *    -------------------------------
           05  COMPCDL PIC S9(0004) COMP.
           05  COMPCDF PIC  X(0001).
           05  FILLER REDEFINES COMPCDF.
               10  COMPCDA PIC  X(0001).
           05  COMPCDI PIC  9(3).
      *    -------------------------------
           05  ISSAGEL PIC S9(0004) COMP.
           05  ISSAGEF PIC  X(0001).
           05  FILLER REDEFINES ISSAGEF.
               10  ISSAGEA PIC  X(0001).
           05  ISSAGEI PIC  X(0002).
      *    -------------------------------
           05  RATMETHL PIC S9(0004) COMP.
           05  RATMETHF PIC  X(0001).
           05  FILLER REDEFINES RATMETHF.
               10  RATMETHA PIC  X(0001).
           05  RATMETHI PIC  X(0001).
      *    -------------------------------
           05  CALCODEL PIC S9(0004) COMP.
           05  CALCODEF PIC  X(0001).
           05  FILLER REDEFINES CALCODEF.
               10  CALCODEA PIC  X(0001).
           05  CALCODEI PIC  X(0001).
      *    -------------------------------
           05  APRL PIC S9(0004) COMP.
           05  APRF PIC  X(0001).
           05  FILLER REDEFINES APRF.
               10  APRA PIC  X(0001).
           05  APRI PIC  S9(3)V9(4).
      *    -------------------------------
           05  CAPTERML PIC S9(0004) COMP.
           05  CAPTERMF PIC  X(0001).
           05  FILLER REDEFINES CAPTERMF.
               10  CAPTERMA PIC  X(0001).
           05  CAPTERMI PIC  S9(3).
      *    -------------------------------
           05  PAYFREQL PIC S9(0004) COMP.
           05  PAYFREQF PIC  X(0001).
           05  FILLER REDEFINES PAYFREQF.
               10  PAYFREQA PIC  X(0001).
           05  PAYFREQI PIC  X(0002).
      *    -------------------------------
           05  CERTISSL PIC S9(0004) COMP.
           05  CERTISSF PIC  X(0001).
           05  FILLER REDEFINES CERTISSF.
               10  CERTISSA PIC  X(0001).
           05  CERTISSI PIC  X(0006).
      *    -------------------------------
           05  FSTPMTL PIC S9(0004) COMP.
           05  FSTPMTF PIC  X(0001).
           05  FILLER REDEFINES FSTPMTF.
               10  FSTPMTA PIC  X(0001).
           05  FSTPMTI PIC  X(0006).
      *    -------------------------------
           05  EXTDAYL PIC S9(0004) COMP.
           05  EXTDAYF PIC  X(0001).
           05  FILLER REDEFINES EXTDAYF.
               10  EXTDAYA PIC  X(0001).
           05  EXTDAYI PIC  X(0003).
      *    -------------------------------
           05  DEVPCTL PIC S9(0004) COMP.
           05  DEVPCTF PIC  X(0001).
           05  FILLER REDEFINES DEVPCTF.
               10  DEVPCTA PIC  X(0001).
           05  DEVPCTI PIC  S99V9(6).
      *    -------------------------------
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  ERRMSGI PIC  X(0072).
       01  EL013AO REDEFINES EL013AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ORIGTRMO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRCODEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLASSCDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEVCODEO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATEO PIC  ZZ.9(5).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ORIGBENO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PREMIUMO PIC  Z(7).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ORIGPRMO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACTORO PIC  ZZ.9(5).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENCODEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STCODEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STABBRO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ISSAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATMETHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CALCODEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APRO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAPTERMO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYFREQO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTISSO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FSTPMTO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTDAYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEVPCTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0072).
      *    -------------------------------
00107      EJECT
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
00109  01  DFHCOMMAREA                 PIC X(450).
00110      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL013' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00112      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00113      MOVE '5'                    TO DC-OPTION-CODE.
00114      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00115      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00116      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00117
00118      IF EIBAID = DFHCLEAR
00119          GO TO 8950-CLEAR-RETURN.
00120
00121      MOVE LOW-VALUES             TO EL013AO.
00122
00123      IF EIBCALEN NOT GREATER THAN ZERO
00124          GO TO 8100-SEND-INITIAL-MAP.
00125
00126      MOVE DFHCOMMAREA            TO CALCULATION-PASS-AREA.
00127
00128      
      * EXEC CICS RECEIVE
00129 *        MAP     (MAP-NAME)
00130 *        MAPSET  (MAPSET-NAME)
00131 *        INTO    (EL013AI)
00132 *    END-EXEC.
           MOVE LENGTH OF
            EL013AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001399' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL013AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00133
00134      EJECT
00135  1000-MOVE-DATA-TO-PASS-AREA.
00136      IF ORIGTRML NOT = ZEROS
00137          IF ORIGTRMI NUMERIC
00138              IF ORIGTRMI GREATER 0 AND LESS 361
00139                  MOVE ORIGTRMI   TO CP-ORIGINAL-TERM
00140                  MOVE AL-UANON   TO ORIGTRMA
00141              ELSE
00142                  MOVE ER-2550    TO EMI-ERROR
00143                  MOVE -1         TO ORIGTRML
00144                  MOVE AL-UNBON   TO ORIGTRMA
00145                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00146          ELSE
00147              MOVE ER-2496        TO EMI-ERROR
00148              MOVE -1             TO ORIGTRML
00149              MOVE AL-UNBON       TO ORIGTRMA
00150              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00151      ELSE
00152          MOVE ER-2477            TO EMI-ERROR
00153          MOVE -1                 TO ORIGTRML
00154          MOVE AL-UNBON           TO ORIGTRMA
00155          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00156
00157      IF CLASSCDL NOT = ZEROS
00158          MOVE CLASSCDI           TO CP-CLASS-CODE
00159          MOVE AL-UANON           TO CLASSCDA
00160      ELSE
00161          MOVE ER-2551            TO EMI-ERROR
00162          MOVE -1                 TO CLASSCDL
00163          MOVE AL-UABON           TO CLASSCDA
00164          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00165
00166      IF DEVCODEL NOT = ZEROS
00167          MOVE DEVCODEI           TO CP-DEVIATION-CODE
00168          MOVE AL-UANON           TO DEVCODEA
00169      ELSE
00170          MOVE ER-2552            TO EMI-ERROR
00171          MOVE -1                 TO DEVCODEL
00172          MOVE AL-UABON           TO DEVCODEA
00173          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00174
00175      IF ORIGBENL NOT = ZEROS
00176          IF ORIGBENI NUMERIC
00177              MOVE ORIGBENI       TO CP-ORIGINAL-BENEFIT
00178              MOVE AL-UNNON       TO ORIGBENA
00179          ELSE
00180              MOVE ER-2485        TO EMI-ERROR
00181              MOVE -1             TO ORIGBENL
00182              MOVE AL-UNBON       TO ORIGBENA
00183              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00184      ELSE
00185          MOVE ER-2553            TO EMI-ERROR
00186          MOVE -1                 TO ORIGBENL
00187          MOVE AL-UNBON           TO ORIGBENA
00188          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00189
00190      IF ORIGPRML NOT = ZEROS
00191          IF ORIGPRMI NUMERIC
00192              MOVE ORIGPRMI       TO CP-ORIGINAL-PREMIUM
00193              MOVE AL-UNNON       TO ORIGPRMA
00194          ELSE
00195              MOVE ER-2484        TO EMI-ERROR
00196              MOVE -1             TO ORIGPRML
00197              MOVE AL-UNBON       TO ORIGPRMA
00198              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00199      ELSE
00200          MOVE ZEROS              TO CP-ORIGINAL-PREMIUM.
00201
00202      IF BENTYPEI = 'L' OR 'A'
00203          MOVE BENTYPEI           TO CP-BENEFIT-TYPE
00204          MOVE AL-UANON           TO BENTYPEA
00205          IF BENTYPEI IS EQUAL TO 'L'
00206              MOVE BENTYPEI       TO CP-LIFE-OVERRIDE-CODE
00207          ELSE
00208              MOVE BENTYPEI       TO CP-AH-OVERRIDE-CODE
00209      ELSE
00210          MOVE ER-2481            TO EMI-ERROR
00211          MOVE -1                 TO BENTYPEL
00212          MOVE AL-UABON           TO BENTYPEA
00213          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00214
00215      IF BENCODEL NOT = ZEROS
00216          MOVE BENCODEI           TO WS-EDIT-BEN-CODE
00217          IF NOT INVALID-BENEFIT-CODE
00218              MOVE BENCODEI       TO CP-BENEFIT-CD
00219              MOVE AL-UANON       TO BENCODEA
00220          ELSE
00221              MOVE ER-2489        TO EMI-ERROR
00222              MOVE -1             TO BENCODEL
00223              MOVE AL-UABON       TO BENCODEA
00224              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00225      ELSE
00226          MOVE ER-2554            TO EMI-ERROR
00227          MOVE -1                 TO BENCODEL
00228          MOVE AL-UABON           TO BENCODEA
00229          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00230
00231      IF STCODEL NOT = ZEROS
00232          MOVE STCODEI            TO CP-STATE
00233          MOVE AL-UNNON           TO STCODEA
00234      ELSE
00235          MOVE ER-2555            TO EMI-ERROR
00236          MOVE -1                 TO STCODEL
00237          MOVE AL-UNBON           TO STCODEA
00238          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00239
00240      IF STABBRL NOT = ZEROS
00241          MOVE STABBRI            TO CP-STATE-STD-ABBRV
00242          MOVE AL-UANON           TO STABBRA.
00243
00244      IF COMPIDL NOT = ZEROS
00245          MOVE COMPIDI            TO CP-COMPANY-ID
00246          MOVE AL-UANON           TO COMPIDA.
00247
00248      IF COMPCDL NOT = ZEROS
00249          IF COMPCDI NUMERIC
00250              MOVE COMPCDI        TO WS-COMPANY-CODE
00251              MOVE WS-COMP-CD     TO CP-COMPANY-CD
00252              MOVE AL-UANON       TO COMPCDA
00253          ELSE
00254              MOVE ER-2488        TO EMI-ERROR
00255              MOVE -1             TO COMPCDL
00256              MOVE AL-UNBON       TO COMPCDA
00257              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00258      ELSE
00259          MOVE ER-2556            TO EMI-ERROR
00260          MOVE -1                 TO COMPCDL
00261          MOVE AL-UNBON           TO COMPCDA
00262          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00263
00264      IF ISSAGEL NOT = ZEROS
00265          IF ISSAGEI NUMERIC
00266              MOVE ISSAGEI        TO CP-ISSUE-AGE
00267              MOVE AL-UNNON       TO ISSAGEA
00268          ELSE
00269              MOVE ER-2490        TO EMI-ERROR
00270              MOVE -1             TO ISSAGEL
00271              MOVE AL-UNBON       TO ISSAGEA
00272              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00273      ELSE
00274          MOVE ER-2557            TO EMI-ERROR
00275          MOVE -1                 TO ISSAGEL
00276          MOVE AL-UNBON           TO ISSAGEA
00277          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00278
00279      IF RATMETHL NOT = ZEROS
00280          MOVE RATMETHI           TO CP-EARNING-METHOD
00281          MOVE AL-UANON           TO RATMETHA.
00282
00283      IF CALCODEL NOT = ZEROS
00284          MOVE CALCODEI           TO CP-SPECIAL-CALC-CD
00285          MOVE AL-UANON           TO CALCODEA.
00286
00287      IF APRL NOT = ZEROS
00288          IF APRI NUMERIC
00289              MOVE APRI           TO CP-LOAN-APR
00290              MOVE AL-UNNON       TO APRA
00291          ELSE
00292              MOVE ER-2487        TO EMI-ERROR
00293              MOVE -1             TO APRL
00294              MOVE AL-UNBON       TO APRA
00295              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00296
00297      IF CP-EARN-AS-NET-PAY  AND  CP-TRUNCATED-LIFE
00298          IF CAPTERML NOT = ZEROS
00299              IF CAPTERMI NUMERIC
00300                  IF CAPTERMI GREATER THAN 0 AND LESS THAN 361
00301                      MOVE CAPTERMI   TO CP-LOAN-TERM
00302                      MOVE AL-UANON   TO CAPTERMA
00303                  ELSE
00304                      MOVE ER-2550    TO EMI-ERROR
00305                      MOVE -1         TO CAPTERML
00306                      MOVE AL-UNBON   TO CAPTERMA
00307                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00308              ELSE
00309                  MOVE ER-2496        TO EMI-ERROR
00310                  MOVE -1             TO CAPTERML
00311                  MOVE AL-UNBON       TO CAPTERMA
00312                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00313          ELSE
00314              MOVE ER-2477            TO EMI-ERROR
00315              MOVE -1                 TO CAPTERML
00316              MOVE AL-UNBON           TO CAPTERMA
00317              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00318      ELSE
00319          MOVE ZEROS              TO CP-LOAN-TERM
00320          MOVE AL-UANON           TO CAPTERMA.
00321
00322      IF PAYFREQL NOT = ZEROS
00323          MOVE PAYFREQI           TO CP-PAY-FREQUENCY
00324          MOVE AL-UANON           TO PAYFREQA.
00325
00326      IF CERTISSL NOT = ZEROS
00327          IF CERTISSI NUMERIC
00328              MOVE CERTISSI       TO DC-GREG-DATE-1-MDY
00329              MOVE 4              TO DC-OPTION-CODE
00330              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00331              IF NO-CONVERSION-ERROR
00332                  MOVE DC-BIN-DATE-1  TO CP-CERT-EFF-DT
00333                  MOVE AL-UNNON   TO CERTISSA
00334              ELSE
00335                  MOVE ER-2474    TO EMI-ERROR
00336                  MOVE -1         TO CERTISSL
00337                  MOVE AL-UNBON   TO CERTISSA
00338                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00339          ELSE
00340              MOVE ER-2482        TO EMI-ERROR
00341              MOVE -1             TO CERTISSL
00342              MOVE AL-UNBON       TO CERTISSA
00343              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00344      ELSE
00345          MOVE EIBDATE            TO DC-JULIAN-YYDDD
00346          MOVE 5                  TO DC-OPTION-CODE
00347          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00348          MOVE DC-BIN-DATE-1      TO CP-CERT-EFF-DT.
00349
00350      IF FSTPMTL NOT = ZEROS
00351          IF FSTPMTI NUMERIC
00352              MOVE FSTPMTI        TO DC-GREG-DATE-1-MDY
00353              MOVE 4              TO DC-OPTION-CODE
00354              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00355              IF NO-CONVERSION-ERROR
00356                  MOVE DC-BIN-DATE-1  TO CP-FIRST-PAY-DATE
00357                  MOVE AL-UNNON   TO FSTPMTA
00358              ELSE
00359                  MOVE ER-0349    TO EMI-ERROR
00360                  MOVE -1         TO FSTPMTL
00361                  MOVE AL-UNBON   TO FSTPMTA
00362                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00363          ELSE
00364              MOVE ER-0349        TO EMI-ERROR
00365              MOVE -1             TO FSTPMTL
00366              MOVE AL-UNBON       TO FSTPMTA
00367              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00368      ELSE
00369          MOVE EIBDATE            TO DC-JULIAN-YYDDD
00370          MOVE 5                  TO DC-OPTION-CODE
00371          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00372          MOVE DC-BIN-DATE-1      TO CP-CERT-EFF-DT.
00373
00374      IF EXTDAYL NOT = ZEROS
00375          IF EXTDAYI NUMERIC
00376              MOVE EXTDAYI        TO CP-TERM-OR-EXT-DAYS
00377              MOVE AL-UNNON       TO EXTDAYA
00378          ELSE
00379              MOVE ER-3032        TO EMI-ERROR
00380              MOVE -1             TO EXTDAYL
00381              MOVE AL-UNBON       TO EXTDAYA
00382              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00383
00384      IF DEVPCTL NOT = ZEROS
00385          IF DEVPCTI NUMERIC
00386              MOVE DEVPCTI        TO CP-RATE-DEV-PCT
00387              MOVE AL-UNNON       TO DEVPCTA
00388          ELSE
00389              MOVE ER-3126        TO EMI-ERROR
00390              MOVE -1             TO DEVPCTL
00391              MOVE AL-UNBON       TO DEVPCTA
00392              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00393
00394      IF EMI-ERROR NOT = ZEROS
00395          GO TO 8200-SEND-DATAONLY.
00396
00397      IF COMPIDI IS EQUAL TO 'NCB'
00398          IF STABBRI IS EQUAL TO 'NC'
00399              IF RATMETHI IS EQUAL TO '5'
00400                  COMPUTE CP-ORIGINAL-BENEFIT =
00401                      CP-ORIGINAL-BENEFIT - CP-ORIGINAL-PREMIUM.
00402
00403      MOVE LINK-ELRATE            TO PGM-NAME.
00404
00405      PERFORM 9700-LINK THRU 9700-EXIT.
00406
00407      EJECT
00408  2000-FORMAT-RESULTS.
00409      IF CP-RETURN-CODE = ZERO
00410          MOVE CP-CALC-PREMIUM    TO PREMIUMO
00411          MOVE CP-CDT-FACTOR      TO FACTORO
00412          MOVE CP-PREMIUM-RATE    TO RATEO
00413      ELSE
00414          MOVE CP-RETURN-CODE     TO ERRCODEO.
00415
00416      EJECT
00417  8100-SEND-INITIAL-MAP.
00418      MOVE SAVE-DATE              TO RUNDATEO.
00419      MOVE EIBTIME                TO TIME-IN.
00420      MOVE TIME-OUT               TO RUNTIMEO.
00421      MOVE -1                     TO ORIGTRML.
00422      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
00423
00424      
      * EXEC CICS SEND
00425 *        MAP    (MAP-NAME)
00426 *        MAPSET (MAPSET-NAME)
00427 *        FROM   (EL013AO)
00428 *        ERASE
00429 *        CURSOR
00430 *    END-EXEC.
           MOVE LENGTH OF
            EL013AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00001695' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL013AO, 
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
           
00431
00432      GO TO 9100-RETURN-TRAN.
00433
00434  8200-SEND-DATAONLY.
00435      MOVE SAVE-DATE              TO RUNDATEO.
00436      MOVE EIBTIME                TO TIME-IN.
00437      MOVE TIME-OUT               TO RUNTIMEO.
00438      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
00439
00440      
      * EXEC CICS SEND
00441 *        MAP    (MAP-NAME)
00442 *        MAPSET (MAPSET-NAME)
00443 *        FROM   (EL013AO)
00444 *        DATAONLY
00445 *        CURSOR
00446 *    END-EXEC.
           MOVE LENGTH OF
            EL013AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00001711' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL013AO, 
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
           
00447
00448      MOVE LOW-VALUES             TO EL013AO.
00449
00450      GO TO 9100-RETURN-TRAN.
00451      EJECT
00452  8500-DATE-CONVERT.
00453      MOVE LINK-ELDATCV           TO PGM-NAME.
00454
00455      
      * EXEC CICS LINK
00456 *        PROGRAM    (PGM-NAME)
00457 *        COMMAREA   (DATE-CONVERSION-DATA)
00458 *        LENGTH     (DC-COMM-LENGTH)
00459 *    END-EXEC.
      *    MOVE '."C                   ''   #00001726' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00460
00461  8500-EXIT.
00462      EXIT.
00463
00464      EJECT
00465  8950-CLEAR-RETURN.
00466      MOVE TRAN-COMPLETE-MSG      TO TEXT-AREA.
00467
00468  8990-SEND-TEXT.
00469      
      * EXEC CICS SEND TEXT
00470 *        FROM    (TEXT-AREA)
00471 *        LENGTH  (TEXT-LENGTH)
00472 *        ERASE
00473 *        FREEKB
00474 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001740' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEXT-AREA, 
                 TEXT-LENGTH, 
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
           
00475
00476  9000-RETURN-CICS.
00477      
      * EXEC CICS RETURN
00478 *    END-EXEC.
      *    MOVE '.(                    &   #00001748' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00479
00480
00481
00482  9100-RETURN-TRAN.
00483      
      * EXEC CICS RETURN
00484 *        TRANSID   (TRANS-ID)
00485 *        COMMAREA  (CALCULATION-PASS-AREA)
00486 *        LENGTH    (CP-COMM-LENGTH)
00487 *    END-EXEC.
      *    MOVE '.(CT                  &   #00001754' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00488
00489  9700-LINK.
00490      
      * EXEC CICS LINK
00491 *        PROGRAM   (PGM-NAME)
00492 *        COMMAREA  (CALCULATION-PASS-AREA)
00493 *        LENGTH    (CP-COMM-LENGTH)
00494 *    END-EXEC.
      *    MOVE '."C                   ''   #00001761' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00495
00496  9700-EXIT.
00497       EXIT.
00498
00499      EJECT
00500  9900-ERROR-FORMAT.
00501      IF NOT EMI-ERRORS-COMPLETE
00502          MOVE LINK-001           TO PGM-NAME
00503          
      * EXEC CICS LINK
00504 *            PROGRAM   (PGM-NAME)
00505 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
00506 *            LENGTH    (EMI-COMM-LENGTH)
00507 *        END-EXEC.
      *    MOVE '."C                   ''   #00001774' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00508
00509  9900-EXIT.
00510      EXIT.
00511
00512  9990-ABEND.
00513      MOVE LINK-004               TO PGM-NAME.
00514      MOVE DFHEIBLK               TO EMI-LINE1.
00515
00516      
      * EXEC CICS LINK
00517 *        PROGRAM   (PGM-NAME)
00518 *        COMMAREA  (EMI-LINE1)
00519 *        LENGTH    (72)
00520 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00001787' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00521
00522      GO TO 8200-SEND-DATAONLY.
00523      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL013' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL013' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL013' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
