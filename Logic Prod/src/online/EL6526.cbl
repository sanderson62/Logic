       ID DIVISION.
       PROGRAM-ID. EL6526.
      *AUTHOR.     PABLO.
      *            COLLEYVILLE, TEXAS.
      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *                                                                *
      *            *   OF    CSO      IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      *
      *REMARKS.  TRANSACTION - EXDG - RDS EMAIL ADDRESS DISPLAY ONLY
      *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
022211* 022211    2010012100001  PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*    EL6526 WORKING STORAGE    *'.
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'.
       01  WS-DATE-AREA.
           05  WS-SAVE-DATE                PIC X(8) VALUE SPACES.
           05  WS-SAVE-BIN-DT              PIC XX   VALUE SPACES.
           05  WS-EFF-YMD                  PIC X(6).
           05  WS-TRM-YMD                  PIC X(6).
           05  SUPPRESS-MAP-SW             PIC X    VALUE SPACE.
               88  DO-NOT-MOVE-TO-MAP          VALUE 'N'.
               88  MOVE-TO-MAP                 VALUE 'Y'.
           05  MAP-CHANGED-SW              PIC X    VALUE 'N'.
               88  MAP-NOT-CHANGED             VALUE 'N'.
               88  MAP-CHANGED                 VALUE 'Y'.
           05  BILL-INST-SW                PIC X.
               88  BILL-INST-CHANGED           VALUE 'Y'.
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.
       01  EADR-KEY.
           05  EADR-COMPANY-CD                   PIC X.
           05  EADR-REC-TYPE                     PIC XX.
           05  EADR-ERCOMP-KEY.
               10  EADR-CARRIER                  PIC X.
               10  EADR-GROUPING                 PIC X(6).
               10  EADR-FIN-RESP                 PIC X(10).
               10  EADR-ACCOUNT                  PIC X(10).
               10  EADR-TYPE                     PIC X.
               10  FILLER                        PIC X(17).
       01  STANDARD-AREAS.
           12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1024.
           12  WS-SUB                      PIC S9(4) COMP VALUE +0.
           12  MAP-NAME                    PIC X(8) VALUE 'EL6526A'.
           12  MAPSET-NAME                 PIC X(8) VALUE 'EL6526S'.
           12  SCREEN-NUMBER               PIC X(4) VALUE '652G'.
           12  TRANS-ID                    PIC X(4) VALUE 'EXDG'.
           12  EL611-TRANS-ID              PIC X(4) VALUE 'EXL3'.
           12  THIS-PGM                    PIC X(8) VALUE 'EL6526'.
           12  PGM-NAME                    PIC X(8).
           12  TIME-IN                     PIC S9(7).
           12  TIME-OUT-R  REDEFINES TIME-IN.
               16  FILLER                  PIC X.
               16  TIME-OUT                PIC 99V99.
               16  FILLER                  PIC XX.
           12  XCTL-005                    PIC X(8) VALUE 'EL005'.
           12  XCTL-010                    PIC X(8) VALUE 'EL010'.
           12  XCTL-626                    PIC X(8) VALUE 'EL626'.
           12  XCTL-611                    PIC X(8) VALUE 'EL611'.
           12  XCTL-652                    PIC X(8) VALUE 'EL652'.
           12  LINK-001                    PIC X(8) VALUE 'EL001'.
           12  LINK-004                    PIC X(8) VALUE 'EL004'.
           12  QID.
               16  QID-TERM                PIC X(4) VALUE SPACES.
               16  FILLER                  PIC X(4) VALUE '526A'.
           12  MAP-LENGTH                  PIC S9(4) VALUE +600  COMP.
           12  EREADR-FILE-ID              PIC X(8) VALUE 'EREADR'.
           12  ERCOMP-FILE-ID              PIC X(8) VALUE 'ERCOMP'.
           12  DEEDIT-FIELD                PIC X(15).
           12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).
           12  WS-BILL-INST-SW            PIC X  VALUE SPACES.
               88  BILLING-INSTRUCTIONS-FOUND  VALUE 'Y'.
           12  ERROR-MESSAGES.
               16  ER-0000                 PIC X(4) VALUE '0000'.
               16  ER-0004                 PIC X(4) VALUE '0004'.
               16  ER-0008                 PIC X(4) VALUE '0008'.
               16  ER-0029                 PIC X(4) VALUE '0029'.
               16  ER-0068                 PIC X(4) VALUE '0068'.
               16  ER-0070                 PIC X(4) VALUE '0070'.
               16  ER-0348                 PIC X(4) VALUE '0348'.
               16  ER-0454                 PIC X(4) VALUE '0454'.
               16  ER-0876                 PIC X(4) VALUE '0876'.
               16  ER-1228                 PIC X(4) VALUE '1228'.
               16  ER-1626                 PIC X(4) VALUE '1626'.
               16  ER-1629                 PIC X(4) VALUE '1629'.
               16  ER-2039                 PIC X(4) VALUE '2039'.
               16  ER-2056                 PIC X(4) VALUE '2056'.
               16  ER-2057                 PIC X(4) VALUE '2057'.
               16  ER-2233                 PIC X(4) VALUE '2233'.
               16  ER-7430                 PIC X(4) VALUE '7430'.
               16  ER-7431                 PIC X(4) VALUE '7431'.
               16  ER-7432                 PIC X(4) VALUE '7432'.
               16  ER-7434                 PIC X(4) VALUE '7434'.
               16  ER-7435                 PIC X(4) VALUE '7435'.
               16  ER-7436                 PIC X(4) VALUE '7436'.
               16  ER-7438                 PIC X(4) VALUE '7438'.
               16  ER-7440                 PIC X(4) VALUE '7440'.
               16  ER-7447                 PIC X(4) VALUE '7447'.
               16  ER-7449                 PIC X(4) VALUE '7449'.
               16  ER-7462                 PIC X(4) VALUE '7462'.
               16  ER-7465                 PIC X(4) VALUE '7465'.
               16  ER-7468                 PIC X(4) VALUE '7468'.
               16  ER-7469                 PIC X(4) VALUE '7469'.
               16  ER-8799                 PIC X(4) VALUE '8799'.
               16  ER-9388                 PIC X(4) VALUE '9388'.
               16  ER-9399                 PIC X(4) VALUE '9399'.
               16  ER-9999                 PIC X(4) VALUE '9999'.
           12  ELCNTL-KEY.
               16  CNTL-COMP-ID            PIC X(3) VALUE SPACES.
               16  CNTL-REC-TYPE           PIC X    VALUE SPACES.
               16  CNTL-ACCESS             PIC X(4) VALUE SPACES.
               16  CNTL-SEQ-NO             PIC S9(4) VALUE +0  COMP.
      *    COPY ELCLOGOF.
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
      *    COPY ELCDATE.
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
      *    COPY ELCATTR.
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
      *    COPY ELCEMIB.
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
      *    COPY ELCSCTM.
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
      *    COPY ELCSCRTY.
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
      *    COPY ELCINTF.
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
           12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.
               16  PI-MAINT                PIC  X.
               16  PI-CHECK-TYPE           PIC  X.
               16  PI-CHECK-CARRY-BAL      PIC  X.
               16  PI-FIRST-TIME-SW        PIC  X.
                   88  FIRST-TIME                  VALUE 'Y'.
               16  PI-ERCOMP-EOF-SW        PIC  X.
                   88  ERCOMP-EOF                  VALUE 'Y'.
               16  PI-SAVE-PHONE           PIC  X(10).
               16  PI-SAVE-PHONE-RED REDEFINES PI-SAVE-PHONE  PIC 9(10).
               16  PI-ERC-END-BAL          PIC S9(9)V99       COMP-3.
               16  PI-ERCOMP-KEY.
                   20  PI-ERC-GROUP-CD     PIC  X.
                   20  PI-ERC-CARRIER      PIC  X.
                   20  PI-ERC-GROUP        PIC  X(6).
                   20  PI-ERC-RESP         PIC  X(10).
                   20  PI-ERC-ACCT         PIC  X(10).
                   20  PI-ERC-TYPE         PIC  X.
               16  PI-SAVE-ERCOMP-KEY      PIC  X(29).
               16  PI-BANK-TRANSIT-NUMBER.
                   20  PI-BANK-COMPANY-CD  PIC X.
                   20  PI-FEDERAL-NUMBER   PIC X(4).
                   20  PI-BANK-NUMBER      PIC X(4).
               16  PI-BANK-ACCOUNT-NO      PIC X(17).
               16  PI-BANK-ACTION-CODE     PIC X.
               16  PI-ERCOBI-KEY.
                   20  PI-ERCOBI-COMPANY-CD PIC X.
                   20  PI-ERCOBI-STMT-OWNER PIC X(4).
                   20  PI-ERCOBI-RGID      PIC X(12).
               16  PI-SAVE-ERCOBI-KEY      PIC X(17).
               16  FILLER                  PIC  X(500).
      *    COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  '?'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
       01  FILLER    REDEFINES DFHAID.
           12  FILLER                      PIC X(8).
           12  PF-VALUES                   PIC X       OCCURS 2.
      *                                COPY EL6526S.
       01  EL6526AI.
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
           05  CARRIERL PIC S9(0004) COMP.
           05  CARRIERF PIC  X(0001).
           05  FILLER REDEFINES CARRIERF.
               10  CARRIERA PIC  X(0001).
           05  CARRIERI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0001).
      *    -------------------------------
           05  FINRESPL PIC S9(0004) COMP.
           05  FINRESPF PIC  X(0001).
           05  FILLER REDEFINES FINRESPF.
               10  FINRESPA PIC  X(0001).
           05  FINRESPI PIC  X(0010).
      *    -------------------------------
           05  COACCTL PIC S9(0004) COMP.
           05  COACCTF PIC  X(0001).
           05  FILLER REDEFINES COACCTF.
               10  COACCTA PIC  X(0001).
           05  COACCTI PIC  X(0010).
      *    -------------------------------
           05  NAME1L PIC S9(0004) COMP.
           05  NAME1F PIC  X(0001).
           05  FILLER REDEFINES NAME1F.
               10  NAME1A PIC  X(0001).
           05  NAME1I PIC  X(0030).
      *    -------------------------------
           05  EADR1L PIC S9(0004) COMP.
           05  EADR1F PIC  X(0001).
           05  FILLER REDEFINES EADR1F.
               10  EADR1A PIC  X(0001).
           05  EADR1I PIC  X(0045).
      *    -------------------------------
           05  NAME2L PIC S9(0004) COMP.
           05  NAME2F PIC  X(0001).
           05  FILLER REDEFINES NAME2F.
               10  NAME2A PIC  X(0001).
           05  NAME2I PIC  X(0030).
      *    -------------------------------
           05  EADR2L PIC S9(0004) COMP.
           05  EADR2F PIC  X(0001).
           05  FILLER REDEFINES EADR2F.
               10  EADR2A PIC  X(0001).
           05  EADR2I PIC  X(0045).
      *    -------------------------------
           05  NAME3L PIC S9(0004) COMP.
           05  NAME3F PIC  X(0001).
           05  FILLER REDEFINES NAME3F.
               10  NAME3A PIC  X(0001).
           05  NAME3I PIC  X(0030).
      *    -------------------------------
           05  EADR3L PIC S9(0004) COMP.
           05  EADR3F PIC  X(0001).
           05  FILLER REDEFINES EADR3F.
               10  EADR3A PIC  X(0001).
           05  EADR3I PIC  X(0045).
      *    -------------------------------
           05  NAME4L PIC S9(0004) COMP.
           05  NAME4F PIC  X(0001).
           05  FILLER REDEFINES NAME4F.
               10  NAME4A PIC  X(0001).
           05  NAME4I PIC  X(0030).
      *    -------------------------------
           05  EADR4L PIC S9(0004) COMP.
           05  EADR4F PIC  X(0001).
           05  FILLER REDEFINES EADR4F.
               10  EADR4A PIC  X(0001).
           05  EADR4I PIC  X(0045).
      *    -------------------------------
           05  NAME5L PIC S9(0004) COMP.
           05  NAME5F PIC  X(0001).
           05  FILLER REDEFINES NAME5F.
               10  NAME5A PIC  X(0001).
           05  NAME5I PIC  X(0030).
      *    -------------------------------
           05  EADR5L PIC S9(0004) COMP.
           05  EADR5F PIC  X(0001).
           05  FILLER REDEFINES EADR5F.
               10  EADR5A PIC  X(0001).
           05  EADR5I PIC  X(0045).
      *    -------------------------------
           05  NAME6L PIC S9(0004) COMP.
           05  NAME6F PIC  X(0001).
           05  FILLER REDEFINES NAME6F.
               10  NAME6A PIC  X(0001).
           05  NAME6I PIC  X(0030).
      *    -------------------------------
           05  EADR6L PIC S9(0004) COMP.
           05  EADR6F PIC  X(0001).
           05  FILLER REDEFINES EADR6F.
               10  EADR6A PIC  X(0001).
           05  EADR6I PIC  X(0045).
      *    -------------------------------
           05  NAME7L PIC S9(0004) COMP.
           05  NAME7F PIC  X(0001).
           05  FILLER REDEFINES NAME7F.
               10  NAME7A PIC  X(0001).
           05  NAME7I PIC  X(0030).
      *    -------------------------------
           05  EADR7L PIC S9(0004) COMP.
           05  EADR7F PIC  X(0001).
           05  FILLER REDEFINES EADR7F.
               10  EADR7A PIC  X(0001).
           05  EADR7I PIC  X(0045).
      *    -------------------------------
           05  NAME8L PIC S9(0004) COMP.
           05  NAME8F PIC  X(0001).
           05  FILLER REDEFINES NAME8F.
               10  NAME8A PIC  X(0001).
           05  NAME8I PIC  X(0030).
      *    -------------------------------
           05  EADR8L PIC S9(0004) COMP.
           05  EADR8F PIC  X(0001).
           05  FILLER REDEFINES EADR8F.
               10  EADR8A PIC  X(0001).
           05  EADR8I PIC  X(0045).
      *    -------------------------------
           05  NAME9L PIC S9(0004) COMP.
           05  NAME9F PIC  X(0001).
           05  FILLER REDEFINES NAME9F.
               10  NAME9A PIC  X(0001).
           05  NAME9I PIC  X(0030).
      *    -------------------------------
           05  EADR9L PIC S9(0004) COMP.
           05  EADR9F PIC  X(0001).
           05  FILLER REDEFINES EADR9F.
               10  EADR9A PIC  X(0001).
           05  EADR9I PIC  X(0045).
      *    -------------------------------
           05  NAME10L PIC S9(0004) COMP.
           05  NAME10F PIC  X(0001).
           05  FILLER REDEFINES NAME10F.
               10  NAME10A PIC  X(0001).
           05  NAME10I PIC  X(0030).
      *    -------------------------------
           05  EADR10L PIC S9(0004) COMP.
           05  EADR10F PIC  X(0001).
           05  FILLER REDEFINES EADR10F.
               10  EADR10A PIC  X(0001).
           05  EADR10I PIC  X(0045).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  99.
       01  EL6526AO REDEFINES EL6526AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRESPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAME1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EADR1O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAME2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EADR2O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAME3O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EADR3O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAME4O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EADR4O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAME5O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EADR5O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAME6O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EADR6O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAME7O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EADR7O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAME8O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EADR8O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAME9O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EADR9O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAME10O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EADR10O PIC  X(0045).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
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
       01  DFHCOMMAREA                     PIC X(1024).
      *                                COPY ERCEADR.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCEADR                             *
      *                                                                *
      *   ONLINE CREDIT SYSTEM                                         *
      *                                                                *
      *   FILE DESCRIPTION = EMAIL ADDRESS FILE                        *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 910   RECFORM = FIXED                          *
      *                                                                *
      *   BASE CLUSTER NAME = EREADR                   RKP=2,LEN=48    *
      *       ALTERNATE PATH = NONE                                    *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 100703    2003080800002  PEMA  NEW FILE
      ******************************************************************
       01  EMAIL-ADDRESS.
           12  EA-RECORD-ID                          PIC XX.
               88  VALID-EA-ID                          VALUE 'EA'.
           12  EA-CONTROL-PRIMARY.
               16  EA-COMPANY-CD                     PIC X.
               16  EA-RECORD-TYPE                    PIC XX.
                   88  EA-ERCOMP-EMAIL                  VALUE 'CO'.
               16  EA-REST-OF-KEY                    PIC X(45).
               16  EA-ERCOMP-EMAIL-KEY REDEFINES EA-REST-OF-KEY.
                   20  EA-CO-CARRIER                 PIC X.
                   20  EA-CO-GROUPING                PIC X(6).
                   20  EA-CO-FIN-RESP                PIC X(10).
                   20  EA-CO-ACCOUNT                 PIC X(10).
                   20  EA-CO-TYPE                    PIC X.
                   20  FILLER                        PIC X(17).
           12  EA-RECORD-BODY.
               16  FILLER OCCURS 10.
                   20  EA-PERSONS-NAME               PIC X(30).
                   20  EA-PERSONS-EMAIL              PIC X(45).
               16  EA-LAST-UPDATE-DT                 PIC 9(8).
           12  FILLER                                PIC X(102).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA EMAIL-ADDRESS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6526' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE EIBDATE                TO DC-JULIAN-YYDDD.
           MOVE '5'                    TO DC-OPTION-CODE.
           PERFORM 9700-DATE-CONVERT THRU 9700-EXIT.
           MOVE DC-GREG-DATE-1-EDIT    TO WS-SAVE-DATE.
           MOVE DC-BIN-DATE-1          TO WS-SAVE-BIN-DT.
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
           MOVE EIBTRMID               TO QID-TERM.
           MOVE +1                     TO EMI-NUMBER-OF-LINES.
           MOVE SPACE                  TO SUPPRESS-MAP-SW.
           IF EIBCALEN = 0
               GO TO 8800-UNAUTHORIZED-ACCESS.
           MOVE LOW-VALUES             TO EL6526AI.
           IF PI-CALLING-PROGRAM NOT = THIS-PGM
               MOVE PI-SAVED-PROGRAM-5 TO  PI-SAVED-PROGRAM-6
               MOVE PI-SAVED-PROGRAM-4 TO  PI-SAVED-PROGRAM-5
               MOVE PI-SAVED-PROGRAM-3 TO  PI-SAVED-PROGRAM-4
               MOVE PI-SAVED-PROGRAM-2 TO  PI-SAVED-PROGRAM-3
               MOVE PI-SAVED-PROGRAM-1 TO  PI-SAVED-PROGRAM-2
               MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
               MOVE PI-CALLING-PROGRAM TO  PI-RETURN-TO-PROGRAM
               MOVE THIS-PGM           TO  PI-CALLING-PROGRAM.
           IF EIBTRNID NOT = TRANS-ID
              GO TO 4000-SHOW
           END-IF
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR  (9600-PGMID-ERROR)
      *        ERROR     (9990-ABEND)
      *    END-EXEC.
      *    MOVE '"$L.                  ! " #00001333' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031333333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF EIBAID = DFHCLEAR
               GO TO 9400-CLEAR.
           .
       0200-RECEIVE.
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
               MOVE ER-0008            TO EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               MOVE -1                 TO PFENTERL
               GO TO 8200-SEND-DATAONLY.
           
      * EXEC CICS RECEIVE
      *        MAP      (MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        INTO     (EL6526AI)
      *    END-EXEC.
           MOVE LENGTH OF
            EL6526AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001346' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6526AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PFENTERL = 0
               GO TO 0300-CHECK-PFKEYS.
           IF EIBAID NOT = DFHENTER
               MOVE ER-0004            TO EMI-ERROR
               GO TO 0320-INPUT-ERROR.
           IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)
               MOVE PF-VALUES (PFENTERI) TO EIBAID
           ELSE
               MOVE ER-0029              TO EMI-ERROR
               GO TO 0320-INPUT-ERROR.
       0300-CHECK-PFKEYS.
           IF EIBAID = DFHPF23
               GO TO 8810-PF23.
           IF EIBAID = DFHPF24
               GO TO 9200-RETURN-MAIN-MENU.
           IF EIBAID = DFHPF12
               GO TO 9500-PF12.
           IF EIBAID = DFHENTER
               GO TO 0330-CHECK-MAINTYP.
           MOVE ER-0029                TO EMI-ERROR.
       0320-INPUT-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE AL-UNBON               TO PFENTERA.
           MOVE -1                     TO PFENTERL.
           GO TO 8200-SEND-DATAONLY.
       0330-CHECK-MAINTYP.
           .
       4000-SHOW.
           MOVE PI-COMPANY-CD          TO EADR-KEY
           MOVE 'CO'                   TO EADR-REC-TYPE
           MOVE PI-ERC-CARRIER         TO EADR-CARRIER
           MOVE PI-ERC-GROUP           TO EADR-GROUPING
           MOVE PI-ERC-RESP            TO EADR-FIN-RESP
           MOVE PI-ERC-ACCT            TO EADR-ACCOUNT
           MOVE PI-ERC-TYPE            TO EADR-TYPE
           display ' key = ' eadr-key (2:47)
           
      * EXEC CICS READ
      *       DATASET   (EREADR-FILE-ID)
      *       SET       (ADDRESS OF EMAIL-ADDRESS)
      *       RIDFLD    (EADR-KEY)
      *       RESP      (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        E          (  N#00001387' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303031333837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EREADR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EADR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF EMAIL-ADDRESS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              display ' not normal ' ws-response
              MOVE ER-9999             TO EMI-ERROR
              MOVE -1                  TO CARRIERL
              MOVE AL-SABOF            TO CARRIERA
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           display ' good read ' ea-co-account
           display ' ea email ' ea-persons-email (1)
           MOVE LOW-VALUES             TO EL6526AO
           MOVE EA-CO-CARRIER          TO CARRIERO
           MOVE EA-CO-GROUPING         TO GROUPO
           MOVE EA-CO-FIN-RESP         TO FINRESPO
           MOVE EA-CO-ACCOUNT          TO COACCTO
           MOVE EA-CO-TYPE             TO TYPEO
           MOVE EA-PERSONS-NAME (1)    TO NAME1O
           MOVE EA-PERSONS-EMAIL (1)   TO EADR1O
           MOVE EA-PERSONS-NAME (2)    TO NAME2O
           MOVE EA-PERSONS-EMAIL (2)   TO EADR2O
           MOVE EA-PERSONS-NAME (3)    TO NAME3O
           MOVE EA-PERSONS-EMAIL (3)   TO EADR3O
           MOVE EA-PERSONS-NAME (4)    TO NAME4O
           MOVE EA-PERSONS-EMAIL (4)   TO EADR4O
           MOVE EA-PERSONS-NAME (5)    TO NAME5O
           MOVE EA-PERSONS-EMAIL (5)   TO EADR5O
           MOVE EA-PERSONS-NAME (6)    TO NAME6O
           MOVE EA-PERSONS-EMAIL (6)   TO EADR6O
           MOVE EA-PERSONS-NAME (7)    TO NAME7O
           MOVE EA-PERSONS-EMAIL (7)   TO EADR7O
           MOVE EA-PERSONS-NAME (8)    TO NAME8O
           MOVE EA-PERSONS-EMAIL (8)   TO EADR8O
           MOVE EA-PERSONS-NAME (9)    TO NAME9O
           MOVE EA-PERSONS-EMAIL (9)   TO EADR9O
           MOVE EA-PERSONS-NAME (10)   TO NAME10O
           MOVE EA-PERSONS-EMAIL (10)  TO EADR10O
           MOVE AL-SANOF               TO CARRIERA
           MOVE -1                     TO CARRIERL
           GO TO 8100-SEND-INITIAL-MAP
           .
       5099-EXIT.
           EXIT.
      ******************************************************************
       8100-SEND-INITIAL-MAP.
           display ' made 8100 '
           MOVE WS-SAVE-DATE           TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
           MOVE PI-COMPANY-ID          TO CMPNYIDO.
           MOVE PI-PROCESSOR-ID        TO USERIDO.
           MOVE -1                     TO CARRIERL
      *    MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
           
      * EXEC CICS SEND
      *        MAP      (MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        FROM     (EL6526AO)
      *        ERASE
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL6526AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00001445' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6526AO, 
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
           
           GO TO 9100-RETURN-TRAN.
       8200-SEND-DATAONLY.
           IF EIBTRNID NOT = TRANS-ID
              GO TO 8100-SEND-INITIAL-MAP
           END-IF
           MOVE WS-SAVE-DATE           TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
           MOVE PI-COMPANY-ID          TO CMPNYIDO.
           MOVE PI-PROCESSOR-ID        TO USERIDO.
      *    MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O
           
      * EXEC CICS SEND
      *        MAP      (MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        FROM     (EL6526AO)
      *        DATAONLY
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL6526AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00001463' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6526AO, 
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
           
           GO TO 9100-RETURN-TRAN.
       EJECT
       8300-SEND-TEXT.
           
      * EXEC CICS SEND TEXT
      *        FROM     (LOGOFF-TEXT)
      *        LENGTH   (LOGOFF-LENGTH)
      *        ERASE
      *        FREEKB
      *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001473' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343733' TO DFHEIV0(25:11)
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
           
           
      * EXEC CICS RETURN
      *    END-EXEC.
      *    MOVE '.(                    &   #00001479' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       EJECT
       8600-DEEDIT.
           
      * EXEC CICS BIF DEEDIT
      *         FIELD(DEEDIT-FIELD)
      *         LENGTH(15)
      *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00001483' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       EJECT
       8800-UNAUTHORIZED-ACCESS.
           MOVE UNACCESS-MSG           TO LOGOFF-MSG.
           GO TO 8300-SEND-TEXT.
       8810-PF23.
           MOVE EIBAID                 TO PI-ENTRY-CD-1.
           MOVE XCTL-005               TO PGM-NAME.
           GO TO 9300-XCTL.
       EJECT
       9100-RETURN-TRAN.
           MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
           MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
           
      * EXEC CICS RETURN
      *        TRANSID    (TRANS-ID)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (WS-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.(CT                  &   #00001499' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9200-RETURN-MAIN-MENU.
           MOVE XCTL-626               TO PGM-NAME.
           GO TO 9300-XCTL.
       EJECT
       9300-XCTL.
           
      * EXEC CICS XCTL
      *        PROGRAM    (PGM-NAME)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (WS-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.$C                   $   #00001509' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9400-CLEAR.
           MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
           GO TO 9300-XCTL.
       9500-PF12.
           MOVE XCTL-010               TO PGM-NAME.
           GO TO 9300-XCTL.
       9600-PGMID-ERROR.
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR    (8300-SEND-TEXT)
      *    END-EXEC.
      *    MOVE '"$L                   ! # #00001521' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031353231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
           MOVE ' '                    TO PI-ENTRY-CD-1.
           MOVE XCTL-005               TO PGM-NAME.
           MOVE PGM-NAME               TO LOGOFF-PGM.
           MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
           GO TO 9300-XCTL.
       9700-DATE-CONVERT.
           
      * EXEC CICS LINK
      *        PROGRAM    ('ELDATCV')
      *        COMMAREA   (DATE-CONVERSION-DATA)
      *        LENGTH     (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00001531' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9700-EXIT.
           EXIT.
       EJECT
       9900-ERROR-FORMAT.
           IF NOT EMI-ERRORS-COMPLETE
               MOVE LINK-001           TO PGM-NAME
               
      * EXEC CICS LINK
      *            PROGRAM    (PGM-NAME)
      *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
      *            LENGTH     (EMI-COMM-LENGTH)
      *        END-EXEC.
      *    MOVE '."C                   ''   #00001542' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9900-EXIT.
           EXIT.
       9990-ABEND.
           MOVE LINK-004               TO PGM-NAME.
           MOVE DFHEIBLK               TO EMI-LINE1.
           
      * EXEC CICS LINK
      *        PROGRAM   (PGM-NAME)
      *        COMMAREA  (EMI-LINE1)
      *        LENGTH    (72)
      *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00001552' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 8200-SEND-DATAONLY.
       9995-SECURITY-VIOLATION.
      *           COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00001576' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353736' TO DFHEIV0(25:11)
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
       9995-EXIT.
            EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6526' TO DFHEIV1
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
           MOVE 'EL6526' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
