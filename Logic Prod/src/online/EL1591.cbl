       ID DIVISION.
       PROGRAM-ID.                 EL1591.
      *
      *AUTHOR.     PABLO
      *            OMAHA NE
      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CENTRAL STATES IS EXPRESSLY PROHIBITED       *
      *            *   WITHOUT THE PRIOR WRITTEN PERMISSION OF         *
      *            *   CENTRAL STATES                                  *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.
      *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED
      *    FOR PRODUCT DEFINITIONS CLP UEP FACTORS.
      *    SCREENS     - EL159B - PRODUCT/FACTORS DEFINITION
      *    ENTERED BY  - EL159 - MAINTENANCE MENU
      *    EXIT TO     - EL159 - MAINTENANCE MENU
      *    INPUT FILE  - ERPDEF -              - PRODUCT DEFINITION
      *    OUTPUT FILE - ERPDEF -              - PRODUCT DEFINITION
      *    COMMAREA    - PASSED
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON
      *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
      *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
      *                  ENTRIES (XCTL FROM CICS VIA E031) THE SCREEN
      *                  WILL BE READ AND ACTION WILL BE BASED ON THE
      *                  MAINTENANCE TYPE INDICATED.
041515******************************************************************
041515*                   C H A N G E   L O G
041515*
041515* Changes are marked by the Change Effective date.
041515*-----------------------------------------------------------------
041515*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
041515* EFFECTIVE    NUMBER
041515*-----------------------------------------------------------------
041515* 041515    2015041400001  PEMA  USE CORRECT FORMATTED MAP FLD
041515******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*   EL1591 WORKING STORAGE     *'.
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'.
       77  FIRST-READ-PREV-SW              PIC X(01)   VALUE SPACES.
           88  FIRST-READ-PREV                         VALUE 'Y'.
       77  M1                              PIC S999 COMP-3 VALUE +0.
       77  M2                              PIC S999 COMP-3 VALUE +0.
       77  X1                              PIC S999 COMP-3 VALUE +0.
       77  Y1                              PIC S999 COMP-3 VALUE +0.
       01  ACCESS-KEYS.
           12  ERPDEF-KEY.
               16  ERPDEF-COMPANY-CD       PIC X.
               16  ERPDEF-STATE            PIC XX.
               16  ERPDEF-PROD-CD          PIC XXX.
               16  FILLER                  PIC X(7).
               16  ERPDEF-BEN-TYPE         PIC X.
               16  ERPDEF-BEN-CODE         PIC XX.
               16  ERPDEF-EXP-DT           PIC XX.
       01  WS-DATE-AREA.
           05  SAVE-DATE                   PIC X(08)   VALUE SPACES.
           05  SAVE-BIN-DATE               PIC X(02)   VALUE SPACES.
       01  MISC-WORK-AREAS.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
           12  WS-NUMVAL.
               16  WS-NUMVAL-OF-DEEDIT     PIC 9(11) VALUE ZEROS.
               16  WS-9V999-OF-DEEDIT REDEFINES
                   WS-NUMVAL-OF-DEEDIT     PIC 9(8)V999.
           12  DEEDIT-FIELD                PIC X(11).
           12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD
                                           PIC S9(11).
           12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD
                                           PIC S9(09)V99.
           12  WS-EXP-DT                   PIC X(02)   VALUE LOW-VALUES.
       01  STANDARD-AREAS.
           12  SC-ITEM                     PIC S9(4)   VALUE +1  COMP.
           12  TRANS-ID                    PIC X(04)   VALUE 'E032'.
           12  PGM-NAME                    PIC X(08).
           12  TIME-IN                     PIC S9(07).
           12  TIME-OUT-R  REDEFINES TIME-IN.
               16  FILLER                  PIC X(01).
               16  TIME-OUT                PIC 99V99.
               16  FILLER                  PIC X(02).
           12  XCTL-005                    PIC X(08)   VALUE 'EL005'.
           12  XCTL-010                    PIC X(08)   VALUE 'EL010'.
           12  XCTL-155                    PIC X(08)   VALUE 'EL155'.
           12  XCTL-626                    PIC X(08)   VALUE 'EL626'.
           12  LINK-001                    PIC X(08)   VALUE 'EL001'.
           12  LINK-004                    PIC X(08)   VALUE 'EL004'.
           12  LINK-ELDATCV                PIC X(08)   VALUE 'ELDATCV'.
           12  THIS-PGM                    PIC X(08)   VALUE 'EL1591'.
           12  ERPDEF-FILE-ID              PIC X(08)   VALUE 'ERPDEF'.
           12  ERPDEF-LENGTH               PIC S9(04)  VALUE +1319 COMP.
           12  SUB                         PIC 9(02).
           12  SUB-1                       PIC 9(02).
           12  SUB2                        PIC 9(02).
           12  GETMAIN-SPACE               PIC X(01)   VALUE SPACE.
           12  MAPSET-NAME                 PIC X(08)   VALUE 'EL1591S'.
           12  WS-MAP-NAME                 PIC X(08)   VALUE 'EL159B'.
           12  WS-PF-KEY                   PIC 9(02)   VALUE ZEROS.
           12  WS-MAXAMT                   PIC S9(9)    VALUE +0 COMP-3.
           12  WS-MAXATTAGE                PIC S999     VALUE +0 COMP-3.
           12  WS-MINAGE                   PIC S999     VALUE +0 COMP-3.
           12  WS-MAXAGE                   PIC S999     VALUE +0 COMP-3.
           12  WS-MAXTERM                  PIC S999     VALUE +0 COMP-3.
           12  WS-EXCL                     PIC S999     VALUE +0 COMP-3.
           12  WS-COV-ENDS                 PIC S999     VALUE +0 COMP-3.
           12  WS-ACC-ONLY                 PIC S999     VALUE +0 COMP-3.
           12  WS-CRIT-PER                 PIC S999     VALUE +0 COMP-3.
      *                                    COPY ELCSCTM.
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
      *                                    COPY ELCSCRTY.
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
       01  ERROR-MESSAGES.
           12  ER-0000                     PIC X(04)   VALUE '0000'.
           12  ER-0023                     PIC X(04)   VALUE '0023'.
           12  ER-0029                     PIC X(04)   VALUE '0029'.
           12  ER-0050                     PIC X(04)   VALUE '0050'.
           12  ER-0068                     PIC X(04)   VALUE '0068'.
           12  ER-0070                     PIC X(04)   VALUE '0070'.
           12  ER-0130                     PIC X(04)   VALUE '0130'.
           12  ER-0131                     PIC X(04)   VALUE '0131'.
           12  ER-0132                     PIC X(04)   VALUE '0132'.
           12  ER-0138                     PIC X(04)   VALUE '0138'.
           12  ER-0144                     PIC X(04)   VALUE '0144'.
           12  ER-0145                     PIC X(04)   VALUE '0145'.
           12  ER-0418                     PIC X(04)   VALUE '0418'.
           12  ER-0582                     PIC X(04)   VALUE '0582'.
           12  ER-0701                     PIC X(04)   VALUE '0701'.
           12  ER-0702                     PIC X(04)   VALUE '0702'.
           12  ER-0703                     PIC X(04)   VALUE '0703'.
           12  ER-0704                     PIC X(04)   VALUE '0704'.
           12  ER-0705                     PIC X(04)   VALUE '0705'.
           12  ER-0706                     PIC X(04)   VALUE '0706'.
           12  ER-0707                     PIC X(04)   VALUE '0707'.
           12  ER-0708                     PIC X(04)   VALUE '0708'.
           12  ER-0709                     PIC X(04)   VALUE '0709'.
           12  ER-0710                     PIC X(04)   VALUE '0710'.
           12  ER-0711                     PIC X(04)   VALUE '0711'.
           12  ER-0712                     PIC X(04)   VALUE '0712'.
           12  ER-0713                     PIC X(04)   VALUE '0713'.
           12  ER-0717                     PIC X(04)   VALUE '0717'.
           12  ER-0718                     PIC X(04)   VALUE '0718'.
           12  ER-0719                     PIC X(04)   VALUE '0719'.
           12  ER-0720                     PIC X(04)   VALUE '0720'.
           12  ER-0721                     PIC X(04)   VALUE '0721'.
           12  ER-0722                     PIC X(04)   VALUE '0722'.
           12  ER-0723                     PIC X(04)   VALUE '0723'.
           12  ER-0724                     PIC X(04)   VALUE '0724'.
           12  ER-0725                     PIC X(04)   VALUE '0725'.
           12  ER-0726                     PIC X(04)   VALUE '0726'.
           12  ER-0727                     PIC X(04)   VALUE '0727'.
           12  ER-0729                     PIC X(04)   VALUE '0729'.
           12  ER-0754                     PIC X(04)   VALUE '0754'.
           12  ER-2241                     PIC X(04)   VALUE '2241'.
           12  ER-2276                     PIC X(04)   VALUE '2276'.
           12  ER-7008                     PIC X(04)   VALUE '7008'.
           12  ER-7031                     PIC X(04)   VALUE '7031'.
           12  ER-7123                     PIC X(04)   VALUE '7123'.
           12  ER-8150                     PIC X(04)   VALUE '8150'.
           12  ER-9999                     PIC XXXX    VALUE '9999'.
      *                                    COPY ELCDATE.
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
      *                                    COPY ELCLOGOF.
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
      *                                    COPY ELCATTR.
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
      *                                    COPY ELCEMIB.
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
      *                                    COPY ELCJPFX.
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
                                           PIC X(530).
      *                                    COPY ELCINTF.
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
           12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
               16  PI-FORM-NUMBER          PIC X(12).
               16  PI-PREV-PROD-KEY.
                   20  PI-PREV-COMPANY-CD  PIC X.
                   20  PI-PREV-STATE       PIC XX.
                   20  PI-PREV-PROD-CD     PIC XXX.
                   20  FILLER              PIC X(7).
                   20  PI-PREV-BEN-TYPE    PIC X.
                   20  PI-PREV-BEN-CODE    PIC XX.
                   20  PI-PREV-EXP-DT      PIC XX.
               16  PI-PROD-KEY.
                   20  PI-PK-COMPANY-CD    PIC X.
                   20  PI-PK-STATE         PIC XX.
                   20  PI-PK-PROD-CD       PIC XXX.
                   20  FILLER              PIC X(7).
                   20  PI-BEN-TYPE         PIC X.
                   20  PI-BEN-CODE         PIC XX.
                   20  PI-EXP-DT           PIC XX.
               16  PI-LEFT-RIGHT           PIC X.
                   88  PI-FAR-LEFT           VALUE 'L'.
                   88  PI-FAR-RIGHT          VALUE 'R'.
               16  PI-UP-DOWN              PIC X.
                   88  PI-FAR-UP             VALUE 'U'.
                   88  PI-FAR-DOWN           VALUE 'D'.
               16  PI-X1                   PIC S999 COMP-3.
               16  PI-Y1                   PIC S999 COMP-3.
               16  FILLER                  PIC X(583).
      *                                    COPY ELCAID.
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
       01  FILLER    REDEFINES DFHAID.
           12  FILLER                      PIC X(08).
           12  PF-VALUES                   PIC X         OCCURS 24.
      *                                    COPY EL1591S.
       01  EL159BI.
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
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  MAINTBYL PIC S9(0004) COMP.
           05  MAINTBYF PIC  X(0001).
           05  FILLER REDEFINES MAINTBYF.
               10  MAINTBYA PIC  X(0001).
           05  MAINTBYI PIC  X(0004).
      *    -------------------------------
           05  MAINTONL PIC S9(0004) COMP.
           05  MAINTONF PIC  X(0001).
           05  FILLER REDEFINES MAINTONF.
               10  MAINTONA PIC  X(0001).
           05  MAINTONI PIC  X(0008).
      *    -------------------------------
           05  MAINTATL PIC S9(0004) COMP.
           05  MAINTATF PIC  X(0001).
           05  FILLER REDEFINES MAINTATF.
               10  MAINTATA PIC  X(0001).
           05  MAINTATI PIC  X(0005).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  PRODCDL PIC S9(0004) COMP.
           05  PRODCDF PIC  X(0001).
           05  FILLER REDEFINES PRODCDF.
               10  PRODCDA PIC  X(0001).
           05  PRODCDI PIC  X(0003).
      *    -------------------------------
           05  BENTYPL PIC S9(0004) COMP.
           05  BENTYPF PIC  X(0001).
           05  FILLER REDEFINES BENTYPF.
               10  BENTYPA PIC  X(0001).
           05  BENTYPI PIC  X(0001).
      *    -------------------------------
           05  BENCODEL PIC S9(0004) COMP.
           05  BENCODEF PIC  X(0001).
           05  FILLER REDEFINES BENCODEF.
               10  BENCODEA PIC  X(0001).
           05  BENCODEI PIC  X(0002).
      *    -------------------------------
           05  EXPDTL PIC S9(0004) COMP.
           05  EXPDTF PIC  X(0001).
           05  FILLER REDEFINES EXPDTF.
               10  EXPDTA PIC  X(0001).
           05  EXPDTI PIC  X(0010).
      *    -------------------------------
           05  PDESCL PIC S9(0004) COMP.
           05  PDESCF PIC  X(0001).
           05  FILLER REDEFINES PDESCF.
               10  PDESCA PIC  X(0001).
           05  PDESCI PIC  X(0080).
      *    -------------------------------
           05  YR1L PIC S9(0004) COMP.
           05  YR1F PIC  X(0001).
           05  FILLER REDEFINES YR1F.
               10  YR1A PIC  X(0001).
           05  YR1I PIC  99.
      *    -------------------------------
           05  YR2L PIC S9(0004) COMP.
           05  YR2F PIC  X(0001).
           05  FILLER REDEFINES YR2F.
               10  YR2A PIC  X(0001).
           05  YR2I PIC  99.
      *    -------------------------------
           05  YR3L PIC S9(0004) COMP.
           05  YR3F PIC  X(0001).
           05  FILLER REDEFINES YR3F.
               10  YR3A PIC  X(0001).
           05  YR3I PIC  99.
      *    -------------------------------
           05  YR4L PIC S9(0004) COMP.
           05  YR4F PIC  X(0001).
           05  FILLER REDEFINES YR4F.
               10  YR4A PIC  X(0001).
           05  YR4I PIC  99.
      *    -------------------------------
           05  YR5L PIC S9(0004) COMP.
           05  YR5F PIC  X(0001).
           05  FILLER REDEFINES YR5F.
               10  YR5A PIC  X(0001).
           05  YR5I PIC  99.
      *    -------------------------------
           05  YR6L PIC S9(0004) COMP.
           05  YR6F PIC  X(0001).
           05  FILLER REDEFINES YR6F.
               10  YR6A PIC  X(0001).
           05  YR6I PIC  99.
      *    -------------------------------
           05  YR7L PIC S9(0004) COMP.
           05  YR7F PIC  X(0001).
           05  FILLER REDEFINES YR7F.
               10  YR7A PIC  X(0001).
           05  YR7I PIC  99.
      *    -------------------------------
           05  YR8L PIC S9(0004) COMP.
           05  YR8F PIC  X(0001).
           05  FILLER REDEFINES YR8F.
               10  YR8A PIC  X(0001).
           05  YR8I PIC  99.
      *    -------------------------------
           05  LOTRM1L PIC S9(0004) COMP.
           05  LOTRM1F PIC  X(0001).
           05  FILLER REDEFINES LOTRM1F.
               10  LOTRM1A PIC  X(0001).
           05  LOTRM1I PIC  X(0003).
      *    -------------------------------
           05  HITRM1L PIC S9(0004) COMP.
           05  HITRM1F PIC  X(0001).
           05  FILLER REDEFINES HITRM1F.
               10  HITRM1A PIC  X(0001).
           05  HITRM1I PIC  X(0003).
      *    -------------------------------
           05  LOAMT1L PIC S9(0004) COMP.
           05  LOAMT1F PIC  X(0001).
           05  FILLER REDEFINES LOAMT1F.
               10  LOAMT1A PIC  X(0001).
           05  LOAMT1I PIC  X(0006).
      *    -------------------------------
           05  HIAMT1L PIC S9(0004) COMP.
           05  HIAMT1F PIC  X(0001).
           05  FILLER REDEFINES HIAMT1F.
               10  HIAMT1A PIC  X(0001).
           05  HIAMT1I PIC  X(0008).
      *    -------------------------------
           05  FACT01L PIC S9(0004) COMP.
           05  FACT01F PIC  X(0001).
           05  FILLER REDEFINES FACT01F.
               10  FACT01A PIC  X(0001).
           05  FACT01I PIC  X(0005).
      *    -------------------------------
           05  FACT11L PIC S9(0004) COMP.
           05  FACT11F PIC  X(0001).
           05  FILLER REDEFINES FACT11F.
               10  FACT11A PIC  X(0001).
           05  FACT11I PIC  X(0005).
      *    -------------------------------
           05  FACT21L PIC S9(0004) COMP.
           05  FACT21F PIC  X(0001).
           05  FILLER REDEFINES FACT21F.
               10  FACT21A PIC  X(0001).
           05  FACT21I PIC  X(0005).
      *    -------------------------------
           05  FACT31L PIC S9(0004) COMP.
           05  FACT31F PIC  X(0001).
           05  FILLER REDEFINES FACT31F.
               10  FACT31A PIC  X(0001).
           05  FACT31I PIC  X(0005).
      *    -------------------------------
           05  FACT41L PIC S9(0004) COMP.
           05  FACT41F PIC  X(0001).
           05  FILLER REDEFINES FACT41F.
               10  FACT41A PIC  X(0001).
           05  FACT41I PIC  X(0005).
      *    -------------------------------
           05  FACT51L PIC S9(0004) COMP.
           05  FACT51F PIC  X(0001).
           05  FILLER REDEFINES FACT51F.
               10  FACT51A PIC  X(0001).
           05  FACT51I PIC  X(0005).
      *    -------------------------------
           05  FACT61L PIC S9(0004) COMP.
           05  FACT61F PIC  X(0001).
           05  FILLER REDEFINES FACT61F.
               10  FACT61A PIC  X(0001).
           05  FACT61I PIC  X(0005).
      *    -------------------------------
           05  FACT71L PIC S9(0004) COMP.
           05  FACT71F PIC  X(0001).
           05  FILLER REDEFINES FACT71F.
               10  FACT71A PIC  X(0001).
           05  FACT71I PIC  X(0005).
      *    -------------------------------
           05  LOTRM2L PIC S9(0004) COMP.
           05  LOTRM2F PIC  X(0001).
           05  FILLER REDEFINES LOTRM2F.
               10  LOTRM2A PIC  X(0001).
           05  LOTRM2I PIC  X(0003).
      *    -------------------------------
           05  HITRM2L PIC S9(0004) COMP.
           05  HITRM2F PIC  X(0001).
           05  FILLER REDEFINES HITRM2F.
               10  HITRM2A PIC  X(0001).
           05  HITRM2I PIC  X(0003).
      *    -------------------------------
           05  LOAMT2L PIC S9(0004) COMP.
           05  LOAMT2F PIC  X(0001).
           05  FILLER REDEFINES LOAMT2F.
               10  LOAMT2A PIC  X(0001).
           05  LOAMT2I PIC  X(0006).
      *    -------------------------------
           05  HIAMT2L PIC S9(0004) COMP.
           05  HIAMT2F PIC  X(0001).
           05  FILLER REDEFINES HIAMT2F.
               10  HIAMT2A PIC  X(0001).
           05  HIAMT2I PIC  X(0008).
      *    -------------------------------
           05  FACT02L PIC S9(0004) COMP.
           05  FACT02F PIC  X(0001).
           05  FILLER REDEFINES FACT02F.
               10  FACT02A PIC  X(0001).
           05  FACT02I PIC  X(0005).
      *    -------------------------------
           05  FACT12L PIC S9(0004) COMP.
           05  FACT12F PIC  X(0001).
           05  FILLER REDEFINES FACT12F.
               10  FACT12A PIC  X(0001).
           05  FACT12I PIC  X(0005).
      *    -------------------------------
           05  FACT22L PIC S9(0004) COMP.
           05  FACT22F PIC  X(0001).
           05  FILLER REDEFINES FACT22F.
               10  FACT22A PIC  X(0001).
           05  FACT22I PIC  X(0005).
      *    -------------------------------
           05  FACT32L PIC S9(0004) COMP.
           05  FACT32F PIC  X(0001).
           05  FILLER REDEFINES FACT32F.
               10  FACT32A PIC  X(0001).
           05  FACT32I PIC  X(0005).
      *    -------------------------------
           05  FACT42L PIC S9(0004) COMP.
           05  FACT42F PIC  X(0001).
           05  FILLER REDEFINES FACT42F.
               10  FACT42A PIC  X(0001).
           05  FACT42I PIC  X(0005).
      *    -------------------------------
           05  FACT52L PIC S9(0004) COMP.
           05  FACT52F PIC  X(0001).
           05  FILLER REDEFINES FACT52F.
               10  FACT52A PIC  X(0001).
           05  FACT52I PIC  X(0005).
      *    -------------------------------
           05  FACT62L PIC S9(0004) COMP.
           05  FACT62F PIC  X(0001).
           05  FILLER REDEFINES FACT62F.
               10  FACT62A PIC  X(0001).
           05  FACT62I PIC  X(0005).
      *    -------------------------------
           05  FACT72L PIC S9(0004) COMP.
           05  FACT72F PIC  X(0001).
           05  FILLER REDEFINES FACT72F.
               10  FACT72A PIC  X(0001).
           05  FACT72I PIC  X(0005).
      *    -------------------------------
           05  LOTRM3L PIC S9(0004) COMP.
           05  LOTRM3F PIC  X(0001).
           05  FILLER REDEFINES LOTRM3F.
               10  LOTRM3A PIC  X(0001).
           05  LOTRM3I PIC  X(0003).
      *    -------------------------------
           05  HITRM3L PIC S9(0004) COMP.
           05  HITRM3F PIC  X(0001).
           05  FILLER REDEFINES HITRM3F.
               10  HITRM3A PIC  X(0001).
           05  HITRM3I PIC  X(0003).
      *    -------------------------------
           05  LOAMT3L PIC S9(0004) COMP.
           05  LOAMT3F PIC  X(0001).
           05  FILLER REDEFINES LOAMT3F.
               10  LOAMT3A PIC  X(0001).
           05  LOAMT3I PIC  X(0006).
      *    -------------------------------
           05  HIAMT3L PIC S9(0004) COMP.
           05  HIAMT3F PIC  X(0001).
           05  FILLER REDEFINES HIAMT3F.
               10  HIAMT3A PIC  X(0001).
           05  HIAMT3I PIC  X(0008).
      *    -------------------------------
           05  FACT03L PIC S9(0004) COMP.
           05  FACT03F PIC  X(0001).
           05  FILLER REDEFINES FACT03F.
               10  FACT03A PIC  X(0001).
           05  FACT03I PIC  X(0005).
      *    -------------------------------
           05  FACT13L PIC S9(0004) COMP.
           05  FACT13F PIC  X(0001).
           05  FILLER REDEFINES FACT13F.
               10  FACT13A PIC  X(0001).
           05  FACT13I PIC  X(0005).
      *    -------------------------------
           05  FACT23L PIC S9(0004) COMP.
           05  FACT23F PIC  X(0001).
           05  FILLER REDEFINES FACT23F.
               10  FACT23A PIC  X(0001).
           05  FACT23I PIC  X(0005).
      *    -------------------------------
           05  FACT33L PIC S9(0004) COMP.
           05  FACT33F PIC  X(0001).
           05  FILLER REDEFINES FACT33F.
               10  FACT33A PIC  X(0001).
           05  FACT33I PIC  X(0005).
      *    -------------------------------
           05  FACT43L PIC S9(0004) COMP.
           05  FACT43F PIC  X(0001).
           05  FILLER REDEFINES FACT43F.
               10  FACT43A PIC  X(0001).
           05  FACT43I PIC  X(0005).
      *    -------------------------------
           05  FACT53L PIC S9(0004) COMP.
           05  FACT53F PIC  X(0001).
           05  FILLER REDEFINES FACT53F.
               10  FACT53A PIC  X(0001).
           05  FACT53I PIC  X(0005).
      *    -------------------------------
           05  FACT63L PIC S9(0004) COMP.
           05  FACT63F PIC  X(0001).
           05  FILLER REDEFINES FACT63F.
               10  FACT63A PIC  X(0001).
           05  FACT63I PIC  X(0005).
      *    -------------------------------
           05  FACT73L PIC S9(0004) COMP.
           05  FACT73F PIC  X(0001).
           05  FILLER REDEFINES FACT73F.
               10  FACT73A PIC  X(0001).
           05  FACT73I PIC  X(0005).
      *    -------------------------------
           05  LOTRM4L PIC S9(0004) COMP.
           05  LOTRM4F PIC  X(0001).
           05  FILLER REDEFINES LOTRM4F.
               10  LOTRM4A PIC  X(0001).
           05  LOTRM4I PIC  X(0003).
      *    -------------------------------
           05  HITRM4L PIC S9(0004) COMP.
           05  HITRM4F PIC  X(0001).
           05  FILLER REDEFINES HITRM4F.
               10  HITRM4A PIC  X(0001).
           05  HITRM4I PIC  X(0003).
      *    -------------------------------
           05  LOAMT4L PIC S9(0004) COMP.
           05  LOAMT4F PIC  X(0001).
           05  FILLER REDEFINES LOAMT4F.
               10  LOAMT4A PIC  X(0001).
           05  LOAMT4I PIC  X(0006).
      *    -------------------------------
           05  HIAMT4L PIC S9(0004) COMP.
           05  HIAMT4F PIC  X(0001).
           05  FILLER REDEFINES HIAMT4F.
               10  HIAMT4A PIC  X(0001).
           05  HIAMT4I PIC  X(0008).
      *    -------------------------------
           05  FACT04L PIC S9(0004) COMP.
           05  FACT04F PIC  X(0001).
           05  FILLER REDEFINES FACT04F.
               10  FACT04A PIC  X(0001).
           05  FACT04I PIC  X(0005).
      *    -------------------------------
           05  FACT14L PIC S9(0004) COMP.
           05  FACT14F PIC  X(0001).
           05  FILLER REDEFINES FACT14F.
               10  FACT14A PIC  X(0001).
           05  FACT14I PIC  X(0005).
      *    -------------------------------
           05  FACT24L PIC S9(0004) COMP.
           05  FACT24F PIC  X(0001).
           05  FILLER REDEFINES FACT24F.
               10  FACT24A PIC  X(0001).
           05  FACT24I PIC  X(0005).
      *    -------------------------------
           05  FACT34L PIC S9(0004) COMP.
           05  FACT34F PIC  X(0001).
           05  FILLER REDEFINES FACT34F.
               10  FACT34A PIC  X(0001).
           05  FACT34I PIC  X(0005).
      *    -------------------------------
           05  FACT44L PIC S9(0004) COMP.
           05  FACT44F PIC  X(0001).
           05  FILLER REDEFINES FACT44F.
               10  FACT44A PIC  X(0001).
           05  FACT44I PIC  X(0005).
      *    -------------------------------
           05  FACT54L PIC S9(0004) COMP.
           05  FACT54F PIC  X(0001).
           05  FILLER REDEFINES FACT54F.
               10  FACT54A PIC  X(0001).
           05  FACT54I PIC  X(0005).
      *    -------------------------------
           05  FACT64L PIC S9(0004) COMP.
           05  FACT64F PIC  X(0001).
           05  FILLER REDEFINES FACT64F.
               10  FACT64A PIC  X(0001).
           05  FACT64I PIC  X(0005).
      *    -------------------------------
           05  FACT74L PIC S9(0004) COMP.
           05  FACT74F PIC  X(0001).
           05  FILLER REDEFINES FACT74F.
               10  FACT74A PIC  X(0001).
           05  FACT74I PIC  X(0005).
      *    -------------------------------
           05  LOTRM5L PIC S9(0004) COMP.
           05  LOTRM5F PIC  X(0001).
           05  FILLER REDEFINES LOTRM5F.
               10  LOTRM5A PIC  X(0001).
           05  LOTRM5I PIC  X(0003).
      *    -------------------------------
           05  HITRM5L PIC S9(0004) COMP.
           05  HITRM5F PIC  X(0001).
           05  FILLER REDEFINES HITRM5F.
               10  HITRM5A PIC  X(0001).
           05  HITRM5I PIC  X(0003).
      *    -------------------------------
           05  LOAMT5L PIC S9(0004) COMP.
           05  LOAMT5F PIC  X(0001).
           05  FILLER REDEFINES LOAMT5F.
               10  LOAMT5A PIC  X(0001).
           05  LOAMT5I PIC  X(0006).
      *    -------------------------------
           05  HIAMT5L PIC S9(0004) COMP.
           05  HIAMT5F PIC  X(0001).
           05  FILLER REDEFINES HIAMT5F.
               10  HIAMT5A PIC  X(0001).
           05  HIAMT5I PIC  X(0008).
      *    -------------------------------
           05  FACT05L PIC S9(0004) COMP.
           05  FACT05F PIC  X(0001).
           05  FILLER REDEFINES FACT05F.
               10  FACT05A PIC  X(0001).
           05  FACT05I PIC  X(0005).
      *    -------------------------------
           05  FACT15L PIC S9(0004) COMP.
           05  FACT15F PIC  X(0001).
           05  FILLER REDEFINES FACT15F.
               10  FACT15A PIC  X(0001).
           05  FACT15I PIC  X(0005).
      *    -------------------------------
           05  FACT25L PIC S9(0004) COMP.
           05  FACT25F PIC  X(0001).
           05  FILLER REDEFINES FACT25F.
               10  FACT25A PIC  X(0001).
           05  FACT25I PIC  X(0005).
      *    -------------------------------
           05  FACT35L PIC S9(0004) COMP.
           05  FACT35F PIC  X(0001).
           05  FILLER REDEFINES FACT35F.
               10  FACT35A PIC  X(0001).
           05  FACT35I PIC  X(0005).
      *    -------------------------------
           05  FACT45L PIC S9(0004) COMP.
           05  FACT45F PIC  X(0001).
           05  FILLER REDEFINES FACT45F.
               10  FACT45A PIC  X(0001).
           05  FACT45I PIC  X(0005).
      *    -------------------------------
           05  FACT55L PIC S9(0004) COMP.
           05  FACT55F PIC  X(0001).
           05  FILLER REDEFINES FACT55F.
               10  FACT55A PIC  X(0001).
           05  FACT55I PIC  X(0005).
      *    -------------------------------
           05  FACT65L PIC S9(0004) COMP.
           05  FACT65F PIC  X(0001).
           05  FILLER REDEFINES FACT65F.
               10  FACT65A PIC  X(0001).
           05  FACT65I PIC  X(0005).
      *    -------------------------------
           05  FACT75L PIC S9(0004) COMP.
           05  FACT75F PIC  X(0001).
           05  FILLER REDEFINES FACT75F.
               10  FACT75A PIC  X(0001).
           05  FACT75I PIC  X(0005).
      *    -------------------------------
           05  LOTRM6L PIC S9(0004) COMP.
           05  LOTRM6F PIC  X(0001).
           05  FILLER REDEFINES LOTRM6F.
               10  LOTRM6A PIC  X(0001).
           05  LOTRM6I PIC  X(0003).
      *    -------------------------------
           05  HITRM6L PIC S9(0004) COMP.
           05  HITRM6F PIC  X(0001).
           05  FILLER REDEFINES HITRM6F.
               10  HITRM6A PIC  X(0001).
           05  HITRM6I PIC  X(0003).
      *    -------------------------------
           05  LOAMT6L PIC S9(0004) COMP.
           05  LOAMT6F PIC  X(0001).
           05  FILLER REDEFINES LOAMT6F.
               10  LOAMT6A PIC  X(0001).
           05  LOAMT6I PIC  X(0006).
      *    -------------------------------
           05  HIAMT6L PIC S9(0004) COMP.
           05  HIAMT6F PIC  X(0001).
           05  FILLER REDEFINES HIAMT6F.
               10  HIAMT6A PIC  X(0001).
           05  HIAMT6I PIC  X(0008).
      *    -------------------------------
           05  FACT06L PIC S9(0004) COMP.
           05  FACT06F PIC  X(0001).
           05  FILLER REDEFINES FACT06F.
               10  FACT06A PIC  X(0001).
           05  FACT06I PIC  X(0005).
      *    -------------------------------
           05  FACT16L PIC S9(0004) COMP.
           05  FACT16F PIC  X(0001).
           05  FILLER REDEFINES FACT16F.
               10  FACT16A PIC  X(0001).
           05  FACT16I PIC  X(0005).
      *    -------------------------------
           05  FACT26L PIC S9(0004) COMP.
           05  FACT26F PIC  X(0001).
           05  FILLER REDEFINES FACT26F.
               10  FACT26A PIC  X(0001).
           05  FACT26I PIC  X(0005).
      *    -------------------------------
           05  FACT36L PIC S9(0004) COMP.
           05  FACT36F PIC  X(0001).
           05  FILLER REDEFINES FACT36F.
               10  FACT36A PIC  X(0001).
           05  FACT36I PIC  X(0005).
      *    -------------------------------
           05  FACT46L PIC S9(0004) COMP.
           05  FACT46F PIC  X(0001).
           05  FILLER REDEFINES FACT46F.
               10  FACT46A PIC  X(0001).
           05  FACT46I PIC  X(0005).
      *    -------------------------------
           05  FACT56L PIC S9(0004) COMP.
           05  FACT56F PIC  X(0001).
           05  FILLER REDEFINES FACT56F.
               10  FACT56A PIC  X(0001).
           05  FACT56I PIC  X(0005).
      *    -------------------------------
           05  FACT66L PIC S9(0004) COMP.
           05  FACT66F PIC  X(0001).
           05  FILLER REDEFINES FACT66F.
               10  FACT66A PIC  X(0001).
           05  FACT66I PIC  X(0005).
      *    -------------------------------
           05  FACT76L PIC S9(0004) COMP.
           05  FACT76F PIC  X(0001).
           05  FILLER REDEFINES FACT76F.
               10  FACT76A PIC  X(0001).
           05  FACT76I PIC  X(0005).
      *    -------------------------------
           05  LOTRM7L PIC S9(0004) COMP.
           05  LOTRM7F PIC  X(0001).
           05  FILLER REDEFINES LOTRM7F.
               10  LOTRM7A PIC  X(0001).
           05  LOTRM7I PIC  X(0003).
      *    -------------------------------
           05  HITRM7L PIC S9(0004) COMP.
           05  HITRM7F PIC  X(0001).
           05  FILLER REDEFINES HITRM7F.
               10  HITRM7A PIC  X(0001).
           05  HITRM7I PIC  X(0003).
      *    -------------------------------
           05  LOAMT7L PIC S9(0004) COMP.
           05  LOAMT7F PIC  X(0001).
           05  FILLER REDEFINES LOAMT7F.
               10  LOAMT7A PIC  X(0001).
           05  LOAMT7I PIC  X(0006).
      *    -------------------------------
           05  HIAMT7L PIC S9(0004) COMP.
           05  HIAMT7F PIC  X(0001).
           05  FILLER REDEFINES HIAMT7F.
               10  HIAMT7A PIC  X(0001).
           05  HIAMT7I PIC  X(0008).
      *    -------------------------------
           05  FACT07L PIC S9(0004) COMP.
           05  FACT07F PIC  X(0001).
           05  FILLER REDEFINES FACT07F.
               10  FACT07A PIC  X(0001).
           05  FACT07I PIC  X(0005).
      *    -------------------------------
           05  FACT17L PIC S9(0004) COMP.
           05  FACT17F PIC  X(0001).
           05  FILLER REDEFINES FACT17F.
               10  FACT17A PIC  X(0001).
           05  FACT17I PIC  X(0005).
      *    -------------------------------
           05  FACT27L PIC S9(0004) COMP.
           05  FACT27F PIC  X(0001).
           05  FILLER REDEFINES FACT27F.
               10  FACT27A PIC  X(0001).
           05  FACT27I PIC  X(0005).
      *    -------------------------------
           05  FACT37L PIC S9(0004) COMP.
           05  FACT37F PIC  X(0001).
           05  FILLER REDEFINES FACT37F.
               10  FACT37A PIC  X(0001).
           05  FACT37I PIC  X(0005).
      *    -------------------------------
           05  FACT47L PIC S9(0004) COMP.
           05  FACT47F PIC  X(0001).
           05  FILLER REDEFINES FACT47F.
               10  FACT47A PIC  X(0001).
           05  FACT47I PIC  X(0005).
      *    -------------------------------
           05  FACT57L PIC S9(0004) COMP.
           05  FACT57F PIC  X(0001).
           05  FILLER REDEFINES FACT57F.
               10  FACT57A PIC  X(0001).
           05  FACT57I PIC  X(0005).
      *    -------------------------------
           05  FACT67L PIC S9(0004) COMP.
           05  FACT67F PIC  X(0001).
           05  FILLER REDEFINES FACT67F.
               10  FACT67A PIC  X(0001).
           05  FACT67I PIC  X(0005).
      *    -------------------------------
           05  FACT77L PIC S9(0004) COMP.
           05  FACT77F PIC  X(0001).
           05  FILLER REDEFINES FACT77F.
               10  FACT77A PIC  X(0001).
           05  FACT77I PIC  X(0005).
      *    -------------------------------
           05  LOTRM8L PIC S9(0004) COMP.
           05  LOTRM8F PIC  X(0001).
           05  FILLER REDEFINES LOTRM8F.
               10  LOTRM8A PIC  X(0001).
           05  LOTRM8I PIC  X(0003).
      *    -------------------------------
           05  HITRM8L PIC S9(0004) COMP.
           05  HITRM8F PIC  X(0001).
           05  FILLER REDEFINES HITRM8F.
               10  HITRM8A PIC  X(0001).
           05  HITRM8I PIC  X(0003).
      *    -------------------------------
           05  LOAMT8L PIC S9(0004) COMP.
           05  LOAMT8F PIC  X(0001).
           05  FILLER REDEFINES LOAMT8F.
               10  LOAMT8A PIC  X(0001).
           05  LOAMT8I PIC  X(0006).
      *    -------------------------------
           05  HIAMT8L PIC S9(0004) COMP.
           05  HIAMT8F PIC  X(0001).
           05  FILLER REDEFINES HIAMT8F.
               10  HIAMT8A PIC  X(0001).
           05  HIAMT8I PIC  X(0008).
      *    -------------------------------
           05  FACT08L PIC S9(0004) COMP.
           05  FACT08F PIC  X(0001).
           05  FILLER REDEFINES FACT08F.
               10  FACT08A PIC  X(0001).
           05  FACT08I PIC  X(0005).
      *    -------------------------------
           05  FACT18L PIC S9(0004) COMP.
           05  FACT18F PIC  X(0001).
           05  FILLER REDEFINES FACT18F.
               10  FACT18A PIC  X(0001).
           05  FACT18I PIC  X(0005).
      *    -------------------------------
           05  FACT28L PIC S9(0004) COMP.
           05  FACT28F PIC  X(0001).
           05  FILLER REDEFINES FACT28F.
               10  FACT28A PIC  X(0001).
           05  FACT28I PIC  X(0005).
      *    -------------------------------
           05  FACT38L PIC S9(0004) COMP.
           05  FACT38F PIC  X(0001).
           05  FILLER REDEFINES FACT38F.
               10  FACT38A PIC  X(0001).
           05  FACT38I PIC  X(0005).
      *    -------------------------------
           05  FACT48L PIC S9(0004) COMP.
           05  FACT48F PIC  X(0001).
           05  FILLER REDEFINES FACT48F.
               10  FACT48A PIC  X(0001).
           05  FACT48I PIC  X(0005).
      *    -------------------------------
           05  FACT58L PIC S9(0004) COMP.
           05  FACT58F PIC  X(0001).
           05  FILLER REDEFINES FACT58F.
               10  FACT58A PIC  X(0001).
           05  FACT58I PIC  X(0005).
      *    -------------------------------
           05  FACT68L PIC S9(0004) COMP.
           05  FACT68F PIC  X(0001).
           05  FILLER REDEFINES FACT68F.
               10  FACT68A PIC  X(0001).
           05  FACT68I PIC  X(0005).
      *    -------------------------------
           05  FACT78L PIC S9(0004) COMP.
           05  FACT78F PIC  X(0001).
           05  FILLER REDEFINES FACT78F.
               10  FACT78A PIC  X(0001).
           05  FACT78I PIC  X(0005).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  99.
       01  EL159BO REDEFINES EL159BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRODCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENCODEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDESCO PIC  X(0080).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR5O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR6O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR7O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR8O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOTRM1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HITRM1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOAMT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HIAMT1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT01O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT11O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT21O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT31O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT41O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT51O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT61O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT71O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOTRM2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HITRM2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOAMT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HIAMT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT02O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT12O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT22O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT32O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT42O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT52O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT62O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT72O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOTRM3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HITRM3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOAMT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HIAMT3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT03O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT13O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT23O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT33O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT43O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT53O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT63O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT73O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOTRM4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HITRM4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOAMT4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HIAMT4O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT04O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT14O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT24O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT34O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT44O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT54O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT64O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT74O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOTRM5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HITRM5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOAMT5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HIAMT5O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT05O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT15O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT25O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT35O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT45O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT55O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT65O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT75O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOTRM6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HITRM6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOAMT6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HIAMT6O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT06O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT16O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT26O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT36O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT46O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT56O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT66O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT76O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOTRM7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HITRM7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOAMT7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HIAMT7O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT07O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT17O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT27O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT37O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT47O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT57O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT67O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT77O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOTRM8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HITRM8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOAMT8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HIAMT8O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT08O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT18O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT28O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT38O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT48O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT58O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT68O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACT78O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  99.
      *    -------------------------------
       01  EL159BI-R REDEFINES EL159BI.
           12  FILLER                      PIC X(217).
           12  EL159-FACT-TABLE OCCURS 8.
               16  LO-TERM-LEN             PIC S9(04)  COMP.
               16  LO-TERM-ATTRB           PIC X(01).
               16  LO-TERM                 PIC 999.
               16  HI-TERM-LEN             PIC S9(04)  COMP.
               16  HI-TERM-ATTRB           PIC X(01).
               16  HI-TERM                 PIC 999.
               16  LO-AMT-LEN              PIC S9(04)  COMP.
               16  LO-AMT-ATTRB            PIC X(01).
               16  LO-AMT                  PIC 9(6).
               16  LO-AMTO REDEFINES LO-AMT PIC Z9,999.
               16  HI-AMT-LEN              PIC S9(04)  COMP.
               16  HI-AMT-ATTRB            PIC X(01).
               16  HI-AMT                  PIC 9(8).
               16  HI-AMTO REDEFINES HI-AMT PIC ZZZ9,999.
               16  FACT0-LEN               PIC S9(04)  COMP.
               16  FACT0-ATTRB             PIC X(01).
               16  FACT0                   PIC X(5).
               16  FACT0O REDEFINES FACT0  PIC 9.999.
               16  FACT0I REDEFINES FACT0  PIC 99V999.
               16  FACT1-LEN               PIC S9(04)  COMP.
               16  FACT1-ATTRB             PIC X(01).
               16  FACT1                   PIC X(5).
               16  FACT1O REDEFINES FACT1  PIC 9.999.
               16  FACT1I REDEFINES FACT1  PIC 99V999.
               16  FACT2-LEN               PIC S9(04)  COMP.
               16  FACT2-ATTRB             PIC X(01).
               16  FACT2                   PIC X(5).
               16  FACT2O REDEFINES FACT2  PIC 9.999.
               16  FACT2I REDEFINES FACT2  PIC 99V999.
               16  FACT3-LEN               PIC S9(04)  COMP.
               16  FACT3-ATTRB             PIC X(01).
               16  FACT3                   PIC X(5).
               16  FACT3O REDEFINES FACT3  PIC 9.999.
               16  FACT3I REDEFINES FACT3  PIC 99V999.
               16  FACT4-LEN               PIC S9(04)  COMP.
               16  FACT4-ATTRB             PIC X(01).
               16  FACT4                   PIC X(5).
               16  FACT4O REDEFINES FACT4  PIC 9.999.
               16  FACT4I REDEFINES FACT4  PIC 99V999.
               16  FACT5-LEN               PIC S9(04)  COMP.
               16  FACT5-ATTRB             PIC X(01).
               16  FACT5                   PIC X(5).
               16  FACT5O REDEFINES FACT5  PIC 9.999.
               16  FACT5I REDEFINES FACT5  PIC 99V999.
               16  FACT6-LEN               PIC S9(04)  COMP.
               16  FACT6-ATTRB             PIC X(01).
               16  FACT6                   PIC X(5).
               16  FACT6O REDEFINES FACT6  PIC 9.999.
               16  FACT6I REDEFINES FACT6  PIC 99V999.
               16  FACT7-LEN               PIC S9(04)  COMP.
               16  FACT7-ATTRB             PIC X(01).
               16  FACT7                   PIC X(5).
               16  FACT7O REDEFINES FACT7  PIC 9.999.
               16  FACT7I REDEFINES FACT7  PIC 99V999.
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
      *                                    COPY ERCPDEF.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCPDEF.                            *
      *                                                                *
      *    FILE DESCRIPTION = PRODUCT DEFINITION MASTER                *
      *                                                                *
      *    FILE TYPE = VSAM,KSDS                                       *
      *    RECORD SIZE = 1319 RECFORM = FIXED                          *
      *                                                                *
      *    BASE CLUSTER = ERPDEF                      RKP=02,LEN=18    *
      *                                                                *
      *    LOG = YES                                                   *
      *    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
051414*                   C H A N G E   L O G
051414*
051414* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
051414*-----------------------------------------------------------------
051414*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
051414* EFFECTIVE    NUMBER
051414*-----------------------------------------------------------------
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100314* 100314  CR2014061900001  PEMA  ADD PCT OF BENEFIT
      ******************************************************************
       01  PRODUCT-MASTER.
          12  PD-RECORD-ID                 PIC X(02).
              88  VALID-PD-ID                  VALUE 'PD'.
          12  PD-CONTROL-PRIMARY.
              16  PD-COMPANY-CD            PIC X.
              16  PD-STATE                 PIC XX.
              16  PD-PRODUCT-CD            PIC XXX.
              16  PD-FILLER                PIC X(7).
              16  PD-BEN-TYPE              PIC X.
              16  PD-BEN-CODE              PIC XX.
              16  PD-PROD-EXP-DT           PIC XX.
          12  FILLER                       PIC X(50).
          12  PD-PRODUCT-DATA OCCURS 8.
              16  PD-PROD-CODE             PIC X.
                  88  PD-PROD-LIFE           VALUE 'L'.
                  88  PD-PROD-PROP           VALUE 'P'.
                  88  PD-PROD-AH             VALUE 'A'.
                  88  PD-PROD-IU             VALUE 'I'.
                  88  PD-PROD-GAP            VALUE 'G'.
052614            88  PD-PROD-FAML           VALUE 'F'.
              16  PD-MAX-ATT-AGE           PIC S999        COMP-3.
              16  PD-MIN-ISSUE-AGE         PIC S999        COMP-3.
              16  PD-MAX-ISSUE-AGE         PIC S999        COMP-3.
              16  PD-MAX-TERM              PIC S999        COMP-3.
              16  PD-MAX-AMT               PIC S9(07)      COMP-3.
              16  FILLER                   PIC X.
              16  PD-PRE-EXIST-EXCL-TYPE   PIC 99.
              16  PD-EXCLUSION-PERIOD-DAYS PIC S999        COMP-3.
              16  PD-COVERAGE-ENDS-MOS     PIC S999        COMP-3.
              16  PD-ACCIDENT-ONLY-MOS     PIC S999        COMP-3.
              16  PD-CRIT-PERIOD           PIC S999        COMP-3.
              16  PD-REC-CRIT-PERIOD       PIC 99.
              16  PD-REC-CP-ALPHA  REDEFINES PD-REC-CRIT-PERIOD.
                  20  PD-RECURRING-YN      PIC X.
                  20  FILLER               PIC X.
              16  PD-RTW-MOS               PIC 99.
051414        16  PD-MAX-EXTENSION         PIC 99.
100314        16  pd-ben-pct               pic sv999 comp-3.
100314*       16  FILLER                   PIC XX.
          12  PD-1ST-YR-ADMIN-ALLOW        PIC S9(3)V99    COMP-3.
          12  PD-TERM-LIMITS OCCURS 15.
              16  PD-LOW-TERM              PIC S999        COMP-3.
              16  PD-HI-TERM               PIC S999        COMP-3.
      *  THE LOAN AMT LIMITS CORRESPOND TO THE TERM LIMITS ABOVE
          12  PD-LOAN-AMT-LIMITS OCCURS 15.
              16  PD-LOW-AMT               PIC S9(5)       COMP-3.
              16  PD-HI-AMT                PIC S9(7)       COMP-3.
          12  PD-EARN-FACTORS.
              16  FILLER OCCURS 15.
                  20  FILLER OCCURS 15.
                      24  PD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
          12  PD-PRODUCT-DESC              PIC X(80).
          12  PD-TRUNCATED                 PIC X.
          12  FILLER                       PIC X(59).
          12  PD-MAINT-INFORMATION.
              16  PD-LAST-MAINT-DT         PIC X(02).
              16  PD-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
              16  PD-LAST-MAINT-BY         PIC X(04).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PRODUCT-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1591' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK
           IF EIBCALEN = 0
              GO TO 8800-UNAUTHORIZED-ACCESS.
           MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6
           MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6
           MOVE 1                      TO EMI-NUMBER-OF-LINES
           
      * EXEC CICS HANDLE CONDITION
      *        DUPREC     (8850-DUPREC)
      *        NOTOPEN    (8870-NOTOPEN)
      *        NOTFND     (8880-NOT-FOUND)
      *        PGMIDERR   (9600-PGMID-ERROR)
      *        ERROR      (9990-ABEND)
      *    END-EXEC.
      *    MOVE '"$%JIL.               ! " #00002282' TO DFHEIV0
           MOVE X'2224254A494C2E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032323832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           .
       0150-SET-PROGRAM-SAVES.
           IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM
               IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
                   MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
                   MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
                   MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
                   MOVE THIS-PGM             TO PI-CALLING-PROGRAM
               ELSE
                   MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
                   MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
                   MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
                   MOVE SPACES               TO PI-SAVED-PROGRAM-6
           ELSE
               GO TO 0200-RECEIVE.
           GO TO 1000-SHOW-PROD-RECORD
           .
       0200-RECEIVE.
           IF EIBAID = DFHCLEAR
              GO TO 9400-CLEAR.
           IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
               MOVE LOW-VALUES             TO  EL159BI
               MOVE ER-7008                TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               MOVE -1                     TO  MAINTL
               GO TO 8200-SEND-DATAONLY.
           IF PI-PROCESSOR-ID EQUAL 'LGXX'
               NEXT SENTENCE
           ELSE
               
      * EXEC CICS READQ TS
      *            QUEUE  (PI-SECURITY-TEMP-STORE-ID)
      *            INTO   (SECURITY-CONTROL)
      *            LENGTH (SC-COMM-LENGTH)
      *            ITEM   (SC-ITEM)
      *        END-EXEC
      *    MOVE '*$II   L              ''   #00002326' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
               MOVE SC-CLAIMS-DISPLAY (4)  TO  PI-DISPLAY-CAP
               MOVE SC-CLAIMS-UPDATE  (4)  TO  PI-MODIFY-CAP.
           
      * EXEC CICS RECEIVE
      *        MAP      (WS-MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        INTO     (EL159BI)
      *    END-EXEC
           MOVE LENGTH OF
            EL159BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002334' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL159BI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF PFKEYL = +0
               GO TO 0300-CHECK-PFKEYS.
           IF (PFKEYI NUMERIC) AND (PFKEYI GREATER 0 AND LESS 25)
               MOVE PF-VALUES (PFKEYI)     TO  EIBAID
           ELSE
               MOVE ER-0029                TO  EMI-ERROR
               GO TO 0320-INPUT-ERROR.
       0300-CHECK-PFKEYS.
           IF EIBAID EQUAL DFHPF23
               GO TO 8810-PF23.
           IF EIBAID EQUAL DFHPF24
               GO TO 9200-RETURN-MAIN-MENU.
           IF EIBAID EQUAL DFHPF12
               GO TO 9500-PF12.
           IF (MAINTL <> 0) AND (EIBAID <> DFHENTER)
               MOVE ER-0050            TO EMI-ERROR
               GO TO 0320-INPUT-ERROR.
           EVALUATE EIBAID
              WHEN DFHPF1
                 GO TO 5000-FIND-NEXT-PROD-RECORD
              WHEN DFHPF2
                 GO TO 5100-FIND-PREV-PROD-RECORD
              WHEN DFHPF3
                 GO TO 3000-SCROLL-FWD
              WHEN DFHPF4
                 GO TO 3100-SCROLL-BWD
              WHEN DFHPF5
                 GO TO 3200-SCROLL-RT
              WHEN DFHPF6
                 GO TO 3300-SCROLL-LT
              WHEN DFHENTER
                 GO TO 0330-EDIT-DATA
           END-EVALUATE
           IF EIBAID EQUAL DFHENTER
               GO TO 0330-EDIT-DATA.
           MOVE ER-0029                    TO  EMI-ERROR.
       0320-INPUT-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE AL-UNBON                   TO  PFKEYA.
           MOVE -1                         TO  PFKEYL.
           GO TO 8200-SEND-DATAONLY.
           EJECT
       0330-EDIT-DATA.
           IF NOT DISPLAY-CAP
               MOVE 'READ'                 TO  SM-READ
               PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
               MOVE ER-0070                TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               MOVE -1                     TO  MAINTL
               GO TO 8100-SEND-INITIAL-MAP.
           IF MAINTI EQUAL 'S'
               GO TO 1000-SHOW-PROD-RECORD.
           IF MAINTI = 'C'
              IF NOT MODIFY-CAP
                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
                  MOVE ER-0070             TO  EMI-ERROR
                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                  MOVE LOW-VALUES          TO  EL159BO
                  MOVE -1                  TO  MAINTL
                  GO TO 8100-SEND-INITIAL-MAP.
           IF MAINTI EQUAL 'C'
               GO TO 2000-CHANGE-PROD-RECORD.
           MOVE ER-0023                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE -1                         TO  MAINTL.
           MOVE AL-UABON                   TO  MAINTA.
           GO TO 8200-SEND-DATAONLY.
       1000-SHOW-PROD-RECORD.
           MOVE PI-PROD-KEY            TO ERPDEF-KEY
                                          PI-PREV-PROD-KEY
           
      * EXEC CICS READ
      *        DATASET    (ERPDEF-FILE-ID)
      *        SET        (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD     (ERPDEF-KEY)
      *        RESP       (WS-RESPONSE)
      *    END-EXEC.
      *    MOVE '&"S        E          (  N#00002409' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032343039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF RESP-NORMAL
              MOVE +1                  TO X1 Y1 PI-X1 PI-Y1
              SET PI-FAR-LEFT          TO TRUE
              SET PI-FAR-UP            TO TRUE
              GO TO 7000-BUILD-OUTPUT-MAP
           ELSE
              DISPLAY ' BAD READ ' WS-RESPONSE ' ' ERPDEF-KEY
           END-IF
           MOVE ER-0418                TO EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE -1                     TO MAINTL
           MOVE AL-UABOF               TO BENTYPA
                                          STATEA
                                          PRODCDA
                                          BENCODEA
                                          EXPDTA
           GO TO 8200-SEND-DATAONLY
           .
       2000-CHANGE-PROD-RECORD.
           IF PI-PROD-KEY NOT = PI-PREV-PROD-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF
           PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT
           IF NOT EMI-NO-ERRORS
               GO TO 8200-SEND-DATAONLY.
           MOVE PI-PROD-KEY            TO ERPDEF-KEY
           
      * EXEC CICS READ
      *        DATASET    (ERPDEF-FILE-ID)
      *        SET        (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD     (ERPDEF-KEY)
      *        UPDATE
      *    END-EXEC.
      *    MOVE '&"S        EU         (   #00002444' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PD-LAST-MAINT-BY     NOT = PI-UPDATE-BY OR
              PD-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
               
      * EXEC CICS UNLOCK
      *            DATASET   (ERPDEF-FILE-ID)
      *        END-EXEC
      *    MOVE '&*                    #   #00002452' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
               MOVE ER-0068                TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               GO TO 1000-SHOW-PROD-RECORD.
           MOVE PI-PROCESSOR-ID            TO  PD-LAST-MAINT-BY.
           MOVE EIBTIME                    TO  PD-LAST-MAINT-HHMMSS.
           MOVE SAVE-BIN-DATE              TO  PD-LAST-MAINT-DT.
           IF PDESCL > +0
              MOVE PDESCI              TO PD-PRODUCT-DESC
           END-IF
           MOVE PI-X1                  TO X1
           MOVE PI-Y1                  TO Y1
           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
              IF LO-TERM-LEN (M1) > +0
                 MOVE LO-TERM (M1)     TO PD-LOW-TERM (X1)
              END-IF
              IF HI-TERM-LEN (M1) > +0
                 MOVE HI-TERM (M1)     TO PD-HI-TERM (X1)
              END-IF
              IF LO-AMT-LEN (M1) > +0
                 MOVE LO-AMT (M1)      TO PD-LOW-AMT (X1)
              END-IF
              IF HI-AMT-LEN (M1) > +0
                 MOVE HI-AMT (M1)      TO PD-HI-AMT (X1)
              END-IF
              IF FACT0-LEN (M1) > +0
                 MOVE FACT0I (M1)      TO PD-UEP-FACTOR (X1 Y1)
              END-IF
              IF FACT1-LEN (M1) > +0
                 MOVE FACT1I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 1)
              END-IF
              IF FACT2-LEN (M1) > +0
                 MOVE FACT2I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 2)
              END-IF
              IF FACT3-LEN (M1) > +0
                 MOVE FACT3I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 3)
              END-IF
              IF FACT4-LEN (M1) > +0
                 MOVE FACT4I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 4)
              END-IF
              IF FACT5-LEN (M1) > +0
                 MOVE FACT5I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 5)
              END-IF
              IF FACT6-LEN (M1) > +0
                 MOVE FACT6I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 6)
              END-IF
              IF FACT7-LEN (M1) > +0
                 MOVE FACT7I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 7)
              END-IF
              ADD +1 TO X1
           END-PERFORM
           .
       2000-CONTINUE-CHANGE.
           
      * EXEC CICS REWRITE
      *        DATASET   (ERPDEF-FILE-ID)
      *        FROM      (PRODUCT-MASTER)
      *    END-EXEC.
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002508' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 PRODUCT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE ER-0000                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 1000-SHOW-PROD-RECORD
           .
       3000-SCROLL-FWD.
           MOVE PI-PROD-KEY            TO ERPDEF-KEY
           
      * EXEC CICS READ
      *        DATASET    (ERPDEF-FILE-ID)
      *        SET        (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD     (ERPDEF-KEY)
      *        RESP       (WS-RESPONSE)
      *    END-EXEC.
      *    MOVE '&"S        E          (  N#00002518' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032353138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF RESP-NORMAL
              MOVE +8                  TO X1 PI-X1
              MOVE PI-Y1               TO Y1
              SET PI-FAR-DOWN          TO TRUE
              GO TO 7000-BUILD-OUTPUT-MAP
           ELSE
              DISPLAY ' BAD READ ' WS-RESPONSE ' ' ERPDEF-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF
           .
       3000-EXIT.
          EXIT.
       3100-SCROLL-BWD.
           MOVE PI-PROD-KEY            TO ERPDEF-KEY
           
      * EXEC CICS READ
      *        DATASET    (ERPDEF-FILE-ID)
      *        SET        (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD     (ERPDEF-KEY)
      *        RESP       (WS-RESPONSE)
      *    END-EXEC.
      *    MOVE '&"S        E          (  N#00002541' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032353431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF RESP-NORMAL
              MOVE +1                  TO X1 PI-X1
              MOVE PI-Y1               TO Y1
              SET PI-FAR-UP            TO TRUE
              GO TO 7000-BUILD-OUTPUT-MAP
           ELSE
              DISPLAY ' BAD READ ' WS-RESPONSE ' ' ERPDEF-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF
           .
       3100-EXIT.
          EXIT.
       3200-SCROLL-RT.
           MOVE PI-PROD-KEY            TO ERPDEF-KEY
           
      * EXEC CICS READ
      *        DATASET    (ERPDEF-FILE-ID)
      *        SET        (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD     (ERPDEF-KEY)
      *        RESP       (WS-RESPONSE)
      *    END-EXEC.
      *    MOVE '&"S        E          (  N#00002564' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032353634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF RESP-NORMAL
              MOVE +8                  TO Y1 PI-Y1
              MOVE PI-X1               TO X1
              SET PI-FAR-RIGHT         TO TRUE
              GO TO 7000-BUILD-OUTPUT-MAP
           ELSE
              DISPLAY ' BAD READ ' WS-RESPONSE ' ' ERPDEF-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF
           .
       3200-EXIT.
          EXIT.
       3300-SCROLL-LT.
           MOVE PI-PROD-KEY            TO ERPDEF-KEY
           
      * EXEC CICS READ
      *        DATASET    (ERPDEF-FILE-ID)
      *        SET        (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD     (ERPDEF-KEY)
      *        RESP       (WS-RESPONSE)
      *    END-EXEC.
      *    MOVE '&"S        E          (  N#00002587' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032353837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF RESP-NORMAL
              MOVE +1                  TO Y1 PI-Y1
              MOVE PI-X1               TO X1
              SET PI-FAR-LEFT          TO TRUE
              GO TO 7000-BUILD-OUTPUT-MAP
           ELSE
              DISPLAY ' BAD READ ' WS-RESPONSE ' ' ERPDEF-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF
           .
       3300-EXIT.
          EXIT.
       5000-FIND-NEXT-PROD-RECORD.
           MOVE PI-PROD-KEY            TO ERPDEF-KEY
           
      * EXEC CICS STARTBR
      *        DATASET   (ERPDEF-FILE-ID)
      *        RIDFLD    (ERPDEF-KEY)
      *        GTEQ
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00002610' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032363130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              MOVE -1                  TO PFKEYL
              MOVE ER-0130             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           .
       5000-READNEXT-LOOP.
           
      * EXEC CICS READNEXT
      *        DATASET   (ERPDEF-FILE-ID)
      *        SET       (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD    (ERPDEF-KEY)
      *        RESP      (WS-RESPONSE)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00002624' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303032363234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF (NOT RESP-NORMAL)
              OR (PD-COMPANY-CD NOT = PI-COMPANY-CD)
              PERFORM 5000-END-BROWSE
              MOVE -1                  TO PFKEYL
              MOVE ER-0130             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              GO TO 1000-SHOW-PROD-RECORD
           END-IF
           IF ERPDEF-KEY = PI-PREV-PROD-KEY
              GO TO 5000-READNEXT-LOOP
           END-IF
           MOVE ERPDEF-KEY             TO PI-PROD-KEY
           PERFORM 5000-END-BROWSE
           MOVE +1                  TO X1 Y1 PI-X1 PI-Y1
           SET PI-FAR-LEFT          TO TRUE
           SET PI-FAR-UP            TO TRUE
           GO TO 7000-BUILD-OUTPUT-MAP
           .
       5000-END-BROWSE.
           
      * EXEC CICS ENDBR
      *        DATASET   (ERPDEF-FILE-ID)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002649' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       5100-FIND-PREV-PROD-RECORD.
           MOVE PI-PREV-PROD-KEY       TO ERPDEF-KEY
      *    IF STATEL > +0
      *       MOVE STATEI              TO ERPDEF-STATE
      *    END-IF
      *    IF PRODCDL > +0
      *       MOVE PRODCDI             TO ERPDEF-PROD-CD
      *    END-IF
      *    IF BENTYPL > +0
      *        MOVE BENTYPI                TO  ERPDEF-BEN-TYPE.
      *    IF BENCODEL > +0
      *        MOVE BENCODEI               TO  ERPDEF-BEN-CODE.
      *    IF EXPDTL IS GREATER THAN +0
      *        MOVE EXPDTI                 TO  DEEDIT-FIELD
      *        PERFORM 8600-DEEDIT THRU 8600-EXIT
      *        IF WS-NUMVAL-OF-DEEDIT >= 999999
      *            MOVE HIGH-VALUES        TO  ERPDEF-EXP-DT
      *        ELSE
      *          STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:12)
      *             DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
      *          END-STRING
      *           MOVE 'L'                    TO  DC-OPTION-CODE
      *            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
      *            IF NO-CONVERSION-ERROR
      *                MOVE DC-BIN-DATE-1  TO  ERPDEF-EXP-DT
      *            ELSE
      *                MOVE LOW-VALUES     TO  ERPDEF-EXP-DT.
           
      * EXEC CICS HANDLE CONDITION
      *        ENDFILE (5100-UNSUCCESSFUL-SEARCH)
      *    END-EXEC.
      *    MOVE '"$''                   ! # #00002680' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032363830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS STARTBR
      *        DATASET   (ERPDEF-FILE-ID)
      *        RIDFLD    (ERPDEF-KEY)
      *        GTEQ
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002683' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       5100-READPREV-LOOP.
           
      * EXEC CICS READPREV
      *        DATASET   (ERPDEF-FILE-ID)
      *        SET       (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD    (ERPDEF-KEY)
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00002689' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PD-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
               GO TO 5100-UNSUCCESSFUL-SEARCH.
           IF ERPDEF-KEY IS EQUAL TO PI-PREV-PROD-KEY
               GO TO 5100-READPREV-LOOP.
           MOVE ERPDEF-KEY             TO PI-PROD-KEY
           MOVE +1                  TO X1 Y1 PI-X1 PI-Y1
           SET PI-FAR-LEFT          TO TRUE
           SET PI-FAR-UP            TO TRUE
           GO TO 7000-BUILD-OUTPUT-MAP.
       5100-END-BROWSE.
           
      * EXEC CICS ENDBR
      *        DATASET   (ERPDEF-FILE-ID)
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002704' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       5100-UNSUCCESSFUL-SEARCH.
           PERFORM 5100-END-BROWSE.
           MOVE ER-0131                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 1000-SHOW-PROD-RECORD.
           .
       6000-EDIT-INPUT-DATA.
           IF PDESCL > +0
              MOVE AL-UANON            TO PDESCA
           END-IF
           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
              IF LO-TERM-LEN (M1) > +0
                 MOVE LO-TERM (M1)     TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 MOVE WS-NUMVAL-OF-DEEDIT TO LO-TERM (M1)
                 MOVE AL-UNNON         TO LO-TERM-ATTRB (M1)
              END-IF
              IF HI-TERM-LEN (M1) > +0
                 MOVE HI-TERM (M1)     TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 MOVE WS-NUMVAL-OF-DEEDIT TO HI-TERM (M1)
                 MOVE AL-UNNON         TO HI-TERM-ATTRB (M1)
              END-IF
              IF LO-AMT-LEN (M1) > +0
                 MOVE LO-AMT (M1)      TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 MOVE WS-NUMVAL-OF-DEEDIT TO LO-AMT (M1)
                 MOVE AL-UNNON         TO LO-AMT-ATTRB (M1)
              END-IF
              IF HI-AMT-LEN (M1) > +0
                 MOVE HI-AMT (M1)      TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 MOVE WS-NUMVAL-OF-DEEDIT TO HI-AMT (M1)
                 MOVE AL-UNNON         TO HI-AMT-ATTRB (M1)
              END-IF
              IF FACT0-LEN (M1) > +0
                 MOVE FACT0 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
                 MOVE WS-9V999-OF-DEEDIT TO FACT0I (M1)
                 MOVE AL-UNNON         TO FACT0-ATTRB (M1)
              END-IF
              IF FACT1-LEN (M1) > +0
                 MOVE FACT1 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
                 MOVE WS-9V999-OF-DEEDIT TO FACT1I (M1)
                 MOVE AL-UNNON         TO FACT1-ATTRB (M1)
              END-IF
              IF FACT2-LEN (M1) > +0
                 MOVE FACT2 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
                 MOVE WS-9V999-OF-DEEDIT TO FACT2I (M1)
                 MOVE AL-UNNON         TO FACT2-ATTRB (M1)
              END-IF
              IF FACT3-LEN (M1) > +0
                 MOVE FACT3 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
041515           MOVE WS-9V999-OF-DEEDIT TO FACT3I (M1)
                 MOVE AL-UNNON         TO FACT3-ATTRB (M1)
              END-IF
              IF FACT4-LEN (M1) > +0
                 MOVE FACT4 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
041515           MOVE WS-9V999-OF-DEEDIT TO FACT4I (M1)
                 MOVE AL-UNNON         TO FACT4-ATTRB (M1)
              END-IF
              IF FACT5-LEN (M1) > +0
                 MOVE FACT5 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
041515           MOVE WS-9V999-OF-DEEDIT TO FACT5I (M1)
                 MOVE AL-UNNON         TO FACT5-ATTRB (M1)
              END-IF
              IF FACT6-LEN (M1) > +0
                 MOVE FACT6 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
041515           MOVE WS-9V999-OF-DEEDIT TO FACT6I (M1)
                 MOVE AL-UNNON         TO FACT6-ATTRB (M1)
              END-IF
              IF FACT7-LEN (M1) > +0
                 MOVE FACT7 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
041515           MOVE WS-9V999-OF-DEEDIT TO FACT7I (M1)
                 MOVE AL-UNNON         TO FACT7-ATTRB (M1)
              END-IF
           END-PERFORM
           .
       6000-EXIT.
           EXIT.
           EJECT
       7000-BUILD-OUTPUT-MAP.
           MOVE LOW-VALUES             TO  EL159BO
           MOVE PI-COMPANY-CD          TO  PI-PREV-COMPANY-CD
           MOVE PD-STATE               TO  STATEO
                                           PI-PREV-STATE
           MOVE PD-PRODUCT-CD          TO  PRODCDO
                                           PI-PREV-PROD-CD
           MOVE PD-BEN-TYPE            TO  BENTYPO
                                           PI-PREV-BEN-TYPE
           MOVE PD-BEN-CODE            TO  BENCODEO
                                           PI-PREV-BEN-CODE
           IF PD-PROD-EXP-DT = HIGH-VALUES
              MOVE '99/99/9999'        TO EXPDTO
              MOVE HIGH-VALUES         TO PI-PREV-EXP-DT
           ELSE
              MOVE PD-PROD-EXP-DT      TO DC-BIN-DATE-1
                                          PI-PREV-EXP-DT
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE +0                  TO DC-ELAPSED-DAYS
                                          DC-ELAPSED-MONTHS
              PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EXPDTO
              ELSE
                 MOVE LOW-VALUES       TO EXPDTO
              END-IF
           END-IF
           MOVE PD-PRODUCT-DESC        TO PDESCO
           MOVE Y1                     TO YR1O
           COMPUTE YR2O = YR1O + 1
           COMPUTE YR3O = YR2O + 1
           COMPUTE YR4O = YR3O + 1
           COMPUTE YR5O = YR4O + 1
           COMPUTE YR6O = YR5O + 1
           COMPUTE YR7O = YR6O + 1
           COMPUTE YR8O = YR7O + 1
           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
              MOVE PD-LOW-TERM (X1)      TO LO-TERM (M1)
              MOVE PD-HI-TERM (X1)       TO HI-TERM (M1)
              MOVE PD-LOW-AMT (X1)       TO LO-AMTO (M1)
              MOVE PD-HI-AMT (X1)        TO HI-AMTO (M1)
              MOVE PD-UEP-FACTOR (X1 Y1)  TO FACT0O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 1)  TO FACT1O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 2)  TO FACT2O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 3)  TO FACT3O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 4)  TO FACT4O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 5)  TO FACT5O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 6)  TO FACT6O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 7)  TO FACT7O (M1)
              ADD +1 TO X1
           END-PERFORM
           MOVE PD-LAST-MAINT-BY           TO  MAINTBYO
                                               PI-UPDATE-BY.
           MOVE PD-LAST-MAINT-HHMMSS       TO  TIME-IN
                                               PI-UPDATE-HHMMSS.
           MOVE TIME-OUT                   TO  MAINTATO.
           MOVE PD-LAST-MAINT-DT           TO  DC-BIN-DATE-1.
           MOVE ' '                        TO  DC-OPTION-CODE.
           MOVE +0                         TO  DC-ELAPSED-DAYS
                                               DC-ELAPSED-MONTHS.
           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-1-EDIT    TO  MAINTONO
           ELSE
               MOVE LOW-VALUES             TO  MAINTONO.
           MOVE -1                         TO  MAINTL.
           MOVE AL-UANOF                   TO  STATEA
                                               PRODCDA
                                               BENTYPA
                                               BENCODEA
                                               EXPDTA
           .
       8100-SEND-INITIAL-MAP.
           MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.
           MOVE EIBTIME                    TO  TIME-IN.
           MOVE SAVE-DATE                  TO  DATEO.
           MOVE TIME-OUT                   TO  TIMEO.
           
      * EXEC CICS SEND
      *        MAP      (WS-MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        FROM     (EL159BO)
      *        ERASE
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL159BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002876' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL159BO, 
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
           
           MOVE '159B'                 TO PI-CURRENT-SCREEN-NO
           GO TO 9100-RETURN-TRAN
           .
       8200-SEND-DATAONLY.
           MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.
           MOVE EIBTIME                    TO  TIME-IN.
           MOVE SAVE-DATE                  TO  DATEO.
           MOVE TIME-OUT                   TO  TIMEO.
           
      * EXEC CICS SEND
      *        MAP      (WS-MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        FROM     (EL159BO)
      *        DATAONLY
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL159BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002891' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL159BO, 
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
           
           GO TO 9100-RETURN-TRAN
           .
       8300-SEND-TEXT.
           
      * EXEC CICS SEND TEXT
      *        FROM  (LOGOFF-TEXT)
      *        LENGTH(LOGOFF-LENGTH)
      *        ERASE
      *        FREEKB
      *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002901' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393031' TO DFHEIV0(25:11)
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
      *        END-EXEC.
      *    MOVE '.(                    ''   #00002907' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           .
       8500-DEEDIT.
           MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
                                       TO WS-9V999-OF-DEEDIT
           .
       8500-EXIT.
           EXIT.
       8600-DEEDIT.
           MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
                                       TO WS-NUMVAL-OF-DEEDIT
           .
       8600-EXIT.
           EXIT.
       8800-UNAUTHORIZED-ACCESS.
           MOVE UNACCESS-MSG               TO  LOGOFF-MSG.
           GO TO 8300-SEND-TEXT.
       8810-PF23.
           MOVE EIBAID                     TO  PI-ENTRY-CD-1.
           MOVE XCTL-005                   TO  PGM-NAME.
           GO TO 9300-XCTL.
       8850-DUPREC.
           MOVE ER-0132                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE -1                         TO  BENTYPL
           MOVE AL-UABON                   TO  BENTYPA BENCODEA EXPDTA
           GO TO 8100-SEND-INITIAL-MAP.
       8870-NOTOPEN.
           MOVE LOW-VALUES                 TO  EL159BO.
           MOVE -1                         TO  MAINTL.
           MOVE ER-0701                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 8100-SEND-INITIAL-MAP.
       8880-NOT-FOUND.
           MOVE ER-0702                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE -1                         TO  BENTYPL
           MOVE AL-UABON                   TO  BENTYPA BENCODEA EXPDTA
           GO TO 8100-SEND-INITIAL-MAP.
           .
       9100-RETURN-TRAN.
           MOVE EMI-ERROR-NUMBER (1)       TO  PI-LAST-ERROR-NO.
           
      * EXEC CICS RETURN
      *        TRANSID    (TRANS-ID)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (PI-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.(CT                  ''   #00002950' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9200-RETURN-MAIN-MENU.
           MOVE XCTL-626                   TO  PGM-NAME.
           GO TO 9300-XCTL.
       9300-XCTL.
           
      * EXEC CICS XCTL
      *        PROGRAM    (PGM-NAME)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (PI-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.$C                   %   #00002959' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9400-CLEAR.
           MOVE PI-RETURN-TO-PROGRAM       TO  PGM-NAME.
           GO TO 9300-XCTL.
       9500-PF12.
           MOVE XCTL-010                   TO  PGM-NAME.
           GO TO 9300-XCTL.
       9600-PGMID-ERROR.
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR   (8300-SEND-TEXT)
      *    END-EXEC.
      *    MOVE '"$L                   ! $ #00002971' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032393731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE PGM-NAME                   TO  PI-CALLING-PROGRAM.
           MOVE ' '                        TO  PI-ENTRY-CD-1.
           MOVE XCTL-005                   TO  PGM-NAME.
           MOVE PGM-NAME                   TO  LOGOFF-PGM.
           MOVE PGMIDERR-MSG               TO  LOGOFF-FILL.
           GO TO 9300-XCTL.
           EJECT
       9700-LINK-DATE-CONVERT.
           
      * EXEC CICS LINK
      *        PROGRAM    ('ELDATCV')
      *        COMMAREA   (DATE-CONVERSION-DATA)
      *        LENGTH     (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00002982' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9700-EXIT.
           EXIT.
       9900-ERROR-FORMAT.
           IF NOT EMI-ERRORS-COMPLETE
               MOVE LINK-001               TO  PGM-NAME
               
      * EXEC CICS LINK
      *            PROGRAM    (PGM-NAME)
      *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
      *            LENGTH     (EMI-COMM-LENGTH)
      *        END-EXEC.
      *    MOVE '."C                   (   #00002992' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9900-EXIT.
           EXIT.
       9990-ABEND.
           MOVE LINK-004                   TO  PGM-NAME.
           MOVE DFHEIBLK                   TO  EMI-LINE1.
           
      * EXEC CICS LINK
      *        PROGRAM   (PGM-NAME)
      *        COMMAREA  (EMI-LINE1)
      *        LENGTH    (72)
      *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00003002' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 8100-SEND-INITIAL-MAP.
           EJECT
       9995-SECURITY-VIOLATION.
      *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00003027' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303237' TO DFHEIV0(25:11)
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
       9995-EXIT.
           EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1591' TO DFHEIV1
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
               GO TO 5100-UNSUCCESSFUL-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1591' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
