       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL6523.
      *              PROGRAM CONVERTED BY
      *              COBOL CONVERSION AID PO 5785-ABJ
      *              CONVERSION DATE 02/14/96 11:58:19.
      *                            VMOD=2.005
      *
      *AUTHOR.     LOGIC,INC.
      *            DALLAS, TEXAS.
      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.    TRANSACTION - EXDD - DISPLAY BANK
      *                                 CROSS REFERENCE RECORDS.
       ENVIRONMENT DIVISION.
           EJECT
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*    EL6523 WORKING STORAGE    *'.
       77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'.
       77  NDX     PIC S9(5)  COMP-3 VALUE +1.
       77  WSUB1   PIC S9(5)  COMP-3 VALUE +0.
       77  WSUB2   PIC S9(5)  COMP-3 VALUE +0.
       77  WSUB3   PIC S9(5)  COMP-3 VALUE +0.
       01  WS-WORK-ACCT-TABLE.
           12  WS-BANKS OCCURS 725.
               16  WS-AM-ACCOUNT       PIC X(10).
               16  WS-AM-EXP-DT        PIC XX.
               16  WS-AM-EFF-DT        PIC XX.
               16  WS-AM-STATE         PIC XX.
               16  FILLER              PIC X(20).
       01  WS-DATE-AREA.
           05  SAVE-DATE           PIC X(8)    VALUE SPACES.
           05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
       01  WS-CONTROL-PRIMARY.
           05  WS-COMPANY-CD       PIC X.
           05  WS-CARRIER          PIC X.
           05  WS-GROUPING         PIC X(6).
           05  WS-BANK             PIC X(10).
           05  WS-OPEN-COUNT       PIC S9(4)    COMP-3 VALUE ZEROS.
      *                            COPY ELCATTR SUPPRESS.
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
           EJECT
       01  ERROR-MESSAGES.
           12  ER-0000                 PIC X(4)  VALUE '0000'.
           12  ER-0004                 PIC X(4)  VALUE '0004'.
           12  ER-0029                 PIC X(4)  VALUE '0029'.
           12  ER-0033                 PIC X(4)  VALUE '0033'.
           12  ER-0034                 PIC X(4)  VALUE '0034'.
           12  ER-0068                 PIC X(4)  VALUE '0068'.
           12  ER-0070                 PIC X(4)  VALUE '0070'.
           12  ER-0130                 PIC X(4)  VALUE '0130'.
           12  ER-0131                 PIC X(4)  VALUE '0131'.
           12  ER-0142                 PIC X(4)  VALUE '0142'.
           12  ER-0193                 PIC X(4)  VALUE '0193'.
           12  ER-0226                 PIC X(4)  VALUE '0226'.
           12  ER-0234                 PIC X(4)  VALUE '0234'.
           12  ER-0235                 PIC X(4)  VALUE '0235'.
           12  ER-0348                 PIC X(4)  VALUE '0348'.
           12  ER-0454                 PIC X(4)  VALUE '0454'.
           12  ER-1162                 PIC X(4)  VALUE '1162'.
           12  ER-1164                 PIC X(4)  VALUE '1164'.
           12  ER-2039                 PIC X(4)  VALUE '2039'.
           12  ER-2056                 PIC X(4)  VALUE '2056'.
           12  ER-2057                 PIC X(4)  VALUE '2057'.
           12  ER-2237                 PIC X(4)  VALUE '2237'.
           12  ER-2238                 PIC X(4)  VALUE '2238'.
           12  ER-2339                 PIC X(4)  VALUE '2339'.
           12  ER-2370                 PIC X(4)  VALUE '2370'.
           12  ER-2947                 PIC X(4)  VALUE '2947'.
           12  ER-3112                 PIC X(4)  VALUE '3112'.
           12  ER-5004                 PIC X(4)  VALUE '5004'.
           12  ER-5005                 PIC X(4)  VALUE '5005'.
           12  ER-6508                 PIC X(4)  VALUE '6508'.
      *                          COPY ELCSCTM.
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
      *                          COPY ELCSCRTY.
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
       01  STANDARD-AREAS.
           12  WS-ERACCT-KEY.
               16  WS-ERACCT-COMP-CD   PIC X.
               16  WS-ERACCT-CARRIER   PIC X.
               16  WS-ERACCT-GROUPING  PIC X(6).
               16  WS-ERACCT-STATE     PIC XX.
               16  WS-ERACCT-ACCOUNT   PIC X(10).
               16  WS-ERACCT-EXP-DT    PIC XX.
               16  FILLER              PIC X(4).
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           12  WS-CHANGE-SW            PIC X  VALUE ' '.
               88  CHANGES-MADE               VALUE 'Y'.
               88  NO-CHANGES-MADE            VALUE 'N'.
           12  WS-LINE-CHANGE-SW       PIC X  VALUE ' '.
               88  LINE-CHANGES               VALUE 'Y'.
               88  NO-LINE-CHANGES            VALUE 'N'.
           12  GETMAIN-SPACE           PIC X     VALUE SPACE.
           12  WS-DEEDIT-FIELD         PIC X(10) VALUE SPACES.
           12  WS-DT-DEEDIT-FIELD REDEFINES WS-DEEDIT-FIELD
                                            PIC X(10).
           12  WS-DEEDIT-FIELD-DATE REDEFINES WS-DT-DEEDIT-FIELD.
               16  FILLER                   PIC X(4).
               16  WS-DEEDIT-FIELD-DATE-OUT PIC X(6).
           12  SC-ITEM-CL-CR       PIC S9(4)         VALUE +1   COMP.
           12  WS-ACCESS.
               16  FILLER          PIC  X(3)         VALUE SPACES.
               16  WS-ACARRIER     PIC  X.
           12  ELCNTL-KEY.
               16  CNTL-COMP-ID    PIC  X(3)         VALUE SPACES.
               16  CNTL-REC-TYPE   PIC  X            VALUE SPACES.
               16  CNTL-ACCESS     PIC  X(4)         VALUE SPACES.
               16  CNTL-SEQ-NO     PIC S9(4)         VALUE +0   COMP.
           12  QID.
               16  QID-TERM        PIC X(4).
               16  FILLER          PIC X(4)    VALUE '652D'.
           12  RETURNED-FROM       PIC X(8)    VALUE SPACES.
           12  QID-MAP-LENGTH      PIC S9(4)   VALUE +1376   COMP.
           12  MAP-NAME                PIC X(8)    VALUE 'EL652D'.
           12  MAPSET-NAME             PIC X(8)    VALUE 'EL6523S'.
           12  TRANS-ID                PIC X(4)    VALUE 'EXDD'.
           12  EL652-TRANS-ID          PIC X(4)    VALUE 'EXD4'.
           12  EL650-TRANS-ID          PIC X(4)    VALUE 'EXC4'.
           12  PGM-NAME                PIC X(8)    VALUE SPACES.
           12  THIS-PGM                PIC X(8)    VALUE 'EL6523'.
           12  XCTL-650                PIC X(8)    VALUE 'EL650'.
           12  BXRF-FILE-ID            PIC X(8)    VALUE 'ERBXRF'.
           12  CNTL-FILE-ID            PIC X(8)    VALUE 'ELCNTL'.
           12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
           12  LINK-001                PIC X(8)    VALUE 'EL001'.
           12  LINK-004                PIC X(8)    VALUE 'EL004'.
           12  XCTL-005                PIC X(8)    VALUE 'EL005'.
           12  XCTL-010                PIC X(8)    VALUE 'EL010'.
           12  XCTL-626                PIC X(8)    VALUE 'EL626'.
           12  TIME-IN                 PIC S9(7).
           12  TIME-OUT-R   REDEFINES TIME-IN.
               16  FILLER              PIC X.
               16  TIME-OUT            PIC 99V99.
               16  FILLER              PIC X(2).
           12  BROWSE-STARTED-SW       PIC X       VALUE ' '.
               88  BROWSE-STARTED      VALUE 'Y'.
       01  WS-RECORD-LENGTH COMP       SYNCHRONIZED.
           12  BXRF-MAX-REC-LENGTH     PIC S9(8)    VALUE +26178.
           12  BXRF-REC-LENGTH         PIC S9(4)    VALUE +0.
      *                                COPY ELCDATE.
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
      *                                COPY ELCEMIB SUPPRESS.
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
      *                  COPY ELCLOGOF SUPPRESS.
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
      *                                COPY ELCINTF.
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
           12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
               16  PI-ERBXRF-KEY.
                   20  PI-BXRF-COMP-CD     PIC X.
                   20  PI-BXRF-CARRIER     PIC X.
                   20  PI-BXRF-GROUPING    PIC X(6).
                   20  PI-BXRF-BANK        PIC X(10).
               16  PI-TOP-NDX              PIC S9(5)  COMP-3.
               16  PI-NDX                  PIC S9(5)  COMP-3.
               16  PI-EOF-SW               PIC X.
                   88  PI-FILE-EOF             VALUE 'Y'.
               16  PI-CHECK-MAINT-TYPE     PIC  X.
                   88  VALID-MAINT-TYPE            VALUE 'S' 'A'
                                                         'C' 'D'.
                   88  ADD-FUNCTION                VALUE 'A'.
                   88  SHOW-FUNCTION               VALUE 'S'.
                   88  DELETE-FUNCTION             VALUE 'D'.
                   88  CHANGE-FUNCTION             VALUE 'C'.
               16  PI-ERC-KEY.
                   20  PI-ERC-COMPANY-CD   PIC  X.
                   20  PI-ERC-CARRIER      PIC  X.
                   20  PI-ERC-GROUP        PIC  X(6).
                   20  PI-ERC-BANK         PIC  X(10).
               16  PI-SAVE-ERBXRF-KEY      PIC  X(18).
               16  FILLER                  PIC X(617).
           EJECT
      *                            COPY ELCAID SUPPRESS.
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
           12  FILLER              PIC X(8).
           12  PF-VALUES           PIC X       OCCURS 24.
      *                            COPY EL6523S.
       01  EL652DI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  MAINTYPL PIC S9(0004) COMP.
           05  MAINTYPF PIC  X(0001).
           05  FILLER REDEFINES MAINTYPF.
               10  MAINTYPA PIC  X(0001).
           05  MAINTYPI PIC  X(0001).
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
           05  BANKL PIC S9(0004) COMP.
           05  BANKF PIC  X(0001).
           05  FILLER REDEFINES BANKF.
               10  BANKA PIC  X(0001).
           05  BANKI PIC  X(0010).
      *    -------------------------------
           05  MAINTDTL PIC S9(0004) COMP.
           05  MAINTDTF PIC  X(0001).
           05  FILLER REDEFINES MAINTDTF.
               10  MAINTDTA PIC  X(0001).
           05  MAINTDTI PIC  X(0008).
      *    -------------------------------
           05  TOTOPNL PIC S9(0004) COMP.
           05  TOTOPNF PIC  X(0001).
           05  FILLER REDEFINES TOTOPNF.
               10  TOTOPNA PIC  X(0001).
           05  TOTOPNI PIC  X(0005).
      *    -------------------------------
           05  TOTOCCL PIC S9(0004) COMP.
           05  TOTOCCF PIC  X(0001).
           05  FILLER REDEFINES TOTOCCF.
               10  TOTOCCA PIC  X(0001).
           05  TOTOCCI PIC  X(0005).
      *    -------------------------------
           05  ACCT01L PIC S9(0004) COMP.
           05  ACCT01F PIC  X(0001).
           05  FILLER REDEFINES ACCT01F.
               10  ACCT01A PIC  X(0001).
           05  ACCT01I PIC  X(0010).
      *    -------------------------------
           05  EFFDT01L PIC S9(0004) COMP.
           05  EFFDT01F PIC  X(0001).
           05  FILLER REDEFINES EFFDT01F.
               10  EFFDT01A PIC  X(0001).
           05  EFFDT01I PIC  X(0008).
      *    -------------------------------
           05  EXPDT01L PIC S9(0004) COMP.
           05  EXPDT01F PIC  X(0001).
           05  FILLER REDEFINES EXPDT01F.
               10  EXPDT01A PIC  X(0001).
           05  EXPDT01I PIC  X(0008).
      *    -------------------------------
           05  STATE01L PIC S9(0004) COMP.
           05  STATE01F PIC  X(0001).
           05  FILLER REDEFINES STATE01F.
               10  STATE01A PIC  X(0001).
           05  STATE01I PIC  X(0002).
      *    -------------------------------
           05  ACCT02L PIC S9(0004) COMP.
           05  ACCT02F PIC  X(0001).
           05  FILLER REDEFINES ACCT02F.
               10  ACCT02A PIC  X(0001).
           05  ACCT02I PIC  X(0010).
      *    -------------------------------
           05  EFFDT02L PIC S9(0004) COMP.
           05  EFFDT02F PIC  X(0001).
           05  FILLER REDEFINES EFFDT02F.
               10  EFFDT02A PIC  X(0001).
           05  EFFDT02I PIC  X(0008).
      *    -------------------------------
           05  EXPDT02L PIC S9(0004) COMP.
           05  EXPDT02F PIC  X(0001).
           05  FILLER REDEFINES EXPDT02F.
               10  EXPDT02A PIC  X(0001).
           05  EXPDT02I PIC  X(0008).
      *    -------------------------------
           05  STATE02L PIC S9(0004) COMP.
           05  STATE02F PIC  X(0001).
           05  FILLER REDEFINES STATE02F.
               10  STATE02A PIC  X(0001).
           05  STATE02I PIC  X(0002).
      *    -------------------------------
           05  ACCT03L PIC S9(0004) COMP.
           05  ACCT03F PIC  X(0001).
           05  FILLER REDEFINES ACCT03F.
               10  ACCT03A PIC  X(0001).
           05  ACCT03I PIC  X(0010).
      *    -------------------------------
           05  EFFDT03L PIC S9(0004) COMP.
           05  EFFDT03F PIC  X(0001).
           05  FILLER REDEFINES EFFDT03F.
               10  EFFDT03A PIC  X(0001).
           05  EFFDT03I PIC  X(0008).
      *    -------------------------------
           05  EXPDT03L PIC S9(0004) COMP.
           05  EXPDT03F PIC  X(0001).
           05  FILLER REDEFINES EXPDT03F.
               10  EXPDT03A PIC  X(0001).
           05  EXPDT03I PIC  X(0008).
      *    -------------------------------
           05  STATE03L PIC S9(0004) COMP.
           05  STATE03F PIC  X(0001).
           05  FILLER REDEFINES STATE03F.
               10  STATE03A PIC  X(0001).
           05  STATE03I PIC  X(0002).
      *    -------------------------------
           05  ACCT04L PIC S9(0004) COMP.
           05  ACCT04F PIC  X(0001).
           05  FILLER REDEFINES ACCT04F.
               10  ACCT04A PIC  X(0001).
           05  ACCT04I PIC  X(0010).
      *    -------------------------------
           05  EFFDT04L PIC S9(0004) COMP.
           05  EFFDT04F PIC  X(0001).
           05  FILLER REDEFINES EFFDT04F.
               10  EFFDT04A PIC  X(0001).
           05  EFFDT04I PIC  X(0008).
      *    -------------------------------
           05  EXPDT04L PIC S9(0004) COMP.
           05  EXPDT04F PIC  X(0001).
           05  FILLER REDEFINES EXPDT04F.
               10  EXPDT04A PIC  X(0001).
           05  EXPDT04I PIC  X(0008).
      *    -------------------------------
           05  STATE04L PIC S9(0004) COMP.
           05  STATE04F PIC  X(0001).
           05  FILLER REDEFINES STATE04F.
               10  STATE04A PIC  X(0001).
           05  STATE04I PIC  X(0002).
      *    -------------------------------
           05  ACCT05L PIC S9(0004) COMP.
           05  ACCT05F PIC  X(0001).
           05  FILLER REDEFINES ACCT05F.
               10  ACCT05A PIC  X(0001).
           05  ACCT05I PIC  X(0010).
      *    -------------------------------
           05  EFFDT05L PIC S9(0004) COMP.
           05  EFFDT05F PIC  X(0001).
           05  FILLER REDEFINES EFFDT05F.
               10  EFFDT05A PIC  X(0001).
           05  EFFDT05I PIC  X(0008).
      *    -------------------------------
           05  EXPDT05L PIC S9(0004) COMP.
           05  EXPDT05F PIC  X(0001).
           05  FILLER REDEFINES EXPDT05F.
               10  EXPDT05A PIC  X(0001).
           05  EXPDT05I PIC  X(0008).
      *    -------------------------------
           05  STATE05L PIC S9(0004) COMP.
           05  STATE05F PIC  X(0001).
           05  FILLER REDEFINES STATE05F.
               10  STATE05A PIC  X(0001).
           05  STATE05I PIC  X(0002).
      *    -------------------------------
           05  ACCT06L PIC S9(0004) COMP.
           05  ACCT06F PIC  X(0001).
           05  FILLER REDEFINES ACCT06F.
               10  ACCT06A PIC  X(0001).
           05  ACCT06I PIC  X(0010).
      *    -------------------------------
           05  EFFDT06L PIC S9(0004) COMP.
           05  EFFDT06F PIC  X(0001).
           05  FILLER REDEFINES EFFDT06F.
               10  EFFDT06A PIC  X(0001).
           05  EFFDT06I PIC  X(0008).
      *    -------------------------------
           05  EXPDT06L PIC S9(0004) COMP.
           05  EXPDT06F PIC  X(0001).
           05  FILLER REDEFINES EXPDT06F.
               10  EXPDT06A PIC  X(0001).
           05  EXPDT06I PIC  X(0008).
      *    -------------------------------
           05  STATE06L PIC S9(0004) COMP.
           05  STATE06F PIC  X(0001).
           05  FILLER REDEFINES STATE06F.
               10  STATE06A PIC  X(0001).
           05  STATE06I PIC  X(0002).
      *    -------------------------------
           05  ACCT07L PIC S9(0004) COMP.
           05  ACCT07F PIC  X(0001).
           05  FILLER REDEFINES ACCT07F.
               10  ACCT07A PIC  X(0001).
           05  ACCT07I PIC  X(0010).
      *    -------------------------------
           05  EFFDT07L PIC S9(0004) COMP.
           05  EFFDT07F PIC  X(0001).
           05  FILLER REDEFINES EFFDT07F.
               10  EFFDT07A PIC  X(0001).
           05  EFFDT07I PIC  X(0008).
      *    -------------------------------
           05  EXPDT07L PIC S9(0004) COMP.
           05  EXPDT07F PIC  X(0001).
           05  FILLER REDEFINES EXPDT07F.
               10  EXPDT07A PIC  X(0001).
           05  EXPDT07I PIC  X(0008).
      *    -------------------------------
           05  STATE07L PIC S9(0004) COMP.
           05  STATE07F PIC  X(0001).
           05  FILLER REDEFINES STATE07F.
               10  STATE07A PIC  X(0001).
           05  STATE07I PIC  X(0002).
      *    -------------------------------
           05  ACCT08L PIC S9(0004) COMP.
           05  ACCT08F PIC  X(0001).
           05  FILLER REDEFINES ACCT08F.
               10  ACCT08A PIC  X(0001).
           05  ACCT08I PIC  X(0010).
      *    -------------------------------
           05  EFFDT08L PIC S9(0004) COMP.
           05  EFFDT08F PIC  X(0001).
           05  FILLER REDEFINES EFFDT08F.
               10  EFFDT08A PIC  X(0001).
           05  EFFDT08I PIC  X(0008).
      *    -------------------------------
           05  EXPDT08L PIC S9(0004) COMP.
           05  EXPDT08F PIC  X(0001).
           05  FILLER REDEFINES EXPDT08F.
               10  EXPDT08A PIC  X(0001).
           05  EXPDT08I PIC  X(0008).
      *    -------------------------------
           05  STATE08L PIC S9(0004) COMP.
           05  STATE08F PIC  X(0001).
           05  FILLER REDEFINES STATE08F.
               10  STATE08A PIC  X(0001).
           05  STATE08I PIC  X(0002).
      *    -------------------------------
           05  ACCT09L PIC S9(0004) COMP.
           05  ACCT09F PIC  X(0001).
           05  FILLER REDEFINES ACCT09F.
               10  ACCT09A PIC  X(0001).
           05  ACCT09I PIC  X(0010).
      *    -------------------------------
           05  EFFDT09L PIC S9(0004) COMP.
           05  EFFDT09F PIC  X(0001).
           05  FILLER REDEFINES EFFDT09F.
               10  EFFDT09A PIC  X(0001).
           05  EFFDT09I PIC  X(0008).
      *    -------------------------------
           05  EXPDT09L PIC S9(0004) COMP.
           05  EXPDT09F PIC  X(0001).
           05  FILLER REDEFINES EXPDT09F.
               10  EXPDT09A PIC  X(0001).
           05  EXPDT09I PIC  X(0008).
      *    -------------------------------
           05  STATE09L PIC S9(0004) COMP.
           05  STATE09F PIC  X(0001).
           05  FILLER REDEFINES STATE09F.
               10  STATE09A PIC  X(0001).
           05  STATE09I PIC  X(0002).
      *    -------------------------------
           05  ACCT10L PIC S9(0004) COMP.
           05  ACCT10F PIC  X(0001).
           05  FILLER REDEFINES ACCT10F.
               10  ACCT10A PIC  X(0001).
           05  ACCT10I PIC  X(0010).
      *    -------------------------------
           05  EFFDT10L PIC S9(0004) COMP.
           05  EFFDT10F PIC  X(0001).
           05  FILLER REDEFINES EFFDT10F.
               10  EFFDT10A PIC  X(0001).
           05  EFFDT10I PIC  X(0008).
      *    -------------------------------
           05  EXPDT10L PIC S9(0004) COMP.
           05  EXPDT10F PIC  X(0001).
           05  FILLER REDEFINES EXPDT10F.
               10  EXPDT10A PIC  X(0001).
           05  EXPDT10I PIC  X(0008).
      *    -------------------------------
           05  STATE10L PIC S9(0004) COMP.
           05  STATE10F PIC  X(0001).
           05  FILLER REDEFINES STATE10F.
               10  STATE10A PIC  X(0001).
           05  STATE10I PIC  X(0002).
      *    -------------------------------
           05  ACCT11L PIC S9(0004) COMP.
           05  ACCT11F PIC  X(0001).
           05  FILLER REDEFINES ACCT11F.
               10  ACCT11A PIC  X(0001).
           05  ACCT11I PIC  X(0010).
      *    -------------------------------
           05  EFFDT11L PIC S9(0004) COMP.
           05  EFFDT11F PIC  X(0001).
           05  FILLER REDEFINES EFFDT11F.
               10  EFFDT11A PIC  X(0001).
           05  EFFDT11I PIC  X(0008).
      *    -------------------------------
           05  EXPDT11L PIC S9(0004) COMP.
           05  EXPDT11F PIC  X(0001).
           05  FILLER REDEFINES EXPDT11F.
               10  EXPDT11A PIC  X(0001).
           05  EXPDT11I PIC  X(0008).
      *    -------------------------------
           05  STATE11L PIC S9(0004) COMP.
           05  STATE11F PIC  X(0001).
           05  FILLER REDEFINES STATE11F.
               10  STATE11A PIC  X(0001).
           05  STATE11I PIC  X(0002).
      *    -------------------------------
           05  ACCT12L PIC S9(0004) COMP.
           05  ACCT12F PIC  X(0001).
           05  FILLER REDEFINES ACCT12F.
               10  ACCT12A PIC  X(0001).
           05  ACCT12I PIC  X(0010).
      *    -------------------------------
           05  EFFDT12L PIC S9(0004) COMP.
           05  EFFDT12F PIC  X(0001).
           05  FILLER REDEFINES EFFDT12F.
               10  EFFDT12A PIC  X(0001).
           05  EFFDT12I PIC  X(0008).
      *    -------------------------------
           05  EXPDT12L PIC S9(0004) COMP.
           05  EXPDT12F PIC  X(0001).
           05  FILLER REDEFINES EXPDT12F.
               10  EXPDT12A PIC  X(0001).
           05  EXPDT12I PIC  X(0008).
      *    -------------------------------
           05  STATE12L PIC S9(0004) COMP.
           05  STATE12F PIC  X(0001).
           05  FILLER REDEFINES STATE12F.
               10  STATE12A PIC  X(0001).
           05  STATE12I PIC  X(0002).
      *    -------------------------------
           05  ACCT13L PIC S9(0004) COMP.
           05  ACCT13F PIC  X(0001).
           05  FILLER REDEFINES ACCT13F.
               10  ACCT13A PIC  X(0001).
           05  ACCT13I PIC  X(0010).
      *    -------------------------------
           05  EFFDT13L PIC S9(0004) COMP.
           05  EFFDT13F PIC  X(0001).
           05  FILLER REDEFINES EFFDT13F.
               10  EFFDT13A PIC  X(0001).
           05  EFFDT13I PIC  X(0008).
      *    -------------------------------
           05  EXPDT13L PIC S9(0004) COMP.
           05  EXPDT13F PIC  X(0001).
           05  FILLER REDEFINES EXPDT13F.
               10  EXPDT13A PIC  X(0001).
           05  EXPDT13I PIC  X(0008).
      *    -------------------------------
           05  STATE13L PIC S9(0004) COMP.
           05  STATE13F PIC  X(0001).
           05  FILLER REDEFINES STATE13F.
               10  STATE13A PIC  X(0001).
           05  STATE13I PIC  X(0002).
      *    -------------------------------
           05  ACCOCCL PIC S9(0004) COMP.
           05  ACCOCCF PIC  X(0001).
           05  FILLER REDEFINES ACCOCCF.
               10  ACCOCCA PIC  X(0001).
           05  ACCOCCI PIC  9(4).
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
       01  EL652DO REDEFINES EL652DI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BANKO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTOPNO PIC  Z,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTOCCO PIC  Z,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT01O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT02O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT03O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT04O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT05O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT06O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT07O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT08O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT09O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT09O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT09O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE09O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDT13O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDT13O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCOCCO PIC  Z(4).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
       01  FILLER REDEFINES EL652DI.
           12  FILLER                  PIC X(88).
           12  BLD-LINE OCCURS 13.
               16  BL-ACCOUNTL         PIC S9(4) COMP.
               16  BL-ACCOUNTA         PIC X.
               16  BL-ACCOUNT          PIC X(10).
               16  BL-EFFECTL          PIC S9(4) COMP.
               16  BL-EFFECTA          PIC X.
               16  BL-EFFECT           PIC X(8).
               16  BL-EXPIREL          PIC S9(4) COMP.
               16  BL-EXPIREA          PIC X.
               16  BL-EXPIRE           PIC X(8).
               16  BL-STATEL           PIC S9(4) COMP.
               16  BL-STATEA           PIC X.
               16  BL-STATE            PIC XX.
           EJECT
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
       01  DFHCOMMAREA                 PIC X(1024).
      *                                COPY ELCCNTL.
      *                                COPY ERCBXRF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCBXRF                             *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = BANK CROSS REFERENCE                      *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 62 - 32,062   RECFORM = VARIABLE
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERBKRF                   RKP=2,LEN=18    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
00021
00022  01  BANK-CROSS-REFERENCE.
00023      12  BK-RECORD-ID                PIC XX.
00024          88  VALID-BK-ID             VALUE 'BK'.
00025
00026      12  BK-CONTROL-PRIMARY.
00027          16  BK-COMPANY-CD           PIC X.
00028          16  BK-CARRIER              PIC X.
00029          16  BK-GROUPING             PIC X(6).
00030          16  BK-BANK-NO              PIC X(10).
00031
00032      12  BK-MAINT-INFORMATION.
00033          16  BK-LAST-MAINT-DT        PIC XX.
00034          16  BK-LAST-MAINT-HHMMSS    PIC S9(7)  COMP-3.
00035          16  BK-LAST-MAINT-USER      PIC X(4).
00036          16  FILLER                  PIC X(9).
00037
00038      12  FILLER                      PIC X(37).
00039
00040      12  BK-BANK-POINTER-CNT         PIC S9(4)  COMP.
00041
00042      12  BK-BANK-POINTER   OCCURS 1 TO 725 TIMES
00043                             DEPENDING ON BK-BANK-POINTER-CNT.
00047          16  BK-AM-ACCOUNT           PIC X(10).
00048          16  BK-AM-EXP-DT            PIC XX.
00051          16  BK-AM-EFF-DT            PIC XX.
00046          16  BK-AM-STATE             PIC XX.
00052          16  FILLER                  PIC X(20).
00053
00054 ******************************************************************
      *                                COPY ERCACCT.
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                BANK-CROSS-REFERENCE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6523' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
           MOVE EIBDATE               TO DC-JULIAN-YYDDD.
           MOVE '5'                   TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
           MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
           MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
           MOVE EIBTRMID              TO QID-TERM.
           
      * EXEC CICS HANDLE CONDITION
      *        ERROR     (9990-ABEND)
      *        MAPFAIL   (8100-SEND-INITIAL-MAP)
      *    END-EXEC.
      *    MOVE '"$.?                  ! " #00001648' TO DFHEIV0
           MOVE X'22242E3F2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031363438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PI-CALLING-PROGRAM NOT = THIS-PGM
               IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
      *  THIS IS THE FIRST TIME THRU
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
                   MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
                   MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
                   MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
                   MOVE THIS-PGM             TO PI-CALLING-PROGRAM
                   PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
               ELSE
      * THIS IS WHEN I COME BACK FROM WHERE I XCTL'D TO
                   MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
                   MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
                   MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
                   MOVE SPACES               TO PI-SAVED-PROGRAM-6
               END-IF
      * ALL SUBSEQUENT TIMES
           END-IF
           IF EIBTRNID = TRANS-ID
               IF EIBAID = DFHCLEAR
                   GO TO 9400-CLEAR
               ELSE
                   GO TO 0200-RECEIVE-MAP.
           IF EIBTRNID  = EL652-TRANS-ID
      * THIS IS WHEN I CAME FROM COMP MAINT
               MOVE DFHENTER       TO EIBAID
               MOVE PI-CR-CARRIER  TO CARRIERI
               MOVE PI-CR-GROUPING TO GROUPI
               MOVE PI-CR-FIN-RESP TO BANKI
               MOVE 'S'            TO MAINTYPI
               MOVE 1              TO CARRIERL
               MOVE 6              TO GROUPL
               MOVE 10             TO BANKL
               MOVE 1              TO MAINTYPL
               MOVE AL-UANON       TO CARRIERA GROUPA BANKA MAINTYPA
               GO TO 4000-EDIT-MAINT
           END-IF
           MOVE LOW-VALUES       TO PI-ERBXRF-KEY    EL652DI.
           MOVE PI-COMPANY-CD    TO PI-BXRF-COMP-CD  WS-COMPANY-CD.
           IF EIBTRNID  = EL650-TRANS-ID
      * THIS IS WHEN I CAME FROM ACCOUNT MAINT
               GO TO 0600-RECOVER-TEMP-STORAGE.
           GO TO 8100-SEND-INITIAL-MAP.
           EJECT
       0200-RECEIVE-MAP.
           MOVE LOW-VALUES             TO EL652DI
           
      * EXEC CICS RECEIVE
      *        MAP (MAP-NAME)
      *        MAPSET (MAPSET-NAME)
      *        INTO (EL652DI)
      *    END-EXEC
           MOVE LENGTH OF
            EL652DI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001705' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL652DI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF PFENTERL = 0
              GO TO 0300-CHECK-PFKEYS
           END-IF
           IF EIBAID NOT = DFHENTER
              MOVE ER-0004             TO EMI-ERROR
              GO TO 0320-INPUT-ERROR
           END-IF
           IF (PFENTERI NUMERIC) AND (PFENTERI > 0 AND < 25)
              MOVE PF-VALUES (PFENTERI)
                                       TO EIBAID
           ELSE
              MOVE ER-0029             TO EMI-ERROR
              GO TO 0320-INPUT-ERROR
           END-IF
           .
       0300-CHECK-PFKEYS.
           IF EIBAID = DFHPF23
               GO TO 8810-PF23.
           IF EIBAID = DFHPF24
               GO TO 9200-RETURN-MAIN-MENU.
           IF EIBAID = DFHPF12
               GO TO 9500-PF12.
           IF EIBAID = DFHPF1 OR DFHPF2
              GO TO 5000-BROWSE-FILE
           END-IF
           IF EIBAID = DFHPF3 OR DFHPF4
               GO TO 6000-OCCURRENCE.
           IF EIBAID = DFHPF5
               GO TO 4000-ACCT-MAINT.
           IF EIBAID = DFHENTER
              GO TO 4000-EDIT-MAINT
           END-IF
           MOVE ER-0029                TO EMI-ERROR
           .
       0320-INPUT-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE -1                     TO CARRIERL.
           GO TO 8200-SEND-DATAONLY.
           EJECT
       0500-CREATE-TEMP-STORAGE.
           
      * EXEC CICS WRITEQ TS
      *        QUEUE   (QID)
      *        FROM    (EL652DI)
      *        LENGTH  (QID-MAP-LENGTH)
      *        END-EXEC.
      *    MOVE '*"                    ''   #00001750' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL652DI, 
                 QID-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       0599-EXIT.
            EXIT.
       0600-RECOVER-TEMP-STORAGE.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND  (1500-BXRF-NOT-FOUND)
      *        QIDERR  (0690-QIDERR)
      *        END-EXEC.
      *    MOVE '"$IN                  ! # #00001758' TO DFHEIV0
           MOVE X'2224494E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031373538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READQ TS
      *        QUEUE    (QID)
      *        INTO     (EL652DI)
      *        LENGTH   (QID-MAP-LENGTH)
      *        END-EXEC.
      *    MOVE '*$I    L              ''   #00001762' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL652DI, 
                 QID-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS DELETEQ TS
      *        QUEUE   (QID)
      *    END-EXEC
      *    MOVE '*&                    #   #00001767' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF CARRIERL NOT = 0
               MOVE AL-UANON TO CARRIERA.
           IF GROUPL NOT = 0
               MOVE AL-UANON TO GROUPA.
           IF BANKL NOT = 0
               MOVE AL-UANON TO BANKA.
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD.
           MOVE CARRIERO               TO WS-CARRIER.
           MOVE GROUPO                 TO WS-GROUPING.
           MOVE BANKO                  TO WS-BANK.
           GO TO 1050-READ-BANK.
       0690-QIDERR.
           MOVE ER-0033                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           GO TO 8100-SEND-INITIAL-MAP
           .
       4000-EDIT-MAINT.
           IF MAINTYPL > ZERO
              MOVE MAINTYPI            TO PI-CHECK-MAINT-TYPE
              IF VALID-MAINT-TYPE
                 MOVE AL-UANON         TO MAINTYPA
              ELSE
                 MOVE -1               TO MAINTYPL
                 MOVE AL-UABON         TO MAINTYPA
                 MOVE ER-2039          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              END-IF
           ELSE
              MOVE -1                  TO MAINTYPL
              MOVE AL-UABON            TO MAINTYPA
              MOVE ER-2039             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           MOVE PI-COMPANY-CD          TO PI-ERC-COMPANY-CD
           IF (NOT MODIFY-CAP)
              AND (NOT SHOW-FUNCTION)
              MOVE 'UPDATE'            TO SM-READ
              PERFORM 9995-SECURITY-VIOLATION
                                       THRU 9995-EXIT
              MOVE ER-0070             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8100-SEND-INITIAL-MAP
           END-IF
           IF CARRIERL > ZERO
              IF PI-CARRIER-SECURITY > SPACES
                 IF CARRIERI = PI-CARRIER-SECURITY
                    CONTINUE
                 ELSE
                    MOVE -1            TO CARRIERL
                    MOVE ER-2370       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    MOVE AL-UABON      TO CARRIERA
                    GO TO 8200-SEND-DATAONLY
                 END-IF
              END-IF
           END-IF
           IF CARRIERL > ZERO
               IF ADD-FUNCTION
                   IF PI-ZERO-CARRIER
                     OR PI-ZERO-CAR-GROUP
                       MOVE ZEROS      TO  PI-ERC-CARRIER
                                           CARRIERI
                       MOVE AL-UANON   TO  CARRIERA
                   ELSE
                       MOVE CARRIERI   TO  WS-ACARRIER
                                           PI-ERC-CARRIER
                       MOVE '6'        TO  CNTL-REC-TYPE
                       PERFORM 7400-READ-CONTROL-FILE  THRU  7499-EXIT
               ELSE
                   IF PI-ZERO-CARRIER
                     OR PI-ZERO-CAR-GROUP
                       MOVE ZEROS      TO  PI-ERC-CARRIER
                                           CARRIERI
                       MOVE AL-UANON   TO  CARRIERA
                   ELSE
                       MOVE AL-UANON   TO  CARRIERA
                       MOVE CARRIERI   TO  PI-ERC-CARRIER
           ELSE
               IF ADD-FUNCTION
                   IF PI-ZERO-CARRIER
                     OR PI-ZERO-CAR-GROUP
                       MOVE ZEROS      TO  PI-ERC-CARRIER
                                           CARRIERI
                       MOVE AL-UANON   TO  CARRIERA
                   ELSE
                       MOVE -1         TO  CARRIERL
                       MOVE AL-UABON   TO  CARRIERA
                       MOVE ER-0193    TO  EMI-ERROR
                       PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
               ELSE
                   MOVE -1             TO  CARRIERL
                   MOVE AL-UABON       TO  CARRIERA
                   MOVE ER-0193        TO  EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
           IF GROUPL > ZERO
               IF PI-ZERO-GROUPING
                 OR PI-ZERO-CAR-GROUP
                   MOVE ZEROS          TO  PI-ERC-GROUP
                                           GROUPI
                   MOVE AL-UANON       TO  GROUPA
               ELSE
                   MOVE AL-UANON       TO  GROUPA
                   MOVE GROUPI         TO  PI-ERC-GROUP
           ELSE
               IF ADD-FUNCTION
                   IF PI-ZERO-GROUPING
                     OR PI-ZERO-CAR-GROUP
                       MOVE ZEROS      TO  PI-ERC-GROUP
                                           GROUPI
                       MOVE AL-UANON   TO  GROUPA
                   ELSE
                       MOVE LOW-VALUES  TO  PI-ERC-GROUP
               ELSE
                   MOVE LOW-VALUES     TO  PI-ERC-GROUP.
           IF BANKL > ZERO
               MOVE AL-UANON           TO BANKA
               MOVE BANKI              TO PI-ERC-BANK
           ELSE
               MOVE LOW-VALUES         TO PI-ERC-BANK
           END-IF
           IF NOT MODIFY-CAP
               IF SHOW-FUNCTION
                   GO TO 1000-EDIT-INPUT
               ELSE
                   MOVE 'UPDATE'       TO SM-READ
                   PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
                   MOVE ER-0070        TO  EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                   GO TO 8100-SEND-INITIAL-MAP.
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              IF EIBTRNID NOT = TRANS-ID
                 GO TO 8100-SEND-INITIAL-MAP
              ELSE
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF
           IF CHANGE-FUNCTION
              GO TO 4400-CHANGE
           END-IF
           IF DELETE-FUNCTION
              GO TO 4600-DELETE
           END-IF
           IF SHOW-FUNCTION
              GO TO 1000-EDIT-INPUT
           END-IF
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF
           IF ADD-FUNCTION
              GO TO 4200-ADD
           END-IF
           MOVE -1                     TO MAINTYPL
           MOVE ER-2056                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           GO TO 8200-SEND-DATAONLY
           .
       4000-EXIT.
           EXIT.
       EJECT
       1000-EDIT-INPUT.
           IF CARRIERL NOT = ZEROS
              MOVE CARRIERI            TO WS-CARRIER
           ELSE
              MOVE ER-0234             TO EMI-ERROR
              MOVE -1                  TO CARRIERL
              MOVE AL-UABON            TO CARRIERA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           IF GROUPL NOT = ZEROS
              MOVE GROUPI              TO WS-GROUPING
           ELSE
              MOVE ER-0235             TO EMI-ERROR
              MOVE -1                  TO GROUPL
              MOVE AL-UABON            TO GROUPA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           IF BANKL  NOT = ZEROS
              MOVE BANKI               TO WS-BANK
           ELSE
              MOVE ER-6508             TO EMI-ERROR
              MOVE -1                  TO BANKL
              MOVE AL-UABON            TO BANKA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           .
       1050-READ-BANK.
           
      * EXEC CICS HANDLE CONDITION
      *         NOTFND (1500-BXRF-NOT-FOUND)
      *    END-EXEC
      *    MOVE '"$I                   ! $ #00001971' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031393731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           PERFORM 7050-READ-ERBXRF    THRU 7050-EXIT
           MOVE BK-LAST-MAINT-USER     TO PI-UPDATE-BY
           IF BK-LAST-MAINT-HHMMSS NUMERIC
              MOVE BK-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF
           MOVE WS-CONTROL-PRIMARY     TO PI-SAVE-ERBXRF-KEY
           MOVE +1                     TO PI-NDX
                                          PI-TOP-NDX
           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           GO TO 5200-FORMAT-SCREEN
           .
       1500-BXRF-NOT-FOUND.
           MOVE ER-0142                TO EMI-ERROR
           MOVE -1                     TO CARRIERL
           MOVE AL-UABON               TO CARRIERA
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           IF EIBTRNID  = EL652-TRANS-ID
              GO TO 8100-SEND-INITIAL-MAP
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF
           .
       3000-BLD-LINE.
           MOVE 1                      TO NDX
           .
       3000-BLD-LINE-LOOP.
           IF PI-NDX > BK-BANK-POINTER-CNT
              GO TO 3000-XIT
           END-IF
           MOVE BK-AM-ACCOUNT (PI-NDX) TO BL-ACCOUNT (NDX)
           IF BK-AM-EFF-DT (PI-NDX) NOT = SPACES AND LOW-VALUES
              MOVE BK-AM-EFF-DT (PI-NDX)
                                       TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-EDIT TO BL-EFFECT (NDX)
           END-IF
           IF BK-AM-EXP-DT (PI-NDX) NOT = SPACES AND LOW-VALUES
              AND HIGH-VALUES
              MOVE BK-AM-EXP-DT (PI-NDX)
                                       TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-EDIT TO BL-EXPIRE (NDX)
           END-IF
           IF BK-AM-EXP-DT (PI-NDX) = HIGH-VALUES
              MOVE '99/99/99'          TO BL-EXPIRE (NDX)
            END-IF
      *    MOVE BK-AM-LEVEL-NO (PI-NDX)       TO BL-LEVEL.
           MOVE BK-AM-STATE (PI-NDX)   TO BL-STATE (NDX)
           IF (PI-NDX = BK-BANK-POINTER-CNT)  OR  (NDX = 13)
               GO TO 3000-XIT
           END-IF
           ADD 1                       TO PI-NDX NDX
           GO TO 3000-BLD-LINE-LOOP
           .
       3000-XIT.
           EXIT.
           EJECT
       4000-ACCT-MAINT.
           IF (CARRIERI    NOT = PI-BXRF-CARRIER)
              OR (GROUPI   NOT = PI-BXRF-GROUPING)
              OR (BANKI    NOT = PI-BXRF-BANK)
              MOVE ER-5005             TO EMI-ERROR
              GO TO 4050-ERR
           END-IF
           IF ACCOCCI NOT NUMERIC
              GO TO 4010-ERROR
           END-IF
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           MOVE CARRIERI               TO WS-CARRIER
           MOVE GROUPI                 TO WS-GROUPING
           MOVE BANKI                  TO WS-BANK
           
      * EXEC CICS HANDLE CONDITION
      *         NOTFND (4010-ERROR)
      *    END-EXEC
      *    MOVE '"$I                   ! % #00002053' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303032303533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           PERFORM 7050-READ-ERBXRF    THRU 7050-EXIT
           MOVE ACCOCCI                TO NDX
           COMPUTE NDX = PI-TOP-NDX + NDX - 1
           IF NDX  NOT > BK-BANK-POINTER-CNT
              MOVE BK-CARRIER          TO PI-CR-CARRIER
              MOVE BK-GROUPING         TO PI-CR-GROUPING
              MOVE BK-AM-STATE (NDX)   TO PI-CR-STATE
              MOVE BK-AM-ACCOUNT (NDX) TO PI-CR-ACCOUNT
              PERFORM 0500-CREATE-TEMP-STORAGE
                                       THRU 0599-EXIT
              MOVE XCTL-650            TO PGM-NAME
              GO TO 9300-XCTL
           END-IF
           .
       4010-ERROR.
            MOVE ER-5004 TO EMI-ERROR.
       4050-ERR.
            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
            MOVE -1                     TO ACCOCCL.
            MOVE AL-UNBON               TO ACCOCCA.
            GO TO 8200-SEND-DATAONLY.
           EJECT
       4200-ADD.
      *    PERFORM 7000-EDIT           THRU 7000-EXIT
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF
           
      * EXEC CICS HANDLE CONDITION
      *        NOTOPEN  (9990-ABEND)
      *        NOTFND   (4250-CONT)
      *    END-EXEC.
      *    MOVE '"$JI                  ! & #00002085' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303032303835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 7050-READ-ERBXRF    THRU 7050-EXIT
           MOVE ER-2057                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE LOW-VALUES             TO PI-SAVE-ERBXRF-KEY
           MOVE -1                     TO  MAINTYPL
           GO TO 8200-SEND-DATAONLY
           .
       4250-CONT.
           
      * EXEC CICS GETMAIN
      *        SET      (ADDRESS OF BANK-CROSS-REFERENCE)
      *        LENGTH   (BXRF-REC-LENGTH)
      *        INITIMG  (GETMAIN-SPACE)
      *    END-EXEC
      *    MOVE ',"IL                  $   #00002098' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 BXRF-REC-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF BANK-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE SPACES                 TO BANK-CROSS-REFERENCE
           MOVE PI-COMPANY-CD          TO BK-COMPANY-CD
           MOVE 'BK'                   TO BK-RECORD-ID
           MOVE PI-ERC-KEY             TO BK-CONTROL-PRIMARY
           MOVE +1                     TO PI-NDX
           SET NO-CHANGES-MADE         TO TRUE
           PERFORM 7300-EDIT-DATA      THRU 7300-EXIT
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              
      * EXEC CICS UNLOCK
      *            DATASET  (BXRF-FILE-ID)
      *       END-EXEC
      *    MOVE '&*                    #   #00002113' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              GO TO 8200-SEND-DATAONLY
           END-IF
           PERFORM 7325-COMPRESS       THRU 7325-EXIT
           MOVE PI-NDX                 TO BK-BANK-POINTER-CNT
           PERFORM 7510-SORT-ACCOUNTS  THRU 7599-EXIT
           MOVE PI-PROCESSOR-ID        TO BK-LAST-MAINT-USER
           MOVE EIBTIME                TO BK-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO BK-LAST-MAINT-DT
           
      * EXEC CICS WRITE
      *        DATASET  (BXRF-FILE-ID)
      *        FROM     (BANK-CROSS-REFERENCE)
      *        RIDFLD   (BK-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE LENGTH OF
            BANK-CROSS-REFERENCE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00002124' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 BANK-CROSS-REFERENCE, 
                 DFHEIV11, 
                 BK-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE LOW-VALUES             TO EL652DO
           MOVE 'S'                    TO MAINTYPO
           MOVE +1                     TO MAINTYPL
           MOVE AL-UANON               TO MAINTYPA
           MOVE PI-ERC-CARRIER         TO CARRIERO
           MOVE AL-UANON               TO CARRIERA
           MOVE +1                     TO CARRIERL
           IF PI-ERC-GROUP NOT = SPACES
               MOVE PI-ERC-GROUP       TO GROUPO
               MOVE AL-UANON           TO GROUPA
               MOVE +6                 TO GROUPL
           END-IF
           IF PI-ERC-BANK NOT = SPACES
              MOVE PI-ERC-BANK         TO BANKO
              MOVE AL-UANON            TO BANKA
              MOVE +10                 TO BANKL
           END-IF
           GO TO 1000-EDIT-INPUT
           .
       4200-EXIT.
           EXIT.
       4400-CHANGE.
           IF PI-ERC-KEY = PI-SAVE-ERBXRF-KEY
              CONTINUE
           ELSE
              MOVE ER-2056             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 7200-READ-ERBXRF-UPDATE
                                       THRU 7200-EXIT
           IF (BK-LAST-MAINT-USER = PI-UPDATE-BY)
              AND (BK-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
              CONTINUE
           ELSE
              
      * EXEC CICS UNLOCK
      *            DATASET  (BXRF-FILE-ID)
      *       END-EXEC
      *    MOVE '&*                    #   #00002167' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              MOVE ER-0068             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-TOP-NDX             TO PI-NDX
           SET NO-CHANGES-MADE         TO TRUE
           PERFORM 7300-EDIT-DATA      THRU 7300-EXIT
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              
      * EXEC CICS UNLOCK
      *            DATASET  (BXRF-FILE-ID)
      *       END-EXEC
      *    MOVE '&*                    #   #00002181' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              GO TO 8200-SEND-DATAONLY
           END-IF
           IF NO-CHANGES-MADE
              MOVE ER-3112             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF
           COMPUTE PI-NDX = BK-BANK-POINTER-CNT + +13
           PERFORM 7325-COMPRESS       THRU 7325-EXIT
           MOVE PI-NDX                 TO BK-BANK-POINTER-CNT
           PERFORM 7510-SORT-ACCOUNTS  THRU 7599-EXIT
           MOVE PI-PROCESSOR-ID        TO BK-LAST-MAINT-USER
           MOVE EIBTIME                TO BK-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO BK-LAST-MAINT-DT
           
      * EXEC CICS REWRITE
      *        DATASET  (BXRF-FILE-ID)
      *        FROM     (BANK-CROSS-REFERENCE)
      *    END-EXEC
           MOVE LENGTH OF
            BANK-CROSS-REFERENCE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002200' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 BANK-CROSS-REFERENCE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           GO TO 1000-EDIT-INPUT
           .
       4600-DELETE.
           IF PI-ERC-KEY = PI-SAVE-ERBXRF-KEY
              CONTINUE
           ELSE
              MOVE ER-2056             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 7200-READ-ERBXRF-UPDATE
                                       THRU 7200-EXIT
           IF (BK-LAST-MAINT-USER = PI-UPDATE-BY)
              AND (BK-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
              CONTINUE
           ELSE
              
      * EXEC CICS UNLOCK
      *            DATASET  (BXRF-FILE-ID)
      *       END-EXEC
      *    MOVE '&*                    #   #00002223' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              MOVE ER-0068             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           
      * EXEC CICS DELETE
      *        DATASET  (BXRF-FILE-ID)
      *    END-EXEC
      *    MOVE '&(                    &   #00002231' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE ER-0000                TO  EMI-ERROR
           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
           MOVE LOW-VALUES             TO  EL652DO
           MOVE PI-ERC-CARRIER         TO  CARRIERO
           MOVE AL-UANON               TO  CARRIERA
           IF PI-ERC-GROUP NOT = SPACES
               MOVE PI-ERC-GROUP       TO  GROUPO
               MOVE AL-UANON           TO  GROUPA
           END-IF
           IF PI-ERC-BANK NOT = SPACES
              MOVE PI-ERC-BANK        TO  BANKO
              MOVE AL-UANON           TO  BANKA
           END-IF
           MOVE LOW-VALUES             TO  PI-SAVE-ERBXRF-KEY
           GO TO 8100-SEND-INITIAL-MAP
           .
       4600-EXIT.
           EXIT.
       5000-BROWSE-FILE.
           MOVE SPACE                  TO BROWSE-STARTED-SW
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (5800-NO-RECORD)
      *        ENDFILE (5900-END-OF-FILE)
      *    END-EXEC
      *    MOVE '"$I''                  ! '' #00002254' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303032323534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE PI-SAVE-ERBXRF-KEY     TO WS-CONTROL-PRIMARY
           
      * EXEC CICS STARTBR
      *        DATASET (BXRF-FILE-ID)
      *        RIDFLD (WS-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002259' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE 'Y'                    TO BROWSE-STARTED-SW
           IF EIBAID = DFHPF2
              GO TO 5100-BROWSE-BKWD
           END-IF
           .
       5010-READ-LOOP.
           MOVE BXRF-MAX-REC-LENGTH    TO BXRF-REC-LENGTH
           
      * EXEC CICS READNEXT
      *        DATASET (BXRF-FILE-ID)
      *        SET     (ADDRESS OF BANK-CROSS-REFERENCE)
      *        RIDFLD  (WS-CONTROL-PRIMARY)
      *        LENGTH  (BXRF-REC-LENGTH)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.SL                  )   #00002270' TO DFHEIV0
           MOVE X'262E534C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 DFHEIV20, 
                 BXRF-REC-LENGTH, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BANK-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE BK-BANK-POINTER-CNT    TO BK-BANK-POINTER-CNT
           MOVE +1                     TO PI-NDX
           IF BK-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 5900-END-OF-FILE
           END-IF
           IF EIBAID = DFHPF1
              IF CARRIERO     = BK-CARRIER
                 AND GROUPO   = BK-GROUPING
                 AND BANKO    = BK-BANK-NO
                 GO TO 5010-READ-LOOP
              END-IF
           END-IF
           MOVE BK-LAST-MAINT-USER     TO PI-UPDATE-BY
           IF BK-LAST-MAINT-HHMMSS NUMERIC
              MOVE BK-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF
           MOVE BK-CONTROL-PRIMARY     TO PI-SAVE-ERBXRF-KEY
           GO TO 5200-FORMAT-SCREEN
           .
       5100-BROWSE-BKWD.
           
      * EXEC CICS HANDLE CONDITION
      *         NOTFND (5900-END-OF-FILE)
      *    END-EXEC
      *    MOVE '"$I                   ! ( #00002299' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303032323939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE BXRF-MAX-REC-LENGTH    TO BXRF-REC-LENGTH
           
      * EXEC CICS READPREV
      *        DATASET (BXRF-FILE-ID)
      *        SET     (ADDRESS OF BANK-CROSS-REFERENCE)
      *        LENGTH  (BXRF-REC-LENGTH)
      *        RIDFLD  (WS-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0SL                  )   #00002303' TO DFHEIV0
           MOVE X'2630534C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 DFHEIV20, 
                 BXRF-REC-LENGTH, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BANK-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE BXRF-MAX-REC-LENGTH    TO BXRF-REC-LENGTH
           IF PI-FILE-EOF
              MOVE SPACE               TO PI-EOF-SW
           ELSE
              
      * EXEC CICS READPREV
      *            DATASET (BXRF-FILE-ID)
      *            SET     (ADDRESS OF BANK-CROSS-REFERENCE)
      *            LENGTH  (BXRF-REC-LENGTH)
      *            RIDFLD  (WS-CONTROL-PRIMARY)
      *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0SL                  )   #00002313' TO DFHEIV0
           MOVE X'2630534C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 DFHEIV20, 
                 BXRF-REC-LENGTH, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BANK-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           MOVE BK-BANK-POINTER-CNT TO BK-BANK-POINTER-CNT
           MOVE +1                     TO PI-NDX
           IF BK-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 5900-END-OF-FILE
           END-IF
           MOVE BK-LAST-MAINT-USER     TO PI-UPDATE-BY
           IF BK-LAST-MAINT-HHMMSS NUMERIC
              MOVE BK-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF
           MOVE BK-CONTROL-PRIMARY     TO PI-SAVE-ERBXRF-KEY
           GO TO 5200-FORMAT-SCREEN
           .
       5200-FORMAT-SCREEN.
           MOVE LOW-VALUES             TO EL652DI
           MOVE BK-CONTROL-PRIMARY     TO PI-ERBXRF-KEY
           MOVE BK-CARRIER             TO CARRIERO
           MOVE BK-GROUPING            TO GROUPO
           MOVE BK-BANK-NO             TO BANKO
           MOVE BK-BANK-POINTER-CNT    TO TOTOCCO
           PERFORM 7500-COUNT-OPEN-RANGES
                                       THRU 7500-EXIT
           MOVE WS-OPEN-COUNT          TO TOTOPNO
           IF BK-LAST-MAINT-DT NOT = SPACES AND LOW-VALUES AND ZEROS
              MOVE BK-LAST-MAINT-DT    TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-EDIT TO MAINTDTO
           ELSE
              MOVE '00/00/00'          TO MAINTDTO
            END-IF
           PERFORM 3000-BLD-LINE       THRU 3000-XIT
           MOVE AL-UANON               TO CARRIERA GROUPA
                                          BANKA
           GO TO 8100-SEND-INITIAL-MAP
           .
       5800-NO-RECORD.
           MOVE -1                     TO CARRIERL.
           MOVE AL-UANON               TO CARRIERA   GROUPA
                                           BANKA
           MOVE ER-1164                TO EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF BROWSE-STARTED
             
      * EXEC CICS ENDBR
      *           DATASET  (BXRF-FILE-ID)
      *           END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002366' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 8200-SEND-DATAONLY.
       5900-END-OF-FILE.
           IF EIBAID = DFHPF1
               MOVE 'Y'                TO PI-EOF-SW
               MOVE ER-2237            TO EMI-ERROR
           ELSE
               MOVE LOW-VALUES         TO PI-ERBXRF-KEY
               MOVE PI-COMPANY-CD      TO PI-BXRF-COMP-CD
               MOVE ER-2238            TO EMI-ERROR.
           MOVE -1            TO CARRIERL.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF BROWSE-STARTED
             
      * EXEC CICS ENDBR
      *           DATASET  (BXRF-FILE-ID)
      *           END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002381' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BXRF-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE SPACE TO BROWSE-STARTED-SW.
           GO TO 8200-SEND-DATAONLY.
           EJECT
       6000-OCCURRENCE.
           MOVE PI-ERBXRF-KEY          TO WS-CONTROL-PRIMARY
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (1500-BXRF-NOT-FOUND)
      *    END-EXEC
      *    MOVE '"$I                   ! ) #00002389' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303032333839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           PERFORM 7050-READ-ERBXRF    THRU 7050-EXIT
           IF EIBAID = DFHPF3
              GO TO 6500-NEXT-OCC
           ELSE
              GO TO 7000-PRIOR-OCC
           END-IF
           .
       6500-NEXT-OCC.
      *    IF PI-NDX = BK-BANK-POINTER-CNT
      *       MOVE -1                  TO CARRIERL
      *       MOVE ER-0130             TO EMI-ERROR
      *       PERFORM 9900-ERROR-FORMAT
      *                                THRU 9900-EXIT
      *       GO TO 8200-SEND-DATAONLY
      *    END-IF
           ADD +1                      TO PI-NDX
           MOVE PI-NDX                 TO PI-TOP-NDX
           GO TO 5200-FORMAT-SCREEN
           .
       7000-PRIOR-OCC.
           IF PI-NDX LESS 14
              MOVE -1                  TO CARRIERL
              MOVE ER-0131             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           COMPUTE PI-NDX = (PI-NDX - 1) / 13
           IF PI-NDX NOT = 1
              COMPUTE PI-NDX = PI-NDX * 13
              COMPUTE PI-NDX = PI-NDX - 12
           END-IF
           MOVE PI-NDX                 TO PI-TOP-NDX
           GO TO 5200-FORMAT-SCREEN
           .
       7050-READ-ERBXRF.
           MOVE BXRF-MAX-REC-LENGTH    TO BXRF-REC-LENGTH
           
      * EXEC CICS READ
      *         SET     (ADDRESS OF BANK-CROSS-REFERENCE)
      *         DATASET ('ERBXRF')
      *         LENGTH  (BXRF-REC-LENGTH)
      *         RIDFLD  (WS-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE 'ERBXRF' TO DFHEIV1
      *    MOVE '&"SL       E          (   #00002429' TO DFHEIV0
           MOVE X'2622534C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 BXRF-REC-LENGTH, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BANK-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE BK-BANK-POINTER-CNT    TO BK-BANK-POINTER-CNT
           .
       7050-EXIT.
           EXIT.
       7200-READ-ERBXRF-UPDATE.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (7200-BXRF-NOT-FOUND)
      *    END-EXEC
      *    MOVE '"$I                   ! * #00002440' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303032343430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE BXRF-MAX-REC-LENGTH    TO BXRF-REC-LENGTH
           
      * EXEC CICS READ
      *         SET     (ADDRESS OF BANK-CROSS-REFERENCE)
      *         DATASET ('ERBXRF')
      *         LENGTH  (BXRF-REC-LENGTH)
      *         RIDFLD  (WS-CONTROL-PRIMARY)
      *         UPDATE
      *    END-EXEC
           MOVE 'ERBXRF' TO DFHEIV1
      *    MOVE '&"SL       EU         (   #00002444' TO DFHEIV0
           MOVE X'2622534C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 BXRF-REC-LENGTH, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BANK-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE BK-BANK-POINTER-CNT    TO BK-BANK-POINTER-CNT
           .
       7200-EXIT.
           EXIT.
           .
       7200-BXRF-NOT-FOUND.
           MOVE ER-0142                TO EMI-ERROR
           MOVE -1                     TO CARRIERL
           MOVE AL-UABON               TO CARRIERA
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           IF EIBTRNID  = EL652-TRANS-ID
              GO TO 8100-SEND-INITIAL-MAP
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF
           .
       7300-EDIT-DATA.
           SET NO-LINE-CHANGES         TO TRUE
           PERFORM VARYING NDX FROM +1 BY +1 UNTIL
              (NDX > +13)
              IF BL-ACCOUNTL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE BL-ACCOUNT (NDX) TO BK-AM-ACCOUNT (PI-NDX)
              END-IF
              IF BL-STATEL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE BL-STATE (NDX)   TO BK-AM-STATE (PI-NDX)
              END-IF
              IF BL-EFFECTL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE BL-EFFECT (NDX)  TO WS-DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 MOVE WS-DEEDIT-FIELD-DATE-OUT
                                       TO  DC-GREG-DATE-1-MDY
                 MOVE '4'              TO  DC-OPTION-CODE
                 PERFORM 9700-DATE-CONVERSION
                 IF DATE-CONVERSION-ERROR
                    MOVE ER-0348              TO EMI-ERROR
                    MOVE -1                   TO BL-EFFECTL (NDX)
                    MOVE AL-UABON             TO BL-EFFECTA (NDX)
                    PERFORM 9900-ERROR-FORMAT THRU  9900-EXIT
                 ELSE
                    MOVE DC-GREG-DATE-1-EDIT  TO BL-EFFECT (NDX)
                    MOVE AL-UANON             TO BL-EFFECTA (NDX)
                    MOVE DC-BIN-DATE-1        TO BK-AM-EFF-DT (PI-NDX)
                 END-IF
              END-IF
              IF BL-EXPIREL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE BL-EXPIRE (NDX)  TO WS-DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-DEEDIT-FIELD-DATE-OUT = 999999
                    MOVE HIGH-VALUES   TO BK-AM-EXP-DT (PI-NDX)
                    MOVE '99/99/99'    TO BL-EXPIRE (NDX)
                    MOVE AL-UANON      TO BL-EXPIREA (NDX)
                 ELSE
                    MOVE WS-DEEDIT-FIELD-DATE-OUT
                                       TO DC-GREG-DATE-1-MDY
                    MOVE '4'           TO DC-OPTION-CODE
                    PERFORM 9700-DATE-CONVERSION
                    IF DATE-CONVERSION-ERROR
                       MOVE ER-0454    TO EMI-ERROR
                       MOVE -1         TO BL-EXPIREL (NDX)
                       MOVE AL-UABON   TO BL-EXPIREA (NDX)
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    ELSE
                       MOVE DC-GREG-DATE-1-EDIT
                                       TO BL-EXPIRE (NDX)
                       MOVE AL-UANON   TO BL-EXPIREA (NDX)
                       MOVE DC-BIN-DATE-1
                                       TO BK-AM-EXP-DT (PI-NDX)
                    END-IF
                 END-IF
              END-IF
              IF LINE-CHANGES
                 SET CHANGES-MADE      TO TRUE
                 IF BK-AM-EXP-DT (PI-NDX) NOT > BK-AM-EFF-DT (PI-NDX)
                    MOVE ER-2339       TO EMI-ERROR
                    MOVE -1            TO BL-EXPIREL (NDX)
                    MOVE AL-UABON      TO BL-EXPIREA (NDX)
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
                 PERFORM 7350-VERIFY-ACCOUNT
                                       THRU 7350-EXIT
              END-IF
              ADD +1                   TO PI-NDX
              SET NO-LINE-CHANGES      TO TRUE
           END-PERFORM
           .
       7300-EXIT.
           EXIT.
       7325-COMPRESS.
           PERFORM VARYING WSUB1 FROM +1 BY +1 UNTIL
              (WSUB1 > PI-NDX)
              IF BK-AM-ACCOUNT (WSUB1) = SPACES OR LOW-VALUES OR ZEROS
                 PERFORM VARYING WSUB2 FROM WSUB1 BY +1 UNTIL
                    (BK-AM-ACCOUNT (WSUB2) NOT = SPACES AND LOW-VALUES
                                             AND ZEROS)
                    OR (WSUB2 > PI-NDX)
                 END-PERFORM
                 IF WSUB2 NOT > PI-NDX
                    MOVE BK-AM-ACCOUNT (WSUB2)
                                       TO BK-AM-ACCOUNT (WSUB1)
                    MOVE BK-AM-EFF-DT (WSUB2)
                                       TO BK-AM-EFF-DT (WSUB1)
                    MOVE BK-AM-EXP-DT (WSUB2)
                                       TO BK-AM-EXP-DT (WSUB1)
                    MOVE BK-AM-STATE (WSUB2)
                                       TO BK-AM-STATE (WSUB1)
                    MOVE LOW-VALUES    TO BK-AM-ACCOUNT (WSUB2)
                                          BK-AM-EFF-DT  (WSUB2)
                                          BK-AM-EXP-DT  (WSUB2)
                                          BK-AM-STATE   (WSUB2)
                 END-IF
              END-IF
           END-PERFORM
           PERFORM VARYING WSUB1 FROM +1 BY +1 UNTIL
              BK-AM-ACCOUNT (WSUB1) = SPACES OR LOW-VALUES
           END-PERFORM
           COMPUTE PI-NDX = WSUB1 - +1
           .
       7325-EXIT.
           EXIT.
       7350-VERIFY-ACCOUNT.
           MOVE LOW-VALUES             TO WS-ERACCT-KEY
           MOVE BK-COMPANY-CD          TO WS-ERACCT-COMP-CD
           MOVE BK-CARRIER             TO WS-ERACCT-CARRIER
           MOVE BK-GROUPING            TO WS-ERACCT-GROUPING
           MOVE BK-AM-STATE (PI-NDX)   TO WS-ERACCT-STATE
           MOVE BK-AM-ACCOUNT (PI-NDX) TO WS-ERACCT-ACCOUNT
           MOVE BK-AM-EXP-DT (PI-NDX)  TO WS-ERACCT-EXP-DT
           IF BK-AM-ACCOUNT (PI-NDX) NOT = SPACES AND ZEROS
              
      * EXEC CICS READ
      *          DATASET  ('ERACCT')
      *          SET      (ADDRESS OF ACCOUNT-MASTER)
      *          RIDFLD   (WS-ERACCT-KEY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00002585' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032353835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF (RESP-NORMAL)
                 AND (BK-AM-EFF-DT (PI-NDX) = AM-EFFECTIVE-DT)
                 CONTINUE
              ELSE
                 MOVE ER-0226          TO EMI-ERROR
                 MOVE -1               TO BL-ACCOUNTL (NDX)
                 MOVE AL-UABON         TO BL-ACCOUNTA (NDX)
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
           .
       7350-EXIT.
           EXIT.
       7400-READ-CONTROL-FILE.
           MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
           MOVE WS-ACCESS              TO CNTL-ACCESS
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND  (7490-NOT-FOUND)
      *        ERROR   (9990-ABEND)
      *    END-EXEC
      *    MOVE '"$I.                  ! + #00002608' TO DFHEIV0
           MOVE X'2224492E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303032363038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * EXEC CICS READ
      *        DATASET  (CNTL-FILE-ID)
      *        SET      (ADDRESS OF CONTROL-FILE)
      *        RIDFLD   (ELCNTL-KEY)
      *    END-EXEC
      *    MOVE '&"S        E          (   #00002612' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF CNTL-REC-TYPE = '6'
              MOVE AL-UANON           TO CARRIERA
              MOVE CARRIERI           TO PI-ERC-CARRIER
              GO TO 7499-EXIT
           END-IF
           .
       7490-NOT-FOUND.
           IF CNTL-REC-TYPE = '6'
              MOVE -1                  TO CARRIERL
              MOVE AL-UABON            TO CARRIERA
              MOVE ER-0193             TO EMI-ERROR
           END-IF
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           .
       7499-EXIT.
           EXIT.
       7500-COUNT-OPEN-RANGES.
           MOVE +1                     TO NDX
           MOVE +0                     TO WS-OPEN-COUNT.
       7500-LOOP.
           IF NDX GREATER THAN BK-BANK-POINTER-CNT
                MOVE +1                TO NDX
                GO TO 7500-EXIT.
           IF BK-AM-EXP-DT (NDX) = HIGH-VALUES
               ADD +1 TO WS-OPEN-COUNT.
           ADD +1 TO NDX
           GO TO 7500-LOOP.
       7500-EXIT.
           EXIT.
           EJECT
00837  7510-SORT-ACCOUNTS.
00857      IF BK-BANK-POINTER-CNT NOT > +1
00858         GO TO 7599-EXIT
           END-IF
00860      MOVE +1                     TO WSUB1
00861                                     WSUB2
00862      MOVE HIGH-VALUES            TO WS-WORK-ACCT-TABLE
           .
00864  7520-CONTINUE-SORT.
00866      IF WSUB2 > BK-BANK-POINTER-CNT
00867         GO TO 7540-RID-OF-HIGH-VALUES
           END-IF
           IF (BK-AM-ACCOUNT (WSUB1) < WS-AM-ACCOUNT (WSUB2))
              AND (BK-AM-ACCOUNT (WSUB1) NOT = SPACES)
              MOVE BK-BANK-POINTER (WSUB1)
                                       TO WS-BANKS (WSUB2)
              MOVE WSUB1               TO WSUB3
           END-IF
00876      ADD +1 TO WSUB1
00878      IF WSUB1 NOT > BK-BANK-POINTER-CNT
00879         GO TO 7520-CONTINUE-SORT
           END-IF
00890      MOVE HIGH-VALUES            TO BK-BANK-POINTER (WSUB3)
00891      ADD +1                      TO WSUB2
00892      MOVE +1                     TO WSUB1
00893      GO TO 7520-CONTINUE-SORT
00894      .
00897  7540-RID-OF-HIGH-VALUES.
00899      IF WS-BANKS (WSUB1) = HIGH-VALUES
00900         MOVE SPACES              TO WS-BANKS (WSUB1)
           END-IF
00902      ADD +1 TO WSUB1
00904      IF WSUB1 NOT > BK-BANK-POINTER-CNT
00905         GO TO 7540-RID-OF-HIGH-VALUES
           END-IF
00907      MOVE +1                     TO WSUB1
00908      MOVE +2                     TO WSUB2
00909      .
00910  7550-CHECK-DUPS.
00912      IF (WS-AM-ACCOUNT (WSUB1) = WS-AM-ACCOUNT (WSUB2))
00913         AND (WS-AM-STATE (WSUB1) = WS-AM-STATE (WSUB2))
00914         AND (WS-AM-EXP-DT (WSUB1) NOT = SPACES)
00915         MOVE ER-2947             TO  EMI-ERROR
00916         PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
00917         MOVE -1                  TO  MAINTYPL
00918         GO TO 8200-SEND-DATAONLY
           END-IF
00920      ADD +1 TO WSUB2
00922      IF WSUB2 NOT > BK-BANK-POINTER-CNT
00923         GO TO 7550-CHECK-DUPS
           END-IF
00925      ADD +1                      TO WSUB1
00926      MOVE WSUB1                  TO WSUB2
00927      ADD +1                      TO WSUB2
00929      IF WSUB1 NOT > BK-BANK-POINTER-CNT
00930         GO TO 7550-CHECK-DUPS
           END-IF
           PERFORM VARYING WSUB1 FROM +1 BY +1 UNTIL
              (WSUB1 > BK-BANK-POINTER-CNT)
              MOVE WS-BANKS (WSUB1)    TO BK-BANK-POINTER (WSUB1)
           END-PERFORM
           .
00934  7599-EXIT.
00935      EXIT.
       8100-SEND-INITIAL-MAP.
           MOVE SAVE-DATE              TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
           MOVE -1                     TO MAINTYPL
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
           
      * EXEC CICS SEND
      *        MAP    (MAP-NAME)
      *        MAPSET (MAPSET-NAME)
      *        FROM   (EL652DO)
      *        ERASE
      *        CURSOR
      *        END-EXEC.
           MOVE LENGTH OF
            EL652DO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002718' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL652DO, 
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
           MOVE SAVE-DATE              TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
           
      * EXEC CICS SEND
      *        MAP    (MAP-NAME)
      *        MAPSET (MAPSET-NAME)
      *        FROM   (EL652DO)
      *        DATAONLY
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL652DO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002731' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL652DO, 
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
       8500-DATE-CONVERT.
           MOVE LINK-ELDATCV           TO PGM-NAME.
           
      * EXEC CICS LINK
      *        PROGRAM    (PGM-NAME)
      *        COMMAREA   (DATE-CONVERSION-DATA)
      *        LENGTH     (DC-COMM-LENGTH)
      *        END-EXEC.
      *    MOVE '."C                   ''   #00002742' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       8500-EXIT.
           EXIT.
       8600-DEEDIT.
           
      * EXEC CICS  BIF DEEDIT
      *        FIELD   (WS-DEEDIT-FIELD)
      *        LENGTH  (10)
      *    END-EXEC.
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002750' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       8600-EXIT.
            EXIT.
       8810-PF23.
           MOVE EIBAID                 TO PI-ENTRY-CD-1.
           MOVE XCTL-005               TO PGM-NAME.
           GO TO 9300-XCTL.
       9100-RETURN-TRAN.
           MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
           MOVE '652D'               TO PI-CURRENT-SCREEN-NO.
           
      * EXEC CICS RETURN
      *        TRANSID(TRANS-ID)
      *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
      *        LENGTH(PI-COMM-LENGTH)
      *        END-EXEC.
      *    MOVE '.(CT                  &   #00002763' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6523' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9910-INITIALIZE-SECURITY.
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
           IF PI-PROCESSOR-ID NOT = 'LGXX'
              
      * EXEC CICS    READQ TS
      *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
      *            INTO    (SECURITY-CONTROL)
      *            LENGTH  (SC-COMM-LENGTH)
      *            ITEM    (SC-ITEM-CL-CR)
      *       END-EXEC
      *    MOVE '*$II   L              ''   #00002781' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM-CL-CR, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              MOVE SC-CREDIT-DISPLAY (05)
                                       TO PI-DISPLAY-CAP
              MOVE SC-CREDIT-UPDATE  (05)
                                       TO PI-MODIFY-CAP
              IF  NOT DISPLAY-CAP
                  MOVE 'READ'          TO SM-READ
                  PERFORM 9995-SECURITY-VIOLATION
                                       THRU 9995-EXIT
                  MOVE ER-0070         TO EMI-ERROR
                  PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                  GO TO 8100-SEND-INITIAL-MAP
              END-IF
           END-IF
           .
       9910-EXIT.
           EXIT.
       9200-RETURN-MAIN-MENU.
           MOVE XCTL-626               TO PGM-NAME.
           GO TO 9300-XCTL.
           EJECT
       9300-XCTL.
           
      * EXEC CICS XCTL
      *        PROGRAM    (PGM-NAME)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (PI-COMM-LENGTH)
      *        END-EXEC.
      *    MOVE '.$C                   $   #00002809' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9400-CLEAR.
           MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
           GO TO 9300-XCTL.
       9500-PF12.
           MOVE XCTL-010               TO PGM-NAME.
           GO TO 9300-XCTL.
       9700-DATE-CONVERSION.
           
      * EXEC CICS LINK
      *        PROGRAM   ('ELDATCV')
      *        COMMAREA  (DATE-CONVERSION-DATA)
      *        LENGTH    (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00002821' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383231' TO DFHEIV0(25:11)
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
       9900-ERROR-FORMAT.
           IF NOT EMI-ERRORS-COMPLETE
               MOVE LINK-001 TO PGM-NAME
               
      * EXEC CICS LINK
      *            PROGRAM(PGM-NAME)
      *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
      *            LENGTH(EMI-COMM-LENGTH)
      *            END-EXEC.
      *    MOVE '."C                   ''   #00002831' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383331' TO DFHEIV0(25:11)
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
           MOVE LINK-004 TO PGM-NAME.
           MOVE DFHEIBLK TO EMI-LINE1.
           
      * EXEC CICS LINK
      *        PROGRAM   (PGM-NAME)
      *        COMMAREA  (EMI-LINE1)
      *        LENGTH    (72)
      *        END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002841' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383431' TO DFHEIV0(25:11)
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
      *                                COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00002865' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383635' TO DFHEIV0(25:11)
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
           MOVE 'EL6523' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ABEND,
                     8100-SEND-INITIAL-MAP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1500-BXRF-NOT-FOUND,
                     0690-QIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1500-BXRF-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 4010-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 9990-ABEND,
                     4250-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 5800-NO-RECORD,
                     5900-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 5900-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1500-BXRF-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 7200-BXRF-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7490-NOT-FOUND,
                     9990-ABEND
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6523' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
