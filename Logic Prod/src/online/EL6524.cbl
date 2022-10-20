       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 EL6524.
      *
      *AUTHOR.     CENTRAL STATES HEALTH AND LIFE.
      *            OMAHA, NEBRASKA.
      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO.        IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.    TRANSACTION - EXDE - REPRESENTATIVE
      *                                 MAINTENANCE.
       ENVIRONMENT DIVISION.
           EJECT
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*    EL6524 WORKING STORAGE    *'.
       77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'.
       77  NDX     PIC S9(5)  COMP-3 VALUE +1.
       77  WSUB1   PIC S9(5)  COMP-3 VALUE +0.
       77  WSUB2   PIC S9(5)  COMP-3 VALUE +0.
       77  WSUB3   PIC S9(5)  COMP-3 VALUE +0.
       77  WS-TOT-FEES                 PIC S9(3)V99 COMP-3 VALUE +0.
       77  WS-TOT-LFEES                PIC S9(3)V99 COMP-3 VALUE +0.
       77  ERAGTC-LENGTH               PIC S9(4) COMP VALUE +450.
       77  TABLE-SW                    PIC X.
           88  TABLE-OVERLAP                   VALUE 'Y'.
       77  WS-CHECK-DATE-SW            PIC X   VALUE SPACES.
           88  DATE-CHECK-OVER                 VALUE 'Y'.
       77  WS-EFF-SW                   PIC X   VALUE SPACES.
           88  EFF-DT-CHANGE                   VALUE 'Y'.
       77  WS-EXP-SW                   PIC X   VALUE SPACES.
           88  EXP-DT-CHANGE                   VALUE 'Y'.
       77  WS-SKIP-KEY                 PIC X(21) VALUE LOW-VALUES.
       01  WS-DATE-AREA.
           05  SAVE-DATE           PIC X(8)    VALUE SPACES.
           05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
       01  WS-SAVE-ERAGTC              PIC X(450).
       01  WS-ERCOMP-KEY.
           05  WS-COMP-COMPANY-CD      PIC X.
           05  WS-COMP-CARRIER         PIC X.
           05  WS-COMP-GROUPING        PIC X(6).
           05  WS-COMP-FINRESP         PIC X(10).
           05  WS-COMP-ACCOUNT         PIC X(10).
           05  WS-COMP-TYPE            PIC X.
       01  WS-DATE-TABLE.
           05  FILLER OCCURS 40.
               10  WS-TBL-EFF-DT       PIC XX.
               10  WS-TBL-EXP-DT       PIC XX.
       01  WS-CONTROL-PRIMARY.
           05  WS-COMPANY-CD       PIC X.
           05  WS-CARRIER          PIC X.
           05  WS-GROUP            PIC X(6).
           05  WS-BANK             PIC X(10).
           05  WS-EXP-DT           PIC XX.
           05  WS-TYPE             PIC X.
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
           12  ER-2042                 PIC X(4)  VALUE '2042'.
           12  ER-2051                 PIC X(4)  VALUE '2051'.
           12  ER-2053                 PIC X(4)  VALUE '2053'.
           12  ER-2056                 PIC X(4)  VALUE '2056'.
           12  ER-2057                 PIC X(4)  VALUE '2057'.
           12  ER-2173                 PIC X(4)  VALUE '2173'.
           12  ER-2230                 PIC X(4)  VALUE '2230'.
           12  ER-2237                 PIC X(4)  VALUE '2237'.
           12  ER-2238                 PIC X(4)  VALUE '2238'.
           12  ER-2339                 PIC X(4)  VALUE '2339'.
           12  ER-2370                 PIC X(4)  VALUE '2370'.
           12  ER-2947                 PIC X(4)  VALUE '2947'.
           12  ER-2717                 PIC X(4)  VALUE '2717'.
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
           12  DEEDIT-FIELD        PIC  X(08).
           12  DEEDIT-FIELD-V0  REDEFINES
               DEEDIT-FIELD        PIC 9(08).
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
               16  FILLER          PIC X(4)    VALUE '652E'.
           12  RETURNED-FROM       PIC X(8)    VALUE SPACES.
           12  QID-MAP-LENGTH      PIC S9(4)   VALUE +1376   COMP.
           12  MAP-NAME                PIC X(8)    VALUE 'EL652E'.
           12  MAPSET-NAME             PIC X(8)    VALUE 'EL6524S'.
           12  TRANS-ID                PIC X(4)    VALUE 'EXDE'.
           12  EL652-TRANS-ID          PIC X(4)    VALUE 'EXD4'.
           12  EL650-TRANS-ID          PIC X(4)    VALUE 'EXC4'.
           12  PGM-NAME                PIC X(8)    VALUE SPACES.
           12  THIS-PGM                PIC X(8)    VALUE 'EL6524'.
           12  XCTL-650                PIC X(8)    VALUE 'EL650'.
           12  AGTC-FILE-ID            PIC X(8)    VALUE 'ERAGTC'.
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
           12  AGTC-REC-LENGTH         PIC S9(4)    VALUE +0.
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
               16  PI-ERAGTC-KEY.
                   20  PI-AGTC-COMP-CD     PIC X.
                   20  PI-AGTC-CARRIER     PIC X.
                   20  PI-AGTC-GROUPING    PIC X(6).
                   20  PI-AGTC-BANK        PIC X(10).
                   20  PI-AGTC-EXP-DT      PIC XX.
                   20  PI-AGTC-TYPE        PIC X.
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
                   20  PI-ERC-EXP-DT       PIC  XX.
                   20  PI-ERC-TYPE         PIC  X.
               16  PI-SAVE-ERAGTC-KEY      PIC  X(21).
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
       01  WS-BIN-EFF-DT               PIC XX VALUE LOW-VALUES.
       01  WS-BIN-EXP-DT               PIC XX VALUE LOW-VALUES.
      *                            COPY EL6524S.
       01  EL652EI.
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
           05  MAINTDTL PIC S9(0004) COMP.
           05  MAINTDTF PIC  X(0001).
           05  FILLER REDEFINES MAINTDTF.
               10  MAINTDTA PIC  X(0001).
           05  MAINTDTI PIC  X(0008).
      *    -------------------------------
           05  MAINTBYL PIC S9(0004) COMP.
           05  MAINTBYF PIC  X(0001).
           05  FILLER REDEFINES MAINTBYF.
               10  MAINTBYA PIC  X(0001).
           05  MAINTBYI PIC  X(0004).
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
           05  BANKL PIC S9(0004) COMP.
           05  BANKF PIC  X(0001).
           05  FILLER REDEFINES BANKF.
               10  BANKA PIC  X(0001).
           05  BANKI PIC  X(0010).
      *    -------------------------------
           05  EFFDTL PIC S9(0004) COMP.
           05  EFFDTF PIC  X(0001).
           05  FILLER REDEFINES EFFDTF.
               10  EFFDTA PIC  X(0001).
           05  EFFDTI PIC  X(0008).
      *    -------------------------------
           05  EXPDTL PIC S9(0004) COMP.
           05  EXPDTF PIC  X(0001).
           05  FILLER REDEFINES EXPDTF.
               10  EXPDTA PIC  X(0001).
           05  EXPDTI PIC  X(0008).
      *    -------------------------------
           05  REP1L PIC S9(0004) COMP.
           05  REP1F PIC  X(0001).
           05  FILLER REDEFINES REP1F.
               10  REP1A PIC  X(0001).
           05  REP1I PIC  X(0010).
      *    -------------------------------
           05  TYPE1L PIC S9(0004) COMP.
           05  TYPE1F PIC  X(0001).
           05  FILLER REDEFINES TYPE1F.
               10  TYPE1A PIC  X(0001).
           05  TYPE1I PIC  X(0001).
      *    -------------------------------
           05  FEES1L PIC S9(0004) COMP.
           05  FEES1F PIC  X(0001).
           05  FILLER REDEFINES FEES1F.
               10  FEES1A PIC  X(0001).
           05  FEES1I PIC  S9(6)V99.
      *    -------------------------------
           05  RECAL1L PIC S9(0004) COMP.
           05  RECAL1F PIC  X(0001).
           05  FILLER REDEFINES RECAL1F.
               10  RECAL1A PIC  X(0001).
           05  RECAL1I PIC  X(0001).
      *    -------------------------------
           05  LFEES1L PIC S9(0004) COMP.
           05  LFEES1F PIC  X(0001).
           05  FILLER REDEFINES LFEES1F.
               10  LFEES1A PIC  X(0001).
           05  LFEES1I PIC  S9(6)V99.
      *    -------------------------------
           05  LRCAL1L PIC S9(0004) COMP.
           05  LRCAL1F PIC  X(0001).
           05  FILLER REDEFINES LRCAL1F.
               10  LRCAL1A PIC  X(0001).
           05  LRCAL1I PIC  X(0001).
      *    -------------------------------
           05  REP2L PIC S9(0004) COMP.
           05  REP2F PIC  X(0001).
           05  FILLER REDEFINES REP2F.
               10  REP2A PIC  X(0001).
           05  REP2I PIC  X(0010).
      *    -------------------------------
           05  TYPE2L PIC S9(0004) COMP.
           05  TYPE2F PIC  X(0001).
           05  FILLER REDEFINES TYPE2F.
               10  TYPE2A PIC  X(0001).
           05  TYPE2I PIC  X(0001).
      *    -------------------------------
           05  FEES2L PIC S9(0004) COMP.
           05  FEES2F PIC  X(0001).
           05  FILLER REDEFINES FEES2F.
               10  FEES2A PIC  X(0001).
           05  FEES2I PIC  S9(6)V99.
      *    -------------------------------
           05  RECAL2L PIC S9(0004) COMP.
           05  RECAL2F PIC  X(0001).
           05  FILLER REDEFINES RECAL2F.
               10  RECAL2A PIC  X(0001).
           05  RECAL2I PIC  X(0001).
      *    -------------------------------
           05  LFEES2L PIC S9(0004) COMP.
           05  LFEES2F PIC  X(0001).
           05  FILLER REDEFINES LFEES2F.
               10  LFEES2A PIC  X(0001).
           05  LFEES2I PIC  S9(6)V99.
      *    -------------------------------
           05  LRCAL2L PIC S9(0004) COMP.
           05  LRCAL2F PIC  X(0001).
           05  FILLER REDEFINES LRCAL2F.
               10  LRCAL2A PIC  X(0001).
           05  LRCAL2I PIC  X(0001).
      *    -------------------------------
           05  REP3L PIC S9(0004) COMP.
           05  REP3F PIC  X(0001).
           05  FILLER REDEFINES REP3F.
               10  REP3A PIC  X(0001).
           05  REP3I PIC  X(0010).
      *    -------------------------------
           05  TYPE3L PIC S9(0004) COMP.
           05  TYPE3F PIC  X(0001).
           05  FILLER REDEFINES TYPE3F.
               10  TYPE3A PIC  X(0001).
           05  TYPE3I PIC  X(0001).
      *    -------------------------------
           05  FEES3L PIC S9(0004) COMP.
           05  FEES3F PIC  X(0001).
           05  FILLER REDEFINES FEES3F.
               10  FEES3A PIC  X(0001).
           05  FEES3I PIC  S9(6)V99.
      *    -------------------------------
           05  RECAL3L PIC S9(0004) COMP.
           05  RECAL3F PIC  X(0001).
           05  FILLER REDEFINES RECAL3F.
               10  RECAL3A PIC  X(0001).
           05  RECAL3I PIC  X(0001).
      *    -------------------------------
           05  LFEES3L PIC S9(0004) COMP.
           05  LFEES3F PIC  X(0001).
           05  FILLER REDEFINES LFEES3F.
               10  LFEES3A PIC  X(0001).
           05  LFEES3I PIC  S9(6)V99.
      *    -------------------------------
           05  LRCAL3L PIC S9(0004) COMP.
           05  LRCAL3F PIC  X(0001).
           05  FILLER REDEFINES LRCAL3F.
               10  LRCAL3A PIC  X(0001).
           05  LRCAL3I PIC  X(0001).
      *    -------------------------------
           05  REP4L PIC S9(0004) COMP.
           05  REP4F PIC  X(0001).
           05  FILLER REDEFINES REP4F.
               10  REP4A PIC  X(0001).
           05  REP4I PIC  X(0010).
      *    -------------------------------
           05  TYPE4L PIC S9(0004) COMP.
           05  TYPE4F PIC  X(0001).
           05  FILLER REDEFINES TYPE4F.
               10  TYPE4A PIC  X(0001).
           05  TYPE4I PIC  X(0001).
      *    -------------------------------
           05  FEES4L PIC S9(0004) COMP.
           05  FEES4F PIC  X(0001).
           05  FILLER REDEFINES FEES4F.
               10  FEES4A PIC  X(0001).
           05  FEES4I PIC  S9(6)V99.
      *    -------------------------------
           05  RECAL4L PIC S9(0004) COMP.
           05  RECAL4F PIC  X(0001).
           05  FILLER REDEFINES RECAL4F.
               10  RECAL4A PIC  X(0001).
           05  RECAL4I PIC  X(0001).
      *    -------------------------------
           05  LFEES4L PIC S9(0004) COMP.
           05  LFEES4F PIC  X(0001).
           05  FILLER REDEFINES LFEES4F.
               10  LFEES4A PIC  X(0001).
           05  LFEES4I PIC  S9(6)V99.
      *    -------------------------------
           05  LRCAL4L PIC S9(0004) COMP.
           05  LRCAL4F PIC  X(0001).
           05  FILLER REDEFINES LRCAL4F.
               10  LRCAL4A PIC  X(0001).
           05  LRCAL4I PIC  X(0001).
      *    -------------------------------
           05  REP5L PIC S9(0004) COMP.
           05  REP5F PIC  X(0001).
           05  FILLER REDEFINES REP5F.
               10  REP5A PIC  X(0001).
           05  REP5I PIC  X(0010).
      *    -------------------------------
           05  TYPE5L PIC S9(0004) COMP.
           05  TYPE5F PIC  X(0001).
           05  FILLER REDEFINES TYPE5F.
               10  TYPE5A PIC  X(0001).
           05  TYPE5I PIC  X(0001).
      *    -------------------------------
           05  FEES5L PIC S9(0004) COMP.
           05  FEES5F PIC  X(0001).
           05  FILLER REDEFINES FEES5F.
               10  FEES5A PIC  X(0001).
           05  FEES5I PIC  S9(6)V99.
      *    -------------------------------
           05  RECAL5L PIC S9(0004) COMP.
           05  RECAL5F PIC  X(0001).
           05  FILLER REDEFINES RECAL5F.
               10  RECAL5A PIC  X(0001).
           05  RECAL5I PIC  X(0001).
      *    -------------------------------
           05  LFEES5L PIC S9(0004) COMP.
           05  LFEES5F PIC  X(0001).
           05  FILLER REDEFINES LFEES5F.
               10  LFEES5A PIC  X(0001).
           05  LFEES5I PIC  S9(6)V99.
      *    -------------------------------
           05  LRCAL5L PIC S9(0004) COMP.
           05  LRCAL5F PIC  X(0001).
           05  FILLER REDEFINES LRCAL5F.
               10  LRCAL5A PIC  X(0001).
           05  LRCAL5I PIC  X(0001).
      *    -------------------------------
           05  REP6L PIC S9(0004) COMP.
           05  REP6F PIC  X(0001).
           05  FILLER REDEFINES REP6F.
               10  REP6A PIC  X(0001).
           05  REP6I PIC  X(0010).
      *    -------------------------------
           05  TYPE6L PIC S9(0004) COMP.
           05  TYPE6F PIC  X(0001).
           05  FILLER REDEFINES TYPE6F.
               10  TYPE6A PIC  X(0001).
           05  TYPE6I PIC  X(0001).
      *    -------------------------------
           05  FEES6L PIC S9(0004) COMP.
           05  FEES6F PIC  X(0001).
           05  FILLER REDEFINES FEES6F.
               10  FEES6A PIC  X(0001).
           05  FEES6I PIC  S9(6)V99.
      *    -------------------------------
           05  RECAL6L PIC S9(0004) COMP.
           05  RECAL6F PIC  X(0001).
           05  FILLER REDEFINES RECAL6F.
               10  RECAL6A PIC  X(0001).
           05  RECAL6I PIC  X(0001).
      *    -------------------------------
           05  LFEES6L PIC S9(0004) COMP.
           05  LFEES6F PIC  X(0001).
           05  FILLER REDEFINES LFEES6F.
               10  LFEES6A PIC  X(0001).
           05  LFEES6I PIC  S9(6)V99.
      *    -------------------------------
           05  LRCAL6L PIC S9(0004) COMP.
           05  LRCAL6F PIC  X(0001).
           05  FILLER REDEFINES LRCAL6F.
               10  LRCAL6A PIC  X(0001).
           05  LRCAL6I PIC  X(0001).
      *    -------------------------------
           05  REP7L PIC S9(0004) COMP.
           05  REP7F PIC  X(0001).
           05  FILLER REDEFINES REP7F.
               10  REP7A PIC  X(0001).
           05  REP7I PIC  X(0010).
      *    -------------------------------
           05  TYPE7L PIC S9(0004) COMP.
           05  TYPE7F PIC  X(0001).
           05  FILLER REDEFINES TYPE7F.
               10  TYPE7A PIC  X(0001).
           05  TYPE7I PIC  X(0001).
      *    -------------------------------
           05  FEES7L PIC S9(0004) COMP.
           05  FEES7F PIC  X(0001).
           05  FILLER REDEFINES FEES7F.
               10  FEES7A PIC  X(0001).
           05  FEES7I PIC  S9(6)V99.
      *    -------------------------------
           05  RECAL7L PIC S9(0004) COMP.
           05  RECAL7F PIC  X(0001).
           05  FILLER REDEFINES RECAL7F.
               10  RECAL7A PIC  X(0001).
           05  RECAL7I PIC  X(0001).
      *    -------------------------------
           05  LFEES7L PIC S9(0004) COMP.
           05  LFEES7F PIC  X(0001).
           05  FILLER REDEFINES LFEES7F.
               10  LFEES7A PIC  X(0001).
           05  LFEES7I PIC  S9(6)V99.
      *    -------------------------------
           05  LRCAL7L PIC S9(0004) COMP.
           05  LRCAL7F PIC  X(0001).
           05  FILLER REDEFINES LRCAL7F.
               10  LRCAL7A PIC  X(0001).
           05  LRCAL7I PIC  X(0001).
      *    -------------------------------
           05  REP8L PIC S9(0004) COMP.
           05  REP8F PIC  X(0001).
           05  FILLER REDEFINES REP8F.
               10  REP8A PIC  X(0001).
           05  REP8I PIC  X(0010).
      *    -------------------------------
           05  TYPE8L PIC S9(0004) COMP.
           05  TYPE8F PIC  X(0001).
           05  FILLER REDEFINES TYPE8F.
               10  TYPE8A PIC  X(0001).
           05  TYPE8I PIC  X(0001).
      *    -------------------------------
           05  FEES8L PIC S9(0004) COMP.
           05  FEES8F PIC  X(0001).
           05  FILLER REDEFINES FEES8F.
               10  FEES8A PIC  X(0001).
           05  FEES8I PIC  S9(6)V99.
      *    -------------------------------
           05  RECAL8L PIC S9(0004) COMP.
           05  RECAL8F PIC  X(0001).
           05  FILLER REDEFINES RECAL8F.
               10  RECAL8A PIC  X(0001).
           05  RECAL8I PIC  X(0001).
      *    -------------------------------
           05  LFEES8L PIC S9(0004) COMP.
           05  LFEES8F PIC  X(0001).
           05  FILLER REDEFINES LFEES8F.
               10  LFEES8A PIC  X(0001).
           05  LFEES8I PIC  S9(6)V99.
      *    -------------------------------
           05  LRCAL8L PIC S9(0004) COMP.
           05  LRCAL8F PIC  X(0001).
           05  FILLER REDEFINES LRCAL8F.
               10  LRCAL8A PIC  X(0001).
           05  LRCAL8I PIC  X(0001).
      *    -------------------------------
           05  REP9L PIC S9(0004) COMP.
           05  REP9F PIC  X(0001).
           05  FILLER REDEFINES REP9F.
               10  REP9A PIC  X(0001).
           05  REP9I PIC  X(0010).
      *    -------------------------------
           05  TYPE9L PIC S9(0004) COMP.
           05  TYPE9F PIC  X(0001).
           05  FILLER REDEFINES TYPE9F.
               10  TYPE9A PIC  X(0001).
           05  TYPE9I PIC  X(0001).
      *    -------------------------------
           05  FEES9L PIC S9(0004) COMP.
           05  FEES9F PIC  X(0001).
           05  FILLER REDEFINES FEES9F.
               10  FEES9A PIC  X(0001).
           05  FEES9I PIC  S9(6)V99.
      *    -------------------------------
           05  RECAL9L PIC S9(0004) COMP.
           05  RECAL9F PIC  X(0001).
           05  FILLER REDEFINES RECAL9F.
               10  RECAL9A PIC  X(0001).
           05  RECAL9I PIC  X(0001).
      *    -------------------------------
           05  LFEES9L PIC S9(0004) COMP.
           05  LFEES9F PIC  X(0001).
           05  FILLER REDEFINES LFEES9F.
               10  LFEES9A PIC  X(0001).
           05  LFEES9I PIC  S9(6)V99.
      *    -------------------------------
           05  LRCAL9L PIC S9(0004) COMP.
           05  LRCAL9F PIC  X(0001).
           05  FILLER REDEFINES LRCAL9F.
               10  LRCAL9A PIC  X(0001).
           05  LRCAL9I PIC  X(0001).
      *    -------------------------------
           05  REP10L PIC S9(0004) COMP.
           05  REP10F PIC  X(0001).
           05  FILLER REDEFINES REP10F.
               10  REP10A PIC  X(0001).
           05  REP10I PIC  X(0010).
      *    -------------------------------
           05  TYPE10L PIC S9(0004) COMP.
           05  TYPE10F PIC  X(0001).
           05  FILLER REDEFINES TYPE10F.
               10  TYPE10A PIC  X(0001).
           05  TYPE10I PIC  X(0001).
      *    -------------------------------
           05  FEES10L PIC S9(0004) COMP.
           05  FEES10F PIC  X(0001).
           05  FILLER REDEFINES FEES10F.
               10  FEES10A PIC  X(0001).
           05  FEES10I PIC  S9(6)V99.
      *    -------------------------------
           05  RECAL10L PIC S9(0004) COMP.
           05  RECAL10F PIC  X(0001).
           05  FILLER REDEFINES RECAL10F.
               10  RECAL10A PIC  X(0001).
           05  RECAL10I PIC  X(0001).
      *    -------------------------------
           05  LFEES10L PIC S9(0004) COMP.
           05  LFEES10F PIC  X(0001).
           05  FILLER REDEFINES LFEES10F.
               10  LFEES10A PIC  X(0001).
           05  LFEES10I PIC  S9(6)V99.
      *    -------------------------------
           05  LRCAL10L PIC S9(0004) COMP.
           05  LRCAL10F PIC  X(0001).
           05  FILLER REDEFINES LRCAL10F.
               10  LRCAL10A PIC  X(0001).
           05  LRCAL10I PIC  X(0001).
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
       01  EL652EO REDEFINES EL652EI.
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
           05  MAINTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTBYO PIC  X(0004).
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
           05  BANKO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REP1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEES1O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFEES1O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRCAL1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REP2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEES2O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFEES2O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRCAL2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REP3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEES3O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFEES3O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRCAL3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REP4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEES4O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFEES4O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRCAL4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REP5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEES5O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFEES5O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRCAL5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REP6O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEES6O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFEES6O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRCAL6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REP7O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEES7O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFEES7O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRCAL7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REP8O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEES8O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFEES8O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRCAL8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REP9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEES9O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFEES9O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRCAL9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REP10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEES10O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECAL10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFEES10O PIC  ZZZ99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRCAL10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
00405  01  MAP-R REDEFINES EL652EI.
100703     12  FILLER                  PIC X(105).
00407      12  MAP-AGENT-AREA.
00408          16  AGENT-AREA OCCURS 10 TIMES INDEXED BY M-INDEX.
                   20  REPRL           PIC S9(4) COMP.
                   20  REPRA           PIC X.
                   20  REPR            PIC X(10).
00412              20  ATYPEL          PIC S9(4) COMP.
00413              20  ATYPEA          PIC X.
00414              20  ATYPE           PIC X.
00415              20  FEESL           PIC S9(4) COMP.
00416              20  FEESA           PIC X.
00417              20  FEES-COMM-T     PIC X(8).
00418              20  FEES-COMM-N     REDEFINES FEES-COMM-T.
00419                  24  FEES-COMM-R PIC 9(6)V99.
00421              20  FEES-COMM-O     REDEFINES FEES-COMM-T
00422                                  PIC ZZZZ9.99.
00439              20  RECALL          PIC S9(4) COMP.
00440              20  RECALA          PIC X.
00441              20  RECAL           PIC X.
092205             20  LFEESL          PIC S9(4) COMP.
092205             20  LFEESA          PIC X.
092205             20  LFEES-COMM-T    PIC X(8).
092205             20  LFEES-COMM-N    REDEFINES LFEES-COMM-T.
092205                 24  LFEES-COMM-R PIC 9(6)V99.
092205             20  LFEES-COMM-O    REDEFINES LFEES-COMM-T
092205                                 PIC ZZZZ9.99.
092205             20  LRCALL          PIC S9(4) COMP.
092205             20  LRCALA          PIC X.
092205             20  LRCAL           PIC X.
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
031808
031808         16  FILLER                         PIC X(82).
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
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
00498          16  FILLER                             PIC  X(240).
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
00607              20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
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
011410         16  FILLER                         PIC X(187).
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
082603             20  FILLER                     PIC X(11).
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
092705         16  FILLER                         PIC X(448).
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
      *                                COPY ERCAGTC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCAGTC                             *
      *                            VMOD=2.001                          *
      *                                                                *
      *   ONLINE CREDIT SYSTEM                                         *
      *                                                                *
      *   FILE DESCRIPTION = AGENT COMMISSIONS                         *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 450   RECFORM = FIXED                          *
      *                                                                *
      *   BASE CLUSTER NAME = ERAGTC                   RKP=2,LEN=21    *
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
      * 111004    2004110300005  PEMA  ADD DISTRIBUTION OF ADDENDUM FEE
      * 111004                         NEW FILE AND COPYBOOK
092205* 092205                   PEMA  ADD PROCESSING FOR SPP LEASE
      ******************************************************************
       01  AGENT-COMMISSIONS.
           12  AG-RECORD-ID                          PIC XX.
               88  VALID-CO-ID                          VALUE 'AG'.
           12  AG-CONTROL-PRIMARY.
               16  AG-COMPANY-CD                     PIC X.
               16  AG-CONTROL.
                   20  AG-CTL-1.
                       24  AG-CARR-GROUP.
                           28  AG-CARRIER            PIC X.
                           28  AG-GROUPING           PIC X(6).
                       24  AG-BANK                   PIC X(10).
                   20  AG-CTL-2.
                       24  AG-EXP-DT                 PIC XX.
               16  AG-TYPE                           PIC X.
                   88  AG-GEN-AGENT-TYPE                VALUE 'G'.
                   88  AG-ACCOUNT-TYPE                  VALUE 'A'.
           12  AG-MAINT-INFORMATION.
               16  AG-LAST-MAINT-DT                  PIC XX.
               16  AG-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
               16  AG-LAST-MAINT-USER                PIC X(4).
               16  FILLER                            PIC X(10).
           12  AG-EFF-DT                             PIC XX.
           12  AG-COMM-STRUCTURE.
               16  AG-AGT-COMMS OCCURS 10.
                   20  AG-AGT                PIC X(10).
                   20  AG-COM-TYP            PIC X.
                   20  AG-SPP-FEES           PIC S9(5)V99   COMP-3.
                   20  AG-RECALC-LV-INDIC    PIC X.
092205             20  AG-SPP-LFEES          PIC S9(5)V99   COMP-3.
092205             20  AG-LRCALC-LV-INDIC    PIC X.
092205             20  FILLER                PIC X(05).
           12  FILLER                  PIC X(145).
      ******************************************************************
      *                                COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE
                                AGENT-COMMISSIONS COMPENSATION-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6524' TO DFHEIV1.
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
      *    MOVE '"$.?                  ! " #00003554' TO DFHEIV0
           MOVE X'22242E3F2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033353534' TO DFHEIV0(25:11)
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
      * THIS IS WHEN I COME FROM COMP MAINT
               MOVE DFHENTER       TO EIBAID
               MOVE PI-CR-CARRIER  TO CARRIERI
               MOVE PI-CR-GROUPING TO GROUPI
               MOVE PI-CR-FIN-RESP TO BANKI
               MOVE PI-CR-TYPE     TO TYPEI
               MOVE 'S'            TO MAINTYPI
               MOVE 1              TO CARRIERL
               MOVE 6              TO GROUPL
               MOVE 10             TO BANKL
               MOVE 1              TO TYPEL
               MOVE 1              TO MAINTYPL
               MOVE AL-UANON       TO CARRIERA GROUPA TYPEA
                                      BANKA MAINTYPA
               GO TO 4000-EDIT-MAINT
           END-IF
           MOVE LOW-VALUES       TO PI-ERAGTC-KEY    EL652EI.
           MOVE PI-COMPANY-CD    TO PI-AGTC-COMP-CD  WS-COMPANY-CD.
           IF EIBTRNID  = EL650-TRANS-ID
      * THIS IS WHEN I CAME FROM ACCOUNT MAINT
               GO TO 0600-RECOVER-TEMP-STORAGE.
           GO TO 8100-SEND-INITIAL-MAP.
           EJECT
       0200-RECEIVE-MAP.
           MOVE LOW-VALUES             TO EL652EI
           
      * EXEC CICS RECEIVE
      *        MAP (MAP-NAME)
      *        MAPSET (MAPSET-NAME)
      *        INTO (EL652EI)
      *    END-EXEC
           MOVE LENGTH OF
            EL652EI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003614' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL652EI, 
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
      *       GO TO 4000-EDIT-MAINT
           END-IF
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
      *        FROM    (EL652EI)
      *        LENGTH  (QID-MAP-LENGTH)
      *        END-EXEC.
      *    MOVE '*"                    ''   #00003656' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL652EI, 
                 QID-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       0599-EXIT.
            EXIT.
       0600-RECOVER-TEMP-STORAGE.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND  (1500-AGTC-NOT-FOUND)
      *        QIDERR  (0690-QIDERR)
      *        END-EXEC.
      *    MOVE '"$IN                  ! # #00003664' TO DFHEIV0
           MOVE X'2224494E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033363634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READQ TS
      *        QUEUE    (QID)
      *        INTO     (EL652EI)
      *        LENGTH   (QID-MAP-LENGTH)
      *        END-EXEC.
      *    MOVE '*$I    L              ''   #00003668' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL652EI, 
                 QID-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS DELETEQ TS
      *        QUEUE   (QID)
      *    END-EXEC
      *    MOVE '*&                    #   #00003673' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF CARRIERL NOT = 0
               MOVE AL-UANON TO CARRIERA.
           IF GROUPL NOT = 0
               MOVE AL-UANON TO GROUPA.
           IF BANKL NOT = 0
               MOVE AL-UANON TO BANKA
           END-IF
           IF TYPEL NOT = 0
               MOVE AL-UANON           TO TYPEA
           END-IF
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           MOVE CARRIERO               TO WS-CARRIER
           MOVE GROUPO                 TO WS-GROUP
           MOVE BANKO                  TO WS-BANK
           MOVE TYPEO                  TO WS-TYPE
           GO TO 1050-READ-AGTC.
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
           IF TYPEL > ZERO
               MOVE AL-UANON           TO TYPEA
               MOVE TYPEI              TO PI-ERC-TYPE
           ELSE
               MOVE LOW-VALUES         TO PI-ERC-TYPE
           END-IF
           IF (EFFDTL NOT = ZEROS)
              AND (EFFDTI NOT = SPACES)
              MOVE EFFDTI                TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT
              MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY
              MOVE '4'                    TO DC-OPTION-CODE
              PERFORM 9700-DATE-CONVERSION
              IF DATE-CONVERSION-ERROR
                 MOVE ER-0348             TO EMI-ERROR
                 MOVE -1                  TO EFFDTL
                 MOVE AL-UABON            TO EFFDTA
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              ELSE
                 MOVE DC-BIN-DATE-1    TO WS-BIN-EFF-DT
                 MOVE DC-GREG-DATE-1-EDIT TO EFFDTI
                 MOVE AL-UANON            TO EFFDTA
                 SET CHANGES-MADE         TO TRUE
                 IF CHANGE-FUNCTION
                    SET EFF-DT-CHANGE     TO TRUE
                 END-IF
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-0348             TO EMI-ERROR
                 MOVE -1                  TO EFFDTL
                 MOVE AL-UABON            TO EFFDTA
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
           IF (EXPDTL NOT = ZEROS)
              AND (EXPDTI NOT = SPACES)
              MOVE EXPDTI              TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT
              MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
              MOVE '4'                 TO DC-OPTION-CODE
              IF DC-GREG-DATE-1-MDY = 999999
                 MOVE HIGH-VALUES      TO WS-BIN-EXP-DT
                                          PI-ERC-EXP-DT
                 MOVE '99/99/99'       TO EXPDTI
                 MOVE AL-UANON         TO EXPDTA
              ELSE
                 PERFORM 9700-DATE-CONVERSION
                 IF DATE-CONVERSION-ERROR
                    MOVE ER-0454       TO EMI-ERROR
                    MOVE -1            TO EXPDTL
                    MOVE AL-UABON      TO EXPDTA
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 ELSE
                    MOVE DC-BIN-DATE-1 TO WS-BIN-EXP-DT
                                          PI-ERC-EXP-DT
                    MOVE DC-GREG-DATE-1-EDIT
                                       TO EXPDTI
                    MOVE AL-UANON         TO EXPDTA
                 END-IF
              END-IF
           ELSE
              IF ADD-FUNCTION
                 MOVE ER-0454             TO EMI-ERROR
                 MOVE -1                  TO EFFDTL
                 MOVE AL-UABON            TO EFFDTA
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
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
              MOVE GROUPI              TO WS-GROUP
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
           IF TYPEL     NOT = ZEROS
              MOVE TYPEI               TO WS-TYPE
           ELSE
              MOVE ER-2042             TO EMI-ERROR
              MOVE -1                  TO TYPEL
              MOVE AL-UABON            TO TYPEA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           IF EXPDTL    NOT = ZEROS
              MOVE WS-BIN-EXP-DT       TO WS-EXP-DT
           ELSE
              MOVE LOW-VALUES          TO WS-EXP-DT
           END-IF
           IF NOT EMI-NO-ERRORS
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-COMPANY-CD          TO WS-COMPANY-CD
           .
       1050-READ-AGTC.
           
      * EXEC CICS HANDLE CONDITION
      *         NOTFND (1500-AGTC-NOT-FOUND)
      *    END-EXEC
      *    MOVE '"$I                   ! $ #00003966' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033393636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * EXEC CICS STARTBR
      *        DATASET (AGTC-FILE-ID)
      *        RIDFLD (WS-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003969' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE 'Y'                    TO BROWSE-STARTED-SW
           MOVE +1                     TO PI-NDX
           .
       1060-READ-LOOP.
           
      * EXEC CICS READNEXT
      *        DATASET (AGTC-FILE-ID)
      *        SET     (ADDRESS OF AGENT-COMMISSIONS)
      *        RIDFLD  (WS-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003977' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AGENT-COMMISSIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF (AG-COMPANY-CD NOT = PI-COMPANY-CD)
              OR (WS-CARRIER NOT = PI-ERC-CARRIER)
              OR (WS-GROUP   NOT = PI-ERC-GROUP)
              OR (WS-BANK    NOT = PI-ERC-BANK)
              OR (WS-TYPE    NOT = PI-ERC-TYPE)
              GO TO 1500-AGTC-NOT-FOUND
           END-IF
           MOVE AG-LAST-MAINT-USER     TO PI-UPDATE-BY
           IF AG-LAST-MAINT-HHMMSS NUMERIC
              MOVE AG-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF
           MOVE AG-CONTROL-PRIMARY     TO PI-SAVE-ERAGTC-KEY
           MOVE +1                     TO PI-NDX
                                          PI-TOP-NDX
           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           GO TO 5200-FORMAT-SCREEN
           .
       1500-AGTC-NOT-FOUND.
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
           PERFORM VARYING PI-NDX FROM +1 BY +1 UNTIL
              (PI-NDX > +10)
              IF AG-AGT (PI-NDX) NOT = SPACES AND ZEROS
                 MOVE AG-AGT (PI-NDX)  TO REPR (PI-NDX)
                 MOVE AG-COM-TYP (PI-NDX)
                                       TO ATYPE (PI-NDX)
                 IF AG-SPP-FEES (PI-NDX) NOT = ZEROS
                    MOVE AG-SPP-FEES (PI-NDX)
                                       TO FEES-COMM-O (PI-NDX)
                 END-IF
                 MOVE AG-RECALC-LV-INDIC (PI-NDX)
                                       TO RECAL (PI-NDX)
                 IF AG-SPP-LFEES (PI-NDX) NOT NUMERIC
                    MOVE ZEROS         TO AG-SPP-LFEES (PI-NDX)
                 END-IF
                 IF AG-SPP-LFEES (PI-NDX) NOT = ZEROS
                    MOVE AG-SPP-LFEES (PI-NDX)
                                       TO LFEES-COMM-O (PI-NDX)
                 END-IF
                 MOVE AG-LRCALC-LV-INDIC (PI-NDX)
                                       TO LRCAL (PI-NDX)
              END-IF
           END-PERFORM
           .
       3000-XIT.
           EXIT.
       3100-GET-BANK-DATES.
           
      * EXEC CICS STARTBR
      *        DATASET (AGTC-FILE-ID)
      *        RIDFLD  (WS-CONTROL-PRIMARY)
      *        RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00004042' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303034303432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              GO TO 3100-EXIT
           END-IF
           MOVE 'Y'                    TO BROWSE-STARTED-SW
           MOVE +1                     TO PI-NDX
           .
       3100-READ-LOOP.
           
      * EXEC CICS READNEXT
      *        DATASET (AGTC-FILE-ID)
      *        SET     (ADDRESS OF AGENT-COMMISSIONS)
      *        RIDFLD  (WS-CONTROL-PRIMARY)
      *        RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00004054' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303034303534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AGENT-COMMISSIONS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              AND (AG-COMPANY-CD = PI-COMPANY-CD)
              AND (WS-CARRIER = PI-ERC-CARRIER)
              AND (WS-GROUP   = PI-ERC-GROUP)
              AND (WS-BANK    = PI-ERC-BANK)
              AND (WS-TYPE    = PI-ERC-TYPE)
              MOVE AG-EFF-DT           TO WS-TBL-EFF-DT (PI-NDX)
              MOVE AG-EXP-DT           TO WS-TBL-EXP-DT (PI-NDX)
              ADD +1                   TO PI-NDX
              GO TO 3100-READ-LOOP
           END-IF
           .
       3100-EXIT.
           EXIT.
       4200-ADD.
           IF WS-BIN-EXP-DT NOT > WS-BIN-EFF-DT
              MOVE ER-2053             TO EMI-ERROR
              MOVE -1                  TO EFFDTL
              MOVE AL-UABON            TO EFFDTA
                                          EXPDTA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF
           
      * EXEC CICS HANDLE CONDITION
      *        NOTOPEN  (9990-ABEND)
      *        NOTFND   (4250-CONT)
      *    END-EXEC.
      *    MOVE '"$JI                  ! % #00004088' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034303838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 7050-READ-ERAGTC    THRU 7050-EXIT
           MOVE ER-2057                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE LOW-VALUES             TO PI-SAVE-ERAGTC-KEY
           MOVE -1                     TO  MAINTYPL
           GO TO 8200-SEND-DATAONLY
           .
       4250-CONT.
           MOVE LOW-VALUES             TO WS-DATE-TABLE
           MOVE ' '                    TO TABLE-SW
           MOVE WS-CONTROL-PRIMARY     TO PI-SAVE-ERAGTC-KEY
           MOVE LOW-VALUES             TO PI-ERC-EXP-DT
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 3100-GET-BANK-DATES THRU 3100-EXIT
           PERFORM VARYING PI-NDX FROM +1 BY +1 UNTIL
              (WS-TBL-EXP-DT (PI-NDX) = LOW-VALUES)
              OR (PI-NDX > +40)
              IF ((WS-BIN-EXP-DT > WS-TBL-EFF-DT (PI-NDX))
                 AND (WS-BIN-EXP-DT NOT > WS-TBL-EXP-DT (PI-NDX)))
                           OR
                 ((WS-BIN-EFF-DT > WS-TBL-EFF-DT (PI-NDX))
                 AND (WS-BIN-EFF-DT < WS-TBL-EXP-DT (PI-NDX)))
                 SET TABLE-OVERLAP        TO TRUE
              END-IF
           END-PERFORM
           IF BROWSE-STARTED
              
      * EXEC CICS ENDBR
      *            DATASET  (AGTC-FILE-ID)
      *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004119' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              MOVE ' '                 TO BROWSE-STARTED-SW
           END-IF
           IF TABLE-OVERLAP
              MOVE ER-2051             TO EMI-ERROR
              MOVE -1                  TO EFFDTL
              MOVE AL-UABON            TO EFFDTA
                                          EXPDTA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-SAVE-ERAGTC-KEY     TO WS-CONTROL-PRIMARY
                                          PI-ERC-KEY
           
      * EXEC CICS GETMAIN
      *        SET      (ADDRESS OF AGENT-COMMISSIONS)
      *        INITIMG  (GETMAIN-SPACE)
      *        LENGTH   (ERAGTC-LENGTH)
      *    END-EXEC
      *    MOVE ',"IL                  $   #00004135' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERAGTC-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF AGENT-COMMISSIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE SPACES                 TO AGENT-COMMISSIONS
           MOVE PI-COMPANY-CD          TO AG-COMPANY-CD
           MOVE 'AG'                   TO AG-RECORD-ID
           MOVE PI-ERC-KEY             TO AG-CONTROL-PRIMARY
           PERFORM VARYING PI-NDX FROM +1 BY +1 UNTIL
              PI-NDX > +10
              MOVE +0                  TO AG-SPP-FEES (PI-NDX)
              MOVE +0                  TO AG-SPP-LFEES (PI-NDX)
           END-PERFORM
           MOVE +1                     TO PI-NDX
           SET NO-CHANGES-MADE         TO TRUE
           MOVE WS-BIN-EFF-DT          TO AG-EFF-DT
           PERFORM 7300-EDIT-DATA      THRU 7300-EXIT
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              
      * EXEC CICS UNLOCK
      *            DATASET  (AGTC-FILE-ID)
      *       END-EXEC
      *    MOVE '&*                    #   #00004156' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE +0                     TO WS-TOT-FEES
                                          WS-TOT-LFEES
           PERFORM VARYING PI-NDX FROM +2 BY +1 UNTIL
              PI-NDX > +10
              COMPUTE WS-TOT-FEES = WS-TOT-FEES +
                 AG-SPP-FEES (PI-NDX)
              COMPUTE WS-TOT-LFEES = WS-TOT-LFEES +
                 AG-SPP-LFEES (PI-NDX)
           END-PERFORM
           MOVE LOW-VALUES             TO WS-ERCOMP-KEY
           MOVE PI-ERC-KEY (1:18)      TO WS-ERCOMP-KEY (1:18)
           MOVE PI-ERC-TYPE            TO WS-COMP-TYPE
           PERFORM 7350-VERIFY-ERCOMP  THRU 7350-EXIT
           IF RESP-NORMAL
              IF CO-MAX-BANK-FEE NOT NUMERIC
                 MOVE +0               TO CO-MAX-BANK-FEE
              END-IF
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF
           IF CO-MAX-BANK-FEE = ZEROS
              CONTINUE
           ELSE
              IF WS-TOT-FEES > CO-MAX-BANK-FEE
                 MOVE ER-2717          TO EMI-ERROR
                 MOVE -1               TO CARRIERL
                 MOVE AL-UABON         TO CARRIERA
                                          GROUPA
                                          TYPEA
                                          BANKA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              ELSE
                 COMPUTE AG-SPP-FEES (1) = CO-MAX-BANK-FEE
                    - WS-TOT-FEES
              END-IF
           END-IF
           IF CO-MAX-BANK-FEE-LEASE = ZEROS
              CONTINUE
           ELSE
              IF WS-TOT-LFEES > CO-MAX-BANK-FEE-LEASE
                 MOVE ER-2717          TO EMI-ERROR
                 MOVE -1               TO CARRIERL
                 MOVE AL-UABON         TO CARRIERA
                                          GROUPA
                                          TYPEA
                                          BANKA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              ELSE
                 COMPUTE AG-SPP-LFEES (1) = CO-MAX-BANK-FEE-LEASE
                    - WS-TOT-LFEES
              END-IF
           END-IF
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              
      * EXEC CICS UNLOCK
      *            DATASET  (AGTC-FILE-ID)
      *       END-EXEC
      *    MOVE '&*                    #   #00004218' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-PROCESSOR-ID        TO AG-LAST-MAINT-USER
           MOVE EIBTIME                TO AG-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO AG-LAST-MAINT-DT
           
      * EXEC CICS WRITE
      *        DATASET  (AGTC-FILE-ID)
      *        FROM     (AGENT-COMMISSIONS)
      *        RIDFLD   (AG-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE LENGTH OF
            AGENT-COMMISSIONS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004226' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 AGENT-COMMISSIONS, 
                 DFHEIV11, 
                 AG-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           GO TO 1050-READ-AGTC
      *    MOVE LOW-VALUES             TO EL652EO
      *    MOVE 'S'                    TO MAINTYPO
      *    MOVE +1                     TO MAINTYPL
      *    MOVE AL-UANON               TO MAINTYPA
      *    MOVE PI-ERC-CARRIER         TO CARRIERO
      *    MOVE AL-UANON               TO CARRIERA
      *    MOVE +1                     TO CARRIERL
      *    IF PI-ERC-GROUP NOT = SPACES
      *        MOVE PI-ERC-GROUP       TO GROUPO
      *        MOVE AL-UANON           TO GROUPA
      *        MOVE +6                 TO GROUPL
      *    END-IF
      *
      *    IF PI-ERC-BANK NOT = SPACES
      *       MOVE PI-ERC-BANK         TO BANKO
      *       MOVE AL-UANON            TO BANKA
      *       MOVE +10                 TO BANKL
      *    END-IF
      *    IF PI-ERC-TYPE NOT = SPACES
      *       MOVE PI-ERC-TYPE         TO TYPEO
      *       MOVE AL-UANON            TO TYPEA
      *       MOVE +1                  TO TYPEL
      *    END-IF
      *    GO TO 1000-EDIT-INPUT
           .
       4200-EXIT.
           EXIT.
       4400-CHANGE.
           IF PI-ERC-KEY = PI-SAVE-ERAGTC-KEY
              CONTINUE
           ELSE
              IF PI-ERC-KEY (1:18) = PI-SAVE-ERAGTC-KEY (1:18)
                 PERFORM 4650-CHG-EXPDT THRU 4650-EXIT
              ELSE
                 MOVE ER-2056          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 MOVE -1               TO MAINTYPL
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF
           IF EFF-DT-CHANGE
              PERFORM 4660-DATE-CHANGE THRU 4660-EXIT
              IF NOT EMI-NO-ERRORS
                 GO TO 8200-SEND-DATAONLY
              END-IF
           END-IF
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 7200-READ-ERAGTC-UPDATE
                                       THRU 7200-EXIT
           IF (AG-LAST-MAINT-USER = PI-UPDATE-BY)
              AND (AG-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
              CONTINUE
           ELSE
              
      * EXEC CICS UNLOCK
      *            DATASET  (AGTC-FILE-ID)
      *       END-EXEC
      *    MOVE '&*                    #   #00004286' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              MOVE ER-0068             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-TOP-NDX             TO PI-NDX
      *    SET NO-CHANGES-MADE         TO TRUE
           PERFORM 7300-EDIT-DATA      THRU 7300-EXIT
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              
      * EXEC CICS UNLOCK
      *            DATASET  (AGTC-FILE-ID)
      *       END-EXEC
      *    MOVE '&*                    #   #00004300' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              GO TO 8200-SEND-DATAONLY
           END-IF
           IF (NO-CHANGES-MADE)
              AND (NO-LINE-CHANGES)
              MOVE ER-3112             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE +0                     TO WS-TOT-FEES
                                          WS-TOT-LFEES
           PERFORM VARYING PI-NDX FROM +2 BY +1 UNTIL
              PI-NDX > +10
              COMPUTE WS-TOT-FEES = WS-TOT-FEES +
                 AG-SPP-FEES (PI-NDX)
              COMPUTE WS-TOT-LFEES = WS-TOT-LFEES +
                 AG-SPP-LFEES (PI-NDX)
           END-PERFORM
           MOVE LOW-VALUES             TO WS-ERCOMP-KEY
           MOVE PI-ERC-KEY (1:18)      TO WS-ERCOMP-KEY (1:18)
           MOVE PI-ERC-TYPE            TO WS-COMP-TYPE
           PERFORM 7350-VERIFY-ERCOMP  THRU 7350-EXIT
           IF RESP-NORMAL
              IF CO-MAX-BANK-FEE NOT NUMERIC
                 MOVE +0               TO CO-MAX-BANK-FEE
              END-IF
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF
           IF CO-MAX-BANK-FEE = ZEROS
              CONTINUE
           ELSE
              IF WS-TOT-FEES > CO-MAX-BANK-FEE
                 MOVE ER-2717          TO EMI-ERROR
                 MOVE -1               TO CARRIERL
                 MOVE AL-UABON         TO CARRIERA
                                          GROUPA
                                          TYPEA
                                          BANKA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              ELSE
                 COMPUTE AG-SPP-FEES (1) = CO-MAX-BANK-FEE
                    - WS-TOT-FEES
              END-IF
           END-IF
           IF CO-MAX-BANK-FEE-LEASE = ZEROS
              CONTINUE
           ELSE
              IF WS-TOT-LFEES > CO-MAX-BANK-FEE-LEASE
                 MOVE ER-2717          TO EMI-ERROR
                 MOVE -1               TO CARRIERL
                 MOVE AL-UABON         TO CARRIERA
                                          GROUPA
                                          TYPEA
                                          BANKA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              ELSE
                 COMPUTE AG-SPP-LFEES (1) = CO-MAX-BANK-FEE-LEASE
                    - WS-TOT-LFEES
              END-IF
           END-IF
           IF EFFDTL NOT = ZEROS
              MOVE WS-BIN-EFF-DT       TO AG-EFF-DT
           END-IF
           IF EMI-NO-ERRORS
              CONTINUE
           ELSE
              
      * EXEC CICS UNLOCK
      *            DATASET  (AGTC-FILE-ID)
      *       END-EXEC
      *    MOVE '&*                    #   #00004373' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-PROCESSOR-ID        TO AG-LAST-MAINT-USER
           MOVE EIBTIME                TO AG-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO AG-LAST-MAINT-DT
           
      * EXEC CICS REWRITE
      *        DATASET  (AGTC-FILE-ID)
      *        FROM     (AGENT-COMMISSIONS)
      *    END-EXEC
           MOVE LENGTH OF
            AGENT-COMMISSIONS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004381' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 AGENT-COMMISSIONS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           GO TO 1050-READ-AGTC
      *    GO TO 1000-EDIT-INPUT
           .
       4600-DELETE.
           IF PI-ERC-KEY = PI-SAVE-ERAGTC-KEY
              CONTINUE
           ELSE
              MOVE ER-2056             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           PERFORM 7200-READ-ERAGTC-UPDATE
                                       THRU 7200-EXIT
           IF (AG-LAST-MAINT-USER = PI-UPDATE-BY)
              AND (AG-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
              CONTINUE
           ELSE
              
      * EXEC CICS UNLOCK
      *            DATASET  (AGTC-FILE-ID)
      *       END-EXEC
      *    MOVE '&*                    #   #00004405' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              MOVE ER-0068             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           
      * EXEC CICS DELETE
      *        DATASET  (AGTC-FILE-ID)
      *    END-EXEC
      *    MOVE '&(                    &   #00004413' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE ER-0000                TO  EMI-ERROR
           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
           MOVE LOW-VALUES             TO  EL652EO
           MOVE PI-ERC-CARRIER         TO  CARRIERO
           MOVE AL-UANON               TO  CARRIERA
           IF PI-ERC-GROUP NOT = SPACES
               MOVE PI-ERC-GROUP       TO  GROUPO
               MOVE AL-UANON           TO  GROUPA
           END-IF
           IF PI-ERC-BANK    NOT = SPACES
              MOVE PI-ERC-BANK        TO  BANKO
              MOVE AL-UANON           TO  BANKA
           END-IF
           IF PI-ERC-TYPE NOT = SPACES
              MOVE PI-ERC-TYPE        TO  TYPEO
              MOVE AL-UANON           TO  TYPEA
           END-IF
           MOVE LOW-VALUES             TO  PI-SAVE-ERAGTC-KEY
           GO TO 8100-SEND-INITIAL-MAP
           .
       4600-EXIT.
           EXIT.
       4650-CHG-EXPDT.
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           
      * EXEC CICS READ
      *         SET     (ADDRESS OF AGENT-COMMISSIONS)
      *         DATASET ('ERAGTC')
      *         RIDFLD  (WS-CONTROL-PRIMARY)
      *         RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERAGTC' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00004440' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034343430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AGENT-COMMISSIONS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              MOVE ER-2057             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE LOW-VALUES          TO PI-SAVE-ERAGTC-KEY
              MOVE -1                  TO  MAINTYPL
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-SAVE-ERAGTC-KEY     TO WS-CONTROL-PRIMARY
           PERFORM 7200-READ-ERAGTC-UPDATE
                                       THRU 7200-EXIT
           IF (AG-LAST-MAINT-USER = PI-UPDATE-BY)
              AND (AG-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
              CONTINUE
           ELSE
              
      * EXEC CICS UNLOCK
      *            DATASET  (AGTC-FILE-ID)
      *       END-EXEC
      *    MOVE '&*                    #   #00004461' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              MOVE ER-0068             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE AGENT-COMMISSIONS     TO WS-SAVE-ERAGTC
           
      * EXEC CICS DELETE
      *        DATASET  (AGTC-FILE-ID)
      *    END-EXEC
      *    MOVE '&(                    &   #00004470' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE WS-SAVE-ERAGTC         TO AGENT-COMMISSIONS
           MOVE PI-ERC-KEY             TO AG-CONTROL-PRIMARY
           MOVE PI-PROCESSOR-ID        TO AG-LAST-MAINT-USER
           MOVE EIBTIME                TO AG-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO AG-LAST-MAINT-DT
           
      * EXEC CICS WRITE
      *        DATASET  (AGTC-FILE-ID)
      *        FROM     (AGENT-COMMISSIONS)
      *        RIDFLD   (AG-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE LENGTH OF
            AGENT-COMMISSIONS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004478' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 AGENT-COMMISSIONS, 
                 DFHEIV11, 
                 AG-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE PI-ERC-KEY             TO WS-CONTROL-PRIMARY
           GO TO 1050-READ-AGTC
           .
       4650-EXIT.
           EXIT.
       4660-DATE-CHANGE.
           MOVE PI-SAVE-ERAGTC-KEY     TO WS-SKIP-KEY
           MOVE LOW-VALUES             TO WS-CONTROL-PRIMARY
           MOVE PI-SAVE-ERAGTC-KEY (1:18)
                                       TO WS-CONTROL-PRIMARY (1:18)
           
      * EXEC CICS STARTBR
      *        DATASET (AGTC-FILE-ID)
      *        RIDFLD  (WS-CONTROL-PRIMARY)
      *        GTEQ
      *        RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00004493' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303034343933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              GO TO 1500-AGTC-NOT-FOUND
           END-IF
           MOVE 'Y'                    TO BROWSE-STARTED-SW
           MOVE SPACES                 TO WS-CHECK-DATE-SW
           PERFORM UNTIL
              (DATE-CHECK-OVER)
              PERFORM 4095-READNEXT-ERAGTC
                                       THRU 4095-EXIT
              IF RESP-NORMAL
                 IF AG-CONTROL-PRIMARY (1:18) =
                                 PI-SAVE-ERAGTC-KEY (1:18)
                    IF AG-CONTROL-PRIMARY NOT = WS-SKIP-KEY
                       IF (WS-BIN-EFF-DT NOT < AG-EFF-DT)
                          AND (WS-BIN-EFF-DT < AG-EXP-DT)
                          MOVE ER-0348 TO EMI-ERROR
                          MOVE -1      TO EFFDTL
                          MOVE AL-UABON
                                       TO EFFDTA
                          PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                          SET DATE-CHECK-OVER TO TRUE
                       END-IF
                    END-IF
                 ELSE
                    SET DATE-CHECK-OVER TO TRUE
                 END-IF
              ELSE
                 SET DATE-CHECK-OVER   TO TRUE
              END-IF
           END-PERFORM
           IF BROWSE-STARTED
              
      * EXEC CICS ENDBR
      *            DATASET  (AGTC-FILE-ID)
      *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004531' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              MOVE ' '                 TO BROWSE-STARTED-SW
           END-IF
           .
       4660-EXIT.
           EXIT.
       4095-READNEXT-ERAGTC.
           
      * EXEC CICS READNEXT
      *        DATASET (AGTC-FILE-ID)
      *        SET     (ADDRESS OF AGENT-COMMISSIONS)
      *        RIDFLD  (WS-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004540' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AGENT-COMMISSIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       4095-EXIT.
           EXIT.
       5000-BROWSE-FILE.
           MOVE SPACE                  TO BROWSE-STARTED-SW
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (5800-NO-RECORD)
      *        ENDFILE (5900-END-OF-FILE)
      *    END-EXEC
      *    MOVE '"$I''                  ! & #00004550' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034353530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           MOVE PI-SAVE-ERAGTC-KEY     TO WS-CONTROL-PRIMARY
                                          PI-ERC-KEY
           
      * EXEC CICS STARTBR
      *        DATASET (AGTC-FILE-ID)
      *        RIDFLD (WS-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004556' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
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
           PERFORM 4095-READNEXT-ERAGTC
                                       THRU 4095-EXIT
           MOVE +1                     TO PI-NDX
           IF AG-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 5900-END-OF-FILE
           END-IF
      *    IF EIBAID = DFHPF1
      *       IF CARRIERO          = AG-CARRIER
      *          AND GROUPO        = AG-GROUPING
      *          AND BANKO         = AG-BANK
      *          AND TYPEO         = AG-TYPE
      *          AND WS-BIN-EXP-DT = AG-EXP-DT
      *          GO TO 5010-READ-LOOP
      *       END-IF
      *    END-IF
           IF EIBAID = DFHPF1
              IF PI-ERC-CARRIER    = AG-CARRIER
                 AND PI-ERC-GROUP  = AG-GROUPING
                 AND PI-ERC-BANK   = AG-BANK
                 AND PI-ERC-TYPE   = AG-TYPE
                 AND PI-ERC-EXP-DT = AG-EXP-DT
                 GO TO 5010-READ-LOOP
              END-IF
           END-IF
           MOVE AG-LAST-MAINT-USER     TO PI-UPDATE-BY
           IF AG-LAST-MAINT-HHMMSS NUMERIC
              MOVE AG-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF
           MOVE AG-CONTROL-PRIMARY     TO PI-SAVE-ERAGTC-KEY
           GO TO 5200-FORMAT-SCREEN
           .
       5100-BROWSE-BKWD.
           
      * EXEC CICS HANDLE CONDITION
      *         NOTFND (5900-END-OF-FILE)
      *    END-EXEC
      *    MOVE '"$I                   ! '' #00004601' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034363031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * EXEC CICS READPREV
      *        DATASET (AGTC-FILE-ID)
      *        SET     (ADDRESS OF AGENT-COMMISSIONS)
      *        RIDFLD  (WS-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004604' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AGENT-COMMISSIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF PI-FILE-EOF
              MOVE SPACE               TO PI-EOF-SW
           ELSE
              
      * EXEC CICS READPREV
      *            DATASET (AGTC-FILE-ID)
      *            SET     (ADDRESS OF AGENT-COMMISSIONS)
      *            RIDFLD  (WS-CONTROL-PRIMARY)
      *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004612' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AGENT-COMMISSIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           MOVE +1                     TO PI-NDX
           IF AG-COMPANY-CD NOT = PI-COMPANY-CD
              GO TO 5900-END-OF-FILE
           END-IF
           MOVE AG-LAST-MAINT-USER     TO PI-UPDATE-BY
           IF AG-LAST-MAINT-HHMMSS NUMERIC
              MOVE AG-LAST-MAINT-HHMMSS
                                       TO PI-UPDATE-HHMMSS
           ELSE
              MOVE ZEROS               TO PI-UPDATE-HHMMSS
           END-IF
           MOVE AG-CONTROL-PRIMARY     TO PI-SAVE-ERAGTC-KEY
           .
       5200-FORMAT-SCREEN.
           MOVE LOW-VALUES             TO EL652EI
           MOVE AG-CONTROL-PRIMARY     TO PI-ERAGTC-KEY
           MOVE AG-CARRIER             TO CARRIERO
           MOVE AG-GROUPING            TO GROUPO
           MOVE AG-BANK                TO BANKO
           MOVE AG-TYPE                TO TYPEO
           IF AG-LAST-MAINT-DT NOT = SPACES AND LOW-VALUES AND ZEROS
              MOVE AG-LAST-MAINT-DT    TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-EDIT TO MAINTDTO
           ELSE
              MOVE '00/00/00'          TO MAINTDTO
           END-IF
           MOVE AG-LAST-MAINT-USER     TO MAINTBYO
           IF AG-EXP-DT NOT = HIGH-VALUES
              MOVE AG-EXP-DT           TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-EDIT TO EXPDTO
           ELSE
              MOVE '99/99/99'          TO EXPDTO
           END-IF
           IF AG-EFF-DT NOT = LOW-VALUES AND ZEROS AND SPACES
              MOVE AG-EFF-DT           TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
           ELSE
              MOVE '00/00/00'          TO EFFDTO
           END-IF
           PERFORM 3000-BLD-LINE       THRU 3000-XIT
           MOVE AL-UANON               TO CARRIERA GROUPA
                                          BANKA TYPEA EXPDTA
           GO TO 8100-SEND-INITIAL-MAP
           .
       5800-NO-RECORD.
           MOVE -1                     TO CARRIERL
           MOVE AL-UANON               TO CARRIERA  GROUPA
                                           BANKA TYPEA
           MOVE ER-1164                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           IF BROWSE-STARTED
              
      * EXEC CICS ENDBR
      *            DATASET  (AGTC-FILE-ID)
      *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004678' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           GO TO 8200-SEND-DATAONLY
           .
       5900-END-OF-FILE.
           IF EIBAID = DFHPF1
              MOVE 'Y'                 TO PI-EOF-SW
              MOVE ER-2237             TO EMI-ERROR
           ELSE
              MOVE LOW-VALUES          TO PI-ERAGTC-KEY
              MOVE PI-COMPANY-CD       TO PI-AGTC-COMP-CD
              MOVE ER-2238             TO EMI-ERROR
           END-IF
           MOVE -1                     TO CARRIERL
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           IF BROWSE-STARTED
              
      * EXEC CICS ENDBR
      *            DATASET  (AGTC-FILE-ID)
      *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004696' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AGTC-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
           MOVE SPACE                  TO BROWSE-STARTED-SW
           GO TO 8200-SEND-DATAONLY
           .
       7050-READ-ERAGTC.
           
      * EXEC CICS READ
      *         SET     (ADDRESS OF AGENT-COMMISSIONS)
      *         DATASET ('ERAGTC')
      *         RIDFLD  (WS-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE 'ERAGTC' TO DFHEIV1
      *    MOVE '&"S        E          (   #00004704' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AGENT-COMMISSIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       7050-EXIT.
           EXIT.
       7200-READ-ERAGTC-UPDATE.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (7200-AGTC-NOT-FOUND)
      *    END-EXEC
      *    MOVE '"$I                   ! ( #00004713' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034373133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * EXEC CICS READ
      *         SET     (ADDRESS OF AGENT-COMMISSIONS)
      *         DATASET ('ERAGTC')
      *         RIDFLD  (WS-CONTROL-PRIMARY)
      *         UPDATE
      *    END-EXEC
           MOVE 'ERAGTC' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00004716' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AGENT-COMMISSIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       7200-EXIT.
           EXIT.
           .
       7200-AGTC-NOT-FOUND.
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
              (NDX > +10)
              IF (REPRL      (NDX) NOT > +0)
                 AND (ATYPEL  (NDX) NOT > +0)
                 AND (FEESL   (NDX) NOT > +0)
                 AND (RECALL   (NDX) NOT > +0)
092205           AND (LFEESL   (NDX) NOT > +0)
092205           AND (LRCALL   (NDX) NOT > +0)
                 CONTINUE
              ELSE
               IF REPRL (NDX) > +0
                 IF REPR (NDX) = SPACES OR ZEROS
                    MOVE SPACES        TO ATYPE (NDX)
                                          RECAL (NDX)
                                          LRCAL (NDX)
                    MOVE ZEROS         TO FEES-COMM-R (NDX)
                                          LFEES-COMM-R (NDX)
                    MOVE +1            TO ATYPEL  (NDX)
                                          FEESL   (NDX)
                                          RECALL  (NDX)
                                          LFEESL  (NDX)
                                          LRCALL  (NDX)
                 ELSE
                    MOVE WS-CONTROL-PRIMARY (1:8)
                                       TO WS-ERCOMP-KEY (1:8)
                    MOVE REPR (NDX)    TO WS-COMP-FINRESP
                    MOVE LOW-VALUES    TO WS-COMP-ACCOUNT
                    MOVE ATYPE (NDX)   TO WS-COMP-TYPE
                    PERFORM 7350-VERIFY-ERCOMP
                                       THRU 7350-EXIT
                 END-IF
                 SET LINE-CHANGES      TO TRUE
                 MOVE REPR (NDX)       TO AG-AGT (NDX)
               END-IF
               IF ATYPEL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE ATYPE (NDX)      TO AG-COM-TYP (NDX)
               END-IF
               IF FEESL   (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 
      * EXEC CICS BIF
      *               DEEDIT
      *               FIELD   (FEES-COMM-R (NDX))
      *               LENGTH  (8)
      *          END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004779' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FEES-COMM-R(NDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF FEES-COMM-R (NDX) NUMERIC
                    MOVE FEES-COMM-R (NDX)
                                       TO AG-SPP-FEES (NDX)
                 END-IF
               END-IF
               IF RECALL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE RECAL (NDX)      TO AG-RECALC-LV-INDIC (NDX)
               END-IF
               IF LFEESL   (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 
      * EXEC CICS BIF
      *               DEEDIT
      *               FIELD   (LFEES-COMM-R (NDX))
      *               LENGTH  (8)
      *          END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004795' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LFEES-COMM-R(NDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF LFEES-COMM-R (NDX) NUMERIC
                    MOVE LFEES-COMM-R (NDX)
                                       TO AG-SPP-LFEES (NDX)
                 END-IF
               END-IF
               IF LRCALL (NDX) > +0
                 SET LINE-CHANGES      TO TRUE
                 MOVE LRCAL (NDX)      TO AG-LRCALC-LV-INDIC (NDX)
               END-IF
               IF AG-AGT (NDX) NOT = SPACES AND ZEROS
                  IF AG-COM-TYP (NDX) = SPACES OR ZEROS
                     MOVE ER-2173      TO EMI-ERROR
                     MOVE -1           TO ATYPEL (NDX)
                     MOVE AL-UABON     TO ATYPEA (NDX)
                     PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                  END-IF
               END-IF
              END-IF
           END-PERFORM
           .
       7300-EXIT.
           EXIT.
       7350-VERIFY-ERCOMP.
           
      * EXEC CICS READ
      *         DATASET  ('ERCOMP')
      *         SET      (ADDRESS OF COMPENSATION-MASTER)
      *         RIDFLD   (WS-ERCOMP-KEY)
      *         RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00004824' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              MOVE ER-2230             TO EMI-ERROR
              MOVE -1                  TO REPRL (NDX)
              MOVE AL-UABON            TO REPRA (NDX)
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           .
       7350-EXIT.
           EXIT.
       7375-READ-ERCOMP.
           MOVE +0                     TO WS-TOT-FEES
                                          WS-TOT-LFEES
           MOVE WS-CONTROL-PRIMARY     TO WS-ERCOMP-KEY
           
      * EXEC CICS READ
      *         DATASET  ('ERCOMP')
      *         SET      (ADDRESS OF COMPENSATION-MASTER)
      *         RIDFLD   (WS-ERCOMP-KEY)
      *         RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00004844' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              CONTINUE
           ELSE
              MOVE ER-2230             TO EMI-ERROR
              MOVE -1                  TO REPRL (NDX)
              MOVE AL-UABON            TO REPRA (NDX)
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           .
       7375-EXIT.
           EXIT.
       7400-READ-CONTROL-FILE.
           MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
           MOVE WS-ACCESS              TO CNTL-ACCESS
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND  (7490-NOT-FOUND)
      *        ERROR   (9990-ABEND)
      *    END-EXEC
      *    MOVE '"$I.                  ! ) #00004865' TO DFHEIV0
           MOVE X'2224492E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303034383635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * EXEC CICS READ
      *        DATASET  (CNTL-FILE-ID)
      *        SET      (ADDRESS OF CONTROL-FILE)
      *        RIDFLD   (ELCNTL-KEY)
      *    END-EXEC
      *    MOVE '&"S        E          (   #00004869' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383639' TO DFHEIV0(25:11)
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
       8100-SEND-INITIAL-MAP.
           MOVE SAVE-DATE              TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
           MOVE -1                     TO MAINTYPL
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
           
      * EXEC CICS SEND
      *        MAP    (MAP-NAME)
      *        MAPSET (MAPSET-NAME)
      *        FROM   (EL652EO)
      *        ERASE
      *        CURSOR
      *        END-EXEC.
           MOVE LENGTH OF
            EL652EO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004896' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL652EO, 
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
      *        FROM   (EL652EO)
      *        DATAONLY
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL652EO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004909' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL652EO, 
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
      *    MOVE '."C                   ''   #00004920' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393230' TO DFHEIV0(25:11)
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
      *        FIELD   (DEEDIT-FIELD)
      *        LENGTH  (8)
      *    END-EXEC.
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004928' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
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
           MOVE '652E'               TO PI-CURRENT-SCREEN-NO.
           
      * EXEC CICS RETURN
      *        TRANSID(TRANS-ID)
      *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
      *        LENGTH(PI-COMM-LENGTH)
      *        END-EXEC.
      *    MOVE '.(CT                  &   #00004941' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6524' TO DFHEIV1
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
      *    MOVE '*$II   L              ''   #00004959' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393539' TO DFHEIV0(25:11)
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
      *    MOVE '.$C                   $   #00004987' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393837' TO DFHEIV0(25:11)
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
      *    MOVE '."C                   ''   #00004999' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393939' TO DFHEIV0(25:11)
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
      *    MOVE '."C                   ''   #00005009' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303039' TO DFHEIV0(25:11)
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
      *    MOVE '."C                   ''   #00005019' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303139' TO DFHEIV0(25:11)
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
      *    MOVE '."C                   ''   #00005043' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303433' TO DFHEIV0(25:11)
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
           MOVE 'EL6524' TO DFHEIV1
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
               GO TO 1500-AGTC-NOT-FOUND,
                     0690-QIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1500-AGTC-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 9990-ABEND,
                     4250-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 5800-NO-RECORD,
                     5900-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 5900-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7200-AGTC-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 7490-NOT-FOUND,
                     9990-ABEND
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6524' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
