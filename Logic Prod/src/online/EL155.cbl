00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL155 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 11/01/95 08:50:15.
00007 *                            VMOD=2.003.
00008 *
00008 *
00009 *AUTHOR.        LOGIC, INC.
00010 *               DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS. EX31 - POLICY FORM DISPLAY.
00025      EJECT
00026  ENVIRONMENT DIVISION.
00027  DATA DIVISION.
00028  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00029  01  LCP-TIME-OF-DAY-XX.
00030      05  LCP-TIME-OF-DAY-68        PIC 9(6).
00031      05  FILLER                    PIC 99.
00032  01  LCP-CICS-TIME                 PIC 9(15).
00033  77  FILLER  PIC X(32)  VALUE '********************************'.
00034  77  FILLER  PIC X(32)  VALUE '*   EL155  WORKING STORAGE     *'.
00035  77  FILLER  PIC X(32)  VALUE '************ V/M 2.003 *********'.
00036
00037  01  WS-DATE-AREA.
00038      05  SAVE-DATE               PIC X(8)    VALUE SPACES.
00039      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.
00040
00041  01  LITERALS-NUMBERS.
00042      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.
00043      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.
00044      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.
00045
00046      12  THIS-PROG               PIC X(8)    VALUE 'EL155'.
00047      12  TRANS-ID                PIC X(4)    VALUE 'EX31'.
00048      12  WS-DMD-PASSED-KEY       PIC X(6).
00049
00050  01  ERROR-NUMBERS.
00051      12  ER-0004                 PIC X(4)    VALUE '0004'.
00052      12  ER-7008                 PIC X(4)    VALUE '7008'.
00053      12  ER-0015                 PIC X(4)    VALUE '0015'.
00054      12  ER-0029                 PIC X(4)    VALUE '0029'.
00055      12  ER-0126                 PIC X(4)    VALUE '0126'.
00056      12  ER-0130                 PIC X(4)    VALUE '0130'.
00057      12  ER-0131                 PIC X(4)    VALUE '0131'.
00058      12  ER-0418                 PIC X(4)    VALUE '0418'.
00059
00060  01  EDIT-WORK-AREA.
00061      12  COUNT-1                 PIC 99.
00062      12  CHECK-PFKEYS            PIC 99.
00063      12  CALL-PGM                PIC X(8).
00064
00065  01  FORMATTED-LINE.
00066      12  FILLER                  PIC X(2)    VALUE SPACES.
00067      12  HOLD-LINE               PIC X(70).
00068      12  FILLER                  PIC X(7)    VALUE SPACES.
00069
00070  01  TIME-UNFORMATTED.
00071      12  UN-HOURS                PIC X(2).
00072      12  UN-MINUTES              PIC X(2).
00073      12  FILLER                  PIC X(2).
00074
00075  01  TIME-FORMATTED.
00076      12  FOR-HOURS               PIC X(2).
00077      12  FILLER                  PIC X       VALUE '.'.
00078      12  FOR-MINUTES             PIC X(2).
00079
00080  01  ERROR-SWITCHES.
00081      12  ERROR-SWITCH            PIC X       VALUE SPACE.
00082          88  END-OF-FILE                     VALUE 'E'.
00083          88  SCREEN-ERROR                    VALUE 'X'.
00084
00085  01  ELFORM-KEY.
00086      12  FILE-PARTIAL-KEY.
00087          16  COMPANY-CODE        PIC X.
00088          16  FORM-NO             PIC X(12).
00089      12  LINE-SEQUENCE           PIC S9(4)   COMP.
00090
00091      EJECT
00092 *    COPY ELCLOGOF.
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
00093
00094      EJECT
00095 *    COPY ELCDATE.
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
00096
00097      EJECT
00098 *    COPY EL155S.
       01  EL155AI.
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
           05  FORMNOL PIC S9(0004) COMP.
           05  FORMNOF PIC  X(0001).
           05  FILLER REDEFINES FORMNOF.
               10  FORMNOA PIC  X(0001).
           05  FORMNOI PIC  X(0012).
           05  INFOD OCCURS 15  TIMES.
      *    -------------------------------
               10  INFOL PIC S9(0004) COMP.
               10  INFOF PIC  X(0001).
               10  FILLER REDEFINES INFOF.
                   15  INFOA PIC  X(0001).
               10  INFOI PIC  X(0079).
      *    -------------------------------
           05  MSGL PIC S9(0004) COMP.
           05  MSGF PIC  X(0001).
           05  FILLER REDEFINES MSGF.
               10  MSGA PIC  X(0001).
           05  MSGI PIC  X(0075).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  X(0002).
       01  EL155AO REDEFINES EL155AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORMNOO PIC  X(0012).
      *    -------------------------------
           05  INFOD OCCURS 15  TIMES.
               10  FILLER        PIC  X(0003).
               10  INFOO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSGO PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
00099
00100      EJECT
00101 *    COPY ELCAID.
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
00102
00103  01  FILLER REDEFINES DFHAID.
00104      12  FILLER                  PIC X(8).
00105      12  AID-KEYS OCCURS 24 TIMES.
00106          16  FILLER              PIC X.
00107      EJECT
00108 *    COPY ELCINTF.
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
00109
00110      12  EL155-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00111          16  PASSED-KEY          PIC X(12).
00112          16  SAVE-KEY.
00113           20  SAVE-PARTIAL-KEY.
00114                  24  FILLER          PIC X.
00115                  24  SAVE-CONTROL    PIC X(12).
00116              20  SAVE-LINE           PIC S9(4)   COMP.
00117          16  EOF-SWITCH          PIC X.
00118              88  TOP-OF-FILE                 VALUE 'X'.
00119              88  BOTTOM-OF-FILE              VALUE 'Y'.
00120              88  BAD-BROWSE                  VALUE 'Z'.
00121          16  FILLER              PIC X(514).
00122          16  PASSED-FORM-EL150   PIC X(12).
00123          16  FILLER              PIC X(86).
00124
00125      EJECT
00126 *    COPY ELCEMIB.
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
00127
00128      EJECT
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
00130  01  DFHCOMMAREA                 PIC X(1024).
00131
00132 *    COPY ELCTEXT.
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
00133
00134      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA TEXT-FILES.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL155' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00136
00137      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00138      MOVE '5'                   TO DC-OPTION-CODE.
00139      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00140      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00141      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00142
00143      IF EIBCALEN = ZERO
00144          GO TO 8800-UNAUTHORIZED-ACCESS.
00145
00146      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00147
00148      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00149          MOVE LOW-VALUES         TO EL155AO
00150          MOVE ER-7008            TO EMI-ERROR
00151          PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
00152          GO TO 8110-SEND-DATA.
00153
00154      IF PI-CALLING-PROGRAM NOT = THIS-PROG
00155          MOVE LOW-VALUES         TO EL155AO
00156          GO TO 0100-UPDATE-PI.
00157
00158      IF EIBAID = DFHCLEAR
00159          GO TO 8200-RETURN-PRIOR.
00160
00161      
      * EXEC CICS HANDLE CONDITION
00162 *        PGMIDERR  (8820-PGMID-ERROR)
00163 *        MAPFAIL   (8100-SEND-MAP)
00164 *        ERROR     (9990-ABEND)
00165 *    END-EXEC.
      *    MOVE '"$L?.                 ! " #00001023' TO DFHEIV0
           MOVE X'22244C3F2E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031303233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00166
00167      
      * EXEC CICS RECEIVE
00168 *        MAP     ('EL155A')
00169 *        MAPSET  ('EL155S')
00170 *    END-EXEC.
           MOVE 'EL155A' TO DFHEIV1
           MOVE 'EL155S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00001029' TO DFHEIV0
           MOVE X'382254202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL155AI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00171
00172      IF PFKEYL GREATER THAN ZERO
00173          PERFORM 0200-TRANS-PF THRU 0210-TRANS-PF-EXIT.
00174
00175      IF SCREEN-ERROR
00176          MOVE ER-7008            TO EMI-ERROR
00177          PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
00178          GO TO 8110-SEND-DATA.
00179
00180      IF EIBAID = DFHPF12
00181          GO TO 8300-GET-HELP.
00182
00183      IF EIBAID = DFHPF23
00184          GO TO 8810-PF23-ENTERED.
00185
00186      IF EIBAID = DFHPF24
00187          GO TO 8400-RETURN-MASTER.
00188
00189      IF EIBAID = DFHPF1 OR DFHPF3
00190          GO TO 2000-BROWSE-FORWARD.
00191
00192      IF EIBAID = DFHPF2 OR DFHPF4
00193          GO TO 2100-BROWSE-BACKWARD.
00194
00195      IF EIBAID = DFHENTER
00196          GO TO 2000-BROWSE-FORWARD.
00197
00198      MOVE ER-0029                TO EMI-ERROR.
00199      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.
00200      GO TO 8110-SEND-DATA.
00201      EJECT
00202
00203  0100-UPDATE-PI.
00204      IF PI-CALLING-PROGRAM = 'EL150'
00205          MOVE PASSED-FORM-EL150  TO PASSED-KEY.
00206
00207      MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6.
00208      MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5.
00209      MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4.
00210      MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3.
00211      MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2.
00212      MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1.
00213      MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM.
00214      MOVE THIS-PROG              TO PI-CALLING-PROGRAM.
00215      MOVE ZEROS                  TO SAVE-LINE   SAVE-PARTIAL-KEY.
00216      MOVE SPACES                 TO EOF-SWITCH.
00217
00218      IF PI-RETURN-TO-PROGRAM NOT = XCTL-EL126
00219         IF PASSED-KEY = SPACES
00220            MOVE ER-0418          TO EMI-ERROR
00221            PERFORM 9900-ERROR-FORMAT
00222               THRU 9900-ERROR-FORMAT-EXIT
00223            GO TO 8110-SEND-DATA
00224         ELSE
00225         IF PI-COMPANY-ID = 'DMD'
00226            MOVE PASSED-KEY (7:6)  TO WS-DMD-PASSED-KEY
00227            MOVE WS-DMD-PASSED-KEY TO PASSED-KEY
00228         END-IF
00229            MOVE PASSED-KEY        TO FORMNOO
00230            GO TO 2000-BROWSE-FORWARD.
00231
00232      GO TO 8100-SEND-MAP.
00233
00234  0200-TRANS-PF.
00235
00236      IF EIBAID NOT = DFHENTER
00237         MOVE ER-0004             TO EMI-ERROR
00238         PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT
00239         MOVE -1                  TO PFKEYL
00240         GO TO 8110-SEND-DATA.
00241
00242      IF PFKEYI NOT NUMERIC
00243          MOVE 'X'                TO ERROR-SWITCH
00244          GO TO 0210-TRANS-PF-EXIT.
00245
00246      IF PFKEYI LESS THAN 1
00247        OR
00248         PFKEYI GREATER THAN 24
00249          MOVE 'X'                TO ERROR-SWITCH
00250          GO TO 0210-TRANS-PF-EXIT.
00251
00252      MOVE PFKEYI                 TO CHECK-PFKEYS.
00253      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
00254
00255  0210-TRANS-PF-EXIT.
00256      EXIT.
00257      EJECT
00258  2000-BROWSE-FORWARD.
00259
00260      IF FORMNOI NOT = SAVE-CONTROL
00261         MOVE SPACE               TO EOF-SWITCH
00262         MOVE ZEROS               TO LINE-SEQUENCE
00263         MOVE FORMNOI             TO FORM-NO
00264         MOVE PI-COMPANY-CD       TO COMPANY-CODE
00265         MOVE ELFORM-KEY          TO SAVE-KEY
00266         GO TO 2010-SKIP-ADD.
00267
00268      IF BOTTOM-OF-FILE
00269          MOVE ER-0130            TO EMI-ERROR
00270          PERFORM 9900-ERROR-FORMAT
00271              THRU 9900-ERROR-FORMAT-EXIT
00272         GO TO 8110-SEND-DATA.
00273
00274      MOVE SPACE                  TO EOF-SWITCH.
00275
00276      IF EIBAID = DFHPF1
00277         ADD 15                   TO SAVE-LINE
00278      ELSE
00279         ADD 5                    TO SAVE-LINE.
00280
00281      MOVE SAVE-KEY               TO ELFORM-KEY.
00282
00283  2010-SKIP-ADD.
00284      MOVE 1                      TO COUNT-1
00285      MOVE SPACES TO ERROR-SWITCH FORMATTED-LINE.
00286
00287      PERFORM 4000-START-BROWSE THRU 4010-START-BROWSE-EXIT.
00288
00289      IF BAD-BROWSE
00290         MOVE ER-0126             TO EMI-ERROR
00291         PERFORM 9900-ERROR-FORMAT
00292            THRU 9900-ERROR-FORMAT-EXIT
00293         MOVE 'Y'                 TO EOF-SWITCH
00294         PERFORM 3100-FILL-SCREEN THRU 3110-FILL-SCREEN-EXIT
00295                 VARYING COUNT-1 FROM 1 BY 1 UNTIL COUNT-1 > 15
00296         GO TO 8110-SEND-DATA.
00297
00298      PERFORM 4020-READ-FILE THRU 4040-READ-FILE-EXIT.
00299
00300      IF END-OF-FILE
00301         GO TO 2020-END-FILE.
00302
00303      MOVE ELFORM-KEY TO SAVE-KEY.
00304
00305      IF TX-FORM-NO NOT = FORMNOI
00306          MOVE TX-FORM-NO TO FORMNOO.
00307
00308      PERFORM 3000-BUILD-SCREEN THRU 3010-BUILD-SCREEN-EXIT
00309          VARYING COUNT-1 FROM 1 BY 1 UNTIL COUNT-1 > 15
00310                  OR END-OF-FILE.
00311
00312  2020-END-FILE.
00313
00314      IF END-OF-FILE
00315          PERFORM 3100-FILL-SCREEN THRU 3110-FILL-SCREEN-EXIT
00316            VARYING COUNT-1 FROM COUNT-1 BY 1 UNTIL COUNT-1 > 15.
00317
00318      PERFORM 4050-END-BROWSE THRU 4060-END-BROWSE-EXIT.
00319      GO TO 8110-SEND-DATA.
00320
00321      EJECT
00322  2100-BROWSE-BACKWARD.
00323
00324      IF FORMNOI NOT = SAVE-CONTROL
00325         GO TO 2000-BROWSE-FORWARD.
00326
00327      IF TOP-OF-FILE
00328          MOVE ER-0131            TO EMI-ERROR
00329          PERFORM 9900-ERROR-FORMAT
00330              THRU 9900-ERROR-FORMAT-EXIT
00331          GO TO 8110-SEND-DATA.
00332
00333      MOVE SPACE TO EOF-SWITCH.
00334
00335      IF SAVE-LINE = 1
00336          GO TO 2500-NEW-SCREEN.
00337
00338      IF EIBAID = DFHPF2
00339         SUBTRACT 15              FROM SAVE-LINE
00340      ELSE
00341         SUBTRACT 5               FROM SAVE-LINE.
00342
00343      IF SAVE-LINE LESS THAN ZEROES
00344          MOVE ZEROES             TO SAVE-LINE.
00345
00346      MOVE SAVE-KEY               TO ELFORM-KEY.
00347      GO TO 2010-SKIP-ADD.
00348
00349      EJECT
00350  2500-NEW-SCREEN.
00351      MOVE SAVE-KEY               TO ELFORM-KEY.
00352      PERFORM 4000-START-BROWSE THRU 4010-START-BROWSE-EXIT.
00353      PERFORM 4070-READ-PREV THRU 4090-READ-PREV-EXIT.
00354      PERFORM 4070-READ-PREV THRU 4090-READ-PREV-EXIT.
00355
00356      IF END-OF-FILE
00357         MOVE LOW-VALUES          TO FORMNOI
00358      ELSE
00359         MOVE TX-FORM-NO          TO FORMNOI.
00360
00361      PERFORM 4050-END-BROWSE  THRU 4060-END-BROWSE-EXIT.
00362      GO TO 2000-BROWSE-FORWARD.
00363
00364      EJECT
00365  3000-BUILD-SCREEN.
00366      MOVE TX-TEXT-LINE           TO HOLD-LINE.
00367      MOVE FORMATTED-LINE         TO INFOO (COUNT-1).
00368      PERFORM 4020-READ-FILE THRU 4040-READ-FILE-EXIT.
00369
00370      IF TX-FORM-NO NOT = SAVE-CONTROL
00371          MOVE 'E'                TO ERROR-SWITCH.
00372
00373  3010-BUILD-SCREEN-EXIT.
00374      EXIT.
00375
00376      EJECT
00377  3100-FILL-SCREEN.
00378      MOVE SPACES                 TO FORMATTED-LINE.
00379      MOVE FORMATTED-LINE         TO INFOO (COUNT-1).
00380
00381  3110-FILL-SCREEN-EXIT.
00382      EXIT.
00383
00384      EJECT
00385  4000-START-BROWSE.
00386
00387      
      * EXEC CICS HANDLE CONDITION
00388 *        NOTOPEN  (5000-FORM-NOT-OPEN)
00389 *        NOTFND   (4005-KEY-NOT-FOUND)
00390 *    END-EXEC.
      *    MOVE '"$JI                  ! # #00001249' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031323439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00391
00392      
      * EXEC CICS STARTBR
00393 *        DATASET  ('ELFORM')
00394 *        RIDFLD   (ELFORM-KEY)
00395 *    END-EXEC.
           MOVE 'ELFORM' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001254' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELFORM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00396
00397      GO TO 4010-START-BROWSE-EXIT.
00398
00399  4005-KEY-NOT-FOUND.
00400      MOVE 'Z'                    TO EOF-SWITCH.
00401
00402  4010-START-BROWSE-EXIT.
00403      EXIT.
00404
00405  4020-READ-FILE.
00406
00407      
      * EXEC CICS HANDLE CONDITION
00408 *        NOTFND   (4030-END-OF-FILE)
00409 *        ENDFILE  (4030-END-OF-FILE)
00410 *    END-EXEC.
      *    MOVE '"$I''                  ! $ #00001269' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031323639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00411
00412      
      * EXEC CICS READNEXT
00413 *        SET      (ADDRESS OF TEXT-FILES)
00414 *        DATASET  ('ELFORM')
00415 *        RIDFLD   (ELFORM-KEY)
00416 *    END-EXEC.
           MOVE 'ELFORM' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001274' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELFORM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00417
00418      IF PI-COMPANY-CD = TX-COMPANY-CD
00419         GO TO 4040-READ-FILE-EXIT.
00420
00421  4030-END-OF-FILE.
00422      MOVE 'E'                    TO ERROR-SWITCH.
00423      MOVE 'Y'                    TO EOF-SWITCH
00424      MOVE ER-0130                TO EMI-ERROR.
00425      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.
00426      GO TO 8110-SEND-DATA.
00427
00428  4040-READ-FILE-EXIT.
00429      EXIT.
00430
00431  4050-END-BROWSE.
00432
00433      
      * EXEC CICS ENDBR
00434 *        DATASET ('ELFORM')
00435 *    END-EXEC.
           MOVE 'ELFORM' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001295' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00436
00437  4060-END-BROWSE-EXIT.
00438      EXIT.
00439
00440  4070-READ-PREV.
00441
00442      
      * EXEC CICS HANDLE CONDITION
00443 *        NOTFND   (4080-END-OF-FILE)
00444 *        ENDFILE  (4080-END-OF-FILE)
00445 *    END-EXEC.
      *    MOVE '"$I''                  ! % #00001304' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303031333034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00446
00447      
      * EXEC CICS READPREV
00448 *        SET      (ADDRESS OF TEXT-FILES)
00449 *        DATASET  ('ELFORM')
00450 *        RIDFLD   (ELFORM-KEY)
00451 *    END-EXEC.
           MOVE 'ELFORM' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00001309' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELFORM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00452
00453      CONTINUE.
00454
00455      IF PI-COMPANY-CD = TX-COMPANY-CD
00456         GO TO 4090-READ-PREV-EXIT.
00457
00458  4080-END-OF-FILE.
00459      MOVE 'X'                    TO EOF-SWITCH
00460      MOVE 'E'                    TO ERROR-SWITCH.
00461      MOVE ER-0131                TO EMI-ERROR.
00462      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.
00463      GO TO 8110-SEND-DATA.
00464
00465  4090-READ-PREV-EXIT.
00466      EXIT.
00467
00468      EJECT
00469  5000-FORM-NOT-OPEN.
00470      MOVE ER-0015                TO EMI-ERROR.
00471      PERFORM 9900-ERROR-FORMAT THRU 9900-ERROR-FORMAT-EXIT.
00472      GO TO 8110-SEND-DATA.
00473
00474      EJECT
00475  8100-SEND-MAP.
00476      PERFORM 8120-FORMAT-TIME-DATE
00477          THRU 8130-FORMAT-TIME-DATE-EXIT.
00478
00479      IF PFKEYL NOT = -1
00480         MOVE -1                  TO FORMNOL.
00481
00482      
      * EXEC CICS SEND
00483 *        MAP     ('EL155A')
00484 *        MAPSET  ('EL155S')
00485 *        ERASE
00486 *        CURSOR
00487 *        FREEKB
00488 *    END-EXEC.
           MOVE 'EL155A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL155S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00001344' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL155AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00489
00490      GO TO 9000-RETURN-TRANS.
00491
00492  8110-SEND-DATA.
00493
00494      IF EIBTRNID NOT = TRANS-ID
00495          GO TO 8100-SEND-MAP.
00496
00497      PERFORM 8120-FORMAT-TIME-DATE
00498          THRU 8130-FORMAT-TIME-DATE-EXIT.
00499
00500      IF PFKEYL NOT = -1
00501         MOVE -1                  TO FORMNOL.
00502
00503      
      * EXEC CICS SEND
00504 *        MAP     ('EL155A')
00505 *        MAPSET  ('EL155S')
00506 *        DATAONLY
00507 *        CURSOR
00508 *        FREEKB
00509 *    END-EXEC.
           MOVE 'EL155A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL155S' TO DFHEIV2
      *    MOVE '8$D    CT    F  H     ,   #00001365' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL155AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00510
00511      GO TO 9000-RETURN-TRANS.
00512
00513  8120-FORMAT-TIME-DATE.
00514      MOVE SAVE-DATE              TO DATEO.
00515
00516      
      * EXEC CICS ASKTIME
00517 *        ABSTIME(LCP-CICS-TIME)
00518 *    END-EXEC.
      *    MOVE '0"A                   "   #00001378' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00519
00520      
      * EXEC CICS FORMATTIME
00521 *        ABSTIME(LCP-CICS-TIME)
00522 *        TIME(LCP-TIME-OF-DAY-XX)
00523 *    END-EXEC.
      *    MOVE 'j$(     (             #   #00001382' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00524
00525      MOVE LCP-TIME-OF-DAY-68     TO TIME-UNFORMATTED.
00526      MOVE UN-HOURS               TO FOR-HOURS.
00527      MOVE UN-MINUTES             TO FOR-MINUTES.
00528      MOVE TIME-FORMATTED         TO TIMEO.
00529      MOVE '155A'                 TO PI-CURRENT-SCREEN-NO.
00530      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
00531      MOVE EMI-MESSAGE-AREA (1)   TO MSGO.
00532
00533  8130-FORMAT-TIME-DATE-EXIT.
00534      EXIT.
00535
00536  8200-RETURN-PRIOR.
00537      MOVE PI-RETURN-TO-PROGRAM   TO CALL-PGM.
00538      GO TO 9200-XCTL.
00539
00540  8300-GET-HELP.
00541      MOVE XCTL-EL010             TO CALL-PGM.
00542      GO TO 9200-XCTL.
00543
00544  8400-RETURN-MASTER.
00545      MOVE XCTL-EL126             TO CALL-PGM.
00546      GO TO 9200-XCTL.
00547
00548  8800-UNAUTHORIZED-ACCESS.
00549      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
00550      GO TO 8990-SEND-TEXT.
00551
00552  8810-PF23-ENTERED.
00553      MOVE EIBAID                 TO PI-ENTRY-CD-1.
00554      MOVE XCTL-EL005             TO CALL-PGM.
00555      GO TO 9200-XCTL.
00556
00557  8820-PGMID-ERROR.
00558
00559      
      * EXEC CICS HANDLE CONDITION
00560 *        PGMIDERR (8990-SEND-TEXT)
00561 *    END-EXEC.
      *    MOVE '"$L                   ! & #00001421' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303031343231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00562
00563      MOVE SPACE                  TO PI-ENTRY-CD-1.
00564      MOVE CALL-PGM               TO PI-CALLING-PROGRAM LOGOFF-PGM
00565      MOVE XCTL-EL005             TO CALL-PGM.
00566      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
00567      GO TO 9200-XCTL.
00568
00569  8990-SEND-TEXT.
00570
00571      
      * EXEC CICS SEND TEXT
00572 *        FROM    (LOGOFF-TEXT)
00573 *        LENGTH  (LOGOFF-LENGTH)
00574 *        ERASE
00575 *        FREEKB
00576 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001433' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343333' TO DFHEIV0(25:11)
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
           
00577
00578      GO TO 9100-RETURN-CICS.
00579
00580      EJECT
00581  9000-RETURN-TRANS.
00582
00583      
      * EXEC CICS RETURN
00584 *        TRANSID   (TRANS-ID)
00585 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00586 *        LENGTH    (PI-COMM-LENGTH)
00587 *    END-EXEC.
      *    MOVE '.(CT                  &   #00001445' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00588
00589      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL155' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00590
00591  9100-RETURN-CICS.
00592
00593      
      * EXEC CICS RETURN
00594 *    END-EXEC.
      *    MOVE '.(                    &   #00001455' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00595
00596      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL155' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00597
00598  9200-XCTL.
00599
00600      
      * EXEC CICS XCTL
00601 *        PROGRAM   (CALL-PGM)
00602 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00603 *        LENGTH    (PI-COMM-LENGTH)
00604 *    END-EXEC.
      *    MOVE '.$C                   $   #00001462' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00605
00606  9700-LINK-DATE-CONVERT.
00607
00608      
      * EXEC CICS LINK
00609 *        PROGRAM    ('ELDATCV')
00610 *        COMMAREA   (DATE-CONVERSION-DATA)
00611 *        LENGTH     (DC-COMM-LENGTH)
00612 *        END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00001470' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00613
00614  9700-EXIT.
00615      EXIT.
00616
00617  9900-ERROR-FORMAT.
00618
00619      IF NOT EMI-ERRORS-COMPLETE
00620          
      * EXEC CICS LINK
00621 *            PROGRAM   ('EL001')
00622 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
00623 *            LENGTH    (EMI-COMM-LENGTH)
00624 *        END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00001482' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00625
00626  9900-ERROR-FORMAT-EXIT.
00627      EXIT.
00628
00629  9990-ABEND.
00630      MOVE DFHEIBLK TO EMI-LINE1.
00631
00632      
      * EXEC CICS LINK
00633 *        PROGRAM   ('EL004')
00634 *        COMMAREA  (EMI-LINE1)
00635 *        LENGTH    (72)
00636 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00001494' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00637
00638      GO TO 8110-SEND-DATA.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL155' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8820-PGMID-ERROR,
                     8100-SEND-MAP,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 5000-FORM-NOT-OPEN,
                     4005-KEY-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 4030-END-OF-FILE,
                     4030-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 4080-END-OF-FILE,
                     4080-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8990-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL155' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
