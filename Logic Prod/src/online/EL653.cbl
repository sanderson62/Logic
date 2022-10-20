00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL653 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:14:25.
00007 *                            VMOD 2.007
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
00024 *
00025 *REMARKS.
00026 *        TRANSACTION - EXD5 - COMMISSION MASTER MAINT.
00024 *
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL653AI FILLER
101101******************************************************************
00027
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  EJECT
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*    EL653 WORKING STORAGE     *'.
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.007 *********'.
00035
00036  01  WS-DATE-AREA.
00037      12  SAVE-DATE           PIC  X(8)       VALUE SPACES.
00038      12  SAVE-BIN-DATE       PIC  XX         VALUE SPACES.
00039
00040  01  STANDARD-AREAS.
00041      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.
00042      12  MAP-NAME            PIC  X(8)       VALUE 'EL653A'.
00043      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL653S'.
00044      12  TRANS-ID            PIC  X(4)       VALUE 'EXD5'.
00045      12  THIS-PGM            PIC  X(8)       VALUE 'EL653'.
00046      12  PGM-NAME            PIC  X(8).
00047      12  WS-COMP-CD-R.
00048          16  FILLER          PIC  X.
00049          16  WS-COMP-CD-X    PIC  X.
00050      12  WS-COMP-CD  REDEFINES
00051          WS-COMP-CD-R        PIC S9(4)                  COMP.
00052      12  TIME-IN             PIC S9(7).
00053      12  TIME-OUT-R  REDEFINES  TIME-IN.
00054          16  FILLER          PIC  X.
00055          16  TIME-OUT        PIC  99V99.
00056          16  FILLER          PIC  XX.
00057      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
00058      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
00059      12  XCTL-626            PIC  X(8)       VALUE 'EL126'.
00060      12  LINK-001            PIC  X(8)       VALUE 'EL001'.
00061      12  LINK-004            PIC  X(8)       VALUE 'EL004'.
00062      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.
00063      12  FILE-ID             PIC  X(8)       VALUE  SPACES.
00064      12  CTBL-FILE-ID        PIC  X(8)       VALUE 'ERCTBL'.
00065      12  CNTL-FILE-ID        PIC  X(8)       VALUE 'ELCNTL'.
00066
00067  01  MISC-WORK-AREAS.
00068      12  WS-PHONE-IN         PIC  9(10).
00069      12  WS-PHONE-IN-R  REDEFINES  WS-PHONE-IN.
00070          16  WSPI-AREA       PIC  X(3).
00071          16  WSPI-PFX        PIC  X(3).
00072          16  WSPI-SFX        PIC  X(4).
00073      12  WS-PHONE-OUT.
00074          16  WSPO-AREA       PIC  X(3).
00075          16  FILLER          PIC  X           VALUE '-'.
00076          16  WSPO-PFX        PIC  X(3).
00077          16  FILLER          PIC  X           VALUE '-'.
00078          16  WSPO-SFX        PIC  X(4).
00079      12  DEEDIT-FIELD        PIC  X(15).
00080      12  FILLER  REDEFINES  DEEDIT-FIELD.
00081          16  FILLER          PIC  X(8).
00082          16  DEEDIT-FIELD-X7 PIC  X(7).
00083      12  DEEDIT-FIELD-V0  REDEFINES
00084          DEEDIT-FIELD        PIC S9(15).
00085      12  DEEDIT-FIELD-V1  REDEFINES
00086          DEEDIT-FIELD        PIC S9(13)V99.
00087      12  DEEDIT-FIELD-V5  REDEFINES
00088          DEEDIT-FIELD        PIC S9(10)V9(5).
00089      12  SUB1                PIC S9(4)        VALUE +0   COMP.
00090      12  SUB2                PIC S9(4)        VALUE +0   COMP.
00091      12  SUB3                PIC S9(4)        VALUE +0   COMP.
00092      12  SC-ITEM             PIC S9(4)        VALUE +1   COMP.
00093      12  ERCTBL-LENGTH       PIC S9(4)        VALUE +340 COMP.
00094      12  SV-CLMTOL           PIC  9(3)V99   VALUE ZEROS.
00095      12  DATE-TEST-AREA      PIC  9(6).
00096      12  DATE-TEST-AREA-R  REDEFINES  DATE-TEST-AREA.
00097          16  DATE-TEST-MM    PIC  99.
00098          16  DATE-TEST-DD    PIC  99.
00099          16  DATE-TEST-YY    PIC  99.
00100      12  DIVIDE-RESULT       PIC  99.
00101      12  DIVIDE-REMAINDER    PIC  9.
00102      12  WS-ZERO             PIC  X           VALUE '0'.
00103      12  WS-ONE              PIC  X           VALUE '1'.
00104      12  WS-TWO              PIC  X           VALUE '2'.
00105      12  WS-MAX-AGE          PIC  99          VALUE 99.
00106      12  WS-MAX-BENEFIT      PIC S9(7)V99 VALUE +9999999.99.
00107      12  WS-BENEFIT          PIC S9(7)V99 VALUE +0.
00108      12  WS-PREV-TERM        PIC  9(3)        VALUE ZERO.
00109      12  WS-PREV-AGE         PIC  99          VALUE ZERO.
00110      12  WS-PREV-BENEFIT     PIC S9(7)V99 VALUE +0.
00111      12  WS-RATE             PIC SV9(5)   VALUE ZEROS.
00112      12  WS-SAVE-TABLE       PIC  X(3)        VALUE SPACES.
00113      12  BIN-CURRENT-SAVE    PIC  XX          VALUE SPACES.
00114      12  ERCTBL-KEY.
00115          16  CTBL-COMP-ID    PIC  X(3)        VALUE SPACES.
00116          16  CTBL-REC-TYPE   PIC  X           VALUE SPACES.
00117          16  CTBL-ACCESS     PIC  X(4)        VALUE SPACES.
00118          16  CTBL-SEQ-NO     PIC S9(4)        VALUE +0   COMP.
00119      12  ELCNTL-KEY.
00120          16  CNTL-COMP-ID    PIC  X(3)        VALUE SPACES.
00121          16  CNTL-REC-TYPE   PIC  X           VALUE SPACES.
00122          16  CNTL-ACCESS     PIC  X(4)        VALUE SPACES.
00123          16  CNTL-SEQ-NO     PIC S9(4)        VALUE +0   COMP.
00124  EJECT
00125  01  ERROR-NUMBERS.
00126      12  ER-0000             PIC  X(4)       VALUE '0000'.
00127      12  ER-0004             PIC  X(4)       VALUE '0004'.
00128      12  ER-0008             PIC  X(4)       VALUE '0008'.
00129      12  ER-0029             PIC  X(4)       VALUE '0029'.
00130      12  ER-0050             PIC  X(4)       VALUE '0050'.
00131      12  ER-0068             PIC  X(4)       VALUE '0068'.
00132      12  ER-0070             PIC  X(4)       VALUE '0070'.
00133      12  ER-0142             PIC  X(4)       VALUE '0142'.
00134      12  ER-0583             PIC  X(4)       VALUE '0583'.
00135      12  ER-0590             PIC  X(4)       VALUE '0590'.
00136      12  ER-0591             PIC  X(4)       VALUE '0591'.
00137      12  ER-2039             PIC  X(4)       VALUE '2039'.
00138      12  ER-2055             PIC  X(4)       VALUE '2055'.
00139      12  ER-2056             PIC  X(4)       VALUE '2056'.
00140      12  ER-2067             PIC  X(4)       VALUE '2067'.
00141      12  ER-2115             PIC  X(4)       VALUE '2115'.
00142      12  ER-2116             PIC  X(4)       VALUE '2116'.
00143      12  ER-2117             PIC  X(4)       VALUE '2117'.
00144      12  ER-2118             PIC  X(4)       VALUE '2118'.
00145      12  ER-2120             PIC  X(4)       VALUE '2120'.
00146      12  ER-2121             PIC  X(4)       VALUE '2121'.
00147      12  ER-2122             PIC  X(4)       VALUE '2122'.
00148      12  ER-2123             PIC  X(4)       VALUE '2123'.
00149      12  ER-2124             PIC  X(4)       VALUE '2124'.
00150      12  ER-2125             PIC  X(4)       VALUE '2125'.
00151      12  ER-2127             PIC  X(4)       VALUE '2127'.
00152      12  ER-2128             PIC  X(4)       VALUE '2128'.
00153      12  ER-2130             PIC  X(4)       VALUE '2130'.
00154      12  ER-2133             PIC  X(4)       VALUE '2133'.
00155      12  ER-2135             PIC  X(4)       VALUE '2135'.
00156  EJECT
00157 *                            COPY ELCSCTM.
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
00158  EJECT
00159 *                            COPY ELCSCRTY.
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
00160  EJECT
00161 *                            COPY ELCDATE.
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
00162  EJECT
00163 *                            COPY ELCLOGOF.
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
00164  EJECT
00165 *                            COPY ELCATTR.
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
00166  EJECT
00167 *                            COPY ELCEMIB.
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
00168  EJECT
00169 *                            COPY ELCINTF.
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
00170      12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.
00171          16  PI-CHECK-MAINT-TYPE     PIC  X.
00172              88  VALID-MAINT-TYPE            VALUE 'S' 'A'
00173                                                    'C' 'D'.
00174              88  ADD-FUNCTION                VALUE 'A'.
00175              88  SHOW-FUNCTION               VALUE 'S'.
00176              88  DELETE-FUNCTION             VALUE 'D'.
00177              88  CHANGE-FUNCTION             VALUE 'C'.
00178          16  PI-CHECK-TBLCODE.
00179              20  PI-TBLCODE-1ST-CHAR PIC  X.
00180                  88  1ST-CHAR-ALPHA          VALUE 'A' THRU 'Z'.
00181              20  PI-TBLCODE-REST     PIC  XX.
00182          16  PI-CHECK-COVERAGE       PIC  X.
00183          16  PI-BROWSE-SW            PIC  X.
00184              88  BROWSE-STARTED              VALUE 'Y'.
00185          16  PI-FIRST-TIME-SW        PIC  X.
00186              88  FIRST-TIME                  VALUE 'Y'.
00187          16  PI-ERCTBL-EOF-SW        PIC  X.
00188              88  ERCTBL-EOF                  VALUE 'Y'.
00189          16  PI-ERCTBL-KEY.
00190              20  PI-ERC-COMPANY-CD   PIC  X.
00191              20  PI-ERC-TABLE        PIC  X(3).
00192              20  PI-ERC-CNTRL2.
00193                  24  PI-ERC-BEN-TYPE
00194                                      PIC  X.
00195                  24  PI-ERC-BEN-CODE
00196                                      PIC  XX.
00197          16  PI-SAVE-ERCTBL-KEY      PIC  X(7).
00198          16  FILLER                  PIC  X(618).
00199  EJECT
00200 *                            COPY ELCJPFX.
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
00201                              PIC  X(750).
00202  EJECT
00203 *                            COPY ELCAID.
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
00204
00205  01  FILLER  REDEFINES  DFHAID.
00206      12  FILLER              PIC  X(8).
00207      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.
00208  EJECT
00209 *                            COPY EL653S.
       01  EL653AI.
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
           05  MAINTYPL PIC S9(0004) COMP.
           05  MAINTYPF PIC  X(0001).
           05  FILLER REDEFINES MAINTYPF.
               10  MAINTYPA PIC  X(0001).
           05  MAINTYPI PIC  X(0001).
      *    -------------------------------
           05  TBLCODEL PIC S9(0004) COMP.
           05  TBLCODEF PIC  X(0001).
           05  FILLER REDEFINES TBLCODEF.
               10  TBLCODEA PIC  X(0001).
           05  TBLCODEI PIC  X(0003).
      *    -------------------------------
           05  COVTYPEL PIC S9(0004) COMP.
           05  COVTYPEF PIC  X(0001).
           05  FILLER REDEFINES COVTYPEF.
               10  COVTYPEA PIC  X(0001).
           05  COVTYPEI PIC  X(0001).
      *    -------------------------------
           05  BENCODEL PIC S9(0004) COMP.
           05  BENCODEF PIC  X(0001).
           05  FILLER REDEFINES BENCODEF.
               10  BENCODEA PIC  X(0001).
           05  BENCODEI PIC  X(0002).
      *    -------------------------------
           05  TERM1L PIC S9(0004) COMP.
           05  TERM1F PIC  X(0001).
           05  FILLER REDEFINES TERM1F.
               10  TERM1A PIC  X(0001).
           05  TERM1I PIC  X(0003).
      *    -------------------------------
           05  TERM2L PIC S9(0004) COMP.
           05  TERM2F PIC  X(0001).
           05  FILLER REDEFINES TERM2F.
               10  TERM2A PIC  X(0001).
           05  TERM2I PIC  X(0003).
      *    -------------------------------
           05  TERM3L PIC S9(0004) COMP.
           05  TERM3F PIC  X(0001).
           05  FILLER REDEFINES TERM3F.
               10  TERM3A PIC  X(0001).
           05  TERM3I PIC  X(0003).
      *    -------------------------------
           05  BEN1L PIC S9(0004) COMP.
           05  BEN1F PIC  X(0001).
           05  FILLER REDEFINES BEN1F.
               10  BEN1A PIC  X(0001).
           05  BEN1I PIC  S9(9)V99.
      *    -------------------------------
           05  AGE10L PIC S9(0004) COMP.
           05  AGE10F PIC  X(0001).
           05  FILLER REDEFINES AGE10F.
               10  AGE10A PIC  X(0001).
           05  AGE10I PIC  X(0002).
      *    -------------------------------
           05  RATE1L PIC S9(0004) COMP.
           05  RATE1F PIC  X(0001).
           05  FILLER REDEFINES RATE1F.
               10  RATE1A PIC  X(0001).
           05  RATE1I PIC  X(0007).
      *    -------------------------------
           05  RATE2L PIC S9(0004) COMP.
           05  RATE2F PIC  X(0001).
           05  FILLER REDEFINES RATE2F.
               10  RATE2A PIC  X(0001).
           05  RATE2I PIC  X(0007).
      *    -------------------------------
           05  RATE3L PIC S9(0004) COMP.
           05  RATE3F PIC  X(0001).
           05  FILLER REDEFINES RATE3F.
               10  RATE3A PIC  X(0001).
           05  RATE3I PIC  X(0007).
      *    -------------------------------
           05  AGE11L PIC S9(0004) COMP.
           05  AGE11F PIC  X(0001).
           05  FILLER REDEFINES AGE11F.
               10  AGE11A PIC  X(0001).
           05  AGE11I PIC  X(0002).
      *    -------------------------------
           05  RATE4L PIC S9(0004) COMP.
           05  RATE4F PIC  X(0001).
           05  FILLER REDEFINES RATE4F.
               10  RATE4A PIC  X(0001).
           05  RATE4I PIC  X(0007).
      *    -------------------------------
           05  RATE5L PIC S9(0004) COMP.
           05  RATE5F PIC  X(0001).
           05  FILLER REDEFINES RATE5F.
               10  RATE5A PIC  X(0001).
           05  RATE5I PIC  X(0007).
      *    -------------------------------
           05  RATE6L PIC S9(0004) COMP.
           05  RATE6F PIC  X(0001).
           05  FILLER REDEFINES RATE6F.
               10  RATE6A PIC  X(0001).
           05  RATE6I PIC  X(0007).
      *    -------------------------------
           05  AGE12L PIC S9(0004) COMP.
           05  AGE12F PIC  X(0001).
           05  FILLER REDEFINES AGE12F.
               10  AGE12A PIC  X(0001).
           05  AGE12I PIC  X(0002).
      *    -------------------------------
           05  RATE7L PIC S9(0004) COMP.
           05  RATE7F PIC  X(0001).
           05  FILLER REDEFINES RATE7F.
               10  RATE7A PIC  X(0001).
           05  RATE7I PIC  X(0007).
      *    -------------------------------
           05  RATE8L PIC S9(0004) COMP.
           05  RATE8F PIC  X(0001).
           05  FILLER REDEFINES RATE8F.
               10  RATE8A PIC  X(0001).
           05  RATE8I PIC  X(0007).
      *    -------------------------------
           05  RATE9L PIC S9(0004) COMP.
           05  RATE9F PIC  X(0001).
           05  FILLER REDEFINES RATE9F.
               10  RATE9A PIC  X(0001).
           05  RATE9I PIC  X(0007).
      *    -------------------------------
           05  BEN2L PIC S9(0004) COMP.
           05  BEN2F PIC  X(0001).
           05  FILLER REDEFINES BEN2F.
               10  BEN2A PIC  X(0001).
           05  BEN2I PIC  S9(9)V99.
      *    -------------------------------
           05  AGE20L PIC S9(0004) COMP.
           05  AGE20F PIC  X(0001).
           05  FILLER REDEFINES AGE20F.
               10  AGE20A PIC  X(0001).
           05  AGE20I PIC  X(0002).
      *    -------------------------------
           05  RATE10L PIC S9(0004) COMP.
           05  RATE10F PIC  X(0001).
           05  FILLER REDEFINES RATE10F.
               10  RATE10A PIC  X(0001).
           05  RATE10I PIC  X(0007).
      *    -------------------------------
           05  RATE11L PIC S9(0004) COMP.
           05  RATE11F PIC  X(0001).
           05  FILLER REDEFINES RATE11F.
               10  RATE11A PIC  X(0001).
           05  RATE11I PIC  X(0007).
      *    -------------------------------
           05  RATE12L PIC S9(0004) COMP.
           05  RATE12F PIC  X(0001).
           05  FILLER REDEFINES RATE12F.
               10  RATE12A PIC  X(0001).
           05  RATE12I PIC  X(0007).
      *    -------------------------------
           05  AGE21L PIC S9(0004) COMP.
           05  AGE21F PIC  X(0001).
           05  FILLER REDEFINES AGE21F.
               10  AGE21A PIC  X(0001).
           05  AGE21I PIC  X(0002).
      *    -------------------------------
           05  RATE13L PIC S9(0004) COMP.
           05  RATE13F PIC  X(0001).
           05  FILLER REDEFINES RATE13F.
               10  RATE13A PIC  X(0001).
           05  RATE13I PIC  X(0007).
      *    -------------------------------
           05  RATE14L PIC S9(0004) COMP.
           05  RATE14F PIC  X(0001).
           05  FILLER REDEFINES RATE14F.
               10  RATE14A PIC  X(0001).
           05  RATE14I PIC  X(0007).
      *    -------------------------------
           05  RATE15L PIC S9(0004) COMP.
           05  RATE15F PIC  X(0001).
           05  FILLER REDEFINES RATE15F.
               10  RATE15A PIC  X(0001).
           05  RATE15I PIC  X(0007).
      *    -------------------------------
           05  AGE22L PIC S9(0004) COMP.
           05  AGE22F PIC  X(0001).
           05  FILLER REDEFINES AGE22F.
               10  AGE22A PIC  X(0001).
           05  AGE22I PIC  X(0002).
      *    -------------------------------
           05  RATE16L PIC S9(0004) COMP.
           05  RATE16F PIC  X(0001).
           05  FILLER REDEFINES RATE16F.
               10  RATE16A PIC  X(0001).
           05  RATE16I PIC  X(0007).
      *    -------------------------------
           05  RATE17L PIC S9(0004) COMP.
           05  RATE17F PIC  X(0001).
           05  FILLER REDEFINES RATE17F.
               10  RATE17A PIC  X(0001).
           05  RATE17I PIC  X(0007).
      *    -------------------------------
           05  RATE18L PIC S9(0004) COMP.
           05  RATE18F PIC  X(0001).
           05  FILLER REDEFINES RATE18F.
               10  RATE18A PIC  X(0001).
           05  RATE18I PIC  X(0007).
      *    -------------------------------
           05  BEN3L PIC S9(0004) COMP.
           05  BEN3F PIC  X(0001).
           05  FILLER REDEFINES BEN3F.
               10  BEN3A PIC  X(0001).
           05  BEN3I PIC  S9(9)V99.
      *    -------------------------------
           05  AGE30L PIC S9(0004) COMP.
           05  AGE30F PIC  X(0001).
           05  FILLER REDEFINES AGE30F.
               10  AGE30A PIC  X(0001).
           05  AGE30I PIC  X(0002).
      *    -------------------------------
           05  RATE19L PIC S9(0004) COMP.
           05  RATE19F PIC  X(0001).
           05  FILLER REDEFINES RATE19F.
               10  RATE19A PIC  X(0001).
           05  RATE19I PIC  X(0007).
      *    -------------------------------
           05  RATE20L PIC S9(0004) COMP.
           05  RATE20F PIC  X(0001).
           05  FILLER REDEFINES RATE20F.
               10  RATE20A PIC  X(0001).
           05  RATE20I PIC  X(0007).
      *    -------------------------------
           05  RATE21L PIC S9(0004) COMP.
           05  RATE21F PIC  X(0001).
           05  FILLER REDEFINES RATE21F.
               10  RATE21A PIC  X(0001).
           05  RATE21I PIC  X(0007).
      *    -------------------------------
           05  AGE31L PIC S9(0004) COMP.
           05  AGE31F PIC  X(0001).
           05  FILLER REDEFINES AGE31F.
               10  AGE31A PIC  X(0001).
           05  AGE31I PIC  X(0002).
      *    -------------------------------
           05  RATE22L PIC S9(0004) COMP.
           05  RATE22F PIC  X(0001).
           05  FILLER REDEFINES RATE22F.
               10  RATE22A PIC  X(0001).
           05  RATE22I PIC  X(0007).
      *    -------------------------------
           05  RATE23L PIC S9(0004) COMP.
           05  RATE23F PIC  X(0001).
           05  FILLER REDEFINES RATE23F.
               10  RATE23A PIC  X(0001).
           05  RATE23I PIC  X(0007).
      *    -------------------------------
           05  RATE24L PIC S9(0004) COMP.
           05  RATE24F PIC  X(0001).
           05  FILLER REDEFINES RATE24F.
               10  RATE24A PIC  X(0001).
           05  RATE24I PIC  X(0007).
      *    -------------------------------
           05  AGE32L PIC S9(0004) COMP.
           05  AGE32F PIC  X(0001).
           05  FILLER REDEFINES AGE32F.
               10  AGE32A PIC  X(0001).
           05  AGE32I PIC  X(0002).
      *    -------------------------------
           05  RATE25L PIC S9(0004) COMP.
           05  RATE25F PIC  X(0001).
           05  FILLER REDEFINES RATE25F.
               10  RATE25A PIC  X(0001).
           05  RATE25I PIC  X(0007).
      *    -------------------------------
           05  RATE26L PIC S9(0004) COMP.
           05  RATE26F PIC  X(0001).
           05  FILLER REDEFINES RATE26F.
               10  RATE26A PIC  X(0001).
           05  RATE26I PIC  X(0007).
      *    -------------------------------
           05  RATE27L PIC S9(0004) COMP.
           05  RATE27F PIC  X(0001).
           05  FILLER REDEFINES RATE27F.
               10  RATE27A PIC  X(0001).
           05  RATE27I PIC  X(0007).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0072).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0072).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  99.
       01  EL653AO REDEFINES EL653AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBLCODEO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENCODEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEN1O PIC  ZZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE1O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE2O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE3O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE4O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE5O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE6O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE7O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE8O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE9O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEN2O PIC  ZZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE20O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE10O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE11O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE12O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE21O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE13O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE14O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE15O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE22O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE16O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE17O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE18O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEN3O PIC  ZZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE30O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE19O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE20O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE21O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE31O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE22O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE23O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE24O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE32O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE25O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE26O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RATE27O PIC  .99999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
00210
00211  01  EL653AO-R  REDEFINES  EL653AI.
101101     12  FILLER                      PIC  X(81).
00213      12  SCREEN-TABLE    OCCURS 3 TIMES
00214                              INDEXED BY ST-INDX.
00215          16  CTBL-TBF-L              PIC S9(4)          COMP.
00216          16  CTBL-TBF-A              PIC  X.
00217          16  CTBL-TBF                PIC S9(11).
00218          16  CTBL-TBF-DISP  REDEFINES
00219              CTBL-TBF                PIC ZZZZ,ZZZ.99.
00220          16  CTBL-AGE-TABLE  OCCURS 3 TIMES
00221                                  INDEXED BY AT-INDX.
00222              20  CTBL-AGE-L          PIC S9(4)          COMP.
00223              20  CTBL-AGE-A          PIC  X.
00224              20  CTBL-AGE            PIC  99.
00225              20  CTBL-RATE-TABLE OCCURS 3 TIMES
00226                                      INDEXED BY RT-INDX.
00227                  24  CTBL-RATE-L     PIC S9(4)          COMP.
00228                  24  CTBL-RATE-A     PIC  X.
00229                  24  CTBL-RATE       PIC X(7).
00230                  24  CTBL-RATE-DISP  REDEFINES
00231                      CTBL-RATE       PIC .99999-.
00232      12  FILLER                      PIC  X(5).
00233  EJECT
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
00235
00236  01  DFHCOMMAREA             PIC  X(1024).
00237
00238  EJECT
00239 *                            COPY ERCCTBL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCTBL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION TABLE                        *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 200   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCTBL                   RKP=2,LEN=7     *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 *                                                                *
00021 ******************************************************************
00022
00023  01  COMM-TABLE-RECORD.
00024      12  CT-RECORD-ID                      PIC XX.
00025          88  VALID-CT-ID                      VALUE 'CT'.
00026
00027      12  CT-CONTROL-PRIMARY.
00028          16  CT-COMPANY-CD                 PIC X.
00029          16  CT-TABLE                      PIC XXX.
00030          16  CT-CNTRL-2.
00031              20  CT-BEN-TYPE               PIC X.
00032              20  CT-BEN-CODE               PIC XX.
00033
00034      12  CT-MAINT-INFORMATION.
00035          16  CT-LAST-MAINT-DT              PIC XX.
00036          16  CT-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00037          16  CT-LAST-MAINT-USER            PIC X(4).
00038          16  FILLER                        PIC X(31).
00039
00040      12  CT-LIMITS.
00041          16  CT-TBF OCCURS 3 TIMES         PIC S9(7)V99   COMP-3.
00042
00043          16  CT-AGE OCCURS 3 TIMES         PIC S99        COMP-3.
00044
00045          16  CT-TRM OCCURS 3 TIMES         PIC S999       COMP-3.
00046
00047      12  CT-RATES.
00048          16  CT-RTX          OCCURS 27 TIMES.
00049              20  CT-RT                     PIC SV9(5)     COMP-3.
00050              20  CT-RT-R   REDEFINES
00051                  CT-RT                     PIC XXX.
00052
00053      12  FILLER                            PIC  X(42).
00054
00055 ******************************************************************
00240  EJECT
00241 *                            COPY ELCCNTL.
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
00242  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA COMM-TABLE-RECORD
                                CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL653' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00244      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00245      MOVE '5'                    TO  DC-OPTION-CODE.
00246
00247      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
00248
00249      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00250      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00251      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00252
00253  1000-START.
00254      IF EIBCALEN = ZERO
00255          GO TO 8800-UNAUTHORIZED-ACCESS.
00256
00257      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00258          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00259              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00260              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00261              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00262              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00263              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00264              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00265              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00266              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00267          ELSE
00268              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00269              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00270              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00271              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00272              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00273              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00274              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00275              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00276
00277      
      * EXEC CICS HANDLE CONDITION
00278 *        NOTOPEN   (9990-ABEND)
00279 *        NOTFND    (8880-NOT-FOUND)
00280 *        PGMIDERR  (9600-PGMID-ERROR)
00281 *        ERROR     (9990-ABEND)
00282 *    END-EXEC.
      *    MOVE '"$JIL.                ! " #00003135' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033313335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00283
00284      IF EIBTRNID NOT = TRANS-ID
00285          MOVE 'Y'                TO  PI-FIRST-TIME-SW
00286          MOVE LOW-VALUES         TO  EL653AI
00287          GO TO 8100-SEND-INITIAL-MAP.
00288
00289      IF EIBAID = DFHCLEAR
00290          GO TO 9400-CLEAR.
00291
00292      IF PI-PROCESSOR-ID = 'LGXX'
00293          GO TO 2000-RECEIVE.
00294
00295      
      * EXEC CICS  READQ TS
00296 *        QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00297 *        INTO    (SECURITY-CONTROL)
00298 *        LENGTH  (SC-COMM-LENGTH)
00299 *        ITEM    (SC-ITEM)
00300 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00003153' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00301
00302      MOVE SC-CREDIT-DISPLAY (08)  TO  PI-DISPLAY-CAP.
00303      MOVE SC-CREDIT-UPDATE  (08)  TO  PI-MODIFY-CAP.
00304
00305      IF NOT DISPLAY-CAP
00306          MOVE 'READ'          TO SM-READ
00307          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00308          MOVE ER-0070        TO  EMI-ERROR
00309          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00310          GO TO 8100-SEND-INITIAL-MAP.
00311
00312  EJECT
00313  2000-RECEIVE.
00314      MOVE LOW-VALUES             TO  EL653AI.
00315
00316      IF EIBAID = DFHPA1 OR  DFHPA2  OR  DFHPA3
00317          MOVE ER-0008            TO  EMI-ERROR
00318          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00319          MOVE -1                 TO  MAINTYPL
00320          GO TO 8200-SEND-DATAONLY.
00321
00322      
      * EXEC CICS RECEIVE
00323 *        MAP     (MAP-NAME)
00324 *        MAPSET  (MAPSET-NAME)
00325 *        INTO    (EL653AI)
00326 *    END-EXEC.
           MOVE LENGTH OF
            EL653AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003180' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL653AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00327
00328      IF PFENTERL = ZERO
00329          GO TO 3000-CHECK-PFENTERS.
00330
00331      IF EIBAID NOT = DFHENTER
00332          MOVE ER-0004            TO  EMI-ERROR
00333          GO TO 3100-INPUT-ERROR.
00334
00335      IF PFENTERI GREATER 0 AND LESS 25
00336          MOVE PF-VALUES (PFENTERI)  TO  EIBAID
00337      ELSE
00338          MOVE ER-0029               TO  EMI-ERROR
00339          GO TO 3100-INPUT-ERROR.
00340  EJECT
00341  3000-CHECK-PFENTERS.
00342      IF EIBAID = DFHPF23
00343          GO TO 8810-PF23.
00344
00345      IF EIBAID = DFHPF24
00346          GO TO 9200-RETURN-MAIN-MENU.
00347
00348      IF EIBAID = DFHPF12
00349          GO TO 9500-PF12.
00350
00351      IF MAINTYPL GREATER  ZERO
00352          IF MAINTYPI NOT = SPACE
00353              IF EIBAID NOT = DFHENTER
00354                  MOVE ER-0050    TO  EMI-ERROR
00355                  GO TO 3100-INPUT-ERROR.
00356
00357      IF EIBAID = DFHPF1
00358          GO TO 7300-PAGE-TABLE-FORWARD.
00359
00360      IF EIBAID = DFHPF2
00361          GO TO 7400-PAGE-TABLE-BACKWARD.
00362
00363      IF EIBAID = DFHPF3
00364          GO TO 7500-PAGE-BENEFIT-FORWARD.
00365
00366      IF EIBAID = DFHPF4
00367          GO TO 7600-PAGE-BENEFIT-BACKWARD.
00368
00369      IF EIBAID = DFHENTER
00370          GO TO 4000-EDIT-MAINT.
00371
00372      MOVE ER-0029                TO  EMI-ERROR.
00373
00374  3100-INPUT-ERROR.
00375      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00376
00377      MOVE AL-UNBON               TO  PFENTERA.
00378      MOVE -1                     TO  PFENTERL.
00379
00380      GO TO 8200-SEND-DATAONLY.
00381  EJECT
00382  4000-EDIT-MAINT.
00383      IF MAINTYPL GREATER ZERO
00384          MOVE MAINTYPI           TO  PI-CHECK-MAINT-TYPE
00385          IF VALID-MAINT-TYPE
00386              MOVE AL-UANON       TO  MAINTYPA
00387          ELSE
00388              MOVE -1             TO  MAINTYPL
00389              MOVE AL-UABON       TO  MAINTYPA
00390              MOVE ER-2039        TO  EMI-ERROR
00391              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00392      ELSE
00393          MOVE -1                 TO  MAINTYPL
00394          MOVE AL-UABON           TO  MAINTYPA
00395          MOVE ER-2039            TO  EMI-ERROR
00396          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00397
00398      IF NOT MODIFY-CAP
00399          IF SHOW-FUNCTION
00400              NEXT SENTENCE
00401          ELSE
00402              MOVE 'UPDATE'       TO SM-READ
00403              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00404              MOVE ER-0070        TO  EMI-ERROR
00405              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00406              GO TO 8100-SEND-INITIAL-MAP.
00407
00408      IF TBLCODEL GREATER ZERO
00409          MOVE TBLCODEI           TO  PI-CHECK-TBLCODE
00410          IF 1ST-CHAR-ALPHA
00411              MOVE TBLCODEI       TO  PI-ERC-TABLE
00412              MOVE AL-UANON       TO  TBLCODEA
00413          ELSE
00414              MOVE -1             TO  TBLCODEL
00415              MOVE AL-UABON       TO  TBLCODEA
00416              MOVE ER-2115        TO  EMI-ERROR
00417              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00418      ELSE
00419          MOVE -1                 TO  TBLCODEL
00420          MOVE AL-UABON           TO  TBLCODEA
00421          MOVE ER-2116            TO  EMI-ERROR
00422          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00423
00424      IF COVTYPEL GREATER ZERO
00425          MOVE COVTYPEI           TO  PI-CHECK-COVERAGE
00426          IF PI-CHECK-COVERAGE = PI-LIFE-OVERRIDE-L1
00427            OR  PI-CHECK-COVERAGE = PI-AH-OVERRIDE-L1
00428              MOVE COVTYPEI       TO  PI-ERC-BEN-TYPE
00429              MOVE AL-UANON       TO  COVTYPEA
00430          ELSE
00431              MOVE -1             TO  COVTYPEL
00432              MOVE AL-UABON       TO  COVTYPEA
00433              MOVE ER-2117        TO  EMI-ERROR
00434              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00435      ELSE
00436          MOVE -1                 TO  COVTYPEL
00437          MOVE AL-UABON           TO  COVTYPEA
00438          MOVE ER-2117            TO  EMI-ERROR
00439          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00440
00441      IF BENCODEL GREATER ZERO
00442          MOVE BENCODEI           TO  PI-ERC-BEN-CODE
00443          MOVE AL-UANON           TO  BENCODEA
00444      ELSE
00445          MOVE -1                 TO  BENCODEL
00446          MOVE AL-UABON           TO  BENCODEA
00447          MOVE ER-2118            TO  EMI-ERROR
00448          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00449
00450      IF EMI-NO-ERRORS
00451          NEXT SENTENCE
00452      ELSE
00453          GO TO 8200-SEND-DATAONLY.
00454
00455      IF CHANGE-FUNCTION
00456          GO TO 4400-CHANGE.
00457
00458      IF DELETE-FUNCTION
00459          GO TO 4600-DELETE.
00460
00461      IF SHOW-FUNCTION
00462          GO TO 5000-BUILD-INITIAL-SCREEN.
00463
00464      IF ADD-FUNCTION
00465          GO TO 4200-ADD.
00466
00467  4000-EXIT.
00468      EXIT.
00469  EJECT
00470  4200-ADD.
00471      PERFORM 7000-EDIT  THRU  7049-EXIT.
00472
00473      IF EMI-NO-ERRORS
00474          NEXT SENTENCE
00475      ELSE
00476          GO TO 8200-SEND-DATAONLY.
00477
00478      
      * EXEC CICS HANDLE CONDITION
00479 *        NOTOPEN  (9990-ABEND)
00480 *        NOTFND   (4250-CONT)
00481 *        END-EXEC.
      *    MOVE '"$JI                  ! # #00003336' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033333336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00482
00483      PERFORM 7050-READ-ERCTBL  THRU  7050-EXIT.
00484
00485      MOVE ER-2133                TO  EMI-ERROR.
00486
00487      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00488
00489      MOVE -1                     TO  MAINTYPL.
00490
00491      GO TO 8200-SEND-DATAONLY.
00492
00493  4250-CONT.
00494      PERFORM 7150-ERCTBL-GETMAIN  THRU  7150-EXIT.
00495
00496      MOVE SPACES                 TO  COMM-TABLE-RECORD.
00497      MOVE 'CT'                   TO  CT-RECORD-ID.
00498      MOVE PI-COMPANY-CD          TO  CT-COMPANY-CD.
00499      MOVE ZEROS                  TO  CT-LAST-MAINT-HHMMSS.
00500      MOVE +99                    TO  CT-AGE (1)
00501                                      CT-AGE (2)
00502                                      CT-AGE (3).
00503      MOVE +999                   TO  CT-TRM (1)
00504                                      CT-TRM (2)
00505                                      CT-TRM (3).
00506      MOVE WS-MAX-BENEFIT         TO  CT-TBF (1)
00507                                      CT-TBF (2)
00508                                      CT-TBF (3).
00509      MOVE +1                     TO  SUB1.
00510
00511  4275-ZERO-RATES.
00512      IF SUB1 GREATER +27
00513          NEXT SENTENCE
00514      ELSE
00515          MOVE ZEROS              TO  CT-RT (SUB1)
00516          ADD +1                  TO  SUB1
00517          GO TO 4275-ZERO-RATES.
00518
00519      PERFORM 6000-CHECK-FOR-UPDATE  THRU  6099-EXIT.
00520
00521      MOVE PI-PROCESSOR-ID        TO  CT-LAST-MAINT-USER.
00522      MOVE EIBTIME                TO  CT-LAST-MAINT-HHMMSS.
00523      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00524      MOVE '5'                    TO  DC-OPTION-CODE.
00525      MOVE LINK-CLDATCV           TO  PGM-NAME.
00526
00527      
      * EXEC CICS LINK
00528 *        PROGRAM   (PGM-NAME)
00529 *        COMMAREA  (DATE-CONVERSION-DATA)
00530 *        LENGTH    (DC-COMM-LENGTH)
00531 *    END-EXEC.
      *    MOVE '."C                   ''   #00003385' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00532
00533      MOVE DC-BIN-DATE-1          TO  CT-LAST-MAINT-DT
00534                                      BIN-CURRENT-SAVE.
00535      MOVE PI-COMPANY-CD          TO  CT-COMPANY-CD.
00536      MOVE 'CT'                   TO  CT-RECORD-ID.
00537      MOVE CTBL-FILE-ID           TO  FILE-ID.
00538      MOVE 'A'                    TO  JP-RECORD-TYPE.
00539      MOVE COMM-TABLE-RECORD      TO  JP-RECORD-AREA.
00540
00541      
      * EXEC CICS WRITE
00542 *        DATASET  (CTBL-FILE-ID)
00543 *        FROM     (COMM-TABLE-RECORD)
00544 *        RIDFLD   (CT-CONTROL-PRIMARY)
00545 *    END-EXEC.
           MOVE LENGTH OF
            COMM-TABLE-RECORD
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003399' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CTBL-FILE-ID, 
                 COMM-TABLE-RECORD, 
                 DFHEIV11, 
                 CT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00546
00547      PERFORM 8400-LOG-JOURNAL-RECORD.
00548
00549      PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT.
00550
00551      MOVE ER-0000                TO  EMI-ERROR.
00552
00553      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00554
00555      MOVE LOW-VALUES             TO  EL653AO.
00556      MOVE PI-ERC-TABLE           TO  TBLCODEO.
00557      MOVE PI-ERC-BEN-TYPE        TO  COVTYPEO.
00558      MOVE PI-ERC-BEN-CODE        TO  BENCODEO.
00559      MOVE AL-UANON               TO  TBLCODEA
00560                                      COVTYPEA
00561                                      BENCODEA.
00562
00563      GO TO 8100-SEND-INITIAL-MAP.
00564
00565  4299-EXIT.
00566      EXIT.
00567  EJECT
00568  4400-CHANGE.
00569      IF PI-ERCTBL-KEY = PI-SAVE-ERCTBL-KEY
00570          NEXT SENTENCE
00571      ELSE
00572          MOVE ER-2056            TO  EMI-ERROR
00573          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00574          MOVE -1                 TO  MAINTYPL
00575          GO TO 8200-SEND-DATAONLY.
00576
00577      PERFORM 7000-EDIT  THRU  7049-EXIT.
00578
00579      IF EMI-NO-ERRORS
00580          NEXT SENTENCE
00581      ELSE
00582          GO TO 8200-SEND-DATAONLY.
00583
00584      PERFORM 7200-READ-ERCTBL-UPDATE  THRU  7200-EXIT.
00585
00586      MOVE COMM-TABLE-RECORD      TO  JP-RECORD-AREA.
00587
00588      PERFORM 6000-CHECK-FOR-UPDATE  THRU  6099-EXIT.
00589
00590      IF CT-LAST-MAINT-USER   = PI-UPDATE-BY   OR
00591         CT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00592          NEXT SENTENCE
00593      ELSE
00594          
      * EXEC CICS UNLOCK
00595 *             DATASET  (CTBL-FILE-ID)
00596 *        END-EXEC
      *    MOVE '&*                    #   #00003452' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CTBL-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00597          MOVE ER-0068            TO  EMI-ERROR
00598          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00599          GO TO 8200-SEND-DATAONLY.
00600
00601      MOVE PI-PROCESSOR-ID        TO  CT-LAST-MAINT-USER.
00602      MOVE EIBTIME                TO  CT-LAST-MAINT-HHMMSS.
00603      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00604      MOVE '5'                    TO  DC-OPTION-CODE.
00605      MOVE LINK-CLDATCV           TO  PGM-NAME.
00606
00607      
      * EXEC CICS LINK
00608 *        PROGRAM   (PGM-NAME)
00609 *        COMMAREA  (DATE-CONVERSION-DATA)
00610 *        LENGTH    (DC-COMM-LENGTH)
00611 *    END-EXEC.
      *    MOVE '."C                   ''   #00003465' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00612
00613      MOVE DC-BIN-DATE-1          TO  CT-LAST-MAINT-DT
00614                                      BIN-CURRENT-SAVE.
00615      MOVE 'B'                    TO  JP-RECORD-TYPE.
00616      MOVE CTBL-FILE-ID           TO  FILE-ID.
00617
00618      PERFORM 8400-LOG-JOURNAL-RECORD.
00619
00620      MOVE COMM-TABLE-RECORD      TO  JP-RECORD-AREA.
00621
00622      
      * EXEC CICS REWRITE
00623 *        DATASET  (CTBL-FILE-ID)
00624 *        FROM     (COMM-TABLE-RECORD)
00625 *    END-EXEC.
           MOVE LENGTH OF
            COMM-TABLE-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003480' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CTBL-FILE-ID, 
                 COMM-TABLE-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00626
00627      MOVE 'C'                    TO  JP-RECORD-TYPE.
00628      MOVE CTBL-FILE-ID           TO  FILE-ID.
00629
00630      PERFORM 8400-LOG-JOURNAL-RECORD.
00631
00632      PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT.
00633
00634      MOVE ER-0000                TO  EMI-ERROR.
00635
00636      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00637
00638      MOVE LOW-VALUES             TO  EL653AO.
00639      MOVE PI-ERC-TABLE           TO  TBLCODEO.
00640      MOVE PI-ERC-BEN-TYPE        TO  COVTYPEO.
00641      MOVE PI-ERC-BEN-CODE        TO  BENCODEO.
00642      MOVE AL-UANON               TO  TBLCODEA
00643                                      COVTYPEA
00644                                      BENCODEA.
00645
00646      GO TO 5000-BUILD-INITIAL-SCREEN.
00647
00648  4400-EXIT.
00649      EXIT.
00650  EJECT
00651  4600-DELETE.
00652      IF PI-ERCTBL-KEY = PI-SAVE-ERCTBL-KEY
00653          NEXT SENTENCE
00654      ELSE
00655          MOVE ER-2056            TO  EMI-ERROR
00656          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00657          MOVE -1                 TO  MAINTYPL
00658          GO TO 8200-SEND-DATAONLY.
00659
00660      PERFORM 7200-READ-ERCTBL-UPDATE  THRU  7200-EXIT.
00661
00662      MOVE COMM-TABLE-RECORD      TO  JP-RECORD-AREA.
00663
00664      IF CT-LAST-MAINT-USER   = PI-UPDATE-BY   OR
00665         CT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00666          NEXT SENTENCE
00667      ELSE
00668          
      * EXEC CICS UNLOCK
00669 *             DATASET  (CTBL-FILE-ID)
00670 *        END-EXEC
      *    MOVE '&*                    #   #00003526' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CTBL-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00671          MOVE ER-0068            TO  EMI-ERROR
00672          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00673          GO TO 8200-SEND-DATAONLY.
00674
00675      
      * EXEC CICS DELETE
00676 *         DATASET  (CTBL-FILE-ID)
00677 *    END-EXEC.
      *    MOVE '&(                    &   #00003533' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CTBL-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00678
00679      MOVE 'D'                    TO  JP-RECORD-TYPE.
00680      MOVE CTBL-FILE-ID           TO  FILE-ID.
00681
00682      PERFORM 8400-LOG-JOURNAL-RECORD.
00683
00684      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00685      MOVE '5'                    TO  DC-OPTION-CODE.
00686      MOVE LINK-CLDATCV           TO  PGM-NAME.
00687
00688      
      * EXEC CICS LINK
00689 *        PROGRAM   (PGM-NAME)
00690 *        COMMAREA  (DATE-CONVERSION-DATA)
00691 *        LENGTH    (DC-COMM-LENGTH)
00692 *    END-EXEC.
      *    MOVE '."C                   ''   #00003546' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00693
00694      MOVE DC-BIN-DATE-1          TO  BIN-CURRENT-SAVE.
00695
00696      PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT.
00697
00698      MOVE ER-0000                TO  EMI-ERROR.
00699
00700      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00701
00702      MOVE LOW-VALUES             TO  EL653AO.
00703      MOVE PI-ERC-TABLE           TO  TBLCODEO.
00704      MOVE PI-ERC-BEN-TYPE        TO  COVTYPEO.
00705      MOVE PI-ERC-BEN-CODE        TO  BENCODEO.
00706      MOVE AL-UANON               TO  TBLCODEA
00707                                      COVTYPEA
00708                                      BENCODEA.
00709
00710      GO TO 8100-SEND-INITIAL-MAP.
00711
00712  4600-EXIT.
00713      EXIT.
00714  EJECT
00715  5000-BUILD-INITIAL-SCREEN.
00716      MOVE LOW-VALUES             TO  EL653AO.
00717      MOVE PI-ERC-TABLE           TO  TBLCODEO.
00718      MOVE PI-ERC-BEN-TYPE        TO  COVTYPEO.
00719      MOVE PI-ERC-BEN-CODE        TO  BENCODEO.
00720      MOVE AL-UANON               TO  MAINTYPA
00721                                      TBLCODEA
00722                                      COVTYPEA
00723                                      BENCODEA.
00724      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
00725
00726      PERFORM 7050-READ-ERCTBL  THRU  7050-EXIT.
00727
00728      MOVE CT-LAST-MAINT-USER     TO  PI-UPDATE-BY.
00729      MOVE CT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
00730
00731  5005-SET-UP-SCREEN.
00732      MOVE CT-TRM (1)             TO  TERM1O.
00733      MOVE CT-TRM (2)             TO  TERM2O.
00734      MOVE CT-TRM (3)             TO  TERM3O.
00735      MOVE AL-UNNON               TO  TERM1A
00736                                      TERM2A
00737                                      TERM3A.
00738
00739      SET ST-INDX                 TO  +1.
00740      SET AT-INDX                 TO  +1.
00741      SET RT-INDX                 TO  +1.
00742
00743      MOVE +1                     TO  SUB1
00744                                      SUB2
00745                                      SUB3.
00746
00747  5010-SET-UP-BENEFIT.
00748      IF SUB1 GREATER +3
00749          MOVE -1                 TO  MAINTYPL
00750          GO TO 8100-SEND-INITIAL-MAP.
00751
00752      MOVE CT-TBF (SUB1)          TO  CTBL-TBF-DISP (ST-INDX)
00753      MOVE AL-UNNON               TO  CTBL-TBF-A (ST-INDX).
00754
00755  5020-SET-UP-AGES.
00756      IF SUB2 GREATER +3
00757          SET ST-INDX  UP  BY  +1
00758          SET AT-INDX             TO  +1
00759          ADD  +1                 TO  SUB1
00760          MOVE +1                 TO  SUB2
00761          GO TO 5010-SET-UP-BENEFIT.
00762
00763      MOVE CT-AGE (SUB2)          TO  CTBL-AGE (ST-INDX AT-INDX)
00764
00765      IF SUB1 = +1
00766          MOVE AL-UNNON       TO  CTBL-AGE-A (ST-INDX AT-INDX)
00767      ELSE
00768          MOVE AL-SANOF       TO  CTBL-AGE-A (ST-INDX AT-INDX).
00769
00770  5030-SET-UP-RATES.
00771      IF RT-INDX GREATER +3
00772          ADD +1                  TO  SUB2
00773          SET AT-INDX  UP  BY  +1
00774          SET RT-INDX             TO  +1
00775          GO TO 5020-SET-UP-AGES.
00776
00777      IF CT-RT (SUB3) = ZEROS
00778          NEXT SENTENCE
00779      ELSE
00780          MOVE CT-RT (SUB3)
00781              TO  CTBL-RATE-DISP (ST-INDX AT-INDX RT-INDX)
00782          MOVE AL-UNNON
00783              TO  CTBL-RATE-A (ST-INDX AT-INDX RT-INDX).
00784
00785      ADD +1                      TO  SUB3.
00786
00787      SET RT-INDX  UP  BY  +1.
00788
00789      GO TO 5030-SET-UP-RATES.
00790
00791  5099-EXIT.
00792      EXIT.
00793  EJECT
00794  6000-CHECK-FOR-UPDATE.
00795       IF CHANGE-FUNCTION
00796           GO TO 6010-CONT.
00797
00798       IF TBLCODEL GREATER ZERO
00799           MOVE TBLCODEI          TO  CT-TABLE.
00800
00801       IF COVTYPEL GREATER ZERO
00802           MOVE COVTYPEI          TO  CT-BEN-TYPE.
00803
00804       IF BENCODEL GREATER ZERO
00805           MOVE BENCODEI          TO  CT-BEN-CODE.
00806
00807  6010-CONT.
00808      IF TERM1L GREATER ZERO
00809          MOVE TERM1I             TO  CT-TRM (1).
00810
00811      IF TERM2L GREATER ZERO
00812          MOVE TERM2I             TO  CT-TRM (2).
00813
00814      IF TERM3L GREATER ZERO
00815          MOVE TERM3I             TO  CT-TRM (3).
00816
00817      SET ST-INDX                 TO  +1.
00818      SET AT-INDX                 TO  +1.
00819      SET RT-INDX                 TO  +1.
00820
00821      MOVE +1                     TO  SUB1
00822                                      SUB2
00823                                      SUB3.
00824
00825  6010-PROCESS.
00826      IF ST-INDX GREATER +3
00827          GO TO 6099-EXIT.
00828
00829      IF CTBL-TBF-L (ST-INDX) GREATER ZERO
00830          MOVE CTBL-TBF (ST-INDX)  TO  DEEDIT-FIELD
00831          PERFORM 7100-DEEDIT  THRU  7100-EXIT
00832          MOVE DEEDIT-FIELD-V1     TO  CT-TBF (SUB1).
00833
00834  6020-UPDATE-AGES.
00835      IF AT-INDX GREATER +3
00836          SET ST-INDX  UP  BY  +1
00837          SET AT-INDX             TO  +1
00838          ADD +1                  TO  SUB1
00839          MOVE +1                 TO  SUB2
00840          GO TO 6010-PROCESS.
00841
00842      IF CTBL-AGE-L (ST-INDX AT-INDX) GREATER ZERO
00843          MOVE CTBL-AGE (ST-INDX AT-INDX)  TO  CT-AGE (SUB2).
00844
00845  6030-UPDATE-RATES.
00846      IF RT-INDX GREATER +3
00847          SET AT-INDX  UP  BY  +1
00848          ADD +1                  TO  SUB2
00849          SET RT-INDX             TO  +1
00850          GO TO 6020-UPDATE-AGES.
00851
00852      IF CTBL-RATE-L (ST-INDX AT-INDX RT-INDX) GREATER ZERO
00853          MOVE SPACES             TO  DEEDIT-FIELD
00854          MOVE CTBL-RATE (ST-INDX AT-INDX RT-INDX)
00855              TO  DEEDIT-FIELD-X7
00856          PERFORM 7100-DEEDIT  THRU  7100-EXIT
00857          MOVE DEEDIT-FIELD-V5    TO  CT-RT (SUB3).
00858
00859      SET RT-INDX  UP  BY  +1.
00860
00861      ADD +1                      TO  SUB3.
00862
00863      GO TO 6030-UPDATE-RATES.
00864
00865  6099-EXIT.
00866      EXIT.
00867  EJECT
00868  7000-EDIT.
00869      IF TERM1L GREATER ZERO
00870          IF TERM1I NUMERIC
00871              IF TERM1I GREATER WS-PREV-TERM  OR
00872                 TERM1I = 000 OR  999
00873                  MOVE TERM1I     TO  WS-PREV-TERM
00874                  MOVE AL-UNNON   TO  TERM1A
00875              ELSE
00876                  MOVE -1         TO  TERM1L
00877                  MOVE AL-UNBON   TO  TERM1A
00878                  MOVE ER-2123    TO  EMI-ERROR
00879                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00880          ELSE
00881              MOVE -1             TO  TERM1L
00882              MOVE AL-UNBON       TO  TERM1A
00883              MOVE ER-2120        TO  EMI-ERROR
00884              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00885
00886      IF TERM2L GREATER ZERO
00887          IF TERM2I NUMERIC
00888              IF TERM2I GREATER WS-PREV-TERM  OR
00889                 TERM2I = 999
00890                  MOVE TERM2I     TO  WS-PREV-TERM
00891                  MOVE AL-UNNON   TO  TERM2A
00892              ELSE
00893                  MOVE -1         TO  TERM2L
00894                  MOVE AL-UNBON   TO  TERM2A
00895                  MOVE ER-2123    TO  EMI-ERROR
00896                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00897          ELSE
00898              MOVE -1             TO  TERM2L
00899              MOVE AL-UNBON       TO  TERM2A
00900              MOVE ER-2121        TO  EMI-ERROR
00901              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00902
00903      IF TERM3L GREATER ZERO
00904          IF TERM3I NUMERIC
00905              IF TERM3I GREATER WS-PREV-TERM  OR
00906                 TERM3I = 999
00907                  MOVE TERM3I     TO  WS-PREV-TERM
00908                  MOVE AL-UNNON   TO  TERM3A
00909              ELSE
00910                  MOVE -1         TO  TERM3L
00911                  MOVE AL-UNBON   TO  TERM3A
00912                  MOVE ER-2123    TO  EMI-ERROR
00913                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00914          ELSE
00915              MOVE -1             TO  TERM3L
00916              MOVE AL-UNBON       TO  TERM3A
00917              MOVE ER-2122        TO  EMI-ERROR
00918              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00919
00920      SET ST-INDX                 TO  +1.
00921      SET AT-INDX                 TO  +1.
00922      SET RT-INDX                 TO  +1.
00923
00924  7010-PROCESS.
00925      IF ST-INDX GREATER +3
00926          GO TO 7049-EXIT.
00927
00928      IF CTBL-TBF-L (ST-INDX) GREATER ZERO
00929          MOVE CTBL-TBF (ST-INDX)  TO  DEEDIT-FIELD
00930          IF CTBL-TBF (ST-INDX) NUMERIC
00931              IF CTBL-TBF (ST-INDX) = ZEROS
00932                  MOVE AL-UNBON    TO  CTBL-TBF-A (ST-INDX)
00933                  MOVE -1          TO  CTBL-TBF-L (ST-INDX)
00934                  MOVE ER-2124     TO  EMI-ERROR
00935                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00936              END-IF
00938          END-IF
00940      ELSE
00941          GO TO 7020-EDIT-AGE.
00942
00943      PERFORM 7100-DEEDIT  THRU  7100-EXIT.
00944
00945      IF DEEDIT-FIELD-V1 GREATER ZERO
00946          MOVE DEEDIT-FIELD-V1    TO  WS-BENEFIT
CIDMOD         IF WS-BENEFIT = +0999999.99
CIDMOD             MOVE +9999999.99    TO WS-BENEFIT
CIDMOD         END-IF
00947          IF WS-BENEFIT GREATER WS-PREV-BENEFIT OR
00948             WS-BENEFIT = WS-MAX-BENEFIT
00949              MOVE AL-UNNON       TO  CTBL-TBF-A (ST-INDX)
00950              MOVE WS-BENEFIT     TO  CTBL-TBF-DISP (ST-INDX)
00951                                      WS-PREV-BENEFIT
00952          ELSE
00953              MOVE WS-BENEFIT     TO  CTBL-TBF-DISP (ST-INDX)
00954              MOVE AL-UNBON       TO  CTBL-TBF-A (ST-INDX)
00955              MOVE -1             TO  CTBL-TBF-L (ST-INDX)
00956              MOVE ER-2125        TO  EMI-ERROR
00957              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
CIDMOD         END-IF
00958      ELSE
00959          MOVE AL-UNBON           TO  CTBL-TBF-A (ST-INDX)
00960          MOVE -1                 TO  CTBL-TBF-L (ST-INDX)
00961          MOVE ER-2124            TO  EMI-ERROR
00962          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00963
00964  7020-EDIT-AGE.
00965      IF AT-INDX GREATER +3
00966          SET ST-INDX  UP  BY  +1
00967          SET AT-INDX      TO  +1
00968          GO TO 7010-PROCESS.
00969
00970      IF CTBL-AGE-L (ST-INDX AT-INDX) GREATER ZERO
00971          NEXT SENTENCE
00972      ELSE
00973          GO TO 7030-EDIT-RATE.
00974
00975      IF CTBL-AGE (ST-INDX AT-INDX) NUMERIC
00976          IF CTBL-AGE (ST-INDX AT-INDX) GREATER WS-PREV-AGE  OR
00977             CTBL-AGE (ST-INDX AT-INDX) = WS-MAX-AGE
00978              MOVE AL-UNNON
00979                  TO  CTBL-AGE-A (ST-INDX AT-INDX)
00980              MOVE CTBL-AGE (ST-INDX AT-INDX)
00981                  TO  WS-PREV-AGE
00982          ELSE
00983              MOVE -1        TO  CTBL-AGE-L (ST-INDX AT-INDX)
00984              MOVE AL-UNBON  TO  CTBL-AGE-A (ST-INDX AT-INDX)
00985              MOVE ER-2127   TO  EMI-ERROR
00986              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00987      ELSE
00988          MOVE -1            TO  CTBL-AGE-L (ST-INDX AT-INDX)
00989          MOVE AL-UNBON      TO  CTBL-AGE-A (ST-INDX AT-INDX)
00990          MOVE ER-2128       TO  EMI-ERROR
00991          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00992
00993  7030-EDIT-RATE.
00994      IF RT-INDX GREATER +3
00995          SET AT-INDX  UP  BY  +1
00996          SET RT-INDX      TO  +1
00997          GO TO 7020-EDIT-AGE.
00998
00999      IF CTBL-RATE-L (ST-INDX AT-INDX RT-INDX) GREATER ZERO
01000          MOVE SPACES             TO  DEEDIT-FIELD
01001          MOVE CTBL-RATE (ST-INDX AT-INDX RT-INDX)
01002              TO  DEEDIT-FIELD-X7
01003          PERFORM 7100-DEEDIT  THRU  7100-EXIT
01004          MOVE DEEDIT-FIELD-V5    TO  WS-RATE
01005          IF WS-RATE  NUMERIC
01006              MOVE AL-UNNON
01007                  TO  CTBL-RATE-A (ST-INDX AT-INDX RT-INDX)
01008              MOVE WS-RATE
01009                  TO  CTBL-RATE-DISP (ST-INDX AT-INDX RT-INDX)
01010          ELSE
01011              MOVE AL-UNBON
01012                  TO  CTBL-RATE-A (ST-INDX AT-INDX RT-INDX)
01013              MOVE -1
01014                  TO  CTBL-RATE-L (ST-INDX AT-INDX RT-INDX)
01015              MOVE WS-RATE
01016                  TO  CTBL-RATE-DISP (ST-INDX AT-INDX RT-INDX)
01017              MOVE ER-2130   TO EMI-ERROR
01018              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01019
01020      SET RT-INDX  UP  BY  +1.
01021
01022      GO TO 7030-EDIT-RATE.
01023
01024  7049-EXIT.
01025      EXIT.
01026  EJECT
01027  7050-READ-ERCTBL.
01028      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
01029
01030      
      * EXEC CICS READ
01031 *         DATASET  (CTBL-FILE-ID)
01032 *         SET      (ADDRESS OF COMM-TABLE-RECORD)
01033 *         RIDFLD   (PI-ERCTBL-KEY)
01034 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003890' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CTBL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCTBL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-TABLE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01035
01036      MOVE PI-ERCTBL-KEY          TO  PI-SAVE-ERCTBL-KEY.
01037
01038  7050-EXIT.
01039      EXIT.
01040  EJECT
01041  7100-DEEDIT.
01042      
      * EXEC CICS BIF
01043 *        DEEDIT
01044 *        FIELD   (DEEDIT-FIELD)
01045 *        LENGTH  (15)
01046 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003902' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01047
01048  7100-EXIT.
01049      EXIT.
01050  EJECT
01051  7150-ERCTBL-GETMAIN.
01052      
      * EXEC CICS GETMAIN
01053 *        SET      (ADDRESS OF COMM-TABLE-RECORD)
01054 *        LENGTH   (ERCTBL-LENGTH)
01055 *        INITIMG  (GETMAIN-SPACE)
01056 *    END-EXEC.
      *    MOVE ',"IL                  $   #00003912' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCTBL-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMM-TABLE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01057
01058  7150-EXIT.
01059      EXIT.
01060  EJECT
01061  7200-READ-ERCTBL-UPDATE.
01062      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
01063
01064      
      * EXEC CICS READ
01065 *        DATASET  (CTBL-FILE-ID)
01066 *        SET      (ADDRESS OF COMM-TABLE-RECORD)
01067 *        RIDFLD   (PI-ERCTBL-KEY)
01068 *        UPDATE
01069 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00003924' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CTBL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCTBL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-TABLE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01070
01071  7200-EXIT.
01072      EXIT.
01073  EJECT
01074  7250-READ-ERCTBL-GTEQ.
01075      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
01076
01077      
      * EXEC CICS READ
01078 *        DATASET  (CTBL-FILE-ID)
01079 *        SET      (ADDRESS OF COMM-TABLE-RECORD)
01080 *        RIDFLD   (PI-ERCTBL-KEY)
01081 *        GTEQ
01082 *    END-EXEC.
      *    MOVE '&"S        G          (   #00003937' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CTBL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCTBL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-TABLE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01083
01084  7250-EXIT.
01085      EXIT.
01086  EJECT
01087  7300-PAGE-TABLE-FORWARD.
01088      MOVE SPACES                 TO  PI-ERCTBL-EOF-SW.
01089
01090      IF TBLCODEL GREATER ZERO
01091          MOVE TBLCODEI           TO  PI-ERC-TABLE
01092                                      WS-SAVE-TABLE
01093          MOVE LOW-VALUES         TO  PI-ERC-BEN-TYPE
01094                                      PI-ERC-BEN-CODE
01095      ELSE
01096          MOVE 'Y'                TO  PI-FIRST-TIME-SW
01097          MOVE LOW-VALUES         TO  PI-ERCTBL-KEY
01098                                      WS-SAVE-TABLE.
01099
01100      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
01101
01102      PERFORM 7700-START-BROWSE  THRU  7700-EXIT.
01103
01104  7310-READNEXT.
01105      
      * EXEC CICS HANDLE CONDITION
01106 *        ENDFILE  (7350-ENDFILE)
01107 *        NOTFND   (7375-NOTFOUND)
01108 *    END-EXEC.
      *    MOVE '"$''I                  ! $ #00003965' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033393635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01109
01110      PERFORM 7800-READNEXT  THRU  7800-EXIT.
01111
01112      IF ERCTBL-EOF
01113          IF FIRST-TIME
01114              MOVE LOW-VALUES     TO  EL653AO
01115              MOVE ER-0583        TO  EMI-ERROR
01116              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01117              MOVE -1             TO  MAINTYPL
01118              GO TO 8100-SEND-INITIAL-MAP
01119          ELSE
01120              IF BROWSE-STARTED
01121                  PERFORM 7950-END-BROWSE  THRU  7950-EXIT
01122                  MOVE LOW-VALUES  TO  EL653AO
01123                  GO TO 7300-PAGE-TABLE-FORWARD.
01124
01125      MOVE SPACE                  TO  PI-FIRST-TIME-SW.
01126
01127      IF WS-SAVE-TABLE = PI-ERC-TABLE
01128          GO TO 7310-READNEXT.
01129
01130      MOVE CT-LAST-MAINT-USER     TO  PI-UPDATE-BY.
01131      MOVE CT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
01132      MOVE PI-ERCTBL-KEY          TO  PI-SAVE-ERCTBL-KEY.
01133      MOVE LOW-VALUES             TO  EL653AO.
01134      MOVE CT-TABLE               TO  TBLCODEO.
01135      MOVE CT-BEN-TYPE            TO  COVTYPEO.
01136      MOVE CT-BEN-CODE            TO  BENCODEO.
01137      MOVE AL-UANON               TO  TBLCODEA
01138                                      COVTYPEA
01139                                      BENCODEA
01140                                      MAINTYPA.
01141
01142      GO TO 5005-SET-UP-SCREEN.
01143
01144  7350-ENDFILE.
01145      IF FIRST-TIME
01146          MOVE LOW-VALUES         TO  EL653AO
01147          MOVE ER-0583            TO  EMI-ERROR
01148          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01149          MOVE -1                 TO  MAINTYPL
01150          GO TO 8100-SEND-INITIAL-MAP.
01151
01152      IF BROWSE-STARTED
01153          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
01154
01155      MOVE ER-2067                TO  EMI-ERROR
01156
01157      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01158
01159      MOVE LOW-VALUES             TO  EL653AO
01160
01161      GO TO 7300-PAGE-TABLE-FORWARD.
01162
01163  7375-NOTFOUND.
01164      IF BROWSE-STARTED
01165          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
01166
01167      GO TO 8880-NOT-FOUND.
01168
01169  7399-EXIT.
01170      EXIT.
01171  EJECT
01172  7400-PAGE-TABLE-BACKWARD.
01173      MOVE SPACES                 TO  PI-ERCTBL-EOF-SW.
01174
01175      IF TBLCODEL GREATER ZERO
01176          MOVE TBLCODEI           TO  PI-ERC-TABLE
01177                                      WS-SAVE-TABLE
01178          MOVE LOW-VALUES         TO  PI-ERC-BEN-TYPE
01179                                      PI-ERC-BEN-CODE
01180      ELSE
01181          GO TO 7450-ENDFILE.
01182
01183      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
01184
01185      PERFORM 7700-START-BROWSE  THRU  7700-EXIT.
01186
01187      PERFORM 7800-READNEXT      THRU  7800-EXIT.
01188
01189  7410-READPREV.
01190      
      * EXEC CICS HANDLE CONDITION
01191 *        ENDFILE  (7450-ENDFILE)
01192 *        NOTFND   (7475-NOTFOUND)
01193 *    END-EXEC.
      *    MOVE '"$''I                  ! % #00004050' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034303530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01194
01195      PERFORM 7900-READPREV  THRU  7900-EXIT.
01196
01197      IF ERCTBL-EOF
01198          IF BROWSE-STARTED
01199              PERFORM 7950-END-BROWSE  THRU  7950-EXIT
01200              MOVE LOW-VALUES     TO  PI-ERCTBL-KEY
01201              GO TO 7300-PAGE-TABLE-FORWARD.
01202
01203      IF WS-SAVE-TABLE = PI-ERC-TABLE
01204          GO TO 7410-READPREV
01205      ELSE
01206          MOVE LOW-VALUES         TO  PI-ERC-BEN-TYPE
01207                                      PI-ERC-BEN-CODE
01208          PERFORM 7250-READ-ERCTBL-GTEQ  THRU  7250-EXIT.
01209
01210      IF ERCTBL-EOF
01211          IF BROWSE-STARTED
01212              PERFORM 7950-END-BROWSE  THRU  7950-EXIT
01213              MOVE LOW-VALUES     TO  PI-ERCTBL-KEY
01214              GO TO 7300-PAGE-TABLE-FORWARD.
01215
01216      MOVE CT-LAST-MAINT-USER     TO  PI-UPDATE-BY.
01217      MOVE CT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
01218      MOVE PI-ERCTBL-KEY          TO  PI-SAVE-ERCTBL-KEY.
01219      MOVE LOW-VALUES             TO  EL653AO.
01220      MOVE CT-TABLE               TO  TBLCODEO.
01221      MOVE CT-BEN-TYPE            TO  COVTYPEO.
01222      MOVE CT-BEN-CODE            TO  BENCODEO.
01223      MOVE AL-UANON               TO  TBLCODEA
01224                                      COVTYPEA
01225                                      BENCODEA
01226                                      MAINTYPA.
01227
01228      GO TO 5005-SET-UP-SCREEN.
01229
01230  7450-ENDFILE.
01231      IF BROWSE-STARTED
01232          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
01233
01234      MOVE ER-2135                TO  EMI-ERROR
01235
01236      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01237
01238      MOVE LOW-VALUES             TO  EL653AO
01239
01240      GO TO 7300-PAGE-TABLE-FORWARD.
01241
01242  7475-NOTFOUND.
01243      IF BROWSE-STARTED
01244          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
01245
01246      GO TO 8880-NOT-FOUND.
01247
01248  7499-EXIT.
01249      EXIT.
01250  EJECT
01251  7500-PAGE-BENEFIT-FORWARD.
01252      MOVE SPACES                 TO  PI-ERCTBL-EOF-SW.
01253
01254      IF TBLCODEL GREATER ZERO
01255          MOVE TBLCODEI           TO  PI-ERC-TABLE
01256                                      WS-SAVE-TABLE
01257          MOVE COVTYPEI           TO  PI-ERC-BEN-TYPE
01258          MOVE BENCODEI           TO  PI-ERC-BEN-CODE
01259      ELSE
01260          MOVE 'Y'                TO  PI-FIRST-TIME-SW
01261          MOVE LOW-VALUES         TO  PI-ERCTBL-KEY
01262                                      WS-SAVE-TABLE.
01263
01264      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
01265
01266      PERFORM 7700-START-BROWSE  THRU  7700-EXIT.
01267
01268      
      * EXEC CICS HANDLE CONDITION
01269 *        ENDFILE  (7550-ENDFILE)
01270 *        NOTFND   (7575-NOTFOUND)
01271 *    END-EXEC.
      *    MOVE '"$''I                  ! & #00004128' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034313238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01272
01273      PERFORM 7800-READNEXT  THRU  7800-EXIT.
01274
01275      IF ERCTBL-EOF
01276          IF FIRST-TIME
01277              MOVE LOW-VALUES     TO  EL653AO
01278              MOVE ER-0583        TO  EMI-ERROR
01279              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01280              MOVE -1             TO  MAINTYPL
01281              GO TO 8100-SEND-INITIAL-MAP
01282          ELSE
01283              IF BROWSE-STARTED
01284                  PERFORM 7950-END-BROWSE  THRU  7950-EXIT
01285                  GO TO 7500-PAGE-BENEFIT-FORWARD.
01286
01287      IF NOT FIRST-TIME
01288          PERFORM 7800-READNEXT  THRU  7800-EXIT.
01289
01290      IF ERCTBL-EOF
01291          PERFORM 7950-END-BROWSE  THRU  7950-EXIT
01292          MOVE LOW-VALUES         TO  PI-ERCTBL-KEY
01293          MOVE ER-0590            TO  EMI-ERROR
01294          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01295          MOVE 'Y'                TO  PI-FIRST-TIME-SW
01296          GO TO 7500-PAGE-BENEFIT-FORWARD.
01297
01298      IF PI-ERC-TABLE = WS-SAVE-TABLE
01299          NEXT SENTENCE
01300      ELSE
01301          IF FIRST-TIME
01302              NEXT SENTENCE
01303          ELSE
01304              MOVE LOW-VALUES     TO  PI-ERCTBL-KEY
01305              MOVE ER-0590        TO  EMI-ERROR
01306              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01307              MOVE 'Y'            TO  PI-FIRST-TIME-SW
01308              PERFORM 7950-END-BROWSE  THRU  7950-EXIT
01309              GO TO 7500-PAGE-BENEFIT-FORWARD.
01310
01311      MOVE SPACES                 TO  PI-FIRST-TIME-SW.
01312      MOVE CT-LAST-MAINT-USER     TO  PI-UPDATE-BY.
01313      MOVE CT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
01314      MOVE PI-ERCTBL-KEY          TO  PI-SAVE-ERCTBL-KEY.
01315      MOVE LOW-VALUES             TO  EL653AO.
01316      MOVE CT-TABLE               TO  TBLCODEO.
01317      MOVE CT-BEN-TYPE            TO  COVTYPEO.
01318      MOVE CT-BEN-CODE            TO  BENCODEO.
01319      MOVE AL-UANON               TO  TBLCODEA
01320                                      COVTYPEA
01321                                      BENCODEA
01322                                      MAINTYPA.
01323
01324      GO TO 5005-SET-UP-SCREEN.
01325
01326  7550-ENDFILE.
01327      IF BROWSE-STARTED
01328          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
01329
01330      MOVE LOW-VALUES             TO  PI-ERCTBL-KEY.
01331      MOVE ER-0590                TO  EMI-ERROR.
01332
01333      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01334
01335      MOVE 'Y'                    TO  PI-FIRST-TIME-SW.
01336
01337      GO TO 7500-PAGE-BENEFIT-FORWARD.
01338
01339  7575-NOTFOUND.
01340      IF BROWSE-STARTED
01341          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
01342
01343      GO TO 8880-NOT-FOUND.
01344
01345  7599-EXIT.
01346      EXIT.
01347  EJECT
01348  7600-PAGE-BENEFIT-BACKWARD.
01349      MOVE SPACES                 TO  PI-ERCTBL-EOF-SW.
01350
01351      IF TBLCODEL GREATER ZERO
01352          MOVE TBLCODEI           TO  PI-ERC-TABLE
01353                                      WS-SAVE-TABLE
01354          MOVE COVTYPEI           TO  PI-ERC-BEN-TYPE
01355          MOVE BENCODEI           TO  PI-ERC-BEN-CODE
01356      ELSE
01357          GO TO 7650-ENDFILE.
01358
01359      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
01360
01361      PERFORM 7700-START-BROWSE  THRU  7700-EXIT.
01362
01363      
      * EXEC CICS HANDLE CONDITION
01364 *        ENDFILE  (7650-ENDFILE)
01365 *        NOTFND   (7675-NOTFOUND)
01366 *    END-EXEC.
      *    MOVE '"$''I                  ! '' #00004223' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034323233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01367
01368      PERFORM 7900-READPREV  THRU  7900-EXIT.
01369
01370      IF ERCTBL-EOF
01371          IF BROWSE-STARTED
01372              PERFORM 7950-END-BROWSE  THRU  7950-EXIT
01373              GO TO 7500-PAGE-BENEFIT-FORWARD.
01374
01375      IF FIRST-TIME
01376          MOVE SPACES             TO  PI-FIRST-TIME-SW
01377      ELSE
01378          PERFORM 7900-READPREV  THRU  7900-EXIT.
01379
01380      IF ERCTBL-EOF
01381          IF BROWSE-STARTED
01382              PERFORM 7950-END-BROWSE  THRU  7950-EXIT
01383              GO TO 7500-PAGE-BENEFIT-FORWARD.
01384
01385      IF PI-ERC-TABLE = WS-SAVE-TABLE
01386          NEXT SENTENCE
01387      ELSE
01388          MOVE LOW-VALUES         TO  PI-ERCTBL-KEY
01389          MOVE ER-0591            TO  EMI-ERROR
01390          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01391          MOVE 'Y'                TO  PI-FIRST-TIME-SW
01392          PERFORM 7950-END-BROWSE  THRU  7950-EXIT
01393          GO TO 7500-PAGE-BENEFIT-FORWARD.
01394
01395      MOVE CT-LAST-MAINT-USER     TO  PI-UPDATE-BY.
01396      MOVE CT-LAST-MAINT-USER     TO  PI-UPDATE-BY.
01397      MOVE CT-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
01398      MOVE PI-ERCTBL-KEY          TO  PI-SAVE-ERCTBL-KEY.
01399      MOVE LOW-VALUES             TO  EL653AO.
01400      MOVE CT-TABLE               TO  TBLCODEO.
01401      MOVE CT-BEN-TYPE            TO  COVTYPEO.
01402      MOVE CT-BEN-CODE            TO  BENCODEO.
01403      MOVE AL-UANON               TO  TBLCODEA
01404                                      COVTYPEA
01405                                      BENCODEA
01406                                      MAINTYPA.
01407
01408      GO TO 5005-SET-UP-SCREEN.
01409
01410  7650-ENDFILE.
01411      IF BROWSE-STARTED
01412          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
01413
01414      MOVE ER-0591                TO  EMI-ERROR.
01415
01416      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01417
01418      MOVE LOW-VALUES             TO  EL653AO.
01419
01420      GO TO 7300-PAGE-TABLE-FORWARD.
01421
01422  7675-NOTFOUND.
01423      IF BROWSE-STARTED
01424          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
01425
01426      GO TO 8880-NOT-FOUND.
01427
01428  7699-EXIT.
01429      EXIT.
01430  EJECT
01431  7700-START-BROWSE.
01432      
      * EXEC CICS STARTBR
01433 *        DATASET  (CTBL-FILE-ID)
01434 *        RIDFLD   (PI-ERCTBL-KEY)
01435 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004292' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CTBL-FILE-ID, 
                 PI-ERCTBL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01436
01437      MOVE 'Y'                    TO  PI-BROWSE-SW.
01438
01439  7700-EXIT.
01440      EXIT.
01441  EJECT
01442  7800-READNEXT.
01443      
      * EXEC CICS READNEXT
01444 *        DATASET  (CTBL-FILE-ID)
01445 *        SET      (ADDRESS OF COMM-TABLE-RECORD)
01446 *        RIDFLD   (PI-ERCTBL-KEY)
01447 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004303' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CTBL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCTBL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-TABLE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01448
01449      IF PI-COMPANY-CD NOT = CT-COMPANY-CD
01450          MOVE LOW-VALUES         TO  EL653AO
01451          MOVE 'Y'                TO  PI-ERCTBL-EOF-SW
01452          IF FIRST-TIME
01453              MOVE ER-0583        TO  EMI-ERROR
01454          ELSE
01455              MOVE ER-2067        TO  EMI-ERROR
01456      ELSE
01457          GO TO 7800-EXIT.
01458
01459      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01460
01461  7800-EXIT.
01462      EXIT.
01463  EJECT
01464  7900-READPREV.
01465      
      * EXEC CICS READPREV
01466 *        DATASET  (CTBL-FILE-ID)
01467 *        SET      (ADDRESS OF COMM-TABLE-RECORD)
01468 *        RIDFLD   (PI-ERCTBL-KEY)
01469 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004325' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CTBL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCTBL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-TABLE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01470
01471      IF PI-COMPANY-CD NOT = CT-COMPANY-CD
01472          MOVE ER-2067            TO EMI-ERROR
01473          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01474          MOVE LOW-VALUES         TO  EL653AO
01475          MOVE 'Y'                TO  PI-ERCTBL-EOF-SW.
01476
01477  7900-EXIT.
01478      EXIT.
01479  EJECT
01480  7950-END-BROWSE.
01481      
      * EXEC CICS ENDBR
01482 *        DATASET  (CTBL-FILE-ID)
01483 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004341' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CTBL-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01484
01485      MOVE SPACE          TO PI-BROWSE-SW.
01486
01487  7950-EXIT.
01488      EXIT.
01489  EJECT
01490  8000-UPDATE-MAINT-DATE.
01491      MOVE SPACES                 TO  ELCNTL-KEY.
01492      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
01493      MOVE '1'                    TO  CNTL-REC-TYPE.
01494      MOVE +0                     TO  CNTL-SEQ-NO.
01495
01496      
      * EXEC CICS HANDLE CONDITION
01497 *        NOTFND  (8000-EXIT)
01498 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00004356' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034333536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01499
01500      
      * EXEC CICS READ
01501 *        UPDATE
01502 *        DATASET  (CNTL-FILE-ID)
01503 *        SET      (ADDRESS OF CONTROL-FILE)
01504 *        RIDFLD   (ELCNTL-KEY)
01505 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004360' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333630' TO DFHEIV0(25:11)
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01506
01507      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01508      MOVE 'B'                    TO  JP-RECORD-TYPE.
01509      MOVE CNTL-FILE-ID           TO  FILE-ID.
01510
01511      PERFORM 8400-LOG-JOURNAL-RECORD.
01512
01513      MOVE BIN-CURRENT-SAVE       TO  CF-COMMISSION-TAB-MAINT-DT.
01514      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01515      MOVE 'C'                    TO  JP-RECORD-TYPE.
01516      MOVE CNTL-FILE-ID           TO  FILE-ID.
01517
01518      
      * EXEC CICS REWRITE
01519 *        DATASET  (CNTL-FILE-ID)
01520 *        FROM     (CONTROL-FILE)
01521 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004378' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01522
01523      PERFORM 8400-LOG-JOURNAL-RECORD.
01524
01525  8000-EXIT.
01526       EXIT.
01527  EJECT
01528  8100-SEND-INITIAL-MAP.
01529      MOVE SAVE-DATE              TO  RUNDATEO.
01530      MOVE EIBTIME                TO  TIME-IN.
01531      MOVE TIME-OUT               TO  RUNTIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01532      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
01533      MOVE -1                     TO  MAINTYPL.
01534
01535      
      * EXEC CICS SEND
01536 *        MAP     (MAP-NAME)
01537 *        MAPSET  (MAPSET-NAME)
01538 *        FROM    (EL653AO)
01539 *        ERASE
01540 *        CURSOR
01541 *    END-EXEC.
           MOVE LENGTH OF
            EL653AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004397' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL653AO, 
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
           
01542
01543      GO TO 9100-RETURN-TRAN.
01544
01545  8200-SEND-DATAONLY.
01546      MOVE SAVE-DATE              TO  RUNDATEO.
01547      MOVE EIBTIME                TO  TIME-IN.
01548      MOVE TIME-OUT               TO  RUNTIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01549      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
01550
01551      
      * EXEC CICS SEND
01552 *        MAP     (MAP-NAME)
01553 *        MAPSET  (MAPSET-NAME)
01554 *        FROM    (EL653AO)
01555 *        DATAONLY
01556 *        CURSOR
01557 *    END-EXEC.
           MOVE LENGTH OF
            EL653AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004415' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL653AO, 
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
           
01558
01559      GO TO 9100-RETURN-TRAN.
01560
01561  8300-SEND-TEXT.
01562      
      * EXEC CICS SEND TEXT
01563 *        FROM    (LOGOFF-TEXT)
01564 *        LENGTH  (LOGOFF-LENGTH)
01565 *        ERASE
01566 *        FREEKB
01567 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004426' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343236' TO DFHEIV0(25:11)
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
           
01568
01569      
      * EXEC CICS RETURN
01570 *    END-EXEC.
      *    MOVE '.(                    &   #00004433' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01571
01572  8400-LOG-JOURNAL-RECORD.
01573      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
01574      MOVE FILE-ID                TO  JP-FILE-ID.
01575      MOVE THIS-PGM               TO  JP-PROGRAM-ID.
01576
pemuni*    EXEC CICS JOURNAL
pemuni*        JFILEID  (PI-JOURNAL-FILE-ID)
pemuni*        JTYPEID  ('EL')
pemuni*        FROM     (JOURNAL-RECORD)
pemuni*        LENGTH   (223)
pemuni*    END-EXEC.
01583
01584  8800-UNAUTHORIZED-ACCESS.
01585      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
01586
01587      GO TO 8300-SEND-TEXT.
01588
01589  8810-PF23.
01590      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01591      MOVE XCTL-005               TO  PGM-NAME.
01592
01593      GO TO 9300-XCTL.
01594
01595  8880-NOT-FOUND.
01596      MOVE ER-0142                TO  EMI-ERROR.
01597
01598      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01599
01600      MOVE -1                     TO  MAINTYPL.
01601
01602      IF EIBTRNID NOT = TRANS-ID
01603          GO TO 8100-SEND-INITIAL-MAP.
01604
01605      GO TO 8200-SEND-DATAONLY.
01606
01607  9100-RETURN-TRAN.
01608      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01609      MOVE '653A'                 TO  PI-CURRENT-SCREEN-NO.
01610
01611      
      * EXEC CICS RETURN
01612 *        TRANSID   (TRANS-ID)
01613 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01614 *        LENGTH    (PI-COMM-LENGTH)
01615 *    END-EXEC.
      *    MOVE '.(CT                  &   #00004475' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01616
01617  9200-RETURN-MAIN-MENU.
01618      MOVE XCTL-626               TO  PGM-NAME.
01619
01620      GO TO 9300-XCTL.
01621
01622  9300-XCTL.
01623      
      * EXEC CICS XCTL
01624 *        PROGRAM   (PGM-NAME)
01625 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01626 *        LENGTH    (PI-COMM-LENGTH)
01627 *    END-EXEC.
      *    MOVE '.$C                   $   #00004487' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01628
01629  9400-CLEAR.
01630      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
01631
01632      GO TO 9300-XCTL.
01633
01634  9500-PF12.
01635      MOVE XCTL-010               TO  PGM-NAME.
01636
01637      GO TO 9300-XCTL.
01638
01639  9600-PGMID-ERROR.
01640      
      * EXEC CICS HANDLE CONDITION
01641 *        PGMIDERR  (8300-SEND-TEXT)
01642 *    END-EXEC.
      *    MOVE '"$L                   ! ) #00004504' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303034353034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01643
01644      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
01645      MOVE ' '                    TO  PI-ENTRY-CD-1.
01646      MOVE XCTL-005               TO  PGM-NAME.
01647      MOVE PGM-NAME               TO  LOGOFF-PGM.
01648      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01649
01650      GO TO 9300-XCTL.
01651
01652  9700-LINK-DATE-CONVERT.
01653      
      * EXEC CICS LINK
01654 *        PROGRAM   ('ELDATCV')
01655 *        COMMAREA  (DATE-CONVERSION-DATA)
01656 *        LENGTH    (DC-COMM-LENGTH)
01657 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00004517' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01658
01659  9700-EXIT.
01660      EXIT.
01661
01662  9900-ERROR-FORMAT.
01663      IF NOT EMI-ERRORS-COMPLETE
01664          MOVE LINK-001           TO  PGM-NAME
01665          
      * EXEC CICS LINK
01666 *            PROGRAM   (PGM-NAME)
01667 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
01668 *            LENGTH    (EMI-COMM-LENGTH)
01669 *        END-EXEC.
      *    MOVE '."C                   ''   #00004529' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01670
01671  9900-EXIT.
01672      EXIT.
01673
01674  9990-ABEND.
01675      MOVE LINK-004               TO  PGM-NAME.
01676      MOVE DFHEIBLK               TO  EMI-LINE1.
01677
01678      
      * EXEC CICS LINK
01679 *        PROGRAM   (PGM-NAME)
01680 *        COMMAREA  (EMI-LINE1)
01681 *        LENGTH    (72)
01682 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00004542' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01683
01684      GO TO 8200-SEND-DATAONLY.
01685
01686      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL653' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01687
01688  9995-SECURITY-VIOLATION.
01689 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00004570' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353730' TO DFHEIV0(25:11)
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
01690
01691  9995-EXIT.
01692       EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL653' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ABEND,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9990-ABEND,
                     4250-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 7350-ENDFILE,
                     7375-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7450-ENDFILE,
                     7475-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7550-ENDFILE,
                     7575-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7650-ENDFILE,
                     7675-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL653' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
