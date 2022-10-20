00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL6505.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/16/95 10:30:26.
00007 *                            VMOD=2.015
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
00022 *
00023 *REMARKS.  TRANSACTION - EXH6 - ACCOUNT MAINT (BENEFIT CONTROLS).
00022 *
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL6505AI FILLER
101101******************************************************************
00024
00025  ENVIRONMENT DIVISION.
00026
00027      EJECT
00028  DATA DIVISION.
00029  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00030  77  FILLER  PIC X(32)  VALUE '********************************'.
00031  77  FILLER  PIC X(32)  VALUE '*    EL6505 WORKING STORAGE    *'.
00032  77  FILLER  PIC X(32)  VALUE '*********** VMOD 2.015 *********'.
00033
00034  01  WS-DATE-AREA.
00035      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00036      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
00037
00038  01  STANDARD-AREAS.
00039      12  RETURNED-FROM               PIC X(8)    VALUE SPACES.
00040      12  QID1.
00041          16  QID1-TERM               PIC X(4).
00042          16  FILLER                  PIC X(4)    VALUE '650A'.
00043      12  QID2.
00044          16  QID2-TERM               PIC X(4).
00045          16  FILLER                  PIC X(4)    VALUE '650B'.
00046      12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1500.
00047      12  WS-MAP-LENGTH               PIC S9(4) COMP VALUE +885.
00048      12  MAP-NAME                    PIC X(8)    VALUE 'EL6505A'.
00049      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6505S'.
00050      12  SCREEN-NUMBER               PIC X(4)    VALUE '650F'.
00051      12  TRANS-ID                    PIC X(4)    VALUE 'EXH6'.
00052      12  THIS-PGM                    PIC X(8)    VALUE 'EL6505'.
00053      12  PGM-NAME                    PIC X(8).
00054      12  TIME-IN                     PIC S9(7).
00055      12  TIME-OUT-R  REDEFINES TIME-IN.
00056          16  FILLER                  PIC X.
00057          16  TIME-OUT                PIC 99V99.
00058          16  FILLER                  PIC XX.
00059      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00060      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00061      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.
00062      12  XCTL-650                    PIC X(8)    VALUE 'EL650'.
00063      12  XCTL-6501                   PIC X(8)    VALUE 'EL6501'.
00064      12  XCTL-6502                   PIC X(8)    VALUE 'EL6502'.
00065      12  XCTL-6503                   PIC X(8)    VALUE 'EL6503'.
00066      12  XCTL-6504                   PIC X(8)    VALUE 'EL6504'.
00067      12  XCTL-6506                   PIC X(8)    VALUE 'EL6506'.
00068      12  XCTL-6507                   PIC X(8)    VALUE 'EL6507'.
00069      12  XCTL-6565                   PIC X(8)    VALUE 'EL6565'.
00070      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00071      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00072      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00073      12  FILE-ID                     PIC X(8)    VALUE SPACES.
00074      12  ERACCT-FILE                 PIC X(8)    VALUE 'ERACCT'.
00075      12  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL'.
00076      12  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.
00077      12  YMD-CURRENT-SAVE            PIC X(6)    VALUE SPACES.
00078
00079      12  ERACCT-LENGTH               PIC S9(4)   VALUE +2023 COMP.
00080      12  ELCNTL-LENGTH               PIC S9(4)   VALUE +527  COMP.
00081      12  SC-ITEM                     PIC S9(4)   VALUE +1    COMP.
00082      12  WS-JOURNAL-FILE-LENGTH      PIC S9(4)   VALUE +0    COMP.
00083      12  SUB1                        PIC S9(4)   VALUE +0    COMP.
00084      12  SUB2                        PIC S9(4)   VALUE +0    COMP.
00085      12  SUB3                        PIC S9(4)   VALUE +0    COMP.
00086
00087      12  DEEDIT-FIELD                PIC X(15).
00088      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).
CIDMOD     12  DEEDIT-FIELD-V6  REDEFINES DEEDIT-FIELD PIC S9(9)V9(6).
00089
00090      12  WS-EDIT-FIELD-CONV          PIC S9(4)   VALUE +0.
00091
00092      EJECT
00093      12  ERROR-MESSAGES.
00094          16  ER-0000                 PIC X(4)    VALUE '0000'.
00095          16  ER-0002                 PIC X(4)    VALUE '0002'.
00096          16  ER-0004                 PIC X(4)    VALUE '0004'.
00097          16  ER-0008                 PIC X(4)    VALUE '0008'.
00098          16  ER-0029                 PIC X(4)    VALUE '0029'.
00099          16  ER-0068                 PIC X(4)    VALUE '0068'.
00100          16  ER-0070                 PIC X(4)    VALUE '0070'.
00101          16  ER-0150                 PIC X(4)    VALUE '0150'.
00102          16  ER-0151                 PIC X(4)    VALUE '0151'.
00103          16  ER-0627                 PIC X(4)    VALUE '0627'.
00104          16  ER-2039                 PIC X(4)    VALUE '2039'.
00105          16  ER-2298                 PIC X(4)    VALUE '2298'.
00106          16  ER-2387                 PIC X(4)    VALUE '2387'.
00107          16  ER-2388                 PIC X(4)    VALUE '2388'.
00108          16  ER-2389                 PIC X(4)    VALUE '2389'.
00109          16  ER-2390                 PIC X(4)    VALUE '2390'.
00110          16  ER-2572                 PIC X(4)    VALUE '2572'.
00111          16  ER-2946                 PIC X(4)    VALUE '2946'.
00112          16  ER-2947                 PIC X(4)    VALUE '2947'.
00113          16  ER-2948                 PIC X(4)    VALUE '2948'.
00114          16  ER-3126                 PIC X(4)    VALUE '3126'.
00115          16  ER-7240                 PIC X(4)    VALUE '7240'.
00116          16  ER-7241                 PIC X(4)    VALUE '7241'.
00117          16  ER-8034                 PIC X(4)    VALUE '8034'.
00118          16  ER-8149                 PIC X(4)    VALUE '8149'.
00119          16  ER-8150                 PIC X(4)    VALUE '8150'.
00120          16  ER-XXXX                 PIC X(4)    VALUE '9999'.
00121
00122      12  ELCNTL-KEY.
00123          16  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.
00124          16  CNTL-REC-TYPE           PIC X       VALUE SPACES.
00125          16  CNTL-ACCESS.
00126              20  FILLER              PIC X(02)   VALUE SPACES.
00127              20  CNTL-HI-BEN         PIC X(02)   VALUE SPACES.
00128          16  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.
00129
00130      12  WS-MAX-BNDX                 PIC S99.
00131
00132      12  WS-SELECT-LINE              PIC S99.
00133
00134      12  WS-EDIT-USECODE             PIC X.
00135          88  VALID-USE-CODE             VALUE ' ' 'N' 'Y'.
00136      12  WS-EDIT-BENCODE             PIC XX.
00137          88  INVALID-BENEFIT            VALUE '  '.
00138      12  WS-EDIT-REMTERM             PIC X.
00139          88  VALID-REM-TERM             VALUE ' ' '1' THRU '7'.
00140      12  WS-WORK-BENEFIT-TABLE.
00141          16  WS-WORK-TABLE OCCURS 20.
00142              20  WS-WORK-BENEFIT         PIC X(02).
00143              20  WS-WORK-TYPE            PIC X(01).
00144              20  WS-WORK-REVISION        PIC X(03).
00145              20  WS-WORK-REM-TERM        PIC X(01).
00146              20  WS-WORK-RETRO-Y-N       PIC X(01).
00147              20  WS-FILLER               PIC X(02).
00148          16  FILLER                      PIC X(80).
00149
00150      12  WS-DMD-WORK-BENEFIT-TABLE.
00151          16  WS-DMD-WORK-TABLE OCCURS 50.
00152              20  WS-DMD-WORK-BENEFIT     PIC X(02).
00153              20  WS-DMD-WORK-TYPE        PIC X(01).
00154              20  WS-DMD-WORK-REVISION    PIC X(03).
00155              20  WS-DMD-WORK-REM-TERM    PIC X(01).
00156              20  WS-DMD-WORK-RETRO-Y-N   PIC X(01).
00157              20  WS-DMD-FILLER           PIC X(02).
00158          16  FILLER                      PIC X(80).
00159
00160      12  WS-DMD-WORK-BENEFIT-TABLE-2.
00161          16  WS-DMD-WORK-TABLE-2 OCCURS 50.
00162              20  WS-DMD-WORK-BENEFIT-2   PIC X(02).
00163              20  WS-DMD-WORK-TYPE-2      PIC X(01).
00164              20  WS-DMD-WORK-REVISION-2  PIC X(03).
00165              20  WS-DMD-WORK-REM-TERM-2  PIC X(01).
00166              20  WS-DMD-WORK-RETRO-Y-N-2 PIC X(01).
00167              20  WS-DMD-FILLER-2         PIC X(02).
00168          16  FILLER                      PIC X(80).
00169
00170      EJECT
00171 *                          COPY ELCSCTM.
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
00172      EJECT
00173 *                          COPY ELCSCRTY.
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
00174      EJECT
00175 *                                    COPY ELCLOGOF.
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
00176
00177      EJECT
00178 *                                    COPY ELCDATE.
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
00179
00180      EJECT
00181 *                                    COPY ELCATTR.
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
00182
00183      EJECT
00184 *                                    COPY ELCEMIB.
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
00185
00186      EJECT
00187 *                                    COPY ELCINTF.
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
00188 *                                    COPY ELC650PI.
00001 ******************************************************************
00002 *
00003 *                            ELC650PI.
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007
00006 *  ***  NOTE  ***   IF ANY CHANGES ARE MADE TO THIS COPYBOOK
00007 *   YOU MUST CONSIDER ALL PROGRAMS THAT USE THIS COPYBOOK AND
00008 * PROGRAM EL6565.  ALSO, CONSIDER EL106 AND EL1061
00009 *
00010 ******************************************************************
101916******************************************************************
101916*                   C H A N G E   L O G
101916*
101916* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101916*-----------------------------------------------------------------
101916*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101916* EFFECTIVE    NUMBER
101916*-----------------------------------------------------------------
101916* 101916  IR2016101900001  PEMA  Inc tot line to 3 bytes
00011
00012      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00013          16  PI-MAINT                   PIC X.
00014          16  PI-PREV-ACCOUNT            PIC X(20).
00015          16  PI-PREV-VG-ACCOUNT         PIC X(20).
00016          16  PI-ACCT-KEY.
00017              20  PI-ACCT-CCGSA-KEY.
00018                  24  PI-ACCT-CO             PIC X.
00019                  24  PI-ACCT-CARRIER        PIC X.
00020                  24  PI-ACCT-GROUPING       PIC X(6).
00021                  24  PI-ACCT-STATE          PIC XX.
00022                  24  PI-ACCT-ACCOUNT        PIC X(10).
00023              20  PI-ACCT-EXP-DT           PIC XX.
00024              20  PI-ACCT-REST-OF-EXP      PIC X(4).
00025          16  PI-ACCT-ID                 PIC X(8).
00026          16  PI-PLAN-KEY.
00027              20  PI-PLAN-ACCT-KEY.
00028                  24  PI-PLAN-COMPANY-CD PIC X.
00029                  24  PI-PLAN-CARRIER    PIC X.
00030                  24  PI-PLAN-GROUP      PIC X(6).
00031                  24  PI-PLAN-STATE      PIC X(2).
00032                  24  PI-PLAN-ACCOUNT    PIC X(10).
00033              20  PI-PLAN-BEN-TYPE       PIC X.
00034              20  PI-PLAN-BEN            PIC XX.
00035              20  PI-PLAN-REVISION       PIC X(3).
00036          16  PI-WS-STATE                PIC XX.
00037          16  PI-WS-CLASS                PIC XX.
00038          16  PI-WS-DEV                  PIC X(3).
00039          16  PI-WS-TYPE                 PIC X.
00040          16  PI-WS-PLAN                 PIC XX.
00041
00042          16  PI-ERPNDB-ALT-KEY.
00043              20  PI-PB-COMPANY-CD-A1    PIC X.
00044              20  PI-PB-CARRIER          PIC X.
00045              20  PI-PB-GROUPING         PIC X(6).
00046              20  PI-PB-STATE            PIC XX.
00047              20  PI-PB-ACCOUNT          PIC X(10).
00048              20  PI-PB-CERT-EFF-DT      PIC XX.
00049              20  PI-PB-CERT-NO          PIC X(10).
00050              20  PI-PB-ALT-CHG-SEQ-NO   PIC S9(4)      COMP.
00051              20  PI-PB-RECORD-TYPE      PIC X.
00052
00053          16  PI-DATE-RANGE-TABLE.
00054              20  PI-TABLE-ENT OCCURS 32 TIMES
00055                             INDEXED BY T-INDEX.
00056                  24  PI-BIN-EFF-DT          PIC XX.
00057                  24  PI-BIN-EXP-DT          PIC XX.
00058                  24  PI-BIN-MAINT-DT        PIC XX.
00059                  24  PI-BIN-LO-CERT         PIC XX.
00060                  24  PI-BIN-AR-HI-CERT      PIC XX.
00061                  24  PI-BIN-HI-CERT         PIC XX.
00062          16  PI-PAGE-NUMBER             PIC S9.
00063              88  PI-FST-PAGE               VALUE +1.
00064              88  PI-2ND-PAGE               VALUE +2.
00065              88  PI-3RD-PAGE               VALUE +3.
00066              88  PI-LST-PAGE               VALUE +4.
101916         16  PI-TOTAL-LINES             PIC S999.
00068          16  PI-LINE-SELECTED    PIC S9.
00069 ***  Y2K PROJ 7744
00070          16  EFFCHG-SAVE         PIC 9(11)   COMP-3.
00071          16  BIN-EFFCHG-SAVE     PIC XX.
00072          16  EXPCHG-SAVE         PIC 9(11)   COMP-3.
00073 ***  Y2K PROJ 7744
00074          16  BIN-EXPCHG-SAVE     PIC XX.
00075          16  PI-RECORD-ADDED-SW  PIC X.
00076              88  PI-RECORD-ADDED            VALUE '1'.
00077              88  PI-RECORD-NOT-CREATED      VALUE SPACE.
00078          16  PI-ACCNAME          PIC X(30).
00079          16  PI-COMM-POINTER     PIC S9(8)   COMP.
00080          16  PI-SV-MAINT         PIC X.
00081          16  PI-CURRENT-LINE     PIC S9(3)   COMP-3.
00082          16  PI-TEMP-STOR-ITEMS  PIC S9(4)   COMP.
00083          16  PI-UPDATE-SW        PIC X.
00084              88  PI-CHANGES-MADE             VALUE '1'.
00085          16  PI-NOTE-TYPE        PIC X.
00086              88  PI-ACCT-NOTE                VALUE '1'.
00087          16  PI-DMD-FILE-SW      PIC X.
00088              88  END-OF-FILE                 VALUE 'E'.
00089              88  INTO-NEXT-BENEFITS          VALUE 'I'.
00090              88  FIRST-OCCURS                VALUE 'F'.
00091          16  PI-DMD-OCCURS       PIC S999.
00092          16  PI-DMD-SCREEN       PIC X.
00093              88  SCREEN-1-DISPLAYED  VALUE '1'.
00094              88  SCREEN-2-DISPLAYED  VALUE '2'.
00095              88  SCREEN-3-DISPLAYED  VALUE '3'.
00096          16  PI-NAMEFLG          PIC X.
PEMTST         16  PI-EL650-DEL-SW     PIC X.
               16  PI-MAX-MFEE         PIC S9(5) COMP-3.
               16  PI-DCC-PROD-CODE    PIC XXX.
101916         16  FILLER              PIC X(34).
00098      EJECT
00189
00190 *                                    COPY ELCJPFX.
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
00191                                      PIC X(2000).
00192
00193      EJECT
00194 *                                    COPY ELCAID.
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
00195  01  FILLER    REDEFINES DFHAID.
00196      12  FILLER                      PIC X(8).
00197      12  PF-VALUES                   PIC X       OCCURS 2.
00198
00199      EJECT
00200 *                                    COPY EL6505S.
       01  EL6505AI.
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
           05  MAINTYPL PIC S9(0004) COMP.
           05  MAINTYPF PIC  X(0001).
           05  FILLER REDEFINES MAINTYPF.
               10  MAINTYPA PIC  X(0001).
           05  MAINTYPI PIC  X(0001).
      *    -------------------------------
           05  CARL PIC S9(0004) COMP.
           05  CARF PIC  X(0001).
           05  FILLER REDEFINES CARF.
               10  CARA PIC  X(0001).
           05  CARI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  EFFDTEL PIC S9(0004) COMP.
           05  EFFDTEF PIC  X(0001).
           05  FILLER REDEFINES EFFDTEF.
               10  EFFDTEA PIC  X(0001).
           05  EFFDTEI PIC  X(0008).
      *    -------------------------------
           05  EXPDTEL PIC S9(0004) COMP.
           05  EXPDTEF PIC  X(0001).
           05  FILLER REDEFINES EXPDTEF.
               10  EXPDTEA PIC  X(0001).
           05  EXPDTEI PIC  X(0008).
      *    -------------------------------
           05  USECODEL PIC S9(0004) COMP.
           05  USECODEF PIC  X(0001).
           05  FILLER REDEFINES USECODEF.
               10  USECODEA PIC  X(0001).
           05  USECODEI PIC  X(0001).
      *    -------------------------------
           05  RETRO1L PIC S9(0004) COMP.
           05  RETRO1F PIC  X(0001).
           05  FILLER REDEFINES RETRO1F.
               10  RETRO1A PIC  X(0001).
           05  RETRO1I PIC  X(0005).
      *    -------------------------------
           05  RETRO2L PIC S9(0004) COMP.
           05  RETRO2F PIC  X(0001).
           05  FILLER REDEFINES RETRO2F.
               10  RETRO2A PIC  X(0001).
           05  RETRO2I PIC  X(0005).
      *    -------------------------------
           05  RETRO3L PIC S9(0004) COMP.
           05  RETRO3F PIC  X(0001).
           05  FILLER REDEFINES RETRO3F.
               10  RETRO3A PIC  X(0001).
           05  RETRO3I PIC  X(0005).
      *    -------------------------------
           05  RETRO4L PIC S9(0004) COMP.
           05  RETRO4F PIC  X(0001).
           05  FILLER REDEFINES RETRO4F.
               10  RETRO4A PIC  X(0001).
           05  RETRO4I PIC  X(0005).
      *    -------------------------------
           05  NUMB01L PIC S9(0004) COMP.
           05  NUMB01F PIC  X(0001).
           05  FILLER REDEFINES NUMB01F.
               10  NUMB01A PIC  X(0001).
           05  NUMB01I PIC  X(0004).
      *    -------------------------------
           05  CODE01L PIC S9(0004) COMP.
           05  CODE01F PIC  X(0001).
           05  FILLER REDEFINES CODE01F.
               10  CODE01A PIC  X(0001).
           05  CODE01I PIC  X(0002).
      *    -------------------------------
           05  TYPE01L PIC S9(0004) COMP.
           05  TYPE01F PIC  X(0001).
           05  FILLER REDEFINES TYPE01F.
               10  TYPE01A PIC  X(0001).
           05  TYPE01I PIC  X(0001).
      *    -------------------------------
           05  REVSN1L PIC S9(0004) COMP.
           05  REVSN1F PIC  X(0001).
           05  FILLER REDEFINES REVSN1F.
               10  REVSN1A PIC  X(0001).
           05  REVSN1I PIC  X(0003).
      *    -------------------------------
           05  RETRO01L PIC S9(0004) COMP.
           05  RETRO01F PIC  X(0001).
           05  FILLER REDEFINES RETRO01F.
               10  RETRO01A PIC  X(0001).
           05  RETRO01I PIC  X(0001).
      *    -------------------------------
           05  RTRM01L PIC S9(0004) COMP.
           05  RTRM01F PIC  X(0001).
           05  FILLER REDEFINES RTRM01F.
               10  RTRM01A PIC  X(0001).
           05  RTRM01I PIC  X(0001).
      *    -------------------------------
           05  NUMB02L PIC S9(0004) COMP.
           05  NUMB02F PIC  X(0001).
           05  FILLER REDEFINES NUMB02F.
               10  NUMB02A PIC  X(0001).
           05  NUMB02I PIC  X(0004).
      *    -------------------------------
           05  CODE02L PIC S9(0004) COMP.
           05  CODE02F PIC  X(0001).
           05  FILLER REDEFINES CODE02F.
               10  CODE02A PIC  X(0001).
           05  CODE02I PIC  X(0002).
      *    -------------------------------
           05  TYPE02L PIC S9(0004) COMP.
           05  TYPE02F PIC  X(0001).
           05  FILLER REDEFINES TYPE02F.
               10  TYPE02A PIC  X(0001).
           05  TYPE02I PIC  X(0001).
      *    -------------------------------
           05  REVSN2L PIC S9(0004) COMP.
           05  REVSN2F PIC  X(0001).
           05  FILLER REDEFINES REVSN2F.
               10  REVSN2A PIC  X(0001).
           05  REVSN2I PIC  X(0003).
      *    -------------------------------
           05  RETRO02L PIC S9(0004) COMP.
           05  RETRO02F PIC  X(0001).
           05  FILLER REDEFINES RETRO02F.
               10  RETRO02A PIC  X(0001).
           05  RETRO02I PIC  X(0001).
      *    -------------------------------
           05  RTRM02L PIC S9(0004) COMP.
           05  RTRM02F PIC  X(0001).
           05  FILLER REDEFINES RTRM02F.
               10  RTRM02A PIC  X(0001).
           05  RTRM02I PIC  X(0001).
      *    -------------------------------
           05  NUMB03L PIC S9(0004) COMP.
           05  NUMB03F PIC  X(0001).
           05  FILLER REDEFINES NUMB03F.
               10  NUMB03A PIC  X(0001).
           05  NUMB03I PIC  X(0004).
      *    -------------------------------
           05  CODE03L PIC S9(0004) COMP.
           05  CODE03F PIC  X(0001).
           05  FILLER REDEFINES CODE03F.
               10  CODE03A PIC  X(0001).
           05  CODE03I PIC  X(0002).
      *    -------------------------------
           05  TYPE03L PIC S9(0004) COMP.
           05  TYPE03F PIC  X(0001).
           05  FILLER REDEFINES TYPE03F.
               10  TYPE03A PIC  X(0001).
           05  TYPE03I PIC  X(0001).
      *    -------------------------------
           05  REVSN3L PIC S9(0004) COMP.
           05  REVSN3F PIC  X(0001).
           05  FILLER REDEFINES REVSN3F.
               10  REVSN3A PIC  X(0001).
           05  REVSN3I PIC  X(0003).
      *    -------------------------------
           05  RETRO03L PIC S9(0004) COMP.
           05  RETRO03F PIC  X(0001).
           05  FILLER REDEFINES RETRO03F.
               10  RETRO03A PIC  X(0001).
           05  RETRO03I PIC  X(0001).
      *    -------------------------------
           05  RTRM03L PIC S9(0004) COMP.
           05  RTRM03F PIC  X(0001).
           05  FILLER REDEFINES RTRM03F.
               10  RTRM03A PIC  X(0001).
           05  RTRM03I PIC  X(0001).
      *    -------------------------------
           05  NUMB04L PIC S9(0004) COMP.
           05  NUMB04F PIC  X(0001).
           05  FILLER REDEFINES NUMB04F.
               10  NUMB04A PIC  X(0001).
           05  NUMB04I PIC  X(0004).
      *    -------------------------------
           05  CODE04L PIC S9(0004) COMP.
           05  CODE04F PIC  X(0001).
           05  FILLER REDEFINES CODE04F.
               10  CODE04A PIC  X(0001).
           05  CODE04I PIC  X(0002).
      *    -------------------------------
           05  TYPE04L PIC S9(0004) COMP.
           05  TYPE04F PIC  X(0001).
           05  FILLER REDEFINES TYPE04F.
               10  TYPE04A PIC  X(0001).
           05  TYPE04I PIC  X(0001).
      *    -------------------------------
           05  REVSN4L PIC S9(0004) COMP.
           05  REVSN4F PIC  X(0001).
           05  FILLER REDEFINES REVSN4F.
               10  REVSN4A PIC  X(0001).
           05  REVSN4I PIC  X(0003).
      *    -------------------------------
           05  RETRO04L PIC S9(0004) COMP.
           05  RETRO04F PIC  X(0001).
           05  FILLER REDEFINES RETRO04F.
               10  RETRO04A PIC  X(0001).
           05  RETRO04I PIC  X(0001).
      *    -------------------------------
           05  RTRM04L PIC S9(0004) COMP.
           05  RTRM04F PIC  X(0001).
           05  FILLER REDEFINES RTRM04F.
               10  RTRM04A PIC  X(0001).
           05  RTRM04I PIC  X(0001).
      *    -------------------------------
           05  NUMB05L PIC S9(0004) COMP.
           05  NUMB05F PIC  X(0001).
           05  FILLER REDEFINES NUMB05F.
               10  NUMB05A PIC  X(0001).
           05  NUMB05I PIC  X(0004).
      *    -------------------------------
           05  CODE05L PIC S9(0004) COMP.
           05  CODE05F PIC  X(0001).
           05  FILLER REDEFINES CODE05F.
               10  CODE05A PIC  X(0001).
           05  CODE05I PIC  X(0002).
      *    -------------------------------
           05  TYPE05L PIC S9(0004) COMP.
           05  TYPE05F PIC  X(0001).
           05  FILLER REDEFINES TYPE05F.
               10  TYPE05A PIC  X(0001).
           05  TYPE05I PIC  X(0001).
      *    -------------------------------
           05  REVSN5L PIC S9(0004) COMP.
           05  REVSN5F PIC  X(0001).
           05  FILLER REDEFINES REVSN5F.
               10  REVSN5A PIC  X(0001).
           05  REVSN5I PIC  X(0003).
      *    -------------------------------
           05  RETRO05L PIC S9(0004) COMP.
           05  RETRO05F PIC  X(0001).
           05  FILLER REDEFINES RETRO05F.
               10  RETRO05A PIC  X(0001).
           05  RETRO05I PIC  X(0001).
      *    -------------------------------
           05  RTRM05L PIC S9(0004) COMP.
           05  RTRM05F PIC  X(0001).
           05  FILLER REDEFINES RTRM05F.
               10  RTRM05A PIC  X(0001).
           05  RTRM05I PIC  X(0001).
      *    -------------------------------
           05  NUMB06L PIC S9(0004) COMP.
           05  NUMB06F PIC  X(0001).
           05  FILLER REDEFINES NUMB06F.
               10  NUMB06A PIC  X(0001).
           05  NUMB06I PIC  X(0004).
      *    -------------------------------
           05  CODE06L PIC S9(0004) COMP.
           05  CODE06F PIC  X(0001).
           05  FILLER REDEFINES CODE06F.
               10  CODE06A PIC  X(0001).
           05  CODE06I PIC  X(0002).
      *    -------------------------------
           05  TYPE06L PIC S9(0004) COMP.
           05  TYPE06F PIC  X(0001).
           05  FILLER REDEFINES TYPE06F.
               10  TYPE06A PIC  X(0001).
           05  TYPE06I PIC  X(0001).
      *    -------------------------------
           05  REVSN6L PIC S9(0004) COMP.
           05  REVSN6F PIC  X(0001).
           05  FILLER REDEFINES REVSN6F.
               10  REVSN6A PIC  X(0001).
           05  REVSN6I PIC  X(0003).
      *    -------------------------------
           05  RETRO06L PIC S9(0004) COMP.
           05  RETRO06F PIC  X(0001).
           05  FILLER REDEFINES RETRO06F.
               10  RETRO06A PIC  X(0001).
           05  RETRO06I PIC  X(0001).
      *    -------------------------------
           05  RTRM06L PIC S9(0004) COMP.
           05  RTRM06F PIC  X(0001).
           05  FILLER REDEFINES RTRM06F.
               10  RTRM06A PIC  X(0001).
           05  RTRM06I PIC  X(0001).
      *    -------------------------------
           05  NUMB07L PIC S9(0004) COMP.
           05  NUMB07F PIC  X(0001).
           05  FILLER REDEFINES NUMB07F.
               10  NUMB07A PIC  X(0001).
           05  NUMB07I PIC  X(0004).
      *    -------------------------------
           05  CODE07L PIC S9(0004) COMP.
           05  CODE07F PIC  X(0001).
           05  FILLER REDEFINES CODE07F.
               10  CODE07A PIC  X(0001).
           05  CODE07I PIC  X(0002).
      *    -------------------------------
           05  TYPE07L PIC S9(0004) COMP.
           05  TYPE07F PIC  X(0001).
           05  FILLER REDEFINES TYPE07F.
               10  TYPE07A PIC  X(0001).
           05  TYPE07I PIC  X(0001).
      *    -------------------------------
           05  REVSN7L PIC S9(0004) COMP.
           05  REVSN7F PIC  X(0001).
           05  FILLER REDEFINES REVSN7F.
               10  REVSN7A PIC  X(0001).
           05  REVSN7I PIC  X(0003).
      *    -------------------------------
           05  RETRO07L PIC S9(0004) COMP.
           05  RETRO07F PIC  X(0001).
           05  FILLER REDEFINES RETRO07F.
               10  RETRO07A PIC  X(0001).
           05  RETRO07I PIC  X(0001).
      *    -------------------------------
           05  RTRM07L PIC S9(0004) COMP.
           05  RTRM07F PIC  X(0001).
           05  FILLER REDEFINES RTRM07F.
               10  RTRM07A PIC  X(0001).
           05  RTRM07I PIC  X(0001).
      *    -------------------------------
           05  NUMB08L PIC S9(0004) COMP.
           05  NUMB08F PIC  X(0001).
           05  FILLER REDEFINES NUMB08F.
               10  NUMB08A PIC  X(0001).
           05  NUMB08I PIC  X(0004).
      *    -------------------------------
           05  CODE08L PIC S9(0004) COMP.
           05  CODE08F PIC  X(0001).
           05  FILLER REDEFINES CODE08F.
               10  CODE08A PIC  X(0001).
           05  CODE08I PIC  X(0002).
      *    -------------------------------
           05  TYPE08L PIC S9(0004) COMP.
           05  TYPE08F PIC  X(0001).
           05  FILLER REDEFINES TYPE08F.
               10  TYPE08A PIC  X(0001).
           05  TYPE08I PIC  X(0001).
      *    -------------------------------
           05  REVSN8L PIC S9(0004) COMP.
           05  REVSN8F PIC  X(0001).
           05  FILLER REDEFINES REVSN8F.
               10  REVSN8A PIC  X(0001).
           05  REVSN8I PIC  X(0003).
      *    -------------------------------
           05  RETRO08L PIC S9(0004) COMP.
           05  RETRO08F PIC  X(0001).
           05  FILLER REDEFINES RETRO08F.
               10  RETRO08A PIC  X(0001).
           05  RETRO08I PIC  X(0001).
      *    -------------------------------
           05  RTRM08L PIC S9(0004) COMP.
           05  RTRM08F PIC  X(0001).
           05  FILLER REDEFINES RTRM08F.
               10  RTRM08A PIC  X(0001).
           05  RTRM08I PIC  X(0001).
      *    -------------------------------
           05  NUMB09L PIC S9(0004) COMP.
           05  NUMB09F PIC  X(0001).
           05  FILLER REDEFINES NUMB09F.
               10  NUMB09A PIC  X(0001).
           05  NUMB09I PIC  X(0004).
      *    -------------------------------
           05  CODE09L PIC S9(0004) COMP.
           05  CODE09F PIC  X(0001).
           05  FILLER REDEFINES CODE09F.
               10  CODE09A PIC  X(0001).
           05  CODE09I PIC  X(0002).
      *    -------------------------------
           05  TYPE09L PIC S9(0004) COMP.
           05  TYPE09F PIC  X(0001).
           05  FILLER REDEFINES TYPE09F.
               10  TYPE09A PIC  X(0001).
           05  TYPE09I PIC  X(0001).
      *    -------------------------------
           05  REVSN9L PIC S9(0004) COMP.
           05  REVSN9F PIC  X(0001).
           05  FILLER REDEFINES REVSN9F.
               10  REVSN9A PIC  X(0001).
           05  REVSN9I PIC  X(0003).
      *    -------------------------------
           05  RETRO09L PIC S9(0004) COMP.
           05  RETRO09F PIC  X(0001).
           05  FILLER REDEFINES RETRO09F.
               10  RETRO09A PIC  X(0001).
           05  RETRO09I PIC  X(0001).
      *    -------------------------------
           05  RTRM09L PIC S9(0004) COMP.
           05  RTRM09F PIC  X(0001).
           05  FILLER REDEFINES RTRM09F.
               10  RTRM09A PIC  X(0001).
           05  RTRM09I PIC  X(0001).
      *    -------------------------------
           05  NUMB10L PIC S9(0004) COMP.
           05  NUMB10F PIC  X(0001).
           05  FILLER REDEFINES NUMB10F.
               10  NUMB10A PIC  X(0001).
           05  NUMB10I PIC  X(0004).
      *    -------------------------------
           05  CODE10L PIC S9(0004) COMP.
           05  CODE10F PIC  X(0001).
           05  FILLER REDEFINES CODE10F.
               10  CODE10A PIC  X(0001).
           05  CODE10I PIC  X(0002).
      *    -------------------------------
           05  TYPE10L PIC S9(0004) COMP.
           05  TYPE10F PIC  X(0001).
           05  FILLER REDEFINES TYPE10F.
               10  TYPE10A PIC  X(0001).
           05  TYPE10I PIC  X(0001).
      *    -------------------------------
           05  REVSN10L PIC S9(0004) COMP.
           05  REVSN10F PIC  X(0001).
           05  FILLER REDEFINES REVSN10F.
               10  REVSN10A PIC  X(0001).
           05  REVSN10I PIC  X(0003).
      *    -------------------------------
           05  RETRO10L PIC S9(0004) COMP.
           05  RETRO10F PIC  X(0001).
           05  FILLER REDEFINES RETRO10F.
               10  RETRO10A PIC  X(0001).
           05  RETRO10I PIC  X(0001).
      *    -------------------------------
           05  RTRM10L PIC S9(0004) COMP.
           05  RTRM10F PIC  X(0001).
           05  FILLER REDEFINES RTRM10F.
               10  RTRM10A PIC  X(0001).
           05  RTRM10I PIC  X(0001).
      *    -------------------------------
           05  NUMB11L PIC S9(0004) COMP.
           05  NUMB11F PIC  X(0001).
           05  FILLER REDEFINES NUMB11F.
               10  NUMB11A PIC  X(0001).
           05  NUMB11I PIC  X(0004).
      *    -------------------------------
           05  CODE11L PIC S9(0004) COMP.
           05  CODE11F PIC  X(0001).
           05  FILLER REDEFINES CODE11F.
               10  CODE11A PIC  X(0001).
           05  CODE11I PIC  X(0002).
      *    -------------------------------
           05  TYPE11L PIC S9(0004) COMP.
           05  TYPE11F PIC  X(0001).
           05  FILLER REDEFINES TYPE11F.
               10  TYPE11A PIC  X(0001).
           05  TYPE11I PIC  X(0001).
      *    -------------------------------
           05  REVSN11L PIC S9(0004) COMP.
           05  REVSN11F PIC  X(0001).
           05  FILLER REDEFINES REVSN11F.
               10  REVSN11A PIC  X(0001).
           05  REVSN11I PIC  X(0003).
      *    -------------------------------
           05  RETRO11L PIC S9(0004) COMP.
           05  RETRO11F PIC  X(0001).
           05  FILLER REDEFINES RETRO11F.
               10  RETRO11A PIC  X(0001).
           05  RETRO11I PIC  X(0001).
      *    -------------------------------
           05  RTRM11L PIC S9(0004) COMP.
           05  RTRM11F PIC  X(0001).
           05  FILLER REDEFINES RTRM11F.
               10  RTRM11A PIC  X(0001).
           05  RTRM11I PIC  X(0001).
      *    -------------------------------
           05  NUMB12L PIC S9(0004) COMP.
           05  NUMB12F PIC  X(0001).
           05  FILLER REDEFINES NUMB12F.
               10  NUMB12A PIC  X(0001).
           05  NUMB12I PIC  X(0004).
      *    -------------------------------
           05  CODE12L PIC S9(0004) COMP.
           05  CODE12F PIC  X(0001).
           05  FILLER REDEFINES CODE12F.
               10  CODE12A PIC  X(0001).
           05  CODE12I PIC  X(0002).
      *    -------------------------------
           05  TYPE12L PIC S9(0004) COMP.
           05  TYPE12F PIC  X(0001).
           05  FILLER REDEFINES TYPE12F.
               10  TYPE12A PIC  X(0001).
           05  TYPE12I PIC  X(0001).
      *    -------------------------------
           05  REVSN12L PIC S9(0004) COMP.
           05  REVSN12F PIC  X(0001).
           05  FILLER REDEFINES REVSN12F.
               10  REVSN12A PIC  X(0001).
           05  REVSN12I PIC  X(0003).
      *    -------------------------------
           05  RETRO12L PIC S9(0004) COMP.
           05  RETRO12F PIC  X(0001).
           05  FILLER REDEFINES RETRO12F.
               10  RETRO12A PIC  X(0001).
           05  RETRO12I PIC  X(0001).
      *    -------------------------------
           05  RTRM12L PIC S9(0004) COMP.
           05  RTRM12F PIC  X(0001).
           05  FILLER REDEFINES RTRM12F.
               10  RTRM12A PIC  X(0001).
           05  RTRM12I PIC  X(0001).
      *    -------------------------------
           05  NUMB13L PIC S9(0004) COMP.
           05  NUMB13F PIC  X(0001).
           05  FILLER REDEFINES NUMB13F.
               10  NUMB13A PIC  X(0001).
           05  NUMB13I PIC  X(0004).
      *    -------------------------------
           05  CODE13L PIC S9(0004) COMP.
           05  CODE13F PIC  X(0001).
           05  FILLER REDEFINES CODE13F.
               10  CODE13A PIC  X(0001).
           05  CODE13I PIC  X(0002).
      *    -------------------------------
           05  TYPE13L PIC S9(0004) COMP.
           05  TYPE13F PIC  X(0001).
           05  FILLER REDEFINES TYPE13F.
               10  TYPE13A PIC  X(0001).
           05  TYPE13I PIC  X(0001).
      *    -------------------------------
           05  REVSN13L PIC S9(0004) COMP.
           05  REVSN13F PIC  X(0001).
           05  FILLER REDEFINES REVSN13F.
               10  REVSN13A PIC  X(0001).
           05  REVSN13I PIC  X(0003).
      *    -------------------------------
           05  RETRO13L PIC S9(0004) COMP.
           05  RETRO13F PIC  X(0001).
           05  FILLER REDEFINES RETRO13F.
               10  RETRO13A PIC  X(0001).
           05  RETRO13I PIC  X(0001).
      *    -------------------------------
           05  RTRM13L PIC S9(0004) COMP.
           05  RTRM13F PIC  X(0001).
           05  FILLER REDEFINES RTRM13F.
               10  RTRM13A PIC  X(0001).
           05  RTRM13I PIC  X(0001).
      *    -------------------------------
           05  NUMB14L PIC S9(0004) COMP.
           05  NUMB14F PIC  X(0001).
           05  FILLER REDEFINES NUMB14F.
               10  NUMB14A PIC  X(0001).
           05  NUMB14I PIC  X(0004).
      *    -------------------------------
           05  CODE14L PIC S9(0004) COMP.
           05  CODE14F PIC  X(0001).
           05  FILLER REDEFINES CODE14F.
               10  CODE14A PIC  X(0001).
           05  CODE14I PIC  X(0002).
      *    -------------------------------
           05  TYPE14L PIC S9(0004) COMP.
           05  TYPE14F PIC  X(0001).
           05  FILLER REDEFINES TYPE14F.
               10  TYPE14A PIC  X(0001).
           05  TYPE14I PIC  X(0001).
      *    -------------------------------
           05  REVSN14L PIC S9(0004) COMP.
           05  REVSN14F PIC  X(0001).
           05  FILLER REDEFINES REVSN14F.
               10  REVSN14A PIC  X(0001).
           05  REVSN14I PIC  X(0003).
      *    -------------------------------
           05  RETRO14L PIC S9(0004) COMP.
           05  RETRO14F PIC  X(0001).
           05  FILLER REDEFINES RETRO14F.
               10  RETRO14A PIC  X(0001).
           05  RETRO14I PIC  X(0001).
      *    -------------------------------
           05  RTRM14L PIC S9(0004) COMP.
           05  RTRM14F PIC  X(0001).
           05  FILLER REDEFINES RTRM14F.
               10  RTRM14A PIC  X(0001).
           05  RTRM14I PIC  X(0001).
      *    -------------------------------
           05  NUMB15L PIC S9(0004) COMP.
           05  NUMB15F PIC  X(0001).
           05  FILLER REDEFINES NUMB15F.
               10  NUMB15A PIC  X(0001).
           05  NUMB15I PIC  X(0004).
      *    -------------------------------
           05  CODE15L PIC S9(0004) COMP.
           05  CODE15F PIC  X(0001).
           05  FILLER REDEFINES CODE15F.
               10  CODE15A PIC  X(0001).
           05  CODE15I PIC  X(0002).
      *    -------------------------------
           05  TYPE15L PIC S9(0004) COMP.
           05  TYPE15F PIC  X(0001).
           05  FILLER REDEFINES TYPE15F.
               10  TYPE15A PIC  X(0001).
           05  TYPE15I PIC  X(0001).
      *    -------------------------------
           05  REVSN15L PIC S9(0004) COMP.
           05  REVSN15F PIC  X(0001).
           05  FILLER REDEFINES REVSN15F.
               10  REVSN15A PIC  X(0001).
           05  REVSN15I PIC  X(0003).
      *    -------------------------------
           05  RETRO15L PIC S9(0004) COMP.
           05  RETRO15F PIC  X(0001).
           05  FILLER REDEFINES RETRO15F.
               10  RETRO15A PIC  X(0001).
           05  RETRO15I PIC  X(0001).
      *    -------------------------------
           05  RTRM15L PIC S9(0004) COMP.
           05  RTRM15F PIC  X(0001).
           05  FILLER REDEFINES RTRM15F.
               10  RTRM15A PIC  X(0001).
           05  RTRM15I PIC  X(0001).
      *    -------------------------------
           05  NUMB16L PIC S9(0004) COMP.
           05  NUMB16F PIC  X(0001).
           05  FILLER REDEFINES NUMB16F.
               10  NUMB16A PIC  X(0001).
           05  NUMB16I PIC  X(0004).
      *    -------------------------------
           05  CODE16L PIC S9(0004) COMP.
           05  CODE16F PIC  X(0001).
           05  FILLER REDEFINES CODE16F.
               10  CODE16A PIC  X(0001).
           05  CODE16I PIC  X(0002).
      *    -------------------------------
           05  TYPE16L PIC S9(0004) COMP.
           05  TYPE16F PIC  X(0001).
           05  FILLER REDEFINES TYPE16F.
               10  TYPE16A PIC  X(0001).
           05  TYPE16I PIC  X(0001).
      *    -------------------------------
           05  REVSN16L PIC S9(0004) COMP.
           05  REVSN16F PIC  X(0001).
           05  FILLER REDEFINES REVSN16F.
               10  REVSN16A PIC  X(0001).
           05  REVSN16I PIC  X(0003).
      *    -------------------------------
           05  RETRO16L PIC S9(0004) COMP.
           05  RETRO16F PIC  X(0001).
           05  FILLER REDEFINES RETRO16F.
               10  RETRO16A PIC  X(0001).
           05  RETRO16I PIC  X(0001).
      *    -------------------------------
           05  RTRM16L PIC S9(0004) COMP.
           05  RTRM16F PIC  X(0001).
           05  FILLER REDEFINES RTRM16F.
               10  RTRM16A PIC  X(0001).
           05  RTRM16I PIC  X(0001).
      *    -------------------------------
           05  NUMB17L PIC S9(0004) COMP.
           05  NUMB17F PIC  X(0001).
           05  FILLER REDEFINES NUMB17F.
               10  NUMB17A PIC  X(0001).
           05  NUMB17I PIC  X(0004).
      *    -------------------------------
           05  CODE17L PIC S9(0004) COMP.
           05  CODE17F PIC  X(0001).
           05  FILLER REDEFINES CODE17F.
               10  CODE17A PIC  X(0001).
           05  CODE17I PIC  X(0002).
      *    -------------------------------
           05  TYPE17L PIC S9(0004) COMP.
           05  TYPE17F PIC  X(0001).
           05  FILLER REDEFINES TYPE17F.
               10  TYPE17A PIC  X(0001).
           05  TYPE17I PIC  X(0001).
      *    -------------------------------
           05  REVSN17L PIC S9(0004) COMP.
           05  REVSN17F PIC  X(0001).
           05  FILLER REDEFINES REVSN17F.
               10  REVSN17A PIC  X(0001).
           05  REVSN17I PIC  X(0003).
      *    -------------------------------
           05  RETRO17L PIC S9(0004) COMP.
           05  RETRO17F PIC  X(0001).
           05  FILLER REDEFINES RETRO17F.
               10  RETRO17A PIC  X(0001).
           05  RETRO17I PIC  X(0001).
      *    -------------------------------
           05  RTRM17L PIC S9(0004) COMP.
           05  RTRM17F PIC  X(0001).
           05  FILLER REDEFINES RTRM17F.
               10  RTRM17A PIC  X(0001).
           05  RTRM17I PIC  X(0001).
      *    -------------------------------
           05  NUMB18L PIC S9(0004) COMP.
           05  NUMB18F PIC  X(0001).
           05  FILLER REDEFINES NUMB18F.
               10  NUMB18A PIC  X(0001).
           05  NUMB18I PIC  X(0004).
      *    -------------------------------
           05  CODE18L PIC S9(0004) COMP.
           05  CODE18F PIC  X(0001).
           05  FILLER REDEFINES CODE18F.
               10  CODE18A PIC  X(0001).
           05  CODE18I PIC  X(0002).
      *    -------------------------------
           05  TYPE18L PIC S9(0004) COMP.
           05  TYPE18F PIC  X(0001).
           05  FILLER REDEFINES TYPE18F.
               10  TYPE18A PIC  X(0001).
           05  TYPE18I PIC  X(0001).
      *    -------------------------------
           05  REVSN18L PIC S9(0004) COMP.
           05  REVSN18F PIC  X(0001).
           05  FILLER REDEFINES REVSN18F.
               10  REVSN18A PIC  X(0001).
           05  REVSN18I PIC  X(0003).
      *    -------------------------------
           05  RETRO18L PIC S9(0004) COMP.
           05  RETRO18F PIC  X(0001).
           05  FILLER REDEFINES RETRO18F.
               10  RETRO18A PIC  X(0001).
           05  RETRO18I PIC  X(0001).
      *    -------------------------------
           05  RTRM18L PIC S9(0004) COMP.
           05  RTRM18F PIC  X(0001).
           05  FILLER REDEFINES RTRM18F.
               10  RTRM18A PIC  X(0001).
           05  RTRM18I PIC  X(0001).
      *    -------------------------------
           05  NUMB19L PIC S9(0004) COMP.
           05  NUMB19F PIC  X(0001).
           05  FILLER REDEFINES NUMB19F.
               10  NUMB19A PIC  X(0001).
           05  NUMB19I PIC  X(0004).
      *    -------------------------------
           05  CODE19L PIC S9(0004) COMP.
           05  CODE19F PIC  X(0001).
           05  FILLER REDEFINES CODE19F.
               10  CODE19A PIC  X(0001).
           05  CODE19I PIC  X(0002).
      *    -------------------------------
           05  TYPE19L PIC S9(0004) COMP.
           05  TYPE19F PIC  X(0001).
           05  FILLER REDEFINES TYPE19F.
               10  TYPE19A PIC  X(0001).
           05  TYPE19I PIC  X(0001).
      *    -------------------------------
           05  REVSN19L PIC S9(0004) COMP.
           05  REVSN19F PIC  X(0001).
           05  FILLER REDEFINES REVSN19F.
               10  REVSN19A PIC  X(0001).
           05  REVSN19I PIC  X(0003).
      *    -------------------------------
           05  RETRO19L PIC S9(0004) COMP.
           05  RETRO19F PIC  X(0001).
           05  FILLER REDEFINES RETRO19F.
               10  RETRO19A PIC  X(0001).
           05  RETRO19I PIC  X(0001).
      *    -------------------------------
           05  RTRM19L PIC S9(0004) COMP.
           05  RTRM19F PIC  X(0001).
           05  FILLER REDEFINES RTRM19F.
               10  RTRM19A PIC  X(0001).
           05  RTRM19I PIC  X(0001).
      *    -------------------------------
           05  NUMB20L PIC S9(0004) COMP.
           05  NUMB20F PIC  X(0001).
           05  FILLER REDEFINES NUMB20F.
               10  NUMB20A PIC  X(0001).
           05  NUMB20I PIC  X(0004).
      *    -------------------------------
           05  CODE20L PIC S9(0004) COMP.
           05  CODE20F PIC  X(0001).
           05  FILLER REDEFINES CODE20F.
               10  CODE20A PIC  X(0001).
           05  CODE20I PIC  X(0002).
      *    -------------------------------
           05  TYPE20L PIC S9(0004) COMP.
           05  TYPE20F PIC  X(0001).
           05  FILLER REDEFINES TYPE20F.
               10  TYPE20A PIC  X(0001).
           05  TYPE20I PIC  X(0001).
      *    -------------------------------
           05  REVSN20L PIC S9(0004) COMP.
           05  REVSN20F PIC  X(0001).
           05  FILLER REDEFINES REVSN20F.
               10  REVSN20A PIC  X(0001).
           05  REVSN20I PIC  X(0003).
      *    -------------------------------
           05  RETRO20L PIC S9(0004) COMP.
           05  RETRO20F PIC  X(0001).
           05  FILLER REDEFINES RETRO20F.
               10  RETRO20A PIC  X(0001).
           05  RETRO20I PIC  X(0001).
      *    -------------------------------
           05  RTRM20L PIC S9(0004) COMP.
           05  RTRM20F PIC  X(0001).
           05  FILLER REDEFINES RTRM20F.
               10  RTRM20A PIC  X(0001).
           05  RTRM20I PIC  X(0001).
      *    -------------------------------
           05  SELCDEL PIC S9(0004) COMP.
           05  SELCDEF PIC  X(0001).
           05  FILLER REDEFINES SELCDEF.
               10  SELCDEA PIC  X(0001).
           05  SELCDEI PIC  99.
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0075).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0075).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  99.
      *    -------------------------------
           05  PF1KEYL PIC S9(0004) COMP.
           05  PF1KEYF PIC  X(0001).
           05  FILLER REDEFINES PF1KEYF.
               10  PF1KEYA PIC  X(0001).
           05  PF1KEYI PIC  X(0018).
      *    -------------------------------
           05  PF2KEYL PIC S9(0004) COMP.
           05  PF2KEYF PIC  X(0001).
           05  FILLER REDEFINES PF2KEYF.
               10  PF2KEYA PIC  X(0001).
           05  PF2KEYI PIC  X(0018).
       01  EL6505AO REDEFINES EL6505AI.
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
           05  MAINTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USECODEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO3O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO4O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB01O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB02O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB03O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB04O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB05O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB06O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB07O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB08O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB09O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE09O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN9O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM09O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB10O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN10O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB11O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN11O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB12O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN12O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB13O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN13O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB14O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN14O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB15O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN15O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB16O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE16O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN16O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB17O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE17O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN17O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB18O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE18O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN18O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB19O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE19O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE19O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN19O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO19O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM19O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NUMB20O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CODE20O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE20O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVSN20O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RETRO20O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTRM20O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SELCDEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF1KEYO PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF2KEYO PIC  X(0018).
      *    -------------------------------
00201  01  EL6505AO-R                  REDEFINES
00202      EL6505AI.
101101     12  FILLER                      PIC X(144).
00204      12  BENEFIT-TABLE           OCCURS 20 TIMES
00205                                           INDEXED BY BNDX.
00206          16  BENCODE-L               PIC S9(4)       COMP.
00207          16  BENCODE-A               PIC X.
00208          16  BENCODE                 PIC XX.
00209          16  BENTYPE-L               PIC S9(4)       COMP.
00210          16  BENTYPE-A               PIC X.
00211          16  BENTYPE                 PIC X.
00212          16  REVNO-L                 PIC S9(4)       COMP.
00213          16  REVNO-A                 PIC X.
00214          16  REVNO                   PIC XXX.
00215          16  RETROYN-L               PIC S9(4)       COMP.
00216          16  RETROYN-A               PIC X.
00217          16  RETROYN                 PIC X.
00218          16  REMTERM-L               PIC S9(4)       COMP.
00219          16  REMTERM-A               PIC X.
00220          16  REMTERM                 PIC X.
00221          16  FILLER                  PIC X(7).
00222
00223      EJECT
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
00225  01  DFHCOMMAREA                     PIC X(1500).
00226
00227      EJECT
00228 *01 PARMLIST .
00229 *    02  FILLER                      PIC S9(8)   COMP.
00230 *    02  ERACCT-POINTER              PIC S9(8)   COMP.
00231 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.
00232
00233 *                                    COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030211* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
101711* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
021916* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
030211     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
030211         16  FILLER                        PIC X(10).
030211         16  AM-VG-KEY3.
030211             20  AM-VG3-ACCOUNT            PIC X(10).
030211             20  AM-VG3-EXP-DT             PIC XX.
030211         16  FILLER                        PIC X(4).
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
031811         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
021916         88  AM-ACCOUNT-DROPPED               VALUE '6'.
021916         88  AM-ACCOUNT-LAPSED                VALUE '7'.
021916         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
021916         88  AM-ACCOUNT-PENDING               VALUE '9'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
           12  AM-DCC-UEF-STATE                  PIC XX.
           12  FILLER                            PIC XXX.
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
00234      EJECT
00235 *                                    COPY ELCCNTL.
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
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
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
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
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
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
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
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
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
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
012913         16  FILLER                         PIC X(181).
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
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
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
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
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
00236      EJECT
00237
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA ACCOUNT-MASTER
                                CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6505' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00239
00240      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00241      MOVE '5'                    TO  DC-OPTION-CODE.
00242      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00243      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00244      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00245      MOVE DC-GREG-DATE-1-YMD     TO  YMD-CURRENT-SAVE.
00246
00247      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00248      MOVE 2                      TO  EMI-NUMBER-OF-LINES
00249                                      EMI-SWITCH2.
00250
00251      MOVE EIBTRMID               TO  QID1-TERM
00252                                      QID2-TERM.
00253
00254      IF EIBCALEN = 0
00255          GO TO 8800-UNAUTHORIZED-ACCESS.
00256
00257      IF PI-COMPANY-ID EQUAL 'DMD'
00258          IF PI-CALLING-PROGRAM NOT = THIS-PGM
00259              IF PI-CALLING-PROGRAM = XCTL-6507
00260                  IF PI-DMD-SCREEN EQUAL '2'
00261                      MOVE +0         TO PI-DMD-OCCURS
00262                  ELSE
00263                      IF PI-DMD-SCREEN EQUAL '3'
00264                          MOVE +20    TO PI-DMD-OCCURS
00265                      ELSE
00266                          MOVE 'F'    TO PI-DMD-FILE-SW
00267                          MOVE +0     TO PI-DMD-OCCURS
00268                          MOVE '1'    TO PI-DMD-SCREEN
00269              ELSE
00270                  MOVE 'F'            TO PI-DMD-FILE-SW
00271                  MOVE +0             TO PI-DMD-OCCURS
00272                  MOVE '1'            TO PI-DMD-SCREEN.
00273
00274      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00275          IF PI-CALLING-PROGRAM = XCTL-6501
00276              MOVE PI-CALLING-PROGRAM
00277                                  TO  PI-RETURN-TO-PROGRAM
00278              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM
00279          ELSE
00280              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM.
00281
00282      MOVE LOW-VALUES             TO  EL6505AI.
00283
00284      IF EIBTRNID NOT = TRANS-ID
00285          MOVE PI-MAINT           TO  MAINTYPO
00286          MOVE AL-UANON           TO  MAINTYPA
00287          MOVE -1                 TO  MAINTYPL
00288         IF PI-MAINT = 'S' OR 'C'
00289             GO TO 4000-SHOW
00290          ELSE
00291             IF PI-MAINT = 'A'
00292                 MOVE 'C'        TO  PI-MAINT
00293                 GO TO 4000-SHOW
00294              ELSE
00295                 GO TO 8100-SEND-INITIAL-MAP.
00296
00297      
      * EXEC CICS HANDLE CONDITION
00298 *        PGMIDERR  (9600-PGMID-ERROR)
00299 *        ERROR     (9990-ABEND)
00300 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00004549' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034353439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00301
00302      IF EIBAID = DFHCLEAR
00303          GO TO 9400-CLEAR.
00304
00305      EJECT
00306  0200-RECEIVE.
00307      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00308          MOVE ER-0008            TO  EMI-ERROR
00309          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00310          MOVE -1                 TO  PFENTERL
00311          GO TO 8200-SEND-DATAONLY.
00312
00313      
      * EXEC CICS RECEIVE
00314 *        MAP      (MAP-NAME)
00315 *        MAPSET   (MAPSET-NAME)
00316 *        INTO     (EL6505AI)
00317 *    END-EXEC.
           MOVE LENGTH OF
            EL6505AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004565' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6505AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00318
00319      IF PFENTERL = 0
00320          GO TO 0300-CHECK-PFKEYS.
00321
00322      IF EIBAID NOT = DFHENTER
00323          MOVE ER-0004            TO  EMI-ERROR
00324          GO TO 0320-INPUT-ERROR.
00325
00326      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)
00327          MOVE PF-VALUES (PFENTERI)   TO  EIBAID
00328      ELSE
00329          MOVE ER-0029                TO  EMI-ERROR
00330          GO TO 0320-INPUT-ERROR.
00331
00332      EJECT
00333  0300-CHECK-PFKEYS.
00334
00335      IF EIBAID = DFHPF23
00336          GO TO 8810-PF23.
00337      IF EIBAID = DFHPF24
00338          GO TO 9200-RETURN-MAIN-MENU.
00339      IF EIBAID = DFHPF12
00340          GO TO 9500-PF12.
00341
00342      IF PI-COMPANY-ID EQUAL 'DMD'
00343          IF EIBAID EQUAL DFHPF1
00344             IF FIRST-OCCURS
00345                 MOVE 'I'         TO PI-DMD-FILE-SW
00346                 MOVE +0          TO PI-DMD-OCCURS
00347                 MOVE '2'         TO PI-DMD-SCREEN
00348                 GO TO 4000-SHOW
00349             ELSE
00350                 IF PI-DMD-OCCURS EQUAL  +0
00351                     MOVE '2'     TO PI-DMD-SCREEN
00352                     GO TO 4000-SHOW
00353                 ELSE
00354                     MOVE 'E'     TO PI-DMD-FILE-SW
00355                     MOVE '3'     TO PI-DMD-SCREEN
00356                     MOVE +20     TO PI-DMD-OCCURS
00357                     GO TO 4000-SHOW.
00358
00359      IF PI-COMPANY-ID EQUAL 'DMD'
00360          IF EIBAID EQUAL DFHPF2
00361             IF  FIRST-OCCURS
00362                 MOVE ER-8149     TO  EMI-ERROR
00363                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00364                 GO TO 4000-SHOW
00365             ELSE
00366                 IF (INTO-NEXT-BENEFITS AND
00367                    PI-DMD-OCCURS EQUAL +20)
00368                 MOVE 'F'         TO  PI-DMD-FILE-SW
00369                 MOVE ER-8149     TO  EMI-ERROR
00370                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00371                 GO TO 4000-SHOW
00372             ELSE
00373                 IF (INTO-NEXT-BENEFITS AND
00374                    PI-DMD-OCCURS NOT LESS THAN +20)
00375                     MOVE +0          TO  PI-DMD-OCCURS
00376                     GO TO 4000-SHOW
00377                 ELSE
00378                     IF END-OF-FILE
00379                         MOVE 'I'         TO  PI-DMD-FILE-SW
00380                         MOVE +0          TO  PI-DMD-OCCURS
00381                         GO TO 4000-SHOW.
00382
00387      IF EIBAID = DFHPF5
00388          MOVE XCTL-6502          TO  PGM-NAME
00389          GO TO 9300-XCTL.
00390
00391      IF EIBAID = DFHPF7
00392          MOVE XCTL-6504          TO  PGM-NAME
00393          GO TO 9300-XCTL.
00394
00395      IF EIBAID = DFHPF8
00396          MOVE XCTL-6506          TO  PGM-NAME
00397          GO TO 9300-XCTL.
00398
00399      IF EIBAID = DFHPF9
00400          MOVE XCTL-6501          TO  PGM-NAME
00401          GO TO 9300-XCTL.
00402
00403      IF EIBAID EQUAL DFHENTER
00404         IF PI-COMPANY-ID EQUAL 'DMD'
00405             IF SELCDEL GREATER THAN +0
00406                IF (SELCDEI NUMERIC) AND
00407                   (SELCDEI GREATER THAN 00 AND
00408                   SELCDEI LESS THAN 21)
00409                      NEXT SENTENCE
00410                ELSE
00411                    MOVE SELCDEI  TO WS-SELECT-LINE
00412                    IF WS-SELECT-LINE LESS THAN 41
00413                       ADD -20         TO WS-SELECT-LINE
00414                       MOVE WS-SELECT-LINE TO SELCDEI
00415                    ELSE
00416                       ADD -40         TO WS-SELECT-LINE
00417                       MOVE WS-SELECT-LINE TO SELCDEI.
00418
00419
00420      IF EIBAID EQUAL DFHENTER
00421         IF SELCDEL GREATER THAN +0
00422            IF (SELCDEI NUMERIC) AND
00423               (SELCDEI GREATER THAN 00 AND
00424               SELCDEI LESS THAN 21)
00425               MOVE XCTL-6507     TO  PGM-NAME
00426               MOVE PI-ACCT-CCGSA-KEY
00427                                  TO PI-PLAN-KEY
00428               MOVE BENCODE (SELCDEI)
00429                                  TO PI-PLAN-BEN
00430               MOVE BENTYPE (SELCDEI)
00431                                  TO PI-PLAN-BEN-TYPE
00432               MOVE REVNO (SELCDEI)
00433                                  TO PI-PLAN-REVISION
00434               GO TO 9300-XCTL
00435            ELSE
00436               MOVE -1            TO SELCDEL
00437               MOVE AL-UNBON      TO SELCDEA
00438               MOVE ER-2946       TO EMI-ERROR
00439               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00440               GO TO 8200-SEND-DATAONLY.
00441
00442      IF EIBAID = DFHENTER
00443          GO TO 0330-CHECK-MAINTYP.
00444
00445      MOVE ER-0029                TO  EMI-ERROR.
00446  EJECT
00447  0320-INPUT-ERROR.
00448
00449      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00450      MOVE AL-UNBON               TO  PFENTERA.
00451      MOVE -1                     TO  PFENTERL.
00452      GO TO 8200-SEND-DATAONLY.
00453
00454  0330-CHECK-MAINTYP.
00455
00456      IF MAINTYPL GREATER ZERO
00457          IF MAINTYPI = 'S' OR 'C' OR 'A'
00458              MOVE AL-UANON       TO  MAINTYPA
00459              MOVE MAINTYPI       TO  PI-MAINT
00460          ELSE
00461              MOVE -1             TO  MAINTYPL
00462              MOVE AL-UABON       TO  MAINTYPA
00463              MOVE ER-2039        TO  EMI-ERROR
00464              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00465              GO TO 8200-SEND-DATAONLY
00466      ELSE
00467          MOVE -1                 TO  MAINTYPL
00468          MOVE AL-UABON           TO  MAINTYPA
00469          MOVE ER-2039            TO  EMI-ERROR
00470          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00471          GO TO 8200-SEND-DATAONLY.
00472
00473      IF PI-MAINT = 'S'
00474          IF PI-COMPANY-ID EQUAL 'DMD'
00475              IF SCREEN-1-DISPLAYED
00476                  MOVE 'F'       TO PI-DMD-FILE-SW
00477                  MOVE +0        TO PI-DMD-OCCURS
00478                  GO TO 4000-SHOW
00479              ELSE
00480                  IF SCREEN-2-DISPLAYED
00481                      MOVE +0        TO PI-DMD-OCCURS
00482                      GO TO 4000-SHOW
00483                  ELSE
00484                     IF SCREEN-3-DISPLAYED
00485                        MOVE +20     TO PI-DMD-OCCURS
00486                        GO TO 4000-SHOW
00487          ELSE
00488              GO TO 4000-SHOW.
00489
00490      PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.
00491
00492      IF EMI-ERROR NOT = ZEROS
00493          MOVE -1                 TO  MAINTYPL
00494          GO TO 8200-SEND-DATAONLY.
00495
00496      GO TO 4200-MAINT.
00497
00498      EJECT
00499  4000-SHOW.
00500      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.
00501      MOVE LOW-VALUES             TO  EL6505AO.
00502      GO TO 5000-BUILD-INITIAL-SCREEN.
00503
00504      EJECT
00505
00506  4200-MAINT.
00507      IF NOT MODIFY-CAP
00508          MOVE 'UPDATE'       TO SM-READ
00509          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00510          MOVE ER-0070             TO  EMI-ERROR
00511          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00512          GO TO 8100-SEND-INITIAL-MAP.
00513
00514      PERFORM 7000-EDIT THRU 7099-EXIT.
00515
00516      IF EMI-NO-ERRORS
00517          NEXT SENTENCE
00518      ELSE
00519          GO TO 8200-SEND-DATAONLY.
00520
00521      PERFORM 7300-READ-ERACCT-UPDATE THRU 7300-EXIT.
00522
00523      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
00524      MOVE ERACCT-FILE            TO  FILE-ID.
00525
00526      IF PI-COMPANY-ID EQUAL 'DMD'
00527          MOVE SPACES           TO WS-DMD-WORK-BENEFIT-TABLE
00528                                   WS-DMD-WORK-BENEFIT-TABLE-2
00529          MOVE +1                       TO SUB1
00530                                           SUB2
00531          PERFORM 6700-LOAD-BENEFIT-CODES
00532                                        THRU 6700-LOAD-EXIT
00533          PERFORM 6600-CHECK-FOR-UPDATE
00534                                        THRU 6649-EXIT
00535          PERFORM 6800-SORT-BENEFITS    THRU 6899-EXIT
00536          MOVE +1                       TO SUB1
00537                                           SUB2
00538          PERFORM 6900-LOAD-ACCOUNT-MASTER
00539                                        THRU 6900-LOAD-EXIT
00540      ELSE
00541          PERFORM 6000-CHECK-FOR-UPDATE THRU 6049-EXIT
00542          PERFORM 6500-SORT-BENEFITS    THRU 6599-EXIT.
00543
00544      IF AM-LAST-MAINT-USER   = PI-UPDATE-BY OR
00545         AM-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00546          NEXT SENTENCE
00547      ELSE
00548          
      * EXEC CICS UNLOCK
00549 *             DATASET  (ERACCT-FILE)
00550 *        END-EXEC
      *    MOVE '&*                    #   #00004796' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00551          MOVE ER-0068            TO  EMI-ERROR
00552          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00553          PERFORM 7100-READ-ERACCT  THRU 7100-EXIT
00554          MOVE LOW-VALUES         TO  EL6505AO
00555          MOVE -1                 TO  MAINTYPL
00556          MOVE 'S'                TO  PI-MAINT
00557          GO TO 5000-BUILD-INITIAL-SCREEN.
00558
00559      MOVE PI-PROCESSOR-ID        TO  AM-LAST-MAINT-USER.
00560      MOVE EIBTIME                TO  AM-LAST-MAINT-HHMMSS.
00561      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00562      MOVE '5'                    TO  DC-OPTION-CODE.
00563      MOVE LINK-ELDATCV           TO  PGM-NAME.
00564
00565      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00566
00567      MOVE DC-BIN-DATE-1          TO  AM-LAST-MAINT-DT
00568                                      BIN-CURRENT-SAVE.
00569      MOVE ERACCT-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
00570      MOVE 'B'                    TO  JP-RECORD-TYPE.
00571      PERFORM 8400-LOG-JOURNAL-RECORD.
00572      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
00573
00574      
      * EXEC CICS REWRITE
00575 *        DATASET  (ERACCT-FILE)
00576 *        FROM     (ACCOUNT-MASTER)
00577 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004822' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00578
00579      MOVE 'C'                    TO  JP-RECORD-TYPE.
00580      MOVE ERACCT-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
00581      MOVE ERACCT-FILE            TO  FILE-ID.
00582      PERFORM 8400-LOG-JOURNAL-RECORD.
00583      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00584      MOVE ER-0000                TO  EMI-ERROR.
00585      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00586
00587      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.
00588      MOVE LOW-VALUES             TO  EL6505AO.
00589      MOVE 'C'                    TO  PI-MAINT.
00590
00591      EJECT
00592  5000-BUILD-INITIAL-SCREEN.
00593      MOVE AM-CARRIER             TO  CARO.
00594      MOVE AM-GROUPING            TO  GROUPO.
00595      MOVE AM-STATE               TO  STATEO
00596                                      PI-WS-STATE.
00597      MOVE AM-ACCOUNT             TO  ACCTO.
00598
00599      MOVE AM-EFFECTIVE-DT        TO  DC-BIN-DATE-1.
00600      MOVE ' '                    TO  DC-OPTION-CODE.
00601
00602      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00603
00604      MOVE DC-GREG-DATE-1-EDIT    TO  EFFDTEO.
00605
00606      IF AM-EXPIRATION-DT NOT = HIGH-VALUES
00607         MOVE AM-EXPIRATION-DT    TO  DC-BIN-DATE-1
00608         MOVE ' '                 TO  DC-OPTION-CODE
00609         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00610         MOVE DC-GREG-DATE-1-EDIT TO  EXPDTEO
00611      ELSE
00612         MOVE '99/99/99'          TO  EXPDTEO.
00613
00614      MOVE AM-BENEFIT-TABLE-USAGE TO  USECODEO.
00615      MOVE AL-UANON               TO  USECODEA.
00616
00617      MOVE +0                     TO  SUB1.
00618      SET BNDX                    TO  SUB1.
00619
00620      IF PI-COMPANY-ID EQUAL 'DMD'
00621         IF FIRST-OCCURS
00622             NEXT SENTENCE
00623         ELSE
00624             IF END-OF-FILE
00625                 MOVE ER-8150     TO  EMI-ERROR
00626                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00627                 MOVE +0          TO SUB1
00628                 SET BNDX TO SUB1
00629                 MOVE +20         TO SUB1
00630                 MOVE '3'         TO PI-DMD-SCREEN
00631                 GO TO 5125-DMD-MOVE-EXTRA
00632             ELSE
00633                 MOVE +0          TO SUB1
00634                 SET BNDX TO SUB1
00635                 MOVE PI-DMD-OCCURS   TO SUB1
00636                 MOVE '2'             TO PI-DMD-SCREEN
00637                 GO TO 5125-DMD-MOVE-EXTRA.
00638
00639  5025-SET-UP-BENEFITS.
00640      SET BNDX UP BY +1.
00641      ADD +1                      TO  SUB1.
00642      MOVE '1'                    TO PI-DMD-SCREEN.
00643
00644      IF BNDX GREATER +20
00645          GO TO 5050-CONT.
00646
00647      IF AM-BENEFIT-CODE (SUB1) = SPACES  OR  ZEROS
00648          GO TO 5025-SET-UP-BENEFITS.
00649
00650      MOVE AM-BENEFIT-CODE (SUB1)         TO  BENCODE (BNDX).
00651      MOVE AM-BENEFIT-TYPE (SUB1)         TO  BENTYPE (BNDX).
00652      MOVE AM-BENEFIT-REVISION (SUB1)     TO  REVNO   (BNDX).
00653
00654      IF PI-COMPANY-ID = 'DMD'
00655         MOVE AM-BENEFIT-RETRO-Y-N (SUB1) TO  RETROYN (BNDX)
00656      ELSE
00657         IF AM-BENEFIT-RETRO-Y-N (SUB1) = 'N'
00658            MOVE 'N'                      TO  RETROYN (BNDX)
00659         ELSE
00660            MOVE 'Y'                      TO  RETROYN (BNDX).
00661
00662      MOVE AM-BENEFIT-REM-TERM (SUB1)     TO  REMTERM (BNDX).
00663
00664      MOVE AL-UANON               TO  BENCODE-A (BNDX)
00665                                      BENTYPE-A (BNDX)
00666                                      REVNO-A   (BNDX)
00667                                      RETROYN-A (BNDX)
00668                                      REMTERM-A (BNDX).
00669
00670      GO TO 5025-SET-UP-BENEFITS.
00671
00672  5050-CONT.
00673      IF PI-COMPANY-ID EQUAL 'DMD'
00674          MOVE +0                 TO  PI-DMD-OCCURS
00675          MOVE 'I'                TO  PI-DMD-FILE-SW.
00676
00677      MOVE PI-MAINT               TO  MAINTYPO.
00678      MOVE AL-UANON               TO  MAINTYPA.
00679      MOVE -1                     TO  MAINTYPL.
00680
00681      GO TO 8100-SEND-INITIAL-MAP.
00682
00683  5099-EXIT.
00684      EXIT.
00685      EJECT
00686  5125-DMD-MOVE-EXTRA.
00687
00688      SET BNDX UP BY +1.
00689      ADD +1                      TO  SUB1.
00690      ADD +1                      TO  PI-DMD-OCCURS.
00691
00692      IF BNDX GREATER +20
00693          SUBTRACT +1             FROM PI-DMD-OCCURS
00694          GO TO 5150-CONT.
00695
00696      IF PI-DMD-OCCURS GREATER +30
00697          MOVE +19                TO PI-DMD-OCCURS
00698          MOVE 'E'                TO PI-DMD-FILE-SW
00699          MOVE ER-8150            TO  EMI-ERROR
00700          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00701          GO TO 5150-CONT.
00702
00703      IF AM-BENEFIT-DMD-CODE (SUB1) = SPACES  OR  ZEROS
00704          GO TO 5125-DMD-MOVE-EXTRA.
00705
00706      MOVE AM-BENEFIT-DMD-CODE (SUB1)     TO  BENCODE (BNDX).
00707      MOVE AM-BENEFIT-DMD-TYPE (SUB1)     TO  BENTYPE (BNDX).
00708      MOVE AM-BENEFIT-DMD-REVISION (SUB1) TO  REVNO   (BNDX).
00709
00710      MOVE AM-BENEFIT-DMD-RETRO-Y-N (SUB1) TO  RETROYN (BNDX).
00711
00712      MOVE AM-BENEFIT-DMD-REM-TERM (SUB1)  TO  REMTERM (BNDX).
00713
00714      MOVE AL-UANON               TO  BENCODE-A (BNDX)
00715                                      BENTYPE-A (BNDX)
00716                                      REVNO-A   (BNDX)
00717                                      RETROYN-A (BNDX)
00718                                      REMTERM-A (BNDX).
00719
00720      GO TO 5125-DMD-MOVE-EXTRA.
00721
00722  5150-CONT.
00723      MOVE PI-MAINT               TO  MAINTYPO.
00724      MOVE AL-UANON               TO  MAINTYPA.
00725      MOVE -1                     TO  MAINTYPL.
00726
00727      GO TO 8100-SEND-INITIAL-MAP.
00728
00729  5199-EXIT.
00730      EXIT.
00731      EJECT
00732
00733  6000-CHECK-FOR-UPDATE.
00734
00735      IF USECODEL GREATER +0
00736          MOVE USECODEI         TO  AM-BENEFIT-TABLE-USAGE.
00737
00738      MOVE AL-UANON             TO  USECODEA.
00739
00740      MOVE +0                   TO  SUB1
00741      SET BNDX                  TO  SUB1.
00742
00743  6005-CHECK-BENEFITS.
00744
00745      SET BNDX UP BY +1.
00746      ADD +1                    TO  SUB1.
00747
00748      IF BNDX GREATER +20
00749          GO TO 6049-EXIT.
00750
00751      IF BENCODE-L (BNDX) NOT GREATER THAN +0
00752         GO TO 6030-BYPASS-BENEFIT-CHECK.
00753
00754      IF BENCODE-L (BNDX) GREATER THAN +0
00755         IF BENCODE (BNDX) EQUAL '91' OR '92' OR '93' OR '94'
00756                              OR '98' OR '99' OR '  ' OR '00'
00757            GO TO 6030-BYPASS-BENEFIT-CHECK.
00758
00759  6010-READ-BEN-CNTL.
00760
00761      MOVE SPACES                 TO ELCNTL-KEY.
00762      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
00763
00764      IF BENTYPE (BNDX) EQUAL PI-LIFE-OVERRIDE-L1
00765         MOVE '4'                 TO CNTL-REC-TYPE
00766      ELSE
00767         MOVE '5'                 TO CNTL-REC-TYPE.
00768
00769      MOVE BENCODE (BNDX)         TO CNTL-HI-BEN
00770      MOVE +0                     TO CNTL-SEQ-NO.
00771
00772      
      * EXEC CICS HANDLE CONDITION
00773 *        NOTFND   (6025-NOT-FOUND)
00774 *        ENDFILE  (6025-NOT-FOUND)
00775 *    END-EXEC.
      *    MOVE '"$I''                  ! # #00005020' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035303230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00776
00777      
      * EXEC CICS READ
00778 *        DATASET   (CNTL-FILE-ID)
00779 *        SET       (ADDRESS OF CONTROL-FILE)
00780 *        RIDFLD    (ELCNTL-KEY)
00781 *        GTEQ
00782 *    END-EXEC.
      *    MOVE '&"S        G          (   #00005025' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303235' TO DFHEIV0(25:11)
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
           
00783
00784  6015-BYPASS-ELCNTL-READ.
00785
00786      PERFORM 6020-BENEFIT-LOOP-DUMMY
00787        VARYING SUB2 FROM +1 BY +1 UNTIL
00788         CF-BENEFIT-CODE (SUB2) EQUAL BENCODE (BNDX) OR
00789          SUB2 GREATER THAN +8.
00790
00791      IF SUB2 GREATER THAN +8
00792         GO TO 6025-NOT-FOUND.
00793
00794      GO TO 6030-BYPASS-BENEFIT-CHECK.
00795
00796  6020-BENEFIT-LOOP-DUMMY.
00797
00798  6025-NOT-FOUND.
00799
00800      MOVE -1                     TO  BENCODE-L (BNDX)
00801      MOVE AL-UABON               TO  BENCODE-A (BNDX)
00802      MOVE ER-0150                TO  EMI-ERROR
00803      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00804      GO TO 8200-SEND-DATAONLY.
00805
00806  6030-BYPASS-BENEFIT-CHECK.
00807
00808      IF BENCODE-L (BNDX) GREATER +0
00809          MOVE BENCODE (BNDX)   TO  AM-BENEFIT-CODE (SUB1).
00810
00811      IF BENTYPE-L (BNDX) GREATER +0
00812          MOVE BENTYPE (BNDX)   TO  AM-BENEFIT-TYPE (SUB1).
00813
00814      IF REVNO-L (BNDX) GREATER +0
00815          MOVE REVNO (BNDX)     TO  AM-BENEFIT-REVISION (SUB1).
00816
00817      IF RETROYN-L (BNDX) GREATER +0
00818          MOVE RETROYN (BNDX)   TO  AM-BENEFIT-RETRO-Y-N (SUB1).
00819
00820      IF REMTERM-L (BNDX) GREATER +0
00821          MOVE REMTERM (BNDX)   TO  AM-BENEFIT-REM-TERM (SUB1).
00822
00823      MOVE AL-UANON             TO  BENCODE-A (BNDX)
00824                                    BENTYPE-A (BNDX)
00825                                    REVNO-A   (BNDX)
00826                                    RETROYN-A (BNDX)
00827                                    REMTERM-A (BNDX).
00828
00829      IF AM-BENEFIT-CODE (SUB1) = ZEROS
00830          MOVE SPACES           TO AM-ALLOWABLE-BENEFITS (SUB1).
00831
00832      GO TO 6005-CHECK-BENEFITS.
00833
00834  6049-EXIT.
00835      EXIT.
00836      EJECT
00837  6500-SORT-BENEFITS.
00838
00839      MOVE +1                     TO SUB2
00840                                     SUB3
00841      MOVE SPACES                 TO WS-WORK-BENEFIT-TABLE.
00842
00843  6510-BENEFIT-MOVE.
00844
00845      IF AM-BENEFIT-CODE (SUB2) NOT EQUAL SPACES AND ZEROS
00846         MOVE AM-ALLOWABLE-BENEFITS (SUB2)
00847                                  TO WS-WORK-TABLE (SUB3)
00848         MOVE SPACES              TO WS-FILLER (SUB3)
00849         ADD +1                   TO SUB3.
00850
00851      ADD +1                      TO SUB2
00852      IF SUB2 NOT GREATER THAN +20
00853         GO TO 6510-BENEFIT-MOVE.
00854
00855      MOVE WS-WORK-BENEFIT-TABLE  TO AM-BENEFIT-CONTROLS
00856
00857      IF AM-BENEFIT-CONTROLS EQUAL SPACES
00858         GO TO 6599-EXIT.
00859
00860      MOVE +1                     TO SUB2
00861                                     SUB3
00862      MOVE HIGH-VALUES            TO WS-WORK-BENEFIT-TABLE.
00863
00864  6520-CONTINUE-SORT.
00865
00866      IF SUB3 GREATER THAN +20
00867         GO TO 6540-RID-OF-HIGH-VALUES.
00868
00869      IF (AM-BENEFIT-CODE (SUB2) LESS THAN WS-WORK-BENEFIT (SUB3))
00870        AND
00871         (AM-BENEFIT-CODE (SUB2) NOT EQUAL SPACES)
00872         MOVE AM-ALLOWABLE-BENEFITS (SUB2)
00873                                  TO WS-WORK-TABLE (SUB3)
00874         MOVE SPACES              TO WS-FILLER (SUB3).
00875
00876      ADD +1 TO SUB2
00877
00878      IF SUB2 NOT GREATER THAN +20
00879         GO TO 6520-CONTINUE-SORT.
00880
00881      PERFORM 6530-DUMMY VARYING SUB2 FROM +1 BY +1
00882      UNTIL WS-WORK-BENEFIT (SUB3) EQUAL
00883            AM-BENEFIT-CODE (SUB2) OR
00884            SUB2 GREATER THAN +20.
00885
00886      IF SUB2 GREATER THAN +20
00887         MOVE +1 TO SUB2
00888         GO TO 6540-RID-OF-HIGH-VALUES.
00889
00890      MOVE HIGH-VALUES            TO AM-ALLOWABLE-BENEFITS (SUB2)
00891      ADD +1                      TO SUB3
00892      MOVE +1                     TO SUB2
00893      GO TO 6520-CONTINUE-SORT.
00894
00895  6530-DUMMY.
00896
00897  6540-RID-OF-HIGH-VALUES.
00898
00899      IF WS-WORK-TABLE (SUB2) EQUAL HIGH-VALUES
00900         MOVE SPACES               TO WS-WORK-TABLE (SUB2).
00901
00902      ADD +1 TO SUB2
00903
00904      IF SUB2 NOT GREATER THAN +20
00905         GO TO 6540-RID-OF-HIGH-VALUES.
00906
00907      MOVE +1                     TO SUB2
00908      MOVE +2                     TO SUB3.
00909
00910  6550-CHECK-DUPS.
00911
00912      IF WS-WORK-BENEFIT (SUB2) EQUAL WS-WORK-BENEFIT (SUB3) AND
00913         WS-WORK-TYPE (SUB2)    EQUAL WS-WORK-TYPE (SUB3) AND
00914         WS-WORK-BENEFIT (SUB2) NOT EQUAL SPACES
00915         MOVE ER-2947             TO  EMI-ERROR
00916         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00917         MOVE -1                  TO  MAINTYPL
00918         GO TO 8200-SEND-DATAONLY.
00919
00920      ADD +1 TO SUB3
00921
00922      IF SUB3 NOT GREATER THAN +20
00923         GO TO 6550-CHECK-DUPS.
00924
00925      ADD +1                      TO SUB2
00926      MOVE SUB2                   TO SUB3
00927      ADD +1                      TO SUB3
00928
00929      IF SUB2 NOT GREATER THAN +20
00930         GO TO 6550-CHECK-DUPS.
00931
00932      MOVE WS-WORK-BENEFIT-TABLE  TO AM-BENEFIT-CONTROLS.
00933
00934  6599-EXIT.
00935      EXIT.
00936
00937  6600-CHECK-FOR-UPDATE.
00938
00939      IF USECODEL GREATER +0
00940          MOVE USECODEI         TO  AM-BENEFIT-TABLE-USAGE.
00941
00942      MOVE AL-UANON             TO  USECODEA.
00943
00944      MOVE +0                   TO  SUB1.
00945      SET BNDX                  TO  SUB1.
00946      MOVE +20                  TO  WS-MAX-BNDX.
00947
00948      IF SCREEN-1-DISPLAYED
00949          NEXT SENTENCE
00950      ELSE
00951          IF SCREEN-2-DISPLAYED
00952              MOVE +20              TO SUB1
00953          ELSE
00954              MOVE +40              TO SUB1
00955              MOVE +10              TO WS-MAX-BNDX.
00956
00957  6605-CHECK-BENEFITS.
00958
00959      SET BNDX UP BY +1.
00960      ADD +1                    TO  SUB1.
00961
00962      IF BNDX GREATER WS-MAX-BNDX
00963          GO TO 6649-EXIT.
00964
00965      IF BENCODE-L (BNDX) NOT GREATER THAN +0
00966         GO TO 6630-BYPASS-BENEFIT-CHECK.
00967
00968      IF BENCODE-L (BNDX) GREATER THAN +0
00969         IF BENCODE (BNDX) EQUAL '91' OR '92' OR '93' OR '94'
00970                              OR '98' OR '99' OR '  ' OR '00'
00971            GO TO 6630-BYPASS-BENEFIT-CHECK.
00972
00973  6610-READ-BEN-CNTL.
00974
00975      MOVE SPACES                 TO ELCNTL-KEY.
00976      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
00977
00978      IF BENTYPE (BNDX) EQUAL PI-LIFE-OVERRIDE-L1
00979         MOVE '4'                 TO CNTL-REC-TYPE
00980      ELSE
00981         MOVE '5'                 TO CNTL-REC-TYPE.
00982
00983      MOVE BENCODE (BNDX)         TO CNTL-HI-BEN
00984      MOVE +0                     TO CNTL-SEQ-NO.
00985
00986      
      * EXEC CICS HANDLE CONDITION
00987 *        NOTFND   (6625-NOT-FOUND)
00988 *        ENDFILE  (6625-NOT-FOUND)
00989 *    END-EXEC.
      *    MOVE '"$I''                  ! $ #00005234' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035323334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00990
00991      
      * EXEC CICS READ
00992 *        DATASET   (CNTL-FILE-ID)
00993 *        SET       (ADDRESS OF CONTROL-FILE)
00994 *        RIDFLD    (ELCNTL-KEY)
00995 *        GTEQ
00996 *    END-EXEC.
      *    MOVE '&"S        G          (   #00005239' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323339' TO DFHEIV0(25:11)
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
           
00997
00998  6615-BYPASS-ELCNTL-READ.
00999
01000      PERFORM 6620-BENEFIT-LOOP-DUMMY
01001        VARYING SUB2 FROM +1 BY +1 UNTIL
01002         CF-BENEFIT-CODE (SUB2) EQUAL BENCODE (BNDX) OR
01003          SUB2 GREATER THAN +8.
01004
01005      IF SUB2 GREATER THAN +8
01006         GO TO 6625-NOT-FOUND.
01007
01008      GO TO 6630-BYPASS-BENEFIT-CHECK.
01009
01010  6620-BENEFIT-LOOP-DUMMY.
01011
01012  6625-NOT-FOUND.
01013
01014      MOVE -1                     TO  BENCODE-L (BNDX)
01015      MOVE AL-UABON               TO  BENCODE-A (BNDX)
01016      MOVE ER-0150                TO  EMI-ERROR
01017      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01018      GO TO 8200-SEND-DATAONLY.
01019
01020  6630-BYPASS-BENEFIT-CHECK.
01021
01022      IF BENCODE-L (BNDX) GREATER +0
01023          MOVE BENCODE (BNDX) TO  WS-DMD-WORK-BENEFIT-2 (SUB1).
01024
01025      IF BENTYPE-L (BNDX) GREATER +0
01026          MOVE BENTYPE (BNDX) TO  WS-DMD-WORK-TYPE-2 (SUB1).
01027
01028      IF REVNO-L (BNDX) GREATER +0
01029          MOVE REVNO (BNDX)   TO  WS-DMD-WORK-REVISION-2 (SUB1).
01030
01031      IF RETROYN-L (BNDX) GREATER +0
01032          MOVE RETROYN (BNDX) TO  WS-DMD-WORK-RETRO-Y-N-2 (SUB1).
01033
01034      IF REMTERM-L (BNDX) GREATER +0
01035          MOVE REMTERM (BNDX)   TO  WS-DMD-WORK-REM-TERM-2 (SUB1).
01036
01037      MOVE AL-UANON             TO  BENCODE-A (BNDX)
01038                                    BENTYPE-A (BNDX)
01039                                    REVNO-A   (BNDX)
01040                                    RETROYN-A (BNDX)
01041                                    REMTERM-A (BNDX).
01042
01043      IF WS-DMD-WORK-BENEFIT-2 (SUB1) = ZEROS
01044          MOVE SPACES           TO WS-DMD-WORK-TABLE-2 (SUB1).
01045
01046      GO TO 6605-CHECK-BENEFITS.
01047
01048  6649-EXIT.
01049      EXIT.
01050  6700-LOAD-BENEFIT-CODES.
01051
01052      MOVE AM-ALLOWABLE-BENEFITS (SUB1)
01053                                 TO WS-DMD-WORK-TABLE-2 (SUB2).
01054      MOVE SPACES                TO WS-DMD-FILLER-2 (SUB2).
01055
01056      ADD +1                     TO SUB1
01057                                    SUB2.
01058
01059      IF SUB1 NOT GREATER THAN +20
01060         GO TO 6700-LOAD-BENEFIT-CODES.
01061
01062      MOVE +1                    TO SUB1.
01063
01064  6700-LOAD-CONTINUE.
01065
01066      MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB1)
01067                                 TO WS-DMD-WORK-TABLE-2 (SUB2).
01068      MOVE SPACES                TO WS-DMD-FILLER-2 (SUB2).
01069
01070      ADD +1                     TO SUB1
01071                                    SUB2.
01072
01073      IF SUB1 NOT GREATER THAN +30
01074         GO TO 6700-LOAD-CONTINUE.
01075
01076  6700-LOAD-EXIT.
01077  EJECT
01078  6800-SORT-BENEFITS.
01079
01080      IF WS-DMD-WORK-BENEFIT-TABLE-2 EQUAL SPACES
01081         GO TO 6899-EXIT.
01082
01083      MOVE +1                     TO SUB2
01084                                     SUB3.
01085
01086      MOVE HIGH-VALUES            TO WS-DMD-WORK-BENEFIT-TABLE.
01087
01088
01089  6820-CONTINUE-SORT.
01090
01091      IF SUB3 GREATER THAN +50
01092         GO TO 6840-RID-OF-HIGH-VALUES.
01093
01094      IF (WS-DMD-WORK-BENEFIT-2 (SUB2) LESS THAN
01095                           WS-DMD-WORK-BENEFIT (SUB3))
01096        AND
01097         (WS-DMD-WORK-BENEFIT-2 (SUB2) NOT EQUAL SPACES)
01098         MOVE WS-DMD-WORK-TABLE-2 (SUB2)
01099                                  TO WS-DMD-WORK-TABLE (SUB3)
01100         MOVE SPACES              TO WS-DMD-FILLER (SUB3).
01101
01102      ADD +1                      TO SUB2.
01103
01104      IF SUB2 NOT GREATER THAN +50
01105         GO TO 6820-CONTINUE-SORT.
01106
01107      PERFORM 6830-DUMMY VARYING SUB2 FROM +1 BY +1
01108      UNTIL WS-DMD-WORK-BENEFIT (SUB3) EQUAL
01109            WS-DMD-WORK-BENEFIT-2 (SUB2) OR
01110            SUB2 GREATER THAN +50.
01111
01112      IF SUB2 GREATER THAN +50
01113         MOVE +1                  TO SUB2
01114         GO TO 6840-RID-OF-HIGH-VALUES.
01115
01116      MOVE HIGH-VALUES         TO WS-DMD-WORK-BENEFIT-2 (SUB2).
01117      ADD  +1                  TO SUB3.
01118      MOVE +1                  TO SUB2.
01119      GO TO 6820-CONTINUE-SORT.
01120
01121  6830-DUMMY.
01122
01123  6840-RID-OF-HIGH-VALUES.
01124
01125      IF WS-DMD-WORK-TABLE (SUB2) EQUAL HIGH-VALUES
01126         MOVE SPACES               TO WS-DMD-WORK-TABLE (SUB2).
01127
01128      ADD +1                       TO SUB2.
01129
01130      IF SUB2 NOT GREATER THAN +50
01131         GO TO 6840-RID-OF-HIGH-VALUES.
01132
01133      MOVE +1                     TO SUB2
01134      MOVE +2                     TO SUB3.
01135
01136  6850-CHECK-DUPS.
01137
01138      IF WS-DMD-WORK-BENEFIT (SUB2) EQUAL
01139                                 WS-DMD-WORK-BENEFIT (SUB3) AND
01140         WS-DMD-WORK-TYPE (SUB2) EQUAL WS-DMD-WORK-TYPE (SUB3) AND
01141         WS-DMD-WORK-BENEFIT (SUB2) NOT EQUAL SPACES
01142         MOVE ER-2947             TO  EMI-ERROR
01143         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01144         MOVE -1                  TO  MAINTYPL
01145         GO TO 8200-SEND-DATAONLY.
01146
01147      ADD +1                     TO SUB3.
01148
01149      IF SUB3 NOT GREATER THAN +50
01150         GO TO 6850-CHECK-DUPS.
01151
01152      ADD +1                      TO SUB2
01153      MOVE SUB2                   TO SUB3
01154      ADD +1                      TO SUB3
01155
01156      IF SUB2 NOT GREATER THAN +50
01157         GO TO 6850-CHECK-DUPS.
01158
01159  6899-EXIT.
01160      EXIT.
01161      EJECT
01162
01163
01164  6900-LOAD-ACCOUNT-MASTER.
01165
01166      MOVE WS-DMD-WORK-TABLE (SUB2)
01167                                 TO AM-ALLOWABLE-BENEFITS (SUB1).
01168
01169      ADD +1                     TO SUB1
01170                                    SUB2.
01171
01172      IF SUB1 NOT GREATER THAN +20
01173         GO TO 6900-LOAD-ACCOUNT-MASTER.
01174
01175      MOVE +1                    TO SUB1.
01176
01177  6900-LOAD-CONTINUE.
01178
01179      MOVE WS-DMD-WORK-TABLE (SUB2)
01180                              TO AM-ALLOWABLE-DMD-BENEFITS (SUB1).
01181
01182      ADD +1                     TO SUB1
01183                                    SUB2.
01184
01185      IF SUB1 NOT GREATER THAN +30
01186         GO TO 6900-LOAD-CONTINUE.
01187
01188      IF SCREEN-1-DISPLAYED
01189          MOVE 'F'                TO PI-DMD-FILE-SW
01190          MOVE +0                 TO PI-DMD-OCCURS
01191      ELSE
01192          IF SCREEN-2-DISPLAYED
01193              MOVE 'I'            TO PI-DMD-FILE-SW
01194              MOVE +0             TO PI-DMD-OCCURS
01195          ELSE
01196              IF SCREEN-3-DISPLAYED
01197                  MOVE 'E'        TO PI-DMD-FILE-SW
01198                  MOVE +20        TO PI-DMD-OCCURS.
01199
01200  6900-LOAD-EXIT.
01201  EJECT
01202  7000-EDIT.
01203
01204      IF USECODEL GREATER ZERO
01205          MOVE USECODEI         TO  WS-EDIT-USECODE
01206          IF NOT VALID-USE-CODE
01207              MOVE -1           TO  USECODEL
01208              MOVE AL-UABON     TO  USECODEA
01209              MOVE ER-7240      TO  EMI-ERROR
01210              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01211
01212      MOVE +0                   TO  SUB1.
01213      SET BNDX                  TO  SUB1.
01214
01215  7025-EDIT-BENEFITS.
01216      SET BNDX UP BY +1.
01217      ADD +1                    TO  SUB1.
01218
01219      IF BNDX GREATER +20
01220          GO TO 7099-EXIT.
01221
01222      IF BENCODE-L (BNDX) GREATER ZERO
01223          MOVE BENCODE (BNDX)   TO  WS-EDIT-BENCODE.
01224
01225      IF BENTYPE-L (BNDX) GREATER ZERO  OR
01226         BENCODE-L (BNDX) GREATER ZERO
01227          IF BENTYPE (BNDX) = PI-LIFE-OVERRIDE-L1  OR
01228                              PI-AH-OVERRIDE-L1
01229              MOVE AL-UANON     TO  BENTYPE-A (BNDX)
01230          ELSE
01231              MOVE -1           TO  BENTYPE-L (BNDX)
01232              MOVE AL-UABON     TO  BENTYPE-A (BNDX)
01233              MOVE ER-0151      TO  EMI-ERROR
01234              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01235
01236      IF (BENTYPE-L (BNDX) GREATER ZERO  OR
01237         BENCODE-L (BNDX) GREATER ZERO) AND
01238         REVNO-L (BNDX) EQUAL +0 AND
01239         REVNO (BNDX) EQUAL LOW-VALUES OR SPACES
01240         MOVE '000'               TO REVNO (BNDX)
01241         MOVE AL-UANON            TO REVNO-L (BNDX).
01242
01243      IF REVNO-L (BNDX) GREATER THAN +0
01244         IF REVNO (BNDX) NOT EQUAL SPACES
01245            MOVE AL-UANON     TO  REVNO-A   (BNDX)
01246         ELSE
01247            MOVE -1           TO  REVNO-L   (BNDX)
01248            MOVE AL-UABON     TO  REVNO-A   (BNDX)
01249            MOVE ER-2948      TO  EMI-ERROR
01250            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01251
01252      IF BENTYPE-L (BNDX) NOT GREATER ZERO  AND
01253         BENCODE-L (BNDX) NOT GREATER ZERO  AND
01254         REVNO-L (BNDX)   NOT GREATER ZERO
01255         GO TO 7025-CONTINUE.
01256
01257      IF PI-COMPANY-ID = 'DMD'
01258          IF RETROYN (BNDX) = 'R'  OR  'C'  OR  'S'
01259              GO TO 7025-CONTINUE
01260          ELSE
01261              MOVE -1           TO  RETROYN-L   (BNDX)
01262              MOVE AL-UABON     TO  RETROYN-A   (BNDX)
01263              MOVE ER-8034      TO  EMI-ERROR
01264              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
CIDMOD         END-IF
01265      ELSE
CIDMOD*        IF RETROYN (BNDX) = ' '  OR  'Y'  OR  'N'
CIDMOD         IF RETROYN-L (BNDX) GREATER THAN +0
CIDMOD             MOVE 'Y'          TO  RETROYN     (BNDX)
01269          ELSE
01270              MOVE -1           TO  RETROYN-L   (BNDX)
01271              MOVE AL-UABON     TO  RETROYN-A   (BNDX)
01272              MOVE ER-0627      TO  EMI-ERROR
01273              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
CIDMOD         END-IF
CIDMOD     END-IF.
01274
01275  7025-CONTINUE.
01276      IF REMTERM-L (BNDX) GREATER ZERO
01277          MOVE REMTERM (BNDX)   TO  WS-EDIT-REMTERM
01278          IF NOT VALID-REM-TERM
01279              MOVE -1           TO  REMTERM-L (BNDX)
01280              MOVE AL-UABON     TO  REMTERM-A (BNDX)
01281              MOVE ER-2298      TO  EMI-ERROR
01282              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01283
01284      GO TO 7025-EDIT-BENEFITS.
01285
01286  7099-EXIT.
01287      EXIT.
01288      EJECT
01289  7100-READ-ERACCT.
01290      
      * EXEC CICS READ
01291 *         DATASET  (ERACCT-FILE)
01292 *         SET      (ADDRESS OF ACCOUNT-MASTER)
01293 *         RIDFLD   (PI-ACCT-KEY)
01294 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005541' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01295
01296      MOVE AM-LAST-MAINT-USER     TO  PI-UPDATE-BY.
01297      MOVE AM-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
01298
01299  7100-EXIT.
01300      EXIT.
01301      EJECT
01302  EJECT
01303  7200-DEEDIT.
01304      
      * EXEC CICS BIF
01305 *         DEEDIT
01306 *         FIELD  (DEEDIT-FIELD)
01307 *         LENGTH (15)
01308 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005555' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01309
01310  7200-EXIT.
01311      EXIT.
01312      EJECT
01313  7300-READ-ERACCT-UPDATE.
01314      
      * EXEC CICS READ
01315 *         DATASET  (ERACCT-FILE)
01316 *         SET      (ADDRESS OF ACCOUNT-MASTER)
01317 *         RIDFLD   (PI-ACCT-KEY)
01318 *         UPDATE
01319 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005565' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01320
01321  7300-EXIT.
01322      EXIT.
01323      EJECT
01324  7800-COMPANY-REC-READ.
01325
01326      MOVE SPACES                 TO  ELCNTL-KEY.
01327      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
01328      MOVE '1'                    TO  CNTL-REC-TYPE.
01329      MOVE +0                     TO  CNTL-SEQ-NO.
01330      
      * EXEC CICS HANDLE CONDITION
01331 *        NOTFND   (7880-NO-COMP)
01332 *    END-EXEC.
      *    MOVE '"$I                   ! % #00005581' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035353831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01333
01334      
      * EXEC CICS READ
01335 *        DATASET   (CNTL-FILE-ID)
01336 *        SET       (ADDRESS OF CONTROL-FILE)
01337 *        RIDFLD    (ELCNTL-KEY)
01338 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005585' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353835' TO DFHEIV0(25:11)
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
           
01339
01340      IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES
01341          MOVE ER-2572            TO  EMI-ERROR
01342          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01343
01344      GO TO 7899-EXIT.
01345
01346  7880-NO-COMP.
01347      MOVE ER-0002                TO  EMI-ERROR.
01348      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01349
01350  7899-EXIT.
01351      EXIT.
01352      EJECT
01353  8000-UPDATE-MAINT-DATE.
01354      MOVE SPACES                 TO  ELCNTL-KEY.
01355
01356      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
01357      MOVE '1'                    TO  CNTL-REC-TYPE.
01358      MOVE +0                     TO  CNTL-SEQ-NO.
01359
01360      
      * EXEC CICS HANDLE CONDITION
01361 *        NOTFND   (8000-EXIT)
01362 *    END-EXEC.
      *    MOVE '"$I                   ! & #00005611' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035363131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01363
01364      
      * EXEC CICS READ
01365 *        UPDATE
01366 *        DATASET   (CNTL-FILE-ID)
01367 *        SET       (ADDRESS OF CONTROL-FILE)
01368 *        RIDFLD    (ELCNTL-KEY)
01369 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005615' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363135' TO DFHEIV0(25:11)
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
           
01370
01371      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01372      MOVE 'B'                    TO  JP-RECORD-TYPE.
01373      MOVE ELCNTL-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
01374      MOVE CNTL-FILE-ID           TO  FILE-ID.
01375      PERFORM 8400-LOG-JOURNAL-RECORD.
01376
01377      MOVE BIN-CURRENT-SAVE       TO  CF-ACCOUNT-MSTR-MAINT-DT.
01378
01379      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01380      MOVE 'C'                    TO  JP-RECORD-TYPE.
01381      MOVE CNTL-FILE-ID           TO  FILE-ID.
01382
01383      
      * EXEC CICS REWRITE
01384 *        DATASET   (CNTL-FILE-ID)
01385 *        FROM      (CONTROL-FILE)
01386 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005634' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01387
01388      MOVE ELCNTL-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
01389      PERFORM 8400-LOG-JOURNAL-RECORD.
01390
01391  8000-EXIT.
01392       EXIT.
01393      EJECT
01394  8100-SEND-INITIAL-MAP.
01395      MOVE SAVE-DATE              TO  DATEO.
01396      MOVE EIBTIME                TO  TIME-IN.
01397      MOVE TIME-OUT               TO  TIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01398      MOVE -1                     TO  PFENTERL.
01399      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
01400
01401      MOVE 'RETRO'                TO  RETRO1I
01402                                      RETRO2I.
01403      MOVE ' Y/N '                TO  RETRO3I
01404                                      RETRO4I.
01405      IF PI-COMPANY-ID NOT = 'DMD'
01406         GO TO 8100-SEND-CONTINUE.
01407
01408      MOVE ' CONT'               TO  RETRO1I
01409                                     RETRO2I.
01410
01411      MOVE ' TYPE'               TO  RETRO3I
01412                                     RETRO4I.
01413
01414      MOVE 'PF1=BROWSE FORWARD'  TO  PF1KEYI
01415      MOVE 'PF2=BROWSE BACK   '  TO  PF2KEYI.
01416
01417      IF SCREEN-2-DISPLAYED
01418         MOVE '21. '             TO  NUMB01I
01419         MOVE '22. '             TO  NUMB02I
01420         MOVE '23. '             TO  NUMB03I
01421         MOVE '24. '             TO  NUMB04I
01422         MOVE '25. '             TO  NUMB05I
01423         MOVE '26. '             TO  NUMB06I
01424         MOVE '27. '             TO  NUMB07I
01425         MOVE '28. '             TO  NUMB08I
01426         MOVE '29. '             TO  NUMB09I
01427         MOVE '30. '             TO  NUMB10I
01428         MOVE '31. '             TO  NUMB11I
01429         MOVE '32. '             TO  NUMB12I
01430         MOVE '33. '             TO  NUMB13I
01431         MOVE '34. '             TO  NUMB14I
01432         MOVE '35. '             TO  NUMB15I
01433         MOVE '36. '             TO  NUMB16I
01434         MOVE '37. '             TO  NUMB17I
01435         MOVE '38. '             TO  NUMB18I
01436         MOVE '39. '             TO  NUMB19I
01437         MOVE '40. '             TO  NUMB20I
01438      ELSE
01439         IF SCREEN-3-DISPLAYED
01440             MOVE '41. '             TO  NUMB01I
01441             MOVE '42. '             TO  NUMB02I
01442             MOVE '43. '             TO  NUMB03I
01443             MOVE '44. '             TO  NUMB04I
01444             MOVE '45. '             TO  NUMB05I
01445             MOVE '46. '             TO  NUMB06I
01446             MOVE '47. '             TO  NUMB07I
01447             MOVE '48. '             TO  NUMB08I
01448             MOVE '49. '             TO  NUMB09I
01449             MOVE '50. '             TO  NUMB10I
01450             MOVE '    '             TO  NUMB11I
01451             MOVE '    '             TO  NUMB12I
01452             MOVE '    '             TO  NUMB13I
01453             MOVE '    '             TO  NUMB14I
01454             MOVE '    '             TO  NUMB15I
01455             MOVE '    '             TO  NUMB16I
01456             MOVE '    '             TO  NUMB17I
01457             MOVE '    '             TO  NUMB18I
01458             MOVE '    '             TO  NUMB19I
01459             MOVE '    '             TO  NUMB20I
01460             MOVE AL-SANON           TO  CODE11A
01461                                         TYPE11A
01462                                         REVSN11A
01463                                         RETRO11A
01464                                         RTRM11A
01465                                         CODE12A
01466                                         TYPE12A
01467                                         REVSN12A
01468                                         RETRO12A
01469                                         RTRM12A
01470                                         CODE13A
01471                                         TYPE13A
01472                                         REVSN13A
01473                                         RETRO13A
01474                                         RTRM13A
01475                                         CODE14A
01476                                         TYPE14A
01477                                         REVSN14A
01478                                         RETRO14A
01479                                         RTRM14A
01480                                         CODE15A
01481                                         TYPE15A
01482                                         REVSN15A
01483                                         RETRO15A
01484                                         RTRM15A
01485                                         CODE16A
01486                                         TYPE16A
01487                                         REVSN16A
01488                                         RETRO16A
01489                                         RTRM16A
01490                                         CODE17A
01491                                         TYPE17A
01492                                         REVSN17A
01493                                         RETRO17A
01494                                         RTRM17A
01495                                         CODE18A
01496                                         TYPE18A
01497                                         REVSN18A
01498                                         RETRO18A
01499                                         RTRM18A
01500                                         CODE19A
01501                                         TYPE19A
01502                                         REVSN19A
01503                                         RETRO19A
01504                                         RTRM19A
01505                                         CODE20A
01506                                         TYPE20A
01507                                         REVSN20A
01508                                         RETRO20A
01509                                         RTRM20A.
01510
01511
01512  8100-SEND-CONTINUE.
01513
01514      
      * EXEC CICS SEND
01515 *        MAP      (MAP-NAME)
01516 *        MAPSET   (MAPSET-NAME)
01517 *        FROM     (EL6505AO)
01518 *        ERASE
01519 *        CURSOR
01520 *    END-EXEC.
           MOVE LENGTH OF
            EL6505AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005767' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6505AO, 
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
           
01521
01522      GO TO 9100-RETURN-TRAN.
01523
01524  8200-SEND-DATAONLY.
01525      MOVE SAVE-DATE              TO  DATEO.
01526      MOVE EIBTIME                TO  TIME-IN.
01527      MOVE TIME-OUT               TO  TIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01528      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O
01529
01530      MOVE 'RETRO'                TO  RETRO1I
01531                                      RETRO2I.
01532      MOVE ' Y/N '                TO  RETRO3I
01533                                      RETRO4I.
01534      IF PI-COMPANY-ID = 'DMD'
01535         MOVE ' CONT'             TO  RETRO1I
01536                                      RETRO2I
01537         MOVE ' TYPE'             TO  RETRO3I
01538                                      RETRO4I.
01539
01540      IF PI-COMPANY-ID = 'DMD'
01541         MOVE 'PF1=BROWSE FORWARD' TO PF1KEYI
01542         MOVE 'PF2=BROWSE BACK   ' TO PF2KEYI.
01543
01544      
      * EXEC CICS SEND
01545 *        MAP      (MAP-NAME)
01546 *        MAPSET   (MAPSET-NAME)
01547 *        FROM     (EL6505AO)
01548 *        DATAONLY
01549 *        ERASEAUP
01550 *        CURSOR
01551 *    END-EXEC.
           MOVE LENGTH OF
            EL6505AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00005799' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6505AO, 
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
           
01552
01553      GO TO 9100-RETURN-TRAN.
01554
01555  8300-SEND-TEXT.
01556      
      * EXEC CICS SEND TEXT
01557 *        FROM     (LOGOFF-TEXT)
01558 *        LENGTH   (LOGOFF-LENGTH)
01559 *        ERASE
01560 *        FREEKB
01561 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005811' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383131' TO DFHEIV0(25:11)
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
           
01562
01563      
      * EXEC CICS RETURN
01564 *    END-EXEC.
      *    MOVE '.(                    ''   #00005818' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01565
01566  8400-LOG-JOURNAL-RECORD.
01567      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
01568      MOVE FILE-ID                TO  JP-FILE-ID.
01569      MOVE THIS-PGM               TO  JP-PROGRAM-ID.
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO
pemuni*        EXEC CICS JOURNAL
pemuni*            JFILEID     (PI-JOURNAL-FILE-ID)
pemuni*            JTYPEID     ('ER')
pemuni*            FROM        (JOURNAL-RECORD)
pemuni*            LENGTH      (WS-JOURNAL-FILE-LENGTH)
pemuni*        END-EXEC.
01577
01578  8800-UNAUTHORIZED-ACCESS.
01579      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
01580      GO TO 8300-SEND-TEXT.
01581
01582  8810-PF23.
01583      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01584      MOVE XCTL-005               TO  PGM-NAME.
01585      GO TO 9300-XCTL.
01586
01587  9000-RETURN-CICS.
01588      
      * EXEC CICS RETURN
01589 *    END-EXEC.
      *    MOVE '.(                    ''   #00005843' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01590
01591  9100-RETURN-TRAN.
01592      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01593      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01594      
      * EXEC CICS RETURN
01595 *        TRANSID    (TRANS-ID)
01596 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01597 *        LENGTH     (WS-COMM-LENGTH)
01598 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005849' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01599
01600  9200-RETURN-MAIN-MENU.
01601      MOVE XCTL-626               TO  PGM-NAME.
01602      GO TO 9300-XCTL.
01603
01604  9300-XCTL.
01605      
      * EXEC CICS XCTL
01606 *        PROGRAM    (PGM-NAME)
01607 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01608 *        LENGTH     (WS-COMM-LENGTH)
01609 *    END-EXEC.
      *    MOVE '.$C                   %   #00005860' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01610
01611  9400-CLEAR.
01612      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
01613      GO TO 9300-XCTL.
01614
01615  9500-PF12.
01616      MOVE XCTL-010               TO  PGM-NAME.
01617      GO TO 9300-XCTL.
01618
01619  9600-PGMID-ERROR.
01620      
      * EXEC CICS HANDLE CONDITION
01621 *        PGMIDERR    (8300-SEND-TEXT)
01622 *    END-EXEC.
      *    MOVE '"$L                   ! '' #00005875' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035383735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01623
01624      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
01625      MOVE ' '                    TO  PI-ENTRY-CD-1.
01626      MOVE XCTL-005               TO  PGM-NAME.
01627      MOVE PGM-NAME               TO  LOGOFF-PGM.
01628      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01629      GO TO 9300-XCTL.
01630
01631  9700-LINK-DATE-CONVERT.
01632      
      * EXEC CICS LINK
01633 *        PROGRAM    ('ELDATCV')
01634 *        COMMAREA   (DATE-CONVERSION-DATA)
01635 *        LENGTH     (DC-COMM-LENGTH)
01636 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005887' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01637
01638  9700-EXIT.
01639      EXIT.
01640
01641  9900-ERROR-FORMAT.
01642      IF NOT EMI-ERRORS-COMPLETE
01643          MOVE LINK-001           TO  PGM-NAME
01644          
      * EXEC CICS LINK
01645 *            PROGRAM    (PGM-NAME)
01646 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01647 *            LENGTH     (EMI-COMM-LENGTH)
01648 *        END-EXEC.
      *    MOVE '."C                   (   #00005899' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01649
01650  9900-EXIT.
01651      EXIT.
01652
01653  9990-ABEND.
01654      MOVE LINK-004               TO  PGM-NAME.
01655      MOVE DFHEIBLK               TO  EMI-LINE1.
01656      
      * EXEC CICS LINK
01657 *        PROGRAM   (PGM-NAME)
01658 *        COMMAREA  (EMI-LINE1)
01659 *        LENGTH    (72)
01660 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005911' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01661
01662      GO TO 8200-SEND-DATAONLY.
01663
01664      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6505' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01665
01666  9995-SECURITY-VIOLATION.
01667 *           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00005939' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393339' TO DFHEIV0(25:11)
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
01668  9995-EXIT.
01669       EXIT.
01670


       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6505' TO DFHEIV1
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
               GO TO 6025-NOT-FOUND,
                     6025-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 6625-NOT-FOUND,
                     6625-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7880-NO-COMP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6505' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
