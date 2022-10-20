00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL1276.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 09:59:22.
00007 *                            VMOD=2.004.
00008 *
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
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
00025 *REMARKS.    TRANSACTION - EXX6 - CERTIFICATE AND BILLING NOTES.
00026 *
101201******************************************************************
101201*                   C H A N G E   L O G
101201*
101201* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101201*-----------------------------------------------------------------
101201*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101201* EFFECTIVE    NUMBER
101201*-----------------------------------------------------------------
101201* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101201*                              ADJUSTED REDEFINES EL127FO FILLER
092309* 092309    2008100900003  AJRA  CERT NOTES MOVED TO NEW SCREEN
110811* 110811  CR2011110800002  PEMA CORRECT SECURITY ISSUE
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
101201******************************************************************
00027  ENVIRONMENT DIVISION.
00028
00029      EJECT
00030  DATA DIVISION.
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*    EL1276 WORKING STORAGE    *'.
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.004 *********'.
00035
00036 *    COPY ELCSCTM.
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
00037
00038 *    COPY ELCSCRTY.
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
00039
00040  01  WS-DATE-AREA.
00041      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00042      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
00043
00044  01  STANDARD-AREAS.
00045      12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.
00046      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00047      12  MAP-NAME                    PIC X(8)    VALUE 'EL127F'.
00048      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL1276S'.
00049      12  SCREEN-NUMBER               PIC X(4)    VALUE '127F'.
00050      12  TRANS-ID                    PIC X(4)    VALUE 'EXX6'.
00051      12  THIS-PGM                    PIC X(8)    VALUE 'EL1276'.
00052      12  PGM-NAME                    PIC X(8).
00053      12  TIME-IN                     PIC S9(7).
00054      12  TIME-OUT-R  REDEFINES TIME-IN.
00055          16  FILLER                  PIC X.
00056          16  TIME-OUT                PIC 99V99.
00057          16  FILLER                  PIC XX.
00058      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00059      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00060      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.
092309     12  PGM-1279                    PIC X(8)    VALUE 'EL1279'.
00061      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00062      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00063      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00064      12  ERNOTE-ID                   PIC X(8)    VALUE 'ERNOTE'.
00065      12  ELCERT-ID                   PIC X(8)    VALUE 'ELCERT'.
00066      12  WS-RECORD-LENGTHS   COMP.
00067          16  WS-ERNOTE-RECORD-LENGTH  PIC S9(4)  VALUE +825.
00068          16  WS-ELCERT-RECORD-LENGTH  PIC S9(4)  VALUE +450.
00069          16  WS-JOURNAL-RECORD-LENGTH PIC S9(4)  VALUE ZEROS.
00070
00071      12  DEEDIT-FIELD                PIC X(15).
00072      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
00073
00074      12  RETURN-FROM                 PIC X(8).
00075      12  QID.
00076          16  QID-TERM                PIC X(4).
00077          16  FILLER                  PIC X(4)    VALUE '127F'.
00078      12  WS-SUB1                     PIC  S99  COMP.
00079      12  WS-NOTE-LINE-SW             PIC  X      VALUE SPACE.
00080          88  NOTE-LINES-PRESENT                  VALUE 'Y'.
00081      12  WS-RECORD-FOUND-SW          PIC  X      VALUE SPACE.
00082          88  RECORD-FOUND                        VALUE 'Y'.
00083          88  RECORD-NOT-FOUND                    VALUE 'N'.
00084      12  WS-DUPREC-SW                PIC  X      VALUE SPACE.
00085          88  DUPLICATE-RECORD-FOUND              VALUE 'Y'.
00086
00087      EJECT
00088      12  ERROR-MESSAGES.
00089          16  ER-0000                 PIC  X(4)   VALUE '0000'.
00090          16  ER-0004                 PIC  X(4)   VALUE '0004'.
00091          16  ER-0008                 PIC  X(4)   VALUE '0008'.
00092          16  ER-0023                 PIC  X(4)   VALUE '0023'.
00093          16  ER-0029                 PIC  X(4)   VALUE '0029'.
00094          16  ER-0070                 PIC  X(4)   VALUE '0070'.
00095          16  ER-0142                 PIC  X(4)   VALUE '0142'.
00096          16  ER-2520                 PIC  X(4)   VALUE '2520'.
00097          16  ER-2521                 PIC  X(4)   VALUE '2521'.
00098          16  ER-2522                 PIC  X(4)   VALUE '2522'.
00099          16  ER-2523                 PIC  X(4)   VALUE '2523'.
00100          16  ER-2524                 PIC  X(4)   VALUE '2524'.
00101          16  ER-2525                 PIC  X(4)   VALUE '2525'.
00102          16  ER-2528                 PIC  X(4)   VALUE '2528'.
00103          16  ER-2706                 PIC  X(4)   VALUE '2706'.
00104
00105      EJECT
00106
00107  01  WS-CM-CONTROL-PRIMARY.
00108      05  WS-CM-COMPANY-CD            PIC  X.
00109      05  WS-CM-CARRIER               PIC  X.
00110      05  WS-CM-GROUPING              PIC  X(6).
00111      05  WS-CM-STATE                 PIC  XX.
00112      05  WS-CM-ACCOUNT               PIC  X(10).
00113      05  WS-CM-CERT-EFF-DT           PIC  XX.
00114      05  WS-CM-CERT-NO.
00115          10  WS-CM-CERT-PRIME        PIC  X(10).
00116          10  WS-CM-CERT-SFX          PIC  X.
00117
041320 01  WS-CN-CONTROL-PRIMARY.
041320     05  WS-CN-COMPANY-CD            PIC  X.
041320     05  WS-CN-CARRIER               PIC  X.
041320     05  WS-CN-GROUPING              PIC  X(6).
041320     05  WS-CN-STATE                 PIC  XX.
041320     05  WS-CN-ACCOUNT               PIC  X(10).
041320     05  WS-CN-CERT-EFF-DT           PIC  XX.
041320     05  WS-CN-CERT-NO.
041320         10  WS-CN-CERT-PRIME        PIC  X(10).
041320         10  WS-CN-CERT-SFX          PIC  X.
041320     05  WS-CN-RECORD-TYPE           PIC  X.
00118 *    COPY ELCDATE.
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
00119
00120      EJECT
00121 *    COPY ELCLOGOF.
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
00122
00123      EJECT
00124 *    COPY ELCATTR.
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
00125
00126      EJECT
00127 *    COPY ELCEMIB.
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
00128
00129      EJECT
00130 *    COPY ELCINTF.
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
00161      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
092309         16  FILLER              PIC X(318).
041320         16  PI-TOTAL-LINES      PIC S9(3).
041320         16  PI-CURRENT-LINE     PIC S9(3)   COMP-3.
041320         16  PI-TEMP-STOR-ITEMS  PIC S9(4)   COMP.
041320         16  PI-UPDATE-SW        PIC X.
041320             88  PI-CHANGES-MADE             VALUE '1'.
041320         16  PI-PF5-PRESSED      PIC X.
041320             88 PF5-PRESSED                  VALUE 'Y'.
041320         16  PI-PF6-PRESSED      PIC X.
041320             88 PF6-PRESSED                  VALUE 'Y'.
092309         16  PI-CURR-NOTE-REC-TYPE PIC X.
092309             88 PI-CERT-NOTE                 VALUE '1'.
092309             88 PI-CLAIM-NOTE                VALUE '2'.
092309         16  PI-BILLING-NOTES-EXIST PIC X.
092309         16  PI-CERT-NOTES-EXIST    PIC X.
092309         16  PI-CLAIM-NOTES-EXIST   PIC X.
041320         16  filler                 pic x.
041320         16  pi-iss-can-trans    pic x.
041320             88  pi-from-issue     value '1'.
041320             88  pi-from-cancel    value '2'.
041320         16  pi-active-notes        pic x.
041320             88  pi-issue-notes       value '1'.
041320             88  pi-cancel-notes      value '2'.
00131
00134 *    COPY ELCJPFX.
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
00135                                      PIC X(523).
00136      EJECT
00137
00138 *    COPY ELCAID.
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
00139  01  FILLER    REDEFINES DFHAID.
00140      12  FILLER                      PIC X(8).
00141      12  PF-VALUES                   PIC X       OCCURS 24 TIMES.
00142
00143      EJECT
00144 *    COPY EL1276S.
       01  EL127FI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FDATEL PIC S9(0004) COMP.
           05  FDATEF PIC  X(0001).
           05  FILLER REDEFINES FDATEF.
               10  FDATEA PIC  X(0001).
           05  FDATEI PIC  X(0008).
      *    -------------------------------
           05  FTIMEL PIC S9(0004) COMP.
           05  FTIMEF PIC  X(0001).
           05  FILLER REDEFINES FTIMEF.
               10  FTIMEA PIC  X(0001).
           05  FTIMEI PIC  X(0005).
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
           05  FMAINTL PIC S9(0004) COMP.
           05  FMAINTF PIC  X(0001).
           05  FILLER REDEFINES FMAINTF.
               10  FMAINTA PIC  X(0001).
           05  FMAINTI PIC  X(0001).
      *    -------------------------------
           05  FCERTL PIC S9(0004) COMP.
           05  FCERTF PIC  X(0001).
           05  FILLER REDEFINES FCERTF.
               10  FCERTA PIC  X(0001).
           05  FCERTI PIC  X(0010).
      *    -------------------------------
           05  FCRTSFXL PIC S9(0004) COMP.
           05  FCRTSFXF PIC  X(0001).
           05  FILLER REDEFINES FCRTSFXF.
               10  FCRTSFXA PIC  X(0001).
           05  FCRTSFXI PIC  X(0001).
      *    -------------------------------
           05  FACOUNTL PIC S9(0004) COMP.
           05  FACOUNTF PIC  X(0001).
           05  FILLER REDEFINES FACOUNTF.
               10  FACOUNTA PIC  X(0001).
           05  FACOUNTI PIC  X(0010).
      *    -------------------------------
           05  FSTL PIC S9(0004) COMP.
           05  FSTF PIC  X(0001).
           05  FILLER REDEFINES FSTF.
               10  FSTA PIC  X(0001).
           05  FSTI PIC  X(0002).
      *    -------------------------------
           05  FCARRIRL PIC S9(0004) COMP.
           05  FCARRIRF PIC  X(0001).
           05  FILLER REDEFINES FCARRIRF.
               10  FCARRIRA PIC  X(0001).
           05  FCARRIRI PIC  X(0001).
      *    -------------------------------
           05  FGROUPL PIC S9(0004) COMP.
           05  FGROUPF PIC  X(0001).
           05  FILLER REDEFINES FGROUPF.
               10  FGROUPA PIC  X(0001).
           05  FGROUPI PIC  X(0006).
      *    -------------------------------
           05  FEFFDTL PIC S9(0004) COMP.
           05  FEFFDTF PIC  X(0001).
           05  FILLER REDEFINES FEFFDTF.
               10  FEFFDTA PIC  X(0001).
           05  FEFFDTI PIC  X(0008).
      *    -------------------------------
           05  FBSTARTL PIC S9(0004) COMP.
           05  FBSTARTF PIC  X(0001).
           05  FILLER REDEFINES FBSTARTF.
               10  FBSTARTA PIC  X(0001).
           05  FBSTARTI PIC  X(0002).
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
           05  MAINTBYL PIC S9(0004) COMP.
           05  MAINTBYF PIC  X(0001).
           05  FILLER REDEFINES MAINTBYF.
               10  MAINTBYA PIC  X(0001).
           05  MAINTBYI PIC  X(0004).
      *    -------------------------------
           05  FBENDL PIC S9(0004) COMP.
           05  FBENDF PIC  X(0001).
           05  FILLER REDEFINES FBENDF.
               10  FBENDA PIC  X(0001).
           05  FBENDI PIC  X(0002).
      *    -------------------------------
           05  FCERTYNL PIC S9(0004) COMP.
           05  FCERTYNF PIC  X(0001).
           05  FILLER REDEFINES FCERTYNF.
               10  FCERTYNA PIC  X(0001).
           05  FCERTYNI PIC  X(0003).
      *    -------------------------------
           05  FCLMYNL PIC S9(0004) COMP.
           05  FCLMYNF PIC  X(0001).
           05  FILLER REDEFINES FCLMYNF.
               10  FCLMYNA PIC  X(0001).
           05  FCLMYNI PIC  X(0003).
      *    -------------------------------
           05  FHEADL PIC S9(0004) COMP.
           05  FHEADF PIC  X(0001).
           05  FILLER REDEFINES FHEADF.
               10  FHEADA PIC  X(0001).
           05  FHEADI PIC  X(0025).
      *    -------------------------------
           05  FNLINE1L PIC S9(0004) COMP.
           05  FNLINE1F PIC  X(0001).
           05  FILLER REDEFINES FNLINE1F.
               10  FNLINE1A PIC  X(0001).
           05  FNLINE1I PIC  X(0077).
      *    -------------------------------
           05  FNLINE2L PIC S9(0004) COMP.
           05  FNLINE2F PIC  X(0001).
           05  FILLER REDEFINES FNLINE2F.
               10  FNLINE2A PIC  X(0001).
           05  FNLINE2I PIC  X(0077).
      *    -------------------------------
           05  FNLINE3L PIC S9(0004) COMP.
           05  FNLINE3F PIC  X(0001).
           05  FILLER REDEFINES FNLINE3F.
               10  FNLINE3A PIC  X(0001).
           05  FNLINE3I PIC  X(0077).
      *    -------------------------------
           05  FNLINE4L PIC S9(0004) COMP.
           05  FNLINE4F PIC  X(0001).
           05  FILLER REDEFINES FNLINE4F.
               10  FNLINE4A PIC  X(0001).
           05  FNLINE4I PIC  X(0077).
      *    -------------------------------
           05  FNLINE5L PIC S9(0004) COMP.
           05  FNLINE5F PIC  X(0001).
           05  FILLER REDEFINES FNLINE5F.
               10  FNLINE5A PIC  X(0001).
           05  FNLINE5I PIC  X(0077).
      *    -------------------------------
           05  FNLINE6L PIC S9(0004) COMP.
           05  FNLINE6F PIC  X(0001).
           05  FILLER REDEFINES FNLINE6F.
               10  FNLINE6A PIC  X(0001).
           05  FNLINE6I PIC  X(0077).
      *    -------------------------------
           05  FNLINE7L PIC S9(0004) COMP.
           05  FNLINE7F PIC  X(0001).
           05  FILLER REDEFINES FNLINE7F.
               10  FNLINE7A PIC  X(0001).
           05  FNLINE7I PIC  X(0077).
      *    -------------------------------
           05  FNLINE8L PIC S9(0004) COMP.
           05  FNLINE8F PIC  X(0001).
           05  FILLER REDEFINES FNLINE8F.
               10  FNLINE8A PIC  X(0001).
           05  FNLINE8I PIC  X(0077).
      *    -------------------------------
           05  FNLINE9L PIC S9(0004) COMP.
           05  FNLINE9F PIC  X(0001).
           05  FILLER REDEFINES FNLINE9F.
               10  FNLINE9A PIC  X(0001).
           05  FNLINE9I PIC  X(0077).
      *    -------------------------------
           05  FNLINEAL PIC S9(0004) COMP.
           05  FNLINEAF PIC  X(0001).
           05  FILLER REDEFINES FNLINEAF.
               10  FNLINEAA PIC  X(0001).
           05  FNLINEAI PIC  X(0077).
      *    -------------------------------
           05  FERRMSGL PIC S9(0004) COMP.
           05  FERRMSGF PIC  X(0001).
           05  FILLER REDEFINES FERRMSGF.
               10  FERRMSGA PIC  X(0001).
           05  FERRMSGI PIC  X(0077).
      *    -------------------------------
           05  FPFENTRL PIC S9(0004) COMP.
           05  FPFENTRF PIC  X(0001).
           05  FILLER REDEFINES FPFENTRF.
               10  FPFENTRA PIC  X(0001).
           05  FPFENTRI PIC  99.
      *    -------------------------------
           05  FPF7HL PIC S9(0004) COMP.
           05  FPF7HF PIC  X(0001).
           05  FILLER REDEFINES FPF7HF.
               10  FPF7HA PIC  X(0001).
           05  FPF7HI PIC  X(0016).
       01  EL127FO REDEFINES EL127FI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FCERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FCRTSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FACOUNTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FCARRIRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FBSTARTO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FBENDO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FCERTYNO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FCLMYNO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FHEADO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE1O PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE2O PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE3O PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE4O PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE5O PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE6O PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE7O PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE8O PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINE9O PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FNLINEAO PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FERRMSGO PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FPFENTRO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FPF7HO PIC  X(0016).
      *    -------------------------------
00145
00146  01  FILLER REDEFINES EL127FO.
041320     05  FILLER                      PIC X(183).
00148      05  DNOTE-LINES   OCCURS 10 TIMES.
00149          10  DNOTE-LINE-LENGTH       PIC S9(4) COMP.
00150          10  DNOTE-LINE-ATTRB        PIC X.
00151          10  DNOTE-LINE              PIC X(77).
041320     05  FILLER                      PIC X(104).
00153
00154      EJECT
00155
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
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
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
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
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00157  01  DFHCOMMAREA                     PIC X(1024).
00158
00159      EJECT
00160
00161 *    COPY ERCNOTE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCNOTE                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = CERTIFICATE AND BILLING NOTES        *
00008 *                                                                *
00009 *        FILE TYPE= VSAM,KSDS                                    *
00010 *        RECORD SIZE = 825    RECFORM = FIXED                    *
00011 *                                                                *
00012 *        BASE CLUSTER = ERNOTE        RKP=2,LEN=34               *
00013 *                                                                *
00014 *        LOG = YES                                               *
00015 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00016 *                                                                *
091509******************************************************************
091509*                   C H A N G E   L O G
091509*
091509* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091509*-----------------------------------------------------------------
091509*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091509* EFFECTIVE    NUMBER
091509*-----------------------------------------------------------------
091509* 091509  CR2008100900003  AJRA  CERT NOTES MOVED TO NEW FILE. THI
091509*                                FILE WILL CONTAIN BILLING NOTES O
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
00017 ******************************************************************
00018
00019  01  CERTIFICATE-NOTE.
00020      12  CN-RECORD-ID                PIC  XX.
00021          88  VALID-CN-ID                  VALUE 'CN'.
00022
00023      12  CN-CONTROL-PRIMARY.
00024          16  CN-COMPANY-CD           PIC X.
00025          16  CN-CARRIER              PIC X.
00026          16  CN-GROUPING.
00027              20 CN-GROUPING-PREFIX   PIC XXX.
00028              20 CN-GROUPING-PRIME    PIC XXX.
00029          16  CN-STATE                PIC XX.
00030          16  CN-ACCOUNT.
00031              20 CN-ACCOUNT-PREFIX    PIC X(4).
00032              20 CN-ACCOUNT-PRIME     PIC X(6).
00033          16  CN-CERT-EFF-DT          PIC XX.
00034          16  CN-CERT-NO.
00035              20  CN-CERT-PRIME       PIC X(10).
00036              20  CN-CERT-SFX         PIC X.
041320         16  CN-RECORD-TYPE          PIC X.
041320             88  CN-ISSUE-BILLING-NOTE    VALUE '1'.
041320             88  CN-CANCEL-BILLING-NOTE   VALUE '2'.
00038      12  CN-BILLING-START-LINE-NO    PIC 99.
00039      12  CN-BILLING-END-LINE-NO      PIC 99.
00040
00041      12  CN-LINES.
00042          16  CN-LINE OCCURS 10       PIC X(77).
00043
00044      12  CN-LAST-MAINT-DT            PIC XX.
00045      12  CN-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
00046      12  CN-LAST-MAINT-USER          PIC X(4).
041320     12  FILLER                      PIC X(5).
00048 ******************************************************************
00162
00163      EJECT
00164
00165 *    COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
010716* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
090314         16  cm-temp-epiq                  pic xx.
090314             88  EPIQ-CLASS                  value 'EQ'.
090314*        16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
010716         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
011410         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
011410                                           PIC S9(5)V99  COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
062017         16  CM-REF-INTERFACE-SW           PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
062017     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
032612     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
032612     12  FILLER REDEFINES CM-USER-RESERVED.
032612         16  CM-AH-CLASS-CD                PIC XX.
032612         16  F                             PIC X.
00286 ******************************************************************
00166
00167      EJECT
00168
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CERTIFICATE-NOTE
                                CERTIFICATE-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1276' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00170
00171      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00172      MOVE '5'                    TO  DC-OPTION-CODE.
00173      PERFORM 9700-DATE-LINK.
00174      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00175      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00176
00177      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00178      MOVE 1                      TO  EMI-NUMBER-OF-LINES.
00179      MOVE EIBTRMID               TO  QID-TERM.
00180
00181      IF EIBCALEN = 0
00182          GO TO 8800-UNAUTHORIZED-ACCESS.
00183
092309     IF PI-CALLING-PROGRAM = PGM-1279
041320         move pi-iss-can-trans   to pi-active-notes
092309         MOVE THIS-PGM           TO PI-CALLING-PROGRAM
092309     END-IF.
092309
00184      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00185          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00186              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00187              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00188              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00189              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00190              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00191              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00192              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00193              MOVE THIS-PGM TO PI-CALLING-PROGRAM
00194          ELSE
00195              MOVE PI-CALLING-PROGRAM   TO RETURN-FROM
00196              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00197              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00198              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00199              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00200              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00201              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00202              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00203              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00204
092309     
      * EXEC CICS HANDLE AID
092309*         CLEAR(9400-CLEAR)
092309*    END-EXEC.
      *    MOVE '"&=                  V! " #00001819' TO DFHEIV0
           MOVE X'22263D202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020562120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031383139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
092309*     IF EIBAID = DFHCLEAR
092309*         GO TO 9400-CLEAR.
00207
00208      IF PI-PROCESSOR-ID = 'LGXX'
00209          NEXT SENTENCE
00210      ELSE
00211          
      * EXEC CICS READQ TS
00212 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00213 *            INTO    (SECURITY-CONTROL)
00214 *            LENGTH  (SC-COMM-LENGTH)
00215 *            ITEM    (SC-ITEM)
00216 *        END-EXEC
      *    MOVE '*$II   L              ''   #00001828' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00217          MOVE SC-CREDIT-DISPLAY (32)   TO  PI-DISPLAY-CAP
00218          MOVE SC-CREDIT-UPDATE  (32)   TO  PI-MODIFY-CAP
00219          IF NOT DISPLAY-CAP
00220              MOVE 'READ'               TO  SM-READ
00221              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00222              MOVE ER-0070              TO  EMI-ERROR
00223              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00224              GO TO 8100-SEND-INITIAL-MAP.
00225
00226      IF  EIBTRNID NOT = TRANS-ID
00227          MOVE LOW-VALUES         TO  EL127FI
00228          PERFORM 7000-FORMAT-SCREEN
00229          GO TO 8100-SEND-INITIAL-MAP.
00237
00238      
      * EXEC CICS    HANDLE    CONDITION
00239 *         PGMIDERR          (9600-PGMID-ERROR)
00240 *         ERROR             (9990-ABEND)
00241 *         END-EXEC.
      *    MOVE '"$L.                  ! # #00001848' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031383438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00242
00243      EJECT
00244
00245  0200-RECEIVE.
00246      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00247          MOVE ER-0008            TO  EMI-ERROR
00248          PERFORM 9900-ERROR-FORMAT
00249          MOVE -1                 TO  FMAINTL
00250          GO TO 8200-SEND-DATAONLY.
00251
00252      
      * EXEC CICS RECEIVE
00253 *        MAP      (MAP-NAME)
00254 *        MAPSET   (MAPSET-NAME)
00255 *        INTO     (EL127FI)
00256 *    END-EXEC.
           MOVE LENGTH OF
            EL127FI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001862' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127FI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00257
00258      IF FPFENTRL = 0
00259          GO TO 0300-CHECK-PFKEYS.
00260
00261      IF EIBAID NOT = DFHENTER
00262          MOVE ER-0004            TO  EMI-ERROR
00263          GO TO 0320-INPUT-ERROR.
00264
00265      IF  FPFENTRI NUMERIC
00266          IF  FPFENTRI = '12' OR '23' OR '24'
00267              MOVE PF-VALUES (FPFENTRI)   TO  EIBAID
00268          ELSE
00269              MOVE ER-0029                TO  EMI-ERROR
00270              GO TO 0320-INPUT-ERROR.
00271
00272  0300-CHECK-PFKEYS.
00273      IF EIBAID = DFHPF23
00274          GO TO 8810-PF23.
00275
00276      IF EIBAID = DFHPF24
00277          GO TO 9200-RETURN-MAIN-MENU.
00278
00279      IF EIBAID = DFHPF12
00280          GO TO 9500-PF12.
00281
092309     IF EIBAID = DFHPF5
092309         MOVE PGM-1279         TO PGM-NAME
092309         SET PI-CERT-NOTE     TO TRUE
092309         GO TO 9300-XCTL
092309     END-IF.
092309
092309     IF EIBAID = DFHPF6
092309         MOVE PGM-1279         TO PGM-NAME
092309         SET PI-CLAIM-NOTE     TO TRUE
092309         GO TO 9300-XCTL
092309     END-IF.
041320     if eibaid = dfhpf7
041320        if pi-cancel-notes
041320           set pi-issue-notes to true
041320        else
041320           set pi-cancel-notes to true
041320        end-if
041320        perform 7000-format-screen
041320        go to 8100-send-initial-map
041320     end-if
00282      IF EIBAID = DFHENTER
00283          GO TO 400-EDIT-INPUT-DATA.
00284
00285      MOVE ER-0029                TO  EMI-ERROR.
00286
00287  0320-INPUT-ERROR.
00288      PERFORM 9900-ERROR-FORMAT.
00289      MOVE AL-UNBON               TO  FPFENTRA.
00290
00291      IF FPFENTRL = 0
00292          MOVE -1                 TO  FMAINTL
00293      ELSE
00294          MOVE -1                 TO  FPFENTRL.
00295
00296      GO TO 8200-SEND-DATAONLY.
00297
00298      EJECT
00299
00300  400-EDIT-INPUT-DATA.
00301      IF FMAINTI = 'A' OR 'C' OR 'D'
00302          NEXT SENTENCE
00303      ELSE
00304          MOVE ER-0023            TO  EMI-ERROR
00305          MOVE -1                 TO  FMAINTL
00306          MOVE AL-UABON           TO  FMAINTA
00307          PERFORM 9900-ERROR-FORMAT
00308          GO TO 8200-SEND-DATAONLY.
110811     IF NOT MODIFY-CAP
110811        MOVE 'UPDATE'            TO SM-READ
110811        PERFORM 9995-SECURITY-VIOLATION
110811                                 THRU 9995-EXIT
110811        MOVE ER-0070             TO EMI-ERROR
110811        PERFORM 9900-ERROR-FORMAT
110811                                 THRU 9900-EXIT
110811        GO TO 8100-SEND-INITIAL-MAP
110811     END-IF
00310      IF FMAINTI = 'D'
00311          GO TO 410-CHECK-ERRORS.
00312
00313      IF FBSTARTL GREATER THAN ZERO
00314         IF FBSTARTI NOT NUMERIC
00315             MOVE ER-2520         TO  EMI-ERROR
00316             MOVE AL-UNBON        TO  FBSTARTA
00317             MOVE -1              TO  FBSTARTL
00318             PERFORM 9900-ERROR-FORMAT
00319         ELSE
00320             IF FBSTARTI LESS '01' OR GREATER '10'
00321                 MOVE ER-2520     TO  EMI-ERROR
00322                 MOVE -1          TO  FBSTARTL
00323                 PERFORM 9900-ERROR-FORMAT.
00324
00325      IF FBENDL GREATER THAN ZERO
00326         IF FBENDI NOT NUMERIC
00327             MOVE ER-2521         TO  EMI-ERROR
00328             MOVE AL-UNBON        TO  FBENDA
00329             MOVE -1              TO  FBENDL
00330             PERFORM 9900-ERROR-FORMAT
00331         ELSE
00332             IF FBENDI LESS '01' OR GREATER '10'
00333                 MOVE ER-2521     TO  EMI-ERROR
00334                 MOVE -1          TO  FBENDL
00335                 PERFORM 9900-ERROR-FORMAT.
00336
00337      IF FBENDL GREATER THAN ZEROS
00338         IF FBSTARTL = ZEROS
00339             MOVE ER-2524         TO  EMI-ERROR
00340             MOVE -1              TO  FBSTARTL
00341             PERFORM 9900-ERROR-FORMAT
00342             GO TO 410-CHECK-ERRORS.
00343
00344      IF FBSTARTL = ZERO OR
00345         FBENDL   = ZERO
00346            GO TO 410-CHECK-ERRORS.
00347
00348      IF FBSTARTI GREATER THAN FBENDI
00349          MOVE ER-2522            TO  EMI-ERROR
00350          MOVE -1                 TO  FBSTARTL
00351          PERFORM 9900-ERROR-FORMAT.
00352
00353  410-CHECK-ERRORS.
00354      IF EMI-ERROR = ZEROS
00355         NEXT SENTENCE
00356      ELSE
00357          GO TO 8200-SEND-DATAONLY.
00358
00359      IF FMAINTI = 'A'
00360          GO TO 1000-ADD-RECORD.
00361
00362      IF FMAINTI = 'C'
00363          GO TO 2000-CHANGE-RECORD.
00364
00365      GO TO 3000-DELETE-RECORD.
00366
00367      EJECT
00368
00369  1000-ADD-RECORD       SECTION.
00370
00371      
      * EXEC CICS GETMAIN
00372 *        SET      (ADDRESS OF CERTIFICATE-NOTE)
00373 *        LENGTH   (825)
00374 *        INITIMG  (GETMAIN-SPACE)
00375 *    END-EXEC.
           MOVE 825
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00002009' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00376
00377      MOVE  SPACES                TO  CERTIFICATE-NOTE.
00378
00379      MOVE  'CN'                  TO  CN-RECORD-ID.
00380
041320     MOVE  PI-CARRIER            TO  WS-CN-CARRIER
041320     MOVE  PI-GROUPING           TO  WS-CN-GROUPING
041320     MOVE  PI-STATE              TO  WS-CN-STATE
041320     MOVE  PI-ACCOUNT            TO  WS-CN-ACCOUNT
041320     MOVE  PI-CERT-PRIME         TO  WS-CN-CERT-PRIME
041320     MOVE  PI-CERT-SFX           TO  WS-CN-CERT-SFX
041320     MOVE  PI-CERT-EFF-DT        TO  WS-CN-CERT-EFF-DT
041320     MOVE  PI-COMPANY-CD         TO  WS-CN-COMPANY-CD
041320     IF PI-CANCEL-NOTES
041320        MOVE '2'                 TO  WS-CN-RECORD-TYPE
041320     ELSE
041320        MOVE '1'                 TO  WS-CN-RECORD-TYPE
041320     END-IF
041320
041320     MOVE  WS-CN-CONTROL-PRIMARY TO  CN-CONTROL-PRIMARY
041320                                     ws-cm-control-primary
00391
00392      IF FBSTARTL GREATER ZERO
00393          MOVE FBSTARTI           TO  CN-BILLING-START-LINE-NO
00394      ELSE
00395          MOVE ZEROS              TO  CN-BILLING-START-LINE-NO.
00396
00397      IF FBENDL GREATER ZERO
00398          MOVE FBENDI             TO  CN-BILLING-END-LINE-NO
00399      ELSE
00400          MOVE ZEROS              TO  CN-BILLING-END-LINE-NO.
00401
00402      IF FBENDL = ZEROS
00403          IF FBSTARTL GREATER ZERO
00404             MOVE FBSTARTI        TO  CN-BILLING-END-LINE-NO.
00405
00406      MOVE +0                     TO  WS-SUB1.
00407
00408  1100-BUILD-NOTE-LINES.
00409      ADD +1                      TO  WS-SUB1.
00410
00411      IF WS-SUB1 GREATER +10
00412         GO TO 1700-WRITE-RECORD.
00413
00414      IF DNOTE-LINE-LENGTH (WS-SUB1) GREATER ZERO
00415          MOVE DNOTE-LINE   (WS-SUB1) TO  CN-LINE (WS-SUB1)
00416          MOVE 'Y'                    TO  WS-NOTE-LINE-SW
00417        ELSE
00418          MOVE SPACES                 TO  CN-LINE (WS-SUB1).
00419
00420      GO TO 1100-BUILD-NOTE-LINES.
00421
00422  1700-WRITE-RECORD.
00423      IF NOTE-LINES-PRESENT
00424          NEXT SENTENCE
00425      ELSE
00426          MOVE ER-2523            TO  EMI-ERROR
00427          MOVE -1                 TO  FMAINTL
00428          PERFORM 9900-ERROR-FORMAT
00429          GO TO 8200-SEND-DATAONLY.
00430
00431      MOVE PI-PROCESSOR-ID        TO  CN-LAST-MAINT-USER.
00432      MOVE EIBTIME                TO  CN-LAST-MAINT-HHMMSS.
00433      MOVE SAVE-BIN-DATE          TO  CN-LAST-MAINT-DT.
00434
00435      MOVE 'A'                    TO  JP-RECORD-TYPE.
00436      MOVE CERTIFICATE-NOTE       TO  JP-RECORD-AREA.
00437
00438      PERFORM 6400-WRITE-NOTE-RECORD.
00439
00440      IF DUPLICATE-RECORD-FOUND
00441          GO TO 1800-DUPLICATE-RECORD.
00442
00443      COMPUTE WS-JOURNAL-RECORD-LENGTH =
00444              WS-ERNOTE-RECORD-LENGTH  +  23.
00445
00446      PERFORM 8400-LOG-JOURNAL-RECORD.
00447      MOVE ER-0000                TO  EMI-ERROR.
00448      PERFORM 9900-ERROR-FORMAT.
00449      PERFORM 7000-FORMAT-SCREEN.
00450      PERFORM 4000-CERTIFICATE-UPDATE.
00451      GO TO 8100-SEND-INITIAL-MAP.
00452
00453  1800-DUPLICATE-RECORD.
00454      MOVE ER-2525                TO  EMI-ERROR.
00455      PERFORM 9900-ERROR-FORMAT.
00456      PERFORM 7000-FORMAT-SCREEN.
00457      GO TO 8100-SEND-INITIAL-MAP.
00458
00459  1900-EXIT.
00460      EXIT.
00461
00462      EJECT
00463
00464  2000-CHANGE-RECORD   SECTION.
00465
041320     MOVE  PI-CARRIER            TO  WS-CN-CARRIER
041320     MOVE  PI-GROUPING           TO  WS-CN-GROUPING
041320     MOVE  PI-STATE              TO  WS-CN-STATE
041320     MOVE  PI-ACCOUNT            TO  WS-CN-ACCOUNT
041320     MOVE  PI-CERT-PRIME         TO  WS-CN-CERT-PRIME
041320     MOVE  PI-CERT-SFX           TO  WS-CN-CERT-SFX
041320     MOVE  PI-CERT-EFF-DT        TO  WS-CN-CERT-EFF-DT
041320     MOVE  PI-COMPANY-CD         TO  WS-CN-COMPANY-CD
041320     IF PI-CANCEL-NOTEs
041320        MOVE '2'                 TO  WS-CN-RECORD-TYPE
041320     ELSE
041320        MOVE '1'                 TO  WS-CN-RECORD-TYPE
041320     END-IF
041320
041320     MOVE WS-CN-CONTROL-PRIMARY  TO WS-CM-CONTROL-PRIMARY
00475      PERFORM 6300-READ-NOTE-FILE-UPDT
00476
00477      IF RECORD-NOT-FOUND
00478          GO TO 1000-ADD-RECORD.
00479
00480      MOVE 'B'                    TO  JP-RECORD-TYPE.
00481      MOVE CERTIFICATE-NOTE       TO  JP-RECORD-AREA.
00482
00483      COMPUTE WS-JOURNAL-RECORD-LENGTH =
00484              WS-ERNOTE-RECORD-LENGTH  +  23.
00485
00486      PERFORM 8400-LOG-JOURNAL-RECORD.
00487
00488      IF FBSTARTL GREATER ZERO
00489          MOVE FBSTARTI           TO  CN-BILLING-START-LINE-NO
00490      ELSE
00491          MOVE ZEROS              TO  CN-BILLING-START-LINE-NO.
00492
00493      IF FBENDL GREATER ZERO
00494          MOVE FBENDI             TO  CN-BILLING-END-LINE-NO
00495      ELSE
00496          MOVE ZEROS              TO  CN-BILLING-END-LINE-NO.
00497
00498      MOVE +0                     TO  WS-SUB1.
00499
00500  2100-BUILD-NOTE-LINES.
00501      ADD +1                      TO  WS-SUB1.
00502
00503      IF WS-SUB1 GREATER 10
00504         GO TO 2700-REWRITE-RECORD.
00505
00506      IF DNOTE-LINE-LENGTH (WS-SUB1) GREATER ZERO
00507          MOVE DNOTE-LINE   (WS-SUB1) TO  CN-LINE (WS-SUB1)
00508          MOVE 'Y'                    TO  WS-NOTE-LINE-SW
00509      ELSE
00510          MOVE SPACES                 TO  CN-LINE (WS-SUB1).
00511
00512      GO TO 2100-BUILD-NOTE-LINES.
00513
00514  2700-REWRITE-RECORD.
00515      IF NOTE-LINES-PRESENT
00516          NEXT SENTENCE
00517      ELSE
00518          MOVE ER-2523            TO  EMI-ERROR
00519          MOVE -1                 TO  FMAINTL
00520          PERFORM 9900-ERROR-FORMAT
00521          GO TO 8200-SEND-DATAONLY.
00522
00523      MOVE PI-PROCESSOR-ID        TO  CN-LAST-MAINT-USER.
00524      MOVE EIBTIME                TO  CN-LAST-MAINT-HHMMSS.
00525      MOVE SAVE-BIN-DATE          TO  CN-LAST-MAINT-DT.
00526
00527      MOVE 'C'                    TO  JP-RECORD-TYPE.
00528      MOVE CERTIFICATE-NOTE       TO  JP-RECORD-AREA.
00529
00530      PERFORM 6500-REWRITE-NOTE-RECORD.
00531
00532      COMPUTE WS-JOURNAL-RECORD-LENGTH =
00533              WS-ERNOTE-RECORD-LENGTH  +  23.
00534
00535      PERFORM 8400-LOG-JOURNAL-RECORD.
00536
00537      MOVE ER-0000                TO  EMI-ERROR.
00538      PERFORM 9900-ERROR-FORMAT.
00539      PERFORM 7000-FORMAT-SCREEN.
00540      PERFORM 4000-CERTIFICATE-UPDATE.
00541      GO TO 8100-SEND-INITIAL-MAP.
00542
00543  2900-EXIT.
00544      EXIT.
00545
00546      EJECT
00547
00548  3000-DELETE-RECORD    SECTION.
00549
00550      MOVE  PI-CARRIER            TO  WS-CN-CARRIER.
00551      MOVE  PI-GROUPING           TO  WS-CN-GROUPING.
00552      MOVE  PI-STATE              TO  WS-CN-STATE.
00553      MOVE  PI-ACCOUNT            TO  WS-CN-ACCOUNT.
00554      MOVE  PI-CERT-PRIME         TO  WS-CN-CERT-PRIME.
00555      MOVE  PI-CERT-SFX           TO  WS-CN-CERT-SFX.
00556      MOVE  PI-CERT-EFF-DT        TO  WS-CN-CERT-EFF-DT.
00557      MOVE  PI-COMPANY-CD         TO  WS-CN-COMPANY-CD.
           IF PI-CANCEL-NOTEs
              MOVE '2'                 TO  WS-CN-RECORD-TYPE
           ELSE
              MOVE '1'                 TO  WS-CN-RECORD-TYPE
           END-IF
           MOVE WS-CN-CONTROL-PRIMARY  TO WS-CM-CONTROL-PRIMARY
00559      PERFORM 6300-READ-NOTE-FILE-UPDT.
00560
00561      IF RECORD-NOT-FOUND
00562         GO TO 3600-RECORD-PREVS-DELETED.
00563
00564      MOVE 'D'                    TO  JP-RECORD-TYPE.
00565      MOVE CERTIFICATE-NOTE       TO  JP-RECORD-AREA.
00566
00567      PERFORM 6600-DELETE-NOTE-RECORD.
00568
00569      COMPUTE WS-JOURNAL-RECORD-LENGTH =
00570              WS-ERNOTE-RECORD-LENGTH  +  23.
00571
00572      PERFORM 8400-LOG-JOURNAL-RECORD.
00573      PERFORM 4000-CERTIFICATE-UPDATE.
00574
00575  3500-RECORD-DELETED.
00576      MOVE SPACE                  TO  FMAINTO.
00577      MOVE ER-0000                TO  EMI-ERROR.
00578      MOVE -1                     TO  FMAINTL.
00579      PERFORM 9900-ERROR-FORMAT.
00580      GO TO 8200-SEND-DATAONLY.
00581
00582  3600-RECORD-PREVS-DELETED.
00583      MOVE ER-2528                TO  EMI-ERROR.
00584      MOVE -1                     TO  FMAINTL.
00585      PERFORM 9900-ERROR-FORMAT.
00586      GO TO 8200-SEND-DATAONLY.
00587
00588  3900-EXIT.
00589      EXIT.
00590
00591      EJECT
00592
00593  4000-CERTIFICATE-UPDATE         SECTION.
00594
00595      
      * EXEC CICS HANDLE CONDITION
00596 *        NOTFND   (4400-NOT-FOUND)
00597 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00002250' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032323530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00598
00599      
      * EXEC CICS READ
00600 *        EQUAL
00601 *        DATASET   (ELCERT-ID)
00602 *        SET       (ADDRESS OF CERTIFICATE-MASTER)
00603 *        RIDFLD    (WS-CM-CONTROL-PRIMARY)
00604 *        UPDATE
00605 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00002254' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00606
00607      IF FMAINTI = 'D'
092309       IF CM-NOTE-SW = '2'
092309         MOVE SPACE              TO  CM-NOTE-SW
092309       ELSE
092309         IF CM-NOTE-SW = '3'
092309            MOVE '1'             TO  CM-NOTE-SW
092309         ELSE
092309           IF CM-NOTE-SW = '6'
092309              MOVE '4'           TO  CM-NOTE-SW
092309           ELSE
092309             IF CM-NOTE-SW = '7'
092309                MOVE '5'         TO  CM-NOTE-SW
092309             END-IF
092309           END-IF
092309         END-IF
092309       END-IF
092309       MOVE 'N'                  TO  PI-BILLING-NOTES-EXIST
092309       GO TO 4200-REWRITE-CERT-MASTER
092309     END-IF.
092309
092309     IF CM-NOTE-SW = ' '
092309         MOVE '2'                TO  CM-NOTE-SW
092309     ELSE
092309       IF CM-NOTE-SW = '1'
092309          MOVE '3'               TO  CM-NOTE-SW
092309       ELSE
092309         IF CM-NOTE-SW = '4'
092309            MOVE '6'             TO  CM-NOTE-SW
092309         ELSE
092309           IF CM-NOTE-SW = '5'
092309              MOVE '7'           TO  CM-NOTE-SW
092309           END-IF
092309         END-IF
092309       END-IF
092309     END-IF.
092309     MOVE 'Y'                    TO  PI-BILLING-NOTES-EXIST
092309     GO TO 4200-REWRITE-CERT-MASTER.
00610
00611      MOVE 'B'                    TO  JP-RECORD-TYPE.
00612      MOVE CERTIFICATE-MASTER     TO  JP-RECORD-AREA.
00613
00614      COMPUTE WS-JOURNAL-RECORD-LENGTH =
00615              WS-ELCERT-RECORD-LENGTH  +  23.
00616
00617      PERFORM 8400-LOG-JOURNAL-RECORD.
00618
092309*00619      IF CN-BILLING-START-LINE-NO = ZERO
092309*00620          MOVE '1'                TO  CM-NOTE-SW
092309*00621          GO TO 4200-REWRITE-CERT-MASTER.
092309*00622
092309*00623      MOVE +0                     TO  WS-SUB1.
092309*00624      MOVE +2                     TO  CM-NOTE-SW.
092309*00625
092309*00626  4100-CHECK-NOTE-LINE.
092309*00627      ADD +1  TO  WS-SUB1.
092309*00628
092309*00629      IF WS-SUB1 GREATER +10
092309*00630          GO TO 4200-REWRITE-CERT-MASTER.
092309*00631
092309*00632      IF WS-SUB1 LESS    CN-BILLING-START-LINE-NO  OR
092309*00633                 GREATER CN-BILLING-END-LINE-NO
092309*00634          NEXT SENTENCE
092309*00635      ELSE
092309*00636          GO TO 4100-CHECK-NOTE-LINE.
092309*00637
092309*00638      IF CN-LINE (WS-SUB1) GREATER THAN SPACES
092309*00639          MOVE  3                 TO  CM-NOTE-SW
092309*00640          GO TO 4200-REWRITE-CERT-MASTER
092309*00641      ELSE
092309*00642          GO TO 4100-CHECK-NOTE-LINE.
00643
00644  4200-REWRITE-CERT-MASTER.
00645      MOVE 'C'                    TO  JP-RECORD-TYPE.
00646      MOVE CERTIFICATE-MASTER     TO  JP-RECORD-AREA.
00647
00648      
      * EXEC CICS REWRITE
00649 *        FROM      (CERTIFICATE-MASTER)
00650 *        DATASET   (ELCERT-ID)
00651 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002337' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00652
00653      COMPUTE WS-JOURNAL-RECORD-LENGTH =
00654              WS-ELCERT-RECORD-LENGTH  +  23.
00655
00656      PERFORM 8400-LOG-JOURNAL-RECORD.
00657
00658      GO TO 4500-EXIT.
00659
00660  4400-NOT-FOUND.
00661      MOVE -1                     TO  FMAINTL.
00662      MOVE ER-0142                TO  EMI-ERROR.
00663      PERFORM 9900-ERROR-FORMAT.
00664      GO TO 8200-SEND-DATAONLY.
00665
00666  4500-EXIT.
00667      EXIT.
00668
00669      EJECT
00670
00671  6200-READ-NOTE-FILE             SECTION.
00672
00673      
      * EXEC CICS HANDLE CONDITION
00674 *        NOTFND   (6250-NOT-FOUND)
00675 *    END-EXEC.
      *    MOVE '"$I                   ! % #00002362' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303032333632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00676
00677      
      * EXEC CICS READ
00678 *        EQUAL
00679 *        DATASET   (ERNOTE-ID)
00680 *        SET       (ADDRESS OF CERTIFICATE-NOTE)
041320*        RIDFLD    (WS-CN-CONTROL-PRIMARY)
00682 *    END-EXEC.
      *    MOVE '&"S        E          (   #00002366' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CN-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00683
00684      GO TO 6290-EXIT.
00685
00686  6250-NOT-FOUND.
00687      MOVE 'N'                    TO  WS-RECORD-FOUND-SW.
00688
00689  6290-EXIT.
00690       EXIT.
00691
00692      EJECT
00693
00694  6300-READ-NOTE-FILE-UPDT        SECTION.
00695
00696      
      * EXEC CICS HANDLE CONDITION
00697 *        NOTFND   (6350-NOT-FOUND)
00698 *    END-EXEC.
      *    MOVE '"$I                   ! & #00002385' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303032333835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00699
00700      
      * EXEC CICS READ
00701 *        EQUAL
00702 *        DATASET   (ERNOTE-ID)
00703 *        SET       (ADDRESS OF CERTIFICATE-NOTE)
041320*        RIDFLD    (WS-CN-CONTROL-PRIMARY)
00705 *        UPDATE
00706 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00002389' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CN-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00707
00708         GO TO 6390-EXIT.
00709
00710  6350-NOT-FOUND.
00711      MOVE 'N'                    TO  WS-RECORD-FOUND-SW.
00712
00713  6390-EXIT.
00714       EXIT.
00715
00716       EJECT
00717
00718  6400-WRITE-NOTE-RECORD          SECTION.
00719
00720      
      * EXEC CICS HANDLE CONDITION
00721 *        DUPREC   (6450-DUPLICATE-RECORD)
00722 *    END-EXEC.
      *    MOVE '"$%                   ! '' #00002409' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303032343039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00723
00724      
      * EXEC CICS WRITE
00725 *        FROM      (CERTIFICATE-NOTE)
00726 *        DATASET   (ERNOTE-ID)
041320*        RIDFLD    (CN-CONTROL-PRIMARY)
00728 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00002413' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 CN-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00729
00730         GO TO 6490-EXIT.
00731
00732  6450-DUPLICATE-RECORD.
00733      MOVE 'Y'                    TO  WS-DUPREC-SW.
00734
00735  6490-EXIT.
00736      EXIT.
00737
00738      EJECT
00739
00740  6500-REWRITE-NOTE-RECORD        SECTION.
00741
00742      
      * EXEC CICS REWRITE
00743 *        FROM      (CERTIFICATE-NOTE)
00744 *        DATASET   (ERNOTE-ID)
00745 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002431' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00746
00747
00748  6590-EXIT.
00749      EXIT.
00750
00751      EJECT
00752
00753  6600-DELETE-NOTE-RECORD         SECTION.
00754
00755      
      * EXEC CICS HANDLE CONDITION
00756 *        NOTFND (6650-RECORD-PREVS-DELETED)
00757 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00002444' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303032343434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00758
00759      
      * EXEC CICS DELETE
00760 *        DATASET   (ERNOTE-ID)
00761 *    END-EXEC.
      *    MOVE '&(                    &   #00002448' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00762
00763      GO TO 6690-EXIT.
00764
00765  6650-RECORD-PREVS-DELETED.
00766      MOVE 'N'                    TO  WS-RECORD-FOUND-SW.
00767
00768  6690-EXIT.
00769      EXIT.
00770
00771      EJECT
00772
00773  7000-FORMAT-SCREEN              SECTION.
00774
00775      MOVE LOW-VALUES             TO  EL127FI.
00776
00777      MOVE  PI-CARRIER            TO  FCARRIRO.
00778      MOVE  PI-GROUPING           TO  FGROUPO.
00779      MOVE  PI-STATE              TO  FSTO
00780      MOVE  PI-ACCOUNT            TO  FACOUNTO.
00781      MOVE  PI-CERT-PRIME         TO  FCERTO.
00782      MOVE  PI-CERT-SFX           TO  FCRTSFXO.
00783      MOVE  ' '                   TO  DC-OPTION-CODE.
00784      MOVE  PI-CERT-EFF-DT        TO  DC-BIN-DATE-1.
00785      PERFORM 9700-DATE-LINK.
00786      MOVE DC-GREG-DATE-1-EDIT    TO  FEFFDTO.
092309     IF PI-CERT-NOTES-EXIST EQUAL 'Y'
092309         MOVE 'YES'              TO  FCERTYNO
092309     ELSE
092309         MOVE 'NO '              TO  FCERTYNO
092309     END-IF.
092309     IF PI-CLAIM-NOTES-EXIST EQUAL 'Y'
092309         MOVE 'YES'              TO  FCLMYNO
092309     ELSE
092309         MOVE 'NO '              TO  FCLMYNO
092309     END-IF.
041320     if pi-cancel-notes
041320        move '*****  C A N C E L  *****'
041320                                 TO FHEADO
041320        move 'PF7=ISSUE NOTES'   TO FPF7HO
041320        move '2'                 to ws-cn-record-type
041320     ELSE
041320        move '*****   I S S U E   *****'
041320                                 TO FHEADO
041320        MOVE 'PF7=CANCEL NOTES'  TO FPF7HO
041320        move '1'                 to ws-cn-record-type
041320     END-IF
041320     MOVE  PI-CARRIER            TO  WS-CN-CARRIER
041320     MOVE  PI-GROUPING           TO  WS-CN-GROUPING
041320     MOVE  PI-STATE              TO  WS-CN-STATE
041320     MOVE  PI-ACCOUNT            TO  WS-CN-ACCOUNT
041320     MOVE  PI-CERT-PRIME         TO  WS-CN-CERT-PRIME
041320     MOVE  PI-CERT-SFX           TO  WS-CN-CERT-SFX
041320     MOVE  PI-CERT-EFF-DT        TO  WS-CN-CERT-EFF-DT
041320     MOVE  PI-COMPANY-CD         TO  WS-CN-COMPANY-CD
00797      PERFORM 6200-READ-NOTE-FILE.
00798
00799      IF RECORD-NOT-FOUND
00800         MOVE 'A'                 TO  FMAINTI
00801         MOVE AL-UANON            TO  FMAINTA
00802         GO TO 7900-EXIT.
00803
00804      MOVE 'C'                    TO  FMAINTO.
00805      MOVE AL-UANON               TO  FMAINTA.
00806
00807      IF CN-LAST-MAINT-DT = SPACES
00808          MOVE EIBTIME            TO  CN-LAST-MAINT-HHMMSS
00809          MOVE SAVE-BIN-DATE      TO  CN-LAST-MAINT-DT.
00810
00811      IF CN-LAST-MAINT-DT NOT = SPACES AND LOW-VALUES
00812          MOVE  ' '               TO  DC-OPTION-CODE
00813          MOVE  CN-LAST-MAINT-DT  TO  DC-BIN-DATE-1
00814          PERFORM 9700-DATE-LINK
00815          MOVE DC-GREG-DATE-1-EDIT TO  MAINTONO.
00816
00817      IF CN-LAST-MAINT-HHMMSS NUMERIC
00818          MOVE CN-LAST-MAINT-HHMMSS TO TIME-IN
00819          MOVE TIME-OUT             TO MAINTATO
00820        ELSE
00821          MOVE ZEROS                TO TIME-IN
00822          MOVE TIME-OUT             TO MAINTATO.
00823
00824      MOVE CN-LAST-MAINT-USER       TO MAINTBYO.
00825
00826      IF CN-BILLING-START-LINE-NO GREATER THAN ZEROS
00827          MOVE CN-BILLING-START-LINE-NO   TO  FBSTARTO
00828          MOVE AL-UNNON                   TO  FBSTARTA.
00829
00830      IF CN-BILLING-END-LINE-NO GREATER THAN ZEROS
00831          MOVE CN-BILLING-END-LINE-NO     TO  FBENDO
00832          MOVE AL-UNNON                   TO  FBENDA.
00833
00834      MOVE +0                     TO  WS-SUB1.
00835
00836  7100-BUILD-NOTE-LINES.
00837      ADD +1 TO  WS-SUB1.
00838
00839      IF WS-SUB1 GREATER THAN +10
00840         GO TO 7900-EXIT.
00841
00842      MOVE  CN-LINE (WS-SUB1)     TO  DNOTE-LINE       (WS-SUB1).
00843      MOVE  AL-UANON              TO  DNOTE-LINE-ATTRB (WS-SUB1).
00844
00845      GO TO 7100-BUILD-NOTE-LINES.
00846
00847  7900-EXIT.
00848      EXIT.
00849
00850      EJECT
00851
00852  8100-SEND-INITIAL-MAP SECTION.
00853
00854      MOVE SAVE-DATE              TO  FDATEO.
00855      MOVE EIBTIME                TO  TIME-IN.
00856      MOVE TIME-OUT               TO  FTIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00857      MOVE -1                     TO  FMAINTL.
00858      MOVE EMI-MESSAGE-AREA (1)   TO  FERRMSGO.
00859      
      * EXEC CICS SEND
00860 *        MAP      (MAP-NAME)
00861 *        MAPSET   (MAPSET-NAME)
00862 *        FROM     (EL127FO)
00863 *        ERASE
00864 *        CURSOR
00865 *    END-EXEC.
           MOVE LENGTH OF
            EL127FO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002569' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127FO, 
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
           
00866
00867      GO TO 9100-RETURN-TRAN.
00868
00869  8200-SEND-DATAONLY     SECTION.
00870
00871      MOVE SAVE-DATE              TO  FDATEO.
00872      MOVE EIBTIME                TO  TIME-IN.
00873      MOVE TIME-OUT               TO  FTIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00874      MOVE EMI-MESSAGE-AREA (1)   TO  FERRMSGO.
00875      
      * EXEC CICS SEND
00876 *        MAP      (MAP-NAME)
00877 *        MAPSET   (MAPSET-NAME)
00878 *        FROM     (EL127FO)
00879 *        DATAONLY
00880 *        ERASEAUP
00881 *        CURSOR
00882 *    END-EXEC.
           MOVE LENGTH OF
            EL127FO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00002587' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127FO, 
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
           
00883
00884      GO TO 9100-RETURN-TRAN.
00885
00886      EJECT
00887
00888  8300-SEND-TEXT         SECTION.
00889      
      * EXEC CICS SEND TEXT
00890 *        FROM     (LOGOFF-TEXT)
00891 *        LENGTH   (LOGOFF-LENGTH)
00892 *        ERASE
00893 *        FREEKB
00894 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002601' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363031' TO DFHEIV0(25:11)
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
           
00895
00896      
      * EXEC CICS RETURN
00897 *    END-EXEC.
      *    MOVE '.(                    ''   #00002608' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00898
00899      EJECT
00900
00901  8400-LOG-JOURNAL-RECORD         SECTION.
00902
00903 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
00904 *    MOVE ERNOTE-ID              TO  JP-FILE-ID.
00905 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.
00906 *    EXEC CICS JOURNAL
00907 *        JFILEID     (PI-JOURNAL-FILE-ID)
00908 *        JTYPEID     ('ER')
00909 *        FROM        (JOURNAL-RECORD)
00910 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
00911 *    END-EXEC.
00912
00913  8400-EXIT.
00914      EXIT.
00915
00916  8600-DEEDIT           SECTION.
00917      
      * EXEC CICS BIF DEEDIT
00918 *         FIELD   (DEEDIT-FIELD)
00919 *         LENGTH  (15)
00920 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002629' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00921
00922  8800-UNAUTHORIZED-ACCESS        SECTION.
00923      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
00924      GO TO 8300-SEND-TEXT.
00925
00926  8810-PF23              SECTION.
00927      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
00928      MOVE XCTL-005               TO  PGM-NAME.
00929      GO TO 9300-XCTL.
00930
00931  9100-RETURN-TRAN       SECTION.
00932      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
00933      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
00934      
      * EXEC CICS RETURN
00935 *        TRANSID    (TRANS-ID)
00936 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
00937 *        LENGTH     (PI-COMM-LENGTH)
00938 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00002646' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00939
00940  9200-RETURN-MAIN-MENU SECTION.
00941      MOVE XCTL-626               TO  PGM-NAME.
00942      GO TO 9300-XCTL.
00943
00944  9300-XCTL             SECTION.
00945      
      * EXEC CICS XCTL
00946 *        PROGRAM    (PGM-NAME)
00947 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
00948 *        LENGTH     (PI-COMM-LENGTH)
00949 *    END-EXEC.
      *    MOVE '.$C                   %   #00002657' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00950
00951  9400-CLEAR            SECTION.
00952      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
00953      GO TO 9300-XCTL.
00954
00955  9500-PF12             SECTION.
00956      MOVE XCTL-010               TO  PGM-NAME.
00957      GO TO 9300-XCTL.
00958
00959  9600-PGMID-ERROR      SECTION.
00960      
      * EXEC CICS HANDLE CONDITION
00961 *        PGMIDERR    (8300-SEND-TEXT)
00962 *    END-EXEC.
      *    MOVE '"$L                   ! ) #00002672' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303032363732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00963
00964      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
00965      MOVE ' '                    TO  PI-ENTRY-CD-1.
00966      MOVE XCTL-005               TO  PGM-NAME.
00967      MOVE PGM-NAME               TO  LOGOFF-PGM.
00968      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
00969      GO TO 9300-XCTL.
00970
00971  9700-DATE-LINK         SECTION.
00972      MOVE LINK-ELDATCV           TO  PGM-NAME
00973      
      * EXEC CICS LINK
00974 *        PROGRAM    (PGM-NAME)
00975 *        COMMAREA   (DATE-CONVERSION-DATA)
00976 *        LENGTH     (DC-COMM-LENGTH)
00977 *    END-EXEC.
      *    MOVE '."C                   (   #00002685' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00978
00979  9900-ERROR-FORMAT       SECTION.
00980      IF NOT EMI-ERRORS-COMPLETE
00981          MOVE LINK-001           TO  PGM-NAME
00982          
      * EXEC CICS LINK
00983 *            PROGRAM    (PGM-NAME)
00984 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
00985 *            LENGTH     (EMI-COMM-LENGTH)
00986 *        END-EXEC.
      *    MOVE '."C                   (   #00002694' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00987
00988  9900-EXIT.
00989      EXIT.
00990
00991  9990-ABEND             SECTION.
00992      MOVE LINK-004               TO  PGM-NAME.
00993      MOVE DFHEIBLK               TO  EMI-LINE1
00994
00995      
      * EXEC CICS LINK
00996 *        PROGRAM   (PGM-NAME)
00997 *        COMMAREA  (EMI-LINE1)
00998 *        LENGTH    (72)
00999 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00002707' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01000
01001      MOVE -1                     TO  FMAINTL.
01002      GO TO 8200-SEND-DATAONLY.
01003
01004  9995-SECURITY-VIOLATION.
01005 *    COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00002734' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373334' TO DFHEIV0(25:11)
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
01006
01007  9995-EXIT.
01008      EXIT.
01009

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1276' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9400-CLEAR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 4400-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 6250-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 6350-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 6450-DUPLICATE-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 6650-RECORD-PREVS-DELETED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1276' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
