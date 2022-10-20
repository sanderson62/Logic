00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL300
00003  PROGRAM-ID.                 EL300 .                                 LV042
00004 *              PROGRAM CONVERTED BY                               EL300
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL300
00006 *              CONVERSION DATE 02/12/96 12:24:26.                 EL300
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE.         EL300
00008 *                            VMOD=2.034.                          EL300
00009                                                                   EL300
00010 *AUTHOR.     LOGIC, INC.                                          EL300
00011 *            DALLAS, TEXAS.                                       EL300
00012                                                                   EL300
00013 *DATE-COMPILED.                                                   EL300
00014                                                                   EL300
00015 *SECURITY.   *****************************************************EL300
00016 *            *                                                   *EL300
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL300
00018 *            *                                                   *EL300
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL300
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL300
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL300
00022 *            *                                                   *EL300
00023 *            *****************************************************EL300
00024                                                                   EL300
00025 *REMARKS.                                                         EL300
00026                                                                   EL300
00027 *    LOAD ENVIRONMENT FILE AND DISPLAY OPTIONS SELECTED.          EL300
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 600 TO 900
092602******************************************************************
00028                                                                   EL300
00029  ENVIRONMENT DIVISION.                                            EL300
00030  CONFIGURATION SECTION.                                           EL300
00031  SPECIAL-NAMES.                                                   EL300
00032      C02 IS LCP-CH2                                               EL300
00033      C03 IS LCP-CH3                                               EL300
00034      C04 IS LCP-CH4                                               EL300
00035      C05 IS LCP-CH5                                               EL300
00036      C06 IS LCP-CH6                                               EL300
00037      C07 IS LCP-CH7                                               EL300
00038      C08 IS LCP-CH8                                               EL300
00039      C09 IS LCP-CH9                                               EL300
00040      C10 IS LCP-CH10                                              EL300
00041      C11 IS LCP-CH11                                              EL300
00042      C12 IS LCP-CH12                                              EL300
00043      S01 IS LCP-P01                                               EL300
00044      S02 IS LCP-P02.                                              EL300
00045  INPUT-OUTPUT SECTION.                                            EL300
00046  FILE-CONTROL.                                                    EL300
00047                                                                   EL300
00048      SELECT CARD-FILE ASSIGN TO SYS006.
00049      SELECT PRNT-FILE ASSIGN TO SYS008-UR-1403-S-SYS008.          EL300
00050      SELECT DISK-FILE ASSIGN TO SYS019-UT-3380-S-SYS019.          EL300
00051                                                                   EL300
00052      SELECT ELCNTL    ASSIGN TO SYS010-3380-ELCNTL                EL300
00053                       ORGANIZATION IS INDEXED                     EL300
00054                       ACCESS IS DYNAMIC                           EL300
00055                       RECORD KEY IS CF-CONTROL-PRIMARY            EL300
00056                       FILE STATUS IS ELCNTL-FILE-STATUS.          EL300
00057                                                                   EL300
00058      SELECT ELPGMO    ASSIGN TO SYS011-3380-ELPGMO                EL300
00059                       ORGANIZATION IS INDEXED                     EL300
00060                       ACCESS IS DYNAMIC                           EL300
00061                       RECORD KEY IS PO-CONTROL-PRIMARY            EL300
00062                       FILE STATUS IS ELPGMO-FILE-STATUS.          EL300
00063                                                                   EL300
00064      SELECT ELPGMS    ASSIGN TO SYS012-3380-ELPGMS                EL300
00065                       ORGANIZATION IS INDEXED                     EL300
00066                       ACCESS IS DYNAMIC                           EL300
00067                       RECORD KEY IS PS-CONTROL-PRIMARY            EL300
00068                       FILE STATUS IS ELPGMS-FILE-STATUS.          EL300
00069                                                                   EL300
00070      SELECT ELPGMN    ASSIGN TO SYS013-3380-ELPGMN                EL300
00071                       ORGANIZATION IS INDEXED                     EL300
00072                       ACCESS IS DYNAMIC                           EL300
00073                       RECORD KEY IS PN-CONTROL-PRIMARY            EL300
00074                       FILE STATUS IS ELPGMN-FILE-STATUS.          EL300
00075                                                                   EL300
00076      EJECT                                                        EL300
00077  DATA DIVISION.                                                   EL300
00078  FILE SECTION.                                                    EL300
00079                                                                   EL300
00080  FD  CARD-FILE                                                    EL300
00081      RECORDING MODE F.                                            EL300
00082  01  INPUT-CARD.                                                  EL300
00083      12  FILLER                      PIC X(80).                   EL300
00084                                                                   EL300
00085  FD  PRNT-FILE                                                    EL300
00086                                  COPY ELCPRTFD.                   EL300
00087                                                                   EL300
00088      EJECT                                                        EL300
00089  FD  DISK-FILE                                                    EL300
00090                                  COPY ELCDTEFD.                   EL300
00091                                                                   EL300
00092      EJECT                                                        EL300
00093  FD  ELPGMN.                                                      EL300
00094      COPY ELCPGMN.                                                EL300
00095                                                                   EL300
00096      EJECT                                                        EL300
00097  FD  ELPGMS.                                                      EL300
00098      COPY ELCPGMS.                                                EL300
00099                                                                   EL300
00100      EJECT                                                        EL300
00101  FD  ELPGMO.                                                      EL300
00102      COPY ELCPGMO.                                                EL300
00103                                                                   EL300
00104      EJECT                                                        EL300
00105  FD  ELCNTL.                                                      EL300
00106      COPY ELCCNTL.                                                EL300
00107                                                                   EL300
00108      EJECT                                                        EL300
00109                                                                   EL300
00110  WORKING-STORAGE SECTION.                                         EL300
00111  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL300
00112  77  LCP-ASA                       PIC X.                         EL300
00113                                                                   EL300
00114  77  FILLER  PIC X(32) VALUE '********************************'.  EL300
00115  77  FILLER  PIC X(32) VALUE '*    EL300  WORKING-STORAGE    *'.  EL300
00116  77  FILLER  PIC X(32) VALUE '**** VMOD=2.034 ****************'.  EL300
00117                                                                   EL300
00118  77  RX1                COMP         PIC S9(4) VALUE ZERO.        EL300
00119  77  X1                 COMP         PIC S9(4) VALUE ZERO.        EL300
00120  77  X2                 COMP         PIC S9(4) VALUE ZERO.        EL300
00121  77  X3                 COMP         PIC S9(4) VALUE ZERO.        EL300
00122  77  X4                 COMP         PIC S9(4) VALUE ZERO.        EL300
00123  77  ERR-X              COMP         PIC S9(4) VALUE ZERO.        EL300
00124  77  LINE-CNT           COMP-3       PIC S9(3) VALUE +100.        EL300
00125  77  PAGE-CNT           COMP-3       PIC S9(3) VALUE ZERO.        EL300
00126  77  NEW-PAGE           COMP-3       PIC S9(3) VALUE +49.         EL300
00127  77  END-PAGE           COMP-3       PIC S9(3) VALUE +55.         EL300
00128  77  CLAS-CNT           COMP-3       PIC S9(5) VALUE ZERO.        EL300
00129  77  CLAF-CNT           COMP-3       PIC S9(5) VALUE ZERO.        EL300
00130  77  COLC-CNT           COMP-3       PIC S9(5) VALUE ZERO.        EL300
00131  77  CARD-CNT           COMP-3       PIC S9(5) VALUE ZERO.        EL300
00132  77  OPTION-CNT         COMP-3       PIC S9(5) VALUE ZERO.        EL300
00133  77  SWITCH-CNT         COMP-3       PIC S9(5) VALUE ZERO.        EL300
00134  77  INS-CODE           COMP-3       PIC S9 VALUE ZERO.           EL300
00135  77  OLC-DT-OVRIDE-SW                PIC  X VALUE SPACE.          EL300
00136  77  FICHE-SW           COMP-3       PIC S9 VALUE ZERO.           EL300
00137  77  ERROR-SW           COMP-3       PIC S9 VALUE ZERO.           EL300
00138  77  WS-ZERO            COMP-3       PIC S9     VALUE ZERO.       EL300
00139  77  WS-RETURN-CODE     COMP         PIC S9(4) VALUE +1999.       EL300
00140  77  WS-ABEND-MESSAGE                PIC  X(70) VALUE SPACES.     EL300
00141  77  WS-ABEND-FILE-STATUS            PIC  XX    VALUE ZERO.       EL300
00142  77  CREDIT-OPEN-SW     COMP-3       PIC  S9    VALUE ZERO.       EL300
00143  77  CLASIC-ST-CALL-DEFAULT          PIC  X     VALUE SPACES.     EL300
00144                                                                   EL300
00145  01  FILLER.                                                      EL300
00146      12  CLASIC-RUN-DATE             PIC  9(08)      VALUE ZERO.  EL300
00147      12  ELCNTL-FILE-STATUS          PIC  X(02)      VALUE ZERO.  EL300
00148      12  ELPGMO-FILE-STATUS          PIC  X(02)      VALUE ZERO.  EL300
00149      12  ELPGMN-FILE-STATUS          PIC  X(02)      VALUE ZERO.  EL300
00150      12  ELPGMS-FILE-STATUS          PIC  X(02)      VALUE ZERO.  EL300
00151      12  W-CARRIER-SWITCH            PIC  X(01)      VALUE SPACE. EL300
00152          88  USE-GIVEN-CARRIER                       VALUE SPACE. EL300
00153      12  W-CNTL-IBNR-SW              PIC  X(01)      VALUE SPACE. EL300
00154      12  W-CNTL-IBNR-PERCENT         PIC S9(01)V9(04) COMP-3      EL300
00155                                                      VALUE ZEROS. EL300
00156      12  W-CNTL-PERCENT-OF-CIDA      PIC S9(01)V9(04) COMP-3      EL300
00157                                                      VALUE ZEROS. EL300
00158                                                                   EL300
00159                                                                   EL300
00160      EJECT                                                        EL300
00161      COPY ELCDATE.                                                EL300
00162      EJECT                                                        EL300
00163 *                                                                 EL300
00164  01  OPTION-TABLE.                                                EL300
00165      12  OT-OPTIONS-ID               PIC  X(4)     VALUE 'CLAD'.  EL300
00166      12  OT-MIN-PREM                 PIC  99V99 VALUE ZERO.       EL300
00167      12  OT-MP-REPORT-LANGUAGE-IND   PIC  X    VALUE SPACE.       EL300
00168      12  OT-MIN-AGE                  PIC  99    VALUE ZERO.       EL300
00169      12  OT-DEFAULT-AGE              PIC  99    VALUE ZERO.       EL300
00170      12  FILLER                      PIC  X    VALUE SPACE.       EL300
00171      12  OT-MAX-TERM                 PIC  9(3)     VALUE ZERO.    EL300
00172      12  OT-COMP-VG                  PIC  X    VALUE SPACE.       EL300
00173      12  OT-REM-TRM                  PIC  X    VALUE SPACE.       EL300
00174      12  OT-PRM-CK                   PIC S99V99 VALUE ZERO.       EL300
00175      12  OT-PRORATA                  PIC  X    VALUE SPACE.       EL300
00176      12  OT-CLM-REJ                  PIC  X    VALUE SPACE.       EL300
00177      12  OT-REF-REJ                  PIC  X    VALUE SPACE.       EL300
00178      12  OT-COM-TBL-USED             PIC  X    VALUE SPACE.       EL300
00179      12  OT-CERT-NAME                PIC  X    VALUE SPACE.       EL300
00180      12  OT-QTR-CO                   PIC  X    VALUE SPACE.       EL300
00181      12  OT-REF-CK                   PIC S99V99 VALUE ZERO.       EL300
00182      12  OT-CLM-CK                   PIC S99V99 VALUE ZERO.       EL300
00183      12  OT-CONV-DT                  PIC 9(11)  COMP-3 VALUE 0.   EL300
00184      12  OT-DEFAULT-SEX              PIC  X    VALUE SPACE.       EL300
00185      12  OT-EPL-FORMAT               PIC  X    VALUE SPACE.       EL300
00186      12  OT-CLICO                    PIC  X    VALUE SPACE.       EL300
00187      12  OT-JT-AGE                   PIC  X    VALUE SPACE.       EL300
00188      12  OT-KEY-BIRTH                PIC  X    VALUE SPACE.       EL300
00189      12  OT-REINSURANCE              PIC  X    VALUE SPACE.       EL300
00190      12  OT-CLAIM-SORT               PIC  X    VALUE SPACE.       EL300
00191      12  OT-WRT-OFF                  PIC  S99V99   VALUE ZERO.    EL300
00192      12  OT-R78                      PIC  X    VALUE SPACE.       EL300
00193      12  OT-DLY-BILL                 PIC  X    VALUE SPACE.       EL300
00194      12  OT-ALT-MORT-CODE            PIC  X(4)     VALUE SPACE.   EL300
00195      12  OT-CLAIM-PAID-THRU-TO       PIC  X(01)    VALUE SPACES.  EL300
00196      12  OT-COMPENSATION-ACCESS      PIC  X(01)    VALUE SPACES.  EL300
00197      12  OT-MORTG-ACCESS-CNTL        PIC  X(01)    VALUE SPACES.  EL300
00198      12  OT-MP-ALT-MORT-CODE         PIC  X(4)     VALUE SPACE.   EL300
00199      12  OT-MORTALITY-AGE-CALC-METHOD                             EL300
00200                                      PIC  X(1)     VALUE SPACE.   EL300
00201      12  OT-RESERVE-OPTION-SWITCH    PIC  X(01)    VALUE SPACE.   EL300
00202          88  OT-OPT-RESERVE-METHOD-AUTH            VALUE 'Y'.     EL300
00203          88  OT-OPT-RESERVE-METHOD-UNAUTH          VALUE ' '.     EL300
00204      12  OT-REM-TRM-CALC-OPTION      PIC  X(01)    VALUE SPACE.   EL300
00205      12  OT-EXPERIENCE-RETENT-AGE    PIC  9(01)    VALUE ZERO.    EL300
00206      12  OT-SYSTEM.                                               EL300
00207          16  OT-SYS-A-CREDIT         PIC  X    VALUE ZERO.        EL300
00208          16  OT-SYS-B-PEND-CLAIM     PIC  X    VALUE ZERO.        EL300
00209          16  OT-SYS-C-CONFIRMATIONS  PIC  X    VALUE ZERO.        EL300
00210          16  OT-SYS-D-DEMAND-BILL    PIC  X    VALUE ZERO.        EL300
00211          16  OT-SYS-E-CLAS-IC-CLAIM  PIC  X    VALUE ZERO.        EL300
00212          16  OT-SYS-F-CLAS-IC-CREDIT PIC  X    VALUE ZERO.        EL300
00213          16  OT-SYS-G-AR-USED        PIC  X    VALUE ZERO.        EL300
00214          16  OT-SYS-H                PIC  X    VALUE ZERO.        EL300
00215          16  OT-SYS-I                PIC  X    VALUE ZERO.        EL300
00216          16  OT-SYS-J                PIC  X    VALUE ZERO.        EL300
00217          16  OT-SYS-K                PIC  X    VALUE ZERO.        EL300
00218          16  OT-SYS-L                PIC  X    VALUE ZERO.        EL300
00219          16  OT-SYS-M                PIC  X    VALUE ZERO.        EL300
00220          16  OT-SYS-N                PIC  X    VALUE ZERO.        EL300
00221          16  OT-SYS-O                PIC  X    VALUE ZERO.        EL300
00222          16  OT-SYS-P                PIC  X    VALUE ZERO.        EL300
00223          16  OT-SYS-Q                PIC  X    VALUE ZERO.        EL300
00224          16  OT-SYS-R                PIC  X    VALUE ZERO.        EL300
00225          16  OT-SYS-S                PIC  X    VALUE ZERO.        EL300
00226          16  OT-SYS-T                PIC  X    VALUE ZERO.        EL300
00227          16  OT-SYS-U                PIC  X    VALUE ZERO.        EL300
00228          16  OT-SYS-V                PIC  X    VALUE ZERO.        EL300
00229          16  OT-SYS-W                PIC  X    VALUE ZERO.        EL300
00230          16  OT-SYS-X                PIC  X    VALUE ZERO.        EL300
00231          16  OT-SYS-Y                PIC  X    VALUE ZERO.        EL300
00232          16  OT-SYS-Z                PIC  X    VALUE ZERO.        EL300
00233      12  FILLER                      REDEFINES OT-SYSTEM.         EL300
00234          16  OT-SYS-CODE             OCCURS 26 TIMES              EL300
00235                                      PIC  X.                      EL300
00236      12  OT-CLIENT                   PIC  X(3)     VALUE SPACE.   EL300
00237 *                                                                 EL300
00238 *                                                                 EL300
00239  01  PRINT-AREA.                                                  EL300
00240      12  PRT-CTL                     PIC  9 VALUE 1.              EL300
00241      12  PRT-TBL                     PIC  X(4) VALUE '1 0-'.      EL300
00242      12  FILLER                      REDEFINES PRT-TBL.           EL300
00243          16  FILLER                  OCCURS 4 TIMES.              EL300
00244              20  PRINT-CTL           PIC  X.                      EL300
00245                                                                   EL300
00246  01  COMPANY-CONVERT-WORK.                                        EL300
00247      12  FULL-BINARY-CD              PIC S9(4)     COMP.          EL300
00248      12  ONE-BYTE-WORK  REDEFINES FULL-BINARY-CD.                 EL300
00249          16  FILLER                  PIC X.                       EL300
00250          16  COMPANY-CD-BYTE         PIC X.                       EL300
00251                                                                   EL300
00252  01  TELE-WORK-AREA.                                              EL300
00253      12  NUMERIC-TELE-NUMBER         PIC 9(11).                   EL300
00254      12  TELE-PARTS  REDEFINES NUMERIC-TELE-NUMBER.               EL300
00255          16  FILLER                  PIC X.                       EL300
00256          16  TELE-AREA               PIC X(3).                    EL300
00257          16  TELE-PREFIX             PIC X(3).                    EL300
00258          16  TELE-NUMBER             PIC X(4).                    EL300
00259      12  EDITED-TELE.                                             EL300
00260          16  TELE-AREA-E             PIC X(3).                    EL300
00261          16  FILLER                  PIC X           VALUE '-'.   EL300
00262          16  TELE-PREFIX-E           PIC X(3).                    EL300
00263          16  FILLER                  PIC X           VALUE '-'.   EL300
00264          16  TELE-NUMBER-E           PIC X(4).                    EL300
00265                                                                   EL300
00266  01  ZIP-CD-WORK.                                                 EL300
00267      12  W-NINE-DIG-ZIP              PIC 9(9).                    EL300
00268      12  W-FIVE-DIG-ZIP-A  REDEFINES W-NINE-DIG-ZIP.              EL300
00269          16  W-CHECK-ZERO-A.                                      EL300
00270              20  W-ZIP-DIG-1         PIC X.                       EL300
00271              20  FILLER              PIC XXX.                     EL300
00272          16  W-ZIP-CD-A              PIC X(5).                    EL300
00273      12  W-FIVE-DIG-ZIP-B  REDEFINES W-NINE-DIG-ZIP.              EL300
00274          16  W-ZIP-CD-B              PIC X(5).                    EL300
00275          16  W-CHECK-ZERO-B          PIC X(4).                    EL300
00276      12  W-CANADIAN-POSTAL-CODE  REDEFINES  W-NINE-DIG-ZIP.       EL300
00277          16  W-POSTAL-CODE-1         PIC XXX.                     EL300
00278          16  W-POSTAL-CODE-2         PIC XXX.                     EL300
00279          16  FILLER                  PIC XXX.                     EL300
00280                                                                   EL300
00281  01  ZIP-TELE-LINE.                                               EL300
00282      12  LINE5-TELE                  PIC X(12).                   EL300
00283      12  FILLER                      PIC X.                       EL300
00284      12  LINE5-ZIP-CODE.                                          EL300
00285          16  LINE5-ZIP-FIVE          PIC X(5).                    EL300
00286          16  LINE5-ZIP-DASH          PIC X.                       EL300
00287          16  LINE5-ZIP-FOUR          PIC X(4).                    EL300
00288      12  LINE5-POSTAL-CODE  REDEFINES  LINE5-ZIP-CODE.            EL300
00289          16  LINE5-POST-CODE1        PIC XXX.                     EL300
00290          16  FILLER                  PIC X.                       EL300
00291          16  LINE5-POST-CODE2        PIC XXX.                     EL300
00292          16  FILLER                  PIC XXX.                     EL300
00293      12  FILLER                      PIC X(7).                    EL300
00294                                                                   EL300
00295  01  UTIL-TBL.                                                    EL300
00296      12  TEST-OPT PIC X.                                          EL300
00297      12  TEST-SYS PIC X.                                          EL300
00298      12  ALPH-TBL PIC X(28) VALUE '123456789ABCDEFGHIJKLMNOPQRX'. EL300
00299      12  NUM-TBL  PIC X(28) VALUE '1234567891234567891234567899'. EL300
00300      12  ALPH-SET PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.   EL300
00301      12  NUM-SET  PIC X(26) VALUE '12345678901234567890123456'.   EL300
00302                                                                   EL300
00303  01  DATE-TEST.                                                   EL300
00304      12  DATE-MO                     PIC  99.                     EL300
00305      12  DATE-DA                     PIC  99.                     EL300
00306      12  DATE-CC                     PIC  99.                     EL300
00307      12  DATE-YY                     PIC  99.                     EL300
00308                                                                   EL300
00309  01  WORK-DATE.                                                   EL300
00310      12  WORK-MO                     PIC  99.                     EL300
00311      12  FILLER                      PIC  X.                      EL300
00312      12  WORK-DA                     PIC  99.                     EL300
00313      12  FILLER                      PIC  X.                      EL300
00314      12  WORK-YR                     PIC  99.                     EL300
00315                                                                   EL300
00316  01  MONTH-TABLE.                                                 EL300
00317      12  FILLER PIC X(13) VALUE '09 JANUARY 31'.                  EL300
00318      12  FILLER PIC X(13) VALUE '09FEBRUARY 28'.                  EL300
00319      12  FILLER PIC X(13) VALUE '08  MARCH  31'.                  EL300
00320      12  FILLER PIC X(13) VALUE '08  APRIL  30'.                  EL300
00321      12  FILLER PIC X(13) VALUE '07   MAY   31'.                  EL300
00322      12  FILLER PIC X(13) VALUE '07  JUNE   30'.                  EL300
00323      12  FILLER PIC X(13) VALUE '07  JULY   31'.                  EL300
00324      12  FILLER PIC X(13) VALUE '08 AUGUST  31'.                  EL300
00325      12  FILLER PIC X(13) VALUE '10SEPTEMBER30'.                  EL300
00326      12  FILLER PIC X(13) VALUE '09 OCTOBER 31'.                  EL300
00327      12  FILLER PIC X(13) VALUE '09NOVEMBER 30'.                  EL300
00328      12  FILLER PIC X(13) VALUE '09DECEMBER 31'.                  EL300
00329                                                                   EL300
00330  01  MONTH-TBL                       REDEFINES MONTH-TABLE.       EL300
00331      12  FILLER                      OCCURS 12 TIMES.             EL300
00332          16  MONTH-X                 PIC  99.                     EL300
00333          16  MONTH-N                 PIC  X(9).                   EL300
00334          16  MONTH-D                 PIC  99.                     EL300
00335                                                                   EL300
00336  01  EDIT-CNT.                                                    EL300
00337      12  EDIT-NO                     PIC  Z9.                     EL300
00338      12  EDIT-TP                     PIC  XX VALUE 'TH'.          EL300
00339      12  FILLER                      PIC  X(5) VALUE ' EDIT'.     EL300
00340                                                                   EL300
00341  01  WORK-1.                                                      EL300
00342      12  FILLER                      OCCURS 30 TIMES.             EL300
00343          16  WK-1                    PIC  X.                      EL300
00344                                                                   EL300
00345  01  WORK-2.                                                      EL300
00346      12  FILLER                      OCCURS 30 TIMES.             EL300
00347          16  WK-2                    PIC  X.                      EL300
00348                                                                   EL300
00349                                                                   EL300
00350  01  EDIT-DATE.                                                   EL300
00351      12  EDIT-DA                     PIC  99.                     EL300
00352      12  FILLER                      PIC  X(2) VALUE ', '.        EL300
00353      12  EDIT-CC                     PIC  99.                     EL300
00354      12  EDIT-YR                     PIC  99.                     EL300
00355                                                                   EL300
00356  01  CLAS-CARD.                                                   EL300
00357      12  CLAS-ID                     PIC  X(4).                   EL300
00358      12  CLAS-RUN-DATE.                                           EL300
00359          16  CLAS-RUN-MO             PIC  99  VALUE 0.            EL300
00360          16  CLAS-RUN-DA             PIC  99  VALUE 0.            EL300
00361          16  CLAS-RUN-YR             PIC  99  VALUE 0.            EL300
00362                                                                   EL300
00363      12  CLAS-ALPH-DATE              PIC  X(18).                  EL300
00364      12  CLAS-EP-DATE.                                            EL300
00365          16  CLAS-EP-MO              PIC  99  VALUE 0.            EL300
00366          16  CLAS-EP-DA              PIC  99  VALUE 0.            EL300
00367          16  CLAS-EP-YR              PIC  99  VALUE 0.            EL300
00368      12  CLAS-PEND-ACT-FILE          PIC  XX.                     EL300
00369      12  CLAS-EP-SW                  PIC  X.                      EL300
00370      12  CLAS-TAPE-BATCHES           PIC  X.                      EL300
00371      12  CLAS-BIN-RUN-DATE           PIC  X(02).                  EL300
00372      12  CLAS-RUN-CENTURY            PIC  X(02).                  EL300
00373      12  FILLER                      PIC  X(01).                  EL300
00374      12  CLAS-MORT-OR.                                            EL300
00375          16  CLAS-MORT-1             PIC  X.                      EL300
00376          16  CLAS-MORT-2             PIC  99.                     EL300
00377          16  CLAS-MORT-4             PIC  9.                      EL300
00378      12  CLAS-DOC                    PIC  X.                      EL300
00379      12  FILLER                      PIC  XX.                     EL300
00380      12  CLAS-COMPANY-NAME.                                       EL300
00381          16  CLAS-COMPANY-TEST       PIC  X(10).                  EL300
00382          16  FILLER                  PIC  X(20).                  EL300
00383                                                                   EL300
00384  01  CLPS-CARD                       REDEFINES CLAS-CARD.         EL300
00385      12  CLPS-ID                     PIC  X(4).                   EL300
00386      12  CLPS-SWITCH                 OCCURS 12 TIMES.             EL300
00387          16  FILLER                  PIC  X.                      EL300
00388          16  CLPS-PROG               PIC  9(3).                   EL300
00389          16  FILLER                  PIC  X.                      EL300
00390          16  CLPS-SW                 PIC  X.                      EL300
00391      12  FILLER                      PIC  X(4).                   EL300
00392                                                                   EL300
00393  01  CLAF-CARD                       REDEFINES CLAS-CARD.         EL300
00394      12  CLAF-ID                     PIC  X(4).                   EL300
00395      12  CLAF-FACTORS                OCCURS 6 TIMES.              EL300
00396          16  CLAF-FACTOR             PIC S9(3)V99999.             EL300
00397          16  CLAF-FACTOR-X REDEFINES CLAF-FACTOR                  EL300
00398                                      PIC  X(08).                  EL300
00399      12  FILLER                      PIC  X(28).                  EL300
00400                                                                   EL300
00401  01  COLC-CARD                       REDEFINES CLAS-CARD.         EL300
00402      12  COLC-ID                     PIC X(4).                    EL300
00403      12  COLC-COMPANY-ID             PIC X(3).                    EL300
00404      12  COLC-DT-OVRIDE              PIC X(1).                    EL300
00405      12  FILLER                      PIC X(72).                   EL300
00406                                                                   EL300
00407      EJECT                                                        EL300
00408  01  PROGRAM-NAME-TABLE.                                          EL300
00409      12  PROG-NM-TB                  OCCURS 1501 TIMES.           EL300
00410          16  PNT-PROG                PIC 999.                     EL300
00411          16  PNT-SYSTEM              PIC X.                       EL300
00412          16  PNT-MESSAGE             PIC X(40).                   EL300
00413                                                                   EL300
00414  01  COMPANY-TBL.                                                 EL300
00415      12  FILLER                      OCCURS 07 TIMES.             EL300
00416          16  COMPANY-LINE            PIC  X(30).                  EL300
00417                                                                   EL300
00418  01  PROGRAM-TBL.                                                 EL300
00419      12  FILLER                      OCCURS 1501 TIMES.           EL300
00420          16  PROG-TBL.                                            EL300
00421              20  PROG-TBL-PRT        PIC X.                       EL300
00422              20  PROG-TBL-FMT        PIC X.                       EL300
00423              20  PROG-TBL-PRC        PIC X.                       EL300
00424              20  PROG-TBL-TOT        PIC X.                       EL300
00425          16  PROG-SYS-NAME           PIC XX.                      EL300
00426                                                                   EL300
00427  01  PROG-SET.                                                    EL300
00428      12  SWITCH-SET                  OCCURS 20 TIMES.             EL300
00429          16  SW-SET-PRT              PIC X.                       EL300
00430          16  SW-SET-FMT              PIC X.                       EL300
00431          16  SW-SET-PRC              PIC X.                       EL300
00432          16  SW-SET-TOT              PIC X.                       EL300
00433                                                                   EL300
00434  01  PROG-SET-2.                                                  EL300
00435      12  SWITCH-SET-2                OCCURS 50 TIMES.             EL300
00436          16  SW-SET-PRC-2            PIC X.                       EL300
00437                                                                   EL300
00438  01  PROGRAM-OVERRIDE.                                            EL300
00439      12  FILLER                      OCCURS 1500 TIMES.           EL300
00440          16  PROG-OR.                                             EL300
00441              20  PROG-OR-PRT         PIC X.                       EL300
00442              20  PROG-OR-FMT         PIC X.                       EL300
00443              20  PROG-OR-PRC         PIC X.                       EL300
00444              20  PROG-OR-TOT         PIC X.                       EL300
00445                                                                   EL300
00446  01  SWITCH-TBL.                                                  EL300
00447      12  FILLER                      OCCURS 1502 TIMES.           EL300
00448          16  SWITCH-LINE.                                         EL300
00449              20  SW-CMD              PIC  X(4).                   EL300
00450              20  FILLER              PIC  X.                      EL300
00451              20  SW-PROG             PIC  9(3).                   EL300
00452              20  FILLER              PIC  X.                      EL300
00453              20  SW-OPT.                                          EL300
00454                  24  SW-PRNT         PIC  X.                      EL300
00455                  24  SW-FMT          PIC  X.                      EL300
00456                  24  SW-PROC         PIC  X.                      EL300
00457                  24  SW-TOT          PIC  X.                      EL300
00458                                                                   EL300
00459                                                                   EL300
00460  01  SWITCH-SAVE.                                                 EL300
00461      12  FILLER                      OCCURS 1502 TIMES.           EL300
00462          16  FILLER                  PIC  X(13).                  EL300
00463                                                                   EL300
00464  01  INSURANCE-TBL.                                               EL300
CIDMOD*    12  FILLER                      OCCURS 300 TIMES.            EL300
092602*    12  FILLER                      OCCURS 600 TIMES.            EL300
092602     12  FILLER                      OCCURS 900 TIMES.            EL300
00466          16  INSURANCE-LINE.                                      EL300
00467              20  INS-BEN             PIC  XX.                     EL300
00468              20  INS-AB3.                                         EL300
00469                  24  INS-AB1         PIC  X.                      EL300
00470                  24  INS-AB2         PIC  XX.                     EL300
00471              20  INS-AB10            PIC  X(10).                  EL300
00472              20  INS-COMMENT         PIC  X(10).                  EL300
00473              20  INS-JOINT           PIC  X.                      EL300
00474              20  INS-RLA             PIC  X.                      EL300
00475              20  INS-CALC-TYPE       PIC  X.                      EL300
00476              20  INS-EP              PIC  X.                      EL300
00477              20  INS-BEN-I-G-CD      PIC  X.                      EL300
00478              20  INS-REM-TERM-CALC   PIC  X.                      EL300
00479              20  FILLER              PIC  X(7).                   EL300
00480                                                                   EL300
00481                                                                   EL300
00482  01  CARRIER-NAME-TBL.                                            EL300
00483      12  FILLER                      OCCURS 26 TIMES.             EL300
00484          16  CN-NAME-LINE.                                        EL300
00485              20  CN-CODE             PIC  X.                      EL300
00486              20  CN-DOM-ST           PIC  XX.                     EL300
00487              20  CN-NAME             PIC  X(30).                  EL300
00488              20  CN-IBNR-UEP-PCT     PIC  S9(01)V9(04) COMP-3.    EL300
00489              20  CN-IBNR-R78-PCT     PIC  S9(01)V9(04) COMP-3.    EL300
00490              20  CN-IBNR-PRO-PCT     PIC  S9(01)V9(04) COMP-3.    EL300
00491                                                                   EL300
00492  01  MORTALITY-DESC.                                              EL300
00493      12  M-RESERVE-ADJ               PIC  9(03).                  EL300
00494      12  FILLER                      PIC  X(02) VALUE '%'.        EL300
00495      12  M-YEAR                      PIC  X(04).                  EL300
00496      12  FILLER                      PIC  X(01) VALUE ' '.        EL300
00497      12  M-TABLE-TYPE                PIC  X(03).                  EL300
00498      12  FILLER                      PIC  X(01) VALUE ' '.        EL300
00499      12  M-AGE-METHOD                PIC  X(02).                  EL300
00500      12  FILLER                      PIC  X(01) VALUE ' '.        EL300
00501      12  M-INTEREST                  PIC  9(02).9(02).            EL300
00502      12  FILLER                      PIC  X(02) VALUE '% '.       EL300
00503      12  M-NATURE-OF-TABLE           PIC  X(01).                  EL300
00504                                                                   EL300
00505  01  W-TABLE-DIVIDE.                                              EL300
00506      12  W-TABLE-YEAR.                                            EL300
00507          20  W-TABLE-YEAR-N          PIC  9(02).                  EL300
00508      12  W-TABLE-TYPE                PIC  X(03).                  EL300
00509                                                                   EL300
00510  01  W-FULL-YEAR.                                                 EL300
00511      12  W-TBL-DECADE                PIC  X(02).                  EL300
00512      12  W-TBL-YEAR                  PIC  X(02).                  EL300
00513                                                                   EL300
00514  01  WS-CLAS-RUN-DT.                                              EL300
00515      12  FILLER                  PIC   9(03)  VALUE 0.            EL300
00516      12  WS-CLAS-RUN-CCYY.                                        EL300
00517          20  WS-CLAS-RUN-CC      PIC   99     VALUE 0.            EL300
00518          20  WS-CLAS-RUN-YR      PIC   99     VALUE 0.            EL300
00519      12  WS-CLAS-RUN-MO          PIC   99     VALUE 0.            EL300
00520      12  WS-CLAS-RUN-DA          PIC   99     VALUE 0.            EL300
00521  01  WS-CLAS-RUN-DT-N REDEFINES                                   EL300
00522         WS-CLAS-RUN-DT           PIC   9(11).                     EL300
00523                                                                   EL300
00524  01  WS-CLAS-EP-DATE.                                             EL300
00525      12  FILLER                  PIC   9(03) VALUE  0.            EL300
00526      12  WS-CLAS-EP-CCYY.                                         EL300
00527          16  WS-CLAS-EP-CC       PIC   99 VALUE  0.               EL300
00528          16  WS-CLAS-EP-YR       PIC   99 VALUE  0.               EL300
00529          16  WS-CLAS-EP-MO       PIC   99 VALUE  0.               EL300
00530          16  WS-CLAS-EP-DA       PIC   99 VALUE  0.               EL300
00531  01  WS-CLAS-EP-DATE-N REDEFINES                                  EL300
00532         WS-CLAS-EP-DATE          PIC   9(11).                     EL300
00533                                                                   EL300
00534  01  WS-CTL-RUN-DATE.                                             EL300
00535      12  FILLER                  PIC  9(03)  VALUE 0.             EL300
00536      12  CTL-RUN-CC              PIC  99     VALUE 0.             EL300
00537      12  CTL-RUN-DT.                                              EL300
00538          16  CTL-RUN-YR          PIC  99     VALUE 0.             EL300
00539          16  CTL-RUN-MO          PIC  99     VALUE 0.             EL300
00540          16  CTL-RUN-DA          PIC  99     VALUE 0.             EL300
00541  01  WS-CTL-RUN-DATE-N REDEFINES                                  EL300
00542         WS-CTL-RUN-DATE          PIC 9(11).                       EL300
00543                                                                   EL300
00544  01  WS-CTL-EP-DATE.                                              EL300
00545      12  FILLER                  PIC  9(03)  VALUE 0.             EL300
00546      12  CTL-EP-CC               PIC  99     VALUE 0.             EL300
00547      12  CTL-EP-DT.                                               EL300
00548          16  CTL-EP-YR           PIC  99     VALUE 0.             EL300
00549          16  CTL-EP-MO           PIC  99     VALUE 0.             EL300
00550          16  CTL-EP-DA           PIC  99     VALUE 0.             EL300
00551  01  WS-CTL-EP-DATE-N REDEFINES                                   EL300
00552         WS-CTL-EP-DATE           PIC 9(11).                       EL300
00553                                                                   EL300
00554  01  WS-OT-CONV-DTE.                                              EL300
00555      12  FILLER                  PIC  9(03)  VALUE 0.             EL300
00556      12  OT-CONV-CCYR            PIC  9(04)  VALUE 0.             EL300
00557      12  OT-CONV-CCYY REDEFINES OT-CONV-CCYR.                     EL300
00558          16  OT-CONV-CC          PIC  99.                         EL300
00559          16  OT-CONV-YR          PIC  99.                         EL300
00560      12  OT-CONV-MO              PIC  99     VALUE 0.             EL300
00561      12  OT-CONV-DA              PIC  99     VALUE 0.             EL300
00562                                                                   EL300
00563  01  MORTALITY-TBL.                                               EL300
00564      12  FILLER                      OCCURS 99 TIMES.             EL300
00565          16  MORTALITY-LINE.                                      EL300
00566              20  M-CODE.                                          EL300
00567                  24  M-CODE-1        PIC  X.                      EL300
00568                  24  M-CODE-2        PIC  99.                     EL300
00569                  24  M-CODE-4        PIC  9.                      EL300
00570              20  M-J-CODE            PIC  X.                      EL300
00571              20  M-J-FACT            PIC S9V9999.                 EL300
00572              20  M-DESC              PIC  X(26).                  EL300
00573                                                                   EL300
00574  01  BUSINESS-TBL.                                                EL300
00575      12  FILLER                      OCCURS 51 TIMES.             EL300
00576          16  BUSINESS-LINE.                                       EL300
00577              20  B-CODE              PIC  99.                     EL300
00578              20  B-GROUP             PIC  X.                      EL300
00579              20  B-DESC              PIC  X(19).                  EL300
00580              20  B-EXCL-TYPE         PIC  X(01).                  EL300
00581              20  FILLER              PIC  X(04).                  EL300
00582                                                                   EL300
00583  01  STATE-TBL.                                                   EL300
00584      12  FILLER                      OCCURS 76 TIMES.             EL300
00585          16  STATE-LINE.                                          EL300
00586              20  S-CODE              PIC  XX.                     EL300
00587              20  S-NAME.                                          EL300
00588                  24  S-ABBR          PIC  XX.                     EL300
00589                  24  FILLER          PIC  X(3).                   EL300
00590                  24  S-DESC          PIC  X(20).                  EL300
00591                  24  S-UNER          PIC  X.                      EL300
00592                  24  S-REPT          PIC  X.                      EL300
00593                  24  S-RATE          PIC  XXX.                    EL300
00594                                                                   EL300
00595  01  STATE-TARGET-LOSS-RATIOS.                                    EL300
00596      12  STATE-TLR-FLD               OCCURS 75 TIMES.             EL300
00597          16  STATE-TARGET-LOSS-RATIO PIC S9(01)V9(04) COMP-3.     EL300
00598          16  STATE-CALC-INTEREST     PIC S9(01)V9(04) COMP-3.     EL300
00599                                                                   EL300
00600  01  BUSINESS-TRGT-LOSS-RATIO-MODS.                               EL300
00601      12  BUS-TLRM-FLD                OCCURS 99 TIMES.             EL300
00602          16  BUS-TRGT-LOSS-RATIO-MOD PIC S9(01)V9(04) COMP-3.     EL300
00603                                                                   EL300
00604  01  CARRIER-OPT-CLM-RSV-DATA.                                    EL300
00605      12  CARR-OPT-CLM-FLDS           OCCURS 25 TIMES.             EL300
00606          16  CARR-IBNR-SW            PIC  X(01).                  EL300
00607          16  CARR-IBNR-PERCENT       PIC S9(01)V9(04) COMP-3.     EL300
00608          16  CARR-PERCENT-OF-CIDA    PIC S9(01)V9(04) COMP-3.     EL300
00609                                                                   EL300
00610  01  MISC-OPTIONAL-CLAIM-DATA.                                    EL300
00611      12  COMPANY-CALC-INTEREST       PIC S9(01)V9(04) COMP-3.     EL300
00612      12  COMPANY-CIDA-DISCOUNT       PIC S9(01)V9(04) COMP-3.     EL300
00613      12  COMPANY-CRDB-TABLE-SELECTION                             EL300
00614                                      PIC  X(01).                  EL300
00615      12  COMPANY-IBNR-AH-FACTOR      PIC S9(01)V9(04) COMP-3.     EL300
00616      12  COMPANY-IBNR-LAG-MONTHS     PIC S9(03)       COMP-3.     EL300
00617      12  COMPANY-IBNR-LIFE-FACTOR    PIC S9(01)V9(04) COMP-3.     EL300
00618      12  COMPANY-OPTION-START-DATE   PIC  X(02).                  EL300
00619      12  INDEXCA                     PIC S9(04)       COMP.       EL300
00620      12  INDEXST                     PIC S9(04)       COMP.       EL300
00621      12  INDEXBS                     PIC S9(04)       COMP.       EL300
00622                                                                   EL300
00623  01  CONTROL-TBL.                                                 EL300
00624      12  CTL-ID                      PIC  X(4).                   EL300
00625      12  CTL-RUN-DATE                PIC  9(11)  COMP-3.          EL300
00626      12  CTL-ALPH-DATE               PIC  X(18).                  EL300
00627      12  CTL-EP-DATE                 PIC  9(11)  COMP-3.          EL300
00628      12  CTL-PEND-ACT-FILE           PIC  XX.                     EL300
00629      12  CTL-EP-SW                   PIC  X.                      EL300
00630      12  CTL-TAPE-BATCHES            PIC  X.                      EL300
00631      12  CTL-BIN-RUN-DATE            PIC  X(02).                  EL300
00632      12  CTL-RUN-CENTURY             PIC  X(02).                  EL300
00633      12  FILLER                      PIC  X.                      EL300
00634      12  CTL-MORT-OR.                                             EL300
00635          16  CTL-MORT-1              PIC  X.                      EL300
00636          16  CTL-MORT-2              PIC  99.                     EL300
00637          16  CTL-MORT-4              PIC  9.                      EL300
00638      12  CTL-DOC                     PIC  X.                      EL300
00639      12  FILLER                      PIC  XX.                     EL300
00640      12  CTL-COMPANY-NAME            PIC  X(30).                  EL300
00641                                                                   EL300
00642  01  FACTOR-TBL.                                                  EL300
00643      12  FACT-ID                     PIC  X(4).                   EL300
00644      12  FACT-FACTORS.                                            EL300
00645          16  FILLER                  OCCURS 6 TIMES.              EL300
00646              20  FACT-FACTOR         PIC S9(3)V99999.             EL300
00647                                                                   EL300
00648                                                                   EL300
00649                                                                   EL300
00650  01  INDEX-TBL.                                                   EL300
00651      12  CLAX-ID                     PIC  X(4) VALUE 'CLAX'.      EL300
00652      12  CLAS-STARTC                 PIC S9(4) COMP VALUE ZERO.   EL300
00653      12  CLAS-MAXC                   PIC S9(4) COMP VALUE ZERO.   EL300
00654      12  CLAS-STARTL                 PIC S9(4) COMP VALUE ZERO.   EL300
00655      12  CLAS-MAXL                   PIC S9(4) COMP VALUE ZERO.   EL300
00656      12  CLAS-STARTA                 PIC S9(4) COMP VALUE ZERO.   EL300
00657      12  CLAS-MAXA                   PIC S9(4) COMP VALUE ZERO.   EL300
00658      12  CLAS-STARTM                 PIC S9(4) COMP VALUE ZERO.   EL300
00659      12  CLAS-MAXM                   PIC S9(4) COMP VALUE ZERO.   EL300
00660      12  CLAS-STARTB                 PIC S9(4) COMP VALUE ZERO.   EL300
00661      12  CLAS-MAXB                   PIC S9(4) COMP VALUE ZERO.   EL300
00662      12  CLAS-STARTS                 PIC S9(4) COMP VALUE ZERO.   EL300
00663      12  CLAS-MAXS                   PIC S9(4) COMP VALUE ZERO.   EL300
00664      12  CLAS-STARTE                 PIC S9(4) COMP VALUE ZERO.   EL300
00665      12  CLAS-MAXE                   PIC S9(4) COMP VALUE ZERO.   EL300
00666      12  CLAS-STARTCN                PIC S9(4) COMP VALUE ZERO.   EL300
00667      12  CLAS-MAXCN                  PIC S9(4) COMP VALUE ZERO.   EL300
00668                                                                   EL300
00669                                                                   EL300
00670  01  CLASIC-TBL.                                                  EL300
00671      12  CLASIC-ID                   PIC X(4)       VALUE 'COLC'. EL300
00672      12  CLASIC-COMPANY-CD           PIC X.                       EL300
00673      12  CLASIC-COMPANY-NUMBER       PIC 999.                     EL300
00674 *    12  CLASIC-CLAIM-ACCESS         PIC X.                       EL300
00675      12  FILLER                      PIC X.                       EL300
00676      12  CLASIC-REIN-MAINT           PIC XX.                      EL300
00677      12  CLASIC-COMP-MAINT           PIC XX.                      EL300
00678      12  CLASIC-ACCT-MAINT           PIC XX.                      EL300
00679      12  CLASIC-CTBL-MAINT           PIC XX.                      EL300
00680      12  CLASIC-RATE-MAINT           PIC XX.                      EL300
00681      12  CLASIC-CREDIT-EOM-DT        PIC XX.                      EL300
00682      12  CLASIC-CLAIMS-EOM-DT        PIC XX.                      EL300
00683      12  CLASIC-LIFE-OVERRIDE-L1     PIC X.                       EL300
00684      12  CLASIC-LIFE-OVERRIDE-L2     PIC XX.                      EL300
00685      12  CLASIC-LIFE-OVERRIDE-L6     PIC X(6).                    EL300
00686      12  CLASIC-LIFE-OVERRIDE-L12    PIC X(12).                   EL300
00687      12  CLASIC-AH-OVERRIDE-L1       PIC X.                       EL300
00688      12  CLASIC-AH-OVERRIDE-L2       PIC XX.                      EL300
00689      12  CLASIC-AH-OVERRIDE-L6       PIC X(6).                    EL300
00690      12  CLASIC-AH-OVERRIDE-L12      PIC X(12).                   EL300
00691      12  CLASIC-REPORT-CD1-CAPTION   PIC X(10).                   EL300
00692      12  CLASIC-REPORT-CD2-CAPTION   PIC X(10).                   EL300
00693      12  CLASIC-MORTG-EOM-DT         PIC XX.                      EL300
00694      12  CLASIC-AR-EOM-DT            PIC XX.                      EL300
00695      12  FILLER                      PIC X(11)      VALUE SPACE.  EL300
00696      EJECT                                                        EL300
00697                                                                   EL300
00698  01  VSAM-ERROR-MESSAGE.                                          EL300
00699      12  FILLER                      PIC X(22)                    EL300
00700                             VALUE '****VSAM ERROR CODE - '.       EL300
00701      12  VSAM-CD                     PIC XX.                      EL300
00702      12  FILLER                      PIC X(29)                    EL300
00703                             VALUE ' FILE BEING ACCESSED WAS - '.  EL300
00704      12  VSAM-PGM                    PIC X(6).                    EL300
00705      12  FILLER                      PIC X(79)                    EL300
00706                             VALUE ' **********************'.      EL300
00707                                                                   EL300
00708                                                                   EL300
00709  01  T01.                                                         EL300
00710      12  FILLER PIC X(48) VALUE SPACES.                           EL300
00711      12  T01-T  PIC X(28).                                        EL300
00712      12  FILLER PIC X(48) VALUE SPACES.                           EL300
00713      12  T01-I  PIC X(3) VALUE ' EL'.                             EL300
00714      12  FILLER PIC X(5) VALUE '-DATE'.                           EL300
00715                                                                   EL300
00716  01  T02.                                                         EL300
00717      12  FILLER PIC X(47) VALUE SPACES.                           EL300
00718      12  T02-N  PIC X(30) VALUE '     COMPANY NAME UNKNOWN     '. EL300
00719      12  FILLER PIC X(47) VALUE SPACES.                           EL300
00720      12  T02-D  PIC X(8).                                         EL300
00721                                                                   EL300
00722  01  T03.                                                         EL300
00723      12  FILLER PIC X(53) VALUE SPACES.                           EL300
00724      12  T03-D  PIC X(18) VALUE '   DATE UNKNOWN   '.             EL300
00725      12  FILLER PIC X(41) VALUE SPACES.                           EL300
00726      12  FILLER PIC X(5) VALUE 'PAGE '.                           EL300
00727      12  T03-P  PIC ZZ,ZZ9.                                       EL300
00728                                                                   EL300
00729  01  T11.                                                         EL300
00730      12  FILLER PIC X(17) VALUE SPACES.                           EL300
00731      12  T11-T  PIC X(28).                                        EL300
00732      12  FILLER PIC X(17) VALUE SPACES.                           EL300
00733      12  T11-I  PIC X(3) VALUE ' EL'.                             EL300
00734      12  FILLER PIC X(5) VALUE '-DATE'.                           EL300
00735                                                                   EL300
00736  01  T12.                                                         EL300
00737      12  FILLER PIC X(16) VALUE SPACES.                           EL300
00738      12  T12-N  PIC X(30) VALUE '     COMPANY NAME UNKNOWN     '. EL300
00739      12  FILLER PIC X(16) VALUE SPACES.                           EL300
00740      12  T12-D  PIC X(8).                                         EL300
00741                                                                   EL300
00742  01  T13.                                                         EL300
00743      12  FILLER PIC X(22) VALUE SPACES.                           EL300
00744      12  T13-D  PIC X(18) VALUE '   DATE UNKNOWN   '.             EL300
00745      12  FILLER PIC X(19) VALUE SPACES.                           EL300
00746      12  FILLER PIC X(5) VALUE 'PAGE '.                           EL300
00747      12  T13-P  PIC ZZ,ZZ9.                                       EL300
00748                                                                   EL300
00749  01  S01-H01.                                                     EL300
00750      12  FILLER PIC X(25) VALUE 'COMPANY NAME AND ADDRESS '.      EL300
00751      12  S01-C  PIC X(11) VALUE SPACE.                            EL300
00752                                                                   EL300
00753  01  S03-H01.                                                     EL300
00754      12  FILLER PIC X(31) VALUE 'STANDARD PROGRAM OPTIONS SELECT'.EL300
00755      12  FILLER PIC X(3) VALUE 'ED '.                             EL300
00756      12  S03-C  PIC X(11) VALUE SPACE.                            EL300
00757                                                                   EL300
00758  01  S03-H02.                                                     EL300
00759      12  FILLER PIC X(28) VALUE '     PROGRAM OPT DESCRIPTION'.   EL300
00760                                                                   EL300
00761  01  S04-H01.                                                     EL300
00762      12  FILLER PIC X(31) VALUE 'STANDARD MICROFICHE OPTIONS SEL'.EL300
00763      12  FILLER PIC X(6) VALUE 'ECTED '.                          EL300
00764      12  S04-C  PIC X(11) VALUE SPACE.                            EL300
00765                                                                   EL300
00766  01  S04-H02.                                                     EL300
00767      12  FILLER PIC X(25) VALUE '     PROGRAM  DESCRIPTION'.      EL300
00768                                                                   EL300
00769                                                                   EL300
00770  01  S09-H01.                                                     EL300
00771      12  FILLER PIC X(26) VALUE 'PROGRAM OPTIONS AVAILABLE '.     EL300
00772      12  S09-C  PIC X(11) VALUE SPACE.                            EL300
00773                                                                   EL300
00774  01  S09-H02.                                                     EL300
00775      12  FILLER PIC X(28) VALUE '     PROGRAM OPT DESCRIPTION'.   EL300
00776                                                                   EL300
00777  01  S11-H01.                                                     EL300
00778      12  FILLER PIC X(20) VALUE 'PROGRAM ERROR TABLE '.           EL300
00779      12  S11-C  PIC X(11) VALUE SPACE.                            EL300
00780                                                                   EL300
00781  01  S11-H02.                                                     EL300
00782      12  FILLER PIC X(24) VALUE '     ERROR   DESCRIPTION'.       EL300
00783                                                                   EL300
00784  01  S13-H01.                                                     EL300
00785      12  FILLER PIC X(25) VALUE 'SELECTED PROGRAM OPTIONS '.      EL300
00786      12  S13-C  PIC X(11) VALUE SPACE.                            EL300
00787                                                                   EL300
00788  01  S13-H02.                                                     EL300
00789      12  FILLER PIC X(23) VALUE '     PROGRAM OPT   TYPE'.        EL300
00790      12  FILLER PIC X(19) VALUE '        DESCRIPTION'.            EL300
00791                                                                   EL300
00792  01  S14-H01.                                                     EL300
00793      12  FILLER PIC X(28) VALUE 'SELECTED PRINT OVERRIDES    '.   EL300
00794      12  S14-C  PIC X(11) VALUE SPACE.                            EL300
00795                                                                   EL300
00796  01  S14-H02.                                                     EL300
00797      12  FILLER PIC X(25) VALUE '     PROGRAM  DESCRIPTION'.      EL300
00798                                                                   EL300
00799  01  S15-H01.                                                     EL300
00800      12  FILLER PIC X(21) VALUE 'COMPANY FACTOR TABLE '.          EL300
00801      12  S15-C  PIC X(11) VALUE SPACE.                            EL300
00802                                                                   EL300
00803  01  S15-H02.                                                     EL300
00804      12  FILLER PIC X(22) VALUE '     CODE       FACTOR'.         EL300
00805                                                                   EL300
00806  01  S16-H01.                                                     EL300
00807      12  FILLER PIC X(27) VALUE '     CODE     ERROR MESSAGE'.    EL300
00808                                                                   EL300
00809                                                                   EL300
00810  01  D-LINE.                                                      EL300
00811      12  POS-01.                                                  EL300
00812          16  FILLER                  PIC  X(5).                   EL300
00813          16  POS-06.                                              EL300
00814              20  FILLER              PIC  X(5).                   EL300
00815              20  POS-11.                                          EL300
00816                  24  FILLER          PIC  X(34).                  EL300
00817                  24  POS-45          PIC  X(26).                  EL300
00818      12  FILLER                      PIC  X(62).                  EL300
00819                                                                   EL300
00820  01  S01-L01                         REDEFINES D-LINE.            EL300
00821      12  FILLER                      PIC  X(5).                   EL300
00822      12  S01-NAME                    PIC  X(30).                  EL300
00823                                                                   EL300
00824  01  S13-L01                         REDEFINES D-LINE.            EL300
00825      12  FILLER                      PIC  X(5).                   EL300
00826      12  S13-CLAS                    PIC  X(4).                   EL300
00827      12  S13-PROGRAM                 PIC  9(3).                   EL300
00828      12  FILLER                      PIC  X(5).                   EL300
00829      12  S13-NAME                    PIC  X(40).                  EL300
00830                                                                   EL300
00831  01  S13-L02                         REDEFINES D-LINE.            EL300
00832      12  FILLER                      PIC  X(14).                  EL300
00833      12  S13-OPTION                  PIC  X.                      EL300
00834      12  FILLER                      PIC  X(4).                   EL300
00835      12  S13-OPT-TYPE                PIC  X(7).                   EL300
00836      12  FILLER                      PIC  X(5).                   EL300
00837      12  S13-DESC                    PIC  X(40).                  EL300
00838      12  FILLER                      PIC  XX.                     EL300
00839      12  S13-MESSAGE                 PIC  X(8).                   EL300
00840                                                                   EL300
00841  01  S14-L01                         REDEFINES D-LINE.            EL300
00842      12  FILLER                      PIC  X(5).                   EL300
00843      12  S14-CLAS                    PIC  X(4).                   EL300
00844      12  S14-PROGRAM                 PIC  9(3).                   EL300
00845      12  FILLER                      PIC  XX.                     EL300
00846      12  S14-NAME                    PIC  X(40).                  EL300
00847      12  FILLER                      PIC  X.                      EL300
00848      12  S14-MESSAGE                 PIC  X(15).                  EL300
00849                                                                   EL300
00850  01  S15-L01                         REDEFINES D-LINE.            EL300
00851      12  FILLER                      PIC  X(7).                   EL300
00852      12  S15-CODE                    PIC  9.                      EL300
00853      12  FILLER                      PIC  X(5).                   EL300
00854      12  S15-FACTOR                  PIC  ZZ9.99999.              EL300
00855                                                                   EL300
00856  01  S16-L01                         REDEFINES D-LINE.            EL300
00857      12  FILLER                      PIC  X(5).                   EL300
00858      12  S16-CODE                    PIC  ZZZ9.                   EL300
00859      12  FILLER                      PIC  X(5).                   EL300
00860      12  S16-MESSAGE                 PIC  X(40).                  EL300
00861      12  S16-CONT                    PIC  X(7).                   EL300
00862                                                                   EL300
00863  01  S16-L02                         REDEFINES D-LINE.            EL300
00864      12  FILLER                      PIC  X(16).                  EL300
00865      12  S16-ERR-DETAIL              PIC  X(44).                  EL300
00866                                                                   EL300
00867  01  S17-L01                         REDEFINES D-LINE.            EL300
00868      12  FILLER                      PIC  X(8).                   EL300
00869      12  S17-IN                      PIC  XX.                     EL300
00870      12  FILLER                      PIC  X(8).                   EL300
00871      12  S17-OUT                     PIC  XX.                     EL300
00872                                                                   EL300
00873                                                                   EL300
00874                                                                   EL300
00875  01  CMD-MESSAGES.                                                EL300
00876      12  FILLER PIC X(22) VALUE 'NONEOPTION IS ALWAYS S'.         EL300
00877      12  FILLER PIC X(22) VALUE 'ET                    '.         EL300
00878                                                                   EL300
00879      12  FILLER PIC X(22) VALUE 'ED=YEDIT-NUMBER IS EQU'.         EL300
00880      12  FILLER PIC X(22) VALUE 'AL TO A VALUE         '.         EL300
00881                                                                   EL300
00882      12  FILLER PIC X(22) VALUE 'ED=NEDIT-NUMBER IS EQU'.         EL300
00883      12  FILLER PIC X(22) VALUE 'AL TO SPACE           '.         EL300
00884                                                                   EL300
00885      12  FILLER PIC X(22) VALUE 'UD=YUPDATE-SWITCH IS E'.         EL300
00886      12  FILLER PIC X(22) VALUE 'QUAL TO 1             '.         EL300
00887                                                                   EL300
00888      12  FILLER PIC X(22) VALUE 'UD=NUPDATE-SWITCH IS E'.         EL300
00889      12  FILLER PIC X(22) VALUE 'QUAL TO SPACE         '.         EL300
00890                                                                   EL300
00891      12  FILLER PIC X(22) VALUE 'MO=YRUN-MONTH IS EQUAL'.         EL300
00892      12  FILLER PIC X(22) VALUE ' TO 12                '.         EL300
00893                                                                   EL300
00894      12  FILLER PIC X(22) VALUE 'MO/YRUN-MONTH IS NOT E'.         EL300
00895      12  FILLER PIC X(22) VALUE 'QUAL TO 12            '.         EL300
00896                                                                   EL300
00897      12  FILLER PIC X(22) VALUE 'MO=QRUN-MONTH IS EQUAL'.         EL300
00898      12  FILLER PIC X(22) VALUE ' TO 3, 6, 9, OR 12    '.         EL300
00899                                                                   EL300
00900      12  FILLER PIC X(22) VALUE 'MO/QRUN-MONTH IS NOT E'.         EL300
00901      12  FILLER PIC X(22) VALUE 'QUAL TO 3, 6, 9, OR 12'.         EL300
00902                                                                   EL300
00903      12  FILLER PIC X(22) VALUE 'MO=ERUN-MONTH EQUAL TO'.         EL300
00904      12  FILLER PIC X(22) VALUE ' 2, 4, 6, 8, 10 OR 12 '.         EL300
00905                                                                   EL300
00906      12  FILLER PIC X(22) VALUE 'MO=ORUN-MONTH EQUAL TO'.         EL300
00907      12  FILLER PIC X(22) VALUE ' 1, 3, 5, 7, 9 OR 11  '.         EL300
00908                                                                   EL300
00909      12  FILLER PIC X(22) VALUE 'MO=1RUN-MONTH IS EQUAL'.         EL300
00910      12  FILLER PIC X(22) VALUE ' TO JANUARY           '.         EL300
00911                                                                   EL300
00912      12  FILLER PIC X(22) VALUE 'MO=2RUN-MONTH IS EQUAL'.         EL300
00913      12  FILLER PIC X(22) VALUE ' TO FEBRUARY          '.         EL300
00914                                                                   EL300
00915      12  FILLER PIC X(22) VALUE 'MO=3RUN-MONTH IS EQUAL'.         EL300
00916      12  FILLER PIC X(22) VALUE ' TO MARCH             '.         EL300
00917                                                                   EL300
00918      12  FILLER PIC X(22) VALUE 'MO=4RUN-MONTH IS EQUAL'.         EL300
00919      12  FILLER PIC X(22) VALUE ' TO APRIL             '.         EL300
00920                                                                   EL300
00921      12  FILLER PIC X(22) VALUE 'MO=5RUN-MONTH IS EQUAL'.         EL300
00922      12  FILLER PIC X(22) VALUE ' TO MAY               '.         EL300
00923                                                                   EL300
00924      12  FILLER PIC X(22) VALUE 'MO=6RUN-MONTH IS EQUAL'.         EL300
00925      12  FILLER PIC X(22) VALUE ' TO JUNE              '.         EL300
00926                                                                   EL300
00927      12  FILLER PIC X(22) VALUE 'MO=7RUN-MONTH IS EQUAL'.         EL300
00928      12  FILLER PIC X(22) VALUE ' TO JULY              '.         EL300
00929                                                                   EL300
00930      12  FILLER PIC X(22) VALUE 'MO=8RUN-MONTH IS EQUAL'.         EL300
00931      12  FILLER PIC X(22) VALUE ' TO AUGUST            '.         EL300
00932                                                                   EL300
00933      12  FILLER PIC X(22) VALUE 'MO=9RUN-MONTH IS EQUAL'.         EL300
00934      12  FILLER PIC X(22) VALUE ' TO SEPTEMBER         '.         EL300
00935                                                                   EL300
00936      12  FILLER PIC X(22) VALUE 'MO=ARUN-MONTH IS EQUAL'.         EL300
00937      12  FILLER PIC X(22) VALUE ' TO OCTOBER           '.         EL300
00938                                                                   EL300
00939      12  FILLER PIC X(22) VALUE 'MO=BRUN-MONTH IS EQUAL'.         EL300
00940      12  FILLER PIC X(22) VALUE ' TO NOVEMBER          '.         EL300
00941                                                                   EL300
00942      12  FILLER PIC X(22) VALUE 'MO=CRUN-MONTH IS EQUAL'.         EL300
00943      12  FILLER PIC X(22) VALUE ' TO DECEMBER          '.         EL300
00944                                                                   EL300
00945      12  FILLER PIC X(22) VALUE 'UP=YUPDATE-SWITCH IS E'.         EL300
00946      12  FILLER PIC X(22) VALUE 'QUAL TO 1             '.         EL300
00947                                                                   EL300
00948      12  FILLER PIC X(22) VALUE 'UP=NUPDATE-SWITCH IS E'.         EL300
00949      12  FILLER PIC X(22) VALUE 'QUAL TO SPACE         '.         EL300
00950                                                                   EL300
00951      12  FILLER PIC X(44) VALUE HIGH-VALUES.                      EL300
00952                                                                   EL300
00953  01  CMD-MESSAGES-R                  REDEFINES CMD-MESSAGES.      EL300
00954      12  FILLER                      OCCURS 26 TIMES.             EL300
00955          16  CMD-TEST                PIC  X(4).                   EL300
00956          16  CMD-MSG                 PIC  X(40).                  EL300
00957      EJECT                                                        EL300
00958  01  WS-DATE-AND-TIME.                                            EL300
00959      12  WS-ACCEPT-DATE.                                          EL300
00960          16  WS-AD-YY                PIC  99.                     EL300
00961          16  WS-AD-MM                PIC  99.                     EL300
00962          16  WS-AD-DD                PIC  99.                     EL300
00963      12  WS-CURRENT-DATE.                                         EL300
00964          16  WS-CD-MM                PIC  99.                     EL300
00965          16  FILLER                  PIC  X          VALUE '/'.   EL300
00966          16  WS-CD-DD                PIC  99.                     EL300
00967          16  FILLER                  PIC  X          VALUE '/'.   EL300
00968          16  WS-CD-YY                PIC  99.                     EL300
00969      12  WS-TIME-OF-DAY.                                          EL300
00970          16  WS-TIME                 PIC  9(6).                   EL300
00971          16  WS-HUN-SEC              PIC  99.                     EL300
00972      EJECT                                                        EL300
00973  01  ERR-MESSAGES.                                                EL300
00974                                                                   EL300
00975      12  FILLER PIC X(22) VALUE '0001*COLC* CLIENT ID I'.         EL300
00976      12  FILLER PIC X(22) VALUE 'S SPACES              '.         EL300
00977                                                                   EL300
00978      12  FILLER PIC X(22) VALUE '0002*COLC*CARD MUST BE'.         EL300
00979      12  FILLER PIC X(22) VALUE ' FIRST INPUT          '.         EL300
00980                                                                   EL300
00981      12  FILLER PIC X(22) VALUE '0003*COLC*DUPLICATE CA'.         EL300
00982      12  FILLER PIC X(22) VALUE 'RDS WERE INPUT        '.         EL300
00983                                                                   EL300
00984      12  FILLER PIC X(22) VALUE '0004*COLC* DATE OVER-R'.         EL300
00985      12  FILLER PIC X(22) VALUE 'IDE SWITCH INVALID    '.         EL300
00986                                                                   EL300
00987      12  FILLER PIC X(22) VALUE '0005VSAM ACCESS ERROR-'.         EL300
00988      12  FILLER PIC X(22) VALUE 'LOOK FOR ERROR LINE   '.         EL300
00989                                                                   EL300
00990      12  FILLER PIC X(22) VALUE '0006NO MATCHING COMPAN'.         EL300
00991      12  FILLER PIC X(22) VALUE 'Y CONTROL RECORD      '.         EL300
00992                                                                   EL300
00993      12  FILLER PIC X(22) VALUE '0007*COLC* MISSING    '.         EL300
00994      12  FILLER PIC X(22) VALUE '                      '.         EL300
00995                                                                   EL300
00996      12  FILLER PIC X(22) VALUE '0008NO DATE IN COLC OR'.         EL300
00997      12  FILLER PIC X(22) VALUE 'CLAS CARDS            '.         EL300
00998                                                                   EL300
00999      12  FILLER PIC X(22) VALUE '0009TOO MANY PROGRAM O'.         EL300
01000      12  FILLER PIC X(22) VALUE 'PTIONS FOR HOLD TABLE '.         EL300
01001                                                                   EL300
01002      12  FILLER PIC X(22) VALUE '0010CLAS AND CLASIC DA'.         EL300
01003      12  FILLER PIC X(22) VALUE 'TES NOT = NO OVER-RIDE'.         EL300
01004                                                                   EL300
01005      12  FILLER PIC X(22) VALUE '0011INVALID DATE IN CL'.         EL300
01006      12  FILLER PIC X(22) VALUE 'ASIC COMPPANY RECORD  '.         EL300
01007                                                                   EL300
CIDMOD*    12  FILLER PIC X(22) VALUE '0012OVER 300 BENEFIT T'.         EL300
CIDMOD     12  FILLER PIC X(22) VALUE '0012OVER 600 BENEFIT T'.         EL300
01009      12  FILLER PIC X(22) VALUE 'YPES IN CLASIC SYSTEM '.         EL300
01010                                                                   EL300
01011      12  FILLER PIC X(22) VALUE '0013OVER 75 STATES IN '.         EL300
01012      12  FILLER PIC X(22) VALUE 'CLASIC SYSTEM         '.         EL300
01013                                                                   EL300
01014      12  FILLER PIC X(22) VALUE '0014OVER 25 CARRIERS I'.         EL300
01015      12  FILLER PIC X(22) VALUE 'N CLASIC SYSTEM       '.         EL300
01016                                                                   EL300
01017      12  FILLER PIC X(22) VALUE '0015FREQUENCY OPTION I'.         EL300
01018      12  FILLER PIC X(22) VALUE 'S NOT AVAILABLE       '.         EL300
01019                                                                   EL300
01020      12  FILLER PIC X(22) VALUE '0016TOO MANY PROGRAM N'.         EL300
01021      12  FILLER PIC X(22) VALUE 'AMES FOR HOLD TABLE   '.         EL300
01022                                                                   EL300
01023 ********************************  DELETED TABLE ENTRIES  *****    EL300
01024      12  FILLER PIC X(836) VALUE ZERO.                            EL300
01025 **************************************************************    EL300
01026                                                                   EL300
01027      12  FILLER PIC X(22) VALUE '0036SWITCH TABLE EXCEE'.         EL300
01028      12  FILLER PIC X(22) VALUE 'DS ALLOWABLE SIZE     '.         EL300
01029                                                                   EL300
01030 *********************************  DELETED TABLE ENTRIES  ****    EL300
01031      12  FILLER PIC X(308) VALUE ZERO.                            EL300
01032 **************************************************************    EL300
01033                                                                   EL300
01034      12  FILLER PIC X(22) VALUE '0044INSURANCE TABLE EX'.         EL300
01035      12  FILLER PIC X(22) VALUE 'CEEDS ALLOWABLE SIZE  '.         EL300
01036                                                                   EL300
01037 *********************************  DELETED TABLE ENTRIES  ****    EL300
01038      12  FILLER PIC X(176) VALUE ZERO.                            EL300
01039 **************************************************************    EL300
01040                                                                   EL300
01041      12  FILLER PIC X(22) VALUE '0049INSURANCE TABLE EP'.         EL300
01042      12  FILLER PIC X(22) VALUE ' CODE IS INVALID      '.         EL300
01043                                                                   EL300
01044 *********************************  DELETED TABLE ENTRIES  ****    EL300
01045      12  FILLER PIC X(352) VALUE ZERO.                            EL300
01046 **************************************************************    EL300
01047                                                                   EL300
01048      12  FILLER PIC X(22) VALUE '0058MORTALITY TABLE EX'.         EL300
01049      12  FILLER PIC X(22) VALUE 'CEEDS ALLOWABLE SIZE  '.         EL300
01050                                                                   EL300
01051 *********************************  DELETED TABLE ENTRIES  ****    EL300
01052      12  FILLER PIC X(572) VALUE ZERO.                            EL300
01053 **************************************************************    EL300
01054                                                                   EL300
01055      12  FILLER PIC X(22) VALUE '0072STATE TABLE EXCEED'.         EL300
01056      12  FILLER PIC X(22) VALUE 'S 75 ENTRIES          '.         EL300
01057                                                                   EL300
01058 *********************************  DELETED TABLE ENTRIES  ****    EL300
01059      12  FILLER PIC X(44) VALUE ZERO.                             EL300
01060 **************************************************************    EL300
01061                                                                   EL300
01062      12  FILLER PIC X(22) VALUE '0074INVALID DATE/CONTR'.         EL300
01063      12  FILLER PIC X(22) VALUE 'OL CARD               '.         EL300
01064                                                                   EL300
01065      12  FILLER PIC X(22) VALUE '0075*CLAF* ONLY ONE FA'.         EL300
01066      12  FILLER PIC X(22) VALUE 'CTOR CARD ALLOWED     '.         EL300
01067                                                                   EL300
01068      12  FILLER PIC X(22) VALUE '0076*CLAF* FACTOR IS N'.         EL300
01069      12  FILLER PIC X(22) VALUE 'OT NUMERIC            '.         EL300
01070                                                                   EL300
01071      12  FILLER PIC X(22) VALUE '0077*CLAS* DATE CARD M'.         EL300
01072      12  FILLER PIC X(22) VALUE 'ISSING                '.         EL300
01073                                                                   EL300
01074      12  FILLER PIC X(22) VALUE '0078*CLAS* DATE CARD N'.         EL300
01075      12  FILLER PIC X(22) VALUE 'OT FIRST CARD         '.         EL300
01076                                                                   EL300
01077      12  FILLER PIC X(22) VALUE '0079*CLAS* ONLY ONE DA'.         EL300
01078      12  FILLER PIC X(22) VALUE 'TE CARD ALLOWED       '.         EL300
01079                                                                   EL300
01080      12  FILLER PIC X(22) VALUE '0080*CLAS* RUN DATE IN'.         EL300
01081      12  FILLER PIC X(22) VALUE 'VALID                 '.         EL300
01082                                                                   EL300
01083      12  FILLER PIC X(22) VALUE '0081*CLAS* EARNED PREM'.         EL300
01084      12  FILLER PIC X(22) VALUE 'IUM DATE INVALID      '.         EL300
01085                                                                   EL300
01086      12  FILLER PIC X(22) VALUE '0082*CLAS* EDIT NUMBER'.         EL300
01087      12  FILLER PIC X(22) VALUE ' IS INVALID           '.         EL300
01088                                                                   EL300
01089      12  FILLER PIC X(22) VALUE '0083*CLAS* UPDATE SWIT'.         EL300
01090      12  FILLER PIC X(22) VALUE 'CH IS INVALID         '.         EL300
01091                                                                   EL300
01092      12  FILLER PIC X(22) VALUE '0084*CLAS* TAPE INPUT '.         EL300
01093      12  FILLER PIC X(22) VALUE 'NUMBER IS INVALID     '.         EL300
01094                                                                   EL300
01095      12  FILLER PIC X(22) VALUE '0085*CLAS* CERTIFICATE'.         EL300
01096      12  FILLER PIC X(22) VALUE ' MASTER SWITCH INVALID'.         EL300
01097                                                                   EL300
01098      12  FILLER PIC X(22) VALUE '0086*OPEN             '.         EL300
01099      12  FILLER PIC X(22) VALUE '                      '.         EL300
01100                                                                   EL300
01101      12  FILLER PIC X(22) VALUE '0087*OPEN             '.         EL300
01102      12  FILLER PIC X(22) VALUE '                      '.         EL300
01103                                                                   EL300
01104      12  FILLER PIC X(22) VALUE '0088*OPEN             '.         EL300
01105      12  FILLER PIC X(22) VALUE '                      '.         EL300
01106                                                                   EL300
01107      12  FILLER PIC X(22) VALUE '0089*OPEN             '.         EL300
01108      12  FILLER PIC X(22) VALUE '                      '.         EL300
01109                                                                   EL300
01110      12  FILLER PIC X(22) VALUE '0090*CLAS* MORTALITY O'.         EL300
01111      12  FILLER PIC X(22) VALUE 'VERRIDE IS INVALID    '.         EL300
01112                                                                   EL300
01113      12  FILLER PIC X(22) VALUE '0091*OPEN             '.         EL300
01114      12  FILLER PIC X(22) VALUE '                      '.         EL300
01115                                                                   EL300
01116      12  FILLER PIC X(22) VALUE '0092*OVERRIDE* PROGRAM'.         EL300
01117      12  FILLER PIC X(22) VALUE ' NO. NOT NUMERIC      '.         EL300
01118                                                                   EL300
01119      12  FILLER PIC X(22) VALUE '0093*OVERRIDE*    CODE'.         EL300
01120      12  FILLER PIC X(22) VALUE ' IS INVALID           '.         EL300
01121                                                                   EL300
01122      12  FILLER PIC X(22) VALUE '0094*OVERRIDE* PROGRAM'.         EL300
01123      12  FILLER PIC X(22) VALUE ' NO. IS NON-EXISTENT  '.         EL300
01124                                                                   EL300
01125      12  FILLER PIC X(22) VALUE '0095*OVERRIDE* OPTION '.         EL300
01126      12  FILLER PIC X(22) VALUE 'NOT AVAILABLE         '.         EL300
01127                                                                   EL300
01128      12  FILLER PIC X(22) VALUE '0096*OVERRIDE* MORE TH'.         EL300
01129      12  FILLER PIC X(22) VALUE 'AN 1 PER PROGRAM      '.         EL300
01130                                                                   EL300
01131 *********************************  DELETED TABLE ENTRIES  ****    EL300
01132      12  FILLER PIC X(44) VALUE ZERO.                             EL300
01133 **************************************************************    EL300
01134                                                                   EL300
01135      12  FILLER PIC X(22) VALUE '0098REQUESTED PROGRAM '.         EL300
01136      12  FILLER PIC X(22) VALUE 'IS NOT AVAILABLE      '.         EL300
01137                                                                   EL300
01138      12  FILLER PIC X(22) VALUE '0099WRITE ERROR ON DIS'.         EL300
01139      12  FILLER PIC X(22) VALUE 'K DATE FILE           '.         EL300
01140                                                                   EL300
01141 *********************************  DELETED TABLE ENTRIES  ****    EL300
01142      12  FILLER PIC X(44) VALUE ZERO.                             EL300
01143 **************************************************************    EL300
01144                                                                   EL300
01145      12  FILLER PIC X(22) VALUE '0101OPTION TABLE SYSTE'.         EL300
01146      12  FILLER PIC X(22) VALUE 'M CODE INVALID        '.         EL300
01147                                                                   EL300
01148      12  FILLER PIC X(22) VALUE '0102LIFE TYPE TABLE EX'.         EL300
01149      12  FILLER PIC X(22) VALUE 'CEEDS ALLOWABLE SIZE  '.         EL300
01150                                                                   EL300
01151 *********************************  DELETED TABLE ENTRIES  ****    EL300
01152      12  FILLER PIC X(220) VALUE ZERO.                            EL300
01153 **************************************************************    EL300
01154                                                                   EL300
01155      12  FILLER PIC X(22) VALUE '0108COMPENSATION WRITE'.         EL300
01156      12  FILLER PIC X(22) VALUE ' OFF NOT NUMERIC      '.         EL300
01157                                                                   EL300
01158      12  FILLER PIC X(44) VALUE HIGH-VALUES.                      EL300
01159                                                                   EL300
01160  01  ERR-MESSAGES-R                  REDEFINES ERR-MESSAGES.      EL300
01161      12  FILLER                      OCCURS 109 TIMES.            EL300
01162          16  ERR-LIST                PIC  9.                      EL300
01163          16  ERR-CODE                PIC  9(3).                   EL300
01164          16  ERR-MSG                 PIC  X(40).                  EL300
01165                                                                   EL300
01166  01  ERR-TABLE.                                                   EL300
01167      12  FILLER                      OCCURS 50 TIMES.             EL300
01168          16  ERR-NBR-H               PIC  9(3).                   EL300
01169          16  ERR-DETAIL-H            PIC  X(44).                  EL300
01170      12  ERR-NBR                     PIC  9(3).                   EL300
01171      12  ERR-DETAIL                  PIC  X(44).                  EL300
01172                                                                   EL300
01173  01  SAVE-HEAD.                                                   EL300
01174      12  SAVE-HEAD-1                 PIC  X(70).                  EL300
01175      12  SAVE-HEAD-2                 PIC  X(70).                  EL300
01176      12  SAVE-LINE                   PIC  X(70).                  EL300
01177      EJECT                                                        EL300
01178  LINKAGE SECTION.                                                 EL300
01179                                                                   EL300
01180                                                                   EL300
01181      EJECT                                                        EL300
01182  PROCEDURE DIVISION.                                              EL300
01183                                                                   EL300
01184      DISPLAY ' '.                                                 EL300
01185      DISPLAY '**** THE FOLLOWING MESSAGES WERE CREATED BY'        EL300
01186          ' EL300 ****'.                                           EL300
01187      DISPLAY ' '.                                                 EL300
01188                                                                   EL300
01189      ACCEPT WS-ACCEPT-DATE       FROM  DATE.                      EL300
01190                                                                   EL300
01191      MOVE WS-AD-YY               TO  WS-CD-YY.                    EL300
01192      MOVE WS-AD-MM               TO  WS-CD-MM.                    EL300
01193      MOVE WS-AD-DD               TO  WS-CD-DD.                    EL300
01194                                                                   EL300
01195      ACCEPT WS-TIME-OF-DAY       FROM  TIME.                      EL300
01196                                                                   EL300
01197      OPEN INPUT CARD-FILE                                         EL300
01198                 ELCNTL                                            EL300
01199                 ELPGMN                                            EL300
01200                 ELPGMO                                            EL300
01201                 ELPGMS                                            EL300
01202           OUTPUT PRNT-FILE                                        EL300
01203                  DISK-FILE.                                       EL300
01204                                                                   EL300
01205      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL300
01206          NEXT SENTENCE                                            EL300
01207        ELSE                                                       EL300
01208          MOVE 'ERROR OCCURED OPEN - ELCNTL'  TO  WS-ABEND-MESSAGE EL300
01209          MOVE ELCNTL-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL300
01210          PERFORM ABEND-PGM.                                       EL300
01211                                                                   EL300
01212      IF ELPGMN-FILE-STATUS  = '00' OR '97'                        EL300
01213          NEXT SENTENCE                                            EL300
01214        ELSE                                                       EL300
01215          MOVE 'ERROR OCCURED OPEN - ELPGMN'  TO  WS-ABEND-MESSAGE EL300
01216          MOVE ELPGMN-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL300
01217          PERFORM ABEND-PGM.                                       EL300
01218                                                                   EL300
01219      IF ELPGMS-FILE-STATUS  = '00' OR '97'                        EL300
01220          NEXT SENTENCE                                            EL300
01221        ELSE                                                       EL300
01222          MOVE 'ERROR OCCURED OPEN - ELPGMS'  TO  WS-ABEND-MESSAGE EL300
01223          MOVE ELPGMS-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL300
01224          PERFORM ABEND-PGM.                                       EL300
01225                                                                   EL300
01226      IF ELPGMO-FILE-STATUS  = '00' OR '97'                        EL300
01227          NEXT SENTENCE                                            EL300
01228        ELSE                                                       EL300
01229          MOVE 'ERROR OCCURED OPEN - ELPGMO'  TO  WS-ABEND-MESSAGE EL300
01230          MOVE ELPGMO-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL300
01231          PERFORM ABEND-PGM.                                       EL300
01232                                                                   EL300
01233      MOVE WS-CURRENT-DATE        TO T02-D                         EL300
01234                                     T12-D.                        EL300
01235      MOVE ZERO                   TO FACT-FACTORS                  EL300
01236                                     PROGRAM-NAME-TABLE.           EL300
01237      MOVE ALL '9'                TO ERR-TABLE.                    EL300
01238                                                                   EL300
01239 *                                                                 EL300
01240 *    READ COLC AND CLAS DATE CARDS                                EL300
01241 *                                                                 EL300
01242                                                                   EL300
01243      READ CARD-FILE RECORD INTO CLAS-CARD                         EL300
01244          AT END GO TO READ-END.                                   EL300
020303     DISPLAY INPUT-CARD.
01245                                                                   EL300
01246      IF CLAS-ID = 'COLC'                                          EL300
01247          PERFORM C04-START THRU C04-EXIT                          EL300
01248       ELSE                                                        EL300
01249          MOVE 1                  TO ERR-LIST (02).                EL300
01250                                                                   EL300
01251      READ CARD-FILE RECORD INTO CLAS-CARD                         EL300
01252          AT END GO TO READ-END.                                   EL300
01253                                                                   EL300
020303     DISPLAY INPUT-CARD.
01254      IF CLAS-ID = 'CLAS'                                          EL300
01255          PERFORM C01-START THRU C01-EXIT                          EL300
01256       ELSE                                                        EL300
01257          MOVE 1                  TO ERR-LIST (78).                EL300
01258                                                                   EL300
01259  READ-NEXT.                                                       EL300
01260      PERFORM ERROR-START THRU ERROR-EXIT.                         EL300
01261                                                                   EL300
01262      IF ERROR-SW = 1                                              EL300
01263          GO TO PRINT-ERROR.                                       EL300
01264                                                                   EL300
01265 *                                                                 EL300
01266 *    APPLY PROGRAM SWITCHES - LOAD CLASIC PROGRAM NAME FILE       EL300
01267 *                                                                 EL300
01268                                                                   EL300
01269      PERFORM A01-START THRU A01-EXIT.                             EL300
01270      PERFORM L02-START THRU L02-EXIT.                             EL300
01271                                                                   EL300
01272 *                                                                 EL300
01273 *    READ DATE CARDS                                              EL300
01274 *                                                                 EL300
01275                                                                   EL300
01276  READ-CARD.                                                       EL300
01277      READ CARD-FILE RECORD INTO CLAS-CARD                         EL300
01278          AT END GO TO READ-END.                                   EL300
01279                                                                   EL300
01280      IF CLAS-ID = 'CLAS'                                          EL300
01281          PERFORM C01-START THRU C01-EXIT                          EL300
01282          GO TO READ-CARD.                                         EL300
01283      IF CLPS-ID = 'CLPS' OR 'FMTO' OR 'PRCO'                      EL300
01284                                    OR 'PRTO' OR 'TOTO'            EL300
01285          PERFORM C02-START THRU C02-EXIT                          EL300
01286          GO TO READ-CARD.                                         EL300
01287      IF CLAF-ID = 'CLAF'                                          EL300
01288          PERFORM C03-START THRU C03-EXIT                          EL300
01289          GO TO READ-CARD.                                         EL300
01290      IF COLC-ID = 'COLC'                                          EL300
01291          PERFORM C04-START THRU C04-EXIT                          EL300
01292          GO TO READ-CARD.                                         EL300
01293                                                                   EL300
01294      MOVE 1 TO ERR-LIST (74).                                     EL300
01295                                                                   EL300
01296      GO TO READ-CARD.                                             EL300
01297                                                                   EL300
01298  READ-END.                                                        EL300
01299      IF CLAS-CNT = ZERO                                           EL300
01300          MOVE 1 TO ERR-LIST (77).                                 EL300
01301      IF COLC-CNT = ZERO                                           EL300
01302          MOVE 1 TO ERR-LIST (07).                                 EL300
01303      PERFORM ERROR-START THRU ERROR-EXIT.                         EL300
01304      IF ERROR-SW = 1                                              EL300
01305          GO TO PRINT-ERROR.                                       EL300
01306                                                                   EL300
01307 *                                                                 EL300
01308 *    LOAD CLAS-IC CONTROL FILES INTO TABLES                       EL300
01309 *                                                                 EL300
01310                                                                   EL300
01311  LOAD-CLASIC-FILES.                                               EL300
01312      PERFORM L01-START THRU L01-EXIT.                             EL300
01313                                                                   EL300
01314 *                                                                 EL300
01315 *    WRITE DISK RECORDS                                           EL300
01316 *                                                                 EL300
01317                                                                   EL300
01318      PERFORM W01-START THRU W01-EXIT.                             EL300
01319      PERFORM W02-START THRU W02-EXIT.                             EL300
01320      PERFORM W03-START THRU W03-EXIT.                             EL300
01321      PERFORM W04-START THRU W04-EXIT.                             EL300
01322      PERFORM W05-START THRU W05-EXIT.                             EL300
01323      PERFORM W06-START THRU W06-EXIT.                             EL300
01324      PERFORM W07-START THRU W07-EXIT.                             EL300
01325      PERFORM W08-START THRU W08-EXIT.                             EL300
01326      PERFORM W09-START THRU W09-EXIT.                             EL300
01327      PERFORM W10-START THRU W10-EXIT.                             EL300
01328      PERFORM W12-START THRU W12-EXIT.                             EL300
01329      PERFORM W13-START THRU W13-EXIT.                             EL300
01330      PERFORM W14-START THRU W14-EXIT.                             EL300
01331      PERFORM W15-START THRU W15-EXIT.                             EL300
01332      PERFORM W16-START THRU W16-EXIT.                             EL300
01333      PERFORM W17-START THRU W17-EXIT.                             EL300
01334      PERFORM ERROR-START THRU ERROR-EXIT.                         EL300
01335      IF ERROR-SW = 1                                              EL300
01336          GO TO PRINT-ERROR.                                       EL300
01337                                                                   EL300
01338 *                                                                 EL300
01339 *    PRINT DATE CARD LOAD                                         EL300
01340 *                                                                 EL300
01341                                                                   EL300
01342  PRINT-LOAD.                                                      EL300
01343      MOVE '   CLAS-IC DATE FILE LOAD   ' TO T01-T.                EL300
01344      MOVE T01-T                  TO T11-T.                        EL300
01345      PERFORM P01-START THRU P01-EXIT.                             EL300
01346      PERFORM P02-START THRU P02-EXIT.                             EL300
01347      PERFORM P03-START THRU P03-EXIT.                             EL300
01348      PERFORM P04-START THRU P04-EXIT.                             EL300
01349      GO TO EOJ.                                                   EL300
01350                                                                   EL300
01351 *                                                                 EL300
01352 *    PRINT DATE CARD ERROR                                        EL300
01353 *                                                                 EL300
01354                                                                   EL300
01355  PRINT-ERROR.                                                     EL300
01356      MOVE '  CLAS-IC DATE LOAD ERRORS  ' TO T01-T.                EL300
01357      MOVE T01-T                  TO T11-T.                        EL300
01358      PERFORM P16-START THRU P16-EXIT.                             EL300
01359      MOVE 'PROGRAM ABENDING DUE TO ERRORS LISTED IN ERROR RPT'    EL300
01360                                  TO WS-ABEND-MESSAGE.             EL300
01361      PERFORM ABEND-PGM.                                           EL300
01362      EJECT                                                        EL300
01363                                                                   EL300
01364 *                                                                 EL300
01365 *    APPLY PROGRAM SWITCHES                                       EL300
01366 *                                                                 EL300
01367                                                                   EL300
01368  A01-START.                                                       EL300
01369      MOVE ZERO                   TO X1 X3.                        EL300
01370      MOVE +9999 TO X2.                                            EL300
01371                                                                   EL300
01372      MOVE ALL '9'                TO PROGRAM-TBL.                  EL300
01373                                                                   EL300
01374      MOVE SPACE                  TO PROG-SET.                     EL300
01375      MOVE SPACE                  TO SWITCH-TBL.                   EL300
01376      MOVE SPACE                  TO PROGRAM-OVERRIDE.             EL300
01377      MOVE SWITCH-TBL             TO SWITCH-SAVE.                  EL300
01378                                                                   EL300
01379      MOVE ZERO                   TO OPTION-CNT.                   EL300
01380                                                                   EL300
01381      MOVE 'ELPGMS' TO VSAM-PGM.                                   EL300
01382                                                                   EL300
01383      MOVE LOW-VALUE TO PS-CONTROL-PRIMARY.                        EL300
01384      MOVE CLASIC-COMPANY-CD TO PS-COMPANY-CD.                     EL300
01385                                                                   EL300
01386      START ELPGMS KEY IS NOT LESS THAN PS-CONTROL-PRIMARY         EL300
01387               INVALID KEY   GO TO A01-LOOP-0-END.                 EL300
01388                                                                   EL300
01389  A01-LOOP-0.                                                      EL300
01390      READ ELPGMS NEXT RECORD                                      EL300
01391          AT END                                                   EL300
01392            GO TO A01-LOOP-0-END.                                  EL300
01393                                                                   EL300
01394      IF CLASIC-COMPANY-CD NOT = PS-COMPANY-CD                     EL300
01395          GO TO A01-LOOP-0-END.                                    EL300
01396                                                                   EL300
01397      MOVE +0 TO X4.                                               EL300
01398                                                                   EL300
01399      IF PS-PROGRAM-SEQUENCE NOT NUMERIC                           EL300
01400          GO TO A01-LOOP-0.                                        EL300
01401                                                                   EL300
01402      IF (PS-SYSTEM-CODE = 'EC')                                   EL300
01403          IF ((PS-PROGRAM-SEQUENCE GREATER 000)                    EL300
01404             AND (PS-PROGRAM-SEQUENCE LESS 301))                   EL300
01405              GO TO A01-LOOP-0-ADD.                                EL300
01406                                                                   EL300
01407                                                                   EL300
01408      IF (PS-SYSTEM-CODE = 'EL')                                   EL300
01409          IF ((PS-PROGRAM-SEQUENCE GREATER 300)                    EL300
01410             AND (PS-PROGRAM-SEQUENCE LESS 400))                   EL300
01411              GO TO A01-LOOP-0-ADD.                                EL300
01412                                                                   EL300
01413      IF (PS-SYSTEM-CODE = 'EC')                                   EL300
01414          IF ((PS-PROGRAM-SEQUENCE GREATER 399)                    EL300
01415             AND (PS-PROGRAM-SEQUENCE LESS 500))                   EL300
01416              GO TO A01-LOOP-0-ADD.                                EL300
01417                                                                   EL300
01418      IF (PS-SYSTEM-CODE = 'EL')                                   EL300
01419          IF ((PS-PROGRAM-SEQUENCE GREATER 499)                    EL300
01420             AND (PS-PROGRAM-SEQUENCE LESS 601))                   EL300
01421              GO TO A01-LOOP-0-ADD.                                EL300
01422                                                                   EL300
01423      IF (PS-SYSTEM-CODE = 'EL')                                   EL300
01424          IF ((PS-PROGRAM-SEQUENCE GREATER 800)                    EL300
01425             AND (PS-PROGRAM-SEQUENCE LESS 900))                   EL300
01426              GO TO A01-LOOP-0-ADD.                                EL300
01427                                                                   EL300
01428      IF (PS-SYSTEM-CODE = 'EC')                                   EL300
01429          IF ((PS-PROGRAM-SEQUENCE GREATER 600)                    EL300
01430             AND (PS-PROGRAM-SEQUENCE LESS 801))                   EL300
01431              GO TO A01-LOOP-0-ADD.                                EL300
01432                                                                   EL300
01433      IF (PS-SYSTEM-CODE = 'EC')                                   EL300
01434          IF  PS-PROGRAM-SEQUENCE GREATER 899                      EL300
01435              GO TO A01-LOOP-0-ADD.                                EL300
01436                                                                   EL300
01437      IF (PS-SYSTEM-CODE = 'GL')                                   EL300
01438          IF ((PS-PROGRAM-SEQUENCE GREATER 799)                    EL300
01439             AND (PS-PROGRAM-SEQUENCE LESS 900))                   EL300
01440              GO TO A01-LOOP-0-ADD.                                EL300
01441                                                                   EL300
01442      GO TO A01-LOOP-0.                                            EL300
01443                                                                   EL300
01444      EJECT                                                        EL300
01445  A01-LOOP-0-ADD.                                                  EL300
01446      ADD +1 TO X4.                                                EL300
01447                                                                   EL300
01448      IF X4 GREATER +4                                             EL300
01449          GO TO A01-LOOP-0.                                        EL300
01450                                                                   EL300
01451      IF PS-FREQUENCY-CODE (X4) = SPACES                           EL300
01452          NEXT SENTENCE                                            EL300
01453       ELSE                                                        EL300
01454          ADD +1 TO X3                                             EL300
01455          MOVE PS-FREQUENCY-CODE (X4) TO SW-CMD (X3)               EL300
01456          MOVE PS-PROGRAM-SEQUENCE    TO SW-PROG (X3)              EL300
01457          MOVE PS-PRINT-OPTION (X4)   TO SW-PRNT (X3)              EL300
01458          MOVE PS-FORMAT-OPTION (X4)  TO SW-FMT (X3)               EL300
01459          MOVE PS-TOTAL-OPTION (X4)   TO SW-TOT (X3)               EL300
01460          MOVE PS-PROCESS-OPTION (X4) TO SW-PROC (X3)              EL300
01461 *        TRANSFORM SW-OPT (X3) FROM SPACES TO '9'.                EL300
01462          INSPECT SW-OPT (X3) REPLACING ALL SPACE BY '9'.          EL300
01463                                                                   EL300
01464      IF X3 GREATER +1500                                          EL300
01465          MOVE 1 TO ERR-LIST (09)                                  EL300
01466          GO TO A01-LOOP-2.                                        EL300
01467                                                                   EL300
01468      GO TO A01-LOOP-0-ADD.                                        EL300
01469                                                                   EL300
01470      EJECT                                                        EL300
01471  A01-LOOP-0-END.                                                  EL300
01472      MOVE X3 TO SWITCH-CNT.                                       EL300
01473                                                                   EL300
01474      ADD +1 TO X3.                                                EL300
01475                                                                   EL300
01476      MOVE HIGH-VALUES TO SWITCH-LINE (X3).                        EL300
01477                                                                   EL300
01478  A01-LOOP-2.                                                      EL300
01479      ADD 1 TO X1.                                                 EL300
01480                                                                   EL300
01481      IF X1 IS GREATER THAN SWITCH-CNT                             EL300
01482          GO TO A01-APPLY-DEFAULTS.                                EL300
01483                                                                   EL300
01484      MOVE +1 TO X3.                                               EL300
01485                                                                   EL300
01486  A01-LOOP-3.                                                      EL300
01487      IF SW-CMD (X1) = HIGH-VALUES                                 EL300
01488          MOVE 1                  TO ERR-LIST (15)                 EL300
01489          GO TO A01-END.                                           EL300
01490                                                                   EL300
01491      IF SW-CMD (X1) NOT = CMD-TEST (X3)                           EL300
01492          IF X3 LESS THAN +25                                      EL300
01493              ADD +1  TO  X3                                       EL300
01494              GO TO A01-LOOP-3                                     EL300
01495            ELSE                                                   EL300
01496              GO TO A01-LOOP-2.                                    EL300
01497                                                                   EL300
01498      MOVE SW-PROG (X1) TO X2.                                     EL300
01499                                                                   EL300
01500      GO TO A01-01  A01-02  A01-03  A01-04  A01-05                 EL300
01501            A01-06  A01-07  A01-08  A01-09  A01-10                 EL300
01502            A01-11  A01-12  A01-13  A01-14  A01-15                 EL300
01503            A01-16  A01-17  A01-18  A01-19  A01-20                 EL300
01504            A01-21  A01-22  A01-23  A01-04  A01-05                 EL300
01505            DEPENDING ON X3.                                       EL300
01506 *                                                                 EL300
01507 *        NONE - OPTION IS ALWAYS SET ON                           EL300
01508  A01-01.                                                          EL300
01509      MOVE SW-OPT (X1)            TO PROG-TBL (X2).                EL300
01510                                                                   EL300
01511      MOVE SPACE                  TO PROG-OR (X2).                 EL300
01512                                                                   EL300
01513      IF PROG-TBL-PRT (X2) = 'F' OR 'B'                            EL300
01514          MOVE 1                  TO FICHE-SW.                     EL300
01515                                                                   EL300
01516      GO TO A01-LOOP-2.                                            EL300
01517 *                                                                 EL300
01518 *        ED=Y - EDIT-NUMBER = A VALUE                             EL300
01519  A01-02.                                                          EL300
01520      IF CTL-PEND-ACT-FILE = SPACE                                 EL300
01521          GO TO A01-LOOP-2.                                        EL300
01522      GO TO A01-01.                                                EL300
01523 *                                                                 EL300
01524 *        ED=N - EDIT-NUMBER = SPACE                               EL300
01525  A01-03.                                                          EL300
01526      IF CTL-PEND-ACT-FILE = SPACE                                 EL300
01527          GO TO A01-01.                                            EL300
01528      GO TO A01-LOOP-2.                                            EL300
01529 *                                                                 EL300
01530 *        UD=Y - UPDATE-SWITCH = 1                                 EL300
01531  A01-04.                                                          EL300
01532      IF CTL-EP-SW = '1'                                           EL300
01533          GO TO A01-01.                                            EL300
01534      GO TO A01-LOOP-2.                                            EL300
01535 *                                                                 EL300
01536 *        UD=N - UPDATE-SWITCH = SPACE                             EL300
01537  A01-05.                                                          EL300
01538      IF CTL-EP-SW = SPACE                                         EL300
01539          GO TO A01-01.                                            EL300
01540      GO TO A01-LOOP-2.                                            EL300
01541 *                                                                 EL300
01542 *        MO=Y - RUN-MONTH = 12                                    EL300
01543  A01-06.                                                          EL300
01544      IF CTL-RUN-MO = 12                                           EL300
01545          GO TO A01-01.                                            EL300
01546      GO TO A01-LOOP-2.                                            EL300
01547 *                                                                 EL300
01548 *        MO/Y - RUN-MONTH NOT = 12                                EL300
01549  A01-07.                                                          EL300
01550      IF CTL-RUN-MO = 12                                           EL300
01551          GO TO A01-LOOP-2.                                        EL300
01552      GO TO A01-01.                                                EL300
01553 *                                                                 EL300
01554 *        MO=Q - RUN-MONTH = 3, 6, 9, OR 12                        EL300
01555  A01-08.                                                          EL300
01556      IF CTL-RUN-MO = 3 OR 6 OR 9 OR 12                            EL300
01557          GO TO A01-01.                                            EL300
01558      GO TO A01-LOOP-2.                                            EL300
01559 *                                                                 EL300
01560 *        MO/Q - RUN-MONTH NOT = 3, 6, 9, OR 12                    EL300
01561  A01-09.                                                          EL300
01562      IF CTL-RUN-MO = 3 OR 6 OR 9 OR 12                            EL300
01563          GO TO A01-LOOP-2.                                        EL300
01564      GO TO A01-01.                                                EL300
01565 *                                                                 EL300
01566 *        MO=E - RUN-MONTH = 2, 4, 6, 8, 10 OR 12                  EL300
01567  A01-10.                                                          EL300
01568      IF CTL-RUN-MO = 2 OR 4 OR 6 OR 8 OR 10 OR 12                 EL300
01569          GO TO A01-01.                                            EL300
01570      GO TO A01-LOOP-2.                                            EL300
01571 *                                                                 EL300
01572 *        MO=O - RUN-MONTH = 1, 3, 5, 7, 9 OR 11                   EL300
01573  A01-11.                                                          EL300
01574      IF CTL-RUN-MO = 1 OR 3 OR 5 OR 7 OR 9 OR 11                  EL300
01575          GO TO A01-01.                                            EL300
01576      GO TO A01-LOOP-2.                                            EL300
01577 *                                                                 EL300
01578 *        MO=1 - RUN-MONTH = JANUARY                               EL300
01579  A01-12.                                                          EL300
01580      IF CTL-RUN-MO = 1                                            EL300
01581          GO TO A01-01.                                            EL300
01582      GO TO A01-LOOP-2.                                            EL300
01583 *                                                                 EL300
01584 *        MO=2 - RUN-MONTH = FEBRUARY                              EL300
01585  A01-13.                                                          EL300
01586      IF CTL-RUN-MO = 2                                            EL300
01587          GO TO A01-01.                                            EL300
01588      GO TO A01-LOOP-2.                                            EL300
01589 *                                                                 EL300
01590 *        MO=3 - RUN-MONTH = MARCH                                 EL300
01591  A01-14.                                                          EL300
01592      IF CTL-RUN-MO = 3                                            EL300
01593          GO TO A01-01.                                            EL300
01594      GO TO A01-LOOP-2.                                            EL300
01595 *                                                                 EL300
01596 *        MO=4 - RUN-MONTH = APRIL                                 EL300
01597  A01-15.                                                          EL300
01598      IF CTL-RUN-MO = 4                                            EL300
01599          GO TO A01-01.                                            EL300
01600      GO TO A01-LOOP-2.                                            EL300
01601 *                                                                 EL300
01602 *        MO=5 - RUN-MONTH = MAY                                   EL300
01603  A01-16.                                                          EL300
01604      IF CTL-RUN-MO = 5                                            EL300
01605          GO TO A01-01.                                            EL300
01606      GO TO A01-LOOP-2.                                            EL300
01607 *                                                                 EL300
01608 *        MO=6 - RUN-MONTH = JUNE                                  EL300
01609  A01-17.                                                          EL300
01610      IF CTL-RUN-MO = 6                                            EL300
01611          GO TO A01-01.                                            EL300
01612      GO TO A01-LOOP-2.                                            EL300
01613 *                                                                 EL300
01614 *        MO=7 - RUN-MONTH = JULY                                  EL300
01615  A01-18.                                                          EL300
01616      IF CTL-RUN-MO = 7                                            EL300
01617          GO TO A01-01.                                            EL300
01618      GO TO A01-LOOP-2.                                            EL300
01619 *                                                                 EL300
01620 *        MO=8 - RUN-MONTH = AUGUST                                EL300
01621  A01-19.                                                          EL300
01622      IF CTL-RUN-MO = 8                                            EL300
01623          GO TO A01-01.                                            EL300
01624      GO TO A01-LOOP-2.                                            EL300
01625 *                                                                 EL300
01626 *        MO=9 - RUN-MONTH = SEPTEMBER                             EL300
01627  A01-20.                                                          EL300
01628      IF CTL-RUN-MO = 9                                            EL300
01629          GO TO A01-01.                                            EL300
01630      GO TO A01-LOOP-2.                                            EL300
01631 *                                                                 EL300
01632 *        MO=A - RUN-MONTH = OCTOBER                               EL300
01633  A01-21.                                                          EL300
01634      IF CTL-RUN-MO = 10                                           EL300
01635          GO TO A01-01.                                            EL300
01636      GO TO A01-LOOP-2.                                            EL300
01637 *                                                                 EL300
01638 *        MO=B - RUN-MONTH = NOVEMBER                              EL300
01639  A01-22.                                                          EL300
01640      IF CTL-RUN-MO = 11                                           EL300
01641          GO TO A01-01.                                            EL300
01642      GO TO A01-LOOP-2.                                            EL300
01643 *                                                                 EL300
01644 *        MO=C - RUN-MONTH = DECEMBER                              EL300
01645  A01-23.                                                          EL300
01646      IF CTL-RUN-MO = 12                                           EL300
01647          GO TO A01-01.                                            EL300
01648      GO TO A01-LOOP-2.                                            EL300
01649 *                                                                 EL300
01650  A01-APPLY-DEFAULTS.                                              EL300
01651      MOVE LOW-VALUE TO PO-CONTROL-PRIMARY.                        EL300
01652      MOVE 'ELPGMO' TO VSAM-PGM.                                   EL300
01653      START ELPGMO  KEY IS NOT LESS THAN PO-CONTROL-PRIMARY        EL300
01654                                                                   EL300
01655      IF ELPGMO-FILE-STATUS NOT = ZERO                             EL300
01656          MOVE 'ERROR OCCURED START - ELPGMO'  TO  WS-ABEND-MESSAGEEL300
01657          MOVE ELPGMO-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL300
01658          PERFORM ABEND-PGM.                                       EL300
01659                                                                   EL300
01660                                                                   EL300
01661  A01-DEFAULT-LOOP-1.                                              EL300
01662      READ ELPGMO NEXT RECORD                                      EL300
01663           AT END   GO TO A01-END.                                 EL300
01664                                                                   EL300
01665      IF (PO-SYSTEM-CODE = 'EL')                                   EL300
01666          IF ((PO-PROGRAM-SEQUENCE GREATER 300)                    EL300
01667             AND (PO-PROGRAM-SEQUENCE LESS 400))                   EL300
01668              GO TO A01-ADD-DEFAULT.                               EL300
01669      IF (PO-SYSTEM-CODE = 'EL')                                   EL300
01670          IF ((PO-PROGRAM-SEQUENCE GREATER 800)                    EL300
01671             AND (PO-PROGRAM-SEQUENCE LESS 900))                   EL300
01672              GO TO A01-ADD-DEFAULT.                               EL300
01673      IF (PO-SYSTEM-CODE = 'EC')                                   EL300
01674          IF ((PO-PROGRAM-SEQUENCE GREATER 000)                    EL300
01675             AND (PO-PROGRAM-SEQUENCE LESS 301))                   EL300
01676              GO TO A01-ADD-DEFAULT.                               EL300
01677      IF (PO-SYSTEM-CODE = 'EC')                                   EL300
01678          IF ((PO-PROGRAM-SEQUENCE GREATER 399)                    EL300
01679             AND (PO-PROGRAM-SEQUENCE LESS 801))                   EL300
01680              GO TO A01-ADD-DEFAULT.                               EL300
01681      IF (PO-SYSTEM-CODE = 'EC')                                   EL300
01682          IF  PO-PROGRAM-SEQUENCE GREATER 900                      EL300
01683              GO TO A01-ADD-DEFAULT.                               EL300
01684      IF (PO-SYSTEM-CODE = 'GL')                                   EL300
01685          IF ((PO-PROGRAM-SEQUENCE GREATER 799)                    EL300
01686             AND (PO-PROGRAM-SEQUENCE LESS 900))                   EL300
01687              GO TO A01-ADD-DEFAULT.                               EL300
01688                                                                   EL300
01689      GO TO A01-DEFAULT-LOOP-1.                                    EL300
01690                                                                   EL300
01691  A01-ADD-DEFAULT.                                                 EL300
01692      MOVE PO-PROGRAM-SEQUENCE    TO X1.                           EL300
01693      MOVE PO-SYSTEM-CODE         TO PROG-SYS-NAME (X1).           EL300
01694      IF PO-OPTION-TYPE = 'F'                                      EL300
01695          IF PROG-TBL-FMT (X1) = '9'                               EL300
01696             IF PO-PGM-OPTION-CD NOT = SPACE                       EL300
01697                MOVE PO-PGM-OPTION-CD TO PROG-TBL-FMT (X1)         EL300
01698                MOVE 'D'          TO PROG-OR-FMT (X1).             EL300
01699      IF PO-OPTION-TYPE = 'P'                                      EL300
01700          IF PROG-TBL-PRC (X1) = '9'                               EL300
01701             IF PO-PGM-OPTION-CD NOT = SPACE                       EL300
01702                MOVE PO-PGM-OPTION-CD TO PROG-TBL-PRC (X1)         EL300
01703                MOVE 'D'          TO PROG-OR-PRC (X1).             EL300
01704      IF PO-OPTION-TYPE = 'T'                                      EL300
01705          IF PROG-TBL-TOT (X1) = '9'                               EL300
01706             IF PO-PGM-OPTION-CD NOT = SPACE                       EL300
01707                MOVE PO-PGM-OPTION-CD TO PROG-TBL-TOT (X1)         EL300
01708                MOVE 'D'          TO PROG-OR-TOT (X1).             EL300
01709      GO TO A01-DEFAULT-LOOP-1.                                    EL300
01710                                                                   EL300
01711  A01-END.                                                         EL300
01712      MOVE SWITCH-SAVE            TO SWITCH-TBL.                   EL300
01713  A01-EXIT.                                                        EL300
01714      EXIT.                                                        EL300
01715      EJECT                                                        EL300
01716 *                                                                 EL300
01717 *    CARD INPUT 01 EDIT - CLAS DATE CARD                          EL300
01718 *                                                                 EL300
01719                                                                   EL300
01720  C01-START.                                                       EL300
01721      ADD 1 TO CLAS-CNT.                                           EL300
020303     DISPLAY 'C01-START for clas record'.
01722                                                                   EL300
01723      IF CLAS-CNT IS GREATER THAN 1                                EL300
01724          MOVE 1                  TO ERR-LIST (79)                 EL300
01725          GO TO C01-EXIT.                                          EL300
01726                                                                   EL300
01727       IF CLAS-RUN-DATE = SPACES OR ZEROS                          EL300
01728          MOVE WS-CURRENT-DATE    TO WORK-DATE                     EL300
01729          MOVE WORK-MO            TO CLAS-RUN-MO                   EL300
01730                                     WS-CLAS-RUN-MO                EL300
01731                                     DC-YMD-MONTH                  EL300
01732          MOVE WORK-DA            TO CLAS-RUN-DA                   EL300
01733                                     WS-CLAS-RUN-DA                EL300
01734                                     DC-YMD-DAY                    EL300
01735          MOVE WORK-YR            TO CLAS-RUN-YR                   EL300
01736                                     WS-CLAS-RUN-YR                EL300
01737                                     DC-YMD-YEAR                   EL300
01738          MOVE '3'                TO DC-OPTION-CODE                EL300
01739          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                 EL300
01740          IF NO-CONVERSION-ERROR                                   EL300
01741             MOVE DC-BIN-DATE-1   TO CLAS-BIN-RUN-DATE             EL300
01742             MOVE DC-ALPHA-CEN-N  TO WS-CLAS-RUN-CC                EL300
01743          END-IF                                                   EL300
01744        ELSE                                                       EL300
01745           MOVE CLAS-RUN-YR        TO WS-CLAS-RUN-YR               EL300
01746                                      DC-CYMD-YEAR                 EL300
01747           MOVE CLAS-RUN-MO        TO WS-CLAS-RUN-MO               EL300
01748                                      DC-CYMD-MONTH                EL300
01749           MOVE CLAS-RUN-DA        TO WS-CLAS-RUN-DA               EL300
01750                                      DC-CYMD-DAY                  EL300
01751 **        MOVE CLAS-RUN-CENTURY   TO WS-CLAS-RUN-CC               EL300
LGC189          IF CLAS-RUN-YR   >=     50
LGC189              MOVE 19             TO WS-CLAS-RUN-CC
01752                                      DC-CYMD-CEN                  EL300
LGC189          ELSE
LGC189              MOVE 20             TO WS-CLAS-RUN-CC
LGC189                                     DC-CYMD-CEN                  EL300
LGC189          END-IF                                                  EL300
01753           MOVE 'L' TO DC-OPTION-CODE                              EL300
01754           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                EL300
01755           IF NO-CONVERSION-ERROR                                  EL300
01756              MOVE DC-BIN-DATE-1 TO CLAS-BIN-RUN-DATE.             EL300
01757                                                                   EL300
01758      IF (CLASIC-RUN-DATE = ZERO) AND (CLAS-RUN-DATE = ZERO)       EL300
01759          MOVE 1                  TO ERR-LIST (08)                 EL300
01760          GO TO C01-SKIP-0.                                        EL300
01761                                                                   EL300
01762      IF CLASIC-RUN-DATE = WS-CLAS-RUN-DT                          EL300
01763          GO TO C01-SKIP-0.                                        EL300
01764                                                                   EL300
01765      IF OLC-DT-OVRIDE-SW = '1'                                    EL300
01766          GO TO C01-SKIP-0.                                        EL300
01767                                                                   EL300
01768      IF CLAS-RUN-DATE = +0                                        EL300
01769          MOVE CLASIC-RUN-DATE TO WS-CLAS-RUN-DT                   EL300
01770          GO TO C01-SKIP-0.                                        EL300
01771                                                                   EL300
01772      IF CLAS-RUN-DATE NOT = CLASIC-RUN-DATE                       EL300
01773          MOVE 1                  TO ERR-LIST (10).                EL300
01774                                                                   EL300
01775  C01-SKIP-0.                                                      EL300
01776                                                                   EL300
01777      MOVE ZEROS                  TO DATE-TEST.                    EL300
01778      MOVE CLAS-RUN-MO            TO DATE-MO.                      EL300
01779      MOVE CLAS-RUN-DA            TO DATE-DA.                      EL300
01780 **   MOVE CLAS-RUN-CENTURY       TO DATE-CC.                      EL300
01781      MOVE CLAS-RUN-YR            TO DATE-YY.                      EL300
LGC189     IF CLAS-RUN-YR   >=     50
LGC189         MOVE 19                 TO DATE-CC
LGC189     ELSE
LGC189         MOVE 20                 TO DATE-CC
LGC189     END-IF
01782      PERFORM DATE-START THRU DATE-EXIT.                           EL300
01783                                                                   EL300
01784      IF DATE-TEST = SPACE                                         EL300
01785          MOVE 1                  TO ERR-LIST (80).                EL300
01786                                                                   EL300
01787      IF CLAS-EP-DATE = +0 OR SPACES                               EL300
01788          MOVE WS-CLAS-RUN-DT     TO WS-CLAS-EP-DATE               EL300
01789          MOVE CLAS-RUN-DATE      TO CLAS-EP-DATE                  EL300
01790          GO TO C01-SKIP-1.                                        EL300
01791                                                                   EL300
01792 **   MOVE CLAS-RUN-CENTURY       TO DATE-CC.                      EL300
LGC189     IF CLAS-EP-YR   >=     50
LGC189         MOVE 19                 TO DATE-CC
LGC189     ELSE
LGC189         MOVE 20                 TO DATE-CC
LGC189     END-IF
01793      MOVE CLAS-EP-YR             TO DATE-YY.                      EL300
01794      MOVE CLAS-EP-DA             TO DATE-DA.                      EL300
01795      MOVE CLAS-EP-MO             TO DATE-MO.                      EL300
01796      PERFORM DATE-START THRU DATE-EXIT.                           EL300
01797                                                                   EL300
01798      IF DATE-TEST = SPACE                                         EL300
01799          MOVE 1                  TO ERR-LIST (81).                EL300
01800                                                                   EL300
01801      MOVE DATE-CC                TO WS-CLAS-EP-CC.                EL300
01802      MOVE DATE-YY                TO WS-CLAS-EP-YR.                EL300
01803      MOVE DATE-MO                TO WS-CLAS-EP-MO.                EL300
01804      MOVE DATE-DA                TO WS-CLAS-EP-DA.                EL300
01805                                                                   EL300
01806  C01-SKIP-1.                                                      EL300
01807      IF CLAS-EP-SW NOT = SPACE AND '1' AND '2' AND '3'            EL300
01808          MOVE 1 TO ERR-LIST (83).                                 EL300
01809                                                                   EL300
01810      IF CLAS-TAPE-BATCHES = SPACE                                 EL300
01811          GO TO C01-SKIP-3.                                        EL300
01812                                                                   EL300
01813      IF CLAS-TAPE-BATCHES IS NOT NUMERIC                          EL300
01814          MOVE 1                  TO ERR-LIST (84).                EL300
01815                                                                   EL300
01816  C01-SKIP-3.                                                      EL300
01817      IF CLAS-MORT-OR = SPACE                                      EL300
01818          GO TO C01-SKIP-4.                                        EL300
01819                                                                   EL300
01820      IF CLAS-MORT-1 = SPACE                                       EL300
01821          MOVE 1                  TO ERR-LIST (90).                EL300
01822                                                                   EL300
01823      IF CLAS-MORT-2 IS NOT NUMERIC                                EL300
01824          MOVE 1                  TO ERR-LIST (90).                EL300
01825                                                                   EL300
01826      IF CLAS-MORT-4 NOT = ZERO                                    EL300
01827          MOVE 1                  TO ERR-LIST (90).                EL300
01828                                                                   EL300
01829  C01-SKIP-4.                                                      EL300
01830      IF CLAS-COMPANY-TEST = SPACE                                 EL300
01831          MOVE SPACE              TO CLAS-COMPANY-NAME.            EL300
01832                                                                   EL300
01833      MOVE CLAS-ID            TO CTL-ID.                           EL300
01834      MOVE WS-CLAS-RUN-DT-N   TO CTL-RUN-DATE                      EL300
01835                                 WS-CTL-RUN-DATE-N.                EL300
01836      MOVE CLAS-ALPH-DATE     TO CTL-ALPH-DATE.                    EL300
01837      MOVE WS-CLAS-EP-DATE-N  TO CTL-EP-DATE                       EL300
01838                                 WS-CTL-EP-DATE-N.                 EL300
01839      MOVE CLAS-PEND-ACT-FILE TO CTL-PEND-ACT-FILE.                EL300
01840      MOVE CLAS-EP-SW         TO CTL-EP-SW.                        EL300
01841      MOVE CLAS-TAPE-BATCHES  TO CTL-TAPE-BATCHES.                 EL300
01842      MOVE CLAS-BIN-RUN-DATE  TO CTL-BIN-RUN-DATE                  EL300
01843 **   MOVE CLAS-RUN-CENTURY   TO CTL-RUN-CENTURY.                  EL300
LGC189     IF CLAS-EP-DATE  =  +0 OR SPACES
LGC189         IF CLAS-EP-YR    >=    50
LGC189             MOVE 19             TO CTL-RUN-CENTURY
LGC189         ELSE
LGC189             MOVE 20             TO CTL-RUN-CENTURY
LGC189         END-IF
LGC189     ELSE
LGC189         IF CLAS-RUN-YR   >=    50
LGC189             MOVE 19             TO CTL-RUN-CENTURY
LGC189         ELSE
LGC189             MOVE 20             TO CTL-RUN-CENTURY
LGC189         END-IF
LGC189     END-IF
01844      MOVE CLAS-MORT-OR       TO CTL-MORT-OR.                      EL300
01845      MOVE CLAS-DOC           TO CTL-DOC.                          EL300
01846      MOVE CLAS-COMPANY-NAME  TO CTL-COMPANY-NAME.                 EL300
01847                                                                   EL300
01848      MOVE CTL-COMPANY-NAME       TO WORK-1.                       EL300
01849                                                                   EL300
01850      IF WORK-1 = SPACE                                            EL300
01851          MOVE COMPANY-LINE (1)   TO WORK-1.                       EL300
01852                                                                   EL300
01853      IF WORK-1 = SPACE                                            EL300
01854          GO TO C01-SKIP-6.                                        EL300
01855                                                                   EL300
01856      MOVE SPACE                  TO WORK-2.                       EL300
01857                                                                   EL300
01858      MOVE ZERO                   TO X1.                           EL300
01859      MOVE 1                      TO X2.                           EL300
01860                                                                   EL300
01861  C01-LOOP-1.                                                      EL300
01862      ADD 1 TO X1.                                                 EL300
01863                                                                   EL300
01864      IF WK-1 (X1) = SPACE                                         EL300
01865          GO TO C01-LOOP-1.                                        EL300
01866                                                                   EL300
01867  C01-LOOP-2.                                                      EL300
01868      MOVE WK-1 (X1) TO WK-2 (X2).                                 EL300
01869                                                                   EL300
01870      ADD 1                      TO X1 X2.                         EL300
01871                                                                   EL300
01872      IF X1 IS LESS THAN 31                                        EL300
01873          GO TO C01-LOOP-2.                                        EL300
01874                                                                   EL300
01875  C01-SKIP-5.                                                      EL300
01876      MOVE 31                     TO X2.                           EL300
01877                                                                   EL300
01878      MOVE ZERO TO X3.                                             EL300
01879                                                                   EL300
01880  C01-LOOP-6.                                                      EL300
01881      SUBTRACT 1 FROM X2.                                          EL300
01882      ADD 1 TO X3.                                                 EL300
01883                                                                   EL300
01884      IF WK-2 (X2) = SPACE                                         EL300
01885          GO TO C01-LOOP-6.                                        EL300
01886                                                                   EL300
01887      DIVIDE 2 INTO X3 GIVING X1                                   EL300
01888          REMAINDER X4.                                            EL300
01889                                                                   EL300
01890      ADD X4 TO X1.                                                EL300
01891      MOVE 1                     TO X2.                            EL300
01892                                                                   EL300
01893      MOVE SPACE TO WORK-1.                                        EL300
01894                                                                   EL300
01895  C01-LOOP-7.                                                      EL300
01896      MOVE WK-2 (X2)             TO WK-1 (X1).                     EL300
01897                                                                   EL300
01898      IF X1 IS LESS THAN 30                                        EL300
01899          ADD 1                   TO X1 X2                         EL300
01900          GO TO C01-LOOP-7.                                        EL300
01901                                                                   EL300
01902      MOVE WORK-1                 TO CTL-COMPANY-NAME              EL300
01903                                     T02-N                         EL300
01904                                     T12-N.                        EL300
01905                                                                   EL300
01906  C01-SKIP-6.                                                      EL300
01907      IF CTL-ALPH-DATE = SPACE                                     EL300
01908          GO TO C01-SKIP-7.                                        EL300
01909                                                                   EL300
01910      MOVE CTL-ALPH-DATE          TO WORK-1.                       EL300
01911      MOVE SPACE                  TO WORK-2.                       EL300
01912                                                                   EL300
01913      MOVE ZERO                   TO X1.                           EL300
01914      MOVE 1                      TO X2.                           EL300
01915                                                                   EL300
01916  C01-LOOP-8.                                                      EL300
01917      ADD 1 TO X1.                                                 EL300
01918                                                                   EL300
01919      IF WK-1 (X1) = SPACE                                         EL300
01920          GO TO C01-LOOP-8.                                        EL300
01921                                                                   EL300
01922  C01-LOOP-9.                                                      EL300
01923      MOVE WK-1 (X1)             TO WK-2 (X2).                     EL300
01924      ADD 1 TO X1 X2.                                              EL300
01925                                                                   EL300
01926      IF X1 IS LESS THAN 19                                        EL300
01927          GO TO C01-LOOP-9.                                        EL300
01928                                                                   EL300
01929      MOVE 19                    TO X2.                            EL300
01930      MOVE ZERO                  TO X3.                            EL300
01931                                                                   EL300
01932  C01-LOOP-10.                                                     EL300
01933      SUBTRACT 1 FROM X2.                                          EL300
01934      ADD 1 TO X3.                                                 EL300
01935                                                                   EL300
01936      IF WK-2 (X2) = SPACE                                         EL300
01937          GO TO C01-LOOP-10.                                       EL300
01938                                                                   EL300
01939      DIVIDE 2 INTO X3 GIVING X1                                   EL300
01940          REMAINDER X4.                                            EL300
01941                                                                   EL300
01942      ADD X4 TO X1.                                                EL300
01943      MOVE 1                      TO X2.                           EL300
01944                                                                   EL300
01945      MOVE SPACE                  TO WORK-1.                       EL300
01946                                                                   EL300
01947  C01-LOOP-11.                                                     EL300
01948      MOVE WK-2 (X2)              TO WK-1 (X1).                    EL300
01949                                                                   EL300
01950      IF X1 IS LESS THAN 18                                        EL300
01951          ADD 1 TO X1 X2                                           EL300
01952          GO TO C01-LOOP-11.                                       EL300
01953                                                                   EL300
01954      MOVE WORK-1                 TO CTL-ALPH-DATE                 EL300
01955                                     T03-D                         EL300
01956                                     T13-D.                        EL300
01957                                                                   EL300
01958      GO TO C01-SKIP-8.                                            EL300
01959                                                                   EL300
01960  C01-SKIP-7.                                                      EL300
01961      IF ERR-LIST (80) = 1                                         EL300
01962          GO TO C01-SKIP-8.                                        EL300
01963                                                                   EL300
01964      MOVE MONTH-N (CTL-RUN-MO)  TO WORK-1.                        EL300
01965      MOVE MONTH-X (CTL-RUN-MO)  TO X1.                            EL300
01966                                                                   EL300
01967      MOVE CTL-RUN-DA            TO EDIT-DA.                       EL300
01968      MOVE CTL-RUN-YR            TO EDIT-YR.                       EL300
01969      MOVE CTL-RUN-CC            TO EDIT-CC.                       EL300
01970                                                                   EL300
01971      MOVE EDIT-DATE             TO WORK-2.                        EL300
01972                                                                   EL300
01973      MOVE ZERO                  TO X2.                            EL300
01974                                                                   EL300
01975  C01-LOOP-12.                                                     EL300
01976      ADD 1 TO X1 X2.                                              EL300
01977                                                                   EL300
01978      MOVE WK-2 (X2)              TO WK-1 (X1).                    EL300
01979                                                                   EL300
01980      IF X2 IS LESS THAN 8                                         EL300
01981          GO TO C01-LOOP-12.                                       EL300
01982                                                                   EL300
01983      MOVE WORK-1                 TO CTL-ALPH-DATE                 EL300
01984                                     T03-D                         EL300
01985                                     T13-D.                        EL300
01986                                                                   EL300
01987  C01-SKIP-8.                                                      EL300
01988      MOVE OT-CLIENT              TO T01-I                         EL300
01989                                     T11-I.                        EL300
01990                                                                   EL300
01991  C01-EXIT.                                                        EL300
01992      EXIT.                                                        EL300
01993                                                                   EL300
01994      EJECT                                                        EL300
01995 *                                                                 EL300
01996 *    CARD INPUT 02 EDIT - CLPS PROGRAM SWITCH OVERRIDE            EL300
01997 *                       - PRTO PRINT SWITCH OVERRIDE              EL300
01998 *                       - PRCO PROCESS SWITCH OVERRIDE            EL300
01999 *                       - FMTO FORMAT SWITCH OVERRIDE             EL300
02000 *                       - TOTO TOTAL SWITCH OVERRIDE              EL300
02001                                                                   EL300
02002 *                                                                 EL300
02003  C02-START.                                                       EL300
02004      MOVE ZERO                    TO X1.                          EL300
02005                                                                   EL300
02006  C02-LOOP-1.                                                      EL300
02007      ADD 1 TO X1.                                                 EL300
02008                                                                   EL300
02009      IF X1 IS GREATER THAN 12                                     EL300
02010          GO TO C02-TEST.                                          EL300
02011                                                                   EL300
02012      IF CLPS-SWITCH (X1) = SPACE                                  EL300
02013          GO TO C02-TEST.                                          EL300
02014                                                                   EL300
02015      MOVE CLPS-SWITCH (X1)       TO ERR-DETAIL.                   EL300
02016                                                                   EL300
02017      IF CLPS-PROG (X1) IS NOT NUMERIC                             EL300
02018          MOVE ZERO               TO CLPS-PROG (X1)                EL300
02019          MOVE 'X'                TO CLPS-SW (X1)                  EL300
02020          MOVE 92                 TO ERR-NBR                       EL300
02021          PERFORM LOAD-START THRU LOAD-EXIT.                       EL300
02022                                                                   EL300
02023      MOVE CLPS-SW (X1)           TO TEST-OPT.                     EL300
02024                                                                   EL300
02025      INSPECT TEST-OPT CONVERTING ALPH-TBL TO SPACE.               EL300
02026                                                                   EL300
02027      IF TEST-OPT = 'S' OR 'T'                                     EL300
02028          NEXT SENTENCE                                            EL300
02029       ELSE                                                        EL300
02030          IF TEST-OPT NOT = SPACE                                  EL300
02031             MOVE 'X'             TO CLPS-SW (X1)                  EL300
02032             MOVE 93              TO ERR-NBR                       EL300
02033             PERFORM LOAD-START THRU LOAD-EXIT.                    EL300
02034                                                                   EL300
02035      MOVE CLPS-SW (X1)           TO TEST-OPT.                     EL300
02036                                                                   EL300
02037      IF (TEST-OPT IS NOT NUMERIC) AND                             EL300
02038          (CLPS-ID = 'CLPS' OR 'PRCO') AND                         EL300
02039          (TEST-OPT NOT = 'X')                                     EL300
02040          MOVE 1                  TO FICHE-SW.                     EL300
02041                                                                   EL300
02042      IF CLPS-ID = 'PRTO'                                          EL300
02043          IF TEST-OPT = 'B' OR 'F'                                 EL300
02044             MOVE 1               TO FICHE-SW.                     EL300
02045                                                                   EL300
02046      INSPECT TEST-OPT REPLACING ALL ALPH-TBL BY NUM-TBL.          EL300
02047                                                                   EL300
02048      MOVE ZERO                   TO X2.                           EL300
02049                                                                   EL300
02050  C02-LOOP-2.                                                      EL300
02051      ADD 1 TO X2.                                                 EL300
02052                                                                   EL300
02053      IF (PROG-NM-TB (X2) = HIGH-VALUES) OR
02054         (X2 = +1502)                                              EL300
02055            MOVE 94               TO ERR-NBR                       EL300
02056            PERFORM LOAD-START THRU LOAD-EXIT                      EL300
02057            GO TO C02-LOOP-1.                                      EL300
02058                                                                   EL300
02059      IF PNT-PROG (X2) IS NOT NUMERIC                              EL300
02060          GO TO C02-LOOP-2.                                        EL300
02061                                                                   EL300
02062      IF PNT-PROG (X2) = CLPS-PROG (X1)                            EL300
02063          GO TO C02-LOOP-1.                                        EL300
02064                                                                   EL300
02065      GO TO C02-LOOP-2.                                            EL300
02066                                                                   EL300
02067  C02-TEST.                                                        EL300
02068      PERFORM ERROR-START THRU ERROR-EXIT.                         EL300
02069                                                                   EL300
02070      IF ERROR-SW = 1                                              EL300
02071          GO TO C02-EXIT.                                          EL300
02072                                                                   EL300
02073      MOVE ZERO TO X1.                                             EL300
02074                                                                   EL300
02075  C02-LOOP-4.                                                      EL300
02076      ADD 1 TO X1.                                                 EL300
02077                                                                   EL300
02078      IF CLPS-SWITCH (X1) = SPACE                                  EL300
02079          GO TO C02-SET-PRINT.                                     EL300
02080                                                                   EL300
02081      MOVE CLPS-PROG (X1)         TO X2.                           EL300
02082                                                                   EL300
02083      IF CLAS-ID = 'PRTO'                                          EL300
02084        GO TO C02-ADD-PRT-SW.                                      EL300
02085                                                                   EL300
02086      IF CLAS-ID = 'FMTO'                                          EL300
02087        GO TO C02-ADD-FMT-SW.                                      EL300
02088                                                                   EL300
02089      IF CLAS-ID = 'PRCO' OR 'CLPS'                                EL300
02090        GO TO C02-ADD-PRC-SW.                                      EL300
02091                                                                   EL300
02092      GO TO C02-ADD-TOT-SW.                                        EL300
02093                                                                   EL300
02094  C02-ADD-PRT-SW.                                                  EL300
02095      IF PROG-OR-PRT (X2) = 'O'                                    EL300
02096          MOVE CLPS-SWITCH (X1)   TO ERR-DETAIL                    EL300
02097          MOVE 96                 TO ERR-NBR                       EL300
02098          PERFORM LOAD-START THRU LOAD-EXIT                        EL300
02099          GO TO C02-EXIT.                                          EL300
02100                                                                   EL300
02101      MOVE CLPS-SW (X1)           TO PROG-TBL-PRT (X2).            EL300
02102      MOVE 'O'                    TO PROG-OR-PRT  (X2).            EL300
02103      GO TO C02-LOOP-4.                                            EL300
02104                                                                   EL300
02105  C02-ADD-FMT-SW.                                                  EL300
02106      IF PROG-OR-FMT (X2) = 'O'                                    EL300
02107          MOVE CLPS-SWITCH (X1)   TO ERR-DETAIL                    EL300
02108          MOVE 96                 TO ERR-NBR                       EL300
02109          PERFORM LOAD-START THRU LOAD-EXIT                        EL300
02110          GO TO C02-EXIT.                                          EL300
02111                                                                   EL300
02112      MOVE CLPS-SW (X1)           TO PROG-TBL-FMT (X2).            EL300
02113      MOVE 'O'                    TO PROG-OR-FMT  (X2).            EL300
02114      GO TO C02-LOOP-4.                                            EL300
02115                                                                   EL300
02116  C02-ADD-PRC-SW.                                                  EL300
02117      IF PROG-OR-PRC (X2) = 'O'                                    EL300
02118          MOVE CLPS-SWITCH (X1)   TO ERR-DETAIL                    EL300
02119          MOVE 96                 TO ERR-NBR                       EL300
02120          PERFORM LOAD-START THRU LOAD-EXIT                        EL300
02121          GO TO C02-EXIT.                                          EL300
02122                                                                   EL300
02123      MOVE CLPS-SW (X1)           TO PROG-TBL-PRC (X2).            EL300
02124      MOVE 'O'                    TO PROG-OR-PRC  (X2).            EL300
02125      GO TO C02-LOOP-4.                                            EL300
02126                                                                   EL300
02127  C02-ADD-TOT-SW.                                                  EL300
02128      IF PROG-OR-TOT (X2) = 'O'                                    EL300
02129          MOVE CLPS-SWITCH (X1)   TO ERR-DETAIL                    EL300
02130          MOVE 96                 TO ERR-NBR                       EL300
02131          PERFORM LOAD-START THRU LOAD-EXIT                        EL300
02132          GO TO C02-EXIT.                                          EL300
02133                                                                   EL300
02134      MOVE CLPS-SW (X1)           TO PROG-TBL-TOT (X2).            EL300
02135      MOVE 'O'                    TO PROG-OR-TOT  (X2).            EL300
02136      GO TO C02-LOOP-4.                                            EL300
02137                                                                   EL300
02138  C02-SET-PRINT.                                                   EL300
02139      PERFORM C02-TRANSFORM THRU C02-TRANS-EXIT                    EL300
02140        VARYING X1 FROM +1 BY +1 UNTIL X1 GREATER +1500.           EL300
02141                                                                   EL300
02142      GO TO C02-EXIT.                                              EL300
02143                                                                   EL300
02144  C02-TRANSFORM.                                                   EL300
02145      IF PROG-SYS-NAME (X1) NOT = 'EL' OR 'EC' OR 'GL'             EL300
02146          GO TO C02-TRANS-EXIT.                                    EL300
02147                                                                   EL300
02148      IF PROG-TBL (X1) NOT NUMERIC                                 EL300
02149          GO TO C02-TRANS-EXIT.                                    EL300
02150                                                                   EL300
02151      IF PROG-TBL-PRT (X1) = '9' OR SPACE                          EL300
02152          MOVE 'P'                TO PROG-TBL-PRT (X1).            EL300
02153                                                                   EL300
02154      IF PROG-TBL-PRC (X1) NUMERIC                                 EL300
02155          GO TO C02-TRANS-EXIT.                                    EL300
02156                                                                   EL300
02157      IF PROG-TBL-PRC (X1) NOT LESS 'A' AND                        EL300
02158         PROG-TBL-PRC (X1) NOT GREATER 'I'                         EL300
02159          MOVE 'F'                TO PROG-TBL-PRT (X1).            EL300
02160                                                                   EL300
02161      IF PROG-TBL-PRC (X1) NOT LESS 'J' AND                        EL300
02162         PROG-TBL-PRC (X1) NOT GREATER 'R'                         EL300
02163          MOVE 'B'                TO PROG-TBL-PRT (X1).            EL300
02164                                                                   EL300
02165      INSPECT PROG-TBL-PRC (X1) REPLACING ALL ALPH-TBL             EL300
02166          BY NUM-TBL.                                              EL300
02167                                                                   EL300
02168  C02-TRANS-EXIT.                                                  EL300
02169      EXIT.                                                        EL300
02170                                                                   EL300
02171                                                                   EL300
02172  C02-EXIT.                                                        EL300
02173      EXIT.                                                        EL300
02174                                                                   EL300
02175      EJECT                                                        EL300
02176 *                                                                 EL300
02177 *    CARD INPUT 03 EDIT - CLAF FACTOR CARD                        EL300
02178 *                                                                 EL300
02179  C03-START.                                                       EL300
02180      ADD 1 TO CLAF-CNT.                                           EL300
02181                                                                   EL300
02182      IF CLAF-CNT IS GREATER THAN 1                                EL300
02183          MOVE 1                  TO ERR-LIST (75)                 EL300
02184          GO TO C03-EXIT.                                          EL300
02185                                                                   EL300
02186      MOVE ZERO                   TO X1.                           EL300
02187                                                                   EL300
02188  C03-LOOP-1.                                                      EL300
02189      ADD 1 TO X1.                                                 EL300
02190                                                                   EL300
02191      IF X1 IS GREATER THAN 6                                      EL300
02192          GO TO C03-EXIT.                                          EL300
02193                                                                   EL300
02194      IF CLAF-FACTORS (X1) = SPACE                                 EL300
02195          GO TO C03-EXIT.                                          EL300
02196                                                                   EL300
02197      IF CLAF-FACTOR-X (X1) NUMERIC                                EL300
02198          MOVE CLAF-FACTOR-X (X1) TO FACT-FACTOR (X1)              EL300
02199          GO TO C03-LOOP-1.                                        EL300
02200                                                                   EL300
02201      MOVE CLAF-FACTOR-X (X1)     TO ERR-DETAIL.                   EL300
02202      MOVE 76                     TO ERR-NBR.                      EL300
02203                                                                   EL300
02204      PERFORM LOAD-START THRU LOAD-EXIT.                           EL300
02205                                                                   EL300
02206  C03-EXIT.                                                        EL300
02207      EXIT.                                                        EL300
02208                                                                   EL300
02209      EJECT                                                        EL300
02210 *                                                                 EL300
02211 * CARD INPUT 04 CLASS-IC COMPATIBILTY PARAMETERS                  EL300
02212 *                                                                 EL300
02213                                                                   EL300
02214  C04-START.                                                       EL300
02215      ADD 1 TO COLC-CNT.                                           EL300
020303     DISPLAY 'C04-START for colc card'.
02216                                                                   EL300
02217      IF COLC-CNT GREATER 1                                        EL300
02218          MOVE 1 TO ERR-LIST (03)                                  EL300
02219          GO TO C04-EXIT.                                          EL300
02220                                                                   EL300
02221      MOVE COLC-COMPANY-ID TO OT-CLIENT.                           EL300
02222                                                                   EL300
02223      IF OT-CLIENT = SPACES                                        EL300
02224          MOVE 1                  TO ERR-LIST (01).                EL300
02225                                                                   EL300
02226      IF COLC-DT-OVRIDE NOT = SPACES AND '1'                       EL300
02227          MOVE 1                  TO ERR-LIST (04)                 EL300
02228       ELSE                                                        EL300
02229          MOVE COLC-DT-OVRIDE     TO OLC-DT-OVRIDE-SW.             EL300
02230                                                                   EL300
02231  C04-READ-CLASIC-CO-REC.                                          EL300
02232      MOVE LOW-VALUES             TO CF-CONTROL-PRIMARY.           EL300
02233      MOVE OT-CLIENT              TO CF-COMPANY-ID.                EL300
02234                                                                   EL300
02235  C04-READ-MORTG-CONTROL.                                          EL300
02236                                                                   EL300
02237      MOVE 'N'                    TO CF-RECORD-TYPE.               EL300
02238      START ELCNTL KEY IS NOT LESS THAN CF-CONTROL-PRIMARY.        EL300
02239                                                                   EL300
02240      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL300
02241          MOVE 'ERROR OCCURED START - ELCNTL'  TO  WS-ABEND-MESSAGEEL300
02242          MOVE ELCNTL-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL300
02243          PERFORM ABEND-PGM.                                       EL300
02244                                                                   EL300
02245      READ ELCNTL NEXT                                             EL300
02246          AT END                                                   EL300
02247            MOVE 1                TO ERR-LIST (6)                  EL300
02248            GO TO C04-EXIT.                                        EL300
02249                                                                   EL300
02250      MOVE CF-MORTG-ACCESS-CONTROL                                 EL300
02251                                  TO OT-MORTG-ACCESS-CNTL.         EL300
02252      MOVE CF-MORTG-ALT-MORT-CODE TO OT-MP-ALT-MORT-CODE.          EL300
02253      MOVE CF-MP-REPORT-LANGUAGE-IND                               EL300
02254                                  TO OT-MP-REPORT-LANGUAGE-IND.    EL300
02255                                                                   EL300
02256  C04-READ-COMPANY-CONTROL.                                        EL300
02257                                                                   EL300
02258      MOVE 'ELCNTL'               TO VSAM-PGM.                     EL300
02259                                                                   EL300
02260      MOVE '1'                    TO CF-RECORD-TYPE.               EL300
02261                                                                   EL300
02262  C04-READ-NEXT-CONTROL.                                           EL300
02263                                                                   EL300
02264      READ ELCNTL.                                                 EL300
02265                                                                   EL300
02266      MOVE CF-COMPENSATION-MSTR-MAINT-DT  TO CLASIC-COMP-MAINT.    EL300
02267      MOVE CF-COMMISSION-TAB-MAINT-DT     TO CLASIC-CTBL-MAINT.    EL300
02268      MOVE CF-REINSURANCE-TAB-MAINT-DT    TO CLASIC-REIN-MAINT.    EL300
02269      MOVE CF-ACCOUNT-MSTR-MAINT-DT       TO CLASIC-ACCT-MAINT.    EL300
02270      MOVE CF-RATES-FILE-MAINT-DT         TO CLASIC-RATE-MAINT.    EL300
02271      MOVE CF-CURRENT-MONTH-END           TO CLASIC-CLAIMS-EOM-DT. EL300
02272      MOVE CF-CR-MONTH-END-DT             TO CLASIC-CREDIT-EOM-DT. EL300
02273      MOVE CF-MP-MONTH-END-DT             TO CLASIC-MORTG-EOM-DT.  EL300
02274      MOVE CF-AR-MONTH-END-DT             TO CLASIC-AR-EOM-DT.     EL300
02275                                                                   EL300
02276      MOVE CF-LIFE-OVERRIDE-L1       TO CLASIC-LIFE-OVERRIDE-L1.   EL300
02277      MOVE CF-LIFE-OVERRIDE-L2       TO CLASIC-LIFE-OVERRIDE-L2.   EL300
02278      MOVE CF-LIFE-OVERRIDE-L6       TO CLASIC-LIFE-OVERRIDE-L6.   EL300
02279      MOVE CF-LIFE-OVERRIDE-L12      TO CLASIC-LIFE-OVERRIDE-L12.  EL300
02280      MOVE CF-AH-OVERRIDE-L1         TO CLASIC-AH-OVERRIDE-L1.     EL300
02281      MOVE CF-AH-OVERRIDE-L2         TO CLASIC-AH-OVERRIDE-L2.     EL300
02282      MOVE CF-AH-OVERRIDE-L6         TO CLASIC-AH-OVERRIDE-L6.     EL300
02283      MOVE CF-AH-OVERRIDE-L12        TO CLASIC-AH-OVERRIDE-L12.    EL300
02284                                                                   EL300
02285      MOVE CF-REPORT-CD1-CAPTION     TO CLASIC-REPORT-CD1-CAPTION. EL300
02286      MOVE CF-REPORT-CD2-CAPTION     TO CLASIC-REPORT-CD2-CAPTION. EL300
02287                                                                   EL300
02288      MOVE ZERO                      TO FULL-BINARY-CD.            EL300
02289      MOVE CF-CL-MAIL-TO-NAME        TO COMPANY-LINE (1).          EL300
02290      MOVE CF-COMPANY-CD             TO COMPANY-CD-BYTE.           EL300
02291      MOVE CF-COMPANY-CD             TO CLASIC-COMPANY-CD.         EL300
02292      MOVE FULL-BINARY-CD            TO CLASIC-COMPANY-NUMBER.     EL300
02293      MOVE CF-CO-ST-CALL-RPT-CNTL    TO CLASIC-ST-CALL-DEFAULT.    EL300
02294      MOVE CF-CARRIER-CONTROL-LEVEL  TO W-CARRIER-SWITCH.          EL300
02295                                                                   EL300
02296      IF CF-CURRENT-MONTH-END = LOW-VALUE                          EL300
02297          MOVE ZEROS                      TO CLASIC-RUN-DATE       EL300
02298          GO TO C04-EXIT.                                          EL300
02299                                                                   EL300
02300      MOVE CF-CURRENT-MONTH-END           TO DC-BIN-DATE-1.        EL300
02301      MOVE SPACE                          TO DC-OPTION-CODE.       EL300
02302                                                                   EL300
02303      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL300
02304                                                                   EL300
02305      IF DATE-CONVERSION-ERROR                                     EL300
02306          MOVE 1                          TO ERR-LIST (11)         EL300
02307          GO TO C04-EXIT.                                          EL300
02308                                                                   EL300
02309      MOVE DC-GREG-DATE-CYMD              TO CLASIC-RUN-DATE.      EL300
02310                                                                   EL300
02311                                                                   EL300
02312   C04-EXIT.                                                       EL300
02313      EXIT.                                                        EL300
02314      EJECT                                                        EL300
02315 *                                                                 EL300
02316 * ELCNTL FILE - COMPANY-RECORD TRANSFER                           EL300
02317 *                                                                 EL300
02318  L01-START.                                                       EL300
02319                                                                   EL300
02320      MOVE SPACES                TO COMPANY-TBL.                   EL300
02321                                                                   EL300
02322      MOVE CF-CL-MAIL-TO-NAME    TO COMPANY-LINE (1).              EL300
02323      MOVE CF-CL-ADDR-LINE-1     TO COMPANY-LINE (2).              EL300
02324      MOVE CF-CL-ADDR-LINE-2     TO COMPANY-LINE (3).              EL300
02325      MOVE CF-CL-CITY-STATE      TO COMPANY-LINE (4).              EL300
02326                                                                   EL300
02327      MOVE SPACES                TO ZIP-TELE-LINE.                 EL300
02328                                                                   EL300
02329      IF CF-CL-ZIP-CODE-NUM NOT NUMERIC                            EL300
02330          MOVE ZEROS             TO CF-CL-ZIP-CODE-NUM.            EL300
02331      IF CF-CL-ZIP-CODE-NUM NOT = ZEROS                            EL300
02332          MOVE CF-CL-ZIP-CODE-NUM TO W-NINE-DIG-ZIP                EL300
02333      ELSE                                                         EL300
02334          MOVE CF-CL-ZIP-CODE     TO ZIP-CD-WORK.                  EL300
02335                                                                   EL300
02336      IF W-ZIP-DIG-1 NOT NUMERIC                                   EL300
02337          MOVE W-POSTAL-CODE-1    TO LINE5-POST-CODE1              EL300
02338          MOVE W-POSTAL-CODE-2    TO LINE5-POST-CODE2              EL300
02339      ELSE                                                         EL300
02340          IF W-CHECK-ZERO-A  =  '0000'  AND                        EL300
02341             W-ZIP-CD-A NOT  =  '00000'                            EL300
02342              MOVE W-ZIP-CD-A           TO LINE5-ZIP-FIVE          EL300
02343              IF W-CHECK-ZERO-A NOT = ZEROS                        EL300
02344                  MOVE '-'              TO LINE5-ZIP-DASH          EL300
02345                  MOVE W-CHECK-ZERO-A   TO LINE5-ZIP-FOUR          EL300
02346              ELSE                                                 EL300
02347                  NEXT SENTENCE                                    EL300
02348          ELSE                                                     EL300
02349              MOVE W-ZIP-CD-B           TO LINE5-ZIP-FIVE          EL300
02350              IF W-CHECK-ZERO-B NOT = ZEROS                        EL300
02351                  MOVE '-'              TO LINE5-ZIP-DASH          EL300
02352                  MOVE W-CHECK-ZERO-B   TO LINE5-ZIP-FOUR.         EL300
02353                                                                   EL300
02354      MOVE CF-CL-PHONE-NO        TO NUMERIC-TELE-NUMBER.           EL300
02355      MOVE TELE-AREA             TO TELE-AREA-E.                   EL300
02356      MOVE TELE-PREFIX           TO TELE-PREFIX-E.                 EL300
02357      MOVE TELE-NUMBER           TO TELE-NUMBER-E.                 EL300
02358      MOVE EDITED-TELE           TO LINE5-TELE.                    EL300
02359                                                                   EL300
02360      MOVE ZIP-TELE-LINE         TO COMPANY-LINE (5).              EL300
02361                                                                   EL300
02362      MOVE CF-TAX-ID-NUMBER      TO COMPANY-LINE (6).              EL300
02363                                                                   EL300
02364      MOVE +7                    TO CLAS-MAXC.                     EL300
02365      MOVE +1                    TO CLAS-STARTC.                   EL300
02366                                                                   EL300
02367  L01-SET-SYSTEMS.                                                 EL300
02368                                                                   EL300
02369      IF CO-HAS-CLAS-IC-CLAIM                                      EL300
02370          MOVE 'Y' TO OT-SYS-E-CLAS-IC-CLAIM.                      EL300
02371                                                                   EL300
02372      IF CO-HAS-CLAS-IC-CREDIT                                     EL300
02373          MOVE 'Y' TO OT-SYS-F-CLAS-IC-CREDIT                      EL300
02374                      OT-SYS-A-CREDIT.                             EL300
02375                                                                   EL300
02376      IF CONFIRMATION-SYS-USED                                     EL300
02377          MOVE '1' TO OT-SYS-C-CONFIRMATIONS.                      EL300
02378                                                                   EL300
02379      IF DAILY-BILL-SYS-USED                                       EL300
02380          MOVE '1' TO OT-DLY-BILL                                  EL300
02381          MOVE '1' TO OT-SYS-D-DEMAND-BILL.                        EL300
02382                                                                   EL300
02383      IF CF-AR-SYSTEM-USED                                         EL300
02384          MOVE '1' TO OT-SYS-G-AR-USED.                            EL300
02385                                                                   EL300
02386  L01-SET-OPTIONS.                                                 EL300
02387                                                                   EL300
02388      IF  CF-CO-RESERVE-OPTION-SWITCH NOT EQUAL 'Y'                EL300
02389          GO TO L01-SET-OPTIONS-CONT.                              EL300
02390                                                                   EL300
02391      DISPLAY ' '                                                  EL300
02392      DISPLAY 'THE FOLLOWING LIST CONTAINS THE PROBLEMS DETEC'     EL300
02393         'TED, THE DEFAULTS USED,'                                 EL300
02394      DISPLAY 'AND COMMENTS MADE BY EL300 WHILE SUPPORTING '       EL300
02395         'THE OPTIONAL METHOD OF '                                 EL300
02396      DISPLAY 'CALCULATING CLAIM RESERVES.'                        EL300
02397      DISPLAY ' '                                                  EL300
02398      MOVE CF-CO-RESERVE-OPTION-SWITCH                             EL300
02399                                  TO OT-RESERVE-OPTION-SWITCH.     EL300
02400      MOVE CF-CO-CRDB-TABLE-SELECTION                              EL300
02401                                  TO COMPANY-CRDB-TABLE-SELECTION. EL300
02402      MOVE CF-CO-OPTION-START-DATE                                 EL300
02403                                  TO COMPANY-OPTION-START-DATE.    EL300
02404                                                                   EL300
02405      IF  CF-CO-CIDA-TABLE-DISCOUNT-PCT NUMERIC                    EL300
02406              AND                                                  EL300
02407          CF-CO-CIDA-TABLE-DISCOUNT-PCT GREATER THAN ZEROS         EL300
02408          MOVE CF-CO-CIDA-TABLE-DISCOUNT-PCT                       EL300
02409                                  TO COMPANY-CIDA-DISCOUNT         EL300
02410      ELSE                                                         EL300
02411          MOVE +1.0               TO COMPANY-CIDA-DISCOUNT.        EL300
02412                                                                   EL300
02413      IF  CF-CO-CALCULATION-INTEREST NUMERIC                       EL300
02414              AND                                                  EL300
02415          CF-CO-CALCULATION-INTEREST GREATER THAN ZEROS            EL300
02416          MOVE CF-CO-CALCULATION-INTEREST                          EL300
02417                                  TO COMPANY-CALC-INTEREST         EL300
02418      ELSE                                                         EL300
02419          MOVE +.03               TO COMPANY-CALC-INTEREST         EL300
02420          DISPLAY 'COMPANY CALCULATION INTEREST HAS BEEN '         EL300
02421             'SET TO A DEFAULT OF 3 PERCENT.'.                     EL300
02422                                                                   EL300
02423      IF  CF-CO-IBNR-LAG-MONTHS NUMERIC                            EL300
02424              AND                                                  EL300
02425          CF-CO-IBNR-LAG-MONTHS GREATER THAN ZEROS                 EL300
02426          MOVE CF-CO-IBNR-LAG-MONTHS                               EL300
02427                                  TO COMPANY-IBNR-LAG-MONTHS       EL300
02428      ELSE                                                         EL300
02429          MOVE +3                 TO COMPANY-IBNR-LAG-MONTHS       EL300
02430          DISPLAY 'COMPANY IBNR LAG MONTHS HAS BEEN '              EL300
02431             'SET TO A DEFAULT OF 3.'.                             EL300
02432                                                                   EL300
02433      IF  CF-CO-IBNR-LIFE-FACTOR NUMERIC                           EL300
02434              AND                                                  EL300
02435          CF-CO-IBNR-LIFE-FACTOR GREATER THAN ZEROS                EL300
02436          MOVE CF-CO-IBNR-LIFE-FACTOR                              EL300
02437                                  TO COMPANY-IBNR-LIFE-FACTOR      EL300
02438      ELSE                                                         EL300
02439          MOVE +0.5               TO COMPANY-IBNR-LIFE-FACTOR      EL300
02440          DISPLAY 'COMPANY IBNR LIFE FACTOR HAS BEEN '             EL300
02441             'SET TO A DEFAULT OF 0.5.'.                           EL300
02442                                                                   EL300
02443      IF  CF-CO-IBNR-AH-FACTOR NUMERIC                             EL300
02444              AND                                                  EL300
02445          CF-CO-IBNR-AH-FACTOR GREATER THAN ZEROS                  EL300
02446          MOVE CF-CO-IBNR-AH-FACTOR                                EL300
02447                                  TO COMPANY-IBNR-AH-FACTOR        EL300
02448      ELSE                                                         EL300
02449          MOVE +0.5               TO COMPANY-IBNR-AH-FACTOR        EL300
02450          DISPLAY 'COMPANY IBNR LIFE FACTOR HAS BEEN '             EL300
02451             'SET TO A DEFAULT OF 0.5'.                            EL300
02452                                                                   EL300
02453  L01-SET-OPTIONS-CONT.                                            EL300
02454                                                                   EL300
02455      IF  CF-EXPERIENCE-RETENTION-AGE NUMERIC                      EL300
02456              AND                                                  EL300
02457          CF-EXPERIENCE-RETENTION-AGE GREATER THAN ZERO            EL300
02458          MOVE CF-EXPERIENCE-RETENTION-AGE                         EL300
02459                                  TO OT-EXPERIENCE-RETENT-AGE      EL300
02460      ELSE                                                         EL300
02461          MOVE 2                  TO OT-EXPERIENCE-RETENT-AGE.     EL300
02462                                                                   EL300
02463      MOVE CF-CLAIM-PAID-THRU-TO  TO OT-CLAIM-PAID-THRU-TO         EL300
02464      MOVE CF-CR-R78-METHOD       TO OT-R78.                       EL300
02465      MOVE CF-MORTALITY-AGE-CALC-METHOD                            EL300
02466                                  TO OT-MORTALITY-AGE-CALC-METHOD. EL300
02467      MOVE CF-CR-PR-METHOD        TO OT-PRORATA.                   EL300
02468      MOVE CF-CR-REM-TERM-CALC    TO OT-REM-TRM.                   EL300
02469      MOVE CF-REM-TRM-CALC-OPTION TO                               EL300
02470                               OT-REM-TRM-CALC-OPTION.             EL300
02471      MOVE CF-ALT-MORT-CODE       TO OT-ALT-MORT-CODE.             EL300
02472      MOVE CF-CERT-ACCESS-CONTROL TO OT-COMP-VG.                   EL300
02473      MOVE CF-CAR-GROUP-ACCESS-CNTL  TO  OT-COMPENSATION-ACCESS.   EL300
02474 *    MOVE CF-CLAIM-ACCESS-CONTROL TO CLASIC-CLAIM-ACCESS.         EL300
02475      MOVE CF-MIN-PREMIUM         TO   OT-MIN-PREM.                EL300
02476      MOVE CF-MIN-AGE             TO   OT-MIN-AGE.                 EL300
02477      MOVE CF-DEFAULT-AGE         TO   OT-DEFAULT-AGE.             EL300
02478      MOVE CF-DEFAULT-SEX         TO   OT-DEFAULT-SEX.             EL300
02479      MOVE CF-MAX-TERM            TO   OT-MAX-TERM.                EL300
02480                                                                   EL300
02481      MOVE CF-CO-TOL-CLAIM        TO   OT-CLM-CK.                  EL300
02482      MOVE CF-CO-TOL-PREM         TO   OT-PRM-CK.                  EL300
02483      MOVE CF-CO-TOL-REFUND       TO   OT-REF-CK.                  EL300
02484                                                                   EL300
02485      MOVE CF-CO-CLAIM-REJECT-SW  TO OT-CLM-REJ.                   EL300
02486      MOVE CF-CO-REF-REJECT-SW    TO OT-REF-REJ.                   EL300
02487                                                                   EL300
02488      IF  CF-COMP-WRITE-OFF-AMT NUMERIC                            EL300
02489          MOVE CF-COMP-WRITE-OFF-AMT                               EL300
02490                                  TO OT-WRT-OFF.                   EL300
02491                                                                   EL300
02492      MOVE CF-CONVERSION-DT       TO DC-BIN-DATE-1.                EL300
02493      MOVE SPACE                  TO DC-OPTION-CODE.               EL300
02494      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL300
02495      IF DATE-CONVERSION-ERROR                                     EL300
02496         MOVE ZERO                TO OT-CONV-DT                    EL300
02497                                     WS-OT-CONV-DTE                EL300
02498      ELSE                                                         EL300
02499         MOVE DC-GREG-DATE-CYMD   TO OT-CONV-DT                    EL300
02500                                     WS-OT-CONV-DTE.               EL300
02501                                                                   EL300
02502      IF CF-JOINT-AGE-IS-INPUT                                     EL300
02503          MOVE '1'                TO OT-JT-AGE                     EL300
02504       ELSE                                                        EL300
02505          MOVE ' '                TO OT-JT-AGE.                    EL300
02506      IF CF-BIRTH-DATE-IS-INPUT                                    EL300
02507          MOVE '1'                TO OT-KEY-BIRTH                  EL300
02508       ELSE                                                        EL300
02509          MOVE ' '                TO OT-KEY-BIRTH.                 EL300
02510      IF REIN-TABLES-ARE-USED                                      EL300
02511          MOVE '1'                TO OT-REINSURANCE                EL300
02512       ELSE                                                        EL300
02513          MOVE ' '                TO OT-REINSURANCE.               EL300
02514      IF COMP-TABLES-ARE-USED                                      EL300
02515          MOVE ' '                TO OT-COM-TBL-USED               EL300
02516        ELSE                                                       EL300
02517          MOVE '1'                TO OT-COM-TBL-USED.              EL300
02518      IF CO-IS-PROCESSED-ON-QTR                                    EL300
02519          MOVE '1' TO OT-QTR-CO                                    EL300
02520        ELSE                                                       EL300
02521          MOVE ' ' TO OT-QTR-CO.                                   EL300
02522                                                                   EL300
02523      EJECT                                                        EL300
02524 *                                                                 EL300
02525 * ELCNTL  -  BENEFIT RECORD TRANSFER                              EL300
02526 *                                                                 EL300
02527  L01-SET-BENEFIT-TYPES.                                           EL300
02528                                                                   EL300
02529      MOVE SPACES                 TO INSURANCE-TBL.                EL300
02530                                                                   EL300
02531      MOVE SPACES                 TO CF-CONTROL-PRIMARY.           EL300
02532      MOVE ZERO                   TO CF-SEQUENCE-NO                EL300
02533                                     CLAS-STARTL                   EL300
02534                                     CLAS-STARTA                   EL300
02535                                     CLAS-MAXL                     EL300
02536                                     CLAS-MAXA                     EL300
02537                                     OPTION-CNT.                   EL300
02538      MOVE OT-CLIENT              TO CF-COMPANY-ID.                EL300
02539      MOVE '4'                    TO CF-RECORD-TYPE.               EL300
02540                                                                   EL300
02541      START ELCNTL KEY IS NOT LESS THAN CF-CONTROL-PRIMARY         EL300
02542                                                                   EL300
02543                                                                   EL300
02544      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL300
02545          MOVE 'ERROR OCCURED START - ELCNTL'  TO  WS-ABEND-MESSAGEEL300
02546          MOVE ELCNTL-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL300
02547          PERFORM ABEND-PGM.                                       EL300
02548                                                                   EL300
02549  L01-BENEFIT-LOOP-1.                                              EL300
02550                                                                   EL300
02551      READ ELCNTL  NEXT RECORD                                     EL300
02552          AT END   GO TO L01-BENEFIT-END.                          EL300
02553                                                                   EL300
02554      MOVE ZERO TO X1.                                             EL300
02555                                                                   EL300
02556      IF CF-RECORD-TYPE NOT = '4' AND '5'                          EL300
02557          GO TO L01-BENEFIT-END.                                   EL300
02558                                                                   EL300
02559      IF (CF-RECORD-TYPE = '5') AND (CLAS-STARTA = ZERO)           EL300
02560          MOVE OPTION-CNT         TO CLAS-MAXL                     EL300
02561          COMPUTE CLAS-STARTA = CLAS-MAXL + +1.                    EL300
02562                                                                   EL300
02563  L01-BENEFIT-LOOP-2.                                              EL300
02564                                                                   EL300
02565      ADD 1 TO X1.                                                 EL300
02566                                                                   EL300
02567      IF X1 GREATER THAN 8                                         EL300
02568          GO TO L01-BENEFIT-LOOP-1.                                EL300
02569                                                                   EL300
02570      IF CF-BENEFIT-CODE (X1) = ZEROS or spaces                    EL300
02571          GO TO L01-BENEFIT-LOOP-2.                                EL300
02572                                                                   EL300
CIDMOD*    OPTION-CNT REPRESENTS THE TOTAL LIFE AND A/H BENEFITS
CIDMOD*      IE YOU CAN HAVE NO MORE THAN 900 LIFE AND A/H
CIDMOD*      BENEFIT CODES
CIDMOD*
CIDMOD*    IF OPTION-CNT LESS THAN 300                                  EL300
092602*    IF OPTION-CNT LESS THAN 600                                  EL300
092602     IF OPTION-CNT LESS THAN 900                                  EL300
02574          ADD 1 TO OPTION-CNT                                      EL300
02575      ELSE                                                         EL300
02576          MOVE 1                 TO ERR-LIST (12)                  EL300
02577          GO TO L01-BENEFIT-END.                                   EL300
02578                                                                   EL300
02579      MOVE CF-BENEFIT-CODE (X1)     TO INS-BEN (OPTION-CNT).       EL300
02580      MOVE CF-BENEFIT-ALPHA (X1)    TO INS-AB3 (OPTION-CNT).       EL300
02581      MOVE CF-BENEFIT-DESCRIP (X1)  TO INS-AB10 (OPTION-CNT).      EL300
02582      MOVE CF-BENEFIT-COMMENT (X1)  TO INS-COMMENT (OPTION-CNT).   EL300
02583      MOVE CF-LF-COVERAGE-TYPE (X1) TO INS-RLA (OPTION-CNT).       EL300
02584      MOVE CF-CO-BEN-I-G-CD  (X1) TO INS-BEN-I-G-CD (OPTION-CNT).  EL300
02585      MOVE CF-CO-REM-TERM-CALC (X1)                                EL300
02586          TO INS-REM-TERM-CALC (OPTION-CNT).                       EL300
02587                                                                   EL300
02588      IF CF-RECORD-TYPE = '5'                                      EL300
02589          MOVE 'A' TO INS-RLA (OPTION-CNT).                        EL300
02590                                                                   EL300
02591      MOVE CF-JOINT-INDICATOR (X1)  TO INS-JOINT (OPTION-CNT).     EL300
02592      MOVE CF-SPECIAL-CALC-CD (X1)  TO INS-CALC-TYPE (OPTION-CNT). EL300
02593      IF INS-CALC-TYPE (OPTION-CNT) = 'O'                          EL300
02594          MOVE 'B' TO INS-CALC-TYPE (OPTION-CNT).                  EL300
02595                                                                   EL300
02596      IF CF-CO-EARNINGS-CALC (X1) = '1'                            EL300
02597          MOVE 'R'                  TO INS-EP (OPTION-CNT).        EL300
02598                                                                   EL300
02599      IF CF-CO-EARNINGS-CALC (X1) = '2'                            EL300
02600          MOVE 'P'                  TO INS-EP (OPTION-CNT).        EL300
02601                                                                   EL300
02602      IF CF-CO-EARNINGS-CALC (X1) = '3'                            EL300
02603          MOVE 'C'                  TO INS-EP (OPTION-CNT).        EL300
02604                                                                   EL300
02605      IF CF-CO-EARNINGS-CALC (X1) = '4'                            EL300
02606          MOVE 'T'                  TO INS-EP (OPTION-CNT).        EL300
02607                                                                   EL300
02608      IF CF-CO-EARNINGS-CALC (X1) = '5'                            EL300
02609          MOVE 'N'                  TO INS-EP (OPTION-CNT).        EL300
02610                                                                   EL300
02611      IF CF-CO-EARNINGS-CALC (X1) = '6'                            EL300
02612          MOVE 'A'                  TO INS-EP (OPTION-CNT).        EL300
02613                                                                   EL300
02614      IF CF-CO-EARNINGS-CALC (X1) = '8'                            EL300
02615          MOVE 'M'                  TO INS-EP (OPTION-CNT).        EL300
02616                                                                   EL300
02617      IF CF-CO-EARNINGS-CALC (X1) = 'B'                            EL300
02618          MOVE 'B'                  TO INS-EP (OPTION-CNT).        EL300
02619                                                                   EL300
02620      GO TO L01-BENEFIT-LOOP-2.                                    EL300
02621                                                                   EL300
02622  L01-BENEFIT-END.                                                 EL300
02623                                                                   EL300
02624      IF CLAS-STARTA NOT = ZERO                                    EL300
02625          MOVE OPTION-CNT         TO CLAS-MAXA                     EL300
02626       ELSE                                                        EL300
02627          MOVE ZERO               TO CLAS-STARTA CLAS-MAXA         EL300
02628          MOVE OPTION-CNT         TO CLAS-MAXL.                    EL300
02629                                                                   EL300
02630      IF CLAS-MAXL NOT = ZERO                                      EL300
02631          MOVE +1                 TO CLAS-STARTL                   EL300
02632       ELSE                                                        EL300
02633          MOVE ZERO               TO CLAS-STARTL CLAS-MAXL.        EL300
02634                                                                   EL300
02635                                                                   EL300
02636      EJECT                                                        EL300
02637 *                                                                 EL300
02638 * ELCNTL  -  MORTALITY TABLES BUILD                               EL300
02639 *                                                                 EL300
02640  L01-CREATE-MORT-TABLE.                                           EL300
02641                                                                   EL300
02642      MOVE SPACES                 TO MORTALITY-TBL.                EL300
02643                                                                   EL300
02644      MOVE SPACES                 TO CF-CONTROL-PRIMARY.           EL300
02645      MOVE ZERO                   TO CF-SEQUENCE-NO.               EL300
02646      MOVE OT-CLIENT              TO CF-COMPANY-ID.                EL300
02647      MOVE '7'                    TO CF-RECORD-TYPE.               EL300
02648      MOVE +1                     TO CLAS-STARTM.                  EL300
02649      MOVE ZERO                   TO CLAS-MAXM.                    EL300
02650                                                                   EL300
02651      START ELCNTL KEY IS NOT LESS THAN CF-CONTROL-PRIMARY         EL300
02652                                                                   EL300
02653      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL300
02654          GO TO L01-LF-MORT-END.                                   EL300
02655                                                                   EL300
02656  L01-MORT-BLD-LOOP-1.                                             EL300
02657                                                                   EL300
02658      READ ELCNTL  NEXT RECORD                                     EL300
02659          AT END   GO TO L01-LF-MORT-END.                          EL300
02660                                                                   EL300
02661      MOVE ZERO                   TO X1.                           EL300
02662      IF CF-RECORD-TYPE NOT = '7'                                  EL300
02663          GO TO L01-LF-MORT-END.                                   EL300
02664                                                                   EL300
02665  L01-MORT-BLD-LOOP-2.                                             EL300
02666                                                                   EL300
02667      IF CF-MORT-INTEREST (1) NOT NUMERIC                          EL300
02668          DISPLAY 'MORTALITY TABLES NEED CONVERSION'               EL300
02669          GO TO L01-LF-MORT-END.                                   EL300
02670                                                                   EL300
02671      ADD 1 TO X1.                                                 EL300
02672      IF X1 GREATER +9                                             EL300
02673          GO TO L01-MORT-BLD-LOOP-1.                               EL300
02674                                                                   EL300
02675      IF CF-MORT-TABLE-CODE (X1) EQUAL SPACES OR LOW-VALUES        EL300
02676          GO TO L01-MORT-BLD-LOOP-2.                               EL300
02677                                                                   EL300
02678      ADD +1 TO CLAS-MAXM.                                         EL300
02679      MOVE CF-MORT-TABLE-CODE (X1)                                 EL300
02680                                  TO M-CODE (CLAS-MAXM).           EL300
02681                                                                   EL300
02682      MOVE CF-MORT-TABLE-TYPE (X1)                                 EL300
02683                                  TO M-J-CODE (CLAS-MAXM).         EL300
02684      MOVE CF-MORT-JOINT-FACTOR (X1)                               EL300
02685                                  TO M-J-FACT (CLAS-MAXM).         EL300
02686                                                                   EL300
02687      COMPUTE M-INTEREST                                           EL300
02688          = CF-MORT-INTEREST (X1) * 100.                           EL300
02689      COMPUTE M-RESERVE-ADJ                                        EL300
02690          = CF-MORT-RESERVE-ADJUSTMENT (X1) * 100                  EL300
02691      MOVE CF-MORT-AGE-METHOD (X1)                                 EL300
02692                                  TO M-AGE-METHOD.                 EL300
02693                                                                   EL300
02694      IF  CF-MORT-TABLE (X1) EQUAL 'XXXXX'                         EL300
02695              AND                                                  EL300
02696          CF-MORT-COMMENTS (X1) GREATER THAN SPACES                EL300
02697          MOVE CF-MORT-COMMENTS (X1)                               EL300
02698                                  TO W-TABLE-DIVIDE                EL300
02699      ELSE                                                         EL300
02700          MOVE CF-MORT-TABLE (X1) TO W-TABLE-DIVIDE.               EL300
02701                                                                   EL300
LGXMOD*    MOVE W-TABLE-YEAR-N          TO DC-ALPHA-YEAR.               EL300
LGXMOD*                                                                 EL300
LGXMOD*    MOVE '7'                     TO DC-OPTION-CODE.              EL300
LGXMOD*    PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL300
LGXMOD*    IF DATE-CONVERSION-ERROR                                     EL300
LGXMOD*       IF ONLY-CENTURY                                           EL300
LGXMOD*          MOVE DC-ALPHA-CENTURY  TO W-TBL-DECADE                 EL300
LGXMOD*       ELSE                                                      EL300
LGXMOD*          MOVE 1                 TO ERR-LIST (11)                EL300
LGXMOD*          GO TO L01-LF-MORT-END.                                 EL300
LGXMOD     IF W-TABLE-YEAR > '40'
LGXMOD        MOVE '19'                 TO W-TBL-DECADE
LGXMOD     ELSE
LGXMOD        MOVE '20'                 TO W-TBL-DECADE
LGXMOD     END-IF
02712                                                                   EL300
02713      MOVE W-TABLE-YEAR            TO W-TBL-YEAR.                  EL300
02714      MOVE W-FULL-YEAR             TO M-YEAR.                      EL300
02715                                                                   EL300
02716      IF  OT-CLIENT EQUAL 'HAN'                                    EL300
02717              AND                                                  EL300
02718          W-TABLE-TYPE EQUAL 'FSO'                                 EL300
02719              OR                                                   EL300
02720          W-TABLE-TYPE EQUAL 'MSO'                                 EL300
02721          MOVE 'CSO'              TO M-TABLE-TYPE                  EL300
02722      ELSE                                                         EL300
02723          MOVE W-TABLE-TYPE       TO M-TABLE-TYPE.                 EL300
02724                                                                   EL300
02725      MOVE MORTALITY-DESC         TO M-DESC (CLAS-MAXM).           EL300
02726                                                                   EL300
02727      IF CLAS-MAXM  GREATER 99                                     EL300
02728          MOVE 1                  TO ERR-LIST (058)                EL300
02729          GO TO L01-LF-MORT-END.                                   EL300
02730                                                                   EL300
02731      GO TO L01-MORT-BLD-LOOP-2.                                   EL300
02732                                                                   EL300
02733  L01-LF-MORT-END.                                                 EL300
02734                                                                   EL300
02735      IF CLAS-MAXM = ZERO                                          EL300
02736         MOVE ZERO                TO CLAS-STARTM.                  EL300
02737                                                                   EL300
02738      EJECT                                                        EL300
02739 *                                                                 EL300
02740 * ELCNTL  -  BUSINESS TYPES TRANSFER                              EL300
02741 *                                                                 EL300
02742  L01-CREATE-BUS-TYPE-TABLE.                                       EL300
02743                                                                   EL300
02744      MOVE SPACES                 TO BUSINESS-TBL.                 EL300
02745                                                                   EL300
02746      MOVE LOW-VALUE              TO CF-CONTROL-PRIMARY.           EL300
02747      MOVE ZERO                   TO CF-SEQUENCE-NO.               EL300
02748      MOVE OT-CLIENT              TO CF-COMPANY-ID.                EL300
02749      MOVE '8'                    TO CF-RECORD-TYPE.               EL300
02750      MOVE +1                     TO CLAS-STARTB.                  EL300
02751      MOVE ZERO                   TO CLAS-MAXB                     EL300
02752                                     X2                            EL300
02753                                     INDEXBS.                      EL300
02754                                                                   EL300
02755      START ELCNTL KEY IS NOT LESS THAN CF-CONTROL-PRIMARY         EL300
02756                                                                   EL300
02757      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL300
02758          GO TO L01-BUS-TAB-END.                                   EL300
02759                                                                   EL300
02760                                                                   EL300
02761  L01-BUS-TABLE-LOOP-1.                                            EL300
02762                                                                   EL300
02763      READ ELCNTL  NEXT RECORD                                     EL300
02764          AT END   GO TO L01-BUS-TAB-END.                          EL300
02765                                                                   EL300
02766      MOVE ZERO                   TO X1.                           EL300
02767                                                                   EL300
02768      IF CF-RECORD-TYPE NOT = '8'                                  EL300
02769          GO TO L01-BUS-TAB-END.                                   EL300
02770                                                                   EL300
02771  L01-BUS-TABLE-LOOP-2.                                            EL300
02772                                                                   EL300
02773      ADD +1 TO X1.                                                EL300
02774                                                                   EL300
02775      IF X1 GREATER +20                                            EL300
02776          GO TO L01-BUS-TABLE-LOOP-1.                              EL300
02777                                                                   EL300
02778      ADD +1 TO X2.                                                EL300
02779                                                                   EL300
02780      IF  OT-OPT-RESERVE-METHOD-AUTH                               EL300
02781          ADD +1                  TO INDEXBS                       EL300
02782          IF  CF-BUS-MOD-ST-TRGT-LOSS-RATIO (X1) NOT NUMERIC       EL300
02783              DISPLAY 'BUS TLR NOT NUMERIC - ' X1                  EL300
02784                  ' ' INDEXBS                                      EL300
02785              MOVE +1                                              EL300
02786                  TO BUS-TRGT-LOSS-RATIO-MOD (INDEXBS)             EL300
02787          ELSE                                                     EL300
02788              MOVE CF-BUS-MOD-ST-TRGT-LOSS-RATIO (X1)              EL300
02789                  TO BUS-TRGT-LOSS-RATIO-MOD (INDEXBS).            EL300
02790                                                                   EL300
02791      IF CF-BUSINESS-TITLE (X1) = SPACES                           EL300
02792          GO TO L01-BUS-TABLE-LOOP-2.                              EL300
02793                                                                   EL300
02794      ADD +1 TO CLAS-MAXB.                                         EL300
02795      MOVE CF-BUSINESS-TITLE (X1)   TO B-DESC (CLAS-MAXB).         EL300
02796      MOVE X2                       TO B-CODE (CLAS-MAXB).         EL300
02797      MOVE CF-BUS-EXCL-ST-CALL (X1) TO B-EXCL-TYPE (CLAS-MAXB).    EL300
02798                                                                   EL300
02799      IF CLAS-MAXB  GREATER 50                                     EL300
02800          MOVE 1                  TO ERR-LIST (064)                EL300
02801          GO TO L01-BUS-TAB-END.                                   EL300
02802                                                                   EL300
02803      GO TO L01-BUS-TABLE-LOOP-2.                                  EL300
02804                                                                   EL300
02805  L01-BUS-TAB-END.                                                 EL300
02806      IF CLAS-MAXB = ZERO                                          EL300
02807         MOVE ZERO TO CLAS-STARTB.                                 EL300
02808                                                                   EL300
02809      EJECT                                                        EL300
02810 *                                                                 EL300
02811 *  ELCNTL - STATE RECORD TRANSFER                                 EL300
02812 *                                                                 EL300
02813  L01-STATE-NAMES.                                                 EL300
02814                                                                   EL300
02815      MOVE ZERO                   TO X1.                           EL300
02816                                                                   EL300
02817      MOVE SPACES                 TO CF-CONTROL-PRIMARY.           EL300
02818      MOVE ZERO                   TO CF-SEQUENCE-NO.               EL300
02819      MOVE OT-CLIENT              TO CF-COMPANY-ID.                EL300
02820      MOVE '3'                    TO CF-RECORD-TYPE.               EL300
02821                                                                   EL300
02822      START ELCNTL  KEY IS NOT LESS THAN CF-CONTROL-PRIMARY        EL300
02823                                                                   EL300
02824      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL300
02825          MOVE 'ERROR OCCURED START - ELCNTL'  TO  WS-ABEND-MESSAGEEL300
02826          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL300
02827          PERFORM ABEND-PGM.                                       EL300
02828                                                                   EL300
02829                                                                   EL300
02830      MOVE SPACES TO STATE-TBL                                     EL300
02831                     STATE-TARGET-LOSS-RATIOS.                     EL300
02832                                                                   EL300
02833  L01-STATE-LOOP-1.                                                EL300
02834                                                                   EL300
02835      READ ELCNTL NEXT RECORD                                      EL300
02836          AT END  MOVE X1         TO CLAS-MAXS                     EL300
02837                  GO TO L01-STATE-END.                             EL300
02838                                                                   EL300
02839      IF CF-RECORD-TYPE NOT = '3'                                  EL300
02840          MOVE X1                 TO CLAS-MAXS                     EL300
02841          GO TO L01-STATE-END.                                     EL300
02842                                                                   EL300
02843      ADD +1 TO X1.                                                EL300
02844                                                                   EL300
02845      IF X1 GREATER +75                                            EL300
02846           MOVE 1                 TO ERR-LIST (13)                 EL300
02847           GO TO L01-STATE-END.                                    EL300
02848                                                                   EL300
02849      MOVE CF-STATE-CODE          TO S-CODE (X1).                  EL300
02850      MOVE CF-STATE-ABBREVIATION  TO S-ABBR (X1).                  EL300
02851      MOVE CF-STATE-NAME          TO S-DESC (X1).                  EL300
02852      MOVE CF-ST-CALL-UNEARNED    TO S-UNER (X1).                  EL300
02853                                                                   EL300
02854      IF CF-ST-CALL-RPT-CNTL EQUAL SPACES OR LOW-VALUES            EL300
02855          MOVE CLASIC-ST-CALL-DEFAULT TO S-REPT (X1)               EL300
02856      ELSE                                                         EL300
02857          MOVE CF-ST-CALL-RPT-CNTL    TO S-REPT (X1).              EL300
02858                                                                   EL300
02859      MOVE CF-ST-CALL-RATE-DEV    TO S-RATE (X1).                  EL300
02860                                                                   EL300
02861      IF  OT-OPT-RESERVE-METHOD-UNAUTH                             EL300
02862          GO TO L01-STATE-LOOP-1.                                  EL300
02863                                                                   EL300
02864      IF  CF-ST-TARGET-LOSS-RATIO NUMERIC                          EL300
02865          MOVE CF-ST-TARGET-LOSS-RATIO                             EL300
02866                                  TO STATE-TARGET-LOSS-RATIO (X1)  EL300
02867      ELSE                                                         EL300
02868          DISPLAY 'STATE TLR NOT NUMERIC - ' X1 ' FOR '            EL300
02869              CF-STATE-CODE.                                       EL300
02870                                                                   EL300
02871      IF  CF-ST-CALC-INTEREST NUMERIC                              EL300
02872              AND                                                  EL300
02873          CF-ST-CALC-INTEREST GREATER THAN ZEROS                   EL300
02874          MOVE CF-ST-CALC-INTEREST                                 EL300
02875                                  TO STATE-CALC-INTEREST (X1)      EL300
02876      ELSE                                                         EL300
02877          MOVE COMPANY-CALC-INTEREST                               EL300
02878                                  TO STATE-CALC-INTEREST (X1)      EL300
02879          DISPLAY 'STATE CALC INT NOT VALID - ' X1 ' FOR '         EL300
02880              CF-STATE-CODE ' COMPANY VALUE USED.'.                EL300
02881                                                                   EL300
02882      GO TO L01-STATE-LOOP-1.                                      EL300
02883                                                                   EL300
02884  L01-STATE-END.                                                   EL300
02885      IF CLAS-MAXS NOT = ZERO                                      EL300
02886         MOVE +1                  TO CLAS-STARTS.                  EL300
02887      EJECT                                                        EL300
02888 *                                                                 EL300
02889 *  ELCNTL - CARRIER RECORD TRANSFER                               EL300
02890 *                                                                 EL300
02891  L01-CARR-NAMES.                                                  EL300
02892                                                                   EL300
02893      MOVE ZERO                   TO X1.                           EL300
02894                                                                   EL300
02895      MOVE SPACES                 TO CF-CONTROL-PRIMARY.           EL300
02896      MOVE ZERO                   TO CF-SEQUENCE-NO.               EL300
02897      MOVE OT-CLIENT              TO CF-COMPANY-ID.                EL300
02898      MOVE '6'                    TO CF-RECORD-TYPE.               EL300
02899                                                                   EL300
02900      START ELCNTL  KEY IS NOT LESS THAN CF-CONTROL-PRIMARY        EL300
02901                                                                   EL300
02902      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL300
02903          MOVE 'ERROR OCCURED START - ELCNTL'  TO  WS-ABEND-MESSAGEEL300
02904          MOVE ELCNTL-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL300
02905          PERFORM ABEND-PGM.                                       EL300
02906                                                                   EL300
02907                                                                   EL300
02908      MOVE SPACES                 TO CARRIER-NAME-TBL.             EL300
02909                                                                   EL300
02910  L01-CARR-LOOP-1.                                                 EL300
02911                                                                   EL300
02912      READ ELCNTL NEXT RECORD                                      EL300
02913          AT END  MOVE X1         TO CLAS-MAXCN                    EL300
02914                  GO TO L01-CARR-END.                              EL300
02915                                                                   EL300
02916      IF CF-RECORD-TYPE NOT = '6'                                  EL300
02917          MOVE X1                 TO CLAS-MAXCN                    EL300
02918          GO TO L01-CARR-END.                                      EL300
02919                                                                   EL300
02920      ADD +1 TO X1.                                                EL300
02921                                                                   EL300
02922      IF X1 GREATER +25                                            EL300
02923          MOVE 1                  TO ERR-LIST (14)                 EL300
02924          GO TO L01-EXIT.                                          EL300
02925                                                                   EL300
02926      MOVE CF-CARRIER-CNTL        TO CN-CODE (X1).                 EL300
02927      MOVE CF-DOMICILE-STATE      TO CN-DOM-ST (X1).               EL300
02928      MOVE CF-MAIL-TO-NAME        TO CN-NAME (X1).                 EL300
02929                                                                   EL300
02930      MOVE ZEROS                  TO CN-IBNR-UEP-PCT (X1)          EL300
02931                                     CN-IBNR-R78-PCT (X1)          EL300
02932                                     CN-IBNR-PRO-PCT (X1).         EL300
02933                                                                   EL300
02934      IF CF-IBNR-UEPRM-PERCENT NUMERIC                             EL300
02935          MOVE CF-IBNR-UEPRM-PERCENT TO CN-IBNR-UEP-PCT (X1).      EL300
02936      IF CF-IBNR-R78-PERCENT NUMERIC                               EL300
02937          MOVE CF-IBNR-R78-PERCENT   TO CN-IBNR-R78-PCT (X1).      EL300
02938      IF CF-IBNR-PRO-PERCENT NUMERIC                               EL300
02939          MOVE CF-IBNR-PRO-PERCENT   TO CN-IBNR-PRO-PCT (X1).      EL300
02940                                                                   EL300
02941                                                                   EL300
02942      IF  OT-OPT-RESERVE-METHOD-UNAUTH                             EL300
02943          GO TO L01-CARR-LOOP-1.                                   EL300
02944                                                                   EL300
02945      IF  USE-GIVEN-CARRIER                                        EL300
02946          PERFORM L01-CARR-GET-OPT-DATA THRU L01-CARROPT-EXIT      EL300
02947          GO TO L01-CARR-LOOP-1                                    EL300
02948      ELSE                                                         EL300
02949          IF  CF-CARRIER-CNTL EQUAL W-CARRIER-SWITCH               EL300
02950              PERFORM L01-CARR-GET-OPT-DATA THRU L01-CARROPT-EXIT  EL300
02951              MOVE CARR-IBNR-SW (X1)                               EL300
02952                                  TO W-CNTL-IBNR-SW                EL300
02953              MOVE CARR-IBNR-PERCENT (X1)                          EL300
02954                                  TO W-CNTL-IBNR-PERCENT           EL300
02955              MOVE CARR-PERCENT-OF-CIDA (X1)                       EL300
02956                                  TO W-CNTL-PERCENT-OF-CIDA        EL300
02957              PERFORM L01-CARR-REPLACE-PASSED-DATA                 EL300
02958                  THRU L01-CARRREP-EXIT                            EL300
02959                      VARYING                                      EL300
02960                  RX1 FROM 1 BY 1                                  EL300
02961                      UNTIL                                        EL300
02962                  RX1 EQUAL X1                                     EL300
02963              GO TO L01-CARR-LOOP-1                                EL300
02964          ELSE                                                     EL300
02965              MOVE W-CNTL-IBNR-SW TO CARR-IBNR-SW (X1)             EL300
02966              MOVE W-CNTL-IBNR-PERCENT                             EL300
02967                                  TO CARR-IBNR-PERCENT (X1)        EL300
02968              MOVE W-CNTL-PERCENT-OF-CIDA                          EL300
02969                                  TO CARR-PERCENT-OF-CIDA (X1)     EL300
02970              GO TO L01-CARR-LOOP-1.                               EL300
02971                                                                   EL300
02972  L01-CARR-GET-OPT-DATA.                                           EL300
02973                                                                   EL300
02974      MOVE CF-IBNR-SW             TO CARR-IBNR-SW (X1).            EL300
02975                                                                   EL300
02976      IF  CF-PERCENT-OF-CDT NUMERIC                                EL300
02977          COMPUTE CARR-PERCENT-OF-CIDA (X1)                        EL300
02978              = CF-PERCENT-OF-CDT * .01                            EL300
02979      ELSE                                                         EL300
02980          DISPLAY 'PERCENT OF CDT NOT NUMERIC - '                  EL300
02981              X1 ' FOR CARRIER ' CF-CARRIER-CNTL                   EL300
02982          MOVE +0.0000            TO CARR-PERCENT-OF-CIDA (X1).    EL300
02983                                                                   EL300
02984      IF  CF-IBNR-PERCENT NUMERIC                                  EL300
02985          MOVE CF-IBNR-PERCENT    TO CARR-IBNR-PERCENT (X1)        EL300
02986      ELSE                                                         EL300
02987          DISPLAY 'IBNR PERCENT NOT NUMERIC - '                    EL300
02988              X1 ' FOR CARRIER ' CF-CARRIER-CNTL                   EL300
02989          MOVE +1.0000            TO CARR-IBNR-PERCENT (X1).       EL300
02990                                                                   EL300
02991  L01-CARROPT-EXIT.                                                EL300
02992      EXIT.                                                        EL300
02993                                                                   EL300
02994  L01-CARR-REPLACE-PASSED-DATA.                                    EL300
02995                                                                   EL300
02996      MOVE W-CNTL-IBNR-SW         TO CARR-IBNR-SW (RX1).           EL300
02997      MOVE W-CNTL-PERCENT-OF-CIDA TO CARR-PERCENT-OF-CIDA (RX1).   EL300
02998      MOVE W-CNTL-IBNR-PERCENT    TO CARR-IBNR-PERCENT (RX1).      EL300
02999                                                                   EL300
03000  L01-CARRREP-EXIT.                                                EL300
03001      EXIT.                                                        EL300
03002                                                                   EL300
03003  L01-CARR-END.                                                    EL300
03004      IF CLAS-MAXCN NOT = ZERO                                     EL300
03005         MOVE +1 TO CLAS-STARTCN                                   EL300
03006       ELSE                                                        EL300
03007          MOVE ZERO               TO CLAS-STARTCN.                 EL300
03008                                                                   EL300
03009  L01-EXIT.                                                        EL300
03010      EXIT.                                                        EL300
03011                                                                   EL300
03012      EJECT                                                        EL300
03013 *                                                                 EL300
03014 *  ELPGMN - PROGRAM NAMES TRANSFER                                EL300
03015 *                                                                 EL300
03016  L02-START.                                                       EL300
03017                                                                   EL300
03018      MOVE ZERO                   TO X1.                           EL300
03019                                                                   EL300
03020      MOVE LOW-VALUE TO PN-CONTROL-PRIMARY.                        EL300
03021                                                                   EL300
03022      START ELPGMN  KEY IS NOT LESS THAN PN-CONTROL-PRIMARY        EL300
03023                                                                   EL300
03024      IF ELPGMN-FILE-STATUS NOT = ZERO                             EL300
03025          MOVE 'ERROR OCCURED START - ELPGMN'  TO  WS-ABEND-MESSAGEEL300
03026          MOVE ELPGMN-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL300
03027          PERFORM ABEND-PGM.                                       EL300
03028                                                                   EL300
03029      MOVE ZEROES TO PROGRAM-NAME-TABLE.                           EL300
03030                                                                   EL300
03031  L02-PROG-LOOP-1.                                                 EL300
03032                                                                   EL300
03033      READ ELPGMN NEXT RECORD                                      EL300
03034          AT END                                                   EL300
03035                ADD +1 TO X1                                       EL300
03036                MOVE HIGH-VALUE TO PROG-NM-TB (X1)                 EL300
03037                GO TO L02-EXIT.                                    EL300
03038                                                                   EL300
03039                                                                   EL300
03040      IF PN-PROGRAM-SEQUENCE NOT NUMERIC                           EL300
03041          GO TO L02-PROG-LOOP-1.                                   EL300
03042                                                                   EL300
03043      IF (PN-SYSTEM-CODE = 'EC')                                   EL300
03044          IF ((PN-PROGRAM-SEQUENCE GREATER 000)                    EL300
03045             AND (PN-PROGRAM-SEQUENCE LESS 301))                   EL300
03046              GO TO L02-LOOP-ADD.                                  EL300
03047                                                                   EL300
03048      IF (PN-SYSTEM-CODE = 'EL')                                   EL300
03049          IF ((PN-PROGRAM-SEQUENCE GREATER 300)                    EL300
03050             AND (PN-PROGRAM-SEQUENCE LESS 400))                   EL300
03051              GO TO L02-LOOP-ADD.                                  EL300
03052                                                                   EL300
03053      IF (PN-SYSTEM-CODE = 'EC')                                   EL300
03054          IF ((PN-PROGRAM-SEQUENCE GREATER 399)                    EL300
03055             AND (PN-PROGRAM-SEQUENCE LESS 500))                   EL300
03056              GO TO L02-LOOP-ADD.                                  EL300
03057                                                                   EL300
03058      IF (PN-SYSTEM-CODE = 'EL')                                   EL300
03059          IF ((PN-PROGRAM-SEQUENCE GREATER 499)                    EL300
03060             AND (PN-PROGRAM-SEQUENCE LESS 601))                   EL300
03061              GO TO L02-LOOP-ADD.                                  EL300
03062                                                                   EL300
03063      IF (PN-SYSTEM-CODE = 'EL')                                   EL300
03064          IF ((PN-PROGRAM-SEQUENCE GREATER 800)                    EL300
03065             AND (PN-PROGRAM-SEQUENCE LESS 900))                   EL300
03066              GO TO L02-LOOP-ADD.                                  EL300
03067                                                                   EL300
03068      IF (PN-SYSTEM-CODE = 'EC')                                   EL300
03069          IF ((PN-PROGRAM-SEQUENCE GREATER 600)                    EL300
03070             AND (PN-PROGRAM-SEQUENCE LESS 801))                   EL300
03071              GO TO L02-LOOP-ADD.                                  EL300
03072                                                                   EL300
03073      IF (PN-SYSTEM-CODE = 'EC')                                   EL300
03074          IF ((PN-PROGRAM-SEQUENCE GREATER 399)                    EL300
03075             AND (PN-PROGRAM-SEQUENCE LESS 801))                   EL300
03076              GO TO L02-LOOP-ADD.                                  EL300
03077                                                                   EL300
03078      IF (PN-SYSTEM-CODE = 'EC')                                   EL300
03079          IF  PN-PROGRAM-SEQUENCE GREATER 899                      EL300
03080              GO TO L02-LOOP-ADD.                                  EL300
03081                                                                   EL300
03082      IF (PN-SYSTEM-CODE = 'GL')                                   EL300
03083          IF ((PN-PROGRAM-SEQUENCE GREATER 799)                    EL300
03084             AND (PN-PROGRAM-SEQUENCE LESS 900))                   EL300
03085              GO TO L02-LOOP-ADD.                                  EL300
03086                                                                   EL300
03087      GO TO L02-PROG-LOOP-1.                                       EL300
03088                                                                   EL300
03089  L02-LOOP-ADD.                                                    EL300
03090                                                                   EL300
03091      ADD +1 TO X1.                                                EL300
03092                                                                   EL300
03093      IF X1 GREATER +1500                                          EL300
03094          MOVE 1                  TO ERR-LIST (16)                 EL300
03095          GO TO L02-EXIT.                                          EL300
03096                                                                   EL300
03097      MOVE PN-PROGRAM-SEQUENCE    TO PNT-PROG (X1).                EL300
03098      MOVE PN-PROGRAM-DESCRIPTION TO PNT-MESSAGE (X1).             EL300
03099                                                                   EL300
03100      IF PN-SYSTEM-CODE = 'EC'                                     EL300
03101         MOVE 'S'                 TO PNT-SYSTEM (X1)               EL300
03102       ELSE                                                        EL300
03103          IF PN-SYSTEM-CODE = 'GL'                                 EL300
03104             MOVE 'G'             TO PNT-SYSTEM (X1)               EL300
03105          ELSE                                                     EL300
03106              MOVE 'C'            TO PNT-SYSTEM (X1).              EL300
03107                                                                   EL300
03108      GO TO L02-PROG-LOOP-1.                                       EL300
03109                                                                   EL300
03110  L02-EXIT.                                                        EL300
03111      EXIT.                                                        EL300
03112                                                                   EL300
03113      EJECT                                                        EL300
03114                                                                   EL300
03115 *                                                                 EL300
03116 *                                                                 EL300
03117 *    RECORD 01 WRITE - CLAS - DATE CONTROL RECORD                 EL300
03118 *                                                                 EL300
03119  W01-START.                                                       EL300
03120                                                                   EL300
03121      MOVE CONTROL-TBL            TO DATE-DISK.                    EL300
03122                                                                   EL300
03123      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03124                                                                   EL300
03125  W01-EXIT.                                                        EL300
03126      EXIT.                                                        EL300
03127      EJECT                                                        EL300
03128 *                                                                 EL300
03129 *    RECORD 02 WRITE - CLAD - OPTION RECORD                       EL300
03130 *                                                                 EL300
03131  W02-START.                                                       EL300
03132                                                                   EL300
03133      MOVE OPTION-TABLE           TO DATE-DISK.                    EL300
03134                                                                   EL300
03135      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03136                                                                   EL300
03137  W02-EXIT.                                                        EL300
03138      EXIT.                                                        EL300
03139      EJECT                                                        EL300
03140 *                                                                 EL300
03141 *    RECORD 03 WRITE - CLAX - INDEX RECORD                        EL300
03142 *                                                                 EL300
03143  W03-START.                                                       EL300
03144                                                                   EL300
03145      MOVE INDEX-TBL TO DATE-DISK.                                 EL300
03146                                                                   EL300
03147      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03148                                                                   EL300
03149  W03-EXIT.                                                        EL300
03150      EXIT.                                                        EL300
03151      EJECT                                                        EL300
03152 *                                                                 EL300
03153 *    RECORD 04 WRITE - CLAC - COMPANY RECORDS                     EL300
03154 *                                                                 EL300
03155  W04-START.                                                       EL300
03156                                                                   EL300
03157      MOVE ZERO                   TO X1.                           EL300
03158                                                                   EL300
03159  W04-LOOP.                                                        EL300
03160                                                                   EL300
03161      ADD 1 TO X1.                                                 EL300
03162                                                                   EL300
03163      IF X1 IS GREATER THAN CLAS-MAXC                              EL300
03164          GO TO W04-EXIT.                                          EL300
03165                                                                   EL300
03166      MOVE SPACE                  TO DATE-DISK.                    EL300
03167                                                                   EL300
03168      MOVE 'CLAC'                 TO DD-ID.                        EL300
03169      MOVE X1                     TO DD-CR1.                       EL300
03170      MOVE COMPANY-LINE (X1)      TO DD-CR1N.                      EL300
03171                                                                   EL300
03172      ADD 1 TO X1.                                                 EL300
03173                                                                   EL300
03174      IF X1 IS GREATER THAN CLAS-MAXC                              EL300
03175          PERFORM WRITE-START THRU WRITE-EXIT                      EL300
03176          GO TO W04-EXIT.                                          EL300
03177                                                                   EL300
03178      MOVE X1 TO DD-CR2.                                           EL300
03179      MOVE COMPANY-LINE (X1)      TO DD-CR2N.                      EL300
03180                                                                   EL300
03181      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03182                                                                   EL300
03183      GO TO W04-LOOP.                                              EL300
03184                                                                   EL300
03185  W04-EXIT.                                                        EL300
03186      EXIT.                                                        EL300
03187      EJECT                                                        EL300
03188 *                                                                 EL300
03189 *    RECORD 05 WRITE - CLAF - FACTOR RECORD                       EL300
03190 *                                                                 EL300
03191  W05-START.                                                       EL300
03192                                                                   EL300
03193      MOVE 'CLAF'                 TO FACT-ID.                      EL300
03194      MOVE FACTOR-TBL             TO DATE-DISK.                    EL300
03195                                                                   EL300
03196      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03197                                                                   EL300
03198  W05-EXIT.                                                        EL300
03199      EXIT.                                                        EL300
03200      EJECT                                                        EL300
03201 *                                                                 EL300
03202 *    RECORD 06 WRITE - STAT - STATE RECORDS                       EL300
03203 *                                                                 EL300
03204  W06-START.                                                       EL300
03205                                                                   EL300
03206      MOVE 1                      TO X1.                           EL300
03207                                                                   EL300
03208  W06-LOOP-1.                                                      EL300
03209                                                                   EL300
03210      MOVE SPACE                  TO DATE-DISK.                    EL300
03211                                                                   EL300
03212      MOVE 'STAT'                 TO DD-ID.                        EL300
03213      MOVE ZERO                   TO X2.                           EL300
03214                                                                   EL300
03215  W06-LOOP-2.                                                      EL300
03216                                                                   EL300
03217      ADD 1                       TO X2.                           EL300
03218                                                                   EL300
03219      IF X2 IS GREATER THAN 3                                      EL300
03220          PERFORM WRITE-START THRU WRITE-EXIT                      EL300
03221          GO TO W06-LOOP-1.                                        EL300
03222                                                                   EL300
03223      MOVE STATE-LINE (X1)        TO DD-ST (X2).                   EL300
03224                                                                   EL300
03225      ADD 1 TO X1.                                                 EL300
03226                                                                   EL300
03227      IF X1 IS GREATER THAN CLAS-MAXS                              EL300
03228          PERFORM WRITE-START THRU WRITE-EXIT                      EL300
03229          GO TO W06-EXIT.                                          EL300
03230                                                                   EL300
03231      GO TO W06-LOOP-2.                                            EL300
03232                                                                   EL300
03233  W06-EXIT.                                                        EL300
03234      EXIT.                                                        EL300
03235      EJECT                                                        EL300
03236 *                                                                 EL300
03237 *    RECORD 07 WRITE - BUSC - BUSINESS RECORDS                    EL300
03238 *                                                                 EL300
03239  W07-START.                                                       EL300
03240                                                                   EL300
03241      MOVE 1                     TO X1.                            EL300
03242                                                                   EL300
03243      IF CLAS-MAXB = ZERO                                          EL300
03244          GO TO W07-EXIT.                                          EL300
03245                                                                   EL300
03246  W07-LOOP-1.                                                      EL300
03247                                                                   EL300
03248      MOVE SPACE                  TO DATE-DISK.                    EL300
03249                                                                   EL300
03250      MOVE 'BUSC'                 TO DD-ID.                        EL300
03251      MOVE ZERO                   TO X2.                           EL300
03252                                                                   EL300
03253  W07-LOOP-2.                                                      EL300
03254                                                                   EL300
03255      ADD 1                       TO X2.                           EL300
03256                                                                   EL300
03257      IF X2 IS GREATER THAN 3                                      EL300
03258          PERFORM WRITE-START THRU WRITE-EXIT                      EL300
03259          GO TO W07-LOOP-1.                                        EL300
03260                                                                   EL300
03261      MOVE BUSINESS-LINE (X1)     TO DD-ST (X2).                   EL300
03262                                                                   EL300
03263      ADD 1                       TO X1.                           EL300
03264                                                                   EL300
03265      IF X1 IS GREATER THAN CLAS-MAXB                              EL300
03266          PERFORM WRITE-START THRU WRITE-EXIT                      EL300
03267          GO TO W07-EXIT.                                          EL300
03268                                                                   EL300
03269      GO TO W07-LOOP-2.                                            EL300
03270                                                                   EL300
03271  W07-EXIT.                                                        EL300
03272      EXIT.                                                        EL300
03273      EJECT                                                        EL300
03274 *                                                                 EL300
03275 *    RECORD 08 WRITE - MORT - MORTALITY RECORDS                   EL300
03276 *                                                                 EL300
03277  W08-START.                                                       EL300
03278                                                                   EL300
03279      IF CLAS-MAXM = ZERO                                          EL300
03280          GO TO W08-EXIT.                                          EL300
03281                                                                   EL300
03282      MOVE 1                      TO X1.                           EL300
03283                                                                   EL300
03284  W08-LOOP-1.                                                      EL300
03285                                                                   EL300
03286      MOVE SPACE                  TO DATE-DISK.                    EL300
03287      MOVE 'MORT'                 TO DD-ID.                        EL300
03288                                                                   EL300
03289      MOVE ZERO                   TO X2.                           EL300
03290                                                                   EL300
03291  W08-LOOP-2.                                                      EL300
03292                                                                   EL300
03293      ADD 1 TO X2.                                                 EL300
03294                                                                   EL300
03295      IF X2 IS GREATER THAN 2                                      EL300
03296          PERFORM WRITE-START THRU WRITE-EXIT                      EL300
03297          GO TO W08-LOOP-1.                                        EL300
03298                                                                   EL300
03299      MOVE MORTALITY-LINE (X1)    TO DD-MORT (X2).                 EL300
03300                                                                   EL300
03301      ADD 1 TO X1.                                                 EL300
03302                                                                   EL300
03303      IF X1 IS GREATER THAN CLAS-MAXM                              EL300
03304          PERFORM WRITE-START THRU WRITE-EXIT                      EL300
03305          GO TO W08-EXIT.                                          EL300
03306                                                                   EL300
03307      GO TO W08-LOOP-2.                                            EL300
03308                                                                   EL300
03309  W08-EXIT.                                                        EL300
03310      EXIT.                                                        EL300
03311      EJECT                                                        EL300
03312 *                                                                 EL300
03313 *    RECORD 09 WRITE - CLAI - INSURANCE RECORDS                   EL300
03314 *                                                                 EL300
03315  W09-START.                                                       EL300
03316                                                                   EL300
03317      MOVE 1 TO X1.                                                EL300
03318                                                                   EL300
03319 ********* IF NO A&H, SET WRITE-LOOP TO MAXL                       EL300
03320      IF CLAS-MAXL GREATER CLAS-MAXA                               EL300
03321         MOVE CLAS-MAXL           TO CLAS-MAXA.                    EL300
03322                                                                   EL300
03323  W09-LOOP-1.                                                      EL300
03324                                                                   EL300
03325      MOVE SPACE                  TO DATE-DISK.                    EL300
03326      MOVE 'CLAI'                 TO DD-ID.                        EL300
03327                                                                   EL300
03328      MOVE ZERO                   TO X2.                           EL300
03329                                                                   EL300
03330  W09-LOOP-2.                                                      EL300
03331                                                                   EL300
03332      ADD 1 TO X2.                                                 EL300
03333                                                                   EL300
03334      IF X2 IS GREATER THAN 2                                      EL300
03335          PERFORM WRITE-START THRU WRITE-EXIT                      EL300
03336          GO TO W09-LOOP-1.                                        EL300
03337                                                                   EL300
03338      MOVE INSURANCE-LINE (X1) TO DD-TYPE (X2).                    EL300
03339                                                                   EL300
03340      ADD 1 TO X1.                                                 EL300
03341                                                                   EL300
03342      IF X1 IS GREATER THAN CLAS-MAXA                              EL300
03343          PERFORM WRITE-START THRU WRITE-EXIT                      EL300
03344          GO TO W09-EXIT.                                          EL300
03345                                                                   EL300
03346      GO TO W09-LOOP-2.                                            EL300
03347                                                                   EL300
03348  W09-EXIT.                                                        EL300
03349      EXIT.                                                        EL300
03350      EJECT                                                        EL300
03351 *                                                                 EL300
03352 *    RECORD 10 WRITE - CLXX - PROGRAM SWITCH RECORDS              EL300
03353 *                                                                 EL300
03354  W10-START.                                                       EL300
03355                                                                   EL300
03356      MOVE ZERO                   TO X1 CARD-CNT OPTION-CNT.       EL300
03357                                                                   EL300
03358  W10-LOOP-1.                                                      EL300
03359                                                                   EL300
03360      PERFORM W10-LOOP-2 VARYING CARD-CNT FROM +1 BY +1            EL300
03361              UNTIL CARD-CNT GREATER +50.                          EL300
03362                                                                   EL300
03363      GO TO W10-START-CL.                                          EL300
03364                                                                   EL300
03365  W10-LOOP-2.                                                      EL300
03366                                                                   EL300
03367      PERFORM W10-LOOP-3 VARYING X1 FROM +1 BY +1                  EL300
03368              UNTIL X1 GREATER +20.                                EL300
03369                                                                   EL300
03370      MOVE SPACE                  TO DATE-DISK.                    EL300
03371                                                                   EL300
03372      MOVE CARD-CNT               TO DD-IDX.                       EL300
03373      MOVE 'PO'                   TO DD-IDA.                       EL300
03374      MOVE PROG-SET TO DD-DATA.                                    EL300
03375                                                                   EL300
03376      INSPECT DATE-DISK REPLACING ALL SPACE BY '9'.                EL300
03377      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03378                                                                   EL300
03379  W10-LOOP-3.                                                      EL300
03380                                                                   EL300
03381      ADD 1 TO OPTION-CNT.                                         EL300
03382                                                                   EL300
03383      MOVE PROG-TBL-PRT (OPTION-CNT) TO SW-SET-PRT (X1).           EL300
03384      MOVE PROG-TBL-FMT (OPTION-CNT) TO SW-SET-FMT (X1).           EL300
03385      MOVE PROG-TBL-PRC (OPTION-CNT) TO SW-SET-PRC (X1).           EL300
03386      MOVE PROG-TBL-TOT (OPTION-CNT) TO SW-SET-TOT (X1).           EL300
03387      IF PROG-TBL-PRT (OPTION-CNT) = 'P' OR 'F' OR 'B' OR          EL300
03388                                     'S' OR 'T'                    EL300
03389          NEXT SENTENCE                                            EL300
03390       ELSE                                                        EL300
03391        MOVE 'P' TO SW-SET-PRT (X1).                               EL300
03392                                                                   EL300
03393  W10-START-CL.                                                    EL300
03394                                                                   EL300
03395      MOVE ZERO                   TO X1 CARD-CNT OPTION-CNT.       EL300
03396                                                                   EL300
03397  W10-CL-LOOP-1.                                                   EL300
03398                                                                   EL300
03399      PERFORM W10-CL-LOOP-2 VARYING CARD-CNT FROM +1 BY +1         EL300
03400              UNTIL CARD-CNT GREATER +20.                          EL300
03401                                                                   EL300
03402      GO TO W10-EXIT.                                              EL300
03403                                                                   EL300
03404  W10-CL-LOOP-2.                                                   EL300
03405                                                                   EL300
03406      PERFORM W10-CL-LOOP-3 VARYING X1 FROM +1 BY +1               EL300
03407              UNTIL X1 GREATER +50.                                EL300
03408                                                                   EL300
03409      MOVE SPACE                 TO DATE-DISK.                     EL300
03410                                                                   EL300
03411      MOVE CARD-CNT              TO DD-IDX.                        EL300
03412      MOVE 'CL'                  TO DD-IDA.                        EL300
03413      MOVE PROG-SET-2            TO DD-DATA.                       EL300
03414                                                                   EL300
03415      INSPECT DATE-DISK REPLACING ALL SPACE BY '9'.                EL300
03416      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03417                                                                   EL300
03418  W10-CL-LOOP-3.                                                   EL300
03419                                                                   EL300
03420      ADD 1 TO OPTION-CNT.                                         EL300
03421                                                                   EL300
03422      MOVE PROG-TBL-PRC (OPTION-CNT) TO SW-SET-PRC-2 (X1).         EL300
03423                                                                   EL300
03424      IF PROG-TBL-PRT (OPTION-CNT) = 'F'                           EL300
03425         INSPECT SW-SET-PRC-2 (X1) CONVERTING '123456789' TO       EL300
03426         'ABCDEFGHI'.                                              EL300
03427                                                                   EL300
03428      IF PROG-TBL-PRT (OPTION-CNT) = 'B'                           EL300
03429         INSPECT SW-SET-PRC-2 (X1) CONVERTING '123456789' TO       EL300
03430         'JKLMNOPQR'.                                              EL300
03431                                                                   EL300
03432  W10-EXIT.                                                        EL300
03433      EXIT.                                                        EL300
03434      EJECT                                                        EL300
03435                                                                   EL300
03436 *                                                                 EL300
03437 *    RECORD 12 WRITE - CARR - CARRIER NAME TABLE                  EL300
03438 *                                                                 EL300
03439  W12-START.                                                       EL300
03440                                                                   EL300
03441      IF CLAS-MAXCN = ZERO                                         EL300
03442          GO TO W12-EXIT.                                          EL300
03443                                                                   EL300
03444      MOVE ZERO TO X1.                                             EL300
03445                                                                   EL300
03446  W12-LOOP.                                                        EL300
03447                                                                   EL300
03448      ADD +1 TO X1.                                                EL300
03449                                                                   EL300
03450      IF X1 GREATER THAN CLAS-MAXCN                                EL300
03451          GO TO W12-EXIT.                                          EL300
03452                                                                   EL300
03453      MOVE SPACE                  TO DATE-DISK.                    EL300
03454                                                                   EL300
03455      MOVE 'CARR'                 TO DD-ID.                        EL300
03456      MOVE CN-CODE (X1)           TO DD-CARR1.                     EL300
03457      MOVE CN-DOM-ST (X1)         TO DD-CARR1S.                    EL300
03458      MOVE CN-NAME (X1)           TO DD-CARR1N.                    EL300
03459      MOVE CN-IBNR-UEP-PCT (X1)   TO DD-CARR1-UEP-PCT.             EL300
03460      MOVE CN-IBNR-R78-PCT (X1)   TO DD-CARR1-R78-PCT.             EL300
03461      MOVE CN-IBNR-PRO-PCT (X1)   TO DD-CARR1-PRO-PCT.             EL300
03462                                                                   EL300
03463      ADD 1 TO X1.                                                 EL300
03464                                                                   EL300
03465      MOVE ZEROS                  TO DD-CARR2-UEP-PCT              EL300
03466                                     DD-CARR2-R78-PCT              EL300
03467                                     DD-CARR2-PRO-PCT.             EL300
03468                                                                   EL300
03469      IF X1 GREATER THAN CLAS-MAXCN                                EL300
03470          PERFORM WRITE-START THRU WRITE-EXIT                      EL300
03471          GO TO W12-EXIT.                                          EL300
03472                                                                   EL300
03473      MOVE CN-CODE (X1)           TO DD-CARR2.                     EL300
03474      MOVE CN-DOM-ST (X1)         TO DD-CARR2S.                    EL300
03475      MOVE CN-NAME (X1)           TO DD-CARR2N.                    EL300
03476      MOVE CN-IBNR-UEP-PCT (X1)   TO DD-CARR2-UEP-PCT.             EL300
03477      MOVE CN-IBNR-R78-PCT (X1)   TO DD-CARR2-R78-PCT.             EL300
03478      MOVE CN-IBNR-PRO-PCT (X1)   TO DD-CARR2-PRO-PCT.             EL300
03479                                                                   EL300
03480      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03481                                                                   EL300
03482      GO TO W12-LOOP.                                              EL300
03483                                                                   EL300
03484  W12-EXIT.                                                        EL300
03485      EXIT.                                                        EL300
03486      EJECT                                                        EL300
03487 *                                                                 EL300
03488 *    RECORD 13 WRITE - COLC - CLASIC COMPATIBILITY PARAMETERS     EL300
03489 *                                                                 EL300
03490  W13-START.                                                       EL300
03491                                                                   EL300
03492      MOVE CLASIC-TBL             TO DATE-DISK.                    EL300
03493      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03494                                                                   EL300
03495  W13-EXIT.                                                        EL300
03496      EXIT.                                                        EL300
03497      EJECT                                                        EL300
03498 *                                                                 EL300
03499 *    RECORD 14 WRITE - OPCA - OPTIONAL CLAIM RESERVE CARRIER DATA EL300
03500 *                                                                 EL300
03501  W14-START.                                                       EL300
03502                                                                   EL300
03503      IF  NOT OT-OPT-RESERVE-METHOD-AUTH                           EL300
03504          GO TO W14-EXIT.                                          EL300
03505                                                                   EL300
03506      MOVE +0                     TO X2.                           EL300
03507      MOVE SPACE                  TO DATE-DISK.                    EL300
03508      MOVE 'OPCA'                 TO DD-ID.                        EL300
03509                                                                   EL300
03510      PERFORM W14-LOOP THRU W14-LOOP-EXIT                          EL300
03511              VARYING                                              EL300
03512          X1 FROM 1 BY 1                                           EL300
03513              UNTIL                                                EL300
03514          X1 GREATER THAN +13                                      EL300
03515              OR                                                   EL300
03516          X1 GREATER THAN CLAS-MAXCN.                              EL300
03517                                                                   EL300
03518      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03519                                                                   EL300
03520      IF  X1 GREATER THAN CLAS-MAXCN                               EL300
03521          GO TO W14-EXIT.                                          EL300
03522                                                                   EL300
03523      MOVE +0                     TO X2.                           EL300
03524      MOVE SPACE                  TO DATE-DISK.                    EL300
03525      MOVE 'OPCA'                 TO DD-ID.                        EL300
03526                                                                   EL300
03527      PERFORM W14-LOOP THRU W14-LOOP-EXIT                          EL300
03528              VARYING                                              EL300
03529          X1 FROM X1 BY 1                                          EL300
03530              UNTIL                                                EL300
03531          X1 GREATER THAN +25                                      EL300
03532              OR                                                   EL300
03533          X1 GREATER THAN CLAS-MAXCN.                              EL300
03534                                                                   EL300
03535      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03536      MOVE X1                     TO INDEXCA.                      EL300
03537                                                                   EL300
03538      GO TO W14-EXIT.                                              EL300
03539                                                                   EL300
03540  W14-LOOP.                                                        EL300
03541                                                                   EL300
03542      ADD +1                      TO X2.                           EL300
03543      MOVE CARR-OPT-CLM-FLDS (X1) TO DD-CARR-FLDS (X2).            EL300
03544                                                                   EL300
03545  W14-LOOP-EXIT.                                                   EL300
03546      EXIT.                                                        EL300
03547                                                                   EL300
03548  W14-EXIT.                                                        EL300
03549      EXIT.                                                        EL300
03550      EJECT                                                        EL300
03551 *                                                                 EL300
03552 *    RECORD 15 WRITE - OPST - OPTIONAL CLAIM RESERVE STATE DATA   EL300
03553 *                                                                 EL300
03554  W15-START.                                                       EL300
03555                                                                   EL300
03556      IF  NOT OT-OPT-RESERVE-METHOD-AUTH                           EL300
03557          GO TO W15-EXIT.                                          EL300
03558                                                                   EL300
03559      MOVE +1                     TO X1.                           EL300
03560                                                                   EL300
03561      PERFORM W15-LOOP-A THRU W15-LOOP-A-EXIT                      EL300
03562              UNTIL                                                EL300
03563          X1 GREATER THAN +75                                      EL300
03564              OR                                                   EL300
03565          X1 GREATER THAN CLAS-MAXS.                               EL300
03566                                                                   EL300
03567      MOVE X1                     TO INDEXST.                      EL300
03568      GO TO W15-EXIT.                                              EL300
03569                                                                   EL300
03570  W15-LOOP-A.                                                      EL300
03571                                                                   EL300
03572      MOVE SPACE                  TO DATE-DISK.                    EL300
03573      MOVE 'OPST'                 TO DD-ID.                        EL300
03574                                                                   EL300
03575      PERFORM W15-LOOP-B THRU W15-LOOP-B-EXIT                      EL300
03576              VARYING                                              EL300
03577          X2 FROM 1 BY 1                                           EL300
03578              UNTIL                                                EL300
03579          X2 GREATER THAN +16                                      EL300
03580              OR                                                   EL300
03581          X1 GREATER THAN CLAS-MAXS                                EL300
03582              OR                                                   EL300
03583          X1 GREATER +75.                                          EL300
03584                                                                   EL300
03585      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03586                                                                   EL300
03587  W15-LOOP-A-EXIT.                                                 EL300
03588      EXIT.                                                        EL300
03589                                                                   EL300
03590  W15-LOOP-B.                                                      EL300
03591                                                                   EL300
03592      MOVE STATE-TLR-FLD (X1)     TO DD-STATE-FLDS (X2).           EL300
03593      ADD +1                      TO X1.                           EL300
03594                                                                   EL300
03595  W15-LOOP-B-EXIT.                                                 EL300
03596      EXIT.                                                        EL300
03597                                                                   EL300
03598  W15-EXIT.                                                        EL300
03599      EXIT.                                                        EL300
03600      EJECT                                                        EL300
03601 *                                                                 EL300
03602 *    RECORD 16 WRITE - OPBS - OPTIONAL CLAIM RESERVE BUSINESS DATAEL300
03603 *                                                                 EL300
03604  W16-START.                                                       EL300
03605                                                                   EL300
03606      IF  NOT OT-OPT-RESERVE-METHOD-AUTH                           EL300
03607          GO TO W16-EXIT.                                          EL300
03608                                                                   EL300
03609      MOVE +0                     TO X2.                           EL300
03610      MOVE SPACE                  TO DATE-DISK.                    EL300
03611      MOVE 'OPBS'                 TO DD-ID.                        EL300
03612                                                                   EL300
03613      PERFORM W16-LOOP THRU W16-LOOP-EXIT                          EL300
03614              VARYING                                              EL300
03615          X1 FROM 1 BY 1                                           EL300
03616              UNTIL                                                EL300
03617          X1 GREATER THAN +32.                                     EL300
03618                                                                   EL300
03619      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03620                                                                   EL300
03621      MOVE +0                     TO X2.                           EL300
03622      MOVE SPACE                  TO DATE-DISK.                    EL300
03623      MOVE 'OPBS'                 TO DD-ID.                        EL300
03624                                                                   EL300
03625      PERFORM W16-LOOP THRU W16-LOOP-EXIT                          EL300
03626              VARYING                                              EL300
03627          X1 FROM X1 BY 1                                          EL300
03628              UNTIL                                                EL300
03629          X1 GREATER THAN +64.                                     EL300
03630                                                                   EL300
03631      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03632                                                                   EL300
03633      MOVE +0                     TO X2.                           EL300
03634      MOVE SPACE                  TO DATE-DISK.                    EL300
03635      MOVE 'OPBS'                 TO DD-ID.                        EL300
03636                                                                   EL300
03637      PERFORM W16-LOOP THRU W16-LOOP-EXIT                          EL300
03638              VARYING                                              EL300
03639          X1 FROM X1 BY 1                                          EL300
03640              UNTIL                                                EL300
03641          X1 GREATER THAN +96.                                     EL300
03642                                                                   EL300
03643      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03644                                                                   EL300
03645      MOVE +0                     TO X2.                           EL300
03646      MOVE SPACE                  TO DATE-DISK.                    EL300
03647      MOVE 'OPBS'                 TO DD-ID.                        EL300
03648                                                                   EL300
03649      PERFORM W16-LOOP THRU W16-LOOP-EXIT                          EL300
03650              VARYING                                              EL300
03651          X1 FROM X1 BY 1                                          EL300
03652              UNTIL                                                EL300
03653          X1 GREATER THAN +99.                                     EL300
03654                                                                   EL300
03655      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03656      MOVE X1                     TO INDEXBS.                      EL300
03657                                                                   EL300
03658      GO TO W16-EXIT.                                              EL300
03659                                                                   EL300
03660  W16-LOOP.                                                        EL300
03661                                                                   EL300
03662      ADD +1                      TO X2.                           EL300
03663      MOVE BUS-TLRM-FLD (X1)      TO DD-TLR (X2).                  EL300
03664                                                                   EL300
03665  W16-LOOP-EXIT.                                                   EL300
03666      EXIT.                                                        EL300
03667                                                                   EL300
03668  W16-EXIT.                                                        EL300
03669      EXIT.                                                        EL300
03670      EJECT                                                        EL300
03671 *                                                                 EL300
03672 *    RECORD 17 WRITE - OPMS - OPTIONAL CLAIM RESERVE MISC DATA    EL300
03673 *                                                                 EL300
03674  W17-START.                                                       EL300
03675                                                                   EL300
03676      IF  NOT OT-OPT-RESERVE-METHOD-AUTH                           EL300
03677          GO TO W17-EXIT.                                          EL300
03678                                                                   EL300
03679      MOVE +0                     TO X2.                           EL300
03680      MOVE SPACE                  TO DATE-DISK.                    EL300
03681      MOVE 'OPMS'                 TO DD-ID.                        EL300
03682      MOVE MISC-OPTIONAL-CLAIM-DATA                                EL300
03683                                  TO DD-MISC-DATA.                 EL300
03684                                                                   EL300
03685      PERFORM WRITE-START THRU WRITE-EXIT.                         EL300
03686                                                                   EL300
03687  W17-EXIT.                                                        EL300
03688      EXIT.                                                        EL300
03689      EJECT                                                        EL300
03690 *                                                                 EL300
03691 *    SECTION 01 PRINT - COMPANY NAME AND ADDRESS                  EL300
03692 *                                                                 EL300
03693  P01-START.                                                       EL300
03694                                                                   EL300
03695      MOVE ZERO TO X1.                                             EL300
03696                                                                   EL300
03697      IF LINE-CNT IS GREATER THAN NEW-PAGE                         EL300
03698          PERFORM HEAD-START THRU HEAD-EXIT.                       EL300
03699                                                                   EL300
03700      MOVE S01-H01                TO SAVE-HEAD-1.                  EL300
03701                                                                   EL300
03702      MOVE SPACE                  TO SAVE-HEAD-2.                  EL300
03703                                                                   EL300
03704      PERFORM SECT-START THRU SECT-EXIT.                           EL300
03705                                                                   EL300
03706      MOVE '(CONTINUED)'          TO S01-C.                        EL300
03707      MOVE S01-H01                TO SAVE-HEAD-1.                  EL300
03708                                                                   EL300
03709  P01-LOOP.                                                        EL300
03710                                                                   EL300
03711      ADD 1 TO X1.                                                 EL300
03712                                                                   EL300
03713      IF X1 IS GREATER THAN CLAS-MAXC                              EL300
03714          GO TO P01-EXIT.                                          EL300
03715                                                                   EL300
03716      IF LINE-CNT IS GREATER THAN END-PAGE                         EL300
03717          PERFORM HEAD-START THRU HEAD-EXIT                        EL300
03718          PERFORM SECT-START THRU SECT-EXIT.                       EL300
03719                                                                   EL300
03720      MOVE COMPANY-LINE (X1)      TO S01-NAME.                     EL300
03721                                                                   EL300
03722      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
03723                                                                   EL300
03724      GO TO P01-LOOP.                                              EL300
03725                                                                   EL300
03726  P01-EXIT.                                                        EL300
03727      EXIT.                                                        EL300
03728      EJECT                                                        EL300
03729 *                                                                 EL300
03730 *                                                                 EL300
03731 *    SECTION 02 PRINT - SELECTED PROGRAM OPTIONS                  EL300
03732 *                                                                 EL300
03733  P02-START.                                                       EL300
03734                                                                   EL300
03735      MOVE ZERO                  TO X1.                            EL300
03736      MOVE 1                     TO X2 X3.                         EL300
03737                                                                   EL300
03738      IF LINE-CNT IS GREATER THAN NEW-PAGE                         EL300
03739          PERFORM HEAD-START THRU HEAD-EXIT.                       EL300
03740                                                                   EL300
03741      MOVE S13-H01                TO SAVE-HEAD-1.                  EL300
03742      MOVE S13-H02                TO SAVE-HEAD-2.                  EL300
03743      PERFORM SECT-START THRU SECT-EXIT.                           EL300
03744                                                                   EL300
03745      MOVE '(CONTINUED)' TO S13-C.                                 EL300
03746      MOVE S13-H01                TO SAVE-HEAD-1.                  EL300
03747                                                                   EL300
03748  P02-LOOP-1.                                                      EL300
03749                                                                   EL300
03750      ADD 1 TO X1.                                                 EL300
03751                                                                   EL300
03752      IF X1 IS GREATER THAN 1500                                   EL300
03753          GO TO P02-EXIT.                                          EL300
03754                                                                   EL300
03755      IF PROG-TBL-FMT (X1) = '9' AND                               EL300
03756         PROG-TBL-PRC (X1) = '9' AND                               EL300
03757         PROG-TBL-TOT (X1) = '9'                                   EL300
03758          GO TO P02-LOOP-1.                                        EL300
03759                                                                   EL300
03760      MOVE PROG-TBL-PRC (X1)      TO TEST-OPT.                     EL300
03761                                                                   EL300
03762      MOVE +1 TO X2.                                               EL300
03763                                                                   EL300
03764                                                                   EL300
03765  P02-LOOP-2.                                                      EL300
03766                                                                   EL300
03767      IF LINE-CNT IS GREATER THAN END-PAGE                         EL300
03768          PERFORM HEAD-START THRU HEAD-EXIT                        EL300
03769          PERFORM SECT-START THRU SECT-EXIT.                       EL300
03770                                                                   EL300
03771      IF (X2 GREATER +250) OR                                      EL300
03772 *       (PROG-NM-TB (X2) = HIGH-VALUE)
pemuni        (Pnt-prog (X2) = HIGH-VALUE)
03773             MOVE '???-'         TO S13-CLAS                       EL300
03774             MOVE X1             TO S13-PROGRAM                    EL300
03775             MOVE ALL '*'        TO S13-NAME                       EL300
03776             GO TO P02-LOOP-2-PRT.                                 EL300
03777                                                                   EL300
03778      IF PNT-PROG (X2) NOT = X1                                    EL300
03779          ADD 1 TO X2                                              EL300
03780          GO TO P02-LOOP-2.                                        EL300
03781                                                                   EL300
03782      MOVE 2                     TO PRT-CTL.                       EL300
03783                                                                   EL300
03784      IF PNT-SYSTEM (X2) = 'S'                                     EL300
03785          MOVE 'ECS-'             TO S13-CLAS                      EL300
03786       ELSE                                                        EL300
03787          IF PNT-SYSTEM (X2) = 'G'                                 EL300
03788              MOVE ' GL-'         TO S13-CLAS                      EL300
03789          ELSE                                                     EL300
03790              MOVE ' EL-'         TO S13-CLAS.                     EL300
03791                                                                   EL300
03792      MOVE X1                     TO S13-PROGRAM.                  EL300
03793                                                                   EL300
03794      MOVE PNT-MESSAGE (X2)       TO S13-NAME.                     EL300
03795                                                                   EL300
03796  P02-LOOP-2-PRT.                                                  EL300
03797                                                                   EL300
03798      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
03799                                                                   EL300
03800      MOVE TEST-OPT               TO S13-OPTION.                   EL300
03801                                                                   EL300
03802      IF TEST-OPT = 'X'                                            EL300
03803          MOVE 'PROGRAM IS NOT TO BE RUN' TO S13-DESC              EL300
03804       ELSE                                                        EL300
03805           GO TO P02-RESOLVE-DESCRIP.                              EL300
03806                                                                   EL300
03807      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
03808                                                                   EL300
03809      GO TO P02-LOOP-1.                                            EL300
03810                                                                   EL300
03811  P02-RESOLVE-DESCRIP.                                             EL300
03812                                                                   EL300
03813      IF PNT-SYSTEM (X2) = 'E'                                     EL300
03814          MOVE 'EC'               TO PO-SYSTEM-CODE                EL300
03815       ELSE                                                        EL300
03816          IF PNT-SYSTEM (X2) = 'G'                                 EL300
03817              MOVE 'GL'           TO PO-SYSTEM-CODE                EL300
03818          ELSE                                                     EL300
03819          MOVE 'EL'               TO PO-SYSTEM-CODE.               EL300
03820      MOVE X1                     TO PO-PROGRAM-SEQUENCE.          EL300
03821                                                                   EL300
03822  P02-FMT-DESCRIP.                                                 EL300
03823                                                                   EL300
03824      MOVE 'FORMAT'               TO S13-OPT-TYPE.                 EL300
03825      MOVE SPACE                  TO S13-MESSAGE.                  EL300
03826      MOVE 'F'                    TO PO-OPTION-TYPE.               EL300
03827                                                                   EL300
03828      MOVE PROG-TBL-FMT (X1)      TO PO-PGM-OPTION-CD, S13-OPTION. EL300
03829      IF PNT-SYSTEM (X2) = 'S'                                     EL300
03830          MOVE 'EC'               TO PO-SYSTEM-CODE                EL300
03831       ELSE                                                        EL300
03832          IF PNT-SYSTEM (X2) = 'G'                                 EL300
03833              MOVE 'GL'           TO PO-SYSTEM-CODE                EL300
03834          ELSE                                                     EL300
03835              MOVE 'EL'           TO PO-SYSTEM-CODE.               EL300
03836                                                                   EL300
03837      MOVE X1                     TO PO-PROGRAM-SEQUENCE.          EL300
03838                                                                   EL300
03839      IF (PROG-TBL-FMT (X1) = '9') OR                              EL300
03840         (PROG-TBL-FMT (X1) NOT NUMERIC)                           EL300
03841          MOVE 'NO FORMAT OPTION AVAILABLE' TO S13-DESC            EL300
03842          PERFORM PRINT-START THRU PRINT-EXIT                      EL300
03843          GO TO P02-PRC-DESCRIP.                                   EL300
03844                                                                   EL300
03845      PERFORM P02-READ-OPTION THRU P02-READ-EXIT.                  EL300
03846                                                                   EL300
03847      IF PROG-OR-FMT (X1) = 'D'                                    EL300
03848          MOVE 'DEFAULT'          TO S13-MESSAGE.                  EL300
03849                                                                   EL300
03850      IF PROG-OR-FMT (X1) = 'O'                                    EL300
03851          MOVE 'OVERRIDE'         TO S13-MESSAGE.                  EL300
03852                                                                   EL300
03853      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
03854                                                                   EL300
03855  P02-PRC-DESCRIP.                                                 EL300
03856                                                                   EL300
03857      MOVE 'PROCESS'              TO S13-OPT-TYPE.                 EL300
03858      MOVE SPACE                  TO S13-MESSAGE.                  EL300
03859      MOVE 'P'                    TO PO-OPTION-TYPE.               EL300
03860      MOVE PROG-TBL-PRC (X1)      TO PO-PGM-OPTION-CD, S13-OPTION. EL300
03861      IF PNT-SYSTEM (X2) = 'S'                                     EL300
03862          MOVE 'EC'               TO PO-SYSTEM-CODE                EL300
03863       ELSE                                                        EL300
03864          IF PNT-SYSTEM (X2) = 'G'                                 EL300
03865              MOVE 'GL'           TO PO-SYSTEM-CODE                EL300
03866          ELSE                                                     EL300
03867              MOVE 'EL'           TO PO-SYSTEM-CODE.               EL300
03868                                                                   EL300
03869      MOVE X1                     TO PO-PROGRAM-SEQUENCE.          EL300
03870                                                                   EL300
03871      IF (PROG-TBL-PRC (X1) = '9') OR                              EL300
03872         (PROG-TBL-PRC (X1) NOT NUMERIC)                           EL300
03873          MOVE 'NO PROCESS OPTION AVAILABLE' TO S13-DESC           EL300
03874          PERFORM PRINT-START THRU PRINT-EXIT                      EL300
03875          GO TO P02-TOT-DESCRIP.                                   EL300
03876                                                                   EL300
03877      PERFORM P02-READ-OPTION THRU P02-READ-EXIT.                  EL300
03878                                                                   EL300
03879      IF PROG-OR-PRC (X1) = 'D'                                    EL300
03880          MOVE 'DEFAULT'          TO S13-MESSAGE.                  EL300
03881                                                                   EL300
03882      IF PROG-OR-PRC (X1) = 'O'                                    EL300
03883          MOVE 'OVERRIDE'         TO S13-MESSAGE.                  EL300
03884                                                                   EL300
03885      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
03886                                                                   EL300
03887  P02-TOT-DESCRIP.                                                 EL300
03888                                                                   EL300
03889      MOVE 'TOTAL'                TO  S13-OPT-TYPE.                EL300
03890      MOVE SPACE                  TO  S13-MESSAGE.                 EL300
03891      MOVE 'T'                    TO PO-OPTION-TYPE.               EL300
03892      MOVE PROG-TBL-TOT (X1)      TO PO-PGM-OPTION-CD, S13-OPTION. EL300
03893      IF PNT-SYSTEM (X2) = 'S'                                     EL300
03894          MOVE 'EC'               TO PO-SYSTEM-CODE                EL300
03895       ELSE                                                        EL300
03896          IF PNT-SYSTEM (X2) = 'G'                                 EL300
03897              MOVE 'GL'           TO PO-SYSTEM-CODE                EL300
03898          ELSE                                                     EL300
03899              MOVE 'EL'           TO PO-SYSTEM-CODE.               EL300
03900      MOVE X1                     TO PO-PROGRAM-SEQUENCE.          EL300
03901                                                                   EL300
03902      IF (PROG-TBL-TOT (X1) = '9') OR                              EL300
03903         (PROG-TBL-TOT (X1) NOT NUMERIC)                           EL300
03904          MOVE 'NO TOTAL OPTION AVAILABLE' TO S13-DESC             EL300
03905          PERFORM PRINT-START THRU PRINT-EXIT                      EL300
03906          GO TO P02-LOOP-1.                                        EL300
03907                                                                   EL300
03908      PERFORM P02-READ-OPTION THRU P02-READ-EXIT.                  EL300
03909                                                                   EL300
03910      IF PROG-OR-TOT (X1) = 'D'                                    EL300
03911          MOVE 'DEFAULT'          TO S13-MESSAGE.                  EL300
03912                                                                   EL300
03913      IF PROG-OR-TOT (X1) = 'O'                                    EL300
03914          MOVE 'OVERRIDE'         TO S13-MESSAGE.                  EL300
03915                                                                   EL300
03916      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
03917                                                                   EL300
03918      GO TO P02-LOOP-1.                                            EL300
03919                                                                   EL300
03920  P02-READ-OPTION.                                                 EL300
03921                                                                   EL300
03922      MOVE 'ELPGMN'                TO VSAM-PGM.                    EL300
03923                                                                   EL300
03924      READ ELPGMO RECORD                                           EL300
03925          INVALID KEY                                              EL300
03926              MOVE '* PROGRAM OPTION NOT IN FILE *' TO S13-DESC    EL300
03927              GO TO P02-READ-EXIT.                                 EL300
03928                                                                   EL300
03929      MOVE PO-OPTION-DESCRIPTION TO S13-DESC.                      EL300
03930                                                                   EL300
03931  P02-READ-EXIT.                                                   EL300
03932      EXIT.                                                        EL300
03933                                                                   EL300
03934                                                                   EL300
03935  P02-EXIT.                                                        EL300
03936      EXIT.                                                        EL300
03937      EJECT                                                        EL300
03938 *                                                                 EL300
03939 *    SECTION 03 PRINT - SELECTED PRINT OVERRIDES                  EL300
03940 *                                                                 EL300
03941  P03-START.                                                       EL300
03942                                                                   EL300
03943      MOVE ZERO TO X1.                                             EL300
03944      MOVE 1 TO X2.                                                EL300
03945                                                                   EL300
03946      IF LINE-CNT IS GREATER THAN NEW-PAGE                         EL300
03947          PERFORM HEAD-START THRU HEAD-EXIT.                       EL300
03948                                                                   EL300
03949      MOVE S14-H01                TO SAVE-HEAD-1.                  EL300
03950      MOVE S14-H02                TO SAVE-HEAD-2.                  EL300
03951      PERFORM SECT-START THRU SECT-EXIT.                           EL300
03952                                                                   EL300
03953      IF FICHE-SW = ZERO                                           EL300
03954          MOVE 'NONE GIVEN'       TO POS-06                        EL300
03955          PERFORM PRINT-START THRU PRINT-EXIT                      EL300
03956          GO TO P03-EXIT.                                          EL300
03957                                                                   EL300
03958      MOVE '(CONTINUED)'          TO S14-C.                        EL300
03959      MOVE S14-H01                TO SAVE-HEAD-1.                  EL300
03960                                                                   EL300
03961  P03-LOOP-1.                                                      EL300
03962                                                                   EL300
03963      ADD 1 TO X1.                                                 EL300
03964      MOVE 1 TO X2.                                                EL300
03965                                                                   EL300
03966      IF X1 IS GREATER THAN 1500                                   EL300
03967          GO TO P03-EXIT.                                          EL300
03968                                                                   EL300
03969      IF PROG-TBL-PRC (X1) = 'X'                                   EL300
03970          GO TO P03-LOOP-1.                                        EL300
03971                                                                   EL300
03972      IF PROG-TBL-PRT (X1) = 'P' OR '9'                            EL300
03973          GO TO P03-LOOP-1.                                        EL300
03974                                                                   EL300
03975  P03-LOOP-2.                                                      EL300
03976                                                                   EL300
03977      IF (X2 = +250) OR                                            EL300
03978 *       (PROG-NM-TB (X2) = HIGH-VALUE)
pemuni        (Pnt-prog (X2) = HIGH-VALUE)
03979             GO TO P03-LOOP-1.                                     EL300
03980                                                                   EL300
03981      IF PNT-PROG (X2) IS NOT = X1                                 EL300
03982          ADD 1 TO X2                                              EL300
03983          GO TO P03-LOOP-2.                                        EL300
03984                                                                   EL300
03985      IF LINE-CNT IS GREATER THAN END-PAGE                         EL300
03986          PERFORM HEAD-START THRU HEAD-EXIT                        EL300
03987          PERFORM SECT-START THRU SECT-EXIT.                       EL300
03988                                                                   EL300
03989      IF PNT-SYSTEM (X2) = 'S'                                     EL300
03990          MOVE 'ECS-' TO S14-CLAS                                  EL300
03991       ELSE                                                        EL300
03992          IF PNT-SYSTEM (X2) = 'G'                                 EL300
03993              MOVE ' GL-' TO S14-CLAS                              EL300
03994       ELSE                                                        EL300
03995          IF PNT-SYSTEM (X2) = 'L'                                 EL300
03996              MOVE ' LF-' TO S14-CLAS                              EL300
03997          ELSE                                                     EL300
03998              MOVE ' EL-' TO S14-CLAS.                             EL300
03999                                                                   EL300
04000      MOVE X1 TO S14-PROGRAM.                                      EL300
04001      MOVE PNT-MESSAGE (X2) TO S14-NAME.                           EL300
04002                                                                   EL300
04003      IF PROG-TBL-PRT (X1) = 'B'                                   EL300
04004          MOVE 'FICHE AND PRINT'  TO S14-MESSAGE.                  EL300
04005                                                                   EL300
04006      IF PROG-TBL-PRT (X1) = 'F'                                   EL300
04007          MOVE 'FICHE ONLY'       TO S14-MESSAGE.                  EL300
04008                                                                   EL300
04009      IF PROG-TBL-PRT (X1) = 'S'                                   EL300
04010          MOVE 'SAVE ONLINE'      TO S14-MESSAGE.                  EL300
04011                                                                   EL300
04012      IF PROG-TBL-PRT (X1) = 'T'                                   EL300
04013          MOVE 'SAVE AND PRINT'   TO S14-MESSAGE.                  EL300
04014                                                                   EL300
04015      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
04016                                                                   EL300
04017      GO TO P03-LOOP-1.                                            EL300
04018                                                                   EL300
04019  P03-EXIT.                                                        EL300
04020      EXIT.                                                        EL300
04021      EJECT                                                        EL300
04022 *                                                                 EL300
04023 *    SECTION 04 PRINT - COMPANY FACTOR TABLE                      EL300
04024 *                                                                 EL300
04025  P04-START.                                                       EL300
04026                                                                   EL300
04027      MOVE ZERO TO X1.                                             EL300
04028                                                                   EL300
04029      IF LINE-CNT IS GREATER THAN NEW-PAGE                         EL300
04030          PERFORM HEAD-START THRU HEAD-EXIT.                       EL300
04031                                                                   EL300
04032      MOVE S15-H01                TO SAVE-HEAD-1.                  EL300
04033      MOVE S15-H02                TO SAVE-HEAD-2.                  EL300
04034      PERFORM SECT-START THRU SECT-EXIT.                           EL300
04035                                                                   EL300
04036      IF CLAF-CNT = ZERO                                           EL300
04037          MOVE 'NONE GIVEN' TO POS-06                              EL300
04038          PERFORM PRINT-START THRU PRINT-EXIT                      EL300
04039          GO TO P04-EXIT.                                          EL300
04040                                                                   EL300
04041      MOVE '(CONTINUED)'          TO S15-C.                        EL300
04042      MOVE S15-H01                TO SAVE-HEAD-1.                  EL300
04043                                                                   EL300
04044  P04-LOOP.                                                        EL300
04045      ADD 1                       TO X1.                           EL300
04046                                                                   EL300
04047      IF X1 IS GREATER THAN 6                                      EL300
04048          GO TO P04-EXIT.                                          EL300
04049                                                                   EL300
04050      IF LINE-CNT IS GREATER THAN END-PAGE                         EL300
04051          PERFORM HEAD-START THRU HEAD-EXIT                        EL300
04052          PERFORM SECT-START THRU SECT-EXIT.                       EL300
04053                                                                   EL300
04054      MOVE X1 TO S15-CODE.                                         EL300
04055      MOVE FACT-FACTOR (X1)      TO S15-FACTOR.                    EL300
04056                                                                   EL300
04057      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
04058                                                                   EL300
04059      GO TO P04-LOOP.                                              EL300
04060                                                                   EL300
04061  P04-EXIT.                                                        EL300
04062      EXIT.                                                        EL300
04063      EJECT                                                        EL300
04064 *                                                                 EL300
04065 *    SECTION 16 PRINT - ERROR LISTING                             EL300
04066 *                                                                 EL300
04067  P16-START.                                                       EL300
04068                                                                   EL300
04069      MOVE ZERO TO X1.                                             EL300
04070                                                                   EL300
04071      IF LINE-CNT IS GREATER THAN NEW-PAGE                         EL300
04072          PERFORM HEAD-START THRU HEAD-EXIT.                       EL300
04073                                                                   EL300
04074      MOVE S16-H01                TO SAVE-HEAD-1.                  EL300
04075      MOVE SPACE                  TO SAVE-HEAD-2.                  EL300
04076      PERFORM SECT-START THRU SECT-EXIT.                           EL300
04077                                                                   EL300
04078  P16-LOOP-1.                                                      EL300
04079                                                                   EL300
04080      ADD 1 TO X1.                                                 EL300
04081                                                                   EL300
04082      IF ERR-MSG (X1) = HIGH-VALUES                                EL300
04083          GO TO P16-EXIT.                                          EL300
04084                                                                   EL300
04085      IF ERR-LIST (X1) = ZERO                                      EL300
04086          GO TO P16-LOOP-1.                                        EL300
04087                                                                   EL300
04088      IF LINE-CNT IS GREATER THAN END-PAGE                         EL300
04089          PERFORM HEAD-START THRU HEAD-EXIT                        EL300
04090          PERFORM SECT-START THRU SECT-EXIT.                       EL300
04091                                                                   EL300
04092      MOVE X1                     TO S16-CODE.                     EL300
04093      MOVE ERR-MSG (X1)           TO S16-MESSAGE.                  EL300
04094                                                                   EL300
04095      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
04096      MOVE ZERO                   TO X2.                           EL300
04097                                                                   EL300
04098  P16-LOOP-2.                                                      EL300
04099                                                                   EL300
04100      ADD 1 TO X2.                                                 EL300
04101                                                                   EL300
04102      IF X2 IS GREATER THAN 50                                     EL300
04103          GO TO P16-LOOP-1.                                        EL300
04104                                                                   EL300
04105      IF ERR-NBR-H (X2) = 999                                      EL300
04106          GO TO P16-LOOP-1.                                        EL300
04107                                                                   EL300
04108      IF ERR-NBR-H (X2) NOT = X1                                   EL300
04109          GO TO P16-LOOP-2.                                        EL300
04110                                                                   EL300
04111      IF LINE-CNT IS GREATER THAN END-PAGE                         EL300
04112          PERFORM HEAD-START THRU HEAD-EXIT                        EL300
04113          PERFORM SECT-START THRU SECT-EXIT                        EL300
04114          MOVE X1                 TO S16-CODE                      EL300
04115          MOVE ERR-MSG (X1)       TO S16-MESSAGE                   EL300
04116          MOVE ' (CONT)'          TO S16-CONT                      EL300
04117          PERFORM PRINT-START THRU PRINT-EXIT.                     EL300
04118                                                                   EL300
04119      MOVE ERR-DETAIL-H (X2)      TO S16-ERR-DETAIL.               EL300
04120      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
04121                                                                   EL300
04122      GO TO P16-LOOP-2.                                            EL300
04123                                                                   EL300
04124  P16-EXIT.                                                        EL300
04125      EXIT.                                                        EL300
04126      EJECT                                                        EL300
04127 *                                                                 EL300
04128 *                                                                 EL300
04129 *    LOAD DETAIL ERROR TABLE                                      EL300
04130 *                                                                 EL300
04131  LOAD-START.                                                      EL300
04132                                                                   EL300
04133      IF ERR-X IS GREATER THAN 50                                  EL300
04134          GO TO LOAD-EXIT.                                         EL300
04135                                                                   EL300
04136      ADD 1 TO ERR-X.                                              EL300
04137      MOVE ERR-NBR                TO ERR-NBR-H (ERR-X).            EL300
04138      MOVE ERR-DETAIL             TO ERR-DETAIL-H (ERR-X).         EL300
04139      MOVE 1                      TO ERR-LIST (ERR-NBR).           EL300
04140                                                                   EL300
04141  LOAD-EXIT.                                                       EL300
04142      EXIT.                                                        EL300
04143 *                                                                 EL300
04144 *    TEST FOR ERRORS OCCURD                                       EL300
04145 *                                                                 EL300
04146  ERROR-START.                                                     EL300
04147                                                                   EL300
04148      IF ERROR-SW = 1                                              EL300
04149          GO TO ERROR-EXIT.                                        EL300
04150                                                                   EL300
04151      MOVE ZERO                   TO X1.                           EL300
04152                                                                   EL300
04153  ERROR-LOOP.                                                      EL300
04154                                                                   EL300
04155      ADD 1 TO X1.                                                 EL300
04156                                                                   EL300
04157      IF ERR-MSG (X1) = HIGH-VALUES                                EL300
04158          GO TO ERROR-EXIT.                                        EL300
04159                                                                   EL300
04160      IF ERR-LIST (X1) = ZERO                                      EL300
04161          GO TO ERROR-LOOP.                                        EL300
04162                                                                   EL300
04163      MOVE 1                      TO ERROR-SW.                     EL300
04164                                                                   EL300
04165  ERROR-EXIT.                                                      EL300
04166      EXIT.                                                        EL300
04167 *                                                                 EL300
04168 *    DATE EDIT                                                    EL300
04169 *                                                                 EL300
04170  8500-DATE-CONVERT.                                               EL300
04171      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL300
04172                                                                   EL300
04173  8500-EXIT.                                                       EL300
04174      EXIT.                                                        EL300
04175                                                                   EL300
04176  DATE-START.                                                      EL300
04177                                                                   EL300
04178      IF DATE-TEST IS NOT NUMERIC                                  EL300
04179          GO TO DATE-ERR.                                          EL300
04180                                                                   EL300
04181      IF DATE-MO IS LESS THAN 1                                    EL300
04182          GO TO DATE-ERR.                                          EL300
04183                                                                   EL300
04184      IF DATE-MO IS GREATER THAN 12                                EL300
04185          GO TO DATE-ERR.                                          EL300
04186                                                                   EL300
04187      MOVE DATE-DA      TO HOLD-CEN-1-DA.                          EL300
04188      MOVE DATE-MO      TO HOLD-CEN-1-MO.                          EL300
04189      MOVE DATE-YY      TO HOLD-CEN-1-YY.                          EL300
04190      MOVE DATE-CC      TO HOLD-CEN-1-CC.                          EL300
04191                                                                   EL300
04192      MOVE 'H'          TO DC-OPTION-CODE.                         EL300
04193      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL300
04194      IF DATE-CONVERSION-ERROR                                     EL300
04195         IF ONLY-LEAP-YEAR                                         EL300
04196            MOVE HOLD-CEN-1-DA     TO MONTH-D (2)                  EL300
04197         ELSE                                                      EL300
04198            MOVE 99 TO DATE-DA                                     EL300
04199         END-IF                                                    EL300
04200         MOVE SPACE TO DC-ERROR-CODE.                              EL300
04201                                                                   EL300
04202      IF DATE-DA IS GREATER THAN MONTH-D (DATE-MO)                 EL300
04203          GO TO DATE-ERR.                                          EL300
04204                                                                   EL300
04205      IF DATE-DA IS GREATER THAN ZERO                              EL300
04206          GO TO DATE-EXIT.                                         EL300
04207                                                                   EL300
04208  DATE-ERR.                                                        EL300
04209                                                                   EL300
04210      MOVE SPACE TO DATE-TEST.                                     EL300
04211                                                                   EL300
04212  DATE-EXIT.                                                       EL300
04213      EXIT.                                                        EL300
04214 *                                                                 EL300
04215 *    DATE FILE WRITE                                              EL300
04216 *                                                                 EL300
04217  WRITE-START.                                                     EL300
04218      WRITE DATE-DISK                                              EL300
04219          INVALID KEY MOVE 1 TO ERR-LIST (99).                     EL300
04220  WRITE-EXIT.                                                      EL300
04221      EXIT.                                                        EL300
04222 *                                                                 EL300
04223 *    PAGE HEADING PRINT                                           EL300
04224 *                                                                 EL300
04225  HEAD-START.                                                      EL300
04226                                                                   EL300
04227      ADD 1 TO PAGE-CNT.                                           EL300
04228                                                                   EL300
04229      MOVE PAGE-CNT               TO T03-P.                        EL300
04230                                                                   EL300
04231      MOVE T01                    TO D-LINE.                       EL300
04232      MOVE ZERO                   TO PRT-CTL.                      EL300
04233      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
04234                                                                   EL300
04235      MOVE T02                    TO D-LINE.                       EL300
04236      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
04237                                                                   EL300
04238      MOVE T03                    TO D-LINE.                       EL300
04239      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
04240                                                                   EL300
04241  HEAD-EXIT.                                                       EL300
04242      EXIT.                                                        EL300
04243      EJECT                                                        EL300
04244 *                                                                 EL300
04245 *    SECTION HEADING PRINT                                        EL300
04246 *                                                                 EL300
04247  SECT-START.                                                      EL300
04248                                                                   EL300
04249      MOVE SPACE                  TO D-LINE.                       EL300
04250      MOVE 2                      TO PRT-CTL.                      EL300
04251      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
04252                                                                   EL300
04253      MOVE SAVE-HEAD-1            TO D-LINE.                       EL300
04254      MOVE 2                      TO PRT-CTL.                      EL300
04255      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
04256                                                                   EL300
04257      MOVE 2                      TO PRT-CTL.                      EL300
04258                                                                   EL300
04259      IF SAVE-HEAD-2 = SPACE                                       EL300
04260          GO TO SECT-EXIT.                                         EL300
04261                                                                   EL300
04262      MOVE SAVE-HEAD-2            TO D-LINE.                       EL300
04263      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
04264                                                                   EL300
04265      MOVE 2                      TO PRT-CTL.                      EL300
04266                                                                   EL300
04267  SECT-EXIT.                                                       EL300
04268      EXIT.                                                        EL300
04269 *                                                                 EL300
04270 *    SPECIAL PRINT FOR SECTIONS 02 AND 12                         EL300
04271 *                                                                 EL300
04272  SPEC-START.                                                      EL300
04273                                                                   EL300
04274      IF LINE-CNT IS GREATER THAN END-PAGE                         EL300
04275          MOVE D-LINE TO SAVE-LINE                                 EL300
04276          PERFORM HEAD-START THRU HEAD-EXIT                        EL300
04277          PERFORM SECT-START THRU SECT-EXIT                        EL300
04278          MOVE SAVE-LINE TO D-LINE.                                EL300
04279                                                                   EL300
04280      PERFORM PRINT-START THRU PRINT-EXIT.                         EL300
04281                                                                   EL300
04282  SPEC-EXIT.                                                       EL300
04283      EXIT.                                                        EL300
04284 *                                                                 EL300
04285 *    UNIVERSAL PRINT                                              EL300
04286 *                                                                 EL300
04287  PRINT-START.                                                     EL300
04288                                                                   EL300
04289      MOVE D-LINE                 TO P-DATA.                       EL300
04290      MOVE PRT-CTL                TO P-CTL.                        EL300
04291      ADD PRT-CTL                 TO LINE-CNT.                     EL300
04292                                                                   EL300
04293      IF PRT-CTL = ZERO                                            EL300
04294          MOVE ZERO               TO LINE-CNT.                     EL300
04295                                                                   EL300
04296      ADD 1 TO PRT-CTL.                                            EL300
04297                                                                   EL300
04298      MOVE PRINT-CTL (PRT-CTL) TO LCP-ASA                          EL300
04299      PERFORM LCP-WRITE-POS-PRT                                    EL300
04300          THRU LCP-WRITE-END-PRT.                                  EL300
04301                                                                   EL300
04302      MOVE 1                      TO PRT-CTL.                      EL300
04303      MOVE SPACE                  TO D-LINE.                       EL300
04304                                                                   EL300
04305  PRINT-EXIT.                                                      EL300
04306      EXIT.                                                        EL300
04307 *                                                                 EL300
04308 *    END OF JOB                                                   EL300
04309 *                                                                 EL300
04310  EOJ.                                                             EL300
04311                                                                   EL300
04312      CLOSE CARD-FILE                                              EL300
04313            ELCNTL                                                 EL300
04314            ELPGMN                                                 EL300
04315            ELPGMS                                                 EL300
04316            ELPGMO                                                 EL300
04317            PRNT-FILE                                              EL300
04318            DISK-FILE                                              EL300
04319                                                                   EL300
04320      IF ELCNTL-FILE-STATUS NOT = ZERO                             EL300
04321          MOVE 'ERROR OCCURED CLOSE - ELCNTL'  TO  WS-ABEND-MESSAGEEL300
04322          MOVE ELCNTL-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL300
04323          PERFORM ABEND-PGM.                                       EL300
04324                                                                   EL300
04325      IF ELPGMN-FILE-STATUS NOT = ZERO                             EL300
04326          MOVE 'ERROR OCCURED CLOSE - ELPGMN'  TO  WS-ABEND-MESSAGEEL300
04327          MOVE ELPGMN-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL300
04328          PERFORM ABEND-PGM.                                       EL300
04329                                                                   EL300
04330      IF ELPGMS-FILE-STATUS NOT = ZERO                             EL300
04331          MOVE 'ERROR OCCURED CLOSE - ELPGMS'  TO  WS-ABEND-MESSAGEEL300
04332          MOVE ELPGMS-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL300
04333          PERFORM ABEND-PGM.                                       EL300
04334                                                                   EL300
04335      IF ELPGMO-FILE-STATUS NOT = ZERO                             EL300
04336          MOVE 'ERROR OCCURED CLOSE - ELPGMO'  TO  WS-ABEND-MESSAGEEL300
04337          MOVE ELPGMO-FILE-STATUS  TO  WS-ABEND-FILE-STATUS        EL300
04338          PERFORM ABEND-PGM.                                       EL300
04339                                                                   EL300
04340      DISPLAY ' '.                                                 EL300
04341      DISPLAY '**** THE END OF COMMENTS CREATED BY EL300. ****'.   EL300
04342      DISPLAY ' '.                                                 EL300
04343                                                                   EL300
04344      GOBACK.                                                      EL300
04345                                                                   EL300
04346  ABEND-PGM SECTION. COPY ELCABEND.                                EL300
04347 /                                                                 EL300
04348  LCP-WRITE-POS-PRT SECTION.                                       EL300
04349      IF LCP-ASA = '+'                                             EL300
04350          WRITE PRT AFTER 0 LINE                                   EL300
04351      ELSE                                                         EL300
04352      IF LCP-ASA = ' '                                             EL300
04353          WRITE PRT AFTER ADVANCING 1 LINE                         EL300
04354      ELSE                                                         EL300
04355      IF LCP-ASA = '0'                                             EL300
04356          WRITE PRT AFTER ADVANCING 2 LINE                         EL300
04357      ELSE                                                         EL300
04358      IF LCP-ASA = '-'                                             EL300
04359          WRITE PRT AFTER ADVANCING 3 LINE                         EL300
04360      ELSE                                                         EL300
04361      IF LCP-ASA = '1'                                             EL300
04362          WRITE PRT AFTER ADVANCING PAGE                           EL300
04363      ELSE                                                         EL300
04364      IF LCP-ASA = '2'                                             EL300
04365          WRITE PRT AFTER ADVANCING LCP-CH2                        EL300
04366      ELSE                                                         EL300
04367      IF LCP-ASA = '3'                                             EL300
04368          WRITE PRT AFTER ADVANCING LCP-CH3                        EL300
04369      ELSE                                                         EL300
04370      IF LCP-ASA = '4'                                             EL300
04371          WRITE PRT AFTER ADVANCING LCP-CH4                        EL300
04372      ELSE                                                         EL300
04373      IF LCP-ASA = '5'                                             EL300
04374          WRITE PRT AFTER ADVANCING LCP-CH5                        EL300
04375      ELSE                                                         EL300
04376      IF LCP-ASA = '6'                                             EL300
04377          WRITE PRT AFTER ADVANCING LCP-CH6                        EL300
04378      ELSE                                                         EL300
04379      IF LCP-ASA = '7'                                             EL300
04380          WRITE PRT AFTER ADVANCING LCP-CH7                        EL300
04381      ELSE                                                         EL300
04382      IF LCP-ASA = '8'                                             EL300
04383          WRITE PRT AFTER ADVANCING LCP-CH8                        EL300
04384      ELSE                                                         EL300
04385      IF LCP-ASA = '9'                                             EL300
04386          WRITE PRT AFTER ADVANCING LCP-CH9                        EL300
04387      ELSE                                                         EL300
04388      IF LCP-ASA = 'A'                                             EL300
04389          WRITE PRT AFTER ADVANCING LCP-CH10                       EL300
04390      ELSE                                                         EL300
04391      IF LCP-ASA = 'B'                                             EL300
04392          WRITE PRT AFTER ADVANCING LCP-CH11                       EL300
04393      ELSE                                                         EL300
04394      IF LCP-ASA = 'C'                                             EL300
04395          WRITE PRT AFTER ADVANCING LCP-CH12                       EL300
04396      ELSE                                                         EL300
04397      IF LCP-ASA = 'V'                                             EL300
04398          WRITE PRT AFTER ADVANCING LCP-P01                        EL300
04399      ELSE                                                         EL300
04400      IF LCP-ASA = 'W'                                             EL300
04401          WRITE PRT AFTER ADVANCING LCP-P02                        EL300
04402      ELSE                                                         EL300
04403      DISPLAY 'ASA CODE ERROR'.                                    EL300
04404  LCP-WRITE-END-PRT.                                               EL300
04405      EXIT.                                                        EL300
